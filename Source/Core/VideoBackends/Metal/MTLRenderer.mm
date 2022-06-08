// Copyright 2022 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoBackends/Metal/MTLRenderer.h"

#include "VideoBackends/Metal/MTLBoundingBox.h"
#include "VideoBackends/Metal/MTLObjectCache.h"
#include "VideoBackends/Metal/MTLPipeline.h"
#include "VideoBackends/Metal/MTLStateTracker.h"
#include "VideoBackends/Metal/MTLTexture.h"
#include "VideoBackends/Metal/MTLUtil.h"
#include "VideoBackends/Metal/MTLVertexManager.h"

#include "VideoCommon/FramebufferManager.h"
#include "VideoCommon/NativeVertexFormat.h"
#include "VideoCommon/VertexShaderGen.h"

Metal::Renderer::Renderer(MRCOwned<CAMetalLayer*> layer, int width, int height, float layer_scale)
  : ::Renderer(width, height, layer_scale, Util::ToAbstract([layer pixelFormat]))
  , m_layer(std::move(layer))
{
  UpdateActiveConfig();
}

Metal::Renderer::~Renderer() = default;

bool Metal::Renderer::IsHeadless() const
{
  return m_layer == nullptr;
}

bool Metal::Renderer::Initialize()
{
  if (!::Renderer::Initialize())
    return false;
  SetupSurface();
  g_state_tracker->FlushEncoders();
  return true;
}

// MARK: Texture Creation

std::unique_ptr<AbstractTexture> Metal::Renderer::CreateTexture(
  const TextureConfig& config, std::string_view name)
{ @autoreleasepool {
  MRCOwned<MTLTextureDescriptor*> desc = MRCTransfer([MTLTextureDescriptor new]);
  [desc setTextureType:config.samples > 1 ? MTLTextureType2DMultisampleArray : MTLTextureType2DArray];
  [desc setPixelFormat:Util::FromAbstract(config.format)];
  [desc setWidth:config.width];
  [desc setHeight:config.height];
  [desc setMipmapLevelCount:config.levels];
  [desc setArrayLength:config.layers];
  [desc setSampleCount:config.samples];
  [desc setStorageMode:MTLStorageModePrivate];
  MTLTextureUsage usage = MTLTextureUsageShaderRead;
  if (config.IsRenderTarget())
    usage |= MTLTextureUsageRenderTarget;
  if (config.IsComputeImage())
    usage |= MTLTextureUsageShaderWrite;
  [desc setUsage:usage];
  id<MTLTexture> texture = [g_device newTextureWithDescriptor:desc];
  if (!texture)
    return nullptr;
  auto label = MRCTransfer([[NSString alloc] initWithBytes:name.data() length:name.size() encoding:NSUTF8StringEncoding]);
  [texture setLabel:label];
  return std::make_unique<Texture>(MRCTransfer(texture), config);
}}

std::unique_ptr<AbstractStagingTexture> Metal::Renderer::CreateStagingTexture(
  StagingTextureType type, const TextureConfig& config)
{ @autoreleasepool {
  size_t stride = config.GetStride();
  size_t buffer_size = stride * static_cast<size_t>(config.height);

  MTLResourceOptions options = MTLStorageModeShared;
  if (type == StagingTextureType::Upload)
    options |= MTLResourceCPUCacheModeWriteCombined;

  id<MTLBuffer> buffer = [g_device newBufferWithLength:buffer_size options:options];
  if (!buffer)
    return nullptr;
  [buffer setLabel:[NSString stringWithFormat:@"Staging Texture %d", m_staging_texture_counter++]];
  return std::make_unique<StagingTexture>(MRCTransfer(buffer), type, config);
}}

std::unique_ptr<AbstractFramebuffer> Metal::Renderer::CreateFramebuffer(
  AbstractTexture* color_attachment, AbstractTexture* depth_attachment)
{
  AbstractTexture* either_attachment = color_attachment ? color_attachment : depth_attachment;
  return std::make_unique<Framebuffer>(color_attachment, depth_attachment,
    either_attachment->GetWidth(),  either_attachment->GetHeight(),
    either_attachment->GetLayers(), either_attachment->GetSamples());
}

// MARK: Pipeline Creation

namespace Metal
{
class VertexFormat : public NativeVertexFormat
{
public:
  VertexFormat(const PortableVertexDeclaration& vtx_decl)
    : NativeVertexFormat(vtx_decl)
    , m_desc(MRCTransfer([MTLVertexDescriptor new]))
  {
    [[[m_desc layouts] objectAtIndexedSubscript:0] setStride:vtx_decl.stride];
    SetAttribute(SHADER_POSITION_ATTRIB, vtx_decl.position);
    SetAttributes(SHADER_NORMAL_ATTRIB, vtx_decl.normals);
    SetAttributes(SHADER_COLOR0_ATTRIB, vtx_decl.colors);
    SetAttributes(SHADER_TEXTURE0_ATTRIB, vtx_decl.texcoords);
    SetAttribute(SHADER_POSMTX_ATTRIB, vtx_decl.posmtx);
  }

  MTLVertexDescriptor* Get() const { return m_desc; }

private:
  template <size_t N>
  void SetAttributes(u32 attribute, const AttributeFormat(&format)[N])
  {
    for (size_t i = 0; i < N; i++)
      SetAttribute(attribute + i, format[i]);
  }
  void SetAttribute(u32 attribute, const AttributeFormat& format)
  {
    if (!format.enable)
      return;
    MTLVertexAttributeDescriptor* desc = [[m_desc attributes] objectAtIndexedSubscript:attribute];
    [desc setFormat:ConvertFormat(format.type, format.components, format.integer)];
    [desc setOffset:format.offset];
    [desc setBufferIndex:0];
  }

  static MTLVertexFormat ConvertFormat(ComponentFormat format, int count, bool int_format)
  {
    static constexpr MTLVertexFormat formats[2][5][4] = {
      [false] = {
        [static_cast<int>(ComponentFormat::UByte)]  = { MTLVertexFormatUCharNormalized,  MTLVertexFormatUChar2Normalized,  MTLVertexFormatUChar3Normalized,  MTLVertexFormatUChar4Normalized  },
        [static_cast<int>(ComponentFormat::Byte)]   = { MTLVertexFormatCharNormalized,   MTLVertexFormatChar2Normalized,   MTLVertexFormatChar3Normalized,   MTLVertexFormatChar4Normalized   },
        [static_cast<int>(ComponentFormat::UShort)] = { MTLVertexFormatUShortNormalized, MTLVertexFormatUShort2Normalized, MTLVertexFormatUShort3Normalized, MTLVertexFormatUShort4Normalized },
        [static_cast<int>(ComponentFormat::Short)]  = { MTLVertexFormatShortNormalized,  MTLVertexFormatShort2Normalized,  MTLVertexFormatShort3Normalized,  MTLVertexFormatShort4Normalized  },
        [static_cast<int>(ComponentFormat::Float)]  = { MTLVertexFormatFloat,            MTLVertexFormatFloat2,            MTLVertexFormatFloat3,            MTLVertexFormatFloat4            },
      },
      [true] = {
        [static_cast<int>(ComponentFormat::UByte)]  = { MTLVertexFormatUChar,  MTLVertexFormatUChar2,  MTLVertexFormatUChar3,  MTLVertexFormatUChar4  },
        [static_cast<int>(ComponentFormat::Byte)]   = { MTLVertexFormatChar,   MTLVertexFormatChar2,   MTLVertexFormatChar3,   MTLVertexFormatChar4   },
        [static_cast<int>(ComponentFormat::UShort)] = { MTLVertexFormatUShort, MTLVertexFormatUShort2, MTLVertexFormatUShort3, MTLVertexFormatUShort4 },
        [static_cast<int>(ComponentFormat::Short)]  = { MTLVertexFormatShort,  MTLVertexFormatShort2,  MTLVertexFormatShort3,  MTLVertexFormatShort4  },
        [static_cast<int>(ComponentFormat::Float)]  = { MTLVertexFormatFloat,  MTLVertexFormatFloat2,  MTLVertexFormatFloat3,  MTLVertexFormatFloat4  },
      },
    };
    return formats[int_format][static_cast<int>(format)][count - 1];
  }

  MRCOwned<MTLVertexDescriptor*> m_desc;
};
} // namespace Metal

std::unique_ptr<AbstractShader> Metal::Renderer::CreateShaderFromSource(
  ShaderStage stage, std::string_view source, std::string_view name)
{ @autoreleasepool {
  std::string msl = Util::PrepareMSLShader(stage, source);
  NSError* err = nullptr;
  auto lib = MRCTransfer([g_device newLibraryWithSource:[NSString stringWithUTF8String:msl.data()] options:nil error:&err]);
  if (err)
  {
    PanicAlertFmt("Failed to compile shader {}: {}", name, [[err localizedDescription] UTF8String]);
    WARN_LOG_FMT(VIDEO, "Failing shader: {}", msl);
    return nullptr;
  }
  auto fn = MRCTransfer([lib newFunctionWithName:@"main0"]);
  if (!fn)
  {
    PanicAlertFmt("Shader {} is missing its main0 function", name);
    WARN_LOG_FMT(VIDEO, "Failing shader: {}", msl);
    return nullptr;
  }
  [fn setLabel:MRCTransfer([[NSString alloc] initWithBytes:name.data() length:name.size() encoding:NSUTF8StringEncoding])];
  if (stage == ShaderStage::Compute)
  {
    MRCOwned<id<MTLComputePipelineState>> pipeline = MRCTransfer([g_device newComputePipelineStateWithFunction:fn error:&err]);
    if (err)
    {
      PanicAlertFmt("Failed to compile pipeline {}", name);
      WARN_LOG_FMT(VIDEO, "Failing shader: {}", msl);
      return nullptr;
    }
    return std::make_unique<ComputePipeline>(stage, std::move(fn), std::move(pipeline));
  }
  return std::make_unique<Shader>(stage, std::move(fn));
}}

std::unique_ptr<AbstractShader> Metal::Renderer::CreateShaderFromBinary(
  ShaderStage stage, const void* data, size_t length, std::string_view name)
{
  PanicAlertFmt("CreateShaderFromBinary called, but it's unsupported!");
  return nullptr;
}

std::unique_ptr<NativeVertexFormat> Metal::Renderer::CreateNativeVertexFormat(
  const PortableVertexDeclaration& vtx_decl)
{ @autoreleasepool {
  return std::make_unique<VertexFormat>(vtx_decl);
}}

static MTLPrimitiveTopologyClass GetClass(PrimitiveType prim)
{
  switch (prim)
  {
    case PrimitiveType::Points:
      return MTLPrimitiveTopologyClassPoint;
    case PrimitiveType::Lines:
      return MTLPrimitiveTopologyClassLine;
    case PrimitiveType::Triangles:
    case PrimitiveType::TriangleStrip:
      return MTLPrimitiveTopologyClassTriangle;
  }
}

static MTLPrimitiveType Convert(PrimitiveType prim)
{
  switch (prim)
  {
    case PrimitiveType::Points:        return MTLPrimitiveTypePoint;
    case PrimitiveType::Lines:         return MTLPrimitiveTypeLine;
    case PrimitiveType::Triangles:     return MTLPrimitiveTypeTriangle;
    case PrimitiveType::TriangleStrip: return MTLPrimitiveTypeTriangleStrip;
  }
}

static MTLCullMode Convert(CullMode cull)
{
  switch (cull)
  {
    case CullMode::None:
    case CullMode::All: // Handled by disabling rasterization
      return MTLCullModeNone;
    case CullMode::Front:
      return MTLCullModeFront;
    case CullMode::Back:
      return MTLCullModeBack;
  }
}

static MTLBlendFactor Convert(DstBlendFactor factor, bool src1)
{
  static constexpr MTLBlendFactor factors[2][8] = {
    [false] = {
      [static_cast<int>(DstBlendFactor::Zero)]        = MTLBlendFactorZero,
      [static_cast<int>(DstBlendFactor::One)]         = MTLBlendFactorOne,
      [static_cast<int>(DstBlendFactor::SrcClr)]      = MTLBlendFactorSourceColor,
      [static_cast<int>(DstBlendFactor::InvSrcClr)]   = MTLBlendFactorOneMinusSourceColor,
      [static_cast<int>(DstBlendFactor::SrcAlpha)]    = MTLBlendFactorSourceAlpha,
      [static_cast<int>(DstBlendFactor::InvSrcAlpha)] = MTLBlendFactorOneMinusSourceAlpha,
      [static_cast<int>(DstBlendFactor::DstAlpha)]    = MTLBlendFactorDestinationAlpha,
      [static_cast<int>(DstBlendFactor::InvDstAlpha)] = MTLBlendFactorOneMinusDestinationAlpha,
    },
    [true] = {
      [static_cast<int>(DstBlendFactor::Zero)]        = MTLBlendFactorZero,
      [static_cast<int>(DstBlendFactor::One)]         = MTLBlendFactorOne,
      [static_cast<int>(DstBlendFactor::SrcClr)]      = MTLBlendFactorSourceColor,
      [static_cast<int>(DstBlendFactor::InvSrcClr)]   = MTLBlendFactorOneMinusSource1Color,
      [static_cast<int>(DstBlendFactor::SrcAlpha)]    = MTLBlendFactorSource1Alpha,
      [static_cast<int>(DstBlendFactor::InvSrcAlpha)] = MTLBlendFactorOneMinusSource1Alpha,
      [static_cast<int>(DstBlendFactor::DstAlpha)]    = MTLBlendFactorDestinationAlpha,
      [static_cast<int>(DstBlendFactor::InvDstAlpha)] = MTLBlendFactorOneMinusDestinationAlpha,
    },
  };
  return factors[src1][static_cast<int>(factor)];
}

static MTLBlendFactor Convert(SrcBlendFactor factor, bool src1)
{
  static constexpr MTLBlendFactor factors[2][8] = {
    [false] = {
      [static_cast<int>(SrcBlendFactor::Zero)]        = MTLBlendFactorZero,
      [static_cast<int>(SrcBlendFactor::One)]         = MTLBlendFactorOne,
      [static_cast<int>(SrcBlendFactor::DstClr)]      = MTLBlendFactorDestinationColor,
      [static_cast<int>(SrcBlendFactor::InvDstClr)]   = MTLBlendFactorOneMinusDestinationColor,
      [static_cast<int>(SrcBlendFactor::SrcAlpha)]    = MTLBlendFactorSourceAlpha,
      [static_cast<int>(SrcBlendFactor::InvSrcAlpha)] = MTLBlendFactorOneMinusSourceAlpha,
      [static_cast<int>(SrcBlendFactor::DstAlpha)]    = MTLBlendFactorDestinationAlpha,
      [static_cast<int>(SrcBlendFactor::InvDstAlpha)] = MTLBlendFactorOneMinusDestinationAlpha,
    },
    [true] = {
      [static_cast<int>(SrcBlendFactor::Zero)]        = MTLBlendFactorZero,
      [static_cast<int>(SrcBlendFactor::One)]         = MTLBlendFactorOne,
      [static_cast<int>(SrcBlendFactor::DstClr)]      = MTLBlendFactorDestinationColor,
      [static_cast<int>(SrcBlendFactor::InvDstClr)]   = MTLBlendFactorOneMinusDestinationColor,
      [static_cast<int>(SrcBlendFactor::SrcAlpha)]    = MTLBlendFactorSource1Alpha,
      [static_cast<int>(SrcBlendFactor::InvSrcAlpha)] = MTLBlendFactorOneMinusSource1Alpha,
      [static_cast<int>(SrcBlendFactor::DstAlpha)]    = MTLBlendFactorDestinationAlpha,
      [static_cast<int>(SrcBlendFactor::InvDstAlpha)] = MTLBlendFactorOneMinusDestinationAlpha,
    },
  };
  return factors[src1][static_cast<int>(factor)];
}

std::unique_ptr<AbstractPipeline> Metal::Renderer::CreatePipeline(
  const AbstractPipelineConfig& config, const void* cache_data, size_t cache_data_length)
{ @autoreleasepool {
  assert(!config.geometry_shader);
  auto desc = MRCTransfer([MTLRenderPipelineDescriptor new]);
  [desc setVertexFunction:static_cast<const Shader*>(config.vertex_shader)->GetShader()];
  [desc setFragmentFunction:static_cast<const Shader*>(config.pixel_shader)->GetShader()];
  if (config.vertex_format)
    [desc setVertexDescriptor:static_cast<const VertexFormat*>(config.vertex_format)->Get()];
  RasterizationState rs = config.rasterization_state;
  [desc setInputPrimitiveTopology:GetClass(rs.primitive)];
  if (rs.cullmode == CullMode::All)
    [desc setRasterizationEnabled:NO];
  MTLRenderPipelineColorAttachmentDescriptor* color0 = [[desc colorAttachments] objectAtIndexedSubscript:0];
  BlendingState bs = config.blending_state;
  MTLColorWriteMask mask = MTLColorWriteMaskNone;
  if (bs.colorupdate)
    mask |= MTLColorWriteMaskRed | MTLColorWriteMaskGreen | MTLColorWriteMaskBlue;
  if (bs.alphaupdate)
    mask |= MTLColorWriteMaskAlpha;
  [color0 setWriteMask:mask];
  if (bs.blendenable)
  {
    [color0 setBlendingEnabled:YES];
    [color0 setSourceRGBBlendFactor:       Convert(bs.srcfactor,      bs.usedualsrc)];
    [color0 setSourceAlphaBlendFactor:     Convert(bs.srcfactoralpha, bs.usedualsrc)];
    [color0 setDestinationRGBBlendFactor:  Convert(bs.dstfactor,      bs.usedualsrc)];
    [color0 setDestinationAlphaBlendFactor:Convert(bs.dstfactoralpha, bs.usedualsrc)];
    [color0 setRgbBlendOperation:  bs.subtract      ? MTLBlendOperationReverseSubtract : MTLBlendOperationAdd];
    [color0 setAlphaBlendOperation:bs.subtractAlpha ? MTLBlendOperationReverseSubtract : MTLBlendOperationAdd];
  }
  FramebufferState fs = config.framebuffer_state;
  [color0 setPixelFormat:Util::FromAbstract(fs.color_texture_format)];
  [desc setDepthAttachmentPixelFormat:Util::FromAbstract(fs.depth_texture_format)];
  if (Util::HasStencil(fs.depth_texture_format))
    [desc setStencilAttachmentPixelFormat:Util::FromAbstract(fs.depth_texture_format)];
  NSError* err = nullptr;
  MTLRenderPipelineReflection* reflection = nullptr;
  id<MTLRenderPipelineState> pipe = [g_device
    newRenderPipelineStateWithDescriptor:desc
                                 options:MTLPipelineOptionArgumentInfo
                              reflection:&reflection
                                   error:&err];
  if (err)
  {
    PanicAlertFmt("Failed to compile pipeline for {} and {}: {}",
      [[[desc vertexFunction] name] UTF8String], [[[desc fragmentFunction] name] UTF8String], [[err localizedDescription] UTF8String]);
    return nullptr;
  }
  return std::make_unique<Pipeline>(MRCTransfer(pipe), reflection, Convert(rs.primitive), Convert(rs.cullmode), config.depth_state, config.usage);
}}

void Metal::Renderer::Flush()
{ @autoreleasepool {
  g_state_tracker->FlushEncoders();
}}

void Metal::Renderer::WaitForGPUIdle()
{ @autoreleasepool {
  g_state_tracker->FlushEncoders();
  g_state_tracker->WaitForFlushedEncoders();
}}

void Metal::Renderer::OnConfigChanged(u32 bits)
{
  if (bits & CONFIG_CHANGE_BIT_VSYNC)
    [m_layer setDisplaySyncEnabled:g_ActiveConfig.bVSyncActive];

  if (bits & CONFIG_CHANGE_BIT_ANISOTROPY)
  {
    g_object_cache->ReloadSamplers();
    g_state_tracker->ReloadSamplers();
  }
}

void Metal::Renderer::ClearScreen(const MathUtil::Rectangle<int>& rc, bool color_enable, bool alpha_enable, bool z_enable, u32 color, u32 z)
{
  MathUtil::Rectangle<int> target_rc = Renderer::ConvertEFBRectangle(rc);
  target_rc.ClampUL(0, 0, m_target_width, m_target_height);

  // All Metal render passes are fullscreen, so we can only run a fast clear if the target is fullscreen
  if (target_rc == MathUtil::Rectangle<int>(0, 0, m_target_width, m_target_height))
  {
    // Determine whether the EFB has an alpha channel. If it doesn't, we can clear the alpha
    // channel to 0xFF. This hopefully allows us to use the fast path in most cases.
    if (bpmem.zcontrol.pixel_format == PixelFormat::RGB565_Z16 ||
        bpmem.zcontrol.pixel_format == PixelFormat::RGB8_Z24 ||
        bpmem.zcontrol.pixel_format == PixelFormat::Z24)
    {
      // Force alpha writes, and clear the alpha channel. This is different to the other backends,
      // where the existing values of the alpha channel are preserved.
      alpha_enable = true;
      color &= 0x00FFFFFF;
    }

    bool c_ok = (color_enable && alpha_enable) || g_state_tracker->GetCurrentFramebuffer()->GetColorFormat() == AbstractTextureFormat::Undefined;
    bool z_ok = z_enable || g_state_tracker->GetCurrentFramebuffer()->GetDepthFormat() == AbstractTextureFormat::Undefined;
    if (c_ok && z_ok)
    {
      @autoreleasepool
      {
        MTLClearColor cc = MTLClearColorMake(
          static_cast<double>((color >> 16) & 0xFF) / 255.0,
          static_cast<double>((color >>  8) & 0xFF) / 255.0,
          static_cast<double>((color >>  0) & 0xFF) / 255.0,
          static_cast<double>((color >> 24) & 0xFF) / 255.0);
        float f_z = static_cast<float>(z & 0xFFFFFF) / 16777216.0f;
        if (!g_Config.backend_info.bSupportsReversedDepthRange)
          f_z = 1.f - f_z;
        g_state_tracker->BeginClearRenderPass(cc, f_z);
        return;
      }
    }
  }

  g_state_tracker->EnableEncoderLabel(false);
  g_framebuffer_manager->ClearEFB(rc, color_enable, alpha_enable, z_enable, color, z);
  g_state_tracker->EnableEncoderLabel(true);
}

void Metal::Renderer::SetPipeline(const AbstractPipeline* pipeline)
{
  g_state_tracker->SetPipeline(static_cast<const Pipeline*>(pipeline));
}

void Metal::Renderer::SetFramebuffer(AbstractFramebuffer* framebuffer)
{
  // Shouldn't be bound as a texture.
  if (AbstractTexture* color = framebuffer->GetColorAttachment())
    g_state_tracker->UnbindTexture(static_cast<Texture*>(color)->GetMTLTexture());
  if (AbstractTexture* depth = framebuffer->GetDepthAttachment())
    g_state_tracker->UnbindTexture(static_cast<Texture*>(depth)->GetMTLTexture());

  m_current_framebuffer = framebuffer;
  g_state_tracker->SetCurrentFramebuffer(static_cast<Framebuffer*>(framebuffer));
}

void Metal::Renderer::SetAndDiscardFramebuffer(AbstractFramebuffer* framebuffer)
{ @autoreleasepool {
  SetFramebuffer(framebuffer);
  g_state_tracker->BeginRenderPass(MTLLoadActionDontCare);
}}

void Metal::Renderer::SetAndClearFramebuffer(AbstractFramebuffer* framebuffer, const ClearColor& color_value, float depth_value)
{ @autoreleasepool {
  SetFramebuffer(framebuffer);
  MTLClearColor color = MTLClearColorMake(color_value[0], color_value[1], color_value[2], color_value[3]);
  g_state_tracker->BeginClearRenderPass(color, depth_value);
}}

void Metal::Renderer::SetScissorRect(const MathUtil::Rectangle<int>& rc)
{
  g_state_tracker->SetScissor(rc);
}

void Metal::Renderer::SetTexture(u32 index, const AbstractTexture* texture)
{
  g_state_tracker->SetTexture(index, texture ? static_cast<const Texture*>(texture)->GetMTLTexture() : nullptr);
}

void Metal::Renderer::SetSamplerState(u32 index, const SamplerState& state)
{
  g_state_tracker->SetSampler(index, state);
}

void Metal::Renderer::SetComputeImageTexture(AbstractTexture* texture, bool read, bool write)
{
  g_state_tracker->SetComputeTexture(static_cast<const Texture*>(texture));
}

void Metal::Renderer::UnbindTexture(const AbstractTexture* texture)
{
  g_state_tracker->UnbindTexture(static_cast<const Texture*>(texture)->GetMTLTexture());
}

void Metal::Renderer::SetViewport(float x, float y, float width, float height, float near_depth, float far_depth)
{
  g_state_tracker->SetViewport(x, y, width, height, near_depth, far_depth);
}

void Metal::Renderer::Draw(u32 base_vertex, u32 num_vertices)
{ @autoreleasepool {
  g_state_tracker->Draw(base_vertex, num_vertices);
}}

void Metal::Renderer::DrawIndexed(u32 base_index, u32 num_indices, u32 base_vertex)
{ @autoreleasepool {
  g_state_tracker->DrawIndexed(base_index, num_indices, base_vertex);
}}

void Metal::Renderer::DispatchComputeShader(const AbstractShader* shader, u32 groups_x, u32 groups_y, u32 groups_z)
{ @autoreleasepool {
  g_state_tracker->SetPipeline(static_cast<const ComputePipeline*>(shader));
  g_state_tracker->DispatchComputeShader(groups_x, groups_y, groups_z);
}}

void Metal::Renderer::BindBackbuffer(const ClearColor& clear_color)
{ @autoreleasepool {
  CheckForSurfaceChange();
  CheckForSurfaceResize();
  m_drawable = MRCRetain([m_layer nextDrawable]);
  m_bb_texture->SetMTLTexture(MRCRetain([m_drawable texture]));
  SetAndClearFramebuffer(m_backbuffer.get(), clear_color);
}}

void Metal::Renderer::PresentBackbuffer()
{ @autoreleasepool {
  g_state_tracker->EndRenderPass();
  if (m_drawable)
  {
    [g_state_tracker->GetRenderCmdBuf() addScheduledHandler:[drawable = std::move(m_drawable)](id){
      [drawable present];
    }];
    m_drawable = nullptr;
  }
  g_state_tracker->FlushEncoders();
}}

std::unique_ptr<::BoundingBox> Metal::Renderer::CreateBoundingBox() const
{
  return std::make_unique<BoundingBox>();
}

void Metal::Renderer::CheckForSurfaceChange()
{
  if (!m_surface_changed.TestAndClear())
    return;
  m_layer = MRCRetain(static_cast<CAMetalLayer*>(m_new_surface_handle));
  m_new_surface_handle = nullptr;
  SetupSurface();
}

void Metal::Renderer::CheckForSurfaceResize()
{
  if (!m_surface_resized.TestAndClear())
    return;
  SetupSurface();
}

void Metal::Renderer::SetupSurface()
{
  CGSize size = [m_layer bounds].size;
  // TODO: Update m_backbuffer_scale (need to make doing that not break everything)
  float backbuffer_scale = [m_layer contentsScale];
  size.width *= backbuffer_scale;
  size.height *= backbuffer_scale;
  [m_layer setDrawableSize:size];
  m_backbuffer_width = size.width;
  m_backbuffer_height = size.height;
  TextureConfig cfg(m_backbuffer_width, m_backbuffer_height, 1, 1, 1, m_backbuffer_format, AbstractTextureFlag_RenderTarget);
  m_bb_texture = std::make_unique<Texture>(nullptr, cfg);
  m_backbuffer = std::make_unique<Framebuffer>(m_bb_texture.get(), nullptr, m_backbuffer_width, m_backbuffer_height, 1, 1);
}
