// Copyright 2022 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoBackends/Metal/MTLUtil.h"

#include <fstream>
#include <string>

#include "Common/MsgHandler.h"

Metal::DeviceFeatures Metal::g_features;

std::vector<MRCOwned<id<MTLDevice>>> Metal::Util::GetAdapterList()
{
  std::vector<MRCOwned<id<MTLDevice>>> list;
  id<MTLDevice> default_dev = MTLCreateSystemDefaultDevice();
  if (default_dev)
    list.push_back(MRCTransfer(default_dev));

  auto devices = MRCTransfer(MTLCopyAllDevices());
  for (id<MTLDevice> device in devices.Get())
  {
    if (device != default_dev)
      list.push_back(MRCRetain(device));
  }
  return list;
}

void Metal::Util::PopulateBackendInfo(VideoConfig* config)
{
  config->backend_info.api_type = APIType::Metal;
  config->backend_info.bUsesLowerLeftOrigin = false;
  config->backend_info.bSupportsExclusiveFullscreen = false;
  config->backend_info.bSupportsDualSourceBlend = true;
  config->backend_info.bSupportsPrimitiveRestart = true;
  config->backend_info.bSupportsGeometryShaders = false;
  config->backend_info.bSupportsComputeShaders = true;
  config->backend_info.bSupports3DVision = false;
  config->backend_info.bSupportsEarlyZ = true;
  config->backend_info.bSupportsBindingLayout = true;
  config->backend_info.bSupportsBBox = true;
  config->backend_info.bSupportsGSInstancing = false;
  config->backend_info.bSupportsPostProcessing = true;
  config->backend_info.bSupportsPaletteConversion = true;
  config->backend_info.bSupportsClipControl = true;
  config->backend_info.bSupportsSSAA = true;
  config->backend_info.bSupportsFragmentStoresAndAtomics = true;
  config->backend_info.bSupportsReversedDepthRange = false;
  config->backend_info.bSupportsLogicOp = false;
  config->backend_info.bSupportsMultithreading = false;
  config->backend_info.bSupportsGPUTextureDecoding = true;
  config->backend_info.bSupportsCopyToVram = true;
  config->backend_info.bSupportsBitfield = true;
  config->backend_info.bSupportsDynamicSamplerIndexing = true;
  config->backend_info.bSupportsFramebufferFetch = false;
  config->backend_info.bSupportsBackgroundCompiling = true;
  config->backend_info.bSupportsLargePoints = true;
  config->backend_info.bSupportsPartialDepthCopies = true;
  config->backend_info.bSupportsDepthReadback = true;
  config->backend_info.bSupportsShaderBinaries = false;
  config->backend_info.bSupportsPipelineCacheData = false;
  config->backend_info.bSupportsCoarseDerivatives = false;
  config->backend_info.bSupportsTextureQueryLevels = true;
  config->backend_info.bSupportsLodBiasInSampler = false;
  config->backend_info.bSupportsSettingObjectNames = true;
}

void Metal::Util::PopulateBackendInfoAdapters(VideoConfig* config, const std::vector<MRCOwned<id<MTLDevice>>>& adapters)
{
  config->backend_info.Adapters.clear();
  for (id<MTLDevice> adapter : adapters)
  {
    config->backend_info.Adapters.push_back([[adapter name] UTF8String]);
  }
}

void Metal::Util::PopulateBackendInfoFeatures(VideoConfig* config, id<MTLDevice> device)
{
#ifdef TARGET_OS_MAC
  config->backend_info.bSupportsDepthClamp = true;
  config->backend_info.bSupportsST3CTextures = true;
  config->backend_info.bSupportsBPTCTextures = true;
#else
  bool supports_mac1 = false;
  bool supports_apple4 = false;
  if (@available(iOS 13, *))
  {
    supports_mac1 = [device supportsFamily:MTLGPUFamilyMac1];
    supports_apple4 = [device supportsFamily:MTLGPUFamilyApple4];
  }
  else
  {
    supports_apple4 = [device supportsFeatureSet:MTLFeatureSet_iOS_GPUFamily4_v1];
  }
  config->backend_info.bSupportsDepthClamp = supports_mac1 || supports_apple4;
  config->backend_info.bSupportsST3CTextures = supports_mac1;
  config->backend_info.bSupportsBPTCTextures = supports_mac1;
#endif
  if (char* env = getenv("MTL_UNIFIED_MEMORY"))
    g_features.unified_memory = env[0] == '1' || env[0] == 'y' || env[0] == 'Y';
  else if (@available(macOS 10.15, iOS 13, *))
    g_features.unified_memory = [device hasUnifiedMemory];
  else
    g_features.unified_memory = false;

  g_features.subgroup_ops = false;
  if (@available(macOS 10.15, iOS 13, *))
  {
    // Requires SIMD-scoped reduction operations
    g_features.subgroup_ops = [device supportsFamily:MTLGPUFamilyMac2] || [device supportsFamily:MTLGPUFamilyApple6];
  }
  if ([[device name] containsString:@"AMD"])
  {
    // Broken
    g_features.subgroup_ops = false;
  }
}

AbstractTextureFormat Metal::Util::ToAbstract(MTLPixelFormat format)
{
  switch (format)
  {
    case MTLPixelFormatRGBA8Unorm:            return AbstractTextureFormat::RGBA8;
    case MTLPixelFormatBGRA8Unorm:            return AbstractTextureFormat::BGRA8;
    case MTLPixelFormatBC1_RGBA:              return AbstractTextureFormat::DXT1;
    case MTLPixelFormatBC2_RGBA:              return AbstractTextureFormat::DXT3;
    case MTLPixelFormatBC3_RGBA:              return AbstractTextureFormat::DXT5;
    case MTLPixelFormatBC7_RGBAUnorm:         return AbstractTextureFormat::BPTC;
    case MTLPixelFormatR16Unorm:              return AbstractTextureFormat::R16;
    case MTLPixelFormatDepth16Unorm:          return AbstractTextureFormat::D16;
    case MTLPixelFormatDepth24Unorm_Stencil8: return AbstractTextureFormat::D24_S8;
    case MTLPixelFormatR32Float:              return AbstractTextureFormat::R32F;
    case MTLPixelFormatDepth32Float:          return AbstractTextureFormat::D32F;
    case MTLPixelFormatDepth32Float_Stencil8: return AbstractTextureFormat::D32F_S8;
    default:                                  return AbstractTextureFormat::Undefined;
  }
}

MTLPixelFormat Metal::Util::FromAbstract(AbstractTextureFormat format)
{
  switch (format)
  {
    case AbstractTextureFormat::RGBA8:     return MTLPixelFormatRGBA8Unorm;
    case AbstractTextureFormat::BGRA8:     return MTLPixelFormatBGRA8Unorm;
    case AbstractTextureFormat::DXT1:      return MTLPixelFormatBC1_RGBA;
    case AbstractTextureFormat::DXT3:      return MTLPixelFormatBC2_RGBA;
    case AbstractTextureFormat::DXT5:      return MTLPixelFormatBC3_RGBA;
    case AbstractTextureFormat::BPTC:      return MTLPixelFormatBC7_RGBAUnorm;
    case AbstractTextureFormat::R16:       return MTLPixelFormatR16Unorm;
    case AbstractTextureFormat::D16:       return MTLPixelFormatDepth16Unorm;
    case AbstractTextureFormat::D24_S8:    return MTLPixelFormatDepth24Unorm_Stencil8;
    case AbstractTextureFormat::R32F:      return MTLPixelFormatR32Float;
    case AbstractTextureFormat::D32F:      return MTLPixelFormatDepth32Float;
    case AbstractTextureFormat::D32F_S8:   return MTLPixelFormatDepth32Float_Stencil8;
    case AbstractTextureFormat::Undefined: return MTLPixelFormatInvalid;
  }
}

static const char MSL_SHADER_HEADER[] = R"(
#include <metal_stdlib>
using namespace metal;

typedef metal::texture2d_array<float> main_texture;

#define API_METAL_MSL
#define INPUT_DECL_BEGIN struct Input {
#define INPUT_DECL_END };
#define VERTEX_INPUT_DECL_BEGIN struct StageData {
#define VERTEX_INPUT_DECL_END };
#define VERTEX_OUTPUT_DECL_BEGIN struct Output {
#define VERTEX_OUTPUT_DECL_END };
#define PIXEL_INPUT_DECL_BEGIN struct StageData {
#define PIXEL_INPUT_DECL_END };
#define PIXEL_OUTPUT_DECL_BEGIN struct Output {
#define PIXEL_OUTPUT_DECL_END };

#define DECL_CB_PS struct PSUniform
#define DECL_CB_VS struct VSUniform
#define DECL_CB_UTILITY struct UtilityUniform
#define DECL_INPUT_CB_PS constant PSUniform& cb_ps [[buffer(0)]];
#define DECL_INPUT_CB_VS constant VSUniform& cb_vs [[buffer(1)]];
#define DECL_INPUT_CB_UTILITY constant UtilityUniform& cb_util [[buffer(1)]];
#define DECL_INPUT_TEXTURE(name, binding)               main_texture                         name [[texture(binding)]];
#define DECL_INPUT_TEXTURE_ARRAY(name, binding, length) metal::array<main_texture, length>   name [[texture(binding)]];
#define DECL_INPUT_TEXTURE_MS(name, binding)            metal::texture2d_ms_array<float>     name [[texture(binding)]];
#define DECL_INPUT_DEPTH(name, binding)                 metal::depth2d_array<float>          name [[texture(binding)]];
#define DECL_INPUT_DEPTH_MS(name, binding)              metal::depth2d_ms_array<float>       name [[texture(binding)]];
#define DECL_INPUT_SAMPLER(name, binding)               metal::sampler                       name [[sampler(binding)]];
#define DECL_INPUT_SAMPLER_ARRAY(name, binding, length) metal::array<metal::sampler, length> name [[sampler(binding)]];
#define DECL_INPUT_TEXEL_BUFFER(type, name, binding)    device const type*                   name [[buffer((binding) + 4)]];
#define DECL_VERTEX_INPUT(type, name, semantic, binding) type name [[attribute(binding)]];
#define DECL_VERTEX_OUTPUT(type, name, semantic, binding) type name [[user(semantic)]];
#define DECL_VERTEX_INPUT_VID
#define DECL_VERTEX_OUTPUT_POSITION float4 position [[position]];
#define DECL_VERTEX_OUTPUT_CLIPDIST [[clip_distance]] float clip_dist[2];
#define DECL_VERTEX_OUTPUT_POINT_SIZE float point_size [[point_size]];
#define DECL_PIXEL_INPUT(type, name, semantic, binding) type name [[user(semantic)]];
#define DECL_PIXEL_INPUT_POSITION float4 position [[position]];
#define DECL_PIXEL_INPUT_CLIPDIST
#define DECL_PIXEL_INPUT_SAMPLE_IDX float4 sample_id [[sample_id]];
#define DECL_PIXEL_INPUT_ARRAY_IDX(name) uint name [[render_target_array_index]];
#define DECL_PIXEL_OUTPUT_COLOR0_UINT uint4 col0 [[color(0), index(0)]];
#define DECL_PIXEL_OUTPUT_COLOR0(type) type col0 [[color(0), index(0)]];
#define DECL_PIXEL_OUTPUT_COLOR1 float4 col1 [[color(0), index(1)]];
#define DECL_PIXEL_OUTPUT_DEPTH float depth [[depth(any)]];

#define DECL_MAIN void main()

#define IN(type) thread const type&
#define INOUT(type) thread type&
#define TEX(member) input.member
#define INPUT(member) stage_data.member
#define OUTPUT(member) output.member
#define opos output.position
#define oclipDist0 output.clip_dist[0]
#define oclipDist1 output.clip_dist[1]
#define gl_PointSize output.point_size
#define frag_coord stage_data.position
#define gl_SampleID stage_data.sample_id
#define ocol0 output.col0
#define ocol1 output.col1
#define odepth output.depth
#define TEXTURE_SAMPLE(tex, sampler, coords) input.tex.sample(input.sampler, (coords).xy, uint(round((coords).z)))
#define TEXTURE_SAMPLE_OFFSET(tex, sampler, coords, offset) input.tex.sample(input.sampler, (coords).xy, uint(round((coords).z)), offset)
#define TEXTURE_SAMPLE_LAYER(tex, sampler, coords, layer) input.tex.sample(input.sampler, coords, layer)
#define TEXTURE_SAMPLE_LAYER_BIAS(tex, sampler, coords, layer, biasval) input.tex.sample(input.sampler, coords, layer, metal::bias(biasval))
#define TEXTURE_FETCH(tex, coords, layer) input.tex.read(uint2(coords), layer)
#define TEXTURE_FETCH_LOD(tex, coords, layer, lod) input.tex.read(uint2(coords), layer, lod)
#define TEXTURE_FETCH_MS(tex, coords, layer, sample) input.tex.read(uint2(coords), layer, sample)
#define DEPTH_SAMPLE(tex, sampler, coords) float4(input.tex.sample(input.sampler, (coords).xy, uint(round((coords).z))))
#define DEPTH_FETCH_MS(tex, coords, layer, sample) float4(input.tex.read(uint2(coords), layer, sample))
#define TEXEL_BUFFER_FETCH_1(tex, index, offset) input.tex[index]
#define CB_PS(member) input.cb_ps.member
#define CB_VS(member) input.cb_vs.member
#define CB_UTILITY(member) input.cb_util.member

#define bitfieldExtract metal::extract_bits
#define roundEven metal::rint
#define frac metal::fract
#define lerp metal::mix
#define dFdx metal::dfdx
#define dFdy metal::dfdy
#define UNREACHABLE __builtin_unreachable();
#define MAYBE_UNUSED [[maybe_unused]]
#define BOOL uint
#define discard discard_fragment()

#define FIXUP_OPOS opos.y = -opos.y
#define FIXUP_OPOS_VK

struct Main
{
)";

static const char MSL_SHADER_FOOTER_VERTEX[] = R"(
  uint vid;
  thread StageData& stage_data;
  thread Input& input;
  Output output;
  Main(uint vid, thread StageData& stage_data, thread Input& input): vid(vid), stage_data(stage_data), input(input) {}
};

vertex Main::Output main0(uint vid [[vertex_id]], Main::StageData stage_data [[stage_in]], Main::Input input)
{
  Main main(vid, stage_data, input);
  main.main();
  return main.output;
}
)";

static const char MSL_SHADER_FOOTER_FRAGMENT[] = R"(
  thread StageData& stage_data;
  thread Input& input;
  Output output;
#ifdef USE_FRAMEBUFFER_FETCH
  decltype(Output::col0) initial_ocol0;
#endif
  Main(thread StageData& stage_data, thread Input& input): stage_data(stage_data), input(input) {}
};

#ifdef FORCE_EARLY_Z
[[early_fragment_tests]]
#endif
fragment Main::Output main0(
#ifdef USE_FRAMEBUFFER_FETCH
  decltype(Main::Output::col0) cin [[color(0)]],
#endif
  Main::StageData stage_data [[stage_in]],
  Main::Input input)
{
  Main main(stage_data, input);
#ifdef USE_FRAMEBUFFER_FETCH
  main.initial_ocol0 = cin;
#endif
  main.main();
  return main.output;
}
)";

static const char MSL_SHADER_FOOTER_COMPUTE[] = R"(
  thread Input& input;
  Main(thread Input& input): input(input) {}
};

kernel void main0(Main::Input input)
{
  Main main(input);
  main.main();
}
)";

std::string Metal::Util::PrepareMSLShader(ShaderStage stage, std::string_view source)
{
  std::string output(MSL_SHADER_HEADER);
  output.append(source);
  switch (stage)
  {
    case ShaderStage::Vertex:
      output.append(MSL_SHADER_FOOTER_VERTEX);
      break;
    case ShaderStage::Geometry:
      PanicAlertFmt("Attempted to compile Metal geometry shader, but Metal doesn't support geometry shaders!");
      break;
    case ShaderStage::Pixel:
      output.append(MSL_SHADER_FOOTER_FRAGMENT);
      break;
    case ShaderStage::Compute:
      output.append(MSL_SHADER_FOOTER_COMPUTE);
      break;
  }

  return output;
}
