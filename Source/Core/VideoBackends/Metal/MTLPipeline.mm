// Copyright 2022 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoBackends/Metal/MTLPipeline.h"

#include "Common/MsgHandler.h"

#include "VideoCommon/NativeVertexFormat.h"

template <size_t N>
static void Populate(u32* offsets, const AttributeFormat (&format)[N], bool needs_components)
{
  for (size_t i = 0; i < N; i++)
    offsets[i] = format[i].offset | (needs_components ? format[i].components << 16 : 0);
}

Metal::UberShaderVertexAttributes::UberShaderVertexAttributes(const PortableVertexDeclaration& fmt)
{
  stride = fmt.stride;
  offsets[SHADER_POSITION_ATTRIB] = fmt.position.offset | (fmt.position.components << 16);
  offsets[SHADER_POSMTX_ATTRIB] = fmt.posmtx.offset;
  Populate(offsets + SHADER_NORMAL_ATTRIB, fmt.normals, false);
  Populate(offsets + SHADER_COLOR0_ATTRIB, fmt.colors, false);
  Populate(offsets + SHADER_TEXTURE0_ATTRIB, fmt.texcoords, true);
}

static void MarkAsUsed(u32* list, u32 start, u32 length)
{
  for (u32 i = start; i < start + length; i++)
    *list |= 1 << i;
}

static void GetArguments(NSArray<MTLArgument*>* arguments, u32* textures, u32* samplers, u32* buffers)
{
  for (MTLArgument* argument in arguments)
  {
    u32 idx = [argument index];
    u32 length = [argument arrayLength];
    if (idx + length > 32)
    {
      PanicAlertFmt("Making a MTLPipeline with high argument index {:d}..<{:d} for {:s}",
                    idx, idx + length, [[argument name] UTF8String]);
      continue;
    }
    switch ([argument type])
    {
      case MTLArgumentTypeTexture:
        if (textures)
          MarkAsUsed(textures, idx, length);
        else
          PanicAlertFmt("Vertex function wants a texture!");
        break;
      case MTLArgumentTypeSampler:
        if (samplers)
          MarkAsUsed(samplers, idx, length);
        else
          PanicAlertFmt("Vertex function wants a sampler!");
        break;
      case MTLArgumentTypeBuffer:
        MarkAsUsed(buffers, idx, length);
        break;
      default:
        break;
    }
  }
}

Metal::Pipeline::Pipeline(MRCOwned<id<MTLRenderPipelineState>> pipeline, MTLRenderPipelineReflection* reflection,
    MTLPrimitiveType prim, MTLCullMode cull, DepthState depth, AbstractPipelineUsage usage, UberShaderVertexAttributes attributes)
  : m_pipeline(std::move(pipeline))
  , m_prim(prim)
  , m_cull(cull)
  , m_dss(depth)
  , m_usage(usage)
  , m_attributes(attributes)
{
  GetArguments([reflection vertexArguments], nullptr, nullptr, &m_vertex_buffers);
  GetArguments([reflection fragmentArguments], &m_textures, &m_samplers, &m_fragment_buffers);
}

Metal::ComputePipeline::ComputePipeline(ShaderStage stage, MRCOwned<id<MTLFunction>> shader, MRCOwned<id<MTLComputePipelineState>> pipeline)
  : Shader(stage, std::move(shader))
  , m_compute_pipeline(std::move(pipeline))
{
}
