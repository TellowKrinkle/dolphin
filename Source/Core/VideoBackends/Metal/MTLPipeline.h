// Copyright 2022 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <Metal/Metal.h>

#include "VideoBackends/Metal/MRCHelpers.h"
#include "VideoBackends/Metal/MTLObjectCache.h"
#include "VideoBackends/Metal/MTLShader.h"

#include "VideoCommon/AbstractPipeline.h"
#include "VideoCommon/AbstractShader.h"

namespace Metal
{
class Pipeline final : public AbstractPipeline
{
public:
  explicit Pipeline(MRCOwned<id<MTLRenderPipelineState>> pipeline,
                    MTLRenderPipelineReflection* reflection, MTLPrimitiveType prim,
                    MTLCullMode cull, DepthState depth, AbstractPipelineUsage usage);

  id<MTLRenderPipelineState> Get() const { return m_pipeline; }
  MTLPrimitiveType Prim() const { return m_prim; }
  MTLCullMode Cull() const { return m_cull; }
  DSSSelector DSS() const { return m_dss; }
  AbstractPipelineUsage Usage() const { return m_usage; }
  u32 GetTextures() const { return m_textures; }
  u32 GetSamplers() const { return m_samplers; }
  u32 GetVertexBuffers() const { return m_vertex_buffers; }
  u32 GetFragmentBuffers() const { return m_fragment_buffers; }
  bool UsesVertexBuffer(u32 index) const { return m_vertex_buffers & (1 << index); }
  bool UsesFragmentBuffer(u32 index) const { return m_fragment_buffers & (1 << index); }

private:
  MRCOwned<id<MTLRenderPipelineState>> m_pipeline;
  MTLPrimitiveType m_prim;
  MTLCullMode m_cull;
  DSSSelector m_dss;
  AbstractPipelineUsage m_usage;
  u32 m_textures = 0;
  u32 m_samplers = 0;
  u32 m_vertex_buffers = 0;
  u32 m_fragment_buffers = 0;
};

class ComputePipeline : public Shader
{
public:
  explicit ComputePipeline(ShaderStage stage, MTLComputePipelineReflection* reflection,
                           std::string msl, MRCOwned<id<MTLFunction>> shader,
                           MRCOwned<id<MTLComputePipelineState>> pipeline);

  id<MTLComputePipelineState> GetComputePipeline() const { return m_compute_pipeline; }
  bool UsesTexture(u32 index) const { return m_textures & (1 << index); }
  bool UsesBuffer(u32 index) const { return m_buffers & (1 << index); }

private:
  MRCOwned<id<MTLComputePipelineState>> m_compute_pipeline;
  u32 m_textures = 0;
  u32 m_buffers = 0;
};
}  // namespace Metal
