// Copyright 2022 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <Metal/Metal.h>

#include "VideoBackends/Metal/MRCHelpers.h"

#include "VideoCommon/AbstractPipeline.h"
#include "VideoCommon/AbstractShader.h"

namespace Metal
{
class Shader : public AbstractShader
{
public:
  explicit Shader(ShaderStage stage, MRCOwned<id<MTLFunction>> shader)
    : AbstractShader(stage)
    , m_shader(std::move(shader))
  {
  }


  id<MTLFunction> GetShader() const { return m_shader; }

private:
  MRCOwned<id<MTLFunction>> m_shader;
};
} // namespace Metal
