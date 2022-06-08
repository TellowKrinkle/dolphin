// Copyright 2008 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#pragma once

#include <string_view>
#include "Common/CommonTypes.h"

class ShaderCode;

#define LIGHT_COL "CB_VS({})[{}].color.{}"
#define LIGHT_COL_PARAMS(index, swizzle) (I_LIGHTS), (index), (swizzle)

#define LIGHT_COSATT "CB_VS({})[{}].cosatt"
#define LIGHT_COSATT_PARAMS(index) (I_LIGHTS), (index)

#define LIGHT_DISTATT "CB_VS({})[{}].distatt"
#define LIGHT_DISTATT_PARAMS(index) (I_LIGHTS), (index)

#define LIGHT_POS "CB_VS({})[{}].pos"
#define LIGHT_POS_PARAMS(index) (I_LIGHTS), (index)

#define LIGHT_DIR "CB_VS({})[{}].dir"
#define LIGHT_DIR_PARAMS(index) (I_LIGHTS), (index)

/**
 * Common uid data used for shader generators that use lighting calculations.
 */
struct LightingUidData
{
  u32 matsource : 4;       // 4x1 bit
  u32 enablelighting : 4;  // 4x1 bit
  u32 ambsource : 4;       // 4x1 bit
  u32 diffusefunc : 8;     // 4x2 bits
  u32 attnfunc : 8;        // 4x2 bits
  u32 light_mask : 32;     // 4x8 bits
};

constexpr char s_lighting_struct[] = "struct Light {\n"
                                     "  int4 color;\n"
                                     "  float4 cosatt;\n"
                                     "  float4 distatt;\n"
                                     "  float4 pos;\n"
                                     "  float4 dir;\n"
                                     "};\n";

void GenerateLightingShaderCode(ShaderCode& object, const LightingUidData& uid_data,
                                std::string_view in_color_name0, std::string_view in_color_name1,
                                std::string_view dest0, std::string_view dest1);
void GetLightingShaderUid(LightingUidData& uid_data);
