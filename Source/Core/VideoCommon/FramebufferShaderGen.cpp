// Copyright 2019 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoCommon/FramebufferShaderGen.h"

#include <string_view>

#include "Common/Logging/Log.h"

#include "VideoCommon/FramebufferManager.h"
#include "VideoCommon/ShaderGenCommon.h"
#include "VideoCommon/TextureDecoder.h"
#include "VideoCommon/VertexShaderGen.h"
#include "VideoCommon/VideoCommon.h"
#include "VideoCommon/VideoConfig.h"

namespace FramebufferShaderGen
{
namespace
{
APIType GetAPIType()
{
  return g_ActiveConfig.backend_info.api_type;
}

void EmitVertexInputsAndOutputs(ShaderCode& code, u32 num_tex_inputs, u32 num_color_inputs,
                                bool position_input, u32 num_tex_outputs, u32 num_color_outputs,
                                bool point_size = false, bool vid = false)
{
  code.Write("VERTEX_INPUT_DECL_BEGIN\n");
  if (vid)
    code.Write("  DECL_VERTEX_INPUT_VID\n");
  for (u32 i = 0; i < num_tex_inputs; i++)
    code.Write("  DECL_VERTEX_INPUT(float3, rawtex{}, TEXCOORD{}, {})\n", i, i, SHADER_TEXTURE0_ATTRIB + i);
  for (u32 i = 0; i < num_color_inputs; i++)
    code.Write("  DECL_VERTEX_INPUT(float4, rawcolor{}, COLOR{}, {})\n", i, i, SHADER_COLOR0_ATTRIB + i);
  if (position_input)
    code.Write("  DECL_VERTEX_INPUT(float4, rawpos, POSITION, {})\n", SHADER_POSITION_ATTRIB);
  code.Write("VERTEX_INPUT_DECL_END\n");
  code.Write("VERTEX_OUTPUT_DECL_BEGIN\n");
  for (u32 i = 0; i < num_tex_outputs; i++)
    code.Write("  DECL_VERTEX_OUTPUT(float3, v_tex{}, TEXCOORD{}, {})\n", i, i, i);
  for (u32 i = 0; i < num_color_outputs; i++)
    code.Write("  DECL_VERTEX_OUTPUT(float4, v_col{}, COLOR{}, {})\n", i, i, num_tex_outputs + i);
  code.Write("  DECL_VERTEX_OUTPUT_POSITION\n");
  if (point_size && g_ActiveConfig.backend_info.bSupportsLargePoints)
    code.Write("  DECL_VERTEX_OUTPUT_POINT_SIZE\n");
  code.Write("VERTEX_OUTPUT_DECL_END\n\n");
}

void EmitPixelInputsAndOutputs(ShaderCode& code, u32 num_tex_inputs, u32 num_color_inputs,
                               std::string_view output_type = "float4",
                               std::string_view extra_vars = {}, bool emit_frag_coord = false)
{
  code.Write("PIXEL_INPUT_DECL_BEGIN\n");
  for (u32 i = 0; i < num_tex_inputs; i++)
    code.Write("  DECL_PIXEL_INPUT(float3, v_tex{}, TEXCOORD{}, {})\n", i, i, i);
  for (u32 i = 0; i < num_color_inputs; i++)
    code.Write("  DECL_PIXEL_INPUT(float4, v_col{}, COLOR{}, {})\n", i, i, num_tex_inputs + i);
  if (emit_frag_coord)
    code.Write("  DECL_PIXEL_INPUT_POSITION\n");
  if (!extra_vars.empty())
    code.Write("  {};\n", extra_vars);
  code.Write("PIXEL_INPUT_DECL_END\n");
  code.Write("PIXEL_OUTPUT_DECL_BEGIN\n");
  code.Write("  DECL_PIXEL_OUTPUT_COLOR0({})\n", output_type);
  code.Write("PIXEL_OUTPUT_DECL_END\n\n");
}

}  // Anonymous namespace

std::string GenerateScreenQuadVertexShader()
{
  ShaderCode code;
  code.Write("// SHADER_SUPPORTS_MSL\n"
             "INPUT_DECL_BEGIN\n"
             "INPUT_DECL_END\n\n");
  EmitVertexInputsAndOutputs(code, 0, 0, false, 1, 0, false, true);
  code.Write(
      "DECL_MAIN\n"
      "{{\n"
      "  OUTPUT(v_tex0) = float3(float((vid << 1) & 2), float(vid & 2), 0.0f);\n"
      "  opos = float4(OUTPUT(v_tex0.xy) * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f), 0.0f, 1.0f);\n"
      "  // NDC space is flipped in Vulkan. We also flip in GL so that (0,0) is in the lower-left.\n"
      "  FIXUP_OPOS;\n"
      "}}\n");
  return code.GetBuffer();
}

std::string GeneratePassthroughGeometryShader(u32 num_tex, u32 num_colors)
{
  ShaderCode code;
  if (GetAPIType() == APIType::D3D)
  {
    code.Write("struct VS_OUTPUT\n"
               "{{\n");
    for (u32 i = 0; i < num_tex; i++)
      code.Write("  float3 tex{} : TEXCOORD{};\n", i, i);
    for (u32 i = 0; i < num_colors; i++)
      code.Write("  float4 color{} : COLOR{};\n", i, i);
    code.Write("  float4 position : SV_Position;\n"
               "}};\n");

    code.Write("struct GS_OUTPUT\n"
               "{{");
    for (u32 i = 0; i < num_tex; i++)
      code.Write("  float3 tex{} : TEXCOORD{};\n", i, i);
    for (u32 i = 0; i < num_colors; i++)
      code.Write("  float4 color{} : COLOR{};\n", i, i);
    code.Write("  float4 position : SV_Position;\n"
               "  uint slice : SV_RenderTargetArrayIndex;\n"
               "}};\n\n");

    code.Write("[maxvertexcount(6)]\n"
               "void main(triangle VS_OUTPUT vso[3], inout TriangleStream<GS_OUTPUT> output)\n"
               "{{\n"
               "  for (uint slice = 0; slice < 2u; slice++)\n"
               "  {{\n"
               "    for (int i = 0; i < 3; i++)\n"
               "    {{\n"
               "      GS_OUTPUT gso;\n"
               "      gso.position = vso[i].position;\n");
    for (u32 i = 0; i < num_tex; i++)
      code.Write("      gso.tex{} = float3(vso[i].tex{}.xy, float(slice));\n", i, i);
    for (u32 i = 0; i < num_colors; i++)
      code.Write("      gso.color{} = vso[i].color{};\n", i, i);
    code.Write("      gso.slice = slice;\n"
               "      output.Append(gso);\n"
               "    }}\n"
               "    output.RestartStrip();\n"
               "  }}\n"
               "}}\n");
  }
  else if (GetAPIType() == APIType::OpenGL || GetAPIType() == APIType::Vulkan || GetAPIType() == APIType::Metal)
  {
    code.Write("layout(triangles) in;\n"
               "layout(triangle_strip, max_vertices = 6) out;\n");

    if (num_tex > 0 || num_colors > 0)
    {
      code.Write("VARYING_LOCATION(0) in VertexData {{\n");
      for (u32 i = 0; i < num_tex; i++)
        code.Write("  float3 v_tex{};\n", i);
      for (u32 i = 0; i < num_colors; i++)
        code.Write("  float4 v_col{};\n", i);
      code.Write("}} v_in[];\n");

      code.Write("VARYING_LOCATION(0) out VertexData {{\n");
      for (u32 i = 0; i < num_tex; i++)
        code.Write("  float3 v_tex{};\n", i);
      for (u32 i = 0; i < num_colors; i++)
        code.Write("  float4 v_col{};\n", i);
      code.Write("}} v_out;\n");
    }
    code.Write("\n"
               "void main()\n"
               "{{\n"
               "  for (int j = 0; j < 2; j++)\n"
               "  {{\n"
               "    gl_Layer = j;\n");

    // We have to explicitly unroll this loop otherwise the GL compiler gets cranky.
    for (u32 v = 0; v < 3; v++)
    {
      code.Write("    gl_Position = gl_in[{}].gl_Position;\n", v);
      for (u32 i = 0; i < num_tex; i++)
      {
        code.Write("    v_out.v_tex{} = float3(v_in[{}].v_tex{}.xy, float(j));\n", i, v, i);
      }
      for (u32 i = 0; i < num_colors; i++)
        code.Write("    v_out.v_col{} = v_in[{}].v_col{};\n", i, v, i);
      code.Write("    EmitVertex();\n\n");
    }
    code.Write("    EndPrimitive();\n"
               "  }}\n"
               "}}\n");
  }

  return code.GetBuffer();
}

std::string GenerateTextureCopyVertexShader()
{
  ShaderCode code;
  code.Write("// SHADER_SUPPORTS_MSL\n"
             "DECL_CB_UTILITY\n"
             "{{\n"
             "  float2 src_offset;\n"
             "  float2 src_size;\n"
             "}};\n"
             "\n"
             "INPUT_DECL_BEGIN\n"
             "  DECL_INPUT_CB_UTILITY\n"
             "INPUT_DECL_END\n\n");
  EmitVertexInputsAndOutputs(code, 0, 0, false, 1, 0, false, true);
  code.Write(
    "DECL_MAIN\n"
    "{{\n"
    "  float3 tmp = float3(float((vid << 1) & 2), float(vid & 2), 0.0f);\n"
    "  opos = float4(tmp.xy * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f), 0.0f, 1.0f);\n"
    "  OUTPUT(v_tex0) = float3(CB_UTILITY(src_offset) + (CB_UTILITY(src_size) * tmp.xy), 0.0f);\n"
    "  // NDC space is flipped in Vulkan. We also flip in GL so that (0,0) is in the lower-left.\n"
    "  FIXUP_OPOS;\n"
    "}}\n");
  return code.GetBuffer();
}

std::string GenerateTextureCopyPixelShader()
{
  ShaderCode code;
  code.Write("// SHADER_SUPPORTS_MSL\n"
             "INPUT_DECL_BEGIN\n"
             "  DECL_INPUT_TEXTURE(tex0, 0)\n"
             "  DECL_INPUT_SAMPLER(samp0, 0)\n"
             "INPUT_DECL_END\n\n");
  EmitPixelInputsAndOutputs(code, 1, 0);
  code.Write("DECL_MAIN\n"
             "{{\n"
             "  ocol0 = TEXTURE_SAMPLE(tex0, samp0, INPUT(v_tex0));\n"
             "}}\n");
  return code.GetBuffer();
}

std::string GenerateColorPixelShader()
{
  ShaderCode code;
  code.Write("// SHADER_SUPPORTS_MSL\n"
             "INPUT_DECL_BEGIN\n"
             "INPUT_DECL_END\n\n");
  EmitPixelInputsAndOutputs(code, 0, 1);
  code.Write("DECL_MAIN\n"
             "{{\n"
             "  ocol0 = INPUT(v_col0);\n"
             "}}\n");
  return code.GetBuffer();
}

std::string GenerateResolveDepthPixelShader(u32 samples)
{
  ShaderCode code;
  code.Write("// SHADER_SUPPORTS_MSL\n"
             "INPUT_DECL_BEGIN\n"
             "  DECL_INPUT_DEPTH_MS(tex0, 0)\n"
             "INPUT_DECL_END\n\n");
  EmitPixelInputsAndOutputs(code, 1, 0, "float", {}, true);
  code.Write("DECL_MAIN\n"
             "{{\n"
             "  int layer = int(INPUT(v_tex0.z));\n"
             "  int2 coords = int2(frag_coord.xy);\n"
             "  ocol0 = DEPTH_FETCH_MS(tex0, coords, layer, 0).r;\n");
  code.Write("  for (int i = 0; i < {}; i++)\n", samples);
  code.Write("    ocol0 = min(ocol0, DEPTH_FETCH_MS(tex0, coords, layer, i).r);\n"
             "}}\n");
  return code.GetBuffer();
}

std::string GenerateClearVertexShader()
{
  ShaderCode code;
  code.Write("// SHADER_SUPPORTS_MSL\n"
             "DECL_CB_UTILITY\n"
             "{{\n"
             "  float4 clear_color;\n"
             "  float clear_depth;\n"
             "}};\n"
             "INPUT_DECL_BEGIN\n"
             "  DECL_INPUT_CB_UTILITY\n"
             "INPUT_DECL_END\n");
  EmitVertexInputsAndOutputs(code, 0, 0, false, 0, 1, false, true);
  code.Write("DECL_MAIN\n"
             "{{\n"
             "  float2 coord = float2(float((vid << 1) & 2), float(vid & 2));\n"
             "  opos = float4(coord * float2(2.0f, -2.0f) + float2(-1.0f, 1.0f),\n"
             "                CB_UTILITY(clear_depth), 1.0f);\n"
             "  OUTPUT(v_col0) = CB_UTILITY(clear_color);\n"
             "  FIXUP_OPOS_VK;\n"
             "}}\n");

  return code.GetBuffer();
}

std::string GenerateEFBPokeVertexShader()
{
  ShaderCode code;
  code.Write("// SHADER_SUPPORTS_MSL\n"
             "INPUT_DECL_BEGIN\n"
             "INPUT_DECL_END\n\n");
  EmitVertexInputsAndOutputs(code, 0, 1, true, 0, 1, true);
  code.Write("DECL_MAIN\n"
             "{{\n"
             "OUTPUT(v_col0) = INPUT(rawcolor0);\n"
             "opos = float4(INPUT(rawpos.xyz), 1.0f);\n");
  if (g_ActiveConfig.backend_info.bSupportsLargePoints)
    code.Write("  gl_PointSize = INPUT(rawpos.w);\n");
  code.Write("  FIXUP_OPOS_VK;\n}}\n");
  return code.GetBuffer();
}

std::string GenerateFormatConversionShader(EFBReinterpretType convtype, u32 samples)
{
  ShaderCode code;
  code.Write("// SHADER_SUPPORTS_MSL\n");
  code.Write("INPUT_DECL_BEGIN\n");
  code.Write("  DECL_INPUT_TEXTURE{}(tex0, 0)\n", samples > 1 ? "_MS" : "");
  code.Write("  DECL_INPUT_SAMPLER(samp0, 0)\n");
  code.Write("INPUT_DECL_END\n\n");
  EmitPixelInputsAndOutputs(code, 1, 0, "float4", g_ActiveConfig.bSSAA ? "DECL_PIXEL_INPUT_SAMPLE_IDX" : "", true);
  code.Write("DECL_MAIN\n"
             "{{\n"
             "  int layer = int(INPUT(v_tex0.z));\n"
             "  int2 coords = int2(frag_coord.xy);\n");
  if (samples == 1)
  {
    // No MSAA at all.
    code.Write("  float4 val = TEXTURE_FETCH(tex0, coords, layer);\n");
  }
  else if (g_ActiveConfig.bSSAA)
  {
    // Sample shading, shader runs once per sample
    code.Write("  float4 val = TEXTURE_FETCH_MS(tex0, coords, layer, gl_SampleID);\n");
  }
  else
  {
    // MSAA without sample shading, average out all samples.
    code.Write("  float4 val = float4(0.0f, 0.0f, 0.0f, 0.0f);\n");
    code.Write("  for (int i = 0; i < {}; i++)\n", samples);
    code.Write("    val += TEXTURE_FETCH_MS(tex0, coords, layer, i);\n");
    code.Write("  val /= float({});\n", samples);
  }

  switch (convtype)
  {
  case EFBReinterpretType::RGB8ToRGBA6:
    code.Write("  int4 src8 = int4(round(val * 255.f));\n"
               "  int4 dst6;\n"
               "  dst6.r = src8.r >> 2;\n"
               "  dst6.g = ((src8.r & 0x3) << 4) | (src8.g >> 4);\n"
               "  dst6.b = ((src8.g & 0xF) << 2) | (src8.b >> 6);\n"
               "  dst6.a = src8.b & 0x3F;\n"
               "  ocol0 = float4(dst6) / 63.f;\n");
    break;

  case EFBReinterpretType::RGB8ToRGB565:
    code.Write("  ocol0 = val;\n");
    break;

  case EFBReinterpretType::RGBA6ToRGB8:
    code.Write("  int4 src6 = int4(round(val * 63.f));\n"
               "  int4 dst8;\n"
               "  dst8.r = (src6.r << 2) | (src6.g >> 4);\n"
               "  dst8.g = ((src6.g & 0xF) << 4) | (src6.b >> 2);\n"
               "  dst8.b = ((src6.b & 0x3) << 6) | src6.a;\n"
               "  dst8.a = 255;\n"
               "  ocol0 = float4(dst8) / 255.f;\n");
    break;

  case EFBReinterpretType::RGBA6ToRGB565:
    code.Write("  ocol0 = val;\n");
    break;

  case EFBReinterpretType::RGB565ToRGB8:
    code.Write("  ocol0 = val;\n");
    break;

  case EFBReinterpretType::RGB565ToRGBA6:
    //
    code.Write("  ocol0 = val;\n");
    break;
  }

  code.Write("}}\n");
  return code.GetBuffer();
}

std::string GenerateTextureReinterpretShader(TextureFormat from_format, TextureFormat to_format)
{
  ShaderCode code;
  code.Write("// SHADER_SUPPORTS_MSL\n"
             "INPUT_DECL_BEGIN\n"
             "  DECL_INPUT_TEXTURE(tex0, 0)\n"
             "INPUT_DECL_END\n\n");
  EmitPixelInputsAndOutputs(code, 1, 0, "float4", {}, true);
  code.Write("DECL_MAIN\n"
             "{{\n"
             "  int layer = int(INPUT(v_tex0.z));\n"
             "  int2 coords = int2(frag_coord.xy);\n");

  // Convert to a 32-bit value encompassing all channels, filling the most significant bits with zeroes.
  code.Write("  uint raw_value;\n");
  switch (from_format)
  {
  case TextureFormat::I8:
  case TextureFormat::C8:
  {
    code.Write("  float4 temp_value = TEXTURE_FETCH(tex0, coords, layer);\n"
               "  raw_value = uint(temp_value.r * 255.0);\n");
  }
  break;

  case TextureFormat::IA8:
  {
    code.Write("  float4 temp_value = TEXTURE_FETCH(tex0, coords, layer);\n"
               "  raw_value = uint(temp_value.r * 255.0) | (uint(temp_value.a * 255.0) << 8);\n");
  }
  break;

  case TextureFormat::I4:
  {
    code.Write("  float4 temp_value = TEXTURE_FETCH(tex0, coords, layer);\n"
               "  raw_value = uint(temp_value.r * 15.0);\n");
  }
  break;

  case TextureFormat::IA4:
  {
    code.Write("  float4 temp_value = TEXTURE_FETCH(tex0, coords, layer);"
               "  raw_value = uint(temp_value.r * 15.0) | (uint(temp_value.a * 15.0) << 4);\n");
  }
  break;

  case TextureFormat::RGB565:
  {
    code.Write("  float4 temp_value = TEXTURE_FETCH(tex0, coords, layer);"
               "  raw_value = uint(temp_value.b * 31.0) | (uint(temp_value.g * 63.0) << 5) |\n"
               "              (uint(temp_value.r * 31.0) << 11);\n");
  }
  break;

  case TextureFormat::RGB5A3:
  {
    // 0.8784 = 224 / 255 which is the maximum alpha value that can be represented in 3 bits
    code.Write(
        "  float4 temp_value = TEXTURE_FETCH(tex0, coords, layer);"
        "  if (temp_value.a > 0.878f) {{\n"
        "    raw_value = (uint(temp_value.b * 31.0)) | (uint(temp_value.g * 31.0) << 5) |\n"
        "                (uint(temp_value.r * 31.0) << 10) | 0x8000u;\n"
        "  }} else {{\n"
        "     raw_value = (uint(temp_value.b * 15.0)) | (uint(temp_value.g * 15.0) << 4) |\n"
        "                 (uint(temp_value.r * 15.0) << 8) | (uint(temp_value.a * 7.0) << 12);\n"
        "  }}\n");
  }
  break;

  default:
    WARN_LOG_FMT(VIDEO, "From format {} is not supported", static_cast<u32>(from_format));
    return "{}\n";
  }

  // Now convert it to its new representation.
  switch (to_format)
  {
  case TextureFormat::I8:
  case TextureFormat::C8:
  {
    code.Write("  float orgba = float(raw_value & 0xFFu) / 255.0;\n"
               "  ocol0 = float4(orgba, orgba, orgba, orgba);\n");
  }
  break;

  case TextureFormat::IA8:
  {
    code.Write("  float orgb = float(raw_value & 0xFFu) / 255.0;\n"
               "  ocol0 = float4(orgb, orgb, orgb, float((raw_value >> 8) & 0xFFu) / 255.0);\n");
  }
  break;

  case TextureFormat::IA4:
  {
    code.Write("  float orgb = float(raw_value & 0xFu) / 15.0;\n"
               "  ocol0 = float4(orgb, orgb, orgb, float((raw_value >> 4) & 0xFu) / 15.0);\n");
  }
  break;

  case TextureFormat::RGB565:
  {
    code.Write("  ocol0 = float4(float((raw_value >> 10) & 0x1Fu) / 31.0,\n"
               "                 float((raw_value >> 5) & 0x1Fu) / 31.0,\n"
               "                 float(raw_value & 0x1Fu) / 31.0, 1.0);\n");
  }
  break;

  case TextureFormat::RGB5A3:
  {
    code.Write("  if ((raw_value & 0x8000u) != 0u) {{\n"
               "    ocol0 = float4(float((raw_value >> 10) & 0x1Fu) / 31.0,\n"
               "                   float((raw_value >> 5) & 0x1Fu) / 31.0,\n"
               "                   float(raw_value & 0x1Fu) / 31.0, 1.0);\n"
               "  }} else {{\n"
               "    ocol0 = float4(float((raw_value >> 8) & 0x0Fu) / 15.0,\n"
               "                   float((raw_value >> 4) & 0x0Fu) / 15.0,\n"
               "                   float(raw_value & 0x0Fu) / 15.0,\n"
               "                   float((raw_value >> 12) & 0x07u) / 7.0);\n"
               "  }}\n");
  }
  break;
  default:
    WARN_LOG_FMT(VIDEO, "To format {} is not supported", static_cast<u32>(to_format));
    return "{}\n";
  }

  code.Write("}}\n");
  return code.GetBuffer();
}

std::string GenerateEFBRestorePixelShader()
{
  ShaderCode code;
  code.Write("// SHADER_SUPPORTS_MSL\n"
             "INPUT_DECL_BEGIN\n"
             "  DECL_INPUT_TEXTURE(tex0, 0)\n"
             "  DECL_INPUT_DEPTH(tex1, 1)\n"
             "  DECL_INPUT_SAMPLER(samp0, 0)\n"
             "  DECL_INPUT_SAMPLER(samp1, 1)\n"
             "INPUT_DECL_END\n"
             "PIXEL_INPUT_DECL_BEGIN\n"
             "  DECL_PIXEL_INPUT(float3, v_tex0, TEXCOORD0, 0)\n"
             "PIXEL_INPUT_DECL_END\n"
             "PIXEL_OUTPUT_DECL_BEGIN\n"
             "  DECL_PIXEL_OUTPUT_COLOR0(float4)\n"
             "  DECL_PIXEL_OUTPUT_DEPTH\n"
             "PIXEL_OUTPUT_DECL_END\n\n"
             "DECL_MAIN\n"
             "{{\n"
             "  ocol0 = TEXTURE_SAMPLE(tex0, samp0, INPUT(v_tex0));\n"
             "  odepth = DEPTH_SAMPLE(tex1, samp1, INPUT(v_tex0)).r;\n"
             "}}\n");
  return code.GetBuffer();
}

std::string GenerateImGuiVertexShader()
{
  ShaderCode code;
  code.Write(
    "// SHADER_SUPPORTS_MSL\n"
    "DECL_CB_UTILITY\n"
    "{{\n"
    "  // Uniform buffer contains the viewport size, and we transform in the vertex shader.\n"
    "  float2 rcp_viewport_size_mul2;\n"
    "}};\n"
    "\n"
    "INPUT_DECL_BEGIN\n"
    "  DECL_INPUT_CB_UTILITY\n"
    "INPUT_DECL_END\n");
  EmitVertexInputsAndOutputs(code, 1, 1, true, 1, 1);
  code.Write("DECL_MAIN\n"
             "{{\n"
             "  OUTPUT(v_tex0) = float3(INPUT(rawtex0.xy), 0.0);\n"
             "  OUTPUT(v_col0) = INPUT(rawcolor0);\n"
             "  opos = float4(INPUT(rawpos.x) * CB_UTILITY(rcp_viewport_size_mul2.x) - 1.0,\n"
             "                1.0 - INPUT(rawpos.y) * CB_UTILITY(rcp_viewport_size_mul2.y),\n"
             "                0.0, 1.0);\n"
             "  FIXUP_OPOS_VK;\n"
             "}}\n");
  return code.GetBuffer();
}

std::string GenerateImGuiPixelShader()
{
  ShaderCode code;
  code.Write("// SHADER_SUPPORTS_MSL\n"
             "INPUT_DECL_BEGIN\n"
             "  DECL_INPUT_TEXTURE(tex0, 0)\n"
             "  DECL_INPUT_SAMPLER(samp0, 0)\n"
             "INPUT_DECL_END\n\n");
  EmitPixelInputsAndOutputs(code, 1, 1);
  code.Write("DECL_MAIN\n"
             "{{\n"
             "  ocol0 = TEXTURE_SAMPLE_LAYER(tex0, samp0, INPUT(v_tex0.xy), 0) * INPUT(v_col0);\n"
             "}}\n");
  return code.GetBuffer();
}

}  // namespace FramebufferShaderGen
