// Copyright 2015 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoCommon/UberShaderVertex.h"

#include "VideoCommon/DriverDetails.h"
#include "VideoCommon/NativeVertexFormat.h"
#include "VideoCommon/UberShaderCommon.h"
#include "VideoCommon/VertexShaderGen.h"
#include "VideoCommon/VideoCommon.h"
#include "VideoCommon/XFMemory.h"

namespace UberShader
{
VertexShaderUid GetVertexShaderUid()
{
  VertexShaderUid out;

  vertex_ubershader_uid_data* const uid_data = out.GetUidData();
  uid_data->num_texgens = xfmem.numTexGen.numTexGens;

  return out;
}

static void GenVertexShaderTexGens(APIType api_type, u32 num_texgen, ShaderCode& out);

ShaderCode GenVertexShader(APIType api_type, const ShaderHostConfig& host_config,
                           const vertex_ubershader_uid_data* uid_data)
{
  const bool msaa = host_config.msaa;
  const bool ssaa = host_config.ssaa;
  const bool per_pixel_lighting = host_config.per_pixel_lighting;
  const bool vertex_rounding = host_config.vertex_rounding;
  const u32 num_texgen = uid_data->num_texgens;
  ShaderCode out;

  out.Write("// Vertex UberShader\n\n");
  out.Write("// SHADER_SUPPORTS_MSL\n\n");
  out.Write("{}", s_lighting_struct);

  // uniforms
  out.Write("DECL_CB_VS {{\n"
            "{}\n"
            "}};\n",
            s_shader_uniforms);

  if (api_type == APIType::Metal)
  {
    // Metal uses a dynamic vertex fetch routine to avoid needing to compile separate pipelines
    //   for each vertex layout
    out.Write(R"(
struct VertexLayoutMetadata {{
  uint offsets[{nattributes}];
  uint stride;
}};

INPUT_DECL_BEGIN
  DECL_INPUT_CB_VS
  device const void* vertex_data [[buffer(0)]];
  constant VertexLayoutMetadata& vertex_layout [[buffer(2)]];
INPUT_DECL_END
VERTEX_INPUT_DECL_BEGIN
  DECL_VERTEX_INPUT_VID
VERTEX_INPUT_DECL_END

template <typename T, typename U> struct InputAttribute {{ uint idx; }};

device const void* GetInputPtr(uint offset)
{{
  uint full_offset = input.vertex_layout.stride * vid + offset;
  return static_cast<device const void*>(static_cast<device const char*>(input.vertex_data) + full_offset);
}}

float4 GetInput(InputAttribute<float4, float> marker)
{{
  uint info = input.vertex_layout.offsets[marker.idx];
  device const float* ptr = static_cast<device const float*>(GetInputPtr(info & 0xffff));
  switch (info >> 16)
  {{
    case 4u: return float4(ptr[0], ptr[1], ptr[2], ptr[3]);
    case 3u: return float4(ptr[0], ptr[1], ptr[2], 1.0f);
    case 2u: return float4(ptr[0], ptr[1], 0.0f, 1.0f);
    case 1u: return float4(ptr[0], 0.0f, 0.0f, 1.0f);
    default: return float4(0.0f, 0.0f, 0.0f, 1.0f);
  }}
}}

float3 GetInput(InputAttribute<float3, float> marker)
{{
  uint info = input.vertex_layout.offsets[marker.idx];
  device const float* ptr = static_cast<device const float*>(GetInputPtr(info & 0xffff));
  switch (info >> 16)
  {{
    case 3u: return float3(ptr[0], ptr[1], ptr[2]);
    case 2u: return float3(ptr[0], ptr[1], 0.0f);
    case 1u: return float3(ptr[0], 0.0f, 0.0f);
    default: return float3(0.0f, 0.0f, 0.0f);
  }}
}}

template <uint N>
vec<float, N> GetInput(InputAttribute<vec<float, N>, vec<float, N>> marker)
{{
  device const void* ptr = GetInputPtr(input.vertex_layout.offsets[marker.idx]);
  return *static_cast<device const vec<float, N>*>(ptr);
}}

template <uint N>
vec<float, N> GetInput(InputAttribute<vec<float, N>, vec<uchar, N>> marker)
{{
  device const void* ptr = GetInputPtr(input.vertex_layout.offsets[marker.idx]);
  return vec<float, N>(*static_cast<device const vec<uchar, N>*>(ptr)) / 255.0f;
}}

template <uint N>
vec<uint, N> GetInput(InputAttribute<vec<uint, N>, vec<uchar, N>> marker)
{{
  device const void* ptr = GetInputPtr(input.vertex_layout.offsets[marker.idx]);
  return vec<uint, N>(*static_cast<device const vec<uchar, N>*>(ptr));
}}

#undef INPUT
#define INPUT(name) GetInput(_iattr_##name)
)", fmt::arg("nattributes", SHADER_TEXTURE7_ATTRIB + 1));

    out.Write("static constexpr constant InputAttribute<float4, float> _iattr_rawpos{{{}}};\n",
              SHADER_POSITION_ATTRIB);
    out.Write("static constexpr constant InputAttribute<uint4, uchar4> _iattr_posmtx{{{}}};\n",
              SHADER_POSMTX_ATTRIB);
    out.Write("static constexpr constant InputAttribute<float3, float3> _iattr_rawnormal{{{}}};\n",
              SHADER_NORMAL_ATTRIB);
    out.Write("static constexpr constant InputAttribute<float3, float3> _iattr_rawtangent{{{}}};\n",
              SHADER_TANGENT_ATTRIB);
    out.Write("static constexpr constant InputAttribute<float3, float3> _iattr_rawbinormal{{{}}};\n",
              SHADER_BINORMAL_ATTRIB);
    out.Write("static constexpr constant InputAttribute<float4, uchar4> _iattr_rawcolor0{{{}}};\n",
              SHADER_COLOR0_ATTRIB);
    out.Write("static constexpr constant InputAttribute<float4, uchar4> _iattr_rawcolor1{{{}}};\n",
              SHADER_COLOR1_ATTRIB);
    for (int i = 0; i < 8; i++)
    {
      out.Write("static constexpr constant InputAttribute<float3, float> _iattr_rawtex{}{{{}}};\n",
                i, SHADER_TEXTURE0_ATTRIB + i);
    }
  }
  else
  {
    out.Write("INPUT_DECL_BEGIN\n"
              "  DECL_INPUT_CB_VS\n"
              "INPUT_DECL_END\n\n");

    out.Write("VERTEX_INPUT_DECL_BEGIN\n");
    out.Write("  DECL_VERTEX_INPUT(float4, rawpos, POSITION, {})\n", SHADER_POSITION_ATTRIB);
    out.Write("  DECL_VERTEX_INPUT(uint4, posmtx, BLENDINDICES, {})\n", SHADER_POSMTX_ATTRIB);
    out.Write("  DECL_VERTEX_INPUT(float3, rawnormal, NORMAL, {})\n", SHADER_NORMAL_ATTRIB);
    out.Write("  DECL_VERTEX_INPUT(float3, rawtangent, TANGENT, {})\n", SHADER_TANGENT_ATTRIB);
    out.Write("  DECL_VERTEX_INPUT(float3, rawbinormal, BINORMAL, {})\n", SHADER_BINORMAL_ATTRIB);
    out.Write("  DECL_VERTEX_INPUT(float4, rawcolor0, COLOR0, {})\n", SHADER_COLOR0_ATTRIB);
    out.Write("  DECL_VERTEX_INPUT(float4, rawcolor1, COLOR1, {})\n", SHADER_COLOR1_ATTRIB);
    for (int i = 0; i < 8; i++)
    {
      out.Write("  DECL_VERTEX_INPUT(float3, rawtex{}, TEXCOORD{}, {})\n",
                i, i, SHADER_TEXTURE0_ATTRIB + i);
    }
    out.Write("VERTEX_INPUT_DECL_END\n\n");
  }

  const char* qualifier = GetInterpolationQualifier(api_type, msaa, ssaa);
  if (host_config.backend_geometry_shaders)
  {
    if (api_type == APIType::OpenGL || api_type == APIType::Vulkan)
      qualifier = GetInterpolationQualifier(api_type, msaa, ssaa, true, false);
  }
  out.Write("VERTEX_OUTPUT_DECL_BEGIN\n");
  GenerateVSOutputMembers(out, num_texgen, host_config, qualifier, "VERTEX_OUTPUT");
  out.Write("VERTEX_OUTPUT_DECL_END\n\n");

  WriteIsNanHeader(out, api_type);
  WriteBitfieldExtractHeader(out, api_type, host_config);
  WriteLightingFunction(out);

  out.Write("DECL_MAIN {{\n");
  if (api_type != APIType::D3D && host_config.backend_geometry_shaders)
  {
    out.Write("  float clipDist0;\n"
              "  float clipDist1;\n");
  }
  // Transforms
  out.Write("// Position matrix\n"
            "float4 P0;\n"
            "float4 P1;\n"
            "float4 P2;\n"
            "\n"
            "// Normal matrix\n"
            "float3 N0;\n"
            "float3 N1;\n"
            "float3 N2;\n"
            "\n"
            "if ((CB_VS(components) & {}u) != 0u) {{ // VB_HAS_POSMTXIDX\n",
            VB_HAS_POSMTXIDX);
  out.Write("  // Vertex format has a per-vertex matrix\n"
            "  int posidx = int(INPUT(posmtx).r);\n"
            "  P0 = CB_VS(" I_TRANSFORMMATRICES ")[posidx];\n"
            "  P1 = CB_VS(" I_TRANSFORMMATRICES ")[posidx+1];\n"
            "  P2 = CB_VS(" I_TRANSFORMMATRICES ")[posidx+2];\n"
            "\n"
            "  int normidx = posidx >= 32 ? (posidx - 32) : posidx;\n"
            "  N0 = CB_VS(" I_NORMALMATRICES ")[normidx].xyz;\n"
            "  N1 = CB_VS(" I_NORMALMATRICES ")[normidx+1].xyz;\n"
            "  N2 = CB_VS(" I_NORMALMATRICES ")[normidx+2].xyz;\n"
            "}} else {{\n"
            "  // One shared matrix\n"
            "  P0 = CB_VS(" I_POSNORMALMATRIX ")[0];\n"
            "  P1 = CB_VS(" I_POSNORMALMATRIX ")[1];\n"
            "  P2 = CB_VS(" I_POSNORMALMATRIX ")[2];\n"
            "  N0 = CB_VS(" I_POSNORMALMATRIX ")[3].xyz;\n"
            "  N1 = CB_VS(" I_POSNORMALMATRIX ")[4].xyz;\n"
            "  N2 = CB_VS(" I_POSNORMALMATRIX ")[5].xyz;\n"
            "}}\n"
            "\n"
            "// Multiply the position vector by the position matrix\n"
            "float4 pos = float4(dot(P0, INPUT(rawpos)), dot(P1, INPUT(rawpos)), dot(P2, INPUT(rawpos)), 1.0);\n"
            "opos = float4(dot(CB_VS(" I_PROJECTION ")[0], pos), dot(CB_VS(" I_PROJECTION ")[1], "
            "pos), dot(CB_VS(" I_PROJECTION ")[2], pos), dot(CB_VS(" I_PROJECTION ")[3], pos));\n"
            "\n"
            "// The scale of the transform matrix is used to control the size of the emboss map\n"
            "// effect by changing the scale of the transformed binormals (which only get used by\n"
            "// emboss map texgens). By normalising the first transformed normal (which is used\n"
            "// by lighting calculations and needs to be unit length), the same transform matrix\n"
            "// can do double duty, scaling for emboss mapping, and not scaling for lighting.\n"
            "float3 _normal = float3(0.0, 0.0, 0.0);\n"
            "if ((CB_VS(components) & {}u) != 0u) // VB_HAS_NORMAL\n",
            VB_HAS_NORMAL);
  out.Write("  _normal = normalize(float3(dot(N0, INPUT(rawnormal)), dot(N1, INPUT(rawnormal)),"
            " dot(N2, INPUT(rawnormal))));\n"
            "\n"
            "float3 _tangent = float3(0.0, 0.0, 0.0);\n"
            "if ((CB_VS(components) & {}u) != 0u) // VB_HAS_TANGENT\n",
            VB_HAS_TANGENT);
  out.Write("  _tangent = float3(dot(N0, INPUT(rawtangent)), dot(N1, INPUT(rawtangent)),"
            " dot(N2, INPUT(rawtangent)));\n"
            "else\n"
            "  _tangent = float3(dot(N0, CB_VS(" I_CACHED_TANGENT ").xyz), dot(N1, CB_VS("
            I_CACHED_TANGENT ").xyz), dot(N2, CB_VS(" I_CACHED_TANGENT ").xyz));\n"
            "\n"
            "float3 _binormal = float3(0.0, 0.0, 0.0);\n"
            "if ((CB_VS(components) & {}u) != 0u) // VB_HAS_BINORMAL\n",
            VB_HAS_BINORMAL);
  out.Write("  _binormal = float3(dot(N0, INPUT(rawbinormal)), dot(N1, INPUT(rawbinormal)),"
            " dot(N2, INPUT(rawbinormal)));\n"
            "else\n"
            "  _binormal = float3(dot(N0, CB_VS(" I_CACHED_BINORMAL ").xyz), dot(N1, CB_VS("
            I_CACHED_BINORMAL ").xyz), dot(N2, CB_VS(" I_CACHED_BINORMAL ").xyz));\n"
            "\n");

  // Hardware Lighting
  out.Write("// xfmem.numColorChans controls the number of color channels available to TEV,\n"
            "// but we still need to generate all channels here, as it can be used in texgen.\n"
            "// Cel-damage is an example of this.\n"
            "float4 vertex_color_0, vertex_color_1;\n"
            "\n");
  out.Write("// To use color 1, the vertex descriptor must have color 0 and 1.\n"
            "// If color 1 is present but not color 0, it is used for lighting channel 0.\n"
            "bool use_color_1 = ((CB_VS(components) & {0}u) == {0}u); // VB_HAS_COL0 | VB_HAS_COL1\n",
            VB_HAS_COL0 | VB_HAS_COL1);

  out.Write("for (uint color = 0u; color < {}u; color++) {{\n", NUM_XF_COLOR_CHANNELS);
  out.Write("  if ((color == 0u || use_color_1) && (CB_VS(components) & ({}u << color)) != 0u) {{\n",
            VB_HAS_COL0);
  out.Write("    // Use color0 for channel 0, and color1 for channel 1 if both colors 0 and 1 are "
            "present.\n"
            "    if (color == 0u)\n"
            "      vertex_color_0 = INPUT(rawcolor0);\n"
            "    else\n"
            "      vertex_color_1 = INPUT(rawcolor1);\n"
            "  }} else if (color == 0u && (CB_VS(components) & {}u) != 0u) {{\n",
            VB_HAS_COL1);
  out.Write("    // Use color1 for channel 0 if color0 is not present.\n"
            "    vertex_color_0 = INPUT(rawcolor1);\n"
            "  }} else {{\n"
            "    if (color == 0u)\n"
            "      vertex_color_0 = CB_VS(missing_color_value);\n"
            "    else\n"
            "      vertex_color_1 = CB_VS(missing_color_value);\n"
            "  }}\n"
            "}}\n"
            "\n");

  WriteVertexLighting(out, api_type, "pos.xyz", "_normal", "vertex_color_0", "vertex_color_1",
                      "OUTPUT(colors_0)", "OUTPUT(colors_1)");

  // Texture Coordinates
  if (num_texgen > 0)
    GenVertexShaderTexGens(api_type, num_texgen, out);

  if (per_pixel_lighting)
  {
    out.Write("// When per-pixel lighting is enabled, the vertex colors are passed through\n"
              "// unmodified so we can evaluate the lighting in the pixel shader.\n"
              "// Lighting is also still computed in the vertex shader since it can be used to\n"
              "// generate texture coordinates. We generated them above, so now the colors can\n"
              "// be reverted to their previous stage.\n"
              "OUTPUT(colors_0) = vertex_color_0;\n"
              "OUTPUT(colors_1) = vertex_color_1;\n"
              "// Note that the numColorChans logic should be (but currently isn't)\n"
              "// performed in the pixel shader.\n");
  }
  else
  {
    out.Write("// The number of colors available to TEV is determined by numColorChans.\n"
              "// We have to provide the fields to match the interface, so set to zero\n"
              "// if it's not enabled.\n"
              "if (CB_VS(xfmem_numColorChans) == 0u)\n"
              "  OUTPUT(colors_0) = float4(0.0, 0.0, 0.0, 0.0);\n"
              "if (CB_VS(xfmem_numColorChans) <= 1u)\n"
              "  OUTPUT(colors_1) = float4(0.0, 0.0, 0.0, 0.0);\n");
  }

  if (!host_config.fast_depth_calc)
  {
    // clipPos/w needs to be done in pixel shader, not here
    out.Write("OUTPUT(clipPos) = opos;\n");
  }

  if (per_pixel_lighting)
  {
    out.Write("OUTPUT(Normal) = _normal;\n"
              "OUTPUT(WorldPos) = pos.xyz;\n");
  }

  // If we can disable the incorrect depth clipping planes using depth clamping, then we can do
  // our own depth clipping and calculate the depth range before the perspective divide if
  // necessary.
  if (host_config.backend_depth_clamp)
  {
    // Since we're adjusting z for the depth range before the perspective divide, we have to do our
    // own clipping. We want to clip so that -w <= z <= 0, which matches the console -1..0 range.
    // We adjust our depth value for clipping purposes to match the perspective projection in the
    // software backend, which is a hack to fix Sonic Adventure and Unleashed games.
    out.Write("float clipDepth = opos.z * (1.0 - 1e-7);\n"
              "oclipDist0 = clipDepth + opos.w;\n"  // Near: z < -w
              "oclipDist1 = -clipDepth;\n");         // Far: z > 0
  }

  // Write the true depth value. If the game uses depth textures, then the pixel shader will
  // override it with the correct values if not then early z culling will improve speed.
  // There are two different ways to do this, when the depth range is oversized, we process
  // the depth range in the vertex shader, if not we let the host driver handle it.
  //
  // Adjust z for the depth range. We're using an equation which incorperates a depth inversion,
  // so we can map the console -1..0 range to the 0..1 range used in the depth buffer.
  // We have to handle the depth range in the vertex shader instead of after the perspective
  // divide, because some games will use a depth range larger than what is allowed by the
  // graphics API. These large depth ranges will still be clipped to the 0..1 range, so these
  // games effectively add a depth bias to the values written to the depth buffer.
  out.Write("opos.z = opos.w * CB_VS(" I_PIXELCENTERCORRECTION ").w - "
            "opos.z * CB_VS(" I_PIXELCENTERCORRECTION ").z;\n");

  if (!host_config.backend_clip_control)
  {
    // If the graphics API doesn't support a depth range of 0..1, then we need to map z to
    // the -1..1 range. Unfortunately we have to use a substraction, which is a lossy floating-point
    // operation that can introduce a round-trip error.
    out.Write("opos.z = opos.z * 2.0 - opos.w;\n");
  }

  // Correct for negative viewports by mirroring all vertices. We need to negate the height here,
  // since the viewport height is already negated by the render backend.
  out.Write("opos.xy *= sign(CB_VS(" I_PIXELCENTERCORRECTION ").xy * float2(1.0, -1.0));\n");

  // The console GPU places the pixel center at 7/12 in screen space unless
  // antialiasing is enabled, while D3D and OpenGL place it at 0.5. This results
  // in some primitives being placed one pixel too far to the bottom-right,
  // which in turn can be critical if it happens for clear quads.
  // Hence, we compensate for this pixel center difference so that primitives
  // get rasterized correctly.
  out.Write("opos.xy = opos.xy - opos.w * CB_VS(" I_PIXELCENTERCORRECTION ").xy;\n");

  if (vertex_rounding)
  {
    // By now our position is in clip space. However, higher resolutions than the Wii outputs
    // cause an additional pixel offset. Due to a higher pixel density we need to correct this
    // by converting our clip-space position into the Wii's screen-space.
    // Acquire the right pixel and then convert it back.
    out.Write("if (opos.w == 1.0f)\n"
              "{{\n");

    out.Write("\tfloat ss_pixel_x = ((opos.x + 1.0f) * (CB_VS(" I_VIEWPORT_SIZE ").x * 0.5f));\n"
              "\tfloat ss_pixel_y = ((opos.y + 1.0f) * (CB_VS(" I_VIEWPORT_SIZE ").y * 0.5f));\n");

    out.Write("\tss_pixel_x = round(ss_pixel_x);\n"
              "\tss_pixel_y = round(ss_pixel_y);\n");

    out.Write("\topos.x = ((ss_pixel_x / (CB_VS(" I_VIEWPORT_SIZE ").x * 0.5f)) - 1.0f);\n"
              "\topos.y = ((ss_pixel_y / (CB_VS(" I_VIEWPORT_SIZE ").y * 0.5f)) - 1.0f);\n"
              "}}\n");
  }

  // Vulkan NDC space has Y pointing down (right-handed NDC space).
  out.Write("  FIXUP_OPOS_VK;\n");
  out.Write("}}\n");

  return out;
}

static void GenVertexShaderTexGens(APIType api_type, u32 num_texgen, ShaderCode& out)
{
  // The HLSL compiler complains that the output texture coordinates are uninitialized when trying
  // to dynamically index them.
  for (u32 i = 0; i < num_texgen; i++)
    out.Write("OUTPUT(tex{}) = float3(0.0, 0.0, 0.0);\n", i);

  out.Write("// Texture coordinate generation\n");
  if (num_texgen == 1)
  {
    out.Write("{{ const uint texgen = 0u;\n");
  }
  else
  {
    out.Write("{}for (uint texgen = 0u; texgen < {}u; texgen++) {{\n",
              api_type == APIType::D3D ? "[loop] " : "", num_texgen);
  }

  out.Write("  // Texcoord transforms\n");
  out.Write("  float4 coord = float4(0.0, 0.0, 1.0, 1.0);\n"
            "  uint texMtxInfo = xfmem_texMtxInfo(texgen);\n");
  out.Write("  switch ({}) {{\n", BitfieldExtract<&TexMtxInfo::sourcerow>("texMtxInfo"));
  out.Write("  case {:s}:\n", SourceRow::Geom);
  out.Write("    coord.xyz = INPUT(rawpos).xyz;\n");
  out.Write("    break;\n\n");
  out.Write("  case {:s}:\n", SourceRow::Normal);
  out.Write("    coord.xyz = ((CB_VS(components) & {}u /* VB_HAS_NORMAL */) != 0u) ?"
            " INPUT(rawnormal).xyz : coord.xyz;",
            VB_HAS_NORMAL);
  out.Write("    break;\n\n");
  out.Write("  case {:s}:\n", SourceRow::BinormalT);
  out.Write("    coord.xyz = ((CB_VS(components) & {}u /* VB_HAS_TANGENT */) != 0u) ?"
            " INPUT(rawtangent).xyz : coord.xyz;",
            VB_HAS_TANGENT);
  out.Write("    break;\n\n");
  out.Write("  case {:s}:\n", SourceRow::BinormalB);
  out.Write("    coord.xyz = ((CB_VS(components) & {}u /* VB_HAS_BINORMAL */) != 0u) ?"
            " INPUT(rawbinormal).xyz : coord.xyz;",
            VB_HAS_BINORMAL);
  out.Write("    break;\n\n");
  for (u32 i = 0; i < 8; i++)
  {
    out.Write("  case {:s}:\n", static_cast<SourceRow>(static_cast<u32>(SourceRow::Tex0) + i));
    out.Write(
        "    coord = ((CB_VS(components) & {}u /* VB_HAS_UV{} */) != 0u) ?"
        " float4(INPUT(rawtex{}).x, INPUT(rawtex{}).y, 1.0, 1.0) : coord;\n",
        VB_HAS_UV0 << i, i, i, i);
    out.Write("    break;\n\n");
  }
  out.Write("  }}\n"
            "\n");

  out.Write("  // Input form of AB11 sets z element to 1.0\n");
  out.Write("  if ({} == {:s}) // inputform == AB11\n",
            BitfieldExtract<&TexMtxInfo::inputform>("texMtxInfo"), TexInputForm::AB11);
  out.Write("    coord.z = 1.0f;\n"
            "\n");

  // Convert NaNs to 1 - needed to fix eyelids in Shadow the Hedgehog during cutscenes
  // See https://bugs.dolphin-emu.org/issues/11458
  out.Write("  // Convert NaN to 1\n");
  out.Write("  if (dolphin_isnan(coord.x)) coord.x = 1.0;\n");
  out.Write("  if (dolphin_isnan(coord.y)) coord.y = 1.0;\n");
  out.Write("  if (dolphin_isnan(coord.z)) coord.z = 1.0;\n");

  out.Write("  // first transformation\n");
  out.Write("  uint texgentype = {};\n", BitfieldExtract<&TexMtxInfo::texgentype>("texMtxInfo"));
  out.Write("  float3 output_tex;\n"
            "  switch (texgentype)\n"
            "  {{\n");
  out.Write("  case {:s}:\n", TexGenType::EmbossMap);
  out.Write("    {{\n");
  out.Write("      uint light = {};\n",
            BitfieldExtract<&TexMtxInfo::embosslightshift>("texMtxInfo"));
  out.Write("      uint source = {};\n",
            BitfieldExtract<&TexMtxInfo::embosssourceshift>("texMtxInfo"));
  out.Write("      switch (source) {{\n");
  for (u32 i = 0; i < num_texgen; i++)
    out.Write("      case {}u: output_tex.xyz = OUTPUT(tex{}); break;\n", i, i);
  out.Write("      default: output_tex.xyz = float3(0.0, 0.0, 0.0); break;\n"
            "      }}\n"
            "      float3 ldir = normalize(CB_VS(" I_LIGHTS ")[light].pos.xyz - pos.xyz);\n"
            "      output_tex.xyz += float3(dot(ldir, _tangent), dot(ldir, _binormal), 0.0);\n"
            "    }}\n"
            "    break;\n\n");
  out.Write("  case {:s}:\n", TexGenType::Color0);
  out.Write("    output_tex.xyz = float3(OUTPUT(colors_0).x, OUTPUT(colors_0).y, 1.0);\n"
            "    break;\n\n");
  out.Write("  case {:s}:\n", TexGenType::Color1);
  out.Write("    output_tex.xyz = float3(OUTPUT(colors_1).x, OUTPUT(colors_1).y, 1.0);\n"
            "    break;\n\n");
  out.Write("  case {:s}:\n", TexGenType::Regular);
  out.Write("  default:\n"
            "    {{\n");
  out.Write("      if ((CB_VS(components) & ({}u /* VB_HAS_TEXMTXIDX0 */ << texgen)) != 0u) {{\n",
            VB_HAS_TEXMTXIDX0);
  out.Write("        // This is messy, due to dynamic indexing of the input texture coordinates.\n"
            "        // Hopefully the compiler will unroll this whole loop anyway and the switch.\n"
            "        int tmp = 0;\n"
            "        switch (texgen) {{\n");
  for (u32 i = 0; i < num_texgen; i++)
    out.Write("        case {}u: tmp = int(INPUT(rawtex{}).z); break;\n", i, i);
  out.Write("        }}\n"
            "\n");
  out.Write("        if ({} == {:s}) {{\n", BitfieldExtract<&TexMtxInfo::projection>("texMtxInfo"),
            TexSize::STQ);
  out.Write("          output_tex.xyz = float3(dot(coord, CB_VS(" I_TRANSFORMMATRICES ")[tmp]),\n"
            "                                  dot(coord, CB_VS(" I_TRANSFORMMATRICES ")[tmp + 1]),\n"
            "                                  dot(coord, CB_VS(" I_TRANSFORMMATRICES ")[tmp + 2]));\n"
            "        }} else {{\n"
            "          output_tex.xyz = float3(dot(coord, CB_VS(" I_TRANSFORMMATRICES ")[tmp]),\n"
            "                                  dot(coord, CB_VS(" I_TRANSFORMMATRICES ")[tmp + 1]),\n"
            "                                  1.0);\n"
            "        }}\n"
            "      }} else {{\n");
  out.Write("        if ({} == {:s}) {{\n", BitfieldExtract<&TexMtxInfo::projection>("texMtxInfo"),
            TexSize::STQ);
  out.Write("          output_tex.xyz = float3(dot(coord, CB_VS(" I_TEXMATRICES ")[3u * texgen]),\n"
            "                                  dot(coord, CB_VS(" I_TEXMATRICES ")[3u * texgen + 1u]),\n"
            "                                  dot(coord, CB_VS(" I_TEXMATRICES ")[3u * texgen + 2u]));\n"
            "        }} else {{\n"
            "          output_tex.xyz = float3(dot(coord, CB_VS(" I_TEXMATRICES ")[3u * texgen]),\n"
            "                                  dot(coord, CB_VS(" I_TEXMATRICES ")[3u * texgen + 1u]),\n"
            "                                  1.0);\n"
            "        }}\n"
            "      }}\n"
            "    }}\n"
            "    break;\n\n"
            "  }}\n"
            "\n");

  out.Write("  if (CB_VS(xfmem_dualTexInfo) != 0u) {{\n");
  out.Write("    uint postMtxInfo = xfmem_postMtxInfo(texgen);");
  out.Write("    uint base_index = {};\n", BitfieldExtract<&PostMtxInfo::index>("postMtxInfo"));
  out.Write("    float4 P0 = CB_VS(" I_POSTTRANSFORMMATRICES ")[base_index & 0x3fu];\n"
            "    float4 P1 = CB_VS(" I_POSTTRANSFORMMATRICES ")[(base_index + 1u) & 0x3fu];\n"
            "    float4 P2 = CB_VS(" I_POSTTRANSFORMMATRICES ")[(base_index + 2u) & 0x3fu];\n"
            "\n");
  out.Write("    if ({} != 0u)\n", BitfieldExtract<&PostMtxInfo::normalize>("postMtxInfo"));
  out.Write("      output_tex.xyz = normalize(output_tex.xyz);\n"
            "\n"
            "    // multiply by postmatrix\n"
            "    output_tex.xyz = float3(dot(P0.xyz, output_tex.xyz) + P0.w,\n"
            "                            dot(P1.xyz, output_tex.xyz) + P1.w,\n"
            "                            dot(P2.xyz, output_tex.xyz) + P2.w);\n"
            "  }}\n\n");

  // When q is 0, the GameCube appears to have a special case
  // This can be seen in devkitPro's neheGX Lesson08 example for Wii
  // Makes differences in Rogue Squadron 3 (Hoth sky) and The Last Story (shadow culling)
  out.Write("  if (texgentype == {:s} && output_tex.z == 0.0)\n", TexGenType::Regular);
  out.Write(
      "    output_tex.xy = clamp(output_tex.xy / 2.0f, float2(-1.0f,-1.0f), float2(1.0f,1.0f));\n"
      "\n");

  out.Write("  // Hopefully GPUs that can support dynamic indexing will optimize this.\n");
  out.Write("  switch (texgen) {{\n");
  for (u32 i = 0; i < num_texgen; i++)
    out.Write("  case {}u: OUTPUT(tex{}) = output_tex; break;\n", i, i);
  out.Write("  }}\n"
            "}}\n");
}

void EnumerateVertexShaderUids(const std::function<void(const VertexShaderUid&)>& callback)
{
  VertexShaderUid uid;

  for (u32 texgens = 0; texgens <= 8; texgens++)
  {
    vertex_ubershader_uid_data* const vuid = uid.GetUidData();
    vuid->num_texgens = texgens;
    callback(uid);
  }
}
}  // namespace UberShader
