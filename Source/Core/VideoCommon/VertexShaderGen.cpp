// Copyright 2008 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoCommon/VertexShaderGen.h"

#include "Common/Assert.h"
#include "Common/CommonTypes.h"
#include "VideoCommon/BPMemory.h"
#include "VideoCommon/LightingShaderGen.h"
#include "VideoCommon/NativeVertexFormat.h"
#include "VideoCommon/VertexLoaderManager.h"
#include "VideoCommon/VideoCommon.h"
#include "VideoCommon/VideoConfig.h"
#include "VideoCommon/XFMemory.h"

VertexShaderUid GetVertexShaderUid()
{
  ASSERT(bpmem.genMode.numtexgens == xfmem.numTexGen.numTexGens);
  ASSERT(bpmem.genMode.numcolchans == xfmem.numChan.numColorChans);

  VertexShaderUid out;
  vertex_shader_uid_data* const uid_data = out.GetUidData();
  uid_data->numTexGens = xfmem.numTexGen.numTexGens;
  uid_data->components = VertexLoaderManager::g_current_components;
  uid_data->numColorChans = xfmem.numChan.numColorChans;

  GetLightingShaderUid(uid_data->lighting);

  // transform texcoords
  for (u32 i = 0; i < uid_data->numTexGens; ++i)
  {
    auto& texinfo = uid_data->texMtxInfo[i];

    texinfo.sourcerow = xfmem.texMtxInfo[i].sourcerow;
    texinfo.texgentype = xfmem.texMtxInfo[i].texgentype;
    texinfo.inputform = xfmem.texMtxInfo[i].inputform;

    // first transformation
    switch (texinfo.texgentype)
    {
    case TexGenType::EmbossMap:  // calculate tex coords into bump map
      if ((uid_data->components & (VB_HAS_TANGENT | VB_HAS_BINORMAL)) != 0)
      {
        // transform the light dir into tangent space
        texinfo.embosslightshift = xfmem.texMtxInfo[i].embosslightshift;
        texinfo.embosssourceshift = xfmem.texMtxInfo[i].embosssourceshift;
      }
      else
      {
        texinfo.embosssourceshift = xfmem.texMtxInfo[i].embosssourceshift;
      }
      break;
    case TexGenType::Color0:
    case TexGenType::Color1:
      break;
    case TexGenType::Regular:
    default:
      uid_data->texMtxInfo_n_projection |= static_cast<u32>(xfmem.texMtxInfo[i].projection.Value())
                                           << i;
      break;
    }

    uid_data->dualTexTrans_enabled = xfmem.dualTexTrans.enabled;
    // CHECKME: does this only work for regular tex gen types?
    if (uid_data->dualTexTrans_enabled && texinfo.texgentype == TexGenType::Regular)
    {
      auto& postInfo = uid_data->postMtxInfo[i];
      postInfo.index = xfmem.postMtxInfo[i].index;
      postInfo.normalize = xfmem.postMtxInfo[i].normalize;
    }
  }

  return out;
}

ShaderCode GenerateVertexShaderCode(APIType api_type, const ShaderHostConfig& host_config,
                                    const vertex_shader_uid_data* uid_data)
{
  ShaderCode out;

  const bool per_pixel_lighting = g_ActiveConfig.bEnablePixelLighting;
  const bool msaa = host_config.msaa;
  const bool ssaa = host_config.ssaa;
  const bool vertex_rounding = host_config.vertex_rounding;

  out.Write("// SHADER_SUPPORTS_MSL\n\n");
  out.Write("{}", s_lighting_struct);

  // uniforms
  out.Write("DECL_CB_VS {{\n"
            "{}\n"
            "}};\n",
            s_shader_uniforms);

  out.Write("INPUT_DECL_BEGIN\n"
            "  DECL_INPUT_CB_VS\n"
            "INPUT_DECL_END\n\n");

  out.Write("VERTEX_INPUT_DECL_BEGIN\n");
  out.Write("  DECL_VERTEX_INPUT(float4, rawpos, POSITION, {})\n", SHADER_POSITION_ATTRIB);
  if (uid_data->components & VB_HAS_POSMTXIDX)
    out.Write("  DECL_VERTEX_INPUT(uint4, posmtx, BLENDINDICES, {})\n", SHADER_POSMTX_ATTRIB);
  if (uid_data->components & VB_HAS_NORMAL)
    out.Write("  DECL_VERTEX_INPUT(float3, rawnormal, NORMAL, {})\n", SHADER_NORMAL_ATTRIB);
  if (uid_data->components & VB_HAS_TANGENT)
    out.Write("  DECL_VERTEX_INPUT(float3, rawtangent, TANGENT, {})\n", SHADER_TANGENT_ATTRIB);
  if (uid_data->components & VB_HAS_BINORMAL)
    out.Write("  DECL_VERTEX_INPUT(float3, rawbinormal, BINORMAL, {})\n", SHADER_BINORMAL_ATTRIB);
  if (uid_data->components & VB_HAS_COL0)
    out.Write("  DECL_VERTEX_INPUT(float4, rawcolor0, COLOR0, {})\n", SHADER_COLOR0_ATTRIB);
  if (uid_data->components & VB_HAS_COL1)
    out.Write("  DECL_VERTEX_INPUT(float4, rawcolor1, COLOR1, {})\n", SHADER_COLOR1_ATTRIB);
  for (int i = 0; i < 8; i++)
  {
    const bool has_texmtx = uid_data->components & (VB_HAS_TEXMTXIDX0 << i);
    if ((uid_data->components & (VB_HAS_UV0 << i)) || has_texmtx)
      out.Write("  DECL_VERTEX_INPUT(float{}, rawtex{}, TEXCOORD{}, {})\n",
                has_texmtx ? 3 : 2, i, i, SHADER_TEXTURE0_ATTRIB + i);
  }
  out.Write("VERTEX_INPUT_DECL_END\n\n");

  const char* qualifier = GetInterpolationQualifier(api_type, msaa, ssaa);
  if (host_config.backend_geometry_shaders)
  {
    if (api_type == APIType::OpenGL || api_type == APIType::Vulkan)
      qualifier = GetInterpolationQualifier(api_type, msaa, ssaa, true, false);
  }
  out.Write("VERTEX_OUTPUT_DECL_BEGIN\n");
  GenerateVSOutputMembers(out, uid_data->numTexGens, host_config, qualifier, "VERTEX_OUTPUT");
  out.Write("VERTEX_OUTPUT_DECL_END\n\n");

  WriteIsNanHeader(out, api_type);

  out.Write("DECL_MAIN {{\n");

  // xfmem.numColorChans controls the number of color channels available to TEV, but we still need
  // to generate all channels here, as it can be used in texgen. Cel-damage is an example of this.
  out.Write("float4 vertex_color_0, vertex_color_1;\n");

  // To use color 1, the vertex descriptor must have color 0 and 1.
  // If color 1 is present but not color 0, it is used for lighting channel 0.
  const bool use_color_1 =
      (uid_data->components & (VB_HAS_COL0 | VB_HAS_COL1)) == (VB_HAS_COL0 | VB_HAS_COL1);
  for (u32 color = 0; color < NUM_XF_COLOR_CHANNELS; color++)
  {
    if ((color == 0 || use_color_1) && (uid_data->components & (VB_HAS_COL0 << color)) != 0)
    {
      // Use color0 for channel 0, and color1 for channel 1 if both colors 0 and 1 are present.
      out.Write("vertex_color_{0} = INPUT(rawcolor{0});\n", color);
    }
    else if (color == 0 && (uid_data->components & VB_HAS_COL1) != 0)
    {
      // Use color1 for channel 0 if color0 is not present.
      out.Write("vertex_color_{} = INPUT(rawcolor1);\n", color);
    }
    else
    {
      out.Write("vertex_color_{0} = CB_VS(missing_color_value);\n", color);
    }
  }

  // transforms
  if ((uid_data->components & VB_HAS_POSMTXIDX) != 0)
  {
    // Vertex format has a per-vertex matrix
    out.Write("int posidx = int(INPUT(posmtx).r);\n"
              "float4 P0 = CB_VS(" I_TRANSFORMMATRICES ")[posidx];\n"
              "float4 P1 = CB_VS(" I_TRANSFORMMATRICES ")[posidx + 1];\n"
              "float4 P2 = CB_VS(" I_TRANSFORMMATRICES ")[posidx + 2];\n");
    if ((uid_data->components & VB_HAS_NORMAL) != 0)
    {
      out.Write("int normidx = posidx & 31;\n"
                "float3 N0 = CB_VS(" I_NORMALMATRICES ")[normidx].xyz;\n"
                "float3 N1 = CB_VS(" I_NORMALMATRICES ")[normidx + 1].xyz;\n"
                "float3 N2 = CB_VS(" I_NORMALMATRICES ")[normidx + 2].xyz;\n");
    }
  }
  else
  {
    // One shared matrix
    out.Write("float4 P0 = CB_VS(" I_POSNORMALMATRIX ")[0];\n"
              "float4 P1 = CB_VS(" I_POSNORMALMATRIX ")[1];\n"
              "float4 P2 = CB_VS(" I_POSNORMALMATRIX ")[2];\n");
    if ((uid_data->components & VB_HAS_NORMAL) != 0)
    {
      out.Write("float3 N0 = CB_VS(" I_POSNORMALMATRIX ")[3].xyz;\n"
                "float3 N1 = CB_VS(" I_POSNORMALMATRIX ")[4].xyz;\n"
                "float3 N2 = CB_VS(" I_POSNORMALMATRIX ")[5].xyz;\n");
    }
  }

  out.Write("// Multiply the position vector by the position matrix\n"
            "float4 pos = float4(dot(P0, INPUT(rawpos)), dot(P1, INPUT(rawpos)), "
            "dot(P2, INPUT(rawpos)), 1.0);\n");
  if ((uid_data->components & VB_HAS_NORMAL) != 0)
  {
    std::string_view rawtangent = uid_data->components & VB_HAS_TANGENT
      ? "INPUT(rawtangent)"
      : "CB_VS(" I_CACHED_TANGENT ").xyz";
    std::string_view rawbinormal = uid_data->components & VB_HAS_BINORMAL
      ? "INPUT(rawbinormal)"
      : "CB_VS(" I_CACHED_BINORMAL ").xyz";

    // The scale of the transform matrix is used to control the size of the emboss map effect, by
    // changing the scale of the transformed binormals (which only get used by emboss map texgens).
    // By normalising the first transformed normal (which is used by lighting calculations and needs
    // to be unit length), the same transform matrix can do double duty, scaling for emboss mapping,
    // and not scaling for lighting.
    out.Write("MAYBE_UNUSED float3 _normal = normalize(float3(dot(N0, INPUT(rawnormal)), "
              "dot(N1, INPUT(rawnormal)), dot(N2, INPUT(rawnormal))));\n"
              "MAYBE_UNUSED float3 _tangent = float3(dot(N0, {rawtangent}), "
              "dot(N1, {rawtangent}), dot(N2, {rawtangent}));\n"
              "MAYBE_UNUSED float3 _binormal = float3(dot(N0, {rawbinormal}), "
              "dot(N1, {rawbinormal}), dot(N2, {rawbinormal}));\n",
              fmt::arg("rawtangent", rawtangent), fmt::arg("rawbinormal", rawbinormal));
  }
  else
  {
    out.Write("MAYBE_UNUSED float3 _normal = float3(0.0, 0.0, 0.0);\n");
    out.Write("MAYBE_UNUSED float3 _binormal = float3(0.0, 0.0, 0.0);\n");
    out.Write("MAYBE_UNUSED float3 _tangent = float3(0.0, 0.0, 0.0);\n");
  }

  out.Write("opos = float4(dot(CB_VS(" I_PROJECTION ")[0], pos), dot(CB_VS(" I_PROJECTION
            ")[1], pos), dot(CB_VS(" I_PROJECTION ")[2], pos), dot(CB_VS(" I_PROJECTION ")[3], pos));\n");

  out.Write("int4 lacc;\n"
            "MAYBE_UNUSED float3 ldir, h, cosAttn, distAttn;\n"
            "MAYBE_UNUSED float dist, dist2, attn;\n");

  GenerateLightingShaderCode(out, uid_data->lighting, "vertex_color_0", "vertex_color_1",
                             "OUTPUT(colors_0)", "OUTPUT(colors_1)");

  // transform texcoords
  out.Write("MAYBE_UNUSED float4 coord = float4(0.0, 0.0, 1.0, 1.0);\n");
  for (u32 i = 0; i < uid_data->numTexGens; ++i)
  {
    auto& texinfo = uid_data->texMtxInfo[i];

    out.Write("{{\n");
    out.Write("coord = float4(0.0, 0.0, 1.0, 1.0);\n");
    switch (texinfo.sourcerow)
    {
    case SourceRow::Geom:
      out.Write("coord.xyz = INPUT(rawpos).xyz;\n");
      break;
    case SourceRow::Normal:
      if ((uid_data->components & VB_HAS_NORMAL) != 0)
      {
        out.Write("coord.xyz = INPUT(rawnormal).xyz;\n");
      }
      break;
    case SourceRow::Colors:
      ASSERT(texinfo.texgentype == TexGenType::Color0 || texinfo.texgentype == TexGenType::Color1);
      break;
    case SourceRow::BinormalT:
      if ((uid_data->components & VB_HAS_TANGENT) != 0)
      {
        out.Write("coord.xyz = INPUT(rawtangent).xyz;\n");
      }
      break;
    case SourceRow::BinormalB:
      if ((uid_data->components & VB_HAS_BINORMAL) != 0)
      {
        out.Write("coord.xyz = INPUT(rawbinormal).xyz;\n");
      }
      break;
    default:
      ASSERT(texinfo.sourcerow >= SourceRow::Tex0 && texinfo.sourcerow <= SourceRow::Tex7);
      u32 texnum = static_cast<u32>(texinfo.sourcerow) - static_cast<u32>(SourceRow::Tex0);
      if ((uid_data->components & (VB_HAS_UV0 << (texnum))) != 0)
      {
        out.Write("coord = float4(INPUT(rawtex{0}).x, INPUT(rawtex{0}).y, 1.0, 1.0);\n", texnum);
      }
      break;
    }
    // Input form of AB11 sets z element to 1.0

    if (texinfo.inputform == TexInputForm::AB11)
      out.Write("coord.z = 1.0;\n");

    // Convert NaNs to 1 - needed to fix eyelids in Shadow the Hedgehog during cutscenes
    // See https://bugs.dolphin-emu.org/issues/11458
    out.Write("// Convert NaN to 1\n");
    out.Write("if (dolphin_isnan(coord.x)) coord.x = 1.0;\n");
    out.Write("if (dolphin_isnan(coord.y)) coord.y = 1.0;\n");
    out.Write("if (dolphin_isnan(coord.z)) coord.z = 1.0;\n");

    // first transformation
    switch (texinfo.texgentype)
    {
    case TexGenType::EmbossMap:  // calculate tex coords into bump map

      // transform the light dir into tangent space
      out.Write("ldir = normalize(CB_VS(" LIGHT_POS ").xyz - pos.xyz);\n",
                LIGHT_POS_PARAMS(texinfo.embosslightshift));
      out.Write("OUTPUT(tex{}).xyz = OUTPUT(tex{}).xyz + float3(dot(ldir, _tangent), "
                "dot(ldir, _binormal), 0.0);\n",
                i, texinfo.embosssourceshift);

      break;
    case TexGenType::Color0:
      out.Write("OUTPUT(tex{}).xyz = float3(OUTPUT(colors_0).x, OUTPUT(colors_0).y, 1);\n", i);
      break;
    case TexGenType::Color1:
      out.Write("OUTPUT(tex{}).xyz = float3(OUTPUT(colors_1).x, OUTPUT(colors_1).y, 1);\n", i);
      break;
    case TexGenType::Regular:
    default:
      if ((uid_data->components & (VB_HAS_TEXMTXIDX0 << i)) != 0)
      {
        out.Write("int tmp = int(rawtex{}.z);\n", i);
        if (static_cast<TexSize>((uid_data->texMtxInfo_n_projection >> i) & 1) == TexSize::STQ)
        {
          out.Write("OUTPUT(tex{}).xyz = float3(dot(coord, CB_VS(" I_TRANSFORMMATRICES
                    ")[tmp]), dot(coord, CB_VS(" I_TRANSFORMMATRICES
                    ")[tmp+1]), dot(coord, CB_VS(" I_TRANSFORMMATRICES ")[tmp+2]));\n",
                    i);
        }
        else
        {
          out.Write("OUTPUT(tex{}).xyz = float3(dot(coord, CB_VS(" I_TRANSFORMMATRICES
                    ")[tmp]), dot(coord, CB_VS(" I_TRANSFORMMATRICES ")[tmp+1]), 1);\n",
                    i);
        }
      }
      else
      {
        if (static_cast<TexSize>((uid_data->texMtxInfo_n_projection >> i) & 1) == TexSize::STQ)
        {
          out.Write("OUTPUT(tex{}).xyz = float3(dot(coord, CB_VS(" I_TEXMATRICES
                    ")[{}]), dot(coord, CB_VS(" I_TEXMATRICES ")[{}]), dot(coord, CB_VS("
                    I_TEXMATRICES ")[{}]));\n",
                    i, 3 * i, 3 * i + 1, 3 * i + 2);
        }
        else
        {
          out.Write("OUTPUT(tex{}).xyz = float3(dot(coord, CB_VS(" I_TEXMATRICES
                    ")[{}]), dot(coord, CB_VS(" I_TEXMATRICES ")[{}]), 1);\n",
                    i, 3 * i, 3 * i + 1);
        }
      }
      break;
    }

    // CHECKME: does this only work for regular tex gen types?
    if (uid_data->dualTexTrans_enabled && texinfo.texgentype == TexGenType::Regular)
    {
      auto& postInfo = uid_data->postMtxInfo[i];

      out.Write("float4 P0 = CB_VS(" I_POSTTRANSFORMMATRICES ")[{}];\n"
                "float4 P1 = CB_VS(" I_POSTTRANSFORMMATRICES ")[{}];\n"
                "float4 P2 = CB_VS(" I_POSTTRANSFORMMATRICES ")[{}];\n",
                postInfo.index & 0x3f, (postInfo.index + 1) & 0x3f, (postInfo.index + 2) & 0x3f);

      if (postInfo.normalize)
        out.Write("OUTPUT(tex{}).xyz = normalize(OUTPUT(tex{}).xyz);\n", i, i);

      // multiply by postmatrix
      out.Write(
          "OUTPUT(tex{0}).xyz = float3(dot(P0.xyz, OUTPUT(tex{0}).xyz) + P0.w, "
          "dot(P1.xyz, OUTPUT(tex{0}).xyz) + P1.w, dot(P2.xyz, OUTPUT(tex{0}).xyz) + P2.w);\n",
          i);
    }

    // When q is 0, the GameCube appears to have a special case
    // This can be seen in devkitPro's neheGX Lesson08 example for Wii
    // Makes differences in Rogue Squadron 3 (Hoth sky) and The Last Story (shadow culling)
    // TODO: check if this only affects XF_TEXGEN_REGULAR
    if (texinfo.texgentype == TexGenType::Regular)
    {
      out.Write("if(OUTPUT(tex{0}).z == 0.0f)\n"
                "\tOUTPUT(tex{0}).xy = clamp(OUTPUT(tex{0}).xy / 2.0f, float2(-1.0f,-1.0f), "
                "float2(1.0f,1.0f));\n",
                i);
    }

    out.Write("}}\n");
  }

  if (per_pixel_lighting)
  {
    // When per-pixel lighting is enabled, the vertex colors are passed through
    // unmodified so we can evaluate the lighting in the pixel shader.

    // Lighting is also still computed in the vertex shader since it can be used to
    // generate texture coordinates. We generated them above, so now the colors can
    // be reverted to their previous stage.
    out.Write("OUTPUT(colors_0) = vertex_color_0;\n");
    out.Write("OUTPUT(colors_1) = vertex_color_1;\n");
    // Note that the numColorChans logic is performed in the pixel shader.
  }
  else
  {
    // The number of colors available to TEV is determined by numColorChans.
    // We have to provide the fields to match the interface, so set to zero if it's not enabled.
    if (uid_data->numColorChans == 0)
      out.Write("OUTPUT(colors_0) = float4(0.0, 0.0, 0.0, 0.0);\n");
    if (uid_data->numColorChans <= 1)
      out.Write("OUTPUT(colors_1) = float4(0.0, 0.0, 0.0, 0.0);\n");
  }

  // clipPos/w needs to be done in pixel shader, not here
  if (!host_config.fast_depth_calc)
    out.Write("OUTPUT(clipPos) = opos;\n");

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
  else
  {
    // Same depth adjustment for Sonic. Without depth clamping, it unfortunately
    // affects non-clipping uses of depth too.
    out.Write("opos.z = opos.z * (1.0 - 1e-7);\n");
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
    // By now our position is in clip space
    // however, higher resolutions than the Wii outputs
    // cause an additional pixel offset
    // due to a higher pixel density
    // we need to correct this by converting our
    // clip-space position into the Wii's screen-space
    // acquire the right pixel and then convert it back
    out.Write("if (opos.w == 1.0f)\n"
              "{{\n"

              "\tfloat ss_pixel_x = ((opos.x + 1.0f) * (CB_VS(" I_VIEWPORT_SIZE ").x * 0.5f));\n"
              "\tfloat ss_pixel_y = ((opos.y + 1.0f) * (CB_VS(" I_VIEWPORT_SIZE ").y * 0.5f));\n"

              "\tss_pixel_x = round(ss_pixel_x);\n"
              "\tss_pixel_y = round(ss_pixel_y);\n"

              "\topos.x = ((ss_pixel_x / (CB_VS(" I_VIEWPORT_SIZE ").x * 0.5f)) - 1.0f);\n"
              "\topos.y = ((ss_pixel_y / (CB_VS(" I_VIEWPORT_SIZE ").y * 0.5f)) - 1.0f);\n"
              "}}\n");
  }

  // Vulkan NDC space has Y pointing down (right-handed NDC space).
  out.Write("  FIXUP_OPOS_VK;\n");
  out.Write("}}\n");

  return out;
}
