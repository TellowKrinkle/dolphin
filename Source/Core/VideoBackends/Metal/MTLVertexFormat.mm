// Copyright 2022 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoBackends/Metal/MTLVertexFormat.h"

#include "VideoCommon/VertexShaderGen.h"

static MTLVertexFormat ConvertFormat(ComponentFormat format, int count, bool int_format)
{
  // clang-format off
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
  // clang-format on
  return formats[int_format][static_cast<int>(format)][count - 1];
}

static void SetAttribute(MTLVertexDescriptor* desc, u32 attribute, const AttributeFormat& format)
{
  if (!format.enable)
    return;
  MTLVertexAttributeDescriptor* attr_desc = [[desc attributes] objectAtIndexedSubscript:attribute];
  [attr_desc setFormat:ConvertFormat(format.type, format.components, format.integer)];
  [attr_desc setOffset:format.offset];
  [attr_desc setBufferIndex:0];
}

template <size_t N>
static void SetAttributes(MTLVertexDescriptor* desc, u32 attribute,
                          const AttributeFormat (&format)[N])
{
  for (size_t i = 0; i < N; ++i)
    SetAttribute(desc, attribute + i, format[i]);
}

Metal::VertexFormat::VertexFormat(const PortableVertexDeclaration& vtx_decl)
    : NativeVertexFormat(vtx_decl), m_desc(MRCTransfer([MTLVertexDescriptor new]))
{
  [[[m_desc layouts] objectAtIndexedSubscript:0] setStride:vtx_decl.stride];
  SetAttribute(m_desc, SHADER_POSITION_ATTRIB, vtx_decl.position);
  SetAttributes(m_desc, SHADER_NORMAL_ATTRIB, vtx_decl.normals);
  SetAttributes(m_desc, SHADER_COLOR0_ATTRIB, vtx_decl.colors);
  SetAttributes(m_desc, SHADER_TEXTURE0_ATTRIB, vtx_decl.texcoords);
  SetAttribute(m_desc, SHADER_POSMTX_ATTRIB, vtx_decl.posmtx);
}
