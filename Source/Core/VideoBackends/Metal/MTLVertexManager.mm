// Copyright 2022 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoBackends/Metal/MTLVertexManager.h"

#include "VideoBackends/Metal/MTLStateTracker.h"

#include "VideoCommon/PixelShaderManager.h"
#include "VideoCommon/Statistics.h"
#include "VideoCommon/VertexShaderManager.h"

Metal::VertexManager::VertexManager()
{
}

Metal::VertexManager::~VertexManager() = default;

void Metal::VertexManager::UploadUtilityUniforms(const void* uniforms, u32 uniforms_size)
{
  g_state_tracker->SetUtilityUniform(uniforms, uniforms_size);
}

bool Metal::VertexManager::UploadTexelBuffer(
  const void* data, u32 data_size, TexelBufferFormat format, u32* out_offset)
{
  // TODO: Implement
  return false;
}

bool Metal::VertexManager::UploadTexelBuffer(
  const void* data,         u32 data_size,    TexelBufferFormat format,         u32* out_offset,
  const void* palette_data, u32 palette_size, TexelBufferFormat palette_format, u32* out_palette_offset)
{
  // TODO: Implement
  return false;
}

void Metal::VertexManager::ResetBuffer(u32 vertex_stride)
{
  void* vertex = g_state_tracker->Preallocate(StateTracker::UploadBuffer::Vertex, MAXVBUFFERSIZE);
  void* index  = g_state_tracker->Preallocate(StateTracker::UploadBuffer::Index, MAXIBUFFERSIZE * sizeof(u16));

  m_cur_buffer_pointer = m_base_buffer_pointer = static_cast<u8*>(vertex);
  m_end_buffer_pointer = m_base_buffer_pointer + MAXVBUFFERSIZE;
  m_index_generator.Start(static_cast<u16*>(index));
}

void Metal::VertexManager::CommitBuffer(
  u32 num_vertices, u32 vertex_stride, u32 num_indices, u32* out_base_vertex, u32* out_base_index)
{
  u32 vsize = num_vertices * vertex_stride;
  u32 isize = num_indices * sizeof(u16);
  StateTracker::Map vmap = g_state_tracker->CommitPreallocation(StateTracker::UploadBuffer::Vertex, vsize, StateTracker::AlignMask::Other);
  StateTracker::Map imap = g_state_tracker->CommitPreallocation(StateTracker::UploadBuffer::Index,  isize, StateTracker::AlignMask::Other);

  ADDSTAT(g_stats.this_frame.bytes_vertex_streamed, vsize);
  ADDSTAT(g_stats.this_frame.bytes_index_streamed, isize);

  g_state_tracker->SetVerticesAndIndices(vmap, imap);
  *out_base_vertex = 0;
  *out_base_index = 0;
}

void Metal::VertexManager::UploadUniforms()
{
  g_state_tracker->InvalidateUniforms(VertexShaderManager::dirty, PixelShaderManager::dirty);
  VertexShaderManager::dirty = false;
  PixelShaderManager::dirty = false;
}
