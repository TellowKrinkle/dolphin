// Copyright 2022 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoBackends/Metal/MTLBoundingBox.h"

#include "VideoBackends/Metal/MTLObjectCache.h"
#include "VideoBackends/Metal/MTLStateTracker.h"

static constexpr size_t BUFFER_SIZE = sizeof(BBoxType) * NUM_BBOX_VALUES;

Metal::BoundingBox::~BoundingBox()
{
  if (g_state_tracker)
    g_state_tracker->SetBBoxBuffer(nullptr, nullptr, nullptr);
}

bool Metal::BoundingBox::Initialize()
{
  const MTLResourceOptions gpu_storage_mode = g_features.unified_memory ? MTLResourceStorageModeShared : MTLResourceStorageModePrivate;
  const MTLResourceOptions gpu_options = gpu_storage_mode | MTLResourceHazardTrackingModeUntracked;
  const id<MTLDevice> dev = g_device;
  m_upload_fence = MRCTransfer([dev newFence]);
  m_download_fence = MRCTransfer([dev newFence]);
  m_gpu_buffer = MRCTransfer([dev newBufferWithLength:BUFFER_SIZE options:gpu_options]);
  if (g_features.unified_memory)
  {
    m_cpu_buffer_ptr = static_cast<BBoxType*>([m_gpu_buffer contents]);
  }
  else
  {
    m_cpu_buffer = MRCTransfer([dev newBufferWithLength:BUFFER_SIZE options:MTLResourceStorageModeShared]);
    m_cpu_buffer_ptr = static_cast<BBoxType*>([m_cpu_buffer contents]);
  }
  g_state_tracker->SetBBoxBuffer(m_gpu_buffer, m_upload_fence, m_download_fence);
  return true;
}

std::vector<BBoxType> Metal::BoundingBox::Read(u32 index, u32 length)
{ @autoreleasepool {
  g_state_tracker->EndRenderPass();
  if (!g_features.unified_memory)
  {
    id<MTLBlitCommandEncoder> blit = [g_state_tracker->GetRenderCmdBuf() blitCommandEncoder];
    [blit setLabel:@"BBox Download"];
    [blit waitForFence:m_download_fence];
    [blit copyFromBuffer:m_gpu_buffer
            sourceOffset:0
                toBuffer:m_cpu_buffer
       destinationOffset:0
                    size:BUFFER_SIZE];
    [blit endEncoding];
  }
  g_state_tracker->FlushEncoders();
  g_state_tracker->WaitForFlushedEncoders();
  return std::vector<BBoxType>(m_cpu_buffer_ptr + index, m_cpu_buffer_ptr + index + length);
}}

void Metal::BoundingBox::Write(u32 index, const std::vector<BBoxType>& values)
{ @autoreleasepool {
  const u32 size = values.size() * sizeof(BBoxType);
  if (g_features.unified_memory && !g_state_tracker->HasUnflushedData() && !g_state_tracker->GPUBusy())
  {
    // We can just write directly to the buffer!
    memcpy(m_cpu_buffer_ptr + index, values.data(), size);
  }
  else
  {
    StateTracker::Map map = g_state_tracker->Allocate(StateTracker::UploadBuffer::Other, size, StateTracker::AlignMask::Other);
    memcpy(map.cpu_buffer, values.data(), size);
    g_state_tracker->EndRenderPass();
    id<MTLBlitCommandEncoder> blit = [g_state_tracker->GetRenderCmdBuf() blitCommandEncoder];
    [blit setLabel:@"BBox Upload"];
    [blit copyFromBuffer:map.gpu_buffer
            sourceOffset:map.gpu_offset
                toBuffer:m_gpu_buffer
       destinationOffset:index * sizeof(BBoxType)
                    size:size];
    [blit updateFence:m_upload_fence];
    [blit endEncoding];
  }
}}
