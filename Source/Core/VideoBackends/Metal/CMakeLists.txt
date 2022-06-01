add_library(videometal
  MRCHelpers.h
  MTLBoundingBox.mm
  MTLBoundingBox.h
  MTLMain.mm
  MTLObjectCache.h
  MTLObjectCache.mm
  MTLPerfQuery.mm
  MTLPerfQuery.h
  MTLPipeline.mm
  MTLPipeline.h
  MTLRenderer.mm
  MTLRenderer.h
  MTLShader.h
  MTLStateTracker.mm
  MTLStateTracker.h
  MTLTexture.mm
  MTLTexture.h
  MTLUtil.mm
  MTLUtil.h
  MTLVertexManager.mm
  MTLVertexManager.h
  VideoBackend.h
)

find_package(spirv_cross_core REQUIRED)
find_package(spirv_cross_glsl REQUIRED)
find_package(spirv_cross_cpp REQUIRED)
find_package(spirv_cross_msl REQUIRED)
find_library(METAL_LIBRARY Metal)
find_library(QUARTZCORE_LIBRARY QuartzCore)

target_link_libraries(videometal
PUBLIC
  common
  videocommon
PRIVATE
  glslang
  spirv-cross-cpp
  spirv-cross-msl
  ${METAL_LIBRARY}
  ${QUARTZCORE_LIBRARY}
)

target_include_directories(videometal SYSTEM PRIVATE ${CMAKE_SOURCE_DIR}/Externals/glslang)