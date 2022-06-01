// Copyright 2022 Dolphin Emulator Project
// SPDX-License-Identifier: GPL-2.0-or-later

#include "VideoBackends/Metal/MTLUtil.h"

#include <SPIRV/GlslangToSpv.h>
#include <StandAlone/ResourceLimits.h>
#include <glslang/Public/ShaderLang.h>
#include <SPIRV/disassemble.h>
#include <spirv_msl.hpp>
#include <fstream>
#include <string>

#include "Common/MsgHandler.h"

Metal::DeviceFeatures Metal::g_features;

std::vector<MRCOwned<id<MTLDevice>>> Metal::Util::GetAdapterList()
{
  std::vector<MRCOwned<id<MTLDevice>>> list;
  id<MTLDevice> default_dev = MTLCreateSystemDefaultDevice();
  if (default_dev)
    list.push_back(MRCTransfer(default_dev));

  auto devices = MRCTransfer(MTLCopyAllDevices());
  for (id<MTLDevice> device in devices.Get())
  {
    if (device != default_dev)
      list.push_back(MRCRetain(device));
  }
  return list;
}

void Metal::Util::PopulateBackendInfo(VideoConfig* config)
{
  config->backend_info.api_type = APIType::Metal;
  config->backend_info.bUsesLowerLeftOrigin = false;
  config->backend_info.bSupportsExclusiveFullscreen = false;
  config->backend_info.bSupportsDualSourceBlend = true;
  config->backend_info.bSupportsPrimitiveRestart = true;
  config->backend_info.bSupportsGeometryShaders = false;
  config->backend_info.bSupportsComputeShaders = true;
  config->backend_info.bSupports3DVision = false;
  config->backend_info.bSupportsEarlyZ = true;
  config->backend_info.bSupportsBindingLayout = true;
  config->backend_info.bSupportsBBox = true;
  config->backend_info.bSupportsGSInstancing = false;
  config->backend_info.bSupportsPostProcessing = true;
  config->backend_info.bSupportsPaletteConversion = true;
  config->backend_info.bSupportsClipControl = true;
  config->backend_info.bSupportsSSAA = true;
  config->backend_info.bSupportsFragmentStoresAndAtomics = true;
  config->backend_info.bSupportsReversedDepthRange = false;
  config->backend_info.bSupportsLogicOp = false;
  config->backend_info.bSupportsMultithreading = false;
  config->backend_info.bSupportsGPUTextureDecoding = true;
  config->backend_info.bSupportsCopyToVram = true;
  config->backend_info.bSupportsBitfield = true;
  config->backend_info.bSupportsDynamicSamplerIndexing = true;
  config->backend_info.bSupportsFramebufferFetch = false;
  config->backend_info.bSupportsBackgroundCompiling = true;
  config->backend_info.bSupportsLargePoints = true;
  config->backend_info.bSupportsPartialDepthCopies = true;
  config->backend_info.bSupportsDepthReadback = true;
  config->backend_info.bSupportsShaderBinaries = false;
  config->backend_info.bSupportsPipelineCacheData = false;
  config->backend_info.bSupportsCoarseDerivatives = false;
  config->backend_info.bSupportsTextureQueryLevels = true;
  config->backend_info.bSupportsLodBiasInSampler = false;
  config->backend_info.bSupportsSettingObjectNames = true;
}

void Metal::Util::PopulateBackendInfoAdapters(VideoConfig* config, const std::vector<MRCOwned<id<MTLDevice>>>& adapters)
{
  config->backend_info.Adapters.clear();
  for (id<MTLDevice> adapter : adapters)
  {
    config->backend_info.Adapters.push_back([[adapter name] UTF8String]);
  }
}

void Metal::Util::PopulateBackendInfoFeatures(VideoConfig* config, id<MTLDevice> device)
{
#ifdef TARGET_OS_MAC
  config->backend_info.bSupportsDepthClamp = true;
  config->backend_info.bSupportsST3CTextures = true;
  config->backend_info.bSupportsBPTCTextures = true;
#else
  bool supports_mac1 = false;
  bool supports_apple4 = false;
  if (@available(iOS 13, *))
  {
    supports_mac1 = [device supportsFamily:MTLGPUFamilyMac1];
    supports_apple4 = [device supportsFamily:MTLGPUFamilyApple4];
  }
  else
  {
    supports_apple4 = [device supportsFeatureSet:MTLFeatureSet_iOS_GPUFamily4_v1];
  }
  config->backend_info.bSupportsDepthClamp = supports_mac1 || supports_apple4;
  config->backend_info.bSupportsST3CTextures = supports_mac1;
  config->backend_info.bSupportsBPTCTextures = supports_mac1;
#endif
  g_features.subgroup_ops = true;
  if (char* env = getenv("MTL_UNIFIED_MEMORY"))
    g_features.unified_memory = env[0] == '1' || env[0] == 'y' || env[0] == 'Y';
  else if (@available(macOS 10.15, iOS 13.0, *))
    g_features.unified_memory = [device hasUnifiedMemory];
  else
    g_features.unified_memory = false;

  if ([[device name] containsString:@"AMD"])
  {
    // Broken
    g_features.subgroup_ops = false;
  }
}

AbstractTextureFormat Metal::Util::ToAbstract(MTLPixelFormat format)
{
  switch (format)
  {
    case MTLPixelFormatRGBA8Unorm:            return AbstractTextureFormat::RGBA8;
    case MTLPixelFormatBGRA8Unorm:            return AbstractTextureFormat::BGRA8;
    case MTLPixelFormatBC1_RGBA:              return AbstractTextureFormat::DXT1;
    case MTLPixelFormatBC2_RGBA:              return AbstractTextureFormat::DXT3;
    case MTLPixelFormatBC3_RGBA:              return AbstractTextureFormat::DXT5;
    case MTLPixelFormatBC7_RGBAUnorm:         return AbstractTextureFormat::BPTC;
    case MTLPixelFormatR16Unorm:              return AbstractTextureFormat::R16;
    case MTLPixelFormatDepth16Unorm:          return AbstractTextureFormat::D16;
    case MTLPixelFormatDepth24Unorm_Stencil8: return AbstractTextureFormat::D24_S8;
    case MTLPixelFormatR32Float:              return AbstractTextureFormat::R32F;
    case MTLPixelFormatDepth32Float:          return AbstractTextureFormat::D32F;
    case MTLPixelFormatDepth32Float_Stencil8: return AbstractTextureFormat::D32F_S8;
    default:                                  return AbstractTextureFormat::Undefined;
  }
}

MTLPixelFormat Metal::Util::FromAbstract(AbstractTextureFormat format)
{
  switch (format)
  {
    case AbstractTextureFormat::RGBA8:     return MTLPixelFormatRGBA8Unorm;
    case AbstractTextureFormat::BGRA8:     return MTLPixelFormatBGRA8Unorm;
    case AbstractTextureFormat::DXT1:      return MTLPixelFormatBC1_RGBA;
    case AbstractTextureFormat::DXT3:      return MTLPixelFormatBC2_RGBA;
    case AbstractTextureFormat::DXT5:      return MTLPixelFormatBC3_RGBA;
    case AbstractTextureFormat::BPTC:      return MTLPixelFormatBC7_RGBAUnorm;
    case AbstractTextureFormat::R16:       return MTLPixelFormatR16Unorm;
    case AbstractTextureFormat::D16:       return MTLPixelFormatDepth16Unorm;
    case AbstractTextureFormat::D24_S8:    return MTLPixelFormatDepth24Unorm_Stencil8;
    case AbstractTextureFormat::R32F:      return MTLPixelFormatR32Float;
    case AbstractTextureFormat::D32F:      return MTLPixelFormatDepth32Float;
    case AbstractTextureFormat::D32F_S8:   return MTLPixelFormatDepth32Float_Stencil8;
    case AbstractTextureFormat::Undefined: return MTLPixelFormatInvalid;
  }
}

static const TBuiltInResource* GetCompilerResourceLimits()
{
  return &glslang::DefaultTBuiltInResource;
}

static bool InitializeGlslang()
{
  static bool glslang_initialized = false;
  if (glslang_initialized)
    return true;

  if (!glslang::InitializeProcess())
  {
    PanicAlertFmt("Failed to initialize glslang shader compiler");
    return false;
  }

  std::atexit([]() { glslang::FinalizeProcess(); });

  glslang_initialized = true;
  return true;
}

static const char SHADER_HEADER[] = R"(
  // Target GLSL 4.5.
  #version 450 core
  #define ATTRIBUTE_LOCATION(x) layout(location = x)
  #define FRAGMENT_OUTPUT_LOCATION(x) layout(location = x)
  #define FRAGMENT_OUTPUT_LOCATION_INDEXED(x, y) layout(location = x, index = y)
  #define UBO_BINDING(packing, x) layout(packing, set = 0, binding = (x - 1))
  #define SAMPLER_BINDING(x) layout(set = 1, binding = x)
  #define TEXEL_BUFFER_BINDING(x) layout(set = 1, binding = (x + 8))
  #define SSBO_BINDING(x) layout(set = 2, binding = x)
  #define INPUT_ATTACHMENT_BINDING(x, y, z) layout(set = x, binding = y, input_attachment_index = z)
  #define VARYING_LOCATION(x) layout(location = x)
  #define FORCE_EARLY_Z layout(early_fragment_tests) in

  // Metal framebuffer fetch helpers.
  #define FB_FETCH_VALUE subpassLoad(in_ocol0)

  // hlsl to glsl function translation
  #define API_METAL_SPV 1
  #define float2 vec2
  #define float3 vec3
  #define float4 vec4
  #define uint2 uvec2
  #define uint3 uvec3
  #define uint4 uvec4
  #define int2 ivec2
  #define int3 ivec3
  #define int4 ivec4
  #define frac fract
  #define lerp mix

  // These were changed in Vulkan
  #define gl_VertexID gl_VertexIndex
  #define gl_InstanceID gl_InstanceIndex
)";
static const char COMPUTE_SHADER_HEADER[] = R"(
  // Target GLSL 4.5.
  #version 450 core
  // All resources are packed into one descriptor set for compute.
  #define UBO_BINDING(packing, x) layout(packing, set = 0, binding = (x - 1))
  #define SAMPLER_BINDING(x) layout(set = 0, binding = (1 + x))
  #define TEXEL_BUFFER_BINDING(x) layout(set = 0, binding = (3 + x))
  #define IMAGE_BINDING(format, x) layout(format, set = 0, binding = (5 + x))

  // hlsl to glsl function translation
  #define API_METAL_SPV 1
  #define float2 vec2
  #define float3 vec3
  #define float4 vec4
  #define uint2 uvec2
  #define uint3 uvec3
  #define uint4 uvec4
  #define int2 ivec2
  #define int3 ivec3
  #define int4 ivec4
  #define frac fract
  #define lerp mix
)";
static const char SUBGROUP_HELPER_HEADER[] = R"(
  #extension GL_KHR_shader_subgroup_basic : enable
  #extension GL_KHR_shader_subgroup_arithmetic : enable
  #extension GL_KHR_shader_subgroup_ballot : enable

  #define SUPPORTS_SUBGROUP_REDUCTION 1
  #define CAN_USE_SUBGROUP_REDUCTION true
  #define IS_HELPER_INVOCATION gl_HelperInvocation
  #define IS_FIRST_ACTIVE_INVOCATION (gl_SubgroupInvocationID == subgroupBallotFindLSB(subgroupBallot(!gl_HelperInvocation)))
  #define SUBGROUP_MIN(value) value = subgroupMin(value)
  #define SUBGROUP_MAX(value) value = subgroupMax(value)
)";

using SPIRVCodeType = u32;
using SPIRVCodeVector = std::vector<SPIRVCodeType>;

static std::optional<SPIRVCodeVector> CompileShaderToSPV(EShLanguage stage,
                                                         const char* stage_filename,
                                                         std::string_view source,
                                                         std::string_view header)
{
  if (!InitializeGlslang())
    return std::nullopt;

  std::unique_ptr<glslang::TShader> shader = std::make_unique<glslang::TShader>(stage);
  std::unique_ptr<glslang::TProgram> program;
  glslang::TShader::ForbidIncluder includer;
  EProfile profile = ECoreProfile;
  EShMessages messages =
      static_cast<EShMessages>(EShMsgDefault | EShMsgSpvRules | EShMsgVulkanRules);
  int default_version = 450;

  std::string full_source_code;
  const char* pass_source_code = source.data();
  int pass_source_code_length = static_cast<int>(source.size());
  if (!header.empty())
  {
    constexpr size_t subgroup_helper_header_length = std::size(SUBGROUP_HELPER_HEADER) - 1;
    full_source_code.reserve(header.size() + subgroup_helper_header_length + source.size());
    full_source_code.append(header);

    if (Metal::g_features.subgroup_ops)
      full_source_code.append(SUBGROUP_HELPER_HEADER, subgroup_helper_header_length);
    full_source_code.append(source);
    pass_source_code = full_source_code.c_str();
    pass_source_code_length = static_cast<int>(full_source_code.length());
  }

  // Sub-group operations require Vulkan 1.1 and SPIR-V 1.3.
  shader->setEnvTarget(glslang::EShTargetSpv, glslang::EShTargetSpv_1_3);

  shader->setStringsWithLengths(&pass_source_code, &pass_source_code_length, 1);

  auto DumpBadShader = [&](const char* msg) {
    static int counter = 0;
    std::string filename = fmt::format("/tmp/BadShader_{}_{}.txt", stage_filename, counter++);
    std::ofstream stream(filename);
    if (stream.good())
    {
      stream << full_source_code << std::endl;
      stream << msg << std::endl;
      stream << "Shader Info Log:" << std::endl;
      stream << shader->getInfoLog() << std::endl;
      stream << shader->getInfoDebugLog() << std::endl;
      if (program)
      {
        stream << "Program Info Log:" << std::endl;
        stream << program->getInfoLog() << std::endl;
        stream << program->getInfoDebugLog() << std::endl;
      }
    }
    stream.close();

    PanicAlertFmt("{} (written to {})\nDebug info:\n{}", msg, filename, shader->getInfoLog());
  };

  if (!shader->parse(GetCompilerResourceLimits(), default_version, profile, false, true, messages,
                     includer))
  {
    DumpBadShader("Failed to parse shader");
    return std::nullopt;
  }

  // Even though there's only a single shader, we still need to link it to generate SPV
  program = std::make_unique<glslang::TProgram>();
  program->addShader(shader.get());
  if (!program->link(messages))
  {
    DumpBadShader("Failed to link program");
    return std::nullopt;
  }

  glslang::TIntermediate* intermediate = program->getIntermediate(stage);
  if (!intermediate)
  {
    DumpBadShader("Failed to generate SPIR-V");
    return std::nullopt;
  }

  SPIRVCodeVector out_code;
  spv::SpvBuildLogger logger;
  glslang::SpvOptions options;

  if (g_ActiveConfig.bEnableValidationLayer)
  {
    // Attach the source code to the SPIR-V for tools like RenderDoc.
    intermediate->addSourceText(pass_source_code, pass_source_code_length);

    options.generateDebugInfo = true;
    options.disableOptimizer = true;
    options.optimizeSize = false;
    options.disassemble = false;
    options.validate = true;
  }

  glslang::GlslangToSpv(*intermediate, out_code, &logger, &options);

  // Write out messages
  // Temporary: skip if it contains "Warning, version 450 is not yet complete; most version-specific
  // features are present, but some are missing."
  if (strlen(shader->getInfoLog()) > 108)
    WARN_LOG_FMT(VIDEO, "Shader info log: {}", shader->getInfoLog());
  if (strlen(shader->getInfoDebugLog()) > 0)
    WARN_LOG_FMT(VIDEO, "Shader debug info log: {}", shader->getInfoDebugLog());
  if (strlen(program->getInfoLog()) > 25)
    WARN_LOG_FMT(VIDEO, "Program info log: {}", program->getInfoLog());
  if (strlen(program->getInfoDebugLog()) > 0)
    WARN_LOG_FMT(VIDEO, "Program debug info log: {}", program->getInfoDebugLog());
  const std::string spv_messages = logger.getAllMessages();
  if (!spv_messages.empty())
    WARN_LOG_FMT(VIDEO, "SPIR-V conversion messages: {}", spv_messages);

  // Dump source code of shaders out to file if enabled.
  if (g_ActiveConfig.iLog & CONF_SAVESHADERS)
  {
    static int counter = 0;
    std::string filename = fmt::format("Shader_{}_{}.txt", stage_filename, counter++);

    std::ofstream stream(filename);
    if (stream.good())
    {
      stream << full_source_code << std::endl;
      stream << "Shader Info Log:" << std::endl;
      stream << shader->getInfoLog() << std::endl;
      stream << shader->getInfoDebugLog() << std::endl;
      stream << "Program Info Log:" << std::endl;
      stream << program->getInfoLog() << std::endl;
      stream << program->getInfoDebugLog() << std::endl;
      stream << "SPIR-V conversion messages: " << std::endl;
      stream << spv_messages;
      stream << "SPIR-V:" << std::endl;
      spv::Disassemble(stream, out_code);
    }
  }

  return out_code;
}

std::string Metal::Util::CompileShader(ShaderStage stage, std::string_view source)
{
  std::optional<SPIRVCodeVector> code;
  switch (stage)
  {
    case ShaderStage::Vertex:
      code = CompileShaderToSPV(EShLangVertex, "vs", source, SHADER_HEADER);
      break;
    case ShaderStage::Geometry:
      code = CompileShaderToSPV(EShLangGeometry, "gs", source, SHADER_HEADER);
      break;
    case ShaderStage::Pixel:
      code = CompileShaderToSPV(EShLangFragment, "ps", source, SHADER_HEADER);
      break;
    case ShaderStage::Compute:
      code = CompileShaderToSPV(EShLangCompute, "cs", source, COMPUTE_SHADER_HEADER);
      break;
  }
  if (!code)
    return "";

  static const spirv_cross::MSLResourceBinding resource_bindings[] = {
    {spv::ExecutionModelVertex,   0, 0, 0, 1, 0, 0}, // vs/ubo
    {spv::ExecutionModelVertex,   0, 1, 0, 1, 0, 0}, // vs/ubo
    {spv::ExecutionModelFragment, 0, 0, 0, 0, 0, 0}, // vs/ubo
    {spv::ExecutionModelFragment, 0, 1, 0, 1, 0, 0}, // vs/ubo
    {spv::ExecutionModelFragment, 1, 0, 0, 0, 0, 0}, // ps/samp0
    {spv::ExecutionModelFragment, 1, 1, 0, 0, 1, 1}, // ps/samp1
    {spv::ExecutionModelFragment, 1, 2, 0, 0, 2, 2}, // ps/samp2
    {spv::ExecutionModelFragment, 1, 3, 0, 0, 3, 3}, // ps/samp3
    {spv::ExecutionModelFragment, 1, 4, 0, 0, 4, 4}, // ps/samp4
    {spv::ExecutionModelFragment, 1, 5, 0, 0, 5, 5}, // ps/samp5
    {spv::ExecutionModelFragment, 1, 6, 0, 0, 6, 6}, // ps/samp6
    {spv::ExecutionModelFragment, 1, 7, 0, 0, 7, 7}, // ps/samp7
    {spv::ExecutionModelFragment, 1, 8, 0, 0, 8, 8}, // ps/samp8
    {spv::ExecutionModelFragment, 2, 0, 0, 2, 0, 0}  // ps/ssbo(bbox)
  };

  spirv_cross::CompilerMSL compiler(std::move(*code));

  spirv_cross::CompilerMSL::Options options;
  options.platform = spirv_cross::CompilerMSL::Options::macOS;
  options.set_msl_version(2, 2);
  compiler.set_msl_options(options);

  for (auto& binding : resource_bindings)
    compiler.add_msl_resource_binding(binding);

  return compiler.compile();
}
