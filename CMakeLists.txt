cmake_minimum_required(VERSION 3.14.3)

project(llva
  VERSION 0.1.0
  LANGUAGES C CXX ASM
  )

option(LLVM_CONFIG "llvm-config command path")
if (NOT LLVM_CONFIG)
  set (LLVM_CONFIG "llvm-config")
endif()

execute_process(
  COMMAND "${LLVM_CONFIG}"
  "--prefix"
  "--bindir"
  "--cmakedir"
  RESULT_VARIABLE HAD_ERROR
  OUTPUT_VARIABLE LLVM_CONFIG_OUTPUT
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

if(HAD_ERROR)
  message(FATAL_ERROR "llvm-config failed with status ${HAD_ERROR}")
endif()

string(
  REGEX REPLACE "[ \t]*[\r\n]+[ \t]*" ";" LLVM_CONFIG_OUTPUT
  "${LLVM_CONFIG_OUTPUT}"
)

list(GET LLVM_CONFIG_OUTPUT 0 INSTALL_ROOT)
list(GET LLVM_CONFIG_OUTPUT 1 BIN_DIR)
list(GET LLVM_CONFIG_OUTPUT 2 LLVM_CMAKE_PATH)

set(LLVM_ROOT_DIR ${INSTALL_ROOT} CACHE PATH "path to LLVM install root")
set(LLVM_BINARY_DIR ${BIN_DIR} CACHE PATH "path to llvm/bin")

if(NOT EXISTS "${LLVM_CMAKE_PATH}/LLVMConfig.cmake")
  message(FATAL_ERROR "LLVMConfig.cmake not found")
endif()
include("${LLVM_CMAKE_PATH}/LLVMConfig.cmake")

# Add LLVM cmake scripts
list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_PATH}")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

set(LLVM_LIBRARY_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib${LLVM_LIBDIR_SUFFIX})
set(LLVM_RUNTIME_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bin)
set(LLVM_BUILD_TOOLS ON)

include(AddLLVM)
include(HandleLLVMOptions)
include(AddLLVA)

include_directories("${LLVM_INCLUDE_DIRS}" ${CMAKE_CURRENT_SOURCE_DIR}/include
  ${CMAKE_CURRENT_BINARY_DIR}/include)
link_directories(${LLVM_LIBRARY_DIRS})

add_subdirectory(src)
add_subdirectory(tools)
