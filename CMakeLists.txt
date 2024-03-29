cmake_minimum_required(VERSION 3.14.3)

project(llva
  VERSION 0.1.0
  LANGUAGES C CXX ASM
  )

if (NOT LLVA_LLVM_CONFIG)
  set (LLVA_LLVM_CONFIG "llvm-config")
endif()

execute_process(
  COMMAND "${LLVA_LLVM_CONFIG}"
  "--prefix"
  "--libdir"
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

list(GET LLVM_CONFIG_OUTPUT 0 LLVM_CONFIG_MAKE_PATH)
list(GET LLVM_CONFIG_OUTPUT 1 LIBRARY_DIR)
list(GET LLVM_CONFIG_OUTPUT 2 INCLUDE_DIR)
list(GET LLVM_CONFIG_OUTPUT 3 TOOLS_BINARY_DIR)
file(TO_CMAKE_PATH ${LLVM_CONFIG_MAKE_PATH} LLVM_CMAKE_PATH)

find_package(LLVM REQUIRED HINTS "${LLVM_CMAKE_PATH}")

set(LLVM_LIBRARY_DIR ${LIBRARY_DIR} CACHE PATH "path to llvm/lib")
set(LLVM_INCLUDE_DIR ${INCLUDE_DIR} CACHE PATH "path to llvm/include")
set(LLVM_TOOLS_BINARY_DIR ${TOOLS_BINARY_DIR} CACHE PATH "path to llvm/bin")

# Add LLVM cmake scripts
list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_PATH}")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

set(LLVM_LIBRARY_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/lib${LLVM_LIBDIR_SUFFIX})
set(LLVM_RUNTIME_OUTPUT_INTDIR ${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/bin)
set(LLVM_BUILD_TOOLS ON)

include(HandleLLVMOptions)
include(AddLLVM)
include(AddLLVA)

include_directories("${LLVM_INCLUDE_DIRS}" ${CMAKE_CURRENT_SOURCE_DIR}/include
  ${CMAKE_CURRENT_BINARY_DIR}/include)
link_directories(${LLVM_LIBRARY_DIRS})

add_subdirectory(src)
add_subdirectory(tools)
