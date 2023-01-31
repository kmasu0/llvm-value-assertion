#!/bin/bash

LLVM_CONFIG="/usr/local/bin/llvm-config"
BUILD_TYPE=Debug
INSTALL_DIR=$(pwd -P)
if [ $# -ne 0 ]; then
  INSTALL_DIR="$1"
fi

[ -d $BUILD_TYPE ] && rm -r $BUILD_TYPE
cmake -H. -B$BUILD_TYPE -GNinja           \
  -DCMAKE_BUILD_TYPE=$BUILD_TYPE          \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON      \
  -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR     \
  -DCMAKE_C_COMPILER=clang                \
  -DCMAKE_CXX_COMPILER=clang++            \
  -DCMAKE_CXX_STANDARD=17                 \
  -DLLVA_LLVM_CONFIG=$LLVM_CONFIG

cmake --build $BUILD_TYPE --parallel 4
cmake --install $BUILD_TYPE
