#!/bin/bash

LLVM_CONFIG=/usr/local/bin/llvm-config
BUILD_TYPE=Debug
INSTALL_DIR=$(pwd -P)
if [ $# -ne 0 ]; then
  INSTALL_DIR="$1"
fi

[ -d $BUILD_TYPE ] && rm -r $BUILD_TYPE
cmake -H. -B$BUILD_TYPE -GNinja \
  -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
  -DLLVM_CONFIG=$LLVM_CONFIG \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR

cmake --build $BUILD_TYPE --parallel 4
cmake --install $BUILD_TYPE
