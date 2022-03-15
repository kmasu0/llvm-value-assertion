#!/bin/bash

LLVM_CONFIG=/usr/local/bin/llvm-config
BUILD_TYPE=Debug

[ -d $BUILD_TYPE ] && rm -r $BUILD_TYPE
cmake -H. -B$BUILD_TYPE -GNinja \
  -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
  -DLLVM_CONFIG=$LLVM_CONFIG \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON

cmake --build $BUILD_TYPE
