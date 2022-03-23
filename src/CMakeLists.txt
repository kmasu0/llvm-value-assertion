set(LLVM_LINK_COMPONENTS
  ADT
  Support
  Core
  )

add_llvm_pass_plugin(libLLVA-plugin
  AssertInliner.cpp
  Passes.cpp
  )

add_llvm_library(LLVA
  AssertInliner.cpp
  Passes.cpp
  )
