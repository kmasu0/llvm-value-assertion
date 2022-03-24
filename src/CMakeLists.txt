set(LLVM_LINK_COMPONENTS
  Support
  Core
  )

add_llvm_library(LLVA
  AssertInliner.cpp
  Passes.cpp
  )
