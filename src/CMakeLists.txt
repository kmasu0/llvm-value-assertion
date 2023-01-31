set(LLVM_LINK_COMPONENTS
  Support
  Core
  )

add_llvm_library(llvaAssertInliner
  AssertInliner.cpp
  Passes.cpp
  )
