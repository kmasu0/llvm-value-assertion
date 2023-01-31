set(LLVM_LINK_COMPONENTS
  Support
  Core
  )

add_llvm_library(llvaAssertInliner
  Core.cpp
  Passes.cpp
  )
