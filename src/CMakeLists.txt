set(LLVM_LINK_COMPONENTS
  ADT
  Support
  Core
  )

add_llvm_pass_plugin(libLLVA
  llva.cpp
  )
