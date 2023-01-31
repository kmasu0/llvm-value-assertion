add_llva_tool(llva
  llva.cpp

  LLVM_LINK_LIBS
    Analysis
    Core
    IRReader
    Passes
    TransformUtils

  LINK_LIBS
    llvaAssertInliner
  )
