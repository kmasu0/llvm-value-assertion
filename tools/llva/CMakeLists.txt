add_llva_tool(llva
  llva.cpp

  LLVM_LINK_LIBS
    Core
    IRReader
    Analysis
    TransformUtils

  LINK_LIBS
    LLVA
  )
