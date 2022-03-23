macro(add_llva_tool name)
  cmake_parse_arguments(ARG
    ""
    ""
    "LINK_LIBS;LLVM_LINK_LIBS"
    ${ARGN})

  set(LLVM_LINK_COMPONENTS
    ${ARG_LLVM_LINK_LIBS}
    )

  add_llvm_tool(${name}
    ${ARG_UNPARSED_ARGUMENTS}
    )
  target_link_libraries(llva
    PRIVATE
    ${ARG_LINK_LIBS}
    )
endmacro(add_llva_tool)
