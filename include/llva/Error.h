#ifndef LLVA_ERROR_H
#define LLVA_ERROR_H

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"

namespace llva {
void set_target_file_name(llvm::StringRef FileName);
[[noreturn]] void report_error_with_source_location(llvm::Twine Msg,
                                                    llvm::StringRef File,
                                                    unsigned Line);
} // namespace llva
#define report_llva_error(MSG)                                                 \
  ::llva::report_error_with_source_location(MSG, __FILE__, __LINE__)

#endif
