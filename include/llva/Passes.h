#ifndef LLVA_PASSES_H
#define LLVA_PASSES_H

#include "llvm/IR/PassManager.h"

namespace llva {
void addAssertInlinerPass(llvm::ModulePassManager &MPM, bool ExitOnFail,
                          bool DefaultOrdered);
}

#endif
