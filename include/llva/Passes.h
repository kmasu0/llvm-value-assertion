#ifndef LLVA_PASSES_H
#define LLVA_PASSES_H

#include "llvm/IR/PassManager.h"

namespace llva {
void addAssertInliner(llvm::ModulePassManager &MPM);
}

#endif
