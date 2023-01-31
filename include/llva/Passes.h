#ifndef LLVA_PASSES_H
#define LLVA_PASSES_H

#include "llvm/IR/PassManager.h"

namespace llva {
void addAllPasses(llvm::ModulePassManager &MPM, bool DefaultOrdered,
                  bool ExitOnFail);

void addAssertInlinerPass(llvm::ModulePassManager &MPM, bool DefaultOrdered);
void addRunnerGeneratorPass(llvm::ModulePassManager &MPM);
void addResultInlinerPass(llvm::ModulePassManager &MPM, bool ExitOnFail);
} // namespace llva

#endif
