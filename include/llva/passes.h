#ifndef LLVA_PASSES_H
#define LLVA_PASSES_H

#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"

namespace llva {
enum AssertKind {
  AK_Eq, // llva.assert.eq
  AK_Ne, // llva.assert.ne
};

class AssertInlininer : public llvm::PassInfoMixin<AssertInlininer> {
public:
  AssertInlininer() = default;
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
};
} // namespace llva

#endif
