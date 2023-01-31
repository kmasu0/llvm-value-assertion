#ifndef LLVA_ASSERTINLINER_H
#define LLVA_ASSERTINLINER_H

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"

namespace llva {
/// Generate llva.assert.<predicate> functions body and additional codes, and
/// return true when IR is changed, otherwise return false.
bool inlineAssertCmps(llvm::Module &M, bool ExitOnFail, bool DefaultOrdered);
} // namespace llva

#endif
