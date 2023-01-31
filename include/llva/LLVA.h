#ifndef LLVA_LLVA_H
#define LLVA_LLVA_H

namespace llvm {
class Module;
}

namespace llva {
/// Generate llva.assert.<predicate> functions body and additional codes, and
/// return true when IR is changed, otherwise return false.
bool inlineAssertCmps(llvm::Module &M, bool DefaultOrdered);
bool generateRunner(llvm::Module &M);
bool inlineResult(llvm::Module &M, bool ExitOnFail);
}

#endif
