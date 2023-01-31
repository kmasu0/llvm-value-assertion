#ifndef LLVA_ASSERTINLINER_H
#define LLVA_ASSERTINLINER_H

namespace llvm {
class Module;
}

namespace llva {
/// Generate llva.assert.<predicate> functions body and additional codes, and
/// return true when IR is changed, otherwise return false.
bool inlineAssertCmps(llvm::Module &M, bool ExitOnFail, bool DefaultOrdered);
} // namespace llva

#endif
