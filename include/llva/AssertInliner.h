#ifndef LLVA_ASSERTINLINER_H
#define LLVA_ASSERTINLINER_H

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"

namespace llva {
/// Generate llva.assert.<predicate> functions body and additional codes, and
/// return true when IR is changed, otherwise return false.
bool inlineAssertCmpIR(llvm::Module &M, bool DefaultOrdered = true);

/// Return icmp and fcmp predicate for the specified assert function.
std::pair<llvm::CmpInst::Predicate, llvm::CmpInst::Predicate>
parseAssertCmpFunctionPredicate(llvm::Function &F, bool DefaultOrdered = true);

inline bool parseAssertCmpFunctionPredicate(llvm::Function &F,
                                            llvm::CmpInst::Predicate &ICmpPred,
                                            llvm::CmpInst::Predicate &FCmpPred,
                                            bool DefaultOrdered = true) {
  std::tie(ICmpPred, FCmpPred) =
      parseAssertCmpFunctionPredicate(F, DefaultOrdered);
  return llvm::CmpInst::isIntPredicate(ICmpPred) ||
         llvm::CmpInst::isFPPredicate(FCmpPred);
}
/// Insert print call for the specified assert function call, and return true
/// when success, otherwise return false.
bool insertAssertCallPrint(llvm::CallInst &CI, llvm::IRBuilder<> &Builder);
/// Get or create and return assert function using comparison of the specified
/// predicates.
llvm::Function *getOrCreateAssertCmpFunction(llvm::StringRef Name,
                                             llvm::CmpInst::Predicate ICmpPred,
                                             llvm::CmpInst::Predicate FCmpPred,
                                             llvm::Type *ArgT, llvm::Module &M,
                                             llvm::IRBuilder<> &Builder);
/// Generate the specified function body using comparison of the specified
/// predicates.
bool generateAssertCmpBody(llvm::Function &F, llvm::CmpInst::Predicate ICmpPred,
                           llvm::CmpInst::Predicate FCmpPred,
                           llvm::IRBuilder<> &Builder);
/// Generate the specified function body by comparing of predicates searched by
/// the specified funtion name.
inline bool generateAssertCmpBody(llvm::Function &F, llvm::IRBuilder<> &Builder,
                                  bool DefaultOrdered = true) {
  llvm::CmpInst::Predicate ICmpPred, FCmpPred;
  if (parseAssertCmpFunctionPredicate(F, ICmpPred, FCmpPred, DefaultOrdered))
    return generateAssertCmpBody(F, ICmpPred, FCmpPred, Builder);
  return false;
}
} // namespace llva

#endif
