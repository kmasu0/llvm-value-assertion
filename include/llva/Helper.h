#ifndef LLVA_HELPER_H
#define LLVA_HELPER_H

#include "llvm/IR/IRBuilder.h"
#include "llvm/ADT/StringRef.h"
#include <map>
#include <string>
#include <vector>

namespace llvm {
class Constant;
class DataLayout;
class FunctionCallee;
class Module;
class Value;
class Type;

class raw_ostream;
class StringRef;
} // namespace llvm

namespace llva {
class LLVAHelper {
  unsigned NumGlobalTmps = 0;
  std::map<llvm::Type *, llvm::Value *> TypeFormatStrs;

public:
  std::string createLocalTmpName(unsigned N);

  llvm::Value *getGlobalTmpStr(llvm::StringRef Str, llvm::Module &M) {
    return getGlobalStr(Str, std::to_string(NumGlobalTmps++), M);
  }

  llvm::Value *getCheckStr(llvm::Module &M) {
    return getGlobalStr("llva: check %s\n", "check", M);
  }
  llvm::Value *getNGStr(llvm::Module &M) {
    return getGlobalStr("llva: assertion failed!\n", "ng", M);
  }
  llvm::Value *getLHSStr(llvm::Module &M) {
    return getGlobalStr("left", "lhs", M);
  }
  llvm::Value *getRHSStr(llvm::Module &M) {
    return getGlobalStr("right", "rhs", M);
  }

  llvm::Value *getTypeFormatStr(llvm::Type *T, llvm::Module &M);

  llvm::FunctionCallee getFormatStringPrinter(llvm::Module &M);

  llvm::Value *getGlobalStr(llvm::StringRef Str, llvm::StringRef Name,
                            llvm::Module &M);

  void callFail(llvm::IRBuilder<> &Builder, llvm::Module &M);

  void dividePrintableSize(llvm::Value *V, bool Signed,
                           llvm::IRBuilder<> &Builder,
                           const llvm::DataLayout &DL,
                           std::vector<llvm::Value *> &Dsts);

private:
  void printTypedFormatStr(llvm::Type *T, llvm::raw_ostream &OS,
                           const llvm::DataLayout &DL);
  llvm::Value *castToI8Ptr(llvm::Constant *C);
};
} // namespace llva

#endif
