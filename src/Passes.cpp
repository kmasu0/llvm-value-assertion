#include "llva/Passes.h"
#include "llva/AssertInliner.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;

namespace {
class AssertInlininerPass : public llvm::PassInfoMixin<AssertInlininerPass> {
  bool ExitOnFail = true;
  bool DefaultOrdered = true;

public:
  AssertInlininerPass() = default;
  AssertInlininerPass(bool ExitOnFail, bool DefaultOrdered)
      : ExitOnFail(ExitOnFail), DefaultOrdered(DefaultOrdered) {}
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM) {
    return llva::inlineAssertCmps(M, ExitOnFail, DefaultOrdered)
               ? llvm::PreservedAnalyses::none()
               : llvm::PreservedAnalyses::all();
  }
};
} // namespace

namespace llva {
void addAssertInlinerPass(llvm::ModulePassManager &MPM, bool ExitOnFail,
                          bool DefaultOrdered) {
  MPM.addPass(AssertInlininerPass(ExitOnFail, DefaultOrdered));
}
} // namespace llva

PassPluginLibraryInfo getLLVMUserPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "LLVA", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
#if LLVM_VERSION_MAJOR >= 14
            PB.registerPipelineStartEPCallback(
                [](ModulePassManager &MPM, OptimizationLevel) {
                  MPM.addPass(AssertInlininerPass());
                });
#elif LLVM_VERSION_MAJOR >= 12
            PB.registerPipelineStartEPCallback(
                [](ModulePassManager &MPM, PassBuilder::OptimizationLevel) {
                  MPM.addPass(AssertInlininerPass());
                });
#else
            PB.registerPipelineStartEPCallback([](ModulePassManager &MPM) {
              MPM.addPass(AssertInlininerPass());
            });
#endif
            PB.registerPipelineParsingCallback(
                [](StringRef Name, ModulePassManager &MPM,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name == "llva") {
                    MPM.addPass(AssertInlininerPass());
                    return true;
                  }
                  return false;
                });
          }};
}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return getLLVMUserPluginInfo();
}
