#include "llva/Passes.h"
#include "llva/AssertInliner.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"

using namespace llvm;

namespace llva {
void addAssertInliner(llvm::ModulePassManager &MPM) {
  MPM.addPass(AssertInlininer());
}
} // namespace llva

PassPluginLibraryInfo getLLVMUserPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "LLVA", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
#if LLVM_VERSION_MAJOR > 11
            PB.registerPipelineStartEPCallback(
                [](ModulePassManager &MPM, PassBuilder::OptimizationLevel) {
                  MPM.addPass(llva::AssertInlininer());
                });
#else
            PB.registerPipelineStartEPCallback([](ModulePassManager &MPM) {
              MPM.addPass(llva::AssertInlininer());
            });
#endif
            PB.registerPipelineParsingCallback(
                [](StringRef Name, ModulePassManager &MPM,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name == "llva") {
                    MPM.addPass(llva::AssertInlininer());
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
