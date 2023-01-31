#include "llva/Passes.h"
#include "llva/LLVA.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;

namespace {
class AssertInlininerPass : public llvm::PassInfoMixin<AssertInlininerPass> {
  bool DefaultOrdered = true;

public:
  AssertInlininerPass() = default;
  AssertInlininerPass(bool DefaultOrdered) : DefaultOrdered(DefaultOrdered) {}
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM) {
    return llva::inlineAssertCmps(M, DefaultOrdered)
               ? llvm::PreservedAnalyses::none()
               : llvm::PreservedAnalyses::all();
  }
};

class RunnerGeneratorPass : public llvm::PassInfoMixin<RunnerGeneratorPass> {
public:
  RunnerGeneratorPass() = default;
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM) {
    return llva::generateRunner(M) ? llvm::PreservedAnalyses::none()
                                   : llvm::PreservedAnalyses::all();
  }
};

class ResultInlinerPass : public llvm::PassInfoMixin<ResultInlinerPass> {
  bool ExitOnFail = false;

public:
  ResultInlinerPass() = default;
  ResultInlinerPass(bool ExitOnFail) : ExitOnFail(ExitOnFail) {}
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM) {
    return llva::inlineResult(M, ExitOnFail) ? llvm::PreservedAnalyses::none()
                                             : llvm::PreservedAnalyses::all();
  }
};

} // namespace

namespace llva {
void addAllPasses(llvm::ModulePassManager &MPM, bool DefaultOrdered,
                  bool ExitOnFail) {
  addAssertInlinerPass(MPM, DefaultOrdered);
  addRunnerGeneratorPass(MPM);
  addResultInlinerPass(MPM, ExitOnFail);
}

void addAssertInlinerPass(llvm::ModulePassManager &MPM, bool DefaultOrdered) {
  MPM.addPass(AssertInlininerPass(DefaultOrdered));
}

void addRunnerGeneratorPass(llvm::ModulePassManager &MPM) {
  MPM.addPass(RunnerGeneratorPass());
}

void addResultInlinerPass(llvm::ModulePassManager &MPM, bool ExitOnFail) {
  MPM.addPass(ResultInlinerPass(ExitOnFail));
}
} // namespace llva

PassPluginLibraryInfo getLLVMUserPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "LLVA", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
#if LLVM_VERSION_MAJOR >= 14
            PB.registerPipelineStartEPCallback(
                [](ModulePassManager &MPM, OptimizationLevel) {
                  llva::addAllPasses(MPM, true, false);
                });
#elif LLVM_VERSION_MAJOR >= 12
            PB.registerPipelineStartEPCallback(
                [](ModulePassManager &MPM, PassBuilder::OptimizationLevel) {
                  llva::addAllPasses(MPM, true, false);
                });
#else
            PB.registerPipelineStartEPCallback([](ModulePassManager &MPM) {
              llva::addAllPasses(MPM, true, false);
            });
#endif
            PB.registerPipelineParsingCallback(
                [](StringRef Name, ModulePassManager &MPM,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name == "llva") {
                    llva::addAllPasses(MPM, true, false);
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
