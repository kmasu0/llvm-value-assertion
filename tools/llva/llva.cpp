#include "llva/LLVA.h"
#include "llva/Error.h"
#include "llva/Passes.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/InitializePasses.h"
#include "llvm/PassRegistry.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>

using namespace llvm;

static cl::OptionCategory LLVACategory("llva option category");

static cl::opt<std::string> InputFilename(cl::Positional, cl::cat(LLVACategory),
                                          cl::desc("<input LLVM IR file>"),
                                          cl::init("-"));
static cl::opt<std::string> OutputFilename("o", cl::cat(LLVACategory),
                                           cl::desc("Output filename"),
                                           cl::value_desc("filename"));
static cl::opt<bool> ExitOnFail("exit-on-fail", cl::init(false),
                                cl::cat(LLVACategory),
                                cl::desc("Terminate when assertion failed"));
static cl::opt<bool>
    DefaultOrdered("default-ordered", cl::init(true), cl::cat(LLVACategory),
                   cl::desc("Use ordered predicate for floating-point "
                            "comparison in inlining an aggregate value"));

int main(int argc, const char **argv) {
  InitLLVM X(argc, argv);

  std::string Buf;
  raw_string_ostream Errs(Buf);
  const char *Overview = R"(llvm-value-assertion)";

  EnableDebugBuffering = true;
  LLVMContext Ctx;

  PassRegistry *Reg = PassRegistry::getPassRegistry();
  initializeCore(*Reg);
  initializeTransformUtils(*Reg);
  initializeAnalysis(*Reg);

  cl::HideUnrelatedOptions({LLVACategory});
  cl::ParseCommandLineOptions(argc, argv, Overview, &Errs);

  SMDiagnostic ErrDiag;
  std::unique_ptr<Module> M = parseIRFile(InputFilename, ErrDiag, Ctx);
  if (!M) {
    ErrDiag.print("llva", errs());
    return 1;
  }

  LoopAnalysisManager LAM;
  FunctionAnalysisManager FAM;
  CGSCCAnalysisManager CGAM;
  ModuleAnalysisManager MAM;
  PassBuilder PB;

  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  ModulePassManager MPM;

  llva::addAllPasses(MPM, DefaultOrdered, ExitOnFail);
  MPM.run(*M, MAM);

  bool EmitFile = true;
  if (OutputFilename.empty()) {
    OutputFilename = "-";
    EmitFile = false;
  }

  std::error_code ErrCode;
  sys::fs::OpenFlags Flags = sys::fs::OF_Text;
  auto Out = std::make_unique<ToolOutputFile>(OutputFilename, ErrCode, Flags);
  if (ErrCode)
    report_llva_error(ErrCode.message());

  PrintModulePass(Out->os()).run(*M, MAM);

  if (EmitFile)
    Out->keep();

  return 0;
}
