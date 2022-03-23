#include "llva/Passes.h"
#include "llva/AssertInliner.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/InitializePasses.h"
#include "llvm/PassRegistry.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>

using namespace llvm;

static cl::opt<std::string> InputFilename(cl::Positional,
                                          cl::desc("<input LLVM asm file>"),
                                          cl::init("-"));

int main(int argc, const char **argv) {
  InitLLVM X(argc, argv);

  std::string buf;
  raw_string_ostream err_rsos(buf);
  const char *overview = R"(llvm-value-assertion)";

  EnableDebugBuffering = true;
  LLVMContext ctx;

  PassRegistry *reg = PassRegistry::getPassRegistry();
  initializeCore(*reg);
  initializeTransformUtils(*reg);
  initializeAnalysis(*reg);

  cl::ParseCommandLineOptions(argc, argv, overview, &err_rsos);
  auto args = makeArrayRef(argv + 1, argv + argc);

  SMDiagnostic err_diag;
  std::unique_ptr<Module> mod = parseIRFile(InputFilename, err_diag, ctx);
  if (!mod) {
    err_diag.print(argv[0], WithColor::error(errs(), argv[0]));
    return 1;
  }

  ModuleAnalysisManager mam;
  llva::AssertInlininer inliner;
  inliner.run(*mod, mam);

  outs() << *mod;

  return 0;
}
