#include "llva/AssertInliner.h"
#include "llva/Passes.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/InitializePasses.h"
#include "llvm/PassRegistry.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>

using namespace llvm;

static cl::OptionCategory LLVACategory("LLVA Options");

static cl::opt<std::string> InputFilename(cl::Positional, cl::cat(LLVACategory),
                                          cl::desc("<input LLVM IR file>"),
                                          cl::init("-"));
static cl::opt<std::string> OutputFilename("o", cl::cat(LLVACategory),
                                           cl::desc("Output filename"),
                                           cl::value_desc("filename"));

LLVM_ATTRIBUTE_NORETURN static void report_error(Twine msg,
                                                 StringRef filename = "") {
  SmallString<256> prefix;
  if (!filename.empty()) {
    if (filename == "-")
      filename = "<stdin>";
    ("'" + Twine(filename) + "':").toStringRef(prefix);
  }
  WithColor::error(errs(), "llva") << prefix << msg << "\n";
  exit(1);
}

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

  cl::HideUnrelatedOptions({LLVACategory});
  cl::ParseCommandLineOptions(argc, argv, overview, &err_rsos);
  auto args = makeArrayRef(argv + 1, argv + argc);

  SMDiagnostic err_diag;
  std::unique_ptr<Module> mod = parseIRFile(InputFilename, err_diag, ctx);
  if (!mod) {
    err_diag.print("llva", WithColor::error(errs(), argv[0]));
    return 1;
  }

  ModuleAnalysisManager mam;
  llva::AssertInlininer inliner;
  inliner.run(*mod, mam);

  bool emit_to_file = true;
  if (OutputFilename.empty()) {
    OutputFilename = "-";
    emit_to_file = false;
  }

  std::error_code errcode;
  sys::fs::OpenFlags flags = sys::fs::OF_Text;
  auto out = std::make_unique<ToolOutputFile>(OutputFilename, errcode, flags);
  if (errcode)
    report_error(errcode.message());

  PrintModulePass(out->os()).run(*mod, mam);

  if (emit_to_file)
    out->keep();

  return 0;
}
