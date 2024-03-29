#include "llva/Error.h"
#include "llva/LLVA.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"
#include <map>

#define DEBUG_TYPE "llva"
using namespace llvm;

/// Prefix of internal string variable names.
static constexpr const char *StrPrefix = "llva.str.";
/// Prefix of internal virtual register name.
static constexpr const char *LocalTmpPrefix = "llva.t";
/// Prefix of assert function names.
static constexpr const char *AssertFnPrefix = "llva.assert.";

static const unsigned MaxPrintableSize = 8;
// TODO: Change this.
/// Name of file being processed.
static StringRef TargetFileName = "<unknown>";

static bool generateRunnerFunctionBody(Module &M, IRBuilder<> &Builder);
static bool inlineResultFunctions(Module &M, IRBuilder<> &Builder,
                                  bool ExitOnFail);
static FunctionCallee getFormatStringPrinter(Module &M);
static Value *getGlobalStr(StringRef Str, StringRef Name, Module &M);

namespace {
class AssertInliner {
  bool DefaultOrdered = true;
  unsigned NumLocalTmps = 0;
  unsigned NumGlobalTmps = 0;
  std::map<Type *, Value *> TypeFormatStrs;

public:
  AssertInliner() = default;
  AssertInliner(bool DefaultOrdered) : DefaultOrdered(DefaultOrdered) {}

  /// Generate llva.assert.<predicate> functions body and additional codes, and
  /// return true when IR is changed, otherwise return false.
  bool inlineAsserts(Module &M);
  /// Return icmp and fcmp predicate for the specified assert function.
  std::pair<CmpInst::Predicate, CmpInst::Predicate>
  parseAssertFnName(Function &F);
  bool parseAssertFnName(Function &F, CmpInst::Predicate &ICmpPred,
                         CmpInst::Predicate &FCmpPred);
  /// Insert call printing assertion failure.
  void printFail(Value *LHS, Value *RHS, bool Signed, IRBuilder<> &Builder,
                 Module &M);
  /// Insert print call for the specified assert function call, and return true
  /// when success, otherwise return false.
  void printCall(CallInst &CI, IRBuilder<> &Builder);

  /// Get or create and return assert function using comparison of the specified
  /// predicates.
  Function *getOrCreateAssertFn(StringRef Name, CmpInst::Predicate ICmpPred,
                                CmpInst::Predicate FCmpPred, Type *ArgT,
                                Module &M, IRBuilder<> &Builder);

private:
  std::string createLocalTmpName(unsigned N);

  Value *getGlobalTmpStr(StringRef Str, Module &M) {
    return getGlobalStr(Str, std::to_string(NumGlobalTmps++), M);
  }

  Value *getCheckStr(Module &M) {
    return getGlobalStr("llva: check %s\n", "check", M);
  }
  Value *getNGStr(Module &M) {
    return getGlobalStr("llva: assertion failed!\n", "ng", M);
  }
  Value *getLHSStr(Module &M) { return getGlobalStr("left", "lhs", M); }
  Value *getRHSStr(Module &M) { return getGlobalStr("right", "rhs", M); }

  Value *getTypeFormatStr(Type *T, Module &M);

  void callFail(IRBuilder<> &Builder, Module &M);

  void dividePrintableSize(Value *V, bool Signed, IRBuilder<> &Builder,
                           const DataLayout &DL, std::vector<Value *> &Dsts);

private:
  void printTypedFormatStr(Type *T, raw_ostream &OS, const DataLayout &DL);

  /// Generate the specified function body using comparison of the specified
  /// predicates.
  bool generateAssertFnBody(Function &F, CmpInst::Predicate ICmpPred,
                            CmpInst::Predicate FCmpPred, IRBuilder<> &Builder);
  /// Generate the specified function body by comparing of predicates searched
  /// by the specified funtion name.
  inline bool generateAssertFnBody(Function &F, IRBuilder<> &Builder) {
    CmpInst::Predicate ICmpPred, FCmpPred;
    if (parseAssertFnName(F, ICmpPred, FCmpPred))
      return generateAssertFnBody(F, ICmpPred, FCmpPred, Builder);
    return false;
  }

  /// Insert call printing the given value. The given value is treated as LHS
  /// when \p LHS is true, otherwise it is treated as RHS.
  void printValue(Value *V, bool LHS, bool Signed, IRBuilder<> &Builder,
                  Module &M);
  void printLHS(Value *V, bool Signed, IRBuilder<> &Builder, Module &M) {
    printValue(V, true, Signed, Builder, M);
  }
  void printRHS(Value *V, bool Signed, IRBuilder<> &Builder, Module &M) {
    printValue(V, false, Signed, Builder, M);
  }

  Value *insertCmp(CmpInst::Predicate ICmpPred, CmpInst::Predicate FCmpPred,
                   Value *LHS, Value *RHS, IRBuilder<> &Builder,
                   raw_ostream &Err);

  std::string createLocalTmpName() {
    return createLocalTmpName(NumLocalTmps++);
  }
};
} // namespace

namespace llva {
bool inlineAssertCmps(Module &M, bool DefaultOrdered) {
  set_target_file_name(M.getSourceFileName());

  AssertInliner Inliner(DefaultOrdered);
  return Inliner.inlineAsserts(M);
}

bool generateRunner(llvm::Module &M) {
  IRBuilder<> Builder(M.getContext());
  return generateRunnerFunctionBody(M, Builder);
}

bool inlineResult(llvm::Module &M, bool ExitOnFail) {
  IRBuilder<> Builder(M.getContext());
  return inlineResultFunctions(M, Builder, ExitOnFail);
}

void set_target_file_name(StringRef FileName) { TargetFileName = FileName; }

[[noreturn]] void report_error_with_source_location(Twine Msg, StringRef File,
                                                    unsigned Line) {
  SmallString<256> Prefix;
  if (!TargetFileName.empty()) {
    if (TargetFileName == "-")
      TargetFileName = "<stdin>";
    (Twine(TargetFileName) + ": ").toStringRef(Prefix);
  }
  WithColor::error(errs(), "llva") << Prefix << Msg << '\n'
                                   << "(at " << File << ':' << Line << '\n';
  llvm_unreachable("llva crashed");
}
} // namespace llva

std::string AssertInliner::createLocalTmpName(unsigned N) {
  std::string Name = LocalTmpPrefix;
  Name += std::to_string(N);
  return Name;
}

Value *AssertInliner::getTypeFormatStr(Type *T, Module &M) {
  auto It = TypeFormatStrs.find(T);
  if (It != TypeFormatStrs.end())
    return It->second;

  unsigned N = TypeFormatStrs.size();
  auto &Entry = TypeFormatStrs[T];
  std::string Buf;
  raw_string_ostream RSOS(Buf);

  RSOS << "\t%5s: ";
  printTypedFormatStr(T, RSOS, M.getDataLayout());
  RSOS << '\n';

  Entry = getGlobalStr(RSOS.str(), "type" + std::to_string(N), M);
  return Entry;
}

void AssertInliner::printTypedFormatStr(Type *T, raw_ostream &OS,
                                        const DataLayout &DL) {
  if (auto *IT = dyn_cast<IntegerType>(T)) {
    unsigned Size = DL.getTypeAllocSize(IT);
    unsigned N = divideCeil(Size, MaxPrintableSize);
    OS << "0x";
    for (unsigned i = 0; i != N; ++i)
      OS << "%lx,";
  } else if (T->isFloatingPointTy()) {
    if (T->isFloatTy())
      OS << "%f";
    else if (T->isDoubleTy())
      OS << "%lf";
    else if (T->isX86_FP80Ty())
      OS << "%Lf";
    else if (T->isFP128Ty())
      OS << "%Lf";
    else
      llvm_unreachable("unsupported floating-point type");
  } else if (T->isPointerTy()) {
    OS << "%p";
  } else if (auto *VT = dyn_cast<VectorType>(T)) {
    std::string EtStr;
    raw_string_ostream EtOS(EtStr);
    printTypedFormatStr(VT->getElementType(), EtOS, DL);
    EtOS.flush();

    OS << "<";

#if LLVM_VERSION_MAJOR >= 12
    unsigned N = VT->getElementCount().getKnownMinValue();
#else
    unsigned N = VT->getNumElements();
#endif
    for (unsigned I = 0; I != N; ++I) {
      if (I != 0)
        OS << ", ";
      OS << EtStr;
    }
    OS << ">";
  } else if (auto *AT = dyn_cast<ArrayType>(T)) {
    std::string EtStr;
    raw_string_ostream EtOS(EtStr);
    printTypedFormatStr(AT->getElementType(), EtOS, DL);
    EtOS.flush();

    OS << "[";
    for (unsigned I = 0, E = AT->getNumElements(); I != E; ++I) {
      if (I != 0)
        OS << ", ";
      OS << EtStr;
    }
    OS << "]";
  } else if (auto *ST = dyn_cast<StructType>(T)) {
    std::string EtStr;
    raw_string_ostream EtOS(EtStr);

    if (ST->isPacked())
      OS << "<";
    OS << "{";

    for (unsigned I = 0, E = ST->getNumElements(); I != E; ++I) {
      if (I != 0)
        OS << ", ";

      EtStr.clear();
      printTypedFormatStr(ST->getElementType(I), EtOS, DL);
      OS << EtOS.str();
    }

    OS << "}";
    if (ST->isPacked())
      OS << ">";
  } else {
    llvm_unreachable("unsupported type found");
  }
}

void AssertInliner::callFail(IRBuilder<> &Builder, Module &M) {
  LLVMContext &Ctx = M.getContext();
  FunctionType *FT = FunctionType::get(Type::getVoidTy(Ctx), false);
  FunctionCallee F = M.getOrInsertFunction("llva.fail", FT);
  Builder.CreateCall(F);
}

void AssertInliner::dividePrintableSize(Value *V, bool Signed,
                                        IRBuilder<> &Builder,
                                        const DataLayout &DL,
                                        std::vector<Value *> &Dsts) {
  Type *T = V->getType();
  LLVMContext &Ctx = T->getContext();

  if (auto *IT = dyn_cast<IntegerType>(T)) {
    unsigned Size = DL.getTypeAllocSize(IT).getFixedValue();
    IntegerType *EtT = Type::getIntNTy(Ctx, MaxPrintableSize * 8);

    if (Size > MaxPrintableSize) {
      // Extend the big value to dividable size, then divide it.
      unsigned N = divideCeil(Size, MaxPrintableSize);
      Type *WideT = Type::getIntNTy(Ctx, N * EtT->getBitWidth());
      Value *Ext =
          Signed ? Builder.CreateSExt(V, WideT) : Builder.CreateZExt(V, WideT);

      for (unsigned i = 0; i != N; ++i) {
        Value *Amt = ConstantInt::get(WideT, EtT->getBitWidth() * (N - i - 1));
        Value *Part = i != N - 1 ? Builder.CreateLShr(Ext, Amt) : Ext;
        Part = Builder.CreateTrunc(Part, EtT);
        Dsts.push_back(Part);
      }
    } else if (Size == MaxPrintableSize) {
      Dsts.push_back(V);
    } else {
      Value *Ext =
          Signed ? Builder.CreateSExt(V, EtT) : Builder.CreateZExt(V, EtT);
      Dsts.push_back(Ext);
    }
  } else if (T->isFloatingPointTy()) {
    Dsts.push_back(V);
  } else if (T->isPointerTy()) {
    Dsts.push_back(V);
  } else if (auto *VT = dyn_cast<VectorType>(T)) {
#if LLVM_VERSION_MAJOR >= 12
    unsigned N = VT->getElementCount().getKnownMinValue();
#else
    unsigned N = VT->getNumElements();
#endif
    for (unsigned i = 0; i != N; ++i) {
      std::vector<Value *> Ets;
      dividePrintableSize(Builder.CreateExtractValue(V, i), Signed, Builder, DL,
                          Ets);
      for (auto *Et : Ets)
        Dsts.push_back(Et);
    }
  } else if (auto *AT = dyn_cast<ArrayType>(T)) {
    for (unsigned i = 0, e = AT->getNumElements(); i != e; ++i) {
      std::vector<Value *> Ets;
      dividePrintableSize(Builder.CreateExtractValue(V, i), Signed, Builder, DL,
                          Ets);
      for (auto *Et : Ets)
        Dsts.push_back(Et);
    }
  } else if (auto *ST = dyn_cast<StructType>(T)) {
    for (unsigned i = 0, e = ST->getNumElements(); i != e; ++i) {
      std::vector<Value *> Ets;
      dividePrintableSize(Builder.CreateExtractValue(V, i), Signed, Builder, DL,
                          Ets);
      for (auto *Et : Ets)
        Dsts.push_back(Et);
    }
  } else {
    llvm_unreachable("unsupported type found");
  }
}

bool AssertInliner::inlineAsserts(Module &M) {
  bool Changed = false;
  IRBuilder<> Builder(M.getContext());

  for (Function &F : M)
    if (generateAssertFnBody(F, Builder))
      Changed = true;

  for (Function &F : M) {
    for (BasicBlock &BB : F) {
      for (Instruction &Inst : BB) {
        CallInst *CI = dyn_cast<CallInst>(&Inst);
        if (!CI)
          continue;

        Function *Callee = CI->getCalledFunction();
        if (!Callee)
          continue;

        CmpInst::Predicate ICmpPred, FCmpPred;
        if (parseAssertFnName(*Callee, ICmpPred, FCmpPred)) {
          printCall(*CI, Builder);
          Changed = true;
        }
      }
    }
  }

  return Changed;
}

std::pair<CmpInst::Predicate, CmpInst::Predicate>
AssertInliner::parseAssertFnName(Function &F) {
  StringRef Name(F.getName());
  if (!Name.startswith(AssertFnPrefix))
    report_llva_error("invalid llva.assert function!");

  StringRef Prefix(AssertFnPrefix);
  StringRef Pred = Name.drop_front(Prefix.size()).split('.').first;
  CmpInst::Predicate ICmpPred = CmpInst::BAD_ICMP_PREDICATE,
                     FCmpPred = CmpInst::BAD_FCMP_PREDICATE;

  if (Pred == "eq") {
    ICmpPred = CmpInst::ICMP_EQ;
    FCmpPred = DefaultOrdered ? CmpInst::FCMP_OEQ : CmpInst::FCMP_UEQ;
  } else if (Pred == "ne") {
    ICmpPred = CmpInst::ICMP_NE;
    FCmpPred = DefaultOrdered ? CmpInst::FCMP_OEQ : CmpInst::FCMP_UEQ;
  } else {
    Type *T = F.args().begin()->getType();
    if (T->isIntegerTy()) {
      ICmpPred = StringSwitch<CmpInst::Predicate>(Pred)
                     .Case("eq", CmpInst::ICMP_EQ)
                     .Case("ne", CmpInst::ICMP_NE)
                     .Case("sgt", CmpInst::ICMP_SGT)
                     .Case("sge", CmpInst::ICMP_SGE)
                     .Case("slt", CmpInst::ICMP_SLT)
                     .Case("sle", CmpInst::ICMP_SLE)
                     .Case("ugt", CmpInst::ICMP_UGT)
                     .Case("uge", CmpInst::ICMP_UGE)
                     .Case("ult", CmpInst::ICMP_ULT)
                     .Case("ule", CmpInst::ICMP_ULE)
                     .Default(CmpInst::BAD_ICMP_PREDICATE);
    } else if (T->isFloatingPointTy()) {
      FCmpPred = StringSwitch<CmpInst::Predicate>(Pred)
                     .Case("oeq", CmpInst::FCMP_OEQ)
                     .Case("one", CmpInst::FCMP_ONE)
                     .Case("ogt", CmpInst::FCMP_OGT)
                     .Case("oge", CmpInst::FCMP_OGE)
                     .Case("olt", CmpInst::FCMP_OLT)
                     .Case("ole", CmpInst::FCMP_OLE)
                     .Case("ueq", CmpInst::FCMP_UEQ)
                     .Case("une", CmpInst::FCMP_UNE)
                     .Case("ugt", CmpInst::FCMP_UGT)
                     .Case("uge", CmpInst::FCMP_UGE)
                     .Case("ult", CmpInst::FCMP_ULT)
                     .Case("ule", CmpInst::FCMP_ULE)
                     .Default(CmpInst::BAD_FCMP_PREDICATE);
    }
  }

  if (ICmpPred == CmpInst::BAD_ICMP_PREDICATE &&
      FCmpPred == CmpInst::BAD_FCMP_PREDICATE)
    report_llva_error("wrong llva.assert compare predicate found.");

  return std::make_pair(ICmpPred, FCmpPred);
}

bool AssertInliner::parseAssertFnName(Function &F, CmpInst::Predicate &ICmpPred,
                                      CmpInst::Predicate &FCmpPred) {
  if (!F.getName().startswith(AssertFnPrefix))
    return false;

  std::tie(ICmpPred, FCmpPred) = parseAssertFnName(F);
  return true;
}

void AssertInliner::printCall(CallInst &CI, IRBuilder<> &Builder) {
  std::string Buf;
  raw_string_ostream RSOS(Buf);
  CI.print(RSOS, true);
  Module &M = *CI.getModule();
  Value *S = getGlobalTmpStr(RSOS.str(), M);

  Builder.SetInsertPoint(&CI);
  Builder.CreateCall(getFormatStringPrinter(M), {getCheckStr(M), S});
}

void AssertInliner::printFail(Value *LHS, Value *RHS, bool Signed,
                              IRBuilder<> &Builder, Module &M) {
  FunctionCallee FmtPrint = getFormatStringPrinter(M);
  Value *FmtStr = getNGStr(M);
  Builder.CreateCall(FmtPrint, {FmtStr});
  printLHS(LHS, Signed, Builder, M);
  printRHS(RHS, Signed, Builder, M);
}

void AssertInliner::printValue(Value *V, bool LHS, bool Signed,
                               IRBuilder<> &Builder, Module &M) {
  std::vector<Value *> Args;
  Args.emplace_back(getTypeFormatStr(V->getType(), M));
  Args.emplace_back(LHS ? getLHSStr(M) : getRHSStr(M));
  dividePrintableSize(V, Signed, Builder, M.getDataLayout(), Args);
  Builder.CreateCall(getFormatStringPrinter(M), Args);
}

Function *AssertInliner::getOrCreateAssertFn(StringRef Name,
                                             CmpInst::Predicate ICmpPred,
                                             CmpInst::Predicate FCmpPred,
                                             Type *ArgT, Module &M,
                                             IRBuilder<> &Builder) {
  LLVMContext &Ctx = ArgT->getContext();

  FunctionType *FT =
      FunctionType::get(Type::getVoidTy(Ctx), {ArgT, ArgT}, false);
  FunctionCallee Callee = M.getOrInsertFunction(Name, FT);
  Function *F = dyn_cast<Function>(Callee.getCallee());
  if (!F)
    return nullptr;

  generateAssertFnBody(*F, ICmpPred, FCmpPred, Builder);
  return F;
}

bool AssertInliner::generateAssertFnBody(Function &F,
                                         CmpInst::Predicate ICmpPred,
                                         CmpInst::Predicate FCmpPred,
                                         IRBuilder<> &Builder) {
  if (!F.isDeclaration())
    return false;

  LLVM_DEBUG(dbgs() << "inline assert function body\n" << F << '\n');
  // Clear virtual register names.
  NumLocalTmps = 0;

  Module &M = *F.getParent();
  LLVMContext &Ctx = M.getContext();
  std::string Buf;
  raw_string_ostream Err(Buf);

  if (F.getReturnType() != Type::getVoidTy(Ctx)) {
    Ctx.diagnose(DiagnosticInfoUnsupported(
        F, "assert function must return void.", F.getSubprogram()));
    return false;
  }

  if (F.arg_size() != 2) {
    Ctx.diagnose(DiagnosticInfoUnsupported(
        F, "function does not have 2 arguments.", F.getSubprogram()));
    return false;
  }

  Value *LHS = F.getArg(0);
  Value *RHS = F.getArg(1);
  if (LHS->getType() != RHS->getType()) {
    Ctx.diagnose(DiagnosticInfoUnsupported(
        F, "arguments does not have the same type.", F.getSubprogram()));
    return false;
  }

  // Start insert instructions.
  BasicBlock *Entry = BasicBlock::Create(Ctx, "entry", &F);
  Builder.SetInsertPoint(Entry);
  Value *Cmp = insertCmp(ICmpPred, FCmpPred, LHS, RHS, Builder, Err);
  BasicBlock *Fail = BasicBlock::Create(Ctx, "assert.fail", &F);
  BasicBlock *End = BasicBlock::Create(Ctx, "assert.end", &F);
  if (Cmp)
    Builder.CreateCondBr(Cmp, End, Fail);
  else
    Builder.CreateBr(Fail);

  // Construct fail block.
  Builder.SetInsertPoint(Fail);
  // Print fail message.
  printFail(LHS, RHS, CmpInst::isSigned(ICmpPred), Builder, M);
  // Call llva.fail.
  callFail(Builder, M);

  Builder.CreateBr(End);
  Builder.SetInsertPoint(End);
  Builder.CreateRetVoid();

  return true;
}

Value *AssertInliner::insertCmp(CmpInst::Predicate ICmpPred,
                                CmpInst::Predicate FCmpPred, Value *LHS,
                                Value *RHS, IRBuilder<> &Builder,
                                raw_ostream &Err) {
  if (LHS->getType() != RHS->getType())
    return nullptr;

  Type *T = LHS->getType();

  if (T->isAggregateType()) {
    Value *Result = nullptr;
    unsigned N =
        T->isStructTy() ? T->getStructNumElements() : T->getArrayNumElements();

    for (unsigned I = 0; I != N; ++I) {
      Value *LHSEt = Builder.CreateExtractValue(LHS, I, createLocalTmpName());
      Value *RHSEt = Builder.CreateExtractValue(RHS, I, createLocalTmpName());
      Value *Cmp = insertCmp(ICmpPred, FCmpPred, LHSEt, RHSEt, Builder, Err);
      if (!Cmp)
        return nullptr;

      Result = I == 0 ? Cmp
                      : Builder.CreateSelect(
                            Result, Cmp,
                            ConstantInt::get(Result->getType(), 0, false),
                            createLocalTmpName());
    }
    return Result;
  } else if (T->isVectorTy()) {
    Type *elt_type = cast<VectorType>(T)->getElementType();
    if (elt_type->isIntegerTy() || elt_type->isPointerTy()) {
      if (!CmpInst::isIntPredicate(ICmpPred))
        return nullptr;

      return Builder.CreateICmp(ICmpPred, LHS, RHS, createLocalTmpName());
    } else if (elt_type->isFloatingPointTy()) {
      if (!CmpInst::isFPPredicate(FCmpPred))
        return nullptr;

      return Builder.CreateFCmp(FCmpPred, LHS, RHS, createLocalTmpName());
    }
  } else if (T->isIntegerTy() || T->isPointerTy()) {
    if (!CmpInst::isIntPredicate(ICmpPred))
      return nullptr;

    return Builder.CreateICmp(ICmpPred, LHS, RHS, createLocalTmpName());
  } else if (T->isFloatingPointTy()) {
    if (!CmpInst::isFPPredicate(FCmpPred))
      return nullptr;

    return Builder.CreateFCmp(FCmpPred, LHS, RHS, createLocalTmpName());
  }

  return nullptr;
}

static FunctionCallee getResultFunction(Module &M) {
  static const char *Name = "llva.result";
  return M.getOrInsertFunction(Name, Type::getInt32Ty(M.getContext()), false);
}

static bool generateRunnerFunctionBody(Module &M, IRBuilder<> &Builder) {
  Function *F = M.getFunction("llva.runtest");
  // Return when llva.runtest is not used or already defined.
  if (!F || !F->isDeclaration())
    return false;

  assert(F->getReturnType()->isIntegerTy(32) &&
         "llva.runtest must return i32!");
  assert(F->arg_size() == 0 && "llva.runtest must not have argument!");

  LLVMContext &Ctx = M.getContext();
  std::map<unsigned, FunctionCallee> Tests;

  for (auto &F : M.functions()) {
    if (F.isDeclaration())
      continue;
    StringRef Name = F.getName();
    StringRef Prefix = "llva.test.";
    if (Name.startswith(Prefix)) {
      APInt Num(32, Name.drop_front(Prefix.size()), 10);
      Tests[Num.getZExtValue()] = FunctionCallee(&F);
    }
  }

  BasicBlock *Entry = BasicBlock::Create(Ctx, "entry", F);
  Builder.SetInsertPoint(Entry);

  for (auto NumAndTest : Tests) {
    unsigned N = NumAndTest.first;
    FunctionCallee Test = NumAndTest.second;
    std::string Str = "[ ";
    Str += Test.getCallee()->getName();
    Str += " ]\n";
    std::string Name = "test" + std::to_string(N);
    Builder.CreateCall(getFormatStringPrinter(M), {getGlobalStr(Str, Name, M)});
    Builder.CreateCall(Test);
  }

  auto *Res = Builder.CreateCall(getResultFunction(M));
  Builder.CreateRet(Res);
  return true;
}

static GlobalVariable *getAssertFailCounter(Module &M) {
  static const char *Name = "llva.num_failed";
  Type *T = Type::getInt32Ty(M.getContext());
  return cast<GlobalVariable>(M.getOrInsertGlobal(Name, T, [&] {
    return new GlobalVariable(M, T, false, GlobalValue::CommonLinkage,
                              ConstantInt::get(T, 0), Name);
  }));
}

static bool inlineResultFunctions(Module &M, IRBuilder<> &Builder,
                                  bool ExitOnFail) {
  bool Changed = false;
  LLVMContext &Ctx = M.getContext();

  for (auto &F : M.functions()) {
    for (auto &BB : F) {
      for (auto &I : make_early_inc_range(BB)) {
        auto *CI = dyn_cast_or_null<CallInst>(&I);
        if (!CI)
          continue;
        Function *F = CI->getCalledFunction();
        if (!F)
          continue;

        if (F->getName() == "llva.fail") {
          Builder.SetInsertPoint(CI);
          if (ExitOnFail) {
            Builder.CreateUnreachable();
          } else {
            auto *GV = getAssertFailCounter(M);
            auto *Cnt = Builder.CreateLoad(Type::getInt32Ty(Ctx), GV);
            auto *Updated = Builder.CreateAdd(
                Cnt, ConstantInt::get(Type::getInt32Ty(Ctx), 1));
            CI->replaceAllUsesWith(Builder.CreateStore(Updated, GV));
            CI->eraseFromParent();
          }
          Changed = true;
        } else if (F->getName() == "llva.result") {
          Builder.SetInsertPoint(CI);
          auto *GV = getAssertFailCounter(M);
          CI->replaceAllUsesWith(Builder.CreateLoad(Type::getInt32Ty(Ctx), GV));
          CI->eraseFromParent();
          Changed = true;
        }
      }
    }
  }

  return Changed;
}

static FunctionCallee getFormatStringPrinter(Module &M) {
  // TODO: Change this.
  static const char *Name = "printf";
  LLVMContext &Ctx = M.getContext();
  FunctionType *FT = FunctionType::get(
      Type::getInt32Ty(Ctx), {Type::getInt8Ty(Ctx)->getPointerTo()}, true);
  return M.getOrInsertFunction(Name, FT);
}

static Value *castToI8Ptr(Constant *C) {
  auto *T = C->getType();
  assert(T->isPointerTy() && "argument of castToI8Ptr must be pointer!");

  if (T->isOpaquePointerTy())
    return C;
  else
    return ConstantExpr::getBitCast(C, Type::getInt8PtrTy(C->getContext()));
}

static Value *getGlobalStr(StringRef Str, StringRef BaseName, Module &M) {
  LLVMContext &Ctx = M.getContext();
  Constant *Init = ConstantDataArray::getString(Ctx, Str);
  std::string Name = StrPrefix;
  Name += BaseName;
  auto *GV = cast<GlobalValue>(M.getOrInsertGlobal(Name, Init->getType(), [&] {
    return new GlobalVariable(M, Init->getType(), true,
                              GlobalValue::PrivateLinkage, Init, Name, nullptr);
  }));
  return castToI8Ptr(GV);
}
