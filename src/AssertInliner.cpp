#include "llva/AssertInliner.h"
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
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "llva"
using namespace llvm;

static const char *AssertFnPrefix = "llva.assert.";

static std::string get_global_tmp_name();
static std::string get_local_tmp_name();
static void clear_local_tmp_names();
static GlobalValue *get_string_gv(StringRef str, StringRef name, Module &mod);
static FunctionCallee get_print_format(Module &mod);
/// Return "Check: %s\n"
static GlobalValue *get_check_string(Module &mod);

static inline Value *get_string_i8ptr(GlobalValue *gv, IRBuilder<> &builder) {
  LLVMContext &ctx = gv->getContext();
  Type *i64 = Type::getInt64Ty(ctx);
  Constant *zero = ConstantInt::get(i64, 0);
  return builder.CreateGEP(gv->getType()->getPointerElementType(), gv,
                           {zero, zero});
}
static Value *get_check_string(Module &mod, IRBuilder<> &builder) {
  return get_string_i8ptr(get_check_string(mod), builder);
}
/// Return "NG: %s\n"
static GlobalValue *get_ng_string(Module &mod);
static Value *get_ng_string(Module &mod, IRBuilder<> &builder) {
  return get_string_i8ptr(get_ng_string(mod), builder);
}
/// Return "\tleft: "
static GlobalValue *get_left_string(Module &mod);
static Value *get_left_string(Module &mod, IRBuilder<> &builder) {
  return get_string_i8ptr(get_left_string(mod), builder);
}
/// Return "\tright: "
static GlobalValue *get_right_string(Module &mod);
static Value *get_right_string(Module &mod, IRBuilder<> &builder) {
  return get_string_i8ptr(get_right_string(mod), builder);
}
/// Return format string for given type.
static GlobalValue *get_format_string(Type *type, Module &mod);
static Value *get_format_string(Type *type, Module &mod, IRBuilder<> &builder) {
  return get_string_i8ptr(get_format_string(type, mod), builder);
}
static Value *insert_compare(CmpInst::Predicate icmp_pred,
                             CmpInst::Predicate fcmp_pred, Value *lhs,
                             Value *rhs, IRBuilder<> &builder,
                             raw_ostream &err);
static void insert_divide_value_for_print(Value *val, IRBuilder<> &builder,
                                          std::vector<Value *> &dsts,
                                          bool is_signed);

namespace llva {
CmpInst::Predicate parseICmpPredicate(StringRef pred) {
  return StringSwitch<CmpInst::Predicate>(pred)
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
}

CmpInst::Predicate parseFCmpPredicate(StringRef pred) {
  return StringSwitch<CmpInst::Predicate>(pred)
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

std::pair<llvm::CmpInst::Predicate, llvm::CmpInst::Predicate>
parseAssertCmpFunctionPredicate(Function &func, bool DefaultOrdered) {
  StringRef prefix = AssertFnPrefix;
  StringRef name = func.getName();
  std::pair<CmpInst::Predicate, CmpInst::Predicate> bad_preds = {
      CmpInst::BAD_ICMP_PREDICATE, CmpInst::BAD_FCMP_PREDICATE};

  if (!name.startswith(prefix))
    return bad_preds;

  StringRef pred = name.drop_front(prefix.size()).split('.').first;
  if (pred == "eq")
    return {CmpInst::ICMP_EQ,
            DefaultOrdered ? CmpInst::FCMP_OEQ : CmpInst::FCMP_UEQ};
  else if (pred == "ne")
    return {CmpInst::ICMP_NE,
            DefaultOrdered ? CmpInst::FCMP_ONE : CmpInst::FCMP_UNE};

  Type *type = func.args().begin()->getType();
  if (type->isIntegerTy())
    return {parseICmpPredicate(pred), CmpInst::BAD_FCMP_PREDICATE};
  else if (type->isFloatingPointTy())
    return {CmpInst::BAD_ICMP_PREDICATE, parseFCmpPredicate(pred)};

  return bad_preds;
}

bool insertAssertCallPrint(llvm::CallInst &call, llvm::IRBuilder<> &builder) {
  LLVM_DEBUG(dbgs() << "insert assert call print\n" << call << '\n');

  std::string buf;
  llvm::raw_string_ostream os(buf);
  call.print(os, true);

  Module &mod = *call.getModule();
  Value *call_str = get_string_gv(os.str(), get_global_tmp_name(), mod);

  FunctionCallee assert_fn(call.getCalledFunction());
  FunctionCallee print_format_fn = get_print_format(mod);

  builder.SetInsertPoint(&call);
  builder.CreateCall(print_format_fn,
                     {get_check_string(mod, builder), call_str});

  return true;
}

Function *getOrCreateAssertCmpFunction(StringRef name,
                                       CmpInst::Predicate icmp_pred,
                                       CmpInst::Predicate fcmp_pred,
                                       Type *arg_type, Module &mod,
                                       IRBuilder<> &builder) {
  LLVMContext &ctx = arg_type->getContext();

  FunctionType *fn_type =
      FunctionType::get(Type::getVoidTy(ctx), {arg_type, arg_type}, false);
  FunctionCallee callee = mod.getOrInsertFunction(name, fn_type);
  Function *func = dyn_cast<Function>(callee.getCallee());
  if (!func)
    return nullptr;

  generateAssertCmpBody(*func, icmp_pred, fcmp_pred, builder);
  return func;
}

bool generateAssertCmpBody(Function &func, CmpInst::Predicate icmp_pred,
                           CmpInst::Predicate fcmp_pred, IRBuilder<> &builder) {
  if (!func.isDeclaration())
    return false;

  clear_local_tmp_names();
  LLVM_DEBUG(dbgs() << "inline assert function body\n" << func << '\n');

  Module &mod = *func.getParent();
  LLVMContext &ctx = mod.getContext();
  std::string buf;
  raw_string_ostream err(buf);

  if (func.getReturnType() != Type::getVoidTy(ctx)) {
    ctx.diagnose(DiagnosticInfoUnsupported(
        func, "assert function must return void.", func.getSubprogram()));
    return false;
  }

  if (func.arg_size() != 2) {
    ctx.diagnose(DiagnosticInfoUnsupported(
        func, "function does not have 2 arguments.", func.getSubprogram()));
    return false;
  }

  Value *lhs = func.getArg(0);
  Value *rhs = func.getArg(1);
  if (lhs->getType() != rhs->getType()) {
    ctx.diagnose(DiagnosticInfoUnsupported(
        func, "arguments does not have the same type.", func.getSubprogram()));
    return false;
  }

  FunctionCallee print_format_fn = get_print_format(mod);
  Type *type = lhs->getType();

  // Start insert instructions.
  BasicBlock *entry = BasicBlock::Create(ctx, "entry", &func);
  builder.SetInsertPoint(entry);
  Value *cmp = insert_compare(icmp_pred, fcmp_pred, lhs, rhs, builder, err);
  BasicBlock *failbb = BasicBlock::Create(ctx, "assert.fail", &func);
  BasicBlock *endbb = BasicBlock::Create(ctx, "assert.end", &func);
  if (cmp)
    builder.CreateCondBr(cmp, endbb, failbb);
  else
    builder.CreateBr(failbb);

  builder.SetInsertPoint(failbb);
  // Print "NG:".
  Value *format_str = get_ng_string(mod, builder);
  builder.CreateCall(print_format_fn, {format_str});

  std::vector<Value *> args;
  // Print "\tleft: <values>".
  format_str = get_format_string(type, mod, builder);
  args.push_back(format_str);
  args.push_back(get_left_string(mod, builder));
  insert_divide_value_for_print(lhs, builder, args,
                                CmpInst::isSigned(icmp_pred));
  builder.CreateCall(print_format_fn, args);
  // Print "\tright: <values>".
  args.clear();
  args.push_back(format_str);
  args.push_back(get_right_string(mod, builder));
  insert_divide_value_for_print(rhs, builder, args,
                                CmpInst::isSigned(icmp_pred));
  builder.CreateCall(print_format_fn, args);
  builder.CreateBr(endbb);

  builder.SetInsertPoint(endbb);
  builder.CreateRetVoid();

  return true;
}

bool inlineAssertCmpIR(llvm::Module &mod, bool DefaultOrdered) {
  IRBuilder<> builder(mod.getContext());

  bool changed = false;
  for (Function &func : mod) {
    if (generateAssertCmpBody(func, builder))
      changed = true;
  }

  for (Function &func : mod) {
    for (BasicBlock &block : func) {
      for (Instruction &inst : block) {
        CallInst *call = dyn_cast<CallInst>(&inst);
        if (!call)
          continue;

        Function *callee_fn = call->getCalledFunction();
        if (!callee_fn)
          continue;

        llvm::CmpInst::Predicate ICmpPred, FCmpPred;
        if (parseAssertCmpFunctionPredicate(*callee_fn, ICmpPred, FCmpPred,
                                            DefaultOrdered)) {
          insertAssertCallPrint(*call, builder);
          changed = true;
        }
      }
    }
  }

  return changed;
}
} // namespace llva

static const unsigned WordSizeInBits = 64;

static Value *insert_compare(CmpInst::Predicate ipred, CmpInst::Predicate fpred,
                             Value *lhs, Value *rhs, IRBuilder<> &builder,
                             raw_ostream &err);
static void insert_divide_value_for_print(Value *val, IRBuilder<> &builder,
                                          std::vector<Value *> &dsts);

/// Helper functions
static void get_format_string_impl(Type *type, raw_ostream &os);

static unsigned NumGlobalNames = 0;
static std::string get_global_tmp_name() {
  std::string Name = "llva.global.";
  Name += std::to_string(NumGlobalNames++);
  return Name;
}
static unsigned NumLocalNames = 0;
static std::string get_local_tmp_name() {
  std::string Name = "llva.tmp.";
  Name += std::to_string(NumLocalNames++);
  return Name;
}
static void clear_local_tmp_names() { NumLocalNames = 0; }

static Value *insert_compare(CmpInst::Predicate icmp_pred,
                             CmpInst::Predicate fcmp_pred, Value *lhs,
                             Value *rhs, IRBuilder<> &builder,
                             raw_ostream &err) {
  if (lhs->getType() != rhs->getType())
    return nullptr;

  Type *type = lhs->getType();

  if (type->isAggregateType()) {
    Value *result = nullptr;
    unsigned NumElts = type->isStructTy() ? type->getStructNumElements()
                                          : type->getArrayNumElements();

    for (unsigned I = 0; I != NumElts; ++I) {
      Value *lhs_elt = builder.CreateExtractValue(lhs, I, get_local_tmp_name());
      Value *rhs_elt = builder.CreateExtractValue(rhs, I, get_local_tmp_name());
      Value *cmp =
          insert_compare(icmp_pred, fcmp_pred, lhs_elt, rhs_elt, builder, err);
      if (!cmp)
        return nullptr;
      result = I == 0 ? cmp
                      : builder.CreateSelect(
                            result, cmp,
                            ConstantInt::get(result->getType(), 0, false),
                            get_local_tmp_name());
    }
    return result;
  } else if (type->isVectorTy()) {
    Type *elt_type = cast<VectorType>(type)->getElementType();
    if (elt_type->isIntegerTy() || elt_type->isPointerTy()) {
      if (!CmpInst::isIntPredicate(icmp_pred))
        return nullptr;
      return builder.CreateICmp(icmp_pred, lhs, rhs, get_local_tmp_name());
    } else if (elt_type->isFloatingPointTy()) {
      if (!CmpInst::isFPPredicate(fcmp_pred))
        return nullptr;
      return builder.CreateFCmp(fcmp_pred, lhs, rhs, get_local_tmp_name());
    }
  } else if (type->isIntegerTy() || type->isPointerTy()) {
    if (!CmpInst::isIntPredicate(icmp_pred))
      return nullptr;
    return builder.CreateICmp(icmp_pred, lhs, rhs, get_local_tmp_name());
  } else if (type->isFloatingPointTy()) {
    if (!CmpInst::isFPPredicate(fcmp_pred))
      return nullptr;
    return builder.CreateFCmp(fcmp_pred, lhs, rhs, get_local_tmp_name());
  }

  return nullptr;
}

static void insert_divide_value_for_print(Value *val, IRBuilder<> &builder,
                                          std::vector<Value *> &dsts,
                                          bool is_signed) {
  Type *type = val->getType();
  LLVMContext &ctx = type->getContext();

  if (auto *int_type = dyn_cast<IntegerType>(type)) {
    unsigned width = int_type->getBitWidth();
    Type *i64 = Type::getInt64Ty(ctx);

    if (width > WordSizeInBits) {
      unsigned num_words = divideCeil(width, WordSizeInBits);
      Type *wide_type = Type::getIntNTy(ctx, num_words * WordSizeInBits);
      Value *ext = is_signed ? builder.CreateSExt(val, wide_type)
                             : builder.CreateZExt(val, wide_type);

      for (unsigned i = 0; i != num_words; ++i) {
        Value *amt =
            ConstantInt::get(wide_type, WordSizeInBits * (num_words - i - 1));
        Value *part = i != num_words - 1 ? builder.CreateLShr(ext, amt) : ext;
        part = builder.CreateTrunc(part, i64);
        dsts.push_back(part);
      }
    } else if (width == WordSizeInBits) {
      dsts.push_back(val);
    } else {
      Value *ext = is_signed ? builder.CreateSExt(val, i64)
                             : builder.CreateZExt(val, i64);
      dsts.push_back(ext);
    }
  } else if (type->isFloatingPointTy()) {
    dsts.push_back(val);
  } else if (type->isPointerTy()) {
    dsts.push_back(val);
  } else if (auto *vec_type = dyn_cast<VectorType>(type)) {
#if LLVM_VERSION_MAJOR > 11
    unsigned num_elts = vec_type->getElementCount().getValue();
#else
    unsigned num_elts = vec_type->getNumElements();
#endif
    for (unsigned i = 0; i != num_elts; ++i) {
      Value *elt = builder.CreateExtractElement(val, i);
      dsts.push_back(elt);
    }
  } else if (auto *arr_type = dyn_cast<ArrayType>(type)) {
    for (unsigned i = 0, e = arr_type->getNumElements(); i != e; ++i) {
      Value *elt = builder.CreateExtractValue(val, i);
      dsts.push_back(elt);
    }
  } else if (auto *struct_type = dyn_cast<StructType>(type)) {
    for (unsigned i = 0, e = struct_type->getNumElements(); i != e; ++i) {
      Value *elt = builder.CreateExtractValue(val, i);
      dsts.push_back(elt);
    }
  } else {
    llvm_unreachable("unsupported type found");
  }
}

static GlobalValue *get_string_gv(StringRef str, StringRef name, Module &mod) {
  LLVMContext &ctx = mod.getContext();
  Constant *init = ConstantDataArray::getString(ctx, str);
  return cast<GlobalValue>(mod.getOrInsertGlobal(name, init->getType(), [&] {
    return new GlobalVariable(mod, init->getType(), true,
                              GlobalValue::PrivateLinkage, init, name, nullptr);
  }));
}

static GlobalValue *get_check_string(Module &mod) {
  return get_string_gv("llva: check %s\n", "llva.format_str.check", mod);
}
static GlobalValue *get_ng_string(Module &mod) {
  return get_string_gv("llva: assertion failed!\n", "llva.format_str.ng", mod);
}

static GlobalValue *get_left_string(Module &mod) {
  return get_string_gv("left", "llva.format_str.left", mod);
}
static GlobalValue *get_right_string(Module &mod) {
  return get_string_gv("right", "llva.format_str.right", mod);
}

static GlobalValue *get_format_string(Type *type, Module &mod) {
  static unsigned NumFormatStr = 0;
  static std::map<Type *, GlobalValue *> Map;

  auto itr = Map.find(type);
  if (itr != Map.end())
    return itr->second;

  auto &entry = Map[type];

  std::string buf;
  raw_string_ostream os(buf);
  os << "\t%5s: ";
  get_format_string_impl(type, os);
  os << '\n';

  std::string name = "llva.format_str.";
  name += std::to_string(NumFormatStr++);
  entry = get_string_gv(os.str(), name, mod);

  return entry;
}

static void get_format_string_impl(Type *type, raw_ostream &os) {
  if (auto *int_type = dyn_cast<IntegerType>(type)) {
    unsigned width = int_type->getBitWidth();
    unsigned num_words = divideCeil(width, WordSizeInBits);
    os << "0x";
    for (unsigned i = 0; i != num_words; ++i) {
      os << "%lx,";
    }
  } else if (type->isFloatingPointTy()) {
    if (type->isFloatTy())
      os << "%f";
    else if (type->isDoubleTy())
      os << "%lf";
    else if (type->isX86_FP80Ty())
      os << "%Lf";
    else if (type->isFP128Ty())
      os << "%Lf";

    llvm_unreachable("unsupported floating-point type");
  } else if (type->isPointerTy()) {
    os << "%p";
  } else if (auto *vec_type = dyn_cast<VectorType>(type)) {
    std::string EltStr;
    raw_string_ostream elt_os(EltStr);
    get_format_string_impl(vec_type->getElementType(), elt_os);
    elt_os.flush();

    os << "<";

#if LLVM_VERSION_MAJOR > 11
    unsigned num_elts = vec_type->getElementCount().getValue();
#else
    unsigned num_elts = vec_type->getNumElements();
#endif
    for (unsigned i = 0; i != num_elts; ++i) {
      os << EltStr;
      if (i != num_elts - 1)
        os << ", ";
    }
    os << ">";
  } else if (auto *arr_type = dyn_cast<ArrayType>(type)) {
    std::string elt_str;
    raw_string_ostream elt_os(elt_str);
    get_format_string_impl(arr_type->getElementType(), elt_os);
    elt_os.flush();

    os << "[";
    for (unsigned i = 0, e = arr_type->getNumElements(); i != e; ++i) {
      os << elt_str;
      if (i != e - 1)
        os << ", ";
    }
    os << "]";
  } else if (auto *struct_type = dyn_cast<StructType>(type)) {
    std::string elt_str;
    raw_string_ostream elt_os(elt_str);

    if (struct_type->isPacked())
      os << "<";
    os << "{";

    for (unsigned i = 0, e = struct_type->getNumElements(); i != e; ++i) {
      elt_str.clear();
      get_format_string_impl(struct_type->getElementType(i), elt_os);
      os << elt_os.str();
      if (i != e - 1)
        os << ", ";
    }

    os << "}";
    if (struct_type->isPacked())
      os << ">";
  } else {
    llvm_unreachable("unsupported type found");
  }
}

static FunctionCallee get_print_format(Module &mod) {
  static const char *name = "printf";

  LLVMContext &ctx = mod.getContext();
  FunctionType *fn_type = FunctionType::get(
      Type::getInt32Ty(ctx), {Type::getInt8Ty(ctx)->getPointerTo()}, true);
  return mod.getOrInsertFunction(name, fn_type);
}
