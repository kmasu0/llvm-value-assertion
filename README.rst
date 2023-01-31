LLVM Value Assertion - llva
===========================

This is a tool to make it easy to write assertion code of LLVM IR value.

Example
-------

Inline assert functions.::

   $ cat sample.ll
   declare void llva.assert.eq.i32(i32, i32)

   define dso_local void @func(i32 %0) {
   bb0:
     call void llva.assert.eq.i32(i32 %0, i32 10)
     ret void
   }

   define i32 main(void) {
      call void func(i32 9)
      ret i32 0
   }

   $ llva -o sample.tmp.ll sample.ll
   $ clang -o sample sample.tmp.ll
   $ ./sample
   llva: check   call void @llva.assert.eq.i32(i32 %0, i32 10)
   llva: check   call void @llva.assert.eq.i32(i32 %0, i32 9)
   llva: assertion failed!
     left: 0xa
     right: 0x9

Generate test runner.::

  $ cat sample2.ll
  @x = dso_local global float 5.500000e+00, align 4
  @y = dso_local global float 0x4028333340000000, align 4

  define void @llva.run.1() {
    %lhs = load float, ptr @x, align 4
    %rhs = load float, ptr @y, align 4
    call void @llva.assert.oeq.float(float %lhs, float %rhs)
    ret void
  }

  define i32 @main(i32 %args, ptr %argv) {
    %res = call i32 @llva.runtest()
    ret i32 %res
  }

  declare void @llva.assert.oeq.float(float, float)
  declare i32 @llva.runtest()

  $ llva -o sample2.tmp.ll  sample2.ll
  $ clang -o sample2 sample2.tmp.ll
  $ ./sample2
  llva: run all tests...

Assert Functions
----------------

**void llva.assert.eq.<type>(<any type> %lhs, <any type> %rhs)**
  Apply `icmp.eq` and `fcmp.[ou]eq` to each element of `%lhs` and `%rhs` and assert each result.

**void llva.assert.ne.<type>(<any type> %lhs, <any type> %rhs)**
  Apply `icmp.ne` and `fcmp.[ou]ne` to each element of `%lhs` and `%rhs` and assert each result.

**void llva.assert.<icmp predicate>.<integer type>(<integer type> %lhs, <integer type> %rhs)**
  Apply `icmp.<predicate>` to `%lhs` and `%rhs` and assert its result.

**void llva.assert.<fcmp predicate>.<floaing-point type>(<floaing-point type> %lhs, <floaing-point type> %rhs)**
  Apply `fcmp.<predicate>` to `%lhs` and `%rhs` and assert its result.

Runner Functions
----------------

**void llva.test.<any>()**
  Test function which will be ran from `llva.runtest`\ .

**i32 llva.runtest()**
  Invoke all tests `llva.test.<any>()` and return number of failurs.

**void llva.runtest.fail()**
  Increment number of test failed. `llva.assert` will call this function when option `--exit-on-fail` is not specified.

**i32 llva.runtest.result()**
  Return number of call `llva.runtest.fail`\ .
