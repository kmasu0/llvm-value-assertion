LLVM Value Assertion - llva
---------------------------

This is a tool to make it easy to write assertion code of LLVM IR value.

Sample
******

.. code:: console

   $ cat sample.ll
   declare void llvc.assert.eq.i32(i32, i32)

   define dso_local void @func(i32 %0) {
   bb0:
     call void llvc.assert.eq.i32(i32 %0, i32 10)
     ret void
   }

   define i32 main(void) {
      call void func(i32 9)
      ret i32 0
   }

   $ llva -o sample.2.ll sample.ll
   $ clang -o sample sample.2.ll
   $ ./sample
   llva: check   call void @llva.assert.eq.i32(i32 %0, i32 10)
   llva: check   call void @llva.assert.eq.i32(i32 %0, i32 9)
   llva: assertion failed!
     left: 0xa
     right: 0x9

Assert Functions
^^^^^^^^^^^^^^^^

**void llvc.assert.eq.<type>(<type> %lhs, <type> %rhs)**

**void llvc.assert.ne.<type>(<type> %lhs, <type> %rhs)**

**void llvc.assert.<icmp predicate>.<integer type>(<integer type> %lhs, <integer type> %rhs)**

**void llvc.assert.<fcmp predicate>.<floaing-point type>(<floaing-point type> %lhs, <floaing-point type> %rhs)**

