LLVM Value Assertion - llva
---------------------------

This is a tool to make it easy to write tests of LLVM IR assembly.

**Assert Inliner**

   Inserting value assertion codes.

   .. code::

      declare void llvc.assert.eq.i32(i32, i32)

      define dso_local void @func(i32 %0) {
      bb0:
        ...
        call void llvc.assert.eq.i32(i32 %0, i32 10)
        ...
      }

   A function `llvc.assert.eq.i32` body is generated after `AssertInliner`.

