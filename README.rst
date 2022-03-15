LLVM Value Assertion - llva
---------------------------

This is a tool to transform input llvm-ir by inserting llvm ir value assertion codes.

.. code::

   declare void llvc.assert.eq.i32(i32, i32)

   define dso_local void @func(i32 %0) {
   bb0:
     ...
     call void llvc.assert.eq.i32(i32 %0, i32 10)
     ...
   }

llva creates a function `llvc.assert.eq.i32` body.
