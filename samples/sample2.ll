@x = dso_local global float 5.500000e+00, align 4
@y = dso_local global float 0x4028333340000000, align 4

define void @llva.test.1() {
  %lhs = load float, float* @x, align 4
  %rhs = load float, float* @y, align 4
  call void @llva.assert.olt.float(float %lhs, float %rhs)
  ret void
}

define i32 @main(i32 %args, i8** %argv) {
  %res = call i32 @llva.runtest()
  ret i32 %res
}

declare void @llva.assert.olt.float(float, float)
declare i32 @llva.runtest()
