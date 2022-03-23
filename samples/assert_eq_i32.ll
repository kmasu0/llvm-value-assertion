declare void @llva.assert.eq.i32(i32, i32)

define void @func(i32 %0) {
  call void @llva.assert.eq.i32(i32 %0, i32 10)
  call void @llva.assert.eq.i32(i32 %0, i32 9)
  ret void
}

define void @main() {
  call void @func(i32 10);
  ret void
}
