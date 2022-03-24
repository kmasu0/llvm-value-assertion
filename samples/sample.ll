declare void @llva.assert.eq.i300(i300, i300)
declare void @llva.assert.ne.i300(i300, i300)
declare void @llva.assert.sgt.i300(i300, i300)
declare void @llva.assert.sge.i300(i300, i300)
declare void @llva.assert.slt.i300(i300, i300)
declare void @llva.assert.sle.i300(i300, i300)
declare void @llva.assert.ugt.i300(i300, i300)
declare void @llva.assert.uge.i300(i300, i300)
declare void @llva.assert.ult.i300(i300, i300)
declare void @llva.assert.ule.i300(i300, i300)

define void @func1(i300 %0) {
  call void @llva.assert.eq.i300(i300 %0, i300 -1)
  call void @llva.assert.eq.i300(i300 %0, i300 -2)
  call void @llva.assert.sgt.i300(i300 %0, i300 -2)
  call void @llva.assert.sge.i300(i300 %0, i300 -2)
  call void @llva.assert.slt.i300(i300 %0, i300 0)
  call void @llva.assert.sle.i300(i300 %0, i300 0)
  call void @llva.assert.ugt.i300(i300 %0, i300 0)
  call void @llva.assert.uge.i300(i300 %0, i300 0)
  call void @llva.assert.ult.i300(i300 0, i300 %0)
  call void @llva.assert.ule.i300(i300 0, i300 %0)
  ret void
}

define void @main() {
  call void @func1(i300 -1);
  ret void
}
