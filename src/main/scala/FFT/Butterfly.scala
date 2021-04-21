
package FFT

import chisel3._
import chisel3.experimental._

class MyComplex extends Bundle
  with HasDataConfig {
  val re = FixedPoint(DataWidth.W, BinaryPoint.BP)
  val im = FixedPoint(DataWidth.W, BinaryPoint.BP)
}

class ComplexIO extends Bundle {
  val op1 = Input(new MyComplex())
  val op2 = Input(new MyComplex())
  val res = Output(new MyComplex())
}

class ComplexAdd extends Module {
  val io = IO(new ComplexIO())
  io.res.re := io.op1.re + io.op2.re
  io.res.im := io.op1.im + io.op2.im
}
object ComplexAdd {
  def apply(op1: MyComplex, op2: MyComplex): MyComplex = {
    val inst = Module(new ComplexAdd)
    inst.io.op1 := op1
    inst.io.op2 := op2
    inst.io.res
  }
}

class ComplexSub extends Module {
  val io = IO(new ComplexIO())
  io.res.re := io.op1.re - io.op2.re
  io.res.im := io.op1.im - io.op2.im
}
object ComplexSub {
  def apply(op1: MyComplex, op2: MyComplex): MyComplex = {
    val inst = Module(new ComplexSub)
    inst.io.op1 := op1
    inst.io.op2 := op2
    inst.io.res
  }
}

class ComplexTran extends Module {
  val io = IO(new Bundle {
    val in = Input(new MyComplex())
    val out = Output(new MyComplex())
  })
  io.out.re := io.in.im
  io.out.im := -io.in.re
}
object ComplexTran {
  def apply(in: MyComplex): MyComplex = {
    val inst = Module(new ComplexTran)
    inst.io.in := in
    inst.io.out
  }
}

class ComplexMul extends Module 
  with HasElaborateConfig{
  val io = IO(new ComplexIO())
  if (useGauss) {
    val k1 = io.op2.re * (io.op1.re + io.op1.im)
    val k2 = io.op1.re * (io.op2.im - io.op2.re)
    val k3 = io.op1.im * (io.op2.re + io.op2.im)
    io.res.re := k1 - k3
    io.res.im := k1 + k2
  } else {
    io.res.re := io.op1.re * io.op2.re - io.op1.im * io.op2.im
    io.res.im := io.op1.re * io.op2.im + io.op1.im * io.op2.re
  }
}
object ComplexMul {
  def apply(op1: MyComplex, op2: MyComplex): MyComplex = {
    val inst = Module(new ComplexMul)
    inst.io.op1 := op1
    inst.io.op2 := op2
    inst.io.res
  }
}

class ButterflyAddIO extends Bundle
  with HasDataConfig {
  val in1 = Input(new MyComplex())
  val in2 = Input(new MyComplex())
  val in3 = Input(new MyComplex())
  val in4 = Input(new MyComplex())
  val out1 = Output(new MyComplex())
  val out2 = Output(new MyComplex())
  val out3 = Output(new MyComplex())
  val out4 = Output(new MyComplex()) 
}

class ButterflyAdd extends Module {
  val io = IO(new ButterflyAddIO())
  val add1 = ComplexAdd(io.in1, io.in3)
  val add2 = ComplexAdd(io.in2, io.in4)
  val sub1 = ComplexSub(io.in1, io.in3)
  val sub2 = ComplexSub(io.in2, io.in4)
  val add3 = ComplexAdd(add1, add2)
  val sub3 = ComplexSub(add1, add2)
  val tran = ComplexTran(sub2)
  val add4 = ComplexAdd(sub1, tran)
  val sub4 = ComplexSub(sub1, tran)
  io.out1 := add3
  io.out2 := sub3
  io.out3 := add4
  io.out4 := sub4
}
object ButterflyAdd {
  def apply(in1: MyComplex, in2: MyComplex, in3: MyComplex, in4: MyComplex): (MyComplex, MyComplex, MyComplex, MyComplex) = {
  val inst = Module(new ButterflyAdd)
  inst.io.in1 := in1
  inst.io.in2 := in2
  inst.io.in3 := in3
  inst.io.in4 := in4
  (inst.io.out1, inst.io.out2, inst.io.out3, inst.io.out4)
  }
}

class ButterflyMulIO extends Bundle
  with HasDataConfig {
  val in1 = Input(new MyComplex())
  val in2 = Input(new MyComplex())
  val in3 = Input(new MyComplex())
  val in4 = Input(new MyComplex())
  val wn2 = Input(new MyComplex())
  val wn3 = Input(new MyComplex())
  val wn4 = Input(new MyComplex())
  val out1 = Output(new MyComplex())
  val out2 = Output(new MyComplex())
  val out3 = Output(new MyComplex())
  val out4 = Output(new MyComplex()) 
}

class ButterflyMul extends Module {
  val io = IO(new ButterflyMulIO())
  val add1 = ComplexAdd(io.in1, io.in3)
  val add2 = ComplexAdd(io.in2, io.in4)
  val sub1 = ComplexSub(io.in1, io.in3)
  val sub2 = ComplexSub(io.in2, io.in4)
  val add3 = ComplexAdd(add1, add2)
  val sub3 = ComplexSub(add1, add2)
  val tran = ComplexTran(sub2)
  val add4 = ComplexAdd(sub1, tran)
  val sub4 = ComplexSub(sub1, tran)
  val mul2 = ComplexMul(sub3, io.wn2)
  val mul3 = ComplexMul(add4, io.wn3)
  val mul4 = ComplexMul(sub4, io.wn4)
  io.out1 := add3
  io.out2 := mul2
  io.out3 := mul3
  io.out4 := mul4
}
object ButterflyMul {
  def apply(in1: MyComplex, in2: MyComplex, in3: MyComplex, in4: MyComplex, wn2: MyComplex, wn3: MyComplex, wn4: MyComplex): (MyComplex, MyComplex, MyComplex, MyComplex) = {
  val inst = Module(new ButterflyMul)
  inst.io.in1 := in1
  inst.io.in2 := in2
  inst.io.in3 := in3
  inst.io.in4 := in4
  inst.io.wn2 := wn2
  inst.io.wn3 := wn3
  inst.io.wn4 := wn4
  (inst.io.out1, inst.io.out2, inst.io.out3, inst.io.out4)
  }
}

class SwitchIO extends Bundle {
  val in1 = Input(new MyComplex())
  val in2 = Input(new MyComplex())
  val in3 = Input(new MyComplex())
  val in4 = Input(new MyComplex())
  val sel1 = Input(Bool())
  val sel0 = Input(Bool())
  val out1 = Output(new MyComplex())
  val out2 = Output(new MyComplex())
  val out3 = Output(new MyComplex())
  val out4 = Output(new MyComplex()) 
}

class Switch extends Module {
  val io = IO(new SwitchIO())
  io.out1 := io.in1
  io.out2 := io.in1
  io.out3 := io.in1
  io.out4 := io.in1
  when(!io.sel1 && !io.sel0) {
  io.out1 := io.in1
  io.out2 := io.in4
  io.out3 := io.in3
  io.out4 := io.in2
  }.elsewhen(!io.sel1 && io.sel0){
  io.out1 := io.in2
  io.out2 := io.in1
  io.out3 := io.in4
  io.out4 := io.in3
  }.elsewhen(io.sel1 && !io.sel0){
  io.out1 := io.in3
  io.out2 := io.in2
  io.out3 := io.in1
  io.out4 := io.in4
  }.elsewhen(io.sel1 && io.sel0){
  io.out1 := io.in4
  io.out2 := io.in3
  io.out3 := io.in2
  io.out4 := io.in1
  }
}
object Switch {
  def apply(in1: MyComplex, in2: MyComplex, in3: MyComplex, in4: MyComplex, sel1: Bool, sel0: Bool): (MyComplex, MyComplex, MyComplex, MyComplex) = {
  val inst = Module(new Switch)
  inst.io.in1 := in1
  inst.io.in2 := in2
  inst.io.in3 := in3
  inst.io.in4 := in4
  inst.io.sel1 := sel1
  inst.io.sel0 := sel0
  (inst.io.out1, inst.io.out2, inst.io.out3, inst.io.out4)
  }
}



