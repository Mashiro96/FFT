
package FFT

import chisel3._
import chisel3.experimental._
import chisel3.util._
import scala.math._

class FFT extends Module
  with HasDataConfig
  with HasElaborateConfig {
  val io = IO(new Bundle {
    val dIn = Input(new MyComplex())
    val din_valid = Input(new Bool())
    val dOut1 = Output(new MyComplex())
    val dOut2 = Output(new MyComplex())
    val dOut3 = Output(new MyComplex())
    val dOut4 = Output(new MyComplex())
    val dout_valid = Output(new Bool())
    val busy = Output(new Bool())
  })
  
  val stages = log2Ceil(FFTLength) / 2
  val cntNum = 2 * stages
  def sinTable(k: Int, n: Int): Vec[FixedPoint] = {
    val times = (0 until FFTLength / 4 by pow(4, k).toInt)
      .map(i => -(i * n * 2 * Pi) / FFTLength.toDouble)
    val inits = times.map(t => FixedPoint.fromDouble(sin(t), DataWidth.W, BinaryPoint.BP))
    VecInit(inits)
  }
  def cosTable(k: Int, n: Int): Vec[FixedPoint] = {
    val times = (0 until FFTLength /4 by pow(4, k).toInt)
      .map(i => -(i * n * 2 * Pi) / FFTLength.toDouble)
    val inits = times.map(t => FixedPoint.fromDouble(cos(t), DataWidth.W, BinaryPoint.BP))
    VecInit(inits)
  }
  def wnTable(k: Int, n: Int)(idx: UInt): MyComplex = {
    val res = Wire(new MyComplex())
    res.re := cosTable(k, n)(idx)
    res.im := sinTable(k, n)(idx)
    res
  }
  
  val cnt = RegInit(0.U((cntNum + 1).W))
  val busy = cnt =/= 0.U
  when(io.din_valid || busy){
    cnt := Mux(cnt === (FFTLength * 5 / 4 - 1).asUInt(), 0.U, cnt+1.U)
  }
  io.busy := busy
  
  val out1 = VecInit(Seq.fill(stages + 1)(0.S((2 * DataWidth).W).asTypeOf(new MyComplex)))
  val out2 = VecInit(Seq.fill(stages + 1)(0.S((2 * DataWidth).W).asTypeOf(new MyComplex)))
  val out3 = VecInit(Seq.fill(stages + 1)(0.S((2 * DataWidth).W).asTypeOf(new MyComplex)))
  val out4 = VecInit(Seq.fill(stages + 1)(0.S((2 * DataWidth).W).asTypeOf(new MyComplex)))
  out1(0) := io.dIn
  out2(0) := io.dIn
  out3(0) := io.dIn
  out4(0) := io.dIn
  
  for (i <- 0 until stages - 1) {
    val wnCtrl = cnt((cntNum - 3 - 2 * i),0)
    val wn2 = wnTable(i, 2)(wnCtrl)
    val wn3 = wnTable(i, 1)(wnCtrl)
    val wn4 = wnTable(i, 3)(wnCtrl)
    val BFM1234 = ButterflyMul(ShiftRegister(out1(i), (3 * FFTLength / pow(4 , i + 1)).toInt), ShiftRegister(out2(i), (2 * FFTLength / pow(4 , i + 1)).toInt), ShiftRegister(out3(i), (FFTLength / pow(4 , i + 1)).toInt), out4(i), wn2, wn3, wn4)
    val swCtrl1 = cnt(cntNum - 3 - 2 * i)
    val swCtrl0 = cnt(cntNum - 4 - 2 * i)
    val sw1234 = Switch(BFM1234._1, ShiftRegister(BFM1234._2, (FFTLength / pow(4 , i + 2)).toInt), ShiftRegister(BFM1234._3, (2 * FFTLength / pow(4 , i + 2)).toInt), ShiftRegister(BFM1234._4, (3 * FFTLength / pow(4 , i + 2)).toInt), swCtrl1, swCtrl0)
    out1(i + 1) := sw1234._1
    out2(i + 1) := sw1234._2
    out3(i + 1) := sw1234._3
    out4(i + 1) := sw1234._4
  }
  val out1D3 = ShiftRegister(out1(stages - 1), 3)
  val out2D2 = ShiftRegister(out2(stages - 1), 2)
  val out3D1 = ShiftRegister(out3(stages - 1), 1)
  val BFA1234 = ButterflyAdd(out1D3, out2D2, out3D1, out4(stages - 1))
  out1(stages) := BFA1234._1
  out2(stages) := BFA1234._2
  out3(stages) := BFA1234._3
  out4(stages) := BFA1234._4
  
  io.dOut1 := RegNext(out1(stages))
  io.dOut2 := RegNext(out2(stages))
  io.dOut3 := RegNext(out3(stages))
  io.dOut4 := RegNext(out4(stages))
  io.dout_valid := RegNext(cnt) === (FFTLength - 1).asUInt()
  
}



