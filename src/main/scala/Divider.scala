package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode.{AddrDecode, AddrDecodeError, AddrDecodeParams}
import tech.rocksavage.chiselware.addressable.RegisterMap


// A 32-bit unsigned integer divider with a simple iterative algorithm.
// You supply (numerator, denominator, start=1) and wait until valid=1.
class Divider extends Module {
  val io = IO(new Bundle {
    val numerator   = Input(UInt(32.W))  // Dividend
    val denominator = Input(UInt(32.W))  // Divisor
    val start       = Input(Bool())      // Start signal
    val result      = Output(UInt(32.W)) // Quotient
    val remainder   = Output(UInt(32.W)) // Remainder
    val valid       = Output(Bool())     // High => 'result' is valid
  })

  // Internal registers
  val quotient   = RegInit(0.U(32.W))
  val remainder  = RegInit(0.U(32.W))
  val counter    = RegInit(0.U(log2Ceil(32 + 1).W))  // up to 32
  val busy       = RegInit(false.B)

  // Default outputs
  io.result    := quotient
  io.remainder := remainder
  io.valid     := !busy

  // Start condition
  when(io.start && !busy) {
    quotient  := 0.U
    remainder := io.numerator
    counter   := 32.U   // do up to 32 iterations
    busy      := true.B
  }

  // Iterative division
  when(busy) {
    when(counter > 0.U) {
      // Shift comparator:
      val shiftedDenom = io.denominator << (counter - 1.U)
      // If remainder >= shiftedDenominator, subtract and set that bit of quotient
      when(remainder >= shiftedDenom) {
        remainder := remainder - shiftedDenom
        quotient  := quotient  | (1.U << (counter - 1.U))
      }
      counter := counter - 1.U
    }
    .otherwise {
      // Done
      busy := false.B
    }
  }
}