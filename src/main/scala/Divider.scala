package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode.{AddrDecode, AddrDecodeError, AddrDecodeParams}
import tech.rocksavage.chiselware.addressable.RegisterMap

class Divider() extends Module {
  val io = IO(new Bundle {
    val numerator   = Input(UInt(32.W))   // Dividend
    val denominator = Input(UInt(32.W))  // Divisor
    val start       = Input(Bool())            // Start signal
    val result      = Output(UInt(32.W)) // Quotient
    val remainder   = Output(UInt(32.W)) // Remainder
    val valid       = Output(Bool())           // Valid signal to indicate result is ready
  })

  // Internal registers
  val quotient   = RegInit(0.U(32.W))
  val remainder  = RegInit(0.U(32.W))
  val counter    = RegInit(0.U(log2Ceil(32 + 1).W))
  val busy       = RegInit(false.B)
  val validReg   = RegInit(false.B) // Register to hold the valid signal

  // Wires for calculations
  val subtractResult = Wire(UInt(32.W))
  subtractResult := remainder - (io.denominator << (counter - 1.U))

  // Default outputs
  io.result := quotient
  io.remainder := remainder
  io.valid := !busy

  when(io.start && !busy) {
    // Start the division process
    quotient := 0.U
    remainder := io.numerator
    counter := 32.U
    busy := true.B
  }

  when(busy) {
    when(counter > 0.U) {
      // Perform division step
      val canSubtract = subtractResult(32 - 1) === 0.U // Check if subtraction is valid
      when(canSubtract) {
        remainder := subtractResult
        quotient := quotient | (1.U << (counter - 1.U)) // Set the corresponding bit in the quotient
      }
      counter := counter - 1.U
    }.otherwise {
      // Division is complete
      busy := false.B
    }
  }
}
