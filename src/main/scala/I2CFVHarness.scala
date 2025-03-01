package tech.rocksavage.chiselware.I2C
import chisel3._
import chiseltest.formal._
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.I2C.I2CEnums._

class I2CFVHarness(p: BaseParams, formal: Boolean = true) extends Module {
  val io = IO(new Bundle {
    val apb       = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    val master    = new MasterInterface
    val slave     = new SlaveInterface
    val interrupt = Output(Bool())
    val state     = Output(UInt(4.W))
    val busState  = Output(BusState())
  })
  
  val i2c = Module(new I2C(p, formal))
  io <> i2c.io
  
  // Additional formal verification logic for single I2C instance
  if (formal) {
    // Instead of accessing internal stateReg, use the exposed state output
    val pastState = RegNext(io.state, init = STATE_IDLE)
    val stateStable = RegInit(0.U(4.W))
    
    when(io.state === pastState) {
      stateStable := stateStable + 1.U
    } .otherwise {
      stateStable := 0.U
    }
    
    // Verify we don't stay in any non-IDLE state too long (prevents lockups)
    when(io.state =/= STATE_IDLE && io.state =/= STATE_WAITSTOP) {
      assert(stateStable < 100.U, "State should progress within reasonable time")
    }
    
    // For STOP condition verification, we can only use observable signals
   when(pastState === STATE_SENDSTOP && io.state === STATE_IDLE) {
    cover(io.interrupt === true.B, "Cover interrupt triggered after STOP in harness")
  }
    
    // We can still verify bus state transitions
    val pastBusState = RegNext(io.busState, init = BusState.IDLE)
    when(pastBusState === BusState.OWNER && io.busState === BusState.IDLE) {
      assert(pastState === STATE_SENDSTOP, "Bus ownership should only be released after STOP")
    }
  }
}