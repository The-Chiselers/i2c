package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._
import tech.rocksavage.chiselware.apb.{ApbBundle, ApbParams}
import tech.rocksavage.chiselware.addrdecode.{AddrDecode, AddrDecodeError, AddrDecodeParams}
import tech.rocksavage.chiselware.addressable.RegisterMap

class I2C(p: BaseParams) extends Module {
  val io = IO(new Bundle {
    val apb = new ApbBundle(ApbParams(p.dataWidth, p.addrWidth))
    val master = new MasterInterface
    val slave = new SlaveInterface
    val interrupt = Output(Bool())  // Interrupt
  })

  // ------------------------------------------------------------------------
  // Register Map
  // ------------------------------------------------------------------------
  val registerMap = new RegisterMap(p.regWidth, p.addrWidth)

  //Internal Flags
  val maddrFlag = RegInit(false.B)
  
  // Shift Registers
  val i2cShift = RegInit(0.U(p.dataWidth.W))
  val addrShift = RegInit(0.U(8.W))
  val shiftCounter = RegInit(0.U((log2Ceil(p.dataWidth) + 1).W))
  val frameCounter = RegInit(0.U((5.W)))

  // Master registers
  val mctrla  = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mctrla,  "mctrla")

  val mctrlb  = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mctrlb,  "mctrlb")

  val mstatus = RegInit(0.U(p.regWidth.W))  // bits for ARBLOST, BUSERR
  registerMap.createAddressableRegister(mstatus, "mstatus")

  val mbaud   = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(mbaud,   "mbaud")

  val maddr   = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(maddr,   "maddr")

  val mdata   = RegInit(0.U(p.dataWidth.W))
  registerMap.createAddressableRegister(mdata,   "mdata")

  // Slave registers
  val sctrla    = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(sctrla,    "sctrla")

  val sctrlb    = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(sctrlb,    "scrtlb")

  val sstatus   = RegInit(0.U(p.regWidth.W)) // bits for COLL, BUSERR, etc.
  registerMap.createAddressableRegister(sstatus,   "sstatus")

  val saddr     = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(saddr,     "saddr")

  val sdata     = RegInit(0.U(p.dataWidth.W))
  registerMap.createAddressableRegister(sdata,     "sdata")

  val saddrmask = RegInit(0.U(p.regWidth.W))
  registerMap.createAddressableRegister(saddrmask, "saddrmask")

  // ------------------------------------------------------------------------
  // AddrDecode Integration
  // ------------------------------------------------------------------------
  val addrDecodeParams = registerMap.getAddrDecodeParams
  val addrDecode       = Module(new AddrDecode(addrDecodeParams))
  addrDecode.io.addr       := io.apb.PADDR
  addrDecode.io.addrOffset := 0.U
  addrDecode.io.en         := true.B
  addrDecode.io.selInput   := true.B

  // ------------------------------------------------------------------------
  // APB Interface
  // ------------------------------------------------------------------------
  io.apb.PREADY  := (io.apb.PENABLE && io.apb.PSEL)
  io.apb.PSLVERR := (addrDecode.io.errorCode === AddrDecodeError.AddressOutOfRange)
  io.apb.PRDATA  := 0.U

  when(io.apb.PSEL && io.apb.PENABLE) {
    when(io.apb.PWRITE) {
      for (reg <- registerMap.getRegisters) {
        when(addrDecode.io.sel(reg.id)) {
          when((reg.name == "mdata").B){
            i2cShift := io.apb.PWDATA
          }.otherwise{
            reg.writeCallback(addrDecode.io.addrOffset, io.apb.PWDATA)
          }
          when((reg.name == "maddr").B){
            maddrFlag := true.B
          }
        }
      }
    } .otherwise {
      for (reg <- registerMap.getRegisters) {
        when(addrDecode.io.sel(reg.id)) {
          when((reg.name == "mdata").B){
            io.apb.PRDATA := i2cShift
          }.otherwise {
            io.apb.PRDATA := reg.readCallback(addrDecode.io.addrOffset)
          }
        }
      }
    }
  }

  // ------------------------------------------------------------------------
  // State Machine Initilization
  // ------------------------------------------------------------------------  
  object State extends ChiselEnum {
    val idle, masterAddress, wait4Ack, masterWrite, masterRead, stop = Value
  }
  import State._
  val stateReg = RegInit(idle)

  // ------------------------------------------------------------------------
  // Divider Module Instantiation
  // ------------------------------------------------------------------------  
    val dividerFreq = Module(new Divider())
    dividerFreq.io.numerator := (p.clkFreq.U * 1000000.U) 
    dividerFreq.io.denominator := (10.U + 2.U * mbaud)
    dividerFreq.io.start := 0.U

    val dividerPer = Module(new Divider())
    dividerPer.io.numerator := 0.U
    dividerPer.io.denominator := 0.U
    dividerPer.io.start := 0.U

  // ------------------------------------------------------------------------
  // Baud Rate Generator (Master)
  // ------------------------------------------------------------------------
    val sclCounter    = RegInit(0.U(16.W))
    val sclReg        = RegInit(true.B)
    val prevClk       = RegInit(true.B)
    val sclPer        = RegInit(0.U(32.W))

    when(mctrla(0) === 1.U){
      dividerFreq.io.start := 1.U
      when(dividerFreq.io.valid){
        dividerPer.io.numerator := (p.clkFreq.U * 1000000.U) 
        dividerPer.io.denominator := dividerFreq.io.result
        dividerPer.io.start := 1.U
      }
      when(dividerPer.io.valid){
        sclPer := dividerPer.io.result
      }

      when(sclCounter === (sclPer - 1.U)) {
          sclCounter := 0.U
          sclReg := ~sclReg
      } .otherwise {
          sclCounter := sclCounter + 1.U
      }
      io.master.scl := sclReg
      
    }.otherwise {
      io.master.scl := false.B
      io.master.sdaOut := 0.U
    }
    
  // ------------------------------------------------------------------------
  // State Machine Operation
  // ------------------------------------------------------------------------  

    //Internal Regs
    val rwBit = RegInit(0.U(1.W))

    io.interrupt := 0.U //Temp
    io.slave.sdaOut := 0.U //Temp
    io.master.sdaOut := 1.U //Default Case

    switch(stateReg) {
      is(idle) {
          io.master.sdaOut := 1.U
          shiftCounter := 0.U
          frameCounter := 0.U
          when(mctrla(0) === 1.U) { // Master Mode
              sclReg := true.B
              when(maddrFlag){
                  io.master.sdaOut := 0.U //Start Condition - Drive SDA Low before SCL switches hi to low
                  maddrFlag := false.B
                  addrShift := maddr  //Load address to shift out to slave
                  stateReg := masterAddress //Go to address shift state
              }
          }
      }
      is(masterAddress){
          io.master.sdaOut := addrShift(p.dataWidth - 1)
          prevClk := sclReg
          when(~prevClk & io.master.scl){
              when(frameCounter < 8.U) {
                  addrShift := addrShift(p.dataWidth - 2, 0) ## 0.U
                  when(frameCounter === 7.U){
                      rwBit := addrShift(p.dataWidth - 1)
                  }
                frameCounter := frameCounter + 1.U
                stateReg := masterAddress
              }.otherwise {
                  stateReg := wait4Ack
              }
          }
      }
      is(wait4Ack){   //How do we know which slave sent the ACK?
          io.master.sdaOut := 1.U
          prevClk := sclReg   //What happens if we never get ACK?
          frameCounter := 0.U
          when(~prevClk & io.master.scl){
              when(io.master.sdaIn === 0.U) {     //How do we know slave only sent 1 bit ack?
                  when(rwBit === 1.U){
                      stateReg := masterWrite
                  }.otherwise{
                      stateReg := masterRead
                  }
              }
          }
      }
      is(masterWrite){
        io.master.sdaOut := i2cShift(p.dataWidth - 1)
        prevClk := sclReg
        when(~prevClk & io.master.scl) {
          when(frameCounter < 8.U){
            i2cShift := i2cShift(p.dataWidth - 2, 0) ## 0.U
            frameCounter := frameCounter + 1.U
            shiftCounter := shiftCounter + 1.U
            stateReg := masterWrite
          }.otherwise{
            when(shiftCounter === p.dataWidth.U){
              sclReg := true.B
              stateReg := stop
            }.otherwise {
              stateReg := wait4Ack
            }
          }
        }
      }
      is(masterRead){
        io.master.sdaOut := 1.U
        prevClk := sclReg
        when(~prevClk & io.master.scl) {
          when(frameCounter < 8.U){
            i2cShift := i2cShift(p.dataWidth - 2, 0) ## io.master.sdaIn
            frameCounter := frameCounter + 1.U
            shiftCounter := shiftCounter + 1.U
            stateReg := masterRead
          }.otherwise{
            when(shiftCounter === p.dataWidth.U){
              sclReg := true.B
              stateReg := stop
            }.otherwise {
              stateReg := wait4Ack
            }
          }
        }          
      }
      is(stop){
          io.master.sdaOut := 1.U
          sclReg := true.B
          stateReg := idle
      }
    }
}