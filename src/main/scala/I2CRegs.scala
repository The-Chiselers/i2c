package tech.rocksavage.chiselware.SPI

import chisel3._
import chisel3.util._

class SPIRegs(p: BaseParams) extends Bundle {
  val REG_SIZE: Int = p.regWidth
  val CTRLA_SIZE: Int = p.regWidth
  val CTRLB_SIZE: Int = p.regWidth
  val INTCTRL_SIZE: Int = p.regWidth
  val INTFLAGS_SIZE: Int = p.regWidth
  val DATA_SIZE: Int = p.dataWidth

  // #####################################################################
  // REGS
  // #####################################################################
  val CTRLA = RegInit(0.U(CTRLA_SIZE.W))
  val CTRLB = RegInit(0.U(CTRLB_SIZE.W))
  val INTCTRL = RegInit(0.U(INTCTRL_SIZE.W))
  val INTFLAGS = RegInit(0.U(INTFLAGS_SIZE.W))
  //val DATA = RegInit(0.U(DATA_SIZE.W)) Not a physical register
  // #####################################################################

  // Should always be 8
  val CTRLA_ADDR: Int = 0
  val CTRLA_REG_SIZE: Int = (CTRLA_SIZE + REG_SIZE - 1) / REG_SIZE
  val CTRLA_ADDR_MAX: Int = CTRLA_ADDR + CTRLA_REG_SIZE - 1

  val CTRLB_ADDR: Int = CTRLA_ADDR_MAX + 1
  val CTRLB_REG_SIZE: Int = (CTRLB_SIZE + REG_SIZE - 1) / REG_SIZE
  val CTRLB_ADDR_MAX: Int = CTRLB_ADDR + CTRLB_REG_SIZE - 1

  val INTCTRL_ADDR: Int = CTRLB_ADDR_MAX + 1
  val INTCTRL_REG_SIZE: Int = (INTCTRL_SIZE + REG_SIZE - 1) / REG_SIZE
  val INTCTRL_ADDR_MAX: Int = INTCTRL_ADDR + INTCTRL_REG_SIZE - 1

  val INTFLAGS_ADDR: Int = INTCTRL_ADDR_MAX + 1
  val INTFLAGS_REG_SIZE: Int = (INTFLAGS_SIZE + REG_SIZE - 1) / REG_SIZE
  val INTFLAGS_ADDR_MAX: Int = INTFLAGS_ADDR + INTFLAGS_REG_SIZE - 1
  //

  // Can be 8, 16, 32
  val DATA_ADDR: Int = INTFLAGS_ADDR_MAX + 1
  val DATA_REG_SIZE: Int = (DATA_SIZE + REG_SIZE - 1) / REG_SIZE
  val DATA_ADDR_MAX: Int = DATA_ADDR + DATA_REG_SIZE - 1
}
