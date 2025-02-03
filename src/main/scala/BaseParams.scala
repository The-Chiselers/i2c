package tech.rocksavage.chiselware.I2C

import chisel3._
import chisel3.util._


case class BaseParams(
    dataWidth: Int = 8,
    addrWidth: Int = 8,
    regWidth: Int = 8,
    clkFreq: Int = 50,
    coverage: Boolean = true,
    testMode:   Boolean = false
)
