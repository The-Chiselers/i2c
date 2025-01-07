package tech.rocksavage.chiselware.SPI

import chisel3._
import chisel3.util._

case class BaseParams(
    dataWidth: Int = 8,
    addrWidth: Int = 8,
    regWidth: Int = 8,
    coverage: Boolean = false
) {}
