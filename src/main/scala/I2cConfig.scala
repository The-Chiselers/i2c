package tech.rocksavage.chiselware.I2C

import tech.rocksavage.chiselware.I2C.BaseParams
import tech.rocksavage.traits.ModuleConfig

class I2cConfig extends ModuleConfig {
  override def getDefaultConfigs: Map[String, Any] = Map(
    "8_8_8" -> Seq(
      BaseParams(
        dataWidth = 8,
        addrWidth = 8,
        regWidth = 8,
        wordWidth = 8,
        clkFreq = 50
      ),
      false
    )
  )
}