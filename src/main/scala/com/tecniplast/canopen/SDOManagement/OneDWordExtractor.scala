package com.tecniplast.canopen.SDOManagement

import com.tecniplast.canopen.SDOManagement.SDO.SDORequestResponseOK
import com.tecniplast.canopen._

/**
* Created by fbignardi on 3/23/15.
*/
trait OneDWordExtractor {
  self: SDORequestResponseOK =>

    private val _getValue = (x: Array[Byte]) => {
      try {
        Some(
        (((-+(x(0))).toLong) 		& 0x000000FF) +
        (((-+(x(1))).toLong << 8)	& 0x0000FF00) +
        (((-+(x(2))).toLong << 16)	& 0x00FF0000) +
        (((-+(x(3))).toLong << 32)	& 0xFF000000)
        )
      } catch {
        case _ : Throwable => None
      }
      }

    def getValue1: Long =
      _getValue(getValue).getOrElse(0x00)

    def getOptValue1: Option[Long] =
      _getValue(getValue)
}
