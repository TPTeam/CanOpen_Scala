package com.tecniplast.canopen.SDOManagement

import com.tecniplast.canopen.SDOManagement.SDO.SDORequestResponseOK
import com.tecniplast.canopen._

/**
* Created by fbignardi on 3/23/15.
*/
trait OneWordExtractor {
  self: SDORequestResponseOK =>
    def getValue1 =
      try {
        (-+(getValue(1)) << 8) + -+(getValue(0))
      } catch {
      case _ : Throwable => 0x00
    }
    def getOptValue1 =
      try {
        Some((-+(getValue(1)) << 8) + -+(getValue(0)))
      } catch {
      case _ : Throwable => None
    }
}
