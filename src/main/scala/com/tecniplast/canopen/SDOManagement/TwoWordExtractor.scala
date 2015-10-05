package com.tecniplast.canopen.SDOManagement

import com.tecniplast.canopen.SDOManagement.SDO.SDORequestResponseOK
import com.tecniplast.canopen._

/**
* Created by fbignardi on 3/23/15.
*/
trait TwoWordExtractor {
  self: SDORequestResponseOK =>
    def getValue2 =
      try {
        (-+(getValue(3)) << 8) + -+(getValue(2))
      } catch {
      case _ : Throwable => 0x00
    }
    def getOptValue2 =
      try {
        Some((-+(getValue(3)) << 8) + -+(getValue(2)))
      } catch {
      case _ : Throwable => None
    }
    def getValue1 =
      try {
        (-+(getValue(1)) << 8) + -+(getValue(0))
      } catch {
      case _ : Throwable => 0x00
    }
    def getOptValue1 =
      try {
        Some(((-+(getValue(1))) << 8) + -+(getValue(0)))
      } catch {
      case _ : Throwable => None
    }
}
