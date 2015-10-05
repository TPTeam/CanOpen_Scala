package com.tecniplast.canopen.SDOManagement

import com.tecniplast.canopen.SDOManagement.SDO.SDORequestResponseOK

/**
* Created by fbignardi on 3/23/15.
*/
trait ThreeBitsExtractor {
  self: SDORequestResponseOK =>
    def getStatus =
      try {
        (getValue(0) & 0x07)
      } catch {
      case _ : Throwable => 0x00
    }
}
