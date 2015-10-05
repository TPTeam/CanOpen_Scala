package com.tecniplast.canopen.SDOManagement

import com.tecniplast.canopen.SDOManagement.SDO.SDORequestResponseOK

/**
* Created by fbignardi on 3/23/15.
*/
trait SingleBooleanExtractor {
  self: SDORequestResponseOK =>
    def getStatus: Boolean =
      try {
        ((getValue(0) & 0x01) > 0)
      } catch {
      case _ : Throwable => false
    }
}
