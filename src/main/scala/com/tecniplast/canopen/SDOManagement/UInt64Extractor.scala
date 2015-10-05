package com.tecniplast.canopen.SDOManagement

import java.io.{DataInput, ByteArrayInputStream}
import java.nio.ByteBuffer

import com.tecniplast.canopen.SDOManagement.SDO.SDORequestResponseOK
import com.tecniplast.canopen._

/**
 * Created by fbignardi on 5/19/15.
 */
trait UInt64Extractor {
  me: SDORequestResponseOK =>

  def getUInt64Value: Long =
    java.lang.Long.reverseBytes(ByteBuffer.wrap(me.getValue).getLong)

}
