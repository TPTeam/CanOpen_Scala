package com.tecniplast.canopen.SDOManagement

import java.nio.ByteBuffer

import com.tecniplast.canopen.SDOManagement.SDO.SDORequestResponseOK
import com.tecniplast.canopen._

/**
 * Created by fbignardi on 5/19/15.
 */
trait UInt32Extractor extends TwoWordExtractor{
  me: SDORequestResponseOK =>

  def getUInt32Value =  //(getValue2 << 16 ) + (getValue1 & 0x0000FFFF)
    java.lang.Integer.reverseBytes(ByteBuffer.wrap(me.getValue).getInt)

  def getUInt32HexString =
    get4DigitsHex(getValue2.toLong).concat(get4DigitsHex(getValue1.toLong).drop(2))

}
