package com.tecniplast

package object canopen {

  implicit def +- (x: Int): Byte =
    if (x>Byte.MaxValue.toInt || x<=Byte.MinValue.toInt) {
      ((x & 0x7F) * (-(x >> 7))).toByte
    } else x.toByte
    
  implicit def +- (x: Long): Byte =
    if (x>Byte.MaxValue.toLong || x<=Byte.MinValue.toLong) {
      ((x & 0x7F) * (-(x >> 7))).toByte
   } else x.toByte
    
  implicit def -+ (x: Byte): Int =
    if (x < 0)
      (x & 0xFF)
    else x.toInt
    
  def get2DigitsHex(b: Byte): String = {
    val s = (-+(b).toHexString).toUpperCase
    if (s.length > 1)
      "0x"+s
    else
      "0x0"+s
  }
  
  def get4DigitsHex(x: Long): String = {
    val s = (x.toHexString).toUpperCase
    if (s.length > 3)
      "0x"+s
    else if (s.length == 3)
      "0x0"+s
    else if (s.length == 2)
      "0x00"+s
    else if (s.length == 1)
      "0x000"+s
    else
      "0x0000"+s
  }

}
