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
      (x.toInt.abs + 128)
    else x.toInt

}