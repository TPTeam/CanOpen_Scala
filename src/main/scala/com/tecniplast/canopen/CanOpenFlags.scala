package com.tecniplast.canopen

object CanOpenFlags {
  
  final val MSGTYPE_STATUS		= 0x80     // used to mark a status TPCANMsg --> NOT IMPLEMENTED IN LIBRARY
  final val MSGTYPE_EXTENDED    = 0x02     // declares a extended frame
  final val MSGTYPE_RTR         = 0x01     // marks a remote frame
  final val MSGTYPE_STANDARD    = 0x00     // marks a standard frame
  
}