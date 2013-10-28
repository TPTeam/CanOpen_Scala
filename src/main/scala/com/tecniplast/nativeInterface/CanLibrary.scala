package com.tecniplast.nativeInterface

import com.sun.jna._

trait C_Can_Bridge extends Library {

  def initLibrary()

  def openPort(port: Int, bitrate: Int, function: msgReceived): Int

  def closePort(port: Int): Int

  def echoPort(port: Int, enable: Boolean): Int

  def writeMsg(port: Int, id: Long, dlc: Int, flags: Int, msg: Pointer): Int

}

/*
 * Scala callback when a message is received
 */
trait msgReceived extends Callback {
        def invoke(port: Int,
                   id: Long,
                   dlc: Int,
                   flags: Int,
                   msg: Pointer)
}

object CanLibrary {
  System.setProperty("jna.library.path","./")
  
  val library: C_Can_Bridge =
	  Native.synchronizedLibrary(
	      Native.loadLibrary(
   	  		  "C-Can-Bridge",
   			  classOf[C_Can_Bridge]) match {
   		case lib: C_Can_Bridge => lib
  	  }) match {
    	case lib: C_Can_Bridge => lib
  	  }

  library.initLibrary()

  object BitRate {
    final val CAN_BAUD_1M 		=   0x0014  //   1 MBit/s
	final val CAN_BAUD_500K   	=	0x001C  // 500 kBit/s
	final val CAN_BAUD_250K		=   0x011C  // 250 kBit/s
	final val CAN_BAUD_125K   	= 	0x031C  // 125 kBit/s
  }

}