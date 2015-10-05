package com.tecniplast.nativeInterface

import com.sun.jna._
import com.typesafe.config.ConfigFactory

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
  //Viene caricata solo da qui...non capisco perchè...
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
    
    final val BITRATE_1M = "1M"
    final val BITRATE_800K = "800K"
    final val BITRATE_500K = "500K"
    final val BITRATE_250K = "250K"
    final val BITRATE_125K = "125K"
    
    final val CAN_BAUD_1M 		=   0x0014  //   1 MBit/s
    final val CAN_BAUD_800K   =   0x0016  // 800 kBit/s
	  final val CAN_BAUD_500K  	= 	0x001C  // 500 kBit/s
	  final val CAN_BAUD_250K		=   0x011C  // 250 kBit/s
	  final val CAN_BAUD_125K  	= 	0x031C  // 125 kBit/s
    
	val conf_bitrate = 
    try {
    	ConfigFactory.load("canopen.conf").getString("bitrate")
    } catch {
      case _: Throwable => ""
    }
    
    def getBitRate =  
      conf_bitrate match {
      	case BITRATE_1M => CAN_BAUD_1M
        case BITRATE_800K => CAN_BAUD_800K
      	case BITRATE_500K => CAN_BAUD_500K 
      	case BITRATE_250K => CAN_BAUD_250K 
      	case BITRATE_125K => CAN_BAUD_125K 
      	case any =>
      	  {
      	    println("NO Bitrate loaded, setted to 1M.")
      	    CAN_BAUD_1M 
      	  }
    }
    
      /*
	S0 Setup 10Kbit
	S1 Setup 20Kbit
	S2 Setup 50Kbit 
	S3 Setup 100Kbit 
	S4 Setup 125Kbit  
	S5 Setup 250Kbit  
	S6 Setup 500Kbit 
	S7 Setup 800Kbit 
	S8 Setup 1Mbit 
   */
    
    def getBitRateToInt = 
      conf_bitrate match{
      	case BITRATE_1M => 8
        case BITRATE_800K => 7
      	case BITRATE_500K => 6
      	case BITRATE_250K => 5
      	case BITRATE_125K => 4
      	case any => 8
    }
    
    
	
  }

}