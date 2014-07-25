package com.tecniplast.canopen

import com.tecniplast.device.CanDevice._
import CanOpenFlags._
import CanOpenMsgType._
import scala.annotation.tailrec

object CanOpenMessages {
  
  abstract class SendCanOpenMessage(id: Long, msg: Array[Byte], flags: Int) {
    def toCan =
      CanMsgSend(id,msg,flags)
  }
  
  case class SendRawCanOpenMessage(id: Long, msg: Array[Byte], flags: Int) 
  	extends SendCanOpenMessage(id,msg,flags) 
  
  case class SendSyncCanOpenMessage() 
  	extends SendCanOpenMessage(
  	    EMERGENCY_OR_SYNC,
  	    Array(),
  	    MSGTYPE_STANDARD)
  
  case class SendNMTCMDCanOpenMessage(address: Int,command: Int) 
  	extends SendCanOpenMessage(
  	    NMT_CMD,
  	    Array(command, address),
  	    MSGTYPE_STANDARD)
  case class SendNMTSTATUREQCanOpenMessage(address: Int) 
  	extends SendCanOpenMessage(
  	    CanOpenIdNMT(address).cobid.getId,
  	    Array(),
  	    MSGTYPE_STANDARD)
   
  case class SendTPDOCanOpenMessage(number: Int, address: Int, value: Array[Byte]) 
  	extends SendCanOpenMessage(
  	    CanOpenIdTPDO(number,address).cobid.getId,
  	    value,
  	    MSGTYPE_STANDARD)
   
  case class SendTPDORequestCanOpenMessage(number: Int, address: Int) 
  	extends SendCanOpenMessage(
  	    CanOpenIdTPDO(number,address).cobid.getId,
  	    Array(),
  	    MSGTYPE_RTR)
  
  case class SendRPDOCanOpenMessage(number: Int, address: Int, value: Array[Byte]) 
  	extends SendCanOpenMessage(
  	    CanOpenIdRPDO(number,address).cobid.getId,
  	    value,
  	    MSGTYPE_STANDARD)
  
  case class SendRPDORequestCanOpenMessage(number: Int, address: Int) 
  	extends SendCanOpenMessage(
  	    CanOpenIdRPDO(number,address).cobid.getId,
  	    Array(),
  	    MSGTYPE_RTR)
  
  final val fillWith: Byte = 0x00
  private def stdSize(value: Array[Byte]): Array[Byte] =
    stdSize(value,Array())
  @tailrec
  private def stdSize(value: Array[Byte], partial: Array[Byte]): Array[Byte] =
    if (partial.size >= 8) partial
    else if (value.headOption.isDefined) stdSize(value.tail,partial :+ value.head)
    else stdSize(Array(),partial :+ fillWith)
      
  
  case class SendTSSDOCanOpenMessage(address: Int, value: Array[Byte]) 
  	extends SendCanOpenMessage(
  	    CanOpenIdTSSDO(address).cobid.getId,
  	    stdSize(value),
  	    MSGTYPE_STANDARD)
  
  case class SendRCSDOCanOpenMessage(address: Int, value: Array[Byte]) 
  	extends SendCanOpenMessage(
  	    CanOpenIdRCSDO(address).cobid.getId,
  	    stdSize(value),
  	    MSGTYPE_STANDARD)
  
  case class SendTimeStampCanOpenMessage(timestamp: Long) 
  	extends SendCanOpenMessage(
  	    CanOpenTimeStamp.cobid.getId,
  	    Array(
  	    ((timestamp >> 24) & 0xFF),
  	    ((timestamp >> 16) & 0xFF),
  	    ((timestamp >> 8 ) & 0xFF),
  	    ((timestamp		 ) & 0xFF)
  	    ),
  	    MSGTYPE_STANDARD)
  
  abstract class ReceivedCanOpenMessage(function: Int,address: Int, message: Array[Byte]) {
    	def getAddress = address
    	def getMessage = message
    	def getFunction = function
  }
  
  /* TPDO Messages */
  trait ReceivedTPDO {}  
  case class ReceivedTPDO1CanOpenMessage(address: Int, message: Array[Byte]) 
  		extends ReceivedCanOpenMessage(TPDO1,address,message)
  		with ReceivedTPDO
  case class ReceivedTPDO2CanOpenMessage(address: Int, message: Array[Byte]) 
  		extends ReceivedCanOpenMessage(TPDO2,address,message)
  		with ReceivedTPDO
  case class ReceivedTPDO3CanOpenMessage(address: Int, message: Array[Byte]) 
  		extends ReceivedCanOpenMessage(TPDO3,address,message)
  		with ReceivedTPDO
  case class ReceivedTPDO4CanOpenMessage(address: Int, message: Array[Byte]) 
  		extends ReceivedCanOpenMessage(TPDO4,address,message)
  		with ReceivedTPDO
  /* RPDO Messages */
  trait ReceivedRPDO {} 
  case class ReceivedRPDO1CanOpenMessage(address: Int, message: Array[Byte]) 
  		extends ReceivedCanOpenMessage(RPDO1,address,message)
  		with ReceivedRPDO
  case class ReceivedRPDO2CanOpenMessage(address: Int, message: Array[Byte]) 
  		extends ReceivedCanOpenMessage(RPDO2,address,message)
  		with ReceivedRPDO
  case class ReceivedRPDO3CanOpenMessage(address: Int, message: Array[Byte]) 
  		extends ReceivedCanOpenMessage(RPDO3,address,message)
  		with ReceivedRPDO
  case class ReceivedRPDO4CanOpenMessage(address: Int, message: Array[Byte]) 
  		extends ReceivedCanOpenMessage(RPDO4,address,message)
  		with ReceivedRPDO
  /* SDO Messagees */
  trait ReceivedSDO {
    self : ReceivedCanOpenMessage =>
  } 
  case class ReceivedTSSDOCanOpenMessage(address: Int, message: Array[Byte]) 
  		extends ReceivedCanOpenMessage(TSSDO,address,message)
  		with ReceivedSDO
  case class ReceivedRCSDOCanOpenMessage(address: Int, message: Array[Byte]) 
  		extends ReceivedCanOpenMessage(RCSDO,address,message)
  		with ReceivedSDO
  /* NMT Messages */
  trait ReceivedNMT {} 
  case class ReceivedNMTCanOpenMessage(address: Int, message: Array[Byte]) 
  		extends ReceivedCanOpenMessage(NMT_CMD,address,message)
  		with ReceivedNMT
  case class ReceivedNMTERRORCanOpenMessage(address: Int, message: Array[Byte]) 
  		extends ReceivedCanOpenMessage(NMT_ERROR,address,message)
  		with ReceivedNMT
  case class ReceivedNMTHEARTBEATCanOpenMessage(address: Int, message: Array[Byte]) 
  		extends ReceivedCanOpenMessage(NMT_HEARTBEAT,address,message)
  		with ReceivedNMT
  /* SYNC */
  trait ReceivedSYNC {} 
  case class ReceivedSYNCCanOpenMessage() 
  		extends ReceivedCanOpenMessage(EMERGENCY_OR_SYNC,0,Array())
  		with ReceivedSYNC
  /* EMERGENCY */
  trait ReceivedEMERGENCY {} 
  case class ReceivedEMERGENCYCanOpenMessage(address: Int, message: Array[Byte]) 
  		extends ReceivedCanOpenMessage(EMERGENCY_OR_SYNC,address,message)
  		with ReceivedEMERGENCY
  /* TimeStamp */
  trait ReceivedTIMESTAMP {} 
  case class ReceivedTimestampCanOpenMessage(address: Int, message: Array[Byte])
  		extends ReceivedCanOpenMessage(TIME_STAMP,address,message)
  		with ReceivedTIMESTAMP
  
  /*
   * Message converter for code modularization
   */
  object RecivedCanOpenMessage {
    def apply(id: Long, msg: Array[Byte], flags: Int): ReceivedCanOpenMessage = {
      (getFunction(id),getAddress(id)) match {
        case (NMT_CMD,addr) => //convert into NMT
          	ReceivedNMTCanOpenMessage(addr,msg)
        case (EMERGENCY_OR_SYNC,addr) =>
          if (addr==0) 		//SYNC
            ReceivedSYNCCanOpenMessage()
          else 				//EMERGENCY
            ReceivedEMERGENCYCanOpenMessage(addr, msg)
        case (TIME_STAMP,addr) =>			//Time stamp
          ReceivedTimestampCanOpenMessage(addr, msg)
        case (TPDO1, addr) =>		//Received TPDO type 1
          ReceivedTPDO1CanOpenMessage(addr, msg)
        case (TPDO2, addr) =>		//Received TPDO type 2
          ReceivedTPDO2CanOpenMessage(addr, msg)
        case (TPDO3, addr) =>		//Received TPDO type 3
          ReceivedTPDO3CanOpenMessage(addr, msg)
        case (TPDO4, addr) =>		//Received TPDO type 4
          ReceivedTPDO4CanOpenMessage(addr, msg)
        case (RPDO1, addr) =>		//Received RPDO type 1
          ReceivedRPDO1CanOpenMessage(addr, msg)
        case (RPDO2, addr) =>		//Received RPDO type 2
          ReceivedRPDO2CanOpenMessage(addr, msg)
        case (RPDO3, addr) =>		//Received RPDO type 3
          ReceivedRPDO3CanOpenMessage(addr, msg)
        case (RPDO4, addr) =>		//Received RPDO type 4
          ReceivedRPDO4CanOpenMessage(addr, msg)
        case (TSSDO, addr) =>		//Received SDO sended from server
          ReceivedTSSDOCanOpenMessage(addr, msg)
        case (RCSDO, addr) =>		//Received SDO from client
          ReceivedRCSDOCanOpenMessage(addr, msg)
        case (NMT_ERROR, addr) => 	//Receiveved NMT error
          ReceivedNMTERRORCanOpenMessage(addr, msg)
        case (NMT_HEARTBEAT, addr) =>	//Received NMT heart beat
          ReceivedNMTHEARTBEATCanOpenMessage(addr, msg)
        case any => 
          println("This is an unmanaged message with function: "+any._1+" and address: "+any._2)
          throw new Exception("Unmanaged message received!")
      }
    }
  }

}