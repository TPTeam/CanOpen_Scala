package com.tecniplast.canopen

import akka.actor._
import com.typesafe.config._
import com.tecniplast.device.CanDevice._
import CanOpenMessages._
import akka.dispatch.UnboundedPriorityMailbox
import akka.dispatch.PriorityGenerator
import scala.concurrent.duration._

/*
 * Manage SDO
 * send and receive
 */

object SDO {
  /*
  Command Specifiers
  Start implementing only

  Initiate Domain Download
  Initiate Domain Upload
  */
  import CanOpenObjectDictionary._
  //Commands sended from server
  abstract class SDOToSend(address: Int)(dictObj: CanOpenDictionaryElement)  {
    
    def getDictObj = dictObj
    def getAddress = address
    
    val respTimeout = 20 milliseconds
	val respMaxRetry = 7
    
    def getByte0: Byte
    
    def getMessage: SendRCSDOCanOpenMessage
  }
  
  class SDORequest(address: Int)(dictObj: CanOpenDictionaryElement) 
  	extends SDOToSend(address)(dictObj) {
    def getByte0 = 0x40
    
    def getMessage = SendRCSDOCanOpenMessage(
        address,
        (Array(
            getByte0,
            dictObj.getIndex(0),
            dictObj.getIndex(1),
            dictObj.getSubIndex)
     ))
  } 
  class SDOCommand(address: Int)(dictObj: CanOpenDictionaryElement, value: Array[Byte]) 
  	extends SDOToSend(address)(dictObj) {
    final val data_max_len = 0x04
    require(
        value.length <= data_max_len
        )
    def getValue = value
    def getByte0 =
      0x20 + 0x03
    
      //probabilmente baco sul protocollo Micrichip
      //+ (data_max_len-value.length << 2)
    val completeArr: Array[Byte] = Array(0x00,0x00,0x00,0x00)
    val extendedValue: Array[Byte] = (value ++: completeArr).take(4)
      
    def getMessage = SendRCSDOCanOpenMessage(
        address,
        (Array(
            getByte0,
            dictObj.getIndex(0),
            dictObj.getIndex(1),
            dictObj.getSubIndex,
            extendedValue(0),
            extendedValue(1),
            extendedValue(2),
            extendedValue(3))
     ))
  }
  //Response received from client
  class SDORequestResponse(address: Int)(dictObj: CanOpenDictionaryElement, value: Array[Byte]) {
    final val data_max_len = 0x04
    require(
        value.length <= data_max_len
        )
    
    def getAddress = address
    
    def getByte0 =
      0x40 & 0x03 & (data_max_len-value.length << 2)
  }
  class SDORequestResponseOK(address: Int)(dictObj: CanOpenDictionaryElement, value: Array[Byte]) 
  	extends SDORequestResponse(address)(dictObj,value) {
    def getValue = value
  }
  class SDORequestResponseFault(address: Int)(dictObj: CanOpenDictionaryElement)
    extends SDORequestResponse(address)(dictObj,Array())
  class SDOCommandResponse(address: Int)(dictObj: CanOpenDictionaryElement) {
    def getByte0 =
      0x60
  }
  case class SDOCommandResponseOK(address: Int)(dictObj: CanOpenDictionaryElement)
  	extends SDOCommandResponse(address)(dictObj)
  case class SDOCommandResponseFault(address: Int)(dictObj: CanOpenDictionaryElement)
    extends SDOCommandResponse(address)(dictObj)

  object SDORequestResponse {
    
    def apply(resp: SendRCSDOCanOpenMessage): SDORequestResponseFault = {
        new SDORequestResponseFault(resp.address)(
            CanOpenObjectDictionary(
                (resp.value(2) << 8) + resp.value(1),resp.value(3)))
    }
    
    def apply(resp: ReceivedTSSDOCanOpenMessage): SDORequestResponseOK = {
       new SDORequestResponseOK(resp.address)(CanOpenObjectDictionary(
                (resp.message(2) << 8) + resp.message(1),resp.message(3)), 
                resp.message.drop(4))
      
    }
    
  }
}


class SDOManager() extends Actor {
  
  import SDO._
  
  def getNameFromMsg(msg: ReceivedCanOpenMessage): String =
    "" + msg.getAddress +
       + msg.getMessage(1) +
       + msg.getMessage(2) +
       + msg.getMessage(3)
  def getNameFromMsg(msg: SDOToSend): String =
    "" + msg.getAddress +
       + msg.getDictObj.getIndex(0) +
       + msg.getDictObj.getIndex(1) +
       + msg.getDictObj.getSubIndex
  
  
  def receive = {
    case rq: SDOToSend => 
      val actualSender = sender
      val actName = getNameFromMsg(rq)
      if (context.child(actName).isEmpty) {
    	  (rq) match {
    	    case r: SDORequest =>
    	      context.actorOf(Props(SDORequestActor(r,actualSender,0x40,r.respTimeout)),actName)
    	    case r: SDOCommand =>
    	        	context.actorOf(
    	        	    Props(
    	        	        SDORequestActor(r,actualSender,0x60,r.respTimeout)),actName)
    	    case any => 
    	      println("SDO manager received from children unmanaged "+any)  
    	  }
    	  
  	  } else {
  	    //too late
  	    //import context.dispatcher
  	    //context.system.scheduler.scheduleOnce(respTimeout)(actualSender forward rq)
  	    
  	    //println("children yet present?? "+actName)
  	    //self ! rq
  	  }
    case resp: ReceivedCanOpenMessage =>
      val childOpt = 
      	context.child(getNameFromMsg(resp))
      if (childOpt.isDefined)
      	childOpt.map(_ ! resp)
      //else
      //  println("cannot find child with name "+getNameFromMsg(resp))
    case send: SendRCSDOCanOpenMessage =>
      context.parent ! send
    case any => 
      println("SDO manager received unmanaged "+any+" "+sender.path)
  }
  
  case class SDORequestActor(msgToSend: SDOToSend,answareTo: ActorRef, okMask : Byte, delay: FiniteDuration) extends Actor {
	case object Timeout
	
	val sendedMsg = msgToSend.getMessage
	/*	  
     SendRCSDOCanOpenMessage(
        msgToSend.getAddress,
        (Array(
            msgToSend.getByte0,
            msgToSend.getDictObj.getIndex(0),
            msgToSend.getDictObj.getIndex(1),
            msgToSend.getDictObj.getSubIndex)
    
     ))
     */
	
    context.parent ! sendedMsg
     
    import context.dispatcher
    context.system.scheduler.scheduleOnce(delay)(self ! Timeout)
    
    def checkResponse(resp: ReceivedTSSDOCanOpenMessage) = 
	  (resp.message.length >= 4 &&
	      resp.message(1) == sendedMsg.value(1) &&
	      resp.message(2) == sendedMsg.value(2) &&
	      resp.message(3) == sendedMsg.value(3) 
	  )
    
    def receive =
	  waitAnswareAndRetry(msgToSend.respMaxRetry)
	
	def waitAnswareAndRetry(retry: Int): PartialFunction[Any,Unit] = {
	   case resp: ReceivedTSSDOCanOpenMessage =>
        if (checkResponse(resp)) {
          //verificare se va bene anche con command
          if (((resp.message(0) & 0xE0) == 0x40)) {
        	  answareTo ! SDORequestResponse(resp)
        	  context.stop(self)
          } else {
            if (retry>0) {
              //così mantengo l'esecuzione lineare
            	//context.become(waitAnswareAndRetry(retry-1))
            	//context.parent ! sendedMsg
            	//context.system.scheduler.scheduleOnce(delay)(self ! Timeout)
            } else {
            	answareTo ! SDORequestResponse(sendedMsg)
        		context.stop(self)
            } 
            //Wait for timeout
            //println("causato da un fault?")
        	//answareTo ! SDORequestResponse(sendedMsg)
          }
          //context.stop(self)
        } else {
          println("...must not happens...")
        }
      case Timeout =>
        if (retry>0) {
        	context.become(waitAnswareAndRetry(retry-1))
        	context.parent ! sendedMsg
        	context.system.scheduler.scheduleOnce(delay)(self ! Timeout)
        } else {
        	answareTo ! SDORequestResponse(sendedMsg)
        	context.stop(self)
        }
	}
    
  }
  
}

import SDO._

  trait OneWordExtractor {
    self: SDORequestResponseOK =>
      def getValue1 =
      	try {
      		(-+(getValue(1)) << 8) + -+(getValue(0))
      	} catch {
        case _ : Throwable => 0x00
      }
      def getOptValue1 =
      	try {
      		Some((-+(getValue(1)) << 8) + -+(getValue(0)))
      	} catch {
        case _ : Throwable => None
      }
  }
  trait OneDWordExtractor {
    self: SDORequestResponseOK =>
      def getValue1 =
      	try {
      		(((-+(getValue(1)) << 8) + -+(getValue(0))) << 16) +
      		((-+(getValue(3)) << 8) + -+(getValue(2)))
      	} catch {
        case _ : Throwable => 0x00
      }
      def getOptValue1 =
      	try {
      		Some((((-+(getValue(1)) << 8) + -+(getValue(0))) << 16) +
      		((-+(getValue(3)) << 8) + -+(getValue(2))))
      	} catch {
        case _ : Throwable => None
      }
  }

  trait TwoWordExtractor {
    self: SDORequestResponseOK =>
      def getValue2 =
      	try {
      		(-+(getValue(3)) << 8) + -+(getValue(2))
      	} catch {
        case _ : Throwable => 0x00
      }
      def getOptValue2 =
      	try {
      		Some((-+(getValue(3)) << 8) + -+(getValue(2)))
      	} catch {
        case _ : Throwable => None
      }
      def getValue1 =
      	try {
      		(-+(getValue(1)) << 8) + -+(getValue(0))
      	} catch {
        case _ : Throwable => 0x00
      }
      def getOptValue1 =
      	try {
      	  Some(((-+(getValue(1))) << 8) + -+(getValue(0)))
      	} catch {
        case _ : Throwable => None
      }
  }
  trait ThreeBitsExtractor {
    self: SDORequestResponseOK =>
      def getStatus = 
        try {
      		(getValue(0) & 0x07)
      	} catch {
        case _ : Throwable => 0x00
      }
  }
  trait SingleBooleanExtractor {
    self: SDORequestResponseOK =>
      def getStatus: Boolean = 
        try {
      		((getValue(0) & 0x01) > 0)
      	} catch {
        case _ : Throwable => false
      }
  }
  
