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
  //Commands sended from server
  abstract class SDOToSend(address: Int)(dictObj: CanOpenDictionaryElement)  {
    
    def getDictObj = dictObj
    def getAddress = address
    
    val respTimeout = 20 milliseconds
	val respMaxRetry = 7
    
    def getByte0: Byte
    
    def getMessage: SendRCSDOCanOpenMessage
  }
  
  class SDORequest(address: Int)(dictObj: CanOpenDictionaryElement, value: Array[Byte] = Array()) 
  	extends SDOToSend(address)(dictObj) {
    def getByte0 = 0x40
    
    def getMessage = SendRCSDOCanOpenMessage(
        address,
        (Array(
            getByte0,
            dictObj.getIndex(0),
            dictObj.getIndex(1),
            dictObj.getSubIndex) ++
            value
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
    
      //probabilmente baco sul protocollo Microchip
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
  
  class SDOBlockRequest(address: Int)(dictObj: CanOpenDictionaryElement) 
  	extends SDOToSend(address)(dictObj) {
    def getByte0 = 0x40
    
    override val respTimeout = 500 milliseconds
	override val respMaxRetry = 0
	
	val partTimeout = 200 milliseconds
    
    def getMessage = SendRCSDOCanOpenMessage(
        address,
        (Array(
            getByte0,
            dictObj.getIndex(0),
            dictObj.getIndex(1),
            dictObj.getSubIndex)
     ))
  } 
  
  //Response received from client
  class SDORequestResponse(address: Int)(dictObj: CanOpenDictionaryElement, value: Array[Byte]) {
    final val data_max_len = 0x04
    
    /* to manage block response...
    require(
        value.length <= data_max_len
    )
    */
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
  class SDOCommandResponseOK(address: Int)(dictObj: CanOpenDictionaryElement)
  	extends SDOCommandResponse(address)(dictObj)
  class SDOCommandResponseFault(address: Int)(dictObj: CanOpenDictionaryElement)
    extends SDOCommandResponse(address)(dictObj)

  abstract class SDORequestResponseInterface {
    
    def apply(resp: SendRCSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDORequestResponseFault 
    
    def apply(resp: ReceivedTSSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDORequestResponseOK 
    
    def apply(sended: SendRCSDOCanOpenMessage,resp: Seq[ReceivedTSSDOCanOpenMessage])(implicit dict: CanOpenObjectDictionary): SDORequestResponseOK 
  }
  
  abstract class SDOCommandResponseInterface {
    
    def apply(resp: SendRCSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDOCommandResponseFault 
    
    def apply(resp: ReceivedTSSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDOCommandResponseOK 
    
  }
    
  object SDORequestResponse extends SDORequestResponseInterface {
    
    def apply(resp: SendRCSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDORequestResponseFault = {
        new SDORequestResponseFault(resp.address)(
            dict.apply(
                (resp.value(2) << 8) + resp.value(1),resp.value(3)))
    }
    
    def apply(resp: ReceivedTSSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDORequestResponseOK = {
       new SDORequestResponseOK(resp.address)(
			dict.apply(
                (resp.message(2) << 8) + resp.message(1),resp.message(3)), 
                resp.message.drop(4))
    }
    
    def apply(sended: SendRCSDOCanOpenMessage,resp: Seq[ReceivedTSSDOCanOpenMessage])(implicit dict: CanOpenObjectDictionary): SDORequestResponseOK = {
      def aggregateBody(in: Seq[ReceivedTSSDOCanOpenMessage], part: Array[Byte]): Array[Byte] = 
        if (in.isEmpty) part
        else aggregateBody(in.tail, part.++:(in.head.message.drop(1)))
      
      
       new SDORequestResponseOK(sended.address)(
           dict.apply(
                (sended.value(2) << 8) + sended.value(1),sended.value(3)), 
                aggregateBody(resp,Array()))
    }
  }
  
  object SDOCommandResponse extends SDOCommandResponseInterface {
    
    def apply(resp: SendRCSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDOCommandResponseFault = {
        new SDOCommandResponseFault(resp.address)(
            dict.apply(
                (resp.value(2) << 8) + resp.value(1),resp.value(3)))
    }
    
    def apply(resp: ReceivedTSSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDOCommandResponseOK = {
       new SDOCommandResponseOK(resp.address)(
			dict.apply(
                (resp.message(2) << 8) + resp.message(1),resp.message(3)))
    }
    
  }
}


abstract class SDOManager() extends Actor {
  
  implicit val object_dict: CanOpenObjectDictionary //= CanOpenObjectDictionary
  
  val requestResponceResolver: SDO.SDORequestResponseInterface = SDO.SDORequestResponse
  val commandResponceResolver: SDO.SDOCommandResponseInterface = SDO.SDOCommandResponse
  
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
    
  def getBlockName(msg: ReceivedCanOpenMessage): String =
    "" + msg.getAddress + "block"
  def getBlockName(msg: SDOToSend): String =
    "" + msg.getAddress + "block"
  
  def receive = {
    case rq: SDOToSend => 
      val actualSender = sender.path
      val actName = getNameFromMsg(rq)
      val blockName = getBlockName(rq)
      if (context.child(actName).isEmpty && context.child(blockName).isEmpty) {
    	  (rq) match {
    	    case r: SDORequest =>
    	      context.actorOf(
    	          Props(
    	              SDORequestActor(r,actualSender,0x40,r.respTimeout)),actName)
    	    case c: SDOCommand =>
    	      context.actorOf(
    	          Props(
    	              SDORequestActor(c,actualSender,0x60,c.respTimeout)),actName)
    	    case b: SDOBlockRequest =>
    	      context.actorOf(
    	      	    Props(
    	      	       SDOBlockRequestActor(b,actualSender,0x40,b.respTimeout)),blockName)
    	    case any => 
    	      println("SDO manager received from children unmanaged "+any)  
    	  }
    	  
  	  } else {
  	    import context.dispatcher
  	    //trying later
  	    context.system.scheduler.scheduleOnce(10 millis)({
            for {
              realSend <- context.system.actorSelection(actualSender).resolveOne(1 second)
            } yield {
              self.tell(rq,realSend)
            }
  	    })
  	  }
    case resp: ReceivedCanOpenMessage =>
      val blockOpt = (context.child(getBlockName(resp)))
      if (blockOpt.isDefined) {
        blockOpt.map(_ ! resp)
      } else {
    	  val childOpt = 
    			  context.child(getNameFromMsg(resp))
    			  
    	  if (childOpt.isDefined)
    		  childOpt.map(_ ! resp)
      }
    case send: SendRCSDOCanOpenMessage =>
      context.parent ! send
    case any => 
      println("SDO manager received unmanaged "+any+" "+sender.path)
  }
  
  case class SDORequestActor(msgToSend: SDOToSend,_answareTo: ActorPath, okMask : Byte, delay: FiniteDuration) extends Actor {
    import context.dispatcher
	case object Timeout
	
	val sendedMsg = msgToSend.getMessage
	
    context.parent ! sendedMsg
    
    def answareTo = context.actorSelection(_answareTo)
     
    def doTimeout =
    	context.system.scheduler.scheduleOnce(delay)(self ! Timeout)
    
    def checkResponse(resp: ReceivedTSSDOCanOpenMessage) = 
	  (resp.message.length >= 4 &&
	      resp.message(1) == sendedMsg.value(1) &&
	      resp.message(2) == sendedMsg.value(2) &&
	      resp.message(3) == sendedMsg.value(3) 
	  )
    
    def receive =
	  waitAnswareAndRetry(msgToSend.respMaxRetry,doTimeout)
	      
	def waitAnswareAndRetry(retry: Int, timeout: Cancellable): PartialFunction[Any,Unit] = {
	   case resp: ReceivedTSSDOCanOpenMessage =>
        if (checkResponse(resp)) {          
          if (((resp.message(0) & 0xE0) == okMask)) {

            if (okMask == 0x40.toByte) {
              answareTo ! requestResponceResolver(resp)
            } else if (okMask == 0x60.toByte) {
              answareTo ! commandResponceResolver(resp)
            } else
              println("Message cannot be managed in this way.")
            
        	timeout.cancel
        	context.stop(self)
          } else {
            if (retry<=0) {
            	if (okMask == 0x40.toByte) {
            		answareTo ! requestResponceResolver(sendedMsg)
            	} else if (okMask == 0x60.toByte) {
            		answareTo ! commandResponceResolver(sendedMsg)
            	} else
            		println("This fault cannot be managed in this way.")
            		
            	timeout.cancel
        		context.stop(self)
            } 
          }
        } else {
          println("...must not happens...")
        }
      case Timeout =>
        if (retry>0) {
        	context.become(waitAnswareAndRetry(retry-1, doTimeout), true)
        	context.parent ! sendedMsg
        } else {
        	if (okMask == 0x40.toByte) {
            	answareTo ! requestResponceResolver(sendedMsg)
           	} else if (okMask == 0x60.toByte) {
            	answareTo ! commandResponceResolver(sendedMsg)
           	} else 
           	  	println("Message not managed in this way.")

        	timeout.cancel
        	context.stop(self)
        }
	}
    
  }
  
  case class SDOBlockRequestActor(msgToSend: SDOBlockRequest,_answareTo: ActorPath, okMask : Byte, delay: FiniteDuration) extends Actor {
	case object Timeout
	
	val sendedMsg = msgToSend.getMessage

	def answareTo = context.actorSelection(_answareTo)
	
    context.parent ! sendedMsg
    val initDate = new java.util.Date().getTime
     
    import context.dispatcher
    def doTimeout(d: FiniteDuration) = 
      context.system.scheduler.scheduleOnce(d)(self ! Timeout)
    
    def checkResponse(resp: ReceivedTSSDOCanOpenMessage) = 
	  (resp.message.length >= 4 &&
	      resp.message(1) == sendedMsg.value(1) &&
	      resp.message(2) == sendedMsg.value(2) &&
	      resp.message(3) == sendedMsg.value(3) 
	  )
	  
    def receive =
	  waitAnswareAndRetry(msgToSend.respMaxRetry, doTimeout(delay))
	
	def waitAnswareAndRetry(retry: Int, timeout: Cancellable): PartialFunction[Any,Unit] = {
	   case resp: ReceivedTSSDOCanOpenMessage =>
        if (checkResponse(resp)) {
          timeout.cancel
          val packetToDownload =  -+(resp.getMessage(4)) + (-+(resp.getMessage(5)) << 8)

          context.become(
              sequentialReceive(packetToDownload, doTimeout(msgToSend.partTimeout), Seq()), true)
        } else {
          //message for another request maybe...
          context.parent ! resp
        }
      case Timeout =>
        if (retry>0) {
        	context.become(waitAnswareAndRetry(retry-1,timeout))
        	context.parent ! sendedMsg
        	context.system.scheduler.scheduleOnce(delay)(self ! Timeout)
        } else {
        	//println("TIMEOUT SU PRIMA RISPOSTA...")
        	answareTo ! requestResponceResolver(sendedMsg)
        	context.stop(self)
        }
	}
	
	//CRC calc
	def crcCalc(buffer: Array[Byte]) = {
		val initCrc = 0x1d0f
    	val polynomial = 0x1021
    
    	val bitList = (0).to(7)
    
    	def bitCalc(b: Byte, part: Int, blist: Seq[Int] = bitList): Int =
    		if (blist.isEmpty) part
    		else {
    			val bit = ((b   >> (7-blist.head) & 1) == 1)
    			val c15 = ((part >> 15    & 1) == 1)
        
    			val newPart = 
    				(if (c15 ^ bit) (((part << 1) ^ polynomial))
    				else (part << 1)) & 0xFFFF
        
   				bitCalc(b, newPart, blist.tail)
    		} 
    
    	def _crcCalc(in: Array[Byte], crc: Int): Int =
    		if (in.isEmpty) 
    			crc
    		else
    			_crcCalc(in.tail, bitCalc(in.head,crc))
    
    	_crcCalc(buffer, initCrc)
	}
	
	def sequentialReceive(totalPacket: Int, timeout: Cancellable,buffer: Seq[ReceivedTSSDOCanOpenMessage]): Receive = {
	  case resp: ReceivedTSSDOCanOpenMessage =>
	   timeout.cancel
	   //order is not checkable
		 if (buffer.size == totalPacket) {
		   def aggregate(in: Seq[Array[Byte]], part: Array[Byte]): Array[Byte] =
		     if (in.isEmpty) part
		     else aggregate(in.tail, part.++(in.head))
		   
		   val orderedFrames =
			 (buffer.+:(resp)).sortBy(x =>(x.getMessage(0) & 0xFF).toInt)
		   val orderedBuffer = 
		     orderedFrames.take(buffer.size)
		   val orderedBufferNoSeq =
		     orderedBuffer.map(_.getMessage.drop(1))
		     
		   val byteForCrc = 
		     aggregate(orderedBufferNoSeq,Array())
		     
		   val lastMsg =   
		     orderedFrames.last
		     	     
		   val boardCrc =
		     ((lastMsg.getMessage(1).toInt & 0xFF) + 
		     (((lastMsg.getMessage(2).toInt & 0xFF) << 8) & 0xFFFF)
		     & 0xFFFF)
		     
		   val myCrc = 
		     crcCalc(byteForCrc)
		   
		    if (myCrc==boardCrc)
		      answareTo ! requestResponceResolver(sendedMsg, orderedBuffer)
		    else
		      answareTo ! requestResponceResolver(sendedMsg)
		   
		   context.stop(self)
		 } else {
		  context.become(
		      sequentialReceive(
		          totalPacket, 
		          doTimeout(msgToSend.partTimeout), 
		          buffer.+:(resp)
		   ), true)
		 }
	  case Timeout =>
	    //println("Single message timeout... buffer is "+buffer.size+" total are "+totalPacket)
	    val allM = 1.to(totalPacket).map(_.toByte)
	    /*println("I'm missing "+
	    			allM.filterNot(i => 
	    			  buffer.exists(m => m.getMessage(0) == i)
	    			)
	    		)
   		*/
	    answareTo ! requestResponceResolver(sendedMsg)
        context.stop(self)
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
      
      private val _getValue = (x: Array[Byte]) => {
        try {
          Some(
        	(((-+(x(0))).toLong) 		& 0x000000FF) +
        	(((-+(x(1))).toLong << 8)	& 0x0000FF00) +
        	(((-+(x(2))).toLong << 16)	& 0x00FF0000) +
        	(((-+(x(3))).toLong << 32)	& 0xFF000000)
          )
        } catch {
          case _ : Throwable => None
        }
        }
      
      def getValue1: Long =
      	_getValue(getValue).getOrElse(0x00)
      def getOptValue1: Option[Long] =
      	_getValue(getValue)
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
  
