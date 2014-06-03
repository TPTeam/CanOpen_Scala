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
//    msg.getMessage.mkString("")
       + msg.getMessage(1) +
       + msg.getMessage(2) +
       + msg.getMessage(3)

  def getNameFromMsg(msg: SDOToSend): String =
    "" + msg.getAddress +
    //msg.getMessage.value.mkString("")
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
    	      context.actorOf(Props(SDORequestActor(r,actualSender,0x40,r.respTimeout)),actName)
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
  	    //println("[SDO MANAGER] -> Request still present "+actName+" "+context.child(actName).isEmpty)
  	    //too late
  	    import context.dispatcher
  	    /* così funziona
  	    context.system.scheduler.scheduleOnce(rq.respTimeout)(
  	        self.tell(rq,context.actorFor(actualSender))
  	    )*/
  	    //così è più elegante
  	    //provo con un ritardo minimo
  	    context.system.scheduler.scheduleOnce(10 millis)({
            for {
              realSend <- context.system.actorSelection(actualSender).resolveOne(1 second)
            } yield {
              self.tell(rq,realSend)
            }
  	    })
  	    
  	    /* NON mantiene integrità sul sender...
  	    context.system.scheduler.scheduleOnce(rq.respTimeout)(
  	        self forward rq
  	    )
  	    */
  	    //println("children yet present?? "+actName)
  	    //self.forward(rq)
  	    //self ! rq
  	  }
    case resp: ReceivedCanOpenMessage =>
      val blockOpt = (context.child(getBlockName(resp)))
      if (blockOpt.isDefined) {
        blockOpt.map(_ ! resp)
      } else {
    	  val childOpt = 
    			  context.child(getNameFromMsg(resp))
    			  
    	  //println("Risposta per "+getNameFromMsg(resp)+" "+childOpt.isDefined)
    	  if (childOpt.isDefined)
    		  childOpt.map(_ ! resp)
      }
      //else
      //  println("cannot find child with name "+getNameFromMsg(resp))
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
          //println("ok qui devo rispondere!!")
          
          if (((resp.message(0) & 0xE0) == okMask)) {
            if (okMask == 0x40.toByte) {
              //println("Avanti da qui!!!")
              answareTo ! requestResponceResolver(resp)
            } else if (okMask == 0x60.toByte)
              answareTo ! commandResponceResolver(resp)
            else
              println("Message not managed in this way.")
            
        	  //answareTo ! requestResponceResolver(resp)
        	  timeout.cancel
        	  context.stop(self)
          } else {
            println("received timeout! "+retry)
            if (retry<=0) {
            	if (okMask == 0x40.toByte)
            		answareTo ! requestResponceResolver(sendedMsg)
            	else if (okMask == 0x60.toByte)
            		answareTo ! commandResponceResolver(sendedMsg)
            	else
            		println("Message not managed in this way.")
            		
            	//answareTo ! requestResponceResolver(sendedMsg)
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
        	answareTo ! requestResponceResolver(sendedMsg)
        	timeout.cancel
        	context.stop(self)
        }
	}
    
  }
  
  case class SDOBlockRequestActor(msgToSend: SDOBlockRequest,_answareTo: ActorPath, okMask : Byte, delay: FiniteDuration) extends Actor {
	case object Timeout
	
	//DA FINIRE DI IMPLEMENTARE

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
          //avanti da qui devo leggere il numero di pacchetti e proseguire
          timeout.cancel
          val packetToDownload =  -+(resp.getMessage(4)) + (-+(resp.getMessage(5)) << 8)
          //println("pacchetti da scaricare  "+packetToDownload)
          context.become(
              sequentialReceive(packetToDownload, doTimeout(msgToSend.partTimeout), Seq()), true)
        } else {
          println("...must not happens...")
          answareTo ! requestResponceResolver(sendedMsg)
          context.stop(self)
        }
      case Timeout =>
        if (retry>0) {
        	context.become(waitAnswareAndRetry(retry-1,timeout))
        	context.parent ! sendedMsg
        	context.system.scheduler.scheduleOnce(delay)(self ! Timeout)
        } else {
        	println("TIMEOUT SU PRIMA RISPOSTA...")
        	answareTo ! SDORequestResponse(sendedMsg)
        	context.stop(self)
        }
	}
	
	def sequentialReceive(totalPacket: Int, timeout: Cancellable,buffer: Seq[ReceivedTSSDOCanOpenMessage]): Receive = {
	  case resp: ReceivedTSSDOCanOpenMessage =>
	   timeout.cancel
	   //L'ordine non e' verificabile...
	   //if (-+(resp.message(0)) == buffer.size/*+1*/) {
		 if (buffer.size == totalPacket) {
		   //println("ULTIMO PACCHETTO!!! manca la verifica del CRC PRIMA "+crc.getValue()+" "+self.path.name)
		   //crc.update(resp.message,7, 1)
		   //resp.message.drop(1).foreach(x => crc.update(-+(x)))
		   //println("ULTIMO PACCHETTO!!! manca la verifica del CRC DOPO  "+crc.getValue()+" "+self.path.name)
		   //val now = new java.util.Date().getTime
		   //println("Init at "+initDate+" now is "+now+" used "+(now-initDate))
		   answareTo ! requestResponceResolver(sendedMsg, buffer.+:(resp))
		 } else {
		  println("GOING ON "+buffer.size+" received packet "+ -+(resp.message(0))+" "+self.path.name)
		  //crc.update(resp.message,7, 1)
		  //resp.message.drop(1).foreach(x => crc.update(-+(x)))
		  context.become(
		      sequentialReceive(
		          totalPacket, 
		          doTimeout(msgToSend.partTimeout), 
		          buffer.+:(resp)
		   ), true)
		 }
	   /*} else {
	     println("out of order or strange packet... "+ -+(resp.message(0))+" I was waiting for "+buffer.size)
	     answareTo ! SDORequestResponse(sendedMsg)
         context.stop(self)
	   }*/
	  case Timeout =>
	    println("Single message timeout...")
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
  
