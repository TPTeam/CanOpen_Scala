package com.tecniplast.canopen.SDOManagement.legacy

import akka.actor.{Actor, ActorPath, Cancellable, Props}
import com.tecniplast.canopen.CanOpenMessages.{ReceivedCanOpenMessage, ReceivedTSSDOCanOpenMessage, SendRCSDOCanOpenMessage}
import com.tecniplast.canopen.SDOManagement.{SDO, SDOManager}
import com.tecniplast.canopen._

import scala.concurrent.duration._

/**
 * Created by fbignardi on 3/23/15.
 */
abstract class SingleDictionarySDOManager() extends SDOManager {

  implicit val object_dict: CanOpenObjectDictionary //= CanOpenObjectDictionary

  val requestResponceResolver: SDO.LegacySDORequestResponseInterface = SDO.LegacySDORequestResponse
  val commandResponceResolver: SDO.LegacySDOCommandResponseInterface = SDO.LegacySDOCommandResponse

  import com.tecniplast.canopen.SDOManagement.SDO._

  def getNameFromMsg(msg: ReceivedCanOpenMessage): String = {
    val x = "" + msg.getAddress +
      +msg.getMessage(1) +
      +msg.getMessage(2) +
      +msg.getMessage(3)

    println(s"getNameFromMsg => $x")
    x
  }
  def getNameFromMsg(msg: SDOToSend): String = {
    val x = "" + msg.getAddress +
      +msg.getDictObj.getIndex(0) +
      +msg.getDictObj.getIndex(1) +
      +msg.getDictObj.getSubIndex

    println(s"getNameFromMsg => $x")
    x
  }
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
      println(s"SDOMANAGER received => $resp and blockName is => ${getBlockName(resp)}")
      val blockOpt = (context.child(getBlockName(resp)))
      if (blockOpt.isDefined) {
        blockOpt.map(_ ! resp)
      } else {
        println(s"SDOMANAGER no BLOCK child!")
    	  val childOpt =
    	    try
    			context.child(getNameFromMsg(resp))
    		catch {
    		  case _ : Throwable =>
            println(s"SDOMANAGER Throwable in getNameFromMSG")
            None
    		}

    	  if (childOpt.isDefined) {
          println(s"SDOMANAGER YES Response child!")
          childOpt.map(_ ! resp)
        }
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
       println(s"SDO request actor: received ReceivedTSSDOCanOpenMessage => ${resp.message(0)}")
        if (checkResponse(resp)) {
          if (((resp.message(0) & 0xE0) == okMask)) {

            if (okMask == 0x40.toByte) {
              println(s"SDO request actor: okMask is Equal to 0x40 => sending to requestResponceResolver ")
              answareTo ! requestResponceResolver(resp)
            } else if (okMask == 0x60.toByte) {
              println(s"SDO request actor: okMask is Equal to 0x60 => sending to commandResponceResolver ")
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
            } else{
              println(s"response message first byte => ${resp.message(0)} & 0xE0 is not equal to $okMask")

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
