package com.tecniplast.canopen.SDOManagement.legacy

import akka.actor.{Cancellable, ActorLogging, Actor, ActorPath}
import com.tecniplast.canopen.CanOpenMessages.ReceivedTSSDOCanOpenMessage
import com.tecniplast.canopen.SDOManagement.LegacyDictionaryRecord
import com.tecniplast.canopen.SDOManagement.SDO.{SDOBlockRequest, SDOToSend}
import com.tecniplast.canopen._

import scala.concurrent.duration.FiniteDuration

/**
 * Created by fbignardi on 5/25/15.
 */
case class SDORequestActor(msgToSend: SDOToSend,_answareTo: ActorPath, okMask : Byte, delay: FiniteDuration, dRecord: LegacyDictionaryRecord) extends Actor with ActorLogging{
  import context.dispatcher
  case object Timeout

  implicit val dict = dRecord.dictionary

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
            answareTo ! dRecord.requestResponseManager(resp)
          } else if (okMask == 0x60.toByte) {
            answareTo ! dRecord.commandResponseManager(resp)
          } else
            log.error("Message cannot be managed in this way.")

          timeout.cancel
          context.stop(self)
        } else {
          if (retry<=0) {
            if (okMask == 0x40.toByte) {
              answareTo ! dRecord.requestResponseManager(sendedMsg)
            } else if (okMask == 0x60.toByte) {
              answareTo ! dRecord.commandResponseManager(sendedMsg)
            } else
              log.error("This fault cannot be managed in this way.")

            timeout.cancel
            context.stop(self)
          }
        }
      } else {
        log.error("...must not happens...")
      }
    case Timeout =>
      if (retry>0) {
        context.become(waitAnswareAndRetry(retry-1, doTimeout), true)
        context.parent ! sendedMsg
      } else {
        if (okMask == 0x40.toByte) {
          answareTo ! dRecord.requestResponseManager(sendedMsg)
        } else if (okMask == 0x60.toByte) {
          answareTo ! dRecord.commandResponseManager(sendedMsg)
        } else
          log.error("Message not managed in this way.")

        timeout.cancel
        context.stop(self)
      }
  }

}

case class SDOBlockRequestActor(msgToSend: SDOBlockRequest,_answareTo: ActorPath, okMask : Byte, delay: FiniteDuration, dRecord: LegacyDictionaryRecord) extends Actor {
  case object Timeout

  implicit val dict = dRecord.dictionary

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
        answareTo ! dRecord.requestResponseManager(sendedMsg)
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
          answareTo ! dRecord.requestResponseManager(sendedMsg, orderedBuffer)
        else
          answareTo ! dRecord.requestResponseManager(sendedMsg)

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
      answareTo ! dRecord.requestResponseManager(sendedMsg)
      context.stop(self)
  }

}