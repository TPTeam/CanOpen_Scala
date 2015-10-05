package com.tecniplast.canopen.SDOManagement

import akka.actor._
import com.tecniplast.canopen.CanOpenMessages.{ReceivedCanOpenMessage, ReceivedTSSDOCanOpenMessage, SendRCSDOCanOpenMessage}
import com.tecniplast.canopen.SDOManagement.CanOpenSDOMessages.{SDOSegmentRequest, SDORequestMessage}
import com.tecniplast.canopen.SDOManagement.SDO.SDOToSend
import com.tecniplast.canopen._
import scala.concurrent.duration.FiniteDuration

case object Timeout

/**
 * Created by fbignardi on 5/25/15.
 */
trait SDORequestMessageResolver {

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

  def getSegmentActorName(address: Int, byte0: Int): String =
    "" + address + byte0

  def getBlockName(msg: ReceivedCanOpenMessage): String =
    "" + msg.getAddress + "block"

  def getBlockName(msg: SDOToSend): String =
    "" + msg.getAddress + "block"

}

case class SegmentedSDOPayload(value: Array[Byte])

case object SDOTransferAborted

abstract class SDORequestMessageManager(sr: SDORequestMessage,_answareTo: ActorPath, delay: FiniteDuration, dRecord: DictionaryRecord)
  extends Actor with SDORequestMessageResolver with ActorLogging{

  import context.dispatcher

  val sendedMsg = sr.initMessage.getMessage

  implicit val dict = dRecord.dictionary

  def answareTo = context.actorSelection(_answareTo)

  def doTimeout =
    context.system.scheduler.scheduleOnce(delay)(self ! Timeout)

  def checkResponse(req: SendRCSDOCanOpenMessage,resp: ReceivedTSSDOCanOpenMessage) =
    (resp.message.length >= 4 &&
      resp.message(1) == req.value(1) &&
      resp.message(2) == req.value(2) &&
      resp.message(3) == req.value(3)
      )


  def receive: Receive = initPhase(sr.initMessage.respMaxRetry,doTimeout)

  def getAndSendInitMessage()

  def manageResponse(resp: ReceivedTSSDOCanOpenMessage)

  def initPhase(retry: Int, timeout: Cancellable): PartialFunction[Any,Unit] = {

    getAndSendInitMessage()

    def _waitInitResponse(count: Int): Receive = {

      case SDOTransferAborted =>
        log.warning(s"SDORequestMessageActor => current SDO transfer aborted, retry...")
        timeout.cancel
        context.children.map(x => x ! PoisonPill)
        doTimeout

      case resp: ReceivedTSSDOCanOpenMessage =>
        if (checkResponse(sendedMsg, resp)) {
          timeout.cancel
          if (SDOFlags.isSDOResponse(resp.message(0))) {
            manageResponse(resp)
          } else {
            log.error(s"SDORequestMessageActor => ReceivedTSSDOCanOpenMessage...unmanaged response! ${(resp.message(0) & 0xE0)}")
            context.stop(self)
          }
        } else {
          log.error(s"SDORequestMessageActor unmanaged SDO Response received => $resp")
          context.stop(self)
        }
      case Timeout =>
        val str = sr.initMessage.getMessage.value.map(x => get2DigitsHex(x)).fold("")((x,y)=> x+" "+y)
        log.warning(s"SDORequestMessageActor => TIMEOUT in waitInitResponse for message => $str")
        if (retry > 0) {
          context.become(initPhase(retry - 1, doTimeout), true)
        } else {
          log.error("SDORequestMessageActor => Message init failed.")
          timeout.cancel
          context.stop(self)
        }

      case res: SegmentedSDOPayload =>
        val header = Array(sendedMsg.value(0),sendedMsg.value(1),sendedMsg.value(2),sendedMsg.value(3))
        answareTo ! dRecord.requestResponseManager(ReceivedTSSDOCanOpenMessage(sendedMsg.address,Array.concat(header,res.value)),false)
        context.stop(self)

      case send: SendRCSDOCanOpenMessage =>
        context.parent ! send
    }
    _waitInitResponse(sr.initMessage.respMaxRetry)

  }

}


abstract class SDOSegmentedMessageActor(sr: SDOSegmentRequest, retry: Int, delay: FiniteDuration) extends Actor with ActorLogging{

  import context.dispatcher

  def receive = waitSegments(retry,sr,segmentTimeout)

  def segmentTimeout =
    context.system.scheduler.scheduleOnce(delay)(self ! Timeout)

  def processSegmentedPayload(resp: ReceivedTSSDOCanOpenMessage, currentPayload: Array[Byte])

  def waitSegments(retry: Int, msg: SDOSegmentRequest, timeout: Cancellable, payload: Array[Byte] = Array()): Receive = {

    val sendedMsg = msg.getMessage
    log.debug("Sending segment")
    context.parent ! sendedMsg

    def _waitSegs: Receive = {
      case resp: ReceivedTSSDOCanOpenMessage =>
        timeout.cancel
        log.debug(s"SDOSegmentedMessageActor: Received segment => ${resp.message}")
        if (SDOFlags.isLastSegment(resp.message(0))) {
          processSegmentedPayload(resp,payload)
        } else {
          context.become(waitSegments(retry, msg.nextSegment, segmentTimeout, Array.concat(payload,resp.message.drop(1))), true)
        }

      case Timeout =>
        if (retry > 0) {
          log.warning("SDOSegmentedMessageActor => TIMEOUT in _waitSDOSegment Retry...")
          context.become(waitSegments(retry - 1, msg.nextSegment, segmentTimeout, payload), true)
        } else {
          log.error("SDOSegmentedMessageActor => TIMEOUT in _waitSDOSegment Message segment receive failed.")
          timeout.cancel
          context.stop(self)
        }
      case _ =>
    }

    _waitSegs
  }
}


