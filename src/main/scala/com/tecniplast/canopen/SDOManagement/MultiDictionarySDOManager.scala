package com.tecniplast.canopen.SDOManagement

import akka.actor._
import com.tecniplast.canopen.CanOpenMessages.{ReceivedCanOpenMessage, ReceivedTSSDOCanOpenMessage, SendRCSDOCanOpenMessage}
import com.tecniplast.canopen.SDOManagement.CanOpenSDOMessages._
import com.tecniplast.canopen.SDOManagement.legacy.{SDOBlockRequestActor, SDORequestActor}
import com.tecniplast.canopen._
import scala.concurrent.duration._

/**
 * Created by fbignardi on 3/17/15.
 */

abstract class DictionaryRecordEntry

case class DictionaryRecord(dictionary: CanOpenObjectDictionary,
                            requestResponseManager: SDOManagement.SDO.CanOpenSDORequestResponseInterface) extends DictionaryRecordEntry

case class LegacyDictionaryRecord(dictionary: CanOpenObjectDictionary,
                            requestResponseManager: SDOManagement.SDO.LegacySDORequestResponseInterface,
                            commandResponseManager: SDOManagement.SDO.LegacySDOCommandResponseInterface) extends DictionaryRecordEntry



abstract class MultiDictionarySDOManager extends SDOManager with SDORequestMessageResolver with ActorLogging {

  import com.tecniplast.canopen.SDOManagement.SDO._
  import context.dispatcher

  val legacyDictionaryRecord: LegacyDictionaryRecord
  val dictionaryRecord: DictionaryRecord

  def receive = {

    case sr: SDORequestMessage =>
      val actualSender = sender.path
      val actName = getNameFromMsg(sr.initMessage)

      sr match {
        case x: SDOUploadRequest =>
          context.child(actName).map(c => {
            context.stop(c)
            log.warning(s"MultiDictionarySDOManager => killed still alive child for SDO upload for board ${x.getAddress}")
          })
          try {
            context.actorOf(Props(SDOUploadRequestMessageActor(x, actualSender, sr.initMessage.respTimeout, dictionaryRecord)), actName)
          } catch {
            case ex: InvalidActorNameException => {
              context.system.scheduler.scheduleOnce(50 milliseconds, self, sr)
              log.error(s"MultiDictionarySDOManager => InvalidActorNameException while creating child for SDO upload for board ${x.getAddress}!")
            }
            case any: Throwable =>
              log.error(s"MultiDictionarySDOManager => thrown unmanaged $any while creating child for SDO upload for board ${x.getAddress}!")
          }

        case y: SDODownloadRequest =>
          context.child(actName).map(c => {
            context.stop(c)
            log.warning(s"MultiDictionarySDOManager => killed still alive child for SDO download for board ${y.getAddress}")
          })
          try {
            context.actorOf(
            Props(SDODownloadRequestMessageActor(y, actualSender, sr.initMessage.respTimeout, dictionaryRecord)), actName)
          } catch {
            case ex: InvalidActorNameException => {
              context.system.scheduler.scheduleOnce(50 milliseconds, self, sr)
              log.error(s"MultiDictionarySDOManager => InvalidActorNameException while creating child for SDO download for board ${y.getAddress}!")
            }
            case any: Throwable =>
              log.error(s"MultiDictionarySDOManager => thrown unmanaged $any while creating child for SDO download for board ${y.getAddress}!")
          }
      }

    case rq: SDOToSend =>
      val actualSender = sender.path
      val actName = getNameFromMsg(rq)
      val blockName = getBlockName(rq)
      if (context.child(actName).isEmpty && context.child(blockName).isEmpty) {
        rq match {
          case r: SDORequest =>
            context.actorOf(
              Props(
                SDORequestActor(r, actualSender, 0x40, r.respTimeout, legacyDictionaryRecord)), actName)
          case c: SDOCommand =>
            context.actorOf(
              Props(
                SDORequestActor(c, actualSender, 0x60, c.respTimeout, legacyDictionaryRecord)), actName)
          case b: SDOBlockRequest =>
            context.actorOf(
              Props(
                SDOBlockRequestActor(b, actualSender, 0x40, b.respTimeout, legacyDictionaryRecord)), blockName)
          case any =>
            log.warning("SDO manager received from children unmanaged " + any)
        }

      } else {
        //trying later
        context.system.scheduler.scheduleOnce(10 millis)({
          for {
            realSend <- context.system.actorSelection(actualSender).resolveOne(1 second)
          } yield {
            self.tell(rq, realSend)
          }
        })
      }
    case resp: ReceivedCanOpenMessage =>
      if (SDOFlags.isSegmentResponse(resp.getMessage(0))) {
        val x = SDOFlags.getSegmentRequestByte0(resp.getMessage(0))
        context.actorSelection(self.path + "/*/" + getSegmentActorName(resp.getAddress, x)) ! resp

      } else if (SDOFlags.isAbortSegmentResponse(resp.getMessage(0))) {
        context.child(getNameFromMsg(resp)).map(x => x ! SDOTransferAborted)
      } else {
        val blockOpt = (context.child(getBlockName(resp)))
        if (blockOpt.isDefined) {
          blockOpt.map(_ ! resp)
        } else {
          val childOpt = context.child(getNameFromMsg(resp))

          if (childOpt.isDefined) {
            childOpt.map(_ ! resp)
          }
          else {
            log.warning(s"MultiDictionarySDOManager Child not found: board => ${resp.getAddress} message => ${ resp.getMessage.map(x => get2DigitsHex(x)).fold("")((x,y)=> x+" "+y) }!")
          }
        }
      }

    case send: SendRCSDOCanOpenMessage =>
      context.parent ! send
    case any =>
      log.warning("SDO manager received unmanaged " + any + " " + sender.path)
  }
}


case class SDODownloadRequestMessageActor(sr: SDODownloadRequest,_answareTo: ActorPath, delay: FiniteDuration, dRecord: DictionaryRecord)
  extends SDORequestMessageManager(sr=sr,_answareTo=_answareTo, delay=delay, dRecord=dRecord){

    def getAndSendInitMessage() = context.parent ! sendedMsg


    def manageResponse(resp: ReceivedTSSDOCanOpenMessage) = {

      if(sr.initMessage.isSegmented){
          val actName = getSegmentActorName(sendedMsg.address,sr.segmentRequest.getByte0)
          println(s"SDODownloadRequestMessageActor => ReceivedTSSDOCanOpenMessage Segmented message received creating ${actName}")
          context.actorOf(
            Props(SDODownloadSegmentedMessageActor(sr.segmentRequest,sr.initMessage.respMaxRetry,sr.initMessage.respTimeout)),actName)
      }else{
        answareTo ! dRecord.requestResponseManager(resp,true)
        context.stop(self)
      }

    }

    case class SDODownloadSegmentedMessageActor(sr: SDOSegmentRequest,retry: Int, delay: FiniteDuration)
      extends SDOSegmentedMessageActor(sr: SDOSegmentRequest,retry: Int, delay: FiniteDuration){

      def processSegmentedPayload(resp: ReceivedTSSDOCanOpenMessage, currentPayload: Array[Byte]) = {
        context.parent ! SegmentedSDOPayload(resp.message.drop(1))
        context.stop(self)
      }

    }

  }

case class SDOUploadRequestMessageActor(sr: SDORequestMessage,_answareTo: ActorPath, delay: FiniteDuration, dRecord: DictionaryRecord)
  extends SDORequestMessageManager(sr=sr,_answareTo=_answareTo, delay=delay, dRecord=dRecord){

      def getAndSendInitMessage() = context.parent ! sendedMsg

      def manageResponse(resp: ReceivedTSSDOCanOpenMessage) = {
        val msgFlags = SDOFlags(resp.message(0))
        if(msgFlags.segmented){
          val segReq = sr.segmentRequest
          val actName = getSegmentActorName(sendedMsg.address,segReq.getByte0)
          context.actorOf(Props(SDOUploadSegmentedMessageActor(segReq,sr.initMessage.respMaxRetry,sr.segmentRequest.respTimeout)),actName)
        }else{
          answareTo ! dRecord.requestResponseManager(resp,false)
          context.stop(self)
        }
      }


      case class SDOUploadSegmentedMessageActor(sr: SDOSegmentRequest,retry: Int, delay: FiniteDuration)
          extends SDOSegmentedMessageActor(sr: SDOSegmentRequest,retry: Int, delay: FiniteDuration){

        def processSegmentedPayload(resp: ReceivedTSSDOCanOpenMessage, currentPayload: Array[Byte]) = {
          val n = SDOFlags.getSegmentedFreeBytes(resp.message(0))
          val fp = Array.concat(currentPayload,resp.message.drop(1).dropRight(n))
          context.parent ! SegmentedSDOPayload(fp)
          context.stop(self)
        }

      }

  }

