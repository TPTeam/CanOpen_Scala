package com.tecniplast.canopen.SDOManagement

import com.tecniplast.canopen.CanOpenMessages.{ReceivedTSSDOCanOpenMessage, SendRCSDOCanOpenMessage}
import com.tecniplast.canopen.{CanOpenDictionaryElement, CanOpenObjectDictionary}
import scala.concurrent.duration._

/**
 * Created by fbignardi on 3/19/15.
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
    val respMaxRetry = 2

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
      (0x20.toByte + 0x03.toByte).toByte

    //probabilmente baco sul protocollo Microchip
    //+ (data_max_len-value.length << 2)
    val completeArr: Array[Byte] = Array(0x00.toByte,0x00.toByte,0x00.toByte,0x00.toByte)
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
      0x40 & 0x01 & (data_max_len-value.length << 2)
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


  abstract class CanOpenSDORequestResponseInterface {

    def apply(resp: SendRCSDOCanOpenMessage, download: Boolean)(implicit dict: CanOpenObjectDictionary): SDORequestResponseFault

    def apply(resp: ReceivedTSSDOCanOpenMessage, download: Boolean)(implicit dict: CanOpenObjectDictionary): SDORequestResponseOK

  }

  abstract class LegacySDORequestResponseInterface{

    def apply(resp: SendRCSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDORequestResponseFault

    def apply(resp: ReceivedTSSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDORequestResponseOK

    def apply(sended: SendRCSDOCanOpenMessage,resp: Seq[ReceivedTSSDOCanOpenMessage])(implicit dict: CanOpenObjectDictionary): SDORequestResponseOK

  }

  abstract class LegacySDOCommandResponseInterface {

    def apply(resp: SendRCSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDOCommandResponseFault

    def apply(resp: ReceivedTSSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDOCommandResponseOK

  }

  object LegacySDORequestResponse extends LegacySDORequestResponseInterface {

    def apply(resp: SendRCSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDORequestResponseFault = {
      new SDORequestResponseFault(resp.address)(
        dict.apply(
          (resp.value(2) << 8) + resp.value(1),resp.value(3),false))
    }

    def apply(resp: ReceivedTSSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDORequestResponseOK = {
      new SDORequestResponseOK(resp.address)(
        dict.apply(
          (resp.message(2) << 8) + resp.message(1),resp.message(3),false),
        resp.message.drop(4))
    }

    def apply(sended: SendRCSDOCanOpenMessage,resp: Seq[ReceivedTSSDOCanOpenMessage])(implicit dict: CanOpenObjectDictionary): SDORequestResponseOK = {
      def aggregateBody(in: Seq[ReceivedTSSDOCanOpenMessage], part: Array[Byte]): Array[Byte] =
        if (in.isEmpty) part
        else aggregateBody(in.tail, part.++:(in.head.message.drop(1)))


      new SDORequestResponseOK(sended.address)(
        dict.apply(
          (sended.value(2) << 8) + sended.value(1),sended.value(3),false),
        aggregateBody(resp,Array()))
    }
  }

  object LegacySDOCommandResponse extends LegacySDOCommandResponseInterface {

    def apply(resp: SendRCSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDOCommandResponseFault = {
      new SDOCommandResponseFault(resp.address)(
        dict.apply(
          (resp.value(2) << 8) + resp.value(1),resp.value(3),false))
    }

    def apply(resp: ReceivedTSSDOCanOpenMessage)(implicit dict: CanOpenObjectDictionary): SDOCommandResponseOK = {
      new SDOCommandResponseOK(resp.address)(
        dict.apply(
          (resp.message(2) << 8) + resp.message(1),resp.message(3),false))
    }

  }
}
