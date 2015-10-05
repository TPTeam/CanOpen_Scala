package com.tecniplast.canopen.SDOManagement

import com.tecniplast.canopen.CanOpenDictionaryElement
import com.tecniplast.canopen.CanOpenMessages.SendRCSDOCanOpenMessage
import com.tecniplast.canopen.SDOManagement.SDO.SDOToSend
import scala.concurrent.duration._

/**
 * Created by fbignardi on 3/19/15.
 */
object CanOpenSDOMessages {

  abstract class SDORequestMessage(address: Int)(dictObj: CanOpenDictionaryElement, value: Array[Byte] = Array()){

    val initMessage: SDOInitMessage

    val segmentRequest: SDOSegmentRequest

    def getAddress = address
  }

  abstract class SDOInitMessage(address: Int)(dictObj: CanOpenDictionaryElement)
    extends SDOToSend(address)(dictObj)

  abstract class SDOSegmentRequest(address: Int)(toggle_bit: Int, dictObj: CanOpenDictionaryElement, value: Array[Byte] = Array())
    extends  SDOToSend(address)(dictObj){

    def nextSegment: SDOSegmentRequest

  }


  // SDO Upload

  class SDOUploadRequest(address: Int)(dictObj: CanOpenDictionaryElement, value: Array[Byte] = Array())
    extends SDORequestMessage(address)( dictObj, value){

    val initMessage = new SDOUploadInitRequest(address)( dictObj, value)

    val segmentRequest = new SDOUploadSegmentRequest(address)(0x00, dictObj, value)
  }

  class SDOUploadInitRequest(address: Int)(dictObj: CanOpenDictionaryElement, value: Array[Byte] = Array())
    extends  SDOInitMessage(address)(dictObj){

    override val respTimeout = 3000 milliseconds

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

  class SDOUploadSegmentRequest(address: Int)(toggle_bit: Int, dictObj: CanOpenDictionaryElement, value: Array[Byte] = Array())
    extends  SDOSegmentRequest(address)(toggle_bit,dictObj,value){

    override val respTimeout = 3000 milliseconds

    def getByte0 = (0x60 + toggle_bit).toByte

    def getMessage = SendRCSDOCanOpenMessage(
      address,
      (Array(
        getByte0) ++
        value
        ))

    def nextSegment =
      new SDOUploadSegmentRequest(address)(SDOFlags.toggle(toggle_bit), dictObj)

  }

  // SDO Download => SDODownloadRequest is composed by an init message and a segment request

  class SDODownloadRequest(address: Int)(dictObj: CanOpenDictionaryElement, value: Array[Byte] = Array())
    extends SDORequestMessage(address)( dictObj, value){

    val initMessage = new SDODownloadInitRequest(address)( dictObj, value)

    val segmentRequest = new SDODownloadSegmentRequest(address)(0x00, dictObj, value)

  }

  class SDODownloadInitRequest(address: Int)(dictObj: CanOpenDictionaryElement, value: Array[Byte] = Array())
    extends  SDOInitMessage(address)(dictObj){

    override val respTimeout = 3000 milliseconds

    def getByte0 = SDOFlags.getDownloadInitByte0(value.length)

    def isSegmented = value.length > 4

    def actualPayload =
       if(isSegmented)
         Array[Byte]()
       else
         value

    def getMessage = SendRCSDOCanOpenMessage(
      address,
      (Array(
        getByte0,
        dictObj.getIndex(0),
        dictObj.getIndex(1),
        dictObj.getSubIndex) ++
        actualPayload
        ))
  }

  class SDODownloadSegmentRequest(address: Int)(toggle_bit: Int, dictObj: CanOpenDictionaryElement, value: Array[Byte] = Array())
    extends  SDOSegmentRequest(address)(toggle_bit,dictObj,value){

    override val respTimeout = 3000 milliseconds

    def getByte0 = (0x00 + toggle_bit + isLastSegment + SDOFlags.getDownlaodSegmentedFreeBytes(getCurrentPayload.length)).toByte

    def getMessage = SendRCSDOCanOpenMessage(
      address,
      (Array(
        getByte0) ++
        getCurrentPayload
        ))

    def getCurrentPayload =
      if(value.length > 7)
        value.dropRight(value.length - 7)
      else
        value

    def getNextPayload =
      if(value.length > 7)
        value.drop(7)
      else
        Array[Byte]()

    def isLastSegment =
      if(value.length > 7)
        0x00
      else
        0x01

    def nextSegment =
      new SDODownloadSegmentRequest(address)(SDOFlags.toggle(toggle_bit), dictObj, getNextPayload)

  }

}
