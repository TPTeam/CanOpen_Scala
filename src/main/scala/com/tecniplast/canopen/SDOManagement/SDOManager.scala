package com.tecniplast.canopen.SDOManagement

import akka.actor.Actor

/**
 * Created by fbignardi on 3/23/15.
 */

object SDOFlags {

  // SDO Byte0 bits definitions

  // the second lsb or a server response indicates if it's expedited or segmented
  private final val expedited_mask = 0x02
  private final val segmented_mask = 0x00

  // the first lsb indicates if the the data size is indicated or not
  private final val data_size_indicated_mask = 0x01

  // EXPEDITED => the fourth and third lsb indicate how many bytes of payload are free
  private final val data_exp_size_mask = 0x0C
  private final val data_exp_size_full = 0x00
  private final val data_exp_size_one_byte_empty = 0x04
  private final val data_exp_size_two_bytes_empty = 0x08
  private final val data_exp_size_three_bytes_empty = 0x0C

  // SEGMENT => the second, third and fourth lsb indicate how many bytes of payload are free
  private final val data_seg_size_mask = 0x0E
  private final val data_seg_size_full = 0x00
  private final val data_seg_size_one_byte_empty = 0x02
  private final val data_seg_size_two_bytes_empty = 0x04
  private final val data_seg_size_three_bytes_empty = 0x06
  private final val data_seg_size_four_bytes_empty = 0x08
  private final val data_seg_size_five_bytes_empty = 0x0A
  private final val data_seg_size_six_bytes_empty = 0x0C
  private final val data_seg_size_seven_bytes_empty = 0x0E

  // Server response => the three msb indicates the SDO msg type
  private final val segment_response_mask = 0xE0
  private final val sdo_upload_init_response = 0x40
  private final val sdo_download_init_response = 0x60
  private final val sdo_abort_transfer_byte0 = 0x80

  // during a segmented SDO transfer the fifth lsb is toggle bit,
  // setted equal 0 for the first segment request and then toggled
  private final val segment_toggle_bit_mask = 0x10

  // from server the first lsb indicates if this is the last segment
  private final val last_segment_mask = 0x01

  // without toggle bit
  private final val upload_segment_request = 0x60
  private final val upload_segment_response = 0x00
  private final val download_segment_request = 0x00
  private final val download_segment_response = 0x20


  case class SDOFlags(segmented: Boolean,
                      with_data_size: Boolean = false,
                      n_bytes_data: Int)

   private def compareWithMask(byte: Byte, mask: Int): Boolean =
     if((byte & mask) == mask)
       true
     else
       false

   def isSDOResponse(byte0: Byte) =
     ((byte0 & segment_response_mask) == sdo_upload_init_response) || ((byte0 & segment_response_mask) == sdo_download_init_response)

   private def isExpedited(byte0: Byte) =
     (byte0 & expedited_mask) == expedited_mask

   private def isSegmented(byte0: Byte) =
     (byte0 & expedited_mask) == segmented_mask

   private def isDataSizeIndicated(byte0: Byte) =
     compareWithMask(byte0,data_size_indicated_mask)

   def getDownloadInitByte0(length: Int) = {
      if(length > 4)
        0x20.toByte
      else
        length match {
          case 1 => 0x2F.toByte
          case 2 => 0x2B.toByte
          case 3 => 0x27.toByte
          case 4 => 0x23.toByte
        }
   }


   def getExpeditedFreeBytes(byte0: Byte): Int = {
     val d = (byte0 & data_exp_size_mask)
     d match {
       case `data_exp_size_full` => 0

       case `data_exp_size_one_byte_empty` => 1

       case `data_exp_size_two_bytes_empty` => 2

       case `data_exp_size_three_bytes_empty` => 3
     }
   }

  def getSegmentedFreeBytes(byte0: Byte): Int = {
    val d = (byte0 & data_seg_size_mask)
    d match {
      case `data_seg_size_full` => 0

      case `data_seg_size_one_byte_empty` => 1

      case `data_seg_size_two_bytes_empty` => 2

      case `data_seg_size_three_bytes_empty` => 3

      case `data_seg_size_four_bytes_empty` => 4

      case `data_seg_size_five_bytes_empty` => 5

      case `data_seg_size_six_bytes_empty` => 6

      case `data_seg_size_seven_bytes_empty` => 7
    }

  }

  def getDownlaodSegmentedFreeBytes(length: Int) = {
    length match {
      case 7 => data_seg_size_full

      case 6 => data_seg_size_one_byte_empty

      case 5 => data_seg_size_two_bytes_empty

      case 4 => data_seg_size_three_bytes_empty

      case 3 => data_seg_size_four_bytes_empty

      case 2 => data_seg_size_five_bytes_empty

      case 1 => data_seg_size_six_bytes_empty

      case 0 => data_seg_size_seven_bytes_empty
    }


  }

  def toggle(toggle_bit: Int) = {
    toggle_bit match {
      case 0x00 => 0x10
      case 0x10 => 0x00
      case _ => {
        println(s"SDOFlags => we don't have a single bit! => $toggle_bit")
        0x00
      }
    }
  }

  def isAbortSegmentResponse(byte0: Byte) =
    (byte0 & segment_response_mask) == sdo_abort_transfer_byte0

  def isSegmentResponse(byte0: Byte) =
    isDownloadSegmentResponse(byte0) || isUploadSegmentResponse(byte0)

  def isDownloadSegmentResponse(byte0: Byte) =
    (byte0 & segment_response_mask) == download_segment_response

  def isUploadSegmentResponse(byte0: Byte) =
    (byte0 & segment_response_mask) == upload_segment_response

  def getSegmentRequestByte0(byte0: Byte) =
   if(isDownloadSegmentResponse(byte0))
     download_segment_request
   else
     upload_segment_request


  def isLastSegment(byte0: Byte) =
     compareWithMask(byte0,last_segment_mask)


  def apply(byte0: Byte, expedited: Boolean = true): SDOFlags =
    SDOFlags(isSegmented(byte0),
             isDataSizeIndicated(byte0),
             if(expedited) getExpeditedFreeBytes(byte0)
             else getSegmentedFreeBytes(byte0))

}


abstract class SDOManager extends Actor
