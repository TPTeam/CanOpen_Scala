package com.tecniplast.canopen


/*
 * Stato attuale e dizionario degli oggetti
 */

//please implement yours
//object CanOpenObjectDictionary extends CanOpenObjectDictionary{}
  case class CanOpenDictionaryElement(
      index: Long, 
      sub_index: Int, 
      name: String,
      download: Boolean
  ) {
    
    def getIndex: Array[Byte] = 
      Array((0x00FF & index),((0xFF00 & index) >> 8))
    def getSubIndex: Byte =
      sub_index
    
    override def equals(x: Any) =
      x match {
      	case code: CanOpenDictionaryElement => 
      	  (index == code.index && sub_index == code.sub_index)
      	case _ => false
      }
  }
trait CanOpenObjectDictionary {
  
  //def getElement(name: String)(implicit index: Long, sub_index:Int): CanOpenDictionaryElement =
  //  getElement(name,download)

  def getElement(name: String,download: Boolean)(implicit index: Long, sub_index:Int): CanOpenDictionaryElement =
    CanOpenDictionaryElement(index,sub_index,name,download)

  def apply(implicit index: Long, sub_index: Int, download: Boolean) = {
    //println(s"CanOpenObjectDictionary Apply! ${index.toString} ${sub_index.toString} ${download.toString}")
    (index,sub_index) match {
      /*address: Int
       * GENERAL
       */
      case (0x1008,0x00) =>
        getElement("Manufacturer Device Name",download)
      case (0x1009,0x00) =>
        getElement("Manufacturer Hardware Version",download)
      case (0x100A,0x00) =>
        getElement("Manufacturer Software Version",download)
      /*
       * Identity
       */
      case (0x1018,s_i) =>
        (s_i) match {
          case 0x01 => getElement("Vendor Id",download)
          case 0x02 => getElement("Product Code",download)
          case 0x03 => getElement("Revision Number",download)
          case 0x04 => getElement("Serial Number",download)
        }
    }
  }
  
}