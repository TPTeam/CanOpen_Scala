package com.tecniplast.canopen


/*
 * Stato attuale e dizionario degli oggetti
 */
object CanOpenObjectDictionary extends CanOpenObjectDictionary{}
trait CanOpenObjectDictionary {
  
  case class CanOpenDictionaryElement(
      index: Long, 
      sub_index: Int, 
      name: String,
      writable: Boolean = false
  ) {
    
    def getIndex: Array[Byte] = 
      Array((0x00FF & index),((0xFF00 & index) >> 8))
    def getSubIndex: Byte =
      sub_index
    
  }
  
  def getElement
  		(name: String)
  		(implicit index: Long, sub_index:Int)
  		: CanOpenDictionaryElement = 
    getElement(name,false)
  def getElement
  		(name: String,writable: Boolean)
  		(implicit index: Long, sub_index:Int)
  		: CanOpenDictionaryElement = 
    CanOpenDictionaryElement(index,sub_index,name,writable)

  def apply(implicit index: Long, sub_index: Int) = { 
    (index,sub_index) match {
      /*address: Int
       * GENERAL
       */
      case (0x1008,0x00) =>
        getElement("Manufacturer Device Name")
      case (0x1009,0x00) =>
        getElement("Manufacturer Hardware Version")
      case (0x100A,0x00) =>
        getElement("Manufacturer Software Version")
      /*
       * Identity
       */
      case (0x1018,s_i) =>
        (s_i) match {
          case 0x01 => getElement("Vendor Id")
          case 0x02 => getElement("Product Code")
          case 0x03 => getElement("Revision Number") 
          case 0x04 => getElement("Serial Number")
        }
    }
  }
  
}