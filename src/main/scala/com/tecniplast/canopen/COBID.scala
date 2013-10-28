package com.tecniplast.canopen


object CanOpenMsgType {
  
  def getAddress(id: Long) =
    (id & 0x7F).toInt
  def getFunction(id: Long) =
    (id >> 7).toInt
  
  final val NMT_CMD				= 0x00
  final val EMERGENCY_OR_SYNC 	= 0x01
  final val TIME_STAMP 			= 0x02
  final val TPDO1 				= 0x03
  final val RPDO1 				= 0x04
  final val TPDO2 				= 0x05
  final val RPDO2 				= 0x06
  final val TPDO3 				= 0x07
  final val RPDO3 				= 0x08
  final val TPDO4 				= 0x09
  final val RPDO4 				= 0x0A
  final val TSSDO  				= 0x0B		// domain download 2 --> write to devices
  final val RCSDO  				= 0x0C		// domain upload 4 --> read from devices
  final val NMT_ERROR 			= 0x0D
  final val NMT_HEARTBEAT 		= 0x0E
  
  abstract class CanOpenMsgId{
	  val function: Int
	  val address: Int
	  lazy val cobid = COBID(address,function)
  }
  
  trait CanOpenEmergency {
    self: CanOpenMsgId =>
      val function = EMERGENCY_OR_SYNC
  }
  
  case object CanOpenSYNC extends CanOpenMsgId {
	  val function = EMERGENCY_OR_SYNC
	  val address = 0x00
  }
  
  case object CanOpenNMT extends CanOpenMsgId {
	  val function = NMT_CMD
	  val address = 0x00
  }
  
  trait CanOpenNMTHeartBeat {
    self: CanOpenMsgId =>
      val function = NMT_HEARTBEAT
  }
  case class CanOpenIdNMT(_address: Int) extends CanOpenMsgId with CanOpenNMTHeartBeat {
    val address = _address
  }
  
  case object CanOpenTimeStamp extends CanOpenMsgId {
	  val function = TIME_STAMP
	  val address = 0x00
  }
  
  trait CanOpenTPDO1 {
    self: CanOpenMsgId =>
      val function = TPDO1
  }
  case class CanOpenIdTPDO1(_address: Int) extends CanOpenMsgId with CanOpenTPDO1 {
    val address = _address
  }
  trait CanOpenRPDO1 {
    self: CanOpenMsgId =>
      val function = RPDO1
  }
  case class CanOpenIdRPDO1(_address: Int) extends CanOpenMsgId with CanOpenRPDO1 {
    val address = _address
  }
  
  trait CanOpenTPDO2 {
    self: CanOpenMsgId =>
      val function = TPDO2
  }
  case class CanOpenIdTPDO2(_address: Int) extends CanOpenMsgId with CanOpenTPDO2 {
    val address = _address
  }
  trait CanOpenRPDO2 {
    self: CanOpenMsgId =>
      val function = RPDO2
  }
  case class CanOpenIdRPDO2(_address: Int) extends CanOpenMsgId with CanOpenRPDO2 {
    val address = _address
  }
  
  trait CanOpenTPDO3 {
    self: CanOpenMsgId =>
      val function = TPDO3
  }
  case class CanOpenIdTPDO3(_address: Int) extends CanOpenMsgId with CanOpenTPDO3 {
    val address = _address
  }
  trait CanOpenRPDO3 {
    self: CanOpenMsgId =>
      val function = RPDO3
  }
  case class CanOpenIdRPDO3(_address: Int) extends CanOpenMsgId with CanOpenRPDO3 {
    val address = _address
  }
  
  trait CanOpenTPDO4 {
    self: CanOpenMsgId =>
      val function = TPDO4
  }
  case class CanOpenIdTPDO4(_address: Int) extends CanOpenMsgId with CanOpenTPDO4 {
    val address = _address
  }
  trait CanOpenRPDO4 {
    self: CanOpenMsgId =>
      val function = RPDO4
  }
  case class CanOpenIdRPDO4(_address: Int) extends CanOpenMsgId with CanOpenRPDO4 {
    val address = _address
  }
  
  /*
   * TPDO aggregator
   */
  case class CanOpenIdTPDO(number: Int, _address: Int) extends CanOpenMsgId {
    assert (number > 0)
    assert (number < 5)
    
    val options = Seq(
    				CanOpenIdTPDO1(_address),
    				CanOpenIdTPDO2(_address),
    				CanOpenIdTPDO3(_address),
    				CanOpenIdTPDO4(_address)
    				)
    
    val index = number-1
    
    val function = options(index).function
    val address = options(index).address
    
  }
  case class CanOpenIdRPDO(number: Int, _address: Int) extends CanOpenMsgId {
    assert (number > 0)
    assert (number < 5)
    
    val options = Seq(
    				CanOpenIdRPDO1(_address),
    				CanOpenIdRPDO2(_address),
    				CanOpenIdRPDO3(_address),
    				CanOpenIdRPDO4(_address)
    				)
    
    val index = number-1
    
    val function = options(index).function
    val address = options(index).address
    
  }
  
  trait CanOpenTSSDO {
    self: CanOpenMsgId =>
      val function = TSSDO
  }
  case class CanOpenIdTSSDO(_address: Int) extends CanOpenMsgId with CanOpenTSSDO {
    val address = _address
  }
  trait CanOpenRCSDO {
    self: CanOpenMsgId =>
      val function = RCSDO
  }
  case class CanOpenIdRCSDO(_address: Int) extends CanOpenMsgId with CanOpenRCSDO {
    val address = _address
  }
  
  trait CanOpenNMTError {
    self: CanOpenMsgId =>
      val function = NMT_ERROR
  }
  
}

/*
 * CANOpen 2.0A --> 11 bit --> ONLY ONE IMPLEMENTED
 * CANOpen 2.0B --> 29 bit
 * 
 *  4 or 18 bit function 
 *  7 bit address
 */
case class COBID(address: Int, function: Int) {
  val addressLimit = 0x7F
  val functionLimit = 
    if (!CanOpenManager.protocolVersion)
      0xF
    else
      0x3FFFF
  
  
  assert (address <= 10)
  assert (function <= functionLimit)
  
  
  def getId: Long = {
    //println("-------> Msg Id: "+function+" "+address+" --> "+((function << 7) | address))
    ((function << 7) | address)
    }
    
   	
}