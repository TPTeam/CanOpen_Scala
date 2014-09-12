package com.tecniplast.device

import com.sun.jna._
import com.tecniplast.nativeInterface._
import akka.actor._
import com.tecniplast.device.CanDevice.GetDispatcher
import com.tecniplast.device.CanDevice.RefDispatcher

object CanDevice {
  case object CanOpened
  case object CanNotOpened
  case object CanClosed
  case object CanNotClosed

  case object CanClose

  case class CanMsgReceived(id: Long, msg: Array[Byte], flags: Int)
  case class CanMsgSend(id: Long, msg: Array[Byte], flags: Int)
  
  case class FirmwareDownload(address: Int, filePath: String)
  
  case class GetDispatcher()
  case class RefDispatcher(ref: ActorRef)
}

case class CanDevice(port: Int)(dispatcher_prop: Props) 
	extends AbstractCanDevice(dispatcher_prop)
	with CanLibraryActorWrapper {
  val portNumber = port
  //only for debugging purposes looks not working
  //setEcho(true)
  
  def receive = {
    openCanDevice
  }

  def openCanDevice: PartialFunction[Any,Unit] = {
    def _openCanDevice: PartialFunction[Any,Unit] = {
    case CanDevice.CanOpened =>
      context.become(operative, true)
    case CanDevice.CanNotOpened =>
      context.become(openCanDevice, true)
    case x: GetDispatcher =>
      sender ! RefDispatcher(dispatcher)
    case earlyMsg =>
      self ! earlyMsg
  }
    
    
    openPort
    _openCanDevice
  }
  

  def operative: PartialFunction[Any,Unit] = {
    case msg: CanDevice.CanMsgReceived =>
      dispatcher ! msg
    case msg : CanDevice.CanMsgSend =>
      writeMessage(msg.id,msg.msg,msg.flags)
    case CanDevice.CanClose =>
      context.become(closeCanDevice, true)
    case x: GetDispatcher =>
      sender ! RefDispatcher(dispatcher)
  }

  def closeCanDevice: PartialFunction[Any,Unit] = {
    closePort
    _closeCanDevice
  }
  def _closeCanDevice: PartialFunction[Any,Unit] = {
  	case CanDevice.CanClosed =>
      context.stop(self)
    case CanDevice.CanNotClosed =>
      context.become(closeCanDevice, true)
  }

  override def postStop = closePort

}

trait CanLibraryActorWrapper {
  me: Actor =>
  def portNumber: Int

  import CanLibrary.BitRate._
  val bitrate = CAN_BAUD_1M

  val messageReceivedFunction: msgReceived = new msgReceived {
    def invoke(		port: Int,
                    id: Long,
                    dlc: Int,
                    flags: Int,
                    msg: Pointer) {
        // To verify if a Message can have dlc == 0
    	if (port == portNumber /*&& dlc>0*/) {
    		self ! CanDevice.CanMsgReceived(id,
    										msg.getByteArray(0, dlc)/*.getString(0).substring(0, dlc)*/,
    										flags)
    	} else
    	  throw new Exception("Library message fault")
    }
  }

  def openPort() = {
	try {
		if (CanLibrary.library.openPort(portNumber,bitrate,messageReceivedFunction)==0)
			self ! CanDevice.CanOpened
		else
			self ! CanDevice.CanNotOpened
	} catch {
	  case err : Throwable =>
	    err.printStackTrace
	    self ! CanDevice.CanNotOpened
	}
  }

  def closePort() = {
    try {
		if (CanLibrary.library.closePort(portNumber)==0)
			self ! CanDevice.CanClosed
		else
			self ! CanDevice.CanNotClosed
	} catch {
	  case err : Throwable =>
	    err.printStackTrace
	    self ! CanDevice.CanNotClosed
	}
  }

  def setEcho(enable : Boolean): Boolean = {
    (CanLibrary.library.echoPort(portNumber,enable) == 0)
  }

  final val MAX_MSG_SIZE = 8

  /*
  def writeMessage(id: Long, msg: String,flags: Int): Boolean = {
    try {
      val length = if (msg.length<MAX_MSG_SIZE) msg.length else MAX_MSG_SIZE
      val tmp_msg = new Memory(length+1)
      tmp_msg.clear()
      for {
        b <- msg.substring(0, length).toCharArray.map(x => x.asInstanceOf[Byte]).zipWithIndex
      } {
        tmp_msg.setByte(b._2,b._1)
      }
      //tmp_msg.write(0,msg.substring(0, length).toCharArray.map(x => x.asInstanceOf[Byte]),0,length+1)
      //tmp_msg.setString(0, msg.substring(0, length))

      (CanLibrary.library.writeMsg(portNumber,id,length,flags,tmp_msg) == 0)

    } catch {
      case err: Throwable =>
        err.printStackTrace
        false
    }
  }
  */
  
  def writeMessage(id: Long, msg: Array[Byte],flags: Int): Boolean = {
    try {
      val length = if (msg.length<MAX_MSG_SIZE) msg.length else MAX_MSG_SIZE
      val tmp_msg = new Memory(length+1)
      tmp_msg.clear()
      for {
        b <- msg.zipWithIndex
      } {
        tmp_msg.setByte(b._2,b._1)
      }
      //println("Writing message to library "+msg.mkString("[",",","]"))
      (CanLibrary.library.writeMsg(portNumber,id,length,flags,tmp_msg) == 0)
    } catch {
      case err: Throwable =>
        err.printStackTrace
        false
    }
  }

}


case class CanUSBDevice(serial_port: String)(dispatcher_prop: Props) 
	extends AbstractCanDevice(dispatcher_prop) {
  
  def driver =
    (context.child("serial_driver")) match {
      case Some(dr) => dr
      case _ => context.actorOf(Props(NewSerialDevice(serial_port, 921600)), "serial_driver")
    }
  
  def stopDriver = 
    context.child("serial_driver").map(context.stop(_))
    
  def receive = {
    driver
    openCanDevice
  }
  import NewSerialDeviceMsgs._
  import CanUsbCommands._

  def openCanDevice: Receive = {
    _openCanDevice
  }
  def _openCanDevice: Receive = {
    case Connected =>
      driver ! DataWrite(NOP().toMsg)
      driver ! DataWrite(NOP().toMsg)
      driver ! DataWrite(CloseChannel().toMsg)
      driver ! DataWrite(NOP().toMsg)
      driver ! DataWrite(NOP().toMsg)
      driver ! DataWrite(NOP().toMsg)
      driver ! DataWrite(SetCanBitRate(8).toMsg)
      driver ! DataWrite(OpenChannel().toMsg)
      
      context.become(operative, true)
    case Disconnected =>
      stopDriver
      context.become(openCanDevice, true)
    case x: GetDispatcher =>
      sender ! RefDispatcher(dispatcher)
    case earlyMsg =>
      self ! earlyMsg
  }

  def operative: Receive = {
    case msg: DataRead =>
      fromByteArrayToCan(msg.arr).map(dispatcher ! _)
    case msg : CanDevice.CanMsgSend =>
      driver ! DataWrite(fromCanToByteArray(msg))
    case Disconnected =>
      context.become(closeCanDevice, true)
    case x: GetDispatcher =>
      sender ! RefDispatcher(dispatcher)
  }
  
  import com.tecniplast.canopen.CanOpenFlags._
  def fromByteArrayToCan(arr: Array[Byte]) : Option[CanDevice.CanMsgReceived] = {
    CanUsbCommands(arr) match {
      case std : TransmitStdCanFrame =>
        Some(CanDevice.CanMsgReceived(std.id,std.cont,MSGTYPE_STANDARD))
      case rtr : TransmitRtrCanFrame =>
        Some(CanDevice.CanMsgReceived(rtr.id,Array(rtr.dlc.toByte),MSGTYPE_RTR))
      case _ =>
        //sometimes happens..
        //println("nothing but "+arr.map(CanUsbCommands.get2DigitsHex(_)).mkString("["," ","]"))
        None
    }
  }
  
  def fromCanToByteArray(cansend: CanDevice.CanMsgSend): Array[Byte] = {
    (cansend.flags) match {
      case MSGTYPE_STANDARD =>
        TransmitStdCanFrame(cansend.id,cansend.msg).toMsg
      case MSGTYPE_RTR =>
        TransmitRtrCanFrame(cansend.id,cansend.msg(0)).toMsg //??
      case _ => Array()
    }
  }

  def closeCanDevice: Receive = {
    stopDriver
    context.stop(self)
    doNothing
  }
  
  def doNothing: Receive = {
    case _ =>
  }

}

object CanUsbCommands {
  
  final val usefullTrails =
    List('t',
    	 'r')
    	 
  def -+ (x: Byte): Int =
    if (x < 0)
      (x & 0xFF)
    else x.toInt
    	 
  def get2DigitsHex(b: Byte): String = {
    val s = (-+(b).toHexString).toUpperCase
    if (s.length > 1)
      s
    else
      "0"+s
  }
  
  def get3DigitsHex(l: Long): String = {
    val s = (l.toInt.toHexString).toUpperCase
    if (s.length == 3)
      s
    else if (s.length == 2)
      "0"+s
    else
      "00"+s
  }
    	 
  def apply(arr: Array[Byte]): CanUsbMessage = {
    if (arr.length>0) {
      arr(0) match {
        case 't' =>
          try {
            val id = Integer.parseInt(new String(Array(arr(1),arr(2),arr(3))), 16).toLong
            val length = Integer.parseInt(arr(4).toChar.toString, 16)
            
            val content =
            (for {
              i <- 5.to(4+(length*2),2)
            } yield {
              Integer.parseInt(new String(Array(arr(i),arr(i+1))),16).toByte
            }).toArray
            
             TransmitStdCanFrame(id, content)
          } catch {
            case err : Throwable => /*err.printStackTrace();*/NOP()
          }
        case 'r' =>
          println("TODO received r message")
          NOP()
        case _ => NOP()
      }
    } else NOP()
  } 
  
  
  class CanUsbMessage(cmd: String) {
    def toMsg: Array[Byte] = {
      val ret = cmd.toCharArray().map(_.toByte).toSeq.:+(0x0D.toByte).toArray
      ret
    }
  }
  
  //send a couple of nop before starting communication
  case class NOP() extends CanUsbMessage(new String())
  
  /*
	S0 Setup 10Kbit
	S1 Setup 20Kbit
	S2 Setup 50Kbit 
	S3 Setup 100Kbit 
	S4 Setup 125Kbit  
	S5 Setup 250Kbit  
	S6 Setup 500Kbit 
	S7 Setup 800Kbit 
	S8 Setup 1Mbit 
   */
  case class SetCanBitRate(bitrate: Int) extends CanUsbMessage("S"+bitrate) {
    assert {
      bitrate >= 0
      bitrate <= 8
    }
  }
  
  case class OpenChannel() extends CanUsbMessage("O")
  case class CloseChannel() extends CanUsbMessage("C")
  
  case class TransmitStdCanFrame(id: Long, cont: Array[Byte]) 
  	extends CanUsbMessage(
  	    "t"+
  	    get3DigitsHex(id)+
  	    (cont.length).toHexString+
  	    cont.map(c => get2DigitsHex(c)).mkString) {
    assert {
      cont.length <= 8
    }
  } 
  
  case class TransmitRtrCanFrame(id: Long, dlc: Int) 
  	extends CanUsbMessage("r"+get3DigitsHex(id)+get2DigitsHex((dlc & 0xFF).toByte)) {
  } 
  
}
case class CanUsbMessage(arr: Array[Byte]) {
  
}

/*
 * CAN JNI
 */
object CanJniInterface {
  import com.tecniplast.jni._
  //Devono essere sincronizzati o posso dispatcheare in altra maniera?
  lazy val jni = new CanJni()
  
  def setDevice(device: CanJniDevice,num: Int) =
    jni.setDriver(device,num)
    
  def openPort(num: Int, bitrate: Int): Int =
    jni.openPort(num, bitrate)

  def closePort(num: Int): Int =
    jni.closePort(num)
  
  def writeMsg(num: Int ,id: Long,length: Int,flags: Int,tmp_msg: Array[Byte]): Int = 
    synchronized  {
    	jni.writeMsg(num,id,length,flags,tmp_msg)
    }

  def firmwareDownload(num: Int, address: Int, filePath: String) = 
    synchronized  {
    	jni.fwDownload( num, address,filePath)
    }
    
  val device_verbose = 
    try {
    	com.typesafe.config.ConfigFactory.load("canopen.conf").getBoolean("device.receive.verbose")
    } catch {
      case _: Throwable => false
    }
}

case class CanJniDevice(port: Int)(dispatcher_prop: Props) 
	extends AbstractCanDevice(dispatcher_prop) with
	CanJniLibraryActorWrapper {
  val portNumber = port
  //only for debugging purposes looks not working
  //setEcho(true)
  
  def receive = {
    openCanDevice
  }

  def openCanDevice: PartialFunction[Any,Unit] = {
    openPort
    _openCanDevice
  }
  def _openCanDevice: PartialFunction[Any,Unit] = {
    case CanDevice.CanOpened =>
      context.become(operative, true)
      //context.become(operative_queue(Seq()), true)
    case CanDevice.CanNotOpened =>
      context.become(openCanDevice, true)
    case x: GetDispatcher =>
      sender ! RefDispatcher(dispatcher)
    case earlyMsg =>
      self ! earlyMsg
  }

  def operative: PartialFunction[Any,Unit] = {
    case msg: CanDevice.CanMsgReceived =>
      dispatcher ! msg
    case msg : CanDevice.CanMsgSend =>
      writeMessage(msg.id,msg.msg,msg.flags)
    case msg: CanDevice.FirmwareDownload =>
      println("Downloading firmware "+msg.filePath+" on board "+msg.address)
      val res = firmwareDownload(msg.address, msg.filePath)
      println("Firmware downloaded? "+res)
    case CanDevice.CanClose =>
      context.become(closeCanDevice, true)
    case x: GetDispatcher =>
      sender ! RefDispatcher(dispatcher)
  }
  
  /*
  case class Remove(sent: CanDevice.CanMsgSend)
  def compareMsgs(x1: Array[Byte], x2: Array[Byte]): Boolean = {
    if (x1.length == 1 && x2.length == 1)
      x1(0) == x2(0)
    else if (x1.length <= 0 && x2.length <= 0)
      false
    else 
      (x1(0) == x2(0)) && compareMsgs(x1.tail,x2.tail)
  }
  def operative_queue(inWrite: Seq[CanDevice.CanMsgSend]) : Receive = {
    case msg: CanDevice.CanMsgReceived =>
      dispatcher ! msg    
    case msg : CanDevice.CanMsgSend =>
      if (inWrite.exists(written => (written.id == msg.id)))
    	  self ! msg
      else {
    	  import scala.concurrent._
    	  import duration._
    	  writeMessage(msg.id,msg.msg,msg.flags)
    	  /*context.system.scheduler.scheduleOnce(1 millis)*/
    	  self ! Remove(msg)
    	  context.become(
    		operative_queue(
              inWrite.+:(msg)), true)
      }
   case rem: Remove =>
      context.become(
          operative_queue(
              inWrite.filterNot(written => {
                written.id == rem.sent.id &&
                written.flags == rem.sent.flags &&
                compareMsgs(written.msg, rem.sent.msg)
              } )), true)
    case msg: CanDevice.FirmwareDownload =>
      println("Downloading firmware "+msg.filePath+" on board "+msg.address)
      val res = firmwareDownload(msg.address, msg.filePath)
      println("Firmware downloaded? "+res)
    case CanDevice.CanClose =>
      context.become(closeCanDevice, true)
    case x: GetDispatcher =>
      sender ! RefDispatcher(dispatcher)
  }
  */

  def closeCanDevice: PartialFunction[Any,Unit] = {
    closePort
    _closeCanDevice
  }
  def _closeCanDevice: PartialFunction[Any,Unit] = {
  	case CanDevice.CanClosed =>
      context.stop(self)
    case CanDevice.CanNotClosed =>
      context.become(closeCanDevice, true)
  }

  override def postStop = closePort

}
import com.tecniplast.jni.CanJniInterfacePojo
trait CanJniLibraryActorWrapper extends CanJniInterfacePojo {
  me: CanJniDevice =>
  def portNumber: Int

  import CanLibrary.BitRate._
  val bitrate = CAN_BAUD_1M
  
  def msgReaded(id: Long,
                dlc: Int,
                flags: Int,
                msg: Array[Byte]) = {
    //DA TESTARE
    try {
      //DEBUG
      def printMsg(arr: Array[Byte]): String = {
    		  import com.tecniplast.canopen._
    		  arr.map(a => get2DigitsHex(a)).mkString("[",",","]")
      }
      
    if (CanJniInterface.device_verbose)
    	println("DEBUG DEVICE Message readed ID: "+id+" MSG: "+printMsg(msg)+" FLAGS: "+flags+" DLC: "+dlc)
    	
    self ! CanDevice.CanMsgReceived(id,
    								msg.toSeq.slice(0, dlc).toArray,
    								flags)
    } catch {
      case err: Throwable => err.printStackTrace()
    }
  }

  def openPort() = {
	try {
	 CanJniInterface.setDevice(this, portNumber)
		if (CanJniInterface.openPort(portNumber,bitrate)==0)
			self ! CanDevice.CanOpened
		else
			self ! CanDevice.CanNotOpened
	} catch {
	  case err : Throwable =>
	    err.printStackTrace
	    self ! CanDevice.CanNotOpened
	}
  }

  def closePort() = {
    try {
	CanJniInterface.setDevice(null, portNumber)
		if (CanJniInterface.closePort(portNumber)==0)
			self ! CanDevice.CanClosed
		else
			self ! CanDevice.CanNotClosed
	} catch {
	  case err : Throwable =>
	    err.printStackTrace
	    self ! CanDevice.CanNotClosed
	}
  }

  final val MAX_MSG_SIZE = 8
 
  import scala.concurrent._
  
  def writeMessage(id: Long, msg: Array[Byte],flags: Int): Boolean = {
    try {
      val length = if (msg.length<MAX_MSG_SIZE) msg.length else MAX_MSG_SIZE
      val tmp_msg = msg.slice(0, length)
      
      //DEBUG
      def printMsg(arr: Array[Byte]): String = {
    		  import com.tecniplast.canopen._
    		  arr.map(a => get2DigitsHex(a)).mkString("[",",","]")
      }
      
      //println((new java.util.Date()).getTime()+" --> DEVICE Writing ID: "+id+" MSG: "+printMsg(msg)+" FLAGS: "+flags+" DLC: "+length)
      
     val result = CanJniInterface.writeMsg(portNumber,id,length,flags,tmp_msg)
     if (result == 0) {}
      else {
        
        println("***********************************************************")
        println("ERRORE!")
        println("il rilutato è: "+result)
        println("***********************************************************")
        
      }
      (result == 0)
    } catch {
      case err: Throwable =>
        err.printStackTrace
        false
    }
  }
  
  def firmwareDownload(address: Int, filePath: String): Boolean = {
    try {
      (CanJniInterface.firmwareDownload(portNumber, address, filePath) == 0)
    } catch {
      case err: Throwable =>
        err.printStackTrace
        false
    }
  }

}