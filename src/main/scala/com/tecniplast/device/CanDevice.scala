package com.tecniplast.device

import com.sun.jna._
import com.tecniplast.nativeInterface._
import akka.actor._

object CanDevice {
  case object CanOpened
  case object CanNotOpened
  case object CanClosed
  case object CanNotClosed

  case object CanClose

  case class CanMsgReceived(id: Long, msg: Array[Byte], flags: Int)
  case class CanMsgSend(id: Long, msg: Array[Byte], flags: Int)
}

case class CanDevice(port: Int)(dispatcher_prop: Props) extends Actor with CanLibraryActorWrapper {
  val portNumber = port
  //only for debugging purposes looks not working
  //setEcho(true)
  
  val dispatcher =
    context.actorOf(dispatcher_prop,"dispatcher")

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
    case CanDevice.CanNotOpened =>
      context.become(openCanDevice, true)
    case earlyMsg =>
      self ! earlyMsg
  }

  def operative: PartialFunction[Any,Unit] = {
    case msg: CanDevice.CanMsgReceived =>
      dispatcher ! msg
    case msg : CanDevice.CanMsgSend =>
      writeMessage(msg.id,msg.msg,msg.flags)
    case CanDevice.CanClose =>
      context.become(closeCanDevice, true)
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