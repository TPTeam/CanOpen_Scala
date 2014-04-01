package com.tecniplast.canopen

import akka.actor._
import com.typesafe.config._
import com.tecniplast.device.CanDevice._
import CanOpenMessages._
import com.tecniplast.canopen._
import com.tecniplast.canopen.CanOpenDispatcher._
/*
 * This actor register ActorRef consumers with type and dipatch messages
 * "per type"
 * 
 */
object CanOpenDispatcher {
  case class GetPDOManager()
  case class GetSDOManager()
  case class GetNMTManager()
  
  case class RefPDOManager(ref: ActorRef)
  case class RefSDOManager(ref: ActorRef)
  case class RefNMTManager(ref: ActorRef)
}

case class CanOpenDispatcher(
		pdo_manager_type: Props,
		sdo_manager_type: Props,
		nmt_manager_type: Props
		) extends Actor {
  
  val receive_verbose = 
    try {
    	ConfigFactory.load("canopen.conf").getBoolean("dispatcher.receive.verbose")
    } catch {
      case _: Throwable => false
    }
  val send_verbose = 
    try {
    	ConfigFactory.load("canopen.conf").getBoolean("dispatcher.send.verbose")
    } catch {
      case _: Throwable => false
    }

  
  val pdo_manager =
    context.actorOf(pdo_manager_type,"pdo_manager")
    
    //
  val sdo_manager =
    context.actorOf(sdo_manager_type.withDispatcher("sdo-prio-dispatcher"),"sdo_manager")
    
  val nmt_manager =
    context.actorOf(nmt_manager_type,"nmt_manager")
    
  def printMsg(arr: Array[Byte]): String = {
    arr.map(a => get2DigitsHex(a)).mkString("[",",","]")
  }
  
  def receive = {
    case msg: SendCanOpenMessage =>
      if (send_verbose)
    	  println(self.path+" sending CAN RAW "+get4DigitsHex(msg.toCan.id)+" "+printMsg(msg.toCan.msg)+" "+msg.toCan.flags)
      context.parent ! msg.toCan
      
    case CanMsgReceived(id, msg, flags) =>
      if (receive_verbose)
    	  println(self.path+" received CAN RAW "+get4DigitsHex(id)+" "+printMsg(msg)+" "+flags)
      RecivedCanOpenMessage(id,msg,flags) match {
        case tpdo : ReceivedTPDO =>
          //if (verbose)
          //  println(self.path+" received TPDO "+get4DigitsHex(id)+" "+printMsg(msg))
          pdo_manager ! tpdo
        case rpdo : ReceivedRPDO =>
          //if (verbose)
          //	  println(self.path+" received RPDO "+get4DigitsHex(id)+" "+printMsg(msg))
          pdo_manager ! rpdo
        case sdo : ReceivedSDO =>
          //println(self.path+/*" "+new java.util.Date().getTime()+*/" received SDO "+get4DigitsHex(id)+" "+printMsg(msg))
          sdo_manager ! sdo
        case nmt : ReceivedNMT =>
          //println(self.path+" received NMT "+get4DigitsHex(id)+" "+printMsg(msg))
          nmt_manager ! nmt
        case sync : ReceivedSYNC =>
          //if (verbose)
          //  println(self.path+" received SYNC "+get4DigitsHex(id)+" "+printMsg(msg))
        case emergency: ReceivedEMERGENCY =>
          //if (verbose)
          //  println(self.path+" received EMERGENCY "+get4DigitsHex(id)+" "+printMsg(msg))
        case timestamp: ReceivedTIMESTAMP =>
          //if (verbose)
          //	  println(self.path+"received TimeStamp "+get4DigitsHex(id)+" "+printMsg(msg))
      }
    case CanClose => 
      context.parent ! CanClose
      context.children.foreach(c => context.stop(c))
      context.stop(self)
    case x: GetPDOManager =>
      sender ! RefPDOManager(pdo_manager)
    case x: GetSDOManager =>
      sender ! RefSDOManager(sdo_manager)
    case x: GetNMTManager =>
      sender ! RefNMTManager(nmt_manager)
    //To implement a Manager Setter / Getter  
    case any => println("Can dispatcher received unknown message "+any+" from "+sender.path.toString)
  }
  
}