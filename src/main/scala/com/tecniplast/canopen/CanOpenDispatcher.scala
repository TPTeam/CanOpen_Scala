package com.tecniplast.canopen

import akka.actor._
import com.tecniplast.canopen.mailboxes.SDO_PrioMailbox
import com.typesafe.config._
import com.tecniplast.device.CanDevice._
import CanOpenMessages._
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

class CanOpenDispatcher(
		pdo_manager_type: Props,
		sdo_manager_type: Props,
		nmt_manager_type: Props
		) extends Actor with ActorLogging{
  
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
    context.actorOf(pdo_manager_type.withDispatcher("pdo-prio-dispatcher"),"pdo_manager")

  val sdo_manager =
    context.actorOf(sdo_manager_type.withDispatcher("sdo-prio-dispatcher"),"sdo_manager")
    
  val nmt_manager =
    context.actorOf(nmt_manager_type,"nmt_manager")
    
  def printMsg(arr: Array[Byte]): String = {
    arr.map(a => get2DigitsHex(a)).mkString("[",",","]")
  }
  
  def receive = {
    managersGetter orElse {
    	case msg: SendCanOpenMessage =>
    		if (send_verbose)
    			println(new java.util.Date+" "+self.path+" sending CAN RAW "+get4DigitsHex(msg.toCan.id)+" "+printMsg(msg.toCan.msg)+" "+msg.toCan.flags)
    		context.parent ! msg.toCan
      
    	case CanMsgReceived(id, msg, flags) =>
    		if (receive_verbose)
    			println(new java.util.Date+" "+self.path+" received CAN RAW "+get4DigitsHex(id)+" "+printMsg(msg)+" "+flags)
    		RecivedCanOpenMessage(id,msg,flags) match {
    			case tpdo : ReceivedTPDO =>
    				pdo_manager ! tpdo
    			case rpdo : ReceivedRPDO =>
    				pdo_manager ! rpdo
    			case sdo : ReceivedSDO =>
    				sdo_manager ! sdo
    			case nmt : ReceivedNMT =>
    				nmt_manager ! nmt
    			case sync : ReceivedSYNC =>
    			case emergency: ReceivedEMERGENCY =>
    			case timestamp: ReceivedTIMESTAMP =>
          case any => log.warning("unknown type!!!")
    		}
    	case CanClose => 
    		context.parent ! CanClose
    		context.children.foreach(c => context.stop(c))
    		context.stop(self)
    }
  }
  
  def managersGetter: Receive = {
    case x: GetPDOManager =>
      sender ! RefPDOManager(pdo_manager)
    case x: GetSDOManager =>
      sender ! RefSDOManager(sdo_manager)
    case x: GetNMTManager =>
      sender ! RefNMTManager(nmt_manager)
  }
  
}