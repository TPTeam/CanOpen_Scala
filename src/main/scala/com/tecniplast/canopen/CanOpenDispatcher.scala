package com.tecniplast.canopen

import akka.actor._
import com.typesafe.config._
import com.tecniplast.device.CanDevice._
import CanOpenMessages._
import com.tecniplast.canopen._
/*
 * This actor register ActorRef consumers with type and dipatch messages
 * "per type"
 * 
 */
case class CanOpenDispatcher() extends Actor {
  
  val pdo_manager =
    context.actorOf(Props(PDOManager),"pdo_manager")
    
  val sdo_manager =
    context.actorOf(Props(SDOManager).withDispatcher("sdo-prio-dispatcher"),"sdo_manager")
 
    
  val nmt_manager =
    context.actorOf(Props(PerNodeNMTManager),"nmt_manager")
   
  def get2DigitsHex(b: Byte): String = {
    val s = (-+(b).toHexString).toUpperCase
    if (s.length > 1)
      "0x"+s
    else
      "0x0"+s
  }
  
  def get4DigitsHex(x: Long): String = {
    val s = (x.toHexString).toUpperCase
    if (s.length > 3)
      "0x"+s
    else if (s.length == 3)
      "0x0"+s
    else if (s.length == 2)
      "0x00"+s
    else if (s.length == 1)
      "0x000"+s
    else
      "0x0000"+s
  }
    
  def printMsg(arr: Array[Byte]): String = {
    arr.map(a => get2DigitsHex(a)).mkString("[",",","]")
  }
  
  def receive = {
    case msg: SendCanOpenMessage =>
      context.parent ! msg.toCan
      
    case CanMsgReceived(id, msg, flags) =>
      //println(self.path+" received CAN RAW "+get4DigitsHex(id)+" "+printMsg(msg)+" "+flags)
      RecivedCanOpenMessage(id,msg,flags) match {
        case tpdo : ReceivedTPDO =>
          println(self.path+" received TPDO "+get4DigitsHex(id)+" "+printMsg(msg))
          pdo_manager ! tpdo
        case rpdo : ReceivedRPDO =>
          println(self.path+" received RPDO "+get4DigitsHex(id)+" "+printMsg(msg))
          pdo_manager ! rpdo
        case sdo : ReceivedSDO =>
          println(self.path+" "+new java.util.Date().getTime()+" received SDO "+get4DigitsHex(id)+" "+printMsg(msg))
          sdo_manager ! sdo
        case nmt : ReceivedNMT =>
          println(self.path+" received NMT "+get4DigitsHex(id)+" "+printMsg(msg))
        case sync : ReceivedSYNC =>
          println(self.path+" received SYNC "+get4DigitsHex(id)+" "+printMsg(msg))
        case emergency: ReceivedEMERGENCY =>
          println(self.path+" received EMERGENCY "+get4DigitsHex(id)+" "+printMsg(msg))
        case timestamp: ReceivedTIMESTAMP =>
          println(self.path+"received TimeStamp "+get4DigitsHex(id)+" "+printMsg(msg))
      }
    case CanClose => 
      context.parent ! CanClose
      context.children.foreach(c => context.stop(c))
      context.stop(self)
      
    case any => println("Can dispatcher received unknown message "+any)
  }
  
}