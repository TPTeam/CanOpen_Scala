package com.tecniplast.canopen

import akka.actor._
import com.typesafe.config._
import com.tecniplast.device.CanDevice._
import scala.concurrent.duration._
import NMT._
import CanOpenMessages._

object NMT {
  //Broadcast value
  final val ALL_SLAVES = 0x00
  
  //Command Secifier
  final val CMD_START 				= 0x01
  final val CMD_STOP 				= 0x02
  final val CMD_PRE_OPERATIONAL 	= 0x80
  final val CMD_RESET_NODE		 	= 0x81
  final val CMD_RESET_COMMUNICATION	= 0x82
  
  //State values
  final val STATUS_INITIALISING		= 0x00
  final val STATUS_STOPPED			= 0x04
  final val STATUS_OPERATIONAL		= 0x05
  final val STATUS_PRE_OPERATIONAL	= 0x7F
  
  case object Check
  /*
   * 
   * Managed NMT
   */
  case class WatchNodes(watcher: ActorRef, adresses: List[Int])
  case class UnWatchNodes(watcher: ActorRef, adresses: List[Int])
  
  //Status
  class NMTStatus(address: Int, status: Int) {
    def getAddress = address
    
    def msg = 
      (status) match {
      	case STATUS_INITIALISING 	=> 	NodeBootingUp(address)
      	case STATUS_STOPPED 		=> 	NodeIdling(address)
      	case STATUS_OPERATIONAL 	=> 	NodeStarted(address)
      	case STATUS_PRE_OPERATIONAL => 	NodeStopped(address)
      }
  }
  case class NodeBootingUp(address: Int) 	extends NMTStatus(address, STATUS_INITIALISING) 
  case class NodeIdling(address: Int) 		extends NMTStatus(address, STATUS_STOPPED)
  case class NodeStarted(address: Int) 		extends NMTStatus(address, STATUS_OPERATIONAL)
  case class NodeStopped(address: Int)	 	extends NMTStatus(address, STATUS_PRE_OPERATIONAL)
  
  case class NodeUnWatched(address: Int)
 
  //Commands
  class NMTCmd(address: Int,command: Int) {
    def getAddress = address
    def getCommand = command
    
    def mag =
      (command) match {
      	case CMD_START					=> 	StartNode(address)
      	case CMD_STOP					=>	StopNode(address)
      	case CMD_PRE_OPERATIONAL 		=>	IdleNode(address)
      	case CMD_RESET_NODE		 		=>	ResetNode(address)
      	case CMD_RESET_COMMUNICATION	=>	ResetCommunication(address)
      }
  }
  case class StartNode(address: Int) 			extends NMTCmd(address,CMD_START)
  case class StopNode(address: Int) 			extends NMTCmd(address,CMD_STOP)
  case class IdleNode(address: Int) 			extends NMTCmd(address,CMD_PRE_OPERATIONAL)
  case class ResetNode(address: Int) 			extends NMTCmd(address,CMD_RESET_NODE)
  case class ResetCommunication(address: Int) 	extends NMTCmd(address,CMD_RESET_COMMUNICATION)
 
}

trait NMTManager {
  me: Actor =>
    
    val checkTime = 5 seconds
    lazy val initTime = checkTime
    
    override def preStart(): Unit = {
    	import context.dispatcher	
    	context.system.scheduler.schedule(initTime, checkTime)(self ! Check)
  	}
}

/*
 * Try to keep everythig always alive withoput any feedback
 */
case class UnmanagedNMTManager() extends Actor with NMTManager {
  override lazy val initTime = 100 milliseconds
  
  def receive = {
    firstTimeBootUp
  } 
  
  def firstTimeBootUp: PartialFunction[Any,Unit] = {
    case Check =>
    	context.parent ! SendNMTCMDCanOpenMessage(ALL_SLAVES,CMD_START)
    	context.become(restartAlways,true)
    case any => self ! any
  }
  
  def restartAlways: PartialFunction[Any,Unit] = {
    case Check =>
    case ReceivedNMTHEARTBEATCanOpenMessage(addr,msg) =>
      msg.toList.headOption.map(_ & 0x7F) match {
        case Some(STATUS_INITIALISING) => 
          context.parent ! SendNMTCMDCanOpenMessage(addr,CMD_START)
        case _ =>
          throw new Exception("Recived uncorrect NMT Msg")
      }
    case any => println("Unmanaged message to NMTManager: "+any)
  }
  
}

/*
 * My parent is the dispatcher
 * 
 */
case class PerNodeNMTManager() extends Actor with NMTManager {
  
  def receive = {
    watcher(List())
  }
  
  def watcher(watchee: List[(ActorRef,Int)]): PartialFunction[Any,Unit] = {
    case WatchNodes(actor,adresses) =>
      context.become(watcher(watchee ::: adresses.map((actor,_))),true)
    case UnWatchNodes(actor,adresses) =>
      context.become(watcher(watchee.filterNot(x => (x._1.path.equals(actor.path) && adresses.contains(x._2)))), true)
      
    case cmd: NMTCmd =>
      context.parent !  SendNMTCMDCanOpenMessage(cmd.getAddress,cmd.getCommand) 
      
    case ReceivedNMTHEARTBEATCanOpenMessage(addr, msg) => 
      msg.toList.headOption.map(_ & 0x7F) match {
        case Some(status) => 
          watchee.filter(x => addr == x._2).map(_._1).foreach(_ ! new NMTStatus(addr,status).msg)
        case _ =>
          throw new Exception("Recived uncorrect NMT Msg")
      }
    case Check =>
    case any => println("NMTPerNode received unmanaged "+any)
  }
}