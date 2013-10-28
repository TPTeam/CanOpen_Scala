package com.tecniplast.canopen

import akka.actor._
import com.typesafe.config._
import com.tecniplast.device.CanDevice._
import CanOpenMessages._
import akka.dispatch.UnboundedPriorityMailbox
import akka.dispatch.PriorityGenerator

/*
 * Manage SDO
 * send and receive
 */
case class SDOManager() extends Actor {
 
  //Diventa una variabile per questione di prestazioni
  //var queue: Seq[SendCanOpenMessage] = Seq()
  //var waitingAnsware = false
  
  import SDOManagerMsgs._
  def receive = {
    /*case EnqueueMsg(msg) =>
      if (!waitingAnsware) {
	   	send(msg)
      } else {
        queue = queue :+ msg
      }
    case ReceivedTSSDOCanOpenMessage(address, message) =>
      println(self.path+" "+new java.util.Date().getTime()+" received TSSDO RESPONSE "+address)
      if (queue.size >0) {
	send(queue.head)
	queue = queue.tail
      }
    */   
    firstEnqueue
  
  }
  
  /*
  def send(msg: SendCanOpenMessage) = {
    context.parent ! msg
	waitingAnsware = true
  } 
  */
    
  
  //SendCanOpenMessage va specializzato
  def processNext(queue: Seq[SendCanOpenMessage]): PartialFunction[Any,Unit] = {
	if (queue.size > 0) {
	  context.parent ! queue.head
	  waitAnsware(queue.tail)
	} else 
		firstEnqueue
  }

  //Per ogni target devo fare una coda diversa...quindi un attore diverso che aspetta la risposta
  def firstEnqueue: PartialFunction[Any,Unit] = {
	 case EnqueueMsg(msg) =>
	   	context.parent ! msg
	   	context.become(waitAnsware(Seq()), true)
  }

  // Devo inserire un timeout...
  def waitAnsware(queue: Seq[SendCanOpenMessage]): PartialFunction[Any,Unit] = {
	case EnqueueMsg(msg) =>
		context.become(waitAnsware(queue :+ msg), true)
	case ReceivedTSSDOCanOpenMessage(address, message) =>
        println(self.path+" "+new java.util.Date().getTime()+" received TSSDO RESPONSE "+address)
        context.become(processNext(queue), true)
  }
    

}

case class SDOManager2 extends Actor {
  
  val single_nodes = 
    for (
        addr <- 1 to 127
    ) yield {
    	context.actorOf(Props(SingleNodeSDOManager(addr)))
    }
  
  import SDOManagerMsgs._
  def receive = {
    case EnqueueMsg2(msg) =>
      
  }
  
  
  
  case class SingleNodeSDOManager(address: Int) extends Actor {
    
    def receive = {
      //TODO
      case _ => 
    }
    
  }
  
}

object SDOManagerMsgs {
  case class EnqueueMsg(msg: SendCanOpenMessage)
  case class EnqueueMsg2(msg: SendRCSDOCanOpenMessage)
}


import akka.dispatch.PriorityGenerator
import akka.dispatch.UnboundedPriorityMailbox
import com.typesafe.config.Config
 
// We inherit, in this case, from UnboundedPriorityMailbox
// and seed it with the priority generator
class SDOPrioMailbox(settings: ActorSystem.Settings, config: Config)
  extends UnboundedPriorityMailbox(
    // Create a new PriorityGenerator, lower prio means more important
    PriorityGenerator {
      // 'highpriority messages should be treated first if possible
      case _ : ReceivedTSSDOCanOpenMessage => 0
 
      // We default to 1, which is in between high and low
      case otherwise     ⇒ 1
      
      // 'lowpriority messages should be treated last if possible
      case 'lowpriority  ⇒ 2
 
      // PoisonPill when no other left
      case PoisonPill    ⇒ 3
    }
)
