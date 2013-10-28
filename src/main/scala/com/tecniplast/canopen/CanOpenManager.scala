package com.tecniplast.canopen

import akka.actor._
import com.typesafe.config._

object CanOpenManager {
  
  val system =
    ActorSystem("vivathron-CAN-Actor-system")
    
  val config =
    ConfigFactory.load("canopen.conf")
    
   /*
    *  0-> 2.0 A
    *  1-> 2.0 B
    */
  val protocolVersion = 
    config.getString("protocolVersion").equals("B")
  
  val manager = 
    system.actorOf(Props(CanOpenManagerActor(config)),"can-open-manager")
    
  
}

case class CanOpenManagerActor(config : Config) extends Actor {
  
  def receive = {
    case _ => 
  }
  
}