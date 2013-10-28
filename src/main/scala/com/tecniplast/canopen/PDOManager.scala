package com.tecniplast.canopen

import akka.actor._
import com.typesafe.config._
import com.tecniplast.device.CanDevice._

/*
 * Manage RPDO and TPDO
 * send and receive
 */
case class PDOManager() extends Actor {
  
  def receive = { 
    case _ =>
  }

}