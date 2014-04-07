package com.tecniplast.device

import akka.actor._

abstract class AbstractCanDevice(dispatcher_prop: Props) 
	extends Actor {
  
  val dispatcher =
    context.actorOf(dispatcher_prop,"dispatcher")
  
}