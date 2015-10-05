package com.tecniplast.canopen.mailboxes

import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import com.tecniplast.canopen.CanOpenMessages
import com.typesafe.config.Config
 
// We inherit, in this case, from UnboundedPriorityMailbox
// and seed it with the priority generator
class SDO_PrioMailbox(settings: akka.actor.ActorSystem.Settings, config: Config)
  extends UnboundedPriorityMailbox(
    // Create a new PriorityGenerator, lower prio means more important
    PriorityGenerator {
      // 'highpriority messages should be treated first if possible
      case _ : CanOpenMessages.ReceivedTSSDOCanOpenMessage => 0
      
      // 'lowpriority messages should be treated last if possible
      case 'lowpriority  => 2
 
      // PoisonPill when no other left
      case akka.actor.PoisonPill    => 3
      
      // We default to 1, which is in between high and low
      case otherwise     => 1

    }
)