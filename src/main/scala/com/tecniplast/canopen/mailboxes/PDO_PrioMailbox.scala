package com.tecniplast.canopen.mailboxes

/**
 * Created by fbignardi on 5/13/15.
 */
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import com.tecniplast.canopen.CanOpenMessages.ReceivedTPDO1CanOpenMessage
import com.typesafe.config.Config

// We inherit, in this case, from UnboundedPriorityMailbox
// and seed it with the priority generator
class PDO_PrioMailbox(settings: akka.actor.ActorSystem.Settings, config: Config)
  extends UnboundedPriorityMailbox(
    // Create a new PriorityGenerator, lower prio means more important
    PriorityGenerator {
      // 'highpriority messages should be treated first if possible
      case _ : ReceivedTPDO1CanOpenMessage=> 0

      // 'lowpriority messages should be treated last if possible
      case 'lowpriority  => 2

      // PoisonPill when no other left
      case akka.actor.PoisonPill    => 3

      // We default to 1, which is in between high and low
      case otherwise     => 1

    }
  )
