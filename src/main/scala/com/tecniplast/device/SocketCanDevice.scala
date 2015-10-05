package com.tecniplast.device

import akka.actor._
import com.tecniplast.device.CanDevice.GetDispatcher
import com.tecniplast.device.CanDevice.RefDispatcher
import de.entropia.can.CanSocket
import de.entropia.can.CanSocket.Mode
import de.entropia.can.CanSocket.CanInterface
import de.entropia.can.CanSocket.CanFrame
import de.entropia.can.CanSocket.CanId
import com.tecniplast.canopen.CanOpenFlags
import java.util.Date

case class SocketCanDevice(cInterface: String)(dispatcher_prop: Props)
	extends AbstractCanDevice(dispatcher_prop) with SocketCanAdapter{

    val socketCan: CanSocket = new CanSocket(Mode.RAW)
    val canInterface: CanInterface = new CanInterface(socketCan, cInterface)     
    
    def receive = {
      frameReceiver.start()
      send_and_rec
    }
      
    def send_and_rec: Receive = {
      //frameReceiver ! ReadFrame
      def _s_rcv: Receive = {
      	case x: CanDevice.CanMsgReceived => 
      		dispatcher ! x
      		//frameReceiver ! ReadFrame
      		
      	case msg: CanDevice.CanMsgSend =>
      		frameTransmitter ! msg
      	
      	case x: GetDispatcher =>
      		sender ! RefDispatcher(dispatcher)
      		
      	case any =>
       }
     
       _s_rcv
    }  
}

case object ReadFrame

trait SocketCanAdapter{
  me: SocketCanDevice =>
 
  val socketCan: CanSocket
  val canInterface: CanInterface
  
  val frameReceiver = new Thread(new Receiver(self)) //context.actorOf(Props(Receiver()), "canReceiver")
  val frameTransmitter =  context.actorOf(Props(Transmitter()), "canTransmitter")
  
  override def preStart() = socketCan.bind(canInterface)  
  
  override def postStop() = socketCan.close()

  case class Receiver(p: ActorRef) extends Runnable {

    override def run() = {
      while(true) {
        val frame = socketCan.recv()
        p ! CanDevice.CanMsgReceived(
          frame.getCanId().getCanId_EFF(),
          frame.getData(),
          0x00)
      }

    }

  }

  /*case class Receiver() extends Actor{
    
    def listen() = {
      while(true) {
        val frame = socketCan.recv()
        context.parent ! CanDevice.CanMsgReceived(
          frame.getCanId().getCanId_EFF(),
          frame.getData(),
          0x00)
      }
    }      
    
    def receive: Receive = {
      
      case ReadFrame =>
        listen()
        
      case any =>
    }

  }
*/
  case class Transmitter() extends Actor{

    def writeMessage(id: Long, msg: Array[Byte], flags: Int) = {
    	val cId = new CanId(id.toInt)
    	if(flags == CanOpenFlags.MSGTYPE_RTR)
    		cId.setRTR()
      	val cFrame = new CanFrame(canInterface,cId,msg)
    	socketCan.send(cFrame)

  	}
    
    def receive: Receive = {
      
      case msg: CanDevice.CanMsgSend =>
      		writeMessage(msg.id,msg.msg,msg.flags)
      		
      case any =>
    }
    

    
  }
  
}
