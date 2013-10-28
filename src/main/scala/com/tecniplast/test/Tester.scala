package com.tecniplast.test

import com.tecniplast.nativeInterface._
import akka.actor._
import com.tecniplast.device._
import com.sun.jna._

object Tester extends App {
  println("Start")
    /*
    CanLibrary.library.openPort(1,CanLibrary.BitRate.CAN_BAUD_1M,null)

    CanLibrary.library.closePort(0)
    CanLibrary.library.closePort(1)
    CanLibrary.library.closePort(2)
    */
     
   val system =
    ActorSystem("vivathron-CAN-system")
  
  import com.tecniplast.canopen._
 
  val can0 =
    system.actorOf(Props(CanDevice(0)(Props(CanOpenDispatcher))),"pcan0")
    
  val can1 =
    system.actorOf(Props(CanDevice(1)(Props(CanOpenDispatcher))),"pcan1")
  
  //Thread.sleep(1000)  
    
  import com.tecniplast.device.CanDevice._
  import com.tecniplast.canopen.CanOpenMessages._
  
  import NMT._

  val stringToSend = "ciao"
    
  Thread.sleep(1000)
  
  val pcan0_dispatcher =
    system.actorFor("/user/pcan0/dispatcher")

  val pcan0_nmt_manager =
    system.actorFor("/user/pcan0/dispatcher/nmt_manager")
    
  val pcan0_sdo_manager =
    system.actorFor("/user/pcan0/dispatcher/sdo_manager")
    
  //Thread.sleep(15000)
    
  val msg: Array[Byte] = Array(0x00,0x02,0x03, 0xFF)
  //println("son qui2")
  //val msgS = new String(msg)
    //"ciÿao"//"ÿ"//
  //println("Stringa da mandare "+msgS+" length "+msgS.length())
    
  //pcan0_dispatcher ! SendTPDOCanOpenMessage(1,5,msg)
  
  //pcan0_dispatcher ! SendTPDOCanOpenMessage(1,9,msg)
  
  println("Mando il messaggio")
  
  //pcan0_nmt_manager ! StartNode(9)
  /*
   * PARTE TESTATA PER LA RICEZIONE CON RTR IN LOCCIONI
   
  for (
      i <- 0 to 1000
      ) {
    Thread.sleep(5000)
  // Significa rimandami il valore del PDO 0x0A(presenza gabbia) --> |TPDO1 | indirizzo9
  
    
    pcan0_dispatcher ! SendRPDORequestCanOpenMessage(0x01,0x09)
  }
  
  *
  */
  
  //SDO example --> Initiate Domain Upload - 0x40 // + indirizzo in little endian(2 byte) + sottoindirizzo(1 byte)
  //pcan0_dispatcher ! SendRCSDOCanOpenMessage(0x09, Array(0x40,0x01,0x12,0x01))
  //se ok 0x4B
  
  // SDO Example --> Initiate Domain Download - 0x20 + 0x03(expedited + size specified)  --> i primi due bit contano quanti byte NON sono usati
  // + indirizzo in little endian(2 byte) + sottoindirizzo(1 byte) + dato(in questo caso 4 byte) 
  //pcan0_dispatcher ! SendRCSDOCanOpenMessage(0x09, Array(0x23,0x01,0x12,0x21,0x02,0x01,0x04,0x03))
  // se ok 0x60
  
  //pcan0_dispatcher ! SendRCSDOCanOpenMessage(0x09, Array(0x40,0x01,0x12,0x21))
  
  
  //Prendo dei valori capacitivi
  /*
  for (
      i <- 0x01 to 0x11 )
  pcan0_dispatcher ! SendRCSDOCanOpenMessage(0x09, Array(0x40,0x01,0x12,i))
  */
  //Così dico che invio un solo byte
  
  
  //Coloro il LED RGB
  /*
  for (
      i <- 0x00 to 0x07) {
      Thread.sleep(3000)
	  pcan0_dispatcher ! SendRCSDOCanOpenMessage(0x09, Array(0x2F,0x01,0x12,0x20,i))
  }
  
  pcan0_dispatcher ! SendRCSDOCanOpenMessage(0x09, Array(0x2F,0x01,0x12,0x20,0x00))
  */
  
  Thread.sleep(2000)
  val now = new java.util.Date().getTime()
  
  
  println("Inizio invio --> "+now)
  import SDOManagerMsgs._
  
  for (
      i <- 0x02 to 0x09 ) {
	  pcan0_sdo_manager ! EnqueueMsg(SendRCSDOCanOpenMessage(0x09, Array(0x40,0x01,0x12,i)))
  }
  println("Fine invio --> "+(new java.util.Date().getTime()-now))
  
  Thread.sleep(10000)
  
  //pcan0_nmt_manager ! StopNode(9)
  
  

  //pcan0_nmt_manager ! StartNode(9)
  
  
  //pcan1_nmt_manager ! StopNode(0)
  
  //can0 ! CanMsgSend(0x205, msg, 0)
  
  //can1 ! CanMsgSend(112, "1234567890", 0)
  
  //Thread.sleep(20000)
  
  //pcan1_nmt_manager ! StopNode(0)
  
  //Thread.sleep(2000)
  
  can0 ! CanClose
  
  //can1 ! CanClose
  
  Thread.sleep(2000)
  
  system.shutdown
  println("End")
  
  System.exit(0)
  
}