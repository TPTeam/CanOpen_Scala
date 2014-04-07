package com.tecniplast.device

import purejavacomm._
import java.io._
import akka.actor._
import scala.concurrent.duration._
import NewSerialDeviceMsgs._

case class NewSerialDevice(portName: String, speed: Integer)
    extends Actor {

  def portId =
    if (System.getProperty("os.name").startsWith("Windows"))
    try {
      val ret = 
      Some(CommPortIdentifier.getPortIdentifier(portName))
      println("ok")
      ret
    } catch {
      case err : Throwable =>
        err.printStackTrace()
        None
    }
    else
    findPort(CommPortIdentifier.getPortIdentifiers())

  val maxRetry = 10//10// 100//10000
  val maxAnswareRetry = 100
  
  /*
   * Inner messages
   */
  case object DoConnect
  case object DoDisconnect
  case object Loop
  
  def findPort(e: java.util.Enumeration[_]): Option[CommPortIdentifier] =
    try
      if (e.hasMoreElements()) {
        val __port = e.nextElement().asInstanceOf[CommPortIdentifier]
        if (portName.endsWith(__port.getName()))
          Some(__port)
        else
          findPort(e)
      } else None
    catch {
      case _: Throwable => None
    }

  def disconnect(implicit in: InputStream, out: OutputStream) : Receive = {
    context.parent ! Disconnected
    try
      in.close()
    catch { case _: Throwable => }
    try
      out.close()
    catch { case _: Throwable => }
    try
      resetConn()
    catch { case _: Throwable => }
    
    connect()
  }

  /*
   * Low level connection e disconnection for resetting dirty states
   * only for linux??
   */
  def resetConn() = {
    try {
      import jtermios.JTermios._
      val fd = open(portName, O_RDWR | O_NOCTTY | O_NONBLOCK)
      fcntl(fd, F_SETFL, 0)
      tcflush(fd, TCIOFLUSH)
      val ec = close(fd)
    } catch {
      case err: Throwable =>
        err.printStackTrace
    }
  }
  
  def receive = connect
  
  def connect(): Receive = {
    self ! DoConnect
    _connect()
  }
  
  def _connect(): Receive = {
    case DoConnect =>
      (portId) match {
        case Some(port_id) =>
          	resetConn()
          
          	val serial =
          		port_id.open(portName, 1000).asInstanceOf[SerialPort]
        	serial.removeEventListener()

        	serial.setSerialPortParams(speed, SerialPort.DATABITS_8,
        			SerialPort.STOPBITS_1, SerialPort.PARITY_NONE)
          	serial.notifyOnDataAvailable(false)
        	serial.notifyOnOutputEmpty(false)
   			serial.setFlowControlMode(SerialPort.FLOWCONTROL_NONE + SerialPort.FLOWCONTROL_NONE)
        	serial.disableReceiveFraming()
        	serial.disableReceiveThreshold()
        	serial.disableReceiveTimeout()
        	implicit val out = (serial.getOutputStream())
        	implicit val in = (serial.getInputStream())
        	context.become(operative, true)
        case _ => 
          resetConn()
          import context.dispatcher
          context.system.scheduler.scheduleOnce(1 second)(
        		  self ! DoConnect)
      }
    case any => self ! any
  }
  
  def operative(implicit in: InputStream, out: OutputStream) : Receive = {
    context.parent ! Connected
    self ! Loop
    _operative
  }
  
  def splitMessages(in: List[Byte]): List[List[Byte]] = {
    def _split(in: List[Byte], partial: List[List[Byte]]): List[List[Byte]] =
      if (in.isEmpty) partial
      else if (CanUsbCommands.usefullTrails.exists(_ == in.head)) _split(in.tail,partial.+:(List(in.head)))
      else _split(in.tail,(partial.tail).+:((partial.head.:+(in.head))))
      
    _split(in,List(List()))
  }
  
  def _operative(implicit in: InputStream, out: OutputStream) : Receive = {
    case Loop =>
      if (checkDataToRead) {
        val read = 
         doReadAsPossible
         
        if (read.size > 0) {
          splitMessages(read).foreach(msg => {
            	if (!msg.isEmpty) {
        		  context.parent ! DataRead(msg.toArray)
            	}
          })
        }
      }
      if (checkDataToRead)
        self ! Loop
      else {
    	  import context.dispatcher
    	  context.system.scheduler.scheduleOnce(1 milliseconds)({
    		  self ! Loop  
    	  })
      }
    case dw: DataWrite =>
      if (!checkDataToRead)  {
    	  if (doWrite(dw.arr)) {
    	    //ritento all'infinito
    	    if (!doReadAnsware)
    	    	self ! dw
    	  }
      } else
        self ! dw
    case DoDisconnect =>
      context.become(disconnect, true)
  }
  
  def checkDataToRead(implicit in: InputStream): Boolean =
          try {
            in.available() > 0
          } catch {
            case err: Throwable =>
              err.printStackTrace
              false
          }
  
  def doWrite(arr: Array[Byte])(implicit out: OutputStream) =
    try {
          out.write(arr, 0, arr.length)
          out.flush()
          true
    } catch {
    	case err: Throwable =>
          	self ! DoDisconnect
          	false
    }
    
  def doReadAnsware(implicit in: InputStream): Boolean =
    doReadAnsware(maxAnswareRetry)
    
  def doReadAnsware(retry: Integer)(implicit in: InputStream): Boolean =
    try {
  	  val n = in.available
      if (n > 0) {
        val buffer = new Array[Byte](1)
        in.read(buffer, 0, 1)
        if (buffer.size >= 1) {
          if (buffer(0) == 0x0D) true
          else if (buffer(0) == 0x07) true//false
          else if (retry > 0) doReadAnsware(retry-1)
          else false
        } else if (retry > 0) doReadAnsware(retry-1)
          else false
      } else if (retry > 0) {
        try {Thread.sleep(1)} catch {case _ : Throwable =>}
        doReadAnsware(retry-1)
      }
      else false
    } catch {
      case err: Throwable =>
        err.printStackTrace
        false
    }

  def doReadAsPossible(implicit in: InputStream): List[Byte] =
    doReadAsPossible(List(), maxRetry)
    

  def cleanTrail(in: Array[Byte]): Array[Byte] = 
      if (in.isEmpty) Array()
      else if (CanUsbCommands.usefullTrails.exists(_ == in.head)) in
      else cleanTrail(in.tail)

  def doReadAsPossible(partial: List[Byte], retry: Integer)(implicit in: InputStream): List[Byte] =
    try {
  	  val actual = partial.size
  	  val n = in.available
      if (n > 0) {
        if (actual > 0) {
        	//waiting for last CR
        	val buffer = new Array[Byte](n)
        	in.read(buffer, 0, n)
        	val total = (buffer.toList).:::(partial)
        	
        	if (total.last == 0x0D) total
        	else if (retry > 0) doReadAsPossible(total, retry-1)
        	else total
        } else {
          val buffer = new Array[Byte](n)
          in.read(buffer, 0, n)
          val usefullPart =
            cleanTrail(buffer)
  
          if (!usefullPart.isEmpty && usefullPart.last == 0x013)
            usefullPart.toList
          else 
            doReadAsPossible(usefullPart.toList, maxRetry)
        }
      } else if (actual> 0 && retry > 0) {
        try {
    	    Thread.sleep(0)
        } catch {case _ : Throwable =>}
        doReadAsPossible(partial, retry - 1)
      } else {
        partial
      }
    } catch {
      case err: Throwable =>
        err.printStackTrace
        List()
    }
}

object NewSerialDeviceMsgs {
  case object Connected
  case object Disconnected
  
  case class DataRead(arr: Array[Byte])
  case class DataWrite(arr: Array[Byte])
}
