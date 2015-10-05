package com.tecniplast.jni;

public class CanJni {
	
	private static CanJniInterfacePojo canD0;
	private static CanJniInterfacePojo canD1;
	private static CanJniInterfacePojo canD2;
	private static CanJniInterfacePojo canD3;
	
	public synchronized void setDriver(CanJniInterfacePojo canD, Integer number) throws Exception {
		if (number==0)
			canD0 = canD;
		else if (number==1)
			canD1 = canD;
		else if (number==2)
			canD2 = canD;
		else if (number==3)
			canD3 = canD;
		else
			throw new Exception("Can Device number not existent");
	}

	public synchronized static void msgReaded(int device,long id, int dlc, int flags,byte[] msg) {
		if (device==0 && canD0!=null)
			canD0.msgReaded(id, dlc, flags, msg);
		else if (device==1 && canD1!=null)
			canD1.msgReaded(id, dlc, flags, msg);
		else if (device==2 && canD2!=null)
			canD2.msgReaded(id, dlc, flags, msg);
		else if (device==3 && canD3!=null)
			canD3.msgReaded(id, dlc, flags, msg);
		else {
			System.out.println("No , message for non initialized port "+device+" "+id);
		}
		
	}
	
	public synchronized native int openPort(int device, int bitrate);
	
	public synchronized native int closePort(int device);
	
	public synchronized native int writeMsg(
			int device,
			long id,
			int length,
			int flags,
			byte[] msg);
	/*
	public native int fwDownload(
			int device,
			int address,
			String filePath
			);
	*/
	static{
	    System.loadLibrary("C-Can-JNI-ScalaBridge");
	}

}
