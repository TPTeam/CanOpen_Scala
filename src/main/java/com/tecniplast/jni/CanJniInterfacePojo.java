package com.tecniplast.jni;

public interface CanJniInterfacePojo {
	
	public void msgReaded(long id,
              			int dlc,
              			int flags,
              			byte[] msg);
	
	public void openPort();

	public void closePort();

	public boolean writeMessage(long id, byte[] msg, int flags);
	
	public boolean firmwareDownload(int address, String filePath);

}
