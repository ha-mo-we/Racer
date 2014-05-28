package com.racersystems.jracertest;
import com.racersystems.jracer.RacerClient;

public class Test {

	public static void main(String[] argv) {
		String ip = "127.0.0.1";
		int port = 8088;
		String filename="\"/Applications/RacerPro 2.0 preview/examples/owl/people-pets.owl\"";

		RacerClient racer = new RacerClient(ip,port);
		try {
			racer.openConnection();
			System.out.println(racer.sendRaw("(owl-read-file " + filename + ")"));
			System.out.println(racer.sendRaw("(all-atomic-concepts)"));
			racer.closeConnection();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}

}