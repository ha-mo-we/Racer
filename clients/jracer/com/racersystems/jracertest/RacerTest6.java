package com.racersystems.jracertest;
import com.racersystems.jracer.RacerClient;

public class RacerTest6 {

	public static void main(String[] args) {

		// please adjust this path to match your environment: 

		String ip = "localhost";
		int port = 8088;

		RacerClient racer = new RacerClient(ip,port);		

		try {

			racer.openConnection();

			racer.loggingOn();
			racer.sendRaw("(logging-on)");

			racer.fullReset();

			racer.definePrimitiveConceptM("i","|http://www.test.org#test|");
			racer.definePrimitiveConceptM("i","|http://www.test.org#test2|");
			racer.definePrimitiveConceptM("i","|http://www.test.org#test\\|mit\\|Sonderzeichen\\)|");

			System.out.println(racer.allAtomicConcepts());

			// System.out.println(racer.exitServer());

		} catch (Exception e) {

			e.printStackTrace();

		}

	}

}


