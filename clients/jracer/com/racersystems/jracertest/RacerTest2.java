package com.racersystems.jracertest;
import com.racersystems.jracer.RacerClient;

public class RacerTest2 {

	public static void main(String[] args) {

		String ip = "localhost";
		int port = 8088;

		RacerClient racer = new RacerClient(ip,port);		

		try {

			racer.openConnection();

			racer.loggingOn();

			racer.fullReset();

			racer.impliesM("|http://www.test.de\\|#A|", "B");
			racer.defineConcreteDomainAttributeM("name",":type","string"); 
			racer.constrainedM("i","i-name","name"); 

			String string = "\"String with \\\"inner String\\\" yes!\""; 
			racer.constraintsM("(string= i-name "+string+")"); 

			System.out.println(racer.allAtomicConcepts()); 
			System.out.println(racer.allIndividuals()); 
			System.out.println(racer.allConstraints()); 
			System.out.println(racer.aboxConsistentMP()); 


		} catch (Exception e) {

			e.printStackTrace();

		}

	}

}


