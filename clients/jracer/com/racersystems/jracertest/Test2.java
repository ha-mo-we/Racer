package com.racersystems.jracertest;
import com.racersystems.jracer.RacerClient;

public class Test2 {

	public static void main(String[] argv) {
		String ip = "127.0.0.1";
		int port = 8088;
		RacerClient racer = new RacerClient(ip,port);		

		try {
			racer.openConnection();

			racer.fullReset();
			racer.owlapiNewReasoner("test");
			racer.owlapiNewOntology("test");
			racer.owlapiAutoAddAxiomsTo("test");

			racer.owlapiGetOWLClassAssertionAxiom("d","j");
			racer.owlapiGetOWLObjectPropertyAssertionAxiom("j", "r", "k");

			racer.owlapiLoadOntology("test");

			System.out.println(racer.tboxCoherentP("test")); 
			System.out.println(racer.aboxConsistentP("test")); 

			racer.closeConnection();
		}
		catch (Exception e) {
			e.printStackTrace();

		}
	}

}
