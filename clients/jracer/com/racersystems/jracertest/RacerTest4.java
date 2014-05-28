package com.racersystems.jracertest;
import com.racersystems.jracer.RacerClient;
import com.racersystems.jracer.RacerList;
import com.racersystems.jracer.RacerResult;
import com.racersystems.jracer.RacerSymbol;

public class RacerTest4 {

	public static void main(String[] args) {

		// please adjust this path to match your environment: 

		String peopleAndPets = "\"c:/people+pets.owl\""; 

		String ip = "localhost";
		int port = 8088;

		RacerClient racer = new RacerClient(ip,port);		

		try {

			racer.openConnection();

			racer.loggingOn$();
			racer.sendRaw("(logging-on)");

			racer.fullReset$();

			racer.owlReadFile$(peopleAndPets); 

			boolean consistent = racer.aboxConsistentP(); 

			RacerList<RacerList>
			res1 = (RacerList<RacerList>)
			racer.taxonomy$(); 

			for (RacerList triple : res1) {
				System.out.println("--------"); 
				System.out.println("Concept : "+triple.getValue().get(0)); 
				System.out.println("Parents : "+triple.getValue().get(1)); 
				System.out.println("Children: "+triple.getValue().get(2)); 
			} 

			RacerResult res2 = (RacerResult)
					racer.racerAnswerQuery$("(?x ?y)","(and (?x #!:person) (?x ?y #!:has_pet))"); 

			if (res2 instanceof RacerSymbol) { // no instances? nil can't be cast in a RacerList!
				System.out.println("No instances!"); 
			} else {
				for (RacerList<RacerList<RacerSymbol>> bindings : (RacerList<RacerList<RacerList<RacerSymbol>>>)res2) {
					for (RacerList<RacerSymbol> binding : bindings) {
						for (RacerSymbol varval : binding) {
							System.out.println(varval);
						}
					}
				} 

			}

		} catch (Exception e) {

			e.printStackTrace();

		}

	}

}


