package com.racersystems.jracertest;
import com.racersystems.jracer.RacerClient;
import com.racersystems.jracer.RacerList;
import com.racersystems.jracer.RacerResult;
import com.racersystems.jracer.RacerSymbol;

public class RacerTest3 {

	public static void main(String[] args) {

		// please adjust this path to match your environment: 

		String peopleAndPets = "\"C:/people+pets.owl\""; 

		String ip = "localhost";
		int port = 8088;

		RacerClient racer = new RacerClient(ip,port);		

		try {

			racer.openConnection();

			racer.loggingOn();
			racer.sendRaw("(logging-on)");

			racer.fullReset();

			racer.owlReadFile(peopleAndPets); 

			String res = racer.conceptAncestorsM("#!:person"); 

			System.out.println(res); 

			RacerResult 
			res3 = (RacerResult)
			racer.conceptAncestorsM$("#!:person"); 

			if (!(res3 instanceof RacerSymbol)) {
				for (RacerList<RacerSymbol> synset : (RacerList<RacerList<RacerSymbol>>) res3) {
					System.out.println(); 
					System.out.println("New Synset:"); 
					for (RacerSymbol concept : (RacerList<RacerSymbol>) synset) {
						System.out.println(concept); 
					}
				}
			} else {
				System.out.println((RacerSymbol) res3); 

			}

		} catch (Exception e) {

			e.printStackTrace();

		}

	}

}


