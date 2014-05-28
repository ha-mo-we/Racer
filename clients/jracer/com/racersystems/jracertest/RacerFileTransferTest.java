package com.racersystems.jracertest;

import com.racersystems.jracer.RacerClient;
import com.racersystems.jracer.RacerList;
import com.racersystems.jracer.RacerSymbol;

public class RacerFileTransferTest {

	public static void main(String[] args) {

		// please adjust this path to match your environment: 

		// String peopleAndPets = "C:/jracer-2-0/demo/people+pets.owl"; 

		String peopleAndPets = "/home/mi.wessel/KBs/people+pets.owl"; 

		// please note that RacerPro must be running in unsafe mode for this test!
		// start RacerPro with -- -u option! 

		String ip = "localhost";
		int port = 8088;

		RacerClient racer = new RacerClient(ip,port);		

		try {

			racer.openConnection();

			racer.loggingOn();
			racer.sendRaw("(logging-on)");

			racer.fullReset();

			racer.transferFile(peopleAndPets,"owl"); 

			RacerList<RacerList<RacerList<RacerSymbol>>>
			res2 = (RacerList<RacerList<RacerList<RacerSymbol>>>)
			racer.racerAnswerQuery$("(?x ?y)","(and (?x #!:person) (?x ?y #!:has_pet))"); 

			for (RacerList<RacerList<RacerSymbol>> bindings : res2) {
				for (RacerList<RacerSymbol> binding : bindings) {
					System.out.println();
					System.out.print(binding.getValue().get(0));
					System.out.print(" "+binding.getValue().get(1));
				}
				System.out.println(); 
			} 

		} catch (Exception e) {

			e.printStackTrace();

		}

	}

}


