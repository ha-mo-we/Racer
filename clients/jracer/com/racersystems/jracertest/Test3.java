package com.racersystems.jracertest;
import com.racersystems.jracer.RacerClient;
import com.racersystems.jracer.RacerList;
import com.racersystems.jracer.RacerSymbol;

public class Test3 {

	public static void main(String[] args) {

		String filename = "\"C:/people+pets.owl\"";

		String ip = "localhost";
		int port = 8088;

		RacerClient racer = new RacerClient(ip, port);

		try {

			racer.openConnection();

			racer.fullReset();

			racer.owlReadFile(filename);

			String individualName = "hello you";
			String concept = "this is a test";

			racer.addConceptAssertion("default", individualName, concept);

			System.out.println(racer.racerAnswerQuery("(?x ?y)",
					"(and (?x #!:person) (?x ?y #!:has_pet))"));

			@SuppressWarnings("unchecked")
			RacerList<RacerList<RacerList<RacerSymbol>>> res2 = 
			(RacerList<RacerList<RacerList<RacerSymbol>>>) racer
			.racerAnswerQuery$("(?x ?y)",
					"(and (?x #!:person) (?x ?y #!:has_pet))");

			for (RacerList<RacerList<RacerSymbol>> bindings : res2) {
				for (RacerList<RacerSymbol> binding : bindings) {
					for (RacerSymbol varval : binding) {
						System.out.println(varval);
					}
				}
			}

		} catch (Exception e) {

			e.printStackTrace();

		}

	}

}
