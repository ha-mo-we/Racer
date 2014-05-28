package com.racersystems.jracertest;
import com.racersystems.jracer.RacerClient;
import com.racersystems.jracer.RacerList;
import com.racersystems.jracer.RacerResult;
import com.racersystems.jracer.RacerSymbol;

public class RacerTest {

	public static void main(String[] args) {

		// please adjust this path to match your environment: 

		String peopleAndPets = "C:/people+pets.owl"; 

		String ip = "localhost";
		int port = 8088;

		RacerClient racer = new RacerClient(ip,port);		

		try {

			racer.openConnection();

			racer.loggingOn();
			racer.sendRaw("(logging-on)");

			racer.fullReset();

			racer.owlReadFile("\""+peopleAndPets+"\""); 

			System.out.println(racer.taxonomy()); 
			System.out.println(racer.taxonomy$()); 

			RacerList<RacerList>
			res1 = (RacerList<RacerList>)
			racer.taxonomy$(); 

			for (RacerList triple : res1) {
				System.out.println("--------"); 
				System.out.println("Concept : "+triple.getValue().get(0)); 
				System.out.println("Parents : "+triple.getValue().get(1)); 
				System.out.println("Children: "+triple.getValue().get(2)); 
			} 

			System.out.println(racer.currentAbox()); 
			System.out.println(racer.currentAbox$()); 

			racer.instanceM("i","C"); 

			System.out.println(racer.aboxConsistentP()); 
			System.out.println(racer.aboxConsistentP(racer.currentAbox())); 
			System.out.println(racer.aboxConsistentMP(racer.currentAbox())); 
			System.out.println(racer.aboxConsistentP(racer.currentAbox$())); 
			System.out.println(racer.aboxConsistentMP(racer.currentAbox$())); 

			System.out.println(racer.racerAnswerQuery("(?x ?y)",
					"(and (?x #!:person) (?x ?y #!:has_pet))")); 

			RacerList<RacerList<RacerList<RacerSymbol>>>
			res2 = (RacerList<RacerList<RacerList<RacerSymbol>>>)
			racer.racerAnswerQuery$("(?x ?y)","(and (?x #!:person) (?x ?y #!:has_pet))"); 

			for (RacerList<RacerList<RacerSymbol>> bindings : res2) {
				for (RacerList<RacerSymbol> binding : bindings) {
					for (RacerSymbol varval : binding) {
						System.out.println(varval);
					}
				}
			} 


			System.out.println(racer.retrieveM$("(?x ?y)",
					"(and (?x #!:person) (?x ?y #!:has_pet))")); 

			RacerList<RacerList<RacerList<RacerSymbol>>>
			res2b = (RacerList<RacerList<RacerList<RacerSymbol>>>)
			racer.racerAnswerQuery$("(?x ?y)","(and (?x #!:person) (?x ?y #!:has_pet))"); 

			for (RacerList<RacerList<RacerSymbol>> bindings : res2b) {
				for (RacerList<RacerSymbol> binding : bindings) {
					for (RacerSymbol varval : binding) {
						System.out.println(varval);
					}
				}
			}

			System.out.println(racer.describeAllQueries()); 
			System.out.println(racer.describeAllQueries(true)); 
			System.out.println(racer.describeAllQueries(false)); 
			System.out.println(racer.describeAllQueries$(true)); 
			System.out.println(racer.describeAllQueries$("nil")); 
			System.out.println(racer.describeAllQueries$("t")); 
			System.out.println(racer.describeAllQueries$(new RacerSymbol("nil"))); 
			System.out.println(racer.describeAllQueries$(new RacerSymbol("t"))); 
			System.out.println(racer.describeAllQueries(new RacerSymbol("nil"))); 
			System.out.println(racer.describeAllQueries(new RacerSymbol("t"))); 

			RacerList res3 = (RacerList)
					racer.racerAnswerQuery$("(?x)",
							"(?x #!:person)",
							":how-many",3,
							":exclude-permutations",true);

			System.out.println(res3); 

			String res4 = 
					racer.racerAnswerQuery("(?x)",
							"(?x #!:person)",
							":how-many",3,
							":exclude-permutations",true);

			System.out.println(res4); 

			RacerList<RacerList<RacerResult>>
			res5 = (RacerList<RacerList<RacerResult>>)
			racer.allConceptAssertions$(); 

			for (RacerList<RacerResult> ass : res5) {
				System.out.println("-----------");
				System.out.println("Individual: "+ass.getValue().get(0)); 
				System.out.println("Concept   : "+ass.getValue().get(1)+" of "+ass.getValue().get(1).getClass()); 

			}

			RacerList<RacerList<RacerResult>>
			res6 = (RacerList<RacerList<RacerResult>>)
			racer.allAnnotationConceptAssertions$(); 

			for (RacerList<RacerResult> ass : res6) {
				System.out.println("-----------");
				System.out.println("Individual: "+ass.getValue().get(0)); 
				System.out.println("Concept   : "+ass.getValue().get(1)+" of "+ass.getValue().get(1).getClass()); 

			}

			racer.withUniqueNameAssumption(); 

			racer.aboxConsistentP(); 

			racer.withNrqlSettings(":how-many-tuples",1); 
			System.out.println(racer.racerAnswerQuery$("(?x)","(?x #!:person)")); 

			racer.endWithNrqlSettings(); 

			System.out.println(racer.racerAnswerQuery$("(?x)","(?x #!:person)")); 

			RacerList<RacerSymbol> head = new RacerList();
			head.getValue().add(new RacerSymbol("?x")); 
			RacerList body = (RacerList)racer.parseRacerAnswer("(?x #!:person)"); 
			System.out.println(racer.racerAnswerQuery$(head,body)); 
			racer.endWithUniqueNameAssumption(); 

			System.out.println(racer.racerAnswerQuery(head,body)); 

			// sometimes, type casts and dynamic type checks can't be avoided: 

			for(String concept : new String[] {"top","#!:person","#!:mad_cow","bottom"}) {

				System.out.println(); 
				System.out.println("Retrieving instances of "+concept+":"); 

				RacerResult 
				res7 = (RacerResult)
				racer.conceptInstancesM$(concept); 

				if (res7 instanceof RacerSymbol) { // no instances? nil can't be cast in a RacerList!
					System.out.println("No instances!"); 
				} else {
					for (RacerSymbol ind : (RacerList<RacerSymbol>) res7) {
						System.out.println(ind); 
					}
				}
			}

			System.out.println(racer.evaluateM$("(+ 1 2)")); 

			racer.mirror("\"http://www.people+pets.owl/people+pets.owl\"","\"file://"+peopleAndPets+"\"");

			racer.owlReadDocument("\"http://www.people+pets.owl/people+pets.owl\"");

			racer.fullReset();

			racer.mirror(racer.RacerStringArgument("http://www.people+pets.owl/people+pets.owl"),
					racer.RacerStringArgument("file://"+peopleAndPets));

			racer.owlReadDocument(racer.RacerStringArgument("http://www.people+pets.owl/people+pets.owl"));

			System.out.println(racer.describeAbox(racer.currentAbox())); 

			System.out.println(racer.describeAbox(racer.RacerSymbolArgument("file://"+peopleAndPets))); 

			System.out.println(racer.racerAnswerQuery(
					"(?x ?y)",
					"(and (?x #!:person) (?x ?y #!:has_pet))")); 

			System.out.println(racer.describeQuery(racer.RacerSymbolArgument(":last"),racer.RacerBooleanArgument(true)));

			// System.out.println(racer.exitServer());

		} catch (Exception e) {

			e.printStackTrace();

		}

	}

}


