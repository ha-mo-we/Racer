package com.racersystems.jracertest;
import java.util.Timer;
import java.util.TimerTask;

import com.racersystems.jracer.RacerClient;

public class RacerTest9 {

	public static void main(String[] args) {

		// please adjust this path to match your environment: 

		String cyc = "\"file:///home/mi.wessel/racer-support/jracer-2-0/demo/cyc.owl\""; 
		String ip = "localhost";
		int port = 8088;
		int controlPort = 8089;

		String[][] allTests = { 

				// Test1: load axioms using OWLAPI-sequence 
				// this is from a log of the P4 adapter 
				{ "(OWLAPI-newReasoner testReasoner)",
					"(OWLAPI-newOntology testReasoner testReasoner)",
					"(OWLAPI-loadOntology testReasoner testReasoner)",
					"(OWLAPI-autoBatchAddAxiomsTo testReasoner testReasoner)",

					// we have 178 axioms to transmit to Racer
					// The progress bar will go from 0% to 90%. 
					// So, 90% of the progress bar will be filled
					// by the transmission process. After that comes
					// the actual loading by Racer (see OWLAPI-batchSynchronize). 

					"(OWLAPI-setProgressRange 178 0 90 testReasoner)", 

					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |company|))",
					"(OWLAPI-getOWLSubClassAxiom |newspaper| (or |broadsheet| |tabloid|))",
					"(OWLAPI-getOWLSubClassAxiom |giraffe| |animal|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |truck|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |tabloid|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |white_van_man|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |kid|))",
					"(OWLAPI-getOWLObjectPropertyAssertionAxiom |Walt| |has_pet| |Dewey|)",
					"(OWLAPI-getOWLSubClassAxiom |dog| (some |eats| |bone|))",
					"(OWLAPI-getOWLClassAssertionAxiom |Q123_ABC| |white_thing|)",
					"(OWLAPI-getOWLInverseObjectPropertiesAxiom |eaten_by| |eats|)",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|dog_liker| (and |person| (some |likes| |dog|))))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |giraffe|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |cat_owner|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |woman|))",
					"(OWLAPI-getOWLObjectPropertyAssertionAxiom |Fred| |has_pet| |Tibbs|)",
					"(OWLAPI-getOWLSubClassAxiom |car| |vehicle|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |tiger|))",
					"(OWLAPI-getOWLClassAssertionAxiom |Kevin| |person|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |bicycle|))",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|haulage_worker| (some |works_for| (or |haulage_company| (some |part_of| |haulage_company|)))))",
					"(OWLAPI-getOWLObjectPropertyAssertionAxiom |Minnie| |has_pet| |Tom|)",
					"(OWLAPI-getOWLObjectSubPropertyAxiom |has_mother| |has_parent|)",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|grownup| (and |adult| |person|)))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |pet_owner|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (ObjectProperty |has_pet|))",
					"(OWLAPI-getOWLSubClassAxiom |elderly| |adult|)",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|cat_liker| (and |person| (some |likes| |cat|))))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |old_lady|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |newspaper|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (ObjectProperty |drives|))",
					"(OWLAPI-getOWLClassAssertionAxiom |Tom| top)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |mad_cow|))",
					"(OWLAPI-getOWLSubClassAxiom |bus_company| |company|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |vegetarian|))",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|pet| (some |is_pet_of| top)))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |publication|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |van_driver|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (DataProperty |service_number|))",
					"(OWLAPI-getOWLSubClassAxiom |magazine| |publication|)",
					"(OWLAPI-getOWLObjectPropertyAssertionAxiom |Joe| |has_pet| |Fido|)",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|vegetarian| (and |animal| (all |eats| (not |animal|)) (all |eats| (not (some |part_of| |animal|))))))",
					"(OWLAPI-getOWLSubClassAxiom |van| |vehicle|)",
					"(OWLAPI-getOWLClassAssertionAxiom |Tibbs| |cat|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |haulage_company|))",
					"(OWLAPI-getOWLClassAssertionAxiom |Walt| |person|)",
					"(OWLAPI-getOWLSubClassAxiom |leaf| (some |part_of| |tree|))",
					"(OWLAPI-getOWLSubClassAxiom |truck| |vehicle|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |broadsheet|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |brain|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (ObjectProperty |has_father|))",
					"(OWLAPI-getOWLClassAssertionAxiom |Mick| |male|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |cow|))",
					"(OWLAPI-getOWLDisjointClassesAxiom (|cat| |dog|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |sheep|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |car|))",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|lorry_driver| (and |person| (some |drives| |lorry|))))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |Axiom_2|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |bus_company|))",
					"(OWLAPI-getOWLSubClassAxiom |quality_broadsheet| |broadsheet|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (ObjectProperty |eats|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |grass|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (ObjectProperty |eaten_by|))",
					"(OWLAPI-getOWLSubClassAxiom |haulage_company| |company|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |pet|))",
					"(OWLAPI-getOWLSubClassAxiom |sheep| |animal|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |dog_liker|))",
					"(OWLAPI-getOWLObjectPropertyAssertionAxiom |Rex| |is_pet_of| |Mick|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |dog|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |quality_broadsheet|))",
					"(OWLAPI-getOWLClassAssertionAxiom |Joe| |person|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |man|))",
					"(OWLAPI-getOWLObjectPropertyRangeAxiom |has_pet| |animal|)",
					"(OWLAPI-getOWLSubClassAxiom |bicycle| |vehicle|)",
					"(OWLAPI-getOWLClassAssertionAxiom |Dewey| |duck|)",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|old_lady| (and |elderly| |female| |person|)))",
					"(OWLAPI-getOWLSubClassAxiom |tabloid| |newspaper|)",
					"(OWLAPI-getOWLSubClassAxiom |animal| (some |eats| top))",
					"(OWLAPI-getOWLClassAssertionAxiom |Flossie| |cow|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |animal|))",
					"(OWLAPI-getOWLObjectPropertyRangeAxiom |has_mother| |woman|)",
					"(OWLAPI-getOWLSubClassAxiom |white_van_man| (all |reads| |tabloid|))",
					"(OWLAPI-getOWLObjectPropertyRangeAxiom |reads| |publication|)",
					"(OWLAPI-getOWLSubClassAxiom |lorry| |vehicle|)",
					"(OWLAPI-getOWLSubClassAxiom |giraffe| (all |eats| |leaf|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |cat|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (ObjectProperty |has_mother|))",
					"(OWLAPI-getOWLClassAssertionAxiom |The_Times| |broadsheet|)",
					"(OWLAPI-getOWLClassAssertionAxiom |Huey| |duck|)",
					"(OWLAPI-getOWLClassAssertionAxiom |Joe| (at-most 1 |has_pet|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |adult|))",
					"(OWLAPI-getOWLClassAssertionAxiom |Daily_Mirror| top)",
					"(OWLAPI-getOWLClassAssertionAxiom |Fred| |person|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |young|))",
					"(OWLAPI-getOWLSubClassAxiom |newspaper| |publication|)",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|pet_owner| (and |person| (some |has_pet| |animal|))))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (ObjectProperty |likes|))",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|cat_owner| (and |person| (some |has_pet| |cat|))))",
					"(OWLAPI-getOWLSubClassAxiom |grass| |plant|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (ObjectProperty |has_child|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |cat_liker|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (ObjectProperty |works_for|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |dog_owner|))",
					"(OWLAPI-getOWLObjectSubPropertyAxiom |has_pet| |likes|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |bus_driver|))",
					"(OWLAPI-getOWLObjectSubPropertyAxiom |has_father| |has_parent|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |lorry|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |leaf|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |grownup|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |elderly|))",
					"(OWLAPI-getOWLDisjointClassesAxiom (|adult| |young|))",
					"(OWLAPI-getOWLSubClassAxiom |red_top| |tabloid|)",
					"(OWLAPI-getOWLClassAssertionAxiom |The_Guardian| |broadsheet|)",
					"(OWLAPI-getOWLSubClassAxiom |tiger| |animal|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |driver|))",
					"(OWLAPI-getOWLSubClassAxiom |old_lady| (and (some |has_pet| |animal|) (all |has_pet| |cat|)))",
					"(OWLAPI-getOWLClassAssertionAxiom |Minnie| |female|)",
					"(OWLAPI-getOWLSubClassAxiom |person| |animal|)",
					"(OWLAPI-getOWLInverseObjectPropertiesAxiom |has_pet| |is_pet_of|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |female|))",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|kid| (and |person| |young|)))",
					"(OWLAPI-getOWLDisjointClassesAxiom (|broadsheet| |tabloid|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |duck|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |male|))",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|man| (and |adult| |male| |person|)))",
					"(OWLAPI-getOWLDisjointClassesAxiom (|Axiom_2| (or |plant| (some |part_of| |plant|))))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |person|))",
					"(OWLAPI-getOWLSubClassAxiom |bus| |vehicle|)",
					"(OWLAPI-getOWLClassAssertionAxiom |Fluffy| |tiger|)",
					"(OWLAPI-getOWLObjectPropertyRangeAxiom |has_father| |man|)",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|mad_cow| (and |cow| (some |eats| (and |brain| (some |part_of| |sheep|))))))",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|haulage_truck_driver| (and |person| (some |drives| |truck|) (some |works_for| (some |part_of| |haulage_company|)))))",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|Axiom_2| (or |animal| (some |part_of| |animal|))))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (ObjectProperty |has_part|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |bus|))",
					"(OWLAPI-getOWLObjectPropertyAssertionAxiom |Walt| |has_pet| |Huey|)",
					"(OWLAPI-getOWLInverseObjectPropertiesAxiom |has_part| |part_of|)",
					"(OWLAPI-getOWLObjectPropertyDomainAxiom |eats| |animal|)",
					"(OWLAPI-getOWLSubClassAxiom |broadsheet| |newspaper|)",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|driver| (and |person| (some |drives| |vehicle|))))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |red_top|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (ObjectProperty |reads|))",
					"(OWLAPI-getOWLSubClassAxiom |sheep| (all |eats| |grass|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |animal_lover|))",
					"(OWLAPI-getOWLClassAssertionAxiom |Minnie| |elderly|)",
					"(OWLAPI-getOWLObjectPropertyAssertionAxiom |Mick| |drives| |Q123_ABC|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |tree|))",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|woman| (and |adult| |female| |person|)))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |haulage_worker|))",
					"(OWLAPI-getOWLObjectPropertyAssertionAxiom |Walt| |has_pet| |Louie|)",
					"(OWLAPI-getOWLClassAssertionAxiom |Louie| |duck|)",
					"(OWLAPI-getOWLSubClassAxiom |driver| |adult|)",
					"(OWLAPI-getOWLSubClassAxiom |cat| |animal|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |haulage_truck_driver|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |white_thing|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |magazine|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (ObjectProperty |has_parent|))",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|white_van_man| (and |man| (some |drives| (and |van| |white_thing|)))))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |vehicle|))",
					"(OWLAPI-getOWLObjectPropertyAssertionAxiom |Mick| |reads| |Daily_Mirror|)",
					"(OWLAPI-getOWLObjectPropertyDomainAxiom |has_pet| |person|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |bone|))",
					"(OWLAPI-getOWLClassAssertionAxiom |Rex| |dog|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |van|))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |lorry_driver|))",
					"(OWLAPI-getOWLClassAssertionAxiom |Q123_ABC| |van|)",
					"(OWLAPI-getOWLSubClassAxiom |cow| |vegetarian|)",
					"(OWLAPI-getOWLSubClassAxiom |duck| |animal|)",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|dog_owner| (and |person| (some |has_pet| |dog|))))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (ObjectProperty |part_of|))",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|animal_lover| (and |person| (at-least 3 |has_pet|))))",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (ObjectProperty |is_pet_of|))",
					"(OWLAPI-getOWLSubClassAxiom |tree| |plant|)",
					"(OWLAPI-getOWLImplicitDeclarationAxiom (OWLClass |plant|))",
					"(OWLAPI-getOWLDataPropertyRangeAxiom |service_number| (d-base-type |http://www.w3.org/2001/XMLSchema#integer|))",
					"(OWLAPI-getOWLClassAssertionAxiom |Fido| |dog|)",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|van_driver| (and |person| (some |drives| |van|))))",
					"(OWLAPI-getOWLEquivalentClassesAxiom (|bus_driver| (and |person| (some |drives| |bus|))))",

					// set the progress bar to the 90 to 100 % range, 
					// for 178 Axioms which are to be loaded internally
					// by Racer. This is done by BatchSynchronize. 

					"(OWLAPI-setProgressRange 178 90 100 testReasoner)", 
					"(OWLAPI-batchSynchronize testReasoner testReasoner)",
					"(OWLAPI-classify testReasoner nil)" 

				}, 

				// Test2: load axioms from OWL file (cyc.owl)

				{ "(full-reset)", 
					"(OWLAPI-newReasoner testReasoner)",
					"(OWLAPI-readOntology "+cyc+" :kb-name testReasoner)", 
					"(OWLAPI-classify testReasoner nil)" // nil = no ABox consistency check
				}

		};

		RacerClient racer = new RacerClient(ip,port);		
		final RacerClient racer2 = new RacerClient(ip,controlPort);		

		try {


			racer2.openConnection();

			TimerTask progressChecker = new TimerTask() {

				boolean abortSent = false;

				public void run() {
					try{ 

						System.out.println();

						System.out.print("    Progress: "); 
						racer2.out.println("(OWLAPI-getProgress testReasoner)");
						racer2.out.flush();
						int progress = new Integer(racer2.in.readLine()); 
						System.out.println(progress);

						System.out.print("    Command : "); 
						racer2.out.println("(OWLAPI-getCurrentRequest testReasoner)");
						racer2.out.flush();
						String request = racer2.in.readLine(); 
						System.out.println(request);

						// test of OWLAPI-abort: abort OWLAPI-classify after 50% 

						if ((progress > 10) && (! abortSent) && (request.equals("(OWLAPI-classify testReasoner nil)"))) {
							abortSent = true;
							System.out.print("Sending abort and receiving over CPort: "); 
							racer2.out.println("(OWLAPI-abort testReasoner)");
							racer2.out.flush();
							System.out.println(racer2.in.readLine()); 
						};			  

						System.out.println();

					} catch (Exception e) {}
				}
			};

			Timer timer = new Timer();
			timer.schedule(progressChecker,0,1000);

			racer.openConnection();

			// racer.loggingOn$();
			// racer.sendRaw("(logging-on)");	

			for (String[] test : allTests) {
				racer.fullReset$();
				for (String command : test) {
					System.out.println("Sending: "+command); 
					String res = racer.sendRaw(command); 
					System.out.println("Result received: "+res);
					Thread.sleep(100); 
				}

				/* boolean consistent = racer.aboxConsistentP("testReasoner"); 
		System.out.println("ABox Consistent: "+consistent); 

		RacerList<RacerList>
		    res1 = (RacerList<RacerList>)
		    racer.taxonomy$("testReasoner"); 

		for (RacerList triple : res1) {
		    System.out.println("--------"); 
		    System.out.println("Concept : "+triple.getValue().get(0)); 
		    System.out.println("Parents : "+triple.getValue().get(1)); 
		    System.out.println("Children: "+triple.getValue().get(2)); 
		    } 	 */     
			}

			progressChecker.cancel();

			System.out.println("Done."); 

			System.exit(0);

		} catch (Exception e) {

			e.printStackTrace();

		}

	}

}

