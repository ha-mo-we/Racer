package com.racersystems.racer.tcp;

import com.racersystems.racer.ReasonerFactory;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.*;

public class EvaluationSemintec {

		/**
	 * @param args
	 */
	public static void main(String[] args) {



	    String file_uri;
		if (args.length == 0)
            file_uri = "file:///home/stefan/.workspace/u2r3/ontologien/semintec-0.owl";
        else
            file_uri =  args[0];
            String ONTO_URI = "http://www.informatik.uni-ulm.de/ki/aboxbench/semintec/semintec.owl#";

		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		OWLDataFactory factory = manager.getOWLDataFactory();

		long start;
		long end;

		OWLOntology ont = null;
		try {
			ont = manager.loadOntology(IRI.create(file_uri));
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		}

        ConsoleProgressMonitor progressMonitor = new ConsoleProgressMonitor();
	    OWLReasonerConfiguration config = new SimpleConfiguration(progressMonitor);

        OWLReasonerFactory reasonerFactory = new ReasonerFactory();
		start = System.currentTimeMillis();
        // Exception: RacerReasonerConfiguration is expected when using config
        OWLReasoner reasoner = reasonerFactory.createReasoner(ont);
		end = System.currentTimeMillis();
		System.out.println("Reasoner created and ontology loaded, " + (end-start) + "ms");

        // ParseException: Encountered "\"2.0\"" at line 9, column 1.
        //Version rv = reasoner.getReasonerVersion();
        //System.out.println("Reasoner: " + reasoner.getReasonerName() + " " + rv.getMajor() + "." +
        //rv.getMinor() + "." + rv.getPatch() + " build: " + rv.getBuild());

		start = System.currentTimeMillis();
		reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS,InferenceType.OBJECT_PROPERTY_ASSERTIONS,InferenceType.DATA_PROPERTY_ASSERTIONS);
		end = System.currentTimeMillis();

		System.out.println("REASONING DONE, " + (end-start) + "ms");


		NodeSet<OWLNamedIndividual> res;

//		 1. Instanzen von NoProblemsFinishedLoan
		start = System.currentTimeMillis();
		res = reasoner.getInstances(factory.getOWLClass(IRI.create(ONTO_URI + "NoProblemsFinishedLoan")), true);
		end = System.currentTimeMillis();
		System.out.println("Instances for NoProblemsFinishedLoan: " + (end-start) + "ms, #" + res.getFlattened().size());

//		 2. FŸller von (inv hasSexValue) zu svMaleSex
		start = System.currentTimeMillis();
		res = reasoner.getObjectPropertyValues(factory.getOWLNamedIndividual(IRI.create(ONTO_URI + "svMaleSex")), factory.getOWLObjectProperty(IRI.create(ONTO_URI + "hasSexValue")).getInverseProperty());
		end = System.currentTimeMillis();
        // Exception
		System.out.println("Filler of (inv hasSexValue) to svMaleSex: " + (end-start) + "ms, #" + res.getFlattened().size());
	}

}


