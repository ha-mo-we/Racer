package com.racersystems.racer;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.*;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * @author Olaf Noppens
 */
public class ReasonerFactory implements OWLReasonerFactory {
    private static Reasoner.ReasonerFactory factory = new Reasoner.ReasonerFactory();

    
    public final String getReasonerName() {
       return factory.getReasonerName();
    }

    public Racer createNonBufferingReasoner(OWLOntology ontology) {
        return (Racer) factory.createNonBufferingReasoner(ontology);
    }

    public Racer createReasoner(OWLOntology ontology) {
        return (Racer) factory.createReasoner(ontology);
    }

    public Racer createNonBufferingReasoner(OWLOntology ontology, OWLReasonerConfiguration config) throws IllegalConfigurationException {
        return (Racer) factory.createNonBufferingReasoner(ontology, config);
    }

    public Racer createReasoner(OWLOntology ontology, OWLReasonerConfiguration config) throws IllegalConfigurationException {
        return (Racer) factory.createReasoner(ontology, config);
    }

}
