package com.racersystems.racer;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.*;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * @author Olaf Noppens
 */
public class RacerReasonerFactory implements OWLReasonerFactory {
    public static final String RACERNAME = "Racer";

    public final String getReasonerName() {
        return RACERNAME;
    }

    public RacerReasoner createNonBufferingReasoner(OWLOntology ontology) {
        return new RacerReasonerImpl(ontology, new RacerReasonerConfiguration(), BufferingMode.NON_BUFFERING);
    }

    public RacerReasoner createReasoner(OWLOntology ontology) {
        return new RacerReasonerImpl(ontology, new RacerReasonerConfiguration(), BufferingMode.BUFFERING);
    }

    public RacerReasoner createNonBufferingReasoner(OWLOntology ontology, OWLReasonerConfiguration config) throws IllegalConfigurationException {
        if (config instanceof RacerReasonerConfiguration)
            return new RacerReasonerImpl(ontology, (RacerReasonerConfiguration) config, BufferingMode.NON_BUFFERING);
        throw new IllegalConfigurationException("RacerReasonerConfiguration is expected", config);
    }

    public RacerReasoner createReasoner(OWLOntology ontology, OWLReasonerConfiguration config) throws IllegalConfigurationException {
        if (config instanceof RacerReasonerConfiguration)
            return new RacerReasonerImpl(ontology, (RacerReasonerConfiguration) config, BufferingMode.BUFFERING);
        throw new IllegalConfigurationException("RacerReasonerConfiguration is expected", config);
    }

}
