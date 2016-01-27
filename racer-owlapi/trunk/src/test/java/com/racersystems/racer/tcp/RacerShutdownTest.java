package com.racersystems.racer.tcp;

import com.racersystems.racer.Reasoner;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 22.07.2011
 */
public class RacerShutdownTest extends AbstractReasonerOntologyCase {
    @Override
    protected OWLOntology createRootOntology() throws OWLOntologyCreationException {
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        OWLOntology ontology = manager.createOntology();
        manager.addAxiom(ontology,getDataFactory().getOWLSubObjectPropertyOfAxiom(getOWLObjectProperty("A"), getDataFactory().getOWLTopObjectProperty()));

        return ontology;
    }

    public void testDummy() throws Exception {
         Reasoner impl = (Reasoner) reasoner;
        impl.shutdown();
    }

    @Override
    public void tearDown() throws Exception {
        
        super.tearDown();

    }
}