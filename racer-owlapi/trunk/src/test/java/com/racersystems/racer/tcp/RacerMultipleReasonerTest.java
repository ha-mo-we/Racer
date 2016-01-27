package com.racersystems.racer.tcp;

import com.racersystems.racer.Racer;
import com.racersystems.racer.ReasonerFactory;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 17.12.2010
 */
public class RacerMultipleReasonerTest extends AbstractReasonerOntologyCase {

    @Override
    protected OWLOntology createRootOntology() throws OWLOntologyCreationException {
        OWLOntology ont = manager.createOntology();
        OWLClass A = getOWLClass("A");
        OWLClass B = getOWLClass("B");
        OWLClass C = getOWLClass("C");
        manager.addAxiom(ont, getDataFactory().getOWLSubClassOfAxiom(B, A));
        manager.addAxiom(ont, getDataFactory().getOWLSubClassOfAxiom(C, B));
        return ont;
    }

    public void testCreateNewReasoner() throws Exception {
        OWLOntology ont = manager.createOntology();
        OWLClass A = getOWLClass("A");
        OWLClass B = getOWLClass("B");
        manager.addAxiom(ont, getDataFactory().getOWLSubClassOfAxiom(B, A));
        Racer reasoner = new ReasonerFactory().createReasoner(ont, getConfiguration());

        assertTrue(reasoner.getSubClasses(A, false).getFlattened().contains(B));
        assertTrue(this.reasoner.getSubClasses(A, false).getFlattened().contains(B));
    }

   
}
