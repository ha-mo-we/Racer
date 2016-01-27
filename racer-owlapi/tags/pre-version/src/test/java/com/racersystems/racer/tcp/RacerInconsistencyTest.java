package com.racersystems.racer.tcp;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.InferenceType;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 19.11.2010
 */
public class RacerInconsistencyTest extends AbstractReasonerOntologyCase {

    @Override
    protected OWLOntology createRootOntology() throws OWLOntologyCreationException {
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        OWLOntology ontology = manager.loadOntology(IRI.create("file:///Users/noppens/Ontologien/inconsistent-o.owl"));

        return ontology;
    }

    public void testClassifyHierarchy() throws Exception {
        reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS);
        assertFalse(reasoner.isConsistent());
    }


}