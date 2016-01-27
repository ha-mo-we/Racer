package com.racersystems.racer.tcp;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.NodeSet;

import java.io.File;

/*******************************************************************************
 * Copyright (c) 2013 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 18.04.2013
 */
public class RacerTestFamilyOntology extends AbstractReasonerOntologyCase {
    @Override
    protected OWLOntology createRootOntology() throws OWLOntologyCreationException {
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        OWLOntology ontology = manager.loadOntologyFromOntologyDocument(new File("/Users/noppens/Ontologien/roberts-RL-family-full-D.owl.xml"));

        return ontology;
    }

    public void testClassifyHierarchy() throws Exception {
        reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY);
    }

    public void testGetSubClasses() throws Exception {
        NodeSet<OWLClass> subClasses = reasoner.getSubClasses(getDataFactory().getOWLThing(), false);
        subClasses = reasoner.getSubClasses(getDataFactory().getOWLThing(), true);
        assertFalse(subClasses.getFlattened().contains(getDataFactory().getOWLThing()));
    }

}
