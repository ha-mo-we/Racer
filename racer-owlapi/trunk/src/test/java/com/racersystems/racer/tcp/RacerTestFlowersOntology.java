package com.racersystems.racer.tcp;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.NodeSet;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Test case for a Racer-Error
 * "An error condition has been signalled: role
                                       *top-object-role*
                                       already
                                       encoded
 *
 * Author: Olaf Noppens
 * Date: 03.12.2010
 */
public class RacerTestFlowersOntology extends AbstractReasonerOntologyCase {
    @Override
    protected OWLOntology createRootOntology() throws OWLOntologyCreationException {
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        OWLOntology ontology = manager.loadOntology(IRI.create("http://www.informatik.uni-ulm.de/ki/Liebig/ontologies/flowers.owl"));

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
