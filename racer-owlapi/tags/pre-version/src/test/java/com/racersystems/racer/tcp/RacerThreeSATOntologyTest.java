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
public class RacerThreeSATOntologyTest extends AbstractReasonerOntologyCase{
    @Override
        protected OWLOntology createRootOntology() throws OWLOntologyCreationException {
            OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
            OWLOntology ontology = manager.loadOntology(IRI.create("http://owl.cs.manchester.ac.uk/repository/download?ontology=http://www.semanticweb.org/ontologies/Three-SAT"));

            return ontology;
        }

        public void testClassifyHierarchy() throws Exception {
            reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY);
        }

}
