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
 * Date: 03.12.2010
 */
public class RacerObjectPropertyTest extends AbstractReasonerOntologyCase {
    @Override
    protected OWLOntology createRootOntology() throws OWLOntologyCreationException {
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        OWLOntology ontology = manager.createOntology();
        manager.addAxiom(ontology,getDataFactory().getOWLSubObjectPropertyOfAxiom(getOWLObjectProperty("A"), getDataFactory().getOWLTopObjectProperty()));
     //   manager.addAxiom(ontology,getDataFactory().getOWLSubObjectPropertyOfAxiom(getOWLObjectProperty("B"), getDataFactory().getOWLTopObjectProperty()));
     //   manager.addAxiom(ontology,getDataFactory().getOWLSubObjectPropertyOfAxiom(getOWLObjectProperty("C"), getDataFactory().getOWLTopObjectProperty()));
     //   manager.addAxiom(ontology,getDataFactory().getOWLSubObjectPropertyOfAxiom(getOWLObjectProperty("C"), getOWLObjectProperty("A")));
        
        return ontology;
    }

    public void testClassifyObjectProperties() throws Exception {
        reasoner.precomputeInferences(InferenceType.OBJECT_PROPERTY_HIERARCHY);
    }

}