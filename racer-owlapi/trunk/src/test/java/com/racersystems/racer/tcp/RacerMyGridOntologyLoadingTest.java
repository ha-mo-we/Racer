package com.racersystems.racer.tcp;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.FileDocumentSource;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.InferenceType;

import java.io.File;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 06.10.2011
 */
public class RacerMyGridOntologyLoadingTest extends AbstractReasonerOntologyCase {

    @Override
    protected OWLOntology createRootOntology() throws OWLOntologyCreationException {
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
      //  OWLOntology ontology = manager.loadOntologyFromOntologyDocument(new FileDocumentSource(new File("/Users/noppens/Ontologien/ontology.owx")));
          OWLOntology ontology = manager.loadOntologyFromOntologyDocument(new FileDocumentSource(new File("/Users/noppens/Ontologien/VAST2009-UNA-easy.owl")));

        return ontology;
    }

    public void testClassifyHierarchy() throws Exception {

    }


}