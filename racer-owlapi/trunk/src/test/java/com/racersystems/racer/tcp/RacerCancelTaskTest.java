package com.racersystems.racer.tcp;

import com.racersystems.racer.Configuration;
import com.racersystems.racer.ReasonerConfiguration;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.ConsoleProgressMonitor;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 19.09.2011
 */
public class RacerCancelTaskTest extends AbstractReasonerOntologyCase {
    protected ReasonerProgressMonitor monitor;

    @Override
    public void setUp() throws Exception {
        this.monitor = new ConsoleProgressMonitor();
        super.setUp();

    }

    @Override
    protected OWLOntology createRootOntology() throws OWLOntologyCreationException {
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        OWLOntology ontology = manager.loadOntology(IRI.create("file:///Users/noppens/Ontologien/VAST2009-UNA-easy.owl"));
        return ontology;
    }


    @Override
    protected Configuration getConfiguration() {
        Configuration conf = new Configuration();
        conf.setReasonerProgressMonitor(monitor);
        return conf;
    }

    public void testCancel() throws Exception {
        Timer timer = new Timer(2000, new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                System.out.println("interrupt");
                reasoner.interrupt();
            }
        });
        timer.start();
        reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS);
        
    }

    public void testClassifyHierarchy() throws Exception {
       reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY);
    }

     public void testRealize() throws Exception {
       reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS);
    }

    public void testEquivalentClasses() throws Exception {
        reasoner.getEquivalentClasses(getDataFactory().getOWLClass(IRI.create("http://www.informatik.uni-ulm.de/ki/Liebig/owl/mondial.owl#Country")));
    }


}