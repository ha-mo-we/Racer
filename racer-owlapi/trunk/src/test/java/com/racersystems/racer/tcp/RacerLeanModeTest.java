package com.racersystems.racer.tcp;

import com.racersystems.racer.Configuration;
import com.racersystems.racer.ReasonerFactory;
import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.reasoner.NodeSet;

import java.io.File;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 22.10.2012
 */
public class RacerLeanModeTest extends AbstractReasonerOntologyCase {

     @Override
    protected Configuration getConfiguration() {
        Configuration conf = new Configuration();
        conf.setMemorySavingModeEnabled(true);
        return conf;
    }
    
    @Override
    protected OWLOntology createRootOntology() throws OWLOntologyCreationException {
        OWLOntology ont = manager.createOntology();
        OWLClass A = getOWLClass("A");
        OWLClass B = getOWLClass("B");
        OWLClass C = getOWLClass("C");

        manager.addAxiom(ont, getDataFactory().getOWLSubClassOfAxiom(B, A));
        return ont;
    }

    public void setUp() throws Exception {
           if ((new File("log4j.properties")).exists()) {
               PropertyConfigurator.configure("log4j.properties");
           } else {
               BasicConfigurator.configure();
           }
           Logger.getRootLogger().setLevel(Level.INFO);
           this.logger = Logger.getLogger(getClass());
           this.manager = OWLManager.createOWLOntologyManager();
           this.rootOntology = createRootOntology();
           logger.info("create RacerTCPReasoner...");
           switch (getBufferingMode()) {
               case BUFFERING:
                   this.reasoner = new ReasonerFactory().createReasoner(rootOntology, getConfiguration());
                   break;
               default:
                   this.reasoner = new ReasonerFactory().createNonBufferingReasoner(rootOntology, getConfiguration());
           }
           logger.info("...RacerTCPReasoner created");
       }

    public void test() throws Exception {

        NodeSet<OWLClass> classes =  reasoner.getSubClasses(getOWLClass("A"), false);
        reasoner.isConsistent();

    }

}
