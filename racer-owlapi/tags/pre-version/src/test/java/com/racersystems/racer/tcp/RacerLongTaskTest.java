package com.racersystems.racer.tcp;

import com.racersystems.racer.RacerReasonerConfiguration;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.ConsoleProgressMonitor;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor;

/**
 * Author: Olaf Noppens
 * Date: 11.10.2010
 */
public class RacerLongTaskTest extends AbstractReasonerOntologyCase {
    protected ReasonerProgressMonitor monitor;

    @Override
    public void setUp() throws Exception {
        this.monitor = new ConsoleProgressMonitor();
        super.setUp();

    }

    @Override
    protected OWLOntology createRootOntology() throws OWLOntologyCreationException {
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        OWLOntology ontology = manager.loadOntology(IRI.create("file:///Users/noppens/Ontologien/mondial.owl"));

        return ontology;
    }


    @Override
    protected RacerReasonerConfiguration getConfiguration() {
        return new RacerReasonerConfiguration(monitor);
    }

    public void testClassifyHierarchy() throws Exception {
       reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY);
    }

    public void testEquivalentClasses() throws Exception {
        reasoner.getEquivalentClasses(getDataFactory().getOWLClass(IRI.create("http://www.informatik.uni-ulm.de/ki/Liebig/owl/mondial.owl#Country")));
    }
}                                                                        