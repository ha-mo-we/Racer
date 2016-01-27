package com.racersystems.racer.tcp;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.InferenceType;

/**
 * Author: Olaf Noppens
 * Date: 05.10.2010
 */
public class RacerOntologyTest extends AbstractReasonerOntologyCase {
    @Override
    protected OWLOntology createRootOntology() throws OWLOntologyCreationException {
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        //OWLOntology ontology = manager.loadOntology(IRI.create("http://www.informatik.uni-ulm.de/ki/Liebig/owl/mondial.owl"));
    //    OWLOntology ontology = manager.loadOntology(IRI.create("http://www.informatik.uni-ulm.de/ki/Liebig/owl2rl/DomusAG-06-03-2010-X2.owl"));
//        OWLOntology ontology = manager.loadOntology(IRI.create("http://www.informatik.uni-ulm.de/ki/Liebig/owl2rl/VAST2009-RL.owl"));
      //  OWLOntology ontology = manager.loadOntology(IRI.create("http://www.informatik.uni-ulm.de/ki/Liebig/owl2rl/mondial-RL.owl"));
        //
//OWLOntology ontology = manager.loadOntology(IRI.create("file:///Users/noppens/Ontologien/DomusAG-06-03-2010-X2.owl"));
        OWLOntology ontology = manager.loadOntology(IRI.create("file:///Users/noppens/Ontologien/mondial.owl"));
        return ontology;
    }

    public void testClassifyHierarchy() throws Exception {
        reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY);
        reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS);
    }

    public void testEquivalentClasses() throws Exception {
        reasoner.getEquivalentClasses(getDataFactory().getOWLClass(IRI.create("http://www.informatik.uni-ulm.de/ki/Liebig/owl/mondial.owl#Country")));
    }
}
