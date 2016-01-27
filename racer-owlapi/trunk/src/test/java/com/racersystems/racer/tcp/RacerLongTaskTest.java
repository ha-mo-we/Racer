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
        //OWLOntology ontology = manager.loadOntology(IRI.create("file:///Users/noppens/Ontologien/mondial-RL.owl"));
       // OWLOntology ontology = manager.loadOntology(IRI.create("file:///Users/noppens/Ontologien/people+pets.owl"));
       OWLOntology ontology = manager.loadOntology(IRI.create("file:///Users/noppens/Ontologien/10aPHP56+44b-ALCN.owl"));
        return ontology;
    }


    @Override
    protected Configuration getConfiguration() {
        Configuration conf = new Configuration();
        conf.setReasonerProgressMonitor(monitor);
        return conf;
    }

    public void testClassifyHierarchy() throws Exception {


       Thread t = new Thread(new Runnable() {
           public void run() {
               try {
                   Thread.currentThread().sleep(10000);
                   System.out.println("awake, trying to interrupt");
                   reasoner.interrupt();
                    System.out.println("interrrupted");
               } catch (InterruptedException e) {
                   e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
               }
           }
       });
        t.start();

       reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY);
    }

     public void testRealize() throws Exception {
       reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS);
    }

    public void testEquivalentClasses() throws Exception {
        reasoner.getEquivalentClasses(getDataFactory().getOWLClass(IRI.create("http://www.informatik.uni-ulm.de/ki/Liebig/owl/mondial.owl#Country")));
    }

    
}                                                                        