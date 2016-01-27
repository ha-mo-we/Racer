package com.racersystems.racer.tcp;

import com.racersystems.racer.RacerReasoner;
import com.racersystems.racer.RacerReasonerConfiguration;
import com.racersystems.racer.RacerReasonerFactory;
import junit.framework.TestCase;
import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.BufferingMode;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;

import java.io.File;
import java.util.Iterator;
import java.util.Set;

/**
 * Author: Olaf Noppens         ^
 * Date: 16.09.2010
 */
public abstract class AbstractReasonerOntologyCase extends TestCase {
    protected Logger logger;
    protected RacerReasoner reasoner;
    protected OWLDataFactory dataFactory;
    protected OWLOntologyManager manager;
    protected String NS = "test";
    protected OWLOntology rootOntology;

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
                this.reasoner = new RacerReasonerFactory().createReasoner(rootOntology, getConfiguration());
                break;
            default:
                this.reasoner = new RacerReasonerFactory().createNonBufferingReasoner(rootOntology, getConfiguration());
        }
        logger.info("...RacerTCPReasoner created");
    }

    /**
     * Returns the BufferingMode to be used for the U2R3Reasoner.
     * <blockquote>
     * Note that the BufferingMode will be set during {@link #setUp()}
     * </blockquote>
     *
     * @return BufferingMode to be set for U2R3
     */
    protected BufferingMode getBufferingMode() {
        return BufferingMode.NON_BUFFERING;
    }

    protected RacerReasonerConfiguration getConfiguration() {
        return new RacerReasonerConfiguration();
    }

    public void tearDown() throws Exception {
        if (reasoner != null)
            reasoner.dispose();
        this.manager.removeOntology(this.rootOntology);
        this.rootOntology = null;
        this.reasoner = null;
    }

    protected abstract OWLOntology createRootOntology() throws OWLOntologyCreationException;

    public OWLOntologyManager getManager() {
        return this.manager;
    }


    public OWLOntology getRootOntology() {
        return this.rootOntology;
    }

    public OWLDataFactory getDataFactory() {
        return getManager().getOWLDataFactory();
    }


    public OWLClass getOWLClass(String name) {
        return getDataFactory().getOWLClass(IRI.create(NS + "#" + name));
    }


    public OWLDataProperty getOWLDataProperty(String name) {
        return getDataFactory().getOWLDataProperty(IRI.create(NS + "#" + name));
    }

    public OWLObjectProperty getOWLObjectProperty(String name) {
        return getDataFactory().getOWLObjectProperty(IRI.create(NS + "#" + name));
    }

    public OWLNamedIndividual getOWLIndividual(String name) {
        return getDataFactory().getOWLNamedIndividual(IRI.create(NS + "#" + name));
    }

    public OWLDatatype getOWLDatatype(String name) {
        return getDataFactory().getOWLDatatype(IRI.create(NS + "#" + name));
    }

    public OWLAnnotationProperty getOWLAnnotationProperty(String name) {
        return getDataFactory().getOWLAnnotationProperty(IRI.create(NS + "#" + name));
    }

    public OWLLiteral getLiteral(int value) {
        return getDataFactory().getOWLTypedLiteral(value);
    }

    public void addAxioms(OWLOntology ont, Set<OWLAxiom> axioms) {
        manager.addAxioms(ont, axioms);
    }

    public void removeAxioms(OWLOntology ont, Set<OWLAxiom> axioms) {
        manager.removeAxioms(ont, axioms);
    }

    /**
     * Adds an import declaration for the given ontology wrt. {@link #getRootOntology()}
     *
     * @param ont
     */
    public void addOntology(OWLOntology ont) {
        manager.applyChange(new AddImport(ont, getDataFactory().getOWLImportsDeclaration(ont.getOntologyID().getOntologyIRI())));
    }

    /**
     * @param ont
     * @see #addOntology(org.semanticweb.owlapi.model.OWLOntology)
     */
    public void removeOntology(OWLOntology ont) {
        manager.applyChange(new RemoveImport(ont, getDataFactory().getOWLImportsDeclaration(ont.getOntologyID().getOntologyIRI())));
    }


    public void addAxiom(OWLOntology ont, OWLAxiom ax) {
        try {
            manager.addAxiom(ont, ax);
        }
        catch (OWLOntologyChangeException e) {
            fail(e.getMessage() + " " + e.getStackTrace().toString());
        }
    }

    public void removeAxiom(OWLOntology ont, OWLAxiom ax) {
        try {
            manager.removeAxiom(ont, ax);
        }
        catch (OWLOntologyChangeException e) {
            fail(e.getMessage() + " " + e.getStackTrace().toString());
        }
    }

    public static <E extends OWLLogicalEntity> void dump(NodeSet<E> nodeSet) {
        System.out.println("*********************");
        System.out.println("[");
        for (Node<E> node : nodeSet) {
            System.out.print("{");
            for (Iterator<E> iter = node.iterator(); iter.hasNext();) {
                System.out.print(iter.next());
                if (iter.hasNext())
                    System.out.print(", ");
            }
            System.out.println("}");
        }
        System.out.println("]");
    }


}
