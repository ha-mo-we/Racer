package com.racersystems.racer.tcp;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.util.Version;

import java.util.HashSet;
import java.util.Set;

/**
 * Author: Olaf Noppens
 * Date: 16.09.2010
 */
public class RacerInterfaceTest extends AbstractReasonerOntologyCase {

    @Override
    protected OWLOntology createRootOntology() throws OWLOntologyCreationException {
        OWLOntology ont = manager.createOntology();
        OWLClass A = getOWLClass("A");
        OWLClass B = getOWLClass("B");
        OWLClass C = getOWLClass("C");
        for (int i = 0; i < 0; i++) {
            manager.addAxiom(ont, getDataFactory().getOWLClassAssertionAxiom(B, getOWLIndividual("" + i + "B")));
            manager.addAxiom(ont, getDataFactory().getOWLClassAssertionAxiom(C, getOWLIndividual("" + i + "C")));
        }
        manager.addAxiom(ont, getDataFactory().getOWLSubClassOfAxiom(B, A));
        manager.addAxiom(ont, getDataFactory().getOWLSubClassOfAxiom(C, A));
        manager.addAxiom(ont, getDataFactory().getOWLSubObjectPropertyOfAxiom(getOWLObjectProperty("P"), getOWLObjectProperty("P2")));

        manager.addAxiom(ont, getDataFactory().getOWLSameIndividualAxiom(getOWLIndividual("j"), getOWLIndividual("k")));
        manager.addAxiom(ont, getDataFactory().getOWLObjectPropertyAssertionAxiom(getOWLObjectProperty("Q"), getOWLIndividual("i"), getOWLIndividual("j")));

        manager.addAxiom(ont, getDataFactory().getOWLClassAssertionAxiom(getOWLClass("D"), getOWLIndividual("j")));
        manager.addAxiom(ont, getDataFactory().getOWLClassAssertionAxiom(getOWLClass("D"), getOWLIndividual("l")));


        return ont;
    }

    /*public void testSpecialCharacters() throws Exception {
        Set<OWLAxiom> axioms = new HashSet<OWLAxiom>();
        String s2 = "http://Speci|alCharacter";
        axioms.add(getDataFactory().getOWLSubClassOfAxiom(getOWLClass("http://Speci|alCharacter"), getOWLClass("E")));
        manager.addAxioms(getRootOntology(), axioms);
        String s = reasoner.getSubClasses(getOWLClass("E"), true).getNodes().iterator().next().getRepresentativeElement().getIRI().toString();
        System.out.println(s.equals(s2));
    } */


    public void testGetSubClasses() throws Exception {
        assertTrue(reasoner.getSubClasses(getOWLClass("A"), true).containsEntity(getOWLClass("B")));
        assertTrue(reasoner.getSubClasses(getOWLClass("A"), true).containsEntity(getOWLClass("C")));
        assertTrue(reasoner.getSubClasses(getDataFactory().getOWLNothing(), true).isEmpty());
        /* manager.addAxiom(getRootOntology(), getDataFactory().getOWLSubClassOfAxiom(getOWLClass("X"), getOWLClass("Y")));
       assertTrue(reasoner.getSubClasses(getOWLClass("Y"), true).containsEntity(getOWLClass("X")));
       manager.removeAxiom(getRootOntology(), getDataFactory().getOWLSubClassOfAxiom(getOWLClass("B"), getOWLClass("A")));
       assertFalse(reasoner.getSubClasses(getOWLClass("A"), true).containsEntity(getOWLClass("B")));
       manager.removeAxiom(getRootOntology(), getDataFactory().getOWLSubClassOfAxiom(getOWLClass("X"), getOWLClass("Y")));
      // assertFalse(reasoner.getSubClasses(getOWLClass("Y"), true).containsEntity(getOWLClass("X")));
        */
        // assertTrue(reasoner.isEntailed(getDataFactory().getOWLSubClassOfAxiom(getOWLClass("B"), getOWLClass("A"))));
    }

    public void testEquivalentClasses() throws Exception {
        OWLObjectUnionOf union = getDataFactory().getOWLObjectUnionOf(getOWLClass("U1"), getOWLClass("U2"), getOWLClass("U3"));
        Set<OWLAxiom> axioms = new HashSet<OWLAxiom>();
        axioms.add(getDataFactory().getOWLEquivalentClassesAxiom(getOWLClass("A"), union));
        axioms.add(getDataFactory().getOWLEquivalentClassesAxiom(getOWLClass("A"), getOWLClass("D")));
        axioms.add(getDataFactory().getOWLEquivalentClassesAxiom(getOWLClass("B"), getDataFactory().getOWLNothing()));
        manager.addAxioms(getRootOntology(), axioms);

        assertTrue(reasoner.getUnsatisfiableClasses().contains(getOWLClass("B")));
        assertTrue(reasoner.getEquivalentClasses(getOWLClass("A")).contains(getOWLClass("D")));
        assertTrue(reasoner.getEquivalentClasses(getOWLClass("D")).contains(getOWLClass("A")));
        manager.removeAxioms(getRootOntology(), axioms);
        assertFalse(reasoner.getUnsatisfiableClasses().contains(getOWLClass("B")));
        assertFalse(reasoner.getEquivalentClasses(getOWLClass("A")).contains(getOWLClass("D")));
        assertFalse(reasoner.getEquivalentClasses(getOWLClass("D")).contains(getOWLClass("A")));
    }

    public void testGetObjectPropertyValues() throws Exception {
        NodeSet<OWLNamedIndividual> values = reasoner.getObjectPropertyValues(getOWLIndividual("i"), getOWLObjectProperty("Q"));
        //ordered by name
        assertTrue(values.getFlattened().size() == 2);
        assertTrue(values.getNodes().size() == 2);
    }

    public void testIsPrecomputed() throws Exception {
        reasoner.isPrecomputed(InferenceType.CLASS_HIERARCHY);
        reasoner.isPrecomputed(InferenceType.OBJECT_PROPERTY_ASSERTIONS);
    }

    public void testGetInstancesEmpty() throws Exception {
        reasoner.getInstances(getOWLClass("A"), false);
    }

    public void testGetBottomProperty() throws Exception {
        reasoner.getEquivalentObjectProperties(getDataFactory().getOWLBottomObjectProperty());
    }

    public void testGetVersion() throws Exception {
        Version v = reasoner.getReasonerVersion();
    }

    public void testGetInstances() throws Exception {
        NodeSet<OWLNamedIndividual> indis = reasoner.getInstances(getOWLClass("D"), false);
        assertTrue(indis.getNodes().size() == 2);
    }

    public void testInconsistentAbox() throws Exception {
        Set<OWLAxiom> axioms = new HashSet<OWLAxiom>();
        axioms.add(getDataFactory().getOWLClassAssertionAxiom(getDataFactory().getOWLNothing(), getOWLIndividual("i")));
        manager.addAxioms(getRootOntology(), axioms);
        try {
            reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS);
            reasoner.getTypes(getOWLIndividual("i"), false);
            assertTrue(false);
        } catch (Exception e) {
            assertTrue(true);
        }

        manager.removeAxioms(getRootOntology(), axioms);
    }


}
