package com.racersystems.racer;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.*;
import org.semanticweb.owlapi.util.Version;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/*******************************************************************************
 * Copyright (c) 2013 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 18.04.2013
 */
public class ReasonerCache  {

    Translator translator;
    LinkedHashMap<OWLClassExpression, Node<OWLClass>> equivalentClasses;
    LinkedHashMap<OWLClassExpression, NodeSet<OWLClass>> disjointClasses;
    LinkedHashMap<OWLClassExpression, NodeSet<OWLClass>> subClasses;
    LinkedHashMap<OWLClassExpression, NodeSet<OWLClass>> subClassesDirect;
    LinkedHashMap<OWLClassExpression, NodeSet<OWLClass>> superClassesDirect;
    LinkedHashMap<OWLClassExpression, NodeSet<OWLClass>> superClasses;
    LinkedHashMap<OWLClassExpression, Boolean> isSatisfiable;


    private class CacheMap<V,K> extends LinkedHashMap<V,K> {
        final int MAXENTRIES;

        public CacheMap(int maxEntries) {
            this.MAXENTRIES = maxEntries;
        }


        @Override
        protected boolean removeEldestEntry(Map.Entry<V, K> eldest) {
            return size() > MAXENTRIES;
        }
    }



    private boolean isConsistent;
    private boolean isConsistentInvalid=false;

    private boolean invalidated = true;


    public ReasonerCache(Translator reasoner) {
        this.translator = reasoner;
        this.equivalentClasses = new CacheMap<OWLClassExpression, Node<OWLClass>>(1000);
        this.disjointClasses = new CacheMap<OWLClassExpression, NodeSet<OWLClass>>(1000);
        this.subClasses = new CacheMap<OWLClassExpression, NodeSet<OWLClass>>(1000);
        this.subClassesDirect = new CacheMap<OWLClassExpression, NodeSet<OWLClass>>(1000);
        this.superClassesDirect = new CacheMap<OWLClassExpression, NodeSet<OWLClass>>(1000);
        this.superClasses = new CacheMap<OWLClassExpression, NodeSet<OWLClass>>(1000);
        isSatisfiable = new CacheMap<OWLClassExpression, Boolean>(1000);
        isConsistentInvalid = true;
    }


    public void invalidate() {
        this.invalidated = true;
        this.equivalentClasses.clear();
        this.disjointClasses.clear();
        this.subClasses.clear();
        this.superClasses.clear();
        this.superClassesDirect.clear();
        this.subClassesDirect.clear();
        isSatisfiable.clear();
        isConsistentInvalid = true;
    }


    public boolean isConsistent() throws ReasonerInterruptedException, TimeOutException {
        if (!isConsistentInvalid) {
            return isConsistent;
        }
        isConsistent = translator.isConsistent();
        isConsistentInvalid = false;
        return isConsistent;
    }

    public boolean isSatisfiable(OWLClassExpression classExpression) throws ReasonerInterruptedException, TimeOutException, ClassExpressionNotInProfileException, FreshEntitiesException, InconsistentOntologyException {
        Boolean value = isSatisfiable.get(classExpression);
        if (value == null) {
            value = translator.isConsistent(classExpression);
            isSatisfiable.put(classExpression, value);
        }
        return value;
    }


    public NodeSet<OWLClass> getSubClasses(OWLClassExpression ce, boolean direct) throws ReasonerInterruptedException, TimeOutException, FreshEntitiesException, InconsistentOntologyException, ClassExpressionNotInProfileException {
        if (direct) {
            NodeSet<OWLClass> classes = this.subClassesDirect.get(ce);
            if (classes == null) {
                classes = translator.getSubClasses(ce, true);
                this.subClassesDirect.put(ce, classes);
            }
            return classes;
        } else {
            NodeSet<OWLClass> classes = this.subClasses.get(ce);
            if (classes == null) {
                classes = translator.getSubClasses(ce, false);
                this.subClasses.put(ce, classes);
            }
            return classes;
        }
    }

    public NodeSet<OWLClass> getSuperClasses(OWLClassExpression ce, boolean direct) throws InconsistentOntologyException, ClassExpressionNotInProfileException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
         if (direct) {
            NodeSet<OWLClass> classes = this.superClassesDirect.get(ce);
            if (classes == null) {
                classes = translator.getSuperClasses(ce, true);
                this.superClassesDirect.put(ce, classes);
            }
            return classes;
        } else {
            NodeSet<OWLClass> classes = this.superClasses.get(ce);
            if (classes == null) {
                classes = translator.getSuperClasses(ce, false);
                this.superClasses.put(ce, classes);
            }
            return classes;
        }
    }

    public Node<OWLClass> getEquivalentClasses(OWLClassExpression ce) throws InconsistentOntologyException, ClassExpressionNotInProfileException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        Node<OWLClass> classes = this.equivalentClasses.get(ce);
        if (classes == null) {
            classes = translator.getEquivalentClasses(ce);
            this.equivalentClasses.put(ce, classes);
        }
        return classes;
    }

    public NodeSet<OWLClass> getDisjointClasses(OWLClassExpression ce) throws ReasonerInterruptedException, TimeOutException, FreshEntitiesException, InconsistentOntologyException {
        NodeSet<OWLClass> classes = this.disjointClasses.get(ce);
        if (classes == null) {
            classes = translator.getDisjointClasses(ce);
            this.disjointClasses.put(ce, classes);
        }
        return classes;
    }


    public void dispose() {
        invalidate();
    }
}
