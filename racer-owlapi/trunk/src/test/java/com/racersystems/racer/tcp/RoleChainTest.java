package com.racersystems.racer.tcp;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.util.CollectionFactory;

import java.util.ArrayList;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 24.10.2012
 */
public class RoleChainTest extends AbstractReasonerOntologyCase    {

    @Override
   protected OWLOntology createRootOntology() throws OWLOntologyCreationException {
       OWLOntology ont = manager.createOntology();
        OWLObjectProperty r = getOWLObjectProperty("r");
        OWLObjectProperty s = getOWLObjectProperty("s");
        OWLObjectProperty t = getOWLObjectProperty("t");
        OWLObjectProperty u = getOWLObjectProperty("u");

        ArrayList<OWLObjectProperty> props = new ArrayList<OWLObjectProperty>();
        props.add(r);
        props.add(s);
        props.add(t);

        manager.addAxiom(ont, getDataFactory().getOWLSubPropertyChainOfAxiom(props, u));

       return ont;
   }

    public void testSuper() throws Exception {
        NodeSet<OWLObjectPropertyExpression> props = reasoner.getSubObjectProperties(getOWLObjectProperty("u"), false);    
    }

}
