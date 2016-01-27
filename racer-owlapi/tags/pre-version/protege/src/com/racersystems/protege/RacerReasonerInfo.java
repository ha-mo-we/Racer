package com.racersystems.protege;

import com.racersystems.racer.RacerReasonerConfiguration;
import com.racersystems.racer.RacerReasonerFactory;
import org.protege.editor.owl.model.inference.AbstractProtegeOWLReasonerInfo;
import org.semanticweb.owlapi.reasoner.BufferingMode;
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 08.10.2010
 */
public class RacerReasonerInfo extends AbstractProtegeOWLReasonerInfo {
    private final RacerReasonerFactory factory;

    public RacerReasonerInfo() {
        this.factory = new RacerReasonerFactory();
    }

    public BufferingMode getRecommendedBuffering() {
        return BufferingMode.BUFFERING;
    }

    public OWLReasonerFactory getReasonerFactory() {
        return factory;
    }

    @Override
    public OWLReasonerConfiguration getConfiguration(ReasonerProgressMonitor monitor) {
        RacerReasonerConfiguration configuration = new RacerReasonerConfiguration(monitor);
        return configuration;
    }


}
