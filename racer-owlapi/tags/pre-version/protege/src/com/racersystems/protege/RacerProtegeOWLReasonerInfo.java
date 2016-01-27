package com.racersystems.protege;

import com.racersystems.protege.preferences.RacerPreferences;
import com.racersystems.racer.RacerReasonerConfiguration;
import org.protege.editor.owl.model.inference.AbstractProtegeOWLReasonerInfo;
import org.semanticweb.owlapi.reasoner.BufferingMode;
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor;

import java.net.InetSocketAddress;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 14.10.2010
 */
public class RacerProtegeOWLReasonerInfo extends AbstractProtegeOWLReasonerInfo {
    private final RacerProtegeOWLReasonerFactory factory;

    public RacerProtegeOWLReasonerInfo() {
        this.factory = new RacerProtegeOWLReasonerFactory();
    }

    public BufferingMode getRecommendedBuffering() {
        return BufferingMode.BUFFERING;
    }

    public OWLReasonerFactory getReasonerFactory() {
        return factory;
    }

    @Override
    public OWLReasonerConfiguration getConfiguration(ReasonerProgressMonitor monitor) {
        RacerPreferences prefs = new RacerPreferences();
        if (prefs.isStartRacerEnabled()) {
            InetSocketAddress address = new InetSocketAddress("127.0.0.1", 8088);
            return new RacerReasonerConfiguration(address, monitor);
        } else {
            InetSocketAddress address = new InetSocketAddress("127.0.0.1", 8088);
            return new RacerReasonerConfiguration(address, monitor);
        }
    }

}