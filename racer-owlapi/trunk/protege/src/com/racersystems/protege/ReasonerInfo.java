package com.racersystems.protege;

import com.racersystems.protege.preferences.RacerProPreferences;
import com.racersystems.racer.AxiomLoadingStrategy;
import com.racersystems.racer.Configuration;
import org.protege.editor.owl.model.inference.AbstractProtegeOWLReasonerInfo;
import org.semanticweb.owlapi.reasoner.BufferingMode;
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor;

import java.net.InetSocketAddress;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 14.10.2010
 */
public class ReasonerInfo extends AbstractProtegeOWLReasonerInfo {

    public ReasonerInfo() {
    }

    public BufferingMode getRecommendedBuffering() {
        return BufferingMode.BUFFERING;
    }

    public OWLReasonerFactory getReasonerFactory() {
        return new ReasonerFactory(getOWLModelManager().getOWLReasonerManager());
    }


    @Override
    public OWLReasonerConfiguration getConfiguration(ReasonerProgressMonitor monitor) {
        RacerProPreferences prefs = new RacerProPreferences();
        if (prefs.isStartRacerEnabled()) {
            InetSocketAddress address = new InetSocketAddress("127.0.0.1", prefs.getLocalRacerPort());
            Configuration config = new Configuration(address, monitor);
            config.setAxiomLoadingStrategy(AxiomLoadingStrategy.LOAD_ON_PRECOMPUTE);
            config.setAboxConsistencyTestEnabled(!prefs.isDisableABoxConsistencyTest());
            config.setMemorySavingModeEnabled(prefs.isMemorySavingModeEnabled());
            return config;
        } else {
            InetSocketAddress address = new InetSocketAddress(prefs.getRemoteRacerAddress(), prefs.getRemoteRacerPort());
            Configuration config = new Configuration(address, monitor);
            config.setAxiomLoadingStrategy(AxiomLoadingStrategy.LOAD_ON_PRECOMPUTE);
            config.setAboxConsistencyTestEnabled(!prefs.isDisableABoxConsistencyTest());
            config.setMemorySavingModeEnabled(prefs.isMemorySavingModeEnabled());
            return config;
        }
    }

}