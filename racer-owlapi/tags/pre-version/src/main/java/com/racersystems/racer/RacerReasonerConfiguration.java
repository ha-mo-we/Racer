package com.racersystems.racer;

import org.semanticweb.owlapi.reasoner.IndividualNodeSetPolicy;
import org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor;
import org.semanticweb.owlapi.reasoner.SimpleConfiguration;

import java.net.InetSocketAddress;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 19.05.2010
 */
public class RacerReasonerConfiguration extends SimpleConfiguration {
    private InetSocketAddress address;
    private boolean isAnnotationConsidered;

    public RacerReasonerConfiguration(ReasonerProgressMonitor progressMonitor) {
        this(new InetSocketAddress("127.0.0.1", 8088), progressMonitor);
    }

    public RacerReasonerConfiguration() {
        this(new InetSocketAddress("127.0.0.1", 8088));
    }

    public RacerReasonerConfiguration(InetSocketAddress address) {
        this.address = address;
    }

    public RacerReasonerConfiguration(InetSocketAddress address, ReasonerProgressMonitor progressMonitor) {
        super(progressMonitor);
        this.address = address;
    }

    public InetSocketAddress getSocketAddress() {
        return this.address;
    }

    public boolean getAnnotationConsidered() {
        return this.isAnnotationConsidered;
    }
}
