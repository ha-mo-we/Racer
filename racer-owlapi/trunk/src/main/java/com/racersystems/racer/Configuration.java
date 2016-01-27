package com.racersystems.racer;

import org.semanticweb.owlapi.reasoner.*;

import java.io.Serializable;
import java.net.InetSocketAddress;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 22.10.2012
 */
public class Configuration implements Cloneable, Serializable, OWLReasonerConfiguration {
    private ReasonerProgressMonitor progressMonitor;
    private FreshEntityPolicy freshEntityPolicy;
    private IndividualNodeSetPolicy individualNodeSetPolicy;
    private long timeOut;

    private InetSocketAddress address;
    private boolean isAnnotationConsidered;
    private AxiomLoadingStrategy axiomLoadingStrategy;
    private boolean disableABoxConsistencyTest;
    private boolean isMemorySavingMode;


    public Configuration() {
        this(new InetSocketAddress("127.0.0.1", 8088));
    }

    public Configuration(InetSocketAddress address,ReasonerProgressMonitor monitor) {
        this(address);
        this.progressMonitor = monitor;
    }

    public Configuration(InetSocketAddress address) {
        this.address = address;
        this.progressMonitor = new NullReasonerProgressMonitor();
        this.freshEntityPolicy = FreshEntityPolicy.ALLOW;
        this.individualNodeSetPolicy = IndividualNodeSetPolicy.BY_NAME;
        timeOut = Long.MAX_VALUE;
        axiomLoadingStrategy = AxiomLoadingStrategy.LOAD_ON_CREATION;
        this.disableABoxConsistencyTest = false;
        this.isMemorySavingMode = false;
    }

    public Configuration(OWLReasonerConfiguration configuration) {
        this();
        progressMonitor = configuration.getProgressMonitor();
        freshEntityPolicy = configuration.getFreshEntityPolicy();
        individualNodeSetPolicy = configuration.getIndividualNodeSetPolicy();
        timeOut = configuration.getTimeOut();
        if (configuration instanceof Configuration) {
            Configuration conf = (Configuration) configuration;
            address = conf.address;
            isAnnotationConsidered = conf.isAnnotationConsidered;
            axiomLoadingStrategy = conf.axiomLoadingStrategy;
            disableABoxConsistencyTest = conf.disableABoxConsistencyTest;
            isMemorySavingMode = conf.isMemorySavingMode;
        }
    }

    @Override
    public Configuration clone() {
        try {
            Configuration configuration = (Configuration) super.clone();
            return configuration;
        } catch (CloneNotSupportedException e) {
            return new Configuration();
        }
    }

    public static Configuration clone(OWLReasonerConfiguration configuration) {
        Configuration config = new Configuration();
        config.progressMonitor = configuration.getProgressMonitor();
        config.freshEntityPolicy = configuration.getFreshEntityPolicy();
        config.individualNodeSetPolicy = configuration.getIndividualNodeSetPolicy();
        config.timeOut = configuration.getTimeOut();
        if (configuration instanceof Configuration) {
            Configuration conf = (Configuration) configuration;
            config.address = conf.address;
            config.isAnnotationConsidered = conf.isAnnotationConsidered;
            config.axiomLoadingStrategy = conf.axiomLoadingStrategy;
            config.disableABoxConsistencyTest = conf.disableABoxConsistencyTest;
            config.isMemorySavingMode = conf.isMemorySavingMode;
        }
        return config;
    }

    public void setAxiomLoadingStrategy(AxiomLoadingStrategy strategy) {
        this.axiomLoadingStrategy = strategy;
    }

    public AxiomLoadingStrategy getAxiomLoadingStrategy() {
        return this.axiomLoadingStrategy;
    }

    public void setMemorySavingModeEnabled(boolean enable) {
        this.isMemorySavingMode = enable;
    }

    public boolean isMemorySavingModeEnabled() {
        return this.isMemorySavingMode;
    }

    public void setAboxConsistencyTestEnabled(boolean enable) {
        this.disableABoxConsistencyTest = !enable;
    }

    public boolean isAboxConsistencyTestEnabled() {
        return !this.disableABoxConsistencyTest;
    }

    public InetSocketAddress getSocketAddress() {
        return this.address;
    }

    public ReasonerProgressMonitor getProgressMonitor() {
        return this.progressMonitor;
    }

    public void setReasonerProgressMonitor(ReasonerProgressMonitor monitor) {
        this.progressMonitor = monitor;
    }

    public long getTimeOut() {
        return this.timeOut;
    }

    public FreshEntityPolicy getFreshEntityPolicy() {
        return this.freshEntityPolicy;
    }

    public IndividualNodeSetPolicy getIndividualNodeSetPolicy() {
        return this.individualNodeSetPolicy;
    }
}
