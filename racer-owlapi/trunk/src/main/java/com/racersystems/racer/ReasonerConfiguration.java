package com.racersystems.racer;

import org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor;
import org.semanticweb.owlapi.reasoner.SimpleConfiguration;

import java.net.InetSocketAddress;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * This configuration is used for configuring a RacerReasoner instance.
 * The OWLAPI racer reasoner instance is connecting to an already running (external)
 * RacerPro reasoner instance. Please see the different constructors.
 * The AxiomLoadingStrategy specifies when axioms
 * should be loaded into the reasoner, see {@link com.racersystems.racer.AxiomLoadingStrategy}
 * for further information.
 * <p/>
 * Author: Olaf Noppens
 * Date: 19.05.2010
 */
public class ReasonerConfiguration extends SimpleConfiguration {
    private InetSocketAddress address;
    private boolean isAnnotationConsidered;
    private AxiomLoadingStrategy axiomLoadingStrategy = AxiomLoadingStrategy.LOAD_ON_CREATION;
    private boolean disableABoxConsistencyTest = false;
    private boolean isMemorySavingMode = false;

    /**
     * Creates a RacerReasonerConfiguration with the specified monitor and the default
     * tcp address "localhost" and port number "8088". The AxiomLoadingStrategy is
     * LOAD_ON_CREATION.
     *
     * @param progressMonitor
     */
    public ReasonerConfiguration(ReasonerProgressMonitor progressMonitor) {
        this(new InetSocketAddress("127.0.0.1", 8088), progressMonitor);
    }

    /**
     * Creates a RacerReasonerConfiguration with the default TCP address "localhost"
     * and the port number "8088". The AxiomLoadingStrategy is
     * LOAD_ON_CREATION.
     */
    public ReasonerConfiguration() {
        this(new InetSocketAddress("127.0.0.1", 8088));
    }

    public ReasonerConfiguration(boolean memorySavingMode) {
        this(new InetSocketAddress("127.0.0.1", 8088));
        this.isMemorySavingMode = memorySavingMode;
    }

    public ReasonerConfiguration(SimpleConfiguration configuration) {
        this(new InetSocketAddress("127,0,0,1", 8088), configuration.getProgressMonitor(),
                AxiomLoadingStrategy.LOAD_ON_CREATION);
        if (configuration instanceof ReasonerConfiguration) {
            ReasonerConfiguration other = (ReasonerConfiguration) configuration;
            this.disableABoxConsistencyTest = other.disableABoxConsistencyTest;
            this.isMemorySavingMode = other.isMemorySavingMode;
            this.axiomLoadingStrategy = other.axiomLoadingStrategy;
            this.isAnnotationConsidered = other.isAnnotationConsidered;
            this.address = other.address;
        }
    }

    /**
     * Creates a RacerReasonerConfiguration with the given address. The AxiomLoadingStrategy is
     * LOAD_ON_CREATION.
     *
     * @param address
     */
    public ReasonerConfiguration(InetSocketAddress address) {
        this.address = address;
    }

    public ReasonerConfiguration(InetSocketAddress address, boolean memorySavingMode) {
        this.address = address;
        this.isMemorySavingMode = memorySavingMode;
    }

    /**
     * Creates a RacerReasonerConfiguration with the given address and the given monitor.
     * The AxiomLoadingStrategy is LOAD_ON_CREATION.
     *
     * @param address
     * @param progressMonitor
     */
    public ReasonerConfiguration(InetSocketAddress address, ReasonerProgressMonitor progressMonitor) {
        this(address, progressMonitor, AxiomLoadingStrategy.LOAD_ON_CREATION);
    }

    /**
     * Creates a RacerReasonerConfiguration with the given address, the given monitor, and the given
     * AxiomLoadingStrategy.
     *
     * @param serverAddress
     * @param progressMonitor
     * @param axiomLoadingStrategy
     */
    public ReasonerConfiguration(InetSocketAddress serverAddress, ReasonerProgressMonitor progressMonitor,
                                 AxiomLoadingStrategy axiomLoadingStrategy) {
        this(serverAddress, progressMonitor, axiomLoadingStrategy, false, false);
    }

    public ReasonerConfiguration(InetSocketAddress serverAddress, ReasonerProgressMonitor progressMonitor,
                                 AxiomLoadingStrategy axiomLoadingStrategy,
                                 boolean disableABoxConsistencyTest, boolean isMemorySaving) {
        super(progressMonitor);
        this.address = serverAddress;
        this.axiomLoadingStrategy = axiomLoadingStrategy;
        this.disableABoxConsistencyTest = disableABoxConsistencyTest;
        this.isMemorySavingMode = true;

    }

    /**
     * Returns the socketaddress (IP address and port number) where the external RacerPro reasoner instance
     * is running.
     *
     * @return
     */
    public InetSocketAddress getSocketAddress() {
        return this.address;
    }

    public boolean getAnnotationConsidered() {
        return this.isAnnotationConsidered;
    }

    /**
     * Returns the AxiomLoadingStrategy.
     * <p>
     * <blockquote>Please refer to {@link AxiomLoadingStrategy} for a discussion about
     * the effect of the different strategies.</blockquote>
     * </p>
     *
     * @return AxiomLoadingStrategy
     */
    public AxiomLoadingStrategy getAxiomLoadingStrategy() {
        return this.axiomLoadingStrategy;
    }

    public boolean isABoxConsistencyTestDisabled() {
        return this.disableABoxConsistencyTest;
    }

    public boolean isMemorySavingModeEnabled() {
        return this.isMemorySavingMode;
    }

}
