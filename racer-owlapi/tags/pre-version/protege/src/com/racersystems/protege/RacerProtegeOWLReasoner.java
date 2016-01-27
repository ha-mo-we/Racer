package com.racersystems.protege;

import com.racersystems.racer.RacerReasonerConfiguration;
import com.racersystems.racer.RacerReasonerImpl;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.BufferingMode;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 14.10.2010
 */
public class RacerProtegeOWLReasoner extends RacerReasonerImpl {
    private Process process;

    public RacerProtegeOWLReasoner(Process process, OWLOntology rootOntology, RacerReasonerConfiguration configuration, BufferingMode bufferingMode) {
        super(rootOntology, configuration, bufferingMode);
        this.process = process;
    }

    @Override
    public void dispose() {
       // System.out.println("dispose Reasoner");
        super.dispose();
        if (this.process != null) {
            RacerProtegeOWLReasonerFactory.destroyRunningRacer(this.process);
            this.process = null;
        }
    }
}
