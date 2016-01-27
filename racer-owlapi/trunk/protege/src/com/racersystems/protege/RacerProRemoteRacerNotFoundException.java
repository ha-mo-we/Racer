package com.racersystems.protege;

import org.semanticweb.owlapi.reasoner.OWLReasonerRuntimeException;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 20.12.2011
 */
public class RacerProRemoteRacerNotFoundException extends OWLReasonerRuntimeException {
    private String host;
    private int port;

    public RacerProRemoteRacerNotFoundException(String host, int port) {
        this.host = host;
        this.port = port;
    }

    public String getHostAddress() {
        return this.host;
    }

    public int getPortNumber() {
        return this.port;
    }
}
