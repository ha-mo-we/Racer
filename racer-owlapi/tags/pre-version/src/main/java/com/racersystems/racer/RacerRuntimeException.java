package com.racersystems.racer;

import org.semanticweb.owlapi.model.OWLRuntimeException;
/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * @author Olaf Noppens
 */
public class RacerRuntimeException extends OWLRuntimeException {
    public RacerRuntimeException() {
    }

    public RacerRuntimeException(String message) {
        super(message);
    }

    public RacerRuntimeException(String message, Throwable cause) {
        super(message, cause);
    }

    public RacerRuntimeException(Throwable cause) {
        super(cause);
    }
}
