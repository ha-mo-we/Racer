package com.racersystems.racer;

import org.semanticweb.owlapi.util.Version;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 21.10.2009
 */
public class NotSupportedRacerException extends RacerRuntimeException{
    private final Version version;

    public NotSupportedRacerException(Version version) {
        super("Racer binaries prior to version 2.0 are not supported (found Version " + version +")");
        this.version = version;
    }

    public Version getAvailableVersion() {
        return this.version;
    }
}
