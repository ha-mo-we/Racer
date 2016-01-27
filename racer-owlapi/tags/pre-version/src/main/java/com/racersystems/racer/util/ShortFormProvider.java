package com.racersystems.racer.util;

import org.semanticweb.owlapi.model.IRI;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * @author Olaf Noppens 
 * @version 1.0
 */
public interface ShortFormProvider {
    public String shortForm(IRI uri);

    public IRI longForm(String string);
}
