package com.racersystems.racer.util;

import org.semanticweb.owlapi.model.IRI;

import static com.racersystems.racer.RacerKRSSVocabulary.DELIMITER_STRING;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * @author Olaf Noppens 
 * @version 1.0
 */
public class NonShortFormProvider implements ShortFormProvider {

    public String shortForm(IRI uri) {
         return DELIMITER_STRING + uri.toString() + DELIMITER_STRING;
    }

    public IRI longForm(String string) {
        return IRI.create(string);
    }

    public void dispose() {
    }
}
