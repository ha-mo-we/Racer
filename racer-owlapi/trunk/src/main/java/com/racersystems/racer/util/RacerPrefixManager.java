package com.racersystems.racer.util;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.util.DefaultPrefixManager;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 07.10.2011
 */
public class RacerPrefixManager extends DefaultPrefixManager {
    final String defaultPrefixName = "p";
    private String prefixName = defaultPrefixName;
    int counter = -1;


    public String getShortForm(IRI iri) {
        String sf = getPrefixIRI(iri);
        String iriString = iri.toString();
        if (sf == null) {
            String prefixName = generatePrefix(iri);
            StringBuilder sb = new StringBuilder();
            sb.append(prefixName);
            String localName = iriString.substring(getPrefix(prefixName).length());
            sb.append(localName);
            return "#!"+sb.toString();
        }
        return "#!"+sf;
    }


    public String generatePrefix(IRI iri) {
        String iriString = iri.toString();
        int index = iriString.indexOf("#");
        if (index > -1) {
            String prefix = iriString.substring(0, index+1);
            String prefixName = nextPrefixName();
            setPrefix(prefixName, prefix);
            return prefixName;
        }
        return null;
    }

    private String nextPrefixName() {
        return prefixName + (counter++) + ":";
    }

    public void setPrefix(String prefixName, String prefix) {
        super.setPrefix(prefixName, prefix);
        this.newprefixName = prefixName;
    }

    String newprefixName = null;

    public String getNewPrefixName()  {
        return this.newprefixName;
    }

    public void reset() {
        this.newprefixName = null;
    }

}
