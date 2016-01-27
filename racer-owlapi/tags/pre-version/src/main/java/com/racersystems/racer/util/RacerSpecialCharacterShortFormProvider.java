package com.racersystems.racer.util;

import com.racersystems.racer.RacerKRSSVocabulary;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLEntity;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 14.10.2010
 */
public class RacerSpecialCharacterShortFormProvider implements ShortFormProvider{
   private final OWLDataFactory factory;
    private final IRI top;
    private final IRI bottom;
    private final IRI bottomObjectProperty;
    private final IRI bottomDataProperty;
    private final IRI topObjectProperty;
    private final IRI topDataProperty;

    public RacerSpecialCharacterShortFormProvider(OWLDataFactory factory) {
        this.factory = factory;
        this.top = factory.getOWLThing().getIRI();
        this.bottom = factory.getOWLNothing().getIRI();
        this.bottomObjectProperty = factory.getOWLBottomObjectProperty().getIRI();
        this.bottomDataProperty = factory.getOWLBottomDataProperty().getIRI();
        this.topObjectProperty = factory.getOWLTopObjectProperty().getIRI();
        this.topDataProperty = factory.getOWLTopDataProperty().getIRI();
    }

    public final String getShortForm(OWLEntity entity) {
        return shortForm(entity.getIRI());
    }

    protected String makePretty(String s) {
        String res = s.replaceAll("\n", "\\\\N");
        res = res.replace("(", "\\(");
        res = res.replace(")", "\\)");
        res = res.replace("|", "\\|");
        return res.replace("\"", "\\S");
    }

    public final String shortForm(IRI IRI) {
        if (IRI == top)
            return RacerKRSSVocabulary.TOP;
        else if (IRI == bottom)
            return RacerKRSSVocabulary.BOTTOM;
        else if (IRI == bottomDataProperty)
            return RacerKRSSVocabulary.BOTTOM_DATA_ROLE;
        else if (IRI == bottomObjectProperty)
            return RacerKRSSVocabulary.BOTTOM_OBJECT_ROLE;
        else if (IRI == topObjectProperty)
            return RacerKRSSVocabulary.TOP_OBJECT_ROLE;
        else if (IRI == topDataProperty)
            return RacerKRSSVocabulary.TOP_DATA_ROLE;
        else {
            return RacerKRSSVocabulary.DELIMITER_LINE + makePretty(IRI.toString()) + RacerKRSSVocabulary.DELIMITER_LINE;
        }
    }

    public final IRI longForm(String string) {
        if (string == (RacerKRSSVocabulary.TOP))
            return factory.getOWLThing().getIRI();
        else if (string == (RacerKRSSVocabulary.BOTTOM))
            return factory.getOWLNothing().getIRI();
        else if (string == RacerKRSSVocabulary.BOTTOM_DATA_ROLE)
            return bottomDataProperty;
        else if (string == RacerKRSSVocabulary.BOTTOM_OBJECT_ROLE)
            return bottomObjectProperty;
        else if (string == RacerKRSSVocabulary.TOP_DATA_ROLE)
            return topDataProperty;
        else if (string == RacerKRSSVocabulary.TOP_OBJECT_ROLE)
            return topObjectProperty;
        string = string.replaceAll(RacerKRSSVocabulary.DELIMITER_STRING, "");
        return IRI.create(string);
    }
}