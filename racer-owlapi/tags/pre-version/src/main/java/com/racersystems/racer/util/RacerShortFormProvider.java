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
 * @author Olaf Noppens
 * @version 1.0
 */
public class RacerShortFormProvider implements ShortFormProvider {
    private final OWLDataFactory factory;
    private final IRI top;
    private final IRI bottom;
    private final IRI bottomObjectProperty;
    private final IRI bottomDataProperty;
    private final IRI topObjectProperty;
    private final IRI topDataProperty;

    public RacerShortFormProvider(OWLDataFactory factory) {
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

    public final String shortForm(IRI IRI) {
        if (IRI.equals(top))
            return RacerKRSSVocabulary.TOP;
        else if (IRI.equals(bottom))
            return RacerKRSSVocabulary.BOTTOM;
        else if (IRI.equals(bottomDataProperty))
            return RacerKRSSVocabulary.BOTTOM_DATA_ROLE;
        else if (IRI.equals(bottomObjectProperty))
            return RacerKRSSVocabulary.BOTTOM_OBJECT_ROLE;
        else if (IRI.equals(topObjectProperty))
            return RacerKRSSVocabulary.TOP_OBJECT_ROLE;
        else if (IRI.equals(topDataProperty))
            return RacerKRSSVocabulary.TOP_DATA_ROLE;
        else {
            return RacerKRSSVocabulary.DELIMITER_LINE + IRI.toString() + RacerKRSSVocabulary.DELIMITER_LINE;
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
