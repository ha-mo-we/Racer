package com.racersystems.protege;

import com.racersystems.protege.preferences.RacerProPreferences;
import com.racersystems.racer.Configuration;
import com.racersystems.racer.Translator;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.*;

import java.util.List;
import java.util.Vector;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 14.10.2010
 */
public class Reasoner extends com.racersystems.racer.Reasoner {
    private Process process;

    public Reasoner(Process process, OWLOntology rootOntology, Configuration configuration, BufferingMode bufferingMode) {
        super(rootOntology, configuration, bufferingMode);
        this.process = process;
    }

    @Override
    protected Translator createRacerTranslator() {
        return new Translator(getOWLOntologyManager(), getReasonerConfiguration(),
                serverAdapter.getOutputWriter(), serverAdapter.getInputReader()) {
            @Override
            public void disposeKnowledgeBase() {
                if (new RacerProPreferences().isDisposeReasonerOnClose())
                    super.disposeKnowledgeBase();
            }
        };
    }

    protected void disposeSilently() {
        if (this.process != null) {
            super.racerTranslator.shutdown();
            super.disposeSilently();
        }
    }

    protected void disposeInternaly() {
        super.racerTranslator.setSimplifiedRacerDispose(true);
        super.dispose();
        this.process = null;
    }

    @Override
    public void dispose() {
        com.racersystems.protege.ReasonerFactory.disposeRunningRacerReasoner(this.process, this);
    }

    @Override
    public void precomputeInferences(InferenceType... inferenceTypes) throws ReasonerInterruptedException, TimeOutException, InconsistentOntologyException {
        if (new RacerProPreferences().isDisableABoxRealization()) {
            List<InferenceType> types = new Vector<InferenceType>();
            for (InferenceType type : inferenceTypes) {
                switch (type) {
                    case CLASS_HIERARCHY:
                        types.add(type);
                        break;
                    case DATA_PROPERTY_HIERARCHY:
                        types.add(type);
                        break;
                    case OBJECT_PROPERTY_HIERARCHY:
                        types.add(type);
                        break;
                    case CLASS_ASSERTIONS:
                        break;
                    case DATA_PROPERTY_ASSERTIONS:
                        break;
                    case DIFFERENT_INDIVIDUALS:
                        break;
                    case DISJOINT_CLASSES:
                        types.add(type);
                        break;
                    case OBJECT_PROPERTY_ASSERTIONS:
                        break;
                    case SAME_INDIVIDUAL:
                        break;
                }
            }
            super.precomputeInferences(types.toArray(new InferenceType[types.size()]));
        } else
            super.precomputeInferences(inferenceTypes);

    }
}
