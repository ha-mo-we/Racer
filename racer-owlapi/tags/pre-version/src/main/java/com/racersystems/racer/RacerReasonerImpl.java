package com.racersystems.racer;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.*;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNodeSet;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet;
import org.semanticweb.owlapi.reasoner.impl.OWLReasonerBase;
import org.semanticweb.owlapi.util.CollectionFactory;
import org.semanticweb.owlapi.util.Version;

import java.io.*;
import java.net.InetSocketAddress;
import java.util.*;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * TCP-based OWLAPI Reasoner adapter for the RACER reasoner system 1.9.2 or higher.
 * This implementation uses the new RACER OWLAPI interface which is only available
 * since 1.9.2 or higher.
 *
 * @author Olaf Noppens
 */
public class RacerReasonerImpl extends OWLReasonerBase implements RacerReasoner {
    protected final OWLOntologyManager manager;
    protected final OWLDataFactory factory;
    protected final RacerSocketAdapter serverAdapter;
    protected final RacerTranslator racerTranslator;
    private boolean isUnloadOntologyBeforeReloading = false;
    boolean interrupted = false;


    class ProgressMonitorTask extends TimerTask {
        RacerReasonerConfiguration configuration;
        PrintWriter out;
        BufferedReader reader;
        String reasonerID;
        boolean isCanceled;

        public ProgressMonitorTask(RacerReasonerConfiguration configuration, String reasonerID) throws IOException {
            this.configuration = configuration;
            this.reasonerID = reasonerID;
            RacerSocketAdapter socket = new RacerSocketAdapter(configuration.getSocketAddress());
            out = (PrintWriter) socket.getOutputWriter();
            reader = new BufferedReader(socket.getInputReader());
            //first we try to connecto to the server
            out.println(RacerOWLAPIVocabulary.OPEN_BRACKET + RacerOWLAPIVocabulary.OWLAPI_PROGRESS_CERTAIN + " " + reasonerID + RacerOWLAPIVocabulary.CLOSE_BRACKET);
            out.flush();
            reader.readLine();
        }

        public void run() {
            try {
                //if it is certain?
                out.println(RacerOWLAPIVocabulary.OPEN_BRACKET + RacerOWLAPIVocabulary.OWLAPI_PROGRESS_CERTAIN + " " + reasonerID + RacerOWLAPIVocabulary.CLOSE_BRACKET);
                out.flush();
                if (reader.readLine().equals("t")) {
                    out.print(RacerOWLAPIVocabulary.OPEN_BRACKET);
                    out.print(RacerOWLAPIVocabulary.GETPROGRESSINDICATOR);
                    out.println(RacerOWLAPIVocabulary.CLOSE_BRACKET);
                    out.flush();
                    String value = reader.readLine();
                    configuration.getProgressMonitor().reasonerTaskProgressChanged(Integer.parseInt(value), 100);
                } else {
                    configuration.getProgressMonitor().reasonerTaskBusy();
                }
            } catch (IOException io) {
                io.printStackTrace();
            }
        }

        public void start(String taskName) {
            configuration.getProgressMonitor().reasonerTaskStarted(taskName);
        }

        public boolean cancel() {
            isCanceled = true;
            out.print(RacerOWLAPIVocabulary.OPEN_BRACKET);
            out.print(RacerOWLAPIVocabulary.OWLAPI_ABORT);
            out.print(RacerOWLAPIVocabulary.SPACE);
            out.print(reasonerID);
            out.println(RacerOWLAPIVocabulary.CLOSE_BRACKET);
            try {
                reader.readLine();
            } catch (IOException e) {
            }
            return super.cancel();
        }

        public void stop(String taskName) {
            configuration.getProgressMonitor().reasonerTaskStopped();
        }


    }


   Timer timer = new Timer();

    private void startReasonerTask(String taskName) {
       timer.cancel();
        try {
           timer = new Timer();
            getReasonerConfiguration().getProgressMonitor().reasonerTaskStarted(taskName);
            ProgressMonitorTask task = new ProgressMonitorTask(getReasonerConfiguration(), racerTranslator.getKnowledgeBaseName());
           timer.schedule(task, 0, 1000);
        } catch (Exception io) {

        }
    }

    private void stopReasonerTask(String taskName) {
      timer.cancel();
        getReasonerConfiguration().getProgressMonitor().reasonerTaskStopped();
    }

    private void cancelReasonerTask(String taskName) {
      timer.cancel();
        try {
            ProgressMonitorTask task = new ProgressMonitorTask(getReasonerConfiguration(), racerTranslator.getKnowledgeBaseName());
            task.cancel();
        } catch (Exception io) {

        }
    }


    protected RacerReasonerImpl(OWLOntology rootOntology, RacerReasonerConfiguration configuration, BufferingMode bufferingMode) {
        super(rootOntology, configuration, bufferingMode);
        this.manager = rootOntology.getOWLOntologyManager();
        this.factory = manager.getOWLDataFactory();

        try {
            serverAdapter = new RacerSocketAdapter(configuration.getSocketAddress());
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }


        this.racerTranslator = new RacerTranslator(getOWLOntologyManager(), configuration,
                serverAdapter.getOutputWriter(), serverAdapter.getInputReader());
        startReasonerTask("Loading axioms");
        this.racerTranslator.handleChanges(getReasonerAxioms(), Collections.<OWLAxiom>emptySet());
        stopReasonerTask("Loading axioms");
    }

    public String getReasonerName() {
        return "Racer";
    }

    public Version getReasonerVersion() {
        return racerTranslator.getRacerVersionNumber();
    }

    @Override
    public RacerReasonerConfiguration getReasonerConfiguration() {
        return (RacerReasonerConfiguration) super.getReasonerConfiguration();
    }

    public final void setUnloadOntologyBeforeReloading(final boolean enabled) {
        this.isUnloadOntologyBeforeReloading = enabled;
    }

    public final boolean isUnloadOntologyBeforeReloading() {
        return isUnloadOntologyBeforeReloading;
    }

    public final OWLOntologyManager getOWLOntologyManager() {
        return manager;
    }

    @Override
    protected void handleChanges(Set<OWLAxiom> addAxioms, Set<OWLAxiom> removeAxioms) {
        racerTranslator.handleChanges(addAxioms, removeAxioms);
    }


    /**
     * @inheritDoc
     */
    public void dispose() {
        //timer.cancel();
        super.dispose();
        racerTranslator.dispose();
        try {
            serverAdapter.closeConnection();
        } catch (Exception e) {
            logger.severe(e.toString());
            throw new OWLRuntimeException(e);
        }

    }

    @Override
    protected void finalize() throws Throwable {
        try {
            //
            // ();
        } catch (Exception e) {
        }
        super.finalize();
    }

    ///////////////////////////////////////////
    //
    // Precompute inferences
    //
    ///////////////////////////////////////////

    public void precomputeInferences(InferenceType... inferenceTypes) throws ReasonerInterruptedException, TimeOutException, InconsistentOntologyException {
        boolean classify = false, realise = false;
        for (InferenceType inferenceType : inferenceTypes)
            switch (inferenceType) {
                case CLASS_HIERARCHY:
                    classify = true;
                case DATA_PROPERTY_HIERARCHY:
                    classify = true;
                case OBJECT_PROPERTY_HIERARCHY:
                    classify = true;
                case CLASS_ASSERTIONS:
                    realise = true;
                case DATA_PROPERTY_ASSERTIONS:
                    realise = true;
                case DIFFERENT_INDIVIDUALS:
                    realise = true;
                case DISJOINT_CLASSES:
                    classify = true;
                case OBJECT_PROPERTY_ASSERTIONS:
                    realise = true;
                case SAME_INDIVIDUAL:
                    realise = true;
            }

        if (classify)
            classify();
        if (realise)
            realise();
    }

    public boolean isPrecomputed(InferenceType inferenceType) {
        switch (inferenceType) {
            case CLASS_HIERARCHY:
                return isClassified();
            case DATA_PROPERTY_HIERARCHY:
                return isClassified();
            case OBJECT_PROPERTY_HIERARCHY:
                return isClassified();
            case CLASS_ASSERTIONS:
                return isRealised();
            case DATA_PROPERTY_ASSERTIONS:
                return isRealised();
            case DIFFERENT_INDIVIDUALS:
                return isRealised();
            case DISJOINT_CLASSES:
                return isClassified();
            case OBJECT_PROPERTY_ASSERTIONS:
                return isRealised();
            case SAME_INDIVIDUAL:
                return isRealised();
            default:
                return false;
        }
    }

    public Set<InferenceType> getPrecomputableInferenceTypes() {
        return CollectionFactory.createSet(InferenceType.CLASS_HIERARCHY, InferenceType.OBJECT_PROPERTY_HIERARCHY, InferenceType.DATA_PROPERTY_HIERARCHY);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // OWLReasonerBase implementation
    //
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    public void classify() {
        startReasonerTask("Classify");
        try {
            racerTranslator.classify();
        } finally {
            //task.stop();
            stopReasonerTask("Classify");
        }
    }

    public boolean isRealised() {
        return racerTranslator.isRealised();
    }

    public void realise() {
        startReasonerTask(ReasonerProgressMonitor.REALIZING);
        try {
            racerTranslator.realise();
        } finally {
            stopReasonerTask(ReasonerProgressMonitor.REALIZING);
        }
    }

    public boolean isDefined(OWLClass cls) {
        return racerTranslator.isDefined(cls);
    }

    public boolean isDefined(OWLObjectProperty prop) {
        return racerTranslator.isDefined(prop);
    }

    public boolean isDefined(OWLDataProperty prop) {
        return racerTranslator.isDefined(prop);
    }

    public boolean isDefined(OWLNamedIndividual ind) {
        return racerTranslator.isDefined(ind);
    }

    public boolean isClassified() {
        return racerTranslator.isClassified();
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // OWLSatisfiableChecker implementation
    //
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    public boolean isSatisfiable(OWLClassExpression description) {
        return racerTranslator.isConsistent(description);
    }

    public boolean isEntailed(Set<? extends OWLAxiom> axioms) throws ReasonerInterruptedException, UnsupportedEntailmentTypeException, TimeOutException, AxiomNotInProfileException, FreshEntitiesException {
        return racerTranslator.isEntailed(axioms);
    }

    public boolean isEntailed(OWLAxiom axiom) throws ReasonerInterruptedException, UnsupportedEntailmentTypeException, TimeOutException, AxiomNotInProfileException, FreshEntitiesException {
        return racerTranslator.isEntailed(axiom);
    }

    public void interrupt() {
        cancelReasonerTask("");
    }

    public boolean isEntailmentCheckingSupported(AxiomType<?> axiomType) {
        return racerTranslator.isEntailmentCheckingSupported(axiomType);
    }


    public Node<OWLClass> getTopClassNode() {
        return getEquivalentClasses(factory.getOWLThing());
    }

    public Node<OWLClass> getBottomClassNode() {
        return getEquivalentClasses(factory.getOWLNothing());
    }

    public Node<OWLDataProperty> getTopDataPropertyNode() {
        return getEquivalentDataProperties(factory.getOWLTopDataProperty());
    }

    public Node<OWLDataProperty> getBottomDataPropertyNode() {
        return getEquivalentDataProperties(factory.getOWLBottomDataProperty());
    }

    public Node<OWLObjectPropertyExpression> getTopObjectPropertyNode() {
        return getEquivalentObjectProperties(factory.getOWLTopObjectProperty());
    }

    public Node<OWLObjectPropertyExpression> getBottomObjectPropertyNode() {
        return getEquivalentObjectProperties(factory.getOWLBottomObjectProperty());
    }

    public NodeSet<OWLClass> getSubClasses(OWLClassExpression ce, boolean direct) {
        return racerTranslator.getSubClasses(ce, direct);
    }

    public NodeSet<OWLClass> getSuperClasses(OWLClassExpression ce, boolean direct) throws InconsistentOntologyException, ClassExpressionNotInProfileException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getSuperClasses(ce, direct);
    }

    public Node<OWLClass> getEquivalentClasses(OWLClassExpression clsC) {
        return racerTranslator.getEquivalentClasses(clsC);
    }

    public Node<OWLClass> getUnsatisfiableClasses() throws ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getInconsistentClasses();
    }

    public NodeSet<OWLClass> getDisjointClasses(OWLClassExpression owlClassExpression) throws ReasonerInterruptedException, TimeOutException, FreshEntitiesException, InconsistentOntologyException {
        return null;
    }

    public NodeSet<OWLObjectPropertyExpression> getSuperObjectProperties(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getSuperObjectProperties(pe, direct);
    }

    public NodeSet<OWLObjectPropertyExpression> getSubObjectProperties(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getSubObjectProperties(pe, direct);
    }

    public Node<OWLObjectPropertyExpression> getEquivalentObjectProperties(OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getEquivalentObjectProperties(pe);
    }

    public NodeSet<OWLClass> getObjectPropertyDomains(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getObjectPropertyDomains(pe, direct);
    }

    public NodeSet<OWLClass> getObjectPropertyRanges(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getObjectPropertyRanges(pe, direct);
    }

    public Node<OWLObjectPropertyExpression> getInverseObjectProperties(OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getInverseObjectProperties(pe);
    }

    public NodeSet<OWLDataProperty> getSuperDataProperties(OWLDataProperty pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getSuperDataProperties(pe, direct);
    }

    public NodeSet<OWLDataProperty> getSubDataProperties(OWLDataProperty pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getSubDataProperties(pe, direct);
    }

    public NodeSet<OWLObjectPropertyExpression> getDisjointObjectProperties(OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return null;
    }

    public NodeSet<OWLDataProperty> getDisjointDataProperties(OWLDataPropertyExpression owlDataPropertyExpression) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return null;
    }

    public Node<OWLDataProperty> getEquivalentDataProperties(OWLDataProperty pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getEquivalentProperties(pe);
    }

    public NodeSet<OWLClass> getDataPropertyDomains(OWLDataProperty pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return new OWLClassNodeSet();
    }

    /////////////////////////////////////////////////////////////////////////////////
    //
    // OWLIndividualReasoner implementation
    //
    /////////////////////////////////////////////////////////////////////////////////


    public Set<OWLLiteral> getDataPropertyValues(OWLNamedIndividual ind, OWLDataProperty pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        Map<OWLDataProperty, Set<OWLLiteral>> map = racerTranslator.getDataPropertyRelationships(ind);
        if (map.containsKey(pe))
            return map.get(pe);
        return Collections.emptySet();
    }

    public NodeSet<OWLNamedIndividual> getObjectPropertyValues(OWLNamedIndividual ind, OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getObjectPropertyValues(ind, pe);


        //todo needs to be implemented by Michael
        /*  Map<OWLObjectProperty, Set<OWLIndividual>> map = racerTranslator.getObjectPropertyRelationships(ind);
     if (map.containsKey(pe)) {
         //return new OWLNamedIndividualNodeSet(map.get(pe));
     }
     return new OWLNamedIndividualNodeSet(); */
    }

    public NodeSet<OWLNamedIndividual> getInstances(OWLClassExpression ce, boolean direct) throws InconsistentOntologyException, ClassExpressionNotInProfileException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getIndividuals(ce, direct);
    }

    public NodeSet<OWLClass> getTypes(OWLNamedIndividual ind, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getTypes(ind, direct);
    }


    /////////////////////////////////////////////////////////////////////////////////
    //
    // OWLIndiviudalPropertyReasoner implementation
    //
    /////////////////////////////////////////////////////////////////////////////////

    public Node<OWLNamedIndividual> getSameIndividuals(OWLNamedIndividual ind) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        return racerTranslator.getSameIndividuals(ind);
    }

    public NodeSet<OWLNamedIndividual> getDifferentIndividuals(OWLNamedIndividual ind) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        NodeSet<OWLNamedIndividual> nodeSet = new OWLNamedIndividualNodeSet(ind);
        return nodeSet;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // Racer specific functionality
    //
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    public String getKnowledgeBaseName() {
        return racerTranslator.getKnowledgeBaseName();
    }

    /**
     * Sends a native racer command.
     * Note: do not end the string with a newline character.
     *
     * @param command
     * @return
     */
    public String sendNativeCommand(String command, boolean inKB) {
        return racerTranslator.sendNativeCommand(command, inKB);
    }

    public boolean isConsistent() {
        return racerTranslator.isConsistent();
    }

    /**
     * Determines whether getRanges(OWLObjectProperty) will return a set of named classes
     * or also anonymous descriptions.
     *
     * @param rangeDescription if <code>true</code> descriptions as well as named classes
     *                         are returned otherwise only named classes
     */
    public void setRangeDescription(boolean rangeDescription) {
        racerTranslator.setRangeDescriptions(rangeDescription);
    }

    /////////////////////////////////////////////
    //
    // Monitorable reasoner implementation
    //
    /////////////////////////////////////////////


    public OWLEntity getCurrentEntity() {
        return manager.getOWLDataFactory().getOWLThing();
    }

    static class MonitorTask3 {
        String reasonerID;
        ReasonerProgressMonitor monitor;
        RacerSocketAdapter adapter;
        PrintWriter writer;
        BufferedReader inputStreamReader;
        Timer timer;
        TimerTask task;
        RacerOWLParser parser;
        volatile boolean isRunning;
        volatile boolean isCanceled;
        final int delay = 100;

        public MonitorTask3(OWLOntologyManager manager, RacerReasonerConfiguration configuration, String reasonerID) {
            InetSocketAddress address = configuration.getSocketAddress();
            this.monitor = configuration.getProgressMonitor();
            this.reasonerID = reasonerID;
            try {
                adapter = new RacerSocketAdapter(address);
                writer = new PrintWriter(new BufferedWriter(adapter.getOutputWriter()));
                inputStreamReader = new BufferedReader(new
                        InputStreamReader(adapter.getInputStream(), "UTF-8"));
                parser = new RacerOWLParser(adapter.getInputStream(), "UTF-8");
                parser.setUp(manager.getOWLDataFactory());
                //we just try to connect to the reasoner
                writer.print(RacerOWLAPIVocabulary.OPEN_BRACKET);
                writer.print(RacerOWLAPIVocabulary.OWLAPI_PROGRESS_CERTAIN);
                writer.print(RacerOWLAPIVocabulary.SPACE);
                writer.print(reasonerID);
                writer.println(RacerOWLAPIVocabulary.CLOSE_BRACKET);
                inputStreamReader.readLine();
            } catch (IOException e) {

            }
            this.timer = new Timer();
        }

        public void start(String message) {
            if (isRunning) stop();
            task = new TimerTask() {
                @Override
                public void run() {
                    try {
                        MonitorTask3.this.run();
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                }
            };
            monitor.reasonerTaskStarted(message);
            isRunning = true;
            timer.schedule(task, delay, 10);
        }

        public void cancel() {
            isCanceled = true;
            stop();
        }

        public void stop() {
            if (isRunning) {
                timer.cancel();
                monitor.reasonerTaskStopped();
                isRunning = false;
            }
            isCanceled = false;
        }

        public void run() throws ParseException {
            boolean isCertain = false;
            writer.print(RacerOWLAPIVocabulary.OPEN_BRACKET);
            writer.print(RacerOWLAPIVocabulary.OWLAPI_PROGRESS_CERTAIN);
            writer.print(RacerOWLAPIVocabulary.SPACE);
            writer.print(reasonerID);
            writer.println(RacerOWLAPIVocabulary.CLOSE_BRACKET);
            try {
                inputStreamReader.readLine();
            } catch (IOException e) {
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }

            isCertain = parser.BooleanResult();

            if (!isCertain) {
                monitor.reasonerTaskBusy();
            } else {
                writer.print(RacerOWLAPIVocabulary.OPEN_BRACKET);
                writer.print(RacerOWLAPIVocabulary.OWLAPI_GET_PROGRESS);
                writer.print(RacerOWLAPIVocabulary.SPACE);
                writer.print(reasonerID);
                writer.println(RacerOWLAPIVocabulary.CLOSE_BRACKET);
                int i = parser.parseIntegerResult();
            }
            while (isRunning) {
                if (isCanceled) {
                    //call abort!
                    writer.print(RacerOWLAPIVocabulary.OPEN_BRACKET);
                    writer.print(RacerOWLAPIVocabulary.OWLAPI_ABORT);
                    writer.print(RacerOWLAPIVocabulary.SPACE);
                    writer.print(reasonerID);
                    writer.println(RacerOWLAPIVocabulary.CLOSE_BRACKET);
                    try {
                        inputStreamReader.readLine();
                    } catch (IOException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                    }
                }
                //isProgressCertain?
                writer.print(RacerOWLAPIVocabulary.OPEN_BRACKET);
                writer.print(RacerOWLAPIVocabulary.OWLAPI_PROGRESS_CERTAIN);
                writer.print(RacerOWLAPIVocabulary.SPACE);
                writer.print(reasonerID);
                writer.println(reasonerID);
                writer.println(RacerOWLAPIVocabulary.CLOSE_BRACKET);

                if (isCertain) {
                    writer.print(RacerOWLAPIVocabulary.OPEN_BRACKET);
                    writer.print(RacerOWLAPIVocabulary.OWLAPI_GET_PROGRESS);
                    writer.println(reasonerID);
                    writer.println(RacerOWLAPIVocabulary.CLOSE_BRACKET);
                    int i = parser.parseIntegerResult();
                    monitor.reasonerTaskProgressChanged(i, 100);
                } else {
                    monitor.reasonerTaskBusy();
                }

            }


        }

    }



    private java.util.logging.Logger logger = java.util.logging.Logger.getLogger(RacerReasonerImpl.class.getName());


}
