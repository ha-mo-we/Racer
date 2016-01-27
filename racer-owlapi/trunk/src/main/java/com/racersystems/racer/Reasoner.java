package com.racersystems.racer;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.*;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNodeSet;
import org.semanticweb.owlapi.reasoner.impl.OWLReasonerBase;
import org.semanticweb.owlapi.util.CollectionFactory;
import org.semanticweb.owlapi.util.Version;

import java.io.*;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

import static com.racersystems.racer.RacerOWLAPIVocabulary.*;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * TCP-based OWLAPI Reasoner adapter for the RacerPro reasoner system 1.9.2 or higher.
 * This implementation uses the new RACER OWLAPI interface which is only available
 * since 1.9.2 or higher.
 *
 * @author Olaf Noppens
 */
public class Reasoner extends OWLReasonerBase implements Racer {

    protected final OWLOntologyManager manager;
    protected final OWLDataFactory factory;
    protected final SocketAdapter serverAdapter;
    protected static SocketAdapter serverControlAdapter;

    protected final Translator racerTranslator;
    private boolean isUnloadOntologyBeforeReloading = false;
    protected AtomicBoolean isInterrupted = new AtomicBoolean(false);
    private Timer timer = new Timer();

    private boolean isAxiomsLoaded = false;

    Thread controlThread;
    ControlPortTask controlPortTask;

    private ReasonerCache reasonerCache;

    class ControlPortTask implements Runnable {
        PrintWriter pw;
        BufferedReader reader;
        String reasonerID;
        AtomicBoolean isCancelled = new AtomicBoolean(false);
        AtomicBoolean isRunning = new AtomicBoolean(false);

        public ControlPortTask(String reasonerID) throws IOException {
            this.reasonerID = reasonerID;
            pw = (PrintWriter) serverControlAdapter.getOutputWriter();
            reader = new BufferedReader(serverControlAdapter.getInputReader());
        }

        public void run() {
            while (this.isRunning.get()) {
                try {
                    synchronized (pw) {

                        if (isCancelled.get()) {
                            pw.print(OPEN_BRACKET);
                            pw.print(OWLAPI_ABORT);
                            pw.print(SPACE);
                            pw.print(reasonerID);
                            pw.println(CLOSE_BRACKET);
                            pw.flush();
                            // System.out.println("sended: cancel!");
                            try {
                                String s = reader.readLine();
                               // System.out.println("getting answer" + s);
                                pw.close();
                            } catch (Exception e) {
                            }
                            this.isRunning.set(false);
                            return;
                        }
                        pw.print(OPEN_BRACKET);
                        pw.print(OWLAPI_PROGRESS_CERTAIN);
                        pw.print(SPACE_CHAR);
                        pw.print(reasonerID);
                        pw.println(CLOSE_BRACKET);
                        pw.flush();
                        String answer = reader.readLine();
                        if ("t".equals(answer)) {
                            pw.print(OPEN_BRACKET);
                            pw.print(OWLAPI_GET_PROGRESS);
                            pw.print(SPACE);
                            pw.print(reasonerID);
                            pw.println(CLOSE_BRACKET);
                            pw.flush();
                            String value = reader.readLine();
                            //System.out.println("\t progress " + value);
                            if (this.isRunning.get())
                                getReasonerConfiguration().getProgressMonitor().reasonerTaskProgressChanged(Integer.parseInt(value), 100);
                        } else {
                            if (this.isRunning.get())
                                getReasonerConfiguration().getProgressMonitor().reasonerTaskBusy();
                        }
                    }
                    if (this.isRunning.get())
                        Thread.currentThread().sleep(500);
                } catch (InterruptedException e) {
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

        public void start() {
            synchronized (pw) {
                this.isRunning.set(true);
            }
        }

        public void stop() {
            synchronized (pw) {
                this.isRunning.set(false);
            }
        }

        public void cancel() {
            synchronized (pw) {
                this.isCancelled.set(true);
                //System.out.println("set isCancelled to true");
            }
        }
    }

    @Deprecated
    class ProgressMonitorTask extends TimerTask {
        PrintWriter out;
        BufferedReader reader;
        String reasonerID;
        boolean isCanceled;

        String task;

        public ProgressMonitorTask(String task, ReasonerConfiguration configuration, String reasonerID) throws IOException {
            //  this.configuration = configuration;
            this.reasonerID = reasonerID;
            out = (PrintWriter) serverControlAdapter.getOutputWriter();
            reader = new BufferedReader(serverControlAdapter.getInputReader());
            this.task = task;
        }

        public void run() {
            try {
                //if it is certain?
                // System.out.println("asking racerpro...");
                out.println(OPEN_BRACKET + OWLAPI_PROGRESS_CERTAIN + " " + reasonerID + CLOSE_BRACKET);
                out.flush();
                // System.out.println("...waiting");
                if (reader.readLine().equals("t")) {
                    //  System.out.println("...got answer");
                    //  System.out.print("Timepoint " + System.currentTimeMillis());
                    // System.out.println("... got an answer");
                    out.print(OPEN_BRACKET);
                    out.print(OWLAPI_GET_PROGRESS);
                    out.print(SPACE);
                    out.print(reasonerID);
                    out.println(CLOSE_BRACKET);
                    out.flush();
                    String value = reader.readLine();
                    //System.out.println("\t progress " + value);
                    getReasonerConfiguration().getProgressMonitor().reasonerTaskProgressChanged(Integer.parseInt(value), 100);
                } else {
                    //  System.out.println("...got answer");
                    getReasonerConfiguration().getProgressMonitor().reasonerTaskBusy();
                }
            } catch (IOException io) {
                System.out.println("run exception " + task + io.getMessage());
            }
        }

        public void start(String taskName) {
            getReasonerConfiguration().getProgressMonitor().reasonerTaskStarted(taskName);
        }

        public boolean cancel() {
            //System.out.println("sending cancel command");
            isCanceled = true;
            out.print(OPEN_BRACKET);
            out.print(OWLAPI_ABORT);
            out.print(SPACE);
            out.print(reasonerID);
            out.println(CLOSE_BRACKET);
            try {
                String s = reader.readLine();
                //System.out.println(s);
                out.close();
            } catch (IOException e) {
            }
            return super.cancel();
        }

        public void stop(String taskName) {
            getReasonerConfiguration().getProgressMonitor().reasonerTaskStopped();
        }


    }

    private void startReasonerTask(String taskName) {

        if (this.controlThread != null) {
            this.controlPortTask.stop();
        }
        this.controlThread = new Thread((controlPortTask));
        controlPortTask.start();
        this.controlThread.start();

        getReasonerConfiguration().getProgressMonitor().reasonerTaskStarted(taskName);
    }

    private void setProgressRange(int count, int secondCount) {
         PrintWriter out = (PrintWriter) serverAdapter.getOutputWriter();
        BufferedReader reader = new BufferedReader(serverAdapter.getInputReader());

        String s = (OPEN_BRACKET + OWLAPI_SET_PROGRESSRANGE
                + SPACE
                + count
                + SPACE
                + "0"
                + SPACE
                + secondCount
                + SPACE
                + racerTranslator.getKnowledgeBaseName()
                + CLOSE_BRACKET);

        out.println(s);
        out.flush();
        try {
            reader.readLine();
        } catch (IOException e) {
            e.printStackTrace();
        }


    }

    private void stopReasonerTask(String taskName) {
        this.controlPortTask.stop();
        getReasonerConfiguration().getProgressMonitor().reasonerTaskStopped();
    }

    private void cancelReasonerTask(String taskName) {
         this.controlPortTask.cancel();
    }


    public Reasoner(OWLOntology rootOntology, Configuration configuration, BufferingMode bufferingMode) {
        super(rootOntology, configuration.clone(), bufferingMode);
        //System.out.println("reasoner creation...");
        this.manager = rootOntology.getOWLOntologyManager();
        this.factory = manager.getOWLDataFactory();

        try {
            serverAdapter = new SocketAdapter(configuration.getSocketAddress());

            if (serverControlAdapter == null) {
                serverControlAdapter = new SocketAdapter(new InetSocketAddress(
                        getReasonerConfiguration().getSocketAddress().getAddress(), 8089));
            }

             //System.out.println("serverControlAdapter created");
             /* this.serverControlAdapter = new SocketAdapter(new InetSocketAddress(
                    getReasonerConfiguration().getSocketAddress().getAddress(), 8089));
              */

        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
        //System.out.println("adapter created");

        this.racerTranslator = createRacerTranslator();
        //System.out.println("translator created");
        try {
            this.controlPortTask = new ControlPortTask(racerTranslator.getKnowledgeBaseName());
        } catch (IOException e) {
            e.printStackTrace();
        }

        //System.out.println("reasoner created...");
        if (configuration.getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_CREATION) {
            loadAxioms();
        }
        this.reasonerCache = new ReasonerCache(this.racerTranslator);
    }

    protected Translator createRacerTranslator() {
        return new Translator(getOWLOntologyManager(), getReasonerConfiguration(),
                serverAdapter.getOutputWriter(), serverAdapter.getInputReader());
    }

    protected void loadAxioms() {
        if (!this.isAxiomsLoaded) {
            setProgressRange(getReasonerAxioms().size(), 90);
            startReasonerTask(ReasonerProgressMonitor.LOADING);
            //System.out.println("load axioms...");
            this.racerTranslator.handleChanges(getReasonerAxioms(), Collections.<OWLAxiom>emptySet(), getReasonerConfiguration().getProgressMonitor());
            stopReasonerTask(ReasonerProgressMonitor.LOADING);
            this.isAxiomsLoaded = true;
            //System.out.println("axioms loaded.");
        }

    }

    public String getReasonerName() {
        return "Racer";
    }

    public Version getReasonerVersion() {
        return racerTranslator.getRacerVersionNumber();
    }

    @Override
    public Configuration getReasonerConfiguration() {
        return (Configuration) super.getReasonerConfiguration();
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
        if (getReasonerConfiguration().isMemorySavingModeEnabled()) {
            if (removeAxioms.size() > 0) {
                racerTranslator.disposeKnowledgeBase();
                racerTranslator.createKnowledgeBase();
                racerTranslator.handleChanges(getReasonerAxioms(), Collections.<OWLAxiom>emptySet());
            } else {
                racerTranslator.handleChanges(addAxioms, removeAxioms);
            }
        } else
            racerTranslator.handleChanges(addAxioms, removeAxioms);

        reasonerCache.invalidate();
    }


    /**
     * @inheritDoc
     */
    public void dispose() {
        timer.cancel();
        super.dispose();
        racerTranslator.dispose();
        try {
            serverAdapter.closeConnection();
        } catch (Exception e) {
            logger.severe(e.toString());
            throw new OWLRuntimeException(e);
        } finally {
            try {
                //serverControlAdapter.closeConnection();
            } catch (Exception e) {
                logger.severe(e.toString());
                throw new OWLRuntimeException(e);
            }
        }
    }

    public void close() {
        disposeSilently();
    }

    protected void disposeSilently() {
        timer.cancel();
        super.dispose();
        racerTranslator.disposeSilently();
        try {
            serverAdapter.closeConnection();
        } catch (Exception e) {
            logger.severe(e.toString());
            throw new OWLRuntimeException(e);
        } finally {
            try {
                //serverControlAdapter.closeConnection();
            } catch (Exception e) {
                logger.severe(e.toString());
                throw new OWLRuntimeException(e);
            }
        }

    }

    @Override
    protected void finalize() throws Throwable {
        try {
            dispose();
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
        this.isInterrupted.set(false);
        for (InferenceType inferenceType : inferenceTypes)
            switch (inferenceType) {
                case CLASS_HIERARCHY:
                    classify = true;
                    break;
                case DATA_PROPERTY_HIERARCHY:
                    classify = true;
                    break;
                case OBJECT_PROPERTY_HIERARCHY:
                    classify = true;
                    break;
                case CLASS_ASSERTIONS:
                    realise = true;
                    break;
                case DATA_PROPERTY_ASSERTIONS:
                    realise = true;
                    break;
                case DIFFERENT_INDIVIDUALS:
                    realise = true;
                    break;
                case DISJOINT_CLASSES:
                    classify = true;
                    break;
                case OBJECT_PROPERTY_ASSERTIONS:
                    realise = true;
                    break;
                case SAME_INDIVIDUAL:
                    realise = true;
                    break;
            }
        if (inferenceTypes == null || inferenceTypes.length == 0) {
            classify = true;
            realise = true;
        }
        if (!this.isAxiomsLoaded && (getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY ||
                getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_PRECOMPUTE))
            loadAxioms();
        if (classify && !isInterrupted.get())
            classify();
        if (realise && !isInterrupted.get())
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
        return CollectionFactory.createSet(InferenceType.CLASS_HIERARCHY, InferenceType.OBJECT_PROPERTY_HIERARCHY,
                InferenceType.DATA_PROPERTY_HIERARCHY, InferenceType.CLASS_ASSERTIONS,
                InferenceType.DATA_PROPERTY_ASSERTIONS, InferenceType.OBJECT_PROPERTY_ASSERTIONS,
                InferenceType.SAME_INDIVIDUAL, InferenceType.DIFFERENT_INDIVIDUALS, InferenceType.DISJOINT_CLASSES
        );
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // OWLReasonerBase implementation
    //
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    public void classify() {
        if (!this.isAxiomsLoaded && (getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY ||
                getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_PRECOMPUTE))
            loadAxioms();
        startReasonerTask(ReasonerProgressMonitor.CLASSIFYING);
        try {
            //System.out.println("startClassify");
            racerTranslator.classify();
            //System.out.println("endClassify");
        } catch (Exception e) {
            System.out.println("exception when classify" + e);
        } finally {
            stopReasonerTask(ReasonerProgressMonitor.CLASSIFYING);
        }
    }

    public boolean isRealised() {
        return racerTranslator.isRealised();
    }

    public void realise() {
        if (!this.isAxiomsLoaded && (getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY
                || getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_PRECOMPUTE))
            loadAxioms();
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
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.isConsistent(description);
    }

    public boolean isEntailed(Set<? extends OWLAxiom> axioms) throws ReasonerInterruptedException, UnsupportedEntailmentTypeException, TimeOutException, AxiomNotInProfileException, FreshEntitiesException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.isEntailed(axioms);
    }

    public boolean isEntailed(OWLAxiom axiom) throws ReasonerInterruptedException, UnsupportedEntailmentTypeException, TimeOutException, AxiomNotInProfileException, FreshEntitiesException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.isEntailed(axiom);
    }

    public void interrupt() {
        //System.out.println("interrupt current reasoner operation!");
        this.isInterrupted.set(true);
        this.racerTranslator.interrupt();
        cancelReasonerTask("");
        //System.out.println("interrupted!");
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
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return reasonerCache.getSubClasses(ce, direct);
    }

    public NodeSet<OWLClass> getSuperClasses(OWLClassExpression ce, boolean direct) throws InconsistentOntologyException, ClassExpressionNotInProfileException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return reasonerCache.getSuperClasses(ce,direct);
    }

    public Node<OWLClass> getEquivalentClasses(OWLClassExpression clsC) {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return reasonerCache.getEquivalentClasses(clsC);
    }

    public Node<OWLClass> getUnsatisfiableClasses() throws ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getInconsistentClasses();
    }

    public NodeSet<OWLClass> getDisjointClasses(OWLClassExpression owlClassExpression) throws ReasonerInterruptedException, TimeOutException, FreshEntitiesException, InconsistentOntologyException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return reasonerCache.getDisjointClasses(owlClassExpression);
    }

    public NodeSet<OWLObjectPropertyExpression> getSuperObjectProperties(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getSuperObjectProperties(pe, direct);
    }

    public NodeSet<OWLObjectPropertyExpression> getSubObjectProperties(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getSubObjectProperties(pe, direct);
    }

    public Node<OWLObjectPropertyExpression> getEquivalentObjectProperties(OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getEquivalentObjectProperties(pe);
    }

    public NodeSet<OWLClass> getObjectPropertyDomains(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getObjectPropertyDomains(pe, direct);
    }

    public NodeSet<OWLClass> getObjectPropertyRanges(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getObjectPropertyRanges(pe, direct);
    }

    public Node<OWLObjectPropertyExpression> getInverseObjectProperties(OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getInverseObjectProperties(pe);
    }

    public NodeSet<OWLDataProperty> getSuperDataProperties(OWLDataProperty pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getSuperDataProperties(pe, direct);
    }

    public NodeSet<OWLDataProperty> getSubDataProperties(OWLDataProperty pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getSubDataProperties(pe, direct);
    }

    public NodeSet<OWLObjectPropertyExpression> getDisjointObjectProperties(OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getDisjointObjectProperties(pe);
    }

    public NodeSet<OWLDataProperty> getDisjointDataProperties(OWLDataPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getDisjointDataProperties(pe);
    }

    public Node<OWLDataProperty> getEquivalentDataProperties(OWLDataProperty pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
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
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        Map<OWLDataProperty, Set<OWLLiteral>> map = racerTranslator.getDataPropertyRelationships(ind);
        if (map.containsKey(pe))
            return map.get(pe);
        return Collections.emptySet();
    }

    public NodeSet<OWLNamedIndividual> getObjectPropertyValues(OWLNamedIndividual ind, OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getObjectPropertyValues(ind, pe);


        //todo needs to be implemented by Michael
        /*  Map<OWLObjectProperty, Set<OWLIndividual>> map = racerTranslator.getObjectPropertyRelationships(ind);
     if (map.containsKey(pe)) {
         //return new OWLNamedIndividualNodeSet(map.get(pe));
     }
     return new OWLNamedIndividualNodeSet(); */
    }

    public NodeSet<OWLNamedIndividual> getInstances(OWLClassExpression ce, boolean direct) throws InconsistentOntologyException, ClassExpressionNotInProfileException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getIndividuals(ce, direct);
    }

    public NodeSet<OWLClass> getTypes(OWLNamedIndividual ind, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getTypes(ind, direct);
    }


    /////////////////////////////////////////////////////////////////////////////////
    //
    // OWLIndiviudalPropertyReasoner implementation
    //
    /////////////////////////////////////////////////////////////////////////////////

    public Node<OWLNamedIndividual> getSameIndividuals(OWLNamedIndividual ind) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getSameIndividuals(ind);
    }

    public NodeSet<OWLNamedIndividual> getDifferentIndividuals(OWLNamedIndividual ind) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (!this.isAxiomsLoaded && getReasonerConfiguration().getAxiomLoadingStrategy() == AxiomLoadingStrategy.LOAD_ON_QUERY)
            loadAxioms();
        return racerTranslator.getDifferentIndividuals(ind);
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // Racer specific functionality
    //
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    public void shutdown() {
        racerTranslator.shutdown();
    }


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
        return reasonerCache.isConsistent();

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
        SocketAdapter adapter;
        PrintWriter writer;
        BufferedReader inputStreamReader;
        Timer timer;
        TimerTask task;
        RacerOWLParser parser;
        volatile boolean isRunning;
        volatile boolean isCanceled;
        final int delay = 100;

        public MonitorTask3(OWLOntologyManager manager, ReasonerConfiguration configuration, String reasonerID) {
            InetSocketAddress address = configuration.getSocketAddress();
            this.monitor = configuration.getProgressMonitor();
            this.reasonerID = reasonerID;
            try {
                adapter = new SocketAdapter(address);
                writer = new PrintWriter(new BufferedWriter(adapter.getOutputWriter()));
                inputStreamReader = new BufferedReader(new
                        InputStreamReader(adapter.getInputStream(), "UTF-8"));
                parser = new RacerOWLParser(adapter.getInputStream(), "UTF-8");
                parser.setUp(manager.getOWLDataFactory());
                //we just try to connect to the reasoner
                writer.print(OPEN_BRACKET);
                writer.print(OWLAPI_PROGRESS_CERTAIN);
                writer.print(SPACE);
                writer.print(reasonerID);
                writer.println(CLOSE_BRACKET);
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
            writer.print(OPEN_BRACKET);
            writer.print(OWLAPI_PROGRESS_CERTAIN);
            writer.print(SPACE);
            writer.print(reasonerID);
            writer.println(CLOSE_BRACKET);
            try {
                inputStreamReader.readLine();
            } catch (IOException e) {
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }

            isCertain = parser.BooleanResult();

            if (!isCertain) {
                monitor.reasonerTaskBusy();
            } else {
                writer.print(OPEN_BRACKET);
                writer.print(OWLAPI_GET_PROGRESS);
                writer.print(SPACE);
                writer.print(reasonerID);
                writer.println(CLOSE_BRACKET);
                int i = parser.parseIntegerResult();
            }
            while (isRunning) {
                if (isCanceled) {
                    //call abort!
                    writer.print(OPEN_BRACKET);
                    writer.print(OWLAPI_ABORT);
                    writer.print(SPACE);
                    writer.print(reasonerID);
                    writer.println(CLOSE_BRACKET);
                    try {
                        inputStreamReader.readLine();
                    } catch (IOException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                    }
                }
                //isProgressCertain?
                writer.print(OPEN_BRACKET);
                writer.print(OWLAPI_PROGRESS_CERTAIN);
                writer.print(SPACE);
                writer.print(reasonerID);
                writer.println(reasonerID);
                writer.println(CLOSE_BRACKET);

                if (isCertain) {
                    writer.print(OPEN_BRACKET);
                    writer.print(OWLAPI_GET_PROGRESS);
                    writer.println(reasonerID);
                    writer.println(CLOSE_BRACKET);
                    int i = parser.parseIntegerResult();
                    monitor.reasonerTaskProgressChanged(i, 100);
                } else {
                    monitor.reasonerTaskBusy();
                }

            }


        }

    }


    private java.util.logging.Logger logger = java.util.logging.Logger.getLogger(Reasoner.class.getName());


    public class RacerClient {

        private int socket;
        private String host = null;

        private boolean printWarningMessages = true;
        private boolean debugging = false;
        private boolean useStringBuildersEvenForTokens = false;
        private boolean globallyEnableSimplifiedProtocol = true;

        private String lastWarning = "";

        private Socket racerSocket = null;
        public PrintWriter out = null;
        public BufferedReader in = null;

        private Map<String, Object[]> withMacroStack = new LinkedHashMap<String, Object[]>();

        public RacerClient(String host, int socket) {
            this.host = host;
            this.socket = socket;
        }

        public void openConnection() throws RuntimeException {

            try {

                racerSocket = new Socket(host, socket);

                racerSocket.setReuseAddress(true);

                out = new PrintWriter(new OutputStreamWriter(racerSocket.getOutputStream(), "UTF-8"));

                in = new BufferedReader(new InputStreamReader(racerSocket.getInputStream(), "UTF-8"));

                //  out.println("(|OWLAPI-disableSimplifiedProtocol| :|global|)");
//            out.flush();
//            in.readLine();

                //          if (globallyEnableSimplifiedProtocol) {

//                out.println("(|OWLAPI-enableSimplifiedProtocol| :|global|)");
//                out.flush();
//                in.readLine();

//            }

            } catch (Exception ex) {

                throw new RuntimeException(ex.getMessage());

            }

        }

        public void closeConnection() {
            try {

                out.close();

                in.close();

                racerSocket.close();

            } catch (IOException ex) {

                throw new RuntimeException(ex.getMessage());

            }

        }


        public String sendRaw(String command) {
            try {

                // out.println("(|OWLAPI-disableSimplifiedProtocol| :|global|)");
                //out.flush();
                //in.readLine();

                out.println(command);
                out.flush();

                String result = parse(in.readLine());

                //if (globallyEnableSimplifiedProtocol) {

//                out.println("(|OWLAPI-enableSimplifiedProtocol| :|global|)");
//                out.flush();
//                in.readLine();

                //          }

                return result;

            } catch (Exception ex) {

                throw new RuntimeException(ex.getMessage());

            }

        }


        private String parse(String result) {
            return result;
        }

        private String simpleParse(String result) throws RuntimeException {

            return result;

        }


    }

    /**
     * Convenience method for creating a reasoner object.
     * <br/>
     * It is equivalent to new ReasonerFactory().createReasoner();
     *
     * @param ontology
     * @return
     */
    public static OWLReasoner createReasoner(OWLOntology ontology) {
        return new ReasonerFactory().createReasoner(ontology);
    }

    /**
     * Convenience method for creating a reasoner object.
     * <br/>
     * It is equivalent to new ReasonerFactory().createReasoner(OWLOntology, OWLReaonerConfiguration);
     *
     * @param ontology
     * @return
     */
    public static OWLReasoner createReasoner(OWLOntology ontology, OWLReasonerConfiguration config) {
        return new ReasonerFactory().createReasoner(ontology, config);
    }

    /**
     * Convenience method for creating a reasoner object.
     * <br/>
     * It is equivalent to new ReasonerFactory().createNonBufferingReasoner(OWLOntology);
     *
     * @param ontology
     * @return
     */
    public static OWLReasoner createNonBufferingReasoner(OWLOntology ontology) {
        return new ReasonerFactory().createNonBufferingReasoner(ontology);
    }

    /**
     * Convenience method for creating a reasoner object.
     * <br/>
     * It is equivalent to new ReasonerFactory().createReasoner(OWLONt);
     *
     * @param ontology
     * @return
     */
    public static OWLReasoner CreateNonBufferingReasoner(OWLOntology ontology, OWLReasonerConfiguration config) {
        return new ReasonerFactory().createNonBufferingReasoner(ontology, config);
    }

    public static class ReasonerFactory implements OWLReasonerFactory {
        private static final String reasonerName = "Racer";

        public final String getReasonerName() {
            return reasonerName;
        }

        protected Configuration getConfiguration(OWLReasonerConfiguration configuration) {
            if (configuration instanceof Configuration) return (Configuration) configuration;
            return new Configuration(configuration);
        }

        public OWLReasoner createNonBufferingReasoner(OWLOntology ontology) {
            return new Reasoner(ontology, new Configuration(), BufferingMode.NON_BUFFERING);
        }

        public OWLReasoner createReasoner(OWLOntology ontology) {
            return new Reasoner(ontology, new Configuration(), BufferingMode.BUFFERING);
        }

        public OWLReasoner createNonBufferingReasoner(OWLOntology ontology, OWLReasonerConfiguration config) throws IllegalConfigurationException {
            return new Reasoner(ontology, getConfiguration(config), BufferingMode.NON_BUFFERING);
        }

        public OWLReasoner createReasoner(OWLOntology ontology, OWLReasonerConfiguration config) throws IllegalConfigurationException {
            return new Reasoner(ontology, getConfiguration(config), BufferingMode.BUFFERING);
        }

    }


}
