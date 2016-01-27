package com.racersystems.racer;

import com.racersystems.racer.util.RacerBufferedWriter;
import com.racersystems.racer.util.RacerPrefixManager;
import com.racersystems.racer.util.RacerShortFormProvider;
import com.racersystems.racer.util.ShortFormProvider;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.*;
import org.semanticweb.owlapi.reasoner.impl.*;
import org.semanticweb.owlapi.util.OWLAxiomVisitorAdapter;
import org.semanticweb.owlapi.util.Version;

import java.io.*;
import java.util.*;

import static com.racersystems.racer.RacerKRSSVocabulary.EXIT;
import static com.racersystems.racer.RacerKRSSVocabulary.INIT_ATTRIBUTE;
import static com.racersystems.racer.RacerKRSSVocabulary.IN_KNOWLEDGEBASE;
import static com.racersystems.racer.RacerKRSSVocabulary.NIL;
import static com.racersystems.racer.RacerKRSSVocabulary.SPACE;
import static com.racersystems.racer.RacerKRSSVocabulary.TRUE;
import static com.racersystems.racer.RacerOWLAPIVocabulary.*;
import static org.semanticweb.owlapi.util.CollectionFactory.createSet;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * @author Olaf Noppens
 */
public class Translator extends OWLAxiomVisitorAdapter implements OWLOntologyChangeVisitor {
    protected PrintWriter writer;
    protected final Reader inputStream;
    protected DescriptionRenderer renderer;
    protected NamedObjectForAnnotationRenderer noaRenderer;
    protected RacerOWLParser resultParser;
    protected OWLOntologyManager owlOntologyManager;
    protected String reasonerID;
    protected String ontologyID;
    protected Set<OWLClass> definedClasses;
    protected boolean isAdd = true;
    private boolean autoBatchSynchronizeNeeded = false;
    private Configuration configuration;
    public StringBuffer buffer = new StringBuffer();
    private boolean rangeDescriptions = false;
    private boolean isSimplifiedRacerDispose = false;
    boolean isInterrupted = false;
    SingleCommand commandVisitor;

    RacerPrefixManager prefixManager = new RacerPrefixManager();
    RacerBufferedWriter bufferedWriter;
    PrintWriter directWriter;

    public Translator(OWLOntologyManager manager, Configuration configuration, final Writer writer, Reader reader) {
        this.owlOntologyManager = manager;
        this.configuration = configuration;
        this.bufferedWriter = new RacerBufferedWriter(writer);
        this.writer = new PrintWriter(bufferedWriter);

        this.directWriter = new PrintWriter(new BufferedWriter(writer));



        this.inputStream = reader;
        this.renderer = createDefaultRacerDescriptionRenderer();
        this.noaRenderer = new NamedObjectForAnnotationRenderer(this.writer, this.renderer.getShortFormProvider());
        this.resultParser = createDefaultRacerParser();
        this.definedClasses = createSet();

        Version availableVersion = getRacerVersionNumber();
        Version expectedVersion = new Version(2, 0, 0, 20101025);
        if (availableVersion.getMajor() < expectedVersion.getMajor() &&
                availableVersion.getMinor() <= expectedVersion.getMinor() &&
                availableVersion.getBuild() <= expectedVersion.getBuild()) {
            throw new NotSupportedRacerException(availableVersion);
        }
        //System.out.println(availableVersion);
        if (!isOWLAPIInit())
            try {
                writeOpenPar();
                writeKRSSMethodName(OWLAPI_INIT);
                writeCloseParln();
                // System.out.println("sending OWLAPI_INIT");
                String s = readFromSocket();
                //System.out.println("answer " + s);
            } catch (Exception e) {
            }


        createKnowledgeBase();
        // autoApplyChanges();
        this.commandVisitor = new SingleCommand();
    }

    protected void interrupt() {
        this.isInterrupted = true;
    }

    public void setSimplifiedRacerDispose(boolean simplified) {
        isSimplifiedRacerDispose = simplified;
    }

    public boolean isOWLAPIInit() {
        //System.out.println("isOWLAPIINIT---");
        try {
            writeOpenPar();
            writeKRSSMethodName("all-tboxes");
            writeCloseParln();
            String s = readFromSocket();
            s = s.replace("(", "");
            s = s.replace(")", "");
            s = s.replace("\"", "");
            StringTokenizer st = new StringTokenizer(s, " ");
            while (st.hasMoreTokens()) {
                String tbox = st.nextToken();
                if (tbox.equals("OWLAPI-KB"))
                    return true;
            }
        } catch (Exception e) {
        }
        return false;
    }

    public String getKnowledgeBaseName() {
        return reasonerID;
    }

    protected RacerOWLParser createDefaultRacerParser() {
        RacerOWLParser parser = new RacerOWLParser(inputStream);
        parser.setUp(owlOntologyManager.getOWLDataFactory());
        return parser;
    }

    protected DescriptionRenderer createDefaultRacerDescriptionRenderer() {
        return new DescriptionRenderer(prefixManager, writer, new RacerShortFormProvider(owlOntologyManager.getOWLDataFactory()));
    }

    public final DescriptionRenderer getDescriptionRenderer() {
        return renderer;
    }

    ////////////////////////////////////////////////////////////////////////////////
    //
    //      OWLOntologyChangeVisitor
    //
    ////////////////////////////////////////////////////////////////////////////////

    public final void visit(final AddAxiom change) {
        /*     final OWLOntology ontology = change.getOntology();
if (!isAdd)
 batchSynchronize(this.currentOntology);
if (!isAdd || !ontology.equals(this.currentOntology)) {
 this.currentOntology = change.getOntology();
 autoAddAxioms(this.currentOntology, false);
}
isAdd = true;
change.getAxiom().accept(this);           */
    }

    public final void visit(final RemoveAxiom change) {
        /* final OWLOntology ontology = change.getOntology();
     if (isAdd || !ontology.equals(this.currentOntology)) {
         this.currentOntology = change.getOntology();
         autoBatchRemoveAxiomsFrom(this.currentOntology);
         //autoRemoveAxioms(this.currentOntology, false);
     }
     isAdd = false;
     change.getAxiom().accept(this);   */
    }

    public void visit(SetOntologyID change) {
    }

    public void visit(AddImport change) {
    }

    public void visit(RemoveImport change) {
    }

    public void visit(AddOntologyAnnotation change) {
    }

    public void visit(RemoveOntologyAnnotation change) {
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // KnowledgeBase creation
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

    public final void createKnowledgeBase() {
        if (this.reasonerID != null)
            disposeOWLReasoner();
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_NEW_REASONER);
            writeSpace();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_CREATE_ATTRIBUTE);
            writeCloseParln();
            String tmp = resultParser.readNativeLine();
            if (tmp.startsWith(":answer")) {
                tmp = tmp.replace(":answer", "");
                int i = tmp.indexOf("\"");
                if (i > -1) {
                    int l = tmp.lastIndexOf("\"");
                    if (l <= i)
                        l = tmp.length();
                    tmp = tmp.substring(i, l);
                }
            }
            tmp = tmp.replace("\"", "");
            tmp = tmp.replace("\\", "");
            tmp = tmp.replace(" ", "");

            this.reasonerID = tmp;
            // System.out.println(reasonerID);
            this.ontologyID = reasonerID;

            enableSimplifiedProtocol();

            writeOpenPar();
            writeKRSSMethodName(OWLAPI_NEW_ONTOLOGY);
            writeS(ontologyID);
            writeS(reasonerID);
            writeCloseParln();
            resultParser.parseOWLStringAnswer();


            /*    writeOpenPar();
       writeKRSSMethodName(OWLAPI_ENABLE_MEMORYSAVING_MODE);
       writeS(ontologyID);
       writeS(reasonerID);
       writeCloseParln();
       String s = readFromSocket();  */

            // autoAddAxioms(reasonerID);

            writeOpenPar();
            writeKRSSMethodName(OWLAPI_LOADONTOLOGY);
            writeS(ontologyID);
            writeS(reasonerID);
            writeCloseParln();
            resultParser.parseOWLAnswer();

            if (configuration.isMemorySavingModeEnabled())
                enableMemorySavingMode();
        } catch (Exception e) {
            reasonerID = null;
            throw new RacerRuntimeException(e);
        }
    }

    protected void enableMemorySavingMode() {
        writeOpenPar();
        writeKRSSMethodName(OWLAPI_ENABLE_MEMORYSAVING_MODE);
        writeS(ontologyID);
        writeS(reasonerID);
        writeCloseParln();
        String s = readFromSocket();
    }

    public void disposeKnowledgeBase() {
        try {

            if (ontologyID != null && !isSimplifiedRacerDispose) {
                writeOpenPar();
                writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_UNLOAD_ONTOLOGY);
                writeS(ontologyID);
                writeCloseParln();
                this.ontologyID = null;
                resultParser.parseOWLAnswer();
            }
            if (this.reasonerID != null) {
                writeOpenPar();
                writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_DISPOSE_REASONER);
                writeS(reasonerID);
                writeCloseParln();
                this.reasonerID = null;
                resultParser.readNativeLine();
            }
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void disposeSilently() {
        this.ontologyID = null;
        this.reasonerID = null;
    }

    @Deprecated
    public final void createOWLReasoner() {
        if (reasonerID != null) {
            disposeOWLReasoner();
        }
        try {
            reasonerID = "OWLAPIReasoner" + String.valueOf(System.currentTimeMillis());
//            System.out.println("reasoner name");
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_NEW_REASONER);
            writeS(reasonerID);
            writeCloseParln();
            /*System.out.println("read from socket:");*/
            readFromSocket();
            //resultParser.parseOWLSimpleAnswer();
            /*System.out.println("enable protocoll");*/
            enableSimplifiedProtocol();
        } catch (Exception e) {
            /*System.out.println("exception!");
            e.printStackTrace();*/
            reasonerID = null;
            throw new RacerRuntimeException(e);
        }
    }

    public void disposeOWLReasoner() {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_DISPOSE_REASONER);
            writeS(reasonerID);
            writeCloseParln();
            resultParser.parseOWLAnswer();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public final void enableSimplifiedProtocol() {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_ENABLE_SIMPLIFY_PROTOCOL);
            writeS(reasonerID);
            writeCloseParln();
            String s = resultParser.readNativeLine();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public final void autoApplyChanges() {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_AUTOAPPLYCHANGES);
            writeS(reasonerID);
            writeCloseParln();
            resultParser.readNativeLine();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    protected final void setAxiomID() {
        writeOpenPar();
        writeKRSSMethodName(OWLAPI_SETAXIOMXCOUNTER);
        writeS(0);
        writeClosePar();
    }

    /*
protected final void autoAddAxioms(OWLOntology ontology, boolean waitForAnswer) {
 if (waitForAnswer) this.autoAddAxioms(ontology);
 else {
     try {
         writeOpenPar();
         writeKRSSMethodName(OWLAPI_AUTOADDAXIOMS);
         writeS(ontology.getOntologyID().getOntologyIRI());
         writeS(reasonerID);
         writeClosePar();
         isAdd = true;
     } catch (Exception e) {
         throw new RacerRuntimeException(e);
     }
 }
}       */


    protected final void autoBatchRemoveAxioms() {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_AUTOBATCHREMOVEAXIOMSFROM);
            writeS(this.ontologyID);
            writeS(reasonerID);
            writeCloseParln();
            resultParser.readNativeLine();
            isAdd = false;
            this.autoBatchSynchronizeNeeded = true;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    /**
     * Switches into autoBatchAddAxiomsTo mode. The command will be send, the anser will be read.
     */
    protected final void autoBatchAddAxioms() {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_AUTOBATCHADDAXIOMSTO);
            writeS(this.ontologyID);
            writeS(reasonerID);
            writeCloseParln();
            resultParser.readNativeLine();
            isAdd = false;
            this.autoBatchSynchronizeNeeded = true;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }


    protected final void batchSynchronize() {
        if (!this.autoBatchSynchronizeNeeded) return;
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_BATCHSYNCHONIZE);
            writeS(ontologyID);
            writeS(reasonerID);
            writeCloseParln();
            String s = resultParser.readNativeLine();
            isAdd = !isAdd;
            this.autoBatchSynchronizeNeeded = false;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    protected final void autoRemoveAxioms(String ontology, boolean waitForAnswer) {
        if (waitForAnswer) this.autoRemoveAxioms(ontology, false);
        else {
            try {
                writeOpenPar();
                writeKRSSMethodName(OWLAPI_AUTOREMOVEAXIOMS);
                writeS(ontology);
                writeS(reasonerID);
                writeCloseParln();
                resultParser.readNativeLine();
                isAdd = false;
            } catch (Exception e) {
                throw new RacerRuntimeException(e);
            }
        }
    }

    public final void autoAddAxioms(String ontologyID) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_AUTOADDAXIOMS);
            writeS(ontologyID);
            writeS(reasonerID);
            writeCloseParln();
            resultParser.readNativeLine();
            isAdd = true;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public final void autoAddAxioms(OWLOntology ontology) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_AUTOADDAXIOMS);
            writeS(ontology.getOntologyID().getOntologyIRI());
            writeS(reasonerID);
            writeCloseParln();
            resultParser.readNativeLine();
            isAdd = true;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    /* public final void autoRemoveAxioms(OWLOntology ontology) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_AUTOADDAXIOMS);
            writeS(ontology.getURI());
            writeS(reasoner);
            writeCloseParln();
            resultParser.readNativeLine();
            isAdd = false;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }*/

    /**
     * Returns the racer version string.
     *
     * @return racer version string, e.g. 1.9.1
     */
    public Version getRacerVersionNumber() {
        String version = "";
        try {
            // System.out.println("sending getRacerNumber");
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.GET_RACER_VERSION);
            writeCloseParln();
            version = readFromSocket();
            //version = resultParser.readNativeLine();
            //

            if (version.startsWith(":answer")) {
                version = version.replace(":answer", "");
                int i = version.indexOf("\"");
                if (i > -1) {
                    int l = version.lastIndexOf("\"");
                    if (l <= i)
                        l = version.length();
                    version = version.substring(i, l);
                }
            }
            version = version.replace("\"", "");
            version = version.replace("\\", "");
            version = version.replace(" ", "");
            version = version.replace("\"", "");
            int major = 0, minor = 0, patch = 0, build = 0;
            final StringTokenizer tokenizer = new StringTokenizer(version, ".");
            if (tokenizer.hasMoreTokens())   {
                String s1 = tokenizer.nextToken();
                major = Integer.parseInt(s1);
            }
            if (tokenizer.hasMoreTokens()) {
                String s1 = tokenizer.nextToken();
                s1 = s1.trim();
                s1 = s1.replace("\"", "");
                minor = Integer.parseInt(s1);
            }
            if (tokenizer.hasMoreTokens()) {
                String s1 = tokenizer.nextToken();
                s1 = s1.trim();
                s1 = s1.replace("\"", "");
                build = Integer.parseInt(s1);
            }
            if (tokenizer.hasMoreTokens()) {
                String s1 = tokenizer.nextToken();
                s1 = s1.trim();
                s1 = s1.replace("\"", "");
                patch = Integer.parseInt(s1);
            }

            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.GET_RACER_BUILD_VERSION);
            writeCloseParln();
            version = resultParser.readNativeLine();
            if (version.startsWith(":answer")) {
                version = version.replace(":answer", "");
                int i = version.indexOf("\"");
                if (i > -1) {
                    int l = version.lastIndexOf("\"");
                    if (l <= i)
                        l = version.length();
                    version = version.substring(i, l);
                }
            }
            version = version.trim();
            version = version.replace("\"", "");
            version = version.replace("\\", "");
            version = version.replace(" ", "");
             version = version.replace("\"", "");
            build = Integer.parseInt(version.replace("-", ""));

            return new Version(major, minor, patch, build);
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

   /* public void unloadOntology(OWLOntology ontology) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_UNLOAD_ONTOLOGY);
            writer.write(ontology.getOntologyID().getOntologyIRI().toString());
            writeCloseParln();

        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }  */

    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // Utility methods for writing common stuff
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

    public void writeSequenceProlog() {
        writeOpenPar();
        writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_SEQUENCE);
        writeS(reasonerID);
    }

    public void writeSequenceEpilog() {
        writeCloseParln();
        try {
            resultParser.parseOWLAnswer();
        } catch (ParseException e) {
            throw new RacerRuntimeException(e);
        } catch (InconsistentOntologyException e) {

        }
    }

    /**
     * Writes an open paranthesis.
     * <blockquote>
     * The method is final so that the compile can introduce the method inline.
     * </blockquote>
     */
    public final void writeProlog() {
        writeOpenPar();
    }

    /**
     * Writes a newline character and parses the answer without returning the answer.
     * <blockquote>
     * The method is final: the compiler can introduce the method inline.
     * </blokquote>
     */
    public final void writeEpilog() {
        writer.println();
        writer.flush();
        try {
            //if notmemorysavingmode:
            //int i =   resultParser.parseIntegerResult();
            //else
            resultParser.parseOWLAnswer();
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }

    protected final void writeOpenPar() {
        writer.write(RacerOWLAPIVocabulary.OPEN_BRACKET);
    }

    protected final void writeClosePar() {
        writer.write(RacerOWLAPIVocabulary.CLOSE_BRACKET);
    }

    protected final void writeCloseParln() {
        writer.println(RacerOWLAPIVocabulary.CLOSE_BRACKET);
        writer.flush();
    }


    protected final void send() {
        writer.println();
        writer.flush();
        try {
            resultParser.parseOWLAnswer();
        } catch (ParseException e) {
            throw new RacerRuntimeException(e);
        } catch (InconsistentOntologyException e) {

        }
    }


    protected final void writeKRSSMethodName(final String commandKey) {
        writer.write(commandKey);
    }

    protected final void writeS(final String s) {
        writer.write(SPACE);
        writer.write(s);
    }

    protected final void writeSpace() {
        writer.write(SPACE);
    }

    protected final void writeS(final int i) {
        writer.write(SPACE);
        writer.write(Integer.toString(i));
    }


    protected final void writeS(final IRI uri) {
        writer.write(SPACE);
        writer.write(RacerOWLAPIVocabulary.DELIMITER_STRING);
        writer.write(uri.toString());
        writer.write(RacerOWLAPIVocabulary.DELIMITER_STRING);
    }

    protected final void writeAddAxiomProlog() {
        writeKRSSMethodName(OWLAPI_ADDAXIOM);
        writeS(this.ontologyID);
    }

    protected final void writeRemoveAxiomProlog() {
        writeKRSSMethodName(OWLAPI_REMOVEAXIOM);
        writeSpace();
        writeS(this.ontologyID);
    }

    protected final void writeDescriptions(final Set<OWLClassExpression> descs) throws IOException {
        writeOpenPar();
        for (OWLClassExpression desc : descs)
            renderer.write(desc);
        writeClosePar();
    }

    protected final void writeProperties(final Set<OWLPropertyExpression> descs) throws IOException {
        writeOpenPar();
        for (OWLPropertyExpression desc : descs)
            renderer.write(desc);
        writeClosePar();
    }

    /* protected void ensureToBeInReasoner(final String reasoner) throws RacerReasonerException {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_SETCURRENT_REASONER);
            writeS(reasoner);
            writeCloseParln();
            resultParser.parseOWLAnswer();
        } catch (Exception e) {
            throw new RacerReasonerException(e);
        }
    }

    protected void ensureToBeInTBox(final String racerTbox) throws RacerReasonerException {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.IN_TBOX);
            writeS(racerTbox);
            writeS(RacerOWLAPIVocabulary.INIT_ATTRIBUTE);
            writeS(RacerOWLAPIVocabulary.NIL);
            writeCloseParln();
            resultParser.parseOWLAnswer();
        } catch (Exception e) {
            throw new RacerReasonerException(e);
        }
    }*/

    /*  protected void ensureToBeInAbox(String racerTBox, String racerABox) throws RacerReasonerException {
            try {
                writeOpenPar();
                writeKRSSMethodName(RacerOWLAPIVocabulary.IN_ABOX);
                writeS(racerABox);
                writeS(racerTBox);
                writeCloseParln();
                resultParser.parseOWLAnswer();
            } catch (Exception e) {
                throw new RacerReasonerException(e);
            }
        }
    */
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // ReasonerBase implementation
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

    public void dispose() {
        disposeKnowledgeBase();
    }

    public void unloadOntologies(Set<OWLOntology> ontologies) {
        for (OWLOntology ontology : ontologies) {
            try {
                writeOpenPar();
                writeKRSSMethodName(OWLAPI_UNLOAD_ONTOLOGY);
                writeS(ontology.getOntologyID().getOntologyIRI());
                writeS(reasonerID);
                writeCloseParln();
                resultParser.parseOWLAnswer();
            } catch (ParseException e) {
                reportError(e);
            } catch (Exception e) {
                throw new RacerRuntimeException(e.getCause());
            }
        }
    }


    public void classify() {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_CLASSIFY);
            writeS(reasonerID);
            if (!configuration.isAboxConsistencyTestEnabled())
                writeS(RacerOWLAPIVocabulary.NIL);
            writeCloseParln();
            //resultParser.parseOWLAnswer();
            String  s = readFromSocket();
            //System.out.println("answer from classify " + s);
            //throw new ReasonerInterruptedException();
           // reportError(null);
            //resultParser.parseOWLAnswer();
        } catch (Exception e) {
            System.out.println(e);
            throw new ReasonerInterruptedException();
            //reportError(e);
        } /*catch (InconsistentOntologyException e) {
            throw new InconsistentOntologyException();
        }   */
    }

    public boolean isClassified() {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_ISCLASSIFIED);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        }
    }

    public boolean isRealised() {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_ISREALIZED);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        }
    }


    public void realise() {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_REALIZE);
            writeS(reasonerID);
            writeCloseParln();
            resultParser.parseOWLAnswer();
        } catch (ParseException e) {
            reportError(e);
        }
    }


    public final boolean isDefined(final OWLClass clazz) {
        try {
            if (clazz.isBuiltIn())
                return true;
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_ISDEFINED_CLASS);
            clazz.accept((OWLNamedObjectVisitor) renderer);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        }
    }

    public final boolean isDefined(final OWLObjectProperty prop) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_ISDEFINED_OBJECTPROPERTY);
            prop.accept((OWLNamedObjectVisitor) renderer);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        }
    }

    public final boolean isDefined(final OWLDataProperty prop) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_ISDEFINED_DATAPROPERTY);
            prop.accept((OWLNamedObjectVisitor) renderer);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        }
    }

    public final boolean isDefined(final OWLNamedIndividual indi) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_ISDEFINED_INDIVIDUAL);
            renderer.write(indi.asOWLNamedIndividual());
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // OWLClassReasoner implementation
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

    public boolean isSubClassOf(final OWLClassExpression clsC, final OWLClassExpression clsD) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_ISSUBCLASSOF);
            clsC.accept(renderer);
            clsD.accept(renderer);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        }
    }

    public NodeSet<OWLClass> getSubClasses(OWLClassExpression ce, boolean direct) {
        Set<Set<OWLClass>> classes;
        if (direct) {
            classes = getSubClasses(ce);
        } else
            classes = getDescendantClasses(ce);
        return convertToClassNodeSet(classes);
    }

    public NodeSet<OWLClass> getSuperClasses(OWLClassExpression ce, boolean direct) {
        Set<Set<OWLClass>> classes;
        if (direct) {
            classes = getSuperClasses(ce);
        } else
            classes = getAncestorClasses(ce);
        return convertToClassNodeSet(classes);
    }

    NodeSet<OWLClass> convertToClassNodeSet(Set<Set<OWLClass>> classes) {
        OWLClassNodeSet nodeSet = new OWLClassNodeSet();
        for (Set<OWLClass> set : classes) {
            OWLClassNode node = new OWLClassNode(set);
            nodeSet.addNode(node);
        }
        return nodeSet;
    }

    public Set<Set<OWLClass>> getSubClasses(final OWLClassExpression clsC) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GETSUBCLASSES);
            clsC.accept(renderer);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseConceptSoSResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    public Set<Set<OWLClass>> getSuperClasses(final OWLClassExpression clsC) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GETSUPERCLASSES);
            clsC.accept(renderer);
            writeS(reasonerID);
            writeCloseParln();
            Set<Set<OWLClass>> s = resultParser.parseConceptSoSResult();
            return s;

        } catch (ParseException e) {
            reportError(e);
            return null;
        }
    }

    public Node<OWLClass> getEquivalentClasses(final OWLClassExpression clsC) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_EQUIVALENT_CLASSES);
            clsC.accept(renderer);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseClassNodeResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        }
    }

    public Node<OWLClass> getInconsistentClasses() {
        return getEquivalentClasses(owlOntologyManager.getOWLDataFactory().getOWLNothing());
    }

    public NodeSet<OWLClass> getDisjointClasses(OWLClassExpression cls) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_DISJOINT_CLASSES);
            cls.accept(renderer);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseOWLClassNodeSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        }
    }

    public Set<Set<OWLClass>> getAncestorClasses(final OWLClassExpression clsC) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_ANCESTOR_CLASSES);
            clsC.accept(renderer);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseConceptSoSResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        }
    }

    public Set<Set<OWLClass>> getDescendantClasses(final OWLClassExpression clsC) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_DESCENDANT_CLASSES);
            clsC.accept(renderer);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseConceptSoSResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        }
    }

    public boolean isConsistent(final OWLClassExpression clsC) {
        return !getEquivalentClasses(clsC).contains(owlOntologyManager.getOWLDataFactory().getOWLNothing());
    }

    public boolean isEquivalentClass(final OWLClassExpression clsC, final OWLClassExpression clsD) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_ISEQUIVLANT_CLASS);
            clsC.accept(renderer);
            clsD.accept(renderer);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // OWLPropertyReasoner implementation
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

    public NodeSet<OWLDataProperty> getAncestorProperties(final OWLDataProperty property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_ANCESTOR_PROPERTIES);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseOWLDataPropertyNodeSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public NodeSet<OWLObjectPropertyExpression> getAncestorProperties(final OWLObjectPropertyExpression property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_ANCESTOR_PROPERTIES);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseObjectPropertyNodeSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public NodeSet<OWLDataProperty> getDescendantProperties(OWLDataProperty property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_DESCENDANT_PROPERTIES);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseOWLDataPropertyNodeSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public NodeSet<OWLObjectPropertyExpression> getDescendantProperties(OWLObjectPropertyExpression property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_DESCENDANT_PROPERTIES);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseObjectPropertyNodeSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    protected NodeSet<OWLObjectPropertyExpression> convertToObjectPropertiesNodeSet(Set<Set<OWLObjectPropertyExpression>> props) {
        OWLObjectPropertyNodeSet nodeSet = new OWLObjectPropertyNodeSet();
        for (Set<OWLObjectPropertyExpression> set : props) {
            OWLObjectPropertyNode node = new OWLObjectPropertyNode(set);
            nodeSet.addNode(node);
        }
        return nodeSet;
    }

    public NodeSet<OWLObjectPropertyExpression> getSuperObjectProperties(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (direct)
            return getSuperProperties(pe);
        else
            return getAncestorProperties(pe);
    }

    public NodeSet<OWLObjectPropertyExpression> getSubObjectProperties(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (direct)
            return getSubProperties(pe);
        else
            return getDescendantProperties(pe);
    }

    public NodeSet<OWLClass> getObjectPropertyDomains(OWLObjectPropertyExpression pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_DOMAINS);
            renderer.write(pe);
            writeS(reasonerID);
            writeS(3);
            writeCloseParln();
            return resultParser.parseOWLClassNodeSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }


    public Node<OWLDataProperty> getEquivalentProperties(OWLDataProperty property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_EQUIVALENT_PROPERTIES);
            renderer.write(property);
            writeS(reasonerID);
            writeS(NIL);
            writeCloseParln();
            return resultParser.parseOWLDataPropertyNodeResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public Node<OWLObjectPropertyExpression> getEquivalentObjectProperties(OWLObjectPropertyExpression property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_EQUIVALENT_PROPERTIES);
            renderer.write(property);
            writeS(reasonerID);
            writeS(NIL);
            writeCloseParln();
            return resultParser.parseObjectPropertyNodeResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public Node<OWLObjectPropertyExpression> getInverseObjectProperties(OWLObjectPropertyExpression pe) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_INVERSE_PROPERTIES);
            renderer.write(pe.getSimplified());
            writeS(reasonerID);
            writeCloseParln();
            NodeSet<OWLObjectPropertyExpression> nodeSet = resultParser.parseObjectPropertyNodeSetResult();
            if (nodeSet.getNodes().isEmpty()) return new OWLObjectPropertyNode();
            else return nodeSet.getNodes().iterator().next();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public Set<OWLDatatype> getRanges(OWLDataProperty property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_RANGES);
            renderer.write(property);
            writeS(reasonerID);
            writeS(2);
            writeCloseParln();
            return resultParser.parseDatatypeSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public NodeSet<OWLClass> getObjectPropertyRanges(OWLObjectPropertyExpression pe, boolean direct) {
        if (pe.isAnonymous()) throw new RacerRuntimeException("Anonymouse ObjectProperties are not supported");
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_RANGES);
            renderer.write(pe);
            writeS(reasonerID);
            if (!rangeDescriptions) {
                writeS(2);
                writeCloseParln();
                return resultParser.parseOWLClassNodeSetResult();
                //return OWLReasonerAdapter.flattenSetOfSets(resultParser.parseDescriptionSoSResult());

            } else {
                writeCloseParln();
                //return resultParser.parseConceptExpressionSetResult();
            }

        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
        return null;
    }

    public NodeSet<OWLDataProperty> getSubProperties(OWLDataProperty property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GETSUBPROPERTIES);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseOWLDataPropertyNodeSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public NodeSet<OWLObjectPropertyExpression> getSubProperties(OWLObjectPropertyExpression property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GETSUBPROPERTIES);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseObjectPropertyNodeSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public NodeSet<OWLDataProperty> getSuperDataProperties(OWLDataProperty pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (direct)
            return getSuperProperties(pe);
        else
            return getAncestorProperties(pe);
    }

    public NodeSet<OWLDataProperty> getSubDataProperties(OWLDataProperty pe, boolean direct) throws InconsistentOntologyException, FreshEntitiesException, ReasonerInterruptedException, TimeOutException {
        if (direct)
            return getSubProperties(pe);
        else
            return getDescendantProperties(pe);
    }


    public NodeSet<OWLDataProperty> getSuperProperties(OWLDataProperty property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GETSUPERPROPERTIES);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseOWLDataPropertyNodeSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (Exception e) {
            throw new RacerRuntimeException(e.getCause());
        }
    }

    public NodeSet<OWLObjectPropertyExpression> getSuperProperties(OWLObjectPropertyExpression property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GETSUPERPROPERTIES);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseObjectPropertyNodeSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public NodeSet<OWLObjectPropertyExpression> getDisjointObjectProperties(OWLObjectPropertyExpression pe) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_DISJOINT_OBJECTPROPERTIES);
            renderer.write(pe);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseObjectPropertyNodeSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public NodeSet<OWLDataProperty> getDisjointDataProperties(OWLDataPropertyExpression pe) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_DISJOINT_DATAPROPERTIES);
            renderer.write(pe);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseOWLDataPropertyNodeSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public boolean isAntiSymmetric(OWLObjectProperty property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_IS_ANTISYMMETRIC);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public boolean isFunctional(OWLDataProperty property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_IS_FUNCTIONAL);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public boolean isFunctional(OWLObjectProperty property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_IS_FUNCTIONAL);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public boolean isInverseFunctional(OWLObjectProperty property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_IS_INVERSEFUNCTIONAL);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public boolean isIrreflexive(OWLObjectProperty property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_IS_IRREFLEXIVE);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public boolean isReflexive(OWLObjectProperty property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_IS_REFLEXIVE);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public boolean isSymmetric(OWLObjectProperty property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_IS_SYMMETRIC);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }

    }

    public boolean isTransitive(OWLObjectProperty property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_IS_TRANSITIVE);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }

    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // OWLIndividualPropertyReasoner implementation
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

    public NodeSet<OWLClass> getTypes(OWLNamedIndividual individual, boolean direct) {

        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GETTYPES);
            renderer.write(individual);
            if (direct)
                writeS(TRUE);
            else
                writeS(NIL);
            writeS(reasonerID);
            writeCloseParln();
            //readFromSocket();
            return resultParser.parseOWLClassNodeSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        } catch (Exception e) {
            throw new RacerRuntimeException(e.getCause());
        }
    }

    public boolean hasType(OWLIndividual individual, OWLClassExpression desc, boolean direct) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_HASTYPE);
            renderer.write(individual.asOWLNamedIndividual());
            renderer.write(desc);
            if (direct)
                writeS(TRUE);
            else
                writeS(NIL);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }

    }

    public NodeSet<OWLNamedIndividual> getIndividuals(OWLClassExpression clsC, boolean direct) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_INSTANCES);
            renderer.write(clsC);
            if (direct)
                writeS(TRUE);
            else
                writeS(NIL);
            writeS(reasonerID);
            if (configuration.getIndividualNodeSetPolicy() == IndividualNodeSetPolicy.BY_NAME) {
                writeS(NIL);
            } else {
                writeS(TRUE);
            }
            writeCloseParln();
            return resultParser.parseOWLNamedIndividualNodeSetResult();

            /**      writeOpenPar();
             writeKRSSMethodName(OWLAPI_GET_INDIVIDIDUALS);
             renderer.write(clsC);
             if (direct)
             writeS(TRUE);
             else
             writeS(NIL);
             writeS(reasonerID);
             if (configuration.getIndividualNodeSetPolicy() == IndividualNodeSetPolicy.BY_NAME) {
             writeS(NIL);
             writeCloseParln();
             OWLNamedIndividualNodeSet nodeSet = new OWLNamedIndividualNodeSet();
             nodeSet.addDifferentEntities(resultParser.parseOWLNamedIndividualSetResult());
             return nodeSet;
             } else {
             writeS(TRUE);
             writeCloseParln();
             OWLNamedIndividualNodeSet nodeSet = new OWLNamedIndividualNodeSet();

             Node<OWLNamedIndividual> indis = resultParser.parseOWLNamedIndividualNodeResult();
             for (OWLNamedIndividual indi : indis) {
             nodeSet.addNode(getSameIndividuals(indi));
             }
             return nodeSet;

             //return resultParser.parseOWLNamedIndividualNodeSetResult();
             }     */
/*
            writeCloseParln();
            Node<OWLNamedIndividual> indis = resultParser.parseOWLNamedIndividualNodeResult();
            switch (configuration.getIndividualNodeSetPolicy()) {
                case BY_NAME:
                    nodeSet.addDifferentEntities(indis.getEntities());
                    break;
                case BY_SAME_AS:
                    for (OWLNamedIndividual indi : indis) {
                        nodeSet.addNode(getSameIndividuals(indi));
                    }
            }
            return nodeSet;     */
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        } catch (Error e) {
            reportError(null);
            return null;
        }
    }

    public NodeSet<OWLNamedIndividual> getRelatedIndividuals(OWLNamedIndividual subject, OWLObjectPropertyExpression property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_RELATEDINDIVIDUALS);
            renderer.write(subject);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            Node<OWLNamedIndividual> indis = resultParser.parseOWLNamedIndividualNodeResult();
            OWLNamedIndividualNodeSet nodeSet = new OWLNamedIndividualNodeSet();
            switch (configuration.getIndividualNodeSetPolicy()) {
                case BY_NAME:
                    nodeSet.addDifferentEntities(indis.getEntities());
                    break;
                case BY_SAME_AS:
                    for (OWLNamedIndividual indi : indis) {
                        nodeSet.addNode(getSameIndividuals(indi));
                    }
            }
            return nodeSet;
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public NodeSet<OWLNamedIndividual> getRelatedIndividuals(OWLNamedIndividual subject, OWLObjectPropertyExpression property, boolean direct) {
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_RELATEDINDIVIDUALS);
            renderer.write(subject);
            renderer.write(property);
            if (direct)
                writeS(TRUE);
            else
                writeS(NIL);
            writeS(reasonerID);
            writeCloseParln();
            Node<OWLNamedIndividual> indis = resultParser.parseOWLNamedIndividualNodeResult();
            OWLNamedIndividualNodeSet nodeSet = new OWLNamedIndividualNodeSet();
            switch (configuration.getIndividualNodeSetPolicy()) {
                case BY_NAME:
                    nodeSet.addDifferentEntities(indis.getEntities());
                    break;
                case BY_SAME_AS:
                    for (OWLNamedIndividual indi : indis) {
                        nodeSet.addNode(getSameIndividuals(indi));
                    }
            }
            return nodeSet;
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public Node<OWLNamedIndividual> getSameIndividuals(OWLNamedIndividual individual) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_SAMEINDIVIDUALS);
            renderer.write(individual);
            writeS(reasonerID);
            writeCloseParln();
            OWLNamedIndividualNode node = (OWLNamedIndividualNode) resultParser.parseOWLNamedIndividualNodeResult();
            if (!node.contains(individual)) {
                node.add(individual);
            }
            return node;
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public boolean isSameIndividual(OWLNamedIndividual indi1, OWLNamedIndividual indi2) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_IS_SAMEINDIVIDUAL);
            renderer.write(indi1);
            renderer.write(indi2);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public NodeSet<OWLNamedIndividual> getDifferentIndividuals(OWLNamedIndividual individual) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_DIFFERENT_INDIVIDUALS);
            renderer.write(individual);
            writeS(reasonerID);
            writeCloseParln();
            OWLNamedIndividualNode node = (OWLNamedIndividualNode) resultParser.parseOWLNamedIndividualNodeResult();
            OWLNamedIndividualNodeSet nodeSet = new OWLNamedIndividualNodeSet();
            switch (configuration.getIndividualNodeSetPolicy()) {
                case BY_NAME:
                    nodeSet.addDifferentEntities(node.getEntities());
                    break;
                case BY_SAME_AS:
                    for (OWLNamedIndividual indi : node) {
                        nodeSet.addNode(getSameIndividuals(indi));
                    }
            }
            return nodeSet;
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    /*  public Set<OWLObjectProperty> getRelatedObjectPropertiesForIndividual(OWLIndividual individual) throws RacerReasonerException {
            try {
                writeOpenPar();
                writeKRSSMethodName(RacerKRSSVocabulary.DESCRIBE_INDIVIDUAL);
                renderer.write(individual);
                writeS(aboxName);
                writeCloseParln();
                return resultParser.parseRelatedRoleFillersForIndividualViaDescribeResult();
            } catch (Exception e) {
                throw new RacerReasonerException(e);
            }
        }
    */

    public Map<OWLObjectProperty, Set<OWLIndividual>> getObjectPropertyRelationships(OWLNamedIndividual individual) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GETOBJECTPROPERTYRELATIONSHIPS);
            renderer.write(individual);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseObjectPropertyFillerMapResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public boolean hasObjectPropertyRelationship(OWLIndividual subject, OWLObjectPropertyExpression property, OWLIndividual object) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_HASOBJECTPROPERTYRELATIONSHIP);
            renderer.write(subject);
            renderer.write(property);
            renderer.write(object);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
    }

    public Set<OWLLiteral> getRelatedValues(OWLNamedIndividual subject, OWLDataPropertyExpression property) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_RELATEDVALUES);
            renderer.write(subject);
            renderer.write(property);
            writeS(reasonerID);
            writeCloseParln();
            // readFromSocket2();
            return resultParser.parseConstantSetResult();
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        } catch (Exception e) {
            throw new RacerRuntimeException(e.getCause());
        }
    }

    public final Map<OWLDataProperty, Set<OWLLiteral>> getDataPropertyRelationships(OWLNamedIndividual individual) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_DATAPROPERTYRELATIONSHIPS);
            renderer.write(individual);
            writeS(reasonerID);
            writeCloseParln();
            return resultParser.parseDataPropertyValueMapResult();

        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        } catch (Exception e) {
            throw new RacerRuntimeException(e.getCause());
        }
    }


    public final NodeSet<OWLNamedIndividual> getObjectPropertyValues(OWLNamedIndividual ind, OWLObjectPropertyExpression pe) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_OBJECTPROPERTY_VALUES);
            renderer.write(ind);
            renderer.write(pe);
            writeS(reasonerID);
            if (configuration.getIndividualNodeSetPolicy() == IndividualNodeSetPolicy.BY_NAME) {
                writeS(NIL);
                writeCloseParln();
                return resultParser.parseOWLNamedIndividualNodeSetResult();
            } else {
                writeS(TRUE);
                writeCloseParln();
                return resultParser.parseOWLNamedIndividualNodeSetResult();
            }
        } catch (ParseException e) {
            reportError(e);
            return null;
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        } catch (Exception e) {
            throw new RacerRuntimeException(e.getCause());
        }
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // AxiomVisitor implementation
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////


    protected void handleChanges(Collection<OWLAxiom> addAxioms, Collection<OWLAxiom> removeAxioms) {
        if (addAxioms.size() > 0) {
            //autoAddAxioms(this.ontologyID);
            autoBatchAddAxioms();
            isAdd = true;
            for (OWLAxiom axiom : addAxioms) {
                axiom.accept(commandVisitor);
                writer.flush();
            }
            //if (this.autoBatchSynchronizeNeeded)
            batchSynchronize();
            //writeSequenceEpilog();
        }
        if (removeAxioms.size() > 0) {
            autoBatchRemoveAxioms();
            isAdd = false;
            for (OWLAxiom axiom : removeAxioms) {
                axiom.accept(commandVisitor);
            }
            batchSynchronize();
        }
    }

    protected void handleChanges(Collection<OWLAxiom> addAxioms, Collection<OWLAxiom> removeAxioms, ReasonerProgressMonitor monitor) {
        // long time = System.currentTimeMillis();
        int size = addAxioms.size() + removeAxioms.size();
        if (size > 0) {
            try {
                int i = 0;
                if (addAxioms.size() > 0) {
                    autoBatchAddAxioms();
                    isAdd = true;
                    for (OWLAxiom axiom : addAxioms) {
                        if (isInterrupted) {
                            //System.out.println("Interrupted!");
                            //disposing etc. is the task of the application, etc. Protege!
                            //it does not work because Protege does not handle this properly.
                            return;
                        }
                        axiom.accept(commandVisitor);
                        i++;

                    }
                    batchSynchronize();
                }
                if (removeAxioms.size() > 0) {
                    autoBatchRemoveAxioms();
                    isAdd = false;
                    for (OWLAxiom axiom : removeAxioms) {
                        axiom.accept(commandVisitor);
                        i++;
                    }
                    batchSynchronize();
                }
            } finally {
            }
        }
        // long time_end = System.currentTimeMillis();
        //System.out.println( "Time " + (time_end - time) );
    }

    protected void handleChanges_old(Collection<OWLAxiom> addAxioms, Collection<OWLAxiom> removeAxioms, ReasonerProgressMonitor monitor) {
        int size = addAxioms.size() + removeAxioms.size();
        if (size > 0) {
            try {
                monitor.reasonerTaskStarted("Update Reasoner Axioms...");
                int i = 0;
                if (addAxioms.size() > 0) {
                    autoAddAxioms(this.ontologyID);
                    writeSequenceProlog();
                    isAdd = true;
                    for (OWLAxiom axiom : addAxioms) {
                        axiom.accept(this);
                        i++;
                        monitor.reasonerTaskProgressChanged(i, size);
                    }
                    if (this.autoBatchSynchronizeNeeded)
                        batchSynchronize();
                    writeSequenceEpilog();
                }
                if (removeAxioms.size() > 0) {
                    autoRemoveAxioms(this.ontologyID, false);
                    writeSequenceProlog();
                    isAdd = false;
                    for (OWLAxiom axiom : removeAxioms) {
                        axiom.accept(this);
                        i++;
                        monitor.reasonerTaskProgressChanged(i, size);
                    }
                    if (this.autoBatchSynchronizeNeeded)
                        batchSynchronize();
                    writeSequenceEpilog();
                }
            } finally {
                monitor.reasonerTaskStopped();
            }
        }
    }


    public void applyChanges(List<OWLOntologyChange> changes) {
        /*if (changes.size() > 0) {
            autoAddAxioms(currentOntology);
            writeSequenceProlog();
            for (OWLOntologyChange change : changes) {
                change.accept(this);
            }
            if (this.autoBatchSynchronizeNeeded)
                batchSynchronize(this.currentOntology);
            writeSequenceEpilog();

        } */
    }


    protected final void applyChanges() {
/*
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_APPLY_CHANGES);
            writeS(reasoner);
            writeCloseParln();
            resultParser.parseOWLAnswer();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
*/
    }

    public void visit(OWLSubClassOfAxiom axiom) {
        try {
            if (isAdd) {
                writeOpenPar();
                writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_OWLSUBCLASS_AXIOM);
                renderer.write(axiom.getSubClass());
                renderer.write(axiom.getSuperClass());
                //       writeS(reasonerID);
                //       writeS(reasonerID);
                writeClosePar();
            } else {
                writeOpenPar();
                writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_OWLSUBCLASS_AXIOM);
                renderer.write(axiom.getSubClass());
                renderer.write(axiom.getSuperClass());
                //  writeS(reasonerID);
                writeClosePar();
            }
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLEquivalentClassesAxiom axiom) {
        try {
            if (isAdd) {
                writeOpenPar();
                writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_EQUIVALENTCLASSES_AXIOM);
                writeSpace();
                writeOpenPar();
                for (OWLClassExpression desc : axiom.getClassExpressions()) {
                    renderer.write(desc);
                }
                writeClosePar();
                //  writeS(reasonerID);
                writeClosePar();
            } else {
                writeOpenPar();
                writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_EQUIVALENTCLASSES_AXIOM);
                writeSpace();
                writeOpenPar();
                for (OWLClassExpression desc : axiom.getClassExpressions()) {
                    renderer.write(desc);
                }
                writeClosePar();
                // writeS(reasonerID);
                writeClosePar();
            }
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLDisjointClassesAxiom axiom) {
        try {
            if (isAdd) {
                writeOpenPar();
                writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_DISJOINTCLASSES_AXIOM);
                writeOpenPar();
                for (OWLClassExpression desc : axiom.getClassExpressions()) {
                    renderer.write(desc);
                }
                writeClosePar();
                //  writeS(reasonerID);
                writeClosePar();
            } else {
                writeOpenPar();
                writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_DISJOINTCLASSES_AXIOM);
                writeOpenPar();
                for (OWLClassExpression desc : axiom.getClassExpressions()) {
                    renderer.write(desc);
                }
                writeClosePar();
                // writeS(reasonerID);
                writeClosePar();
            }

        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLDisjointUnionAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_DISJOINTUNTION_AXIOM);
            renderer.write(axiom.getOWLClass());
            writeOpenPar();
            for (OWLClassExpression desc : axiom.getClassExpressions()) {
                renderer.write(desc);
            }
            writeClosePar();
            //writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

///////////////////////////////////////////
//
//  Properties
//
///////////////////////////////////////////

    @Override
    public void visit(OWLSubObjectPropertyOfAxiom axiom) {
        try {
            if (isAdd) {
                writeOpenPar();
                writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_OWLOBJECTSUBPROPERTY_AXIOM);
                renderer.write(axiom.getSubProperty());
                renderer.write(axiom.getSuperProperty());
                //   writeS(reasonerID);
                writeClosePar();
            } else {
                writeOpenPar();
                writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_GET_OWLOBJECTSUBPROPERTY_AXIOM);
                renderer.write(axiom.getSubProperty());
                renderer.write(axiom.getSuperProperty());
                //    writeS(reasonerID);
                writeClosePar();
            }
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLEquivalentObjectPropertiesAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_EQUIVALENTOBJECTPROPERTIES_AXIOM);
            writeOpenPar();
            for (OWLObjectPropertyExpression exp : axiom.getProperties())
                renderer.write(exp);
            writeClosePar();
            //   writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }


    @Override
    public void visit(OWLObjectPropertyDomainAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_OWLOBJECTPROPERTYDOMAIN_AXIOM);
            renderer.write(axiom.getProperty());
            renderer.write(axiom.getDomain());
            //   writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    @Override
    public void visit(OWLTransitiveObjectPropertyAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_OWLTRANSITIVEOBJECTPROPERTY_AXIOM);
            renderer.write(axiom.getProperty());
            //   writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLSymmetricObjectPropertyAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_OWLSYMMETRICOBJECTPROPERTY_AXIOM);
            renderer.write(axiom.getProperty());
            //  writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }


    @Override
    public void visit(OWLInverseObjectPropertiesAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_OWLINVERSEOBJECTPROPERTIES_AXIOM);
            renderer.write(axiom.getFirstProperty());
            renderer.write(axiom.getSecondProperty());
            //  writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }


    @Override
    public void visit(OWLObjectPropertyRangeAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_OWLOBJECTPROPERTYRANGE_AXIOM);
            renderer.write(axiom.getProperty());
            renderer.write(axiom.getRange());
            //  writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    @Override
    public void visit(OWLAsymmetricObjectPropertyAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_ASYMMETRICPROPERTY_AXIOM);
            renderer.write(axiom.getProperty());
            //  writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }


    public void visit(OWLReflexiveObjectPropertyAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_REFLEXIVEPROPERTY_AXIOM);
            renderer.write(axiom.getProperty());
            //  writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLDisjointObjectPropertiesAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_DISJOINTOBJECTPROPERTY_AXIOM);
            writeOpenPar();
            for (OWLObjectPropertyExpression exp : axiom.getProperties())
                renderer.write(exp);
            writeClosePar();
            //  writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLFunctionalObjectPropertyAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_FUNCTIONALOBJECTPROPERTY_AXIOM);
            renderer.write(axiom.getProperty());
            //    writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLIrreflexiveObjectPropertyAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPPI_GET_IRREFLEXIVEOBJECTPROPERTY_AXIOM);
            renderer.write(axiom.getProperty());
            //    writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLInverseFunctionalObjectPropertyAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_INVERSEFUNCTIONALOBJECTPROPERTY_AXIOM);
            renderer.write(axiom.getProperty());
            //    writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    @Override
    public void visit(OWLSubPropertyChainOfAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_OBJECTPROPERTYCHAINSUBPROPERTY_AXIOM);
            writeOpenPar();
            for (OWLObjectPropertyExpression exp : axiom.getPropertyChain())
                renderer.write(exp);
            writeClosePar();
            renderer.write(axiom.getSuperProperty());
            //   writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }


    @Override
    public void visit(OWLSubDataPropertyOfAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_OWLDATASUBPROPERTY_AXIOM);
            renderer.write(axiom.getSubProperty());
            renderer.write(axiom.getSuperProperty());
            //    writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    @Override
    public void visit(OWLDataPropertyRangeAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_OWLDATAPROPERTYRANGE_AXIOM);
            renderer.write(axiom.getProperty());
            axiom.getRange().accept(renderer);
            //   writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }

    }

    public void visit(OWLDataPropertyDomainAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_DATAPROPERTYDOMAIN_AXIOM);
            renderer.write(axiom.getProperty());
            renderer.write(axiom.getDomain());
            //    writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLDisjointDataPropertiesAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_DISJOINTDATAPROPERTY_AXIOM);
            writeOpenPar();
            for (OWLPropertyExpression prop : axiom.getProperties())
                renderer.write(prop);
            writeClosePar();
            //    writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLFunctionalDataPropertyAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_FUNCTIONALDATAPROPERTY_AXIOM);
            renderer.write(axiom.getProperty());
            //     writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLEquivalentDataPropertiesAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_EQUIVALENTDATAPROPERTY_AXIOM);
            writeOpenPar();
            for (OWLDataPropertyExpression exp : axiom.getProperties())
                renderer.write(exp);
            writeClosePar();
            //  writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

///////////////////////////////////////////
//
//  Individuals
//
///////////////////////////////////////////

    @Override
    public void visit(OWLClassAssertionAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_CLASSASSERTION_AXIOM);
            renderer.write(axiom.getIndividual());
            renderer.write(axiom.getClassExpression());
            // writeS(reasonerID);
            writeClosePar();

        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    @Override
    public void visit(OWLObjectPropertyAssertionAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_OBJECTPROPERTYASSERTION_AXIOM);
            renderer.write(axiom.getSubject());
            renderer.write(axiom.getProperty());
            renderer.write(axiom.getObject());
            // writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }


    public void visit(OWLNegativeObjectPropertyAssertionAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_NEGATIVEOBJECTPROPERTYASSERTION_AXIOM);
            renderer.write(axiom.getSubject());
            renderer.write(axiom.getProperty());
            renderer.write(axiom.getObject());
            // writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLSameIndividualAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_SAMEINDIVIDUALAS_AXIOM);
            writeOpenPar();
            for (OWLIndividual indi : axiom.getIndividuals())
                renderer.write(indi);
            writeClosePar();
            // writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLDifferentIndividualsAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_DIFFERENTINDIVIDUALS_AXIOM);
            writeOpenPar();
            for (OWLIndividual indi : axiom.getIndividuals())
                renderer.write(indi);
            writeClosePar();
            // writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }


    @Override
    public void visit(OWLDataPropertyAssertionAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_OWLDATAPROPERTYASSERTION_AXIOM);
            renderer.write(axiom.getSubject());
            renderer.write(axiom.getProperty());
            axiom.getObject().accept(renderer);
            // writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public void visit(OWLNegativeDataPropertyAssertionAxiom axiom) {
        try {
            writeOpenPar();
            writeKRSSMethodName(OWLAPI_GET_OWLNEGATIVEPDATAROPERTYASSERTION_AXIOM);
            renderer.write(axiom.getSubject());
            renderer.write(axiom.getProperty());
            axiom.getObject().accept(renderer);
            // writeS(reasonerID);
            writeClosePar();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Declaration
//

    class BuiltInEntityDeclaration extends RuntimeException {
        OWLEntity entity;

        public BuiltInEntityDeclaration(OWLEntity entity) {
            this.entity = entity;
        }

        public OWLEntity getEntity() {
            return entity;
        }
    }

    public void visit(OWLDeclarationAxiom axiom) {
        /*if (axiom.getEntity().isBottomEntity() || axiom.getEntity().isTopEntity()) {
            throw new BuiltInEntityDeclaration(axiom.getEntity());
        } */
        try {
            if (axiom.getEntity() instanceof OWLObjectProperty) {
                writeOpenPar();
                writeKRSSMethodName(OWLAPI_GET_DECLARATION_AXIOM);
                writeSpace();
                writeOpenPar();
                writeKRSSMethodName(OBJECTPROPERTY);
                renderer.write((OWLObjectProperty) axiom.getEntity());
                writeClosePar();
                //     writeS(reasonerID);
                writeClosePar();
            } else if (axiom.getEntity() instanceof OWLDataProperty) {
                writeOpenPar();
                writeKRSSMethodName(OWLAPI_GET_DECLARATION_AXIOM);
                writeSpace();
                writeOpenPar();
                writeKRSSMethodName(DATAPROPERTY);
                renderer.write((OWLDataProperty) axiom.getEntity());
                writeClosePar();
                //      writeS(reasonerID);
                writeClosePar();
            } else if (axiom.getEntity() instanceof OWLClass) {
                writeOpenPar();
                writeKRSSMethodName(OWLAPI_GET_DECLARATION_AXIOM);
                writeSpace();
                writeOpenPar();
                writeKRSSMethodName(OWLCLASS);
                renderer.write((OWLClass) axiom.getEntity());
                writeClosePar();
                //     writeS(reasonerID);
                writeClosePar();
            } else if (axiom.getEntity() instanceof OWLIndividual) {
                writeOpenPar();
                writeKRSSMethodName(OWLAPI_GET_DECLARATION_AXIOM);
                writeSpace();
                writeOpenPar();
                writeKRSSMethodName(INDIVIDUAL);
                renderer.write((OWLIndividual) axiom.getEntity());
                writeClosePar();
                //     writeS(reasonerID);
                writeClosePar();
            } else if (axiom.getEntity() instanceof OWLAnnotationProperty) {
                writeOpenPar();
                writeKRSSMethodName(OWLAPI_GET_DECLARATION_AXIOM);
                writeSpace();
                writeOpenPar();
                writeKRSSMethodName(ANNOTATIONPROPERTY);
                renderer.write((OWLAnnotationProperty) axiom.getEntity());
                writeClosePar();
                writeClosePar();
            } else {
                //throw new BuiltInEntityDeclaration(axiom.getEntity());
            }
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Annotations
//
////////////////////////////////////////////////////////////////////////////////////////////////////////
    /*
@Override
public void visit(OWLEntityAnnotationAxiom axiom) {
    if (ignoreAnnotationAxioms) return;
    try {
        writeOpenPar();
        writeKRSSMethodName(OWLAPI_GET_ENTITYANNOTATION_AXIOM);
        axiom.getSubject().accept(noaRenderer);
        axiom.getAnnotation().accept(renderer);
        writeS(reasoner);
        writeClosePar();
    } catch (Exception e) {
        throw new RacerRuntimeException(e);
    }
}

public void visit(OWLAxiomAnnotationAxiom axiom) {
}

public void visit(OWLOntologyAnnotationAxiom axiom) {
}    */


    protected class NamedObjectForAnnotationRenderer implements OWLNamedObjectVisitor {

        ShortFormProvider shortFormProvider;
        Writer writer;

        public NamedObjectForAnnotationRenderer(Writer writer, ShortFormProvider sfp) {
            this.writer = writer;
            this.shortFormProvider = sfp;
        }

        public final void visit(OWLClass owlClass) {
            try {
                writeSpace();
                writer.write(RacerOWLAPIVocabulary.OPEN_BRACKET);
                writer.write(RacerOWLAPIVocabulary.OWLCLASS);
                writeSpace();
                writer.write(shortFormProvider.shortForm(owlClass.getIRI()));
                writer.write(RacerOWLAPIVocabulary.CLOSE_BRACKET);
            } catch (IOException e) {
                throw new RacerRuntimeException(e);
            }
        }

        public final void visit(OWLObjectProperty property) {
            try {
                writeSpace();
                writer.write(RacerOWLAPIVocabulary.OPEN_BRACKET);
                writer.write(RacerOWLAPIVocabulary.OBJECTPROPERTY);
                writeSpace();
                writer.write(shortFormProvider.shortForm(property.getIRI()));
                writer.write(RacerOWLAPIVocabulary.CLOSE_BRACKET);
            } catch (IOException e) {
                throw new RacerRuntimeException(e);
            }
        }

        public final void visit(OWLDataProperty property) {
            try {
                writeSpace();
                writer.write(RacerOWLAPIVocabulary.OPEN_BRACKET);
                writer.write(RacerOWLAPIVocabulary.DATAPROPERTY);
                writeSpace();
                writer.write(shortFormProvider.shortForm(property.getIRI()));
                writer.write(RacerOWLAPIVocabulary.CLOSE_BRACKET);
            } catch (IOException e) {
                throw new RacerRuntimeException(e);
            }
        }

        public final void visit(OWLIndividual owlIndividual) {
            try {
                writer.write(RacerOWLAPIVocabulary.OPEN_BRACKET);
                writer.write(RacerOWLAPIVocabulary.INDIVIDUAL);
                writeSpace();
                if (owlIndividual.isAnonymous())
                    writer.write(owlIndividual.asOWLAnonymousIndividual().getID().toString());
                else
                    writer.write(shortFormProvider.shortForm(owlIndividual.asOWLNamedIndividual().getIRI()));
                writer.write(RacerOWLAPIVocabulary.CLOSE_BRACKET);
            } catch (IOException e) {
                throw new RacerRuntimeException(e);
            }
        }

        public void visit(OWLAnnotationProperty property) {
        }

        public void visit(OWLNamedIndividual owlIndividual) {
            try {
                writer.write(RacerOWLAPIVocabulary.OPEN_BRACKET);
                writer.write(RacerOWLAPIVocabulary.INDIVIDUAL);
                writeSpace();
                writer.write(shortFormProvider.shortForm(owlIndividual.asOWLNamedIndividual().getIRI()));
                writer.write(RacerOWLAPIVocabulary.CLOSE_BRACKET);
            } catch (IOException e) {
                throw new RacerRuntimeException(e);
            }
        }

        public void visit(OWLOntology ontology) {
        }

        public void visit(OWLDatatype dataType) {
        }
    }
    ////////////////////////////////////////////////////////////////////////////////////////////////////////
//
    // Entailments access
//
////////////////////////////////////////////////////////////////////////////////////////////////////////
    EntailmentChecker entailmentChecker = new EntailmentChecker();

    public boolean isEntailed(OWLAxiom axiom) throws ReasonerInterruptedException, UnsupportedEntailmentTypeException, TimeOutException, AxiomNotInProfileException, FreshEntitiesException {
//        autoAddAxioms(reasonerID);
        try {
            writeOpenPar();
            writer.write(RacerOWLAPIVocabulary.OWLAPI_ISENTAILED);
            writeSpace();
            axiom.accept(this);
            // writeS(reasonerID);
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        }
        //return axiom.accept(entailmentChecker);
    }

    public boolean isEntailed(Set<? extends OWLAxiom> axioms) throws ReasonerInterruptedException, UnsupportedEntailmentTypeException, TimeOutException, AxiomNotInProfileException, FreshEntitiesException {
        for (OWLAxiom axiom : axioms) {
            if (!isEntailed(axiom)) return false;
        }
        return true;
    }

    public boolean isEntailmentCheckingSupported(AxiomType<?> axiomType) {
        return entailmentChecker.isEntailmentCheckingSupported(axiomType);
    }

    class EntailmentChecker implements OWLAxiomVisitorEx<Boolean> {
        Set<AxiomType> supportedAxiomsTypes;

        public EntailmentChecker() {
            this.supportedAxiomsTypes = new HashSet<AxiomType>();
            this.supportedAxiomsTypes.add(AxiomType.SUBCLASS_OF);
            this.supportedAxiomsTypes.add(AxiomType.ASYMMETRIC_OBJECT_PROPERTY);
            this.supportedAxiomsTypes.add(AxiomType.REFLEXIVE_OBJECT_PROPERTY);
            this.supportedAxiomsTypes.add(AxiomType.FUNCTIONAL_OBJECT_PROPERTY);
            this.supportedAxiomsTypes.add(AxiomType.IRREFLEXIVE_OBJECT_PROPERTY);
            this.supportedAxiomsTypes.add(AxiomType.CLASS_ASSERTION);
            this.supportedAxiomsTypes.add(AxiomType.EQUIVALENT_CLASSES);

        }

        public boolean isEntailmentCheckingSupported(AxiomType<?> axiomType) {
            return supportedAxiomsTypes.contains(axiomType);
        }

        public Boolean visit(OWLSubClassOfAxiom axiom) {
            return isSubClassOf(axiom.getSubClass(), axiom.getSuperClass());
        }

        public Boolean visit(OWLNegativeObjectPropertyAssertionAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLAsymmetricObjectPropertyAxiom axiom) {
            return isAntiSymmetric(axiom.getProperty().asOWLObjectProperty());
        }

        public Boolean visit(OWLReflexiveObjectPropertyAxiom axiom) {
            return isReflexive(axiom.getProperty().asOWLObjectProperty());
        }

        public Boolean visit(OWLDisjointClassesAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLDataPropertyDomainAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLObjectPropertyDomainAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLEquivalentObjectPropertiesAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLNegativeDataPropertyAssertionAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLDifferentIndividualsAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLDisjointDataPropertiesAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLDisjointObjectPropertiesAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLObjectPropertyRangeAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLObjectPropertyAssertionAxiom axiom) {
            return hasObjectPropertyRelationship(axiom.getSubject(), axiom.getProperty(), axiom.getObject());
        }

        public Boolean visit(OWLFunctionalObjectPropertyAxiom axiom) {
            return isFunctional(axiom.getProperty().asOWLObjectProperty());
        }

        public Boolean visit(OWLSubObjectPropertyOfAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLDisjointUnionAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLDeclarationAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLAnnotationAssertionAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLSymmetricObjectPropertyAxiom axiom) {
            return isSymmetric(axiom.getProperty().asOWLObjectProperty());
        }

        public Boolean visit(OWLDataPropertyRangeAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLFunctionalDataPropertyAxiom axiom) {
            return isFunctional(axiom.getProperty().asOWLDataProperty());
        }

        public Boolean visit(OWLEquivalentDataPropertiesAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLClassAssertionAxiom axiom) {
            return hasType(axiom.getIndividual(), axiom.getClassExpression(), false);
        }

        public Boolean visit(OWLEquivalentClassesAxiom axiom) {
            Vector<OWLClassExpression> expressions = new Vector<OWLClassExpression>();
            expressions.addAll(axiom.getClassExpressions());
            for (int i = 0; i < expressions.size(); i++) {
                for (int j = i; j < expressions.size(); j++) {
                    if (!isEquivalentClass(expressions.get(i), expressions.get(j))) return false;
                }
            }
            return true;

        }

        public Boolean visit(OWLDataPropertyAssertionAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLTransitiveObjectPropertyAxiom axiom) {
            return isTransitive(axiom.getProperty().asOWLObjectProperty());
        }

        public Boolean visit(OWLIrreflexiveObjectPropertyAxiom axiom) {
            return isIrreflexive(axiom.getProperty().asOWLObjectProperty());
        }

        public Boolean visit(OWLSubDataPropertyOfAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLInverseFunctionalObjectPropertyAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLSameIndividualAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLSubPropertyChainOfAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLInverseObjectPropertiesAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLHasKeyAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLDatatypeDefinitionAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(SWRLRule rule) {
            return false;
        }

        public Boolean visit(OWLSubAnnotationPropertyOfAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLAnnotationPropertyDomainAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }

        public Boolean visit(OWLAnnotationPropertyRangeAxiom axiom) {
            throw new UnsupportedEntailmentTypeException(axiom);
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Native access
//
////////////////////////////////////////////////////////////////////////////////////////////////////////

    public String sendNativeCommand(String command, final boolean inKB) {
        try {
            if (inKB) {
                writeOpenPar();
                writer.print(IN_KNOWLEDGEBASE);
                writeS(reasonerID);
                writeS(INIT_ATTRIBUTE);
                writeS(NIL);
                writeCloseParln();
                resultParser.readNativeLine();
            }
            writer.println(command);
            writer.flush();
            return resultParser.readNativeLine();
        } catch (Exception e) {
            throw new RacerRuntimeException(e);
        }
    }

    public boolean isConsistent() {
      //  if (isInterrupted) throw new ReasonerInterruptedException();
        try {
            writeOpenPar();
            writeKRSSMethodName(RacerOWLAPIVocabulary.OWLAPI_ISCONSISTENT);
            writeS(ontologyID);
            writeS(reasonerID);
            if (!configuration.isAboxConsistencyTestEnabled()){
                writeS(RacerOWLAPIVocabulary.NIL);
            }
            writeCloseParln();
            return resultParser.BooleanResult();
        } catch (ParseException e) {
            reportError(e);
            return false;
        }
    }

////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Some test methods
//
////////////////////////////////////////////////////////////////////////////////////////////////////////

    private String readFromSocket2() {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        char c = 0;
        try {
            c = (char) this.inputStream.read();
            baos.write(c);
            while (true) {
                c = (char) inputStream.read();
                boolean b = Character.isWhitespace(c);
                {
                    baos.write(c);
                }

            }
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
//        return s;

    }


    private String readFromSocket() {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        int c;
        try {
            c = this.inputStream.read();
            baos.write(c);
            while (c != 10) {
                c = inputStream.read();
                if (c != 10) {
                    baos.write(c);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
            throw new RacerRuntimeException(e);
        }
        String s = baos.toString();
        //System.out.println("readFromSocket " + s);
        return s;

    }

    public void reset() {
// deleteKnowledgeBase();
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////
    //
    // options
    //
    ////////////////////////////////////////////////////////////////////////////////////////////////////////

    public final void setRangeDescriptions(final boolean rangeDescriptions) {
        this.rangeDescriptions = rangeDescriptions;
    }


    /**
     * Returns always an exception wrapping an error message from RACER.
     *
     * @param exception
     */
    public void reportError(ParseException exception) {
        //   resultParser.errorRecovery();
        String s = sendNativeCommand("(OWLAPI-getLastAnswer)", false);
        s = sendNativeCommand("(OWLAPI-getLastAnswer)", false);
        throw new RacerRuntimeException(s, exception);
    }

    /////////////////////////////////////////////////////////////////////////////////
    //
    // Shutdown
    //
    /////////////////////////////////////////////////////////////////////////////////

    public void shutdown() {
        writeOpenPar();
        writeKRSSMethodName(EXIT);
        writeCloseParln();
    }


    class SingleCommand implements OWLAxiomVisitor {
        Translator delegate;

        public SingleCommand() {
            this.delegate = Translator.this;
        }

        public void visit(OWLDeclarationAxiom axiom) {
            final OWLEntity entity = axiom.getEntity();
            if (entity.isBuiltIn())
                return;
            else if (entity.isOWLClass() || entity.isOWLDataProperty() || entity.isOWLNamedIndividual() ||
                    entity.isOWLAnnotationProperty() || entity.isOWLObjectProperty()) {
                delegate.visit(axiom);
                writeEpilog();
            }
        }

        public void visit(OWLSubClassOfAxiom axiom) {
            delegate.visit(axiom);

            /*  if (prefixManager.getNewPrefixName() != null) {
                //writeAddPrefix     #
                directWriter.write("(OWLAPI-addPrefix ");
                directWriter.write("\"" +prefixManager.getNewPrefixName() + "\"");
                directWriter.write("\"" +prefixManager.getPrefix(prefixManager.getNewPrefixName()) + "\"");
                directWriter.write(")\n");
                directWriter.flush();
                try {
                    readFromSocket();
                } catch (Exception e) {
                    e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                }
                prefixManager.reset();
            }*/
            writeEpilog();

        }

        public void writePrefix(String prefixName, String prefix) {
            writeOpenPar();
            writeKRSSMethodName("OWLAPI-addPrefix");
            writer.write(SPACE);
            writer.write("\"");
            writer.write(prefixName);
            writer.write("\"");
            writer.write("\"");
            writer.write(prefix);
            writer.write("\"");
            writeCloseParln();

        }

        public void visit(OWLNegativeObjectPropertyAssertionAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLAsymmetricObjectPropertyAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLReflexiveObjectPropertyAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLDisjointClassesAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLDataPropertyDomainAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLObjectPropertyDomainAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLEquivalentObjectPropertiesAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLNegativeDataPropertyAssertionAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLDifferentIndividualsAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLDisjointDataPropertiesAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLDisjointObjectPropertiesAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLObjectPropertyRangeAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLObjectPropertyAssertionAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLFunctionalObjectPropertyAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLSubObjectPropertyOfAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLDisjointUnionAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLSymmetricObjectPropertyAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();

        }

        public void visit(OWLDataPropertyRangeAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();

        }

        public void visit(OWLFunctionalDataPropertyAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();

        }

        public void visit(OWLEquivalentDataPropertiesAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();

        }

        public void visit(OWLClassAssertionAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();

        }

        public void visit(OWLEquivalentClassesAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();

        }

        public void visit(OWLDataPropertyAssertionAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();

        }

        public void visit(OWLTransitiveObjectPropertyAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();

        }

        public void visit(OWLIrreflexiveObjectPropertyAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();

        }

        public void visit(OWLSubDataPropertyOfAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();

        }

        public void visit(OWLInverseFunctionalObjectPropertyAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLSameIndividualAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLSubPropertyChainOfAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLInverseObjectPropertiesAxiom axiom) {
            delegate.visit(axiom);
            writeEpilog();
        }

        public void visit(OWLHasKeyAxiom axiom) {
        }

        public void visit(OWLDatatypeDefinitionAxiom axiom) {
        }

        public void visit(SWRLRule rule) {
        }

        public void visit(OWLAnnotationAssertionAxiom axiom) {
        }

        public void visit(OWLSubAnnotationPropertyOfAxiom axiom) {
        }

        public void visit(OWLAnnotationPropertyDomainAxiom axiom) {
        }

        public void visit(OWLAnnotationPropertyRangeAxiom axiom) {
        }
    }
}


