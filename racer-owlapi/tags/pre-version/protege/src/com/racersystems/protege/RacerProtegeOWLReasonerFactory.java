package com.racersystems.protege;

import com.racersystems.protege.preferences.RacerPreferences;
import com.racersystems.racer.RacerReasoner;
import com.racersystems.racer.RacerReasonerConfiguration;
import com.racersystems.racer.RacerReasonerFactory;
import com.racersystems.racer.RacerSocketAdapter;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.*;

import java.io.File;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 08.10.2010
 */
public class RacerProtegeOWLReasonerFactory extends RacerReasonerFactory {

    public static void main(String[] args) throws Exception {
        final StringBuilder builder = new StringBuilder();
        List<String> binaryArguments = new ArrayList<String>();
        binaryArguments.add("./RacerPro");
        binaryArguments.add("--");
        binaryArguments.add("-ef");
        binaryArguments.add("@UTF8");
        binaryArguments.add("-debug");
        ProcessBuilder b;
        b = new ProcessBuilder(binaryArguments);
        b.directory(new File("/Applications/RacerPro-2.0/RacerPro-2-0-Preview-2010-10-11-Mac"));
        final Process process = b.start();
        //System.out.println("Process started");
        InetSocketAddress address = new InetSocketAddress("127.0.0.1", 8088);
        int i = 0;
        //we poll to get the right time for connecting (racer takes some time...)
        while (true) {
            try {
                try {
                    Thread.currentThread().sleep(2500);
                } catch (InterruptedException e) {
                }
                Socket racerSocket = new Socket(address.getAddress(), address.getPort());


                racerSocket.close();
                break;
            } catch (Exception x) {
                i++;
            }
            if (i > 10000) break;
        }
        RacerSocketAdapter adapter = new RacerSocketAdapter(address);
        adapter.closeConnection();
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        OWLOntology rootOntology = manager.loadOntology(IRI.create("http://www.w3c.org/TR/owl-guide/wine.rdf"));
        rootOntology = manager.createOntology();
        for (int j = 0; j < 10; j++) {
            manager.addAxiom(rootOntology, manager.getOWLDataFactory().getOWLSubClassOfAxiom(manager.getOWLDataFactory().getOWLClass(IRI.create("ff" + j)), manager.getOWLDataFactory().getOWLClass(IRI.create("jj" + j))));
        }
        OWLReasoner reasoner = new RacerReasonerFactory().createReasoner(rootOntology);
        reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY);
        reasoner.dispose();
        process.destroy();
    }

    static AtomicInteger numberOfReasoners = new AtomicInteger(0);


    protected Process startRacer() {
        if (numberOfReasoners.get() > 0) {
            return null;
        }
        final RacerPreferences prefs = RacerPreferences.getInstance();
        final StringBuilder builder = new StringBuilder();
        List<String> binaryArguments = new ArrayList<String>();
        if (prefs.isStartRacerEnabled()) {
            if (prefs.isIntegratedRacerEnabled()) {
                RacerInstalledBinaryInfo info = RacerInstalledBinaryInfo.getInstance();
                binaryArguments.add(info.getBinaryAbsoluteFile().getAbsolutePath());
            } else {
                String path = prefs.getExternalRacerBinaryPath();
                binaryArguments.add(path);
            }
            binaryArguments.add("--");
            binaryArguments.add("-http");
            binaryArguments.add("0");
            binaryArguments.add("-u");
            binaryArguments.add("-ef");
            binaryArguments.add("@UTF8");
            ProcessBuilder b;
            if (binaryArguments.size() > 2) {
                b = new ProcessBuilder(binaryArguments);
               // System.out.println("Starting Racer with the following options: ");
                for (String s : binaryArguments) {
                    //System.out.print(s + " ");
                }
                //System.out.println("---");
            } else {
                b = new ProcessBuilder(binaryArguments.get(0));
            }
            b.redirectErrorStream(true);
            try {
                //   System.out.println("Starting process ");
                Process process = b.start();
                //  System.out.println("Process started");
                InetSocketAddress address = new InetSocketAddress("127.0.0.1", prefs.getLocalRacerPort());
                int i = 0;
                //we poll to get the right time for connecting (racer takes some time...)
                while (true) {
                    try {
                        //System.out.println(i + " th try");
                        Socket racerSocket = new Socket(address.getAddress(), address.getPort());
                        //System.out.println("open!");
                        racerSocket.close();
                        break;
                    } catch (Exception x) {
                        i++;
                        //System.out.println("not yet open!");
                        try {
                     //       System.out.println("sleep");
                            Thread.currentThread().sleep(100);
                       //     System.out.println("awoke");
                        } catch (InterruptedException e) {
                        }
                    }
                    if (i > 10000) break;
                }
                //   System.out.println("return process");
                RacerProtegeOWLReasonerFactory.numberOfReasoners.incrementAndGet();
                return process;

            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        return null;
    }

    public static int getNumberOfRunningRacerReasoners() {
        synchronized (numberOfReasoners) {
            return numberOfReasoners.get();
        }
    }

    public static void destroyRunningRacer(Process process) {
        synchronized (numberOfReasoners) {
            //System.out.println(numberOfReasoners.intValue());
            if (numberOfReasoners.decrementAndGet() < 1) {
                //System.out.println("Racer shutdowned");
                process.destroy();
            }
        }
    }

    @Override
    public RacerReasoner createNonBufferingReasoner(OWLOntology ontology) {
        Process process = startRacer();
        return new RacerProtegeOWLReasoner(process, ontology, new RacerReasonerConfiguration(), BufferingMode.NON_BUFFERING);
    }

    @Override
    public RacerReasoner createReasoner(OWLOntology ontology) {
        Process process = startRacer();
        return new RacerProtegeOWLReasoner(process, ontology, new RacerReasonerConfiguration(), BufferingMode.BUFFERING);
    }

    @Override
    public RacerReasoner createNonBufferingReasoner(OWLOntology ontology, OWLReasonerConfiguration config) throws IllegalConfigurationException {
        Process process = startRacer();
        if (config instanceof RacerReasonerConfiguration)
            return new RacerProtegeOWLReasoner(process, ontology, (RacerReasonerConfiguration) config, BufferingMode.NON_BUFFERING);
        throw new IllegalConfigurationException("RacerReasonerConfiguration is expected", config);

    }

    @Override
    public RacerReasoner createReasoner(OWLOntology ontology, OWLReasonerConfiguration config) throws IllegalConfigurationException {
        Process process = startRacer();
        if (config instanceof RacerReasonerConfiguration)
            return new RacerProtegeOWLReasoner(process, ontology, (RacerReasonerConfiguration) config, BufferingMode.BUFFERING);
        throw new IllegalConfigurationException("RacerReasonerConfiguration is expected", config);
    }

    


}
