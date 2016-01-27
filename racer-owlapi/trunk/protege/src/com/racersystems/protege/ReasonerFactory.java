package com.racersystems.protege;

import com.racersystems.protege.logging.LogFileManager;
import com.racersystems.protege.preferences.RacerProPreferences;
import com.racersystems.protege.ui.ProtegeReasonerMenuUpdater;
import com.racersystems.protege.ui.TerminalWindow;
import com.racersystems.racer.Configuration;
import com.racersystems.racer.RacerOWLAPIVocabulary;
import com.racersystems.racer.RacerRuntimeException;
import org.protege.editor.owl.model.inference.NoOpReasonerFactory;
import org.protege.editor.owl.model.inference.OWLReasonerManager;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.*;

import javax.swing.*;
import java.io.*;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 08.10.2010
 */
public class ReasonerFactory implements OWLReasonerFactory {

    static AtomicInteger numberOfReasoners = new AtomicInteger(0);
    static TerminalWindow terminalWindow;
    private OWLReasonerManager manager;

    private com.racersystems.racer.Reasoner.ReasonerFactory factory = new com.racersystems.racer.Reasoner.ReasonerFactory();

    public ReasonerFactory(OWLReasonerManager manager) {
        this.manager = manager;
    }

    private void showShippedRacerProBinariesNotFound() {
        JOptionPane.showMessageDialog(null, "The RacerPro plugin tried to use the internal RacerPro but could not find \n"
                + "a valid binary. \n" +
                "The executable will be downloaded when starting ProtŽgŽ.\n" +
                "However,  you can also configure the plugin to use an external RacerPro (see configuration dialog).\n\n" +
                "Switching to \"None reasoner\".",
                "RacerPro binaries are missing",
                JOptionPane.ERROR_MESSAGE);
    }

    private void showExternalRacerProBinariesNotFound() {
        JOptionPane.showMessageDialog(null, "The RacerPro plugin tried to start an external RacerPro but could not find \n"
                + "a valid binary. \n" +
                "Switching to \"None reasoner\".",
                "RacerPro binaries are missing",
                JOptionPane.ERROR_MESSAGE);
    }

    private void showStartingRacerProException(boolean internal) {


        JOptionPane.showMessageDialog(null, "Could not start the " + (internal ? "internal " : "external ") + "RacerPro.\n" +
                (internal ? "" : "Please make sure that the path to the external RacerPro binary file is valid:\n" +
                        new RacerProPreferences().getExternalRacerBinaryPath() +
                        "\nSee " +
                        "the RacerPro plugin configuration dialog.") +
                "\n\n" +
                "Switching to \"None reasoner\".",
                "Error starting RacerPro",
                JOptionPane.ERROR_MESSAGE);
    }


    protected Process startRacer() {
        //System.out.println("startRacer");
        //System.out.println(numberOfReasoners.intValue());
        if (numberOfReasoners.intValue() >= 1) {

        /*    if (!terminalWindow.isVisible()) {
                terminalWindow.RestartListening();
            }
            */
            ReasonerFactory.numberOfReasoners.incrementAndGet();
            //System.out.println("startRacer not needed");

            return null;
        }
        final RacerProPreferences prefs = RacerProPreferences.getInstance();
        List<String> binaryArguments = new ArrayList<String>();
        if (prefs.isStartRacerEnabled()) {
            if (!checkForRunningRacer("127.0.0.1", prefs.getPort(), 1)) {
                RacerProInstalledBinaryInfo info = RacerProInstalledBinaryInfo.getInstance();

                if (!info.isBinaryFileAvailable() && prefs.isIntegratedRacerEnabled()) {
                    //no binary available!
                    showShippedRacerProBinariesNotFound();
                    throw new RacerProBinariesNotFoundException();
                }

                if (prefs.isIntegratedRacerEnabled()) {
                    binaryArguments.add(info.getBinaryAbsoluteFile().getAbsolutePath());
                } else {
                    String path = prefs.getExternalRacerBinaryPath();

                    if (!new File(path).exists()) {
                        showExternalRacerProBinariesNotFound();
                        throw new RacerProBinariesNotFoundException();
                    }
                    binaryArguments.add(path);
                }

                if (info.getOSType() == RacerProInstalledBinaryInfo.OSTYPE.WIN) {
                    binaryArguments.add("+cx");
                }

                binaryArguments.add("--");
                if (prefs.isLoggingEnabled()) {
                    binaryArguments.add("-logging");
                    LogFileManager manager = new LogFileManager();
                    manager.cleanUpLogFiles();
                    binaryArguments.add(manager.getLogFile());
                }
                binaryArguments.add("-http");
                binaryArguments.add("0");
                binaryArguments.add("-u");
                binaryArguments.add("-ef");
                binaryArguments.add("@UTF8");


                ProcessBuilder b;
                if (binaryArguments.size() > 2) {
                    b = new ProcessBuilder(binaryArguments);
                    //System.out.println("Starting Racer with the following options: ");
                    //for (String s : binaryArguments) {
                    // System.out.print(s + " ");
                    //}
                    //System.out.println("---");
                } else {
                    b = new ProcessBuilder(binaryArguments.get(0));
                }
                b.redirectErrorStream(true);
                try {
                    //   System.out.println("Starting process ");
                    Process process = b.start();
                    //  System.out.println("Process started");
                    b.redirectErrorStream(true)  ;

                    final InputStream errorStream = process.getErrorStream();
                    final InputStream inputStream = process.getInputStream();

                    if (prefs.isTerminalWindowEnabled()) {

                        terminalWindow = new TerminalWindow(inputStream, errorStream);
                        terminalWindow.setSize(300, 300);
                        terminalWindow.setVisible(true);
                    }



                    /*
                    Thread t_1 = new Thread(new Runnable() {
                        public void run() {
                            BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
                            PrintStream devNull = new PrintStream(new DevNullStream());
                            String line = "";
                            try {
                                while ((line = reader.readLine()) != null) {
                                    devNull.println(line);
                                    System.out.println(line);
                                }
                            } catch (IOException e) {
                            }
                        }
                    });

                    Thread t_2 = new Thread(new Runnable() {
                        public void run() {
                            BufferedReader reader = new BufferedReader(new InputStreamReader(errorStream));
                            PrintStream devNull = new PrintStream(new DevNullStream());
                            String line = "";
                            try {
                                while ((line = reader.readLine()) != null) {
                                    devNull.println(line);
                                    System.out.println(line);
                                }
                            } catch (IOException e) {
                            }
                        }
                    });
                    t_1.start();
                    t_2.start();

                    */

                    InetSocketAddress address = new InetSocketAddress("127.0.0.1", prefs.getLocalRacerPort());
                    int i = 0;
                    boolean successful = false;
                    //we poll to get the right time for connecting (racer takes some time...)
                    while (true) {
                        try {
                            Socket racerSocket = new Socket(address.getAddress(), address.getPort());
                            racerSocket.close();
                            successful = true;
                            break;
                        } catch (Exception x) {
                            i++;
                            try {
                                Thread.currentThread().sleep(100);
                            } catch (InterruptedException e) {
                            }
                        }
                        if (i > 10000) break;
                    }
                    if (successful) {
                        //System.out.println("successfull");
                        ReasonerFactory.numberOfReasoners.incrementAndGet();
                        //System.out.println(numberOfReasoners.intValue());
                        return process;
                    } else {
                        showStartingRacerProException(prefs.isIntegratedRacerEnabled());
                        throw new RacerProStartingReasonerException();
                    }
                } catch (IOException e) {
                    showStartingRacerProException(prefs.isIntegratedRacerEnabled());
                    throw new RacerProStartingReasonerException();
                } catch (Exception e) {
                    showStartingRacerProException(prefs.isIntegratedRacerEnabled());
                    throw new RacerProStartingReasonerException();
                }
            } else {
                JOptionPane.showMessageDialog(null, "RacerPro is alrady running on port " + prefs.getPort() + "\nConnected to the running RacerPro", "RacerPro", JOptionPane.INFORMATION_MESSAGE);
            }
        } else {
            if (!checkForRunningRacer(prefs.getRemoteRacerAddress(), prefs.getRemoteRacerPort(), 4)) {
                JOptionPane.showMessageDialog(null, "Could not connect to a running RacerPro listening at TCP \n" +
                        "(remote address " + prefs.getRemoteRacerAddress() + " and TCP port number " + prefs.getRemoteRacerPort() + ")\n" +
                        "Please make shure that RacerPro is running \n\n" +
                        "Switching to \"None reasoner\".",
                        "Could not connect to running RacerPro",
                        JOptionPane.ERROR_MESSAGE);
                throw new RacerProRemoteRacerNotFoundException(prefs.getRemoteRacerAddress(), prefs.getRemoteRacerPort());

            }
        }

        return null;
    }

    /**
     * Returns <code>true</code> if a running racer instance on the given host on the given port is found, otherwise
     * <code>false</code> is returned.
     *
     * @param host
     * @param port
     * @param numberOfTries
     * @return
     */
    private boolean checkForRunningRacer(String host, int port, int numberOfTries) {
        //System.out.println("checkforrunningracer");
        InetSocketAddress address = new InetSocketAddress(host, port);
        int i = 0;
        while (true) {
            Socket racerSocket = null;
            PrintWriter pw = null;
            BufferedReader reader = null;
            try {
                racerSocket = new Socket(address.getAddress(), address.getPort());
                //we just guess it is racer
                pw = new PrintWriter(new OutputStreamWriter(racerSocket.getOutputStream(), "UTF-8"));
                pw.println(RacerOWLAPIVocabulary.OPEN_BRACKET + RacerOWLAPIVocabulary.GET_RACER_VERSION + RacerOWLAPIVocabulary.CLOSE_BRACKET);
                pw.flush();
                InputStreamReader inputStreamReader = new InputStreamReader(racerSocket.getInputStream(), "UTF-8");
                reader = new BufferedReader(inputStreamReader);
                //System.out.println("waiting for read");
                String version = reader.readLine();
                //System.out.println("version" + version);
                pw.close();
                inputStreamReader.close();
                racerSocket.close();
                if (version.startsWith(":answer")) {
                    //we guess it is racer.
                    return true;
                } //else if (version)
                return true;
            } catch (Exception x) {
                x.printStackTrace();
                i++;
                try {
                    Thread.currentThread().sleep(100);
                } catch (InterruptedException e) {
                }
                if (racerSocket != null) {
                    try {
                        racerSocket.close();
                    } catch (Exception e) {
                    }
                }
                if (pw != null) {
                    pw.close();
                }
                if (reader != null) {
                    try {
                        reader.close();
                    } catch (Exception e) {
                    }
                }
            }
            if (i > numberOfTries) return false;
        }

    }

    private String readFromSocket(InputStream inputStream) {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        int c;
        try {
            c = inputStream.read();
            baos.write(c);
            while (c != 10) {
                c = inputStream.read();
                if (c != 10) {
                    baos.write(c);
                }

            }
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
        String s = baos.toString();
        return s;

    }

    public static int getNumberOfRunningRacerReasoners() {
        synchronized (numberOfReasoners) {
            return numberOfReasoners.get();
        }
    }

    public static void disposeRunningRacerReasoner(Process process, Reasoner reasoner) {
        //System.out.println("disposeRunningRacerReasoner");
        synchronized (numberOfReasoners) {
            // System.out.println(numberOfReasoners);
            if (process != null) {
                if (numberOfReasoners.decrementAndGet() < 1) {
                    //System.out.println("disposeSilenty");
                    reasoner.disposeSilently();
                } else
                    reasoner.disposeInternaly();
            } else
                reasoner.disposeInternaly();
        }
    }

    public static boolean destroyRunningRacer(Process process) {
        synchronized (numberOfReasoners) {
            if (numberOfReasoners.decrementAndGet() < 1) {
                if (RacerProPreferences.getInstance().isStartRacerEnabled()) {
                    process.destroy();
                    return true;
                }
            }
        }
        return false;
    }

    public static boolean delegateReasonerdispose(Process process, Reasoner reasoner) {
        synchronized (numberOfReasoners) {
            if (numberOfReasoners.decrementAndGet() < 1) {
                if (RacerProPreferences.getInstance().isStartRacerEnabled()) {
                    process.destroy();
                    return true;
                }
            }
            reasoner.dispose();
        }
        return false;
    }

    protected Configuration getConfiguration(OWLReasonerConfiguration configuration) {
        if (configuration instanceof Configuration) return (Configuration) configuration;
        return new Configuration(configuration);
    }

    public OWLReasoner createNonBufferingReasoner(OWLOntology ontology) {
        //System.out.println("createNonBufferingReasoner....");
        try {
            Process process = startRacer();
            return new Reasoner(process, ontology, new Configuration(), BufferingMode.NON_BUFFERING);
        } catch (RacerProBinariesNotFoundException e) {
            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createNonBufferingReasoner(ontology);
        } catch (RacerProRemoteRacerNotFoundException e) {
            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createNonBufferingReasoner(ontology);
        } catch (RacerProStartingReasonerException e) {
            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createNonBufferingReasoner(ontology);
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null, "An exception raised while connecting to Racerpro\n\n" +
                    "\n\n" +
                    "Switching to \"None reasoner\".",
                    "RacerPro exception",
                    JOptionPane.ERROR_MESSAGE);
            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createNonBufferingReasoner(ontology);
        }
    }

    public OWLReasoner createReasoner(OWLOntology ontology) {
         //System.out.println("createReasoner....");
        try {
            Process process = startRacer();

            return new Reasoner(process, ontology, new Configuration(), BufferingMode.BUFFERING);
        } catch (RacerProBinariesNotFoundException e) {
            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createReasoner(ontology);
        } catch (RacerProRemoteRacerNotFoundException e) {
            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createNonBufferingReasoner(ontology);
        } catch (RacerProStartingReasonerException e) {
            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createNonBufferingReasoner(ontology);
        } catch (RacerRuntimeException e) {


            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createReasoner(ontology);

        } catch (Exception e) {
            JOptionPane.showMessageDialog(null, "An exception raised while connecting to Racerpro\n\n" +
                    "\n\n" +
                    "Switching to \"None reasoner\".",
                    "RacerPro exception",
                    JOptionPane.ERROR_MESSAGE);
            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createReasoner(ontology);
        }
    }

    public OWLReasoner createNonBufferingReasoner(OWLOntology ontology, OWLReasonerConfiguration config) throws IllegalConfigurationException {
          //System.out.println("createNonBufferingReasoner with conf....");
        try {
            Process process = startRacer();
            return new Reasoner(process, ontology, getConfiguration(config), BufferingMode.NON_BUFFERING);
        } catch (RacerProBinariesNotFoundException e) {
            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createNonBufferingReasoner(ontology);
        } catch (RacerProRemoteRacerNotFoundException e) {
            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createNonBufferingReasoner(ontology);
        } catch (RacerProStartingReasonerException e) {
            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createNonBufferingReasoner(ontology);
        }
    }

    public OWLReasoner createReasoner(OWLOntology ontology, OWLReasonerConfiguration config) throws IllegalConfigurationException {
        //System.out.println("createReasoner with conf....");
        try {
            Process process = startRacer();
            return new Reasoner(process, ontology, getConfiguration(config), BufferingMode.BUFFERING);
        } catch (RacerProBinariesNotFoundException e) {
            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createReasoner(ontology);
        } catch (RacerProRemoteRacerNotFoundException e) {
            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createNonBufferingReasoner(ontology);
        } catch (RacerProStartingReasonerException e) {
            ProtegeReasonerMenuUpdater updater = new ProtegeReasonerMenuUpdater();
            updater.setNoneReasoner(manager);
            return new NoOpReasonerFactory().createNonBufferingReasoner(ontology);
        }
    }

    public String getReasonerName() {
        return this.factory.getReasonerName();
    }

    class DevNullStream extends OutputStream {

        @Override
        public void write(int b) throws IOException {

        }
    }

}
