package com.racersystems.racer.tcp;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

import com.racersystems.jracer.RacerClient;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.net.URL;
import java.util.Timer;
import java.util.TimerTask;
import java.util.Vector;

public class RacerTest10 {

    public static void main(String[] args) throws IOException {

        String ip = "localhost";
        int port = 8088;
        int controlPort = 8089;
        
        // please adjust this path to match your environment:
        String chemisty = "/Users/noppens/Projekte/racer-owlapi-adapter/racer-owlapi/trunk/src/test/java/com/racersystems/racer/tcp/Chemistry.txt";
        File file = new File(chemisty);

        FileReader reader = new FileReader(file);
        BufferedReader breader = new BufferedReader(reader);
        String line = null;
        Vector<String> v = new Vector<String>();
        while ((line = breader.readLine()) != null) {
            v.add(line);
        }

        final String[][] allTests = new String[4][];
        allTests[0] = new String[] { "(OWLAPI-newReasoner testReasoner)",
                        "(OWLAPI-enableSimplifiedProtocol testReasoner)",
                        "(OWLAPI-newOntology testReasoner testReasoner)",
                        "(OWLAPI-loadOntology testReasoner testReasoner)",
                        "(OWLAPI-setProgressRange 178 0 50 testReasoner)",
                        "(OWLAPI-autoBatchAddAxiomsTo testReasoner testReasoner)"};
        allTests[1] = new String[v.size()];
        for (int i=0; i<v.size(); i++)
            allTests[1][i] = v.get(i);
        allTests[2] = new String[] {"(OWLAPI-batchSynchronize testReasoner testReasoner)"};

        allTests[3] = new String[] {"(OWLAPI-classify testReasoner)"};

        RacerClient racer = new RacerClient(ip, port);
        final RacerClient racer2 = new RacerClient(ip, controlPort);

        try {


            racer2.openConnection();

            TimerTask progressChecker = new TimerTask() {
                public void run() {
                    try {
                        System.out.println();
                        System.out.print("Progress: ");
                        racer2.out.println("(OWLAPI-getProgress testReasoner)");
                        racer2.out.flush();
                        System.out.println(racer2.in.readLine());
                        System.out.print("Command: ");
                        racer2.out.println("(OWLAPI-getCurrentRequest testReasoner)");
                        racer2.out.flush();
                        System.out.println(racer2.in.readLine());
                        System.out.println();

                    } catch (Exception e) {
                    }
                }
            };

            Timer timer = new Timer();
            //  timer.schedule(progressChecker, 0, 100);

            racer.openConnection();

            // racer.loggingOn$();
            // racer.sendRaw("(logging-on)");
            int i = 0;
            for (String[] test : allTests) {
                if (i == 3) {
                    javax.swing.Timer timer2 = new javax.swing.Timer(2000, new ActionListener() {
                        public void actionPerformed(ActionEvent e) {
                            System.out.println("interrupt");
                            racer2.out.println("(OWLAPI-abortRequest testReasoner)");
                            racer2.out.flush();
                            try {
                                System.out.println(racer2.in.readLine());

                                racer2.out.println("(OWLAPI-getLastAnswer)");
                                racer2.out.flush();
                                String s = racer2.in.readLine();
                                System.out.println(s);

                            } catch (IOException e1) {
                                e1.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                            }
                        }
                    });
                    timer2.start();
                }
                for (String command : test) {
                    System.out.println("Sending: " + command);
                    String s = racer.sendRaw(command);
                    Thread.sleep(10);
                }
                i++;

            }

            progressChecker.cancel();

            System.out.println("Done.");

        } catch (Exception e) {

            e.printStackTrace();

        }

    }

}

