package com.racersystems.racer.tcp;

import com.racersystems.jracer.RacerClient;

import java.util.Timer;
import java.util.TimerTask;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 22.10.2012
 */
public class RacerTestSavingMode {

     public static void main(String[] args) {

        // please adjust this path to match your environment:

        String ip = "localhost";
        int port = 8088;
        int controlPort = 8089;

        String[][] allTests = {

                {       "(full-reset)",
                        "(OWLAPI-newReasoner OWLAPI14)",
                        "(OWLAPI-enableSimplifiedProtocol OWLAPI14)",
                         "(OWLAPI-newOntology OWLAPI14 OWLAPI14)",
                        "(OWLAPI-loadOntology OWLAPI14 OWLAPI14)",
                        "(OWLAPI-enableMemorySavingMode OWLAPI14 OWLAPI14)",
                         "(OWLAPI-autoBatchAddAxiomsTo OWLAPI14 OWLAPI14)",
                          "(OWLAPI-getOWLSubClassAxiom  |test#B|  |test#A|)",
                          "(OWLAPI-batchSynchronize OWLAPI14 OWLAPI14)",
                         "(OWLAPI-getDescendantClasses |test#A| OWLAPI14)"

                }

        };

        RacerClient racer = new RacerClient(ip, port);
        final RacerClient racer2 = new RacerClient(ip, controlPort);

        try {


            racer2.openConnection();
            racer.openConnection();

            for (String[] test : allTests) {
                for (String command : test) {
                    System.out.println("Sending: " + command);
                    String s = racer.sendRaw(command);
                    System.out.println("Answer " + s);
                }

            }


            System.out.println("Done.");

        } catch (Exception e) {

            e.printStackTrace();

        }

    }
}
