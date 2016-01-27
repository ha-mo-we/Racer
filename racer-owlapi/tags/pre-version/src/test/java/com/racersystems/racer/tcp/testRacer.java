package com.racersystems.racer.tcp;

import com.racersystems.racer.RacerRuntimeException;

import java.io.*;
import java.net.InetSocketAddress;
import java.net.Socket;

/**
 * Created by IntelliJ IDEA.
 * Author: Olaf Noppens
 * Date: 05.02.2010
 */
public class testRacer {
    public static final String PHYSICAL_URI = "http://www.w3.org/TR/owl-guide/wine.rdf";


    public static void main(String[] args) throws Exception {
        InetSocketAddress address = new InetSocketAddress("localhost", 8088);
        while (true) {
            Socket racerSocket = new Socket(address.getAddress(), address.getPort());
            racerSocket.setReuseAddress(true);
            // racerSocket.setSendBufferSize(64000);
            InputStream racerInputStream = racerSocket.getInputStream();
            PrintWriter racerOutputStream = new PrintWriter(new OutputStreamWriter(racerSocket.getOutputStream(), "UTF-8"));
            BufferedReader inputStreamReader = new BufferedReader(new
                    InputStreamReader(racerSocket.getInputStream(), "UTF-8"));

            PrintWriter out = new PrintWriter(new OutputStreamWriter(racerSocket.getOutputStream(), "UTF-8"));

            BufferedReader in = new BufferedReader(new InputStreamReader(racerSocket.getInputStream(), "UTF-8"));


            //racerOutputStream.println("(OWLAPI-getProgress r)");
            //racerOutputStream.flush();
            //readFromSocket(racerInputStream);

            out.println("(OWLAPI-getProgress r1)");
            out.flush();
            System.out.println(in.readLine());
            racerSocket.close();
            Thread.currentThread().sleep(20);

        }
        //readFromSocket(racerInputStream);

    }

    private static String readFromSocket(InputStream inputstream) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        int c = 0;
        try {
            c = inputstream.read();
            baos.write(c);
            while (c != 10) {
                c = inputstream.read();
                if (c != 10) {
                    baos.write(c);
                }

            }
        } catch (IOException e) {
            throw new RacerRuntimeException(e);
        }
        String s = baos.toString();
        System.out.println(s);
        return s;

    }
}
