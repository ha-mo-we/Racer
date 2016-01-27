package com.racersystems.racer;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.BufferingMode;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;
import uk.ac.manchester.cs.owl.owlapi.OWLOntologyManagerImpl;

import java.io.*;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Adapter for socket communication with RACER.
 *
 * @author Olaf Noppens
 * @version 1.0
 */
public final class RacerSocketAdapter {
    /**
     * The socket that enables communication with the racer server.
     */
    private final Socket racerSocket;


    /**
     * The input stream from the RACER server socket.
     */

    private final InputStream racerInputStream;


    /**
     * The output stream to the RACER server socket.
     */

    private final Writer racerOutputStream;

    private Reader inputStreamReader;

    private boolean isClosed = false;

    /**
     * This is the string for letting know the racer server the process has ended.
     */

    private static final String SERVER_END_STRING = ":eof";


    public RacerSocketAdapter(final InetSocketAddress address) throws IOException {
        racerSocket = new Socket(address.getAddress(), address.getPort());
        racerSocket.setReuseAddress(true);
        racerSocket.setSendBufferSize(64000);
        racerInputStream = racerSocket.getInputStream();
        //racerOutputStream = new PrintWriter((racerSocket.getOutputStream()));
        racerOutputStream = new PrintWriter(new OutputStreamWriter(racerSocket.getOutputStream(), "UTF-8"));
        /*inputStreamReader = new BufferedReader(new
                InputStreamReader(racerSocket.getInputStream(), "UTF-8"));*/
        inputStreamReader = new
                InputStreamReader(racerSocket.getInputStream(), "UTF-8");
    }


    public void closeConnection() throws IOException {
        if (racerSocket.isConnected() && !isClosed) {
            //  racerOutputStream.write(TCPRacerSocketAdapter.SERVER_END_STRING);
            /*racerOutputStream.flush();
            racerInputStream.close();
            racerOutputStream.close();
            racerSocket.close();*/
        }
        racerOutputStream.flush();
        racerInputStream.close();
        racerOutputStream.close();
        racerSocket.close();
        isClosed = true;

    }

    public final Writer getOutputWriter() {
        return racerOutputStream;
    }

    public final InputStream getInputStream() {
        return racerInputStream;
    }

    public final Reader getInputReader() {
        return inputStreamReader;
    }


    /**
     * Returns the InetAddress to which this socket is bound to.
     *
     * @return InetAddress to which this socket is bound to.
     */
    public final InetAddress getInetAddress() {
        return racerSocket.getInetAddress();
    }

    private static String readFromSocket(InputStream inputStream) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        int c = 0;
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
}
