package com.racersystems.racer.tcp;

import com.racersystems.racer.RacerRuntimeException;

import java.io.*;
import java.net.InetSocketAddress;
import java.net.Socket;

/**
 * Author: Olaf Noppens
 * Date: 11.10.2010
 */
public class progresstest {
    public static void main(String[] args) throws Exception {
        InetSocketAddress address = new InetSocketAddress("127.0.0.1", 8088);

        Socket racerSocket = new Socket(address.getAddress(), address.getPort());
        racerSocket.setReuseAddress(true);
        racerSocket.setSendBufferSize(640);
        InputStream racerInputStream = racerSocket.getInputStream();
        //racerOutputStream = new PrintWriter((racerSocket.getOutputStream()));
        PrintWriter racerOutputStream = new PrintWriter(new OutputStreamWriter(racerSocket.getOutputStream()));
        BufferedReader inputStreamReader = new BufferedReader(new
                InputStreamReader(racerSocket.getInputStream()));


        //TCPRacerSocketAdapter adapter = new TCPRacerSocketAdapter(address);
        //InputStream in = adapter.getInputStream();
        //Writer out = new PrintWriter(adapter.getOutputWriter());
        while (true) {
            racerOutputStream.write("(get-progress-indicator)\n");
            //out.write("(in-tbox olaf)\n");
            racerOutputStream.flush();
            System.out.println(readFromSocket(racerInputStream));

        }
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
