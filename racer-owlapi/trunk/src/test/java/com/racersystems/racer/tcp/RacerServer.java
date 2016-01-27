package com.racersystems.racer.tcp;

import java.io.*;
import java.net.Socket;


/**
 * This class implements a racer client with a plain interface. It allows to establish a connection
 * with the RACER server, and send to it plain string messages.
 */

public class RacerServer {

    /**
     * The socket that enables communication with the racer server.
     */

    private Socket racerSocket;


    /**
     * The input stream from the RACER server socket.
     */

    private InputStream racerInputStream;


    /**
     * The output stream to the RACER server socket.
     */

    private PrintStream racerOutputStream;


    /**
     * The IP location where the racer server is located.
     */

    private String racerServerIP;


    /**
     * The port used by the racer server.
     */

    private int racerServerPort;


    /**
     * This is the string for letting know the racer server the process has ended.
     */

    private static final String SERVER_END_STRING = ":eof";


    /**
     * This method builds a new racer client.
     */

    public RacerServer(String ip, int port) {
        racerServerIP = ip;
        racerServerPort = port;
    }

    /**
     * This method tries to establish a connection with the racer server. If there is any problem, an
     * IOException is thrown.
     */

    public void openConnection() throws IOException {
        racerSocket = new Socket(racerServerIP, racerServerPort);
        racerInputStream = racerSocket.getInputStream();
        OutputStream out = racerSocket.getOutputStream();
        racerOutputStream = new PrintStream(out, true);
    }

    public void closeConnection() throws IOException {
        if (racerOutputStream != null) {
            racerOutputStream.print(SERVER_END_STRING);
            racerOutputStream.flush();
            racerInputStream.close();
            racerOutputStream.close();
            racerOutputStream = null;
        }
        if (racerSocket != null) {
            racerSocket.close();
            racerSocket = null;
        }
    }


    /**
     * This method reads a string from the racer socket connection.
     */

    private static String readFromSocket(InputStream in) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        int c = in.read();
        baos.write(c);
        while (c != 10) {
            c = in.read();
            if (c != 10) {
                baos.write(c);
            }

        }
        return baos.toString();

    }


    /**
     * This method sends a command to the RACER server and returns a string with the answer. If the racer
     * server returns an ":ok" message, the answer is the null String; if the racer server returns an
     * ":answer" message, the returned value is the String corresponding to the answer; and finally, if the
     * racer server returns an ":error" message, a RacerException is thrown.
     */

    public String send(String command) throws RacerException, IOException {
        racerOutputStream.println(command);
        String result = readFromSocket(racerInputStream);
        return parseResult(command, result);
    }


    /**
     * This method parses the result obtained from RACER. If RACER produced an ":ok" message,
     * null is returned; if it produced an ":answer" message, the result is then parsed; and
     * if there is an error message, an exception is thrown.
     */

    protected String parseResult(String message, String result) throws RacerException {
        // return null if the result is ok
        if (result.charAt(1) == 'o') {
            String warning = getWarningFromOK(result);
            if (warning != null) printWarning(warning);
            return null;
        }
        // we check whether it has been an error and throw an exception
        if (result.charAt(1) == 'e') {
            String error = getError(result);
            throw new RacerException(error);
        }
        // otherwise, we check whether the result is an answer and retrieve the answer
        if (result.charAt(1) == 'a') {
            String[] ansAndWar = getResultAndWarningFromAnswer(result);
            if (ansAndWar[1] != null) printWarning(ansAndWar[1]);
            return ansAndWar[0];
        }
        // unknown answer
        throw new RacerException("RACER MESSAGE: " + message + "; produced a RACER answer unknown: " + result);
    }

    /**
     * This method returns the answer and warning messages from the RACER result.
     *
     * @param result java.lang.String
     * @return java.lang.String
     */

    protected String getError(String result) {
        String s = new String();
        int iniMessage = result.indexOf(' ', 7);
        int ini = result.indexOf('"', iniMessage);
        int fi = result.indexOf('"', ini + 1);
        s = iniMessage + 1 < ini - 1 ? result.substring(iniMessage + 1, ini - 1) : "";
        if (iniMessage + 1 < ini - 1 && ini + 1 < fi) s = s + ". ";
        s = s + result.substring(ini + 1, fi);
        return s;
    }

    /**
     * This method returns the answer and warning messages from the RACER result.
     *
     * @param result java.lang.String
     * @return java.lang.String
     */

    protected String[] getResultAndWarningFromAnswer(String result) {
        String[] s = new String[2];
        int ini = result.indexOf('"', 10);
        int fi = ini;
        boolean esFinal = false;
        while (!esFinal) {
            fi = result.indexOf('"', fi + 1);
            esFinal = result.charAt(fi - 1) != '\\';
        }
        s[0] = result.substring(ini + 1, fi);
        if (fi + 4 < result.length()) s[1] = result.substring(fi + 3, result.length() - 1);
        return s;
    }

    /**
     * This method returns the answer and warning messages from the RACER result.
     *
     * @param result java.lang.String
     * @return java.lang.String
     */

    protected String getWarningFromOK(String result) {
        String warning = null;
        int ini = result.indexOf('"', 6);
        int fi = result.length() - 1;
        if (ini < fi - 1) warning = result.substring(ini + 1, fi);
        return warning;
    }

    /**
     * This method prints a warning message. The default behaviour is to print it in the console. But
     * the method can be redefined in order to print it to other streams.
     *
     * @param warning java.lang.String
     */
    protected void printWarning(String warning) {
        for (int i = 0; i < warning.length(); i++) {
            char c = warning.charAt(i);
            if (c == (char) 9) System.out.println();
            else System.out.print(warning.charAt(i));
        }
        System.out.println();
    }


    public class RacerException extends Exception {


        /**
         * The constructor creates a RACER exception from the whole string returned by the racer server.
         */

        public RacerException(String racerError) {
            super(racerError);
        }
    }

    public static void main(String[] args) throws Exception {
        RacerServer server = new RacerServer("127.0.0.1", 8088);
        server.openConnection();
        String answer = server.send("(OWLAPI-setmodel)\n");
        System.out.println(answer);
    }

}