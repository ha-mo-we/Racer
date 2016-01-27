package com.racersystems.jracer;

import java.io.*;
import java.net.Socket;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * RacerClient is a socket client that opens a socket to a racer server,
 * sends messages to and receives answers from it.
 * By default the client prints no racer warning messages.
 *
 * @author AK 23.11.2007
 * @refurbished and enhanched MW 26.11.2009 ;-)
 * @note works only with mlisp RacerPro!
 */

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

    PrintWriter controlOut;
    BufferedReader controlIn;

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



            Socket controlSocket = new Socket(host, 8089);

            controlOut = new PrintWriter(new OutputStreamWriter(controlSocket.getOutputStream(), "UTF-8"));

            controlIn = new BufferedReader(new InputStreamReader(controlSocket.getInputStream(), "UTF-8"));



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


    private String parse(String result)  {
        return result;
    }

    private String simpleParse(String result) throws RuntimeException {

        return result;

    }


    public static void main(String[] args) {
    RacerClient client =new RacerClient("localhost", 8088);
        client.openConnection();
      //  client.sendRaw("(init-tbox olaf)");
//        client.sendRaw("(OWLAPI-newReasoner r)");
//        client.sendRaw("(OWLAPI-newOntology r r)");
//        client.sendRaw("(OWLAPI-loadOntology r r)");
        client.sendRaw("(OWLAPI-getProgress r)");
        client.sendRaw("(OWLAPI-abort r)");
        client.closeConnection();
    }

}
