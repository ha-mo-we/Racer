package com.racersystems.jracer;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.net.Socket;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Scanner;

/**
 * RacerClient is a socket client that opens a socket to a racer server,
 * sends messages to and receives answers from it. 
 * By default the client prints no racer warning messages. 
 *
 * @author AK 23.11.2007 
 * @refurbished and enhanched MW 26.11.2009, 14.10.2012, 26.02.2013, 16.06.2013, 16.04.2014 
 * @note works only with mlisp RacerPro!
 */

public class RacerClient extends RacerStubs {

	private int socket;
	private String host = null;

	private boolean useStringBuildersEvenForTokens = false; 
	private boolean globallyEnableSimplifiedProtocol = true; 
	private boolean returnListsForEmptyLists = true;

	public boolean throwRacerErrorsAsExceptions = false; 
	public boolean printWarningMessages = true;
	public boolean debugging = false;

	private String lastWarning = ""; 
	private String lastError = ""; 

	private Socket racerSocket = null;
	public PrintWriter out = null;
	public BufferedReader in = null;

	private Map<String,Object[]> withMacroStack = new LinkedHashMap<String,Object[]>(); 

	public RacerClient(String host, int socket) {
		this.host = host;
		this.socket = socket;			
	}

	public void openConnection() throws RacerClientException {

		try {

			racerSocket = new Socket(host, socket);

			racerSocket.setReuseAddress(true);

			out = new PrintWriter(new OutputStreamWriter(racerSocket.getOutputStream(), "UTF-8"));	

			in = new BufferedReader(new InputStreamReader(racerSocket.getInputStream(), "UTF-8"));

			out.println("(|OWLAPI-disableSimplifiedProtocol| :|global|)"); 
			out.flush();
			in.readLine();

			if (globallyEnableSimplifiedProtocol) {

				out.println("(|OWLAPI-enableSimplifiedProtocol| :|global|)"); 
				out.flush();
				in.readLine();

			}

		} catch (Exception ex) {

			throw new RacerClientException(ex.getMessage());

		}

	}

	public void closeConnection() throws RacerClientException {	   
		try {

			out.close();

			in.close();

			racerSocket.close();

		} catch (IOException ex) {

			throw new RacerClientException(ex.getMessage());

		}	   

	}

	public void addArgs(StringBuilder call, Object... args) {

		for (Object arg : (Object[])args) {

			if (arg instanceof Object[]) 
				addArgs(call,(Object[])arg);
			else
				if (arg instanceof String) {
					if (((String) arg).matches(".*\\(.*\\).*")) {
						// List? 
						call.append(" ").append(arg);
					} else if (((String) arg).matches(".*\\\".*\\\".*")) {
						// String? 
						call.append(" ").append(arg);
					} else if (((String) arg).matches(".*\\s+.*")) {
						// Identifier with whitespace? needs bars! 
						call.append(" |").append(arg).append("|");
					} else { 
						call.append(" ").append(arg);
					}
				}
				else 
					if (arg instanceof Boolean) 
						if ((Boolean)arg) 
							call.append(" t");
						else 
							call.append(" nil)"); 
					else {
						call.append(" ").append(arg.toString());
					}
		}

	}

	public StringBuilder racerCallString(Object... args) {	   
		StringBuilder call = new StringBuilder();

		call.append("(");

		addArgs(call,args);

		call.append(")");

		return call; 
	}


	public RacerResult racerCall(Object... args) throws RacerClientException {	   
		StringBuilder call = racerCallString(args); 
		return send(call); 
	}

	public boolean returnBoolean(RacerResult answer) {
		if (answer instanceof RacerList) {                                                                
			// empty list = nil = false! 
			return !(((RacerList) answer).value.size()==0);
		}
		else {                                                                
			String result = (String)answer.getValue(); 
			return result.equals("t") || result.equals("T"); 
		}
	}

	public boolean returnBoolean(String answer) {
		return answer.equals("t") || answer.equals("T"); 
	}

	public String sendRaw(String command) throws RacerClientException {		   
		try {

			out.println("(|OWLAPI-disableSimplifiedProtocol| :|global|)"); 
			out.flush();
			in.readLine();

			if (debugging) System.out.println("***** Call raw: "+command.toString()); 

			out.println(command);
			out.flush();

			String result = parse(in.readLine()).toString(); 

			if (debugging) System.out.println("***** Raw result: "+result); 

			if (result.equals(":abort")) {
				if (printWarningMessages) 
					System.out.println("Request was aborted!"); 
				return result; 
			}

			if (globallyEnableSimplifiedProtocol) {

				out.println("(|OWLAPI-enableSimplifiedProtocol| :|global|)"); 
				out.flush();
				in.readLine();

			}

			if (debugging) System.out.println("***** Raw result returns: "+result.toString()); 

			return result;

		} catch (Exception ex) {

			throw new RacerClientException(ex.getMessage());

		}

	}
	
	public RacerResult sendRaw$(String command) throws RacerClientException {			
		return racerCall(command);		
	}

	public void transferFile(String filename, String extension) throws RacerClientException {

		try {

			File f = new File(filename); 

			FileInputStream fin = new FileInputStream(f);

			if (!globallyEnableSimplifiedProtocol) {
				out.println("(|OWLAPI-enableSimplifiedProtocol|)"); 
				out.flush();
				in.readLine(); 
			}

			out.println("(transmit-file \""+extension+"\" "+f.length()+")"); 
			out.flush();

			String tempFile=in.readLine();

			byte[] buffer = new byte[0xFFFF];

			for (int len; (len = fin.read(buffer)) != -1; )
				racerSocket.getOutputStream().write(buffer,0,len); 
			out.flush();

			if (extension.equals("owl")) 
				owlReadFile(tempFile);
			else if (extension.equals("racer"))
				racerReadFile(tempFile);
			else 
				owlapiReadOntology(tempFile);

		} catch (Exception ex) {

			throw new RacerClientException(ex.getMessage());

		}

	}

	public void pushWith(String withMacro, Object... args) {	   
		withMacroStack.put(withMacro,args); 

	}

	public void popWith(String withMacro) {
		withMacroStack.remove(withMacro); 

	}

	public RacerResult send(StringBuilder command) throws RacerClientException {		   

		try {

			if (!globallyEnableSimplifiedProtocol) {
				out.println("(|OWLAPI-enableSimplifiedProtocol|)"); 
				out.flush();
				in.readLine();
			}

			StringBuilder context = new StringBuilder(); 
			int counter = 0; 

			for(Map.Entry<String,Object[]> pair : withMacroStack.entrySet()) {
				counter++; 
				String withContext = pair.getKey(); 
				Object[] args = pair.getValue(); 

				StringBuilder withArgs = new StringBuilder(); 

				addArgs(withArgs,args); 

				if ((withArgs.length())==0)
					context.append("(").append(withContext).append(" ");
				else
					context.append("(").append(withContext).append(" (").append(withArgs).append(") ");
			}

			if (context.length() > 0) // efficency 
				command = context.append(command); 

			for(int i=1 ; i<=counter; i++) command.append(")"); 

			if (debugging) System.out.println("***** Call: "+command.toString()); 

			out.println(command.toString());
			out.flush();

			String answer=in.readLine();
			String warning = ""; 
			String error = ""; 
			Boolean simplified = true; 

			if (answer.equals(":abort")) {
				if (printWarningMessages) 
					System.out.println("Request was aborted!"); 
				return parse1(answer); 
			}

			if (answer.equals(":error")) {
				try {
					out.println("(|OWLAPI-getLastAnswer|)"); 
					out.flush();
					error=in.readLine();
				} catch (Exception ex) { }
			}

			if (printWarningMessages) {
				try {
					out.println("(|OWLAPI-getLastOutputStreamString|)"); 
					out.flush();
					warning=in.readLine();
				} catch (Exception ex) { }
			}

			if (!globallyEnableSimplifiedProtocol) {
				try {
					out.println("(|OWLAPI-usesSimplifiedProtocol|)"); 
					out.flush();
					String result = in.readLine(); 
					simplified=result.equals("t") || result.equals("T");; 
				} catch (Exception ex) { } 
			} else 
				simplified=true; 

			if ( simplified ) {

				if (printWarningMessages && !warning.equals("\"\"") && 
						!warning.equals("nil") && 
						!warning.equals("NIL") ) {
					warning=warning.replace('\t','\n');
					warning=warning.substring(1,warning.length()-1); 
					System.out.println("Racer Message (STDOUT):"+warning); 
					System.out.println(); 
					lastWarning=warning;
				}

				if (!error.equals("")) {
					error=error.substring(1,error.length()-1); 
					System.out.println("Racer ERROR: "+error); 
					System.out.println(); 
					lastError=error;

					if (throwRacerErrorsAsExceptions) 
						throw new RacerClientException(error); 

				}

				return parse1(answer);

			} else {

				parse(answer); 

				return parse1(answer); }

		} catch (Exception ex) {

			throw new RacerClientException(ex.getMessage());

		}

	}

	public RacerResult parsingTest() throws RacerClientException {		   

		try {

			String line = "(((?x |http://cohse.semanticweb.org/ontologies/people#Fred|) (?y |http://cohse.semanticweb.org/ontologies/people#Tibbs|)) ((?x |http://cohse.semanticweb.org/ontologies/people#Joe|) (?y |http://cohse.semanticweb.org/ontologies/people#Fido|)) ((?x |http://cohse.semanticweb.org/ontologies/people#Walt|) (?y |http://cohse.semanticweb.org/ontologies/people#Louie|)) ((?x |http://cohse.semanticweb.org/ontologies/people#Walt|) (?y |http://cohse.semanticweb.org/ontologies/people#Dewey|)) ((?x |http://cohse.semanticweb.org/ontologies/people#Walt|) (?y |http://cohse.semanticweb.org/ontologies/people#Huey|)) ((?x |http://cohse.semanticweb.org/ontologies/people#Minnie|) (?y |http://cohse.semanticweb.org/ontologies/people#Tom|)) ((?x |http://cohse.semanticweb.org/ontologies/people#Mick|) (?y |http://cohse.semanticweb.org/ontologies/people#Rex|)))";
			return parse1(line);

		} catch (Exception ex) {

			throw new RacerClientException(ex.getMessage());

		}

	}

	public RacerResult parseRacerAnswer(String answer) {

		RacerList<RacerResult> res = new RacerList<RacerResult>(); 

		if (debugging) System.out.println("***** Parse: "+answer); 

		if (answer != null) 
			if (useStringBuildersEvenForTokens) 
				parseList1(answer,res,0,0);
			else
				parseList(answer,res,0,0);

		if (debugging) System.out.println("***** Parsed: "+res+" Length: "+res.value.size());

		if (res.value.size() == 0)
			if (returnListsForEmptyLists) 
				return res;
			else
				return new RacerNull(); 
		else 
			if (res.value.size()>1)
				return res; 
			else
				return res.value.get(0); 

	}

	private int parseList(String string, RacerList<RacerResult> items, int pos, int level) {

		int n = string.length(); 

		boolean readingString = false; 
		boolean readingEscapedSymbol = false; 
		boolean escaped = false; 

		String curItem = ""; 

		while (pos < n) {

			char cur = string.charAt(pos);

			switch (cur) {

			case '\\' : {

				curItem=curItem+'\\'; 
				escaped=true; 
				break; 

			}

			case '|' : {

				if (! escaped && !readingString) {

					curItem=curItem+'|'; 

					if (readingEscapedSymbol) { 
						readingEscapedSymbol=false; 

						items.value.add(new RacerSymbol(curItem)); 
						curItem=""; 
					} else {
						readingEscapedSymbol=true; 
					}

					break; 

				}

				// prevent next case!

				escaped=false;
				curItem=curItem+cur; 
				break;

			}

			case '"' : { 

				if (! escaped && ! readingEscapedSymbol) {

					curItem=curItem+'"'; 

					if (readingString) { 
						readingString=false; 
						items.value.add(new RacerString(curItem)); 
						curItem=""; 
					} else {
						readingString=true; 
					}

					break;
				}
			}

			case ' ' : ;
			case '\t' : ;
			case '\f' : ;
			case '\n' : {

				if (! readingString && ! escaped && ! readingEscapedSymbol) {
					if (! curItem.equals("")) {
						items.value.add(new RacerSymbol(curItem)); 
						curItem=""; 
					}

					break;  
				}
			}

			case ')' : {

				if (! readingString && ! escaped && ! readingEscapedSymbol) {
					if (! curItem.equals("")) {
						items.value.add(new RacerSymbol(curItem)); 
						curItem=""; 
					}

					return pos; 
				}

			}

			case '(' : {

				if (! readingString && ! escaped && ! readingEscapedSymbol) {
					if (! curItem.equals("")) {
						items.value.add(new RacerSymbol(curItem)); 
						curItem=""; 
					}

					RacerList<RacerResult> items2 = new RacerList<RacerResult>(); 
					items.value.add(items2); 

					pos = parseList(string,items2,1+pos,1+level); 
					break; 

				}

			}

			case '0' : ;
			case '1' : ;
			case '2' : ;
			case '3' : ;
			case '4' : ;
			case '5' : ;
			case '6' : ;
			case '7' : ;
			case '8' : ;
			case '9' : ;
			case '.' : { 

				if (curItem.equals("")) { // start of number? 

					Scanner scanner = new Scanner(string.substring(pos)); 

					boolean found = false; 

					if (scanner.hasNextInt()) {
						int item; 
						item = scanner.nextInt(); 
						items.value.add(new RacerNumber(item)); 
						found=true; 
					} else if (scanner.hasNextBigInteger()) {
						BigInteger item; 
						item = scanner.nextBigInteger(); 
						items.value.add(new RacerNumber(item)); 
						found=true; 
					} else if (scanner.hasNextDouble()) {
						double item; 
						item = scanner.nextDouble(); 
						items.value.add(new RacerNumber(item)); 
						found=true; 
					} else if (scanner.hasNextFloat()) {
						float item; 
						item = scanner.nextFloat(); 
						items.value.add(new RacerNumber(item)); 
						found=true; 
					} 

					if (found) pos=pos+scanner.match().end()-1; 
					
					scanner.close();
					
					break;
				}

			}

			default : 

				escaped=false;
				curItem=curItem+cur; 

			}

			pos++;

		}

		if ((!curItem.equals("")) 
				&& (!curItem.equals("nil"))
				&& (!curItem.equals("NIL"))) { 

			// no closing bracket? single symbol? 
			// but only if not NIL! 

			items.value.add(new RacerSymbol(curItem)); 

		}

		return pos;

	}


	private int parseList1(String string, RacerList<RacerResult> items, int pos, int level) {

		int n = string.length(); 

		boolean readingString = false; 
		boolean readingEscapedSymbol = false; 
		boolean escaped = false; 

		StringBuilder curItem = new StringBuilder(); 

		while (pos < n) {

			char cur = string.charAt(pos);

			switch (cur) {

			case '\\' : {

				curItem.append("\\"); 
				escaped=true; 
				break; 

			}

			case '|' : {

				if (! escaped && !readingString) {

					curItem.append("|"); 

					if (readingEscapedSymbol) { 
						readingEscapedSymbol=false; 

						items.value.add(new RacerSymbol(curItem.toString())); 
						curItem=new StringBuilder(); 
					} else {
						readingEscapedSymbol=true; 
					}

					break; 

				}

				// prevent next case!

				escaped=false;
				curItem.append(cur); 
				break;

			}

			case '"' : { 

				if (! escaped && ! readingEscapedSymbol) {

					curItem.append('"'); 

					if (readingString) { 
						readingString=false; 
						items.value.add(new RacerString(curItem.toString())); 
						curItem=new StringBuilder(); 
					} else {
						readingString=true; 
					}

					break;
				}
			}

			case ' ' : ;
			case '\t' : ;
			case '\f' : ;
			case '\n' : {

				if (! readingString && ! escaped && ! readingEscapedSymbol) {
					if (curItem.length()>0) {
						items.value.add(new RacerSymbol(curItem.toString())); 
						curItem=new StringBuilder(); 
					}

					break;  
				}
			}

			case ')' : {

				if (! readingString && ! escaped && ! readingEscapedSymbol) {
					if (curItem.length()>0) {
						items.value.add(new RacerSymbol(curItem.toString())); 
						curItem=new StringBuilder(); 
					}

					return pos; 
				}

			}

			case '(' : {

				if (! readingString && ! escaped && ! readingEscapedSymbol) {
					if (curItem.length()>0) {
						items.value.add(new RacerSymbol(curItem.toString())); 
						curItem=new StringBuilder(); 
					}

					RacerList<RacerResult> items2 = new RacerList<RacerResult>(); 
					items.value.add(items2); 

					pos = parseList(string,items2,1+pos,1+level); 
					break; 

				}

			}

			case '0' : ;
			case '1' : ;
			case '2' : ;
			case '3' : ;
			case '4' : ;
			case '5' : ;
			case '6' : ;
			case '7' : ;
			case '8' : ;
			case '9' : ;
			case '.' : { 

				if (curItem.length()==0) { // start of number? 

					Scanner scanner = new Scanner(string.substring(pos)); 

					boolean found = false; 

					if (scanner.hasNextInt()) {
						int item; 
						item = scanner.nextInt(); 
						items.value.add(new RacerNumber(item)); 
						found=true; 
					} else if (scanner.hasNextBigInteger()) {
						BigInteger item; 
						item = scanner.nextBigInteger(); 
						items.value.add(new RacerNumber(item)); 
						found=true; 
					} else if (scanner.hasNextDouble()) {
						double item; 
						item = scanner.nextDouble(); 
						items.value.add(new RacerNumber(item)); 
						found=true; 
					} else if (scanner.hasNextFloat()) {
						float item; 
						item = scanner.nextFloat(); 
						items.value.add(new RacerNumber(item)); 
						found=true; 
					} 

					if (found) pos=pos+scanner.match().end()-1;
					
					scanner.close();

					break;
				}

			}

			default : 

				escaped=false;
				curItem.append(cur); 

			}

			pos++;

		}

		if ((!curItem.equals("")) 
				&& (!curItem.equals("nil"))
				&& (!curItem.equals("NIL"))) { 

			// no closing bracket? single symbol? 
			// but only if not NIL! 
			items.value.add(new RacerSymbol(curItem.toString())); 

		}

		return pos;

	}


	private RacerResult parse1(String result) throws RacerClientException {

		return parseRacerAnswer(simpleParse(result)); 

	}

	private String parse(String result) throws RacerClientException {

		if (debugging) System.out.println("***** Parse: "+result); 

		if (result.charAt(1)=='a') { // answer or abort message		    

			if (result.charAt(2)=='n') { // answer message 

				String answer="";

				String warning=null;

				int ini=result.indexOf('"',10);

				int fi=ini;

				boolean found=false;

				while (!found) {
					fi=result.indexOf('"',fi+1);
					found=result.charAt(fi-1)!='\\';
				}

				answer=result.substring(ini+1,fi);

				if (fi+4<result.length()) 
					warning=result.substring(fi+3,result.length()-1);	    	

				if (printWarningMessages && warning!=null) {
					System.out.println("Racer Message (STDOUT):"+warning.replace('\t','\n'));
					System.out.println(); 
					lastWarning=warning;
				}

				return answer; 

			} else return ":abort";

		} else if (result.charAt(1)=='o') { // warning message	    	

			String warning=null;

			int start=result.indexOf('"',6);

			int end=result.length()-1;

			if (start<end-1) warning=result.substring(start+1,end);

			if (printWarningMessages && warning!=null) {
				System.out.println("Racer Message (STDOUT):"+warning.replace('\t','\n'));
				System.out.println(); 
			}

			return "";

		} else if (result.charAt(1)=='e') { // error message		        

			throw new RacerClientException(result);	

		}

		throw new RacerClientException("Racer returned an unexpected answer: "+result);

	}

	private String simpleParse(String result) throws RacerClientException {

		return result; 

	}

	public void setPrintWarningMessages(boolean printWarningMessages) {

		this.printWarningMessages = printWarningMessages;

	}

	public RacerString RacerStringArgument(String string) {
		return new RacerString("\""+string+"\""); 
	}

	public RacerSymbol RacerSymbolArgument(String string) {
		if (string.indexOf(':') > 0)
			return new RacerSymbol("|"+string+"|"); 
		else 
			return new RacerSymbol(string); 
	}

	public boolean RacerBooleanArgument(boolean bool) {
		return bool;
	}

	public static void main(String[] args) {

		java.util.Locale.setDefault(java.util.Locale.ENGLISH); // for scanner!

	}

}
