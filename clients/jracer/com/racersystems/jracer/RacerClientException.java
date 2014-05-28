package com.racersystems.jracer;

/**
 * RacerClientException encapsulates all exceptions that may occur 
 * during the interaction of a racer client with a racer server
 * @author AK
 *
 */

public class RacerClientException extends Exception {

	private static final long serialVersionUID = -8084332748232738306L;

	public RacerClientException(String racerError) {

		super(racerError);

	}
    
}


