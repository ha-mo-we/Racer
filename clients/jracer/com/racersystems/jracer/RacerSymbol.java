package com.racersystems.jracer;

public class RacerSymbol extends RacerLiteral {
	public String value; 

	public String getValue() { return value; }

	public RacerSymbol(String value) {
		this.value=value; 
	}
	
}
