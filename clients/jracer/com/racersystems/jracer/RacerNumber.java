package com.racersystems.jracer;

public class RacerNumber extends RacerLiteral {
    public Number value; 

    public Number getValue() { return value; }

    public RacerNumber(Number value) {
	    this.value=value;
    }

}

