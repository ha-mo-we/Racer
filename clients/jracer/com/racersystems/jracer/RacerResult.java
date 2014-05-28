package com.racersystems.jracer;

public abstract class RacerResult {

    public abstract Object getValue();

    public String toString() {
	return getValue().toString(); 
    }

}

