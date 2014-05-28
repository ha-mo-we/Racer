package com.racersystems.jracer;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class RacerList<E extends RacerResult> extends RacerResult implements Iterable<E> {
	public List<E> value;

	public String toString() {

		StringBuilder res = new StringBuilder(); 

		boolean first = true; 

		if (value.size()>0) {
			for (Object arg : value) {
				if (first)
					res.append("(").append(arg.toString()); 
				else 
					res.append(" ").append(arg.toString()); 
				first=false;
			}
			res.append(")"); 
		}
		else
			res.append("()"); 

		return res.toString(); 
	}

	public List<E> getValue() { return value; }

	public Iterator<E> iterator() { return value.iterator(); }; 

	public RacerList() {
		this.value=new ArrayList<E>();
	}

}




