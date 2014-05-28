package com.racersystems.jracertest;

import com.racersystems.jracer.RacerClient;

public class RacerLargeFileTest {

	public static void main(String[] args) {

		// please adjust this path to match your environment: 

		String testfile = "\"/home/mi.wessel/oezguer/test.racer\""; 

		String ip = "localhost";
		int port = 8088;

		RacerClient racer = new RacerClient(ip,port);		

		try {

			racer.openConnection();

			racer.loggingOff();
			racer.fullReset();
			System.out.println(racer.racerReadFile(testfile)); 
			System.out.println(racer.allAtomicConcepts()); 


		} catch (Exception e) {

			e.printStackTrace();

		}

	}

}


