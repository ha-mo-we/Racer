package com.racersystems.jracertest;
import com.racersystems.jracer.RacerClient;

public class RacerUTF8Test {

	public static void main(String[] args) {

		// please adjust this path to match your environment: 

		// String jFamily = "\"C:/jracer-2-0/demo/family-j-utf8.racer\""; 

		String jFamily = "\"/home/mi.wessel/racer-support/jracer-2-0/demo/family-j-utf8.racer\""; 

		// UTF-8 Test
		// (Note: RacerPro must be running in UTF8 mode for this! 
		// Start as "RacerPro -- -ef @UTF8" 
		// Thanks to Mr. Kuroda Hisao for providing this japanese version
		// of the family.racer KB 

		String ip = "localhost";
		int port = 8088;

		RacerClient racer = new RacerClient(ip,port);		

		try {

			racer.openConnection();

			racer.loggingOn();
			racer.fullReset();
			racer.racerReadFile(jFamily); 
			System.out.println(racer.taxonomy()); 

			racer.fullReset(); 
			racer.inKnowledgeBaseM("å®¶æ—?","éˆ´æœ¨å®¶"); 
			racer.signatureM(":atomic-concepts",
					"(ãƒ’ãƒˆ äººé–“ é›Œ é›„ å¥³ ç”· è¦ª æ¯? çˆ¶ ç¥–æ¯? ä¼¯æ¯? å?”çˆ¶ å§‰å¦¹ å…„å¼Ÿ)",
					":roles", 
					"((å­?å­«ã‚’æŒ?ã?¤ :transitive t) (å­?ä¾›ã‚’æŒ?ã?¤ :parent å­?å­«ã‚’æŒ?ã?¤) ã??ã‚‡ã?†ã? ã?„ã‚’æŒ?ã?¤ (å§‰å¦¹ã‚’æŒ?ã?¤ :parent ã??ã‚‡ã?†ã? ã?„ã‚’æŒ?ã?¤) (å…„å¼Ÿã‚’æŒ?ã?¤ :parent ã??ã‚‡ã?†ã? ã?„ã‚’æŒ?ã?¤) (æ€§ã‚’æŒ?ã?¤ :feature t))",
					":individuals",
					"(ã?¿ã‚ˆ ã?¯ã?ª ã?Ÿã‚?ã?† ã‚†ã?? ã?¨ã‚?)"); 

			racer.impliesM("*top*","(all å­?ä¾›ã‚’æŒ?ã?¤ äººé–“)"); 	    
			racer.impliesM("(some å­?ä¾›ã‚’æŒ?ã?¤ *top*)","è¦ª");	    
			racer.impliesM("(some ã??ã‚‡ã?†ã? ã?„ã‚’æŒ?ã?¤ *top*)","(or å§‰å¦¹ å…„å¼Ÿ)");
			racer.impliesM("*top*","(all ã??ã‚‡ã?†ã? ã?„ã‚’æŒ?ã?¤ (or å§‰å¦¹ å…„å¼Ÿ))"); 
			racer.impliesM("*top*","(all å§‰å¦¹ã‚’æŒ?ã?¤ (some æ€§ã‚’æŒ?ã?¤ é›Œ))"); 
			racer.impliesM("*top*","(all å…„å¼Ÿã‚’æŒ?ã?¤ (some æ€§ã‚’æŒ?ã?¤ é›„))"); 
			racer.impliesM("äººé–“","(and ãƒ’ãƒˆ (some æ€§ã‚’æŒ?ã?¤ (or é›Œ é›„)))");
			racer.disjointM("é›Œ","é›„"); 
			racer.impliesM("å¥³","(and äººé–“ (some æ€§ã‚’æŒ?ã?¤ é›Œ))"); 
			racer.impliesM("ç”·","(and äººé–“ (some æ€§ã‚’æŒ?ã?¤ é›„))"); 
			racer.equivalentM("è¦ª","(and äººé–“ (some å­?ä¾›ã‚’æŒ?ã?¤ äººé–“))"); 
			racer.equivalentM("æ¯?","(and å¥³ è¦ª)"); 
			racer.equivalentM("çˆ¶","(and ç”· è¦ª)"); 
			racer.equivalentM("ç¥–æ¯?","(and æ¯? (some å­?ä¾›ã‚’æŒ?ã?¤ (some å­?ä¾›ã‚’æŒ?ã?¤ äººé–“)))"); 
			racer.equivalentM("ä¼¯æ¯?","(and å¥³ (some ã??ã‚‡ã?†ã? ã?„ã‚’æŒ?ã?¤ è¦ª))"); 
			racer.equivalentM("å?”çˆ¶","(and ç”· (some ã??ã‚‡ã?†ã? ã?„ã‚’æŒ?ã?¤ è¦ª))"); 
			racer.equivalentM("å…„å¼Ÿ","(and ç”· (some ã??ã‚‡ã?†ã? ã?„ã‚’æŒ?ã?¤ äººé–“))"); 
			racer.equivalentM("å§‰å¦¹","(and å¥³ (some ã??ã‚‡ã?†ã? ã?„ã‚’æŒ?ã?¤ äººé–“))"); 
			racer.instanceM("ã?¿ã‚ˆ","æ¯?"); 
			racer.relatedM("ã?¿ã‚ˆ","ã?¯ã?ª","å­?ä¾›ã‚’æŒ?ã?¤"); 
			racer.relatedM("ã?¿ã‚ˆ","ã?Ÿã‚?ã?†","å­?ä¾›ã‚’æŒ?ã?¤"); 
			racer.instanceM("ã?¯ã?ª","æ¯?"); 
			racer.relatedM("ã?¯ã?ª","ã‚†ã??","å­?ä¾›ã‚’æŒ?ã?¤"); 
			racer.relatedM("ã?¯ã?ª","ã?¨ã‚?","å­?ä¾›ã‚’æŒ?ã?¤"); 
			racer.instanceM("ã?Ÿã‚?ã?†","å…„å¼Ÿ"); 
			racer.relatedM("ã?Ÿã‚?ã?†","ã?¯ã?ª", "ã??ã‚‡ã?†ã? ã?„ã‚’æŒ?ã?¤"); 
			racer.instanceM("ã?Ÿã‚?ã?†","(at-most 1 ã??ã‚‡ã?†ã? ã?„ã‚’æŒ?ã?¤)"); 
			racer.relatedM("ã‚†ã??","ã?¨ã‚?","å§‰å¦¹ã‚’æŒ?ã?¤"); 
			racer.relatedM("ã?¨ã‚?","ã‚†ã??","å§‰å¦¹ã‚’æŒ?ã?¤"); 

			System.out.println(racer.taxonomy()); 


		} catch (Exception e) {

			e.printStackTrace();

		}

	}

}


