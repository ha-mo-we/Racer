The SemWeb demo demonstrates the synergy between RacerPro and AllegroGraph, as
well as some advanced features of RacerPro (e.g., HTML report generation from
query results).  Please ensure that RacerPro is running in unsafe mode, i.e.,
start RacerPro with the command line option "-u" (as "RacerPro -- -u"), or
simply launch RacerPorter from the RacerPro 2.0 directory which automatically
launches RacerPro in unsafe mode.

The demo consists of three files, which need to be processed in the following
order (using RacerPorter): 

1. lubm-gen-example-ts.racer: 
   Generates an AllegroGraph triple store from the LUBM OWL ontology files. 
   Needs to be executed only once. 

2. lubm-prepare.racer: 
   Prepares RacerPro to use the previously generated LUBM TripleStore. 

3. lubm-queries.racer
   Contains the example queries and also demonstrates
   further TripleStore use cases, e.g., inference materialization, native
   Sparql query answering, etc.

Please adjust  

(defpar demodir "examples/semweb/")

to match the directory of your "semweb" folder, e.g., 

(defpar demodir "C:/RacerPro/examples/semweb/")

in all three files. 


