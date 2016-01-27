Racer
=====

Racer is a knowledge representation system that implements a highly optimized tableau calculus for the description logic SRIQ(D). Racer is provided with a BSD-3 license (see the file LICENSE.txt).

In order to compile and load Racer inside Common Lisp use
Quicklisp or ASDF directly. Make sure ASDF finds the file racer.asd, e.g., with by evaluating something like `(pushnew #P"~/Downloads/racer-master/" asdf:*central-registry*)` and execute `(asdf:load-system "racer")`. With Quicklisp you can use `(ql:quickload "racer")`. 

Racer has been tested with ACL 8.2, CCL 1.9, LW 6.1, and SBCL 1.1.8.
All Racer forms are available from the package racer. See the doc directory for a User's Guide and a Reference Manual. In order to start the Racer server using ACL, CCL, or LW (sorry not for SBCL), just execute `(racer:racer-toplevel)`

? (racer:racer-toplevel)

;;; Welcome to Racer Version 2.0 2014-01-04! 

;;; Racer: Renamed Abox and Concept Expression Reasoner

;;; Supported description logic: SRIQ(D)

;;; Racer comes with ABSOLUTELY NO WARRANTY; use at your own risk. 

;;; The XML/RDF/RDFS/OWL parser is implemented with Wilbur developed

;;; by Ora Lassila. For more information on Wilbur see 

;;; http://wilbur-rdf.sourceforge.net/.


HTTP service enabled for: http://localhost:8080/

HTML documentation at   : http://localhost:8080/reference-toplevel.html

TCP service enabled for : http://localhost:8088/

TCP control enabled for : http://localhost:8089/

You can you your browser with URL [http://localhost:8080/reference-toplevel.html](http://localhost:8080/reference-toplevel.html) in order to access the reference manual online while the Racer server is running.

If you would like to access Racer from the OWLAPI you can find respective code in the directory racer-owlapi.
Additionally, a Protege plugin is provided there (racer-owlapi/trunk/protege). The Protege plugin was tested with Protege 4.2.
