;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

(in-package :cl-user)

(open-server-connection)

(enable-lracer-read-macros)

(full-reset)

(logging-on)

(define-prefix "people" "http://cohse.semanticweb.org/ontologies/people")

(define-prefix "oldrdf" "http://www.w3.org/2000/01/rdf-schema")

(pprint 

 (list (owl-read-file (namestring (translate-logical-pathname "lracer:demo;people-pets.owl")))
       
       (all-individuals)

       (all-atomic-concepts)

       (abox-consistent?)
       
       
       (retrieve () (?x top))

       (retrieve (?x ?y) 
		 (and (?x #!people:person)
		      (?x ?y #!people:has_pet)))

       (retrieve () 
		 (and (#!people:Minnie
		       #!people:person)))


       (retrieve () 
		 (and (#!people:Minnie #!people:cat)))


       (describe-query :last)

       (describe-query :last nil)

       (describe-query :last t)

       (retrieve (?x ?y (annotation-datatype-fillers (#!oldrdf:label ?x)))
		 (and (?x #!people:person)
		      (?x ?y #!people:has_pet)))

       (define-concrete-domain-attribute age :type real)

       (instance j (= age 36.5))

       (retrieve (?x (told-value-if-exists (age ?x))) (?x top))

       (tbox-coherent?)
       
       (retrieve (?x ((lambda (x) 123) ?x)) (?x top) :dont-show-lambdas-p nil)
       
       (retrieve (?x ((lambda (x) 123) ?x)) (?x top) :dont-show-lambdas-p t)))



(close-server-connection)
