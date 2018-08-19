;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWL-SYNTAXES; Base: 10 -*-

;;; Copyright (c) 1998-2014, 
;;; Volker Haarslev, Ralf Moeller, Michael Wessel.  
;;; All rights reserved.

;;; Racer is distributed under the following BSD 3-clause license

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are
;;; met:

;;; Redistributions of source code must retain the above copyright notice,
;;; this list of conditions and the following disclaimer.

;;; Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.

;;; Neither the name Racer nor the names of its contributors may be used
;;; to endorse or promote products derived from this software without
;;; specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
;;; BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
;;; FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
;;; VOLKER HAARSLEV, RALF MOELLER, NOR MICHAEL WESSEL BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES, LOSS OF USE, DATA, OR PROFITS, OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
;;; IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :owl-syntaxes)

;;; --------------------------------------------------------------------------------------
;;;
;;;   The Original Software is 
;;;   OntoLisp (NOSA): A Semantic Web Framework for OWL 2 and OWLlink in Common Lisp
;;;
;;;   Copyright (c) 2007-2010 Michael Wessel and Racer Systems GmbH & Co. KG 
;;;   All Rights Reserved.
;;;
;;;   Contributor(s): Michael Wessel  (mailto:michael_wessel@gmx.de
;;;                                    mailto:wessel@racer-systems.com) 
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   Purpose: Provides a renderer / printer for OWL 2 functional syntax.
;;; 

#+:racer-server
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-boolean-readers)
  (wilbur-racer:enable-node-shorthand))

;;;
;;; OWL2 Functional Syntax Renderer
;;; 

(defparameter *converter-mode* nil)

(defvar *ontology* nil)

(defvar *blank-node-id* 0)

(defvar *axioms-rendered* 0)

(defvar *add-prefixes* nil)

(defvar *p4-mode* nil)

(defvar *catch-return* +owlapi-owl-top+)

(defvar *comments* t)

(defvar *functional-to-xml-mode* nil)

(defvar *sexpr-rendering* nil)

;;;
;;;
;;;

(define-constant +rdfs-comment+ 
  #+:racer-server
  !rdfs:comment
  #-:racer-server
  (intern 
   (concatenate 'string 
                "http://www.w3.org/2000/01/rdf-schema#comment")))

(define-constant +rdfs-label+ 
  #+:racer-server
  !rdfs:label
  #-:racer-server
  (intern 
   (concatenate 'string 
                "http://www.w3.org/2000/01/rdf-schema#label")))

(define-constant +rdfs-see-also+ 
  #+:racer-server
  !rdfs:seeAlso
  #-:racer-server
  (intern 
   (concatenate 'string 
                "http://www.w3.org/2000/01/rdf-schema#seeAlso")))


(define-constant +owl-is-defined-by+ 
  #+:racer-server
  !owl:isDefinedBy
  #-:racer-server
  (intern 
   (concatenate 'string 
                "http://www.w3.org/2002/07/owl#isDefinedBy")))


(define-constant +owl-version-info+ 
  #+:racer-server
  !owl:versionInfo
  #-:racer-server
  (intern 
   (concatenate 'string 
                "http://www.w3.org/2002/07/owl#isDefinedBy")))

;;;
;;;
;;;

(define-constant +owl2-standard-prefixes+
    '(("rdf"   "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
      ("rdfs"  "http://www.w3.org/2000/01/rdf-schema#")
      ("xsd"   "http://www.w3.org/2001/XMLSchema#")
      ("owl"   "http://www.w3.org/2002/07/owl#")))

(defparameter *owl2-tags-input-attributes* 
  ;;; 
  (let ((hash (mht)))
    (loop as (key val) in 
          '((|Ontology| ("ontologyIRI" "http://www.w3.org/2002/07/owl#ontologyIRI"))
            (|AnonymousIndividual| ("nodeID" "http://www.w3.org/1999/02/22-rdf-syntax-ns#nodeID"
                                             "http://www.w3.org/2002/07/owl#nodeID"))
            (|OWL***-Literal| ("http://www.w3.org/2002/07/owl#datatypeIRI")))
            
          do
          (setf (gethash key hash) val))
    hash))
          

(defparameter *owl2-tags-input* 
  ;;; 
  (let ((hash (mht)))
    (dolist (key '(|Import|
                   |IRI| ; f. OWLXML
                   |AbbreviatedIRI| ; f. OWLXML
                   |Namespace|
                   |Declaration|
                   |Datatype|
                   ;;;|Literal|
                   |OWL***-Literal|
                   |OWLClass|
                   |Class|
                   |ObjectProperty|
                   |DataProperty|
                   |AnnotationProperty|
                   |SubAnnotationPropertyOf|
                   |AnnotationPropertyDomain|
                   |AnnotationPropertyRange|
                   |InverseObjectProperty|
                   |ObjectInverseOf|
                   |Individual|
                   |NamedIndividual|
                   |AnonymousIndividual|
                   |Ontology|
                   |FacetRestriction| ; !!!!
                   |DataOneOf|
                   |DataComplementOf|
                   |DataIntersectionOf|
                   |DataUnionOf|
                   |DatatypeRestriction|
                   |EntityAnnotation|
                   |AnnotationAssertion|
                   |ObjectUnionOf|
                   |ObjectIntersectionOf|
                   |ObjectComplementOf|
                   |ObjectOneOf|
                   |ObjectAllValuesFrom|
                   |ObjectSomeValuesFrom|
                   |ObjectExistsSelf|
                   |ObjectHasSelf|
                   |ObjectHasValue|
                   |ObjectMaxCardinality|
                   |ObjectMinCardinality|
                   |ObjectExactCardinality|
                   |DataAllValuesFrom|
                   |DataSomeValuesFrom|
                   |DataHasValue|
                   |DataMaxCardinality|
                   |DataMinCardinality|
                   |DataExactCardinality|
                   |SubclassOf|
                   |SubClassOf|
                   |EquivalentClasses|
                   |DisjointClasses|
                   |DisjointUnion|
                   |SubObjectPropertyChain|
                   |ObjectPropertyChain|
                   |SubObjectPropertyOf|
                   |EquivalentObjectProperties|
                   |DisjointObjectProperties|
                   |ObjectPropertyDomain|
                   |ObjectPropertyRange|
                   |InverseObjectProperties|
                   |FunctionalObjectProperty|
                   |InverseFunctionalObjectProperty|
                   |ReflexiveObjectProperty|
                   |IrreflexiveObjectProperty|
                   |SymmetricObjectProperty|
                   |AsymmetricObjectProperty|
                   |TransitiveObjectProperty|
                   |SubDataPropertyOf|
                   |EquivalentDataProperties|
                   |DisjointDataProperties|
                   |DataPropertyDomain|
                   |DataPropertyRange|
                   |FunctionalDataProperty|
                   |HasKey|
                   |DatatypeDefinition|
                   |SameIndividual|
                   |SameIndividuals|
                   |DifferentIndividual|
                   |DifferentIndividuals|
                   |ClassAssertion|
                   |ObjectPropertyAssertion|
                   |NegativeObjectPropertyAssertion|
                   |DataPropertyAssertion|
                   |NegativeDataPropertyAssertion|
                   |Annotation|))

      (setf (gethash key hash) t))

    hash))

;;;
;;;
;;;

(defvar *simplify-remove-owl-entities* t)

(defvar *abbreviate-iris* t)

(defvar *dont-render-tags* nil)

(defvar *dont-render-attributes* nil)

(defvar *ensure-attributes-for-tag* nil)

(defvar *compress-tags-with-prefixes* nil)

;;;
;;;
;;;

(defun iri-owl (iri &optional (separator #\:))
  (let* ((iri 
          (if (symbolp iri)
              (case iri 
                (top    "owl:Thing")
                (bottom "owl:Nothing")
                (otherwise iri))
            iri))
         (iri (format nil "~A" iri))
         (iri (remove-sharp-brackets iri)))

    (labels (
	     #+:ignore
	     (find-in (namespace prefixes)
               (let ((res
                      (or (find namespace 
                                prefixes
                                :test #'string-equal
                                :key #'second)
                          (find namespace 
                                +owl2-standard-prefixes+
                                :test #'string-equal
                                :key #'second))))
                 res)))
      
      (cond ((and *abbreviate-iris* 
                  (and (> (length iri) 3)
                       (or (string-equal "http" (subseq iri 0 4))
                           (string-equal "file" (subseq iri 0 4)))))
	     
             (let* ((found 
                     (or (find-if #'(lambda (prefix)
                                      (let ((res 
                                             (and (second prefix) ; not NIL! 
                                                  (search (second prefix) 
                                                          iri))))
                                        (and res (zerop res))))
                                  *add-prefixes*)
                                  
                         (find-if #'(lambda (prefix)
                                      (let ((res 
                                             (and (second prefix)
                                                  (search (second prefix) 
                                                          iri))))
                                        (and res (zerop res))))
                                  +owl2-standard-prefixes+))))
                        
               (cond (found 

                      (let ((postfix (subseq iri (length (second found))))
                            (prefix (first found)))

                        (if (is-default-prefix-p prefix)
                            (if (string= prefix ":") ; OWL madness
                                (format nil ":~A" postfix)
                              (format nil "~A" postfix))
                         
                          (if (char= separator #\&)
                              (format nil "&~A;~A" prefix postfix)
                            (values (format nil "~A~A~A" prefix separator postfix) 
                                    ;;; second return value t = use abbreviatedIRI for attribute 
                                    t)))))    
                     
                     (t 

                      (let* ((pos (position #\: iri))
			     (prefix 
                              (subseq iri 0 pos)))
			      
                        (if (and pos
				 (not (string-equal prefix "http"))
				 (not (string-equal prefix "file"))
				 (not (string-equal prefix "https")))
			    
                            (if (char= separator #\&)
                                (format nil "&~A;~A" prefix (subseq iri (1+ pos)))
                              (format nil "~A~A~A" 
                                      prefix separator (subseq iri (1+ pos))))

                          iri))))))

            (t

             (let* ((pos (position #\: iri))
                    (prefix
                     (subseq iri 0 pos)))

               ;;; bereits abgekuerzt? dann sicherstellen, 
               ;;; dass abbreviatedIRI verwendet wird! 
               ;;; aber nur, wenn nicht URL... 
              
               (if (and pos
                        (not (string-equal prefix "http"))
                        (not (string-equal prefix "file"))
                        (not (string-equal prefix "https")))
                        
                   (if (char= separator #\&)
                       (values 
                        (format nil "&~A;~A" prefix (subseq iri (1+ pos)))
                        t)

                     (if (is-default-prefix-p prefix)
                         (values
                          (format nil "~A" (subseq iri (1+ pos)))
                          t)
                       (values
                        (format nil "~A~A~A" 
                                prefix separator (subseq iri (1+ pos)))
                        t)))
                
                 iri)))))))

;;;
;;;
;;; 

(defvar *owllink-mode* nil)

(defvar *owl2-tags-disabled* nil)

(defvar *disable-owl2-tags-for* nil)

(defparameter *owl2-tags* 
  (let ((hash (mht)))
    (dolist (key '(|Import|
                   |Namespace|
                   ;;; |Prefix| 
                   |Declaration|
                   ;;; |Datatype| OWLlink Datatype 
                   |Literal|
                   |OWLClass|
                   |Class|
                   |ObjectProperty|
                   |DataProperty|
                   |AnnotationProperty|
                   |SubAnnotationPropertyOf|
                   |AnnotationPropertyDomain|
                   |AnnotationPropertyRange|
                   |InverseObjectProperty|
                   |ObjectInverseOf|
                   |Individual|
                   |NamedIndividual|
                   |AnonymousIndividual|
                   |Ontology|
                   |Facet|
                   |DataOneOf|
                   |DataComplementOf|
                   |DataIntersectionOf|
                   |DataUnionOf|
                   |DatatypeRestriction|
                   |EntityAnnotation|
                   |AnnotationAssertion|
                   |ObjectUnionOf|
                   |ObjectIntersectionOf|
                   |ObjectComplementOf|
                   |ObjectOneOf|
                   |ObjectAllValuesFrom|
                   |ObjectSomeValuesFrom|
                   |ObjectExistsSelf|
                   |ObjectHasSelf|
                   |ObjectHasValue|
                   |ObjectMaxCardinality|
                   |ObjectMinCardinality|
                   |ObjectExactCardinality|
                   |DataAllValuesFrom|
                   |DataSomeValuesFrom|
                   |DataHasValue|
                   |DataMaxCardinality|
                   |DataMinCardinality|
                   |DataExactCardinality|
                   |SubclassOf|
                   |SubClassOf|
                   |EquivalentClasses|
                   |DisjointClasses|
                   |DisjointUnion|
                   |SubObjectPropertyChain|
                   |ObjectPropertyChain|
                   |SubObjectPropertyOf|
                   |EquivalentObjectProperties|
                   |DisjointObjectProperties|
                   |ObjectPropertyDomain|
                   |ObjectPropertyRange|
                   |InverseObjectProperties|
                   |FunctionalObjectProperty|
                   |InverseFunctionalObjectProperty|
                   |ReflexiveObjectProperty|
                   |IrreflexiveObjectProperty|
                   |SymmetricObjectProperty|
                   |AsymmetricObjectProperty|
                   |TransitiveObjectProperty|
                   |SubDataPropertyOf|
                   |EquivalentDataProperties|
                   |DisjointDataProperties|
                   |DataPropertyDomain|
                   |DataPropertyRange|
                   |FunctionalDataProperty|
                   |HasKey|
                   |DatatypeDefinition|
                   |SameIndividual|
                   |SameIndividuals|
                   |DifferentIndividual|
                   |DifferentIndividuals|
                   |ClassAssertion|
                   |ObjectPropertyAssertion|
                   |NegativeObjectPropertyAssertion|
                   |DataPropertyAssertion|
                   |NegativeDataPropertyAssertion|
                   |Annotation|))

      (setf (gethash key hash) t))

    hash))

;;;
;;;
;;;

(defun toplevel-indent-p  ()
  (or (= *indent* 0)
      (= *indent* 2)))

(defmacro newline1 (stream)
  `(progn 
     (format ,stream "~%")
     (dotimes (i *indent*) (format ,stream " "))))

(defmacro tag-printer (stream tag &body cont)
  `(let ((tag ,(cond ((stringp tag)
                      tag)
                     ((symbolp tag)
                      (symbol-name tag))
                     (t
                      (format nil "~A" tag)))))

     (if (member tag *dont-render-tags*)

         (progn 
           ,@cont)

       (progn

         (when (toplevel-indent-p)
           (newline1 stream))
     
         (newline1 stream)
     
         (format ,stream "~A(" tag)

         (let* ((*indent* (+ 2 *indent*)))
           ,@cont)
     
         ;; (newline1 stream)
     
         (format ,stream ")")))))

(defmacro tag-printer1 (stream tag &body cont)
  `(progn 

     (if (member ,tag *dont-render-tags*)

         (progn 
           ,@cont)

       (progn 
         (when (toplevel-indent-p)
           (newline1 stream))
       
         (newline1 stream)
       
         (format ,stream "~A(" ,tag)
       
         (let* ((*indent* (+ 2 *indent*)))
           ,@cont)
     
         ;; (newline1 stream)
     
         (format ,stream ")")))))

(defmacro item-printer (stream item &optional (space-p t))
  `(progn 
     ,(if space-p 
          `(format ,stream "~A " ,item)
        `(format ,stream "~A" ,item))))

(defmacro string-printer (stream item &optional (space-p t))
  `(progn 
     ,(if space-p 
          `(format ,stream "\"~A\" " ,item)
        `(format ,stream "\"~A\"" ,item))))

(defun iri-printer (stream iri)
  (let ((iri
         (iri-owl iri)))
    (if (and (> (length iri) 4)
             (or (string-equal "http:" (subseq iri 0 5))
                 (string-equal "file:" (subseq iri 0 5))))
        (if *sexpr-rendering*
            (item-printer stream (format nil "|~A|" iri))
          (item-printer stream (format nil "<~A>" iri)))
      (if (and *sexpr-rendering*
               (needs-bars-p iri))
          (item-printer stream (format nil "|~A|" iri))
        (item-printer stream iri)))))

(defun needs-bars-p (string)
  (find-if #'(lambda (x)
	       (or (whitespace-char-p x)
		   (not (alpha-char-p x))))
	   string))

(defun compress-functional-tag-name (name1 &optional (separator #\.))
  (if (symbolp name1)

      (let* ((name (symbol-name name1))
             (pos (position #\# name :from-end t))
             (pos1 (position #\: name)))
        
        (cond ((and pos *compress-tags-with-prefixes*)
               (let* ((ns (subseq name 0 (1+ pos)))
                      (prefix 
                       (first
                        (find ns *compress-tags-with-prefixes*
                              :key #'second :test #'string=))))
                 (if prefix
                     (intern 
                      (format nil "~A~A~A" 
                              prefix separator (subseq name (1+ pos))))
                   (let ((prefix 
                          (subseq name 0 pos1)))
                     (if (or (string-equal "http" prefix)
                             (string-equal "ftp" prefix)
                             (string-equal "file" prefix))
                         (if *sexpr-rendering*
                             (intern (format nil "|~A|" name1))
                           (intern (format nil "<~A>" name1)))
                       name1)))))

              (pos1 
               (let ((prefix 
                      (subseq name 0 pos1)))

                 (if (is-default-prefix-p prefix)
                     (intern 
                      (format nil "~A" (subseq name (1+ pos1))))
                   (if (or (string-equal "http" prefix)
                           (string-equal "ftp" prefix)
                           (string-equal "file" prefix))
                       (intern (format nil "<~A>" name1))
                     name1))))

              (t name1)))
    name1))

(defvar *tag-stack* nil)

(defun func-printer (stream s-expr)

  (cond ((consp s-expr) 
         
         (let* ((tag (first s-expr))
                (attributes (when *owllink-mode* 
                              (second s-expr)))
                (rest (if *owllink-mode*
                          (cddr s-expr)
                        (cdr s-expr))))

           (cond ((eq tag :literal) ; owl functional export (s. trans-literal)

                  (if (second rest)
                      (format stream "~S^^~A" 
                              (first rest) 
                              (iri-printer nil (second (second rest))))
                    (format stream "~S^^xsd:string" 
                            (first rest))))
                 
                 ((and *owllink-mode* 
                       (or (eq tag '|OWLLiteral|)
                           ;;; not an OWLlink Literal 
                           (eq tag '|Literal|))
                       rest
                       (consp (first rest)))

                  (let ((val (second (first rest)))
                        (type (second (third (first rest)))))
                    
                    (if type
                        (format stream "~S^^~A" 
                                val
                                (iri-printer nil type))
                      (format stream "~S^^xsd:string" 
                              val))))

                 ((and *owllink-mode* 
                       (eq tag '|OWLLiteral|))

                  (let ((type (first rest))
                        (val (second rest)))
                    
                    (newline1 stream)

                    (if type
                        (format stream "~S^^~A" 
                                val
                                (iri-printer nil type))
                      (format stream "~S^^xsd:string" 
                              val))))

                 ((and (eq tag '|Attribute|)
                       (member (first rest) *dont-render-attributes*)
                       (not (member (first rest) 
                                    (second (assoc tag *ensure-attributes-for-tag*))))))
                 
                 (t 

                  (let* ((*owl2-tags-disabled* 
                          (when *owllink-mode*
                            (or *owl2-tags-disabled* 
                                (member tag *disable-owl2-tags-for*))))

                         (is-owl2-tag-p 
                          (when *owllink-mode*
                            (and (not *owl2-tags-disabled*)
                                 (gethash tag *owl2-tags*))))

                         
                         (*abbreviate-iris*
                          (if (not *owllink-mode*)
                              *abbreviate-iris*
                            (and *owllink-mode*
                                 (let ((res (assoc '|INT-abbreviatesIRIs| attributes)))
                                   (if res
                                       (string-to-boolean
                                        (second res))
                                     *abbreviate-iris*)))))
                         
                         (*add-prefixes*
                          (if (not *owllink-mode*)
                              *add-prefixes*
                            (or (when *abbreviate-iris*
                                  (let ((res 
                                         (assoc '|INT-prefixes| attributes))) ; ueberschreiben stets!
                                    (if res 
                                        (second res)
                                      *add-prefixes*)))
                                *add-prefixes*)))

                         (tag
                          (if is-owl2-tag-p 
                              (intern (format nil "owl.~A" tag))
                            (compress-functional-tag-name tag)))

                         (*tag-stack* (cons tag *tag-stack*)))
           
                    (cond ((and *simplify-remove-owl-entities*
                                (not (eq (second *tag-stack*) '|owl.Declaration|))
                                (member tag
                                        '(|owl.Class| 
                                          |owl.NamedIndividual|
                                          |owl.AnonymousIndividual|
                                          |owl.ObjectProperty|
                                          |owl.DataProperty|
                                          |owl.AnnotationProperty|)))
                           (newline1 stream)
                           (iri-printer stream (first rest)))

                          ((and 
                            *converter-mode*
                            (stringp (first rest))
                            (member tag
                                    '(|owl.Class| 
                                      |owl.NamedIndividual|
                                      |owl.AnonymousIndividual|
                                      |owl.ObjectProperty|
                                      |owl.DataProperty|
                                      |owl.AnnotationProperty|)))
                           (newline1 stream)
                           (tag-printer1 stream tag
                             (iri-printer stream (first rest))))

                          (t

                           (tag-printer1 stream tag
                             (progn 
                               (dolist (s-expr attributes)
                                 (let ((*owllink-mode* nil))
                                   (func-printer stream
                                                 `(|Attribute| ,@s-expr))))
                               (dolist (s-expr rest)
                                 (func-printer stream s-expr)))))))))))
        
        ((symbolp s-expr)
         (newline1 stream)
         (iri-printer stream s-expr))
        
        ((stringp s-expr)
         (newline1 stream)
	 (if (and (> (length s-expr) 4)
		  (or (string-equal "http:" (subseq s-expr 0 5))
		      (string-equal "file:" (subseq s-expr 0 5))))
	     (item-printer stream (format nil "<~A>" s-expr))
	   (string-printer stream s-expr)))

        (t
         (newline1 stream)
         (item-printer stream s-expr))))

;;;
;;;
;;; 

(defun trans-data-range (expr)
  (cond ((symbolp expr)
         ;;; datatypeURI
         (if *functional-to-xml-mode*
             `(|Datatype| ,expr)
           expr))

        ((consp expr)
         
         (case (first expr)

           (d-literal
            (trans-literal expr))
                        
           (d-base-type
            (trans-data-range (second expr)))

           (d-complement
            `(|DataComplementOf| 
              ,(trans-data-range (second expr))))

           (d-and 
            `(|DataIntersectionOf| 
              ,@(mapcar #'trans-data-range (rest expr))))
                        
           (d-or 
            `(|DataUnionOf| 
              ,@(mapcar #'trans-data-range (rest expr))))

           (d-possible-values
            `(|DataOneOf|
              ,@(mapcar #'trans-data-range (rest expr))))
           
            (d-restriction
            (let* ((basetype (second expr))
                   (facets-and-values (cddr expr))
                   (facets-and-values
                    (apply #'append
                           (loop as facet in facets-and-values 
                                 collect (list (trans-facet (second facet))
                                               (trans-literal (third facet)))))))
                           
              `(|DatatypeRestriction| ,(trans-data-range basetype)
                                      ,@facets-and-values)))))))

(defun trans-facet (facet)
  facet)

(defun trans-ind (ind)
  (if *functional-to-xml-mode*
      `(|NamedIndividual| ,ind)
    ind))

(defun trans-cardinality (n)
  (if *functional-to-xml-mode*
      `(|Cardinality| ,n)
    n))

(defun trans-role (role)
  (if (consp role)
      (if (eq (first role) 'inv)
          (if *p4-mode*
              (progn 
                (owlapi-warning "In P4 compatibility mode - cannot render ObjectInverseOf, skipping")
                (throw :cant-render *catch-return*))
            `(|ObjectInverseOf| ,(trans-role (second role))))
        ;;; property chain
        `(|ObjectPropertyChain| ,@(mapcar #'trans-role role)))
    (if *functional-to-xml-mode* 
        `(|ObjectProperty| ,role)
      role)))

(defun trans-annotation-role (role)
  (if *functional-to-xml-mode* 
      `(|AnnotationProperty| ,role)
    role))

(defun trans-data-role (role)
  (if *functional-to-xml-mode* 
      `(|DataProperty| ,role)
    role))


(defun trans-concept (concept)
  (cond ((symbolp concept)
         (if *functional-to-xml-mode* 
             `(|Class| ,concept)
           concept))
	
        ((stringp concept)
         (trans-concept 
          (intern concept)))

        ((consp concept)
              
         (case (first concept)

           (not
            `(|ObjectComplementOf| ,(trans-concept (second concept))))

           (and
            (if (and *p4-mode* (not (rest concept)))
                +owlapi-owl-top+
              `(|ObjectIntersectionOf| ,@(mapcar #'trans-concept (rest concept)))))

           (or
            (if (and *p4-mode* (not (rest concept)))
                +owlapi-owl-bottom+
              `(|ObjectUnionOf| ,@(mapcar #'trans-concept (rest concept)))))
                
           (some 
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                `(|ObjectSomeValuesFrom| ,(trans-role (second concept))
                                         ,(trans-concept (third concept))))))
                
           (all
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                `(|ObjectAllValuesFrom| ,(trans-role (second concept))
                                        ,(trans-concept (third concept))))))
                
           (at-least 
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                `(|ObjectMinCardinality| ,(trans-cardinality (second concept))
                                         ,(trans-role (third concept))
                                         ,@(when (fourth concept)
                                             (list (trans-concept (fourth concept))))))))
                
           (at-most 
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                `(|ObjectMaxCardinality| ,(trans-cardinality (second concept))
                                         ,(trans-role (third concept))
                                         ,@(when (fourth concept)
                                             (list (trans-concept (fourth concept))))))))
                                
           (exactly 
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                `(|ObjectExactCardinality| ,(trans-cardinality (second concept))
                                           ,(trans-role (third concept))
                                           ,@(when (fourth concept)
                                               (list (trans-concept (fourth concept))))))))
                
           (one-of
            (if (and *p4-mode* (not (rest concept)))
                +owlapi-owl-bottom+
              `(|ObjectOneOf| ,@(mapcar #'trans-ind (rest concept)))))

           (has-value
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                `(|ObjectHasValue| ,(trans-role (second concept))
                                   ,(trans-ind (third concept))))))

           (self-reference
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                `(|ObjectHasSelf| ,(trans-role (second concept))))))
           
           (d-some
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                `(|DataSomeValuesFrom| ,(trans-data-role (second concept))
                                       ,(trans-concept (third concept))))))
                
           (d-all
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                `(|DataAllValuesFrom| ,(trans-data-role (second concept))
                                      ,(trans-concept (third concept))))))
                
           (d-at-least 
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                `(|DataMinCardinality| ,(trans-cardinality (second concept))
                                       ,@(mapcar #'(lambda (role)
                                                     (trans-data-role role))
                                                 (butlast (cddr concept)))
                                       ,(trans-data-range (first (last concept)))))))
                
           (d-at-most
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                `(|DataMaxCardinality| ,(trans-cardinality (second concept))
                                       ,@(mapcar #'(lambda (role)
                                                     (trans-data-role role))
                                                 (butlast (cddr concept)))
                                       ,(trans-data-range (first (last concept)))))))
                                
           (d-exactly 
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                `(|DataExactCardinality| ,(trans-cardinality (second concept))
                                         ,@(mapcar #'(lambda (role)
                                                       (trans-data-role role))
                                                   (butlast (cddr concept)))
                                         ,(trans-data-range (first (last concept)))))))

           (d-filler
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                `(|DataHasValue| ,(trans-data-role (second concept))
                                 ,(trans-literal (third concept))))))

           (d-restriction
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                (trans-data-range concept))))

           (otherwise
            (let ((*catch-return* +owlapi-owl-top+))
              (catch :cant-render
                (trans-data-range concept))))))))


(defun trans-literal (lit)
  (if (consp lit)
      (let ((val (second lit))
            (type (third lit)))
        (declare (ignorable type))
        (if type
            `(:literal ,val ,type)
          `(:literal ,val)))
    `(:literal ,lit)))

;;;
;;;
;;; 

(defun func-comment (stream comment)
  (format stream "// ~A" comment))

#+:racer-server
(defun func-author-comment (name stream)
  (func-comment stream (format nil "Ontology \"~A\" rendered by ~A Version ~A Build ~A"
                              name
                              (get-product-name)
                              (get-product-version)
                              (get-build-version))))


#-:racer-server
(defun func-author-comment (name stream)
  (func-comment stream (format nil "Ontology \"~A\" rendered by the Lisp-OWLAPI"
                              name)))
        

(defun func-block-comment (stream comment)
  (let* ((expr (with-output-to-string (stream2)
                 (pprint comment stream2)))
         (expr1 (cdr (coerce expr 'list)))) ; remove first newline

    (loop while expr1 do
          (let* ((pos (position #\newline expr1)))
            (if pos
                (let ((expr2 (subseq expr1 0 pos))
                      (expr3 (subseq expr1 (1+ pos))))
                  (format stream "~%// ~A" (coerce expr2 'string))
                  (setf expr1 expr3))
              (progn 
                (format stream "~%// ~A" (coerce expr1 'string))
                (setf expr1 nil))))))

  nil)

;;;
;;;
;;; 

(defmethod render-literal ((stream stream) lit (syntax (eql :owl-functional)))
  (func-printer stream 
                (trans-literal (third lit))))

(defmethod render-annotation ((stream stream) annotation (syntax (eql :owl-functional))
                              &key axiom &allow-other-keys)

  (tag-printer stream |Annotation|

    (when axiom
      (render-axiom-annotations stream axiom syntax))
      
    (func-printer stream (trans-annotation-role (second annotation)))

    (render-literal stream annotation syntax)))

(defmethod render-axiom-annotations ((stream stream) axiom (syntax (eql :owl-functional)))
  (dolist (axiom (get-annotation-axioms-for-axiom axiom))
    (render-annotation stream (annotation axiom) syntax :axiom axiom)))

;;;
;;;
;;;

(defmethod render :around ((stream stream) (axiom |OWLAxiom|) (syntax (eql :owl-functional)))

  (incf *axioms-rendered*)

  (when *comments*
    (terpri stream)
    (terpri stream)

    (func-comment stream (format nil "Axiom ID ~A"
                                 (axiom-id axiom)))
    
    (terpri stream)
    
    (func-block-comment stream (cons (type-of axiom) 
                                     (butlast (cdr (told axiom))))))

  (call-next-method))

(defmethod render ((stream stream) (axiom |OWLAxiom|) (syntax (eql :owl-functional)))

  (decf *axioms-rendered*)
  
  (owlapi-warning "Warning - cannot render axioms of type ~A in syntax ~A"
                 (type-of axiom) 
                 syntax)
  nil)


(defmethod render ((stream stream) (axiom |OWLOntologyVersionDeclarationAxiom|) (syntax (eql :owl-functional)))
  (func-printer stream (uri axiom))
  t)

(defmethod render ((stream stream) (axiom |OWLImportsDeclarationAxiom|) (syntax (eql :owl-functional)))
  (tag-printer stream |Import| 
    (iri-printer stream (uri axiom)))
  t)

;;;
;;;
;;;

(defmethod render ((stream stream) (axiom |OWLSubClassAxiom|) (syntax (eql :owl-functional)))
  (tag-printer stream |SubClassOf|
    (render-axiom-annotations stream axiom syntax)
    (func-printer stream (trans-concept (first (descriptions axiom))))
    (func-printer stream (trans-concept (second (descriptions axiom)))))
  t)

(defmethod render ((stream stream) (axiom |OWLEquivalentClassesAxiom|) (syntax (eql :owl-functional)))
  (tag-printer stream |EquivalentClasses|
    (render-axiom-annotations stream axiom syntax)
    (dolist (class (descriptions axiom))
      (func-printer stream (trans-concept class))))
  t)

(defmethod render ((stream stream) (axiom |OWLDisjointClassesAxiom|) (syntax (eql :owl-functional)))
  (tag-printer stream |DisjointClasses|
    (render-axiom-annotations stream axiom syntax)
    (dolist (class (descriptions axiom))
      (func-printer stream (trans-concept class))))
  t)

(defmethod render ((stream stream) (axiom |OWLDisjointUnionAxiom|) (syntax (eql :owl-functional)))
  (tag-printer stream |DisjointUnion|
    (render-axiom-annotations stream axiom syntax)
    (dolist (class (descriptions axiom))
      (func-printer stream (trans-concept class))))
  t)

;;;
;;;
;;;

(defmacro with-p4-ignored-axiom (&body body)
  `(catch :cant-render
     ,@body))

(defmethod render ((stream stream) (axiom |OWLObjectSubPropertyAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-role (sub-object-property axiom)))
          (r2 (trans-role (object-property axiom))))
      (tag-printer stream |SubObjectPropertyOf|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream r2))))
  t)

(defmethod render ((stream stream) (axiom |OWLObjectPropertyChainSubPropertyAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-role (object-property-chain axiom)))
          (r2 (trans-role (object-property axiom))))
    (tag-printer stream |SubObjectPropertyOf|
      (render-axiom-annotations stream axiom syntax)
      (func-printer stream r1)
      (func-printer stream r2))))
  t)

(defmethod render ((stream stream) (axiom |OWLEquivalentObjectPropertiesAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (mapcar #'trans-role (object-properties axiom))))
      (tag-printer stream |EquivalentObjectProperties|
        (render-axiom-annotations stream axiom syntax)
        (dolist (prop r1)
          (func-printer stream prop)))))
  t)

(defmethod render ((stream stream) (axiom |OWLDisjointObjectPropertiesAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (mapcar #'trans-role (object-properties axiom))))
      (tag-printer stream |DisjointObjectProperties|
        (render-axiom-annotations stream axiom syntax)
        (dolist (prop r1)
          (func-printer stream prop)))))
  t)

(defmethod render ((stream stream) (axiom |OWLObjectPropertyDomainAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-role (object-property axiom))))
      (tag-printer stream |ObjectPropertyDomain|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream (trans-concept (object-property-domain axiom))))))
  t)

(defmethod render ((stream stream) (axiom |OWLObjectPropertyRangeAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-role (object-property axiom))))
      (tag-printer stream |ObjectPropertyRange|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream (trans-concept (object-property-range axiom))))))
  t)

(defmethod render ((stream stream) (axiom |OWLInverseObjectPropertiesAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (mapcar #'trans-role (object-properties axiom))))
      (tag-printer stream |InverseObjectProperties|
        (render-axiom-annotations stream axiom syntax)
        (dolist (prop r1)
          (func-printer stream prop)))))
  t)

(defmethod render ((stream stream) (axiom |OWLFunctionalObjectPropertyAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-role (object-property axiom))))
      (tag-printer stream |FunctionalObjectProperty|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1))))
  t)

(defmethod render ((stream stream) (axiom |OWLInverseFunctionalObjectPropertyAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-role (object-property axiom))))
      (tag-printer stream |InverseFunctionalObjectProperty|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1))))
  t)

(defmethod render ((stream stream) (axiom |OWLReflexiveObjectPropertyAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-role (object-property axiom))))
      (tag-printer stream |ReflexiveObjectProperty|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1))))
  t)

(defmethod render ((stream stream) (axiom |OWLIrreflexiveObjectPropertyAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-role (object-property axiom))))
      (tag-printer stream |IrreflexiveObjectProperty|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1))))
  t)

(defmethod render ((stream stream) (axiom |OWLSymmetricObjectPropertyAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-role (object-property axiom))))
      (tag-printer stream |SymmetricObjectProperty|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1))))
  t)

(defmethod render ((stream stream) (axiom |OWLAsymmetricObjectPropertyAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-role (object-property axiom))))
      (tag-printer stream |AsymmetricObjectProperty|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1))))
  t)

(defmethod render ((stream stream) (axiom |OWLTransitiveObjectPropertyAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-role (object-property axiom))))
      (tag-printer stream |TransitiveObjectProperty|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1))))
  t)

;;;
;;;
;;;

(defmethod render ((stream stream) (axiom |OWLDataSubPropertyAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-data-role (sub-data-property axiom)))
          (r2 (trans-data-role (data-property axiom))))
      (tag-printer stream |SubDataPropertyOf|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream r2))))
  t)

(defmethod render ((stream stream) (axiom |OWLEquivalentDataPropertiesAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (mapcar #'trans-data-role (data-properties axiom))))
      (tag-printer stream |EquivalentDataProperties|
        (render-axiom-annotations stream axiom syntax)
        (dolist (prop r1)
          (func-printer stream prop)))))
  t)

(defmethod render ((stream stream) (axiom |OWLDisjointDataPropertiesAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (mapcar #'trans-data-role (data-properties axiom))))
      (tag-printer stream |DisjointDataProperties|
        (render-axiom-annotations stream axiom syntax)
        (dolist (prop r1)
          (func-printer stream prop)))))
  t)

(defmethod render ((stream stream) (axiom |OWLDataPropertyDomainAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-data-role (data-property axiom))))
      (tag-printer stream |DataPropertyDomain|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream (trans-concept (data-property-domain axiom))))))
  t)

(defmethod render ((stream stream) (axiom |OWLDataPropertyRangeAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-data-role (data-property axiom))))
      (tag-printer stream |DataPropertyRange|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream (trans-data-range (data-property-range axiom))))))
  t)

(defmethod render ((stream stream) (axiom |OWLFunctionalDataPropertyAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-data-role (data-property axiom))))
      (tag-printer stream |FunctionalDataProperty|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1))))
  t)

(defmethod render ((stream stream) (axiom |OWLDatatypeDefinitionAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-data-range (datatype-name axiom))))
      (tag-printer stream |DatatypeDefinition|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream (trans-data-range (data-range axiom))))))
  t)

;;;
;;;
;;;

(defmethod render ((stream stream) (axiom |OWLHasKeyAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (mapcar #'trans-role (key-object-properties axiom)))
          (r2 (mapcar #'trans-data-role (key-data-properties axiom))))
      (tag-printer stream |HasKey|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream (trans-concept (key-class axiom)))
        (newline1 stream)
        (if r1
            (format stream " ~A" (mapcar #'(lambda (x) (iri-printer nil x)) r1))
          (format stream " ()"))
        (newline1 stream)
        (if r2
            (format stream " ~A" (mapcar #'(lambda (x) (iri-printer nil x)) r2))
          (format stream " ()"))
        (newline1 stream))))
  t)

;;;
;;;
;;;

(defmethod render ((stream stream) (axiom |OWLClassAssertionAxiom|) (syntax (eql :owl-functional)))
  (tag-printer stream |ClassAssertion|
    (render-axiom-annotations stream axiom syntax)
    (cond (*p4-mode* 
           (func-printer stream (trans-ind (ax-individual axiom)))
           (func-printer stream (trans-concept (description axiom))))
          (t
           (func-printer stream (trans-concept (description axiom)))
           (func-printer stream (trans-ind (ax-individual axiom))))))
  t)

(defmethod render ((stream stream) (axiom |OWLObjectPropertyAssertionAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-role (rel-object-property axiom))))
      (tag-printer stream |ObjectPropertyAssertion|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream (trans-ind (subject axiom)))
        (func-printer stream (trans-ind (object axiom))))))
  t)

(defmethod render ((stream stream) (axiom |OWLNegativeObjectPropertyAssertionAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-role (rel-object-property axiom))))
      (tag-printer stream |NegativeObjectPropertyAssertion|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream (trans-ind (subject axiom)))
        (func-printer stream (trans-ind (object axiom))))))
  t)
  
(defmethod render ((stream stream) (axiom |OWLSameIndividualsAxiom|) (syntax (eql :owl-functional)))
  (tag-printer stream |SameIndividual| ; |SameIndividuals| ?
    (render-axiom-annotations stream axiom syntax)
    (dolist (ind (individuals axiom))
      (func-printer stream (trans-ind ind))))
  t)

(defmethod render ((stream stream) (axiom |OWLDifferentIndividualsAxiom|) (syntax (eql :owl-functional)))
  (tag-printer stream |DifferentIndividuals| 
    (render-axiom-annotations stream axiom syntax)
    (dolist (ind (individuals axiom))
      (func-printer stream (trans-ind ind))))
  t)

;;;
;;;
;;;   

(defmethod render ((stream stream) (axiom |OWLDataPropertyAssertionAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-data-role (rel-data-property axiom))))
      (tag-printer stream |DataPropertyAssertion|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream (trans-ind (subject axiom)))
        (func-printer stream (trans-literal (data-literal axiom))))))
  t)

(defmethod render ((stream stream) (axiom |OWLNegativeDataPropertyAssertionAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-data-role (rel-data-property axiom))))
      (tag-printer stream |NegativeDataPropertyAssertion|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream (trans-ind (subject axiom)))
        (func-printer stream (trans-literal (data-literal axiom))))))
  t)

;;;
;;;
;;;

(defmethod render ((stream stream) (axiom |OWLDeclarationAxiom|) (syntax (eql :owl-functional)))
  (tag-printer stream |Declaration|
    (render-axiom-annotations stream axiom syntax)
    (func-printer stream (entity axiom)))
  t)

(defmethod render ((stream stream) (axiom |OWLImplicitDeclarationAxiom|) (syntax (eql :owl-functional)))
  nil)

;;;
;;;
;;;

(defmethod render ((stream stream) (axiom |OWLAxiomAnnotationAxiom|) (syntax (eql :owl-functional)))
  nil)

;;;
;;; Old EntityAnnotationAxiom
;;; 

(defmethod render ((stream stream) (axiom |OWLEntityAnnotationAxiom|) (syntax (eql :owl-functional)))
  (dolist (entity (ensure-list (first (entity axiom))))
    (let ((entity (cons entity (cdr (entity axiom)))))
      (tag-printer stream |EntityAnnotation|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream entity)
        (func-printer stream 
                      `(,(let ((sym (intern (ensure-string (second (annotation axiom))))))
                           (cond ((eq sym +rdfs-comment+)
                                  '|Comment|)
                                 ((eq sym +rdfs-label+)
                                  '|Label|)
                                 ((eq sym +rdfs-see-also+)
                                  '|seeAlso|)
                                 ((eq sym +owl-is-defined-by+)
                                  '|isDefinedBy|)
                                 ((eq sym +owl-version-info+)
                                  '|priorVersion|)
                                 (t
                                  (if *p4-mode*
                                      '|Comment|
                                    (intern (ensure-string (second (annotation axiom))))))))
                        ,(format nil "~A" (second (third (annotation axiom)))))))))
  t)

;;;
;;;
;;; 

(defmethod render ((stream stream) (axiom |OWLOntologyAnnotationAxiom|) (syntax (eql :owl-functional)))
  (render-annotation stream (annotation axiom)
                     syntax
                     :axiom axiom)
  t)

(defmethod render ((stream stream) (axiom |OWLAnnotationAssertionAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-annotation-role (annotation-property axiom))))
      (tag-printer stream |AnnotationAssertion|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream (trans-ind (annotation-subject axiom)))
        (func-printer stream
                      (if (and (consp (annotation-value axiom))
                               (member (first (annotation-value axiom))
                                       '(d-literal)))
                          (trans-literal (annotation-value axiom))
                        (annotation-value axiom))))))
  t)

(defmethod render ((stream stream) (axiom |OWLAnnotationPropertyRangeAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-annotation-role (annotation-property2 axiom))))
      (tag-printer stream |AnnotationPropertyRange|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream (trans-concept (annotation-property-range axiom))))))
  t)

(defmethod render ((stream stream) (axiom |OWLAnnotationPropertyDomainAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-annotation-role (annotation-property1 axiom))))
      (tag-printer stream |AnnotationPropertyDomain|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream (trans-concept (annotation-property-domain axiom))))))
  t)

(defmethod render ((stream stream) (axiom |OWLSubAnnotationPropertyOfAxiom|) (syntax (eql :owl-functional)))
  (with-p4-ignored-axiom
    (let ((r1 (trans-role (annotation-sub-property axiom)))
          (r2 (trans-role (annotation-super-property axiom))))
      (tag-printer stream |SubAnnotationPropertyOf|
        (render-axiom-annotations stream axiom syntax)
        (func-printer stream r1)
        (func-printer stream r2))))
  t)


;;;
;;; 
;;;

(defmacro with-prefixes ((ont var default) &body body)
  `(let* ((,default 
           (or (expand-prefix :defaultnamespace)
               (reasoner-get-default-namespace-prefix 
                (owlapi-tbox *cur-reasoner*))))

          (,var 
           (remove-if #'(lambda (x) 
                          (string-equal (first x) "xml"))
                      (mapcar #'(lambda (x) 
                                  (list (ensure-string (first x))
                                        (ensure-string (second x))))
                 
                              (append 
                   
                               *add-prefixes*
          
                               (reasoner-get-prefixes (owlapi-tbox *cur-reasoner*) nil)

                               (mapcar #'(lambda (axiom)
                                           (list (without-colon
                                                  (prefix axiom))
                                                 (uri axiom)))
                               
                                       (get-axioms-of-type-for 
                                        (axioms ,ont)
                                        '|OWLPrefixDeclarationAxiom|))))))
        
          (,var
           (remove-duplicates 
            ,var
            :test #'(lambda (x y)
                      (and (string-equal (first x) (first y))
                           (string-equal (second x) (second y)))))))

     ,@body))

(defmethod render ((stream stream) (ont ontology) (syntax (eql :owl-functional)))
  (let* ((*ontology* ont)
         (*axioms-rendered* 0)
         (all-axioms nil))

    (with-prefixes (ont prefixes default)

      (with-slots (name axioms) ont
      
        (func-author-comment name stream)        

        
        (loop as (prefix namespace) in 
              
              (cons (list nil default)
                    (remove-if #'is-default-prefix-p prefixes :key #'first))
              
              do
              
              (when namespace
                (tag-printer1 stream 
                    (if *p4-mode* 
                        '|Namespace| 
                      '|Prefix|)
                  (if (is-default-prefix-p prefix)
                      (format stream ":=<~A>" namespace)
                    (format stream "~A:=<~A>" prefix namespace)))))

        (let ((*add-prefixes* 
               (cons (list ":" default)
                     (remove-if #'is-default-prefix-p prefixes :key #'first))))

          (tag-printer stream |Ontology| 

            (iri-printer stream (make-url-from-filename name))

            (dolist (type '(|OWLOntologyVersionDeclarationAxiom|
                            |OWLImportsDeclarationAxiom|
                            |OWLOntologyAnnotationAxiom|

                            #|
                        (not (or |OWLOntologyVersionDeclarationAxiom|
                                 |OWLImportsDeclarationAxiom|
                                 |OWLOntologyAnnotationAxiom|
                                 |OWLPrefixDeclarationAxiom|)) |# 

                            |OWLDeclarationAxiom|
                            |OWLDatatypeDefinitionAxiom|
                            |OWLClassAxiom|
                            |OWLPropertyAxiom|
                            |OWLIndividualAxiom|
                            |OWLHasKeyAxiom|
                            (and |OWLAnnotationAxiom| 
                                 (not |OWLOntologyAnnotationAxiom|)
                                 (not |OWLAxiomAnnotationAxiom|))))
          
              (dolist (axiom 
                       (get-axioms-of-type-for axioms type))

                (push axiom all-axioms)
                (render stream axiom syntax))))

          ;;; (pprint (set-difference axioms all-axioms))
          
          (newline1 stream)

          (newline1 stream)
      
          (func-author-comment name stream)        

          #+:ignore
          (let ((axioms (remove-if #'(lambda (x) 
                                       (or (typep x '|OWLAxiomAnnotationAxiom|)
                                           (typep x '|OWLOntologyAnnotationAxiom|)
                                           (typep x '|OWLPrefixDeclarationAxiom|)))
                                   axioms)))
            (when (set-difference axioms all-axioms)
              (owlapi-warning "Upps! Forgot to render axioms of types ~A!"
                            (remove-duplicates (mapcar #'type-of (set-difference axioms all-axioms))))))

          *axioms-rendered*)))))
