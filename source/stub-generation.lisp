;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: RACER; Base: 10 -*-

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

(in-package :racer)

(declaim (special +excluded+ *new-pprint-dispatch* *old-pprint-dispatch*))

(declaim (ftype (function (t) t) get-owlapi-synonyms )
         (ftype (function (t) t) check-server-interface)
         (ftype (function (t) t) get-racer-api)
         (ftype (function (t) t) get-all-racer-symbols)
         (ftype (function (t) t) get-racer-macros)
         (ftype (function (t) t) undefined-server-functions)
         (ftype (function (t) t) create-jracer-methods)
         (ftype (function (t) t) create-lracer-functions)
         (ftype (function (t) t) get-racer-with-macros)
         (ftype (function (t) t) create-porter-registry)
         (ftype (function (t) t) get-racer-functions))

;;;
;;; Package Lisp: nrql-symbols.lisp generation
;;;

(defun create-package-list (&optional upper-symbols-p)
  (with-open-file (stream "nrql:nrql-symbols.lisp"
                          :direction :output 
                          :if-does-not-exist :create
                          :if-exists :supersede)

    (format stream "(in-package :cl-user)~%~%")

    (format stream ";;;~%")
    (format stream ";;;----------------------------------------------~%")
    (format stream ";;;   Automatically Generated nRQL Symbol List   ~%")
    (format stream ";;;          Version: ~A, Build: ~A ~%" (get-product-version) (get-build-version))
    (format stream ";;;----------------------------------------------~%")
    (format stream ";;;~%~%")
    
    (let* ((*print-case* :downcase)
           (syms (cons "IRI"
                       (remove-duplicates
                        (append `(with-nrql-standard-settings   
                                   racer-retrieve-individual-filled-roles
                                   racer-retrieve-individual-fillers 
                                   racer-retrieve-related-individuals

                                   racer-dummy-substrate
                                   data-substrate
                                   mirror-data-substrate
                                   mirror-sql-data-substrate
                                   rcc-substrate
                                   rcc-mirror-substrate

                                   master-top-query
                                   master-bottom-query
                                   top-query
                                   bottom-query
                             
                                   top-role
                                   bottom-role

                                   defer
			    
                                   xml-output
                                   xml-input
                                   xml-native-output
                                   
                                   sequence
                                   answer-sequence
                                   quiet-sequence

                                   owlapi-sequence
				   owlapi-test-sequence
                                   owlapi-answer-sequence
                                   owlapi-quiet-sequence
				   
				   |OWLAPI-sequence|
				   |OWLAPI-testSequence|
				   
                                   |OWLAPI-answerSequence|
                                   |OWLAPI-quietSequence|
			    
                                   |OWLAPI-getLastAnswer|
                                   |OWLAPI-getIDsOfLastAnswer| 
                                   |OWLAPI-getLastOutputStreamString|

                                   lisp-to-xml
                                   xml-to-lisp
			    
                                   nrql-equal-role
                                   taxonomy-node
                                   has-synonym
                                   has-child
                                   has-parent
                                   has-descendant 
                                   has-ancestor

                                   count
                                   sum
                                   prod
                                   avg
                                   substract 
                                   div 
                                   minilisp 

                                   minilisp-server-function-p
                                   minilisp-server-value-p
                             
                                   |OWLAxiom|
                                   |OWLLogicalAxiom|
                                   |OWLOntologyURIAttributeAxiom|
                                   |OWLAxiomWithEntitySlotAxiom|
                                   |OWLHasKeyAxiom|
                                   |OWLDatatypeDefinitionAxiom|
                                   |OWLPropertyAxiom|
                                   |OWLIndividualAxiom|
                                   |OWLClassAxiom|
                                   |OWLNaryDataPropertyAxiom|
                                   |OWLNaryObjectPropertyAxiom|
                                   |OWLDataPropertyAxiom|
                                   |OWLObjectPropertyAxiom|
                                   |OWLEquivalentDataPropertiesAxiom|
                                   |OWLDisjointDataPropertiesAxiom|
                                   |OWLInverseObjectPropertiesAxiom|
                                   |OWLEquivalentObjectPropertiesAxiom|
                                   |OWLDisjointObjectPropertiesAxiom|
                                   |OWLDataPropertyRangeAxiom|
                                   |OWLDataPropertyDomainAxiom|
                                   |OWLDataSubPropertyAxiom|
                                   |OWLFunctionalDataPropertyAxiom|
                                   |OWLObjectPropertyRangeAxiom|
                                   |OWLObjectPropertyDomainAxiom|
                                   |OWLObjectSubPropertyAxiom|
                                   |OWLTransitiveObjectPropertyAxiom|
                                   |OWLSymmetricObjectPropertyAxiom|
                                   |OWLReflexiveObjectPropertyAxiom|
                                   |OWLIrreflexiveObjectPropertyAxiom|
                                   |OWLInverseFunctionalObjectPropertyAxiom|
                                   |OWLAsymmetricObjectPropertyAxiom|
                                   |OWLAntiSymmetricObjectPropertyAxiom|
                                   |OWLObjectPropertyChainSubPropertyAxiom|
                                   |OWLFunctionalObjectPropertyAxiom|
                                   |OWLIndividualRelationshipAxiom|
                                   |OWLClassAssertionAxiom|
                                   |OWLNaryIndividualAxiom|
                                   |OWLObjectPropertyAssertionAxiom|
                                   |OWLDataPropertyAssertionAxiom|
                                   |OWLNegativeObjectPropertyAssertionAxiom|
                                   |OWLNegativeDataPropertyAssertionAxiom|
                                   |OWLSameIndividualsAxiom|
                                   |OWLDifferentIndividualsAxiom|
                                   |OWLNaryClassAxiom|
                                   |OWLSubClassAxiom|
                                   |OWLEquivalentClassesAxiom|
                                   |OWLDisjointUnionAxiom|
                                   |OWLDisjointClassesAxiom|
                                   |OWLPrefixDeclarationAxiom|
                                   |OWLOntologyVersionDeclarationAxiom|
                                   |OWLImportsDeclarationAxiom|
                                   |OWLDeclarationAxiom|
                                   |OWLAnnotationAxiom|
                                   |OWLImplicitDeclarationAxiom|
                                   |OWLAnnotationPropertyRangeAxiom|
                                   |OWLAnnotationPropertyDomainAxiom|
                                   |OWLSubAnnotationPropertyOfAxiom|
                                   |OWLAnnotationAssertionAxiom|
                                   |OWLOntologyAnnotationAxiom|
                                   |OWLEntityAnnotationAxiom|
                                   |OWLAxiomAnnotationAxiom|

                                   |SetOntologyURI|
                                   |OWLAxiomChange| 
                                   |RemoveAxiom| 
                                   |AddAxiom|

                                   |Reasoner|
                                   |WithReasoner|
                                   |Import|
                                   |Namespace|
                                   |Prefix|
                                   |Declaration|
                                   |Datatype|
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
                                   |DatatypeRestriction| 
  				   |Facet|
				  
                                   ;;; f. OWLlink benoetigt: 
				  
                                   |owl.Class| 
                                   |owl.NamedIndividual|
                                   |owl.AnonymousIndividual|
                                   |owl.ObjectProperty|
                                   |owl.DataProperty|
                                   |owl.AnnotationProperty|

                                   ;;;
				  
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
                                   ;; |SubclassOf|
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
                                   |SubClassOf|
                                   |ObjectPropertyAssertion|
                                   |NegativeObjectPropertyAssertion|
                                   |DataPropertyAssertion|
                                   |NegativeDataPropertyAssertion|

                                   |Label|
                                   |Comment|
                                   |Annotation|

                                   |name|
                                   |kb|
                                   |OWL***-Literal|
                                   |OWLLiteral|
                                   |Literal|
                                   |Constant|
                                   |facet|
                                   |FacetRestriction|

                                   ;;; Facets
                             
                                   |length| 
                                   |minLength|
                                   |maxLength|
                                   |pattern|
                                   |minInclusive|
                                   |maxInclusive|
                                   |minExclusive|
                                   |maxExclusive|
                                   |totalDigits|
                                   |fractionDigits|
                                   |DataExactCardinality|

                                   
                                   ;;;
                                   ;;;
                                   ;;;

                                   |Cardinality|

                                   ;;;
                                   ;;; XML Datatypes 
                                   ;;; Wichtig, dass die in nRQL Symbols sind, 
                                   ;;; sonst erzeugt der Functional Parser evtl. Typen per "intern", 
                                   ;;; die Racer nicht findet im Encoding!
                                   ;;; 

                                   ,(intern (concatenate 'string nox:-xsd-uri- "byte"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "negativeInteger"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "nonPositiveInteger"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "positiveInteger"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "nonNegativeInteger"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "integer"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "unsignedShort"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "unsignedInt"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "unsignedLong"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "unsignedByte"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "long"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "int"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "short"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "float"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "double"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "decimal"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "boolean"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "XMLLiteral"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "Literal"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "anyURI")) 
                                   ,(intern (concatenate 'string nox:-xsd-uri- "base64binary")) 
                                   ,(intern (concatenate 'string nox:-xsd-uri- "hexBinary")) 
                                   ,(intern (concatenate 'string nox:-xsd-uri- "string")) 
                                   ,(intern (concatenate 'string nox:-xsd-uri- "normalizedString"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "token"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "language"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "Name"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "NMTOKEN"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "NMTOKENS"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "NCName"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "ID"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "IDREF"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "IDREFS"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "ENTITY"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "ENTITIES"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "QName"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "date"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "time"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "dateTime"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "gYear"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "gYearMonth"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "gMonth"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "gMonthDay"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "gDay"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "duration"))

                                   ;;;
                                   ;;;
                                   ;;;


                                   ,(intern (concatenate 'string nox:-xsd-uri- "negativeInteger"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "nonNegativeInteger"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "nonPositiveInteger"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "integer"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "byte"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "unsignedByte"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "unsignedShort"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "unsignedInt"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "unsignedLong"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "short"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "int"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "long"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "float"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "double"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "decimal"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "boolean"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "string"))

                                   ,(intern (concatenate 'string nox:-xsd-uri- "maxExclusive"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "maxInclusive"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "minExclusive"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "minInclusive"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "minLength"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "maxLength"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "length"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "pattern"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "totalDigits"))
                                   ,(intern (concatenate 'string nox:-xsd-uri- "fractionDigits"))
			
                                   ,(intern (concatenate 'string nox:-rdfs-uri- "comment"))
                                   ,(intern (concatenate 'string nox:-rdfs-uri- "Comment"))
                                   ,(intern (concatenate 'string nox:-rdfs-uri- "label"))
                                   ,(intern (concatenate 'string nox:-rdfs-uri- "Label"))
                                   ,(intern (concatenate 'string nox:-rdfs-uri- "seeAlso"))
                                   ,(intern (concatenate 'string nox:-rdfs-uri- "SeeAlso"))

                                   ;;;
                                   ;;;
                                   ;;;
			
                                   ,(intern (concatenate 'string +owl-version+ "isDefinedBy")) 
                                   ,(intern (concatenate 'string +owl-version+ "IsDefinedBy")) 
                                   ,(intern (concatenate 'string +owl-version+ "versionInfo")) 
                                   ,(intern (concatenate 'string +owl-version+ "VersionInfo")) 

                                   ,(intern (concatenate 'string +owl-version+ "Thing")) 
                                   ,(intern (concatenate 'string +owl-version+ "Nothing")) 
                                   
                                   ,(intern (concatenate 'string +owl-version+ "topDataProperty"))
                                   ,(intern (concatenate 'string +owl-version+ "bottomDataProperty"))
                                   ,(intern (concatenate 'string +owl-version+ "topObjectProperty"))
                                   ,(intern (concatenate 'string +owl-version+ "bottomObjectProperty"))

                                   ;;;
                                   ;;;
                                   ;;;

                                   #:d-filler
                                   #:d-literal
                                   #:d-all
                                   #:d-some
                                   #:d-at-least
                                   #:d-at-most
                                   #:d-exactly
                                   #:d-facet
                                   #:d-base-type
                                   #:d-complement
                                   #:d-and
                                   #:d-or
                                   #:d-datarange
                                   #:d-restriction
                                   #:d-possible-values

                                   ;;;
                                   ;;;
                                   ;;; 
                             
                                   nrql-warning
                                   nrql-error
                                   owlapi-kb
                                   with-reasoner
                                   last-answer
                                   last-error
                                   last-output-stream-string
                                   simple-output
                                   return-policy
                                   auto-mode
                                   auto-apply

                                   owllink2-eval-request
                                   get-minilisp-value 

                                   racer-prepare-substrate
                                   retrieve-annotation-datatype-values

                                   ;; whitespace-char-p
                            
                                   )
			  
                                (remove :racer ts::+reserved-tokens+)
                          
                                (mapcar #'first 
                                        (remove-if #'fourth
                                                   (append ts::*nrql-functions*
                                                           ts::*nrql-methods*
                                                           ts::*nrql-macros*
                                                           ts::*nrql-with-macros*))))))))
      (format stream  
              "(defpackage nrql-symbols
                 (:use :common-lisp)
                 (:export 
                  #:*owllink2-output-syntax*
                  #:*owllink2-input-syntax*
                  #:*converter-mode*
                  #:*told-information-reasoning-p*
                  #:*toggle-una-variables-p*
                  #:*cur-reasoner*
                  #:*cur-substrate*
                  #:query")

      (terpri stream)

      (dolist (sym syms)
        (if (stringp sym)
            (format stream "                          #:~A~%" 
                    sym)
          (if (not upper-symbols-p)
              (format stream "                          #:~S~%" 
                      (intern (symbol-name sym)))
            (format stream "                          ~S~%" sym))))
      (format stream "))~%~%")
      (format stream "(#+:sbcl defparameter #-:sbcl defconstant +nrql-symbols+ '(~%")
      (dolist (sym syms)
        (if (stringp sym)
            (format stream "                          #:~A~%" 
                    sym)
          (if (not upper-symbols-p) 
              (format stream "                          #:~S~%" 
                      (intern (symbol-name sym)))
            (format stream "                          ~S~%" sym))))
      (format stream "))~%~%"))))

;;;
;;; Server Case generation: nrql-server-case.lisp
;;; 

(defun create-server-case ()

  (let ((*print-case* :downcase))
    
    (with-open-file (stream "nrql:nrql-server-case.lisp"
                            :direction :output 
                            :if-does-not-exist :create
                            :if-exists :supersede)

      (format stream "(in-package cl-user)~%~%")

      (format stream ";;;~%")
      (format stream ";;;----------------------------------------------~%")
      (format stream ";;;   Automatically Generated nRQL Server Case   ~%")
      (format stream ";;;          Version: ~A, Build: ~A ~%" (get-product-version) (get-build-version))
      (format stream ";;;----------------------------------------------~%")
      (format stream ";;;~%")

      (pprint 
       '(defvar *null-stream*
          (make-broadcast-stream))
       stream)

      (terpri stream)

      (pprint 

       `(defun process-nrql-request (expr stream n state output-string-stream)

          (case (first expr)
	    
            ,@(remove nil
		      
                      (append 

                       `(((xml-output xml-native-output)
			  
                          (process-racer-expr (second expr)
                                              nil
                                              n
                                              state
                                              output-string-stream)
			  
                          (let* ((native (eq (first expr) 'xml-native-output))
                                 (value
                                  (with-output-to-string (sstream)
                                    (apply #'ts::lisp-to-xml 
                                           (if *cur-reasoner*
                                               (if (last-error *cur-reasoner*)
                                                   (last-error *cur-reasoner*)
                                                 (last-answer *cur-reasoner*))
                                             (if *last-error*
                                                 *last-error*
                                               *last-answer*))
                                           sstream
                                           :include-answer-string-p native
                                           :top-level-attributes
                                           (format nil "id=\"~d\" type=\"~A\""
                                                   n
                                                   (if (and *cur-reasoner*
                                                            (last-error *cur-reasoner*))
                                                       'error
                                                     'answer))
                                           (cddr expr)))))
			    
                            (format stream ":answer ~D \"~A\" \"~A\"" 
                                    n (transform-value value)
                                    (convert-output-to-string output-string-stream))
			    
                            (racer-eol stream)))
			 
                         ((xml-input)
			  
                          (let* ((string (second expr))
                                 (expr (ts::xml-to-lisp string)))
       
                            (process-racer-expr expr
                                                stream
                                                n
                                                state
                                                output-string-stream))))
                       
                       `(((|OWLAPI-getLastOutputStreamString|)

                          (with-reasoner ((if (cdr expr)
                                              (second expr)
                                            *cur-reasoner*))

                            (let ((old-policy 
                                   (return-policy *cur-reasoner*)))

                              (unwind-protect
                                  (progn 
                                    (setf (return-policy *cur-reasoner*) :answer-direct)
                                    
                                    (answer expr
                                            state
                                            stream
                                            n
                                            (last-output-stream-string *cur-reasoner*)
                                            output-string-stream))

                                (setf (return-policy *cur-reasoner*) old-policy)))))
                                
                         ((|OWLAPI-getLastAnswer|)
                          
                          (with-reasoner ((if (cdr expr)
                                              (second expr)
                                            *cur-reasoner*))

                            (let ((old-policy 
                                   (return-policy *cur-reasoner*)))

                              (unwind-protect
                                  (progn 
                                    (setf (return-policy *cur-reasoner*) :answer-direct)
                                    
                                    (answer expr
                                            state
                                            stream
                                            n
                                            (last-answer *cur-reasoner*)
                                            output-string-stream))

                                (setf (return-policy *cur-reasoner*) old-policy)))))   
			 
                         ((|OWLAPI-getIDsOfLastAnswer|)	
		  
                          (labels ((do-it (obj)
                                     (or (|OWLAPI-findIDFromObject| obj)
                                         (if (listp obj)
                                             (mapcar #'do-it obj)
                                           obj))))

			  
                            (with-reasoner ((if (cdr expr)
                                                (second expr)
                                              *cur-reasoner*))

                              (let ((old-policy 
                                     (return-policy *cur-reasoner*)))

                                (unwind-protect
                                    (progn 
                                      (setf (return-policy *cur-reasoner*) :answer-direct)
                                    
                                      (answer expr
                                              state
                                              stream
                                              n
                                              (do-it (last-answer *cur-reasoner*))
                                              output-string-stream))

                                  (setf (return-policy *cur-reasoner*) old-policy)))))))   

                       (mapcar #'(lambda (x)
                                   (let ((name (first x))
                                         (corresponding-function (second x)))
				     
                                     (unless corresponding-function
                                       (error ":nrql-function missing: ~A!" name))
				     
                                     `((,name)
				       
                                       (let* ((saved-timeout *server-timeout*)
                                              (*server-timeout* nil))
					 
                                         ;;; die nRQL-Funktionen setzen ihr eigenes Timeout auf! 
                                         (answer expr
                                                 state
                                                 stream n 
                                                 (let ((*server-timeout* 
                                                        saved-timeout))
                                                   (apply (symbol-function ',corresponding-function)
                                                          (rest expr)))
                                                 output-string-stream)))))
			       
                               (remove-if #'fourth
                                          ts::*nrql-macros*))
                       
                       `(((sequence)
                          (let ((*server-timeout* nil) (reasoner *cur-reasoner*)
                                (*check-subscriptions-inhibited* t)
                                (ts::*changes-for-aboxes-pending* nil))
                            
                            (with-output-to-string (string-stream)
                              (prog1

                                  (mapl #'(lambda (expressions)
                                            (let ((expr1 (first expressions)))
                                              (prog1
                                                  (process-racer-expr expr1
                                                                      (if (cdr expressions)
                                                                          *null-stream*
                                                                        stream)
                                                                      n
                                                                      state
                                                                      (if (cdr expressions)
                                                                          string-stream
                                                                        output-string-stream))
                                                (when (and (last-error reasoner)
                                                           (cdr expressions))
                                                  (error (last-error reasoner))))))
                                        (rest expr))

                                (let ((*check-subscriptions-inhibited* nil))
                                  (dolist (abox ts::*changes-for-aboxes-pending*)
                                    (check-nrql-subscriptions abox))))))))
                       
                       `(((|OWLAPI-sequence| owlapi-sequence |OWLAPI-testSequence| owlapi-test-sequence)
			  (let ((*server-timeout* nil) (reasoner *cur-reasoner*)
                                (*check-subscriptions-inhibited* t)
                                (ts::*changes-for-aboxes-pending* nil)
                                (count 0)
				(test-sequence-p 
                                 (member (first expr) '(|OWLAPI-testSequence| owlapi-test-sequence))))

                            (with-reasoner ((second expr))
                              (let ((*progress-value* 0))
                                (with-progress-range ((length (cddr expr)) (0 100))
                                  (with-output-to-string (string-stream)
                                    (prog1

                                        (mapl #'(lambda (expressions)
                                                  (let ((expr1 (first expressions)))
						    (when test-sequence-p 
						      (sleep 0.1))
                                                    (set-progress-value (incf count))
                                                    (prog1
                                                        (process-racer-expr expr1
                                                                            (if (cdr expressions)
                                                                                *null-stream*
                                                                              stream)
                                                                            n
                                                                            state
                                                                            (if (cdr expressions)
                                                                                string-stream
                                                                              output-string-stream))
                                                      (when (and (last-error reasoner)
                                                                 (cdr expressions))
                                                        (error (last-error reasoner))))))
                                              (cddr expr))

                                      (let ((*check-subscriptions-inhibited* nil))
                                        (dolist (abox ts::*changes-for-aboxes-pending*)
                                          (check-nrql-subscriptions abox)))))))))))

                       `(((answer-sequence)
                          (let ((*server-timeout* nil) (reasoner *cur-reasoner*)
                                (*check-subscriptions-inhibited* t)
                                (ts::*changes-for-aboxes-pending* nil))

                            (prog1

                                (answer expr
                                        state
                                        stream
                                        n
                                        (let ((x nil))
                                          (with-output-to-string (string-stream)
                                            (setf x (mapcar #'(lambda (expr1)
                                                                (process-racer-expr expr1
                                                                                    *null-stream*
                                                                                    n
                                                                                    state
                                                                                    output-string-stream)
                                                                (if (last-error reasoner)
                                                                    (error (last-error reasoner))
                                                                  (last-answer reasoner)))
                                                            (rest expr))))
                                          x)
                                        output-string-stream)

                              (let ((*check-subscriptions-inhibited* nil))
                                (dolist (abox ts::*changes-for-aboxes-pending*)
                                  (check-nrql-subscriptions abox)))))))

                       `(((|OWLAPI-answerSequence| owlapi-answer-sequence)
                          (let ((*server-timeout* nil) (reasoner *cur-reasoner*)
                                (*check-subscriptions-inhibited* t)
                                (ts::*changes-for-aboxes-pending* nil)
                                (count 0))

                            (prog1
                                
                                (with-reasoner ((second expr))
                                  (let ((*progress-value* 0))
                                    (with-progress-range ((length (cddr expr)) (0 100))
                                      (answer expr
                                              state
                                              stream
                                              n
                                              (let ((x nil))
                                                (with-output-to-string (string-stream)
                                                  (setf x (mapcar #'(lambda (expr1)
                                                                      (set-progress-value (incf count))
                                                                      (process-racer-expr expr1
                                                                                          *null-stream*
                                                                                          n
                                                                                          state
                                                                                          output-string-stream)
                                                                      (if (last-error reasoner)
                                                                          (error (last-error reasoner))
                                                                        (last-answer reasoner)))
                                                                  (cddr expr))))
                                                x)
                                              output-string-stream)

                                      (let ((*check-subscriptions-inhibited* nil))
                                        (dolist (abox ts::*changes-for-aboxes-pending*)
                                          (check-nrql-subscriptions abox))))))))))
                       
                       `(((quiet-sequence)
                          (let ((*server-timeout* nil) (reasoner *cur-reasoner*)
                                (*check-subscriptions-inhibited* t)
                                (ts::*changes-for-aboxes-pending* nil))

                            (with-output-to-string (string-stream)
                              (mapc #'(lambda (expr1)
                                        (prog1
                                            (process-racer-expr expr1
                                                                *null-stream*
                                                                n
                                                                state
                                                                output-string-stream)
                                          (when (last-error reasoner)
                                            (error (last-error reasoner)))))
                                    (rest expr)))

                            (prog1 

                                (ok expr stream n state nil output-string-stream)
                            
                              (let ((*check-subscriptions-inhibited* nil))
                                (dolist (abox ts::*changes-for-aboxes-pending*)
                                  (check-nrql-subscriptions abox)))))))
		      
                       `(((|OWLAPI-quietSequence| owlapi-quiet-sequence)
                          (let ((*server-timeout* nil) (reasoner *cur-reasoner*)
                                (*check-subscriptions-inhibited* t)
                                (ts::*changes-for-aboxes-pending* nil)
                                (count 0))

                            (with-reasoner ((second expr))
                              (let ((*progress-value* 0))
                                (with-progress-range ((length (cddr expr)) (0 100))
                                  (with-output-to-string (string-stream)
                                    (mapc #'(lambda (expr1)
                                              (set-progress-value (incf count))
                                              (prog1
                                                  (process-racer-expr expr1
                                                                      *null-stream*
                                                                      n
                                                                      state
                                                                      output-string-stream)
                                                (when (last-error reasoner)
                                                  (error (last-error reasoner)))))
                                          (cddr expr)))

                                  (prog1 

                                      (ok expr stream n state nil output-string-stream)
                            
                                    (let ((*check-subscriptions-inhibited* nil))
                                      (dolist (abox ts::*changes-for-aboxes-pending*)
                                        (check-nrql-subscriptions abox))))))))))
		      
                       (mapcar #'(lambda (x)
                                   (let ((name (first x))
                                         (corresponding-function (second x)))
				     
                                     (unless corresponding-function
                                       (error ":nrql-function missing: ~A!" name))
				     
                                     `((,name)

                                       (let ((*server-timeout* nil))
					 
                                         ;; bei with-macros wird das server-timeout ignoriert! 
                                         ;; dafuer gibt es :timeout ... 
					 
                                         (apply (symbol-function ',corresponding-function)
                                                (lambda ()
                                                  (loop for expr1 in (cddr expr) do
                                                        (process-racer-expr expr1 stream 
                                                                            n state output-string-stream)))
                                                (second expr))))))
			       
                               (remove-if #'fourth
                                          ts::*nrql-with-macros*))
		       
                       (list 
                        (list (remove-duplicates 
                               (mapcar #'first 
                                       (remove-if #'fourth
                                                  (append ts::*nrql-methods*
                                                          ts::*nrql-functions*))))
			      
                              `(let* ((saved-timeout *server-timeout*)
                                      (*server-timeout* nil))

                                 (answer expr
                                         state
                                         stream n 
                                         (let ((*server-timeout* 
                                                saved-timeout))
                                           (apply (symbol-function (first expr))
                                                  (rest expr)))
                                         output-string-stream))))))
	    
            (otherwise

             (cond ((ts::minilisp-server-function-p (first expr))

                    ;;; user extension? 

                    (answer expr
                            state
                            stream
                            n
                            (apply #'ts::call-function expr)
                            output-string-stream))

                   (t 

                    (let ((res :hook-not-found))
                      
                      (loop as hook in ts:*server-hooks*
                            do (let ((res1 (funcall hook expr)))
                                 (unless (eq res1 :hook-not-found)
                                   (setf res res1)
                                   (return))))
                      
                      (case res
                        (:hook-not-found 
                         (error "Illegal operator in ~A" expr))
                        (otherwise
                         (answer expr
                                 state
                                 stream
                                 n
                                 res
                                 output-string-stream)))))))))
       stream)
      
      (terpri stream)

      (pprint '(defun server-patch-hook (expr) 
                 (declare (ignorable expr))
                 ;;; diese Funktion kann per Patch-Mechanismus redefiniert werden
                 ;;; dann muss nicht so ein Riesen-FASL-Patch fuer das gesamte Server-Case
                 ;;; an den Kunden ausgeliefert werden....

                 :hook-not-found)
              stream)

      (terpri stream)
      
      (pprint '(ts:server-hook 'server-patch-hook) stream)

      (terpri stream))))

;;;
;;; Utilities
;;;

(defun process-lambda (lambda)
  (when lambda
    (case (first lambda)
      (&rest
       (process-lambda (cddr lambda)))
      (&allow-other-keys
       (process-lambda (cddr lambda)))
      (&optional
       `(,(second lambda) 
         ,@(process-lambda (cddr lambda))))
      (&key
       (apply #'append 
              (mapcar 
               #'(lambda (x) 
                   `(,(intern (format nil "~A" x)
                              (find-package :keyword))
                     ,x))
               (rest lambda))))
      (otherwise 
       `(,(first lambda)
         ,@(process-lambda (rest lambda)))))))

(defun cl-user (x)
  (let ((name (symbol-name x)))
    (if (and (position-if #'upper-case-p name)
             (position-if #'lower-case-p name))
        (format nil "|~A|" (symbol-name x))
      (string-downcase (symbol-name x)))))

(defun remove-key-etc (list)
  (remove '&optional 
          (remove '&key 
                  (remove '&allow-other-keys
                          (remove '&args
                                  list)))))                         
          
(defun collect-args (required-args
                     optional-args
                     key-args 
                     args-args)

  (append (mapcar #'first (mapcar #'ensure-list optional-args))
          (mapcar #'first (mapcar #'ensure-list key-args))
          args-args
          (mapcan #'(lambda (arg)
                      (if (consp arg)
                          (multiple-value-call 
                              #'collect-args 
                            (get-lambda-parts arg))
                        (list arg)))
                  required-args)))

;;;
;;; LRacer
;;; 
          
(defun lracer-standard-stub (stream x macro-p 
                                    &key
                                    (auto-lambda-p t)
                                    required-args optional-args key-args args-args)
                                    
  (let* ((name (cl-user x)))      

    (multiple-value-bind (required-args
                          optional-args
                          key-args
                          args-args)

        (if (or (not auto-lambda-p) 
                required-args optional-args key-args args-args)
            (values required-args optional-args key-args args-args)
          (get-lambda-parts x))

      (dolist (x (list `(required-args ,required-args)
                       `(optional-args ,optional-args)
                       `(key-args ,key-args)
                       `(args-args ,args-args)))

        (let ((args (second x))
              (new-args nil))
          
          (unless (= (length args)
                     (length 
                      (setf new-args
                            (remove-duplicates args
                                               :test #'(lambda (x y)
                                                         (string= (symbol-name x)
                                                                  (symbol-name y)))))))
            
            (ecase (first x) 
              (required-args (setf required-args new-args))
              (optional-args (setf optional-args new-args))
              (key-args (setf key-args new-args))
              (args-args (setf args-args new-args)))
            
            
            (nrql-warning "*** FOR ~A: FOUND SYMBOLS WITH SAME SYMBOL-NAME: ~S, REMOVING DUPLICATES" 
                          name
                          args))))
      
      (if macro-p
          (format stream "~%~%(defmacro ~A (" name)
        (format stream "~%~%(defun ~A (" name))

      (labels ((process-fn-args (required-args
                                 optional-args
                                 key-args
                                 args-args)

                 (dolist (arg required-args)
                   (if (consp arg) ; nested ? 
                       (progn 
                         (format stream " (")
                         (multiple-value-call #'process-fn-args 
                           (get-lambda-parts arg)))
                     (format stream " ~A" arg)))

                 (when optional-args
                   (format stream " &optional ~{ ~A~}" 
                           (mapcar #'(lambda (key)
                                       (let ((key (first (ensure-list key))))
                                         `(,key nil ,(format nil "~A-supplied-p"
                                                             key))))
                                   optional-args)))
                 (when args-args
                   (format stream " &rest ~{ ~A~}" args-args))

                 (when key-args
                   (format stream " &key ~{ ~A~}" 
                           (mapcar #'(lambda (key)
                                       (let ((key (first (ensure-list key))))
                                         `(,key nil ,(format nil "~A-supplied-p"
                                                             key))))
                                   key-args)))
                 (format stream ")")))

        (process-fn-args required-args
                         optional-args
                         key-args
                         args-args)

        (terpri stream) 

        (format stream " (declare (ignorable ~{ ~A~}))~%" 
                (collect-args required-args
                              optional-args
                              key-args 
                              args-args))
          
        (format stream "  (with-standard-io-syntax-1~%")

        (princ "    (let ((req-string
           (concatenate 'string " stream)
            
        (terpri stream) 
        (format stream "      \"(\"")
        (terpri stream) 
        (format stream "      ~S" name) 

        (labels ((process-args (required-args
                                optional-args
                                key-args
                                args-args)

                   (dolist (arg required-args)
                     (if (consp arg) ; nested ? 
                         (progn 
                           (terpri stream)
                           (format stream "      \" (\"")
                           (terpri stream)
                           (multiple-value-call #'process-args 
                             (get-lambda-parts arg))
                           (terpri stream)
                           (format stream "      \") \"")
                           (terpri stream))
                       (progn 
                         (terpri stream)
                         (write-string "      (myformat nil \" ~S\" (transform-s-expr " stream)
                         (format stream "~A" arg)
                         (write-string "))" stream))))

                   (dolist (arg optional-args)
                     (let ((arg (first (ensure-list arg))))
                       (terpri stream)
                       (format stream "      (when ~A-supplied-p" arg)
                       (write-string " (myformat nil \" ~S\" (transform-s-expr " stream)
                       (format stream "~A" arg)
                       (write-string ")))" stream)))

                   (dolist (key key-args)
                     (let ((sym (first (ensure-list key))))
                       (terpri stream)
                       (format stream "      (when ~A-supplied-p" sym)
                       (write-string " (myformat nil \"" stream)
                       (format stream " :~A" sym)
                       (write-string " ~S\" " stream)
                       (format stream "(transform-s-expr ")
                       (format stream "~A" sym)
                       (write-string ")))" stream)))
          
                   (when args-args
                     (terpri stream)
                     (write-string  "      (myformat nil \" ~{ ~S~}\"" stream)
                     (format stream " (mapcar #'transform-s-expr ~A))" (first args-args)))))

          (process-args required-args
                        optional-args
                        key-args
                        args-args)

          (terpri stream)
          (princ "      \")\"" stream)
          (princ " )))" stream)
          (terpri stream)
          (terpri stream)
        
          (if macro-p 
              (write-string "   `(service-request ,req-string))))" stream)
            (write-string "   (service-request req-string))))" stream)))))))

(defun lracer-with-macro-stub (stream x)
  (let* ((name (cl-user x))
         (lambda (get-lambda-parts x)))
    (if lambda 
        (format stream "~%~%(declare-with-macro ~A ~{ ~A~})" name lambda)
      (format stream "~%~%(declare-with-macro ~A)" name))))

(defun lracer-progn-macro-stub (stream x)
  (let* ((name (cl-user x)))
    (format stream "~%~%(defmacro ")
    (format stream "~A (&body body)~%" name)
    (format stream "  (with-standard-io-syntax-1~%")
    (format stream "    `(service-request~%")
    (princ "      ,(myformat nil \"(~A ~{ ~S~})\"" stream)
    (terpri stream)
    (format stream "      \"~A\"~%" x)
    (format stream "      (mapcar #'transform-s-expr body)))))")))
      
(defun create-lracer-functions ()

  (with-open-file (stream "nrql:lracer-stubs.lisp"
                          :direction :output 
                          :if-does-not-exist :create
                          :if-exists :supersede)

    (format stream "(in-package racer)~%~%")

    (format stream ";;;~%")
    (format stream ";;;----------------------------------------------~%")
    (format stream ";;;       Automatically Generated Racer Stubs    ~%")
    (format stream ";;;          Version: ~A, Build: ~A ~%" (get-product-version) (get-build-version))
    (format stream ";;;----------------------------------------------~%")
    (format stream ";;;")

    (let* ((*print-case* :downcase)

           (without-defs
            (undefined-server-functions))

           (excluded '(time 
                       sequence
                       quiet-sequence
                       owlapi-sequence
		       owlapi-test-sequence
                       owlapi-quiet-sequence
                       owlapi-answer-sequence
                       exit-server

                       ;;; muessen bereits in lracer-core definiert sein: 

                       enable-alisp-compatibility-mode
                       disable-alisp-compatibility-mode
                       get-product-version

                       ;;; get-namespace-prefixes
                       ;;; get-namespace-prefix

                       evaluate
                       evaluate1
                       
                       ))

           (functions  
            (sort 
             (set-difference 
              (get-racer-functions)
              without-defs)
             #'string-lessp 
             :key #'symbol-name))

           (macros 
            (sort 
             (set-difference
              (get-racer-macros)
              without-defs)
             #'string-lessp 
             :key #'symbol-name))
           
           (with-macros 
            (sort 
             (get-racer-with-macros)
             #'string-lessp 
             :key #'symbol-name))

           (undefined 
            (set-difference
             without-defs
             with-macros)))
      
      ;;;
      ;;; Functions
      ;;;

      (dolist (fns (list functions macros))
        (dolist (x fns)
          (unless (member x excluded)
            (let ((macro-p (eq fns macros)))
              (lracer-standard-stub stream x macro-p)))))
           
      ;;;
      ;;; With Macros
      ;;; 
    
      (dolist (x with-macros)
        (unless (member x excluded)
          (lracer-with-macro-stub stream x)))

      ;;;
      ;;; Undefined (treat as &rest args macros...) 
      ;;; 
    
      (dolist (x undefined)
        (unless (member x excluded)
          (get-lracer-stub-for stream x)))

      ;;;
      ;;;
      ;;;

      (get-lracer-stub-for stream 'evaluate)
      (get-lracer-stub-for stream 'evaluate1)

      (terpri stream)

      (pprint '(defun exit-server ()
                 (declare (ignorable ))
                 (handler-case
                     (with-standard-io-syntax-1
                      (let ((req-string
                             (concatenate 'string 
                                          "("
                                          "exit-server"
                                          ")" )))
                        (service-request req-string)))
                   (racer-error (error) 
                     (error error))
                   (lracer-connection-error (error)
                     t))) stream)

      (terpri stream))

    ;;;
    ;;;
    ;;;
    
    (terpri stream)))

(defun get-lracer-stub-for (stream x) 
  (ecase x 

    ((ANSWER-SEQUENCE 
      QUIET-SEQUENCE 
      SEQUENCE
      |OWLAPI-sequence|
      |OWLAPI-testSequence|
      |OWLAPI-answerSequence|
      |OWLAPI-quietSequence|)
       
     (lracer-progn-macro-stub stream x))
      
    ((DISABLE-ALISP-COMPATIBILITY-MODE 
      ENABLE-ALISP-COMPATIBILITY-MODE 
      ENSURE-SMALL-TBOXES
      GET-NAMESPACE-PREFIXES
      GET-RACER-VERSION
      
      GET-INITIAL-SIZE-OF-PROCESS-POOL
      GET-MAXIMUM-SIZE-OF-PROCESS-POOL 
      GET-PROCESS-POOL-SIZE
      SET-INITIAL-SIZE-OF-PROCESS-POOL
      SET-MAXIMUM-SIZE-OF-PROCESS-POOL)

     (lracer-standard-stub stream x t
                           :auto-lambda-p nil))

    ((|OWLAPI-getIDsOfLastAnswer| 
      |OWLAPI-getLastAnswer| 
      |OWLAPI-getLastOutputStreamString|)

     (lracer-standard-stub stream x t
                           :optional-args '(reasoner)
                           :auto-lambda-p nil))
       
    ((XML-INPUT 
      XML-NATIVE-OUTPUT 
      XML-OUTPUT)

     (lracer-standard-stub stream x t
                           :required-args '(expr)
                           :auto-lambda-p nil))

    ((EVALUATE
      EVALUATE1)

     (lracer-standard-stub stream x t
                           :required-args '(expr)
                           :auto-lambda-p nil))))

;;;
;;; JRacer 
;;;

(defvar *jracer-methods* nil)

(defvar *cls-counter* 0)

(defun create-jracer-methods ()

  (with-open-file (stream "nrql:jracer.java"
                          :direction :output 
                          :if-does-not-exist :create
                          :if-exists :supersede)

    (format stream "package com.racersystems.jracer;~%~%")


    (format stream "/** ~%")
    (format stream " *       Automatically Generated Racer Stubs    ~%")
    (format stream " *          Version: ~A, Build: ~A ~%" (get-product-version) (get-build-version))
    (format stream " */~%~%")

  
    (format stream "abstract public class RacerStubs {

    abstract public boolean returnBoolean(RacerResult answer); 

    abstract public boolean returnBoolean(String answer); 

    abstract public RacerResult racerCall(Object... args) throws RacerClientException; 

    abstract public void pushWith(String withMacro, Object... args); 

    abstract public void popWith(String withMacro);~%~%")


    (let* ((*print-case* :downcase)
           (*package* (find-package :cl-user))
           (*jracer-methods* nil)

           (without-defs
            (undefined-server-functions))

           (excluded 
            (append '(time 
                      sequence
                      owlapi-sequence
		      owlapi-test-sequence
                      quiet-sequence
                      owlapi-quiet-sequence
                      owlapi-answer-sequence
                      exit-server
                      evaluate
                      evaluate1
                      ;;; muessen bereits in lracer-core definiert sein: 
                      enable-alisp-compatibility-mode
                      disable-alisp-compatibility-mode
                      get-product-version
                      get-racer-version
                       ;get-namespace-prefixes
                       ;get-namespace-prefix
                       
                      )
                    (mapcar #'caadr (cdr (get-owlapi-synonyms)))))

           (functions  
            (sort 
             (set-difference 
              (get-racer-functions)
              without-defs)
             #'string-lessp 
             :key #'symbol-name))

           (macros 
            (sort 
             (set-difference
              (get-racer-macros)
              without-defs)
             #'string-lessp 
             :key #'symbol-name))
           
           (with-macros 
            (sort 
             (get-racer-with-macros)
             #'string-lessp 
             :key #'symbol-name))

           (undefined 
            (set-difference
             without-defs
             with-macros)))
      
      ;;;
      ;;; Functions
      ;;;

      (dolist (fns (list functions macros))
        (dolist (x fns)
          (unless (member x excluded
                          :test #'string-equal
                          :key #'symbol-name)
            (jracer-standard-stub stream x (eq fns macros)))))
           
      ;;;
      ;;; With Macros
      ;;; 
    
      (dolist (x with-macros)
        (unless (member x excluded)
          (jracer-with-macro-stub stream x)))

      ;;;
      ;;; Undefined (treat as &rest args macros...) 
      ;;; 
    
      (dolist (x undefined)
        (unless (member x excluded)
          (get-jracer-stub-for stream x)))

      ;;;
      ;;; Spezialbehandlung:
      ;;; 

      (get-jracer-stub-for stream 'evaluate)
      (get-jracer-stub-for stream 'evaluate1)

   
      (format stream "/** Racer Function exit-server
(exit-server)
 */

  public boolean exitServer( ) throws RacerClientException {

    try {
	    
	racerCall(\"exit-server\"  ).toString();
	
    } catch (RacerClientException ex) { return true; }

    return false;
  
  }
"))

    ;;;
    ;;;
    ;;;
    
    (format stream "~%}~%")))

(defun make-java-name (name)
  (let* ((new nil)
         (old (cond ((stringp name) 
                     name)
                    (t 
                     (let ((name2
                            (format nil "~S" name)))
                       (if (char= #\| (elt name2 0))
                           (symbol-name name)
                         (format nil "~A" name))))))
         (old (if (string-equal "class" old)
                  (format nil "cls~A" (incf *cls-counter*))
                old))
               
         (old (coerce old 'list)))

    (loop while old do
          (let ((cur (pop old)))
            (case cur
              (#\- 
               (push (char-upcase (pop old)) new))
              ((#\?) 
               (push #\P new))
              (otherwise
               (push cur new)))))

    (let* ((new (coerce (reverse new) 'string))
           (pos (search "OWLAPI" new)))
      (if (and pos (zerop pos))
          (concatenate 'string "owlapi" (subseq new 6))
        new))))          

(defun pretty-string (string)
  (let* ((n (length string))
         (pos1 (or (search "thematic-substrate::" string) n))
         (pos2 (or (search "racer::" string) n))
         (min (min pos1 pos2)))

    (if (not (= min n))
        (concatenate 'string 
                     (subseq string 0 min)
                     (pretty-string
                      (if (= pos1 min)
                          (subseq string (+ min 20))
                        (subseq string (+ min 7)))))
      string)))

(defun jracer-standard-stub (stream x macro-p 
                                    &key
                                    (auto-lambda-p t)
                                    required-args optional-args key-args args-args)
                                    
  (let* ((name (cl-user x))
         (java-name (make-java-name x))
         
         (is-predicate-p 
          (char= #\P (first (last (coerce java-name 'list)))))
         
         (java-name
          (if macro-p
              (if (not is-predicate-p)
                  (concatenate 'string java-name "M")
                (concatenate 'string
                             (subseq java-name
                                     0 
                                     (1- (length java-name)))
                             "MP"))
            java-name)))

    (if (member java-name *jracer-methods* :test #'string=)

        (nrql-warning "Skipping function ~A. Make sure it's a synonym." java-name)

      (multiple-value-bind (required-args
                            optional-args
                            key-args
                            args-args)

          (if (or (not auto-lambda-p) 
                  required-args optional-args key-args args-args)
              (values required-args optional-args key-args args-args)
            (get-lambda-parts x))

        (labels ((indent ()
                   (format stream "     "))

                 (sym (x)
                   (if (consp x)
                       (first x)
                     x)))          

          (push java-name *jracer-methods*)

          (unless (every #'symbolp required-args)
            (nrql-warning "JRacer has currently no implementation for: ~A" name))

          (when (every #'symbolp required-args)

            (format stream "/** Racer ~A ~A" 
                    (if macro-p "Macro" "Function")
                    x)

            (format stream 
                    "~A" 
                    (pretty-string
                     (with-output-to-string (stream)
                       (pprint `(,x ,@required-args
                                    ,@(when optional-args `(&optional ,@(mapcar #'sym optional-args)))
                                    ,@(when key-args `(&key ,@(mapcar #'sym key-args)))
                                    ,@(when args-args `(&rest ,args-args)))
                               stream))))

            (format stream "~% */~%~%")


            (labels ((output (args &key key-args)
                     
                       (let ((args (mapcar #'make-java-name args)))
                             
                         (let ((req-args
                                (format nil "~{Object ~A~#[~:;, ~]~}" args))
                               (act-args
                                (format nil "~{, ~A~}" args))
                               (key-args
                                (if key-args 
                                    (format nil "~A Object... keyArgs" (if args "," ""))
                                  ""))
                               (act-key
                                (if key-args
                                    (format nil ", keyArgs")
                                  "")))

                           (indent) 
        
                           (if is-predicate-p 
                               (format stream "public boolean ~A(~A ~A) throws RacerClientException {~%" 
                                       java-name req-args key-args)
                             (format stream "public String ~A(~A ~A) throws RacerClientException {~%" 
                                     java-name req-args key-args))

                           (indent) 
                           (indent) 
        
                           (if is-predicate-p 
                               (format stream "return returnBoolean(racerCall(~S ~A ~A));~%" name act-args act-key)
                             (format stream "return racerCall(~S ~A ~A).toString();~%" name act-args act-key))

                           (indent) 
                           (format stream "}~%~%")

                           ;;;
                           ;;;
                           ;;; 

                           (unless is-predicate-p 
                             (indent)
                
                             (format stream "public RacerResult ~A$(~A ~A) throws RacerClientException {~%" 
                                     java-name req-args key-args)

                             (indent) 
                             (indent) 
        
                             (format stream "return racerCall(~S ~A ~A);~%" 
                                     name act-args act-key)

                             (indent) 
                             (format stream "}~%~%"))))))
            
              (mapl #'(lambda (opt-args)
                      
                        (let* ((args
                                (append required-args
                                        (unless (eq (first opt-args) 'nil)
                                          (mapcar #'(lambda (x)
                                                      (if (consp x)
                                                          (first x)
                                                        x))
                                                  (remove nil 
                                                          (reverse opt-args)))))))

                          (output args)))

                    (cons nil (reverse optional-args)))

              (if optional-args 
                  (when (or key-args args-args)
                    (output (append required-args
                                    (mapcar #'sym optional-args))

                            :key-args (or key-args args-args)))
                (when (or key-args args-args)
                  (output required-args
                          :key-args (or key-args args-args))))
                
                                            
            
              )))))))

(defun jracer-with-macro-stub (stream x)
                                    
  (let* ((java-name (make-java-name x))
         (end-java-name
          (concatenate 'string
                       "end"
                       (coerce 
                        (let ((chars 
                               (coerce java-name 'list)))
                          (cons (char-upcase (first chars))
                                (rest chars)))
                        'string)))
         (lambda1 (get-lambda x))
         (has-lambda 
          (if (consp (first lambda1))
              (only-keys (first lambda1))
            lambda1)))
    
    (labels ((indent ()
               (format stream "     ")))

      (format stream "/** Racer With-Macro ~A" x)
      
      (format stream 
              "~A" 
              (pretty-string
               (with-output-to-string (stream)
                 (pprint `(,x ,@(when has-lambda `((&key ,@has-lambda)))
                              &body body)
                         stream))))

      (format stream "~% */~%~%")

      (indent) 
        
      (cond 
       (has-lambda
        (format stream "public void ~A(Object... keyArgs) {~%" java-name)
        (indent) 
        (indent) 
        (format stream "pushWith(\"~S\", keyArgs);~%" x)
        (indent) 
        (format stream "}~%~%"))
       (t
        (format stream "public void ~A() {~%" java-name)
        (indent) 
        (indent) 
        (format stream "pushWith(\"~S\");~%" x)
        (indent) 
        (format stream "}~%~%")))
    

      (indent)
      (format stream "public void ~A() {~%" end-java-name)
      (indent) 
      (indent) 
      (format stream "popWith(\"~S\");~%" x)
      (indent) 
      (format stream "}~%~%"))))

(defun get-jracer-stub-for (stream x) 
  (ecase x 
    ((ANSWER-SEQUENCE 
      QUIET-SEQUENCE 
      SEQUENCE
      |OWLAPI-sequence|
      |OWLAPI-testSequence|
      |OWLAPI-answerSequence|
      |OWLAPI-quietSequence|)
       
     nil)
      
    ((DISABLE-ALISP-COMPATIBILITY-MODE 
      ENABLE-ALISP-COMPATIBILITY-MODE 
      ENSURE-SMALL-TBOXES
      GET-NAMESPACE-PREFIXES

      GET-INITIAL-SIZE-OF-PROCESS-POOL
      GET-MAXIMUM-SIZE-OF-PROCESS-POOL 
      GET-PROCESS-POOL-SIZE
      SET-INITIAL-SIZE-OF-PROCESS-POOL
      SET-MAXIMUM-SIZE-OF-PROCESS-POOL)

     (jracer-standard-stub stream x t
                           :auto-lambda-p nil))

    ((|OWLAPI-getIDsOfLastAnswer| 
      |OWLAPI-getLastAnswer| 
      |OWLAPI-getLastOutputStreamString|)

     (jracer-standard-stub stream x t
                           :optional-args '(reasoner)
                           :auto-lambda-p nil))
       
    ((XML-INPUT 
      XML-NATIVE-OUTPUT 
      XML-OUTPUT)

     (jracer-standard-stub stream x t
                           :required-args '(expr)
                           :auto-lambda-p nil))

         
    ((EVALUATE
      EVALUATE1)

     (jracer-standard-stub stream x t
                           :required-args '(expr)
                           :auto-lambda-p nil))))

;;;
;;; OWLAPI Synonyms
;;;

(defun create-owlapi-synonyms ()

  (with-open-file (stream "nrql:owlapi-synonyms.lisp"
                          :direction :output 
                          :if-does-not-exist :create
                          :if-exists :supersede)

    (format stream "(in-package owlapi)~%~%")

    (format stream ";;;~%")
    (format stream ";;;----------------------------------------------~%")
    (format stream ";;;   Automatically Generated OWLAPI Synonyms   ~%")
    (format stream ";;;          Version: ~A, Build: ~A ~%" (get-product-version) (get-build-version))
    (format stream ";;;----------------------------------------------~%")
    (format stream ";;;")

    (let ((*print-case* :downcase)
          (*package* (find-package :owlapi)))
      (pprint (get-owlapi-synonyms) stream))))

;;;
;;; Racer Porter Registry for Completion and Documentation etc. 
;;;

(defun create-porter-registry ()
  (with-open-file (stream "sirius-dev:registry-new.lisp"
                          :direction :output 
                          :if-does-not-exist :create
                          :if-exists :supersede)

    (let* (#+:ignore
           (syns (mapcar #'caadr (cdr (get-owlapi-synonyms))))
           (api 
            (set-difference 
             #| (set-difference (get-racer-api)
                             syns) |# 
             (get-racer-api)
             +excluded+)))

      (format stream "(in-package sirius)~%~%")

      (format stream ";;;~%")
      (format stream ";;;----------------------------------------------~%")
      (format stream ";;;    Automatically Generated Sirius Registry   ~%")
      (format stream ";;;          Version: ~A, Build: ~A ~%" (get-product-version) (get-build-version))
      (format stream ";;;----------------------------------------------~%")
      (format stream ";;;")
    
      (terpri stream)

      (let ((*print-case* :downcase)
            (*package* (find-package :cl-user)))
        
        (pprint `(defconstant +racer-symbols+ (make-hash-table :test #'equal)) stream)
        
        (terpri stream)
        
        (pprint `(defconstant +commands+ (make-hash-table)) stream)
        
        (terpri stream)
        
        (pprint `(defconstant +minilisp-commands+ (make-hash-table)) stream)
        
        (terpri stream)
        
        (pprint `(defconstant +commands-hash+ (make-hash-table :test #'equal)) stream)

        (terpri stream)

        (pprint `(defconstant +mixed-case-commands-hash+ (make-hash-table :test #'equal)) stream)

        (terpri stream)

        (pprint `(defconstant +lambda-hash+ (make-hash-table :test #'equal)) stream)

        (terpri stream)

        (pprint `(defconstant +list-of-racer-symbols+ 
                   (quote ,(get-all-racer-symbols)))
                stream)

        (terpri stream)

        (pprint `(defconstant +list-of-with-macros+ 
                   (quote ,(mapcar #'(lambda (x) 
                                       (if (get-lambda-parts x)
                                           (list x 1)
                                         (list x 0)))
                                   (set-difference
                                    (get-racer-with-macros)
                                    +excluded+))))
                stream)

        (terpri stream)

        (pprint `(defconstant +list-of-mixed-case-commands+ 
                   (quote ,(remove-if-not #'(lambda (x) 
                                              (and (some #'upper-case-p (symbol-name x))
                                                   (some #'lower-case-p (symbol-name x))))
                                          api)))
                stream)

        (terpri stream)

        (pprint `(defconstant +list-of-commands+ 
                   (quote ,(mapcar #'(lambda (x) 
                                       (if (keywordp x)
                                           (string-downcase (format nil ":~A" (symbol-name x)))
                                         (if (not (some #'lower-case-p (symbol-name x)))
                                             (string-downcase (symbol-name x))
                                           (symbol-name x))))
                                   (append '(* ** ***
                                               :quit
                                               :exit
                                               :bye
                                               :disconnect
                                               :editor 
                                               :edit
                                               :open
                                               load)
                                           api))))
                stream)

        (terpri stream)

        (pprint `(defconstant +list-of-minilisp-commands+ 
                   (quote ,(sort 
                            (mapcar #'(lambda (x) 
                                       (if (not (some #'lower-case-p (symbol-name x)))
                                           (string-downcase (symbol-name x))
                                         (symbol-name x)))
                                   (append '(* ** ***)
                                           (loop as com being the hash-key in ts::+allowed-cl-functions+ collect com)
                                           ts::+native-minilisp-functions+))
                            #'string-lessp)))
                stream)

        (terpri stream)
      
        (pprint `(defconstant +list-of-query-functions+ 
                   (quote (|retrieve|
                           |retrieve1|
                           |tbox-retrieve|
                           |racer-answer-query|
                           |execute-query|
                           |get-answer|
                           |sparql-retrieve|
                           |sparql-answer-query|)))
                stream)

        (terpri stream)
      
        (pprint `(defconstant +list-of-describe-functions+
                   (quote (|describe-individual|
                           |describe-individual1|
                           |describe-role|
                           |describe-concept|
                           |describe-tbox|
                           |describe-abox|)))
                stream)

        (terpri stream)

        (pprint `(defconstant +list-of-args+ 
                   (quote ,(mapcar #'(lambda (x) 
                                       (string-downcase (symbol-name x)))
                                   '(*
                                     **
                                     ***
                          
                                     *t*
                                     *a*
                                     *c*
                                     *i*
                                     *n*
                                     *r*
                                     *qor*
                                     *def*))))
                stream)

        (terpri stream)

        (pprint `(defconstant +lambda-lists+
                   (quote ,(mapcar #'(lambda (x) 
                                       (list (symbol-name x)
                                             (let ((lambda (collect-nrql-args x)))
                                               (if (eq lambda 'unknown)
                                                   "unknown"
                                                 (ts::tree-map #'(lambda (x) 
                                                                   (if (symbolp x) 
                                                                       (symbol-name x)
                                                                     (format nil "~S" x)))
                                                               lambda)))))
                                   api)))
                stream)

        (terpri stream)

        (pprint `(progn 
                    (dolist (command +list-of-racer-symbols+)
                      (setf (gethash (string-upcase (symbol-name command)) +racer-symbols+) t))

                    (dolist (command +list-of-commands+)
                     (setf (gethash command +commands-hash+) t)
                     (let ((sym (char-downcase (elt command 0))))
                       (if (gethash sym +commands+)
                           (push command (gethash sym +commands+))
                         (setf (gethash sym +commands+) (list command)))))

                   (dolist (command +list-of-minilisp-commands+)
                     (setf (gethash (string-upcase command) +racer-symbols+) t)
                     (let ((sym (char-downcase (elt command 0))))
                       (if (gethash sym +minilisp-commands+)
                           (push command (gethash sym +minilisp-commands+))
                         (setf (gethash sym +minilisp-commands+) (list command)))))

                   (dolist (entry +lambda-lists+)
                     (let ((present (gethash (first entry) +lambda-hash+)))
                       (if present
                           (when (> (length (second entry)) 
                                    (length present)) ; mehr Info? 
                             (setf (gethash (first entry) +lambda-hash+)
                                   (second entry)))
                         (setf (gethash (first entry) +lambda-hash+)
                               (second entry)))))

                   (dolist (entry +list-of-mixed-case-commands+)
                     (setf (gethash (symbol-name entry) +mixed-case-commands-hash+) t)))
              
                stream)))))

;;;
;;; Lambda Registry for Minilisp etc.
;;;

(defun create-lambda-registry ()
  (with-open-file (stream "nrql-dev:lambda-registry.lisp"
                          :direction :output 
                          :if-does-not-exist :create
                          :if-exists :supersede)

    (let ((api 
           (set-difference 
            (get-racer-api)
            +excluded+)))

      (format stream "(in-package cl-user)~%~%")

      (format stream ";;;~%")
      (format stream ";;;----------------------------------------------~%")
      (format stream ";;;    Automatically Generated Lambda Registry   ~%")
      (format stream ";;;          Version: ~A, Build: ~A ~%" (get-product-version) (get-build-version))
      (format stream ";;;----------------------------------------------~%")
      (format stream ";;;")
    
      (terpri stream)

      (let ((*print-case* :downcase)
            (*package* (find-package :cl-user)))
        
        (pprint `(defconstant +lambda-registry+
                       (quote ,(remove nil 
                                       (mapcar #'(lambda (x) 
                                                   (when (fboundp x)
                                                     (list x
                                                           (let ((args
                                                                  (get-lambda x)))
                                                             (ts:tree-map #'(lambda (sym)
                                                                              ;;; Rewrite CLHTTP symbols! 
                                                                              ;;; Package doesn't exist in ACL
                                                                              (if (symbolp sym)
                                                                                  (if (or (eq (symbol-package sym)
                                                                                              (find-package :http))
                                                                                          (eq (symbol-package sym)
                                                                                              (find-package :url)))
                                                                                      (intern (symbol-name sym))
                                                                                    sym)
                                                                                sym))
                                                                          args)))))
                                                                                      
                                               api))))
                stream)

        (pprint '(defun get-lambda (fn) 
                   (second (assoc fn +lambda-registry+)))
                stream)

        
        (terpri stream)))))

;;;
;;;
;;;

(defun create-stubs (&optional (lracer-jracer-p t))
  #-:lispworks (break "Stub generation only works on Lispworks!")
  (load "nrql-dev:racer-registry.lisp")
  (check-server-interface)
  (create-owlapi-synonyms)
  (eval (get-owlapi-synonyms))
  (create-package-list)
  (create-lambda-registry)
  (create-server-case)
  (when lracer-jracer-p 
    (create-lracer-functions)
    (create-jracer-methods))
  (create-porter-registry)
  (create-docgen-stubs))
