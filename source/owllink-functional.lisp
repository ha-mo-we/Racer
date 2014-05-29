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

;;;
;;;;  owllink-functional.lisp
;;;
;;;
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
;;;   Purpose: The OWLlink functional processor. Core of the OWLlink implementation. 
;;; 

(defparameter *owllink-major* "1")

(defparameter *owllink-minor* "0")

(defparameter *debug-p* nil)

(defparameter *product-name* 
  #+:racer-server
  (get-product-name)
  #-:racer-server
  "OntoLisp")

(defparameter *product-version*
  #+:racer-server
  (get-product-version)
  #-:racer-server "0.9")

(defparameter *product-build*
  #+:racer-server
  (get-build-id)
  #-:racer-server "2010-07-04")

;;;
;;;
;;;

(defvar *owllink-reasoners* nil)

(defvar *owllink-kb-and-reasoner* nil)

#-:racer-server
(defvar *use-unique-name-assumption* nil)

;;;
;;;
;;;

(defparameter *owllink2-output-syntax* :owllink-xml)

(defparameter *owllink2-input-syntax* :owllink-xml)

;;;
;;;
;;;

(defvar *telling-active* nil)

(defvar *last-request-xml-and-functional-namespaces* nil)

(defvar *last-request-rendering-options* nil)

(defvar *last-request-xml-base* nil)

(defvar *owllink-kb-settings* (owlapi:mht :test #'equal))

(defvar *owllink-kb-names* (owlapi:mht :test #'equal))

;;;
;;;
;;;

(defvar *additional-answer-namespaces-used* nil)

;;;
;;;
;;;

(defun get-default-kb-settings ()
  (copy-tree ; will be changed with setf! 
   `(("uniqueNameAssumption" ,(if *use-unique-name-assumption* "true" "false"))
     #+:racer-server
     ("verbose" ,(if *tbox-verbose* "true" "false"))
     ("leanMode" "false")
     ("keepsAxioms" "true")
     ("usesLessMemory" "false")
     ("ignoresAnnotations" "false")
     ("ignoresDeclarations" "false")
     ("incremental" "true")
     ("selectedProfile" (one-of "OWL 2 DL"
                                "OWL DL"
                                "SHIQ(D-)"
                                ;;; last = default wert 
                                "OWL 2 DL"))
     ("abbreviatesIRIs" ,(if *abbreviate-iris* "true" "false")))))

;;;
;;;
;;;

(defvar *kb* nil)

(defvar *name* nil)

(defvar *ano-kb-counter* 0)

(defvar *full-iri* nil)

(defvar *key* nil) 

(defvar *value* nil)

(defvar *consider-imports* nil)

(defvar *iri* nil)

(defvar *direct* nil)

(defvar *negative* nil)

(defvar *command* nil)

(defvar *as-warning* nil)

;;;
;;;
;;; 

(defun error-message (string &rest args)
  (substitute #\' #\" (apply #'format nil string args)))

(defun get-attribute-value (attribute attributes)
  (second (assoc attribute attributes)))

(defun get-attribute-value1 (attribute attributes)
  (assoc attribute attributes))

(defun get-attributes (expressions attributes)

  ;;; starts with message name! 

  (if (and expressions attributes)
      
      (let ((matched-attributes nil)
            (other-attributes nil))

        (loop as (key1 val) in (second expressions) 
              as key = (expand-functional-tag-name key1) 
              do
              (if (member key attributes)
                  (push
                   (list key val)
                   matched-attributes)
                (push (list key1 val) other-attributes)))

        (values 
           
         matched-attributes
                 
         `(,(first expressions)
           ,other-attributes
           ,@(cddr expressions))))

    (values nil expressions)))

(defun get-all-attributes (expressions)
  (values 
   (second expressions)
   (list* (first expressions)
          nil
          (cddr expressions))))

(defun map-attributes (attributes) ; assoc-list
  attributes)

;;;
;;;
;;;

(defun make-owllink-message (tag attributes &optional content)
  (if (listp content)
      `(,tag ,(map-attributes attributes) 
             ,@content)
    `(,tag ,(map-attributes attributes) 
           ,content)))
   
(defun owllink-ok-message (&key warning)
  (make-owllink-message 
   '|OK| 
   (when warning `((|warning| ,warning)))))

(defun owllink-error-message (&key error)
  (make-owllink-message
   '|Error| 
   (when error `((|error| ,error)))))

(defun owllink-syntax-error-message (&key error)
  (make-owllink-message
   '|SyntaxError|
   (when error `((|error| ,error)))))

(defun owllink-kb-error-message (&key error warning)
  (make-owllink-message
   '|KBError| 
   `(,@(when error   `((|error| ,error)))
     ,@(when warning `((|warning| ,warning))))))

(defun owllink-semantic-error-message (&key error warning)
  (make-owllink-message
   '|SemanticError| 
   `(,@(when error   `((|error| ,error)))
     ,@(when warning `((|warning| ,warning))))))

(defun owllink-kb-message (&key warning)
  (make-owllink-message 
   '|KB| 
   (when warning `((|warning| ,warning)))))

(defun owllink-unsatisfiable-kb-error-message (&key error)
  (make-owllink-message
   '|UnsatisfiableKBError|
   (when error `((|error| ,error)))))

(defun owllink-not-supported-datatype-error (&key error)
  (make-owllink-message
   '|NotSupportedDatatypeError|
   (when error `((|error| ,error)))))

;;;
;;;
;;;

(defun owllink-boolean-response-message (result &key warning)
  (make-owllink-message
   '|BooleanResponse| 
   `((|result| ,(if result "true" "false"))
     ,@(when warning `((|warning| ,warning))))))

(defun owllink-string-response-message (result &key warning)
  (make-owllink-message
   '|StringResponse| 
   `((|result| ,result)
     ,@(when warning `((|warning| ,warning))))))

(defun owllink-unknown-message (&key warning)
  (make-owllink-message
   '|Unknown| 
   `(,@(when warning `((|warning| ,warning))))))

(defun owllink-to-be-implemented-message (type)
  (owllink-error-message 
   :error
   (error-message "OWLlink request ~A not yet implemented" type)))

;;;
;;;
;;;

(defun owllink-init ()
  (clrhash *owllink-kb-settings*)
  (dolist (reasoner *owllink-reasoners*)
    (unregister-owllink-reasoner reasoner))
  (setf *owllink-kb-and-reasoner* nil
        *owllink-reasoners* nil))

(defmethod register-owllink-reasoner ((reasoner symbol))
  (push reasoner *owllink-reasoners*))

(defmethod register-owllink-reasoner ((reasoner owlapi:reasoner))
  (register-owllink-reasoner (owlapi:owlapi-reasoner-name reasoner)))

(defmethod unregister-owllink-reasoner ((reasoner symbol))
  (let ((reasoner (find-reasoner reasoner nil)))
    (when reasoner
      (unregister-owllink-reasoner reasoner))))

(defmethod unregister-owllink-reasoner ((reasoner owlapi:reasoner))
  (setf *owllink-reasoners*
        (delete (owlapi:owlapi-reasoner-name reasoner) *owllink-reasoners*))
  (unregister-kb-name reasoner)
  (clear-kb-settings reasoner))

(defun owllink-create-kb (kb &optional name)
  (|OWLAPI-newReasoner| kb)
  (|OWLAPI-newOntology| kb kb)

  (|OWLAPI-registerReferencedEntities| kb)

  (|OWLAPI-registerDeclaredEntities| kb)  
    
  (register-owllink-reasoner kb)

  (when name
    (register-kb-name kb name))

  (setf (gethash kb *owllink-kb-settings*)
        (get-default-kb-settings) )
  
  kb)

(defun owllink-release-kb (&optional (kb *owllink-kb-and-reasoner*))
  (unregister-owllink-reasoner kb)
  (|OWLAPI-disposeReasoner| kb))
          
;;;
;;;
;;; 

(defun register-kb-name (kb name)
  (setf (gethash kb *owllink-kb-names*) name))

(defun unregister-kb-name (&optional (kb *owllink-kb-and-reasoner*))
  (remhash kb *owllink-kb-names*))

(defun get-kb-name (&optional (kb *owllink-kb-and-reasoner*))
  (gethash kb *owllink-kb-names*))

;;;
;;;
;;; 

(defun normalize-prefix (prefix)
  (when prefix
    (let ((string (owlapi:ensure-string prefix)))
      (if (member string '("default" "" "nil" "NIL")
                  :test #'string-equal)
          ""
        string))))

(defun normalize-url (url)
  (when url ; nil bleibt nil! 
    (remove-sharp-brackets
     (owlapi:ensure-string url))))

(defun get-kb-prefixes (&optional (kb *owllink-kb-and-reasoner*))
  (gethash (list kb :prefixes) *owllink-kb-settings*))

(defun set-kb-prefixes (prefixes &optional (kb *owllink-kb-and-reasoner*))
  (setf (gethash (list kb :prefixes) *owllink-kb-settings*)
    prefixes))

;;;
;;;
;;;

(defun clear-kb-settings (&optional (kb *owllink-kb-and-reasoner*))
  (remhash kb *owllink-kb-settings*)
  (remhash (list kb :prefixes) *owllink-kb-settings*))

(defun set-kb-setting (key val &optional (kb *owllink-kb-and-reasoner*))
  (let* ((settings
          (gethash kb *owllink-kb-settings*) )
         (pos 
          (position key settings :test #'string-equal :key #'first)))
    (if pos
        (setf (nth pos settings) 
              (let* ((entry (nth pos settings))
                     (old-key (second entry)))
                (if (and (consp old-key)
                         (eq (first old-key) 'one-of))
                    (let ((new  
                           (list key 
                                 (append (butlast old-key) (list val)))))
                      new)
                  (list key val))))

      (if settings 
          (nconc settings (list (list key val)))
        (setf (gethash kb *owllink-kb-settings*)
              (list (list key val))))))

  (case (intern (owlapi:ensure-string key))
    (|una| ) ; 
    (|verbose| ) ;
    (|abbreviatesIRIs| ) ; 
    (|selectedProfile| ); 
    (|keepsAxioms| 
     (if (string-to-boolean val)
         (|OWLAPI-disableMemorySavingMode| kb)
       (|OWLAPI-enableMemorySavingMode| kb kb)))
    (|leanMode| 
     (if (not (string-to-boolean val))
         (|OWLAPI-disableMemorySavingMode| kb)
       (|OWLAPI-enableMemorySavingMode| kb kb)))
    (|ignoresAnnotations| 
     (if (string-to-boolean val)
         (|OWLAPI-ignoreAnnotations| kb)  
       (|OWLAPI-keepAnnotations| kb)))
    (|ignoresDeclarations| 
     (if (string-to-boolean val)
         (|OWLAPI-ignoreDeclarations| kb)
       (|OWLAPI-considerDeclarations| kb)))
    (|incremental| 
     (if (string-to-boolean val)
         (|OWLAPI-enableIncrementalUpdates| kb)
       (|OWLAPI-disableIncrementalUpdates| kb)))))

  

(defun unset-kb-setting (key &optional (kb *owllink-kb-and-reasoner*))
  (let* ((settings
          (gethash kb *owllink-kb-settings*) ))
    (setf (gethash kb *owllink-kb-settings*) 
          (delete key settings :key #'first))))

(defun get-kb-setting (key &optional (kb *owllink-kb-and-reasoner*))
   (let* ((settings
          (gethash kb *owllink-kb-settings*)))
     (second (assoc key settings :test #'equal))))       

(defun get-kb-settings (&optional (kb *owllink-kb-and-reasoner*))
  (gethash kb *owllink-kb-settings*))

;;;
;;;
;;;

(defmacro with-consistent-kb (&body body)
  (let ((unsat-concepts (gensym))
        (tbox-unsat (gensym))
        (abox-sat (gensym))
        (sat (gensym)))
    `(let* ((,unsat-concepts
             (reasoner-atomic-concept-synonyms 'bottom *owllink-kb-and-reasoner*))
            (,tbox-unsat
             (member 'top ,unsat-concepts))
            (,abox-sat
             (reasoner-abox-consistent-p *owllink-kb-and-reasoner*))
            (,sat (and (not ,tbox-unsat) ,abox-sat)))

       (cond (,sat
              ,@body)
             (t
              (values 
               (owllink-unsatisfiable-kb-error-message 
                :error 
                (format nil "TBox satisfiable: ~A. ABox satisfiable: ~A. Unsatisfiable classes: ~A"
                       (yes-or-no-string (not ,tbox-unsat))
                       (yes-or-no-string ,abox-sat)
                       ,unsat-concepts))
               expressions))))))

(defmacro with-consistent-abox (&body body)
  (let ((abox-sat (gensym)))
    `(let* ((,abox-sat
             (reasoner-abox-consistent-p *owllink-kb-and-reasoner*)))

       (cond (,abox-sat
              ,@body)
             (t
              (values 
               (owllink-unsatisfiable-kb-error-message 
                :error 
                (format nil "ABox satisfiable: ~A."
                        (yes-or-no-string ,abox-sat)))
               expressions))))))
  
(defmacro with-consistent-tbox (&body body) ; = satisfiable 
  (let ((unsat-concepts (gensym))
        (tbox-unsat (gensym)))
    `(let* ((,unsat-concepts
             (reasoner-atomic-concept-synonyms 'bottom *owllink-kb-and-reasoner*))
            (,tbox-unsat
             (member 'top ,unsat-concepts)))
       (cond ((not ,tbox-unsat)
              ,@body)
             (t
              (values 
               (owllink-unsatisfiable-kb-error-message 
                :error 
                (format nil "TBox satisfiable: ~A. Unsatisfiable classes: ~A"
                        (yes-or-no-string (not ,tbox-unsat))
                        ,unsat-concepts))
               expressions))))))
  
(defmacro with-present-kb (&body body)
  `(if *owllink-kb-and-reasoner*
       (handler-case 
           (let* ((*use-unique-name-assumption* 
                   (string-to-boolean (get-kb-setting "uniqueNameAssumption")))
                  #+:racer-server
                  (*tbox-verbose* 
                   (string-to-boolean (get-kb-setting "verbose")))
                  #+:racer-server
                  (*abox-verbose* *tbox-verbose*)
                  #+:racer-server
                  (*use-less-tbox-memory*
                   (string-to-boolean (get-kb-setting "usesLessMemory")))
                  (*abbreviate-iris* t))
             (with-reasoner (*owllink-kb-and-reasoner*)
               ,@body))
         (error (error)
           (owllink-semantic-error-message
            :error (error-message "Reasoning error ~A occured" error))))
     (values (owllink-kb-error-message 
              :error 
              (if *kb* 
                  (error-message "KB ~A not found. Request denied" *kb*)
                (error-message "No 'kb' attribute specified for request. Request denied")))
             (rest expressions))))

;;;
;;;
;;;                      

(dummy-tag-parser |FunctionalRequestMessageWithHeader|
  (multiple-value-bind (result expressions)
      (|owllink-parse-FunctionalHeaders| expressions)
    (declare (ignore result))
    (|owllink-parse-RequestMessage| expressions)))

(ano-optional-loop-parser |FunctionalHeaders| |FunctionalHeader|)

(optional-choice-parser |FunctionalHeader| nil
    (|NamespacePrefix| 
     |RenderingOption|))

;;;
;;;
;;;

(loop-parser |Ontologies| 
    ()
    |owl.Ontology|
    nil

  (owllink-ok-message))

(dummy-tag-parser |owl.Ontology| 
  (parse-ontology expressions))

;;;
;;;
;;;


(loop-parser |RequestMessage| 
    (nil (let ((*owl-xml-mode* t)
               (*use-xml-base* 
                *last-request-xml-base*))))
    |Request|
    (setf *additional-answer-namespaces-used* nil)

  (make-owllink-message 
   '|ResponseMessage| 

   `((|INT-abbreviatesIRIs| "true")
     (|INT-prefixes|
       ,(loop as ns in *additional-answer-namespaces-used* 
              as prefix 
              = (first 
                 (find ns
                       *last-request-xml-and-functional-namespaces*
                       :test #'string=
                       :key #'cdr ; !! 
                       ))
              when prefix
              collect 
              (list prefix ns))))
    
   (nconc 
    
    #+:ignore
    (mapcar #'(lambda (x) 
               (make-owllink-message '|NamespacePrefix| nil (list (car x) (cdr x))))
           *last-request-xml-and-functional-namespaces*)

    response)))

(choice-parser-with-timeout |Request| nil
    (#+:racer-server
     |http://www.racer-systems.com/owllink/ext/racer#Racer|
     #+:racer-server
     |http://www.racer-systems.com/owllink/ext/racer#FullReset|

     |GetDescription|
     |CreateKB|
     |KBRequest|
     ))

(choice-parser |KBRequest| 
    ((|kb|
      |http://www.owllink.org/ext/retraction#kb|
      |<http://www.owllink.org/ext/retraction#kb>|)
     (let* ((*kb* (or (get-attribute-value '|kb| attributes)
                      (get-attribute-value 
                       '|http://www.owllink.org/ext/retraction#kb|
                       attributes)
                      (get-attribute-value 
                       '|<http://www.owllink.org/ext/retraction#kb>|
                       attributes)))
	    (*kb* (normalize-url *kb*))
            (*owllink-kb-and-reasoner* 
             (find-reasoner
              (intern *kb*)
              nil)))
       (when *owllink-kb-and-reasoner*
         (setf *owllink-kb-and-reasoner*
               (owlapi:owlapi-reasoner-name *owllink-kb-and-reasoner*)))))

    (|http://www.owllink.org/ext/retraction#Retract|
     
     ;;; 

     |GetPrefixes|
     |GetSettings|
     |Set|
     |ReleaseKB|
     |Tell|
     |LoadOntologies|
     |Classify|
     |Realize|
     |GetAllObjectProperties|
     |GetAllDataProperties|
     |GetAllAnnotationProperties|
     |GetAllDatatypes|
     |GetAllIndividuals|
     |GetAllClasses|
     |IsKBSatisfiable|
     |IsKBConsistentlyDeclared|
     |GetKBLanguage|
     |IsClassSatisfiable|
     |IsClassSubsumedBy|
     |AreClassesEquivalent|
     |AreClassesDisjoint|
     |GetDisjointClasses|
     |GetFlattenedDisjointClasses|
     |GetSubClasses|
     |GetSuperClasses|
     |GetFlattenedSubClasses|
     |GetFlattenedSuperClasses|
     |GetEquivalentClasses|
     |GetSubClassHierarchy|
     |GetSuperClassHierarchy|
     |AreObjectPropertiesEquivalent|
     |IsObjectPropertySatisfiable|
     |IsObjectPropertySubsumedBy|
     |AreObjectPropertiesDisjoint|
     |IsObjectPropertyFunctional|
     |IsObjectPropertyInverseFunctional|
     |IsObjectPropertyReflexive|
     |IsObjectPropertyIrreflexive|
     |IsObjectPropertySymmetric|
     |IsObjectPropertyAsymmetric|
     |IsObjectPropertyTransitive|
     |GetSubObjectProperties|
     |GetSuperObjectProperties|
     |GetEquivalentObjectProperties|
     |GetDisjointObjectProperties|
     |GetFlattenedDisjointObjectProperties|
     |GetFunctionalObjectProperties|
     |GetInverseFunctionalObjectProperties|
     |GetReflexiveObjectProperties|
     |GetIrreflexiveObjectProperties|
     |GetSymmetricObjectProperties|
     |GetAsymmetricObjectProperties|
     |GetTransitiveObjectProperties|
     |GetSubObjectPropertyHierarchy|
     |GetSuperObjectPropertyHierarchy|
     |AreDataPropertiesEquivalent|
     |AreDataPropertiesDisjoint|
     |IsDataPropertySatisfiable|
     |IsDataPropertySubsumedBy|
     |IsDataPropertyFunctional|
     |GetSubDataProperties|
     |GetSuperDataProperties|
     |GetEquivalentDataProperties|
     |GetFunctionalDataProperties| 
     |GetDisjointDataProperties| 
     |GetFlattenedDisjointDataProperties| 
     |GetSubDataPropertyHierarchy|
     |GetSuperDataPropertyHierarchy|
     |AreIndividualsEquivalent|
     |AreIndividualsDisjoint|
     |IsInstanceOf|
     |GetTypes|
     |GetFlattenedTypes|
     |GetDisjointIndividuals| 
     |GetDifferentIndividuals| 
     |GetFlattenedDisjointIndividuals| 
     |GetFlattenedDifferentIndividuals| 
     |GetEquivalentIndividuals|
     |GetSameIndividuals|
     |GetObjectPropertiesBetween|
     |GetFlattenedObjectPropertiesBetween|
     |GetObjectPropertiesOfSource|
     |GetFlattenedObjectPropertiesOfSource|
     |GetObjectPropertiesOfTarget|
     |GetFlattenedObjectPropertiesOfTarget|
     |AreIndividualsRelated|
     |IsIndividualRelatedWithLiteral| 
     |GetDataPropertiesBetween|
     |GetFlattenedDataPropertiesBetween|
     |GetDataPropertiesOfSource|
     |GetFlattenedDataPropertiesOfSource|
     |GetDataPropertiesOfLiteral|
     |GetFlattenedDataPropertiesOfLiteral|
     |GetInstances|
     |GetFlattenedInstances|
     |GetObjectPropertyTargets|
     |GetFlattenedObjectPropertyTargets|
     |GetObjectPropertySources|
     |GetFlattenedObjectPropertySources|
     |GetDataPropertyTargets|
     |GetDataPropertySources|
     |GetFlattenedDataPropertySources|

     ;;; 

     |IsEntailedDirect|
     |IsEntailed|
     
     )

  (values 
   (if (third response) ; content? 
       (make-owllink-message (first response)
                             (nconc 
                              (second response)
                              `((|INT-abbreviatesIRIs| 
                                 ,(get-kb-setting "abbreviatesIRIs"))
                                (|INT-kb| ,*kb*)
                                (|INT-prefixes| 
                                 ,(get-kb-prefixes *owllink-kb-and-reasoner*))))
                             (when (third response)
                               (cddr response)))
     (make-owllink-message (first response) (second response)))
   expressions))

;;;
;;; 
;;;

(defun boolean-type-p (x)
  (or (eq x t) (eq x nil)
      (string= x "true")
      (string= x "false")))

(defun guess-type (value)
  (typecase value
    ((satisfies boolean-type-p)
     '|xsd:boolean|)
    (string
     '|xsd:string|)
    (otherwise
     '|xsd:anyURI|)))

(defun map-configuration (type key values)
  (let ((values (owlapi:ensure-list values)))

    (make-owllink-message 
     type
     
     `((|key| ,(if (symbolp key) 
                   (symbol-name key)
                 key)))
     
     (cons

      (if (cdr values)
          (if (eq (first values) 'one-of)
              (make-owllink-message 
               '|OneOf| 
               `((|type| ,(guess-type (second values))))
               (mapcar #'(lambda (x) 
                           (make-owllink-message
                            '|Literal| 
                            `((|value| ,(format nil "~A" x)))))
                       (butlast (cdr values)))) ; last = default!
            (make-owllink-message
             '|List| 
             `((|type| ,(guess-type (first values))))))
        (make-owllink-message
         '|Datatype| 
         `((|IRI| ,(guess-type (first values))))))
      
      (mapcar #'(lambda (x) 
                  (make-owllink-message
                   '|Literal| 
                   `((|value| ,(format nil "~A" x)))))
              
              (if (eq (first values) 'one-of)
                  (last values)
                values))))))

(defun map-property (key values)
  (map-configuration '|Property| key values))

(defun map-setting (key values)
  (map-configuration '|Setting| key values))

(tag-parser |GetDescription| nil 
  (let ((name *product-name*)
        (message nil)
        (warning nil))

    (values 

     (make-owllink-message
      '|Description| 
      
      `(,@(when warning
            `((|warning| ,warning)))
        ,@(when name
            `((|name| ,name)))
        ,@(when message
            `((|message| ,message))))

      (nconc 

       (list 
        (make-owllink-message 
        '|ProtocolVersion| 
        `((|major| ,*owllink-major*)
          (|minor| ,*owllink-minor*))))

       (list 
        (make-owllink-message
        '|ReasonerVersion| 
        `((|major| ,(subseq *product-version*
                            0
                            (position #\. *product-version*)))
          (|minor| ,(subseq *product-version*
                            (1+ 
                             (position #\. *product-version*))))
          (|build| ,*product-build*))))

       (mapcar #'(lambda (prop) 
                   (apply #'map-property prop))
               `((|appliedSemantics| 
                  (one-of "direct" "direct"))
                 (|supportedDatatypes| 
                  ,(mapcar #'(lambda (dtp)
                               (owlapi:make-uri "xsd" dtp))
                           '(|negativeInteger|
                             |nonNegativeInteger|
                             |positiveInteger|
                             |nonPositiveInteger|
                             |integer|
                             |byte|
                             |unsignedByte|
                             |unsignedShort|
                             |unsignedInt|
                             |unsignedLong|
                             |short|
                             |int|
                             |long|
                             |float|
                             |double|
                             |decimal|
                             |string|
                             |boolean|)))))

       (mapcar #'(lambda (prop) 
                   (apply #'map-setting 
                          (list (intern (owlapi:ensure-string (first prop)))
                                (second prop))))
               (get-default-kb-settings))

       (list (make-owllink-message 
              '|SupportedExtension| 
              '((|identifier| "http://www.racer-systems.com/owllink/ext/racer"))))
       
       (list (make-owllink-message 
              '|SupportedExtension| 
              '((|identifier| "http://www.owllink.org/ext/retraction"))))
                     
       (mapcar #'(lambda (kb)
                   (make-owllink-message 
                    '|PublicKB| 
                    `((|name| ,(get-kb-name kb))
                      (|kb| ,kb))))
               (remove-if-not #'get-kb-name *owllink-reasoners*))))

     expressions)))

;;;
;;;
;;; 

(tag-parser |CreateKB| 
    ((|kb| |name|) 
     (let ((*kb* (normalize-url (get-attribute-value '|kb| attributes)))
           (*name* (get-attribute-value '|name| attributes)))))

  (with-parser-call (|Prefixes| expressions)
    (let ((prefixes response)
          (kb
           (intern
            (or *kb*
                (format nil "http://www.racer-systems.com/kb-~A/"
                        (incf *ano-kb-counter*))))))

      (if (find-reasoner kb nil)
          (owllink-kb-error-message :error
                                    (error-message "KB ~A already exists, request denied"
                                                   kb))
        (progn 

          (owllink-create-kb kb *name*)
          
          (let ((prefixes 
                 (nconc 
                  (loop as (prefix . namespace) in *last-request-xml-and-functional-namespaces* 
                        unless (member prefix '("owl" "xsd" "rdf" "rdfs") :test #'string-equal)
                        collect (list (or prefix
                                          "defaultnamespace")
                                      namespace))
                  prefixes)))

            (set-kb-prefixes prefixes kb)
          
            (loop as (prefix namespace) in prefixes do
                  (owlapi:|OWLAPI-addPrefix| prefix
                                             namespace kb)))

          (make-owllink-message 
           '|KB| 
           `((|kb| ,kb)
             #+:ignore
             ,@(when *name*
                 `((|name| ,*name*)))
             )))))))


(ano-loop-parser |Prefixes| |Prefix|)

(tag-parser |Prefix| 
    ((|name| |fullIRI|)
     (let ((*name* (get-attribute-value '|name| attributes))
           (*full-iri* (get-attribute-value '|fullIRI| attributes)))))

    (list (normalize-prefix *name*)
	  (normalize-url *full-iri*)))

;;;
;;;
;;;

(tag-parser |GetSettings| 
    nil
  (with-present-kb
    (make-owllink-message 
     '|Settings| 
     nil
     (mapcar #'(lambda (setting)
                 (apply #'map-setting setting))
             (get-kb-settings)))))

;;;
;;;
;;;

(tag-parser |GetPrefixes| 
    nil
  (with-present-kb
    (make-owllink-message 
     '|Prefixes| 
     nil
     (mapcar #'(lambda (prefix)
                 (make-owllink-message '|Prefix| 
                                       `((|name| ,(first prefix))
                                         (|fullIRI| ,(second prefix)))))
             (remove-if #'(lambda (x)
                            (or (member (first x) 
					'("xsi" "defaultnamespace" "NIL" "nil")
					:test #'string-equal)
				#+:ignore 
                                (find (cons (first x) (second x))
				      *last-request-xml-and-functional-namespaces* 
				      :test #'equal)))
                        (append
                         (remove-duplicates 
                          (append 
                           (get-kb-prefixes *owllink-kb-and-reasoner*)
                           +owl2-standard-prefixes+)
                          :test #'equal)))))))

;;;
;;;
;;;
              
(tag-parser |Set| 
    ((|key|) 
     (let ((*key* (get-attribute-value '|key| attributes)))))
  (with-present-kb
    (with-parser-call (|Literals| expressions)
      (let ((cur (get-kb-setting *key*))
            (lit (first (owlapi:ensure-list  response))))
        
        (if cur
            (progn 
              (typecase cur
                (cons ; one-of? 
                 (if (member lit cur :test #'string-equal)
                     (progn 
                     (set-kb-setting *key* lit)
                     (owllink-ok-message))
                   (owllink-kb-error-message
                    :error
                    (error-message "Setting ~A cannot be set for KB ~A - ~A is not a valid value for ~S. Request denied" *key* *owllink-kb-and-reasoner* lit (butlast cur)))))
                
                (otherwise 
                 (set-kb-setting *key* lit)
                 (owllink-ok-message))))

          (owllink-kb-error-message
           :error
           (error-message "Setting ~A cannot be set for KB ~A. Request denied" *key* *owllink-kb-and-reasoner*)))))))

(ano-loop-parser |Literals| |Literal|)

(tag-parser |Literal| 
    ((|value|) ; Special treatment f. Literal: Value = Content!
     (let ((*value* (get-attribute-value '|value| attributes)))))
  (or *value* 
      (first expressions) ; content !
      ))
         
(tag-parser |ReleaseKB| 
    nil
  (with-present-kb
    (owllink-release-kb)
    (owllink-ok-message)))

;;;
;;;
;;;

(loop-parser |Tell| nil 
    |owl.Axiom|    
    (with-present-kb
      (|OWLAPI-autoBatchAddAxiomsTo| *owllink-kb-and-reasoner*
                                       *owllink-kb-and-reasoner*))
  (with-present-kb
      (let ((errors (remove-if #'(lambda (x) 
				   (or (numberp x)
				       (eq x :void)))
			       response)))

      (cond ((not (owlapi:dont-keep-axioms-p (find-reasoner *owllink-kb-and-reasoner*)))
             (|OWLAPI-loadOntology|
              *owllink-kb-and-reasoner*
              *owllink-kb-and-reasoner*))
            (t
             ))
      
      (|OWLAPI-batchSynchronize| *owllink-kb-and-reasoner* *owllink-kb-and-reasoner*)
      
      (if errors
          (owllink-syntax-error-message 
           :error 
           (error-message "Ignored non-valid OWLlink Tell requests: ~S"
                   errors))
        (owllink-ok-message)))))

(loop-parser |http://www.owllink.org/ext/retraction#Retract| nil
    |owl.Axiom|
    (with-present-kb 
      (when (owlapi:dont-keep-axioms-p (find-reasoner *owllink-kb-and-reasoner*))
        (return-from parser
          (values 
           (owllink-semantic-error-message 
            :error
            "Retraction is only possible if memory saving mode is disabled.")
           (rest expressions1))))
      (|OWLAPI-autoBatchRemoveAxiomsFrom| *owllink-kb-and-reasoner*))

  (with-present-kb
    (let ((errors (remove-if #'numberp response)))

      (cond ((not (owlapi:dont-keep-axioms-p (find-reasoner *owllink-kb-and-reasoner*)))
             )
            (t
             ))
    
      (|OWLAPI-batchSynchronize| *owllink-kb-and-reasoner* *owllink-kb-and-reasoner*)

      (|OWLAPI-disableAutoMode| *owllink-kb-and-reasoner*)
   
      (if errors
          (owllink-syntax-error-message 
           :error 
           (error-message "Ignored non-valid OWLlink Tell requests: ~S"
                   errors))
        (owllink-ok-message)))))

(dummy-tag-parser |owl.Axiom| 
  (with-present-kb
    (multiple-value-bind (axiom-id expressions)
        (handler-case (parse-axiom expressions)
          (error () 
            (values (first expressions)
                    (rest expressions))))
      (values 
       axiom-id
       expressions))))

(defun substitute-name (name mappings)
  (loop as (source target) in mappings
        when (let ((pos (search source name)))
               (and (numberp pos)
                    (= 0 pos)))
        return
        (concatenate 'string
                     target
                     (subseq name (length source)))))

(tag-parser |LoadOntologies| 
    ((|considerImports|)
     (let ((*consider-imports* 
            (let ((res (get-attribute-value1 '|considerImports| attributes)))
              (if res 
                  (second (string-to-boolean res))
                t))))))

  (with-present-kb
    (with-parser-call (|IRIMappings-OntologyIRIs| expressions)
      (let* ((load (remove-if #'consp response))
             (mappings (remove-if-not #'consp response))
             (*owllink-mirror-mappings* 
              (append
               mappings
               *owllink-mirror-mappings*))
             (error (find '|SyntaxError| mappings :key #'first)))

        (if error 
            (values error
                    (rest expressions))
          (progn 

            (let* ((error-p nil)
                   (answers nil)
                   (table nil))
              
              (declare (ignorable table))

              #+:racer-server
              (maphash #'(lambda (key val)
                           (push (list key val) table))
                       *uri-mirror-table*)

              #+:racer-server
              (clear-mirror-table)

              #+:racer-server
              (loop as (source target) in mappings do
                    (mirror source target))
              
              (dolist (ont load)
                (let* ((ont (or (substitute-name ont *owllink-mirror-mappings*)
                                ont)))
                  
                  (push 
                   (list ont
                         '--> 
                         (handler-case
                             (let ((*package*
                                    (find-package :owl-syntaxes)))

                               (owlapi-import-ontology ont
                                                       :maintain-owlapi-axioms 
                                                       (not (owlapi:dont-keep-axioms-p *cur-reasoner*))
                                                       :init nil
                                                       :reasoner-name *owllink-kb-and-reasoner*))

                           (error (error)
                             (setf error-p t)
                             (error-message "In LoadOntologies of ~S: ~A" 
                                            ont error))))

                   answers)))

              #+:racer-server
              (loop as (prefix url) in table do
                    (mirror prefix url))

              (if error-p
                  
                  (values (owllink-error-message :error (error-message "~S" answers))
                          (rest expressions))

                (values (owllink-ok-message :warning (format nil "~A" answers))
                        (rest expressions))))))))))
                                       

(ano-loop-parser |IRIMappings-OntologyIRIs| |IRIMapping-OntologyIRI|)

(choice-parser |IRIMapping-OntologyIRI|
    nil
    (|IRIMapping|
     |OntologyIRI|))

(tag-parser |IRIMapping| 
    ((|key| |value| |IRI|)
     (let* ((*key* (get-attribute-value '|key| attributes))
            (*value* (or (get-attribute-value '|value| attributes)
                         (get-attribute-value '|IRI| attributes)))
            (*key* (normalize-prefix *key*))
            (*value* (normalize-url *value*)))))
  (if (not (and *key* *value*))
      (values (owllink-syntax-error-message
               :error (error-message "Incomplete IRIMapping specification in ~S" (first expressions)))
              expressions)
    (values `(,*key* ,*value*)
            expressions)))

(tag-parser |OntologyIRI| 
    ((|IRI|)
     (let ((*iri* (normalize-url 
                   (get-attribute-value '|IRI| attributes))))))
  (values *iri* expressions))

;;;
;;; 
;;;

(tag-parser |Classify| 
    ()
  (with-present-kb
    (reasoner-taxonomy *owllink-kb-and-reasoner*)
    (values (owllink-ok-message)
            expressions)))

(tag-parser |Realize| 
    ()
  (with-present-kb
    (with-consistent-abox
      (reasoner-realize *owllink-kb-and-reasoner*)
      (values (owllink-ok-message)
              expressions))))

;;;
;;; 
;;;

(tag-parser |IsEntailedDirect| 
    ()
  (with-present-kb
    (owlapi:with-transient-axiom-mode (*owllink-kb-and-reasoner*)
      (let ((old-expressions expressions))
        (handler-case 
            (multiple-value-bind (axiom expressions)
                (parse-axiom expressions)
              (declare (ignorable expressions))
              (if (not (typep axiom '|OWLAxiom|))
                  (owllink-syntax-error-message
                   :error (error-message "Bad Axiom in ~S" (first old-expressions)))
                (with-consistent-kb
                  (values 
                   (let ((res (or (and (owlapi:axiom-id axiom) ; old and loaded? entailed! 
                                       (owlapi:loaded-p axiom))
                                  (owllink-direct-entailed-p axiom))))
                     (if (eq res :unknown)
                         (owllink-unknown-message 
                          :warning 
                          (format nil "Request IsEntailedDirect is not yet implemented for axioms of type ~A" 
                                  (type-of axiom)))
                       (owllink-boolean-response-message 
                        res)))
                   expressions))))
          (error (error)
            (values
             (owllink-semantic-error-message
              :error (error-message "Reasoning error ~A occured" error))
             expressions)))))))

(tag-parser |IsEntailed| 
    ()
  (with-present-kb
    (owlapi:with-transient-axiom-mode (*owllink-kb-and-reasoner*)
      (let ((old-expressions expressions))
        (handler-case 
            (multiple-value-bind (axiom expressions)
                (parse-axiom expressions)
              (declare (ignorable expressions))
              (if (not (typep axiom '|OWLAxiom|))
                  (owllink-syntax-error-message
                   :error (error-message "Bad Axiom in ~S" (first old-expressions)))
                (with-consistent-kb
                  (values 
                   (let ((res (or (and (owlapi:axiom-id axiom) ; old and loaded? entailed! 
                                       (owlapi:loaded-p axiom))
                                  (owllink-entailed-p axiom))))
                     (if (eq res :unknown)
                         (owllink-unknown-message 
                          :warning
                          (format nil "Request IsEntailed is not yet implemented for axioms of type ~A" 
                                  (type-of axiom)))
                       (owllink-boolean-response-message 
                        res)))
                   expressions))))
          (error (error)
            (values
             (owllink-semantic-error-message
              :error (error-message "Reasoning error ~A occured" error))
             expressions)))))))

;;;
;;;
;;;

(defun make-set-of (plural singular set &optional (form (format nil "SetOf~A" plural)))
  (make-owllink-message
   (if (symbolp form) 
       form
     (intern form))
   nil
   (mapcar #'(lambda (x) 
               (if (consp x)
                   (if (eq (first x) singular)
                       x
                     :error)
                 (make-owllink-message singular nil x)))
           (remove-duplicates set :test #'equal))))

(defun make-set-of-synsets (type set-of-synsets &optional set-of-syntax-p)
  (make-owllink-message
   (if set-of-syntax-p 
       (intern (format nil "SetOf~ASynsets" type))
     (intern (format nil "~ASynsets" type)))
   nil
   (remove-duplicates set-of-synsets :test #'equal)))

(defun make-synset (type set &optional marker)
  (when set
    (make-owllink-message
     (or marker (intern (format nil "~ASynset" type)))
     nil
     (mapcar #'(lambda (x) 
                 (if (consp x)
                     (if (eq (first x) type)
                         x
                       :error)
                   (make-owllink-message type nil x)))
             (remove-duplicates set :test #'equal)))))


(defun make-equi-set (type set &optional marker)
  (when set
    (make-owllink-message
     (or marker (intern (format nil "Equivalent~A" type)))
     nil
     (mapcar #'(lambda (x) 
                 (make-owllink-message type nil x))
             set))))


(defun make-set-of-individuals (set)
  (make-owllink-message
   '|SetOfIndividuals|
   nil
   (mapcar #'(lambda (x) 
                  (if (is-ano-ind-p x)
                      (make-owllink-message '|AnonymousIndividual| nil (without-ano-ind-prefix x))
                    (make-owllink-message '|NamedIndividual| nil x)))
              set)))

(defun make-individual-synset (set)
  (when set
    (make-owllink-message
     '|IndividualSynset| 
     nil
     (mapcar #'(lambda (x) 
                 (if (is-ano-ind-p x)
                     (make-owllink-message '|AnonymousIndividual| nil (without-ano-ind-prefix x))
                   (make-owllink-message '|NamedIndividual| nil x)))
             set))))


(defun make-individual-synonyms (set)
  (when set
    (make-owllink-message
     '|IndividualSynonyms| 
     nil
     (mapcar #'(lambda (x) 
                 (if (is-ano-ind-p x)
                     (make-owllink-message '|AnonymousIndividual| nil (without-ano-ind-prefix x))
                   (make-owllink-message '|NamedIndividual| nil x)))
             set))))

;;;
;;;
;;;

(defun remove-concept-by-synset (concept concepts)
  (let ((syns (reasoner-atomic-concept-synonyms concept *owllink-kb-and-reasoner*)))
    (remove-if #'(lambda (x) 
                   (find x syns))
               concepts)))

(defun remove-role-by-synset (role roles)
  (cond ((or (eq role owlapi:+owlapi-owl-top-object-role+)
             (eq role owlapi:+owlapi-owl-bottom-object-role+)
             (eq role owlapi:+owlapi-owl-top-data-role+)
             (eq role owlapi:+owlapi-owl-bottom-data-role+))
         (remove role roles))

        (t 
         (let ((syns
                (reasoner-equivalent-roles role *owllink-kb-and-reasoner*)))
           (remove-if #'(lambda (x) 
                          (find x syns))
                      roles)))))

(defun remove-ind-by-synset (ind inds)
  (let ((syns (reasoner-individual-synonyms ind *owllink-kb-and-reasoner*)))
    (remove-if #'(lambda (x) 
                   (find x syns))
               inds)))

;;;
;;;
;;; 

(tag-parser |GetAllObjectProperties|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (values 
     (make-set-of '|ObjectProperties| '|ObjectProperty|
                  #+:ignore 
                  (owllink-get-object-properties)
                  (remove-duplicates
                   (let ((res nil))
                     (dolist (ont (owlapi:get-ontologies *owllink-kb-and-reasoner*))
                       (loop as p being the hash-key in (owlapi:declared-object-properties ont)
                             do (if (consp p)
                                    (push (second p) res)
                                  (push p res)))
                       (loop as p being the hash-key in (owlapi:referenced-object-properties ont)
                             do (if (consp p)
                                    (push (second p) res)
                                  (push p res))))
                     res)))

     expressions)))

(tag-parser |GetAllDataProperties|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (values 
     (make-set-of '|DataProperties| '|DataProperty|
                  #+:ignore 
                  (owllink-get-data-properties)
                  (remove-duplicates
                   (let ((res nil))
                     (dolist (ont (owlapi:get-ontologies *owllink-kb-and-reasoner*))
                       (loop as p being the hash-key in (owlapi:declared-data-properties ont)
                             do (if (consp p)
                                    (push (second p) res)
                                  (push p res)))
                       (loop as p being the hash-key in (owlapi:referenced-data-properties ont)
                             do (if (consp p)
                                    (push (second p) res)
                                  (push p res))))
                     res)))

     expressions)))


(tag-parser |GetAllAnnotationProperties|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (values 
     (make-set-of '|AnnotationProperties| '|AnnotationProperty|
                  #+:ignore
                  (owllink-get-annotation-properties)
                  (remove-duplicates
                   (let ((res nil))
                     (dolist (ont (owlapi:get-ontologies *owllink-kb-and-reasoner*))
                       (loop as p being the hash-key in (owlapi:declared-annotation-properties ont)
                             do (if (consp p)
                                    (push (second p) res)
                                  (push p res)))
                       (loop as p being the hash-key in (owlapi:referenced-annotation-properties ont)
                             do (if (consp p)
                                    (push (second p) res)
                                  (push p res))))
                     res)))
     
     expressions)))

(tag-parser |GetAllDatatypes|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (values 
     (make-set-of '|Datatypes| '|owl:Datatype|
                  (remove-duplicates
                   (let ((res nil))
                     (dolist (ont (owlapi:get-ontologies *owllink-kb-and-reasoner*))
                       (loop as d being the hash-key in (owlapi:declared-datatypes ont)
                             do (when (symbolp d)
                                  (push d res)))
                       (loop as d being the hash-key in (owlapi:referenced-datatypes ont)
                             do (when (symbolp d)
                                  (push d res))))
                     res)))

     expressions)))

(tag-parser |GetAllIndividuals|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (values 
     (make-set-of-individuals
      #+:ignore (reasoner-all-individuals *owllink-kb-and-reasoner*)
      (remove-duplicates
       (let ((res nil))
         (dolist (ont (owlapi:get-ontologies *owllink-kb-and-reasoner*))
           (loop as i being the hash-key in (owlapi:declared-individuals ont)
                 do (push i res))
           (loop as i being the hash-key in (owlapi:referenced-individuals ont)
                 do (push i res)))
         res)))

     expressions)))

(tag-parser |GetAllClasses|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (values 
     (make-set-of '|Classes| '|Class|
                  #+:ignore
                  (cons owlapi:+owlapi-owl-thing+
                        (cons owlapi:+owlapi-owl-nothing+
                              (without-top-and-bottom 
                               (reasoner-all-atomic-concepts *owllink-kb-and-reasoner*))))

                  (remove-duplicates
                   (let ((res nil))
                     (dolist (ont (owlapi:get-ontologies *owllink-kb-and-reasoner*))
                       (loop as c being the hash-key in (owlapi:declared-concepts ont)
                             do (push c res)))
                     (dolist (ont (owlapi:get-ontologies *owllink-kb-and-reasoner*))
                       (loop as c being the hash-key in (owlapi:referenced-concepts ont)
                             do (push c res)))
                     res)))

     expressions)))
     
;;;
;;; 
;;;

(defun yes-or-no-string (boolean)
  (if boolean
      "yes"
    "no"))

(tag-parser |IsKBSatisfiable|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (let* ((unsat-concepts
            (reasoner-atomic-concept-synonyms 'bottom *owllink-kb-and-reasoner*))
           (tbox-unsat
            (member 'top unsat-concepts))
           (abox-sat
            (reasoner-abox-consistent-p *owllink-kb-and-reasoner*))
           (sat (and (not tbox-unsat) abox-sat)))
      (values 
       (if sat
           (if (cddr unsat-concepts)
               (owllink-boolean-response-message 
                t
                :warning (format nil "Unsatisfiable classes: ~A"
                                 unsat-concepts))
             (owllink-boolean-response-message t))
         (owllink-boolean-response-message 
          nil
          :warning (format nil "TBox satisfiable: ~A. ABox satisfiable: ~A. Unsatisfiable classes: ~A"
                           (yes-or-no-string (not tbox-unsat))
                           (yes-or-no-string abox-sat)
                           unsat-concepts)))

       expressions))))
     
(tag-parser |IsKBConsistentlyDeclared|
    ()
 (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
   (values 
    (owllink-to-be-implemented-message '|IsKBConsistentlyDeclared|)
    expressions)))

(tag-parser |GetKBLanguage|
    ()
 (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
   (values 
    (owllink-string-response-message
     (reasoner-get-abox-language *owllink-kb-and-reasoner*)
     :warning (format nil "But TBox language is: ~A" 
                      (reasoner-get-tbox-language *owllink-kb-and-reasoner*)))
    expressions)))
     
(defmacro no-class-expression-error ()
  `(values 
    (owllink-syntax-error-message
     :error (error-message "No ClassExpression in ~S" (first expressions)))
    expressions))

(defmacro bad-class-expressions-error ()
  `(values 
    (owllink-syntax-error-message
     :error (error-message "Bad ClassExpressions in ~S" response))
    expressions))

(tag-parser |IsClassSatisfiable|
    ()
  (with-present-kb
    (reasoner-tbox-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ClassExpression| expressions)
      (let ((concept response))
        (if concept
            (with-consistent-tbox
              (values 
               (owllink-boolean-response-message
                (reasoner-concept-satisfiable-p concept *owllink-kb-and-reasoner*))
               expressions))
          (no-class-expression-error))))))


(dummy-tag-parser |owl.ClassExpression| 
  (multiple-value-bind (concept expressions)
      (parse-class-expression expressions)
    (values concept
            expressions)))

;;;
;;;
;;;

(tag-parser |IsClassSubsumedBy|
    ()
  (with-present-kb
    (reasoner-tbox-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ClassExpression| expressions)
      (let ((subclass response))
        (if (not subclass)
            (no-class-expression-error)
          (with-parser-call (|owl.ClassExpression| expressions)
            (let ((superclass response))
              (if (not superclass)
                  (no-class-expression-error)
                (with-consistent-tbox
                  (values
                   (owllink-boolean-response-message
                    (reasoner-concept-subsumes-p superclass subclass *owllink-kb-and-reasoner*))
                   expressions))))))))))

(tag-parser |AreClassesEquivalent|
    ()
  (with-present-kb
    (reasoner-tbox-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|ClassExpressions| expressions)
      (let ((concepts response))
        (if (member nil concepts)
            (bad-class-expressions-error)
          (with-consistent-tbox
            (values
             (owllink-boolean-response-message
              (owlapi:pairwise-equivalent-p 
               concepts 
               #'(lambda (x y)
                   (reasoner-concepts-equivalent-p
                    x y *owllink-kb-and-reasoner*))))
             expressions)))))))

(ano-loop-parser |ClassExpressions| 
    |owl.ClassExpression|)


(tag-parser |AreClassesDisjoint|
    ()
  (with-present-kb
    (reasoner-tbox-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|ClassExpressions| expressions)
      (let ((concepts response))
        (if (member nil concepts)
            (bad-class-expressions-error)
          (with-consistent-tbox
            (values
             (owllink-boolean-response-message
              (owlapi:for-all-pairs-holds-p concepts 
                                            #'(lambda (a b) 
                                                (reasoner-concepts-disjoint-p
                                                 a b *owllink-kb-and-reasoner*))))
             expressions)))))))

;;;
;;;
;;;

(defun concept-list-result (set &optional single-p set-of-syntax-p)
  (if (not single-p)
      (let ((set 
             (mapcar #'(lambda (x) 
                         (make-synset '|Class| x))
                     (delete nil
                             (delete-duplicates
                              (loop as c in set
                                    collect 
                                    (let ((c (if (consp c) (first c) c)))
                                      (owllink-concept-synonyms c)))
                              :test #'equal)))))

        (make-set-of-synsets 
         '|Class| 
         set
         set-of-syntax-p))

  (let ((set 
         (owllink-concept-synonyms set)))

    (make-set-of '|Classes| '|Class| set))))

(defun flat-concept-list-result (set &optional single-p)
  (let ((set 
         (if single-p
             (owllink-concept-synonyms set)
           (delete nil
                   (delete-duplicates
                    (apply #'append
                           (loop as c in set 
                                 collect 
                                 (let ((c (if (consp c) (first c) c)))
                                   (owllink-concept-synonyms c)))))))))
    ;;(make-set-of '|Classes| '|Class| set)
    (make-set-of nil '|Class| set '|Classes|)))

;;;
;;;
;;;

(tag-parser |GetDisjointClasses|
    ()
  (with-present-kb
    (reasoner-tbox-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ClassExpression| expressions)
      (let ((concept response))
        (if (not concept)
            (no-class-expression-error)
          (with-consistent-tbox
            (values
             (owllink-get-disjoint-classes concept)
             expressions)))))))

(tag-parser |GetFlattenedDisjointClasses|
    ()
  (with-present-kb
    (reasoner-tbox-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ClassExpression| expressions)
      (let ((concept response))
        (if (not concept)
            (no-class-expression-error)
          (with-consistent-tbox
            (values
             (owllink-get-disjoint-classes concept t)
             expressions)))))))

(tag-parser |GetSubClasses|
    ((|direct|) (let ((*direct* (string-to-boolean (get-attribute-value '|direct| attributes))))))
  (with-present-kb
    (reasoner-tbox-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ClassExpression| expressions)
      (let ((concept response))
        (if (not concept)
            (no-class-expression-error)
          (with-consistent-tbox
            (values (owllink-get-sub-classes concept)
                    expressions)))))))

(tag-parser |GetFlattenedSubClasses|
    ((|direct|) (let ((*direct* (string-to-boolean (get-attribute-value '|direct| attributes))))))
  (with-present-kb
    (reasoner-tbox-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ClassExpression| expressions)
      (let ((concept response))
        (if (not concept)
            (no-class-expression-error)
          (with-consistent-tbox
            (values (owllink-get-sub-classes concept t)
                    expressions)))))))

(tag-parser |GetSuperClasses|
    ((|direct|) (let ((*direct* (string-to-boolean (get-attribute-value '|direct| attributes))))))
  (with-present-kb
    (reasoner-tbox-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ClassExpression| expressions)
    (let ((concept response))
        (if (not concept)
            (no-class-expression-error)
          (with-consistent-tbox
            (values (owllink-get-super-classes concept)
                    expressions)))))))

(tag-parser |GetFlattenedSuperClasses|
    ((|direct|) (let ((*direct* (string-to-boolean (get-attribute-value '|direct| attributes))))))
  (with-present-kb
    (reasoner-tbox-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ClassExpression| expressions)
    (let ((concept response))
        (if (not concept)
            (no-class-expression-error)
          (with-consistent-tbox
            (values (owllink-get-super-classes concept t)
                    expressions)))))))

;;;
;;; 
;;;

(tag-parser |GetEquivalentClasses|
    ()
  (with-present-kb
    (reasoner-tbox-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ClassExpression| expressions)
    (let ((concept response))
      (if (not concept)
          (no-class-expression-error)
        (with-consistent-tbox
          (values
           (concept-list-result concept t)
           expressions)))))))


(tag-parser |GetSubClassHierarchy|
    ()
  (with-present-kb
    (reasoner-tbox-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ClassExpression| expressions)
    (let ((concept (or response 
                       'top)))
      (with-consistent-tbox
        (let ((res (owllink-class-hierarchy2 concept)))
          (values res expressions)))))))

(tag-parser |GetSuperClassHierarchy|
    ()
  (with-present-kb
    (reasoner-tbox-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ClassExpression| expressions)
    (let ((concept (or response
                       'bottom)))
      (with-consistent-tbox
        (let ((res (owllink-class-hierarchy2 concept t)))
          (values res expressions)))))))

;;;
;;;
;;;

(defmacro no-object-property-expression-error ()
  `(values 
    (owllink-syntax-error-message
     :error (error-message "No ObjectPropertyExpression in ~S" (first expressions)))
    expressions))

(defmacro no-data-property-error ()
  `(values 
    (owllink-syntax-error-message 
     :error (error-message "No DataProperty in ~S" (first expressions)))
    expressions))

(defmacro no-individual-error ()
  `(values 
    (owllink-syntax-error-message
     :error (error-message "No Individual in ~S" (first expressions)))
    expressions))

(defmacro no-literal-error ()
  `(values 
    (owllink-syntax-error-message
     :error (error-message "No Literal in ~S" (first expressions)))
    expressions))


(defmacro bad-object-property-expressions-error ()
  `(values 
    (owllink-syntax-error-message
     :error (error-message "Bad ObjectPropertyExpressions in ~S" response))
    expressions))

(defmacro bad-data-properties-error ()
  `(values 
    (owllink-syntax-error-message
     :error (error-message "Bad DataProperties in ~S" response))
    expressions))

(defmacro bad-individuals-error ()
  `(values 
    (owllink-syntax-error-message
     :error (error-message "Bad Individuals in ~S" response))
    expressions))

(defmacro not-all-object-property-expressions-error ()
  `(values 
    (owllink-semantic-error-message
     :error (error-message "Found strange ObjectPropertyExpressions in ~S" response))
    expressions))

(defmacro not-all-data-properties-error ()
  `(values 
    (owllink-semantic-error-message
     :error (error-message "Found strange DataProperty in ~S" response))
    expressions))

(defmacro not-all-individuals-error ()
  `(values 
    (owllink-semantic-error-message
     :error (error-message "Found strange Individuals in ~S" response))
    expressions))

;;; 
;;;
;;;  

(tag-parser |AreObjectPropertiesEquivalent|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|ObjectPropertyExpressions| expressions)
      (let ((roles response))
        (if (member nil roles)
            (bad-object-property-expressions-error)
          (if (not (every #'owllink-object-property-p roles))
              (not-all-object-property-expressions-error)
            (with-consistent-tbox
              (values
               (owllink-boolean-response-message
                (owlapi:pairwise-equivalent-p roles #'owllink-roles-equivalent-p))
               expressions))))))))

(tag-parser |AreObjectPropertiesDisjoint|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|ObjectPropertyExpressions| expressions)
      (let ((roles response))
        (if (member nil roles)
            (bad-object-property-expressions-error)
          (if (not (every #'owllink-object-property-p roles))
              (not-all-object-property-expressions-error)
            (with-consistent-tbox
              (values
               (owllink-boolean-response-message
                (owlapi:for-all-pairs-holds-p roles #'owllink-roles-disjoint-p))
               expressions))
            #+:ignore
            (values 
             (owllink-to-be-implemented-message '|AreObjectPropertiesDisjoint|)
             expressions)))))))

;;;
;;;
;;;      

(ano-loop-parser |ObjectPropertyExpressions| |owl.ObjectPropertyExpression|)

(dummy-tag-parser |owl.ObjectPropertyExpression| 
  (multiple-value-bind (role expressions)
      (parse-object-property-expression expressions nil)
    (values role
            expressions)))

(dummy-tag-parser |owl.ObjectProperty| 
  (multiple-value-bind (role expressions)
      (parse-object-property expressions)
    (values role
            expressions)))

(tag-parser |IsObjectPropertySatisfiable|
    ()
  (with-present-kb 
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (not (owllink-object-property-p role))
              (not-all-object-property-expressions-error)
            (with-consistent-tbox
              (values
               (owllink-boolean-response-message
                (owllink-role-satisfiable-p role))
               expressions))))))))

(tag-parser |IsObjectPropertyFunctional|
    ()
  (with-present-kb 
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (not (owllink-object-property-p role))
              (not-all-object-property-expressions-error)
            (with-consistent-tbox
              (values
               (owllink-boolean-response-message
                (owllink-role-functional-p role))
               expressions))))))))

(tag-parser |IsObjectPropertyInverseFunctional|
    ()
  (with-present-kb 
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (not (owllink-object-property-p role))
              (not-all-object-property-expressions-error)
            (with-consistent-tbox
              (values
               (owllink-boolean-response-message
                (owllink-role-inverse-functional-p role))
               expressions))))))))

(tag-parser |IsObjectPropertyReflexive|
    ()
  (with-present-kb 
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (not (owllink-object-property-p role))
              (not-all-object-property-expressions-error)
            (with-consistent-tbox
              (values
               (owllink-boolean-response-message
                (owllink-role-reflexive-p role))
               expressions))))))))

(tag-parser |IsObjectPropertyIrreflexive|
    ()
  (with-present-kb 
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (not (owllink-object-property-p role))
              (not-all-object-property-expressions-error)
            (with-consistent-tbox
              (values
               (owllink-boolean-response-message
                (owllink-role-irreflexive-p role))
               expressions))
            #+:ignore
            (values 
             (owllink-to-be-implemented-message '|IsObjectPropertyIrreflexive|)
             expressions)))))))

(tag-parser |IsObjectPropertySymmetric|
    ()
  (with-present-kb 
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (not (owllink-object-property-p role))
              (not-all-object-property-expressions-error)
            (with-consistent-tbox
              (values
               (owllink-boolean-response-message
                (owllink-role-symmetric-p role))
               expressions))))))))

(tag-parser |IsObjectPropertyAsymmetric|
    ()
  (with-present-kb 
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (not (owllink-object-property-p role))
              (not-all-object-property-expressions-error)
            (with-consistent-tbox
              (values
               (owllink-boolean-response-message
                (owllink-role-asymmetric-p role)) ; Klammerungsfehler beseitigt (RM Apr. 14)
               expressions))
            #+:ignore
            (values 
             (owllink-to-be-implemented-message '|IsObjectPropertyAsymmetric|)
             expressions)))))))

(tag-parser |IsObjectPropertyTransitive|
    ()
  (with-present-kb 
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (not (owllink-object-property-p role))
              (not-all-object-property-expressions-error)
            (with-consistent-tbox
              (values
               (owllink-boolean-response-message
                (owllink-role-transitive-p role))
               expressions))))))))

(tag-parser |IsObjectPropertySubsumedBy|
    ()
  (with-present-kb 
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((subproperty response))
        (if (not subproperty)
            (no-object-property-expression-error)
          (with-parser-call (|owl.ObjectPropertyExpression| expressions)
            (let ((superproperty response))
              (if (not superproperty)
                  (no-object-property-expression-error)
                (if (not (and (owllink-object-property-p superproperty)
                              (owllink-object-property-p subproperty)))
                    (not-all-object-property-expressions-error)
                  (with-consistent-tbox
                    (values
                     (owllink-boolean-response-message
                      (owllink-role-subsumes-p superproperty subproperty))
                     expressions)))))))))))

;;;
;;;
;;;

(defun object-property-list-result (set &optional single-p synset-p set-of-syntax-p)
  (if (not single-p)
      (let ((set 
             (mapcar #'(lambda (x) 
                         (make-synset '|ObjectProperty| x))
                     (mapcar 
                      #'(lambda (x) 
                          (reasoner-only-object-properties x *owllink-kb-and-reasoner*))
                      (remove nil 
                              (remove-duplicates
                               (loop as r in (reasoner-only-object-properties 
                                              set
                                              *owllink-kb-and-reasoner*)
                                     collect
                                     (let ((r (if (consp r) (first r) r)))
                                       (owllink-object-property-synonyms r))))
                              :test #'equal)))))
        
        (make-set-of-synsets 
         '|ObjectProperty| 
         set
         set-of-syntax-p))
    
    (let ((set 
           (when (owllink-object-property-p set)
             (reasoner-only-object-properties
              (owllink-object-property-synonyms set)
              *owllink-kb-and-reasoner*))))

      (if synset-p
          (make-synset '|ObjectProperty| set)
        (make-set-of '|ObjectProperties| '|ObjectProperty| set)))))


(defun flat-object-property-list-result (set &optional single-p)
  (let ((set 
         (if single-p 
             (when (owllink-object-property-p set)
               (reasoner-only-object-properties
                (owllink-object-property-synonyms set)
                *owllink-kb-and-reasoner*))
           
           (remove nil
                   (remove-duplicates
                    (apply #'append
                            (loop as r in (reasoner-only-object-properties set *owllink-kb-and-reasoner*)
                                  collect 
                                  (let ((r (if (consp r) (first r) r)))
                                    (owllink-object-property-synonyms r)))))))))
    
    (make-set-of '|ObjectProperties| '|ObjectProperty| set)))

;;;
;;;
;;; 

(tag-parser |GetSubObjectProperties|
    ((|direct|) (let ((*direct* 
                       (string-to-boolean 
                        (get-attribute-value '|direct| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (not (owllink-object-property-p role))
              (not-all-object-property-expressions-error)
            (with-consistent-tbox
              (values
               (object-property-list-result
                (if *direct*
                    (owllink-role-children role)
                  (owllink-role-descendants role))
                nil 
                nil 
                t)
               expressions))))))))

(tag-parser |GetSuperObjectProperties|
    ((|direct|) (let ((*direct* 
                       (string-to-boolean 
                        (get-attribute-value '|direct| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (not (owllink-object-property-p role))
              (not-all-object-property-expressions-error)
            (with-consistent-tbox
              (values
               (object-property-list-result
                (if *direct*
                    (owllink-role-parents role)
                  (owllink-role-ancestors role))
                nil 
                nil 
                t)
               expressions))))))))

;;;
;;;
;;;

(tag-parser |GetEquivalentObjectProperties|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (not (owllink-object-property-p role))
              (not-all-object-property-expressions-error)
            (values
             (object-property-list-result role t nil) ; nil ist korrekt hier!
             expressions)))))))

(tag-parser |GetDisjointObjectProperties|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if nil ; (not (owllink-object-property-p role))
              (not-all-object-property-expressions-error)
            (with-consistent-tbox
              (values
               (owllink-get-disjoint-object-properties role)
               expressions))
            #+:ignore
            (values 
             (owllink-to-be-implemented-message '|GetDisjointObjectProperties|)
             expressions)))))))

(tag-parser |GetFlattenedDisjointObjectProperties|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (not (owllink-object-property-p role))
              (not-all-object-property-expressions-error)
            (with-consistent-tbox
              (values
               (owllink-get-disjoint-object-properties role t)
               expressions))
            #+:ignore
            (values 
             (owllink-to-be-implemented-message '|GetFlattenDisjointObjectProperties|)
             expressions)))))))

;;;
;;;
;;;

(defun get-object-properties-with-property (predicate expressions)
 (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-consistent-tbox
      (values
       (object-property-list-result
        (remove-if-not predicate (reasoner-get-object-properties *owllink-kb-and-reasoner*))
        nil nil t)
       expressions))))

(tag-parser |GetFunctionalObjectProperties|
    ()
  (get-object-properties-with-property #'owllink-role-functional-p expressions))

(tag-parser |GetInverseFunctionalObjectProperties|
    ()
  (get-object-properties-with-property #'owllink-role-inverse-functional-p expressions))

(tag-parser |GetSymmetricObjectProperties|
    ()
  (get-object-properties-with-property #'owllink-role-symmetric-p expressions))

(tag-parser |GetAsymmetricObjectProperties|
    ()
  (get-object-properties-with-property #'owllink-role-asymmetric-p expressions)
  #+:ignore
  (values 
   (owllink-to-be-implemented-message '|GetAsymmetricObjectProperties|)
   expressions))

(tag-parser |GetReflexiveObjectProperties|
    ()
  (get-object-properties-with-property #'owllink-role-reflexive-p expressions))

(tag-parser |GetIrreflexiveObjectProperties|
    ()
  (get-object-properties-with-property #'owllink-role-irreflexive-p expressions)
  #+:ignore
  (values 
   (owllink-to-be-implemented-message '|GetIrreflexiveObjectProperties|)
   expressions))

(tag-parser |GetTransitiveObjectProperties|
    ()
  (get-object-properties-with-property #'owllink-role-transitive-p expressions))

;;;
;;;
;;;        
                        
(tag-parser |GetSubObjectPropertyHierarchy|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectProperty| expressions)
      (let ((role (or response 
                      owlapi:+owlapi-owl-top-object-role+)))
        (if (not (owllink-object-property-p role))
            (not-all-object-property-expressions-error)
          (with-consistent-tbox
            (let ((res (owllink-object-property-hierarchy2 role)))
              (values res expressions))))))))

(tag-parser |GetSuperObjectPropertyHierarchy|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectProperty| expressions)
      (let ((role (or response
                      owlapi:+owlapi-owl-bottom-object-role+)))
        (if (not (owllink-object-property-p role))
            (not-all-object-property-expressions-error)
          (with-consistent-tbox
            (let ((res (owllink-object-property-hierarchy2 role t)))
              (values res expressions))))))))

;;;
;;;
;;;

(tag-parser |AreDataPropertiesEquivalent|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|DataProperties| expressions)
      (let ((roles response))
        (if (member nil roles)
            (bad-data-properties-error)
          (if (not (every #'owllink-data-property-p roles))
              (not-all-data-properties-error)
            (with-consistent-tbox
              (values
               (owllink-boolean-response-message
                (owlapi:pairwise-equivalent-p roles #'owllink-roles-equivalent-p))
               expressions))))))))

(tag-parser |AreDataPropertiesDisjoint|
    ()
   (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|DataProperties| expressions)
      (let ((roles response))
        (if (member nil roles)
            (bad-data-properties-error)
          (if (not (every #'owllink-data-property-p roles))
              (not-all-data-properties-error)
            (with-consistent-tbox
              (values
               (owllink-boolean-response-message
                (owlapi:pairwise-equivalent-p roles #'owllink-roles-disjoint-p))
               expressions))
            #+:ignore
            (values 
             (owllink-to-be-implemented-message '|AreDataPropertiesDisjoint|)
             expressions)))))))

(ano-loop-parser |DataProperties| |owl.DataProperty|)

(dummy-tag-parser |owl.DataProperty| 
  (multiple-value-bind (dtp-role expressions)
      (parse-data-property expressions)
    (values dtp-role
            expressions)))

(tag-parser |IsDataPropertySatisfiable|
    ()
  (with-present-kb 
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.DataProperty| expressions)
      (let ((role response))
        (if (not role)
            (no-data-property-error)
          (if (not (owllink-data-property-p role))
              (not-all-data-properties-error)
            (with-consistent-tbox
              (values
               (owllink-boolean-response-message
                (owllink-role-satisfiable-p role))
               expressions))))))))

(tag-parser |IsDataPropertyFunctional|
    ()
  (with-present-kb 
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.DataProperty| expressions)
      (let ((role response))
        (if (not role)
            (no-data-property-error)
          (if (not (owllink-data-property-p role))
              (not-all-data-properties-error)
            (with-consistent-tbox
              (values
               (owllink-boolean-response-message
                (owllink-role-functional-p role))
               expressions))))))))   

(tag-parser |IsDataPropertySubsumedBy|
    ()
 (with-present-kb 
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.DataProperty| expressions)
      (let ((subproperty response))
        (if (not subproperty)
            (no-data-property-error)
          (with-parser-call (|owl.DataProperty| expressions)
            (let ((superproperty response))
              (if (not superproperty)
                  (no-data-property-error)
                (if (not (and (owllink-data-property-p superproperty)
                              (owllink-data-property-p subproperty)))
                    (not-all-data-properties-error)
                  (with-consistent-tbox
                    (values
                     (owllink-boolean-response-message
                      (owllink-role-subsumes-p superproperty subproperty))
                     expressions)))))))))))

;;;
;;;
;;;

(defun get-data-properties-with-property (predicate expressions)
 (with-present-kb
   (reasoner-sync *owllink-kb-and-reasoner*)
   (with-consistent-tbox
     (values
      (data-property-list-result
       (remove-if-not predicate (reasoner-get-data-properties *owllink-kb-and-reasoner*))
       nil nil t)
      expressions))))

(tag-parser |GetFunctionalDataProperties|
    ()
  (get-data-properties-with-property #'owllink-role-functional-p expressions))

;;;
;;;
;;; 

(defun data-property-list-result (set &optional single-p synset set-of-syntax-p)
  (if (not single-p)
      (let ((set 
             (mapcar #'(lambda (x) 
                         (make-synset '|DataProperty| x))
                     (mapcar 
                      #'(lambda (x) 
                          (reasoner-only-data-properties x *owllink-kb-and-reasoner*))
                      (remove nil
                              (remove-duplicates
                               (loop as r in (reasoner-only-data-properties set *owllink-kb-and-reasoner*)
                                     collect 
                                     (let ((r (if (consp r) (first r) r)))
                                       (owllink-data-property-synonyms r)))
                               :test #'equal))))))
        
        (make-set-of-synsets 
         '|DataProperty| 
         set
         set-of-syntax-p))

    (let ((set
           (when (owllink-data-property-p set)
             (reasoner-only-data-properties
              (owllink-data-property-synonyms set)
              *owllink-kb-and-reasoner*))))

      (if synset
          (if (eq synset :equi)
              (make-equi-set 
               '|DataProperty| 
               set
               ;;'|nil:EquivalentDataProperties|
               '|DataPropertySynonyms|)
            (make-synset '|DataProperty| set))
        (make-set-of '|DataProperties| '|DataProperty| set)))))


(defun flat-data-property-list-result (set &optional single-p)
  (let ((set 
         (if single-p 
             (when (owllink-data-property-p set)
               (reasoner-only-data-properties
                (owllink-data-property-synonyms set)
                *owllink-kb-and-reasoner*))

           (remove nil 
                   (remove-duplicates
                    (apply #'append
                           (loop as r in (reasoner-only-data-properties set *owllink-kb-and-reasoner*)
                                 collect 
                                 (let ((r (if (consp r) (first r) r)))
                                   (owllink-data-property-synonyms r))))
                    :test #'equal)))))
    
    (make-set-of '|DataProperties| '|DataProperty| set)))

;;;
;;;
;;;

(tag-parser |GetSubDataProperties|
    ((|direct|) (let ((*direct* (string-to-boolean (get-attribute-value '|direct| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.DataProperty| expressions)
      (let ((role response))
        (if (not role)
            (no-data-property-error)
          (if (not (owllink-data-property-p role))
              (not-all-data-properties-error)
            (with-consistent-tbox
              (values
               (data-property-list-result
                (if *direct*
                    (owllink-role-children role)
                  (owllink-role-descendants role))
                nil 
                nil 
                t)
               expressions))))))))
      
(tag-parser |GetSuperDataProperties|
    ((|direct|) (let ((*direct* (string-to-boolean (get-attribute-value '|direct| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.DataProperty| expressions)
      (let ((role response))
        (if (not role)
            (no-data-property-error)
          (if (not (owllink-data-property-p role))
              (not-all-data-properties-error)
            (with-consistent-tbox
              (values
               (data-property-list-result
                (if *direct*
                    (owllink-role-parents role)
                  (owllink-role-ancestors role))
                nil 
                nil 
                t)
               expressions))))))))

;;;
;;;
;;;

(tag-parser |GetEquivalentDataProperties|
    ()
 (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.DataProperty| expressions)
      (let ((role response))
        (if (not role)
            (no-data-property-error)
          (if (not (owllink-data-property-p role))
              (not-all-data-properties-error)
            (with-consistent-tbox
              (values
               (data-property-list-result role t :equi)
               expressions))))))))

(tag-parser |GetDisjointDataProperties|
    ()
 (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.DataProperty| expressions)
      (let ((role response))
        (if (not role)
            (no-data-property-error)
          (if nil ; (not (owllink-data-property-p role))
              (not-all-data-properties-error)
            (with-consistent-tbox
              (values
               (owllink-get-disjoint-data-properties role)
               expressions))
            #+:ignore
            (values 
             (owllink-to-be-implemented-message '|GetDisjointDataProperties|)
             expressions)))))))

(tag-parser |GetFlattenedDisjointDataProperties|
    ()
 (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.DataProperty| expressions)
      (let ((role response))
        (if (not role)
            (no-data-property-error)
          (if (not (owllink-data-property-p role))
              (not-all-data-properties-error)
            (with-consistent-tbox
              (values
               (owllink-get-disjoint-data-properties role t)
               expressions))
            #+:ignore
            (values 
             (owllink-to-be-implemented-message '|GetFlattenedDisjointDataProperties|)
             expressions)))))))

;;;
;;;
;;;
      
(tag-parser |GetSubDataPropertyHierarchy|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.DataProperty| expressions)
      (let ((role (or response
                      owlapi:+owlapi-owl-top-data-role+)))
        (if (not (owllink-data-property-p role))
            (not-all-data-properties-error)
          (with-consistent-tbox
            (let ((res (owllink-data-property-hierarchy2 role)))
              (values res expressions))))))))

(tag-parser |GetSuperDataPropertyHierarchy|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.DataProperty| expressions)
      (let ((role (or response
                      owlapi:+owlapi-owl-bottom-data-role+)))
        (if (not (owllink-data-property-p role))
            (not-all-data-properties-error)
          (with-consistent-tbox
            (let ((res (owllink-data-property-hierarchy2 role t)))
              (values res expressions))))))))
       
;;;
;;;
;;;

(tag-parser |AreIndividualsEquivalent|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|Individuals| expressions)
      (let ((inds response))
        (if (member nil inds)
            (bad-individuals-error)
          (with-consistent-abox
            (values
             (owllink-boolean-response-message
              (owlapi:pairwise-equivalent-p 
               inds 
               #'(lambda (x y)
                   (member x 
                           (reasoner-individual-synonyms y *owllink-kb-and-reasoner*)))))
             expressions)))))))

(tag-parser |AreIndividualsDisjoint|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|Individuals| expressions)
      (let ((inds response))
        (if (member nil inds)
            (bad-individuals-error)
          (with-consistent-abox
            (values
             (owllink-boolean-response-message
              (owlapi:for-all-pairs-holds-p 
               inds 
               #'(lambda (x y) 
                   (member x 
                           (reasoner-individual-antonyms y *owllink-kb-and-reasoner*)))))
             expressions)))))))

(ano-loop-parser |Individuals| |owl.Individual|)

(dummy-tag-parser |owl.Individual| 
  (multiple-value-bind (ind expressions)
      (parse-individual expressions)
    (values ind
            expressions)))

(tag-parser |IsInstanceOf|
    ((|direct|) (let ((*direct* (string-to-boolean (get-attribute-value '|direct| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((ind response))
        (if (not ind)
            (no-individual-error)
          (with-parser-call (|owl.ClassExpression| expressions)
            (let ((concept response))
              (if (not concept)
                  (no-class-expression-error)
                (with-consistent-abox
                  (values (owllink-boolean-response-message
                           (owllink-is-instance-of ind concept *direct*))
                          expressions))))))))))

;;;
;;;
;;;

(tag-parser |GetTypes|
    ((|direct|) (let ((*direct* (string-to-boolean (get-attribute-value '|direct| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((ind response))
        (if (not ind)
            (no-individual-error)
          (with-consistent-abox
            (values 
             (owllink-get-types ind)
             expressions)))))))

(tag-parser |GetFlattenedTypes|
    ((|direct|) (let ((*direct* (string-to-boolean (get-attribute-value '|direct| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((ind response))
        (if (not ind)
            (no-individual-error)
          (with-consistent-abox
            (values 
             (owllink-get-types ind t)
             expressions)))))))

;;;
;;;
;;;

(defun individual-list-result (set &optional single-p synset-p set-of-syntax-p)
  (if (not single-p)
      (let ((set 
             (mapcar #'(lambda (x) 
                         (make-individual-synset x))
                     (remove nil
                             (remove-duplicates
                              (loop as c in set
                                    collect 
                                    (reasoner-individual-synonyms 
                                     c *owllink-kb-and-reasoner*)))))))
        (make-set-of-synsets 
         '|Individual| 
         set 
         set-of-syntax-p))
    
    (let ((set 
           (reasoner-individual-synonyms set *owllink-kb-and-reasoner*)))

      (if synset-p 
          (make-individual-synonyms set)
        (make-set-of-individuals set)))))

(defun flat-individual-list-result (set &optional single-p)
  (let ((set 
         (if single-p
             (reasoner-individual-synonyms
              set *owllink-kb-and-reasoner*)
           (remove nil 
                   (remove-duplicates
                    (apply #'append
                           (loop as c in set 
                                 collect 
                                 (reasoner-individual-synonyms
                                  c *owllink-kb-and-reasoner*))))))))
    (make-set-of-individuals set)))

(tag-parser |GetDisjointIndividuals|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((ind response))
        (if (not ind)
            (no-individual-error)
          (with-consistent-abox
            (values 
             (owllink-get-disjoint-individuals ind)
             expressions)))))))

(tag-parser |GetDifferentIndividuals|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((ind response))
        (if (not ind)
            (no-individual-error)
          (with-consistent-abox
            (values 
             (owllink-get-disjoint-individuals ind)
             expressions)))))))

;;;
;;;
;;;

(tag-parser |GetFlattenedDisjointIndividuals|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((ind response))
        (if (not ind)
            (no-individual-error)
          (with-consistent-abox
            (values 
             (owllink-get-disjoint-individuals ind t)
             expressions)))))))

(tag-parser |GetFlattenedDifferentIndividuals|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((ind response))
        (if (not ind)
            (no-individual-error)
          (with-consistent-abox
            (values 
             (owllink-get-disjoint-individuals ind t)
             expressions)))))))

(tag-parser |GetEquivalentIndividuals|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((ind response))
        (if (not ind)
            (no-individual-error)
          (with-consistent-abox
            (values 
             (individual-list-result ind t t))))))))

(tag-parser |GetSameIndividuals|
    ()
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((ind response))
        (if (not ind)
            (no-individual-error)
          (with-consistent-abox
            (values 
             (individual-list-result ind t t))))))))

;;;
;;;
;;;

(tag-parser |GetObjectPropertiesBetween|
    ((|negative|)
     (let ((*negative* (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((source response))
        (if (not source)
            (no-individual-error)
          (with-parser-call (|owl.Individual| expressions)
            (let ((target response))
              (if (not source)
                  (no-individual-error)
                (with-consistent-abox
                  (values
                   (owllink-get-object-properties-between source target)
                   expressions))))))))))

(tag-parser |GetFlattenedObjectPropertiesBetween|
    ((|negative|) 
     (let ((*negative* (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((source response))
        (if (not source)
            (no-individual-error)
          (with-parser-call (|owl.Individual| expressions)
            (let ((target response))
              (if (not source)
                  (no-individual-error)
                (with-consistent-abox
                  (values
                   (owllink-get-object-properties-between source target t)
                   expressions))))))))))

(tag-parser |GetObjectPropertiesOfSource|
    ((|negative|) 
     (let ((*negative* (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((source response))
        (if (not source)
            (no-individual-error)
          (with-consistent-abox
            (values 
             (owllink-get-object-properties-of-source source)
             expressions)))))))

(tag-parser |GetFlattenedObjectPropertiesOfSource|
    ((|negative|) (let ((*negative* 
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((source response))
        (if (not source)
            (no-individual-error)
          (with-consistent-abox
            (values 
             (owllink-get-object-properties-of-source source t)
             expressions)))))))

(tag-parser |GetObjectPropertiesOfTarget|
    ((|negative|) (let ((*negative*
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((target response))
        (if (not target)
            (no-individual-error)
          (with-consistent-abox
            (values 
             (owllink-get-object-properties-of-target target)
             expressions)))))))

(tag-parser |GetFlattenedObjectPropertiesOfTarget|
    ((|negative|) (let ((*negative* 
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.Individual| expressions)
      (let ((target response))
        (if (not target)
            (no-individual-error)
          (with-consistent-abox
            (values 
             (owllink-get-object-properties-of-target target t)
             expressions)))))))

;;;
;;;
;;;

(tag-parser |AreIndividualsRelated|
    ((|negative|) (let ((*negative*
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (owllink-data-property-p role)
              (bad-object-property-expressions-error)
            (with-parser-call (|owl.Individual| expressions)
              (let ((source response))
                (if (not source)
                    (no-individual-error)
                  (with-parser-call (|owl.Individual| expressions)
                    (let ((target response))
                      (if (not target)
                          (no-individual-error)
                        (with-consistent-abox
                          (values (owllink-boolean-response-message
                                   (reasoner-individuals-related-p
                                    source
                                    target
                                    (if *negative*
                                        `(not ,role)
                                      role)
                                    *owllink-kb-and-reasoner*))
                                  expressions))))))))))))))
              
(tag-parser |IsIndividualRelatedWithLiteral|
    ((|negative|)
     (let ((*negative* 
            (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (if *negative*
        (owllink-semantic-error-message 
         :error (error-message "Cannot deal with negated DataProperties yet. To be implemented"))
      (with-parser-call (|owl.DataProperty| expressions)
        (let ((role response))
          (if (not role)
              (no-data-property-error)
            (if (not (owllink-data-property-p role))
                (bad-data-properties-error)
              (with-parser-call (|owl.Individual| expressions)
                (let ((source response))
                  (if (not source)
                      (no-individual-error)
                    (with-parser-call (|owl.Literal| expressions)
                      (let ((literal response))
                        (if (not literal)
                            (no-literal-error)
                          (with-consistent-abox
                            (values (owllink-boolean-response-message 
                                     (find literal
                                           (reasoner-get-individual-datatype-fillers 
                                            source *owllink-kb-and-reasoner*)
                                           :key #'second
                                           :test (lambda (literal literal-list)
                                                   (find literal literal-list :test #'equal))))
                                    expressions)))))))))))))))

(dummy-tag-parser |owl.Literal| 
  (multiple-value-bind (literal expressions)
      (parse-literal expressions)
    (values literal
            expressions)))

;;;
;;;
;;;
  
(tag-parser |GetDataPropertiesBetween|
    ((|negative|) (let ((*negative* 
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (if *negative*
        (owllink-semantic-error-message 
         :error (error-message "Cannot deal with negated DataProperties yet. To be implemented"))
      (with-parser-call (|owl.Individual| expressions)
        (let ((source response))
          (if (not source)
              (no-individual-error)
            (with-parser-call (|owl.Literal| expressions)
              (let ((literal response))
                (if (not literal)
                    (no-literal-error)
                  (with-consistent-abox
                    (values
                     (owllink-get-data-properties-between source literal)
                     expressions)))))))))))

(tag-parser |GetFlattenedDataPropertiesBetween|
    ((|negative|) (let ((*negative* 
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (if *negative*
        (owllink-semantic-error-message 
         :error (error-message "Cannot deal with negated DataProperties yet. To be implemented"))
      (with-parser-call (|owl.Individual| expressions)
        (let ((source response))
          (if (not source)
              (no-individual-error)
            (with-parser-call (|owl.Literal| expressions)
              (let ((literal response))
                (if (not literal)
                    (no-literal-error)
                  (with-consistent-abox
                    (values
                     (owllink-get-data-properties-between source literal t)
                     expressions)))))))))))

(tag-parser |GetDataPropertiesOfSource|
    ((|negative|) (let ((*negative* 
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (if *negative*
        (owllink-semantic-error-message 
         :error (error-message "Cannot deal with negated DataProperties yet. To be implemented"))
      (with-parser-call (|owl.Individual| expressions)
        (let ((source response))
          (if (not source)
              (no-individual-error)
            (with-consistent-abox
              (values 
               (owllink-get-data-properties-of-source source)
               expressions))))))))

(tag-parser |GetFlattenedDataPropertiesOfSource|
    ((|negative|) (let ((*negative* 
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (if *negative*
        (owllink-semantic-error-message 
         :error (error-message "Cannot deal with negated DataProperties yet. To be implemented"))
      (with-parser-call (|owl.Individual| expressions)
        (let ((source response))
          (if (not source)
              (no-individual-error)
            (with-consistent-abox
              (values 
               (owllink-get-data-properties-of-source source t)
               expressions))))))))

(tag-parser |GetDataPropertiesOfLiteral|
    ((|negative|) (let ((*negative* 
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (if *negative*
        (owllink-semantic-error-message 
         :error (error-message "Cannot deal with negated DataProperties yet. To be implemented"))
      (with-parser-call (|owl.Literal| expressions)
        (let ((literal response))
          (if (not literal)
              (no-literal-error)
            (with-consistent-abox
              (values 
               (owllink-get-data-properties-of-literal literal)
               expressions))))))))

(tag-parser |GetFlattenedDataPropertiesOfLiteral|
    ((|negative|) (let ((*negative*
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (if *negative*
        (owllink-semantic-error-message 
         :error (error-message "Cannot deal with negated DataProperties yet. To be implemented"))
      (with-parser-call (|owl.Literal| expressions)
        (let ((literal response))
          (if (not literal)
              (no-literal-error)
            (with-consistent-abox
              (values 
               (owllink-get-data-properties-of-literal literal t)
               expressions))))))))

;;;
;;;
;;; 

(tag-parser |GetInstances|
    ((|direct|) (let ((*direct* 
                       (string-to-boolean (get-attribute-value '|direct| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ClassExpression| expressions)
      (let ((concept response))
        (if (not concept)
            (no-class-expression-error)
          (with-consistent-abox
            (values (owllink-get-instances concept *direct*)
                    expressions)))))))

(tag-parser |GetObjectPropertyTargets|
    ((|negative|) (let ((*negative* 
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (owllink-data-property-p role)
              (bad-object-property-expressions-error)
            (with-parser-call (|owl.Individual| expressions)
              (let ((ind response))
                (if (not ind)
                    (no-individual-error)
                  (with-consistent-abox
                    (values (owllink-get-object-property-targets ind role)
                            expressions)))))))))))

(tag-parser |GetObjectPropertySources|
    ((|negative|) (let ((*negative* 
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (owllink-data-property-p role)
              (bad-object-property-expressions-error)
            (with-parser-call (|owl.Individual| expressions)
              (let ((ind response))
                (if (not ind)
                    (no-individual-error)
                  (with-consistent-abox
                    (values (owllink-get-object-property-sources ind role)
                            expressions)))))))))))

;;;
;;;
;;; 
              
(tag-parser |GetFlattenedInstances|
    ((|direct|) (let ((*direct* (string-to-boolean (get-attribute-value '|direct| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ClassExpression| expressions)
      (let ((concept response))
        (if (not concept)
            (no-class-expression-error)
          (with-consistent-abox
            (values (owllink-get-instances concept *direct* t)
                    expressions)))))))

(tag-parser |GetFlattenedObjectPropertyTargets|
    ((|negative|) (let ((*negative* 
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (owllink-data-property-p role)
              (bad-object-property-expressions-error)
            (with-parser-call (|owl.Individual| expressions)
              (let ((ind response))
                (if (not ind)
                    (no-individual-error)
                  (with-consistent-abox
                    (values (owllink-get-object-property-targets ind role t)
                            expressions)))))))))))

(tag-parser |GetFlattenedObjectPropertySources|
    ((|negative|) (let ((*negative* 
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
 (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (with-parser-call (|owl.ObjectPropertyExpression| expressions)
      (let ((role response))
        (if (not role)
            (no-object-property-expression-error)
          (if (owllink-data-property-p role)
              (bad-object-property-expressions-error)
            (with-parser-call (|owl.Individual| expressions)
              (let ((ind response))
                (if (not ind)
                    (no-individual-error)
                  (with-consistent-abox
                    (values (owllink-get-object-property-sources ind role t)
                            expressions)))))))))))

;;;
;;;
;;; 

(tag-parser |GetDataPropertySources|
    ((|negative|) (let ((*negative* 
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (if *negative*
        (owllink-semantic-error-message 
         :error (error-message "Cannot deal with negated DataProperties yet. To be implemented"))
    
      (with-parser-call (|owl.DataProperty| expressions)
        (let ((role response))
          (if (not role)
              (no-data-property-error)
            (if (not (owllink-data-property-p role))
                (bad-data-properties-error)
              (with-parser-call (|owl.Literal| expressions)
                (let ((literal response))
                  (if (not literal)
                      (no-literal-error)
                    (with-consistent-abox
                      (values (owllink-get-data-property-sources literal role)
                              expressions))))))))))))

(defun flat-literal-list-result (set)
  (make-set-of '|Literals| '|Literal| set))

(tag-parser |GetDataPropertyTargets|
    ((|negative|) (let ((*negative* 
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (if *negative*
        (owllink-semantic-error-message 
         :error (error-message "Cannot deal with negated DataProperties yet. To be implemented"))

      (with-parser-call (|owl.DataProperty| expressions)
        (let ((role response))
          (if (not role)
              (no-data-property-error)
            (if (not (owllink-data-property-p role))
                (bad-data-properties-error)
              (with-parser-call (|owl.Individual| expressions)
                (let ((ind response))
                  (if (not ind)
                      (no-individual-error)
                    (with-consistent-abox
                      (values (owllink-get-data-property-targets ind role)
                              expressions))))))))))))

;;;
;;;
;;; 
 
(tag-parser |GetFlattenedDataPropertySources|
    ((|negative|) (let ((*negative* 
                         (string-to-boolean (get-attribute-value '|negative| attributes))))))
  (with-present-kb
    (reasoner-sync *owllink-kb-and-reasoner*)
    (if *negative*
        (owllink-semantic-error-message 
         :error (error-message "Cannot deal with negated DataProperties yet. To be implemented"))    
      (with-parser-call (|owl.DataProperty| expressions)
        (let ((role response))
          (if (not role)
              (no-data-property-error)
            (if (not (owllink-data-property-p role))
                (bad-data-properties-error)
              (with-parser-call (|owl.Literal| expressions)
                (let ((literal response))
                  (if (not literal)
                      (no-literal-error)
                    (with-consistent-abox
                      (values (owllink-get-data-property-sources literal role t)
                              expressions))))))))))))

;;;
;;;
;;;

#+:racer-server
(tag-parser |http://www.racer-systems.com/owllink/ext/racer#Racer|  
    ((|http://www.racer-systems.com/owllink/ext/racer#command|) 
     (let ((*command*
            (get-attribute-value 
             '|http://www.racer-systems.com/owllink/ext/racer#command| attributes))
           (reasoner
            (or *owllink-kb-and-reasoner*
                owlapi:*default-reasoner-name*)))))
  (with-reasoner (reasoner)
    (let ((string1 nil)
          (string2 nil)
          (command (or *command* 
                       (if (stringp (first expressions))
                           (first expressions)
                         (with-output-to-string (stream)
                           (pprint (first expressions) stream))))))

      (declare (ignorable string2))

      (reasoner-sync *owllink-kb-and-reasoner*)

      (setf string1
            (with-output-to-string (stream1)
              (setf string2 
                    (with-output-to-string (stream2)
                      (let ((*one-simple-output* t))
                        (process-racer-string 
                         stream1 command 0 (find-package :cl-user) stream2))))))

      (if (and (>= (length string1) 6)
               (string-equal (subseq string1 0 6) ":error"))
          (owllink-error-message 
           :error 
           (error-message "Problems with Racer request: ~S" command))
        (progn 
          (pushnew "http://www.racer-systems.com/owllink/ext/racer#" 
                   *additional-answer-namespaces-used* :test #'string=)

          (make-owllink-message 
           '|http://www.racer-systems.com/owllink/ext/racer#RacerAnswer|
           nil
           string1))))))


#+:racer-server
(tag-parser |http://www.racer-systems.com/owllink/ext/racer#FullReset|  
    ()
  (full-reset)
  (values 
     (owllink-ok-message)
     expressions))


;;;
;;; 
;;; 

#+:ignore
(defun add-package-nickname (name nickname)
  (let ((p (find-package name)))
    (rename-package p
                    (package-name p)
                    (cons nickname (package-nicknames name)))))

(tag-parser |NamespacePrefix|  
    () 
  (let ((prefix (normalize-prefix (first expressions)))
        (namespace (normalize-url (second expressions))))

    (pushnew (cons prefix namespace)
             *last-request-xml-and-functional-namespaces* :test #'equal)

    ;;; (add-package-nickname namespace prefix)    

    (values 
     (owllink-ok-message :warning (format nil "Prefix ~A = ~A" prefix namespace))
     expressions)))

(tag-parser |RenderingOption|  
    () 
  (let ((key (first expressions))
        (value (second expressions)))

    (pushnew (list (owlapi:ensure-string key)
                   (owlapi:ensure-string value))
             
             *last-request-rendering-options* :test #'equal)

    (values 
     (owllink-ok-message :warning (format nil "RenderingOption ~A = ~A" key value))
     expressions)))

;;;
;;;
;;;

(defun expand-functional-tag-name (name1)
  (if (symbolp name1)
      (let* ((name (symbol-name name1))
             (pos (position #\. name)))
        (if pos 
            (let* ((prefix (subseq name 0 pos)))
              (if (or (string= prefix "owl") ; only extensions have full qualified name in this implementation
                      (string= prefix "ol"))
                  (intern (subseq name (1+ pos)))
                (let ((ns (cdr (assoc prefix *last-request-xml-and-functional-namespaces* :test #'string=))))
                  (if ns
                      (intern (format nil "~A~A" ns (subseq name (1+ pos))))
                    name1))))
          name1))
    name1))

(defun get-tag-and-prefix (name1)
  (if (symbolp name1)
      (let* ((name (symbol-name name1))
             (pos (position #\. name)))
        (if pos 
            (values (intern (subseq name (1+ pos)))
                    (intern (subseq name 0 pos)))
          name1))
    name1))

