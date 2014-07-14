;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

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

(in-package :thematic-substrate)

;;;
;;; Mini-Lisp (terminierungssicher)
;;; Achtung! Mini-LIsp kennt keine Macros
;;; und behandelt diese als Funktionen. 
;;; Argumente von Makros muessen daher 
;;; u.U. gequotet werden, z.B. (retrieve '(?x) `(?x ,x))
;;; MiniLisp funktioniert nur im Package RACER-USER!
;;; 

(defun minilisp-function-p (name) 
  (multiple-value-bind (def foundp)
      (gethash (to-keyword-big name) *registered-functions*)
    (declare (ignorable def))
    foundp))

(defun minilisp-value-p (name) 
  (multiple-value-bind (def foundp)
      (gethash (to-keyword-big name) *registered-values*)
    (declare (ignorable def))
    foundp))


(defun minilisp-server-function-p (name) 
  (multiple-value-bind (def foundp)
      (gethash (to-keyword-big name) *registered-server-functions*)
    (declare (ignorable def))
    foundp))


(defun minilisp-server-value-p (name) 
  (multiple-value-bind (def foundp)
      (gethash (to-keyword-big name) *registered-server-values*)
    (declare (ignorable def))
    foundp))

(defun get-minilisp-function (name) 
  (multiple-value-bind (def foundp)
      (gethash (to-keyword-big name) *registered-functions*)
    (declare (ignorable foundp))
    (if foundp 
        def
      (minilisp-error "Function ~A not found" name))))


(defun get-minilisp-value (name) 
  (multiple-value-bind (def foundp)
      (gethash (to-keyword-big name) *registered-values*)
    (declare (ignorable foundp))
    (if foundp 
        def
      (minilisp-error "Value ~A not found" name))))


(defun get-minilisp-functions () 
  (let ((res nil))

    (maphash #'(lambda (key val)
                 (push `(define ,(change-package-of-description key :racer-user t)
                                ,(second val) ,(first val)) res))
             *registered-functions*)

    (sort res #'string<= :key #'(lambda (x) 
                                  (symbol-name (second x))))))


(defun get-minilisp-values () 
  (let ((res nil))

    (maphash #'(lambda (key val)
                 (push `(defcon ,(change-package-of-description key :racer-user t)
                                ,val)
                       res))
             *registered-values*)

    (sort res #'string<= :key #'(lambda (x) 
                                  (symbol-name (second x))))))



(defun get-minilisp-server-functions () 
  (let ((res nil))

    (maphash #'(lambda (key val)
                 (when (gethash key *registered-server-functions*)
                   (push `(define ,(change-package-of-description key :racer-user t)
                                  ,(second val) ,(first val)) res)))
             *registered-functions*)

    (sort res #'string<= :key #'(lambda (x) 
                                  (symbol-name (second x))))))


(defun get-minilisp-server-values () 
  (let ((res nil))

    (maphash #'(lambda (key val)
                 (when (gethash key *registered-server-values*)
                   (push `(defcon ,(change-package-of-description key :racer-user t)
                                  ,val) res)))
             *registered-values*)

    (sort res #'string<= :key #'(lambda (x) 
                                  (symbol-name (second x))))))

;;;
;;;
;;; 

(defun register-function (name arglist &rest body)
  (if (gethash (to-keyword-big name) +allowed-cl-functions+)
      
      (nrql-error "Built-in function ~A already exists, please use a different name" name)
      
    (setf (gethash (to-keyword-big name) *registered-functions*)
          (list `(progn ,@body) arglist))))


(defun register-value (name value)
  (setf (gethash (to-keyword-big name) *registered-values*)
        value)
  value)


(defun unregister-function (name)
  (remhash (to-keyword-big name) *registered-functions*))

(defun unregister-value (name)
  (remhash (to-keyword-big name) *registered-values*))

(defun register-as-server-function (name) 
  (if (minilisp-function-p name)
      (setf (gethash (to-keyword-big name) *registered-server-functions*) t)
    (minilisp-error "Function ~A not found, cannot register as server function" name)))

(defun register-as-server-value (name) 
  (if (minilisp-value-p name)
      (setf (gethash (to-keyword-big name) *registered-server-values*) t)
    (minilisp-error "Value ~A not found, cannot register as server value" name)))

;;;
;;;
;;;

(defun call-function (name &rest args)
  (let* ((name (to-keyword-big name))
	 (def (get-minilisp-function name))
         (body (first def))
         (fargs (second def)))
    (if (not (= (length args) 
                (length fargs)))
        (minilisp-error "Function ~A expected ~A arguments, but got ~A arguments" 
                        name 
                        (length fargs) 
                        (length args))
      (if (member name *call-stack*)
          (minilisp-error "Recursive function call of ~A detected, call aborted to ensure termination. Call history: ~A"
                          name 
                          (reverse *call-stack*))
        (let ((*call-stack* (cons name *call-stack*)))
          (eval-expr fargs
                     body 
                     args))))))

;;;
;;;
;;; 

(defun process-fun-arg (fn)
  (let* ((op1 (to-keyword-big fn))
         (op 
          (gethash op1 +allowed-cl-functions+)))
    (if op
        (if (second op) ;Makro?
            (minilisp-error "~A is a macro and cannot be used as function" fn)
          (if (fboundp fn)
              (symbol-function fn)
            (minilisp-error "~A is not a function" fn)))

      (when (get-minilisp-function op1)
        ;;; gibt error falls nicht bekannt!
        #'(lambda (&rest args)
            (apply #'call-function op1 args))))))

#|

(defun the-abox-name (abox)
  (if (symbolp abox)
      abox
    (racer::abox-name abox)))

(defun the-tbox-name (tbox)
  (if (symbolp tbox)
      tbox
    (racer::tbox-name tbox)))

|#

(defconstant +racer-functions+ (quote
                                ("MOST-SPECIFIC-INSTANTIATORS"
				 "INSTANTIATORS"
				 
				 "CL-EVALUATE"
				 "CL-EVALUATE-UNQUOTED"
				 
				 ;;;
				 ;;;
				 ;;;
				 
				 "DIG"
                                 "REMOTE-CONNECTIONS"
                                 "TOPLEVEL"
                                 "OWLAPI-ADD-AXIOM"
                                 "OWLAPI-ADD-AXIOMS"
                                 "OWLAPI-AXIOM-LOADED?"
                                 "OWLAPI-AXIOM-TO-ID"
                                 "OWLAPI-ID-TO-AXIOM"
                                 "OWLAPI-REMOVE-AXIOM"
                                 "OWLAPI-REMOVE-AXIOMS"
                                 "OWLAPI-SET-ONTOLOGY-URI"
                                 "OWLAPI-ABORT"
                                 "OWLAPI-ADD-PREFIX"
                                 "OWLAPI-ADVANCE-PROGRESS"
                                 "OWLAPI-APPLY-CHANGES"
                                 "OWLAPI-AUTO-ADD-AXIOMS-TO"
                                 "OWLAPI-AUTO-APPLY-CHANGES"
                                 "OWLAPI-AUTO-BATCH-ADD-AXIOMS-TO"
                                 "OWLAPI-AUTO-BATCH-REMOVE-AXIOMS-FROM"
                                 "OWLAPI-AUTO-REMOVE-AXIOMS-FROM"
                                 "OWLAPI-BATCH-SYNCHRONIZE"
                                 "OWLAPI-CLASSIFY"
                                 "OWLAPI-CLEAR-CHANGES"
                                 "OWLAPI-CLEAR-ONTOLOGIES"
                                 "OWLAPI-CLEAR-REGISTRY"
                                 "OWLAPI-CONSIDER-DECLARATIONS"
                                 "OWLAPI-CONTAINS"
                                 "OWLAPI-DESCRIBE-ONTOLOGIES"
                                 "OWLAPI-DESCRIBE-ONTOLOGY"
                                 "OWLAPI-DESCRIBE-REASONER"
                                 "OWLAPI-DESCRIBE-REASONERS"
                                 "OWLAPI-DISABLE-AUTO-MODE"
                                 "OWLAPI-DISABLE-INCREMENTAL-UPDATES"
                                 "OWLAPI-DISABLE-LOOKUP-MODE"
                                 "OWLAPI-DISABLE-MEMORY-SAVING-MODE"
                                 "OWLAPI-DISABLE-SIMPLIFIED-PROTOCOL"
                                 "OWLAPI-DISABLE-TRANSIENT-AXIOM-MODE"
                                 "OWLAPI-DISPOSE"
                                 "OWLAPI-DISPOSE-AXIOM"
                                 "OWLAPI-DISPOSE-AXIOMS"
                                 "OWLAPI-DISPOSE-ONTOLOGIES"
                                 "OWLAPI-DISPOSE-ONTOLOGY"
                                 "OWLAPI-DISPOSE-REASONER"
                                 "OWLAPI-DONT-REGISTER-DECLARED-ENTITIES"
                                 "OWLAPI-DONT-REGISTER-REFERENCED-ENTITIES"
                                 "OWLAPI-ENABLE-INCREMENTAL-UPDATES"
                                 "OWLAPI-ENABLE-LOOKUP-MODE"
                                 "OWLAPI-ENABLE-MEMORY-SAVING-MODE"
                                 "OWLAPI-ENABLE-SIMPLIFIED-PROTOCOL"
                                 "OWLAPI-ENABLE-TRANSIENT-AXIOM-MODE"
                                 "OWLAPI-EXPORT-ONTOLOGY"
                                 "OWLAPI-EXPORT-REASONER"
                                 "OWLAPI-FIND-ID-FROM-OBJECT"
                                 "OWLAPI-FIND-OBJECT-FROM-ID"
                                 "OWLAPI-GET-ALL-ONTOLOGIES"
                                 "OWLAPI-GET-ANCESTOR-CLASSES"
                                 "OWLAPI-GET-ANCESTOR-PROPERTIES"
                                 "OWLAPI-GET-ANNOTATION-AXIOMS-FOR-AXIOM"
                                 "OWLAPI-GET-AUTO-DECLARE-DATA-PROPERTIES"
                                 "OWLAPI-GET-AUTO-ONTOLOGY"
                                 "OWLAPI-GET-AXIOM-COUNTER"
                                 "OWLAPI-GET-AXIOMS"
                                 "OWLAPI-GET-AXIOMS-IN"
                                 "OWLAPI-GET-AXIOMS-OF-TYPE"
                                 "OWLAPI-GET-AXIOMS-OF-TYPE-IN"
                                 "OWLAPI-GET-AXIOMS-PER-ONTOLOGY"
                                 "OWLAPI-GET-CHANGES"
                                 "OWLAPI-GET-CURRENT-REASONER"
                                 "OWLAPI-GET-DATA-PROPERTY-RELATIONSHIPS"
                                 "OWLAPI-GET-DATA-PROPERTY-VALUES"
                                 "OWLAPI-GET-DESCENDANT-CLASSES"
                                 "OWLAPI-GET-DESCENDANT-PROPERTIES"
                                 "OWLAPI-GET-DIFFERENT-INDIVIDUALS"
                                 "OWLAPI-GET-DISJOINT-CLASSES"
                                 "OWLAPI-GET-DISJOINT-DATA-PROPERTIES"
                                 "OWLAPI-GET-DISJOINT-OBJECT-PROPERTIES"
                                 "OWLAPI-GET-DOMAINS"
                                 "OWLAPI-GET-EQUIVALENT-CLASSES"
                                 "OWLAPI-GET-EQUIVALENT-PROPERTIES"
                                 "OWLAPI-GET-INCONSISTENT-CLASSES"
                                 "OWLAPI-GET-INDIVIDUALS"
                                 "OWLAPI-GET-INSTANCES"
                                 "OWLAPI-GET-INVERSE-PROPERTIES"
                                 "OWLAPI-GET-LOADED-ONTOLOGIES"
                                 "OWLAPI-GET-OWL-ANNOTATION-ASSERTION-AXIOM"
                                 "OWLAPI-GET-OWL-ANNOTATION-PROPERTY-DOMAIN-AXIOM"
                                 "OWLAPI-GET-OWL-ANNOTATION-PROPERTY-RANGE-AXIOM"
                                 "OWLAPI-GET-OWL-ASYMMETRIC-OBJECT-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-AXIOM-ANNOTATION-AXIOM"
                                 "OWLAPI-GET-OWL-CLASS-ASSERTION-AXIOM"
                                 "OWLAPI-GET-OWL-DATA-PROPERTY-ASSERTION-AXIOM"
                                 "OWLAPI-GET-OWL-DATA-PROPERTY-DOMAIN-AXIOM"
                                 "OWLAPI-GET-OWL-DATA-PROPERTY-RANGE-AXIOM"
                                 "OWLAPI-GET-OWL-DATA-SUB-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-DATATYPE-DEFINITION-AXIOM"
                                 "OWLAPI-GET-OWL-DECLARATION-AXIOM"
                                 "OWLAPI-GET-OWL-DIFFERENT-INDIVIDUALS-AXIOM"
                                 "OWLAPI-GET-OWL-DISJOINT-CLASSES-AXIOM"
                                 "OWLAPI-GET-OWL-DISJOINT-DATA-PROPERTIES-AXIOM"
                                 "OWLAPI-GET-OWL-DISJOINT-OBJECT-PROPERTIES-AXIOM"
                                 "OWLAPI-GET-OWL-DISJOINT-UNION-AXIOM"
                                 "OWLAPI-GET-OWL-ENTITY-ANNOTATION-AXIOM"
                                 "OWLAPI-GET-OWL-EQUIVALENT-CLASSES-AXIOM"
                                 "OWLAPI-GET-OWL-EQUIVALENT-DATA-PROPERTIES-AXIOM"
                                 "OWLAPI-GET-OWL-EQUIVALENT-OBJECT-PROPERTIES-AXIOM"
                                 "OWLAPI-GET-OWL-FUNCTIONAL-DATA-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-FUNCTIONAL-OBJECT-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-HAS-KEY-AXIOM"
                                 "OWLAPI-GET-OWL-IMPLICIT-DECLARATION-AXIOM"
                                 "OWLAPI-GET-OWL-IMPORTS-DECLARATION-AXIOM"
                                 "OWLAPI-GET-OWL-INVERSE-FUNCTIONAL-OBJECT-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-INVERSE-OBJECT-PROPERTIES-AXIOM"
                                 "OWLAPI-GET-OWL-IRREFLEXIVE-OBJECT-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-NEGATIVE-DATA-PROPERTY-ASSERTION-AXIOM"
                                 "OWLAPI-GET-OWL-NEGATIVE-OBJECT-PROPERTY-ASSERTION-AXIOM"
                                 "OWLAPI-GET-OWL-OBJECT-PROPERTY-ASSERTION-AXIOM"
                                 "OWLAPI-GET-OWL-OBJECT-PROPERTY-CHAIN-SUB-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-OBJECT-PROPERTY-DOMAIN-AXIOM"
                                 "OWLAPI-GET-OWL-OBJECT-PROPERTY-RANGE-AXIOM"
                                 "OWLAPI-GET-OWL-OBJECT-SUB-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-ONTOLOGY-ANNOTATION-AXIOM"
                                 "OWLAPI-GET-OWL-ONTOLOGY-VERSION-DECLARATION-AXIOM"
                                 "OWLAPI-GET-OWL-PREFIX-DECLARATION-AXIOM"
                                 "OWLAPI-GET-OWL-REALLY-IMPLICIT-DECLARATION-AXIOM"
                                 "OWLAPI-GET-OWL-REFLEXIVE-OBJECT-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-SAME-INDIVIDUALS-AXIOM"
                                 "OWLAPI-GET-OWL-SUB-ANNOTATION-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-SUB-ANNOTATION-PROPERTY-OF-AXIOM"
                                 "OWLAPI-GET-OWL-SUB-CLASS-AXIOM"
                                 "OWLAPI-GET-OWL-SYMMETRIC-OBJECT-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-TRANSITIVE-OBJECT-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OBJECT-PROPERTY-RELATIONSHIPS"
                                 "OWLAPI-GET-OBJECT-PROPERTY-VALUES"
                                 "OWLAPI-GET-ONTOLOGIES"
                                 "OWLAPI-GET-PREFIXES"
                                 "OWLAPI-GET-RANGES"
                                 "OWLAPI-GET-REASONERS"
                                 "OWLAPI-GET-RELATED-INDIVIDUALS"
                                 "OWLAPI-GET-RELATED-VALUES"
                                 "OWLAPI-GET-SAME-INDIVIDUALS"
                                 "OWLAPI-GET-SUB-CLASSES"
                                 "OWLAPI-GET-SUB-PROPERTIES"
                                 "OWLAPI-GET-SUPER-CLASSES"
                                 "OWLAPI-GET-SUPER-PROPERTIES"
                                 "OWLAPI-GET-TYPES"
                                 "OWLAPI-HAS-DATA-PROPERTY-RELATIONSHIP"
                                 "OWLAPI-HAS-OBJECT-PROPERTY-RELATIONSHIP"
                                 "OWLAPI-HAS-TYPE"
                                 "OWLAPI-IGNORE-ANNOTATIONS"
                                 "OWLAPI-IGNORE-DECLARATIONS"
                                 "OWLAPI-INIT"
                                 "OWLAPI-IS-ASYMMETRIC"
                                 "OWLAPI-IS-CLASS"
                                 "OWLAPI-IS-CLASSIFIED"
                                 "OWLAPI-IS-CONSISTENT"
                                 "OWLAPI-IS-DEFINED-CLASS"
                                 "OWLAPI-IS-DEFINED-DATA-PROPERTY"
                                 "OWLAPI-IS-DEFINED-INDIVIDUAL"
                                 "OWLAPI-IS-DEFINED-OBJECT-PROPERTY"
                                 "OWLAPI-IS-DIFFERENT-INDIVIDUAL"
                                 "OWLAPI-IS-ENTAILED"
                                 "OWLAPI-IS-EQUIVALENT-CLASS"
                                 "OWLAPI-IS-FUNCTIONAL"
                                 "OWLAPI-IS-INVERSE-FUNCTIONAL"
                                 "OWLAPI-IS-IRREFLEXIVE"
                                 "OWLAPI-IS-REALISED"
                                 "OWLAPI-IS-REFLEXIVE"
                                 "OWLAPI-IS-SAME-INDIVIDUAL"
                                 "OWLAPI-IS-SATISFIABLE"
                                 "OWLAPI-IS-SUB-CLASS-OF"
                                 "OWLAPI-IS-SYMMETRIC"
                                 "OWLAPI-IS-TRANSITIVE"
                                 "OWLAPI-KEEP-ANNOTATIONS"
                                 "OWLAPI-LOAD-AXIOM"
                                 "OWLAPI-LOAD-AXIOMS"
                                 "OWLAPI-LOAD-ONTOLOGIES"
                                 "OWLAPI-LOAD-ONTOLOGY"
                                 "OWLAPI-MANUALLY-APPLY-CHANGES"
                                 "OWLAPI-MERGE-ONTOLOGIES"
                                 "OWLAPI-NEW-ONTOLOGY"
                                 "OWLAPI-NEW-REASONER"
                                 "OWLAPI-NEW-REASONER1"
                                 "OWLAPI-NEXT-AXIOM-USE-ID"
                                 "OWLAPI-PARSE"
                                 "OWLAPI-PARSE-NATIVE"
                                 "OWLAPI-READ-FUNCTIONAL-ONTOLOGY-DOCUMENT"
                                 "OWLAPI-READ-FUNCTIONAL-ONTOLOGY-FILE"
                                 "OWLAPI-READ-ONTOLOGY"
                                 "OWLAPI-READ-XML-ONTOLOGY-DOCUMENT"
                                 "OWLAPI-READ-XML-ONTOLOGY-FILE"
                                 "OWLAPI-REALIZE"
                                 "OWLAPI-REGISTER-DECLARED-ENTITIES"
                                 "OWLAPI-REGISTER-LAST-ANSWER"
                                 "OWLAPI-REGISTER-OBJECT"
                                 "OWLAPI-REGISTER-REFERENCED-ENTITIES"
                                 "OWLAPI-RELOAD-LOADED-ONTOLOGIES"
                                 "OWLAPI-REMOVE-PREFIX"
                                 "OWLAPI-RESET-AXIOM-COUNTER"
                                 "OWLAPI-RESTORE-IMAGE"
                                 "OWLAPI-SAVE-ONTOLOGY"
                                 "OWLAPI-SET-AUTO-DECLARE-DATA-PROPERTIES"
                                 "OWLAPI-SET-AXIOM-COUNTER"
                                 "OWLAPI-SET-CURRENT-REASONER"
                                 "OWLAPI-SET-PROGRESS"
                                 "OWLAPI-SET-PROGRESS-RANGE"
                                 "OWLAPI-SET-RETURN-POLICY"
                                 "OWLAPI-SLEEP"
                                 "OWLAPI-STORE-IMAGE"
                                 "OWLAPI-UNLOAD-AXIOM"
                                 "OWLAPI-UNLOAD-AXIOMS"
                                 "OWLAPI-UNLOAD-ONTOLOGIES"
                                 "OWLAPI-UNLOAD-ONTOLOGY"
                                 "OWLAPI-USES-INCREMENTAL-UPDATES"
                                 "OWLAPI-USES-SIMPLIFIED-PROTOCOL"
                                 "OWLAPI-WRITE-FUNCTIONAL-ONTOLOGY-FILE"
                                 "OWLAPI-WRITE-ONTOLOGY-FILE"
                                 "OWLAPI-WRITE-XML-ONTOLOGY-FILE"
                                 "XML-READ-TBOX-FILE"
                                 "XML-OUTPUT"
                                 "XML-NATIVE-OUTPUT"
                                 "XML-INPUT"
                                 "WITHOUT-UNIQUE-NAME-ASSUMPTION"
                                 "WITH-UNIQUE-NAME-ASSUMPTION"
                                 "WITH-NRQL-SETTINGS-EVALUATED"
                                 "WITH-NRQL-SETTINGS"
                                 "WITH-FUTURE-BINDINGS-EVALUATED"
                                 "WITH-FUTURE-BINDINGS"
                                 "WITH-CRITICAL-SECTION"
                                 "WITH-BINDINGS-EVALUATED"
                                 "WITH-BINDINGS"
                                 "WAITING-RULES"
                                 "WAITING-QUERIES"
                                 "WAITING-EXPENSIVE-RULES"
                                 "WAITING-EXPENSIVE-QUERIES"
                                 "WAITING-CHEAP-RULES"
                                 "WAITING-CHEAP-QUERIES"
                                 "WAIT-FOR-RULES-TO-TERMINATE"
                                 "WAIT-FOR-QUERIES-TO-TERMINATE"
                                 "VERIFY-WITH-CONCEPT-TREE-LIST"
                                 "VERIFY-WITH-ABOX-INDIVIDUALS-LIST"
                                 "USE-TRIPLE-STORE"
                                 "USE-INJECTIVE-VARIABLES-BY-DEFAULT"
                                 "USE-INDIVIDUAL-SYNONYM-EQUIVALENCE-CLASSES"
                                 "UPDATE-RACER"
                                 "UNSUBSCRIBE-FROM"
                                 "UNSUBSCRIBE-1"
                                 "UNSUBSCRIBE"
                                 "UNRELATED"
                                 "UNPUBLISH-1"
                                 "UNPUBLISH"
                                 "UNDEFQUERY"
                                 "UNDEFINE1"
                                 "UNDEFINE-QUERY"
                                 "UNDEFINE-ALL"
                                 "UNDEFINE"
                                 "UNBIND1"
                                 "UNBIND-ALL"
                                 "UNBIND"
                                 "UNAPPLICABLE-RULES"
                                 "TRIPLE-STORE-READ-FILE"
                                 "TRIPLE-STORE-OPEN-P"
                                 "TRIPLE-STORE-GRAPHS"
                                 "TRANSMIT-FILE"
                                 "TRANSITIVE?"
                                 "TRANSITIVE-P"
                                 "TRANSITIVE"
                                 "TOLD-VALUE"
                                 "TIMENET-RETRIEVE"
                                 "TIMENET-ANSWER-QUERY"
                                 "TIME"
                                 "TERMINATED-RULES"
                                 "TERMINATED-QUERIES"
                                 "TBOX-RETRIEVE1"
                                 "TBOX-RETRIEVE"
                                 "TBOX-PREPARED?"
                                 "TBOX-PREPARED-P"
                                 "TBOX-CYCLIC?"
                                 "TBOX-CYCLIC-P"
                                 "TBOX-COHERENT?"
                                 "TBOX-COHERENT-P"
                                 "TBOX-CLASSIFIED?"
                                 "TBOX-CLASSIFIED-P"
                                 "TAXONOMY"
                                 "SYMMETRIC?"
                                 "SYMMETRIC-P"
                                 "SYMMETRIC"
                                 "SWRL-FORWARD-CHAINING"
                                 "SWRL-CREATE-FORWARD-CHAINGING-RULES"
                                 "SWRL-CREATE-ABDUCTION-RULES-IF-POSSIBLE"
                                 "SUBSCRIBE-TO"
                                 "SUBSCRIBE-1"
                                 "SUBSCRIBE"
                                 "STORE-TBOXES-IMAGE"
                                 "STORE-TBOX-IMAGE"
                                 "STORE-SUBSTRATE-FOR-ABOX"
                                 "STORE-SERVER-IMAGE"
                                 "STORE-KBS-IMAGE"
                                 "STORE-KB-IMAGE"
                                 "STORE-ALL-SUBSTRATES"
                                 "STORE-ABOXES-IMAGE"
                                 "STORE-ABOX-IMAGE"
                                 "STATE"
                                 "SPARQL-RETRIEVE"
                                 "SPARQL-ANSWER-QUERY"
                                 "SLEEPING-RULES"
                                 "SLEEPING-QUERIES"
                                 "SLEEPING-EXPENSIVE-RULES"
                                 "SLEEPING-EXPENSIVE-QUERIES"
                                 "SLEEPING-CHEAP-RULES"
                                 "SLEEPING-CHEAP-QUERIES"
                                 "SIGNATURE"
                                 "SHOW-QBOX-FOR-ABOX"
                                 "SET-UNIQUE-NAME-ASSUMPTION"
                                 "SET-SUBSTRATE-TYPE"
                                 "SET-SERVER-TIMEOUT"
                                 "SET-REWRITE-DEFINED-CONCEPTS"
                                 "SET-RCC-BOX"
                                 "SET-RACER-PARAMETER"
                                 "SET-PROXY-SERVER"
                                 "SET-NRQL-MODE"
                                 "SET-NEW-IND-PREFIX"
                                 "SET-NEW-IND-COUNTER"
                                 "SET-MIRROR-DATA-BOX"
                                 "SET-MAXIMUM-SIZE-OF-PROCESS-POOL"
                                 "SET-MAX-NO-OF-TUPLES-BOUND"
                                 "SET-INITIAL-SIZE-OF-PROCESS-POOL"
                                 "SET-FIND-TBOX"
                                 "SET-FIND-ABOX"
                                 "SET-EDGE-LABEL-FOR-NON-EXISTENT-EDGES"
                                 "SET-DATA-BOX"
                                 "SET-CURRENT-TBOX"
                                 "SET-CURRENT-ABOX"
                                 "SET-ATTRIBUTE-FILLER"
                                 "SERVER-VALUE"
                                 "SERVER-FUNCTION"
                                 "SERVER-CASE"
                                 "SEQUENCE"
                                 "SAVE-TBOX"
                                 "SAVE-ONTOLOGY-TO-TRIPLE-STORE"
                                 "SAVE-KB"
                                 "SAVE-ABOX"
                                 "SAME-INDIVIDUAL-AS"
                                 "SAME-AS"
                                 "RUNNING-RULES"
                                 "RUNNING-QUERIES"
                                 "RUNNING-EXPENSIVE-RULES"
                                 "RUNNING-EXPENSIVE-QUERIES"
                                 "RUNNING-CHEAP-RULES"
                                 "RUNNING-CHEAP-QUERIES"
                                 "RUN-ALL-RULES"
                                 "RUN-ALL-QUERIES"
                                 "RULE-WAITING-P"
                                 "RULE-UNAPPLICABLE-P"
                                 "RULE-TERMINATED-P"
                                 "RULE-SLEEPING-P"
                                 "RULE-RUNNING-P"
                                 "RULE-READY-P"
                                 "RULE-PROCESSED-P"
                                 "RULE-PREPARED-P"
                                 "RULE-CONSISTENT-P"
                                 "RULE-CONSEQUENCE"
                                 "RULE-APPLICABLE-P"
                                 "RULE-ANTECEDENCE"
                                 "RULE-ACTIVE-P"
                                 "RULE-ACCURATE-P"
                                 "ROLES-EQUIVALENT-1"
                                 "ROLES-EQUIVALENT"
                                 "ROLES-DISJOINT-1"
                                 "ROLES-DISJOINT"
                                 "ROLE?"
                                 "ROLE-USED-AS-DATATYPE-PROPERTY-P"
                                 "ROLE-USED-AS-ANNOTATION-PROPERTY-P"
                                 "ROLE-SYNONYMS"
                                 "ROLE-SUBSUMES?"
                                 "ROLE-SUBSUMES-P"
                                 "ROLE-SATISFIABLE?"
                                 "ROLE-SATISFIABLE-P"
                                 "ROLE-RANGE"
                                 "ROLE-PARENTS"
                                 "ROLE-P"
                                 "ROLE-IS-USED-AS-DATATYPE-PROPERTY"
                                 "ROLE-IS-USED-AS-ANNOTATION-PROPERTY"
                                 "ROLE-IS-TRANSITIVE"
                                 "ROLE-IS-SYMMETRIC"
                                 "ROLE-IS-REFLEXIVE"
                                 "ROLE-IS-IRREFLEXIVE"
                                 "ROLE-IS-FUNCTIONAL"
                                 "ROLE-IS-ASYMMETRIC"
                                 "ROLE-INVERSE"
                                 "ROLE-HAS-RANGE"
                                 "ROLE-HAS-PARENT"
                                 "ROLE-HAS-DOMAIN"
                                 "ROLE-EQUIVALENT?"
                                 "ROLE-EQUIVALENT-P"
                                 "ROLE-DOMAIN"
                                 "ROLE-DISJOINT?"
                                 "ROLE-DISJOINT-P"
                                 "ROLE-DESCENDANTS"
                                 "ROLE-CHILDREN"
                                 "ROLE-ANCESTORS"
                                 "RMI"
                                 "RETRIEVE1"
                                 "RETRIEVE-WITH-EXPLANATION"
                                 "RETRIEVE-UNDER-PREMISE1"
                                 "RETRIEVE-UNDER-PREMISE"
                                 "RETRIEVE-RELATED-INDIVIDUALS"
                                 "RETRIEVE-INDIVIDUAL-TOLD-DATATYPE-FILLERS"
                                 "RETRIEVE-INDIVIDUAL-TOLD-ATTRIBUTE-VALUE"
                                 "RETRIEVE-INDIVIDUAL-SYNONYMS"
                                 "RETRIEVE-INDIVIDUAL-FILLERS"
                                 "RETRIEVE-INDIVIDUAL-FILLED-ROLES"
                                 "RETRIEVE-INDIVIDUAL-ATTRIBUTE-FILLERS"
                                 "RETRIEVE-INDIVIDUAL-ANTONYMS"
                                 "RETRIEVE-INDIVIDUAL-ANNOTATION-PROPERTY-FILLERS"
                                 "RETRIEVE-DIRECT-PREDECESSORS"
                                 "RETRIEVE-CONCEPT-INSTANCES"
                                 "RETRIEVE"
                                 "RESTORE-TBOXES-IMAGE"
                                 "RESTORE-TBOX-IMAGE"
                                 "RESTORE-SUBSTRATE"
                                 "RESTORE-STANDARD-SETTINGS"
                                 "RESTORE-SERVER-IMAGE"
                                 "RESTORE-KBS-IMAGE"
                                 "RESTORE-KB-IMAGE"
                                 "RESTORE-ALL-SUBSTRATES"
                                 "RESTORE-ABOXES-IMAGE"
                                 "RESTORE-ABOX-IMAGE"
                                 "RESET-NRQL-ENGINE"
                                 "RESET-ALL-SUBSTRATES"
                                 "REPREPARE-RULE"
                                 "REPREPARE-QUERY"
                                 "REPORT-INCONSISTENT-QUERIES-AND-RULES"
                                 "REMOVE-IMPLIED-CONCEPT-ASSERTIONS"
                                 "RELATED-INDIVIDUALS"
                                 "RELATED"
                                 "REGISTER-RCC-SYNONYM"
                                 "REFLEXIVE?"
                                 "REFLEXIVE-P"
                                 "REFLEXIVE"
                                 "REEXECUTE-RULE"
                                 "REEXECUTE-QUERY"
                                 "REEXECUTE-ALL-RULES"
                                 "REEXECUTE-ALL-QUERIES"
                                 "RECOGNIZE-EVENTS"
                                 "REALIZE-ABOX"
                                 "READY-RULES"
                                 "READY-QUERIES"
                                 "RDFS-READ-TBOX-FILE"
                                 "RCC-SYNONYM"
                                 "RCC-RELATED1"
                                 "RCC-RELATED"
                                 "RCC-NODE1"
                                 "RCC-NODE-LABEL1"
                                 "RCC-NODE-LABEL"
                                 "RCC-NODE-DESCRIPTION1"
                                 "RCC-NODE-DESCRIPTION"
                                 "RCC-NODE"
                                 "RCC-INSTANCE1"
                                 "RCC-INSTANCE"
                                 "RCC-EDGE1"
                                 "RCC-EDGE-LABEL1"
                                 "RCC-EDGE-LABEL"
                                 "RCC-EDGE-DESCRIPTION1"
                                 "RCC-EDGE-DESCRIPTION"
                                 "RCC-EDGE"
                                 "RCC-CONSISTENT?"
                                 "RCC-CONSISTENT-P"
                                 "RANGE"
                                 "RACER-READ-FILE"
                                 "RACER-READ-DOCUMENT"
                                 "RACER-PREPARE-TBOX-QUERY1"
                                 "RACER-PREPARE-TBOX-QUERY"
                                 "RACER-PREPARE-RULE1"
                                 "RACER-PREPARE-RULE"
                                 "RACER-PREPARE-QUERY1"
                                 "RACER-PREPARE-QUERY"
                                 "RACER-APPLY-RULE1"
                                 "RACER-APPLY-RULE-UNDER-PREMISE1"
                                 "RACER-APPLY-RULE-UNDER-PREMISE"
                                 "RACER-APPLY-RULE"
                                 "RACER-ANSWER-TBOX-QUERY1"
                                 "RACER-ANSWER-TBOX-QUERY"
                                 "RACER-ANSWER-QUERY1"
                                 "RACER-ANSWER-QUERY-WITH-EXPLANATION"
                                 "RACER-ANSWER-QUERY-UNDER-PREMISE1"
                                 "RACER-ANSWER-QUERY-UNDER-PREMISE"
                                 "RACER-ANSWER-QUERY"
                                 "QUIET-SEQUENCE"
                                 "QUERY-WAITING-P"
                                 "QUERY-TERMINATED-P"
                                 "QUERY-SUBSCRIBERS"
                                 "QUERY-SLEEPING-P"
                                 "QUERY-RUNNING-P"
                                 "QUERY-READY-P"
                                 "QUERY-PROCESSED-P"
                                 "QUERY-PREPARED-P"
                                 "QUERY-PARENTS"
                                 "QUERY-HEAD"
                                 "QUERY-EQUIVALENTS"
                                 "QUERY-EQUIVALENT-P"
                                 "QUERY-ENTAILS-P"
                                 "QUERY-DESCENDANTS"
                                 "QUERY-CONSISTENT-P"
                                 "QUERY-CHILDREN"
                                 "QUERY-BODY"
                                 "QUERY-ANCESTORS"
                                 "QUERY-ACTIVE-P"
                                 "QUERY-ACCURATE-P"
                                 "PUBLISH-FILE"
                                 "PUBLISH-1"
                                 "PUBLISH"
                                 "PROCESSED-RULES"
                                 "PROCESSED-QUERIES"
                                 "PROCESS-TUPLE-AT-A-TIME"
                                 "PROCESS-SET-AT-A-TIME"
                                 "PRINT-TBOX-TREE"
                                 "PRINT-ABOX-INDIVIDUALS"
                                 "PRETRIEVE"
                                 "PREPRULE1"
                                 "PREPRULE"
                                 "PREPARED-RULES"
                                 "PREPARED-QUERIES"
                                 "PREPARE-TBOX-QUERY1"
                                 "PREPARE-TBOX-QUERY"
                                 "PREPARE-RULE1"
                                 "PREPARE-RULE"
                                 "PREPARE-RACER-ENGINE"
                                 "PREPARE-QUERY1"
                                 "PREPARE-QUERY"
                                 "PREPARE-NRQL-ENGINE"
                                 "PREPARE-ABOX-RULE1"
                                 "PREPARE-ABOX-RULE"
                                 "PREPARE-ABOX-QUERY1"
                                 "PREPARE-ABOX-QUERY"
                                 "PREPARE-ABOX"
                                 "PREFER-DEFINED-QUERIES"
                                 "PRACER-ANSWER-QUERY"
                                 "OWLLINK-READ-FILE"
                                 "OWLLINK-READ-DOCUMENT"
                                 "OWLAPI-writeXMLOntologyFile"
                                 "OWLAPI-writeOntologyFile"
                                 "OWLAPI-writeFunctionalOntologyFile"
                                 "OWLAPI-usesSimplifiedProtocol"
                                 "OWLAPI-usesIncrementalUpdates"
                                 "OWLAPI-unloadOntology"
                                 "OWLAPI-unloadOntologies"
                                 "OWLAPI-unloadAxioms"
                                 "OWLAPI-unloadAxiom"
                                 "OWLAPI-storeImage"
                                 "OWLAPI-sleep"
                                 "OWLAPI-setReturnPolicy"
                                 "OWLAPI-setCurrentReasoner"
                                 "OWLAPI-setAxiomCounter"
                                 "OWLAPI-setAutoDeclareDataProperties"
                                 "OWLAPI-sequence"
                                 "OWLAPI-saveOntology"
                                 "OWLAPI-restoreImage"
                                 "OWLAPI-resetAxiomCounter"
                                 "OWLAPI-removePrefix"
                                 "OWLAPI-removeAxioms"
                                 "OWLAPI-removeAxiom"
                                 "OWLAPI-reloadLoadedOntologies"
                                 "OWLAPI-registerReferencedEntities"
                                 "OWLAPI-registerObject"
                                 "OWLAPI-registerLastAnswer"
                                 "OWLAPI-registerDeclaredEntities"
                                 "OWLAPI-realize"
                                 "OWLAPI-readXMLOntologyFile"
                                 "OWLAPI-readXMLOntologyDocument"
                                 "OWLAPI-readOntology"
                                 "OWLAPI-readFunctionalOntologyFile"
                                 "OWLAPI-readFunctionalOntologyDocument"
                                 "OWLAPI-quietSequence"
                                 "OWLAPI-parseNative"
                                 "OWLAPI-parse"
                                 "OWLAPI-nextAxiomUseID"
                                 "OWLAPI-newReasoner1"
                                 "OWLAPI-newReasoner"
                                 "OWLAPI-newOntology"
                                 "OWLAPI-mergeOntologies"
                                 "OWLAPI-manuallyApplyChanges"
                                 "OWLAPI-loadOntology"
                                 "OWLAPI-loadOntologies"
                                 "OWLAPI-loadAxioms"
                                 "OWLAPI-loadAxiom"
                                 "OWLAPI-keepAnnotations"
                                 "OWLAPI-isTransitive"
                                 "OWLAPI-isSymmetric"
                                 "OWLAPI-isSubClassOf"
                                 "OWLAPI-isSatisfiable"
                                 "OWLAPI-isSameIndividual"
                                 "OWLAPI-isReflexive"
                                 "OWLAPI-isRealised"
                                 "OWLAPI-isIrreflexive"
                                 "OWLAPI-isInverseFunctional"
                                 "OWLAPI-isFunctional"
                                 "OWLAPI-isEquivalentClass"
                                 "OWLAPI-isEntailed"
                                 "OWLAPI-isDifferentIndividual"
                                 "OWLAPI-isDefinedObjectProperty"
                                 "OWLAPI-isDefinedIndividual"
                                 "OWLAPI-isDefinedDataProperty"
                                 "OWLAPI-isDefinedClass"
                                 "OWLAPI-isConsistent"
                                 "OWLAPI-isClassified"
                                 "OWLAPI-isClass"
                                 "OWLAPI-isAsymmetric"
                                 "OWLAPI-init"
                                 "OWLAPI-ignoreDeclarations"
                                 "OWLAPI-ignoreAnnotations"
                                 "OWLAPI-hasType"
                                 "OWLAPI-hasObjectPropertyRelationship"
                                 "OWLAPI-hasDataPropertyRelationship"
                                 "OWLAPI-getTypes"
                                 "OWLAPI-getSuperProperties"
                                 "OWLAPI-getSuperClasses"
                                 "OWLAPI-getSubProperties"
                                 "OWLAPI-getSubClasses"
                                 "OWLAPI-getSameIndividuals"
                                 "OWLAPI-getRelatedValues"
                                 "OWLAPI-getRelatedIndividuals"
                                 "OWLAPI-getReasoners"
                                 "OWLAPI-getRanges"
                                 "OWLAPI-getPrefixes"
                                 "OWLAPI-getOntologies"
                                 "OWLAPI-getObjectPropertyValues"
                                 "OWLAPI-getObjectPropertyRelationships"
                                 "OWLAPI-getOWLTransitiveObjectPropertyAxiom"
                                 "OWLAPI-getOWLSymmetricObjectPropertyAxiom"
                                 "OWLAPI-getOWLSubClassAxiom"
                                 "OWLAPI-getOWLSubAnnotationPropertyOfAxiom"
                                 "OWLAPI-getOWLSubAnnotationPropertyAxiom"
                                 "OWLAPI-getOWLSameIndividualsAxiom"
                                 "OWLAPI-getOWLReflexiveObjectPropertyAxiom"
                                 "OWLAPI-getOWLReallyImplicitDeclarationAxiom"
                                 "OWLAPI-getOWLPrefixDeclarationAxiom"
                                 "OWLAPI-getOWLOntologyVersionDeclarationAxiom"
                                 "OWLAPI-getOWLOntologyAnnotationAxiom"
                                 "OWLAPI-getOWLObjectSubPropertyAxiom"
                                 "OWLAPI-getOWLObjectPropertyRangeAxiom"
                                 "OWLAPI-getOWLObjectPropertyDomainAxiom"
                                 "OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom"
                                 "OWLAPI-getOWLObjectPropertyAssertionAxiom"
                                 "OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom"
                                 "OWLAPI-getOWLNegativeDataPropertyAssertionAxiom"
                                 "OWLAPI-getOWLIrreflexiveObjectPropertyAxiom"
                                 "OWLAPI-getOWLInverseObjectPropertiesAxiom"
                                 "OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom"
                                 "OWLAPI-getOWLImportsDeclarationAxiom"
                                 "OWLAPI-getOWLImplicitDeclarationAxiom"
                                 "OWLAPI-getOWLHasKeyAxiom"
                                 "OWLAPI-getOWLFunctionalObjectPropertyAxiom"
                                 "OWLAPI-getOWLFunctionalDataPropertyAxiom"
                                 "OWLAPI-getOWLEquivalentObjectPropertiesAxiom"
                                 "OWLAPI-getOWLEquivalentDataPropertiesAxiom"
                                 "OWLAPI-getOWLEquivalentClassesAxiom"
                                 "OWLAPI-getOWLEntityAnnotationAxiom"
                                 "OWLAPI-getOWLDisjointUnionAxiom"
                                 "OWLAPI-getOWLDisjointObjectPropertiesAxiom"
                                 "OWLAPI-getOWLDisjointDataPropertiesAxiom"
                                 "OWLAPI-getOWLDisjointClassesAxiom"
                                 "OWLAPI-getOWLDifferentIndividualsAxiom"
                                 "OWLAPI-getOWLDeclarationAxiom"
                                 "OWLAPI-getOWLDatatypeDefinitionAxiom"
                                 "OWLAPI-getOWLDataSubPropertyAxiom"
                                 "OWLAPI-getOWLDataPropertyRangeAxiom"
                                 "OWLAPI-getOWLDataPropertyDomainAxiom"
                                 "OWLAPI-getOWLDataPropertyAssertionAxiom"
                                 "OWLAPI-getOWLClassAssertionAxiom"
                                 "OWLAPI-getOWLAxiomAnnotationAxiom"
                                 "OWLAPI-getOWLAsymmetricObjectPropertyAxiom"
                                 "OWLAPI-getOWLAnnotationPropertyRangeAxiom"
                                 "OWLAPI-getOWLAnnotationPropertyDomainAxiom"
                                 "OWLAPI-getOWLAnnotationAssertionAxiom"
                                 "OWLAPI-getLoadedOntologies"
                                 "OWLAPI-getLastOutputStreamString"
                                 "OWLAPI-getLastAnswer"
                                 "OWLAPI-getInverseProperties"
                                 "OWLAPI-getInstances"
                                 "OWLAPI-getIndividuals"
                                 "OWLAPI-getInconsistentClasses"
                                 "OWLAPI-getIDsOfLastAnswer"
                                 "OWLAPI-getEquivalentProperties"
                                 "OWLAPI-getEquivalentClasses"
                                 "OWLAPI-getDomains"
                                 "OWLAPI-getDisjointObjectProperties"
                                 "OWLAPI-getDisjointDataProperties"
                                 "OWLAPI-getDisjointClasses"
                                 "OWLAPI-getDifferentIndividuals"
                                 "OWLAPI-getDescendantProperties"
                                 "OWLAPI-getDescendantClasses"
                                 "OWLAPI-getDataPropertyValues"
                                 "OWLAPI-getDataPropertyRelationships"
                                 "OWLAPI-getCurrentReasoner"
                                 "OWLAPI-getChanges"
                                 "OWLAPI-getAxiomsPerOntology"
                                 "OWLAPI-getAxiomsOfTypeIn"
                                 "OWLAPI-getAxiomsOfType"
                                 "OWLAPI-getAxiomsIn"
                                 "OWLAPI-getAxioms"
                                 "OWLAPI-getAxiomCounter"
                                 "OWLAPI-getAutoOntology"
                                 "OWLAPI-getAutoDeclareDataProperties"
                                 "OWLAPI-getAnnotationAxiomsForAxiom"
                                 "OWLAPI-getAncestorProperties"
                                 "OWLAPI-getAncestorClasses"
                                 "OWLAPI-getAllOntologies"
                                 "OWLAPI-findObjectFromID"
                                 "OWLAPI-findIDFromObject"
                                 "OWLAPI-exportReasoner"
                                 "OWLAPI-exportOntology"
                                 "OWLAPI-enableTransientAxiomMode"
                                 "OWLAPI-enableSimplifiedProtocol"
                                 "OWLAPI-enableMemorySavingMode"
                                 "OWLAPI-enableLookupMode"
                                 "OWLAPI-enableIncrementalUpdates"
                                 "OWLAPI-dontRegisterReferencedEntities"
                                 "OWLAPI-dontRegisterDeclaredEntities"
                                 "OWLAPI-disposeReasoner"
                                 "OWLAPI-disposeOntology"
                                 "OWLAPI-disposeOntologies"
                                 "OWLAPI-disposeAxioms"
                                 "OWLAPI-disposeAxiom"
                                 "OWLAPI-dispose"
                                 "OWLAPI-disableTransientAxiomMode"
                                 "OWLAPI-disableSimplifiedProtocol"
                                 "OWLAPI-disableMemorySavingMode"
                                 "OWLAPI-disableLookupMode"
                                 "OWLAPI-disableIncrementalUpdates"
                                 "OWLAPI-disableAutoMode"
                                 "OWLAPI-describeReasoners"
                                 "OWLAPI-describeReasoner"
                                 "OWLAPI-describeOntology"
                                 "OWLAPI-describeOntologies"
                                 "OWLAPI-contains"
                                 "OWLAPI-considerDeclarations"
                                 "OWLAPI-clearRegistry"
                                 "OWLAPI-clearOntologies"
                                 "OWLAPI-clearChanges"
                                 "OWLAPI-classify"
                                 "OWLAPI-batchSynchronize"
                                 "OWLAPI-autoRemoveAxiomsFrom"
                                 "OWLAPI-autoBatchRemoveAxiomsFrom"
                                 "OWLAPI-autoBatchAddAxiomsTo"
                                 "OWLAPI-autoApplyChanges"
                                 "OWLAPI-autoAddAxiomsTo"
                                 "OWLAPI-applyChanges"
                                 "OWLAPI-answerSequence"
                                 "OWLAPI-addPrefix"
                                 "OWLAPI-addAxioms"
                                 "OWLAPI-addAxiom"
                                 "OWLAPI-abort"
                                 "OWLAPI-WRITE-XML-ONTOLOGY-FILE"
                                 "OWLAPI-WRITE-ONTOLOGY-FILE"
                                 "OWLAPI-WRITE-FUNCTIONAL-ONTOLOGY-FILE"
                                 "OWLAPI-USES-SIMPLIFIED-PROTOCOL"
                                 "OWLAPI-USES-INCREMENTAL-UPDATES"
                                 "OWLAPI-UNLOAD-ONTOLOGY"
                                 "OWLAPI-UNLOAD-ONTOLOGIES"
                                 "OWLAPI-UNLOAD-AXIOMS"
                                 "OWLAPI-UNLOAD-AXIOM"
                                 "OWLAPI-SetOntologyURI"
                                 "OWLAPI-STORE-IMAGE"
                                 "OWLAPI-SLEEP"
                                 "OWLAPI-SET-RETURN-POLICY"
                                 "OWLAPI-SET-ONTOLOGY-URI"
                                 "OWLAPI-SET-CURRENT-REASONER"
                                 "OWLAPI-SET-AXIOM-COUNTER"
                                 "OWLAPI-SET-AUTO-DECLARE-DATA-PROPERTIES"
                                 "OWLAPI-SEQUENCE"
                                 "OWLAPI-SAVE-ONTOLOGY"
                                 "OWLAPI-RemoveAxioms"
                                 "OWLAPI-RemoveAxiom"
                                 "OWLAPI-RESTORE-IMAGE"
                                 "OWLAPI-RESET-AXIOM-COUNTER"
                                 "OWLAPI-REMOVE-PREFIX"
                                 "OWLAPI-REMOVE-AXIOMS"
                                 "OWLAPI-REMOVE-AXIOM"
                                 "OWLAPI-RELOAD-LOADED-ONTOLOGIES"
                                 "OWLAPI-REGISTER-REFERENCED-ENTITIES"
                                 "OWLAPI-REGISTER-OBJECT"
                                 "OWLAPI-REGISTER-LAST-ANSWER"
                                 "OWLAPI-REGISTER-DECLARED-ENTITIES"
                                 "OWLAPI-REALIZE"
                                 "OWLAPI-READ-XML-ONTOLOGY-FILE"
                                 "OWLAPI-READ-XML-ONTOLOGY-DOCUMENT"
                                 "OWLAPI-READ-ONTOLOGY"
                                 "OWLAPI-READ-FUNCTIONAL-ONTOLOGY-FILE"
                                 "OWLAPI-READ-FUNCTIONAL-ONTOLOGY-DOCUMENT"
                                 "OWLAPI-QUIET-SEQUENCE"
                                 "OWLAPI-PARSE-NATIVE"
                                 "OWLAPI-PARSE"
                                 "OWLAPI-NEXT-AXIOM-USE-ID"
                                 "OWLAPI-NEW-REASONER1"
                                 "OWLAPI-NEW-REASONER"
                                 "OWLAPI-NEW-ONTOLOGY"
                                 "OWLAPI-MERGE-ONTOLOGIES"
                                 "OWLAPI-MANUALLY-APPLY-CHANGES"
                                 "OWLAPI-LOAD-ONTOLOGY"
                                 "OWLAPI-LOAD-ONTOLOGIES"
                                 "OWLAPI-LOAD-AXIOMS"
                                 "OWLAPI-LOAD-AXIOM"
                                 "OWLAPI-KEEP-ANNOTATIONS"
                                 "OWLAPI-IS-TRANSITIVE"
                                 "OWLAPI-IS-SYMMETRIC"
                                 "OWLAPI-IS-SUB-CLASS-OF"
                                 "OWLAPI-IS-SATISFIABLE"
                                 "OWLAPI-IS-SAME-INDIVIDUAL"
                                 "OWLAPI-IS-REFLEXIVE"
                                 "OWLAPI-IS-REALISED"
                                 "OWLAPI-IS-IRREFLEXIVE"
                                 "OWLAPI-IS-INVERSE-FUNCTIONAL"
                                 "OWLAPI-IS-FUNCTIONAL"
                                 "OWLAPI-IS-EQUIVALENT-CLASS"
                                 "OWLAPI-IS-ENTAILED"
                                 "OWLAPI-IS-DIFFERENT-INDIVIDUAL"
                                 "OWLAPI-IS-DEFINED-OBJECT-PROPERTY"
                                 "OWLAPI-IS-DEFINED-INDIVIDUAL"
                                 "OWLAPI-IS-DEFINED-DATA-PROPERTY"
                                 "OWLAPI-IS-DEFINED-CLASS"
                                 "OWLAPI-IS-CONSISTENT"
                                 "OWLAPI-IS-CLASSIFIED"
                                 "OWLAPI-IS-CLASS"
                                 "OWLAPI-IS-ASYMMETRIC"
                                 "OWLAPI-INIT"
                                 "OWLAPI-IGNORE-DECLARATIONS"
                                 "OWLAPI-IGNORE-ANNOTATIONS"
                                 "OWLAPI-IDToAxiom"
                                 "OWLAPI-ID-TO-AXIOM"
                                 "OWLAPI-HAS-TYPE"
                                 "OWLAPI-HAS-OBJECT-PROPERTY-RELATIONSHIP"
                                 "OWLAPI-HAS-DATA-PROPERTY-RELATIONSHIP"
                                 "OWLAPI-GET-TYPES"
                                 "OWLAPI-GET-SUPER-PROPERTIES"
                                 "OWLAPI-GET-SUPER-CLASSES"
                                 "OWLAPI-GET-SUB-PROPERTIES"
                                 "OWLAPI-GET-SUB-CLASSES"
                                 "OWLAPI-GET-SAME-INDIVIDUALS"
                                 "OWLAPI-GET-RELATED-VALUES"
                                 "OWLAPI-GET-RELATED-INDIVIDUALS"
                                 "OWLAPI-GET-REASONERS"
                                 "OWLAPI-GET-RANGES"
                                 "OWLAPI-GET-PREFIXES"
                                 "OWLAPI-GET-OWL-TRANSITIVE-OBJECT-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-SYMMETRIC-OBJECT-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-SUB-CLASS-AXIOM"
                                 "OWLAPI-GET-OWL-SUB-ANNOTATION-PROPERTY-OF-AXIOM"
                                 "OWLAPI-GET-OWL-SUB-ANNOTATION-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-SAME-INDIVIDUALS-AXIOM"
                                 "OWLAPI-GET-OWL-REFLEXIVE-OBJECT-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-REALLY-IMPLICIT-DECLARATION-AXIOM"
                                 "OWLAPI-GET-OWL-PREFIX-DECLARATION-AXIOM"
                                 "OWLAPI-GET-OWL-ONTOLOGY-VERSION-DECLARATION-AXIOM"
                                 "OWLAPI-GET-OWL-ONTOLOGY-ANNOTATION-AXIOM"
                                 "OWLAPI-GET-OWL-OBJECT-SUB-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-OBJECT-PROPERTY-RANGE-AXIOM"
                                 "OWLAPI-GET-OWL-OBJECT-PROPERTY-DOMAIN-AXIOM"
                                 "OWLAPI-GET-OWL-OBJECT-PROPERTY-CHAIN-SUB-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-OBJECT-PROPERTY-ASSERTION-AXIOM"
                                 "OWLAPI-GET-OWL-NEGATIVE-OBJECT-PROPERTY-ASSERTION-AXIOM"
                                 "OWLAPI-GET-OWL-NEGATIVE-DATA-PROPERTY-ASSERTION-AXIOM"
                                 "OWLAPI-GET-OWL-IRREFLEXIVE-OBJECT-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-INVERSE-OBJECT-PROPERTIES-AXIOM"
                                 "OWLAPI-GET-OWL-INVERSE-FUNCTIONAL-OBJECT-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-IMPORTS-DECLARATION-AXIOM"
                                 "OWLAPI-GET-OWL-IMPLICIT-DECLARATION-AXIOM"
                                 "OWLAPI-GET-OWL-HAS-KEY-AXIOM"
                                 "OWLAPI-GET-OWL-FUNCTIONAL-OBJECT-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-FUNCTIONAL-DATA-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-EQUIVALENT-OBJECT-PROPERTIES-AXIOM"
                                 "OWLAPI-GET-OWL-EQUIVALENT-DATA-PROPERTIES-AXIOM"
                                 "OWLAPI-GET-OWL-EQUIVALENT-CLASSES-AXIOM"
                                 "OWLAPI-GET-OWL-ENTITY-ANNOTATION-AXIOM"
                                 "OWLAPI-GET-OWL-DISJOINT-UNION-AXIOM"
                                 "OWLAPI-GET-OWL-DISJOINT-OBJECT-PROPERTIES-AXIOM"
                                 "OWLAPI-GET-OWL-DISJOINT-DATA-PROPERTIES-AXIOM"
                                 "OWLAPI-GET-OWL-DISJOINT-CLASSES-AXIOM"
                                 "OWLAPI-GET-OWL-DIFFERENT-INDIVIDUALS-AXIOM"
                                 "OWLAPI-GET-OWL-DECLARATION-AXIOM"
                                 "OWLAPI-GET-OWL-DATATYPE-DEFINITION-AXIOM"
                                 "OWLAPI-GET-OWL-DATA-SUB-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-DATA-PROPERTY-RANGE-AXIOM"
                                 "OWLAPI-GET-OWL-DATA-PROPERTY-DOMAIN-AXIOM"
                                 "OWLAPI-GET-OWL-DATA-PROPERTY-ASSERTION-AXIOM"
                                 "OWLAPI-GET-OWL-CLASS-ASSERTION-AXIOM"
                                 "OWLAPI-GET-OWL-AXIOM-ANNOTATION-AXIOM"
                                 "OWLAPI-GET-OWL-ASYMMETRIC-OBJECT-PROPERTY-AXIOM"
                                 "OWLAPI-GET-OWL-ANNOTATION-PROPERTY-RANGE-AXIOM"
                                 "OWLAPI-GET-OWL-ANNOTATION-PROPERTY-DOMAIN-AXIOM"
                                 "OWLAPI-GET-OWL-ANNOTATION-ASSERTION-AXIOM"
                                 "OWLAPI-GET-ONTOLOGIES"
                                 "OWLAPI-GET-OBJECT-PROPERTY-VALUES"
                                 "OWLAPI-GET-OBJECT-PROPERTY-RELATIONSHIPS"
                                 "OWLAPI-GET-LOADED-ONTOLOGIES"
                                 "OWLAPI-GET-INVERSE-PROPERTIES"
                                 "OWLAPI-GET-INSTANCES"
                                 "OWLAPI-GET-INDIVIDUALS"
                                 "OWLAPI-GET-INCONSISTENT-CLASSES"
                                 "OWLAPI-GET-EQUIVALENT-PROPERTIES"
                                 "OWLAPI-GET-EQUIVALENT-CLASSES"
                                 "OWLAPI-GET-DOMAINS"
                                 "OWLAPI-GET-DISJOINT-OBJECT-PROPERTIES"
                                 "OWLAPI-GET-DISJOINT-DATA-PROPERTIES"
                                 "OWLAPI-GET-DISJOINT-CLASSES"
                                 "OWLAPI-GET-DIFFERENT-INDIVIDUALS"
                                 "OWLAPI-GET-DESCENDANT-PROPERTIES"
                                 "OWLAPI-GET-DESCENDANT-CLASSES"
                                 "OWLAPI-GET-DATA-PROPERTY-VALUES"
                                 "OWLAPI-GET-DATA-PROPERTY-RELATIONSHIPS"
                                 "OWLAPI-GET-CURRENT-REASONER"
                                 "OWLAPI-GET-CHANGES"
                                 "OWLAPI-GET-AXIOMS-PER-ONTOLOGY"
                                 "OWLAPI-GET-AXIOMS-OF-TYPE-IN"
                                 "OWLAPI-GET-AXIOMS-OF-TYPE"
                                 "OWLAPI-GET-AXIOMS-IN"
                                 "OWLAPI-GET-AXIOMS"
                                 "OWLAPI-GET-AXIOM-COUNTER"
                                 "OWLAPI-GET-AUTO-ONTOLOGY"
                                 "OWLAPI-GET-AUTO-DECLARE-DATA-PROPERTIES"
                                 "OWLAPI-GET-ANNOTATION-AXIOMS-FOR-AXIOM"
                                 "OWLAPI-GET-ANCESTOR-PROPERTIES"
                                 "OWLAPI-GET-ANCESTOR-CLASSES"
                                 "OWLAPI-GET-ALL-ONTOLOGIES"
                                 "OWLAPI-FIND-OBJECT-FROM-ID"
                                 "OWLAPI-FIND-ID-FROM-OBJECT"
                                 "OWLAPI-EXPORT-REASONER"
                                 "OWLAPI-EXPORT-ONTOLOGY"
                                 "OWLAPI-ENABLE-TRANSIENT-AXIOM-MODE"
                                 "OWLAPI-ENABLE-SIMPLIFIED-PROTOCOL"
                                 "OWLAPI-ENABLE-MEMORY-SAVING-MODE"
                                 "OWLAPI-ENABLE-LOOKUP-MODE"
                                 "OWLAPI-ENABLE-INCREMENTAL-UPDATES"
                                 "OWLAPI-DONT-REGISTER-REFERENCED-ENTITIES"
                                 "OWLAPI-DONT-REGISTER-DECLARED-ENTITIES"
                                 "OWLAPI-DISPOSE-REASONER"
                                 "OWLAPI-DISPOSE-ONTOLOGY"
                                 "OWLAPI-DISPOSE-ONTOLOGIES"
                                 "OWLAPI-DISPOSE-AXIOMS"
                                 "OWLAPI-DISPOSE-AXIOM"
                                 "OWLAPI-DISPOSE"
                                 "OWLAPI-DISABLE-TRANSIENT-AXIOM-MODE"
                                 "OWLAPI-DISABLE-SIMPLIFIED-PROTOCOL"
                                 "OWLAPI-DISABLE-MEMORY-SAVING-MODE"
                                 "OWLAPI-DISABLE-LOOKUP-MODE"
                                 "OWLAPI-DISABLE-INCREMENTAL-UPDATES"
                                 "OWLAPI-DISABLE-AUTO-MODE"
                                 "OWLAPI-DESCRIBE-REASONERS"
                                 "OWLAPI-DESCRIBE-REASONER"
                                 "OWLAPI-DESCRIBE-ONTOLOGY"
                                 "OWLAPI-DESCRIBE-ONTOLOGIES"
                                 "OWLAPI-CONTAINS"
                                 "OWLAPI-CONSIDER-DECLARATIONS"
                                 "OWLAPI-CLEAR-REGISTRY"
                                 "OWLAPI-CLEAR-ONTOLOGIES"
                                 "OWLAPI-CLEAR-CHANGES"
                                 "OWLAPI-CLASSIFY"
                                 "OWLAPI-BATCH-SYNCHRONIZE"
                                 "OWLAPI-AxiomToID"
                                 "OWLAPI-AxiomLoaded?"
                                 "OWLAPI-AddAxioms"
                                 "OWLAPI-AddAxiom"
                                 "OWLAPI-AXIOM-TO-ID"
                                 "OWLAPI-AXIOM-LOADED?"
                                 "OWLAPI-AUTO-REMOVE-AXIOMS-FROM"
                                 "OWLAPI-AUTO-BATCH-REMOVE-AXIOMS-FROM"
                                 "OWLAPI-AUTO-BATCH-ADD-AXIOMS-TO"
                                 "OWLAPI-AUTO-APPLY-CHANGES"
                                 "OWLAPI-AUTO-ADD-AXIOMS-TO"
                                 "OWLAPI-APPLY-CHANGES"
                                 "OWLAPI-ANSWER-SEQUENCE"
                                 "OWLAPI-ADD-PREFIX"
                                 "OWLAPI-ADD-AXIOMS"
                                 "OWLAPI-ADD-AXIOM"
                                 "OWLAPI-ABORT"
                                 "OWL-READ-FILE"
                                 "OWL-READ-DOCUMENT"
                                 "ORIGINAL-RULE-CONSEQUENCE"
                                 "ORIGINAL-RULE-ANTECEDENCE"
                                 "ORIGINAL-QUERY-HEAD"
                                 "ORIGINAL-QUERY-BODY"
                                 "OPTIMIZER-USE-CARDINALITY-HEURISTICS"
                                 "OPTIMIZER-SET-TIME-BOUND"
                                 "OPTIMIZER-SET-NO-OF-PLANS-UPPER-BOUND"
                                 "OPTIMIZER-GET-TIME-BOUND"
                                 "OPTIMIZER-GET-NO-OF-PLANS-UPPER-BOUND"
                                 "OPTIMIZER-ENSURE-LATE-LAMBDA-EVALUATION"
                                 "OPTIMIZER-DONT-USE-CARDINALITY-HEURISTICS"
                                 "OPTIMIZER-DONT-ENSURE-LATE-LAMBDA-EVALUATION"
                                 "OPEN-TRIPLE-STORE"
                                 "NODE-LABEL1"
                                 "NODE-LABEL"
                                 "NODE-DESCRIPTION1"
                                 "NODE-DESCRIPTION"
                                 "NEXT-TUPLE-AVAILABLE-P"
                                 "NEXT-SET-OF-RULE-CONSEQUENCES-AVAILABLE-P"
                                 "MSC-K"
                                 "MOVE-RULES"
                                 "MOST-SPECIFIC-INSTANTIATORS"
                                 "MIRROR"
                                 "MATERIALIZE-INFERENCES"
                                 "MAKE-QUERY-FROM-ABOX"
                                 "MAKE-PLUGIN-FROM-FASL-FILE"
                                 "MAKE-FORWARD-RULE-FROM-ABOXES"
                                 "MAKE-BACKWARD-RULE-FROM-ABOXES"
                                 "MAKE-ABDUCTION-RULE-FROM-ABOXES"
                                 "LOGGING-ON"
                                 "LOGGING-OFF"
                                 "LOAD-RACER-PLUGINS"
                                 "LOAD-RACER-PLUGIN"
                                 "LOAD-RACER-PATCHES"
                                 "LOAD-RACER-PATCH"
                                 "LCS-UNFOLD"
                                 "LCS"
                                 "KEEP-DEFINED-QUERY-ATOMS"
                                 "KB-ONTOLOGIES"
                                 "IRREFLEXIVE?"
                                 "IRREFLEXIVE-P"
                                 "IRREFLEXIVE"
                                 "INVERSE-OF-ROLE"
                                 "INVERSE-FEATURE-P"
                                 "INVERSE"
                                 "INTERNAL-INDIVIDUALS-RELATED-P"
                                 "INSTANTIATORS"
                                 "INSTANCE"
                                 "INSTALLED-PLUGINS"
                                 "INSTALLED-PATCHES"
                                 "INIT-TBOX"
                                 "INIT-SUBSCRIPTIONS-1"
                                 "INIT-SUBSCRIPTIONS"
                                 "INIT-PUBLICATIONS-1"
                                 "INIT-PUBLICATIONS"
                                 "INIT-ABOX"
                                 "INDIVIDUALS-RELATED?"
                                 "INDIVIDUALS-RELATED-P"
                                 "INDIVIDUALS-NOT-EQUAL?"
                                 "INDIVIDUALS-NOT-EQUAL-P"
                                 "INDIVIDUALS-EQUAL?"
                                 "INDIVIDUALS-EQUAL-P"
                                 "INDIVIDUAL?"
                                 "INDIVIDUAL-TYPES"
                                 "INDIVIDUAL-TOLD-DATATYPE-FILLERS"
                                 "INDIVIDUAL-TOLD-ATTRIBUTE-VALUE"
                                 "INDIVIDUAL-SYNONYMS"
                                 "INDIVIDUAL-P"
                                 "INDIVIDUAL-INSTANCE?"
                                 "INDIVIDUAL-INSTANCE-P"
                                 "INDIVIDUAL-FILLERS"
                                 "INDIVIDUAL-FILLED-ROLES"
                                 "INDIVIDUAL-DIRECT-TYPES"
                                 "INDIVIDUAL-ATTRIBUTE-FILLERS"
                                 "INDIVIDUAL-ANTONYMS"
                                 "INDEX-ALL-TRIPLES"
                                 "INCLUDE-PERMUTATIONS"
                                 "INCLUDE-KB"
                                 "INACCURATE-RULES"
                                 "INACCURATE-QUERIES"
                                 "IN-UNSAFE-MODE?"
                                 "IN-TBOX"
                                 "IN-RCC-BOX"
                                 "IN-MIRROR-DATA-BOX"
                                 "IN-KNOWLEDGE-BASE"
                                 "IN-DATA-BOX"
                                 "IN-ABOX"
                                 "IMPLIES-ROLE"
                                 "IMPLIES"
                                 "GET-TBOX-VERSION"
                                 "GET-TBOX-SIGNATURE"
                                 "GET-TBOX-LANGUAGE"
                                 "GET-SUBSTRATE-TYPE"
                                 "GET-SUBSTRATE-NODES"
                                 "GET-SUBSTRATE-EDGES"
                                 "GET-SERVER-TIMEOUT"
                                 "GET-ROLE-HIERARCHY"
                                 "GET-ROLE-DATATYPE"
                                 "GET-RACER-VERSION"
                                 "GET-PROXY-SERVER"
                                 "GET-PROCESS-POOL-SIZE"
                                 "GET-PREFIXES"
                                 "GET-OBJECT-BOTTOM-ROLE"
                                 "GET-NUMBER-OF-EXPLANATIONS"
                                 "GET-NRQL-VERSION"
                                 "GET-NODES-IN-QBOX-FOR-ABOX"
                                 "GET-NEXT-TUPLE"
                                 "GET-NEXT-SET-OF-RULE-CONSEQUENCES"
                                 "GET-NEXT-N-REMAINING-TUPLES"
                                 "GET-NEXT-N-REMAINING-SETS-OF-RULE-CONSEQUENCES"
                                 "GET-NEW-IND-PREFIX"
                                 "GET-NEW-IND-COUNTER"
                                 "GET-NAMESPACE-PREFIXES"
                                 "GET-NAMESPACE-PREFIX"
                                 "GET-MINIMUM"
                                 "GET-META-CONSTRAINT"
                                 "GET-MAXIMUM-SIZE-OF-PROCESS-POOL"
                                 "GET-MAXIMUM"
                                 "GET-MAX-NO-OF-TUPLES-BOUND"
                                 "GET-KB-SIGNATURE"
                                 "GET-INITIAL-SIZE-OF-PROCESS-POOL"
                                 "GET-INDIVIDUAL-SUCCESSORS"
                                 "GET-INDIVIDUAL-PMODEL"
                                 "GET-INDIVIDUAL-DATATYPE-FILLERS"
                                 "GET-INDIVIDUAL-ANNOTATION-FILLERS"
                                 "GET-INDIVIDUAL-ANNOTATION-DATATYPE-FILLERS"
                                 "GET-EXPLANATIONS"
                                 "GET-EDGE-LABEL-FOR-NON-EXISTENT-EDGES"
                                 "GET-DATA-NODE-LABEL"
                                 "GET-DATA-NODE-DESCRIPTION"
                                 "GET-DATA-EDGE-LABEL"
                                 "GET-DATA-EDGE-DESCRIPTION"
                                 "GET-DATA-BOTTOM-ROLE"
                                 "GET-DAG-OF-QBOX-FOR-ABOX"
                                 "GET-CURRENT-TUPLE"
                                 "GET-CURRENT-SET-OF-RULE-CONSEQUENCES"
                                 "GET-CONCEPT-PROPERTIES"
                                 "GET-CONCEPT-PMODEL"
                                 "GET-CONCEPT-NEGATED-DEFINITION-1"
                                 "GET-CONCEPT-NEGATED-DEFINITION"
                                 "GET-CONCEPT-DEFINITION-1"
                                 "GET-CONCEPT-DEFINITION"
                                 "GET-CHOSEN-SETS-OF-RULE-CONSEQUENCES"
                                 "GET-BUILD-VERSION"
                                 "GET-ANSWER-SIZE"
                                 "GET-ANSWER"
                                 "GET-ALL-VALUES"
                                 "GET-ALL-SERVER-VALUES"
                                 "GET-ALL-SERVER-FUNCTIONS"
                                 "GET-ALL-REMAINING-TUPLES"
                                 "GET-ALL-REMAINING-SETS-OF-RULE-CONSEQUENCES"
                                 "GET-ALL-FUNCTIONS"
                                 "GET-ALL-ANSWERS"
                                 "GET-AGRAPH-VERSION"
                                 "GET-ABOX-VERSION"
                                 "GET-ABOX-SIGNATURE"
                                 "GET-ABOX-LANGUAGE"
                                 "GET-ABOX-GRAPH"
                                 "FUNCTIONAL"
                                 "FULL-RESET"
                                 "FORGET-TBOX"
                                 "FORGET-STATEMENT"
                                 "FORGET-SAME-INDIVIDUAL-AS-ASSERTION"
                                 "FORGET-ROLE-AXIOMS"
                                 "FORGET-ROLE-ASSERTION"
                                 "FORGET-NEGATIVE-DATATYPE-ROLE-FILLER"
                                 "FORGET-NEGATED-ROLE-ASSERTION"
                                 "FORGET-INDIVIDUAL"
                                 "FORGET-DISJOINTNESS-AXIOM-STATEMENT"
                                 "FORGET-DISJOINTNESS-AXIOM"
                                 "FORGET-DIFFERENT-FROM-ASSERTION"
                                 "FORGET-DATATYPE-ROLE-FILLER"
                                 "FORGET-CONSTRAINT"
                                 "FORGET-CONSTRAINED-ASSERTION"
                                 "FORGET-CONCEPT-AXIOM"
                                 "FORGET-CONCEPT-ASSERTION"
                                 "FORGET-ANNOTATION-CONCEPT-ASSERTION"
                                 "FORGET-ALL-DIFFERENT-ASSERTION"
                                 "FORGET-ABOX"
                                 "FORGET"
                                 "FIRERULE1"
                                 "FIRERULE-UNDER-PREMISE1"
                                 "FIRERULE-UNDER-PREMISE"
                                 "FIRERULE"
                                 "FIND-TBOX"
                                 "FIND-ABOX"
                                 "FEATURE?"
                                 "FEATURE-P"
                                 "FCALL"
                                 "EXPENSIVE-RULES"
                                 "EXPENSIVE-RULE-P"
                                 "EXPENSIVE-QUERY-P"
                                 "EXPENSIVE-QUERIES"
                                 "EXIT-SERVER"
                                 "EXECUTE-RULE"
                                 "EXECUTE-QUERY"
                                 "EXECUTE-OR-REEXECUTE-RULE"
                                 "EXECUTE-OR-REEXECUTE-QUERY"
                                 "EXECUTE-OR-REEXECUTE-ALL-RULES"
                                 "EXECUTE-OR-REEXECUTE-ALL-QUERIES"
                                 "EXECUTE-APPLICABLE-RULES"
                                 "EXECUTE-ALL-RULES"
                                 "EXECUTE-ALL-QUERIES"
                                 "EXCLUDE-PERMUTATIONS"
                                 "EVALUATE1"
                                 "EVALUATE"
                                 "EQUIVALENT"
                                 "ENSURE-TBOX-SIGNATURE"
                                 "ENSURE-SUBSUMPTION-BASED-QUERY-ANSWERING"
                                 "ENSURE-SMALL-TBOXES"
                                 "ENSURE-ABOX-SIGNATURE"
                                 "ENABLE-VERY-SMART-ABOX-MIRRORING"
                                 "ENABLE-TWO-PHASE-QUERY-PROCESSING-MODE"
                                 "ENABLE-TOLD-INFORMATION-QUERYING"
                                 "ENABLE-SMART-ABOX-MIRRORING"
                                 "ENABLE-RCC-SUBSTRATE-MIRRORING"
                                 "ENABLE-QUERY-REPOSITORY"
                                 "ENABLE-QUERY-REALIZATION"
                                 "ENABLE-QUERY-OPTIMIZATION"
                                 "ENABLE-PHASE-TWO-STARTS-WARNING-TOKENS"
                                 "ENABLE-OPTIMIZED-QUERY-PROCESSING"
                                 "ENABLE-NRQL-WARNINGS"
                                 "ENABLE-LAZY-UNFOLDING-OF-DEFINED-QUERIES"
                                 "ENABLE-LAZY-TUPLE-COMPUTATION"
                                 "ENABLE-KB-HAS-CHANGED-WARNING-TOKENS"
                                 "ENABLE-EAGER-TUPLE-COMPUTATION"
                                 "ENABLE-DEFINED-QUERIES"
                                 "ENABLE-DATA-SUBSTRATE-MIRRORING"
                                 "ENABLE-ALISP-COMPATIBILITY-MODE"
                                 "ENABLE-ABOX-MIRRORING"
                                 "ENABLE-ABDUCTION"
                                 "EDGE-LABEL1"
                                 "EDGE-LABEL"
                                 "EDGE-DESCRIPTION1"
                                 "EDGE-DESCRIPTION"
                                 "DONT-USE-INJECTIVE-VARIABLES-BY-DEFAULT"
                                 "DONT-USE-INDIVIDUAL-SYNONYM-EQUIVALENCE-CLASSES"
                                 "DONT-REPORT-INCONSISTENT-QUERIES-AND-RULES"
                                 "DONT-PREFER-DEFINED-QUERIES"
                                 "DONT-KEEP-DEFINED-QUERY-ATOMS"
                                 "DONT-CHECK-ABOX-CONSISTENCY-BEFORE-QUERYING"
                                 "DONT-ALLOW-OVERLOADED-DEFINITIONS"
                                 "DONT-ADD-RULE-CONSEQUENCES-AUTOMATICALLY"
                                 "DONT-ADD-ROLE-ASSERTIONS-FOR-DATATYPE-PROPERTIES"
                                 "DONT-ADD-MISSING-TOP-CONJUNCTS"
                                 "DOMAIN"
                                 "DISJOINT"
                                 "DISABLE-TWO-PHASE-QUERY-PROCESSING-MODE"
                                 "DISABLE-TOLD-INFORMATION-QUERYING"
                                 "DISABLE-RCC-SUBSTRATE-MIRRORING"
                                 "DISABLE-QUERY-REPOSITORY"
                                 "DISABLE-QUERY-REALIZATION"
                                 "DISABLE-QUERY-OPTIMIZATION"
                                 "DISABLE-PHASE-TWO-STARTS-WARNING-TOKENS"
                                 "DISABLE-NRQL-WARNINGS"
                                 "DISABLE-LAZY-UNFOLDING-OF-DEFINED-QUERIES"
                                 "DISABLE-KB-HAS-CHANGED-WARNING-TOKENS"
                                 "DISABLE-DEFINED-QUERIES"
                                 "DISABLE-DATA-SUBSTRATE-MIRRORING"
                                 "DISABLE-ALISP-COMPATIBILITY-MODE"
                                 "DISABLE-ABOX-MIRRORING"
                                 "DISABLE-ABDUCTION"
                                 "DIRECT-PREDECESSORS"
                                 "DIG-READ-FILE"
                                 "DIG-READ-DOCUMENT"
                                 "DIFFERENT-FROM"
                                 "DESCRIPTION-IMPLIES?"
                                 "DESCRIPTION-IMPLIES-P"
                                 "DESCRIBE-TBOX"
                                 "DESCRIBE-SUBSTRATE"
                                 "DESCRIBE-RULE-STATUS"
                                 "DESCRIBE-RULE"
                                 "DESCRIBE-ROLE"
                                 "DESCRIBE-QUERY-STATUS"
                                 "DESCRIBE-QUERY-PROCESSING-MODE"
                                 "DESCRIBE-QUERY"
                                 "DESCRIBE-INDIVIDUAL1"
                                 "DESCRIBE-INDIVIDUAL"
                                 "DESCRIBE-DEFINITION"
                                 "DESCRIBE-CURRENT-SUBSTRATE"
                                 "DESCRIBE-CONCEPT"
                                 "DESCRIBE-ALL-SUBSTRATES"
                                 "DESCRIBE-ALL-RULES"
                                 "DESCRIBE-ALL-QUERIES"
                                 "DESCRIBE-ALL-NODES"
                                 "DESCRIBE-ALL-EDGES"
                                 "DESCRIBE-ALL-DEFINITIONS"
                                 "DESCRIBE-ABOX"
                                 "DELETE-TBOX"
                                 "DELETE-RULE"
                                 "DELETE-RCC-SYNONYMS"
                                 "DELETE-QUERY"
                                 "DELETE-PREFIX-MAPPINGS"
                                 "DELETE-DATA-NODE"
                                 "DELETE-DATA-EDGE"
                                 "DELETE-ALL-TBOXES"
                                 "DELETE-ALL-SUBSTRATES"
                                 "DELETE-ALL-RULES"
                                 "DELETE-ALL-QUERIES"
                                 "DELETE-ALL-DEFINITIONS"
                                 "DELETE-ALL-ABOXES"
                                 "DELETE-ABOX"
                                 "DEL-RCC-NODE1"
                                 "DEL-RCC-NODE"
                                 "DEL-RCC-EDGE1"
                                 "DEL-RCC-EDGE"
                                 "DEL-DOC-ENTRY1"
                                 "DEL-DOC-ENTRY"
                                 "DEL-DATA-NODE1"
                                 "DEL-DATA-NODE"
                                 "DEL-DATA-EDGE1"
                                 "DEL-DATA-EDGE"
                                 "DEFQUERY"
                                 "DEFPAR1"
                                 "DEFPAR"
                                 "DEFINE1"
                                 "DEFINE-TBOX"
                                 "DEFINE-RULE"
                                 "DEFINE-QUERY"
                                 "DEFINE-PRIMITIVE-ROLE"
                                 "DEFINE-PRIMITIVE-CONCEPT"
                                 "DEFINE-PRIMITIVE-ATTRIBUTE"
                                 "DEFINE-PREFIX"
                                 "DEFINE-INDIVIDUAL"
                                 "DEFINE-EVENT-RULE"
                                 "DEFINE-EVENT-ASSERTION"
                                 "DEFINE-DISTINCT-INDIVIDUAL"
                                 "DEFINE-DISJOINT-PRIMITIVE-CONCEPT"
                                 "DEFINE-DATATYPE-PROPERTY"
                                 "DEFINE-CONCRETE-DOMAIN-ATTRIBUTE"
                                 "DEFINE-CONCEPT"
                                 "DEFINE-AND-PREPARE-QUERY"
                                 "DEFINE-AND-EXECUTE-QUERY"
                                 "DEFINE-ABOX"
                                 "DEFINE"
                                 "DEFCON1"
                                 "DEFCON"
                                 "DEF-AND-PREP-QUERY"
                                 "DEF-AND-EXEC-QUERY"
                                 "DECLARE-DISJOINT"
                                 "DECLARE-CURRENT-KNOWLEDGE-BASES-AS-PERSISTENT"
                                 "DEACTIVATE-DEFINED-QUERY"
                                 "DATATYPE-ROLE-RANGE"
                                 "DATATYPE-ROLE-HAS-RANGE"
                                 "DATATYPE-ROLE-FILLER"
                                 "DATA-NODE1"
                                 "DATA-NODE"
                                 "DATA-EDGE1"
                                 "DATA-EDGE"
                                 "CURRENT-TBOX"
                                 "CURRENT-ABOX"
                                 "CREATE-TRIPLE-STORE"
                                 "CREATE-TBOX-INTERNAL-MARKER-CONCEPT"
                                 "CREATE-TBOX-CLONE"
                                 "CREATE-SUBGRAPH-ABOXES"
                                 "CREATE-RCC-NODE"
                                 "CREATE-RCC-EDGE"
                                 "CREATE-DATA-NODE"
                                 "CREATE-DATA-EDGE"
                                 "CREATE-ABOX-CLONE"
                                 "COPY-RULES"
                                 "CONVERT-EVENT-SPECS"
                                 "CONSTRAINTS"
                                 "CONSTRAINT-ENTAILED?"
                                 "CONSTRAINT-ENTAILED-P"
                                 "CONSTRAINED"
                                 "CONCEPT?"
                                 "CONCEPT-SYNONYMS"
                                 "CONCEPT-SUBSUMES?"
                                 "CONCEPT-SUBSUMES-P"
                                 "CONCEPT-SATISFIABLE?"
                                 "CONCEPT-SATISFIABLE-P"
                                 "CONCEPT-PARENTS"
                                 "CONCEPT-P"
                                 "CONCEPT-IS-PRIMITIVE?"
                                 "CONCEPT-IS-PRIMITIVE-P"
                                 "CONCEPT-INSTANCES"
                                 "CONCEPT-EQUIVALENT?"
                                 "CONCEPT-EQUIVALENT-P"
                                 "CONCEPT-DISJOINT?"
                                 "CONCEPT-DISJOINT-P"
                                 "CONCEPT-DESCENDANTS"
                                 "CONCEPT-CHILDREN"
                                 "CONCEPT-ANCESTORS"
                                 "COMPUTE-SUBGRAPH-ABOXES"
                                 "COMPUTE-INDEX-FOR-INSTANCE-RETRIEVAL"
                                 "COMPUTE-IMPLICIT-ROLE-FILLERS"
                                 "COMPUTE-ALL-IMPLICIT-ROLE-FILLERS"
                                 "COMPUTE-ABOX-DIFFERENCE2"
                                 "COMPUTE-ABOX-DIFFERENCE1"
                                 "COMPUTE-ABOX-DIFFERENCE-ALTERNATIVE"
                                 "COMPUTE-ABOX-DIFFERENCE"
                                 "CLOSE-TRIPLE-STORE"
                                 "CLONE-TBOX"
                                 "CLONE-ABOX"
                                 "CLEAR-DEFAULT-TBOX"
                                 "CLEAR-ALL-DOCUMENTATION"
                                 "CLASSIFY-TBOX"
                                 "CLASSIFY-QUERY"
                                 "CHOOSE-CURRENT-SET-OF-RULE-CONSEQUENCES"
                                 "CHECK-TBOX-COHERENCE"
                                 "CHECK-SUBSCRIPTIONS"
                                 "CHECK-ONTOLOGY"
                                 "CHECK-NRQL-SUBSCRIPTIONS"
                                 "CHECK-FOR-UPDATES"
                                 "CHECK-CONCEPT-COHERENCE"
                                 "CHECK-ABOX-CONSISTENCY-BEFORE-QUERYING"
                                 "CHECK-ABOX-COHERENCE"
                                 "CHEAP-RULES"
                                 "CHEAP-RULE-P"
                                 "CHEAP-QUERY-P"
                                 "CHEAP-QUERIES"
                                 "CD-OBJECT?"
                                 "CD-OBJECT-P"
                                 "CD-ATTRIBUTE?"
                                 "CD-ATTRIBUTE-P"
                                 "ATTRIBUTE-TYPE"
                                 "ATTRIBUTE-FILLER"
                                 "ATTRIBUTE-DOMAIN-1"
                                 "ATTRIBUTE-DOMAIN"
                                 "ATOMIC-ROLE-SYNONYMS"
                                 "ATOMIC-ROLE-RANGE"
                                 "ATOMIC-ROLE-PARENTS"
                                 "ATOMIC-ROLE-INVERSE"
                                 "ATOMIC-ROLE-DOMAIN"
                                 "ATOMIC-ROLE-DESCENDANTS"
                                 "ATOMIC-ROLE-CHILDREN"
                                 "ATOMIC-ROLE-ANCESTORS"
                                 "ATOMIC-CONCEPT-SYNONYMS"
                                 "ATOMIC-CONCEPT-PARENTS"
                                 "ATOMIC-CONCEPT-DESCENDANTS"
                                 "ATOMIC-CONCEPT-CHILDREN"
                                 "ATOMIC-CONCEPT-ANCESTORS"
                                 "ASYMMETRIC?"
                                 "ASYMMETRIC-P"
                                 "ASYMMETRIC"
                                 "ASSOCIATED-TBOX"
                                 "ASSOCIATED-ABOXES"
                                 "APPLY-RULE-UNDER-PREMISE1"
                                 "APPLY-RULE-UNDER-PREMISE"
                                 "APPLY-RULE"
                                 "APPLY-ABOX-RULE1"
                                 "APPLY-ABOX-RULE-UNDER-PREMISE1"
                                 "APPLY-ABOX-RULE-UNDER-PREMISE"
                                 "APPLY-ABOX-RULE"
                                 "APPLICABLE-RULES"
                                 "ANSWER-TBOX-QUERY1"
                                 "ANSWER-TBOX-QUERY"
                                 "ANSWER-SEQUENCE"
                                 "ANSWER-QUERY1"
                                 "ANSWER-QUERY-UNDER-PREMISE1"
                                 "ANSWER-QUERY-UNDER-PREMISE"
                                 "ANSWER-QUERY"
                                 "ALLOW-OVERLOADED-DEFINITIONS"
                                 "ALL-TRANSITIVE-ROLES"
                                 "ALL-TBOXES"
                                 "ALL-SUBSTRATES"
                                 "ALL-SAME-AS-ASSERTIONS"
                                 "ALL-RULES"
                                 "ALL-ROLES"
                                 "ALL-ROLE-ASSERTIONS-FOR-INDIVIDUAL-IN-RANGE"
                                 "ALL-ROLE-ASSERTIONS-FOR-INDIVIDUAL-IN-DOMAIN"
                                 "ALL-ROLE-ASSERTIONS"
                                 "ALL-QUERIES"
                                 "ALL-INDIVIDUALS"
                                 "ALL-FEATURES"
                                 "ALL-EQUIVALENT-CONCEPTS"
                                 "ALL-DIFFERENT-FROM-ASSERTIONS"
                                 "ALL-DIFFERENT"
                                 "ALL-CONSTRAINTS"
                                 "ALL-CONCEPT-ASSERTIONS-FOR-INDIVIDUAL"
                                 "ALL-CONCEPT-ASSERTIONS"
                                 "ALL-ATTRIBUTES"
                                 "ALL-ATTRIBUTE-ASSERTIONS"
                                 "ALL-ATOMIC-CONCEPTS"
                                 "ALL-ANNOTATION-ROLE-ASSERTIONS"
                                 "ALL-ANNOTATION-CONCEPT-ASSERTIONS"
                                 "ALL-ABOXES"
                                 "ALC-CONCEPT-COHERENT"
                                 "ADD-SAME-INDIVIDUAL-AS-ASSERTION"
                                 "ADD-RULE-CONSEQUENCES-AUTOMATICALLY"
                                 "ADD-RULE-AXIOM"
                                 "ADD-ROLE-AXIOMS"
                                 "ADD-ROLE-AXIOM"
                                 "ADD-ROLE-ASSERTIONS-FOR-DATATYPE-PROPERTIES"
                                 "ADD-ROLE-ASSERTION"
                                 "ADD-PREFIX"
                                 "ADD-NEGATIVE-DATATYPE-ROLE-FILLER"
                                 "ADD-NEGATED-ROLE-ASSERTION"
                                 "ADD-MISSING-TOP-CONJUNCTS"
                                 "ADD-EXPLANATION-ASSERTIONS"
                                 "ADD-EVENT-RULE"
                                 "ADD-EVENT-ASSERTION"
                                 "ADD-DOC-PHRASE1"
                                 "ADD-DOC-PHRASE"
                                 "ADD-DOC-IMAGE-FILE1"
                                 "ADD-DOC-IMAGE-FILE"
                                 "ADD-DOC-IMAGE-DATA1"
                                 "ADD-DOC-IMAGE-DATA-FROM-FILE1"
                                 "ADD-DOC-IMAGE-DATA-FROM-FILE"
                                 "ADD-DOC-IMAGE-DATA"
                                 "ADD-DOC-ENTRY1"
                                 "ADD-DOC-ENTRY"
                                 "ADD-DISJOINTNESS-AXIOM"
                                 "ADD-DIFFERENT-FROM-ASSERTION"
                                 "ADD-DATATYPE-ROLE-FILLER"
                                 "ADD-DATATYPE-PROPERTY"
                                 "ADD-CONSTRAINT-ASSERTION"
                                 "ADD-CONCEPT-AXIOM"
                                 "ADD-CONCEPT-ASSERTION"
                                 "ADD-CHOSEN-SETS-OF-RULE-CONSEQUENCES"
                                 "ADD-ATTRIBUTE-ASSERTION"
                                 "ADD-ANNOTATION-ROLE-ASSERTION"
                                 "ADD-ANNOTATION-CONCEPT-ASSERTION"
                                 "ADD-ALL-DIFFERENT-ASSERTION"
                                 "ACTIVE-RULES"
                                 "ACTIVE-QUERIES"
                                 "ACTIVATE-DEFINED-QUERY"
                                 "ACCURATE-RULES"
                                 "ACCURATE-QUERIES"
                                 "ABOX-UNA-CONSISTENT?"
                                 "ABOX-UNA-CONSISTENT-P"
                                 "ABOX-REALIZED?"
                                 "ABOX-REALIZED-P"
                                 "ABOX-PREPARED?"
                                 "ABOX-PREPARED-P"
                                 "ABOX-ENTAILS-ABOX-P"
                                 "ABOX-CONSISTENT?"
                                 "ABOX-CONSISTENT-P"
                                 "ABOX-CONSISTENT-IF-ASSERTIONS-ADDED-P"
                                 "ABORT-RULE"
                                 "ABORT-QUERY"
                                 "ABORT-ALL-RULES"
                                 "ABORT-ALL-QUERIES")))

(dolist (entry 
         (append #-:dlmaps 
                 (mapcar #'(lambda (x) (list (intern x :keyword) nil nil)) 
			 +racer-functions+)

                 ;;; Reihenfolge wichtig!
                 (mapcar #'(lambda (x) (list (first x) nil nil)) *nrql-methods*)
                 (mapcar #'(lambda (x) (list (first x) nil nil)) *nrql-functions*)
                 (mapcar #'(lambda (x) (list (first x) nil t)) *nrql-macros*)
                  
                 ;;; KEINE Special-Forms hier eintragen!
                 ;;; verhindert werden muessen Aufrufe der Art
                 ;;; (apply #'member "abc" '("abc") :test 'break nil)
                 ;;; daher: geborgte CL-Funktionen, die Funktionsargumente akzeptieren, 
                 ;;; werden umhuellt, s. Z.B. member

		 `(

                   ;;;
                   ;;; Racer 
                   ;;; 
                   ;;; muessen ueberschrieben werden, sonst kann man per load
                   ;;; beliebige Lisp-Ausdruecke evaluieren! 
                   ;;; 
                   
                   (include-kb 1 nil 
                               ,(lambda (file)
                                  (let ((*package* *racer-user-package*))
                                    (process-racer-file file))))

                   (racer-read-file 1 nil 
                                    ,(lambda (file)
                                       (let ((*package* *racer-user-package*))
                                         (process-racer-file file))))

                   (racer-read-document 1 nil
                                        ,(lambda (file)
                                           (let ((*package* *racer-user-package*))
                                             (racer-read-document file))))

                   ;;;
                   ;;; neue Operationen
                   ;;;           
                   
                   (describe-ind 2 nil ; nil=kein Makro
                                 ,(lambda (ind abox) 
                                    (read-from-string
                                     (with-output-to-string (stream)
                                       (let ((*standard-output* stream))
                                         (describe-individual ind abox))))))

                   (attribute-fillers 2 nil
                                      ,(lambda (ind attrib)
                                         (unlist-if-one (nrql-head-get-attribute-fillers
                                                         (or *running-substrate* 
                                                             *cur-substrate*)
                                                         ind attrib))))
                   
                   (told-value 2 nil
                               ,(lambda (ind attrib)
                                  (unlist-if-one (nrql-head-get-told-value
                                                  (or *running-substrate*
                                                      *cur-substrate*)
                                                  ind attrib))))
                   
                   (told-value-if-exists 2 nil
                                         ,(lambda (ind attrib)
                                            (unlist-if-one (nrql-head-get-told-value-if-exists
                                                            (or *running-substrate*
                                                                *cur-substrate*)
                                                            ind attrib))))

                   (datatype-fillers 2 nil
                                     ,(lambda (ind attrib)
                                        (unlist-if-one (nrql-head-get-datatype-fillers (or *running-substrate*
                                                                                           *cur-substrate*)
                                                                                       ind attrib))))

                   (annotation-datatype-fillers 2 nil
                                                ,(lambda (ind attrib)
                                                   (unlist-if-one (nrql-head-get-annotation-datatype-fillers 
                                                                   (or *running-substrate* 
                                                                       *cur-substrate*)
                                                                   ind attrib))))
                   
                   ;;;
                   ;;; 
                   ;;;

                   (alisp-racer-p nil nil
                                  ,(lambda () #+:mlisp nil #-:mlisp t))
                   
                   (mlisp-racer-p nil nil
                                  ,(lambda () #+:mlisp t #-:mlisp nil))
              
                                      
                   ;;;
                   ;;; Typ-Praedikate
                   ;;; 
      
                   (null 1)      
                   (zerop 1)
                   (minusp 1)
                   (plusp 1)
                   (realp 1)
                   (floatp 1)
                   (rationalp t)
                   (consp 1)
                   (listp 1)
                   (symbolp 1)
                   (stringp 1)
                   (numberp 1)
                   (evenp 1)
                   (oddp 1)

                   ;;;
                   ;;; Identitaet
                   ;;; 
                   
                   (identity 1 nil)

                   ;;;
                   ;;; Vergleichs-Praedikate
                   ;;; 
            
                   (eq 2)
                   (eql 2)
                   (equal 2)
                   (equalp 2)
                   (string= 2)
                   (string-equal 2)
                   (set-equal (2 nil) nil ,(lambda (a b &key (test 'equal) (key 'identity))
                                             (and (subsetp a b 
                                                           :test (process-fun-arg test)
                                                           :key (process-fun-arg key))
                                                  (subsetp b a 
                                                           :test (process-fun-arg test)
                                                           :key (process-fun-arg key)))))
                   (= n)
                   (< n)
                   (<= n)
                   (> n)
                   (>= n)

                   (string-lessp 2)
                   (string< 2)
                   (string<= 2)
                   (string-greaterp 2)
                   (string> 2)
                   (string>= 2)
                   (set-subset (2 nil)  nil ,(lambda (a b &key (test 'equal) (key 'identity))
                                               (subsetp a b 
                                                        :test (process-fun-arg test)
                                                        :key (process-fun-arg key))))
                   
                   ;;;
                   ;;; Sequences 
                   ;;; 

                   (count 1 nil)
                   
                   ;;; count-if? special

                   (member (2 nil) nil ,(lambda (a b &key (key 'identity) (test 'equal))
                                          (member a b 
                                                  :test (process-fun-arg test)
                                                  :key (process-fun-arg key))))
                   
                   ;;; member-if? special 

                   (find (2 nil) nil ,(lambda (a b &key (key 'identity) (test 'equal))
                                        (find a b 
                                              :test (process-fun-arg test)
                                              :key (process-fun-arg key))))
                   
                   ;;; find-if?  special 

                   (search (2 nil) nil  ,(lambda (a b &key (key 'identity) (test 'equal))
                                           (search a b 
                                                   :test (process-fun-arg test)
                                                   :key (process-fun-arg key))))

                   (position (2 nil) nil ,(lambda (a b &key (key 'identity) (test 'equal))
                                            (position a b 
                                                      :test (process-fun-arg test)
                                                      :key (process-fun-arg key))))

                   ;;; position-if? special 

                   (assoc (2 nil) nil ,(lambda (a b &key (key 'identity) (test 'equal))
                                         (assoc a b 
                                                :test (process-fun-arg test)
                                                :key (process-fun-arg key))))

                   (remove (2 nil) nil ,(lambda (a b &key (key 'identity) (test 'equal))
                                          (remove a b 
                                                  :test (process-fun-arg test)
                                                  :key (process-fun-arg key))))

                   ;;; remove-if? special 


                   (remove-duplicates (1 nil) nil ,(lambda (a &key (key 'identity) (test 'equal))
                                                     (remove-duplicates a 
                                                                        :test (process-fun-arg test)
                                                                        :key (process-fun-arg key))))

                   (substitute 3 nil ,(lambda (from to seq)
                                        (typecase seq
                                          (symbol (intern (substitute from to (symbol-name seq)  :test #'equal)
                                                          (find-package :racer-user)))
                                          (otherwise (substitute from to seq  :test #'equal)))))


                   (tree-substitute 3 nil ,(lambda (from to tree)
                                             (labels ((do-it (tree)
                                                        (if (consp tree)
                                                            (let ((tree
                                                                   (substitute from to tree :test #'equal)))
                                                              (cons (do-it (car tree))
                                                                    (do-it (cdr tree))))
                                                          tree)))
                                               (do-it tree))))
                   
                   (list n)
                   (cons 2)
                   
                   (elt 2)
                   (nth 2)
                   
                   (length 1) 

                   (append n)
                   (copy-list 1)
                   (copy-tree 1)
                   (reverse 1)

                   (flatten 1 nil ,(lambda (list)
                                     (labels ((flatten (list)
                                                (if (consp list)
                                                    (append (flatten (first list))
                                                            (flatten (rest list)))
                                                  (ensure-list list))))
                                       (flatten list))))

                   (subseq (1 3))
                   
                   
                   (set-difference (2 nil) nil ,(lambda (a b &key (test 'equal) (key 'identity))
                                                  (set-difference a b 
                                                                  :test (process-fun-arg test)
                                                                  :key (process-fun-arg key))))

                   (union (2 nil) nil ,(lambda (a b &key (test 'equal) (key 'identity))
                                         (union a b 
                                                :test (process-fun-arg test)
                                                :key (process-fun-arg key))))

                   (intersection  (2 nil) nil ,(lambda (a b &key (test 'equal) (key 'identity))
                                                 (intersection a b 
                                                               :test (process-fun-arg test)
                                                               :key (process-fun-arg key))))

                   ;;;
                   ;;; 
                   ;;; 
                   
                   (car 1)
                   (cdr 1)
                   (cadr 1)
                   (cddr 1)
                   (caddr 1)
                   (cdddr 1)
                   (cddddr 1)

                   (caar 1)
                   (caaar 1)
                   (caaaar 1)
                   (caadr 1)
                   (cadar 1)
                   (caadar 1)
                   (caaadar 1)
                   (cadaar 1)
                   (caadaar 1)
                   
                   (last 1)
                   (butlast 1)

                   (first 1)
                   (second 1)
                   (third 1)
                   (fourth 1)
                   (fifth 1)
                   (sixth 1)
                   (seventh 1)
                   (eighth 1)
                   (ninth 1)
                   (tenth 1)
                   (rest 1)
      
      
                   ;;;
                   ;;; Konvertierer
                   ;;; 

                   (type-of 1)

                   (ensure-list 1 nil ,(lambda (a)
                                         (if (listp a)
                                             a
                                           (list a))))

                   (to-string 1 nil ,(lambda (val)
                                       (format nil "~A" val)))

                   (to-symbol 1 nil ,(lambda (val)
                                       (intern (format nil "~A" val) 
                                               (find-package :racer-user))))

                   (to-keyword 1 nil ,(lambda (val)
                                        (intern (format nil "~A" val) 
                                                (find-package :keyword))))

                   (to-integer 1 nil ,(lambda (val)
                                        (parse-integer
                                         (format nil "~A" val))))
                   
                   (to-float 1 nil ,(lambda (val)
                                      (let ((res 
                                             (read-from-string val)))
                                        (if (numberp res)
                                            (coerce res 'float)
                                          (minilisp-error "Not a float: ~A" val)))))

                   (to-char 1 nil ,(lambda (val)
                                     (read-from-string
                                      (format nil "#\\~A" val))))

                   (read-from-string 1)

                   ;;;
                   ;;; File IO
                   ;;;

                   (directory 1 nil ,(lambda (pat)
                                       (mapcar #'namestring (directory pat))))

                   (delete-file 1 nil ,(lambda (fn)
					 (if *unsafe-mode* 
					     (if (probe-file fn)
						 (delete-file fn)
					       (minilisp-error "Can't delete file ~S, since it doesn't exist" fn))
                   		       
                                           (minilisp-error "Can't delete file ~S, since Racer is not running in unsafe mode" fn))))
                   
                   (probe-file 1 nil ,(lambda (fn)
                                        (when (probe-file fn)
                                          (namestring (probe-file fn)))))

                   (file-namestring 1)

                   (directory-namestring 1)
                   
                   ;;;
                   ;;;
                   ;;;

                   (display (1 nil) nil ,(lambda (string &rest args)
                                           `(:display-request
                                             ,(apply #'format nil string 
                                                     (mapcar #'(lambda (x) 
                                                                 (evaluate1 `(progn (quote ,x)))) 
                                                             args)))))

                   ;;;
                   ;;;
                   ;;;

                   (sleep 1)

                   (get-seconds 0 nil ,(lambda ()
                                         (float (/ (get-internal-real-time) internal-time-units-per-second))))
                   
                   ;;;
                   ;;; Splitting 
                   ;;; 
                   
                   (after-separator (1 2) nil ,(lambda (val &optional (sep "#"))
                                                 (typecase val
                                                   ((or string symbol)
                                                    (let* ((val-string 
                                                            (if (stringp val)
                                                                val
                                                              (symbol-name val)))
                                                           (pos (search sep val-string)))
                                                      (if pos
                                                          (let* ((length (length sep))
                                                                 (new-val 
                                                                  (subseq val-string (+ length pos))))
                                                            (typecase val
                                                              (string new-val)
                                                              (otherwise (intern (format nil "~A" new-val)
                                                                                 (find-package :racer-user)))))
                                                        val)))
                                                   (otherwise val))))

                   (before-separator (1 2) nil ,(lambda (val &optional (sep "#"))
                                                  (typecase val
                                                    ((or string symbol)
                                                     (let* ((val-string 
                                                             (if (stringp val)
                                                                 val
                                                               (symbol-name val)))
                                                            (pos (search sep val-string)))
                                                       (if pos
                                                           (let ((new-val 
                                                                  (subseq val-string 0 pos)))
                                                             (typecase val
                                                               (string new-val)
                                                               (otherwise (intern (format nil "~A" new-val)
                                                                                  (find-package :racer-user)))))
                                                         val)))
                                                    (otherwise val))))

                   ;;;
                   ;;;
                   ;;;

                   (group-by 2 nil
                             ;;; (group-by '((1 2 1) (1 2 2) (1 3 1)) '(0 1))
                             ,(lambda (rel vec)
                                (let ((hash (make-hash-table :test #'equal)))
                                  (dolist (tuple rel)
                                    (let ((key
                                           (mapcar #'(lambda (pos)
                                                       (nth pos tuple))
                                                   vec)))
                                      (push tuple (gethash key hash))))
    
                                  (let ((res nil))
                                    (maphash #'(lambda (key val)
                                                 (push (list key val) res))
                                             hash)
                                    res))))

                   ;;; 
                   ;;; Sortierer 
                   ;;; 

                   (sort (2 nil) nil ,(lambda (a test &key (key 'identity))
                                        (sort (copy-tree a)
                                              (if test
                                                  (process-fun-arg test)
                                                #'<) 
                                              :key (process-fun-arg key))))

                   (stable-sort (2 nil) nil ,(lambda (a test &key (key 'identity))
                                               (stable-sort (copy-tree a)
                                                            (if test
                                                                (process-fun-arg test)
                                                              #'<) 
                                                            :key (process-fun-arg key))))


                   (tsort 1 nil ,(lambda (edges)
                                   (labels ((do-it (rem-edges rem-nodes processed-nodes processed-edges)
                                              
                                              ;; (pprint (list rem-edges rem-nodes processed-nodes))
                                              
                                              (let* ((next-nodes
                                                      (remove-if
                                                       #'(lambda (next)
                                                           (some
                                                            #'(lambda (edge)
                                                                (and 
                                                                 (eq (second edge) next)
                                                                 (not (member (first edge) processed-nodes))))
                                                            rem-edges))
                                                       rem-nodes))

                                                     (cycle? 
                                                      (some #'(lambda (node)
                                                                (member node processed-nodes))
                                                            next-nodes))

                                                     (next-node (first next-nodes))
                                                     
                                                     (edges (remove-if-not #'(lambda (edge) 
                                                                               (eq (second edge) next-node))
                                                                           rem-edges))
                                                     (rem-edges (remove-if #'(lambda (edge) 
                                                                               (eq (second edge) next-node))
                                                                           rem-edges)))                                                      

                                                ;; (pprint (list next-node cycle?))
                                                ;; (terpri)
                                                
                                                (cond ((or (and rem-nodes (not next-node))
                                                           cycle?)
                                                               
                                                       :no-partial-order)

                                                      ((not rem-nodes)
                                                       (list (nreverse processed-nodes)
                                                             (nreverse processed-edges)))

                                                      (t 
                                                       (do-it
                                                        rem-edges
                                                        (remove next-node rem-nodes)
                                                        (cons next-node processed-nodes)
                                                        (nconc edges processed-edges)))))))
                                     
                                  
                                     (let* ((dir-edges 
                                             (set-difference
                                              edges
                                              (mapcan #'(lambda (edge)
                                                          (let* ((from (first edge))
                                                                 (over (second edge))
                                                                 (succs
                                                                  (mapcar #'second
                                                                          (remove-if-not #'(lambda (edge2)
                                                                                             (eq over
                                                                                                 (first edge2)))
                                                                                         (remove edge edges)))))
                                                            (mapcar #'(lambda (to)
                                                                        (list from to))
                                                                    succs)))
                                                      edges)
                                              :test #'equal))

                                            (nodes 
                                             (remove-duplicates 
                                              (apply #'append dir-edges))))
                                       
                                       (do-it dir-edges nodes nil nil)))))

                   (make-graph-from 2 nil ,(lambda (domain pred)
                                             (let ((res nil))
                                               (dolist (from domain)
                                                 (dolist (to domain)
                                                   (when (funcall (process-fun-arg pred) from to)
                                                     (push (list from to) res))))
                                               res)))
                   
                   (find-components 1 nil ,(lambda (edges)
                                             (let ((eh (make-hash-table)))
                                               (dolist (edge edges)
                                                 (let* ((from (first edge))
                                                        (to (second edge))

                                                        (edges (remove-duplicates
                                                                (cons edge
                                                                      (append (gethash from eh)
                                                                              (gethash to eh)))
                                                                :test #'equal)))

                                                   (dolist (edge edges)
                                                     (setf (gethash (first edge) eh) edges
                                                           (gethash (second edge) eh) edges))))

                                               (remove-duplicates
                                                (loop as val being the hash-value of eh collect val)))))

                   (find-cycles 1 nil ,(lambda (edges)
                                         (let ((eh (make-hash-table))
                                               (cycles nil)
                                               (res nil))

                                           (dolist (edge (remove-duplicates edges :test #'equal))
                                             (let* ((from (first edge))
                                                    (to (second edge))
                                                    (edges (remove-duplicates
                                                            (append (gethash from eh)
                                                                    (gethash to eh))
                                                            :test #'equal)))

                                               (when (or (eq from to) (member to edges :key #'first))
                                                 (pushnew from cycles))

                                               (let ((all (cons edge edges)))
                                                 (dolist (edge all)
                                                   (setf (gethash (first edge) eh) all
                                                         (gethash (second edge) eh) all)))))

                                           (loop while cycles do
                                                 (let* ((root (pop cycles))
                                                        (cluster (reduce #'append (gethash root eh))))
                                                   (setf cycles (set-difference cycles cluster))
                                                   (push (gethash root eh) res)))

                                           res)))
                   
                   ;;;
                   ;;; alte Sortier-Funktionen (nun redundant, aber evtl. convenient to have)
                   ;;;

                   (sort< 1 nil ,(lambda (args)
                                   (funcall #'sort (copy-list args) #'<)))

                   (sort> 1 nil (lambda (args)
                                  (funcall #'sort (copy-list args) #'>)))

                   (sort-string< 1 nil ,(lambda (args)
                                          (funcall #'sort (copy-list args) #'string<)))

                   (sort-string> 1 nil ,(lambda (args)
                                          (funcall #'sort (copy-list args) #'string>)))

                   (sort-string-lessp 1 nil ,(lambda (args)
                                               (funcall #'sort (copy-list args) #'string-lessp)))

                   (sort-string-greaterp 1 nil ,(lambda (args)
                                                  (funcall #'sort (copy-list args) #'string-greaterp)))

                   (sort-symbol-name-lessp 1 nil ,(lambda (args)
                                                    (funcall #'sort (copy-list args) #'string-lessp :key #'symbol-name)))

                   (sort-symbol-name-greaterp 1 nil ,(lambda (args)
                                                       (funcall #'sort (copy-list args) #'string-lessp :key #'symbol-name)))

                   ;;;
                   ;;; Zahlen 
                   ;;; 

                   (max (1 nil) nil ,(lambda (list &key (key 'identity))
                                       (let ((fn (process-fun-arg key)))
                                         (apply 'max (mapcar fn list)))))

                   (min (1 nil) nil ,(lambda (list &key (key 'identity))
                                       (let ((fn (process-fun-arg key)))
                                         (apply 'min (mapcar fn list)))))
                   
                   (maximum (1 nil) nil ,(lambda (list &key (key 'identity))
                                           (let ((fn (process-fun-arg key)))
                                             (apply 'max 
                                                    (mapcar fn list)))))

                   (minimum (1 nil) nil ,(lambda (list &key (key 'identity))
                                           (let ((fn (process-fun-arg key)))
                                             (apply 'min 
                                                    (mapcar fn list)))))

                   (+ n) 
                   (- n)
                   (* n)
                   (/ n)
                   
                   (ceiling (1 2))
                   (floor (1 2))
                   (mod 2)

                   (random 1)

                   (abs 1)
                   (asin 1)
                   (asinh 1)
                   (atan 1)
                   (atanh 1)

                   (expt (1 2))
                   (log (1 2))
                   
                   (float 1)
                   (rationalize 1)
                   (round 1)

                   (cos 1) 
                   (cosh 1)
                   (sin 1)
                   (sinh 1)
                   (sqrt 1)
                   (tan 1)
                   (tanh 1)
                   (1+ 1)
                   (1- 1)
                   (signum 1)
                   
                   ;;;
                   ;;; Strings  
                   ;;;
                   
                   (concat n nil ,(lambda  (&rest args)
                                    (apply #'concatenate 'string args)))

                   (string-upcase 1)
                   (string-downcase 1)
                   (string-capitalize 1)    

                   ;;;
                   ;;; Symbole 
                   ;;; 

                   (symbol-name 1)
                   
                   (reset-gensym-counter 0 nil ,(lambda () 
                                                 (setf *sym-counter* 0)))
                   
                   (gensym (0 1) nil ,(lambda (&optional (string "GENSYM"))
                                        (incf *sym-counter*)
                                        (intern (format nil "~A-~A" string *sym-counter*)
                                                (find-package :racer-user))))
                                        
      
                   ;;;
                   ;;;
                   ;;; 

                   (format (2 nil))
                   (princ 1) 
                   (write 1)
                   (pprint 1)
                   (terpri 0)
                   
                   ;;;
                   ;;; XML Support
                   ;;; 
            
                   (lisp-to-xml (1 nil))
                   (xml-to-lisp 1))))

  (unless (member (to-keyword-big (first entry))
                  '(:DEFINE :DEFINE1 :DEFCON :DEFCON1 :DEFPAR :DEFPAR1 
                    :EVALUATE :CL-EVALUATE :CL-EVALUATE-UNQUOTED
                    ;;; muss sonderbehandelt werden! 
                    ;;; wg. (evaluate (let ((x '(evaluate x))) (evaluate x))) -> Endlosschleife! 
                    ))

    
    (setf (gethash (to-keyword-big (first entry))
                   +allowed-cl-functions+)
          (rest entry))))

(defun add-server-hook-function (name)
  ;;; f. Patch / Extension Mechanism 
  ;;; Macht Patch / Extension aufrufbar von
  ;;; MiniLisp
  (setf (gethash (to-keyword-big name)
                 +allowed-cl-functions+)
        (list nil 
              nil
              #'(lambda (&rest args)
                  (funcall (symbol-function name) 
                           (cons name args))))))

;;;
;;;
;;;


(defun upper-keyword (expr) 
  (if (and (consp expr)
           (symbolp (first expr)))
      (intern (string-upcase (symbol-name (first expr)))
              (find-package :keyword))
    expr))

(defun transform-args (expr)
  #+:mlisp
  (mapcar (lambda (x) 
	    (cond ((keywordp x) 
		   (intern (string-downcase (symbol-name x)) :keyword))
		  ((member x '(T racer::T racer-user::T ts::T)) t)			      
		  ((member x '(NIL racer::NIL racer-user::NIL ts::NIL)) nil)	
		  ((member x '(*TOP* racer::*TOP* racer-user::*TOP* ts::*TOP*)) '*top*)	
		  ((member x '(*BOTTOM* racer::*BOTTOM* racer-user::*BOTTOM* ts::*BOTTOM*)) '*bottom*)
		  (t x)))
	  expr)
  #-:mlisp
  expr)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf +native-minilisp-functions+ 
        '(:QUOTE
          :BACKQUOTE 
          :BQ-COMMA
          :BQ-COMMA-ATSIGN
          :REDUCE
          :AND
          :OR
          :NOT
          :=>
          :SETQ 
          :SETF
          :INCF 
          :DECF
          :PUSH 
          :POP
          :IF
          :WHEN
          :UNLESS
          :COND
          :MAPTREE
          :MAPLIST
          :MAPCOUNT
          :RETURN
          :DOLIST
          :DOTIMES
          :COUNT-IF
          :MEMBER-IF
          :FIND-IF
          :POSITION-IF
          :REMOVE-IF
          :REMOVE-IF-NOT
          :COLLECT-IF
          :TREE-REMOVE-IF
          :TREE-REMOVE-IF-NOT
          :TREE-COLLECT-IF
          :EVERY
          :SOME
          :PROGN
          :PROG1
          :LET
          :LET*
          :MULTIPLE-VALUE-BIND
          :VALUES
          :BLET
          :LAMBDA
          :WITH-NRQL-SETTINGS
          :HTML
          :CONTENT
          :WITH-HTML
          :DEFER)))

(defun eval-expr (lvars body lvals)
  
  (let* (#|(body (change-package-of-description body :racer-user)) ;;; ???
         (lvars (change-package-of-description lvars :racer-user)) ;;; ??? |# 

         (global-bindings (mapcar #'(lambda (lvar lval)
                                      (list lvar lval))
                                  lvars lvals))
         
         (output-stream t)
         (*multiprocess-queries* nil))

    (labels ((process-arg (arg bindings)
               (cond ((eq arg :reject)
                      +reject-tuple+)
                     ((keywordp arg) arg)
                     ((numberp arg) arg)
                     ((stringp arg) arg)
                     ((member arg '(nil t ())) arg)
                     ((symbolp arg)
                      (let ((found (assoc arg bindings
                                          :key #'symbol-name
                                          :test #'string-equal)))
                        (if found
                            (values (second found) found bindings)
                          (multiple-value-bind (binding found-p)
                              (gethash (to-keyword-big arg) *registered-values*)
                            (if found-p 
                                (values binding :value)
                              (let ((found (assoc arg global-bindings
                                                  :key #'symbol-name
                                                  :test #'string-equal)))
                                (if found
                                    (values (second found) found global-bindings)
                                  (minilisp-error "Variable ~A is unbound" arg))))))))
                     ((characterp arg) arg)
                     (t (minilisp-error "Unrecognized literal ~A in ~A" arg body))))

             (my-eval (expr bindings bq in-evaluate-p &optional defer-p)
	       
	       ;;; bq (backquote) wird nur fuer Allegro gebraucht

	       ;;; (push (list expr bindings bq in-evaluate-p) *res*)
	       
               (when *debug-evaluator-p* 
                 (format t "~%Expr: ~S   BQ: ~A  Bindings: ~A~%" expr bq bindings)
                 (when (consp expr)
                   (format t "OP: ~A~%~%" (first expr))))

               (cond ((consp expr)

                      (let ((op (upper-keyword expr)))

                        (cond 

			 ;;;
			 ;;; COMMON LISP LOOPHOLE EVALUATE
			 ;;; 
			 
			 ((eq op :CL-EVALUATE)
			  (let ((*package* (find-package :racer-user)))
			    (let ((cbindings bindings)
				  (cexpr (copy-tree (cons 'progn (cdr expr)))))
			      (loop while cbindings do
				    (let* ((var-val (pop cbindings))
					   (var (first var-val))
					   (val (second var-val)))
				      (setf cexpr
					(ts:tree-substitute cexpr var `(quote ,val)))))
			      (eval cexpr))))
			 
			  ((eq op :CL-EVALUATE-UNQUOTED)
			  (let ((*package* (find-package :racer-user)))
			    (let ((cbindings bindings)
				  (cexpr (copy-tree (cons 'progn (cdr expr)))))
			      (loop while cbindings do
				    (let* ((var-val (pop cbindings))
					   (var (first var-val))
					   (val (second var-val)))
				      (setf cexpr
					(ts:tree-substitute cexpr var val))))
			      (eval cexpr))))
                         
                         ;;;
                         ;;; EVALUATE - only 1 level
                         ;;; 

                         ((eq op :EVALUATE)
                          (when in-evaluate-p
                            (minilisp-error "Nested calls of EVALUATE are not allowed in order to ensure termination: ~A"
                                            expr))
                          
                          (my-eval (my-eval (second expr) bindings bq t)
                                   bindings bq t))
                         
                         ;;;
                         ;;; DEFER
                         ;;; 

                         ((eq op :DEFER)

                          ;;; ACHTUNG - defer-p = t gilt nur FUER DIESEN Eval-Level!
                          ;;; jede Sub-Sexpression muss deferred werden, wenn gewuenscht: 
                          ;;; (defer (x (s))) =/= (defer (x (defer (s)))) 

                          `(progn 
                             ,@(mapcar #'(lambda (expr)
                                           (my-eval expr bindings bq in-evaluate-p t))
                                       (rest expr))))

                         ;;;
                         ;;; quote, backquote, comma
                         ;;; 

                         ((eq op :QUOTE)
			  (if (not (zerop bq))
			      
			      (cons 'quote
				    (multiple-value-bind (res flatten)
					(my-eval (second expr) bindings bq in-evaluate-p)
				      (if flatten 
					  res
					(list res))))

			    (second expr)))
		      
                         ((and (not (zerop bq))
                               (not (member op
                                            '(:BACKQUOTE 
					      :BQ-COMMA
                                              :BQ-COMMA-ATSIGN))))

                          (apply #'append
                                 (mapcar #'(lambda (x)
                                             (multiple-value-bind (res flatten)
                                                 (my-eval x bindings bq in-evaluate-p)
                                               (if flatten 
                                                   res
                                                 (list res))))
                                         expr)))
		      
                         ((eq op :BACKQUOTE)
                          (my-eval (second expr) bindings (1+ bq) in-evaluate-p))
		      
                         ((eq op :BQ-COMMA)
                          (if (zerop bq)
                              (minilisp-error "Comma without backquote found")
			    (values
                             (my-eval (second expr) bindings (1- bq) in-evaluate-p)
                             nil)))
		      
                         ((eq op :BQ-COMMA-ATSIGN)
                          (if (zerop bq)
                              (minilisp-error "Comma without backquote found")
                            (values
                             (my-eval (second expr) bindings (1- bq) in-evaluate-p)
                             t)))
		      
                         ((eq op :BQ-LIST*)
                          (apply #'list* 
                                 (mapcar #'(lambda (x) 
                                             (my-eval x bindings bq in-evaluate-p))
                                         (rest expr))))

                         ((eq op :BQ-LIST)
                          (mapcar #'(lambda (x) 
                                      (my-eval x bindings bq in-evaluate-p))
                                  (rest expr)))

                         ((eq op :BQ-APPEND)
                          (apply #'append
                                 (mapcar #'(lambda (x) 
                                             (my-eval x bindings bq in-evaluate-p))
                                         (rest expr))))

                         #+:ccl
                         ((eq op :LIST*)
			  (apply #'list* 
                                 (mapcar #'(lambda (x) 
                                             (my-eval x bindings bq in-evaluate-p))
                                         (rest expr))))

                         #+:ccl
                         ((eq op :LIST)
                          (mapcar #'(lambda (x) 
                                      (my-eval x bindings bq in-evaluate-p))
                                  (rest expr)))

                         #+:ccl
                         ((eq op :APPEND)
                          (apply #'append
                                 (mapcar #'(lambda (x) 
                                             (my-eval x bindings bq in-evaluate-p))
                                         (rest expr))))

                         ;;;
                         ;;; reduce
                         ;;;

                         ((eq op :REDUCE) 
                          
			  (let* ((vals (my-eval (third expr) bindings bq in-evaluate-p))
				 (op (second expr)))
			    
			    (labels ((do-it (vals)
				       (when vals
					 (if (not (cddr vals))
					     `(,op ',(first vals)
						   ',(second vals))
					   `(,op ',(first vals)
						 ,(do-it (cdr vals)))))))
			      
			      (my-eval (do-it vals) bindings bq in-evaluate-p))))
		      
                         ;;;
                         ;;; and, or, not 
                         ;;; 

                         ((eq op :AND)
		       
                          (every #'(lambda (expr)
                                     (my-eval expr bindings bq in-evaluate-p))
                                 (rest expr)))

                         ((eq op :OR)
		       
                          (some #'(lambda (expr)
                                    (my-eval expr bindings bq in-evaluate-p))
                                (rest expr)))

                         ((eq op :=>)
                          (my-eval `(or (not ,(second expr))
                                        ,(third expr))
                                   bindings bq in-evaluate-p))

                         ((eq op :NOT)

                          (not (my-eval (second expr) bindings bq in-evaluate-p)))
		      
                         ;;;
                         ;;; incf, decf, setf, setq
                         ;;;
		      
                         ((member op '(:SETF :SETQ :INCF :DECF))

                          (let* ((var (second expr))
                                 (val (third expr))
                                 (val
                                  (if val
                                      (my-eval val bindings bq in-evaluate-p)
                                    (when (member op '(:INCF :DECF))
                                      1))))

                            (multiple-value-bind (val2 entry env)
                                (process-arg var bindings)

                              (declare (ignorable val2))

                              (let ((val 
                                     (case op
                                       (:INCF (+ val2 val))
                                       (:DECF (- val2 val))
                                       (otherwise 
                                        val))))

                                (if (eq entry :value)
                                    (register-value var val)
                                  (if entry
                                      (setf (second entry) val)
                                    (setf env (cons (list var val) env))))
                              
                                val))))
                         
                         ;;;
                         ;;; push, pop 
                         ;;;
		      
                         ((eq op :PUSH)

                          (let* ((var (third expr))
                                 (val (second expr))
                                 (val 
                                  (my-eval val bindings bq in-evaluate-p)))

                            (multiple-value-bind (val2 entry env)
                                (process-arg var bindings)

                              (let ((val 
                                     (cons val val2)))
                                
                                (if (eq entry :value)
                                    (register-value var val)
                                  (if entry
                                      (setf (second entry) val)
                                    (setf env (cons (list var val) env))))
                              
                                val))))

                         ((eq op :POP)

                          (let* ((var (second expr)))

                            (multiple-value-bind (val2 entry env)
                                (process-arg var bindings)

                              (let ((val 
                                     (rest val2)))
                                
                                (if (eq entry :value)
                                    (register-value var val)
                                  (if entry
                                      (setf (second entry) val)
                                    (setf env (cons (list var val) env))))
                              
                                val))))
                      
                         ;;;
                         ;;; if, when, unless, cond 
                         ;;; 
                      
                         ((eq op :IF)
		       
                          (my-eval 
                           (if (my-eval (second expr) bindings bq in-evaluate-p)
                               (third expr)
                             (fourth expr))
                           bindings
                           bq
                           in-evaluate-p
                           ))
		      
                         ((eq op :WHEN)
		       
                          (my-eval 
                           (when (my-eval (second expr) bindings bq in-evaluate-p)
                             `(progn ,@(cddr expr)))
                           bindings
                           bq
                           in-evaluate-p
                           ))
                      

                         ((eq op :UNLESS)
		       
                          (my-eval 
                           (unless (my-eval (second expr) bindings bq in-evaluate-p)
                             `(progn ,@(cddr expr)))
                           bindings
                           bq
                           in-evaluate-p
                           ))
                      
                         ((eq op :COND)
		       
                          (dolist (cond-clause (rest expr))
                            (when (my-eval (first cond-clause) bindings bq in-evaluate-p)
                              (return (my-eval `(progn ,@(rest cond-clause)) bindings bq in-evaluate-p)))))

                         ;;;
                         ;;; maptree, maplist, mapcount
                         ;;; 

                         ((eq op :MAPTREE)
                       
                          (let ((tree (my-eval (third expr) bindings bq in-evaluate-p))
                                (lambda (second expr)))

                            (tree-map #'(lambda (x) 
                                          (my-eval (list lambda `(quote ,x))
                                                   bindings bq in-evaluate-p))
                                      tree)))

                         ((eq op :MAPLIST)
                       
                          (let ((tree (my-eval (third expr) bindings bq in-evaluate-p))
                                (lambda (second expr)))

                            (mapcar #'(lambda (x) 
                                        (my-eval (list lambda `(quote ,x))
                                                 bindings bq in-evaluate-p))
                                    tree)))

                         ((eq op :MAPCOUNT)                       
                          (let* ((from (my-eval (third expr) bindings bq in-evaluate-p))
                                 (to (my-eval (fourth expr) bindings bq in-evaluate-p))
                                 (by (or (when (fifth expr)
                                           (my-eval (fifth expr) bindings bq in-evaluate-p))
                                         1))
                                 (lambda (second expr)))

                            (when (or (zerop by)
                                      (minusp by))
                              (minilisp-error "Bad MAPCOUNT increment ~A given" by))

                            (if (<= from to)
                                (loop as x from from to to by by collect
                                      (my-eval (list lambda x)
                                               bindings bq in-evaluate-p))
                              (loop as x from from downto to by by collect
                                    (my-eval (list lambda x)
                                             bindings bq in-evaluate-p)))))
                         
                         ;;;
                         ;;; dolist, dotimes -> sicherstellen, dass incf / decf nicht moeglich! 
                         ;;; 

                         ((eq op :RETURN)
                          (throw :return-from-dolist 
                                 (my-eval (second expr)
                                          bindings bq in-evaluate-p)))
                         
                         ((eq op :DOLIST)

                          (let ((res 
                                 (catch :return-from-dolist
                                   (dolist (var 
                                            (my-eval (second (second expr)) bindings bq in-evaluate-p))
                                     
                                     (my-eval 
                                     
                                      `(let ((,(first (second expr)) ',var))
                                         ,@(cddr expr))

                                      bindings bq in-evaluate-p)))))

                            res))
                         
                         ((eq op :DOTIMES)

                          (let ((res 
                                 (catch :return-from-dolist
                                   (dotimes (var 
                                             (my-eval (second (second expr)) bindings bq in-evaluate-p))
                                     
                                     (my-eval 
                                     
                                      `(let ((,(first (second expr)) ',var))
                                         ,@(cddr expr))

                                      bindings bq in-evaluate-p)))))

                            res))

                         ;;;
                         ;;; count-if, member-if, find-if, position-if,  
                         ;;; 

                         ((eq op :COUNT-IF)
                       
                          (let ((tree (my-eval (third expr) bindings bq in-evaluate-p))
                                (lambda (second expr)))

                            (count-if #'(lambda (x) 
                                          (my-eval (list lambda `(quote ,x))
                                                   bindings bq in-evaluate-p))
                                      tree)))

                         ((eq op :MEMBER-IF)
                       
                          (let ((tree (my-eval (third expr) bindings bq in-evaluate-p))
                                (lambda (second expr)))

                            (member-if #'(lambda (x) 
                                           (my-eval (list lambda `(quote ,x))
                                                    bindings bq in-evaluate-p))
                                       tree)))

                         ((eq op :FIND-IF)
                       
                          (let ((tree (my-eval (third expr) bindings bq in-evaluate-p))
                                (lambda (second expr)))

                            (find-if #'(lambda (x) 
                                         (my-eval (list lambda `(quote ,x))
                                                  bindings bq in-evaluate-p))
                                     tree)))
                       
                         ((eq op :POSITION-IF)
                       
                          (let ((tree (my-eval (third expr) bindings bq in-evaluate-p))
                                (lambda (second expr)))

                            (position-if #'(lambda (x) 
                                             (my-eval (list lambda `(quote ,x))
                                                      bindings bq in-evaluate-p))
                                         tree
                                         :from-end (second (member :from-end (cddr expr))))))

                         ;;;
                         ;;; remove-if, tree-remove-if 
                         ;;;                        
		      
                         ((eq op :REMOVE-IF)
                       
                          (let ((tree (my-eval (third expr) bindings bq in-evaluate-p))
                                (lambda (second expr)))

                            (remove-if #'(lambda (x) 
                                           (my-eval (list lambda `(quote ,x))
                                                    bindings bq in-evaluate-p))
                                       tree)))


                         
                         ((member op '(:REMOVE-IF-NOT :COLLECT-IF))
                       
                          (let ((tree (my-eval (third expr) bindings bq in-evaluate-p))
                                (lambda (second expr)))

                            (remove-if-not #'(lambda (x) 
                                               (my-eval (list lambda `(quote ,x))
                                                        bindings bq in-evaluate-p))
                                           tree)))

                         ((eq op :TREE-REMOVE-IF)
                       
                          (let ((tree (my-eval (third expr) bindings bq in-evaluate-p))
                                (lambda (second expr)))

                            (tree-remove #'(lambda (x) 
                                             (my-eval (list lambda `(quote ,x))
                                                      bindings bq in-evaluate-p))
                                         tree)))

                         ((member op '(:TREE-REMOVE-IF-NOT :TREE-COLLECT-IF))
                       
                          (let ((tree (my-eval (third expr) bindings bq in-evaluate-p))
                                (lambda (second expr)))

                            (tree-remove #'(lambda (x) 
                                             (not (my-eval (list lambda `(quote ,x))
                                                           bindings bq in-evaluate-p)))
                                         tree)))

                         ;;;
                         ;;;
                         ;;;

                         ((member op '(:TREE-REMOVE-IF*))
                          
                          (let ((tree (my-eval (third expr) bindings bq in-evaluate-p))
                                (lambda (second expr)))

                            (labels ((do-it (tree)
                                       (if (consp tree)
                                           (let ((tree
                                                  (remove-if #'(lambda (x) 
                                                                 (my-eval (list lambda `(quote ,x))
                                                                          bindings bq in-evaluate-p))
                                                             tree)))
                                             (cons (do-it (car tree))
                                                   (do-it (cdr tree))))
                                         tree)))
                              (do-it tree))))
                         
                         ;;;
                         ;;; every, some
                         ;;; 

                         ((eq op :EVERY)
                       
                          (let ((tree (my-eval (third expr) bindings bq in-evaluate-p))
                                (lambda (second expr)))
                         
                            (every #'(lambda (x) 
                                       (my-eval (list lambda `(quote ,x)) bindings bq in-evaluate-p))
                                   tree)))
                      
                         ((eq op :SOME)
                       
                          (let ((tree (my-eval (third expr) bindings bq in-evaluate-p))
                                (lambda (second expr)))

                            (some #'(lambda (x) 
                                      (my-eval (list lambda `(quote ,x)) bindings bq in-evaluate-p))
                                  tree)))
                      
                         ;;; 
                         ;;; progn, prog1, let, let* 
                         ;;;
                      
                         ((eq op :PROGN)
		       
                          (loop as expr in (rest expr)
                                as val = (my-eval expr bindings bq in-evaluate-p)
                                finally (return val)))

                         ((eq op :PROG1)
		       
                          (prog1 
                              (my-eval (second expr) bindings bq in-evaluate-p)
                         
                            (loop as expr in (cddr expr) do (my-eval expr bindings
                                                                     bq in-evaluate-p))))

                         ((eq op :LET)

                          (my-eval (cons 'progn (cddr expr))
                                   (let ((new-bindings bindings))
                                     (loop as (var expr) in (second expr) do
                                           (push (list var (my-eval expr bindings bq in-evaluate-p))
                                                 new-bindings))
                                     new-bindings)
                                   bq 
                                   in-evaluate-p
                                   ))

                         ((eq op :LET*)

                          (my-eval (cons 'progn (cddr expr))
                                   (let ((new-bindings bindings))
                                     (loop as (var expr) in (second expr) do
                                           (push (list var (my-eval expr new-bindings bq in-evaluate-p))
                                                 new-bindings))
                                     new-bindings)
                                   bq 
                                   in-evaluate-p
                                   ))


                         ((eq op :MULTIPLE-VALUE-BIND)
                          
                          (let* ((vars (second expr))
                                 (res-form (third expr))
                                 (body (cdddr expr))

                                 (res (multiple-value-call 
                                          #'list
                                        (my-eval 
                                         res-form
                                         bindings 
                                         bq
                                         in-evaluate-p
                                         ))))
                            
                            (my-eval (cons 'progn body)
                                     (let ((new-bindings bindings))
                                       (mapcar #'(lambda (var val)
                                                   (push (list var val) 
                                                         new-bindings))
                                               vars res)
                                       new-bindings)
                                     bq 
                                     in-evaluate-p
                                     )))

                         ((eq op :VALUES)
                          
                          (let* ((res-forms (cdr expr))

                                 (res (mapcar #'(lambda (res-form)
                                                  (my-eval 
                                                   res-form
                                                   bindings 
                                                   bq
                                                   in-evaluate-p
                                                   ))
                                              res-forms)))

                            (apply #'values res)))

                         ;;;
                         ;;;
                         ;;;
                      
                         ((member op '(:BLET)) ; Unterschied irrelevant, weil Query-Vars keine Nrql-Vars sind
  
                          (loop as (var expr) in (second expr) do
                                (push (list var (my-eval expr bindings bq in-evaluate-p))
                                      *established-bindings*))
                              
                          (unwind-protect 
                              (my-eval (cons 'progn (cddr expr))
                                       bindings
                                       bq in-evaluate-p)

                            (dolist (var (second expr))
                              (declare (ignorable var))
                              (pop *established-bindings*))))
                      
                         ;;;
                         ;;; :lambda 
                         ;;; 

                         ((and (consp op)
                               (eq (upper-keyword (first op)) :LAMBDA))
                       
                          (let ((lambda (first expr))
                                (args (rest expr)))
			 
                            (my-eval `(let (,@(mapcar #'(lambda (var val) 
                                                          (list var val))
                                                      (second lambda)
                                                      args))
                                        (progn ,@(cddr lambda)))
                                     bindings
                                     bq in-evaluate-p)))

                         ;;;
                         ;;; File IO 
                         ;;; 

                         #+:racer-server
                         ((eq op :WITH-OPEN-OUTPUT-FILE-APPEND)
                       
                          (let ((name (my-eval (first (second expr)) bindings bq in-evaluate-p)))

                            (if *unsafe-mode* 
                                (with-open-file (stream name
                                                        :direction :output
                                                        :if-exists :append
                                                        :if-does-not-exist :create)
                                  (setf output-stream stream)
                                  (my-eval `(progn ,@(cddr expr))
                                           (cons (list '*output-stream* 
                                                       stream)
                                                 bindings)
                                           bq in-evaluate-p))
                              (minilisp-error "Can't open file for output, Racer is not running in unsafe mode"))))
                     
                         #+:racer-server
                         ((eq op :WITH-OPEN-OUTPUT-FILE)
                       
                          (let ((name (my-eval (first (second expr)) bindings bq in-evaluate-p)))

                            (if *unsafe-mode* 
                                (with-open-file (stream name
                                                        :direction :output
                                                        :if-exists :supersede
                                                        :if-does-not-exist :create)
                                  (setf output-stream stream)
                                  (my-eval `(progn ,@(cddr expr))
                                           (cons (list '*output-stream* 
                                                       stream)
                                                 bindings)
                                           bq in-evaluate-p))
                              (minilisp-error "Can't open file for output, Racer is not running in unsafe mode"))))
                      
                         ;;;
                         ;;; 
                         ;;; 

                         ((eq op :WITH-NRQL-SETTINGS)
                       
                          (apply #'eval-nrql-settings 
                                 #'(lambda ()
                                     (my-eval `(progn ,@(cddr expr))
                                              bindings
                                              bq in-evaluate-p))
                                 (mapcar #'(lambda (x) 
                                             (my-eval x bindings bq in-evaluate-p))
                                         (transform-args (second expr)))))

                         ;;; 
                         ;;; HTML-Support
                         ;;; 

                         ((eq op :HTML)
                       
                          (let* ((tag-and-args 
                                  (second expr))
                                 (tag (first tag-and-args))
                                 (args (rest tag-and-args))
                                 (body
                                  (cddr expr)))

                            (format output-stream
                                    "<~A" (string-downcase (symbol-name tag)))

                            (loop while args do
                                  (let ((key (pop args))
                                        (val (pop args)))
                                    (format output-stream
                                            " ~A=\"~A\"" 
                                            (string-downcase (symbol-name key)) 
                                            val)))

                            (format output-stream ">~%")

                            (my-eval `(progn ,@body)
                                     bindings
                                     bq
                                     in-evaluate-p
                                     )

                            (format output-stream
                                    "~%</~A>" (string-downcase (symbol-name tag))) nil))

                         ((eq op :CONTENT)
                       
                          (format output-stream "~A" 
                                  (my-eval (second expr) bindings bq in-evaluate-p))

                          (second expr))

                         ((eq op :WITH-HTML) 

                          (my-eval `(with-open-output-file ,(second expr)

                                                           (content "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"	\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
                                                           (format *output-stream* "~%")
                                                           (html (html :xmlns "http://www.w3.org/1999/xhtml")
                                                                 ,@(cddr expr)))
                                   bindings
                                   bq 
                                   in-evaluate-p 
                                   ))
                      
                         ;;;
                         ;;; CL-Funktion 
                         ;;; 

                         (t 

                          (cond ((gethash (to-keyword-big op) +allowed-cl-functions+)
                                 
                                 (let ((op
                                        (gethash (to-keyword-big op) +allowed-cl-functions+)))
			     
                                   (let* ((arity (first op))
					  (fn-name 
					   #+:mlisp
					   (intern (string-downcase (symbol-name (first expr)))
						   (symbol-package (first expr)))
					   #-:mlisp
					   (first expr)
                                           )
                                          (fn (or (third op) 
                                                  (and (fboundp fn-name)
                                                       (symbol-function fn-name))))
                                          (args (transform-args (cdr expr)))
                                          (min (when (consp arity)
                                                 (first arity)))
                                          (max (when (consp arity)
                                                 (second arity)))
                                          (macro-p (macro-function 
						    (change-package-of-description fn-name :racer))))
						    
                                     (cond ((not fn)
                                            (minilisp-error
                                             "Unkown function ~A" fn-name))
                                           
                                           ((and arity 
                                                 (not (consp arity))
                                                 (not (eq arity 'n))
                                                 (not (= (length args) arity)))
                                             
                                            (minilisp-error
                                             "Function ~A expects ~A arguments, was given ~A arguments" fn-name arity (length args)))

                                           ((and min 
                                                 (< (length args) min))
                                  
                                            (minilisp-error
                                             "Function ~A expects at least ~A arguments, was given only ~A arguments" fn-name
                                             min (length args)))


                                           ((and max 
                                                 (> (length args) max))

                                            (minilisp-error
                                             "Function ~A expects at most ~A arguments, but was given ~A arguments" fn-name 
                                             max (length args))) 
 
                                           (t
                      
                                            (cond (macro-p 

                                                   ;; Makro? geht nur fuer mit nrql-defmacro definierte!  
                                                   ;; assoziierter Funktion wird benoetigt: 

                                                   (let ((fn (get-function-for-macro fn-name)))
				     
                                                     (if (not fn)

                                                         (minilisp-error 
                                                          "Cannot find corresponding function for macro ~A! 
Please look up the corresponding function from the manual (e.g., \"add-concept-assertion\" instead of \"instance\")  
" fn-name) 

                                                       (let* ((fn (symbol-function fn))
                                                              (args 
                                                               (mapcar #'(lambda (x)
                                                                           (my-eval x bindings bq in-evaluate-p)) 
                                                                       args)))

                                                         (handler-case 
                                                             (let ((*disable-deadlock-checking* t)
                                                                   (*multiprocess-queries* nil))

                                                               (if defer-p
                                                                   `((:lambda () 
                                                                       (,(first expr) 
                                                                        ,@(mapcar #'(lambda (x) 
                                                                                      `(quote ,x))
                                                                                  args)))
                                                                     ())
								 (apply fn args)))
                                                           (error (error)
                                                             (minilisp-error "Bad function call: ~S, Error: ~A" 
                                                                             (cons fn-name args)
                                                                             error)))))))

                                                  (t 
					 
                                                   (let* ((args 
                                                           (mapcar #'(lambda (x) 
                                                                       (my-eval x bindings bq in-evaluate-p))
                                                                   args)))

                                                     (when *debug-evaluator-p*
                                                       (format t "~%Applying ~A to ~A...~%"
                                                               fn args))

                                                     (handler-case 
                                                         (let ((*disable-deadlock-checking* t)
                                                               (*multiprocess-queries* nil))
                                                           (if defer-p
                                                               `((:lambda () 
                                                                   (,(first expr) 
                                                                    ,@(mapcar #'(lambda (x) 
                                                                                  `(quote ,x))
                                                                              args)))
                                                                 ())
							     (apply fn args)))
                                                       (error (error)
                                                         (minilisp-error "Bad function call: ~S, Error: ~A" 
                                                                         (cons fn-name args)
                                                                         error)))))))))))

                                (t

                                 (when (get-minilisp-function op)
                                   
                                   ;;; gibt error falls nicht bekannt!
				   
				   (let* ((args (transform-args (rest expr)))
					  (args 
					   (mapcar #'(lambda (x) 
						       (my-eval x bindings bq in-evaluate-p))
						   args))

                                          (special-bindings (remove-if-not #'(lambda (x) 
                                                                               (minilisp-value-p (first x)))
                                                                           bindings))
                                          (vals (mapcar #'(lambda (x) 
                                                            (list (first x)
                                                                  (second x)
                                                                  (get-minilisp-value (first x))))
                                                        special-bindings)))

                                     (mapc #'(lambda (var-val-special-val)
                                               (register-value (first var-val-special-val) 
                                                               (second var-val-special-val)))
                                           vals)

                                     (prog1 
                                         (apply #'call-function op args)
                                       (mapc #'(lambda (var-val-special-val)
                                                 (register-value (first var-val-special-val) 
                                                                 (third var-val-special-val)))
                                             vals))))))))))
                 
	       
                     (t 

                      ;;;
                      ;;; Literale, Variablen 
                      ;;; 

                      (if (not (zerop bq))
                          expr
                        (process-arg expr bindings))))))


      (my-eval body
               (cons (list '*current-abox* *running-abox*)
                     (cons (list '*current-tbox* *running-tbox*)
                           global-bindings))
               0
               nil
               nil))))

        
