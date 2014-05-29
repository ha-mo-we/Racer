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

(declaim (special +category-for-function+
                  +super-sections+
                  +known-args-and-types+
                  +cur-server-functions+
                  +function-for-macro+))

(defvar *all-args* nil)

(defvar *all-racer-functions* nil)

(defvar *missing-argument-types* nil)

(defvar *missing-type-descriptions* nil)

(defvar *unknown-macros-for-functions* nil)

(defvar *unknown-functions-for-macros* nil)

(defvar *unknown-return-types* nil)

(defvar *tex-rewrite*
  (mapcar #'(lambda (x) (list (first x) 
                              (precompile-regexp (first x))
                              (second x)
                              (third x)))
          '(("\\\\&optional" "&optional")
            ("\\\\&rest" "&rest")
            ("\\\\&args" "&args")

            ("{\\\\tt[^{}\\]*}" "@ic{[^{}\\]*}" "[^{}\\]*")            
            ("\\\\code{[^{}\\]*}" "@ic{[^{}\\]*}" "[^{}\\]*")
            ("\\\\emph{[^{}\\]*}" "@em{[^{}\\]*}" "[^{}\\]*")
            
            ("\\\\funref{[^{}\\]*}" "@funref{[^{}\\]*}" "[^{}\\]*")
            ("\\\\argument{[^{}\\]*}"  "@arg{[^{}\\]*}" "[^{}\\]*"))))

;;;
;;;
;;; 

(defun stringsubst-char-with-string (orgstring char string)
  (let ((pos (position char orgstring)))
    (if pos
	(concatenate 'string
                     (subseq orgstring 0 pos)
                     string
                     (stringsubst-char-with-string (subseq orgstring (1+ pos))
                                                   char string))
      orgstring)))

;;;
;;; TeX Doc
;;; 

(defvar *doc-structure* 
  '((:basic-commands "Basic Commands")
    (:query-management "Query Management")
    (:rule-management "Rule Management")
    (:query-lifecycle "Query Life Cycle")
    (:rule-lifecycle "Rule Life Cycle")
    (:execution-control "Execution Control")
    (:abox-queries "ABox Queries")
    (:tbox-queries "TBox Queries")
    (:getting-answers "Getting Answers")
    (:defined-queries "Defined Queries")
    (:rules "Rules")
    (:querying-modes "Querying Modes")
    (:inference "Inference") 
    (:query-repository "Query Repository")
    (:substrate-layer "The Substrate Representation Layer")
    (:persistency-facility "The nRQL Persistency Facility")))

(defun create-documentation ()
  (with-open-file (stream "nrql:nrql-doc.tex"
                          :direction :output 
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (let ((*print-case* :downcase)
          (*print-escape* t)
          (*package* (find-package :ts))
	  
          (fns (append ts::*nrql-functions*
                       ts::*nrql-methods*))
          (macros (append ts::*nrql-macros*
                          ts::*nrql-with-macros*))

          (assoc-fn nil))

      (format stream "~%\\newcommand{\\argument}[1]{{\\tt #1}}~%")

      (labels ((empty ()
                 (format stream "~%{}"))

               (texify (symbol)
                 (stringsubst-char-with-string 
                  (format nil "~A" symbol)
                  #\$
                  "\\$"))

               (doc-entry (fn macro-p lambda doc)

                 (declare (ignorable lambda))
		 
                 (let ((doc-type (second (assoc :doc-type doc))) ; :short, :long 
                       (description (second (assoc :description doc))) ; string 
                       (syntax (second (assoc :syntax doc))) ; sexpr / lambda list 
                       (arguments (rest (assoc :arguments doc))) ; list of arg. descr. 
                       (values (rest (assoc :values doc))) ; possible return values
                       (remarks (second (assoc :remarks doc))) ; string
                       (examples (rest (assoc :examples doc))) ; list of sexpr's 
                       (see-also (rest (assoc :see-also doc))) ; list of funrefs 
                       (rule-equivalent-of (second (assoc :rule-equivalent-of doc)))
                       (inverse-of (second (assoc :inverse-of doc)))
                       (synonym (second (assoc :synonym doc)))
                       (corresponding-p (if (assoc :show-corresponding-p doc)
                                            (second (assoc :show-corresponding-p doc))
                                          t)))

                   (unless doc-type 
                     (error "No doc-type for ~A" fn))

                   (ecase doc-type

                     (:short 

                      (format stream "~%~%\\shortfundoc{~A}" 
                              (texify fn))

                      (format stream "~%\%what")
                      (if macro-p 
                          (format stream "~%{Macro}")
                        (format stream "~%{Function}"))
                      
                      (format stream "~%\%Description")
                      
                      (if (and (not description)
                               (not (assoc fn assoc-fn))
                               (not (second (assoc fn assoc-fn)))
                               (not synonym)
                               (not inverse-of)
                               (not rule-equivalent-of))
                          (format stream "~%{")
                        (progn

                          (format stream "~%{")
                          
                          (when description
                            (format stream "~A. " description))
                          
                          (when (and corresponding-p (second (assoc fn assoc-fn)))
                            (format stream "See also corresponding ~A \\funref{~A}." 
				    (if macro-p 
					"function" 
				      "macro")
				    (texify (second (assoc fn assoc-fn)))))
			  
                          (when synonym 
                            (format stream " Synonym for \\funref{~A}."
                                    (texify synonym)))

                          (when inverse-of 
                            (format stream " Inverse of \\funref{~A}."
                                    (texify inverse-of)))

                          (when rule-equivalent-of
                            (format stream " This is the rule equivalent of \\funref{~A}."
                                    (texify rule-equivalent-of)))))
                      
                      (format stream "~%}"))

                     (:long

                      (format stream "~%~%\\fundoc{~A}" 
                              (texify fn))

                      (format stream "~%\%what")
                      (if macro-p 
                          (format stream "~%{Macro}")
                        (format stream "~%{Function}"))

                      (format stream "~%\%Description")
                      (unless description 
                        (error "No description for ~A!" fn))                 

                      (if (and corresponding-p (second (assoc fn assoc-fn)))
                          (format stream "~%{~A See also corresponding ~A \\funref{~A}." 
                                  description
                                  (if macro-p 
                                      "function" 
                                    "macro")
                                  (texify (second (assoc fn assoc-fn))))
                        (format stream "~%{~A" description))

                      (when synonym 
                        (format stream " Synonym for \\funref{~A}."
                                (texify synonym)))

                      
                      (when inverse-of 
                        (format stream " Inverse of \\funref{~A}."
                                (texify inverse-of)))
                      
                      (when rule-equivalent-of
                        (format stream " This is the rule equivalent of \\funref{~A}."
                                (texify rule-equivalent-of)))

                      (format stream "~%}")

                      (format stream "~%\%Syntax")
                      (unless syntax 
                        (error "No syntax for ~A!" fn))                 
                      (format stream "~%{{\\tt ~A}}" 
                              (stringsubst-char-with-string 
                               (format nil "~A" syntax)
                               #\&
                               "\\&"))
		      
                      (format stream "~%\%Arguments")

                      (if (not arguments)
                          (progn 
                            (format stream "~%{ \\item[] }")
					;(empty)
                            )
                        (progn
			  ;;(format stream "~%{\\begin{itemize}")
                          (format stream "~%{")
                          (loop as (type name default description) 
                                in 
                                arguments 
                                do
                                (progn 
				  
                                  (case type
                                    (:comment  
                                     (format stream 
                                             "~%\\item[other arguments ~A]"
                                             "\\hfill -~")
                                     (setf description name))
                                    (:reference 
                                     (format stream 
                                             "~%\\item[~{ \\argument{~A}~} ~A]"
                                             (ensure-list name) "\\hfill -~")
                                     (setf description 
                                           (format nil "see ~{ \\funref{~A}~}" 
                                                   (mapcar #'texify (ensure-list default)))))
                                    ((:key :arg :body :rest :args)
                                     (if (not (eq default :unspecified))
                                         (typecase default
                                           (string
                                            (format stream "~%\\item[\\argument{~A}, default \\textit{~A} ~A]" 
                                                    name default "\\hfill -~"))
                                           (otherwise
                                            (format stream "~%\\item[\\argument{~A}, default {\\tt ~A} ~A]" 
                                                    name default "\\hfill -~")))
				       
                                       (format stream "~%\\item[\\argument{~A} ~A]" name "\\hfill -~")))

                                    (otherwise 
				     (setf description default)
				     
				     (format stream "~%\\item[\\argument{~A} ~A]" name "\\hfill -~")))
                                  
                                  (format stream "~%\~A." description)))
			  ;;(format stream "~%\\end{itemize}")
                          (format stream "~%}")))

                      (format stream "~%\%Values")
                      (if (not values)
                          (empty)
                        (progn 
                          (format stream "~%{")
                          (loop as vals on values do 
                                (let ((val (first vals)))
                                  (if (stringp val)
                                      (format stream "~%~A" val)
                                    (if (cdr vals )
                                        (format stream "~%{\\tt ~S} or" val)
                                      (format stream "~%{\\tt ~S}" val)))))
                          (format stream ".~%}")))

		      
                      (format stream "~%\%Remarks")
                      (if (not remarks)
                          (empty)
                        (progn
                          (format stream "~%{")
                          (when remarks
                            (format stream "~%~A." remarks))
                          (format stream "~%}")))
		      
                      (format stream "~%\%Examples")
                      (if (not examples)
                          (empty)
                        (progn
			  ;;(format stream "~%{\\begin{verbatim}")
                          (format stream "~%{\\tt")
                          (loop as ex on examples do
                                (pprint (first ex) stream)
                                (when (cdr ex)
                                  (format stream "\\\\")))
			  ;;(format stream "~%\\end{verbatim}}")
                          (format stream "~%}")))
		      
                      (format stream "~%\%See also")
                      
                      (if (and (not see-also) (not (second (assoc fn assoc-fn))))
                          (empty)
                        (progn

                          (format stream "~%{")

                          (when (and corresponding-p (second (assoc fn assoc-fn)))
                            (format stream "Corresponding ~A: \\funref{~A}. "
                                    (if macro-p "function" "macro")
                                    (texify (second (assoc fn assoc-fn)))))

                          (when rule-equivalent-of 
                            (format stream "Rule equivalent of \\funref{~A}. "
                                    (texify rule-equivalent-of)))


                          (when inverse-of 
                            (format stream " Inverse of \\funref{~A}."
                                    (texify inverse-of)))
			  
                          (loop as see-also on see-also do
                                (let ((fn (first see-also)))
                                  (if (cdr see-also)
                                      (format stream "\\funref{~A}, " (texify fn))
                                    (format stream "\\funref{~A}" (texify fn)))))
                          (format stream "~%}"))))))))
        
        (let ((processed nil))

          (dolist (fn macros)
            (push (list (first fn) (second fn)) assoc-fn)
            (push (list (second fn) (first fn)) assoc-fn))

          (loop as (key title) in *doc-structure*
                do 

                (format stream "~%~%~%\\newpage")
                (format stream "~%\\section{~A}" title)

                (format t "~%Chapter ~A" title)

                (let ((fns
                       (sort 
                        (remove-if-not #'(lambda (x) 
                                           (eq (second (assoc :category (third x))) key))
                                       fns)
                        #'string-lessp
                        :key #'(lambda (x) 
                                 (symbol-name (first x)))))

                      (macros 
                       (sort
                        (remove-if-not #'(lambda (x) 
                                           (eq (second (assoc :category (third x))) key))
                                       macros)
                        #'string-lessp
                        :key #'(lambda (x)
                                 (symbol-name (first x))))))
		  
                  (loop as (fn lambda doc) in fns 
                        do
                        (when doc 
                          (format t "~%    Function / Method ~A" fn)
                          
                          (push fn processed)
                          (doc-entry fn nil lambda doc)))

                  (loop as (fn assoc-fn doc) in macros 
                        do
                        (when doc 
                          (format t "~%    Macro ~A" fn)
                          
                          (push fn processed)
                          (doc-entry fn t
                                     ;;; lambda von der assoziierten Funktion holen
                                     (second (assoc assoc-fn fns))
                                     doc)))))

          (let ((unclassified (set-difference 
                               (mapcar #'first (append fns macros))
                               processed)))

            (when unclassified
              (format stream "~%~%~% \\newpage ~%~%*WARNING* Unclassified: ~A" 
                      (mapcar #'texify unclassified))
              (format t "~%~%*WARNING* Unclassified: ~A" unclassified))))))))

;;;
;;; Function Categories
;;;

(defun get-updated-category-for-function-list ()
  (create-docgen-stubs t)
  (set-difference 
   (mapcar #'(lambda (x) 
               (or (assoc x +category-for-function+)
                   (progn 
                     (format t "~%Adding new entry: ~S" (list x :unknown))
                     (list x :unknown))))
           (reverse *all-racer-functions*))
   +category-for-function+ 
   :test #'equalp))

(defun check-category-for-function-list ()
  (let ((sections (mapcar #'first +super-sections+)))
    (remove-if #'(lambda (x) 
                   (or (member :ignore (cdr x))
                       (subsetp (cdr x) sections)))
               +category-for-function+)))

(defconstant +category-for-function+ 
  ;;; update: 2012-02-07
  '((XML-READ-TBOX-FILE :IO :TBOX-MANAGEMENT)
    (XML-OUTPUT :GENERAL :IO)
    (XML-NATIVE-OUTPUT :GENERAL :IO)
    (XML-INPUT :GENERAL :IO)
    (VERIFY-WITH-CONCEPT-TREE-LIST :TESTING)
    (VERIFY-WITH-ABOX-INDIVIDUALS-LIST :TESTING)
    (USE-TRIPLE-STORE :AGRAPH)
    (UNSUBSCRIBE-1 :PUBLISH-SUBSCRIBE)
    (UNPUBLISH-1 :PUBLISH-SUBSCRIBE)
    (TRIPLE-STORE-READ-FILE :AGRAPH :IO)
    (TRIPLE-STORE-OPEN-P :AGRAPH)
    (TRIPLE-STORE-GRAPHS :AGRAPH)
    (TRANSMIT-FILE :IO)
    (TRANSITIVE-P :TBOX-ASK)
    (TOLD-VALUE :ABOX-CD-TELL)
    (TIMENET-ANSWER-QUERY :EVENTS)
    (TBOX-PREPARED-P :TBOX-ASK)
    (TBOX-CYCLIC-P :TBOX-ASK)
    (TBOX-COHERENT-P :TBOX-ASK)
    (TBOX-CLASSIFIED-P :TBOX-ASK)
    (TAXONOMY :TBOX-ASK)
    (SYMMETRIC-P :TBOX-ASK)
    (SWRL-FORWARD-CHAINING :SWRL)
    (SUBSCRIBE-1 :PUBLISH-SUBSCRIBE)
    (STORE-TBOXES-IMAGE :IO)
    (STORE-TBOX-IMAGE :IO)
    (STORE-KBS-IMAGE :IO)
    (STORE-KB-IMAGE :IO)
    (STORE-ABOXES-IMAGE :IO)
    (STORE-ABOX-IMAGE :IO)
    (SET-UNIQUE-NAME-ASSUMPTION :ABOX-MANAGEMENT)
    (SET-SERVER-TIMEOUT :GENERAL)
    (SET-FIND-TBOX :TBOX-MANAGEMENT)
    (SET-FIND-ABOX :ABOX-MANAGEMENT)
    (SET-CURRENT-TBOX :TBOX-MANAGEMENT)
    (SET-CURRENT-ABOX :ABOX-MANAGEMENT)
    (SET-ATTRIBUTE-FILLER :ABOX-CD-TELL)
    (SERVER-CASE :ignore) 
    (SEQUENCE :GENERAL)
    (SAVE-TBOX :IO :TBOX-MANAGEMENT)
    (SAVE-ONTOLOGY-TO-TRIPLE-STORE :AGRAPH :IO :OWL-INTERFACE)
    (SAVE-KB :IO :KB-MANAGEMENT)
    (SAVE-ABOX :IO :ABOX-MANAGEMENT)
    (ROLES-EQUIVALENT-1 :TBOX-TELL)
    (ROLES-DISJOINT-1 :TBOX-TELL)
    (ROLE-USED-AS-DATATYPE-PROPERTY-P :TBOX-OWL-ASK)
    (ROLE-USED-AS-ANNOTATION-PROPERTY-P :TBOX-OWL-ASK)
    (ROLE-SUBSUMES-P :TBOX-ASK)
    (ROLE-SATISFIABLE-P :TBOX-ASK)
    (ROLE-P :TBOX-ASK)
    (ROLE-IS-USED-AS-DATATYPE-PROPERTY :TBOX-OWL-TELL)
    (ROLE-IS-USED-AS-ANNOTATION-PROPERTY :TBOX-OWL-TELL)
    (ROLE-IS-TRANSITIVE :TBOX-TELL)
    (ROLE-IS-SYMMETRIC :TBOX-TELL)
    (ROLE-IS-REFLEXIVE :TBOX-TELL)
    (ROLE-IS-IRREFLEXIVE :TBOX-TELL)
    (ROLE-IS-FUNCTIONAL :TBOX-TELL)
    (ROLE-IS-ASYMMETRIC :TBOX-TELL)
    (ROLE-HAS-RANGE :TBOX-TELL)
    (ROLE-HAS-PARENT :TBOX-TELL)
    (ROLE-HAS-DOMAIN :TBOX-TELL)
    (ROLE-EQUIVALENT-P :TBOX-ASK)
    (ROLE-DISJOINT-P :TBOX-ASK)
    (RETRIEVE-RELATED-INDIVIDUALS :ABOX-ASK)
    (RETRIEVE-INDIVIDUAL-TOLD-DATATYPE-FILLERS :ABOX-OWL-ASK)
    (RETRIEVE-INDIVIDUAL-TOLD-ATTRIBUTE-VALUE :ABOX-CD-ASK)
    (RETRIEVE-INDIVIDUAL-SYNONYMS :ABOX-ASK)
    (RETRIEVE-INDIVIDUAL-FILLERS :ABOX-ASK)
    (RETRIEVE-INDIVIDUAL-FILLED-ROLES :ABOX-ASK)
    (RETRIEVE-INDIVIDUAL-ATTRIBUTE-FILLERS :ABOX-CD-ASK)
    (RETRIEVE-INDIVIDUAL-ANTONYMS :ABOX-ASK)
    (RETRIEVE-INDIVIDUAL-ANNOTATION-PROPERTY-FILLERS :ABOX-CD-ASK)
    (RETRIEVE-DIRECT-PREDECESSORS :ABOX-ASK)
    (RETRIEVE-CONCEPT-INSTANCES :ABOX-ASK)
    (RESTORE-TBOXES-IMAGE :IO :PERSISTENCE)
    (RESTORE-TBOX-IMAGE :IO :PERSISTENCE)
    (RESTORE-KBS-IMAGE :IO :PERSISTENCE)
    (RESTORE-KB-IMAGE :IO :PERSISTENCE)
    (RESTORE-ABOXES-IMAGE :IO :PERSISTENCE)
    (RESTORE-ABOX-IMAGE :IO :PERSISTENCE)
    (REFLEXIVE-P :TBOX-ASK)
    (RECOGNIZE-EVENTS :EVENTS)
    (REALIZE-ABOX :ABOX-MANAGEMENT)
    (RDFS-READ-TBOX-FILE :IO :TBOX-MANAGEMENT)
    (RACER-READ-FILE :IO :KB-MANAGEMENT)
    (RACER-READ-DOCUMENT :IO :KB-MANAGEMENT)
    (RACER-ANSWER-QUERY-WITH-EXPLANATION :NRQL-ABOX)
    (QUIET-SEQUENCE :GENERAL)
    (PUBLISH-FILE :PUBLISH-SUBSCRIBE)
    (PUBLISH-1 :PUBLISH-SUBSCRIBE)
    (PRINT-TBOX-TREE :TBOX-ASK)
    (PRINT-ABOX-INDIVIDUALS :ABOX-ASK)
    (PREPARE-RACER-ENGINE :NRQL-ABOX)
    (PREPARE-ABOX :ABOX-MANAGEMENT)
    (PRACER-ANSWER-QUERY :AGRAPH)
    (OWLLINK-READ-FILE :OWLLINK :IO)
    (OWLLINK-READ-DOCUMENT :OWLLINK :IO)

    (OWLAPI-SEQUENCE :ignore)
    (|OWLAPI-sequence| :owlapi-tell)
    (|OWLAPI-quietSequence| :owlapi-tell)
    (OWLAPI-QUIET-SEQUENCE :ignore)
    (|OWLAPI-getLastOutputStreamString| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getLastAnswer| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getIDsOfLastAnswer| :OWLAPI-MANAGEMENT)
    (|OWLAPI-answerSequence| :owlapi-tell)
    (OWLAPI-ANSWER-SEQUENCE :ignore)
    (OWL-READ-FILE :OWL-INTERFACE :IO)
    (OWL-READ-DOCUMENT :OWL-INTERFACE :IO)
    (OPEN-TRIPLE-STORE :AGRAPH)
    (MSC-K :TBOX-ASK)
    (MOST-SPECIFIC-INSTANTIATORS :ABOX-ASK)
    (MIRROR :OWL-INTERFACE)
    (MATERIALIZE-INFERENCES :AGRAPH)
    (LOGGING-ON :GENERAL)
    (LOGGING-OFF :GENERAL)
    (LCS-UNFOLD :TBOX-ASK)
    (LCS :TBOX-ASK)
    (KB-ONTOLOGIES :TBOX-OWL-ASK :OWL-INTERFACE)
    (IRREFLEXIVE-P :TBOX-ASK)
    (INVERSE-OF-ROLE :TBOX-ASK)
    (INVERSE-FEATURE-P :TBOX-ASK)
    (INTERNAL-INDIVIDUALS-RELATED-P :ABOX-ASK)
    (INSTANTIATORS :ABOX-ASK)
    (INIT-TBOX :TBOX-MANAGEMENT)
    (INIT-SUBSCRIPTIONS-1 :PUBLISH-SUBSCRIBE)
    (INIT-PUBLICATIONS-1 :PUBLISH-SUBSCRIBE)
    (INIT-ABOX :ABOX-MANAGEMENT)
    (INDIVIDUALS-RELATED-P :ABOX-ASK)
    (INDIVIDUALS-NOT-EQUAL-P :ABOX-ASK)
    (INDIVIDUALS-EQUAL-P :ABOX-ASK)
    (INDIVIDUAL-P :ABOX-ASK)
    (INDIVIDUAL-INSTANCE-P :ABOX-ASK)
    (INDEX-ALL-TRIPLES :AGRAPH)
    (INCLUDE-KB :TBOX-MANAGEMENT)
    (GET-TBOX-VERSION :TBOX-ASK)
    (GET-TBOX-SIGNATURE :TBOX-ASK)
    (GET-TBOX-LANGUAGE :TBOX-ASK)
    (GET-SERVER-TIMEOUT :GENERAL)
    (GET-ROLE-DATATYPE :TBOX-OWL-ASK)
    (GET-RACER-VERSION :updates)
    (GET-OBJECT-BOTTOM-ROLE :TBOX-OWL-ASK)
    (GET-NAMESPACE-PREFIXES :OWL-INTERFACE :GENERAL :IO)
    (GET-NAMESPACE-PREFIX :OWL-INTERFACE)
    (GET-META-CONSTRAINT :TBOX-ASK)
    (GET-KB-SIGNATURE :KB-MANAGEMENT)
    (GET-INDIVIDUAL-PMODEL :ABOX-ASK)
    (GET-DATA-BOTTOM-ROLE :TBOX-OWL-ASK)
    (GET-CONCEPT-PMODEL :TBOX-ASK)
    (GET-CONCEPT-NEGATED-DEFINITION-1 :TBOX-ASK)
    (GET-CONCEPT-DEFINITION-1 :TBOX-ASK)
    (GET-BUILD-VERSION :GENERAL)
    (GET-ABOX-VERSION :ABOX-ASK)
    (GET-ABOX-SIGNATURE :ABOX-ASK)
    (GET-ABOX-LANGUAGE :ABOX-ASK)
    (FORGET-TBOX :TBOX-MANAGEMENT)
    (FORGET-STATEMENT :KB-MANAGEMENT)
    (FORGET-SAME-INDIVIDUAL-AS-ASSERTION :ABOX-FORGET)
    (FORGET-ROLE-AXIOMS :TBOX-FORGET)
    (FORGET-ROLE-ASSERTION :ABOX-FORGET)
    (FORGET-NEGATIVE-DATATYPE-ROLE-FILLER :ABOX-OWL-FORGET)
    (FORGET-NEGATED-ROLE-ASSERTION :ABOX-FORGET)
    (FORGET-INDIVIDUAL :ABOX-FORGET)
    (FORGET-DISJOINTNESS-AXIOM-STATEMENT :TBOX-FORGET)
    (FORGET-DISJOINTNESS-AXIOM :TBOX-FORGET)
    (FORGET-DIFFERENT-FROM-ASSERTION :ABOX-FORGET)
    (FORGET-DATATYPE-ROLE-FILLER :ABOX-OWL-FORGET)
    (FORGET-CONSTRAINT :ABOX-CD-FORGET)
    (FORGET-CONSTRAINED-ASSERTION :ABOX-CD-FORGET)
    (FORGET-CONCEPT-AXIOM :TBOX-FORGET)
    (FORGET-CONCEPT-ASSERTION :ABOX-FORGET)
    (FORGET-ANNOTATION-CONCEPT-ASSERTION :ABOX-FORGET)
    (FORGET-ALL-DIFFERENT-ASSERTION :ABOX-FORGET)
    (FORGET-ABOX :ABOX-MANAGEMENT)
    (FIND-TBOX :TBOX-MANAGEMENT)
    (FIND-ABOX :ABOX-MANAGEMENT)
    (FEATURE-P :TBOX-ASK)
    (ENSURE-TBOX-SIGNATURE :TBOX-MANAGEMENT)
    (ENSURE-SUBSUMPTION-BASED-QUERY-ANSWERING :ABOX-MANAGEMENT)
    (ENSURE-SMALL-TBOXES :TBOX-MANAGEMENT)
    (ENSURE-ABOX-SIGNATURE :ABOX-MANAGEMENT)
    (ENABLE-OPTIMIZED-QUERY-PROCESSING :QUERY-OPTIMIZER)
    (ENABLE-ALISP-COMPATIBILITY-MODE :GENERAL :IO)
    (DISABLE-ALISP-COMPATIBILITY-MODE :GENERAL :IO)
    (DIG-READ-FILE :DIG :IO)
    (DIG-READ-DOCUMENT :DIG :IO)
    (DESCRIBE-TBOX :TBOX-ASK)
    (DESCRIBE-ROLE :TBOX-ASK)
    (DESCRIBE-INDIVIDUAL1 :ABOX-ASK)
    (DESCRIBE-INDIVIDUAL :ABOX-ASK)
    (DESCRIBE-CONCEPT :TBOX-ASK)
    (DESCRIBE-ABOX :ABOX-ASK)
    (DELETE-ALL-TBOXES :TBOX-MANAGEMENT)
    (DELETE-ALL-ABOXES :ABOX-MANAGEMENT)
    (DECLARE-DISJOINT :TBOX-TELL)
    (DECLARE-CURRENT-KNOWLEDGE-BASES-AS-PERSISTENT :KB-MANAGEMENT)
    (DATATYPE-ROLE-RANGE :TBOX-OWL-TELL)
    (DATATYPE-ROLE-HAS-RANGE :TBOX-OWL-TELL)
    (CURRENT-TBOX :TBOX-MANAGEMENT)
    (CURRENT-ABOX :ABOX-MANAGEMENT)
    (CREATE-TRIPLE-STORE :AGRAPH)
    (CREATE-TBOX-INTERNAL-MARKER-CONCEPT :TBOX-MANAGEMENT)
    (CREATE-TBOX-CLONE :TBOX-MANAGEMENT)
    (CREATE-ABOX-CLONE :ABOX-MANAGEMENT)
    (CONVERT-EVENT-SPECS :EVENTS)
    (CONSTRAINT-ENTAILED-P :ABOX-CD-ASK)
    (CONCEPT-SUBSUMES-P :TBOX-ASK)
    (CONCEPT-SATISFIABLE-P :TBOX-ASK)
    (CONCEPT-P :TBOX-ASK)
    (CONCEPT-IS-PRIMITIVE-P :TBOX-ASK)
    (CONCEPT-EQUIVALENT-P :TBOX-ASK)
    (CONCEPT-DISJOINT-P :TBOX-ASK)
    (COMPUTE-INDEX-FOR-INSTANCE-RETRIEVAL :ABOX-MANAGEMENT)
    (COMPUTE-IMPLICIT-ROLE-FILLERS :ABOX-MANAGEMENT)
    (COMPUTE-ALL-IMPLICIT-ROLE-FILLERS :ABOX-MANAGEMENT)
    (CLOSE-TRIPLE-STORE :AGRAPH)
    (CLEAR-DEFAULT-TBOX :TBOX-MANAGEMENT)
    (CLASSIFY-TBOX :TBOX-MANAGEMENT)
    (CHECK-TBOX-COHERENCE :TBOX-ASK)
    (CHECK-SUBSCRIPTIONS :PUBLISH-SUBSCRIBE)
    (CHECK-ABOX-COHERENCE :ABOX-ASK)
    (CD-OBJECT-P :ABOX-CD-ASK)
    (CD-ATTRIBUTE-P :ABOX-CD-ASK)
    (ATTRIBUTE-TYPE :ABOX-CD-ASK)
    (ATTRIBUTE-DOMAIN-1 :ABOX-CD-ASK)
    (ATOMIC-ROLE-SYNONYMS :TBOX-ASK)
    (ATOMIC-ROLE-RANGE :TBOX-ASK)
    (ATOMIC-ROLE-PARENTS :TBOX-ASK)
    (ATOMIC-ROLE-INVERSE :TBOX-ASK)
    (ATOMIC-ROLE-DOMAIN :TBOX-ASK)
    (ATOMIC-ROLE-DESCENDANTS :TBOX-ASK)
    (ATOMIC-ROLE-CHILDREN :TBOX-ASK)
    (ATOMIC-ROLE-ANCESTORS :TBOX-ASK)
    (ATOMIC-CONCEPT-SYNONYMS :TBOX-ASK)
    (ATOMIC-CONCEPT-PARENTS :TBOX-ASK)
    (ATOMIC-CONCEPT-DESCENDANTS :TBOX-ASK)
    (ATOMIC-CONCEPT-CHILDREN :TBOX-ASK)
    (ATOMIC-CONCEPT-ANCESTORS :TBOX-ASK)
    (ASYMMETRIC-P :TBOX-ASK)
    (ASSOCIATED-TBOX :ABOX-ASK)
    (ASSOCIATED-ABOXES :TBOX-ASK)
    (ANSWER-SEQUENCE :GENERAL :IO)
    (ALL-TRANSITIVE-ROLES :TBOX-ASK)
    (ALL-TBOXES :TBOX-MANAGEMENT)
    (ALL-ROLES :TBOX-ASK)
    (ALL-ROLE-ASSERTIONS-FOR-INDIVIDUAL-IN-RANGE :ABOX-ASK)
    (ALL-ROLE-ASSERTIONS-FOR-INDIVIDUAL-IN-DOMAIN :ABOX-ASK)
    (ALL-ROLE-ASSERTIONS :ABOX-ASK)
    (ALL-INDIVIDUALS :TBOX-ASK)
    (ALL-FEATURES :TBOX-ASK)
    (ALL-EQUIVALENT-CONCEPTS :TBOX-ASK)
    (ALL-CONSTRAINTS :ABOX-CD-ASK)
    (ALL-CONCEPT-ASSERTIONS-FOR-INDIVIDUAL :ABOX-ASK)
    (ALL-CONCEPT-ASSERTIONS :ABOX-ASK)
    (ALL-ATTRIBUTES :TBOX-CD-ASK)
    (ALL-ATTRIBUTE-ASSERTIONS :ABOX-CD-ASK)
    (ALL-ATOMIC-CONCEPTS :TBOX-ASK)
    (ALL-ANNOTATION-ROLE-ASSERTIONS :TBOX-OWL-ASK)
    (ALL-ANNOTATION-CONCEPT-ASSERTIONS :ABOX-OWL-ASK)
    (ALL-ABOXES :ABOX-MANAGEMENT)
    (ALC-CONCEPT-COHERENT :TBOX-ASK)
    (ADD-SAME-INDIVIDUAL-AS-ASSERTION :ABOX-TELL)
    (ADD-RULE-AXIOM :RULE-MANAGEMENT)
    (ADD-ROLE-AXIOMS :TBOX-TELL)
    (ADD-ROLE-AXIOM :TBOX-TELL)
    (ADD-ROLE-ASSERTION :ABOX-TELL)
    (ADD-PREFIX :OWL-INTERFACE)
    (ADD-NEGATIVE-DATATYPE-ROLE-FILLER :ABOX-TELL)
    (ADD-NEGATED-ROLE-ASSERTION :ABOX-TELL)
    (ADD-EVENT-RULE :EVENTS)
    (ADD-EVENT-ASSERTION :EVENTS)
    (ADD-DISJOINTNESS-AXIOM :TBOX-TELL)
    (ADD-DIFFERENT-FROM-ASSERTION :ABOX-TELL)
    (ADD-DATATYPE-ROLE-FILLER :ABOX-OWL-TELL)
    (ADD-DATATYPE-PROPERTY :TBOX-OWL-TELL)
    (ADD-CONSTRAINT-ASSERTION :ABOX-CD-TELL)
    (ADD-CONCEPT-AXIOM :TBOX-TELL)
    (ADD-CONCEPT-ASSERTION :ABOX-TELL)
    (ADD-ATTRIBUTE-ASSERTION :ABOX-CD-TELL)
    (ADD-ANNOTATION-ROLE-ASSERTION :ABOX-OWL-TELL)
    (ADD-ANNOTATION-CONCEPT-ASSERTION :ABOX-OWL-TELL)
    (ADD-ALL-DIFFERENT-ASSERTION :ABOX-TELL)
    (ABOX-UNA-CONSISTENT-P :ABOX-ASK)
    (ABOX-REALIZED-P :ABOX-ASK)
    (ABOX-PREPARED-P :ABOX-ASK)
    (ABOX-CONSISTENT-P :ABOX-ASK)
    (ABOX-CONSISTENT-IF-ASSERTIONS-ADDED-P :ABOX-ASK)
    (WAITING-RULES :RULE-MANAGEMENT)
    (WAITING-QUERIES :NRQL-ABOX)
    (WAITING-EXPENSIVE-RULES :RULE-MANAGEMENT)
    (WAITING-EXPENSIVE-QUERIES :NRQL-ABOX)
    (WAITING-CHEAP-RULES :RULE-MANAGEMENT)
    (WAITING-CHEAP-QUERIES :NRQL-ABOX)
    (WAIT-FOR-RULES-TO-TERMINATE :RULE-MANAGEMENT)
    (WAIT-FOR-QUERIES-TO-TERMINATE :NRQL-ABOX)
    (USE-INJECTIVE-VARIABLES-BY-DEFAULT :NRQL-ABOX)
    (USE-INDIVIDUAL-SYNONYM-EQUIVALENCE-CLASSES :NRQL-ABOX)
    (UPDATE-RACER :UPDATES)
    (UNDEFINE1 :NRQL-ABOX)
    (UNDEFINE-QUERY :DEFINED-QUERIES)
    (UNDEFINE-ALL :NRQL-ABOX)
    (UNBIND1 :MINILISP)
    (UNBIND-ALL :NRQL-ABOX)
    (TERMINATED-RULES :RULE-MANAGEMENT)
    (TERMINATED-QUERIES :NRQL-ABOX)
    (SWRL-CREATE-FORWARD-CHAINGING-RULES :SWRL)
    (SWRL-CREATE-ABDUCTION-RULES-IF-POSSIBLE :SWRL)
    (STORE-SUBSTRATE-FOR-ABOX :IO)
    (STORE-SERVER-IMAGE :IO)
    (STORE-ALL-SUBSTRATES :IO)
    (SLEEPING-RULES :RULE-MANAGEMENT)
    (SLEEPING-QUERIES :NRQL-ABOX)
    (SLEEPING-EXPENSIVE-RULES :RULE-MANAGEMENT)
    (SLEEPING-EXPENSIVE-QUERIES :NRQL-ABOX)
    (SLEEPING-CHEAP-RULES :RULE-MANAGEMENT)
    (SLEEPING-CHEAP-QUERIES :NRQL-ABOX)
    (SET-SUBSTRATE-TYPE :SUBSTRATE-MANAGEMENT)
    (SET-REWRITE-DEFINED-CONCEPTS :QUERY-OPTIMIZER)
    (SET-RCC-BOX :RCC-SUBSTRATE)
    (SET-RACER-PARAMETER :GENERAL)
    (SET-PROXY-SERVER :owl-interface)
    (SET-NRQL-MODE :NRQL-ABOX)
    (SET-NEW-IND-PREFIX :abduction)
    (SET-NEW-IND-COUNTER :ABDUCTION)
    (SET-MIRROR-DATA-BOX :DATA-SUBSTRATE)
    (SET-MAXIMUM-SIZE-OF-PROCESS-POOL :QUERY-OPTIMIZER)
    (SET-MAX-NO-OF-TUPLES-BOUND :NRQL-ABOX)
    (SET-INITIAL-SIZE-OF-PROCESS-POOL :QUERY-OPTIMIZER)
    (SET-EDGE-LABEL-FOR-NON-EXISTENT-EDGES :rcc-substrate)
    (SET-DATA-BOX :SUBSTRATE-MANAGEMENT)
    (SERVER-VALUE :MINILISP)
    (SERVER-FUNCTION :MINILISP)
    (RUNNING-RULES :RULE-MANAGEMENT)
    (RUNNING-QUERIES :NRQL-ABOX)
    (RUNNING-EXPENSIVE-RULES :RULE-MANAGEMENT)
    (RUNNING-EXPENSIVE-QUERIES :NRQL-ABOX)
    (RUNNING-CHEAP-RULES :RULE-MANAGEMENT)
    (RUNNING-CHEAP-QUERIES :NRQL-ABOX)
    (RUN-ALL-RULES :RULE-MANAGEMENT)
    (RUN-ALL-QUERIES :NRQL-ABOX)
    (RMI :ignore)
    (RESTORE-SUBSTRATE :IO :PERSISTENCE :SUBSTRATE-MANAGEMENT)
    (RESTORE-STANDARD-SETTINGS :NRQL-ABOX)
    (RESTORE-SERVER-IMAGE :IO :PERSISTENCE)
    (RESTORE-ALL-SUBSTRATES :IO :PERSISTENCE :SUBSTRATE-MANAGEMENT)
    (RESET-NRQL-ENGINE :NRQL-ABOX)
    (RESET-ALL-SUBSTRATES :SUBSTRATE-MANAGEMENT)
    (REPORT-INCONSISTENT-QUERIES-AND-RULES :QUERY-REASONING)
    (REMOVE-IMPLIED-CONCEPT-ASSERTIONS :abox-management)
    (REGISTER-RCC-SYNONYM :RCC-SUBSTRATE)
    (REEXECUTE-ALL-RULES :RULE-MANAGEMENT)
    (REEXECUTE-ALL-QUERIES :NRQL-ABOX)
    (READY-RULES :RULE-MANAGEMENT)
    (READY-QUERIES :NRQL-ABOX)
    (RCC-RELATED1 :RCC-SUBSTRATE)
    (RCC-NODE1 :RCC-SUBSTRATE)
    (RCC-NODE-LABEL1 :RCC-SUBSTRATE)
    (RCC-NODE-DESCRIPTION1 :RCC-SUBSTRATE)
    (RCC-INSTANCE1 :RCC-SUBSTRATE)
    (RCC-EDGE1 :RCC-SUBSTRATE)
    (RCC-EDGE-LABEL1 :RCC-SUBSTRATE)
    (RCC-EDGE-DESCRIPTION1 :RCC-SUBSTRATE)
    (RCC-CONSISTENT-P :RCC-SUBSTRATE)
    (RACER-PREPARE-TBOX-QUERY1 :NRQL-TBOX)
    (RACER-PREPARE-TBOX-QUERY :NRQL-TBOX)
    (RACER-PREPARE-RULE1 :RULE-MANAGEMENT)
    (RACER-PREPARE-RULE :RULE-MANAGEMENT)
    (RACER-PREPARE-QUERY1 :NRQL-ABOX)
    (RACER-PREPARE-QUERY :NRQL-ABOX)
    (RACER-APPLY-RULE1 :RULE-MANAGEMENT)
    (RACER-APPLY-RULE-UNDER-PREMISE1 :RULE-MANAGEMENT)
    (RACER-APPLY-RULE-UNDER-PREMISE :RULE-MANAGEMENT)
    (RACER-APPLY-RULE :RULE-MANAGEMENT)
    (RACER-ANSWER-TBOX-QUERY1 :NRQL-TBOX)
    (RACER-ANSWER-TBOX-QUERY :NRQL-TBOX)
    (RACER-ANSWER-QUERY1 :NRQL-ABOX)
    (RACER-ANSWER-QUERY-UNDER-PREMISE1 :NRQL-ABOX)
    (RACER-ANSWER-QUERY-UNDER-PREMISE :NRQL-ABOX)
    (RACER-ANSWER-QUERY :NRQL-ABOX)
    (PROCESSED-RULES :RULE-MANAGEMENT)
    (PROCESSED-QUERIES :NRQL-ABOX)
    (PROCESS-TUPLE-AT-A-TIME :NRQL-ABOX)
    (PROCESS-SET-AT-A-TIME :NRQL-ABOX)
    (PREPARED-RULES :RULE-MANAGEMENT)
    (PREPARED-QUERIES :NRQL-ABOX)
    (PREPARE-RULE1 :RULE-MANAGEMENT)
    (PREPARE-RULE :RULE-MANAGEMENT)
    (PREPARE-QUERY1 :NRQL-ABOX)
    (PREPARE-QUERY :NRQL-ABOX)
    (PREPARE-NRQL-ENGINE :NRQL-ABOX)
    (PREFER-DEFINED-QUERIES :DEFINED-QUERIES :ABDUCTION)
    (|OWLAPI-writeXMLOntologyFile| :OWLAPI-MANAGEMENT)
    (|OWLAPI-writeOntologyFile| :OWLAPI-MANAGEMENT)
    (|OWLAPI-writeFunctionalOntologyFile| :OWLAPI-MANAGEMENT)
    (|OWLAPI-usesSimplifiedProtocol| :OWLAPI-MANAGEMENT)
    (|OWLAPI-usesIncrementalUpdates| :OWLAPI-MANAGEMENT)
    (|OWLAPI-unloadOntology| :OWLAPI-MANAGEMENT)
    (|OWLAPI-unloadOntologies| :OWLAPI-MANAGEMENT)
    (|OWLAPI-unloadAxioms| :OWLAPI-MANAGEMENT)
    (|OWLAPI-unloadAxiom| :OWLAPI-MANAGEMENT)
    (|OWLAPI-storeImage| :OWLAPI-MANAGEMENT :IO :PERSISTENCE)
    (|OWLAPI-sleep| :owlapi-management)
    (|OWLAPI-setReturnPolicy| :OWLAPI-MANAGEMENT)
    (|OWLAPI-setProgressRange| :owlapi-tell)
    (|OWLAPI-setProgress| :owlapi-tell)
    (|OWLAPI-SetOntologyURI| :OWLAPI-MANAGEMENT)
    (|OWLAPI-setCurrentReasoner| :OWLAPI-MANAGEMENT)
    (|OWLAPI-setAxiomCounter| :OWLAPI-MANAGEMENT)
    (|OWLAPI-setAutoDeclareDataProperties| :OWLAPI-MANAGEMENT)
    (|OWLAPI-saveOntology| :OWLAPI-MANAGEMENT :IO)
    (|OWLAPI-restoreImage| :OWLAPI-MANAGEMENT :IO :PERSISTENCE)
    (|OWLAPI-resetAxiomCounter| :OWLAPI-MANAGEMENT)
    (|OWLAPI-removePrefix| :OWLAPI-MANAGEMENT)
    (|OWLAPI-RemoveAxioms| :OWLAPI-MANAGEMENT)
    (|OWLAPI-removeAxioms| :OWLAPI-MANAGEMENT)
    (|OWLAPI-RemoveAxiom| :OWLAPI-MANAGEMENT)
    (|OWLAPI-removeAxiom| :OWLAPI-MANAGEMENT)
    (|OWLAPI-reloadLoadedOntologies| :OWLAPI-MANAGEMENT)
    (|OWLAPI-registerReferencedEntities| :OWLAPI-MANAGEMENT)
    (|OWLAPI-registerObject| :OWLAPI-MANAGEMENT)
    (|OWLAPI-registerLastAnswer| :OWLAPI-MANAGEMENT)
    (|OWLAPI-registerDeclaredEntities| :OWLAPI-MANAGEMENT)
    (|OWLAPI-realize| :OWLAPI-MANAGEMENT)
    (|OWLAPI-readXMLOntologyFile| :OWLAPI-MANAGEMENT :IO)
    (|OWLAPI-readXMLOntologyDocument| :OWLAPI-MANAGEMENT :IO)
    (|OWLAPI-readOntology| :OWLAPI-MANAGEMENT :IO)
    (|OWLAPI-readFunctionalOntologyFile| :OWLAPI-MANAGEMENT :IO)
    (|OWLAPI-readFunctionalOntologyDocument| :OWLAPI-MANAGEMENT :IO)
    (|OWLAPI-parseNative| :OWLAPI-MANAGEMENT)
    (|OWLAPI-parse| :OWLAPI-MANAGEMENT)
    (|OWLAPI-nextAxiomUseID| :OWLAPI-MANAGEMENT)
    (|OWLAPI-newReasoner1| :ignore)
    (|OWLAPI-newReasoner| :OWLAPI-MANAGEMENT)
    (|OWLAPI-newOntology| :OWLAPI-MANAGEMENT)
    (|OWLAPI-mergeOntologies| :OWLAPI-MANAGEMENT)
    (|OWLAPI-manuallyApplyChanges| :OWLAPI-MANAGEMENT)
    (|OWLAPI-loadOntology| :OWLAPI-MANAGEMENT)
    (|OWLAPI-loadOntologies| :OWLAPI-MANAGEMENT)
    (|OWLAPI-loadAxioms| :OWLAPI-MANAGEMENT)
    (|OWLAPI-loadAxiom| :OWLAPI-MANAGEMENT)
    (|OWLAPI-keepAnnotations| :OWLAPI-MANAGEMENT)
    (|OWLAPI-isTransitive| :OWLAPI-ASK)
    (|OWLAPI-isSymmetric| :OWLAPI-ASK)
    (|OWLAPI-isSubClassOf| :OWLAPI-ASK)
    (|OWLAPI-isSatisfiable| :OWLAPI-ASK)
    (|OWLAPI-isSameIndividual| :OWLAPI-ASK)
    (|OWLAPI-isReflexive| :OWLAPI-ASK)
    (|OWLAPI-isRealised| :OWLAPI-ASK)
    (|OWLAPI-isIrreflexive| :OWLAPI-ASK)
    (|OWLAPI-isInverseFunctional| :OWLAPI-ASK)
    (|OWLAPI-isFunctional| :OWLAPI-ASK)
    (|OWLAPI-isEquivalentClass| :OWLAPI-ASK)
    (|OWLAPI-isEntailed| :owlapi-ask)
    (|OWLAPI-isDifferentIndividual| :OWLAPI-ASK)
    (|OWLAPI-isDefinedObjectProperty| :OWLAPI-ASK)
    (|OWLAPI-isDefinedIndividual| :OWLAPI-ASK)
    (|OWLAPI-isDefinedDataProperty| :OWLAPI-ASK)
    (|OWLAPI-isDefinedClass| :OWLAPI-ASK)
    (|OWLAPI-isConsistent| :OWLAPI-ASK)
    (|OWLAPI-isClassified| :OWLAPI-ASK)
    (|OWLAPI-isClass| :OWLAPI-ASK)
    (|OWLAPI-isAsymmetric| :OWLAPI-ASK)
    (|OWLAPI-init| :OWLAPI-MANAGEMENT)
    (|OWLAPI-ignoreDeclarations| :OWLAPI-MANAGEMENT)
    (|OWLAPI-ignoreAnnotations| :OWLAPI-MANAGEMENT)
    (|OWLAPI-IDToAxiom| :OWLAPI-MANAGEMENT)
    (|OWLAPI-hasType| :OWLAPI-ASK)
    (|OWLAPI-hasObjectPropertyRelationship| :OWLAPI-ASK)
    (|OWLAPI-hasDataPropertyRelationship| :OWLAPI-ASK)
    (|OWLAPI-getTypes| :OWLAPI-ASK)
    (|OWLAPI-getSuperProperties| :OWLAPI-ASK)
    (|OWLAPI-getSuperClasses| :OWLAPI-ASK)
    (|OWLAPI-getSubProperties| :OWLAPI-ASK)
    (|OWLAPI-getSubClasses| :OWLAPI-ASK)
    (|OWLAPI-getSameIndividuals| :OWLAPI-ASK)
    (|OWLAPI-getRelatedValues| :OWLAPI-ASK)
    (|OWLAPI-getRelatedIndividuals| :OWLAPI-ASK)
    (|OWLAPI-getReasoners| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getRanges| :OWLAPI-ASK)
    (|OWLAPI-getPrefixes| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getOWLTransitiveObjectPropertyAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLSymmetricObjectPropertyAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLSubClassAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLSubAnnotationPropertyOfAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLSubAnnotationPropertyAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLSameIndividualsAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLReflexiveObjectPropertyAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLReallyImplicitDeclarationAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLPrefixDeclarationAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLOntologyVersionDeclarationAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLOntologyAnnotationAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLObjectSubPropertyAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLObjectPropertyRangeAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLObjectPropertyDomainAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLObjectPropertyAssertionAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLNegativeDataPropertyAssertionAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLIrreflexiveObjectPropertyAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLInverseObjectPropertiesAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLImportsDeclarationAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLImplicitDeclarationAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLHasKeyAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLFunctionalObjectPropertyAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLFunctionalDataPropertyAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLEquivalentObjectPropertiesAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLEquivalentDataPropertiesAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLEquivalentClassesAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLEntityAnnotationAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLDisjointUnionAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLDisjointObjectPropertiesAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLDisjointDataPropertiesAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLDisjointClassesAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLDifferentIndividualsAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLDeclarationAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLDatatypeDefinitionAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLDataSubPropertyAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLDataPropertyRangeAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLDataPropertyDomainAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLDataPropertyAssertionAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLClassAssertionAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLAxiomAnnotationAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLAsymmetricObjectPropertyAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLAnnotationPropertyRangeAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLAnnotationPropertyDomainAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOWLAnnotationAssertionAxiom| :OWLAPI-TELL)
    (|OWLAPI-getOntologies| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getObjectPropertyValues| :owlapi-ask)
    (|OWLAPI-getObjectPropertyRelationships| :OWLAPI-ASK)
    (|OWLAPI-getLoadedOntologies| :OWLAPI-ASK)
    (|OWLAPI-getInverseProperties| :OWLAPI-ASK)
    (|OWLAPI-getInstances| :owlapi-ask)
    (|OWLAPI-getIndividuals| :OWLAPI-ASK)
    (|OWLAPI-getInconsistentClasses| :OWLAPI-ASK)
    (|OWLAPI-getEquivalentProperties| :OWLAPI-ASK)
    (|OWLAPI-getEquivalentClasses| :OWLAPI-ASK)
    (|OWLAPI-getDomains| :OWLAPI-ASK)
    (|OWLAPI-getDisjointObjectProperties| :owlapi-ask)
    (|OWLAPI-getDisjointDataProperties| :owlapi-ask)
    (|OWLAPI-getDisjointClasses| :owlapi-ask)
    (|OWLAPI-getDifferentIndividuals| :OWLAPI-ASK)
    (|OWLAPI-getDescendantProperties| :OWLAPI-ASK)
    (|OWLAPI-getDescendantClasses| :OWLAPI-ASK)
    (|OWLAPI-getDataPropertyValues| :owlapi-ask)
    (|OWLAPI-getDataPropertyRelationships| :OWLAPI-ASK)
    (|OWLAPI-getCurrentReasoner| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getChanges| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getAxiomsPerOntology| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getAxiomsOfTypeIn| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getAxiomsOfType| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getAxiomsIn| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getAxioms| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getAxiomCounter| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getAutoOntology| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getAutoDeclareDataProperties| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getAnnotationAxiomsForAxiom| :OWLAPI-MANAGEMENT)
    (|OWLAPI-getAncestorProperties| :OWLAPI-ASK)
    (|OWLAPI-getAncestorClasses| :OWLAPI-ASK)
    (|OWLAPI-getAllOntologies| :OWLAPI-MANAGEMENT)
    (|OWLAPI-findObjectFromID| :OWLAPI-MANAGEMENT)
    (|OWLAPI-findIDFromObject| :OWLAPI-MANAGEMENT)
    (|OWLAPI-exportReasoner| :OWLAPI-MANAGEMENT :IO)
    (|OWLAPI-exportOntology| :OWLAPI-MANAGEMENT :IO)
    (|OWLAPI-enableTransientAxiomMode| :OWLAPI-MANAGEMENT)
    (|OWLAPI-enableSimplifiedProtocol| :OWLAPI-MANAGEMENT)
    (|OWLAPI-enableMemorySavingMode| :OWLAPI-MANAGEMENT)
    (|OWLAPI-enableLookupMode| :OWLAPI-MANAGEMENT)
    (|OWLAPI-enableIncrementalUpdates| :OWLAPI-MANAGEMENT)
    (|OWLAPI-dontRegisterReferencedEntities| :OWLAPI-MANAGEMENT)
    (|OWLAPI-dontRegisterDeclaredEntities| :OWLAPI-MANAGEMENT)
    (|OWLAPI-disposeReasoner| :OWLAPI-MANAGEMENT)
    (|OWLAPI-disposeOntology| :OWLAPI-MANAGEMENT)
    (|OWLAPI-disposeOntologies| :OWLAPI-MANAGEMENT)
    (|OWLAPI-disposeAxioms| :OWLAPI-MANAGEMENT)
    (|OWLAPI-disposeAxiom| :OWLAPI-MANAGEMENT)
    (|OWLAPI-dispose| :OWLAPI-MANAGEMENT)
    (|OWLAPI-disableTransientAxiomMode| :OWLAPI-MANAGEMENT)
    (|OWLAPI-disableSimplifiedProtocol| :OWLAPI-MANAGEMENT)
    (|OWLAPI-disableMemorySavingMode| :OWLAPI-MANAGEMENT)
    (|OWLAPI-disableLookupMode| :OWLAPI-MANAGEMENT)
    (|OWLAPI-disableIncrementalUpdates| :OWLAPI-MANAGEMENT)
    (|OWLAPI-disableAutoMode| :OWLAPI-MANAGEMENT)
    (|OWLAPI-describeReasoners| :OWLAPI-MANAGEMENT)
    (|OWLAPI-describeReasoner| :OWLAPI-MANAGEMENT)
    (|OWLAPI-describeOntology| :OWLAPI-MANAGEMENT)
    (|OWLAPI-describeOntologies| :OWLAPI-MANAGEMENT)
    (|OWLAPI-contains| :OWLAPI-MANAGEMENT)
    (|OWLAPI-considerDeclarations| :OWLAPI-MANAGEMENT)
    (|OWLAPI-clearRegistry| :OWLAPI-MANAGEMENT)
    (|OWLAPI-clearOntologies| :OWLAPI-MANAGEMENT)
    (|OWLAPI-clearChanges| :OWLAPI-MANAGEMENT)
    (|OWLAPI-classify| :OWLAPI-ASK)
    (|OWLAPI-batchSynchronize| :OWLAPI-MANAGEMENT)
    (|OWLAPI-AxiomToID| :OWLAPI-MANAGEMENT)
    (|OWLAPI-AxiomLoaded?| :OWLAPI-MANAGEMENT)
    (|OWLAPI-autoRemoveAxiomsFrom| :OWLAPI-MANAGEMENT)
    (|OWLAPI-autoBatchRemoveAxiomsFrom| :OWLAPI-MANAGEMENT)
    (|OWLAPI-autoBatchAddAxiomsTo| :OWLAPI-MANAGEMENT)
    (|OWLAPI-autoApplyChanges| :OWLAPI-MANAGEMENT)
    (|OWLAPI-autoAddAxiomsTo| :OWLAPI-MANAGEMENT)
    (|OWLAPI-applyChanges| :OWLAPI-MANAGEMENT)
    (|OWLAPI-advanceProgress| :owlapi-management)
    (|OWLAPI-addPrefix| :OWLAPI-MANAGEMENT)
    (|OWLAPI-AddAxioms| :OWLAPI-MANAGEMENT)
    (|OWLAPI-addAxioms| :OWLAPI-MANAGEMENT)
    (|OWLAPI-AddAxiom| :OWLAPI-MANAGEMENT)
    (|OWLAPI-addAxiom| :OWLAPI-MANAGEMENT)
    (|OWLAPI-abort| :owlapi-management)
    (OPTIMIZER-USE-CARDINALITY-HEURISTICS :QUERY-OPTIMIZER)
    (OPTIMIZER-SET-TIME-BOUND :QUERY-OPTIMIZER)
    (OPTIMIZER-SET-NO-OF-PLANS-UPPER-BOUND :QUERY-OPTIMIZER)
    (OPTIMIZER-GET-TIME-BOUND :QUERY-OPTIMIZER)
    (OPTIMIZER-GET-NO-OF-PLANS-UPPER-BOUND :QUERY-OPTIMIZER)
    (OPTIMIZER-ENSURE-LATE-LAMBDA-EVALUATION :query-optimizer)
    (OPTIMIZER-DONT-USE-CARDINALITY-HEURISTICS :QUERY-OPTIMIZER)
    (OPTIMIZER-DONT-ENSURE-LATE-LAMBDA-EVALUATION :query-optimizer)
    (NODE-LABEL1 :DATA-SUBSTRATE)
    (NODE-DESCRIPTION1 :DATA-SUBSTRATE)
    (MOVE-RULES :rule-management)
    (MAKE-QUERY-FROM-ABOX :ABOX-DIFF)
    (MAKE-PLUGIN-FROM-FASL-FILE :PLUGINS)
    (MAKE-FORWARD-RULE-FROM-ABOXES :ABOX-DIFF)
    (MAKE-BACKWARD-RULE-FROM-ABOXES :ABOX-DIFF)
    (MAKE-ABDUCTION-RULE-FROM-ABOXES :ABOX-DIFF)
    (LOAD-RACER-PLUGINS :PLUGINS)
    (LOAD-RACER-PLUGIN :PLUGINS)
    (LOAD-RACER-PATCHES :PATCHES)
    (LOAD-RACER-PATCH :PATCHES)
    (KEEP-DEFINED-QUERY-ATOMS :DEFINED-QUERIES :ABDUCTION)
    (INSTALLED-PLUGINS :PLUGINS)
    (INSTALLED-PATCHES :PATCHES)
    (INCLUDE-PERMUTATIONS :NRQL-ABOX)
    (INACCURATE-RULES :RULE-MANAGEMENT)
    (INACCURATE-QUERIES :NRQL-ABOX)
    (IN-UNSAFE-MODE? :GENERAL :IO)
    (GET-SUBSTRATE-TYPE :SUBSTRATE-MANAGEMENT)
    (GET-SUBSTRATE-NODES :DATA-SUBSTRATE)
    (GET-SUBSTRATE-EDGES :DATA-SUBSTRATE)
    (GET-ROLE-HIERARCHY :TBOX-ASK)
    (GET-PROXY-SERVER :owl-interface)
    (GET-PROCESS-POOL-SIZE :QUERY-OPTIMIZER)
    (GET-PREFIXES :OWL-INTERFACE)
    (GET-NRQL-VERSION :GENERAL :NRQL-ABOX)
    (GET-NEW-IND-PREFIX :abduction)
    (GET-NEW-IND-COUNTER :ABDUCTION)
    (GET-MINIMUM :ABDUCTION)
    (GET-MAXIMUM-SIZE-OF-PROCESS-POOL :QUERY-OPTIMIZER)
    (GET-MAXIMUM :ABDUCTION)
    (GET-MAX-NO-OF-TUPLES-BOUND :QUERY-OPTIMIZER)
    (GET-INITIAL-SIZE-OF-PROCESS-POOL :QUERY-OPTIMIZER)
    (GET-INDIVIDUAL-SUCCESSORS :ABOX-ASK)
    (GET-INDIVIDUAL-DATATYPE-FILLERS :ABOX-OWL-ASK)
    (GET-INDIVIDUAL-ANNOTATION-FILLERS :ABOX-OWL-ASK)
    (GET-INDIVIDUAL-ANNOTATION-DATATYPE-FILLERS :ABOX-OWL-ASK)
    (GET-EDGE-LABEL-FOR-NON-EXISTENT-EDGES :rcc-substrate)
    (GET-DATA-NODE-LABEL :DATA-SUBSTRATE)
    (GET-DATA-NODE-DESCRIPTION :DATA-SUBSTRATE)
    (GET-DATA-EDGE-LABEL :DATA-SUBSTRATE)
    (GET-DATA-EDGE-DESCRIPTION :DATA-SUBSTRATE)
    (GET-CONCEPT-PROPERTIES :TBOX-ASK)
    (GET-ANSWER-SIZE :NRQL-ABOX)
    (GET-ALL-VALUES :MINILISP)
    (GET-ALL-SERVER-VALUES :MINILISP)
    (GET-ALL-SERVER-FUNCTIONS :MINILISP)
    (GET-ALL-FUNCTIONS :MINILISP)
    (GET-ALL-ANSWERS :NRQL-ABOX)
    (GET-AGRAPH-VERSION :GENERAL :AGRAPH)
    (GET-ABOX-GRAPH :ABOX-ASK)
    (FULL-RESET :GENERAL)
    (FCALL :MINILISP)
    (EXPENSIVE-RULES :RULE-MANAGEMENT)
    (EXPENSIVE-QUERIES :NRQL-ABOX)
    (EXIT-SERVER :GENERAL)
    (EXECUTE-OR-REEXECUTE-ALL-RULES :RULE-MANAGEMENT)
    (EXECUTE-OR-REEXECUTE-ALL-QUERIES :NRQL-ABOX)
    (EXECUTE-ALL-RULES :RULE-MANAGEMENT)
    (EXECUTE-ALL-QUERIES :NRQL-ABOX)
    (EXCLUDE-PERMUTATIONS :NRQL-ABOX)
    (EVALUATE1 :MINILISP)
    (ENABLE-VERY-SMART-ABOX-MIRRORING :SUBSTRATE-MANAGEMENT :DATA-SUBSTRATE)
    (ENABLE-TWO-PHASE-QUERY-PROCESSING-MODE :NRQL-ABOX)
    (ENABLE-TOLD-INFORMATION-QUERYING :NRQL-ABOX)
    (ENABLE-SMART-ABOX-MIRRORING :SUBSTRATE-MANAGEMENT :DATA-SUBSTRATE)
    (ENABLE-RCC-SUBSTRATE-MIRRORING :RCC-SUBSTRATE :SUBSTRATE-MANAGEMENT)
    (ENABLE-QUERY-REPOSITORY :QUERY-REASONING)
    (ENABLE-QUERY-REALIZATION :QUERY-REASONING)
    (ENABLE-QUERY-OPTIMIZATION :QUERY-OPTIMIZER)
    (ENABLE-PHASE-TWO-STARTS-WARNING-TOKENS :NRQL-ABOX)
    (ENABLE-NRQL-WARNINGS :NRQL-ABOX)
    (ENABLE-LAZY-UNFOLDING-OF-DEFINED-QUERIES :defined-queries)
    (ENABLE-LAZY-TUPLE-COMPUTATION :NRQL-ABOX)
    (ENABLE-KB-HAS-CHANGED-WARNING-TOKENS :NRQL-ABOX)
    (ENABLE-EAGER-TUPLE-COMPUTATION :NRQL-ABOX)
    (ENABLE-DEFINED-QUERIES :defined-queries)
    (ENABLE-DATA-SUBSTRATE-MIRRORING :SUBSTRATE-MANAGEMENT :DATA-SUBSTRATE)
    (ENABLE-ABOX-MIRRORING :SUBSTRATE-MANAGEMENT :DATA-SUBSTRATE)
    (ENABLE-ABDUCTION :ABDUCTION)
    (EDGE-LABEL1 :DATA-SUBSTRATE)
    (EDGE-DESCRIPTION1 :DATA-SUBSTRATE)
    (DONT-USE-INJECTIVE-VARIABLES-BY-DEFAULT :NRQL-ABOX)
    (DONT-USE-INDIVIDUAL-SYNONYM-EQUIVALENCE-CLASSES :NRQL-ABOX)
    (DONT-REPORT-INCONSISTENT-QUERIES-AND-RULES :QUERY-REASONING)
    (DONT-PREFER-DEFINED-QUERIES :DEFINED-QUERIES :ABDUCTION)
    (DONT-KEEP-DEFINED-QUERY-ATOMS :DEFINED-QUERIES :ABDUCTION)
    (DONT-CHECK-ABOX-CONSISTENCY-BEFORE-QUERYING :QUERY-REASONING)
    (DONT-ALLOW-OVERLOADED-DEFINITIONS :DEFINED-QUERIES :ABDUCTION)
    (DONT-ADD-RULE-CONSEQUENCES-AUTOMATICALLY :RULE-MANAGEMENT)
    (DONT-ADD-ROLE-ASSERTIONS-FOR-DATATYPE-PROPERTIES :RULE-MANAGEMENT)
    (DONT-ADD-MISSING-TOP-CONJUNCTS :NRQL-ABOX)
    (DISABLE-TWO-PHASE-QUERY-PROCESSING-MODE :NRQL-ABOX)
    (DISABLE-TOLD-INFORMATION-QUERYING :NRQL-ABOX)
    (DISABLE-RCC-SUBSTRATE-MIRRORING :RCC-SUBSTRATE :SUBSTRATE-MANAGEMENT)
    (DISABLE-QUERY-REPOSITORY :QUERY-REASONING)
    (DISABLE-QUERY-REALIZATION :QUERY-REASONING)
    (DISABLE-QUERY-OPTIMIZATION :QUERY-OPTIMIZER)
    (DISABLE-PHASE-TWO-STARTS-WARNING-TOKENS :NRQL-ABOX)
    (DISABLE-NRQL-WARNINGS :NRQL-ABOX)
    (DISABLE-LAZY-UNFOLDING-OF-DEFINED-QUERIES :defined-queries)
    (DISABLE-KB-HAS-CHANGED-WARNING-TOKENS :NRQL-ABOX)
    (DISABLE-DEFINED-QUERIES :defined-queries)
    (DISABLE-DATA-SUBSTRATE-MIRRORING :SUBSTRATE-MANAGEMENT :DATA-SUBSTRATE)
    (DISABLE-ABOX-MIRRORING :SUBSTRATE-MANAGEMENT)
    (DISABLE-ABDUCTION :ABDUCTION)
    (DESCRIPTION-IMPLIES-P :DATA-SUBSTRATE)
    (DESCRIBE-SUBSTRATE :SUBSTRATE-MANAGEMENT)
    (DESCRIBE-QUERY-PROCESSING-MODE :NRQL-ABOX)
    (DESCRIBE-DEFINITION :DEFINED-QUERIES)
    (DESCRIBE-CURRENT-SUBSTRATE :SUBSTRATE-MANAGEMENT)
    (DESCRIBE-ALL-SUBSTRATES :SUBSTRATE-MANAGEMENT)
    (DESCRIBE-ALL-RULES :RULE-MANAGEMENT)
    (DESCRIBE-ALL-QUERIES :NRQL-ABOX)
    (DESCRIBE-ALL-NODES :SUBSTRATE-MANAGEMENT)
    (DESCRIBE-ALL-EDGES :SUBSTRATE-MANAGEMENT)
    (DESCRIBE-ALL-DEFINITIONS :DEFINED-QUERIES)
    (DELETE-RCC-SYNONYMS :RCC-SUBSTRATE)
    (DELETE-PREFIX-MAPPINGS :OWL-INTERFACE)
    (DELETE-DATA-NODE :DATA-SUBSTRATE)
    (DELETE-DATA-EDGE :DATA-SUBSTRATE)
    (DELETE-ALL-SUBSTRATES :SUBSTRATE-MANAGEMENT)
    (DELETE-ALL-RULES :RULE-MANAGEMENT)
    (DELETE-ALL-QUERIES :NRQL-ABOX)
    (DELETE-ALL-DEFINITIONS :DEFINED-QUERIES)
    (DEL-RCC-NODE1 :RCC-SUBSTRATE)
    (DEL-RCC-EDGE1 :RCC-SUBSTRATE)
    (DEL-DOC-ENTRY1 :DOC)
    (DEL-DATA-NODE1 :DATA-SUBSTRATE)
    (DEL-DATA-EDGE1 :DATA-SUBSTRATE)
    (DEFPAR1 :MINILISP)
    (DEFINE1 :MINILISP)
    (DEFINE-QUERY :DEFINED-QUERIES)
    (DEFINE-AND-PREPARE-QUERY :DEFINED-QUERIES)
    (DEFINE-AND-EXECUTE-QUERY :DEFINED-QUERIES)
    (DEFCON1 :MINILISP)
    (DEACTIVATE-DEFINED-QUERY :defined-queries)
    (DATA-NODE1 :DATA-SUBSTRATE)
    (DATA-EDGE1 :DATA-SUBSTRATE)
    (CREATE-SUBGRAPH-ABOXES :ABOX-DIFF)
    (CREATE-RCC-NODE :RCC-SUBSTRATE)
    (CREATE-RCC-EDGE :RCC-SUBSTRATE)
    (CREATE-DATA-NODE :DATA-SUBSTRATE)
    (CREATE-DATA-EDGE :DATA-SUBSTRATE)
    (COPY-RULES :RULE-MANAGEMENT)
    (COMPUTE-SUBGRAPH-ABOXES :ABOX-ASK)
    (COMPUTE-ABOX-DIFFERENCE2 :ABOX-DIFF)
    (COMPUTE-ABOX-DIFFERENCE1 :ABOX-DIFF)
    (CLEAR-ALL-DOCUMENTATION :doc)
    (CHECK-ONTOLOGY :EXPLANATIONS :OWL-INTERFACE)
    (CHECK-NRQL-SUBSCRIPTIONS :publish-subscribe)
    (CHECK-FOR-UPDATES :UPDATES)
    (CHECK-CONCEPT-COHERENCE :TBOX-ASK)
    (CHECK-ABOX-CONSISTENCY-BEFORE-QUERYING :NRQL-ABOX)
    (CHEAP-RULES :RULE-MANAGEMENT)
    (CHEAP-QUERIES :NRQL-ABOX)
    (APPLY-RULE-UNDER-PREMISE1 :RULE-MANAGEMENT)
    (APPLY-RULE-UNDER-PREMISE :RULE-MANAGEMENT)
    (APPLY-RULE :RULE-MANAGEMENT)
    (ANSWER-TBOX-QUERY1 :NRQL-TBOX)
    (ANSWER-TBOX-QUERY :NRQL-TBOX)
    (ANSWER-QUERY1 :NRQL-ABOX)
    (ANSWER-QUERY-UNDER-PREMISE1 :NRQL-ABOX)
    (ANSWER-QUERY-UNDER-PREMISE :NRQL-ABOX)
    (ANSWER-QUERY :NRQL-ABOX)
    (ALLOW-OVERLOADED-DEFINITIONS :DEFINED-QUERIES :ABDUCTION)
    (ALL-SUBSTRATES :SUBSTRATE-MANAGEMENT)
    (ALL-SAME-AS-ASSERTIONS :ABOX-ASK)
    (ALL-RULES :RULE-MANAGEMENT)
    (ALL-QUERIES :NRQL-ABOX)
    (ALL-DIFFERENT-FROM-ASSERTIONS :ABOX-ASK)
    (ADD-RULE-CONSEQUENCES-AUTOMATICALLY :RULE-MANAGEMENT)
    (ADD-ROLE-ASSERTIONS-FOR-DATATYPE-PROPERTIES :NRQL-ABOX)
    (ADD-MISSING-TOP-CONJUNCTS :NRQL-ABOX)
    (ADD-DOC-PHRASE1 :DOC)
    (ADD-DOC-IMAGE-FILE1 :DOC)
    (ADD-DOC-IMAGE-DATA1 :DOC)
    (ADD-DOC-IMAGE-DATA-FROM-FILE1 :DOC)
    (ADD-DOC-ENTRY1 :DOC)
    (ACTIVE-RULES :RULE-MANAGEMENT)
    (ACTIVE-QUERIES :NRQL-ABOX)
    (ACTIVATE-DEFINED-QUERY :defined-queries)
    (ACCURATE-RULES :RULE-MANAGEMENT)
    (ACCURATE-QUERIES :NRQL-ABOX)
    (ABOX-ENTAILS-ABOX-P :ABOX-ASK)
    (ABORT-ALL-RULES :RULE-MANAGEMENT)
    (ABORT-ALL-QUERIES :NRQL-ABOX)
    (UNSUBSCRIBE-FROM :publish-subscribe)
    (UNAPPLICABLE-RULES :RULE-MANAGEMENT)
    (SUBSCRIBE-TO :publish-subscribe)
    (SHOW-QBOX-FOR-ABOX :QUERY-REASONING)
    (RULE-WAITING-P :RULE-MANAGEMENT)
    (RULE-UNAPPLICABLE-P :RULE-MANAGEMENT)
    (RULE-TERMINATED-P :RULE-MANAGEMENT)
    (RULE-SLEEPING-P :RULE-MANAGEMENT)
    (RULE-RUNNING-P :RULE-MANAGEMENT)
    (RULE-READY-P :RULE-MANAGEMENT)
    (RULE-PROCESSED-P :RULE-MANAGEMENT)
    (RULE-PREPARED-P :RULE-MANAGEMENT)
    (RULE-CONSISTENT-P :RULE-MANAGEMENT)
    (RULE-CONSEQUENCE :RULE-MANAGEMENT)
    (RULE-APPLICABLE-P :RULE-MANAGEMENT)
    (RULE-ANTECEDENCE :RULE-MANAGEMENT)
    (RULE-ACTIVE-P :RULE-MANAGEMENT)
    (RULE-ACCURATE-P :RULE-MANAGEMENT)
    (REPREPARE-RULE :RULE-MANAGEMENT)
    (REPREPARE-QUERY :NRQL-ABOX)
    (REEXECUTE-RULE :RULE-MANAGEMENT)
    (REEXECUTE-QUERY :NRQL-ABOX)
    (QUERY-WAITING-P :NRQL-ABOX)
    (QUERY-TERMINATED-P :NRQL-ABOX)
    (QUERY-SUBSCRIBERS :publish-subscribe)
    (QUERY-SLEEPING-P :NRQL-ABOX)
    (QUERY-RUNNING-P :NRQL-ABOX)
    (QUERY-READY-P :NRQL-ABOX)
    (QUERY-PROCESSED-P :NRQL-ABOX)
    (QUERY-PREPARED-P :NRQL-ABOX)
    (QUERY-PARENTS :QUERY-REASONING)
    (QUERY-HEAD :NRQL-ABOX)
    (QUERY-EQUIVALENTS :QUERY-REASONING)
    (QUERY-EQUIVALENT-P :QUERY-REASONING)
    (QUERY-ENTAILS-P :QUERY-REASONING)
    (QUERY-DESCENDANTS :QUERY-REASONING)
    (QUERY-CONSISTENT-P :QUERY-REASONING)
    (QUERY-CHILDREN :QUERY-REASONING)
    (QUERY-BODY :NRQL-ABOX)
    (QUERY-ANCESTORS :QUERY-REASONING)
    (QUERY-ACTIVE-P :NRQL-ABOX)
    (QUERY-ACCURATE-P :NRQL-ABOX)
    (ORIGINAL-RULE-CONSEQUENCE :RULE-MANAGEMENT)
    (ORIGINAL-RULE-ANTECEDENCE :RULE-MANAGEMENT)
    (ORIGINAL-QUERY-HEAD :NRQL-ABOX)
    (ORIGINAL-QUERY-BODY :NRQL-ABOX)
    (NEXT-TUPLE-AVAILABLE-P :NRQL-ABOX)
    (NEXT-SET-OF-RULE-CONSEQUENCES-AVAILABLE-P :RULE-MANAGEMENT)
    (GET-NUMBER-OF-EXPLANATIONS :ABDUCTION)
    (GET-NODES-IN-QBOX-FOR-ABOX :QUERY-REASONING)
    (GET-NEXT-TUPLE :NRQL-ABOX)
    (GET-NEXT-SET-OF-RULE-CONSEQUENCES :RULE-MANAGEMENT)
    (GET-NEXT-N-REMAINING-TUPLES :NRQL-ABOX)
    (GET-NEXT-N-REMAINING-SETS-OF-RULE-CONSEQUENCES :RULE-MANAGEMENT)
    (GET-EXPLANATIONS :ABDUCTION)
    (GET-DAG-OF-QBOX-FOR-ABOX :QUERY-REASONING)
    (GET-CURRENT-TUPLE :NRQL-ABOX)
    (GET-CURRENT-SET-OF-RULE-CONSEQUENCES :RULE-MANAGEMENT)
    (GET-CHOSEN-SETS-OF-RULE-CONSEQUENCES :RULE-MANAGEMENT)
    (GET-ANSWER :NRQL-ABOX)
    (GET-ALL-REMAINING-TUPLES :NRQL-ABOX)
    (GET-ALL-REMAINING-SETS-OF-RULE-CONSEQUENCES :RULE-MANAGEMENT)
    (EXPENSIVE-RULE-P :RULE-MANAGEMENT)
    (EXPENSIVE-QUERY-P :NRQL-ABOX)
    (EXECUTE-RULE :RULE-MANAGEMENT)
    (EXECUTE-QUERY :NRQL-ABOX)
    (EXECUTE-OR-REEXECUTE-RULE :RULE-MANAGEMENT)
    (EXECUTE-OR-REEXECUTE-QUERY :NRQL-ABOX)
    (EXECUTE-APPLICABLE-RULES :RULE-MANAGEMENT)
    (DESCRIBE-RULE-STATUS :RULE-MANAGEMENT)
    (DESCRIBE-RULE :RULE-MANAGEMENT)
    (DESCRIBE-QUERY-STATUS :NRQL-ABOX)
    (DESCRIBE-QUERY :NRQL-ABOX)
    (DELETE-RULE :RULE-MANAGEMENT)
    (DELETE-QUERY :NRQL-ABOX)
    (CLASSIFY-QUERY :QUERY-REASONING)
    (CHOOSE-CURRENT-SET-OF-RULE-CONSEQUENCES :RULE-MANAGEMENT)
    (CHEAP-RULE-P :RULE-MANAGEMENT)
    (CHEAP-QUERY-P :NRQL-ABOX)
    (APPLICABLE-RULES :RULE-MANAGEMENT)
    (ADD-EXPLANATION-ASSERTIONS :ABDUCTION)
    (ADD-CHOSEN-SETS-OF-RULE-CONSEQUENCES :RULE-MANAGEMENT)
    (ABORT-RULE :RULE-MANAGEMENT)
    (ABORT-QUERY :NRQL-ABOX)
    (UNSUBSCRIBE :PUBLISH-SUBSCRIBE)
    (UNRELATED :ABOX-TELL)
    (UNPUBLISH :PUBLISH-SUBSCRIBE)
    (TRANSITIVE? :TBOX-ASK)
    (TRANSITIVE :TBOX-TELL)
    (TIMENET-RETRIEVE :EVENTS)
    (TIME :TESTING)
    (TBOX-PREPARED? :TBOX-ASK)
    (TBOX-CYCLIC? :TBOX-ASK)
    (TBOX-COHERENT? :TBOX-ASK)
    (TBOX-CLASSIFIED? :TBOX-ASK)
    (SYMMETRIC? :TBOX-ASK)
    (SYMMETRIC :TBOX-TELL)
    (SUBSCRIBE :PUBLISH-SUBSCRIBE)
    (STATE :ABOX-TELL)
    (SPARQL-RETRIEVE :SPARQL)
    (SPARQL-ANSWER-QUERY :SPARQL)
    (SIGNATURE :KB-MANAGEMENT)
    (SAME-INDIVIDUAL-AS :ABOX-TELL)
    (SAME-AS :ABOX-TELL)
    (ROLES-EQUIVALENT :TBOX-TELL)
    (ROLES-DISJOINT :TBOX-TELL)
    (ROLE? :TBOX-ASK)
    (ROLE-SYNONYMS :TBOX-ASK)
    (ROLE-SUBSUMES? :TBOX-ASK)
    (ROLE-SATISFIABLE? :TBOX-ASK)
    (ROLE-RANGE :TBOX-ASK)
    (ROLE-PARENTS :TBOX-ASK)
    (ROLE-INVERSE :TBOX-ASK)
    (ROLE-EQUIVALENT? :TBOX-ASK)
    (ROLE-DOMAIN :TBOX-ASK)
    (ROLE-DISJOINT? :TBOX-ASK)
    (ROLE-DESCENDANTS :TBOX-ASK)
    (ROLE-CHILDREN :TBOX-ASK)
    (ROLE-ANCESTORS :TBOX-ASK)
    (RELATED-INDIVIDUALS :TBOX-ASK)
    (RELATED :TBOX-TELL)
    (REFLEXIVE? :TBOX-ASK)
    (REFLEXIVE :TBOX-TELL)
    (RANGE :TBOX-TELL)
    (PUBLISH :PUBLISH-SUBSCRIBE)
    (PRETRIEVE :AGRAPH)
    (IRREFLEXIVE? :TBOX-ASK)
    (IRREFLEXIVE :TBOX-TELL)
    (INVERSE :TBOX-TELL)
    (INSTANCE :ABOX-TELL)
    (INIT-SUBSCRIPTIONS :PUBLISH-SUBSCRIBE)
    (INIT-PUBLICATIONS :PUBLISH-SUBSCRIBE)
    (INDIVIDUALS-RELATED? :ABOX-ASK)
    (INDIVIDUALS-NOT-EQUAL? :ABOX-ASK)
    (INDIVIDUALS-EQUAL? :ABOX-ASK)
    (INDIVIDUAL? :ABOX-ASK)
    (INDIVIDUAL-TYPES :ABOX-ASK)
    (INDIVIDUAL-TOLD-DATATYPE-FILLERS :ABOX-OWL-ASK)
    (INDIVIDUAL-TOLD-ATTRIBUTE-VALUE :ABOX-CD-ASK)
    (INDIVIDUAL-SYNONYMS :ABOX-ASK)
    (INDIVIDUAL-INSTANCE? :ABOX-ASK)
    (INDIVIDUAL-FILLERS :ABOX-ASK)
    (INDIVIDUAL-FILLED-ROLES :ABOX-ASK)
    (INDIVIDUAL-DIRECT-TYPES :ABOX-ASK)
    (INDIVIDUAL-ATTRIBUTE-FILLERS :ABOX-CD-ASK)
    (INDIVIDUAL-ANTONYMS :ABOX-ASK)
    (IN-TBOX :TBOX-MANAGEMENT)
    (IN-KNOWLEDGE-BASE :KB-MANAGEMENT)
    (IN-ABOX :ABOX-MANAGEMENT)
    (IMPLIES-ROLE :TBOX-TELL)
    (IMPLIES :TBOX-TELL)
    (GET-CONCEPT-NEGATED-DEFINITION :TBOX-ASK)
    (GET-CONCEPT-DEFINITION :TBOX-ASK)
    (FUNCTIONAL :TBOX-TELL)
    (FORGET :TBOX-FORGET)
    (FEATURE? :TBOX-ASK)
    (EQUIVALENT :TBOX-TELL)
    (DOMAIN :TBOX-TELL)
    (DISJOINT :TBOX-TELL)
    (DIRECT-PREDECESSORS :ABOX-ASK)
    (DIFFERENT-FROM :ABOX-TELL)
    (DELETE-TBOX :TBOX-MANAGEMENT)
    (DELETE-ABOX :ABOX-MANAGEMENT)
    (DEFINE-TBOX :TBOX-MANAGEMENT)
    (DEFINE-RULE :RULE-MANAGEMENT)
    (DEFINE-PRIMITIVE-ROLE :TBOX-TELL)
    (DEFINE-PRIMITIVE-CONCEPT :TBOX-TELL)
    (DEFINE-PRIMITIVE-ATTRIBUTE :TBOX-CD-TELL)
    (DEFINE-PREFIX :OWL-INTERFACE)
    (DEFINE-INDIVIDUAL :ABOX-TELL)
    (DEFINE-EVENT-RULE :EVENTS)
    (DEFINE-EVENT-ASSERTION :EVENTS)
    (DEFINE-DISTINCT-INDIVIDUAL :ABOX-TELL)
    (DEFINE-DISJOINT-PRIMITIVE-CONCEPT :TBOX-TELL)
    (DEFINE-DATATYPE-PROPERTY :TBOX-OWL-TELL)
    (DEFINE-CONCRETE-DOMAIN-ATTRIBUTE :TBOX-CD-TELL)
    (DEFINE-CONCEPT :TBOX-TELL)
    (DEFINE-ABOX :ABOX-MANAGEMENT)
    (DATATYPE-ROLE-FILLER :ABOX-OWL-TELL)
    (CONSTRAINTS :ABOX-CD-TELL)
    (CONSTRAINT-ENTAILED? :ABOX-CD-ASK)
    (CONSTRAINED :ABOX-CD-TELL)
    (CONCEPT? :TBOX-ASK)
    (CONCEPT-SYNONYMS :TBOX-ASK)
    (CONCEPT-SUBSUMES? :TBOX-ASK)
    (CONCEPT-SATISFIABLE? :TBOX-ASK)
    (CONCEPT-PARENTS :TBOX-ASK)
    (CONCEPT-IS-PRIMITIVE? :TBOX-ASK)
    (CONCEPT-INSTANCES :ABOX-ASK)
    (CONCEPT-EQUIVALENT? :TBOX-ASK)
    (CONCEPT-DISJOINT? :TBOX-ASK)
    (CONCEPT-DESCENDANTS :TBOX-ASK)
    (CONCEPT-CHILDREN :TBOX-ASK)
    (CONCEPT-ANCESTORS :TBOX-ASK)
    (CLONE-TBOX :TBOX-MANAGEMENT)
    (CLONE-ABOX :ABOX-MANAGEMENT)
    (CD-OBJECT? :ABOX-CD-ASK)
    (CD-ATTRIBUTE? :TBOX-CD-ASK)
    (ATTRIBUTE-FILLER :TBOX-CD-TELL)
    (ATTRIBUTE-DOMAIN :TBOX-CD-ASK)
    (ASYMMETRIC? :TBOX-ASK)
    (ASYMMETRIC :TBOX-TELL)
    (ALL-DIFFERENT :ABOX-TELL)
    (ABOX-UNA-CONSISTENT? :ABOX-ASK)
    (ABOX-REALIZED? :ABOX-ASK)
    (ABOX-PREPARED? :ABOX-ASK)
    (ABOX-CONSISTENT? :ABOX-ASK)
    (UNDEFQUERY :DEFINED-QUERIES)
    (UNDEFINE :NRQL-ABOX)
    (UNBIND :MINILISP)
    (TBOX-RETRIEVE1 :NRQL-TBOX)
    (TBOX-RETRIEVE :NRQL-TBOX)
    (RETRIEVE1 :NRQL-ABOX)
    (RETRIEVE-WITH-EXPLANATION :ABDUCTION)
    (RETRIEVE-UNDER-PREMISE1 :NRQL-ABOX)
    (RETRIEVE-UNDER-PREMISE :NRQL-ABOX)
    (RETRIEVE :NRQL-ABOX)
    (RCC-SYNONYM :RCC-SUBSTRATE)
    (RCC-RELATED :RCC-SUBSTRATE)
    (RCC-NODE-LABEL :RCC-SUBSTRATE)
    (RCC-NODE-DESCRIPTION :RCC-SUBSTRATE)
    (RCC-NODE :RCC-SUBSTRATE)
    (RCC-INSTANCE :RCC-SUBSTRATE)
    (RCC-EDGE-LABEL :RCC-SUBSTRATE)
    (RCC-EDGE-DESCRIPTION :RCC-SUBSTRATE)
    (RCC-EDGE :RCC-SUBSTRATE)
    (RCC-CONSISTENT? :RCC-SUBSTRATE)
    (PREPRULE1 :RULE-MANAGEMENT)
    (PREPRULE :RULE-MANAGEMENT)
    (PREPARE-TBOX-QUERY1 :NRQL-TBOX)
    (PREPARE-TBOX-QUERY :NRQL-TBOX)
    (PREPARE-ABOX-RULE1 :RULE-MANAGEMENT)
    (PREPARE-ABOX-RULE :RULE-MANAGEMENT)
    (PREPARE-ABOX-QUERY1 :NRQL-ABOX)
    (PREPARE-ABOX-QUERY :NRQL-ABOX)
    (NODE-LABEL :DATA-SUBSTRATE)
    (NODE-DESCRIPTION :DATA-SUBSTRATE)
    (IN-RCC-BOX :RCC-SUBSTRATE)
    (IN-MIRROR-DATA-BOX :DATA-SUBSTRATE)
    (IN-DATA-BOX :DATA-SUBSTRATE)
    (FIRERULE1 :RULE-MANAGEMENT)
    (FIRERULE-UNDER-PREMISE1 :RULE-MANAGEMENT)
    (FIRERULE-UNDER-PREMISE :RULE-MANAGEMENT)
    (FIRERULE :RULE-MANAGEMENT)
    (EVALUATE :MINILISP)
    (EDGE-LABEL :DATA-SUBSTRATE)
    (EDGE-DESCRIPTION :DATA-SUBSTRATE)
    (DESCRIPTION-IMPLIES? :DATA-SUBSTRATE)
    (DEL-RCC-NODE :RCC-SUBSTRATE)
    (DEL-RCC-EDGE :RCC-SUBSTRATE)
    (DEL-DOC-ENTRY :DOC)
    (DEL-DATA-NODE :DATA-SUBSTRATE)
    (DEL-DATA-EDGE :DATA-SUBSTRATE)
    (DEFQUERY :DEFINED-QUERIES)
    (DEFPAR :MINILISP)
    (DEFINE :MINILISP)
    (DEFCON :MINILISP)
    (DEF-AND-PREP-QUERY :DEFINED-QUERIES)
    (DEF-AND-EXEC-QUERY :DEFINED-QUERIES)
    (DATA-NODE :DATA-SUBSTRATE)
    (DATA-EDGE :DATA-SUBSTRATE)
    (COMPUTE-ABOX-DIFFERENCE-ALTERNATIVE :ABOX-DIFF)
    (COMPUTE-ABOX-DIFFERENCE :ABOX-DIFF)
    (APPLY-ABOX-RULE1 :RULE-MANAGEMENT)
    (APPLY-ABOX-RULE-UNDER-PREMISE1 :RULE-MANAGEMENT)
    (APPLY-ABOX-RULE-UNDER-PREMISE :RULE-MANAGEMENT)
    (APPLY-ABOX-RULE :RULE-MANAGEMENT)
    (ADD-DOC-PHRASE :DOC)
    (ADD-DOC-IMAGE-FILE :DOC)
    (ADD-DOC-IMAGE-DATA-FROM-FILE :DOC)
    (ADD-DOC-IMAGE-DATA :DOC)
    (ADD-DOC-ENTRY :DOC)
    (WITH-BINDINGS :NRQL-ABOX)
    (WITH-BINDINGS-EVALUATED :NRQL-ABOX)
    (WITH-CRITICAL-SECTION :GENERAL)
    (WITH-FUTURE-BINDINGS :NRQL-ABOX)
    (WITH-FUTURE-BINDINGS-EVALUATED :NRQL-ABOX)
    (WITH-NRQL-SETTINGS :NRQL-ABOX)
    (WITH-NRQL-SETTINGS-EVALUATED :NRQL-ABOX)
    (WITH-UNIQUE-NAME-ASSUMPTION :ABOX-MANAGEMENT)
    (WITHOUT-UNIQUE-NAME-ASSUMPTION :ABOX-MANAGEMENT)
    (OWLAPI-WRITE-XML-ONTOLOGY-FILE :ignore)
    (OWLAPI-WRITE-ONTOLOGY-FILE :ignore)
    (OWLAPI-WRITE-FUNCTIONAL-ONTOLOGY-FILE :ignore)
    (OWLAPI-USES-SIMPLIFIED-PROTOCOL :ignore)
    (OWLAPI-USES-INCREMENTAL-UPDATES :ignore)
    (OWLAPI-UNLOAD-ONTOLOGY :ignore)
    (OWLAPI-UNLOAD-ONTOLOGIES :ignore)
    (OWLAPI-UNLOAD-AXIOMS :ignore)
    (OWLAPI-UNLOAD-AXIOM :ignore)
    (OWLAPI-STORE-IMAGE :ignore)
    (OWLAPI-SLEEP :ignore)
    (OWLAPI-SET-RETURN-POLICY :ignore)
    (OWLAPI-SET-PROGRESS-RANGE :ignore)
    (OWLAPI-SET-PROGRESS :ignore)
    (OWLAPI-SET-CURRENT-REASONER :ignore)
    (OWLAPI-SET-AXIOM-COUNTER :ignore)
    (OWLAPI-SET-AUTO-DECLARE-DATA-PROPERTIES :ignore)
    (OWLAPI-SAVE-ONTOLOGY :ignore)
    (OWLAPI-RESTORE-IMAGE :ignore)
    (OWLAPI-RESET-AXIOM-COUNTER :ignore)
    (OWLAPI-REMOVE-PREFIX :ignore)
    (OWLAPI-RELOAD-LOADED-ONTOLOGIES :ignore)
    (OWLAPI-REGISTER-REFERENCED-ENTITIES :ignore)
    (OWLAPI-REGISTER-OBJECT :ignore)
    (OWLAPI-REGISTER-LAST-ANSWER :ignore)
    (OWLAPI-REGISTER-DECLARED-ENTITIES :ignore)
    (OWLAPI-REALIZE :ignore)
    (OWLAPI-READ-XML-ONTOLOGY-FILE :ignore)
    (OWLAPI-READ-XML-ONTOLOGY-DOCUMENT :ignore)
    (OWLAPI-READ-ONTOLOGY :ignore)
    (OWLAPI-READ-FUNCTIONAL-ONTOLOGY-FILE :ignore)
    (OWLAPI-READ-FUNCTIONAL-ONTOLOGY-DOCUMENT :ignore)
    (OWLAPI-PARSE-NATIVE :ignore)
    (OWLAPI-PARSE :ignore)
    (OWLAPI-NEXT-AXIOM-USE-ID :ignore)
    (OWLAPI-NEW-REASONER1 :ignore)
    (OWLAPI-NEW-REASONER :ignore)
    (OWLAPI-NEW-ONTOLOGY :ignore)
    (OWLAPI-MERGE-ONTOLOGIES :ignore)
    (OWLAPI-MANUALLY-APPLY-CHANGES :ignore)
    (OWLAPI-LOAD-ONTOLOGY :ignore)
    (OWLAPI-LOAD-ONTOLOGIES :ignore)
    (OWLAPI-LOAD-AXIOMS :ignore)
    (OWLAPI-LOAD-AXIOM :ignore)
    (OWLAPI-KEEP-ANNOTATIONS :ignore)
    (OWLAPI-IS-TRANSITIVE :ignore)
    (OWLAPI-IS-SYMMETRIC :ignore)
    (OWLAPI-IS-SUB-CLASS-OF :ignore)
    (OWLAPI-IS-SATISFIABLE :ignore)
    (OWLAPI-IS-SAME-INDIVIDUAL :ignore)
    (OWLAPI-IS-REFLEXIVE :ignore)
    (OWLAPI-IS-REALISED :ignore)
    (OWLAPI-IS-IRREFLEXIVE :ignore)
    (OWLAPI-IS-INVERSE-FUNCTIONAL :ignore)
    (OWLAPI-IS-FUNCTIONAL :ignore)
    (OWLAPI-IS-EQUIVALENT-CLASS :ignore)
    (OWLAPI-IS-ENTAILED :ignore)
    (OWLAPI-IS-DIFFERENT-INDIVIDUAL :ignore)
    (OWLAPI-IS-DEFINED-OBJECT-PROPERTY :ignore)
    (OWLAPI-IS-DEFINED-INDIVIDUAL :ignore)
    (OWLAPI-IS-DEFINED-DATA-PROPERTY :ignore)
    (OWLAPI-IS-DEFINED-CLASS :ignore)
    (OWLAPI-IS-CONSISTENT :ignore)
    (OWLAPI-IS-CLASSIFIED :ignore)
    (OWLAPI-IS-CLASS :ignore)
    (OWLAPI-IS-ASYMMETRIC :ignore)
    (OWLAPI-INIT :ignore)
    (OWLAPI-IGNORE-DECLARATIONS :ignore)
    (OWLAPI-IGNORE-ANNOTATIONS :ignore)
    (OWLAPI-HAS-TYPE :ignore)
    (OWLAPI-HAS-OBJECT-PROPERTY-RELATIONSHIP :ignore)
    (OWLAPI-HAS-DATA-PROPERTY-RELATIONSHIP :ignore)
    (OWLAPI-GET-TYPES :ignore)
    (OWLAPI-GET-SUPER-PROPERTIES :ignore)
    (OWLAPI-GET-SUPER-CLASSES :ignore)
    (OWLAPI-GET-SUB-PROPERTIES :ignore)
    (OWLAPI-GET-SUB-CLASSES :ignore)
    (OWLAPI-GET-SAME-INDIVIDUALS :ignore)
    (OWLAPI-GET-RELATED-VALUES :ignore)
    (OWLAPI-GET-RELATED-INDIVIDUALS :ignore)
    (OWLAPI-GET-REASONERS :ignore)
    (OWLAPI-GET-RANGES :ignore)
    (OWLAPI-GET-PREFIXES :ignore)
    (OWLAPI-GET-ONTOLOGIES :ignore)
    (OWLAPI-GET-OBJECT-PROPERTY-VALUES :ignore)
    (OWLAPI-GET-OBJECT-PROPERTY-RELATIONSHIPS :ignore)
    (OWLAPI-GET-OWL-TRANSITIVE-OBJECT-PROPERTY-AXIOM :ignore)
    (OWLAPI-GET-OWL-SYMMETRIC-OBJECT-PROPERTY-AXIOM :ignore)
    (OWLAPI-GET-OWL-SUB-CLASS-AXIOM :ignore)
    (OWLAPI-GET-OWL-SUB-ANNOTATION-PROPERTY-OF-AXIOM :ignore)
    (OWLAPI-GET-OWL-SUB-ANNOTATION-PROPERTY-AXIOM :ignore)
    (OWLAPI-GET-OWL-SAME-INDIVIDUALS-AXIOM :ignore)
    (OWLAPI-GET-OWL-REFLEXIVE-OBJECT-PROPERTY-AXIOM :ignore)
    (OWLAPI-GET-OWL-REALLY-IMPLICIT-DECLARATION-AXIOM :ignore)
    (OWLAPI-GET-OWL-PREFIX-DECLARATION-AXIOM :ignore)
    (OWLAPI-GET-OWL-ONTOLOGY-VERSION-DECLARATION-AXIOM :ignore)
    (OWLAPI-GET-OWL-ONTOLOGY-ANNOTATION-AXIOM :ignore)
    (OWLAPI-GET-OWL-OBJECT-SUB-PROPERTY-AXIOM :ignore)
    (OWLAPI-GET-OWL-OBJECT-PROPERTY-RANGE-AXIOM :ignore)
    (OWLAPI-GET-OWL-OBJECT-PROPERTY-DOMAIN-AXIOM :ignore)
    (OWLAPI-GET-OWL-OBJECT-PROPERTY-CHAIN-SUB-PROPERTY-AXIOM :ignore)
    (OWLAPI-GET-OWL-OBJECT-PROPERTY-ASSERTION-AXIOM :ignore)
    (OWLAPI-GET-OWL-NEGATIVE-OBJECT-PROPERTY-ASSERTION-AXIOM :ignore)
    (OWLAPI-GET-OWL-NEGATIVE-DATA-PROPERTY-ASSERTION-AXIOM :ignore)
    (OWLAPI-GET-OWL-IRREFLEXIVE-OBJECT-PROPERTY-AXIOM :ignore)
    (OWLAPI-GET-OWL-INVERSE-OBJECT-PROPERTIES-AXIOM :ignore)
    (OWLAPI-GET-OWL-INVERSE-FUNCTIONAL-OBJECT-PROPERTY-AXIOM :ignore)
    (OWLAPI-GET-OWL-IMPORTS-DECLARATION-AXIOM :ignore)
    (OWLAPI-GET-OWL-IMPLICIT-DECLARATION-AXIOM :ignore)
    (OWLAPI-GET-OWL-HAS-KEY-AXIOM :ignore)
    (OWLAPI-GET-OWL-FUNCTIONAL-OBJECT-PROPERTY-AXIOM :ignore)
    (OWLAPI-GET-OWL-FUNCTIONAL-DATA-PROPERTY-AXIOM :ignore)
    (OWLAPI-GET-OWL-EQUIVALENT-OBJECT-PROPERTIES-AXIOM :ignore)
    (OWLAPI-GET-OWL-EQUIVALENT-DATA-PROPERTIES-AXIOM :ignore)
    (OWLAPI-GET-OWL-EQUIVALENT-CLASSES-AXIOM :ignore)
    (OWLAPI-GET-OWL-ENTITY-ANNOTATION-AXIOM :ignore)
    (OWLAPI-GET-OWL-DISJOINT-UNION-AXIOM :ignore)
    (OWLAPI-GET-OWL-DISJOINT-OBJECT-PROPERTIES-AXIOM :ignore)
    (OWLAPI-GET-OWL-DISJOINT-DATA-PROPERTIES-AXIOM :ignore)
    (OWLAPI-GET-OWL-DISJOINT-CLASSES-AXIOM :ignore)
    (OWLAPI-GET-OWL-DIFFERENT-INDIVIDUALS-AXIOM :ignore)
    (OWLAPI-GET-OWL-DECLARATION-AXIOM :ignore)
    (OWLAPI-GET-OWL-DATATYPE-DEFINITION-AXIOM :ignore)
    (OWLAPI-GET-OWL-DATA-SUB-PROPERTY-AXIOM :ignore)
    (OWLAPI-GET-OWL-DATA-PROPERTY-RANGE-AXIOM :ignore)
    (OWLAPI-GET-OWL-DATA-PROPERTY-DOMAIN-AXIOM :ignore)
    (OWLAPI-GET-OWL-DATA-PROPERTY-ASSERTION-AXIOM :ignore)
    (OWLAPI-GET-OWL-CLASS-ASSERTION-AXIOM :ignore)
    (OWLAPI-GET-OWL-AXIOM-ANNOTATION-AXIOM :ignore)
    (OWLAPI-GET-OWL-ASYMMETRIC-OBJECT-PROPERTY-AXIOM :ignore)
    (OWLAPI-GET-OWL-ANNOTATION-PROPERTY-RANGE-AXIOM :ignore)
    (OWLAPI-GET-OWL-ANNOTATION-PROPERTY-DOMAIN-AXIOM :ignore)
    (OWLAPI-GET-OWL-ANNOTATION-ASSERTION-AXIOM :ignore)
    (OWLAPI-GET-LOADED-ONTOLOGIES :ignore)
    (OWLAPI-GET-INVERSE-PROPERTIES :ignore)
    (OWLAPI-GET-INSTANCES :ignore)
    (OWLAPI-GET-INDIVIDUALS :ignore)
    (OWLAPI-GET-INCONSISTENT-CLASSES :ignore)
    (OWLAPI-GET-EQUIVALENT-PROPERTIES :ignore)
    (OWLAPI-GET-EQUIVALENT-CLASSES :ignore)
    (OWLAPI-GET-DOMAINS :ignore)
    (OWLAPI-GET-DISJOINT-OBJECT-PROPERTIES :ignore)
    (OWLAPI-GET-DISJOINT-DATA-PROPERTIES :ignore)
    (OWLAPI-GET-DISJOINT-CLASSES :ignore)
    (OWLAPI-GET-DIFFERENT-INDIVIDUALS :ignore)
    (OWLAPI-GET-DESCENDANT-PROPERTIES :ignore)
    (OWLAPI-GET-DESCENDANT-CLASSES :ignore)
    (OWLAPI-GET-DATA-PROPERTY-VALUES :ignore)
    (OWLAPI-GET-DATA-PROPERTY-RELATIONSHIPS :ignore)
    (OWLAPI-GET-CURRENT-REASONER :ignore)
    (OWLAPI-GET-CHANGES :ignore)
    (OWLAPI-GET-AXIOMS-PER-ONTOLOGY :ignore)
    (OWLAPI-GET-AXIOMS-OF-TYPE-IN :ignore)
    (OWLAPI-GET-AXIOMS-OF-TYPE :ignore)
    (OWLAPI-GET-AXIOMS-IN :ignore)
    (OWLAPI-GET-AXIOMS :ignore)
    (OWLAPI-GET-AXIOM-COUNTER :ignore)
    (OWLAPI-GET-AUTO-ONTOLOGY :ignore)
    (OWLAPI-GET-AUTO-DECLARE-DATA-PROPERTIES :ignore)
    (OWLAPI-GET-ANNOTATION-AXIOMS-FOR-AXIOM :ignore)
    (OWLAPI-GET-ANCESTOR-PROPERTIES :ignore)
    (OWLAPI-GET-ANCESTOR-CLASSES :ignore)
    (OWLAPI-GET-ALL-ONTOLOGIES :ignore)
    (OWLAPI-FIND-OBJECT-FROM-ID :ignore)
    (OWLAPI-FIND-ID-FROM-OBJECT :ignore)
    (OWLAPI-EXPORT-REASONER :ignore)
    (OWLAPI-EXPORT-ONTOLOGY :ignore)
    (OWLAPI-ENABLE-TRANSIENT-AXIOM-MODE :ignore)
    (OWLAPI-ENABLE-SIMPLIFIED-PROTOCOL :ignore)
    (OWLAPI-ENABLE-MEMORY-SAVING-MODE :ignore)
    (OWLAPI-ENABLE-LOOKUP-MODE :ignore)
    (OWLAPI-ENABLE-INCREMENTAL-UPDATES :ignore)
    (OWLAPI-DONT-REGISTER-REFERENCED-ENTITIES :ignore)
    (OWLAPI-DONT-REGISTER-DECLARED-ENTITIES :ignore)
    (OWLAPI-DISPOSE-REASONER :ignore)
    (OWLAPI-DISPOSE-ONTOLOGY :ignore)
    (OWLAPI-DISPOSE-ONTOLOGIES :ignore)
    (OWLAPI-DISPOSE-AXIOMS :ignore)
    (OWLAPI-DISPOSE-AXIOM :ignore)
    (OWLAPI-DISPOSE :ignore)
    (OWLAPI-DISABLE-TRANSIENT-AXIOM-MODE :ignore)
    (OWLAPI-DISABLE-SIMPLIFIED-PROTOCOL :ignore)
    (OWLAPI-DISABLE-MEMORY-SAVING-MODE :ignore)
    (OWLAPI-DISABLE-LOOKUP-MODE :ignore)
    (OWLAPI-DISABLE-INCREMENTAL-UPDATES :ignore)
    (OWLAPI-DISABLE-AUTO-MODE :ignore)
    (OWLAPI-DESCRIBE-REASONERS :ignore)
    (OWLAPI-DESCRIBE-REASONER :ignore)
    (OWLAPI-DESCRIBE-ONTOLOGY :ignore)
    (OWLAPI-DESCRIBE-ONTOLOGIES :ignore)
    (OWLAPI-CONTAINS :ignore)
    (OWLAPI-CONSIDER-DECLARATIONS :ignore)
    (OWLAPI-CLEAR-REGISTRY :ignore)
    (OWLAPI-CLEAR-ONTOLOGIES :ignore)
    (OWLAPI-CLEAR-CHANGES :ignore)
    (OWLAPI-CLASSIFY :ignore)
    (OWLAPI-BATCH-SYNCHRONIZE :ignore)
    (OWLAPI-AUTO-REMOVE-AXIOMS-FROM :ignore)
    (OWLAPI-AUTO-BATCH-REMOVE-AXIOMS-FROM :ignore)
    (OWLAPI-AUTO-BATCH-ADD-AXIOMS-TO :ignore)
    (OWLAPI-AUTO-APPLY-CHANGES :ignore)
    (OWLAPI-AUTO-ADD-AXIOMS-TO :ignore)
    (OWLAPI-APPLY-CHANGES :ignore)
    (OWLAPI-ADVANCE-PROGRESS :ignore)
    (OWLAPI-ADD-PREFIX :ignore)
    (OWLAPI-ABORT :ignore)
    (OWLAPI-SET-ONTOLOGY-URI :ignore)
    (OWLAPI-REMOVE-AXIOMS :ignore)
    (OWLAPI-REMOVE-AXIOM :ignore)
    (OWLAPI-ID-TO-AXIOM :ignore)
    (OWLAPI-AXIOM-TO-ID :ignore)
    (OWLAPI-AXIOM-LOADED? :ignore)
    (OWLAPI-ADD-AXIOMS :ignore)
    (OWLAPI-ADD-AXIOM :ignore)))

;;;
;;; Argument Types 
;;; 

(defconstant +known-args-and-types+ 

  ;;;
  ;;; muss ab und zu aktualisiert werden: 
  ;;; einmal (create-docgen-stubs t) laufen lassen, 
  ;;; dann: 
  ;;; (sort (mapcar #'(lambda (x) (list (first x) :unknown (second x))) *all-args*) #'string< :key #'(lambda (x) (symbol-name (first x))))


  '((A :NRQL-QUERY-ID QUERY-ENTAILS-P QUERY-EQUIVALENT-P)
    (A :ABOX-NAME ABOX-ENTAILS-ABOX-P COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (A :SUBSTRATE-LABEL-DESCRIPTION DESCRIPTION-IMPLIES-P)
    (THEMATIC-SUBSTRATE::ABOX :ABOX-NAME
                              WITH-NRQL-SETTINGS-EVALUATED
                              WITH-NRQL-SETTINGS
                              APPLICABLE-RULES
                              EXECUTE-APPLICABLE-RULES
                              GET-DAG-OF-QBOX-FOR-ABOX
                              GET-NODES-IN-QBOX-FOR-ABOX
                              SHOW-QBOX-FOR-ABOX
                              UNAPPLICABLE-RULES
                              ABORT-ALL-QUERIES
                              ABORT-ALL-RULES
                              ACCURATE-QUERIES
                              ACCURATE-RULES
                              ACTIVE-QUERIES
                              ACTIVE-RULES
                              ALL-DIFFERENT-FROM-ASSERTIONS
                              ALL-QUERIES
                              ALL-RULES
                              ALL-SAME-AS-ASSERTIONS
                              ALL-SUBSTRATES
                              CHEAP-QUERIES
                              CHEAP-RULES
                              CHECK-NRQL-SUBSCRIPTIONS
                              CREATE-DATA-EDGE
                              CREATE-DATA-NODE
                              DATA-EDGE1
                              DATA-NODE1
                              DEL-DATA-EDGE1
                              DEL-DATA-NODE1
                              DELETE-ALL-QUERIES
                              DELETE-ALL-RULES
                              DELETE-ALL-SUBSTRATES
                              DELETE-DATA-EDGE
                              DELETE-DATA-NODE
                              DESCRIBE-ALL-EDGES
                              DESCRIBE-ALL-NODES
                              DESCRIBE-ALL-QUERIES
                              DESCRIBE-ALL-RULES
                              DESCRIBE-SUBSTRATE
                              EDGE-DESCRIPTION1
                              EDGE-LABEL1
                              EXECUTE-ALL-QUERIES
                              EXECUTE-ALL-RULES
                              EXECUTE-OR-REEXECUTE-ALL-QUERIES
                              EXECUTE-OR-REEXECUTE-ALL-RULES
                              EXPENSIVE-QUERIES
                              EXPENSIVE-RULES
                              GET-ABOX-GRAPH
                              GET-ALL-ANSWERS
                              GET-DATA-EDGE-DESCRIPTION
                              GET-DATA-EDGE-LABEL
                              GET-DATA-NODE-DESCRIPTION
                              GET-DATA-NODE-LABEL
                              GET-EDGE-LABEL-FOR-NON-EXISTENT-EDGES
                              GET-INDIVIDUAL-ANNOTATION-DATATYPE-FILLERS
                              GET-INDIVIDUAL-ANNOTATION-FILLERS
                              GET-INDIVIDUAL-DATATYPE-FILLERS
                              GET-INDIVIDUAL-SUCCESSORS
                              GET-SUBSTRATE-EDGES
                              GET-SUBSTRATE-NODES
                              INACCURATE-QUERIES
                              INACCURATE-RULES
                              MAKE-FORWARD-RULE-FROM-ABOXES
                              NODE-DESCRIPTION1
                              NODE-LABEL1
                              PREPARE-NRQL-ENGINE
                              PROCESSED-QUERIES
                              PROCESSED-RULES
                              RACER-ANSWER-QUERY
                              RACER-ANSWER-QUERY-UNDER-PREMISE
                              RACER-ANSWER-QUERY-UNDER-PREMISE1
                              RACER-ANSWER-QUERY1
                              RACER-APPLY-RULE
                              RACER-APPLY-RULE-UNDER-PREMISE
                              RACER-APPLY-RULE-UNDER-PREMISE1
                              RACER-APPLY-RULE1
                              RACER-PREPARE-QUERY
                              RACER-PREPARE-QUERY1
                              RACER-PREPARE-RULE
                              RACER-PREPARE-RULE1
                              RCC-CONSISTENT-P
                              READY-QUERIES
                              READY-RULES
                              REEXECUTE-ALL-QUERIES
                              REEXECUTE-ALL-RULES
                              REMOVE-IMPLIED-CONCEPT-ASSERTIONS
                              RESET-ALL-SUBSTRATES
                              RUNNING-CHEAP-QUERIES
                              RUNNING-CHEAP-RULES
                              RUNNING-EXPENSIVE-QUERIES
                              RUNNING-EXPENSIVE-RULES
                              RUNNING-QUERIES
                              RUNNING-RULES
                              SET-EDGE-LABEL-FOR-NON-EXISTENT-EDGES
                              WAITING-CHEAP-QUERIES
                              WAITING-CHEAP-RULES
                              WAITING-EXPENSIVE-QUERIES
                              WAITING-EXPENSIVE-RULES
                              WAITING-QUERIES
                              WAITING-RULES)
    (ABOX
     :ABOX-NAME
     CLONE-ABOX
     CONCEPT-INSTANCES
     DELETE-ABOX
     DIRECT-PREDECESSORS
     INDIVIDUAL-ATTRIBUTE-FILLERS
     INDIVIDUAL-DIRECT-TYPES
     INDIVIDUAL-FILLED-ROLES
     INDIVIDUAL-FILLERS
     INDIVIDUAL-INSTANCE?
     INDIVIDUAL-TOLD-ATTRIBUTE-VALUE
     INDIVIDUAL-TOLD-DATATYPE-FILLERS
     INDIVIDUAL-TYPES
     INDIVIDUALS-EQUAL?
     INDIVIDUALS-NOT-EQUAL?
     INDIVIDUALS-RELATED?
     INIT-PUBLICATIONS
     INIT-SUBSCRIPTIONS
     PUBLISH
     SUBSCRIBE
     TIMENET-RETRIEVE
     UNPUBLISH
     UNSUBSCRIBE
     ABOX-CONSISTENT-IF-ASSERTIONS-ADDED-P
     ABOX-CONSISTENT-P
     ABOX-PREPARED-P
     ABOX-REALIZED-P
     ABOX-UNA-CONSISTENT-P
     ADD-ALL-DIFFERENT-ASSERTION
     ADD-ANNOTATION-CONCEPT-ASSERTION
     ADD-ANNOTATION-ROLE-ASSERTION
     ADD-ATTRIBUTE-ASSERTION
     ADD-CONCEPT-ASSERTION
     ADD-CONSTRAINT-ASSERTION
     ADD-DATATYPE-ROLE-FILLER
     ADD-DIFFERENT-FROM-ASSERTION
     ADD-EVENT-ASSERTION
     ADD-EVENT-RULE
     ADD-NEGATED-ROLE-ASSERTION
     ADD-NEGATIVE-DATATYPE-ROLE-FILLER
     ADD-ROLE-ASSERTION
     ADD-RULE-AXIOM
     ADD-SAME-INDIVIDUAL-AS-ASSERTION
     ALL-ANNOTATION-CONCEPT-ASSERTIONS
     ALL-ANNOTATION-ROLE-ASSERTIONS
     ALL-ATTRIBUTE-ASSERTIONS
     ALL-CONCEPT-ASSERTIONS
     ALL-CONCEPT-ASSERTIONS-FOR-INDIVIDUAL
     ALL-CONSTRAINTS
     ALL-INDIVIDUALS
     ALL-ROLE-ASSERTIONS
     ALL-ROLE-ASSERTIONS-FOR-INDIVIDUAL-IN-DOMAIN
     ALL-ROLE-ASSERTIONS-FOR-INDIVIDUAL-IN-RANGE
     ASSOCIATED-TBOX
     CD-OBJECT-P
     CHECK-ABOX-COHERENCE
     CHECK-SUBSCRIPTIONS
     COMPUTE-ALL-IMPLICIT-ROLE-FILLERS
     COMPUTE-IMPLICIT-ROLE-FILLERS
     COMPUTE-INDEX-FOR-INSTANCE-RETRIEVAL
     CONSTRAINT-ENTAILED-P
     CREATE-ABOX-CLONE
     DESCRIBE-ABOX
     DESCRIBE-INDIVIDUAL
     DESCRIBE-INDIVIDUAL1
     ENSURE-SUBSUMPTION-BASED-QUERY-ANSWERING
     FORGET-ABOX
     FORGET-ALL-DIFFERENT-ASSERTION
     FORGET-ANNOTATION-CONCEPT-ASSERTION
     FORGET-CONCEPT-ASSERTION
     FORGET-CONSTRAINED-ASSERTION
     FORGET-CONSTRAINT
     FORGET-DATATYPE-ROLE-FILLER
     FORGET-DIFFERENT-FROM-ASSERTION
     FORGET-INDIVIDUAL
     FORGET-NEGATED-ROLE-ASSERTION
     FORGET-NEGATIVE-DATATYPE-ROLE-FILLER
     FORGET-ROLE-ASSERTION
     FORGET-SAME-INDIVIDUAL-AS-ASSERTION
     FORGET-STATEMENT
     GET-ABOX-LANGUAGE
     GET-ABOX-SIGNATURE
     GET-ABOX-VERSION
     GET-INDIVIDUAL-PMODEL
     INDIVIDUAL-INSTANCE-P
     INDIVIDUAL-P
     INDIVIDUALS-EQUAL-P
     INDIVIDUALS-NOT-EQUAL-P
     INDIVIDUALS-RELATED-P
     INIT-ABOX
     INIT-PUBLICATIONS-1
     INIT-SUBSCRIPTIONS-1
     INSTANTIATORS
     INTERNAL-INDIVIDUALS-RELATED-P
     MATERIALIZE-INFERENCES
     MOST-SPECIFIC-INSTANTIATORS
     MSC-K
     PREPARE-ABOX
     PREPARE-RACER-ENGINE
     PRINT-ABOX-INDIVIDUALS
     PUBLISH-1
     REALIZE-ABOX
     RECOGNIZE-EVENTS
     RETRIEVE-CONCEPT-INSTANCES
     RETRIEVE-DIRECT-PREDECESSORS
     RETRIEVE-INDIVIDUAL-ANNOTATION-PROPERTY-FILLERS
     RETRIEVE-INDIVIDUAL-ANTONYMS
     RETRIEVE-INDIVIDUAL-ATTRIBUTE-FILLERS
     RETRIEVE-INDIVIDUAL-FILLED-ROLES
     RETRIEVE-INDIVIDUAL-FILLERS
     RETRIEVE-INDIVIDUAL-SYNONYMS
     RETRIEVE-INDIVIDUAL-TOLD-ATTRIBUTE-VALUE
     RETRIEVE-INDIVIDUAL-TOLD-DATATYPE-FILLERS
     RETRIEVE-RELATED-INDIVIDUALS
     SAVE-ABOX
     SAVE-KB
     SET-ATTRIBUTE-FILLER
     SET-CURRENT-ABOX
     STORE-ABOX-IMAGE
     SUBSCRIBE-1
     SWRL-FORWARD-CHAINING
     TIMENET-ANSWER-QUERY
     TOLD-VALUE
     UNPUBLISH-1
     UNSUBSCRIBE-1
     VERIFY-WITH-ABOX-INDIVIDUALS-LIST)
    (THEMATIC-SUBSTRATE::ABOX-ENTAILMENT (:ONE-OF :MOST-SPECIFIC :MOST-GENERAL) ADD-EXPLANATION-ASSERTIONS GET-EXPLANATIONS)
    (THEMATIC-SUBSTRATE::ABOX-MIRRORING (:ONE-OF NIL T :SMART :VERY-SMART) WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (ABOX-NAME :ABOX-NAME
                      ABOX-CONSISTENT?
                      ABOX-PREPARED?
                      ABOX-REALIZED?
                      ABOX-UNA-CONSISTENT?
                      CD-OBJECT?
                      CONSTRAINT-ENTAILED?
                      DEFINE-ABOX
                      IN-ABOX
                      INDIVIDUAL-ANTONYMS
                      INDIVIDUAL-SYNONYMS
                      INDIVIDUAL?
                      RELATED-INDIVIDUALS)
    (ABOX-NAME-OR-ABOX :ABOX-NAME ENSURE-ABOX-SIGNATURE FIND-ABOX)
    (ABOX-NAME1 :ABOX-NAME SET-FIND-ABOX)
    (ABOX-NAME2 :ABOX-NAME SET-FIND-ABOX)
    (THEMATIC-SUBSTRATE::ABOX-OR-NAME (:or :ABOX-NAME :ABOX) COMPUTE-SUBGRAPH-ABOXES CREATE-SUBGRAPH-ABOXES MAKE-QUERY-FROM-ABOX)
    (ABOX-TOLD-ONLY-P :BOOLEAN MATERIALIZE-INFERENCES)
    (THEMATIC-SUBSTRATE::ABOXES (:LIST :abox) GET-MAXIMUM GET-MINIMUM)
    (ABOXES (:LIST :ABOX-NAME) STORE-ABOXES-IMAGE)
    (THEMATIC-SUBSTRATE::ACTIVE-P :BOOLEAN GET-ALL-ANSWERS)
    (ADD-RULE-CONSEQUENCES-AUTOMATICALLY :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::ADD-RULE-CONSEQUENCES-P :BOOLEAN
                                                 EXECUTE-OR-REEXECUTE-QUERY
                                                 EXECUTE-QUERY
                                                 EXECUTE-ALL-QUERIES
                                                 EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                 RACER-ANSWER-QUERY
                                                 RACER-ANSWER-QUERY-UNDER-PREMISE
                                                 RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                 RACER-ANSWER-QUERY1
                                                 RACER-ANSWER-TBOX-QUERY
                                                 RACER-ANSWER-TBOX-QUERY1
                                                 RACER-PREPARE-QUERY
                                                 RACER-PREPARE-QUERY1
                                                 RACER-PREPARE-TBOX-QUERY
                                                 RACER-PREPARE-TBOX-QUERY1)
    (ADD-STANDARD-PREFIXES :BOOLEAN SPARQL-ANSWER-QUERY SPARQL-RETRIEVE)
    (THEMATIC-SUBSTRATE::ALL-ASSERTIONS-P :BOOLEAN ADD-EXPLANATION-ASSERTIONS GET-EXPLANATIONS)
    (ALL-DIFFERENT-P :BOOLEAN MATERIALIZE-INFERENCES)
    (THEMATIC-SUBSTRATE::ALLOW-MULTIPLE-DEFINITIONS-P :BOOLEAN DEFINE-AND-EXECUTE-QUERY DEFINE-AND-PREPARE-QUERY DEFINE-QUERY)
    (THEMATIC-SUBSTRATE::ALSO-UNMAPPED-DIFFERENCES-P :BOOLEAN COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (OWLAPI:ANNOTATION :STRING |OWLAPI-getOWLAxiomAnnotationAxiom| |OWLAPI-getOWLEntityAnnotationAxiom| |OWLAPI-getOWLOntologyAnnotationAxiom|)
    (ANNOTATION-P :BOOLEAN ADD-ROLE-AXIOM ADD-ROLE-AXIOMS FORGET-ROLE-AXIOMS)
    (OWLAPI:ANNOTATION-PROPERTY :ANNOTATION-PROPERTY |OWLAPI-getOWLAnnotationAssertionAxiom| |OWLAPI-getOWLAnnotationPropertyDomainAxiom| |OWLAPI-getOWLAnnotationPropertyRangeAxiom|)
    (OWLAPI:ANNOTATION-PROPERTY-DOMAIN :CONCEPT-EXPRESSION |OWLAPI-getOWLAnnotationPropertyDomainAxiom|)
    (OWLAPI:ANNOTATION-PROPERTY-RANGE :CONCEPT-EXPRESSION |OWLAPI-getOWLAnnotationPropertyRangeAxiom|)
    (OWLAPI:ANNOTATION-SUB-PROPERTY :ROLE |OWLAPI-getOWLSubAnnotationPropertyOfAxiom|)
    (OWLAPI:ANNOTATION-SUBJECT :ABOX-INDIVIDUAL |OWLAPI-getOWLAnnotationAssertionAxiom|)
    (OWLAPI:ANNOTATION-SUPER-PROPERTY :ROLE |OWLAPI-getOWLSubAnnotationPropertyOfAxiom|)
    (OWLAPI:ANNOTATION-VALUE :STRING |OWLAPI-getOWLAnnotationAssertionAxiom|)
    (ANONYMIZED :BOOLEAN SAVE-TBOX)
    (THEMATIC-SUBSTRATE::ARGLIST :LAMBDA-LIST DEFINE1)
    (OWL-SYNTAXES::ARGS (:LIST :OWLAPI-AXIOM-CONSTRUCTOR-CALL) |OWLAPI-parse|)
    (THEMATIC-SUBSTRATE::ARGS :IGNORE RMI)
    (THEMATIC-SUBSTRATE::ARGS (:GOTO RACER-PREPARE-QUERY) RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (ARGS (:GOTO ADD-DATATYPE-PROPERTY) DEFINE-DATATYPE-PROPERTY)
    (ARGS (:or :ABOX-NAME (:list (:one-of :INIT) :boolean)) IN-KNOWLEDGE-BASE)
    (ARGS (:GOTO PRACER-ANSWER-QUERY) PRETRIEVE)
    (ARGS (:GOTO "db.agraph.sparql::run-sparql") SPARQL-ANSWER-QUERY SPARQL-RETRIEVE)
    (ARGS (:GOTO ADD-ROLE-AXIOMS) ADD-DATATYPE-PROPERTY)
    (ARGS (:GOTO ADD-RESULT-CONCEPT-AXIOM) MSC-K)
    (ARGS (:GOTO OWL-READ-DOCUMENT-1) OWL-READ-DOCUMENT)
    (ARGS (:GOTO OWL-SYNTAXES:OWLLINK-READ-DOCUMENT1) OWLLINK-READ-DOCUMENT)
    (ARGS (:GOTO OWL-SYNTAXES:OWLLINK-READ-FILE1) OWLLINK-READ-FILE)
    (ARGS (:GOTO MATERIALIZE-INFERENCES) SAVE-ONTOLOGY-TO-TRIPLE-STORE)
    (THEMATIC-SUBSTRATE::ARITY :NON-NEGATIVE-INTEGER ACTIVATE-DEFINED-QUERY DEACTIVATE-DEFINED-QUERY DESCRIBE-DEFINITION UNDEFINE-QUERY)
    (ASK-OWLAPI-P :BOOLEAN GET-PREFIXES)
    (ASSERTION :ABOX-ASSERTION DEFINE-EVENT-ASSERTION ADD-EVENT-ASSERTION)
    (ASSERTIONS (:LIST :ABOX-ASSERTION) FORGET ABOX-CONSISTENT-IF-ASSERTIONS-ADDED-P FORGET-STATEMENT)
    (ASYMMETRIC :BOOLEAN DEFINE-PRIMITIVE-ATTRIBUTE DEFINE-PRIMITIVE-ROLE ADD-ROLE-AXIOM ADD-ROLE-AXIOMS)
    (ASYMMETRIC-P :BOOLEAN ADD-ROLE-AXIOM ADD-ROLE-AXIOMS)
    (ATOMIC-CONCEPTS (:LIST :CONCEPT-NAME) SIGNATURE ENSURE-TBOX-SIGNATURE)
    (ATTRIBUTE
     :CD-ATTRIBUTE
     ATTRIBUTE-FILLER
     CD-ATTRIBUTE?
     CONSTRAINED
     INDIVIDUAL-ATTRIBUTE-FILLERS
     INDIVIDUAL-TOLD-ATTRIBUTE-VALUE
     ADD-ATTRIBUTE-ASSERTION
     CD-ATTRIBUTE-P
     RETRIEVE-INDIVIDUAL-ATTRIBUTE-FILLERS
     RETRIEVE-INDIVIDUAL-TOLD-ATTRIBUTE-VALUE
     SET-ATTRIBUTE-FILLER)
    (ATTRIBUTE-NAME :CD-ATTRIBUTE ATTRIBUTE-DOMAIN ATTRIBUTE-DOMAIN-1 ATTRIBUTE-TYPE)
    (ATTRIBUTE-TERM :CD-ATTRIBUTE FORGET-CONSTRAINED-ASSERTION)
    (ATTRIBUTES (:LIST :CD-ATTRIBUTE) SIGNATURE ENSURE-TBOX-SIGNATURE)
    (THEMATIC-SUBSTRATE::AUTO-CORRESPONDANCES-P :BOOLEAN COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (AVOID-DUPLICATE-DEFINITIONS :BOOLEAN SAVE-TBOX)
    (OWLAPI:AXIOM (:or :OWLAPI-AXIOM-ID :OWLAPI-AXIOM-CONSTRUCTOR-CALL) |OWLAPI-AddAxiom| |OWLAPI-loadAxiom| |OWLAPI-RemoveAxiom| |OWLAPI-unloadAxiom|)
    (OWLAPI::AXIOM-CONSTRUCTOR-CALL :OWLAPI-AXIOM-CONSTRUCTOR-CALL |OWLAPI-AxiomToID|)
    (OWLAPI:AXIOM-ID :OWLAPI-AXIOM-ID |OWLAPI-getAnnotationAxiomsForAxiom| |OWLAPI-getOWLAxiomAnnotationAxiom|)
    (OWLAPI::AXIOM-ID-OR-CONSTRUCTOR (:or :OWLAPI-AXIOM-ID :OWLAPI-AXIOM-CONSTRUCTOR-CALL) |OWLAPI-isEntailed|)
    (AXIOMS (:LIST :ABOX-ASSERTION) DEFINE-ABOX)
    (AXIOMS (:LIST :TBOX-AXIOM) DEFINE-TBOX)
    (OWLAPI:AXIOMS (:LIST (:or :OWLAPI-AXIOM-ID :OWLAPI-AXIOM-CONSTRUCTOR-CALL)) |OWLAPI-AddAxioms| |OWLAPI-loadAxioms| |OWLAPI-RemoveAxioms| |OWLAPI-unloadAxioms|)
    (THEMATIC-SUBSTRATE::B :NRQL-QUERY-ID QUERY-ENTAILS-P QUERY-EQUIVALENT-P)
    (THEMATIC-SUBSTRATE::B :ABOX ABOX-ENTAILS-ABOX-P COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (THEMATIC-SUBSTRATE::B :SUBSTRATE-LABEL-DESCRIPTION DESCRIPTION-IMPLIES-P)
    (THEMATIC-SUBSTRATE::BACKWARD-RULE-P :BOOLEAN MAKE-ABDUCTION-RULE-FROM-ABOXES)
    (BACKWARD-RULE-P :BOOLEAN DEFINE-RULE ADD-RULE-AXIOM)
    (THEMATIC-SUBSTRATE::BIND-SPECIALS-P
     :IGNORE
     MAKE-FORWARD-RULE-FROM-ABOXES
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::BINDING-VALIDATOR :IGNORE COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2 ENABLE-ABDUCTION RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (THEMATIC-SUBSTRATE::BINDINGS (:list (:list :nrql-var (:or :abox-individual :substrate-node)) )
                                  WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::BODY :NRQL-QUERY-BODY DEFINE-AND-EXECUTE-QUERY DEFINE-AND-PREPARE-QUERY DEFINE-QUERY)
    (BODY :EVENT-RULE-BODY DEFINE-EVENT-RULE PRETRIEVE ADD-EVENT-RULE)
    (THEMATIC-SUBSTRATE::BROWSING-MODE-P :BOOLEAN GET-ABOX-GRAPH)
    (BYTES (:LIST :BYTE) ADD-DOC-IMAGE-DATA1)
    (THEMATIC-SUBSTRATE::C-MODE :IGNORE COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2 ENABLE-ABDUCTION RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (THEMATIC-SUBSTRATE::CANDIDATE-INDIVIDUALS (:LIST :ABOX-INDIVIDUAL) COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2 ENABLE-ABDUCTION RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (CANDIDATES (:LIST :ABOX-INDIVIDUAL) CONCEPT-INSTANCES RETRIEVE-CONCEPT-INSTANCES)
    (CD-ATTRIBUTE :CD-ATTRIBUTE ADD-ROLE-AXIOM ADD-ROLE-AXIOMS FORGET-ROLE-AXIOMS)
    (THEMATIC-SUBSTRATE::CHECK-ABOX-CONSISTENCY :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (OWLAPI::CHECK-ABOX-CONSISTENCY-P :BOOLEAN |OWLAPI-classify| |OWLAPI-realize|)
    (THEMATIC-SUBSTRATE::CHECK-ABOX-CONSISTENCY-P :BOOLEAN
                                                  EXECUTE-OR-REEXECUTE-QUERY
                                                  EXECUTE-QUERY
                                                  EXECUTE-ALL-QUERIES
                                                  EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                  RACER-ANSWER-QUERY
                                                  RACER-ANSWER-QUERY-UNDER-PREMISE
                                                  RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                  RACER-ANSWER-QUERY1
                                                  RACER-ANSWER-TBOX-QUERY
                                                  RACER-ANSWER-TBOX-QUERY1
                                                  RACER-PREPARE-QUERY
                                                  RACER-PREPARE-QUERY1
                                                  RACER-PREPARE-TBOX-QUERY
                                                  RACER-PREPARE-TBOX-QUERY1)
    (TYPE 
     (:ONE-OF THEMATIC-SUBSTRATE::data-substrate 
      THEMATIC-SUBSTRATE::mirror-data-substrate 
      THEMATIC-SUBSTRATE::rcc-substrate 
      THEMATIC-SUBSTRATE::rcc-mirror-substrate) SET-SUBSTRATE-TYPE)
    (CHECK-P :BOOLEAN INTERNAL-INDIVIDUALS-RELATED-P)
    (CLASS :CONCEPT-EXPRESSION |OWLAPI-getIndividuals| |OWLAPI-getInstances|)
    (THEMATIC-SUBSTRATE::CLASSIFY-CONCEPTS-IN-INSTANCE-ASSERTIONS-P :BOOLEAN
                                                                    EXECUTE-OR-REEXECUTE-QUERY
                                                                    EXECUTE-QUERY
                                                                    EXECUTE-ALL-QUERIES
                                                                    EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                                    RACER-ANSWER-QUERY
                                                                    RACER-ANSWER-QUERY-UNDER-PREMISE
                                                                    RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                                    RACER-ANSWER-QUERY1
                                                                    RACER-ANSWER-TBOX-QUERY
                                                                    RACER-ANSWER-TBOX-QUERY1
                                                                    RACER-PREPARE-QUERY
                                                                    RACER-PREPARE-QUERY1
                                                                    RACER-PREPARE-TBOX-QUERY
                                                                    RACER-PREPARE-TBOX-QUERY1)
    (CLASSIFY-TBOX-P :BOOLEAN PREPARE-RACER-ENGINE)
    (OWLAPI:CLS :CONCEPT-EXPRESSION |OWLAPI-getAncestorClasses| |OWLAPI-getDescendantClasses| |OWLAPI-getEquivalentClasses| |OWLAPI-getSubClasses| |OWLAPI-getSuperClasses| |OWLAPI-isDefinedClass|)
    (OWLAPI:CLSC :CONCEPT-EXPRESSION |OWLAPI-isClass| |OWLAPI-isEquivalentClass| |OWLAPI-isSubClassOf|)
    (OWLAPI:CLSD :CONCEPT-EXPRESSION |OWLAPI-isEquivalentClass| |OWLAPI-isSubClassOf|)
    (OWL-SYNTAXES::COMMENTS :BOOLEAN |OWLAPI-saveOntology| |OWLAPI-writeFunctionalOntologyFile| |OWLAPI-writeOntologyFile| |OWLAPI-writeXMLOntologyFile|)
    (THEMATIC-SUBSTRATE::COMMON-AS-STRICT-ATOMS-P :BOOLEAN MAKE-ABDUCTION-RULE-FROM-ABOXES MAKE-BACKWARD-RULE-FROM-ABOXES MAKE-FORWARD-RULE-FROM-ABOXES MAKE-QUERY-FROM-ABOX)
    (THEMATIC-SUBSTRATE::COMMON-ASSERTIONS-AS-STRICT-ATOMS-P :BOOLEAN COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (THEMATIC-SUBSTRATE::COMMON-CONCEPT-ASSERTIONS (:LIST :CONCEPT-ASSERTION) MAKE-ABDUCTION-RULE-FROM-ABOXES MAKE-BACKWARD-RULE-FROM-ABOXES MAKE-FORWARD-RULE-FROM-ABOXES MAKE-QUERY-FROM-ABOX)
    (THEMATIC-SUBSTRATE::COMMON-DIFFERENT-FROM-ASSERTIONS (:LIST :DIFFERENT-FROM-ASSERTION) MAKE-ABDUCTION-RULE-FROM-ABOXES MAKE-BACKWARD-RULE-FROM-ABOXES MAKE-FORWARD-RULE-FROM-ABOXES MAKE-QUERY-FROM-ABOX)
    (THEMATIC-SUBSTRATE::COMMON-ROLE-ASSERTIONS (:LIST :ROLE-ASSERTION) MAKE-ABDUCTION-RULE-FROM-ABOXES MAKE-BACKWARD-RULE-FROM-ABOXES MAKE-FORWARD-RULE-FROM-ABOXES MAKE-QUERY-FROM-ABOX)
    (THEMATIC-SUBSTRATE::COMMON-SAME-AS-ASSERTIONS (:LIST :SAME-AS-ASSERTION) MAKE-ABDUCTION-RULE-FROM-ABOXES MAKE-BACKWARD-RULE-FROM-ABOXES MAKE-FORWARD-RULE-FROM-ABOXES MAKE-QUERY-FROM-ABOX)
    (THEMATIC-SUBSTRATE::CONCEPT :CONCEPT-EXPRESSION CHECK-CONCEPT-COHERENCE GET-CONCEPT-PROPERTIES)
    (OWLAPI::CONCEPT :CONCEPT-EXPRESSION |OWLAPI-getDisjointClasses|)
    (CONCEPT
     :CONCEPT-EXPRESSION
     DEFINE-DISTINCT-INDIVIDUAL
     DEFINE-INDIVIDUAL
     DOMAIN
     INDIVIDUAL-INSTANCE?
     INSTANCE
     RANGE
     ADD-ANNOTATION-CONCEPT-ASSERTION
     ADD-CONCEPT-ASSERTION
     FORGET-ANNOTATION-CONCEPT-ASSERTION
     FORGET-CONCEPT-ASSERTION
     INDIVIDUAL-INSTANCE-P
     ROLE-HAS-DOMAIN
     ROLE-HAS-RANGE)
    (CONCEPT-1 :CONCEPT-EXPRESSION CONCEPT-DISJOINT? CONCEPT-EQUIVALENT? CONCEPT-SATISFIABLE? CONCEPT-SUBSUMES? CONCEPT-DISJOINT-P CONCEPT-EQUIVALENT-P LCS-UNFOLD)
    (CONCEPT-2 :CONCEPT-EXPRESSION CONCEPT-DISJOINT? CONCEPT-EQUIVALENT? CONCEPT-SUBSUMES? CONCEPT-DISJOINT-P CONCEPT-EQUIVALENT-P LCS-UNFOLD)
    (CONCEPT-EXPR :CONCEPT-EXPRESSION GET-CONCEPT-PMODEL)
    (CONCEPT-MAPPING :BOOLEAN PRINT-ABOX-INDIVIDUALS)
    (CONCEPT-NAME
     :CONCEPT-NAME
     CONCEPT-IS-PRIMITIVE?
     CONCEPT?
     GET-CONCEPT-DEFINITION
     GET-CONCEPT-NEGATED-DEFINITION
     ADD-DISJOINTNESS-AXIOM
     CONCEPT-IS-PRIMITIVE-P
     CONCEPT-P
     DESCRIBE-CONCEPT
     FORGET-DISJOINTNESS-AXIOM
     GET-CONCEPT-DEFINITION-1
     GET-CONCEPT-NEGATED-DEFINITION-1)
    (CONCEPT-NAMES (:LIST :concept-name) DISJOINT)
    (CONCEPT-TERM :CONCEPT-EXPRESSION
                         CONCEPT-ANCESTORS
                         CONCEPT-CHILDREN
                         CONCEPT-DESCENDANTS
                         CONCEPT-INSTANCES
                         CONCEPT-PARENTS
                         CONCEPT-SYNONYMS
                         ALC-CONCEPT-COHERENT
                         ATOMIC-CONCEPT-ANCESTORS
                         ATOMIC-CONCEPT-CHILDREN
                         ATOMIC-CONCEPT-DESCENDANTS
                         ATOMIC-CONCEPT-PARENTS
                         ATOMIC-CONCEPT-SYNONYMS
                         CONCEPT-SATISFIABLE-P
                         RETRIEVE-CONCEPT-INSTANCES)
    (CONCEPT1 :CONCEPT-EXPRESSION LCS)
    (CONCEPT2 :CONCEPT-EXPRESSION LCS)
    (CONCEPTS (:LIST :CONCEPT-NAME) DECLARE-DISJOINT FORGET-DISJOINTNESS-AXIOM-STATEMENT)
    (THEMATIC-SUBSTRATE::CONSIDER-HEAD-ATOM-FOR-CONSISTENCY-CHECK-P :BOOLEAN DEFINE-AND-EXECUTE-QUERY DEFINE-AND-PREPARE-QUERY DEFINE-QUERY)
    (CONSTRAINT :CD-CONSTRAINT-EXPRESSION CONSTRAINT-ENTAILED? ADD-CONSTRAINT-ASSERTION CONSTRAINT-ENTAILED-P FORGET-CONSTRAINT)
    (HTTP::CONTENT-TYPE :CONTENT-TYPE PUBLISH-FILE)
    (THEMATIC-SUBSTRATE::CONTINUATION-BASED-INSTANCE-RETRIEVAL-P :BOOLEAN
                                                                 EXECUTE-OR-REEXECUTE-QUERY
                                                                 EXECUTE-QUERY
                                                                 EXECUTE-ALL-QUERIES
                                                                 EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                                 RACER-ANSWER-QUERY
                                                                 RACER-ANSWER-QUERY-UNDER-PREMISE
                                                                 RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                                 RACER-ANSWER-QUERY1
                                                                 RACER-ANSWER-TBOX-QUERY
                                                                 RACER-ANSWER-TBOX-QUERY1
                                                                 RACER-PREPARE-QUERY
                                                                 RACER-PREPARE-QUERY1
                                                                 RACER-PREPARE-TBOX-QUERY
                                                                 RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::COPY-P :BOOLEAN REPREPARE-QUERY)
    (COPY-RULES :BOOLEAN CREATE-ABOX-CLONE)
    (COUNT :BOOLEAN
           ALL-DIFFERENT-FROM-ASSERTIONS
           ALL-SAME-AS-ASSERTIONS
           ALL-ANNOTATION-CONCEPT-ASSERTIONS
           ALL-ANNOTATION-ROLE-ASSERTIONS
           ALL-ATOMIC-CONCEPTS
           ALL-ATTRIBUTE-ASSERTIONS
           ALL-ATTRIBUTES
           ALL-CONCEPT-ASSERTIONS
           ALL-CONCEPT-ASSERTIONS-FOR-INDIVIDUAL
           ALL-CONSTRAINTS
           ALL-EQUIVALENT-CONCEPTS
           ALL-FEATURES
           ALL-INDIVIDUALS
           ALL-ROLE-ASSERTIONS
           ALL-ROLE-ASSERTIONS-FOR-INDIVIDUAL-IN-DOMAIN
           ALL-ROLE-ASSERTIONS-FOR-INDIVIDUAL-IN-RANGE
           ALL-ROLES
           ALL-TRANSITIVE-ROLES)
    (THEMATIC-SUBSTRATE::CREATE-ABOX-IF-NOT-FOUND-P
     :BOOLEAN
     MAKE-FORWARD-RULE-FROM-ABOXES
     PREPARE-NRQL-ENGINE
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1)
    (THEMATIC-SUBSTRATE::CREATE-TBOX-IF-NOT-FOUND-P :BOOLEAN RACER-ANSWER-TBOX-QUERY RACER-ANSWER-TBOX-QUERY1 RACER-PREPARE-TBOX-QUERY RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::CUTOFF-FN :IGNORE COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2 ENABLE-ABDUCTION RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (OWLAPI:DATA-PROPERTIES (:LIST :datatype-property) |OWLAPI-getOWLDisjointDataPropertiesAxiom| |OWLAPI-getOWLEquivalentDataPropertiesAxiom|)
    (OWLAPI:DATA-PROPERTY :datatype-property |OWLAPI-getOWLDataPropertyDomainAxiom| |OWLAPI-getOWLDataPropertyRangeAxiom| |OWLAPI-getOWLFunctionalDataPropertyAxiom| |OWLAPI-getRelatedValues|)
    (OWLAPI:DATA-PROPERTY-DOMAIN :CONCEPT-EXPRESSION |OWLAPI-getOWLDataPropertyDomainAxiom|)
    (OWLAPI:DATA-RANGE :OWL-DATATYPE-EXPRESSION |OWLAPI-getOWLDataPropertyRangeAxiom| |OWLAPI-getOWLDatatypeDefinitionAxiom|)
    (THEMATIC-SUBSTRATE::DATA-RELATION :SUBSTRATE-RELATION DATA-EDGE1)
    (OWLAPI:DATA-SUB-PROPERTY :datatype-property |OWLAPI-getOWLDataSubPropertyAxiom|)
    (OWLAPI:DATA-SUPER-PROPERTY :datatype-property |OWLAPI-getOWLDataSubPropertyAxiom|)
    (DATA-VERSION-LEVEL :IGNORE CREATE-TRIPLE-STORE MATERIALIZE-INFERENCES TRIPLE-STORE-READ-FILE)
    (DATATYPE :ROLE-DATATYPE ADD-ROLE-AXIOM ADD-ROLE-AXIOMS FORGET-ROLE-AXIOMS)
    (OWLAPI:DATATYPE-NAME :DATATYPE-NAME |OWLAPI-getOWLDatatypeDefinitionAxiom|)
    (DATATYPE-ROLE :datatype-property INDIVIDUAL-TOLD-DATATYPE-FILLERS RETRIEVE-INDIVIDUAL-TOLD-DATATYPE-FILLERS)
    (DB :TRIPLESTORE-NAME CLOSE-TRIPLE-STORE INDEX-ALL-TRIPLES MATERIALIZE-INFERENCES TRIPLE-STORE-GRAPHS TRIPLE-STORE-READ-FILE USE-TRIPLE-STORE)
    (DB-NAME :TRIPLESTORE-NAME TRIPLE-STORE-OPEN-P)
    (DEBUG :BOOLEAN LOGGING-ON)
    (THEMATIC-SUBSTRATE::DEBUG-P :BOOLEAN EXECUTE-OR-REEXECUTE-ALL-QUERIES EXECUTE-OR-REEXECUTE-ALL-RULES)
    (DEFAULT :IGNORE ALL-ROLES)
    (DEFINITION :CONCEPT-EXPRESSION DEFINE-CONCEPT DEFINE-DISJOINT-PRIMITIVE-CONCEPT DEFINE-PRIMITIVE-CONCEPT)
    (THEMATIC-SUBSTRATE::DEFINITIONS-P :BOOLEAN SHOW-QBOX-FOR-ABOX)
    (DELETE-RULES :BOOLEAN SWRL-FORWARD-CHAINING)
    (THEMATIC-SUBSTRATE::DELIVER-KB-HAS-CHANGED-WARNING-TOKENS-P :BOOLEAN
                                                                 EXECUTE-OR-REEXECUTE-QUERY
                                                                 EXECUTE-QUERY
                                                                 EXECUTE-ALL-QUERIES
                                                                 EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                                 RACER-ANSWER-QUERY
                                                                 RACER-ANSWER-QUERY-UNDER-PREMISE
                                                                 RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                                 RACER-ANSWER-QUERY1
                                                                 RACER-ANSWER-TBOX-QUERY
                                                                 RACER-ANSWER-TBOX-QUERY1
                                                                 RACER-PREPARE-QUERY
                                                                 RACER-PREPARE-QUERY1
                                                                 RACER-PREPARE-TBOX-QUERY
                                                                 RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::DELIVER-PHASE-TWO-WARNING-TOKENS-P :BOOLEAN
                                                            EXECUTE-OR-REEXECUTE-QUERY
                                                            EXECUTE-QUERY
                                                            EXECUTE-ALL-QUERIES
                                                            EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                            RACER-ANSWER-QUERY
                                                            RACER-ANSWER-QUERY-UNDER-PREMISE
                                                            RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                            RACER-ANSWER-QUERY1
                                                            RACER-ANSWER-TBOX-QUERY
                                                            RACER-ANSWER-TBOX-QUERY1
                                                            RACER-PREPARE-QUERY
                                                            RACER-PREPARE-QUERY1
                                                            RACER-PREPARE-TBOX-QUERY
                                                            RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::DEPTH :NON-NEGATIVE-INTEGER GET-ABOX-GRAPH)
    (THEMATIC-SUBSTRATE::DESCR :SUBSTRATE-LABEL-DESCRIPTION CREATE-DATA-EDGE CREATE-DATA-NODE DATA-NODE1)
    (OWLAPI:DESCRIPTION :CONCEPT-EXPRESSION |OWLAPI-getOWLClassAssertionAxiom| |OWLAPI-getOWLDisjointUnionAxiom| |OWLAPI-isSatisfiable|)
    (OWLAPI:DESCRIPTIONS (:LIST :CONCEPT-EXPRESSION) |OWLAPI-getOWLDisjointClassesAxiom| |OWLAPI-getOWLDisjointUnionAxiom| |OWLAPI-getOWLEquivalentClassesAxiom|)
    (OWLAPI::DIRECT :BOOLEAN |OWLAPI-getIndividuals| |OWLAPI-getInstances| |OWLAPI-getTypes| |OWLAPI-hasType|)
    (DIRECT-P :BOOLEAN RETRIEVE-INDIVIDUAL-TOLD-DATATYPE-FILLERS)
    (DIRECTORY :DIRECTORY LOAD-RACER-PATCHES LOAD-RACER-PLUGINS CREATE-TRIPLE-STORE MATERIALIZE-INFERENCES OPEN-TRIPLE-STORE TRIPLE-STORE-GRAPHS USE-TRIPLE-STORE)
    (DISJOINT-LIST (:LIST :CONCEPT-NAME) DEFINE-DISJOINT-PRIMITIVE-CONCEPT)
    (OWLAPI:DISPOSE-AXIOMS-P :BOOLEAN |OWLAPI-disposeOntology|)
    (DOMAIN :CONCEPT-EXPRESSION DEFINE-CONCRETE-DOMAIN-ATTRIBUTE DEFINE-PRIMITIVE-ATTRIBUTE DEFINE-PRIMITIVE-ROLE ADD-ROLE-AXIOM ADD-ROLE-AXIOMS FORGET-ROLE-AXIOMS)
    (THEMATIC-SUBSTRATE::DONT-ADD-ABOX-DUPLICATES :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::DONT-ADD-ABOX-DUPLICATES-P :BOOLEAN
                                                    ADD-CHOSEN-SETS-OF-RULE-CONSEQUENCES
                                                    EXECUTE-OR-REEXECUTE-QUERY
                                                    EXECUTE-QUERY
                                                    EXECUTE-ALL-QUERIES
                                                    EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                    RACER-ANSWER-QUERY
                                                    RACER-ANSWER-QUERY-UNDER-PREMISE
                                                    RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                    RACER-ANSWER-QUERY1
                                                    RACER-ANSWER-TBOX-QUERY
                                                    RACER-ANSWER-TBOX-QUERY1
                                                    RACER-PREPARE-QUERY
                                                    RACER-PREPARE-QUERY1
                                                    RACER-PREPARE-TBOX-QUERY
                                                    RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::DONT-CHECK-ID-P
     :BOOLEAN
     MAKE-FORWARD-RULE-FROM-ABOXES
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::DONT-SHOW-HEAD-PROJECTION-OPERATORS :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::DONT-SHOW-HEAD-PROJECTION-OPERATORS-P :BOOLEAN
                                                               EXECUTE-OR-REEXECUTE-QUERY
                                                               EXECUTE-QUERY
                                                               EXECUTE-ALL-QUERIES
                                                               EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                               RACER-ANSWER-QUERY
                                                               RACER-ANSWER-QUERY-UNDER-PREMISE
                                                               RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                               RACER-ANSWER-QUERY1
                                                               RACER-ANSWER-TBOX-QUERY
                                                               RACER-ANSWER-TBOX-QUERY1
                                                               RACER-PREPARE-QUERY
                                                               RACER-PREPARE-QUERY1
                                                               RACER-PREPARE-TBOX-QUERY
                                                               RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::DONT-SHOW-LAMBDAS :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::DONT-SHOW-LAMBDAS-P :BOOLEAN
                                             EXECUTE-OR-REEXECUTE-QUERY
                                             EXECUTE-QUERY
                                             EXECUTE-ALL-QUERIES
                                             EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                             RACER-ANSWER-QUERY
                                             RACER-ANSWER-QUERY-UNDER-PREMISE
                                             RACER-ANSWER-QUERY-UNDER-PREMISE1
                                             RACER-ANSWER-QUERY1
                                             RACER-ANSWER-TBOX-QUERY
                                             RACER-ANSWER-TBOX-QUERY1
                                             RACER-PREPARE-QUERY
                                             RACER-PREPARE-QUERY1
                                             RACER-PREPARE-TBOX-QUERY
                                             RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::DONT-SHOW-VARIABLES :BOOLEAN
                                             WITH-NRQL-SETTINGS-EVALUATED
                                             WITH-NRQL-SETTINGS
                                             EXECUTE-OR-REEXECUTE-QUERY
                                             EXECUTE-QUERY
                                             GET-ANSWER
                                             GET-NUMBER-OF-EXPLANATIONS
                                             EXECUTE-ALL-QUERIES
                                             EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                             GET-ALL-ANSWERS
                                             GET-ANSWER-SIZE
                                             PREPARE-NRQL-ENGINE
                                             RACER-ANSWER-QUERY
                                             RACER-ANSWER-QUERY-UNDER-PREMISE
                                             RACER-ANSWER-QUERY-UNDER-PREMISE1
                                             RACER-ANSWER-QUERY1
                                             RACER-ANSWER-TBOX-QUERY
                                             RACER-ANSWER-TBOX-QUERY1
                                             RACER-PREPARE-QUERY
                                             RACER-PREPARE-QUERY1
                                             RACER-PREPARE-TBOX-QUERY
                                             RACER-PREPARE-TBOX-QUERY1)
    (EDGE-LABEL :SUBSTRATE-DESCRIPTION-LABEL SET-EDGE-LABEL-FOR-NON-EXISTENT-EDGES)
    (THEMATIC-SUBSTRATE::ENSURE-PERMUTATIONS-P :BOOLEAN ADD-EXPLANATION-ASSERTIONS GET-EXPLANATIONS COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2 ENABLE-ABDUCTION RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (THEMATIC-SUBSTRATE::ENSURE-TBOX-CLASSIFICATION-P :BOOLEAN
                                                      EXECUTE-OR-REEXECUTE-QUERY
                                                      EXECUTE-QUERY
                                                      EXECUTE-ALL-QUERIES
                                                      EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                      RACER-ANSWER-QUERY
                                                      RACER-ANSWER-QUERY-UNDER-PREMISE
                                                      RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                      RACER-ANSWER-QUERY1
                                                      RACER-ANSWER-TBOX-QUERY
                                                      RACER-ANSWER-TBOX-QUERY1
                                                      RACER-PREPARE-QUERY
                                                      RACER-PREPARE-QUERY1
                                                      RACER-PREPARE-TBOX-QUERY
                                                      RACER-PREPARE-TBOX-QUERY1)
    (OWLAPI:ENTITY :ENTITY |OWLAPI-getOWLDeclarationAxiom| |OWLAPI-getOWLEntityAnnotationAxiom| |OWLAPI-getOWLImplicitDeclarationAxiom| |OWLAPI-getOWLReallyImplicitDeclarationAxiom|)
    (THEMATIC-SUBSTRATE::EQUI-ORDER-BY (:ONE-OF :PREFER-OLD-INDS :PREFER-NEW-INDS) ADD-EXPLANATION-ASSERTIONS GET-EXPLANATIONS COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (THEMATIC-SUBSTRATE::ERROR-P :BOOLEAN DESCRIBE-ALL-DEFINITIONS DESCRIBE-DEFINITION)
    (ERRORP :BOOLEAN DOMAIN RANGE FIND-ABOX FIND-TBOX ROLE-HAS-DOMAIN ROLE-HAS-RANGE)
    (EXCLUDE-PERMUTATIONS :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::EXCLUDE-PERMUTATIONS-P :BOOLEAN
                                                EXECUTE-OR-REEXECUTE-QUERY
                                                EXECUTE-QUERY
                                                EXECUTE-ALL-QUERIES
                                                EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                RACER-ANSWER-QUERY
                                                RACER-ANSWER-QUERY-UNDER-PREMISE
                                                RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                RACER-ANSWER-QUERY1
                                                RACER-ANSWER-TBOX-QUERY
                                                RACER-ANSWER-TBOX-QUERY1
                                                RACER-PREPARE-QUERY
                                                RACER-PREPARE-QUERY1
                                                RACER-PREPARE-TBOX-QUERY
                                                RACER-PREPARE-TBOX-QUERY1)
    (EXCLUDED-META-ONTOLOGIES (:LIST :URI) |OWLAPI-readOntology| OWL-READ-DOCUMENT OWL-READ-FILE)
    (THEMATIC-SUBSTRATE::EXECUTE-P
     :BOOLEAN
     GET-ALL-REMAINING-TUPLES
     GET-ANSWER
     GET-NEXT-N-REMAINING-TUPLES
     GET-NEXT-TUPLE
     GET-NUMBER-OF-EXPLANATIONS
     GET-ALL-ANSWERS
     GET-ANSWER-SIZE
     MAKE-FORWARD-RULE-FROM-ABOXES
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::EXPL-NO :NON-NEGATIVE-INTEGER ADD-EXPLANATION-ASSERTIONS)
    (THEMATIC-SUBSTRATE::EXPLAIN-ALL :BOOLEAN CHECK-ONTOLOGY)
    (THEMATIC-SUBSTRATE::EXTENSION :FILE-EXTENSION MAKE-PLUGIN-FROM-FASL-FILE)
    (EXTENSION :FILE-EXTENSION TRANSMIT-FILE)
    (FEATURE :BOOLEAN DEFINE-PRIMITIVE-ROLE ADD-ROLE-AXIOM ADD-ROLE-AXIOMS FORGET-ROLE-AXIOMS)
    (FEATURE-P :BOOLEAN ADD-ROLE-AXIOM ADD-ROLE-AXIOMS FORGET-ROLE-AXIOMS)
    (FEATURES (:LIST :ROLE) SIGNATURE ENSURE-TBOX-SIGNATURE)
    (THEMATIC-SUBSTRATE::FILENAME :FILENAME CHECK-ONTOLOGY RESTORE-ALL-SUBSTRATES RESTORE-SERVER-IMAGE RESTORE-SUBSTRATE STORE-ALL-SUBSTRATES STORE-SERVER-IMAGE STORE-SUBSTRATE-FOR-ABOX)
    (HTTP::FILENAME :FILENAME PUBLISH-FILE)
    (ROLE :ROLE FORGET-ROLE-AXIOMS)
    (FILENAME
     :FILENAME
     DIG-READ-FILE
     LOGGING-ON
     OWL-READ-FILE
     OWLLINK-READ-FILE
     RACER-READ-FILE
     RDFS-READ-TBOX-FILE
     RESTORE-ABOX-IMAGE
     RESTORE-ABOXES-IMAGE
     RESTORE-KB-IMAGE
     RESTORE-KBS-IMAGE
     RESTORE-TBOX-IMAGE
     RESTORE-TBOXES-IMAGE
     STORE-ABOX-IMAGE
     STORE-ABOXES-IMAGE
     STORE-KB-IMAGE
     STORE-KBS-IMAGE
     STORE-TBOX-IMAGE
     STORE-TBOXES-IMAGE
     TRIPLE-STORE-READ-FILE
     XML-READ-TBOX-FILE)
    (FILENAME-OR-STREAM :FILENAME CHECK-ABOX-COHERENCE)
    (FILLER-NAME :ABOX-INDIVIDUAL ADD-ANNOTATION-ROLE-ASSERTION ADD-NEGATED-ROLE-ASSERTION ADD-ROLE-ASSERTION FORGET-NEGATED-ROLE-ASSERTION FORGET-ROLE-ASSERTION)
    (THEMATIC-SUBSTRATE::FINAL-CONSISTENCY-CHECKING-P
     :BOOLEAN
     EXECUTE-OR-REEXECUTE-QUERY
     EXECUTE-QUERY
     COMPUTE-ABOX-DIFFERENCE1
     COMPUTE-ABOX-DIFFERENCE2
     ENABLE-ABDUCTION
     EXECUTE-ALL-QUERIES
     EXECUTE-OR-REEXECUTE-ALL-QUERIES
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1
     RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (FIRE-ONCE-P :BOOLEAN ADD-RULE-AXIOM)
    (FIRE-RULES :BOOLEAN |OWLAPI-readOntology| OWL-READ-DOCUMENT OWL-READ-FILE)
    (OWLAPI:FIRST-OBJECT-PROPERTY :OBJECT-PROPERTY |OWLAPI-getOWLInverseObjectPropertiesAxiom|)
    (THEMATIC-SUBSTRATE::FN :FILENAME LOAD-RACER-PATCH LOAD-RACER-PLUGIN)
    (OWLAPI::FN :FILENAME |OWLAPI-exportOntology| |OWLAPI-exportReasoner| |OWLAPI-restoreImage| |OWLAPI-storeImage|)
    (OWL-SYNTAXES::FN :FILENAME |OWLAPI-readFunctionalOntologyFile| |OWLAPI-readXMLOntologyFile| |OWLAPI-saveOntology| |OWLAPI-writeFunctionalOntologyFile| |OWLAPI-writeOntologyFile| |OWLAPI-writeXMLOntologyFile|)
    (THEMATIC-SUBSTRATE::FN2 :FILENAME MAKE-PLUGIN-FROM-FASL-FILE)
    (THEMATIC-SUBSTRATE::FOR-ABOX :ABOX-NAME MAKE-ABDUCTION-RULE-FROM-ABOXES MAKE-BACKWARD-RULE-FROM-ABOXES MAKE-FORWARD-RULE-FROM-ABOXES STORE-SUBSTRATE-FOR-ABOX)
    (THEMATIC-SUBSTRATE::FOR-ANNOTATION-PROPERTIES (:LIST :ANNOTATION-PROPERTY) GET-ABOX-GRAPH)
    (THEMATIC-SUBSTRATE::FOR-BUILD :BUILD-STRING MAKE-PLUGIN-FROM-FASL-FILE)
    (THEMATIC-SUBSTRATE::FOR-DATATYPE-PROPERTIES (:LIST :DATATYPE-PROPERTY) GET-ABOX-GRAPH)
    (THEMATIC-SUBSTRATE::FOR-ROLES (:LIST :ROLE) GET-ABOX-GRAPH GET-CONCEPT-PROPERTIES GET-ROLE-HIERARCHY)
    (THEMATIC-SUBSTRATE::FOR-VERSION :VERSION-STRING MAKE-PLUGIN-FROM-FASL-FILE)
    (OWLAPI::FORCE-P :BOOLEAN |OWLAPI-isConsistent|)
    (SYSTEM::FORM :RACER-EXPRESSION TIME)
    (FORM (:LIST :CONCEPT-EXPRESSION) DEFINE-DISJOINT-PRIMITIVE-CONCEPT DISJOINT ADD-DISJOINTNESS-AXIOM FORGET-DISJOINTNESS-AXIOM)
    (FORMS (:LIST :CD-CONSTRAINT-EXPRESSION) CONSTRAINTS)
    (FORMS (:LIST :ABOX-ASSERTION) STATE)
    (THEMATIC-SUBSTRATE::FORWARD-RULE-CONSEQUENCE-P :BOOLEAN MAKE-ABDUCTION-RULE-FROM-ABOXES MAKE-BACKWARD-RULE-FROM-ABOXES MAKE-FORWARD-RULE-FROM-ABOXES MAKE-QUERY-FROM-ABOX)
    (THEMATIC-SUBSTRATE::FORWARD-RULE-P :BOOLEAN MAKE-ABDUCTION-RULE-FROM-ABOXES)
    (FORWARD-RULE-P :BOOLEAN DEFINE-RULE ADD-RULE-AXIOM)
    (THEMATIC-SUBSTRATE::FROM
     :SUBSTRATE-NODE
     ADD-EXPLANATION-ASSERTIONS
     GET-EXPLANATIONS
     CREATE-DATA-EDGE
     DATA-EDGE1
     DEL-DATA-EDGE1
     DELETE-DATA-EDGE
     EDGE-DESCRIPTION1
     EDGE-LABEL1
     GET-DATA-EDGE-DESCRIPTION
     GET-DATA-EDGE-LABEL)
    (OWLAPI:FROM :NON-NEGATIVE-INTEGER |OWLAPI-setProgressRange|)
    (THEMATIC-SUBSTRATE::FROM-ABOX :ABOX-NAME COPY-RULES MOVE-RULES)
    (THEMATIC-SUBSTRATE::FULL-RESET-P :BOOLEAN RESET-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::FULL-TUPLES-P :BOOLEAN ADD-EXPLANATION-ASSERTIONS GET-EXPLANATIONS COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (THEMATIC-SUBSTRATE::GENERATE-CODE-P
     :IGNORE
     MAKE-FORWARD-RULE-FROM-ABOXES
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1)
    (GRAPH :IGNORE TRIPLE-STORE-READ-FILE USE-TRIPLE-STORE)
    (THEMATIC-SUBSTRATE::GROUP-BY-OPS :IGNORE
                                      MAKE-FORWARD-RULE-FROM-ABOXES
                                      RACER-ANSWER-QUERY
                                      RACER-ANSWER-QUERY-UNDER-PREMISE
                                      RACER-ANSWER-QUERY-UNDER-PREMISE1
                                      RACER-ANSWER-QUERY1
                                      RACER-ANSWER-TBOX-QUERY
                                      RACER-ANSWER-TBOX-QUERY1
                                      RACER-APPLY-RULE
                                      RACER-APPLY-RULE-UNDER-PREMISE
                                      RACER-APPLY-RULE-UNDER-PREMISE1
                                      RACER-APPLY-RULE1
                                      RACER-PREPARE-QUERY
                                      RACER-PREPARE-QUERY1
                                      RACER-PREPARE-RULE
                                      RACER-PREPARE-RULE1
                                      RACER-PREPARE-TBOX-QUERY
                                      RACER-PREPARE-TBOX-QUERY1)
    (GROUP-NAME :DISJOINTNESS-GROUP-ID ADD-DISJOINTNESS-AXIOM FORGET-DISJOINTNESS-AXIOM)
    (THEMATIC-SUBSTRATE::HEAD :NRQL-QUERY-HEAD DEFINE-AND-EXECUTE-QUERY DEFINE-AND-PREPARE-QUERY DEFINE-QUERY)
    (HEAD :EVENT-RULE-HEAD DEFINE-EVENT-RULE PRETRIEVE ADD-EVENT-RULE)
    (OWLAPI::HEADER :BOOLEAN |OWLAPI-exportOntology|)
    (HEADER :BOOLEAN SAVE-ABOX SAVE-KB SAVE-TBOX)
    (THEMATIC-SUBSTRATE::HOW-MANY :NON-NEGATIVE-INTEGER
                                  EXECUTE-OR-REEXECUTE-QUERY
                                  EXECUTE-QUERY
                                  COMPUTE-ABOX-DIFFERENCE1
                                  COMPUTE-ABOX-DIFFERENCE2
                                  ENABLE-ABDUCTION
                                  EXECUTE-ALL-QUERIES
                                  EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                  RACER-ANSWER-QUERY
                                  RACER-ANSWER-QUERY-UNDER-PREMISE
                                  RACER-ANSWER-QUERY-UNDER-PREMISE1
                                  RACER-ANSWER-QUERY1
                                  RACER-ANSWER-TBOX-QUERY
                                  RACER-ANSWER-TBOX-QUERY1
                                  RACER-PREPARE-QUERY
                                  RACER-PREPARE-QUERY1
                                  RACER-PREPARE-TBOX-QUERY
                                  RACER-PREPARE-TBOX-QUERY1
                                  RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (THEMATIC-SUBSTRATE::HOW-MANY-TUPLES :NON-NEGATIVE-INTEGER WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::HYPO-MODE-STACK :IGNORE COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2 ENABLE-ABDUCTION RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (THEMATIC-SUBSTRATE::HYPOTHESIZED-ASSERTIONS-P :BOOLEAN ADD-EXPLANATION-ASSERTIONS GET-EXPLANATIONS)
    (OWLAPI::I :ABOX-INDIVIDUAL |OWLAPI-isDifferentIndividual| |OWLAPI-isSameIndividual|)
    (OWLAPI::ID :OWLAPI-AXIOM-ID |OWLAPI-AxiomLoaded?| |OWLAPI-IDToAxiom| |OWLAPI-nextAxiomUseID|)
    (OWLAPI::ID :OWLAPI-OTHER-ID |OWLAPI-findObjectFromID|)
    (THEMATIC-SUBSTRATE::ID :PLUGIN-ID MAKE-PLUGIN-FROM-FASL-FILE)
    (THEMATIC-SUBSTRATE::ID :NRQL-QUERY-ID
                            MAKE-FORWARD-RULE-FROM-ABOXES
                            RACER-ANSWER-QUERY
                            RACER-ANSWER-QUERY-UNDER-PREMISE
                            RACER-ANSWER-QUERY-UNDER-PREMISE1
                            RACER-ANSWER-QUERY1
                            RACER-ANSWER-TBOX-QUERY
                            RACER-ANSWER-TBOX-QUERY1
                            RACER-APPLY-RULE
                            RACER-APPLY-RULE-UNDER-PREMISE
                            RACER-APPLY-RULE-UNDER-PREMISE1
                            RACER-APPLY-RULE1
                            RACER-PREPARE-QUERY
                            RACER-PREPARE-QUERY1
                            RACER-PREPARE-RULE
                            RACER-PREPARE-RULE1
                            RACER-PREPARE-TBOX-QUERY
                            RACER-PREPARE-TBOX-QUERY1)
    (ID :NRQL-QUERY-ID ADD-RULE-AXIOM PRACER-ANSWER-QUERY)
    (OWLAPI::ID-OR-CONSTRUCTOR (:or :OWLAPI-AXIOM-ID :OWLAPI-AXIOM-CONSTRUCTOR-CALL) |OWLAPI-disposeAxiom|)
    (OWLAPI::IDS-OR-CONSTRUCTORS (:LIST (:or :OWLAPI-AXIOM-ID :OWLAPI-AXIOM-CONSTRUCTOR-CALL)) |OWLAPI-disposeAxioms|)
    (IF-CLOSED :BOOLEAN CLOSE-TRIPLE-STORE)
    (IF-DOES-NOT-EXIST (:ONE-OF :CREATE :ERROR) MATERIALIZE-INFERENCES SAVE-ABOX SAVE-KB SAVE-TBOX)
    (IF-EXISTS (:ONE-OF :SUPERSEDE :ERROR) CREATE-TRIPLE-STORE MATERIALIZE-INFERENCES SAVE-ABOX SAVE-KB SAVE-TBOX TRIPLE-STORE-READ-FILE)
    (OWL-SYNTAXES::IGNORE-ANNOTATIONS :BOOLEAN |OWLAPI-readFunctionalOntologyDocument| |OWLAPI-readFunctionalOntologyFile| |OWLAPI-readOntology| |OWLAPI-readXMLOntologyDocument| |OWLAPI-readXMLOntologyFile|)
    (IGNORE-ANNOTATIONS :BOOLEAN |OWLAPI-readOntology| OWL-READ-DOCUMENT OWL-READ-FILE)
    (IGNORE-ERROR :BOOLEAN VERIFY-WITH-CONCEPT-TREE-LIST)
    (OWL-SYNTAXES::IGNORE-IMPORT :BOOLEAN |OWLAPI-readFunctionalOntologyDocument| |OWLAPI-readFunctionalOntologyFile| |OWLAPI-readOntology| |OWLAPI-readXMLOntologyDocument| |OWLAPI-readXMLOntologyFile|)
    (IGNORE-IMPORT :BOOLEAN |OWLAPI-readOntology| OWL-READ-DOCUMENT OWL-READ-FILE USE-TRIPLE-STORE)
    (IMPORT-LIST (:LIST (:list :prefix-string :url)) SAVE-ABOX)
    (IMPORT-META-ONTOLOGIES :BOOLEAN |OWLAPI-readOntology| OWL-READ-DOCUMENT OWL-READ-FILE)
    (IN-CASE-INDIVIDUALS-ARE-RENAMED-KEEP-ORIGINALS :BOOLEAN MATERIALIZE-INFERENCES)
    (IN-FILE :FILENAME CONVERT-EVENT-SPECS)
    (INCLUDE-DIRECT-TYPES :BOOLEAN MSC-K)
    (INCLUSION-P :BOOLEAN ADD-CONCEPT-AXIOM FORGET-CONCEPT-AXIOM)
    (THEMATIC-SUBSTRATE::IND :SUBSTRATE-NODE GET-INDIVIDUAL-SUCCESSORS)
    (OWLAPI::IND
     :ABOX-INDIVIDUAL
     |OWLAPI-getDataPropertyRelationships|
     |OWLAPI-getDataPropertyValues|
     |OWLAPI-getDifferentIndividuals|
     |OWLAPI-getObjectPropertyRelationships|
     |OWLAPI-getObjectPropertyValues|
     |OWLAPI-getSameIndividuals|
     |OWLAPI-hasType|
     |OWLAPI-isDefinedIndividual|)
    (IND
     :ABOX-INDIVIDUAL
     INDIVIDUAL-ATTRIBUTE-FILLERS
     INDIVIDUAL-TOLD-ATTRIBUTE-VALUE
     INDIVIDUAL-TOLD-DATATYPE-FILLERS
     RETRIEVE-INDIVIDUAL-ATTRIBUTE-FILLERS
     RETRIEVE-INDIVIDUAL-TOLD-ATTRIBUTE-VALUE
     RETRIEVE-INDIVIDUAL-TOLD-DATATYPE-FILLERS)
    (IND-FILLER :ABOX-INDIVIDUAL DIRECT-PREDECESSORS INDIVIDUAL-FILLED-ROLES RETRIEVE-DIRECT-PREDECESSORS RETRIEVE-INDIVIDUAL-FILLED-ROLES)
    (IND-FILLER-NAME-SET :ABOX-INDIVIDUAL INDIVIDUALS-RELATED-P INTERNAL-INDIVIDUALS-RELATED-P)
    (IND-MAPPING :BOOLEAN PRINT-ABOX-INDIVIDUALS)
    (IND-PREDECESSOR :ABOX-INDIVIDUAL INDIVIDUAL-FILLED-ROLES INDIVIDUAL-FILLERS RETRIEVE-INDIVIDUAL-FILLED-ROLES RETRIEVE-INDIVIDUAL-FILLERS)
    (IND-PREDECESSOR-NAME-SET :ABOX-INDIVIDUAL INDIVIDUALS-RELATED-P INTERNAL-INDIVIDUALS-RELATED-P)
    (INDEX-P :BOOLEAN MATERIALIZE-INFERENCES TRIPLE-STORE-READ-FILE)
    (OWLAPI:INDIVIDUAL :ABOX-INDIVIDUAL |OWLAPI-getOWLClassAssertionAxiom| |OWLAPI-getTypes|)
    (INDIVIDUAL
     :ABOX-INDIVIDUAL
     ATTRIBUTE-FILLER
     CONSTRAINED
     DATATYPE-ROLE-FILLER
     INDIVIDUAL-ANTONYMS
     INDIVIDUAL-INSTANCE?
     INDIVIDUAL-SYNONYMS
     PUBLISH
     UNPUBLISH
     ADD-ATTRIBUTE-ASSERTION
     ADD-DATATYPE-ROLE-FILLER
     ADD-NEGATIVE-DATATYPE-ROLE-FILLER
     ALL-ATTRIBUTE-ASSERTIONS
     ALL-CONCEPT-ASSERTIONS-FOR-INDIVIDUAL
     ALL-ROLE-ASSERTIONS-FOR-INDIVIDUAL-IN-DOMAIN
     ALL-ROLE-ASSERTIONS-FOR-INDIVIDUAL-IN-RANGE
     FORGET-DATATYPE-ROLE-FILLER
     FORGET-INDIVIDUAL
     FORGET-NEGATIVE-DATATYPE-ROLE-FILLER
     MSC-K
     PUBLISH-1
     RETRIEVE-INDIVIDUAL-ANTONYMS
     RETRIEVE-INDIVIDUAL-SYNONYMS
     SET-ATTRIBUTE-FILLER
     UNPUBLISH-1)
    (INDIVIDUAL-1 :ABOX-INDIVIDUAL INDIVIDUALS-EQUAL? INDIVIDUALS-NOT-EQUAL? INDIVIDUALS-RELATED? FORGET-DIFFERENT-FROM-ASSERTION FORGET-SAME-INDIVIDUAL-AS-ASSERTION INDIVIDUALS-EQUAL-P INDIVIDUALS-NOT-EQUAL-P)
    (INDIVIDUAL-2 :ABOX-INDIVIDUAL INDIVIDUALS-EQUAL? INDIVIDUALS-NOT-EQUAL? INDIVIDUALS-RELATED? FORGET-DIFFERENT-FROM-ASSERTION FORGET-SAME-INDIVIDUAL-AS-ASSERTION INDIVIDUALS-EQUAL-P INDIVIDUALS-NOT-EQUAL-P)
    (THEMATIC-SUBSTRATE::INDIVIDUAL-NAME :ABOX-INDIVIDUAL GET-INDIVIDUAL-ANNOTATION-DATATYPE-FILLERS GET-INDIVIDUAL-ANNOTATION-FILLERS GET-INDIVIDUAL-DATATYPE-FILLERS)
    (INDIVIDUAL-NAME
     :ABOX-INDIVIDUAL
     DEFINE-DISTINCT-INDIVIDUAL
     DEFINE-INDIVIDUAL
     INDIVIDUAL-DIRECT-TYPES
     INDIVIDUAL-TYPES
     INDIVIDUAL?
     ADD-ANNOTATION-CONCEPT-ASSERTION
     ADD-CONCEPT-ASSERTION
     COMPUTE-IMPLICIT-ROLE-FILLERS
     DESCRIBE-INDIVIDUAL
     DESCRIBE-INDIVIDUAL1
     FORGET-ANNOTATION-CONCEPT-ASSERTION
     FORGET-CONCEPT-ASSERTION
     FORGET-CONSTRAINED-ASSERTION
     GET-INDIVIDUAL-PMODEL
     INDIVIDUAL-INSTANCE-P
     INDIVIDUAL-P
     INSTANTIATORS
     MOST-SPECIFIC-INSTANTIATORS
     REALIZE-ABOX
     RETRIEVE-INDIVIDUAL-ANNOTATION-PROPERTY-FILLERS)
    (INDIVIDUAL-NAME-1 :ABOX-INDIVIDUAL DIFFERENT-FROM SAME-AS SAME-INDIVIDUAL-AS ADD-DIFFERENT-FROM-ASSERTION ADD-SAME-INDIVIDUAL-AS-ASSERTION)
    (INDIVIDUAL-NAME-2 :ABOX-INDIVIDUAL DIFFERENT-FROM SAME-AS SAME-INDIVIDUAL-AS ADD-DIFFERENT-FROM-ASSERTION ADD-SAME-INDIVIDUAL-AS-ASSERTION)
    (INDIVIDUAL-NAME-SET (:LIST :ABOX-INDIVIDUAL) ALL-DIFFERENT ADD-ALL-DIFFERENT-ASSERTION FORGET-ALL-DIFFERENT-ASSERTION)
    (OWLAPI:INDIVIDUALS (:LIST :ABOX-INDIVIDUAL) |OWLAPI-getOWLDifferentIndividualsAxiom| |OWLAPI-getOWLSameIndividualsAxiom|)
    (INDIVIDUALS (:LIST :ABOX-INDIVIDUAL) SIGNATURE ENSURE-ABOX-SIGNATURE)
    (INDIVIDUALS-LIST (:LIST :ABOX-INDIVIDUAL) VERIFY-WITH-ABOX-INDIVIDUALS-LIST)
    (OWLAPI::INIT :BOOLEAN |OWLAPI-exportOntology| |OWLAPI-exportReasoner| |OWLAPI-newReasoner| |OWLAPI-newReasoner1|)
    (OWL-SYNTAXES::INIT :BOOLEAN |OWLAPI-readFunctionalOntologyDocument| |OWLAPI-readFunctionalOntologyFile| |OWLAPI-readOntology| |OWLAPI-readXMLOntologyDocument| |OWLAPI-readXMLOntologyFile|)
    (INIT :BOOLEAN IN-TBOX |OWLAPI-readOntology| DIG-READ-DOCUMENT DIG-READ-FILE OWL-READ-DOCUMENT OWL-READ-FILE TRIPLE-STORE-READ-FILE USE-TRIPLE-STORE)
    (THEMATIC-SUBSTRATE::INITIAL-ABOX-MIRRORING-P :BOOLEAN
                                                  EXECUTE-OR-REEXECUTE-QUERY
                                                  EXECUTE-QUERY
                                                  EXECUTE-ALL-QUERIES
                                                  EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                  RACER-ANSWER-QUERY
                                                  RACER-ANSWER-QUERY-UNDER-PREMISE
                                                  RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                  RACER-ANSWER-QUERY1
                                                  RACER-ANSWER-TBOX-QUERY
                                                  RACER-ANSWER-TBOX-QUERY1
                                                  RACER-PREPARE-QUERY
                                                  RACER-PREPARE-QUERY1
                                                  RACER-PREPARE-TBOX-QUERY
                                                  RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::INITIAL-ROLE-ASSERTION-MIRRORING-P :BOOLEAN
                                                            EXECUTE-OR-REEXECUTE-QUERY
                                                            EXECUTE-QUERY
                                                            EXECUTE-ALL-QUERIES
                                                            EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                            RACER-ANSWER-QUERY
                                                            RACER-ANSWER-QUERY-UNDER-PREMISE
                                                            RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                            RACER-ANSWER-QUERY1
                                                            RACER-ANSWER-TBOX-QUERY
                                                            RACER-ANSWER-TBOX-QUERY1
                                                            RACER-PREPARE-QUERY
                                                            RACER-PREPARE-QUERY1
                                                            RACER-PREPARE-TBOX-QUERY
                                                            RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::INJECTIVE-VARIABLES-P :BOOLEAN MAKE-ABDUCTION-RULE-FROM-ABOXES MAKE-BACKWARD-RULE-FROM-ABOXES MAKE-FORWARD-RULE-FROM-ABOXES MAKE-QUERY-FROM-ABOX)
    (INVERSE :ROLE DEFINE-PRIMITIVE-ATTRIBUTE DEFINE-PRIMITIVE-ROLE ADD-ROLE-AXIOM ADD-ROLE-AXIOMS FORGET-ROLE-AXIOMS)
    (INVERSE-FEATURE-P :BOOLEAN ADD-ROLE-AXIOM ADD-ROLE-AXIOMS)
    (INVERSE-ROLE :ROLE INVERSE INVERSE-OF-ROLE)
    (INVERSE-TEST :IGNORE ALL-ROLES)
    (THEMATIC-SUBSTRATE::IP :IP-ADDRESS SUBSCRIBE-TO UNSUBSCRIBE-FROM)
    (IP :IP-ADDRESS SUBSCRIBE SUBSCRIBE-1)
    (IRREFLEXIVE :BOOLEAN DEFINE-PRIMITIVE-ATTRIBUTE DEFINE-PRIMITIVE-ROLE ADD-ROLE-AXIOM ADD-ROLE-AXIOMS)
    (IRREFLEXIVE-P :BOOLEAN ADD-ROLE-AXIOM ADD-ROLE-AXIOMS)
    (OWLAPI::J :ABOX-INDIVIDUAL |OWLAPI-isDifferentIndividual| |OWLAPI-isSameIndividual|)
    (K :NON-NEGATIVE-INTEGER MSC-K)
    (KB :ABOX-NAME STORE-KB-IMAGE)
    (THEMATIC-SUBSTRATE::KB-HAS-CHANGED-WARNING-TOKENS :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (OWL-SYNTAXES::KB-NAME :TBOX-NAME |OWLAPI-readFunctionalOntologyDocument| |OWLAPI-readFunctionalOntologyFile| |OWLAPI-readOntology| |OWLAPI-readXMLOntologyDocument| |OWLAPI-readXMLOntologyFile|)
    (KB-NAME :TBOX-NAME |OWLAPI-readOntology| DIG-READ-DOCUMENT DIG-READ-FILE GET-KB-SIGNATURE KB-ONTOLOGIES MATERIALIZE-INFERENCES OWL-READ-DOCUMENT OWL-READ-FILE USE-TRIPLE-STORE)
    (KBS (:LIST :ABOX-NAME) STORE-KBS-IMAGE)
    (THEMATIC-SUBSTRATE::KEEP-OLD-NAMES-P :BOOLEAN COPY-RULES)
    (THEMATIC-SUBSTRATE::KEEP-P :BOOLEAN DEFINE-AND-EXECUTE-QUERY DEFINE-AND-PREPARE-QUERY DEFINE-QUERY)
    (THEMATIC-SUBSTRATE::KEY :IGNORE GET-MAXIMUM GET-MINIMUM)
    (OWLAPI:KEY-CLASS :CONCEPT-EXPRESSION |OWLAPI-getOWLHasKeyAxiom|)
    (OWLAPI:KEY-DATA-PROPERTIES (:LIST :datatype-property) |OWLAPI-getOWLHasKeyAxiom|)
    (OWLAPI:KEY-OBJECT-PROPERTIES (:LIST :OBJECT-PROPERTY) |OWLAPI-getOWLHasKeyAxiom|)
    (THEMATIC-SUBSTRATE::KNOWN-CORRESPONDANCES (:list (:list :abox-individual :abox-individual)) COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (THEMATIC-SUBSTRATE::KNOWN-CORRESPONDENCES  (:list (:list :abox-individual :abox-individual)) MAKE-ABDUCTION-RULE-FROM-ABOXES MAKE-BACKWARD-RULE-FROM-ABOXES MAKE-FORWARD-RULE-FROM-ABOXES MAKE-QUERY-FROM-ABOX)
    (LABEL :KEYWORD ADD-DOC-PHRASE1 DEL-DOC-ENTRY1)
    (LEFT :CONCEPT-EXPRESSION EQUIVALENT IMPLIES ADD-CONCEPT-AXIOM FORGET-CONCEPT-AXIOM)
    (LEFT-NAME :ABOX-INDIVIDUAL RELATED UNRELATED)
    (LEFTHAND-SIDE :RULE-LEFTHAND-SIDE DEFINE-RULE ADD-RULE-AXIOM)
    (LOCATOR :URL |OWLAPI-readOntology| OWL-READ-DOCUMENT OWL-READ-FILE)
    (LOGIC (:ONE-OF :K :KM :S4 :KB4) ALC-CONCEPT-COHERENT)
    (OWL-SYNTAXES::MAINTAIN-OWLAPI-AXIOMS :BOOLEAN |OWLAPI-readFunctionalOntologyDocument| |OWLAPI-readFunctionalOntologyFile| |OWLAPI-readOntology| |OWLAPI-readXMLOntologyDocument| |OWLAPI-readXMLOntologyFile|)
    (MAINTAIN-OWLAPI-AXIOMS :BOOLEAN |OWLAPI-readOntology| OWL-READ-DOCUMENT OWL-READ-FILE)
    (OWLAPI::MAKE-RACER-KB-CURRENT-P :BOOLEAN |OWLAPI-newReasoner| |OWLAPI-newReasoner1| |OWLAPI-setCurrentReasoner|)
    (THEMATIC-SUBSTRATE::MAP-NEW-INDS-TO-NEW-INDS-P :BOOLEAN COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (MAPPING :URL DEFINE-PREFIX ADD-PREFIX)
    (MARKER-NAME :IGNORE CREATE-TBOX-INTERNAL-MARKER-CONCEPT)
    (OWL-SYNTAXES::MERGE-IMPORTED-ONTOLOGIES-P :BOOLEAN |OWLAPI-readFunctionalOntologyDocument| |OWLAPI-readFunctionalOntologyFile| |OWLAPI-readOntology| |OWLAPI-readXMLOntologyDocument| |OWLAPI-readXMLOntologyFile|)
    (MERGE-IMPORTED-ONTOLOGIES-P :BOOLEAN |OWLAPI-readOntology| OWL-READ-DOCUMENT OWL-READ-FILE)
    (THEMATIC-SUBSTRATE::MODE :NON-NEGATIVE-INTEGER WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE SET-NRQL-MODE)
    (OWLAPI::N :NON-NEGATIVE-INTEGER |OWLAPI-setAxiomCounter| |OWLAPI-setProgress|)
    (THEMATIC-SUBSTRATE::N :NON-NEGATIVE-INTEGER
                           GET-NEXT-N-REMAINING-SETS-OF-RULE-CONSEQUENCES
                           GET-NEXT-N-REMAINING-TUPLES
                           CHECK-ONTOLOGY
                           OPTIMIZER-SET-NO-OF-PLANS-UPPER-BOUND
                           OPTIMIZER-SET-TIME-BOUND
                           SET-INITIAL-SIZE-OF-PROCESS-POOL
                           SET-MAX-NO-OF-TUPLES-BOUND
                           SET-MAXIMUM-SIZE-OF-PROCESS-POOL
                           SET-NEW-IND-COUNTER)
    (N-BYTES :NON-NEGATIVE-INTEGER TRANSMIT-FILE)
    (OWLAPI:NAME :OWLAPI-REASONER-NAME |OWLAPI-disposeReasoner| |OWLAPI-newOntology| |OWLAPI-setCurrentReasoner|)
    (THEMATIC-SUBSTRATE::NAME :NRQL-DEFINED-QUERY-NAME ACTIVATE-DEFINED-QUERY UNDEFINE-QUERY DEACTIVATE-DEFINED-QUERY DEFINE-AND-EXECUTE-QUERY DEFINE-AND-PREPARE-QUERY DEFINE-QUERY DESCRIBE-DEFINITION)
    (THEMATIC-SUBSTRATE::NAME :SUBSTRATE-NODE CREATE-DATA-NODE DATA-NODE1 DEL-DATA-NODE1 DELETE-DATA-NODE GET-DATA-NODE-DESCRIPTION GET-DATA-NODE-LABEL NODE-DESCRIPTION1 NODE-LABEL1)
    (THEMATIC-SUBSTRATE::NAME :MINILISP-OBJECT-NAME DEFCON1 DEFINE1 DEFPAR1 FCALL SERVER-FUNCTION SERVER-VALUE UNBIND1 UNDEFINE1)
    (THEMATIC-SUBSTRATE::NAME :ABOX-NAME SET-DATA-BOX SET-MIRROR-DATA-BOX SET-RCC-BOX)
    (THEMATIC-SUBSTRATE::NAME :RACER-PARAMETER SET-RACER-PARAMETER)
    (NAME :CONCEPT-NAME DEFINE-CONCEPT DEFINE-DISJOINT-PRIMITIVE-CONCEPT DEFINE-PRIMITIVE-CONCEPT)
    (NAME :CD-ATTRIBUTE DEFINE-CONCRETE-DOMAIN-ATTRIBUTE DEFINE-PRIMITIVE-ATTRIBUTE DEFINE-PRIMITIVE-ROLE)
    (NAME :TBOX-NAME DEFINE-TBOX IN-TBOX)
    (NAME :ABOX-INDIVIDUAL INSTANCE)
    (NAME :datatype-property ADD-DATATYPE-PROPERTY)
    (NAME :TRIPLESTORE-NAME CREATE-TRIPLE-STORE OPEN-TRIPLE-STORE)
    (NAME :CONCEPT-NAME MSC-K)
    (OWLAPI:NAMESPACE :URL |OWLAPI-addPrefix| |OWLAPI-getOWLPrefixDeclarationAxiom|)
    (OWLAPI::NAMESPACE-PREFIX :PREFIX-STRING |OWLAPI-getOWLPrefixDeclarationAxiom|)
    (NATIVE :BOOLEAN SPARQL-ANSWER-QUERY SPARQL-RETRIEVE)
    (THEMATIC-SUBSTRATE::NEGATED-P :BOOLEAN GET-INDIVIDUAL-SUCCESSORS)
    (NEGATED-P :BOOLEAN RETRIEVE-INDIVIDUAL-FILLED-ROLES)
    (THEMATIC-SUBSTRATE::NEW-ID :NRQL-QUERY-ID REPREPARE-QUERY)
    (THEMATIC-SUBSTRATE::NEW-IND-OPS :IGNORE
                                     MAKE-FORWARD-RULE-FROM-ABOXES
                                     RACER-ANSWER-QUERY
                                     RACER-ANSWER-QUERY-UNDER-PREMISE
                                     RACER-ANSWER-QUERY-UNDER-PREMISE1
                                     RACER-ANSWER-QUERY1
                                     RACER-ANSWER-TBOX-QUERY
                                     RACER-ANSWER-TBOX-QUERY1
                                     RACER-APPLY-RULE
                                     RACER-APPLY-RULE-UNDER-PREMISE
                                     RACER-APPLY-RULE-UNDER-PREMISE1
                                     RACER-APPLY-RULE1
                                     RACER-PREPARE-QUERY
                                     RACER-PREPARE-QUERY1
                                     RACER-PREPARE-RULE
                                     RACER-PREPARE-RULE1
                                     RACER-PREPARE-TBOX-QUERY
                                     RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::NEW-INDS-P :BOOLEAN ADD-EXPLANATION-ASSERTIONS GET-EXPLANATIONS)
    (THEMATIC-SUBSTRATE::NEW-NAME :ABOX-NAME CREATE-SUBGRAPH-ABOXES)
    (NEW-NAME :ABOX-NAME CLONE-ABOX CREATE-ABOX-CLONE)
    (NEW-NAME :TBOX-NAME CLONE-TBOX CREATE-TBOX-CLONE)
    (THEMATIC-SUBSTRATE::NO-INVERSES-P :BOOLEAN GET-INDIVIDUAL-SUCCESSORS)
    (NO-INVERSES-P :BOOLEAN RETRIEVE-INDIVIDUAL-FILLED-ROLES)
    (THEMATIC-SUBSTRATE::NO-TOP-ROLE-P :BOOLEAN GET-ABOX-GRAPH GET-INDIVIDUAL-SUCCESSORS)
    (THEMATIC-SUBSTRATE::NO-TRANSITIVES-P :BOOLEAN GET-ABOX-GRAPH GET-INDIVIDUAL-SUCCESSORS)
    (OWLAPI:OBJ :OWLAPI-EXPRESSION |OWLAPI-findIDFromObject| |OWLAPI-registerObject|)
    (OWLAPI:OBJECT :ABOX-INDIVIDUAL |OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom| |OWLAPI-getOWLObjectPropertyAssertionAxiom| |OWLAPI-hasObjectPropertyRelationship|)
    (OWLAPI:OBJECT :OWL-DATAVALUE |OWLAPI-hasDataPropertyRelationship|)
    (OBJECT :ABOX-INDIVIDUAL CONSTRAINED ADD-ATTRIBUTE-ASSERTION)
    (OBJECT :CD-OBJECT TOLD-VALUE)
    (OBJECT-NAME :CD-OBJECT CD-OBJECT? CD-OBJECT-P FORGET-CONSTRAINED-ASSERTION)
    (OBJECT-NAMES :IGNORE ALL-CONSTRAINTS)
    (OWLAPI:OBJECT-PROPERTIES (:LIST :OBJECT-PROPERTY) |OWLAPI-getOWLDisjointObjectPropertiesAxiom| |OWLAPI-getOWLEquivalentObjectPropertiesAxiom|)
    (OWLAPI:OBJECT-PROPERTY :OBJECT-PROPERTY
                            |OWLAPI-getOWLAsymmetricObjectPropertyAxiom|
                            |OWLAPI-getOWLFunctionalObjectPropertyAxiom|
                            |OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom|
                            |OWLAPI-getOWLIrreflexiveObjectPropertyAxiom|
                            |OWLAPI-getOWLObjectPropertyDomainAxiom|
                            |OWLAPI-getOWLObjectPropertyRangeAxiom|
                            |OWLAPI-getOWLReflexiveObjectPropertyAxiom|
                            |OWLAPI-getOWLSymmetricObjectPropertyAxiom|
                            |OWLAPI-getOWLTransitiveObjectPropertyAxiom|
                            |OWLAPI-getRelatedIndividuals|)
    (OWLAPI:OBJECT-PROPERTY-CHAIN (:LIST :OBJECT-PROPERTY) |OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom|)
    (OWLAPI:OBJECT-PROPERTY-DOMAIN :CONCEPT-EXPRESSION |OWLAPI-getOWLObjectPropertyDomainAxiom|)
    (OWLAPI:OBJECT-PROPERTY-RANGE :CONCEPT-EXPRESSION |OWLAPI-getOWLObjectPropertyRangeAxiom|)
    (OWLAPI:OBJECT-SUB-PROPERTY :OBJECT-PROPERTY |OWLAPI-getOWLObjectSubPropertyAxiom|)
    (OWLAPI:OBJECT-SUPER-PROPERTY :OBJECT-PROPERTY |OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom| |OWLAPI-getOWLObjectSubPropertyAxiom|)
    (OBJECTS (:LIST :CD-OBJECT) SIGNATURE ENSURE-ABOX-SIGNATURE)
    (THEMATIC-SUBSTRATE::ONLY-BEST-P :BOOLEAN ADD-EXPLANATION-ASSERTIONS GET-EXPLANATIONS COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2 ENABLE-ABDUCTION RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (THEMATIC-SUBSTRATE::ONLY-DIFFERENCE-P :BOOLEAN COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (THEMATIC-SUBSTRATE::ONLY-IF-P :IGNORE GET-INDIVIDUAL-SUCCESSORS)
    (THEMATIC-SUBSTRATE::ONLY-INVERSES-P :BOOLEAN GET-INDIVIDUAL-SUCCESSORS)
    (THEMATIC-SUBSTRATE::ONLY-NEW-TUPLES-P :BOOLEAN
                                           EXECUTE-OR-REEXECUTE-QUERY
                                           EXECUTE-QUERY
                                           EXECUTE-ALL-QUERIES
                                           EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                           RACER-ANSWER-QUERY
                                           RACER-ANSWER-QUERY-UNDER-PREMISE
                                           RACER-ANSWER-QUERY-UNDER-PREMISE1
                                           RACER-ANSWER-QUERY1
                                           RACER-ANSWER-TBOX-QUERY
                                           RACER-ANSWER-TBOX-QUERY1
                                           RACER-PREPARE-QUERY
                                           RACER-PREPARE-QUERY1
                                           RACER-PREPARE-TBOX-QUERY
                                           RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::ONLY-ONE-P :BOOLEAN GET-INDIVIDUAL-SUCCESSORS)
    (THEMATIC-SUBSTRATE::ONLY-SUCCESSORS-IN-SELECTED-INDIVIDUALS-P :BOOLEAN GET-ABOX-GRAPH)
    (OWLAPI::ONT
     :OWLAPI-ONTOLOGY-NAME
     |OWLAPI-AddAxiom|
     |OWLAPI-AddAxioms|
     |OWLAPI-AxiomToID|
     |OWLAPI-getAxiomsIn|
     |OWLAPI-getAxiomsOfTypeIn|
     |OWLAPI-loadAxiom|
     |OWLAPI-loadAxioms|
     |OWLAPI-RemoveAxiom|
     |OWLAPI-RemoveAxioms|
     |OWLAPI-SetOntologyURI|
     |OWLAPI-unloadAxiom|
     |OWLAPI-unloadAxioms|)
    (OWLAPI::ONT-NAME :OWLAPI-ONTOLOGY-NAME |OWLAPI-contains| |OWLAPI-disposeOntology|)
    (OWLAPI::ONT1 :OWLAPI-ONTOLOGY-NAME |OWLAPI-mergeOntologies|)
    (OWLAPI::ONT2 :OWLAPI-ONTOLOGY-NAME |OWLAPI-mergeOntologies|)
    (OWLAPI:ONTOLOGIES (:LIST :OWLAPI-ONTOLOGY-NAME) |OWLAPI-disposeOntologies| |OWLAPI-loadOntologies| |OWLAPI-unloadOntologies|)
    (OWLAPI:ONTOLOGY
     :OWLAPI-ONTOLOGY-NAME
     |OWLAPI-autoAddAxiomsTo|
     |OWLAPI-autoBatchAddAxiomsTo|
     |OWLAPI-autoBatchRemoveAxiomsFrom|
     |OWLAPI-autoRemoveAxiomsFrom|
     |OWLAPI-batchSynchronize|
     |OWLAPI-describeOntology|
     |OWLAPI-enableMemorySavingMode|
     |OWLAPI-exportOntology|
     |OWLAPI-isConsistent|
     |OWLAPI-loadOntology|
     |OWLAPI-saveOntology|
     |OWLAPI-unloadOntology|
     |OWLAPI-writeFunctionalOntologyFile|
     |OWLAPI-writeOntologyFile|
     |OWLAPI-writeXMLOntologyFile|)
    (OWLAPI::ONTOLOGY-IMPORT-URI :URI |OWLAPI-getOWLImportsDeclarationAxiom|)
    (OWL-SYNTAXES::ONTOLOGY-NAME :OWLAPI-ONTOLOGY-NAME |OWLAPI-readFunctionalOntologyDocument| |OWLAPI-readFunctionalOntologyFile| |OWLAPI-readOntology| |OWLAPI-readXMLOntologyDocument| |OWLAPI-readXMLOntologyFile|)
    (ONTOLOGY-NAME :OWLAPI-ONTOLOGY-NAME |OWLAPI-readOntology| OWL-READ-DOCUMENT OWL-READ-FILE SAVE-ABOX SAVE-KB)
    (OWLAPI::ONTOLOGY-VERSION-URI :URI |OWLAPI-getOWLOntologyVersionDeclarationAxiom|)
    (THEMATIC-SUBSTRATE::OPTIMIZE-P
     :BOOLEAN
     MAKE-FORWARD-RULE-FROM-ABOXES
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::OPTIMIZER-MAX-PLANS :NON-NEGATIVE-INTEGER COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (OPTIMIZER-USE-CARDINALITY-HEURISTICS :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::ORDER-BY
     (:ONE-OF :NEW-PAPER-FN :CAE :CAE2 :RASOULI-PAPER-FN)
     ADD-EXPLANATION-ASSERTIONS
     GET-EXPLANATIONS
     COMPUTE-ABOX-DIFFERENCE1
     COMPUTE-ABOX-DIFFERENCE2
     ENABLE-ABDUCTION
     RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (ORIGINAL :IGNORE INIT-TBOX)
    (THEMATIC-SUBSTRATE::ORIGINAL-QUERY :IGNORE
                                        MAKE-FORWARD-RULE-FROM-ABOXES
                                        RACER-ANSWER-QUERY
                                        RACER-ANSWER-QUERY-UNDER-PREMISE
                                        RACER-ANSWER-QUERY-UNDER-PREMISE1
                                        RACER-ANSWER-QUERY1
                                        RACER-ANSWER-TBOX-QUERY
                                        RACER-ANSWER-TBOX-QUERY1
                                        RACER-APPLY-RULE
                                        RACER-APPLY-RULE-UNDER-PREMISE
                                        RACER-APPLY-RULE-UNDER-PREMISE1
                                        RACER-APPLY-RULE1
                                        RACER-PREPARE-QUERY
                                        RACER-PREPARE-QUERY1
                                        RACER-PREPARE-RULE
                                        RACER-PREPARE-RULE1
                                        RACER-PREPARE-TBOX-QUERY
                                        RACER-PREPARE-TBOX-QUERY1)
    (OUT-FILE :FILENAME CONVERT-EVENT-SPECS)
    (OVERWRITE :BOOLEAN CLONE-ABOX CLONE-TBOX CREATE-ABOX-CLONE CREATE-TBOX-CLONE)
    (OWLAPI:OWLAPI-ABOX :ABOX-NAME |OWLAPI-newReasoner| |OWLAPI-newReasoner1|)
    (OWLAPI::OWLAPI-HACKING-MODE (:ONE-OF 0 1 2) |OWLAPI-getDomains| |OWLAPI-getRanges|)
    (OWLAPI:OWLAPI-REASONER-NAME :OWLAPI-REASONER-NAME |OWLAPI-newReasoner| |OWLAPI-newReasoner1|)
    (OWLAPI:OWLAPI-TBOX :TBOX-NAME |OWLAPI-newReasoner| |OWLAPI-newReasoner1|)
    (OWLAPI::OWN-RACER-P :BOOLEAN |OWLAPI-newReasoner|)
    (OWL-SYNTAXES::P4-MODE :BOOLEAN |OWLAPI-saveOntology| |OWLAPI-writeFunctionalOntologyFile| |OWLAPI-writeOntologyFile| |OWLAPI-writeXMLOntologyFile|)
    (PACKAGE
     :IGNORE
     MAKE-FORWARD-RULE-FROM-ABOXES
     PREPARE-NRQL-ENGINE
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1)
    (PARENT (:LIST :ROLE) DEFINE-PRIMITIVE-ATTRIBUTE DEFINE-PRIMITIVE-ROLE ADD-ROLE-AXIOM ADD-ROLE-AXIOMS FORGET-ROLE-AXIOMS)
    (PARENTS (:LIST :ROLE) DEFINE-PRIMITIVE-ATTRIBUTE DEFINE-PRIMITIVE-ROLE ADD-ROLE-AXIOM ADD-ROLE-AXIOMS FORGET-ROLE-AXIOMS)
    (OWL-SYNTAXES::PARSER :IGNORE |OWLAPI-readFunctionalOntologyDocument| |OWLAPI-readFunctionalOntologyFile| |OWLAPI-readOntology| |OWLAPI-readXMLOntologyDocument| |OWLAPI-readXMLOntologyFile|)
    (THEMATIC-SUBSTRATE::PARSER :IGNORE
                                MAKE-FORWARD-RULE-FROM-ABOXES
                                RACER-ANSWER-QUERY
                                RACER-ANSWER-QUERY-UNDER-PREMISE
                                RACER-ANSWER-QUERY-UNDER-PREMISE1
                                RACER-ANSWER-QUERY1
                                RACER-ANSWER-TBOX-QUERY
                                RACER-ANSWER-TBOX-QUERY1
                                RACER-APPLY-RULE
                                RACER-APPLY-RULE-UNDER-PREMISE
                                RACER-APPLY-RULE-UNDER-PREMISE1
                                RACER-APPLY-RULE1
                                RACER-PREPARE-QUERY
                                RACER-PREPARE-QUERY1
                                RACER-PREPARE-RULE
                                RACER-PREPARE-RULE1
                                RACER-PREPARE-TBOX-QUERY
                                RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::PARSER-CLASS
     :IGNORE
     MAKE-FORWARD-RULE-FROM-ABOXES
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1)
    (PARTITION :IGNORE USE-TRIPLE-STORE)
    (THEMATIC-SUBSTRATE::PATCH-NAME :STRING MAKE-PLUGIN-FROM-FASL-FILE)
    (THEMATIC-SUBSTRATE::PATCHDIR :DIRECTORY UPDATE-RACER)
    (PATHNAME :FILENAME ADD-DOC-IMAGE-DATA-FROM-FILE1 ADD-DOC-IMAGE-FILE1 INCLUDE-KB)
    (TYPE (:one-of :gif-image :jpeg-image) ADD-DOC-IMAGE-DATA1 ADD-DOC-IMAGE-FILE1)
    (PATHNAME-OR-STREAM :FILENAME SAVE-ABOX SAVE-KB SAVE-TBOX)
    (THEMATIC-SUBSTRATE::PHASE-TWO-STARTS-WARNING-TOKENS :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::PLATFORM :IGNORE MAKE-PLUGIN-FROM-FASL-FILE)
    (THEMATIC-SUBSTRATE::PLUGIN-NAME :STRING MAKE-PLUGIN-FROM-FASL-FILE)
    (THEMATIC-SUBSTRATE::PLUGINDIR :DIRECTORY UPDATE-RACER)
    (THEMATIC-SUBSTRATE::PORT :IP-PORT SUBSCRIBE-TO UNSUBSCRIBE-FROM)
    (PORT :IP-PORT SUBSCRIBE SUBSCRIBE-1)
    (THEMATIC-SUBSTRATE::POS :NON-NEGATIVE-INTEGER ACTIVATE-DEFINED-QUERY DEACTIVATE-DEFINED-QUERY)
    (THEMATIC-SUBSTRATE::POSTCOND-ABOX :abox MAKE-ABDUCTION-RULE-FROM-ABOXES MAKE-BACKWARD-RULE-FROM-ABOXES MAKE-FORWARD-RULE-FROM-ABOXES)
    (THEMATIC-SUBSTRATE::PRECOND-ABOX :abox MAKE-ABDUCTION-RULE-FROM-ABOXES MAKE-BACKWARD-RULE-FROM-ABOXES MAKE-FORWARD-RULE-FROM-ABOXES)
    (PREDECESSOR-NAME :ABOX-INDIVIDUAL ADD-ANNOTATION-ROLE-ASSERTION ADD-NEGATED-ROLE-ASSERTION ADD-ROLE-ASSERTION FORGET-NEGATED-ROLE-ASSERTION FORGET-ROLE-ASSERTION)
    (OWLAPI:PREFIX :PREFIX-STRING |OWLAPI-addPrefix| |OWLAPI-removePrefix|)
    (THEMATIC-SUBSTRATE::PREFIX :PREFIX-STRING SET-NEW-IND-PREFIX)
    (PREFIX :PREFIX-STRING DEFINE-PREFIX ADD-PREFIX)
    (OWL-SYNTAXES::PREFIXES (:LIST (:list :prefix-string :url)) |OWLAPI-saveOntology| |OWLAPI-writeFunctionalOntologyFile| |OWLAPI-writeOntologyFile| |OWLAPI-writeXMLOntologyFile|)
    (THEMATIC-SUBSTRATE::PREMISE (:LIST :ABOX-ASSERTION)
                                 MAKE-FORWARD-RULE-FROM-ABOXES
                                 RACER-ANSWER-QUERY
                                 RACER-ANSWER-QUERY-UNDER-PREMISE
                                 RACER-ANSWER-QUERY-UNDER-PREMISE1
                                 RACER-ANSWER-QUERY1
                                 RACER-ANSWER-TBOX-QUERY
                                 RACER-ANSWER-TBOX-QUERY1
                                 RACER-APPLY-RULE
                                 RACER-APPLY-RULE-UNDER-PREMISE
                                 RACER-APPLY-RULE-UNDER-PREMISE1
                                 RACER-APPLY-RULE1
                                 RACER-PREPARE-QUERY
                                 RACER-PREPARE-QUERY1
                                 RACER-PREPARE-RULE
                                 RACER-PREPARE-RULE1
                                 RACER-PREPARE-TBOX-QUERY
                                 RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::PREPARE-NOW-P
     :BOOLEAN
     MAKE-FORWARD-RULE-FROM-ABOXES
     PREPARE-NRQL-ENGINE
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1)
    (PRINT-QUERY :IGNORE PRACER-ANSWER-QUERY)
    (THEMATIC-SUBSTRATE::PROACTIVE-TUPLE-COMPUTATION-P :BOOLEAN
                                                       EXECUTE-OR-REEXECUTE-QUERY
                                                       EXECUTE-QUERY
                                                       EXECUTE-ALL-QUERIES
                                                       EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                       RACER-ANSWER-QUERY
                                                       RACER-ANSWER-QUERY-UNDER-PREMISE
                                                       RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                       RACER-ANSWER-QUERY1
                                                       RACER-ANSWER-TBOX-QUERY
                                                       RACER-ANSWER-TBOX-QUERY1
                                                       RACER-PREPARE-QUERY
                                                       RACER-PREPARE-QUERY1
                                                       RACER-PREPARE-TBOX-QUERY
                                                       RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::PROCESSED-P :BOOLEAN GET-ALL-ANSWERS)
    (OWLAPI:PROPERTY
     :OBJECT-PROPERTY
     |OWLAPI-getAncestorProperties|
     |OWLAPI-getDescendantProperties|
     |OWLAPI-getDisjointObjectProperties|
     |OWLAPI-getDomains|
     |OWLAPI-getEquivalentProperties|
     |OWLAPI-getInverseProperties|
     |OWLAPI-getObjectPropertyValues|
     |OWLAPI-getRanges|
     |OWLAPI-getSubProperties|
     |OWLAPI-getSuperProperties|
     |OWLAPI-hasObjectPropertyRelationship|
     |OWLAPI-isAsymmetric|
     |OWLAPI-isDefinedObjectProperty|
     |OWLAPI-isFunctional|
     |OWLAPI-isInverseFunctional|
     |OWLAPI-isIrreflexive|
     |OWLAPI-isReflexive|
     |OWLAPI-isSymmetric|
     |OWLAPI-isTransitive|)
    (OWLAPI:PROPERTY :datatype-property |OWLAPI-getDataPropertyValues| |OWLAPI-getDisjointDataProperties| |OWLAPI-hasDataPropertyRelationship| |OWLAPI-isDefinedDataProperty|)
    (THEMATIC-SUBSTRATE::PROXY :IP-ADDRESS SET-PROXY-SERVER)
    (THEMATIC-SUBSTRATE::PUT-INTO-REPOSITORY-P :BOOLEAN
                                               MAKE-FORWARD-RULE-FROM-ABOXES
                                               RACER-ANSWER-QUERY
                                               RACER-ANSWER-QUERY-UNDER-PREMISE
                                               RACER-ANSWER-QUERY-UNDER-PREMISE1
                                               RACER-ANSWER-QUERY1
                                               RACER-ANSWER-TBOX-QUERY
                                               RACER-ANSWER-TBOX-QUERY1
                                               RACER-APPLY-RULE
                                               RACER-APPLY-RULE-UNDER-PREMISE
                                               RACER-APPLY-RULE-UNDER-PREMISE1
                                               RACER-APPLY-RULE1
                                               RACER-PREPARE-QUERY
                                               RACER-PREPARE-QUERY1
                                               RACER-PREPARE-RULE
                                               RACER-PREPARE-RULE1
                                               RACER-PREPARE-TBOX-QUERY
                                               RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::QUALIFICATIONS :IGNORE GET-CONCEPT-PROPERTIES)
    (THEMATIC-SUBSTRATE::QUERIES-P :BOOLEAN GET-ALL-ANSWERS)
    (THEMATIC-SUBSTRATE::QUERY :NRQL-QUERY-ID
                               ABORT-QUERY
                               ABORT-RULE
                               ADD-CHOSEN-SETS-OF-RULE-CONSEQUENCES
                               ADD-EXPLANATION-ASSERTIONS
                               CHEAP-QUERY-P
                               CHEAP-RULE-P
                               CHOOSE-CURRENT-SET-OF-RULE-CONSEQUENCES
                               CLASSIFY-QUERY
                               DELETE-QUERY
                               DELETE-RULE
                               DESCRIBE-QUERY
                               DESCRIBE-QUERY-STATUS
                               DESCRIBE-RULE
                               DESCRIBE-RULE-STATUS
                               EXECUTE-OR-REEXECUTE-QUERY
                               EXECUTE-OR-REEXECUTE-RULE
                               EXECUTE-QUERY
                               EXECUTE-RULE
                               EXPENSIVE-QUERY-P
                               EXPENSIVE-RULE-P
                               GET-ALL-REMAINING-SETS-OF-RULE-CONSEQUENCES
                               GET-ALL-REMAINING-TUPLES
                               GET-ANSWER
                               GET-CHOSEN-SETS-OF-RULE-CONSEQUENCES
                               GET-CURRENT-SET-OF-RULE-CONSEQUENCES
                               GET-CURRENT-TUPLE
                               GET-EXPLANATIONS
                               GET-NEXT-N-REMAINING-SETS-OF-RULE-CONSEQUENCES
                               GET-NEXT-N-REMAINING-TUPLES
                               GET-NEXT-SET-OF-RULE-CONSEQUENCES
                               GET-NEXT-TUPLE
                               GET-NUMBER-OF-EXPLANATIONS
                               NEXT-SET-OF-RULE-CONSEQUENCES-AVAILABLE-P
                               NEXT-TUPLE-AVAILABLE-P
                               ORIGINAL-QUERY-BODY
                               ORIGINAL-QUERY-HEAD
                               ORIGINAL-RULE-ANTECEDENCE
                               ORIGINAL-RULE-CONSEQUENCE
                               QUERY-ACCURATE-P
                               QUERY-ACTIVE-P
                               QUERY-ANCESTORS
                               QUERY-BODY
                               QUERY-CHILDREN
                               QUERY-CONSISTENT-P
                               QUERY-DESCENDANTS
                               QUERY-EQUIVALENTS
                               QUERY-HEAD
                               QUERY-PARENTS
                               QUERY-PROCESSED-P
                               QUERY-READY-P
                               QUERY-RUNNING-P
                               QUERY-SUBSCRIBERS
                               QUERY-WAITING-P
                               REEXECUTE-QUERY
                               REEXECUTE-RULE
                               REPREPARE-QUERY
                               REPREPARE-RULE
                               RULE-ACCURATE-P
                               RULE-ACTIVE-P
                               RULE-ANTECEDENCE
                               RULE-APPLICABLE-P
                               RULE-CONSEQUENCE
                               RULE-CONSISTENT-P
                               RULE-PROCESSED-P
                               RULE-READY-P
                               RULE-RUNNING-P
                               RULE-UNAPPLICABLE-P
                               RULE-WAITING-P
                               SUBSCRIBE-TO
                               UNSUBSCRIBE-FROM
                               GET-ANSWER-SIZE
                               RACER-ANSWER-QUERY
                               RACER-ANSWER-QUERY-UNDER-PREMISE
                               RACER-ANSWER-QUERY-UNDER-PREMISE1
                               RACER-ANSWER-QUERY1
                               RACER-ANSWER-TBOX-QUERY
                               RACER-ANSWER-TBOX-QUERY1
                               RACER-APPLY-RULE
                               RACER-APPLY-RULE-UNDER-PREMISE
                               RACER-APPLY-RULE-UNDER-PREMISE1
                               RACER-APPLY-RULE1
                               RACER-PREPARE-QUERY
                               RACER-PREPARE-QUERY1
                               RACER-PREPARE-RULE
                               RACER-PREPARE-RULE1
                               RACER-PREPARE-TBOX-QUERY
                               RACER-PREPARE-TBOX-QUERY1
                               RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (QUERY :TIMENET-QUERY TIMENET-RETRIEVE TIMENET-ANSWER-QUERY)
    (QUERY :NRQL-QUERY-BODY PRACER-ANSWER-QUERY)
    (QUERY-CONCEPT :CONCEPT-NAME SUBSCRIBE UNSUBSCRIBE SUBSCRIBE-1 UNSUBSCRIBE-1)
    (THEMATIC-SUBSTRATE::QUERY-OPTIMIZATION :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::QUERY-REALIZATION :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::QUERY-REPOSITORY :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (OWLAPI::QUOTED :IGNORE |OWLAPI-exportOntology| |OWLAPI-exportReasoner|)
    (THEMATIC-SUBSTRATE::R-MODE :IGNORE COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2 ENABLE-ABDUCTION RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (RACER :BOOLEAN SPARQL-ANSWER-QUERY SPARQL-RETRIEVE)
    (THEMATIC-SUBSTRATE::RACER-DESCR :RACER-SUBSTRATE-DESCRIPTION-LABEL CREATE-DATA-EDGE CREATE-DATA-NODE DATA-EDGE1 DATA-NODE1)
    (THEMATIC-SUBSTRATE::RAND :IGNORE MAKE-PLUGIN-FROM-FASL-FILE)
    (RANGE :CONCEPT-EXPRESSION DEFINE-PRIMITIVE-ATTRIBUTE DEFINE-PRIMITIVE-ROLE ADD-ROLE-AXIOM ADD-ROLE-AXIOMS DATATYPE-ROLE-HAS-RANGE FORGET-ROLE-AXIOMS)
    (THEMATIC-SUBSTRATE::RCC-RELATION :SYMBOL REGISTER-RCC-SYNONYM)
    (THEMATIC-SUBSTRATE::RCC-TYPE (:ONE-OF :RCC1 :RCC2 :RCC3 :RCC5 :RCC8) SET-RCC-BOX)
    (RDFS-REASONING :BOOLEAN OPEN-TRIPLE-STORE)
    (THEMATIC-SUBSTRATE::READY-P :BOOLEAN GET-ALL-ANSWERS)
    (OWLAPI:REASONER
     :OWLAPI-REASONER-NAME
     |OWLAPI-abort|
     |OWLAPI-AddAxiom|
     |OWLAPI-AddAxioms|
     |OWLAPI-addPrefix|
     |OWLAPI-advanceProgress|
     |OWLAPI-applyChanges|
     |OWLAPI-autoAddAxiomsTo|
     |OWLAPI-autoApplyChanges|
     |OWLAPI-autoBatchAddAxiomsTo|
     |OWLAPI-autoBatchRemoveAxiomsFrom|
     |OWLAPI-autoRemoveAxiomsFrom|
     |OWLAPI-AxiomLoaded?|
     |OWLAPI-AxiomToID|
     |OWLAPI-batchSynchronize|
     |OWLAPI-classify|
     |OWLAPI-clearChanges|
     |OWLAPI-clearOntologies|
     |OWLAPI-clearRegistry|
     |OWLAPI-considerDeclarations|
     |OWLAPI-contains|
     |OWLAPI-describeOntologies|
     |OWLAPI-describeOntology|
     |OWLAPI-describeReasoner|
     |OWLAPI-disableAutoMode|
     |OWLAPI-disableIncrementalUpdates|
     |OWLAPI-disableLookupMode|
     |OWLAPI-disableMemorySavingMode|
     |OWLAPI-disableSimplifiedProtocol|
     |OWLAPI-disableTransientAxiomMode|
     |OWLAPI-disposeAxiom|
     |OWLAPI-disposeAxioms|
     |OWLAPI-disposeOntologies|
     |OWLAPI-disposeOntology|
     |OWLAPI-dontRegisterDeclaredEntities|
     |OWLAPI-dontRegisterReferencedEntities|
     |OWLAPI-enableIncrementalUpdates|
     |OWLAPI-enableLookupMode|
     |OWLAPI-enableMemorySavingMode|
     |OWLAPI-enableSimplifiedProtocol|
     |OWLAPI-enableTransientAxiomMode|
     |OWLAPI-exportOntology|
     |OWLAPI-exportReasoner|
     |OWLAPI-getAncestorClasses|
     |OWLAPI-getAncestorProperties|
     |OWLAPI-getAnnotationAxiomsForAxiom|
     |OWLAPI-getAutoDeclareDataProperties|
     |OWLAPI-getAutoOntology|
     |OWLAPI-getAxiomCounter|
     |OWLAPI-getAxioms|
     |OWLAPI-getAxiomsIn|
     |OWLAPI-getAxiomsOfType|
     |OWLAPI-getAxiomsOfTypeIn|
     |OWLAPI-getAxiomsPerOntology|
     |OWLAPI-getChanges|
     |OWLAPI-getDataPropertyRelationships|
     |OWLAPI-getDataPropertyValues|
     |OWLAPI-getDescendantClasses|
     |OWLAPI-getDescendantProperties|
     |OWLAPI-getDifferentIndividuals|
     |OWLAPI-getDisjointClasses|
     |OWLAPI-getDisjointDataProperties|
     |OWLAPI-getDisjointObjectProperties|
     |OWLAPI-getDomains|
     |OWLAPI-getEquivalentClasses|
     |OWLAPI-getEquivalentProperties|
     |OWLAPI-getInconsistentClasses|
     |OWLAPI-getIndividuals|
     |OWLAPI-getInstances|
     |OWLAPI-getInverseProperties|
     |OWLAPI-getLoadedOntologies|
     |OWLAPI-getObjectPropertyRelationships|
     |OWLAPI-getObjectPropertyValues|
     |OWLAPI-getOntologies|
     |OWLAPI-getOWLAnnotationAssertionAxiom|
     |OWLAPI-getOWLAnnotationPropertyDomainAxiom|
     |OWLAPI-getOWLAnnotationPropertyRangeAxiom|
     |OWLAPI-getOWLAsymmetricObjectPropertyAxiom|
     |OWLAPI-getOWLAxiomAnnotationAxiom|
     |OWLAPI-getOWLClassAssertionAxiom|
     |OWLAPI-getOWLDataPropertyAssertionAxiom|
     |OWLAPI-getOWLDataPropertyDomainAxiom|
     |OWLAPI-getOWLDataPropertyRangeAxiom|
     |OWLAPI-getOWLDataSubPropertyAxiom|
     |OWLAPI-getOWLDatatypeDefinitionAxiom|
     |OWLAPI-getOWLDeclarationAxiom|
     |OWLAPI-getOWLDifferentIndividualsAxiom|
     |OWLAPI-getOWLDisjointClassesAxiom|
     |OWLAPI-getOWLDisjointDataPropertiesAxiom|
     |OWLAPI-getOWLDisjointObjectPropertiesAxiom|
     |OWLAPI-getOWLDisjointUnionAxiom|
     |OWLAPI-getOWLEntityAnnotationAxiom|
     |OWLAPI-getOWLEquivalentClassesAxiom|
     |OWLAPI-getOWLEquivalentDataPropertiesAxiom|
     |OWLAPI-getOWLEquivalentObjectPropertiesAxiom|
     |OWLAPI-getOWLFunctionalDataPropertyAxiom|
     |OWLAPI-getOWLFunctionalObjectPropertyAxiom|
     |OWLAPI-getOWLHasKeyAxiom|
     |OWLAPI-getOWLImplicitDeclarationAxiom|
     |OWLAPI-getOWLImportsDeclarationAxiom|
     |OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom|
     |OWLAPI-getOWLInverseObjectPropertiesAxiom|
     |OWLAPI-getOWLIrreflexiveObjectPropertyAxiom|
     |OWLAPI-getOWLNegativeDataPropertyAssertionAxiom|
     |OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom|
     |OWLAPI-getOWLObjectPropertyAssertionAxiom|
     |OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom|
     |OWLAPI-getOWLObjectPropertyDomainAxiom|
     |OWLAPI-getOWLObjectPropertyRangeAxiom|
     |OWLAPI-getOWLObjectSubPropertyAxiom|
     |OWLAPI-getOWLOntologyAnnotationAxiom|
     |OWLAPI-getOWLOntologyVersionDeclarationAxiom|
     |OWLAPI-getOWLPrefixDeclarationAxiom|
     |OWLAPI-getOWLReallyImplicitDeclarationAxiom|
     |OWLAPI-getOWLReflexiveObjectPropertyAxiom|
     |OWLAPI-getOWLSameIndividualsAxiom|
     |OWLAPI-getOWLSubAnnotationPropertyOfAxiom|
     |OWLAPI-getOWLSubClassAxiom|
     |OWLAPI-getOWLSymmetricObjectPropertyAxiom|
     |OWLAPI-getOWLTransitiveObjectPropertyAxiom|
     |OWLAPI-getPrefixes|
     |OWLAPI-getRanges|
     |OWLAPI-getRelatedIndividuals|
     |OWLAPI-getRelatedValues|
     |OWLAPI-getSameIndividuals|
     |OWLAPI-getSubClasses|
     |OWLAPI-getSubProperties|
     |OWLAPI-getSuperClasses|
     |OWLAPI-getSuperProperties|
     |OWLAPI-getTypes|
     |OWLAPI-hasDataPropertyRelationship|
     |OWLAPI-hasObjectPropertyRelationship|
     |OWLAPI-hasType|
     |OWLAPI-IDToAxiom|
     |OWLAPI-ignoreAnnotations|
     |OWLAPI-ignoreDeclarations|
     |OWLAPI-isAsymmetric|
     |OWLAPI-isClass|
     |OWLAPI-isClassified|
     |OWLAPI-isConsistent|
     |OWLAPI-isDefinedClass|
     |OWLAPI-isDefinedDataProperty|
     |OWLAPI-isDefinedIndividual|
     |OWLAPI-isDefinedObjectProperty|
     |OWLAPI-isDifferentIndividual|
     |OWLAPI-isEntailed|
     |OWLAPI-isEquivalentClass|
     |OWLAPI-isFunctional|
     |OWLAPI-isInverseFunctional|
     |OWLAPI-isIrreflexive|
     |OWLAPI-isRealised|
     |OWLAPI-isReflexive|
     |OWLAPI-isSameIndividual|
     |OWLAPI-isSatisfiable|
     |OWLAPI-isSubClassOf|
     |OWLAPI-isSymmetric|
     |OWLAPI-isTransitive|
     |OWLAPI-keepAnnotations|
     |OWLAPI-loadAxiom|
     |OWLAPI-loadAxioms|
     |OWLAPI-loadOntologies|
     |OWLAPI-loadOntology|
     |OWLAPI-manuallyApplyChanges|
     |OWLAPI-mergeOntologies|
     |OWLAPI-newOntology|
     |OWLAPI-nextAxiomUseID|
     |OWLAPI-parse|
     |OWLAPI-parseNative|
     |OWLAPI-realize|
     |OWLAPI-registerDeclaredEntities|
     |OWLAPI-registerLastAnswer|
     |OWLAPI-registerReferencedEntities|
     |OWLAPI-reloadLoadedOntologies|
     |OWLAPI-RemoveAxiom|
     |OWLAPI-RemoveAxioms|
     |OWLAPI-removePrefix|
     |OWLAPI-resetAxiomCounter|
     |OWLAPI-saveOntology|
     |OWLAPI-setAutoDeclareDataProperties|
     |OWLAPI-setAxiomCounter|
     |OWLAPI-SetOntologyURI|
     |OWLAPI-setProgress|
     |OWLAPI-setProgressRange|
     |OWLAPI-setReturnPolicy|
     |OWLAPI-sleep|
     |OWLAPI-unloadAxiom|
     |OWLAPI-unloadAxioms|
     |OWLAPI-unloadOntologies|
     |OWLAPI-unloadOntology|
     |OWLAPI-usesIncrementalUpdates|
     |OWLAPI-usesSimplifiedProtocol|)
    (OWL-SYNTAXES::REASONER-NAME :OWLAPI-REASONER-NAME |OWLAPI-readFunctionalOntologyDocument| |OWLAPI-readFunctionalOntologyFile| |OWLAPI-readOntology| |OWLAPI-readXMLOntologyDocument| |OWLAPI-readXMLOntologyFile|)
    (OWLAPI::REASONERS (:LIST :OWLAPI-REASONER-NAME) |OWLAPI-storeImage|)
    (THEMATIC-SUBSTRATE::RECORD-EXPLANATIONS-P :BOOLEAN
                                               EXECUTE-OR-REEXECUTE-QUERY
                                               EXECUTE-QUERY
                                               EXECUTE-ALL-QUERIES
                                               EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                               RACER-ANSWER-QUERY
                                               RACER-ANSWER-QUERY-UNDER-PREMISE
                                               RACER-ANSWER-QUERY-UNDER-PREMISE1
                                               RACER-ANSWER-QUERY1
                                               RACER-ANSWER-TBOX-QUERY
                                               RACER-ANSWER-TBOX-QUERY1
                                               RACER-PREPARE-QUERY
                                               RACER-PREPARE-QUERY1
                                               RACER-PREPARE-TBOX-QUERY
                                               RACER-PREPARE-TBOX-QUERY1)
    (RECURSIVE :IGNORE |OWLAPI-readOntology| OWL-READ-DOCUMENT OWL-READ-FILE)
    (REFLEXIVE :BOOLEAN DEFINE-PRIMITIVE-ATTRIBUTE DEFINE-PRIMITIVE-ROLE ADD-ROLE-AXIOM ADD-ROLE-AXIOMS FORGET-ROLE-AXIOMS)
    (REFLEXIVE-P :BOOLEAN ADD-ROLE-AXIOM ADD-ROLE-AXIOMS FORGET-ROLE-AXIOMS)
    (OWLAPI:REL-DATA-PROPERTY :datatype-property |OWLAPI-getOWLDataPropertyAssertionAxiom| |OWLAPI-getOWLNegativeDataPropertyAssertionAxiom|)
    (OWLAPI:REL-OBJECT-PROPERTY :OBJECT-PROPERTY |OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom| |OWLAPI-getOWLObjectPropertyAssertionAxiom|)
    (THEMATIC-SUBSTRATE::REMOVE-COMMON-ASSERTIONS-P :BOOLEAN COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (THEMATIC-SUBSTRATE::REMOVE-DUPLICATES-P :BOOLEAN
                                             EXECUTE-OR-REEXECUTE-QUERY
                                             EXECUTE-QUERY
                                             EXECUTE-ALL-QUERIES
                                             EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                             RACER-ANSWER-QUERY
                                             RACER-ANSWER-QUERY-UNDER-PREMISE
                                             RACER-ANSWER-QUERY-UNDER-PREMISE1
                                             RACER-ANSWER-QUERY1
                                             RACER-ANSWER-TBOX-QUERY
                                             RACER-ANSWER-TBOX-QUERY1
                                             RACER-PREPARE-QUERY
                                             RACER-PREPARE-QUERY1
                                             RACER-PREPARE-TBOX-QUERY
                                             RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::REMOVE-ENTAILED-EXPLANATIONS-P :BOOLEAN ADD-EXPLANATION-ASSERTIONS GET-EXPLANATIONS)
    (THEMATIC-SUBSTRATE::REMOVE-IMPLIED-CONCEPT-ASSERTIONS-P :BOOLEAN COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (THEMATIC-SUBSTRATE::REMOVE-MARKER-SYMBOLS-P :BOOLEAN ADD-EXPLANATION-ASSERTIONS GET-EXPLANATIONS)
    (THEMATIC-SUBSTRATE::REMOVE-REDUNDANT-DIFFS-P :BOOLEAN COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (OWLAPI::REMOVE-SELF-P :BOOLEAN |OWLAPI-getAncestorProperties| |OWLAPI-getDescendantProperties| |OWLAPI-getEquivalentProperties|)
    (THEMATIC-SUBSTRATE::REMOVE-SYNONYMS-P :BOOLEAN GET-INDIVIDUAL-SUCCESSORS)
    (RENAME-INDIVIDUALS :BOOLEAN MATERIALIZE-INFERENCES)
    (THEMATIC-SUBSTRATE::REPORT-INCONSISTENT-QUERIES :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::REPORT-INCONSISTENT-QUERIES-P
     :BOOLEAN
     MAKE-FORWARD-RULE-FROM-ABOXES
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::REPORT-TAUTOLOGICAL-QUERIES :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::REPORT-TAUTOLOGICAL-QUERIES-P
     :BOOLEAN
     MAKE-FORWARD-RULE-FROM-ABOXES
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1)
    (RES-ARGS :NRQL-QUERY-HEAD PRACER-ANSWER-QUERY)
    (THEMATIC-SUBSTRATE::RES-ARGS
     :NRQL-QUERY-HEAD
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1
     RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (THEMATIC-SUBSTRATE::RES-ARGS :IGNORE RACER-APPLY-RULE RACER-APPLY-RULE-UNDER-PREMISE RACER-APPLY-RULE-UNDER-PREMISE1 RACER-APPLY-RULE1 RACER-PREPARE-RULE RACER-PREPARE-RULE1)
    (RESET :BOOLEAN INIT-TBOX)
    (THEMATIC-SUBSTRATE::RESET-P :BOOLEAN DISABLE-ABDUCTION ENABLE-ABDUCTION)
    (THEMATIC-SUBSTRATE::RESULT-VOIS :IGNORE
                                     MAKE-FORWARD-RULE-FROM-ABOXES
                                     RACER-ANSWER-QUERY
                                     RACER-ANSWER-QUERY-UNDER-PREMISE
                                     RACER-ANSWER-QUERY-UNDER-PREMISE1
                                     RACER-ANSWER-QUERY1
                                     RACER-ANSWER-TBOX-QUERY
                                     RACER-ANSWER-TBOX-QUERY1
                                     RACER-APPLY-RULE
                                     RACER-APPLY-RULE-UNDER-PREMISE
                                     RACER-APPLY-RULE-UNDER-PREMISE1
                                     RACER-APPLY-RULE1
                                     RACER-PREPARE-QUERY
                                     RACER-PREPARE-QUERY1
                                     RACER-PREPARE-RULE
                                     RACER-PREPARE-RULE1
                                     RACER-PREPARE-TBOX-QUERY
                                     RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::REVERSE-ORDER-P :BOOLEAN ADD-EXPLANATION-ASSERTIONS GET-EXPLANATIONS COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2 ENABLE-ABDUCTION RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (REWRITE-CONCEPT-DEFINITIONS :BOOLEAN ENABLE-OPTIMIZED-QUERY-PROCESSING)
    (THEMATIC-SUBSTRATE::REWRITE-DEFINED-CONCEPTS-P
     :BOOLEAN
     MAKE-FORWARD-RULE-FROM-ABOXES
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::REWRITE-SEMANTICALLY-P
     :BOOLEAN
     MAKE-FORWARD-RULE-FROM-ABOXES
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::REWRITE-TO-DNF :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::REWRITE-TO-DNF-P
     :BOOLEAN
     MAKE-FORWARD-RULE-FROM-ABOXES
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::REWRITTEN-P :BOOLEAN DESCRIBE-QUERY DESCRIBE-RULE DESCRIBE-ALL-QUERIES DESCRIBE-ALL-RULES)
    (RIGHT :CONCEPT-EXPRESSION EQUIVALENT IMPLIES ADD-CONCEPT-AXIOM FORGET-CONCEPT-AXIOM)
    (RIGHT-NAME :ABOX-INDIVIDUAL RELATED UNRELATED)
    (RIGHTHAND-SIDE :RULE-RIGHTHAND-SIDE DEFINE-RULE ADD-RULE-AXIOM)
    (THEMATIC-SUBSTRATE::ROLE :ROLE REGISTER-RCC-SYNONYM)
    (ROLE :datatype-property DATATYPE-ROLE-FILLER ADD-DATATYPE-ROLE-FILLER ADD-NEGATIVE-DATATYPE-ROLE-FILLER FORGET-DATATYPE-ROLE-FILLER FORGET-NEGATIVE-DATATYPE-ROLE-FILLER)
    (ROLE :ROLE ROLE-SATISFIABLE?)
    (ROLE :ROLE ROLE-SATISFIABLE-P)
    (ROLE :ANNOTATION-ROLE RETRIEVE-INDIVIDUAL-ANNOTATION-PROPERTY-FILLERS)
    (ROLE-1 :ROLE ROLE-EQUIVALENT-P)
    (ROLE-2 :ROLE ROLE-EQUIVALENT-P)
    (ROLE-FILLERS :BOOLEAN MATERIALIZE-INFERENCES)
    (ROLE-NAME :ROLE RELATED UNRELATED ADD-ROLE-AXIOM ADD-ROLE-AXIOMS ROLE-USED-AS-ANNOTATION-PROPERTY-P DATATYPE-ROLE-RANGE GET-ROLE-DATATYPE ROLE-USED-AS-DATATYPE-PROPERTY-P)
    (ROLE-SIZE :IGNORE IN-TBOX)
    (ROLE-TERM
     :ROLE
     ASYMMETRIC?
     DIRECT-PREDECESSORS
     FEATURE?
     INDIVIDUAL-FILLERS
     INDIVIDUALS-RELATED?
     IRREFLEXIVE?
     REFLEXIVE?
     RELATED-INDIVIDUALS
     ROLE-ANCESTORS
     ROLE-CHILDREN
     ROLE-DESCENDANTS
     ROLE-DOMAIN
     ROLE-INVERSE
     ROLE-PARENTS
     ROLE-RANGE
     ROLE-SYNONYMS
     ROLE?
     SYMMETRIC?
     TRANSITIVE?
     ADD-ANNOTATION-ROLE-ASSERTION
     ADD-NEGATED-ROLE-ASSERTION
     ADD-ROLE-ASSERTION
     ASYMMETRIC-P
     ATOMIC-ROLE-ANCESTORS
     ATOMIC-ROLE-CHILDREN
     ATOMIC-ROLE-DESCENDANTS
     ATOMIC-ROLE-DOMAIN
     ATOMIC-ROLE-INVERSE
     ATOMIC-ROLE-PARENTS
     ATOMIC-ROLE-RANGE
     ATOMIC-ROLE-SYNONYMS
     DESCRIBE-ROLE
     FEATURE-P
     FORGET-NEGATED-ROLE-ASSERTION
     FORGET-ROLE-ASSERTION
     INDIVIDUALS-RELATED-P
     INTERNAL-INDIVIDUALS-RELATED-P
     INVERSE-FEATURE-P
     IRREFLEXIVE-P
     REFLEXIVE-P
     RETRIEVE-DIRECT-PREDECESSORS
     RETRIEVE-INDIVIDUAL-FILLERS
     RETRIEVE-RELATED-INDIVIDUALS
     ROLE-P
     SYMMETRIC-P
     TRANSITIVE-P)
    (ROLE-TERM-1 :ROLE ROLE-DISJOINT? ROLE-EQUIVALENT? ROLE-SUBSUMES? ROLE-DISJOINT-P ROLE-SUBSUMES-P)
    (ROLE-TERM-2 :ROLE ROLE-DISJOINT? ROLE-EQUIVALENT? ROLE-SUBSUMES? ROLE-DISJOINT-P ROLE-SUBSUMES-P)
    (ROLE1 :ROLE ROLES-DISJOINT ROLES-EQUIVALENT ROLES-DISJOINT-1 ROLES-EQUIVALENT-1)
    (ROLE2 :ROLE ROLES-DISJOINT ROLES-EQUIVALENT ROLES-DISJOINT-1 ROLES-EQUIVALENT-1)
    (ROLENAME
     :ROLE
     ASYMMETRIC
     DOMAIN
     FUNCTIONAL
     INVERSE
     IRREFLEXIVE
     RANGE
     REFLEXIVE
     SYMMETRIC
     TRANSITIVE
     DATATYPE-ROLE-HAS-RANGE
     INVERSE-OF-ROLE
     ROLE-HAS-DOMAIN
     ROLE-HAS-RANGE
     ROLE-IS-ASYMMETRIC
     ROLE-IS-FUNCTIONAL
     ROLE-IS-IRREFLEXIVE
     ROLE-IS-REFLEXIVE
     ROLE-IS-SYMMETRIC
     ROLE-IS-TRANSITIVE
     ROLE-IS-USED-AS-ANNOTATION-PROPERTY
     ROLE-IS-USED-AS-DATATYPE-PROPERTY)
    (ROLENAME-1 :ROLE IMPLIES-ROLE ROLE-HAS-PARENT)
    (ROLENAME-2 :ROLE IMPLIES-ROLE ROLE-HAS-PARENT)
    (THEMATIC-SUBSTRATE::ROLES (:LIST :ROLE) GET-INDIVIDUAL-SUCCESSORS)
    (ROLES (:LIST :ROLE) SIGNATURE ENSURE-TBOX-SIGNATURE RETRIEVE-INDIVIDUAL-FILLED-ROLES)
    (THEMATIC-SUBSTRATE::ROOT-INDIVIDUALS (:LIST :ABOX-INDIVIDUAL) GET-ABOX-GRAPH)
    (THEMATIC-SUBSTRATE::RULE-CON-PATTERN :NRQL-RULE-CONSEQUENCE
                                          MAKE-FORWARD-RULE-FROM-ABOXES
                                          RACER-ANSWER-QUERY
                                          RACER-ANSWER-QUERY-UNDER-PREMISE
                                          RACER-ANSWER-QUERY-UNDER-PREMISE1
                                          RACER-ANSWER-QUERY1
                                          RACER-ANSWER-TBOX-QUERY
                                          RACER-ANSWER-TBOX-QUERY1
                                          RACER-APPLY-RULE
                                          RACER-APPLY-RULE-UNDER-PREMISE
                                          RACER-APPLY-RULE-UNDER-PREMISE1
                                          RACER-APPLY-RULE1
                                          RACER-PREPARE-QUERY
                                          RACER-PREPARE-QUERY1
                                          RACER-PREPARE-RULE
                                          RACER-PREPARE-RULE1
                                          RACER-PREPARE-TBOX-QUERY
                                          RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::RULES-P :BOOLEAN GET-ALL-ANSWERS)
    (THEMATIC-SUBSTRATE::RUNTIME-CONSISTENCY-CHECKING-P
     :BOOLEAN
     EXECUTE-OR-REEXECUTE-QUERY
     EXECUTE-QUERY
     COMPUTE-ABOX-DIFFERENCE1
     COMPUTE-ABOX-DIFFERENCE2
     ENABLE-ABDUCTION
     EXECUTE-ALL-QUERIES
     EXECUTE-OR-REEXECUTE-ALL-QUERIES
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-ANSWER-TBOX-QUERY
     RACER-ANSWER-TBOX-QUERY1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-TBOX-QUERY
     RACER-PREPARE-TBOX-QUERY1
     RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (THEMATIC-SUBSTRATE::SAME-AS-ONLY-P :BOOLEAN COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2 ENABLE-ABDUCTION RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (SAME-INDIVIDUAL-AS :BOOLEAN MATERIALIZE-INFERENCES)
    (OWLAPI:SECOND-OBJECT-PROPERTY :OBJECT-PROPERTY |OWLAPI-getOWLInverseObjectPropertiesAxiom|)
    (OWLAPI:SECONDARY-P :BOOLEAN |OWLAPI-newOntology|)
    (OWLAPI::SECONDS :NON-NEGATIVE-INTEGER |OWLAPI-sleep|)
    (THEMATIC-SUBSTRATE::SELECTED-INDIVIDUALS (:LIST :ABOX-INDIVIDUAL) GET-ABOX-GRAPH)
    (THEMATIC-SUBSTRATE::SHORT-DESCRIPTION :STRING MAKE-PLUGIN-FROM-FASL-FILE)
    (THEMATIC-SUBSTRATE::SHOW-SCORE-P :BOOLEAN ADD-EXPLANATION-ASSERTIONS GET-EXPLANATIONS COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2)
    (THEMATIC-SUBSTRATE::SHOW-SYNONYMS-P :BOOLEAN GET-INDIVIDUAL-SUCCESSORS)
    (SIGNATURE :TBOX-NAME IN-TBOX)
    (SIGNATURE-FORM :IGNORE SIGNATURE)
    (SIMPLE-PROTOCOL-P :BOOLEAN SUBSCRIBE-1)
    (THEMATIC-SUBSTRATE::SIMPLE-RESULT-P :BOOLEAN COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2 RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (SIZE :IGNORE IN-TBOX)
    (SPARQL-QUERY :SPARQL-QUERY-STRING SPARQL-ANSWER-QUERY SPARQL-RETRIEVE)
    (OWLAPI::STATUS (:ONE-OF :LOADED :UNLOADED) |OWLAPI-getAxioms| |OWLAPI-getAxiomsIn| |OWLAPI-getAxiomsOfType| |OWLAPI-getAxiomsOfTypeIn|)
    (OWLAPI::STEPS :NON-NEGATIVE-INTEGER |OWLAPI-setProgressRange|)
    (THEMATIC-SUBSTRATE::STRATEGY :IGNORE COMPUTE-ABOX-DIFFERENCE1 COMPUTE-ABOX-DIFFERENCE2 RACER-ANSWER-QUERY-WITH-EXPLANATION)
    (STREAM :IGNORE SPARQL-ANSWER-QUERY SPARQL-RETRIEVE CHECK-TBOX-COHERENCE DESCRIBE-ABOX DESCRIBE-CONCEPT DESCRIBE-INDIVIDUAL DESCRIBE-INDIVIDUAL1 DESCRIBE-ROLE DESCRIBE-TBOX PRINT-ABOX-INDIVIDUALS PRINT-TBOX-TREE)
    (OWL-SYNTAXES::STRICT-SYNTAX-P :BOOLEAN |OWLAPI-readFunctionalOntologyDocument| |OWLAPI-readFunctionalOntologyFile| |OWLAPI-readOntology| |OWLAPI-readXMLOntologyDocument| |OWLAPI-readXMLOntologyFile|)
    (STRING :STRING ADD-DOC-PHRASE1 |OWLAPI-parseNative|)
    (OWLAPI:SUB-CLASS :CONCEPT-EXPRESSION |OWLAPI-getOWLSubClassAxiom|)
    (SUBGRAPH :IGNORE USE-TRIPLE-STORE)
    (SUBGRAPH-MARKERS :IGNORE MATERIALIZE-INFERENCES)
    (OWLAPI:SUBJECT :ABOX-INDIVIDUAL
                    |OWLAPI-getOWLDataPropertyAssertionAxiom|
                    |OWLAPI-getOWLNegativeDataPropertyAssertionAxiom|
                    |OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom|
                    |OWLAPI-getOWLObjectPropertyAssertionAxiom|
                    |OWLAPI-getRelatedIndividuals|
                    |OWLAPI-getRelatedValues|
                    |OWLAPI-hasDataPropertyRelationship|
                    |OWLAPI-hasObjectPropertyRelationship|)
    (SUBSCRIBER :SYMBOL SUBSCRIBE UNSUBSCRIBE)
    (THEMATIC-SUBSTRATE::SUBSCRIBER-NAME :SYMBOL SUBSCRIBE-TO UNSUBSCRIBE-FROM)
    (SUBSCRIBER-NAME :SYMBOL SUBSCRIBE-1 UNSUBSCRIBE-1)
    (THEMATIC-SUBSTRATE::SUBSTRATE :SUBSTRATE-NAME
                                   MAKE-FORWARD-RULE-FROM-ABOXES
                                   PREPARE-NRQL-ENGINE
                                   RACER-ANSWER-QUERY
                                   RACER-ANSWER-QUERY-UNDER-PREMISE
                                   RACER-ANSWER-QUERY-UNDER-PREMISE1
                                   RACER-ANSWER-QUERY1
                                   RACER-ANSWER-TBOX-QUERY
                                   RACER-ANSWER-TBOX-QUERY1
                                   RACER-APPLY-RULE
                                   RACER-APPLY-RULE-UNDER-PREMISE
                                   RACER-APPLY-RULE-UNDER-PREMISE1
                                   RACER-APPLY-RULE1
                                   RACER-PREPARE-QUERY
                                   RACER-PREPARE-QUERY1
                                   RACER-PREPARE-RULE
                                   RACER-PREPARE-RULE1
                                   RACER-PREPARE-TBOX-QUERY
                                   RACER-PREPARE-TBOX-QUERY1)
    (SUBSUMEE :CONCEPT-EXPRESSION CONCEPT-SUBSUMES-P)
    (SUBSUMER :CONCEPT-EXPRESSION CONCEPT-SUBSUMES-P)
    (OWLAPI:SUPER-CLASS :CONCEPT-EXPRESSION |OWLAPI-getOWLSubClassAxiom|)
    (SYMMETRIC :BOOLEAN DEFINE-PRIMITIVE-ATTRIBUTE DEFINE-PRIMITIVE-ROLE ADD-ROLE-AXIOM ADD-ROLE-AXIOMS FORGET-ROLE-AXIOMS)
    (SYMMETRIC-P :BOOLEAN ADD-ROLE-AXIOM ADD-ROLE-AXIOMS)
    (OWLAPI::SYNONYMS :BOOLEAN |OWLAPI-getDifferentIndividuals| |OWLAPI-getInstances| |OWLAPI-getObjectPropertyValues|)
    (SYNSETS-P :BOOLEAN ATOMIC-ROLE-CHILDREN ATOMIC-ROLE-PARENTS RETRIEVE-INDIVIDUAL-FILLED-ROLES)
    (OWLAPI::SYNTAX :IGNORE |OWLAPI-exportOntology| |OWLAPI-exportReasoner|)
    (OWL-SYNTAXES::SYNTAX (:ONE-OF :OWL-RDF :RDF-XML :OWL :OWL-FUNCTIONAL :OFN :OWF :FUNCT :FUNCTIONAL :OWL-XML :XML :OWX) |OWLAPI-readOntology| |OWLAPI-saveOntology|)
    (SYNTAX (:ONE-OF :RACE :KRSS :OWL :TEST) SAVE-ABOX SAVE-KB SAVE-TBOX)
    (THEMATIC-SUBSTRATE::TBOX :TBOX-NAME
                              WITH-NRQL-SETTINGS-EVALUATED
                              WITH-NRQL-SETTINGS
                              ABOX-ENTAILS-ABOX-P
                              ACTIVATE-DEFINED-QUERY
                              CHECK-CONCEPT-COHERENCE
                              CREATE-SUBGRAPH-ABOXES
                              DEACTIVATE-DEFINED-QUERY
                              DEFINE-AND-EXECUTE-QUERY
                              DEFINE-AND-PREPARE-QUERY
                              DEFINE-QUERY
                              DELETE-ALL-DEFINITIONS
                              DESCRIBE-ALL-DEFINITIONS
                              DESCRIBE-DEFINITION
                              GET-CONCEPT-PROPERTIES
                              GET-ROLE-HIERARCHY
                              PREPARE-NRQL-ENGINE
                              RACER-ANSWER-TBOX-QUERY
                              RACER-ANSWER-TBOX-QUERY1
                              RACER-PREPARE-TBOX-QUERY
                              RACER-PREPARE-TBOX-QUERY1
                              UNDEFINE-QUERY)
    (TBOX :TBOX-NAME
                 ASYMMETRIC
                 ATTRIBUTE-DOMAIN
                 CLONE-TBOX
                 CONCEPT-ANCESTORS
                 CONCEPT-CHILDREN
                 CONCEPT-DESCENDANTS
                 CONCEPT-IS-PRIMITIVE?
                 CONCEPT-PARENTS
                 CONCEPT-SYNONYMS
                 DELETE-TBOX
                 DOMAIN
                 FUNCTIONAL
                 GET-CONCEPT-DEFINITION
                 GET-CONCEPT-NEGATED-DEFINITION
                 IMPLIES-ROLE
                 INVERSE
                 IRREFLEXIVE
                 RANGE
                 REFLEXIVE
                 ROLE-ANCESTORS
                 ROLE-CHILDREN
                 ROLE-DESCENDANTS
                 ROLE-DISJOINT?
                 ROLE-DOMAIN
                 ROLE-EQUIVALENT?
                 ROLE-INVERSE
                 ROLE-PARENTS
                 ROLE-RANGE
                 ROLE-SATISFIABLE?
                 ROLE-SUBSUMES?
                 ROLE-SYNONYMS
                 ROLES-DISJOINT
                 ROLES-EQUIVALENT
                 SYMMETRIC
                 TRANSITIVE
                 GET-PREFIXES
                 ADD-CONCEPT-AXIOM
                 ADD-DATATYPE-PROPERTY
                 ADD-DISJOINTNESS-AXIOM
                 ADD-ROLE-AXIOM
                 ADD-ROLE-AXIOMS
                 ALL-ATOMIC-CONCEPTS
                 ALL-ATTRIBUTES
                 ALL-EQUIVALENT-CONCEPTS
                 ALL-FEATURES
                 ALL-ROLES
                 ALL-TRANSITIVE-ROLES
                 ASSOCIATED-ABOXES
                 ASYMMETRIC-P
                 ATOMIC-CONCEPT-ANCESTORS
                 ATOMIC-CONCEPT-CHILDREN
                 ATOMIC-CONCEPT-DESCENDANTS
                 ATOMIC-CONCEPT-PARENTS
                 ATOMIC-CONCEPT-SYNONYMS
                 ATOMIC-ROLE-ANCESTORS
                 ATOMIC-ROLE-CHILDREN
                 ATOMIC-ROLE-DESCENDANTS
                 ATOMIC-ROLE-DOMAIN
                 ATOMIC-ROLE-INVERSE
                 ATOMIC-ROLE-PARENTS
                 ATOMIC-ROLE-RANGE
                 ATOMIC-ROLE-SYNONYMS
                 ATTRIBUTE-DOMAIN-1
                 ATTRIBUTE-TYPE
                 CD-ATTRIBUTE-P
                 CHECK-TBOX-COHERENCE
                 CLASSIFY-TBOX
                 CONCEPT-DISJOINT-P
                 CONCEPT-EQUIVALENT-P
                 CONCEPT-IS-PRIMITIVE-P
                 CONCEPT-P
                 CONCEPT-SATISFIABLE-P
                 CONCEPT-SUBSUMES-P
                 CREATE-TBOX-CLONE
                 CREATE-TBOX-INTERNAL-MARKER-CONCEPT
                 DATATYPE-ROLE-HAS-RANGE
                 DATATYPE-ROLE-RANGE
                 DECLARE-DISJOINT
                 DESCRIBE-CONCEPT
                 DESCRIBE-ROLE
                 DESCRIBE-TBOX
                 ENSURE-TBOX-SIGNATURE
                 FEATURE-P
                 FIND-TBOX
                 FORGET-CONCEPT-AXIOM
                 FORGET-DISJOINTNESS-AXIOM
                 FORGET-DISJOINTNESS-AXIOM-STATEMENT
                 FORGET-ROLE-AXIOMS
                 FORGET-STATEMENT
                 FORGET-TBOX
                 GET-CONCEPT-DEFINITION-1
                 GET-CONCEPT-NEGATED-DEFINITION-1
                 GET-CONCEPT-PMODEL
                 GET-DATA-BOTTOM-ROLE
                 GET-META-CONSTRAINT
                 GET-NAMESPACE-PREFIX
                 GET-OBJECT-BOTTOM-ROLE
                 GET-ROLE-DATATYPE
                 GET-TBOX-LANGUAGE
                 GET-TBOX-SIGNATURE
                 GET-TBOX-VERSION
                 INIT-ABOX
                 INIT-TBOX
                 INVERSE-FEATURE-P
                 INVERSE-OF-ROLE
                 IRREFLEXIVE-P
                 LCS-UNFOLD
                 MATERIALIZE-INFERENCES
                 PRINT-TBOX-TREE
                 REFLEXIVE-P
                 ROLE-DISJOINT-P
                 ROLE-EQUIVALENT-P
                 ROLE-HAS-DOMAIN
                 ROLE-HAS-PARENT
                 ROLE-HAS-RANGE
                 ROLE-IS-ASYMMETRIC
                 ROLE-IS-FUNCTIONAL
                 ROLE-IS-IRREFLEXIVE
                 ROLE-IS-REFLEXIVE
                 ROLE-IS-SYMMETRIC
                 ROLE-IS-TRANSITIVE
                 ROLE-IS-USED-AS-ANNOTATION-PROPERTY
                 ROLE-IS-USED-AS-DATATYPE-PROPERTY
                 ROLE-P
                 ROLE-SATISFIABLE-P
                 ROLE-SUBSUMES-P
                 ROLE-USED-AS-ANNOTATION-PROPERTY-P
                 ROLE-USED-AS-DATATYPE-PROPERTY-P
                 ROLES-DISJOINT-1
                 ROLES-EQUIVALENT-1
                 SAVE-KB
                 SAVE-TBOX
                 SET-CURRENT-TBOX
                 STORE-TBOX-IMAGE
                 SYMMETRIC-P
                 TAXONOMY
                 TBOX-CLASSIFIED-P
                 TBOX-COHERENT-P
                 TBOX-CYCLIC-P
                 TBOX-PREPARED-P
                 TRANSITIVE-P
                 VERIFY-WITH-CONCEPT-TREE-LIST)
    (TBOX-NAME :TBOX-NAME
                      ASYMMETRIC?
                      CD-ATTRIBUTE?
                      CONCEPT-DISJOINT?
                      CONCEPT-EQUIVALENT?
                      CONCEPT-SATISFIABLE?
                      CONCEPT-SUBSUMES?
                      CONCEPT?
                      FEATURE?
                      IN-ABOX
                      IN-KNOWLEDGE-BASE
                      IRREFLEXIVE?
                      REFLEXIVE?
                      ROLE?
                      SYMMETRIC?
                      TBOX-CLASSIFIED?
                      TBOX-COHERENT?
                      TBOX-CYCLIC?
                      TBOX-PREPARED?
                      TRANSITIVE?)
    (TBOX-NAME1 :TBOX-NAME SET-FIND-TBOX)
    (TBOX-NAME2 :TBOX-NAME SET-FIND-TBOX)
    (TBOXES (:LIST :TBOX-NAME) STORE-TBOXES-IMAGE)
    (TEST :IGNORE ALL-ROLES)
    (THEMATIC-SUBSTRATE::TEXT-DESCRIPTION :STRING MAKE-PLUGIN-FROM-FASL-FILE)
    (THEMATIC-SUBSTRATE::TIMEOUT :NON-NEGATIVE-INTEGER
                                 WITH-NRQL-SETTINGS-EVALUATED
                                 WITH-NRQL-SETTINGS
                                 EXECUTE-OR-REEXECUTE-QUERY
                                 EXECUTE-QUERY
                                 EXECUTE-ALL-QUERIES
                                 EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                 PREPARE-NRQL-ENGINE
                                 RACER-ANSWER-QUERY
                                 RACER-ANSWER-QUERY-UNDER-PREMISE
                                 RACER-ANSWER-QUERY-UNDER-PREMISE1
                                 RACER-ANSWER-QUERY1
                                 RACER-ANSWER-TBOX-QUERY
                                 RACER-ANSWER-TBOX-QUERY1
                                 RACER-PREPARE-QUERY
                                 RACER-PREPARE-QUERY1
                                 RACER-PREPARE-TBOX-QUERY
                                 RACER-PREPARE-TBOX-QUERY1)
    (TIMEOUT :NON-NEGATIVE-INTEGER SET-SERVER-TIMEOUT)
    (THEMATIC-SUBSTRATE::TO :IGNORE ADD-EXPLANATION-ASSERTIONS)
    (THEMATIC-SUBSTRATE::TO :NON-NEGATIVE-INTEGER GET-EXPLANATIONS)
    (THEMATIC-SUBSTRATE::TO :SUBSTRATE-NODE CREATE-DATA-EDGE DATA-EDGE1 DEL-DATA-EDGE1 DELETE-DATA-EDGE EDGE-DESCRIPTION1 EDGE-LABEL1 GET-DATA-EDGE-DESCRIPTION GET-DATA-EDGE-LABEL)
    (OWLAPI::TO :NON-NEGATIVE-INTEGER |OWLAPI-setProgressRange|)
    (THEMATIC-SUBSTRATE::TO-ABOX :ABOX-NAME COPY-RULES MOVE-RULES)
    (THEMATIC-SUBSTRATE::TO-SUBSTRATE :SUBSTRATE-NAME REPREPARE-QUERY)
    (TOLD :BOOLEAN RETRIEVE-INDIVIDUAL-FILLERS)
    (TOLD-DATATYPE-FILLERS :BOOLEAN MATERIALIZE-INFERENCES)
    (THEMATIC-SUBSTRATE::TOLD-INFO-P :BOOLEAN CREATE-DATA-EDGE CREATE-DATA-NODE DELETE-DATA-EDGE DELETE-DATA-NODE)
    (THEMATIC-SUBSTRATE::TOLD-INFORMATION-QUERYING :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::TOLD-INFORMATION-REASONING-P :BOOLEAN
                                                      EXECUTE-OR-REEXECUTE-QUERY
                                                      EXECUTE-QUERY
                                                      EXECUTE-ALL-QUERIES
                                                      EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                      RACER-ANSWER-QUERY
                                                      RACER-ANSWER-QUERY-UNDER-PREMISE
                                                      RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                      RACER-ANSWER-QUERY1
                                                      RACER-ANSWER-TBOX-QUERY
                                                      RACER-ANSWER-TBOX-QUERY1
                                                      RACER-PREPARE-QUERY
                                                      RACER-PREPARE-QUERY1
                                                      RACER-PREPARE-TBOX-QUERY
                                                      RACER-PREPARE-TBOX-QUERY1)
    (TOLD-ONLY :BOOLEAN INDIVIDUAL-ANTONYMS INDIVIDUAL-SYNONYMS RETRIEVE-INDIVIDUAL-ANTONYMS RETRIEVE-INDIVIDUAL-SYNONYMS USE-TRIPLE-STORE)
    (THEMATIC-SUBSTRATE::TOLD-ONLY-P :BOOLEAN GET-ABOX-GRAPH)
    (TRANSFORMED :BOOLEAN SAVE-ABOX SAVE-TBOX)
    (TRANSITIVE :BOOLEAN DEFINE-PRIMITIVE-ROLE ADD-ROLE-AXIOM ADD-ROLE-AXIOMS FORGET-ROLE-AXIOMS)
    (TRANSITIVE-P :BOOLEAN ADD-ROLE-AXIOM ADD-ROLE-AXIOMS FORGET-ROLE-AXIOMS)
    (TRANSITIVE-ROLES (:LIST :ROLE) SIGNATURE ENSURE-TBOX-SIGNATURE)
    (TREE-LIST :IGNORE VERIFY-WITH-CONCEPT-TREE-LIST)
    (THEMATIC-SUBSTRATE::TUPLE-AT-A-TIME-P :BOOLEAN
                                           EXECUTE-OR-REEXECUTE-QUERY
                                           EXECUTE-QUERY
                                           EXECUTE-ALL-QUERIES
                                           EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                           RACER-ANSWER-QUERY
                                           RACER-ANSWER-QUERY-UNDER-PREMISE
                                           RACER-ANSWER-QUERY-UNDER-PREMISE1
                                           RACER-ANSWER-QUERY1
                                           RACER-ANSWER-TBOX-QUERY
                                           RACER-ANSWER-TBOX-QUERY1
                                           RACER-PREPARE-QUERY
                                           RACER-PREPARE-QUERY1
                                           RACER-PREPARE-TBOX-QUERY
                                           RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::TUPLE-COMPUTATION-MODE (:ONE-OF :TUPLE-AT-A-TIME-LAZY :TUPLE-AT-A-TIME-EAGER :SET-AT-A-TIME) WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::TUPLES-P :BOOLEAN ADD-EXPLANATION-ASSERTIONS GET-EXPLANATIONS)
    (THEMATIC-SUBSTRATE::TWO-PHASE-PROCESSING-P :BOOLEAN
                                                EXECUTE-OR-REEXECUTE-QUERY
                                                EXECUTE-QUERY
                                                EXECUTE-ALL-QUERIES
                                                EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                RACER-ANSWER-QUERY
                                                RACER-ANSWER-QUERY-UNDER-PREMISE
                                                RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                RACER-ANSWER-QUERY1
                                                RACER-ANSWER-TBOX-QUERY
                                                RACER-ANSWER-TBOX-QUERY1
                                                RACER-PREPARE-QUERY
                                                RACER-PREPARE-QUERY1
                                                RACER-PREPARE-TBOX-QUERY
                                                RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::TWO-PHASE-QUERY-PROCESSING-MODE :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (TYPE :CD-TYPE SET-ATTRIBUTE-FILLER ATTRIBUTE-FILLER DEFINE-CONCRETE-DOMAIN-ATTRIBUTE)
    (TYPE (:one-of :gif-image :jpeg-image) ADD-DOC-IMAGE-DATA-FROM-FILE1)
    (TYPE :IGNORE MAKE-PLUGIN-FROM-FASL-FILE)
    (TYPE :CONCEPT-EXPRESSION |OWLAPI-hasType|)
    (TYPE (:ONE-OF :ANSWER-DIRECT :GET-LAST-ANSWER :SMART) |OWLAPI-setReturnPolicy|)
    (TYPE (:ONE-OF :RCC-SUBSTRATE :RCC-MIRROR-SUBSTRATE) SET-RCC-BOX)
    (TYPE :DATATYPE DATATYPE-ROLE-FILLER ADD-DATATYPE-ROLE-FILLER ADD-NEGATIVE-DATATYPE-ROLE-FILLER)
    (THEMATIC-SUBSTRATE::TYPE-OF-SUBSTRATE
     (:ONE-OF :DATA-SUBSTRATE :RCC-SUBSTRATE :MIRROR-DATA-SUBSTRATE :RCC-MIRROR-SUBSTRATE)
     WITH-NRQL-SETTINGS-EVALUATED
     WITH-NRQL-SETTINGS
     APPLICABLE-RULES
     EXECUTE-APPLICABLE-RULES
     UNAPPLICABLE-RULES
     ABORT-ALL-QUERIES
     ABORT-ALL-RULES
     ACCURATE-QUERIES
     ACCURATE-RULES
     ACTIVE-QUERIES
     ACTIVE-RULES
     ALL-QUERIES
     ALL-RULES
     ALL-SUBSTRATES
     CHEAP-QUERIES
     CHEAP-RULES
     COPY-RULES
     CREATE-DATA-EDGE
     CREATE-DATA-NODE
     DATA-EDGE1
     DATA-NODE1
     DEL-DATA-EDGE1
     DEL-DATA-NODE1
     DELETE-ALL-QUERIES
     DELETE-ALL-RULES
     DELETE-ALL-SUBSTRATES
     DELETE-DATA-EDGE
     DELETE-DATA-NODE
     DESCRIBE-ALL-EDGES
     DESCRIBE-ALL-NODES
     DESCRIBE-ALL-QUERIES
     DESCRIBE-ALL-RULES
     DESCRIBE-SUBSTRATE
     EDGE-DESCRIPTION1
     EDGE-LABEL1
     EXECUTE-ALL-QUERIES
     EXECUTE-ALL-RULES
     EXECUTE-OR-REEXECUTE-ALL-QUERIES
     EXECUTE-OR-REEXECUTE-ALL-RULES
     EXPENSIVE-QUERIES
     EXPENSIVE-RULES
     GET-ALL-ANSWERS
     GET-DATA-EDGE-DESCRIPTION
     GET-DATA-EDGE-LABEL
     GET-DATA-NODE-DESCRIPTION
     GET-DATA-NODE-LABEL
     GET-EDGE-LABEL-FOR-NON-EXISTENT-EDGES
     GET-SUBSTRATE-EDGES
     GET-SUBSTRATE-NODES
     INACCURATE-QUERIES
     INACCURATE-RULES
     MAKE-FORWARD-RULE-FROM-ABOXES
     MOVE-RULES
     NODE-DESCRIPTION1
     NODE-LABEL1
     PREPARE-NRQL-ENGINE
     PROCESSED-QUERIES
     PROCESSED-RULES
     RACER-ANSWER-QUERY
     RACER-ANSWER-QUERY-UNDER-PREMISE
     RACER-ANSWER-QUERY-UNDER-PREMISE1
     RACER-ANSWER-QUERY1
     RACER-APPLY-RULE
     RACER-APPLY-RULE-UNDER-PREMISE
     RACER-APPLY-RULE-UNDER-PREMISE1
     RACER-APPLY-RULE1
     RACER-PREPARE-QUERY
     RACER-PREPARE-QUERY1
     RACER-PREPARE-RULE
     RACER-PREPARE-RULE1
     RCC-CONSISTENT-P
     READY-QUERIES
     READY-RULES
     REEXECUTE-ALL-QUERIES
     REEXECUTE-ALL-RULES
     RESET-ALL-SUBSTRATES
     RUNNING-CHEAP-QUERIES
     RUNNING-CHEAP-RULES
     RUNNING-EXPENSIVE-QUERIES
     RUNNING-EXPENSIVE-RULES
     RUNNING-QUERIES
     RUNNING-RULES
     SET-EDGE-LABEL-FOR-NON-EXISTENT-EDGES
     STORE-SUBSTRATE-FOR-ABOX
     WAITING-CHEAP-QUERIES
     WAITING-CHEAP-RULES
     WAITING-EXPENSIVE-QUERIES
     WAITING-EXPENSIVE-RULES
     WAITING-QUERIES
     WAITING-RULES)
    (OWLAPI::TYPE-OR-TYPES :OWLAPI-AXIOM-TYPE |OWLAPI-getAxiomsOfType| |OWLAPI-getAxiomsOfTypeIn|)
    (OWLAPI:URI :URI |OWLAPI-SetOntologyURI|)
    (URI :URI SAVE-ABOX SAVE-KB SAVE-TBOX)
    (URL :URL ADD-DOC-IMAGE-DATA-FROM-FILE1 ADD-DOC-IMAGE-DATA1 ADD-DOC-IMAGE-FILE1)
    (OWL-SYNTAXES::URL :URL |OWLAPI-readFunctionalOntologyDocument| |OWLAPI-readOntology| |OWLAPI-readXMLOntologyDocument|)
    (THEMATIC-SUBSTRATE::URL :URL CHECK-FOR-UPDATES UPDATE-RACER)
    (URL :URL OWLLINK-READ-DOCUMENT)
    (URL:URL :URL PUBLISH-FILE)
    (URL-OR-FILENAME (:or :URL :FILENAME) MIRROR)
    (URL-SPEC :URL DIG-READ-DOCUMENT OWL-READ-DOCUMENT RACER-READ-DOCUMENT)
    (URL-SPEC1 :URL MIRROR)
    (OWL-SYNTAXES::USE-FLIPPED-CLASS-ASSERTIONS-P :BOOLEAN |OWLAPI-readFunctionalOntologyDocument| |OWLAPI-readFunctionalOntologyFile| |OWLAPI-readOntology| |OWLAPI-readXMLOntologyDocument| |OWLAPI-readXMLOntologyFile|)
    (THEMATIC-SUBSTRATE::USE-INDIVIDUAL-EQUIVALENCE-CLASSES :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (THEMATIC-SUBSTRATE::USE-INDIVIDUAL-SYNONYMS-P :BOOLEAN
                                                   EXECUTE-OR-REEXECUTE-QUERY
                                                   EXECUTE-QUERY
                                                   EXECUTE-ALL-QUERIES
                                                   EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                                   RACER-ANSWER-QUERY
                                                   RACER-ANSWER-QUERY-UNDER-PREMISE
                                                   RACER-ANSWER-QUERY-UNDER-PREMISE1
                                                   RACER-ANSWER-QUERY1
                                                   RACER-ANSWER-TBOX-QUERY
                                                   RACER-ANSWER-TBOX-QUERY1
                                                   RACER-PREPARE-QUERY
                                                   RACER-PREPARE-QUERY1
                                                   RACER-PREPARE-TBOX-QUERY
                                                   RACER-PREPARE-TBOX-QUERY1)
    (USE-OPTIMIZER :BOOLEAN SPARQL-ANSWER-QUERY SPARQL-RETRIEVE PRACER-ANSWER-QUERY)
    (THEMATIC-SUBSTRATE::USE-REPOSITORY-P :BOOLEAN
                                          MAKE-FORWARD-RULE-FROM-ABOXES
                                          RACER-ANSWER-QUERY
                                          RACER-ANSWER-QUERY-UNDER-PREMISE
                                          RACER-ANSWER-QUERY-UNDER-PREMISE1
                                          RACER-ANSWER-QUERY1
                                          RACER-ANSWER-TBOX-QUERY
                                          RACER-ANSWER-TBOX-QUERY1
                                          RACER-APPLY-RULE
                                          RACER-APPLY-RULE-UNDER-PREMISE
                                          RACER-APPLY-RULE-UNDER-PREMISE1
                                          RACER-APPLY-RULE1
                                          RACER-PREPARE-QUERY
                                          RACER-PREPARE-QUERY1
                                          RACER-PREPARE-RULE
                                          RACER-PREPARE-RULE1
                                          RACER-PREPARE-TBOX-QUERY
                                          RACER-PREPARE-TBOX-QUERY1)
    (USE-SIMPLIFIED-PROTOCOL-P :BOOLEAN SUBSCRIBE)
    (THEMATIC-SUBSTRATE::USE-SIMPLIFIED-PROTOCOL-P :BOOLEAN SUBSCRIBE-TO UNSUBSCRIBE-FROM)
    (OWLAPI::VAL :BOOLEAN |OWLAPI-setAutoDeclareDataProperties|)
    (THEMATIC-SUBSTRATE::VAL :BOOLEAN SET-REWRITE-DEFINED-CONCEPTS)
    (OWLAPI::VALUE :OWL-DATA-LITERAL |OWLAPI-getOWLDataPropertyAssertionAxiom| |OWLAPI-getOWLNegativeDataPropertyAssertionAxiom|)
    (THEMATIC-SUBSTRATE::VALUE :MINILISP-SEXPRESSION DEFCON1 DEFPAR1)
    (THEMATIC-SUBSTRATE::VALUE :VALUE SET-RACER-PARAMETER)
    (VALUE :CD-VALUE ATTRIBUTE-FILLER DATATYPE-ROLE-FILLER ADD-DATATYPE-ROLE-FILLER ADD-NEGATIVE-DATATYPE-ROLE-FILLER FORGET-DATATYPE-ROLE-FILLER FORGET-NEGATIVE-DATATYPE-ROLE-FILLER SET-ATTRIBUTE-FILLER)
    (VALUE :BOOLEAN SET-UNIQUE-NAME-ASSUMPTION)
    (THEMATIC-SUBSTRATE::VERBOSE :BOOLEAN CHECK-ONTOLOGY)
    (VERBOSE :BOOLEAN |OWLAPI-readOntology| OWL-READ-DOCUMENT OWL-READ-FILE RACER-READ-DOCUMENT SWRL-FORWARD-CHAINING TRIPLE-STORE-READ-FILE USE-TRIPLE-STORE)
    (THEMATIC-SUBSTRATE::VERBOSE-P :BOOLEAN
                                   EXECUTE-OR-REEXECUTE-QUERY
                                   EXECUTE-QUERY
                                   EXECUTE-ALL-QUERIES
                                   EXECUTE-OR-REEXECUTE-ALL-QUERIES
                                   RACER-ANSWER-QUERY
                                   RACER-ANSWER-QUERY-UNDER-PREMISE
                                   RACER-ANSWER-QUERY-UNDER-PREMISE1
                                   RACER-ANSWER-QUERY1
                                   RACER-ANSWER-TBOX-QUERY
                                   RACER-ANSWER-TBOX-QUERY1
                                   RACER-PREPARE-QUERY
                                   RACER-PREPARE-QUERY1
                                   RACER-PREPARE-TBOX-QUERY
                                   RACER-PREPARE-TBOX-QUERY1)
    (THEMATIC-SUBSTRATE::WARNINGS :BOOLEAN WITH-NRQL-SETTINGS-EVALUATED WITH-NRQL-SETTINGS PREPARE-NRQL-ENGINE)
    (OWLAPI:WITH-IDS-P :BOOLEAN |OWLAPI-getAxioms| |OWLAPI-getAxiomsIn| |OWLAPI-getAxiomsOfType| |OWLAPI-getAxiomsOfTypeIn|)
    (OWLAPI::WITH-ONT-NAMES-P :BOOLEAN |OWLAPI-getAxioms| |OWLAPI-getAxiomsOfType|)
    (THEMATIC-SUBSTRATE::WITH-TYPES-P :BOOLEAN GET-INDIVIDUAL-ANNOTATION-DATATYPE-FILLERS GET-INDIVIDUAL-DATATYPE-FILLERS)
    (WITH-TYPES-P :BOOLEAN RETRIEVE-INDIVIDUAL-ANNOTATION-PROPERTY-FILLERS RETRIEVE-INDIVIDUAL-TOLD-DATATYPE-FILLERS)))

;; to recrate: 
;;; (mapcar #'(lambda (x) (list x "unknown")) (remove-duplicates (flatten (mapcar #'(lambda (x) (if (consp (second x)) (if (or (member (first (second x)) '(:goto :one-of))) nil (second x)) (list (second x)))) +known-args-and-types+)))  )


#|

 (and
  #!:Person
  (d-all
   #!:hasAge
   (d-datarange
    (d-base-type #!xs:nonNegativeInteger)
    (d-facet
     #!owl11:minInclusive
     (d-literal "10" (d-base-type #!xs:int)))
    (d-facet
     #!owl11:maxExclusive
     (d-literal "20" (d-base-type #!xs:int))))))

|# 


#| 

(remove-if #'numberp
           (remove-if #'keywordp
                      (set-difference 
                       (remove-duplicates 
                        (flatten
                         (mapcar #'(lambda (x) 
                                     (if (consp x) x (list x)))
                                 (append (mapcar #'second +return-type-for-function+) 
                                         (mapcar #'second +known-args-and-types+)))))
                       (mapcar #'first +type-explanations+))))

|# 

(defconstant +type-explanations+
  '((:LAMBDA-LIST "A MiniLisp lambda list, e.g. @ic{(x y)}")
    ;; (:ONE-OF "unknown")
    (:OWLAPI-AXIOM-CONSTRUCTOR-CALL "An OWLAPI axiom constructor call, e.g. @ic{(OWLAPI-getOWLClassAssertionAxiom betty woman)}")
    (:TBOX-AXIOM "A TBox axiom, e.g. @ic{(define-concept mother (and woman (some has-child person)))}")
    (:ABOX "An anonymous ABox, a list of ABox assertions, e.g. @ic{((instance i C) (related i j R) (instance j D))}")
    (:NRQL-VAR "A nRQL query variable, e.g. @ic{?x} or @ic{?*x}")
    (:NRQL-QUERY-BODY "A nRQL query body (antecedence), e.g. @ic{(and (?x C) (?x ?y R) (?y D))}")
    (:EVENT-RULE-BODY "An event rule body (antecedence)")
    (:BYTE "A byte in decimal notation (a non-negative integer between 0 and 255)")
    (:CONCEPT-ASSERTION "A concept assertion, e.g. @ic{(instance betty woman)}")
    (:DIFFERENT-FROM-ASSERTION "A different-from assertion, e.g. @ic{(different-from betty charles)}")
    (:ROLE-ASSERTION "A role assertion, e.g. @ic{(related betty charles has-child)}")
    (:SAME-AS-ASSERTION "A same-as equality assertion, e.g. @ic{(same-as santa-claus weihnachtsmann)}")
    (:CD-CONSTRAINT-EXPRESSION "A concrete-domain constraint expressions, e.g. @ic{(= temperature-fahrenheit (+ (* 1.8 temperature-celsius) 32))}")
    (:CONTENT-TYPE "MIME type string, e.g. @ic{\"text/html\"} or @ic{\"image/jpeg\"}")
    (:OWL-DATATYPE-EXPRESSION "An OWL2 datatype expression, e.g. @ic{(d-base-type |http://www.w3.org/2001/XMLSchema#integer|)}")
    (:SUBSTRATE-RELATION "A substrate relation")
    (:ROLE-DATATYPE "An OWL2 datatype expression, e.g. @ic{(d-base-type http://www.w3.org/2001/XMLSchema#integer)}")
    (:DATATYPE-NAME "An OWL2 user-defined datatype name, a symbol (e.g., @ic{legal-age})")
    (:SUBSTRATE-LABEL-DESCRIPTION "unknown")
    (:SUBSTRATE-DESCRIPTION-LABEL "unknown")
    (:ENTITY "An OWL2 entity, e.g. @ic{(Class Human)}")
    (:FILE-EXTENSION "A file extension string, e.g. @ic{\"txt\"} or @ic{\"owl\"}")
    (:ANNOTATION-PROPERTY "An annotation property, a special role, e.g. @ic{rdfs:comment}")
    (:BUILD-STRING "A build string as returned by @ic{(get-build-version)}, e.g. @ic{\"2012-07-08\"}, or the keyword @ic{:accept}")
    (:DATATYPE-PROPERTY "An OWL2 datatype property, a special role, e.g. @ic{hasAge}")
    (:VERSION-STRING "A version string as returned by @ic{(get-product-version)}, e.g. @ic{\"2.0\"}")
    (:RACER-EXPRESSION "A Racer expression")
    (:DISJOINTNESS-GROUP-ID "A symbol")
    (:EVENT-RULE-HEAD "An event rule head (the rule consequence)")
    (:OWLAPI-OTHER-ID "An OWLAPI ID, a symbol or non-negative integer")
    (:PLUGIN-ID "An ID string, e.g. @ic{\"1.0\"}")
    (:OWLAPI-AXIOM-ID "An OWLAPI axiom ID, a non-negative integer, for identifying OWLAPI axioms")
    (:KEYWORD "A keyword symbol")
    (:RULE-LEFTHAND-SIDE "A rule head, the rule consequence, e.g. @ic{(?x mother)}")
    (:NRQL-DEFINED-QUERY-NAME "A symbol, the name of a nRQL defined query")
    (:MINILISP-OBJECT-NAME "A MiniLisp identifier, e.g. a MiniLisp variable or function name")
    (:RACER-PARAMETER "unknown")
    (:CD-ATTRIBUTE "A concrete domain attribute, e.g. @ic{age}")
    (:TRIPLESTORE-NAME "A triple-store name, a symbol")
    (:PREFIX-STRING "A namespace prefix string, e.g. @ic{\"rdfs\"} or @ic{\"family\"}")
    (:OWL-DATAVALUE "An OWL2 data value, e.g. 24 or @ic{\"betty\"}")
    (:CD-OBJECT "A concrete domain object, e.g. @ic{betty-age}")
    (:OWLAPI-ONTOLOGY-NAME "An OWLAPI2 ontology name, a TBox / ABox name")
    (:TBOX-NAME "A TBox name, e.g. @ic{family}")
    (:DIRECTORY "A directory stringm e.g. @ic{\"/home/mi.wessel/test/\"}")
    (:IP-PORT "A TCP-IP port, e.g. @ic{8080}")
    (:ABOX-ASSERTION "An ABox assertion, e.g. @ic{(instance betty woman)}") 
    (:IP-ADDRESS "A TCP-IP IP address as a string, e.g. @ic{\"192.168.0.1\"}")
    (:NRQL-QUERY-ID "A nRQL query ID, a non-negative-integer or symbol, e.g. 1")
    (:TIMENET-QUERY "unknown")
    (:CONCEPT-NAME "A concept name, e.g. @ic{woman}")
    (:RACER-SUBSTRATE-DESCRIPTION-LABEL "A substrate label description, e.g. @ic{(\"michael\" age 34)}")
    (:OWLAPI-REASONER-NAME "An OWLAPI reasoner name, a TBox / ABox name, e.g. @ic{family}")
    (:NRQL-QUERY-HEAD "A nRQL query head, e.g. @ic{(?x (told-value (age ?x)))}")
    (:RULE-RIGHTHAND-SIDE "A rule body, the antecedence, e.g. @ic{(and (?x woman) (?x ?y has-child))}")
    (:ANNOTATION-ROLE "An OWL2 annotation property, a special role, e.g. @ic{rdfs:comment}")
    (:NRQL-RULE-CONSEQUENCE "A nRQL rule consequence, a generalized ABox, e.g. @ic{((instance ?x mother) (?X ?y has-child))}")
    (:OBJECT-PROPERTY "An OWL2 object property, e.g. @ic{family:hasChild}")
    (:SPARQL-QUERY-STRING "A SPARQL query string")
    (:ABOX-INDIVIDUAL "An ABox individual, e.g. @ic{betty}")
    (:SYMBOL "A symbol")
    (:STRING "A string, e.g. @ic{\"hello\"}")
    (:SUBSTRATE-NODE "A substrate node, a symbol, e.g. @ic{person1}")
    (:NON-NEGATIVE-INTEGER "A non-negative integer (possibly 0)")
    (:ABOX-NAME "The name of an ABox, e.g. @ic{family-abox}")
    (:SUBSTRATE-NAME "The name of a substrate, e.g. @ic{geo-example-rcc}")
    (:ROLE "A role, e.g. @ic{has-child}")
    (:CONCEPT-EXPRESSION "A Racer concept expression, e.g. @ic{(and (some has-child person) woman)}")
    (:DATATYPE "An OWL2 datatype description, e.g. @ic{(d-base-type |http://www.w3.org/2001/XMLSchema#integer|)}")
    (:CD-TYPE "A concrete domain base type, e.g. @ic{integer, racer-boolean, string, real}")
    (:OWLAPI-AXIOM-TYPE "An OWLAPI axiom type, e.g. @ic{OWLClassAssertionAxiom, OWLSubClassAxiom,} etc.")
    (:URI "A URI string, e.g. @ic{\"http://www.example-ontology.com#person\"}")
    (:FILENAME "A filename string, e.g. @ic{\"/home/mi.wessel/ontology.racer\"}")
    (:URL "A URL string, e.g. @ic{\"http://www.example-ontology.com\"}")
    (:OWL-DATA-LITERAL "An OWL2 data literal / value, e.g. @ic{(d-literal \"Michael\" (d-base-type http://www.w3.org/2001/XMLSchema#string))}")
    (:MINILISP-SEXPRESSION "A MiniLisp S-expression")
    (:VALUE "A Racer parameter value")
    (:CD-VALUE "A concrete domain value, e.g. @ic{\"betty\"} or 35")
    (:BOOLEAN "A boolean: @ic{t} (= true) or @ic{nil} (= false)")
    (:OWLAPI-EXPRESSION "An OWLAPI expression, e.g. an OWLAPI axiom constructor call, and OWLAPI entity description, etc.")
    (:CD-LITERAL "A concrete domain literal, e.g., @ic{\"Betty\"} or @ic{80}")
    (:ROLE-TERM "A role name or inverse role name, e.g. the symbol @ic{R} or the expression @ic{(inv R)}")
    (:NRQL-RULE-STATUS "A nRQL rule status description, e.g. @ic{:READY-TO-RUN} or @ic{(:ACCURATE :PROCESSED)}")
    (:NRQL-QUERY-DESCRIPTION "A nRQL query description, e.g. @ic{(:QUERY-1 (:ACCURATE :PROCESSED) (RETRIEVE (?X) (?X C) :ABOX TEST))}")
    (:NRQL-RULE-DESCRIPTION "A nRQL rule description, e.g. @ic{(:RULE-2 (:NOT-ACCURATE :PROCESSED) (PREPARE-ABOX-RULE (?X C) ((INSTANCE ?X C)) :ABOX TEST))}")
    (:SUBSTRATE-EDGE-DESCRIPTION "A substrate edge description, e.g. @ic{(((:FROM-NODE MICHAEL) (:TO-NODE RUDOLF) (:EDGE-LABEL ((HAS-FATHER)))))}")
    (:ROLE-HIEARCHY "A nested list representing the role hierarchy, e.g.
@pc{(((HAS-MOTHER) ((HAS-PARENT)) ((*BOTTOM-OBJECT-ROLE*))) ((HAS-PARENT) ((*TOP-OBJECT-ROLE*)) ((HAS-FATHER) (HAS-MOTHER))) ((HAS-FATHER) ((HAS-PARENT)) ((*BOTTOM-OBJECT-ROLE*))) ((*TOP-OBJECT-ROLE*) ((*TOP-OBJECT-ROLE*)) ((HAS-PARENT))) ((*BOTTOM-OBJECT-ROLE*) ((HAS-MOTHER) (HAS-FATHER)) ((*BOTTOM-OBJECT-ROLE*))) ((*TOP-DATATYPE-ROLE*) ((*TOP-OBJECT-ROLE*)) ((*BOTTOM-DATATYPE-ROLE*) (RACER-INTERNAL%HAS-INTEGER-VALUE) (RACER-INTERNAL%HAS-REAL-VALUE) (RACER-INTERNAL%HAS-STRING-VALUE) (RACER-INTERNAL%HAS-BOOLEAN-VALUE))) ((*BOTTOM-DATATYPE-ROLE*) ((*TOP-DATATYPE-ROLE*) (RACER-INTERNAL%HAS-INTEGER-VALUE) (RACER-INTERNAL%HAS-REAL-VALUE) (RACER-INTERNAL%HAS-STRING-VALUE) (RACER-INTERNAL%HAS-BOOLEAN-VALUE)) ((*BOTTOM-OBJECT-ROLE*))))}")
    (:PATCH-DESCRIPTION "A Racer plugin description")
    (:OWLAPI-ONTOLOGY-DESCRIPTION "A description of the OWLAPI parameters of the OWLAPI ontology, a nested list expression")
    (:OWLAPI-REASONER-DESCRIPTION "A description of the OWLAPI reasoner and its ontologies, a nested list expression")
    (:OWLAPI-CHANGE "An OWLAPI change object, represented by its constructor call")
    (:OWLAPI-RASONER-NAME "A symbol, the name of the OWLAPI reasoner, corresponding to a TBox / ABox name")
    (:NRQL-TBOX-QUERY-ANSWER "A nRQL TBox query answer, e.g. @ic{(((?X D)) ((?X C)) ((?X TOP)) ((?X *TOP*)))}")
    (:NRQL-RULE-ID-AND-STATUS "A nRQL rule id plus status description, e.g. @ic{(:RULE-1 :READY-TO-RUN)}")
    (:NRQL-QUERY-ID-AND-STATUS "A nRQL query id plus status description, e.g. @ic{(:QUERY-1 :READY-TO-RUN)}")
    (:NRQL-RULE-RESULT "An anonymous ABox, e.g. @ic{(((INSTANCE I MOTHER) (RELATED I J HAS-CHILD) (INSTANCE J CHILD)))}")
    (:NRQL-RULE-ID "A nRQL rule ID, e.g. @ic{:rule-1}")
    (:ATTRIBUTE-ASSERTION "A concrete domain attribute assertion, e.g. @ic{(constrained betty betty-age age)}")
    (:ABOX-SIGNATURE "An ABox signature, a nested list")
    (:KB-SIGNATURE "A KB signature, a nested list")
    (:SEXPRESSION "A Lisp S-expression") 
    (:NRQL-QUERY-ANSWER "A nRQL query answer, e.g. @ic{(((?X I) (?Y J)) ((?X I) (?Y I)) ((?X A) (?Y B)))}")
    (:NRQL-ABDUCTIVE-QUERY-ANSWER "A nRQL abductive query answer, e.g. 
@pc{(T (((:TUPLE (?X IND-2)) (:NEW-INDS IND-2) (:HYPOTHESIZED-ASSERTIONS (INSTANCE IND-2 C)))))}")
    (:TAXONOMY "A taxonomy, a nested list structure, e.g. @pc{((TOP NIL (PERSON)) (FATHER (PERSON) (BOTTOM)) (MOTHER (PERSON) (BOTTOM)) (PERSON (TOP) (FATHER MOTHER)) (BOTTOM (FATHER MOTHER) NIL))}")
    (:TIMENET-QUERY-RESULT "A timenet query result")

    (:NRQL-ANSWER-TUPLE "A nRQL answer tuple providing bindings to nRQL query variables, e.g. @ic{((?X I) (?Y J))}")
    (:DEFQUERY-EXPRESSION "A defined query defquery expression, e.g. @ic{((DEFQUERY MOTHER (?X) (AND (?X WOMAN) (?X ?Y HAS-CHILD))))}")
    (:SUBSTRATE-NODE-DESCRIPTION "A data substrate node description, e.g. @ic{((:NODE-NAME MICHAEL) (:NODE-LABEL ((PERSON))) (:NODE-SUCCESSORS NIL) (:NODE-PREDECESSORS NIL))}")
    (:NRQL-QUERY-PROCESSING-MODE "A description of the parameters and settings of the nRQL query answering engine, a nested attribute-value list")
    (:SUBSTRATE-DESCRIPTION "A data substrate description, a nested attribute-value list")
    (:MINILISP-DEFINE-EXPRESSION "A MiniLisp function definition expression, e.g. @ic{(DEFINE TWICE (X) (PROGN (+ X X)))}")
    (:MINILISP-DEFCON-EXPRESSION "A MiniLisp parameter definition expression, e.g. @ic{(DEFCON SENSE-OF-LIFE 42)}")
    (:NRQL-QUERY-STATUS "A nRQL query status description, e.g. @ic{:READY-TO-RUN, (:NOT-ACCURATE :READY-TO-RUN), :PROCESSED,} etc.")))

(defun get-type-description (type)
  (if (or (listp type)
          (and (symbolp type)
               (not 
                (keywordp type))))
      (if (null type)
          (format nil "The symbol nil.")
        (ecase (first type)
          (:list
           (cond ((cddr type) ; more than one? 
                  (format nil "A list of length ~S with objects of the following types: ~{~a~^, ~}" 
                          (length (cdr type))
                          (mapcar #'get-type-description (cdr type))))
                 (t (format nil "A list of objects of the following type: ~A" 
                            (get-type-description (cadr type))))))
          (:dotted-pair
           (format nil "A dotted pair with objects of the following types: ~{~a~^, ~}" 
                   (mapcar #'get-type-description (cddr type))))
          (:or 
           (format nil "One of the following:  ~{~a~^, ~}" 
                   (mapcar #'get-type-description (cdr type))))
          (:one-of 
           (if (cddr type)
             (concatenate 'string
                          (format nil "One of the following symbols: @ic{")
                          (format nil "~{~s~^, ~}"
                                  (cdr type))
                          (format nil "}"))
             (format nil "The symbol ~S" 
                     (second type))))))
    (if (eq type :ignore)
        ""
      (or
       (second (assoc type +type-explanations+))
       (progn
         (format t "~%*** WARNING - I HAVE NO DESCRIPTION FOR TYPE ~S!" type)
         (pushnew type *missing-type-descriptions*)
         "What is this?!")))))

;;;
;;;
;;; 

(defconstant +super-sections+
  '((reference-toplevel "Racer Reference Manual")

    ;;;
    ;;;
    ;;; 
    
    (kb-toplevel "Knowledge Bases" (reference-toplevel 1))

    (:KB-MANAGEMENT "General Management Functions" kb-toplevel)

    (abox-toplevel "The ABox" (kb-toplevel 3))

    (tbox-toplevel "The TBox" (kb-toplevel 2))

    (nrql-toplevel "The new Racer Query Language (nRQL)" (reference-toplevel 5))

    (owlapi-toplevel "The Racer OWLAPI (NOSA)" (reference-toplevel 6))

    ;;;
    ;;;
    ;;;

    (:GENERAL "General Functions" (reference-toplevel 2))

    (:DIG "The DIG Interface" (reference-toplevel 8))

    (:OWLLINK "The OWLlink Interface" (reference-toplevel 7))

    (:MINILISP "Server-Scripting with MiniLisp" (reference-toplevel 9))
    
    (:EXPLANATIONS "The Explanation Facility" (reference-toplevel 10))

    (:PUBLISH-SUBSCRIBE "The Publish-Subscribe Facility" (reference-toplevel 11))

    (:DOC "The Online Documentation Facility" (reference-toplevel 12))

    (:TESTING "Functions for Benchmarking and Testing" (reference-toplevel 13))

    (:EVENTS "The Timenet Event Recognition Facility" (reference-toplevel 14))
        
    (:UPDATES "Updating Racer" (reference-toplevel 20))

    (:PLUGINS "Racer Plugins"  (reference-toplevel 16))

    (:PATCHES "Racer Patches"  (reference-toplevel 17))

    ;;;
    ;;;
    ;;; 

    (:ABOX-MANAGEMENT "General Management Functions" (abox-toplevel 1))

    (:ABOX-TELL "Tell Statements" (abox-toplevel 2))
    (:ABOX-OWL-TELL "OWL-Related Tell Statements" (:abox-tell 3))
    (:ABOX-CD-TELL "Concrete Domain-Related Tell Statements" (:abox-tell 2))

    (:ABOX-FORGET "Retraction" (abox-toplevel 3))
    (:ABOX-OWL-FORGET "OWL-Related Retraction" (:abox-forget 3))
    (:ABOX-CD-FORGET  "Concrete Domain-Related Retraction" (:abox-forget 2))

    (:ABOX-ASK "Basic Queries" (abox-toplevel 4))
    (:ABOX-OWL-ASK "OWL-Related Basic Queries" (:abox-ask 3))
    (:ABOX-CD-ASK "Concrete Domain-Related Basic Queries" (:abox-ask 2))

    (:ABOX-DIFF "ABox Difference" abduction (abox-toplevel 5))

    ;;;
    ;;;
    ;;; 
    
    (:TBOX-MANAGEMENT "General Management Functions" (tbox-toplevel 1))

    (:TBOX-TELL "Tell Statements" (tbox-toplevel 2))
    (:TBOX-OWL-TELL "OWL-Related Tell Statements" (:tbox-tell 3))
    (:TBOX-CD-TELL "Concrete Domain-Related Tell Statements" (:tbox-tell 2))

    (:TBOX-FORGET "Retraction" (tbox-toplevel 3))
    (:TBOX-OWL-FORGET "OWL-Related Retraction" (:tbox-forget 3))
    (:TBOX-CD-FORGET  "Concrete Domain-Related Retraction" (:tbox-forget 2))

    (:TBOX-ASK "Basic Queries" (tbox-toplevel 4))
    (:TBOX-OWL-ASK "OWL-Related Basic Queries" (:tbox-ask 3))
    (:TBOX-CD-ASK "Concrete Domain-Related Basic Queries" (:tbox-ask 2))

    ;;;
    ;;;
    ;;;

    (:IO "Input/Output Functions" (reference-toplevel 4))
    (:PERSISTENCE "Preserving Server States via Images" (:io 2))

    ;;;
    ;;;
    ;;; 

    (:OWL-INTERFACE "The OWL Interface" (reference-toplevel 3))
    (:SWRL "The Semantic Web Rule Language (SWRL)" (:OWL-INTERFACE 1))
    (:AGRAPH "The AllegroGraph TripleStore Interface" (:OWL-INTERFACE 2))
    (:SPARQL "The SPARQL Query Language Interface" :AGRAPH)

    ;;;
    ;;;
    ;;;

    (:OWLAPI-MANAGEMENT "General Management Functions" (owlapi-toplevel 1))
    (:OWLAPI-TELL "Axiom Constructors" (owlapi-toplevel 2))
    (:OWLAPI-ASK "Basic OWLAPI Queries" (owlapi-toplevel 3))

    ;;;
    ;;;
    ;;; 

    (:nrql-abox "nRQL Queries" (nrql-toplevel 1))

    (:nrql-tbox "nRQL TBox Queries" (nrql-toplevel 3))

    (:ABDUCTION "Abductive Queries" (:nrql-abox 5))

    (:QUERY-REASONING "Query Inference" (nrql-toplevel 6))

    (:QUERY-OPTIMIZER "The Query Optimizer" (nrql-toplevel 5))

    (:DEFINED-QUERIES "Defined Queries" (nrql-toplevel 4))

    (:RULE-MANAGEMENT "nRQL Rules" (nrql-toplevel 2))

    ;;;
    ;;;
    ;;; 

    (:SUBSTRATE-MANAGEMENT "The Substrate Layer" (nrql-toplevel 7))

    (:DATA-SUBSTRATE "The Data Substrate" (:SUBSTATE-MANAGEMENT 1))

    (:RCC-SUBSTRATE "The RCC Substrate" (:SUBSTATE-MANAGEMENT 2))))

(defun get-supersections (section)
  (cdr (assoc section +super-sections+)))

(defun get-arg-description (x in)
  (let ((found (assoc x *all-args*)))
    (if found
        (pushnew in (second found))
      (pushnew (list x (list in)) *all-args*)))

  (let ((type
         (second
          (find-if #'(lambda (xx) 
                       (and (eq (first xx) x)
                            (member in (cddr xx))))
                   +known-args-and-types+))))

    (cond ((eq type :ignore)
           (progn 
             (nrql-warning "In get-arg-description ~A ~A: Ignoring argument ~A" x in x)
             :ignore))
          ((and (consp type) (eq (car type) :goto))
           (get-arg-description x (second type)))
          (t
           (let ((type 
                  (or type
                      (progn 
                        ;; (setf *x* (list x in))
                        (nrql-warning "*** In get-arg-description ~S ~S: I have no information about the type of argument ~A" x in x)
                        (pushnew (list x in) *missing-argument-types* :test #'equalp)
                        :unknown))))

             (list 
              ;; arg 
              x
              ;; type
              type 
              ;; description 
              (get-type-description type)))))))

(defun get-sections (x)
  (let ((cat (cdr (assoc x +category-for-function+))))
    (if (equal cat '(:unknown))
        '(all-functions unclassified)
      (cons 'all-functions
            (mapcar #'(lambda (x) 
                        (if (consp x)
                            (list (intern (symbol-name (first x)))
                                  (second x))
                          (intern (symbol-name x))))
                    cat)))))

(defun compactify-string (string)
  (let ((chars (coerce string 'list))
        (new nil)
        (last-was-whitespace-p nil))
    (dolist (char chars)
      (case char
        ((#\newline #\return #\tab #\space)
         (setf last-was-whitespace-p t))
        (otherwise
         (when last-was-whitespace-p 
           (push #\space new))
         (setf last-was-whitespace-p nil)
         (push char new))))
    (ts:string-substitute
     (coerce (nreverse new) 'string)
     '(("\\$" "$" nil)))))

(defun transform-tex-markup (string)
  (when (stringp string)

    (setf string (compactify-string string))

    (dolist (repl *tex-rewrite*)
      (setf string 
            (regexp-replace string (first repl) (second repl) (third repl) (fourth repl)))))
  string)

(defun nrql-function-lookup-arg (function arg)
  (let ((all
         (append 
          (remove-if-not #'(lambda (x) 
                             (eq (first x) function))
                         ts::*nrql-functions*)
          (remove-if-not #'(lambda (x) 
                             (eq (first x) function))
                         ts::*nrql-methods*))))
    (assoc arg 
           (third 
            (find-if (lambda (x) 
                       (cdr (assoc arg (third x))))
                     all)))))

(defun get-macro-for-function (fn)
  (declare (ignorable fn))
  (let ((name (symbol-name fn)))
    (cond ((is-predicate-p fn)
           (let ((res
                  (find (concatenate 'string (subseq name 0 (- (length name) 2))
                                     "?")
                        +cur-server-functions+
                        :test #'(lambda (x y)
                                  (string-equal x (symbol-name y))))))
             (or res
                 (progn 
                   (pushnew fn *unknown-macros-for-functions*)
                   :unknown))))
          (t 
           (let ((res 
                  (find fn +function-for-macro+ :key #'second)))
             ;; assume this list is complete: 
             (if res
                 (first res)
               (progn 
                 ;; (pushnew fn *unknown-macros-for-functions*)
                 ;; :unknown
                 nil)))))))

(defconstant +function-for-macro+ 
  '((ALL-DIFFERENT add-all-different-assertion)
    (ASYMMETRIC role-is-asymmetric)
    (ATTRIBUTE-DOMAIN atomic-role-domain)
    (ATTRIBUTE-FILLER set-attribute-filler)
    (CLONE-ABOX create-abox-clone)
    (CLONE-TBOX create-tbox-clone)
    (CONCEPT-ANCESTORS atomic-concept-ancestors)
    (CONCEPT-CHILDREN atomic-concept-children)
    (CONCEPT-DESCENDANTS atomic-concept-descendants)
    (CONCEPT-INSTANCES retrieve-concept-instances)
    (CONCEPT-PARENTS atomic-concept-parents)
    (CONCEPT-SYNONYMS atomic-concept-synonyms)
    (CONSTRAINED add-attribute-assertion)
    (CONSTRAINTS add-constraint-assertion)
    (DATATYPE-ROLE-FILLER add-datatype-role-filler)
    (define-ABOX define-abox-1)
    (DEFINE-CONCEPT add-concept-axiom)
    (DEFINE-CONCRETE-DOMAIN-ATTRIBUTE defcdattribute)
    (DEFINE-DATATYPE-PROPERTY add-datatype-property)
    (DEFINE-DISJOINT-PRIMITIVE-CONCEPT :none)
    (DEFINE-DISTINCT-INDIVIDUAL add-individual)
    (DEFINE-EVENT-ASSERTION add-event-assertion)
    (DEFINE-EVENT-RULE add-event-rule)
    (DEFINE-INDIVIDUAL add-individual)
    (DEFINE-PREFIX add-prefix)
    (DEFINE-PRIMITIVE-ATTRIBUTE defprimattribute)
    (DEFINE-PRIMITIVE-CONCEPT add-concept-axiom)
    (DEFINE-PRIMITIVE-ROLE :none)
    (DEFINE-RULE add-rule-axiom)
    (DEFINE-TBOX define-tbox-1)
    (DELETE-ABOX forget-abox)
    (DELETE-TBOX forget-tbox)
    (DIFFERENT-FROM add-different-from-assertion)
    (DIRECT-PREDECESSORS retrieve-direct-predecessors)
    (DISJOINT add-disjointness-axiom)
    (DOMAIN role-has-domain)
    (EQUIVALENT add-concept-axiom)
    (FORGET forget-statement)
    (FUNCTIONAL role-is-functional)
    (GET-CONCEPT-DEFINITION get-concept-definition-1)
    (GET-CONCEPT-NEGATED-DEFINITION get-concept-negated-definition-1)
    (IMPLIES add-concept-axiom)
    (IMPLIES-ROLE role-has-parent)
    (IN-ABOX in-abox-internal)
    (IN-KNOWLEDGE-BASE :none)
    (IN-TBOX in-tbox-internal)
    (INDIVIDUAL-ANTONYMS retrieve-individual-antonyms)
    (INDIVIDUAL-ATTRIBUTE-FILLERS retrieve-individual-attribute-fillers)
    (INDIVIDUAL-DIRECT-TYPES most-specific-instantiators)
    (INDIVIDUAL-FILLED-ROLES retrieve-individual-filled-roles)
    (INDIVIDUAL-FILLERS retrieve-individual-fillers)
    (INDIVIDUAL-SYNONYMS retrieve-individual-synonyms)
    (INDIVIDUAL-TOLD-ATTRIBUTE-VALUE retrieve-individual-told-attribute-value)
    (INDIVIDUAL-TOLD-DATATYPE-FILLERS retrieve-individual-told-datatype-fillers)
    (INDIVIDUAL-TYPES instantiators)
    (INIT-PUBLICATIONS init-publications1)
    (INIT-SUBSCRIPTIONS init-subscriptions1)
    (INSTANCE add-concept-assertion)
    (INVERSE inverse-of-role)
    (IRREFLEXIVE role-is-irreflexive)
    (PRETRIEVE pracer-answer-query)
    (PUBLISH publish-1)
    (RANGE role-has-range)
    (REFLEXIVE role-is-reflexive)
    (RELATED add-role-assertion)
    (RELATED-INDIVIDUALS retrieve-related-individuals)
    (ROLE-ANCESTORS atomic-role-ancestors)
    (ROLE-CHILDREN atomic-role-children)
    (ROLE-DESCENDANTS atomic-role-descendants)
    (ROLE-DOMAIN atomic-role-domain)
    (ROLE-INVERSE atomic-role-inverse)
    (ROLE-PARENTS atomic-role-parents)
    (ROLE-RANGE atomic-role-range)
    (ROLE-SYNONYMS atomic-role-synonyms)
    (ROLES-DISJOINT role-disjoint-1)
    (ROLES-EQUIVALENT roles-equivalent-1)
    (SAME-AS add-same-individual-as-assertion)
    (SAME-INDIVIDUAL-AS add-same-individual-as-assertion)
    (SIGNATURE ensure-abox-signature ensure-tbox-signature)
    (SPARQL-ANSWER-QUERY :none)
    (SPARQL-RETRIEVE :none)
    (STATE :none)
    (SUBSCRIBE subscribe-1)
    (SYMMETRIC role-is-symmetric)
    (TIME :none)
    (TIMENET-RETRIEVE timenet-answer-query)
    (TRANSITIVE role-is-transitive)
    (UNPUBLISH unpublish-1)
    (UNRELATED add-negated-role-assertion)
    (UNSUBSCRIBE unsubscribe-1)))

(defun get-function-for-macro (fn)
  (declare (ignorable fn))
  (let ((name (symbol-name fn)))
    (cond ((is-predicate-p fn)
           (let ((res
                  (find (concatenate 'string (subseq name 0 (1- (length name)))
                                     "-p")
                        +cur-server-functions+
                        :test #'(lambda (x y)
                                  (string-equal x (symbol-name y))))))
             (or res
                 (progn 
                   (pushnew fn *unknown-functions-for-macros*)
                   :unknown))))
          (t 
           (or (second (assoc fn +function-for-macro+))
               (progn 
                 (pushnew fn *unknown-functions-for-macros*)
                 :unknown))))))

(defun is-predicate-p (fn)
  (let ((name (symbol-name fn)))
    (or (ends-with-p "?" name)
        (ends-with-p "-p" name)
        (ends-with-p "-P" name))))

(defconstant +return-type-for-function+
  '((ABORT-ALL-RULES (:one-of :okay-all-rules-aborted))
    (ABORT-RULE (:one-of :okay-rule-aborted :not-found))
    (ACCURATE-QUERIES (:list :nrql-query-id))
    (ACCURATE-RULES (:list :nrql-rule-id))
    (ACTIVATE-DEFINED-QUERY (:one-of :okay))
    (ACTIVE-QUERIES (:list :nrql-query-id))
    (ACTIVE-RULES (:list :nrql-rule-id))
    (ADD-DOC-ENTRY1 :ignore)
    (ADD-DOC-IMAGE-DATA-FROM-FILE1 :ignore)
    (ADD-DOC-IMAGE-DATA1 :ignore)
    (ADD-DOC-IMAGE-FILE1 :ignore)
    (ADD-DOC-PHRASE1 :ignore)
    (ADD-EVENT-ASSERTION :ignore)
    (ADD-EVENT-RULE :ignore)
    (ADD-EXPLANATION-ASSERTIONS :ignore)
    (ADD-INDIVIDUAL :ignore)
    (ADD-MISSING-TOP-CONJUNCTS :ignore)
    (ADD-PREFIX :ignore)
    (ADD-RULE-AXIOM :ignore)
    (ALC-CONCEPT-COHERENT :boolean)
    (ALL-ABOXES (:list :abox-name))
    (ALL-ANNOTATION-CONCEPT-ASSERTIONS (:list :concept-assertion))
    (ALL-ANNOTATION-ROLE-ASSERTIONS (:list :role-assertion))
    (ALL-ATOMIC-CONCEPTS (:list :concept-name))
    (ALL-ATTRIBUTE-ASSERTIONS (:list :attribute-assertion))
    (ALL-ATTRIBUTES (:list :cd-attribute))
    (ALL-CONCEPT-ASSERTIONS (:list :concept-assertion))
    (ALL-CONCEPT-ASSERTIONS-FOR-INDIVIDUAL (:list :concept-assertion))
    (ALL-CONSTRAINTS (:list :cd-constraint-expression))
    (ALL-DIFFERENT-FROM-ASSERTIONS (:list :different-from-assertion))
    (ALL-EQUIVALENT-CONCEPTS (:list (:list :concept-name)))
    (ALL-FEATURES (:list :role))
    (ALL-INDIVIDUALS (:list :abox-individual))
    (ALL-QUERIES (:list :nrql-query-id))
    (ALL-ROLE-ASSERTIONS (:list :role-assertion))
    (ALL-ROLE-ASSERTIONS-FOR-INDIVIDUAL-IN-DOMAIN (:list :role-assertion))
    (ALL-ROLE-ASSERTIONS-FOR-INDIVIDUAL-IN-RANGE (:list :role-assertion))
    (ALL-ROLES (:list :role))
    (ALL-RULES (:list :nrql-rule-id))
    (ALL-SAME-AS-ASSERTIONS (:list :same-as-assertion))
    (ALL-TBOXES (:list :tbox-name))
    (ALL-TRANSITIVE-ROLES (:list :role))
    (ALLOW-OVERLOADED-DEFINITIONS (:one-of :OKAY-ALLOWING-OVERLOADED-DEFINITIONS))
    (ANSWER-SEQUENCE :ignore)
    (APPLICABLE-RULES (:list :nrql-rule-id))
    (ASSOCIATED-ABOXES (:list :abox-name))
    (ASSOCIATED-TBOX (:list :tbox-name))
    (ATOMIC-CONCEPT-ANCESTORS (:list (:list :concept-name)))
    (ATOMIC-CONCEPT-CHILDREN (:list (:list :concept-name)))
    (ATOMIC-CONCEPT-DESCENDANTS (:list (:list :concept-name)))
    (ATOMIC-CONCEPT-PARENTS (:list (:list :concept-name)))
    (ATOMIC-CONCEPT-SYNONYMS (:list :concept-name))
    (ATOMIC-ROLE-ANCESTORS (:list :role))
    (ATOMIC-ROLE-CHILDREN (:list :role))
    (ATOMIC-ROLE-DESCENDANTS (:list :role))
    (ATOMIC-ROLE-DOMAIN :concept-expression)
    (ATOMIC-ROLE-INVERSE :role-term)
    (ATOMIC-ROLE-PARENTS (:list :role))
    (ATOMIC-ROLE-RANGE :concept-expression)
    (ATOMIC-ROLE-SYNONYMS (:list :role))
    (ATTRIBUTE-DOMAIN :concept-expression)
    (ATTRIBUTE-DOMAIN-1 :concept-expression)
    (ATTRIBUTE-TYPE :cd-type)
    (CHEAP-QUERIES (:list :nrql-query-id))
    (CHEAP-RULES (:list :nrql-rule-id))
    (CHECK-ABOX-COHERENCE (:list :boolean (:list :abox-assertion)))
    (CHECK-CONCEPT-COHERENCE (:list (:list :concept-name) (:list :role)))
    (CHECK-FOR-UPDATES :ignore)
    (CHECK-NRQL-SUBSCRIPTIONS (:list))
    (CHECK-ONTOLOGY :ignore)
    (CHECK-SUBSCRIPTIONS (:list))
    (CHECK-TBOX-COHERENCE (:list (:list :concept-name) (:list :role)))
    (CLASSIFY-TBOX :ignore)
    (CLEAR-ALL-DOCUMENTATION :ignore)
    (CLEAR-DEFAULT-TBOX :ignore)
    (CLONE-ABOX :abox-name)
    (CLONE-TBOX :tbox-name)
    (CLOSE-TRIPLE-STORE :ignore)
    (COMPUTE-ABOX-DIFFERENCE1 (:list :abox))
    (COMPUTE-ABOX-DIFFERENCE2 (:list :abox))
    (COMPUTE-ALL-IMPLICIT-ROLE-FILLERS :ignore)
    (COMPUTE-IMPLICIT-ROLE-FILLERS :ignore)
    (COMPUTE-INDEX-FOR-INSTANCE-RETRIEVAL :ignore)
    (COMPUTE-SUBGRAPH-ABOXES (:list (:list :abox (:list :abox-individual))))
    (CONCEPT-ANCESTORS (:list (:list :concept-name)))
    (CONCEPT-CHILDREN (:list (:list :concept-name)))
    (CONCEPT-DESCENDANTS (:list (:list :concept-name)))
    (CONCEPT-INSTANCES (:list :abox-individual))
    (CONCEPT-PARENTS (:list (:list :concept-name)))
    (CONCEPT-SYNONYMS (:list (:list :concept-name)))
    (CONVERT-EVENT-SPECS :ignore)
    (COPY-RULES :ignore)
    (CREATE-ABOX-CLONE :abox-name)
    (CREATE-SUBGRAPH-ABOXES :ignore)
    (CREATE-TBOX-CLONE :tbox-name)
    (CREATE-TBOX-INTERNAL-MARKER-CONCEPT :concept-name)
    (CREATE-TRIPLE-STORE :ignore)
    (CURRENT-ABOX :abox-name)
    (CURRENT-TBOX :abox-name)
    (DATA-EDGE1 :ignore)
    (DATA-NODE1 :ignore)
    (DEACTIVATE-DEFINED-QUERY (:one-of :okay))
    (DECLARE-CURRENT-KNOWLEDGE-BASES-AS-PERSISTENT :ignore)
    (DEFCDATTRIBUTE :ignore)
    (DEFINE-ABOX :ignore)
    (DEFINE-ABOX-1 :ignore)
    (DEFINE-AND-EXECUTE-QUERY :ignore)
    (DEFINE-AND-PREPARE-QUERY :ignore)
    (DEFINE-EVENT-ASSERTION :ignore)
    (DEFINE-EVENT-RULE :ignore)
    (DEFINE-PREFIX :ignore)
    (DEFINE-RULE :ignore)
    (DEFINE-TBOX :ignore)
    (DEFINE-TBOX-1 :ignore)
    (DEFPRIMATTRIBUTE :ignore)
    (DEL-DATA-EDGE1 :ignore)
    (DEL-DATA-NODE1 :ignore)
    (DEL-DOC-ENTRY1 :ignore)
    (DELETE-ABOX :ignore)
    (DELETE-ALL-ABOXES :ignore)
    (DELETE-ALL-TBOXES :ignore)
    (DELETE-PREFIX-MAPPINGS :ignore)
    (DELETE-RCC-SYNONYMS :ignore)
    (DELETE-TBOX :ignore)
    (DESCRIBE-ABOX :ignore)
    (DESCRIBE-ALL-EDGES (:list :substrate-edge-description))
    (DESCRIBE-ALL-QUERIES (:list :nrql-query-description))
    (DESCRIBE-ALL-RULES (:list :nrql-rule-description))
    (DESCRIBE-CONCEPT :ignore)
    (DESCRIBE-INDIVIDUAL :ignore)
    (DESCRIBE-INDIVIDUAL1 :ignore)
    (DESCRIBE-ROLE :ignore)
    (DESCRIBE-RULE (:or (:one-of :not-found) :nrql-rule-description))
    (DESCRIBE-RULE-STATUS (:or (:one-of :not-found) :nrql-rule-status))
    (DESCRIBE-TBOX :ignore)
    (DIG-READ-DOCUMENT :ignore)
    (DIG-READ-FILE :ignore)
    (DIRECT-PREDECESSORS (:list :abox-individual))
    (DISABLE-ABDUCTION :ignore)
    (DISABLE-ABOX-MIRRORING :ignore)
    (DISABLE-ALISP-COMPATIBILITY-MODE :ignore)
    (DISABLE-DATA-SUBSTRATE-MIRRORING :ignore)
    (DISABLE-DEFINED-QUERIES :ignore)
    (DISABLE-KB-HAS-CHANGED-WARNING-TOKENS :ignore)
    (DISABLE-LAZY-UNFOLDING-OF-DEFINED-QUERIES :ignore)
    (DISABLE-NRQL-WARNINGS :ignore)
    (DISABLE-PHASE-TWO-STARTS-WARNING-TOKENS :ignore)
    (DISABLE-QUERY-OPTIMIZATION :ignore)
    (DISABLE-QUERY-REALIZATION :ignore)
    (DISABLE-QUERY-REPOSITORY :ignore)
    (DISABLE-RCC-SUBSTRATE-MIRRORING :ignore)
    (DISABLE-TOLD-INFORMATION-QUERYING :ignore)
    (DISABLE-TWO-PHASE-QUERY-PROCESSING-MODE :ignore)
    (DONT-ADD-MISSING-TOP-CONJUNCTS :ignore)
    (DONT-ADD-ROLE-ASSERTIONS-FOR-DATATYPE-PROPERTIES :ignore)
    (DONT-ADD-RULE-CONSEQUENCES-AUTOMATICALLY :ignore)
    (DONT-ALLOW-OVERLOADED-DEFINITIONS :ignore)
    (DONT-CHECK-ABOX-CONSISTENCY-BEFORE-QUERYING :ignore)
    (DONT-KEEP-DEFINED-QUERY-ATOMS :ignore)
    (DONT-PREFER-DEFINED-QUERIES :ignore)
    (DONT-REPORT-INCONSISTENT-QUERIES-AND-RULES :ignore)
    (DONT-USE-INDIVIDUAL-SYNONYM-EQUIVALENCE-CLASSES :ignore)
    (DONT-USE-INJECTIVE-VARIABLES-BY-DEFAULT :ignore)
    (EDGE-DESCRIPTION1 :substrate-edge-description)
    (EDGE-LABEL1  :substrate-description-label)
    (ENABLE-ABDUCTION :ignore)
    (ENABLE-ALISP-COMPATIBILITY-MODE :ignore)
    (ENABLE-DATA-SUBSTRATE-MIRRORING :ignore)
    (ENABLE-DEFINED-QUERIES :ignore)
    (ENABLE-LAZY-TUPLE-COMPUTATION :ignore)
    (ENABLE-LAZY-UNFOLDING-OF-DEFINED-QUERIES :ignore)
    (ENABLE-OPTIMIZED-QUERY-PROCESSING :ignore)
    (ENABLE-QUERY-REALIZATION :ignore)
    (ENABLE-QUERY-REPOSITORY :ignore)
    (ENSURE-ABOX-SIGNATURE :ignore)
    (ENSURE-SMALL-TBOXES :ignore)
    (ENSURE-SUBSUMPTION-BASED-QUERY-ANSWERING :ignore)
    (ENSURE-TBOX-SIGNATURE :ignore)
    (EXECUTE-ALL-RULES (:list :nrql-rule-result))
    (EXECUTE-OR-REEXECUTE-ALL-RULES (:list :nrql-rule-result))
    (EXECUTE-OR-REEXECUTE-QUERY (:or :nrql-query-answer (:one-of :not-found)))
    (EXECUTE-OR-REEXECUTE-RULE  (:or :nrql-rule-result (:one-of :not-found)))
    (EXIT-SERVER :ignore)
    (EXPENSIVE-QUERIES (:list :nrql-query-id))
    (EXPENSIVE-RULES (:list :nrql-rule-id))
    (FIND-ABOX :ignore)
    (FIND-TBOX :ignore)
    (FORGET :ignore)
    (FORGET-ABOX :ignore)
    (FORGET-ALL-DIFFERENT-ASSERTION :ignore)
    (FORGET-ANNOTATION-CONCEPT-ASSERTION :ignore)
    (FORGET-CONCEPT-ASSERTION :ignore)
    (FORGET-CONCEPT-AXIOM :ignore)
    (FORGET-CONSTRAINED-ASSERTION :ignore)
    (FORGET-CONSTRAINT :ignore)
    (FORGET-DATATYPE-ROLE-FILLER :ignore)
    (FORGET-DIFFERENT-FROM-ASSERTION :ignore)
    (FORGET-DISJOINTNESS-AXIOM :ignore)
    (FORGET-DISJOINTNESS-AXIOM-STATEMENT :ignore)
    (FORGET-INDIVIDUAL :ignore)
    (FORGET-NEGATED-ROLE-ASSERTION :ignore)
    (FORGET-NEGATIVE-DATATYPE-ROLE-FILLER :ignore)
    (FORGET-ROLE-ASSERTION :ignore)
    (FORGET-ROLE-AXIOMS :ignore)
    (FORGET-SAME-INDIVIDUAL-AS-ASSERTION :ignore)
    (FORGET-STATEMENT :ignore)
    (FORGET-TBOX :ignore)
    (FULL-RESET :ignore)
    (GET-ABOX-GRAPH :ignore)
    (GET-ABOX-LANGUAGE :string)
    (GET-ABOX-SIGNATURE :abox-signature)
    (GET-ABOX-VERSION :non-negative-integer)
    (GET-AGRAPH-VERSION :string)
    (GET-ALL-REMAINING-SETS-OF-RULE-CONSEQUENCES :nrql-rule-result)
    (GET-ALL-REMAINING-TUPLES :nrql-query-answer)
    (GET-ANSWER-SIZE :non-negative-integer)
    (GET-BUILD-VERSION :string)
    (GET-CONCEPT-DEFINITION :concept-expression)
    (GET-CONCEPT-DEFINITION-1 :concept-expression)
    (GET-CONCEPT-NEGATED-DEFINITION :concept-expression)
    (GET-CONCEPT-NEGATED-DEFINITION-1 :concept-expression)
    (GET-CONCEPT-PMODEL :ignore)
    (GET-CONCEPT-PROPERTIES (:list (:list :role (:list :concept-name))))
    (GET-DATA-BOTTOM-ROLE :role)
    (GET-EDGE-LABEL-FOR-NON-EXISTENT-EDGES :substrate-description-label)
    (GET-EXPLANATIONS :ignore)
    (GET-INDIVIDUAL-ANNOTATION-DATATYPE-FILLERS (:list (:list :annotation-datatype-property (:list :owl-data-literal))))
    (GET-INDIVIDUAL-ANNOTATION-FILLERS (:list (:list :annotation-property (:list :abox-individual))))
    (GET-INDIVIDUAL-DATATYPE-FILLERS (:list (:list :datatype-property (:list :owl-data-literal))))
    (GET-INDIVIDUAL-PMODEL :ignore)
    (GET-INDIVIDUAL-SUCCESSORS (:list (:list :role (:list :abox-individual))))
    (GET-INITIAL-SIZE-OF-PROCESS-POOL :non-negative-integer)
    (GET-KB-SIGNATURE :kb-signature)
    (GET-MAX-NO-OF-TUPLES-BOUND (:or nil :non-negative-integer))
    (GET-MAXIMUM (:list :abox))
    (GET-MAXIMUM-SIZE-OF-PROCESS-POOL :non-negative-integer)
    (GET-META-CONSTRAINT :concept-expression)
    (GET-MINIMUM (:list :abox))
    (GET-NAMESPACE-PREFIX :prefix-string)
    (GET-NAMESPACE-PREFIXES (:list (:list :prefix-string :url)))
    (GET-NEW-IND-COUNTER :non-negative-integer)
    (GET-NEW-IND-PREFIX :string)
    (GET-NEXT-N-REMAINING-SETS-OF-RULE-CONSEQUENCES (:list :abox))
    (GET-NEXT-N-REMAINING-TUPLES :nrql-query-answer)
    (GET-NRQL-VERSION :string)
    (GET-NUMBER-OF-EXPLANATIONS :non-negative-integer)
    (GET-OBJECT-BOTTOM-ROLE :role)
    (GET-PREFIXES (:goto get-namespace-prefixes))
    (GET-PROXY-SERVER :ip-address)
    (GET-RACER-VERSION :string)
    (GET-ROLE-DATATYPE :owl-datatype-expression)
    (GET-ROLE-HIERARCHY :role-hiearchy)
    (GET-SERVER-TIMEOUT (:or nil :non-negative-integer))
    (GET-SUBSTRATE-EDGES (:list (:list :substrate-node :substrate-node)))
    (GET-TBOX-LANGUAGE :string)
    (GET-TBOX-SIGNATURE :string)
    (GET-TBOX-VERSION :non-negative-integer)
    (IN-ABOX :ignore)
    (IN-ABOX-INTERNAL :ignore)
    (IN-KNOWLEDGE-BASE :ignore)
    (IN-TBOX :ignore)
    (IN-TBOX-INTERNAL :ignore)
    (INACCURATE-QUERIES (:list :nrql-query-id))
    (INACCURATE-RULES (:list :nrql-rule-id))
    (INCLUDE-KB :ignore)
    (INCLUDE-PERMUTATIONS :ignore)
    (INDEX-ALL-TRIPLES :ignore)
    (INDIVIDUAL-ANTONYMS (:list :abox-individual))
    (INDIVIDUAL-ATTRIBUTE-FILLERS (:list :cd-object))
    (INDIVIDUAL-DIRECT-TYPES (:list :concept-name))
    (INDIVIDUAL-FILLED-ROLES (:list :role-term))
    (INDIVIDUAL-FILLERS (:list :abox-individual))
    (INDIVIDUAL-SYNONYMS (:list :abox-individual))
    (INDIVIDUAL-TOLD-ATTRIBUTE-VALUE :cd-object)
    (INDIVIDUAL-TOLD-DATATYPE-FILLERS (:list :cd-literal))
    (INDIVIDUAL-TYPES (:list (:list :concept-name)))
    (INIT-ABOX :ignore)
    (INIT-PUBLICATIONS :ignore)
    (INIT-PUBLICATIONS-1 :ignore)
    (INIT-PUBLICATIONS1 :ignore)
    (INIT-SUBSCRIPTIONS :ignore)
    (INIT-SUBSCRIPTIONS-1 :ignore)
    (INIT-SUBSCRIPTIONS1 :ignore)
    (INIT-TBOX :ignore)
    (INSTALLED-PATCHES (:list :patch-description))
    (INSTALLED-PLUGINS (:list :patch-description))
    (INSTANTIATORS (:list (:list :concept-name)))
    (INVERSE-OF-ROLE :role-term)
    (KB-ONTOLOGIES (:list :symbol))
    (KEEP-DEFINED-QUERY-ATOMS :ignore)
    (LCS :concept-expression)
    (LCS-UNFOLD :concept-expression)
    (LOAD-RACER-PATCH :ignore)
    (LOAD-RACER-PATCHES :ignore)
    (LOAD-RACER-PLUGIN :ignore)
    (LOAD-RACER-PLUGINS :ignore)
    (LOGGING-OFF :ignore)
    (LOGGING-ON :ignore)
    (MAKE-ABDUCTION-RULE-FROM-ABOXES :ignore)
    (MAKE-BACKWARD-RULE-FROM-ABOXES :ignore)
    (MAKE-FORWARD-RULE-FROM-ABOXES :ignore)
    (MAKE-PLUGIN-FROM-FASL-FILE :ignore)
    (MAKE-QUERY-FROM-ABOX :nrql-query-body)
    (MATERIALIZE-INFERENCES :ignore)
    (MIRROR :ignore)
    (MOST-SPECIFIC-INSTANTIATORS (:list (:list :concept-name)))
    (MOVE-RULES :ignore)
    (MSC-K :concept-expression)
    (NODE-DESCRIPTION1 :substrate-description-label)
    (NODE-LABEL1  :substrate-description-label)
    (:NONE)
    (OPEN-TRIPLE-STORE :ignore)
    (OPTIMIZER-DONT-ENSURE-LATE-LAMBDA-EVALUATION :ignore)
    (OPTIMIZER-DONT-USE-CARDINALITY-HEURISTICS :ignore)
    (ORIGINAL-QUERY-BODY :nrql-query-body)
    (ORIGINAL-QUERY-HEAD :nrql-query-head)
    (ORIGINAL-RULE-ANTECEDENCE :nrql-query-body)
    (ORIGINAL-RULE-CONSEQUENCE :nrql-rule-consequence)
    (OWL-READ-DOCUMENT :ignore)
    (OWL-READ-FILE :ignore)
    (|OWLAPI-abort| :ignore)
    (|OWLAPI-addAxiom| :ignore)
    (|OWLAPI-AddAxiom| :ignore)
    (|OWLAPI-addAxioms| :ignore)
    (|OWLAPI-AddAxioms| :ignore)
    (|OWLAPI-addPrefix| :ignore)
    (|OWLAPI-advanceProgress| :ignore)
    (OWLAPI-ANSWER-SEQUENCE (:goto |OWLAPI-answerSequence|))
    (|OWLAPI-applyChanges| :ignore)
    (|OWLAPI-autoAddAxiomsTo| :ignore)
    (|OWLAPI-autoApplyChanges| :ignore)
    (|OWLAPI-autoBatchAddAxiomsTo| :ignore)
    (|OWLAPI-autoBatchRemoveAxiomsFrom| :ignore)
    (|OWLAPI-autoRemoveAxiomsFrom| :ignore)
    (|OWLAPI-AxiomToID| :ignore)
    (|OWLAPI-batchSynchronize| :ignore)
    (|OWLAPI-classify| :ignore)
    (|OWLAPI-clearChanges| :ignore)
    (|OWLAPI-clearOntologies| :ignore)
    (|OWLAPI-clearRegistry| :ignore)
    (|OWLAPI-considerDeclarations| :ignore)
    (|OWLAPI-contains| :boolean)
    (|OWLAPI-describeOntologies| (:list :owlapi-ontology-description))
    (|OWLAPI-describeOntology| :owlapi-ontology-description)
    (|OWLAPI-describeReasoner| :owlapi-reasoner-description)
    (|OWLAPI-describeReasoners| (:list :owlapi-reasoner-description))
    (|OWLAPI-disableAutoMode| :ignore)
    (|OWLAPI-disableIncrementalUpdates| :ignore)
    (|OWLAPI-disableLookupMode| :ignore)
    (|OWLAPI-disableMemorySavingMode| :ignore)
    (|OWLAPI-disableSimplifiedProtocol| :ignore)
    (|OWLAPI-disableTransientAxiomMode| :ignore)
    (|OWLAPI-dispose| :ignore)
    (|OWLAPI-disposeAxiom| :ignore)
    (|OWLAPI-disposeAxioms| :ignore)
    (|OWLAPI-disposeOntologies| :ignore)
    (|OWLAPI-disposeOntology| :ignore)
    (|OWLAPI-disposeReasoner| :ignore)
    (|OWLAPI-dontRegisterDeclaredEntities| :ignore)
    (|OWLAPI-dontRegisterReferencedEntities| :ignore)
    (|OWLAPI-enableIncrementalUpdates| :ignore)
    (|OWLAPI-enableLookupMode| :ignore)
    (|OWLAPI-enableMemorySavingMode| :ignore)
    (|OWLAPI-enableSimplifiedProtocol| :ignore)
    (|OWLAPI-enableTransientAxiomMode| :ignore)
    (|OWLAPI-exportOntology| :ignore)
    (|OWLAPI-exportReasoner| :ignore)
    (|OWLAPI-findIDFromObject| :non-negative-integer)
    (|OWLAPI-findObjectFromID| :owlapi-expression)
    (|OWLAPI-getAllOntologies| (:list :owlapi-ontology-name))
    (|OWLAPI-getAncestorClasses| (:list (:list :concept-name)))
    (|OWLAPI-getAncestorProperties| (:list (:list :object-property)))
    (|OWLAPI-getAnnotationAxiomsForAxiom| (:list :owlapi-axiom-constructor-call))
    (|OWLAPI-getAutoDeclareDataProperties| :boolean)
    (|OWLAPI-getAutoOntology| :owlapi-ontology-name)
    (|OWLAPI-getAxiomCounter| :non-negative-integer)
    (|OWLAPI-getAxioms| (:list :owlapi-axiom-constructor-call))
    (|OWLAPI-getAxiomsIn| (:list :owlapi-axiom-constructor-call))
    (|OWLAPI-getAxiomsOfType| (:list :owlapi-axiom-constructor-call))
    (|OWLAPI-getAxiomsOfTypeIn| (:list :owlapi-axiom-constructor-call))
    (|OWLAPI-getAxiomsPerOntology| (:list :owlapi-axiom-constructor-call))
    (|OWLAPI-getChanges| (:list :owlapi-change))
    (|OWLAPI-getCurrentReasoner| :owlapi-reasoner-name)
    (|OWLAPI-getDataPropertyRelationships| (:list (:list :datatype-property (:list :owl-data-literal))))
    (|OWLAPI-getDataPropertyValues| (:list :owl-data-literal))
    (|OWLAPI-getDescendantClasses| (:list (:list :concept-name)))
    (|OWLAPI-getDescendantProperties| (:list (:list :role)))
    (|OWLAPI-getDifferentIndividuals| (:list :abox-individual))
    (|OWLAPI-getDisjointClasses| (:list :concept-name))
    (|OWLAPI-getDisjointDataProperties| (:list :datatype-property))
    (|OWLAPI-getDisjointObjectProperties| (:list :object-property))
    (|OWLAPI-getDomains| (:list (:list :concept-expression)))
    (|OWLAPI-getEquivalentClasses| (:list :concept-name))
    (|OWLAPI-getEquivalentProperties| (:list :role))
    (|OWLAPI-getIDsOfLastAnswer| (:list :non-negative-integer))
    (|OWLAPI-getInconsistentClasses| (:list :concept-name))
    (|OWLAPI-getIndividuals| (:list :abox-individual))
    (|OWLAPI-getInstances| (:list (:list :abox-individual)))
    (|OWLAPI-getInverseProperties| (:list (:list :object-property)))
    (|OWLAPI-getLastAnswer| :sexpression)
    (|OWLAPI-getLastOutputStreamString| :string)
    (|OWLAPI-getLoadedOntologies| (:list :owlapi-ontology-name))
    (|OWLAPI-getObjectPropertyRelationships| (:list (:list :object-property (:list :abox-individual))))
    (|OWLAPI-getObjectPropertyValues| (:list (:list :abox-individual)))
    (|OWLAPI-getOntologies| (:list :owlapi-ontology-name))
    (|OWLAPI-getPrefixes| (:list (:dotted-pair :prefix-string :url)))
    (|OWLAPI-getRanges| (:list (:list :concept-expression)))
    (|OWLAPI-getReasoners| (:list :owlapi-rasoner-name))
    (|OWLAPI-getRelatedIndividuals| (:list :abox-individual))
    (|OWLAPI-getRelatedValues| (:list :owl-data-literal))
    (|OWLAPI-getSameIndividuals| (:list :abox-individual))
    (|OWLAPI-getSubClasses| (:list (:list :concept-name)))
    (|OWLAPI-getSubProperties| (:list (:list :role)))
    (|OWLAPI-getSuperClasses| (:list (:list :concept-name)))
    (|OWLAPI-getSuperProperties| (:list (:list :role)))
    (|OWLAPI-getTypes| (:list (:list :concept-name)))
    (|OWLAPI-hasDataPropertyRelationship| :boolean)
    (|OWLAPI-hasObjectPropertyRelationship| :boolean)
    (|OWLAPI-hasType| :boolean)
    (|OWLAPI-IDToAxiom| :owlapi-axiom-constructor-call)
    (|OWLAPI-ignoreAnnotations| :ignore)
    (|OWLAPI-ignoreDeclarations| :ignore)
    (|OWLAPI-init| :ignore)
    (|OWLAPI-isAsymmetric| :boolean)
    (|OWLAPI-isClass| :boolean)
    (|OWLAPI-isClassified| :boolean)
    (|OWLAPI-isConsistent| :boolean)
    (|OWLAPI-isDefinedClass| :boolean)
    (|OWLAPI-isDefinedDataProperty| :boolean)
    (|OWLAPI-isDefinedIndividual| :boolean)
    (|OWLAPI-isDefinedObjectProperty| :boolean)
    (|OWLAPI-isDifferentIndividual| :boolean)
    (|OWLAPI-isEntailed| :boolean)
    (|OWLAPI-isEquivalentClass| :boolean)
    (|OWLAPI-isFunctional| :boolean)
    (|OWLAPI-isInverseFunctional| :boolean)
    (|OWLAPI-isIrreflexive| :boolean)
    (|OWLAPI-isRealised| :boolean)
    (|OWLAPI-isReflexive| :boolean)
    (|OWLAPI-isSameIndividual| :boolean)
    (|OWLAPI-isSatisfiable| :boolean)
    (|OWLAPI-isSubClassOf| :boolean)
    (|OWLAPI-isSymmetric| :boolean)
    (|OWLAPI-isTransitive| :boolean)
    (|OWLAPI-keepAnnotations| :ignore)
    (|OWLAPI-loadAxiom| :ignore)
    (|OWLAPI-loadAxioms| :ignore)
    (|OWLAPI-loadOntologies| :ignore)
    (|OWLAPI-loadOntology| :ignore)
    (|OWLAPI-manuallyApplyChanges| :ignore)
    (|OWLAPI-mergeOntologies| :ignore)
    (|OWLAPI-newOntology| :owlapi-ontology-name)
    (|OWLAPI-newReasoner| :owlapi-reasoner-name)
    (|OWLAPI-newReasoner1| :owlapi-reasoner-name)
    (|OWLAPI-nextAxiomUseID| :ignore)
    (|OWLAPI-parse| :owlapi-axiom-id)
    (|OWLAPI-parseNative| :owlapi-axiom-id)
    (OWLAPI-QUIET-SEQUENCE :ignore)
    (|OWLAPI-readFunctionalOntologyDocument| :ignore)
    (|OWLAPI-readFunctionalOntologyFile| :ignore)
    (|OWLAPI-readOntology| :ignore)
    (|OWLAPI-readXMLOntologyDocument| :ignore)
    (|OWLAPI-readXMLOntologyFile| :ignore)
    (|OWLAPI-realize| :ignore)
    (|OWLAPI-registerDeclaredEntities| :ignore)
    (|OWLAPI-registerLastAnswer| :ignore)
    (|OWLAPI-registerObject| :non-negative-integer)
    (|OWLAPI-registerReferencedEntities| :ignore)
    (|OWLAPI-reloadLoadedOntologies| :ignore)
    (|OWLAPI-removeAxiom| :ignore)
    (|OWLAPI-RemoveAxiom| :ignore)
    (|OWLAPI-removeAxioms| :ignore)
    (|OWLAPI-RemoveAxioms| :ignore)
    (|OWLAPI-removePrefix| :ignore)
    (|OWLAPI-resetAxiomCounter| :ignore)
    (|OWLAPI-restoreImage| :ignore)
    (|OWLAPI-saveOntology| :ignore)
    (OWLAPI-SEQUENCE :ignore)
    (|OWLAPI-setAutoDeclareDataProperties| :ignore)
    (|OWLAPI-setAxiomCounter| :ignore)
    (|OWLAPI-setCurrentReasoner| :ignore)
    (|OWLAPI-SetOntologyURI| :ignore)
    (|OWLAPI-setReturnPolicy| :ignore)
    (|OWLAPI-sleep| :ignore)
    (|OWLAPI-storeImage| :ignore)
    (|OWLAPI-unloadAxiom| :ignore)
    (|OWLAPI-unloadAxioms| :ignore)
    (|OWLAPI-unloadOntologies| :ignore)
    (|OWLAPI-unloadOntology| :ignore)
    (|OWLAPI-usesIncrementalUpdates| :ignore)
    (|OWLAPI-usesSimplifiedProtocol| :ignore)
    (|OWLAPI-writeFunctionalOntologyFile| :ignore)
    (|OWLAPI-writeOntologyFile| :ignore)
    (|OWLAPI-writeXMLOntologyFile| :ignore)
    (OWLLINK-READ-DOCUMENT :ignore)
    (OWLLINK-READ-FILE :ignore)
    (PRACER-ANSWER-QUERY :nrql-query-answer)
    (PREFER-DEFINED-QUERIES :ignore)
    (PREPARE-ABOX :ignore)
    (PREPARE-RACER-ENGINE :ignore)
    (PRETRIEVE (:goto PRACER-ANSWER-QUERY))
    (PRINT-ABOX-INDIVIDUALS :ignore)
    (PRINT-TBOX-TREE :ignore)
    (PROCESS-SET-AT-A-TIME :ignore)
    (PROCESSED-QUERIES (:list :nrql-query-id))
    (PROCESSED-RULES (:list :nrql-rule-id))
    (PUBLISH :ignore)
    (PUBLISH-1 :ignore)
    (PUBLISH-FILE :ignore)
    (QUERY-ANCESTORS (:list :nrql-query-id))
    (QUERY-DESCENDANTS (:list :nrql-query-id))
    (QUERY-SUBSCRIBERS (:list :symbol))
    (QUIET-SEQUENCE :ignore)
    (RACER-ANSWER-QUERY-UNDER-PREMISE :nrql-query-answer)
    (RACER-ANSWER-QUERY-UNDER-PREMISE1 :nrql-query-answer)
    (RACER-ANSWER-QUERY-WITH-EXPLANATION :nrql-abductive-query-answer)
    (RACER-ANSWER-QUERY1 :nrql-query-answer)
    (RACER-ANSWER-TBOX-QUERY :nrql-tbox-query-answer)
    (RACER-ANSWER-TBOX-QUERY1 :nrql-tbox-query-answer)
    (RACER-APPLY-RULE-UNDER-PREMISE :nrql-rule-result)
    (RACER-APPLY-RULE-UNDER-PREMISE1 :nrql-rule-result)
    (RACER-APPLY-RULE1 :nrql-rule-result)
    (RACER-PREPARE-QUERY1 :nrql-query-id-and-status)
    (RACER-PREPARE-RULE1 :nrql-rule-id-and-status)
    (RACER-PREPARE-TBOX-QUERY1 :nrql-query-id-and-status)
    (RACER-READ-DOCUMENT :ignore)
    (RACER-READ-FILE :ignore)
    (RDFS-READ-TBOX-FILE :ignore)
    (READY-QUERIES (:list :nrql-query-id))
    (READY-RULES (:list :nrql-rule-id))
    (REALIZE-ABOX :ignore)
    (RECOGNIZE-EVENTS (:list (:list :symbol :timenet-query-result)))
    (REEXECUTE-ALL-RULES (:list :nrql-rule-result))
    (REEXECUTE-RULE (:or :nrql-rule-result (:one-of :not-found)))
    (RELATED-INDIVIDUALS (:list (:list :abox-individual :abox-individual)))
    (REMOVE-IMPLIED-CONCEPT-ASSERTIONS (:list (:list :concept-assertion) (:list :concept-assertion)))
    (REPREPARE-RULE (:or :nrql-rule-id-and-status (:one-of :not-found)))
    (RESTORE-ABOX-IMAGE :ignore)
    (RESTORE-ABOXES-IMAGE :ignore)
    (RESTORE-KB-IMAGE :ignore)
    (RESTORE-KBS-IMAGE :ignore)
    (RESTORE-STANDARD-SETTINGS :ignore)
    (RESTORE-TBOX-IMAGE :ignore)
    (RESTORE-TBOXES-IMAGE :ignore)
    (RETRIEVE-CONCEPT-INSTANCES (:list :abox-individual))
    (RETRIEVE-DIRECT-PREDECESSORS (:list :abox-individual))
    (RETRIEVE-INDIVIDUAL-ANNOTATION-PROPERTY-FILLERS (:list :cd-value))
    (RETRIEVE-INDIVIDUAL-ANTONYMS (:list :abox-individual))
    (RETRIEVE-INDIVIDUAL-ATTRIBUTE-FILLERS (:list :cd-object))
    (RETRIEVE-INDIVIDUAL-FILLED-ROLES (:list :role))
    (RETRIEVE-INDIVIDUAL-FILLERS (:list :abox-individual))
    (RETRIEVE-INDIVIDUAL-SYNONYMS (:list :abox-individual))
    (RETRIEVE-INDIVIDUAL-TOLD-ATTRIBUTE-VALUE :cd-value)
    (RETRIEVE-INDIVIDUAL-TOLD-DATATYPE-FILLERS (:list :cd-value))
    (RETRIEVE-RELATED-INDIVIDUALS (:list (:list :abox-individual :abox-individual)))
    (RMI :ignore)
    (ROLE-ANCESTORS (:list :role))
    (ROLE-CHILDREN (:list :role))
    (ROLE-DESCENDANTS (:list :role))
    (ROLE-DISJOINT-1 :ignore)
    (ROLE-DOMAIN :concept-expression)
    (ROLE-INVERSE :role-term)
    (ROLE-PARENTS (:list :role))
    (ROLE-RANGE :concept-expression)
    (ROLE-SYNONYMS (:list :role))
    (RUNNING-CHEAP-QUERIES (:list :nrql-query-id))
    (RUNNING-CHEAP-RULES (:list :nrql-rule-id))
    (RUNNING-EXPENSIVE-QUERIES (:list :nrql-query-id))
    (RUNNING-EXPENSIVE-RULES (:list :nrql-rule-id))
    (RUNNING-QUERIES (:list :nrql-query-id))
    (RUNNING-RULES (:list :nrql-rule-id))
    (SAVE-ABOX :ignore)
    (SAVE-KB :ignore)
    (SAVE-ONTOLOGY-TO-TRIPLE-STORE :ignore)
    (SAVE-TBOX :ignore)
    (SEQUENCE :ignore)
    (SERVER-CASE  :ignore)
    (SET-ATTRIBUTE-FILLER :ignore)
    (SET-CURRENT-ABOX :ignore)
    (SET-CURRENT-TBOX :ignore)
    (SET-EDGE-LABEL-FOR-NON-EXISTENT-EDGES :ignore)
    (SET-FIND-ABOX :ignore)
    (SET-FIND-TBOX :ignore)
    (SET-MIRROR-DATA-BOX :ignore)
    (SET-NEW-IND-COUNTER :ignore)
    (SET-NEW-IND-PREFIX :ignore)
    (SET-PROXY-SERVER :ignore)
    (SET-RACER-PARAMETER :ignore)
    (SET-REWRITE-DEFINED-CONCEPTS :ignore)
    (SET-SERVER-TIMEOUT :ignore)
    (SET-UNIQUE-NAME-ASSUMPTION :ignore)
    (SIGNATURE :ignore)
    (SPARQL-ANSWER-QUERY :nrql-query-answer)
    (SPARQL-RETRIEVE :nrql-query-answer)
    (STORE-ABOX-IMAGE :ignore)
    (STORE-ABOXES-IMAGE :ignore)
    (STORE-KB-IMAGE :ignore)
    (STORE-KBS-IMAGE :ignore)
    (STORE-TBOX-IMAGE :ignore)
    (STORE-TBOXES-IMAGE :ignore)
    (SUBSCRIBE :ignore)
    (SUBSCRIBE-1 :ignore)
    (SUBSCRIBE-TO :ignore)
    (SWRL-CREATE-ABDUCTION-RULES-IF-POSSIBLE :ignore)
    (SWRL-CREATE-FORWARD-CHAINGING-RULES :ignore)
    (SWRL-FORWARD-CHAINING :ignore)
    (TAXONOMY :taxonomy)
    (TIME :ignore)
    (TIMENET-ANSWER-QUERY :timenet-query-result)
    (TIMENET-RETRIEVE :timenet-query-result)
    (TRANSMIT-FILE :ignore)
    (TRIPLE-STORE-GRAPHS :ignore)
    (TRIPLE-STORE-READ-FILE :ignore)
    (UNAPPLICABLE-RULES (:list :nrql-rule-id))
    (UNPUBLISH :ignore)
    (UNPUBLISH-1 :ignore)
    (UNSUBSCRIBE :ignore)
    (UNSUBSCRIBE-1 :ignore)
    (UNSUBSCRIBE-FROM :ignore)
    (UPDATE-RACER :ignore)
    (USE-TRIPLE-STORE :ignore)
    (VERIFY-WITH-ABOX-INDIVIDUALS-LIST :ignore)
    (VERIFY-WITH-CONCEPT-TREE-LIST :ignore)
    (WAIT-FOR-RULES-TO-TERMINATE :ignore)
    (WAITING-CHEAP-QUERIES (:list :nrql-query-id))
    (WAITING-CHEAP-RULES (:list :nrql-rule-id))
    (WAITING-EXPENSIVE-QUERIES (:list :nrql-query-id))
    (WAITING-EXPENSIVE-RULES (:list :nrql-rule-id))
    (WAITING-QUERIES (:list :nrql-query-id))
    (WAITING-RULES (:list :nrql-rule-id))
    (XML-INPUT :ignore)
    (XML-NATIVE-OUTPUT :ignore)
    (XML-OUTPUT :ignore)
    (XML-READ-TBOX-FILE :ignore)
    (ADD-CHOSEN-SETS-OF-RULE-CONSEQUENCES :ignore)
    (CHOOSE-CURRENT-SET-OF-RULE-CONSEQUENCES :ignore)
    (CLASSIFY-QUERY :ignore)
    (DELETE-QUERY :ignore)
    (DELETE-RULE :ignore)
    (DESCRIBE-QUERY (:or (:one-of :not-found) :nrql-query-description))
    (DESCRIBE-QUERY-STATUS  (:or (:one-of :not-found) :nrql-query-status))
    (EXECUTE-APPLICABLE-RULES (:list (:list :abox)))
    (EXECUTE-QUERY (:or (:one-of :not-found) :nrql-query-answer))
    (EXECUTE-RULE (:or (:one-of :not-found) (:list :abox)))
    (GET-ANSWER  (:or (:one-of :not-found) :nrql-query-answer))
    (GET-CHOSEN-SETS-OF-RULE-CONSEQUENCES  (:or (:one-of :not-found) (:list :abox)))
    (GET-CURRENT-SET-OF-RULE-CONSEQUENCES (:or (:one-of :not-found) :abox))
    (GET-CURRENT-TUPLE (:or (:one-of :not-found) :nrql-answer-tuple))
    (GET-DAG-OF-QBOX-FOR-ABOX :ignore)
    (GET-NEXT-SET-OF-RULE-CONSEQUENCES (:or (:one-of :not-found) :abox))
    (GET-NEXT-TUPLE (:or (:one-of :not-found) :nrql-answer-tuple))
    (GET-NODES-IN-QBOX-FOR-ABOX (:list :nrql-query-id))
    (QUERY-BODY (:or (:one-of :not-found) :nrql-query-body))
    (QUERY-CHILDREN (:or (:one-of :not-found) (:list :nrql-query-id)))
    (QUERY-EQUIVALENTS (:or (:one-of :not-found) (:list :nrql-query-id)))
    (QUERY-HEAD (:or (:one-of :not-found) (:list :nrql-query-head)))
    (QUERY-PARENTS (:or (:one-of :not-found) (:list :nrql-query-id)))
    (REEXECUTE-QUERY (:or (:one-of :not-found) :nrql-query-answer))
    (REPREPARE-QUERY (:or (:one-of :not-found)  :nrql-query-id-and-status))
    (RULE-ANTECEDENCE (:or (:one-of :not-found) :rule-lefthand-side))
    (RULE-CONSEQUENCE  (:or (:one-of :not-found) :rule-righthand-side))
    (SHOW-QBOX-FOR-ABOX :ignore)
    (ABORT-ALL-QUERIES :ignore)
    (ADD-ROLE-ASSERTIONS-FOR-DATATYPE-PROPERTIES :ignore)
    (ADD-RULE-CONSEQUENCES-AUTOMATICALLY :ignore)
    (ALL-SUBSTRATES (:list :substrate-name))
    (CHECK-ABOX-CONSISTENCY-BEFORE-QUERYING :ignore)
    (CREATE-DATA-EDGE :ignore)
    (CREATE-DATA-NODE :ignore)
    (DEFCON1 :ignore)
    (DEFINE-QUERY :ignore)
    (DEFINE1 :ignore)
    (DEFPAR1 :ignore)
    (DELETE-ALL-DEFINITIONS :ignore)
    (DELETE-ALL-QUERIES :ignore)
    (DELETE-ALL-RULES :ignore)
    (DELETE-ALL-SUBSTRATES :ignore)
    (DELETE-DATA-EDGE :ignore)
    (DELETE-DATA-NODE :ignore)
    (DESCRIBE-ALL-DEFINITIONS (:list :defquery-expression))
    (DESCRIBE-ALL-NODES (:list :substrate-node-description))
    (DESCRIBE-ALL-SUBSTRATES (:list :substrate-description))
    (DESCRIBE-CURRENT-SUBSTRATE :substrate-description)
    (DESCRIBE-DEFINITION :defquery-expression (:list :defquery-expression))
    (DESCRIBE-QUERY-PROCESSING-MODE :nrql-query-processing-mode)
    (DESCRIBE-SUBSTRATE :substrate-description)
    (ENABLE-ABOX-MIRRORING :ignore)
    (ENABLE-EAGER-TUPLE-COMPUTATION :ignore)
    (ENABLE-KB-HAS-CHANGED-WARNING-TOKENS :ignore)
    (ENABLE-NRQL-WARNINGS :ignore)
    (ENABLE-PHASE-TWO-STARTS-WARNING-TOKENS :ignore)
    (ENABLE-QUERY-OPTIMIZATION :ignore)
    (ENABLE-RCC-SUBSTRATE-MIRRORING :ignore)
    (ENABLE-SMART-ABOX-MIRRORING :ignore)
    (ENABLE-TOLD-INFORMATION-QUERYING :ignore)
    (ENABLE-TWO-PHASE-QUERY-PROCESSING-MODE :ignore)
    (ENABLE-VERY-SMART-ABOX-MIRRORING :ignore)
    (EVALUATE1 :minilisp-sexpression)
    (EXCLUDE-PERMUTATIONS :ignore)
    (EXECUTE-ALL-QUERIES :ignore)
    (EXECUTE-OR-REEXECUTE-ALL-QUERIES :ignore)
    (FCALL :minilisp-sexpression)
    (GET-ALL-ANSWERS (:list :nrql-query-answer))
    (GET-ALL-FUNCTIONS (:list :minilisp-define-expression))
    (GET-ALL-SERVER-FUNCTIONS (:list :minilisp-define-expression))
    (GET-ALL-SERVER-VALUES (:list :minilisp-defcon-expression))
    (GET-ALL-VALUES (:list :minilisp-defcon-expression))
    (GET-DATA-EDGE-DESCRIPTION :substrate-edge-description)
    (GET-DATA-EDGE-LABEL :substrate-description-label)
    (GET-DATA-NODE-DESCRIPTION  :substrate-description-label)
    (GET-DATA-NODE-LABEL  :substrate-description-label)
    (GET-PROCESS-POOL-SIZE :non-negative-integer)
    (GET-SUBSTRATE-NODES (:list :substrate-node))
    (GET-SUBSTRATE-TYPE (:ONE-OF THEMATIC-SUBSTRATE::data-substrate 
                         THEMATIC-SUBSTRATE::mirror-data-substrate 
                         THEMATIC-SUBSTRATE::rcc-substrate 
                         THEMATIC-SUBSTRATE::rcc-mirror-substrate))
    (OPTIMIZER-ENSURE-LATE-LAMBDA-EVALUATION :ignore)
    (OPTIMIZER-GET-NO-OF-PLANS-UPPER-BOUND :non-negative-integer)
    (OPTIMIZER-GET-TIME-BOUND :non-negative-integer)
    (OPTIMIZER-SET-NO-OF-PLANS-UPPER-BOUND :non-negative-integer)
    (OPTIMIZER-SET-TIME-BOUND :non-negative-integer)
    (OPTIMIZER-USE-CARDINALITY-HEURISTICS :ignore)
    (PREPARE-NRQL-ENGINE :ignore)
    (PROCESS-TUPLE-AT-A-TIME :ignore)
    (RACER-ANSWER-QUERY :nrql-query-answer)
    (RACER-APPLY-RULE :nrql-rule-result)
    (RACER-PREPARE-QUERY :nrql-query-id-and-status)
    (RACER-PREPARE-RULE :nrql-rule-id-and-status)
    (RACER-PREPARE-TBOX-QUERY :nrql-query-id-and-status)
    (REEXECUTE-ALL-QUERIES (:list :nrql-query-answer))
    (REGISTER-RCC-SYNONYM :ignore)
    (REPORT-INCONSISTENT-QUERIES-AND-RULES :ignore)
    (RESET-ALL-SUBSTRATES :ignore)
    (RESET-NRQL-ENGINE :ignore)
    (RESTORE-ALL-SUBSTRATES :ignore)
    (RESTORE-SERVER-IMAGE :ignore)
    (RESTORE-SUBSTRATE :ignore)
    (SERVER-FUNCTION :ignore)
    (SERVER-VALUE :ignore)
    (SET-DATA-BOX :ignore)
    (SET-INITIAL-SIZE-OF-PROCESS-POOL :ignore)
    (SET-MAX-NO-OF-TUPLES-BOUND :ignore)
    (SET-MAXIMUM-SIZE-OF-PROCESS-POOL :ignore)
    (SET-NRQL-MODE :ignore)
    (SET-RCC-BOX :ignore)
    (SET-SUBSTRATE-TYPE :ignore)
    (STORE-ALL-SUBSTRATES :ignore)
    (STORE-SERVER-IMAGE :ignore)
    (STORE-SUBSTRATE-FOR-ABOX :ignore)
    (UNBIND-ALL :ignore)
    (UNBIND1 :ignore)
    (UNDEFINE-ALL :ignore)
    (UNDEFINE-QUERY :ignore)
    (UNDEFINE1 :ignore)
    (USE-INDIVIDUAL-SYNONYM-EQUIVALENCE-CLASSES :ignore)
    (USE-INJECTIVE-VARIABLES-BY-DEFAULT :ignore)
    (WAIT-FOR-QUERIES-TO-TERMINATE :ignore)
    (abort-query (:one-of :not-found :okay-query-aborted))))

(defun get-return-type-for-function (fn)
  (let* ((sections
          (mapcar #'symbol-name (get-sections fn)))
         (tell-p 
          (find-if #'(lambda (x)
                       (search "TELL" x))
                   sections))
         (found (assoc fn +return-type-for-function+)))
    (cond (found
           (if (consp (second found))
               (if (eq (first (second found)) :goto)
                   (get-return-type-for-function (second (second found)))
                 (unless (eq (second found) :ignore)
                   (list 
                    (second found)
                    (get-type-description (second found)))))
             (unless (eq (second found) :ignore)
               (list (second found)
                     (get-type-description (second found))))))
          ((is-predicate-p fn)
           (list :boolean (get-type-description :boolean)))
          ((or tell-p
               (eq (second found) :ignore))
           ;; :irrelevant-is-tell-statement
           nil)
          (t
           (pushnew fn *unknown-return-types*)
           (list :unknown "Unknown")))))

;;;
;;;
;;; 

(defun create-docgen-stubs (&optional only-collect-p)

  (clear-all-documentation)

  (load "nrql-dev:docgen-stubs-sofia.lisp")

  (let ((descriptions
         (loop as entry in *documentation* 
               as label =  (second (assoc :label entry))
               as original-entry = (second (assoc :original-entry entry))
               as original-descr =  (second (assoc :description original-entry))
               as descr = (second (assoc :description entry))
               when (and original-descr
                         (not (string-equal original-descr "to be written")))
               collect (list label original-descr descr)))

        (examples 
         (loop as entry in *documentation* 
               as label =  (second (assoc :label entry))
               as original-entry = (second (assoc :original-entry entry))
               as examples =  
               (remove-if #'(lambda (x) 
                              (or (not (second x))
                                  (and (stringp (second x))
                                       (member (second x) '("here")
                                               :test #'string-equal))))
                          (cdr (assoc :examples original-entry)))
               as original-examples =  
               (remove-if #'(lambda (x) 
                              (or (not (second x))
                                  (and (stringp (second x))
                                       (member (second x) '("here")
                                               :test #'string-equal))))
                          (cdr (assoc :examples entry)))
               when examples
               collect (list label 
                             (loop as ex in original-examples
                                   as count from 1 by 1 collect
                                   (list (format nil "Example ~A:" count)
                                         (second ex)))
                             (loop as ex in examples
                                   as count from 1 by 1 collect
                                   (list (format nil "Example ~A:" count)
                                         (second ex)))))))

    (declare (ignorable examples))
    
    (labels ((get-sofia-description (function)
               (second (assoc function descriptions))))

      ;;;
      ;;;
      ;;;

      (with-open-file (stream "nrql-dev:docgen-stubs.lisp"
                              :direction :output 
                              :if-does-not-exist :create
                              :if-exists :supersede)

        (let* ((excluded 
                (append '()
                        (mapcar #'caadr (cdr (get-owlapi-synonyms)))))
           
               (functions  
                (sort 
                 (set-difference 
                  (get-racer-functions)
                  excluded)
                 #'string-lessp 
                 :key #'symbol-name))

               (macros 
                (sort 
                 (set-difference
                  (get-racer-macros)
                  excluded)
                 #'string-lessp 
                 :key #'symbol-name))
           
               (with-macros 
                (sort 
                 (set-difference
                  (get-racer-with-macros)
                  excluded)
                 #'string-lessp 
                 :key #'symbol-name)))

          (format stream "(in-package :racer)~%~%")

          (format stream ";;;~%")
          (format stream ";;;----------------------------------------------~%")
          (format stream ";;;    Automatically Generated Docgen Stubs   ~%")
          (format stream ";;;          Version: ~A, Build: ~A ~%" 
                  (get-product-version) (get-build-version))
          (format stream ";;;----------------------------------------------~%")
          (format stream ";;;")
    
          (terpri stream)

          (format stream "~%(defun load-documentation ()~%~%(progn ~%")

          ;; (pprint `(clear-all-documentation) stream)


          (setf *all-racer-functions* nil)

          (let ((*print-pretty* t)
                (*print-pprint-dispatch* *new-pprint-dispatch*))

            (set-pprint-dispatch 'symbol
                                 #'(lambda (stream &rest args)
                                     (declare (ignorable args))
                                     (write-string
                                      (let ((*print-pretty* nil)
                                            (*print-pprint-dispatch* *old-pprint-dispatch*))
                                        (pretty-symbol-name (first args) t))
                                      stream)))

            ;;;
            ;;;
            ;;;

            (labels ((get-args (function)
                       (let* ((lambda (get-lambda function))
                              (args
                               (when (consp lambda)
                                 (remove nil 
                                         (mapcar
                                          #'(lambda (x)
                                              (let ((arg
                                                     (unless 
                                                         (member x 
                                                                 '(&key &rest &args &body &whole &allow-other-keys &optional))
                                                       (if (consp x) 
                                                           (first x) 
                                                         x))))
                                                arg))
                                          lambda)))))
                         (remove :ignore
                                 (mapcar #'(lambda (x) (get-arg-description x function))
                                         args)))))

              ;;;
              ;;; Render Sections
              ;;;

              (pprint `(clear-all-documentation) stream)

              (terpri stream)

              (pprint `(add-doc-entry (:title "All Functions")
                                      (:type :section)
                                      (:protected t)
                                      (:label all-functions)
                                      (:description "Please choose:"))
                      stream)

              (terpri stream)

              (pprint `(add-doc-entry (:title "Unclassified Functions (SHOULD BE EMPTY!!!)")
                                      (:type :section)
                                      (:protected t)
                                      (:label unclassified)
                                      (:description "Please choose:"))
                      stream)

              (terpri stream)

              ;;;
              ;;;
              ;;; 

              (dolist (item +super-sections+)
                (let ((label (first item))
                      (title (second item))
                      (supersections (cddr item)))

                  (pprint `(add-doc-entry (:title ,title)
                                          (:label ,(intern (symbol-name label)))
                                          (:type :section)
                                          (:protected t)
                                          (:in-sections ,@(mapcar #'(lambda (x) 
                                                                      (if (consp x)
                                                                          (list (intern (symbol-name (first x)))
                                                                                (second x))
                                                                        (intern (symbol-name x))))
                                                                  supersections))
                                          (:description "Please choose:"))
                          stream)
                
                  (terpri stream)))

              ;;;
              ;;; Render Racer Functions and Macros
              ;;;
   
              (let* ((nrql-functions
                      (append 
                       (intersection functions
                                     (mapcar #'first ts::*nrql-functions*))
                       (intersection functions
                                     (mapcar #'first ts::*nrql-methods*))))
                     (functions 
                      (set-difference functions nrql-functions)))

                (dolist (function functions)

                  (pushnew function *all-racer-functions*)
              
                  (unless only-collect-p

                    (pprint `(add-doc-entry (:title ,function)
                                            (:label ,function)
                                    
                                            (:type :function) 

                                            (:in-sections ,@(get-sections function))
                                      
                                            (:corresponding-macro ,(get-macro-for-function function))

                                            (:protected t)
      
                                            (:signature ,(ensure-list (get-lambda function)))

                                            (:returns ,@(get-return-type-for-function function))

                                            (:description ,(get-sofia-description function))

                                            (:arguments 
                                             ,@(get-args function))

                                            (:remarks nil)

                                            (:see-also nil))
                            stream)
        
                    (terpri stream)))

                ;;;
                ;;; Render nRQL Functions
                ;;; 

                (dolist (function nrql-functions)

                  (pushnew function *all-racer-functions*)

                  (unless only-collect-p

                    (let ((syn (nrql-function-lookup-arg function :synonym)))

                      (cond (syn

                             (pprint `(add-doc-entry (:title ,function)
                                                     (:label ,function)
                                                     (:type :synonym-function) 

                                                     (:in-sections ,@(get-sections syn))

                                                     (:protected t)
      
                                                     (:synonym-for ,(second syn)))
                                     stream)

                             )

                            (t

                             (pprint `(add-doc-entry (:title ,function)
                                                     (:label ,function)
                                    
                                                     (:type :function) 

                                                     (:in-sections ,@(get-sections function))

                                                     ,@(let ((found 
                                                              (or 
                                                               (find-if #'(lambda (x) 
                                                                            (eq function (second x)))
                                                                        ts::*nrql-macros*)
                                                               (find-if #'(lambda (x) 
                                                                            (eq function (second x)))
                                                                        ts::*nrql-with-macros*))))
                                                         (when found
                                                           (list (list :corresponding-macro (first found)))))
                                      
                                                     (:protected t)
      
                                                     (:signature ,(ensure-list (get-lambda function)))

                                                     (:returns 
                                                      ,@(or 
                                                         #+:ignore
                                                         (mapcar #'transform-tex-markup 
                                                                 (cdr (nrql-function-lookup-arg function :values)))
                                                         (get-return-type-for-function function)))
                                      
                                                     (:arguments 
                                                      ,@(get-args function))

                                                     (:description 
                                                      ,(or 
                                                        (transform-tex-markup 
                                                         (second (nrql-function-lookup-arg function :description)))))

                                                     (:remarks 
                                                      ,(or 
                                                        (transform-tex-markup
                                                         (second (nrql-function-lookup-arg function :remarks)))))

                                                     (:see-also 
                                                      ,@(cdr (nrql-function-lookup-arg function :see-also)))

                                                     ,@(let ((found 
                                                              (nrql-function-lookup-arg function :rule-equivalent-of )))
                                                         (when found
                                                           (list found)))

                                                     ,@(let ((found 
                                                              (or 
                                                               (find-if #'(lambda (x) 
                                                                            (member function
                                                                                    (cdr 
                                                                                     (assoc :rule-equivalent-of (third x)))))
                                                                        ts::*nrql-functions*)
                                                               (find-if #'(lambda (x) 
                                                                            (member function
                                                                                    (cdr 
                                                                                     (assoc :rule-equivalent-of (third x)))))
                                                                        ts::*nrql-methods*)
                                                               (find-if #'(lambda (x) 
                                                                            (member function
                                                                                    (cdr 
                                                                                     (assoc :rule-equivalent-of (third x)))))
                                                                        ts::*nrql-macros*))))
                                                         (when found
                                                           (list (list :query-equivalent-of (first found)))))
                                      
                                                     (:examples 
                                                      ,@(or
                                                         (let ((found
                                                                (cdr
                                                                 (nrql-function-lookup-arg function :examples)))
                                                               (count 0))
                                                           (if found
                                                               (mapcar #'(lambda (ex)
                                                                           (incf count)
                                                                           (list (format nil "Example ~r" count) ex))
                                                                       found)
                                                             nil)))))
                                     stream)))
        
                      (terpri stream)))))

              ;;;
              ;;; Racer Macros
              ;;;

              (let* ((nrql-macros
                      (intersection macros
                                    (mapcar #'first ts::*nrql-macros*)))
                     (macros 
                      (set-difference macros nrql-macros)))

                (dolist (function macros)

                  (pushnew function *all-racer-functions*)

                  (unless only-collect-p

                    (pprint `(add-doc-entry (:title ,function)
                                            (:label ,function)
                                    
                                            (:type :macro) 
                                            (:macro-for ,(get-function-for-macro function))

                                            (:in-sections ,@(get-sections function))
                                      
                                            (:protected t)
      
                                            (:signature ,(ensure-list (get-lambda function)))

                                            (:returns ,@(or (get-return-type-for-function function)
                                                            (get-return-type-for-function (get-function-for-macro function))))
                                        
                                            (:arguments 
                                             ,@(get-args function))

                                            (:see-also nil)
                                      
                                            (:description ,(get-sofia-description function))

                                            (:remarks nil)

                                            (:examples nil))

                            stream)
            
                    (terpri stream)))

                ;;;
                ;;; Render nRQL Macros
                ;;;

                (dolist (function nrql-macros)

                  (pushnew function *all-racer-functions*)

                  (unless only-collect-p 

                    (let ((corfun (second (assoc function ts::*nrql-macros*)))
                          (syn (assoc :synonym (third (assoc function ts::*nrql-macros*)))))

                      (cond (syn

                             (pprint `(add-doc-entry (:title ,function)
                                                     (:label ,function)
                                                     (:type :synonym-macro) 

                                                     (:in-sections ,@(get-sections syn))

                                                     (:protected t)
      
                                                     (:synonym-for ,(second syn)))
                                     stream)

                             )

                            (t

                             (pprint `(add-doc-entry (:title ,function)
                                                     (:label ,function)
                                    
                                                     (:type :macro) 

                                                     (:macro-for ,corfun)
                                        
                                                     (:see-also 
                                                      ,@(or 
                                                         (cdr (assoc :see-also
                                                                     (third (assoc function ts::*nrql-macros*))))
                                                         (list corfun)))

                                                     ,@(let ((found 
                                                              (assoc :rule-equivalent-of 
                                                                     (third (assoc function ts::*nrql-macros*)))))
                                                         (when found
                                                           (list found)))

                                                     ,@(let ((found 
                                                              (or 
                                                               (find-if #'(lambda (x) 
                                                                            (member function
                                                                                    (cdr 
                                                                                     (assoc :rule-equivalent-of (third x)))))
                                                                        ts::*nrql-functions*)
                                                               (find-if #'(lambda (x) 
                                                                            (member function
                                                                                    (cdr 
                                                                                     (assoc :rule-equivalent-of (third x)))))
                                                                        ts::*nrql-methods*)
                                                               (find-if #'(lambda (x) 
                                                                            (member function
                                                                                    (cdr 
                                                                                     (assoc :rule-equivalent-of (third x)))))
                                                                        ts::*nrql-macros*))))
                                                         (when found
                                                           (list (list :query-equivalent-of (first found))))))
                                     stream)))
            
                      (terpri stream)))))

              ;;;
              ;;; With-macros 
              ;;; 

              (dolist (function with-macros)

                (pushnew function *all-racer-functions*)

                (unless only-collect-p

                  (let* ((lambda (first (get-lambda function)))
                         (args
                          (when (consp lambda)
                            (remove nil 
                                    (mapcar
                                     #'(lambda (x)
                                         (let ((arg
                                                (unless 
                                                    (member x 
                                                            '(&key &rest &args &body &whole &allow-other-keys &optional))
                                                  (if (consp x) 
                                                      (first x) 
                                                    x))))
                                           arg))
                                     lambda))))
                         (args
                          (append
                           (mapcar #'(lambda (x) (get-arg-description x function)) args)
                           (list (list '&body "a description" :type)))))

                    (pprint `(add-doc-entry (:title ,function)
                                            (:label ,function)
                                      
                                            (:type :env-macro) 
                                            (:macro-for ,(second (assoc function ts::*nrql-with-macros*)))
                                    
                                            (:in-sections ,@(get-sections function))

                                            (:protected t)
      
                                            (:signature ,(ensure-list (get-lambda function)))

                                            (:arguments 
                                             ,@args)

                                            (:see-also 
                                             ,@(cdr (assoc :see-also
                                                           (third (assoc function ts::*nrql-with-macros*)))))

                                            (:description ,(get-sofia-description function))

                                            (:remarks nil)

                                            (:examples nil))

                            stream)
        
                    (terpri stream))))

              ;;;
              ;;; OWLAPI Synonym 
              ;;; 

              (dolist (function (remove-duplicates (cdr (get-owlapi-synonyms)) 
                                                   :key #'caadr))

                (let ((name (first (second function)))
                      (for (second (second (fourth function)))))

                  (pushnew name *all-racer-functions*)

                  (unless only-collect-p

                    (pprint `(add-doc-entry (:title ,name)
                                            (:label ,name)
                                            (:type :synonym-function) 

                                            (:in-sections ,@(get-sections for))

                                            (:protected t)
      
                                            (:synonym-for ,for))
                            stream)
        
                    (terpri stream)))))

            (format stream "~%))~%")
            ))))))
  
;;;
;;;
;;;

(defun create-html-doc ()
  
  (setf *missing-argument-types* nil
        *unknown-return-types* nil
        *unknown-macros-for-functions* nil
        *unknown-functions-for-macros* nil
        *missing-type-descriptions* nil)

  (load "nrql-dev:racer-registry3.lisp")
  (create-docgen-stubs)
  (load "nrql-dev:docentries-only.lisp")
  (load "nrql-dev:docgen-stubs.lisp")

  (pprint 
   `(:missing-argument-types
     ,*missing-argument-types*
     :unknown-macros-for-functions
     ,*unknown-macros-for-functions*
     :unknown-functions-for-macros*
     ,*unknown-functions-for-macros*
     :unknown-return-types
     ,*unknown-return-types*
     :missing-type-descriptions
     ,*missing-type-descriptions*)))
