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
;;;
;;;

(defvar *sym-count* 0)

(defconstant +secret-prefix+ 'secret-cdsc7897qcjk)

(defun create-marker (&optional sym (new-p t))
  (if sym       
      (if new-p 
          (intern (format nil "~A-~A-~A" +secret-prefix+ sym (incf *sym-count*)) :cl-user)
        (intern (format nil "~A-~A" +secret-prefix+ sym)  :cl-user))
    (intern (format nil "~A-~A" +secret-prefix+ (incf *sym-count*))  :cl-user)))

(defvar +secret-abox+ (create-marker 'temp-abox))

;;;
;;;
;;;

(defconstant +equal-roles+ '(same-as nrql-equal-role))

(defun is-equal-role-p (role)
  (let ((role (if (and (consp role)
                       (eq (first role) 'inv))
                  (second role) 
                role)))
    (member role +equal-roles+)))

(defconstant +different-from-roles+ '(different-from nrql-different-role))

(defun is-different-from-role-p (role)
  (let ((role (if (and (consp role)
                       (eq (first role) 'inv))
                  (second role) 
                role)))
    (member role +different-from-roles+)))

;;;
;;;
;;; 

(defconstant +top-roles+ (list racer:+top-object-role-symbol+))

(defun is-top-role-p (role)
  (let ((role (if (and (consp role)
                       (eq (first role) 'inv))
                  (second role) 
                role)))
    (member role +top-roles+)))

(defconstant +bottom-roles+ (list racer:+bottom-object-role-symbol+))

(defun is-bottom-role-p (role)
  (let ((role (if (and (consp role)
                       (eq (first role) 'inv))
                  (second role) 
                role)))
    (member role +bottom-roles+)))

;;;
;;;
;;; 

(defconstant +reject-tuple+ :reject-12312321)

;;;
;;;
;;;

(defvar *last-query* nil)

(defvar *last-query-id* nil)

(defvar *last-rule-id* nil)

#-:dlmaps 
(defvar *cur-substrate* nil)

#+:midelora
(defvar *type-of-substrate* 'midelora-substrate)

#-:midelora
(defvar *type-of-substrate* 'racer-dummy-substrate)

;;;
;;; 
;;; 

(defvar *rcc-type* :rcc8)

(defvar *rcc-synonyms* nil)


;;;
;;; Prozesse
;;;

(defvar *lock* nil)

;;;
;;;
;;;

(defvar *racer-check-if-atoms-defined-p*  t)

(defvar *treat-as-abox-inds*  nil)

(defvar *treat-as-substrate-inds*  nil)

(defvar *established-bindings*  nil)

;;;
;;; werden von "with-nrql-settings" gesetzt: 
;;; (verhindert, dass set-current-tbox/abox verwendet werden muss!)
;;; 

(defvar *nrql-abox* nil)

(defvar *nrql-tbox* nil)

;;;
;;; Waehrend der Query Execution: 
;;; 

(defvar *running-abox* nil)

(defvar *running-tbox* nil)

(defvar *running-query* nil)

(defvar *running-substrate* nil)

(defvar *previous-conjunct* nil)

(defvar *candidates* nil)

;;;
;;;
;;;

(defvar *ano-counter* 0)

(defvar *keep-original-var-name-in-ano-vars-p* nil)

(defvar *iterator-id* 0)

(defvar *process-id* nil) ; fuer with-critical-section

(defvar *multiprocess-queries* 
  #-:multiprocess-queries nil
  #+:multiprocess-queries t)  

(defvar *disable-deadlock-checking* nil) 

;;;
;;; Process Pool 
;;;

(defvar *process-pooling* 
  #+:process-pooling t
  #-:process-pooling nil)

;;;
;;;
;;;


(defparameter *min-pool-size* 20)

(defparameter *max-pool-size* 50)

;;;
;;;
;;;

(defvar *pool-counter* 0)

(defvar *all-processes* nil)

(defvar *process-pool* nil)

;;;
;;; MiniLisp 
;;;

(defvar *sym-counter* 0)

(defvar *registered-functions* (make-hash-table))

(defvar *registered-values* (make-hash-table))

(defvar *registered-server-functions* (make-hash-table))

(defvar *registered-server-values* (make-hash-table))

(defvar *call-stack* nil)

(defparameter *debug-evaluator-p* nil)

(defconstant +allowed-cl-functions+  (make-hash-table))

(defparameter +native-minilisp-functions+  nil) ; Liste

;;;
;;;
;;;

(defvar *server-hooks* nil)

;;;
;;;
;;;

(defvar *all-queries* nil)

(defvar *all-rules* nil)

(defvar *ready-queries* nil)

(defvar *ready-rules* nil)

(defvar *active-queries* nil) ; deren Prozess noch lebt! -> unterteilt in running und waiting! 

(defvar *active-rules* nil) 

(defvar *processed-queries* nil)

(defvar *processed-rules* nil)

;;;
;;;
;;;

#-:ccl
(defconstant +sleep-time+ 0.01) 

#+:ccl 
(defconstant +sleep-time+ 2) ; must be fixnum: 2/60 = 1/30 = 0.03 seconds

;;;
;;;
;;;

(defparameter *allow-negated-roles-p* t) ; Queries der Art (?*x ?*y (NOT R)) erlaubt? 

(defparameter *toggle-una-variables-p* t) ; wenn T, werden UNA-Vars zu NON-UNA-VARS, und andersrum 

(defparameter *use-new-syntax-for-unparse-p* nil) ; not -> neg, or -> union, nur f. API-Funktionen describe-query

;;;
;;;
;;;

(defparameter *add-role-assertions-for-datatype-properties-p* nil)

;;; 
;;; bestimmt, ob fuer concept assertions, 
;;; die durch OWL datatype properties entstehen, wie z.B.
;;;
;;; (SOME |http://www.owl-ontologies.com/unnamed.owl#age| (EQUAL RACER-INTERNAL%HAS-INTEGER-VALUE 40))
;;;
;;; Rollen-Nachfolger angelegt werden, damit ":constraint"-Query Atoms verwendet werden koennen
;;;

(defparameter *check-abox-consistency-p* t)

(defparameter *use-individual-synonyms-p* nil) ;;; wenn T werden SYNONYME Individuen als GLEICH betrachtet! 
;;;
;;;
;;;

(defparameter *tuple-at-a-time-p* nil)

(defparameter *proactive-tuple-computation-p* t)

;;;
;;;
;;;

(defparameter *two-phase-processing-p* nil) 

(defparameter *deliver-phase-two-warning-tokens-p* nil)

(defparameter *deliver-kb-has-changed-warning-tokens-p* t)

;;;
;;; Explanations
;;;

(defparameter *record-explanations-p* nil)

(defparameter *explanations* nil)

;;;
;;;
;;;

(defparameter *initial-abox-mirroring-p* nil) ; sollen die Caches beim Erzeugen des Substrates gefuellt werden?  

(defparameter *initial-role-assertion-mirroring-p* t) ; wenn T, kann initial-abox-mirroring-p = NIL ueberschreiben, dann werden nur die Role Assertions gespiegelt! wichtig fuer LUBM etc. 

(defparameter *ensure-tbox-classification-p* nil) ; nur in Kombination mit *initial-tbox-mirroring-p* 
  
(defparameter *told-information-reasoning-p* nil) ; soll auf Racer-Reasoning verzichtet werden? INCOMPLETE!

(defparameter *classify-concepts-in-instance-assertions-p* nil) ; sollen auch komplexe C in i : C TBox-klassifiziert werden? -> very smart Abox mirror

;;;
;;;
;;;

(defparameter *optimize-p* t)

(defparameter *optimizer-max-plans* 1000)

(defparameter *optimizer-spend-at-most-n-seconds* 3)

(defparameter *optimizer-use-cardinality-heuristics-p* t)

(defparameter *optimizer-ensure-late-lambda-evaluation-p* t)

(defparameter *generate-code-p* t)
 
#+:only-runtime-evaluation
(progn   
  (defparameter *compile-queries-p* nil)
  
  (defparameter *compile-inline-p* nil)

  (defparameter *runtime-evaluation-p* t))


#-:only-runtime-evaluation
(progn 
  (defparameter *compile-queries-p* t)
  
  (defparameter *compile-inline-p* nil)

  (defparameter *runtime-evaluation-p* nil))

;;;
;;; 
;;;

(defparameter *warnings-p* t)

(defvar *really-warn-p* nil)

;;;
;;;
;;;

(defparameter *rewrite-to-dnf-p* t)

(defparameter *rewrite-semantically-p* t)

(defparameter *rewrite-defined-concepts-p* nil)

;;;
;;; 
;;;

(defparameter *report-inconsistent-queries-p* nil)

(defparameter *report-tautological-queries-p* nil)

;;;
;;;
;;;

(defparameter *use-repository-p* nil)

(defparameter *put-into-repository-p* nil)

(defparameter *syntactic-repository-p* nil)

;;;
;;;
;;;

(defparameter *use-unique-name-assumption-p* t) ; NICHT redundant! damit kann man UNA-VOIS temporaer "abschalten" 
;;; hat nichts mit racer::*use-unqiue-name-assumption* zu tun!!!

(defparameter *exclude-permutations-p* nil)

;;;
;;;
;;;

(defparameter *continuation-based-instance-retrieval-p* nil)


;;;
;;;
;;;

(defparameter *timeout* nil) ; hat keine Relevanz f. in Racer eingebautes nRQL! racer::*server-timeout*! 

(defvar *how-many* nil)

(defvar *only-new-tuples-p* nil) ; f. Subscribe Modus 

(defvar *dont-show-variables* nil)

(defvar *dont-show-head-projections-operators-p* nil)

(defvar *dont-show-lambdas-p* nil)

;;;
;;; 
;;; 

(defvar *queries-started* nil) ; damit nach timeout gestartete subprozesse / queries gekillt werden koennen

;;;
;;; defined queries
;;; 

(defvar *all-dboxes* nil)

(defvar *allow-multiple-definitions-p* nil)

(defvar *always-prefer-defined-queries-p* nil)

(defvar *disable-defined-queries-p* nil)

(defvar *keep-expanded-predicates-p* nil)

(defvar *lazy-query-unfolding-p* nil)

;;;
;;;
;;;

(defvar *auto-add-top-conjuncts-p* t) ; (retrieve (?x ?y) (?x c)) -> (and (?x c) (top ?y)) 

;;;
;;;
;;;

(defparameter *add-rule-consequences-p* t) ; f.d. Rules

(defparameter *dont-add-abox-duplicates-p* nil) 

(defparameter *remove-duplicates-p* t)

#|

;;;
;;; diese werden vom persitency manager gedumpt: 
;;;

(defconstant +nrql-specials+
  '(*all-substrates*
    *all-dboxes*
    *all-queries*
    *all-rules*
    *ready-queries*
    *ready-rules*
    ;; *active-queries* koennen nicht gedumpt werden!
    ;; *active-rules*   weil Prozesse laufen!
    *processed-queries* 
    *processed-rules* 
    *cur-substrate*
    *last-query*
    *type-of-substrate*
    *nrql-abox*
    *nrql-tbox*
    *ano-counter* 
    *iterator-id*
    *process-id*
    *sym-count*
    *lock*
    *racer-check-if-atoms-defined-p*
    *multiprocess-queries*
    *use-individual-synonyms-p*
    *process-pooling*
    *min-pool-size*
    *max-pool-size*
    *pool-counter*
    *allow-negated-roles-p*
    *check-abox-consistency-p*
    *tuple-at-a-time-p*
    *proactive-tuple-computation-p*
    *two-phase-processing-p*
    *deliver-phase-two-warning-tokens-p*
    *initial-abox-mirroring-p*
    *initial-role-assertion-mirroring-p* 
    *ensure-tbox-classification-p*
    *told-information-reasoning-p*
    *classify-concepts-in-instance-assertions-p*
    *optimize-p*
    *generate-code-p*
    *compile-queries-p*
    *compile-inline-p*
    *runtime-evaluation-p*
    *warnings-p*
    *rewrite-to-dnf-p*
    *rewrite-semantically-p*
    *report-inconsistent-queries-p*
    *report-tautological-queries-p*
    *use-repository-p*
    *put-into-repository-p*
    *syntactic-repository-p*
    *use-unique-name-assumption*
    *exclude-permutations-p*
    *timeout*
    *how-many*
    *add-rule-consequences-p*
    *dont-add-abox-duplicates-p*
    *add-role-assertions-for-datatype-properties-p*
    ))


|#

