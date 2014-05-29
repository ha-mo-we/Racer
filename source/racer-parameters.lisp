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

;;;===========================================================================

(racer-defparameter *dl-prover-name* (get-product-name))

(racer-defparameter *dl-prover-version* (get-product-version))

(defparameter *svn-revision* nil)

;;;===========================================================================
;;; RACER server parameters
;;;===========================================================================

(defparameter *server-timeout* nil)
(defparameter *unsafe-mode* 
  #+:racer-with-sirius t
  #-:racer-with-sirius nil)

(defconstant +dig-1-1+ :dig-1-1)
(defconstant +dig-1-1-racer+ :dig-1-1-racer)
(defconstant +dig-1-2+ :dig-1-2)
(defparameter *dig-version* +dig-1-2+)

(defparameter *use-owllink-interface* nil)
(defparameter *use-owllink2-interface* nil)


;;;===========================================================================
;;; OWL parameters
;;;===========================================================================

(defconstant +owl-version+ "http://www.w3.org/2002/07/owl#")
(defconstant +owl2-version+ "http://www.w3.org/2002/07/owl#")
(defconstant +swrl-version+ "http://www.w3.org/2003/11/swrl#")
(defconstant +swrlb-version+ "http://www.w3.org/2003/11/swrlb#")
(defconstant +swrlx-version+ "http://www.w3.org/2003/11/swrlx#")

(defconstant +swrl-url+ "http://www.daml.org/rules/proposal/swrl.owl")
(defconstant +swrlb-url+ "http://www.daml.org/rules/proposal/swrlb.owl")
(defconstant +swrlx-url+ "http://www.daml.org/rules/proposal/swrlx.owl")
(defconstant +protege-url+ "http://protege.stanford.edu/plugins/owl/protege")

(defconstant +owllink-url-prefix+ "http://www.owllink.org/owllink-xml#")

(defconstant +owl-thing+ (concatenate 'string +owl-version+ "Thing"))
(defconstant +owl-nothing+ (concatenate 'string +owl-version+ "Nothing"))

(defconstant +owl-top-data-role+ 
  (intern (concatenate 'string +owl-version+ "topDataProperty") :racer-user))
(defconstant +owl-bottom-data-role+ 
  (intern (concatenate 'string +owl-version+ "bottomDataProperty") :racer-user))
(defconstant +owl-top-object-role+
  (intern (concatenate 'string +owl-version+ "topObjectProperty") :racer-user))
(defconstant +owl-bottom-object-role+ 
  (intern (concatenate 'string +owl-version+ "bottomObjectProperty") :racer-user))

;;;
;;;
;;;

(defparameter +ignored-meta-ontologies+ 
  (list +swrl-version+
        +swrlx-version+
        +swrlb-version+
        +swrl-url+
        +swrlb-url+
        +swrlb-url+
        "http://www.owl-ontologies.com/2005/08/07/xsp.owl"
        "http://swrl.stanford.edu/ontologies/built-ins/3.3/swrlx.owl"
        "http://swrl.stanford.edu/ontologies/built-ins/3.4/swrlm.owl"
        "http://www.w3.org/2003/11/swrlb"
        "http://swrl.stanford.edu/ontologies/built-ins/3.3/temporal.owl"
        "http://swrl.stanford.edu/ontologies/built-ins/3.3/tbox.owl"
        "http://sqwrl.stanford.edu/ontologies/built-ins/3.4/sqwrl.owl"
        "http://swrl.stanford.edu/ontologies/built-ins/3.3/abox.owl"
        "http://www.w3.org/2003/11/swrl"
        "http://swrl.stanford.edu/ontologies/3.3/swrla.owl"))

;;;===========================================================================
;;; RACER parameters
;;;===========================================================================

(defparameter *tbox-verbose* t)         ; print info about concept synonyms and incoherence
(defparameter *abox-verbose* nil)         ; print info about incoherent ABox
(defparameter *abox-clash-verbose* nil)         ; print trace back of clash dependencies
(defvar *use-less-tbox-memory*)         ; use lean TBox for less memory consumption (initialized from tbox)
(defparameter *prevent-lean-tbox* nil)          ; if T disables use-less-tbox-memory
(defparameter *enforce-lean-tbox-threshold* 20000)      ; number of axioms to enforce use-less-tbox-memory
(defparameter *always-use-lean-tbox* nil)       ; if T always enforces use-less-tbox-memory
(defparameter *use-less-abox-memory* nil)       ; use lean ABox for less memory consumption
(defparameter *debug* nil)              ; enables printing of debugging info

(defparameter *prefix-mappings* nil)  ; for OWL prefixes
(defparameter *cached-prefixes* nil)  ; used in the OWL interface
(defparameter *uri-mirror-table* (make-hash-table :test #'equal)) ; for uri mirring


(racer-defparameter *backjumping* t)          ; enables use dependency-directed backtracking
(racer-defparameter *use-signature-backjumping* t) ; enables use dependency-directed backtracking for signature calculus
(racer-defparameter *use-refined-signature-clash-dependencies* nil) ; enables backjumping with refined clash dependencies where clashes are traced back to qualifications of signatures

(racer-defparameter *derived-models* t)       ; derive models from already known models
(racer-defparameter *model-merging* t)        ; enables use of models
(defparameter *subtableaux-model-merging* t)    ; enables use of models for subtableaux
(defparameter *deep-model-merging* t)   ; enables use of deep models
(racer-defparameter *deep-cd-model-merging* t)        ; merge cd-predicates in models
(racer-defparameter *abox-model-merging* t)   ; enables use of models for individuals
(racer-defparameter *use-alternate-models* nil)         ; create and use alternate pseudo models for merging
(racer-defparameter *use-alternate-ind-models* nil)         ; create and use alternate ind pseudo models for merging
(racer-defparameter *max-no-of-alternate-models* 5)   ; length of list of alternative models
(racer-defparameter *use-elh-model-embedding* t)        ; enables structural ELH subsumption via pseudo models

(racer-defparameter *smart-realization* t)    ; use binary partition scheme for abox realization
(racer-defparameter *use-subsumption-based-instance-retrieval* nil)     ; enforce only classification of tbox for concept instances
(racer-defparameter *use-realization-based-instance-retrieval* nil)   ;  enforce realization of abox for concept instances
(racer-defparameter *use-binary-instance-retrieval* t)        ; use binary partition scheme for instance retrieval
(racer-defparameter *use-dependency-based-instance-retrieval* t) ; use dependency-directed instance retrieval (overwrites *use-binary-instance-retrieval*)

(racer-defparameter *keep-cd-state-minimal* t)        ; do not add redundant predicates to cd-state

(defparameter *tableaux-caching* t)         ; enables caching of expanded subtableaux
(defparameter *tableaux-sat-caching* t)
(defparameter *tableaux-unsat-caching* t)
(racer-defparameter *tableaux-cached-models* nil)       ; enables caching of models
(racer-defparameter *use-equal-tableaux-cache* t)   ; use equal hash table cache
(racer-defparameter *use-subset-superset-cache* t)    ; use subset/superset cache
(racer-defparameter *merging-partitions-caching* t)   ; enables caching of merged exists partitions

(defparameter *taxonomic-encoding* t)   ; enables reuse of encoded concept names

(defparameter *simplify-and-or* t)      ; simplify nested and/or terms

(racer-defparameter *max-or-list-threshold* 40) ; maximal no. of unexpanded ORs that will be processed
(racer-defparameter *max-some-list-threshold* 40) ; maximal no. of unexpanded SOMEs that will be processed

(racer-defparameter *optimize-disjoint-ands* t)
(racer-defparameter *disjoint-ands-threshold* 50)

(racer-defparameter *jw-selection* t)          ; use JW weights for selecting disjuncts
(racer-defparameter *use-success-based-disjunct-selection* t) ; select a disjunct based on the success rate of previous semantic splits
(racer-defparameter *always-use-success-based-disjunct-selection* nil)
(racer-defparameter *randomized-success-based-disjunct-selection* nil) ; if score is even for semantic split use a random split
(racer-defparameter *disjunct-record-history-size* 100) ; max number of saved history entries recording success rate of semantic splits
(racer-defparameter *disjunct-record-sampling-rate* 100) ; least number of changes before a history record is added

(racer-defparameter *transform-gcis* t)       ; do enhanced transformations on concepts and GCIs
(racer-defparameter *absorb-domains-ranges* t)        ; absorb corresponding GCIs
(racer-defparameter *absorb-gcis* t)          ; do standard GCI transformation
(racer-defparameter *use-nary-absorption* t)
(racer-defparameter *use-elh-transformation* t) ;enables polynomial EL+ procedure
(racer-defparameter *use-inverse-role-absorption* nil)
(defparameter *record-gci-absorption-dependencies* t)

(racer-defparameter *use-optimized-tbox-traversal* t)         ; avoid parents of bottom
(racer-defparameter *use-optimized-tbox-traversal-no-bottom-search* t)        ; special case if bottom search is skipped
(racer-defparameter *use-completely-defined-tbox-traversal* t)          ; parents of completely defined concepts are the names occurring in the encoded definition

(racer-defparameter *use-simplex* t)          ; use simplex method instead of signature calculus
(racer-defparameter *use-simplex-for-individuals* nil)        ; use simplex method also for old inds
(racer-defparameter *always-use-simplex* nil)         ; enforces to always use simplex instead of signature calculus
(racer-defparameter *simplex-use-sparse-arrays* t)    ; use sparse arrays implementation for simplex

(racer-defparameter *cd-use-sparse-arrays* t)       ; use sparse arrays implementation for concrete domains

(racer-defparameter *signature-merging* t)    ; use optimized signature merging calculus
(racer-defparameter *signature-partitioning* t)       ; use partitioning scheme

(racer-defparameter *propagate-told-disjoints* nil) ; transitively propagate asserted/told disjoints of concepts
                                                    ; disabled because it is too expensive if many disjoint concepts exist
(racer-defparameter *propagate-told-subsumers* t)     ; make use of told subsumers of concepts

(defparameter *abox-partitioning-p* t)              ; enables the decomposition of an ABox into subgraphs

(defparameter *sorted-concept-list* t)      ; sort concepts for classification in definition order
(racer-defparameter *always-do-bottom-search* nil)    ; do tbox bottom search even if it may be skipped
(racer-defparameter *random-concept-list* nil)        ; sort concepts in random order for classification

(racer-defparameter *tbox-clustering* t)      ; cluster children of concepts
(racer-defparameter *tbox-clustering-upto-n-concepts* 50000)      ; number of concepts up to which tbox clustering is applied (it is memory-intensive)
(racer-defparameter *bucket-size-threshold* 10)       ; minimal size of clusters
(racer-defparameter *merge-buckets* t)        ; merge existing buckets to larger ones
(racer-defparameter *merge-buckets-threshold* 15)     ; start bucket merging if >=

(racer-defparameter *gc-of-obsolete-individuals* t)   ; remove constraints / labels of obsolete individuals

(racer-defparameter *prefer-backpropagated-exists* t)         ; weaken depth-first search in favor of backpropagted exists

(racer-defparameter *depth-first-for-inverse-roles* t)        ; exists constraints of newer inds have higher priority
(racer-defparameter *breadth-first-for-inverse-roles* nil)    ; process exists constraints in breadth search

(racer-defparameter *use-new-role-hierarchy-classification* t); enables role hierarchy construction using concept classification

;;; the following 3 parameters are dynamically set to T in test-abox-satisfiable
;;; setting them to T will enforce to use the stores everywhere and not only in Aboxes
;;; this is only used for testing purposes and not useful otherwise
(defparameter *use-expanded-store* nil)   ; expanded constraints store provides almost direct access to expanded constrains for individuals
(defparameter *use-unexpanded-exists-constraints-store* nil)   ; unexpanded exists constraints store provides almost direct access to unexpanded constraints for individuals
(defparameter *use-unexpanded-disjunctive-constraints-store* nil)   ; unexpanded or constraints store  provides almost direct access to unexpanded constraints for individuals

;;; the following 3 parameters are switched off in debug mode in order 
;;; to be able to enforce the use of the stores even outside of Aboxes
(racer-defparameter *abox-use-expanded-store* t)   ; use expanded constraints store for Aboxes?
(racer-defparameter *abox-use-unexpanded-exists-constraints-store* t)   ; use unexpanded exists constraints store for Aboxes?
(racer-defparameter *abox-use-unexpanded-disjunctive-constraints-store* t)   ; use unexpanded or constraints store for Aboxes?
(racer-defparameter *use-abox-stores-ind-ass-threshold* 1000)
(racer-defparameter *use-abox-stores-role-ass-threshold* 1000)

(defun enable-abox-stores-p (ind-ass-length role-ass-length)
  (or (>= ind-ass-length *use-abox-stores-ind-ass-threshold*)
      (>= role-ass-length *use-abox-stores-role-ass-threshold*)))

(defun enable-constraint-stores ()
  (setf *abox-use-expanded-store* t)
  (setf *abox-use-unexpanded-exists-constraints-store* t)
  (setf *abox-use-unexpanded-disjunctive-constraints-store* t))

(defun disable-constraint-stores ()
  (setf *abox-use-expanded-store* nil)
  (setf *abox-use-unexpanded-exists-constraints-store* nil)
  (setf *abox-use-unexpanded-disjunctive-constraints-store* nil))

(defconstant +unused-constraint-store+ ':unused)
(defparameter *swap-to-expanded-store-threshold* 500)          ; threshold to empty list of constraints into constraint store
(defparameter *cache-size-fraction-from-constraint-store* 0.2)        ; fraction of *swap-to-constraint-store-threshold* cached into constraint list
(defparameter *swap-to-unexpanded-store-threshold* 500)          ; threshold to empty list of constraints into constraint store
(defparameter *cache-size-from-constraint-store-threshold* 500)          ; threshold to empty list of constraints into constraint store

(defun get-cache-size-from-constraint-store ()
  (round (* *swap-to-unexpanded-store-threshold*
            *cache-size-fraction-from-constraint-store*)))

(defparameter *cache-size-from-constraint-store*
  (get-cache-size-from-constraint-store))       ; no of constraints that are added to constraint list and removed from constraint store

(defun set-constraint-store-thresholds (swap-to-store-threshold)
  (setf *swap-to-expanded-store-threshold* swap-to-store-threshold)
  (setf *cache-size-from-constraint-store* (get-cache-size-from-constraint-store))
  (values *swap-to-expanded-store-threshold* *cache-size-from-constraint-store*))

(racer-defparameter *merge-constraint-store-tables-p* nil)          ; if T keep only one table for the constraint store
(racer-defparameter *max-constraint-stores-length* 100000)
(racer-defparameter *use-expanded-stores-index* t)

(racer-defparameter *use-relation-store* t)   ; relation store provides almost direct access to relations constrains for individuals
(racer-defparameter *always-use-relation-store* nil)
(racer-defparameter *use-relation-store-threshold* 200)         ; min. no. of relation constraints to use relation store

(defun use-relation-store-p (relation-store
                                 &optional
                                 (using-precompletion t)
                                 (relation-constraints nil))
  (or *always-use-relation-store*
      (and *use-relation-store*
           (if using-precompletion
             (when relation-store
               t)
             (when relation-constraints
               (> (length relation-constraints) *use-relation-store-threshold*))))))

(racer-defparameter *use-abox-precompletion* t)       ; start ABox test with saved precompletion
(racer-defparameter *use-abox-completion* t)       ; start ABox test with saved completion
(racer-defparameter *delay-abox-exists* t)      ; process exists constraints after other constraints
(racer-defparameter *ignore-abox-redundant-exists* t)   ; ignore exists constraint if matched by relation constraint
(racer-defparameter *optimize-datatype-role-fillers* t) ; simplified processing for OWL datatype properties

(racer-defparameter *recycle-hash-tables* nil) ;use termination facilities to collect and reuse unreferences hash tables
(defparameter *obsolete-table-misses-threshold* 500)
(defvar *no-of-obsolete-table-misses* 0)
(racer-defparameter *recycle-kernel-states* nil) ;use termination facilities to collect and reuse unreferences kernel states
(defparameter *obsolete-basic-kernel-state-misses-threshold* 2000)
(defvar *no-of-obsolete-basic-kernel-state-misses* 0)
(racer-defparameter *recycle-constraints* nil)
(defparameter *obsolete-constraint-misses-threshold* 2000)
(defvar *no-of-obsolete-concept-constraint-misses* 0)
(defvar *no-of-obsolete-or-constraint-misses* 0)
(defvar *no-of-obsolete-relation-constraint-misses* 0)

(defun reset-recycle-counts ()
  (setf *no-of-obsolete-table-misses* 0)
  (setf *no-of-obsolete-basic-kernel-state-misses* 0)
  (setf *no-of-obsolete-concept-constraint-misses* 0)
  (setf *no-of-obsolete-or-constraint-misses* 0)
  (setf *no-of-obsolete-relation-constraint-misses* 0))

(defparameter *allocated-kbs* (make-hash-table :test #'equal))

(defvar *collected-dependencies*)
(defvar *collected-ind-dependencies*)

(defvar *inverse-roles* nil)              ; enables correct treatment of inverse roles
(defvar *blocking-possibly-required*)   ; if T blocking test must be applied

(defvar *flatten-encoded-concepts* nil)
(defvar *set-language-of-atomic-concepts* t)
(defvar *enforce-reencoding* nil)

(defmacro without-taxonomic-encoding (&body body)
  `(let ((*taxonomic-encoding* nil))
     . ,body))

(defmacro without-setting-language-of-atomic-concepts (&body body)
  `(let ((*set-language-of-atomic-concepts* nil))
     . ,body))

(defmacro with-flatten-encodings (&body body)
  `(let ((*flatten-encoded-concepts* t))
     . ,body))

(defmacro with-enforced-reencoding (&body body)
  `(let ((*enforce-reencoding* t))
     . ,body))

(defvar *optimize-to-bottom-during-encoding* t)
(defvar *simplify-redundant-at-most* t)

(defmacro without-optimized-encoding (&body body)
  `(let ((*optimize-to-bottom-during-encoding* nil)
         (*simplify-and-or* nil)
         (*simplify-redundant-at-most* nil))
     . ,body))

(defvar *ignore-role-domain-range* nil)

(defvar *in-precompletion* nil)         ; no exists, at-most or or-constraints processed yet
(defvar *using-precompletion* nil)

(defmacro with-ignored-role-domain-range-encodings (&body body)
  `(let ((*ignore-role-domain-range* t))
     . ,body))

(defmacro with-unique-name-assumption (&body body)
  `(let ((*use-unique-name-assumption* t))
     . ,body))

(defmacro without-unique-name-assumption (&body body)
  `(let ((*use-unique-name-assumption* nil))
     . ,body))


(defun set-unique-name-assumption (value)
  (setf *use-unique-name-assumption* value))

;;; ======================================================================

(defconstant +has-integer-value+ 'racer-internal%has-integer-value)
(defconstant +has-string-value+ 'racer-internal%has-string-value)
(defconstant +has-real-value+ 'racer-internal%has-real-value)
(defconstant +has-boolean-value+ 'racer-internal%has-boolean-value)

(defconstant +internal-roles+ (list +has-string-value+ +has-integer-value+ 
                                       +has-real-value+ +has-boolean-value+))
(defconstant +internal-property-datatypes+ '(string integer real boolean))
(defconstant +internal-role-type+ (mapcar #'cons
                                              +internal-roles+
                                              +internal-property-datatypes+))

(defun internal-role-p (role)
  (member role +internal-roles+))

(defun booleanp (value)
  (or (eq value 't) (null value)))

;;;===========================================================================

(defvar *or-level*)                     ; actual depth of nested or-expansions

(defvar *dl-prover-language*)           ; current ALC language dialect

(defvar *meta-constraint-concepts*)     ; current meta constraints of TBox

(defvar *subsumption-tests*)            ; expensive subsumption tests
(defvar *old-subsumption-tests* 0)

(defvar *signature-clash-culprits*)     ; number restriction clash culprits
(defvar *signature-clash-reasons*)      ; clash culprits in case of an unsatisfiable exists
                                            ; constraint derived from a signature

(defvar *catching-clash-dependencies* nil
  "Used to store the clash dependencies if a clash is detected.
   The dependencies are processed in partially-expanded-or-satisfiable.
   They determine whether tried or alternatives must be continued 
   during recursive fallback or not (dependency-directed backtracking).")

(defparameter *test-with-debug* nil)

(defvar *subsumed-concept-p*)           ; dynamically bound to corresponding function
(defvar *subsumes-concept-p*)
(defvar *concept-clash-p*)
(defvar *matching-constraint-test*)
(defvar *find-matching-constraint*)

(defvar *concept-changed-p*)            ; flag used during concept propagation

(defvar *current-abox* nil)
(defvar *current-tbox* nil)             ; reference to current TBox

(defvar *role-store*)

(defvar *concept-store*)
;;; Used to ensure eq-ness of concept terms. It defines a mapping from
;;; concepts represented as lists of encoded concepts to concepts encoded as
;;; structures. Eq-ness of concept terms is used to ensure fast clash detection
;;; by avoiding structure traversals (see clash-in-cs-p).

(defvar *concrete-domain*)

(defvar *tableaux-cache*)

(defvar *provisionally-inserted-atomic-concepts*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +racer-structure-id-counter-init+ 10
    "Initial value for uniquely identifying structures"))

;;;===========================================================================

(defvar *taxonomic-encoding-dependencies*)

;;;===========================================================================

(defvar *top-concept*)                  ; The top concept
(defvar *bottom-concept*)               ; The bottom concept

(defvar *datatype-top-concept*)         ; The top datatype concept
(defvar *datatype-bottom-concept*)      ; The bottom datatype concept

(defconstant +ind-counter-init+ 0)      ; initial value for *ind-counter*
(defvar *ind-counter*)                  ; for unique constraint ind ids

(defvar *visited-concepts* nil)

;;;===========================================================================

(defvar *simplex-labels*)
(defvar *saved-ind*)
(defvar *saved-relation-constraints*)
(defvar *saved-state*)

(defmacro with-alc-bindings (&body body)
  `(let ((*catching-clash-dependencies* nil)
         (*or-level* 0)
         (*dl-prover-language* (if *use-tbox*
                                   (tbox-language *use-tbox*)
                                 *dl-empty*))
         (*concept-changed-p* nil)
         (*simplex-labels* nil)
         (*saved-ind* nil)
         (*saved-relation-constraints* nil)
         (*saved-state* nil)
         (*signature-clash-reasons* nil)
         (*tableaux-unsat-caching*
          (if (consp *tableaux-unsat-caching*)
              *tableaux-unsat-caching*
            (list *tableaux-unsat-caching*))))
     ,@body))

;;;===========================================================================

(defvar *stable-set-difference-table*)
(defvar *stable-set-difference-last-list2*)
(defvar *racer-remove-duplicates-table*)
(defvar *racer-remove-constraint-duplicates-table*)
(defvar *possible-subsumees*)
(defvar *expanded-constraints-ind-table*)
(defvar *live-inds-table*)
(defvar *obsolete-inds-table*)
(defvar *label-inds-table*)
(defvar *new-inds-table*)
(defvar *concept-set-mark*)
(defvar *role-set-mark*)
(defvar *individual-set-mark*)
(defvar *constraint-set-mark*)
(defvar *tbox-classification-counter*)
(defvar *obsolete-eql-tables*)
(defvar *obsolete-equal-tables*)
(defvar *obsolete-equal-concept-tables*)
(defvar *initial-hash-table-size*)
(defvar *signatures-equal-table*)
(defvar *partitions-table*)
(defvar *set-vector*)
(defvar *select-disjunct-table*)

;;;===========================================================================

;;;temporary hack to ensure tail merging by switching of dynamic extent
#|
(defmacro with-ignored-dynamic-extent-declarations (&body body)
  #+:allegro
  `(excl::compiler-let
     ((compiler:trust-dynamic-extent-declarations-switch nil))
     ,@body)
  #-:allegro
  `(let ()
     ,@body))
|#

(defconstant +racer-specials+ 
  (remove-if #'constantp
             '( *unsafe-mode* 
                *tbox-verbose*
                *abox-verbose*
                *abox-clash-verbose* 
                *prevent-lean-tbox*
                *enforce-lean-tbox-threshold*
                *always-use-lean-tbox*
                *use-less-abox-memory* 
                *debug* 
                *backjumping*
                *use-signature-backjumping*
                *use-refined-signature-clash-dependencies*
                *derived-models*
                *model-merging*
                *subtableaux-model-merging*
                *deep-model-merging*
                *deep-cd-model-merging* 
                *abox-model-merging*
                *use-alternate-models*
                *use-alternate-ind-models*
                *max-no-of-alternate-models*
                *use-elh-model-embedding*
                *smart-realization*
                *use-subsumption-based-instance-retrieval*
                *use-realization-based-instance-retrieval*
                *use-binary-instance-retrieval*
                *use-dependency-based-instance-retrieval*     
                *keep-cd-state-minimal*     
                *tableaux-caching*
                *tableaux-sat-caching*
                *tableaux-unsat-caching*
                *tableaux-cached-models*
                *use-equal-tableaux-cache*
                *use-subset-superset-cache*
                *merging-partitions-caching*
                *taxonomic-encoding*
                *simplify-and-or*
                *max-or-list-threshold*
                *max-some-list-threshold* 
                *optimize-disjoint-ands*
                *disjoint-ands-threshold*
                *jw-selection*
                *use-success-based-disjunct-selection* 
                *always-use-success-based-disjunct-selection*
                *randomized-success-based-disjunct-selection* 
                *disjunct-record-history-size*
                *disjunct-record-sampling-rate*
                *transform-gcis*
                *absorb-domains-ranges*
                *absorb-gcis*
                *use-nary-absorption*
                *use-elh-transformation*
                *use-inverse-role-absorption* 
                *record-gci-absorption-dependencies* 
                *use-optimized-tbox-traversal*
                *use-optimized-tbox-traversal-no-bottom-search*
                *use-simplex*
                *use-simplex-for-individuals*
                *always-use-simplex*
                *simplex-use-sparse-arrays*
                *cd-use-sparse-arrays*
                *signature-merging*
                *signature-partitioning*
                *propagate-told-disjoints*
                *propagate-told-subsumers*
                *abox-partitioning-p* 
                *sorted-concept-list*
                *always-do-bottom-search*
                *random-concept-list*
                *tbox-clustering*
                *tbox-clustering-upto-n-concepts* 
                *bucket-size-threshold*
                *merge-buckets*
                *merge-buckets-threshold*
                *gc-of-obsolete-individuals*
                *prefer-backpropagated-exists*
                *depth-first-for-inverse-roles*
                *breadth-first-for-inverse-roles*
                *use-expanded-store*
                *use-unexpanded-exists-constraints-store*
                *use-unexpanded-disjunctive-constraints-store*
                *abox-use-expanded-store*
                *abox-use-unexpanded-exists-constraints-store*
                *abox-use-unexpanded-disjunctive-constraints-store*
                *use-abox-stores-ind-ass-threshold*
                *use-abox-stores-role-ass-threshold*
                *swap-to-expanded-store-threshold*
                *cache-size-fraction-from-constraint-store*
                *swap-to-unexpanded-store-threshold* 
                *cache-size-from-constraint-store-threshold* 
                *cache-size-from-constraint-store*
                *merge-constraint-store-tables-p*
                *max-constraint-stores-length*
                *use-expanded-stores-index*
                *use-relation-store*
                *always-use-relation-store* 
                *use-relation-store-threshold* 
                *use-abox-precompletion*
                *use-abox-completion*
                *delay-abox-exists*
                *ignore-abox-redundant-exists*
                *optimize-datatype-role-fillers*
                *recycle-hash-tables*
                *obsolete-table-misses-threshold*
                *no-of-obsolete-table-misses*
                *recycle-kernel-states*
                *obsolete-basic-kernel-state-misses-threshold*
                *no-of-obsolete-basic-kernel-state-misses*
                *recycle-constraints*
                *obsolete-constraint-misses-threshold*
                *no-of-obsolete-concept-constraint-misses*
                *no-of-obsolete-or-constraint-misses*
                *no-of-obsolete-relation-constraint-misses*
                *inverse-roles*
                *blocking-possibly-required*
                *flatten-encoded-concepts*
                *set-language-of-atomic-concepts*
                *enforce-reencoding*
                *optimize-to-bottom-during-encoding*
                *simplify-redundant-at-most*
                *ignore-role-domain-range*
                *use-unique-name-assumption*
                *use-new-role-hierarchy-classification*
                #+:allegro excl:*tenured-bytes-limit*
                #+:allegro excl:*global-gc-behavior*
                )))
