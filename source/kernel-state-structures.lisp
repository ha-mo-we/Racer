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

(defstruct (basic-kernel-state
            (:conc-name state-)
	    (:constructor make-basic-kernel-state-internal)
            (:copier copy-basic-kernel-state-internal)
            )
  (unexpanded-deterministic-constraints nil)
  (unexpanded-defined-det-constraints nil)
  (unexpanded-exists-constraints nil)
  (unexpanded-disjunctive-constraints nil)
  (unprocessed-constraints nil)
  (expanded-constraints nil)
  (relation-constraints nil)
  (attribute-constraints nil)
  (concrete-domain-state nil)
  (labels nil)
  (indirectly-blocked-individuals nil)
  (expanded-store nil)
  (expanded-store-index nil)
  (relation-store nil)
  (parameters nil)
  (unexpanded-exists-constraints-store +unused-constraint-store+)
  (copy-relation-store-p nil)
  (unexpanded-disjunctive-constraints-store +unused-constraint-store+)
  (copy-unexpanded-exists-constraints-store-p nil)
  (copy-unexpanded-disjunctive-constraints-store-p nil)
  (copy-expanded-store-p nil)
  (old-individuals nil)
  (no-of-old-individuals 0)
  (partially-expanded-or-stack nil)
  (save-completion nil)
  (old-individuals-dl-language-table nil)
  )

(defmethod print-object ((object basic-kernel-state) stream)
  (print-unreadable-object (object stream :type t :identity t)))

#-(and :debug)
(defmacro set-new-kernel-state (state
                                  &key
			          (unexpanded-deterministic-constraints nil unexp-det-p)
			          (unexpanded-defined-det-constraints nil unexp-def-det-p)
			          (unexpanded-exists-constraints nil unexp-exists-p)
			          (unexpanded-disjunctive-constraints nil unexp-dis-p)
			          (unprocessed-constraints nil unproc-p)
			          (expanded-constraints nil exp-constraints-p)
			          (relation-constraints nil rel-constraints-p)
			          (attribute-constraints nil attr-constraints-p)
			          (concrete-domain-state nil concrete-domain-p)
			          (labels nil labels-p)
			          (indirectly-blocked-individuals nil ind-blocked-inds-p)
			          (expanded-store nil exp-store-p)
			          (expanded-store-index nil exp-store-index-p)
			          (relation-store nil rel-store-p)
			          (parameters nil parameters-p)
			          (unexpanded-exists-constraints-store nil unexp-store-exists-p)
			          (unexpanded-disjunctive-constraints-store nil unexp-store-disjunctive-p)
                                  (copy-relation-store-p nil copy-rel-p)
			          (copy-unexpanded-disjunctive-constraints-store-p nil copy-disj-store-p)
			          (copy-unexpanded-exists-constraints-store-p nil copy-exists-store-p)
			          (copy-expanded-store-p nil copy-exp-store-p)
			          (old-individuals nil old-inds-p)
			          (no-of-old-individuals 0 no-of-old-inds-p)
			          (partially-expanded-or-stack nil or-stack-p)
			          (save-completion nil save-compl-p)
                                  (old-individuals-dl-language-table nil ind-lang-table-p)
                                  &allow-other-keys)
  (declare (ignorable unexpanded-deterministic-constraints
                      unexpanded-defined-det-constraints
                      unexpanded-exists-constraints
                      unexpanded-disjunctive-constraints
                      unprocessed-constraints
                      expanded-constraints
                      relation-constraints
                      attribute-constraints
                      concrete-domain-state
                      labels
                      indirectly-blocked-individuals
                      expanded-store
		      expanded-store-index
                      relation-store
                      parameters
                      unexpanded-exists-constraints-store
                      unexpanded-disjunctive-constraints-store
                      copy-relation-store-p
                      copy-unexpanded-disjunctive-constraints-store-p
                      copy-unexpanded-exists-constraints-store-p
                      copy-expanded-store-p
                      old-individuals
                      no-of-old-individuals
                      partially-expanded-or-stack
                      save-completion
                      old-individuals-dl-language-table))
  `(progn
     ,(if unexp-det-p
        `(setf (state-unexpanded-deterministic-constraints ,state)
	       ,unexpanded-deterministic-constraints)
        `(setf (state-unexpanded-deterministic-constraints ,state) nil))
     ,(if unexp-def-det-p
        `(setf (state-unexpanded-defined-det-constraints ,state)
	       ,unexpanded-defined-det-constraints)
        `(setf (state-unexpanded-defined-det-constraints ,state) nil))
     ,(if unexp-exists-p
        `(setf (state-unexpanded-exists-constraints ,state)
	       ,unexpanded-exists-constraints)
        `(setf (state-unexpanded-exists-constraints ,state) nil))
     ,(if unexp-dis-p
        `(setf (state-unexpanded-disjunctive-constraints ,state)
	       ,unexpanded-disjunctive-constraints)
        `(setf (state-unexpanded-disjunctive-constraints ,state) nil))
     ,(if unproc-p
        `(setf (state-unprocessed-constraints ,state)
	       ,unprocessed-constraints)
        `(setf (state-unprocessed-constraints ,state) nil))
     ,(if exp-constraints-p
        `(setf (state-expanded-constraints ,state) ,expanded-constraints)
        `(setf (state-expanded-constraints ,state) nil))
     ,(if rel-constraints-p
        `(setf (state-relation-constraints ,state) ,relation-constraints)
        `(setf (state-relation-constraints ,state) nil))
     ,(if attr-constraints-p
        `(setf (state-attribute-constraints ,state) ,attribute-constraints)
        `(setf (state-attribute-constraints ,state) nil))
     ,(if concrete-domain-p
        `(setf (state-concrete-domain-state ,state) ,concrete-domain-state)
        `(setf (state-concrete-domain-state ,state) nil))
     ,(if labels-p
        `(setf (state-labels ,state) ,labels)
        `(setf (state-labels ,state) nil))
     ,(if ind-blocked-inds-p
        `(setf (state-indirectly-blocked-individuals ,state)
	       ,indirectly-blocked-individuals)
        `(setf (state-indirectly-blocked-individuals ,state) nil))
     ,(if exp-store-p
        `(setf (state-expanded-store ,state) ,expanded-store)
        `(setf (state-expanded-store ,state) nil))
     ,(if exp-store-index-p
        `(setf (state-expanded-store-index ,state) ,expanded-store-index)
        `(setf (state-expanded-store-index ,state) nil))
     ,(if rel-store-p
        `(setf (state-relation-store ,state) ,relation-store)
        `(setf (state-relation-store ,state) nil))
     ,(if parameters-p
        `(setf (state-parameters ,state) ,parameters)
        `(setf (state-parameters ,state) nil))
     ,(if unexp-store-exists-p
        `(setf (state-unexpanded-exists-constraints-store ,state)
	       ,unexpanded-exists-constraints-store)
        `(setf (state-unexpanded-exists-constraints-store ,state) +unused-constraint-store+))
     ,(if copy-rel-p
        `(setf (state-copy-relation-store-p ,state) ,copy-relation-store-p)
        `(setf (state-copy-relation-store-p ,state) nil))
     ,(if unexp-store-disjunctive-p
        `(setf (state-unexpanded-disjunctive-constraints-store ,state)
	       ,unexpanded-disjunctive-constraints-store)
        `(setf (state-unexpanded-disjunctive-constraints-store ,state) +unused-constraint-store+))
     ,(if copy-disj-store-p
        `(setf (state-copy-unexpanded-disjunctive-constraints-store-p ,state)
	       ,copy-unexpanded-disjunctive-constraints-store-p)
        `(setf (state-copy-unexpanded-disjunctive-constraints-store-p ,state) nil))
     ,(if copy-exists-store-p
        `(setf (state-copy-unexpanded-exists-constraints-store-p ,state)
	       ,copy-unexpanded-exists-constraints-store-p)
        `(setf (state-copy-unexpanded-exists-constraints-store-p ,state) nil))
     ,(if copy-exp-store-p
        `(setf (state-copy-expanded-store-p ,state) ,copy-expanded-store-p)
        `(setf (state-copy-expanded-store-p ,state) nil))
     ,(if old-inds-p
        `(setf (state-old-individuals ,state) ,old-individuals)
        `(setf (state-old-individuals ,state) nil))
     ,(if no-of-old-inds-p
        `(setf (state-no-of-old-individuals ,state) ,no-of-old-individuals)
        `(setf (state-no-of-old-individuals ,state) 0))
     ,(if or-stack-p
        `(setf (state-partially-expanded-or-stack ,state)
	       ,partially-expanded-or-stack)
        `(setf (state-partially-expanded-or-stack ,state) nil))
     ,(if save-compl-p
        `(setf (state-save-completion ,state) ,save-completion)
        `(setf (state-save-completion ,state) nil))
     ,(if ind-lang-table-p
        `(setf (state-old-individuals-dl-language-table ,state) ,old-individuals-dl-language-table)
        `(setf (state-old-individuals-dl-language-table ,state) nil))
     ,state))

#+(and :debug)
(defun set-new-kernel-state (state
                                  &key
			          (unexpanded-deterministic-constraints nil unexp-det-p)
			          (unexpanded-defined-det-constraints nil unexp-def-det-p)
			          (unexpanded-exists-constraints nil unexp-exists-p)
			          (unexpanded-disjunctive-constraints nil unexp-dis-p)
			          (unprocessed-constraints nil unproc-p)
			          (expanded-constraints nil exp-constraints-p)
			          (relation-constraints nil rel-constraints-p)
			          (attribute-constraints nil attr-constraints-p)
			          (concrete-domain-state nil concrete-domain-p)
			          (labels nil labels-p)
			          (indirectly-blocked-individuals nil ind-blocked-inds-p)
			          (expanded-store nil exp-store-p)
			          (expanded-store-index nil exp-store-index-p)
			          (relation-store nil rel-store-p)
			          (parameters nil parameters-p)
			          (unexpanded-exists-constraints-store nil unexp-store-exists-p)
			          (unexpanded-disjunctive-constraints-store nil unexp-store-disjunctive-p)
                                  (copy-relation-store-p nil copy-rel-p)
			          (copy-unexpanded-disjunctive-constraints-store-p nil copy-disj-store-p)
			          (copy-unexpanded-exists-constraints-store-p nil copy-exists-store-p)
			          (copy-expanded-store-p nil copy-exp-store-p)
			          (old-individuals nil old-inds-p)
			          (no-of-old-individuals 0 no-of-old-inds-p)
			          (partially-expanded-or-stack nil or-stack-p)
			          (save-completion nil save-compl-p)
                                  (old-individuals-dl-language-table nil ind-lang-table-p)
                                  &allow-other-keys)
  (if unexp-det-p
    (setf (state-unexpanded-deterministic-constraints state)
          unexpanded-deterministic-constraints)
    (setf (state-unexpanded-deterministic-constraints state) nil))
  (if unexp-def-det-p
    (setf (state-unexpanded-defined-det-constraints state)
          unexpanded-defined-det-constraints)
    (setf (state-unexpanded-defined-det-constraints state) nil))
  (if unexp-exists-p
    (setf (state-unexpanded-exists-constraints state) unexpanded-exists-constraints)
    (setf (state-unexpanded-exists-constraints state) nil))
  (if unexp-dis-p
    (setf (state-unexpanded-disjunctive-constraints state)
          unexpanded-disjunctive-constraints)
    (setf (state-unexpanded-disjunctive-constraints state) nil))
  (if unproc-p
    (setf (state-unprocessed-constraints state) unprocessed-constraints)
    (setf (state-unprocessed-constraints state) nil))
  (if exp-constraints-p
    (setf (state-expanded-constraints state) expanded-constraints)
    (setf (state-expanded-constraints state) nil))
  (if rel-constraints-p
    (setf (state-relation-constraints state) relation-constraints)
    (setf (state-relation-constraints state) nil))
  (if attr-constraints-p
    (setf (state-attribute-constraints state) attribute-constraints)
    (setf (state-attribute-constraints state) nil))
  (if concrete-domain-p
    (setf (state-concrete-domain-state state) concrete-domain-state)
    (setf (state-concrete-domain-state state) nil))
  (if labels-p
    (setf (state-labels state) labels)
    (setf (state-labels state) nil))
  (if ind-blocked-inds-p
    (setf (state-indirectly-blocked-individuals state) indirectly-blocked-individuals)
    (setf (state-indirectly-blocked-individuals state) nil))
  (if exp-store-p
    (setf (state-expanded-store state) expanded-store)
    (setf (state-expanded-store state) nil))
  (if exp-store-index-p
    (setf (state-expanded-store-index state) expanded-store-index)
    (setf (state-expanded-store-index state) nil))
  (if rel-store-p
    (setf (state-relation-store state) relation-store)
    (setf (state-relation-store state) nil))
  (if parameters-p
    (setf (state-parameters state) parameters)
    (setf (state-parameters state) nil))
  (if unexp-store-exists-p
    (setf (state-unexpanded-exists-constraints-store state)
          unexpanded-exists-constraints-store)
    (setf (state-unexpanded-exists-constraints-store state) +unused-constraint-store+))
  (if copy-rel-p
    (setf (state-copy-relation-store-p state) copy-relation-store-p)
    (setf (state-copy-relation-store-p state) nil))
  (if unexp-store-disjunctive-p
    (setf (state-unexpanded-disjunctive-constraints-store state)
          unexpanded-disjunctive-constraints-store)
    (setf (state-unexpanded-disjunctive-constraints-store state) +unused-constraint-store+))
  (if copy-exists-store-p
    (setf (state-copy-unexpanded-exists-constraints-store-p state)
          copy-unexpanded-exists-constraints-store-p)
    (setf (state-copy-unexpanded-exists-constraints-store-p state) nil))
  (if copy-disj-store-p
    (setf (state-copy-unexpanded-disjunctive-constraints-store-p state)
          copy-unexpanded-disjunctive-constraints-store-p)
    (setf (state-copy-unexpanded-disjunctive-constraints-store-p state) nil))
  (if copy-exp-store-p
    (setf (state-copy-expanded-store-p state) copy-expanded-store-p)
    (setf (state-copy-expanded-store-p state) nil))
  (if old-inds-p
    (setf (state-old-individuals state) old-individuals)
    (setf (state-old-individuals state) nil))
  (if no-of-old-inds-p
    (setf (state-no-of-old-individuals state) no-of-old-individuals)
    (setf (state-no-of-old-individuals state) 0))
  (if or-stack-p
    (setf (state-partially-expanded-or-stack state) partially-expanded-or-stack)
    (setf (state-partially-expanded-or-stack state) nil))
  (if save-compl-p
    (setf (state-save-completion state) save-completion)
    (setf (state-save-completion state) nil))
  (if ind-lang-table-p
    (setf (state-old-individuals-dl-language-table state) old-individuals-dl-language-table)
    (setf (state-old-individuals-dl-language-table state) nil))
  state)

#+(and :debug)
(defun make-basic-kernel-state (&rest params)
  (if *recycle-kernel-states*
    (let ((state (recycle-basic-kernel-state)))
      (if state
        (apply #'set-new-kernel-state state params)
	(setf state (apply #'make-basic-kernel-state-internal params)))
      (register-object-for-termination state #'dispose-basic-kernel-state)
      state)
    (apply #'make-basic-kernel-state-internal params)))

#-(and :debug)
(defmacro make-basic-kernel-state (&rest params)
  (let ((state-sym (gensym)))
    `(if *recycle-kernel-states*
       (let ((,state-sym (recycle-basic-kernel-state)))
         (if ,state-sym
           (set-new-kernel-state ,state-sym . ,params)
	   (setf ,state-sym (make-basic-kernel-state-internal . ,params)))
	 (register-object-for-termination ,state-sym #'dispose-basic-kernel-state)
	 ,state-sym)
       (make-basic-kernel-state-internal . ,params))))

(defvar *obsolete-basic-kernel-states* nil)

(defun dispose-basic-kernel-state (state)
  (racer-atomic-push state *obsolete-basic-kernel-states*)
  (set-new-kernel-state state)
  nil)

(defun recycle-basic-kernel-state ()
  (let ((state (when *obsolete-basic-kernel-states*
                 (racer-atomic-pop *obsolete-basic-kernel-states*))))
    #+(or :ccl :allegro)
    (unless state
      (let ((threshold *obsolete-basic-kernel-state-misses-threshold*))
        (incf *no-of-obsolete-basic-kernel-state-misses*)
        (when (> *no-of-obsolete-basic-kernel-state-misses* threshold)
          (format t "-S(~D)" threshold)
          #+:ccl (ccl:gc)
          #+:allegro (excl:gc nil)
          (reset-recycle-counts)
          (let ((no-of-states (length *obsolete-basic-kernel-states*))
                (new-threshold (round (* 1.5 threshold))))
            (if (and (< no-of-states threshold) (< new-threshold 12000))
              (setf *obsolete-basic-kernel-state-misses-threshold* new-threshold)
              (when (and (> no-of-states new-threshold) (> new-threshold 2000))
                (setf *obsolete-basic-kernel-state-misses-threshold* (round (/ threshold 1.5)))))
	    (when (> no-of-states 0)
	      (setf state (racer-atomic-pop *obsolete-basic-kernel-states*)))
            (when state (format t "+S(~D)" no-of-states))))))
    state))

(defun set-recycled-basic-kernel-state (new old)
  (setf (state-unexpanded-deterministic-constraints new) (state-unexpanded-deterministic-constraints old)
	(state-unexpanded-defined-det-constraints new) (state-unexpanded-defined-det-constraints old)
	(state-unexpanded-exists-constraints new) (state-unexpanded-exists-constraints old)
	(state-unexpanded-disjunctive-constraints new) (state-unexpanded-disjunctive-constraints old)
	(state-unprocessed-constraints new) (state-unprocessed-constraints old)
	(state-expanded-constraints new) (state-expanded-constraints old)
	(state-relation-constraints new) (state-relation-constraints old)
	(state-attribute-constraints new) (state-attribute-constraints old)
	(state-concrete-domain-state new) (state-concrete-domain-state old)
	(state-labels new) (state-labels old)
	(state-indirectly-blocked-individuals new) (state-indirectly-blocked-individuals old)
	(state-expanded-store new) (state-expanded-store old)
	(state-expanded-store-index new) (state-expanded-store-index old)
	(state-relation-store new) (state-relation-store old)
	(state-parameters new) (state-parameters old)
	(state-unexpanded-exists-constraints-store new) (state-unexpanded-exists-constraints-store old)
	(state-copy-relation-store-p new) (state-copy-relation-store-p old)
	(state-unexpanded-disjunctive-constraints-store new) (state-unexpanded-disjunctive-constraints-store old)
	(state-copy-unexpanded-exists-constraints-store-p new) (state-copy-unexpanded-exists-constraints-store-p old)
	(state-copy-unexpanded-disjunctive-constraints-store-p new) (state-copy-unexpanded-disjunctive-constraints-store-p old)
	(state-copy-expanded-store-p new) (state-copy-expanded-store-p old)
	(state-old-individuals new) (state-old-individuals old)
	(state-no-of-old-individuals new) (state-no-of-old-individuals old)
	(state-partially-expanded-or-stack new) (state-partially-expanded-or-stack old)
	(state-save-completion new) (state-save-completion old)
	(state-old-individuals-dl-language-table new)
        (state-old-individuals-dl-language-table old)
	)
  )

(defun copy-recycled-basic-kernel-state (state)
  (let ((new-state (recycle-basic-kernel-state)))
    (when new-state
      (set-recycled-basic-kernel-state new-state state)
      new-state)))

(defun copy-basic-kernel-state (state copy-p)
  (when state
    (if (signature-kernel-state-p state)
      (copy-signature-kernel-state state copy-p)
      (let* ((recycle-kernel-states *recycle-kernel-states*)
	     (new-state (or (and recycle-kernel-states
			         (copy-recycled-basic-kernel-state state))
			    (copy-basic-kernel-state-internal state)))
	     (copy-p (or copy-p (not *in-precompletion*))))
        (when recycle-kernel-states 
	  (register-object-for-termination new-state #'dispose-basic-kernel-state))
        (when (and (state-relation-store new-state)
                   (or (eq copy-p t) (eq copy-p ':relation-store)))
          (setf (state-copy-relation-store-p new-state) copy-p))
        (when (and (consp (state-unexpanded-exists-constraints-store new-state))
                   (or (eq copy-p t) (eq copy-p ':unexpanded-exists)))
          (setf (state-copy-unexpanded-exists-constraints-store-p new-state) copy-p)
          (ensure-copying-of-constraint-store
           (state-unexpanded-exists-constraints-store new-state)))
        (when (and (consp (state-unexpanded-disjunctive-constraints-store new-state))
                   (or (eq copy-p t) (eq copy-p ':unexpanded-disjunctive)))
          (setf (state-copy-unexpanded-disjunctive-constraints-store-p new-state) copy-p)
          (ensure-copying-of-constraint-store
           (state-unexpanded-disjunctive-constraints-store new-state)))
        (when (and (consp (state-expanded-store new-state))
                   (or (eq copy-p t) (eq copy-p ':expanded)))
          (setf (state-copy-expanded-store-p new-state) copy-p)
          (ensure-copying-of-constraint-store (state-expanded-store new-state)
					      (state-expanded-store-index new-state)))
        new-state))))

(defun clear-all-obsolete-kernel-states ()
  (racer-atomic-setf *obsolete-basic-kernel-states* nil))

(defun print-obsolete-kernel-states-info (&optional (stream t))
  (format stream "~&Basic-kernel-states:~:D"
	  (length *obsolete-basic-kernel-states*)))


(defstruct (kernel-parameters
            (:conc-name state-)
            )
  (tbox nil)
  (abox nil)
  (top-concept nil)                     ; copied from tbox
  (bottom-concept nil)                  ; copied from tbox
  (concrete-domain nil)                 ; copied from tbox
  (dl-prover-language nil)
  (subsumed-concept-p nil)
  (concept-clash-p nil)
 )

(defmethod print-object ((object kernel-parameters) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defun create-kernel-parameters (&rest params)
  (let* ((new-params (apply #'make-kernel-parameters params))
         (tbox (state-tbox new-params)))
    (when tbox
      (setf (state-top-concept new-params) (tbox-top-node tbox)
            (state-bottom-concept new-params) (tbox-bottom-node tbox)
            (state-concrete-domain new-params) (tbox-concrete-domain tbox)
            ))
    new-params))

#+(and :debug)
(defun changed-kernel-state (state
                                  &key
                                  (unexpanded-deterministic-constraints nil unexp-det-p)
                                  (unexpanded-defined-det-constraints nil unexp-def-det-p)
                                  (unexpanded-exists-constraints nil unexp-exists-p)
                                  (unexpanded-disjunctive-constraints nil unexp-dis-p)
                                  (unprocessed-constraints nil unproc-p)
                                  (expanded-constraints nil exp-constraints-p)
                                  (relation-constraints nil rel-constraints-p)
                                  (attribute-constraints nil attr-constraints-p)
                                  (concrete-domain-state nil concrete-domain-p)
                                  (labels nil labels-p)
                                  (indirectly-blocked-individuals nil ind-blocked-inds-p)
                                  (expanded-store nil exp-store-p)
			          (expanded-store-index nil exp-store-index-p)
                                  (relation-store nil rel-store-p)
                                  (parameters nil parameters-p)
                                  (unexpanded-exists-constraints-store nil unexp-store-exists-p)
                                  (unexpanded-disjunctive-constraints-store nil unexp-store-disjunctive-p)
                                  (copy-unexpanded-disjunctive-constraints-store-p nil copy-disj-store-p)
                                  (copy-unexpanded-exists-constraints-store-p nil copy-exists-store-p)
                                  (copy-expanded-store-p nil copy-exp-store-p)
                                  (old-individuals nil old-inds-p)
                                  (no-of-old-individuals 0 no-of-old-inds-p)
                                  (partially-expanded-or-stack nil or-stack-p)
                                  (save-completion nil save-compl-p)
                                  (old-individuals-dl-language-table nil ind-lang-table-p)
                                  &allow-other-keys)
  (let ((new-state
         (if (and (boundp '*debug2*) (not (signature-kernel-state-p state)))
           (copy-basic-kernel-state state nil)
           state)))
    (when unexp-det-p
      (setf (state-unexpanded-deterministic-constraints new-state)
            unexpanded-deterministic-constraints))
    (when unexp-def-det-p
      (setf (state-unexpanded-defined-det-constraints new-state)
            unexpanded-defined-det-constraints))
    (when unexp-exists-p
      (setf (state-unexpanded-exists-constraints new-state)
            unexpanded-exists-constraints))
    (when unexp-dis-p
      (setf (state-unexpanded-disjunctive-constraints new-state)
            unexpanded-disjunctive-constraints))
    (when unproc-p
      (setf (state-unprocessed-constraints new-state) unprocessed-constraints))
    (when exp-constraints-p
      (setf (state-expanded-constraints new-state) expanded-constraints))
    (when rel-constraints-p
      (setf (state-relation-constraints new-state) relation-constraints))
    (when attr-constraints-p
      (setf (state-attribute-constraints new-state) attribute-constraints))
    (when concrete-domain-p
      (setf (state-concrete-domain-state new-state) concrete-domain-state))
    (when labels-p
      (setf (state-labels new-state) labels))
    (when ind-blocked-inds-p
      (setf (state-indirectly-blocked-individuals new-state)
            indirectly-blocked-individuals))
    (when exp-store-p
      (setf (state-expanded-store new-state) expanded-store))
    (when exp-store-index-p
      (setf (state-expanded-store-index new-state) expanded-store-index))
    (when rel-store-p
      (setf (state-relation-store new-state) relation-store))
    (when parameters-p
      (setf (state-parameters new-state) parameters))
    (when unexp-store-exists-p
      (setf (state-unexpanded-exists-constraints-store new-state)
            unexpanded-exists-constraints-store))
    (when unexp-store-disjunctive-p
      (setf (state-unexpanded-disjunctive-constraints-store new-state)
            unexpanded-disjunctive-constraints-store))
    (when copy-disj-store-p
      (setf (state-copy-unexpanded-disjunctive-constraints-store-p new-state)
            copy-unexpanded-disjunctive-constraints-store-p))
    (when copy-exists-store-p
      (setf (state-copy-unexpanded-exists-constraints-store-p new-state)
            copy-unexpanded-exists-constraints-store-p))
    (when copy-exp-store-p
      (setf (state-copy-expanded-store-p new-state) copy-expanded-store-p))
    (when old-inds-p
      (setf (state-old-individuals new-state) old-individuals))
    (when no-of-old-inds-p
      (setf (state-no-of-old-individuals new-state) no-of-old-individuals))
    (when or-stack-p
      (setf (state-partially-expanded-or-stack new-state)
            partially-expanded-or-stack))
    (when save-compl-p
      (setf (state-save-completion new-state) save-completion))
    (when ind-lang-table-p
      (setf (state-old-individuals-dl-language-table new-state) old-individuals-dl-language-table))
    new-state))

#-(and :debug)
(defmacro changed-kernel-state (state
                                 &key
                                 (unexpanded-deterministic-constraints nil unexp-det-p)
                                 (unexpanded-defined-det-constraints nil unexp-def-det-p)
                                 (unexpanded-exists-constraints nil unexp-exists-p)
                                 (unexpanded-disjunctive-constraints nil unexp-dis-p)
                                 (unprocessed-constraints nil unproc-p)
                                 (expanded-constraints nil exp-constraints-p)
                                 (relation-constraints nil rel-constraints-p)
                                 (attribute-constraints nil attr-constraints-p)
                                 (concrete-domain-state nil concrete-domain-p)
                                 (labels nil labels-p)
                                 (indirectly-blocked-individuals nil ind-blocked-inds-p)
                                 (expanded-store nil exp-store-p)
				 (expanded-store-index nil exp-store-index-p)
                                 (relation-store nil rel-store-p)
                                 (parameters nil parameters-p)
                                 (unexpanded-exists-constraints-store nil unexp-store-exists-p)
                                 (unexpanded-disjunctive-constraints-store nil unexp-store-disjunctive-p)
                                 (copy-unexpanded-disjunctive-constraints-store-p nil copy-disj-store-p)
                                 (copy-unexpanded-exists-constraints-store-p nil copy-exists-store-p)
                                 (copy-expanded-store-p nil copy-exp-store-p)
                                 (old-individuals nil old-inds-p)
                                 (no-of-old-individuals 0 no-of-old-inds-p)
                                 (partially-expanded-or-stack nil or-stack-p)
                                 (save-completion nil save-compl-p)
                                 (old-individuals-dl-language-table nil ind-lang-table-p)
                                 &allow-other-keys)
  (declare (ignorable unexpanded-deterministic-constraints
                      unexpanded-defined-det-constraints
                      unexpanded-exists-constraints
                      unexpanded-disjunctive-constraints
                      unprocessed-constraints
                      expanded-constraints
                      relation-constraints
                      attribute-constraints
                      concrete-domain-state
                      labels
                      indirectly-blocked-individuals
                      expanded-store
                      expanded-store-index
                      relation-store
                      parameters
                      unexpanded-exists-constraints-store
                      unexpanded-disjunctive-constraints-store
                      copy-unexpanded-disjunctive-constraints-store-p
                      copy-unexpanded-exists-constraints-store-p
                      copy-expanded-store-p
                      old-individuals
                      no-of-old-individuals
                      partially-expanded-or-stack
                      save-completion
                      old-individuals-dl-language-table))
  `(progn
     ,@(remove nil
               `(,(when unexp-det-p
                    `(setf (state-unexpanded-deterministic-constraints ,state)
                           ,unexpanded-deterministic-constraints))
                 ,(when unexp-def-det-p
                    `(setf (state-unexpanded-defined-det-constraints ,state)
                           ,unexpanded-defined-det-constraints))
                 ,(when unexp-exists-p
                    `(setf (state-unexpanded-exists-constraints ,state)
                           ,unexpanded-exists-constraints))
                 ,(when unexp-dis-p
                    `(setf (state-unexpanded-disjunctive-constraints ,state)
                           ,unexpanded-disjunctive-constraints))
                 ,(when unproc-p
                    `(setf (state-unprocessed-constraints ,state)
                           ,unprocessed-constraints))
                 ,(when exp-constraints-p
                    `(setf (state-expanded-constraints ,state) ,expanded-constraints))
                 ,(when rel-constraints-p
                    `(setf (state-relation-constraints ,state) ,relation-constraints))
                 ,(when attr-constraints-p
                    `(setf (state-attribute-constraints ,state) ,attribute-constraints))
                 ,(when concrete-domain-p
                    `(setf (state-concrete-domain-state ,state) ,concrete-domain-state))
                 ,(when labels-p
                    `(setf (state-labels ,state) ,labels))
                 ,(when ind-blocked-inds-p
                    `(setf (state-indirectly-blocked-individuals ,state)
                           ,indirectly-blocked-individuals))
                 ,(when exp-store-p
                    `(setf (state-expanded-store ,state) ,expanded-store))
		 ,(when exp-store-index-p
                    `(setf (state-expanded-store-index ,state) ,expanded-store-index))
                 ,(when rel-store-p
                    `(setf (state-relation-store ,state) ,relation-store))
                 ,(when parameters-p
                    `(setf (state-parameters ,state) ,parameters))
                 ,(when unexp-store-exists-p
                    `(setf (state-unexpanded-exists-constraints-store ,state)
                           ,unexpanded-exists-constraints-store))
                 ,(when unexp-store-disjunctive-p
                    `(setf (state-unexpanded-disjunctive-constraints-store ,state)
                           ,unexpanded-disjunctive-constraints-store))
                 ,(when copy-disj-store-p
                    `(setf (state-copy-unexpanded-disjunctive-constraints-store-p ,state)
                           ,copy-unexpanded-disjunctive-constraints-store-p))
                 ,(when copy-exists-store-p
                    `(setf (state-copy-unexpanded-exists-constraints-store-p ,state)
                           ,copy-unexpanded-exists-constraints-store-p))
                 ,(when copy-exp-store-p
                    `(setf (state-copy-expanded-store-p ,state) ,copy-expanded-store-p))
                 ,(when old-inds-p
                    `(setf (state-old-individuals ,state) ,old-individuals))
                 ,(when no-of-old-inds-p
                    `(setf (state-no-of-old-individuals ,state) ,no-of-old-individuals))
                 ,(when or-stack-p
                    `(setf (state-partially-expanded-or-stack ,state)
                           ,partially-expanded-or-stack))
                 ,(when save-compl-p
                    `(setf (state-save-completion ,state) ,save-completion))
                 ,(when ind-lang-table-p
                    `(setf (state-old-individuals-dl-language-table ,state) ,old-individuals-dl-language-table))
                 ))
     ,state))

(defstruct (signature-kernel-state
            (:conc-name state-)
            (:include basic-kernel-state)
            (:copier copy-signature-kernel-state-internal)
            )
  (violated-role nil)
  (at-most-constraint nil)
  (true-some-constraints nil)
  (at-least-bounds nil)
  (at-most-bounds nil)
  (current-at-most-bound nil)
  (remaining-at-most-bounds nil)
  (role-related-exists-constraints nil)
  )

(defun copy-signature-kernel-state (state copy-p)
  (when state
    (let ((new-state (copy-signature-kernel-state-internal state))
          (copy-p (or copy-p (not *in-precompletion*))))
      (when (state-relation-store new-state)
        (setf (state-copy-relation-store-p new-state) copy-p))
      (when (consp (state-unexpanded-exists-constraints-store new-state))
        (setf (state-copy-unexpanded-exists-constraints-store-p new-state) copy-p)
        (ensure-copying-of-constraint-store
         (state-unexpanded-exists-constraints-store new-state)))
      (when (consp (state-unexpanded-disjunctive-constraints-store new-state))
        (setf (state-copy-unexpanded-disjunctive-constraints-store-p new-state) copy-p)
        (ensure-copying-of-constraint-store
         (state-unexpanded-disjunctive-constraints-store new-state)))
      (when (consp (state-expanded-store new-state))
        (setf (state-copy-expanded-store-p new-state) copy-p)
        (ensure-copying-of-constraint-store (state-expanded-store new-state)
					    (state-expanded-store-index new-state)))
      new-state)))

#+(and :ccl :debug)
(defun changed-signature-state (state
                                     &rest params
                                     &key
                                     (violated-role nil violated-role-p)
                                     (at-most-constraint nil at-most-constraint-p)
                                     (true-some-constraints nil true-some-constraints-p)
                                     (at-least-bounds nil at-least-bounds-p)
                                     (at-most-bounds nil at-most-bounds-p)
                                     (current-at-most-bound nil current-at-most-bound-p)
                                     (remaining-at-most-bounds nil remaining-at-most-bounds-p)
                                     (role-related-exists-constraints nil role-related-p)
                                     &allow-other-keys)
                                     
  (let ((new-state
         (apply #'changed-kernel-state
                (if (boundp '*debug2*)
                  (copy-signature-kernel-state state nil)
                  state)
                params)))
    (when violated-role-p
      (setf (state-violated-role new-state) violated-role))
    (when at-most-constraint-p
      (setf (state-at-most-constraint new-state) at-most-constraint))
    (when true-some-constraints-p
      (setf (state-true-some-constraints new-state) true-some-constraints))
    (when at-least-bounds-p
      (setf (state-at-least-bounds new-state) at-least-bounds))
    (when at-most-bounds-p
      (setf (state-at-most-bounds new-state) at-most-bounds))
    (when current-at-most-bound-p
      (setf (state-current-at-most-bound new-state) current-at-most-bound))
    (when remaining-at-most-bounds-p
      (setf (state-remaining-at-most-bounds new-state) remaining-at-most-bounds))
    (when role-related-p
      (setf (state-role-related-exists-constraints new-state)
            role-related-exists-constraints))
    new-state))

#-(and :ccl :debug)
(defmacro changed-signature-state (state
                                    &rest params
                                    &key
                                    (violated-role nil violated-role-p)
                                    (at-most-constraint nil at-most-constraint-p)
                                    (true-some-constraints nil true-some-constraints-p)
                                    (at-least-bounds nil at-least-bounds-p)
                                    (at-most-bounds nil at-most-bounds-p)
                                    (current-at-most-bound nil current-at-most-bound-p)
                                    (remaining-at-most-bounds nil remaining-at-most-bounds-p)
                                    (role-related-exists-constraints nil role-related-p)
                                    &allow-other-keys)
(declare (ignorable violated-role
                    at-most-constraint
                    true-some-constraints
                    at-least-bounds
                    at-most-bounds
                    current-at-most-bound
                    remaining-at-most-bounds
                    role-related-exists-constraints))
  (let ((new-state-0 (gensym))
        (new-state (gensym)))
    `(let* ((,new-state-0 ,state)
            (,new-state (changed-kernel-state ,new-state-0 ,@params)))
       ,@(remove nil
                 `(,(when violated-role-p
                      `(setf (state-violated-role ,new-state) ,violated-role))
                   ,(when at-most-constraint-p
                      `(setf (state-at-most-constraint ,new-state) ,at-most-constraint))
                   ,(when true-some-constraints-p
                      `(setf (state-true-some-constraints ,new-state) ,true-some-constraints))
                   ,(when at-least-bounds-p
                      `(setf (state-at-least-bounds ,new-state) ,at-least-bounds))
                   ,(when at-most-bounds-p
                      `(setf (state-at-most-bounds ,new-state) ,at-most-bounds))
                   ,(when current-at-most-bound-p
                      `(setf (state-current-at-most-bound ,new-state) ,current-at-most-bound))
                   ,(when remaining-at-most-bounds-p
                      `(setf (state-remaining-at-most-bounds ,new-state) ,remaining-at-most-bounds))
                   ,(when role-related-p
                      `(setf (state-role-related-exists-constraints ,new-state)
                             ,role-related-exists-constraints))))
       ,new-state)))

(defmacro basic-signature-copy (function state)
  `(,function
    :unexpanded-deterministic-constraints (state-unexpanded-deterministic-constraints ,state)
    :unexpanded-defined-det-constraints (state-unexpanded-defined-det-constraints ,state)
    :unexpanded-exists-constraints (state-unexpanded-exists-constraints ,state)
    :unexpanded-disjunctive-constraints (state-unexpanded-disjunctive-constraints ,state)
    :unprocessed-constraints (state-unprocessed-constraints ,state)
    :expanded-constraints (state-expanded-constraints ,state)
    :relation-constraints (state-relation-constraints ,state)
    :attribute-constraints (state-attribute-constraints ,state)
    :concrete-domain-state (state-concrete-domain-state ,state)
    :labels (state-labels ,state)
    :indirectly-blocked-individuals (state-indirectly-blocked-individuals ,state)
    :expanded-store (state-expanded-store ,state)
    :expanded-store-index (state-expanded-store-index ,state)
    :relation-store (state-relation-store ,state)
    :parameters (state-parameters ,state)
    :unexpanded-exists-constraints-store (state-unexpanded-exists-constraints-store ,state)
    :unexpanded-disjunctive-constraints-store
    (state-unexpanded-disjunctive-constraints-store ,state)
    :copy-relation-store-p (state-copy-relation-store-p ,state)
    :copy-unexpanded-exists-constraints-store-p
    (state-copy-unexpanded-exists-constraints-store-p ,state)
    :copy-unexpanded-disjunctive-constraints-store-p
    (state-copy-unexpanded-disjunctive-constraints-store-p ,state)
    :copy-expanded-store-p (state-copy-expanded-store-p ,state)
    :old-individuals (state-old-individuals ,state)
    :no-of-old-individuals (state-no-of-old-individuals ,state)
    :partially-expanded-or-stack (state-partially-expanded-or-stack ,state)
    :save-completion (state-save-completion ,state)
    :old-individuals-dl-language-table (state-old-individuals-dl-language-table ,state)
    ))


(defun copy-to-signature-kernel-state (state)
  (basic-signature-copy make-signature-kernel-state state))

(defun copy-to-basic-kernel-state (state)
  (basic-signature-copy make-basic-kernel-state state))
