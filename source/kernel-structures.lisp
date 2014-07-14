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
;;; Definition of concept and relation constraints
;;;===========================================================================
        
(defstruct (constraint-common
            (:include racer-set-element)
            (:conc-name constraint-))
  "Represents common part for all constraints. Only included by other structures."
  term                                  ; the encoded concept description
  (dependencies nil)                    ; precondition constraints
  (or-dependencies nil)                 ; precondition or-constraints
  (signature nil)                       ; created from number merging signature
  (derived-from nil)                    ; points to root constraint
  (or-level *or-level*)                 ; record 'or nesting depth'
  )

(defmethod print-object :around ((object constraint-common) stream)
  (if *print-internals*
      (print-unreadable-object (object stream :type nil :identity t)
	(let ((*print-internals* nil))
	      ;; Set this to t to get the identity of concepts as well.
	  (call-next-method)))
      (call-next-method)))

(defstruct (concept-constraint
            (:include constraint-common)
            (:conc-name constraint-)
            (:constructor make-concept-constraint-internal (ind term negated-p))
            (:copier copy-concept-constraint-internal))
  "Represents a concept constraint, includes constraint-common"
  ind                                   ; ABox individual id (a number)
  negated-p                             ; is this constraint negated?
  (dependency-key nil)                  ; used to order constraints w.r.t. dependencies
  (meta-p nil)                          ; true if used as meta constraint
  (successor-ind nil)                   ; successor ind for some constraints
  (backpropagated-p nil)                ; backpropagated via inverse roles?
  (merging-trigger-p nil)               ; used as trigger for merging: cannot be treated
                                        ; as all-constraint in case of features
  (ind-synonyms nil)                    ; list of synonym individuals
  (ignore-determinism-p nil)            ; if T the constraint cannot be used to remove or-dependencies
                                        ; needed in case an ABox completion is used to test individual subsumption
  )

(defmethod print-object ((object concept-constraint) stream)
  "Print a concept constraint. Negated constraints are marked by a leading '-'"
  (format stream "~D/~D"
          (constraint-or-level object)
          (when (constraint-term object)
            (concept-hash-id (constraint-term object))))
  (if (constraint-negated-p object)
    #+:debug 
    (format stream "(~S -~S)" (constraint-ind object)
            (constraint-term object))
    #-:debug
    (format stream "(~S ~S)" (constraint-ind object)
            (concept-negated-concept-internal (constraint-term object)))
    (format stream "(~S ~S)" (constraint-ind object)
            (constraint-term object))))

(defun reset-concept-constraint (constraint)
  (setf (constraint-term constraint) nil)
  (setf (constraint-dependencies constraint) nil)
  (setf (constraint-or-dependencies constraint) nil)
  (setf (constraint-signature constraint) nil)
  (setf (constraint-derived-from constraint) nil)
  (setf (constraint-ind constraint) nil)
  (setf (constraint-negated-p constraint) nil)
  (setf (constraint-or-level constraint) nil)
  (setf (constraint-dependency-key constraint) nil)
  (setf (constraint-meta-p constraint) nil)
  (setf (constraint-successor-ind constraint) nil)
  (setf (constraint-backpropagated-p constraint) nil)
  (setf (constraint-merging-trigger-p constraint) nil)
  (setf (constraint-ind-synonyms constraint) nil)
  (setf (constraint-ignore-determinism-p constraint) nil))

(defun set-new-concept-constraint (constraint ind term negated-p or-level)
  (setf (constraint-term constraint) term)
  (setf (constraint-ind constraint) ind)
  (setf (constraint-negated-p constraint) negated-p)
  (setf (constraint-or-level constraint) or-level)
  constraint)

(defun set-recycled-concept-constraint (new old)
  (setf (constraint-term new) (constraint-term old))
  (setf (constraint-dependencies new) (constraint-dependencies old))
  (setf (constraint-or-dependencies new) (constraint-or-dependencies old))
  (setf (constraint-signature new) (constraint-signature old))
  (setf (constraint-derived-from new) (constraint-derived-from old))
  (setf (constraint-ind new) (constraint-ind old))
  (setf (constraint-negated-p new) (constraint-negated-p old))
  (setf (constraint-or-level new) (constraint-or-level old))
  (setf (constraint-dependency-key new) (constraint-dependency-key old))
  (setf (constraint-meta-p new) (constraint-meta-p old))
  (setf (constraint-successor-ind new) (constraint-successor-ind old))
  (setf (constraint-backpropagated-p new) (constraint-backpropagated-p old))
  (setf (constraint-merging-trigger-p new) (constraint-merging-trigger-p old))
  (setf (constraint-ind-synonyms new) (constraint-ind-synonyms old))
  (setf (constraint-ignore-determinism-p new) (constraint-ignore-determinism-p old)))

(defvar *obsolete-concept-constraints* nil)

(defun dispose-concept-constraint (constraint)
  (racer-atomic-push constraint *obsolete-concept-constraints*)
  (reset-concept-constraint constraint)
  nil)

(defun recycle-concept-constraint ()
  (let ((constraint (when *obsolete-concept-constraints*
                      (racer-atomic-pop *obsolete-concept-constraints*))))
    #+(or :ccl :allegro)
    (unless constraint
      (let ((threshold *obsolete-constraint-misses-threshold*))
        (incf *no-of-obsolete-concept-constraint-misses*)
        (when (> *no-of-obsolete-concept-constraint-misses* threshold)
          (format t "-CC(~D)" threshold)
          #+:ccl (ccl:gc)
          #+:allegro (excl:gc nil)
          (reset-recycle-counts)
          (let ((no-of-constraints (length *obsolete-concept-constraints*))
                (new-threshold (round (* 1.5 threshold))))
            (if (and (< no-of-constraints threshold) (< new-threshold 12000))
                (setf *obsolete-constraint-misses-threshold* new-threshold)
              (when (and (> no-of-constraints new-threshold) (> new-threshold 2000))
                (setf *obsolete-constraint-misses-threshold* (round (/ threshold 1.5)))))
	    (when (> no-of-constraints 0)
	      (setf constraint (racer-atomic-pop *obsolete-concept-constraints*)))
            (when constraint (format t "+CC(~D)" no-of-constraints))))))
    constraint))


(defun make-concept-constraint (ind term &optional (negated-p nil))
  (if *recycle-constraints*
    (let ((new-constraint (recycle-concept-constraint)))
      (if new-constraint
        (set-new-concept-constraint new-constraint ind term negated-p *or-level*)
        (setf new-constraint (make-concept-constraint-internal ind term negated-p)))
      (when (and *model-merging* *use-alternate-models*)
        (setf (constraint-derived-from new-constraint) (list new-constraint)))
      (register-object-for-termination new-constraint #'dispose-concept-constraint)
      new-constraint)
    (let ((new-constraint (make-concept-constraint-internal ind term negated-p)))
      (when (and *model-merging* *use-alternate-models*)
        (setf (constraint-derived-from new-constraint) (list new-constraint)))
      new-constraint)))

(defun copy-recycled-concept-constraint (constraint)
  (let ((new-constraint (recycle-concept-constraint)))
    (when new-constraint
      (set-recycled-concept-constraint new-constraint constraint)
      new-constraint)))

(defun copy-concept-constraint (constraint)
  (if (or-constraint-p constraint)
    (copy-or-constraint constraint)
    (let* ((recycle-constraints *recycle-constraints*)
           (new-constraint (or (and recycle-constraints
                                    (copy-recycled-concept-constraint constraint))
                               (copy-concept-constraint-internal constraint))))
      (when recycle-constraints
        (register-object-for-termination new-constraint #'dispose-concept-constraint))
      new-constraint)))

(race-inline (dependency-constraint-p))

(defun dependency-constraint-p (constraint)
  (or (or-constraint-p constraint)
      (and (constraint-signature constraint) t)))

(defstruct (or-constraint
            (:include concept-constraint)
            (:conc-name constraint-)
            (:constructor make-or-constraint-internal
                          (ind term or-clauses-counter open-clauses-counter
                               or-clauses-truth-value negated-p))
            (:copier copy-or-constraint-internal))
  "Represents an or-constraint as a specialized concept-constraint. Additionaly
records information about state and structure of or-constraints."
  (or-clauses-counter nil)              ; number of or-clauses
  (open-clauses-counter nil)            ; number of or-clauses with unknown truth
  (other-or-clauses-dependencies nil)   ; dependencies of clashed or-clauses
  (or-clauses-truth-value nil))         ; state vector for truth of or-clauses
        
(defmethod print-object ((object or-constraint) stream)
  "Print an or-constraint marked by a leading number for the open or-clauses, or
by a '-' indicating that all or-clauses clashed, or by a '+' indicating that
at least one or-clause is satisfied."
  #-:debug (declare (ignore stream))
  #+:debug
  (let* ((counter (constraint-open-clauses-counter object))
         (sign (cond ((or-constraint-clashed-p counter) "-")
                     ((or-constraint-satisfied-p counter) "+")
                     (t counter))))
    #+:debug (format stream "~A/" sign))
  (call-next-method))

(defun reset-or-constraint (constraint)
  (reset-concept-constraint constraint)
  (setf (constraint-or-clauses-counter constraint) nil)
  (setf (constraint-open-clauses-counter constraint) nil)
  (setf (constraint-other-or-clauses-dependencies constraint) nil)
  (setf (constraint-or-clauses-truth-value constraint) nil))

(defun set-new-or-constraint (constraint 
                                  ind
                                  term
                                  or-clauses-counter
                                  open-clauses-counter
                                  or-clauses-truth-value
                                  negated-p
                                  or-level)
  (set-new-concept-constraint constraint ind term negated-p or-level)
  (setf (constraint-or-clauses-counter constraint) or-clauses-counter)
  (setf (constraint-open-clauses-counter constraint) open-clauses-counter)
  (setf (constraint-or-clauses-truth-value constraint) or-clauses-truth-value)
  constraint)

(defun set-recycled-or-constraint (new old)
  (set-recycled-concept-constraint new old)
  (setf (constraint-or-clauses-counter new) (constraint-or-clauses-counter old))
  (setf (constraint-open-clauses-counter new) (constraint-open-clauses-counter old))
  (setf (constraint-other-or-clauses-dependencies new)
        (constraint-other-or-clauses-dependencies old))
  (setf (constraint-or-clauses-truth-value new) (constraint-or-clauses-truth-value old))
  )

(defvar *obsolete-or-constraints* nil)

(defun dispose-or-constraint (constraint)
  (racer-atomic-push constraint *obsolete-or-constraints*)
  (reset-or-constraint constraint)
  nil)

(defun recycle-or-constraint ()
  (let ((constraint (when *obsolete-or-constraints*
                      (racer-atomic-pop *obsolete-or-constraints*))))
    #+(or :ccl :allegro)
    (unless constraint
      (let ((threshold *obsolete-constraint-misses-threshold*))
        (incf *no-of-obsolete-or-constraint-misses*)
        (when (> *no-of-obsolete-or-constraint-misses* threshold)
          (format t "-OC(~D)" threshold)
          #+:ccl (ccl:gc)
          #+:allegro (excl:gc nil)
          (reset-recycle-counts)
          (let ((no-of-constraints (length *obsolete-or-constraints*))
                (new-threshold (round (* 1.5 threshold))))
            (if (and (< no-of-constraints threshold) (< new-threshold 12000))
              (setf *obsolete-constraint-misses-threshold* new-threshold)
              (when (and (> no-of-constraints new-threshold) (> new-threshold 2000))
                (setf *obsolete-constraint-misses-threshold* (round (/ threshold 1.5)))))
	    (when (> no-of-constraints 0)
	      (setf constraint (racer-atomic-pop *obsolete-or-constraints*)))
            (when constraint (format t "+OC(~D)" no-of-constraints))))))
    constraint))


(defun make-or-constraint (ind
                              term
                              or-clauses-counter
                              open-clauses-counter
                              or-clauses-truth-value
                              &optional (negated-p nil))
  (if *recycle-constraints*
    (let ((new-constraint (recycle-or-constraint)))
      (if new-constraint
        (set-new-or-constraint new-constraint
                               ind
                               term
                               or-clauses-counter
                               open-clauses-counter
                               or-clauses-truth-value
                               negated-p
                               *or-level*)
        (setf new-constraint (make-or-constraint-internal ind
                                                          term
                                                          or-clauses-counter
                                                          open-clauses-counter
                                                          or-clauses-truth-value
                                                          negated-p)))
      (register-object-for-termination new-constraint #'dispose-or-constraint)
      new-constraint)
    (make-or-constraint-internal ind
                                 term
                                 or-clauses-counter
                                 open-clauses-counter
                                 or-clauses-truth-value
                                 negated-p)))

(defun copy-recycled-or-constraint (constraint)
  (let ((new-constraint (recycle-or-constraint)))
    (when new-constraint
      (set-recycled-or-constraint new-constraint constraint)
      new-constraint)))

(defun copy-or-constraint (constraint)
  (let* ((recycle-constraints *recycle-constraints*)
         (new-constraint (or (and recycle-constraints
                                  (copy-recycled-or-constraint constraint))
                             (copy-or-constraint-internal constraint))))
    (when recycle-constraints
      (register-object-for-termination new-constraint #'dispose-or-constraint))
    new-constraint))

(defstruct (relation-constraint
            (:include constraint-common)
            (:conc-name constraint-)
            (:constructor make-relation-constraint-internal (ind-1 ind-2 term))
            (:copier copy-relation-constraint-internal))
  "Represents a relation constraint, includes constraint-common.
The slot term contains the role name."
  ind-1                                 ; predecessor individual
  ind-2                                 ; successor individual
  (successor-count 1)                   ; modeled successors for number restrictions
  (mark1 nil)                           ; mark1 is used for ABox subgraph partitioning
  (ind-1-synonyms nil)                  ; list of synonym individuals of ind-1
  (ind-2-synonyms nil)                  ; list of synonym individuals of ind-2
  )
        
(defmethod print-object ((object relation-constraint) stream)
  (if #+:debug (not (eql (constraint-successor-count object) 1))
      #-:debug nil
    (format stream "(~D(~S ~S) ~S)"
            (constraint-successor-count object)
            (constraint-ind-1 object) (constraint-ind-2 object)
            (constraint-term object))
    (format stream "((~S ~S) ~A)"
            (constraint-ind-1 object) (constraint-ind-2 object)
            (constraint-term object))))

(defun reset-relation-constraint (constraint)
  (setf (constraint-term constraint) nil)
  (setf (constraint-dependencies constraint) nil)
  (setf (constraint-or-dependencies constraint) nil)
  (setf (constraint-signature constraint) nil)
  (setf (constraint-derived-from constraint) nil)
  (setf (constraint-ind-1 constraint) nil)
  (setf (constraint-ind-2 constraint) nil)
  (setf (constraint-successor-count constraint) 1)
  (setf (constraint-mark1 constraint) nil)
  (setf (constraint-ind-1-synonyms constraint) nil)
  (setf (constraint-ind-2-synonyms constraint) nil))

(defun set-new-relation-constraint (constraint ind-1 ind-2 term)
  (setf (constraint-term constraint) term)
  (setf (constraint-ind-1 constraint) ind-1)
  (setf (constraint-ind-2 constraint) ind-2)
  constraint)

(defun set-recycled-relation-constraint (new old)
  (setf (constraint-term new) (constraint-term old))
  (setf (constraint-dependencies new) (constraint-dependencies old))
  (setf (constraint-or-dependencies new) (constraint-or-dependencies old))
  (setf (constraint-signature new) (constraint-signature old))
  (setf (constraint-derived-from new) (constraint-derived-from old))
  (setf (constraint-ind-1 new) (constraint-ind-1 old))
  (setf (constraint-ind-2 new) (constraint-ind-2 old))
  (setf (constraint-successor-count new) (constraint-successor-count old))
  (setf (constraint-mark1 new) (constraint-mark1 old))
  (setf (constraint-ind-1-synonyms new) (constraint-ind-1-synonyms old))
  (setf (constraint-ind-2-synonyms new) (constraint-ind-2-synonyms old)))

(defvar *obsolete-relation-constraints* nil)

(defun dispose-relation-constraint (constraint)
  (racer-atomic-push constraint *obsolete-relation-constraints*)
  (reset-relation-constraint constraint)
  nil)

(defun recycle-relation-constraint ()
  (let ((constraint (when *obsolete-relation-constraints*
                      (racer-atomic-pop *obsolete-relation-constraints*))))
    #+(or :ccl :allegro)
    (unless constraint
      (let ((threshold *obsolete-constraint-misses-threshold*))
        (incf *no-of-obsolete-relation-constraint-misses*)
        (when (> *no-of-obsolete-relation-constraint-misses* threshold)
          (format t "-RC(~D)" threshold)
          #+:ccl (ccl:gc)
          #+:allegro (excl:gc nil)
          (reset-recycle-counts)
          (let ((no-of-constraints (length *obsolete-relation-constraints*))
                (new-threshold (round (* 1.5 threshold))))
            (if (and (< no-of-constraints threshold) (< new-threshold 12000))
                (setf *obsolete-constraint-misses-threshold* new-threshold)
              (when (and (> no-of-constraints new-threshold) (> new-threshold 2000))
                (setf *obsolete-constraint-misses-threshold* (round (/ threshold 1.5)))))
	    (when (> no-of-constraints 0)
	      (setf constraint (racer-atomic-pop *obsolete-relation-constraints*))
	      (format t "+RC(~D)" no-of-constraints))))))
    constraint))


(defun make-relation-constraint (ind-1 ind-2 term)
  (if *recycle-constraints*
    (let ((new-constraint (recycle-relation-constraint)))
      (if new-constraint
        (set-new-relation-constraint new-constraint ind-1 ind-2 term)
        (setf new-constraint (make-relation-constraint-internal ind-1 ind-2 term)))
      (when (and *model-merging* *use-alternate-models*)
        (setf (constraint-derived-from new-constraint) (list new-constraint)))
      (register-object-for-termination new-constraint #'dispose-relation-constraint)
      new-constraint)
    (let ((new-constraint (make-relation-constraint-internal ind-1 ind-2 term)))
      (when (and *model-merging* *use-alternate-models*)
        (setf (constraint-derived-from new-constraint) (list new-constraint)))
      new-constraint)))

(defun copy-recycled-relation-constraint (constraint)
  (let ((new-constraint (recycle-relation-constraint)))
    (when new-constraint
      (set-recycled-relation-constraint new-constraint constraint)
      new-constraint)))

(defun copy-relation-constraint (constraint)
  (let* ((recycle-constraints *recycle-constraints*)
         (new-constraint (or (and recycle-constraints
                                  (copy-recycled-relation-constraint constraint))
                             (copy-relation-constraint-internal constraint))))
    (when recycle-constraints
      (register-object-for-termination new-constraint #'dispose-relation-constraint))
    new-constraint))

(defun print-obsolete-constraints-info (&optional (stream t))
  (format stream "~&Concept-constraints:~:D, or-constraints: ~:D, relation-constraints: ~:D"
	  (length *obsolete-concept-constraints*)
	  (length *obsolete-or-constraints*)
	  (length *obsolete-relation-constraints*)))

(defun clear-all-obsolete-constraints ()
  (setf *obsolete-concept-constraints* nil)
  (setf *obsolete-or-constraints* nil)
  (setf *obsolete-relation-constraints* nil))
 

;;; ======================================================================

(race-inline (new-constraint-ind ind-eq constraint-term-type))

(defun new-constraint-ind ()
  (incf *ind-counter*))

(defun constraint-term-type (constraint)
  (type-of (constraint-term constraint)))

(defun constraint-concept (constraint)
  (if (constraint-negated-p constraint)
      (concept-negated-concept (constraint-term constraint))
    (constraint-term constraint)))
        
(race-inline (and-constraint-p all-constraint-p some-constraint-p
                               at-least-constraint-p at-most-constraint-p
                               restrict-constraint-p
                               defined-deterministic-constraint-p))

(defun and-constraint-p (constraint)
  (and (not (constraint-negated-p constraint))
       (and-concept-p (constraint-term constraint))))

(defun restrict-constraint-p (constraint)
  (and (constraint-negated-p constraint)
       (exists-concept-p (constraint-term constraint))))

#|
(defun all-constraint-p (constraint)
  (and (constraint-negated-p constraint)
       (some-concept-p (constraint-term constraint))))
|#

(defun some-constraint-p (constraint)
  (and (not (constraint-negated-p constraint))
       (some-concept-p (constraint-term constraint))))

(defun exists-constraint-p (constraint)
  (and (not (constraint-negated-p constraint))
       (exists-concept-p (constraint-term constraint))))

(defun cd-concept-constraint-p (constraint)
  (and (not (constraint-negated-p constraint))
       (cd-concept-p (constraint-term constraint))))

#|
(defun at-least-constraint-p (constraint)
  (and (not (constraint-negated-p constraint))
       (at-least-concept-p (constraint-term constraint))))

(defun at-most-constraint-p (constraint)
  (and (constraint-negated-p constraint)
       (at-least-concept-p (constraint-term constraint))))
|#

(defun defined-deterministic-constraint-p (constraint el+-transformed-table)
  (let ((term (constraint-term constraint)))
    (and (atomic-concept-p term)
         (if (constraint-negated-p constraint)
           (if (concept-encoded-definition term)
             (or (not (concept-primitive-p term))
                 (and el+-transformed-table (gethash term el+-transformed-table)))
             (and (concept-primitive-p term)
                  (concept-encoded-negated-definition term)))
           (or (concept-encoded-definition term)
               (concept-nary-unfold-sets term)
               (concept-elh-role-domain-qualifications term))))))

#+:debug
(defun with-constraint-ignore-determinism (constraint)
  (when (concept-constraint-p constraint)
    (setf (constraint-ignore-determinism-p constraint) t))
  constraint)

#-:debug
(defmacro with-constraint-ignore-determinism (constraint)
  (let ((sym (gensym)))
  `(let ((,sym ,constraint))
     (when (concept-constraint-p ,sym)
       (setf (constraint-ignore-determinism-p ,sym) t))
     ,sym)))
