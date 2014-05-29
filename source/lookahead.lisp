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

(in-package :racer)

(race-inline (or-constraint-clashed-p or-constraint-satisfied-p))

#-:allegro
(race-inline (bcp-or-constraint-p
              fully-expanded-or-constraint-p
              deterministic-or-constraint-p))

(defconstant +satisfied-marker+ 0
  "Used to mark an or-constraint as satisfied by at least one or-clause")
(defconstant +clashed-marker+ -1
  "Used to mark an or-constraint as clashed due to its clashed or-clauses")

(defun or-constraint-clashed-p (open-clauses-no)
  (eql open-clauses-no +clashed-marker+))

(defun or-constraint-satisfied-p (open-clauses-no)
  (eql open-clauses-no +satisfied-marker+))

#-:allegro
(defun bcp-or-constraint-p (open-clauses-no)
  (eql open-clauses-no 1))

#+:allegro
(defmacro bcp-or-constraint-p (open-clauses-no)
  `(eql ,open-clauses-no 1))

#-:allegro
(defun fully-expanded-or-constraint-p (open-clauses-no)
  (< open-clauses-no 1))

#+:allegro
(defmacro fully-expanded-or-constraint-p (open-clauses-no)
  `(< ,open-clauses-no 1))

#-:allegro
(defun deterministic-or-constraint-p (open-clauses-no)
  (< open-clauses-no 2))

#+:allegro
(defmacro deterministic-or-constraint-p (open-clauses-no)
  `(< ,open-clauses-no 2))

(defun forward-checking-or-constraints (state
                                               relation-constraints
                                               new-unexpanded-disjunctive-constraints
                                               added-unexpanded-disjunctive-constraints
                                               unexpanded-disjunctive-constraints-store
                                               &optional
                                               (new-constraints nil new-constraints-p)
                                               (unexp-exists-p nil))
  "Test whether any or-clause in unexpanded-disjunctive-constraints is true or 
   clashes with the elements of new-constraints. If this is the case, create a new 
   modified or-constraint. In case of a detected clash, backtrack immediately. 
   If an or-constraint gets deterministic (i.e. only one open or-clause or at 
   least one or-clause is satisfied), it is removed from the list of unexpanded 
   disjunctive constraints and added to the list of unexpanded deterministic 
   constraints. Return 6 values: clashed-p, added or-constraints, 
   (new) unexpanded-disjunctive-constraints, new-deterministic-or-constraints,
    (new) unexpanded-disjunctive-constraints-store, removed or-constraints.
   Two application scenarios:
   1) new-constraints-p is T => 
      check new-constraints with ALL matching unexpanded or-constraints
      new-unexpanded-disjunctive-constraints
   2) new-constraints-p is NIL =>
      check new-unexpanded-or-constraints with ALL matching expanded constraints"
  (let* ((update-constraint-dependencies (and *use-dependency-based-instance-retrieval*
                                              (or (boundp '*clash-culprits*) 
                                                  (boundp '*clash-reasons*))))
         (or-level *or-level*)
         (use-alternate-models (and *model-merging*
                                    (or *use-alternate-models*
                                        *use-alternate-ind-models*)))
         (params (state-parameters state))
         (subsumed-concept-p (state-subsumed-concept-p params))
         (concept-clash-p (state-concept-clash-p params))
         (expanded-constraints (state-expanded-constraints state))
         (expanded-store (state-expanded-store state))
         (expanded-store-index (state-expanded-store-index state))
         (use-expanded-store-p (and *use-expanded-store*
                                    (not (constraint-store-unused-p expanded-store))))
         (unexpanded-exists-constraints-store
          (state-unexpanded-exists-constraints-store state)))
    (if new-constraints-p
      (if unexp-exists-p
        (forward-checking-or-constraints-2 state
                                           relation-constraints
                                           new-unexpanded-disjunctive-constraints
                                           added-unexpanded-disjunctive-constraints
                                           unexpanded-disjunctive-constraints-store
                                           update-constraint-dependencies
                                           or-level
                                           use-alternate-models
                                           subsumed-concept-p
                                           concept-clash-p
                                           (and *use-unexpanded-exists-constraints-store*
                                                (not (constraint-store-unused-p
                                                      unexpanded-exists-constraints-store)))
                                           (state-unexpanded-exists-constraints state)
                                           unexpanded-exists-constraints-store
                                           nil)
        (forward-checking-or-constraints-1 state
                                           relation-constraints
                                           new-unexpanded-disjunctive-constraints
                                           added-unexpanded-disjunctive-constraints
                                           unexpanded-disjunctive-constraints-store
                                           update-constraint-dependencies
                                           or-level
                                           use-alternate-models
                                           subsumed-concept-p
                                           concept-clash-p
                                           use-expanded-store-p
                                           expanded-constraints
                                           expanded-store
                                           expanded-store-index
                                           (if (and *use-expanded-store*
                                                    (> (length new-constraints) 50))
                                             (loop with table =
                                                   (smart-clrhash *expanded-constraints-ind-table*
								  nil
								  '*expanded-constraints-ind-table*)
                                                   for constraint in new-constraints do
                                                   (push constraint
                                                         (gethash (constraint-ind constraint)
                                                                  table))
                                                   finally (return table))
                                             new-constraints)))
      (forward-checking-or-constraints-2 state
                                         relation-constraints
                                         new-unexpanded-disjunctive-constraints
                                         added-unexpanded-disjunctive-constraints
                                         unexpanded-disjunctive-constraints-store
                                         update-constraint-dependencies
                                         or-level
                                         use-alternate-models
                                         subsumed-concept-p
                                         concept-clash-p
                                         use-expanded-store-p
                                         expanded-constraints
                                         expanded-store
                                         expanded-store-index))))

(defun forward-checking-or-constraints-1 (state
                                                 relation-constraints
                                                 new-unexpanded-disjunctive-constraints
                                                 added-unexpanded-disjunctive-constraints
                                                 unexpanded-disjunctive-constraints-store
                                                 update-constraint-dependencies
                                                 or-level
                                                 use-alternate-models
                                                 subsumed-concept-p
                                                 concept-clash-p
                                                 use-expanded-store-p
                                                 expanded-constraints
                                                 expanded-store
                                                 expanded-store-index
					  new-constraints-table)
  (if (and *use-unexpanded-disjunctive-constraints-store*
	   unexpanded-disjunctive-constraints-store
	   (not (constraint-store-unused-p unexpanded-disjunctive-constraints-store)))
      (if (listp new-constraints-table)
	  (loop for ind in (racer-remove-duplicates (mapcar #'constraint-ind new-constraints-table))
	    for unexpanded-disjunctive-constraints = 
	      (collect-ind-selected-constraints ind
						new-unexpanded-disjunctive-constraints
						unexpanded-disjunctive-constraints-store)
	    for clashed-p = nil
	    for added-or-constraints = nil
	    for removed-or-constraints = nil
	    for new-deterministic-or-constraints = nil
	    do
	      (multiple-value-setq (clashed-p 
				    added-or-constraints
				    removed-or-constraints
				    new-deterministic-or-constraints)
		(forward-checking-or-constraints-1-1 unexpanded-disjunctive-constraints
						     relation-constraints
						     update-constraint-dependencies
						     or-level
						     use-alternate-models
						     subsumed-concept-p
						     concept-clash-p
						     use-expanded-store-p
						     expanded-constraints
						     expanded-store
						     expanded-store-index
						     new-constraints-table))
	    if clashed-p
	    do (return-from forward-checking-or-constraints-1 t)
	    else
	    nconc added-or-constraints into all-added-or-constraints
	    and
	    nconc removed-or-constraints into all-removed-or-constraints
	    and
	    nconc new-deterministic-or-constraints into all-new-deterministic-or-constraints
	    finally
	      (return (return-forward-checking-values state
						      all-added-or-constraints
						      all-removed-or-constraints
						      added-unexpanded-disjunctive-constraints
						      all-new-deterministic-or-constraints
						      unexpanded-disjunctive-constraints-store)))
	(loop with unexpanded-disjunctive-constraints-store-index =
	      (when (> (* (length unexpanded-disjunctive-constraints-store)
			  (hash-table-count new-constraints-table))
		       10000)
		(create-unexpanded-store-index unexpanded-disjunctive-constraints-store))
	    for ind being the hash-key of new-constraints-table
	    for unexpanded-disjunctive-constraints = 
	      (collect-ind-selected-constraints ind
						new-unexpanded-disjunctive-constraints
						unexpanded-disjunctive-constraints-store
						unexpanded-disjunctive-constraints-store-index)
	    for clashed-p = nil
	    for added-or-constraints = nil
	    for removed-or-constraints = nil
	    for new-deterministic-or-constraints = nil
	    do
	      (multiple-value-setq (clashed-p 
				    added-or-constraints
				    removed-or-constraints
				    new-deterministic-or-constraints)
		(forward-checking-or-constraints-1-1 unexpanded-disjunctive-constraints
						     relation-constraints
						     update-constraint-dependencies
						     or-level
						     use-alternate-models
						     subsumed-concept-p
						     concept-clash-p
						     use-expanded-store-p
						     expanded-constraints
						     expanded-store
						     expanded-store-index
						     new-constraints-table))
	    if clashed-p
	    do (return-from forward-checking-or-constraints-1 t)
	    else
	    nconc added-or-constraints into all-added-or-constraints
	    and
	    nconc removed-or-constraints into all-removed-or-constraints
	    and
	    nconc new-deterministic-or-constraints into all-new-deterministic-or-constraints
	    finally
	      (return (return-forward-checking-values state
						      all-added-or-constraints
						      all-removed-or-constraints
						      added-unexpanded-disjunctive-constraints
						      all-new-deterministic-or-constraints
						      unexpanded-disjunctive-constraints-store))))
    (multiple-value-bind (clashed-p 
			  added-or-constraints
			  removed-or-constraints
			  new-deterministic-or-constraints)
	(forward-checking-or-constraints-1-1 new-unexpanded-disjunctive-constraints
					     relation-constraints
					     update-constraint-dependencies
					     or-level
					     use-alternate-models
					     subsumed-concept-p
					     concept-clash-p
					     use-expanded-store-p
					     expanded-constraints
					     expanded-store
					     expanded-store-index
					     new-constraints-table)
      (or clashed-p
	  (return-forward-checking-values state
					  added-or-constraints
					  removed-or-constraints
					  added-unexpanded-disjunctive-constraints
					  new-deterministic-or-constraints
					  unexpanded-disjunctive-constraints-store)))))

(defun forward-checking-or-constraints-1-1 (unexpanded-disjunctive-constraints
					    relation-constraints
                                                 update-constraint-dependencies
                                                 or-level
                                                 use-alternate-models
                                                 subsumed-concept-p
                                                 concept-clash-p
                                                 use-expanded-store-p
                                                 expanded-constraints
                                                 expanded-store
                                                 expanded-store-index
					  new-constraints-table)
  (loop for or-constraint in unexpanded-disjunctive-constraints
        for open-clauses-counter = (constraint-open-clauses-counter or-constraint)
        for new-or-constraint = nil
        if (deterministic-or-constraint-p open-clauses-counter)
        collect or-constraint into new-deterministic-or-constraints
        and collect or-constraint into removed-or-constraints
        else 
        unless (fully-expanded-or-constraint-p open-clauses-counter)
        do
        (setf new-or-constraint
              (forward-checking-or-constraint or-constraint
                                              new-constraints-table
                                              relation-constraints
                                              update-constraint-dependencies
                                              or-level
                                              use-alternate-models
                                              subsumed-concept-p
                                              concept-clash-p
                                              nil
                                              use-expanded-store-p
                                              expanded-constraints
                                              expanded-store
                                              expanded-store-index))
        and
        unless (eq or-constraint new-or-constraint)
        if (null new-or-constraint)
        do (return-from forward-checking-or-constraints-1-1 t)
        else if (deterministic-or-constraint-p
                 (constraint-open-clauses-counter new-or-constraint))
        collect new-or-constraint into new-deterministic-or-constraints
        and collect or-constraint into removed-or-constraints
        else collect new-or-constraint into added-or-constraints
        and collect or-constraint into removed-or-constraints
        finally
        (return (values nil
			added-or-constraints
			removed-or-constraints
			new-deterministic-or-constraints))))

(defun forward-checking-or-constraints-2 (state
                                                 relation-constraints
                                                 new-unexpanded-disjunctive-constraints
                                                 added-unexpanded-disjunctive-constraints
                                                 unexpanded-disjunctive-constraints-store
                                                 update-constraint-dependencies
                                                 or-level
                                                 use-alternate-models
                                                 subsumed-concept-p
                                                 concept-clash-p
                                                 use-expanded-store-p
                                                 expanded-constraints
                                                 expanded-store
                                                 expanded-store-index)
  (loop for or-constraint in new-unexpanded-disjunctive-constraints
        for open-clauses-counter = (constraint-open-clauses-counter or-constraint)
        for new-or-constraint = nil
        if (deterministic-or-constraint-p open-clauses-counter)
        collect or-constraint into new-deterministic-or-constraints
        and collect or-constraint into removed-or-constraints
        else 
        unless (fully-expanded-or-constraint-p open-clauses-counter)
        do
        (setf new-or-constraint
              (forward-checking-or-constraint or-constraint
                                              nil
                                              relation-constraints
                                              update-constraint-dependencies
                                              or-level
                                              use-alternate-models
                                              subsumed-concept-p
                                              concept-clash-p
                                              t
                                              use-expanded-store-p
                                              expanded-constraints
                                              expanded-store
                                              expanded-store-index))
        and
        unless (eq or-constraint new-or-constraint)
        if (null new-or-constraint)
        do (return-from forward-checking-or-constraints-2 t)
        else if (deterministic-or-constraint-p
                 (constraint-open-clauses-counter new-or-constraint))
        collect new-or-constraint into new-deterministic-or-constraints
        and collect or-constraint into removed-or-constraints
        else collect new-or-constraint into added-or-constraints
        and collect or-constraint into removed-or-constraints
        finally
        (return (return-forward-checking-values state
                                                added-or-constraints
                                                removed-or-constraints
                                                added-unexpanded-disjunctive-constraints
                                                new-deterministic-or-constraints
                                                unexpanded-disjunctive-constraints-store))))

(defun return-forward-checking-values (state
                                             added-or-constraints
                                             removed-or-constraints
                                             added-unexpanded-disjunctive-constraints
                                             new-deterministic-or-constraints
                                             unexpanded-disjunctive-constraints-store)
  (let ((count (max (length removed-or-constraints)
                    (length added-or-constraints))))
    (if (zerop count)
      (values nil ;no clash
              nil ;no new or-constraints
              added-unexpanded-disjunctive-constraints
              nil ;no new deterministic or-constraints
              unexpanded-disjunctive-constraints-store
              nil ;no removed or-constraints
              )
      (if (and *use-unexpanded-disjunctive-constraints-store*
               unexpanded-disjunctive-constraints-store
               (not (constraint-store-unused-p
                     unexpanded-disjunctive-constraints-store)))
        (multiple-value-bind
          (new-added-unexpanded-disjunctive-constraints new-store)
          (remove-constraints-from-constraint-store removed-or-constraints
                                                  added-unexpanded-disjunctive-constraints
                                                  unexpanded-disjunctive-constraints-store
                                                  (state-copy-unexpanded-disjunctive-constraints-store-p state)
                                                  state
                                                  #'reset-disjunctive-copy)
          (values nil
                  added-or-constraints 
                  new-added-unexpanded-disjunctive-constraints
                  new-deterministic-or-constraints
                  new-store
                  removed-or-constraints))
        (values nil
                added-or-constraints
                (constraint-set-difference added-unexpanded-disjunctive-constraints
                                           removed-or-constraints)
                new-deterministic-or-constraints
                unexpanded-disjunctive-constraints-store
                removed-or-constraints)))))

(defconstant +unknown+ '?
  "Represents unknown truth value of open or clauses")

(defun forward-checking-or-constraint (or-constraint
                                            new-constraints-table
                                            relation-constraints
                                            update-constraint-dependencies
                                            or-level
                                            use-alternate-models
                                            subsumed-concept-p
                                            concept-clash-p
                                            use-expanded-constraints-only-p
                                            use-expanded-store-p
                                            expanded-constraints
                                            expanded-store
                                            expanded-store-index)
  "Return list of possibly updated or-constraints. See also comment for
forward-checking-or-constraints."
  #+:debug (assert (not (deterministic-or-constraint-p
                         (constraint-open-clauses-counter or-constraint))))
  (loop with new-or-constraint = or-constraint
        with modified = nil
        with or-constraint-ind = (constraint-ind or-constraint)
        with is-list = (listp new-constraints-table)
        with use-expanded-store-p = (and use-expanded-constraints-only-p
                                         use-expanded-store-p)
        with ind = (constraint-ind or-constraint)
        for new-constraint in (if use-expanded-constraints-only-p
                                (if use-expanded-store-p
                                  (collect-ind-selected-constraints
                                   or-constraint-ind
                                   expanded-constraints
                                   expanded-store
                                   expanded-store-index)
                                  expanded-constraints)
                                (if is-list
                                  new-constraints-table
                                  (gethash or-constraint-ind new-constraints-table)))
        when (or (not is-list)
                 use-expanded-store-p
                 (eql ind (constraint-ind new-constraint)))
        do
        (loop with constraint-concept = (constraint-term new-constraint)
              with comparable-concept = (if (constraint-negated-p
                                             new-constraint)
                                          (concept-negated-concept
                                           constraint-concept)
                                          constraint-concept)
              with or-clauses-truth-values = (constraint-or-clauses-truth-value new-or-constraint)
              for concept in (concept-term (concept-negated-concept
                                            (constraint-term or-constraint)))
              for index from 0
              for clashed-p = nil
              for or-clause-truth-value across or-clauses-truth-values
              when (and (eq or-clause-truth-value +unknown+)
                        (or (funcall subsumed-concept-p comparable-concept concept)
                            (setf clashed-p
                                  (funcall concept-clash-p comparable-concept concept))))
              do
              (progn
                (unless modified
                  (setf new-or-constraint (copy-or-constraint or-constraint))
                  (setf (constraint-or-clauses-truth-value new-or-constraint)
                        (copy-seq (constraint-or-clauses-truth-value or-constraint)))
                  (setf (constraint-or-level new-or-constraint) or-level)
                  (race-trace ("~&new or-constraint generated ~S~%" new-or-constraint))
                  (setf modified t))
                ;(break "1")
                (when update-constraint-dependencies
                  (push new-constraint (constraint-dependencies new-or-constraint)))
                (if clashed-p
                  (progn
                    (decf (constraint-open-clauses-counter
                           new-or-constraint))
                    ;(break "2")
                    #+:debug
                    (when (< (constraint-open-clauses-counter new-or-constraint)
                             +satisfied-marker+)
                      (error "confusion in generating new or-constraint ~S"
                             new-or-constraint))
                    (set-or-clause-truth-value new-or-constraint index nil)
                    (when (zerop (constraint-open-clauses-counter new-or-constraint))
                      (setf (constraint-open-clauses-counter new-or-constraint)
                            +clashed-marker+)
                      (setf (constraint-other-or-clauses-dependencies
                             new-or-constraint)
                            (union-dependencies
                             (constraint-other-or-clauses-dependencies
                              new-or-constraint)
                             (constraint-or-dependencies new-constraint)))
                      (handle-clash new-or-constraint
                                    (remove new-or-constraint
                                            (constraint-other-or-clauses-dependencies
                                             new-or-constraint))
                                    relation-constraints
                                    new-constraint
                                    (remove new-or-constraint
                                            (constraint-or-dependencies new-or-constraint)))
                      ;(break)
                      (return-from forward-checking-or-constraint nil))
                    (setf (constraint-other-or-clauses-dependencies
                           new-or-constraint)
                          (union-dependencies
                           (constraint-other-or-clauses-dependencies
                            new-or-constraint)
                           (constraint-or-dependencies new-constraint)
                           )))
                  (progn
                    ;(break "3a")
                    (setf (constraint-open-clauses-counter new-or-constraint)
                          +satisfied-marker+)
                    (set-or-clause-truth-value new-or-constraint index t)
                    (when use-alternate-models
                      (setf (constraint-derived-from new-constraint)
                            (union (constraint-derived-from new-or-constraint)
                                   (constraint-derived-from new-constraint))))
                    (return)))
                ;(break "3c")
                ))
        
        when (fully-expanded-or-constraint-p
              (constraint-open-clauses-counter new-or-constraint))
        do (return new-or-constraint)
        finally (return new-or-constraint)))

