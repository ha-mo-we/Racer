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

(defmacro make-gci-dependency (type &key removed added)
  `(make-gci-dependency-internal ',type ,removed ,added))

(defun transform-gcis-to-defined-concepts (gcis cyclic-concepts)
  (with-race-trace-sublevel ("transform-gcis-to-defined-concepts"
                             :arguments (list gcis cyclic-concepts)
                             :expanded nil
                             :trace-result t)
    (let* ((ant-index (racer-make-hash-table :size (length gcis) :structure-p t))
           (top *top-concept*)
           (bottom *bottom-concept*)
           (gcis (mapcar (lambda (gci)
                           (if (concept-p-internal gci)
                             (let ((term (concept-term gci)))
                               (if (and (or-concept-p gci)
                                        (eql (length term) 2))
                                 (list (concept-negated-concept (first term))
                                       (second term))
                                 (list top gci)))
                             gci))
                         gcis)))
      (loop for gci in gcis
            for ant = (first gci) do
            (push gci (gethash ant ant-index)))
      (loop with record-gci-absorption-dependencies = *record-gci-absorption-dependencies*
	    for gci in gcis
            for (ant con) = gci
            with deleted-gcis = nil
            do
            (when (and (atomic-concept-p con)
                       (concept-primitive-p con)
                       (if (atomic-concept-p ant)
                         (not (or (eq ant top) (eq con top) (eq ant bottom)))
                         (not (or (eq con top) (eq con bottom))))
                       (not (member con cyclic-concepts))
                       (not (refers-to con ant)))
              (let ((match (gethash con ant-index)))
                (if match
                  (let ((unique (and match (null (rest match)))))
                    (when (and unique
                               (equal ant (second (first match)))
                               (null (concept-encoded-definition con)))
                      (when (and (atomic-concept-p ant)
                                 (or (eq con top) (eq con bottom)))
                        (rotatef ant con))
                      #+:debug (assert (not (or (eq con top) (eq con bottom))))
                      (setf (concept-primitive-p con) nil)
                      (setf (concept-definition con) (decode-concept ant))
                      (setf (concept-encoded-definition con)
                            (encode-concept-term (concept-definition con) con))
                      (push gci deleted-gcis)
                      (push (first match) deleted-gcis)
		      (when record-gci-absorption-dependencies
			(push (make-gci-dependency gci-pair-to-defined
						   :removed (list gci (first match))
						   :added con)
			      (concept-gci-dependencies con)))
                      (race-trace ("~&Absorbing 2 GCIs (~S,~S) into defined concept ~S~%"
                                   gci match con))))
                  (when (and (equal ant (concept-encoded-definition con))
                             (not (refers-to (concept-encoded-definition con) con)))
                    (setf (concept-primitive-p con) nil)
                    (push gci deleted-gcis)
		    (when record-gci-absorption-dependencies
		      (push (make-gci-dependency gci-pair-to-defined
						 :removed (list gci) 
						 :added con)
			    (concept-gci-dependencies con)))
                    (race-trace ("~&Absorbing 2 GCIs into defined concept ~S~%" con))))))
            finally
            (return (if deleted-gcis
                      (set-difference gcis deleted-gcis)
                      gcis))))))

(defun split-gcis (concept-list)
  (with-race-trace-sublevel ("split-gcis"
                             :arguments (list concept-list)
                             :expanded nil
                             :trace-result t)
    (loop with resulting-concepts = concept-list
          do (multiple-value-bind (modified-p transformed-concepts)
                                  (split-gcis-internal resulting-concepts)
               (if modified-p
                 (setf resulting-concepts transformed-concepts)
                 (return resulting-concepts))))))

(defun split-gcis-internal (concept-list)
  "Returns 2 values: modified-p, transformed-concepts"
  (loop with deleted-concepts = nil
        with added-concepts = nil
        with record-gci-absorption-dependencies = *record-gci-absorption-dependencies*
        for concept in concept-list do
        (typecase concept
          (or-concept
           (loop with disjuncts = (concept-term concept)
                 for disjunct in disjuncts
                 when (and-concept-p disjunct) do
                 (let* ((conjuncts (concept-term disjunct))
                        (found (find-if #'(lambda (conjunct)
                                            (or (or-concept-p conjunct)
                                                (negated-concept-p conjunct)))
                                        conjuncts)))
                   (when found
		     (let ((gci-dependency nil))
		       (when record-gci-absorption-dependencies
			 (setf gci-dependency (make-gci-dependency defined-to-gci-pair
                                                                   :removed concept))
			 (push gci-dependency (concept-gci-dependencies concept)))
		       (race-trace ("~&Breaking conjunct of GCI ~S into parts~%" concept))
		       (loop with new-disjuncts = (remove disjunct disjuncts)
			   for conjunct in conjuncts
			   for new-concept = (encode-concept-term `(or ,conjunct . ,new-disjuncts))
			   do
			     (when record-gci-absorption-dependencies
			       (push new-concept (gci-dependency-added gci-dependency)))
			     (push new-concept added-concepts))
		       (push concept deleted-concepts)
		       (return))))))
          (and-concept
           (loop with conjuncts = (concept-term concept)
                 for conjunct in conjuncts
                 when (or (or-concept-p conjunct) (negated-concept-p conjunct)) do
		 (when record-gci-absorption-dependencies
		   (push (make-gci-dependency defined-to-gci-pair :removed concept :added conjuncts)
			 (concept-gci-dependencies concept)))
		 (race-trace ("~&Breaking conjunctive GCI ~S into parts~%" concept))
                 (setf added-concepts (append conjuncts added-concepts))
                 (push concept deleted-concepts)
                 (return))))
        finally
        (if deleted-concepts
          (return (values t (union added-concepts
                                   (set-difference concept-list deleted-concepts))))
          (return (values nil concept-list)))))

(defun negated-atomic-concept-p (concept)
  (and (negated-concept-p concept) (atomic-concept-p (concept-term concept))))

(defun convert-defined-concepts-in-gcis-to-primitives (tbox-el+-p concept-list)
  (with-race-trace-sublevel ("convert-defined-concepts-in-gcis-to-primitives"
                             :arguments (list tbox-el+-p concept-list)
                             :expanded nil
                             :trace-result t)
    (loop with added-gcis = nil
	  with record-gci-absorption-dependencies = *record-gci-absorption-dependencies*
          for concept in concept-list
          for or-list = (when (or-concept-p concept)
                          (concept-term concept))
          for atomic-concept =
          (when or-list
            (loop for disjunct in or-list
                  when (and (negated-atomic-concept-p disjunct)
                            (not (concept-primitive-p (concept-term disjunct))))
                  do (return (concept-term disjunct))))
          do
          (when atomic-concept 
            (let* ((encoded-definition (concept-encoded-definition atomic-concept)))
              (when (and encoded-definition
                         (or tbox-el+-p
                             (not (atomic-concept-p encoded-definition))
                             (member +top-symbol+ (concept-name-set encoded-definition))))
		(let ((new-concept-1
                       (encode-concept-term `(or ,(concept-negated-concept encoded-definition)
                                                 ,atomic-concept)))
                      (new-concept-2
                       (when tbox-el+-p
                         (encode-concept-term `(or ,encoded-definition
                                                   ,(concept-negated-concept atomic-concept))))))
		  (setf (concept-primitive-p atomic-concept) t)
		  (push new-concept-1 added-gcis)
                  (when new-concept-2
                    (setf (concept-definition atomic-concept) nil)
                    (setf (concept-encoded-definition atomic-concept) nil)
                    (push new-concept-2 added-gcis)
		    (when record-gci-absorption-dependencies
		      (push (make-gci-dependency defined-to-prim-and-gci :added new-concept-2)
			    (concept-gci-dependencies atomic-concept))))
		  (when record-gci-absorption-dependencies
		    (push (make-gci-dependency defined-to-prim-and-gci :added new-concept-1)
			  (concept-gci-dependencies atomic-concept)))
                  (if new-concept-2
		    (race-trace ("~&Converting defined concept ~S = ~S to 2 GCIs ~S ~S~%"
			         atomic-concept encoded-definition new-concept-1 new-concept-2))
		    (race-trace ("~&Converting defined concept ~S = ~S to primitive ~%~
                                  and adding GCI ~S ~%"
			         atomic-concept encoded-definition new-concept-1)))))))
          finally
          (if added-gcis
            (return (nconc added-gcis concept-list))
            (return concept-list)))))

(defun convert-defined-tbox-concepts-to-primitives (tbox concept-list &optional (tbox-el+-p nil))
  (with-race-trace-sublevel ("convert-defined-tbox-concepts-to-primitives"
                             :arguments (list tbox concept-list tbox-el+-p)
                             :expanded nil
                             :trace-result t)
    (loop with added-gcis = nil
	  with record-gci-absorption-dependencies = *record-gci-absorption-dependencies*
          for atomic-concept in (tbox-encoded-concept-list tbox)
          do
          (unless (concept-primitive-p atomic-concept)
            (let ((encoded-definition (concept-encoded-definition atomic-concept)))
              (setf (concept-primitive-p atomic-concept) t)
              (setf (concept-definition atomic-concept) nil)
              (setf (concept-encoded-definition atomic-concept) nil)
              (if tbox-el+-p
                  (let ((gci-1 (list encoded-definition atomic-concept))
                        (gci-2 (list atomic-concept encoded-definition)))
                    (push gci-1 added-gcis)
                    (push gci-2 added-gcis)
                    (race-trace ("~&Converting defined EL+ concept ~S = ~S to 2 GCIs: ~S ~S~%"
                                 atomic-concept encoded-definition gci-1 gci-2)))
                (let ((new-concept-1
                       (encode-concept-term `(or ,(concept-negated-concept encoded-definition)
                                                 ,atomic-concept)))
                      (new-concept-2
                       (encode-concept-term `(or ,encoded-definition
                                                 ,(concept-negated-concept atomic-concept)))))
                  (push new-concept-1 added-gcis)
                  (push new-concept-2 added-gcis)
                  (when record-gci-absorption-dependencies
                    (push (make-gci-dependency defined-to-prim-and-gci :added new-concept-1)
                          (concept-gci-dependencies atomic-concept))
                    (push (make-gci-dependency defined-to-prim-and-gci :added new-concept-2)
                          (concept-gci-dependencies atomic-concept)))
                  (race-trace ("~&Converting defined concept ~S = ~S to 2 GCIs: ~S ~S~%"
                               atomic-concept encoded-definition new-concept-1 new-concept-2))))))
          finally
          (if added-gcis
            (return (nconc added-gcis concept-list))
            (return concept-list)))))

(defun asserted-disjoints (candidate)
  (cond ((negated-atomic-concept-p candidate)
         (list (concept-term candidate)))
        ((and (and-concept-p candidate)
              (every #'negated-atomic-concept-p
                     (concept-term candidate)))
         (mapcar #'concept-term (concept-term candidate)))
        (t nil)))

(defun nary-gci-absorbed-p (tbox-el+-p
                            gci-concept
                            with-unfolding-p
                            deterministic-only
                            &key
                            (check-only nil)
                            (single-concept nil)
                            (use-nary-absorption t))
  ;;; returns 5 values: absorbed-p, nary-absorbed-p, modified-concept, lhs, rhs
  (let ((concept-set (if (or-concept-p gci-concept)
                       (copy-list (concept-term gci-concept))
                       (list gci-concept))))
    ;(break "~S" gci-concept)
    (loop with record-gci-absorption-dependencies = *record-gci-absorption-dependencies*
          with use-nary-absorption = (and use-nary-absorption *use-nary-absorption* *transform-gcis*)
          for modified-p = nil do
          (loop for elem-set on concept-set
                for element = (first elem-set) do
                (cond
                 ((negated-atomic-concept-p element)
                  (let ((concept (concept-term element)))
                    (when (or (not single-concept) (eq single-concept concept))
                      (let* ((other-negated-atomic-concepts
                              (when (and (not single-concept)
                                         use-nary-absorption
                                         (concept-primitive-p concept)
                                         (null (concept-encoded-negated-definition concept)))
                                (loop for element in (rest elem-set)
                                      when (and (negated-atomic-concept-p element)
                                                (let ((concept (concept-term element)))
                                                  (and (concept-primitive-p concept)
                                                       (null (concept-encoded-negated-definition concept)))))
                                      collect element)))
                             (remaining-disjuncts
                              (when other-negated-atomic-concepts
                                (concept-set-difference concept-set
                                                        (cons element other-negated-atomic-concepts)))))
                        (if (and other-negated-atomic-concepts
                                 (or tbox-el+-p
                                     (not (null remaining-disjuncts))
                                     (rest other-negated-atomic-concepts)))
                            ;; 1. (or (not A1) ... (not An) C1 ... Cm) ---> (implies (and A1 ... An) (or C1 ... Cm)) or
                            ;; 2. (or (not A1) ... (not An)) ---> (implies (and A1 ... An) *bottom*)
                            ;; n+m >= 3
                            (progn
                              (push element other-negated-atomic-concepts)
                              (when check-only
                                (return-from nary-gci-absorbed-p
                                  (values t
                                          t
                                          nil
                                          (mapcar #'concept-negated-concept
                                                  other-negated-atomic-concepts))))
                              (let ((remaining-disjuncts
                                     (or remaining-disjuncts (list *bottom-concept*))))
                                (loop for negated-concept in other-negated-atomic-concepts do
                                      (setf (concept-nary-unfold-sets (concept-term negated-concept)) t))
                                (when-debug t
                                  (if deterministic-only
                                      (race-trace ("~&Conjunction ~S deterministically absorbed GCI ~S~%"
                                                   (mapcar #'concept-negated-concept
                                                           other-negated-atomic-concepts)
                                                   gci-concept))
                                    (race-trace ("~&Conjunction ~S absorbed GCI ~S~%"
                                                 (mapcar #'concept-negated-concept
                                                         other-negated-atomic-concepts)
                                                 gci-concept))))
                                (return-from nary-gci-absorbed-p
                                  (values t
                                          t
                                          nil
                                          (mapcar #'concept-negated-concept
                                                  other-negated-atomic-concepts)
                                          remaining-disjuncts))))
                          (let ((old-definition (concept-definition concept)))
                            (if (concept-primitive-p concept)
                                ;; case 2
                                (when (null (concept-encoded-negated-definition concept))
                                  ;; 1. (or (not A) C1 ... Cn) ---> (implies A (or C1 ... Cn)) or
                                  ;; 2. (or (not A)) ---> (implies A *bottom*)
                                  (when check-only
                                    (return-from nary-gci-absorbed-p (values t nil concept)))
                                  (let* ((concepts (mapcar #'decode-concept (delete element concept-set)))
                                         (absorbed-term (if (rest concepts)
                                                            `(or ,@concepts)
                                                          (if concepts
                                                              (first concepts)
                                                            +bottom-symbol+)))
                                         (new-definition
                                          (if concepts
                                              (if old-definition
                                                  `(and ,absorbed-term ,old-definition)
                                                absorbed-term)
                                            +bottom-symbol+)))
                                    (setf (concept-definition concept) new-definition)
                                    (setf (concept-encoded-definition concept) nil)
                                    (when record-gci-absorption-dependencies
                                      (if deterministic-only
                                          (push (make-gci-dependency primitive-det
                                                                     :added (encode-concept-term
                                                                             absorbed-term))
                                                (concept-gci-dependencies concept))
                                        (push (make-gci-dependency primitive-non-det
                                                                   :added (encode-concept-term absorbed-term))
                                              (concept-gci-dependencies concept))))
                                    (when-debug t
                                      (if deterministic-only
                                          (race-trace ("~&Concept ~S deterministically absorbed GCI ~S: ~
                                                New def=~S, Old def=~S~%"
                                                       concept
                                                       absorbed-term
                                                       new-definition
                                                       old-definition))
                                        (race-trace ("~&Concept ~S absorbed GCI ~S: ~
                                                New def=~S, Old def=~S~%"
                                                     concept
                                                     absorbed-term
                                                     new-definition
                                                     old-definition))))
                                    (return-from nary-gci-absorbed-p (values t nil concept))))
                              ;; case 3
                              (let* ((encoded-definition (concept-encoded-definition concept))
                                     (new-set
                                      (if (and with-unfolding-p
                                               (and-concept-p encoded-definition))
                                          (copy-list (concept-term
                                                      (concept-negated-concept encoded-definition)))
                                        (when (atomic-concept-p encoded-definition)
                                          (list (concept-negated-concept encoded-definition))))))
                                (when new-set
                                  ;; replace defined concept A by its unfolded negated definition
                                  ;; if A = D and (or (not A) C1 ... Cn) ---> (or (not D) C1 ... Cn)
                                  (setf concept-set (nconc new-set (delete element concept-set)))
                                  (setf modified-p t)
                                  (return))))))))))
                 ((and with-unfolding-p (atomic-concept-p element) (not (concept-primitive-p element)))
                  ;; case 4
                  (let* ((encoded-definition (concept-encoded-definition element))
                         (new-set
                          (if (or-concept-p encoded-definition)
                            (copy-list (concept-term encoded-definition))
                            (when (atomic-concept-p encoded-definition)
                              (list encoded-definition)))))
                    (when new-set
                      ;; replace defined concept by A its unfolded definition
                      ;; if A = D and (or A C1 ... Cn) ---> (or D C1 ... Cn)
                      (setf concept-set (nconc new-set (delete element concept-set)))
                      (setf modified-p t)
                      (return))))))
          until (not modified-p))))

(defun convert-primitive-concepts-to-atomics (tbox gcis &optional (tbox-el+-p nil))
  (with-race-trace-sublevel ("convert-primitive-concepts-to-atomics"
                             :arguments (list tbox gcis tbox-el+-p)
                             :expanded nil
                             :trace-result t)
    (loop with added-gcis = nil
          with top = (tbox-top-node tbox)
	  with record-gci-absorption-dependencies = *record-gci-absorption-dependencies*
          for concept in (tbox-encoded-concept-list tbox)
          for encoded-definition = (concept-encoded-definition concept)
          do
          (unless (or (null encoded-definition) (eq encoded-definition top))
            (setf (concept-definition concept) nil)
            (setf (concept-encoded-definition concept) nil)
            (if tbox-el+-p
                (let ((gci (list concept encoded-definition)))
                  (push gci added-gcis)
                  (race-trace ("~&Converting primitive EL+ concept ~S => ~S to atomic ~%~
                                and adding GCI ~S ~%"
                               concept encoded-definition gci)))
              (let ((new-concept
                     (encode-concept-term `(or ,(concept-negated-concept concept)
                                               ,encoded-definition))))
                (push new-concept added-gcis)
                (when record-gci-absorption-dependencies
                  (push (make-gci-dependency primitive-to-atomic-and-gci :added new-concept)
                        (concept-gci-dependencies concept)))
                (race-trace ("~&Converting primitive concept ~S => ~S to atomic ~%~
                            and adding GCI ~S ~%"
                             concept encoded-definition new-concept)))))
          finally
          (if added-gcis
            (return (nconc added-gcis gcis))
            (return gcis)))))

(defun absorb-all-gcis (tbox-el+-p
                        concept-list 
                        with-unfolding-p 
                        deterministic-only 
                        nary-unfolding-table
                        &key
                        (single-concept nil)
                        (use-nary-absorption t))
  (with-race-trace-sublevel ("absorb-all-gcis"
                             :arguments (list tbox-el+-p 
                                              concept-list
                                              with-unfolding-p
                                              deterministic-only
                                              nary-unfolding-table
                                              single-concept
                                              use-nary-absorption)
                             :expanded nil
                             :trace-result t)
    (loop with modified-concepts = nil
          with meta-constraint-concepts = nil
          for concept in concept-list do
          (multiple-value-bind (absorbed-p nary-absorbed-p modified-concept lhs rhs)
              (nary-gci-absorbed-p tbox-el+-p concept with-unfolding-p deterministic-only
                                   :single-concept single-concept
                                   :use-nary-absorption use-nary-absorption)
            (if absorbed-p
              (if nary-absorbed-p
                (progn
                  (unless nary-unfolding-table
                    (setf nary-unfolding-table (racer-make-hash-table :test 'equal :structure-p t)))
                  (push rhs (gethash lhs nary-unfolding-table)))
                (when modified-concept
                  (push modified-concept modified-concepts)))
              (push concept meta-constraint-concepts)))
          finally
          (loop for modified-concept in (concept-set-remove-duplicates modified-concepts) do
                (setf (concept-encoded-definition modified-concept)
                      (encode-concept-term (concept-definition modified-concept)
                                           modified-concept))
                (setf (concept-told-subsumers modified-concept) ;  BEWARE OF DUPLICATES!!!
                      (racer-remove-duplicates 
                       (cons modified-concept
                             (concept-told-subsumers
                              (concept-encoded-definition modified-concept)))))
                (set-language modified-concept))
          (return (values (nreverse (delete *top-concept* meta-constraint-concepts))
                          nary-unfolding-table)))))

(defun regroup-gcis (gci-list)
  (with-race-trace-sublevel ("regroup-gcis"
                             :arguments (list gci-list)
                             :expanded nil
                             :trace-result t)
    (loop for modified-p = nil
	  with record-gci-absorption-dependencies = *record-gci-absorption-dependencies*
	  do
          (loop for gcis on gci-list
                for gci-1 = (first gcis)
                with removed-gcis = nil
                with added-gcis = nil
                with bottom = *bottom-concept*
                if (or-concept-p gci-1)
                do
                (loop for gci-2 in (rest gcis)
                      when (or-concept-p gci-2) do
                      (let* ((or-list-1 (concept-term gci-1))
                             (or-list-2 (concept-term gci-2))
                             (intersection (intersection or-list-1 or-list-2)))
                        (when intersection
			  (let ((new-concept
				 (encode-concept-term
				  `(or ,@intersection
				       (and (or ,@(set-difference or-list-1
								  intersection))
					    (or ,@(set-difference or-list-2
								  intersection)))))))
			    (push gci-1 removed-gcis)
			    (push gci-2 removed-gcis)
			    (push new-concept added-gcis)
			    (setf modified-p t)
			    (when record-gci-absorption-dependencies
			      (push (make-gci-dependency regroup
                                                         :removed (list gci-1 gci-2)
                                                         :added new-concept)
				    (concept-gci-dependencies new-concept)))
			    (race-trace ("~&Combining GCIs ~S and ~S~%into ~S~%"
					 gci-1 gci-2 new-concept))
			    (return)))))
                else
                when (and (atomic-concept-p gci-1)
                          (not (eq gci-1 bottom))
                          (or (eq (concept-definition gci-1) +bottom-symbol+)
                              (eq (concept-encoded-definition gci-1) bottom)))
                do
                (push gci-1 removed-gcis)
                (push bottom added-gcis)
                finally
                (when removed-gcis
                  (setf gci-list (nset-difference gci-list removed-gcis))
                  (setf removed-gcis nil))
                (when added-gcis
                  (setf gci-list (nconc added-gcis gci-list))
                  (setf added-gcis nil)))
          until (not modified-p)
          finally (return gci-list))))

(defun encode-gcis (gcis)
  (concept-set-remove-duplicates
   (loop with top = *top-concept*
         for gci in gcis
         for encoded-gci = (if (listp gci)
                             (encode-concept-term `(or ,(concept-negated-concept (first gci))
                                                       ,(second gci)))
                             gci)
         if (and-concept-p encoded-gci)
         append (concept-term encoded-gci)
         else 
         unless (eq encoded-gci top)
         collect encoded-gci)))

(defvar *ignore-tbox-signature* nil)

(defmacro with-ignored-tbox-signature (&body body)
  `(let ((*ignore-tbox-signature* t))
     ,@body))

(defun tbox-el+-normalization (tbox gci-pairs)
  (with-race-trace-sublevel ("tbox-el+-normalization"
                             :arguments (list tbox gci-pairs)
                             :expanded nil
                             :trace-result t)
    (let ((normalized-gcis-1
           (loop for (lhs rhs) in gci-pairs
                 if (is-top-concept-p lhs)
                 if (or-concept-p rhs)
                 collect 
                 (loop for disjunct in (concept-term rhs) do
                       (when (or (all-concept-p disjunct) (negated-concept-p disjunct))
                         (return (list (concept-negated-concept disjunct)
                                       (remove disjunct (concept-term rhs)))))
                       finally (error "unexpected"))
                 else
                 unless (is-top-concept-p rhs)
                 do
                 (progn
                   #+:debug (assert (all-concept-p rhs))
                   (push (concept-term rhs) (role-range-concept (concept-role rhs))))
                 end
                 else
                 unless (is-bottom-concept-p lhs)
                 collect
                 (progn
                   (list (if (and-concept-p lhs)
                             (concept-term lhs)
                           lhs)
                         (if (and-concept-p rhs)
                             (concept-term rhs)
                           rhs))))))
      (multiple-value-bind (normalized-gcis-2 el+-transformation-table)
          (race-time (el+-normalize-gcis-1 tbox normalized-gcis-1))
        (let ((normalized-gcis-3
               (race-time (el+-normalize-gcis-2 tbox normalized-gcis-2 el+-transformation-table))))
          (absorb-el+-axioms tbox normalized-gcis-3))))))

(defun el+-normalize-gcis-1 (tbox gcis)
  (let ((gcis (race-time (create-nary-disjointness-axioms tbox gcis))))
    (race-time (transform-nary-compositions-to-binary tbox))
    (loop with el+-transformation-table = nil
          with added-gcis = nil
          initially
          (multiple-value-setq (added-gcis el+-transformation-table)
              (split-complex-role-domains tbox el+-transformation-table))
          for old-gcis = (nconc added-gcis gcis) then new-gcis
          for new-gcis = nil
          do
          (multiple-value-setq (new-gcis el+-transformation-table)
              (race-time (replace-lhs-some-concept-conjuncts old-gcis el+-transformation-table)))
          (multiple-value-setq (new-gcis el+-transformation-table)
              (race-time (replace-lhs-complex-some-concept new-gcis el+-transformation-table)))
          until (eq old-gcis new-gcis)
          finally 
          (return (values new-gcis el+-transformation-table)))))


(defun el+-normalize-gcis-2 (tbox normalized-gcis el+-transformation-table)
  (loop with new-normalized-gcis = (nconc (split-complex-role-domains tbox el+-transformation-table)
                                          normalized-gcis)
        for old-gcis = new-normalized-gcis then new-gcis
        for new-gcis = nil
        do
        (multiple-value-setq (new-gcis el+-transformation-table)
            (race-time (split-complex-inclusions old-gcis el+-transformation-table)))
        (multiple-value-setq (new-gcis el+-transformation-table)
            (race-time (replace-rhs-complex-some-concept-conjuncts new-gcis el+-transformation-table)))
        (setf new-gcis (race-time (split-rhs-complex-some-concept-conjunction new-gcis)))
        until (eq old-gcis new-gcis)
        finally (return new-gcis)))

(defun absorb-el+-axioms (tbox gci-pairs)
  (loop with nary-unfolding-table = (tbox-nary-absorption-table tbox)
        for (lhs rhs) in gci-pairs
        do
        #+:debug
        (assert (or (atomic-concept-p rhs)
                    (and (negated-concept-p rhs)
                         (atomic-concept-p lhs)
                         (member (concept-term rhs) (concept-asserted-disjoints lhs)))
                    (and (some-concept-p rhs) (atomic-concept-p (concept-term rhs)))
                    (and (or (listp rhs) (and-concept-p rhs))
                         (loop for conjunct in (if (listp rhs)
                                                   rhs
                                                 (concept-term rhs))
                               thereis
                               (or (atomic-concept-p conjunct)
                                   (and (some-concept-p conjunct)
                                        (atomic-concept-p (concept-term conjunct))))))))

        (etypecase lhs
          (atomic-concept
           #+:debug (assert (not (is-predefined-concept-p lhs)))
           (let ((old-definition (concept-definition lhs)))
             (if (and old-definition
                      (not (or (eq old-definition +krss-top-symbol+)
                               (eq old-definition +top-symbol+))))
                 (if (consp rhs)
                     (setf (concept-definition lhs) (append rhs old-definition))
                   (push rhs (concept-definition lhs)))
               (if (consp rhs)
                   (setf (concept-definition lhs) rhs)
                 (setf (concept-definition lhs) (list rhs))))
             (race-trace ("~&Absorbing EL+ simple inclusion axiom (implies ~S ~S), new RHS for ~S is ~S~%"
                          lhs rhs lhs (concept-definition lhs)))))
          ((or cons and-concept)
           (let ((new-lhs (if (listp lhs)
                              lhs
                            (concept-term lhs))))
             (loop for concept in new-lhs do
                   #+:debug (assert (atomic-concept-p concept))
                   (setf (concept-nary-unfold-sets concept) t))
             (unless nary-unfolding-table
               (setf nary-unfolding-table (racer-make-hash-table :test 'equal :structure-p t)))
             (push rhs (gethash new-lhs nary-unfolding-table))
             (race-trace ("~&EL+ conjunction ~S absorbed RHS ~S ~%" new-lhs rhs))))
          (some-concept
           (let ((qualification (concept-term lhs))
                 (role (concept-role lhs))
                 (rhs-concept (if (listp rhs)
                                  (encode-concept-term `(and .,rhs))
                                rhs)))
             #+:debug (atomic-concept-p qualification)
             (if (is-top-concept-p qualification)
                 (progn
                   (push rhs-concept (role-domain-concept role))
                   (race-trace ("~&Absorbing EL+ simple domain restriction (implies ~S ~S) into role ~S~%"
                                lhs rhs-concept role)))
               (let* ((el+-role-domain-qualifications
                       (concept-elh-role-domain-qualifications qualification))
                      (el+-super-role-domain-qualifications
                       (loop for (role1 . qualifications) in el+-role-domain-qualifications
                             when (and (not (eq role role1)) (subrole-p role role1))
                             append qualifications)))
                 (loop for pair in el+-role-domain-qualifications do
                       (when (subrole-p (car pair) role)
                         (pushnew rhs-concept (cdr pair))))
                 (let ((el+-role-domain-qualification
                        (assoc role el+-role-domain-qualifications)))
                   (if el+-role-domain-qualification
                       (setf (cdr el+-role-domain-qualification)
                             (concept-set-union el+-super-role-domain-qualifications
                                                (cdr el+-role-domain-qualification)))
                     (setf (concept-elh-role-domain-qualifications qualification)
                           (acons role
                                  (adjoin rhs-concept el+-super-role-domain-qualifications)
                                  (concept-elh-role-domain-qualifications
                                   qualification)))))
                 (race-trace ("~&Absorbing EL+ qualified domain restriction (implies ~S ~S) into ~
                                                   role ~S using qualified role domains (~S) ~S~%"
                              lhs rhs-concept role qualification
                              (concept-elh-role-domain-qualifications qualification))))))))
        finally
        (when nary-unfolding-table
          (setf (tbox-nary-absorption-table tbox) nary-unfolding-table)))
  (loop for concept in (append *provisionally-inserted-atomic-concepts* (tbox-encoded-concept-list tbox))
        for definition = (concept-definition concept)
        do
        (when definition
          (when (and (listp definition) (not (member (first definition) '(and or all some not))))
            (setf (concept-definition concept) `(and .,definition)))
          (let ((encoded-definition (encode-concept-term (concept-definition concept))))
            (setf (concept-encoded-definition concept) encoded-definition)
            (encode-concept-term encoded-definition concept))))
  (loop for role in (append *provisionally-inserted-roles* (tbox-encoded-role-list tbox)) do
        (unless (or (is-predefined-role-p role) (role-datatype role))
          (let ((domain (or (role-domain-restriction role) (role-domain-concept role))))
            (when domain
              (if (listp domain)
                  (progn
                    #+:debug (assert (every #'concept-p-internal domain))
                    (setf (role-domain-concept role) `(and .,(mapcar #'decode-concept domain))))
                (progn
                  ;;#+:debug (concept-p-internal domain) SBCL does not seem to like this!
                  (setf (role-domain-concept role) (decode-concept domain))))))
          (let ((range (or (role-range-restriction role) (role-range-concept role))))
            (when range
              (if (listp range)
                  (progn
                    #+:debug (assert (every #'concept-p-internal range))
                    (setf (role-range-concept role) `(and .,(mapcar #'decode-concept range))))
                (progn
                  ;;#+:debug (concept-p-internal range)  SBCL does not seem to like this!
                  (setf (role-range-concept role) (decode-concept range))))))))
  (tbox-nary-absorption-table tbox))


#|(defun create-nary-disjointness-axioms (tbox gcis)
  (loop with bottom = (tbox-bottom-node tbox)
        with created-pairs = (racer-make-hash-table :test 'equal :structure-p t)
        with new-gcis = gcis
        for concept in (tbox-encoded-concept-list tbox)
        for asserted-disjoints = (concept-asserted-disjoints concept)
        do
        (loop for asserted-disjoint in asserted-disjoints
              for key = (cons concept asserted-disjoint)
              do
              (unless (gethash key created-pairs)
                (setf (gethash key created-pairs) t)
                (setf (gethash (cons asserted-disjoint concept) created-pairs) t)
                (push (list (list concept asserted-disjoint) bottom) new-gcis)))
        finally (return new-gcis)))|#

(defun create-nary-disjointness-axioms (tbox gcis)
  (let ((disjoint-set (tbox-disjoint-set tbox)))
    (if (eql (hash-table-count disjoint-set) 0)
        gcis
      (let ((disjoints-table (racer-make-hash-table))
            (new-gcis nil))
        (loop with bottom = (tbox-bottom-node tbox)
              for key-list being the hash-value of disjoint-set
              for key = (sort-concept-list (mapcar #'encode-concept-term key-list))
              do
              (push (list key bottom) new-gcis)
              (loop for elem in key do
                    (setf (gethash elem disjoints-table)
                          (union (remove elem key) (gethash key disjoints-table)))))
        (if (null new-gcis)
            gcis
          (loop with changed-p = nil
                for gci in gcis
                for (lhs rhs) = gci
                for key-list = (when (atomic-concept-p lhs)
                                 (if (negated-concept-p rhs)
                                     (list (concept-term rhs))
                                   (loop with new-rhs = (remove lhs 
                                                                (if (listp rhs)
                                                                    rhs
                                                                  (when (and-concept-p rhs)
                                                                    (concept-term rhs))))
                                         with key-list = nil
                                         for conjunct in new-rhs
                                         do
                                         (if (negated-concept-p conjunct)
                                             (push (concept-term conjunct) key-list)
                                           (return nil))
                                         finally
                                         (return (nreverse key-list)))))
                for key = (when key-list
                            (sort-concept-list key-list))
                if (or (null key) (not (subsetp key (gethash lhs disjoints-table))))
                collect gci into remaining-gcis
                else
                unless changed-p
                do (setf changed-p t)
                finally
                (if changed-p
                    (return (nconc new-gcis remaining-gcis))
                  (return (nconc new-gcis gcis)))))))))


(defun transform-nary-compositions-to-binary (tbox)
  (loop for role in (tbox-encoded-role-list tbox)
        for compositions = (role-compositions role)
        when (and compositions (not (or (is-predefined-role-p role) (role-internal-name-p role))))
        do
        (loop for composition in compositions
              if (rest (rest composition))
              nconc (transform-nary-composition role composition nil)
              into new-compositions
              else collect composition into old-compositions
              finally
              (when new-compositions
                (setf (role-compositions (role-inverse-internal role)) nil)
                (setf (role-compositions role) old-compositions)
                (loop for new-composition in new-compositions do
                      (push (first new-composition) (role-compositions (second new-composition))))
                (loop for new-composition in new-compositions do
                      (pre-encode-role-compositions tbox (second new-composition)))))))


(defun transform-nary-composition (role composition new-compositions)
  ;;; (implies-role (R1 R2 ... Rn) S) --> 
  ;;; (implies-role (R1 R2) U1), (implies-role (U1 R3) U2), ..., (implies-role (Un-2 Rn) S)
  (let ((remainder (rest (rest composition))))
    (if remainder
        (let ((new-role (encode-role-term (gentemp (symbol-name (role-name role))))))
          #+:debug (assert (rest composition))
          (transform-nary-composition role
                                      (cons new-role remainder)
                                      (cons (list (list (first composition) (second composition)) new-role)
                                            new-compositions)))
      (cons (list composition role) new-compositions))))

(defun split-rhs-complex-some-concept-conjunction (gcis)
  ;;; (implies b (and (some r c) a) --> (implies b (some r c)), (implies b a)
  (loop with all-added-gcis = nil
        for gci in gcis
        for (lhs rhs) = gci
        for changed-p = nil
        do
        (when (and (atomic-concept-p lhs) (listp rhs))
          (let ((added-gcis nil))
	    (declare (ignorable added-gcis))
            (loop for conjunct in rhs
                  if (and (some-concept-p conjunct)
                          (not (atomic-concept-p (concept-term conjunct))))
                  do
                  (unless changed-p
                    (setf changed-p t))
                  (let ((new-gci (list lhs conjunct)))
                    (when-race-trace t
                      (push new-gci added-gcis))
                    (push new-gci all-added-gcis))
                  else
                  collect conjunct into remaining-conjuncts
                  finally
                  (when (and changed-p remaining-conjuncts)
                    (let ((new-gci (list lhs (if (rest remaining-conjuncts)
                                                 remaining-conjuncts
                                               (first remaining-conjuncts)))))
                      (when-race-trace t
                        (push new-gci added-gcis))
                      (push new-gci all-added-gcis))
                    (race-trace ("~&Replaced GCI (and,RHS) ~S by GCIs ~S~%" gci added-gcis))))))
        unless changed-p
        collect gci into new-gcis
        finally
        (if all-added-gcis
            (return (nconc all-added-gcis new-gcis))
          (return gcis))))

(defun make-el+-transformation-table ()
  (racer-make-hash-table :test 'equal :structure-p t)
  ;(racer-make-hash-table)
  )

(defun replace-rhs-complex-some-concept-conjuncts (gcis el+-transformation-table)
  ;;; (implies b (some r c)) --> (implies b (some r a)), (implies a c)
  (loop with all-added-gcis = nil
        for gci in gcis
        for (lhs rhs) = gci
        for changed-p = nil
        when (and (atomic-concept-p lhs)
                  (some-concept-p rhs)
                  (not (atomic-concept-p (concept-term rhs))))
        do
        (setf changed-p t)
        (unless el+-transformation-table
          (setf el+-transformation-table (make-el+-transformation-table)))
        (let* ((term (concept-term rhs)) 
               (old-concept (gethash term el+-transformation-table))
               (new-concept (or old-concept
                                (with-ignored-tbox-signature
                                  (encode-concept-term (gentemp))))))
          (unless old-concept
            (setf (gethash term el+-transformation-table) new-concept)
            (setf (concept-visible-p new-concept) nil))
          (unless (gethash new-concept el+-transformation-table)
            (if (and-concept-p term)
                (push (list new-concept (concept-term term)) all-added-gcis)
              (push (list new-concept term) all-added-gcis))
            (setf (gethash new-concept el+-transformation-table) term))
          (push (list lhs (encode-concept-term `(some ,(concept-role rhs) ,new-concept)))
                all-added-gcis)
          (if old-concept
              (race-trace ("~&Replaced GCI (complex-some,RHS) ~S by GCI ~S~%"
                           gci (first all-added-gcis)))
            (race-trace ("~&Replaced GCI (complex-some,RHS) ~S by GCIs ~S and ~S~%"
                         gci (second all-added-gcis) (first all-added-gcis)))))
        unless changed-p
        collect gci into new-gcis
        finally
        (if all-added-gcis
            (return (values (nconc all-added-gcis new-gcis) el+-transformation-table))
          (return (values gcis el+-transformation-table)))))

(defun replace-lhs-some-concept-conjuncts (gcis el+-transformation-table)
  ;;; (implies (and a (some r b)) c) --> (implies (some r b) t), (implies (and a t) c)
  (loop with all-added-gcis = nil
        for gci in gcis
        for (lhs rhs) = gci
        for changed-p = nil
        when (listp lhs)
        do
        (let ((added-gcis nil))
          (declare (ignorable added-gcis))
          (loop with added-atomic-concepts = nil
                with removed-conjuncts = nil
                for conjunct in lhs
                when (some-concept-p conjunct)
                do
                (unless changed-p
                  (setf changed-p t))
                (unless el+-transformation-table
                  (setf el+-transformation-table (racer-make-hash-table)))
                (let* ((old-concept (gethash conjunct el+-transformation-table))
                       (new-concept-1 (or old-concept
                                          (with-ignored-tbox-signature
                                            (encode-concept-term (gentemp))))))
                  (unless old-concept
                    (setf (gethash conjunct el+-transformation-table) new-concept-1)
                    (setf (concept-visible-p new-concept-1) nil))
                  (push new-concept-1 added-atomic-concepts)
                  (push conjunct removed-conjuncts)
                  (unless old-concept
                    (let ((new-gci (list conjunct new-concept-1)))
                      (push new-gci all-added-gcis)
                      (when-race-trace t
                        (push new-gci added-gcis)))))
                finally
                (when added-atomic-concepts
                  (let ((new-gci (list (nconc added-atomic-concepts
                                              (concept-set-difference lhs removed-conjuncts))
                                       rhs)))
                    (push new-gci all-added-gcis)
                    (when-race-trace t
                      (push new-gci added-gcis))
                    (race-trace ("~&Replaced GCI (some,LHS) ~S by GCIs ~S~%" gci added-gcis))))))
        unless changed-p
        collect gci into new-gcis
        finally
        (if all-added-gcis
            (return
             (values (nconc all-added-gcis new-gcis) el+-transformation-table))
          (return (values gcis el+-transformation-table)))))

(defun replace-lhs-complex-some-concept (gcis el+-transformation-table)
  ;;; (implies (some r c) d) --> (implies c a), (implies (some r a) d)
  (loop with added-gcis = nil
        for gci in gcis
        for (lhs rhs) = gci
        for changed-p = nil
        when (and (some-concept-p lhs) (not (atomic-concept-p (concept-term lhs))))
        do
        (setf changed-p t)
        (unless el+-transformation-table
          (setf el+-transformation-table (racer-make-hash-table)))
        (let* ((term (concept-term lhs))
               (old-concept (gethash term el+-transformation-table))
               (new-concept (or old-concept
                                (with-ignored-tbox-signature
                                  (encode-concept-term (gentemp))))))
          (unless old-concept
            (setf (gethash term el+-transformation-table) new-concept)
            (setf (concept-visible-p new-concept) nil)
            (if (and-concept-p term)
                (push (list (concept-term term) new-concept) added-gcis)
              (push (list term new-concept) added-gcis)))
          (push (list (encode-concept-term `(some ,(concept-role lhs) ,new-concept)) rhs)
                added-gcis)
          (if old-concept
              (race-trace ("~&Replaced GCI (complex-some,LHS) ~S by GCI ~S~%"
                           gci (first added-gcis)))
            (race-trace ("~&Replaced GCI (complex-some,LHS) ~S by GCIs ~S ~S~%"
                         gci (first added-gcis) (second added-gcis)))))
        unless changed-p
        collect gci into new-gcis
        finally
        (if added-gcis
            (return (values (nconc added-gcis new-gcis) el+-transformation-table))
          (return (values gcis el+-transformation-table)))))

(defun split-complex-role-domains (tbox el+-transformation-table)
  (loop with added-gcis = nil
        for role in (tbox-encoded-role-list tbox)
        for domain = (role-domain-concept role)
        do
        (when (and domain 
                   (not (or (is-predefined-role-p role)
                            (role-internal-name-p role)
                            (role-datatype role))))
          (let ((domain-restriction (or (role-domain-restriction role)
                                        (encode-concept-term domain))))
            (when (or (some-concept-p domain-restriction)
                      (and (and-concept-p domain-restriction)
                           (dl-some (concept-language domain-restriction))
                           (some #'some-concept-p (concept-term domain-restriction))))
              (unless el+-transformation-table
                (setf el+-transformation-table (make-el+-transformation-table)))
              (let* ((old-concept (gethash domain-restriction el+-transformation-table))
                     (new-concept (or old-concept
                                      (with-ignored-tbox-signature
                                        (encode-concept-term (gentemp))))))
                (setf (role-domain-concept role) (decode-concept new-concept))
                (setf (role-domain-restriction role) new-concept)
                (race-trace ("~&Replacing EL+ role domain ~S for role ~S by ~S~%"
                             domain-restriction role new-concept))
                (unless old-concept
                  (setf (gethash domain-restriction el+-transformation-table) new-concept)
                  (setf (concept-visible-p new-concept) nil)
                  (push (list new-concept
                              (if (and-concept-p domain-restriction)
                                  (concept-term domain-restriction)
                                domain-restriction))
                        added-gcis)
                  (race-trace ("~&Adding EL+ axiom ~S for domain of role ~S~%"
                               (first added-gcis) role)))))))
        finally
        (return (values added-gcis el+-transformation-table))))

(defun proper-complex-inclusion (lhs rhs)
  ; return T if gci represents (implies (some r a) (and b1 ... bn (some s1 c1) ... (some sn cn))) or
  ;                            (implies (and a1 ... an) (and b1 ... bn (some s1 c1) ... (some sn cn))
  (and (or (and (listp rhs) (some #'some-concept-p rhs))
           (some-concept-p rhs))
       (or (listp lhs)
           (and (some-concept-p lhs) (atomic-concept-p (concept-term lhs))))))

(defun split-complex-inclusions (gcis el+-transformation-table)
  ;;; (implies c d) --> (implies c a), (implies a d)
  (loop with all-added-gcis = nil
        for gci in gcis
        for (lhs rhs) = gci
        for changed-p = nil
        do
        (when (proper-complex-inclusion lhs rhs)
          (setf changed-p t)
          (unless el+-transformation-table
            (setf el+-transformation-table (make-el+-transformation-table)))
          (let* ((old-concept (gethash rhs el+-transformation-table))
                 (new-concept (or old-concept
                                  (with-ignored-tbox-signature
                                    (encode-concept-term (gentemp)))))
                 (added-gcis nil)
                 )
	    (declare (ignorable added-gcis))
            (unless old-concept
              (setf (gethash rhs el+-transformation-table) new-concept)
              (setf (concept-visible-p new-concept) nil))
            (unless (gethash new-concept el+-transformation-table)
              (let ((new-gci (list new-concept rhs)))
                (when-race-trace t
                  (push new-gci added-gcis))
                (push new-gci all-added-gcis))
              (setf (gethash new-concept el+-transformation-table) rhs))
            (let ((new-gci (list lhs new-concept)))
              (when-race-trace t
                (push new-gci added-gcis))
              (push new-gci all-added-gcis))
            (race-trace ("~&Replaced GCI (complex-inclusion) ~S by GCIs ~S~%" gci added-gcis))))
        unless changed-p
        collect gci into new-gcis
        finally
        (if all-added-gcis
            (return (values (nconc all-added-gcis new-gcis) el+-transformation-table))
          (return (values gcis el+-transformation-table)))))

(defun transform-gcis (tbox-el+-p
                       gcis
                       use-unfolding
                       deterministic-only
                       nary-unfolding-table)
  (with-race-trace-sublevel ("transform-gcis"
                             :arguments (list tbox-el+-p
                                              gcis
                                              use-unfolding
                                              deterministic-only
                                              nary-unfolding-table)
                             :expanded nil
                             :trace-result t)
    (loop with transformed-gcis = gcis
          with old-transformed-gcis = nil
          while transformed-gcis do
          (setf old-transformed-gcis transformed-gcis)
          (unless tbox-el+-p
            (let ((new-gcis (convert-defined-concepts-in-gcis-to-primitives nil transformed-gcis)))
              (unless (eq transformed-gcis new-gcis)
                (multiple-value-setq (transformed-gcis nary-unfolding-table)
                    (absorb-all-gcis tbox-el+-p 
                                     new-gcis
                                     use-unfolding
                                     deterministic-only
                                     nary-unfolding-table)))))
          (when transformed-gcis
            (let ((new-gcis (split-gcis transformed-gcis)))
              (unless (eq transformed-gcis new-gcis)
                (multiple-value-setq (transformed-gcis nary-unfolding-table)
                    (absorb-all-gcis tbox-el+-p 
                                     new-gcis
                                     use-unfolding
                                     deterministic-only
                                     nary-unfolding-table)))))
          until (eq transformed-gcis old-transformed-gcis)
          finally (return (values transformed-gcis nary-unfolding-table)))))

(defun split-axioms-using-distributive-law (gcis)
  (loop with new-gcis = nil
        with removed-gcis = nil
        for gci in gcis
        do
        (when (or-concept-p gci)
          (let ((gcis (split-axiom-using-distributive-law gci)))
            (when gcis
              (setf new-gcis (nconc gcis new-gcis))
              (push gci removed-gcis))))
        finally
        (if removed-gcis
            (return (nconc new-gcis (concept-set-difference gcis removed-gcis)))
          (return gcis))))

(defun split-axiom-using-distributive-law (gci)
  (let* ((disjuncts (concept-term gci))
         (eligible-conjuncts (axiom-applicable-to-distributive-law disjuncts)))
    (when eligible-conjuncts
      (loop with remaining-disjuncts = (concept-set-difference disjuncts eligible-conjuncts)
            for new-disjunction in (apply-distributive-law eligible-conjuncts)
            collect (encode-concept-term `(or .,(append new-disjunction remaining-disjuncts)))
            into result
            finally
            (when result
              (race-trace ("~&Splitting GCI using distributive law: old=~S, new=~S~%"
                           gci result))
              (return result))))))

(defun apply-distributive-law (conjunctions &optional (result nil))
  (if (null conjunctions)
      result
    (let ((new-result
           (if (null result)
               (mapcar #'list (concept-term (first conjunctions)))
             (loop for conjunct in (concept-term (first conjunctions))
                   nconc
                   (loop for other-conjunction in result
                         collect (cons conjunct other-conjunction))))))
      (apply-distributive-law (rest conjunctions) new-result))))

(defun axiom-applicable-to-distributive-law (disjuncts)
  (loop with axioms-count = 1
        with conjunction-count = 0
        for disjunct in disjuncts
        when (and-concept-p disjunct)
        do 
        (setf axioms-count (* axioms-count (length (concept-term disjunct))))
        (incf conjunction-count)
        and 
        collect disjunct into eligible-conjuncts
        until (> axioms-count 10)
        finally
        (unless (or (eql conjunction-count 0)
                    (> axioms-count 10)
                    (> (* axioms-count (- (length disjuncts) conjunction-count)) 20))
          (return eligible-conjuncts))))

(defun absorb-gci-as-negated-definition (gci-concept 
                                               &optional
                                               (check-only nil)
                                               (used-concepts-table nil))
  (let ((concept-set (if (or-concept-p gci-concept)
                       (if check-only
                         (concept-term gci-concept)
                         (copy-list (concept-term gci-concept)))
                       (list gci-concept))))
    (loop with record-gci-absorption-dependencies = *record-gci-absorption-dependencies*
          with top = *top-concept*
	  for element in concept-set do
          (when (and (not (is-predefined-concept-p element))
                     (atomic-concept-p element)
                     (concept-primitive-p element)
                     (or (null (concept-encoded-definition element))
                         (eq (concept-encoded-definition element) top))
                     (null (concept-nary-unfold-sets element))
                     (null (concept-elh-role-domain-qualifications element)))
            (when check-only
              (if (gethash (first (concept-name-set element)) used-concepts-table)
                (return-from absorb-gci-as-negated-definition nil)
                (return-from absorb-gci-as-negated-definition t)))
            (let* (#+:debug (old-definition (concept-encoded-negated-definition element))
                   (concepts (mapcar #'decode-concept (delete element concept-set)))
                   (added (if (rest concepts)
                            `(or ,@concepts)
                            (if concepts
                              (first concepts)
                              +bottom-symbol+)))
                   (gci-dependency nil))
              #+:debug (assert (listp old-definition))
              (push added (concept-encoded-negated-definition element))
              (setf (concept-encoded-definition element) nil)
              (setf (concept-definition element) nil)
	      (when record-gci-absorption-dependencies
                (setf gci-dependency (make-gci-dependency primitive-negated
                                                          :removed gci-concept 
                                                          :added added))
                (push gci-dependency (concept-gci-dependencies element)))
              (race-trace ("~&Concept (negated def) ~S absorbed GCI ~S: ~
                            Old def=~S, New def=~S~%"
                           element gci-concept old-definition
                           (concept-encoded-negated-definition element)))
              (return (values t element gci-dependency)))))))

(defun absorb-gcis-as-negated-definitions (tbox concept-list)
  (with-race-trace-sublevel ("absorb-gcis-as-negated-definitions"
                             :arguments (list tbox concept-list)
                             :expanded nil
                             :trace-result t)
    (when concept-list
      (loop for concept being the hash-value of (tbox-concept-store tbox) do
            (when (atomic-concept-p concept)
              (let ((definition (concept-encoded-negated-definition concept)))
                (unless (listp definition)
                  (setf (concept-encoded-negated-definition concept) (list definition)))))))
    (loop with removed-concepts = nil
          with modified-concepts = nil
          with gci-dependencies = nil
          for concept in concept-list
          do
          (multiple-value-bind (absorbed-p modified-concept gci-dependency)
                               (absorb-gci-as-negated-definition concept)
            (when absorbed-p
              (push concept removed-concepts)
              (push modified-concept modified-concepts)
              (when gci-dependency
                (push gci-dependency gci-dependencies))))
          finally
          (setf modified-concepts (concept-set-remove-duplicates modified-concepts))
          (when concept-list
            (loop for concept being the hash-value of (tbox-concept-store tbox) do
                  (when (atomic-concept-p concept)
                    (let ((definition (concept-encoded-negated-definition concept)))
                      (when (and (consp definition)
                                 (concept-p-internal (first definition))
                                 (null (rest definition)))
                        (setf (concept-encoded-negated-definition concept) (first definition))))))
            (without-setting-language-of-atomic-concepts ; delayed due to possible forward references
              (loop for concept in modified-concepts do
                    (let ((definition (concept-encoded-negated-definition concept)))
                      #+:debug (assert definition)
                      (unless (concept-p-internal definition)
                        (setf (concept-encoded-negated-definition concept) nil) ; needed to prevent cyclic encoding
                        (setf (concept-encoded-negated-definition concept)
                              (encode-concept-term (if (rest definition)
                                                     (list* 'and definition)
                                                     (first definition))))))))
            (loop for concept in modified-concepts do
                  (set-language (concept-encoded-negated-definition concept))
                  (set-language concept))
            (loop for gci-dependency in gci-dependencies
                  when (gci-dependency-added gci-dependency)
                  do (setf (gci-dependency-added gci-dependency)
                           (encode-concept-term (gci-dependency-added gci-dependency)))))
          (if removed-concepts
            (return (concept-set-difference concept-list removed-concepts))
            (return concept-list)))))

(defun inverse-role-condition-p (concept)
  (and (or-concept-p concept)
       (loop for disjunct in (concept-term concept)
             thereis (or (atomic-concept-p disjunct)
                         (negated-concept-p disjunct)))))

(defun transform-gcis-with-inverse-roles (concept-list)
  (with-race-trace-sublevel ("absorb-gcis-with-inverse-roles"
                             :arguments (list concept-list)
                             :expanded nil
                             :trace-result t)
    (loop with removed-concepts = nil
          with added-concepts = nil
          with gci-dependencies = nil
          for concept in concept-list
          do
          (when (or (or-concept-p concept)
                    (and (all-concept-p concept)
                         (inverse-role-condition-p (concept-term concept))))
            (multiple-value-bind (transformed-p new-concept gci-dependency)
                (transform-gci-with-inverse-roles concept)
              (when transformed-p
                (push concept removed-concepts)
                (push new-concept added-concepts)
                (when gci-dependency
                  (push gci-dependency gci-dependencies)))))
          finally
          (if removed-concepts
              (return 
               (values t (concept-set-union added-concepts
                                            (concept-set-difference concept-list removed-concepts))))
            (return nil)))))

(defun make-inverse-role-all-concept (all-concept level qualification)
  (make-inverse-role-all-concept-1 (collect-inverse-roles all-concept level nil)
                                   qualification))

(defun make-inverse-role-all-concept-1 (role-list qualification)
  (if (null role-list)
    qualification
    `(all ,(first role-list)
          ,(make-inverse-role-all-concept-1 (rest role-list) qualification))))

(defun collect-inverse-roles (all-concept level role-list)
  #+:debug (assert (numberp level))
  (if (eql level 0)
      role-list
    (collect-inverse-roles (concept-term all-concept)
                           (1- level)
                           (cons (role-inverse-internal (concept-role all-concept)) role-list))))

(defun transform-gci-with-inverse-roles (concept)
  ;;pattern: (or d e (all r (not c))) --> (or (not c) (all (inv r) (or d e)))
  ;;pattern: (implies (some r c) (or d e)) --> (implies c (all (inv r) (or d e)))
  #+:allegro (declare (optimize (safety 1) (speed 1)))
  ;;; (safety 0) and (speed 3) cause compiler bug in ACL 8.1
  (let ((term-list (if (or-concept-p concept)
                       (concept-term concept)
                     (list concept))))
    (multiple-value-bind (foundp level term all-concept)
        (loop for elem in term-list do
              (multiple-value-bind (foundp llevel term)
                  (proper-inverse-role-all-concept elem)
                (when foundp
                  #+:debug (assert (numberp llevel))
                  (return (values t llevel term elem)))))
      (when foundp
	#+:allegro (assert (numberp level)) ;needed to avoid a compiler bug in ACL
        (let* ((new-term-list (racer-remove all-concept term-list))
               (atomic-or-other-concept (or (find-if #'atomic-concept-p new-term-list)
                                            (first new-term-list)))
               (new-concept
                (if atomic-or-other-concept
                    (encode-concept-term
                     `(or ,term
                          ,(make-inverse-role-all-concept
                            all-concept
                            level
                            (let ((length (length new-term-list)))
                              (if (> length 2)
                                  `(or ,atomic-or-other-concept
                                       .,(racer-remove atomic-or-other-concept new-term-list))
                                (if (eql length 2)
                                    `(or ,@new-term-list)
                                  atomic-or-other-concept))))))
                  (when (and (all-concept-p concept)
                             (inverse-role-condition-p (concept-term concept)))
                    (encode-concept-term
                     `(or ,term
                          ,(make-inverse-role-all-concept
                            all-concept
                            level
                            +bottom-symbol+)))))))
          (when new-concept
            (race-trace ("~&GCI ~S rewritten to ~S~%" concept new-concept))
            ;(format t "~&GCI ~S rewritten to ~S~%" (decode-concept concept) (decode-concept new-concept))
            (values t new-concept)))))))

(defun proper-inverse-role-all-concept (concept)
  (when (all-concept-p concept)
    (loop for level from 1
          for current-concept = concept then term
          for term = (when (all-concept-p current-concept)
                       (concept-term current-concept))
          do
          (cond ((null term)
                 (return nil))
                ((or (atomic-concept-p term)
                     (negated-concept-p term)
                     (inverse-role-condition-p term))
                 ;(when (inverse-role-condition-p term) (princ "+"))
                 (return (values t level term)))
                ((not (all-concept-p term))
                 (return nil))))))

(defun absorb-domain-range-restrictions (tbox 
                                         concept-list
                                         &key
                                         (pre-absorption nil)
                                         (functional-only nil)
                                         (simple-domain-restrictions t)
                                         (qualified-domain-restrictions t)
                                         (tbox-el+-p nil)
                                         (enforce-range-absorption nil)
                                         (add-qualified-role-domain nil))
  (with-race-trace-sublevel ("absorb-domain-range-restrictions"
                             :arguments (list tbox
                                              concept-list
                                              pre-absorption
                                              functional-only
                                              simple-domain-restrictions
                                              qualified-domain-restrictions
                                              tbox-el+-p
                                              enforce-range-absorption
                                              add-qualified-role-domain)
                             :expanded nil
                             :trace-result t)
    (when-race-trace t
      (if (> (length concept-list) 10000)
          (race-trace ("~&Starting domain/range absorption #GCIs=~D~%"
                       (length concept-list)))
        (race-trace ("~&Starting domain/range absorption #GCIs=~D, GCIs=~S~%"
                     (length concept-list) (copy-list concept-list)))))
    (let ((new-concept-list
           (if pre-absorption
            (loop for elem in concept-list
                  for concept = (if (concept-p-internal elem)
                                    elem
                                  (encode-concept-term `(or (not ,(first elem)) . ,(rest elem))))
                  for and-list = (when (and-concept-p concept)
                                   (flattened-and-concept-list concept))
                  if and-list
                  append and-list
                  else collect concept)
             concept-list)))
      (loop with set-flag = (incf *role-set-mark*)
            for role being the hash-value of (tbox-role-store tbox)
            for domain = (role-domain-concept role)
            for range = (role-range-concept role)
            unless (eql (racer-set-flag role) set-flag)
            do
            ;; We must not apply this operation twice!!!
            ;; This might happen for role synonyms.
            (setf (racer-set-flag role) set-flag)
            (when domain
              (setf (role-domain-concept role) (list domain)))
            (when range
              (setf (role-range-concept role) (list range))))
      (loop with top = (tbox-top-node tbox)
            with datatype-top = (tbox-datatype-top-node tbox)
            with changed = nil
            with changed-roles = nil
            with removed-gcis = nil
	    with new-gcis = nil
	    with record-gci-absorption-dependencies = *record-gci-absorption-dependencies*
            with feature-added = nil
            with reflexivity-added = nil
            with irreflexivity-added = nil
            with absorb-as-backpropagation = (or (and tbox-el+-p
                                                      qualified-domain-restrictions
                                                      *use-elh-transformation*)
                                                 add-qualified-role-domain)
            with use-inverse-role-absorption = *use-inverse-role-absorption*
            for gci in new-concept-list
            do
            #+:debug (assert (and (concept-p-internal gci) (not (and-concept-p gci))))
            (if (not (or-concept-p gci))
                (if (and (all-concept-p gci)
                         (not (concept-self-reference-p gci))
                         (not functional-only)
                         (or (not use-inverse-role-absorption)
                             enforce-range-absorption
                             (not (inverse-role-condition-p (concept-term gci)))
                             (role-reflexive-p (concept-role gci))))
                    (let ((role (concept-role gci))
                          (concept-term (concept-term gci)))
                      #+:debug (assert (listp (role-range-concept role)))
                      (push gci removed-gcis)
                      (if (role-reflexive-p role)
                          (progn
                            #+:debug (assert (listp (role-domain-concept role)))
                            (when record-gci-absorption-dependencies
                              (push (make-gci-dependency reflexive-role-domain-range
                                                         :removed gci :added concept-term)
                                    (role-gci-dependencies role)))
                            (push concept-term new-gcis)
                            (race-trace ("~&Absorbing implicit domain+range restriction (implies TOP ~S) of reflexive role ~S as global axiom ~S ~%"
                                         gci role concept-term)))
                        (progn
                          (when record-gci-absorption-dependencies
                            (push (make-gci-dependency range :removed gci :added concept-term)
                                  (role-gci-dependencies role)))
                          (push (decode-concept concept-term) (role-range-concept role))
                          (pushnew role changed-roles)
                          (race-trace ("~&Absorbing implicit range restriction (implies TOP ~S) into role ~S~%"
                                       gci role))))
                      (setf changed t))
                  (if (at-most-concept-p gci)
                      (if (and (eql (concept-number-restriction gci) 1)
                               (or (eq (concept-term gci) top) (eq (concept-term gci) datatype-top)))
                          (progn
                            (push gci removed-gcis)
                            (setf (role-feature-p (concept-role gci)) t)
                            (when record-gci-absorption-dependencies
                              (push (make-gci-dependency functional :removed gci)
                                    (role-gci-dependencies (concept-role gci))))
                            (race-trace ("~&Absorbing implicit functional restriction (implies TOP ~S) into role ~S~%"
                                         gci (concept-role gci)))
                            (pushnew (concept-role gci) changed-roles)
                            (unless feature-added
                              (setf feature-added t))
                            (setf changed t))
                        (if functional-only
                            (push gci new-gcis)
                          (let ((role (concept-role gci)))
                            #+:debug (assert (listp (role-domain-concept role)))
                            #+:debug (assert (notany #'role-compositions (role-ancestors-internal role)))
                            (push gci removed-gcis)
                            (push (decode-concept gci) (role-domain-concept role))
                            (when record-gci-absorption-dependencies
                              (push (make-gci-dependency simple-domain :removed gci :added gci)
                                    (role-gci-dependencies role)))
                            (race-trace ("~&Absorbing implicit domain restriction (implies TOP ~S) into role ~S~%"
                                         gci role))
                            (pushnew role changed-roles)
                            (setf changed t))))
                    (if (and (some-concept-p gci) (concept-self-reference-p gci))
                        (let ((role (concept-role gci)))
                          (push gci removed-gcis)
                          (if (role-reflexive-p role)
                              (race-trace ("~&Ignoring redundant reflexivity restriction (implies TOP ~S) for reflexive role ~S~%"
                                           gci role))
                            (progn 
                              (setf (role-reflexive-p role) t)
                              (setf (role-reflexive-p (role-inverse-internal role)) t)
                              (when record-gci-absorption-dependencies
                                (push (make-gci-dependency reflexive :removed gci)
                                      (role-gci-dependencies role)))
                              (race-trace ("~&Absorbing implicit reflexivity restriction (implies TOP ~S) into role ~S~%"
                                           gci role))
                              (pushnew role changed-roles)
                              (push role reflexivity-added)
                              (setf changed t))))
                      (if (and (all-concept-p gci) (concept-self-reference-p gci))
                        (let ((role (concept-role gci)))
                          (push gci removed-gcis)
                          (if (role-irreflexive-p role)
                              (race-trace ("~&Ignoring redundant irreflexivity restriction (implies TOP ~S) for irreflexive role ~S~%"
                                           gci role))
                            (progn 
                              (setf (role-irreflexive-p role) t)
                              (setf (role-irreflexive-p (role-inverse-internal role)) t)
                              (when record-gci-absorption-dependencies
                                (push (make-gci-dependency irreflexive :removed gci)
                                      (role-gci-dependencies role)))
                              (race-trace ("~&Absorbing implicit irreflexivity restriction (implies TOP ~S) into role ~S~%"
                                           gci role))
                              (pushnew role changed-roles)
                              (push role irreflexivity-added)
                              (setf changed t))))
                    (push gci new-gcis)))))
              (if functional-only
                  (push gci new-gcis)
                (let* ((all-concept (when (dl-all (concept-language gci))
                                      (find-domain-restriction (concept-term gci) pre-absorption)))
                       (some-concept (when all-concept
                                       (concept-negated-concept all-concept))))
                  (if some-concept
                      (let* ((reduced-gci-term (remove all-concept (concept-term gci)))
                             (term (concept-term some-concept))
                             (true-some-concept-p (some-concept-p some-concept))
                             (role-composition-p (and true-some-concept-p
                                                      (some #'role-compositions
                                                            (role-ancestors-internal (concept-role some-concept))))))
                        (if (and simple-domain-restrictions 
                                 true-some-concept-p
                                 (or (not role-composition-p)
                                     ;; we cannot absorb a domain restriction if the role is defined by a composition
                                     ;; unless the tbox has been EL+ transformed
                                     (tbox-el+-transformed-table tbox))
                                 (or (eq term top) (eq term datatype-top)))
                            (let* ((role (concept-role some-concept))
                                   (concepts (mapcar #'decode-concept reduced-gci-term))
                                   (right (if (rest concepts)
                                              `(or . ,concepts)
                                            (first concepts)))
                                   (removed
                                    (if (concept-p-internal gci)
                                        (list some-concept (encode-concept-term right))
                                      gci)))
                              (push removed removed-gcis)
                              #+:debug (assert (listp (role-domain-concept role)))
                              #+:debug (assert (or (tbox-el+-transformed-table tbox)
                                                   (notany #'role-compositions (role-ancestors-internal role))))
                              (push right (role-domain-concept role))
                              (when record-gci-absorption-dependencies
                                (push (make-gci-dependency simple-domain :removed removed :added right)
                                      (role-gci-dependencies role)))
                              (race-trace ("~&Absorbing implicit domain restriction (implies ~S ~S) into role ~S~%"
                                           some-concept right role))
                              (pushnew role changed-roles)
                              (setf changed t))
                          (if (or (and simple-domain-restrictions qualified-domain-restrictions)
                                  add-qualified-role-domain)
                              (let* ((role (concept-role some-concept))
                                     (right `(or . ,(mapcar #'decode-concept reduced-gci-term)))
                                     (removed (if (concept-p-internal gci)
                                                  (list some-concept (encode-concept-term right))
                                                gci))
                                     (qualification-concept
                                      (concept-negated-concept (concept-term all-concept)))
                                     (atomic-p (atomic-concept-p qualification-concept))
                                     (el+-role-domain-qualifications
                                      (when atomic-p
                                        (concept-elh-role-domain-qualifications qualification-concept)))
                                     (rhs-concept (first reduced-gci-term)))
                                (if (and absorb-as-backpropagation
                                         true-some-concept-p
                                         atomic-p
                                         (null (rest reduced-gci-term))
                                         (not (is-predefined-concept-p qualification-concept))
                                         (not (member rhs-concept 
                                                      (cdr (assoc role el+-role-domain-qualifications)))))
                                    (let ((el+-super-role-domain-qualifications
                                           (loop for (role1 . qualifications) in el+-role-domain-qualifications
                                                 when (and (not (eq role role1)) (subrole-p role role1))
                                                 append qualifications)))
                                      #+:debug (assert (not (is-top-concept-p qualification-concept)))
                                      (loop for pair in el+-role-domain-qualifications
                                            do
                                            (when (subrole-p (car pair) role)
                                              (pushnew rhs-concept (cdr pair))))
                                      (let ((el+-role-domain-qualification
                                             (assoc role el+-role-domain-qualifications)))
                                        (if el+-role-domain-qualification
                                            (setf (cdr el+-role-domain-qualification)
                                                  (concept-set-union el+-super-role-domain-qualifications
                                                                     (cdr el+-role-domain-qualification)))
                                          (setf (concept-elh-role-domain-qualifications qualification-concept)
                                                (acons role
                                                       (adjoin rhs-concept el+-super-role-domain-qualifications)
                                                       (concept-elh-role-domain-qualifications
                                                        qualification-concept)))))
                                      (if (and tbox-el+-p *use-elh-transformation*)
                                          (progn
                                            (race-trace ("~&Absorbing EL+ qualified domain restriction (implies ~S ~S) ~
                                                         into role ~S using qualified role domains (~S) ~S~%"
                                                         some-concept rhs-concept role
                                                         qualification-concept
                                                         (concept-elh-role-domain-qualifications qualification-concept)))
                                            (push removed removed-gcis)
                                            (setf changed t))
                                        (progn
                                          (race-trace ("~&Adding EL+ qualified domain restriction (implies ~S ~S) ~
                                                       to role ~S using qualified role domains (~S) ~S~%"
                                                       some-concept rhs-concept role
                                                       qualification-concept
                                                       (concept-elh-role-domain-qualifications qualification-concept)))
                                          (push gci new-gcis))))
                                  (if (or role-composition-p (not qualified-domain-restrictions))
                                      (push gci new-gcis)
                                    (progn
                                      (push removed removed-gcis)
                                      #+:debug (assert (listp (role-domain-concept role)))
                                      #+:debug (assert (notany #'role-compositions (role-ancestors-internal role)))
                                      (push (decode-concept gci) (role-domain-concept role))
                                      (when record-gci-absorption-dependencies
                                        (push (make-gci-dependency qualified-domain :removed removed :added right)
                                              (role-gci-dependencies role)))
                                      (race-trace ("~&Absorbing implicit qualified domain restriction (implies ~S ~S) into role ~S~%"
                                                   some-concept right role))
                                      (pushnew role changed-roles)
                                      (setf changed t)))))
                            (push gci new-gcis))))
                    (let* ((cd-concept (find-if #'cd-concept-p (concept-term gci)))
                           (cd-all-concept
                            (and cd-concept
                                 (eq (predicate-operator (concept-predicate cd-concept))
                                     'bottom))))
                      (if cd-all-concept
                          (let* ((role (get-tbox-role tbox (first (predicate-parameters (concept-predicate cd-concept)))))
                                 (right `(or . ,(mapcar #'decode-concept
                                                        (remove cd-concept (concept-term gci)))))
                                 (removed (if (concept-p-internal gci)
                                              (list (concept-negated-concept cd-concept)
                                                    (encode-concept-term right))
                                            gci)))
                            (push removed removed-gcis)
                            #+:debug (assert (listp (role-domain-concept role)))
                            (push right (role-domain-concept role))
                            (when record-gci-absorption-dependencies
                              (push (make-gci-dependency cd-range :removed removed :added right)
                                    (role-gci-dependencies role)))
                            (race-trace ("~&Absorbing implicit domain restriction (implies ~S ~S) into attribute ~S~%"
                                         cd-concept right role))
                            (pushnew role changed-roles)
                            (setf changed t))
                        (push gci new-gcis)))))))
            finally
            (setf new-gcis (nreverse new-gcis))
            (when-race-trace t
              (if (> (length new-gcis) 10000)
                  (race-trace ("~&~D meta constraint concepts generated, ~D domain/range ~
                              GCIs absorbed~%"
                               (length new-gcis)
                               (length removed-gcis)))
                (race-trace ("~&~D meta constraint concepts generated, ~D domain/range ~
                              GCIs absorbed, remaining GCIs=~S~%"
                             (length new-gcis)
                             (length removed-gcis)
                             (copy-list new-gcis)))))
            (loop with set-flag = (incf *role-set-mark*)
                  for role being the hash-value of (tbox-role-store tbox)
                  for domain = (role-domain-concept role)
                  for range = (role-range-concept role)
                  unless (eql (racer-set-flag role) set-flag)
                  do
                  ;; We must not apply this operation twice!!!
                  ;; This might happen for role synonyms.
                  (setf (racer-set-flag role) set-flag)
                  (when domain
                    (if (rest domain)
                        (setf (role-domain-concept role) (list* 'and domain))
                      (setf (role-domain-concept role) (first domain))))
                  (when range
                    (if (rest range)
                        (setf (role-range-concept role) (list* 'and range))
                      (setf (role-range-concept role) (first range)))))
            (when changed
              (when feature-added
                (setf (tbox-language tbox) (add-dl-features (tbox-language tbox))))
              (when reflexivity-added 
                (setf (tbox-language tbox) (add-dl-reflexive (tbox-language tbox))))
              (when irreflexivity-added 
                (setf (tbox-language tbox) (add-dl-irreflexive (tbox-language tbox))))
              (unless pre-absorption
                (loop for role being the hash-value of (tbox-role-store tbox) do
                      (when (or (member role changed-roles)
                                (not (lists-disjoint-p (role-ancestors-internal role)
                                                       changed-roles)))
                        (let ((gci-dependencies (role-gci-dependencies role)))
                          (encode-role tbox role t)
                          (when gci-dependencies
                            (setf (role-gci-dependencies role) gci-dependencies)))))
                (encode-domain-range-of-roles tbox))
              (setf (tbox-removed-generalized-concept-inclusions tbox) 
                    (append removed-gcis
                            (tbox-removed-generalized-concept-inclusions tbox))))
            (return (values new-gcis changed reflexivity-added irreflexivity-added))))))

(defun flattened-and-concept-list (and-concept)
  (loop for concept in (concept-term and-concept)
        if (and-concept-p concept)
        append (flattened-and-concept-list concept)
        else collect concept))

(defun find-domain-restriction (disjuncts pre-absorption)
  (let ((matches (loop for disjunct in disjuncts
                       if (and pre-absorption (negated-atomic-primitive-p disjunct))
                       do (return nil)
                       else
                       when (or (all-concept-p disjunct) (at-most-concept-p disjunct))
                       collect disjunct)))
    (when matches
      (or (loop with bottom = *bottom-concept*
                for match in matches
                when (eq (concept-term match) bottom)
                do (return match))
          (first matches)))))

#+:debug
(defmacro with-gci-trace ((what transformed-gcis) &body body)
  `(let ((old-transformed-gcis ,transformed-gcis))
     ,@body
     (if (> (length ,transformed-gcis) 10000)
       (race-trace ("~&~A: ~D GCIs absorbed, ~D remaining~%"
                    ,what
                    (- (length old-transformed-gcis) (length ,transformed-gcis))
                    (length ,transformed-gcis)
                    ;;,transformed-gcis Removed (RM Apr. 2014)
                    ))
       (race-trace ("~&~A: ~D GCIs absorbed, ~D remaining: ~S~%"
                    ,what
                    (- (length old-transformed-gcis) (length ,transformed-gcis))
                    (length ,transformed-gcis)
                    ,transformed-gcis)))))

#-:debug
(defmacro with-gci-trace ((what transformed-gcis) &body body)
  (declare (ignore what transformed-gcis))
  `(progn
     ,@body))

(defun pre-check-all-gcis (tbox-el+-p gcis)
  (multiple-value-bind (remaining-gcis used-concepts)
      (loop with remaining-gcis = nil
            with used-concepts = nil
            for gci in gcis do
            (multiple-value-bind (absorbed-p nary-absorbed-p concept lhs)
                (nary-gci-absorbed-p tbox-el+-p gci nil nil :check-only t)
              (if absorbed-p
                  (if nary-absorbed-p
                      (setf used-concepts
                            (concept-set-union used-concepts lhs))
                    (push concept used-concepts))
                (push gci remaining-gcis)))
            finally (return (values remaining-gcis used-concepts)))
    (or (null remaining-gcis)
        (let ((used-concepts-table (racer-make-hash-table :size (length used-concepts))))
          (loop for concept in used-concepts do
                #+:debug (assert (atomic-concept-p concept))
                (setf (gethash (first (concept-name-set concept)) used-concepts-table) t))
          (loop for gci in remaining-gcis
                always (absorb-gci-as-negated-definition gci t used-concepts-table))))))

(defun process-all-gcis (tbox gci-pairs cyclic-concepts tbox-el+-p)
  (with-flatten-encodings
    (let ((transformed-gcis gci-pairs)
          (nary-unfolding-table nil)
          (gcis-language *dl-empty*)
          (concept-frequency-table (racer-make-hash-table))
          (max-frequency 0)
          (max-name nil)
          (single-concept nil)
          (reflexivity-added nil)
          (irreflexivity-added nil))
      (when (and *transform-gcis* (not tbox-el+-p))
        (with-gci-trace ("Defined concepts" transformed-gcis)
          (setf transformed-gcis
                (race-time (transform-gcis-to-defined-concepts transformed-gcis
                                                               cyclic-concepts)))))
      (unless tbox-el+-p
        (setf transformed-gcis (race-time (encode-gcis transformed-gcis)))
        ;(setf transformed-gcis (unabsorb-selected-definitions transformed-gcis))
        (loop for gci in transformed-gcis do
              (setf gcis-language (union-dl-descriptors (concept-language gci) gcis-language))
              (loop with concept-list = (if (or-concept-p gci) (concept-term gci) (list gci))
                    for concept in concept-list do
                    (when (negated-concept-p concept)
                      (incf (gethash (first (concept-name-set (concept-term concept)))
                                     concept-frequency-table 0)))))
        (loop for name being the hash-key of concept-frequency-table using (hash-value frequency) do
              (when (> frequency max-frequency)
                (setf max-frequency frequency)
                (setf max-name name)))
        (when (and max-name (eql max-frequency (length transformed-gcis)))
          (setf single-concept (get-tbox-concept tbox max-name)))
        (when *absorb-domains-ranges*
          (with-gci-trace ("Domain/range" transformed-gcis)
            (multiple-value-bind (new-transformed-gcis changed-p new-reflexivity-added new-irreflexivity-added)
                (race-time (absorb-domain-range-restrictions tbox
                                                               transformed-gcis
                                                               :pre-absorption t
                                                               :simple-domain-restrictions t
                                                               :qualified-domain-restrictions nil
                                                               :tbox-el+-p tbox-el+-p
                                                               :add-qualified-role-domain t))
              (setf transformed-gcis new-transformed-gcis)
              (when changed-p
                (setf reflexivity-added (role-set-union new-reflexivity-added reflexivity-added))
                (setf irreflexivity-added (role-set-union new-irreflexivity-added irreflexivity-added)))))))
                  
      (if (and (or (not tbox-el+-p) (not *use-elh-transformation*))
	       (pre-check-all-gcis tbox-el+-p transformed-gcis))
          (progn
            (race-trace ("~&GCIs can be absorbed by standard+negation~%"))
            (with-gci-trace ("Standard Absorption" transformed-gcis)
              (multiple-value-setq (transformed-gcis nary-unfolding-table)
                  (race-time (absorb-all-gcis tbox-el+-p transformed-gcis nil nil nary-unfolding-table
                                              :single-concept single-concept))))
            (when transformed-gcis
              (with-gci-trace ("Negated defs" transformed-gcis)
                (setf transformed-gcis
                      (race-time (absorb-gcis-as-negated-definitions tbox transformed-gcis))))))
        (let ((el+-condition (and tbox-el+-p *use-elh-transformation*)))
          (if el+-condition
              (let ((no-of-defined-concepts
                     (loop for concept in (tbox-encoded-concept-list tbox)
                           count (not (concept-primitive-p concept)))))
                (race-trace ("~&GCIs can be absorbed with EL+ transformation~%"))
                (loop with table = (racer-make-hash-table :size no-of-defined-concepts)
                      for concept in (tbox-encoded-concept-list tbox) do
                      (unless (concept-primitive-p concept)
                        (setf (gethash concept table) t))
                      finally (setf (tbox-el+-transformed-table tbox) table))
                (setf transformed-gcis
                      (race-time (convert-defined-tbox-concepts-to-primitives tbox transformed-gcis t)))
                (setf transformed-gcis
                      (race-time (convert-primitive-concepts-to-atomics tbox transformed-gcis t)))
                (setf nary-unfolding-table (race-time (tbox-el+-normalization tbox transformed-gcis)))
                (setf transformed-gcis nil))
            (race-trace ("~&GCIs cannot be absorbed by standard+negation~%")))
          (multiple-value-setq (transformed-gcis nary-unfolding-table)
              (loop-twice-transformations tbox
                                          transformed-gcis
                                          tbox-el+-p
                                          el+-condition
                                          nary-unfolding-table))
          (when (and transformed-gcis *absorb-domains-ranges*)
            (with-gci-trace ("Domain/range" transformed-gcis)
              (multiple-value-bind (new-transformed-gcis changed-p new-reflexivity-added new-irreflexivity-added)
                  (race-time (absorb-domain-range-restrictions tbox
                                                               transformed-gcis
                                                               :tbox-el+-p tbox-el+-p
                                                               :enforce-range-absorption t))
                (setf transformed-gcis new-transformed-gcis)
                (when changed-p
                  (setf reflexivity-added (role-set-union new-reflexivity-added reflexivity-added))
                  (setf irreflexivity-added (role-set-union new-irreflexivity-added irreflexivity-added)))))
            (when (and transformed-gcis (not el+-condition))
              (with-gci-trace ("Distributive law" transformed-gcis)
                (let ((new-gcis (split-axioms-using-distributive-law transformed-gcis)))
                  (unless (eq transformed-gcis new-gcis)
                    (multiple-value-setq (transformed-gcis nary-unfolding-table)
                        (loop-twice-transformations tbox
                                                    transformed-gcis
                                                    tbox-el+-p
                                                    el+-condition
                                                    nary-unfolding-table)))))
              (when transformed-gcis
                (let ((new-gcis (split-axioms-using-distributive-law transformed-gcis)))
                  (unless (eq transformed-gcis new-gcis)
                    (multiple-value-setq (transformed-gcis nary-unfolding-table)
                        (absorb-all-gcis tbox-el+-p 
                                         new-gcis
                                         t
                                         nil
                                         nary-unfolding-table)))))
              (when transformed-gcis
                (with-gci-trace ("Domain/range" transformed-gcis)
                  (setf transformed-gcis
                        (race-time (absorb-domain-range-restrictions tbox
                                                                     transformed-gcis
                                                                     :tbox-el+-p tbox-el+-p
                                                                     :enforce-range-absorption t))))))
            (when (and transformed-gcis
                       (loop for gci in transformed-gcis
                             thereis
                             (and (or-concept-p gci)
                                  (loop for concept in (concept-term gci)
                                        thereis
                                        (and (all-concept-p concept)
                                             (some #'role-compositions
                                                   (role-descendants-internal (concept-role concept))))))))
              ;; example: (implies-role (r s) t) (implies (some t d) c)
              ;; since role t is defined by a role chain one cannot absorb a domain restriction for t
              ;; but one can transform (implies (some t d) c) into (implies d (all (inv t) c)) and
              ;; possibly absorb the new axiom into concept d
              (with-gci-trace ("Inverse role absorption" transformed-gcis)
                (multiple-value-bind (transformed-p new-gcis-1)
                    (transform-gcis-with-inverse-roles transformed-gcis)
                  (when transformed-p
                    (multiple-value-bind (new-gcis-2 nary-unfolding-table-new)
                        (loop-twice-transformations tbox
                                                    new-gcis-1
                                                    tbox-el+-p
                                                    el+-condition
                                                    nary-unfolding-table)
                      (when (or (null new-gcis-2) (< (length new-gcis-2) (length new-gcis-1)))
                        (setf transformed-gcis new-gcis-2)
                        (setf nary-unfolding-table nary-unfolding-table-new)
                        (when transformed-gcis
                          (with-gci-trace ("Domain/range" transformed-gcis)
                            (setf transformed-gcis
                                  (race-time (absorb-domain-range-restrictions tbox
                                                                               transformed-gcis
                                                                               :tbox-el+-p tbox-el+-p
                                                                               :enforce-range-absorption t))))))))))))))
      (encode-domain-range-of-roles tbox)
      (when (and *transform-gcis* transformed-gcis)
        (with-gci-trace ("Regroup" transformed-gcis)
          (setf transformed-gcis (race-time (regroup-gcis transformed-gcis)))))
      (values transformed-gcis nary-unfolding-table reflexivity-added irreflexivity-added))))

(defun unabsorb-selected-definitions (gcis)
  (let ((unabsorb-atomic-concepts
         (loop for gci in gcis
               if (or (or-concept-p gci) (and-concept-p gci))
               nconc
               (loop for elem in (concept-term gci)
                     when (and (atomic-concept-p elem) (concept-definition elem))
                     collect elem)
               else
               when (and (atomic-concept-p gci) (concept-definition gci))
               collect gci)))
    (if unabsorb-atomic-concepts
        (loop with added-gcis = nil
              for concept in unabsorb-atomic-concepts
              for definition = (concept-definition concept)
              do
              (when definition
                (assert (concept-definition concept))
                (unless (concept-primitive-p concept)
                  (push (encode-concept-term `(or (not ,definition) ,concept)) added-gcis))
                (push (encode-concept-term `(or (not ,concept) ,definition)) added-gcis)
                (setf (concept-definition concept) nil)
                (setf (concept-encoded-definition concept) nil)
                (setf (concept-primitive-p concept) t))
              finally
              (return (nconc added-gcis gcis)))
      gcis)))

(defun loop-twice-transformations (tbox transformed-gcis tbox-el+-p el+-condition nary-unfolding-table)
  (loop with repeat = nil
        with second-loop = nil
        do
        (when repeat
          (setf repeat nil)
          (setf second-loop t))
        (when (or transformed-gcis el+-condition)
          (with-gci-trace ("Standard Absorption without nary" transformed-gcis)
            (multiple-value-setq (transformed-gcis nary-unfolding-table)
                (race-time (absorb-all-gcis tbox-el+-p 
                                            transformed-gcis
                                            (not *transform-gcis*)
                                            t
                                            nary-unfolding-table
                                            :use-nary-absorption nil)))))
        (when (and transformed-gcis (not el+-condition))
          (with-gci-trace ("Negated defs" transformed-gcis)
            (setf transformed-gcis
                  (race-time (absorb-gcis-as-negated-definitions tbox transformed-gcis)))))
        (when (or transformed-gcis el+-condition)
          (with-gci-trace ("Standard Absorption" transformed-gcis)
            (multiple-value-setq (transformed-gcis nary-unfolding-table)
                (race-time (absorb-all-gcis tbox-el+-p 
                                            transformed-gcis
                                            (not *transform-gcis*)
                                            t
                                            nary-unfolding-table)))))
        (when transformed-gcis
          (when (and *transform-gcis* transformed-gcis (not el+-condition))
            (with-gci-trace ("Transform" transformed-gcis)
              (multiple-value-setq (transformed-gcis nary-unfolding-table)
                  (race-time (transform-gcis tbox-el+-p 
                                             transformed-gcis
                                             nil
                                             nil
                                             nary-unfolding-table)))))
          (when (and transformed-gcis (not el+-condition))
            (with-gci-trace ("Standard Absorption + unfolding" transformed-gcis)
              (multiple-value-setq (transformed-gcis nary-unfolding-table)
                  (race-time (absorb-all-gcis tbox-el+-p transformed-gcis t nil nary-unfolding-table)))))
          (when (and *transform-gcis* transformed-gcis (not el+-condition))
            (with-gci-trace ("Transform + unfolding" transformed-gcis)
              (multiple-value-setq (transformed-gcis nary-unfolding-table)
                  (race-time (transform-gcis tbox-el+-p 
                                             transformed-gcis
                                             t
                                             nil
                                             nary-unfolding-table)))))
          (when (and transformed-gcis
                     (not el+-condition)
                     (not second-loop)
                     *use-inverse-role-absorption*)
            (with-gci-trace ("Inverse role absorption" transformed-gcis)
              (multiple-value-bind (transformed-p new-gcis)
                  (transform-gcis-with-inverse-roles transformed-gcis)
                (when transformed-p
                  (setf transformed-gcis new-gcis)
                  (setf repeat t)))))
          (when (and transformed-gcis (not el+-condition))
            (with-gci-trace ("Negated defs" transformed-gcis)
              (setf transformed-gcis
                    (race-time (absorb-gcis-as-negated-definitions tbox transformed-gcis))))))
        until (not repeat))
  (values transformed-gcis nary-unfolding-table))

(defun unabsorb-role-restrictions-into-axioms (tbox gci-pairs)
  (loop with all-gci-pairs = gci-pairs
        with bottom-role = (tbox-object-bottom-role tbox)
        for role in (tbox-encoded-role-list tbox)
        unless (eq role bottom-role)
        do
        (if (role-reflexive-p role)
            (let ((domain (role-domain-restriction role))
                  (range (role-range-restriction role))
                  (inv-role (role-inverse-internal role)))
              (when domain
                (setf (role-domain-concept role) nil)
                (setf (role-domain-restriction role) nil)
                (setf (role-range-concept inv-role) nil)
                (setf (role-range-restriction inv-role) nil)
                (push (list *top-concept* (encode-concept-term domain)) all-gci-pairs))
              (when range
                (setf (role-range-concept role) nil)
                (setf (role-range-restriction role) nil)
                (setf (role-domain-concept inv-role) nil)
                (setf (role-domain-restriction inv-role) nil)
                (push (list *top-concept* (encode-concept-term range)) all-gci-pairs)))
          (let ((domain (unless (or (role-datatype role)
                                    (role-cd-attribute role)
                                    (role-internal-name-p role)
                                    (role-internal-conjunction-p role))
                          (role-domain-restriction role))))
            (when domain
              (let ((inv-role (role-inverse-internal role)))
                (setf (role-domain-concept role) nil)
                (setf (role-domain-restriction role) nil)
                (setf (role-range-concept inv-role) nil)
                (setf (role-range-restriction inv-role) nil)
                (if (or (role-datatype role) (role-cd-attribute role))
                    (push (list (encode-concept-term `(d-at-least 1 ,role)) domain) all-gci-pairs)
                  (push (list (encode-concept-term `(at-least 1 ,role)) domain) all-gci-pairs))))))
        finally (return all-gci-pairs)))

(defun absorb-gcis (tbox gci-pairs cyclic-concepts)
  (with-race-trace-sublevel ("absorb-gcis"
                             :arguments (list tbox gci-pairs cyclic-concepts)
                             :expanded nil
                             :trace-result t)
    (when-race-trace t
      (if (> (length gci-pairs) 10000)
          (race-trace ("~&Starting GCI absorption: #GCIs=~D, cyclic concepts=~S.~%"
                       (length gci-pairs) cyclic-concepts #|gci-pairs|#))
        (race-trace ("~&Starting GCI absorption: #GCIs=~D, cyclic concepts=~S, GCIs=~S.~%"
                     (length gci-pairs) cyclic-concepts gci-pairs))))
    (let ((gci-pairs (unabsorb-role-restrictions-into-axioms tbox gci-pairs)))
      (multiple-value-bind (tbox-el+-p #|tbox-l-p|#)
          (unless (or *random-concept-list* (not *transform-gcis*) (not *use-nary-absorption*)
                      (not *use-elh-model-embedding*) (not *absorb-domains-ranges*))
            (tbox-el+-p tbox gci-pairs))
        (when (or gci-pairs
                  (and tbox-el+-p #|(not tbox-l-p)|# #|*use-elh-model-embedding*|# *use-elh-transformation*))
          (multiple-value-bind (meta-constraint-concepts nary-absorption-table reflexivity-added irreflexivity-added)
              (if (or *absorb-gcis* tbox-el+-p)
                  (race-time (process-all-gcis tbox gci-pairs cyclic-concepts tbox-el+-p))
                (when gci-pairs
                  (race-time (encode-gcis gci-pairs))))
            (race-trace ("~&GCI-absorption:~D meta constraint concepts generated, ~D GCIs absorbed.~%"
                         (length meta-constraint-concepts)
                         (- (length gci-pairs) (length meta-constraint-concepts))))
            (when-race-trace t
              (if (> (length meta-constraint-concepts) 10000)
                  (race-trace ("~&Meta constraint concepts: ~S~%" (length meta-constraint-concepts)))
                (race-trace ("~&Meta constraint concepts: ~S~%" meta-constraint-concepts))))
            (setf meta-constraint-concepts (concept-set-remove-duplicates meta-constraint-concepts))
            (when (and *tbox-verbose* meta-constraint-concepts)
              (let ((no-of-meta-constraint-concepts (length meta-constraint-concepts)))
                (when (or (> no-of-meta-constraint-concepts 1)
                          (not (eq (first meta-constraint-concepts) *bottom-concept*)))
                  (racer-warn "Potential performance problem: ~D global axioms could not be absorbed"
                              no-of-meta-constraint-concepts)
                  (when *debug*
                    (racer-warn "Remaining global axioms: " meta-constraint-concepts)))))
            (let ((domain-qualifications-table
                   (when (and tbox-el+-p *use-elh-transformation* (null meta-constraint-concepts))
                      ; +dl-bottom+ can be ignored for the time being because it was introduced by encoding
                     (setf (tbox-language tbox) (remove-dl-bottom (tbox-language tbox)))
                     (loop with table = nil
                           for concept being the hash-value of (tbox-concept-store tbox)
                           for el+-role-domain-qualifications = 
                           (when (atomic-concept-p concept)
                             (concept-elh-role-domain-qualifications concept))
                           do
                           (when el+-role-domain-qualifications
                             (unless table
                               (setf table (racer-make-hash-table :structure-p t)))
                             (loop for el+-role-domain-qualification in el+-role-domain-qualifications
                                   do
                                   (unless (gethash (second el+-role-domain-qualification) table)
                                     (setf (gethash (second el+-role-domain-qualification) table)
                                           t))))
                           finally (return table)))))
              (values meta-constraint-concepts
                      nary-absorption-table
                      domain-qualifications-table
                      tbox-el+-p
                      reflexivity-added
                      irreflexivity-added))))))))

(defun contains-not-p (definition)
  (let ((language (concept-language definition)))
    (and (dl-atomic-not language)
         (or (negated-concept-p definition)
             (some-concept-p definition)
             (and (and-concept-p definition)
                  (some #'negated-concept-p (concept-term definition)))))))

(defun tbox-el+-p (tbox gci-pairs)
  (flet ((reject (culprit &optional explanation)
           (return-from tbox-el+-p (values nil culprit explanation))))
    (let ((l-p t)
          (enhanced-el+-p (and *use-elh-transformation* *use-nary-absorption*)))
      (loop for concept in (tbox-encoded-concept-list tbox)
            for definition = (concept-encoded-definition concept)
            for negated-definition = (concept-encoded-negated-definition concept)
            for language = (concept-language concept)
            do
            (unless (subset-el++-p language)
              (reject concept 'subset-el++-p-1))
            (when l-p
              (setf l-p (subset-l-p language)))
            (when definition
              (setf language (concept-language definition))
              (when (or (not (subset-el++-p language))
                        (and (not (concept-primitive-p concept))
                             (or (is-top-concept-p definition)
                                 (contains-not-p definition))))
                (reject concept 'subset-el++-p-2))
              (when l-p
                (setf l-p (subset-l-p language))))
            (when negated-definition
              (setf language (concept-language negated-definition))
              (unless (subset-el++-p language)
                (reject concept 'negated-definition))
              (when l-p
                (setf l-p (subset-l-p language)))))
      (loop for role in (tbox-encoded-role-list tbox) do
            (unless (or (role-annotation-p role)
                        (role-datatype role)
                        (role-cd-attribute role)
                        (is-predefined-role-p role)
                        (role-internal-name-p role))
              (when (role-irreflexive-p role)
                (reject role 'role-irreflexive))
              (let ((compositions (role-compositions role)))
                (when compositions
                  (let ((range (role-range-restriction role)))
                    (loop for composition in compositions do
                          (when (and (> (length composition) 2)
                                     (or (eq role (first composition))
                                         (eq role (first (last composition)))))
                                     ; EL+ transformation would create cycles that are not recognized yet
                            (reject role 'cyclic))
                          (when (and range 
                                     (not (eq (role-range-restriction (first (last composition)))
                                              range)))
                            (reject role 'role-range))
                          (when (loop for role in composition
                                      thereis
                                      (or (role-symmetric-p role)
                                          (role-asymmetric-p role)
                                          (role-reflexive-p role)
                                          (role-irreflexive-p role)))
                            (reject role 'role-property))))))
              (let ((domain (role-domain-restriction role)))
                (when domain
                  (let ((language (concept-language domain)))
                    (unless (and (subset-el++-p language) (not (contains-not-p domain)))
                      (reject role 'domain-not-el++))
                    (when l-p
                      (setf l-p (subset-l-p language))))))))
      (if *use-elh-transformation* 
          (loop with top = (tbox-top-node tbox)
                with absorb-domains-ranges = *absorb-domains-ranges*
                for pair in gci-pairs
                for (lhs rhs) = pair
                do
                (unless (eq lhs rhs)
                  (if (eq lhs top)
                      (unless (and enhanced-el+-p
                                   (or (and (all-concept-p rhs)
                                            (concept-self-reference-p rhs)
                                            absorb-domains-ranges
                                            (let ((role (concept-role rhs)))
                                              (and (not (or (is-predefined-role-p role)
                                                            (role-datatype role)
                                                            (role-cd-attribute role)))
                                                   (subset-el++-p (concept-language (concept-term rhs))))))
                                       (and (or-concept-p rhs)
                                            (proper-el+-gci rhs))))
                        (reject pair 'axiom-rhs-not-proper-some))
                    (progn
                      (let ((language (concept-language lhs)))
                        (unless (and (subset-el++-p language) (not (contains-not-p lhs)))
                          (reject pair 'axiom-lhs-not-el++))
                        (when l-p
                          (setf l-p (subset-l-p language))))
                      (let ((language (concept-language rhs)))
                        (unless (and (subset-el++-p language) 
                                     (or (atomic-concept-p lhs) (not (contains-not-p rhs))))
                          (reject pair 'axiom-rhs-not-el++))
                        (when l-p
                          (setf l-p (subset-l-p language))))))))
        (reject (first gci-pairs) 'not-elh-transformation))
      (values t l-p))))


(defun proper-el+-gci (concept)
  (and (or-concept-p concept)
       (loop with all-term-count = 0
             with pos-atom-count = 0
             with neg-atom-count = 0
             for disjunct in (concept-term concept)
             for type = (type-of disjunct)
             do
             (case type
               (all-concept (incf all-term-count))
               (negated-concept (incf neg-atom-count))
               (atomic-concept (incf pos-atom-count))
               ((some-concept and-concept))
               (t (return nil)))
             finally
             (return (not (or (< (+ all-term-count pos-atom-count neg-atom-count) 2)
                              (not (eql (+ all-term-count neg-atom-count) 1))))))))
