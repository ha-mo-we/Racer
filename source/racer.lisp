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

(defun language-is-alci-p (own-inverse-role-elems inverse-role-elems)
  (loop for inv-role-elem-1 in own-inverse-role-elems
        for role-ancestors = (remove-top-object-role (role-ancestors-internal (car inv-role-elem-1)))
        thereis
        (loop for inv-role-elem-2 in inverse-role-elems
              thereis
              (lists-not-disjoint-p role-ancestors
                                    (remove-top-object-role
                                     (role-ancestors-internal
                                      (role-inverse-internal (car inv-role-elem-2))))))))

(defun get-language-from-concept-pair (concept-1 concept-2 &optional (inverse-roles nil))
  (if concept-1
      (progn
        #+:debug 
        (assert (or (not (atomic-concept-p concept-1))
                    (null (concept-encoded-definition concept-1))
                    (is-bottom-concept-p (concept-encoded-definition concept-1))
                    (dl-definition-subset-p (concept-language (concept-encoded-definition concept-1)) 
                                            (concept-language concept-1))))
        (if concept-2
            (progn
              #+:debug 
              (assert (or (not (atomic-concept-p concept-2))
                          (null (concept-encoded-definition concept-2))
                          (is-bottom-concept-p (concept-encoded-definition concept-2))
                          (dl-definition-subset-p (concept-language (concept-encoded-definition concept-2)) 
                                                  (concept-language concept-2))))
              (if (and inverse-roles
                       (language-is-alci-p (union-inverse-role-lists (concept-inverse-roles concept-1)
                                                                     (concept-inverse-roles concept-2))
                                           inverse-roles))
                  (add-dl-inverse (union-dl-descriptors (concept-language concept-1)
                                                        (concept-language concept-2)))
                (union-dl-descriptors (concept-language concept-1)
                                      (concept-language concept-2))))
          (if (and inverse-roles
                   (language-is-alci-p (concept-inverse-roles concept-1) inverse-roles))
              (add-dl-inverse (concept-language concept-1))
            (concept-language concept-1))))
    (if (and inverse-roles
             (language-is-alci-p (concept-inverse-roles concept-2) inverse-roles))
        (add-dl-inverse (concept-language concept-2))
      (concept-language concept-2))))

(defun get-language-from-concepts (concept-list &optional (check-p t))
  #-:debug (declare (ignore check-p))
  (if (hash-table-p concept-list)
      (loop with language = *dl-empty*
            with inverse-roles = nil
            for concept being the hash-key of concept-list
            do           
            #+:debug 
            (assert (or (not check-p)
                        (not (atomic-concept-p concept))
                        (null (concept-encoded-definition concept))
                        (is-bottom-concept-p (concept-encoded-definition concept))
                        (dl-definition-subset-p (concept-language (concept-encoded-definition concept)) 
                                                (concept-language concept))))
            (setf language (union-dl-descriptors language (concept-language concept)))
            (setf inverse-roles (union-inverse-role-lists (concept-inverse-roles concept)
                                                          inverse-roles))
            finally
            (when (and inverse-roles (language-is-alci-p inverse-roles inverse-roles))
              (setf language (add-dl-inverse language)))
            (return (values language inverse-roles)))
    (loop with language = *dl-empty*
          with inverse-roles = nil
          for concept in (if (listp concept-list)
                             concept-list
                           (list concept-list))
          do 
          #+:debug 
          (assert (or (not check-p)
                      (not (atomic-concept-p concept))
                      (null (concept-encoded-definition concept))
                      (is-bottom-concept-p (concept-encoded-definition concept))
                      (dl-definition-subset-p (concept-language (concept-encoded-definition concept)) 
                                              (concept-language concept))))
          (setf language (union-dl-descriptors language (concept-language concept)))
          (setf inverse-roles (union-inverse-role-lists (concept-inverse-roles concept)
                                                        inverse-roles))
          finally
          (when (and inverse-roles (language-is-alci-p inverse-roles inverse-roles))
            (setf language (add-dl-inverse language)))
          (return (values language inverse-roles)))))

(defun get-language-from-constraints (constraint-list &optional (inverse-roles nil))
  (loop with language = *dl-empty*
        with own-inverse-roles = nil
        for constraint in (if (consp constraint-list)
                            constraint-list
                            (list constraint-list))
        for concept = (if (constraint-negated-p constraint)
                        (concept-negated-concept (constraint-term constraint))
                        (constraint-term constraint))
        do
        #+:debug 
        (assert (or (not (atomic-concept-p concept))
                    (null (concept-encoded-definition concept))
                    (is-bottom-concept-p (concept-encoded-definition concept))
                    (dl-definition-subset-p (concept-language (concept-encoded-definition concept)) 
                                            (concept-language concept))))
        (setf language (union-dl-descriptors language (concept-language concept)))
        (when inverse-roles
          (setf own-inverse-roles (union-inverse-role-lists (concept-inverse-roles concept)
                                                            own-inverse-roles)))
        finally
        (when (and inverse-roles
                   own-inverse-roles
                   (language-is-alci-p own-inverse-roles inverse-roles))
          (setf language (add-dl-inverse language)))
        (return language)))

(defun get-language-from-initial-cd-state (cd-state)
  (loop with language = *dl-empty*
        for cd-constraint in (solver-state-cd-constraints cd-state)
        do
        (when (> (predicate-arity (cd-constraint-predicate cd-constraint)) 1)
          (return (add-dl-full-concrete-domains language)))
        finally (return language)))

(defun get-language-from-models (model-list)
  (loop with result = *dl-empty*
        for model in (if (listp model-list)
                         model-list
                       (list model-list))
        for descriptor = (cond ((full-model-info-p model) (model-language model))
                               ((model-info-p model)
                                (loop for exists-model in (model-exists-models model)
                                      for model-language = (concept-language exists-model)
                                      unless (subset-elh-p model-language)
                                      ;; we need to know only that we have a superset of ELH
                                      ;; because ELH ind model merging works 
                                      ;; neither for transitive roles nor complex role compositions
                                      do (return model-language)
                                      finally (return *dl-elh*)))
                               ((concept-p-internal model) (concept-language model))
                               (t (error "unexpected")))
        do (setf result (union-dl-descriptors result descriptor))
        finally (return result)))

(defun print-status ()
  (let ((block-status
         (when *blocking-possibly-required*
           (if *inverse-roles*
             (if (shiq-blocking-required *dl-prover-language*)
               "SHIQ"
               "SHI")
             "SUBS"))))
    #-(and :debug :lispworks)
    (declare (ignore block-status))
    (race-trace ("~&Language settings: ~S, inv=~S, block=~A, deep-mm=~S, t-cach=~S, sorted=~S, una=~S~%"
                 *dl-prover-language* *inverse-roles* block-status 
                 *deep-model-merging* *tableaux-caching* *sorted-concept-list* *use-unique-name-assumption*)))
  ;(print (list *dl-prover-language* *inverse-roles* *deep-model-merging* *tableaux-caching* *sorted-concept-list*))
  ;(when (or (dl-merging *dl-prover-language*) (dl-inverse-roles *dl-prover-language*)) (break))
  ;(unless *sorted-concept-list* (break))
  )

(defmacro with-alc-language-settings ((&key (constraints nil)
                                            (cd-state nil)
                                            (models nil)
                                            (concept-1 nil)
                                            (concept-2 nil)
                                            (precompletion-prover-language nil))
                                      &body body)
  (let ((merging-required-sym (gensym))
        (elh-subset-p-sym (gensym)))
    `(multiple-value-bind (dl-prover-language-1 meta-inverse-roles)
	   (if *meta-constraint-concepts*
	       (get-language-from-concepts *meta-constraint-concepts*)
	     *dl-empty*)
         (declare (ignorable meta-inverse-roles))
         (let* ((dl-prover-language-2 
                 (cond (,concept-1 
			(if (concepts-bucket-p ,concept-1)
                          (union-dl-descriptors 
                           (get-language-from-concept-pair ,concept-2
                                                           nil
                                                           meta-inverse-roles)
                           (bucket-members-language ,concept-1))
			  (get-language-from-concept-pair ,concept-1
							  ,concept-2
							  meta-inverse-roles)))
                       (,constraints (get-language-from-constraints ,constraints meta-inverse-roles))
                       (,models (get-language-from-models ,models))
                       (t *dl-empty*)))
                (dl-prover-language-3
                 (union-dl-descriptors dl-prover-language-1 dl-prover-language-2))
                (dl-prover-language-4
                 (if ,cd-state
                     (union-dl-descriptors (get-language-from-initial-cd-state ,cd-state)
                                           dl-prover-language-3)
                   dl-prover-language-3))
                (*dl-prover-language*
                 ,(if precompletion-prover-language
                   `(union-dl-descriptors ,precompletion-prover-language dl-prover-language-4)
                   'dl-prover-language-4))
                (*inverse-roles* (dl-inverse-roles *dl-prover-language*))
                (*deep-model-merging* (if (or *inverse-roles* (not *model-merging*))
                                        nil
                                        *deep-model-merging*))
                (*sorted-concept-list* (if *inverse-roles*
                                         nil
                                         *sorted-concept-list*))
                (,merging-required-sym (dl-merging *dl-prover-language*))
                (,elh-subset-p-sym (unless (or ,merging-required-sym
                                               (not *use-elh-model-embedding*))
                                     (subset-el+-p *dl-prover-language*)))
                (*tableaux-caching* (if *inverse-roles*
                                      nil
                                      *tableaux-caching*))
                (*subsumed-concept-p* (if ,merging-required-sym
                                        'alcn-subsumed-concept-p
                                        'alc-subsumed-concept-p))
                (*subsumes-concept-p* (if ,merging-required-sym
                                        'alcn-subsumes-concept-p
                                        'alc-subsumes-concept-p))
                (*concept-clash-p* (if ,merging-required-sym
                                     'alcn-concept-clash-p
                                     (if ,elh-subset-p-sym
                                       'elh-concept-clash-p
                                       'alc-concept-clash-p)))
                (*matching-constraint-test* (if ,merging-required-sym
                                              'alcn-matching-constraint-test
                                              'alc-matching-constraint-test))
                (*find-matching-constraint* (if ,merging-required-sym
                                              'alcn-find-matching-constraint
                                              'alc-find-matching-constraint)))
           (print-status)
           ,@body))))


(race-inline (subsumes-concept-p concept-clash-p))

#|
(defun subsumed-concept-p (subsumee subsumer)
  (funcall *subsumed-concept-p* subsumee subsumer))
|#

(defun subsumes-concept-p (subsumer subsumee)
  (funcall *subsumes-concept-p* subsumer subsumee))

(defun concept-clash-p (concept-1 concept-2)
  (funcall *concept-clash-p* concept-1 concept-2))

(defun racer-version (&optional (stream nil))
  (format stream "~A Version ~A" (get-product-name) (get-product-version)))

(defun print-racer-info (&optional (stream t))
  #+:lispworks (terpri stream)
  (format stream "~%;;; Welcome to ~A Version ~A ~A! ~%~
                  ~%~
                  ;;; Racer: Renamed Abox and Concept Expression Reasoner~%~
                  ;;; Supported description logic: SRIQ(D)~%~
                  ;;; ~A comes with ABSOLUTELY NO WARRANTY; use at your own risk. ~2%"
	  (get-product-name)
	  (get-product-version)
	  (get-build-version)
	  (get-product-name))
  (values))

(defun print-racer-settings (&optional (stream t))
  #-:debug (format stream "~2&; ~A~%" (racer-version))
  #+:debug (format stream "~2&; ~A (SVN ~A) with :debug compiled~%" (racer-version) (svn-revision))
  (format stream "; Using ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version)))

;;;===========================================================================
;;; Conversion of constraints represented as lists
;;;===========================================================================
        
(defun new-concept-constraint (ind concept)
  "Create a new concept constraint. Takes care of neagtion and or-constraints."
  (let ((type (type-of concept)))
    (case type
      ((negated-concept all-concept at-most-concept ensure-cd-concept ria-initial-concept ria-final-concept)
       (make-concept-constraint ind (concept-negated-concept concept) t))
      (or-concept
       (let* ((term (concept-term concept))
              (or-length (length term))
              (constraint
               (make-or-constraint ind
                                   (concept-negated-concept concept)
                                   or-length
                                   or-length
                                   (make-sequence 'vector or-length
                                                  :initial-element +unknown+)
                                   t)))
         #+:debug
         (when (not (eql or-length
                         (length (concept-term (concept-negated-concept concept)))))
           (error "confusion"))
         (when (and *model-merging* (or *use-alternate-models* *use-alternate-ind-models*))
           (setf (constraint-derived-from constraint) (list constraint)))
         constraint))
      (cd-concept
       (if (eq (predicate-operator (concept-predicate concept)) 'bottom)
           (make-concept-constraint ind (concept-negated-concept concept) t)
         (make-concept-constraint ind concept)))
      (t (make-concept-constraint ind concept)))))

(race-inline (new-root-concept-constraint 
              new-initial-concept-constraint
              new-relation-constraint
              or-clause-truth-value
              set-or-clause-truth-value
              open-or-clause-p))

(defun new-root-concept-constraint (ind concept)
  (let ((constraint (new-concept-constraint ind concept)))
    (setf (constraint-derived-from constraint) (list constraint))
    constraint))

(defun new-initial-concept-constraint (concept)
  (new-root-concept-constraint +ind-counter-init+ concept))

(defun new-relation-constraint (ind-1 ind-2 role)
  (if (role-transitive-p role)
    (make-relation-constraint ind-1 ind-2 (encode-role-term (role-name role)))
    (make-relation-constraint ind-1 ind-2 role)))
        
(defun or-clause-truth-value (or-constraint clause-index)
  (svref (constraint-or-clauses-truth-value or-constraint) clause-index))

(defun set-or-clause-truth-value (or-constraint clause-index new-value)
  (setf (svref (constraint-or-clauses-truth-value or-constraint) clause-index)
        new-value))

(defun open-or-clause-p (or-constraint clause-index)
  (eq (or-clause-truth-value or-constraint clause-index) +unknown+))
        
(defun negate-constraint (constraint precondition-or-constraint)
  "Create a new negated constraint with unchanged dependencies"
  (let ((concept (constraint-term constraint))
        new-constraint)
    (cond
     ((or-constraint-p constraint)
      (progn
        (setf new-constraint (new-concept-constraint (constraint-ind constraint)
                                                     concept))
        (setf (constraint-derived-from new-constraint)
              (constraint-derived-from constraint))
        (setf (constraint-dependencies new-constraint)
              (constraint-dependencies constraint))
        (setf (constraint-or-dependencies new-constraint)
              (constraint-or-dependencies constraint))))
     ((and-concept-p concept)
      (progn
        (setf new-constraint
              (new-concept-constraint (constraint-ind constraint)
                                      (concept-negated-concept concept)))
        (setf (constraint-derived-from new-constraint)
              (constraint-derived-from constraint))
        (setf (constraint-dependencies new-constraint)
              (constraint-dependencies constraint))
        (setf (constraint-or-dependencies new-constraint)
              (list precondition-or-constraint))))
     ((or (eq concept *bottom-concept*)
          (eq concept *top-concept*)
          (is-bottom-datatype-concept-p concept)
          (is-top-datatype-concept-p concept))
      #+:debug (assert (not (constraint-negated-p constraint)))
      (setf new-constraint (copy-concept-constraint constraint))
      (setf (constraint-term new-constraint) (concept-negated-concept concept)))
     
     (t
      (setf new-constraint (copy-concept-constraint constraint))
      (setf (constraint-negated-p new-constraint)
            (not (constraint-negated-p new-constraint)))))
    new-constraint))

(defun encode-constraint (constraint-term)
  ; ((i1 i2) C) or (i C)
  (let ((ind-or-inds (first constraint-term)))
    (if (consp ind-or-inds)
      (make-relation-constraint (first ind-or-inds)
                                (second ind-or-inds)
                                (encode-role-term (second constraint-term)))
      (new-root-concept-constraint
       ind-or-inds
       (encode-concept-term (second constraint-term))))))

(defun encode-constraints-non-optimized (constraint-list)
  (loop for constraint in constraint-list
        for encoded-constraint = (encode-constraint constraint)
        if (relation-constraint-p encoded-constraint)
        collect encoded-constraint into relation-constraints
        else
        collect encoded-constraint into concept-constraints
        finally (return (values concept-constraints relation-constraints))))

(defun encode-constraints-optimized (constraint-list)
  "Encode and simplify a list of constraints represented as lists of symbols.
Recognizes special case for satisfiability tests. Return two values:
list of new concept constraints, list of new relation constraints."
  (if (null (rest constraint-list))
    (let ((encoded-constraint (encode-constraint (first constraint-list))))
      (if (concept-constraint-p encoded-constraint)
        (values encoded-constraint nil)
        (values nil encoded-constraint)))
    (encode-constraints-1 constraint-list)))

(defun encode-constraints-1 (constraint-list)
  "Encode or merge and re-encode a list of constraints. Return two values:
list of new concept constraints, list of new relation constraints."
  (let ((table (racer-make-hash-table))
        (concept-constraints nil)
        (relation-constraints nil))
    (loop for constraint in constraint-list
          for ind-or-inds = (first constraint)
          for term = (second constraint)
          for list-term = (if (concept-p-internal term) (decode-concept term) term)
          if (consp ind-or-inds)
          do (push (make-relation-constraint (first ind-or-inds)
                                             (second ind-or-inds)
                                             (encode-role-term (second constraint)))
                   relation-constraints)
          else do (push list-term (gethash ind-or-inds table)))
    (with-flatten-encodings
      (loop for ind being the hash-key in table
            using (hash-value term) do
            (push (new-root-concept-constraint ind (encode-concept-term `(and ,@term)))
                  concept-constraints)))
    (values concept-constraints relation-constraints)))

(race-inline (atomic-constraint-p))

(defun atomic-constraint-p (constraint)
  (atomic-concept-p (constraint-term constraint)))

;;;===========================================================================
;;; Auxiliary functions for satisfiability testing and constraint expansion
;;;===========================================================================

(defun minimize-list (list key-fcn)
  "Remove a minimum element from list by using the key function.
Return two values: removed element, modified list."
  (if (null (rest list))
    (first list)
    (progn
      ;(print list)
      ;(print (list (mapcar key-fcn list) (mapcar #'most-recent-dependencies list)))
      ;(break)
      (loop with min-elem = (first list)
            with min-key = (funcall key-fcn min-elem)
            for elem in (rest list)
            for elem-key = (funcall key-fcn elem)
            when (< elem-key min-key) ;;; changing < to <= makes big difference !!!
            do (progn (setf min-elem elem)
                      (setf min-key elem-key))
            finally (return (values min-elem
                                    (remove min-elem list :test #'eq)))))))

(defun minimize-list-if (test-less-fcn list)
  "Remove a minimum element from list by using the test function for comparison.
Return two values: removed element, modified list."
  (if (null list)
    (error "can not minimize empty list")
    (loop with min-elem = (first list)
          for elem in (rest list)
          when (funcall test-less-fcn elem min-elem)
          do (setf min-elem elem)
          finally (return (values min-elem
                                  (remove min-elem list :test #'eq))))))

(defun find-min-element (list key-fcn)
  "Find a minimum element from list by using the key function.
Return the selected element."
  (if (null (rest list))
    (first list)
    (progn
      ;(print (list list (mapcar key-fcn list) (mapcar #'most-recent-dependencies list)))
      (loop with min-elem = (first list)
            with min-key = (funcall key-fcn min-elem)
            for elem in (rest list)
            for elem-key = (funcall key-fcn elem)
            when (< elem-key min-key)  ;;; changing < to <= makes big difference !!!
            do (progn (setf min-elem elem)
                      (setf min-key elem-key))
            finally (return min-elem)))))

(defun find-best-exists (exists-list prefer-backpropagation state)
  #+:debug (assert (every #'exists-constraint-p exists-list))
  (if *inverse-roles*
    (if *depth-first-for-inverse-roles*
      (find-best-exists-2 exists-list prefer-backpropagation state)
      (if *breadth-first-for-inverse-roles*
        (find-best-exists-2 exists-list nil state)
        (find-best-exists-2 exists-list prefer-backpropagation state)))
    (find-best-exists-1 exists-list)))

(defun find-best-exists-1 (exists-list)
  "Find a suitable element from exists-list with preference for features.
Return the selected element."
  (if (null (rest exists-list))
    (first exists-list)
    (loop with max-some-list-threshold = *max-some-list-threshold*
          with min-exists = (first exists-list)
          with min-exists-key = (if (null (constraint-or-dependencies min-exists))
                                    0
                                  (constraint-or-level min-exists))
          with min-exists-is-feature-p = (role-feature-p
                                          (concept-role (constraint-term min-exists)))
          for exists in (rest exists-list)
          for count from 0
          for exists-key = (if (null (constraint-or-dependencies exists))
                               0
                             (constraint-or-level exists))
          for exists-is-feature-p = (role-feature-p
                                     (concept-role (constraint-term exists))) do
          (if min-exists-is-feature-p
            (when (and exists-is-feature-p (< exists-key min-exists-key))
              (setf min-exists exists)
              (setf min-exists-key exists-key))
            (when (or (< exists-key min-exists-key) exists-is-feature-p)
              (setf min-exists exists)
              (setf min-exists-key exists-key)
              (setf min-exists-is-feature-p exists-is-feature-p)))
          until (> count max-some-list-threshold)
          finally (return min-exists))))

(defun find-best-exists-2 (exists-list prefer-backpropagation state)
  "Find a suitable element from exists-list. Return the selected element."
  (if (null (rest exists-list))
    (first exists-list)
    (loop with max-some-list-threshold = *max-some-list-threshold*
          with best-exists = (first exists-list)
          with max-exists-key = (true-exists-key best-exists state)
          with best-backpropagated-p = (and prefer-backpropagation
                                            (constraint-backpropagated-p best-exists))
          for exists in (rest exists-list)
          for count from 0
          for exists-key = (true-exists-key exists state)
          do
          (when (or (and (not best-backpropagated-p) (> exists-key max-exists-key))
                    (and prefer-backpropagation (constraint-backpropagated-p exists)))
            (setf best-exists exists)
            (setf max-exists-key exists-key)
            (when prefer-backpropagation
              (setf best-backpropagated-p (constraint-backpropagated-p exists))))
          until (> count max-some-list-threshold)
          finally (return best-exists))))

(defun true-exists-key (constraint state)
  (if (and (dl-merging *dl-prover-language*)
           (at-most-possibly-violated-p (constraint-ind constraint) (constraint-term constraint) state))
      (* (1+ (constraint-or-level constraint)) 2)
  (constraint-or-level constraint)))

(defun find-best-exists-in-unexpanded-store (prefer-backpropagation state)
  (if *inverse-roles*
    (if *depth-first-for-inverse-roles*
      (find-best-exists-in-unexpanded-store-2 prefer-backpropagation state)
      (if *breadth-first-for-inverse-roles*
        (find-best-exists-in-unexpanded-store-2 nil state)
        (find-best-exists-in-unexpanded-store-2 prefer-backpropagation state)))
    (find-best-exists-in-unexpanded-store-1 state)))

(defun find-best-exists-in-unexpanded-store-1 (state)
  "Find a suitable element from exists-list with preference for features.
Return the selected element."
  (let ((min-exists nil)
        (min-exists-key nil)
        (min-exists-is-feature-p nil)
        (max-some-list-threshold *max-some-list-threshold*)
        (unexpanded-exists-constraints (state-unexpanded-exists-constraints state))
        (count 0))
    (when unexpanded-exists-constraints
      (setf min-exists (first unexpanded-exists-constraints))
      (setf min-exists-key (constraint-or-level min-exists))
      (setf min-exists-is-feature-p (role-feature-p (concept-role (constraint-term min-exists))))
      (loop for exists in (rest unexpanded-exists-constraints)
            for exists-key = (constraint-or-level exists)
            for exists-is-feature-p = (role-feature-p (concept-role (constraint-term exists)))
            do
            (incf count)
            (if min-exists-is-feature-p
              (when (and exists-is-feature-p (< exists-key min-exists-key))
                (setf min-exists exists)
                (setf min-exists-key exists-key))
              (when (or (< exists-key min-exists-key) exists-is-feature-p)
                (setf min-exists exists)
                (setf min-exists-key exists-key)
                (setf min-exists-is-feature-p exists-is-feature-p)))
            until (> count max-some-list-threshold)))
    (if (> count max-some-list-threshold)
      min-exists
      (let* ((collect-p (> *cache-size-from-constraint-store* 0))
             (unexpanded-exists-store (state-unexpanded-exists-constraints-store state))
             (max-count (if collect-p
                          (+ *cache-size-from-constraint-store*
                             (length unexpanded-exists-constraints))
                          max-some-list-threshold))
             (abox (state-abox (state-parameters state))))
        (loop-over-all-constraints (exists visited-exists unexpanded-exists-store abox)
                                   (> count max-count)
                                   (finally
                                    (return-from find-best-exists-in-unexpanded-store-1
                                      (values min-exists visited-exists)))
          when collect-p
          collect exists
          do
          (incf count)
          (when (<= count max-some-list-threshold)
            (let ((exists-key (constraint-or-level exists))
                  (exists-is-feature-p (role-feature-p (concept-role (constraint-term exists)))))
              (if min-exists
                (if min-exists-is-feature-p
                  (when (and exists-is-feature-p (< exists-key min-exists-key))
                    (setf min-exists exists)
                    (setf min-exists-key exists-key))
                  (when (or (< exists-key min-exists-key) exists-is-feature-p)
                    (setf min-exists exists)
                    (setf min-exists-key exists-key)
                    (setf min-exists-is-feature-p exists-is-feature-p)))
                (progn
                  (setf min-exists exists)
                  (setf min-exists-key exists-key)
                  (setf min-exists-is-feature-p exists-is-feature-p))))))))
    min-exists))

(defun find-best-exists-in-unexpanded-store-2 (prefer-backpropagation state)
  "Find a suitable element from exists-list. Return the selected element."
  (let* ((best-exists nil)
         (max-exists-key nil)
         (best-exists-or-level nil)
         (best-backpropagated-p nil)
         (max-some-list-threshold *max-some-list-threshold*)
         (unexpanded-exists-constraints (state-unexpanded-exists-constraints state))
         (unexpanded-exists-store (state-unexpanded-exists-constraints-store state))
         (count 0))
    (when unexpanded-exists-constraints
      (setf best-exists (first unexpanded-exists-constraints))
      (setf best-exists-or-level (constraint-or-level best-exists))
      (setf max-exists-key (true-exists-key best-exists state))
      (when prefer-backpropagation
        (setf best-backpropagated-p (constraint-backpropagated-p best-exists)))
      (loop for exists in (rest unexpanded-exists-constraints)
            for exists-key = (constraint-or-level exists)
            do
            (incf count)
            (when (or (and (not best-backpropagated-p) (> exists-key max-exists-key))
                      (and prefer-backpropagation (constraint-backpropagated-p exists))
                      (and (eql exists-key max-exists-key)
                           (< (constraint-or-level exists) best-exists-or-level)))
              (setf best-exists exists)
              (setf best-exists-or-level (constraint-or-level exists))
              (setf max-exists-key exists-key)
              (when prefer-backpropagation
                (setf best-backpropagated-p (constraint-backpropagated-p exists))))
            until (> count max-some-list-threshold)))
    (if (> count max-some-list-threshold)
      best-exists
      (let* ((collect-p (> *cache-size-from-constraint-store* 0))
             (max-count (if collect-p
                          (+ *cache-size-from-constraint-store*
                             (length unexpanded-exists-constraints))
                          max-some-list-threshold))
             (abox (state-abox (state-parameters state))))
        (loop-over-all-constraints (exists visited-exists unexpanded-exists-store abox)
                                   (> count max-count)
                                   (finally
                                    (return-from find-best-exists-in-unexpanded-store-2
                                      (values best-exists visited-exists)))
          when collect-p
          collect exists
          do
          (incf count)
          (when (<= count max-some-list-threshold)
            (let ((exists-key (true-exists-key exists state)))
              (if best-exists
                (when (or (and (not best-backpropagated-p) (> exists-key max-exists-key))
                          (and prefer-backpropagation (constraint-backpropagated-p exists))
                          (and (eql exists-key max-exists-key)
                               (< (constraint-or-level exists) best-exists-or-level)))
                  (setf best-exists exists)
                  (setf best-exists-or-level (constraint-or-level exists))
                  (setf max-exists-key exists-key)
                  (when prefer-backpropagation
                    (setf best-backpropagated-p (constraint-backpropagated-p exists))))
                (progn
                  (setf best-exists exists)
                  (setf best-exists-or-level (constraint-or-level exists))
                  (setf max-exists-key exists-key)
                  (when prefer-backpropagation
                    (setf best-backpropagated-p (constraint-backpropagated-p exists))))))))))
    best-exists))

(defun get-max-ind (max-some-list-threshold count-init unexpanded-exists-store)
  (let ((max 0))
    (loop-over-all-individuals-of-constraint-store (ind unexpanded-exists-store)
     for count from count-init
      unless (or (symbolp ind) (<= ind max))
      do (setf max ind)
      until (> count max-some-list-threshold)
      finally (return-from get-max-ind max))
    max))

(defun at-most-possibly-violated-p (ind exists-concept state)
  (when (dl-merging (state-dl-prover-language (state-parameters state)))
    (let* ((role-ancestors (role-ancestors-internal (concept-role exists-concept)))
           (at-most (find-if-selected-constraints
                     ind
                     (lambda (constraint)
                       (let ((concept 
                              (if (constraint-negated-p constraint)
                                (concept-negated-concept (constraint-term constraint))
                                (constraint-term constraint))))
                         (and (at-most-concept-p concept)
                              (member (concept-role concept) role-ancestors))))
                     (state-expanded-constraints state)
                     (state-expanded-store state)
                     (state-expanded-store-index state))))
      (when at-most
        (let ((upper-bound (1- (concept-number-restriction (constraint-term at-most))))
              (count-1
               (get-exists-counts-from-concepts
                (concept-role exists-concept)
                (collect-ind-selected-constraints ind
                                                  (state-unexpanded-exists-constraints
                                                   state)
                                                  (state-unexpanded-exists-constraints-store
                                                   state))
                :key (lambda (constraint)
                       (if (constraint-negated-p constraint)
                         (concept-negated-concept
                          (constraint-term constraint))
                         (constraint-term constraint))))))
          ;(print (list ind exists-concept upper-bound))
          ;(break)
          (or (> count-1 upper-bound)
              (let ((count-2
                     (+ count-1 (number-of-role-successors ind
                                                           (concept-role exists-concept)
                                                           (state-relation-constraints state)
                                                           state))))
                (> count-2 upper-bound))))))))

#|
(defun find-min-exists (exists-list ignore-at-least)
  "Find a suitable element from exists-list with preference for features.
Return the selected element."
  (declare (ignore ignore-at-least))
  (if (null (rest exists-list))
    (first exists-list)
    (loop with min-exists = (first exists-list)
          with min-exists-ind = (constraint-ind min-exists)
          with min-exists-key = (if (old-individual-p min-exists-ind)
                                  (+ 1000 (constraint-or-level min-exists))
                                  (+ (* (1+ min-exists-ind) 1000)
                                     (constraint-or-level min-exists)))
          with min-exists-is-feature-p = (role-feature-p
                                          (concept-role (constraint-term min-exists)))
          for exists in (rest exists-list)
          for exists-ind = (constraint-ind exists)
          for exists-key = (if (old-individual-p exists-ind)
                             (+ 1000 (constraint-or-level exists))
                             (+ (* (1+ exists-ind) 1000)
                                (constraint-or-level exists)))
          for exists-is-feature-p = (role-feature-p
                                     (concept-role (constraint-term exists))) do
          (if min-exists-is-feature-p
            (when (and exists-is-feature-p (< exists-key min-exists-key))
              (setf min-exists exists)
              (setf min-exists-key exists-key))
            (when (or (< exists-key min-exists-key) exists-is-feature-p)
              (setf min-exists exists)
              (setf min-exists-key exists-key)
              (setf min-exists-is-feature-p exists-is-feature-p)))
          finally (return min-exists))))
|#

#|
(defun find-min-or-element-1 (list)
  (if (null (rest list))
    (first list)
    (loop with min-elem = (first list)
          with min-key = (most-recent-dependencies min-elem)
          with max-or-list-threshold = *max-or-list-threshold*
          for elem in (rest list)
          for elem-key = (most-recent-dependencies elem)
          for count from 0
          when (< elem-key min-key)  ;;; changing < to <= makes big difference !!!
          do (progn (setf min-elem elem)
                    (setf min-key elem-key))
          until (> count max-or-list-threshold)
          finally (return min-elem))))
|#

(defun find-min-or-element-1 (list)
  (if (null (rest list))
      (first list)
    (loop with abox = *use-abox*
          with min-elem = (first list)
          with min-ind-name = (constraint-ind min-elem)
          with min-prover-p = (prover-individual-p min-ind-name)
          with min-ind = (when (and abox (not min-prover-p))
                           (find-individual abox min-ind-name))
          with min-key = (most-recent-dependencies min-elem)
          with max-or-list-threshold = *max-or-list-threshold*
          with inverse-roles = *inverse-roles*
          for elem in (rest list)
          for elem-ind-name = (constraint-ind elem)
          for elem-prover-p = (prover-individual-p elem-ind-name)
          for elem-key = (most-recent-dependencies elem)
          for count from 0
          for lessp = (cond ((or (not abox) (and min-prover-p elem-prover-p))
                             (< elem-key min-key)) ;;; changing < to <= makes big difference !!!
                            ((and abox (not (or min-prover-p elem-prover-p)))
                             (<= (individual-has-fillers (find-individual abox elem-ind-name))
                                 (length (individual-incoming-role-assertions min-ind))))
                            (inverse-roles (< elem-key min-key))
                            (elem-prover-p nil))
          when lessp
          do
          (setf min-elem elem)
          (setf min-prover-p elem-prover-p)
          (setf min-ind-name elem-ind-name)
          (when (and abox (not min-prover-p))
            (setf min-ind (find-individual abox min-ind-name)))
          (setf min-key elem-key)
          until (> count max-or-list-threshold)
          finally (return min-elem))))

(defun find-min-or-element-2 (list)
  (first (last list)))

(defun find-min-or-element (list)
  (if *use-abox* ; better option for Aboxes
    (find-min-or-element-1 (reverse list))
    (find-min-or-element-2 list)))

#|
(defun find-min-or-element-1 (list)
  (first list))

(defparameter *foo* nil)
(defun find-min-or-element-1 (list)
  (let ((selected (nth (random (length list)) list)))
    (push selected *foo*)
    selected))

(defun find-min-or-element-1 (list)
  (let ((selected (first (sort (copy-list list)
                               (lambda (i1 i2)
                                 (cond ((and (symbolp i1) (symbolp i2))
                                        (string-greaterp i1 i2)
                                        )
                                       ((symbolp i1)
                                        t)
                                       ((symbolp i2)
                                        nil)))
                               :key #'constraint-ind))))
    ;(push selected *foo*)
    selected))
|#

(defun find-min-or-element-in-unexpanded-disjunctive-store (state
                                                                      unexpanded-disjunctive-constraints
                                                                      unexpanded-disjunctive-store)
  (let ((min-elem nil)
        (min-key nil)
        (max-or-list-threshold *max-or-list-threshold*)
        (count 0))
    (when unexpanded-disjunctive-constraints
      (if (null (rest unexpanded-disjunctive-constraints))
        (return-from find-min-or-element-in-unexpanded-disjunctive-store
          (first unexpanded-disjunctive-constraints))
        (progn
          (setf min-elem (first unexpanded-disjunctive-constraints))
          (setf min-key (most-recent-dependencies min-elem))
          (loop for elem in (rest unexpanded-disjunctive-constraints)
                for elem-key = (most-recent-dependencies elem)
                do (incf count)
                when (< elem-key min-key)  ;;; changing < to <= makes big difference !!!
                do
                (setf min-elem elem)
                (setf min-key elem-key)
                until (> count max-or-list-threshold)))))
    (if (> count max-or-list-threshold)
      min-elem
      (let* ((collect-p (> *cache-size-from-constraint-store* 0))
             (max-count (if collect-p
                          (+ *cache-size-from-constraint-store*
                             (length unexpanded-disjunctive-constraints))
                          max-or-list-threshold))
             (abox (state-abox (state-parameters state))))
        (loop-over-all-constraints (elem visited-elems unexpanded-disjunctive-store abox)
                                   (> count max-count)
                                   (finally
                                    (return-from find-min-or-element-in-unexpanded-disjunctive-store
                                      (values min-elem visited-elems)))
          when collect-p
          collect elem
          do
          (incf count)
          (when (<= count max-or-list-threshold)
            (let ((elem-key (most-recent-dependencies elem)))
              (when (or (null min-elem) (< elem-key min-key))  ;;; changing < to <= makes big difference !!!
                (setf min-elem elem)
                (setf min-key elem-key)))))))
    min-elem))

(defun select-constraint (state)
  (let ((unexpanded-deterministic-constraints (state-unexpanded-deterministic-constraints state))
        (unexpanded-disjunctive-constraints (state-unexpanded-disjunctive-constraints state))
        (unexpanded-exists-constraints (state-unexpanded-exists-constraints state))
        (unprocessed-constraints (state-unprocessed-constraints state))
        (indirectly-blocked-individuals (state-indirectly-blocked-individuals state))
        (relation-constraints (state-relation-constraints state))
        (labels (state-labels state))
        (unexpanded-disjunctive-constraints-store
         (state-unexpanded-disjunctive-constraints-store state))
        (unexpanded-exists-constraints-store
         (state-unexpanded-exists-constraints-store state)))
    (cond
     (unexpanded-deterministic-constraints
      (race-trace ("~&Selecting new deterministic constraint ~S from ~S, state=~S~%"
                   (first unexpanded-deterministic-constraints)
                   unexpanded-deterministic-constraints
                   (copy-basic-kernel-state-internal state)))
      (values (first unexpanded-deterministic-constraints)
              (let ((new-unexpanded-deterministic-constraints
                     (rest unexpanded-deterministic-constraints)))
                (changed-kernel-state state
                                      :unexpanded-deterministic-constraints
                                      new-unexpanded-deterministic-constraints))))
     ((state-unexpanded-defined-det-constraints state)
      (values nil state))
     ((or unexpanded-disjunctive-constraints
          (and *use-unexpanded-disjunctive-constraints-store*
               unexpanded-disjunctive-constraints-store
               (not (constraint-store-unused-p unexpanded-disjunctive-constraints-store))
               (not (constraint-store-empty-p unexpanded-disjunctive-constraints-store))))
      (flet ((indirectly-blocked-p (constraint)
               (member (constraint-ind constraint)
                       indirectly-blocked-individuals)))
        (multiple-value-bind
          (constraint visited-constraints)
          (if (and *use-unexpanded-disjunctive-constraints-store*
                   unexpanded-disjunctive-constraints-store
                   (not (constraint-store-unused-p unexpanded-disjunctive-constraints-store)))
            (or (when indirectly-blocked-individuals
                  (find-indset-selected-constraint indirectly-blocked-individuals
                                                   unexpanded-disjunctive-constraints
                                                   unexpanded-disjunctive-constraints-store))
                (find-min-or-element-in-unexpanded-disjunctive-store
                 state
                 unexpanded-disjunctive-constraints
                 unexpanded-disjunctive-constraints-store))
            (or (when indirectly-blocked-individuals
                  (find-if #'indirectly-blocked-p unexpanded-disjunctive-constraints))
                (find-min-or-element unexpanded-disjunctive-constraints)))
          (race-trace ("~&Selecting new disjunctive constraint ~S from ~S / ~S, state=~S~%"
                       constraint
                       unexpanded-disjunctive-constraints
                       unexpanded-disjunctive-constraints-store
                       (copy-basic-kernel-state-internal state)))
          #+:debug (assert constraint)
          (multiple-value-bind 
            (new-unexpanded-disjunctive-constraints new-unexpanded-disjunctive-constraint-store)
            (if visited-constraints
              (multiple-value-prog1
                (remove-constraints-from-constraint-store visited-constraints
                                                          unexpanded-disjunctive-constraints
                                                          unexpanded-disjunctive-constraints-store
                                                          (state-copy-unexpanded-disjunctive-constraints-store-p state)
                                                          state
                                                          #'reset-disjunctive-copy)
                (race-trace ("~&Caching disjunctive constraints ~S from ~S / ~S, state=~S~%"
                             visited-constraints
                             unexpanded-disjunctive-constraints
                             unexpanded-disjunctive-constraints-store
                             (copy-basic-kernel-state-internal state))))
              (values unexpanded-disjunctive-constraints unexpanded-disjunctive-constraints-store))
            (values constraint
                    (let ((new-unexpanded-disjunctive-constraints
                           (if visited-constraints
                             (append new-unexpanded-disjunctive-constraints
                                     visited-constraints)
                             new-unexpanded-disjunctive-constraints)))
                      (changed-kernel-state state
                                            :unexpanded-disjunctive-constraints
                                            new-unexpanded-disjunctive-constraints
                                            :unexpanded-disjunctive-constraints-store
                                            new-unexpanded-disjunctive-constraint-store)))))))
     (unprocessed-constraints
      (values nil state))
     ((or unexpanded-exists-constraints
          (and *use-unexpanded-exists-constraints-store*
               unexpanded-exists-constraints-store
               (not (constraint-store-unused-p unexpanded-exists-constraints-store))
               (not (constraint-store-empty-p unexpanded-exists-constraints-store))))
      (flet ((indirectly-blocked-p (constraint)
               (member (constraint-ind constraint)
                       indirectly-blocked-individuals)))
        (let* ((use-unexpanded-exists-constraints-store
                (and *use-unexpanded-exists-constraints-store*
                     unexpanded-exists-constraints-store
                     (not (constraint-store-unused-p unexpanded-exists-constraints-store))))
               (selected-constraint-1
                (when indirectly-blocked-individuals
                  (if use-unexpanded-exists-constraints-store
                    (find-indset-selected-constraint indirectly-blocked-individuals
                                                     unexpanded-exists-constraints
                                                     unexpanded-exists-constraints-store)
                    (find-if #'indirectly-blocked-p unexpanded-exists-constraints))))
               (labels-top (first labels)))
          (multiple-value-bind
            (selected-constraint-2 visited-constraints)
            (or selected-constraint-1
                (if use-unexpanded-exists-constraints-store
                  (find-best-exists-in-unexpanded-store *prefer-backpropagated-exists* state)
                  (find-best-exists unexpanded-exists-constraints
                                    ;(reverse unexpanded-exists-constraints)
                                    *prefer-backpropagated-exists*
                                    state)))
            (race-trace ("~&Selecting new exists constraint ~S from ~S / ~S, state=~S~%"
                         selected-constraint-2
                         unexpanded-exists-constraints
                         unexpanded-exists-constraints-store
                         (copy-basic-kernel-state-internal state)))
            #+:debug (assert selected-constraint-2)
            ;(break "~S" selected-constraint-2)
            (multiple-value-bind 
              (new-unexpanded-exists-constraints new-unexpanded-exists-constraints-store)
              (if visited-constraints
                (remove-constraints-from-constraint-store visited-constraints
                                                          unexpanded-exists-constraints
                                                          unexpanded-exists-constraints-store
                                                          (state-copy-unexpanded-exists-constraints-store-p state)
                                                          state
                                                          #'reset-exists-copy)
                (values unexpanded-exists-constraints unexpanded-exists-constraints-store))
              (when-debug visited-constraints
                (race-trace ("~&Caching exists constraints ~S from ~S / ~S, state=~S~%"
                             (copy-list visited-constraints)
                             new-unexpanded-exists-constraints
                             new-unexpanded-exists-constraints-store
                             (copy-basic-kernel-state-internal state))))
              (let* ((new-unexpanded-exists-constraints
                      (if visited-constraints
                        (append new-unexpanded-exists-constraints
                                visited-constraints)
                        new-unexpanded-exists-constraints))
                     (new-indirectly-blocked-individuals
                      (if (and indirectly-blocked-individuals
                               (null selected-constraint-1))
                        nil
                        indirectly-blocked-individuals))
                     (new-state-1
                      (changed-kernel-state state
                                            :unexpanded-exists-constraints
                                            new-unexpanded-exists-constraints
                                            :unexpanded-exists-constraints-store
                                            new-unexpanded-exists-constraints-store
                                            :indirectly-blocked-individuals
                                            new-indirectly-blocked-individuals)))
                (multiple-value-bind (new-state-2 obsolete-individuals)
                                     (if (and *inverse-roles*
                                              *gc-of-obsolete-individuals*
                                              (if *use-relation-store*
                                                (not (relation-store-empty-p (state-relation-store state)))
                                                relation-constraints)
                                              labels
                                              (true-label-info-p labels-top)
                                              (not (eql (constraint-ind selected-constraint-2)
                                                        (label-info-ind labels-top)))
                                              (not (member (label-info-ind labels-top)
                                                           unexpanded-exists-constraints
                                                           :key #'constraint-ind))
                                              )
                                       (if (> *ignored-live-individuals-cycles* 10)
                                         (progn
                                           (setf *ignored-live-individuals-cycles* 0)
                                           (remove-obsolete-individuals new-state-1))
                                         (progn
                                           (incf *ignored-live-individuals-cycles*)
                                           new-state-1))
                                       new-state-1)
                  (if obsolete-individuals
                    (let* ((new-labels (remove-obsolete-label-infos obsolete-individuals
                                                                    nil
                                                                    (state-labels new-state-2)))
                           (new-state-3 (if (eq new-labels (state-labels new-state-2))
                                          new-state-2
                                          (changed-kernel-state new-state-2
                                                                :labels new-labels))))
                      (values (unless (member (constraint-ind selected-constraint-2)
                                              obsolete-individuals)
                                selected-constraint-2)
                              new-state-3
                              obsolete-individuals))
                    (values selected-constraint-2 new-state-2)))))))))
     (t (values nil state)))))

(defun remove-obsolete-individuals (state)
  (let ((attribute-constraints (state-attribute-constraints state))
        (concrete-domain-state (state-concrete-domain-state state))
        (unexpanded-exists-constraints (state-unexpanded-exists-constraints state))
        (indirectly-blocked-individuals (state-indirectly-blocked-individuals state))
        (expanded-constraints (state-expanded-constraints state))
        (relation-constraints (state-relation-constraints state))
        (labels (state-labels state)))
    (let* ((live-individuals (collect-live-individuals unexpanded-exists-constraints
                                                       relation-constraints
                                                       state))
           (live-individuals-table
            (if (hash-table-p live-individuals)
              live-individuals
              (loop with table = (clrhash *live-inds-table*)
                    for ind in live-individuals do
                    (setf (gethash ind table) t)
                    finally (return table))))
           (obsolete-individuals-table (clrhash *obsolete-inds-table*))
           (label-individuals-table (clrhash *label-inds-table*)))
      (loop for label in labels do
            (when (true-label-info-p label)
              (let ((ind (label-info-ind label)))
                (setf (gethash ind label-individuals-table) t)
                (if (old-individual-p ind)
                  (setf (gethash ind live-individuals-table) t)
                  (when (gethash ind live-individuals-table)
                    (let ((previous-ind (label-info-previous-ind label)))
                      (when previous-ind
                        (setf (gethash previous-ind live-individuals-table) t))))))))
      (loop for ind being the hash-key of live-individuals-table do
            (unless (or (old-individual-p ind) (gethash ind label-individuals-table))
              (remhash ind live-individuals-table)
              (setf (gethash ind obsolete-individuals-table) t)))
      (loop for label in labels do
            (when (true-label-info-p label)
              (let ((ind (label-info-ind label)))
                (unless (gethash ind live-individuals-table)
                  (setf (gethash ind obsolete-individuals-table) t)))))
      (let ((remaining-expanded-constraints
             (loop for constraint in expanded-constraints
                   for ind = (constraint-ind constraint)
                   if (or (old-individual-p ind) (gethash ind live-individuals-table))
                   collect constraint
                   else do
                   (setf (gethash ind obsolete-individuals-table) t))))
        (multiple-value-bind
          (remaining-relation-constraints new-relation-store)
          (if *use-relation-store*
            (values nil
                    (clean-relation-store (lambda (ind)
                                            (gethash ind live-individuals-table))
                                          state))
            (loop for constraint in relation-constraints
                  for ind-1 = (constraint-ind-1 constraint)
                  for ind-2 = (constraint-ind-2 constraint)
                  unless (or (old-individual-p ind-1)
                             (gethash ind-1 live-individuals-table))
                  collect constraint into removed-constraints
                  and do
                  (setf (gethash ind-1 obsolete-individuals-table) t)
                  (unless (or (old-individual-p ind-2)
                              (gethash ind-2 live-individuals-table))
                    (setf (gethash ind-2 obsolete-individuals-table) t))
                  finally
                  (return
                   (values (if removed-constraints
                             (constraint-set-difference relation-constraints removed-constraints)
                             relation-constraints)
                           (state-relation-store state)))))
          (let* ((remaining-attribute-constraints
                  (loop for constraint in attribute-constraints
                        for ind = (first constraint)
                        if (or (old-individual-p ind) (gethash ind live-individuals-table))
                        collect constraint
                        else do
                        (setf (gethash ind obsolete-individuals-table) t)))
                 (new-concrete-domain-state
                  (if (consp concrete-domain-state)
                    (loop for entry in concrete-domain-state
                          for ind = (first entry)
                          if (or (old-individual-p ind) (gethash ind live-individuals-table))
                          collect entry
                          else do
                          (setf (gethash ind obsolete-individuals-table) t))
                    concrete-domain-state))
                 (obsolete-individuals
                  (loop for ind being the hash-key of obsolete-individuals-table
                        collect ind))
                 (new-indirectly-blocked-individuals
                  (if obsolete-individuals
                    (loop for ind in indirectly-blocked-individuals
                          unless (gethash ind obsolete-individuals-table)
                          collect ind)
                    indirectly-blocked-individuals))
                 (unexpanded-exists-constraints-store
                  (state-unexpanded-exists-constraints-store state)))
            (multiple-value-bind
              (new-unexpanded-exists-constraints new-unexpanded-exists-constraints-store)
              (if (and obsolete-individuals
                       *use-unexpanded-exists-constraints-store*
                       unexpanded-exists-constraints-store
                       (not (constraint-store-unused-p unexpanded-exists-constraints-store)))
                (remove-individuals-from-constraint-store obsolete-individuals
                                                          unexpanded-exists-constraints
                                                          unexpanded-exists-constraints-store
                                                          (state-copy-unexpanded-exists-constraints-store-p
                                                           state)
                                                          state
                                                          #'reset-exists-copy)
                (values (remove-obsolete-constraints obsolete-individuals
                                                     unexpanded-exists-constraints)
                        unexpanded-exists-constraints-store))
              (when-race-trace (not (and (eq expanded-constraints remaining-expanded-constraints)
                                         (eq relation-constraints remaining-relation-constraints)
                                         (eq new-relation-store (state-relation-store state))))
                (let ((removed-constraints
                       (constraint-set-difference expanded-constraints remaining-expanded-constraints))
                      (removed-rel-constraints
                       (unless *use-relation-store*
                         (constraint-set-difference relation-constraints
                                                    remaining-relation-constraints))))
                  (race-trace ("~&GC of inds ~S: ~D+~D obsolete constraints ~S and ~S due to ~S, ~
                                new indirectly-blocked-individuals=~S, state=~S~%"
                               obsolete-individuals
                               (length removed-constraints)
                               (length removed-rel-constraints)
                               removed-constraints
                               removed-rel-constraints
                               new-unexpanded-exists-constraints
                               new-indirectly-blocked-individuals
                               (copy-basic-kernel-state-internal state)))))
              (values
               (changed-kernel-state state
                                     :expanded-constraints remaining-expanded-constraints
                                     :relation-constraints remaining-relation-constraints
                                     :attribute-constraints remaining-attribute-constraints
                                     :concrete-domain-state new-concrete-domain-state
                                     :indirectly-blocked-individuals new-indirectly-blocked-individuals
                                     :unexpanded-exists-constraints new-unexpanded-exists-constraints
                                     :unexpanded-exists-constraints-store
                                     new-unexpanded-exists-constraints-store
                                     :relation-store new-relation-store)
               obsolete-individuals))))))))

(defun collect-live-individuals (unexpanded-exists-constraints relation-constraints state)
  (multiple-value-bind
    (new-inds-table new-inds)
    (let ((unexpanded-exists-constraints-store
           (state-unexpanded-exists-constraints-store state)))
      (if (and *use-unexpanded-exists-constraints-store*
               (consp unexpanded-exists-constraints-store))
        (loop with table = (clrhash *new-inds-table*)
              with new-inds =
              (get-individuals-of-constraint-store unexpanded-exists-constraints
                                                   unexpanded-exists-constraints-store)
              for ind in new-inds do
              (setf (gethash ind table) t)
              finally (return (values table new-inds)))
        (loop with table = (clrhash *new-inds-table*)
              with new-inds = nil
              for constraint in unexpanded-exists-constraints
              for ind = (constraint-ind constraint) do
              (unless (gethash ind table)
                (push ind new-inds)
                (setf (gethash ind table) t))
              finally (return (values table new-inds)))))
    (let ((relation-store (state-relation-store state)))
      (if (or (> (length new-inds) 10)
              (if *use-relation-store*
                (not (relation-store-empty-p relation-store))
                (> (length relation-constraints) 100)))
        (collect-live-individuals-1 new-inds-table
                                    new-inds
                                    relation-constraints
                                    relation-store)
        (collect-live-individuals-2 new-inds relation-constraints new-inds)))))

(defun collect-live-individuals-1 (new-inds-table new-inds relation-constraints store)
  (if *use-relation-store*
    (collect-live-individuals-1-2 new-inds new-inds-table store)
    (collect-live-individuals-1-1 new-inds-table
                                  relation-constraints
                                  (copy-hash-table new-inds-table))))

(defun collect-live-individuals-1-1 (new-inds-table relation-constraints all-inds-table)
  (loop with successor-inds-table = (racer-make-hash-table)
        with empty = t
        with remaining-constraints = nil
        for constraint in relation-constraints
        for ind-1 = (constraint-ind-1 constraint)
        for ind-2 = (constraint-ind-2 constraint)
        do
        (if (and (gethash ind-1 new-inds-table)
                 (not (gethash ind-2 all-inds-table)))
            (progn
              (setf (gethash ind-2 successor-inds-table) t)
              (setf (gethash ind-2 all-inds-table) t)
              (when empty
                (setf empty nil)))
          (push constraint remaining-constraints))
        finally (if (and (not empty) remaining-constraints)
                  (return (collect-live-individuals-1-1 successor-inds-table
                                                        remaining-constraints
                                                        all-inds-table))
                  (return all-inds-table))))

(defun collect-live-individuals-1-2 (new-inds all-inds-table store)
  (loop with remaining-inds = nil
        for ind in new-inds
        for successor-inds = (successor-individuals ind store)
        do
        (loop for successor-ind in successor-inds
              unless (gethash successor-ind all-inds-table)
              do
              (setf (gethash successor-ind all-inds-table) t)
              (push successor-ind remaining-inds))
        finally (if remaining-inds
                  (return (collect-live-individuals-1-2 remaining-inds all-inds-table store))
                  (return all-inds-table))))

(defun collect-live-individuals-2 (new-inds relation-constraints all-inds)
  (loop with successor-inds = nil
        with remaining-constraints = nil
        for constraint in relation-constraints
        do
        (if (member (constraint-ind-1 constraint) new-inds)
            (pushnew (constraint-ind-2 constraint) successor-inds)
          (push constraint remaining-constraints))
        finally (if (and successor-inds remaining-constraints)
                  (return (collect-live-individuals-2 successor-inds
                                                      remaining-constraints
                                                      (union successor-inds all-inds)))
                  (return (union successor-inds all-inds)))))

(defun some-as-all-constraint-p (constraint relation-constraints relation-store)
  (unless (constraint-merging-trigger-p constraint)
    (let ((role (concept-role (constraint-term constraint)))
          (use-relation-store *use-relation-store*))
      (and (role-feature-p role)
           (not (constraint-negated-p constraint))
           (if use-relation-store
               (not (relation-store-empty-p relation-store))
             relation-constraints)
           (loop with ind = (constraint-ind constraint)
                 with feature-ancestors-1 = (role-feature-ancestors role)
                 with feature-ancestors-2 = (or feature-ancestors-1 (list role))
                 with ind-relation-constraints =
                 (if use-relation-store
                     (collect-relation-fillers
                      ind
                      role
                      relation-store
                      (lambda (relation-role)
                        (and (role-feature-p relation-role)
                             (let* ((lfeature-ancestors-1 (role-feature-ancestors relation-role))
                                    (lfeature-ancestors-2 (or lfeature-ancestors-1 (list relation-role))))
                               (not (lists-disjoint-p lfeature-ancestors-2 feature-ancestors-2))))))
                   relation-constraints)
                 with found-rel-constraint = nil
                 for rel-constraint in ind-relation-constraints do
                 (when (or use-relation-store 
                           (matching-relation-constraint-p rel-constraint ind role feature-ancestors-1))
                   (if (member role (role-feature-ancestors (constraint-term rel-constraint)))
                       (return t)
                     (unless found-rel-constraint
                       (setf found-rel-constraint rel-constraint))))
                 finally
                 (when found-rel-constraint
                   (return (values t found-rel-constraint))))))))

(defun exists-as-datatype-constraint-p (constraint)
  (unless (constraint-negated-p constraint)
    (let ((concept (constraint-term constraint)))
      (and (exists-concept-p concept)
           (role-datatype (concept-role concept))
           (cd-concept-p (concept-term concept))
           t))))

(defvar *precompletion*)                ; used to store precompletion
(defvar *save-precompletion* nil)       ; flag indicating when to save a precompletion

(defvar *old-swap-to-expanded-store-threshold* *swap-to-expanded-store-threshold*)
(defvar *old-cache-size-from-constraint-store* *cache-size-from-constraint-store*)  

(defun commit-precompletion (state
                                  constraint-or-list
                                  &optional
                                  (completion-p nil))
  #-(and :debug :lispworks)
  (declare (ignore constraint-or-list))
  #+(and :debug :lispworks)
  (when-debug t
    (if (or-constraint-p constraint-or-list)
      (race-trace ("~&End of precompletion due to disjunctive constraint ~S, state=~S"
                   constraint-or-list state))
      (if (null constraint-or-list)
        (if completion-p
          (race-trace ("~&End of completion, state=~S~%" state))
          (race-trace ("~&End of precompletion due to empty tableau, state=~S~%" state)))
        (race-trace ("~&End of precompletion due to merging of ~S, state=~S~%"
                     constraint-or-list state)))))
  (let* ((expanded-store (state-expanded-store state))
         (expanded-constraints (state-expanded-constraints state)))
    (multiple-value-bind (new-expanded-store new-expanded-store-index)
	(if (and *use-expanded-store*
		 expanded-constraints
		 (not (constraint-store-unused-p expanded-store))
		 (or (and (consp expanded-store)
			  (not (constraint-store-empty-p expanded-store)))
		     (swap-to-expanded-constraint-store-p expanded-constraints expanded-store)))
            (let ((copy-p (state-copy-expanded-store-p state)))
              (when copy-p
                (setf (state-copy-expanded-store-p state) nil))
	      (multiple-value-prog1
		  (swap-to-constraint-store expanded-constraints
					    expanded-store
					    ':expanded
					    (state-expanded-store-index state)
					    copy-p)
                (setf expanded-constraints nil)))
	  expanded-store)
      (let* ((unexpanded-disjunctive-constraints (state-unexpanded-disjunctive-constraints state))
	     (unexpanded-disjunctive-constraints-store
	      (state-unexpanded-disjunctive-constraints-store state))
	     (new-unexpanded-disjunctive-constraints-store
	      (if (and *use-unexpanded-disjunctive-constraints-store*
		       unexpanded-disjunctive-constraints
		       (not (constraint-store-unused-p unexpanded-disjunctive-constraints-store))
		       (or (and (consp unexpanded-disjunctive-constraints-store)
				(not (constraint-store-empty-p unexpanded-disjunctive-constraints-store)))
			   (swap-to-unexpanded-constraint-store-p unexpanded-disjunctive-constraints
								  unexpanded-disjunctive-constraints-store)))
		  (let ((copy-p (state-copy-unexpanded-disjunctive-constraints-store-p state)))
		    (when copy-p
		      (setf (state-copy-unexpanded-disjunctive-constraints-store-p state) nil))
		    (prog1
			(swap-to-constraint-store unexpanded-disjunctive-constraints
						  unexpanded-disjunctive-constraints-store
						  ':unexpanded-disjunctive
						  (state-expanded-store-index state)
						  copy-p
						  nil)
		      (setf unexpanded-disjunctive-constraints nil)))
		unexpanded-disjunctive-constraints-store))
	     (unexpanded-exists-constraints (state-unexpanded-exists-constraints state))
	     (unexpanded-exists-constraints-store (state-unexpanded-exists-constraints-store state))
	     (new-unexpanded-exists-constraints-store
	      (if (and *use-unexpanded-exists-constraints-store*
		       unexpanded-exists-constraints
		       (not (constraint-store-unused-p unexpanded-exists-constraints-store))
		       (or (and (consp unexpanded-exists-constraints-store)
				(not (constraint-store-empty-p unexpanded-exists-constraints-store)))
			   (swap-to-unexpanded-constraint-store-p unexpanded-exists-constraints
								  unexpanded-exists-constraints-store)))
		  (let ((copy-p (state-copy-unexpanded-exists-constraints-store-p state)))
		    (when copy-p
		      (setf (state-copy-unexpanded-exists-constraints-store-p state) nil))
		    (prog1
			(swap-to-constraint-store unexpanded-exists-constraints
						  unexpanded-exists-constraints-store
						  ':unexpanded-exists
						  (state-expanded-store-index state)
						  copy-p
						  nil)
		      (setf unexpanded-exists-constraints nil)))
		unexpanded-exists-constraints-store))
	     (new-concrete-domain-state (copy-solver-state (state-concrete-domain-state state)))
	     (new-state
	      (changed-kernel-state state
				    :expanded-constraints expanded-constraints
				    :unexpanded-disjunctive-constraints
				    unexpanded-disjunctive-constraints
				    :unexpanded-exists-constraints unexpanded-exists-constraints
				    :expanded-store new-expanded-store
				    :expanded-store-index new-expanded-store-index
				    :unexpanded-disjunctive-constraints-store
				    new-unexpanded-disjunctive-constraints-store
				    :unexpanded-exists-constraints-store
				    new-unexpanded-exists-constraints-store
				    :concrete-domain-state new-concrete-domain-state)))
        (protect-relation-store (state-relation-store new-state))
	(unless completion-p
	  (setf *in-precompletion* nil)
	  (setf *swap-to-expanded-store-threshold* *old-swap-to-expanded-store-threshold*)
	  (setf *cache-size-from-constraint-store* *old-cache-size-from-constraint-store*))
	(if (and *use-abox-precompletion* *save-precompletion*)
	    (progn
	      #+:debug (assert (eql *precompletion* t))
	      (setf *precompletion* (make-precompletion :state (copy-basic-kernel-state new-state t)
							:language *dl-prover-language*
							:individual-synonyms
							(compute-individual-synonyms new-state)
							:ind-counter-some-satisfiable
							*ind-counter-some-satisfiable*))
	      (when-debug t
			  (if completion-p
			      (race-trace ("~&Saving completion state ~S~%" *precompletion*))
			    (race-trace ("~&Saving precompletion state ~S~%" *precompletion*))))
	      new-state)
	  new-state)))))

(defun compute-individual-synonyms (state)
  (unless *use-unique-name-assumption*
    (let ((synonym-map nil))
      (let ((store (state-relation-store state)))
        (if (and *use-relation-store* store)
          (loop-over-all-rel-constraints (relation-constraint store)
            do
            (let ((synonyms (constraint-ind-1-synonyms relation-constraint)))
              (when synonyms
                (unless synonym-map
                  (setf synonym-map (racer-make-hash-table)))
                (record-individual-synonym (constraint-ind-1 relation-constraint)
                                           synonyms
                                           synonym-map)))
            (let ((synonyms (constraint-ind-2-synonyms relation-constraint)))
              (when synonyms
                (unless synonym-map
                  (setf synonym-map (racer-make-hash-table)))
                (record-individual-synonym (constraint-ind-2 relation-constraint)
                                           synonyms
                                           synonym-map))))
          (loop for relation-constraint in (state-relation-constraints state)
                do
                (let ((synonyms (constraint-ind-1-synonyms relation-constraint)))
                  (when synonyms
                    (unless synonym-map
                      (setf synonym-map (racer-make-hash-table)))
                    (record-individual-synonym (constraint-ind-1 relation-constraint)
                                               synonyms
                                               synonym-map)))
                (let ((synonyms (constraint-ind-2-synonyms relation-constraint)))
                  (when synonyms
                    (unless synonym-map
                      (setf synonym-map (racer-make-hash-table)))
                    (record-individual-synonym (constraint-ind-2 relation-constraint)
                                               synonyms
                                               synonym-map))))))
      (loop for constraint in (state-expanded-constraints state)
            do
            (let ((synonyms (constraint-ind-synonyms constraint)))
              (when synonyms
                (unless synonym-map
                  (setf synonym-map (racer-make-hash-table)))
                (record-individual-synonym (constraint-ind constraint)
                                           synonyms
                                           synonym-map))))
      (let ((store (state-expanded-store state)))
        (when (and *use-expanded-store* (consp store))
          (iterate-over-all-constraints (constraint store)
            do
            (let ((synonyms (constraint-ind-synonyms constraint)))
              (when synonyms
                (unless synonym-map
                  (setf synonym-map (racer-make-hash-table)))
                (record-individual-synonym (constraint-ind constraint) synonyms synonym-map))))))
      #+:debug
      (when synonym-map
        (assert (loop for ind-name being the hash-value of synonym-map
                      never (gethash ind-name synonym-map))))
      synonym-map)))

(defun record-individual-synonym (ind synonyms synonym-map)
  (let ((old-synonym (gethash ind synonym-map)))
    (when old-synonym
      #+:debug (assert (null (gethash old-synonym synonym-map)))
      (let ((obsolete-synonyms
             (loop for synonym being the hash-key of synonym-map using (hash-value new-ind)
                   when (eql new-ind ind)
                   collect synonym)))
        (when obsolete-synonyms
          (loop for synonym in obsolete-synonyms do
                (setf (gethash synonym synonym-map) old-synonym)))))
    (loop with true-ind = (or old-synonym ind)
          for synonym in synonyms
          for entry = (gethash synonym synonym-map)
          do
          (if entry
              (unless (eql true-ind entry)
                (let ((entry-2 (gethash entry synonym-map)))
                  #-:debug (declare (ignore entry-2))
                  #+:debug (assert (or (null entry-2) (eql entry-2 true-ind)))
                  (setf (gethash entry synonym-map) true-ind)))
            (setf (gethash synonym synonym-map) true-ind)))))

(defun added-constraints-satisfiable (conclusion-concept-constraints
                                           conclusion-relation-constraints
                                           state
                                           expand-role-domain-p
                                           expand-domain-range-of-rel-constraints)
  #+:allegro (declare (:explain :tailmerging))
  
  ;; added by MW
  ;; this shows in GUIs (P4, Porter)
  ;; that Racer is still reasoning, 
  ;; and also, set-progress-value 
  ;; will exit if abort was requested. 
  ;; (set-progress-value :tick)
  
  (multiple-value-bind (clashed-p new-state old-new-state)
                       (added-constraints-satisfiable-1 conclusion-concept-constraints
                                                        conclusion-relation-constraints
                                                        state
                                                        expand-role-domain-p
                                                        expand-domain-range-of-rel-constraints)
    (if clashed-p
      (handle-clash-with-backtrack-state new-state nil nil nil nil)
      (added-constraints-satisfiable-2 new-state old-new-state nil nil nil))))

(defun added-constraints-satisfiable-1 (conclusion-concept-constraints
                                              conclusion-relation-constraints
                                              state
                                              expand-role-domain-p
                                              expand-domain-range-of-rel-constraints)
  (multiple-value-bind (clashed-p 
                        new-state-1
                        new-unexpanded-deterministic-constraints
                        new-unexpanded-exists-constraints
                        new-unexpanded-disjunctive-constraints
                        new-relation-constraints-1
                        new-unexpanded-exists-constraints-store
                        new-unexpanded-disjunctive-constraints-store)
                       (distribute-new-constraints conclusion-concept-constraints
                                                   conclusion-relation-constraints
                                                   state
                                                   expand-role-domain-p
                                                   expand-domain-range-of-rel-constraints)
    (if clashed-p
      (values t new-state-1)
      (values nil
              (changed-kernel-state new-state-1
                                    :unexpanded-deterministic-constraints
                                    new-unexpanded-deterministic-constraints
                                    :unexpanded-exists-constraints
                                    new-unexpanded-exists-constraints
                                    :unexpanded-disjunctive-constraints
                                    new-unexpanded-disjunctive-constraints
                                    :relation-constraints new-relation-constraints-1
                                    :unexpanded-exists-constraints-store
                                    new-unexpanded-exists-constraints-store
                                    :unexpanded-disjunctive-constraints-store
                                    new-unexpanded-disjunctive-constraints-store)
              new-state-1))))


(defun added-constraints-satisfiable-2 (state old-state unused-1 unused-2 unused-3)
  (declare (ignore unused-1 unused-2 unused-3))
  #+:allegro (declare (:explain :tailmerging))
  (multiple-value-bind (selected-constraint new-state-1 obsolete-individuals)
                       (select-constraint state)
    (let* ((new-labels
            (remove-obsolete-label-infos obsolete-individuals
                                         (if (eq (state-indirectly-blocked-individuals
                                                  old-state)
                                                 (state-indirectly-blocked-individuals
                                                  new-state-1))
                                           nil
                                           (state-indirectly-blocked-individuals new-state-1))
                                         (state-labels new-state-1)))
           (new-state-2
            (changed-kernel-state new-state-1
                                  :labels new-labels)))
      (alc-cs-satisfiable selected-constraint new-state-2 nil nil nil))))


(defun distribute-new-constraints (conclusion-concept-constraints
                                        conclusion-relation-constraints
                                        state
                                        expand-role-domain-p
                                        expand-domain-range-of-rel-constraints)
  (let* ((relation-constraints (state-relation-constraints state))
         (relation-store (state-relation-store state))
         (expanded-constraints (state-expanded-constraints state))
         (new-relation-constraints
          (cond ((null conclusion-relation-constraints) relation-constraints)
                ((listp conclusion-relation-constraints)
                 (append conclusion-relation-constraints relation-constraints))
                (t (cons conclusion-relation-constraints relation-constraints))))
         (conclusion-concept-constraints
          (nconc (when expand-domain-range-of-rel-constraints
                   (consider-incremental-domain-and-range-restrictions new-relation-constraints
                                                                       relation-store))
                 (if (listp conclusion-concept-constraints)
                   conclusion-concept-constraints
                   (list conclusion-concept-constraints))))
         (new-state-1
          (if (consp conclusion-relation-constraints)
            (let ((new-relation-store
                   (if *use-relation-store*
                     (update-relation-store nil
                                            conclusion-relation-constraints
                                            state
                                            (non-det-old-rel-constraint-p
                                             conclusion-relation-constraints))
                     relation-store)))
              (changed-kernel-state state
                                    :relation-store new-relation-store))
            state)))
    (multiple-value-bind (clashed-p conclusion-concept-constraints-2)
        (if (consp conclusion-relation-constraints)
            (let ((abox (state-abox (state-parameters state))))
              (multiple-value-bind (clash-p culprit-1 culprit-2)
                  (if *use-relation-store*
                      (asymmetric-role-clash-in-extended-relation-store-p (state-abox (state-parameters new-state-1))
                                                                          (state-relation-store new-state-1)
                                                                          conclusion-relation-constraints)
                    (asymmetric-role-clash-in-relation-constraints-p abox new-relation-constraints))
                (if clash-p
                    (handle-clash culprit-1
                                  (constraint-or-dependencies culprit-1)
                                  new-relation-constraints
                                  culprit-2
                                  (when culprit-2 
                                    (constraint-or-dependencies culprit-2)))
                  (distribute-new-constraints-1 new-relation-constraints
                                                conclusion-relation-constraints
                                                conclusion-concept-constraints
                                                expanded-constraints
                                                relation-constraints
                                                new-state-1))))
          (values nil conclusion-concept-constraints))
          (when clashed-p
            (return-from distribute-new-constraints
              (values t new-state-1)))
          (multiple-value-bind (clashed-p
                                new-state-2
                                new-unexpanded-deterministic-constraints
                                new-unexpanded-exists-constraints
                                new-unexpanded-disjunctive-constraints
                                new-relation-constraints-1
                                new-unexpanded-exists-constraints-store
                                new-unexpanded-disjunctive-constraints-store)
              (if conclusion-concept-constraints-2
                  (distribute-new-constraints-2 conclusion-concept-constraints-2
                                                new-relation-constraints
                                                expanded-constraints
                                                new-state-1
                                                expand-role-domain-p)
                (values nil
                        new-state-1
                        (state-unexpanded-deterministic-constraints state)
                        (state-unexpanded-exists-constraints state)
                        (state-unexpanded-disjunctive-constraints state)
                        new-relation-constraints
                        (state-unexpanded-exists-constraints-store state)
                        (state-unexpanded-disjunctive-constraints-store state)))
            (values clashed-p
                    (if clashed-p 
                        new-state-1
                      new-state-2)
                    new-unexpanded-deterministic-constraints
                    new-unexpanded-exists-constraints
                    new-unexpanded-disjunctive-constraints
                    new-relation-constraints-1
                    new-unexpanded-exists-constraints-store
                    new-unexpanded-disjunctive-constraints-store)))))

(defun distribute-new-constraints-1 (new-relation-constraints
                                     conclusion-relation-constraints
                                     conclusion-concept-constraints
                                     expanded-constraints
                                     relation-constraints
                                     state)
  (declare (ignore relation-constraints))
  (loop with temp-expanded-constraints = (append conclusion-concept-constraints
                                                 expanded-constraints)
        with top-concept = *top-concept*
        with use-unique-name-assumption = *use-unique-name-assumption*
        with table = nil
        with abox = (state-abox (state-parameters state))
        for rel-constraint in conclusion-relation-constraints
        do
        (when (and (not use-unique-name-assumption) (clash-in-relation-constraint-p abox rel-constraint))
          (progn
            (handle-clash rel-constraint
                          (constraint-or-dependencies rel-constraint)
                          new-relation-constraints)
            (return-from distribute-new-constraints-1 t)))
        (let ((matching-constraints
               (get-matching-all-some-or-at-most-constraints rel-constraint temp-expanded-constraints state)))
          (loop for constraint in matching-constraints
                for concept = (constraint-term constraint)
                do
                (if (and (constraint-negated-p constraint)
                         (at-least-concept-p concept))
                    (let ((at-most-bound (1- (concept-number-restriction concept)))
                          (qualification (concept-term concept)))
                      (multiple-value-bind
                          (role-successors first-rel-constraint all-succ-inds)
                          (count-role-successors (constraint-ind-1 rel-constraint)
                                                 (make-bound (concept-role concept)
                                                             at-most-bound
                                                             constraint
                                                             :qualification qualification)
                                                 nil
                                                 new-relation-constraints
                                                 temp-expanded-constraints
                                                 state)
                        (when (> role-successors at-most-bound)
                          (race-trace ("~&at-most-constraint ~S violated: (> ~D ~D)~%"
                                       constraint role-successors at-most-bound))
                          (if use-unique-name-assumption
                              (progn
                                (handle-clash constraint
                                              (constraint-or-dependencies constraint)
                                              new-relation-constraints
                                              first-rel-constraint
                                              (constraint-or-dependencies first-rel-constraint))
                                (return-from distribute-new-constraints-1 t))
                            (let* ((trigger-constraint
                                    (when (or (and (not (eq qualification top-concept))
                                                   first-rel-constraint
                                                   (or (null (constraint-signature first-rel-constraint))
                                                       (not (member constraint
                                                                    (signature-dependencies
                                                                     (constraint-signature first-rel-constraint))))))
                                              (and (eq qualification top-concept)
                                                   (> (length all-succ-inds) at-most-bound)))
                                      (let* ((ind (constraint-ind-1 first-rel-constraint))
                                             (role (constraint-term first-rel-constraint))
                                             (key (list ind (role-name role))))
                                        (unless table
                                          (setf table (racer-make-hash-table :test 'equal)))
                                        (unless (gethash key table)
                                          (setf (gethash key table) t)
                                          (concluded-concept-constraint
                                           ind
                                           (without-taxonomic-encoding
                                             (encode-concept-term `(at-least 1 ,role)))
                                           first-rel-constraint
                                           constraint))))))
                              (when trigger-constraint
                                (setf (constraint-merging-trigger-p trigger-constraint) constraint)
                                (race-trace ("~&xAdding trigger exists constraint ~S for ~S ~%"
                                             trigger-constraint first-rel-constraint))
                                (push trigger-constraint conclusion-concept-constraints)))))))
                  (setf conclusion-concept-constraints
                        (append (expand-relation-constraints constraint
                                                             conclusion-relation-constraints
                                                             (some-as-all-constraint-p
                                                              constraint
                                                              new-relation-constraints
                                                              (state-relation-store state))
                                                             (state-relation-store state))
                                conclusion-concept-constraints))))))
  (values nil conclusion-concept-constraints))

(defun distribute-new-constraints-2 (conclusion-concept-constraints
                                          relation-constraints
                                          expanded-constraints
                                          state
                                          expand-role-domain-p)
  ;;; return 8 values: clashed-p, new-state, unexpanded-deterministic-constraints,
  ;;; added-exists-constraints, unexpanded-disjunctive-constraints, relation-constraints,
  ;;; unexpanded-exists-constraints-store, unexpanded-disjunctive-constraints-store
  (let ((new-unexpanded-exists-constraints (state-unexpanded-exists-constraints state))
        (added-unexpanded-exists-constraints nil)
        (all-added-unexpanded-exists-constraints nil)
        (new-unexpanded-disjunctive-constraints nil)
        (added-unexpanded-disjunctive-constraints nil)
        (new-unexpanded-exists-constraints-store
         (state-unexpanded-exists-constraints-store state))
        (new-unexpanded-disjunctive-constraints-store
         (state-unexpanded-disjunctive-constraints-store state))
        (added-expanded-constraints nil)
        (unprocessed-constraints (state-unprocessed-constraints state)))
    (multiple-value-bind (potential-some-as-all-constraints other-conclusion-concept-constraints-1)
                         (loop for constraint in conclusion-concept-constraints
                               if (and (exists-constraint-p constraint)
                                       (not (constraint-negated-p constraint))
                                       (role-feature-p (concept-role (constraint-term constraint))))
                               collect constraint into potential-some-as-all-constraints
                               else
                               collect constraint into other-constraints
                               finally
                               (return (values potential-some-as-all-constraints other-constraints)))
      (multiple-value-bind (new-unexpanded-deterministic-constraints
                            new-relation-constraints
                            other-conclusion-concept-constraints-2
                            ind-role-domain-table
                            new-state-1)
                           (distribute-new-constraints-2-1 relation-constraints
                                                           state
                                                           expand-role-domain-p
                                                           potential-some-as-all-constraints
                                                           other-conclusion-concept-constraints-1)
        ;(break)
        (loop with relation-store = (state-relation-store new-state-1)
              with expanded-store = (state-expanded-store new-state-1)
              with expanded-store-index = (state-expanded-store-index new-state-1)
              with update-dependencies = (and *model-merging*
                                              (or *use-alternate-models* *use-alternate-ind-models*)
                                              ;(boundp '*expanded-model*)
                                              *save-expanded-model*)
              with copy-exists-p = (state-copy-unexpanded-exists-constraints-store-p state)
              with copy-disjunctive-p = 
              (state-copy-unexpanded-disjunctive-constraints-store-p state)
              with use-unexpanded-exists-constraints-store =
              *use-unexpanded-exists-constraints-store*
              with use-unexpanded-disjunctive-constraints-store = 
              *use-unexpanded-disjunctive-constraints-store*
              for concept-constraints = other-conclusion-concept-constraints-2
              then added-constraints
              for added-constraints = nil
              do
              (multiple-value-setq
                (new-unexpanded-deterministic-constraints
                 new-unexpanded-exists-constraints
                 added-unexpanded-exists-constraints
                 all-added-unexpanded-exists-constraints
                 new-unexpanded-disjunctive-constraints
                 added-unexpanded-disjunctive-constraints
                 new-unexpanded-exists-constraints-store
                 new-unexpanded-disjunctive-constraints-store
                 added-constraints
                 copy-disjunctive-p
                 copy-exists-p
                 ind-role-domain-table
                 added-expanded-constraints
                 unprocessed-constraints)
                (distribute-new-constraints-2-2 state
                                                expand-role-domain-p
                                                relation-store
                                                expanded-constraints
                                                expanded-store
                                                expanded-store-index
                                                concept-constraints
                                                use-unexpanded-disjunctive-constraints-store
                                                new-relation-constraints
                                                copy-disjunctive-p
                                                copy-exists-p
                                                use-unexpanded-exists-constraints-store
                                                update-dependencies
                                                ind-role-domain-table
                                                new-unexpanded-deterministic-constraints
                                                new-unexpanded-exists-constraints
                                                added-unexpanded-exists-constraints
                                                all-added-unexpanded-exists-constraints
                                                new-unexpanded-disjunctive-constraints
                                                added-unexpanded-disjunctive-constraints
                                                new-unexpanded-exists-constraints-store
                                                new-unexpanded-disjunctive-constraints-store
                                                unprocessed-constraints))
              (when added-constraints
                (setf other-conclusion-concept-constraints-2
                      (append added-constraints other-conclusion-concept-constraints-2)))
              until (null added-constraints)
              finally
              (let ((clashed-p nil))
                (multiple-value-setq (clashed-p
                                      new-state-1 
                                      new-unexpanded-deterministic-constraints
                                      added-unexpanded-exists-constraints
                                      new-unexpanded-disjunctive-constraints
                                      new-unexpanded-exists-constraints-store
                                      new-unexpanded-disjunctive-constraints-store
                                      copy-disjunctive-p
                                      copy-exists-p)
                  (distribute-new-constraints-2-3 new-state-1
                                                  expanded-constraints
                                                  potential-some-as-all-constraints
                                                  other-conclusion-concept-constraints-2
                                                  use-unexpanded-disjunctive-constraints-store
                                                  new-relation-constraints
                                                  copy-disjunctive-p
                                                  copy-exists-p
                                                  use-unexpanded-exists-constraints-store
                                                  new-unexpanded-deterministic-constraints
                                                  new-unexpanded-disjunctive-constraints
                                                  added-unexpanded-disjunctive-constraints
                                                  added-unexpanded-exists-constraints
                                                  all-added-unexpanded-exists-constraints
                                                  new-unexpanded-exists-constraints-store
                                                  new-unexpanded-disjunctive-constraints-store))
                (if clashed-p
                  (return t)
                  (return (values nil
                                  (let ((new-expanded-constraints
                                         (if added-expanded-constraints
                                           (nconc added-expanded-constraints
                                                  (state-expanded-constraints new-state-1))
                                           (state-expanded-constraints new-state-1))))
                                    (changed-kernel-state new-state-1
                                                          :unprocessed-constraints unprocessed-constraints
                                                          :copy-unexpanded-disjunctive-constraints-store-p
                                                          copy-disjunctive-p
                                                          :copy-unexpanded-exists-constraints-store-p
                                                          copy-exists-p
                                                          :expanded-constraints new-expanded-constraints))
                                  new-unexpanded-deterministic-constraints
                                  (if (and use-unexpanded-exists-constraints-store
                                           (consp new-unexpanded-exists-constraints-store))
                                    (constraint-set-union added-unexpanded-exists-constraints
                                                          (state-unexpanded-exists-constraints new-state-1))
                                    (constraint-set-union added-unexpanded-exists-constraints
                                                          new-unexpanded-exists-constraints))
                                  new-unexpanded-disjunctive-constraints
                                  new-relation-constraints
                                  new-unexpanded-exists-constraints-store
                                  new-unexpanded-disjunctive-constraints-store)))))))))

(defun distribute-new-constraints-2-1 (relation-constraints
                                            state
                                            expand-role-domain-p
                                            potential-some-as-all-constraints
                                            other-conclusion-concept-constraints)
  ;;; return 5 values: new-unexpanded-deterministic-constraints, new-relation-constraints,
  ;;; new-other-conclusion-concept-constraints, ind-role-domain-table, new-state
  (let ((new-state state)
        (new-unexpanded-deterministic-constraints (state-unexpanded-deterministic-constraints state))
        (new-relation-constraints relation-constraints)
        (ind-role-domain-table nil)
        (new-other-conclusion-concept-constraints other-conclusion-concept-constraints))
    (loop with remaining-candidates = potential-some-as-all-constraints
          with update-dependencies = (and *model-merging*
                                          (or *use-alternate-models* *use-alternate-ind-models*)
                                          ;(boundp '*expanded-model*)
                                          *save-expanded-model*)
          with use-relation-store = *use-relation-store*
          for unchanged = t
          do
          (loop with processed-constraints = nil
                for new-constraint in remaining-candidates
                do
                (multiple-value-bind (some-as-all rel-constraint)
                                     (some-as-all-constraint-p new-constraint
                                                               new-relation-constraints
                                                               (state-relation-store new-state))
                  (declare (ignore some-as-all))
                  (when rel-constraint
                    (let* ((new-relation-constraint-1
                            (concluded-relation-constraint (constraint-ind-1 rel-constraint)
                                                           (constraint-ind-2 rel-constraint)
                                                           new-constraint))
                           (inv-role (role-inverse-internal
                                      (concept-role
                                       (constraint-term new-constraint))))
                           (new-relation-constraint-2
                            (concluded-relation-constraint (constraint-ind-2 rel-constraint)
                                                           (constraint-ind-1 rel-constraint)
                                                           new-constraint
                                                           inv-role))
                           (added-relation-constraints (list new-relation-constraint-2
                                                             new-relation-constraint-1)))
                      (setf unchanged nil)
                      (push new-constraint processed-constraints)
                      (push new-constraint new-unexpanded-deterministic-constraints)
                      (if use-relation-store
                        (setf new-state
                              (let ((new-relation-store
                                     (update-relation-store nil
                                                            added-relation-constraints
                                                            new-state)))
                                (changed-kernel-state new-state
                                                      :relation-store new-relation-store)))
                        (setf new-relation-constraints
                              (append added-relation-constraints new-relation-constraints)))
                      (loop for all-constraint in (append (get-matching-all-or-some-constraints
                                                           new-relation-constraint-2
                                                           new-state)
                                                          (get-matching-all-or-some-constraints
                                                           new-relation-constraint-1
                                                           new-state))
                            for some-as-all = (some-as-all-constraint-p all-constraint
                                                                        added-relation-constraints
                                                                        (state-relation-store new-state))
                            do
                            (setf new-other-conclusion-concept-constraints
                                  (append (expand-relation-constraints all-constraint
                                                                       added-relation-constraints
                                                                       some-as-all
                                                                       (state-relation-store new-state))
                                          new-other-conclusion-concept-constraints))))
                    (when expand-role-domain-p
                      (let ((concept-role-domain
                             (concept-role-domain (constraint-term new-constraint))))
                        (when concept-role-domain
                          (unless ind-role-domain-table
                            (setf ind-role-domain-table (racer-make-hash-table)))
                          (let* ((ind (constraint-ind new-constraint))
                                 (old-constraint
                                  (cdr (find concept-role-domain
                                             (gethash ind ind-role-domain-table)
                                             :key #'car))))
                            (if old-constraint
                              (when update-dependencies
                                (let ((domain-constraint
                                       (concluded-concept-constraint (constraint-ind new-constraint)
                                                                     concept-role-domain
                                                                     new-constraint)))
                                  (setf (constraint-derived-from old-constraint)
                                        (union (constraint-derived-from domain-constraint)
                                               (constraint-derived-from old-constraint)))))
                              (let ((domain-constraint
                                     (concluded-concept-constraint (constraint-ind new-constraint)
                                                                   concept-role-domain
                                                                   new-constraint)))
                                (push domain-constraint new-other-conclusion-concept-constraints)
                                (push (cons concept-role-domain domain-constraint)
                                      (gethash ind ind-role-domain-table))))))))))
                finally
                (setf remaining-candidates (constraint-set-difference remaining-candidates
                                                                      processed-constraints)))
          until unchanged
          finally
          (return (values new-unexpanded-deterministic-constraints
                          new-relation-constraints
                          (nconc remaining-candidates new-other-conclusion-concept-constraints)
                          ind-role-domain-table
                          new-state)))))

(defun optimize-datatype-role-fillers (constraint abox table-list dl-prover-language)
  (and (exists-as-datatype-constraint-p constraint)
       (not (role-has-feature-ancestors-p (concept-role (constraint-term constraint))))
       (dl-true-simple-datatype-concrete-domains dl-prover-language)
       (let ((ind (constraint-ind constraint)))
         (or (not (true-old-individual-p ind))
             (dl-true-simple-datatype-concrete-domains
              (or (loop for table in table-list 
                        thereis (and table (gethash ind table)))
                  (individual-language (find-individual abox ind))))))))

(defun distribute-new-constraints-2-2 (state
                                             expand-role-domain-p
                                             relation-store
                                             expanded-constraints
                                             expanded-store
                                             expanded-store-index
                                             concept-constraints
                                             use-unexpanded-disjunctive-constraints-store
                                             new-relation-constraints
                                             copy-disjunctive-p
                                             copy-exists-p
                                             use-unexpanded-exists-constraints-store
                                             update-dependencies
                                             ind-role-domain-table
                                             unexpanded-deterministic-constraints
                                             unexpanded-exists-constraints
                                             added-unexpanded-exists-constraints
                                             all-added-unexpanded-exists-constraints
                                             new-unexpanded-disjunctive-constraints
                                             added-unexpanded-disjunctive-constraints
                                             unexpanded-exists-constraints-store
                                             unexpanded-disjunctive-constraints-store
                                             unprocessed-constraints)
  ;;; return 12 values: new-unexpanded-deterministic-constraints, new-unexpanded-exists-constraints,
  ;;; added-unexpanded-exists-constraints, new-unexpanded-disjunctive-constraints,
  ;;; added-unexpanded-disjunctive-constraints, new-unexpanded-exists-constraints-store,
  ;;; new-unexpanded-disjunctive-constraints-store, added-constraints, copy-disjunctive-p,
  ;;; copy-exists-p, ind-role-domain-table, added-expanded-constraints, unprocessed-constraints
  (let ((new-unexpanded-deterministic-constraints unexpanded-deterministic-constraints)
        (new-unexpanded-exists-constraints unexpanded-exists-constraints)
        (new-unexpanded-exists-constraints-store unexpanded-exists-constraints-store)
        (new-unexpanded-disjunctive-constraints-store unexpanded-disjunctive-constraints-store)
        (added-constraints nil)
        (added-expanded-constraints nil))
    (loop with ignore-abox-redundant-exists = *ignore-abox-redundant-exists*
          with optimize-datatype-role-fillers =
          (and *optimize-datatype-role-fillers*
               (not (dl-full-concrete-domains *dl-prover-language*)))
          with abox = (state-abox (state-parameters state))
          with table = (state-old-individuals-dl-language-table state)
	  for do-not-expand-role-domain = nil
          for new-constraint in concept-constraints do
          (cond
           ((or-constraint-p new-constraint)
            (if (deterministic-or-constraint-p 
                 (constraint-open-clauses-counter new-constraint))
              (push new-constraint new-unexpanded-deterministic-constraints)
              (progn
                (push new-constraint new-unexpanded-disjunctive-constraints)
                (push new-constraint added-unexpanded-disjunctive-constraints)
                (when (and use-unexpanded-disjunctive-constraints-store
                           (swap-to-unexpanded-constraint-store-p added-unexpanded-disjunctive-constraints
                                                                  new-unexpanded-disjunctive-constraints-store))
                  (setf new-unexpanded-disjunctive-constraints-store
                        (add-to-constraint-store added-unexpanded-disjunctive-constraints
                                                 new-unexpanded-disjunctive-constraints-store
                                                 ':unexpanded-disjunctive
                                                 :copy-p copy-disjunctive-p))
                  (when copy-disjunctive-p
                    (setf copy-disjunctive-p nil))
                  (setf added-unexpanded-disjunctive-constraints nil)
                  ;(break "1")
                  ))))
           ((exists-constraint-p new-constraint)
            (if (or (and optimize-datatype-role-fillers
                         (optimize-datatype-role-fillers new-constraint abox table *dl-prover-language*))
                    (some-as-all-constraint-p new-constraint
                                              new-relation-constraints
                                              relation-store))
	      (push new-constraint new-unexpanded-deterministic-constraints)
              (if (and ignore-abox-redundant-exists
                       relation-store
                       (not (constraint-merging-trigger-p new-constraint))
                       (not (constraint-signature new-constraint))
                       (let ((concept (constraint-term new-constraint)))
                         (and (some-concept-p concept)
                              (role-successor-exists-p (constraint-ind new-constraint)
                                                       (concept-role concept)
                                                       (concept-term concept)
                                                       expanded-constraints
                                                       expanded-store
                                                       expanded-store-index
                                                       relation-store))))
                (progn
                  ;(push new-constraint added-expanded-constraints)
                  (race-trace ("~&Ignoring ABox-implied some-constraint ~S" new-constraint))
                  ;(when (constraint-signature new-constraint) (break "~S" new-constraint))
		  (setf do-not-expand-role-domain t)
                  )
                (progn
                  ;(break "~S" new-constraint)
                  (let ((old-constraint
                         (unless (constraint-merging-trigger-p new-constraint)
                           (if (or (not use-unexpanded-exists-constraints-store)
                                   (constraint-store-unused-p new-unexpanded-exists-constraints-store)
                                   (null new-unexpanded-exists-constraints-store))
                             (find new-constraint new-unexpanded-exists-constraints
                                   :test #'constraint-equal-test)
                             (or (find new-constraint unexpanded-exists-constraints
                                       :test #'constraint-equal-test)
                                 (find-if-selected-constraints
                                  (constraint-ind new-constraint)
                                  (lambda (old-constraint)
                                    (same-ind-constraint-equal-test old-constraint new-constraint))
                                  added-unexpanded-exists-constraints
                                  new-unexpanded-exists-constraints-store))))))
                    (if old-constraint
                      (progn
                        (when update-dependencies
                          (setf (constraint-derived-from old-constraint)
                                (union (constraint-derived-from new-constraint)
                                       (constraint-derived-from old-constraint))))
                        (race-trace ("~&Ignoring duplicate exists constraint, old=~S, new=~S~%"
                                     old-constraint new-constraint))
                        ;(break "~S: ~S / ~S" new-constraint old-constraint new-unexpanded-exists-constraints-store)
                        )
                      (progn
                        (push new-constraint added-unexpanded-exists-constraints)
                        (push new-constraint all-added-unexpanded-exists-constraints)
                        (push new-constraint new-unexpanded-exists-constraints)
                        (when (and use-unexpanded-exists-constraints-store
                                   added-unexpanded-exists-constraints
                                   (swap-to-unexpanded-constraint-store-p added-unexpanded-exists-constraints
                                                                          new-unexpanded-exists-constraints-store))
                          (setf new-unexpanded-exists-constraints-store
                                (add-to-constraint-store added-unexpanded-exists-constraints
                                                         new-unexpanded-exists-constraints-store
                                                         ':unexpanded-exists
                                                         :copy-p copy-exists-p))
                          (when copy-exists-p
                            (setf copy-exists-p nil))
                          (setf added-unexpanded-exists-constraints nil)
                          ;(break "1")
                          )))))))
            (let ((concept (constraint-concept new-constraint)))
              (when (some-concept-p concept)
                (let ((role (concept-role concept)))
                  (when (and (or (role-reflexive-p role)
                                 (some #'user-defined-role-reflexive-p (role-ancestors-internal role)))
                             (role-feature-p role))
                    (push (concluded-concept-constraint (constraint-ind new-constraint)
                                                        (concept-term concept)
                                                        new-constraint)
                          added-constraints)))))
            (when (and expand-role-domain-p (not do-not-expand-role-domain))
              (let ((concept-role-domain
                     (concept-role-domain (constraint-term new-constraint))))
                (when concept-role-domain
                  (unless ind-role-domain-table
                    (setf ind-role-domain-table (racer-make-hash-table)))
                  (let* ((ind (constraint-ind new-constraint))
                         (old-constraint
                          (cdr (find concept-role-domain
                                     (gethash ind ind-role-domain-table)
                                     :key #'car))))
                    (if old-constraint
                      (when update-dependencies
                        (let ((domain-constraint
                               (concluded-concept-constraint ind
                                                             concept-role-domain
                                                             new-constraint)))
                          (setf (constraint-derived-from old-constraint)
                                (union (constraint-derived-from domain-constraint)
                                       (constraint-derived-from old-constraint)))))
                      (let ((domain-constraint
                             (concluded-concept-constraint ind
                                                           concept-role-domain
                                                           new-constraint)))
                        (push domain-constraint added-constraints)
                        (push (cons concept-role-domain domain-constraint)
                              (gethash ind ind-role-domain-table)))))))))
           ((and (constraint-negated-p new-constraint)
                 (some-concept-p (constraint-term new-constraint)))
            (let* ((concept (constraint-concept new-constraint))
                   (role (concept-role concept)))
              (when (or (role-reflexive-p role)
                        (some #'user-defined-role-reflexive-p (role-descendants-internal role)))
                (push (concluded-concept-constraint (constraint-ind new-constraint)
                                                    (concept-term concept)
                                                    new-constraint)
                      added-constraints)))
            (push new-constraint new-unexpanded-deterministic-constraints))
           ((cd-concept-constraint-p new-constraint)
            (push new-constraint new-unexpanded-deterministic-constraints)
            (when expand-role-domain-p
              (let ((concept-attribute-domain
                     (concept-attribute-domain (constraint-term new-constraint))))
                (when concept-attribute-domain
                  (unless ind-role-domain-table
                    (setf ind-role-domain-table (racer-make-hash-table)))
                  (let* ((ind (constraint-ind new-constraint))
                         (old-constraint
                          (cdr (find concept-attribute-domain
                                     (gethash ind ind-role-domain-table)
                                     :key #'car))))
                    (if old-constraint
                      (when update-dependencies
                        (let ((domain-constraint
                               (concluded-concept-constraint ind
                                                             concept-attribute-domain
                                                             new-constraint)))
                          (setf (constraint-derived-from old-constraint)
                                (union (constraint-derived-from domain-constraint)
                                       (constraint-derived-from old-constraint)))))
                      (let ((domain-constraint
                             (concluded-concept-constraint ind
                                                           concept-attribute-domain
                                                           new-constraint)))
                        (push domain-constraint added-constraints)
                        (push (cons concept-attribute-domain domain-constraint)
                              (gethash ind ind-role-domain-table)))))))))
           (t (push new-constraint new-unexpanded-deterministic-constraints))))
    #+:debug
    (when (boundp '*debug2*)
      (assert (or (not *signature-merging*)
                  (eql (length (remove-if #'constraint-merging-trigger-p
                                          new-unexpanded-exists-constraints))
                       (length (racer-remove-concept-constraint-duplicates
                                (remove-if #'constraint-merging-trigger-p
                                           new-unexpanded-exists-constraints)))))))
    (values new-unexpanded-deterministic-constraints
            new-unexpanded-exists-constraints
            added-unexpanded-exists-constraints
            all-added-unexpanded-exists-constraints
            new-unexpanded-disjunctive-constraints
            added-unexpanded-disjunctive-constraints
            new-unexpanded-exists-constraints-store
            new-unexpanded-disjunctive-constraints-store
            added-constraints
            copy-disjunctive-p
            copy-exists-p
            ind-role-domain-table
            added-expanded-constraints
            unprocessed-constraints)))

(defun distribute-new-constraints-2-3 (state
                                        expanded-constraints
                                        potential-some-as-all-constraints
                                        other-conclusion-concept-constraints
                                        use-unexpanded-disjunctive-constraints-store
                                        new-relation-constraints
                                        copy-disjunctive-p
                                        copy-exists-p
                                        use-unexpanded-exists-constraints-store
                                        new-unexpanded-deterministic-constraints
                                        new-unexpanded-disjunctive-constraints
                                        added-unexpanded-disjunctive-constraints
                                        added-unexpanded-exists-constraints
                                        all-added-unexpanded-exists-constraints
                                        new-unexpanded-exists-constraints-store
                                        new-unexpanded-disjunctive-constraints-store)
  ;;; return 9 values: clashed-p, new-state,
  ;;; new-unexpanded-deterministic-constraints, added-unexpanded-exists-constraints,
  ;;; new-unexpanded-disjunctive-constraints, new-unexpanded-exists-constraints-store,
  ;;; new-unexpanded-disjunctive-constraints-store, copy-disjunctive-p, copy-exists-p
  (let ((new-state state)
        (unexpanded-exists-constraints (state-unexpanded-exists-constraints state)))
    (when (and use-unexpanded-exists-constraints-store 
               added-unexpanded-exists-constraints
               (swap-to-unexpanded-constraint-store-p added-unexpanded-exists-constraints
                                                      new-unexpanded-exists-constraints-store))
      (setf new-unexpanded-exists-constraints-store
            (add-to-constraint-store added-unexpanded-exists-constraints
                                     new-unexpanded-exists-constraints-store
                                     ':unexpanded-exists
                                     :copy-p copy-exists-p))
      (when copy-exists-p
        (setf copy-exists-p nil))
      (setf added-unexpanded-exists-constraints nil))
    (when (and use-unexpanded-exists-constraints-store 
               unexpanded-exists-constraints
               (swap-to-unexpanded-constraint-store-p unexpanded-exists-constraints
                                                      new-unexpanded-exists-constraints-store))
      (setf new-unexpanded-exists-constraints-store
            (add-to-constraint-store unexpanded-exists-constraints
                                     new-unexpanded-exists-constraints-store
                                     ':unexpanded-exists
                                     :copy-p copy-exists-p))
      (when copy-exists-p
        (setf copy-exists-p nil))
      (setf unexpanded-exists-constraints nil))
    (when (and use-unexpanded-disjunctive-constraints-store
               added-unexpanded-disjunctive-constraints
               (swap-to-unexpanded-constraint-store-p added-unexpanded-disjunctive-constraints
                                                      new-unexpanded-disjunctive-constraints-store))
      (setf new-unexpanded-disjunctive-constraints-store
            (add-to-constraint-store added-unexpanded-disjunctive-constraints
                                     new-unexpanded-disjunctive-constraints-store
                                     ':unexpanded-disjunctive
                                     :copy-p copy-disjunctive-p))
      (when copy-disjunctive-p
        (setf copy-disjunctive-p nil))
      (setf added-unexpanded-disjunctive-constraints nil)
      )
    (let ((all-removed-or-constraints nil))
      (when new-unexpanded-disjunctive-constraints
        (when (if (and *use-expanded-store*
                       (consp (state-expanded-store new-state)))
                (not (constraint-store-empty-p (state-expanded-store new-state)))
                expanded-constraints)
          (multiple-value-bind (clashed-p
                                added-or-constraints
                                new-added-unexpanded-disjunctive-constraints
                                new-det-or-constraints
                                new-store
                                removed-or-constraints)
                               (forward-checking-or-constraints
                                new-state
                                new-relation-constraints
                                new-unexpanded-disjunctive-constraints
                                added-unexpanded-disjunctive-constraints
                                new-unexpanded-disjunctive-constraints-store)
            (if clashed-p
              (progn
                (race-trace ("~&expanded constraints= ~S~%" expanded-constraints)
                            ("~&expanded constraints ~S clashed with new or-constraints ~S~%"
                             expanded-constraints new-unexpanded-disjunctive-constraints))
                (return-from distribute-new-constraints-2-3 t))
              (progn
                (when copy-disjunctive-p
                  (setf copy-disjunctive-p
                        (state-copy-unexpanded-disjunctive-constraints-store-p new-state)))
                (setf new-unexpanded-disjunctive-constraints-store new-store)
                (when (or added-or-constraints removed-or-constraints)
                  (setf new-unexpanded-disjunctive-constraints
                        (append added-or-constraints
                                (constraint-set-difference new-unexpanded-disjunctive-constraints
                                                           removed-or-constraints)))
                  (setf added-unexpanded-disjunctive-constraints
                        (append added-or-constraints new-added-unexpanded-disjunctive-constraints))
                  (setf all-removed-or-constraints (nconc removed-or-constraints
                                                          all-removed-or-constraints)))
                (when new-det-or-constraints
                  (setf new-unexpanded-deterministic-constraints
                        (append new-det-or-constraints
                                new-unexpanded-deterministic-constraints))
		  (if (> (* (length other-conclusion-concept-constraints)
			    (count-if #'or-constraint-p new-det-or-constraints))
			 1000)
		      (let ((table (make-constraints-table new-det-or-constraints)))
			(loop for constraint in other-conclusion-concept-constraints
			    collect (or (when (or-constraint-p constraint)
					  (let ((entry (constraint-found-p constraint table)))
					    (when (and entry (constraint-equal-test constraint entry))
					      entry)))
					constraint)
			    into result
			    finally
			      (setf other-conclusion-concept-constraints result)))
		    (loop for constraint in other-conclusion-concept-constraints
			collect (or (when (or-constraint-p constraint)
				      (find constraint new-det-or-constraints
					    :test #'constraint-equal-test))
				    constraint)
			into result
			finally
			  (setf other-conclusion-concept-constraints result))))))))
        (when (if (and *use-unexpanded-exists-constraints-store*
                       (consp (state-unexpanded-exists-constraints-store new-state)))
                (not (constraint-store-empty-p (state-unexpanded-exists-constraints-store new-state)))
                (state-unexpanded-exists-constraints new-state))
          (multiple-value-bind (clashed-p
                                added-or-constraints
                                new-added-unexpanded-disjunctive-constraints
                                new-det-or-constraints
                                new-store
                                removed-or-constraints)
                               (forward-checking-or-constraints
                                new-state
                                new-relation-constraints
                                new-unexpanded-disjunctive-constraints
                                added-unexpanded-disjunctive-constraints
                                new-unexpanded-disjunctive-constraints-store
                                nil
                                t)
            ;(when (or added-or-constraints new-added-unexpanded-disjunctive-constraints new-det-or-constraints) (princ "+"))
            (if clashed-p
              (progn
                (race-trace ("~&unexpanded exists constraints= ~S~%"
                             (state-unexpanded-exists-constraints new-state))
                            ("~&unexpanded exists constraints ~S clashed with new or-constraints ~S~%"
                             (state-unexpanded-exists-constraints new-state)
                             new-unexpanded-disjunctive-constraints))
                (return-from distribute-new-constraints-2-3 t))
              (progn
                (when copy-disjunctive-p
                  (setf copy-disjunctive-p
                        (state-copy-unexpanded-disjunctive-constraints-store-p new-state)))
                (setf new-unexpanded-disjunctive-constraints-store new-store)
                (when (or added-or-constraints removed-or-constraints)
                  (setf new-unexpanded-disjunctive-constraints
                        (append added-or-constraints
                                (constraint-set-difference new-unexpanded-disjunctive-constraints
                                                           removed-or-constraints)))
                  (setf added-unexpanded-disjunctive-constraints
                        (append added-or-constraints new-added-unexpanded-disjunctive-constraints))
                  (setf all-removed-or-constraints (nconc removed-or-constraints
                                                          all-removed-or-constraints)))
                (when new-det-or-constraints
                  (setf new-unexpanded-deterministic-constraints
                        (append new-det-or-constraints
                                new-unexpanded-deterministic-constraints))
		  (if (> (* (length other-conclusion-concept-constraints)
			    (count-if #'or-constraint-p new-det-or-constraints))
			 1000)
		      (let ((table (make-constraints-table new-det-or-constraints)))
			(loop for constraint in other-conclusion-concept-constraints
			    collect (or (when (or-constraint-p constraint)
					  (let ((entry (constraint-found-p constraint table)))
					    (when (and entry (constraint-equal-test constraint entry))
					      entry)))
					constraint)
			    into result
			    finally
			      (setf other-conclusion-concept-constraints result)))
		    (loop for constraint in other-conclusion-concept-constraints
			collect (or (when (or-constraint-p constraint)
				      (find constraint new-det-or-constraints
					    :test #'constraint-equal-test))
				    constraint)
			into result
			finally
			  (setf other-conclusion-concept-constraints result)))))))))
      (when (and all-added-unexpanded-exists-constraints
                 (if (and use-unexpanded-disjunctive-constraints-store
                          new-unexpanded-disjunctive-constraints-store
                          (not (constraint-store-unused-p new-unexpanded-disjunctive-constraints-store)))
                   (or (state-unexpanded-disjunctive-constraints new-state)
                       (not (constraint-store-empty-p new-unexpanded-disjunctive-constraints-store)))
                   (state-unexpanded-disjunctive-constraints new-state)))
        (multiple-value-bind
          (clashed-p
           added-or-constraints
           new-added-unexpanded-disjunctive-constraints
           new-det-or-constraints
           new-store
           removed-or-constraints)
          (forward-checking-or-constraints
           new-state
           new-relation-constraints
           (if (and use-unexpanded-disjunctive-constraints-store
                    (consp new-unexpanded-disjunctive-constraints-store))
             (append added-unexpanded-disjunctive-constraints
                     (state-unexpanded-disjunctive-constraints new-state))
             (append new-unexpanded-disjunctive-constraints
                     (state-unexpanded-disjunctive-constraints new-state)))
           added-unexpanded-disjunctive-constraints
           new-unexpanded-disjunctive-constraints-store
           all-added-unexpanded-exists-constraints)
          (if clashed-p
            (progn
              (race-trace ("~&unexpanded exists constraints= ~S~%"
                           all-added-unexpanded-exists-constraints)
                          ("~&unexpanded exists constraints clashed with new or-constraints ~S"
                           new-unexpanded-disjunctive-constraints))
              ;(break)
              (return-from distribute-new-constraints-2-3 t))
            (progn
              (when copy-disjunctive-p
                (setf copy-disjunctive-p
                      (state-copy-unexpanded-disjunctive-constraints-store-p new-state)))
              (setf new-unexpanded-disjunctive-constraints-store new-store)
              (when (or added-or-constraints removed-or-constraints)
                (setf new-unexpanded-disjunctive-constraints
                      (append added-or-constraints
                              (constraint-set-difference new-unexpanded-disjunctive-constraints
                                                         removed-or-constraints)))
                (setf added-unexpanded-disjunctive-constraints
                      (append added-or-constraints new-added-unexpanded-disjunctive-constraints))
                (setf all-removed-or-constraints (nconc removed-or-constraints
                                                        all-removed-or-constraints)))
              (when new-det-or-constraints
                (setf new-unexpanded-deterministic-constraints
                      (append new-det-or-constraints
                              new-unexpanded-deterministic-constraints)))
              (when (and use-unexpanded-disjunctive-constraints-store
                         added-unexpanded-disjunctive-constraints
                         (swap-to-unexpanded-constraint-store-p new-unexpanded-disjunctive-constraints
                                                                new-unexpanded-disjunctive-constraints-store))
                (setf new-unexpanded-disjunctive-constraints-store
                      (add-to-constraint-store added-unexpanded-disjunctive-constraints
                                               new-unexpanded-disjunctive-constraints-store
                                               ':unexpanded-disjunctive
                                               :copy-p copy-disjunctive-p)))))))
      (let ((other-conclusion-concept-constraints
             (constraint-set-difference other-conclusion-concept-constraints
                                        all-added-unexpanded-exists-constraints)))
        (when (and (or other-conclusion-concept-constraints
                       potential-some-as-all-constraints
                       )
                   (if (and use-unexpanded-disjunctive-constraints-store
                            new-unexpanded-disjunctive-constraints-store
                            (not (constraint-store-unused-p new-unexpanded-disjunctive-constraints-store)))
                     (or (state-unexpanded-disjunctive-constraints new-state)
                         (not (constraint-store-empty-p new-unexpanded-disjunctive-constraints-store)))
                     (state-unexpanded-disjunctive-constraints new-state)))
          (multiple-value-bind
            (clashed-p
             added-or-constraints
             new-added-unexpanded-disjunctive-constraints
             new-det-or-constraints
             new-store
             removed-or-constraints)
            (forward-checking-or-constraints
             new-state
             new-relation-constraints
             (if (and use-unexpanded-disjunctive-constraints-store
                      (consp new-unexpanded-disjunctive-constraints-store))
               (append added-unexpanded-disjunctive-constraints
                       (state-unexpanded-disjunctive-constraints new-state))
               (append new-unexpanded-disjunctive-constraints
                       (state-unexpanded-disjunctive-constraints new-state)))
             added-unexpanded-disjunctive-constraints
             new-unexpanded-disjunctive-constraints-store
             (if potential-some-as-all-constraints
                 (constraint-set-union potential-some-as-all-constraints other-conclusion-concept-constraints)
               other-conclusion-concept-constraints))
            (if clashed-p
              (progn
                (race-trace ("~&conclusion constraints= ~S~%" other-conclusion-concept-constraints)
                            ("~&conclusion constraints clashed with new or-constraints ~S"
                             new-unexpanded-disjunctive-constraints))
                ;(break)
                (return-from distribute-new-constraints-2-3 t))
              (progn
                (when copy-disjunctive-p
                  (setf copy-disjunctive-p
                        (state-copy-unexpanded-disjunctive-constraints-store-p new-state)))
                (setf new-unexpanded-disjunctive-constraints-store new-store)
                (when (or added-or-constraints removed-or-constraints)
                  (setf new-unexpanded-disjunctive-constraints
                        (append added-or-constraints
                                (constraint-set-difference new-unexpanded-disjunctive-constraints
                                                           removed-or-constraints)))
                  (setf added-unexpanded-disjunctive-constraints
                        (append added-or-constraints new-added-unexpanded-disjunctive-constraints))
                  (setf all-removed-or-constraints (nconc removed-or-constraints
                                                          all-removed-or-constraints)))
                (when new-det-or-constraints
                  (setf new-unexpanded-deterministic-constraints
                        (append new-det-or-constraints
                                new-unexpanded-deterministic-constraints)))
                (when (and use-unexpanded-disjunctive-constraints-store
                           added-unexpanded-disjunctive-constraints
                           (swap-to-unexpanded-constraint-store-p new-unexpanded-disjunctive-constraints
                                                                  new-unexpanded-disjunctive-constraints-store))
                  (setf new-unexpanded-disjunctive-constraints-store
                        (add-to-constraint-store added-unexpanded-disjunctive-constraints
                                                 new-unexpanded-disjunctive-constraints-store
                                                 ':unexpanded-disjunctive
                                                 :copy-p copy-disjunctive-p))))))))
      (let ((all-unexpanded-disjunctive-constraints
             (nconc added-unexpanded-disjunctive-constraints
                    (constraint-set-difference (state-unexpanded-disjunctive-constraints new-state)
                                               all-removed-or-constraints))))
        (when (and use-unexpanded-disjunctive-constraints-store
                   all-unexpanded-disjunctive-constraints
                   (swap-to-unexpanded-constraint-store-p all-unexpanded-disjunctive-constraints
                                                          new-unexpanded-disjunctive-constraints-store))
          (setf new-unexpanded-disjunctive-constraints-store
                (add-to-constraint-store all-unexpanded-disjunctive-constraints
                                         new-unexpanded-disjunctive-constraints-store
                                         ':unexpanded-disjunctive
                                         :copy-p copy-disjunctive-p))
          (setf all-unexpanded-disjunctive-constraints nil))
        (values nil
                (changed-kernel-state new-state
                                      :unexpanded-exists-constraints unexpanded-exists-constraints)
                new-unexpanded-deterministic-constraints
                added-unexpanded-exists-constraints
                all-unexpanded-disjunctive-constraints
                new-unexpanded-exists-constraints-store
                new-unexpanded-disjunctive-constraints-store
                copy-disjunctive-p
                copy-exists-p)))))

;;;===========================================================================

(defun compute-constraint-or-dependencies (precondition-concept-constraint)
  "The or-dependencies of a constraint are either the precondition constraint 
   itself in case of an or-constraint or the or-dependencies of the precondition 
   constraint."
  (if (dependency-constraint-p precondition-concept-constraint)
    (list precondition-concept-constraint)
    (constraint-or-dependencies precondition-concept-constraint)))

(defun concluded-concept-constraint (new-ind 
                                          concept
                                          precondition-concept-constraint
                                          &optional
                                          (other-precondition-constraint nil))
  "Creates a new concept constraint concluded from the precondition constraint 
   and optionally the precondition relation constraint. Especially takes care of 
   the (or) dependencies."
  (let ((new-constraint (new-concept-constraint new-ind concept))
        (inverse-roles *inverse-roles*)
        (other-precondition-constraints (if (listp other-precondition-constraint)
                                          other-precondition-constraint
                                          (list other-precondition-constraint))))
    (when precondition-concept-constraint
      (setf (constraint-dependencies new-constraint)
            (list precondition-concept-constraint))
      (setf (constraint-or-dependencies new-constraint)
            (compute-constraint-or-dependencies precondition-concept-constraint))
      (when (and *model-merging* (or *use-alternate-models* *use-alternate-ind-models*))
        (setf (constraint-derived-from new-constraint)
              (or (constraint-derived-from precondition-concept-constraint)
                  (list precondition-concept-constraint))))
      (if (concept-constraint-p precondition-concept-constraint)
          (progn
            (setf (constraint-backpropagated-p new-constraint)
                  (constraint-backpropagated-p precondition-concept-constraint))
            (setf (constraint-ignore-determinism-p new-constraint)
                  (constraint-ignore-determinism-p precondition-concept-constraint)))
        (when (and inverse-roles
                   (not (true-old-individual-p new-ind))
                   (loop for other-precondition-constraint in other-precondition-constraints
                         thereis
                         (older-individual-p (constraint-ind-2 other-precondition-constraint)
                                             (constraint-ind-1 other-precondition-constraint))))
          (setf (constraint-backpropagated-p new-constraint) t))))
    #+:debug (assert (or precondition-concept-constraint (null other-precondition-constraint)))
    (when other-precondition-constraint
      (loop for other-precondition-constraint in other-precondition-constraints
            do
            (pushnew other-precondition-constraint (constraint-dependencies new-constraint))
            (setf (constraint-or-dependencies new-constraint)
                  (union-dependencies (compute-constraint-or-dependencies
                                       other-precondition-constraint)
                                      (constraint-or-dependencies new-constraint)))
            (when (and *model-merging* (or *use-alternate-models* *use-alternate-ind-models*))
              (pushnew other-precondition-constraint (constraint-derived-from new-constraint)))
            (if (concept-constraint-p other-precondition-constraint)
                (progn
                  (setf (constraint-backpropagated-p new-constraint)
                        (constraint-backpropagated-p other-precondition-constraint))
                  (setf (constraint-ignore-determinism-p new-constraint)
                        (constraint-ignore-determinism-p other-precondition-constraint)))
              (when (and inverse-roles
                         (not (constraint-backpropagated-p new-constraint))
                         (not (true-old-individual-p new-ind))
                         (older-individual-p (constraint-ind-2 other-precondition-constraint)
                                             (constraint-ind-1 other-precondition-constraint)))
                ;(break "~S" other-precondition-constraint)
                (setf (constraint-backpropagated-p new-constraint) t)))))
    #+:debug
    (when (and *model-merging* (or *use-alternate-models* *use-alternate-ind-models*))
      (assert (constraint-derived-from new-constraint)))
    ;(break "~S" new-constraint)
    new-constraint))

(defun concluded-relation-constraint (ind
                                           new-ind
                                           precondition-constraint
                                           &optional (inverse-role nil))
  "Creates a new relation constraint concluded from the precondition constraint. 
   Especially takes care of the (or) dependencies."
  (let ((new-constraint
         (new-relation-constraint ind
                                  new-ind
                                  (or inverse-role
                                      (concept-role (constraint-term precondition-constraint))))))
    (setf (constraint-dependencies new-constraint) (list precondition-constraint))
    (setf (constraint-or-dependencies new-constraint)
          (compute-constraint-or-dependencies precondition-constraint))
    (when (and *model-merging* (or *use-alternate-models* *use-alternate-ind-models*))
      (setf (constraint-derived-from new-constraint)
            (or (constraint-derived-from precondition-constraint)
                (list precondition-constraint))))
    #+:debug (when (and *model-merging* (or *use-alternate-models* *use-alternate-ind-models*))
               (assert (constraint-derived-from new-constraint)))
    new-constraint))

(defun compute-feature-hierarchy-role-conjunct (feature-constraints)
  (let ((role-conjuncts (role-set-remove-duplicates
                          (loop for constraint in feature-constraints
                                collect
                                (concept-role (constraint-term constraint))))))
    (if (rest role-conjuncts)
        (make-nary-and-role role-conjuncts)
      (first role-conjuncts))))

(defun concluded-feature-partition-inverse-relation-constraint (ind
                                                                new-ind
                                                                role
                                                                precondition-constraints)
  (let ((new-constraint
         (new-relation-constraint ind new-ind (role-inverse-internal role))))
    (setf (constraint-dependencies new-constraint) precondition-constraints)
    (loop for constraint in precondition-constraints
          do
          (pushnew constraint (constraint-dependencies new-constraint))
          (setf (constraint-or-dependencies new-constraint)
                (union-dependencies (compute-constraint-or-dependencies constraint)
                                    (constraint-or-dependencies new-constraint)))
          (when (and *model-merging* (or *use-alternate-models* *use-alternate-ind-models*))
            (pushnew constraint (constraint-derived-from new-constraint))))
    #+:debug
    (when (and *model-merging* (or *use-alternate-models* *use-alternate-ind-models*))
      (assert (constraint-derived-from new-constraint)))
    new-constraint))

(defun synthesized-concept-constraint (new-ind 
                                       concept
                                       signature)
  (let ((new-constraint (new-concept-constraint new-ind concept)))
    (setf (constraint-signature new-constraint) signature)
    (when *use-refined-signature-clash-dependencies*
      (let ((concept-dependencies (assoc (concept-term concept)
                                         (signature-concept-dependencies signature))))
        (when concept-dependencies
          (let ((old-constraint (signature-generating-exists-constraint signature)))
            (when old-constraint
              (setf (cdr concept-dependencies)
                    (substitute new-constraint old-constraint (cdr concept-dependencies))))))))
    (setf (constraint-dependencies new-constraint) (list new-constraint))
    (setf (constraint-or-dependencies new-constraint) (list new-constraint))
    (setf (constraint-or-level new-constraint) (signature-or-level signature))
    (when (and *inverse-roles*
               (not (true-old-individual-p new-ind))
               (loop for dependency in (signature-dependencies signature)
                     thereis (and (concept-constraint-p dependency)
                                  (constraint-backpropagated-p dependency))))
      (setf (constraint-backpropagated-p new-constraint) t))
    (when (and *model-merging* (or *use-alternate-models* *use-alternate-ind-models*))
      (setf (constraint-derived-from new-constraint)
            (loop for dependency in (signature-dependencies signature)
                  with result = nil do
                  (setf result (union (constraint-derived-from dependency) result))
                  finally (return result)))
      #+:debug (assert (constraint-derived-from new-constraint)))
    new-constraint))

(defun synthesized-relation-constraint (ind
                                             new-ind
                                             role
                                             signature)
  (let ((new-constraint (new-relation-constraint ind new-ind role)))
    (setf (constraint-signature new-constraint) signature)
    (setf (constraint-dependencies new-constraint) (list new-constraint))
    (setf (constraint-or-dependencies new-constraint) (list new-constraint))
    (when (and *model-merging* (or *use-alternate-models* *use-alternate-ind-models*))
      (setf (constraint-derived-from new-constraint)
            (loop for dependency in (signature-dependencies signature)
                  with result = nil do
                  (setf result (union (constraint-derived-from dependency) result))
                  finally (return result)))
      #+:debug (assert (constraint-derived-from new-constraint)))
    new-constraint))

(defun most-recent-dependencies-1 (constraint)
  "Compute the maximal or level of all constraint dependencies"
  (or (constraint-dependency-key constraint)
      (let ((dependencies (constraint-or-dependencies constraint)))
        (if (null dependencies)
          (setf (constraint-dependency-key constraint) 0)
          (loop with first-dependency = (first dependencies)
                with max-or-level = (if (constraint-common-p first-dependency)
                                        (constraint-or-level first-dependency)
                                      (progn
                                        #+:debug
                                        (assert (qualified-role-signature-p first-dependency))
                                        (signature-or-level first-dependency)))
                for dependency in (rest (constraint-or-dependencies constraint))
                for or-level = (if (constraint-common-p dependency)
                                   (constraint-or-level dependency)
                                 (progn
                                   #+:debug
                                   (assert (qualified-role-signature-p dependency))
                                   (constraint-or-level dependency)))
                when (< max-or-level or-level)
                do (setf max-or-level or-level)
                finally (let ((value (+ (* 100 (1+ max-or-level))
                                        (length dependencies))))
                          (setf (constraint-dependency-key constraint) value)
                          (return value)))))))

(defun most-recent-dependencies-2 (constraint)
  "Compute the maximal or level of all constraint dependencies"
  (let ((key (constraint-dependency-key constraint)))
    (if key
        (progn
          (+ key (* 10000000000 (1+ (constraint-or-level constraint)))))
      (let ((dependencies (constraint-or-dependencies constraint)))
        (let* ((max-or-level
                (loop for dependency in dependencies
                      maximize
                      (if (constraint-common-p dependency)
                          (constraint-or-level dependency)
                        (progn
                          #+:debug (assert (qualified-role-signature-p dependency))
                          (signature-or-level dependency)))))
               (value (+ (* 10000000 (1+ max-or-level))
                         (* 100000 (length dependencies))
                         #|(concept-age (constraint-term constraint))|#)))
          (setf (constraint-dependency-key constraint) value)
          (+ value (* 10000000000 (1+ (constraint-or-level constraint)))))))))

(defun most-recent-dependencies (constraint)
  (if *use-unique-name-assumption*
    (most-recent-dependencies-1 constraint)
    (most-recent-dependencies-2 constraint)))


#|
(defun most-recent-dependencies-1 (constraint)
  "Compute the maximal or level of all constraint dependencies"
  (or (constraint-dependency-key constraint)
      (let ((dependencies (constraint-or-dependencies constraint)))
        (if (null dependencies)
          (setf (constraint-dependency-key constraint) 0)
          (loop with max-or-level = (constraint-or-level (first dependencies))
                for dependency in (rest (constraint-or-dependencies constraint))
                for or-level = (constraint-or-level dependency)
                when (< max-or-level or-level)
                do (setf max-or-level or-level)
                finally (let ((value (+ (* 10000 (1+ (constraint-or-level constraint)))
                                        (* 100 (1+ max-or-level))
                                        (length dependencies))))
                          (setf (constraint-dependency-key constraint) value)
                          (return value)))))))

(defun most-recent-dependencies (constraint)
  "Compute the maximal or level of all constraint dependencies"
  (let ((key (constraint-dependency-key constraint)))
    (if key
      (+ key (* 10000000 (1+ (constraint-or-level constraint))))
      (let ((dependencies (constraint-or-dependencies constraint)))
        (let* ((max-or-level
                (loop for dependency in dependencies
                      maximize (constraint-or-level dependency)))
               (value (+ (* 100000 (1+ max-or-level))
                         (* 100 (concept-age (constraint-term constraint)))
                         (length dependencies))))
          (setf (constraint-dependency-key constraint) value)
          (+ value (* 10000000 (1+ (constraint-or-level constraint)))))))))

(defun most-recent-dependencies (constraint)
  "Compute the maximal or level of all constraint dependencies"
  #+:debug (assert (or-constraint-p constraint))
  (let ((value (constraint-dependency-key constraint))
        (open (constraint-open-clauses-counter constraint)))
    (if value
      (+ value open)
      (let ((dependencies (constraint-or-dependencies constraint)))
        (if (null dependencies)
          (progn
            (setf (constraint-dependency-key constraint) 0)
            open)
          (loop with max-or-level = (constraint-or-level (first dependencies))
                for dependency in (rest (constraint-or-dependencies constraint))
                for or-level = (constraint-or-level dependency)
                when (< max-or-level or-level)
                do (setf max-or-level or-level)
                finally (let ((value (+ (* 1000 (1+ max-or-level))
                                        (* 100 (length dependencies))
                                        open)))
                          (setf (constraint-dependency-key constraint) value)
                          (return (+ value open)))))))))
|#

;;; ======================================================================

(defun expand-and-constraint (and-constraint)
  "Create a list of new constraints concluded from the and-constraint"
  (let ((ind (constraint-ind and-constraint)))
    (mapcar #'(lambda (concept)
                (concluded-concept-constraint ind concept and-constraint))
            (concept-term (constraint-term and-constraint)))))

(defun expanded-and-satisfiable (and-constraint state unused-1 unused-2 unused-3)
  "Check whether the constraints concluded the and-constraint are satisfiable"
  (declare (ignore unused-1 unused-2 unused-3))
  (added-constraints-satisfiable (expand-and-constraint and-constraint)
                                 nil ; no new relation constraint
                                 state
                                 t nil))

;;; ======================================================================

(defun get-max-or-clauses-constraint (ind state)
  "Compute the maximal length of or-clauses"
  (let ((or-constraints (state-unexpanded-disjunctive-constraints state))
        (store (state-unexpanded-disjunctive-constraints-store state))
        (max-or-list-threshold *max-or-list-threshold*))
    (ind-maximize-constraints (or-constraint)
                              ind
                              (constraint-or-clauses-counter or-constraint)
                              or-constraints
                              store
                              max-or-list-threshold)))
#+:debug
(defun shift-left (i dist)
  (dpb i (byte 16 dist) 0))

#-:debug
(defmacro shift-left (i dist)
  `(dpb ,i (byte 16 ,dist) 0))

#+:debug
(defun alpha (pos-p neg-p)
  (+ (shift-left (* pos-p neg-p) 16) pos-p neg-p))

#-:debug
(defmacro alpha (pos-p neg-p)
  `(+ (shift-left (* ,pos-p ,neg-p) 16) ,pos-p ,neg-p))

#+:debug
(defun jw-weight (clause-len max-len)
  (shift-left 1 (- max-len clause-len)))

#-:debug
(defmacro jw-weight (clause-len max-len)
  `(shift-left 1 (- ,max-len ,clause-len)))

(defmacro compute-weight (jw-weight
                            or-constraint
                            concept
                            negated-concept
                            pos-weight
                            neg-weight)
  `(loop with pos-count = 0
         with neg-count = 0
         for clause in (concept-term (concept-negated-concept (constraint-term ,or-constraint)))
         do
         (if (eq clause ,concept)
           (incf pos-count)
           (when (eq clause ,negated-concept)
             (incf neg-count)))
         finally
         (when (plusp pos-count)
           (incf ,pos-weight (* pos-count ,jw-weight)))
         (when (plusp neg-count)
           (incf ,neg-weight (* neg-count ,jw-weight)))))

(defun compute-weights (ind concept max-length state)
  "Compute the JW weigths corresponding to unnegated and negated occurences 
   of concept in or-clauses of open or-constraints. Return two values: 
   weight for unnegated occurences, weight for negated occurences."
  (let ((pos-weight 0)
        (neg-weight 0)
        (negated-concept (concept-negated-concept concept))
        (count 0)
        (max-or-list-threshold *max-or-list-threshold*)
        (or-constraints (state-unexpanded-disjunctive-constraints state)))
    (when or-constraints
      (loop for or-constraint in (state-unexpanded-disjunctive-constraints state)
            when (and (eql ind (constraint-ind or-constraint))
                      (not (deterministic-or-constraint-p 
                            (constraint-open-clauses-counter or-constraint))))
            do
            (incf count)
            (let* ((length (constraint-or-clauses-counter or-constraint))
                   (jw-weight (jw-weight length max-length)))
              #+:debug
              (when (> length max-length)
                (error "inconsistency with maximal or-length found"))
              (compute-weight jw-weight or-constraint concept negated-concept pos-weight neg-weight))
            until (> count max-or-list-threshold)))
    (if (> count max-or-list-threshold)
      (values pos-weight neg-weight)
      (let ((store (state-unexpanded-disjunctive-constraints-store state)))
        (when (and *use-unexpanded-disjunctive-constraints-store*
                   store
                   (not (constraint-store-unused-p store)))
          (loop-over-ind-expanded-constraints (or-constraint ind store)
            do
            (incf count)
            (let* ((length (constraint-or-clauses-counter or-constraint))
                   (jw-weight (jw-weight length max-length)))
              #+:debug
              (when (> length max-length)
                (error "inconsistency with maximal or-length found"))
              (compute-weight jw-weight or-constraint concept negated-concept pos-weight neg-weight))
            until (> count max-or-list-threshold)))
        (values pos-weight neg-weight)))))

(defun select-or-clause-candidate (or-constraint state)
  "Select from or-constraint an or-clause candidate for further expansion. 
The clause with the maximal priority in accordance to JW weights is selected.
Return 3 values: selected clause, positive weight, negative weight."
  (if *jw-selection*
    (let ((max-priority -1)
          (selected-clause-term nil)
          (clause-pos-weight nil)
          (clause-neg-weight nil))
      (loop with ind = (constraint-ind or-constraint)
            with max-length = (get-max-or-clauses-constraint ind state)
            with no-tbox = (null *use-tbox*)
            for clause-term in (concept-term (concept-negated-concept (constraint-term
                                                                       or-constraint)))
            for index from 0
            when (open-or-clause-p or-constraint index) do
            (multiple-value-bind (pos-weight neg-weight)
                                 (compute-weights ind clause-term max-length state)
              (let ((priority (alpha pos-weight neg-weight)))
                (if (> priority max-priority)
                  (progn
                    (setf max-priority priority)
                    (setf selected-clause-term clause-term)
                    (setf clause-pos-weight pos-weight)
                    (setf clause-neg-weight neg-weight))
                  (when (and (eql priority max-priority)
                             (atomic-concept-p clause-term)
                             (or no-tbox
                                 (let ((definition
                                         (concept-encoded-definition clause-term)))
                                   (or (null definition)
                                       (atomic-concept-p definition)
                                       (negated-concept-p definition)))))
                    (setf selected-clause-term clause-term)
                    (setf clause-pos-weight pos-weight)
                    (setf clause-neg-weight neg-weight))))))
      (values selected-clause-term clause-pos-weight clause-neg-weight))
    (values (loop for clause-term in (concept-term (concept-negated-concept (constraint-term
                                                                             or-constraint)))
                  for index from 0
                  when (open-or-clause-p or-constraint index) do
                  (return clause-term))
            1 0)))

(defun expand-bcp-or-constraint-clause (or-constraint)
  "Create a new constraint from the only open or-clause of or-constraint. 
   The or-dependencies are set to the union of the dependencies from the other 
   clashed or-clauses and the dependencies of the or-constraint itself."
  (let*
    ((selected-clause
      (loop for clause-term in (concept-term (concept-negated-concept (constraint-term
                                                                       or-constraint)))
            for index = 0 then (1+ index)
            when (open-or-clause-p or-constraint index)
            do (return clause-term)
            finally (error "no open clause found in bcp expansion of ~S"
                           or-constraint)))
     (new-constraint (concluded-concept-constraint (constraint-ind or-constraint)
                                                   selected-clause or-constraint)))
    (setf (constraint-or-dependencies new-constraint)
          (union-dependencies (constraint-other-or-clauses-dependencies or-constraint)
                              (constraint-or-dependencies or-constraint)))
    new-constraint))


#+:debug
(defun get-disjunct-table-count (concept table)
  (gethash concept (select-disjunct-table table) 0))

#-:debug
(defmacro get-disjunct-table-count (concept table)
  `(gethash ,concept (select-disjunct-table ,table) 0))

#+:debug
(defun hits-priority (pos-hits neg-hits)
  (+ pos-hits neg-hits))

#-:debug
(defmacro hits-priority (pos-hits neg-hits)
  `(+ ,pos-hits ,neg-hits))

(defun expand-or-constraint-clause (or-constraint state &optional (success-only nil))
  (let* ((table *select-disjunct-table*)
         (use-succ
          (and *use-success-based-disjunct-selection*
               (or *always-use-success-based-disjunct-selection*
                   ;(>= (select-disjunct-last-sum-rate table) -0.3)
                   (> (select-disjunct-total-sum table) -200)
                   ))))
    (if use-succ
        (let ((selected-clause (select-new-or-clause-candidate or-constraint state success-only)))
          ;(princ "+")
          (concluded-concept-constraint (constraint-ind or-constraint)
                                        selected-clause
                                        or-constraint))
      (multiple-value-bind (selected-clause clause-pos-weight clause-neg-weight)
          (select-or-clause-candidate or-constraint state)
        ;(princ "-")
        (concluded-concept-constraint (constraint-ind or-constraint)
                                      (if (> clause-pos-weight clause-neg-weight)
                                          selected-clause
                                        (concept-negated-concept selected-clause))
                                      or-constraint)))))

(defun select-new-or-clause-candidate (or-constraint state success-only)
  "Return 5 values: selected clause, positive weight, negative weight, number of positive hits, number of negative hits."
  (if *jw-selection*
      (loop with max-priority = -1
            with selected-clause-term = nil
            with max-pos-weight = nil
            with max-neg-weight = nil
            with max-pos-hits = -1
            with max-neg-hits = -1
            with max-hits-priority = 0
            with ind = (constraint-ind or-constraint)
            with max-length = (unless success-only
                                (get-max-or-clauses-constraint ind state))
            with select-disjunct-table = *select-disjunct-table*
            with use-success-based-disjunct-selection = *use-success-based-disjunct-selection*
            for clause-term in (concept-term (concept-negated-concept (constraint-term or-constraint)))
            for index from 0
            when (open-or-clause-p or-constraint index) do
            (multiple-value-bind (pos-weight neg-weight)
                (if success-only
                    (values 0 0)
                  (compute-weights ind clause-term max-length state))
              (let* ((priority (unless success-only
                                 (alpha pos-weight neg-weight)))
                     (pos-hits (if use-success-based-disjunct-selection
                                   (get-disjunct-table-count clause-term select-disjunct-table)
                                 0))
                     (neg-hits (if use-success-based-disjunct-selection
                                   (get-disjunct-table-count (concept-negated-concept clause-term)
                                                             select-disjunct-table)
                                 0))
                     (hits-priority (if use-success-based-disjunct-selection
                                        (hits-priority pos-hits neg-hits)
                                      0))
                     (increased-priority (and (not success-only) (> priority max-priority)))
                     (increased-hits-priority (> hits-priority max-hits-priority -1))
                     (equal-priority (and (not success-only) (eql priority max-priority)))
                     (no-success-info (eql max-hits-priority 0))
                     (select-p 
                      (if success-only
                          (or (null selected-clause-term) increased-hits-priority)
                        (or (and increased-priority increased-hits-priority)
			    (and increased-priority
                                 (or no-success-info
                                     ;(> priority 1000)
                                     (>= hits-priority max-hits-priority 0)
                                     (< max-hits-priority -10)
				     ))
                            (and increased-hits-priority
                                 (or equal-priority (<= max-priority 1)))
                            (and equal-priority
                                 (or (>= hits-priority max-hits-priority)
                                     (< max-hits-priority -10)
				     )
                                 (atomic-concept-p clause-term)
				 (let ((definition
                                        (concept-encoded-definition clause-term)))
                                   (or (null definition)
                                       (atomic-concept-p definition)
                                       (negated-concept-p definition))))))))
                (when select-p
                  (unless success-only
                    (setf max-priority priority)
                    (setf max-pos-weight pos-weight)
                    (setf max-neg-weight neg-weight))
                  (setf selected-clause-term clause-term)
                  (setf max-pos-hits pos-hits)
                  (setf max-neg-hits neg-hits)
                  (setf max-hits-priority hits-priority))))
            finally
            (if success-only
                (if (> max-pos-hits max-neg-hits)
                    (return selected-clause-term)
                  (return (concept-negated-concept selected-clause-term)))
              (let ((pos-success (> max-pos-hits max-neg-hits))
                    (pos-weights (> max-pos-weight max-neg-weight)))
                (if pos-success
                    (if pos-weights
                        (return selected-clause-term)
                      (if (or (eql max-neg-weight max-pos-weight)
                              (<= max-pos-weight max-neg-weight 1)
                              (<= (- max-neg-weight max-pos-weight) 1)
                              (>= (- max-pos-hits max-neg-hits) 5))
                          (return (concept-negated-concept selected-clause-term))
                        (return selected-clause-term)))
                  (if pos-weights
                      (if (or #|(= max-pos-hits max-neg-hits 0)
                                 (eql max-pos-hits max-neg-hits)|#
                              (<= (- max-neg-hits max-pos-hits) 50))
                          (return selected-clause-term)
                        (return (concept-negated-concept selected-clause-term)))
                    (let ((neg-success (< max-pos-hits max-neg-hits))
                          (neg-weights (< max-pos-weight max-neg-weight)))
                      (if (not neg-success)
                          (if (not neg-weights)
                              (return (concept-negated-concept selected-clause-term))
                            (if t #|(or (= max-pos-hits max-neg-hits 0)
                                        (eql max-pos-hits max-neg-hits)
                                        (>= (- max-neg-weight max-pos-weight) 100)
                                        (<= (- max-pos-hits max-neg-hits) 5))|#
                              (return (concept-negated-concept selected-clause-term))
                              (return selected-clause-term)))
                        (if (not neg-weights)
                            (if (or (eql max-neg-weight max-pos-weight)
                                    (<= max-neg-weight max-pos-weight 1)
                                    (<= (- max-pos-weight max-neg-weight) 1)
                                    (>= (- max-neg-hits max-pos-hits) 5))
                                (return (concept-negated-concept selected-clause-term))
                              (return selected-clause-term))
                          (return (concept-negated-concept selected-clause-term))))))))))
    (loop for clause-term in (concept-term (concept-negated-concept (constraint-term or-constraint)))
          for index from 0
          when (open-or-clause-p or-constraint index) do
          (return clause-term))))

#+:debug
(defun incf-select-disjunct-table-entry (constraint &optional (increment 1))
  (when *use-success-based-disjunct-selection*
    (increment-disjunct-table-entry (constraint-concept constraint)
                                    *select-disjunct-table*
                                    increment)))

#-:debug
(defmacro incf-select-disjunct-table-entry (constraint &optional (increment 1))
  `(when *use-success-based-disjunct-selection*
     (increment-disjunct-table-entry (constraint-concept ,constraint)
                                     *select-disjunct-table*
                                     ,increment)))

#+:debug
(defun decf-select-disjunct-table-entry (constraint &optional (decrement 1))
  (when *use-success-based-disjunct-selection*
    (decrement-disjunct-table-entry (constraint-concept constraint)
                                    *select-disjunct-table*
                                    decrement)))

#-:debug
(defmacro decf-select-disjunct-table-entry (constraint &optional (decrement 1))
  `(when *use-success-based-disjunct-selection*
     (decrement-disjunct-table-entry (constraint-concept ,constraint)
                                     *select-disjunct-table*
                                     ,decrement)))

(defun fully-expanded-or-satisfiable (or-constraint state unused-1 unused-2 unused-3)
  "Checks whether a fully expanded or-constraint (i.e. an or-constraint with 
   a true or-clause) is satisfiable"
  (declare (ignore unused-1 unused-2 unused-3 
		   #-:debug or-constraint))
  #+:debug
  (unless (or-constraint-satisfied-p (constraint-open-clauses-counter or-constraint))
    (error "Internal confusion with or expansion of ~S" or-constraint))
  (race-trace ("~&one disjunct in ~S satisfied~%" or-constraint))
  (added-constraints-satisfiable nil ; no new concept conclusions
                                 nil ; no new relation constraint
                                 state t nil))

(defun expanded-bcp-or-satisfiable (or-constraint state unused-1 unused-2 unused-3)
  "Checks whether an expanded bcp-or-constraint (i.e. an or-constraint with 
   only one open or-clause) is satisfiable by establishing the concluded 
   constraint as unnegated."
  (declare (ignore unused-1 unused-2 unused-3))
  (let ((new-constraint (expand-bcp-or-constraint-clause or-constraint)))
    (added-constraints-satisfiable new-constraint
                                   nil ; no new relation constraint
                                   state t nil)))

(defun partially-expanded-or-satisfiable (or-constraint state unused-1 unused-2 unused-3)
  "Checks whether a partially expanded or-constraint (i.e. an or-constraint with 
   at least 2 open or-clauses) is satisfiable by establishing one of the 
   remaining or-clause alternatives. Implements 'semantic branching' (i.e. 
   either the unnegated or neagted or-clause is established first depending on 
   the computed JW weights. If the first alternative fails (clashes), its negated 
   counterpart is tried. It is the only place where *or-level*'s value is changed."
  (declare #+:allegro (:explain :tailmerging)
           (ignore unused-1 unused-2 unused-3))
  (incf *or-level*)
  (setf-statistics *max-or-level* (max (get-local-statistics-integer-value *max-or-level*)
                                       *or-level*))
  (let* ((new-state-1 (if *in-precompletion*
                          (commit-precompletion state or-constraint)
                        state))
         (new-constraint (expand-or-constraint-clause or-constraint new-state-1))
         (new-other-constraint nil)
         (first-clash-dependencies nil))
    (labels
        ((partially-expanded-or-satisfiable-continuation-1 (sat-p state unused-1 unused-2 unused-3)
           (declare (ignore unused-1 unused-2 unused-3))
           (if sat-p
               (progn
                 (if new-other-constraint
                     (progn
                       (race-trace ("~&first variant of success-based disjunct ~S in ~S satisfied~%"
                                    new-other-constraint or-constraint))
                       ;(princ "+")
                       (incf-select-disjunct-table-entry new-other-constraint))
                   (progn
                     (race-trace ("~&first variant of disjunct ~S in ~S satisfied~%"
                                  new-constraint or-constraint))
                     (incf-select-disjunct-table-entry new-constraint)))
                 (decf *or-level*)
                 (added-constraints-satisfiable nil nil state t nil))
             (let ((new-other-constraint-generated-p (not (null new-other-constraint))))
               (if new-other-constraint-generated-p
                   (progn
                     (decf-select-disjunct-table-entry new-other-constraint)
                     (decf-statistics *sat-first-alt-split*))
                 (progn
                   (decf-select-disjunct-table-entry new-constraint)
                   (decf-statistics *sat-first-split*)))
               (if (or (not *backjumping*)
                       (backjumping-stopped-here or-constraint))
                   ;; no backjumping allowed
                   (let* ((new-negated-constraint 
                           (if new-other-constraint-generated-p
                               (negate-constraint new-other-constraint or-constraint)
                             (negate-constraint new-constraint or-constraint)))
                          (use-success-based-disjunct-selection *use-success-based-disjunct-selection*)
                          (select-disjunct-table *select-disjunct-table*)
                          (new-disjunct-hits
                           (when (and use-success-based-disjunct-selection
                                      (not new-other-constraint-generated-p))
                             (get-disjunct-table-count (constraint-concept new-negated-constraint)
                                                       select-disjunct-table))))
                     (when-debug (not *backjumping*)
                       (unless (backjumping-stopped-here or-constraint)
                         (race-trace ("Ignoring backjumping with dependencies ~S for ~S"
                                      *catching-clash-dependencies* or-constraint))))
                     (setf first-clash-dependencies
                           (union-dependencies *catching-clash-dependencies* first-clash-dependencies))
                     (when (and use-success-based-disjunct-selection
                                (not new-other-constraint-generated-p)
                                (< new-disjunct-hits 0)
                                (> (constraint-open-clauses-counter or-constraint) 1))
                       (let* ((constraint (expand-or-constraint-clause or-constraint new-state-1))
                              (concept (constraint-concept constraint))
                              (alternate-disjunct-hits
                               (get-disjunct-table-count concept select-disjunct-table)))
                         (unless (or (eq (constraint-concept new-negated-constraint) concept)
                                     (eq (constraint-concept new-constraint) concept)
                                     (< alternate-disjunct-hits 0)
                                     (<= alternate-disjunct-hits new-disjunct-hits))
                           (setf new-other-constraint constraint)
                           (when-debug t
                             (let* ((old-selected-clause (constraint-concept new-negated-constraint))
                                    (old-pos-hits (get-disjunct-table-count old-selected-clause
                                                                            select-disjunct-table))
                                    (old-neg-hits (get-disjunct-table-count (concept-negated-concept
                                                                             old-selected-clause)
                                                                            select-disjunct-table))
                                    (newly-selected-clause concept)
                                    (pos-hits (get-disjunct-table-count newly-selected-clause
                                                                        select-disjunct-table))
                                    (neg-hits (get-disjunct-table-count (concept-negated-concept
                                                                         newly-selected-clause)
                                                                        select-disjunct-table)))
                               (race-trace ("~&Replacing old disjunct ~S (pos=~D,neg=~D) by success-based disjunct ~S (pos=~D,neg=~D) for constraint ~S and table ~S~%"
                                            old-selected-clause old-pos-hits old-neg-hits
                                            newly-selected-clause pos-hits neg-hits
                                            or-constraint select-disjunct-table)))))))
                     (race-trace ("~&saving dependencies ~S~%" *catching-clash-dependencies*))
                     (set-clash-dependencies nil)
                     (if new-other-constraint-generated-p
                         (race-trace ("~&first variant of success-based disjunct ~S in ~S clashed~%"
                                      new-other-constraint or-constraint))
                       (race-trace ("~&first variant of disjunct ~S in ~S clashed~%"
                                    new-constraint or-constraint)))
                     (labels
                         ((partially-expanded-or-satisfiable-continuation-2 (sat-p state unused-1 unused-2 unused-3)
                            (declare (ignore unused-1 unused-2 unused-3))
                            (decf *or-level*)
                            (if sat-p
                                (progn
                                  (if new-other-constraint
                                      (race-trace ("~&second variant of success-based disjunct ~S in ~S satisfied~%"
                                                   new-negated-constraint or-constraint))
                                    (race-trace ("~&second variant of disjunct ~S in ~S satisfied~%"
                                                 new-negated-constraint or-constraint)))
                                  (incf-select-disjunct-table-entry new-negated-constraint)
                                    ;(when new-other-constraint (princ "@"))
                                  (added-constraints-satisfiable nil nil state t nil))
                              (progn
                                  ;(when new-other-constraint (princ "-"))
                                (if new-other-constraint
                                    (decf-statistics *sat-second-alt-split*)
                                  (decf-statistics *sat-second-split*))
                                (decf-select-disjunct-table-entry new-negated-constraint)
                                (if (backjumping-stopped-here or-constraint)
                                    (progn
                                      (add-clash-dependencies first-clash-dependencies)
                                      (when (constraint-signature or-constraint)
                                        (add-signature-dependencies
                                         (signature-dependencies (constraint-signature or-constraint))))
                                      (remove-dependency or-constraint)
                                      (if *use-refined-signature-clash-dependencies*
                                          (add-clash-dependencies
                                           (refine-signature-clash-dependencies (constraint-concept or-constraint)
                                                                                nil
                                                                                (constraint-or-dependencies or-constraint)
                                                                                or-constraint
                                                                                nil
                                                                                nil))
                                        (add-clash-dependencies (constraint-or-dependencies or-constraint)))
                                      (if new-other-constraint
                                          (progn
                                            (race-trace ("~&both variants of success-based disjunct ~S in ~S clashed~%"
                                                         new-negated-constraint or-constraint))
                                            (incf-statistics *unsat-alt-splits*))
                                        (progn
                                          (race-trace ("~&both variants of disjunct ~S in ~S clashed~%"
                                                       new-negated-constraint or-constraint))
                                          (incf-statistics *unsat-splits*)))
                                      (race-trace ("~&dependencies = ~S" *catching-clash-dependencies*))
                                      (handle-clash-with-backtrack-state new-state-1 nil nil nil nil))
                                  (progn
                                    ;; still doing backjumping
                                    (if new-other-constraint
                                        (progn
                                          (race-trace ("~&backjumping: both variants of success-based disjunct ~S in ~S clashed, dep=~S~%"
                                                       new-negated-constraint or-constraint
                                                       *catching-clash-dependencies*))
                                          (incf-statistics *skipped-unsat-alt-splits*))
                                      (progn
                                        (race-trace ("~&backjumping: both variants of disjunct ~S in ~S clashed, dep=~S~%"
                                                     new-negated-constraint or-constraint
                                                     *catching-clash-dependencies*))
                                        (incf-statistics *skipped-unsat-splits*)))
                                    (handle-clash-with-backtrack-state new-state-1
                                                                       nil nil nil nil)))))))
                       (if (and (not new-other-constraint-generated-p) new-other-constraint)
                           (let* ((new-state-2 (copy-basic-kernel-state new-state-1 nil))
                                  (new-partially-expanded-or-stack
                                   (push-backtrack-stack #'partially-expanded-or-satisfiable-continuation-1
                                                         (state-partially-expanded-or-stack
                                                          new-state-2)))
                                  (new-state-3
                                   (changed-kernel-state new-state-2
                                                         :partially-expanded-or-stack
                                                         new-partially-expanded-or-stack)))
                             (race-trace ("~&Saving backtrack closure ~S in state ~S, stack=~S~%"
                                          #'partially-expanded-or-satisfiable-continuation-1
                                          new-state-3
                                          (copy-seq new-partially-expanded-or-stack)))
                             (race-trace ("~&Trying first variant of success-based disjunct (or-level=~D) ~S in ~S, state=~S~%"
                                          *or-level* new-other-constraint or-constraint new-state-3))
                             (incf-statistics *sat-first-alt-split*)
                             (incf-statistics *alt-splits*)
                             (added-constraints-satisfiable new-other-constraint
                                                            nil ; no new relation constraint
                                                            new-state-3
                                                            t nil))
                         (let* ((new-partially-expanded-or-stack
                                 (push-backtrack-stack #'partially-expanded-or-satisfiable-continuation-2
                                                       (state-partially-expanded-or-stack
                                                        new-state-1)))
                                (new-state-2
                                 (changed-kernel-state new-state-1
                                                       :partially-expanded-or-stack
                                                       new-partially-expanded-or-stack)))
                           (race-trace ("~&Saving backtrack closure ~S in state ~S, stack=~S~%" 
                                        #'partially-expanded-or-satisfiable-continuation-2
                                        new-state-2
                                        (copy-seq new-partially-expanded-or-stack)))
                           (if new-other-constraint
                               (progn
                                 (race-trace ("~&Trying second variant of success-based disjunct (or-level=~D) ~S in ~S, state=~S~%"
                                              *or-level* new-other-constraint or-constraint new-state-2))
                                 (incf-statistics *sat-second-alt-split*))
                             (progn
                               (race-trace ("~&Trying second variant of disjunct (or-level=~D) ~S in ~S, state=~S~%"
                                            *or-level* new-negated-constraint or-constraint new-state-2))
                               (incf-statistics *sat-second-split*)))
                           (added-constraints-satisfiable new-negated-constraint
                                                          nil ; no new relation constraint
                                                          new-state-2
                                                          t nil)))))
                 (progn
                   (race-trace ("~&backjumping: skipping ~S, dep=~S~%"
                                or-constraint *catching-clash-dependencies*))
                   (if new-other-constraint
                       (incf-statistics *skipped-unsat-alt-splits*)
                     (incf-statistics *skipped-unsat-splits*))
                   (handle-clash-with-backtrack-state new-state-1 nil nil nil nil)))))))
      (let* ((new-state-2 (copy-basic-kernel-state new-state-1 nil))
             (new-partially-expanded-or-stack
              (push-backtrack-stack #'partially-expanded-or-satisfiable-continuation-1
                                    (state-partially-expanded-or-stack new-state-2)))
             (new-state-3
              (changed-kernel-state new-state-2
                                    :partially-expanded-or-stack new-partially-expanded-or-stack)))
        (race-trace ("~&Saving backtrack closure ~S in state ~S, stack=~S~%"
                     #'partially-expanded-or-satisfiable-continuation-1
                     new-state-3
                     (copy-seq new-partially-expanded-or-stack)))
        (race-trace ("~&Trying first variant of disjunct (or-level=~D) ~S in ~S, state=~S~%"
                     *or-level* new-constraint or-constraint new-state-1))
        (incf-statistics *sat-first-split*)
        (incf-statistics *splits*)
        (added-constraints-satisfiable new-constraint
                                       nil ; no new relation constraint
                                       new-state-3
                                       t nil)))))

;;; ======================================================================

(defun expand-relation-constraints (all-constraint relation-constraints some-feature-p store)
  "Create a list of new constraints (successors to relation-constraints) 
   concluded from firing all-constraints"
  #+:debug (assert (or (all-concept-p (constraint-concept all-constraint))
                       (and some-feature-p (some-concept-p (constraint-concept all-constraint)))))
  (let* ((ind (constraint-ind all-constraint))
         (term (constraint-term all-constraint))
         (self-concept-p (concept-self-reference-p term))
         (neg-term (concept-negated-concept term))
         (all-role (concept-role term))
         (inverse-all-role (role-inverse-internal all-role))
         (all-role-transitive-p (role-transitive-p all-role))
         (all-role-transitive-ancestors (remove-if #'role-not-transitive-p
                                                   (role-ancestors-internal all-role)))
         (all-term (or (concept-role-range term) (concept-term term)))
         (neg-all-term (concept-negated-concept all-term))
         (feature-ancestors (and some-feature-p (role-feature-ancestors all-role)))
         (relation-constraints (if *use-relation-store*
                                   (collect-all-fillers (list ind) store)
                                 relation-constraints)))
    (loop with tbox = *use-tbox*
          with bottom = *bottom-concept*
          with reflexive-roles-p = (when tbox
                                     (dl-reflexive-roles (tbox-language tbox)))
          for relation-constraint in relation-constraints
          for relation-role = (constraint-term relation-constraint)
          for inverse-relation-role = (role-inverse-internal relation-role)
          for reflexive-link-p = (eql (constraint-ind-1 relation-constraint)
                                      (constraint-ind-2 relation-constraint))
          for normal-forward-link-p =
          (and (eql (constraint-ind-1 relation-constraint) ind)
               (or (and some-feature-p
                        (matching-relation-constraint-simple-p relation-role
                                                               all-role
                                                               feature-ancestors))
                   (if (role-has-ancestors-p relation-role)
                       (member all-role (role-ancestors-internal relation-role))
                     (eq all-role relation-role))))
          for pseudo-forward-link-p =
          (and (not normal-forward-link-p)
               (eql (constraint-ind-2 relation-constraint) ind)
               (new-individual-p (constraint-ind-1 relation-constraint))
               (or (and some-feature-p
                        (matching-inverse-relation-constraint-simple-p inverse-relation-role
                                                                       all-role
                                                                       feature-ancestors))
                   (if (role-has-ancestors-p inverse-relation-role)
                       (member all-role (role-ancestors-internal inverse-relation-role))
                     (eq all-role inverse-relation-role))))
          for result = 
          (if (and reflexive-link-p
                   (or (and self-concept-p
                            (if (constraint-negated-p all-constraint)
                                (if (role-has-ancestors-p relation-role)
                                    (or (member all-role (role-ancestors-internal relation-role))
                                        (member inverse-all-role
                                                (role-ancestors-internal inverse-relation-role)))
                                  (or (eq all-role relation-role)
                                      (eq inverse-all-role inverse-relation-role)))
                              (if (role-has-ancestors-p all-role)
                                  (or (member relation-role (role-ancestors-internal all-role))
                                      (member inverse-relation-role
                                              (role-ancestors-internal (role-inverse-internal all-role))))
                                (or (eq relation-role all-role)
                                    (eq inverse-relation-role (role-inverse-internal all-role))))))
                       (and some-feature-p
                            reflexive-roles-p
                            (member-if (lambda (roles) (some #'role-reflexive-p roles))
                                       (role-ancestors-internal all-role)
                                       :key #'role-disjoint-roles))))
              (list (concluded-concept-constraint (constraint-ind-1 relation-constraint)
                                                  bottom
                                                  all-constraint
                                                  relation-constraint))
            (when (and (not self-concept-p) (or normal-forward-link-p pseudo-forward-link-p))
              (expand-relation-constraint relation-constraint
                                          relation-role
                                          inverse-relation-role
                                          normal-forward-link-p
                                          neg-term
                                          all-role
                                          all-role-transitive-p
                                          all-role-transitive-ancestors
                                          all-term
                                          neg-all-term
                                          all-constraint
                                          some-feature-p)))
          when result
          nconc result)))

(defun expand-relation-constraint (relation-constraint
                                   relation-role
                                   inverse-relation-role
                                   normal-forward-link-p
                                   neg-term
                                   all-role
                                   all-role-transitive-p
                                   all-role-transitive-ancestors
                                   all-term
                                   neg-all-term
                                   all-constraint
                                   some-feature-p)
  (let* ((effective-role (if normal-forward-link-p
                             relation-role
                           inverse-relation-role))
         (transitive-ancestors
          (and (not (eq all-role effective-role))
               (not (role-feature-p all-role))
               (role-has-ancestors-p effective-role)
               (set-difference (loop for role in (role-ancestors-internal effective-role)
                                     when (and (subrole-p role all-role)
                                               (role-transitive-p role))
                                     collect role)
                               all-role-transitive-ancestors)))
         (successor-ind (if normal-forward-link-p
                            (constraint-ind-2 relation-constraint)
                          (constraint-ind-1 relation-constraint))))
    (when (and transitive-ancestors
               (role-transitive-p all-role)
               (member all-role (role-ancestors-internal effective-role)))
      (pushnew all-role transitive-ancestors))
    (if transitive-ancestors
        (list* (concluded-concept-constraint successor-ind
                                             neg-all-term
                                             all-constraint
                                             relation-constraint)
               (loop for role in transitive-ancestors
                     collect
                     (concluded-concept-constraint successor-ind
                                                   (if (eq role all-role)
                                                       neg-term
                                                     (encode-concept-term
                                                      `(all ,role ,neg-all-term)))
                                                   all-constraint
                                                   relation-constraint)))
      (if (or all-role-transitive-p some-feature-p)
          (if all-role-transitive-p
              (list (concluded-concept-constraint successor-ind
                                                  neg-all-term
                                                  all-constraint
                                                  relation-constraint)
                    (concluded-concept-constraint successor-ind
                                                  neg-term
                                                  all-constraint
                                                  relation-constraint))
            (list (concluded-concept-constraint successor-ind
                                                all-term
                                                all-constraint
                                                relation-constraint)))
        (list (concluded-concept-constraint successor-ind
                                            neg-all-term
                                            all-constraint
                                            relation-constraint))))))

(defun get-violated-exists-constraints (ind role state)
  (collect-selected-constraints ind
                                (lambda (constraint)
                                  (and (exists-constraint-p constraint)
                                       (let ((concept (constraint-term constraint)))
                                         (or (member role (role-ancestors-internal (concept-role concept)))
                                             (and *inverse-roles* 
                                                  (concept-self-reference-p concept)
                                                  (member (role-inverse-internal role)
                                                          (role-ancestors-internal (concept-role concept))))))))
                                (state-expanded-constraints state)
                                (state-expanded-store state)
				(state-expanded-store-index state)))

(defun copy-violated-exists-constraints (constraints)
  (if *inverse-roles*
    (loop for constraint in constraints
          if (constraint-successor-ind constraint)
          collect (let ((new-constraint (copy-concept-constraint constraint)))
                    (setf (constraint-successor-ind new-constraint) nil) 
                    new-constraint)
          else collect constraint)
    constraints))

(defun get-violated-blocked-constraints (ind blocked-constraints state)
  (when blocked-constraints
    (collect-selected-constraints ind
                                  (lambda (constraint)
                                    (and (exists-constraint-p constraint)
                                         (member constraint blocked-constraints)))
                                  (state-expanded-constraints state)
                                  (state-expanded-store state)
				  (state-expanded-store-index state))))

(race-inline (old-individual-p true-old-individual-p older-individual-p
                               root-individual-p shiq-blocking-required
                               new-individual-p prover-individual-p))

(defun new-individual-p (ind)
  (and (numberp ind) (not (zerop ind))))

(defun prover-individual-p (ind)
  (numberp ind))

(defun old-individual-p (ind)
  (or (symbolp ind) (zerop ind)))

(defun true-old-individual-p (ind)
  (symbolp ind))

(defun older-individual-p (ind-1 ind-2)
  ;;; is ind-1 older than ind-2
  (if (numberp ind-1)
    (and (numberp ind-2) (< ind-1 ind-2))
    (numberp ind-2)))

(defun root-individual-p (ind)
  (and (numberp ind) (zerop ind)))

(defun shiq-blocking-required (language)
  (or (dl-merging language) (dl-features language)))

(defun expanded-ria-initial-satisfiable (ria-initial-constraint state unused-1 unused-2 unused-3)
  (declare (ignore unused-1 unused-2 unused-3))
  (let* ((relation-constraints (state-relation-constraints state))
         (relation-store (state-relation-store state))
         (new-constraints (nconc (when (if *use-relation-store*
                                           (not (relation-store-empty-p relation-store))
                                         relation-constraints)
                                   (expand-ria-initial-relation-constraints ria-initial-constraint
                                                                            relation-constraints
                                                                            relation-store))
                                 (expand-ria-initial-constraint ria-initial-constraint))))
    (added-constraints-satisfiable new-constraints
                                   nil ;no new relation constraint
                                   state
                                   t nil)))

(defun expand-ria-initial-relation-constraints (ria-initial-constraint relation-constraints store)
  #+:debug (assert (ria-initial-concept-p (constraint-concept ria-initial-constraint)))
  (let* ((ind (constraint-ind ria-initial-constraint))
         (term (constraint-concept ria-initial-constraint))
         (all-role (concept-role term))
         (relation-constraints (if *use-relation-store*
                                   (collect-all-fillers (list ind) store)
                                 relation-constraints)))
    (loop for relation-constraint in relation-constraints
          for relation-role = (constraint-term relation-constraint)
          for inverse-relation-role = (role-inverse-internal relation-role)
          for normal-forward-link-p =
          (and (eql (constraint-ind-1 relation-constraint) ind)
               (if (role-has-ancestors-p relation-role)
                   (member all-role (role-ancestors-internal relation-role))
                 (eq all-role relation-role)))
          for pseudo-forward-link-p =
          (and (not normal-forward-link-p)
               (eql (constraint-ind-2 relation-constraint) ind)
               (new-individual-p (constraint-ind-1 relation-constraint))
               (if (role-has-ancestors-p inverse-relation-role)
                   (member all-role (role-ancestors-internal inverse-relation-role))
                 (eq all-role inverse-relation-role)))
          when (or normal-forward-link-p pseudo-forward-link-p)
          collect
          (let ((successor-ind (if normal-forward-link-p
                                   (constraint-ind-2 relation-constraint)
                                 (constraint-ind-1 relation-constraint))))
            (concluded-concept-constraint successor-ind
                                          (encode-concept-term `(f-all ,all-role ,(concept-term term)))
                                          ria-initial-constraint
                                          relation-constraint)))))

(defun expand-complex-all-constraint (all-constraint)
  (let* ((concept (constraint-concept all-constraint))
         (all-role (concept-role concept))
         (complex-subroles-p (loop for subrole in (role-descendants-internal all-role)
                                   thereis (role-compositions subrole))))
    (when complex-subroles-p
      (list (concluded-concept-constraint (constraint-ind all-constraint)
                                          (encode-concept-term `(i-all ,all-role ,(concept-term concept)))
                                          all-constraint)))))

(defun expand-ria-initial-constraint (ria-initial-constraint)
  (let* ((concept (constraint-concept ria-initial-constraint))
         (all-role (concept-role concept))
         (complex-subroles (loop for subrole in (role-descendants-internal all-role)
                                 when (role-compositions subrole)
                                 collect subrole)))
    (when complex-subroles
      (let* ((term (concept-term concept))
             (ind (constraint-ind ria-initial-constraint)))
        (loop for subrole in complex-subroles
              nconc (initial-complex-all-constraints subrole
                                                     (role-compositions subrole)
                                                     ind
                                                     ria-initial-constraint
                                                     term))))))

(defun initial-complex-all-constraints (role compositions ind all-constraint term)
  (loop for composition in compositions
        if (eq (first (last composition)) role)
        collect (concluded-concept-constraint ind
                                              (encode-concept-term
                                               (create-ria-initial-concept (butlast composition) role term))
                                              all-constraint)
        else
        unless (eq (first composition) role)
        collect (concluded-concept-constraint ind
                                              (encode-concept-term
                                               (create-ria-final-concept composition role term))
                                              all-constraint)))

(defun create-ria-initial-concept (composition all-role term)
  (if (null composition)
      `(i-all ,all-role ,term)
    `(all ,(first composition) ,(create-ria-initial-concept (rest composition) all-role term))))

(defun create-ria-final-concept (composition all-role term)
  (if (null composition)
      `(f-all ,all-role ,term)
    `(all ,(first composition) ,(create-ria-final-concept (rest composition) all-role term))))

(defun expanded-ria-final-satisfiable (ria-final-constraint state unused-1 unused-2 unused-3)
  (declare (ignore unused-1 unused-2 unused-3))
  (let* ((concept (constraint-concept ria-final-constraint))
         (term (concept-term concept))
         (all-role (concept-role concept))
         (role-range (role-range-restriction all-role))
         (complex-subroles (loop for subrole in (role-descendants-internal all-role)
                                 when (role-compositions subrole)
                                 collect subrole))
         (ind (constraint-ind ria-final-constraint))
         (new-constraints
          (nconc (list (concluded-concept-constraint ind term ria-final-constraint))
                 (when role-range
                   (list (concluded-concept-constraint ind role-range ria-final-constraint)))
                 (unless (is-bottom-concept-p term)
                   (loop for subrole in complex-subroles
                         nconc (final-complex-all-constraints subrole
                                                              (role-compositions subrole)
                                                              ind
                                                              ria-final-constraint
                                                              term))))))
    (added-constraints-satisfiable new-constraints
                                   nil ;no new relation constraint
                                   state
                                   t nil)))

(defun final-complex-all-constraints (role compositions ind ria-final-constraint term)
  (loop for composition in compositions
        when (eq (first composition) role)
        collect (concluded-concept-constraint ind
                                              (encode-concept-term
                                               (create-ria-final-concept (rest composition) role term))
                                              ria-final-constraint)))

(defun expanded-all-satisfiable (all-constraint state some-feature-p unused-1 unused-2)
  (declare (ignore unused-1 unused-2))
  (if *inverse-roles*
    (expanded-all-satisfiable-2 all-constraint state some-feature-p nil nil)
    (expanded-all-satisfiable-1 all-constraint state some-feature-p nil nil)))

(defun expanded-all-satisfiable-1 (all-constraint state some-feature-p unused-1 unused-2)
  "Checks whether the constraints concluded from all-constraint are satisfiable"
  (declare (ignore unused-1 unused-2))
  (let ((unexpanded-exists-constraints (state-unexpanded-exists-constraints state))
        (expanded-constraints (state-expanded-constraints state))
        (relation-constraints (state-relation-constraints state))
        (relation-store (state-relation-store state)))
    (let* ((ind (constraint-ind all-constraint))
           (new-constraints (nconc (when (if *use-relation-store*
                                             (not (relation-store-empty-p relation-store))
                                           relation-constraints)
                                     (expand-relation-constraints all-constraint
                                                                  relation-constraints
                                                                  some-feature-p
                                                                  relation-store))
                                   (expand-complex-all-constraint all-constraint)))
           (violated-exists
            (when (or (dl-locally-reflexive-roles *dl-prover-language*)
                      (and (true-old-individual-p ind)
                           (not some-feature-p)
                           (or *using-precompletion*
                               (if *use-relation-store*
                                   (not (relation-store-empty-p relation-store))
                                 relation-constraints))))
              (get-violated-exists-constraints ind
                                               (concept-role
                                                (constraint-term all-constraint))
                                               state))))
      (multiple-value-bind (obsolete-individuals remaining-relation-constraints new-store)
                           (if violated-exists
                             (get-obsolete-individuals violated-exists
                                                       relation-constraints
                                                       state)
                             (values nil relation-constraints relation-store))
        ;(break "~S" all-constraint)
        (if obsolete-individuals
          (let ((new-violated-exists
                 (when violated-exists
                   (remove-obsolete-constraints obsolete-individuals
                                                violated-exists))))
            (multiple-value-bind (new-expanded-constraints-1 new-expanded-store-1 new-expanded-store-index-1)
                                 (remove-obsolete-expanded-constraints obsolete-individuals
                                                                       expanded-constraints
                                                                       new-store
                                                                       (state-expanded-store-index state)
                                                                       state)
              (multiple-value-bind
                (new-expanded-constraints-2 new-expanded-store-2 new-expanded-store-index-2)
                (remove-constraints-from-constraint-store new-violated-exists
                                                          new-expanded-constraints-1
                                                          new-expanded-store-1
                                                          (state-copy-expanded-store-p state)
                                                          state
                                                          #'reset-expanded-copy
                                                          new-expanded-store-index-1)
                (multiple-value-bind
                  (new-unexpanded-exists-constraints new-unexpanded-exists-constraints-store)
                  (if some-feature-p
                    (let ((store (state-unexpanded-exists-constraints-store state)))
                      (if (and *use-unexpanded-exists-constraints-store*
                               store
                               (not (constraint-store-unused-p store)))
                        (remove-constraint-from-constraint-store all-constraint
                                                                 unexpanded-exists-constraints
                                                                 store
                                                                 (state-copy-unexpanded-exists-constraints-store-p state)
                                                                 state
                                                                 #'reset-exists-copy)
                        (values (racer-remove all-constraint unexpanded-exists-constraints) store)))
                    (values unexpanded-exists-constraints
                            (state-unexpanded-exists-constraints-store state)))
                  (let ((new-state
                         (changed-kernel-state state
                                               :unexpanded-exists-constraints
                                               new-unexpanded-exists-constraints
                                               :expanded-constraints new-expanded-constraints-2
                                               :relation-constraints remaining-relation-constraints
                                               :expanded-store new-expanded-store-2
                                               :expanded-store-index new-expanded-store-index-2
                                               :relation-store new-store
                                               :unexpanded-exists-constraints-store
                                               new-unexpanded-exists-constraints-store)))
                    (expanded-all-satisfiable-1-1 new-state
                                                  obsolete-individuals
                                                  new-violated-exists
                                                  new-constraints
                                                  nil))))))
          (let ((copied-violated-exists
                 (when violated-exists
                   (copy-violated-exists-constraints violated-exists))))
            (multiple-value-bind
              (new-expanded-constraints new-expanded-store new-expanded-store-index)
              (if violated-exists
                (remove-constraints-from-constraint-store violated-exists
                                                          expanded-constraints
                                                          (state-expanded-store state)
                                                          (state-copy-expanded-store-p state)
                                                          state
                                                          #'reset-expanded-copy
                                                          (state-expanded-store-index state))
                (values expanded-constraints (state-expanded-store state) (state-expanded-store-index state)))
              (multiple-value-bind
                (new-unexpanded-exists-constraints new-unexpanded-exists-constraints-store)
                (if some-feature-p
                  (let ((store (state-unexpanded-exists-constraints-store state)))
                    (if (and *use-unexpanded-exists-constraints-store*
                             store
                             (not (constraint-store-unused-p store)))
                      (remove-constraint-from-constraint-store all-constraint
                                                               unexpanded-exists-constraints
                                                               store
                                                               (state-copy-unexpanded-exists-constraints-store-p state)
                                                               state
                                                               #'reset-exists-copy)
                      (values (racer-remove all-constraint unexpanded-exists-constraints) store)))
                  (values unexpanded-exists-constraints
                          (state-unexpanded-exists-constraints-store state)))
                (let ((new-state (changed-kernel-state state
                                                       :unexpanded-exists-constraints
                                                       new-unexpanded-exists-constraints
                                                       :expanded-constraints
                                                       new-expanded-constraints
                                                       :expanded-store new-expanded-store
                                                       :expanded-store-index new-expanded-store-index
                                                       :relation-store new-store
                                                       :unexpanded-exists-constraints-store
                                                       new-unexpanded-exists-constraints-store)))
                  (added-constraints-satisfiable (append new-constraints
                                                         copied-violated-exists)
                                                 nil ;no new relation constraint
                                                 new-state
                                                 t nil))))))))))


(defun expanded-all-satisfiable-1-1 (state
                                           obsolete-individuals
                                           violated-exists
                                           new-constraints
                                           unused)
  (declare (ignore unused))
  ;(break "~S" all-constraint)
  (let* ((new-unexpanded-deterministic-constraints
          (remove-obsolete-constraints obsolete-individuals
                                       (state-unexpanded-deterministic-constraints state)))
         (new-unexpanded-defined-det-constraints
          (remove-obsolete-constraints obsolete-individuals
                                       (state-unexpanded-defined-det-constraints state)))
         (copied-violated-exists
          (when violated-exists
            (copy-violated-exists-constraints violated-exists)))
         (new-labels (remove-obsolete-label-infos obsolete-individuals
                                                  (state-indirectly-blocked-individuals state)
                                                  (state-labels state)))
         (indirectly-blocked-individuals (state-indirectly-blocked-individuals state))
         (unexpanded-exists-constraints-store
          (state-unexpanded-exists-constraints-store state))
         (unexpanded-disjunctive-constraints-store
          (state-unexpanded-disjunctive-constraints-store state))
         #+(and :debug :lispworks)
         (relation-constraints (state-relation-constraints state)))
    (multiple-value-bind
      (new-unexpanded-exists-constraints new-unexpanded-exists-constraints-store)
      (if (and obsolete-individuals 
               *use-unexpanded-exists-constraints-store*
               unexpanded-exists-constraints-store
               (not (constraint-store-unused-p unexpanded-exists-constraints-store)))
        (remove-individuals-from-constraint-store obsolete-individuals
                                                (state-unexpanded-exists-constraints state)
                                                unexpanded-exists-constraints-store
                                                (state-copy-unexpanded-exists-constraints-store-p
                                                 state)
                                                state
                                                #'reset-exists-copy)
        (values (remove-obsolete-constraints obsolete-individuals
                                             (state-unexpanded-exists-constraints state))
                unexpanded-exists-constraints-store))
      (multiple-value-bind
        (new-unexpanded-disjunctive-constraints new-unexpanded-disjunctive-constraints-store)
        (if (and obsolete-individuals 
                 *use-unexpanded-disjunctive-constraints-store*
                 unexpanded-disjunctive-constraints-store
                 (not (constraint-store-unused-p unexpanded-disjunctive-constraints-store)))
          (remove-individuals-from-constraint-store obsolete-individuals
                                                  (state-unexpanded-disjunctive-constraints state)
                                                  unexpanded-disjunctive-constraints-store
                                                  (state-copy-unexpanded-disjunctive-constraints-store-p
                                                   state)
                                                  state
                                                  #'reset-disjunctive-copy)
          (values (remove-obsolete-constraints obsolete-individuals
                                               (state-unexpanded-disjunctive-constraints state))
                  unexpanded-disjunctive-constraints-store))
        ;(break "retry-all")
        (race-trace ("New constraints (~S ~S ~S ~S ~S ~S ~S ~S ~S ~S)~%"
                     new-unexpanded-deterministic-constraints
                     new-unexpanded-defined-det-constraints
                     new-unexpanded-exists-constraints
                     new-unexpanded-disjunctive-constraints
                     (state-expanded-constraints state)
                     relation-constraints
                     (state-attribute-constraints state)
                     (state-concrete-domain-state state)
                     new-labels
                     indirectly-blocked-individuals)
                    ("~&Removed obsolete individuals ~S~%" obsolete-individuals))
        (let ((new-indirectly-blocked-individuals
               (set-difference indirectly-blocked-individuals
                               obsolete-individuals)))
          (when-debug copied-violated-exists
            (race-trace ("~&Retrying exists constraints ~S (~S)~%"
                         copied-violated-exists relation-constraints)))
          ;(when violated-blocks (break "violated-blocks:~S" violated-blocks))
          (let ((new-state (changed-kernel-state state
                                                 :unexpanded-deterministic-constraints
                                                 new-unexpanded-deterministic-constraints
                                                 :unexpanded-defined-det-constraints
                                                 new-unexpanded-defined-det-constraints
                                                 :unexpanded-exists-constraints
                                                 new-unexpanded-exists-constraints
                                                 :unexpanded-disjunctive-constraints
                                                 new-unexpanded-disjunctive-constraints
                                                 :labels new-labels
                                                 :indirectly-blocked-individuals
                                                 new-indirectly-blocked-individuals
                                                 :unexpanded-exists-constraints-store
                                                 new-unexpanded-exists-constraints-store
                                                 :unexpanded-disjunctive-constraints-store
                                                 new-unexpanded-disjunctive-constraints-store)))
            (added-constraints-satisfiable (append new-constraints
                                                   copied-violated-exists)
                                           nil ;no new relation constraint
                                           new-state
                                           t nil)))))))

(defun expanded-all-satisfiable-2 (all-constraint state some-feature-p unused-1 unused-2)
  (declare (ignore unused-1 unused-2))
  (multiple-value-bind (new-state new-constraints)
                       (expanded-all-satisfiable-2-1 all-constraint state some-feature-p nil nil)
    (added-constraints-satisfiable new-constraints
                                   nil ;no new relation constraint
                                   new-state
                                   t nil)))


(defun expanded-all-satisfiable-2-1 (all-constraint state some-feature-p unused-1 unused-2)
  (declare (ignore unused-1 unused-2))
  (let ((unexpanded-deterministic-constraints (state-unexpanded-deterministic-constraints state))
        (unexpanded-defined-det-constraints (state-unexpanded-defined-det-constraints state))
        (unexpanded-exists-constraints (state-unexpanded-exists-constraints state))
        #+(and :debug :lispworks) 
        (unexpanded-disjunctive-constraints (state-unexpanded-disjunctive-constraints state))
        (expanded-constraints (state-expanded-constraints state))
        (relation-constraints (state-relation-constraints state))
        #+(and :debug :lispworks) 
        (attribute-constraints (state-attribute-constraints state))
        #+(and :debug :lispworks) 
        (concrete-domain-state (state-concrete-domain-state state))
        (labels (state-labels state))
        (indirectly-blocked-individuals (state-indirectly-blocked-individuals state))
        (relation-store (state-relation-store state)))
    (let* ((ind (constraint-ind all-constraint))
           (new-constraints (nconc (when (if *use-relation-store*
                                             (not (relation-store-empty-p relation-store))
                                           relation-constraints)
                                     (expand-relation-constraints all-constraint
                                                                  relation-constraints
                                                                  some-feature-p
                                                                  relation-store))
                                   (expand-complex-all-constraint all-constraint)))
           (possible-backpropagation (and *inverse-roles*
                                          new-constraints
                                          (not (old-individual-p ind))))
           (back-propagated-constraints
            (when possible-backpropagation
              (loop for constraint in new-constraints
                    for target-ind = (constraint-ind constraint)
                    when (and (not (old-individual-p target-ind))
                              (older-individual-p target-ind ind))
                    collect constraint
                    and do
                    (setf (constraint-backpropagated-p constraint) t))))
           (back-propagation-list
            (when back-propagated-constraints
              (get-backpropagation-list back-propagated-constraints labels))))
      (multiple-value-bind (new-labels-1 retry-labels-1)
                           (if back-propagation-list
                             (copy-labels-list back-propagation-list labels)
                             labels)
        (when-debug back-propagation-list
          (race-trace ("~&All-backpropagating ~S due to ~S, old labels=~S, new labels=~S ~
                        rel-constraints=~S~%"
                       new-constraints back-propagation-list labels new-labels-1
                       relation-constraints)))
        (multiple-value-bind (new-labels-2 blocked-labels retry-labels-2)
                             (if retry-labels-1
                               (if (shiq-blocking-required *dl-prover-language*)
                                 (shiq-find-newly-blocked-labels retry-labels-1
                                                                 new-labels-1
                                                                 state
                                                                 back-propagated-constraints)
                                 (shi-find-newly-blocked-labels retry-labels-1
                                                                new-labels-1
                                                                state
                                                                back-propagated-constraints))
                               new-labels-1)
          ;(unless blocked-labels
          ;  (when (find-newly-blocked-labels new-labels-2) (break)))
          (let* ((blocked-individuals (when blocked-labels
                                        (mapcar #'label-info-ind blocked-labels)))
                 (new-indirectly-blocked-individuals-1
                  (if blocked-individuals
                    (union (get-obsolete-successors-of-inds blocked-individuals
                                                            relation-constraints
                                                            relation-store)
                           (union blocked-individuals indirectly-blocked-individuals))
                    indirectly-blocked-individuals))
                 (violated-exists
                  (when (or (dl-locally-reflexive-roles *dl-prover-language*)
                            (and (not some-feature-p)
                                 (or (and *using-precompletion* (true-old-individual-p ind))
                                     (if *use-relation-store*
                                         (not (relation-store-empty-p relation-store))
                                       relation-constraints))))
                    (remove-if (lambda (constraint)
                                 (let ((successor-ind (constraint-successor-ind constraint)))
                                   (and successor-ind
                                        (if *use-relation-store*
                                          (is-left-individual-p successor-ind relation-store)
                                          (member successor-ind relation-constraints
                                                  :key #'constraint-ind-1)))))
                               (get-violated-exists-constraints ind
                                                                (concept-role
                                                                 (constraint-term all-constraint))
                                                                state)))))
            (when-debug blocked-labels
              (race-trace ("~&Blocking existing labels ~S, labels=~S, new blocked inds=~S~%"
                           blocked-labels new-labels-2 new-indirectly-blocked-individuals-1)))
            (multiple-value-bind
              (new-labels-3 violated-blocks)
              (if retry-labels-2
                (let ((broken-blocks nil))
                  #-:debug (declare (ignore broken-blocks))
                  (multiple-value-prog1
                    (loop for label in retry-labels-2
                          for blocked-constraints = (label-info-blocked-constraints label)
                          when blocked-constraints
                          append blocked-constraints into result
                          and collect
                          (cons label
                                (let ((new-label (copy-label-info label)))
                                  #+:debug (push label broken-blocks)
                                  (setf (label-info-blocked-constraints new-label) nil)
                                  (when (label-info-blocked-labels new-label)
                                    (loop for blocked-label in (label-info-blocked-labels new-label) do
                                          (setf (label-info-blocked-p blocked-label) nil))
                                    (setf (label-info-blocked-labels new-label) nil))
                                  new-label))
                          into new-labels
                          finally (return (values (replace-modified-labels new-labels labels)
                                                  result)))
                    #+:debug
		    (when broken-blocks
                      (race-trace ("~&All-unblocking blocked labels ~S~%" broken-blocks)))
                    ;(when broken-blocks (break "broken-blocks:~S" broken-blocks))
                    ))
                new-labels-2)
              (when-debug (not (eq new-labels-2 new-labels-3))
                (race-trace ("~&New unblocked labels ~S~%" new-labels-3)))
              (let ((all-violated-exists (append violated-exists violated-blocks)
                                         ;violated-blocks
                                         ))
                (when-debug new-indirectly-blocked-individuals-1
                  (race-trace ("~&Indirectly blocked inds=~S, blocked labels=~S~%"
                               new-indirectly-blocked-individuals-1 blocked-labels))
                  (race-trace ("Constraints (~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S)~%"
                               state
                               unexpanded-deterministic-constraints
                               unexpanded-defined-det-constraints
                               unexpanded-exists-constraints
                               unexpanded-disjunctive-constraints
                               expanded-constraints
                               relation-constraints
                               attribute-constraints
                               concrete-domain-state
                               labels
                               new-indirectly-blocked-individuals-1)))
                (multiple-value-bind (obsolete-individuals remaining-relation-constraints new-store)
                                     (if all-violated-exists
                                       (get-obsolete-individuals all-violated-exists
                                                                 relation-constraints
                                                                 state)
                                       (values nil relation-constraints relation-store))
                  ;(when violated-exists (break "~S: ~S" all-constraint violated-exists))
                  (if obsolete-individuals
                    (let* ((new-constraints
                            (remove-obsolete-constraints obsolete-individuals new-constraints))
                           (new-unexpanded-deterministic-constraints
                            (remove-obsolete-constraints obsolete-individuals
                                                         unexpanded-deterministic-constraints))
                           (new-unexpanded-defined-det-constraints
                            (remove-obsolete-constraints obsolete-individuals
                                                         unexpanded-defined-det-constraints))
                           (new-violated-exists
                            (when all-violated-exists
                              (remove-obsolete-constraints obsolete-individuals
                                                           all-violated-exists)))
                           (copied-violated-exists
                            (when new-violated-exists
                              (copy-violated-exists-constraints new-violated-exists)))
                           (new-labels-4 (remove-obsolete-label-infos obsolete-individuals
                                                                      new-indirectly-blocked-individuals-1
                                                                      new-labels-3))
                           (unexpanded-disjunctive-constraints-store
                            (state-unexpanded-disjunctive-constraints-store state))
                           (unexpanded-exists-constraints-store
                            (state-unexpanded-exists-constraints-store state)))
                      (multiple-value-bind
                        (new-unexpanded-exists-constraints new-unexpanded-exists-constraints-store-1)
                        (if (and obsolete-individuals 
                                 *use-unexpanded-exists-constraints-store*
                                 unexpanded-exists-constraints-store
                                 (not (constraint-store-unused-p unexpanded-exists-constraints-store)))
                          (remove-individuals-from-constraint-store obsolete-individuals
                                                                  unexpanded-exists-constraints
                                                                  unexpanded-exists-constraints-store
                                                                  (state-copy-unexpanded-exists-constraints-store-p
                                                                   state)
                                                                  state
                                                                  #'reset-exists-copy)
                          (values (remove-obsolete-constraints obsolete-individuals
                                                               (state-unexpanded-exists-constraints state))
                                  unexpanded-exists-constraints-store))
                        (multiple-value-bind
                          (new-unexpanded-disjunctive-constraints
                           new-unexpanded-disjunctive-constraints-store)
                          (if (and obsolete-individuals 
                                   *use-unexpanded-disjunctive-constraints-store*
                                   unexpanded-disjunctive-constraints-store
                                   (not (constraint-store-unused-p
                                         unexpanded-disjunctive-constraints-store)))
                            (remove-individuals-from-constraint-store obsolete-individuals
                                                                    (state-unexpanded-disjunctive-constraints state)
                                                                    unexpanded-disjunctive-constraints-store
                                                                    (state-copy-unexpanded-disjunctive-constraints-store-p
                                                                     state)
                                                                    state
                                                                    #'reset-disjunctive-copy)
                            (values (remove-obsolete-constraints obsolete-individuals
                                                                 (state-unexpanded-disjunctive-constraints state))
                                    unexpanded-disjunctive-constraints-store))
                          (multiple-value-bind
                            (new-expanded-constraints-1 new-expanded-store-1 new-expanded-store-index-1)
                            (remove-obsolete-expanded-constraints obsolete-individuals
                                                                  expanded-constraints
                                                                  (state-expanded-store state)
                                                                  (state-expanded-store-index state)
                                                                  state)
                            (multiple-value-bind
                              (new-expanded-constraints-2 new-expanded-store-2 new-expanded-store-index-2)
                              (remove-constraints-from-constraint-store new-violated-exists
                                                                        new-expanded-constraints-1
                                                                        new-expanded-store-1
                                                                        (state-copy-expanded-store-p
                                                                         state)
                                                                        state
                                                                        #'reset-expanded-copy
                                                                        new-expanded-store-index-1)
                              ;(break "retry-all")
                              (race-trace ("New constraints (~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S)~%"
                                           new-constraints
                                           new-unexpanded-deterministic-constraints
                                           new-unexpanded-defined-det-constraints
                                           new-unexpanded-exists-constraints
                                           new-unexpanded-disjunctive-constraints
                                           new-expanded-constraints-2
                                           remaining-relation-constraints
                                           attribute-constraints
                                           concrete-domain-state
                                           new-labels-4
                                           new-indirectly-blocked-individuals-1)
                                          ("~&Removed obsolete individuals ~S~%" obsolete-individuals))
                              (let ((new-indirectly-blocked-individuals-2
                                     (set-difference new-indirectly-blocked-individuals-1
                                                     obsolete-individuals)))
                                (when-debug copied-violated-exists
                                  (race-trace ("~&Retrying exists constraints ~S (~S)~%"
                                               copied-violated-exists remaining-relation-constraints)))
                                ;(when violated-blocks (break "violated-blocks:~S" violated-blocks))
                                (multiple-value-bind
                                  (new-unexpanded-exists-constraints-2
                                   new-unexpanded-exists-constraints-store-2)
                                  (if some-feature-p
                                    (if (and *use-unexpanded-exists-constraints-store*
                                            new-unexpanded-exists-constraints-store-1
                                             (not (constraint-store-unused-p
                                                   new-unexpanded-exists-constraints-store-1)))
                                      (remove-constraint-from-constraint-store all-constraint
                                                                               new-unexpanded-exists-constraints
                                                                               new-unexpanded-exists-constraints-store-1
                                                                               (state-copy-unexpanded-exists-constraints-store-p
                                                                                state)
                                                                               state
                                                                               #'reset-exists-copy)
                                      (values (racer-remove all-constraint new-unexpanded-exists-constraints)
                                              new-unexpanded-exists-constraints-store-1))
                                    (values new-unexpanded-exists-constraints
                                            new-unexpanded-exists-constraints-store-1))
			          (values (changed-kernel-state state
                                                                :unexpanded-deterministic-constraints
                                                                new-unexpanded-deterministic-constraints
                                                                :unexpanded-defined-det-constraints
                                                                new-unexpanded-defined-det-constraints
                                                                :unexpanded-exists-constraints
                                                                new-unexpanded-exists-constraints-2
                                                                :unexpanded-disjunctive-constraints
                                                                new-unexpanded-disjunctive-constraints
                                                                :expanded-constraints
                                                                new-expanded-constraints-2
                                                                :relation-constraints
                                                                remaining-relation-constraints
                                                                :labels new-labels-4
                                                                :indirectly-blocked-individuals
                                                                new-indirectly-blocked-individuals-2
                                                                :expanded-store new-expanded-store-2
                                                                :expanded-store-index new-expanded-store-index-2
                                                                :relation-store new-store
                                                                :unexpanded-exists-constraints-store
                                                                new-unexpanded-exists-constraints-store-2
                                                                :unexpanded-disjunctive-constraints-store
                                                                new-unexpanded-disjunctive-constraints-store)
				          (append new-constraints copied-violated-exists)))))))))
                    (let ((copied-violated-exists
                           (when all-violated-exists
                             (copy-violated-exists-constraints all-violated-exists)))
                          (unexpanded-exists-constraints-store
                           (state-unexpanded-exists-constraints-store state)))
                      (multiple-value-bind
                        (new-expanded-constraints new-expanded-store new-expanded-store-index)
                        (if all-violated-exists
                          (remove-constraints-from-constraint-store all-violated-exists
                                                                    expanded-constraints
                                                                    (state-expanded-store state)
                                                                    (state-copy-expanded-store-p state)
                                                                    state
                                                                    #'reset-expanded-copy
                                                                    (state-expanded-store-index state))
                          (values expanded-constraints (state-expanded-store state) (state-expanded-store-index state)))
                        (multiple-value-bind
                          (new-unexpanded-exists-constraints new-unexpanded-exists-constraints-store)
                          (if some-feature-p
                            (if (and *use-unexpanded-exists-constraints-store*
                                     unexpanded-exists-constraints-store
                                     (not (constraint-store-unused-p unexpanded-exists-constraints-store)))
                              (remove-constraint-from-constraint-store all-constraint
                                                                       unexpanded-exists-constraints
                                                                       unexpanded-exists-constraints-store
                                                                       (state-copy-unexpanded-exists-constraints-store-p
                                                                        state)
                                                                       state
                                                                       #'reset-exists-copy)
                              (values (racer-remove all-constraint unexpanded-exists-constraints)
                                      unexpanded-exists-constraints-store))
                            (values unexpanded-exists-constraints
                                    unexpanded-exists-constraints-store))
                          (values (changed-kernel-state state
                                                        :unexpanded-exists-constraints
                                                        new-unexpanded-exists-constraints
                                                        :expanded-constraints new-expanded-constraints
                                                        :labels new-labels-3
                                                        :indirectly-blocked-individuals
                                                        new-indirectly-blocked-individuals-1
                                                        :expanded-store new-expanded-store
                                                        :expanded-store-index new-expanded-store-index
                                                        :relation-store new-store
                                                        :unexpanded-exists-constraints-store
                                                        new-unexpanded-exists-constraints-store)
                                  (append new-constraints copied-violated-exists)))))))))))))))

(defun copy-labels-list (back-propagation-list labels)
  (loop for label in labels
        for update = (assoc (label-info-ind label) back-propagation-list)
        with new-label = nil
        if (and update
                (not (member-of-label-p (cdr update) (label-info-back-propagated-concepts label))))
        do (setf new-label (let ((new-label (copy-label-info label)))
                             (add-to-back-propagated-concepts (cdr update) new-label)
                             new-label))
        and collect new-label into result
        and collect new-label into new-labels
        else collect label into result
        finally (return (if new-labels
                          (values result new-labels)
                          labels))))

(defun member-of-label-p (concept concept-list)
  (if (and-concept-p concept)
    (concept-set-subsetp (concept-term concept) concept-list)
    (member concept concept-list)))

(defun add-to-back-propagated-concepts (concept label)
  (setf (label-info-back-propagated-concepts label)
        (make-simple-label concept (label-info-back-propagated-concepts label)))
  label)

(defun get-backpropagation-list (new-constraints labels)
  (loop with back-propagation-list = nil
        for constraint in new-constraints
        for concept = (if (constraint-negated-p constraint)
                        (concept-negated-concept (constraint-term constraint))
                        (constraint-term constraint))
        for target-ind = (constraint-ind constraint) do
        (unless (true-old-individual-p target-ind)
          (let ((label (first (find-label-for-ind target-ind labels))))
            #+:debug (assert label)
            (unless (member concept (label-info-back-propagated-concepts label))
              (push (cons target-ind concept) back-propagation-list))))
        finally (return back-propagation-list)))

(defun get-retry-labels (back-propagation-list labels)
  (loop for label in labels
        when (assoc (label-info-ind label) back-propagation-list)
        collect label))

(defun remove-obsolete-constraints (obsolete-individuals constraints)
  (if obsolete-individuals
    (loop for constraint in constraints
          unless (member (constraint-ind constraint) obsolete-individuals)
          collect constraint)
    constraints))

(defun remove-obsolete-expanded-constraints (obsolete-individuals
                                                   expanded-constraints
                                                   expanded-store
                                                   expanded-store-index
                                                   state)
  ;;return 2 values: new-expanded-constraints, new-expanded-store
  (if obsolete-individuals
    (remove-individuals-from-constraint-store obsolete-individuals
                                              expanded-constraints
                                              expanded-store
                                              (state-copy-expanded-store-p state)
                                              state
                                              #'reset-expanded-copy
                                              expanded-store-index)
    (values expanded-constraints expanded-store expanded-store-index)))

(defun get-obsolete-successors-of-inds (inds relation-constraints store)
  (loop with collected-inds = nil
        with remaining-relation-constraints = relation-constraints
        with removed-inds = inds
        for changed = nil
        do
        (multiple-value-bind (new-inds removed-relation-constraints)
                             (get-matching-relation-inds-2 removed-inds
                                                           remaining-relation-constraints
                                                           store)
          (when new-inds
            (unless (subsetp new-inds collected-inds)
              (setf changed t)
              (setf collected-inds (stable-union new-inds collected-inds))
              (setf removed-inds new-inds))
            (when removed-relation-constraints
              (setf remaining-relation-constraints
                    (constraint-set-difference remaining-relation-constraints
                                               removed-relation-constraints)))))
        until (not changed)
        finally (return collected-inds)))

(defun get-obsolete-individuals (violated-exists relation-constraints state)
  (loop with collected-inds = nil
        with remaining-relation-constraints = nil
        with removed-inds = nil
        with store = (state-relation-store state)
        with use-relation-store = *use-relation-store*
        initially
        (multiple-value-bind (new-inds removed-relation-constraints)
                             (get-matching-relation-inds-1 violated-exists
                                                           relation-constraints
                                                           store)
          (setf collected-inds new-inds)
          (setf removed-inds new-inds)
          (unless use-relation-store
            (setf remaining-relation-constraints
                  (constraint-set-difference relation-constraints removed-relation-constraints))))
        for changed = nil do
        (when removed-inds
          (multiple-value-bind (new-inds removed-relation-constraints)
                               (get-matching-relation-inds-2 removed-inds
                                                             remaining-relation-constraints
                                                             store)
            (when new-inds
              (setf changed t)
              (setf collected-inds (union new-inds collected-inds))
              (setf removed-inds new-inds)
              (unless use-relation-store
                (setf remaining-relation-constraints
                      (constraint-set-difference remaining-relation-constraints
                                                 removed-relation-constraints))))))
        until (not changed)
        finally
        (return (values collected-inds
                        remaining-relation-constraints
                        (if (and use-relation-store
                                 collected-inds
                                 (not (relation-store-empty-p store)))
                          (clean-relation-store (lambda (ind)
                                                  (or (old-individual-p ind)
                                                      (not (member ind collected-inds))))
                                                state)
                          store)))))

(defun get-matching-relation-inds-1 (violated-exists relation-constraints store)
  (loop with new-inds = nil
        with removed-relation-constraints = nil
        with use-relation-store = *use-relation-store*
        for violated-exist in violated-exists
        for ind = (constraint-ind violated-exist)
        for role = (role-inverse-internal (concept-role (constraint-term violated-exist))) do
        (unless (true-old-individual-p ind)
          (if use-relation-store
            (setf new-inds (relation-predecessor-individuals ind role store))
            (loop for relation-constraint in relation-constraints do
                  (when (and (eq (constraint-term relation-constraint) role)
                             (eql (constraint-ind-2 relation-constraint) ind))
                    (pushnew (constraint-ind-1 relation-constraint) new-inds)
                    (push relation-constraint removed-relation-constraints)))))
        finally (return (values new-inds removed-relation-constraints))))

(defun get-matching-relation-inds-2 (removed-inds relation-constraints store)
  (if *use-relation-store*
    (loop for ind in removed-inds
          nconc (predecessor-individuals ind store) into inds
          finally (return (racer-remove-duplicates inds)))
    (loop with new-inds = nil
          with removed-relation-constraints = nil
          for relation-constraint in relation-constraints do
          (when (member (constraint-ind-2 relation-constraint) removed-inds)
            (pushnew (constraint-ind-1 relation-constraint) new-inds)
            (push relation-constraint removed-relation-constraints))
          finally (return (values new-inds removed-relation-constraints)))))

;;; ======================================================================

(race-inline (matching-all-constraint-p
              matching-all-or-some-constraint-p
              matching-some-constraint-p))

(defun get-matching-all-some-or-at-most-constraints (relation-constraint
                                                             temp-expanded-constraints
                                                             state)
  (let* ((role (constraint-term relation-constraint))
         (role-ancestors (and (role-has-ancestors-p role)
                              (role-ancestors-internal role)))
         (ind (constraint-ind-1 relation-constraint))
         (feature-ancestors (role-feature-ancestors role)))
    (collect-selected-constraints ind
                                  (lambda (constraint)
                                    (let ((concept (constraint-term constraint)))
                                      (and (exists-concept-p concept)
                                           (let ((concept-role (concept-role concept)))
                                             (if (constraint-negated-p constraint)
                                               (if role-ancestors
                                                 (member concept-role role-ancestors)
                                                 (eq concept-role role))
                                               (and feature-ancestors
                                                    (not (lists-disjoint-p feature-ancestors
                                                                           (role-feature-ancestors
                                                                            concept-role)))))))))
                                  temp-expanded-constraints
                                  (state-expanded-store state)
				  (state-expanded-store-index state))))

(defun get-matching-all-or-some-constraints (relation-constraint state)
  (let* ((role (constraint-term relation-constraint))
         (role-ancestors (and (role-has-ancestors-p role)
                              (role-ancestors-internal role)))
         (ind (constraint-ind-1 relation-constraint))
         (feature-p (role-feature-p role))
         (feature-ancestors (and feature-p (role-feature-ancestors role))))
    (if feature-p
      (collect-selected-constraints ind
                                    (lambda (constraint)
                                      (let ((concept (constraint-term constraint)))
                                        (when (some-concept-p concept)
                                          (if role-ancestors
                                            (if (constraint-negated-p constraint)
                                              (member (concept-role concept) role-ancestors)
                                              (and feature-ancestors
                                                   (not (lists-disjoint-p feature-ancestors
                                                                          (role-feature-ancestors
                                                                           (concept-role concept))))))
                                            (eq (concept-role concept) role)))))
                                    (state-expanded-constraints state)
                                    (state-expanded-store state)
				    (state-expanded-store-index state))
      
      (collect-selected-constraints ind
                                    (lambda (constraint)
                                      (when (constraint-negated-p constraint)
                                        (let ((concept (constraint-term constraint)))
                                          (and (some-concept-p concept)
                                               (if role-ancestors
                                                 (member (concept-role concept) role-ancestors)
                                                 (eq (concept-role concept) role))))))
                                    (state-expanded-constraints state)
                                    (state-expanded-store state)
				    (state-expanded-store-index state)))))

(defun matching-all-constraint-p (constraint ind role role-ancestors concept)
  "Test whether constraint is an all-constraint matching the other parameters"
  (and (some-concept-p concept)
       (eql (constraint-ind constraint) ind)
       (constraint-negated-p constraint)
       (if role-ancestors
         (member (concept-role concept) role-ancestors)
         (eq (concept-role concept) role))))

(defun get-related-all-or-some-constraints (some-constraints state)
  (loop for some-constraint in some-constraints
        for all-constraints = 
        (let* ((some-concept (constraint-term some-constraint))
               (role (concept-role some-concept))
               (role-ancestors (and (role-has-ancestors-p role)
                                    (role-ancestors-internal role))))
          (if (role-feature-p role)
              (let ((feature-ancestors (role-feature-ancestors role)))
                (collect-selected-constraints (constraint-ind some-constraint)
                                              (lambda (constraint)
                                                (let ((concept (constraint-term constraint)))
                                                  (and (some-concept-p concept)
                                                       (or (constraint-negated-p constraint)
                                                           (not (member constraint some-constraints
                                                                        :test #'constraint-equal-test)))
                                                       (if role-ancestors
                                                           (if (constraint-negated-p constraint)
                                                               (member (concept-role concept) role-ancestors)
                                                             (and feature-ancestors
                                                                  (not (lists-disjoint-p feature-ancestors
                                                                                         (role-feature-ancestors
                                                                                          (concept-role concept))))))
                                                         (eq (concept-role concept) role)))))
                                              (state-expanded-constraints state)
                                              (state-expanded-store state)
                                              (state-expanded-store-index state)))
            (collect-selected-constraints (constraint-ind some-constraint)
                                          (lambda (constraint)
                                            (let ((concept (constraint-term constraint)))
                                              (and (or (some-concept-p concept)
                                                       (neg-ria-initial-concept-p concept))
                                                   (constraint-negated-p constraint)
                                                   (or (if role-ancestors
                                                           (member (concept-role concept) role-ancestors)
                                                         (eq (concept-role concept) role))
                                                       (when (concept-self-reference-p some-concept)
                                                         (let* ((inv-role (role-inverse-internal role))
                                                                (inv-role-ancestors
                                                                 (and (role-has-ancestors-p inv-role)
                                                                      (role-ancestors-internal inv-role))))
                                                           (if inv-role-ancestors
                                                               (member (concept-role concept) inv-role-ancestors)
                                                             (eq (concept-role concept) inv-role))))))))
                                          (state-expanded-constraints state)
                                          (state-expanded-store state)
					  (state-expanded-store-index state))))
        collect (list some-constraint all-constraints)))

(defun matching-all-at-most-constraint-p (constraint ind role role-ancestors concept)
  "Test whether constraint is an all- or at-most constraint matching the other parameters"
  (and (exists-concept-p concept)
       (eql (constraint-ind constraint) ind)
       (constraint-negated-p constraint)
       (if role-ancestors
         (member (concept-role concept) role-ancestors)
         (eq (concept-role concept) role))))

(defun get-related-all-some-atmost-constraints (exists-constraints state)
  (loop for exists-constraint in exists-constraints
        append
        (let* ((exists-concept (constraint-term exists-constraint))
               (role (concept-role exists-concept))
               (role-ancestors (and (role-has-ancestors-p role)
                                    (role-ancestors-internal role)))
               (ind (constraint-ind exists-constraint)))
          (collect-selected-constraints
           ind
           (lambda (constraint)
             (matching-all-at-most-constraint-p constraint
                                                ind
                                                role
                                                role-ancestors
                                                (constraint-term constraint)))
           (state-expanded-constraints state)
           (state-expanded-store state)
	   (state-expanded-store-index state)))))

(defun matching-relation-constraint-p (constraint some-ind some-role feature-ancestors)
  "Test whether constraint matches the other parameters"
  (let ((relation-role (constraint-term constraint)))
    (and (role-feature-p relation-role)
         (eql (constraint-ind-1 constraint) some-ind)
         (if feature-ancestors
           (not (lists-disjoint-p feature-ancestors
                                  (role-feature-ancestors relation-role)))
           (eq relation-role some-role)))))

(defun matching-relation-constraint-simple-p (relation-role some-role feature-ancestors)
  (and (role-feature-p relation-role)
       (if feature-ancestors
         (not (lists-disjoint-p feature-ancestors
                                (role-feature-ancestors relation-role)))
         (eq relation-role some-role))))

(defun matching-inverse-relation-constraint-simple-p (relation-role some-role feature-ancestors)
  (and (role-feature-p relation-role)
       (if feature-ancestors
         (not (lists-disjoint-p feature-ancestors
                                (role-feature-ancestors relation-role)))
         (eq relation-role some-role))))

(defun matching-some-constraint-p (constraint ind role feature-ancestors concept)
  "Test whether constraint matches the other parameters"
  (and (eql (constraint-ind constraint) ind)
       (some-concept-p concept)
       (if feature-ancestors
         (not (lists-disjoint-p feature-ancestors
                                (role-feature-ancestors (concept-role concept))))
         (eq (concept-role concept) role))))

(defun get-related-some-constraints (selected-some-constraint state)
  "Returns 4 values: feature-related-some-constraints, new-unexpanded-exists-constraints,
   new-unexpanded-exists-constraint-store, feature-hierarchy-collapsed-p"
  (let* ((selected-role (concept-role (constraint-term selected-some-constraint)))
         (unexpanded-exists-constraints (state-unexpanded-exists-constraints state))
         (unexpanded-exists-constraint-store (state-unexpanded-exists-constraints-store state)))
    (multiple-value-bind (new-unexpanded-exists-constraints
                          new-unexpanded-exists-constraint-store)
                         (remove-constraint-from-constraint-store selected-some-constraint
                                                                  unexpanded-exists-constraints
                                                                  unexpanded-exists-constraint-store
                                                                  (state-copy-unexpanded-exists-constraints-store-p
                                                                   state)
                                                                  state
                                                                  #'reset-exists-copy)
      (if (role-feature-p selected-role)
        (let* ((ind-related-exists-constraints
                (collect-ind-selected-constraints (constraint-ind selected-some-constraint)
                                                  new-unexpanded-exists-constraints
                                                  new-unexpanded-exists-constraint-store))
               (initial-some-partition
                (get-related-some-partitions selected-some-constraint
                                             selected-role
                                             ind-related-exists-constraints
                                             state)))
          (if (null initial-some-partition)
            (values nil
                    new-unexpanded-exists-constraints
                    new-unexpanded-exists-constraint-store
                    nil)
            (let ((some-partitions
                   (cons initial-some-partition
                         (loop for exists-constraints on ind-related-exists-constraints
                               for some-constraint = (first exists-constraints)
                               for role = (concept-role (constraint-term some-constraint))
                               for partition =
                               (when (role-feature-p role)
                                 (get-related-some-partitions some-constraint
                                                              role
                                                              (rest exists-constraints)
                                                              state))
                               when partition
                               collect partition))))
              (if (rest some-partitions)
                (let* ((new-exists-partitions
                        (loop for partitions on some-partitions
                              for partition = (first partitions)
                              unless (or (and (rest partitions)
                                              (subsuming-some-partition-found partition
                                                                              (rest partitions)))
                                         (and new-partitions
                                              (subsuming-some-partition-found partition
                                                                              new-partitions)))
                              collect partition into new-partitions
                              finally (return new-partitions)))
                       (new-merged-exists-partitions
                        (if (rest new-exists-partitions)
                          (merge-intersecting-some-partitions new-exists-partitions)
                          new-exists-partitions))
                       (merged-p (not (eq new-merged-exists-partitions
                                          new-exists-partitions)))
                       (selected-partition (find selected-some-constraint
                                                 new-merged-exists-partitions
                                                 :test #'member)))
                  #+:debug (assert (or (null new-merged-exists-partitions)
                                       selected-partition))
                  (multiple-value-bind
                    (new-unexpanded-exists-constraints-2
                     new-unexpanded-exists-constraint-store-2)
                    (remove-constraints-from-constraint-store
                     selected-partition
                     new-unexpanded-exists-constraints
                     new-unexpanded-exists-constraint-store
                     (state-copy-unexpanded-exists-constraints-store-p state)
                     state
                     #'reset-exists-copy)
                    (values (racer-remove selected-some-constraint selected-partition)
                            new-unexpanded-exists-constraints-2
                            new-unexpanded-exists-constraint-store-2
                            merged-p)))
                (let ((selected-partition (find selected-some-constraint some-partitions
                                                :test #'member)))
                  #+:debug (assert (or (null some-partitions) selected-partition))
                  (multiple-value-bind
                    (new-unexpanded-exists-constraints-2
                     new-unexpanded-exists-constraint-store-2)
                    (remove-constraints-from-constraint-store
                     selected-partition
                     new-unexpanded-exists-constraints
                     new-unexpanded-exists-constraint-store
                     (state-copy-unexpanded-exists-constraints-store-p state)
                     state
                     #'reset-exists-copy)
                    (values (racer-remove selected-some-constraint selected-partition)
                            new-unexpanded-exists-constraints-2
                            new-unexpanded-exists-constraint-store-2
                            nil)))))))
        (values nil
                new-unexpanded-exists-constraints
                new-unexpanded-exists-constraint-store
                nil)))))

(defun get-related-some-partitions (some-constraint role unexpanded-exists-constraints state)
  #+:debug (assert (role-feature-p role))
  (loop with ind = (constraint-ind some-constraint)
        with feature-ancestors = (when (role-has-feature-ancestors-p role)
                                   (role-feature-ancestors role))
        with role-ancestors = (role-ancestors-internal role)
        with expanded-constraints = (state-expanded-constraints state)
        with expanded-store = (state-expanded-store state)
        for constraint in unexpanded-exists-constraints
        unless (constraint-equal-test constraint some-constraint)
        when (and (matching-some-constraint-p constraint ind role feature-ancestors
                                              (constraint-term constraint))
                  (not (member constraint related-somes :test #'constraint-equal-test)))
        if (and (dl-merging *dl-prover-language*)
                (ind-selected-constraints-p (constraint-ind constraint)
                                            (lambda (constraint)
                                              (let ((concept (constraint-term constraint)))
                                                (and (at-least-concept-p concept)
                                                     (constraint-negated-p constraint)
                                                     (not (role-feature-p (concept-role concept)))
                                                     (member (concept-role concept) role-ancestors))))
                                            expanded-constraints
                                            expanded-store
					    (state-expanded-store-index state)))
        do (return nil)
        else collect constraint into related-somes
        end
        end
        finally (return (cons some-constraint related-somes))))

(defun merge-intersecting-some-partitions (some-partitions)
  (loop with new-partitions = some-partitions
        for modified = nil do
        (loop for partitions on new-partitions
              for selected-partition = (first partitions)
              for match = (find selected-partition (rest partitions)
                                :test-not #'lists-disjoint-p)
              do
              (when match
                (setf new-partitions (remove selected-partition (remove match new-partitions)))
                (setf selected-partition (stable-union selected-partition match))
                (pushnew selected-partition new-partitions)
                (setf modified t)
                (return)))
        until (not modified)
        finally (return new-partitions)))

(defun subsuming-some-partition-found (partition partitions)
  (loop for current-partition in partitions
        thereis (subsetp partition current-partition)))

(defun expand-all-or-some-constraint (some-constraint
                                      all-or-some-constraint
                                      successor-ind)
  (let* ((term (constraint-term all-or-some-constraint))
         (all-term (concept-term term))
         (neg-all-term (concept-negated-concept all-term))
         (all-role (concept-role term))
         (some-role (concept-role (constraint-term some-constraint)))
         (transitive-ancestors
          (and (not (eq all-role some-role))
               (not (role-feature-p all-role))
               (role-has-ancestors-p some-role)
               (nset-difference (loop for role in (role-ancestors-internal some-role)
                                      when (and (subrole-p role all-role)
                                                (role-transitive-p role))
                                      collect role)
                                (remove-if #'role-not-transitive-p
                                           (role-ancestors-internal all-role)))))
         (composition-ancestors
          (and (not (eq all-role some-role))
               (not (role-feature-p all-role))
               (role-has-ancestors-p some-role)
               (nset-difference (loop for role in (role-ancestors-internal some-role)
                                      when (and (subrole-p role all-role)
                                                (role-compositions role))
                                      collect role)
                                (remove-if-not #'role-compositions
                                               (role-ancestors-internal all-role))))))
    #+:debug (assert (or (some-concept-p term) (neg-ria-initial-concept-p term)))
    (when (and transitive-ancestors
               (role-transitive-p all-role)
               (member all-role (role-ancestors-internal some-role)))
      (pushnew all-role transitive-ancestors))
    (let ((result
           (cond
            (transitive-ancestors
             (list* (concluded-concept-constraint successor-ind
                                                  neg-all-term
                                                  all-or-some-constraint
                                                  some-constraint)
                    (loop for role in transitive-ancestors
                          collect
                          (concluded-concept-constraint successor-ind
                                                        (if (eq role all-role)
                                                            (concept-negated-concept term)
                                                          (encode-concept-term
                                                           `(all ,role ,neg-all-term)))
                                                        all-or-some-constraint
                                                        some-constraint))))
            ((role-transitive-p all-role)
             (list (concluded-concept-constraint successor-ind
                                                 (concept-negated-concept all-term)
                                                 all-or-some-constraint
                                                 some-constraint)
                   (concluded-concept-constraint successor-ind
                                                 (concept-negated-concept term)
                                                 all-or-some-constraint
                                                 some-constraint)))
            ((constraint-negated-p all-or-some-constraint)
             (when (or (not (concept-self-reference-p term))
                       (and (concept-self-reference-p (constraint-term some-constraint))
                            (or (member all-role (role-ancestors-internal some-role))
                                (member (role-inverse-internal all-role) (role-ancestors-internal some-role)))))
               (list (concluded-concept-constraint successor-ind
                                                   neg-all-term
                                                   all-or-some-constraint
                                                   some-constraint))))
            (t
             #+:debug (assert (role-feature-p all-role))
             (list (concluded-concept-constraint successor-ind
                                                 all-term
                                                 all-or-some-constraint
                                                 some-constraint))))))
      (when (neg-ria-initial-concept-p term)
        (push (concluded-concept-constraint successor-ind
                                            (encode-concept-term `(f-all ,all-role ,neg-all-term))
                                            all-or-some-constraint
                                            some-constraint)
              result)
        (when composition-ancestors
          (loop for role in composition-ancestors do
                (push (concluded-concept-constraint successor-ind
                                                    (encode-concept-term `(f-all ,role ,neg-all-term))
                                                    all-or-some-constraint
                                                    some-constraint)
                      result))))
      result)))

(defun expand-some-constraint (some-all-list
                               feature-related-some-constraints
                               &optional (ind-counter +ind-counter-init+))
  "Create a list of new concept constraints (successors of some-constraint) 
   and the new relation constraint. Returns list of new concept constraints."
  (loop for (some-constraint all-or-some-constraints) in some-all-list
        append
        (let* ((result 
                (cons (concluded-concept-constraint ind-counter
                                                    (concept-term
                                                     (constraint-term some-constraint))
                                                    some-constraint)
                      (mapcan #'(lambda (constraint)
                                  (expand-all-or-some-constraint some-constraint
                                                                 constraint
                                                                 ind-counter))
                              all-or-some-constraints)))
               (constraint-term (constraint-term some-constraint))
               (role-range (concept-role-range constraint-term)))
          (if role-range
              (cons (concluded-concept-constraint ind-counter 
                                                  role-range some-constraint)
                    result)
            result))
        into conclusion-concept-constraints
        finally
        (when feature-related-some-constraints
          (let ((feature-dependencies (collect-dependencies feature-related-some-constraints)))
            (when feature-dependencies
              (loop for constraint in conclusion-concept-constraints
                    do
                    (setf (constraint-dependencies constraint) 
                          (append feature-related-some-constraints
                                  (constraint-dependencies constraint)))
                    (setf (constraint-or-dependencies constraint)
                          (union-dependencies feature-dependencies
                                              (constraint-or-dependencies constraint)))))))
        (return conclusion-concept-constraints)))

(defun construct-label (some-constraint all-constraints)
  (loop with constraint-term = (constraint-term some-constraint)
        with concepts = (list* (concept-term constraint-term)
                               (let ((role-range
                                      (concept-role-range constraint-term)))
                                 (when role-range
                                   (list role-range))))
        with bottom = *bottom-concept*
        for all-constraint in all-constraints
        for concept = (constraint-term all-constraint)
        initially
        (when (loop for concept in concepts
                    thereis (or (eq concept bottom) (is-bottom-datatype-concept-p concept)))
          (return (values (list bottom) (list some-constraint))))
        do
        (unless (concept-self-reference-p concept)
          (if (constraint-negated-p all-constraint)
              (progn
                (when (role-transitive-p (concept-role concept))
                  (push (concept-negated-concept concept) concepts))
                (push (concept-negated-concept (concept-term concept)) concepts))
            (progn
              #+:debug (assert (some-concept-p concept))
              (push (or (concept-role-range concept)
                        (concept-term concept))
                    concepts))))
        (let ((elem-1 (first concepts))
              (elem-2 (second concepts)))
          (when (or (and elem-1
                         (or (eq elem-1 bottom)
                             (is-bottom-datatype-concept-p elem-1)))
                    (and elem-2
                         (or (eq elem-2 bottom)
                             (is-bottom-datatype-concept-p elem-2))))
            (return (values (list bottom)
                            (list some-constraint all-constraint)))))
        finally (return (make-label concepts))))

(defun construct-label-from-models (model-list)
  (loop for model in model-list
        for concept = (if (model-info-p model)
                        (model-concept model)
                        model)
        #+:debug do #+:debug (assert (concept-p-internal concept))
        collect concept into concepts
        finally (return (make-label concepts))))

(defun construct-label-from-constraints (constraints-list)
  (make-label
   (loop for constraint in constraints-list
         if (constraint-negated-p constraint)
         collect (concept-negated-concept (constraint-term constraint))
         else collect (constraint-term constraint))))

#|
(defun make-label (concepts)
  (with-flatten-encodings
    (without-optimized-encoding
      (without-taxonomic-encoding
        (let ((term (encode-concept-term `(and ,@concepts) nil t t)))
          (if (listp term)
            term
            (list term)))))))
|#

(defun make-label (concepts)
  (reverse-sort-concept-list 
   (concept-set-remove-duplicates
    (loop with top = *top-concept*
          with bottom = *bottom-concept*
          for concept in concepts
          unless (eq concept top)
          if (and-concept-p concept)
          append (concept-term concept)
          else
          if (eq concept bottom)
          do (return-from make-label (list bottom))
          else
          collect concept))))

(defun make-simple-sorted-label (new-concept concepts)
  (reverse-sort-concept-list
   (copy-list
    (if (and-concept-p new-concept)
      (concept-set-union (concept-term new-concept) concepts)
      (adjoin new-concept concepts)))))

(defun make-simple-label (new-concept concepts)
  (cond
   ((and-concept-p new-concept)
    (concept-set-union (concept-term new-concept) concepts))
   ((listp new-concept)
    (concept-set-union new-concept concepts))
   (t (adjoin new-concept concepts))))

(race-inline (true-label-info-p))

(defun true-label-info-p (label-info)
  (and (basic-label-info-p label-info)
       (not (label-info-model-in-progress label-info))))

(defun find-label-for-ind (ind labels)
  (unless (true-old-individual-p ind)
    (loop for sublabels on labels
          for label = (first sublabels)
          when (and (true-label-info-p label)
                    (eql (label-info-ind label) ind))
          do (return-from find-label-for-ind sublabels))
    (when (eql *ignored-live-individuals-cycles* 0)
      (error "could not find label for ind ~S in ~S" ind labels))))

(defun invalidate-concept-models (invalid-label-infos)
  (let ((all-entries (collect-invalidated-concept-models invalid-label-infos)))
    (race-trace ("~&Retracting concept models or cache entries ~S~%" all-entries))
    ;(break "~S" all-entries)
    (loop with tableaux-caching = *tableaux-caching*
          for entry in all-entries do
          (if (concept-p-internal entry)
            (progn
              (when (concept-model entry)
                (incf-statistics *retracted-models*)
                (setf (concept-model entry) nil))
              (when (atomic-concept-p entry)
                (let ((encoded-definition (concept-encoded-definition entry)))
                  (when (and encoded-definition (concept-model encoded-definition))
                    (incf-statistics *retracted-models*)
                    (setf (concept-model encoded-definition) nil)))))
            (when (and entry tableaux-caching)
              (retract-model entry))))))

(defun collect-invalidated-concept-models (invalid-entries)
  (racer-remove-duplicates (collect-invalidated-concept-models-1 invalid-entries nil)))

(defun collect-invalidated-concept-models-1 (invalid-entries collected-entries)
  (if (null invalid-entries)
    collected-entries
    (let ((label-info (first invalid-entries)))
      (if (true-label-info-p label-info)
        (let ((indirect-invalid-entries (get-label-info-dependent-models label-info))
              (label (label-info-label-or-concept label-info)))
          (if indirect-invalid-entries
            (collect-invalidated-concept-models-1 (append indirect-invalid-entries
                                                          (rest invalid-entries))
                                                  (if label
                                                    (cons label collected-entries)
                                                    collected-entries))
            (collect-invalidated-concept-models-1 (rest invalid-entries)
                                                  (if label
                                                    (cons label collected-entries)
                                                    collected-entries))))
        (let ((model-in-progress (label-info-model-in-progress label-info)))
          (collect-invalidated-concept-models-1 (rest invalid-entries)
                                                (if model-in-progress
                                                  (cons model-in-progress collected-entries)
                                                  collected-entries)))))))

(defun add-blocking-dependencies (witness labels)
  #+:debug (assert (basic-label-info-p witness))
  (loop for (dependency-elem label-elem) on labels
        while (and label-elem (not (eq dependency-elem witness)))
        do (add-label-info-dependent-model label-elem dependency-elem)
        until (eq label-elem witness)))

(defun add-caching-dependencies (labels)
  (loop for (dependency-elem label-elem) on labels
        while label-elem
        do (add-label-info-dependent-model label-elem dependency-elem)))

(defun expanded-some-satisfiable (some-constraint state unused-1 unused-2 unused-3)
  (declare (ignore unused-1 unused-2 unused-3))
  (let* ((concept (constraint-term some-constraint))
         (role-domain (concept-role-domain concept))
         (role-range (concept-role-range concept)))
    (if (or (and role-domain (is-any-bottom-concept-p role-domain))
            (and role-range (is-any-bottom-concept-p role-range)))
        (progn
          (set-clash-dependencies (constraint-or-dependencies some-constraint))
          (race-trace ("~&Unsatisfiable constraint ~S due to bottom domain or range of role ~S~%"
                       some-constraint (concept-role concept)))
          (return-from expanded-some-satisfiable
            (handle-clash-with-backtrack-state state nil nil nil nil)))
      (if (and *optimize-datatype-role-fillers*
               (not (dl-full-concrete-domains *dl-prover-language*))
               (optimize-datatype-role-fillers some-constraint
                                               (state-abox (state-parameters state))
                                               (state-old-individuals-dl-language-table state)
                                               *dl-prover-language*))
          (expanded-datatype-role-filler-satisfiable some-constraint state nil nil nil)
        (if *inverse-roles*
            (expanded-some-satisfiable-2 some-constraint state nil nil nil)
          (expanded-some-satisfiable-1 some-constraint state nil nil nil))))))

(defun expanded-datatype-role-filler-satisfiable (some-constraint
                                                         state
                                                         unused-1
                                                         unused-2
                                                         unused-3)
  (declare (ignore unused-1 unused-2 unused-3))
  (incf-statistics *model-size*)
  (setf-statistics *max-model-size* (max (get-local-statistics-integer-value *max-model-size*)
                                         (get-local-statistics-integer-value *model-size*)))
  (let* ((ind (constraint-ind some-constraint))
         (some-concept (constraint-term some-constraint))
         (qualification (concept-term some-concept))
         (role (concept-role some-concept))
	 (feature-p (role-feature-p role))
	 (ancestors (role-ancestors-internal role))
	 (related-some-constraints nil))
    (flet ((datatype-constraint-p (constraint)
             (and (exists-as-datatype-constraint-p constraint)
                  (member (concept-role
                           (constraint-term constraint))
                          ancestors))))
      (multiple-value-bind
	(new-unexpanded-exists-constraints
	 new-unexpanded-exists-constraint-store)
	(if (or feature-p (dl-merging *dl-prover-language*))
          (let ((new-state
                 (copy-basic-kernel-state state
                                          (and (not *in-precompletion*) ':unexpanded-exists))))
            (remove-constraint-from-constraint-store some-constraint
                                                     (state-unexpanded-exists-constraints new-state)
                                                     (state-unexpanded-exists-constraints-store new-state)
                                                     (state-copy-unexpanded-exists-constraints-store-p
                                                      new-state)
                                                     new-state
                                                     #'reset-exists-copy))
	  (values (state-unexpanded-exists-constraints state)
		  (state-unexpanded-exists-constraints-store state)))
        (if feature-p
	  (let* ((related-exists
		  (cons some-constraint
			(collect-selected-constraints ind
						      #'datatype-constraint-p
						      (state-expanded-constraints state)
						      (state-expanded-store state)
						      (state-expanded-store-index state))))
		 (cardinality (get-datatype-role-filler-cardinality related-exists role)))
	    (if (> cardinality 1)
              (let ((culprits related-exists))
                (set-clash-reasons (append culprits (list (state-relation-constraints state))))
                (set-clash-dependencies (collect-dependencies culprits))
                (race-trace ("~&Feature restriction for datatype role ~S violated by datatype constraints ~S~%"
                             (concept-role (constraint-term some-constraint))
                             culprits))
                (return-from expanded-datatype-role-filler-satisfiable
                  (handle-clash-with-backtrack-state state nil nil nil nil)))
	      (setf related-some-constraints (rest related-exists))))
          (progn
            (let ((related-predicate-bottom
                   (find-if-selected-constraints
                    (constraint-ind some-constraint)
                    (lambda (constraint)
                      (and (constraint-negated-p constraint)
                           (let* ((concept (constraint-term constraint)))
                             (if (and (exists-concept-p concept)
                                      (not (at-least-concept-p concept))
                                      (role-datatype (concept-role concept)))                                  
				 (let ((concept-term (concept-term concept)))
				   (and (cd-concept-p concept-term)
					(let ((predicate (concept-predicate concept-term)))
					  (and (eq (predicate-operator predicate) 'top)
					       (eq (first (predicate-parameters predicate))
						   (first (predicate-parameters 
							   (concept-predicate qualification))))
					       (member (concept-role concept) ancestors)))))))))
                    (state-expanded-constraints state)
                    (state-expanded-store state)
                    (state-expanded-store-index state))))
              (when related-predicate-bottom
                (let ((culprits (list some-constraint related-predicate-bottom)))
                  (set-clash-reasons (append culprits (list (state-relation-constraints state))))
                  (set-clash-dependencies (collect-dependencies culprits))
                  (race-trace ("~&CD-constraint restriction ~S clashed with ~S~%"
                               related-predicate-bottom
                               some-constraint))
                  (return-from expanded-datatype-role-filler-satisfiable
                    (handle-clash-with-backtrack-state state nil nil nil nil)))))
            (when (dl-merging *dl-prover-language*)
              (let* ((related-exists
                      (cons some-constraint
                            (collect-selected-constraints ind
                                                          #'datatype-constraint-p
                                                          (state-expanded-constraints state)
                                                          (state-expanded-store state)
                                                          (state-expanded-store-index state))))
                     (at-most-bounds (create-at-most-bounds ind
                                                            state
                                                            related-exists
                                                            nil)))
                (when at-most-bounds
                  (loop for at-most-bound in at-most-bounds
                        for cardinality = (get-datatype-role-filler-cardinality related-exists 
                                                                                (bound-role at-most-bound))
                        when (> cardinality (bound-number at-most-bound))
                        do
                        (let ((culprits (list (bound-constraint at-most-bound) related-exists)))
                          (set-clash-reasons (append culprits (list (state-relation-constraints state))))
                          (set-clash-dependencies (collect-dependencies culprits))
                          (race-trace ("~&At-most constraint ~S violated for datatype constraints ~S~%"
                                       (bound-constraint at-most-bound) related-exists))
                          (return-from expanded-datatype-role-filler-satisfiable
                            (handle-clash-with-backtrack-state state nil nil nil nil)))))))))
        (let ((new-state
               (changed-kernel-state state
                                     :unexpanded-exists-constraints new-unexpanded-exists-constraints
                                     :unexpanded-exists-constraints-store
                                     new-unexpanded-exists-constraint-store)))
          (expanded-some-satisfiable-1-2 some-constraint
                                         new-state
                                         related-some-constraints
                                         nil
                                         nil))))))

(defun get-datatype-role-filler-cardinality (related-exists role)
  (loop with true-fillers-count = 0
        with at-least-count = 0
        for exists in related-exists
        for exists-concept = (constraint-term exists)
        for term = (concept-term exists-concept)
        when (member role (role-ancestors-internal (concept-role exists-concept))) do
        (if (eq term *top-concept*)
          (setf at-least-count (max at-least-count (concept-number-restriction exists-concept)))
          (if (cd-concept-p term)
            (if (eq (predicate-operator (concept-predicate term)) 'top)
              (setf at-least-count (max at-least-count (concept-number-restriction exists-concept)))
              (incf true-fillers-count))
            (when (or (and-concept-p term) (or-concept-p term))
              (loop for concept in (concept-term term)
                    do
                    (when (cd-concept-p concept)
                      (if (eq (predicate-operator (concept-predicate concept)) 'top)
                        (setf at-least-count
                              (max at-least-count (concept-number-restriction exists-concept)))
                        (incf true-fillers-count))
                      (return))))))
        finally (return (max true-fillers-count at-least-count))))

(defun expanded-some-satisfiable-1 (some-constraint state unused-1 unused-2 unused-3)
  (declare (ignore unused-1 unused-2 unused-3))
  (incf-statistics *model-size*)
  (setf-statistics *max-model-size* (max (get-local-statistics-integer-value *max-model-size*)
                                         (get-local-statistics-integer-value *model-size*)))
  (multiple-value-bind
      (feature-related-some-constraints
       new-unexpanded-exists-constraints
       new-unexpanded-exists-constraint-store
       feature-hierarchy-collapsed-p)
      (get-related-some-constraints some-constraint
                                    (copy-basic-kernel-state state
                                                             (and (not *in-precompletion*)
                                                                  ':unexpanded-exists)))
    (let* ((role (concept-role (constraint-term some-constraint)))
           (feature-p (role-feature-p role)))
      (if (and (null feature-related-some-constraints)
               (or (constraint-merging-trigger-p some-constraint)
                   (not feature-p)
                   (not (every #'role-feature-p (role-ancestors-internal role))))
               (or (dl-merging *dl-prover-language*)
                   (and feature-p
                        (not *use-unique-name-assumption*)
                        (role-has-feature-ancestors-p role))))
          (let ((exists-partitions (at-most-constraint-violated-p some-constraint state)))
            (if exists-partitions
                (let ((new-state (if (end-of-precompletion-p exists-partitions)
                                     (commit-precompletion state exists-partitions)
                                   state))
                      (ind (constraint-ind some-constraint)))
                  (expanded-exists-partitions-satisfiable-1 ind
                                                            exists-partitions
                                                            new-state
                                                            nil nil))
              (let ((new-state
                     (changed-kernel-state state
                                           :unexpanded-exists-constraints new-unexpanded-exists-constraints
                                           :unexpanded-exists-constraints-store
                                           new-unexpanded-exists-constraint-store)))
                (expanded-some-satisfiable-1-1 some-constraint new-state nil nil nil))))
        (if (and feature-related-some-constraints
                 (disjoint-roles-clash-in-feature-exists-constraints-p (state-tbox (state-parameters state))
                                                                       (cons some-constraint
                                                                             feature-related-some-constraints)))
            (progn
              (set-clash-reasons (cons some-constraint feature-related-some-constraints))
              (set-clash-dependencies
               (collect-dependencies (cons some-constraint feature-related-some-constraints)))
              (set-signature-clash-reasons (cons some-constraint feature-related-some-constraints)
                                           *catching-clash-dependencies*)
              (race-trace ("~&Uunsatisfiable feature constraints ~S, dep=~S~%"
                           (cons some-constraint feature-related-some-constraints)
                           *catching-clash-dependencies*))
              (incf-statistics *tableaux-cache-unsat-hits*)
              (return-from expanded-some-satisfiable-1
                (handle-clash-with-backtrack-state state nil nil nil nil)))
          (let ((new-state
                 (changed-kernel-state state
                                       :unexpanded-exists-constraints new-unexpanded-exists-constraints
                                       :unexpanded-exists-constraints-store
                                       new-unexpanded-exists-constraint-store)))
            (expanded-some-satisfiable-1-1 some-constraint
                                           new-state
                                           feature-related-some-constraints
                                           feature-hierarchy-collapsed-p
                                           nil)))))))

(defun end-of-precompletion-p (exists-partitions)
  (when *in-precompletion*
    (loop with top-concept = *top-concept*
          for partition in exists-partitions
          for concept = (constraint-term (third partition))
          thereis (or (not (eq (concept-term concept) top-concept))
                      ;;; > 2 correct due to negation
                      (> (concept-number-restriction concept) 2)
                      ))))

(defun expanded-some-satisfiable-1-1 (some-constraint
                                      state
                                      feature-related-some-constraints
                                      feature-hierarchy-collapsed-p
                                      unused)
  (declare (ignore unused))
  (let* ((expanded-constraints (state-expanded-constraints state))
         (relation-constraints (state-relation-constraints state))
         (labels (state-labels state))
         (model-merging *model-merging*)
         (blocking-possibly-required *blocking-possibly-required*)
         (tableaux-caching *tableaux-caching*)
         (subtableaux-model-merging *subtableaux-model-merging*)
         (concept (constraint-term some-constraint))
         (self-concept-p (concept-self-reference-p concept))
         (self-conclusion-concept-constraints nil))
    (let ((relation-store (when (and *use-relation-store*
                                     (not (relation-store-empty-p (state-relation-store state))))
                            (state-relation-store state))))
      (cond
       ((or (and *use-elh-model-embedding* (subset-el+-p *dl-prover-language*))
            (not (or (role-datatype (concept-role concept))
                     (dl-clash-possible-p *dl-prover-language*))))
        t)
       ((and *ignore-abox-redundant-exists*
             relation-store
             (not (constraint-merging-trigger-p some-constraint))
             (not (constraint-signature some-constraint))
             (not (exists-as-datatype-constraint-p some-constraint))
             (consp (state-expanded-store state))
             (some-concept-p concept)
             (role-successor-exists-p (constraint-ind some-constraint)
                                      (concept-role concept)
                                      (concept-term concept)
                                      expanded-constraints
                                      (state-expanded-store state)
                                      (state-expanded-store-index state)
                                      relation-store))
        (race-trace ("~&Ignoring ABox-implied some-constraint ~S" some-constraint))
        t)
       (t
        (let* ((some-all-list (get-related-all-or-some-constraints
                               (cons some-constraint
                                     feature-related-some-constraints)
                               state))
               (all-constraints
                (append feature-related-some-constraints
                        (reduce #'append some-all-list :key #'second))))
          (multiple-value-bind (label clash-constraints)
              (construct-label some-constraint all-constraints)
            (if (and (eq (first label) *bottom-concept*))
                (progn
                  (set-clash-reasons (append (if clash-constraints
                                                 clash-constraints
                                               (cons some-constraint all-constraints))
                                             (list relation-constraints)))
                  (set-clash-dependencies
                   (if clash-constraints
                       (collect-dependencies clash-constraints)
                     (union-dependencies (constraint-or-dependencies some-constraint)
                                         (collect-dependencies all-constraints))))
                  (if clash-constraints
                      (race-trace ("~&Encoding detected unsatisfiable constraints ~S~%"
                                   clash-constraints))
                    (race-trace ("~&Encoding detected unsatisfiable constraints ~S ~S~%"
                                 some-constraint all-constraints)))
                  (return-from expanded-some-satisfiable-1-1
                    (handle-clash-with-backtrack-state state nil nil nil nil)))
              (let ((witness (when (and blocking-possibly-required (not self-concept-p))
                               (sh-find-witness label labels))))
              ;(break "~S" some-constraint)
                (if witness
                    (progn
                      (race-trace ("~%Blocking constraint ~S by witness ~S~%"
                                   some-constraint witness))
                      (setf *blocking-used* witness)
                      (when (or tableaux-caching model-merging)
                        (add-blocking-dependencies witness labels)
                    ;(break "~S" label)
                        (race-trace ("~%Blocking stack ~S~%" labels)))
                      t)
                  (let ((role (concept-role concept)))
                    (if (and (role-datatype role)
                             (not (dl-any-concrete-domain (concept-language (concept-term concept)))))
                        (progn
                          (set-clash-dependencies (constraint-or-dependencies some-constraint))
                          (race-trace ("~&Unsatisfiable constraint ~S due to datatype property ~S~%"
                                       some-constraint role))
                          (return-from expanded-some-satisfiable-1-1
                            (handle-clash-with-backtrack-state state nil nil nil nil)))
                      (let ((conclusions-key label))
                        (multiple-value-bind (old-result dependencies)
                            (when (and tableaux-caching label (not self-concept-p))
                              (get-model conclusions-key))
                          (declare (ignore dependencies))
                          (if old-result
                              (if (incoherent-model-p old-result)
                                  (progn
                                    (set-clash-reasons (append (cons some-constraint
                                                                     feature-related-some-constraints)
                                                               all-constraints
                                                               (list relation-constraints)))
                                    (set-clash-dependencies
                                     (union-dependencies (collect-dependencies (cons some-constraint
                                                                                     feature-related-some-constraints))
                                                         (collect-dependencies all-constraints)))
                                    (set-signature-clash-reasons
                                     (append (cons some-constraint feature-related-some-constraints)
                                             all-constraints)
                                     *catching-clash-dependencies*)
                                    ;;; the re-use is still unsound
                                    #|(when (and dependencies
                                             (< (length dependencies) (length *catching-clash-dependencies*))
                                             (subsetp dependencies *catching-clash-dependencies*
                                                      :test 'equal))
                                    (setf *catching-clash-dependencies* 
                                          (set-difference *catching-clash-dependencies* 
                                                          (set-difference *catching-clash-dependencies*
                                                                          dependencies :test 'equal)
                                                          :test 'equal))
                                    (princ "+"))|#
                                    (race-trace ("~&Cache hit for unsatisfiable constraints ~S ~S, ~
                                      label=~S, dep=~S~%"
                                                 some-constraint all-constraints
                                                 conclusions-key
                                                 *catching-clash-dependencies*))
                                    (incf-statistics *tableaux-cache-unsat-hits*)
                                    (return-from expanded-some-satisfiable-1-1
                                      (handle-clash-with-backtrack-state state nil nil nil nil)))
                                (progn
                                  (race-trace ("~&Cache hit for satisfiable constraints ~S ~S, label=~S~%"
                                               some-constraint all-constraints conclusions-key))
                                  (incf-statistics *tableaux-cache-sat-hits*)
                            ;(break)
                                  t))
                            (let ((new-labels
                                   (if self-concept-p
                                       labels
                                     (cons (make-basic-label-info
                                            :label-or-concept label
                                            :role (concept-role (constraint-term some-constraint)))
                                           labels))))
                              (when-statistics
                                (when tableaux-caching
                                  (incf-statistics *tableaux-cache-misses*)))
                              (multiple-value-bind
                                  (satisfiable partial-model-p merging-test-skipped)
                                  (if (and model-merging
                                           subtableaux-model-merging
                                           (not self-concept-p)
                                           (use-subtableaux-model-merging-1-p some-constraint)
                                           (every #'use-subtableaux-model-merging-1-p all-constraints))
                                      (constraints-mergable-p some-constraint
                                                              all-constraints
                                                              feature-related-some-constraints
                                                              relation-constraints
                                                              new-labels)
                                    (values nil t t))
                                (or (when satisfiable
                                      (race-trace ("~&models mergable for ~S and ~S~%"
                                                   some-constraint all-constraints))
                                      (when-statistics
                                        (when (and model-merging subtableaux-model-merging)
                                          (incf-statistics *mergable-models*)))
                                      (when (and tableaux-caching *tableaux-sat-caching*)
                                        (add-sat-model conclusions-key))
                                      t)
                                    (if partial-model-p
                                        (let ((conclusion-concept-constraints
                                               (expand-some-constraint some-all-list
                                                                       feature-related-some-constraints)))
                                          (unless self-concept-p
                                            (incf-statistics *model-depth*)
                                            (setf-statistics *max-model-depth*
                                                             (max (get-local-statistics-integer-value
                                                                   *max-model-depth*)
                                                                  (get-local-statistics-integer-value
                                                                   *model-depth*))))
                                          (if merging-test-skipped
                                              (race-trace ("~&Merging test skipped for ~S and ~S~%"
                                                           conclusion-concept-constraints some-constraint))
                                            (progn
                                              (incf-statistics *unmergable-partial-models*)
                                              (race-trace ("~&Partial models NOT mergable for ~S from ~S~%"
                                                           conclusion-concept-constraints some-constraint))))
                                          (if self-concept-p
                                              (setf self-conclusion-concept-constraints conclusion-concept-constraints)
                                            (multiple-value-bind
                                                (result blocking-used)
                                                (let ( ;(*expanded-model* nil)
                                        ;(*save-expanded-model* *tableaux-cached-models*)
                                                      (*save-expanded-model* nil)
                                                      (new-state
                                                       (make-basic-kernel-state :labels new-labels
                                                                                :partially-expanded-or-stack nil
                                                                                :parameters
                                                                                (state-parameters state))))
                                                  (when-debug t
                                                    (if feature-related-some-constraints
                                                        (race-trace ("~&Testing satisfiability of feature partition ~%~
                                                      ~S~%"
                                                                     (cons some-constraint
                                                                           feature-related-some-constraints)))
                                                      (race-trace ("~&Testing satisfiability of some-constraint ~S~%"
                                                                   some-constraint))))
                                                  (race-trace ("~&Testing satisfiability of subconstraint system ~%~
                                                  ~S, state=~S~%"
                                                               conclusion-concept-constraints new-state))
                                                  (cs-satisfiable-2 conclusion-concept-constraints
                                                                    nil
                                                                    +ind-counter-init+
                                                                    new-state
                                                                    nil ;disable expanded store
                                                                    ))
                                              (declare (ignore blocking-used))
                                              (when-debug t
                                                (if result
                                                    (race-trace ("~&Satisfiable subconstraint system ~S, state=~S~%"
                                                                 conclusion-concept-constraints state))
                                                  (race-trace ("~&Unsatisfiable subconstraint system ~S, state=~S~%"
                                                               conclusion-concept-constraints state)))
                                                (when feature-related-some-constraints
                                                  (if result
                                                      (race-trace ("~&Satisfiable feature partition ~S~%"
                                                                   (cons some-constraint
                                                                         feature-related-some-constraints)))
                                                    (race-trace ("~&Unsatisfiable feature partition ~S~%"
                                                                 (cons some-constraint
                                                                       feature-related-some-constraints))))))
                                              (decf-statistics *model-depth*)
                                              (if result
                                                  (when (and model-merging
                                                             subtableaux-model-merging
                                                             (not merging-test-skipped))
                                                    (incf-statistics *unmergable-models-satisfiable*))
                                                (let ((invalid-model-concepts
                                                       (when (or model-merging tableaux-caching)
                                                         (get-label-info-dependent-models (first new-labels)))))
                                                  (when-statistics
                                                    (when (and model-merging
                                                               subtableaux-model-merging
                                                               (not merging-test-skipped))
                                                      (incf-statistics *unmergable-models-unsatisfiable*)))
                                                  (when invalid-model-concepts
                                                    (invalidate-concept-models invalid-model-concepts))
                                                  (when feature-hierarchy-collapsed-p
                                                    ;;; add dependencies of related feature constraints
                                                    (add-clash-dependencies
                                                     (union-dependencies
                                                      (constraint-or-dependencies some-constraint)
                                                      (collect-dependencies
                                                       feature-related-some-constraints))))))
                                              (when tableaux-caching
                                                (if result
                                                    (when *tableaux-sat-caching*
                                                      (add-sat-model conclusions-key t)
                                                      (when blocking-possibly-required
                                                        (add-caching-dependencies new-labels)))
                                                  (when (and (first *tableaux-unsat-caching*)
                                                             (not (dl-qualified-number-restrictions *dl-prover-language*))
                                                             (not (constraint-signature some-constraint)))
                                                    (add-unsat-model conclusions-key *catching-clash-dependencies*))))
                                              (if result
                                                  t
                                                (return-from expanded-some-satisfiable-1-1
                                                  (handle-clash-with-backtrack-state state nil nil nil nil))))))
                                      (progn
                                        (race-trace ("~&models NOT mergable for ~S of conclusions ~S~%"
                                                     all-constraints some-constraint))
                                        (when-statistics
                                          (when (and model-merging subtableaux-model-merging)
                                            (incf-statistics *unmergable-det-models*)))
                                        (let ((invalid-model-concepts
                                               (when (or model-merging tableaux-caching)
                                                 (get-label-info-dependent-models (first new-labels)))))
                                          (when invalid-model-concepts
                                            (invalidate-concept-models invalid-model-concepts)))
                                        (when feature-hierarchy-collapsed-p
                                          ;;; add dependencies of related feature constraints
                                          (add-clash-dependencies
                                           (union-dependencies
                                            (constraint-or-dependencies some-constraint)
                                            (collect-dependencies
                                             feature-related-some-constraints))))
                                        (when tableaux-caching
                                          (when (and (first *tableaux-unsat-caching*)
                                                     (not (dl-qualified-number-restrictions *dl-prover-language*))
                                                     (not (constraint-signature some-constraint)))
                                            (add-unsat-model conclusions-key *catching-clash-dependencies*)))
                                        (return-from expanded-some-satisfiable-1-1
                                          (handle-clash-with-backtrack-state state nil nil nil nil))))))))))))))))))))
    (expanded-some-satisfiable-1-2 some-constraint
                                   state
                                   feature-related-some-constraints
                                   self-conclusion-concept-constraints
                                   nil)))

(defun expanded-some-satisfiable-1-2 (some-constraint
                                      state
                                      related-some-constraints
                                      self-conclusion-concept-constraints
                                      unused)
  (declare (ignore unused))
  (multiple-value-bind (new-unexpanded-exists-constraints
                        new-unexpanded-exists-constraints-store)
      (if related-some-constraints
          (remove-constraints-from-constraint-store
           related-some-constraints
           (state-unexpanded-exists-constraints state)
           (state-unexpanded-exists-constraints-store state)
           (state-copy-unexpanded-exists-constraints-store-p state)
           state
           #'reset-exists-copy)
        (values (state-unexpanded-exists-constraints state)
                (state-unexpanded-exists-constraints-store state)))
    (let* ((new-expanded-constraints
            (cons some-constraint
                  (append related-some-constraints
                          (state-expanded-constraints state))))
           (new-state
            (changed-kernel-state state
                                  :expanded-constraints new-expanded-constraints
                                  :unexpanded-exists-constraints
                                  new-unexpanded-exists-constraints
                                  :unexpanded-exists-constraints-store
                                  new-unexpanded-exists-constraints-store)))
      (added-constraints-satisfiable self-conclusion-concept-constraints
                                     nil ;no conclusion-relation-constraint
                                     new-state
                                     t nil))))

(defun sh-find-witness (label labels)
  (let ((result
         (loop for elem in labels
               when (and (true-label-info-p elem)
                         (concept-set-subsetp label (label-info-label-or-concept elem)))
               do (return elem))))
    (when-statistics
      (if result
        (incf-statistics *number-of-successful-sh-blocking-tests*)
        (incf-statistics *number-of-failed-sh-blocking-tests*)))
    result))

(defun use-subtableaux-model-merging-1-p (constraint)
  (let* ((concept (if (constraint-negated-p constraint)
                    (concept-negated-concept (constraint-term constraint))
                    (constraint-term constraint)))
         (role (concept-role concept))
         (term (or (and (exists-concept-p concept)
                        (concept-role-range concept))
                   (concept-term concept))))
    (and (or (concept-model term)
             (and (null *concept-model-in-progress*)
                  (or (and (negated-concept-p term)
                           (not (concept-self-referencing-p (concept-term term))))
                      (and (atomic-concept-p term)
                           (not (concept-self-referencing-p term)))
                      (cd-concept-p term))))
         (or (not (and (all-concept-p concept)
                       (or (role-transitive-p role)
                           (role-compositions role))))
             (concept-model concept)))))

(defun expanded-some-satisfiable-2 (some-constraint state unused-1 unused-2 unused-3)
  (declare (ignore unused-1 unused-2 unused-3))
  (incf-statistics *model-size*)
  (setf-statistics *max-model-size* (max (get-local-statistics-integer-value *max-model-size*)
                                         (get-local-statistics-integer-value *model-size*)))
  (multiple-value-bind
      (feature-related-some-constraints
       new-unexpanded-exists-constraints
       new-unexpanded-exists-constraint-store
       feature-hierarchy-collapsed-p)
      (get-related-some-constraints some-constraint
                                    (copy-basic-kernel-state state
                                                             (and (not *in-precompletion*)
                                                                  ':unexpanded-exists)))
    (let* ((role (concept-role (constraint-term some-constraint)))
           (feature-p (role-feature-p role)))
      (if (and (null feature-related-some-constraints)
               (or (constraint-merging-trigger-p some-constraint)
                   (not feature-p))
               (or (dl-merging *dl-prover-language*)
                   (and feature-p
                        (not *use-unique-name-assumption*)
                        (role-has-feature-ancestors-p role))))
          (let ((exists-partitions (at-most-constraint-violated-p some-constraint state)))
            (if exists-partitions
                (let ((new-state (if (end-of-precompletion-p exists-partitions)
                                     (commit-precompletion state exists-partitions)
                                   state))
                      (ind (constraint-ind some-constraint)))
                  (expanded-exists-partitions-satisfiable-2 ind
                                                            exists-partitions
                                                            new-state
                                                            nil nil))
              (let ((new-state
                     (changed-kernel-state state
                                           :unexpanded-exists-constraints
                                           new-unexpanded-exists-constraints
                                           :unexpanded-exists-constraints-store
                                           new-unexpanded-exists-constraint-store)))
                (expanded-some-satisfiable-2-1 some-constraint new-state nil nil nil))))
        (if (and feature-related-some-constraints
                 (disjoint-roles-clash-in-feature-exists-constraints-p (state-tbox (state-parameters state))
                                                                       (cons some-constraint
                                                                             feature-related-some-constraints)))
            (progn
              (set-clash-reasons (cons some-constraint feature-related-some-constraints))
              (set-clash-dependencies
               (collect-dependencies (cons some-constraint feature-related-some-constraints)))
              (set-signature-clash-reasons (cons some-constraint feature-related-some-constraints)
                                           *catching-clash-dependencies*)
              (race-trace ("~&Uunsatisfiable feature constraints ~S, dep=~S~%"
                           (cons some-constraint feature-related-some-constraints)
                           *catching-clash-dependencies*))
              (incf-statistics *tableaux-cache-unsat-hits*)
              (return-from expanded-some-satisfiable-2
                (handle-clash-with-backtrack-state state nil nil nil nil)))
          (let ((new-state
                 (changed-kernel-state state
                                       :unexpanded-exists-constraints new-unexpanded-exists-constraints
                                       :unexpanded-exists-constraints-store
                                       new-unexpanded-exists-constraint-store)))
            (expanded-some-satisfiable-2-1 some-constraint
                                           new-state
                                           feature-related-some-constraints
                                           feature-hierarchy-collapsed-p
                                           nil)))))))

(defun compute-new-labels (new-labels-1)
  (let (#+:debug (broken-blocks nil))
    (loop for label in (list (first new-labels-1))
          collect
          (cons label
                (let ((new-label (copy-label-info label)))
                  #+:debug (push label broken-blocks)
                  (setf (label-info-blocked-constraints new-label) nil)
                  (when (label-info-blocked-labels new-label)
                    (loop for blocked-label in (label-info-blocked-labels new-label) do
                          (setf (label-info-blocked-p blocked-label) nil))
                    (setf (label-info-blocked-labels new-label) nil))
                  new-label))
          into new-labels
          finally 
          (return (values (replace-modified-labels new-labels new-labels-1)
                          #+:debug broken-blocks)))))

(defun expanded-some-satisfiable-2-1 (some-constraint
                                      state
                                      feature-related-some-constraints
                                      feature-hierarchy-collapsed-p
                                      unused)
  (declare (ignore unused))
  (let* ((relation-store (when (and *use-relation-store*
                                    (not (relation-store-empty-p (state-relation-store state))))
                           (state-relation-store state)))
         (concept (constraint-term some-constraint))
         (self-concept-p (concept-self-reference-p concept)))
    (if (and *ignore-abox-redundant-exists*
             relation-store
             (not (constraint-merging-trigger-p some-constraint))
             (not (constraint-signature some-constraint))
             (not (exists-as-datatype-constraint-p some-constraint))
             (consp (state-expanded-store state))
             (some-concept-p concept)
             (role-successor-exists-p (constraint-ind some-constraint)
                                      (concept-role concept)
                                      (concept-term concept)
                                      (state-expanded-constraints state)
                                      (state-expanded-store state)
                                      (state-expanded-store-index state)
                                      relation-store))
        (progn
          (race-trace ("~&Ignoring ABox-implied some-constraint ~S" some-constraint))
        ;(break)
          (added-constraints-satisfiable nil ;no conclusion-concept-constraint
                                         nil ;no conclusion-relation-constraint
                                         state
                                         t nil))
      (let* ((relation-constraints (state-relation-constraints state))
             (some-all-list (get-related-all-or-some-constraints
                             (cons some-constraint
                                   feature-related-some-constraints)
                             state))
             (all-constraints
              (append feature-related-some-constraints
                      (reduce #'append some-all-list :key #'second))))
        (multiple-value-bind (label clash-constraints)
            (construct-label some-constraint all-constraints)
          (if (eq (first label) *bottom-concept*)
              (progn
                (set-clash-reasons (append (if clash-constraints
                                               clash-constraints
                                             (cons some-constraint all-constraints))
                                           (list relation-constraints)))
                (set-clash-dependencies
                 (if clash-constraints
                     (collect-dependencies clash-constraints)
                   (union-dependencies (constraint-or-dependencies some-constraint)
                                       (collect-dependencies all-constraints))))
                (if clash-constraints
                    (race-trace ("~&Encoding detected unsatisfiable constraints ~S~%"
                                 clash-constraints))
                  (race-trace ("~&Encoding detected unsatisfiable constraints ~S ~S~%"
                               some-constraint all-constraints)))
                (return-from expanded-some-satisfiable-2-1
                  (handle-clash-with-backtrack-state state nil nil nil nil)))
            (let* ((previous-ind (constraint-ind some-constraint))
                   (labels (state-labels state))
                   (role (concept-role (constraint-term some-constraint)))
                   (new-role (if feature-related-some-constraints
                                 (compute-feature-hierarchy-role-conjunct
                                  (cons some-constraint
                                        feature-related-some-constraints))
                               role))
                   (witness
                    (when (and (not (true-old-individual-p previous-ind))
                               (not self-concept-p)
                               (or *use-tbox* *encode-roles-as-transitive*))
                      (if (shiq-blocking-required *dl-prover-language*)
                          (shiq-find-label-witness new-role previous-ind label labels state nil (list some-constraint))
                        (shi-find-label-witness new-role label previous-ind labels state nil (list some-constraint))))))
            ;(break "~S" some-constraint)
              (if witness
                  (let* ((new-witness (copy-label-info witness))
                         (new-labels (replace-modified-labels (list (cons witness new-witness))
                                                              labels)))
                    (when-debug t
                      (if (shiq-blocking-required *dl-prover-language*)
                          (race-trace ("~&SHIQ-Blocking constraint ~S generating label ~S by witness ~S, state=~S~%"
                                       some-constraint label new-witness state))
                        (race-trace ("~&SHI-Blocking constraint ~S generating label ~S by witness ~S, state=~S~%"
                                     some-constraint label new-witness state))))
                    (setf *blocking-used* new-witness)
                    (pushnew some-constraint (label-info-blocked-constraints new-witness))
                    (when (or *tableaux-caching* *model-merging*)
                      (update-blocking-dependencies new-witness previous-ind new-labels))
                    (race-trace ("~%Blocking stack ~S~%" new-labels))
                    (multiple-value-bind (new-unexpanded-exists-constraints
                                          new-unexpanded-exists-constraints-store)
                        (if feature-related-some-constraints
                            (remove-constraints-from-constraint-store
                             feature-related-some-constraints
                             (state-unexpanded-exists-constraints state)
                             (state-unexpanded-exists-constraints-store state)
                             (state-copy-unexpanded-exists-constraints-store-p state)
                             state
                             #'reset-exists-copy)
                          (values (state-unexpanded-exists-constraints state)
                                  (state-unexpanded-exists-constraints-store state)))
                  ;(break "~S" some-constraint)
                      (let* ((new-expanded-constraints
                              (cons some-constraint
                                    (append feature-related-some-constraints
                                            (state-expanded-constraints state))))
                             (new-state
                              (changed-kernel-state state
                                                    :unexpanded-exists-constraints
                                                    new-unexpanded-exists-constraints
                                                    :expanded-constraints new-expanded-constraints
                                                    :labels new-labels
                                                    :unexpanded-exists-constraints-store
                                                    new-unexpanded-exists-constraints-store)))
                        (added-constraints-satisfiable nil ;no conclusion-concept-constraint
                                                       nil ;no conclusion-relation-constraint
                                                       new-state
                                                       t nil))))
                (multiple-value-bind
                    (new-labels-1 blocked-labels)
                    (if (and labels (not (true-old-individual-p previous-ind)))
                        (let* ((previous-ind-label (find-label-for-ind previous-ind labels))
                               (start-label (when previous-ind-label
                                              (list (first previous-ind-label)))))
                          (if (shiq-blocking-required *dl-prover-language*)
                              (shiq-find-newly-blocked-labels start-label labels state)
                            (shi-find-newly-blocked-labels start-label labels state)))
                      labels)
                ;(unless blocked-labels
                ;  (when (find-newly-blocked-labels new-labels-1) (break)))
                  (let* ((blocked-individuals (when blocked-labels
                                                (mapcar #'label-info-ind blocked-labels)))
                         (indirectly-blocked-individuals (state-indirectly-blocked-individuals state))
                         (new-indirectly-blocked-individuals
                          (if blocked-individuals
                              (union (get-obsolete-successors-of-inds blocked-individuals
                                                                      relation-constraints
                                                                      (state-relation-store state))
                                     (union blocked-individuals indirectly-blocked-individuals))
                            indirectly-blocked-individuals))
                         (violated-exists
                          (unless (true-old-individual-p previous-ind)
                            (get-violated-blocked-constraints previous-ind
                                                              (label-info-blocked-constraints
                                                               (first new-labels-1))
                                                              state)))
                         (new-labels-2
                          (if violated-exists
                              (multiple-value-bind (new-labels-2 #+:debug broken-blocks)
                                  (compute-new-labels new-labels-1)
                                #+:debug
                                (when broken-blocks
                                  (race-trace ("~&Some-unblocking blocked labels ~S~%" broken-blocks)))
                                new-labels-2)
                            new-labels-1)))
                    (when-debug (not (eq new-labels-1 new-labels-2))
                      (race-trace ("~&New unblocked labels ~S~%" new-labels-2)))
                    (let ((copied-violated-exists
                           (when violated-exists
                             (copy-violated-exists-constraints violated-exists))))
                      (multiple-value-bind
                          (new-expanded-constraints new-expanded-store new-expanded-store-index)
                          (if violated-exists
                              (remove-constraints-from-constraint-store violated-exists
                                                                        (state-expanded-constraints state)
                                                                        (state-expanded-store state)
                                                                        (state-copy-expanded-store-p state)
                                                                        state
                                                                        #'reset-expanded-copy
                                                                        (state-expanded-store-index state))
                            (values (state-expanded-constraints state)
                                    (state-expanded-store state)
                                    (state-expanded-store-index state)))
                        (let ((new-labels-3 (if blocked-individuals
                                                (remove-obsolete-label-infos
                                                 nil ; no obsolete-individuals
                                                 new-indirectly-blocked-individuals
                                                 new-labels-2)
                                              new-labels-2)))
                          (when-debug blocked-individuals
                            (race-trace ("~&Indirectly blocked inds=~S, blocked labels=~S~%"
                                         new-indirectly-blocked-individuals blocked-labels))
                            (race-trace ("Constraints (~S ~S ~S)~%"
                                         state
                                         new-labels-2
                                         new-indirectly-blocked-individuals)))
                          (if (member previous-ind new-indirectly-blocked-individuals)
                              (progn
                                (race-trace ("~&Ignoring indirectly blocked constraint ~S, blocked inds=~S~%"
                                             some-constraint new-indirectly-blocked-individuals))
                                (multiple-value-bind (new-unexpanded-exists-constraints
                                                      new-unexpanded-exists-constraints-store)
                                    (if feature-related-some-constraints
                                        (remove-constraints-from-constraint-store
                                         feature-related-some-constraints
                                         (state-unexpanded-exists-constraints state)
                                         (state-unexpanded-exists-constraints-store state)
                                         (state-copy-unexpanded-exists-constraints-store-p state)
                                         state
                                         #'reset-exists-copy)
                                      (values (state-unexpanded-exists-constraints state)
                                              (state-unexpanded-exists-constraints-store state)))
                              ;(break "~S" some-constraint)
                                  (let* ((new-expanded-constraints
                                          (cons some-constraint
                                                (append feature-related-some-constraints
                                                        new-expanded-constraints)))
                                         (new-state
                                          (changed-kernel-state state
                                                                :unexpanded-exists-constraints
                                                                new-unexpanded-exists-constraints
                                                                :expanded-constraints new-expanded-constraints
                                                                :labels new-labels-3
                                                                :indirectly-blocked-individuals
                                                                new-indirectly-blocked-individuals
                                                                :expanded-store new-expanded-store
                                                                :expanded-store-index new-expanded-store-index
                                                                :unexpanded-exists-constraints-store
                                                                new-unexpanded-exists-constraints-store)))
                                    (added-constraints-satisfiable copied-violated-exists
                                                                   nil ;no conclusion-relation-constraint
                                                                   new-state t nil))))
                            (let* ((next-ind-counter (incf *ind-counter-some-satisfiable*))
                                   (new-labels-4
                                    (cons (make-inverse-label-info :constraint some-constraint
                                                                   :label-or-concept label
                                                                   :role new-role
                                                                   :ind next-ind-counter
                                                                   :previous-ind previous-ind
                                                                   )
                                          new-labels-3)))
                              (multiple-value-bind
                                  (satisfiable partial-model-p merging-test-skipped)
                                  (if (and *model-merging*
                                           *subtableaux-model-merging*
                                           (not self-concept-p)
                                           (use-subtableaux-model-merging-2-p some-constraint)
                                           (every #'use-subtableaux-model-merging-2-p all-constraints))
                                      (progn
                                  ;(break)
                                        (constraints-mergable-p some-constraint
                                                                all-constraints
                                                                feature-related-some-constraints
                                                                relation-constraints
                                                                new-labels-4
                                                                (role-inverse-internal new-role)))
                                    (values nil t t))
                                (if satisfiable
                                    (progn
                                      (race-trace ("~&models mergable for ~S and ~S~%"
                                                   some-constraint all-constraints))
                                      (when (and *model-merging* *subtableaux-model-merging*)
                                        (incf-statistics *mergable-models*))
                                      (multiple-value-bind
                                          (new-unexpanded-exists-constraints
                                           new-unexpanded-exists-constraints-store)
                                          (if feature-related-some-constraints
                                              (remove-constraints-from-constraint-store
                                               feature-related-some-constraints
                                               (state-unexpanded-exists-constraints state)
                                               (state-unexpanded-exists-constraints-store state)
                                               (state-copy-unexpanded-exists-constraints-store-p state)
                                               state
                                               #'reset-exists-copy)
                                            (values (state-unexpanded-exists-constraints state)
                                                    (state-unexpanded-exists-constraints-store state)))
                                    ;(break "~S" some-constraint)
                                        (let* ((new-expanded-constraints
                                                (cons some-constraint
                                                      (append feature-related-some-constraints
                                                              new-expanded-constraints)))
                                               (new-state
                                                (changed-kernel-state state
                                                                      :unexpanded-exists-constraints
                                                                      new-unexpanded-exists-constraints
                                                                      :expanded-constraints
                                                                      new-expanded-constraints
                                                                      :labels new-labels-4
                                                                      :indirectly-blocked-individuals
                                                                      new-indirectly-blocked-individuals
                                                                      :expanded-store new-expanded-store
                                                                      :unexpanded-exists-constraints-store
                                                                      new-unexpanded-exists-constraints-store)))
                                          (added-constraints-satisfiable copied-violated-exists
                                                                         nil ;no conclusion-relation-constraint
                                                                         new-state t nil))))
                                  (if partial-model-p
                                      (let ((conclusion-concept-constraints
                                             (if self-concept-p
                                                 (expand-some-constraint some-all-list
                                                                         feature-related-some-constraints
                                                                         previous-ind)
                                               (expand-some-constraint some-all-list
                                                                       feature-related-some-constraints
                                                                       next-ind-counter))))
                                        (unless self-concept-p
                                          (incf-statistics *model-depth*)
                                          (setf-statistics *max-model-depth*
                                                           (max (get-local-statistics-integer-value 
                                                                 *max-model-depth*)
                                                                (get-local-statistics-integer-value 
                                                                 *model-depth*))))
                                        (if merging-test-skipped
                                            (race-trace ("~&Merging test skipped for ~S and ~S~%"
                                                         conclusion-concept-constraints some-constraint))
                                          (progn
                                            (incf-statistics *unmergable-partial-models*)
                                            (race-trace ("~&Partial models NOT mergable for ~S from ~S~%"
                                                         conclusion-concept-constraints some-constraint))))
                                        (when-debug t
                                          (if feature-related-some-constraints
                                              (race-trace ("~&Testing satisfiability of feature partition ~%~
                                                      ~S~%"
                                                           (cons some-constraint
                                                                 feature-related-some-constraints)))
                                            (race-trace ("~&Testing satisfiability of some-constraint ~S~%"
                                                         some-constraint))))
                                        (if self-concept-p
                                            (let* ((new-expanded-constraints
                                                    (cons some-constraint new-expanded-constraints))
                                                   (new-state
                                                    (changed-kernel-state
                                                     state
                                                     :expanded-constraints new-expanded-constraints
                                                     :labels new-labels-4
                                                     :indirectly-blocked-individuals
                                                     new-indirectly-blocked-individuals
                                                     :expanded-store new-expanded-store)))
                                              (race-trace ("~&Testing satisfiability of conclusion constrains ~S~%"
                                                           conclusion-concept-constraints))
                                              ;;(break "~S" some-constraint)
                                              (added-constraints-satisfiable conclusion-concept-constraints
                                                                             nil
                                                                             new-state nil nil))
                                          (let ((new-rel-constraint
                                                 (if feature-related-some-constraints
                                                     (concluded-feature-partition-inverse-relation-constraint
                                                      next-ind-counter
                                                      previous-ind
                                                      new-role
                                                      (cons some-constraint
                                                            feature-related-some-constraints))
                                                   (concluded-relation-constraint
                                                    next-ind-counter
                                                    previous-ind
                                                    some-constraint
                                                    (role-inverse-internal new-role)))))
                                            (loop for meta-constraint-concept in *meta-constraint-concepts*
                                                  for constraint =
                                                  (concluded-concept-constraint next-ind-counter
                                                                                meta-constraint-concept
                                                                                some-constraint
                                                                                feature-related-some-constraints)
                                                  do
                                                  (setf (constraint-meta-p constraint) t)
                                                  (push constraint conclusion-concept-constraints))
                                            (setf (constraint-successor-ind some-constraint)
                                                  next-ind-counter)
                                            (multiple-value-bind
                                                (new-unexpanded-exists-constraints
                                                 new-unexpanded-exists-constraints-store)
                                                (if feature-related-some-constraints
                                                    (remove-constraints-from-constraint-store
                                                     feature-related-some-constraints
                                                     (state-unexpanded-exists-constraints state)
                                                     (state-unexpanded-exists-constraints-store state)
                                                     (state-copy-unexpanded-exists-constraints-store-p state)
                                                     state
                                                     #'reset-exists-copy)
                                                  (values (state-unexpanded-exists-constraints state)
                                                          (state-unexpanded-exists-constraints-store state)))
                                              (let*
                                                  ((new-expanded-constraints
                                                    (cons some-constraint
                                                          (append feature-related-some-constraints
                                                                  new-expanded-constraints)))
                                                   (new-relation-store
                                                    (when *use-relation-store*
                                                      (if (state-relation-store state)
                                                          (update-relation-store nil
                                                                                 (list new-rel-constraint)
                                                                                 state)
                                                        (generate-relation-store (list new-rel-constraint)
                                                                                 nil))))
                                                   (new-state
                                                    (changed-kernel-state
                                                     state
                                                     :unexpanded-exists-constraints
                                                     new-unexpanded-exists-constraints
                                                     :expanded-constraints new-expanded-constraints
                                                     :labels new-labels-4
                                                     :indirectly-blocked-individuals
                                                     new-indirectly-blocked-individuals
                                                     :expanded-store new-expanded-store
                                                     :relation-store new-relation-store
                                                     :unexpanded-exists-constraints-store
                                                     new-unexpanded-exists-constraints-store)))
                                                (race-trace ("~&Testing satisfiability of subconstraint system ~%~
                                                      ~S (label ~S), rels = ~S, new rel-constraint ~S, state ~S~%"
                                                             (append copied-violated-exists
                                                                     conclusion-concept-constraints)
                                                             label
                                                             relation-constraints
                                                             new-rel-constraint
                                                             new-state))
                                                ;;(break "~S" some-constraint)
                                                (added-constraints-satisfiable (append copied-violated-exists
                                                                                       conclusion-concept-constraints)
                                                                               (unless *use-relation-store*
                                                                                 new-rel-constraint)
                                                                               new-state t nil))))))
                                    (progn
                                      (race-trace ("~&models NOT mergable for ~S of conclusions ~S~%"
                                                   all-constraints some-constraint))
                                      (when (and *model-merging* *subtableaux-model-merging*)
                                        (incf-statistics *unmergable-det-models*))
                                      (let ((invalid-model-concepts
                                             (when (or *model-merging* *tableaux-caching*)
                                               (get-label-info-dependent-models (first new-labels-3)))))
                                        (when invalid-model-concepts
                                          (invalidate-concept-models invalid-model-concepts)))
                                      (when feature-related-some-constraints
                                        (race-trace ("~&Unsatisfiable feature partition ~S~%"
                                                     (cons some-constraint
                                                           feature-related-some-constraints))))
                                      (when feature-hierarchy-collapsed-p
                                        ;;; add dependencies of related feature constraints
                                        (add-clash-dependencies
                                         (union-dependencies
                                          (constraint-or-dependencies some-constraint)
                                          (collect-dependencies
                                           feature-related-some-constraints))))
                                      (return-from expanded-some-satisfiable-2-1
                                        (handle-clash-with-backtrack-state state nil nil nil nil)))))))))))))))))))))

(defun use-subtableaux-model-merging-2-p (constraint)
  (let* ((concept (if (constraint-negated-p constraint)
                    (concept-negated-concept (constraint-term constraint))
                    (constraint-term constraint)))
         (term (or (and (exists-concept-p concept)
                        (concept-role-range concept))
                   (concept-term concept))))
    (and (or (concept-model term)
             (and (null *concept-model-in-progress*)
                  ;(not (member term *concept-model-in-progress*))
                  (or (negated-concept-p term)
                      (atomic-concept-p term)
                      (cd-concept-p term))))
         (or (not (and (all-concept-p concept)
                       (role-transitive-p (concept-role concept))))
             (concept-model concept)))))

(defun find-labels-for-successor-inds (retry-labels labels)
  (loop for label in labels
        when (and (true-label-info-p label)
                  (inverse-label-info-p label)
                  (not (label-info-blocked-p label))
                  (member (label-info-previous-ind label) retry-labels :key #'label-info-ind))
        collect label))

(defun successors-of-label-infos (retry-labels labels)
  (nconc (find-labels-for-successor-inds retry-labels labels) retry-labels))

(defun shiq-find-newly-blocked-labels (retry-labels
                                            labels
                                            state
                                            &optional (constraints nil))
  (loop with true-retry-labels = (successors-of-label-infos retry-labels labels)
        for label in true-retry-labels
        for previous-ind = (label-info-previous-ind label)
        for witness = (unless (or (old-individual-p previous-ind)
                                  (not (true-label-info-p label)))
                        (shiq-find-label-witness (label-info-role label)
                                                 (label-info-previous-ind label)
                                                 (label-info-label-or-concept label)
                                                 labels
                                                 state
                                                 (label-info-ind label)
                                                 constraints))
        when witness
        collect
        (cons witness
              (let ((new-witness (copy-label-info witness)))
                #+:debug (assert (label-info-constraint label))
                (pushnew (label-info-constraint label)
                         (label-info-blocked-constraints new-witness))
                (push label (label-info-blocked-labels new-witness))
                new-witness))
        into new-witnesses
        and collect
        (cons label
              (let ((new-label (copy-label-info label)))
                (setf (label-info-blocked-p new-label) t)
                ;(break "~S" label)
                new-label))
        into new-labels
        finally
        (let ((modified-labels (append new-labels new-witnesses)))
          (if modified-labels
            (progn
              ;(princ (length labels)) (princ " ")
              ;(break "~S" retry-labels)
              (return (values (replace-modified-labels modified-labels labels)
                              (mapcar #'cdr new-labels)
                              (set-difference retry-labels (mapcar #'car new-labels)))))
            (return (values labels nil retry-labels))))))

(defun shiq-find-label-witness (role
                                    predecessor-ind
                                    label
                                    labels
                                    state
                                    &optional (ind nil) (constraints nil))
  (let ((result (shiq-find-label-witness-1 role
                                           predecessor-ind
                                           label
                                           labels
                                           state
                                           ind
                                           constraints)))
    (when-statistics
      (unless result
        (incf-statistics *number-of-failed-shiq-blocking-tests*)))
    result))

(defun expand-role-of-concept (role all-or-some-term)
  (let* ((qualification (concept-term all-or-some-term))
         (all-role (concept-role all-or-some-term)))
    (when (member all-role (role-ancestors-internal role))
      (let* ((transitive-ancestors
              (and (not (eq all-role role))
                   (not (role-feature-p all-role))
                   (role-has-ancestors-p role)
                   (nset-difference (loop for role-1 in (role-ancestors-internal role)
                                          when (and (subrole-p role-1 all-role)
                                                    (role-transitive-p role-1))
                                          collect role)
                                    (remove-if #'role-not-transitive-p
                                               (role-ancestors-internal all-role)))))
             (result (progn
                       (when (and transitive-ancestors
                                  (role-transitive-p all-role)
                                  (member all-role (role-ancestors-internal role)))
                         (pushnew all-role transitive-ancestors))
                       (cond
                        ((rest transitive-ancestors)
                         (list* qualification
                                (loop for anc-role in transitive-ancestors
                                      collect
                                      (encode-concept-term (if (eq anc-role all-role)
                                                             all-or-some-term
                                                             (encode-concept-term
                                                              `(all ,anc-role ,qualification)))))))
                        ((role-transitive-p all-role)
                         (list qualification all-or-some-term))
                        ((all-concept-p all-or-some-term)
                         (list qualification))
                        (t
                         #+:debug (assert (role-feature-p all-role))
                         (list qualification)))))
             (role-range (role-range-restriction role)))
        (if role-range
          (cons role-range result)
          result)))))

(defun concept-label-subsetp (list1 list2)
  (loop for elem in (if (listp list1) list1 (list list1))
        always (if (and-concept-p elem)
                 (concept-set-subsetp (concept-term elem) list2)
                 (member elem list2))))

(defun not-atomic-or-negated-p (concept)
  (not (or (atomic-concept-p concept)
           (negated-concept-p concept))))

(defun shiq-find-label-witness-1 (role
                                       predecessor-ind
                                       label
                                       labels
                                       state
                                       ind
                                       constraints)
  (loop with top-concept = *top-concept*
        with no-features-p = (not (dl-features *dl-prover-language*))
        with full-label = (if ind
                            (collect-ind-label ind constraints state)
                            label)
        with next-labels = (find-label-for-ind predecessor-ind labels)
        with predecessor-label-info = (first next-labels)
        with predecessor-label = (when predecessor-label-info
                                   (concept-set-union
                                    (label-info-back-propagated-concepts predecessor-label-info)
                                    (collect-ind-label predecessor-ind constraints state)))
        with role-ancestors = (role-ancestors-internal role)
        with inverse-role-ancestors = (role-ancestors-internal (role-inverse-internal role))
        with condition-b6-known-p = nil
        with condition-b6-true-p = nil
        for witness = predecessor-label-info then predecessor-of-witness
        while witness
        for witness-previous-ind = (label-info-previous-ind witness)
        for remaining-labels = next-labels then next-predecessor-of-witness-labels
        for next-predecessor-of-witness-labels = (find-label-for-ind witness-previous-ind
                                                                     remaining-labels)
        for predecessor-of-witness = (first next-predecessor-of-witness-labels)
        for witness-label = (concept-set-union (label-info-back-propagated-concepts witness)
                                               (collect-ind-label (label-info-ind witness)
                                                                  constraints
                                                                  state))
        for cyclic-block-used = nil
        do 
        (if (and
             ;;; B1
             (concept-set-subsetp full-label witness-label)
             ;(or (break "~S" witness) t)
             (loop for concept in witness-label
                   when (quantification-concept-p concept)
                   do ;(break "~S" concept)
                   (cond
                    ;;; B2
                    ((or (all-concept-p concept)
                         (and (some-concept-p concept)
                              (role-feature-p (concept-role concept))))
                     (when (and (member (concept-role concept) inverse-role-ancestors)
                                (not (concept-label-subsetp
                                      (expand-role-of-concept (role-inverse-internal role) concept)
                                      predecessor-label)))
                       (when-statistics
                         (incf-statistics *number-of-weak-failed-shiq-blocking-tests*))
                       ;(break)
                       (return nil)))
                    ;;; B3 or (B5 and B6)
                    ((at-most-concept-p concept)
                     ;(break "B3 or (B5 and B6)")
                     (or nil ; nil used only for formatting purposes
                         ;;; c-blocked?
                         (and t ; t used only for formatting purposes
                              ;;; B5
                              (or (not (member (concept-role concept) inverse-role-ancestors))
                                  (unless (eq (concept-term concept) top-concept)
                                    (member (concept-negated-concept (concept-term concept))
                                            predecessor-label)))
                              ;;; B6
                              (or (and condition-b6-known-p condition-b6-true-p)
                                  (loop for concept in predecessor-label
                                        when (and (exists-concept-p concept)
                                                  (not (eq (concept-term concept) top-concept))
                                                  (member (concept-role concept) role-ancestors))
                                        do
                                        (unless (member (concept-negated-concept (concept-term concept))
                                                        full-label)
                                          (when-statistics
                                            (incf-statistics *number-of-weak-failed-shiq-blocking-tests*))
                                          ;(break)
                                          (setf condition-b6-known-p t)
                                          (setf condition-b6-true-p nil)
                                          (return nil))
                                        finally
                                        (setf condition-b6-known-p t)
                                        (setf condition-b6-true-p t)
                                        (setf cyclic-block-used t)
                                        ;(princ "*")
                                        (return t))))
                         ;;; B3-a
                         (unless (or (not (member (concept-role concept) inverse-role-ancestors))
                                     ;;; B3-b
                                     (unless (eq (concept-term concept) top-concept)
                                       (member (concept-negated-concept (concept-term concept))
                                               predecessor-label))
                                     ;;; B3-c
                                     (unless (eq (concept-term concept) top-concept)
                                       (member (concept-term concept) predecessor-label)))
                           (multiple-value-bind
                             (unused1 successors unused2 related-exists)
                             (get-exists-counts-from-concepts (concept-role concept)
                                                              witness-label
                                                              :qualification
                                                              (unless (eq (concept-term concept)
                                                                          top-concept)
                                                                (concept-term concept)))
                             (declare (ignore unused1 unused2))
                             (unless (< successors (concept-number-restriction concept))
                               (let ((diff (1+ (- successors
                                                  (concept-number-restriction concept)))))
                                 (loop for elem in related-exists
                                       when (or (eq (concept-term elem) top-concept)
                                                (concept-label-subsetp (concept-term elem)
                                                                       predecessor-label))
                                       do (decf diff)
                                       until (zerop diff))
                                 ;(break "~S" concept)
                                 (when (plusp diff)
                                   (when-statistics
                                     (incf-statistics *number-of-weak-failed-shiq-blocking-tests*))
                                   ;(break)
                                   (return nil)))))))))
                   finally
                   ;(break)
                   (return t))
             ;;; B4 is always fulfilled due to our tableau expansion strategy
             (or no-features-p
                 (no-feature-interaction-p (role-inverse-internal role)
                                           (remove-if-not #'exists-concept-p witness-label))))
          (progn
            #+:debug (assert (or (not *gc-of-obsolete-individuals*)
                                 (not (label-info-blocked-p witness))
                                 (not (eql *ignored-live-individuals-cycles* 0))))
            ;(format t "~&opt SHIQ succeeded for ind ~S, role ~S and ~S~%" predecessor-ind role back-propagation)
            ;(break)
            ;(when (zerop (label-info-ind witness)) (princ "+") '(break))
            (when-statistics
              (when (zerop (label-info-ind witness))
                (incf-statistics *number-of-successful-zero-shiq-blocking-tests*))
              (if cyclic-block-used
                (incf-statistics *number-of-successful-c-shiq-blocking-tests*)
                (incf-statistics *number-of-successful-a-shiq-blocking-tests*)))
            (return witness))
          (progn
            ;(format t "~&opt SHIQ failed for ind ~S, role ~S and ~S~%" predecessor-ind role back-propagation)
            ;(break)
            ))
        until (null predecessor-of-witness)
        finally
        #+:debug
        (progn
          ;(format t "~&opt SHIQ finally failed for ind ~S, role ~S and ~S~%" predecessor-ind role back-propagation)
          ;(break)
          )
        #-:debug (return nil)))

(defun no-feature-interaction-p (inverse-role exists-concepts)
  (when (and (role-feature-p inverse-role) (rest exists-concepts))
    (let ((ancestors-list
           (loop for concept in exists-concepts
                 for role = (concept-role concept)
                 when (and (subrole-p role inverse-role)
                           (role-feature-p role)
                           (role-has-feature-ancestors-p role))
                 collect (role-feature-ancestors role))))
      (loop for ancestors-rest on ancestors-list
            for first-ancestors = (first ancestors-rest) do
            (loop for ancestors in (rest ancestors-rest)
                  unless (lists-disjoint-p first-ancestors ancestors)
                  do (return-from no-feature-interaction-p nil)))))
  t)

#+(and :debug :ccl)
(defun find-newly-blocked-labels (labels state)
  (loop with shiq-blocking = (shiq-blocking-required *dl-prover-language*)
        for label in labels do
        (when (and (inverse-label-info-p label) (not (label-info-blocked-p label)))
          (multiple-value-bind
            (new-labels blocked-labels)
            (if shiq-blocking
              (shiq-find-newly-blocked-labels (list label) labels state)
              (shi-find-newly-blocked-labels (list label) labels state))
            (when (and blocked-labels (not (every #'label-info-blocked-p blocked-labels)))
              (inspect new-labels)
              (break "hier: ~S" blocked-labels)
              )))))

(defun shi-find-newly-blocked-labels (retry-labels labels state &optional (constraints nil))
  (loop for label in (successors-of-label-infos retry-labels labels)
        for previous-ind = (label-info-previous-ind label)
        for witness = (unless (or (true-old-individual-p previous-ind)
                                  (not (true-label-info-p label)))
                        (shi-find-label-witness (label-info-role label)
                                                (label-info-label-or-concept label)
                                                (label-info-previous-ind label)
                                                labels
                                                state
                                                (label-info-ind label)
                                                constraints))
        when witness
        collect
        (cons witness
              (let ((new-witness (copy-label-info witness)))
                #+:debug (assert (label-info-constraint label))
                (pushnew (label-info-constraint label)
                         (label-info-blocked-constraints new-witness))
                (push label (label-info-blocked-labels new-witness))
                new-witness))
        into new-witnesses
        and collect
        (cons label
              (let ((new-label (copy-label-info label)))
                (setf (label-info-blocked-p new-label) t)
                new-label))
        into new-labels
        finally
        (let ((modified-labels (append new-labels new-witnesses)))
          (if modified-labels
            (progn
              ;(break)
              (return (values (replace-modified-labels modified-labels labels)
                              (mapcar #'cdr new-labels)
                              (set-difference retry-labels (mapcar #'car new-labels)))))
            (return (values labels nil retry-labels))))))

(defun shi-find-label-witness (role label predecessor-ind labels state
                                        &optional (ind nil) (constraints nil))
  (let ((inv-role (role-inverse-internal role)))
    (labels ((not-matching-all-concept-p (concept)
               (not (and (all-concept-p concept)
                         (member inv-role (role-ancestors-internal (concept-role concept)))))))
      (let ((result
             (loop with full-label = (if ind
                                       (collect-ind-label ind constraints state)
                                       label)
                   with next-labels = (find-label-for-ind predecessor-ind labels)
                   with predecessor-label-info = (first next-labels)
                   with all-back-propagation = (remove-if #'not-matching-all-concept-p full-label)
                   with length = (length all-back-propagation)
                   for witness = predecessor-label-info then predecessor-of-witness
                   while witness
                   for witness-previous-ind = (label-info-previous-ind witness)
                   for remaining-labels = next-labels then next-predecessor-of-witness-labels
                   for next-predecessor-of-witness-labels = (find-label-for-ind witness-previous-ind
                                                                                remaining-labels)
                   for predecessor-of-witness = (first next-predecessor-of-witness-labels)
                   for witness-label = (collect-ind-label (label-info-ind witness)
                                                          constraints
                                                          state)
                   do
                   (when (concept-set-subsetp label witness-label)
                     (race-trace ("~&SHI-Blocking test-1 subsetp succeeded: ~S ~S ~S (~S)"
                                  witness label witness-label all-back-propagation))
                     (if (loop with count = 0
                               for concept in witness-label
                               do
                               (unless (not-matching-all-concept-p concept)
                                 (incf count)
                                 (when (or (> count length)
                                           (not (member concept all-back-propagation)))
                                   (return nil)))
                               finally
                               (if (eql count length)
                                 (return t)
                                 (return nil)))
                       (progn
                         (race-trace ("~&SHI-Blocking test-2 equal succeeded: ~S ~S ~S"
                                      witness all-back-propagation witness-label))
                         #+:debug (assert (or (not *gc-of-obsolete-individuals*)
                                              (not (label-info-blocked-p witness))
                                              (not (eql *ignored-live-individuals-cycles* 0))))
                         ;(princ "+")
                         ;(break)
                         (return witness))
                       (incf-statistics *number-of-weak-failed-shi-blocking-tests*)))
                   until (null predecessor-of-witness))))
        (when-statistics
          (if result
            (incf-statistics *number-of-successful-shi-blocking-tests*)
            (incf-statistics *number-of-failed-shi-blocking-tests*)))
        result))))

(defun update-blocking-dependencies (witness previous-ind labels)
  (loop with next-labels = (find-label-for-ind previous-ind labels)
        with previous-label = (first next-labels)
        for next-label = previous-label then previous-of-next-label
        while (and (not (eq next-label witness)) (inverse-label-info-p next-label))
        for remaining-labels = next-labels then next-previous-of-next-labels
        for next-previous-of-next-labels = (find-label-for-ind (label-info-previous-ind next-label)
                                                               remaining-labels)
        for previous-of-next-label = (first next-previous-of-next-labels)
        while previous-of-next-label
        do (add-label-info-dependent-model previous-of-next-label next-label)
        until (eq previous-of-next-label witness)))

(defun replace-modified-labels (modified-labels labels)
  (if modified-labels
    (loop for label in labels
          for replacement = (cdr (assoc label modified-labels))
          if replacement
          collect replacement
          else collect
          (loop with changed = nil
                with blocked-labels = (label-info-blocked-labels label)
                for blocked-label in blocked-labels
                for blocked-label-replacement = (cdr (assoc blocked-label modified-labels))
                if blocked-label-replacement
                do
                (unless changed
                  (setf changed t))
                and collect blocked-label-replacement into result 
                else collect blocked-label into result
                finally (return
                         (if changed
                           (let ((new-label (copy-label-info label)))
                             (setf (label-info-blocked-labels new-label) result)
                             new-label)
                           label))))
    labels))

;;;===========================================================================

(defstruct (symbolset-entry
            (:constructor make-symbolset-entry (concept-set constraint-set)))
  (concept-set nil)
  (constraint-set nil))

(defun unfold-symbol-constraints (state)
  (loop with concluded-constraints = nil
        with tbox = (state-tbox (state-parameters state))
        with tbox-elh-p = (subset-el+-p (tbox-language tbox))
        with in-abox-p = (and tbox-elh-p
                              (if *use-relation-store*
                                (not (relation-store-empty-p (state-relation-store state)))
                                (state-relation-constraints state)))
        with known-symbol-sets = nil
        with expanded-constraints = (state-expanded-constraints state)
        with expanded-store = (state-expanded-store state)
        with expanded-store-index = (state-expanded-store-index state)
        with symbol-constraints = (state-unexpanded-defined-det-constraints state)
        for symbol-constraint in symbol-constraints
        for constraint-term = (constraint-term symbol-constraint)
        do
        (if (constraint-negated-p symbol-constraint)
            (if (concept-primitive-p constraint-term)
                (let ((concept-encoded-negated-definition
                       (concept-encoded-negated-definition constraint-term)))
                  (if concept-encoded-negated-definition
                      (push (concluded-concept-constraint (constraint-ind symbol-constraint)
                                                          concept-encoded-negated-definition
                                                          symbol-constraint)
                            concluded-constraints)
                    ;; constraint-term was EL+ transformed and originally defined => unfold as negated defined
                    (push (concluded-concept-constraint (constraint-ind symbol-constraint)
                                                        (concept-negated-concept
                                                         (concept-encoded-definition constraint-term))
                                                        symbol-constraint)
                          concluded-constraints)))
              (push (concluded-concept-constraint (constraint-ind symbol-constraint)
                                                  (concept-negated-concept
                                                   (concept-encoded-definition constraint-term))
                                                  symbol-constraint)
                    concluded-constraints))
          (let ((ind (constraint-ind symbol-constraint)))
            (when (concept-encoded-definition constraint-term)
              (push (concluded-concept-constraint ind
                                                  (concept-encoded-definition constraint-term)
                                                  symbol-constraint)
                    concluded-constraints))
            (when (and in-abox-p (true-old-individual-p ind))
              (let ((elh-role-domain-qualifications
                     (concept-elh-role-domain-qualifications constraint-term)))
                (loop for (role . domain-qualifications) in elh-role-domain-qualifications
                      for predecessors = (get-role-predecessors ind role state)
                      do
                      (loop for ind in predecessors do
                            (loop for concept in domain-qualifications do
                                  (push (concluded-concept-constraint ind
                                                                      concept
                                                                      symbol-constraint)
                                        concluded-constraints))))))
            (when (and tbox
                       (concept-primitive-p constraint-term)
                       (concept-nary-unfold-sets constraint-term))
              (unless known-symbol-sets
                (setf known-symbol-sets (racer-make-hash-table :test 'equal :structure-p t)))
              (let* ((ind (constraint-ind symbol-constraint))
                     (entry (gethash ind known-symbol-sets)))
                (if entry
                  (progn
                    (push constraint-term (symbolset-entry-concept-set entry))
                    (push symbol-constraint (symbolset-entry-constraint-set entry)))
                  (let ((constraints
                          (collect-selected-constraints ind
                                                        (lambda (constraint)
                                                          (and (not (constraint-negated-p constraint))
                                                               (atomic-concept-p
                                                                (constraint-term constraint))))
                                                        expanded-constraints
                                                        expanded-store
                                                        expanded-store-index)))
                    (setf (gethash ind known-symbol-sets)
                          (make-symbolset-entry (mapcar #'constraint-term constraints)
                                                constraints))))))))
        finally
        (when known-symbol-sets
          (loop for entry being the hash-value of known-symbol-sets
                do
                (setf (symbolset-entry-concept-set entry)
                      (concept-set-remove-duplicates (symbolset-entry-concept-set entry)))
                (setf (symbolset-entry-constraint-set entry)
                      (constraint-set-remove-duplicates (symbolset-entry-constraint-set entry))))
          (loop with unfolded-symbol-sets = nil
                with nary-unfolding-table = (tbox-nary-absorption-table tbox)
                for symbol-constraint in symbol-constraints
                for constraint-term = (constraint-term symbol-constraint)
                for ind = (constraint-ind symbol-constraint)
                for entry = (gethash ind known-symbol-sets)
                for symbol-set = (when entry
                                   (symbolset-entry-concept-set entry))
                do
                (when entry
                  (loop for nary-unfold-set in (concept-nary-unfold-sets constraint-term) do
                        (when (and (concept-set-subsetp nary-unfold-set symbol-set)
                                   (or (null unfolded-symbol-sets)
                                       (not (gethash (cons ind nary-unfold-set)
                                                     unfolded-symbol-sets))))
                          (unless unfolded-symbol-sets 
                            (setf unfolded-symbol-sets
                                  (racer-make-hash-table :test 'equal :structure-p t)))
                          (setf (gethash (cons ind nary-unfold-set) unfolded-symbol-sets) t)
                          (push (concluded-concept-constraint
                                 ind
                                 (gethash nary-unfold-set nary-unfolding-table)
                                 (first (symbolset-entry-constraint-set entry))
                                 (rest (symbolset-entry-constraint-set entry)))
                                concluded-constraints)
                          (race-trace ("~&nary unfolding ~S with preconditions ~S to ~S~%"
                                       symbol-constraint 
                                       (symbolset-entry-constraint-set entry)
                                       (first concluded-constraints))))))))
        (return (nreverse concluded-constraints))))

(defun get-role-predecessors (ind role state)
  (if *use-relation-store*
    (relation-predecessor-individuals ind role (state-relation-store state))
    (loop for rel-constraint in (state-relation-constraints state)
          when (and (eql (constraint-ind-2 rel-constraint) ind)
                    (eq role (constraint-term rel-constraint)))
          collect (constraint-ind-1 rel-constraint))))

(defun unfolded-symbols-satisfiable (state unused-1 unused-2 unused-3 unused-4)
  (declare (ignore unused-1 unused-2 unused-3 unused-4))
  (let ((conclusion-concept-constraints (unfold-symbol-constraints state)))
    (race-trace ("~&unfolding ~S to ~S~%"
                 (state-unexpanded-defined-det-constraints state)
                 conclusion-concept-constraints))
    (let ((new-state (changed-kernel-state state :unexpanded-defined-det-constraints nil)))
      (added-constraints-satisfiable conclusion-concept-constraints
                                     nil ; no new relation constraint
                                     new-state
                                     t nil))))

;;; ======================================================================

(defun number-of-role-successors (ind-1 role relation-constraints state)
  (if *use-relation-store*
    (number-of-relation-fillers ind-1 role (state-relation-store state))
    (number-of-role-successors-old ind-1 role relation-constraints)))

(defun number-of-role-successors-old (ind-1 role relation-constraints)
  (if (null relation-constraints)
    0
    (loop with succ-list = nil
          with first-match = nil
          for rel-constraint in relation-constraints
          for current-role = (constraint-term rel-constraint) do
          (when (and (eql ind-1 (constraint-ind-1 rel-constraint))
                     (if (role-has-ancestors-p current-role)
                       (member role (role-ancestors-internal current-role))
                       (eq role current-role)))
            (unless first-match
              (setf first-match rel-constraint))
            (pushnew (constraint-ind-2 rel-constraint) succ-list))
          finally (return (values (length succ-list) first-match succ-list)))))

(defun expanded-at-most-satisfiable (at-most-constraint state unused-1 unused-2 unused-3)
  (declare (ignore unused-1 unused-2 unused-3))
  (let ((unexpanded-deterministic-constraints (state-unexpanded-deterministic-constraints state))
        (unexpanded-defined-det-constraints (state-unexpanded-defined-det-constraints state))
        (unexpanded-exists-constraints (state-unexpanded-exists-constraints state))
        (unexpanded-disjunctive-constraints (state-unexpanded-disjunctive-constraints state))
        (expanded-constraints (state-expanded-constraints state))
        (relation-constraints (state-relation-constraints state))
        #+(and :debug :lispworks) 
        (attribute-constraints (state-attribute-constraints state))
        #+(and :debug :lispworks) 
        (concrete-domain-state (state-concrete-domain-state state))
        (labels (state-labels state))
        (indirectly-blocked-individuals (state-indirectly-blocked-individuals state))
        (expanded-store (state-expanded-store state)))
    (let* ((concept (concept-negated-concept (constraint-term at-most-constraint)))
           #+:debug (role (concept-role concept))
           (no-restriction (concept-number-restriction concept))
           (qualification (concept-term concept))
           (ind (constraint-ind at-most-constraint)))
      #+:debug
      (unless (or (dl-merging *dl-prover-language*)
                  (and (not *use-unique-name-assumption*)
                       (role-feature-p role)
                       (role-has-feature-ancestors-p role)))
        (assert (<= no-restriction 1)))
      (multiple-value-bind
        (no-of-successors first-rel-constraint all-succ-inds)
        (count-role-successors ind
                               (make-bound (concept-role concept)
                                           (concept-number-restriction concept)
                                           at-most-constraint
                                           :qualification qualification)
                               nil
                               relation-constraints
                               expanded-constraints
                               state)
        (if (or (not *use-unique-name-assumption*)
                (not (eq qualification *top-concept*))
                (<= no-of-successors no-restriction))
          (let* ((trigger-constraint
                  (when (or (and (not (eq qualification *top-concept*))
                                 first-rel-constraint
                                 (or (null (constraint-signature first-rel-constraint))
                                     (not (member at-most-constraint
                                                  (signature-dependencies
                                                   (constraint-signature first-rel-constraint))))))
                            (and (eq qualification *top-concept*)
                                 (not *use-unique-name-assumption*)
                                 (> (length all-succ-inds)
                                    (concept-number-restriction concept))))
                    (concluded-concept-constraint
                     (constraint-ind-1 first-rel-constraint)
                     (without-taxonomic-encoding
                       (let ((role (constraint-term first-rel-constraint)))
                         (if (or (role-datatype role) (role-cd-attribute role))
                             (encode-concept-term `(d-at-least 1 ,role))
                           (encode-concept-term `(at-least 1 ,role)))))
                     at-most-constraint
                     first-rel-constraint)))
                 (violated-exists
                  (when (and (or *using-precompletion*
                                 (if *use-relation-store*
                                   (not (relation-store-empty-p (state-relation-store state)))
                                   relation-constraints))
                             (or *inverse-roles*
                                 (true-old-individual-p ind)))
                    (get-violated-exists-constraints ind
                                                     (concept-role concept)
                                                     state)))
                 (copied-violated-exists (when violated-exists
                                           (copy-violated-exists-constraints violated-exists))))
            ;(when trigger-constraint (break))
            (if (and (eq qualification *top-concept*)
                     (<= no-of-successors no-restriction))
              (race-trace ("~&at-most-constraint ~S satisfied: (<= ~D ~D), state=~S~%"
                           at-most-constraint no-of-successors no-restriction
                           (copy-basic-kernel-state-internal state)))
              (progn
                (race-trace ("~&at-most-constraint ~S possibly satisfied for ~D successors, state=~S~%"
                             at-most-constraint no-of-successors (copy-basic-kernel-state-internal state)))
                (when trigger-constraint
                  ;(setf (constraint-backpropagated-p trigger-constraint) t)
                  (setf (constraint-merging-trigger-p trigger-constraint) at-most-constraint)
                  (race-trace ("~&Adding trigger exists constraint ~S for ~S ~%"
                               trigger-constraint first-rel-constraint)))))
            (multiple-value-bind (obsolete-individuals remaining-relation-constraints new-store)
                                 (if (and violated-exists *inverse-roles*)
                                   (get-obsolete-individuals violated-exists
                                                             relation-constraints
                                                             state)
                                   (values nil relation-constraints (state-relation-store state)))
              ;(when copied-violated-exists (break "~S" at-most-constraint))
              (if obsolete-individuals
                (let ((new-unexpanded-deterministic-constraints
                       (remove-obsolete-constraints obsolete-individuals
                                                    unexpanded-deterministic-constraints))
                      (new-unexpanded-defined-det-constraints
                       (remove-obsolete-constraints obsolete-individuals
                                                    unexpanded-defined-det-constraints))
                      (unexpanded-exists-constraints-store
                       (state-unexpanded-exists-constraints-store state))
                      (unexpanded-disjunctive-constraints-store
                       (state-unexpanded-disjunctive-constraints-store state))
                      (new-labels (remove-obsolete-label-infos obsolete-individuals
                                                               indirectly-blocked-individuals
                                                               labels)))
                  (multiple-value-bind (new-expanded-constraints-1 new-expanded-store-1 new-expanded-store-index-1)
                                       (remove-obsolete-expanded-constraints obsolete-individuals
                                                                             expanded-constraints
                                                                             expanded-store
                                                                             (state-expanded-store-index state)
                                                                             state)
                    (multiple-value-bind
                      (new-expanded-constraints-2 new-expanded-store-2 new-expanded-store-index-2)
                      (remove-constraints-from-constraint-store violated-exists
                                                                new-expanded-constraints-1
                                                                new-expanded-store-1
                                                                (state-copy-expanded-store-p state)
                                                                state
                                                                #'reset-expanded-copy
                                                                new-expanded-store-index-1)
                      (multiple-value-bind
                        (new-unexpanded-exists-constraints new-unexpanded-exists-constraints-store)
                        (if (and obsolete-individuals 
                                 *use-unexpanded-exists-constraints-store*
                                 unexpanded-exists-constraints-store
                                 (not (constraint-store-unused-p unexpanded-exists-constraints-store)))
                          (remove-individuals-from-constraint-store obsolete-individuals
                                                                    unexpanded-exists-constraints
                                                                    unexpanded-exists-constraints-store
                                                                    (state-copy-unexpanded-exists-constraints-store-p
                                                                     state)
                                                                    state
                                                                    #'reset-exists-copy)
                          (values (remove-obsolete-constraints obsolete-individuals unexpanded-exists-constraints)
                                  unexpanded-exists-constraints-store))
                        (multiple-value-bind
                          (new-unexpanded-disjunctive-constraints new-unexpanded-disjunctive-constraints-store)
                          (if (and obsolete-individuals 
                                   *use-unexpanded-disjunctive-constraints-store*
                                   unexpanded-disjunctive-constraints-store
                                   (not (constraint-store-unused-p unexpanded-disjunctive-constraints-store)))
                            (remove-individuals-from-constraint-store obsolete-individuals
                                                                    unexpanded-disjunctive-constraints
                                                                    unexpanded-disjunctive-constraints-store
                                                                    (state-copy-unexpanded-disjunctive-constraints-store-p
                                                                     state)
                                                                    state
                                                                    #'reset-disjunctive-copy)
                            (values (remove-obsolete-constraints obsolete-individuals
                                                                 unexpanded-disjunctive-constraints)
                                    unexpanded-disjunctive-constraints-store))
                          ;(print 'retry-at-most)
                          ;(break)
                          (race-trace ("~&Retrying exists constraints ~S (~S)~%"
                                       copied-violated-exists relation-constraints))
                          (race-trace ("New constraints (~S ~S ~S ~S ~S ~S ~S ~S ~S ~S)~%"
                                       new-unexpanded-deterministic-constraints
                                       new-unexpanded-defined-det-constraints
                                       new-unexpanded-exists-constraints
                                       new-unexpanded-disjunctive-constraints
                                       new-expanded-constraints-2
                                       remaining-relation-constraints
                                       attribute-constraints
                                       concrete-domain-state
                                       new-labels
                                       indirectly-blocked-individuals)
                                      ("~&Removed obsolete individuals ~S~%" obsolete-individuals))
                          (let ((new-state
                                 (changed-kernel-state state
                                                       :unexpanded-deterministic-constraints
                                                       new-unexpanded-deterministic-constraints
                                                       :unexpanded-defined-det-constraints
                                                       new-unexpanded-defined-det-constraints
                                                       :unexpanded-exists-constraints
                                                       new-unexpanded-exists-constraints
                                                       :unexpanded-disjunctive-constraints
                                                       new-unexpanded-disjunctive-constraints
                                                       :expanded-constraints new-expanded-constraints-2
                                                       :relation-constraints remaining-relation-constraints
                                                       :labels new-labels
                                                       :expanded-store new-expanded-store-2
                                                       :expanded-store-index new-expanded-store-index-2
                                                       :relation-store new-store
                                                       :unexpanded-exists-constraints-store
                                                       new-unexpanded-exists-constraints-store
                                                       :unexpanded-disjunctive-constraints-store
                                                       new-unexpanded-disjunctive-constraints-store)))
                            (added-constraints-satisfiable (if trigger-constraint
                                                             (cons trigger-constraint copied-violated-exists)
                                                             copied-violated-exists)
                                                           nil ;no new relation constraint
                                                           new-state
                                                           t nil)))))))
                (multiple-value-bind
                  (new-expanded-constraints new-expanded-store new-expanded-store-index)
                  (remove-constraints-from-constraint-store violated-exists
                                                            expanded-constraints
                                                            expanded-store
                                                            (state-copy-expanded-store-p state)
                                                            state
                                                            #'reset-expanded-copy
                                                            (state-expanded-store-index state))
                  (when copied-violated-exists
                    (race-trace ("~&Retrying exists constraints ~S~%" copied-violated-exists)))
                  (let ((new-state
                         (changed-kernel-state state
                                               :expanded-constraints new-expanded-constraints
                                               :expanded-store new-expanded-store
                                               :expanded-store-index new-expanded-store-index
                                               :relation-store new-store)))
                    (added-constraints-satisfiable (if trigger-constraint
                                                     (cons trigger-constraint copied-violated-exists)
                                                     copied-violated-exists)
                                                   nil ;no new relation constraint
                                                   new-state
                                                   t
                                                   nil))))))
          (progn
            (race-trace ("~&at-most-constraint ~S violated: (> ~D ~D)~%"
                         at-most-constraint no-of-successors no-restriction)) 
            (handle-clash at-most-constraint
                          (constraint-or-dependencies at-most-constraint)
                          relation-constraints
                          first-rel-constraint
                          (constraint-or-dependencies first-rel-constraint))
            (handle-clash-with-backtrack-state state nil nil nil nil)))))))

;;;===========================================================================

(defun expanded-cd-constraint-satisfiable (new-constraint state unused-1 unused-2 unused-3)
  (declare (ignore unused-1 unused-2 unused-3))
  (let* ((relation-constraints (state-relation-constraints state))
         (attribute-constraints (state-attribute-constraints state))
         (ind (constraint-ind new-constraint))
         (new-predicate (concept-predicate (constraint-term new-constraint)))
         (parameters (predicate-parameters new-predicate))
         (cd-state (state-concrete-domain-state state)))
    (when (and cd-state
               (or (and (unary-string-predicate-p new-predicate)
                        (eq (predicate-operator new-predicate) 'string=))
                   (equal-predicate-p new-predicate))
               (some #'safe-datatype-role-p parameters))
      (loop with ind = (constraint-ind new-constraint)
            with datatype = (predicate-type new-predicate)
            for constraint in (solver-state-constraints cd-state)
            for predicate = (concept-predicate (constraint-term constraint))
            for pred-parameters = (predicate-parameters predicate)
            when (and (eql (constraint-ind constraint) ind)
                      (or (and (unary-string-predicate-p predicate)
                               (eq (predicate-operator predicate) 'string=))
                          (equal-predicate-p predicate))
                      (some #'safe-datatype-role-p pred-parameters)
                      (not (datatype-compatible-p datatype (predicate-type predicate) nil)))
            do 
            (race-trace ("~&constraint system ~S unsatisfiable due to datatype clash between ~S and ~S~%"
                         cd-state new-constraint constraint))
            (handle-clash new-constraint
                          (constraint-or-dependencies new-constraint)
                          relation-constraints
                          constraint
                          (constraint-or-dependencies constraint))
            (return-from expanded-cd-constraint-satisfiable
              (handle-clash-with-backtrack-state state nil nil nil nil))))
    (let* ((matching-ensure-cd-constraints
            (find-matching-ensure-cd-constraints ind parameters state))
           (bottom-constraints 
            (find-bottom-constraints matching-ensure-cd-constraints)))
      (if (eq (predicate-operator new-predicate) 'top)
          (if (null bottom-constraints)
              (let ((known-attribute-fillers (compute-known-attribute-fillers 
                                              ind parameters attribute-constraints)))
                (multiple-value-bind (attribute-fillers new-attribute-constraints) 
                    (compute-attribute-constraints-and-fillers
                     ind parameters known-attribute-fillers)
                  (declare (ignore attribute-fillers))
                  (let* ((all-attribute-constraints (append new-attribute-constraints
                                                            attribute-constraints))
                         (concluded-cd-constraints
                          (unless (some #'ensure-cd-constraint-p
                                        (constraint-dependencies new-constraint))
                            (compute-concluded-cd-constraints ind
                                                              new-constraint
                                                              matching-ensure-cd-constraints
                                                              all-attribute-constraints))))
                    (when concluded-cd-constraints
                      (race-trace ("~&Adding CD constraints ~S due to ~S and ~S~%"
                                   concluded-cd-constraints new-constraint
                                   matching-ensure-cd-constraints)))
                    (let ((new-state
                           (changed-kernel-state state
                                                 :attribute-constraints all-attribute-constraints)))
                      (added-constraints-satisfiable concluded-cd-constraints
                                                     nil ; no new relation constraint
                                                     new-state
                                                     t nil)))))
            (progn 
              (race-trace ("~&constraint system ~S unsatisfiable due to clash between ~S and ~S~%"
                           cd-state new-constraint bottom-constraints))
              (handle-clash new-constraint
                            (constraint-or-dependencies new-constraint)
                            relation-constraints
                            nil
                            nil
                            (collect-dependencies bottom-constraints))
              (handle-clash-with-backtrack-state state nil nil nil nil)))
        (if (null bottom-constraints)
            (let ((known-attribute-fillers (compute-known-attribute-fillers 
                                            ind parameters attribute-constraints)))
              (multiple-value-bind (attribute-fillers new-attribute-constraints) 
                  (compute-attribute-constraints-and-fillers
                   ind parameters known-attribute-fillers)
                (multiple-value-bind (entailed-p tmp-cd-state)
                    (if (and cd-state *keep-cd-state-minimal*)
                        (progn
                          (race-trace ("~&Testing entailment of constraint ~S by CD-state ~S~%"
                                       new-constraint 
                                       (copy-solver-state cd-state)))
                          (negated-constraint-entailed-p new-constraint
                                                         cd-state
                                                         attribute-fillers
                                                         t))
                      (values nil cd-state))
                  (when (stringp tmp-cd-state)
                    (error tmp-cd-state))
                  (if entailed-p
                      (let ((or-dependencies (constraint-or-dependencies new-constraint)))
                        (if or-dependencies
                            (let*
                                ((new-or-dependencies
                                  (union-dependencies
                                   or-dependencies
                                   (solver-state-entailed-predicate-or-dependencies cd-state)))
                                 (copy-cd-state
                                  (not
                                   (set-equal-p new-or-dependencies
                                                (solver-state-entailed-predicate-or-dependencies cd-state))))
                                 (new-state-1 (if copy-cd-state 
                                                  (copy-basic-kernel-state state t)
                                                state))
                                 (new-concrete-domain-state
                                  (if copy-cd-state
                                      (copy-solver-state cd-state)
                                    cd-state))
                                 (new-state-2
                                  (changed-kernel-state
                                   new-state-1
                                   :concrete-domain-state new-concrete-domain-state)))
                              (when copy-cd-state
                                (setf (solver-state-or-dependencies new-concrete-domain-state)
                                      (union-dependencies
                                       or-dependencies
                                       (solver-state-or-dependencies cd-state)))
                                (setf (solver-state-entailed-predicate-or-dependencies
                                       new-concrete-domain-state)
                                      new-or-dependencies))
                              (incf-statistics *successful-cd-predicate-entailed-tests*)
                              (race-trace ("~&Ignoring CD-constraint ~S entailed by CD-state ~S~%"
                                           new-constraint (copy-solver-state new-concrete-domain-state)))
                              (added-constraints-satisfiable nil ; no concluded constraints
                                                             nil ; no new relation constraint
                                                             new-state-2
                                                             t nil))
                          (progn
                            (incf-statistics *successful-cd-predicate-entailed-tests*)
                            (race-trace ("~&Ignoring CD-constraint ~S entailed by CD-state ~S~%"
                                         new-constraint (copy-solver-state cd-state)))
                            (added-constraints-satisfiable nil ; no concluded constraints
                                                           nil ; no new relation constraint
                                                           state
                                                           t nil))))
                    (let* ((new-or-dependencies
                            (if cd-state
                                (union-dependencies
                                 (constraint-or-dependencies new-constraint)
                                 (solver-state-or-dependencies cd-state))
                              (constraint-or-dependencies new-constraint)))
                           (copy-cd-state
                            (and cd-state
                                 (not (set-equal-p new-or-dependencies
                                                   (solver-state-or-dependencies cd-state))))))
                      (when (and cd-state *keep-cd-state-minimal*)
                        (race-trace ("~&Failed entailment of constraint ~S by CD-state ~S~%"
                                     new-constraint (copy-solver-state cd-state)))
                        (incf-statistics *failed-cd-predicate-entailed-tests*))
                      (let ((cd-constraint (make-constraint new-predicate attribute-fillers)))
                        (flet ((expanded-cd-constraint-satisfiable-continuation-1 (sat-p
                                                                                   state
                                                                                   unused-1
                                                                                   unused-2
                                                                                   unused-3)
                                 (declare (ignore unused-1 unused-2 unused-3))
                                 #-:debug (declare (ignore sat-p))
                                 #+:debug (assert sat-p)
                                 (let ((new-cd-state (state-concrete-domain-state state)))
                                   (when (stringp new-cd-state)
                                     (error new-cd-state))
                                   (push cd-constraint (solver-state-cd-constraints new-cd-state))
                                   (push new-constraint
                                         (solver-state-constraints new-cd-state))
                                   (when (nonlinear-predicate-p new-predicate)
                                     (setf (solver-state-contains-additional-constraints new-cd-state)
                                           t))
                                   (race-trace ("~&Adding constraint ~S to CD-state ~S => ~S~%"
                                                new-constraint 
                                                (copy-solver-state cd-state)
                                                (copy-solver-state new-cd-state)))
                                   (let* ((all-attribute-constraints (append new-attribute-constraints
                                                                             attribute-constraints))
                                          (concluded-cd-constraints
                                           (unless (some #'ensure-cd-constraint-p
                                                         (constraint-dependencies new-constraint))
                                             (compute-concluded-cd-constraints ind
                                                                               new-constraint
                                                                               matching-ensure-cd-constraints
                                                                               all-attribute-constraints))))
                                     (when concluded-cd-constraints
                                       (race-trace ("~&Adding constraints ~S due to ~S and ~S~%"
                                                    concluded-cd-constraints new-constraint
                                                    matching-ensure-cd-constraints)))
                                     (let* ((new-state-1 (copy-basic-kernel-state state t))
                                            (new-concrete-domain-state
                                             (if (or (null cd-state) copy-cd-state)
                                                 new-cd-state
                                               cd-state))
                                            (new-state-2
                                             (changed-kernel-state
                                              new-state-1
                                              :attribute-constraints all-attribute-constraints
                                              :concrete-domain-state new-concrete-domain-state)))
                                       (added-constraints-satisfiable concluded-cd-constraints
                                                                      nil ; no new relation constraint
                                                                      new-state-2
                                                                      t nil)))))
                               (expanded-cd-constraint-satisfiable-continuation-2 (sat-p
                                                                                   new-state
                                                                                   unused-1
                                                                                   unused-2
                                                                                   unused-3)
                                 (declare (ignore unused-1 unused-2 unused-3))
                                 (or sat-p
                                     (let ((new-cd-state (state-concrete-domain-state new-state)))
                                       (when (stringp new-cd-state)
                                         (error new-cd-state))
                                       (race-trace ("~&constraint system ~S becomes unsatisfiable ~
                                                 by adding ~S~%"
                                                    new-cd-state new-constraint))
                                       (when new-cd-state
                                         (handle-cd-clash
                                          new-constraint
                                          (solver-state-constraints new-cd-state)
                                          (solver-state-cd-constraints new-cd-state)
                                          (union-dependencies
                                           (constraint-or-dependencies new-constraint)
                                           (union-dependencies
                                            (solver-state-entailed-predicate-or-dependencies
                                             new-cd-state)
                                            (union-dependencies
                                             *catching-clash-dependencies*
                                             (compute-constraint-dependencies new-cd-state))))
                                          relation-constraints))
                                       (handle-clash-with-backtrack-state state nil nil nil nil)))))
                          (let* ((new-state-1 (copy-basic-kernel-state state t))
                                 (new-backtrack-stack
                                  (push-backtrack-stack #'expanded-cd-constraint-satisfiable-continuation-2
                                                        (state-partially-expanded-or-stack new-state-1)))
                                 (new-state-2
                                  (changed-kernel-state new-state-1
                                                          ;:concrete-domain-state cd-state
                                                        :partially-expanded-or-stack 
                                                        new-backtrack-stack)))
                            (race-trace ("~&Saving backtrack closure ~S in stack ~S of state ~S~%"
                                         #'expanded-cd-constraint-satisfiable-continuation-2
                                         (copy-seq new-backtrack-stack)
                                         new-state-2))
                            (constraint-satisfiable-p-2 cd-constraint
                                                        new-state-2
                                                        copy-cd-state
                                                        new-or-dependencies
                                                        #'expanded-cd-constraint-satisfiable-continuation-1)))))))))
          (progn 
            (race-trace ("~&constraint system unsatisfiable due to ~S and ~S~%"
                         new-constraint bottom-constraints))
            (handle-clash new-constraint
                          (constraint-or-dependencies new-constraint)
                          relation-constraints
                          (first bottom-constraints)
                          (collect-dependencies bottom-constraints))
            (handle-clash-with-backtrack-state state nil nil nil nil)))))))

(defun ensure-cd-constraint-p (constraint)
  (when (concept-constraint-p constraint)
    (and (constraint-negated-p constraint)
         (cd-concept-p (constraint-term constraint)))))


(defun compute-constraint-dependencies (concrete-domain-state)
  (let ((result nil))
    (loop for constraint in (if (null concrete-domain-state)
                              nil
                              (solver-state-constraints concrete-domain-state)) do
          #+:debug (assert (cd-concept-p (constraint-term constraint)))
          (setf result (union-dependencies (constraint-or-dependencies constraint)
                                           result)))
    result))

    
(defun compute-known-attribute-fillers (ind parameters attribute-constraints)
  (loop for parameter in parameters
        collect (find-filler ind parameter attribute-constraints)))

(defun find-filler (ind parameter attribute-constraints)
  (loop for attribute-constraint in attribute-constraints
        when (and (eql (first attribute-constraint) ind)
                  (eq (if *role-store*
                        (role-name (get-role (second attribute-constraint)))
                        (second attribute-constraint))
                      parameter))
        do (return (third attribute-constraint))))

(defun compute-attribute-constraints-and-fillers (ind parameters attribute-fillers)
  (let ((new-attribute-constraints nil)
        (final-attribute-fillers nil))
    (loop for attribute-filler in attribute-fillers
          for parameter in parameters do
          (if (null attribute-filler)
            (let ((attribute-filler (gensym "O")))
              (push (list ind parameter attribute-filler) new-attribute-constraints)
              (push attribute-filler final-attribute-fillers))
            (push attribute-filler final-attribute-fillers)))
    (values (nreverse final-attribute-fillers)
            (nreverse new-attribute-constraints))))

(defun find-non-bottom-constraints (ind attribute state)
  (collect-selected-constraints ind
                                (lambda (constraint)
                                  (let ((concept (constraint-term constraint)))
                                    (and (cd-concept-p concept)
                                         (not (constraint-negated-p constraint))
                                         (member attribute
                                                 (predicate-parameters (concept-predicate concept))))))
                                (rest (state-expanded-constraints state))
                                (state-expanded-store state)
                                (state-expanded-store-index state)))

(defun non-bottom-cd-constraint-p (ind attributes state)
  (ind-selected-constraints-p ind
                              (lambda (constraint)
                                (let ((concept (constraint-term constraint)))
                                  (and (cd-concept-p concept)
                                       (not (constraint-negated-p constraint))
                                       (subsetp attributes
                                                (predicate-parameters (concept-predicate concept))))))
                              (rest (state-expanded-constraints state))
                              (state-expanded-store state)
			      (state-expanded-store-index state)))

(defun find-bottom-constraints (constraints)
  (loop for constraint in constraints
        when (and (constraint-negated-p constraint)
                  (eq (predicate-operator (concept-predicate (constraint-term constraint)))
                      'top))
        collect constraint))

(defun find-matching-ensure-cd-constraints (ind attributes state)
  (collect-selected-constraints ind
                                (lambda (constraint)
                                  (and (constraint-negated-p constraint)
                                       (let ((concept (constraint-term constraint)))
                                         (and (cd-concept-p concept)
                                              (not (lists-disjoint-p (predicate-parameters
                                                                      (concept-predicate concept))
                                                                     attributes))))))
                                (state-expanded-constraints state)
                                (state-expanded-store state)
				(state-expanded-store-index state)))

(defun compute-concluded-cd-constraints (ind
                                              cd-constraint
                                              matching-ensure-cd-constraints
                                              attribute-constraints)
  (loop for constraint in matching-ensure-cd-constraints
        for cd-concept = (concept-term (concept-negated-concept (constraint-term constraint)))
        for parameters = (predicate-parameters (concept-predicate cd-concept))
        for known-attribute-fillers = (compute-known-attribute-fillers ind
                                                                       parameters
                                                                       attribute-constraints)
        unless (some #'null known-attribute-fillers)
        collect (concluded-concept-constraint
                 ind
                 cd-concept
                 constraint
                 cd-constraint)))

;;;===========================================================================

(defun expanded-ensure-cd-constraint-satisfiable (new-constraint state unused-1 unused-2 unused-3)
  (declare (ignore unused-1 unused-2 unused-3))
  (multiple-value-bind (result constraint)
                       (expanded-ensure-cd-constraint-satisfiable-1 new-constraint state)
    (if result
      (added-constraints-satisfiable constraint
                                     nil ; no new relation constraint
                                     state
                                     t nil)
      (handle-clash-with-backtrack-state state nil nil nil nil))))

(defun expanded-ensure-cd-constraint-satisfiable-1 (new-constraint state)
  (let ((relation-constraints (state-relation-constraints state))
        (attribute-constraints (state-attribute-constraints state)))
    (let* ((ind (constraint-ind new-constraint))
           (concept (constraint-term new-constraint))
           (predicate (concept-predicate concept))
           (parameters (predicate-parameters predicate)))
      (if (eq (predicate-operator predicate) 'top)
        (let ((non-bottom-constraints 
               (find-non-bottom-constraints ind (first parameters) state)))
          (if (null non-bottom-constraints)
            (let ((attribute-culprit
                   (find (first parameters) attribute-constraints
                         :test (lambda (elem-1 elem-2)
                                 (and (eql ind (first elem-2))
                                      (eq elem-1 (second elem-2)))))))
              (if attribute-culprit
                (progn 
                  (race-trace ("~&constraint system unsatisfiable due to predicate ~S from ~S ~
                                and attribute assertion ~S~%"
                               predicate new-constraint attribute-culprit))
                  (handle-clash new-constraint
                                (constraint-or-dependencies new-constraint)
                                relation-constraints)
                  nil)
                (values t nil)))
            (progn 
              (race-trace ("~&constraint system unsatisfiable due to predicate ~S from ~S~%"
                           predicate new-constraint))
              (handle-clash new-constraint
                            (constraint-or-dependencies new-constraint)
                            relation-constraints
                            nil
                            nil
                            (collect-dependencies non-bottom-constraints))
              nil)))
        (let* ((known-attribute-fillers
                (compute-known-attribute-fillers ind parameters attribute-constraints))
               (new-cd-constraint
                (when (or (not (some #'null known-attribute-fillers))
                          (non-bottom-cd-constraint-p ind parameters state))
                  (if (member (predicate-operator predicate) '(bottom top))
                      (concluded-concept-constraint ind
                                                    (concept-negated-concept concept)
                                                    new-constraint)
                    (concluded-concept-constraint ind
                                                  (concept-term (concept-negated-concept concept))
                                                  new-constraint)))))
          (when new-cd-constraint
            #+:debug (assert (if (member (predicate-operator predicate) '(bottom top))
                                 (cd-concept-p (concept-negated-concept concept))
                                 (ensure-cd-concept-p (concept-negated-concept concept))))
            (race-trace ("~&Expanding ensure-constraint ~S to ~S (fillers=~S,attributes=~S)~%"
                         new-constraint new-cd-constraint
                         known-attribute-fillers attribute-constraints)))
          (values t new-cd-constraint))))))

(defun expanded-general-cd-constraint-satisfiable (new-constraint state unused-1 unused-2 unused-3)
  (declare (ignore unused-1 unused-2 unused-3))
  (if (is-bottom-datatype-concept-p (constraint-concept new-constraint))
      (progn 
        (race-trace ("~&constraint system unsatisfiable due to empty data range ~S from ~S~%"
                     (constraint-concept new-constraint) new-constraint))
        (handle-clash new-constraint
                      (constraint-or-dependencies new-constraint)
                      (state-relation-constraints state))
        nil)
    (added-constraints-satisfiable nil ; no new constraint
                                   nil ; no new relation constraint
                                   state
                                   t nil)))

;;;===========================================================================

(defun elh-subsumption-test-p (subsumer subsumee)
  (and *use-elh-model-embedding*
       (null *meta-constraint-concepts*)
       (let* ((atomic-subsumer-p (atomic-concept-p subsumer))
              (bucket-subsumer-p (unless atomic-subsumer-p
                                   (concepts-bucket-p subsumer)))
              (tbox *use-tbox*))
         (and (not (if atomic-subsumer-p
                       (or (concept-encoded-negated-definition subsumer)
                           (concept-self-referencing-p subsumer)
                           ;; we might not know whether subsumer has aliases (without classification)
                           ;; we only allow defined concepts if prover language is ELH
                           (not (or (concept-primitive-p subsumer)
                                    (subset-el+-p *dl-prover-language*)))
                           (and tbox
                                (not (tbox-el+-transformed-table tbox))
                                (sufficient-condition-in-nary-absorption-table-p tbox subsumer))
                           )
	             (when (and bucket-subsumer-p
                                *blocking-possibly-required*)
		       (some #'concept-self-referencing-p (bucket-members subsumer)))))
              (not (and (atomic-concept-p subsumee) (concept-self-referencing-p subsumee)))
              (and (if bucket-subsumer-p
                       (and (subset-el+-p (bucket-members-language subsumer))
                            (or (not (bucket-member-defined-p subsumer))
                                (subset-el+-p *dl-prover-language*)))
                     (subset-el+-p (concept-language subsumer)))
	           (subset-el+-p (concept-language subsumee))
	           (or (not *random-concept-list*)
		       (and (or bucket-subsumer-p
		                (not (cyclic-elh-model-p (get-cached-concept-model subsumer))))
		            (not (cyclic-elh-model-p (get-cached-concept-model subsumee))))))))))

(defun cyclic-elh-model-p (model)
  (when (model-info-p model) 
    (or (model-blocked-p model)
        (loop for exists in (model-exists-models model)
              for qualification = (concept-term exists)
              thereis (or (and (atomic-concept-p qualification)
                               (concept-self-referencing-p qualification))
                          (cyclic-elh-model-p (get-cached-concept-model qualification)))))))

(defun obviously-subsumes (subsumer subsumee)
  (let ((bottom *bottom-concept*))
    (if (or (eq subsumee bottom)
            (and (atomic-concept-p subsumee)
                 (eq (concept-encoded-definition subsumee) bottom))
            (and (concept-p-internal subsumer)
                 (or (eq subsumer *top-concept*)
		     (and (atomic-concept-p subsumer)
		          (eq (concept-encoded-negated-definition subsumer) bottom))
		     (and (negated-concept-p subsumer)
		          (eq (concept-encoded-definition (concept-term subsumer)) bottom)))))
      t
      (if (elh-subsumption-test-p subsumer subsumee)
        (if (eq subsumer bottom)
          nil
          (if *model-merging*
            (if (concepts-bucket-p subsumer)
              (if (atomic-concept-p subsumee)
                (model-of-bucket-embeddable-p (bucket-members subsumer)
                                              (get-cached-concept-model subsumee))
                (progn
                  (incf-statistics *model-subsumption-tests*)
                  (model-embeddable-p (get-cached-concept-model (bucket-concept subsumer))
                                      (get-cached-concept-model subsumee))))
              (progn
                (incf-statistics *model-subsumption-tests*)
                (model-embeddable-p (get-cached-concept-model subsumer)
                                    (get-cached-concept-model subsumee))))
            (values nil t)))
        (let ((subsumer (if (concepts-bucket-p subsumer)
                          (bucket-concept subsumer)
                          subsumer)))
          (if (or (incoherent-model-p
                   (get-cached-concept-model (concept-negated-concept subsumer)))
                  (incoherent-model-p (get-cached-concept-model subsumee)))
            t
            (values nil t)))))))

(defun obviously-not-subsumes (subsumer subsumee)
  #+:debug (assert (not (and *model-merging* (elh-subsumption-test-p subsumer subsumee))))
  (with-alc-language-settings (
                               :concept-1 (concept-negated-concept subsumer)
                               :concept-2 subsumee)
    (cond
     ((and (eq subsumer *bottom-concept*) 
           (not (incoherent-model-p (get-cached-concept-model subsumee))))
      (incf-statistics *obvious-subsumptions*)
      t)
     ((and (eq subsumee *top-concept*) 
           (not (incoherent-model-p
                 (get-cached-concept-model (concept-negated-concept subsumer)))))
      (incf-statistics *obvious-subsumptions*)
      t)
     (t (if *model-merging*
            (progn
              (incf-statistics *model-subsumption-tests*)
              (concept-not-subsumes subsumer subsumee))
          (values nil t))))))

(defun create-new-encoded-concept (original concept-term negated-concept-term)
  (let ((new-concept (copy-concept-internal original))
        (new-negated-concept (copy-concept-internal (concept-negated-concept original))))
    (setf (concept-term new-concept) negated-concept-term)
    (setf (concept-term new-negated-concept) concept-term)
    (setf (concept-negated-concept-internal new-concept) new-negated-concept)
    (setf (concept-negated-concept-internal new-negated-concept) new-concept)
    (setf (concept-model new-concept) nil)
    (setf (concept-model new-negated-concept) nil)
    new-concept))

(defun create-bcp-resolved-negated-subsumer (encoded-subsumer encoded-subsumee)
  (with-alc-language-settings (
                               :concept-1 (concept-negated-concept encoded-subsumer)
                               :concept-2 encoded-subsumee)
    (loop with negated-subsumer = (concept-negated-concept encoded-subsumer)
          for subsumer-term in (concept-term encoded-subsumer)
          for neg-term in (concept-term negated-subsumer)
          with modified = nil
          with subsumee-terms = (if (atomic-concept-p encoded-subsumee)
                                  (list encoded-subsumee)
                                  (concept-term encoded-subsumee))
          if (member neg-term subsumee-terms :test #'concept-clash-p)
          do (setf modified t)
          else
          collect neg-term into neg-terms and
          collect subsumer-term into terms
          finally
          (return
           (if modified
             (cond
              ((null neg-terms) nil)
              ((null (rest neg-terms)) (first neg-terms))
              (t (create-new-encoded-concept negated-subsumer
                                             terms
                                             neg-terms)))
             negated-subsumer)))))

(race-inline (get-encoded-definition))

(defun get-encoded-definition (concept)
  (if (concept-primitive-p concept)
    concept
    (concept-encoded-definition concept)))

(defun test-bcp-reduced-subsumes (subsumer subsumee) ;(break)
  (let ((encoded-subsumer (get-encoded-definition subsumer))
        (negated-subsumer (concept-negated-concept subsumer))
        (encoded-subsumee (get-encoded-definition subsumee)))
    (multiple-value-prog1
      (if (and (not (and *model-merging* *use-alternate-models*))
               (and-concept-p encoded-subsumer)
               (or (atomic-concept-p encoded-subsumee)
                   (and-concept-p encoded-subsumee)))
        (let ((new-negated-subsumer
               (create-bcp-resolved-negated-subsumer encoded-subsumer
                                                     encoded-subsumee)))
          (cond ((null new-negated-subsumer)
                 (incf-statistics *obvious-subsumptions*)
                 t)
                ((eq new-negated-subsumer (concept-negated-concept encoded-subsumer))
                 (test-subsumes-2 subsumer subsumee
                                  negated-subsumer encoded-subsumee))
                (t (test-subsumes-2 subsumer subsumee
                                    new-negated-subsumer encoded-subsumee))))
        (test-subsumes-2 subsumer subsumee negated-subsumer encoded-subsumee))
      #|(when nil ;*derived-models*
        (when (and (not (eq subsumer encoded-subsumer))
                   (concept-model encoded-subsumer)
                   (null (concept-model subsumer)))
          (if (concept-primitive-p subsumer)
            (setf (concept-model subsumer)
                  (copy-to-primitive-model encoded-subsumer subsumer))
            (setf (concept-model subsumer)
                  (make-model-copy (concept-model encoded-subsumer) subsumer))))
        (when (and (not (eq subsumee encoded-subsumee))
                   (concept-model encoded-subsumee)
                   (null (concept-model subsumee)))
          (if (concept-primitive-p subsumee)
            (setf (concept-model subsumee)
                  (copy-to-primitive-model encoded-subsumee subsumee))
            (setf (concept-model subsumee)
                  (make-model-copy (concept-model encoded-subsumee)
                                   subsumee)))))|#)))

(defun test-subsumes-2 (subsumer subsumee negated-subsumer encoded-subsumee)
  (when *debug*
    (format t "~&Testing (subsumes ~A ~A)..." subsumer subsumee)
    ;(break)
    )
  (incf *subsumption-tests*)
  ;(break)
  (let* ((*use-relation-store* nil)
         (start (when *debug*
                  (get-internal-run-time)))
         (subsumee-constraint
          (new-initial-concept-constraint (if (and *model-merging* *use-alternate-models*) 
                                            subsumee
                                            encoded-subsumee)))
         (neg-subsumer-constraint
          (new-initial-concept-constraint (if (and *model-merging* *use-alternate-models*) 
                                            (concept-negated-concept subsumer)
                                            negated-subsumer)))
         (result (not (cs-satisfiable-2 (list subsumee-constraint neg-subsumer-constraint)
                                        nil
                                        +ind-counter-init+
                                        nil ; no state given
                                        nil ;disable expanded store
                                        )))
         (time (when *debug*
                 (/ (- (get-internal-run-time) start)
                    internal-time-units-per-second))))
    (when-statistics
      (print-sat-local-statistics))
    (when result
      (incf-statistics *subsumptions-found*))
    (when *model-merging*
      (if result
        (incf-statistics *unmergable-models-unsatisfiable*)
        (incf-statistics *unmergable-models-satisfiable*)))
    ;(break? subsumer subsumee)
    (when *debug*
      (format t "~S (~,3F secs)" result time))
    (values result neg-subsumer-constraint subsumee-constraint)))

(defparameter *verify* nil)

(defun load-concept-tree (filename)
  (with-open-file (tree-file filename :direction :input)
    (loop with table = (racer-make-hash-table)
          for (concept-name parents children) = (read tree-file nil '(done done))
          until (eq concept-name 'done)
          do (setf (gethash concept-name table) (cons parents children))
          finally (return table))))

(defparameter *tree-table*
  (and *verify* (load-concept-tree "dl-test:Data;appn-kb;galen.tree")))

(defun ham-alc-concept-name-filter (name)
  (cond
   ((eq name +top-symbol+) 'top)
   ((eq name +bottom-symbol+) 'bottom)
   (t name)))

(defun find-concept-name-in-supers (subsumer-name supers)
  (or (find subsumer-name supers)
      (loop for super in supers
            thereis
            (find-concept-name-in-supers subsumer-name
                                         (car (gethash super *tree-table*))))))

(defun compare-result (result subsumer-name subsumee-name subsumer subsumee)
  ;(return-from compare-result t)
  (let ((asserted
         (eq (first subsumer-name)
             (find-concept-name-in-supers
              (first subsumer-name) (car (gethash (first subsumee-name) *tree-table*))))))
    ;(break)
    (unless (eq result asserted)
      (format t "~&Recomputing (subsumes ~S ~S)...~%" subsumer subsumee)
      (let ((recheck (with-new-used-by-concept-store
                       (with-alc-bindings
                         (let ((*deep-model-merging* nil)
                               (*tableaux-caching* nil))
                           (test-bcp-reduced-subsumes subsumer subsumee))))))
        (break "incorrect subsumption ~S ~S: result=~S, asserted=~S, (subsumes ~S ~S)=~S"
               subsumer-name subsumee-name result asserted
               subsumer subsumee recheck)))))

;;;===========================================================================
;;; Main functions for testing subsumption and satisfiability of concept terms
;;; and satisfiability of a constraint system. Note that the concept stores
;;; and the individual-counter are not provided and must be managed externally.
;;;===========================================================================

(defun test-subsumed-by (subsumee subsumer)
  "Test whether subsumee is subsumed by subsumer. Concept stores have to be
provided externally."
  (test-subsumes subsumer subsumee))

(defun test-subsumes (subsumer subsumee)
  "Test whether subsumer subsumes subsumee. Concept stores have to be
provided externally."
  #+:debug
  (when (eq subsumer subsumee)
    (racer-warn "arguments of test-subsumes ~S ~S are equal!" subsumer subsumee))
  (with-race-trace-sublevel ("test-subsumes"
                             :arguments (list subsumer subsumee)
                             :expanded nil
                             :trace-result t)
    (with-unique-name-assumption
      (if (and (or (atomic-concept-p subsumer) (negated-concept-p subsumer))
               (or (atomic-concept-p subsumee) (negated-concept-p subsumee)))
        (let* ((*meta-constraint-concepts*
                (and *use-tbox* (tbox-meta-constraint-concepts *use-tbox*)))
               (*blocking-possibly-required*
                (or *encode-roles-as-transitive*
                    (and *use-tbox*
                         (or *meta-constraint-concepts*
                             (tbox-blocking-possibly-required *use-tbox*)))))
               (*blocking-used* nil)
               (result (test-subsumes-1 subsumer subsumee)))
          (when-collect-statistics
            (collect-statistics-data))
          #+:debug
          (when *verify*
            (compare-result result
                            (ham-alc-concept-name-filter (concept-name-set subsumer))
                            (ham-alc-concept-name-filter (concept-name-set subsumee))
                            subsumer subsumee))
          result)
        (progn
          #+:debug (assert *use-tbox*)
          (multiple-value-prog1
            (not (test-satisfiable *use-tbox*
                                   (encode-concept-term `(and ,(concept-negated-concept subsumer)
                                                              ,subsumee))))
            (when-collect-statistics
              (collect-statistics-data))))))))

(defun test-concept-models-subsume (tbox subsumer subsumee)
  (incf-statistics *all-subsumption-tests*)
  (prog1
    (with-race-trace-sublevel ("test-concept-models-subsume"
                               :arguments (list tbox subsumer subsumee)
                               :expanded nil
                               :trace-result t)
      (with-unique-name-assumption
        (with-alc-bindings
          (let* ((*meta-constraint-concepts* (tbox-meta-constraint-concepts tbox))
                 (*blocking-possibly-required*
                  (or *encode-roles-as-transitive*
                      (and *use-tbox*
                           (or *meta-constraint-concepts*
                               (tbox-blocking-possibly-required *use-tbox*)))))
                 (*blocking-used* nil))
            (multiple-value-bind (subsumes-p unknown-p)
                                 (obviously-subsumes subsumer subsumee)
              (if subsumes-p
                (progn
                  (incf-statistics *obvious-subsumptions*)
                  ;(unless (concepts-bucket-p subsumer) (break "1"))
                  t)
                (if unknown-p
                  (let ((subsumer (if (concepts-bucket-p subsumer)
                                    (bucket-concept subsumer)
                                    subsumer)))
                    ;(break "2")
                    (multiple-value-bind (not-subsumes-p unknown-p)
			                 (obviously-not-subsumes subsumer subsumee)
                      (if not-subsumes-p
                        (progn
                          (when *model-merging*
                            (incf-statistics *mergable-models*))
                          nil)
                        (progn
                          (when *model-merging*
                            (if unknown-p
                              (incf-statistics *unmergable-partial-models*)
                              (incf-statistics *unmergable-det-models*)))
                          t ;;; must be true if not-subsumes-p = nil independently of unknown-p
                          ))))
                  nil)))))))
    (when-collect-statistics
      (collect-statistics-data))))

#|
(defun break? (subsumer subsumee)
  (when (or (and (member 'ALL-FORKS-TAKEN (concept-name-set subsumer))
                 (member 'WAITING-FOR-RIGHT-2 (concept-name-set subsumee))
             )
            ;(member 'c1675 (concept-name-set subsumer))
            ;(member 'c2367 (concept-name-set subsumee))
            )
    (break "~S ~S" subsumer subsumee)
    ))
|#

(defun test-subsumes-1 (subsumer subsumee)
  "Test whether subsumer subsumes subsumee. Concept stores have to be provided externally."
  (incf-statistics *all-subsumption-tests*)
  (with-alc-bindings
    (multiple-value-bind (subsumes-p unknown-p)
                         (obviously-subsumes subsumer subsumee)
      (if subsumes-p
        (progn
          (incf-statistics *obvious-subsumptions*)
          t)
        (if unknown-p
          (multiple-value-bind (not-subsumes-p unknown-p)
                               (obviously-not-subsumes subsumer subsumee)
            (cond (not-subsumes-p
                   (when *model-merging*
                     (incf-statistics *mergable-models*))
                   nil)
                  ((not unknown-p)
                   (when *model-merging*
                     (incf-statistics *unmergable-det-models*))
                   t)
                  (t 
                   (when *model-merging*
                     (incf-statistics *unmergable-partial-models*))
                   (if (and (atomic-concept-p subsumer) (atomic-concept-p subsumee))
                     (test-bcp-reduced-subsumes subsumer subsumee)
                     (progn
                       #+:debug (assert *use-tbox*)
                       (multiple-value-prog1
                         (not (test-satisfiable *use-tbox*
                                                (encode-concept-term
                                                 `(and ,(concept-negated-concept subsumer)
                                                       ,subsumee))))
                         (when-collect-statistics
                           (collect-statistics-data))))))))
          nil)))))

(defun test-atomic-concept-satisfiable (tbox atomic-concept)
  "Consistency checker for a concept term. Concept stores have to be
provided externally."
  (when *debug*
    (format t "~&Testing satisfiability of atomic concept ~S..." atomic-concept))
  (with-race-trace-sublevel ("test-atomic-concept-satisfiable"
                             :arguments (list tbox atomic-concept)
                             :trace-result t)
    (with-unique-name-assumption
      (with-alc-bindings
        (let ((result nil)
              (start (when *debug*
                       (get-internal-run-time)))
              (time nil)
              (meta-constraint-concepts (tbox-meta-constraint-concepts tbox)))
          (if (and *use-elh-model-embedding*
                   (null meta-constraint-concepts)
                   (subset-el+-p (concept-language atomic-concept)))
            (let ((bottom *bottom-concept*)
                  (definition (concept-encoded-definition atomic-concept)))
              (if (or (eq atomic-concept bottom)
                      (and (not (null definition))
                           (or (eq definition bottom)
                               (loop for subsumer in (concept-told-subsumers definition)
                                     thereis (eq (concept-encoded-definition subsumer) bottom)))))
                (progn 
                  (setf result nil)
                  (unless (or (eq atomic-concept bottom) (concept-model atomic-concept))
                    (setf (concept-model atomic-concept) bottom)))
                (setf result t))
              (when *debug*
                (setf time (/ (- (get-internal-run-time) start)
                              internal-time-units-per-second))))
            (let* ((*meta-constraint-concepts* meta-constraint-concepts)
                   (*blocking-possibly-required*
                    (or *encode-roles-as-transitive*
                        (and *use-tbox*
                             (or *meta-constraint-concepts*
                                 (tbox-blocking-possibly-required *use-tbox*)))))
                   (*use-relation-store* nil))
              (set-language atomic-concept)
              (setf result (not (incoherent-model-p (get-cached-concept-model atomic-concept))))
              (when *debug*
                (setf time (/ (- (get-internal-run-time) start)
                              internal-time-units-per-second)))))
          (when *debug*
            (format t "~S (~,3F secs)" result time))
          (when-statistics
            (print-sat-local-statistics))
          result)))))

(defun test-satisfiable (tbox concept-term)
  "Consistency checker for a concept term. Concept stores have to be
provided externally."
  (with-unique-name-assumption
    (with-alc-bindings
      (multiple-value-prog1
        (let ((*use-relation-store* nil))
          (unless (eq concept-term (tbox-bottom-node tbox))
            (test-cs-satisfiable tbox (list (list +ind-counter-init+ concept-term)))))
        (when-statistics
          (print-sat-local-statistics))))))

(defun test-cs-satisfiable (tbox constraint-list &key (non-optimized-p nil))
  "ABox consistency checker for a list of constraints. Concept stores have to be
provided externally."
  (let ((*use-tbox* tbox))
    (cs-satisfiable-1 constraint-list non-optimized-p nil nil nil)))

(defun test-abox-satisfiable (abox
                              concept-constraints
                              relation-constraints
                              ind-or-inds
                              attribute-constraints
                              cd-state
                              &optional
                              (precompletion nil)
                              (return-precompletion-p nil)
                              (initial-backtrack-stack nil))
  "Internal ABox consistency checker for a constraint system. Concept stores have to be
provided externally."
  (with-race-trace-sublevel ("test-abox-satisfiable"
                             :expanded nil
                             :arguments (list abox 
                                              concept-constraints
                                              relation-constraints
                                              ind-or-inds 
                                              attribute-constraints
                                              cd-state
                                              precompletion
                                              return-precompletion-p
                                              initial-backtrack-stack)
                             :trace-result t)
    (setf *or-level* 0)
    (let* ((remaining-nondeterministic-constraints nil)
           (cd-constraint nil)
           (enable-abox-stores (enable-abox-stores-p (length (abox-individual-axioms abox))
                                                     (length (abox-role-axioms abox))))
           (*use-unexpanded-exists-constraints-store*
            (and *abox-use-unexpanded-exists-constraints-store* enable-abox-stores))
           (*use-unexpanded-disjunctive-constraints-store*
            (and *abox-use-unexpanded-disjunctive-constraints-store* enable-abox-stores))
           (*use-expanded-store* *abox-use-expanded-store*)
           (*precompletion* (and *use-abox-precompletion* return-precompletion-p))
           (precompletion-state (and precompletion (precompletion-state precompletion)))
           (use-precompletion (and *use-abox-precompletion*
                                   precompletion-state
                                   #|(not return-precompletion-p)|#))
           (use-completion (and *use-abox-completion*
                                (not use-precompletion)
                                precompletion-state
                                (not return-precompletion-p)))
           (using-precompletion (or use-precompletion use-completion))
           (*using-precompletion* using-precompletion)
           (cd-state (if using-precompletion
                         (or cd-state 
                             (copy-solver-state (state-concrete-domain-state precompletion-state))
                             (copy-solver-state (abox-initial-constraint-state abox)))
                       cd-state))
           (*old-swap-to-expanded-store-threshold* *swap-to-expanded-store-threshold*)
           (*old-cache-size-from-constraint-store* *cache-size-from-constraint-store*)
           (*swap-to-expanded-store-threshold*
            (if return-precompletion-p
                (if (< *swap-to-expanded-store-threshold* 100) ;test mode
                    *swap-to-expanded-store-threshold*
                  (min 100 (truncate *swap-to-expanded-store-threshold* 5)))
              *swap-to-expanded-store-threshold*))
           (*cache-size-from-constraint-store*
            (if return-precompletion-p
                (get-cache-size-from-constraint-store)
              *cache-size-from-constraint-store*))
           (*use-relation-store* (use-relation-store-p (when precompletion-state
                                                         (state-relation-store precompletion-state))
                                                       using-precompletion
                                                       relation-constraints))
           (use-relation-store *use-relation-store*)
           (same-as-clash-in-abox-individuals (unless using-precompletion 
                                                (same-as-clash-in-abox-individuals-p abox)))
           (*use-unique-name-assumption*
            (or *use-unique-name-assumption*
                (abox-current-una-assumption abox)
                (if using-precompletion
                    (and (relation-store-empty-p 
                          (state-relation-store precompletion-state))
                         (null (state-relation-constraints precompletion-state)))
                  (null relation-constraints))))
           (use-unique-name-assumption *use-unique-name-assumption*)
           (*use-abox* abox))


      (values
       (labels ((test-abox-satisfiable-continuation-1 (sat-p state unused-1 unused-2 unused-3)
                  (declare (ignore unused-1 unused-2 unused-3))
                  (let ((cd-state (state-concrete-domain-state state)))
                    (race-trace ("~&test-abox-satisfiable-continuation-1 (~S ~S ~S)~%"
                                 sat-p state remaining-nondeterministic-constraints))
                    ;; If satisfiable-p-1 then we must add the last constraint to the list of constraints in the cd-state!
                    ;; We ignore this if cd-constraint is not set (in the first call).
                    (when (and sat-p cd-constraint)
                      (push cd-constraint (solver-state-cd-constraints cd-state))
                      (when (nonlinear-predicate-p (cd-constraint-predicate cd-constraint))
                        (setf (solver-state-contains-additional-constraints cd-state) t)))
                  
                    (when sat-p 
                      (if remaining-nondeterministic-constraints
                          (progn 
                            (setf cd-constraint (first remaining-nondeterministic-constraints))
                            (pop remaining-nondeterministic-constraints)
                            (constraint-satisfiable-p-2 cd-constraint
                                                        state
                                                        t
                                                        nil
                                                        #'test-abox-satisfiable-continuation-1))
                        (let ((features-p nil))
                          (when (dl-features (abox-language abox))
                            (loop for constraint in relation-constraints
                                  for role = (constraint-term constraint)
                                  when (and (not (role-datatype role))
                                            (or (role-feature-p role)
                                                (role-feature-p (role-inverse-internal role))))
                                  do
                                  (setf features-p t)
                                  (return)))
                          (multiple-value-bind (satisfied-p
                                                new-concept-constraints-1
                                                new-relation-constraints-1
                                                removed-inds)
                              (if (or using-precompletion
                                      use-unique-name-assumption
                                      (null relation-constraints)
                                      (not features-p))
                                  (values t (copy-list concept-constraints) relation-constraints)
                                (merge-violated-abox-feature-relation-constraints abox
                                                                                  concept-constraints
                                                                                  relation-constraints))
                            (when satisfied-p
                              (let ((new-relation-constraints-2
                                     (if using-precompletion
                                         (unless use-relation-store
                                           (state-relation-constraints precompletion-state))
                                       new-relation-constraints-1)))
                                (when (and (not using-precompletion) new-relation-constraints-1)
                                  (loop for constraint in new-relation-constraints-1
                                        for role = (constraint-term constraint)
                                        do
                                        (unless (role-datatype role)
                                          (let ((new-constraint (make-relation-constraint
                                                                 (constraint-ind-2 constraint)
                                                                 (constraint-ind-1 constraint)
                                                                 (role-inverse-internal role))))
                                            (setf (constraint-dependencies new-constraint)
                                                  (constraint-dependencies constraint))
                                            (setf (constraint-ind-1-synonyms new-constraint)
                                                  (constraint-ind-2-synonyms constraint))
                                            (setf (constraint-ind-2-synonyms new-constraint)
                                                  (constraint-ind-1-synonyms constraint))
                                            (push new-constraint new-relation-constraints-2)))))
                                (let ((relation-store 
                                       (when (and (not using-precompletion)
                                                  use-relation-store
                                                  new-relation-constraints-2)
                                         (generate-relation-store new-relation-constraints-2))))
                                  (if (and (not using-precompletion)
                                           new-relation-constraints-2
                                           (clash-in-relation-constraints-p abox
                                                                            new-relation-constraints-2
                                                                            relation-store))
                                      nil
                                    (let* ((inds-old (if using-precompletion
                                                         (state-old-individuals precompletion-state)
                                                       (if (consp ind-or-inds)
                                                           ind-or-inds
                                                         (list ind-or-inds))))
                                           (inds (if removed-inds
                                                     (stable-set-difference inds-old removed-inds)
                                                   inds-old))
                                           (new-concept-constraints-2
                                            (racer-remove-concept-constraint-duplicates
                                             (if using-precompletion
                                                 new-concept-constraints-1
                                               (nconc (consider-initial-domain-and-range-restrictions
                                                       new-relation-constraints-2
                                                       relation-store)
                                                      (consider-negated-role-assertions abox)
                                                      new-concept-constraints-1))))
                                           (new-concept-constraints-3
                                            (nconc (unless (or using-precompletion
                                                               use-unique-name-assumption)
                                                     (consider-feature-restrictions inds relation-store))
                                                   new-concept-constraints-2))
                                           (individual-synonyms
                                            (when precompletion
                                              (precompletion-individual-synonyms precompletion)))
                                           (new-concept-constraints-4
                                            (if (or use-unique-name-assumption
                                                    (not using-precompletion)
                                                    (null individual-synonyms)
                                                    (zerop (hash-table-count individual-synonyms)))
                                                new-concept-constraints-3
                                              (consider-individual-synonyms abox
                                                                            individual-synonyms
                                                                            new-concept-constraints-3)))
                                           (new-concept-constraints-5
                                            (if use-relation-store
                                                new-concept-constraints-4
                                              (sort new-concept-constraints-4
                                                    #'string<
                                                    :key #'constraint-ind)))
                                           (old-inds-lang-table
                                            (when new-concept-constraints-5
                                              (compute-individual-language-table abox
                                                                                 use-precompletion
                                                                                 use-completion
                                                                                 precompletion-state
                                                                                 new-concept-constraints-5))))
                                      (multiple-value-prog1
                                          (let ((state
                                                  (if using-precompletion
                                                      (copy-basic-kernel-state precompletion-state t)
                                                    (make-basic-kernel-state
                                                     :attribute-constraints attribute-constraints
                                                     :concrete-domain-state cd-state
                                                     :relation-store relation-store
                                                     :unexpanded-exists-constraints-store
                                                     (unless *use-unexpanded-exists-constraints-store*
                                                       (new-unused-constraint-store))
                                                     :unexpanded-disjunctive-constraints-store
                                                     (unless *use-unexpanded-disjunctive-constraints-store*
                                                       (new-unused-constraint-store))
                                                     :old-individuals inds
                                                     :no-of-old-individuals (length inds)
                                                     :partially-expanded-or-stack
                                                     initial-backtrack-stack
                                                     :old-individuals-dl-language-table
                                                     old-inds-lang-table)))
                                                (new-relation-constraints-3 (unless relation-store
                                                                              new-relation-constraints-2)))
                                            (when using-precompletion
                                              (setf (state-concrete-domain-state state) cd-state)
                                              (setf (state-old-individuals-dl-language-table state)
                                                    old-inds-lang-table)
                                              (setf (state-partially-expanded-or-stack state)
                                                    initial-backtrack-stack))
                                            (when-debug using-precompletion
                                              (if use-precompletion
                                                  (race-trace ("~&Testing Abox ~S with precompletion ~S, state=~S~%"
                                                               abox precompletion state))
                                                (race-trace ("~&Testing Abox ~S with completion ~S, state=~S~%"
                                                             abox precompletion state))))
                                            (cs-satisfiable-2 new-concept-constraints-5
                                                              new-relation-constraints-3
                                                              inds
                                                              state
                                                              t ;enable expanded store
                                                              precompletion
                                                              return-precompletion-p
                                                              abox
                                                              features-p))
                                        (when-statistics
                                          (print-sat-local-statistics)))))))))))))))
         (if same-as-clash-in-abox-individuals
             nil
           (progn
             (setf remaining-nondeterministic-constraints 
                   (and cd-state (solver-state-nondeterministic-constraints cd-state)))
             (multiple-value-prog1
                 (let ((state (make-basic-kernel-state :concrete-domain-state cd-state)))
                   (test-abox-satisfiable-continuation-1 t state nil nil nil))
               (when-collect-statistics
                 (collect-statistics-data))))))
       (when (precompletion-p *precompletion*)
         *precompletion*)))))

(defun consider-individual-synonyms (abox individual-synonyms concept-constraints)
  (loop with synonym-table = individual-synonyms
        for constraint in concept-constraints
        for ind-name = (constraint-ind constraint)
        for synonym-ind-name = (loop for last-synonym = nil then synonym
                                     for synonym = (gethash ind-name synonym-table)
                                     then (gethash synonym synonym-table)
                                     until (null synonym)
                                     finally
                                     (when last-synonym
                                       (setf (gethash ind-name synonym-table) last-synonym))
                                     (return last-synonym))
        if synonym-ind-name
        collect
        (let* ((synonym-ind
                (find-individual abox synonym-ind-name))
               (new_constraint
                (concluded-concept-constraint
                 (first (individual-name-set synonym-ind))
                 (if (constraint-negated-p constraint)
                   (concept-negated-concept
                    (constraint-term constraint))
                   (constraint-term constraint))
                 constraint)))
          (setf (constraint-ind-synonyms new_constraint)
                (list ind-name))
          new_constraint)
        else collect constraint))

(defun compute-individual-language-table (abox
                                          use-precompletion
                                          use-completion
                                          precompletion-state
                                          concept-constraints)
  (let ((old-table
         (when (or use-precompletion use-completion)
           (first (state-old-individuals-dl-language-table precompletion-state)))))
    (if concept-constraints
        (loop with table = (racer-make-hash-table)
              with modified-p = nil
              for constraint in concept-constraints
              for ind-name = (constraint-ind constraint)
              for old-language = (or (gethash ind-name table)
                                     (when old-table
                                       (gethash ind-name old-table))
                                     (individual-language (find-individual abox ind-name)))
              for term-language = (concept-language (constraint-term constraint))
              unless (eq old-language term-language)
              do
              (let ((new-language (union-dl-descriptors term-language old-language)))
                (unless (eq old-language new-language)
                  (setf (gethash ind-name table) new-language)
                  (unless modified-p
                    (setf modified-p t))))
              finally
              (if modified-p
                  (if old-table
                      (return (list table old-table))
                    (return (list old-table)))
                (return (list (or old-table (racer-make-hash-table))))))
      (list (or old-table (racer-make-hash-table))))))

;;;===========================================================================
;;; Main functions for testing subsumption and satisfiability of concept terms
;;; and satisfiability of a constraint system. Note that the concept stores
;;; are created for each single test.
;;;===========================================================================

(defun subsumed-by (subsumee subsumer)
  "Test whether subsumee is subsumed by subsumer"
  (not (satisfiable `(and (not ,subsumer) ,subsumee))))

(defun subsumes (subsumer subsumee)
  "Test whether subsumer subsumes subsumee"
  (subsumed-by subsumee subsumer))

(defun satisfiable (concept-term)
  "Consistency checker for a concept term"
  (let ((*deep-model-merging* nil)
        (*use-relation-store* nil))
    (multiple-value-prog1
      (cs-satisfiable (list (list +ind-counter-init+ concept-term)))
      (when-statistics
        (print-sat-local-statistics))
      (when-collect-statistics
        (collect-statistics-data)))))

(defun alc-concept-coherent (concept-term &key (logic :k))
  (multiple-value-bind (*encode-roles-as-reflexive*
                        *encode-roles-as-transitive*)
                       (ecase logic
                         ((:k :km) (values nil nil))
                         (:s4 (values t t))
                         (:k4) (values nil t))
    (with-race-trace-sublevel ("alc-concept-coherent"
                               :arguments (list concept-term logic)
                               :trace-result t)
      (satisfiable concept-term))))



(defun cs-satisfiable (constraint-list
                         &key (tbox-name nil) (non-optimized-p nil))
  "ABox consistency checker for a list of constraints"
  (let ((tbox (and tbox-name (find-tbox tbox-name)))
        (start (when-print-statistics
                (when-sat-statistics
                 (get-internal-run-time)))))
    (if tbox
      (with-alc-environment (
                             :tbox tbox
                             :id-variable (tbox-structure-id-counter tbox)
                             :tableaux-cache (tbox-tableaux-cache tbox)
                             :tableaux-sat-cache (tbox-tableaux-sat-cache tbox)
                             :tableaux-unsat-cache (tbox-tableaux-unsat-cache tbox)
                             :concept-store (tbox-concept-store tbox)
                             :role-store (tbox-role-store tbox)
                             :concrete-domain (tbox-concrete-domain tbox)
                             :stable-set-difference-table (tbox-stable-set-difference-table tbox)
                             :stable-set-difference-last-list2 (tbox-stable-set-difference-last-list2 tbox)
                             :racer-remove-duplicates-table (tbox-racer-remove-duplicates-table tbox)
                             :racer-remove-constraint-duplicates-table (tbox-racer-remove-constraint-duplicates-table tbox)
                             :possible-subsumees-vector (tbox-possible-subsumees-vector tbox)
                             :expanded-constraints-ind-table (tbox-expanded-constraints-ind-table tbox)
                             :live-inds-table (tbox-live-inds-table tbox)
                             :obsolete-inds-table (tbox-obsolete-inds-table tbox)
                             :label-inds-table (tbox-label-inds-table tbox)
                             :new-inds-table (tbox-new-inds-table tbox)
                             :concept-set-mark (tbox-concept-set-mark tbox)
                             :role-set-mark (tbox-role-set-mark tbox)
                             :individual-set-mark (tbox-individual-set-mark tbox)
                             :constraint-set-mark (tbox-constraint-set-mark tbox)
                             :classification-counter (tbox-classification-counter tbox)
                             :obsolete-eql-tables (tbox-obsolete-eql-tables tbox)
                             :obsolete-equal-tables (tbox-obsolete-equal-tables tbox)
                             :obsolete-equal-concept-tables (tbox-obsolete-equal-concept-tables tbox)
                             :initial-hash-table-size (tbox-initial-hash-table-size tbox)
                             :signatures-equal-table (tbox-signatures-equal-table tbox)
                             :partitions-table (tbox-partitions-table tbox)
                             :use-less-tbox-memory (tbox-use-less-memory tbox)
                             :set-vector (tbox-set-vector tbox)
                             :select-disjunct-table (tbox-select-disjunct-table tbox)
                             )
        (with-new-used-by-concept-store
          (with-alc-bindings
            (cs-satisfiable-1 constraint-list non-optimized-p nil nil nil))))
      (with-alc-environment ()
        (with-new-used-by-concept-store
          (with-alc-bindings
            (multiple-value-prog1
              (cs-satisfiable-1 constraint-list non-optimized-p nil nil nil)
              (when-print-statistics
                (when-sat-statistics
                  (print-sat-statistics *standard-output*
                                        *race-statistics-stream*
                                        start
                                        (get-internal-run-time)))))))))))

;;;===========================================================================
;;; Main internal functions for testing satisfiability of a constraint system.
;;;===========================================================================

(defmacro without-tenured-space (&body body)
  #+:lispworks
  `(hcl:block-promotion . ,body)
  #-:lispworks
  `(progn . ,body)
  )

(defun cs-satisfiable-1 (constraint-list non-optimized-p use-constraint-store-p unused-1 unused-2)
  "Internal ABox consistency checker. The list of constraints is normalized,
simplified, and encoded."
  (declare (ignore unused-1 unused-2))
  (without-tenured-space
   (let ((inds (mapcar #'first constraint-list)))
     (multiple-value-bind (concept-constraints relation-constraints)
                          (if non-optimized-p
                              (encode-constraints-non-optimized constraint-list)
                            (encode-constraints-optimized constraint-list))
       (cs-satisfiable-2 concept-constraints
                         relation-constraints
                         inds
                         nil ; no state given
                         use-constraint-store-p
                         )))))

#+(and :lispworks (not :lispworks-64bit))
(defun call-memory-functions ()
  (hcl:mark-and-sweep 2)          ; first collect all dead objects
  (multiple-value-bind (tf tsb tlb)
                       (hcl:check-fragmentation 2) ; check the fragmentation
    (declare (ignore tsb))
    (when  (and (> 10000000 tlb)         
                (> (ash tf -2) tlb))
      (hcl:try-move-in-generation 2 0))))

#+:lispworks-64bit
(defun call-memory-functions ()
  #|
  (hcl:gc-generation 2)
  (let ((fragmentation-state (hcl:gen-num-segments-fragmentation-state 2)))
    )
  |#
 )

(defun cs-satisfiable-2 (concept-constraints
                            relation-constraints
                            ind-or-inds
                            state
                            use-constraint-store-p
                            &optional
                            (precompletion nil)
                            (save-precompletion nil)
                            (abox nil)
                            (features-p nil))
  "Internal ABox consistency checker. Expects encoded concept and relation 
constraints."
  ;;#-:debug (declare (ignore indirectly-blocked-individuals))
  (with-race-trace-sublevel ("cs-satisfiable-2"
                             :expanded nil
                             :arguments (list concept-constraints
                                              relation-constraints
                                              ind-or-inds
                                              state
                                              use-constraint-store-p
                                              precompletion
                                              save-precompletion
                                              abox
                                              features-p)
                             :trace-result t)
    (let ((blocking-used nil))
      (multiple-value-prog1
        (let* ((tbox *use-tbox*)
               (*in-precompletion* 
                (and use-constraint-store-p 
                     (or (null precompletion)
                         save-precompletion)))
               (*save-precompletion* save-precompletion)
               (encode-roles-as-transitive *encode-roles-as-transitive*)
               (meta-constraint-concepts
                (and tbox (tbox-meta-constraint-concepts tbox)))
               (*meta-constraint-concepts* meta-constraint-concepts)
               (blocking-possibly-required
                (or encode-roles-as-transitive
                    (and tbox
                         (or meta-constraint-concepts
                             (tbox-blocking-possibly-required tbox)))))
               (*blocking-possibly-required* blocking-possibly-required)
               (*blocking-used* nil)
               (ind-counter-some-satisfiable
                (if precompletion 
                  (precompletion-ind-counter-some-satisfiable precompletion)
                  +ind-counter-init+))
               (*ind-counter-some-satisfiable* ind-counter-some-satisfiable)
	       (*ignored-live-individuals-cycles* 0)
               (*collected-dependencies* nil)
               (*collected-ind-dependencies* nil)
               (*signature-set-unsat-cache* nil)
               (new-concept-constraints-1
                (if (listp concept-constraints)
                  concept-constraints
                  (list concept-constraints))))
          (when (and (null precompletion) meta-constraint-concepts)
            #+:debug (assert (not (null ind-or-inds)))
            (if (consp ind-or-inds)
              (loop for ind in ind-or-inds do
                    (loop for meta-constraint-concept in meta-constraint-concepts
                          for constraint = (new-root-concept-constraint ind meta-constraint-concept)
                          do
                          (setf (constraint-meta-p constraint) t)
                          (push constraint new-concept-constraints-1)))
              (loop for meta-constraint-concept in meta-constraint-concepts
                    for constraint = (new-root-concept-constraint ind-or-inds
                                                                  meta-constraint-concept)
                    do
                    (setf (constraint-meta-p constraint) t)
                    (push constraint new-concept-constraints-1))))
          (with-alc-language-settings (
                                       :constraints new-concept-constraints-1
                                       :cd-state (when state 
                                                   (state-concrete-domain-state state))
                                       :precompletion-prover-language
                                       (if precompletion
                                         (precompletion-language precompletion)
                                         (if features-p
                                           (add-dl-features *dl-empty*)
                                           *dl-empty*)))
            (let* ((abox (or (and state (state-parameters state) (state-abox (state-parameters state)))
                             abox))
                   (parameters
                    (create-kernel-parameters :tbox tbox
                                              :abox abox
                                              ;:blocking-possibly-required blocking-possibly-required
                                              ;:ind-counter-some-satisfiable ind-counter-some-satisfiable
                                              ;:or-level *or-level*
                                              :dl-prover-language *dl-prover-language*
                                              ;:inverse-roles *inverse-roles*
                                              ;:deep-model-merging *deep-model-merging*
                                              ;:tableaux-caching *tableaux-caching*
                                              ;:sorted-concept-list *sorted-concept-list*
                                              :subsumed-concept-p *subsumed-concept-p*
                                              :concept-clash-p *concept-clash-p*
                                              ;:matching-constraint-test *matching-constraint-test*
                                              ;:find-matching-constraint *find-matching-constraint*
                                              ;:encode-roles-as-transitive encode-roles-as-transitive
                                              )))
              (multiple-value-bind (new-concept-constraints-2 delayed-constraints)
                                   (if (and abox state (state-relation-store state) *delay-abox-exists*)
                                     (loop with relation-store = (state-relation-store state)
                                           for constraint in new-concept-constraints-1
                                           if (and (some-constraint-p constraint)
                                                   (not (or (exists-as-datatype-constraint-p constraint)
                                                            (some-as-all-constraint-p constraint
                                                                                      relation-constraints
                                                                                      relation-store))))
                                           collect constraint into some-constraints
                                           else
                                           collect constraint into other-constraints
                                           finally
                                           (when-debug some-constraints
                                             (race-trace ("~&Delaying some constraints ~S"
                                                          some-constraints)))
                                           (return (values other-constraints some-constraints)))
                                     new-concept-constraints-1)
                (let* ((expanded-store (when precompletion
                                         (state-expanded-store state)))
                       #+:debug
                       (unexpanded-exists-constraints-store
                        (when precompletion
                          (state-unexpanded-exists-constraints-store state)))
                       #+:debug
                       (unexpanded-disjunctive-constraints-store
                        (when precompletion
                          (state-unexpanded-disjunctive-constraints-store state)))
                       (new-delayed-constraints (if state
                                                  (nconc delayed-constraints
                                                         (state-unprocessed-constraints state))
                                                  delayed-constraints))
                       (new-state
                        (if state
                          (let ((new-labels (cons (make-inverse-label-info :ind +ind-counter-init+)
                                                  (state-labels state)))
                                (new-expanded-store (or expanded-store
                                                        (unless (or ;*inverse-roles*
                                                                 (and use-constraint-store-p
                                                                      *use-expanded-store*))
                                                          (new-unused-constraint-store)))))
                            #-:debug
                            (changed-kernel-state state
                                                  :unprocessed-constraints new-delayed-constraints
                                                  :labels new-labels
                                                  :relation-constraints relation-constraints
                                                  :expanded-store new-expanded-store
                                                  :parameters parameters)
                            #+:debug
                            (let ((new-unexpanded-exists-constraints-store
                                   (or unexpanded-exists-constraints-store
                                       (unless (and use-constraint-store-p
                                                    *use-unexpanded-exists-constraints-store*)
                                         (new-unused-constraint-store))))
                                  (new-unexpanded-disjunctive-constraint-store
                                   (or unexpanded-disjunctive-constraints-store
                                       (unless (and use-constraint-store-p
                                                    *use-unexpanded-disjunctive-constraints-store*)
                                         (new-unused-constraint-store)))))
                              (changed-kernel-state state
                                                    :unprocessed-constraints new-delayed-constraints
                                                    :labels new-labels
                                                    :relation-constraints relation-constraints
                                                    :expanded-store new-expanded-store
                                                    :unexpanded-exists-constraints-store
                                                    new-unexpanded-exists-constraints-store
                                                    :unexpanded-disjunctive-constraints-store
                                                    new-unexpanded-disjunctive-constraint-store
                                                    :parameters parameters)))
                          (let ((new-labels (list (make-inverse-label-info :ind +ind-counter-init+)))
                                (new-expanded-store (unless (and use-constraint-store-p
                                                                 *use-expanded-store*)
                                                      (new-unused-constraint-store))))
                            #-:debug
                            (make-basic-kernel-state :unprocessed-constraints new-delayed-constraints
                                                     :labels new-labels
                                                     :relation-constraints relation-constraints
                                                     :expanded-store new-expanded-store
                                                     :parameters parameters
                                                     :partially-expanded-or-stack nil)
                            #+:debug
                            (let ((new-unexpanded-exists-constraints-store
                                   (unless (and use-constraint-store-p
                                                *use-unexpanded-exists-constraints-store*)
                                     (new-unused-constraint-store)))
                                  (new-unexpanded-disjunctive-constraint-store
                                   (unless (and use-constraint-store-p
                                                *use-unexpanded-disjunctive-constraints-store*)
                                     (new-unused-constraint-store))))
                              (make-basic-kernel-state :unprocessed-constraints new-delayed-constraints
                                                       :labels new-labels
                                                       :relation-constraints relation-constraints
                                                       :expanded-store new-expanded-store
                                                       :unexpanded-exists-constraints-store
                                                       new-unexpanded-exists-constraints-store
                                                       :unexpanded-disjunctive-constraints-store
                                                       new-unexpanded-disjunctive-constraint-store
                                                       :parameters parameters
                                                       :partially-expanded-or-stack nil)))))
                       (*use-expanded-store* use-constraint-store-p))
                  (if *save-expanded-model*
                    (progn
                      (setf (state-save-completion new-state) t)
                      (race-trace ("~&Setting save-expanded-model in state ~S~%" new-state)))
                    (setf (state-save-completion new-state) nil))
                  (race-trace ("~&Testing satisfiability of constraint system ~%~
                                state ~S c-constraints ~S ~%r-constraints ~S~%"
                               (copy-basic-kernel-state-internal new-state)
                               new-concept-constraints-1 relation-constraints))
                  (let ((result
                         (added-constraints-satisfiable new-concept-constraints-2
                                                        nil ; relation constraints already added to state
                                                        new-state
                                                        t nil)))
                    ;(break)
                    (race-trace ("~&Satisfiability of constraint system is ~S, state=~S~%"
                                 result new-state))
                    (setf blocking-used *blocking-used*)
                    (values result *blocking-used*)))))))
        (when (and blocking-used (boundp '*blocking-used*))
          (setf *blocking-used* blocking-used))))))


;;; ======================================================================

(defun add-new-constraint-to-current-label (constraint labels)
  (let ((label-info (first (find-label-for-ind (constraint-ind constraint) labels)))
        (concept (if (constraint-negated-p constraint)
                   (concept-negated-concept (constraint-term constraint))
                   (constraint-term constraint))))
    (if (and label-info (not (eq concept *top-concept*)))
      (let ((new-label (copy-label-info label-info)))
        (pushnew concept (label-info-back-propagated-concepts new-label))
        ;(break "~S" constraint)
        (substitute new-label label-info labels))
      labels)))

(defun add-new-constraints-to-current-label (constraints labels)
  (if constraints
    (let ((label-info (first (find-label-for-ind (constraint-ind (first constraints)) labels)))
          (concepts (loop with top = *top-concept*
                          for constraint in constraints
                          for concept = (if (constraint-negated-p constraint)
                                          (concept-negated-concept (constraint-term constraint))
                                          (constraint-term constraint))
                          unless (eq concept top)
                          collect concept)))
      (if (and label-info concepts)
        (let ((new-label (copy-label-info label-info)))
          (setf (label-info-back-propagated-concepts new-label)
                (concept-set-union concepts (label-info-back-propagated-concepts new-label)))
          ;(break "~S" constraint)
          (substitute new-label label-info labels))
        labels))
    labels))

(defun save-expanded-model (state)
  (let ((state (if (signature-kernel-state-p state)
                 (copy-to-basic-kernel-state state)
                 state)))
    (setf (state-save-completion state) nil)
    (let* ((*use-abox-precompletion* nil)
           (new-state (copy-basic-kernel-state (commit-precompletion state nil t) t)))
      (setf *expanded-model* (make-precompletion :state new-state
                                                 :language *dl-prover-language*
                                                 :individual-synonyms
                                                 (compute-individual-synonyms new-state)
                                                 :ind-counter-some-satisfiable
                                                 *ind-counter-some-satisfiable*))
      (race-trace ("~&Saving tableau ~S with state" *expanded-model*))
      #+:debug *expanded-model*
      )))

(defun alc-cs-satisfiable (new-constraint state unused-1 unused-2 unused-3)
  "Test whether new-constraint doesn't clash. In case of a clash backtrack 
immediately. Test also whether this constraint is already established. In 
case of a duplicate, ignore new-constraint. Otherwise expand the constraint 
and add it to the list of established constraints."
  (declare #+:allegro (:explain :tailmerging)
	   (ignore unused-1 unused-2 unused-3))
  (let ((unexpanded-deterministic-constraints (state-unexpanded-deterministic-constraints state))
        (unexpanded-defined-det-constraints (state-unexpanded-defined-det-constraints state))
        (unexpanded-exists-constraints (state-unexpanded-exists-constraints state))
        (unexpanded-disjunctive-constraints (state-unexpanded-disjunctive-constraints state))
        (unprocessed-constraints (state-unprocessed-constraints state))
        (expanded-constraints (state-expanded-constraints state))
        (relation-constraints (state-relation-constraints state))
        (unexpanded-exists-constraints-store (state-unexpanded-exists-constraints-store state))
        (unexpanded-disjunctive-constraints-store
         (state-unexpanded-disjunctive-constraints-store state)))
    (if (null new-constraint)
      (cond
       ((null (or unexpanded-exists-constraints
                  unexpanded-disjunctive-constraints
                  unexpanded-defined-det-constraints
                  unexpanded-deterministic-constraints
                  unprocessed-constraints))
        (when (and *in-precompletion* *save-precompletion*)
          (commit-precompletion state nil))
        (if (state-save-completion state)
          (save-expanded-model state)
          (race-trace ("~&Satisfied tableau with state ~S" state)))
        (if (empty-backtrack-stack-p (state-partially-expanded-or-stack state))
	    t
          (unwrap-backtrack-state state nil nil nil nil)))
       ((and (null unexpanded-deterministic-constraints)
             unexpanded-defined-det-constraints)
        (unfolded-symbols-satisfiable state nil nil nil nil))
       (unprocessed-constraints
        (let ((new-state (changed-kernel-state state
                                               :unprocessed-constraints nil)))
          (race-trace ("~&Adding delayed constraints to tableaux, state=~S, delayed-constraints=~S"
                       new-state
                       unprocessed-constraints))
          (added-constraints-satisfiable unprocessed-constraints
                                         nil ; no conclusion relation constraint
                                         new-state
                                         t nil)))
       (t (added-constraints-satisfiable nil ; no conclusion concept constraint
                                         nil ; no conclusion relation constraint
                                         state
                                         t nil)))
      (multiple-value-bind (clash-result remove-duplicate)
                           (clash-in-cs-p new-constraint
                                          expanded-constraints
                                          state
                                          t)
        (cond
         ((eq clash-result t)
          (handle-clash-with-backtrack-state state nil nil nil nil))
         ((and (null clash-result) ;duplicate found
               (null remove-duplicate)
               (not (and (exists-constraint-p new-constraint)
                         (constraint-merging-trigger-p new-constraint))))
          (race-trace ("~&Duplicate ~S ignored~%" new-constraint))
          (multiple-value-bind
            (new-unexpanded-exists-constraints
             new-unexpanded-exists-constraints-store)
            (if (and (exists-constraint-p new-constraint)
                     (not (some-as-all-constraint-p new-constraint
                                                    relation-constraints
                                                    (state-relation-store state))))
              (remove-constraint-from-constraint-store new-constraint
                                                       unexpanded-exists-constraints
                                                       unexpanded-exists-constraints-store
                                                       (state-copy-unexpanded-exists-constraints-store-p state)
                                                       state
                                                       #'reset-exists-copy)
              (values unexpanded-exists-constraints unexpanded-exists-constraints-store))
            (multiple-value-bind
              (new-unexpanded-disjunctive-constraints
               new-unexpanded-disjunctive-constraints-store)
              (if (and (or-constraint-p new-constraint)
                       (not (deterministic-or-constraint-p
                             (constraint-open-clauses-counter new-constraint))))
                (remove-constraint-from-constraint-store new-constraint
                                                         unexpanded-disjunctive-constraints
                                                         unexpanded-disjunctive-constraints-store
                                                         (state-copy-unexpanded-disjunctive-constraints-store-p state)
                                                         state
                                                         #'reset-disjunctive-copy)
                (values unexpanded-disjunctive-constraints
                        unexpanded-disjunctive-constraints-store))
              (let ((new-state
                     (changed-kernel-state state
                                           :unexpanded-exists-constraints
                                           new-unexpanded-exists-constraints
                                           :unexpanded-disjunctive-constraints
                                           new-unexpanded-disjunctive-constraints
                                           :unexpanded-exists-constraints-store
                                           new-unexpanded-exists-constraints-store
                                           :unexpanded-disjunctive-constraints-store
                                           new-unexpanded-disjunctive-constraints-store)))
                (added-constraints-satisfiable nil ; no new conclusion constraints
                                               nil ; no new relation constraints
                                               new-state
                                               t nil)))))
         (t
          (let ((expanded-store (state-expanded-store state)))
            (multiple-value-bind
              (new-expanded-constraints-1 new-expanded-store-1 new-expanded-store-index-1)
              (if (and (null clash-result) remove-duplicate)
                (remove-constraint-from-constraint-store remove-duplicate
                                                         expanded-constraints
                                                         expanded-store
                                                         (state-copy-expanded-store-p state)
                                                         state
                                                         #'reset-expanded-copy
                                                         (state-expanded-store-index state))
                (values expanded-constraints expanded-store (state-expanded-store-index state)))
              (let* ((extend-expanded-store-p
                      (and *use-expanded-store*
                           new-expanded-constraints-1
                           (swap-to-expanded-constraint-store-p new-expanded-constraints-1
                                                                new-expanded-store-1)))
                     (copy-p (state-copy-expanded-store-p state))
                     (new-expanded-constraints-2
                      (if extend-expanded-store-p
                        (list new-constraint)
                        (cons new-constraint new-expanded-constraints-1))))
                #+:debug
                (when (boundp '*debug2*)
                  (assert (eql (length new-expanded-constraints-2)
                               (length (racer-remove-duplicates new-expanded-constraints-2)))))
                (when (and copy-p extend-expanded-store-p)
                  (setf (state-copy-expanded-store-p state) nil))
		(multiple-value-bind (new-expanded-store-2 new-expanded-store-index-2)
		                     (if extend-expanded-store-p
			               (swap-to-constraint-store new-expanded-constraints-1
						                 new-expanded-store-1
						                 ':expanded
						                 new-expanded-store-index-1
						                 copy-p)
		                       (values new-expanded-store-1 new-expanded-store-index-1))
		  (let ((new-state
			 (changed-kernel-state state
					       :expanded-constraints new-expanded-constraints-2
					       :expanded-store new-expanded-store-2
					       :expanded-store-index new-expanded-store-index-2)))
                    (when-race-trace extend-expanded-store-p
                      (race-trace ("~&New expanded constraints after swapping: ~S, state=~S~%"
                                   new-expanded-constraints-2 new-state)))
                    (alc-cs-satisfiable-2 new-constraint
                                          new-state
                                          nil
                                          nil
                                          nil))))))))))))

(defun alc-cs-satisfiable-2 (new-constraint state unused-1 unused-2 unused-3)
  (declare (ignore unused-1 unused-2 unused-3)
           #+:allegro (:explain :tailmerging))
  (let* ((unexpanded-defined-det-constraints (state-unexpanded-defined-det-constraints state))
         (unexpanded-exists-constraints (state-unexpanded-exists-constraints state))
         (relation-constraints (state-relation-constraints state))
         (labels (state-labels state))
         (indirectly-blocked-individuals (state-indirectly-blocked-individuals state))
         (expanded-constraints (state-expanded-constraints state))
         (ind (constraint-ind new-constraint))
         (create-new-labels (and *inverse-roles* (not (true-old-individual-p ind))))
         (new-labels (if create-new-labels
                         (add-new-constraint-to-current-label new-constraint labels)
                       labels)))
    (race-trace ("~&Expanding ~S: state=~S, new-labels=~S~%" new-constraint state new-labels))
    (ecase (constraint-term-type new-constraint)
      (atomic-concept
       (let* ((tbox *use-tbox*)
              (el+-transformed-table (when tbox
                                       (tbox-el+-transformed-table tbox))))
         (if (defined-deterministic-constraint-p new-constraint el+-transformed-table)
             (let* ((new-unexpanded-defined-det-constraints
                     (cons new-constraint unexpanded-defined-det-constraints))
                    (new-state
                     (changed-kernel-state state
                                           :unexpanded-defined-det-constraints
                                           new-unexpanded-defined-det-constraints
                                           :labels new-labels)))
               (added-constraints-satisfiable nil
                                              nil ; no new relation constraint
                                              new-state
                                              t nil))
           (let ((new-state
                  (changed-kernel-state state :labels new-labels)))
             (added-constraints-satisfiable nil
                                            nil ; no new relation constraint
                                            new-state t nil)))))
      ((and-concept disjoint-and-concept)
       (if (or-constraint-p new-constraint)
           (let ((open-clauses-no
                  (constraint-open-clauses-counter new-constraint)))
             (cond
              ((fully-expanded-or-constraint-p open-clauses-no)
               (let ((new-state
                      (changed-kernel-state state :labels new-labels)))
                 (fully-expanded-or-satisfiable new-constraint new-state nil nil nil)))
              ((bcp-or-constraint-p open-clauses-no)
               (let ((new-state
                      (changed-kernel-state state :labels new-labels)))
                 (expanded-bcp-or-satisfiable new-constraint new-state nil nil nil)))
              (t (let* ((new-expanded-constraints
                         (racer-remove new-constraint expanded-constraints))
                        (new-state (changed-kernel-state state
                                                         :expanded-constraints new-expanded-constraints
                                                         :labels new-labels)))
                   (partially-expanded-or-satisfiable new-constraint new-state nil nil nil)))))
         (let ((new-state
                (changed-kernel-state state :labels new-labels)))
           (expanded-and-satisfiable new-constraint new-state nil nil nil))))
      (some-concept
       (let ((some-feature-p (some-as-all-constraint-p new-constraint
                                                       relation-constraints
                                                       (state-relation-store state))))
         (if (or (constraint-negated-p new-constraint) some-feature-p)
             (let ((new-state (changed-kernel-state state :labels new-labels)))
               (expanded-all-satisfiable new-constraint new-state some-feature-p nil nil))
           (if (member (constraint-ind new-constraint) indirectly-blocked-individuals)
               (multiple-value-bind (new-unexpanded-exists-constraints
                                     new-unexpanded-exists-constraints-store)
                   (remove-constraint-from-constraint-store
                    new-constraint
                    unexpanded-exists-constraints
                    (state-unexpanded-exists-constraints-store state)
                    (state-copy-unexpanded-exists-constraints-store-p state)
                    state
                    #'reset-exists-copy)
                 (race-trace ("~&Ignoring indirectly blocked constraint ~S, blocked inds=~S~%"
                              new-constraint indirectly-blocked-individuals))
                 (let ((new-state
                        (changed-kernel-state state
                                              :labels new-labels
                                              :unexpanded-exists-constraints
                                              new-unexpanded-exists-constraints
                                              :unexpanded-exists-constraints-store
                                              new-unexpanded-exists-constraints-store)))
                   (added-constraints-satisfiable nil ; no conclusion concept constraint
                                                  nil ; no conclusion relation constraint
                                                  new-state t nil)))
             (if (and create-new-labels
                      (constraint-backpropagated-p new-constraint)
                      (not (constraint-signature new-constraint))
                      (member (constraint-term new-constraint)
                              (label-info-back-propagated-concepts
                               (first (find-label-for-ind ind labels)))))
                 (multiple-value-bind (new-unexpanded-exists-constraints
                                       new-unexpanded-exists-constraints-store)
                     (remove-constraint-from-constraint-store
                      new-constraint
                      unexpanded-exists-constraints
                      (state-unexpanded-exists-constraints-store state)
                      (state-copy-unexpanded-exists-constraints-store-p state)
                      state
                      #'reset-exists-copy)
                   (let ((new-state
                          (changed-kernel-state state
                                                :labels new-labels
                                                :unexpanded-exists-constraints
                                                new-unexpanded-exists-constraints
                                                :unexpanded-exists-constraints-store
                                                new-unexpanded-exists-constraints-store)))
                     (race-trace ("~&Ignoring redundant back-propagated constraint ~S, old/new labels ~S ~S, state=~S~%"
                                  new-constraint labels new-labels new-state))
                     (added-constraints-satisfiable nil ; no conclusion concept constraint
                                                    nil ; no conclusion relation constraint
                                                    new-state t nil)))
               (let* ((new-expanded-constraints
                       (racer-remove new-constraint expanded-constraints))
                      (new-state (changed-kernel-state state
                                                       :expanded-constraints new-expanded-constraints
                                                       :labels new-labels)))
                 (expanded-some-satisfiable new-constraint new-state nil nil nil)))))))
      (at-least-concept
       (if (constraint-negated-p new-constraint)
           (let ((new-state (changed-kernel-state state :labels new-labels)))
             (expanded-at-most-satisfiable new-constraint new-state nil nil nil))
         (if (member (constraint-ind new-constraint) indirectly-blocked-individuals)
             (multiple-value-bind
                 (new-unexpanded-exists-constraints new-unexpanded-exists-constraints-store)
                 (remove-constraint-from-constraint-store
                  new-constraint
                  unexpanded-exists-constraints
                  (state-unexpanded-exists-constraints-store state)
                  (state-copy-unexpanded-exists-constraints-store-p state)
                  state
                  #'reset-exists-copy)
               (race-trace ("~&Ignoring indirectly blocked constraint ~S, blocked inds=~S~%"
                            new-constraint indirectly-blocked-individuals))
               (let ((new-state
                      (changed-kernel-state state
                                            :labels new-labels
                                            :unexpanded-exists-constraints
                                            new-unexpanded-exists-constraints
                                            :unexpanded-exists-constraints-store
                                            new-unexpanded-exists-constraints-store)))
                 (added-constraints-satisfiable nil ; no conclusion concept constraint
                                                nil ; no conclusion relation constraint
                                                new-state
                                                t nil)))
           (if (and create-new-labels
                    (constraint-backpropagated-p new-constraint)
                    (member (constraint-term new-constraint)
                            (label-info-back-propagated-concepts
                             (first (find-label-for-ind ind labels)))))
               (multiple-value-bind (new-unexpanded-exists-constraints
                                     new-unexpanded-exists-constraints-store)
                   (remove-constraint-from-constraint-store
                    new-constraint
                    unexpanded-exists-constraints
                    (state-unexpanded-exists-constraints-store state)
                    (state-copy-unexpanded-exists-constraints-store-p state)
                    state
                    #'reset-exists-copy)
                 (race-trace ("~&Ignoring redundant back-propagated constraint ~S, old/new labels ~S ~S~%"
                              new-constraint labels new-labels))
                 (let ((new-state
                        (changed-kernel-state state
                                              :labels new-labels
                                              :unexpanded-exists-constraints
                                              new-unexpanded-exists-constraints
                                              :unexpanded-exists-constraints-store
                                              new-unexpanded-exists-constraints-store)))
                   (added-constraints-satisfiable nil ; no conclusion concept constraint
                                                  nil ; no conclusion relation constraint
                                                  new-state t nil)))
             (let* ((new-expanded-constraints
                     (racer-remove new-constraint expanded-constraints))
                    (new-state
                     (changed-kernel-state state
                                           :expanded-constraints new-expanded-constraints
                                           :labels new-labels)))
               (expanded-some-satisfiable new-constraint new-state nil nil nil))))))
      (cd-concept 
       (let ((new-state
              (changed-kernel-state state :labels new-labels)))
         (if (constraint-negated-p new-constraint)
             (expanded-ensure-cd-constraint-satisfiable new-constraint new-state nil nil nil)
           (expanded-cd-constraint-satisfiable new-constraint new-state nil nil nil))))
      (general-cd-concept
       (let ((new-state
              (changed-kernel-state state :labels new-labels)))
         (expanded-general-cd-constraint-satisfiable new-constraint new-state nil nil nil)))
      (neg-ria-initial-concept
       (let ((new-state (changed-kernel-state state :labels new-labels)))
         (expanded-ria-initial-satisfiable new-constraint new-state nil nil nil)))
      (neg-ria-final-concept
       (let ((new-state (changed-kernel-state state :labels new-labels)))
         (expanded-ria-final-satisfiable new-constraint new-state nil nil nil)))
      )))

(defun consider-initial-domain-and-range-restrictions (new-relation-constraints
                                                               relation-store)
  (if *use-relation-store*
    (unless (relation-store-empty-p relation-store)
      (collect-role-domain-range-restrictions relation-store))
    (consider-domain-and-range-restrictions-old new-relation-constraints)))

(defun consider-incremental-domain-and-range-restrictions (new-relation-constraints
                                                                    relation-store)
  (if *use-relation-store*
    (when new-relation-constraints
      (loop with ind-table = (racer-make-hash-table :structure-p t)
            for relation-constraint in new-relation-constraints
            for ind-1 = (constraint-ind-1 relation-constraint)
            for ind-2 = (constraint-ind-2 relation-constraint)
            for role = (constraint-term relation-constraint)
            for domain = (role-domain-restriction role)
            for range = (role-range-restriction role)
            for is-filler = nil
            when (and domain
                      (not (is-filler-of-ind-p ind-1 ind-2 role relation-store))
                      (not (member domain (gethash ind-1 ind-table))))
            collect (concluded-concept-constraint ind-1 domain relation-constraint)
            and do 
            (push domain (gethash ind-1 ind-table))
            (setf is-filler t)
            when (and range is-filler (not (member range (gethash ind-2 ind-table))))
            collect (concluded-concept-constraint ind-2 range relation-constraint)
            and do 
            (push range (gethash ind-2 ind-table))))
    (consider-domain-and-range-restrictions-old new-relation-constraints)))


(defun consider-domain-and-range-restrictions-old (relation-constraints)
  (loop with rel-table = (racer-make-hash-table :test 'equal :structure-p t)
        with ind-table = (racer-make-hash-table :structure-p t)
        for relation-constraint in relation-constraints
        for ind-1 = (constraint-ind-1 relation-constraint)
        for ind-2 = (constraint-ind-2 relation-constraint)
        for role = (constraint-term relation-constraint)
        for domain = (role-domain-restriction role)
        for range = (role-range-restriction role)
        when (and domain
                  (not (gethash (cons ind-1 role) rel-table))
                  (not (member domain (gethash ind-1 ind-table))))
        collect (concluded-concept-constraint ind-1 domain relation-constraint)
        and do 
        (setf (gethash (cons ind-1 role) rel-table) t)
        (push domain (gethash ind-1 ind-table))
        when (and range
                  (not (gethash (cons role ind-2) rel-table))
                  (not (member range (gethash ind-2 ind-table))))
        collect (concluded-concept-constraint ind-2 range relation-constraint)
        and do 
        (setf (gethash (cons role (constraint-ind-2 relation-constraint)) rel-table) t)
        (push range (gethash ind-2 ind-table))))

(defun consider-feature-restrictions (old-individuals relation-store)
  (without-optimized-encoding
    (when old-individuals
      (multiple-value-bind (features datatype-features)
          (loop for role in (tbox-encoded-role-list *use-tbox*)
                unless (or (not (role-feature-p role))
                           (role-has-feature-ancestors-p role))
                if (or (role-datatype role) (role-cd-attribute role))
                collect role into datatype-features
                else
                collect role into features
                finally (return (values features datatype-features)))
        (when (or features datatype-features)
          (let ((table (when relation-store
                         (racer-make-hash-table))))
            (if table
                (nconc
                 (loop with datatype-features-atmost-concepts = 
                       (mapcar (lambda (feature)
                                 (encode-concept-term `(d-at-most 1 ,feature)))
                               datatype-features)
                       for ind in old-individuals
                       nconc
                       (loop for concept in datatype-features-atmost-concepts
                             for constraint = (new-concept-constraint ind concept)
                             collect constraint))
                 (when features
                   (let ((at-most-table (racer-make-hash-table)))
                     (loop for feature in features do
                           (setf (gethash (role-name feature) table) nil))
                     (loop for ind in old-individuals
                           for filler-features = (get-filler-roles-of-ind ind relation-store)
                           when filler-features
                           do (clrhash at-most-table)
                           and
                           nconc
                           (loop for feature in filler-features
                                 nconc
                                 (loop for ancestor in (feature-ancestors feature)
                                       for name = (role-name ancestor)
                                       for at-most-concept = nil
                                       for known = nil
                                       do
                                       (unless (role-has-feature-ancestors-p ancestor)
                                         (multiple-value-setq (at-most-concept known) (gethash name table)))
                                       when known
                                       do
                                       (unless at-most-concept
                                         (setf at-most-concept
                                               (if (or (role-datatype ancestor) (role-cd-attribute ancestor))
                                                   (encode-concept-term `(d-at-most 1 ,ancestor))
                                                 (encode-concept-term `(at-most 1 ,ancestor))))
                                         (setf (gethash name table) at-most-concept))
                                       and
                                       unless (gethash name at-most-table)
                                       collect (new-concept-constraint ind at-most-concept)
                                       and
                                       do (setf (gethash name at-most-table) t)))))))
              (loop with all-features = (nconc datatype-features features)
                    with feature-atmost-concepts = 
                    (mapcar (lambda (feature)
                              (if (or (role-datatype feature)
                                      (role-cd-attribute feature))
                                  (encode-concept-term `(d-at-most 1 ,feature))
                                (encode-concept-term `(at-most 1 ,feature))))
                            all-features)
                    with at-most-table = (racer-make-hash-table :test 'equal)
                    initially
                    (loop for feature in all-features do
                          (setf (gethash (role-name feature) at-most-table) nil))
                    for ind in old-individuals
                    nconc
                    (loop for concept in feature-atmost-concepts
                          for feature = (when (at-most-concept-p concept)
                                          (concept-role concept))
                          unless (or (null feature) (gethash (list ind feature) at-most-table))
                          collect (new-concept-constraint ind concept)
                          and
                          do (setf (gethash (list ind feature) at-most-table) t))))))))))

(defun feature-ancestors (feature)
  (if (role-has-feature-ancestors-p feature)
    (role-feature-ancestors feature)
    (list feature)))

(defun merge-violated-abox-feature-relation-constraints (abox
                                                         concept-constraints
                                                         relation-constraints)
  (loop with satisfied-p = t
        with unchanged = nil
        with new-relation-constraints = relation-constraints
        with violated-inds-table = (racer-make-hash-table :test 'equal)
        with removed-inds-table = (racer-make-hash-table)
        do
        (collect-violated-abox-feature-relation-constraints new-relation-constraints
                                                            (clrhash violated-inds-table))
        (multiple-value-bind (satisfied-p-1
                              new-relation-constraints-1
                              changed-p)
            (if (eql (hash-table-count violated-inds-table) 0)
                (values t new-relation-constraints)
              (merge-relation-constraints abox
                                          violated-inds-table
                                          removed-inds-table
                                          new-relation-constraints))
          (if (not satisfied-p-1)
              (setf satisfied-p nil)
            (progn 
              (setf unchanged (not changed-p))
              (when changed-p
                (setf new-relation-constraints new-relation-constraints-1)))))
        until (or unchanged (not satisfied-p))
        finally 
        (when satisfied-p
          (return (values satisfied-p
                          (multiple-value-bind (new-concept-constraints removed-concept-constraints)
                               (when concept-constraints
                                 (merge-concept-constraints removed-inds-table concept-constraints))
                            (racer-merge-remove-concept-constraint-duplicates
                             (nconc new-concept-constraints
                                    (constraint-set-difference concept-constraints
                                                               removed-concept-constraints))))
                          new-relation-constraints
                          (loop for ind being the hash-key of removed-inds-table
                                collect ind))))))

(defun correct-ind-pair (constraint role ancestors)
  (let ((crole (constraint-term constraint)))
    (if (or (eq crole role) (not (lists-disjoint-p (role-feature-ancestors crole) ancestors)))
        (values (constraint-ind-1 constraint) (constraint-ind-2 constraint))
      (values (constraint-ind-2 constraint) (constraint-ind-1 constraint)))))

(defun collect-violated-abox-feature-relation-constraints (relation-constraints violated-inds-table)
  (when relation-constraints
    (loop for relation-constraint in relation-constraints
          for role = (constraint-term relation-constraint)
          for inverse-role = (role-inverse-internal role)
          when (role-feature-p role)
          do
          (push relation-constraint
                (gethash (list (constraint-ind-1 relation-constraint) (role-name role))
                         violated-inds-table))
          (when (role-has-feature-ancestors-p role)
            (loop for ancestor in (rest (role-feature-ancestors role)) do
                  (push relation-constraint
                        (gethash (list (constraint-ind-1 relation-constraint) (role-name ancestor))
                                 violated-inds-table))))
          when (role-feature-p inverse-role)
          do
          (push relation-constraint
                (gethash (list (constraint-ind-2 relation-constraint) (role-name inverse-role))
                         violated-inds-table))
          (when (role-has-feature-ancestors-p inverse-role)
            (loop for ancestor in (rest (role-feature-ancestors inverse-role)) do
                  (push relation-constraint
                        (gethash (list (constraint-ind-2 relation-constraint) (role-name ancestor))
                                 violated-inds-table)))))
    (loop for ind-rel-constraints being the hash-value of violated-inds-table using (hash-key ind-role)
          for role = (get-role (second ind-role))
          for ancestors = (role-feature-ancestors role)
          do
          (unless (and (rest ind-rel-constraints)
                       (multiple-value-bind (ind-1 ind-2)
                           (correct-ind-pair (first ind-rel-constraints) role ancestors)
                         (loop for constraint in (rest ind-rel-constraints)
                               thereis
                               (multiple-value-bind (cind-1 cind-2)
                                   (correct-ind-pair constraint role ancestors)
                                 (and (eql ind-1 cind-1) (not (eql ind-2 cind-2)))))))
            (remhash ind-role violated-inds-table)))))

(defun merge-relation-constraints (abox
                                   violated-inds-table
                                   removed-inds-table
                                   relation-constraints)
  (multiple-value-bind (satisfied-p
                        added-relation-constraints
                        removed-relation-constraints)
      (merge-relation-constraints-1 abox violated-inds-table removed-inds-table)
    (when satisfied-p
      (if (null (or added-relation-constraints removed-relation-constraints))
          (values t relation-constraints)
        (progn
          (multiple-value-bind (added-relation-constraints-1 removed-relation-constraints-1)
              (adjust-relation-constraints added-relation-constraints removed-inds-table)
            #+:debug
            (assert (loop for constraint in added-relation-constraints-1
                          never (or (when (constraint-ind-1-synonyms constraint)
                                      (member (constraint-ind-1 constraint)
                                              (constraint-ind-1-synonyms constraint)))
                                    (when (constraint-ind-2-synonyms constraint)
                                      (member (constraint-ind-2 constraint)
                                              (constraint-ind-2-synonyms constraint))))))
            (setf added-relation-constraints 
                  (nconc added-relation-constraints-1 added-relation-constraints))
            (setf removed-relation-constraints
                  (nconc removed-relation-constraints-1 removed-relation-constraints)))
          (multiple-value-bind (added-relation-constraints-1 removed-relation-constraints-1)
              (adjust-relation-constraints relation-constraints removed-inds-table)
            (setf added-relation-constraints 
                  (nconc added-relation-constraints-1 added-relation-constraints))
            (setf removed-relation-constraints
                  (nconc removed-relation-constraints-1 removed-relation-constraints)))
          (values t
                  (racer-merge-remove-rel-constraint-duplicates
                   (constraint-set-difference (nconc added-relation-constraints
                                                     relation-constraints)
                                              removed-relation-constraints))
                  t))))))

(defun get-true-ind (ind removed-inds-table)
  (gethash ind removed-inds-table ind))

(defun adjust-relation-constraints (relation-constraints removed-inds-table)
  (loop for constraint in relation-constraints
        for old-ind-1 = (constraint-ind-1 constraint)
        for old-ind-2 = (constraint-ind-2 constraint)
        for ind-1 = (get-true-ind old-ind-1 removed-inds-table)
        for ind-2 = (get-true-ind old-ind-2 removed-inds-table)
        for new-constraint = (unless (and (eql old-ind-1 ind-1) (eql old-ind-2 ind-2))
                               (copy-relation-constraint constraint))
        when new-constraint 
        collect new-constraint into new-relation-constraints
        and
        collect constraint into removed-relation-constraints
        and
        do
        (unless (eql old-ind-1 ind-1)
          (setf (constraint-ind-1 new-constraint) ind-1)
          (pushnew old-ind-1 (constraint-ind-1-synonyms new-constraint)))
        (unless (eql old-ind-2 ind-2)
          (setf (constraint-ind-2 new-constraint) ind-2)
          (pushnew old-ind-2 (constraint-ind-2-synonyms new-constraint)))
        #+:debug
        (assert (not (or (when (constraint-ind-1-synonyms new-constraint)
                           (member ind-1 (constraint-ind-1-synonyms new-constraint)))
                         (when (constraint-ind-2-synonyms new-constraint)
                           (member ind-2 (constraint-ind-2-synonyms new-constraint))))))
        finally
        (return (values (racer-merge-remove-rel-constraint-duplicates new-relation-constraints)
                        removed-relation-constraints))))

(defun merge-relation-constraints-1 (abox violated-inds-table removed-inds-table)
  (loop with added-constraints = nil
        for key being the hash-key of violated-inds-table using (hash-value constraints)
        for ind-1 = (get-true-ind (first key) removed-inds-table)
        append constraints into removed-constraints
        do
        (multiple-value-bind (merged-inds merged-roles ind-1-synonyms ind-2-synonyms)
            (loop with merged-inds = nil
                  with merged-roles = nil
                  with ind-1-synonyms = nil
                  with ind-2-synonyms =  nil
                  for constraint in constraints
                  for cind-1 = (get-true-ind (constraint-ind-1 constraint) removed-inds-table)
                  do
                  (if (eql ind-1 cind-1)
                      (progn
                        (pushnew (constraint-term constraint) merged-roles)
                        (push (get-true-ind (constraint-ind-2 constraint) removed-inds-table) merged-inds)
                        (setf ind-2-synonyms
                              (stable-union (constraint-ind-2-synonyms constraint) ind-2-synonyms)))
                    (progn
                      (pushnew (role-inverse-internal (constraint-term constraint)) merged-roles)
                      (push cind-1 merged-inds)
                      (setf ind-1-synonyms
                            (stable-union (constraint-ind-1-synonyms constraint) ind-1-synonyms))))
                  finally (return (values (racer-remove-duplicates merged-inds)
                                          (role-set-remove-duplicates merged-roles)
                                          ind-1-synonyms
                                          ind-2-synonyms)))
          (let* ((ind-2 (first merged-inds))
                 (synonyms (rest merged-inds)))
            (unless (loop for ind in (individual-told-disjoints (find-individual abox ind-2))
                          always (lists-disjoint-p (individual-name-set ind) synonyms))
              (return-from merge-relation-constraints-1 nil))
            (loop for merged-ind in synonyms
                  do (add-synonym-for-ind merged-ind ind-2 removed-inds-table))
            (loop for role in merged-roles do
                  (let ((new-constraint (make-relation-constraint ind-1 ind-2 role)))
                    (setf (constraint-dependencies new-constraint) constraints)
                    (setf (constraint-ind-1-synonyms new-constraint) ind-1-synonyms)
                    (setf (constraint-ind-2-synonyms new-constraint) 
                          (stable-union synonyms ind-2-synonyms))
                    (push new-constraint added-constraints)
                    #+:debug
                    (assert (not (or (when (constraint-ind-1-synonyms new-constraint)
                                       (member (constraint-ind-1 new-constraint)
                                               (constraint-ind-1-synonyms new-constraint)))
                                     (when (constraint-ind-2-synonyms new-constraint)
                                       (member (constraint-ind-2 new-constraint)
                                               (constraint-ind-2-synonyms new-constraint))))))))))
        finally
        (return (values t
                        (racer-merge-remove-rel-constraint-duplicates added-constraints)
                        (constraint-set-remove-duplicates removed-constraints)))))

(defun add-synonym-for-ind (synonym ind table)
  #+:debug (assert (not (eql synonym ind)))
  (loop for key being the hash-key of table using (hash-value value)
        when (eql value synonym)
        do (setf (gethash key table) ind))
  (setf (gethash synonym table) ind))
          

(defun merge-concept-constraints (removed-inds-table concept-constraints)
  (loop for constraint in concept-constraints
        for old-ind = (constraint-ind constraint)
        for ind = (get-true-ind old-ind removed-inds-table)
        unless (eql ind old-ind)
        collect
        (let ((new-constraint (copy-concept-constraint constraint)))
          (setf (constraint-ind new-constraint) ind)
          (pushnew old-ind (constraint-ind-synonyms constraint))
          new-constraint)
        into new-constraints
        and collect constraint into removed-constraints
        finally (return (values new-constraints removed-constraints))))

(defun consider-negated-role-assertions (abox)
  (when (abox-negated-role-axioms abox)
    (loop with tbox = (abox-tbox abox)
          with table = (racer-make-hash-table :test 'equal)
          for ((ind-name-1 ind-name-2) role-name) in (abox-negated-role-axioms abox)
          for role = (if (consp role-name)
                         (role-inverse-internal (get-tbox-role tbox (second role-name)))
                       (get-tbox-role tbox role-name))
          for key = (cons role-name ind-name-2)
          for marker-name-1 = (gethash key table)
          for marker-name-2 = (or marker-name-1
                                  (gensym (concatenate 'string
                                                       (symbol-name (role-name role))
                                                       (symbol-name ind-name-2))))
          for marker-concept = (progn
                                 (unless marker-name-1
                                   (setf (gethash key table) marker-name-2)
                                   (create-tbox-internal-marker-concept tbox marker-name-2))
                                 (encode-concept-term marker-name-2))
          do
          (ensure-individual abox ind-name-1)
          (ensure-individual abox ind-name-2)
          collect (new-concept-constraint ind-name-1 (encode-concept-term `(all ,role ,marker-concept)))
          collect (new-concept-constraint ind-name-2 (concept-negated-concept marker-concept)))))
