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

(defparameter *use-signature-set-unsat-caching* t)
;(setf *use-refined-signature-clash-dependencies* t)
;(setf *use-simplex* nil)

(defstruct (qualified-role-signature
            (:include racer-set-element)
            (:conc-name signature-)
            (:constructor make-signature (ind
                                          dependencies
                                          role
                                          &key
                                          (concepts nil)
                                          (successor-ind-set nil)
                                          (cardinality 1)
                                          (concept-dependencies nil)
                                          (generating-exists-constraint nil))))
  (dependencies nil)                    ; constraints causing this signature
  (role nil)                            ; role of signature
  (concepts nil)                        ; concepts from qualified at-mosts
  (ind nil)                             ; role predecessor
  (successor-ind-set nil)               ; role successor name set (synonyms)
  (cardinality 1)                       ; no. of inds currently represented
  (or-level *or-level*)                 ; adapted or-level
  (concept-dependencies nil)            ; assoc list for concept-dependencies pairs
  (generating-exists-constraint nil)    ; original exists constraint causing to create this signature
  )

(defmethod print-object ((signature qualified-role-signature) stream)
  (let ((ind (signature-ind signature))
        (successor-ind-set (signature-successor-ind-set signature)))
    (if (and (eql ind 0) (null successor-ind-set))
        (format stream "(~D<~D ~S ~S>)"
                (signature-or-level signature)
                (signature-cardinality signature)
                (signature-role signature)
                (signature-concepts signature))
      (format stream "(~D(~A,~A):<~D ~S ~S>)"
              (signature-or-level signature)
              ind
              successor-ind-set
              (signature-cardinality signature)
              (signature-role signature)
              (signature-concepts signature)))))

(defun copy-signature (signature)
  (let ((new-signature (copy-qualified-role-signature signature)))
    (setf (signature-concept-dependencies new-signature)
          (copy-alist (signature-concept-dependencies signature)))
    (setf (signature-or-level new-signature) *or-level*)
    new-signature))

(defvar *ignored-live-individuals-cycles*)

(defun remove-obsolete-label-infos (obsolete-individuals indirectly-blocked-individuals labels)
  (if (and (or obsolete-individuals indirectly-blocked-individuals)
           *gc-of-obsolete-individuals*
           (or (and obsolete-individuals
                    (eql *ignored-live-individuals-cycles* 0))
               indirectly-blocked-individuals))
    (remove-obsolete-label-infos-1 obsolete-individuals indirectly-blocked-individuals labels)
    labels))

(defun remove-obsolete-label-infos-1 (obsolete-individuals indirectly-blocked-individuals labels)
  (when-debug obsolete-individuals
    (race-trace ("~&Removing blocking stack entries for ~S~%" obsolete-individuals)))
  (when-debug indirectly-blocked-individuals
    (race-trace ("~&Cleaning blocking stack entries for ~S~%" indirectly-blocked-individuals)))
  (let* ((all-obsolete-individuals
          (loop with result = obsolete-individuals
                for changed-p = nil
                do
                (loop for label in labels do
                      (when (and (inverse-label-info-p label)
                                 (not (member (label-info-ind label) result))
                                 (member (label-info-previous-ind label) result))
                        (unless changed-p
                          (setf changed-p t))
                        (push (label-info-ind label) result)))
                until (not changed-p)
                finally (return result)))
         (obsolete-in-labels (append all-obsolete-individuals indirectly-blocked-individuals))
         (new-labels
          (loop for label in labels
                unless (or (member (label-info-ind label) all-obsolete-individuals)
                           (not (inverse-label-info-p label))
                           (member (label-info-previous-ind label) all-obsolete-individuals))
                collect
                (if (and obsolete-in-labels (label-info-blocked-constraints label))
                  (let ((new-label (copy-label-info label)))
                    (setf (label-info-blocked-constraints new-label)
                          (loop for blocked-constraint in (label-info-blocked-constraints new-label)
                                unless (member (constraint-ind blocked-constraint) obsolete-in-labels)
                                collect blocked-constraint))
                    (setf (label-info-blocked-labels new-label)
                          (loop for blocked-label in (label-info-blocked-labels new-label)
                                unless (member (label-info-ind blocked-label) obsolete-in-labels)
                                collect blocked-label))
                    (clean-label-info-dependent-models new-label
                                                       (lambda (elem)
                                                         (member (label-info-ind elem)
                                                                 obsolete-in-labels)))
                    (when (null (label-info-blocked-labels new-label))
                      (setf (label-info-blocked-p new-label) nil))
                    new-label)
                  label))))
    (race-trace ("~&Blocking stack: old=~S, new=~S~%" labels new-labels))
    new-labels))

(race-inline (make-concept-list))

(defun make-concept-list (concept)
  (cond ((and-concept-p concept) (copy-list (concept-term concept)))
        ((listp concept) concept)
        (t (list concept))))

(defun signature-backjumping-stopped-here (signatures)
  (signature-backjumping-stopped-here-1 signatures *catching-clash-dependencies*))

(defun signature-backjumping-stopped-here-1 (signatures catching-clash-dependencies)
  (when catching-clash-dependencies
    (if *use-signature-backjumping*
      (loop for constraint in catching-clash-dependencies
            for constraint-signature = (if (qualified-role-signature-p constraint)
                                           constraint
                                         (constraint-signature constraint))
            for successful-signature = constraint-signature
            when (or (and constraint-signature
                          (member constraint-signature signatures))
                     (when *use-refined-signature-clash-dependencies*
                       (setf successful-signature nil)
                       (if (and (concept-constraint-p constraint) (exists-constraint-p constraint))
                           (loop with constraint-ind = (constraint-ind constraint)
                                 for signature in signatures
                                 when (and (eql constraint-ind (signature-ind signature))
                                           (member constraint (signature-dependencies signature))
                                           (let ((exists-concept (constraint-term constraint)))
                                             (and (eq (concept-role exists-concept)
                                                      (signature-role signature))
                                                  (>= (concept-number-restriction exists-concept)
                                                      (signature-cardinality signature))
                                                  (let ((concepts (signature-concepts signature))
                                                        (term (concept-term exists-concept)))
                                                    (or (if (null (rest concepts))
                                                            (eq (first concepts) term)
                                                          (when (and-concept-p term)
                                                            (concept-set-equal concepts (concept-term term))))
                                                        (let ((found (rest (assoc term (signature-concept-dependencies
                                                                                        signature)))))
                                                          (when (and found (member constraint found))
                                                  ;(break "~S ~S" constraint signature)
                                                            t)))))))
                                 do
                                 (setf successful-signature signature)
                                 (return t))
                         (when (relation-constraint-p constraint)
                           (setf successful-signature nil) 
                           (loop with constraint-ind-1 = (constraint-ind-1 constraint)
                                 for signature in signatures
                                 when (and (eql constraint-ind-1 (signature-ind signature))
                                           (member (constraint-ind-2 constraint)
                                                   (signature-successor-ind-set signature))
                                           (eq (signature-role signature) (constraint-term constraint))
                                           (member constraint (signature-dependencies signature)))
                                 do
                                 (setf successful-signature signature)
                                 (return t))))))
            do 
            (progn
              (race-trace ("~&Backjump dependencies succeeded: dep=~S, sig=~S, deps=~S, sigs=~S~%"
                           constraint successful-signature catching-clash-dependencies signatures))
              successful-signature
          
              (return t))
            finally
            (race-trace ("~&Backjump dependencies failed ~S for signatures ~S~%"
                         catching-clash-dependencies signatures))
            (return nil))
      (progn
        (race-trace ("~&Ignoring backjump dependencies ~S for signatures ~S~%"
                     catching-clash-dependencies signatures))
        t))))

(defun add-signature-dependencies (constraints)
  ;;#+:debug
  ;;(when (> (length constraints) 1000)
  ;;  (format t "~&add-signature-dependencies: ~D constraints~%" (length constraints)))
  (loop for constraint in constraints do
        (if (or-constraint-p constraint)
          (pushnew constraint *catching-clash-dependencies*)
          (progn
            ;; Added (constraint-or-dependencies constraint) to the catching clash dependencies
            ;; in case the or-dependencies are defined.
            ;; rm 07/05/02
            (unless (qualified-role-signature-p constraint)
              ;;vh: this makes Racer very slow in backtracking
              ;;vh: I cannot find out why it is necessary: no counter example??!!
              (when (constraint-signature constraint)
                ;(break)
                (add-clash-dependencies (constraint-or-dependencies constraint))))
            (let ((signature (if (qualified-role-signature-p constraint)
                               constraint
                               (constraint-signature constraint))))
              (if signature
                (loop for dependency in (signature-dependencies signature) do
                      (add-clash-dependencies (constraint-or-dependencies dependency))
                      (when (and (relation-constraint-p dependency)
                                 (constraint-signature dependency))
                        (add-clash-dependencies (list (constraint-signature dependency)))))
                (add-clash-dependencies (constraint-or-dependencies constraint)))))))
  (constraint-set-remove-duplicates *catching-clash-dependencies*)
  ;;#+:debug
  ;;(when (> (length *catching-clash-dependencies*) 1000)
  ;;  (format t "~&(length *catching-clash-dependencies*) ~D~%" (length *catching-clash-dependencies*))
  ;;  ;(break)
  ;;  )
  )

(defun clean-catching-clash-dependencies (signatures)
  (set-clash-dependencies (remove-if (lambda (constraint)
                                       (if (qualified-role-signature-p constraint)
                                         (member constraint signatures)
                                         (member (constraint-signature constraint)
                                                 signatures)))
                                     *catching-clash-dependencies*)))

(defun update-catching-clash-dependencies (signatures)
  (set-clash-dependencies (loop with result = nil
                                for element in *catching-clash-dependencies*
                                for signature = (if (qualified-role-signature-p element)
                                                  element
                                                  (constraint-signature element)) do
                                (if (and signature (member signature signatures))
                                  (setf result (union (signature-dependencies signature) result))
                                  (pushnew element result))
                                finally (return result))))

(defun update-clash-dependencies-with-previous-signature (signature
                                                                   all-signatures
                                                                   clean-signatures)
  (when clean-signatures
    (clean-catching-clash-dependencies clean-signatures))
  (let* ((signature (if (qualified-role-signature-p signature)
                      signature
                      (constraint-signature signature)))
         (cardinality-minus (1- (signature-cardinality signature)))
         (cardinality-plus (1+ (signature-cardinality signature))))
    (add-clash-dependencies (loop with role = (signature-role signature)
                                  with concepts = (signature-concepts signature)
                                  with ind = (signature-ind signature)
                                  with succ-ind-set = (signature-successor-ind-set signature)
                                  for element in all-signatures
                                  when (parent-signature-p element
                                                           role
                                                           concepts
                                                           ind
                                                           succ-ind-set
                                                           cardinality-minus
                                                           cardinality-plus)
                                  do (return (list element))))))

#+:debug
(defun top-collected-dependencies ()
  (first *collected-dependencies*))

#-:debug
(defmacro top-collected-dependencies ()
  `(first *collected-dependencies*))

#+:debug
(defun pop-collected-dependencies ()
  (pop *collected-dependencies*))

#-:debug
(defmacro pop-collected-dependencies ()
  `(pop *collected-dependencies*))

(defun add-to-collected-dependencies (dependencies)
  (let ((collected-dependencies (top-collected-dependencies)))
    (when (> (length collected-dependencies) 1000)
      (setf collected-dependencies (clean-constraints *or-level* collected-dependencies)))
    (when (> (length dependencies) 1000)
      (setf dependencies (clean-constraints *or-level* dependencies)))
    (setf (first *collected-dependencies*)
          (union-dependencies dependencies collected-dependencies))))

#+:debug
(defun add-to-collected-ind-dependencies (dependencies)
  (setf (first *collected-ind-dependencies*)
        (union-dependencies dependencies (first *collected-ind-dependencies*))))

#-:debug
(defmacro add-to-collected-ind-dependencies (dependencies)
  `(setf (first *collected-ind-dependencies*)
         (union-dependencies ,dependencies (first *collected-ind-dependencies*))))

(defun parent-signature-p (signature
                              role
                              concepts
                              ind
                              succ-ind-set
                              cardinality-minus
                              cardinality-plus)
  (and (eq role (signature-role signature))
       (eq concepts (signature-concepts signature))
       (eql ind (signature-ind signature))
       (eq succ-ind-set (signature-successor-ind-set signature))
       (let ((card (signature-cardinality signature)))
         (or (eql cardinality-minus card)
             (eql cardinality-plus card)))))

(defun make-and-signature (signature-1 signature-2 role)
  #+:debug (assert (eql (signature-ind signature-1) (signature-ind signature-2)))
  (let* ((top *top-concept*)
         (dependencies-1 (signature-concept-dependencies signature-1))
         (dependencies-2 (signature-concept-dependencies signature-2))
         (top-1 (cdr (assoc top (signature-concept-dependencies signature-1))))
         (top-2 (cdr (assoc top (signature-concept-dependencies signature-2))))
         (dependencies (copy-alist (stable-assoc-union dependencies-1 dependencies-2))))
    (make-signature (signature-ind signature-1)
                    (constraint-set-union (signature-dependencies signature-1)
                                          (signature-dependencies signature-2))
                    role
                    :concepts (concept-set-union (signature-concepts signature-1)
                                                 (signature-concepts signature-2))
                    :successor-ind-set (stable-union (signature-successor-ind-set signature-1)
                                                     (signature-successor-ind-set signature-2))
                    :concept-dependencies
                    (progn
                      (when (or top-1 top-2)
                        (loop for elem in dependencies
                              for new-elem = (copy-list elem)
                              do
                              (when top-1
                                (setf (cdr new-elem) (constraint-set-union top-1 (cdr new-elem))))
                              (when top-2
                                (setf (cdr new-elem) (constraint-set-union top-2 (cdr new-elem))))))
                      dependencies))))

(defun make-signature-from-exists-constraint (constraint no-restriction concepts)
  (let* ((concept (constraint-term constraint))
         (qualification (concept-term concept))
         (signature (constraint-signature constraint)))
    (make-signature (constraint-ind constraint)
                    (list constraint)
                    (concept-role concept)
                    :concepts concepts
                    :cardinality no-restriction
                    :concept-dependencies
                    (if signature
                        (if (set-equal-p concepts (signature-concepts signature))
                            (copy-alist (signature-concept-dependencies signature))
                          (acons qualification
                                 (list* constraint 
                                        (cdr (assoc concept (signature-concept-dependencies signature))))
                                 nil))
                      (acons qualification (list constraint) nil))
                    :generating-exists-constraint constraint)))

(defun create-named-signatures (ind violated-roles relation-constraints store)
  (let* ((relation-constraints (if *use-relation-store* 
                                 (collect-ind-fillers ind store)
                                 relation-constraints))
         (collections
          (when relation-constraints
            (loop with table = (clrhash *signatures-equal-table*)
                  for constraint in relation-constraints
                  for ind-1 = (constraint-ind-1 constraint)
                  when (and (eql ind ind-1)
                            (not (lists-disjoint-p violated-roles
                                                   (role-ancestors-internal
                                                    (constraint-term constraint)))))
                  do (push constraint (gethash (cons ind-1 (constraint-ind-2 constraint))
                                               table))
                  finally
                  (return (loop for collection being the hash-value of table
                                collect collection))))))
    (loop with top = *top-concept*
          for collection in collections
          for role-list = (mapcar #'constraint-term collection)
          for rel-constraint = (first collection)
          collect (make-signature ind 
                                  collection
                                  (if (rest role-list)
                                    (make-nary-and-role role-list)
                                    (first role-list))
                                  :successor-ind-set (list* (constraint-ind-2 rel-constraint)
                                                            (constraint-ind-2-synonyms rel-constraint))
                                  :concept-dependencies (acons top collection nil)))))

(race-inline (make-and-role))

(defun make-and-role (role-1 role-2)
  (cond ((or (eq role-1 role-2) (subrole-p role-1 role-2))
         role-1)
        ((subrole-p role-2 role-1)
         role-2)
        (t (create-internal-and-role *use-tbox* role-1 role-2))))

(defun make-nary-and-role (role-list)
  (let* ((tbox *use-tbox*)
         (bottom-role-p
          (when tbox
            (disjoint-roles-in-role-conjunction-clash-p tbox role-list))))
    (if bottom-role-p
        (tbox-object-bottom-role tbox)
      (loop with and-role = (make-and-role (first role-list) (second role-list))
            for role in (rest (rest role-list))
            do (setf and-role (make-and-role and-role role))
            finally (return and-role)))))

(defun find-signature (ind role concepts signatures)
  (loop with concepts-length = (length concepts)
        for signature in signatures
        when (and (or (null ind) 
                      (if (listp ind)
                        (set-equal-p ind (signature-successor-ind-set signature))
                        (eql ind (signature-successor-ind-set signature))))
                  (eq role (signature-role signature))
                  (set-equal-p concepts (signature-concepts signature) concepts-length))
        do (return signature)))

(defun remove-possibly-redundant-constraints (relation-constraints
                                              exists-constraints
                                              store)
  (loop with redundant-constraints = nil
        with use-relation-store = *use-relation-store*
        with use-alternate-models = *use-alternate-models*
        for selected-constraint in exists-constraints
        for selected-concept = (constraint-term selected-constraint)
        for ind = (constraint-ind selected-constraint)
        for role = (concept-role selected-concept)
        for qualification = (concept-term selected-concept)
        for top-predicate-p = (and (cd-concept-p qualification)
                                   (eq (predicate-operator (concept-predicate qualification)) 'top))
        do
        (when (or top-predicate-p (is-any-top-concept-p qualification))
          (let ((named-count
                 (if use-relation-store
                     (number-of-relation-fillers ind
                                                 role
                                                 store)
                   (count-if #'(lambda (rel-constraint)
                                 (and (eql ind (constraint-ind-1 rel-constraint))
                                      (member role
                                              (role-ancestors-internal
                                               (constraint-term rel-constraint)))))
                             relation-constraints))))
            (when (>= named-count (concept-number-restriction selected-concept))
              (push selected-constraint redundant-constraints))))
        (unless (member selected-constraint redundant-constraints)
          (let ((subsumee (constraint-subsumee-found selected-constraint
                                                     selected-concept
                                                     role
                                                     top-predicate-p
                                                     exists-constraints
                                                     redundant-constraints)))
            (when (and subsumee (not use-alternate-models))
              (push selected-constraint redundant-constraints))))
        finally
        (return redundant-constraints)))

(defun constraint-subsumee-found (subsumer-constraint
                                  subsumer-concept
                                  role
                                  top-predicate-p
                                  exists-constraints
                                  removed-constraints)
  (loop with qualification = (or (concept-role-range subsumer-concept)
                                 (concept-term subsumer-concept))
        with not-subsumer-signature-p = (null (constraint-signature subsumer-constraint))
        for constraint in exists-constraints
        for concept = (constraint-term constraint)
        for exists-qualification = (concept-term concept)
        for concepts = (if (and-concept-p exists-qualification)
                         (concept-term exists-qualification)
                         (list exists-qualification))
        thereis
        (and (not (eq constraint subsumer-constraint))
             (or (null (concept-role-domain concept))
                 (eq (concept-role-domain concept)
                     (concept-role-domain subsumer-concept)))
             (not (member constraint removed-constraints))
             (member role (role-ancestors-internal (concept-role concept)))
             (<= (concept-number-restriction subsumer-concept)
                 (concept-number-restriction concept))
             (or (or top-predicate-p (is-any-top-concept-p qualification))
                 (eq subsumer-concept concept)
                 (concept-clash-p (concept-negated-concept subsumer-concept) concept)
                 (and not-subsumer-signature-p 
                      ;; we cannot remove constraints generated from signature calculus
                      ;; this would remove necessary restrictions
                      concepts
                      (qualification-contained-p qualification concepts)))
             constraint)))

(defun create-signatures (ind
                             state
                             exists-constraints
                             relation-constraints
                             at-least-bounds
                             at-most-bounds
                             store
                             &optional (current-at-most-bound nil))
  (let* ((possibly-redundant-constraints
          (remove-possibly-redundant-constraints relation-constraints
                                                 exists-constraints
                                                 store))
         (mandatory-constraints (constraint-set-difference exists-constraints
                                                           possibly-redundant-constraints))
         (named-signatures
          (create-named-signatures ind
                                   (if current-at-most-bound
                                       (list (bound-role current-at-most-bound))
                                     (mapcar #'bound-role at-most-bounds))
                                   relation-constraints
                                   store))
         (reflexive-role-lists
          (racer-remove-duplicates
           (loop for constraint in mandatory-constraints
                 for role = (concept-role (constraint-term constraint))
                 collect
                 (cons constraint 
                       (loop for anc-role in (role-ancestors-internal role)
                             when (user-defined-role-reflexive-p anc-role)
                             collect anc-role)))))
         (reflexive-role-signatures
          (remove-duplicates
           (loop for (constraint . roles) in reflexive-role-lists
                 nconc
                 (loop for role in roles
                       collect (make-signature ind 
                                               (list constraint)
                                               role
                                               :successor-ind-set (list ind))))
           :key #'signature-role))
         (mandatory-signatures (nconc 
                                (create-signatures-1 ind mandatory-constraints)
                                named-signatures
                                reflexive-role-signatures)))
    (if (or (null possibly-redundant-constraints)
            (signatures-satisfied-p mandatory-signatures at-least-bounds nil state t))
      (sort-signatures at-most-bounds mandatory-signatures)
      (let ((possibly-redundant-signatures
             (create-signatures-1 ind possibly-redundant-constraints)))
        (loop with result = (append possibly-redundant-signatures mandatory-signatures)
              for redundant-signature in possibly-redundant-signatures
              for new-result = (remove redundant-signature result)
              when (signatures-satisfied-p new-result at-least-bounds nil state t)
              do (setf result new-result)
              finally
              (return (sort-signatures at-most-bounds result)))))))

(defun create-signatures-1 (ind exists-constraints)
  (loop with unnamed-signatures = nil
        for constraint in exists-constraints
        for constraint-term = (constraint-term constraint)
        do
        (when (eql ind (constraint-ind constraint))
          (let* ((no-restriction (concept-number-restriction constraint-term))
                 (role (concept-role constraint-term))
                 (concepts (make-concept-list (concept-term constraint-term)))
                 (old-signature (find-signature nil role concepts unnamed-signatures)))
            (if old-signature
                (when (< (signature-cardinality old-signature) no-restriction)
                  (setf unnamed-signatures (delete old-signature unnamed-signatures))
                  (push (make-signature-from-exists-constraint constraint
                                                               no-restriction
                                                               concepts)
                        unnamed-signatures))
              (push (make-signature-from-exists-constraint constraint
                                                           no-restriction
                                                           concepts)
                    unnamed-signatures))))
        finally (return unnamed-signatures)))

(defun sort-signatures (at-most-bounds signatures)
  (let ((table (mapcar #'list at-most-bounds))
        (others nil))
    (loop for signature in signatures do
          (multiple-value-bind (number bound)
                               (get-at-most-bound (signature-role signature)
                                                  (signature-concepts signature)
                                                  at-most-bounds)
            (declare (ignore number))
            (if bound
              (push signature (cdr (assoc bound table)))
              (push signature others))))
    (let ((new-signatures
           (nconc (reduce #'nconc
                          (mapcar #'(lambda (elem)
                                      (sort (sort-signature-qualifications (cdr elem)) #'>
                                            :key #'signature-cardinality))
                                  table)
                          :initial-value nil)
                  (nreverse others))))
      (if *use-unique-name-assumption*
        (nconc (remove-if #'consp new-signatures :key #'signature-successor-ind-set)
               (remove-if #'null new-signatures :key #'signature-successor-ind-set))
        (nconc (remove-if #'null new-signatures :key #'signature-successor-ind-set)
               (remove-if #'consp new-signatures :key #'signature-successor-ind-set))))))

(defun sort-signature-qualifications (signatures)
  (loop with disjoint-crdinalities = nil
        for sig-set on signatures
        for sig-1 = (first sig-set)
        for sig-1-concepts = (signature-concepts sig-1)
        for sig-1-concept = (unless (rest sig-1-concepts)
                              (first sig-1-concepts))
        for no-of-disjoints = 0
        do
        (loop for sig-2 in (rest sig-set)
              for sig-2-concepts = (signature-concepts sig-2)
              for sig-2-concept = (unless (rest sig-2-concepts)
                                    (first sig-2-concepts))
              do
              (if sig-1-concept
                (when (atomic-concept-p sig-1-concept)
                  (if sig-2-concept
                    (when (and (atomic-concept-p sig-2-concept)
                               (member sig-1-concept (concept-told-disjoints sig-2-concept)))
                      (incf no-of-disjoints))
                    (loop for concept in sig-2-concepts
                          do
                          (when (and (atomic-concept-p concept)
                                     (member sig-1-concept (concept-told-disjoints concept)))
                            (incf no-of-disjoints)
                            (return)))))
                (block outerloop
                  (loop with sig-2-concept-disjoints = (and sig-2-concept
                                                            (concept-told-disjoints sig-2-concept))
                        for concept-1 in sig-1-concepts
                        do
                        (when (atomic-concept-p concept-1)
                          (if sig-2-concept
                            (when (and (atomic-concept-p sig-2-concept)
                                       (member concept-1 sig-2-concept-disjoints))
                              (incf no-of-disjoints)
                              (return))
                            (loop for concept-2 in sig-2-concepts
                                  do
                                  (when (and (atomic-concept-p concept-2)
                                             (member concept-1
                                                     (concept-told-disjoints concept-2)))
                                    (incf no-of-disjoints)
                                    (return-from outerloop)))))))))
        (push (cons no-of-disjoints sig-1) disjoint-crdinalities)
        finally
        (return (mapcar #'cdr (stable-sort disjoint-crdinalities #'> :key #'car)))))

(defun make-modified-signature (signature increment-p)
  (when (or (> (signature-cardinality signature) 1) increment-p)
    #+:debug (assert (null (signature-successor-ind-set signature)))
    (let ((new-sig (copy-signature signature)))
      (if increment-p
        (incf (signature-cardinality new-sig))
        (decf (signature-cardinality new-sig)))
      (list new-sig))))

(defun create-constraints-from-signatures (signatures relation-constraints store)
  "Return 2 values: concept-constraints, relation-constraints"
  (with-flatten-encodings
    (without-taxonomic-encoding
      (without-optimized-encoding
        (loop with concept-constraints = nil
              with new-relation-constraints = nil
              for signature in signatures
              for concepts = (signature-concepts signature)
              for concept = (and concepts (encode-concept-term `(and . ,concepts)))
              for role = (signature-role signature)
              for successor-ind-set = (signature-successor-ind-set signature) do
              (if successor-ind-set
                  (progn
                    (when concept
                      (let ((new-constraint
                             (synthesized-concept-constraint (first successor-ind-set) concept signature)))
                        (setf (constraint-backpropagated-p new-constraint) t)
                        (setf (constraint-ind-synonyms new-constraint) (rest successor-ind-set))
                        (pushnew new-constraint concept-constraints :test #'equal)))
                    (unless (member-if #'(lambda (relation-constraint)
                                           (relation-constraints-equal relation-constraint
                                                                       (signature-ind signature)
                                                                       successor-ind-set
                                                                       role))
                                       (if *use-relation-store*
                                           (collect-relation-fillers (signature-ind signature)
                                                                     role
                                                                     store)
                                         relation-constraints))
                      (let ((new-constraint (synthesized-relation-constraint (signature-ind signature)
                                                                             (first successor-ind-set)
                                                                             role
                                                                             signature)))
                        (setf (constraint-ind-2-synonyms new-constraint) (rest successor-ind-set))
                        (push new-constraint new-relation-constraints))
                      (let ((new-constraint (synthesized-relation-constraint (first successor-ind-set)
                                                                             (signature-ind signature)
                                                                             (role-inverse-internal role)
                                                                             signature)))
                        (setf (constraint-ind-1-synonyms new-constraint) (rest successor-ind-set))
                        (push new-constraint new-relation-constraints))))
                (let ((at-least-concept
                       (encode-concept-term
                        (if (or (role-datatype role) (role-cd-attribute role))
                            `(d-at-least ,(signature-cardinality signature) ,role ,concept)
                          `(at-least ,(signature-cardinality signature) ,role ,concept)))))
                  (pushnew (synthesized-concept-constraint (signature-ind signature)
                                                           at-least-concept
                                                           signature)
                           concept-constraints
                           :test #'equal)))
              finally
              (return
               (values (loop with bottom-concept = *bottom-concept*
                             with min-or-level = *or-level*
                             with min-bottom-constraint = nil
                             for constraint in concept-constraints do
                             (when (and (eq (constraint-term constraint) bottom-concept)
                                        (or (null min-bottom-constraint)
                                            (< (constraint-or-level constraint) min-or-level)))
                               (setf min-or-level (constraint-or-level constraint))
                               (setf min-bottom-constraint constraint))
                             finally
                             (if min-bottom-constraint
                                 (return (list min-bottom-constraint))
                               (return concept-constraints)))
                       new-relation-constraints)))))))

(defun signature-merge-relation-concept-constraints (signatures
                                                             remaining-exists-constraints
                                                             state)
  ;; returns 9 values: new-remaining-exists-constraints, unchanged-relation-constraints,
  ;;                   merged-relation-constraints, unchanged-expanded-constraints,
  ;;                   merged-expanded-constraints, new-expanded-store, new-expanded-store-index,
  ;;                   removed individuals, new-unexpanded-exists-constraints-store
  (let ((merged-ind-sets
         (loop for signature in signatures
               for successor-ind-set = (signature-successor-ind-set signature)
               when (rest successor-ind-set)
               collect (list successor-ind-set signature))))
    (multiple-value-bind (unchanged-relation-constraints
                          merged-relation-constraints
                          removed-individuals)
                         (signature-merge-relation-constraints merged-ind-sets state)
      (multiple-value-bind (unchanged-expanded-constraints
                            merged-expanded-constraints
                            new-expanded-store
                            new-expanded-store-index)
                           (signature-merge-concept-constraints merged-ind-sets
                                                                (state-expanded-constraints state)
                                                                (state-expanded-store state)
                                                                (state-expanded-store-index state)
                                                                #'state-copy-expanded-store-p
                                                                #'reset-expanded-copy
                                                                state
                                                                nil
                                                                t)
        (multiple-value-bind
          (unchanged-remaining-exists-constraints
           merged-remaining-exists-constraints
           new-unexpanded-exists-constraints-store)
          (signature-merge-concept-constraints merged-ind-sets
                                               remaining-exists-constraints
                                               (state-unexpanded-exists-constraints-store state)
                                               nil
                                               #'state-copy-unexpanded-exists-constraints-store-p
                                               #'reset-exists-copy
                                               state 
                                               t
                                               t)
          (values (nconc merged-remaining-exists-constraints
                         unchanged-remaining-exists-constraints)
                  unchanged-relation-constraints
                  merged-relation-constraints
                  unchanged-expanded-constraints
                  merged-expanded-constraints
                  new-expanded-store
                  new-expanded-store-index
                  removed-individuals
                  new-unexpanded-exists-constraints-store))))))

(defun signature-merge-relation-constraints (merged-ind-sets state)
  ;;; returns 3 values: unchanged relation constraints, merged relation constraints, removed inds
  (let ((relation-constraints (state-relation-constraints state))
        (relation-store (state-relation-store state)))
    (if merged-ind-sets
      (if *use-relation-store*
        (let ((related-relation-constraints
               (racer-remove-duplicates
                (loop for merged-ind-set in merged-ind-sets
                      append
                      (collect-all-fillers (signature-successor-ind-set (second merged-ind-set))
                                           relation-store)))))
          (signature-merge-relation-constraints-1 merged-ind-sets
                                                  related-relation-constraints
                                                  relation-constraints))
        (signature-merge-relation-constraints-1 merged-ind-sets
                                                relation-constraints
                                                relation-constraints))
      relation-constraints)))

(defun signature-merge-relation-constraints-1 (merged-ind-sets
                                                      related-relation-constraints
                                                      relation-constraints)
  ;;; returns 3 values: unchanged relation constraints, merged relation constraints, removed inds
  (loop with constraint-pairs = nil
        with unchanged-constraints = nil
        with use-relation-store = *use-relation-store*
        for constraint in related-relation-constraints do
        (unless (member constraint constraint-pairs
                        :test (lambda (constraint pair)
                                (or (rel-constraint-equal-test constraint (car pair))
                                    (rel-constraint-equal-test constraint (cdr pair)))))
          (let ((found-1 (member (constraint-ind-1 constraint) merged-ind-sets
                                 :key #'first :test #'member))
                (found-2 (member (constraint-ind-2 constraint) merged-ind-sets
                                 :key #'first :test #'member)))
            (if (and found-1 found-2)
              (let ((new-constraint (copy-relation-constraint constraint))
                    (set (first found-1)))
                (setf (constraint-ind-1 new-constraint) (first (first set)))
                (setf (constraint-ind-1-synonyms new-constraint) (rest (first set)))
                (setf (constraint-ind-2 new-constraint) (first (first (first found-2))))
                (setf (constraint-ind-2-synonyms new-constraint) (rest (first (first found-2))))
                (let ((old-signature (constraint-signature new-constraint)))
                  (when old-signature
                    (pushnew old-signature (constraint-or-dependencies new-constraint))))
                (setf (constraint-signature new-constraint) (second set))
                (push (cons new-constraint constraint) constraint-pairs))
              (if found-1
                (let ((new-constraint (copy-relation-constraint constraint))
                      (set (first found-1)))
                  (setf (constraint-ind-1 new-constraint) (first (first set)))
                  (setf (constraint-ind-1-synonyms new-constraint) (rest (first set)))
                (let ((old-signature (constraint-signature new-constraint)))
                  (when old-signature
                    (pushnew old-signature (constraint-or-dependencies new-constraint))))
                  (setf (constraint-signature new-constraint) (second set))
                  (push (cons new-constraint constraint) constraint-pairs))
                (if found-2
                  (let ((new-constraint (copy-relation-constraint constraint))
                        (set (first found-2)))
                    (setf (constraint-ind-2 new-constraint) (first (first set)))
                    (setf (constraint-ind-2-synonyms new-constraint) (rest (first set)))
                (let ((old-signature (constraint-signature new-constraint)))
                  (when old-signature
                    (pushnew old-signature (constraint-or-dependencies new-constraint))))
                    (setf (constraint-signature new-constraint) (second set))
                    (push (cons new-constraint constraint) constraint-pairs))
                  (unless use-relation-store
                    (push constraint unchanged-constraints)))))))
        finally
        (if (null constraint-pairs)
          (return relation-constraints)
          (return
           (values
            (unless use-relation-store
              (loop for constraint in unchanged-constraints
                    unless (member constraint constraint-pairs
                                   :key #'cdr
                                   :test #'rel-constraint-equal-test)
                    collect constraint))
            (racer-remove-rel-constraint-duplicates (mapcar #'car constraint-pairs))
            (racer-remove-duplicates
             (loop for ind-set in merged-ind-sets
                   append (rest (first ind-set)))))))))

(defun non-det-old-rel-constraint-p (constraints)
  (loop with merging-p = (dl-merging *dl-prover-language*)
        for constraint in constraints
        thereis
        (and (old-individual-p (constraint-ind-1 constraint))
             (old-individual-p (constraint-ind-2 constraint))
             (or (constraint-or-dependencies constraint)
               (when merging-p
                 (let ((signature (constraint-signature constraint)))
                   (when signature
                     (loop for constraint in (signature-dependencies signature)
                           thereis
                           (and (concept-constraint-p constraint)
                                (constraint-negated-p constraint)
                                (let ((term (constraint-term constraint)))
                                  (and (at-least-concept-p term)
                                       (or (> (concept-number-restriction term) 2)
                                           (is-any-top-concept-p term)))))))))))))

#|
(defun inv-rel-constraint-equal-test (rel-constraint-1 rel-constraint-2)
  (and (eql (constraint-ind-1 rel-constraint-1)
            (constraint-ind-2 rel-constraint-2))
       (eql (constraint-ind-2 rel-constraint-1)
            (constraint-ind-1 rel-constraint-2))
       (eq (constraint-term rel-constraint-1)
           (role-inverse-internal (constraint-term rel-constraint-2)))))
|#

(defun signature-merge-concept-constraints (merged-ind-sets
                                            expanded-constraints
                                            expanded-store
                                            expanded-store-index
                                            copy-p-fn
                                            reset-copy-fn
                                            state
                                            init
                                            use-expanded-store-p)
  ;;; returns 3 values: unchanged expanded constraints, merged expanded constraints,
  ;;; new-expanded-store, new-expanded-store-index
  (if merged-ind-sets
    (progn
      (when init
        (loop for ind-set in merged-ind-sets
              do (setf (rest (rest ind-set)) nil)))
      (loop with modified-p = nil
            with removed-inds-set = (racer-remove-duplicates
                                     (loop for merged-ind-set in merged-ind-sets
                                           for removed-inds = (rest (first merged-ind-set))
                                           if (rest removed-inds)
                                           append removed-inds
                                           collect (first removed-inds)))
            for constraint in (if use-expanded-store-p
                                (collect-indset-selected-constraints removed-inds-set 
                                                                     expanded-constraints
                                                                     expanded-store
								     expanded-store-index)
                                expanded-constraints)
            for found = (member (constraint-ind constraint) merged-ind-sets
                                :key #'cdar :test #'member)
            if found
            do
            (push constraint (rest (rest (first found))))
            (setf modified-p t)
            else
            when use-expanded-store-p
            do (error "internal confusion")
            finally
            (if modified-p
              (multiple-value-bind
                (new-expanded-constraints-1 new-expanded-store-1 new-expanded-store-index-1)
                (if use-expanded-store-p
                  (remove-individuals-from-constraint-store removed-inds-set
                                                            expanded-constraints
                                                            expanded-store
                                                            (funcall copy-p-fn state)
                                                            state
                                                            reset-copy-fn
                                                            expanded-store-index)
                  (values (loop for constraint in expanded-constraints
                                unless (member (constraint-ind constraint)
                                               removed-inds-set)
                                collect constraint)
                          expanded-store
                          expanded-store-index))
                (let ((new-merged-expanded-constraints
                       (racer-remove-concept-constraint-duplicates
                        (loop for (ind-set signature . constraints) in merged-ind-sets
                              for ind = (first ind-set)
                              when constraints
                              nconc
                              (mapcar (lambda (constraint)
                                        (let ((new-constraint (copy-concept-constraint constraint)))
                                          (setf (constraint-ind new-constraint) ind)
                                          (setf (constraint-ind-synonyms new-constraint)
                                                (rest ind-set))
                                          (setf (constraint-signature new-constraint) signature)
                                          (push signature
                                                (constraint-or-dependencies new-constraint))
                                          new-constraint))
                                      constraints)))))
                  (multiple-value-bind
                    (new-expanded-constraints-2 new-expanded-store-2 new-expanded-store-index-2)
                    (if use-expanded-store-p
                      (remove-equal-constraints-from-constraint-store new-merged-expanded-constraints
                                                                      new-expanded-constraints-1
                                                                      new-expanded-store-1
                                                                      new-expanded-store-index-1
                                                                      (funcall copy-p-fn state)
                                                                      state
                                                                      reset-copy-fn)
                      (values
                       (stable-constraint-set-difference new-expanded-constraints-1
                                                         new-merged-expanded-constraints)
                       new-expanded-store-1
                       new-expanded-store-index-1))
                    (return (values new-expanded-constraints-2
                                    new-merged-expanded-constraints
                                    new-expanded-store-2
                                    new-expanded-store-index-2)))))
              (return (values expanded-constraints nil expanded-store expanded-store-index)))))
    (values expanded-constraints nil expanded-store expanded-store-index)))

(defun relation-constraints-equal (constraint ind successor-ind role)
  (and (eql (constraint-ind-1 constraint) ind)
       (if (listp successor-ind)
         (member (constraint-ind-2 constraint) successor-ind)
         (eql (constraint-ind-2 constraint) successor-ind))
       (eq (constraint-term constraint) role)))

(defstruct (number-restriction-bound
            (:conc-name bound-)
            #|(:constructor make-bound-internal (role
                                               number
                                               constraint
                                               &key (qualification *top-concept*)))
            |#
           )
  (role nil)
  (concept-list nil)
  (qualification *top-concept*)
  (number nil)
  (constraint nil))

(defun make-bound (role number constraint
                        &key (qualification 
                              (if (role-datatype role)
                                  *datatype-top-concept*
                                  *top-concept*)))
  (make-number-restriction-bound
   :role role
   :concept-list (make-concept-list qualification)
   :qualification qualification
   :number number
   :constraint constraint))

(defmethod print-object ((bound number-restriction-bound) stream)
  (format stream "(~S ~S ~S)"
          (bound-number bound) (bound-role bound) (bound-qualification bound)))

(race-inline (bound-equal get-bound))

(defun bound-equal (role qualification bound)
  (and (eq role (bound-role bound))
       (eq qualification (bound-qualification bound))))

(defun get-bound (role qualification bound-list &key (test #'bound-equal))
  (when bound-list
    (find-if #'(lambda (bound) (funcall test role qualification bound)) bound-list)))

(defun create-at-least-bounds (exists-constraints)
  (loop for constraint in exists-constraints
        for concept = (constraint-term constraint)
        for role = (concept-role concept)
        for no-restriction = (concept-number-restriction concept)
        for qualification = (concept-term concept)
        for old-entry = (get-bound role qualification at-least-bounds)
        if (null old-entry)
        collect (make-bound role no-restriction constraint :qualification qualification)
        into at-least-bounds
        else
        when (> no-restriction (bound-number old-entry))
        do (setf (bound-number old-entry) no-restriction)
        finally (return at-least-bounds)))

(defun get-at-most-bound (role qualification-list bound-list)
  (loop with min-bound = nil
        with bound = nil
        for elem in bound-list
        for at-most-bound = (bound-number elem)
        when (and (subrole-p role (bound-role elem))
                  (or (is-any-top-concept-p (bound-qualification elem))
                      (qualification-contained-p (bound-qualification elem)
                                                 qualification-list)))
        do (if min-bound
             (when (< at-most-bound min-bound)
               (setf min-bound at-most-bound)
               (setf bound elem))
             (progn
               (setf min-bound at-most-bound)
               (setf bound elem)))
        finally (return (values min-bound bound))))

(defun at-most-bounds-violated-p (signatures number at-most-bound)
  (when (> number (bound-number at-most-bound))
    (let* ((true-roles (remove-duplicates (loop for signature in signatures
                                                for role = (signature-role signature)
                                                if (role-internal-conjunction-p role)
                                                append (role-parents-internal role)
                                                else collect role)))
           (common-superroles (if (rest true-roles)
                                (reduce #'intersection true-roles
                                        :key #'role-ancestors-internal)
                                (role-ancestors-internal (first true-roles)))))
      (when common-superroles
        (and (member (bound-role at-most-bound) common-superroles)
             (or (is-any-top-concept-p (bound-qualification at-most-bound))
                 (loop with concept-list = (bound-concept-list at-most-bound)
                       for signature in signatures
                       always (subsetp concept-list (signature-concepts signature)))))))))

(defun create-at-most-bounds (ind state exists-constraints rel-constraints)
  (loop with at-most-bounds = nil
        with expanded-store = (state-expanded-store state)
        with expanded-constraints = (state-expanded-constraints state)
        with use-relation-store = *use-relation-store*
        with relation-constraints = 
        (if use-relation-store
            (collect-relation-filler-constraints ind
                                                 (state-relation-store
                                                  state))
          rel-constraints)
        with exists-roles = (role-set-remove-duplicates
                             (loop for constraint in exists-constraints
                                   collect (concept-role (constraint-term constraint))))
        with top = *top-concept*
        with datatype-top = *datatype-top-concept*
        for constraint in (collect-selected-constraints
                           ind
                           (lambda (constraint)
                             (and (constraint-negated-p constraint)
                                  (let ((concept (constraint-term constraint)))
                                    (and (at-least-concept-p concept)
                                         (member (concept-role concept) exists-roles
                                                 :key #'role-ancestors-internal
                                                 :test #'member)))))
                           expanded-constraints
                           expanded-store
			   (state-expanded-store-index state))
        for concept = (constraint-term constraint)
        for role = (concept-role concept)
        do 
        (if (or (member-if #'(lambda (constraint)
                               (member role
                                       (role-ancestors-internal
                                        (concept-role (constraint-term constraint)))))
                           exists-constraints)
                (and relation-constraints
                     (member-if #'(lambda (constraint)
                                    (member role (role-ancestors-internal
                                                  (constraint-term constraint))))
                                relation-constraints)))
            (let* ((no-restriction (1- (concept-number-restriction concept)))
                   (qualification (concept-term concept))
                   (old-entry (get-bound role qualification at-most-bounds)))
              (if (null old-entry)
                  (push (make-bound role no-restriction constraint :qualification qualification)
                        at-most-bounds)
                (when  (< no-restriction (bound-number old-entry))
                  (setf (bound-number old-entry) no-restriction)
                  (setf (bound-constraint old-entry) constraint)))))
        finally
        (loop for constraint in exists-constraints
              for role = (concept-role (constraint-term constraint))
              do
              (when (role-feature-p role)
                (let ((old-entry (get-bound role
                                            (if (role-datatype role)
                                                datatype-top
                                              top)
                                            at-most-bounds)))
                  (if (null old-entry)
                      (push (make-bound role 1 constraint) at-most-bounds)
                    (when  (> (bound-number old-entry) 1)
                      (setf (bound-number old-entry) 1)
                      (setf (bound-constraint old-entry) nil))))))
        (loop for constraint in relation-constraints
              for role = (constraint-term constraint)
              do
              (when (and (role-feature-p role)
                         (not (get-bound role
                                         (if (role-datatype role)
                                             datatype-top
                                           top)
                                         at-most-bounds)))
                (push (make-bound role 1 nil) at-most-bounds)))
        (return
         (sort at-most-bounds
               (lambda (elem-1 elem-2)
                 (or (and (is-any-top-concept-p (bound-qualification elem-1))
                          (not (is-any-top-concept-p (bound-qualification elem-2))))
                     (and (< (bound-number elem-1) (bound-number elem-2))
                          (or (and (is-any-top-concept-p (bound-qualification elem-1))
                                   (is-any-top-concept-p (bound-qualification elem-2)))
                              (not (or (is-any-top-concept-p (bound-qualification elem-1))
                                       (is-any-top-concept-p (bound-qualification elem-2))))))))))))

(defun find-qualification-matching-constraints (ind-set
                                                qualification-concept
                                                qualification-concepts
                                                verify-only
                                                state)
  (or (find-if-indset-selected-constraint
       ind-set
       (lambda (constraint)
         (and (eq (constraint-concept constraint) qualification-concept)
              #+:debug constraint))
       (state-expanded-constraints state)
       (state-expanded-store state)
       (state-expanded-store-index state))
      (when (and verify-only (rest qualification-concepts))
        (loop for concept in qualification-concepts
              always (find-if-indset-selected-constraint
                      ind-set
                      (lambda (constraint)
                        (declare (ignorable constraint))
                        (and (qualification-contained-p concept qualification-concepts)
                             #+:debug constraint))
                      (state-expanded-constraints state)
                      (state-expanded-store state)
                      (state-expanded-store-index state))))
      (find-if-indset-selected-constraint
       ind-set
       (lambda (constraint)
         (and (eq (constraint-concept constraint) qualification-concept)
              #+:debug constraint))
       (state-unexpanded-exists-constraints state)
       (state-unexpanded-exists-constraints-store state))))

(defun count-signature-instances (bound signatures state &optional (verify-only nil))
  (loop with qualification-concept = (bound-qualification bound)
        with qualification-top-concept-p = 
        (or (is-any-top-concept-p qualification-concept)
            (and (cd-concept-p qualification-concept)
                 (eq (predicate-operator (concept-predicate qualification-concept)) 'top)))
        with role = (bound-role bound)
        with no-restriction = (bound-number bound)
        with cardinality = 0
        with violated-signatures = nil
        with collected-dependency-constraints = nil
        for signature in signatures
        for concepts = (signature-concepts signature)
        when (and (subrole-p (signature-role signature) role)
                  (or qualification-top-concept-p
                      (and concepts (qualification-contained-p qualification-concept concepts))
                      (let ((succ-ind-set (signature-successor-ind-set signature)))
                        (and succ-ind-set
                             (let ((constraint
                                    (find-qualification-matching-constraints succ-ind-set
                                                                             qualification-concept
                                                                             (when verify-only
                                                                               (bound-concept-list bound))
                                                                             verify-only
                                                                             state)))
                               (when constraint
                                 (when (and (not verify-only)
                                            (true-old-individual-p (constraint-ind constraint)))
                                   (push constraint collected-dependency-constraints))
                                 t))))))
        do
        (incf cardinality (signature-cardinality signature))
        (if verify-only
            (when (>= cardinality no-restriction)
              (return cardinality))
          (push signature violated-signatures))
        finally 
        (if verify-only
            (return cardinality)
          (return (values (nreverse violated-signatures) collected-dependency-constraints)))))

(defun count-possible-signature-instances (bound
                                           signatures
                                           state)
  (loop with bound-qualification = (bound-qualification bound)
        with qualification-top-concept-p = (is-any-top-concept-p bound-qualification)
        with negated-qualification = (unless qualification-top-concept-p
                                       (concept-negated-concept bound-qualification))
        with role = (bound-role bound)
        with collected-dependency-constraints = nil
        for signature in signatures
        for concepts = (signature-concepts signature)
        when (and (subrole-p (signature-role signature) role)
                  (or qualification-top-concept-p
                      (and (or (null concepts)
                               (and (not (qualification-contained-p negated-qualification concepts))
                                    (not (qualification-contained-p bound-qualification concepts))))
                           (let ((succ-ind-set (signature-successor-ind-set signature)))
                             (or (null succ-ind-set)
                                 (not 
                                  (let ((constraint 
                                         (or (find-if-indset-selected-constraint
                                              succ-ind-set
                                              (lambda (constraint)
                                                (let ((concept
                                                       (if (constraint-negated-p constraint)
                                                           (concept-negated-concept
                                                            (constraint-term constraint))
                                                         (constraint-term constraint))))
                                                  (and (or (eq concept negated-qualification)
                                                           (eq concept bound-qualification))
                                                       #+:debug constraint)))
                                              (state-expanded-constraints state)
                                              (state-expanded-store state)
                                              (state-expanded-store-index state))
                                             (find-if-indset-selected-constraint
                                              succ-ind-set
                                              (lambda (constraint)
                                                (let ((concept
                                                       (if (constraint-negated-p constraint)
                                                           (concept-negated-concept
                                                            (constraint-term constraint))
                                                         (constraint-term constraint))))
                                                  (and (or (eq concept negated-qualification)
                                                           (eq concept bound-qualification))
                                                       #+:debug constraint)))
                                              (state-unexpanded-exists-constraints state)
                                              (state-unexpanded-exists-constraints-store state)))))
                                    (when constraint
                                      (when (true-old-individual-p (constraint-ind constraint))
                                        (push constraint collected-dependency-constraints))
                                      t))))))))
        collect signature into violated-signatures
        finally 
        (return (values violated-signatures collected-dependency-constraints))))

(defun sum-signatures (signatures)
  (loop for signature in signatures
        sum (signature-cardinality signature)))

(defun signatures-satisfied-p (signatures
                               at-least-bounds
                               current-at-most-bound
                               state
                               &optional (check-at-least nil))
  "Return 4 values: satisfied-p, at-least-violated-p, at-most-violated-p, violated-signatures"
  (if (null signatures)
      t
    (progn
      (when check-at-least
        (loop for at-least-bound in at-least-bounds
              for no-of-instances =
              (count-signature-instances at-least-bound signatures state t)
              when (< no-of-instances (bound-number at-least-bound))
              do (return-from signatures-satisfied-p (values nil at-least-bound))))
      (if current-at-most-bound
          (multiple-value-bind (at-most-violated-signatures dependency-constraints-1)
              (count-signature-instances current-at-most-bound signatures state)
            (let ((no-of-true-signatures (sum-signatures at-most-violated-signatures)))
              (if (> no-of-true-signatures (bound-number current-at-most-bound))
                  (progn
                    #+:debug (assert at-most-violated-signatures)
                    (add-to-collected-ind-dependencies (collect-dependencies dependency-constraints-1))
                    (values nil nil current-at-most-bound at-most-violated-signatures))
                (if (is-any-top-concept-p (bound-qualification current-at-most-bound))
                    t
                  (multiple-value-bind (qualified-at-most-violated-signatures dependency-constraints-2)
                      (count-possible-signature-instances current-at-most-bound
                                                          signatures
                                                          state)
                    (if (> (+ (sum-signatures qualified-at-most-violated-signatures)
                              no-of-true-signatures)
                           (bound-number current-at-most-bound))
                        (progn
                          (add-to-collected-ind-dependencies 
                           (union-dependencies (collect-dependencies dependency-constraints-1)
                                               (collect-dependencies dependency-constraints-2)))
                          (values nil nil nil qualified-at-most-violated-signatures))
                      t))))))
        t))))

(defun signature-violated-p (signatures at-most-bounds)
  (loop for signature in signatures
        for concepts = (signature-concepts signature) do
        (multiple-value-bind (at-most-value at-most-bound)
                             (get-at-most-bound (signature-role signature)
                                                concepts
                                                at-most-bounds)
          (when (and at-most-value (> (signature-cardinality signature) at-most-value))
            (set-clash-dependencies
             (union-dependencies (when (bound-constraint at-most-bound)
                                   (constraint-or-dependencies
                                    (bound-constraint at-most-bound)))
                                 (signature-dependencies signature)))
            (add-to-collected-dependencies *catching-clash-dependencies*)
            (race-trace ("~&Signature ~S for ~S unsatisfiable, dep= ~S, coll-dep= ~S~%"
                         signature
                         (bound-constraint at-most-bound)
                         *catching-clash-dependencies*
                         (top-collected-dependencies)))
            (return t)))))

(defun empty-signature-role (signatures)
  (loop with bottom-concept = *bottom-concept*
        for signature in signatures do
        (when (eq (role-range-restriction (signature-role signature)) bottom-concept)
          (return signature))))

(defun make-merged-signatures (target-signature
                               source-signature
                               signatures
                               current-at-most-bound)
  "Returns 2 values: new-signatures, added-signatures"
  (let ((target-ind-set (signature-successor-ind-set target-signature))
        (source-ind-set (signature-successor-ind-set source-signature)))
    (when (and source-ind-set (null target-ind-set))
      (let ((tmp1 target-ind-set)
            (tmp2 target-signature))
        (setf target-ind-set source-ind-set)
        (setf source-ind-set tmp1)
        (setf target-signature source-signature)
        (setf source-signature tmp2)))
    (let* ((new-signatures signatures)
           (new-concepts (concept-set-union (signature-concepts target-signature)
                                            (signature-concepts source-signature)))
           (new-role (make-and-role (signature-role target-signature)
                                    (signature-role source-signature)))
           (new-target-ind-set (stable-union (signature-successor-ind-set target-signature)
                                             (signature-successor-ind-set source-signature)))
           (added-signatures nil))
      (let ((old-signature (find-signature new-target-ind-set new-role new-concepts signatures)))
        (cond
         ((or (null old-signature)
              (and (not (eq target-signature old-signature))
                   (not (eq source-signature old-signature))))
          (if (and old-signature (null (signature-successor-ind-set old-signature)))
            (progn
              (setf added-signatures (make-modified-signature old-signature t))
              (setf new-signatures (remove old-signature new-signatures)))
            (let ((and-signature (make-and-signature target-signature
                                                     source-signature
                                                     new-role)))
              (push and-signature added-signatures)))
          (setf added-signatures (nconc (make-modified-signature target-signature nil)
                                        (make-modified-signature source-signature nil)
                                        added-signatures))
          (setf new-signatures
                (append added-signatures
                        (delete source-signature (remove target-signature new-signatures)))))
         ((eq source-signature old-signature)
          (setf added-signatures (make-modified-signature target-signature nil))
          (setf new-signatures (append added-signatures
                                       (remove target-signature new-signatures))))
         ((eq target-signature old-signature)
          (setf added-signatures (make-modified-signature source-signature nil))
          (setf new-signatures (append added-signatures
                                       (remove source-signature new-signatures))))
         (t (error "This should never happen")))
        (let ((constraint (bound-constraint current-at-most-bound)))
          (when constraint
            (loop with top = *top-concept*
                  for signature in added-signatures do
                  (pushnew constraint (signature-dependencies signature))
                  (let ((concept-dependencies (signature-concept-dependencies signature)))
                    (when concept-dependencies
                      (unless (assoc top concept-dependencies)
                        (setf concept-dependencies (acons top nil concept-dependencies))
                        (setf (signature-concept-dependencies signature) concept-dependencies))
                      (loop for elem in concept-dependencies do
                            (pushnew constraint (cdr elem))))))))
        (values new-signatures added-signatures)))))

(defun copy-related-at-least-bounds (at-least-bounds violated-role)
  (loop for bound in at-least-bounds
        when (member violated-role (role-ancestors-internal (bound-role bound)))
        collect bound))

(defun merge-constraints (ind state unused-1 unused-2 unused-3)
  (declare #+:allegro (:explain :tailmerging)
           (ignore unused-1 unused-2 unused-3))
  (let* ((true-some-constraints (state-true-some-constraints state))
         (at-most-bounds (create-at-most-bounds ind
                                                state
                                                true-some-constraints
                                                nil))
         (old-at-least-bounds (state-at-least-bounds state))
         (violated-role (state-violated-role state)))
    (when old-at-least-bounds
      (let* ((same-inds-p (eql ind (constraint-ind (bound-constraint (first old-at-least-bounds)))))
             (copied-at-least-bounds (when same-inds-p
                                       (copy-related-at-least-bounds old-at-least-bounds violated-role))))
        (if (not (and same-inds-p copied-at-least-bounds))
            (progn
      (setf (state-at-least-bounds state) nil)
      (setf (state-current-at-most-bound state) nil)
      (setf (state-remaining-at-most-bounds state) nil)
      (setf (state-role-related-exists-constraints state) nil))
          (setf (state-at-least-bounds state) copied-at-least-bounds))))
    (let ((at-least-bounds (union (state-at-least-bounds state)
                                  (create-at-least-bounds true-some-constraints)
                                  :test
                                  (lambda (b1 b2)
                                    (and (eq (bound-role b1) (bound-role b2))
                                         (eql (bound-number b1) (bound-number b2))
                                         (eq (bound-qualification b1) (bound-qualification b2)))))))
      (multiple-value-bind (unused new-unexpanded-exists-constraints-store)
          (if (and *use-unexpanded-exists-constraints-store*
                   (state-unexpanded-exists-constraints-store state))
              (remove-constraints-from-constraint-store
               true-some-constraints
               nil
               (state-unexpanded-exists-constraints-store state)
               (state-copy-unexpanded-exists-constraints-store-p state)
               state
               #'reset-exists-copy)
            (values nil (state-unexpanded-exists-constraints-store state)))
        (declare (ignore unused))
        (if (use-simplex-p state at-most-bounds)
            (let ((new-state
                   (changed-signature-state state
                                            :at-least-bounds at-least-bounds
                                            :at-most-bounds at-most-bounds
                                            :unexpanded-exists-constraints-store
                                            new-unexpanded-exists-constraints-store)))
              (merge-constraints-simplex ind new-state nil nil nil))
          (let* ((new-at-most-bounds
                  (reorder-at-most-bounds at-most-bounds
                                          (state-at-most-constraint state)))
                 (new-state 
                  (changed-signature-state state
                                           :at-least-bounds at-least-bounds
                                           :at-most-bounds new-at-most-bounds
                                           :unexpanded-exists-constraints-store
                                           new-unexpanded-exists-constraints-store)))
            (merge-constraints-signatures ind new-state nil nil nil)))))))

(defun use-simplex-p (state at-most-bounds)
  (and *use-simplex*
       (not *inverse-roles*)
       (or (if *use-relation-store*
             (relation-store-empty-p (state-relation-store state))
             (null (state-relation-constraints state)))
           *use-simplex-for-individuals*)
       (let ((tbox (state-tbox (state-parameters state))))
         (or (null tbox)
             (null (tbox-meta-constraint-concept tbox))
             (not (dl-merging *dl-prover-language*))
             (not (dl-merging (concept-language (tbox-meta-constraint-concept tbox))))))
       (or *always-use-simplex*
           (some (lambda (x) (> (bound-number x) 1)) at-most-bounds))
       (or (not (dl-reflexive-roles *dl-prover-language*))
           (not (member-if #'user-defined-role-reflexive-p at-most-bounds :key #'bound-role)))))

(defun reorder-at-most-bounds (at-most-bounds at-most-constraint)
  (if at-most-constraint
    (let ((selected-bound (find at-most-constraint at-most-bounds :key #'bound-constraint)))
      #+:debug (assert selected-bound)
      (if (eq selected-bound (first at-most-bounds))
        at-most-bounds
        (cons selected-bound (remove selected-bound at-most-bounds))))
    at-most-bounds))

(defvar *signature-set-unsat-cache*)

(defun merge-constraints-signatures (ind state unused-1 unused-2 unused-3)
  (declare (ignore unused-1 unused-2 unused-3))
  (let ((true-some-constraints (state-true-some-constraints state))
        (at-least-bounds (state-at-least-bounds state))
        (at-most-bounds (state-at-most-bounds state))
        (relation-constraints (state-relation-constraints state)))
    (let ((signatures (create-signatures ind
                                         state
                                         true-some-constraints
                                         relation-constraints
                                         at-least-bounds
                                         at-most-bounds
                                         (state-relation-store state)
                                         (when *use-unique-name-assumption*
                                           (first at-most-bounds)))))
      (if (signature-violated-p signatures at-most-bounds)
        (handle-clash-with-backtrack-state state nil nil nil nil)
        (let ((selected-bound (first at-most-bounds)))
          (multiple-value-bind (satisfied-p
                                at-least-violated-p
                                at-most-violated-p
                                violated-signatures)
                               (signatures-satisfied-p signatures
                                                       at-least-bounds
                                                       selected-bound
                                                       state
                                                       t)
            (if at-least-violated-p
              (progn
                (set-clash-dependencies signatures)
                (add-signature-dependencies signatures)
                (race-trace ("~&At-least constraint ~S violated for initial signatures ~S, dep=~S~%"
                             (bound-constraint at-least-violated-p)
                             signatures
                             *catching-clash-dependencies*))
                (handle-clash-with-backtrack-state state nil nil nil nil))
              (progn
                (push nil *signature-clash-reasons*)
                (push nil *signature-set-unsat-cache*)
                (flet ((merge-constraints-signatures-continuation (sat-p
                                                                   xstate
                                                                   unused-1 unused-2 unused-3)
                         (declare (ignore unused-1 unused-2 unused-3))
                         (pop *signature-clash-reasons*)
                         (pop *signature-set-unsat-cache*)
                         (if sat-p
                           (added-constraints-satisfiable nil nil xstate t nil)
                           (handle-clash-with-backtrack-state state nil nil nil nil))))
                  (if satisfied-p
                    (progn
                      (race-trace ("~&Initial signatures partially satisfied: ~S ~S, state=~S~%"
                                   selected-bound signatures state))
                      (let* ((new-partially-expanded-or-stack
                              (push-backtrack-stack #'merge-constraints-signatures-continuation
                                                    (state-partially-expanded-or-stack state)))
                             (new-state
                              (changed-signature-state state
                                                       :current-at-most-bound selected-bound
                                                       :remaining-at-most-bounds
                                                       at-most-bounds
                                                       :partially-expanded-or-stack
                                                       new-partially-expanded-or-stack)))
                        (remaining-at-most-bounds-satisfiable signatures
                                                              new-state
                                                              nil nil nil)))
                    (if (signature-violated-p signatures at-most-bounds)
                      (handle-clash-with-backtrack-state state nil nil nil nil)
                      (progn
                        #+:debug (assert (or at-most-violated-p violated-signatures))
                        (let* ((new-partially-expanded-or-stack
                                (push-backtrack-stack #'merge-constraints-signatures-continuation
                                                      (state-partially-expanded-or-stack state)))
                               (new-state
                                (changed-signature-state state
                                                         :current-at-most-bound
                                                         selected-bound
                                                         :remaining-at-most-bounds
                                                         at-most-bounds
                                                         :role-related-exists-constraints
                                                         true-some-constraints
                                                         :partially-expanded-or-stack
                                                         new-partially-expanded-or-stack)))
                          (race-trace ("~&Starting with bound ~S and signatures ~S, state=~S~%"
                                       selected-bound signatures new-state))
                          (merge-split-signatures violated-signatures
                                                  signatures
                                                  new-state
                                                  at-most-violated-p
                                                  nil))))))))))))))

(defun select-split-signature (signatures qualification negated-qualification state)
  (loop for signature in signatures
        for concepts = (signature-concepts signature)
        when (and (or (null concepts)
                      (and (not (qualification-contained-p qualification concepts))
                           (not (qualification-contained-p negated-qualification
                                                           concepts))))
                  (let ((succ-ind-set (signature-successor-ind-set signature)))
                    (or (null succ-ind-set)
                        (not (or (indset-selected-constraints-p
                                  succ-ind-set
                                  (lambda (constraint)
                                    (let ((concept
                                           (if (constraint-negated-p constraint)
                                               (concept-negated-concept
                                                (constraint-term constraint))
                                             (constraint-term constraint))))
                                      (or (eq concept negated-qualification)
                                          (eq concept qualification))))
                                  (state-expanded-constraints state)
                                  (state-expanded-store state)
                                  (state-expanded-store-index state))
                                 (indset-selected-constraints-p
                                  succ-ind-set
                                  (lambda (constraint)
                                    (let ((concept
                                           (if (constraint-negated-p constraint)
                                               (concept-negated-concept
                                                (constraint-term constraint))
                                             (constraint-term constraint))))
                                      (or (eq concept negated-qualification)
                                          (eq concept qualification))))
                                  (state-unexpanded-exists-constraints state)
                                  (state-unexpanded-exists-constraints-store state)))))))
        do (return signature)))

(defun merge-split-signatures (violated-signatures all-signatures state merging-required-p unused-1)
  (declare (ignore unused-1))
  (let ((all-at-most-bounds (state-at-most-bounds state)))
    (if (signature-violated-p all-signatures all-at-most-bounds)
        (handle-clash-with-backtrack-state state nil nil nil nil)
      (let* ((qualification (bound-qualification (state-current-at-most-bound state)))
             (negated-qualification (concept-negated-concept qualification))
             (selected-split
              (and (not (is-any-top-concept-p qualification))
                   (not merging-required-p)
                   (select-split-signature violated-signatures qualification negated-qualification state))))
        (if selected-split
            (split-signatures selected-split
                              all-signatures
                              state
                              nil nil)
          (if violated-signatures
              (progn
                (race-trace ("~&Trying to merge signatures ~S~%" violated-signatures))
                (merge-signatures violated-signatures all-signatures state nil nil))
            (error "internal error")))))))

(defun merge-signatures (violated-signatures all-signatures state unused-1 unused-2)
  (declare (ignore unused-1 unused-2))
  (race-trace ("~&Trying to merge signature set ~S, all signatures=~S, state=~S~%"
               violated-signatures all-signatures state))
  #+:debug (assert (rest violated-signatures))
  (let ((culprits (when *use-signature-set-unsat-caching*
                    (signature-set-unsat-p (make-signature-set-key violated-signatures)))))
    (if culprits
      (progn
        (add-clash-dependencies (map-signature-culprits culprits violated-signatures))
        (race-trace ("~&Failed merging of signature set ~S due to cache entry ~S, dep=~S~%"
                     violated-signatures culprits *catching-clash-dependencies*))
        (handle-clash-with-backtrack-state state nil nil nil nil))
      (merge-signatures-outer-loop violated-signatures
                                     all-signatures
                                     state
                                     nil nil))))

(defun merge-signatures-outer-loop (violated-signatures
                                           all-signatures
                                           state
                                           unused-1 unused-2)
  (declare (ignore unused-1 unused-2))
  (let ((signatures violated-signatures))
    (labels
      ((merge-signatures-outer-loop-continuation (sat-p xstate unused-1 unused-2 terminate-loop-p)
         (declare (ignore unused-1 unused-2))
         (if sat-p
           (added-constraints-satisfiable nil nil xstate t nil)
           (if terminate-loop-p
               (if (eq terminate-loop-p ':<=1)
                   (handle-clash-with-backtrack-state state nil nil ':<=1 nil)
                 (handle-clash-with-backtrack-state state nil nil nil nil))
             (progn
               (setf signatures (rest signatures))
               (if (rest signatures)
                 (let* ((signature-1 (first signatures))
                        (culprit (list signature-1))
                        (new-signatures (rest signatures))
                        (new-partially-expanded-or-stack
                         (push-backtrack-stack #'merge-signatures-outer-loop-continuation
                                               (state-partially-expanded-or-stack state)))
                        (new-state (changed-signature-state state
                                                            :partially-expanded-or-stack
                                                            new-partially-expanded-or-stack)))
                   (merge-signatures-inner-loop all-signatures
                                                  new-state
                                                  new-signatures
                                                  signature-1
                                                  culprit))
                 (progn
                   (race-trace ("~&Merging of signature set ~S failed, dep=~S~%"
                                violated-signatures *catching-clash-dependencies*))
                   (handle-clash-with-backtrack-state state nil nil nil nil))))))))
      (let* ((signature-1 (first signatures))
             (culprit (list signature-1))
             (new-signatures (rest signatures))
             (new-partially-expanded-or-stack
              (push-backtrack-stack #'merge-signatures-outer-loop-continuation
                                    (state-partially-expanded-or-stack state)))
             (new-state (changed-signature-state state
                                                 :partially-expanded-or-stack
                                                 new-partially-expanded-or-stack)))
        (merge-signatures-inner-loop all-signatures
                                       new-state
                                       new-signatures
                                       signature-1
                                       culprit)))))

(defun merge-signatures-inner-loop (all-signatures
                                    state
                                    signatures
                                    signature-1
                                    culprit)
  (let ((remaining-signatures signatures))
    (labels
        ((merge-signatures-inner-loop-continuation (sat-p xstate unused-1 unused-2 terminate-loop-p)
           (declare (ignore unused-1 unused-2))
           (if sat-p
               (added-constraints-satisfiable nil nil xstate t nil)
             (if terminate-loop-p
                 (if (or (eq terminate-loop-p ':outer) (eq terminate-loop-p ':<=1))
                     (progn
                       (race-trace ("~&Terminate loop due to ~S skip: Skipping merging of ~S with remaining signatures ~S, ~
                               intial signature set ~S~%"
                                    terminate-loop-p signature-1 remaining-signatures signatures))
                       (if (eq terminate-loop-p ':outer)
                           (handle-clash-with-backtrack-state state nil nil ':inner nil)
                         (handle-clash-with-backtrack-state state nil nil ':<=1 nil)))
                   (progn
                     (race-trace ("~&Terminate loop due to inner skip: Skipping merging of ~S with remaining signatures ~S, ~
                               intial signature set ~S~%"
                                  signature-1 remaining-signatures signatures))
                     (handle-clash-with-backtrack-state state nil nil nil nil)))
               (progn
                 ;(break "merge-signatures-inner-loop: ~S" remaining-signatures)
                 (setf remaining-signatures (rest remaining-signatures))
                 (if remaining-signatures
                     (let* ((new-partially-expanded-or-stack
                             (push-backtrack-stack #'merge-signatures-inner-loop-continuation
                                                   (state-partially-expanded-or-stack state)))
                            (new-state (changed-signature-state state
                                                                :partially-expanded-or-stack
                                                                new-partially-expanded-or-stack)))
                       (select-inner-loop-pair all-signatures
                                               new-state
                                               remaining-signatures
                                               signature-1
                                               culprit))
                   (handle-clash-with-backtrack-state state nil nil nil nil)))))))
      (let* ((new-partially-expanded-or-stack
              (push-backtrack-stack #'merge-signatures-inner-loop-continuation
                                    (state-partially-expanded-or-stack state)))
             (new-state (changed-signature-state state
                                                 :partially-expanded-or-stack
                                                 new-partially-expanded-or-stack)))
        (select-inner-loop-pair all-signatures
                                new-state
                                signatures
                                signature-1
                                culprit)))))

(defun select-inner-loop-pair (all-signatures
                                           state
                                           signatures
                                           signature-1
                                           culprit)
  (let ((signature-2 (first signatures))
        (collected-culprits nil))
    (flet 
      ((select-inner-loop-pair-continuation (sat-p xstate unused-1 unused-2 terminate-loop-p)
         (declare (ignore unused-1 unused-2))
         (if sat-p
           (progn
             (decf *or-level*)
             (added-constraints-satisfiable nil nil xstate t nil))
           (let ((signature-1-stopped (signature-backjumping-stopped-here culprit))
                 (signature-2-stopped (signature-backjumping-stopped-here
                                       (list signature-2)))
                 (terminate-loop-p (when (eq terminate-loop-p ':<=1)
                                     :<=1)))
             ;(break "select-inner-loop-pair: ~S" signatures)
             (if signature-2-stopped
                 (progn
                   (pushnew signature-2 collected-culprits)
                   (if signature-1-stopped
                       (progn
                         (pushnew signature-1 collected-culprits)
                         (remove-dependencies (cons signature-2 culprit)))
                     (remove-dependency signature-2))
                   (add-clash-dependencies collected-culprits)
                   (decf *or-level*)
                   (handle-clash-with-backtrack-state state nil nil terminate-loop-p nil))
               (if signature-1-stopped
                 (let ((terminate-loop-p (or terminate-loop-p ':inner)))
                   (pushnew signature-1 collected-culprits)
                   (remove-dependency signature-1)
                   (add-clash-dependencies collected-culprits)
                   (race-trace ("~&Backjumping with ~S condition, dep=~S~%"
                                terminate-loop-p *catching-clash-dependencies*))
                   (decf *or-level*)
                   (handle-clash-with-backtrack-state state nil nil terminate-loop-p nil))
                 (if (and *catching-clash-dependencies*
                          (null (rest *catching-clash-dependencies*))
                          (or (qualified-role-signature-p (first *catching-clash-dependencies*))
                              (constraint-signature (first *catching-clash-dependencies*)))
                          (consp *signature-clash-reasons*)
                          (first *signature-clash-reasons*)
                          (no-at-most-constraint-p nil (first *signature-clash-reasons*)))
                     (let ((terminate-loop-p (or terminate-loop-p ':outer)))
                       (update-clash-dependencies-with-previous-signature
                        (first *catching-clash-dependencies*)
                        all-signatures
                        nil)
                       (race-trace ("~&Skipping merging of ~S with set ~S: unsatisfiable signature restrictions: ~
                                   continuing signature backjumping with dep= ~S, ~S~%"
                                    signature-1
                                    signatures
                                    (first *signature-clash-reasons*)
                                    *catching-clash-dependencies*))
                       ;(break "2: ~S ~S " *signature-clash-reasons* *catching-clash-dependencies*)
                       (add-clash-dependencies collected-culprits)
                       (race-trace ("~&Backjumping with ~S condition, dep=~S~%"
                                    terminate-loop-p *catching-clash-dependencies*))
                       (decf *or-level*)
                       (handle-clash-with-backtrack-state state nil nil terminate-loop-p nil))
                   (if (signature-backjumping-stopped-here signatures)
                     (progn
                       (add-clash-dependencies collected-culprits)
                       (decf *or-level*)
                       (handle-clash-with-backtrack-state state nil nil terminate-loop-p nil))
                     (let ((terminate-loop-p (or terminate-loop-p ':outer)))
                       (update-catching-clash-dependencies (cons signature-2 culprit))
                       (race-trace ("~&Skipping merging of ~S with set ~S: continuing signature backjumping with dep=~S~%"
                                    signature-1
                                    signatures
                                    *catching-clash-dependencies*))
                       ;(break "3: ~S ~S " *signature-clash-reasons* *catching-clash-dependencies*)
                       (add-clash-dependencies collected-culprits)
                       (race-trace ("~&Backjumping with ~S condition, dep=~S~%"
                                    terminate-loop-p *catching-clash-dependencies*))
                       (decf *or-level*)
                       (handle-clash-with-backtrack-state state nil nil terminate-loop-p nil))))))))))
      (let* ((new-state-1 (copy-signature-kernel-state state t))
             (new-partially-expanded-or-stack
              (push-backtrack-stack #'select-inner-loop-pair-continuation
                                    (state-partially-expanded-or-stack new-state-1)))
             (new-state-2
              (changed-signature-state new-state-1
                                       :partially-expanded-or-stack 
                                       new-partially-expanded-or-stack)))
        (incf *or-level*)
        (merge-signature-pair signature-1
                              signature-2
                              all-signatures
                              new-state-2
                              nil)))))

(defun no-at-most-constraint-p (added-signatures signature-clash-reasons)
  (let ((result
         (or (null added-signatures)
             (loop with found = nil
                   for constraint in signature-clash-reasons
                   for result =
                   (cond ((constraint-signature constraint)
                          (if (culprit-signature-p (constraint-or-dependencies constraint)
                                                   added-signatures)
                            (setf found t)
                            (return)))
                         ((and (constraint-or-dependencies constraint)
                               (culprit-signature-p (constraint-or-dependencies constraint)
                                                    added-signatures))
                          ;(break "1:~S" signature-clash-reasons)
                          (setf found t))
                         ((and (concept-constraint-p constraint)
                               (constraint-negated-p constraint)
                               (at-least-concept-p
                                (constraint-term constraint)))
                          (return))
                         (t t))
                   until (null result)
                   finally (return found)))))
    (if result
      t
      (progn
        ;(break "2:~S" signature-clash-reasons)
        nil))))

(defun culprit-signature-p (or-dependencies added-signatures)
  (loop for signature-1 in added-signatures
        for role = (signature-role signature-1)
        for dependencies = (signature-dependencies signature-1)
        for concepts = (signature-concepts signature-1)
        for ind = (signature-ind signature-1)
        for succ-ind-set = (signature-successor-ind-set signature-1)
        thereis
        (loop for constraint in or-dependencies
              for signature-2 = (if (qualified-role-signature-p constraint)
                                  constraint
                                  (constraint-signature constraint))
              thereis
              (and signature-2
                   (matching-signature-p signature-2
                                         role
                                         dependencies
                                         concepts
                                         ind
                                         succ-ind-set)))))

(defun matching-signature-p (signature role dependencies concepts ind succ-ind-set)
  (and (eq role (signature-role signature))
       (eq dependencies (signature-dependencies signature))
       (eq concepts (signature-concepts signature))
       (eql ind (signature-ind signature))
       (eq succ-ind-set (signature-successor-ind-set signature))))

(defun make-signature-set-key (signatures)
  (let* ((card 0)
         (key (sort (loop for signature in signatures
                          for sig-card = (signature-cardinality signature)
                          when (signature-successor-ind-set signature)
                          do (return-from make-signature-set-key nil)
                          collect (list (signature-role signature)
                                        sig-card
                                        (sort-concept-list (copy-list (signature-concepts signature))))
                          do (incf card sig-card))
                    (lambda (signatures-key-1 signatures-key-2)
                      (destructuring-bind (role-1 card-1 concepts-1) signatures-key-1
                        (destructuring-bind (role-2 card-2 concepts-2) signatures-key-2
                          (or (< (role-hash-id role-1) (role-hash-id role-2))
                              (and (eq role-1 role-2)
                                   (or (< card-1 card-2)
                                       (and (eql card-1 card-2)
                                            (subsetp concepts-1 concepts-2)))))))))))
    (cons card key)))

(defun signature-set-unsat-p (signature-set-key-1)
  (when (and *use-signature-set-unsat-caching*
             (first *signature-set-unsat-cache*))
    (let ((entry (gethash signature-set-key-1 (first *signature-set-unsat-cache*))))
      (when entry
        (return-from signature-set-unsat-p
          (values entry signature-set-key-1))))
    #|(loop with signature-set-key-1-card = (first signature-set-key-1)
          with signature-set-key-1-rest = (rest signature-set-key-1)
          with signature-set-key-1-length = (length signature-set-key-1)
          for signature-set-key-2 being the hash-key of (first *signature-set-unsat-cache*)
          using (hash-value entry)
          for subsetp = (and (<= (length signature-set-key-2) signature-set-key-1-length)
                             (subset-signature-set-key-p signature-set-key-2
                                                         signature-set-key-1-card
                                                         signature-set-key-1-rest))
          when subsetp
          do (return (values entry signature-set-key-2)))|#
    nil))

(defun subset-signature-set-key-p (signature-set-key-1
                                        signature-set-key-2-card
                                        signature-set-key-2-rest)
  (and (<= (first signature-set-key-1) signature-set-key-2-card)
       (loop with top = *top-concept*
             for (role-1 card-1 concepts-1) in (rest signature-set-key-1)
             for new-concepts-1 = (remove top concepts-1)
             always
             (loop for signature-key-2 in signature-set-key-2-rest
                   thereis
                   (subset-signature-key-p role-1 card-1 new-concepts-1 signature-key-2)))))

(defun subset-signature-key-p (role-1 card-1 concepts-1 signature-key-2)
  (destructuring-bind (role-2 card-2 concepts-2) signature-key-2
    (and (<= card-1 card-2)
         (subrole-p role-2 role-1)
         (subsetp concepts-1 (remove *top-concept* concepts-2)))))

(defun add-unsat-entry (signature-set-key culprits)
  (unless (signature-set-unsat-p signature-set-key)
    (unless (first *signature-set-unsat-cache*)
      (setf (first *signature-set-unsat-cache*)
            (racer-make-hash-table :test 'equal :structure-p t)))
    (setf (gethash signature-set-key (first *signature-set-unsat-cache*)) culprits)))

(defun map-signature-culprits (culprits signatures)
  (loop for culprit in culprits
        unless (member culprit signatures)
        collect culprit into removed-culprits
        and
        nconc (find-matching-culprits culprit signatures) into added-culprits
        finally
        (if removed-culprits
          (return (nconc (racer-remove-duplicates added-culprits)
                         (constraint-set-difference culprits removed-culprits)))
          (return culprits))))

(defun find-matching-culprits (culprit signatures)
  (loop with role = (signature-role culprit)
        with concepts = (signature-concepts culprit)
        with ind = (signature-ind culprit)
        for signature in signatures
        when (and (roles-intersect-p role (signature-role signature))
                  (eql ind (signature-ind signature))
                  (not (lists-disjoint-p concepts (signature-concepts signature))))
        collect signature into new-culprits
        finally
        (if (null new-culprits)
          (error "internal problem")
          (return new-culprits))))

(defun merge-signature-pair (signature
                             selected-signature
                             all-signatures
                             state
                             unused)
  (flet ((merge-signature-pair-continuation (sat-p
                                             xstate
                                             unsat-signature-set-key
                                             culprits
                                             terminate-loop-p)
           (if sat-p
               (added-constraints-satisfiable nil nil xstate t nil)
             (progn
               (when (and unsat-signature-set-key *use-signature-set-unsat-caching*)
                 (add-unsat-entry unsat-signature-set-key culprits))
               (handle-clash-with-backtrack-state state nil nil terminate-loop-p nil)))))
    (let* ((new-partially-expanded-or-stack
            (push-backtrack-stack #'merge-signature-pair-continuation
                                  (state-partially-expanded-or-stack state)))
           (new-state (changed-signature-state state
                                               :partially-expanded-or-stack
                                               new-partially-expanded-or-stack)))
      (merge-signature-pair-1 signature
                              selected-signature
                              all-signatures
                              new-state
                              unused))))

(defun compute-new-signature-culprits (catching-clash-dependencies)
  (loop for element in catching-clash-dependencies
        for signature = (if (qualified-role-signature-p element)
                            element
                          (constraint-signature element))
        when signature
        collect signature))

(defun merge-signature-pair-1 (signature
                               selected-signature
                               all-signatures
                               state
                               unused)
  (declare (ignore unused))
  (let ((selected-successor-ind-set (signature-successor-ind-set selected-signature))
        (successor-ind-set (signature-successor-ind-set signature))
        (abox *current-abox*)
        (current-at-most-bound (state-current-at-most-bound state))
        (signature-pair (list signature selected-signature)))
    (if (and selected-successor-ind-set
             successor-ind-set
             (lists-disjoint-p selected-successor-ind-set successor-ind-set)
             (or (null *use-tbox*)
                 (null abox)
                 (individual-sets-disjoint-p-internal abox
                                                      selected-successor-ind-set
                                                      successor-ind-set)))
        (progn
          (add-clash-dependencies signature-pair) ; was previously set-clash-dependencies
          (when (bound-constraint current-at-most-bound)
            (add-clash-dependencies (list (bound-constraint current-at-most-bound)))
            (add-to-collected-dependencies (constraint-or-dependencies
                                            (bound-constraint current-at-most-bound))))
          (add-to-collected-dependencies signature-pair)
          (add-to-collected-dependencies (first *collected-ind-dependencies*))
          (race-trace ("~&Old individuals NOT mergable ~S, ~S, dep=~S, coll-dep=~S~%"
                       signature
                       selected-signature
                       *catching-clash-dependencies*
                       (top-collected-dependencies)))
        ;(break "~S ~S" *collected-dependencies* current-at-most-bound)
          (handle-clash-with-backtrack-state state nil nil nil nil))
      (let* ((role-disjoint-roles-1 (role-disjoint-roles (signature-role signature)))
             (selected-role (signature-role selected-signature))
             (role-disjoint-roles-2 (role-disjoint-roles selected-role)))
        (if (and role-disjoint-roles-1
                 role-disjoint-roles-2
                 (not (role-set-disjoint-p role-disjoint-roles-1 (role-ancestors-internal selected-role))))
            (progn
              (add-clash-dependencies signature-pair) ; was previously set-clash-dependencies
              (when (bound-constraint current-at-most-bound)
                (add-clash-dependencies (list (bound-constraint current-at-most-bound)))
                (add-to-collected-dependencies (constraint-or-dependencies
                                                (bound-constraint current-at-most-bound))))
              (add-to-collected-dependencies signature-pair)
              (add-to-collected-dependencies (first *collected-ind-dependencies*))
              (race-trace ("~&Signatures NOT mergable due to disjoint roles ~S, ~S, dep=~S, coll-dep=~S~%"
                           signature
                           selected-signature
                           *catching-clash-dependencies*
                           (top-collected-dependencies)))
              (handle-clash-with-backtrack-state state nil nil nil nil))
          (let ((clashing-concepts
                 (or (concept-set-simple-clash-p (signature-concepts signature)
                                                 (signature-concepts selected-signature))
                     ;(clashing-disjoints-p (signature-concepts signature)
                     ;                      (signature-concepts selected-signature))
                     )))
            (if clashing-concepts
                (multiple-value-bind (clashing-concept-1 clashing-concept-2)
                    (if (listp clashing-concepts)
                        (values-list clashing-concepts)
                      (values clashing-concepts
                              (when (or (atomic-concept-p clashing-concepts)
                                        (negated-concept-p clashing-concepts))
                                (concept-negated-concept clashing-concepts))))
                  (if *use-refined-signature-clash-dependencies*
                      (set-clash-dependencies
                       (refine-signature-clash-dependencies clashing-concept-1
                                                            clashing-concept-2
                                                            (first signature-pair)
                                                            (when clashing-concept-2
                                                              (second signature-pair))
                                                            (list (first signature-pair))
                                                            (when clashing-concept-2
                                                              (rest signature-pair))))
                    (set-clash-dependencies signature-pair))
                  ;(break)
                  (when (bound-constraint current-at-most-bound)
                    (add-clash-dependencies (list (bound-constraint current-at-most-bound)))
                    (add-to-collected-dependencies (constraint-or-dependencies
                                                    (bound-constraint current-at-most-bound))))
                  (add-to-collected-dependencies signature-pair)
                  (add-to-collected-dependencies (first *collected-ind-dependencies*))
                  (race-trace ("~&Signatures NOT mergable due to clashing qualifications ~S, ~S, dep=~S, coll-dep=~S~%"
                               signature
                               selected-signature
                               *catching-clash-dependencies*
                               (top-collected-dependencies)))
                  (handle-clash-with-backtrack-state state nil nil nil nil))
              (progn
                (race-trace ("~&Trying to merge ~S with ~S in ~S~%"
                             signature selected-signature all-signatures))
                (multiple-value-bind (new-signatures added-signatures)
                    (make-merged-signatures signature
                                            selected-signature
                                            all-signatures
                                            current-at-most-bound)
                  (let ((new-signature-set-key (when *use-signature-set-unsat-caching*
                                                 (make-signature-set-key new-signatures))))
                    (multiple-value-bind (culprits culprit-signature-set-key)
                        (when new-signature-set-key
                          (signature-set-unsat-p new-signature-set-key))
                      (declare (ignore culprit-signature-set-key))
                      (if culprits
                          (progn
                            (set-clash-dependencies (map-signature-culprits culprits all-signatures))
                            (add-to-collected-dependencies *catching-clash-dependencies*)
                            (race-trace ("~&Failed merging of signature set ~S and pair ~S ~
                                due to cache entry, dep=~S, coll-dep=~S~%"
                                         all-signatures
                                         culprits
                                         *catching-clash-dependencies*
                                         (top-collected-dependencies)))
                            (handle-clash-with-backtrack-state state nil nil nil nil))
                        (let ((unsatisfiable-signature (empty-signature-role new-signatures)))
                          (if unsatisfiable-signature
                              (let ((culprits (list selected-signature signature)))
                                (set-clash-dependencies (list selected-signature signature))
                                (add-to-collected-dependencies *catching-clash-dependencies*)
                                (race-trace ("~&Signature ~S has unsatisfiable range restriction for role ~S, ~
                                    dep= ~S, coll-dep=~S~%"
                                             unsatisfiable-signature
                                             (signature-role unsatisfiable-signature)
                                             *catching-clash-dependencies*
                                             (top-collected-dependencies)))
                                (handle-clash-with-backtrack-state state
                                                                   new-signature-set-key
                                                                   culprits
                                                                   nil nil))
                            (multiple-value-bind (satisfied-p
                                                  at-least-violated-p
                                                  at-most-violated-p
                                                  violated-signatures)
                                (signatures-satisfied-p new-signatures
                                                        (state-at-least-bounds state)
                                                        current-at-most-bound
                                                        state
                                                        t)
                              (cond
                               (satisfied-p
                                (race-trace ("~&merged ~S and ~S --> ~S~%" selected-signature signature new-signatures))
                                (race-trace ("~&Signatures ~S partially satisfied for ~S~%"
                                             new-signatures current-at-most-bound))
                                (labels
                                    ((merge-signature-pair-1-continuation-1 (sat-p
                                                                             xstate
                                                                             unused-1
                                                                             unused-2
                                                                             unused-3)
                                       (declare (ignore unused-1 unused-2 unused-3))
                                       (if sat-p
                                           (added-constraints-satisfiable nil nil xstate t nil)
                                         (let ((related-signatures (constraint-set-union added-signatures signature-pair))
                                               (signature-culprits (list selected-signature signature)))
                                           (if (signature-backjumping-stopped-here related-signatures)
                                               (if (and (null (rest new-signatures))
                                                        (or *use-unique-name-assumption*
                                                            (null (rest (signature-successor-ind-set
                                                                         (first new-signatures)))))
                                                        (eql (bound-number current-at-most-bound) 1))
                                                   (progn
                                                     (update-catching-clash-dependencies related-signatures)
                                                     (race-trace ("~&<= 1 condition: Continuing signature backjumping with dep= ~S, ~
                                                     merging of pair ~S with ~S failed due to at-most 1 restriction ~S~%"
                                                                  *catching-clash-dependencies*
                                                                  signature-pair
                                                                  new-signatures
                                                                  current-at-most-bound))
                                                     (handle-clash-with-backtrack-state state
                                                                                        new-signature-set-key
                                                                                        signature-culprits
                                                                                        ':<=1
                                                                                        nil))
                                                 (if (and *catching-clash-dependencies*
                                                          (null (rest *catching-clash-dependencies*))
                                                          (or (qualified-role-signature-p (first *catching-clash-dependencies*))
                                                              (constraint-signature (first *catching-clash-dependencies*)))
                                                          (consp *signature-clash-reasons*)
                                                          (first *signature-clash-reasons*)
                                                          (no-at-most-constraint-p added-signatures
                                                                                   (first *signature-clash-reasons*)))
                                                     (progn
                                                       (update-clash-dependencies-with-previous-signature
                                                        (first *catching-clash-dependencies*)
                                                        all-signatures
                                                        related-signatures)
                                                       (if *catching-clash-dependencies*
                                                           (let ((culprits (list selected-signature signature)))
                                                             (race-trace ("~&Skipping merging of pair ~S with ~S ~
                                                                           unsatisfiable signature restrictions: continuing signature backjumping with dep= ~S, ~S~%"
                                                                          signature-pair
                                                                          new-signatures
                                                                          (first *signature-clash-reasons*)
                                                                          *catching-clash-dependencies*))
                                                             ;(break "1: ~S ~S " *signature-clash-reasons* *catching-clash-dependencies*)
                                                             (handle-clash-with-backtrack-state state
                                                                                                new-signature-set-key
                                                                                                culprits
                                                                                                nil nil))
                                                         (let ((culprits (list selected-signature signature)))
                                                           (race-trace ("~&Stopped signature backjumping for pair ~S with set ~S, dep= ~S~%"
                                                                        signature-pair
                                                                        new-signatures
                                                                        *catching-clash-dependencies*))
                                                           (set-clash-dependencies signature-pair)
                                                           (setf (first *signature-clash-reasons*) nil)
                                                           ;(break "2: ~S ~S " *signature-clash-reasons* *catching-clash-dependencies*)
                                                           (handle-clash-with-backtrack-state state
                                                                                              new-signature-set-key
                                                                                              culprits
                                                                                              nil nil))))
                                                   (progn
                                                     (race-trace ("~&Stopped signature backjumping for pair ~S with set ~S, dep= ~S~%"
                                                                  signature-pair
                                                                  new-signatures
                                                                  *catching-clash-dependencies*))
                                                     (clean-catching-clash-dependencies related-signatures)
                                                     (add-clash-dependencies signature-pair)
                                                     (handle-clash-with-backtrack-state state
                                                                                        new-signature-set-key
                                                                                        signature-culprits
                                                                                        nil nil))))
                                             (let ((new-signature-culprits
                                                    (compute-new-signature-culprits *catching-clash-dependencies*)))
                                               (when new-signature-culprits
                                                 (setf signature-culprits new-signature-culprits)
                                                 (setf new-signature-set-key
                                                       (make-signature-set-key signature-culprits)))
                                               (clean-catching-clash-dependencies related-signatures)
                                               (race-trace ("~&Skipping merging of ~S with ~S: continuing signature ~
                                                             backjumping with dep= ~S~%"
                                                            signature-pair
                                                            new-signatures
                                                            *catching-clash-dependencies*))
                                               (handle-clash-with-backtrack-state state
                                                                                  new-signature-set-key
                                                                                  signature-culprits
                                                                                  nil nil)))))))
                                  (let* ((new-partially-expanded-or-stack
                                          (push-backtrack-stack
                                           #'merge-signature-pair-1-continuation-1
                                           (state-partially-expanded-or-stack state)))
                                         (new-state (changed-signature-state state
                                                                             :partially-expanded-or-stack
                                                                             new-partially-expanded-or-stack)))
                                    (remaining-at-most-bounds-satisfiable new-signatures
                                                                          new-state
                                                                          nil nil nil))))
                               (at-least-violated-p
                                (set-clash-dependencies signature-pair)
                                (add-signature-dependencies added-signatures)
                                (race-trace ("~&At-least constraint ~S violated for merged signatures ~S, dep=~S~%"
                                             (bound-constraint at-least-violated-p)
                                             added-signatures
                                             *catching-clash-dependencies*))
                                (handle-clash-with-backtrack-state state nil nil nil nil))
                               (at-most-violated-p
                                #+:debug (assert violated-signatures)
                                (if (null (rest violated-signatures))
                                    (progn
                                      (set-clash-dependencies signature-pair)
                                      (race-trace ("~&At-most constraint ~S violated for merger candidates ~S, ~S, dep=~S~%"
                                                   (bound-constraint at-most-violated-p)
                                                   selected-signature
                                                   signature
                                                   *catching-clash-dependencies*))
                                      (handle-clash-with-backtrack-state state nil nil nil nil))
                                  (progn
                                    (race-trace ("~&merged ~S and ~S --> ~S~%" selected-signature signature new-signatures))
                                    (labels
                                        ((merge-signature-pair-1-continuation-2 (sat-p
                                                                                 xstate
                                                                                 unused-1
                                                                                 unused-2
                                                                                 terminate-loop-p)
                                           (declare (ignore unused-1 unused-2))
                                           (if sat-p
                                               (added-constraints-satisfiable nil nil xstate t nil)
                                             (let ((related-signatures (constraint-set-union added-signatures signature-pair))
                                                   (signature-culprits (list selected-signature signature)))
                                               (declare (ignorable signature-culprits))
                                               (if (signature-backjumping-stopped-here related-signatures)
                                                   (if (and nil (null (rest new-signatures))
                                                        (or *use-unique-name-assumption*
                                                            (null (rest (signature-successor-ind-set
                                                                         (first new-signatures)))))
                                                        (eql (bound-number current-at-most-bound) 1))
                                                   (progn
                                                     (update-catching-clash-dependencies related-signatures)
                                                     (race-trace ("~&<= 1 condition: Continuing signature backjumping with dep= ~S, ~
                                                     merging of pair ~S with ~S failed due to at-most 1 restriction ~S~%"
                                                                  *catching-clash-dependencies*
                                                                  signature-pair
                                                                  new-signatures
                                                                  current-at-most-bound))
                                                     (handle-clash-with-backtrack-state state
                                                                                        new-signature-set-key
                                                                                        signature-culprits
                                                                                        ':<=1
                                                                                        nil))
                                                     (if (and *catching-clash-dependencies*
                                                            (null (rest *catching-clash-dependencies*))
                                                            (or (qualified-role-signature-p (first *catching-clash-dependencies*))
                                                                (constraint-signature (first *catching-clash-dependencies*)))
                                                            (consp *signature-clash-reasons*)
                                                            (first *signature-clash-reasons*)
                                                            (no-at-most-constraint-p added-signatures
                                                                                     (first *signature-clash-reasons*)))
                                                       (let (#+:debug (signature-clash-reasons (first *signature-clash-reasons*)))
                                                         #+:debug (declare (ignore signature-clash-reasons))
                                                         (update-clash-dependencies-with-previous-signature
                                                          (first *catching-clash-dependencies*)
                                                          all-signatures
                                                          related-signatures)
                                                         (if *catching-clash-dependencies*
                                                             (progn
                                                               (race-trace ("~&skipping merging of pair ~S with ~S: ~
                                                                             Unsatisfiable signature restrictions: ~
                                                                             continuing signature backjumping with dep= ~S, ~S~%"
                                                                            signature-pair
                                                                            new-signatures
                                                                            (first *signature-clash-reasons*)
                                                                            *catching-clash-dependencies*))
                                                               ;(break "3: ~S ~S " *signature-clash-reasons* *catching-clash-dependencies*)
                                                               (handle-clash-with-backtrack-state state nil nil terminate-loop-p nil))
                                                           (progn
                                                             (race-trace ("~&Stopped signature backjumping for pair ~S with ~S, dep= ~S~%"
                                                                          signature-pair
                                                                          new-signatures
                                                                          *catching-clash-dependencies*))
                                                             (set-clash-dependencies signature-pair)
                                                             (setf (first *signature-clash-reasons*) nil)
                                                             ;(break "4: ~S ~S " *signature-clash-reasons* *catching-clash-dependencies*)
                                                             (handle-clash-with-backtrack-state state nil nil terminate-loop-p nil))))
                                                     (progn
                                                       (race-trace ("~&Stopped signature backjumping for pair ~S with ~S, dep= ~S~%"
                                                                    signature-pair
                                                                    new-signatures
                                                                    *catching-clash-dependencies*))
                                                       (set-clash-dependencies signature-pair)
                                                       (handle-clash-with-backtrack-state state nil nil terminate-loop-p nil))))
                                                 (let ((culprits (list selected-signature signature)))
                                                   (clean-catching-clash-dependencies related-signatures)
                                                   (race-trace ("~&Skipping merging of pair ~S with ~S: continuing ~
                                                                 signature backjumping with dep= ~S~%"
                                                                signature-pair
                                                                new-signatures
                                                                *catching-clash-dependencies*))
                                                   (handle-clash-with-backtrack-state state
                                                                                      new-signature-set-key
                                                                                      culprits
                                                                                      terminate-loop-p
                                                                                      nil)))))))
                                      (let* ((new-partially-expanded-or-stack
                                              (push-backtrack-stack #'merge-signature-pair-1-continuation-2
                                                                    (state-partially-expanded-or-stack state)))
                                             (new-state (changed-signature-state state
                                                                                 :partially-expanded-or-stack
                                                                                 new-partially-expanded-or-stack)))
                                        (merge-signatures violated-signatures
                                                                     new-signatures
                                                                     new-state
                                                                     nil nil))))))
                               (violated-signatures
                                (race-trace ("~&merged ~S and ~S --> ~S~%" selected-signature signature new-signatures))
                                (labels
                                    ((merge-signature-pair-1-continuation-3 (sat-p
                                                                             xstate
                                                                             unused-1
                                                                             unused-2
                                                                             unused-3)
                                       (declare (ignore unused-1 unused-2 unused-3))
                                       (if sat-p
                                           (added-constraints-satisfiable nil nil xstate t nil)
                                         (let ((related-signatures (constraint-set-union added-signatures signature-pair)))
                                           (if (signature-backjumping-stopped-here related-signatures)
                                               (let ((culprits (list selected-signature signature)))
                                                 (race-trace ("~&Stopped signature backjumping for pair ~S with ~S, ~
                                                               dep= ~S~%"
                                                              signature-pair
                                                              new-signatures
                                                              *catching-clash-dependencies*))
                                                 (add-clash-dependencies signature-pair) ; was previously set-clash-dependencies
                                                 (handle-clash-with-backtrack-state state
                                                                                    new-signature-set-key
                                                                                    culprits
                                                                                    nil nil))
                                             (let ((culprits (list selected-signature signature)))
                                               (clean-catching-clash-dependencies related-signatures)
                                               (race-trace ("~&Skipping merging of pair ~S with ~S: ~
                                                             continuing signature backjumping with dep= ~S~%"
                                                            signature-pair
                                                            new-signatures
                                                            *catching-clash-dependencies*))
                                               (handle-clash-with-backtrack-state state
                                                                                  new-signature-set-key
                                                                                  culprits
                                                                                  nil nil)))))))
                                  (let* ((new-partially-expanded-or-stack
                                          (push-backtrack-stack
                                           #'merge-signature-pair-1-continuation-3
                                           (state-partially-expanded-or-stack state)))
                                         (new-state (changed-signature-state state
                                                                             :partially-expanded-or-stack
                                                                             new-partially-expanded-or-stack)))
                                    (merge-split-signatures violated-signatures new-signatures new-state nil nil))))
                               (t (error "internal error"))))))))))))))))))

(defun clashing-disjoints-p (concept-set-1 concept-set-2)
  (loop for concept-1 in concept-set-1
        for told-disjoints = (concept-told-disjoints concept-1)
        when told-disjoints
        do
        (loop for concept-2 in concept-set-2
              unless (concept-set-disjoint-p told-disjoints (concept-told-subsumers concept-2))
              do (return-from clashing-disjoints-p (list concept-1 concept-2)))))

(defun clean-constraints (current-or-level constraints)
  ;; needed to enable tail merging in ACL
  (loop for dependency in constraints
        if (qualified-role-signature-p dependency)
        when (<= (signature-or-level dependency) current-or-level)
        collect dependency into result
        end
        else
        if (<= (constraint-or-level dependency) current-or-level)
        collect dependency into result
        else
        when (and (constraint-signature dependency)
                  (loop for dependency in (signature-dependencies (constraint-signature dependency))
                        thereis (<= (constraint-or-level dependency) current-or-level)))
        collect dependency into result
        finally
        (when-debug (not (eql (length result) (length constraints)))
          (race-trace ("~&Removed clash dependencies with a level higher than ~D: ~S~%"
                       current-or-level
                       (constraint-set-difference constraints result))))
        (return result)))

(defun remaining-at-most-bounds-satisfiable (signatures state unused-1 unused-2 unused-3)
  (declare (ignore unused-1 unused-2 unused-3))
  (let (#+:debug (remaining-at-most-bounds (state-remaining-at-most-bounds state))
        #+:debug (current-at-most-bound (state-current-at-most-bound state))
        (relation-constraints (state-relation-constraints state))
        (remaining-exists-constraints (state-unexpanded-exists-constraints state)))
    #+:debug (assert (eq (first remaining-at-most-bounds) current-at-most-bound))
    (multiple-value-bind (new-constraints new-relation-constraints-1)
                         (create-constraints-from-signatures signatures
                                                             relation-constraints
                                                             (state-relation-store state))
      (multiple-value-bind (new-remaining-exists-constraints
                            unchanged-relation-constraints
                            merged-relation-constraints
                            unchanged-expanded-constraints
                            merged-expanded-constraints
                            new-expanded-store
                            new-expanded-store-index
                            removed-individuals
                            new-unexpanded-exists-constraint-store)
                           (signature-merge-relation-concept-constraints signatures
                                                                         remaining-exists-constraints
                                                                         state)
        (let ((new-relation-constraints-2 (nconc merged-relation-constraints
                                                 new-relation-constraints-1)))
          (flet
            ((remaining-at-most-bounds-satisfiable-continuation (sat-p
                                                                 xstate
                                                                 unused-1 unused-2 unused-3)
               (declare (ignore unused-1 unused-2 unused-3))
               (if sat-p
                 (progn
                   (race-trace ("~&satisfied signatures succeeded: ~S ~
                                 derived constraints= ~S ~S~%"
                                signatures new-constraints new-relation-constraints-2))
                   (added-constraints-satisfiable nil nil xstate t nil))
                 (let ((collected-dependencies (top-collected-dependencies)))
                   (declare (ignorable collected-dependencies))
                   (add-to-collected-dependencies *catching-clash-dependencies*)
                   ;;; we need to make sure that recent clashes do not shadow backtracking stops for previous clashes
                   ;(add-clash-dependencies collected-dependencies) ;;;vh (2011-04-26): superfluous??!!
                   (set-clash-dependencies
                    (racer-remove-duplicates (clean-constraints *or-level* *catching-clash-dependencies*)))
                   (race-trace ("~&satisfied signatures failed: ~S~% failed rel-constraints ~S, ~
                                 dep=~S, coll-dep=~S~%"
                                signatures
                                new-relation-constraints-2
                                *catching-clash-dependencies*
                                (top-collected-dependencies)))
                   (race-trace ("~&Added collected dependencies to clash dependencies: ~S~%"
                                collected-dependencies))
                   (handle-clash-with-backtrack-state state nil nil nil nil)))))
            (let* ((relation-store (state-relation-store state))
                   (new-relation-store
                    (if (and removed-individuals relation-store *use-relation-store*)
                      (update-relation-store removed-individuals
                                             nil ;new-relation-constraints-2
                                             state
                                             (non-det-old-rel-constraint-p new-relation-constraints-2))
                      relation-store))
                   (new-partially-expanded-or-stack
                    (push-backtrack-stack #'remaining-at-most-bounds-satisfiable-continuation
                                          (state-partially-expanded-or-stack state)))
                   (new-state
                    (changed-signature-state state
                                             :unexpanded-exists-constraints
                                             new-remaining-exists-constraints
                                             :expanded-constraints
                                             unchanged-expanded-constraints
                                             :relation-constraints
                                             unchanged-relation-constraints
                                             :relation-store new-relation-store
                                             :expanded-store new-expanded-store
                                             :expanded-store-index new-expanded-store-index
                                             :unexpanded-exists-constraints-store
                                             new-unexpanded-exists-constraint-store
                                             :partially-expanded-or-stack
                                             new-partially-expanded-or-stack)))
              (added-derived-constraints-satisfiable (signature-ind (first signatures))
                                                     (nconc merged-expanded-constraints
                                                            new-constraints)
                                                     new-relation-constraints-2
                                                     new-state
                                                     nil))))))))

(defun compute-back-propagated-constraints (ind new-constraints)
  (loop for constraint in new-constraints
        when (older-individual-p (constraint-ind constraint) ind)
        collect constraint))

(defun compute-new-labels-violated-blocks (retry-labels-2 labels #+:debug new-labels-2)
  (let (#+:debug (broken-blocks nil))
    (loop for label in retry-labels-2
          for blocked-constraints = (label-info-blocked-constraints label)
          when blocked-constraints
          append blocked-constraints into result
          and collect
          (cons label
                (let ((new-label (copy-label-info label)))
                  #+:debug (push new-label broken-blocks)
                  (setf (label-info-blocked-constraints new-label) nil)
                  (when (label-info-blocked-labels new-label)
                    (loop for blocked-label in (label-info-blocked-labels new-label) do
                          (setf (label-info-blocked-p blocked-label) nil))
                    (setf (label-info-blocked-labels new-label) nil))
                  new-label))
          into new-labels
          finally
          #+:debug
          (when broken-blocks
            (race-trace ("~&Sig-unblocking blocked labels ~S, labels=~S~%"
                         broken-blocks new-labels-2)))
          (return (values (replace-modified-labels new-labels labels)
                          result)))))

(defun added-derived-constraints-satisfiable (ind
                                              new-constraints
                                              new-relation-constraints
                                              state
                                              unused)
  (declare (ignore unused))
  (let ((relation-constraints (state-relation-constraints state))
        (expanded-constraints (state-expanded-constraints state))
        (remaining-exists-constraints (state-unexpanded-exists-constraints state))
        (attribute-constraints (state-attribute-constraints state))
        (concrete-domain-state (state-concrete-domain-state state))
        (labels (state-labels state))
        (indirectly-blocked-individuals (state-indirectly-blocked-individuals state)))
    (race-trace ("~&Testing satisfiability of derived constraints ~S, ~S, state=~S~%"
                 new-constraints new-relation-constraints state))
    (let* ((possible-backpropagation (and *inverse-roles* (not (old-individual-p ind))))
           (back-propagated-constraints
            (when possible-backpropagation
              (compute-back-propagated-constraints ind new-constraints)))
           (back-propagation-list
            (when back-propagated-constraints
              (get-backpropagation-list back-propagated-constraints labels))))
      (multiple-value-bind (new-labels-1 retry-labels-1)
                           (if back-propagation-list
                             (copy-labels-list back-propagation-list labels)
                             labels)
        (when-debug back-propagation-list
          (race-trace ("~&Sig-backpropagating ~S due to ~S, old labels=~S, new labels=~S ~
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
          ;  (find-newly-blocked-labels new-labels-2))
          (let* ((blocked-individuals (when blocked-labels
                                        (mapcar #'label-info-ind blocked-labels)))
                 (new-indirectly-blocked-individuals-1
                  (if blocked-individuals
                    (union (get-obsolete-successors-of-inds blocked-individuals
                                                            relation-constraints
                                                            (state-relation-store state))
                           (union blocked-individuals indirectly-blocked-individuals))
                    indirectly-blocked-individuals)))
            (multiple-value-bind
              (new-labels-3 violated-blocks)
              (if retry-labels-2
                (compute-new-labels-violated-blocks retry-labels-2 labels #+:debug new-labels-2)
                new-labels-2)
              (when-debug new-indirectly-blocked-individuals-1
                (race-trace ("~&Indirectly blocked inds=~S, blocked labels=~S~%"
                             new-indirectly-blocked-individuals-1 blocked-labels))
                (race-trace ("Constraints (~S ~S ~S ~S ~S ~S ~S)~%"
                             remaining-exists-constraints
                             expanded-constraints
                             relation-constraints
                             attribute-constraints
                             concrete-domain-state
                             new-labels-3
                             new-indirectly-blocked-individuals-1)))
              (multiple-value-bind (obsolete-individuals
                                    remaining-relation-constraints
                                    new-store)
                                   (if violated-blocks
                                     (get-obsolete-individuals violated-blocks
                                                               relation-constraints
                                                               state)
                                     (values nil relation-constraints (state-relation-store state)))
                ;(when blocked-labels (break "~S" new-constraints))
                (if obsolete-individuals
                  (let* ((violated-exists
                          (when violated-blocks
                            (remove-obsolete-constraints obsolete-individuals violated-blocks)))
                         (copied-violated-exists
                          (when violated-exists
                            (copy-violated-exists-constraints violated-exists)))
                         (new-constraints-2
                          (remove-obsolete-constraints obsolete-individuals new-constraints))
                         (all-new-constraints (append copied-violated-exists new-constraints-2))
                         (new-labels-4 (remove-obsolete-label-infos obsolete-individuals
                                                                    new-indirectly-blocked-individuals-1
                                                                    new-labels-3))
                         (unexpanded-exists-constraints-store
                          (state-unexpanded-disjunctive-constraints-store state))
                         (unexpanded-disjunctive-constraints-store
                          (state-unexpanded-disjunctive-constraints-store state))
                         (unexpanded-disjunctive-constraints
                          (state-unexpanded-disjunctive-constraints state))
                         (expanded-store (state-expanded-store state)))
                    (multiple-value-bind (new-expanded-constraints-1
                                          new-expanded-store-1
                                          new-expanded-store-index-1)
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
                        (when-debug copied-violated-exists
                          (race-trace ("~&Retrying exists constraints ~S (~S)~%"
                                       copied-violated-exists remaining-relation-constraints)))
                        (multiple-value-bind
                          (new-remaining-exists-constraints
                           new-unexpanded-exists-constraints-store)
                          (if (and obsolete-individuals 
                                   *use-unexpanded-exists-constraints-store*
                                   unexpanded-exists-constraints-store
                                   (not (constraint-store-unused-p unexpanded-exists-constraints-store)))
                            (remove-individuals-from-constraint-store obsolete-individuals
                                                                      remaining-exists-constraints
                                                                      unexpanded-exists-constraints-store
                                                                      (state-copy-unexpanded-exists-constraints-store-p
                                                                       state)
                                                                      state
                                                                      #'reset-exists-copy)
                            (values (remove-obsolete-constraints obsolete-individuals
                                                                 remaining-exists-constraints)
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
                            ;(break)
                            (race-trace ("New constraints (~S ~S ~S ~S ~S ~S ~S)~%"
                                         new-remaining-exists-constraints
                                         new-expanded-constraints-2
                                         remaining-relation-constraints
                                         attribute-constraints
                                         concrete-domain-state
                                         new-labels-4
                                         new-indirectly-blocked-individuals-1)
                                        ("~&Removed obsolete individuals ~S~%" obsolete-individuals))
                            (let* ((new-indirectly-blocked-individuals-2
                                    (set-difference new-indirectly-blocked-individuals-1
                                                    obsolete-individuals))
                                   (new-labels-5 (remove-obsolete-label-infos
                                                  obsolete-individuals
                                                  new-indirectly-blocked-individuals-2
                                                  new-labels-4))
                                   (copied-state
                                    (if (eq (first (last (state-remaining-at-most-bounds state)))
                                            (state-current-at-most-bound state))
                                      (copy-to-basic-kernel-state state)
                                      state))
                                   (expand-domain-range
                                    (when new-relation-constraints
                                      t))
                                   (new-state
                                    (changed-kernel-state copied-state
                                                          :unexpanded-disjunctive-constraints
                                                          new-unexpanded-disjunctive-constraints
                                                          :unexpanded-exists-constraints
                                                          new-remaining-exists-constraints
                                                          :expanded-constraints
                                                          new-expanded-constraints-2
                                                          :relation-constraints
                                                          remaining-relation-constraints
                                                          :labels new-labels-5
                                                          :indirectly-blocked-individuals
                                                          new-indirectly-blocked-individuals-2
                                                          :expanded-store new-expanded-store-2
                                                          :expanded-store-index new-expanded-store-index-2
                                                          :relation-store new-store
                                                          :unexpanded-exists-constraints-store
                                                          new-unexpanded-exists-constraints-store
                                                          :unexpanded-disjunctive-constraints-store
                                                          new-unexpanded-disjunctive-constraints-store)))
                              (added-constraints-satisfiable all-new-constraints
                                                             new-relation-constraints
                                                             new-state
                                                             nil ; do not expand role-domain
                                                             expand-domain-range)))))))
                  (let* ((copied-violated-blocks
                          (when violated-blocks
                            (copy-violated-exists-constraints violated-blocks)))
                         (new-constraints-2
                          (remove-obsolete-constraints obsolete-individuals new-constraints))
                         (all-new-constraints (append copied-violated-blocks new-constraints-2))
                         (new-labels-4 (remove-obsolete-label-infos nil ;no obsolete-individuals
                                                                    new-indirectly-blocked-individuals-1
                                                                    new-labels-3))
                         (expanded-store (state-expanded-store state)))
                    (multiple-value-bind
                      (new-expanded-constraints new-expanded-store new-expanded-store-index)
                      (remove-constraints-from-constraint-store violated-blocks
                                                                expanded-constraints
                                                                expanded-store
                                                                (state-copy-expanded-store-p state)
                                                                state
                                                                #'reset-expanded-copy
                                                                (state-expanded-store-index state))
                      (let* ((copied-state (if (eq (first (last (state-remaining-at-most-bounds state)))
                                                   (state-current-at-most-bound state))
                                             (copy-to-basic-kernel-state state)
                                             state))
                             (expand-domain-range
                              (when new-relation-constraints
                                t))
                             (new-state
                              (changed-kernel-state copied-state
                                                    :unexpanded-exists-constraints
                                                    remaining-exists-constraints
                                                    :expanded-constraints new-expanded-constraints
                                                    :relation-constraints relation-constraints
                                                    :attribute-constraints attribute-constraints
                                                    :concrete-domain-state concrete-domain-state
                                                    :labels new-labels-4
                                                    :indirectly-blocked-individuals
                                                    new-indirectly-blocked-individuals-1
                                                    :expanded-store new-expanded-store
                                                    :expanded-store-index new-expanded-store-index
                                                    :relation-store new-store)))
                        (added-constraints-satisfiable all-new-constraints
                                                       new-relation-constraints
                                                       new-state
                                                       nil ; do not expand role-domain
                                                       expand-domain-range)))))))))))))

(defun possibly-negated-qualification-contained-p (signature qualification)
  (let ((concepts (signature-concepts signature)))
    (or (member qualification concepts)
        (member (concept-negated-concept qualification) concepts))))

(defun split-signature (at-most-constraint
                        qualification
                        signature
                        signatures
                        &optional (full-split nil))
  ;;; returns 2 values: new-signatures, added-signatures
  #+:debug (assert (not (possibly-negated-qualification-contained-p signature qualification)))
  (let* ((new-signature-qualification-list
          (adjoin qualification (signature-concepts signature)))
         (old-split (when (null (signature-successor-ind-set signature))
                      (find-signature (signature-successor-ind-set signature)
                                      (signature-role signature)
                                      new-signature-qualification-list
                                      signatures)))
         (increment (if full-split
                        (signature-cardinality signature)
                      1)))
    (if old-split
        (let ((modified-split (copy-signature old-split))
              (modified-signature (when (> (signature-cardinality signature) increment)
                                    (copy-signature signature))))
          (incf (signature-cardinality modified-split) increment)
          (if modified-signature
              (progn
                (decf (signature-cardinality modified-signature) increment)
                (when at-most-constraint
                  (pushnew at-most-constraint (signature-dependencies modified-signature)))
                (values (list* modified-split modified-signature
                               (delete signature (remove old-split signatures)))
                        (list modified-split modified-signature)))
            (values (cons modified-split (delete signature (remove old-split signatures)))
                    (list modified-split))))
      (let ((new-split (copy-signature signature))
            (modified-signature (when (> (signature-cardinality signature) increment)
                                  (copy-signature signature))))
        (setf (signature-cardinality new-split) increment)
        (setf (signature-concepts new-split) new-signature-qualification-list)
        (when at-most-constraint
          (push at-most-constraint (signature-dependencies new-split))
          (let* ((concept-dependencies (signature-concept-dependencies new-split))
                 (old (when concept-dependencies
                        (assoc qualification concept-dependencies :test 'equal))))
            (if old
                (push at-most-constraint (cdr old))
              (setf (signature-concept-dependencies new-split)
                    (acons qualification (signature-dependencies new-split) concept-dependencies)))))
        (if modified-signature
            (progn
              (when at-most-constraint
                (pushnew at-most-constraint (signature-dependencies modified-signature)))
              (decf (signature-cardinality modified-signature) increment)
              (values (list* modified-signature new-split (remove signature signatures))
                      (list modified-signature new-split)))
          (values (cons new-split (remove signature signatures)) (list new-split)))))))

(defun qualification-clash-p (selected-signature negated-at-most-qualification state)
  (or (let ((concepts (signature-concepts selected-signature)))
        (when concepts
          (or (if (negated-concept-p negated-at-most-qualification)
                (when (member (concept-term negated-at-most-qualification) concepts
                              :key #'concept-told-subsumers :test #'member)
                  ;(format t "~&Subsumers: wrong selection ~S for ~S~%" negated-at-most-qualification selected-signature)
                  t)
                (if (atomic-concept-p negated-at-most-qualification)
                  (when (member negated-at-most-qualification concepts
                                :key #'concept-told-disjoints :test #'member)
                    ;(format t "~&Disjoints: wrong selection ~S for ~S~%" negated-at-most-qualification selected-signature)
                    t)
                  (when (and (and-concept-p negated-at-most-qualification)
                             (loop with term = (concept-term negated-at-most-qualification)
                                   for concept in concepts
                                   thereis
                                   (concept-set-simple-clash-p (concept-told-subsumers concept) term)))
                    ;(break)
                    t)))
              (and (qualification-contained-p (concept-negated-concept negated-at-most-qualification)
                                              concepts)
                   t))))
      (let ((succ-ind-set (signature-successor-ind-set selected-signature)))
        (when succ-ind-set
          (or (indset-selected-constraints-p
               succ-ind-set
               (lambda (constraint)
                 (let ((concept
                        (if (constraint-negated-p constraint)
                          (concept-negated-concept (constraint-term constraint))
                          (constraint-term constraint))))
                   (when (concept-clash-p concept negated-at-most-qualification)
                     ;(format t "~&Old inds: wrong selection ~S for ~S~%" negated-at-most-qualification constraint)
                     t)))
               (state-expanded-constraints state)
               (state-expanded-store state)
               (state-expanded-store-index state))
              (indset-selected-constraints-p
               succ-ind-set
               (lambda (constraint)
                 (let ((concept
                        (if (constraint-negated-p constraint)
                          (concept-negated-concept (constraint-term constraint))
                          (constraint-term constraint))))
                   (when (concept-clash-p concept negated-at-most-qualification)
                     ;(format t "~&Old inds: wrong selection ~S for ~S~%" negated-at-most-qualification constraint)
                     t)))
               (state-unexpanded-exists-constraints state)
               (state-unexpanded-exists-constraints-store state)))))))

(defun split-signatures (selected-signature
                         signatures
                         state
                         unused-1
                         unused-2)
  (declare (ignore unused-1 unused-2))
  (let* ((at-least-bounds (state-at-least-bounds state))
         (all-at-most-bounds (state-at-most-bounds state))
         (current-at-most-bound (state-current-at-most-bound state))
         (at-most-qualification (bound-qualification current-at-most-bound))
         (negated-at-most-qualification (concept-negated-concept at-most-qualification))
         (first-clash-dependencies nil))
    (incf-statistics *signature-splits*)
    (flet
      ((split-signatures-continuation-1 (sat-p xstate unused-1 unused-2 unused-3)
         (declare (ignore unused-1 unused-2 unused-3))
         (if sat-p
           (progn
             (incf-statistics *signature-sat-first-split*)
             (decf *or-level*)
             (added-constraints-satisfiable nil nil xstate t nil))
           (if (qualification-clash-p selected-signature
                                      at-most-qualification
                                      state)
             (progn
               (incf-statistics *signature-unsat-splits*)
               (set-clash-dependencies (list* selected-signature
                                              (when (bound-constraint current-at-most-bound)
                                                (constraint-or-dependencies
                                                 (bound-constraint current-at-most-bound)))))
               (race-trace ("~&Alternative split clashed due to at-most qualification ~S for ~S in ~S, clash culprits: ~S~%"
                            at-most-qualification
                            selected-signature
                            signatures
                            *catching-clash-dependencies*))
               ;(break)
               (decf *or-level*)
               (handle-clash-with-backtrack-state state nil nil nil nil))
             (multiple-value-bind (new-signatures added-signatures)
                                  (split-signature (bound-constraint current-at-most-bound)
                                                   at-most-qualification
                                                   selected-signature
                                                   signatures
                                                   t)
               (race-trace ("~&Fully splitting ~S for ~S by ~S resulting in ~S~%"
                            selected-signature at-most-qualification
                            added-signatures new-signatures))
               (multiple-value-bind (satisfied-p
                                     at-least-violated-p
                                     at-most-violated-p
                                     violated-signatures)
                                    (signatures-satisfied-p new-signatures
                                                            at-least-bounds
                                                            current-at-most-bound
                                                            state)
                 #-:debug (declare (ignore at-least-violated-p))
                 #+:debug (assert (not at-least-violated-p))
                 (flet
                   ((split-signatures-continuation-2 (sat-p xstate unused-1 unused-2 unused-3)
                      (declare (ignore unused-1 unused-2 unused-3))
                      (if sat-p
                        (progn
                          (incf-statistics *signature-sat-second-split*)
                          (decf *or-level*)
                          (added-constraints-satisfiable nil nil xstate t nil))
                        (progn
                          (race-trace ("~&Alternative split ~S failed for ~S in ~S due to clash culprits ~S~%"
                                       at-most-qualification
                                       selected-signature
                                       new-signatures
                                       *catching-clash-dependencies*))
                          (if (if *use-refined-signature-clash-dependencies*
                                  (and (signature-backjumping-stopped-here (list* selected-signature added-signatures))
                                       (or (null (bound-constraint current-at-most-bound))
                                           (backjumping-stopped-here (bound-constraint current-at-most-bound))
                                           (member (bound-constraint current-at-most-bound)
                                                   added-signatures
                                                   :key #'signature-dependencies
                                                   :test #'member)))
                                (or (signature-backjumping-stopped-here added-signatures)
                                    (when (bound-constraint current-at-most-bound)
                                      (backjumping-stopped-here (bound-constraint current-at-most-bound)))))
                            (progn
                              (incf-statistics *signature-unsat-splits*)
                              (add-clash-dependencies first-clash-dependencies)
                              (update-catching-clash-dependencies added-signatures)
                              (add-clash-dependencies (list selected-signature))
                              (race-trace ("~&Stopped signature backjumping for ~S, dep= ~S~%"
                                           selected-signature *catching-clash-dependencies*)))
                            (progn
                              (incf-statistics *signature-skipped-unsat-splits*)
                              (clean-catching-clash-dependencies added-signatures)
                              (race-trace ("~&Skipping alternative split of ~S for ~S: ~
                                            continuing signature backjumping with dep= ~S~%"
                                           selected-signature
                                           at-most-qualification
                                           *catching-clash-dependencies*))))
                          (handle-clash-with-backtrack-state state nil nil nil nil)))))
                   (if satisfied-p
                     (progn
                       (race-trace ("~&Signatures partially satisfied for ~S: ~S~%"
                                    current-at-most-bound new-signatures))
                       (let* ((new-state-1 (copy-signature-kernel-state state t))
                              (new-partially-expanded-or-stack
                               (push-backtrack-stack #'split-signatures-continuation-2
                                                     (state-partially-expanded-or-stack new-state-1)))
                              (new-state-2
                               (changed-signature-state new-state-1
                                                        :partially-expanded-or-stack
                                                        new-partially-expanded-or-stack)))
                         (remaining-at-most-bounds-satisfiable new-signatures
                                                               new-state-2
                                                               nil nil nil)))
                     (if (subsetp violated-signatures added-signatures)
                       (progn
                         #+:debug (assert at-most-violated-p)
                         (race-trace ("~&Full split of ~S failed. ~%Signatures ~S NOT satisfied for ~S: ~S~%"
                                      selected-signature
                                      violated-signatures
                                      current-at-most-bound
                                      new-signatures))
                         (set-clash-dependencies
                          (union-dependencies 
                           (union-dependency-list
                            (mapcar #'signature-dependencies violated-signatures))
                           (when (bound-constraint current-at-most-bound)
                             (constraint-or-dependencies
                              (bound-constraint current-at-most-bound)))))
                         (add-to-collected-dependencies *catching-clash-dependencies*)
                         (race-trace ("~&Signatures ~S for ~S unsatisfiable, dep= ~S, coll-dep=~S~%"
                                      violated-signatures
                                      (bound-constraint current-at-most-bound)
                                      *catching-clash-dependencies*
                                      (top-collected-dependencies)))
                         (split-signatures-continuation-2 nil state nil nil nil))
                       (progn
                         (race-trace ("~&Signatures ~S NOT satisfied for ~S: ~S ~
                                       Trying to split other signatures~%"
                                      violated-signatures
                                      current-at-most-bound
                                      new-signatures))
                         (let* ((new-state-1 (copy-signature-kernel-state state t))
                                (new-partially-expanded-or-stack
                                 (push-backtrack-stack #'split-signatures-continuation-2
                                                       (state-partially-expanded-or-stack new-state-1)))
                                (new-state-2
                                 (changed-signature-state new-state-1
                                                          :partially-expanded-or-stack
                                                          new-partially-expanded-or-stack)))
                           (merge-split-signatures violated-signatures
                                                   new-signatures
                                                   new-state-2
                                                   at-most-violated-p
                                                   nil))))))))))))
      (if (qualification-clash-p selected-signature
                                 negated-at-most-qualification
                                 state)
        (split-signatures-continuation-1 nil state nil nil nil)
        (progn
          (incf *or-level*)
          (setf-statistics *max-or-level* (max (get-local-statistics-integer-value *max-or-level*)
                                               *or-level*))
          (multiple-value-bind (new-signatures added-signatures)
                               (split-signature (bound-constraint current-at-most-bound)
                                                negated-at-most-qualification
                                                selected-signature
                                                signatures)
            (race-trace ("~&Splitting ~S for ~S by ~S resulting in ~S~%"
                         selected-signature negated-at-most-qualification
                         added-signatures new-signatures))
            (multiple-value-bind (satisfied-p
                                  at-least-violated-p
                                  at-most-violated-p
                                  violated-signatures)
                                 (signatures-satisfied-p new-signatures
                                                         at-least-bounds
                                                         current-at-most-bound
                                                         state)
              #-:debug (declare (ignore at-least-violated-p))
              #+:debug (assert (not at-least-violated-p))
              (flet
                ((split-signatures-continuation-3 (sat-p xstate unused-1 unused-2 unused-3)
                   (declare (ignore unused-1 unused-2 unused-3))
                   (if sat-p
                     (progn
                       (incf-statistics *signature-sat-second-split*)
                       (decf *or-level*)
                       (added-constraints-satisfiable nil nil xstate t nil))
                     (progn
                       (race-trace ("~&First split ~S failed for ~S in ~S due to clash culprits ~S~%"
                                    negated-at-most-qualification
                                    selected-signature
                                    new-signatures
                                    *catching-clash-dependencies*))
                       (if (if *use-refined-signature-clash-dependencies*
                               (and (signature-backjumping-stopped-here (list* selected-signature added-signatures))
                                    (or (null (bound-constraint current-at-most-bound))
                                        (backjumping-stopped-here (bound-constraint current-at-most-bound))
                                        ;(break "2")
                                        (member (bound-constraint current-at-most-bound)
                                                added-signatures
                                                :key #'signature-dependencies
                                                :test #'member)))
                             (or (signature-backjumping-stopped-here added-signatures)
                                 (when (bound-constraint current-at-most-bound)
                                   (backjumping-stopped-here (bound-constraint current-at-most-bound)))))
                         (progn
                           (race-trace ("~&Stopped signature backjumping for ~S, dep= ~S~%"
                                        selected-signature *catching-clash-dependencies*))
                           (setf first-clash-dependencies *catching-clash-dependencies*)
                           (set-clash-dependencies nil)
                           (split-signatures-continuation-1 nil state nil nil nil))
                         (progn
                           (incf-statistics *signature-skipped-unsat-splits*)
                           (clean-catching-clash-dependencies added-signatures)
                           (race-trace ("~&Skipping alternative split of ~S for ~S: ~
                                         continuing signature backjumping with dep= ~S~%"
                                        selected-signature
                                        at-most-qualification
                                        *catching-clash-dependencies*))
                           (decf *or-level*)
                           (handle-clash-with-backtrack-state state nil nil nil nil)))
                       ))))
                (if satisfied-p
                  (if (signature-violated-p new-signatures all-at-most-bounds)
                    (split-signatures-continuation-3 nil state nil nil nil)
                    (progn
                      (race-trace ("~&Signatures partially satisfied for ~S: ~S~%"
                                   current-at-most-bound new-signatures))
                      (let* ((new-state-1 (copy-signature-kernel-state state t))
                             (new-partially-expanded-or-stack
                              (push-backtrack-stack #'split-signatures-continuation-3
                                                    (state-partially-expanded-or-stack new-state-1)))
                             (new-state-2
                              (changed-signature-state new-state-1
                                                       :partially-expanded-or-stack
                                                       new-partially-expanded-or-stack)))
                        (remaining-at-most-bounds-satisfiable new-signatures
                                                              new-state-2
                                                              nil nil nil))))
                  (progn
                    (race-trace ("~&Signatures NOT satisfied for ~S: ~S ~S ~
                                  Trying to merge with other signatures~%"
                                 current-at-most-bound
                                 violated-signatures
                                 new-signatures))
                    #+:debug (assert (or violated-signatures at-most-violated-p))
                    (let* ((new-state-1 (copy-signature-kernel-state state t))
                           (new-partially-expanded-or-stack
                            (push-backtrack-stack #'split-signatures-continuation-3
                                                  (state-partially-expanded-or-stack new-state-1)))
                           (new-state-2
                            (changed-signature-state new-state-1
                                                     :partially-expanded-or-stack
                                                     new-partially-expanded-or-stack)))
                        (merge-split-signatures violated-signatures
                                                new-signatures
                                                new-state-2
                                                at-most-violated-p
                                                nil))))))))))))

(defun get-role-related-exists (ind
                                    role
                                    at-most-bound
                                    unexpanded-exists-constraints
                                    state)
  (let ((at-most-bound-concept (when (bound-constraint at-most-bound)
                                 (constraint-term (bound-constraint at-most-bound)))))
    (flet ((matching-exists-constraint-p-1 (constraint)
	     (let ((trigger (constraint-merging-trigger-p constraint)))
	       (or (null trigger)
		   (and at-most-bound-concept
			(eq (constraint-term trigger) at-most-bound-concept)))))
	   (matching-exists-constraint-p-2 (constraint)
             (and (exists-concept-p (constraint-term constraint))
                  (not (constraint-negated-p constraint))
                  (let ((trigger (constraint-merging-trigger-p constraint)))
                    (or (null trigger)
                        (and at-most-bound-concept
                             (eq (constraint-term trigger) at-most-bound-concept)))))))
      (let* ((related-exists
              (get-role-related-exists-1
               role
               (collect-selected-constraints ind
                                             #'matching-exists-constraint-p-1
                                             unexpanded-exists-constraints
                                             nil)))
             (violated-exists
              (when related-exists
                (get-role-related-exists-1
                 role
                 (collect-selected-constraints ind
                                               #'matching-exists-constraint-p-2
                                               (state-expanded-constraints state)
                                               (state-expanded-store state)
                                               (state-expanded-store-index state))))))
        ;(break "~S ~S" violated-exists related-exists)
        (values (if related-exists
                  (constraint-set-union violated-exists related-exists)
                  violated-exists)
                violated-exists)))))

(defun get-role-related-exists-1 (role constraints)
  (loop with related-exists = nil
        for constraint in constraints
        for concept = (constraint-term constraint)
        do
        (let* ((concept-role (concept-role concept))
               (role-ancestors (role-ancestors-internal concept-role)))
          ;(break "~S: ~S ~S" constraint ind role)
          (when (and (or (not *use-unique-name-assumption*)
                         (not (role-feature-p role)))
                     (member role role-ancestors))
            (push constraint related-exists)
            ;(break "~S: ~S" constraint related-exists)
            ))
        finally (return related-exists)))

(defun at-most-constraint-violated-p (selected-some-constraint state)
  (let* ((ind (constraint-ind selected-some-constraint))
         (true-some-constraints
          (collect-ind-selected-constraints ind
                                            (state-unexpanded-exists-constraints state)
                                            (state-unexpanded-exists-constraints-store
                                             state))))
    #+:debug
      (assert (eql (length (remove-if #'constraint-merging-trigger-p true-some-constraints))
                   (length (racer-remove-concept-constraint-duplicates
                            (remove-if #'constraint-merging-trigger-p true-some-constraints)))))
    (let* ((ind-relation-constraints
            (unless *use-relation-store*
              (loop for constraint in (state-relation-constraints state)
                    when (eql ind (constraint-ind-1 constraint))
                    collect constraint)))
           (role (concept-role (constraint-term selected-some-constraint)))
           (ancestors (role-ancestors-internal role))
           (at-most-bounds (create-at-most-bounds ind
                                                  state
                                                  true-some-constraints
                                                  ind-relation-constraints))
           (partitions-table (clrhash *partitions-table*))
           (initial-exists-partitions
            (loop for ancestor-role in (reverse (role-ancestors-internal role))
                  for partitions =
                  (loop for bound in at-most-bounds
                        for partition = (when (subrole-p ancestor-role (bound-role bound))
                                          (get-role-partition ind
                                                              ancestor-role
                                                              bound
                                                              true-some-constraints
                                                              ind-relation-constraints
                                                              state))
                        when partition
                        collect partition)
                  when partitions
                  nconc partitions
                  and do (setf (gethash ancestor-role partitions-table) t))))
      ;(break "~S" selected-some-constraint)
      (if (null initial-exists-partitions)
        nil
        (let* ((exists-partitions
                (nconc initial-exists-partitions
                       (loop for some-constraint in (remove selected-some-constraint 
                                                            true-some-constraints)
                             for some-role = (concept-role (constraint-term some-constraint))
                             unless (lists-disjoint-p (role-ancestors-internal some-role)
                                                      ancestors)
                             nconc
                             (loop for ancestor-role in (reverse
                                                         (role-ancestors-internal some-role))
                                   for partitions =
                                   (unless (gethash ancestor-role partitions-table)
                                     (loop for bound in at-most-bounds
                                           for partition =
                                           (when (subrole-p ancestor-role (bound-role bound))
                                             (get-role-partition ind
                                                                 ancestor-role
                                                                 bound
                                                                 true-some-constraints
                                                                 ind-relation-constraints
                                                                 state))
                                           when partition
                                           collect partition))
                                   when partitions
                                   nconc partitions
                                   and do (setf (gethash ancestor-role partitions-table) t)))))
               (new-exists-partitions
                (loop for partitions on exists-partitions
                      for partition = (first partitions)
                      unless (or (and (rest partitions)
                                      (subsuming-partition-found partition (rest partitions)))
                                 (and new-partitions
                                      (subsuming-partition-found partition new-partitions)))
                      collect partition into new-partitions
                      finally
                      (if (rest new-partitions)
                        (return (merge-intersecting-partitions new-partitions
                                                               (use-simplex-p state
                                                                              at-most-bounds)))
                        (return new-partitions)))))
          ;(break "~S" new-exists-partitions)
          #+:debug (assert (or (find selected-some-constraint new-exists-partitions
                                     :test #'member :key #'fourth)
                               (find selected-some-constraint new-exists-partitions
                                     :key #'fourth
                                     :test #'(lambda (elem list)
                                               (member (concept-negated-concept
                                                        (constraint-term elem))
                                                       list
                                                       :test #'concept-clash-p
                                                       :key #'constraint-term)))
                               (let ((trigger
                                      (constraint-merging-trigger-p selected-some-constraint)))
                                 (when trigger
                                   (loop for partition in new-exists-partitions
                                         for at-most-bound-concept = (constraint-term
                                                                      (third partition))
                                         always (not (eq (constraint-term trigger)
                                                         at-most-bound-concept)))))))
          new-exists-partitions)))))

(defun merge-intersecting-partitions (exists-partitions use-simplex)
  (loop with new-partitions = exists-partitions
        for modified = nil do
        (loop with signature-partitioning = *signature-partitioning*
              for partitions on new-partitions
              for selected-partition = (first partitions)
              for match = (if signature-partitioning
                            (find (fourth selected-partition) (rest partitions)
                                  :key #'fourth :test-not #'lists-disjoint-p)
                            (second partitions))
              do
              (when (and match
                         (or signature-partitioning (third match))
                         (or use-simplex
                             (eq (constraint-term (third selected-partition))
                                 (constraint-term (third match)))))
                (setf new-partitions (remove selected-partition (remove match new-partitions)))
                (setf (fourth selected-partition)
                      (constraint-set-union (fourth selected-partition) (fourth match)))
                (setf (fifth selected-partition)
                      (constraint-set-union (fifth selected-partition) (fifth match)))
                (pushnew selected-partition new-partitions)
                (setf modified t)
                (return)))
        until (not modified)
        finally (return new-partitions)))
        
(defun subsuming-partition-found (partition partitions)
  (destructuring-bind (violated-role max-inds at-most-constraint related-r-exists-constraints
                                     ignore1 ignore2)
      partition
    (declare (ignore ignore1 ignore2))
    (loop for current-partition in partitions
          thereis (and (subrole-p violated-role (first current-partition))
                       (eql (second current-partition) max-inds)
                       (or (null at-most-constraint)
                           (subrole-p (concept-role (constraint-term (third current-partition)))
                                      (concept-role (constraint-term at-most-constraint))))
                       (or (equal related-r-exists-constraints
                                  (fourth current-partition))
                           (subsetp related-r-exists-constraints
                                    (fourth current-partition)))))))

(defun get-role-partition (ind
                           some-role
                           at-most-bound
                           true-some-constraints
                           ind-relation-constraints
                           state)
  (multiple-value-bind (related-exists violated-exists)
                       (get-role-related-exists ind
                                                some-role
                                                at-most-bound
                                                true-some-constraints
                                                state)
    (multiple-value-bind (counter unused old-inds satisfied-exists-constraints)
                         (count-role-successors ind
                                                at-most-bound
                                                (racer-remove-concept-constraint-duplicates
                                                 related-exists)
                                                ind-relation-constraints
                                                (state-expanded-constraints state)
                                                state)
      (declare (ignore unused))
      (let ((bound (bound-number at-most-bound)))
        #+:debug (assert bound)
        ;(break "~S/~D<=~D" some-role counter bound)
        (when (and related-exists
                   (> counter bound)
                   (bound-constraint at-most-bound)
                   (or (not *use-unique-name-assumption*)
                       (not (role-feature-p some-role))))
          (let* ((at-most-concept (constraint-term (bound-constraint at-most-bound)))
                 (at-most-constraint
                  (find-if-selected-constraints
                   ind
                   (lambda (constraint)
                     (eq (constraint-term constraint) at-most-concept))
                   (state-expanded-constraints state)
                   (state-expanded-store state)
		   (state-expanded-store-index state)))
                 (new-violated-exists
                  (constraint-set-difference violated-exists satisfied-exists-constraints)))
            (race-trace ("~&Too many successors ~S ~S (~D, max=~D) for role ~S and ~S~%"
                         related-exists new-violated-exists
                         counter bound some-role at-most-constraint))
            #+:debug (assert (or at-most-constraint (role-feature-p some-role)))
            (list some-role
                  bound
                  at-most-constraint
                  related-exists
                  new-violated-exists
                  (unless *use-unique-name-assumption*
                    old-inds))))))))

(defun qualification-contained-p (qualification concepts)
  (or (qualification-contained-p-correct qualification concepts)
      #|(let ((result (qualification-contained-p-incorrect qualification concepts)))
        (when result
          ;(print (list qualification concepts result))
          (princ "+")
          )
        result)|#))

(defun qualification-contained-p-correct (qualification concepts)
  (or (and (concept-p-internal concepts) (subsumes-concept-p qualification concepts))
      (when (consp concepts)
        (or (member qualification concepts :test 'subsumes-concept-p)
            (if (negated-concept-p qualification)
              (member (concept-term qualification) concepts
                      :key #'concept-told-disjoints :test #'member)
              (when (atomic-concept-p qualification)
                (member qualification concepts :key #'concept-told-subsumers :test #'member)))))
      (cond ((and-concept-p qualification)
             (loop for elem-1 in (concept-term qualification)
                   always (if (listp concepts)
                              (loop for elem-2 in concepts
                                    thereis (qualification-contained-p elem-1 elem-2))
                            (qualification-contained-p elem-1 concepts))))
            ((or-concept-p qualification)
             (loop for elem-1 in (concept-term qualification)
                   thereis (if (listp concepts)
                             (loop for elem-2 in concepts
                                   thereis (qualification-contained-p elem-1 elem-2))
                             (qualification-contained-p elem-1 concepts))))
            (t nil))))

(defun qualification-contained-p-incorrect (qualification concepts)
  (or (and (concept-p-internal concepts) (subsumes-concept-p qualification concepts))
      (when (consp concepts)
        (or (member qualification concepts :test 'subsumes-concept-p)
            (if (negated-concept-p qualification)
              (member (concept-term qualification) concepts
                      :key #'concept-told-disjoints :test #'member)
              (when (atomic-concept-p qualification)
                (member qualification concepts :key #'concept-told-subsumers :test #'member)))))
      (cond ((and-concept-p qualification)
             (loop for elem-1 in (concept-term qualification)
                   always (if (listp concepts)
                            (loop for elem-2 in concepts
                                  thereis (if (or-concept-p elem-2)
                                            (qualification-contained-p elem-1 (concept-term elem-2))
                                            (qualification-contained-p elem-1 elem-2)))
                            (if (or-concept-p concepts)
                              (qualification-contained-p elem-1 (concept-term concepts))
                              (qualification-contained-p elem-1 concepts)))))
            ((or-concept-p qualification)
             (loop for elem-1 in (concept-term qualification)
                   thereis (if (listp concepts)
                             (loop for elem-2 in concepts
                                   thereis (if (or-concept-p elem-2)
                                             (qualification-contained-p elem-1 (concept-term elem-2))
                                             (qualification-contained-p elem-1 elem-2)))
                             (if (or-concept-p concepts)
                               (qualification-contained-p elem-1 (concept-term concepts))
                               (qualification-contained-p elem-1 concepts)))))
            (t nil))))

(defun count-role-successors (ind
                              at-most-bound
                              related-exists
                              ind-relation-constraints
                              expanded-constraints
                              state)
  (let* ((bound-role (bound-role at-most-bound))
         (bound-qualification (bound-qualification at-most-bound))
         (qualification-top-concept-p (is-any-top-concept-p bound-qualification))
         (negated-qualification (unless qualification-top-concept-p
                                  (concept-negated-concept bound-qualification))))
    (multiple-value-bind (unused first-rel-constraint successor-ind-list)
                         (if *use-relation-store*
                           (relation-filler-individuals ind 
                                                        bound-role
                                                        (state-relation-store state))
                           (number-of-role-successors ind
                                                      bound-role
                                                      ind-relation-constraints
                                                      state))
      (declare (ignore unused))
      (multiple-value-bind
        (count satisfied-exists-constraints)
        (loop with max-top-count = 0
              with successor-ind-list-length = (length successor-ind-list)
              for constraint in related-exists
              for concept = (constraint-term constraint)
              for qualification = (concept-term concept)
              for concepts = (make-concept-list (concept-term concept))
              for number-restriction = (concept-number-restriction concept)
              if (eq (concept-role concept) bound-role)
              if (is-any-top-concept-p qualification)
              if (constraint-merging-trigger-p constraint)
              sum 1 into count
              else
              when (> number-restriction successor-ind-list-length)
              do (setf max-top-count (max number-restriction max-top-count))
              end ;when
              end ;constraint-merging-trigger-p
              else
              if (or qualification-top-concept-p
                     (and (not (eq negated-qualification concept))
                          (or (null concepts)
                              (not (qualification-contained-p negated-qualification concepts)))))
              sum (concept-number-restriction concept) into count
              and
              when (role-reflexive-p (concept-role concept))
              sum 1 into count
              end ;when
              else collect constraint into satisfied-exists-constraints
              end ;qualification
              else
              if (and (subrole-p (concept-role concept) bound-role)
                      (or qualification-top-concept-p
                          (and (not (eq negated-qualification concept))
                               (not (qualification-contained-p negated-qualification concepts)))))
              sum (concept-number-restriction concept) into count
              else collect constraint into satisfied-exists-constraints
              end ;subrole-p
              finally
              (return (values (max count max-top-count) satisfied-exists-constraints)))
        (loop with successor-counter = 0
              with expanded-store = (state-expanded-store state)
              with expanded-store-index = (state-expanded-store-index state)
              with unexpanded-exists-constraints-store = (state-unexpanded-exists-constraints-store state)
              with unexpanded-exists-constraints = (state-unexpanded-exists-constraints state)
              for successor-ind in successor-ind-list do
              (when (or qualification-top-concept-p
                        (not (or (ind-selected-constraints-p
                                  successor-ind
                                  (lambda (constraint)
                                    (let ((concept
                                           (if (constraint-negated-p constraint)
                                               (concept-negated-concept
                                                (constraint-term constraint))
                                             (constraint-term constraint))))
                                      (and (eq concept negated-qualification)
                                           #+:debug constraint)))
                                  expanded-constraints
                                  expanded-store
                                  expanded-store-index)
                                 (ind-selected-constraints-p
                                  successor-ind
                                  (lambda (constraint)
                                    (let ((concept
                                           (if (constraint-negated-p constraint)
                                               (concept-negated-concept
                                                (constraint-term constraint))
                                             (constraint-term constraint))))
                                      (and (eq concept negated-qualification)
                                           #+:debug constraint)))
                                  unexpanded-exists-constraints
                                  unexpanded-exists-constraints-store))))
                (incf successor-counter))
              finally
              (return (values (+ successor-counter count)
                              first-rel-constraint
                              successor-ind-list
                              satisfied-exists-constraints)))))))

(defun at-most-constraint-less-p (term-1 term-2 no-restr-1 no-restr-2 role-1 role-2)
  (let ((term-1-top (is-any-top-concept-p term-1))
        (term-2-top (is-any-top-concept-p term-2)))
    (or (and term-1-top (not term-2-top))
        (and (or (and term-1-top term-2-top)
                 (not (or term-1-top term-2-top)))
             (or (< no-restr-1 no-restr-2)
                 (and (not (eq role-1 role-2))
                      (or (member role-1 (role-ancestors-internal role-2))
                          (< (length (role-ancestors-internal role-1))
                             (length (role-ancestors-internal role-2))))))))))

(defun partition-less-p (partition-1 partition-2)
  (let* ((top *top-concept*)
         (datatype-top *datatype-top-concept*)
         (at-most-constraint-1 (third partition-1))
         (at-most-concept-1 (when at-most-constraint-1
                              (constraint-term at-most-constraint-1)))
         (at-most-role-1 (if at-most-concept-1
                             (concept-role at-most-concept-1)
                           (first partition-1)))
         (at-most-constraint-2 (third partition-2))
         (at-most-concept-2 (when at-most-constraint-2
                              (constraint-term at-most-constraint-2)))
         (at-most-role-2 (if at-most-concept-2
                             (concept-role at-most-concept-2)
                           (first partition-2)))
         (qualification-1 (if at-most-concept-1
                              (concept-term at-most-concept-1)
                            (if (role-datatype at-most-role-1)
                                datatype-top
                              top)))
         (at-most-no-restr-1 (if at-most-concept-1
                                 (concept-number-restriction at-most-concept-1)
                               2))
         (qualification-2 (if at-most-concept-2
                              (concept-term at-most-concept-2)
                            (if (role-datatype at-most-role-2)
                                datatype-top
                              top)))
         (at-most-no-restr-2 (if at-most-concept-2
                                 (concept-number-restriction at-most-concept-2)
                               2)))
    (at-most-constraint-less-p qualification-1
                               qualification-2
                               at-most-no-restr-1
                               at-most-no-restr-2
                               at-most-role-1
                               at-most-role-2)))

(defun select-most-restricted-partition (partitions)
  (if (null (rest partitions))
      (first partitions)
    (loop for partition in partitions
          for at-most-constraint = (third partition)
          if (and at-most-constraint
                  (member-if (lambda (constraint)
                               (eq (constraint-merging-trigger-p constraint) at-most-constraint))
                             (fourth partition)))
          collect partition into trigger-partitions
          else
          collect partition into other-partitions
          finally
          (if trigger-partitions
              (return (first (sort trigger-partitions #'partition-less-p)))
            (return (first (sort other-partitions #'partition-less-p)))))))

(defun expanded-exists-partitions-satisfiable-1 (ind
                                                 exists-partitions
                                                 state
                                                 unused-1
                                                 unused-2)
  (declare (ignore unused-1 unused-2))
  (let ((expanded-constraints (state-expanded-constraints state))
        (relation-constraints (state-relation-constraints state))
        (labels (state-labels state))
        (indirectly-blocked-individuals (state-indirectly-blocked-individuals state)))
    (race-trace ("~&Testing satisfiability of partitions ~S, state=~S~%" exists-partitions state))
    (let ((partition (select-most-restricted-partition exists-partitions)))
      (destructuring-bind (violated-role
                           max-inds
                           at-most-constraint
                           related-r-exists-constraints
                           violated-exists
                           old-inds)
                          partition
        (declare (ignore max-inds old-inds))
        (race-trace ("~&Trying to merge partition ~S~%" partition))
        ;(print (list ind (constraint-concept at-most-constraint)))
        ;(when (numberp ind) (break))
        (multiple-value-bind (obsolete-individuals remaining-relation-constraints new-rel-store)
                             (if violated-exists
                               (get-obsolete-individuals violated-exists
                                                         relation-constraints
                                                         state)
                               (values nil relation-constraints (state-relation-store state)))
          (let* ((new-violated-exists (when violated-exists
                                        (copy-violated-exists-constraints violated-exists)))
                 (all-exists-constraints
                  (when (and *tableaux-caching*
                             *merging-partitions-caching*
                             (if *use-relation-store*
                               (relation-store-empty-p new-rel-store)
                               (null relation-constraints))
                             (null violated-exists))
                    (collect-ind-selected-constraints
                     ind
                     (state-unexpanded-exists-constraints state)
                     (state-unexpanded-exists-constraints-store state)))))
            (multiple-value-bind
              (new-unexpanded-exists-constraints new-unexp-exists-store)
              (remove-constraints-from-constraint-store related-r-exists-constraints
                                                        (state-unexpanded-exists-constraints state)
                                                        (state-unexpanded-exists-constraints-store
                                                         state)
                                                        (state-copy-unexpanded-exists-constraints-store-p state)
                                                        state
                                                        #'reset-exists-copy)
              (let* ((all-constraints
                      (when all-exists-constraints
                        (get-related-all-some-atmost-constraints all-exists-constraints
                                                                 state)))
                     (label (and all-constraints
                                 (construct-partition-label ind
                                                            (append all-exists-constraints
                                                                    all-constraints))))
                     (old-result (and label (get-model label)))
                     (incoherent-p (incoherent-model-p old-result)))
                (if (and old-result
                         (or incoherent-p
                             (not (state-save-completion state))))
                  (if incoherent-p
                    (progn
                      (set-clash-reasons (append all-exists-constraints
                                                 all-constraints
                                                 (list relation-constraints)))
                      (set-clash-dependencies
                       (union-dependencies (collect-dependencies all-exists-constraints)
                                           (collect-dependencies all-constraints)))
                      (race-trace ("~&Merging partition cache hit for unsatisfiable constraints ~S ~S, dep=~S~%"
                                   all-exists-constraints
                                   all-constraints
                                   *catching-clash-dependencies*))
                      (incf-statistics *tableaux-cache-unsat-hits*)
                      (handle-clash-with-backtrack-state state nil nil nil nil))
                    (progn
                      (race-trace ("~&Merging partition cache hit for satisfiable constraints ~S ~S~%"
                                   all-exists-constraints all-constraints))
                      (incf-statistics *tableaux-cache-sat-hits*)
                      ;(break)
                      (let* ((new-expanded-constraints
                              (append related-r-exists-constraints expanded-constraints))
                             (new-state 
                              (changed-kernel-state state
                                                    :expanded-constraints new-expanded-constraints
                                                    :unexpanded-exists-constraints
                                                    new-unexpanded-exists-constraints
                                                    :relation-store new-rel-store
                                                    :unexpanded-exists-constraints-store
                                                    new-unexp-exists-store)))
                        (added-constraints-satisfiable nil nil new-state t nil))))
                  (let ((new-state-1 (changed-kernel-state state
                                                           :unexpanded-exists-constraints
                                                           new-unexpanded-exists-constraints
                                                           :relation-store new-rel-store
                                                           :unexpanded-exists-constraints-store
                                                           new-unexp-exists-store)))
                    (push nil *collected-dependencies*)
                    (push nil *collected-ind-dependencies*)
                    (push nil *tableaux-unsat-caching*)
                    (flet
                      ((expanded-exists-partitions-satisfiable-1-continuation (sat-p
                                                                               xstate
                                                                               ignore-1
                                                                               ignore-2
                                                                               ignore-3)
                         (declare (ignore ignore-1 ignore-2 ignore-3))
                         (if sat-p
                           (progn
                             (race-trace ("~&Merging of partition ~S succeeded~%" partition))
                             (when (and *tableaux-caching* *merging-partitions-caching*)
                               (incf-statistics *tableaux-cache-misses*))
                             (when old-result
                               (assert (or (and (eq sat-p t) (eq old-result t))
                                           (and (state-save-completion state)
                                                (eq old-result t)
                                                (null sat-p)))))
                             (when (and *tableaux-caching*
                                        *tableaux-sat-caching*
                                        *merging-partitions-caching*
                                        (not (true-old-individual-p ind))
                                        (if *use-relation-store*
                                          (relation-store-empty-p new-rel-store)
                                          (null relation-constraints))
                                        (null violated-exists)
                                        (or (not (dl-reflexive-roles *dl-prover-language*))
                                            (loop for constraint in all-exists-constraints
                                                  never
                                                  (some #'user-defined-role-reflexive-p
                                                        (role-ancestors-internal
                                                         (concept-role (constraint-term constraint)))))))
                               (add-sat-model label))
                             (pop-collected-dependencies)
                             (pop *collected-ind-dependencies*)
                             (pop *tableaux-unsat-caching*)
                             (added-constraints-satisfiable nil nil xstate t nil))
                           (let ((collected-dependencies
                                  (clean-constraints *or-level* (pop-collected-dependencies))))
                             (pop *collected-ind-dependencies*)
                             #+:debug
                             (when (>= (length collected-dependencies) 1000)
                               (format t "Dep:~D " (length collected-dependencies)) ;(break)
                               )
                             (race-trace ("~&Collected dependencies = ~S~%" collected-dependencies)
                                         ("~&Merging of partition ~S failed~%" partition))
                             (when collected-dependencies
                               (add-signature-dependencies collected-dependencies)
                               (race-trace ("~&Adding collected dependencies ~S for ~S, new dep= ~S~%"
                                            collected-dependencies
                                            related-r-exists-constraints
                                            *catching-clash-dependencies*)))
                             (let ((original-catching-clash-dependencies
                                    *catching-clash-dependencies*)
                                   (signatures (remove-if #'concept-constraint-p
                                                          *catching-clash-dependencies*)))
                               (when signatures
                                 ;vh: in case of higher-level signatures we cannot remove them
                                 ; because we do not easily know what is global or local
                                 ;(remove-dependencies signatures)
                                 (add-signature-dependencies signatures))
                               (add-clash-dependencies
                                (collect-dependencies *catching-clash-dependencies*))
                               (if (and *use-dependency-based-instance-retrieval*
                                        (or (boundp '*clash-culprits*)
                                            (boundp '*clash-reasons*))
                                        (null *catching-clash-dependencies*))
                                 (set-clash-reasons
                                  (append (reduce #'append
                                                  (union all-exists-constraints
                                                         related-r-exists-constraints)
                                                  :key #'constraint-dependencies)
                                          (reduce #'append
                                                  signatures
                                                  :key #'signature-dependencies)
                                          (list relation-constraints)))
                                 (set-clash-reasons (append original-catching-clash-dependencies
                                                            (list relation-constraints)))))
                             (race-trace ("~&Backjumping with dependencies ~S~%"
                                          *catching-clash-dependencies*))
                             (when (and *tableaux-caching*
                                        *merging-partitions-caching*
                                        (not (true-old-individual-p ind))
                                        (if *use-relation-store*
                                          (relation-store-empty-p new-rel-store)
                                          (null relation-constraints))
                                        (null violated-exists))
                               (let ((invalid-model-concepts
                                      (when (and labels
                                                 (or *model-merging*
                                                     *tableaux-caching*
                                                     *merging-partitions-caching*))
                                        (get-label-info-dependent-models (first labels)))))
                                 (when invalid-model-concepts
                                   (invalidate-concept-models invalid-model-concepts))
                                 (when (and (first *tableaux-unsat-caching*)
                                            (some #'constraint-signature all-exists-constraints))
                                   (add-unsat-model label *catching-clash-dependencies*))))
                             (handle-clash-with-backtrack-state state nil nil nil nil)))))
                      (if (and at-most-constraint
                               (eq (clash-in-cs-p at-most-constraint
                                                  related-r-exists-constraints
                                                  new-state-1
                                                  nil)
                                   t))
                        (expanded-exists-partitions-satisfiable-1-continuation nil state nil nil nil)
                        (progn
                          (when new-violated-exists
                            (race-trace ("~&Retrying exists constraints ~S (~S)~%"
                                         new-violated-exists remaining-relation-constraints)))
                          (if obsolete-individuals
                            (let ((unexpanded-disjunctive-constraints-store
                                   (state-unexpanded-disjunctive-constraints-store new-state-1))
                                  (unexpanded-disjunctive-constraints
                                   (state-unexpanded-disjunctive-constraints new-state-1))
                                  (expanded-store (state-expanded-store new-state-1)))
                              (multiple-value-bind (new-expanded-constraints-1
                                                    new-expanded-store-1
                                                    new-expanded-store-index-1)
                                                   (remove-obsolete-expanded-constraints obsolete-individuals
                                                                                         expanded-constraints
                                                                                         expanded-store
                                                                                         (state-expanded-store-index state)
                                                                                         new-state-1)
                                (multiple-value-bind
                                  (new-expanded-constraints-2
                                   new-expanded-store-2
                                   new-expanded-store-index-2)
                                  (remove-constraints-from-constraint-store violated-exists
                                                                            new-expanded-constraints-1
                                                                            new-expanded-store-1
                                                                            (state-copy-expanded-store-p new-state-1)
                                                                            new-state-1
                                                                            #'reset-expanded-copy
                                                                            new-expanded-store-index-1)
                                  (multiple-value-bind
                                    (new-remaining-exists-constraints
                                     new-unexpanded-exists-constraints-store)
                                    (if obsolete-individuals
                                      (remove-individuals-from-constraint-store obsolete-individuals
                                                                                new-unexpanded-exists-constraints
                                                                                new-unexp-exists-store
                                                                                (state-copy-unexpanded-exists-constraints-store-p
                                                                                 new-state-1)
                                                                                new-state-1
                                                                                #'reset-exists-copy)
                                      (values new-unexpanded-exists-constraints new-unexp-exists-store))
                                    (multiple-value-bind
                                      (new-unexpanded-disjunctive-constraints
                                       new-unexpanded-disjunctive-constraints-store)
                                      (if (and obsolete-individuals 
                                               *use-unexpanded-disjunctive-constraints-store*
                                               unexpanded-disjunctive-constraints-store
                                               (not (constraint-store-unused-p unexpanded-disjunctive-constraints-store)))
                                        (remove-individuals-from-constraint-store obsolete-individuals
                                                                                  unexpanded-disjunctive-constraints
                                                                                  unexpanded-disjunctive-constraints-store
                                                                                  (state-copy-unexpanded-disjunctive-constraints-store-p
                                                                                   new-state-1)
                                                                                  new-state-1
                                                                                  #'reset-disjunctive-copy)
                                        (values (remove-obsolete-constraints obsolete-individuals
                                                                             unexpanded-disjunctive-constraints)
                                                unexpanded-disjunctive-constraints-store))
                                      (race-trace ("New constraints (~S ~S ~S)~%"
                                                   new-unexpanded-exists-constraints
                                                   new-expanded-constraints-2
                                                   remaining-relation-constraints)
                                                  ("~&Removed obsolete individuals ~S~%" obsolete-individuals))
                                      (let*
                                        ((new-state-2 (if (signature-kernel-state-p new-state-1)
                                                        (copy-signature-kernel-state new-state-1 t)
                                                        (copy-to-signature-kernel-state
                                                         new-state-1)))
                                         (new-partially-expanded-or-stack
                                          (push-backtrack-stack
                                           #'expanded-exists-partitions-satisfiable-1-continuation
                                           (state-partially-expanded-or-stack new-state-2)))
                                         (new-labels
                                          (remove-obsolete-label-infos
                                           obsolete-individuals
                                           indirectly-blocked-individuals
                                           labels))
                                         (new-state-3
                                          (changed-signature-state new-state-2
                                                                   :unexpanded-disjunctive-constraints
                                                                   new-unexpanded-disjunctive-constraints
                                                                   :unexpanded-exists-constraints
                                                                   new-remaining-exists-constraints
                                                                   :unexpanded-exists-constraints-store
                                                                   new-unexpanded-exists-constraints-store
                                                                   :violated-role violated-role
                                                                   :at-most-constraint at-most-constraint
                                                                   :true-some-constraints
                                                                   related-r-exists-constraints
                                                                   :expanded-constraints
                                                                   new-expanded-constraints-2
                                                                   :relation-constraints
                                                                   remaining-relation-constraints
                                                                   :labels new-labels
                                                                   :expanded-store new-expanded-store-2
                                                                   :expanded-store-index new-expanded-store-index-2
                                                                   :unexpanded-disjunctive-constraints-store
                                                                   new-unexpanded-disjunctive-constraints-store
                                                                   :partially-expanded-or-stack
                                                                   new-partially-expanded-or-stack)))
                                        (merge-constraints ind
                                                           new-state-3
                                                           nil nil nil)))))))
                            (multiple-value-bind
                              (new-expanded-constraints new-expanded-store new-expanded-store-index)
                              (remove-constraints-from-constraint-store violated-exists
                                                                        expanded-constraints
                                                                        (state-expanded-store state)
                                                                        (state-copy-expanded-store-p new-state-1)
                                                                        new-state-1
                                                                        #'reset-expanded-copy
                                                                        (state-expanded-store-index state))
                              (let* ((new-state-2 (if (signature-kernel-state-p new-state-1)
                                                    (copy-signature-kernel-state new-state-1 t)
                                                    (copy-to-signature-kernel-state
                                                     new-state-1)))
                                     (new-partially-expanded-or-stack
                                      (push-backtrack-stack
                                       #'expanded-exists-partitions-satisfiable-1-continuation
                                       (state-partially-expanded-or-stack new-state-2)))
                                     (new-state-3
                                      (changed-signature-state new-state-2
                                                               :violated-role violated-role
                                                               :at-most-constraint at-most-constraint
                                                               :true-some-constraints
                                                               related-r-exists-constraints
                                                               :expanded-constraints
                                                               new-expanded-constraints
                                                               :relation-constraints
                                                               remaining-relation-constraints
                                                               :expanded-store new-expanded-store
                                                               :expanded-store-index new-expanded-store-index
                                                               :partially-expanded-or-stack
                                                               new-partially-expanded-or-stack)))
                                (merge-constraints ind
                                                   new-state-3
                                                   nil nil nil)))))))))))))))))

(defun expanded-exists-partitions-satisfiable-2 (ind
                                                 exists-partitions
                                                 state
                                                 unused-1
                                                 unused-2)
  (declare (ignore unused-1 unused-2))
  (let ((expanded-constraints (state-expanded-constraints state))
        (relation-constraints (state-relation-constraints state))
        (labels (state-labels state))
        (indirectly-blocked-individuals (state-indirectly-blocked-individuals state)))
    (race-trace ("~&Testing satisfiability of partitions ~S, state=~S~%" exists-partitions state))
    (let ((partition (select-most-restricted-partition exists-partitions)))
      (destructuring-bind (violated-role
                           max-inds
                           at-most-constraint
                           related-r-exists-constraints
                           violated-exists
                           old-inds)
                          partition
        (declare (ignore max-inds old-inds))
        (race-trace ("~&Trying to merge partition ~S~%" partition))
        ;(print (list ind (constraint-concept at-most-constraint)))
        ;(when (numberp ind) (break))
        (multiple-value-bind (obsolete-individuals remaining-relation-constraints new-rel-store)
                             (if violated-exists
                               (get-obsolete-individuals violated-exists
                                                         relation-constraints
                                                         state)
                               (values nil relation-constraints (state-relation-store state)))
          (let ((new-violated-exists (when violated-exists
                                       (copy-violated-exists-constraints violated-exists))))
            (multiple-value-bind
              (new-unexpanded-exists-constraints new-unexp-exists-store)
              (remove-constraints-from-constraint-store related-r-exists-constraints
                                                        (state-unexpanded-exists-constraints state)
                                                        (state-unexpanded-exists-constraints-store
                                                         state)
                                                        (state-copy-unexpanded-exists-constraints-store-p state)
                                                        state
                                                        #'reset-exists-copy)
              (let ((new-state-1 (changed-kernel-state state
                                                       :unexpanded-exists-constraints
                                                       new-unexpanded-exists-constraints
                                                       :relation-store new-rel-store
                                                       :unexpanded-exists-constraints-store
                                                       new-unexp-exists-store)))
                (push nil *collected-dependencies*)
                (push nil *collected-ind-dependencies*)
                (flet
                  ((expanded-exists-partitions-satisfiable-2-continuation (sat-p
                                                                           xstate
                                                                           unused-1 unused-2 unused-3)
                     (declare (ignore unused-1 unused-2 unused-3))
                     (if sat-p
                       (progn
                         (race-trace ("~&Merging of partition ~S succeeded~%" partition))
                         (pop-collected-dependencies)
                         (pop *collected-ind-dependencies*)
                         (added-constraints-satisfiable nil nil xstate t nil))
                       (let ((collected-dependencies
                              (clean-constraints *or-level* (pop-collected-dependencies))))
                         (pop *collected-ind-dependencies*)
                         (race-trace ("~&Collected dependencies = ~S~%" collected-dependencies)
                                     ("~&Merging of partition ~S failed~%" partition))
                         (when collected-dependencies
                           (add-signature-dependencies collected-dependencies)
                           (race-trace ("~&Adding collected dependencies ~S for ~S, new dep= ~S~%"
                                            collected-dependencies
                                            related-r-exists-constraints
                                            *catching-clash-dependencies*)))
                         (let ((original-catching-clash-dependencies
                                *catching-clash-dependencies*)
                               (signatures (remove-if #'concept-constraint-p
                                                      *catching-clash-dependencies*)))
                           (when signatures
                             ;vh: in case of higher-level signatures we cannot remove them
                             ; because we do not easily know what is global or local
                             ;(remove-dependencies signatures)
                             (add-signature-dependencies signatures))
                           (add-clash-dependencies
                            (collect-dependencies *catching-clash-dependencies*))
                           (if (and *use-dependency-based-instance-retrieval*
                                    (or (boundp '*clash-culprits*)
                                        (boundp '*clash-reasons*))
                                    (null *catching-clash-dependencies*))
                             (set-clash-reasons
                              (append (reduce #'append
                                              related-r-exists-constraints
                                              :key #'constraint-dependencies)
                                      (reduce #'append
                                              signatures
                                              :key #'signature-dependencies)
                                      (list relation-constraints)))
                             (set-clash-reasons (append original-catching-clash-dependencies
                                                        (list relation-constraints)))))
                         (race-trace ("~&Backjumping with dependencies ~S~%"
                                      *catching-clash-dependencies*))
                         (handle-clash-with-backtrack-state new-state-1 nil nil nil nil)))))
                  (if (and at-most-constraint
                           (eq (clash-in-cs-p at-most-constraint
                                              related-r-exists-constraints
                                              new-state-1
                                              nil)
                               t))
                    (expanded-exists-partitions-satisfiable-2-continuation nil state nil nil nil)
                    (progn
                      (when new-violated-exists
                        (race-trace ("~&Retrying exists constraints ~S (~S)~%"
                                     new-violated-exists remaining-relation-constraints)))
                      (if obsolete-individuals
                        (let ((unexpanded-disjunctive-constraints-store
                               (state-unexpanded-disjunctive-constraints-store new-state-1))
                              (unexpanded-disjunctive-constraints
                               (state-unexpanded-disjunctive-constraints new-state-1))
                              (expanded-store (state-expanded-store new-state-1)))
                          (multiple-value-bind (new-expanded-constraints-1
                                                new-expanded-store-1
                                                new-expanded-store-index-1)
                                               (remove-obsolete-expanded-constraints obsolete-individuals
                                                                                     expanded-constraints
                                                                                     expanded-store
                                                                                     (state-expanded-store-index state)
                                                                                     new-state-1)
                            (multiple-value-bind
                              (new-expanded-constraints-2
                               new-expanded-store-2
                               new-expanded-store-index-2)
                              (remove-constraints-from-constraint-store violated-exists
                                                                        new-expanded-constraints-1
                                                                        new-expanded-store-1
                                                                        (state-copy-expanded-store-p new-state-1)
                                                                        new-state-1
                                                                        #'reset-expanded-copy
                                                                        new-expanded-store-index-1)
                              (multiple-value-bind
                                (new-remaining-exists-constraints
                                 new-unexpanded-exists-constraints-store)
                                (if obsolete-individuals
                                  (remove-individuals-from-constraint-store obsolete-individuals
                                                                            new-unexpanded-exists-constraints
                                                                            new-unexp-exists-store
                                                                            (state-copy-unexpanded-exists-constraints-store-p
                                                                             new-state-1)
                                                                            new-state-1
                                                                            #'reset-exists-copy)
                                  (values new-unexpanded-exists-constraints new-unexp-exists-store))
                                (multiple-value-bind
                                  (new-unexpanded-disjunctive-constraints
                                   new-unexpanded-disjunctive-constraints-store)
                                  (if (and obsolete-individuals 
                                           *use-unexpanded-disjunctive-constraints-store*
                                           unexpanded-disjunctive-constraints-store
                                           (not (constraint-store-unused-p unexpanded-disjunctive-constraints-store)))
                                    (remove-individuals-from-constraint-store obsolete-individuals
                                                                              unexpanded-disjunctive-constraints
                                                                              unexpanded-disjunctive-constraints-store
                                                                              (state-copy-unexpanded-disjunctive-constraints-store-p
                                                                               new-state-1)
                                                                              new-state-1
                                                                              #'reset-disjunctive-copy)
                                    (values (remove-obsolete-constraints obsolete-individuals
                                                                         unexpanded-disjunctive-constraints)
                                            unexpanded-disjunctive-constraints-store))
                                  (race-trace ("New constraints (~S ~S ~S)~%"
                                               new-unexpanded-exists-constraints
                                               new-expanded-constraints-2
                                               remaining-relation-constraints)
                                              ("~&Removed obsolete individuals ~S~%" obsolete-individuals))
                                  (let* ((new-state-2 (if (signature-kernel-state-p new-state-1)
                                                        (copy-signature-kernel-state new-state-1 t)
                                                        (copy-to-signature-kernel-state
                                                         new-state-1)))
                                         (new-labels
                                          (remove-obsolete-label-infos
                                           obsolete-individuals
                                           indirectly-blocked-individuals
                                           labels))
                                         (new-partially-expanded-or-stack
                                          (push-backtrack-stack #'expanded-exists-partitions-satisfiable-2-continuation
                                                                (state-partially-expanded-or-stack new-state-2)))
                                         (new-state-3
                                          (changed-signature-state new-state-2
                                                                   :unexpanded-disjunctive-constraints
                                                                   new-unexpanded-disjunctive-constraints
                                                                   :unexpanded-exists-constraints
                                                                   new-remaining-exists-constraints
                                                                   :unexpanded-exists-constraints-store
                                                                   new-unexpanded-exists-constraints-store
                                                                   :violated-role violated-role
                                                                   :at-most-constraint at-most-constraint
                                                                   :true-some-constraints
                                                                   related-r-exists-constraints
                                                                   :expanded-constraints
                                                                   new-expanded-constraints-2
                                                                   :relation-constraints
                                                                   remaining-relation-constraints
                                                                   :labels new-labels
                                                                   :expanded-store new-expanded-store-2
                                                                   :expanded-store-index new-expanded-store-index-2
                                                                   :unexpanded-disjunctive-constraints-store
                                                                   new-unexpanded-disjunctive-constraints-store
                                                                   :partially-expanded-or-stack
                                                                   new-partially-expanded-or-stack)))
                                    (merge-constraints ind
                                                       new-state-3
                                                       nil nil nil)))))))
                        (multiple-value-bind
                          (new-expanded-constraints new-expanded-store new-expanded-store-index)
                          (remove-constraints-from-constraint-store violated-exists
                                                                    expanded-constraints
                                                                    (state-expanded-store state)
                                                                    (state-copy-expanded-store-p new-state-1)
                                                                    new-state-1
                                                                    #'reset-expanded-copy
                                                                    (state-expanded-store-index state))
                          (let* ((new-state-2 (if (signature-kernel-state-p new-state-1)
                                                (copy-signature-kernel-state new-state-1 t)
                                                (copy-to-signature-kernel-state
                                                 new-state-1)))
                                 (new-partially-expanded-or-stack
                                  (push-backtrack-stack #'expanded-exists-partitions-satisfiable-2-continuation
                                                        (state-partially-expanded-or-stack new-state-2)))
                                 (new-state-3
                                  (changed-signature-state new-state-2
                                                           :violated-role violated-role
                                                           :at-most-constraint at-most-constraint
                                                           :true-some-constraints
                                                           related-r-exists-constraints
                                                           :expanded-constraints
                                                           new-expanded-constraints
                                                           :relation-constraints
                                                           remaining-relation-constraints
                                                           :expanded-store new-expanded-store
                                                           :expanded-store-index new-expanded-store-index
                                                           :partially-expanded-or-stack
                                                           new-partially-expanded-or-stack)))
                            (merge-constraints ind
                                               new-state-3
                                               nil nil nil)))))))))))))))

(defun construct-partition-label (ind constraints)
  (loop for constraint in constraints
        for concept = (constraint-term constraint)
        when (eql ind (constraint-ind constraint))
        if (constraint-negated-p constraint)
        collect (concept-negated-concept concept) into concepts
        else
        collect concept into concepts
        finally
        (return (with-flatten-encodings
		    (without-optimized-encoding
		     (without-taxonomic-encoding
                    (let ((term (encode-concept-term `(and ,@concepts) nil t t)))
                      (if (listp term)
                        term
                        (list term)))))))))
