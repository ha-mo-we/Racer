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

(defvar *test-concept-name* (gensym))

(defun internal-individuals-related-p (ind-predecessor-name-set
                                       ind-filler-name-set
                                       role-term
                                       abox
                                       &optional (check-p t))
  (with-race-trace-sublevel ("internal-individuals-related-p"
                             :arguments (list ind-predecessor-name-set
                                              ind-filler-name-set
                                              role-term
                                              abox
                                              check-p)
                             :trace-result t)
    (setf abox (find-abox abox))        
    (when check-p
      (check-type abox abox)
      (check-role-term role-term))
    (when check-p
      (ensure-knowledge-base-state ':abox-prepared abox)
      (ensure-role-is-known role-term (tbox abox)))
    (unless (listp ind-predecessor-name-set)
      (setf ind-predecessor-name-set (individual-name-set (find-individual abox ind-predecessor-name-set))))
    (unless (listp ind-filler-name-set)
      (setf ind-filler-name-set (individual-name-set (find-individual abox ind-filler-name-set))))
    (let* ((tbox (abox-tbox abox))
           (role (if (consp role-term)
                     (role-inverse-internal (get-tbox-role tbox (second role-term)))
                   (get-tbox-role tbox role-term)))
           (transitive-subrole-p (some-user-defined-role-transitive-p (role-descendants-internal role)))
           (role-name (role-name role))
           (ind (find-individual abox (first ind-predecessor-name-set))))
      (when check-p
        (when (and (symbolp role-term) (role-cd-attribute role))
          (error "No attribute expected - found ~S." role-term))
        (unless (ensure-abox-is-coherent abox)
          (error "ABox ~A is incoherent." (abox-name abox))))
      (when (and check-p (role-cd-attribute role))
        (error "Role expected but attribute ~A found." role-term))
      (with-abox-settings abox
        (with-concept-definition-mapping tbox 
          (with-alc-bindings
            (multiple-value-bind (axiom-found-p individuals-possibly-transitively-related-p)
                (role-axiom-or-implied-transitive-role-found abox
                                                             ind
                                                             ind-filler-name-set
                                                             role
                                                             transitive-subrole-p
                                                             nil)
              (cond
               (axiom-found-p
                t)
               ((and transitive-subrole-p (not individuals-possibly-transitively-related-p))
                nil)
               (t
                (let ((ind2 (find-individual abox (first ind-filler-name-set))))
                  (cond
                   ((and (not individuals-possibly-transitively-related-p)
                         (individuals-not-related-in-additional-model-relations abox ind ind2 role))
                    nil)
                   ((role-axiom-does-not-hold abox
                                              ind-predecessor-name-set
                                              ind-filler-name-set
                                              role-name)
                    ;; Do we already know the individuals are not related?
                    nil)
                   (t
                    (create-tbox-internal-marker-concept tbox *test-concept-name*) 
                    (let ((test-concept (encode-concept-term *test-concept-name*)))
                      (setf (concept-visible-p test-concept) nil)
                      (let ((query-concept-1 (encode-concept-term 
                                              `(all ,role-name ,test-concept)))
                            (query-concept-2 (concept-negated-concept test-concept)))
                        (let ((subgraph (individual-subgraph ind2)))
                          (if (if *use-abox-precompletion*
                                  (test-abox-satisfiable
                                   abox
                                   (nconc (mapcar #'encode-constraint
                                                  (list (list (first ind-predecessor-name-set)
                                                              query-concept-1) 
                                                        (list (first ind-filler-name-set) query-concept-2)))
                                          (subgraph-pending-new-parents subgraph))
                                   nil
                                   nil
                                   nil
                                   nil
                                   (subgraph-precompletion subgraph))
                                (test-abox-satisfiable
                                 abox
                                 (mapcar #'encode-constraint
                                         (list* (list (first ind-predecessor-name-set)
                                                      query-concept-1) 
                                                (list (first ind-filler-name-set) query-concept-2)
                                                (subgraph-encoded-individual-axioms 
                                                 subgraph)))
                                 (subgraph-encoded-role-axioms subgraph)
                                 (subgraph-individual-names subgraph)
                                 (abox-attribute-constraints abox)
                                 (copy-solver-state (abox-initial-constraint-state abox))))
                              ;; Cache and return the negated satisfiability result
                              (progn
                                (push (list (list (first ind-predecessor-name-set)
                                                  (first ind-filler-name-set)) role-name)
                                      (abox-role-axioms-not-holding abox))
                                nil)
                            (progn
                              (push (list (list (first ind-predecessor-name-set)
                                                (first ind-filler-name-set)) role-name)
                                    (abox-role-axioms abox))
                              t)))))))))))))))))


(defun role-axiom-does-not-hold (abox ind-predecessor-name-set ind-filler-name-set role-name)
  (loop for ((ind1 ind2) role-name1) in (abox-role-axioms-not-holding abox)
        thereis (and (member ind1 ind-predecessor-name-set)
                     (member ind2 ind-filler-name-set)
                     (eq role-name1 role-name))))

(defun individuals-not-related-in-additional-model-relations (abox ind1 ind2 role)
  "This function presupposes that ind1 and ind2 are not directly related via role due to role assertions. ~
   This is assumed to be tested with role-axioms-directly found."
  (unless (individual-model-built-p ind1)
    (prepare-individual-model ind1 abox))
  (let* ((synonym-ind1 (or (gethash (first (individual-name-set ind1))
                                    (abox-model-individual-synonyms abox))
                           ind1))
         (added-relation-constraints (individual-added-relation-constraints synonym-ind1)))
    (if added-relation-constraints
        (progn
          (unless (individual-model-built-p ind2)
            (prepare-individual-model ind2 abox))
          (let ((synonym-ind2 (or (gethash (first (individual-name-set ind2))
                                           (abox-model-individual-synonyms abox))
                                  ind2)))
            (loop with ind-set-1 = (individual-name-set synonym-ind1)
                  with ind-set-2 = (individual-name-set synonym-ind2)
                  for relation-constraint in added-relation-constraints
                  never
                  (and (member role (role-ancestors-internal (constraint-term relation-constraint)))
                       (member (constraint-ind-1 relation-constraint) ind-set-1)
                       (member (constraint-ind-2 relation-constraint) ind-set-2)))))
      t)))

(defun role-axiom-or-implied-transitive-role-found (abox
                                                    current-ind
                                                    ind-filler-name-set
                                                    role
                                                    transitive-subrole-p
                                                    visited-flag)
  (when (or (null visited-flag) (not (eql (individual-flag current-ind) visited-flag)))
    (when visited-flag
      (setf (individual-flag current-ind) visited-flag))
    (let ((ind-name (first (individual-name-set current-ind))))
      (if (and (member ind-name ind-filler-name-set)
               (cond ((role-reflexive-p role) t)
                     ((dl-locally-reflexive-roles (abox-language abox))
                      (unless (individual-model-built-p current-ind)
                        (prepare-individual-model current-ind abox))
                      (loop for constraint in (individual-ind-concept-constraints current-ind)
                            for concept = (constraint-concept constraint)
                            thereis 
                            (and (some-concept-p concept)
                                 (concept-self-reference-p concept)
                                 (let ((concept-role (concept-role concept)))
                                   (if (role-has-ancestors-p role)
                                       (let ((ancestors (role-ancestors-internal role)))
                                         (or (member concept-role ancestors)
                                             (member (role-inverse-internal concept-role) ancestors)))
                                     (or (eq concept-role role)
                                         (eq (role-inverse-internal concept-role) role)))))))
                     (t nil)))
          t
        (if (or (role-compositions role)
                (member-if #'role-compositions (role-descendants-internal role)))
            (if (abox-el+-transformed-table abox)
                (and (find-pair-in-role-members current-ind
                                                (find-individual abox (first ind-filler-name-set))
                                                role
                                                (env-role-members (tbox-el+-environment (abox-tbox abox))))
                     t)
              (values nil t))
          (let* ((precompletion 
                  (when (and *use-abox-precompletion* *use-relation-store*)
                    (subgraph-precompletion (individual-subgraph current-ind))))
                 (relation-store 
                  (when precompletion
                    (state-relation-store (precompletion-state precompletion)))))
            (if relation-store
                (let ((successors (relation-successor-individuals ind-name role relation-store)))
                  (when successors
                    (if (lists-not-disjoint-p successors ind-filler-name-set)
                        t
                      (when (or visited-flag transitive-subrole-p)
                        (loop with visited-flag = (or visited-flag (incf *individual-set-mark*))
                              for successor-ind-name in successors
                              do
                              (multiple-value-bind (axiom-found-p individuals-possibly-transitively-related-p)
                                  (role-axiom-or-implied-transitive-role-found
                                   abox
                                   (find-individual abox successor-ind-name)
                                   ind-filler-name-set
                                   role
                                   transitive-subrole-p
                                   visited-flag)
                                (when (or axiom-found-p individuals-possibly-transitively-related-p)
                                  (return-from role-axiom-or-implied-transitive-role-found
                                    (if (and axiom-found-p (role-transitive-p role))
                                        t
                                      (values nil t))))))))))
              (or (loop for assertion in (individual-outgoing-role-assertions current-ind)
                        for ind-2 = (constraint-ind-2 assertion)
                        do
                        (when (member role (role-ancestors-internal (constraint-term assertion)))
                          (if (member ind-2 ind-filler-name-set)
                              (return t)
                            (when transitive-subrole-p
                              (unless visited-flag
                                (setf visited-flag (incf *individual-set-mark*)))
                              (multiple-value-bind (axiom-found-p
                                                    individuals-possibly-transitively-related-p)
                                  (role-axiom-or-implied-transitive-role-found abox
                                                                               (find-individual abox ind-2)
                                                                               ind-filler-name-set
                                                                               role
                                                                               transitive-subrole-p
                                                                               visited-flag)
                                (when (or axiom-found-p individuals-possibly-transitively-related-p)
                                  (return-from role-axiom-or-implied-transitive-role-found
                                    (if (and axiom-found-p (role-transitive-p role))
                                        t
                                      (values nil t)))))))))
                  (loop for assertion in (individual-incoming-role-assertions current-ind)
                        for ind-1 = (constraint-ind-1 assertion)
                        do
                        (when (member role (role-ancestors-internal
                                            (role-inverse-internal (constraint-term assertion))))
                          (if (member ind-1 ind-filler-name-set)
                              (return t)
                            (when transitive-subrole-p
                              (unless visited-flag
                                (setf visited-flag (incf *individual-set-mark*)))
                              (multiple-value-bind (axiom-found-p individuals-possibly-transitively-related-p)
                                  (role-axiom-or-implied-transitive-role-found abox
                                                                               (find-individual abox ind-1)
                                                                               ind-filler-name-set
                                                                               role
                                                                               transitive-subrole-p
                                                                               visited-flag)
                                (when (or axiom-found-p individuals-possibly-transitively-related-p)
                                  (return-from role-axiom-or-implied-transitive-role-found
                                    (if (and axiom-found-p (role-transitive-p role))
                                        t
                                      (values nil t)))))))))
                  (when transitive-subrole-p
                    (unless visited-flag
                      (setf visited-flag (incf *individual-set-mark*)))
                    (unless (individual-model-built-p current-ind)
                      (prepare-individual-model current-ind abox))
                    (loop with added-relation-constraints = (individual-added-relation-constraints current-ind)
                          with current-ind-set = (individual-name-set current-ind)
                          for relation-constraint in added-relation-constraints
                          for rel-role = (constraint-term relation-constraint)
                          do
                          (when (or (and (member role (role-ancestors-internal rel-role))
                                         (member (constraint-ind-1 relation-constraint) current-ind-set)
                                         (let ((ind-2 (constraint-ind-2 relation-constraint)))
                                           (if (member ind-2 ind-filler-name-set)
                                               t
                                             (multiple-value-bind (axiom-found-p
                                                                   individuals-possibly-transitively-related-p)
                                                 (role-axiom-or-implied-transitive-role-found
                                                  abox
                                                  (find-individual abox ind-2)
                                                  ind-filler-name-set
                                                  role
                                                  transitive-subrole-p
                                                  visited-flag)
                                               (or axiom-found-p
                                                   individuals-possibly-transitively-related-p)))))
                                    (and (member role
                                                 (role-ancestors-internal (role-inverse-internal rel-role)))
                                         (member (constraint-ind-2 relation-constraint) current-ind-set)
                                         (let ((ind-1 (constraint-ind-1 relation-constraint)))
                                           (if (member ind-1 ind-filler-name-set)
                                               t
                                             (multiple-value-bind (axiom-found-p
                                                                   individuals-possibly-transitively-related-p)
                                                 (role-axiom-or-implied-transitive-role-found
                                                  abox
                                                  (find-individual abox ind-1)
                                                  ind-filler-name-set
                                                  role
                                                  transitive-subrole-p
                                                  visited-flag)
                                               (or axiom-found-p
                                                   individuals-possibly-transitively-related-p))))))
                            (return-from role-axiom-or-implied-transitive-role-found
                              (or (role-transitive-p role)
                                  (values nil t))))))))))))))

(defun internal-retrieve-individual-filled-roles (ind-predecessor ind-filler abox)
  (check-type ind-predecessor symbol)
  (check-type ind-filler symbol)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (with-race-trace-sublevel ("retrieve-individual-relations"
                             :arguments (list ind-predecessor ind-filler abox)
                             :trace-result t)
    (ensure-knowledge-base-state ':abox-prepared abox)
    (unless (ensure-abox-is-coherent abox)
      (error "ABox ~A is incoherent." (abox-name abox)))
    (let ((result nil))
      (loop for role being the hash-values of (tbox-role-store (abox-tbox abox)) do
            (loop for (ind1 ind2) in (retrieve-related-individuals (role-name role) abox) 
                  when (and (eql ind1 ind-predecessor) (eql ind2 ind-filler))
                  do (pushnew role result)))
      (mapcar #'decode-role (sort result #'>
                                  :key #'(lambda (role)
                                           (length (role-ancestors-internal role))))))))

(defun internal-retrieve-individual-fillers (ind-predecessor role-term abox)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type ind-predecessor symbol)
  (check-role-term role-term)
  (with-race-trace-sublevel ("retrieve-individual-fillers"
                             :arguments (list ind-predecessor role-term abox)
                             :trace-result t)
    (ensure-knowledge-base-state ':abox-prepared abox)
    (ensure-role-is-known role-term (tbox abox))
    (ensure-no-cd-attribute role-term (tbox abox))
    (unless (ensure-abox-is-coherent abox)
      (error "ABox ~A is incoherent." (abox-name abox)))
    (let* ((precompletion 
            (when (and *use-abox-precompletion* *use-relation-store*)
              (subgraph-precompletion (individual-subgraph (find-individual abox ind-predecessor)))))
           (relation-store 
            (when precompletion
              (state-relation-store (precompletion-state precompletion)))))
      (if relation-store
          (relation-successor-individuals ind-predecessor
                                          (get-tbox-role (abox-tbox abox) role-term)
                                          relation-store)
        (loop for ind in (abox-individuals-list abox)
              for ind-name-set = (individual-name-set ind)
              when (internal-individuals-related-p ind-predecessor ind-name-set role-term abox nil)
              append ind-name-set)))))

(defun internal-retrieve-direct-predecessors (role-term ind-filler abox)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-role-term role-term)
  (check-type ind-filler symbol)
  (with-race-trace-sublevel ("retrieve-direct-predecessors"
                             :arguments (list role-term ind-filler abox)
                             :trace-result t)
    (ensure-knowledge-base-state ':abox-prepared abox)
    (ensure-role-is-known role-term (tbox abox))
    (ensure-no-cd-attribute role-term (tbox abox))
    (unless (ensure-abox-is-coherent abox)
      (error "ABox ~A is incoherent." (abox-name abox)))
    (let* ((precompletion 
            (when (and *use-abox-precompletion* *use-relation-store*)
              (subgraph-precompletion (individual-subgraph (find-individual abox ind-filler)))))
           (relation-store 
            (when precompletion
              (state-relation-store (precompletion-state precompletion)))))
      (if relation-store
          (relation-predecessor-individuals ind-filler
                                            (get-tbox-role (abox-tbox abox) role-term)
                                            relation-store)
        (loop for ind in (abox-individuals-list abox)
              for ind-name-set = (individual-name-set ind)
              when (internal-individuals-related-p ind-name-set ind-filler role-term abox nil)
              append ind-name-set)))))

(defun internal-retrieve-related-individuals (role-term abox)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-role-term role-term)
  (with-race-trace-sublevel ("retrieve-related-individuals"
                             :arguments (list role-term abox)
                             :trace-result t)
    (ensure-knowledge-base-state ':abox-prepared abox)
    (ensure-role-is-known role-term (tbox abox))
    (ensure-no-cd-attribute role-term (tbox abox))
    (unless (ensure-abox-is-coherent abox)
      (error "ABox ~A is incoherent." (abox-name abox)))
    (let* ((subgraphs (abox-subgraphs abox))
           (use-relation-store (and subgraphs *use-abox-precompletion* *use-relation-store*)))
      (loop with role = (get-tbox-role (abox-tbox abox) role-term)
            for subgraph in subgraphs
            for precompletion = (when use-relation-store 
                                  (subgraph-precompletion subgraph))
            for relation-store = (when precompletion 
                                   (state-relation-store (precompletion-state precompletion)))
            if relation-store
            nconc
            (loop for ind in (subgraph-individuals subgraph)
                  for ind-name-set = (individual-name-set ind)
                  for fillers = (if (rest ind-name-set)
                                    (loop for ind-name in ind-name-set
                                          for fillers = 
                                          (relation-successor-individuals ind-name
                                                                          role
                                                                          relation-store)
                                          when fillers
                                          do (return fillers))
                                  (relation-successor-individuals (first ind-name-set)
                                                                  role
                                                                  relation-store))
                  when fillers
                  nconc
                  (loop for ind-name-1 in ind-name-set
                        nconc (loop for ind-name-2 in fillers
                                    collect (list ind-name-1 ind-name-2))))
            else
            nconc
            (loop with individuals = (subgraph-individuals subgraph)
                  for ind1 in individuals
                  for ind-name-set-1 = (individual-name-set ind1)
                  nconc
                  (loop for ind2 in individuals
                        for ind-name-set-2 = (individual-name-set ind2)
                        when (internal-individuals-related-p ind-name-set-1
                                                             ind-name-set-2
                                                             role-term
                                                             abox
                                                             nil)
                        nconc (loop for ind-name-1 in ind-name-set-1
                                    nconc (loop for ind-name-2 in ind-name-set-2
                                                collect (list ind-name-1 ind-name-2)))))))))




