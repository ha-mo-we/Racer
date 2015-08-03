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
;;; Model caching
;;;===========================================================================

(defstruct (model-info (:conc-name model-)
                       (:constructor make-elh-model-info-internal (concept
                                                                   blocked-p
                                                                   det-positive-literals
                                                                   exists-models)))
  (concept nil)
  (blocked-p nil)
  (exists-models nil)
  (det-positive-literals nil)
  )

(defstruct (full-model-info (:conc-name model-)
                            (:include model-info)
                            (:constructor make-model-info-truly-internal (concept
                                                                          blocked-p
                                                                          exists-models
                                                                          det-positive-literals
                                                                          non-deterministic-p
                                                                          positive-literals
                                                                          negative-literals
                                                                          restrict-models
                                                                          individual
                                                                          language
                                                                          attributes
                                                                          ensured-attributes
                                                                          det-negative-literals)))
  (non-deterministic-p nil)
  (positive-literals nil)
  (negative-literals nil)
  (restrict-models nil)
  (individual nil)
  (language *dl-empty*)
  (attributes nil)
  (ensured-attributes nil)
  (det-negative-literals nil)
  )

#+:debug
(defun make-model-info-internal (&key (concept nil)
                                          (blocked-p nil)
                                          (exists-models nil)
                                          (det-positive-literals nil)
                                          (non-deterministic-p nil)
                                          (positive-literals nil)
                                          (negative-literals nil)
                                          (restrict-models nil)
                                          (individual nil)
                                          (language *dl-empty*)
                                          (attributes nil)
                                          (ensured-attributes nil)
                                          (det-negative-literals nil))
  (make-model-info-truly-internal  concept
                                   blocked-p
                                   exists-models
                                   det-positive-literals
                                   non-deterministic-p
                                   positive-literals
                                   negative-literals
                                   restrict-models
                                   individual
                                   language
                                   attributes
                                   ensured-attributes
                                   det-negative-literals))

#-:debug
(defmacro make-model-info-internal (&key (concept nil concept-p)
                                         (blocked-p nil blocked-p-p)
                                         (exists-models nil exists-models-p)
                                         (det-positive-literals nil det-positive-literals-p)
                                         (non-deterministic-p nil non-deterministic-p-p)
                                         (positive-literals nil positive-literals-p)
                                         (negative-literals nil negative-literals-p)
                                         (restrict-models nil restrict-models-p)
                                         (individual nil individual-p)
                                         (language *dl-empty* language-p)
                                         (attributes nil attributes-p)
                                         (ensured-attributes nil ensured-attributes-p)
                                         (det-negative-literals nil det-negative-literals-p))
  (declare (ignorable concept
                      blocked-p
                      exists-models
                      det-positive-literals
                      non-deterministic-p
                      positive-literals
                      negative-literals
                      restrict-models
                      individual
                      language
                      attributes
                      ensured-attributes
                      det-negative-literals))
  `(make-model-info-truly-internal ,(when concept-p concept)
                                   ,(when blocked-p-p blocked-p)
                                   ,(when exists-models-p exists-models)
                                   ,(when det-positive-literals-p det-positive-literals)
                                   ,(when non-deterministic-p-p non-deterministic-p)
                                   ,(when positive-literals-p positive-literals)
                                   ,(when negative-literals-p negative-literals)
                                   ,(when restrict-models-p restrict-models)
                                   ,(when individual-p individual)
                                   ,(if language-p language '*dl-empty*)
                                   ,(when attributes-p attributes)
                                   ,(when ensured-attributes-p ensured-attributes)
                                   ,(when det-negative-literals-p det-negative-literals)))

(defmethod print-object ((object model-info) stream)
  (call-next-method object stream))

(defmethod print-object ((object full-model-info) stream)
  (print-unreadable-object (object stream :type t :identity *print-internals*)
    (when (model-non-deterministic-p object)
      (format stream "? "))
    (if (model-individual object)
      (format stream "~S" (model-individual object))
      (format stream "~S" (model-concept object)))))

(defun elh-model-info-p (model)
  (and (model-info-p model) (not (full-model-info-p model))))

(defparameter *full-model-1* (make-model-info-internal))
(defparameter *full-model-2* (make-model-info-internal))

(defun convert-to-full-models (model-1 model-2)
  (values (if (or (full-model-info-p model-1)
                  (concept-p-internal model-1))
            model-1
            (convert-to-full-model model-1 *full-model-1*))
          (if (or (full-model-info-p model-2)
                  (concept-p-internal model-2))
            model-2
            (convert-to-full-model model-2 *full-model-2*))))

(defun convert-to-full-model (model new-model)
  (setf (model-blocked-p new-model) (model-blocked-p model) 
        (model-exists-models new-model) (model-exists-models model)
        (model-det-positive-literals new-model) (model-det-positive-literals model))
  new-model)

(defun model-info-equal-p (model-1 model-2)
  (if (full-model-info-p model-1)
    (if (full-model-info-p model-2)
      (and (eq (model-non-deterministic-p model-1) (model-non-deterministic-p model-2))
           (eq (model-language model-1) (model-language model-2))
           (eql (model-individual model-1) (model-individual model-2))
           (concept-set-equal (model-det-positive-literals model-1)
                              (model-det-positive-literals model-2))
           (concept-set-equal (model-det-negative-literals model-1)
                              (model-det-negative-literals model-2))
           (concept-set-equal (model-positive-literals model-1)
                              (model-positive-literals model-2))
           (concept-set-equal (model-negative-literals model-1)
                              (model-negative-literals model-2))
           (concept-set-equal (model-exists-models model-1) (model-exists-models model-2))
           (concept-set-equal (model-restrict-models model-1)
                              (model-restrict-models model-2))
           (concept-set-equal (model-attributes model-1) (model-attributes model-2))
           (concept-set-equal (model-ensured-attributes model-1)
                              (model-ensured-attributes model-2)))
      (and (not (model-non-deterministic-p model-1))
           (subset-el+-p (model-language model-1))
           (null (model-det-negative-literals model-1))
           (null (model-positive-literals model-1))
           (null (model-negative-literals model-1))
           (null (model-restrict-models model-1))
           (null (model-attributes model-1))
           (null (model-ensured-attributes model-1))
           (concept-set-equal (model-det-positive-literals model-1)
                              (model-det-positive-literals model-2))
           (concept-set-equal (model-exists-models model-1) (model-exists-models model-2))))
    (if (full-model-info-p model-2)
      (and (not (model-non-deterministic-p model-2))
           (subset-el+-p (model-language model-2))
           (null (model-det-negative-literals model-2))
           (null (model-positive-literals model-2))
           (null (model-negative-literals model-2))
           (null (model-restrict-models model-2))
           (null (model-attributes model-2))
           (null (model-ensured-attributes model-2))
           (concept-set-equal (model-det-positive-literals model-1)
                              (model-det-positive-literals model-2))
           (concept-set-equal (model-exists-models model-1) (model-exists-models model-2)))
      (and (concept-set-equal (model-det-positive-literals model-1)
                              (model-det-positive-literals model-2))
           (concept-set-equal (model-exists-models model-1) (model-exists-models model-2))))))
    

#+:debug
(defun make-model-info (&key (concept nil)
                                (blocked-p nil)
                                (non-deterministic-p nil)
                                (det-positive-literals nil)
                                (det-negative-literals nil)
                                (positive-literals nil)
                                (negative-literals nil)
                                (exists-models nil)
                                (restrict-models nil)
                                (attributes nil)
                                (ensured-attributes nil)
                                (individual nil)
                                (constraints nil)
                                (full-p nil)
                                (constraints-subset-elh-p nil))
  (make-model-info-1 concept
                     blocked-p
                     non-deterministic-p
                     det-positive-literals
                     det-negative-literals
                     positive-literals
                     negative-literals
                     exists-models
                     restrict-models
                     attributes
                     ensured-attributes
                     individual
                     constraints
                     full-p
                     constraints-subset-elh-p))

#-:debug
(defmacro make-model-info (&key (concept nil concept-p)
                                 (blocked-p nil blocked-p-p)
                                 (non-deterministic-p nil non-deterministic-p-p)
                                 (det-positive-literals nil det-positive-literals-p)
                                 (det-negative-literals nil det-negative-literals-p)
                                 (positive-literals nil positive-literals-p)
                                 (negative-literals nil negative-literals-p)
                                 (exists-models nil exists-models-p)
                                 (restrict-models nil restrict-models-p)
                                 (attributes nil attributes-p)
                                 (ensured-attributes nil ensured-attributes-p)
                                 (individual nil individual-p)
                                 (constraints nil constraints-p)
                                 (full-p nil full-p-p)
                                 (constraints-subset-elh-p nil constraints-subset-elh-p-p))
  (declare (ignorable concept
                      blocked-p
                      non-deterministic-p
                      det-positive-literals
                      det-negative-literals
                      positive-literals
                      negative-literals
                      exists-models
                      restrict-models
                      attributes
                      ensured-attributes
                      individual
                      constraints
                      full-p
                      constraints-subset-elh-p))
  `(make-model-info-1 ,(when concept-p concept)
                      ,(when blocked-p-p blocked-p)
                      ,(when non-deterministic-p-p non-deterministic-p)
                      ,(when det-positive-literals-p det-positive-literals)
                      ,(when det-negative-literals-p det-negative-literals)
                      ,(when positive-literals-p positive-literals)
                      ,(when negative-literals-p negative-literals)
                      ,(when exists-models-p exists-models)
                      ,(when restrict-models-p restrict-models)
                      ,(when attributes-p attributes)
                      ,(when ensured-attributes-p ensured-attributes)
                      ,(when individual-p individual)
                      ,(when constraints-p constraints)
                      ,(when full-p-p full-p)
                      ,(when constraints-subset-elh-p-p constraints-subset-elh-p)))

(defun make-model-info-1 (concept
                             blocked-p
                             non-deterministic-p
                             det-positive-literals
                             det-negative-literals
                             positive-literals
                             negative-literals
                             exists-models
                             restrict-models
                             attributes
                             ensured-attributes
                             individual
                             constraints
                             full-p
                             constraints-subset-elh-p)
  (let* ((elh-p (and *use-elh-model-embedding*
                     (null (or full-p
                               non-deterministic-p
                               det-negative-literals
                               positive-literals
                               negative-literals
                               restrict-models
                               attributes
                               ensured-attributes))
                     (or (null constraints) constraints-subset-elh-p)))
         (model
          (if elh-p
              (if (and (null exists-models)
                       (null (rest det-positive-literals))
                       (atomic-concept-p concept)
                       (null (concept-encoded-definition concept)))
                  concept
                (make-elh-model-info-internal concept
                                              blocked-p
                                              det-positive-literals
                                              exists-models))
            (make-model-info-internal :concept concept
                                      :blocked-p blocked-p
                                      :non-deterministic-p non-deterministic-p
                                      :det-positive-literals det-positive-literals
                                      :det-negative-literals det-negative-literals
                                      :positive-literals positive-literals
                                      :negative-literals negative-literals
                                      :exists-models exists-models
                                      :restrict-models restrict-models
                                      :attributes attributes
                                      :ensured-attributes ensured-attributes
                                      :individual (when (true-old-individual-p individual)
                                                    individual)
                                      :language *dl-empty*))))
    (unless elh-p
      (if (and concept (null *meta-constraint-concepts*))
        (setf (model-language model) (concept-language concept))
        (set-model-language model))
      (when (or attributes ensured-attributes)
        (setf (model-language model) (add-dl-features (model-language model)))))
    model))

(defun set-model-language (model)
  (when (full-model-info-p model)
    (let ((descriptor *dl-empty*)
          (det-positive-literals (model-det-positive-literals model))
          (det-negative-literals (model-det-negative-literals model))
          (restrict-models (model-restrict-models model))
          (exists-models (model-exists-models model))
          (positive-literals (model-positive-literals model))
          (negative-literals (model-negative-literals model))
          (attributes (model-attributes model))
          (ensured-attributes (model-ensured-attributes model)))
      (when det-positive-literals
        (setf descriptor
              (union-dl-descriptors descriptor
                                    (get-language-from-concepts det-positive-literals))))
      (when det-negative-literals
        (setf descriptor
              (union-dl-descriptors descriptor
                                    (get-language-from-negated-concepts det-negative-literals))))
      (when restrict-models
        (setf descriptor
              (union-dl-descriptors descriptor
                                    (get-language-from-concepts restrict-models))))
      (when exists-models
        (setf descriptor
              (union-dl-descriptors descriptor
                                    (get-language-from-concepts exists-models))))
      (when positive-literals
        (setf descriptor
              (union-dl-descriptors descriptor
                                    (get-language-from-concepts positive-literals))))
      (when negative-literals
        (setf descriptor
              (union-dl-descriptors descriptor
                                    (get-language-from-negated-concepts negative-literals))))
      (when (or attributes ensured-attributes)
        (setf descriptor (add-dl-features descriptor)))
      (setf (model-language model) descriptor))))

(defun get-language-from-negated-concepts (concept-list)
  (loop with language = *dl-empty*
        for orig-concept in concept-list
        for concept = (concept-negated-concept orig-concept)
        do
        (setf language (union-dl-descriptors language (concept-language concept)))
        finally (return language)))

(race-inline (make-simple-model make-model-copy))

(defun make-simple-model (concept full-p)
  #+:debug (assert (and (or (atomic-concept-p concept)
                            (negated-concept-p concept))
                        (null *meta-constraint-concepts*)))
  (if (negated-concept-p concept)
    (make-model-info :full-p full-p :det-negative-literals (list (concept-term concept)))
    (make-model-info :full-p full-p :det-positive-literals (list concept))))

(defun make-model-copy (source-model destination-concept full-p)
  (let ((new-model
         (if (model-info-p source-model)
           (if (or (not full-p) (full-model-info-p source-model))
             (copy-model-info source-model)
             (make-model-info-internal
              :blocked-p (model-blocked-p source-model)
              :exists-models (model-exists-models source-model)
              :det-positive-literals (model-det-positive-literals source-model)))
           (make-simple-model (if (model-info-p source-model) 
                                (model-concept source-model)
                                source-model)
                              full-p))))
    (when (model-info-p new-model)
      (setf (model-concept new-model) destination-concept))
    new-model))

(defun copy-to-primitive-model (source-concept destination-concept)
  #+:debug (assert (and (atomic-concept-p destination-concept)
                        (concept-primitive-p destination-concept)))
  (let ((model (concept-model source-concept)))
    (when model
      (if (incoherent-model-p model)
        model
        (unless (or (and (not (and *use-elh-model-embedding*
                                   (subset-el+-p (concept-language source-concept))
                                   (subset-el+-p (concept-language source-concept))))
                         (eq source-concept (concept-negated-concept destination-concept)))
                    (and (full-model-info-p model)
                         (or (member destination-concept (model-det-negative-literals model))
                             (member destination-concept (model-negative-literals model)))))
          (let* ((disjoints (concept-asserted-disjoints destination-concept))
                 (new-model (make-model-copy model destination-concept disjoints)))
            (push destination-concept (model-det-positive-literals new-model))
            (when disjoints
              (if (member destination-concept disjoints)
                (setf new-model *bottom-concept*)
                (setf (model-det-negative-literals new-model)
                      (concept-set-union disjoints (model-det-negative-literals new-model)))))
            (when (full-model-info-p new-model)
              (set-model-language new-model))
            new-model))))))

(defun copy-to-neg-primitive-model (source-concept destination-concept)
  #+:debug (assert (and (negated-concept-p destination-concept)
                        (atomic-concept-p (concept-term destination-concept))
                        (concept-primitive-p (concept-term destination-concept))))
  (let ((model (concept-model source-concept)))
    (when (and model (model-info-p model))
      (if (incoherent-model-p model)
        model
        (let ((destination-term (concept-term destination-concept)))
          (unless (or (eq source-concept (concept-negated-concept destination-concept))
                      (member destination-term (model-det-positive-literals model))
                      (and (full-model-info-p model)
                           (member destination-term (model-positive-literals model))))
            (let ((new-model (make-model-copy model destination-concept t)))
              (push destination-term (model-det-negative-literals new-model))
              (set-model-language new-model)
              new-model)))))))

(race-inline (incoherent-model-p))

(defun incoherent-model-p (model)
  (eq model *bottom-concept*))

(defvar *expanded-model*)
(defvar *save-expanded-model* nil)

(defun make-model-from-tableau (expanded-constraints
                                    expanded-store
                                    expanded-store-index
                                    relation-constraints
                                    relation-store
                                    &optional (satisfiable-concept nil)
                                    (blocked-p nil)
                                    (individual nil)
                                    (root-constraint nil))
  (let ((ind-name (if (individual-p-internal individual)
                    (first (individual-name-set individual))
                    individual)))
    #+:debug
    (assert (or (null root-constraint)
                (and *model-merging*
                     (or *use-alternate-models* *use-alternate-ind-models*)
                     (eql ind-name (constraint-ind root-constraint)))))
    (loop with det-positive-literals = nil
          with det-negative-literals = nil
          with positive-literals = nil
          with negative-literals = nil
          with exists-models = nil
          with restrict-models = nil
          with attributes = nil
          with ensured-attributes = nil
          with non-deterministic-p = (or-concept-p satisfiable-concept)
          with collected = nil
          with not-collected = nil
          with subset-elh-p = *use-elh-model-embedding*
          with top = *top-concept*
          for constraints = (if (or satisfiable-concept root-constraint (null ind-name))
                              (get-all-expanded-constraints expanded-constraints
                                                            expanded-store)
                              (collect-ind-selected-constraints ind-name
                                                                expanded-constraints
                                                                expanded-store
                                                                expanded-store-index))
          then not-collected
          for first-loop = root-constraint then nil
          do
          (loop for constraint in constraints
                for or-dependencies = (constraint-or-dependencies constraint)
                for concept = (constraint-term constraint)
                for ind = (constraint-ind constraint)
                do
                #+:debug
                (when (and *model-merging*
                           (or *use-alternate-models* *use-alternate-ind-models*))
                  (assert (constraint-derived-from constraint)))
                (when subset-elh-p
                  (let ((language (concept-language (if (constraint-negated-p constraint)
                                                      (concept-negated-concept concept)
                                                      concept))))
                    (unless (subset-el+-p language)
                      (setf subset-elh-p nil))))
                (if (if root-constraint
                      (when (eql ind ind-name)
                        (if first-loop
                          (or (member root-constraint (constraint-derived-from constraint))
                              (some (lambda (constraint)
                                      (and (concept-constraint-p constraint)
                                           (constraint-meta-p constraint)))
                                    (constraint-derived-from constraint)))
                          (not (lists-disjoint-p (constraint-derived-from constraint)
                                                 collected))))
                      (or (null ind-name)
                          (eql ind-name (constraint-ind constraint))))
                  (progn
                    (when root-constraint
                      (push constraint collected))
                    (when (and (not non-deterministic-p) or-dependencies)
                      (setf non-deterministic-p t))
                    (cond
                     ((atomic-concept-p concept)
                      (if (constraint-negated-p constraint)
                        (if or-dependencies
                          (push concept negative-literals)
                          (push concept det-negative-literals))
                        (unless (eq concept top)
                          (if or-dependencies
                            (push concept positive-literals)
                            (push concept det-positive-literals)))))
                     ((and-concept-p concept)
                      (when (and (not non-deterministic-p) (constraint-negated-p constraint))
                        (setf non-deterministic-p t)))
                     ((exists-concept-p concept)
                      (if (constraint-negated-p constraint)
                          (unless (and (at-least-concept-p concept)
                                       (role-feature-p (concept-role concept)))
                            ;; ignore redundant at-most concepts for features
                            (push (concept-negated-concept concept) restrict-models))
                        (progn
                          (push concept exists-models)
                          (when (and (some-concept-p concept)
                                     (role-feature-p (concept-role concept)))
                            (push concept restrict-models))))
                      (when (and (not non-deterministic-p)
                                 (or (role-internal-conjunction-p (concept-role concept))
                                     (constraint-from-merging-p constraint)))
                        ;;internal conjunction roles are result of merging and
                        ;;corrsponding exists constraints must be considered as non-deterministic
                        ;;the same holds for existential constraints resulting from merging
                        (setf non-deterministic-p t)))
                     ((cd-concept-p concept)
                      (if (constraint-negated-p constraint)
                        (push (concept-negated-concept concept) ensured-attributes)
                        (push concept attributes)))
                     ((or (ria-initial-concept-p concept) (neg-ria-initial-concept-p concept)
                          (ria-final-concept-p concept) (neg-ria-final-concept-p concept))
                      ; ignore these constraints
                      )
                     (t (error "expected (or atomic-concept and-concept exists-concept cd-concept ~
                                ria-initial-concept neg-ria-initial-concept ~
                                ria-final-concept neg-ria-final-concept)"))))
                  (when (and root-constraint (eql ind ind-name))
                    (push constraint not-collected))))
          until (not first-loop)
          finally
          (when (or relation-constraints
                    (and relation-store (not (relation-store-is-empty-p relation-store))))
            (without-taxonomic-encoding
              (when relation-constraints
                (loop for relation-constraint in relation-constraints
                      for role = (role-inverse-internal (constraint-term relation-constraint))
                      do
                      (when (eql (constraint-ind-2 relation-constraint) individual)
                        (when (and (not non-deterministic-p)
                                   (or (constraint-or-dependencies relation-constraint)
                                       (role-internal-conjunction-p role)
                                       (constraint-from-merging-p relation-constraint)))
                          ;;internal conjunction roles are result of merging and
                          ;;corrsponding relation constraints must be considered as non-deterministic
                          ;;the same holds for relational constraints resulting from merging
                          (setf non-deterministic-p t))
                        (unless (or (role-datatype role) (role-cd-attribute role))
                          (let ((concept (encode-concept-term `(at-least 1 ,role))))
                            (push concept exists-models)
                            (when (role-feature-p role)
                              (push concept restrict-models)))))))
              (when (and relation-store (not (relation-store-is-empty-p relation-store)))
                (let ((constraints
                        (collect-relation-fillers (successor-individuals individual relation-store)
                                                  nil
                                                  relation-store
                                                  (lambda (constraint)
                                                    (eql (constraint-ind-2 constraint) individual)))))
                  (loop for relation-constraint in constraints
                        for role = (role-inverse-internal (constraint-term relation-constraint))
                        do
                        (when (and (not non-deterministic-p)
                                   (or (constraint-or-dependencies relation-constraint)
                                       (role-internal-conjunction-p role)))
                          (setf non-deterministic-p t))
                        (let ((concept (encode-concept-term `(at-least 1 ,role))))
                          (push concept exists-models)
                          (when (role-feature-p role)
                            (push concept restrict-models))))))))
          (return (make-model-info :concept satisfiable-concept
                                   :blocked-p blocked-p
                                   :individual individual
                                   :det-positive-literals det-positive-literals
                                   :det-negative-literals det-negative-literals
                                   :positive-literals 
                                   (concept-set-difference positive-literals det-positive-literals)
                                   :negative-literals 
                                   (concept-set-difference negative-literals det-negative-literals)
                                   :exists-models exists-models
                                   :restrict-models restrict-models
                                   :attributes attributes
                                   :ensured-attributes ensured-attributes
                                   :non-deterministic-p non-deterministic-p
                                   :constraints expanded-constraints
                                   :constraints-subset-elh-p subset-elh-p)))))

(defun constraint-from-merging-p (constraint)
  (loop for dependency in (constraint-dependencies constraint)
        thereis
        (and (concept-constraint-p dependency)
             (let ((concept (constraint-concept dependency)))
               (and (at-most-concept-p concept)
                    (or (> (concept-number-restriction concept) 1)
                        (not (is-top-concept-p (concept-term concept)))))))))

(defun record-model (satisfiable-concept
                       expanded-constraints
                       expanded-store
                       expanded-store-index
                       relation-constraints
                       relation-store
                       blocked-p
                       ind)
  (let ((old-model (concept-model satisfiable-concept)))
    #+:debug (assert (null old-model))
    (if (null old-model)
      (let ((model (make-model-from-tableau expanded-constraints
                                            expanded-store
                                            expanded-store-index
                                            relation-constraints
                                            relation-store
                                            satisfiable-concept
                                            blocked-p
                                            ind)))
        (setf (concept-model satisfiable-concept) model)
        (when (and *derived-models*
                   (atomic-concept-p satisfiable-concept)
                   (not (concept-primitive-p satisfiable-concept)))
          (let ((encoded-definition
                 (concept-encoded-definition satisfiable-concept)))
            (unless (or (null encoded-definition) (concept-model encoded-definition))
              (setf (concept-model encoded-definition) model))))
        model)
      old-model)))

(defvar *blocking-used*)
(defvar *concept-model-in-progress* nil)

(defun create-individual-model (ind abox constraints)
  (if (abox-el+-transformed-table abox)
      (create-el+-model ind (env-role-members (tbox-el+-environment (abox-tbox abox))))
    (make-model-from-tableau constraints nil nil nil nil nil nil ind)))

(defun create-el+-model (individual role-members-table)
  (if (member *bottom-concept* (individual-told-subsumers individual))
      *bottom-concept*
    (make-model-info
     :individual individual
     :det-positive-literals (individual-told-subsumers individual)
     :exists-models (el+-collect-some-concepts-from-role-members individual role-members-table))))

(defun create-tableau-model (concept labels)
  (declare (ignore labels))
  (if (member concept *concept-model-in-progress*)
    (throw 'cyclic-model-found (values nil t))
    (let* ((*expanded-model* nil)
           (*save-expanded-model* t)
           (*concept-model-in-progress* (cons concept *concept-model-in-progress*))
           (*use-relation-store* nil)
           (new-label (make-basic-label-info :model-in-progress concept))
           (ind-counter +ind-counter-init+)
           (state (make-basic-kernel-state :labels (list new-label)
                                           :partially-expanded-or-stack (make-backtrack-stack))))
      (incf-statistics *computed-models*)
      (with-unique-name-assumption
        (with-alc-bindings
          (multiple-value-bind (result blocking-used-p)
                               (cs-satisfiable-2 (new-initial-concept-constraint concept)
                                                 nil ; no relation-constraints
                                                 ind-counter
                                                 state
                                                 nil ;disable expanded store
                                                 )
            ;(break "~S" concept)
            (when blocking-used-p
              (incf-statistics *cyclic-computed-models*))
            (if result
              (progn
                (unless *expanded-model*
                  (error "Internal error"))
                (let ((state (precompletion-state *expanded-model*))
                      (inverse-p (dl-inverse-roles (precompletion-language *expanded-model*))))
                  (record-model concept
                                (state-expanded-constraints state)
                                (state-expanded-store state)
                                (state-expanded-store-index state)
                                (when inverse-p
                                  (state-relation-constraints state))
                                (when inverse-p
                                  (state-relation-store state))
                                blocking-used-p
                                ind-counter)))
              (setf (concept-model concept) *bottom-concept*))))))))

(defun negated-atomic-primitive-p (concept)
  (and (negated-concept-p concept)
       (let ((term (concept-term concept)))
         #+:debug (assert (atomic-concept-p term))
         (and (concept-primitive-p term)
              (null (concept-encoded-negated-definition term))))))

(defun create-elh-concept-model-p (concept)
  (and *use-elh-model-embedding*
       (subset-el+-p (concept-language concept))
       (not (dl-any-concrete-domain (concept-language concept)))
       (or (not (atomic-concept-p concept))
           (and (not (eq *bottom-concept* (concept-encoded-definition concept)))
                (not (sufficient-condition-in-nary-absorption-table-p *use-tbox* concept))
                (not (concept-self-referencing-p concept))
                (loop for subsumer in (concept-told-subsumers concept)
                      never (or (concept-self-referencing-p subsumer)
                                (concept-nary-unfold-sets subsumer)))))))

(defun create-concept-model (concept labels)
  (incf-statistics *all-computed-models*)
  (if (or *derived-models* 
          (neg-ria-initial-concept-p concept)
          (neg-ria-final-concept-p concept))
      ; always use derived models for neg-ria-initial/final-concept (role compositions) in order to stay in ELR
      (if *meta-constraint-concepts*
          (cond
           ((atomic-concept-p concept)
            (let ((definition (concept-encoded-definition concept)))
              (if (and (null (concept-encoded-negated-definition concept))
                       (concept-model *top-concept*)
                       (or (null definition) (eq definition *top-concept*)))
                  (or (setf (concept-model concept)
                            (copy-to-primitive-model *top-concept* concept))
                      (create-tableau-model concept labels))
                (create-tableau-model concept labels))))
           ((negated-concept-p concept)
            (let* ((concept-term (concept-term concept)))
              (if (or (concept-encoded-negated-definition concept-term)
                      (and (concept-encoded-definition concept-term)
                           (not (concept-primitive-p concept-term))))
                  (create-tableau-model concept labels)
                (or (setf (concept-model concept)
                          (copy-to-neg-primitive-model *top-concept* concept))
                    (create-tableau-model concept labels)))))
           (t (create-tableau-model concept labels)))
        (if (create-elh-concept-model-p concept)
            (create-elh-concept-model concept)
          (cond
           ((atomic-concept-p concept)
            (let ((definition (concept-encoded-definition concept)))
              (if (or (null definition) (eq definition *top-concept*))
                  (let ((disjoints (concept-asserted-disjoints concept)))
                    (if disjoints
                        (if (member concept disjoints)
                            (setf (concept-model concept) *bottom-concept*)
                          (setf (concept-model concept)
                                (make-model-info :concept concept
                                                 :det-positive-literals (list concept)
                                                 :det-negative-literals disjoints)))
                      (setf (concept-model concept) concept)))
                (create-tableau-model concept labels))))
           ((and-concept-p concept)
            (if (every #'negated-atomic-primitive-p (concept-term concept))
                (setf (concept-model concept)
                      (make-model-info :concept concept
                                       :det-negative-literals (mapcar #'concept-term
                                                                      (concept-term concept))))
              (create-tableau-model concept labels)))
           ((negated-concept-p concept)
            (let ((concept-term (concept-term concept)))
              (if (or (concept-encoded-negated-definition concept-term)
                      (and (concept-encoded-definition concept-term)
                           (not (concept-primitive-p concept-term))))
                  (create-tableau-model concept labels)
                (setf (concept-model concept) concept))))
           ((some-concept-p concept)
            (if (or *inverse-roles* (concept-role-domain concept) (subset-el+-p (concept-language concept)))
                (create-tableau-model concept labels)
              (setf (concept-model concept)
                    (cond
                     ((incoherent-model-p
                       (get-cached-concept-model (or (concept-role-range concept)
                                                     (concept-term concept))
                                                 labels))
                      *bottom-concept*)
                     ((role-feature-p (concept-role concept))
                      (make-model-info :concept concept
                                       :exists-models (list concept)
                                       :restrict-models (list concept)))
                     (t (make-model-info :concept concept :exists-models (list concept)))))))
           ((all-concept-p concept)
            (setf (concept-model concept)
                  (make-model-info :concept concept :restrict-models (list concept))))
           ((at-least-concept-p concept)
            (if (or *inverse-roles* (concept-role-domain concept))
                (create-tableau-model concept labels)
              (let ((role-range (concept-role-range concept)))
                (setf (concept-model concept)
                      (if (and role-range
                               (incoherent-model-p (get-cached-concept-model role-range
                                                                             labels)))
                          *bottom-concept*
                        (make-model-info :full-p t :concept concept :exists-models (list concept)))))))
           ((at-most-concept-p concept)
            (setf (concept-model concept)
                  (make-model-info :concept concept :restrict-models (list concept))))
           ((cd-concept-p concept)
            (if (concept-attribute-domain concept)
                (create-tableau-model concept labels)
              (if (eq (predicate-operator (concept-predicate concept)) 'bottom)
                  (setf (concept-model concept)
                        (make-model-info :concept concept :ensured-attributes (list concept)))
                (setf (concept-model concept)
                      (make-model-info :concept concept :attributes (list concept))))))
           ((ensure-cd-concept-p concept)
            (if (concept-attribute-domain (concept-term concept))
                (create-tableau-model concept labels)
              (setf (concept-model concept)
                    (make-model-info :concept concept :ensured-attributes (list concept)))))
           (t (create-tableau-model concept labels)))))
    (create-tableau-model concept labels)))

(defvar *elh-visited-some-concepts*)

(defun create-elh-concept-model (concept)
  (ecase (type-of concept)
    (atomic-concept
     (let ((definition (concept-encoded-definition concept)))
       (if (or (null definition) (eq definition *top-concept*))
         (setf (concept-model concept) concept)
         (let ((model (make-model-info :concept concept
                                       :blocked-p nil
                                       :det-positive-literals (concept-told-subsumers concept))))
           (setf (concept-model concept) model)
           (ecase (type-of definition)
             (and-concept
              (setf (model-exists-models model) (elh-and-concept-exists-models definition)))
             (atomic-concept
              (let ((definition-model (get-cached-concept-model definition)))
                (when (model-info-p definition-model)
                  (setf (model-exists-models model) (model-exists-models definition-model)))))
             (some-concept
              (setf (model-exists-models model) (elh-some-concept-exists-models definition))))
           model))))
    (and-concept
     (let ((model (make-model-info :concept concept
                                   :blocked-p nil
                                   :det-positive-literals (concept-told-subsumers concept)
                                   :exists-models (elh-and-concept-exists-models concept))))
       (setf (concept-model concept) model)))
    (some-concept
     (make-model-info :concept concept
                      :blocked-p nil
                      :exists-models (elh-some-concept-exists-models concept)))
    ((neg-ria-initial-concept neg-ria-final-concept)
     (make-model-info :concept concept
                      :blocked-p nil
                      :exists-models (list concept)))))

(defun elh-some-concept-exists-models (concept)
  (let ((domain (concept-role-domain concept)))
    (if (and domain
             (not (eq domain concept))
             (not (member concept *elh-visited-some-concepts*)))
      (prog2
       (push concept *elh-visited-some-concepts*)
       (let ((domain-model (get-cached-concept-model domain)))
         (if (model-info-p domain-model)
           (cons concept (model-exists-models domain-model))
           (list concept)))
       (pop *elh-visited-some-concepts*))
      (list concept))))

(defun elh-and-concept-exists-models (concept)
  (loop with term = (concept-term concept)
        with other-exists-models = nil
        with own-exists-models = nil
        for elem in term
        do
        (if (atomic-concept-p elem)
          (let ((elem-model (get-cached-concept-model elem)))
            (when (model-info-p elem-model)
              (setf other-exists-models
                    (concept-set-union (model-exists-models elem-model) other-exists-models))))
          (progn
            #+:debug (assert (some-concept-p elem))
            (let ((domain (concept-role-domain elem)))
              (if (and domain (not (member elem *elh-visited-some-concepts*)))
                (progn
                  (push elem *elh-visited-some-concepts*)
                  (let ((domain-model (get-cached-concept-model domain)))
                    (if (model-info-p domain-model)
                      (setf own-exists-models
                            (concept-set-union (cons elem (model-exists-models domain-model))
                                               own-exists-models))
                      (push elem own-exists-models)))
                  (pop *elh-visited-some-concepts*))
                (push elem own-exists-models)))))
        finally
        (return (concept-set-union own-exists-models other-exists-models))))

(defun copy-or-make-simple-model (atomic-concept)
  (let ((encoded-definition (concept-encoded-definition atomic-concept)))
    (if (concept-primitive-p atomic-concept)
      (when (and encoded-definition (not (eq encoded-definition *top-concept*)))
        (setf (concept-model atomic-concept)
              (copy-to-primitive-model encoded-definition atomic-concept)))
      (when (and encoded-definition (concept-model encoded-definition))
        (let ((disjoints (concept-asserted-disjoints atomic-concept)))
          (if disjoints
            (let ((model (concept-model encoded-definition)))
              (if (full-model-info-p model)
                (progn
                  (pushnew atomic-concept (model-det-positive-literals model))
                  (setf (model-det-negative-literals model)
                        (concept-set-union disjoints (model-det-negative-literals model)))
                  model)
                (setf (concept-model atomic-concept)
                      (make-model-info :concept atomic-concept
                                       :det-positive-literals (list atomic-concept)
                                       :det-negative-literals disjoints))))
            (setf (concept-model atomic-concept)
                  (concept-model encoded-definition))))))))

(race-inline (get-cached-model get-cached-concept-model))

(defun get-cached-model (concept)
  (or (concept-model concept)
      (when (and *derived-models* (atomic-concept-p concept)
                 (not (and *use-elh-model-embedding*
                           (subset-el+-p (concept-language concept)))))
        (copy-or-make-simple-model concept))))

(defun get-cached-concept-model (concept &optional (labels nil))
  (let ((cached-model (get-cached-model concept)))
    (if cached-model
      (progn
        (incf-statistics *model-cache-hits*)
        cached-model)
      (progn
        (incf-statistics *model-cache-misses*)
        (if (or (not *use-elh-model-embedding*) (boundp '*elh-visited-some-concepts*))
          (create-concept-model concept labels)
          (let ((*elh-visited-some-concepts* nil))
            (create-concept-model concept labels)))))))                                                     

(defun lists-disjoint-p (list-1 list-2)
  (or (null list-1) (null list-2)
      (progn
        (when (> (- (length list-1) (length list-2)) 5)
          (let ((tmp list-1))
            (setf list-1 list-2)
            (setf list-2 tmp)))
        (loop for elem-1 in list-1
              never (member elem-1 list-2)))))

(race-inline (lists-not-disjoint-p))

(defun lists-not-disjoint-p (list-1 list-2)
  (not (lists-disjoint-p list-1 list-2)))

(defun concept-not-subsumes (subsumer subsumee)
  (with-alternate-models-mergable-p
    (get-cached-concept-model (concept-negated-concept subsumer))
    (get-cached-concept-model subsumee)))

(defun model-embeddable-p (subsumer-model subsumee-model)
  (or (eq subsumer-model *top-concept*)
      (eq subsumer-model subsumee-model)
      (model-embeddable-p-1 subsumer-model subsumee-model)))

(defun model-embeddable-p-1 (subsumer-model subsumee-model)
  (cond ((atomic-concept-p subsumee-model)
         (if (atomic-concept-p subsumer-model)
             (eq subsumee-model subsumer-model)
           (let ((subsumer-concept (model-concept subsumer-model)))
             (if (and (atomic-concept-p subsumer-concept)
                      (not (concept-primitive-p subsumer-concept)))
                 (and (null (model-exists-models subsumer-model))
                      (elh-concept-set-subsetp (model-det-positive-literals subsumer-model)
                                               (list subsumee-model)))
               (when (neg-ria-final-concept-p subsumer-concept)
                 (model-embeddable-p (get-cached-concept-model (concept-term subsumer-concept))
                                     subsumee-model))))))
        ((atomic-concept-p subsumer-model)
         #+:debug (assert (concept-primitive-p subsumer-model))
         (member subsumer-model (model-det-positive-literals subsumee-model)))
        (t
         #+:debug
         (assert (not (and (full-model-info-p subsumer-model)
                           (or (model-non-deterministic-p subsumer-model)
                               (model-negative-literals subsumer-model)
                               (model-restrict-models subsumer-model)
                               (model-det-negative-literals subsumer-model)
                               (model-attributes subsumer-model)
                               (model-ensured-attributes subsumer-model)))))
         #+:debug
         (assert (not (and (full-model-info-p subsumee-model)
                           (or (model-non-deterministic-p subsumee-model)
                               (model-negative-literals subsumee-model)
                               (model-restrict-models subsumee-model)
                               (model-det-negative-literals subsumee-model)
                               (model-attributes subsumee-model)
                               (model-ensured-attributes subsumee-model)))))
         (cond ((null (model-exists-models subsumer-model))
                (elh-concept-set-subsetp (model-det-positive-literals subsumer-model)
                                         (model-det-positive-literals subsumee-model)))
               ((null (model-exists-models subsumee-model))
                nil)
               ((elh-concept-set-subsetp (model-det-positive-literals subsumer-model)
                                         (model-det-positive-literals subsumee-model))
                (loop with trans-p = (dl-transitive-roles *dl-prover-language*)
                      for exists-concept in (model-exists-models subsumer-model)
                      for complex-p = (dl-complex-role-inclusions (concept-language exists-concept))
                      for new-exists-concepts = (if (and (or trans-p complex-p) 
                                                         (some-concept-p exists-concept))
                                                    (expand-complex-some-concept exists-concept
                                                                                 complex-p
                                                                                 trans-p)
                                                  (list exists-concept))
                      always
                      (loop for exists-concept-1 in new-exists-concepts
                            thereis
                            (loop for exists-concept-2 in (model-exists-models subsumee-model)
                                  thereis
                                  (exists-models-embeddable exists-concept-1
                                                            exists-concept-2)))))))))

(defun expand-complex-some-concept (concept complex-p trans-p)
  #+:debug (assert (some-concept-p concept))
  (let ((result nil))
    (when complex-p
      (loop with some-role = (concept-role concept)
            for role in (role-ancestors-internal some-role)
            when (role-compositions role)
            do 
            (push (encode-concept-term `(i-some ,some-role ,(concept-term concept))) result)))
    (when trans-p
      (loop for role in (role-descendants-internal (concept-role concept))
            when (role-transitive-p role)
            do (push (encode-concept-term `(i-some ,role ,(concept-term concept))) result)))
    (or result (list concept))))

(defun expand-ria-initial-concept (concept)
  (loop with term = (concept-term concept)
        for role in (role-ancestors-internal (concept-role concept))
        when (role-compositions role)
        nconc (initial-complex-some-concepts role (role-compositions role) term)))

(defun expand-ria-final-concept (concept)
  (nconc (loop with term = (concept-term concept)
               for role in (role-descendants-internal (concept-role concept))
               when (role-transitive-p role)
               collect (encode-concept-term (create-elr-ria-initial-concept nil role term)))
         (loop with term = (concept-term concept)
               for role in (role-ancestors-internal (concept-role concept))
               when (role-compositions role)
               nconc (final-complex-some-concepts role (role-compositions role) term))))

(defun initial-complex-some-concepts (role compositions term)
  ;; S1 o...o Sn => R or S1 o...o Sn o R => R
  (loop for composition in compositions
        if (not (eq (first composition) role))
        collect (encode-concept-term (create-elr-ria-final-concept composition role term))
        else
        when (eq (first (last composition)) role)
        collect (encode-concept-term (create-elr-ria-initial-concept (butlast composition) role term))))

(defun final-complex-some-concepts (role compositions term)
  ;; R o S1 o...o Sn => R
  (loop for composition in compositions
        when (eq (first composition) role)
        collect (encode-concept-term (create-elr-ria-final-concept (rest composition) role term))))

(defun create-elr-ria-initial-concept (composition some-role term)
  (if (null composition)
      `(i-some ,some-role ,term)
    `(some ,(first composition) ,(create-elr-ria-initial-concept (rest composition) some-role term))))

(defun create-elr-ria-final-concept (composition some-role term)
  (if (null composition)
      `(f-some ,some-role ,term)
    `(some ,(first composition) ,(create-elr-ria-final-concept (rest composition) some-role term))))

(defun exists-models-embeddable (subsumer subsumee)
  (let ((language (concept-language subsumer)))
    (if (or (dl-transitive-roles language) (dl-complex-role-inclusions language))
        (let ((subsumer-role (concept-role subsumer)))
          (if (member subsumer-role (role-ancestors-internal (concept-role subsumee)))
              (cond ((neg-ria-initial-concept-p subsumer)
                     (model-embeddable-p (get-cached-concept-model
                                          (encode-concept-term 
                                           (create-elr-ria-final-concept nil subsumer-role (concept-term subsumer))))
                                         (get-cached-concept-model subsumee)))
                    ((neg-ria-final-concept-p subsumer)
                     (let* ((role-range (role-range-restriction (concept-role subsumer)))
                            (new-subsumee (if role-range
                                              (create-final-ria-term role-range subsumee)
                                            (concept-term subsumee))))
                       (or (model-embeddable-p (get-cached-concept-model (concept-term subsumer))
                                               (get-cached-concept-model new-subsumee))
                           (when (role-transitive-p subsumer-role)
                             (model-embeddable-p (get-cached-concept-model subsumer)
                                                 (get-cached-concept-model new-subsumee))))))
                    (t (let ((subsumer-term (concept-term subsumer)))
                         (if (neg-ria-final-concept-p subsumer-term)
                             (let ((role-range (role-range-restriction (concept-role subsumer-term))))
                               (model-embeddable-p (get-cached-concept-model (concept-term subsumer-term))
                                                   (get-cached-concept-model 
                                                    (if role-range
                                                        (create-final-ria-term role-range subsumee)
                                                      (concept-term subsumee)))))
                           (model-embeddable-p (get-cached-concept-model (concept-term subsumer))
                                               (get-cached-concept-model (concept-term subsumee)))))))
            (cond ((neg-ria-initial-concept-p subsumer)
                   (loop with subsumee-model = (get-cached-concept-model subsumee)
                         for new-subsumer in (expand-ria-initial-concept subsumer)
                         thereis (model-embeddable-p (get-cached-concept-model new-subsumer) subsumee-model)))
                  ((neg-ria-final-concept-p subsumer)
                   (loop with subsumee-model = (get-cached-concept-model subsumee)
                         for new-subsumer in (expand-ria-final-concept subsumer)
                         thereis (model-embeddable-p (get-cached-concept-model new-subsumer) subsumee-model)))
                  (t nil))))
      (when (member (concept-role subsumer) (role-ancestors-internal (concept-role subsumee)))
        (model-embeddable-p (get-cached-concept-model (concept-term subsumer))
                            (get-cached-concept-model (concept-term subsumee)))))))

(defun create-final-ria-term (role-range concept)
  (with-flatten-encodings
    (without-taxonomic-encoding
      (encode-concept-term `(and ,(concept-term concept) ,role-range)))))

(defun model-of-bucket-embeddable-p (bucket-members subsumee-model)
  #+:debug
  (assert (not (and (full-model-info-p subsumee-model)
                    (or (model-non-deterministic-p subsumee-model)
                        (model-negative-literals subsumee-model)
                        (model-restrict-models subsumee-model)
                        (model-det-negative-literals subsumee-model)
                        (model-attributes subsumee-model)
                        (model-ensured-attributes subsumee-model)))))
  (loop for subsumer in bucket-members
        thereis
        (model-embeddable-p (get-cached-concept-model subsumer) subsumee-model)))

#| ;;old definition
(defun elh-concept-set-subsetp (list1 list2)
  (or (null list1)
      (let* ((mark (when list2
                     (inc-marker-variable '*concept-set-mark*)))
             (length2 (loop with length = 0
                            for elem in list2
                            when (concept-primitive-p elem) do
                            (setf (racer-set-flag elem) mark)
                            (incf length)
                            finally (return length))))
        (loop with length1 = 0
              for elem in list1
              always 
              (or (not (concept-primitive-p elem))
                  (when list2
                    (incf length1)
                    (and (<= length1 length2) (eql (racer-set-flag elem) mark))))))))
|#

(defun elh-concept-set-subsetp (list1 list2)
  (if (null list1)
      t
    (if list2
        (let* ((mark (inc-marker-variable '*concept-set-mark*))
               (length2 (loop with length = 0
                              for elem in list2
                              when (concept-primitive-p elem) do
                              (setf (racer-set-flag elem) mark)
                              (incf length)
                              finally (return length))))
          (loop with length1 = 0
                for elem in list1
                always 
                (or (not (concept-primitive-p elem))
                    (progn
                      (incf length1)
                      (and (<= length1 length2) (eql (racer-set-flag elem) mark))))))
      (loop for elem in list1
            never (concept-primitive-p elem)))))

(defun with-alternate-models-mergable-p (neg-subsumer-model subsumee-model)
  (multiple-value-bind (mergable partial)
                       (model-pair-mergable-p neg-subsumer-model subsumee-model)
    (when (or mergable (not partial))
      (return-from with-alternate-models-mergable-p (values mergable partial)))
    (values mergable partial)))

(race-inline (is-eq-or-parent-role-p))

(defun is-eq-or-parent-role-p (parent-role role)
  (if (role-has-ancestors-p role)
    (member parent-role (role-ancestors-internal role))
    (eq parent-role role)))

(defun r-disjoint-p (exists-list restricts-list)
  (or (null exists-list) (null restricts-list)
      (if *use-tbox*
        (loop for restrict in restricts-list
              always 
              (if (some-concept-p restrict)
                (loop with restrict-role = (concept-role restrict)
                      with restrict-ancestors = (role-feature-ancestors restrict-role)
                      for exists in exists-list
                      for exists-role = (concept-role exists)
                      always (if (role-feature-p exists-role)
                               (lists-disjoint-p restrict-ancestors
                                                 (role-feature-ancestors exists-role))
                               t))
                (not (member (concept-role restrict) exists-list
                             :test #'is-eq-or-parent-role-p
                             :key #'concept-role
                             ))))
        (loop for restrict in restricts-list
              never (member (concept-role restrict) exists-list
                            :key #'concept-role)))))

(defvar *model-clash-culprits*)

(defun compute-all-terms (some-term all-term)
  (let* ((some-role (concept-role some-term))
         (all-role (concept-role all-term))
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
         (result (when transitive-ancestors
                   (loop for role in transitive-ancestors
                         collect (if (eq role all-role)
                                   all-term
                                   (encode-concept-term
                                    `(all ,role ,(concept-term all-term))))))))
    (if (and (role-transitive-p all-role)
             (member all-role (role-ancestors-internal some-role)))
      (cons all-term result)
      result)))

(defun compute-model-list (some-term-model
                              some-concept
                              inverse-role-some-concept
                              all-constraints)
  (loop with all-model-list = nil
        for constraint in all-constraints
        for constraint-term = (if (constraint-negated-p constraint)
                                (concept-negated-concept
                                 (constraint-term constraint))
                                (constraint-term constraint))
        for term = (or (and (exists-concept-p constraint-term)
                            (concept-role-range constraint-term))
                       (concept-term constraint-term))
        for all-terms = (nconc (compute-all-terms some-concept constraint-term)
                               (when inverse-role-some-concept
                                 (compute-all-terms inverse-role-some-concept
                                                    constraint-term)))
        for model = (get-cached-concept-model term)
        do
        (when (incoherent-model-p model)
          (return-from compute-model-list constraint))
        (pushnew model all-model-list)
        (when all-terms 
          (loop for all-term in all-terms do
                (pushnew (get-cached-concept-model all-term) all-model-list)))
        finally
        (when (and inverse-role-some-concept
                   (exists-concept-p inverse-role-some-concept))
          (push (make-model-info :concept inverse-role-some-concept
                                 :exists-models (list inverse-role-some-concept)
                                 :restrict-models
                                 (when (role-feature-p
                                        (concept-role inverse-role-some-concept))
                                   (list inverse-role-some-concept)))
                   all-model-list))
        (pushnew some-term-model all-model-list)
        (return all-model-list)))

(defun constraints-mergable-p (some-constraint
                               all-constraints
                               feature-related-some-constraints
                               relation-constraints
                               labels
                               &optional (inverse-role nil))
  (let* ((some-concept (constraint-term some-constraint))
         (some-term-model (get-cached-concept-model (or (concept-role-range some-concept)
                                                        (concept-term some-concept))
                                                    labels))
         (inverse-role-concept (when inverse-role
                                 (without-taxonomic-encoding
                                   (encode-concept-term `(at-least 1 ,inverse-role))))))
    (if (incoherent-model-p some-term-model)
        (return-from constraints-mergable-p
          (values
           (not (handle-clash some-constraint
                              (constraint-or-dependencies some-constraint)
                              relation-constraints))
           nil))
      (when (and (null all-constraints) (not inverse-role))
        (return-from constraints-mergable-p t)))
    (catch 'cyclic-model-found
      (let ((model-list
             (compute-model-list some-term-model
                                 some-concept
                                 inverse-role-concept
                                 all-constraints))
            (*model-clash-culprits* nil))
        (when (concept-constraint-p model-list)
          (return-from constraints-mergable-p
            (progn
              (handle-clash model-list
                            (union-dependencies
                             (constraint-or-dependencies some-constraint)
                             (constraint-or-dependencies model-list))
                            relation-constraints)
              nil)))
        (multiple-value-bind (mergable partial-p)
            (models-mergable-p model-list labels)
          (or mergable
              (if partial-p
                  (values nil t)
                (let ((culprit-constraints
                       (stable-union (collect-culprit-constraints *model-clash-culprits*
                                                                  (cons some-constraint
                                                                        all-constraints))
                                     feature-related-some-constraints)))
                  #+:debug (assert culprit-constraints)
                  (handle-clash (first culprit-constraints)
                                (constraint-or-dependencies (first culprit-constraints))
                                relation-constraints
                                (second culprit-constraints)
                                (when (second culprit-constraints)
                                  (constraint-or-dependencies (second culprit-constraints)))
                                (union-dependencies (constraint-or-dependencies some-constraint)
                                                    (union-dependency-list
                                                     (mapcar #'constraint-or-dependencies
                                                             (rest (rest culprit-constraints))))))
                  nil))))))))

(race-inline (collect-culprit-constraints contraint-concept-term-model contraint-term-model))

(defun contraint-concept-term-model (constraint)
  (let ((concept (if (constraint-negated-p constraint)
                   (concept-negated-concept (constraint-term constraint))
                   (constraint-term constraint))))
    (if (exists-concept-p concept)
      (concept-model (or (concept-role-range concept) (concept-term concept)))
      (concept-model (concept-term concept)))))

(defun contraint-term-model (constraint)
  (concept-model (if (constraint-negated-p constraint)
                   (concept-negated-concept (constraint-term constraint))
                   (constraint-term constraint))))

(defun collect-culprit-constraints (culprit-models constraint-list)
  (loop for constraint in constraint-list
        when (or (member (contraint-concept-term-model constraint) culprit-models)
                 (member (contraint-term-model constraint) culprit-models))
        collect constraint))

(defvar *models*)
(defvar *models-cache*)
(defparameter *local-model-caching* t)

(defun models-mergable-p (model-list labels)
  #+:debug (assert (not (member-if #'incoherent-model-p model-list)))
  (let ((*models* nil)
        (*models-cache* nil))
    (multiple-value-bind (literals-mergable partial-1)
        (model-literals-mergable-p model-list nil)
      (if literals-mergable
          (multiple-value-bind (attributes-mergable-p partial-2)
              (model-attributes-mergable-p model-list partial-1)
            (if attributes-mergable-p
                (cond ((flat-models-roles-mergable-p model-list partial-2)
                       (values t partial-2))
                      (*deep-model-merging*
                       (multiple-value-bind (mergable partial)
                           (deep-models-mergable-p model-list labels t partial-2)
                         (if mergable
                             (values t partial)
                           (values nil partial))))
                      (t (values nil t)))
              (values nil partial-2)))
        (values nil partial-1)))))

(defun model-pair-mergable-p (model-1 model-2)
  (unless (or (incoherent-model-p model-1) (incoherent-model-p model-2))
    (let ((*models* nil)
          (*models-cache* nil)
          (full-model-1-p (full-model-info-p model-1))
          (full-model-2-p (full-model-info-p model-2)))
      (multiple-value-bind (literals-mergable partial true-models)
          (flat-model-pair-literals-mergable-p model-1 model-2)
        (if literals-mergable
            (let ((attributes-mergable-p
                   (if (and true-models full-model-1-p full-model-2-p)
                       (multiple-value-bind (attributes-mergable no-deep-test)
                           (model-pair-attributes-mergable-p model-1 model-2)
                         (or attributes-mergable
                             (and (not no-deep-test)
                                  *deep-cd-model-merging*
                                  (model-pair-cd-states-mergable-p model-1 model-2))))
                     t)))
              (if attributes-mergable-p
                  (cond ((not true-models)
                         (values t partial))
                        ((flat-model-pair-roles-mergable-p model-1 model-2)
                         (values t partial))
                        (*deep-model-merging*
                         (multiple-value-bind (mergable partial)
                             (if (and (not (and (full-model-info-p model-1)
                                                (dl-complex-role-inclusions (model-language model-1))))
                                      (not (and (full-model-info-p model-2)
                                                (dl-complex-role-inclusions (model-language model-2)))))
                                 (deep-models-mergable-p (list model-1 model-2) nil t partial)
                               (values nil t))
                           (if mergable
                               (values t partial)
                             (values nil partial))))
                        (t (values nil t)))
                (values nil t)))
          (progn
            (if partial
                (values nil t)
              nil)))))))

(defun flat-models-roles-mergable-p (model-list partial-p)
  (loop for models on model-list
        for selected-model = (first models)
        when (model-info-p selected-model)
        do
        (loop for model in (rest models)
              when (model-info-p model) do
              (unless (flat-model-pair-roles-mergable-p selected-model model)
                (when (and (not partial-p) (boundp '*model-clash-culprits*))
                  (setf *model-clash-culprits* (list selected-model model)))
                (return-from flat-models-roles-mergable-p nil)))
        finally (return t)))

(defun model-attributes-mergable-p (model-list partial-p)
  (let* ((skip-deep-test nil)
         (flat-mergable-p
          (block outer-loop
            (loop for models on model-list
                  for selected-model = (first models)
                  when (full-model-info-p selected-model)
                  do
                  (loop for model in (rest models) do
                        (multiple-value-bind (mergable no-deep-test)
                            (model-pair-attributes-mergable-p selected-model model)
                          (when (and no-deep-test (not skip-deep-test))
                            (setf skip-deep-test t))
                          (unless mergable
                            (when (and (not partial-p)
                                       (or skip-deep-test
                                           (not *deep-cd-model-merging*))
                                       (boundp '*model-clash-culprits*))
                              (setf *model-clash-culprits* (list selected-model model)))
                            (return-from outer-loop nil))))
                  finally (return t)))))
    (if flat-mergable-p
        (values t partial-p)
      (if skip-deep-test
          (values nil partial-p)
        (values (and *deep-cd-model-merging*
                     (model-cd-states-mergable-p model-list))
                t)))))

(defun models-superset-of-nary-unfold-sets (model-list)
  ; returns 2 values: superset-p deterministic-p
  (multiple-value-bind (det-concept-set concept-set)
      (loop with concept-set = nil
            with det-concept-set = nil
            for model in model-list
            do
            (if (full-model-info-p model)
                (let ((positive-literals (model-positive-literals model)))
                  (when positive-literals
                    (setf concept-set (append positive-literals concept-set)))
                  (setf det-concept-set (append (model-det-positive-literals model) det-concept-set)))
              (when (model-info-p model)
                (setf det-concept-set (append (model-det-positive-literals model) det-concept-set))))
            finally (return (values det-concept-set concept-set)))
    (when (or det-concept-set concept-set)
      (loop with complete-concept-set = (append concept-set det-concept-set)
            for concept in complete-concept-set do
            (loop with tbox = *use-tbox*
                  for unfold-set in (concept-nary-unfold-sets concept) do
                  (when (concept-set-subsetp unfold-set complete-concept-set)
                    (return-from models-superset-of-nary-unfold-sets
                      (values t (and tbox
                                     (concept-set-disjoint-p unfold-set concept-set)
                                     (is-bottom-concept-p
                                      (gethash unfold-set (tbox-nary-absorption-table tbox))))))))))))

(defun concept-pair-member-of-nary-unfold-sets (concept-1 concept-2)
  (and (concept-member-of-nary-unfold-sets concept-1 (concept-nary-unfold-sets concept-2))
       (concept-member-of-nary-unfold-sets concept-2 (concept-nary-unfold-sets concept-1))))

(defun concept-member-of-nary-unfold-sets (concept unfold-sets)
  (loop for unfold-set in unfold-sets
        thereis (member concept unfold-set)))

(defun superset-of-nary-unfold-sets (det-positive-literals-1
                                     positive-literals-1
                                     det-positive-literals-2
                                     positive-literals-2
                                     unfold-sets-1
                                     unfold-sets-2)
  (let ((concept-set (append det-positive-literals-1 positive-literals-1 det-positive-literals-2 positive-literals-2)))
    (when concept-set
      (when unfold-sets-1
        (loop for unfold-set in unfold-sets-1
              when (concept-set-subsetp unfold-set concept-set) do
              (return-from superset-of-nary-unfold-sets
                (values t
                        (and (concept-set-disjoint-p positive-literals-1 unfold-set)
                             (concept-set-disjoint-p positive-literals-2 unfold-set))))))
      (when unfold-sets-2
        (loop for unfold-set in unfold-sets-2
              when (concept-set-subsetp unfold-set concept-set) do
              (return-from superset-of-nary-unfold-sets
                (values t
                        (and (concept-set-disjoint-p positive-literals-1 unfold-set)
                             (concept-set-disjoint-p positive-literals-2 unfold-set)))))))))

(defun flat-model-pair-literals-mergable-p (model-1 model-2)
  ;;; returns 3 values: literals-mergable, partial, true-models
  (cond ((concept-p-internal model-1)
         (if (concept-p-internal model-2)
             (if *use-nary-absorption*
                 (if (concept-clash-p model-1 model-2)
                     nil
                   (if (and (atomic-concept-p model-1) (atomic-concept-p model-2))
                       (values (not (concept-pair-member-of-nary-unfold-sets model-1 model-2))
                               t)
                     t))
               (not (concept-clash-p model-1 model-2)))
           (if (negated-concept-p model-1)
               (let ((concept (concept-term model-1)))
                 (if (member concept (model-det-positive-literals model-2))
                     nil
                   (if (and (full-model-info-p model-2)
                            (member concept (model-positive-literals model-2)))
                       (values nil t)
                     t)))
             (if (full-model-info-p model-2)
                 (if (member model-1 (model-det-negative-literals model-2))
                     nil
                   (if (member model-1 (model-negative-literals model-2))
                       (values nil t)
                     t))
               t))))
        ((concept-p-internal model-2)
         (if (negated-concept-p model-2)
             (let ((concept (concept-term model-2)))
               (if (member concept (model-det-positive-literals model-1))
                   nil
                 (if (and (full-model-info-p model-1)
                          (member concept (model-positive-literals model-1)))
                     (values nil t)
                   t)))
           (if (full-model-info-p model-1)
               (if (member model-2 (model-det-negative-literals model-1))
                   nil
                 (if (member model-2 (model-negative-literals model-1))
                     (values nil t)
                   t))
             t)))
        (t
         (let ((full-model-1-p (full-model-info-p model-1))
               (full-model-2-p (full-model-info-p model-2))
               (concept-1 (model-concept model-1))
               (concept-2 (model-concept model-2)))
           (if (not (or full-model-1-p full-model-2-p))
               (if *use-nary-absorption*
                   (if (and (atomic-concept-p concept-1) (atomic-concept-p concept-2))
                       (values (not (concept-pair-member-of-nary-unfold-sets concept-1 concept-2))
                               t)
                     t)
                 t)
             (let ((superset-p nil)
                   (deterministic-p nil))
               (cond
                ((and concept-1
                      concept-2
                      (concept-p-internal concept-1)
                      (concept-p-internal concept-2)
                      (concept-clash-p concept-1 concept-2))
                 (values nil nil t))
                ((not (and (or (not full-model-2-p)
                               (concept-set-disjoint-p (model-det-positive-literals model-1)
                                                       (model-det-negative-literals model-2)))
                           (or (not full-model-1-p)
                               (concept-set-disjoint-p (model-det-positive-literals model-2)
                                                       (model-det-negative-literals model-1)))))
                 (values nil nil t))
                ((not (and (or (not full-model-2-p)
                               (concept-set-disjoint-p (model-det-positive-literals model-1)
                                                       (model-negative-literals model-2)))
                           (or (not full-model-1-p)
                               (concept-set-disjoint-p (model-det-positive-literals model-2)
                                                       (model-negative-literals model-1)))))
                 (values nil (or full-model-1-p full-model-2-p) t))
                ((not (or (not (and full-model-1-p full-model-2-p))
                          (and (concept-set-disjoint-p (model-positive-literals model-1)
                                                       (model-det-negative-literals model-2))
                               (concept-set-disjoint-p (model-positive-literals model-2)
                                                       (model-det-negative-literals model-1))
                               (concept-set-disjoint-p (model-positive-literals model-1)
                                                       (model-negative-literals model-2))
                               (concept-set-disjoint-p (model-positive-literals model-2)
                                                       (model-negative-literals model-1)))))
                 (values nil (and full-model-1-p full-model-2-p) t))
                ((when *use-nary-absorption*
                   (multiple-value-setq (superset-p deterministic-p)
                       (models-superset-of-nary-unfold-sets (list model-1 model-2)))
                   superset-p)
                 (values nil (not deterministic-p) t))
                (t (values t
                           (or (and full-model-1-p (model-non-deterministic-p model-1))
                               (and full-model-2-p (model-non-deterministic-p model-2)))
                           t)))))))))

(defun flat-model-pair-roles-mergable-p (model-1 model-2)
  (and (or (not (full-model-info-p model-2))
           (and (not (dl-complex-role-inclusions (model-language model-2)))
                (r-disjoint-p (model-exists-models model-1) (model-restrict-models model-2))))
       (or (not (full-model-info-p model-1))
           (and (not (dl-complex-role-inclusions (model-language model-1)))
                (r-disjoint-p (model-exists-models model-2) (model-restrict-models model-1))))))

(defun model-pair-cd-state-satisfiable (model-1 model-2)
  (if (and (full-model-info-p model-1) (full-model-info-p model-2))
    (multiple-value-bind (satisfiable cd-state)
                         (add-model-predicates (model-attributes model-1)
                                               (model-ensured-attributes model-2))
      (when satisfiable
        (multiple-value-bind (satisfiable cd-state)
                             (add-model-predicates (model-attributes model-2)
                                                   (model-ensured-attributes model-1)
                                                   cd-state)
          (when satisfiable
            (multiple-value-bind
              (satisfiable cd-state)
              (add-applicable-ensure-cd-concepts (model-ensured-attributes model-1)
                                                 (model-attributes model-2)
                                                 cd-state)
              (when satisfiable
                (add-applicable-ensure-cd-concepts (model-ensured-attributes model-2)
                                                   (model-attributes model-1)
                                                   cd-state)))))))
    t))

(defun add-applicable-ensure-cd-concepts (ensure-cd-concepts cd-concepts cd-state)
  (if ensure-cd-concepts
    (loop with attributes = (loop with result = (and cd-state
                                                     (solver-state-free-vars cd-state))
                                  for concept in cd-concepts
                                  for predicate = (concept-predicate concept) do
                                  (when (eq (predicate-operator predicate) 'top)
                                    (pushnew (first (predicate-parameters predicate))
                                             result))
                                  finally (return result))
          for ensure-cd-concept in ensure-cd-concepts
          for cd-concept = (if (ensure-cd-concept-p ensure-cd-concept)
                             (concept-term ensure-cd-concept)
                             ensure-cd-concept)
          for predicate = (concept-predicate cd-concept) do
          (when (or (nonlinear-predicate-p predicate)
                    (string-predicate-p predicate))+
                (return nil))
          (when (and (linear-predicate-p predicate)
                     (subsetp (predicate-parameters predicate) attributes))
            (multiple-value-bind (satisfiable new-cd-state)
                                 (add-model-predicates (list cd-concept)
                                                       nil
                                                       cd-state)
              (if satisfiable
                (setf cd-state new-cd-state)
                (return nil))))
          finally (return (values t cd-state)))
    (values t cd-state)))
                        
(defun concept-predicate-parameters-1 (cd-concept)
  (copy-list (concept-predicate-parameters-2 cd-concept)))

(defun concept-predicate-parameters-2 (cd-concept)
  (if (cd-concept-p cd-concept)
    (predicate-parameters (concept-predicate cd-concept))
    (predicate-parameters (concept-predicate (concept-term cd-concept)))))

(defun concept-predicate-operator (cd-concept)
  (if (cd-concept-p cd-concept)
    (predicate-operator (concept-predicate cd-concept))
    (predicate-operator (concept-predicate (concept-term cd-concept)))))

(defun add-model-predicates (cd-concepts ensure-cd-concepts &optional (state nil))
  (loop for cd-concept in cd-concepts
        for pred = (concept-predicate cd-concept)
        for cd-constraint = (make-constraint pred (predicate-parameters pred))
        do
        (multiple-value-bind (satisfiable new-state)
                             (constraint-satisfiable-p cd-constraint state)
          (when (stringp new-state)
            (error new-state))
          (if satisfiable
            (progn
              (when new-state
                (push cd-constraint (solver-state-cd-constraints new-state)))
              (setf state new-state))
            (return nil)))
        (loop with parameters = (predicate-parameters pred)
              for ensure-cd-concept in ensure-cd-concepts do
              (when (and (eq (concept-predicate-operator ensure-cd-concept) 'bottom)
                         (member (first (concept-predicate-parameters-2 ensure-cd-concept))
                                 parameters))
                ;(break "~S:~S" cd-concept ensure-cd-concepts)
                (return-from add-model-predicates nil)))
        finally (return (values t state))))

(defun safe-datatype-role-p (parameter-name)
  (let ((role (get-role parameter-name)))
    (and role (role-datatype role))))

(defun some-datatype-concept-p (cd-concepts)
  (loop for cd-concept in cd-concepts
        for predicate = (concept-predicate cd-concept)
        thereis (and (or (and (unary-string-predicate-p predicate)
                              (eq (predicate-operator predicate) 'string=))
                         (equal-predicate-p predicate))
                     (some #'safe-datatype-role-p (predicate-parameters predicate)))))

(defun model-pair-attributes-mergable-p (model-1 model-2)
  (if (and (full-model-info-p model-1) (full-model-info-p model-2))
    (if (and (some-datatype-concept-p (model-attributes model-1))
             (some-datatype-concept-p (model-attributes model-2)))
      (values nil t)
      (let ((model-1-params (mapcan #'concept-predicate-parameters-1
                                    (model-attributes model-1)))
            (model-2-params (mapcan #'concept-predicate-parameters-1
                                    (model-attributes model-2))))
        (and
         (lists-disjoint-p model-1-params model-2-params)
         (lists-disjoint-p model-1-params
                           (mapcan #'concept-predicate-parameters-1
                                   (model-ensured-attributes model-2)))
         (lists-disjoint-p model-2-params
                           (mapcan #'concept-predicate-parameters-1
                                   (model-ensured-attributes model-1))))))
    t))

(defun model-pair-cd-states-mergable-p (model-1 model-2)
  (if (and (full-model-info-p model-1) (full-model-info-p model-2))
    (if (model-pair-cd-state-satisfiable model-1 model-2)
      (progn
        (incf-statistics *cd-model-merging-satisfiable*)
        t)
      (progn
        (incf-statistics *cd-model-merging-unsatisfiable*)
        nil))
    t))

(defun model-cd-states-mergable-p (models)
  (let ((all-ensured-attributes (reduce #'union models
                                        :key #'safe-model-ensured-attributes
                                        :initial-value nil))
        (all-attributes (reduce #'union models
                                :key #'safe-model-attributes
                                :initial-value nil)))
    (multiple-value-bind (satisfiable cd-state)
                         (add-model-predicates all-attributes all-ensured-attributes)
      (let ((result (and satisfiable
                         (add-applicable-ensure-cd-concepts all-ensured-attributes
                                                            all-attributes
                                                            cd-state))))
        (if result
          (progn
            (incf-statistics *cd-model-merging-satisfiable*)
            t)
          (progn
            (incf-statistics *cd-model-merging-unsatisfiable*)
            nil))))))

(defun safe-model-ensured-attributes (model)
  (when (full-model-info-p model)
    (model-ensured-attributes model)))

(defun safe-model-attributes (model)
  (when (full-model-info-p model)
    (model-attributes model)))

(defun deep-models-mergable-p (model-list
                                   labels 
                                   &optional
                                   (literals-mergable nil)
                                   (partial-p nil))
  (when *local-model-caching*
    (let* ((cached-result (assoc model-list *models-cache* :test #'subsetp))
           (mergable (and cached-result (cadr cached-result))))
      (when cached-result
        (if mergable
          (return-from deep-models-mergable-p (values t (cddr cached-result)))
          (when (equal (car cached-result) model-list)
            (return-from deep-models-mergable-p
              (values nil (cddr cached-result))))))))
  (catch 'cyclic-model-found
    (multiple-value-bind (mergable partial)
                         (deep-models-mergable-p-1 model-list
                                                   literals-mergable
                                                   partial-p
                                                   labels)
      (push (cons model-list (cons mergable partial)) *models-cache*)
      (values mergable partial))))

(defun deep-models-mergable-p-1 (model-list literals-mergable partial-p labels)
  (if (member model-list *models* :test #'subsetp)
    (progn
      (race-trace ("~%Blocking models ~S.~%" model-list))
      t
      )
    (let ((*models* (cons model-list *models*)))
      (multiple-value-bind (mergable-1 partial-1)
                           (if literals-mergable
                             (values t partial-p)
                             (model-literals-mergable-p model-list))
        (if mergable-1
          (let ((attributes-mergable-p
                 (if (dl-any-concrete-domain *dl-prover-language*)
                   (model-attributes-mergable-p model-list partial-p)
                   t)))
            (if attributes-mergable-p
              (multiple-value-bind (mergable-2 partial-2)
                                   (submodels-mergable-p model-list labels)
                (values mergable-2 (or partial-1 partial-2)))
              (progn
                (values nil t))))
          (values nil partial-1))))))

(defun model-literals-mergable-p (model-list &optional (deep-p t))
  (loop for models on model-list
        for selected-model = (first models)
        for selected-model-p = (full-model-info-p selected-model)
        for partial-p = (and selected-model-p
                             (model-non-deterministic-p selected-model))
        then (or partial-p (and selected-model-p
                                (model-non-deterministic-p selected-model)))
        do
        (loop for model in (rest models) do
              (multiple-value-bind (mergable partial)
                  (flat-model-pair-literals-mergable-p selected-model
                                                       model)
                (setf partial-p (or partial-p partial))
                (unless mergable
                  (when (and (not partial-p) deep-p)
                    (setf partial-p
                          (loop for model-1 in (rest (member model (rest models)))
                                thereis (and (full-model-info-p model-1)
                                             (model-non-deterministic-p model-1)))))
                  (when (and (not partial-p) (boundp '*model-clash-culprits*))
                    (setf *model-clash-culprits* (list selected-model model)))
                  ;(break)
                  (return-from model-literals-mergable-p (values nil partial-p)))))
        finally
        (if (models-superset-of-nary-unfold-sets model-list)
            (return (values nil t))
          (return (values t partial-p)))))

(defun get-ind-upper-bounds-from-models (role selected-model model-list)
  (loop with role-ancestors = (and (role-has-ancestors-p role)
                                   (role-ancestors-internal role))
        with at-most-concepts = nil
        for model in model-list
        for restrict-models = (when (full-model-info-p model)
                                (model-restrict-models model))
        unless (eq model selected-model) do
        (loop for concept in restrict-models
              when (and (at-most-concept-p concept)
                        (if role-ancestors
                          (member (concept-role concept) role-ancestors)
                          (eq role (concept-role concept))))
              do (pushnew concept at-most-concepts))
        finally (return (reverse at-most-concepts))))

(defun get-exists-counts-from-concepts (role element-list &key (qualification nil) (key nil))
  (let ((at-least-count 0)
        (hierarchies-used nil)
        (some-count 0)
        (related-exists nil)
        (negated-qualification (and qualification (concept-negated-concept qualification))))
    (loop for element in element-list
          for concept = (if key
                          (funcall key element)
                          element)
          do
          (when (exists-concept-p concept)
            (let* ((concept-role (concept-role concept))
                   (role-ancestors (and (role-has-ancestors-p concept-role)
                                        (role-ancestors-internal concept-role))))
              (when (if role-ancestors
                      (member role role-ancestors)
                      (eq role concept-role))
                (setf hierarchies-used (or hierarchies-used role-ancestors))
                (cond
                 ((and (or (at-least-concept-p concept)
                           (and (some-concept-p concept)
                                (eq (concept-term concept) *top-concept*)))
                       (or (null negated-qualification)
                           (not (concept-found-p (concept-term concept)
                                                 negated-qualification))))
                  (push concept related-exists)
                  (if role-ancestors
                    (setf at-least-count
                          (+ at-least-count
                             (concept-number-restriction concept)))
                    (setf at-least-count
                          (max at-least-count
                               (concept-number-restriction concept)))))
                 ((some-concept-p concept)
                  (when (or (null negated-qualification)
                            (not (concept-found-p negated-qualification
                                                  (concept-term concept))))
                    (push concept related-exists)
                    (incf some-count))))))))
    (values (max at-least-count
                 (if (zerop some-count)
                   0
                   1))
            (if hierarchies-used
              (+ at-least-count some-count)
              (max at-least-count some-count))
            hierarchies-used
            related-exists)))

(defun concept-found-p (concept where)
  (if (and-concept-p where)
    (member concept (concept-term where))
    (eq concept where)))

(defun get-exists-counts-from-models (role model-list)
  "returns 3 values: minimal number of inds, number of inds, hierarchies-used-p"
  (let ((all-exists-models 
         (loop for model in model-list
               for exists-models = (when (model-info-p model)
                                     (model-exists-models model))
               append exists-models)))
    (when all-exists-models
      (get-exists-counts-from-concepts role all-exists-models))))

(defun get-min-max-exists-counts-from-models (role selected-model model-list)
  "returns 4 values: 
minimal number of inds, actual number of inds, applicable at-most constraints, 
hierarchies-used-p"
  (multiple-value-bind (min-inds ind-count hierarchies-used-p)
                       (get-exists-counts-from-models role model-list)
    (values min-inds ind-count
            (get-ind-upper-bounds-from-models role selected-model model-list)
            hierarchies-used-p)))

(defun get-upper-bound-from-models (at-most-concepts)
  (when at-most-concepts
    (concept-number-restriction
     (find-min-element at-most-concepts #'concept-number-restriction))))

(defun at-most-concept-violated-p (exists-concept selected-model model-list)
  (loop for role in (role-ancestors-internal (concept-role exists-concept))
        do
        (multiple-value-bind
          (min-inds ind-count at-most-concepts hierarchies-used-p)
          (get-min-max-exists-counts-from-models role selected-model model-list)
          (let ((max-inds (get-upper-bound-from-models at-most-concepts)))
            (when (and max-inds
                       (or (and (<= min-inds max-inds) (> ind-count max-inds))
                           (and hierarchies-used-p (> min-inds max-inds))
                           (and (at-least-concept-p exists-concept)
                                (not (eq (concept-term exists-concept) *top-concept*)))
                           (not (member *top-concept* at-most-concepts
                                        :key #'concept-term))))
              ;(break)
              (return t))))))

(defun submodels-mergable-p (model-list labels)
  (loop with processed-feature-exists = nil
        for selected-model in model-list
        for selected-model-p = (model-info-p selected-model)
        for full-selected-model-p = (full-model-info-p selected-model-p)
        for selected-exists = (and selected-model-p
                                   (model-exists-models selected-model))
        for partial-p = (and selected-model-p
                             full-selected-model-p
                             (model-non-deterministic-p selected-model))
        then (or partial-p (and selected-model-p
                                full-selected-model-p
                                (model-non-deterministic-p selected-model)))
        do
        (when selected-exists
          (loop for exists-concept in selected-exists
                for role = (concept-role exists-concept)
                for exists-term-model = (get-cached-concept-model
                                         (or (concept-role-range exists-concept)
                                             (concept-term exists-concept))
                                         labels)
                when (and (dl-merging *dl-prover-language*)
                          (exists-concept-p exists-concept)
                          (at-most-concept-violated-p exists-concept
                                                      selected-model
                                                      model-list))
                do (return-from submodels-mergable-p (values nil t))
                when (or (null processed-feature-exists)
                         (not (and (role-feature-p role)
                                   (member exists-concept
                                           processed-feature-exists))))
                do
                (setf partial-p
                      (or partial-p
                          (and (full-model-info-p exists-term-model)
                               (model-non-deterministic-p exists-term-model))))
                (when (incoherent-model-p exists-term-model)
                  (when (and (not partial-p) (boundp '*model-clash-culprits*))
                    (setf *model-clash-culprits*
                          (list selected-model selected-model)))
                  (return-from submodels-mergable-p (values nil partial-p)))
                (multiple-value-bind (collected-models
                                      culprit-model
                                      feature-exists)
                                     (collect-models exists-concept
                                                     role
                                                     exists-term-model
                                                     selected-model
                                                     model-list
                                                     labels)
                  (when culprit-model
                    (setf partial-p
                          (or partial-p
                              (and (full-model-info-p culprit-model)
                                   (model-non-deterministic-p culprit-model))))
                    (when (and (not partial-p) (boundp '*model-clash-culprits*))
                      (setf *model-clash-culprits* (list selected-model culprit-model)))
                    (return-from submodels-mergable-p (values nil partial-p)))
                  (when collected-models
                    (multiple-value-bind (mergable partial)
                                         (deep-models-mergable-p 
                                          collected-models
                                          labels)
                      (setf partial-p (or partial-p partial))
                      (unless mergable
                        (let ((result
                               (and *tableaux-caching* 
                                    (get-model (construct-label-from-models collected-models)))))
                          (if (and result (not (incoherent-model-p result)))
                            t
                            (progn
                              (when (and (not partial-p) (boundp '*model-clash-culprits*))
                                (update-model-clash-culprits *model-clash-culprits* role
                                                             selected-model model-list))
                              (return-from submodels-mergable-p (values nil partial-p))))))))
                  (when (role-feature-p role)
                    (pushnew exists-concept processed-feature-exists))
                  (setf processed-feature-exists
                        (union feature-exists processed-feature-exists)))))
        finally (return (values t partial-p))))

(race-inline (concept-term-model))

(defun concept-term-model (concept)
  (concept-model (concept-term concept)))

(defun find-culprit (culprit role model-list)
  (loop for model in model-list do
        (when (full-model-info-p model)
          (when (or (and (model-restrict-models model)
                         (or (member culprit (model-restrict-models model)
                                     :key #'concept-term-model)
                             (member culprit (model-restrict-models model)
                                     :key #'concept-model)
                             (member culprit model-list)))
                    (and (role-feature-p role)
                         (model-exists-models model)
                         (or (member culprit (model-exists-models model)
                                     :key #'concept-term-model)
                             (member culprit (model-exists-models model)
                                     :key #'concept-model)
                             (member culprit model-list))))
            (return model)))))

(race-inline (update-model-clash-culprits))

(defun update-model-clash-culprits (culprits role model model-list)
  (if (rest culprits)
    (let ((found-culprits (loop for culprit in culprits
                                for model = (find-culprit culprit role model-list)
                                when model collect model)))
      (when (boundp '*model-clash-culprits*)
        (setf *model-clash-culprits* (cons model (or found-culprits culprits)))))
    (when (boundp '*model-clash-culprits*)
      (setf *model-clash-culprits* (list model)))))

(defun collect-models (exists-concept
                         role
                         exists-term-model
                         excluded-model
                         model-list
                         labels)
  "Returns 3 values: collected-models culprit-model feature-exists"
  (loop with all-collected-models = nil
        with all-feature-exists = nil
        for model in model-list
        for model-concept = (if (model-info-p model)
                              (model-concept model)
                              model)
        do
        (unless (or (eq model excluded-model)
                    (not (full-model-info-p model))
                    (null (model-restrict-models model)))
          (multiple-value-bind (collected-models
                                incoherent-concept-p
                                feature-exists)
                               (role-related-models exists-concept role model labels)
            (when incoherent-concept-p
              (return-from collect-models (values nil model)))
            (when collected-models
              (setf all-collected-models
                    (stable-union collected-models all-collected-models))
              (setf all-feature-exists (concept-set-union feature-exists
                                                          all-feature-exists))
              (when (all-concept-p model-concept)
                (let ((all-terms (compute-all-terms exists-concept model-concept)))
                  (when all-terms
                    (loop for all-term in all-terms do
                          (pushnew (get-cached-concept-model all-term labels)
                                   all-collected-models))))))))
        finally
        (if all-collected-models
          (if (and (full-model-info-p excluded-model)
                   (model-restrict-models excluded-model))
            (multiple-value-bind (collected-models
                                  incoherent-concept-p
                                  feature-exists)
                                 (role-related-models exists-concept
                                                      role
                                                      excluded-model
                                                      labels)
              (when incoherent-concept-p
                (return-from collect-models (values nil excluded-model)))
              (pushnew exists-term-model collected-models)
              (return (values
                       (stable-union collected-models all-collected-models)
                       nil
                       (concept-set-union feature-exists all-feature-exists))))
            (progn
              (pushnew exists-term-model all-collected-models)
              (return (values all-collected-models nil all-feature-exists))))
          (return nil))))

(defun role-related-models (exists-concept role model labels)
  "Returns 3 values: collected-models incoherent-concept-p feature-exists"
  (let ((collected-models nil)
        (collected-feature-exists nil)
        (role-ancestors (and (role-has-ancestors-p role) (role-ancestors-internal role))))
    (if (role-feature-p role)
      (loop with feature-ancestors = (role-feature-ancestors role)
            for concept in (model-restrict-models model)
            for concept-role = (concept-role concept)
            do
            (if (at-most-concept-p concept)
              (when (concept-clash-p exists-concept concept)
                (return-from role-related-models (values nil t)))
              (when (if role-ancestors
                      (if (all-concept-p concept)
                        (member concept-role role-ancestors)
                        (if feature-ancestors
                          (not (lists-disjoint-p feature-ancestors
                                                 (role-feature-ancestors concept-role)))
                          (eq role concept-role)))
                      (eq role concept-role))
                (if (concept-clash-p exists-concept concept)
                  (return-from role-related-models (values nil t))
                  (let ((model (get-cached-concept-model
                                (or (when (some-concept-p concept)
                                      (concept-role-range concept))
                                    (concept-term concept))
                                labels)))
                    (when (incoherent-model-p model)
                      (return-from role-related-models (values nil t)))
                    (pushnew model collected-models)
                    (when (all-concept-p concept)
                      (let ((all-terms (compute-all-terms exists-concept concept)))
                        (when all-terms
                          (loop for all-term in all-terms do
                                (pushnew (get-cached-concept-model all-term labels)
                                         collected-models)))))))
                (when (and (null feature-ancestors) (some-concept-p concept))
                  (pushnew concept collected-feature-exists)))))
      (loop for concept in (model-restrict-models model)
            for concept-role = (concept-role concept) do
            (if (at-most-concept-p concept)
              (when (concept-clash-p exists-concept concept)
                (return-from role-related-models (values nil t)))
              (when (and (all-concept-p concept)
                         (if role-ancestors
                           (member concept-role role-ancestors)
                           (eq role concept-role)))
                (if (concept-clash-p exists-concept concept)
                  (return-from role-related-models (values nil t))
                  (let ((model (get-cached-concept-model (concept-term concept) labels)))
                    (when (incoherent-model-p model)
                      (return-from role-related-models (values nil t)))
                    (pushnew model collected-models)
                    (when (all-concept-p concept)
                      (let ((all-terms (compute-all-terms exists-concept concept)))
                        (when all-terms
                          (loop for all-term in all-terms do
                                (pushnew (get-cached-concept-model all-term labels)
                                         collected-models)))))))))))
    (values collected-models nil collected-feature-exists)))
