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
;;; Model generation
;;;===========================================================================

(defmacro with-tbox-environment ((tbox) &body body)
  (let ((tbox-var (gensym)))
    `(let ((,tbox-var ,tbox))
       (let ((*meta-constraint-concepts*
              (tbox-meta-constraint-concepts ,tbox-var)))
         (with-alc-environment (
                                :tbox ,tbox
                                :id-variable (tbox-structure-id-counter ,tbox-var)
                                :concept-store (tbox-concept-store ,tbox-var)
                                :tableaux-cache (tbox-tableaux-cache ,tbox-var)
                                :tableaux-sat-cache (tbox-tableaux-sat-cache ,tbox-var)
                                :tableaux-unsat-cache (tbox-tableaux-unsat-cache ,tbox-var)
                                :role-store (tbox-role-store ,tbox-var)
                                :concrete-domain (tbox-concrete-domain ,tbox-var)
                                :stable-set-difference-table (tbox-stable-set-difference-table ,tbox-var)
                                :stable-set-difference-last-list2 (tbox-stable-set-difference-last-list2 ,tbox-var)
                                :racer-remove-duplicates-table (tbox-racer-remove-duplicates-table ,tbox-var)
                                :racer-remove-constraint-duplicates-table (tbox-racer-remove-constraint-duplicates-table ,tbox-var)
                                :possible-subsumees-vector (tbox-possible-subsumees-vector ,tbox-var)
                                :expanded-constraints-ind-table (tbox-expanded-constraints-ind-table ,tbox-var)
                                :live-inds-table (tbox-live-inds-table ,tbox-var)
                                :obsolete-inds-table (tbox-obsolete-inds-table ,tbox-var)
                                :label-inds-table (tbox-label-inds-table ,tbox-var)
                                :new-inds-table (tbox-new-inds-table ,tbox-var)
                                :concept-set-mark (tbox-concept-set-mark ,tbox-var)
                                :role-set-mark (tbox-role-set-mark ,tbox-var)
                                :individual-set-mark (tbox-individual-set-mark ,tbox-var)
                                :constraint-set-mark (tbox-constraint-set-mark ,tbox-var)
                                :classification-counter (tbox-classification-counter ,tbox-var)
                                :obsolete-eql-tables (tbox-obsolete-eql-tables ,tbox-var)
                                :obsolete-equal-tables (tbox-obsolete-equal-tables ,tbox-var)
                                :obsolete-equal-concept-tables (tbox-obsolete-equal-concept-tables ,tbox-var)
                                :initial-hash-table-size (tbox-initial-hash-table-size ,tbox-var)
                                :signatures-equal-table (tbox-signatures-equal-table ,tbox-var)
                                :partitions-table (tbox-partitions-table ,tbox-var)
                                :use-less-tbox-memory (tbox-use-less-memory ,tbox-var)
                                :set-vector (tbox-set-vector ,tbox-var)
                                :select-disjunct-table (tbox-select-disjunct-table ,tbox-var)
                                )
           (with-new-used-by-concept-store
             (with-alc-bindings
               ,@body)))))))

(defmethod print-abox-assertions ((concept-name symbol) (tbox-name symbol)
                                    &optional (stream t))
  (format stream "~&(")
  (loop for assertion in (create-abox-assertions concept-name (find-tbox tbox-name))
        do (format stream "~S~%" assertion))
  (format stream ")~%"))

(defmethod create-abox-assertions ((concept-name symbol) (tbox-name symbol))
  (create-abox-assertions concept-name (find-tbox tbox-name)))

(defmethod create-abox-assertions ((concept-name symbol) (tbox tbox))
  (create-assertions-from-concept (get-tbox-concept tbox concept-name)
                                  :ind-name concept-name
                                  :tbox tbox))

(defun create-assertions-from-concept (concept &key
                                                    (ind-name nil)
                                                    (tbox *current-tbox*))
  (with-tbox-environment (tbox)
    `(progn
       . ,(create-model-individual-assertions (get-cached-concept-model concept)
                                              (or ind-name
                                                  (gentemp (gen-name concept)))))))
  
(defun create-model-individual-assertions (model ind-name)
  (let ((model (subtract-top-model model))
        (concept (model-concept model)))
    ;(break "~S" concept)
    (if (model-blocked-p model)
      (unless (eq concept *top-concept*)
        `((instance ,ind-name ,concept)))
      (nconc (gen-ind-and-assertion ind-name
                                    (stable-set-difference
                                     (model-positive-literals model)
                                     nil ;(concept-told-subsumers concept)
                                     ))
             #|(gen-ind-neg-and-assertion ind-name
                                        (stable-set-difference
                                         (model-negative-literals model)
                                         (concept-told-disjoints concept)
                                         ))|#
             (gen-ind-and-assertion ind-name
                                    (remove-if #'some-concept-p
                                               (model-restrict-models model)))
             (create-exists-models-assertions ind-name
                                              model
                                              (model-exists-models model))))))

(defun create-exists-models-assertions (ind-name model exists-list)
  (let ((feature-exists (remove-if-not #'role-feature-p exists-list
                                       :key #'concept-role)))
    (nconc (create-feature-models-assertions ind-name model feature-exists)
           (loop for exists in (stable-set-difference exists-list feature-exists)
                 for role = (concept-role exists)
                 for related-alls =
                 (get-related-all-or-some-concepts (list exists)
                                                   (model-restrict-models model))
                 for label = (construct-concept-label exists related-alls)
                 for cached-model = (or (get-model label)
                                        (get-cached-concept-model
                                         (encode-concept-term
                                          (cons 'and label))))
                 for submodel = (and cached-model
                                     (if (model-info-p cached-model)
                                       cached-model
                                       (get-cached-concept-model
                                        (concept-term exists))))
                 for name = (and submodel (gentemp (gen-name label)))
                 ;do (break "~S ~S" ind-name submodel)
                 ;do (when (eql ind-name 8) (break "~S" ind-name))
                 when submodel
                 append (cons `(related ,ind-name ,name ,(role-name role))
                              (create-model-individual-assertions submodel
                                                                  name))))))

(defun create-feature-models-assertions (ind-name model feature-exists-list)
  (let ((features-list nil))
    (loop for feature-exists in feature-exists-list
          for role = (concept-role feature-exists)
          for found = (assoc role features-list) do
          (if found
            (push feature-exists (cdr found))
            (push (cons role (list feature-exists)) features-list)))
    (loop for (role . feature-exists) in features-list
          for related-alls =
          (get-related-all-or-some-concepts feature-exists
                                            (model-restrict-models model))
          for label = (construct-concept-label feature-exists related-alls)
          for cached-model = (or (get-model label)
                                 (get-cached-concept-model
                                  (encode-concept-term
                                   (cons 'and label))))
          for submodel = (and cached-model
                              (if (model-info-p cached-model)
                                cached-model
                                (get-cached-concept-model
                                 (encode-concept-term
                                  (cons 'and label)))))
          for name = (and submodel (gentemp (gen-name label)))
          ;do (break "~S ~S" ind-name submodel)
          ;do (when (eql ind-name 8) (break "~S" ind-name))
          when submodel
          nconc (cons `(related ,ind-name ,name ,(role-name role))
                      (create-model-individual-assertions submodel
                                                          name)))))

(defun create-concept-term-from-model (model)
  `(and ,@(nconc (mapcar #'decode-concept (model-positive-literals model))
                 (mapcar #'(lambda (concept)
                             `(not ,(decode-concept concept)))
                         (model-negative-literals model))
                 (mapcar #'decode-concept (model-exists-models model))
                 (loop for concept in (model-restrict-models model)
                       when (all-concept-p concept)
                       collect concept))))

(defun gen-name (concept)
  (if (and (atomic-concept-p concept) (not (eq concept *top-concept*)))
    (concatenate 'string (symbol-name (first (concept-name-set concept))) "-")
    "I-"))

(defun gen-ind-and-assertion (name concept-list)
  (when concept-list
    (if (rest concept-list)
      `((instance ,name (and ,@(mapcar #'decode-concept concept-list))))
      `((instance ,name ,(decode-concept (first concept-list)))))))

(defun gen-ind-neg-and-assertion (name concept-list)
  (when concept-list
    (if (rest concept-list)
      `((instance ,name (not (or ,@(mapcar #'decode-concept concept-list)))))
      `((instance ,name (not ,(decode-concept (first concept-list))))))))

(defun subtract-top-model (model)
  (let ((top-model (concept-model *top-concept*)))
    (if (eq *top-concept* top-model)
      model
      (let ((new-model (copy-model-info model)))
        (setf (model-positive-literals new-model)
              (set-difference (model-positive-literals new-model)
                              (model-positive-literals top-model)))
        (setf (model-negative-literals new-model)
              (set-difference (model-negative-literals new-model)
                              (model-negative-literals top-model)))
        #|(setf (model-exists-models new-model)
              (set-difference (model-exists-models new-model)
                              (model-exists-models top-model)))|#
        (setf (model-restrict-models new-model)
              (set-difference (model-restrict-models new-model)
                              (model-restrict-models top-model)))
        new-model))))


(defun construct-concept-label (some-concept all-concepts)
  (loop with concepts = (if (consp some-concept)
                          (mapcar #'concept-term some-concept)
                          (list (concept-term some-concept)))
        for concept in all-concepts do
        (when (and (all-concept-p concept)
                   (role-transitive-p (concept-role concept)))
          (push concept concepts))
        (push (concept-term concept) concepts)
        finally
        (return
         (with-flatten-encodings
           (let ((term (encode-concept-term `(and ,@concepts))))
             (if (and-concept-p term)
               (concept-term term)
               (list term)))))))

(defun get-related-all-or-some-concepts (some-concepts all-concepts)
  (loop for some-concept in some-concepts
        append
        (let* ((role (concept-role some-concept))
               (role-ancestors (and (role-has-ancestors-p role)
                                    (role-ancestors-internal role))))
          (if (role-feature-p role)
            (loop with feature-ancestors = (role-feature-ancestors role)
                  for concept in all-concepts
                  for concept-role = (concept-role concept)
                  when (if role-ancestors
                         (if (all-concept-p concept)
                           (member concept-role role-ancestors)
                           (and feature-ancestors
                                (not (lists-disjoint-p feature-ancestors
                                                       (role-feature-ancestors
                                                        concept-role)))))
                         (eq concept-role role))
                  collect concept)
            (loop for concept in all-concepts
                  for concept-role = (concept-role concept)
                  when (if role-ancestors
                         (member concept-role role-ancestors)
                         (eq concept-role role))
                  collect concept)))))
