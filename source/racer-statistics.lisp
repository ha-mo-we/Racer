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
;;; Gathering statistics
;;;===========================================================================

(defvar *race-statistics-stream* nil)   ; stream to statistics file

(defvar *simplex-statistics-list*)      ; collects (var-no eqn-no iter-no) per simplex call

(defparameter *verbose* nil)            ; enables model-size infos for some-concepts

(defparameter *statistics* nil)         ; enables printing of statistics info for TBox/Abox
(defparameter *sat-statistics* nil)     ; enables printing of statistics info for SAT tests

(defparameter *statistics-batch* nil)     ; print batch of statistics data

(defparameter *statistics-integer-vars*
  '(*model-cache-hits*
    *model-cache-misses*
    *tableaux-cache-sat-hits*
    *tableaux-cache-unsat-hits*
    *tableaux-cache-misses*
    *added-tableaux-sat-cache-entries*
    *added-tableaux-unsat-cache-entries*
    *sat-cache-hits*                    ; superset SAT cache hits
    *sat-cache-real-superset-hits*      ; true superset SAT cache hits
    *added-sat-cache-entries*
    *unsat-cache-hits*                  ; subset UNSAT cache hits
    *unsat-cache-real-subset-hits*      ; true subset UNSAT cache hits
    *retracted-models*
    *retracted-tableaux-cache-entries*
    *retracted-sat-cache-entries*
    *subsumption-tests*
    *all-subsumption-tests*             ; all subsumption tests
    *subsumptions-found*                ; successful subsumption tests
    *model-subsumption-tests*
    *obvious-subsumptions*
    *bottom-search-tests*
    *deleted-buckets*
    *merged-buckets*
    *computed-models*                   ; satisfiability tests induced by model caching and model subsumption
    *all-computed-models*               ; *computed-models* + derived models
    *cyclic-computed-models*
    *mergable-models*
    *unmergable-det-models*
    *unmergable-partial-models*
    *unmergable-models-satisfiable*
    *unmergable-models-unsatisfiable*
    *cd-model-merging-satisfiable*
    *cd-model-merging-unsatisfiable*
    *ind-subsumption-tests*
    *ind-subsumptions-found*
    *ind-concept-subsumption-tests*
    *ind-concept-subsumptions-found*
    *ind-abox-completion-tests*
    *ind-abox-completion-non-subsumption*
    *ind-model-cache-hits*
    *ind-model-cache-misses*
    *ind-mergable-models*
    *ind-unmergable-det-models*
    *ind-unmergable-partial-models*
    *ind-unmergable-models-satisfiable*
    *ind-unmergable-models-unsatisfiable*
    *splits*                            ; number of tried semantic branches
    *sat-first-split*                   ; number of satisfied first semantic branches
    *sat-second-split*                  ; number of satisfied second semantic branches
    *unsat-splits*                      ; number of failed semantic branches
    *skipped-unsat-splits*              ; number of skipped semantic branches due to backjumping
    *alt-splits*                        ; number of tried alternative semantic branches
    *sat-first-alt-split*               ; number of satisfied first alternative semantic branches
    *sat-second-alt-split*              ; number of satisfied second alternative semantic branches
    *unsat-alt-splits*                  ; number of failed alternative semantic branches
    *skipped-unsat-alt-splits*          ; number of skipped alternative semantic branches due to backjumping
    *signature-splits*
    *signature-sat-first-split*
    *signature-sat-second-split*
    *signature-unsat-splits*
    *signature-skipped-unsat-splits*
    *taxonomic-encoding-hits*
    *max-or-level*
    *model-depth*                       ; actual nesting depth of some models
    *max-model-depth*
    *model-size*                        ; number of direct some-model descendents
    *max-model-size*
    *successful-cd-predicate-tests*
    *failed-cd-predicate-tests*
    *successful-cd-predicate-entailed-tests*
    *failed-cd-predicate-entailed-tests*
    *number-of-clash-tests*
    *number-of-clashes*
    *number-of-duplicates-during-clash-test*
    *number-of-new-constraints-during-clash-test*
    *number-of-failed-sh-blocking-tests*
    *number-of-failed-shi-blocking-tests*
    *number-of-failed-shiq-blocking-tests*
    *number-of-weak-failed-shi-blocking-tests*
    *number-of-weak-failed-shiq-blocking-tests*
    *number-of-successful-sh-blocking-tests*
    *number-of-successful-shi-blocking-tests*
    *number-of-successful-c-shiq-blocking-tests*        ; number of cyclic blocks used
    *number-of-successful-a-shiq-blocking-tests*        ; number of acyclic blocks used
    *number-of-successful-zero-shiq-blocking-tests*     ; number of blocks where witness is 0 ind

    *number-of-copied-hash-tables*
    *number-of-copied-hash-table-entriess*
    *size-of-copied-hash-tables*
    *number-of-merged-hash-tables*
    *number-of-merged-hash-table-entriess*
    *size-of-merged-hash-tables*

    *number-of-created-expanded-constraint-stores*
    *number-of-copied-expanded-constraint-stores*
    *number-of-merged-expanded-constraint-stores*
    *number-of-swaps-to-expanded-constraint-stores*
    *number-of-extend-swaps-to-expanded-constraint-stores*
    *number-of-swapped-expanded-constraints*

    *number-of-created-unexp-exists-constraint-stores*
    *number-of-copied-unexp-exists-constraint-stores*
    *number-of-swaps-to-unexp-exists-constraint-stores*
    *number-of-swapped-unexp-exists-constraints*
    *number-of-removals-from-unexp-exists-constraint-stores*
    *number-of-removed-unexp-exists-constraints*

    *number-of-created-unexp-disjunctive-constraint-stores*
    *number-of-copied-unexp-disjunctive-constraint-stores*
    *number-of-swaps-to-unexp-disjunctive-constraint-stores*
    *number-of-swapped-unexp-disjunctive-constraints*
    *number-of-removals-from-unexp-disjunctive-constraint-stores*
    *number-of-removed-unexp-disjunctive-constraints*

    *number-of-created-label-caches*
    *number-of-removed-label-cache-entries*
    *number-of-added-label-cache-entries*
    *label-cache-hits*
    *label-cache-misses*

    *number-of-binary-retrievals*
    *number-of-binary-retrieval-candidates*
    *number-obv-inds-in-binary-retrieval*
    *number-of-inds-to-be-tested-in-binary-retrieval*
    *number-of-binary-partition-retrievals*

    *number-of-dependency-retrievals*
    *number-of-dependency-retrieval-candidates*
    *number-obv-inds-in-dependency-retrieval*
    *number-of-inds-to-be-tested-in-dependency-retrieval*
    *number-of-dependency-partition-retrievals*
    ))

(defparameter *statistics-list-vars*
  '(*simplex-statistics-list*))

(defparameter *statistics-max-vars*
  '(*max-or-level*
    *max-model-depth*
    *max-model-size*
    *max-length-of-expanded-constraint-stores*
    *max-length-of-unexp-disjunctive-constraint-stores*
    *max-length-of-unexp-exists-constraint-stores*
    ))

(defparameter *statistics-table-size* (+ (length *statistics-integer-vars*)
                                        (length *statistics-list-vars*)))

(defvar *statistics-table*)
(defvar *statistics-table-printed*)

(defvar *total-statistics-table*)
(defvar *total-statistics-table-printed*)

(defun init-statistics-table (table)
  (loop for var in *statistics-list-vars* do
          (setf (gethash var table) nil))
  table)

(defun make-statistics-table ()
  (init-statistics-table (make-hash-table :size *statistics-table-size*)))

(defun reset-local-statistics-table ()
  (setf *statistics-table-printed* t)
  (setf *statistics-table* (make-statistics-table)))

(defun reset-statistics-tables ()
  (setf *total-statistics-table-printed* t)
  (setf *total-statistics-table* (make-statistics-table))
  (reset-local-statistics-table))

(defmacro unmark-statistics-table ()
  '(when *statistics-table-printed*
     (setf *statistics-table-printed* nil)))

(defmacro when-statistics (&body body)
  `(when (or *statistics* *sat-statistics* *verbose* *race-statistics-stream*)
     . ,body))

(defmacro when-statistics-plus (additional-condition &body body)
  `(when (or *statistics* *sat-statistics* *verbose* *race-statistics-stream*
             ,additional-condition)
     . ,body))

(defmacro if-sat-statistics (additional-condition then-form else-form)
  `(if (and *sat-statistics* ,additional-condition)
     ,then-form
     ,else-form))

(defmacro when-sat-statistics (&body body)
  `(when *sat-statistics*
     . ,body))

(defmacro when-collect-statistics (&body body)
  `(when-statistics
     . ,body))

(defmacro when-print-statistics (&body body)
  `(when-statistics
     (unless *statistics-batch*
       . ,body)))

(defmacro when-print-statistics-plus (additional-condition &body body)
  `(when-statistics-plus ,additional-condition
     (unless *statistics-batch*
       . ,body)))

(defmacro setf-statistics (place new-value)
  `(when-statistics
     (unmark-statistics-table)
     (setf (gethash ',place *statistics-table*) ,new-value)))

(defmacro incf-statistics (place &optional (delta 1))
  `(when-statistics
     (unmark-statistics-table)
     (incf (gethash ',place *statistics-table* 0) ,delta)))

(defmacro decf-statistics (place &optional (delta 1))
  `(when-statistics
     (unmark-statistics-table)
     (decf (gethash ',place *statistics-table* 0) ,delta)))

(defmacro maxf-statistics (place new-value)
  `(when-statistics
     (unmark-statistics-table)
     (setf (gethash ',place *statistics-table* 0)
           (max (gethash ',place *statistics-table* 0) ,new-value))))

(defmacro push-statistics (item place)
  `(when-statistics
     (unmark-statistics-table)
     (push ,item (gethash ',place *statistics-table*))))

(defmacro get-local-statistics-integer-value (symbol)
  `(gethash ',symbol *statistics-table* 0))

(defmacro get-total-statistics-integer-value (symbol)
  `(gethash ',symbol *total-statistics-table* 0))

(defmacro get-statistics-integer-value (symbol &key (local nil))
  `(gethash ',symbol
            (if ,local
              *statistics-table*
              *total-statistics-table*)
            0))

(defmacro get-local-statistics-list-value (symbol)
  `(gethash ',symbol *statistics-table*))

(defmacro get-total-statistics-list-value (symbol)
  `(gethash ',symbol *total-statistics-table*))

(defmacro get-statistics-list-value (symbol &key (local nil))
  `(gethash ',symbol
            (if ,local
              *statistics-table*
              *total-statistics-table*)))

(defmacro with-racer-statistics ((&key (sat-verbose nil)
                                      (sat nil)
                                      (tbox nil)
                                      (abox nil)
                                      (asat nil)
                                      (batch nil))
                                &body body)
  `(let ((*total-statistics-table* (make-statistics-table))
         (*statistics-table* (make-statistics-table))
         (*verbose* ,sat-verbose)
         (*sat-statistics* ,sat)
         (*statistics* (or ,tbox ,abox))
         (*total-statistics-table-printed* nil)
         (*statistics-table-printed* nil)
         (*statistics-batch* ,batch)
         (start (get-internal-run-time)))
     (multiple-value-prog1
       (progn
         . ,body)
       (when *current-tbox*
         (when (and *sat-statistics* *statistics-batch* (not *statistics-table-printed*))
           (print-sat-statistics t nil start (get-internal-run-time)))
         (when (and ,tbox 
                    *statistics-batch*
                    (not *total-statistics-table-printed*)
                    *current-tbox*)
           (print-tbox-statistics *current-tbox* t nil start (get-internal-run-time)))
         (when (and ,abox
                    (or *statistics-batch* ,asat)
                    (not *total-statistics-table-printed*)
                    *current-abox*)
           (print-abox-statistics *current-abox*
                                  t
                                  nil
                                  start
                                  (get-internal-run-time)
                                  (not *statistics-batch*)))))))

(defmacro with-sat-statistics (&body body)
  `(with-racer-statistics (:sat t)
     . ,body))

(defmacro with-all-sat-statistics (&body body)
  `(with-racer-statistics (:sat t :batch t)
     . ,body))

(defmacro with-tbox-statistics (&body body)
  `(with-racer-statistics (:tbox t)
     . ,body))

(defmacro with-abox-statistics (&body body)
  `(with-racer-statistics (:abox t)
     . ,body))

(defmacro with-inds-statistics (&body body)
  `(with-racer-statistics (:abox t :batch t)
     . ,body))

(defmacro with-asat-statistics (&body body)
  `(with-racer-statistics (:abox t :asat t :batch t)
     . ,body))

(defmacro with-verbose-statistics (&body body)
  `(with-racer-statistics (:sat-verbose t)
     . ,body))

(defun collect-statistics-data ()
  (when-statistics
    (setf *total-statistics-table-printed* nil)
    (loop for value being the hash-value of *statistics-table* using (hash-key var)  do
          (if (numberp value)
            (unless (zerop value)
              (if (member var *statistics-max-vars*)
                (setf (gethash var *total-statistics-table*)
                      (max (gethash var *total-statistics-table* 0) value))
                (incf (gethash var *total-statistics-table* 0) value))
              (setf (gethash var *statistics-table*) 0))
            (when (and (consp value) (member var *statistics-list-vars*))
              (setf (gethash var *total-statistics-table*)
                    (append value (gethash var *total-statistics-table*)))
              (setf (gethash var *statistics-table*) nil))))))

;;;===========================================================================

(defun print-semantic-splits (stream &key (local nil))
  (let ((total-splits (get-statistics-integer-value *splits* :local local))
        (total-sat-first-split
         (get-statistics-integer-value *sat-first-split* :local local))
        (total-sat-second-split
         (get-statistics-integer-value *sat-second-split* :local local))
        (total-skipped-unsat-splits
         (get-statistics-integer-value *skipped-unsat-splits* :local local))
        (total-unsat-splits
         (get-statistics-integer-value *unsat-splits* :local local)))
    (unless (zerop total-splits)
      (format stream "~&;Semantic splits: ~D (~D 1. sat (~,2F%), ~D 2. sat (~,2F%), ~%~
                      ;                 ~D backjump unsat (~,2F%), ~D unsat (~,2F%))~%"
              total-splits total-sat-first-split
              (safe-percentage total-sat-first-split total-splits)
              total-sat-second-split
              (safe-percentage total-sat-second-split total-splits)
              total-skipped-unsat-splits
              (safe-percentage total-skipped-unsat-splits total-splits)
              total-unsat-splits
              (safe-percentage total-unsat-splits total-splits)))))

(defun print-alt-semantic-splits (stream &key (local nil))
  (let ((total-splits (get-statistics-integer-value *alt-splits* :local local))
        (total-sat-first-split
         (get-statistics-integer-value *sat-first-alt-split* :local local))
        (total-sat-second-split
         (get-statistics-integer-value *sat-second-alt-split* :local local))
        (total-skipped-unsat-splits
         (get-statistics-integer-value *skipped-unsat-alt-splits* :local local))
        (total-unsat-splits
         (get-statistics-integer-value *unsat-alt-splits* :local local)))
    (unless (zerop total-splits)
      (format stream "~&;Alt. semantic splits: ~D (~D 1. sat (~,2F%), ~D 2. sat (~,2F%), ~%~
                      ;                      ~D backjump unsat (~,2F%), ~D unsat (~,2F%))~%"
              total-splits total-sat-first-split
              (safe-percentage total-sat-first-split total-splits)
              total-sat-second-split
              (safe-percentage total-sat-second-split total-splits)
              total-skipped-unsat-splits
              (safe-percentage total-skipped-unsat-splits total-splits)
              total-unsat-splits
              (safe-percentage total-unsat-splits total-splits)))))

(defun print-signature-splits (stream &key (local nil))
  (let ((signature-total-splits
         (get-statistics-integer-value *signature-splits* :local local))
        (signature-total-sat-first-split
         (get-statistics-integer-value *signature-sat-first-split* :local local))
        (signature-total-sat-second-split
         (get-statistics-integer-value *signature-sat-second-split* :local local))
        (signature-total-skipped-unsat-splits
         (get-statistics-integer-value *signature-unsat-splits* :local local))
        (signature-total-unsat-splits
         (get-statistics-integer-value *signature-skipped-unsat-splits* :local local)))
    (unless (zerop signature-total-splits)
      (format stream "~&;Semantic signature splits: ~D (~D 1. sat (~,2F%), ~D 2. sat (~,2F%), ~%~
                      ;                           ~D backjump unsat (~,2F%), ~D unsat (~,2F%))~%"
              signature-total-splits signature-total-sat-first-split
              (safe-percentage signature-total-sat-first-split signature-total-splits)
              signature-total-sat-second-split
              (safe-percentage signature-total-sat-second-split signature-total-splits)
              signature-total-skipped-unsat-splits
              (safe-percentage signature-total-skipped-unsat-splits signature-total-splits)
              signature-total-unsat-splits
              (safe-percentage signature-total-unsat-splits signature-total-splits)))))

(defun print-model-accesses (tbox stream data-stream &key (local nil))
  (let* ((total-model-cache-hits
          (get-statistics-integer-value *model-cache-hits* :local local))
         (total-model-cache-misses
          (get-statistics-integer-value *model-cache-misses* :local local))
         (model-cache-accesses (+ total-model-cache-hits total-model-cache-misses)))
    (when (and stream (not (zerop model-cache-accesses)))
      (format stream "~&;Model cache: ~:D hits (~,2F%), ~:D misses (~,2F%)~%"
              total-model-cache-hits
              (safe-percentage total-model-cache-hits model-cache-accesses)
              total-model-cache-misses
              (safe-percentage total-model-cache-misses model-cache-accesses))
      (multiple-value-bind (total-models non-det det concepts)
                           (loop with total-models = 0
                                 with non-det = 0
                                 with det = 0
                                 with concepts = 0
                                 for concept being the hash-value of (tbox-concept-store tbox)
                                 for model = (concept-model concept)
                                 do
                                 (when model
                                   (incf total-models)
                                   (if (concept-p-internal model)
                                     (incf concepts)
                                     (if (and (full-model-info-p model)
                                              (model-non-deterministic-p model))
                                       (incf non-det)
                                       (incf det))))
                                 finally (return (values total-models non-det det concepts)))
        (let ((true-models (- total-models concepts)))
          (format stream ";Models: total=~D, ~D concepts (~,2F%), ~D true models (~,2F%, ~
                          ~D non-deterministic (~,2F%), ~D deterministic (~,2F%))~%"
                  total-models
                  concepts
                  (safe-percentage concepts total-models)
                  true-models
                  (safe-percentage true-models total-models)
                  non-det
                  (safe-percentage non-det true-models)
                  det
                  (safe-percentage det true-models)))))
    (when data-stream
      (format data-stream "~16D~10,5F~16D~10,5F"
              total-model-cache-hits
              (safe-percentage total-model-cache-hits model-cache-accesses)
              total-model-cache-misses
              (safe-percentage total-model-cache-misses model-cache-accesses)))))

(defun print-model-merging (tbox stream data-stream &key (local nil))
  (declare (ignore tbox))
  (let* ((total-mergable-models
          (get-statistics-integer-value *mergable-models* :local local))
         (total-unmergable-det-models
          (get-statistics-integer-value *unmergable-det-models* :local local))
         (total-unmergable-partial-models
          (get-statistics-integer-value *unmergable-partial-models* :local local))
         (total-unmergable-models-satisfiable
          (get-statistics-integer-value *unmergable-models-satisfiable* :local local))
         (total-unmergable-models-unsatisfiable 
          (get-statistics-integer-value *unmergable-models-unsatisfiable* :local local))
         (merging-successes (+ total-unmergable-det-models total-mergable-models))
         (merging-tests (+ merging-successes total-unmergable-partial-models)))
    (when (and *model-merging* (> merging-tests 0))
      (when stream
        (format stream ";Model merging: (~:D mergable / ~:D det-unmergable = ~
                        ~:D hits (~,2F%) , ~:D misses (~,2F%))~%"
                total-mergable-models total-unmergable-det-models 
                merging-successes
                (safe-percentage merging-successes merging-tests)
                total-unmergable-partial-models
                (safe-percentage total-unmergable-partial-models merging-tests))
        (format stream ";               ~:D (~,2F%) unmergable models found satisfiable~%~
                        ;               ~:D (~,2F%) unmergable models found unsatisfiable~%"
                total-unmergable-models-satisfiable
                (safe-percentage total-unmergable-models-satisfiable
                                 total-unmergable-partial-models)
                total-unmergable-models-unsatisfiable
                (safe-percentage total-unmergable-models-unsatisfiable
                                 total-unmergable-partial-models))
        #|(when *use-alternate-models*
          (loop for concept being the hash-value of (tbox-concept-store tbox)
                for model = (concept-model concept)
                for alternates = (length (and (model-info-p model)
                                              (model-alternates model)))
                when model
                sum 1 into concepts-with-models
                unless (zerop alternates)
                maximize alternates into max-alternates and
                sum alternates into all-alternates and
                sum 1 into concept-models-with-alternates
                finally
                (unless (zerop all-alternates)
                  (format stream "~&;Alternate models: max-no=~D, total-no=~D, ~
                                  concept-models=~D, concept-model-alternates=~D~%"
                          max-alternates all-alternates
                          concepts-with-models concept-models-with-alternates))))|#
        (when *deep-cd-model-merging*
          (let* ((cd-model-merging-satisfiable
                  (get-statistics-integer-value *cd-model-merging-satisfiable* :local local))
                 (cd-model-merging-unsatisfiable
                  (get-statistics-integer-value *cd-model-merging-unsatisfiable* :local local))
                 (cd-model-merging-tests (+ cd-model-merging-satisfiable
                                            cd-model-merging-unsatisfiable)))
            (when (and stream (not (zerop cd-model-merging-tests)))
              (format stream "~&;Deep CD MM tests: ~:D (~:D successful (~,2F%) / ~:D unsuccessful (~,2F%))~%"
                      cd-model-merging-tests
                      cd-model-merging-satisfiable
                      (safe-percentage cd-model-merging-satisfiable cd-model-merging-tests)
                      cd-model-merging-unsatisfiable
                      (safe-percentage cd-model-merging-unsatisfiable cd-model-merging-tests)))))))
    (when data-stream
      (format data-stream "~16D~16D~16D~10,5F~16D~10,5F~16D~10,5F~16D~10,5F"
              total-mergable-models total-unmergable-det-models 
              merging-successes
              (safe-percentage merging-successes merging-tests)
              total-unmergable-partial-models
              (safe-percentage total-unmergable-partial-models merging-tests)
              total-unmergable-models-satisfiable
              (safe-percentage total-unmergable-models-satisfiable
                               total-unmergable-partial-models)
              total-unmergable-models-unsatisfiable
              (safe-percentage total-unmergable-models-unsatisfiable
                               total-unmergable-partial-models)))
    (let ((total-all-computed-models
           (get-statistics-integer-value *all-computed-models* :local local))
          (total-computed-models
           (get-statistics-integer-value *computed-models* :local local))
          (total-cyclic-computed-models
           (get-statistics-integer-value *cyclic-computed-models*))
          (total-retracted-models
           (get-statistics-integer-value *retracted-models* :local local)))
      (when (and *use-tbox* stream)
        (format stream "~&;No. of cached concept models: total=~:D, tableaux=~:D (~,2F% blocked), derived=~:D~%"
                total-all-computed-models total-computed-models
                (safe-percentage total-cyclic-computed-models total-computed-models)
                (- total-all-computed-models total-computed-models))
        (format stream "~&;No. of retracted concept models: ~D (~,2F%)~%"
                total-retracted-models
                (safe-percentage total-retracted-models total-computed-models)))
      (when data-stream
        (format data-stream "~8D~10,5F~8D~10,5F"
                total-computed-models
                (safe-percentage total-cyclic-computed-models total-computed-models)
                total-retracted-models
                (safe-percentage total-retracted-models total-computed-models))))))

(defun print-tableaux-caching (tbox stream data-stream &key (local nil))
  (let* ((total-tableaux-cache-sat-hits 
          (get-statistics-integer-value *tableaux-cache-sat-hits* :local local))
         (total-tableaux-cache-unsat-hits
          (get-statistics-integer-value *tableaux-cache-unsat-hits* :local local))
         (total-tableaux-cache-misses
          (get-statistics-integer-value *tableaux-cache-misses*))
         (tableaux-cache-hits (+ total-tableaux-cache-sat-hits
                                 total-tableaux-cache-unsat-hits))
         (tableaux-cache-accesses (+ tableaux-cache-hits total-tableaux-cache-misses))
         (added-tableau-sat-cache-entries
          (get-statistics-integer-value *added-tableaux-sat-cache-entries* :local local))
         (added-tableau-unsat-cache-entries
          (get-statistics-integer-value *added-tableaux-unsat-cache-entries* :local local))
         (all-added-entries (+ added-tableau-sat-cache-entries
                               added-tableau-unsat-cache-entries))
         (retracted-tableaux-cache-entries
          (get-statistics-integer-value *retracted-tableaux-cache-entries* :local local))
         (tableaux-cache (tbox-tableaux-cache tbox)))
    (when (and *tableaux-caching* tableaux-cache)
      (when stream
        (format stream ";Tableaux cache: entries added (~D (~,2F%) sat / ~D (~,2F%) unsat), ~
                        entries retracted ~D (~,2F%),~%~
                        ;                entries=~D, accesses (~D sat / ~D unsat = ~
                        ~D hits (~,2F%) , ~D misses (~,2F%))~%"
                added-tableau-sat-cache-entries
                (safe-percentage added-tableau-sat-cache-entries all-added-entries)
                added-tableau-unsat-cache-entries
                (safe-percentage added-tableau-unsat-cache-entries all-added-entries)
                retracted-tableaux-cache-entries
                (safe-percentage retracted-tableaux-cache-entries
                                 added-tableau-sat-cache-entries)
                (if (hash-table-p tableaux-cache)
		    (hash-table-count tableaux-cache)
		  0)
                total-tableaux-cache-sat-hits
                total-tableaux-cache-unsat-hits
                tableaux-cache-hits
                (safe-percentage tableaux-cache-hits tableaux-cache-accesses)
                total-tableaux-cache-misses
                (safe-percentage total-tableaux-cache-misses tableaux-cache-accesses))))
    (when data-stream
      (format data-stream "~16D~16D~16D~10,5F~16D~10,5F~%"
              total-tableaux-cache-sat-hits total-tableaux-cache-unsat-hits
              tableaux-cache-hits
              (safe-percentage tableaux-cache-hits tableaux-cache-accesses)
              total-tableaux-cache-misses
              (safe-percentage total-tableaux-cache-misses tableaux-cache-accesses)))
    (when (and stream *use-subset-superset-cache*)
      (print-cache-statistics :stream stream
                              :sat-cache (tbox-tableaux-sat-cache tbox)
                              :unsat-cache (tbox-tableaux-unsat-cache tbox)))))

(defun print-cd-statistics (stream &key (local nil))
  (let* ((successful-cd-predicate-tests
          (get-statistics-integer-value *successful-cd-predicate-tests* :local local))
         (failed-cd-predicate-tests
          (get-statistics-integer-value *failed-cd-predicate-tests* :local local))
         (all-cd-predicate-tests (+ successful-cd-predicate-tests failed-cd-predicate-tests))
         (successful-cd-predicate-entailed-tests
          (get-statistics-integer-value *successful-cd-predicate-entailed-tests* :local local))
         (failed-cd-predicate-entailed-tests
          (get-statistics-integer-value *failed-cd-predicate-entailed-tests* :local local))
         (all-entailment-tests (+ successful-cd-predicate-entailed-tests
                                  failed-cd-predicate-entailed-tests)))
    (unless (zerop all-cd-predicate-tests)
      (format stream "~&;CD predicate tests: ~D (~D pure (~,2F%), ~D successful (~,2F%), ~D unsuccessful (~,2F%))~%~
                      ;CD entailment tests: ~D (~D successful (~,2F%), ~D unsuccessful (~,2F%))~%"
              all-cd-predicate-tests
              (- all-cd-predicate-tests all-entailment-tests)
              (safe-percentage (- all-cd-predicate-tests all-entailment-tests)
                               all-cd-predicate-tests)
              successful-cd-predicate-tests
              (safe-percentage successful-cd-predicate-tests all-cd-predicate-tests)
              failed-cd-predicate-tests
              (safe-percentage failed-cd-predicate-tests all-cd-predicate-tests)
              all-entailment-tests
              successful-cd-predicate-entailed-tests
              (safe-percentage successful-cd-predicate-entailed-tests all-entailment-tests)
              failed-cd-predicate-entailed-tests
              (safe-percentage failed-cd-predicate-entailed-tests all-entailment-tests)))))

(defun print-clash-statistics (stream &key (local nil))
  (let ((clash-tests (get-statistics-integer-value *number-of-clash-tests* :local local))
        (clashes (get-statistics-integer-value *number-of-clashes* :local local))
        (duplicates
         (get-statistics-integer-value *number-of-duplicates-during-clash-test*
                                       :local local))
        (new-constraints
         (get-statistics-integer-value *number-of-new-constraints-during-clash-test*
                                       :local local)))
    (format stream "~&;Clash tests: ~:D, Clashes: ~:D (~,2F%), Duplicates: ~:D (~,2F%), ~
                    New Constraints: ~:D (~,2F%)~%"
            clash-tests
            clashes
            (safe-percentage clashes clash-tests)
            duplicates
            (safe-percentage duplicates clash-tests)
            new-constraints
            (safe-percentage new-constraints clash-tests))))

(defun print-blocking-statistics (stream &key (local nil))
  (let* ((sh-failed (get-statistics-integer-value *number-of-failed-sh-blocking-tests*
                                                  :local local))
         (shi-failed (get-statistics-integer-value *number-of-failed-shi-blocking-tests*
                                                   :local local))
         (shiq-failed (get-statistics-integer-value *number-of-failed-shiq-blocking-tests*
                                                    :local local))
         (shi-weak-failed
          (get-statistics-integer-value *number-of-weak-failed-shi-blocking-tests*
                                        :local local))
         (shiq-weak-failed
          (get-statistics-integer-value *number-of-weak-failed-shiq-blocking-tests*
                                        :local local))
         (sh-successful (get-statistics-integer-value *number-of-successful-sh-blocking-tests*
                                                      :local local))
         (shi-successful (get-statistics-integer-value *number-of-successful-shi-blocking-tests*
                                                       :local local))
         (c-shiq-successful
          (get-statistics-integer-value *number-of-successful-c-shiq-blocking-tests*
                                        :local local))
         (a-shiq-successful
          (get-statistics-integer-value *number-of-successful-a-shiq-blocking-tests*
                                        :local local))
         (shiq-successful (+ c-shiq-successful a-shiq-successful))
         (zero-shiq-successful
          (get-statistics-integer-value *number-of-successful-zero-shiq-blocking-tests*
                                        :local local))
         (sh-total (+ sh-failed sh-successful))
         (shi-total (+ shi-failed shi-successful))
         (shiq-total (+ shiq-failed shiq-successful)))
    (when (> sh-total 0)
      (format stream "~&;SH Blocking tests: ~:D, Failed: ~:D (~,2F%), Successful: ~:D (~,2F%)~%"
              sh-total
              sh-failed
              (safe-percentage sh-failed sh-total)
              sh-successful
              (safe-percentage sh-successful sh-total)))
    (when (> shi-total 0)
      (format stream "~&;SHI Blocking tests: ~:D, Failed: ~:D (~,2F%), Weak Failed: ~:D ~
                      (~,2F% of failed), Successful: ~:D (~,2F%)~%"
              shi-total
              shi-failed
              (safe-percentage shi-failed shi-total)
              shi-weak-failed
              (safe-percentage shi-weak-failed shi-failed)
              shi-successful
              (safe-percentage shi-successful shi-total)))
    (when (> shiq-total 0)
      (format stream "~&;SHIQ Blocking tests: ~:D, Failed: ~:D (~,2F%), Weak Failed: ~:D ~
                      (~,2F% of failed),~%~
                      ;         Successful: ~:D (~,2F%, Cyclic: ~:D (~,2F%), ~
                      Acyclic: ~:D (~,2F%), Zero witness: ~:D (~,2F%))~%"
              shiq-total
              shiq-failed
              (safe-percentage shiq-failed shiq-total)
              shiq-weak-failed
              (safe-percentage shiq-weak-failed shiq-failed)
              shiq-successful
              (safe-percentage shiq-successful shiq-total)
              c-shiq-successful
              (safe-percentage c-shiq-successful shiq-successful)
              a-shiq-successful
              (safe-percentage a-shiq-successful shiq-successful)
              zero-shiq-successful
              (safe-percentage zero-shiq-successful shiq-successful)))))

(defun print-core-statistics (tbox
                                  stream
                                  &key
                                  (data-stream *race-statistics-stream*)
                                  (local nil))
  (when stream
    (print-blocking-statistics stream :local local)
    (print-clash-statistics stream :local local)
    (print-semantic-splits stream :local local)
    (when *use-success-based-disjunct-selection*
      (print-alt-semantic-splits stream :local local))
    (print-signature-splits stream :local local)
    (print-cd-statistics stream :local local)
    (when (get-statistics-list-value *simplex-statistics-list* :local local)
      (format stream "~&;Simplex profile (var-no eqn-no iter-no): ~S~%"
              (get-statistics-list-value *simplex-statistics-list* :local local))))
  (print-model-accesses tbox stream data-stream :local local)
  (print-model-merging tbox stream data-stream :local local)
  (print-tableaux-caching tbox stream data-stream :local local)
  (print-constraint-store-statistics stream data-stream :local local)
  (print-hash-table-statistics stream data-stream :local local))

(defun print-sat-statistics (stream data-stream start end)
  (let ((time (/ (- end start) internal-time-units-per-second)))
    (when stream
      (format stream "~&;Runtime: ~,2F secs.~%" time)))
  (let ((tbox *current-tbox*))
    (when tbox
      (when stream
        (format stream "~&;Concept store ~D/~D, role store ~D/~D, subtableaux equal cache ~D/~D~%"
                (hash-table-count (tbox-concept-store tbox))
                (hash-table-size (tbox-concept-store tbox))
                (hash-table-count (tbox-role-store tbox))
                (hash-table-size (tbox-role-store tbox))
                (if (hash-table-p (tbox-tableaux-cache tbox))
                  (hash-table-count (tbox-tableaux-cache tbox))
                  0)
                (if (hash-table-p (tbox-tableaux-cache tbox))
                  (hash-table-size (tbox-tableaux-cache tbox))
                  0)))
      (when data-stream
        (format data-stream "~16D~16D~16D~16D~16D~16D"
                (hash-table-count (tbox-concept-store tbox))
                (hash-table-size (tbox-concept-store tbox))
                (hash-table-count (tbox-role-store tbox))
                (hash-table-size (tbox-role-store tbox))
                (if (hash-table-p (tbox-tableaux-cache tbox))
                  (hash-table-count (tbox-tableaux-cache tbox))
                  0)
                (if (hash-table-p (tbox-tableaux-cache tbox))
                  (hash-table-size (tbox-tableaux-cache tbox))
                  0)))
      (print-core-statistics tbox stream :data-stream data-stream :local t)
      (when data-stream
        (force-output data-stream))))
  (reset-local-statistics-table))

;;;===========================================================================

(defun print-tbox-statistics-name (tbox data-stream)
  (when data-stream
    (format data-stream "  ~14A" (tbox-name tbox))))

(defun print-tbox-info (&optional (tbox *current-tbox*) (stream t))
  (let* ((total (length (tbox-encoded-concept-list tbox)))
         (info-1
          (loop with top = (tbox-top-node tbox)
                with named-concepts = nil
                with prim-no = 0
                with definded-no = 0
                with atomic-no = 0
                with cyclic-no = 0
                for concept in (tbox-encoded-concept-list tbox) do
                (if (and (concept-encoded-definition concept)
                         (not (eq (concept-encoded-definition concept) top)))
                    (if (concept-primitive-p concept)
                        (incf prim-no)
                      (incf definded-no))
                  (incf atomic-no))
                (when (concept-self-referencing-p concept)
                  (incf cyclic-no))
                finally
                (setf named-concepts (+ atomic-no prim-no definded-no))
                (return
                 (format stream "~&TBox ~A (~A): No. of named concepts:~D ~
                          (~D (~,2F%) atomic, ~D (~,2F%) primitive, ~D (~,2F%) defined, ~D (~,2F%) cyclic, ~
                          ~D (~,2F%) incoherent)~%"
                         (tbox-name tbox)
                         (tbox-language tbox)
                         named-concepts
                         prim-no
                         (safe-percentage prim-no total)
                         atomic-no
                         (safe-percentage atomic-no total)
                         definded-no
                         (safe-percentage definded-no total)
                         cyclic-no
                         (safe-percentage cyclic-no total)
                         (- (length (concept-name-set (tbox-bottom-node tbox))) 2)
                         (safe-percentage (- (length (concept-name-set (tbox-bottom-node tbox))) 2) total)))))
         (info-2
          (loop with cd-count = 0
                with elh-count = 0
                with end-of-no-bottom-search = (tbox-end-of-no-bottom-search tbox)
                with no-bottom = 
                (if end-of-no-bottom-search
                    (1+ (position end-of-no-bottom-search (tbox-encoded-concept-list tbox)))
                  0)
                with stat-list = (loop with table = (racer-make-hash-table)
                                       for concept in (tbox-encoded-concept-list tbox)
                                       do (incf (gethash (concept-language concept) table 0))
                                       finally
                                       (return
                                        (loop for language being the hash-key of table using (hash-value count)
                                              collect (list language count))))
                for (language count) in stat-list do
                (if (subset-l-minus-p language)
                    (incf cd-count count)
                  (when (subset-el+-p language)
                    (incf elh-count count)))
                finally
                (return
                 (format stream 
                         "Atomic concept statistics: ~D (~,2F%) completely defined, ~
                          ~D (~,2F%) no bottom search, ~D (~,2F%) EL+; all=~A~%"
                         cd-count
                         (safe-percentage cd-count total)
                         no-bottom
                         (safe-percentage no-bottom total)
                         elh-count
                         (safe-percentage elh-count total)
                         (sort stat-list #'> :key 'second))))))
    (values info-1 info-2)))

(defun print-tbox-statistics (tbox stream data-stream start end)
  (let ((time (/ (- end start) internal-time-units-per-second)))
    (let (named-concepts
          (prim-no 0)
          (definded-no 0)
          (atomic-no 0)
          (cyclic-no 0))
      (loop with top = (tbox-top-node tbox)
            for concept in (tbox-encoded-concept-list tbox) do
            (if (and (concept-encoded-definition concept)
                     (not (eq (concept-encoded-definition concept) top)))
                (if (concept-primitive-p concept)
                    (incf prim-no)
                  (incf definded-no))
              (incf atomic-no))
            (when (concept-self-referencing-p concept)
              (incf cyclic-no))
            finally
            (setf named-concepts (+ atomic-no prim-no definded-no)))
      (when stream
        (format stream "~2&;TBox ~A (~A): No. of named concepts:~D ~
                        (~D atomic, ~D primitive, ~D defined, ~D cyclic, ~D incoherent)~%"
                (tbox-name tbox)
                (tbox-language tbox)
                named-concepts
                atomic-no prim-no definded-no
                cyclic-no
                (- (length (concept-name-set (tbox-bottom-node tbox))) 2)))
      (when data-stream
        (format data-stream "~10,3F~8D~8D~8D~8D"
                time named-concepts atomic-no prim-no definded-no))
      (when stream
        (format stream ";Runtime: ~,3F secs.~%" time)
        (format stream ";Taxonomic encoding replacements: ~D"
                (get-total-statistics-integer-value *taxonomic-encoding-hits*)))))
  (when data-stream
    (format data-stream "~8D"
            (get-total-statistics-integer-value *taxonomic-encoding-hits*))
    (format data-stream "~16D~16D~16D~16D~16D~16D"
            (hash-table-count (tbox-concept-store tbox))
            (hash-table-size (tbox-concept-store tbox))
            (hash-table-count (tbox-role-store tbox))
            (hash-table-size (tbox-role-store tbox))
            (if (hash-table-p (tbox-tableaux-cache tbox))
                (hash-table-count (tbox-tableaux-cache tbox))
              0)
            (if (hash-table-p (tbox-tableaux-cache tbox))
                (hash-table-size (tbox-tableaux-cache tbox))
              0)))
  (when stream
    (format stream "~&;Concept store ~D/~D, role store ~D/~D, subtableaux equal cache ~D/~D~%"
            (hash-table-count (tbox-concept-store tbox))
            (hash-table-size (tbox-concept-store tbox))
            (hash-table-count (tbox-role-store tbox))
            (hash-table-size (tbox-role-store tbox))
            (if (hash-table-p (tbox-tableaux-cache tbox))
                (hash-table-count (tbox-tableaux-cache tbox))
              0)
            (if (hash-table-p (tbox-tableaux-cache tbox))
                (hash-table-size (tbox-tableaux-cache tbox))
              0)))                
  (let ((all-inclusions-l (length (append (tbox-generalized-concept-inclusions tbox)
                                          (tbox-added-generalized-concept-inclusions tbox)))))
    (when stream
      (format stream "~&;No. of GCIs: ~D (~D original, ~D absorbed)~%"
              (length (tbox-meta-constraint-concepts tbox))
              all-inclusions-l
              (- all-inclusions-l (length (tbox-meta-constraint-concepts tbox)))))
    (when data-stream
      (format data-stream "~8D~8D~8D"
              (length (tbox-meta-constraint-concepts tbox))
              all-inclusions-l
              (- all-inclusions-l (length (tbox-meta-constraint-concepts tbox))))))
  (let ((all-subsumption-tests
         (get-total-statistics-integer-value *all-subsumption-tests*))
        (subsumptions-found
         (get-total-statistics-integer-value *subsumptions-found*))
        (total-unmergable-det-models
         (get-total-statistics-integer-value *unmergable-det-models*))
        (obvious-subsumptions
         (get-total-statistics-integer-value *obvious-subsumptions*))
        (model-subsumption-tests
         (get-total-statistics-integer-value *model-subsumption-tests*))
        (bottom-search-tests
         (get-total-statistics-integer-value *bottom-search-tests*)))
    (when stream
      (format stream "~&;No. of subsumption tests: total=~:D, success=~:D (~,2F%), model=~:D, tableaux=~:D, obvious subs=~:D~%"
              all-subsumption-tests
              (+ subsumptions-found total-unmergable-det-models)
              (safe-percentage (+ subsumptions-found total-unmergable-det-models)
                               all-subsumption-tests)
              model-subsumption-tests
              *subsumption-tests*
              obvious-subsumptions))
    (when stream
      (let ((end-of-no-bottom-search (tbox-end-of-no-bottom-search tbox))
            (stat-list
             (loop with table = (racer-make-hash-table)
                   for concept in (tbox-encoded-concept-list tbox)
                   do (incf (gethash (concept-language concept) table 0))
                   finally
                   (return
                    (loop for language being the hash-key of table using (hash-value count)
                          collect (list language count))))))
        (when end-of-no-bottom-search
          (format stream "~&;End of no bottom search at pos ~D out of ~D, ~D left~%"
                  (position end-of-no-bottom-search (tbox-encoded-concept-list tbox))
                  (length (tbox-encoded-concept-list tbox))
                  (1+ (- (length (tbox-encoded-concept-list tbox))
                         (position end-of-no-bottom-search (tbox-encoded-concept-list tbox))))))
        (format stream "~&;No. of bottom search tests: ~:D~%" bottom-search-tests)
        (loop with cd-count = 0
              with elh-count = 0
              with total = (length (tbox-encoded-concept-list tbox))
              with no-bottom = 
              (if end-of-no-bottom-search
                  (1+ (position end-of-no-bottom-search (tbox-encoded-concept-list tbox)))
                0)
              for (language count) in stat-list do
              (if (subset-l-minus-p language)
                  (incf cd-count count)
                (when (subset-el+-p language)
                  (incf elh-count count)))
              finally
              (format stream "~&;Atomic concept statistics: ~D (~,2F%) completely defined, ~D (~,2F%) no bottom search, ~D (~,2F%) EL+,~
                              ~%;                           all=~A~%"
                      cd-count
                      (safe-percentage cd-count total)
                      no-bottom
                      (safe-percentage no-bottom total)
                      elh-count
                      (safe-percentage elh-count total)
                      (sort stat-list #'> :key 'second))))
      (print-tbox-children-statistics tbox stream)))
  (print-core-statistics tbox stream :data-stream data-stream)
  (when data-stream
    (force-output data-stream))
  (reset-statistics-tables))

(defun print-tbox-children-statistics (tbox stream)
  (loop with concepts-with-buckets = 0
        with bucket-count = 0
        with concepts-with-children = 0
        with concepts-with-children-below-bucket-size = nil
        with concepts-with-children-above-bucket-size = nil
        with total-children-count = 0
        for concept in (cons (tbox-top-node tbox) (tbox-encoded-concept-list tbox))
        for buckets = (concept-bucket-children concept)
        for buckets-length = (length buckets)
        for children-count = (length (concept-children-internal concept))
        maximize buckets-length into max-buckets-length
        maximize children-count into max-children-count
        when (> buckets-length 1) do
        (push (list children-count buckets-length)
              concepts-with-children-above-bucket-size)
        when (plusp children-count) do
        (incf concepts-with-children)
        (incf total-children-count children-count)
        (when (< 5 children-count *bucket-size-threshold*)
          (push children-count concepts-with-children-below-bucket-size))
        when buckets do
        (incf concepts-with-buckets)
        (incf bucket-count buckets-length)
        finally
        (format stream "~&;No. of children: total=~:D, max-no=~:D, average=~,2F (concepts=~:D)~%"
                total-children-count max-children-count
                (if (zerop concepts-with-children)
                  0
                  (/ total-children-count concepts-with-children))
                concepts-with-children)
        (when *tbox-clustering*
          (let ((deleted-buckets (get-total-statistics-integer-value *deleted-buckets*))
                (merged-buckets (get-total-statistics-integer-value *merged-buckets*)))
            (format stream "~&;Initial bucket size: ~D, bucket merging: ~S, max no. of buckets: ~D~%~
                            ;No. of buckets: current=~:D, created=~:D, deleted=~:D, merged=~:D, max-no=~:D, average=~,2F (concepts=~:D)~%~
                            ;No. of concepts with 5<children<~D: ~:D ~S~%~
                            ;No. of concepts with buckets>5: ~:D ~S~%"
                    *bucket-size-threshold*
                    *merge-buckets*
                    *merge-buckets-threshold*
                    bucket-count (+ bucket-count deleted-buckets)
                    deleted-buckets merged-buckets max-buckets-length
                    (if (zerop concepts-with-buckets)
                      0
                      (/ bucket-count concepts-with-buckets))
                    concepts-with-buckets
                    *bucket-size-threshold*
                    (length concepts-with-children-below-bucket-size)
                    (when (<= (length concepts-with-children-below-bucket-size) 100)
		      concepts-with-children-below-bucket-size)
                    (length concepts-with-children-above-bucket-size)
		    (when (<= (length concepts-with-children-above-bucket-size) 100)
		      concepts-with-children-above-bucket-size))))))

(defun print-statistics-header (stream)
  (format stream "~&# ~14A~10,2F~8@A~8@A~8@A~8@A~
                  ~8A~
                  ~16@A~16@A~16@A~16@A~16@A~16@A~
                  ~8@A~8@A~8@A~
                  ~8@A~8@A~10@A~8@A~10@A"
          "KB" "time" "n-co" "atom" "prim" "def"
          "te-hits"
          "con-st-no" "con-st-sz" "role-st-no" "role-st-sz" "st-ca-no" "st-ca-sz"
          "GCIs" "orig" "absorb"
          "subs" "ca-mo" "blo-mo%" "re-mo" "re-mo%")
  (format stream "~16@A~10@A~16@A~10@A~
                  ~16@A~16@A~16@A~10@A~16@A~10@A~16@A~10@A~16@A~10@A~
                  ~16@A~16@A~16@A~10@A~16@A~10@A~%"
          "tmc-hits" "tmc-hits%" "tmc-miss" "tmc-miss%"
          "tm-mo" "tu-mo" "merg-s" "merg-s%" "tu-pmo" "tu-pmo%" "tu-mo-s" "tu-mo-s%" "tu-mo-u" "tu-mo-u%"
          "stc-sh" "stc-uh" "stc-h" "stc-h%" "stc-m" "stc-m%"))

(defmacro with-statistics-file ((filename) &body body)
  (let ((stream-sym (gensym)))
    `(with-open-file (,stream-sym ,filename :direction :output
                                  :if-exists :append :if-does-not-exist :create)
       (let ((*race-statistics-stream* ,stream-sym))
         (print-statistics-header ,stream-sym)
         . ,body))))

;;;===========================================================================

(defun print-abox-statistics-name (abox data-stream)
  (when data-stream
    (format data-stream "  ~14A" (abox-name abox))))

(defun print-abox-statistics (the-abox stream data-stream start end &optional (local nil))
  (let* ((time (/ (- end start) internal-time-units-per-second))
         (total-computed-models (get-total-statistics-integer-value *computed-models*))
         (subsumptions-found (get-total-statistics-integer-value *ind-subsumptions-found*))
         (subsumption-tests (get-total-statistics-integer-value *ind-subsumption-tests*)))
    ;(break)
    (when data-stream
      (format data-stream "~10,2F~8D~8D"
              time subsumption-tests total-computed-models))
    (when stream
      (format stream "~&;ABox ~A (~A): No. of inds:~D, ind assertions:~D, role assertions:~D, subgraphs:~D~%"
              (abox-name the-abox)
              (abox-language the-abox)
              (length (abox-individuals-list the-abox))
              (length (abox-individual-axioms the-abox))
              (length (abox-role-axioms the-abox))
              (length (abox-subgraphs the-abox)))
      (format stream ";Runtime: ~,2F secs.~%" time)
      (format stream "~&;No. of true ind subsumption tests: ~D, success=~:D (~,2F%)~%"
              subsumption-tests
              subsumptions-found
              (safe-percentage subsumptions-found subsumption-tests))
      (format stream "~&;No. of caching satisfiability tests: ~D~%"
              total-computed-models))
    (print-binary-retrieval-statistics stream :local local)
    (print-dependency-retrieval-statistics stream :local local)
    (print-core-statistics (tbox the-abox) stream :data-stream data-stream :local local)
    (print-core-abox-statistics the-abox stream data-stream)
    (when data-stream
      (force-output data-stream))
    (reset-statistics-tables)))

(defun print-binary-retrieval-statistics (stream &key (local nil))
  (let* ((bin-retrievals
          (get-statistics-integer-value *number-of-binary-retrievals* :local local))
         (candidates
          (get-statistics-integer-value *number-of-binary-retrieval-candidates* :local local))
         (obvious-inds
          (get-statistics-integer-value *number-obv-inds-in-binary-retrieval* :local local))
         (tested-inds
          (get-statistics-integer-value *number-of-inds-to-be-tested-in-binary-retrieval*
                                        :local local))
         ;;(bin-partitionings
         ;; (get-statistics-integer-value *number-of-binary-partition-retrievals* :local local))
         (all-inds (+ obvious-inds tested-inds)))
    ;; Changes made by RM Apr. 14: Too many format arguments
    (when (and stream (> bin-retrievals 0))
      (format stream "~&;Binary Instance retrieval: ~D tests, candidates ~D, ~
                      obvious instances ~D (~,2F%), ~%~
                      inds tested ~,2F~%"
                      ;                           inds tested ~D (~,2F%), Partitionings ~D~%~
                      ;                  Average: candidates ~,2F, obvious instances ~,2F, ~
              bin-retrievals
              candidates
              obvious-inds
              (safe-percentage obvious-inds all-inds)
              #|tested-inds
              (safe-percentage tested-inds all-inds)
              bin-partitionings
              (/ candidates bin-retrievals)
              (/ obvious-inds bin-retrievals)
              (safe-percentage (/ obvious-inds bin-retrievals)
                               (/ all-inds bin-retrievals))
              (round tested-inds bin-retrievals)|#
              (safe-percentage (/ tested-inds bin-retrievals)
                               (/ all-inds bin-retrievals))
              ))))

(defun print-dependency-retrieval-statistics (stream &key (local nil))
  (let* ((dep-retrievals
          (get-statistics-integer-value *number-of-dependency-retrievals* :local local))
         (candidates
          (get-statistics-integer-value *number-of-dependency-retrieval-candidates* :local local))
         (obvious-inds
          (get-statistics-integer-value *number-obv-inds-in-dependency-retrieval* :local local))
         (tested-inds
          (get-statistics-integer-value *number-of-inds-to-be-tested-in-dependency-retrieval*
                                        :local local))
         ;;(dep-partitionings
         ;; (get-statistics-integer-value *number-of-dependency-partition-retrievals* :local local))
         (all-inds (+ obvious-inds tested-inds)))
    ;; Changes made by RM Apr. 14: Too many format arguments
    (when (and stream (> dep-retrievals 0))
      (format stream "~&;Dependency instance retrieval: ~D tests, candidates ~D, ~
                      obvious instances ~D (~,2F%), ~%~
                      inds tested ~,2F~%"
                      ;                               inds tested ~D (~,2F%), Partitionings ~D~%~
                      ;                      Average: candidates ~,2F, obvious instances ~,2F, ~
              dep-retrievals
              candidates
              obvious-inds
              (safe-percentage obvious-inds all-inds)
              #|tested-inds
              (safe-percentage tested-inds all-inds)
              dep-partitionings
              (/ candidates dep-retrievals)
              (/ obvious-inds dep-retrievals)
              (safe-percentage (/ obvious-inds dep-retrievals)
                               (/ all-inds dep-retrievals))
              (round tested-inds dep-retrievals)|#
              (safe-percentage (/ tested-inds dep-retrievals)
                               (/ all-inds dep-retrievals))
              ))))

(defun print-core-abox-statistics (the-abox
                                        stream
                                        &optional (data-stream *race-statistics-stream*))
  (declare (ignore data-stream))
  (let* ((total-ind-mergable-models (get-total-statistics-integer-value *ind-mergable-models*))
         (total-ind-unmergable-det-models
          (get-total-statistics-integer-value *ind-unmergable-det-models*))
         (total-ind-unmergable-partial-models
          (get-total-statistics-integer-value *ind-unmergable-partial-models*))
         (total-ind-unmergable-models-satisfiable
          (get-total-statistics-integer-value *ind-unmergable-models-satisfiable*))
         (total-ind-unmergable-models-unsatisfiable 
          (get-total-statistics-integer-value *ind-unmergable-models-unsatisfiable*))
         (total-ind-model-cache-hits
          (get-total-statistics-integer-value *ind-model-cache-hits*))
         (total-ind-model-cache-misses
          (get-total-statistics-integer-value *ind-model-cache-misses*))
         (model-cache-accesses (+ total-ind-model-cache-hits total-ind-model-cache-misses))
         (merging-successes (+ total-ind-unmergable-det-models total-ind-mergable-models))
         (merging-tests (+ merging-successes total-ind-unmergable-partial-models))
         (total-ind-concept-subsumption-tests
          (get-total-statistics-integer-value *ind-concept-subsumption-tests*))
         (total-ind-concept-subsumptions-found
          (get-total-statistics-integer-value *ind-concept-subsumptions-found*))
         (total-ind-abox-completion-tests
          (get-total-statistics-integer-value *ind-abox-completion-tests*))
         (total-ind-abox-completion-non-subsumption
          (get-total-statistics-integer-value *ind-abox-completion-non-subsumption*)))
    (when stream
      (format stream "~&;ABox model cache: (~D hits (~,2F%), ~D misses (~,2F%))~%"
              total-ind-model-cache-hits
              (safe-percentage  total-ind-model-cache-hits model-cache-accesses)
              total-ind-model-cache-misses
              (safe-percentage  total-ind-model-cache-misses model-cache-accesses))
      (multiple-value-bind (total-models non-det det concepts)
                           (loop with total-models = 0
                                 with non-det = 0
                                 with det = 0
                                 with concepts = 0
                                 for ind in (abox-individuals-list the-abox)
                                 for model = (individual-model ind)
                                 do
                                 (when model
                                   (incf total-models)
                                   (if (concept-p-internal model)
                                     (incf concepts)
                                     (if (and (full-model-info-p model)
                                              (model-non-deterministic-p model))
                                       (incf non-det)
                                       (incf det))))
                                 finally (return (values total-models non-det det concepts)))
        (let ((true-models (- total-models concepts)))
          (format stream ";ABox models: total=~D, ~D concepts (~,2F%), ~D true models (~,2F%, ~
                          ~D non-deterministic (~,2F%), ~D deterministic (~,2F%))~%"
                  total-models
                  concepts
                  (safe-percentage concepts total-models)
                  true-models
                  (safe-percentage true-models total-models)
                  non-det
                  (safe-percentage non-det true-models)
                  det
                  (safe-percentage det true-models)))))
    (when (and stream *abox-model-merging*)
      (format stream ";ABox model merging: (~D mergable / ~D det-unmergable = ~
                      ~D hits (~,2F%) , ~D misses (~,2F%))~%"
              total-ind-mergable-models
              total-ind-unmergable-det-models 
              merging-successes
              (safe-percentage merging-successes merging-tests)
              total-ind-unmergable-partial-models
              (safe-percentage total-ind-unmergable-partial-models merging-tests))
      (format stream ";               ~D (~,2F%) unmergable models found satisfiable~%~
                      ;               ~D (~,2F%) unmergable models found unsatisfiable~%"
              total-ind-unmergable-models-satisfiable
              (safe-percentage  total-ind-unmergable-models-satisfiable
                                total-ind-unmergable-partial-models)
              total-ind-unmergable-models-unsatisfiable
              (safe-percentage  total-ind-unmergable-models-unsatisfiable
                                total-ind-unmergable-partial-models))
      (format stream ";ABox: ~D individual concept subsumption tests, ~D successful (~,2F%)~%~
                      ;      ~D individual abox completion tests, ~D satisfiable (~,2F%)~%"
              total-ind-concept-subsumption-tests
              total-ind-concept-subsumptions-found
              (safe-percentage total-ind-concept-subsumptions-found
                               total-ind-concept-subsumption-tests)
              total-ind-abox-completion-tests
              total-ind-abox-completion-non-subsumption
              (safe-percentage total-ind-abox-completion-non-subsumption
                               total-ind-abox-completion-tests))
      #|(when (and *model-merging* *use-alternate-ind-models*)
        (loop for ind in (abox-individuals-list the-abox)
              for model = (individual-model ind)
              for alternates = (length (and (model-info-p model)
                                            (model-alternates model)))
              when model
              sum 1 into concepts-with-models
              unless (zerop alternates)
              maximize alternates into max-alternates and
              sum alternates into all-alternates and
              sum 1 into concept-models-with-alternates
              finally
              (unless (zerop all-alternates)
                (format stream "~&;Alternate models: max-no=~D, total-no=~D, ~
                                concept-models=~D, concept-model-alternates=~D~%"
                        max-alternates all-alternates
                        concepts-with-models concept-models-with-alternates))))|#)))

(defun print-hash-table-statistics (stream
                                         data-stream
                                         &key (local nil))
  (declare (ignore data-stream))
  (when stream
    (let* ((copied-tables
            (get-statistics-integer-value *number-of-copied-hash-tables* :local local))
           (copied-table-entries
            (get-statistics-integer-value *number-of-copied-hash-table-entriess* :local local))
           (size-of-copied-tables
            (get-statistics-integer-value *size-of-copied-hash-tables* :local local))
           (merged-tables
            (get-statistics-integer-value *number-of-merged-hash-tables* :local local))
           (merged-table-entries
            (get-statistics-integer-value *number-of-merged-hash-table-entriess* :local local))
           (size-of-merged-tables
            (get-statistics-integer-value *size-of-merged-hash-tables* :local local)))
      (when (or (> copied-tables 0) (> merged-tables 0))
        (format stream "~&;Hash Tables: ~:D merged (entries=~:D, total size=~:D), ~
                        ~:D copied (entries=~:D, total size=~:D)~%"
                merged-tables merged-table-entries size-of-merged-tables
                copied-tables copied-table-entries size-of-copied-tables)))))
      
(defun print-constraint-store-statistics (stream
                                                data-stream
                                                &key (local nil))
  (declare (ignore data-stream))
  (when (and stream
             (or *abox-use-expanded-store*
                 *abox-use-unexpanded-disjunctive-constraints-store*
                 *abox-use-unexpanded-exists-constraints-store*))
    (let* ((created-exp-stores
            (get-statistics-integer-value *number-of-created-expanded-constraint-stores*
                                          :local local))
           (created-exists-stores
            (get-statistics-integer-value *number-of-created-unexp-exists-constraint-stores*
                                          :local local))
           (created-disj-stores
            (get-statistics-integer-value *number-of-created-unexp-disjunctive-constraint-stores*
                                          :local local))
           (created-stores
            (+ created-exp-stores created-exists-stores created-disj-stores))
           (copied-exp-stores
            (get-statistics-integer-value *number-of-copied-expanded-constraint-stores*
                                          :local local))
           (copied-exists-stores
            (get-statistics-integer-value *number-of-copied-unexp-exists-constraint-stores*
                                          :local local))
           (copied-disj-stores
            (get-statistics-integer-value *number-of-copied-unexp-disjunctive-constraint-stores*
                                          :local local))
           (copied-stores
            (+ copied-exp-stores copied-exists-stores copied-disj-stores))
           (merged-stores
            (get-statistics-integer-value *number-of-merged-expanded-constraint-stores*
                                          :local local))
           (exp-swaps
            (get-statistics-integer-value *number-of-swaps-to-expanded-constraint-stores*
                                          :local local))
           (exists-swaps
            (get-statistics-integer-value *number-of-swaps-to-unexp-exists-constraint-stores*
                                          :local local))
           (disj-swaps
            (get-statistics-integer-value *number-of-swaps-to-unexp-disjunctive-constraint-stores*
                                          :local local))
           (swaps
            (+ exp-swaps exists-swaps disj-swaps))
           (extend-swaps
            (get-statistics-integer-value *number-of-extend-swaps-to-expanded-constraint-stores*
                                          :local local))
           (swapped-exp-constraints
            (get-statistics-integer-value *number-of-swapped-expanded-constraints*
                                          :local local))
           (swapped-exists-constraints
            (get-statistics-integer-value *number-of-swapped-unexp-exists-constraints*
                                          :local local))
           (swapped-disj-constraints
            (get-statistics-integer-value *number-of-swapped-unexp-disjunctive-constraints*
                                          :local local))
           (swapped-constraints
            (+ swapped-exp-constraints swapped-exists-constraints swapped-disj-constraints))
           (removed-exists-constraints
            (get-statistics-integer-value *number-of-removed-unexp-exists-constraints*
                                          :local local))
           (exists-removals
            (get-statistics-integer-value *number-of-removals-from-unexp-exists-constraint-stores*
                                          :local local))
           (removed-disj-constraints
            (get-statistics-integer-value *number-of-removed-unexp-disjunctive-constraints*
                                          :local local))
           (disj-removals
            (get-statistics-integer-value *number-of-removals-from-unexp-disjunctive-constraint-stores*
                                          :local local))
           (max-exp-length
            (get-statistics-integer-value *max-length-of-expanded-constraint-stores*
                                          :local local))
           (max-disj-length
            (get-statistics-integer-value *max-length-of-unexp-disjunctive-constraint-stores*
                                          :local local))
           (max-exists-length
            (get-statistics-integer-value *max-length-of-unexp-exists-constraint-stores*
                                          :local local))
           (created-caches
            (get-statistics-integer-value *number-of-created-label-caches*
                                          :local local))
           (added-entries
            (get-statistics-integer-value *number-of-added-label-cache-entries*
                                          :local local))
           (removed-entries
            (get-statistics-integer-value *number-of-removed-label-cache-entries*
                                          :local local))
           (cache-hits (get-statistics-integer-value *label-cache-hits*
                                                     :local local))
           (cache-misses (get-statistics-integer-value *label-cache-misses*
                                                       :local local))
           (cache-accesses (+ cache-hits cache-misses))
           )
      (when (> created-stores 0)
        (format stream "~&;Constraint Stores: ~:D created, ~:D copied (~,2F%), ~:D swaps, ~
                        ~:D swapped constraints~%"
                created-stores
                copied-stores
                (safe-percentage copied-stores created-stores)
                swaps
                swapped-constraints)
        (format stream "~&;Constraint Store Parameters: Swap-exp>=~D, Swap-unexp>=~D, ~
                        remove-size=~D, remove-fraction=~D (~,2F%)~%"
                *swap-to-expanded-store-threshold*
                *swap-to-unexpanded-store-threshold*
                *cache-size-from-constraint-store*
                *cache-size-fraction-from-constraint-store*
                (* 100.0 (/ *cache-size-from-constraint-store*
                            *swap-to-unexpanded-store-threshold*)))
        (when (> created-exp-stores 0)
          (format stream "~&;Expanded Constraint Stores: ~:D created, ~:D copied (~,2F%), ~
                          ~:D merged (~,2F%), ~:D swaps (extend ~:D (~,2F%))~%~
                          ;                            ~:D swapped constraints, ~
                          max length = ~D (total max = ~D), ~%"
                  created-exp-stores
                  copied-exp-stores
                  (safe-percentage copied-exp-stores created-exp-stores)
                  merged-stores
                  (safe-percentage merged-stores created-exp-stores)
                  exp-swaps
                  extend-swaps
                  (safe-percentage extend-swaps exp-swaps)
                  swapped-exp-constraints
                  max-exp-length
                  *max-constraint-stores-length*))
        (when (> created-exists-stores 0)
          (format stream "~&;Unexpanded Exists Constraint Stores: ~:D created, ~:D copied (~,2F%), ~
                          ~:D swaps, ~:D swapped constraints~%~
                          ;                                     ~:D removals, ~
                          ~:D removed constraints, max length = ~D~%"
                  created-exists-stores
                  copied-exists-stores
                  (safe-percentage copied-exists-stores created-exists-stores)
                  exists-swaps
                  swapped-exists-constraints
                  exists-removals
                  removed-exists-constraints
                  max-exists-length))
        (when (> created-disj-stores 0)
          (format stream "~&;Unexpanded Disjunctive Constraint Stores: ~:D created, ~:D copied (~,2F%), ~
                          ~:D swaps, ~:D swapped constraints~%~
                          ;                                          ~:D removals, ~
                          ~:D removed constraints, max length = ~D~%"
                  created-disj-stores
                  copied-disj-stores
                  (safe-percentage copied-disj-stores created-disj-stores)
                  disj-swaps
                  swapped-disj-constraints
                  disj-removals
                  removed-disj-constraints
                  max-disj-length)))
      (when (> created-caches 0)
        (format stream ";                   ~:D created label caches, ~:D added entries, ~:D removed, ~%~
                        ;                   ~:D hits (~,2F%), ~:D misses (~,2F%)~%"
                created-caches
                added-entries
                removed-entries
                cache-hits
                (safe-percentage cache-hits cache-accesses)
                cache-misses
                (safe-percentage cache-misses cache-accesses))))))

(defun print-sat-local-statistics (&optional (stream t))
  (when *verbose*
    (format stream "~2&;or depth=~D, subtableau (depth=~D, size=~D)~%"
            (get-local-statistics-integer-value *max-or-level*)
            (get-local-statistics-integer-value *max-model-depth*)
            (get-local-statistics-integer-value *max-model-size*))
    (when *model-merging*
      (format stream ";concept models (hits=~D, misses=~D, mergable=~D, ~
                      det-unmergable=~D, nondet-unmergable=~D)~%"
              (get-local-statistics-integer-value *model-cache-hits*)
              (get-local-statistics-integer-value *model-cache-misses*)
              (get-local-statistics-integer-value *mergable-models*)
              (get-local-statistics-integer-value *unmergable-det-models*)
              (get-local-statistics-integer-value *unmergable-partial-models*)))
    (when *tableaux-caching*
      (format stream ";tableaux cache (sat-hits=~D, unsat-hits=~D, ~
                      misses=~D)~%"
              (get-local-statistics-integer-value *tableaux-cache-sat-hits*)
              (get-local-statistics-integer-value *tableaux-cache-unsat-hits*)
              (get-local-statistics-integer-value *tableaux-cache-misses*)))
    (collect-statistics-data)))
