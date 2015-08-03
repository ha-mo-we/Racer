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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-boolean-readers))

(defvar *provisionally-inserted-roles*)

;;;===========================================================================
;;; Conversion of concept terms represented as lists
;;;===========================================================================

(defun make-new-role-store (&optional (tbox nil))
  (if tbox
      (let ((role-axioms-count (max (hash-table-count (tbox-role-axioms-index tbox))
				    (tbox-expected-role-size tbox))))
	(racer-make-hash-table :size role-axioms-count
			       :structure-p t))
    (racer-make-hash-table :structure-p t)))

(defmacro with-new-role-store ((&key (store nil) (tbox nil)) &body body)
  `(let ((*role-store* (or ,store (make-new-role-store ,tbox))))
     ,@body))

(race-inline (get-role (setf get-role)))

(defun get-role (term)
  (gethash term *role-store*))

(defun (setf get-role) (new-value term)
  (setf (gethash term *role-store*) new-value))

;;;===========================================================================

(defun make-predefined-concepts (table
                                 top-concept
                                 bottom-concept
                                 top-datatype-concept
                                 bottom-datatype-concept)
  (when table
    (loop for concept in (list top-concept bottom-concept) do
          (loop for name in (concept-name-set concept) do
                (setf (gethash name table) concept)))
    (setf (gethash +datatype-top-symbol+ table) top-datatype-concept)
    (setf (gethash +datatype-bottom-symbol+ table) bottom-datatype-concept))
  (setf (concept-children-internal top-concept) (list bottom-concept))
  (setf (concept-parents-internal bottom-concept) (list top-concept))
  (setf (concept-negated-concept-internal top-concept) bottom-concept)
  (setf (concept-negated-concept-internal bottom-concept) top-concept)
  (setf (concept-negated-concept-internal top-datatype-concept) bottom-datatype-concept)
  (setf (concept-negated-concept-internal bottom-datatype-concept) top-datatype-concept))

(defun make-new-concept-store (top-concept
                               bottom-concept
                               top-datatype-concept
                               bottom-datatype-concept
                               &optional (tbox nil))
  ;;(declare (ignore rehash-size)) Commented RM Apr. 2014
  (let* ((concept-axioms-count
	  (if tbox
	      (max (hash-table-count (tbox-concept-axioms-index tbox))
		   (tbox-expected-concept-size tbox)
		   100)
	    100))
         (table (racer-make-hash-table :test 'equal
                                       :size  concept-axioms-count
                                       :structure-p t)))
    (make-predefined-concepts table top-concept bottom-concept
                              top-datatype-concept bottom-datatype-concept)
    table))

(race-inline (make-new-tableaux-cache make-new-sat-tableau-cache
                                      make-new-unsat-tableau-cache))

(defun make-new-tableaux-cache ()
  (when *use-equal-tableaux-cache*
    (racer-make-hash-table :test 'equal :structure-p t)))

(defun make-new-sat-tableaux-cache ()
  (when *use-subset-superset-cache*
    (make-cache "SAT")))

(defun make-new-unsat-tableaux-cache ()
  (when *use-subset-superset-cache*
    (make-cache "UNSAT")))

(race-inline (get-equal-cache-model
              get-subset-unsat-cache-model
              get-superset-sat-cache-model))

(defun get-equal-cache-model (list-term cache)
  (gethash list-term cache))

(defun get-subset-unsat-cache-model (list-term cache)
  (find-subset list-term cache))

(defun get-superset-sat-cache-model (list-term cache)
  (when (superset-exists-p list-term cache)
    t))

(defun get-model (list-term)
  (when (and *use-equal-tableaux-cache* *tableaux-cache*)
    (let ((result (get-equal-cache-model list-term *tableaux-cache*)))
      (when result
        (if (consp result)
            (return-from get-model (values-list result))
          (return-from get-model result)))))
  (when *use-subset-superset-cache*
    (when (and *tableaux-sat-caching* *tableaux-cache-sat*)
      (let ((result (get-superset-sat-cache-model list-term *tableaux-cache-sat*)))
        (when result
          (return-from get-model result))))
    (when (and (first *tableaux-unsat-caching*) *tableaux-cache-unsat*)
      (multiple-value-bind (match-p subset-label)
          (get-subset-unsat-cache-model list-term *tableaux-cache-unsat*)
        (when match-p
          (if subset-label
              (values *bottom-concept* subset-label)
            *bottom-concept*))))))

(race-inline (add-equal-cache-sat-model
              add-equal-cache-unsat-model
              add-superset-sat-cache-model
              add-subset-unsat-cache-model))

(defun add-equal-cache-sat-model (list-term cache)
  (incf-statistics *added-tableaux-sat-cache-entries*)
  (setf (gethash list-term cache) t))

(defun add-equal-cache-unsat-model (list-term dependencies cache)
  (incf-statistics *added-tableaux-unsat-cache-entries*)
  (setf (gethash list-term cache) (list *bottom-concept* dependencies)))

(defun add-superset-sat-cache-model (list-term cache)
  (incf-statistics *added-sat-cache-entries*)
  (insert-into-cache list-term cache))

(defun add-subset-unsat-cache-model (list-term dependencies cache)
  (incf-statistics *added-unsat-cache-entries*)
  (insert-into-cache list-term cache dependencies))

(defun add-sat-model (list-term)
  (when (and *use-equal-tableaux-cache* *tableaux-cache*)
    (add-equal-cache-sat-model list-term *tableaux-cache*))
  (when (and *use-subset-superset-cache*
             *tableaux-sat-caching*
             *tableaux-cache-sat*
             (rest list-term)
             (not (every #'cd-concept-p list-term))) ; avoid cd predicates in superset cache !!!
    (add-superset-sat-cache-model list-term *tableaux-cache-sat*)))

(defun add-unsat-model (list-term dependencies)
  (when (and *use-equal-tableaux-cache* *tableaux-cache*)
    (incf-statistics *added-tableaux-unsat-cache-entries*)
    (add-equal-cache-unsat-model list-term dependencies *tableaux-cache*))
  (when (and *use-subset-superset-cache* (first *tableaux-unsat-caching*) *tableaux-cache-unsat*)
    (add-subset-unsat-cache-model list-term dependencies *tableaux-cache-unsat*)))

(defun retract-model (list-term)
  (when (and *use-equal-tableaux-cache* *tableaux-cache*)
    (when (remhash list-term *tableaux-cache*)
      (incf-statistics *retracted-tableaux-cache-entries*)))
  (when (and *use-subset-superset-cache*
             *tableaux-sat-caching*
             *tableaux-cache-sat*
             (rest list-term)
             (not (every #'cd-concept-p list-term))
             (remove-from-cache list-term *tableaux-cache-sat*))
    (incf-statistics *retracted-sat-cache-entries*)))

(defmacro with-racer-structure-id-counter ((&key variable initial-value) &body body)
  (if (null variable)
    `(let ((*structure-id-counter* ,initial-value))
       ,@body)
    (let ((old-structure-id-counter (gensym)))
      `(let ((,old-structure-id-counter (when (boundp '*structure-id-counter*)
                                          (racer-shared-read *structure-id-counter*))))
         (unwind-protect
           (let ((*structure-id-counter* (or ,old-structure-id-counter ,variable)))
             (unwind-protect 
               (progn ,@body)
               (if ,old-structure-id-counter
                 (setf ,old-structure-id-counter (racer-shared-read *structure-id-counter*))
                 (racer-atomic-setf ,variable *structure-id-counter*))))
           (when ,old-structure-id-counter
             (racer-atomic-setf *structure-id-counter* ,old-structure-id-counter)))))))

(defmacro with-new-used-by-concept-store (&body body)
  `(progn
     ,@body))

(defmacro with-new-concept-store ((&key (store nil)
                                            (cache nil)
                                            (tbox nil)
                                            (sat-cache nil)
                                            (unsat-cache nil)
                                            (concrete-domain nil))
                                      &body body)
  `(let* ((*top-concept* (if (null ,tbox)
                           (make-top-concept)
                           (tbox-top-node ,tbox)))
          (*bottom-concept* (if (null ,tbox)
                              (make-bottom-concept)
                              (tbox-bottom-node ,tbox)))
          (*datatype-top-concept* (if (null ,tbox)
                                      (make-top-datatype-concept)
                                    (tbox-datatype-top-node ,tbox)))
          (*datatype-bottom-concept* (if (null ,tbox)
                                         (make-bottom-datatype-concept)
                                       (tbox-datatype-bottom-node ,tbox)))
          (*concept-store*
           (or ,store 
               (if (null ,tbox)
                 (make-new-concept-store *top-concept*
                                         *bottom-concept*
                                         *datatype-top-concept*
                                         *datatype-bottom-concept*)
                 (error "Concept store is missing"))))
          (*tableaux-cache*
           (or ,cache
               (if (or (null ,tbox) (not *use-equal-tableaux-cache*))
                 (make-new-tableaux-cache)
                 (error "Tableaux cache is missing"))))
          (*tableaux-cache-sat*
           (or ,sat-cache
               (if (or (null ,tbox) (not *use-subset-superset-cache*))
                 (make-new-sat-tableaux-cache)
                 (error "Tableaux SAT cache is missing"))))
          (*tableaux-cache-unsat*
           (or ,unsat-cache
               (if (or (null ,tbox) (not *use-subset-superset-cache*))
                 (make-new-unsat-tableaux-cache)
                 (error "Tableaux UNSAT cache is missing"))))
          (*concrete-domain*
           (or ,concrete-domain
               (if (null ,tbox)
                 (create-concrete-domain)
                 (tbox-concrete-domain ,tbox)))))
     ,@body))

(defmacro with-new-ind-counter (&body body)
  `(let ((*ind-counter* +ind-counter-init+))
     ,@body))

(defmacro with-protected-slot-variable ((tbox
                                        var-symbol
                                        accessor
                                        do-not-save-p
                                        &optional
                                        (initial-value nil))
                                       &body body)
  (if (null tbox)
    `(let ((,var-symbol ,initial-value))
       ,@body)
    (let ((old-value-sym (gensym)))
      `(let ((,old-value-sym (when (boundp ',var-symbol)
                               ,var-symbol)))
         (unwind-protect
           (let ((,var-symbol (or ,old-value-sym ,initial-value)))
             (unwind-protect 
               (progn ,@body)
               (if ,old-value-sym
                 (setf ,old-value-sym ,var-symbol)
                 ,(unless do-not-save-p                 
                    `(setf ,accessor ,var-symbol)))))
           ,(unless do-not-save-p
              `(when ,old-value-sym
                 (racer-atomic-setf ,var-symbol ,old-value-sym))))))))

(defmacro with-protected-slot-variables ((tbox arg-list) &body body)
  (if (null tbox)
    `(let (,@(loop for args in arg-list
                   for var-symbol = (first args)
                   for initial-value = (fourth args)
                   collect `(,var-symbol ,initial-value)))
       ,@body)
    (let ((old-value-syms (loop repeat (length arg-list) collect (gensym))))
      `(let (,@(loop for args in arg-list
                     for old-value-sym in old-value-syms
                     for var-symbol = (first args)
                     collect `(,old-value-sym
                               (when (boundp ',var-symbol)
                                 ,var-symbol))))
         (unwind-protect
           (let (,@(loop for args in arg-list
                         for old-value-sym in old-value-syms
                         for var-symbol = (first args)
                         for initial-value = (fourth args)
                         collect `(,var-symbol (or ,old-value-sym ,initial-value))))
             (unwind-protect 
               (progn ,@body)
               (progn
                 ,@(loop for args in arg-list
                         for old-value-sym in old-value-syms
                         for var-symbol = (first args)
                         for accessor = (second args)
                         for do-not-save-p = (third args)
                         collect `(if ,old-value-sym
                                    (setf ,old-value-sym ,var-symbol)
                                    ,(unless do-not-save-p                 
                                       `(setf ,accessor ,var-symbol)))))))
           (progn
             ,@(loop for args in arg-list
                     for old-value-sym in old-value-syms
                     for var-symbol = (first args)
                     for do-not-save-p = (third args)
                     unless do-not-save-p 
                     collect `(when ,old-value-sym
                                (racer-atomic-setf ,var-symbol ,old-value-sym)))))))))

(defmacro with-global-parameters ((&key
                                   (tbox nil)
                                   (stable-set-difference-table '(racer-make-hash-table))
                                   (stable-set-difference-last-list2 nil)
                                   (racer-remove-duplicates-table
                                    '(racer-make-hash-table :test 'equal :structure-p t))
                                   (racer-remove-constraint-duplicates-table
                                    '(racer-make-hash-table :test 'equal))
                                   (possible-subsumees-vector '(make-possible-subsumees-vector))
                                   (expanded-constraints-ind-table '(racer-make-hash-table))
                                   (live-inds-table '(racer-make-hash-table))
                                   (obsolete-inds-table '(racer-make-hash-table))
                                   (label-inds-table '(racer-make-hash-table))
                                   (new-inds-table '(racer-make-hash-table))
                                   (concept-set-mark 0)
                                   (role-set-mark 0)
                                   (individual-set-mark 0)
                                   (constraint-set-mark 0)
                                   (classification-counter 0)
                                   (obsolete-eql-tables '(make-hash-table))
                                   (obsolete-equal-tables '(make-hash-table))
                                   (obsolete-equal-concept-tables '(make-hash-table))
                                   (initial-hash-table-size (hash-table-size (make-hash-table)))
                                   (signatures-equal-table '(racer-make-hash-table :test 'equal))
                                   (partitions-table '(racer-make-hash-table))
                                   (use-less-tbox-memory nil)
                                   (set-vector '(make-set-vector))
                                   (select-disjunct-table '(make-select-disjunct))
                                   )
                                  &body body)
  (let ((tbox-sym (when tbox
                    (gensym))))
    `(let ,(when tbox
             `((,tbox-sym ,tbox)))
       (with-protected-slot-variables
         (,tbox
          ((*stable-set-difference-table*
            ,(when tbox
               `(tbox-stable-set-difference-table ,tbox-sym))
            nil
            ,stable-set-difference-table)
           (*stable-set-difference-last-list2*
            ,(when tbox
               `(tbox-stable-set-difference-last-list2 ,tbox-sym))
            nil
            ,stable-set-difference-last-list2)
           (*racer-remove-duplicates-table*
            ,(when tbox
               `(tbox-racer-remove-duplicates-table ,tbox-sym))
            nil
            ,racer-remove-duplicates-table)
           (*racer-remove-constraint-duplicates-table*
            ,(when tbox
               `(tbox-racer-remove-constraint-duplicates-table ,tbox-sym))
            nil
            ,racer-remove-constraint-duplicates-table)
           (*possible-subsumees*
            ,(when tbox
               `(tbox-possible-subsumees-vector ,tbox-sym))
            t
            ,possible-subsumees-vector)
           (*expanded-constraints-ind-table*
            ,(when tbox
               `(tbox-expanded-constraints-ind-table ,tbox-sym))
            nil
            ,expanded-constraints-ind-table)
           (*live-inds-table*
            ,(when tbox
               `(tbox-live-inds-table ,tbox-sym))
            t
            ,live-inds-table)
           (*obsolete-inds-table*
            ,(when tbox
               `(tbox-obsolete-inds-table ,tbox-sym))
            t
            ,obsolete-inds-table)
           (*label-inds-table*
            ,(when tbox
               `(tbox-label-inds-table ,tbox-sym))
            t
            ,label-inds-table)
           (*new-inds-table*
            ,(when tbox
               `(tbox-new-inds-table ,tbox-sym))
            t
            ,new-inds-table)
           (*concept-set-mark*
            ,(when tbox
               `(tbox-concept-set-mark ,tbox-sym))
            nil
            ,concept-set-mark)
           (*role-set-mark*
            ,(when tbox
               `(tbox-role-set-mark ,tbox-sym))
            nil
            ,role-set-mark)
           (*individual-set-mark*
            ,(when tbox
               `(tbox-individual-set-mark ,tbox-sym))
            nil
            ,individual-set-mark)
           (*constraint-set-mark*
            ,(when tbox
               `(tbox-constraint-set-mark ,tbox-sym))
            nil
            ,constraint-set-mark)
           (*tbox-classification-counter*
            ,(when tbox
               `(tbox-classification-counter ,tbox-sym))
            nil
            ,classification-counter)
           (*obsolete-eql-tables*
            ,(when tbox
               `(tbox-obsolete-eql-tables ,tbox-sym))
            t
            ,obsolete-eql-tables)
           (*obsolete-equal-tables*
            ,(when tbox
               `(tbox-obsolete-equal-tables ,tbox-sym))
            t
            ,obsolete-equal-tables)
           (*obsolete-equal-concept-tables*
            ,(when tbox
               `(tbox-obsolete-equal-concept-tables ,tbox-sym))
            t
            ,obsolete-equal-concept-tables)
           (*initial-hash-table-size*
            ,(when tbox
               `(tbox-initial-hash-table-size ,tbox-sym))
            t
            ,initial-hash-table-size)
           (*signatures-equal-table*
            ,(when tbox
               `(tbox-signatures-equal-table ,tbox-sym))
            t
            ,signatures-equal-table)
           (*partitions-table*
            ,(when tbox
               `(tbox-partitions-table ,tbox-sym))
            t
            ,partitions-table)
           (*use-less-tbox-memory*
            ,(when tbox
               `(tbox-use-less-memory ,tbox-sym))
            t
            ,use-less-tbox-memory)
           (*set-vector*
            ,(when tbox
               `(tbox-set-vector ,tbox-sym))
            t
            ,set-vector)
           (*select-disjunct-table*
            ,(when tbox
               `(tbox-select-disjunct-table ,tbox-sym))
            t
            ,select-disjunct-table)
           ))
         ,@body))))

(defmacro with-alc-environment ((&key (tbox nil)
                                      (concept-store nil)
                                      (tableaux-cache nil)
                                      (tableaux-sat-cache nil)
                                      (tableaux-unsat-cache nil)
                                      (role-store nil)
                                      (id-variable nil)
                                      (id-counter +racer-structure-id-counter-init+)
                                      (concrete-domain nil)
                                      (stable-set-difference-table '(racer-make-hash-table))
                                      (stable-set-difference-last-list2 nil)
                                      (racer-remove-duplicates-table
                                       '(racer-make-hash-table :test 'equal :structure-p t))
                                      (racer-remove-constraint-duplicates-table
                                       '(racer-make-hash-table :test 'equal))
                                      (possible-subsumees-vector '(make-possible-subsumees-vector))
                                      (expanded-constraints-ind-table '(racer-make-hash-table))
                                      (live-inds-table '(racer-make-hash-table))
                                      (obsolete-inds-table '(racer-make-hash-table))
                                      (label-inds-table '(racer-make-hash-table))
                                      (new-inds-table '(racer-make-hash-table))
                                      (concept-set-mark 0)
                                      (role-set-mark 0)
                                      (individual-set-mark 0)
                                      (constraint-set-mark 0)
                                      (classification-counter 0)
                                      (obsolete-eql-tables '(make-hash-table))
                                      (obsolete-equal-tables '(make-hash-table))
                                      (obsolete-equal-concept-tables '(make-hash-table))
                                      (initial-hash-table-size (hash-table-size (make-hash-table)))
                                      (signatures-equal-table '(racer-make-hash-table :test 'equal))
                                      (partitions-table '(racer-make-hash-table))
                                      (use-less-tbox-memory nil)
                                      (set-vector '(make-set-vector))
                                      (select-disjunct-table '(make-select-disjunct))
                                      )
                                &body body)
  `(with-new-ind-counter
     (with-racer-structure-id-counter (
                                       :variable ,id-variable
                                       :initial-value ,id-counter)
       (with-global-parameters (
                                :tbox ,tbox
                                :stable-set-difference-table ,stable-set-difference-table
                                :stable-set-difference-last-list2 ,stable-set-difference-last-list2
                                :racer-remove-duplicates-table ,racer-remove-duplicates-table
                                :racer-remove-constraint-duplicates-table ,racer-remove-constraint-duplicates-table
                                :possible-subsumees-vector ,possible-subsumees-vector
                                :expanded-constraints-ind-table ,expanded-constraints-ind-table
                                :live-inds-table ,live-inds-table
                                :obsolete-inds-table ,obsolete-inds-table
                                :label-inds-table ,label-inds-table
                                :new-inds-table ,new-inds-table
                                :concept-set-mark ,concept-set-mark
                                :role-set-mark ,role-set-mark
                                :individual-set-mark ,individual-set-mark
                                :constraint-set-mark ,constraint-set-mark
                                :classification-counter ,classification-counter
                                :obsolete-eql-tables ,obsolete-eql-tables
                                :obsolete-equal-tables ,obsolete-equal-tables
                                :obsolete-equal-concept-tables ,obsolete-equal-concept-tables
                                :initial-hash-table-size ,initial-hash-table-size
                                :signatures-equal-table ,signatures-equal-table
                                :partitions-table ,partitions-table
                                :use-less-tbox-memory ,use-less-tbox-memory
                                :set-vector ,set-vector
                                :select-disjunct-table ,select-disjunct-table
                                )
         (with-new-concept-store (
                                  :store ,concept-store
                                  :cache ,tableaux-cache
                                  :sat-cache ,tableaux-sat-cache
                                  :unsat-cache ,tableaux-unsat-cache
                                  :tbox ,tbox
                                  :concrete-domain ,concrete-domain)
           (with-new-role-store (
                                 :store ,role-store
                                 :tbox ,tbox)
             (let ((*use-tbox* ,tbox)
                   (*subsumption-tests* 0))
               ,@body)))))))

(race-inline (get-concept (setf get-concept)))

(defun get-concept (list-term)
  (gethash list-term *concept-store*))

;; only for better tracing purposes
#+:debug
 (defun set-concept (new-value list-term)
   (setf (gethash list-term *concept-store*) new-value))

#-:debug
 (defmacro set-concept (new-value list-term)
   `(setf (gethash ,list-term *concept-store*) ,new-value))

(defun (setf get-concept) (new-value list-term)
  (set-concept new-value list-term))

(defun delete-concept (concept)
  #+:debug (assert (and (or-concept-p concept)
                        (not (minusp (concept-reference-counter concept)))))
  ;(break "~S" concept)
  (when (zerop (concept-reference-counter concept))
    (unless *use-less-tbox-memory*
      (delete-concept-internal concept))
    (delete-concept-internal (concept-negated-concept-internal concept))))

#-:debug
(defun delete-concept-internal (concept)
  (if *use-less-tbox-memory*
    (remhash (if (and-concept-p concept)
               `(and . ,(concept-term concept))
               `(and . ,(concept-term (concept-negated-concept-internal concept))))
             *concept-store*)
    (remhash (list* (if (and-concept-p concept)
                      'and
                      'or)
                    (concept-term concept))
             *concept-store*)))

#+:debug
(defun delete-concept-internal (concept)
  (if *use-less-tbox-memory*
    (let* ((key (if (and-concept-p concept)
                  `(and . ,(concept-term concept))
                  `(and . ,(concept-term (concept-negated-concept-internal concept)))))
           (entry (gethash key *concept-store*)))
      (if (if (and-concept-p concept)
            (eq concept entry)
            (eq (concept-negated-concept-internal concept) entry))
        (remhash key *concept-store*)
        (error "Inconsistency in delete-concept-internal: concept=~S, key=~S, entry=~S"
               concept key entry)))
    (let* ((key (list* (if (and-concept-p concept)
                         'and
                         'or)
                       (concept-term concept)))
           (entry (gethash key *concept-store*)))
      (if (eq concept entry)
        (remhash key *concept-store*)
        (error "Inconsistency in delete-concept-internal: concept=~S, key=~S, entry=~S"
               concept key entry)))))

;;;===========================================================================

(defun set-language-to-alcnq (concept)
  #+:debug (or (at-least-concept-p concept) (at-most-concept-p concept))
  (if (eq (concept-term concept) *top-concept*)
    (progn
      (if (role-feature-p (concept-role concept))
        (setf (concept-language concept) (add-dl-features (concept-language concept)))
        (if (at-least-concept-p concept)
          (setf (concept-language concept)
                (add-dl-simple-at-least (concept-language concept)))
          (setf (concept-language concept)
                (add-dl-simple-at-most (concept-language concept))))))
    (progn
      (if (at-least-concept-p concept)
        (setf (concept-language concept)
              (add-dl-at-least (concept-language concept)))
        (setf (concept-language concept)
              (add-dl-at-most (concept-language concept)))))))

(defun set-language-to-alci (concept &optional (term nil))
  (unless (dl-inverse (concept-language concept))
    (let ((role (concept-role concept))
          (alci-found nil))
      (if (inverse-role-in-descendants-compositions-p role)
          (setf (concept-language concept) (add-dl-inverse (concept-language concept)))
        (progn
          (loop with role-ancestors = (remove-top-object-role (role-ancestors-internal role))
                for inv-role-elem in (concept-inverse-roles (or term (concept-term concept)))
                for inv-role = (role-inverse-internal (car inv-role-elem))
                do
                (when inv-role
                  (if (lists-not-disjoint-p role-ancestors
                                            (remove-top-object-role (role-ancestors-internal inv-role)))
                      (setf alci-found t)
                    (let ((inv-role-compositions (role-compositions inv-role)))
                      (when (and inv-role-compositions
                                 (role-interaction-in-compositions-p role-ancestors inv-role-compositions))
                        (setf alci-found t))))
                  (when alci-found
                    (setf (concept-language concept) (add-dl-inverse (concept-language concept)))))
                until alci-found)
          (unless alci-found
            (loop with role-ancestors = (remove-top-object-role (role-ancestors-internal role))
                  with tbox = *use-tbox*
                  with nary-absorption-table = (when tbox
                                                 (tbox-nary-absorption-table tbox))
                  with concepts = (when (and nary-absorption-table (some-concept-p concept))
                                    (let ((term (concept-term concept)))
                                      (if (atomic-concept-p term)
                                          (list term)
                                        (when (or (and-concept-p term) (or-concept-p term))
                                          (concept-term term)))))
                  for inv-role-elem in (concept-inverse-roles concept)
                  for inv-role = (role-inverse-internal (car inv-role-elem))
                  do
                  (when (and inv-role concepts)
                    (setf alci-found
                          (role-interaction-in-unfold-sets-p concepts role-ancestors nary-absorption-table)))
                  until alci-found))
          (when alci-found
            (setf (concept-language concept) (add-dl-inverse (concept-language concept))))))))
  #+:debug 
  (concept-language concept))

(defun role-interaction-in-unfold-sets-p (concepts role-ancestors unfolding-table)
  (loop for concept in concepts
        thereis
        (when (and (atomic-concept-p concept) (not (is-top-concept-p concept)))
          (let ((nary-unfold-sets (concept-nary-unfold-sets concept)))
            (when (consp nary-unfold-sets)
              (loop for unfold-set in nary-unfold-sets
                    for unfolding = (gethash unfold-set unfolding-table)
                    thereis
                    (when (all-concept-p unfolding)
                      (loop for inv-role-elem in (concept-inverse-roles unfolding)
                            for inv-role = (role-inverse-internal (car inv-role-elem))
                            thereis
                            (when inv-role
                              (lists-not-disjoint-p role-ancestors
                                                    (remove-top-object-role
                                                     (role-ancestors-internal inv-role))))))))))))

(defun role-interaction-in-compositions-p (role-ancestors role-compositions)
  (loop for composition in role-compositions
        thereis
        (loop for comp-role in composition
              thereis 
              (lists-not-disjoint-p role-ancestors
                                    (remove-top-object-role
                                     (role-ancestors-internal comp-role))))))

(defun inverse-role-in-compositions-p (compositions)
  (loop for composition in compositions
        thereis
        (loop for (role-1 role-2) on composition
              while role-2
              for role-2-inverse = (role-inverse-internal role-2)
              thereis
              (or (subrole-p role-1 role-2-inverse)
                  (inverse-role-as-end-of-composition-p role-1 role-2-inverse)))))

(defun inverse-role-as-end-of-composition-p (role inverse-role)
  (let ((compositions (role-compositions role)))
    (when compositions
      (loop for composition in compositions
            for end-role = (first (last composition))
            thereis
            (or (subrole-p end-role inverse-role)
                (and (not (eq end-role role))
                     (inverse-role-as-end-of-composition-p end-role inverse-role)))))))

(defun inverse-role-in-descendants-compositions-p (role)
  (if (hash-table-p (role-descendants-internal role))
      (loop for descendant being the hash-key of (role-descendants-internal role)
            for compositions = (role-compositions descendant)
            thereis (and compositions (inverse-role-in-compositions-p compositions)))            
    (loop for descendant in (role-descendants-internal role)
          for compositions = (role-compositions descendant)
          thereis (and compositions (inverse-role-in-compositions-p compositions)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (concept-changed-info
              (:conc-name concept-changed-)
              (:predicate concept-changed-info-p)
              (:constructor make-concept-changed-info (mark &optional (set 0))))
    (mark nil)
    (set 0 :type fixnum)
    )
  
  (defconstant +told-subsumers+ 1)
  (defconstant +told-disjoints+ (ash +told-subsumers+ 1))
  (defconstant +referenced-disjoints+ (ash +told-disjoints+ 1))
  (defconstant +referencing+ (ash +referenced-disjoints+ 1))
  (defconstant +full-referencing+ (ash +referencing+ 1))
  (defconstant +language+ (ash +full-referencing+ 1))
  (defconstant +inverse-roles+ (ash +language+ 1))
  (defconstant +all-changed-features+ (logior +told-subsumers+
                                              +told-disjoints+
                                              +referenced-disjoints+
                                              +referencing+
                                              +full-referencing+
                                              +language+
                                              +inverse-roles+))
  (defconstant +all-features-no-referencing+ (logior +told-subsumers+
                                                     +told-disjoints+
                                                     +referenced-disjoints+
                                                     +full-referencing+
                                                     +language+
                                                     +inverse-roles+))
  (defconstant +all-features-no-full-referencing+ (logior +told-subsumers+
                                                          +told-disjoints+
                                                          +referenced-disjoints+
                                                          +language+
                                                          +inverse-roles+))
  )

(defun concept-changed-told-subsumers (info)
  (not (eql (logand (concept-changed-set info) +told-subsumers+) 0)))

(defun concept-changed-told-disjoints (info)
  (not (eql (logand (concept-changed-set info) +told-disjoints+) 0)))

(defun concept-changed-referencing (info)
  (not (eql (logand (concept-changed-set info) +referencing+) 0)))

(defun concept-changed-full-referencing (info)
  (not (eql (logand (concept-changed-set info) +full-referencing+) 0)))

(defun concept-changed-language (info)
  (not (eql (logand (concept-changed-set info) +language+) 0)))

(defun concept-changed-inverse-roles (info)
  (not (eql (logand (concept-changed-set info) +inverse-roles+) 0)))

(defun concept-changed-referenced-disjoints (info)
  (not (eql (logand (concept-changed-set info) +referenced-disjoints+) 0)))

(defun update-told-subsumers-p (mask)
  (not (eql (logand mask +told-subsumers+) 0)))

(defun update-told-disjoints-p (mask)
  (not (eql (logand mask +told-disjoints+) 0)))

(defun update-referencing-p (mask)
  (not (eql (logand mask +referencing+) 0)))

(defun update-full-referencing-p (mask)
  (not (eql (logand mask +full-referencing+) 0)))

(defun update-language-p (mask)
  (not (eql (logand mask +language+) 0)))

(defun update-inverse-roles-p (mask)
  (not (eql (logand mask +inverse-roles+) 0)))

(defun update-referenced-disjoints-p (mask)
  (not (eql (logand mask +referenced-disjoints+) 0)))

(defmethod print-object ((object concept-changed-info) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~A" (concept-changed-mark object))
    (when (concept-changed-told-subsumers object)
      (format stream " ~S" :told-subsumers))
    (when (concept-changed-told-disjoints object)
      (format stream " ~S" :told-disjoints))
    (when (concept-changed-referenced-disjoints object)
      (format stream " ~S" :referenced-disjoints))
    (when (concept-changed-referencing object)
      (format stream " ~S" :referencing))
    (when (concept-changed-full-referencing object)
      (format stream " ~S" :full-referencing))
    (when (concept-changed-language object)
      (format stream " ~S" :language))
    (when (concept-changed-inverse-roles object)
      (format stream " ~S" :inverse-roles))))

#+:debug
(defun mark-concept-as-changed (concept changed-mark what)
  (setf *concept-changed-p* t)
  (let ((concept-changed (concept-changed-p concept)))
    (if (and (concept-changed-info-p concept-changed)
             (eq (concept-changed-mark concept-changed) changed-mark))
      (setf (concept-changed-set concept-changed)
            (logior (concept-changed-set concept-changed) what))
      (setf (concept-changed-p concept)
            (make-concept-changed-info changed-mark what))))
  (concept-changed-p concept))

#-:debug
(defmacro mark-concept-as-changed (concept changed-mark what)
  (let ((concept-changed-sym (gensym)))
    `(progn
       (setf *concept-changed-p* t)
       (let ((,concept-changed-sym (concept-changed-p ,concept)))
         (if (and (concept-changed-info-p ,concept-changed-sym)
                  (eq (concept-changed-mark ,concept-changed-sym) ,changed-mark))
           (setf (concept-changed-set ,concept-changed-sym)
                 (logior (concept-changed-set ,concept-changed-sym) ,what))
           (setf (concept-changed-p ,concept)
                 (make-concept-changed-info ,changed-mark ,what)))))))

#+:debug
(defun create-inverse-role-list (concept)
  (let ((role (concept-role concept)))
    (unless (or (role-datatype role) (role-cd-attribute role))
      (list (cons role
                  (if (exists-concept-p concept)
                      'some
                    'all))))))

#-:debug
(defmacro create-inverse-role-list (concept)
  (let ((role-sym (gensym)))
    `(let ((,role-sym (concept-role ,concept)))
       (unless (or (role-datatype ,role-sym) (role-cd-attribute ,role-sym))
         (list (cons ,role-sym
                     (if (exists-concept-p ,concept)
                         'some
                       'all)))))))

(race-inline (union-inverse-role-lists))

(defun union-inverse-role-lists (list-1 list-2-or-table)
  (union-equal list-1 list-2-or-table))

(defun union-inverse-role-concepts (concept-list inverse-roles)
  (let ((table (when (> (length concept-list) 50)
                 (smart-clrhash *racer-remove-duplicates-table*
                                nil
                                '*racer-remove-duplicates-table*))))
    (when table
      (loop for elem in inverse-roles
            do (setf (gethash elem table) t)))
    (loop with result = (if table 
                          table
                          inverse-roles)
          for concept in concept-list
          for concept-inverse-roles = (concept-inverse-roles concept)
          when concept-inverse-roles do
          (setf result (union-inverse-role-lists concept-inverse-roles result))
          finally
          (if table
            (loop for elem being the hash-key of result
                  collect elem)
            (return result)))))

(defun set-language (concept &optional (changed-mark t))
  (let* ((old-language (concept-language concept))
         (new-language old-language)
         (old-inverse-roles (concept-inverse-roles concept))
         (type-of-concept (type-of concept)))
    (ecase type-of-concept
      (atomic-concept
       (when *set-language-of-atomic-concepts*
         (when (eq concept *bottom-concept*)
           (setf new-language (add-dl-bottom new-language)))
         (let ((definition (concept-encoded-definition concept)))
           (when definition
             (let* ((negated-concept (concept-negated-concept-internal concept))
                    (old-neg-language (if negated-concept
                                          (concept-language negated-concept)
                                        *dl-empty*))
                    (new-neg-language old-neg-language)
                    (language (concept-language definition))
                    (primitive-p (concept-primitive-p concept)))
               (setf new-language language)
               (unless primitive-p
                 (let ((negated-definition (concept-negated-concept-internal definition)))
                   (when negated-definition
                     (setf new-neg-language (add-dl-not new-neg-language))
                     (when (dl-merging language)
                       (when (dl-simple-number-restrictions language)
                         (setf new-neg-language (add-dl-simple-number-restrictions new-neg-language)))
                       (when (dl-qualified-number-restrictions language)
                         (setf new-neg-language
                               (add-dl-qualified-number-restrictions new-neg-language))))
                     (setf new-neg-language
                           (union-dl-descriptors (concept-language negated-definition)
                                                 new-neg-language))))
                 (setf new-language (add-dl-equivalent-concept-inclusions new-language)))
               (unless (or (null negated-concept)
                           (eq old-neg-language new-neg-language))
                 (mark-concept-as-changed negated-concept
                                          changed-mark
                                          +language+))
               (when negated-concept
                 (setf (concept-language negated-concept) new-neg-language))
               (setf (concept-inverse-roles concept)
                     (union-inverse-role-lists (concept-inverse-roles definition)
                                               (concept-inverse-roles concept))))))
         (let ((nary-unfold-sets (concept-nary-unfold-sets concept)))
           (when (consp nary-unfold-sets)
             #+:debug (assert *use-tbox*)
             (loop with table = (tbox-nary-absorption-table *use-tbox*)
                   for unfold-set in nary-unfold-sets do
                   (setf new-language
                         (union-dl-descriptors
                          (concept-language (gethash unfold-set table))
                          new-language)))))
         (let* ((negated-definition (concept-encoded-negated-definition concept))
                (language (and negated-definition (concept-language negated-definition))))
           (when language
             (unless (eq new-language language)
               (setf new-language (union-dl-descriptors language new-language))
               (let ((negated-concept (concept-negated-concept-internal concept)))
                 (when negated-concept
                   (let ((old-neg-language (concept-language negated-concept)))
                     (unless (eq old-neg-language new-language)
                       (setf (concept-language negated-concept) new-language)
                       (mark-concept-as-changed negated-concept changed-mark +language+))))))
             (setf new-language (add-dl-not new-language))
             (setf (concept-inverse-roles concept)
                   (union-inverse-role-lists (concept-inverse-roles negated-definition)
                                             (concept-inverse-roles concept)))))
         (when (concept-asserted-disjoints concept)
           (loop for disjoint in (concept-asserted-disjoints concept)
                 unless (concept-primitive-p disjoint)
                 do
                 (setf new-language (add-dl-not new-language))
                 (return)
                 finally
                 (setf new-language (add-dl-atomic-not new-language))))))
      ((and-concept or-concept disjoint-and-concept)
       (loop with cd-p = nil
             for elem in (concept-term concept)
             do
             (setf new-language (union-dl-descriptors new-language (concept-language elem)))
             when (and (not cd-p) (general-cd-concept-p elem))
             do (setf cd-p t)
             finally
             (when nil ;cd-p
               (setf new-language (add-dl-full-datatype-concrete-domains new-language)))
             (if (eq type-of-concept 'or-concept)
                 (setf new-language (add-dl-not (add-dl-or new-language)))
               (setf new-language (add-dl-and new-language))))
       (setf (concept-inverse-roles concept)
             (union-inverse-role-concepts (concept-term concept)
                                          (concept-inverse-roles concept))))
      (negated-concept
       (let* ((atomic-concept (concept-term concept))
              (primitive-p (concept-primitive-p atomic-concept)))
         (if (or primitive-p (null (concept-encoded-definition atomic-concept)))
             (progn
               (unless primitive-p
                 (setf new-language (concept-language atomic-concept)))
               (setf new-language (add-dl-atomic-not new-language)))
           (let ((negated-definition (concept-negated-concept-internal
                                      (concept-encoded-definition atomic-concept))))
             (when negated-definition
               (setf new-language (concept-language negated-definition)))
             (setf new-language (add-dl-not new-language))))
         (setf (concept-inverse-roles concept)
               (union-inverse-role-lists (concept-inverse-roles atomic-concept)
                                         (concept-inverse-roles concept)))))
      ((some-concept all-concept)
       (setf new-language (concept-language (concept-term concept)))
       (when (eq (concept-term concept) *bottom-concept*)
         (setf new-language (add-dl-bottom new-language)))
       (let* ((role (concept-role concept))
	      (datatype (role-datatype role)))
         (if (eq type-of-concept 'all-concept)
             (let ((descendants (role-descendants-internal role)))
               (setf new-language (add-dl-all new-language))
               (when (or (role-compositions role)
                         (if (hash-table-p descendants)
                             (loop for descendant being the hash-key of descendants
                                   thereis (role-compositions descendant))
                           (member-if #'role-compositions descendants)))
                 (setf new-language (add-dl-complex-role-inclusions new-language))))
           (progn
             (if (or (eq (concept-term concept) *top-concept*)
                     (eq (concept-term concept) *bottom-concept*))
                 (setf new-language (add-dl-simple-some new-language))
               (setf new-language (add-dl-some new-language)))
             (when (or (role-compositions role)
                       (member-if #'role-compositions (role-ancestors-internal role)))
               (setf new-language (add-dl-complex-role-inclusions new-language)))
             (when (concept-self-reference-p concept)
               (setf new-language (add-dl-locally-reflexive new-language)))))
	 (when (role-feature-p role)
           (setf new-language (add-dl-features new-language)))
         (if datatype
             (if (or (eq datatype t)
                     (and (eq type-of-concept 'all-concept) (not (role-feature-p role))))
                 (setf new-language (add-dl-full-datatype-concrete-domains new-language))
               (let ((transformed-datatype (transform-type datatype)))
                 (if (member (if (symbolp transformed-datatype)
                                 transformed-datatype
                               (first transformed-datatype))
                             '(integer real string boolean))
                     ;; we are here ignoring subranges of integer, etc.
                     (setf new-language (add-dl-simple-datatype-concrete-domains new-language))
                   (setf new-language (add-dl-full-datatype-concrete-domains new-language)))))
           (when (or (role-transitive-p role)
                     (and (eq type-of-concept 'all-concept)
                          (some-user-defined-role-transitive-p (role-descendants-internal role)
                                                               role))
                     (and (eq type-of-concept 'some-concept)
                          (some-user-defined-role-transitive-p (role-ancestors-internal role)
                                                               role)))
             (setf new-language (add-dl-transitive new-language))))
	 (unless datatype
           (setf (concept-inverse-roles concept) (create-inverse-role-list concept))
           (unless (is-bottom-object-role-p role)
             (set-language-to-alci concept)))
	 (setf new-language (union-dl-descriptors new-language (concept-language concept)))))
      ((at-least-concept at-most-concept)
       (setf (concept-inverse-roles concept) (create-inverse-role-list concept))
       (set-language-to-alcnq concept)
       (set-language-to-alci concept)
       (setf new-language
             (union-dl-descriptors (concept-language concept)
                                   (concept-language (concept-term concept))))
       (let ((datatype (role-datatype (concept-role concept))))
         (when datatype
           (if (member datatype '(integer real))
               (setf new-language (add-dl-simple-concrete-domains (concept-language concept)))
             (setf new-language (add-dl-simple-datatype-concrete-domains (concept-language concept)))))))
      ((cd-concept ensure-cd-concept)
       (let* ((cd-p (eq type-of-concept 'cd-concept))
              (predicate
               (if cd-p
                   (concept-predicate concept)
                 (concept-predicate (concept-term concept))))
              (simple-pred-p (eql (predicate-arity predicate) 1))
              (operator (predicate-operator predicate))
	      (attribute-name (first (predicate-parameters predicate)))
              (attribute (get-role attribute-name)))
         (setf new-language (concept-language concept))
         (unless cd-p
           (setf new-language (concept-language (concept-term concept))))
         (if (null attribute)
             (if simple-pred-p
                 (setf new-language (add-dl-simple-concrete-domains new-language))
               (setf new-language (add-dl-full-concrete-domains new-language)))
           (if (and (member attribute-name +internal-roles+)
                    (or (member operator '(top bottom string= equal string<> unequal))
                        (and simple-pred-p 
                             (or (eq operator '<>)
                                 (eq operator '=)
                                 (and (eq (role-cd-attribute attribute) 'integer)
                                      (or (>= (predicate-right-side predicate) 2147483647)
                                          (<= (predicate-right-side predicate) -2147483648)))))))
               (setf new-language (add-dl-simple-datatype-concrete-domains new-language))
             (if (and simple-pred-p
                      (not (member operator '(top bottom string= equal string<> unequal = <>))))
                 (setf new-language (add-dl-simple-concrete-domains new-language))
               (setf new-language (add-dl-full-concrete-domains new-language)))))
         (when (member operator '(<> string<> unequal))
           (setf new-language (add-dl-atomic-not new-language)))
         (when cd-p
           (let ((attribute-domain (concept-attribute-domain concept)))
             (when attribute-domain
               (setf new-language
                     (union-dl-descriptors new-language (concept-language attribute-domain)))))
           (let ((attribute-range (concept-attribute-range concept)))
             (when attribute-range
               (setf new-language
                     (union-dl-descriptors new-language (concept-language attribute-range))))))))
      (general-cd-concept
       (setf new-language (add-dl-simple-datatype-concrete-domains new-language)))
      (ria-initial-concept
       (setf new-language (concept-language (concept-term concept)))
       (setf new-language (add-dl-all new-language))
       (setf new-language (add-dl-complex-role-inclusions new-language))
       (let ((role (concept-role concept)))
         (when (or (role-transitive-p role)
                   (some-user-defined-role-transitive-p (role-descendants-internal role) role))
           (setf new-language (add-dl-transitive new-language)))))
      (neg-ria-initial-concept
       (setf new-language (concept-language (concept-term concept)))
       (setf new-language (add-dl-some new-language))
       (setf new-language (add-dl-complex-role-inclusions new-language))
       (let ((role (concept-role concept)))
         (when (or (role-transitive-p role)
                   (some-user-defined-role-transitive-p (role-ancestors-internal role) role))
           (setf new-language (add-dl-transitive new-language)))))
      ((ria-final-concept neg-ria-final-concept)
       (setf new-language (concept-language (concept-term concept)))
       (setf new-language (add-dl-complex-role-inclusions new-language))))
    (when (quantification-concept-p concept)
      (let* ((role (concept-role concept))
             (datatype (role-datatype role)))
        (unless (or datatype (is-predefined-role-p role))
          (when (role-reflexive-p role)
            (setf new-language (add-dl-reflexive new-language)))
          (when (role-irreflexive-p role)
            (setf new-language (add-dl-irreflexive new-language)))
          (when (role-symmetric-p role)
            (setf new-language (add-dl-symmetric new-language)))
          (when (role-asymmetric-p role)
            (setf new-language (add-dl-asymmetric new-language)))
          (when (role-disjoint-roles role)
            (setf new-language (add-dl-disjoint-roles new-language))))
        (when datatype
          (let* ((term (concept-term concept))
                 (term-list (if (or (and-concept-p term) (or-concept-p term))
                                (concept-term term)
                              (list term)))
                 (full-p (and (not (eq term *top-concept*))
                              (loop for concept in term-list
                                    thereis (and (cd-concept-p concept)
                                                 (not (member (concept-predicate-operator concept)
                                                              '(top bottom string= string<>)))
                                                 (member datatype '(integer cardinal real)))))))
            (when full-p
              (setf new-language (add-dl-full-concrete-domains new-language)))))
        (when (exists-concept-p concept)
          (let ((role-domain (concept-role-domain concept)))
            (when role-domain
              (setf new-language
                    (union-dl-descriptors new-language (concept-language role-domain)))
              (setf (concept-inverse-roles concept)
                    (union-inverse-role-lists (concept-inverse-roles role-domain)
                                              (concept-inverse-roles concept)))))
          (let ((role-range (concept-role-range concept)))
            (when role-range
              (set-language-to-alci concept role-range)
              (setf new-language
                    (union-dl-descriptors new-language (concept-language concept)))
              (setf new-language
                    (union-dl-descriptors new-language (concept-language role-range))))))
        (setf (role-language-context role)
              (union-dl-descriptors new-language (role-language-context role)))))
    (when (and (not (eq old-language new-language))
               (dl-definition-subset-p old-language new-language))
      #+:debug (assert (not (eql (dl-language-set old-language)
                                 (dl-language-set new-language))))
      (setf (concept-language concept) new-language)
      (mark-concept-as-changed concept changed-mark +language+))
    (unless (eql (length old-inverse-roles) (length (concept-inverse-roles concept)))
      (mark-concept-as-changed concept changed-mark +inverse-roles+)))
  #-:debug concept
  #+:debug (values concept (concept-language concept))
  )


(race-inline (maybe-replaced-by-tbox-name))

(defun maybe-replaced-by-tbox-name (encoded-concept)
  (when (and *use-tbox* *taxonomic-encoding*)
    (let ((replacement
           (gethash encoded-concept (tbox-taxonomic-encoding-table *use-tbox*))))
      (when replacement
        (when (boundp '*taxonomic-encoding-dependencies*)
          (push encoded-concept *taxonomic-encoding-dependencies*))
        (incf-statistics *taxonomic-encoding-hits*)
        replacement))))

(defun find-encoded-concept (list-term)
  (maybe-replaced-by-tbox-name (get-concept list-term)))

(defun add-encoded-concept (list-term encoded-concept)
  (setf (get-concept list-term) encoded-concept)
  encoded-concept)

(defun register-new-concept (type-term
                                encoded-term
                                &key
                                (role nil)
                                (number nil)
                                (cd-term nil)
                                (disjoint-and nil)
                                (concept-node nil))
  ;; In case of type-term being equal to cd-predicate the following is important:
  ;; If cd-term is nil, i.e. no parameter value is specified 
  ;; weird effects are caused. Registering another cd concept
  ;; will lead to the encoded concept of a previously registered
  ;; concept due to the same query term (i.e., nil). Hard to find, hence we 
  ;; better check ---- rm.
  #+:debug (when (eq type-term 'cd-predicate)
             (assert (not (null cd-term))))
  (if *use-less-tbox-memory*
    (register-new-concept-2 type-term encoded-term role number cd-term disjoint-and concept-node)
    (register-new-concept-1 type-term encoded-term role number cd-term disjoint-and concept-node)))

(defun register-new-concept-1 (type-term
                                   encoded-term
                                   role
                                   number
                                   cd-term
                                   disjoint-and
                                   concept-node)
  "A new concept term represented as a list of encoded concepts (encoded-term)
is associated with its corresponding representation as structure. Only previous
unknown concepts are registered. A corresponding entry for the negated 
counterpart of the concept is always registered."
  (let* ((query-term (cond ((eq type-term 'symbol) encoded-term)
                           ((eq type-term 'not) `(not ,encoded-term))
                           ((eq type-term 'cd-predicate) cd-term)
                           ((eq type-term 'ensure)
                            (when (null cd-term)
                              (setf cd-term (predicate-definition (concept-predicate encoded-term))))
                            `(ensure ,cd-term))
                           ((eq type-term 'self-reference) `(self-reference ,role))
                           (number (list type-term number role encoded-term))
                           (role (list type-term role encoded-term))
                           (t (cons type-term encoded-term))))
         (old-concept (if (eq type-term 'not)
                        (concept-negated-concept-internal encoded-term)
                        (get-concept query-term)))
         (tbox-name (and old-concept
                         (not (eq old-concept concept-node))
                         (maybe-replaced-by-tbox-name old-concept))))
    (when (and old-concept
               (or-concept-p old-concept)
               (concept-bucket old-concept))
      ;(break "~S" old-concept)
      (incf (concept-reference-counter old-concept)))
    (or tbox-name
        old-concept
        (let ((new-concept (ecase type-term
                             (symbol
                              (if *use-tbox*
                                (or (ensure-atomic-concept-in-tbox *use-tbox*
                                                                   encoded-term)
                                    (make-atomic-concept :term encoded-term))
                                (make-atomic-concept :term encoded-term
                                                     :primitive-p t)))
                             (not (make-negated-concept encoded-term))
                             (and (if disjoint-and
                                    (make-disjoint-and-concept encoded-term)
                                    (make-and-concept encoded-term)))
                             (or (make-or-concept encoded-term))
                             ((some d-some)
                              (make-some-concept role encoded-term))
                             (i-some (make-neg-ria-initial-concept role encoded-term))
                             (f-some (make-neg-ria-final-concept role encoded-term))
                             ((at-least d-at-least)
                              (make-at-least-concept number role encoded-term))
                             (self-reference
                              (let ((concept (make-some-concept role *top-concept*)))
                                (setf (concept-self-reference-p concept) t)
                                concept))
                             (cd-predicate (make-cd-concept encoded-term))
                             (ensure (make-ensure-cd-concept encoded-term)))))
          (set-language new-concept)
          (setf (get-concept query-term) new-concept)
          (register-new-negated-concept-1 (negate-concept new-concept t)
                                          new-concept
                                          query-term)
          new-concept))))

(defun register-new-negated-concept-1 (negated-concept new-concept query-term)
  (setf (get-concept (negate-concept-term query-term)) negated-concept)
  (if (negated-concept-p new-concept)
    (progn
      (setf (concept-told-disjoints new-concept) (list negated-concept))
      (setf (concept-referenced-disjoints new-concept) (list negated-concept)))
    (when (atomic-concept-p new-concept)
      (setf (concept-told-subsumers new-concept) (list new-concept))
      (setf (concept-told-disjoints negated-concept) (list new-concept))
      (setf (concept-referenced-disjoints negated-concept) (list new-concept)))))

(defun register-new-concept-2 (type-term
                                   encoded-term
                                   role
                                   number
                                   cd-term
                                   disjoint-and
                                   concept-node)
  "A new concept term represented as a list of encoded concepts (encoded-term)
is associated with its corresponding representation as structure. Only previous
unknown concepts are registered. A corresponding entry for the negated 
counterpart of the concept is always registered."
  (let* ((negated-p nil)
         (query-term
          (cond ((eq type-term 'symbol) encoded-term)
                ((eq type-term 'not)
                 (setf negated-p t)
                 encoded-term)
                ((eq type-term 'cd-predicate)
                 (when (member (predicate-operator encoded-term) '(bottom string<> unequal <>))
                   (setf negated-p t))
                 cd-term)
                ((eq type-term 'ensure)
                 (when (null cd-term)
                   (setf cd-term (predicate-definition (concept-predicate encoded-term))))
                 `(ensure ,cd-term))
                ((eq type-term 'self-reference) `(self-reference ,role))
                (number
                 (if (and (eq type-term 'at-least) (eq type-term 'd-at-least))
                   (list type-term number role encoded-term)
                   (progn
                     (setf negated-p t)
                     (negate-concept-term (list type-term number role encoded-term)))))
                (role
                 (if (or (eq type-term 'some) (eq type-term 'd-some))
                   (list type-term role encoded-term)
                   (progn
                     (setf negated-p t)
                     (negate-concept-term (list type-term role encoded-term)))))
                (t
                 (if (eq type-term 'and)
                   (cons type-term encoded-term)
                   (progn
                     #+:debug (assert (eq type-term 'or))
                     (setf negated-p t)
                     (negate-concept-term (cons type-term encoded-term)))))))
         (old-concept (if (eq type-term 'not)
                        (concept-negated-concept-internal encoded-term)
                        (let ((entry (get-concept query-term)))
                          (if (and entry negated-p)
                            (concept-negated-concept-internal entry)
                            entry))))
         (tbox-name (and old-concept
                         (not (eq old-concept concept-node))
                         (maybe-replaced-by-tbox-name old-concept))))
    (when (and old-concept
               (or-concept-p old-concept)
               (concept-bucket old-concept))
      ;(break "~S" old-concept)
      (incf (concept-reference-counter old-concept)))
    (or tbox-name
        old-concept
        (let ((new-concept (ecase type-term
                             (symbol
                              (if *use-tbox*
                                  (or (ensure-atomic-concept-in-tbox *use-tbox*
                                                                     encoded-term)
                                      (make-atomic-concept :term encoded-term))
                                (make-atomic-concept :term encoded-term
                                                     :primitive-p t)))
                             (not (make-negated-concept encoded-term))
                             (and (if disjoint-and
                                      (make-disjoint-and-concept encoded-term)
                                    (make-and-concept encoded-term)))
                             (or (make-or-concept encoded-term))
                             ((some d-some)
                              (make-some-concept role encoded-term))
                             (i-some (make-neg-ria-initial-concept role encoded-term))
                             (f-some (make-neg-ria-final-concept role encoded-term))
                             ((at-least d-at-least)
                              (make-at-least-concept number role encoded-term))
                             (self-reference
                              (let ((concept (make-some-concept role *top-concept*)))
                                (setf (concept-self-reference-p concept) t)
                                concept))
                             (cd-predicate (make-cd-concept encoded-term))
                             (ensure (make-ensure-cd-concept encoded-term)))))
          (set-language new-concept)
          (register-new-negated-concept-2 negated-p
                                          (when negated-p
                                            (negate-concept new-concept t))
                                          new-concept
                                          query-term)
          new-concept))))

(defun register-new-negated-concept-2 (negated-p negated-concept new-concept query-term)
  (if negated-p
    (setf (get-concept query-term) negated-concept)
    (setf (get-concept query-term) new-concept))
  (if (negated-concept-p new-concept)
    (progn
      (setf (concept-told-disjoints new-concept) (list negated-concept))
      (setf (concept-referenced-disjoints new-concept) (list negated-concept)))
    (when (atomic-concept-p new-concept)
      (unless (concept-told-subsumers new-concept)
        (setf (concept-told-subsumers new-concept) (list new-concept)))
      (when negated-concept
        (setf (concept-told-disjoints negated-concept) (list new-concept))
        (setf (concept-referenced-disjoints negated-concept) (list new-concept))))))

(defun create-negated-concept (concept)
  #+:debug (assert *use-less-tbox-memory*)
  (let ((negated-concept (negate-concept concept t))
        (negated-p (or (or-concept-p concept)
                       (negated-concept-p concept)
                       (all-concept-p concept)
                       (at-most-concept-p concept)
                       (and (cd-concept-p concept)
                            (member (predicate-operator (concept-predicate concept))
                                    '(bottom string<> unequal <>))
                            t))))
    (register-new-negated-concept-2 negated-p
                                    negated-concept
                                    concept
                                    (decode-concept-1 concept nil t nil nil))
    negated-concept))

(let (#+:lispworks
      (lw:*handle-warn-on-redefinition* nil)
      #+:lispworks
      (lw:*redefinition-action* nil)
      #+:allegro
      (excl:*redefinition-warnings* nil)
      #+:ccl
      (ccl:*warn-if-redefine* nil)
      )
  ;;; redefine forward reference from module concept-structures
  (setf (symbol-function 'concept-negated-concept)
        (lambda (concept)
          (or (concept-negated-concept-internal concept)
              (create-negated-concept concept)))))

(defun negate-concept (concept link-with-negation-p)
  "Create negation of 'concept' provided it is still unknown"
  (or (concept-negated-concept-internal concept)
      (let
        ((use-less-tbox-memory *use-less-tbox-memory*)
         (negated-concept
          (cond
           ((atomic-concept-p concept) (make-negated-concept concept))
           ((negated-concept-p concept) (concept-term concept))
           ((and-concept-p concept) (make-or-concept (mapcar (lambda (concept)
                                                               (negate-concept concept
                                                                               link-with-negation-p))
                                                             (concept-term concept))))
           ((or-concept-p concept) (make-and-concept (mapcar (lambda (concept)
                                                               (negate-concept concept
                                                                               link-with-negation-p))
                                                             (concept-term concept))))
           ((some-concept-p concept)
            (let ((new-concept (make-all-concept (concept-role concept)
                                                 (negate-concept (concept-term concept)
                                                                 link-with-negation-p))))
              (when (concept-self-reference-p concept)
                (setf (concept-self-reference-p new-concept) t))
              new-concept))
           ((all-concept-p concept)
            (let ((new-concept (make-some-concept (concept-role concept)
                                                  (negate-concept (concept-term concept)
                                                                  link-with-negation-p))))
              (when (concept-self-reference-p concept)
                (setf (concept-self-reference-p new-concept) t))
              new-concept))
           ((at-least-concept-p concept) 
            (let ((number (concept-number-restriction concept)))
              (if (zerop number)
                  *bottom-concept*
                (make-at-most-concept (1- number)
                                      (concept-role concept)
                                      (concept-term concept)))))
           ((at-most-concept-p concept) 
            (make-at-least-concept (1+ (concept-number-restriction concept))
                                   (concept-role concept)
                                   (concept-term concept)))
           ((cd-concept-p concept)
            (let* ((predicate (concept-predicate concept))
                   (operator (predicate-operator predicate)))
              (if (or (eq operator 'top) (eq operator 'bottom))
                  (make-cd-concept (predicate-negation predicate))
                (let ((cd-concept (make-cd-concept (predicate-negation predicate))))
                  (set-language cd-concept)
                  (make-ensure-cd-concept cd-concept)))))
           ((ensure-cd-concept-p concept)
            (let* ((cd-concept (concept-term concept))
                   (operator (predicate-operator (concept-predicate cd-concept))))
              (if (or (eq operator 'top) (eq operator 'bottom))
                  (negate-concept cd-concept link-with-negation-p)
                (concept-term (negate-concept cd-concept link-with-negation-p)))))
           ((ria-initial-concept-p concept)
            (make-neg-ria-initial-concept (concept-role concept) (negate-concept (concept-term concept)
                                                                                 link-with-negation-p)))
           ((neg-ria-initial-concept-p concept)
            (make-ria-initial-concept (concept-role concept) (negate-concept (concept-term concept)
                                                                             link-with-negation-p)))
           ((ria-final-concept-p concept)
            (make-neg-ria-final-concept (concept-role concept) (negate-concept (concept-term concept)
                                                                                link-with-negation-p)))
           ((neg-ria-final-concept-p concept)
            (make-ria-final-concept (concept-role concept) (negate-concept (concept-term concept)
                                                                           link-with-negation-p)))
           (t (error "~S unexpected" concept)))))
        (when (and link-with-negation-p
                   (or (not use-less-tbox-memory)
                       (and (null (concept-negated-concept-internal concept))
                            (null (concept-negated-concept-internal negated-concept)))))
          (setf (concept-negated-concept-internal concept) negated-concept)
          (setf (concept-negated-concept-internal negated-concept) concept))
        (when (and use-less-tbox-memory
                   (atomic-concept-p concept)
                   (not (concept-primitive-p concept))
                   (concept-encoded-definition concept))
          (unless (concept-negated-concept-internal (concept-encoded-definition concept))
            (create-negated-concept (concept-encoded-definition concept))
            (set-language concept)))
        (set-language negated-concept)
        (or (concept-negated-concept-internal concept)
            negated-concept))))

(defun decode-transform-quantifier-specification (quantification-concept)
  (let ((role (concept-role quantification-concept)))
    (if (role-datatype role)
        (let ((decoded-quantification-concept (decode-concept quantification-concept)))
          #+:debug (assert (or (some-concept-p quantification-concept)
                               (all-concept-p quantification-concept)))
          (if (some-concept-p quantification-concept)
              (transform-data-specification (third decoded-quantification-concept) 'some)
            (transform-data-specification (third decoded-quantification-concept) 'all)))
      (decode-concept (concept-term quantification-concept)))))

(defun decode-concept (concept &optional (unfold nil) (internal-roles-p nil))
  "Create list representation (symbols) of an encoded concept"
  (decode-concept-1 concept unfold internal-roles-p nil nil))

(defun decode-concept-1 (concept unfold internal-roles-p unused-1 unused-2)
  (declare (ignore unused-1 unused-2))
  (if (concept-p-internal concept)
      (or (and (quantification-concept-p concept)
               (concept-decoded-list-term concept))
          (let ((term (concept-term concept)))
            (etypecase concept
              (atomic-concept
               (if (and unfold (not (null (concept-encoded-definition concept))))
                   (if (concept-primitive-p concept)
                       `(and ,term
                             ,(decode-concept-1 (concept-encoded-definition concept) t internal-roles-p nil nil))
                     (decode-concept-1 (concept-encoded-definition concept) t internal-roles-p nil nil))
                 term))
              (negated-concept `(not ,(decode-concept-1 term unfold internal-roles-p nil nil)))
              (and-concept 
               `(and ,@(loop for concept in term
                             collect (decode-concept-1 concept unfold internal-roles-p nil nil))))
              (or-concept `(or ,@(loop for concept in term
                                       collect (decode-concept-1 concept unfold internal-roles-p nil nil))))
              (some-concept 
               (let* ((role (concept-role concept))
                      (datatype (transform-type (role-datatype role))))
                 (if (and datatype (not internal-roles-p))
                     (let ((predicate (decode-concept-1 (concept-term concept) internal-roles-p nil nil nil)))
                       (cond ((or (eq predicate +top-symbol+)
                                  (and (consp predicate) (member (first predicate) '(a an))))
                              `(a ,(decode-role role)))
                             ((symbolp predicate)
                              predicate)  ; actually no predicate, i.e. KB is inconsistent
                             ((consp predicate)
                              (case (first predicate)
                                (equal `(equal ,(decode-role role) ,(third predicate)))
                                (= `(= ,(decode-role role) ,(third predicate)))
                                (string=
                                 (if (eq datatype 'string)
                                     `(string= ,(decode-role role) ,(third predicate))
                                   `(boolean= ,(decode-role role)
                                              ,(cond
                                                ((string= (third predicate) "true") *true*)
                                                ((string= (third predicate) "false") *false*)
                                                (t (error "confusion in decoding datatype ~S of ~S"
                                                          datatype concept))))))
                                (t ;;(break) 
                                   predicate)))
                             (t predicate)))
                   (if (concept-self-reference-p concept)
                       `(self-reference ,(decode-role (concept-role concept)))
                     (if (or datatype (role-cd-attribute role))
                         `(d-some ,(decode-role (concept-role concept))
                                  ,(decode-concept-1 term unfold internal-roles-p nil nil))
                       `(some ,(decode-role (concept-role concept))
                              ,(decode-concept-1 term unfold internal-roles-p nil nil)))))))
              (neg-ria-initial-concept
               `(i-some ,(decode-role (concept-role concept))
                        ,(decode-concept-1 term unfold internal-roles-p nil nil)))
              (neg-ria-final-concept
               `(f-some ,(decode-role (concept-role concept))
                        ,(decode-concept-1 term unfold internal-roles-p nil nil)))
              (all-concept
               (let ((role (concept-role concept)))
                 (if (concept-self-reference-p concept)
                     `(not (self-reference ,(decode-role role)))
                   (if (or (role-datatype role) (role-cd-attribute role))
                       `(d-all ,(decode-role (concept-role concept))
                               ,(decode-concept-1 term unfold internal-roles-p nil nil))
                     `(all ,(decode-role role)
                           ,(decode-concept-1 term unfold internal-roles-p nil nil))))))
              (ria-initial-concept
               `(i-all ,(decode-role (concept-role concept))
                        ,(decode-concept-1 term unfold internal-roles-p nil nil)))
              (ria-final-concept
               `(f-all ,(decode-role (concept-role concept))
                        ,(decode-concept-1 term unfold internal-roles-p nil nil)))
              (at-least-concept
               (let ((qualification (decode-concept-1 term unfold internal-roles-p nil nil))
                     (role (concept-role concept)))
                 (if (or (role-datatype role) (role-cd-attribute role))
                     `(d-at-least ,(concept-number-restriction concept)
                                  ,(decode-role role)
                                  ,qualification)
                   (if (eq qualification +top-symbol+)
                       `(at-least ,(concept-number-restriction concept)
                                  ,(decode-role role))
                     `(at-least ,(concept-number-restriction concept)
                                ,(decode-role role)
                                ,qualification)))))
              (at-most-concept
               (let ((qualification (decode-concept-1 term unfold internal-roles-p nil nil))
                     (role (concept-role concept)))
                 #+:debug (assert (not (zerop (concept-number-restriction concept))))
                 (if (or (role-datatype role) (role-cd-attribute role))
                     `(d-at-most ,(concept-number-restriction concept)
                                 ,(decode-role role)
                                 ,qualification)
                   (if (eq qualification +top-symbol+)
                       `(at-most ,(concept-number-restriction concept)
                                 ,(decode-role role))
                     `(at-most ,(concept-number-restriction concept)
                               ,(decode-role role)
                               ,qualification)))))
              (cd-concept
               (let ((predicate (concept-predicate concept)))
                 (or (predicate-definition predicate)
                     (negate-predicate-term (predicate-definition (predicate-negation predicate)))
                     ;;`(not ,(predicate-definition (predicate-negation predicate)))
                     )))
              (ensure-cd-concept `(ensure ,(decode-concept (concept-term concept)
                                                           unfold
                                                           internal-roles-p)))
              (general-cd-concept
               (cond ((is-top-datatype-concept-p concept) +datatype-top-symbol+)
                     ((is-bottom-datatype-concept-p concept) +datatype-bottom-symbol+)
                     (t (error "cannot decode ~S" concept)))))))
    concept))

;;;===========================================================================

(defun negate-predicate-term (list-term)
  (if (cd-concept-p list-term)
    (predicate-negation (concept-predicate list-term))
    (let ((op (first list-term)))
      (cond 
       ((eq op 'min) `(max ,(second list-term) ,(1- (third list-term))))
       ((eq op 'max) `(min ,(second list-term) ,(1+ (third list-term))))
       (t (list* (negated-cd-operator (first list-term)) (rest list-term)))))))

(defun negate-concept-term (list-term)
  "Negate a concept term represented as a list of encoded concepts"
  (cond
   ((symbolp list-term)
    (cond ((eq list-term +top-symbol+) +bottom-symbol+)
          ((eq list-term +bottom-symbol+) +top-symbol+)
          ((eq list-term +datatype-top-symbol+) +datatype-bottom-symbol+)
          ((eq list-term +datatype-bottom-symbol+) +datatype-top-symbol+)
          (t `(not ,list-term))))
   ((and (concept-p-internal list-term)
         (or (concept-negated-concept-internal list-term)
             *use-less-tbox-memory*))
    (or (concept-negated-concept-internal list-term)
        (when *use-less-tbox-memory*
          (negate-concept-term (decode-concept-1 list-term nil t nil nil)))))
   ((predicate-p list-term)
    (predicate-negation list-term))
   (t (ecase (first list-term)
        (not (let ((term (second list-term)))
               (if (or (symbolp term)
                       (and (consp (second list-term))
                            (eq (first (second list-term)) 'self-reference)))
                   term
                 (negate-concept-term term))))
        (and `(or ,@(mapcar #'negate-concept-term (rest list-term))))
        ((or one-of) `(and ,@(mapcar #'negate-concept-term (rest list-term))))
        (some `(all ,(second list-term) ,(negate-concept-term (third list-term))))
        (d-some `(d-all ,(second list-term) ,(negate-concept-term (third list-term))))
        (all `(some ,(second list-term) ,(negate-concept-term (third list-term))))
        (d-all `(d-some ,(second list-term) ,(negate-concept-term (third list-term))))
        (at-least
         (let ((qualification (fourth list-term)))
           (if qualification
               `(at-most ,(1- (second list-term)) ,(third list-term) ,qualification)
             `(at-most ,(1- (second list-term)) ,(third list-term)))))
        (d-at-least
         (let ((qualification (fourth list-term)))
           (if qualification
               `(d-at-most ,(1- (second list-term)) ,(third list-term) ,qualification)
             `(d-at-most ,(1- (second list-term)) ,(third list-term)))))
        (at-most
         (let ((qualification (fourth list-term)))
           (if qualification
               `(at-least ,(1+ (second list-term)) ,(third list-term) ,qualification)
             `(at-least ,(1+ (second list-term)) ,(third list-term)))))
        (d-at-most
         (let ((qualification (fourth list-term)))
           (if qualification
               `(d-at-least ,(1+ (second list-term)) ,(third list-term) ,qualification)
             `(d-at-least ,(1+ (second list-term)) ,(third list-term)))))
        (self-reference
         `(not ,list-term))
        (no `(a ,(second list-term)))
        ((a an) `(no ,(second list-term)))
        ((> >= < <= min max = equal <> unequal string= string<> boolean= boolean<> divisible not-divisible) 
         `(ensure ,(negate-predicate-term list-term)))
        (ensure (negate-concept-term (second list-term)))
        (i-all `(i-some ,(second list-term) ,(negate-concept-term (third list-term))))
        (i-some `(i-all ,(second list-term) ,(negate-concept-term (third list-term))))
        (f-all `(f-some ,(second list-term) ,(negate-concept-term (third list-term))))
        (f-some `(f-all ,(second list-term) ,(negate-concept-term (third list-term))))))))

(defun negate-ensure-term (list-term)
  (if (cd-concept-p list-term)
      (concept-term (concept-negated-concept-internal list-term))
    (let ((op (first list-term)))
      (cond 
       ((eq op 'min) `(max ,(second list-term) ,(1- (third list-term))))
       ((eq op 'max) `(min ,(second list-term) ,(1+ (third list-term))))
       (t (list* (negated-cd-operator (first list-term)) (rest list-term)))))))

(defun get-negated-concept (concept)
  (cond ((concept-p-internal concept)
         (concept-negated-concept concept))
        #+:debug ((not (consp concept)) (error "unexpected"))
        ((eq (first concept) 'and)
         `(or ,@(mapcar (lambda (concept)
                          (negate-concept concept nil))
                        (rest concept))))
        ((eq (first concept) 'or)
         `(and ,@(mapcar (lambda (concept)
                           (negate-concept concept nil))
                         (rest concept))))
        (t (error "unexpected"))))

(race-inline (sort-concept-list reverse-sort-concept-list))

(defun sort-concept-list (list)
  (if (rest list)
    (sort list #'> :key #'concept-hash-id)
    list))

(defun reverse-sort-concept-list (list)
  (if (rest list)
    (sort list #'<= :key #'concept-hash-id)
    list))

(defun sort-role-list (list)
  (if (rest list)
    (sort list #'< :key #'role-hash-id)
    list))

(defun reverse-sort-role-list (list)
  (if (rest list)
    (sort list #'>= :key #'role-hash-id)
    list))

#| ;; used for debugging
(defun encode-concept-term (list-term &optional
                                      (concept-node nil)
                                      (only-sorted nil)
                                      (reverse-sorted nil))
  (let ((result (encode-concept-term-x list-term concept-node only-sorted reverse-sorted))
        (clist (loop for concept being the hash-value of *concept-store* collect concept)))
    (loop for concept-list on clist
          for concept-1 = (first concept-list)
          do
          (when (and-concept-p concept-1)
            (loop for concept-2 in (rest concept-list)
                  do
                  (when (and-concept-p concept-2)
                    (let ((term-1 (mapcar #'decode-concept (concept-term concept-1)))
                          (term-2 (mapcar #'decode-concept (concept-term concept-2))))
                      (when (and (not (equal term-1 term-2))
                                 (subsetp term-1 term-2 :test 'equal)
                                 (subsetp term-2 term-1 :test 'equal))
                        (break "error")))))))
    result))
|#

(defun encode-concept-term (list-term &optional
                                          (concept-node nil)
                                          (only-sorted nil)
                                          (reverse-sorted nil))
  (let* ((use-less-tbox-memory *use-less-tbox-memory*)
         (reencode-p (or only-sorted
                         reverse-sorted
                         concept-node
                         (concept-p-internal list-term)
                         *enforce-reencoding*
                         *flatten-encoded-concepts*)))
    (or (unless reencode-p
          (let ((result (find-encoded-concept list-term)))
            (when result
              (when (and (or-concept-p result)
                         (concept-bucket result))
                (incf (concept-reference-counter result)))
              result)))
        (let ((result (encode-concept-term-1 list-term)))
          (cond
           ((concept-p-internal result)
            (if (or (concept-p-internal list-term) use-less-tbox-memory)
              result
              (add-encoded-concept list-term result)))
           #+:debug
           ((not (consp result)) (error "expected list: ~S" result))
           ((eq (first result) 'or)
            (when concept-node
              (setf result (remove concept-node result)))
            (if only-sorted
              (if reverse-sorted
                (reverse-sort-concept-list (rest result))
                (sort-concept-list (rest result)))
              (progn
                (when (and concept-node (not (rest result)))
                  (if use-less-tbox-memory
                    (return-from encode-concept-term (first result))
                    (return-from encode-concept-term
                      (add-encoded-concept list-term (first result)))))
                (if use-less-tbox-memory
                  (set-told-subsumer-of-or
                   (set-language
                    (register-new-concept 'or
                                          (sort-concept-list (rest result))
                                          :concept-node concept-node))
                   concept-node)
                  (add-encoded-concept
                   list-term
                   (set-told-subsumer-of-or
                    (set-language
                     (register-new-concept 'or
                                           (sort-concept-list (rest result))
                                           :concept-node concept-node))
                    concept-node))))))
           ((eq (first result) 'and)
            (when concept-node
              (setf result (remove concept-node result)))
            (if only-sorted
              (if reverse-sorted
                (reverse-sort-concept-list (rest result))
                (sort-concept-list (rest result)))
              (progn
                (when (and concept-node (not (rest result)))
                  (if use-less-tbox-memory
                    (return-from encode-concept-term (first result))
                    (return-from encode-concept-term
                      (add-encoded-concept list-term (first result)))))
                (if use-less-tbox-memory
                  (set-told-subsumer-of-and
                   (set-language
                    (register-new-concept 'and
                                          (sort-concept-list (rest result))
                                          :concept-node concept-node)))
                  (add-encoded-concept
                   list-term
                   (set-told-subsumer-of-and
                    (set-language
                     (register-new-concept 'and
                                           (sort-concept-list (rest result))
                                           :concept-node concept-node))))))))
           (t (error "unexpected")))))))

(defun update-told-info (concept)
  (let ((subsumers (concept-told-subsumers concept))
        (disjoints (concept-told-disjoints concept))
        (referenced-disjoints (concept-referenced-disjoints concept)))
    (if (and-concept-p concept)
        (set-told-subsumer-of-and concept)
      (set-told-subsumer-of-or concept nil))
    (setf (concept-told-subsumers concept) (concept-set-union subsumers (concept-told-subsumers concept)))
    (setf (concept-told-disjoints concept) (concept-set-union disjoints (concept-told-disjoints concept)))
    (setf (concept-referenced-disjoints concept)
          (concept-set-union referenced-disjoints (concept-referenced-disjoints concept)))))

(defun set-told-subsumer-of-and (and-concept)
  (if (and-concept-p and-concept)
    (loop for concept in (concept-term and-concept)
          append (concept-told-subsumers concept) into subsumers
          append (concept-told-disjoints concept) into disjoints
          append (concept-referenced-disjoints concept) into referenced-disjoints
          finally
          (setf (concept-told-subsumers and-concept) (concept-set-remove-duplicates subsumers))
          (setf (concept-told-disjoints and-concept) (concept-set-remove-duplicates disjoints))
          (setf (concept-referenced-disjoints and-concept)
                (concept-set-remove-duplicates referenced-disjoints))
          (return and-concept))
    and-concept))

(defun set-told-subsumer-of-or (or-concept or-concept-node)
  (if (or-concept-p or-concept)
      (loop with referenced-disjoints = nil
            with add-or-concept = (and or-concept-node
                                       (not (concept-primitive-p or-concept-node)))
            for concept in (concept-term or-concept)
            for concept-subsumers = (concept-told-subsumers concept)
            for concept-disjoints = (concept-told-disjoints concept)
            for subsumers = concept-subsumers then subsumers
            for disjoints = concept-disjoints then disjoints
            do
            (unless (eq concept-subsumers subsumers)
              (setf subsumers (concept-set-intersection concept-subsumers subsumers)))
            (unless (eq concept-disjoints disjoints)
              (setf disjoints (concept-set-intersection concept-disjoints disjoints)))
            (setf referenced-disjoints (concept-set-union (concept-referenced-disjoints concept)
                                                          referenced-disjoints))
            (when add-or-concept
              (pushnew or-concept-node (concept-told-subsumers concept)))
            finally
            (setf (concept-told-subsumers or-concept) subsumers)
            (setf (concept-told-disjoints or-concept) disjoints)
            (setf (concept-referenced-disjoints or-concept) referenced-disjoints)
            #+:debug (return (values or-concept subsumers disjoints))
            #-:debug (return or-concept))
    or-concept))

(declaim (notinline prepend-prefix))

(defun prepend-prefix (concept-name tbox)
  (declare (ignore tbox))
  concept-name)

(defparameter *concept-operator-table*
  (let ((table (make-hash-table))
        (symbols '(and or some all not at-least at-most exactly 
                       nominal one-of self-reference disjoint-and
                       has-value d-filler d-all d-some d-at-least
                       d-at-most d-exactly)))
    (loop for symbol in symbols
          do (setf (gethash symbol table) t))
    table))

(defun encode-concept-term-1 (list-term)
  "Recursively encode, normalize, and simplify a concept term represented
as a list of symbols"
  (let ((result
         (cond
          ((concept-p-internal list-term)
           (if (and *simplify-redundant-at-most*
                    (quantification-concept-p list-term)
                    (role-feature-p (concept-role list-term)))
               (cond ((at-most-concept-p list-term) *top-concept*)
                     ((at-least-concept-p list-term) *bottom-concept*)
                     (t list-term))
             list-term))
          ((symbolp list-term)
           (cond ((eq list-term +top-symbol+) *top-concept*)
                 ((eq list-term +bottom-symbol+) *bottom-concept*)
                 ((eq list-term +datatype-top-symbol+) *datatype-top-concept*)
                 ((eq list-term +datatype-bottom-symbol+) *datatype-bottom-concept*)
                 (t 
                  ;;(unless (gethash list-term (tbox-nominal-table *current-tbox*))
                  ;;  (setf (gethash list-term (tbox-nominal-table *current-tbox*)) :concept))
                  (register-new-concept 'symbol (prepend-prefix list-term *use-tbox*)))))
          (t (case (first list-term)
               (and (encode-and-term list-term nil))
               (or (encode-or-term list-term))
               (nominal 
                (let ((ind (second list-term)))
                  (cond ((eq ind +top-symbol+) *top-concept*)
                        ((eq ind +bottom-symbol+) *bottom-concept*)
                        (t 
                         ;;(unless (gethash ind (tbox-nominal-table *current-tbox*))
                         (setf (gethash ind (tbox-nominal-table *current-tbox*)) :nominal)
                         (register-new-concept 'symbol (prepend-prefix ind *use-tbox*))))))
               (one-of
                (let ((current-tbox *use-tbox*))
                  #+:ignore
                  (when (and *current-abox* current-tbox (eq (tbox *current-abox*) current-tbox))
                    (loop for ind in (rest list-term) do
                          (add-concept-assertion *current-abox* ind ind)))
                  (when current-tbox
                    (dolist (abox (associated-aboxes current-tbox))
                      (loop for ind in (rest list-term) do
                            (add-concept-assertion abox ind ind)))))
                (when *tbox-verbose*
		  (racer-warn "~A encoded as ~A" list-term (cons 'or (rest list-term))))
		(encode-concept-term-1 (cons 'or (mapcar #'(lambda (ind) `(nominal ,ind)) (rest list-term)))))
               (not (encode-not-term list-term))
               (some (encode-some-term list-term))
	       (self-reference (encode-self-term list-term))
               (all (encode-all-term list-term))
               (at-least (encode-at-least-term list-term))
               (at-most (encode-at-most-term list-term))
               (exactly (encode-exactly-term list-term))
               (disjoint-and (encode-disjoint-and-term list-term))
               ((no an a > >= < <= min max string= string<> boolean= boolean<> divisible not-divisible) 
                (encode-cd-term list-term))
               (ensure
                (let ((term (second list-term)))
                  (if (eq (first term) 'not)
                      (encode-ensure-term (list (first list-term) (negate-concept-term term)))
                    (encode-ensure-term list-term))))
               ((= <>) (encode-real-cd-equal-term list-term))
               ((equal unequal) (encode-integer-cd-equal-term list-term))
               (range (encode-cd-range-term list-term))
               ;; (d-base-type (encode-d-base-type list-term)) I do not want to support this because of problems with decoding.
               ;; (d-datarange ...) ditto.
               (has-value (encode-some-term `(some ,(second list-term) (one-of ,(third list-term)))))
               (d-filler (encode-datatype-filler-restriction list-term))
               (d-all (encode-datatype-value-restriction list-term))
               (d-some (encode-datatype-existential-restriction list-term))
               (d-at-least (encode-datatype-at-least-restriction list-term))
               (d-at-most (encode-datatype-at-most-restriction list-term))
               (d-exactly (encode-datatype-exactly-restriction list-term))
               (i-all (encode-ria-initial-concept list-term))
               (i-some (encode-neg-ria-initial-concept list-term))
               (f-all (encode-ria-final-concept list-term))
               (f-some (encode-neg-ria-final-concept list-term))
               (otherwise (error "cannot deal with ~A" list-term)))))))

    (if (listp result)
        result
      (set-language result))))

;;; ======================================================================

(defun ensure-decoding-is-possible (list-term encoded-term)
  (cond ((and (concept-node-p encoded-term) ;; Due to taxonomic encoding an atomic concept might show up here.
              (quantification-concept-p (concept-encoded-definition encoded-term)))
         (setf (concept-decoded-list-term (concept-encoded-definition encoded-term)) list-term))
        ((and (negated-concept-p encoded-term)
              (quantification-concept-p
               (concept-negated-concept (concept-encoded-definition (concept-term encoded-term)))))
         (setf (concept-decoded-list-term
                (concept-negated-concept (concept-encoded-definition (concept-term encoded-term))))
               list-term))
        ((or (eq encoded-term *bottom-concept*)
             (eq encoded-term *top-concept*)
             (eq encoded-term *datatype-bottom-concept*)
             (eq encoded-term *datatype-top-concept*))
         encoded-term)
        ((quantification-concept-p encoded-term)
         (setf (concept-decoded-list-term encoded-term) list-term))
        (t (error "Do not know how to make sure that ~A can be decoded -- should not happen." encoded-term)))
  encoded-term)

(defun encode-datatype-filler-restriction (list-term)
  (let ((role-name (second list-term)))
    (unless (and (eql (length list-term) 3)
                 (or (role-term-p role-name) (role-node-p role-name)))
      (error "Illegal syntax in ~S" list-term))
    (let* ((role (encode-role-term role-name)))
      #+:debug (assert (role-datatype role))
      (let* ((data-spec (third list-term))
           (encoded-p (concept-p-internal data-spec))
           (transformed-data-spec (if encoded-p
                                      data-spec
                                    (if (and (listp data-spec) (eq (first data-spec) 'd-literal))
                                        (transform-literal data-spec)
                                      (transform-literal `(d-literal ,data-spec nil)))))
           (result 
            (encode-datatype-existential-restriction `(d-some ,role ,transformed-data-spec)))
           (final-result
            (if (and (some-concept-p result) (consp transformed-data-spec))
                (let* ((new-role-name (second transformed-data-spec))
                       (used-datatype (if (member new-role-name +internal-roles+)
                                          (cdr (assoc new-role-name +internal-role-type+))
                                        (transform-type (role-datatype (concept-role result)))))
                       (role-datatype (role-datatype role))
                       (declared-datatype 
                        (cond ((symbolp role-datatype)
                               role-datatype)
                              ((eq (first role-datatype) 'd-base-type)
                               (transform-type role-datatype))
                              ((eq (first role-datatype) 'd-datarange)
                               (transform-type (second role-datatype)))
                              (t (racer-warn "Datatype ~A of property ~A is ignored"
                                             role-datatype role-name)
                                 +datatype-top-symbol+))))
                  (if (and used-datatype (symbolp used-datatype))
                      (if (or (eq used-datatype 't)
                              (eq declared-datatype 't)
                              (datatype-compatible-p used-datatype declared-datatype new-role-name))
                          result
                        (progn
                          (racer-warn "~&Type ~A of literal ~A is NOT compatible with declared type ~A of property ~A~%"
                                      used-datatype
                                      data-spec
                                      declared-datatype
                                      role)
                          *bottom-concept*))
                    result))
              result)))
      (ensure-decoding-is-possible list-term final-result)))))

(defun racer-datatype-default-value (datatype)
  (if (eq datatype t)
      +datatype-top-symbol+
    (transform-data-specification datatype 'some)))

(defun encode-datatype-existential-restriction (list-term)
  (let ((role-name (second list-term))
        (bottom *bottom-concept*))
    (unless (and (eql (length list-term) 3)
                 (or (role-term-p role-name) (role-node-p role-name)))
      (error "Illegal syntax in ~S" list-term))
    (let* ((data-spec (third list-term))
           (encoded-p (concept-p-internal data-spec))
           (role (encode-role-term role-name))
           (transformed-data-spec (if encoded-p
                                      data-spec
                                    (transform-data-specification data-spec 'some (role-cd-attribute role))))
           (encoded-data-spec (if encoded-p
                                  data-spec
                                (encode-concept-term transformed-data-spec))))
      (if (is-bottom-datatype-concept-p encoded-data-spec)
          bottom
        (if (and (not (and (role-datatype role) (valid-general-cd-concept-p encoded-data-spec)))
                 (not (role-cd-attribute role)))
            (progn
              (when *tbox-verbose*
                (racer-warn "~S is inconsistent due to datatype property ~A or data specification ~S"
                            list-term role-name data-spec))
              bottom)
          (if (and (cd-concept-p encoded-data-spec)
                   (eq (predicate-operator (concept-predicate encoded-data-spec)) 'bottom))
              bottom
            (let ((range (role-range-restriction role))
                  (domain (unless *ignore-role-domain-range*
                            (role-domain-restriction role))))
              (if (or (and domain (eq domain bottom))
                      (and (cd-concept-p range) (eq (predicate-operator (concept-predicate range)) 'bottom)))
                  bottom
                (encode-datatype-existential-restriction-internal list-term
                                                                  role
                                                                  encoded-data-spec
                                                                  domain)))))))))

(defun encode-datatype-existential-restriction-internal (list-term role encoded-data-spec domain)
  (let* ((top *top-concept*)
         (bottom *bottom-concept*)
         (datatype-bottom *datatype-bottom-concept*)
         (range (role-range-restriction role))
         (use-range (and range (not *ignore-role-domain-range*)))
         (range-term (if use-range
                         `(and ,range ,encoded-data-spec)
                       encoded-data-spec))
         (range-concept (if use-range
                            (with-flatten-encodings
                              (without-taxonomic-encoding
                                (encode-concept-term range-term)))
                          range-term)))
    (if (and use-range (eq range-concept datatype-bottom))
        (add-encoded-concept range-term datatype-bottom)
      (let* ((domain (unless *ignore-role-domain-range*
                       domain))
             (domain-concept (or domain top))
             (new-concept (register-new-concept 'd-some
                                                encoded-data-spec
                                                :role role))
             (augmented-term
              (when (or domain range)
                `(and ,domain-concept ,new-concept))))
        (if (and augmented-term (eq (find-encoded-concept augmented-term) bottom))
            bottom
          (let ((augmented-encoded-term
                 (when augmented-term
                   (with-flatten-encodings
                     (encode-concept-term-1 augmented-term)))))
            (if (eq augmented-encoded-term bottom)
                (add-encoded-concept augmented-term bottom)
              (progn
                (when (some-concept-p new-concept)
                  (when (and domain
                             (not (eq domain-concept top))
                             (not (eq domain-concept (concept-role-domain new-concept))))
                    (setf (concept-role-domain new-concept) domain-concept)
                    (when (and (atomic-concept-p domain-concept) (not (eq domain-concept top)))
                      (pushnew domain-concept (concept-told-subsumers new-concept))))
                  (when (and range
                             (not (is-top-datatype-concept-p range-concept))
                             (not (eq range-concept encoded-data-spec))
                             (not (eq range-concept (concept-role-range new-concept))))
                    (setf (concept-role-range new-concept) range-concept)))
                (ensure-decoding-is-possible list-term new-concept)))))))))

(defun encode-datatype-value-restriction (list-term)
  (let ((role-name (second list-term)))
    (unless (and (eql (length list-term) 3)
                 (or (role-term-p role-name) (role-node-p role-name)))
      (error "Illegal syntax in ~S" list-term))
    (let* ((data-spec (third list-term))
           (encoded-p (concept-p-internal data-spec))
           (role (encode-role-term role-name))
           (transformed-data-spec (if encoded-p 
                                      data-spec
                                    (transform-data-specification data-spec 'all (role-cd-attribute role))))
           (encoded-data-spec (if encoded-p 
                                  data-spec
                                (encode-concept-term transformed-data-spec)))
           (neg-encoded-data-spec (encode-concept-term `(not ,encoded-data-spec)))
           (top *top-concept*)
           (bottom *bottom-concept*))
      (if (and (not (and (role-datatype role) (valid-general-cd-concept-p encoded-data-spec)))
               (not (role-cd-attribute role)))
          (progn
            (when *tbox-verbose*
              (racer-warn "~A is inconsistent due to datatype property ~A or data specification ~A"
                          list-term role-name data-spec))
            bottom)
        (if (eq (role-domain-restriction role) bottom)
            top
          (ensure-decoding-is-possible 
           list-term
           (get-negated-concept
            (encode-datatype-existential-restriction `(d-some ,role ,neg-encoded-data-spec)))))))))

(defun encode-datatype-at-least-restriction (list-term)
  (let ((number (second list-term))
        (role-name (third list-term))
        (data-spec (fourth list-term))
        (top *top-concept*)
        (bottom *bottom-concept*))
    (unless (and (<= 3 (length list-term) 4)
                 (integerp number)
                 (not (minusp number))
                 (or (role-term-p role-name) (role-node-p role-name)))
      (error "Illegal syntax in ~A" list-term))
    (if (zerop number)
        top
      (let* ((role (encode-role-term role-name))
             (cd-attribute (role-cd-attribute role))
             (encoded-p (concept-p-internal data-spec))
             (transformed-data-spec (if encoded-p
                                        data-spec
                                      (transform-data-specification (or data-spec
                                                                        (role-datatype role))
                                                                    'some
                                                                    cd-attribute)))
             (encoded-data-spec (if encoded-p
                                    data-spec
                                  (encode-concept-term transformed-data-spec))))
        (if (is-bottom-datatype-concept-p encoded-data-spec)
            bottom
          (if (and (not (and (role-datatype role) (valid-general-cd-concept-p encoded-data-spec)))
                   (not cd-attribute))
              (progn
                (when *tbox-verbose*
                  (racer-warn "~A is inconsistent due to datatype property ~A or data specification ~A"
                              list-term role-name data-spec))
                bottom)
            (if (and (cd-concept-p encoded-data-spec)
                     (eq (predicate-operator (concept-predicate encoded-data-spec)) 'bottom))
                bottom
              (let ((range (role-range-restriction role))
                    (domain (unless *ignore-role-domain-range*
                              (role-domain-restriction role))))
                (if (or (and domain (eq domain bottom))
                        (and (cd-concept-p range) (eq (predicate-operator (concept-predicate range)) 'bottom)))
                    bottom
                  (if (eql number 1)
                      (encode-datatype-existential-restriction-internal list-term
                                                                        role
                                                                        encoded-data-spec
                                                                        domain)
                    (if (and (role-feature-p role) (> number 1))
                        (progn
                          (when *tbox-verbose*
                            (racer-warn "Term ~S is equivalent to ~S due to functional datatype property ~A~%"
                                        list-term +bottom-symbol+ role-name))
                          bottom)
                      (encode-datatype-at-least-restriction-internal list-term
                                                                     number
                                                                     role
                                                                     encoded-data-spec
                                                                     domain))))))))))))

(defun encode-datatype-at-least-restriction-internal (list-term number role encoded-data-spec domain)
  (let* ((top *top-concept*)
         (bottom *bottom-concept*)
         (datatype-bottom *datatype-bottom-concept*)
         (range (role-range-restriction role))
         (use-range (and range (not *ignore-role-domain-range*)))
         (range-term (if use-range
                         `(and ,range ,encoded-data-spec)
                       encoded-data-spec))
         (range-concept (if use-range
                            (with-flatten-encodings
                              (without-taxonomic-encoding
                                (encode-concept-term range-term)))
                          range-term)))
    (if (and use-range (eq range-concept datatype-bottom))
        (add-encoded-concept range-term datatype-bottom)
      (let* ((domain (unless *ignore-role-domain-range*
                       domain))
             (domain-concept (or domain top))
             (new-concept (register-new-concept 'd-at-least encoded-data-spec
                                                :role role
                                                :number number))
             (augmented-term
              (when (or domain range)
                `(and ,domain-concept ,new-concept))))
        (if (and augmented-term (eq (find-encoded-concept augmented-term) bottom))
            bottom
          (let ((augmented-encoded-term
                 (when augmented-term
                   (with-flatten-encodings
                     (encode-concept-term-1 augmented-term)))))
            (if (eq augmented-encoded-term bottom)
                (add-encoded-concept augmented-term bottom)
              (progn
                (when (at-least-concept-p new-concept)
                  (when (and domain
                             (not (eq domain-concept top))
                             (not (eq domain-concept (concept-role-domain new-concept))))
                    (setf (concept-role-domain new-concept) domain-concept)
                    (when (and (atomic-concept-p domain-concept) (not (eq domain-concept top)))
                      (pushnew domain-concept (concept-told-subsumers new-concept)))))
                (when (and range
                           (not (is-top-datatype-concept-p range-concept))
                           (not (eq range-concept encoded-data-spec))
                           (not (eq range-concept (concept-role-range new-concept))))
                  (setf (concept-role-range new-concept) range-concept))
                (ensure-decoding-is-possible list-term new-concept)))))))))

(defun encode-datatype-at-most-restriction (list-term)
  (let* ((number (second list-term))
         (role-name (third list-term))
         (data-spec (fourth list-term)))
    (unless (and (<= 3 (length list-term) 4)
                 (integerp number)
                 (not (minusp number))
                 (or (role-term-p role-name) (role-node-p role-name)))
      (error "Illegal syntax in ~A" list-term))
    (if (zerop number)
        (if data-spec
            (encode-datatype-value-restriction `(d-all ,role-name (not ,data-spec)))
          (encode-datatype-value-restriction `(d-all ,role-name ,+datatype-bottom-symbol+)))
      (let* ((role (encode-role-term role-name))
             (encoded-p (concept-p-internal data-spec))
             (cd-attribute (role-cd-attribute role))
             (transformed-data-spec (if encoded-p
                                        data-spec
                                      (transform-data-specification data-spec 'all cd-attribute)))
             (encoded-data-spec (if encoded-p
                                    data-spec
                                  (encode-concept-term transformed-data-spec)))
             (top *top-concept*)
             (bottom *bottom-concept*))
        (if (and (not (and (role-datatype role) (valid-general-cd-concept-p encoded-data-spec)))
                 (not cd-attribute))
            (progn
              (when *tbox-verbose*
                (racer-warn "~A is inconsistent due to datatype property ~A or data specification ~A"
                            list-term role-name data-spec))
              bottom)
          (if (and (cd-concept-p encoded-data-spec)
                   (eq (predicate-operator (concept-predicate encoded-data-spec)) 'bottom))
              top
            (let ((domain (role-domain-restriction role)))
              (if (eq domain bottom)
                  top
                (if (and *simplify-redundant-at-most* (role-feature-p role) (not (zerop number)))
                    (progn
                      (when *tbox-verbose*
                        (racer-warn "Term ~S is equivalent to ~S due to functional datatype property ~A~%"
                                    list-term +top-symbol+ role-name))
                      top)
                  (encode-datatype-at-most-restriction-internal list-term
                                                                number
                                                                role
                                                                transformed-data-spec
                                                                encoded-data-spec))))))))))

(defun encode-datatype-at-most-restriction-internal (list-term
                                                     number
                                                     role
                                                     data-spec
                                                     encoded-data-spec)
  (let ((transformed-type (when data-spec (transform-type data-spec)))
        (term (ensure-decoding-is-possible
               list-term
               (get-negated-concept (register-new-concept 'd-at-least encoded-data-spec
                                                          :role role
                                                          :number (1+ number))))))
    (case (if (consp transformed-type)
              (first transformed-type)
            transformed-type)
      (boolean
       (let* ((at-most-true
               `(d-at-most 1 ,role (boolean= ,+has-boolean-value+ #t)))
              (at-most-true-term
               (ensure-decoding-is-possible at-most-true (encode-at-most-term at-most-true)))
              (at-most-false
               `(d-at-most 1 ,role (boolean= ,+has-boolean-value+ #f)))
              (at-most-false-term
               (ensure-decoding-is-possible at-most-false (encode-at-most-term at-most-false))))
         (encode-and-term `(and ,term ,at-most-true-term ,at-most-false-term) nil)))
      (integer 
       #+:ignore
       ;;; zu heavy fuer Racer... 
       (let* ((min (second transformed-type))
              (max (third transformed-type))
              (at-most-terms 
               (loop as i from min to max 
                     as term = `(d-at-most 1 ,role (equal ,+has-integer-value+ ,i))
                     collect (ensure-decoding-is-possible term (encode-at-most-term term)))))
         (encode-and-term `(and ,term ,@at-most-terms) nil))
       term)
      (otherwise
       term))))

(defun encode-datatype-exactly-restriction (list-term)
  (let ((d-at-least
         (encode-datatype-at-least-restriction 
          `(d-at-least ,@(cdr list-term))))
        (d-at-most 
         (encode-datatype-at-most-restriction 
          `(d-at-most ,@(cdr list-term)))))
    (encode-and-term `(and ,d-at-least ,d-at-most) nil)))

(defun transform-literal (data-spec)
  (let* ((value (second data-spec))
         (type (third data-spec))
         (transformed-type (transform-type type value)))
    (ecase (if (symbolp transformed-type)
               transformed-type
             (first transformed-type))
      (integer `(equal ,+has-integer-value+ 
                       ,(if (integerp value)
                            value
                          (read-from-string value))))
      (boolean `(boolean= ,+has-boolean-value+ 
                          ,(if (racer-boolean-p value)
                               value
                             (if (string= value "false")
                                 *false*
                               *true*))))
      (string `(string= ,+has-string-value+ ,value))
      (real `(= ,+has-real-value+ 
                ,(if (numberp value)
                     value
                   (read-from-string value))))
      (#.+datatype-top-symbol+ +datatype-top-symbol+))))

(defun transform-data-specification (datarange-or-ranges quantifier &optional (cd-attribute nil))
  (if (null datarange-or-ranges)

      +datatype-top-symbol+

    (if (symbolp datarange-or-ranges)
        (if (or (eq datarange-or-ranges +datatype-top-symbol+)
                (eq datarange-or-ranges +datatype-bottom-symbol+))
            datarange-or-ranges
          (transform-data-specification `(d-base-type ,datarange-or-ranges) quantifier cd-attribute))

      (let ((op (first datarange-or-ranges))
            (spec (rest datarange-or-ranges)))

        (if (consp op) ; Conjunction of Dataranges? 
            `(and ,@(mapcar #'(lambda (x) 
                                (transform-data-specification x quantifier cd-attribute))
                            datarange-or-ranges))
          (case op
            (d-datarange
             (transform-data-specification spec quantifier cd-attribute))

            (d-base-type
             (let ((transformed-type (transform-type datarange-or-ranges)))
               `(and ,(if (symbolp transformed-type)
                          (ecase transformed-type
                            (integer `(a ,+has-integer-value+))
                            (boolean `(a ,+has-boolean-value+))
                            (string `(a ,+has-string-value+))
                            (real `(a ,+has-real-value+))
                            (#.+datatype-top-symbol+ +datatype-top-symbol+))
                        (encode-type-with-min-max transformed-type))
                     ,@(transform-quantifier quantifier 
                                             (if (symbolp transformed-type)
                                                 transformed-type
                                               (first transformed-type))))))

            (d-complement
             `(not ,(transform-data-specification (first spec) quantifier cd-attribute)))
          
            (d-and
             `(and ,@(mapcar #'(lambda (x)
                                 (transform-data-specification x quantifier cd-attribute))
                             spec)))

            (d-or
             `(or ,@(mapcar #'(lambda (x) 
                                (transform-data-specification x quantifier cd-attribute))
                            spec)))
          
            (d-possible-values
             `(or ,@(loop for literal in spec
                          collect (transform-literal literal))))

            (d-restriction
             (let ((base-type (first spec))
                   (facets (rest spec)))

               `(and ,(transform-data-specification base-type quantifier cd-attribute)
                     ,@(mapcar #'(lambda (x) 
                                   (transform-data-specification x quantifier cd-attribute))
                               facets))))

            (d-facet 
             (let* ((facet-op (map-facet-op (first spec)))
                    (literal (second spec))
                    (transformed-type (transform-type (third literal) (second literal)))
                    (type (if (symbolp transformed-type) 
                              transformed-type
                            (first transformed-type))))

               (ecase facet-op
                 (#.(intern (concatenate 'string nox:-xsd-uri- "maxExclusive") :racer)
                  (ecase type
                    (integer `(max ,+has-integer-value+ ,(- (get-literal literal) 1)))
                    (boolean (progn (racer-warn "Facet ~A ignored." spec) +datatype-top-symbol+))
                    (string (progn (racer-warn "Facet ~A ignored." spec) +datatype-top-symbol+))
                    (real `(< ,+has-real-value+ ,(read-from-string (second literal))))
                    (#.+datatype-top-symbol+ (progn (racer-warn "Facet ~A ignored." spec) +datatype-top-symbol+))))
                 (#.(intern (concatenate 'string nox:-xsd-uri- "maxInclusive") :racer)
                  (ecase type
                    (integer `(max ,+has-integer-value+ ,(get-literal literal)))
                    (boolean (progn (racer-warn "Facet ~A ignored." spec) +datatype-top-symbol+))
                    (string (progn (racer-warn "Facet ~A ignored." spec) +datatype-top-symbol+))
                    (real `(<= ,+has-real-value+ ,(read-from-string (second literal))))
                    (#.+datatype-top-symbol+ (progn (racer-warn "Facet ~A ignored." spec) +datatype-top-symbol+))))
                 (#.(intern (concatenate 'string nox:-xsd-uri- "minExclusive") :racer)
                  (ecase type
                    (integer `(min ,+has-integer-value+ ,(+ (get-literal literal) 1)))
                    (boolean (progn (racer-warn "Facet ~A ignored." spec) +datatype-top-symbol+))
                    (string (progn (racer-warn "Facet ~A ignored." spec) +datatype-top-symbol+))
                    (real `(> ,+has-real-value+ ,(read-from-string (second literal))))
                    (#.+datatype-top-symbol+ (progn (racer-warn "Facet ~A ignored." spec) +datatype-top-symbol+))))
                 (#.(intern (concatenate 'string nox:-xsd-uri- "minInclusive") :racer)
                  (ecase type
                    (integer `(min ,+has-integer-value+ ,(get-literal literal)))
                    (boolean (progn (racer-warn "Facet ~A ignored." spec) +datatype-top-symbol+))
                    (string (progn (racer-warn "Facet ~A ignored." spec) +datatype-top-symbol+))
                    (real `(>= ,+has-real-value+ ,(read-from-string (second literal))))
                    (#.+datatype-top-symbol+ (progn (racer-warn "Facet ~A ignored." spec) +datatype-top-symbol+))))
                 (#.(intern (concatenate 'string nox:-xsd-uri- "minLength") :racer)
                  (racer-warn "Facet ~A ignored." spec)
                  +datatype-top-symbol+)
                 (#.(intern (concatenate 'string nox:-xsd-uri- "maxLength") :racer)
                  (racer-warn "Facet ~A ignored." spec)
                  +datatype-top-symbol+)
                 (#.(intern (concatenate 'string nox:-xsd-uri- "length") :racer)
                  (racer-warn "Facet ~A ignored." spec) 
                  +datatype-top-symbol+)
                 (#.(intern (concatenate 'string nox:-xsd-uri- "pattern") :racer)
                  (racer-warn "Facet ~A ignored." spec) 
                  +datatype-top-symbol+)
                 (#.(intern (concatenate 'string nox:-xsd-uri- "totalDigits") :racer)
                  (racer-warn "Facet ~A ignored." spec) 
                  +datatype-top-symbol+)
                 (#.(intern (concatenate 'string nox:-xsd-uri- "fractionDigits") :racer)
                  (racer-warn "Facet ~A ignored." spec) 
                  +datatype-top-symbol+))))
            (and
             `(and ,@(mapcar #'(lambda (x)
                                 (transform-data-specification x quantifier cd-attribute))
                             spec)))
            (or
             `(or ,@(mapcar #'(lambda (x)
                                (transform-data-specification x quantifier cd-attribute))
                            spec)))
            (not
             `(not ,(transform-data-specification (first spec) quantifier cd-attribute)))
            ((no an a > >= < <= min max string= string<> boolean= boolean<> divisible not-divisible
                 = <> equal unequal)
             (if (or cd-attribute (member (first spec) +internal-roles+)) 
                 datarange-or-ranges
               (error "unknown data range in ~S" datarange-or-ranges)))
            (ensure
             `(ensure ,(transform-data-specification (first spec) quantifier cd-attribute)))
            (otherwise 
             (if cd-attribute
                 (error "unknown concrete domain specification in ~S" datarange-or-ranges)
               (error "unknown data range in ~S" datarange-or-ranges)))))))))

(defun transform-quantifier (quantifier type)
  (ecase quantifier
    (all (ecase type
           (integer `((no ,+has-boolean-value+) (no ,+has-string-value+) (no ,+has-real-value+)))
           (boolean `((or (ensure (boolean= ,+has-boolean-value+ #T))
                          (ensure (boolean= ,+has-boolean-value+ #F)))
                      (no ,+has-integer-value+)
                      (no ,+has-string-value+) 
                      (no ,+has-real-value+)))
           (string `((no ,+has-integer-value+) (no ,+has-boolean-value+) (no ,+has-real-value+)))
           (real `((no ,+has-integer-value+) (no ,+has-boolean-value+) (no ,+has-string-value+)))
           (#.+datatype-top-symbol+ (list +datatype-top-symbol+))))
    (some nil)))

(defun map-facet-op (op)
  (case op
    (>=
     '#.(intern (concatenate 'string nox:-xsd-uri- "minInclusive") :racer))
    (>
     '#.(intern (concatenate 'string nox:-xsd-uri- "minExclusive") :racer))
    (<=
     '#.(intern (concatenate 'string nox:-xsd-uri- "maxInclusive") :racer))
    (<
     '#.(intern (concatenate 'string nox:-xsd-uri- "maxExclusive") :racer))
    (otherwise op)))

(defun get-literal (data-spec)
  (let* ((value (second data-spec))
         (type (third data-spec))
         (transformed-type (transform-type type value)))
    (ecase (if (symbolp transformed-type)
                 transformed-type
               (first transformed-type))
        (integer (if (integerp value) value (read-from-string value)))
        (boolean (if (racer-boolean-p value)
                     value
                   (if (string= value "false")
                       *false*
                     *true*)))
        (string value)
        (real (if (numberp value) value (read-from-string value))))))


(defun encode-type-with-min-max (type)
  (case (first type)
    (integer `(and ,@(if (second type)
                         `((min ,+has-integer-value+ ,(second type)))
                       nil)
                   ,@(if (third type)
                         `((max ,+has-integer-value+ ,(third type)))
                       nil)))
    (real `(and ,@(if (second type)
                      `((>= ,+has-real-value+ ,(second type)))
                    nil)
                ,@(if (third type)
                      `((<= ,+has-real-value+ ,(third type)))
                    nil)))
    (otherwise (error "should not happen."))))

(defun transform-type (type-spec &optional (value nil value-specified))
  (cond ((and (consp type-spec) (eq (first type-spec) 'd-base-type))
         (setf type-spec (second type-spec))
         (cond ((eql type-spec '|http://www.w3.org/2001/XMLSchema#negativeInteger|)
                '(integer nil -1))
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#nonNegativeInteger|)
                '(integer 0 nil))
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#positiveInteger|)
                '(integer 1 nil))
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#nonPositiveInteger|)
                '(integer nil 0))
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#integer|)
                'integer)
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#byte|)
                '(integer 0 255))
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#unsignedByte|)
                '(integer 0 255))
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#unsignedShort|)
                '(integer 0 65535))
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#unsignedInt|)
                '(integer 0 4294967295))
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#unsignedLong|)
                '(integer 0 18446744073709551615))
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#short|)
                '(integer -32768 32767))
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#int|)
                '(integer -2147483648 2147483647))
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#long|)
                '(integer -9223372036854775808 92233720366854775807))
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#float|)
                'real)
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#double|)
                'real)
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#decimal|)
                'real)
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#string|)
                'string)
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#boolean|)
                'boolean)

               ;; added by MW
               ((eq type-spec '|http://www.w3.org/2001/XMLSchema#anyURI|)
                'string)

               ((eq type-spec 'cardinal) 
                '(integer 0 nil))
               ((eq type-spec 'integer) 'integer)
               ((eq type-spec 'real) 'real)
               ((eq type-spec 'string) 'string)
               ((eq type-spec 'boolean) 'boolean)
               (t +datatype-top-symbol+)))
        ((and (null type-spec) value-specified)
         (etypecase value
           (string 'string)
           (racer-boolean 'boolean)
           (integer 'integer)
           (real 'real)))
        (t type-spec)))



;;; ======================================================================

(defun make-normalized-cd-term (list-term)
  (if (eq (first list-term) 'an)
    `(a . ,(rest list-term))
    list-term))

(defun encode-real-cd-equal-term (list-term)
  (cond ((form-represents-linear-predicate-p list-term)
         (encode-cd-term list-term))
        ((form-represents-nonlinear-predicate-p list-term)
         #-:cgb (error "not implemented")
         #+:cgb (encode-cd-term list-term)
         )
        (t (error "Syntax error in concrete domain term: ~S" list-term))))

(defun encode-integer-cd-equal-term (list-term)
  (unless (eql (length list-term) 3)
    (error "invalid syntax for concrete domain EQUAL term in ~S: ~
            syntax should be (EQUAL <attribute> <value>)" list-term))
  (let ((attribute (second list-term))
        (value (third list-term)))
    (unless (symbolp attribute)
      (error "concrete domain attribute name ~S must be a symbol in ~S"
             attribute list-term))
    (unless (integerp value)
      (error "value ~S in ~S must be of type INTEGER"
             value list-term))
    (cond ((form-represents-linear-predicate-p list-term)
           (encode-cd-term list-term))
          (t (error "Syntax error in equal concrete domain term: ~S" list-term)))))

(defun encode-cd-range-term (list-term)
  (unless (eql (length list-term) 4)
    (error "invalid syntax for concrete domain RANGE term in ~S: ~
            syntax should be (RANGE <attribute> <min> <max>)" list-term))
  (let ((attribute (second list-term))
        (min (third list-term))
        (max (fourth list-term)))
    (unless (symbolp attribute)
      (error "concrete domain attribute name ~S must be a symbol in ~S"
             attribute list-term))
    (unless (integerp min)
      (error "minimum range value ~S in ~S must be of type INTEGER"
             min list-term))
    (unless (integerp max)
      (error "maximum range value ~S in ~S must be of type INTEGER"
             max list-term))
    (if (> min max)
      (progn
        (when *tbox-verbose*
          (racer-warn "~D:~D specifies an empty range in ~S and is reduced to ~S"
                      min max list-term +bottom-symbol+))
        +bottom-symbol+)
      (encode-and-term `(and (min ,attribute ,min)
                             (max ,attribute ,max))
                       nil))))


(defun type-compatible-p (used-type declared-type)
  (case declared-type
    ;; If a type is declared as complex, it can be used in real predicates!
    ;; If there are are predicate only (i.e., linear ineqations, there is
    ;; no complex solution iff there is no real solution. If there
    ;; are additional nonlinear predicates, the real predicate is treated 
    ;; appropriately!
    (complex (or (eq used-type 'complex)  (eq used-type 'real)))
    (real (or (eq used-type 'real) 
              ;;(eq used-type 'integer)
              ))
    (integer (eq used-type 'integer))
    (cardinal (eq used-type 'cardinal))
    (string (eq used-type 'string))
    (boolean (eq used-type 'string))))

(defun datatype-compatible-p (used-type declared-type internal-role-name)
  (or (null used-type)
      (null declared-type)
      (eq used-type +datatype-top-symbol+)
      (eq used-type t)
      (eq declared-type +datatype-top-symbol+)
      (eq declared-type t)
      (equal used-type declared-type)
      (if (and (symbolp used-type) (symbolp declared-type))
          (if (member used-type +internal-property-datatypes+)
              (if internal-role-name
                  (eq (cdr (assoc internal-role-name +internal-role-type+))
                      declared-type)
                (eq used-type declared-type)))
        (let* ((new-used-type-1 (transform-type used-type))
               (new-used-type-2 
                (if (consp new-used-type-1)
                    (if (and (eq (first new-used-type-1) 'd-datarange)
                             (eq (first (second new-used-type-1)) 'd-base-type))
                        (second (second new-used-type-1))
                      (first new-used-type-1))
                  new-used-type-1))
               (new-declared-type-1 (transform-type declared-type))
               (new-declared-type-2
                (if (consp new-declared-type-1)
                    (cond ((and (eq (first new-declared-type-1) 'd-datarange)
                                (eq (first (second new-declared-type-1)) 'd-base-type))
                           (second (second new-declared-type-1)))
                          ((eq (first new-declared-type-1) 'd-possible-values)
                           (rest new-declared-type-1))
                          (t (first new-declared-type-1)))
                  new-declared-type-1)))

          (cond ((consp new-declared-type-2)
                 ;; possible values?
                 (some #'(lambda (x)
                           (and (eq (first x) 'd-literal)
                                (eq (transform-type (third x)) used-type)))
                       new-declared-type-2))
                ((not (and (symbolp new-used-type-2) (symbolp new-declared-type-2)))
                 (error "Cannot deal with datatype descriptions ~S and ~S" 
                        used-type declared-type))
                (t (and (eq new-used-type-2 new-declared-type-2)
                        (member new-used-type-2 +internal-property-datatypes+)
                        t)))))))

(defun encode-cd-term (list-term)
  (let* ((tbox *use-tbox*)
         (concrete-domain *concrete-domain*)
         (list-term (make-normalized-cd-term list-term))
         (role (get-role (second list-term)))
         (role-datatype (and role (role-datatype role))))
    (if (and role-datatype (not (eq role-datatype 't))
             (not (member (role-name role) +internal-roles+))
             (not (or (eq (first list-term) 'a)
                      (eq (first list-term) 'an)
                      (eq (first list-term) 'no))))
      (ecase (first list-term)
        (equal
         (if (datatype-compatible-p 'integer role-datatype nil)
           (encode-concept-term `(some ,(role-name role)
                                       (equal ,+has-integer-value+ ,(third list-term))))
           *bottom-concept*))
        (=
         (if (datatype-compatible-p 'real role-datatype nil)
           (encode-concept-term `(some ,(role-name role)
                                       (= ,+has-real-value+ ,(third list-term)))))
         *bottom-concept*)
        (string=
         (if (datatype-compatible-p 'string role-datatype nil)
           (encode-concept-term `(some ,(role-name role)
                                       (string= ,+has-string-value+ ,(third list-term))))
           *bottom-concept*))
        (boolean= 
         (if (datatype-compatible-p 'boolean role-datatype nil)
           (encode-concept-term `(some ,(role-name role)
                                       (boolean= ,+has-boolean-value+
                                                 ,(if (racer-boolean-p (third list-term))
                                                    (third list-term)
                                                    (cond
                                                     ((string= (third list-term) "true") *true*)
                                                     ((string= (third list-term) "false") *false*)
                                                     (t (error "Unknown boolean value -- should not happen.")))))))
           *bottom-concept*)))
      (multiple-value-bind (predicate attributes)
                           (ensure-predicate list-term concrete-domain)
        (if (and *optimize-datatype-role-fillers* role-datatype)
          (unless (datatype-compatible-p (predicate-type predicate)
                                         (if (role-datatype role)
                                             (role-datatype role)
                                           (if (role-cd-attribute role)
                                               (role-cd-attribute
                                                (encode-role-term (first (predicate-parameters predicate))
                                                                  :cd-attribute
                                                                  (predicate-type predicate)))
                                             (error "unexpected")))
                                         (and role (role-name role)))
            (return-from encode-cd-term *bottom-concept*))
          (loop with supported-types = (cd-supported-types concrete-domain)
                with first-type = (predicate-type predicate)
                for parameter-name in (predicate-parameters predicate)
                for attribute = (encode-role-term parameter-name :cd-attribute first-type)
                do
                (unless (or (top-or-bottom-predicate-p predicate)
                            (type-compatible-p first-type (role-cd-attribute attribute)))
                  (if tbox
                    (if (eq (role-cd-attribute attribute) 't)
                      (error "For attribute ~A no range is declared, ~
                              therefore it cannot be used in term ~S found in TBox ~A." 
                             parameter-name list-term (tbox-name tbox))
                      (error "Attribute ~A is declared as ~A and cannot be used in term ~S because a ~A attribute is expected (TBox ~A)."
                             parameter-name (role-cd-attribute attribute) list-term first-type (tbox-name tbox)))
                    (if (eq (role-cd-attribute attribute) 't)
                      (error "For attribute ~A no range is declared, therefore it cannot be used in term ~S."
                             parameter-name list-term)
                      (error "Attribute ~A is declared as ~A and cannot be used in term ~S because a ~A attribute is expected."
                             parameter-name (role-cd-attribute attribute) list-term first-type))))
                (unless (or (top-or-bottom-predicate-p predicate)
                            (member first-type supported-types))
                  (error "concrete domain term ~S is not member of supported types ~S"
                         list-term supported-types))))
        (when tbox
          (ensure-attributes-are-declared tbox attributes list-term))
        (let* ((cd-concept (register-new-concept 'cd-predicate
                                                 predicate
                                                 :cd-term list-term))
               (cd-concept-sibling (register-new-concept 'cd-predicate
                                                         (predicate-negation predicate)
                                                         :cd-term (second (negate-concept-term list-term))))
               (cd-concept-neg-term (concept-term (concept-negated-concept cd-concept)))
               (cd-concept-sibling-neg-term (concept-term (concept-negated-concept cd-concept-sibling))))
          ;;; the pointers between ensure concept terms must be correctly linked to each other
          (when (and cd-concept-neg-term (null (concept-negated-concept-internal cd-concept-neg-term)))
            (setf (concept-term (concept-negated-concept-internal cd-concept)) cd-concept-sibling))
          (when (and cd-concept-sibling-neg-term (null (concept-negated-concept-internal cd-concept-sibling-neg-term)))
            (setf (concept-term (concept-negated-concept-internal cd-concept-sibling)) cd-concept))
          (when (and (cd-concept-p cd-concept)
                     (not (eq (predicate-operator (concept-predicate cd-concept)) 'bottom)))
            (let ((domain (role-domain-restriction
                           (get-role (first (predicate-parameters (concept-predicate cd-concept)))))))
              (when domain
                (setf (concept-attribute-domain cd-concept) domain)
                (when (atomic-concept-p domain)
                  (pushnew domain (concept-told-subsumers cd-concept))))))
          cd-concept)))))

(defun reencode-cd-concept (cd-concept)
  (unless (eq (predicate-operator (concept-predicate cd-concept)) 'bottom)
    (let ((domain (role-domain-restriction
                   (get-role (first (predicate-parameters (concept-predicate cd-concept)))))))
      (when domain
        (setf (concept-attribute-domain cd-concept) domain)
        (when (atomic-concept-p domain)
          (pushnew domain (concept-told-subsumers cd-concept))))))
  cd-concept)

(defun encode-ensure-term (list-term)
  (unless (eql (length list-term) 2)
    (error "Illegal syntax for ENSURE term in ~S: ~
            syntax should be (ENSURE <predicate-form>)" list-term))
  (let ((encoded-cd-concept (encode-concept-term-1 (second list-term))))
    (if (cd-concept-p encoded-cd-concept)
      (register-new-concept 'ensure encoded-cd-concept)
      (if (eq encoded-cd-concept *bottom-concept*)
        *bottom-concept*
        (error "second element in ensure-term ~S must be a concrete predicate term" list-term)))))

(defun ensure-attributes-are-declared (tbox attributes list-term)
  (let ((signature (tbox-signature tbox)))
    (if signature 
      (destructuring-bind (atomic-concepts roles transitive-roles features 
                                           signature-attributes)
                          signature
        (declare (ignore atomic-concepts roles transitive-roles features))
        (loop for attribute in attributes do
              (let ((result 
                     (find attribute signature-attributes :key #'second)))
                (when result
                  (pushnew attribute (tbox-all-attributes tbox)))
                (cond ((null result)
                       (error "Attribute ~A is not declared in the signature of Tbox ~S"
                              attribute 
                              (tbox-name tbox)))
                      ((or (eq (first list-term) 'min)
                           (eq (first list-term) 'max)
                           (eq (first list-term) 'range))
                       (unless (integerp (third list-term))
                         (error "The number in ~S must be an integer."
                                list-term))
                       (unless (eq (first result) 'integer)
                         (error "Attribute ~A is not declared as an integer in the signature of Tbox ~S"
                                attribute 
                                (tbox-name tbox))))
                      ((or (eq (first list-term) 'equal)
                           (eq (first list-term) 'unequal))
                       (unless (eq (first result) 'integer)
                         (error "Attribute ~A is not declared as an integer in the signature of Tbox ~S"
                                attribute 
                                (tbox-name tbox))))
                      ((or (eq (first list-term) 'divisible)
                           (eq (first list-term) 'not-divisible))
                       (unless (eq (first result) 'cardinal)
                         (error "Attribute ~A is not declared as a cardinal in the signature of Tbox ~S"
                                attribute 
                                (tbox-name tbox))))
                      ((or (eq (first list-term) 'string=)
                           (eq (first list-term) 'string<>))
                       (unless (eq (first result) 'string)
                         (error "Attribute ~A is not declared as a string in the signature of Tbox ~S"
                                attribute 
                                (tbox-name tbox))))
                      ((or (eq (first list-term) 'boolean=)
                           (eq (first list-term) 'boolean<>))
                       (unless (eq (first result) 'boolean)
                         (error "Attribute ~A is not declared as a string in the signature of Tbox ~S"
                                attribute 
                                (tbox-name tbox))))
                      ((member (first list-term) '(> >= < <= = <>))
                       (unless (or (eq (first result) 'cardinal) 
                                   (eq (first result) 'real) 
                                   (eq (first result) 'complex))
                         (error "Attribute ~A is not declared to be of type cardinal, real, or complex in the signature of Tbox ~S"
                                attribute 
                                (tbox-name tbox))))))))
      (loop for attribute in attributes do
            (pushnew attribute (tbox-all-attributes tbox))))))



(defun ensure-predicate (predicate-form concrete-domain)
  (let ((form (transform-predicate predicate-form)))
    (let ((predicate (gethash form (cd-registered-predicate-forms concrete-domain))))
      (if predicate
        (values predicate (predicate-parameters predicate))
        (let* ((name (gensym "P")) 
               (neg-name (gensym "NOT-P"))
               (parameters (compute-parameter-sequence form))
               (predicate
                (cond ((and (consp form) (eq (first form) 'no))
                       (add-special-predicate 'bottom form parameters name neg-name 
                                              concrete-domain
                                              (cdr (assoc (second form) +internal-role-type+))))
                      ((and (consp form) (or (eq (first form) 'a) (eq (first form) 'an)))
                       (add-special-predicate 'top form parameters name neg-name 
                                              concrete-domain
                                              (cdr (assoc (second form) +internal-role-type+))))
                      ((form-represents-integer-predicate-p form)
                       (add-integer-predicate (first form)
                                              form parameters name neg-name 
                                              concrete-domain))
                      ((form-represents-linear-predicate-p form)
                       (add-linear-predicate name neg-name 
                                             parameters 
                                             form concrete-domain))
                      ((form-represents-nonlinear-predicate-p form)
                       (add-nonlinear-predicate name neg-name 
                                                parameters 
                                                form concrete-domain))
                      ((form-represents-divisible-predicate-p form)
                       (add-divisible-predicate name neg-name 
                                                parameters 
                                                form concrete-domain))
                      ((form-represents-string-predicate-p form)
                       (add-string-predicate name neg-name 
                                             parameters 
                                             form concrete-domain))
                      
                      ((form-represents-boolean-predicate-p form)
                       (add-boolean-predicate name neg-name 
                                              parameters 
                                              form concrete-domain))
                      (t (error "Predicates of the form ~A are not supported." form)))))
          (values predicate parameters))))))




(defun transform-predicate (form)
  "Replace attribute synonyms (i.e., consider role equivalents)."

  #|
  ;; Currently, a predicate =constant is not needed. Previously, it was used in the OWL parser.
  (cond ((and (consp form) (eq (first form) '=constant))
         (let* ((attribute (second form))
                (object-or-attribute (third form))
                (value (fourth form))
                (domain-of-value (second (gethash attribute (tbox-role-axioms-index tbox)))))
           (case domain-of-value
             (integer (transform-predicate-1 `(equal ,object-or-attribute ,(read-from-string value))))
             ((cardinal real complex) (transform-predicate-1 `(= ,object-or-attribute ,(read-from-string value))))
             (string (transform-predicate-1 `(string= ,object-or-attribute ,value)))
             (otherwise (error "No range restriction for attribute ~A declared. Thus the datatype of
the constant ~A cannot be determined."
                               attribute value)))))
        (t (transform-predicate-1 form)))
   |#

  (transform-predicate-1 form))

(defun transform-predicate-1 (form)
  (cond ((null form) nil)
        ((symbolp form)
         (if *role-store*
           (let ((role (get-role form)))
             (if (null role)
               form
               (role-name role)))
           form))
        ((consp form)
         (cons (transform-predicate-1 (car form))
               (transform-predicate-1 (cdr form))))
        (t form)))



(defun compute-parameters (form)
  (remove-duplicates (loop for elem in (rest form) 
                           if (and (symbolp elem) (not (eq elem 't)) (not (eq elem 'nil)))
                           collect elem
                           else 
                           if (consp elem)
                           nconc (compute-parameters elem))))

(defun compute-parameter-sequence (form)
  (loop for elem in (rest form) 
        if (and (symbolp elem) (not (eq elem 't)) (not (eq elem 'nil)))
        collect elem
        else 
        if (consp elem)
        nconc (compute-parameters elem)))


(defun form-represents-integer-predicate-p (form)
  (and (consp form) 
       (or (eq (first form) 'min) 
           (eq (first form) 'max))))

(defun form-represents-linear-predicate-p (form)
  (and (member (first form) '(>= > < <= equal unequal = <>))
       (linear-term-p (second form))
       (linear-term-p (third form))
       (null (rest (rest (rest form))))))

(defun linear-term-p (term)
  (or (not (consp term))
      (if (consp term) 
        (if (eq (first term) '+)
          (loop for subterm in (rest term) 
                always (linear-term-1-p subterm))
          (linear-term-1-p term)))))

(defun linear-term-1-p (term)
  (or (realp term)
      (symbolp term)
      (and (consp term)
           (eq (first term) '*)
           (realp (second term))
           (symbolp (third term))
           (null (rest (rest (rest term)))))))

(defun form-represents-nonlinear-predicate-p (form)
  (and (member (first form) '(= <>))
       (nonlinear-term-p (second form))
       (nonlinear-term-p (third form))
       (null (rest (rest (rest form))))))

(defun nonlinear-term-p (term)
  (cond ((consp term) 
         (cond ((eq (first term) '+)
                (loop for subterm in (rest term) 
                      always (nonlinear-term-p subterm)))
               ((eq (first term) '-)
                (loop for term in (rest term) 
                      always (nonlinear-term-p term)))
               (t (nonlinear-term-1-p term))))
        ((symbolp term))
        ((numberp term)
         (or (integerp term)
             (error "You specified a non-integer number ~A as a coefficient. ~
                     Please normalize the (in-)equation first."
                    term)))))

(defun nonlinear-term-1-p (term)
  (cond ((consp term)
         (or (nonlinear-term-2-p term)
             (and (eq (first term) '*)
                  (cond ((integerp (second term))
                         (loop for var in (rest (rest term)) always (or (symbolp var) (nonlinear-term-2-p var))))
                        ;;((loop for var in (rest term) always (or (symbolp var) (nonlinear-term-2-p var))))
                        ;; I removed this in order to make the syntax require a coefficient even in the nonlinear case.
                        ;; Although not necessary for the nonlinear case, syntax is more orthogonal.
                        ((numberp (second term))
                         (error "You specified a non-integer number ~A as a coefficient in ~A. ~
                                 Please normalize the (in-)equation first."
                                (second term) term))))))
        ((symbolp term))
        ((numberp term)
         (or (integerp term)
             (error "You specified a non-integer number ~A as a coefficient. ~
                     Please normalize the (in-)equation first."
                    term)))
        (t nil)))

(defun nonlinear-term-2-p (term)
  (and (consp term)
       (eq (first term) 'expt)
       (symbolp (second term))
       (integerp (third term))))

(defun top-or-bottom-predicate-p (predicate)
  (member (predicate-operator predicate) '(top bottom)))

(defun form-represents-divisible-predicate-p (form)
  (and (consp form) 
       (or (eq (first form) 'divisible)
           (eq (first form) 'not-divisible))
       (symbolp (second form))
       (or (and (integerp (third form))
                (> (third form) 0))
           (symbolp (third form)))))

(defun form-represents-string-predicate-p (form)
  (and (consp form) 
       (or (eq (first form) 'string=)
           (eq (first form) 'string<>))
       (symbolp (second form))
       (or (stringp (third form))
           (symbolp (third form)))))

(defun form-represents-boolean-predicate-p (form)
  (and (consp form) 
       (or (eq (first form) 'boolean=)
           (eq (first form) 'boolean<>))
       (symbolp (second form))
       (or (true? (third form))
           (false? (third form)))))



(defun encode-not-term (list-term)
  "Recursively encode and simplify a negated concept term: (not <...>)"
  (unless (eql (length list-term) 2)
    (error "Illegal syntax for NOT term in ~S: ~
            syntax should be (NOT <concept>)" list-term))
  (let* ((term (second list-term))
         (encoded-term (encode-concept-term-1 term)))
    (cond
     ((eq encoded-term *top-concept*)
      (if (and (listp term) (eq (first term) 'self-reference))
          *top-concept*
        *bottom-concept*))
     ((eq encoded-term *bottom-concept*) *top-concept*)
     ((atomic-concept-p encoded-term)
      (if (and *use-tbox* 
               (concept-definition encoded-term)
               (concept-encoded-definition encoded-term))
        (let ((result (register-new-concept 'not encoded-term)))
          (unless (and (atomic-concept-p encoded-term)
                       (concept-primitive-p encoded-term))
            (setf (concept-told-subsumers result)
                  (concept-told-subsumers
                   (concept-negated-concept
                    (concept-encoded-definition encoded-term)))))
          result)
        (register-new-concept 'not encoded-term)))
     (t (get-negated-concept encoded-term)))))

(defun filter-concepts (clause-list negate-p)
  (if (<= (length clause-list) 100)
    (filter-concepts-1 clause-list negate-p)
    (filter-concepts-2 clause-list negate-p)))

(defun filter-concepts-1 (clause-list negate-p)
  "Recursively encode, normalize, and simplify a list of concept terms from an
and-list (negate-p=nil) or an or-list (negate-p=t). Nested and/or lists are
flattened, duplicates are removed, and primitive clashes are detected."
  (loop with encoded-list-1 = nil
        with negate-type = (if negate-p 'and 'or)
        with use-elh-model-embedding = *use-elh-model-embedding*
        with optimize-to-bottom-during-encoding = *optimize-to-bottom-during-encoding*
        with top = *top-concept*
        with bottom = *bottom-concept*
        with datatype-bottom = *datatype-bottom-concept*
        with top-datatype-p = nil
        for clause in clause-list
        for concept-or-list-1 = (encode-concept-term-1 clause)
        for concept-or-list = (if *flatten-encoded-concepts*
                                (if (concept-p-internal concept-or-list-1)
                                  (if negate-p
                                    (if (or-concept-p concept-or-list-1)
                                      (cons 'or (concept-term concept-or-list-1))
                                      concept-or-list-1)
                                    (if (and-concept-p concept-or-list-1)
                                      (cons 'and (concept-term concept-or-list-1))
                                      concept-or-list-1))
                                  concept-or-list-1)
                                concept-or-list-1)
        for concept = (if (concept-p-internal concept-or-list)
                        (if negate-p 
                          (concept-negated-concept concept-or-list)
                          concept-or-list)
                        (when (eq (first concept-or-list) negate-type)
                          (set-told-subsumer-of-or
                           (set-language
                            (if (eq negate-type 'and)
                              (concept-negated-concept
                               (register-new-concept
                                'and (sort-concept-list (rest concept-or-list))))
                              (register-new-concept
                               'or (sort-concept-list (rest concept-or-list)))))
                           nil)))
        do
        (if concept
          (let ((negated-concept (concept-negated-concept-internal concept)))
            (if (and optimize-to-bottom-during-encoding
                     (or (eq concept bottom)
                         (and negated-concept
                              (member negated-concept encoded-list-1))))
                (return bottom)
              (if  (and optimize-to-bottom-during-encoding
			(is-bottom-datatype-concept-p concept))
                  (return datatype-bottom)
                 (if (is-top-datatype-concept-p concept)
		     (setf top-datatype-p t)
		     (unless (eq concept top)
		       (pushnew concept encoded-list-1))))))
          (loop with encoded-list-2 = nil
                with encoded-list-1-elh-p = (when use-elh-model-embedding
                                              (subset-el+-p
                                               (get-language-from-concepts encoded-list-1 nil)))
                for and-clause in (rest concept-or-list)
                for clause = (if negate-p
                               (concept-negated-concept and-clause)
                               and-clause)
                do
                (if (and optimize-to-bottom-during-encoding
                         (or (not use-elh-model-embedding)
                             (not (and encoded-list-1-elh-p
                                       (subset-el+-p (concept-language clause)))))
                         (member (concept-negated-concept clause) encoded-list-1))
                  (return-from filter-concepts-1 bottom)
                  (push clause encoded-list-2))
                finally (setf encoded-list-1 (union encoded-list-2 encoded-list-1))))
        finally (return (values encoded-list-1 top-datatype-p))))

(defun filter-concepts-2 (clause-list negate-p)
  "Recursively encode, normalize, and simplify a list of concept terms from an
and-list (negate-p=nil) or an or-list (negate-p=t). Nested and/or lists are
flattened, duplicates are removed, and primitive clashes are detected."
  (loop with encoded-list-1 = (racer-make-hash-table :size (length clause-list)
                                                     :structure-p t)
        with negate-type = (if negate-p 'and 'or)
        with use-elh-model-embedding = *use-elh-model-embedding*
        with optimize-to-bottom-during-encoding = *optimize-to-bottom-during-encoding*
        with top = *top-concept*
        with bottom = *bottom-concept*
        with datatype-bottom = *datatype-bottom-concept*
        with top-datatype-p = nil
        for clause in clause-list
        for concept-or-list-1 = (encode-concept-term-1 clause)
        for concept-or-list = (if *flatten-encoded-concepts*
                                  (if (concept-p-internal concept-or-list-1)
                                      (if negate-p
                                          (if (or-concept-p concept-or-list-1)
                                              (cons 'or (concept-term concept-or-list-1))
                                            concept-or-list-1)
                                        (if (and-concept-p concept-or-list-1)
                                            (cons 'and (concept-term concept-or-list-1))
                                          concept-or-list-1))
                                    concept-or-list-1)
                                concept-or-list-1)
        for concept = (if (concept-p-internal concept-or-list)
                          (if negate-p 
                              (concept-negated-concept concept-or-list)
                            concept-or-list)
                        (when (eq (first concept-or-list) negate-type)
                          (set-told-subsumer-of-or
                           (set-language
                            (if (eq negate-type 'and)
                                (concept-negated-concept
                                 (register-new-concept
                                  'and (sort-concept-list (rest concept-or-list))))
                              (register-new-concept
                               'or (sort-concept-list (rest concept-or-list)))))
                           nil)))
        do
        (if concept
            (let ((negated-concept (concept-negated-concept-internal concept)))
              (if (and optimize-to-bottom-during-encoding
                       (or (eq concept bottom)
                           (and negated-concept
                                (gethash negated-concept encoded-list-1))))
                  (return bottom)
                (if (and optimize-to-bottom-during-encoding
                         (is-bottom-datatype-concept-p concept))
                    (return datatype-bottom)
                  (if (is-top-datatype-concept-p concept)
                      (setf top-datatype-p t)
                    (unless (eq concept top)
                      (setf (gethash concept encoded-list-1) t))))))
          (loop with encoded-list-1-elh-p = (when use-elh-model-embedding
                                              (subset-el+-p
                                               (get-language-from-concepts encoded-list-1)))
                for and-clause in (rest concept-or-list)
                for clause = (if negate-p
                                 (concept-negated-concept and-clause)
                               and-clause)
                do
                (if (and optimize-to-bottom-during-encoding
                         (or (not use-elh-model-embedding)
                             (not (and encoded-list-1-elh-p
                                       (subset-el+-p (concept-language clause)))))
                         (gethash (concept-negated-concept clause) encoded-list-1))
                    (return-from filter-concepts-2 bottom)
                  (setf (gethash clause encoded-list-1) t))))
        finally
        (return (values (sort-concept-list
                         (loop for concept being the hash-key of encoded-list-1
                               collect concept))
                        top-datatype-p))))

(defun encode-and-term (and-list negate-p)
  "Recursively encode, normalize, and simplify an and-term"
  (multiple-value-bind (encoded-list top-datatype-p)
      (filter-concepts (rest and-list) negate-p)
    (if (eq encoded-list *bottom-concept*)
        *bottom-concept*
      (if (eq encoded-list *datatype-bottom-concept*)
          *datatype-bottom-concept*
        (let ((new-encoded-list (if (and *simplify-and-or* (not negate-p))
                                    (simplify-and-or-list encoded-list)
                                  encoded-list)))
          (cond ((eq new-encoded-list *bottom-concept*) *bottom-concept*)
                ((concept-p-internal new-encoded-list) new-encoded-list)
                ((rest new-encoded-list) (cons 'and new-encoded-list))
                (new-encoded-list (first new-encoded-list))
                (t (if top-datatype-p
                       *datatype-top-concept*
                     *top-concept*))))))))

(defun encode-disjoint-and-term (and-list)
  (if (third and-list)
    (let ((concept (register-new-concept 'and
                                         (sort-concept-list (mapcar #'encode-not-term
                                                                    (rest and-list)))
                                         :disjoint-and t)))
      (set-language concept)
      (setf (concept-told-disjoints concept)
            (mapcar #'concept-negated-concept (concept-term concept)))
      concept)
    (encode-concept-term-1 (second and-list))))

(defun simplify-and-or-list (and-list)
  (let ((and-list-length (length and-list)))
    (if (<= and-list-length 10)
      (let ((new-and-list
             (loop with top = *top-concept*
                   with bottom = *bottom-concept*
                   for conjunct in and-list
                   for new-conjunct = 
                   (if (and (or-concept-p conjunct)
                            (<= (length (concept-term conjunct)) 10))
                       (loop for disjunct in (concept-term conjunct)
                             for match = (find-if #'(lambda (conjunct)
                                                      (or (eq conjunct disjunct)
                                                          (eq (concept-negated-concept
                                                               conjunct)
                                                              disjunct)))
                                                  and-list)
                             if (eq match disjunct)
                             do (return *top-concept*)
                             else
                             when (eq match (concept-negated-concept disjunct))
                             collect disjunct into removed
                             finally
                             (if removed
                                 (return
                                  (encode-concept-term
                                   `(or . ,(set-difference (concept-term conjunct)
                                                           removed))))
                               (return conjunct)))
                     conjunct)
                   if (eq new-conjunct bottom)
                   do (return-from simplify-and-or-list bottom)
                   else
                   unless (eq new-conjunct top)
                   collect new-conjunct)))
        (if (rest new-and-list)
          new-and-list
          (first new-and-list)))
      and-list)))

(defun resolve-and-or-label (label)
  (loop for concept in label
        for resolvent =
        (when (or-concept-p concept)
          (loop for disjunct in (concept-term concept)
                for match = (loop for label-concept in label
                                  when (eq (concept-negated-concept label-concept) disjunct)
                                  do (return label-concept))
                when match
                collect disjunct into removed
                finally
                (if removed
                    (return
                     (without-optimized-encoding
                       (encode-concept-term
                        `(or . ,(set-difference (concept-term concept)
                                                removed)))))
                  (return concept))))
        when resolvent
        collect resolvent into added-concepts
        finally
        (if added-concepts
            (return (nconc added-concepts label))
          (return label))))

(defun encode-or-term (or-list)
  "Recursively encode, normalize, and simplify an or-term as negated and-term"
  (get-negated-concept (encode-and-term (cons 'and (rest or-list)) t)))

(defun role-term-p (term)
  (or (symbolp term)
      (and (consp term) 
           (eql 2 (length term))
           (eq (first term) 'inv))))

(defun encode-self-term (list-term)
  (let ((role-name (second list-term)))
    (unless (and (eql (length list-term) 2)
                 (or (role-term-p role-name) (role-node-p role-name)))
      (error "Illegal syntax for self-term ~S" list-term))
    (let ((role (encode-role-term role-name))
          (top *top-concept*)
          (bottom *bottom-concept*))
      (if (not (role-simple-p role))
          (progn
            (racer-warn "Self restriction ~A cannot refer to non-simple role ~A and is ignored"
                        list-term (role-name role))
            top)
        (if (or (eq (role-domain-restriction role) bottom)
                (eq (role-range-restriction role) bottom))
            bottom
          (let* ((ignore-role-domain-range *ignore-role-domain-range*)
                 (domain (unless ignore-role-domain-range
                           (role-domain-restriction role)))
                 (range (unless ignore-role-domain-range
                          (role-range-restriction role))))
            (if (and (not ignore-role-domain-range)
                     (or (and domain (eq (find-encoded-concept domain) bottom))
                         (and range (eq (find-encoded-concept range) bottom))))
                bottom
              (let* ((new-concept (register-new-concept 'self-reference nil :role role))
                     (augmented-term
                      (when (or domain range)
                        (delete nil `(and ,domain ,range ,new-concept)))))
                (if (and augmented-term (eq (find-encoded-concept augmented-term) bottom))
                    bottom
                  (let ((augmented-encoded-term
                         (when augmented-term
                           (with-flatten-encodings
                             (encode-concept-term-1 augmented-term)))))
                    (if (eq augmented-encoded-term bottom)
                        (add-encoded-concept augmented-term bottom)
                      (progn
                        (when (some-concept-p new-concept)
                          (when (and domain
                                     (not (eq domain top))
                                     (not (eq domain (concept-role-domain new-concept))))
                            (setf (concept-role-domain new-concept) domain)
                            (when (atomic-concept-p domain)
                              (pushnew domain (concept-told-subsumers new-concept))))
                          (when (and range
                                     (not (eq range top))
                                     (not (eq range (concept-role-range new-concept))))
                            (setf (concept-role-range new-concept) range)))
                        new-concept))))))))))))

(defparameter *repair-datatypes* t)

(defun encode-some-term (list-term)
  "Recursively encode, normalize, and simplify a some-term"
  (let ((role-name (second list-term))
        (bottom *bottom-concept*))
    (unless (and (eql (length list-term) 3)
                 (or (role-term-p role-name) (role-node-p role-name)))
      (error "Illegal syntax for some-term ~S" list-term))
    (let ((qualification (encode-concept-term (third list-term)))
          (role (encode-role-term role-name)))
      (cond ((or (eq (role-domain-restriction role) bottom)
                 (eq (role-range-restriction role) bottom))
             bottom)
            ((role-cd-attribute role)
             (error "illegal concrete domain attribute in ~A" list-term))
            ((role-datatype role)
             (if *repair-datatypes*
                 (progn
                   (racer-warn "Bug in Racer:~A is illegal due to datatype property ~A. Repaired to ~A"
                               list-term role-name `(d-some .,(rest list-term)))
                   (encode-datatype-existential-restriction `(d-some .,(rest list-term))))
               (progn
                 (racer-warn "~A is inconsistent due to datatype property ~A"
                             list-term role-name)
                 bottom)))
            ((eq qualification bottom)
             bottom)
            (t (encode-some-term-internal role qualification))))))

(defun encode-some-term-internal (role qualification)
  "Recursively encode, normalize, and simplify a some-term"
  (let* ((top *top-concept*)
         (bottom *bottom-concept*)
         (range (role-range-restriction role))
         (use-range (and range (not *ignore-role-domain-range*)))
         (range-term (if use-range
                         `(and ,range ,qualification)
                       qualification))
         (range-concept (if use-range
                            (with-flatten-encodings
                              (without-taxonomic-encoding
                                (encode-concept-term range-term)))
                          range-term)))
    (if (and use-range (eq range-concept bottom))
        (add-encoded-concept range-term bottom)
      (let* ((domain (unless *ignore-role-domain-range*
                       (role-domain-restriction role)))
             (domain-concept (or domain top))
             (some-concept (register-new-concept 'some qualification :role role))
             (new-concept some-concept)
             (augmented-term
              (when (or domain range)
                `(and ,domain-concept ,new-concept))))
        (if (and augmented-term (eq (find-encoded-concept augmented-term) bottom))
            bottom
          (let ((augmented-encoded-term
                 (when augmented-term
                   (with-flatten-encodings
                     (encode-concept-term-1 augmented-term)))))
            (if (eq augmented-encoded-term bottom)
                (add-encoded-concept augmented-term bottom)
              (progn
                (when (some-concept-p new-concept)
                  (when (and domain
                             (not (eq domain-concept top))
                             (not (eq domain-concept (concept-role-domain new-concept))))
                    (setf (concept-role-domain new-concept) domain-concept)
                    (when (and (atomic-concept-p domain-concept) (not (eq domain-concept top)))
                      (pushnew domain-concept (concept-told-subsumers new-concept))))
                  (when (and range
                             (not (eq range-concept top))
                             (not (eq range-concept qualification))
                             (not (eq range-concept (concept-role-range new-concept))))
                    (setf (concept-role-range new-concept) range-concept)))
                new-concept))))))))

(defun valid-general-cd-concept-p (concept)
  (valid-general-cd-concept-p-1 concept (type-of concept)))

(defun valid-general-cd-concept-p-1 (concept type)
  ;;; return 2 values: cd-p, bottom-p
  (case type 
    ((and-concept or-concept)
     (loop for elem in (concept-term concept)
           do
           (multiple-value-bind (cd-p bottom-p)
               (valid-general-cd-concept-p-1 elem (type-of elem))
             (unless cd-p
               (return nil))
             (when (and bottom-p (eq type 'and-concept))
               (return (values t t))))
           finally (return t)))
    (ensure-cd-concept t)
    (cd-concept
     (values t (eq (predicate-operator (concept-predicate concept)) 'bottom)))
    (general-cd-concept
     (if (is-bottom-datatype-concept-p concept)
         (values t t)
       t))
    (t nil)))

(defun reencode-exists-concept (tbox exists-concept)
  (let* ((role (concept-role exists-concept))
         (new-role (get-tbox-role tbox (role-name role)))
         (qualification (concept-term exists-concept))
         (role-domain (role-domain-restriction new-role))
         (domain-concept (or role-domain *top-concept*))
         (range (role-range-restriction new-role))
         (range-concept (if range
                          (with-flatten-encodings
                            (encode-concept-term `(and ,range ,qualification)))
                          qualification)))
    (unless (eq role new-role)
      ;;concept role must be changed because role is now a synonym of new-role
      (setf (concept-role exists-concept) new-role))
    (when role-domain
      (setf (concept-role-domain exists-concept) domain-concept)
      (setf (concept-told-subsumers exists-concept)
            (concept-set-union (concept-told-subsumers role-domain)
                               (concept-told-subsumers exists-concept))))
    (when (and range
               (not (eq range-concept *top-concept*))
               (not (eq range-concept qualification)))
      (setf (concept-role-range exists-concept) range-concept))
    exists-concept))

(defun reencode-quantification-concept (tbox quantification-concept)
  (let* ((role (concept-role quantification-concept))
         (new-role (get-tbox-role tbox (role-name role)))
         (qualification (concept-term quantification-concept))
         (range (role-range-restriction new-role))
         (range-concept (if range
                            (with-flatten-encodings
                              (encode-concept-term `(and ,range ,qualification)))
                          qualification)))
    (unless (eq role new-role)
      ;;concept role must be changed because role is now a synonym of new-role
      (setf (concept-role quantification-concept) new-role))
    (when (exists-concept-p quantification-concept)
      (let* ((role-domain (role-domain-restriction new-role))
             (domain-concept (or role-domain *top-concept*)))
        (when role-domain
          (setf (concept-role-domain quantification-concept) domain-concept)
          (setf (concept-told-subsumers quantification-concept)
                (concept-set-union (concept-told-subsumers role-domain)
                                   (concept-told-subsumers quantification-concept))))
        (when (and range
                   (not (eq range-concept *top-concept*))
                   (not (eq range-concept qualification)))
          (setf (concept-role-range quantification-concept) range-concept))))
    quantification-concept))

(defun encode-all-term (list-term)
  "Recursively encode, normalize, and simplify an all-term"
  (let ((role-name (second list-term)))
    (unless (and (eql (length list-term) 3)
                 (or (role-term-p role-name) (role-node-p role-name)))
      (error "Illegal syntax for all-term ~S" list-term))
    (let ((qualification (encode-concept-term (third list-term)))
          (role (encode-role-term role-name))
          (top *top-concept*)
          (bottom *bottom-concept*))
      (if (role-datatype role)
          (progn
            (if *repair-datatypes*
                (progn
                  (racer-warn "Bug in Racer:~A is illegal due to datatype property ~A. Repaired to ~A"
                              list-term role-name `(d-all .,(rest list-term)))
                  (encode-datatype-value-restriction `(d-all .,(rest list-term))))
              (progn
                (racer-warn "~A is inconsistent due to datatype property ~A"
                            list-term role-name)
                bottom)))
        (if (eq (role-domain-restriction role) bottom)
            top
          (progn
            (when (and (role-feature-p role) (role-transitive-p role))
              (error "a transitive feature ~A is not allowed" role))
            (when (and (role-datatype role) (not (valid-general-cd-concept-p qualification)))
              (error "datatype property ~S cannot have a concept ~S as value" role (third list-term)))
            (cond ((role-cd-attribute role)
                   (error "illegal concrete domain attribute in ~A" list-term))
                  ((eq qualification top)
                   top)
                  (t 
                   (let ((concept (get-negated-concept
                                   (encode-some-term-internal role (concept-negated-concept qualification)))))
                     (when (role-reflexive-p role)
                       (setf (concept-told-subsumers concept)
                             (concept-set-union (concept-told-subsumers qualification)
                                                (concept-told-subsumers concept))))
                     concept)))))))))

(defun encode-ria-initial-concept (list-term)
  (get-negated-concept
   (encode-neg-ria-initial-concept-internal (encode-role-term (second list-term))
                                            (concept-negated-concept (encode-concept-term (third list-term))))))

(defun encode-neg-ria-initial-concept (list-term)
  (encode-neg-ria-initial-concept-internal (encode-role-term (second list-term))
                                           (encode-concept-term (third list-term))))

(defun encode-neg-ria-initial-concept-internal (role qualification)
  (register-new-concept 'i-some qualification :role role))

(defun encode-ria-final-concept (list-term)
  (get-negated-concept
   (encode-neg-ria-final-concept-internal (encode-role-term (second list-term))
                                          (concept-negated-concept (encode-concept-term (third list-term))))))

(defun encode-neg-ria-final-concept (list-term)
  (encode-neg-ria-final-concept-internal (encode-role-term (second list-term))
                                         (encode-concept-term (third list-term))))

(defun encode-neg-ria-final-concept-internal (role qualification)
  (register-new-concept 'f-some qualification :role role))

(defun encode-at-least-term (list-term)
  "Recursively encode, normalize, and simplify an at-least-term"
  (let* ((number (second list-term))
         (role-name (third list-term))
         (qualification (or (fourth list-term) +top-symbol+))
         (top *top-concept*)
         (bottom *bottom-concept*))
    (unless (and (<= 3 (length list-term) 4)
                 (integerp number)
                 (not (minusp number))
                 (or (role-term-p role-name) (role-node-p role-name)))
      (error "Illegal syntax for at-least-term ~S" list-term))
    (if (zerop number)
        top
      (let ((encoded-qualification (encode-concept-term qualification)))
        (if (eq encoded-qualification bottom)
            bottom
          (let ((role (encode-role-term role-name)))
            (if (role-datatype role)
                (progn
                  (if *repair-datatypes*
                      (progn
                        (racer-warn "Bug in Racer:~A is illegal due to datatype property ~A. Repaired to ~A"
                                    list-term role-name
                                    `(d-at-least .,(if (eq (fourth list-term) +krss-top-symbol+)
                                                       (rest (butlast list-term))
                                                     (rest list-term))))
                        (encode-datatype-at-least-restriction 
                         `(d-at-least .,(if (eq (fourth list-term) +krss-top-symbol+)
                                            (rest (butlast list-term))
                                          (rest list-term)))))
                    (progn
                      (racer-warn "~A is inconsistent due to datatype property ~A"
                                  list-term role-name)
                      bottom)))
              (if (or (eq (role-domain-restriction role) bottom)
                      (eq (role-range-restriction role) bottom))
                  bottom
                (if (eql number 1)
                    (if (role-cd-attribute role)
                        (error "illegal concrete domain attribute in ~A" list-term)
                      (encode-some-term `(some ,role-name ,encoded-qualification)))
                  (if (role-transitive-p role)
                      (progn
                        (when *tbox-verbose*
                          (racer-warn "Transitive role ~A cannot be used in number restriction ~A~%~
                                 ignoring number restriction for role ~A in ~A"
                                      (role-name role) list-term
                                      (role-name role) list-term))
                        top)
                    (if (not (role-simple-p role))
                        (progn
                          (racer-warn "At-least restriction ~A cannot refer to non-simple role ~A and is ignored"
                                      list-term (role-name role))
                          top)
                      (let ((transitive-subrole
                             (find-user-defined-transitive-role (role-descendants-internal role))))
                        (if (and transitive-subrole
                                 (not (or (eq (role-domain-restriction transitive-subrole) bottom)
                                          (eq (role-range-restriction transitive-subrole)
                                              bottom))))
                            (progn
                              (when *tbox-verbose*
                                (racer-warn "Role ~A cannot be used in at-least number restriction ~A ~
                                     since ~A is a transitive subrole of role ~A~%~
                                     ignoring number restriction for role ~A in ~A"
                                            (role-name role) list-term (role-name transitive-subrole)
                                            (role-name role)
                                            (role-name role) list-term))
                              top)
                          (if (role-cd-attribute role)
                              (error "illegal concrete domain attribute in ~A" list-term)
                            (if (and (role-feature-p role) (> number 1))
                                (progn
                                  (when *tbox-verbose*
                                    (racer-warn "Term ~S is equivalent to ~S due to feature ~A~%"
                                                list-term +bottom-symbol+ (role-name role)))
                                  bottom)
                              (encode-at-least-term-internal number
                                                             role
                                                             encoded-qualification))))))))))))))))

(defun encode-at-least-term-internal (number role qualification)
  "Recursively encode, normalize, and simplify an at-least-term"
  (let* ((range (role-range-restriction role))
         (use-range (and range (not *ignore-role-domain-range*)))
         (range-term (if use-range
                         `(and ,range ,qualification)
                       qualification))
         (top *top-concept*)
         (bottom *bottom-concept*))
    (if (and use-range (eq (find-encoded-concept range-term) bottom))
      bottom
      (let ((range-concept (if use-range
                             (with-flatten-encodings
                               (without-taxonomic-encoding
                                 (encode-concept-term range-term)))
                             range-term)))
        (if (and use-range (eq range-concept bottom))
          (add-encoded-concept range-term bottom)
          (let* ((domain (unless *ignore-role-domain-range*
                   (role-domain-restriction role)))
                 (domain-concept (or domain top))
                 (new-concept (register-new-concept 'at-least qualification
                                                    :role role
                                                    :number number))
                 (augmented-term
                  (when (or domain range)
                    `(and ,domain-concept ,new-concept))))
            (if (and augmented-term (eq (find-encoded-concept augmented-term) bottom))
              bottom
              (let ((augmented-encoded-term
                     (when augmented-term
                       (with-flatten-encodings
                         (encode-concept-term-1 augmented-term)))))
                (if (eq augmented-encoded-term bottom)
                  (add-encoded-concept augmented-term bottom)
                  (progn
                    (when (at-least-concept-p new-concept)
                      (when (and domain
                                 (not (eq domain-concept top))
                                 (not (eq domain-concept (concept-role-domain new-concept))))
                        (setf (concept-role-domain new-concept) domain-concept)
                        (when (atomic-concept-p domain-concept)
                          (pushnew domain-concept (concept-told-subsumers new-concept))))
                      (when (and range
                                 (not (eq range-concept top))
                                 (not (eq range-concept qualification))
                                 (not (eq range-concept (concept-role-range new-concept))))
                        (setf (concept-role-range new-concept) range-concept)))
                    new-concept))))))))))

(defun encode-at-most-term (list-term)
  "Recursively encode, normalize, and simplify an at-most-term"
  (let ((number (second list-term))
        (role-name (third list-term)))
    (unless (and (<= 3 (length list-term) 4)
                 (integerp number)
                 (not (minusp number))
                 (or (role-term-p role-name) (role-node-p role-name)))
      (error "Illegal syntax for at-most-term ~S" list-term))
    (let ((role (encode-role-term role-name))
          (bottom *bottom-concept*))
      (if (role-datatype role)
          (progn
            (if *repair-datatypes*
                (progn
                  (racer-warn "Bug in Racer:~A is illegal due to datatype property ~A. Repaired to ~A"
                              list-term role-name
                              `(d-at-most .,(if (eq (fourth list-term) +krss-top-symbol+)
                                                (rest (butlast list-term))
                                              (rest list-term))))
                  (encode-datatype-at-most-restriction 
                   `(d-at-most .,(if (eq (fourth list-term) +krss-top-symbol+)
                                     (rest (butlast list-term))
                                   (rest list-term)))))
              (progn
                (racer-warn "~A is inconsistent due to datatype property ~A"
                            list-term role-name)
                bottom)))
        (let* ((qualification (or (fourth list-term) +top-symbol+))
               (top *top-concept*)
               (encoded-qualification (encode-concept-term qualification)))
          (if (eq encoded-qualification bottom)
              top
            (let ((role (encode-role-term role-name)))
              (if (zerop number)
                  (encode-all-term `(all ,role-name
                                         ,(concept-negated-concept encoded-qualification)))
                (if (eq (role-domain-restriction role) bottom)
                    top
                  (if (role-transitive-p role)
                      (progn
                        (when *tbox-verbose*
                          (racer-warn "Transitive role ~A cannot be used in number restriction ~A~%~
                               ignoring number restriction for role ~A in ~A"
                                      (role-name role) list-term
                                      (role-name role) list-term))
                        top)
                    (if (not (role-simple-p role))
                        (progn
                          (racer-warn "At-most restriction ~A cannot refer to non-simple role ~A and is ignored"
                                      list-term (role-name role))
                          top)
                      (let ((transitive-subrole
                             (find-user-defined-transitive-role (role-descendants-internal role))))
                        (if (and transitive-subrole
                                 (not (eq (role-domain-restriction transitive-subrole) bottom)))
                            (progn
                              (when *tbox-verbose*
                                (racer-warn "Role ~A cannot be used in at-most number restriction ~A ~
                                   since ~A is a transitive subrole of role ~A~%~
                                   ignoring number restriction for role ~A in ~A"
                                            (role-name role) list-term (role-name transitive-subrole)
                                            (role-name role)
                                            (role-name role) list-term))
                              top)
                          (cond
                           ((role-cd-attribute role)
                            (error "illegal concrete domain attribute in ~A" list-term))
                           ((and *simplify-redundant-at-most* (role-feature-p role) (not (zerop number)))
                            (when *tbox-verbose*
                              (racer-warn
                               "Term ~S is equivalent to ~S due to feature ~A~%"
                               list-term +top-symbol+ (role-name role)))
                            top)
                           (t (get-negated-concept
                               (encode-at-least-term-internal (1+ number)
                                                              role
                                                              encoded-qualification)))))))))))))))))

(defun encode-exactly-term (list-term)
  (let ((number (second list-term))
        (role-name (third list-term))
        (qualification (fourth list-term)))
    (unless (and (<= 3 (length list-term) 4)
                 (integerp number)
                 (not (minusp number))
                 (or (symbolp role-name) 
                     (role-node-p role-name)
                     (and (consp role-name)
                          (symbolp (second role-name))
                          (eq (first role-name) 'inv))))
      (error "Illegal syntax for at-most-term ~S" list-term))
    (if (and qualification 
             (not (member qualification (list +krss-top-symbol+ +top-symbol+ +datatype-top-symbol+))))
        (encode-and-term `(and (at-least ,number ,role-name ,qualification)
                               (at-most ,number ,role-name ,qualification))
                         nil)
      (encode-and-term `(and (at-least ,number ,role-name)
                             (at-most ,number ,role-name))
                       nil))))

(defun make-nnf-term (list-term)
  "Create NNF of a concept term represented as list of symbols"
  (if (symbolp list-term)
      list-term
    (ecase (first list-term)
      (not (if (or (symbolp (second list-term))
                   (and (consp (second list-term)) (eq (first (second list-term)) 'self-reference)))
               list-term
             (make-nnf-term (negate-concept-term (second list-term)))))
      ((some all)
       (if (symbolp (third list-term))
           list-term
         (list (first list-term) (second list-term) (make-nnf-term (third list-term)))))
      ((at-least at-most)
       (if (symbolp (fourth list-term))
           list-term
         (list (first list-term) (second list-term) (third list-term)
               (make-nnf-term (fourth list-term)))))
      ((and or)
       (list* (first list-term)
              (mapcar #'(lambda (term)
                          (if (symbolp (third list-term))
                              term
                            (make-nnf-term term)))
                      (rest list-term))))
      (self-reference
       list-term))))

(defun make-pseudo-nnf-term (list-term)
  "Create NNF of a concept term represented as list of symbols but do not walk into
quantifiers."
  (if (symbolp list-term)
      list-term
    (ecase (first list-term)
      (not (if (symbolp (second list-term))
               list-term
             (make-nnf-term (negate-concept-term (second list-term)))))
      ((some all at-least at-most) 
       list-term) ; We do not consider modalities -> pseudo nnf!
      ((and or)
       (list* (first list-term)
              (mapcar #'(lambda (term)
                          (if (symbolp (third list-term))
                              term
                            (make-nnf-term term)))
                      (rest list-term))))
      (self-reference
       list-term))))

(defun make-normalized-term (list-term)
  (with-alc-environment ()
    (with-alc-bindings
      (with-new-used-by-concept-store
        (with-flatten-encodings
          (decode-concept (encode-concept-term list-term)))))))

;;;===========================================================================

(defun generate-inverse-role (role-name)
  #+:debug (assert (symbolp role-name))
  (gensym (concatenate 'string 
                       (symbol-name role-name)
                       "-INV-")))

(defun encode-role-term (role-term &key (cd-attribute nil cd-attribute-spec-p))
  (or (when (role-node-p role-term)
        (when (and cd-attribute (null (role-cd-attribute role-term)))
          (when (role-ancestors-internal role-term)
            (error "role ~A in TBox ~A cannot be used as a concrete domain attribute and an abstract role"
                   (role-name role-term) (tbox-name *current-tbox*)))
          (setf (role-cd-attribute role-term) cd-attribute))
        role-term)
      (let ((inverse-role-descriptor-p (and (consp role-term)
                                            (= (length role-term) 2)
                                            (eq (first role-term) 'inv)
                                            (symbolp (second role-term)))))
        (cond (inverse-role-descriptor-p
               (when *use-tbox*
                 (ensure-role-in-tbox *use-tbox* (second role-term)))
               (if (get-role (second role-term))
                 (role-inverse-internal (get-role (second role-term)))
                 (let* ((role-name (generate-inverse-role (second role-term)))
                        (role (make-role-node :name role-name
                                              :reflexive-p *encode-roles-as-reflexive*
                                              :transitive-p *encode-roles-as-transitive*
                                              :internal-name-p t))
                        (inverse-role (make-role-node :name (second role-term)
                                                      :inverse-internal role
                                                      :reflexive-p *encode-roles-as-reflexive*
                                                      :transitive-p *encode-roles-as-transitive*)))
                   (when (and *use-tbox* (boundp '*provisionally-inserted-roles*))
                     (pushnew role *provisionally-inserted-roles*)
                     (pushnew inverse-role *provisionally-inserted-roles*))
                   (setf (role-inverse-internal role) inverse-role)
                   (setf (get-role (second role-term)) inverse-role)
                   (setf (role-ancestors-internal inverse-role) (list inverse-role))
                   (setf (role-ancestors-internal role) (list role))
                   (setf (role-descendants-internal inverse-role) (list inverse-role))
                   (setf (role-descendants-internal role) (list role))
                   (setf (get-role role-name) role))))
              ((and cd-attribute-spec-p (symbolp role-term))
               (or (let ((role (get-role role-term)))
                     (when (and role cd-attribute (null (role-cd-attribute role)))
                       (when (role-ancestors-internal role)
                         (error "role ~A in TBox ~A cannot be used as a concrete domain attribute and an abstract role"
                                (role-name role) (tbox-name *current-tbox*)))
                       (setf (role-cd-attribute role) cd-attribute))
                     role)
                   (let ((role (make-role-node :name role-term
                                               :cd-attribute cd-attribute)))
                     (setf (get-role role-term) role)
                     role)))
              ((symbolp role-term)
               (when *use-tbox*
                 (ensure-role-in-tbox *use-tbox* role-term))
               (or (get-role role-term)
                   (let* ((role-name role-term)
                          (role (make-role-node :name role-name
                                                :reflexive-p *encode-roles-as-reflexive*
                                                :transitive-p *encode-roles-as-transitive*))
                          (inverse-role (make-role-node :name (generate-inverse-role role-name)
                                                        :inverse-internal role
                                                        :reflexive-p *encode-roles-as-reflexive*
                                                        :transitive-p *encode-roles-as-transitive*
                                                        :internal-name-p t)))
                     (when (and *use-tbox* (boundp '*provisionally-inserted-roles*))
                       (pushnew role *provisionally-inserted-roles*)
                       (pushnew inverse-role *provisionally-inserted-roles*))
                     (setf (role-inverse-internal role) inverse-role)
                     (setf (role-ancestors-internal inverse-role) (list inverse-role))
                     (setf (role-descendants-internal inverse-role) (list inverse-role))
                     (setf (get-role (role-name inverse-role)) inverse-role)
                     (setf (role-ancestors-internal role) (list role))
                     (setf (role-descendants-internal role) (list role))
                     (setf (get-role role-name) role))))
              (t (error "not expected"))))))

(defun encode-tbox-role-term (role-name
			      cd-attribute
			      feature-p
			      transitive-p
			      inverse
			      inverse-feature-p
			      domain-concept
			      range-concept
			      reflexive-p
			      datatype
			      annotation-p
			      irreflexive-p
			      symmetric-p
			      asymmetric-p
                              compositions)
  (let ((encoded-role (or (and (role-node-p role-name) role-name)
                          (get-role role-name))))
    (when encoded-role
      (error "role ~S already encoded" encoded-role))
    (let ((role (make-role-node :name role-name
                                :feature-p feature-p
                                :transitive-p transitive-p
                                :reflexive-p (or reflexive-p
                                                 *encode-roles-as-reflexive*)
                                :inverse-internal inverse
				:inverse-feature-p inverse-feature-p
                                :domain-concept domain-concept
                                :range-concept range-concept
                                :cd-attribute cd-attribute
                                :datatype datatype
                                :annotation-p annotation-p
				:irreflexive-p irreflexive-p
				:symmetric-p symmetric-p
				:asymmetric-p asymmetric-p
                                :compositions compositions)))
      (setf (get-role role-name) role))))

