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

(defstruct (constraint-store
            (:conc-name cs-)
            (:constructor make-cs-internal (table))
            (:copier copy-constraint-store-internal))
  (table nil)                           ; hash table as index from ind-name to constraint list
  (copy-p nil)                          ; if T constraint-store and its table must be copied prior to modification
  )

(defmethod print-object ((object constraint-store) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (cs-copy-p object))
    (when (cs-table object)
      (format stream " ~,D" (hash-table-count (cs-table object))))))

(defstruct (expanded-constraint-store
            (:conc-name cs-)
            (:include constraint-store)
            (:constructor make-exp-cs-internal (table))
            (:copier copy-exp-cs-internal))
  (ind-label-cache nil)
  )

(defstruct (unexpanded-exists-constraint-store
            (:conc-name cs-)
            (:include constraint-store)
            (:constructor make-unexp-exists-cs-internal (table))
            (:copier copy-unexp-exists-cs-internal))
  )

(defstruct (unexpanded-disjunctive-constraint-store
            (:conc-name cs-)
            (:include constraint-store)
            (:constructor make-unexp-disjunctive-cs-internal (table))
            (:copier copy-unexp-disjunctive-cs-internal))
  )

(defstruct (unexpanded-store-index
            (:conc-name exp-index-)
            (:constructor make-unexpanded-store-index (cs-store-list cs-store-list-length table)))
  (cs-store-list nil)
  (cs-store-list-length 0)
  (base-table nil)			; list with 1 hash table containing the assertions from the precompletion 
  (table nil)                           ; hash table as index from ind-name to constraint store list
  )

(defstruct (expanded-store-index
	    (:include unexpanded-store-index)
            (:conc-name exp-index-)
            (:constructor make-expanded-store-index (cs-store-list cs-store-list-length base-table table))
            (:copier copy-expanded-store-index-internal))
  (copy-p nil)                          ; if T expanded-store-index and its table must be copied prior to modification
  )

(defmethod print-object ((object expanded-store-index) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S " (exp-index-copy-p object))
    (if (exp-index-base-table object)
	(format stream "~,D/" (hash-table-count (cs-table (first (exp-index-base-table object)))))
      (format stream "~S/" nil))
    (if (exp-index-table object)
	(format stream "~,D" (hash-table-count (exp-index-table object)))
     (format stream "~S" nil))))

(defun copy-expanded-store-index (expanded-store-index &optional (added-size 0))
  (let ((new-expanded-store-index (copy-expanded-store-index-internal expanded-store-index)))
    (setf (exp-index-table new-expanded-store-index)
          (copy-hash-table (exp-index-table expanded-store-index) added-size))
    (setf (exp-index-copy-p new-expanded-store-index) nil)
    new-expanded-store-index))

(race-inline (type-of-constraint-store))

(defun make-constraint-store (type &optional (table nil))
  (let ((table (or table (racer-make-hash-table))))
    (ecase type
      (:expanded
       (incf-statistics *number-of-created-expanded-constraint-stores*)
       (make-exp-cs-internal table))
      (:unexpanded-exists
       (incf-statistics *number-of-created-unexp-exists-constraint-stores*)
       (make-unexp-exists-cs-internal table))
      (:unexpanded-disjunctive
       (incf-statistics *number-of-created-unexp-disjunctive-constraint-stores*)
       (make-unexp-disjunctive-cs-internal table)))))

(defun type-of-constraint-store (store)
  (ecase (type-of store)
    (expanded-constraint-store :expanded)
    (unexpanded-exists-constraint-store :unexpanded-exists)
    (unexpanded-disjunctive-constraint-store :unexpanded-disjunctive)))

(defun copy-constraint-store (store type)
  (let ((table (copy-hash-table (cs-table store))))
    (ecase type
      (:expanded
       (incf-statistics *number-of-copied-expanded-constraint-stores*)
       (make-exp-cs-internal table))
      (:unexpanded-exists
       (incf-statistics *number-of-copied-unexp-exists-constraint-stores*)
       (make-unexp-exists-cs-internal table))
     (:unexpanded-disjunctive
       (incf-statistics *number-of-copied-unexp-disjunctive-constraint-stores*)
       (make-unexp-disjunctive-cs-internal table)))))

(defun merge-constraint-stores (store-1 store-2)
  #+:debug (assert (and (expanded-constraint-store-p store-1)
                        (expanded-constraint-store-p store-2)))
  (let ((merged-store (make-exp-cs-internal (merge-hash-tables (cs-table store-1)
                                                               (cs-table store-2)))))
    (setf (cs-copy-p merged-store) (or (cs-copy-p store-1) (cs-copy-p store-2)))
    (incf-statistics *number-of-merged-expanded-constraint-stores* 2)
    merged-store))

(race-inline (new-unused-constraint-store
              constraint-store-changed-p
              constraint-store-unused-p
              constraint-store-empty-p
              swap-to-expanded-constraint-store-p
              swap-to-unexpanded-constraint-store-p
              swap-to-constraint-store
              get-constraint-store-entry
              set-constraint-store-entry))

;;;------------------------------------------------------------------------------------

(defun new-unused-constraint-store ()
  +unused-constraint-store+)

(defun constraint-store-changed-p (store-1 store-2)
  (and (consp store-1)
       (consp store-2)
       (not (equal store-1 store-2))))

(defun constraint-store-unused-p (store)
  (eq store +unused-constraint-store+))

(defun constraint-store-empty-p (store)
  #+:debug (assert (consp store))
  (if (rest store)
    (progn
      #+:debug
      (assert (loop for c-store in store
                    for table = (cs-table c-store)
                    never (eql (hash-table-count table) 0)))
      nil)
    (eql (hash-table-count (cs-table (first store))) 0)))

(defun ensure-copying-of-constraint-store (constraint-store &optional (constraint-store-index nil))
  (when (consp constraint-store)
    (loop for store in constraint-store do
          (unless (cs-copy-p store)
            (setf (cs-copy-p store) t)))
    (when constraint-store-index
      (unless (exp-index-copy-p constraint-store-index)
	(setf (exp-index-copy-p constraint-store-index) t)))))

(defun swap-to-expanded-constraint-store-p (list constraint-store)
  (and *use-expanded-store*
       (not (constraint-store-unused-p constraint-store))
       (>= (length list) *swap-to-expanded-store-threshold*)))

(defun swap-to-unexpanded-constraint-store-p (list constraint-store)
  (and (or *use-unexpanded-exists-constraints-store*
           *use-unexpanded-disjunctive-constraints-store*)
       (not (constraint-store-unused-p constraint-store))
       (>= (length list) *swap-to-unexpanded-store-threshold*)))

(defun add-to-constraint-store (list
                                    store
                                    type
				    &key
				    (index nil)
                                    (extend-store-p nil)
                                    (copy-p t))
  #+:debug (assert list)
  #+(and :ccl :debug)
  (when (boundp '*debug2*)
    (setf copy-p t))
  (when-statistics
    (when list
      (ecase type
        (:expanded
         (incf-statistics *number-of-swaps-to-expanded-constraint-stores*)
         (incf-statistics *number-of-swapped-expanded-constraints* (length list)))
        (:unexpanded-exists
         (incf-statistics *number-of-swaps-to-unexp-exists-constraint-stores*)
         (incf-statistics *number-of-swapped-unexp-exists-constraints* (length list)))
        (:unexpanded-disjunctive
         (incf-statistics *number-of-swaps-to-unexp-disjunctive-constraint-stores*)
         (incf-statistics *number-of-swapped-unexp-disjunctive-constraints*
                          (length list))))))
  (let* ((extend-old-store (and (consp store)
                                (eq type :expanded)
                                (or extend-store-p
                                    ;(not (or copy-p (cs-copy-p (first store))))
				    )))
         (new-store (if extend-old-store
                      (if (or copy-p (cs-copy-p (first store)))
                        (copy-constraint-store (first store) type)
                        (progn
                          (incf-statistics *number-of-extend-swaps-to-expanded-constraint-stores*)
                          (first store)))
                      (make-constraint-store type)))
         (table (cs-table new-store))
         (store (if (and (not extend-old-store)
                         (>= (length store) *max-constraint-stores-length*))
                  (progn
                    (maxf-statistics *max-length-of-expanded-constraint-stores*
                                     (1+ *max-constraint-stores-length*))
                    (compact-constraint-stores store type))
                  store))
         (label-cache (when (and (expanded-constraint-store-p new-store)
                                 *inverse-roles*)
                        (cs-ind-label-cache new-store))))
    (if (rest list)
      (loop with collection = (list (first list))
            for constraint in (rest list)
            for old-ind = (constraint-ind (first list)) then ind
            for ind = (constraint-ind constraint)            
            do
            (if (eql ind old-ind)
              (push constraint collection)
              (progn
                (when (and label-cache (gethash old-ind label-cache))
                  (incf-statistics *number-of-removed-label-cache-entries*)
                  (remhash old-ind label-cache))
                (if (rest collection)
                  (setf (gethash old-ind table)
                        (nconc (nreverse collection) (gethash old-ind table)))
                  (setf (gethash old-ind table)
                        (nconc collection (gethash old-ind table))))
                (setf collection (list constraint))))
            finally
            (when collection
              (when (and label-cache (gethash ind label-cache))
                (incf-statistics *number-of-removed-label-cache-entries*)
                (remhash ind label-cache))
              (setf (gethash ind table)
                    (nconc (nreverse collection) (gethash ind table)))))
      (progn
        (when (and label-cache (gethash (constraint-ind (first list)) label-cache))
          (remhash (constraint-ind (first list)) label-cache))
        (push (first list) (gethash (constraint-ind (first list)) table))))
    (let* ((result (if extend-old-store
                     (if (consp store)
                       (cons new-store (rest store))
                       (list new-store))
                     (cons new-store store)))
	   (new-index
            (if (and (eq type :expanded) *use-expanded-stores-index*)
              (if index
                (if extend-old-store
                  (error "unexpected:~S" (list index result store))
                  (if (>= (- (length result) (exp-index-cs-store-list-length index))
                          5)
                    (update-expanded-store-index result index)
                    index))
                (when (>= (length result) 5)
                  (create-expanded-store-index result)))
              index)))
      (race-trace ("~&Swapping to constraint store: copy-p=~S, old-store=~S ~
                    new-store=~S new-stores=~S constraints=~S~%"
                   copy-p store (copy-constraint-store new-store type) result list))
      #+:debug (assert (<= (length result) *max-constraint-stores-length*))
      (ecase type
        (:expanded
         (maxf-statistics *max-length-of-expanded-constraint-stores* (length result)))
        (:unexpanded-disjunctive
         (maxf-statistics *max-length-of-unexp-disjunctive-constraint-stores* (length result)))
        (:unexpanded-exists
         (maxf-statistics *max-length-of-unexp-exists-constraint-stores* (length result))))
      (values result new-index))))

(defun create-unexpanded-store-index (constraint-store)
  (loop with table = (racer-make-hash-table :size (loop for store in constraint-store
                                                        sum (hash-table-count (cs-table store))))
        with index = (make-unexpanded-store-index constraint-store 
						  (length constraint-store)
						  table)
        for store in constraint-store do
	(loop for ind being the hash-key of (cs-table store) do
	      (push store (gethash ind table)))
        finally
	(return index)))

(defun get-exp-index-entry (ind expanded-store expanded-store-index)
  (let ((base-table-list (exp-index-base-table expanded-store-index))
        (result-1 nil)
        (result-2 nil))
    (when (and base-table-list (gethash ind (cs-table (first base-table-list))))
      (setf result-1 base-table-list))
    (let ((table (exp-index-table expanded-store-index)))
      (when table
        (setf result-2 (gethash ind table))
        (unless (eql (length expanded-store)
                     (exp-index-cs-store-list-length expanded-store-index))
          (loop with old-list = (exp-index-cs-store-list expanded-store-index)
                for rest-list on expanded-store
                for added-store = (first rest-list)
                until (eq rest-list old-list)
                do (push added-store result-2)))))
    (if (and result-1 result-2)
      (append result-1 result-2)
      (or result-1 result-2))))

(defun create-expanded-store-index (constraint-store)
  (loop with store-list = (butlast constraint-store)
        with table = (racer-make-hash-table :size (loop for store in store-list
                                                        sum (hash-table-count (cs-table store))))
        with index = (make-expanded-store-index constraint-store 
                                                (length constraint-store)
				                (last constraint-store)
                                                table)
        for store in store-list do
	(loop for ind being the hash-key of (cs-table store) do
	      (push store (gethash ind table)))
        finally
	(return index)))

(defun update-expanded-store-index (constraint-store expanded-store-index)
  (let* ((new-expanded-store-index
          (if (exp-index-copy-p expanded-store-index)
            (copy-expanded-store-index expanded-store-index
                                       (* (hash-table-count
                                           (cs-table (first constraint-store)))
                                          5))
            expanded-store-index))
         (table (exp-index-table new-expanded-store-index)))
    (loop with old-list = (exp-index-cs-store-list new-expanded-store-index)
          for rest-list on constraint-store
          for added-store = (first rest-list)
          until (eq rest-list old-list)
          do
          (loop for ind being the hash-key of (cs-table added-store) do
	        (push added-store (gethash ind table))))
    (setf (exp-index-cs-store-list new-expanded-store-index) constraint-store)
    (setf (exp-index-cs-store-list-length new-expanded-store-index)
          (length constraint-store))
    ;(break "~S" new-expanded-store-index)
    new-expanded-store-index))

(defun compact-constraint-stores (store type)
  (let ((average
         (loop with sum = 0
               for elems on store
               for count from 0
               for elem = (first elems)
               while (rest elems)
               do (incf sum (hash-table-count (cs-table elem)))
               finally (return (/ sum count))))
        (reversed-store (reverse store)))
    (nreverse
     (cons (first reversed-store)
           (or (compact-constraint-stores-1 store average reversed-store type)
               (compact-constraint-stores-1 store
                                            (hash-table-count
                                             (cs-table (second reversed-store)))
                                            reversed-store
                                            type))))))

(defun compact-constraint-stores-1 (store average reversed-store type)
  #-:debug (declare (ignore store type))
  (loop for elems on (rest reversed-store)
        for elem = (first elems)
        unless (second elems)
        do (return)
        if (<= (hash-table-count (cs-table elem)) average) do
        (let* ((new-store (merge-constraint-stores elem (second elems)))
               (result (nconc list (list new-store) (rest (rest elems)))))
          (race-trace ("~&Compacting ~S and ~S to ~S in constraint store ~S, ~
                        new-store=~S~%"
                       (copy-constraint-store elem type)
                       (copy-constraint-store (second elems) type)
                       new-store store result))
          (return result))
        else collect elem into list
        finally (error "unexpected")))

(defun swap-to-constraint-store (list
                                     store
				     type
				     index
                                     copy-p
                                     &optional
                                     (do-not-merge-tables t))
  (add-to-constraint-store list
                           store
                           type
			   :index index
                           :extend-store-p (or (not do-not-merge-tables)
                                               *in-precompletion*
                                               *merge-constraint-store-tables-p*)
                           :copy-p copy-p))

(defun get-constraint-store-entry (ind store-table)
  (gethash ind store-table))

(defun set-constraint-store-entry (ind store store-table new-entry)
  (let ((label-cache (when (and (expanded-constraint-store-p store)
                                *inverse-roles*)
                       (cs-ind-label-cache store))))
    (when (and label-cache (gethash ind label-cache))
      ;(princ "-")
      (remhash ind label-cache)))
  (if new-entry
    (setf (gethash ind store-table) new-entry)
    (remhash ind store-table)))

(defun get-all-expanded-constraints (expanded-constraints constraint-store)
  (if (consp constraint-store)
    (append expanded-constraints
            (loop for store in constraint-store
                  nconc
                  (loop for constraints being the hash-value of (cs-table store)
                        append constraints)))
    expanded-constraints))

(defun reset-expanded-copy (state)
  (setf (state-copy-expanded-store-p state) nil))

(defun reset-exists-copy (state)
  (setf (state-copy-unexpanded-exists-constraints-store-p state) nil))

(defun reset-disjunctive-copy (state)
  (setf (state-copy-unexpanded-disjunctive-constraints-store-p state) nil))

(defun incf-removal-store-statistics (type number)
  (ecase type
    (:expanded)
    (:unexpanded-exists
     (incf-statistics *number-of-removals-from-unexp-exists-constraint-stores*)
     (incf-statistics *number-of-removed-unexp-exists-constraints* number))
    (:unexpanded-disjunctive
     (incf-statistics *number-of-removals-from-unexp-disjunctive-constraint-stores*)
     (incf-statistics *number-of-removed-unexp-disjunctive-constraints* number))))

(defun remove-individuals-from-constraint-store (ind-set
                                                        constraints
                                                        constraint-store
                                                        copy-p
                                                        state
                                                        reset-copy-function
                                                        &optional (constraint-store-index nil))
  ;;returns 2 values: new-constraints, new-store
  (let ((new-constraints 
         (loop for constraint in constraints
               unless (member (constraint-ind constraint) ind-set)
               collect constraint)))
    (if (consp constraint-store)
      (loop with changed = nil
            with sum = 0
            for store in constraint-store
	    for table = (cs-table store)
	    for new-store = 
            (loop with new-table = (unless (or copy-p (cs-copy-p store))
                                     table)
                  for ind in ind-set
                  for ind-constraints = (get-constraint-store-entry ind table)
                  do
                  (when ind-constraints
                    (unless new-table
                      (setf changed t)
                      (funcall reset-copy-function state)
                      (setf new-table (copy-hash-table table)))
                    (when-statistics
                      (incf sum (length ind-constraints)))
                    (set-constraint-store-entry ind store new-table nil))
                  finally
                  (if new-table
                    (if (eql (hash-table-count new-table) 0)
                      (return nil)
                      (return (make-constraint-store (type-of-constraint-store store)
                                                     new-table)))
                    (return store)))
	    if (or (null new-store) (eql (hash-table-count (cs-table new-store)) 0))
            do (setf changed t)
            else
	    collect new-store into new-constraint-store
            finally
            (if changed
              (progn
                (when-statistics
                  (incf-removal-store-statistics (type-of-constraint-store
                                                  (first constraint-store))
                                                 sum))
                (return (values new-constraints new-constraint-store nil)))
              (return (values new-constraints constraint-store constraint-store-index))))
      (values new-constraints constraint-store constraint-store-index))))

(defun remove-selected-constraints-from-constraint-store (predicate
                                                                   remaining-ind
                                                                   constraints
                                                                   constraint-store
                                                                   copy-p
                                                                   state
                                                                   reset-copy-function)
  ;;returns 2 values: new-constraints, new-constraint-store
  (let ((new-constraints (remove-if predicate constraints)))
    (if (consp constraint-store)
      (loop with changed = nil
            with sum = 0
            for store in constraint-store
            for table = (cs-table store)
	    for new-store = 
            (loop with new-table = (unless (or copy-p (cs-copy-p store))
                                     table)
                  for ind being the hash-key of table
                  do
                  (unless (eql ind remaining-ind)
                    (unless new-table
                      (setf changed t)
                      (funcall reset-copy-function state)
                      (setf new-table (copy-hash-table table)))
                    (when-statistics
                      (incf sum (length (get-constraint-store-entry ind new-table))))
                    (set-constraint-store-entry ind store new-table nil))
                  finally
                  (if new-table
                    (if (eql (hash-table-count new-table) 0)
                      (return nil)
                      (return (make-constraint-store (type-of-constraint-store store)
                                                     new-table)))
                    (return store)))
	    if (or (null new-store) (eql (hash-table-count (cs-table new-store)) 0))
            do (setf changed t)
            else
	    collect new-store into new-constraint-store
            finally
            (if changed
              (progn
                (when-statistics
                  (incf-removal-store-statistics (type-of-constraint-store
                                                  (first constraint-store))
                                                 sum))
                (return (values new-constraints
                                (delete nil new-constraint-store))))
              (return (values new-constraints constraint-store))))
      (values new-constraints constraint-store))))

(defun remove-constraint-from-constraint-store (removed-constraint
                                                       constraints
                                                       constraint-store
                                                       copy-p
                                                       state
                                                       reset-copy-function
                                                       &optional
                                                       (constraint-store-index nil))
  ;;returns 3 values: new-constraints, new-constraint-store, (new-constraint-store-index)
  (multiple-value-bind (new-constraints modified-p)
                       (racer-remove removed-constraint constraints)
    (if (not modified-p)
      (if (consp constraint-store)
        (let ((ind (constraint-ind removed-constraint)))
          (loop with changed = nil
                for store in constraint-store
                for table = (cs-table store)
                for new-table = (unless (or copy-p (cs-copy-p store))
                                  table)
                do
                (let ((ind-constraints (get-constraint-store-entry ind table)))
                  (multiple-value-bind
                    (new-ind-constraints modified-p)
                    (racer-remove removed-constraint ind-constraints)
                    (when modified-p
                      (unless new-table
                        (setf new-table (copy-hash-table table))
                        (funcall reset-copy-function state)
                        (setf changed t))
                      (set-constraint-store-entry ind store new-table new-ind-constraints))))
                if new-table
                if (eql (hash-table-count new-table) 0)
                do (setf changed t)
                else
                collect (make-constraint-store (type-of-constraint-store store) new-table)
                into new-constraint-store
                else
                if (eql (hash-table-count (cs-table store)) 0)
                do (setf changed t)
                else collect store into new-constraint-store
                finally
                (if changed
                  (progn
                    (when-statistics
                      (incf-removal-store-statistics (type-of-constraint-store
                                                      (first constraint-store))
                                                     1))
                    (return (values new-constraints new-constraint-store nil)))
                  (return (values new-constraints constraint-store constraint-store-index)))))
        (values constraints constraint-store constraint-store-index))
      (values new-constraints constraint-store constraint-store-index))))

(defun remove-constraints-from-constraint-store (removed-constraints
                                                        constraints
                                                        constraint-store
                                                        copy-p
                                                        state
                                                        reset-copy-function
                                                        &optional
                                                        (constraint-store-index nil))
  ;;returns 3 values: new-constraints, new-constraint-store, (new-constraint-store-index)
  ;(when (eql (length removed-constraints) 1) (break))
  (if removed-constraints
    (if (rest removed-constraints)
      (let ((new-constraints
             (constraint-set-difference constraints removed-constraints)))
        (if (consp constraint-store)
          (let ((remaining-constraints
                 (constraint-set-difference removed-constraints constraints)))
            (if remaining-constraints
              (let ((indset (racer-remove-duplicates (mapcar #'constraint-ind
                                                             remaining-constraints)))
		    (remaining-constraints-table (smart-clrhash *stable-set-difference-table*
								remaining-constraints
								'*stable-set-difference-table*)))
		(loop for constraint in remaining-constraints do
		      (setf (gethash constraint remaining-constraints-table) t))
                (loop with changed = nil
                      for store in constraint-store
                      for table = (cs-table store)
	              for new-store = 
                      (loop with new-table = (unless (or copy-p (cs-copy-p store))
                                               table)
                            for ind in indset
                            for ind-constraints = (get-constraint-store-entry ind table)
			  when ind-constraints
			  do
                            (multiple-value-bind
				(new-ind-constraints modified-p)
				(stable-set-table-difference ind-constraints remaining-constraints-table)
                              (when modified-p
                                (unless new-table
                                  (setf changed t)
                                  (funcall reset-copy-function state)
                                  (setf new-table (copy-hash-table table)))
                                (set-constraint-store-entry ind store new-table new-ind-constraints)))
                            finally
                            (if new-table
                              (if (eql (hash-table-count new-table) 0)
                                (return nil)
                                (return (make-constraint-store (type-of-constraint-store store)
                                                               new-table)))
                              (return store)))
	              if (or (null new-store) (eql (hash-table-count (cs-table new-store)) 0))
                      do (setf changed t)
                      else
	              collect new-store into new-constraint-store
                      finally
                      (if changed
                        (progn
                          (when-statistics
                            (incf-removal-store-statistics (type-of-constraint-store
                                                            (first constraint-store))
                                                           (length remaining-constraints)))
                          (return (values new-constraints new-constraint-store nil)))
                        (return (values new-constraints constraint-store constraint-store-index)))))
              (values new-constraints constraint-store constraint-store-index)))
          (values new-constraints constraint-store constraint-store-index)))
      (if (or constraints (consp constraint-store))
        (remove-constraint-from-constraint-store (first removed-constraints)
                                                 constraints
                                                 constraint-store
                                                 copy-p
                                                 state
                                                 reset-copy-function)
        (values constraints constraint-store constraint-store-index)))
    (values constraints constraint-store constraint-store-index)))

(defun remove-equal-constraints-from-constraint-store (removed-constraints
                                                               constraints
                                                               constraint-store
                                                               constraint-store-index
                                                               copy-p
                                                               state
                                                               reset-copy-function)
  ;;returns 2 values: new-constraints, new-constraint-store
  (if removed-constraints
    (let ((new-constraints
           (stable-constraint-set-difference constraints removed-constraints)))
      (if (consp constraint-store)
        (let ((remaining-constraints
               (stable-constraint-set-difference removed-constraints constraints)))
          (if remaining-constraints
            (let ((indset (racer-remove-duplicates
                           (mapcar #'constraint-ind remaining-constraints))))
              (loop with changed = nil
                    for store in constraint-store
                    for table = (cs-table store)
                    for new-store =
                    (loop with new-table = (unless (or copy-p (cs-copy-p store))
                                             table)
                          for ind in indset
                          for ind-constraints = (get-constraint-store-entry ind table)
                          do
                          (multiple-value-bind
                            (new-ind-constraints modified-p)
                            (stable-constraint-set-difference ind-constraints remaining-constraints)
                            (when modified-p
                              (unless new-table
                                (setf changed t)
                                (funcall reset-copy-function state)
                                (setf new-table (copy-hash-table table)))
                              (set-constraint-store-entry ind store new-table new-ind-constraints)))
                          finally
                          (if new-table
                            (if (eql (hash-table-count new-table) 0)
                              (return nil)
                              (return (make-constraint-store (type-of-constraint-store store)
                                                             new-table)))
                            (return store)))
	            if (or (null new-store) (eql (hash-table-count (cs-table new-store)) 0))
                    do (setf changed t)
                    else
	            collect new-store into new-constraint-store
                    finally
                    (if changed
                      (progn
                        (when-statistics
                          (incf-removal-store-statistics (type-of-constraint-store
                                                          (first constraint-store))
                                                         (length remaining-constraints)))
                        (return (values new-constraints new-constraint-store nil)))
                      (return (values new-constraints constraint-store constraint-store-index)))))
            (values new-constraints constraint-store constraint-store-index)))
        (values new-constraints constraint-store constraint-store-index)))
    (values constraints constraint-store constraint-store-index)))

(defun collect-selected-constraints (ind
				          select-fcn
				          constraints
				          constraint-store
				          &optional (constraint-store-index nil))
  #+:debug (assert ind)
  (nconc (loop for constraint in constraints
               when (and (eql ind (constraint-ind constraint))
                         (funcall select-fcn constraint))
               collect constraint)
         (when (consp constraint-store)
           (loop for store in (if constraint-store-index
                                (get-exp-index-entry ind 
                                                     constraint-store 
                                                     constraint-store-index)
				constraint-store)
                 for table = (cs-table store)
                 nconc
                 (loop for constraint in (get-constraint-store-entry ind table)
                       when (funcall select-fcn constraint)
                       collect constraint)))))

(defun collect-ind-label (ind constraints state)
  (let ((constraint-store (state-expanded-store state))
        (unexpanded-exists-store (state-unexpanded-exists-constraints-store state)))
    (concept-set-union
     (loop for constraint in constraints
           when (eql ind (constraint-ind constraint))
           collect (if (constraint-negated-p constraint)
                     (concept-negated-concept
                      (constraint-term constraint))
                     (constraint-term constraint)))
     (nconc (loop for constraint in (state-expanded-constraints state)
                  when (eql ind (constraint-ind constraint))
                  collect (if (constraint-negated-p constraint)
                            (concept-negated-concept
                             (constraint-term constraint))
                            (constraint-term constraint)))
            (loop for constraint in (state-unexpanded-exists-constraints state)
                  when (eql ind (constraint-ind constraint))
                  collect (constraint-term constraint))
            (when (consp unexpanded-exists-store)
              (loop for store in unexpanded-exists-store
                    for table = (cs-table store)
                    nconc
                    (loop for constraint in (get-constraint-store-entry ind table)
                          collect (constraint-term constraint))))
            (when (consp constraint-store)
              (let* ((newest-store (first constraint-store))
                     (cache (when (and newest-store
                                       (expanded-constraint-store-p newest-store))
                              (cs-ind-label-cache newest-store))))
                (or (when cache
                      (let ((result (gethash ind cache)))
                        (when-statistics
                          (if result
                            (incf-statistics *label-cache-hits*)
                            (incf-statistics *label-cache-misses*)))
                        ;(princ "@")
                        ;(break)
                        result))
                    (let ((label
                           (loop with constraint-store-index = (state-expanded-store-index state)
                                 for store in (if constraint-store-index
                                                (get-exp-index-entry ind 
                                                                     constraint-store 
                                                                     constraint-store-index)
						constraint-store)
                                 for table = (cs-table store)
                                 nconc
                                 (loop for constraint in (get-constraint-store-entry ind
                                                                                     table)
                                       collect (if (constraint-negated-p constraint)
                                                 (concept-negated-concept
                                                  (constraint-term constraint))
                                                 (constraint-term constraint))))))
                      (when (and newest-store
                                 (expanded-constraint-store-p newest-store))
                        (unless cache
                          (incf-statistics *number-of-created-label-caches*)
                          (setf cache (racer-make-hash-table))
                          (setf (cs-ind-label-cache newest-store) cache))
                        ;(princ "$")
                        (incf-statistics *number-of-added-label-cache-entries*)
                        (setf (gethash ind cache) label))
                      label))))))))

(defun append-selected-constraints (ind
                                         select-fcn
                                         constraints
                                         constraint-store
                                         &key (collect-fcn #'identity))
  #+:debug (assert ind)
  (nconc (loop for constraint in constraints
               when (and (eql ind (constraint-ind constraint))
                         (or (null select-fcn)
                             (funcall select-fcn constraint)))
               append (let ((result (funcall collect-fcn constraint)))
                        (if (consp result)
                          result
                          (list result))))
         (when (consp constraint-store)
           (loop for store in constraint-store
                 for table = (cs-table store)
                 nconc
                 (loop for constraint in (get-constraint-store-entry ind table)
                       when (or (null select-fcn)
                                (funcall select-fcn constraint))
                       append (let ((result (funcall collect-fcn constraint)))
                                (if (consp result)
                                  result
                                  (list result))))))))

(defun collect-indset-selected-constraints (indset
                                                  constraints
					          constraint-store
					          &optional (constraint-store-index nil))
  #+:debug (assert indset)
  (nconc (loop for constraint in constraints
               when (member (constraint-ind constraint) indset)
               collect constraint)
         (when (consp constraint-store)
           (loop for ind in indset
	         for store-list = (if constraint-store-index
                                    (get-exp-index-entry ind 
                                                         constraint-store 
                                                         constraint-store-index)
				    constraint-store)
	       for entries = (loop for store in store-list
				 for entries = (get-constraint-store-entry ind (cs-table store))
				 when entries
				 append entries)
	       when entries
	       nconc entries))))

(defun collect-ind-selected-constraints (ind
                                               constraints
                                               constraint-store
                                               &optional (constraint-store-index nil))
  #+:debug (assert ind)
  (nconc (loop for constraint in constraints
               when (eql ind (constraint-ind constraint))
               collect constraint)
         (when (consp constraint-store)
           (loop for store in (if constraint-store-index
                                (get-exp-index-entry ind 
                                                     constraint-store 
                                                     constraint-store-index)
                                constraint-store)
	       for entries = (get-constraint-store-entry ind (cs-table store))
	       when entries
	       append entries))))

(defun find-indset-selected-constraint (indset
                                              constraints
                                              constraint-store)
  #+:debug (assert indset)
  (or (loop for constraint in constraints
            when (member (constraint-ind constraint) indset)
            do (return constraint))
      (when (consp constraint-store)
        (loop for store in constraint-store
              for table = (cs-table store)
              thereis
              (loop for ind in indset
                    thereis (first (get-constraint-store-entry ind table)))))))

(defun indset-selected-constraints-p (indset
                                           fcn
                                           constraints
                                           constraint-store
                                           &optional (constraint-store-index nil))
  #+:debug (assert indset)
  (or (loop for constraint in constraints
            thereis (and (member (constraint-ind constraint) indset)
                         (funcall fcn constraint)))
      (when (consp constraint-store)
        (loop for ind in indset
              thereis
              (loop for store in (if constraint-store-index
                                   (get-exp-index-entry ind 
                                                        constraint-store 
                                                        constraint-store-index)
                                   constraint-store)
                    for table = (cs-table store)
                    thereis
                    (loop for constraint in (get-constraint-store-entry ind table)
                          thereis (funcall fcn constraint)))))))

(defun ind-selected-constraints-p (ind
                                        fcn
                                        constraints
                                        constraint-store
                                        &optional (constraint-store-index nil))
  #+:debug (assert ind)
  (or (loop for constraint in constraints
            thereis (and (eql (constraint-ind constraint) ind)
                         (funcall fcn constraint)))
      (when (consp constraint-store)
        (loop for store in (if constraint-store-index
                             (get-exp-index-entry ind 
                                                  constraint-store 
                                                  constraint-store-index)
			     constraint-store)
              for table = (cs-table store)
              thereis
              (loop for constraint in (get-constraint-store-entry ind table)
                    thereis (funcall fcn constraint))))))

(defun find-if-selected-constraints (ind
                                          fcn
                                          constraints
                                          constraint-store
				          &optional (constraint-store-index nil))
  #+:debug (assert ind)
  (or (loop for constraint in constraints
            when (and (eql ind (constraint-ind constraint))
                      (funcall fcn constraint))
            do (return constraint))
      (when (consp constraint-store)
        (loop for store in (if constraint-store-index
                             (get-exp-index-entry ind 
                                                  constraint-store 
                                                  constraint-store-index)
			     constraint-store)
              for table = (cs-table store)
              for found = (loop for constraint in (get-constraint-store-entry ind table)
                                when (funcall fcn constraint)
                                do (return constraint))
              when found
              do (return found)))))

(defun find-if-indset-selected-constraint (indset
                                           fcn
                                           constraints
                                           constraint-store
                                           &optional (constraint-store-index nil))
  #+:debug (assert indset)
  (or (loop for constraint in constraints
            when (and (member (constraint-ind constraint) indset)
                      (funcall fcn constraint))
            do (return constraint))
      (when (consp constraint-store)
        (loop for ind in indset
              thereis
              (loop for store in (if constraint-store-index
                                     (get-exp-index-entry ind 
                                                          constraint-store 
                                                          constraint-store-index)
                                   constraint-store)
                    for table = (cs-table store)
                    for found = (loop for constraint in (get-constraint-store-entry ind table)
                                      when (funcall fcn constraint)
                                      do (return constraint))
                    when found
                    do (return found))))))

(defun compare-individuals (ind-1 ind-2)
  (if (numberp ind-1)
    (and (numberp ind-2) (< ind-1 ind-2))
    (or (numberp ind-2)
        (let ((l-1 (individual-has-fillers ind-1))
              (l-2 (individual-has-fillers ind-2)))
          (or (< l-1 l-2)
              (and (eql l-1 l-2)
                   (> (individual-used-as-filler ind-1)
                      (individual-used-as-filler ind-2))))))))

#| ;;; inefficient for a large number of individuals
(defun prioritize-individuals (table abox)
  #+:debug (assert abox)
  (let ((sorted-inds
         (sort (loop for ind being the hash-key of table
                     collect (if (true-old-individual-p ind)
                               (find-individual abox ind)
                               ind))
               #'compare-individuals))
        (cache-size-from-constraint-store *cache-size-from-constraint-store*))
    (loop for ind in sorted-inds
          for ind-name = (if (numberp ind)
                           ind
                           (first (individual-name-set ind)))
          sum (length (gethash ind-name table)) into count
          collect ind-name
          until (>= count cache-size-from-constraint-store))))
|#

(defun prioritize-individuals (table abox)
  #+:debug (assert abox)
  (mapcar (lambda (ind)
            (if (numberp ind)
              ind
              (first (individual-name-set ind))))
          (sort (loop with cache-size-from-constraint-store = *cache-size-from-constraint-store*
                      for ind being the hash-key of table using (hash-value constraints)
                      sum (length constraints) into count
                      collect (if (true-old-individual-p ind)
                                (find-individual abox ind)
                                ind)
                      until (>= count cache-size-from-constraint-store))
                #'compare-individuals)))

(defmacro loop-over-all-constraints ((constraint
                                     collected-constraints
                                     constraint-store
                                     abox)
                                    exit-condition
                                    finish-form
                                    &body body)
  (let ((store-sym (gensym))
        (table-sym (gensym))
        (ind-sym (gensym))
        (ind-list-sym (gensym))
        (constraints-sym (gensym)))
    `(loop for ,store-sym in ,constraint-store
           for ,table-sym = (cs-table ,store-sym)
           for ,ind-list-sym = (prioritize-individuals ,table-sym ,abox)
           nconc
           (loop for ,ind-sym in ,ind-list-sym
                 for ,constraints-sym = (gethash ,ind-sym ,table-sym)
                 nconc
                 (loop for ,constraint in (reverse ,constraints-sym)
                       ,@body
                       until ,exit-condition)
                 until ,exit-condition)
           into ,collected-constraints
           until ,exit-condition
           . ,finish-form)))

(defmacro iterate-over-all-constraints ((constraint constraint-store) &body body)
  (let ((store-sym (gensym))
        (table-sym (gensym))
        (constraints-sym (gensym)))
    `(loop for ,store-sym in ,constraint-store
           for ,table-sym = (cs-table ,store-sym)
           do
           (loop for ,constraints-sym being the hash-value of ,table-sym
                 do
                 (loop for ,constraint in ,constraints-sym
                       ,@body)))))

(defmacro loop-over-all-individuals-of-constraint-store ((individual constraint-store) &body body)
  (let ((store-sym (gensym))
        (table-sym (gensym)))
    `(loop for ,store-sym in ,constraint-store
           for ,table-sym = (cs-table ,store-sym)
           do
           (loop for ,individual being the hash-key of ,table-sym
                 . ,body))))

(defmacro loop-over-ind-expanded-constraints ((constraint ind constraint-store) &body body)
  (let ((store-sym (gensym))
        (table-sym (gensym)))
    `(loop for ,store-sym in ,constraint-store
           for ,table-sym = (cs-table ,store-sym)
           do
           (loop for ,constraint in (get-constraint-store-entry ,ind ,table-sym)
                 . ,body))))

(defun get-individuals-of-constraint-store (constraints constraint-store)
  (racer-remove-duplicates
   (nconc (loop for constraint in constraints
                collect (constraint-ind constraint))
          (loop for store in constraint-store
                for table = (cs-table store)
                nconc
                (loop for ind being the hash-key of table
                      collect ind)))))

(defmacro ind-maximize-constraints ((constraint)
                                    ind
                                    value-form
                                    constraints
                                    constraint-store
                                    max-count)
  (let ((store-sym (gensym))
        (table-sym (gensym))
        (count-sym (gensym)))
    `(let ((,count-sym 0))
       (max (loop for ,constraint in ,constraints
                  when (eql (constraint-ind ,constraint) ,ind)
                  maximize ,value-form
                  and do (incf ,count-sym)
                  until (> ,count-sym ,max-count))
            (if (and (<= ,count-sym ,max-count) (consp ,constraint-store))
              (loop for ,store-sym in ,constraint-store
                    for ,table-sym = (cs-table ,store-sym)
                    maximize
                    (loop for ,constraint in (get-constraint-store-entry ,ind ,table-sym)
                          maximize ,value-form
                          do (incf ,count-sym)
                          until (> ,count-sym ,max-count))
                    until (> ,count-sym ,max-count))
              0)))))
