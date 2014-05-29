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

(defstruct (basic-cache (:conc-name cache-))
  (null-marker nil)
  (successors (make-successors nil)))

(defstruct (cache (:constructor make-cache (name))
                   (:include basic-cache)
                   (:conc-name cache-))
  (name nil))

(defstruct (cache-node (:constructor make-cache-node (entry entry-key successors))
                         (:include basic-cache)
                         (:conc-name cache-))
  (entry nil)
  (entry-key nil))

;;; ======================================================================

(defvar *tableaux-cache-sat*)
(defvar *tableaux-cache-unsat*)

;;; In the current implementation the successors of a cache node
;;; are collected into a list or a hash table. 

(defun make-entry (list compare-fn)
  (sort list compare-fn))


(defun insert-node (node successors)
  (if successors
    (let ((node-key (cache-entry-key node)))
      (if (> node-key (cache-entry-key (first successors)))
        (progn
          (insert-node-1 node node-key (rest successors) successors)
          successors)
        (cons node successors)))
    (list node)))

(defun insert-node-1 (node node-key successors prev-successors)
  (if successors
    (if (> node-key (cache-entry-key (first successors)))
      (insert-node-1 node node-key (rest successors) successors)
      (setf (cdr prev-successors) (cons node successors)))
    (setf (cdr prev-successors) (list node))))

(defun make-successors (node)
  (if (null node)
    nil
    (list node)))

(defmacro loop-over-successors (var container &body body)
  `(loop for ,var in ,container .,body))

;;; See also the function 'find-key' below.

;;; ======================================================================

(defmethod print-object ((object cache-node) stream)
  (multiple-value-bind (n-entries average-n-successors max-n-successors depth)
                       (analyze-cache object)
    (print-unreadable-object (object stream :type nil :identity nil)
      (format stream "~S (size ~D) (succ ~D) (ave ~,1F) (max ~D) (depth ~D) ~S"
              (when (cache-node-p object)
                (cache-entry-key object))
              n-entries
              (length (cache-successors object))
              average-n-successors
              max-n-successors
              depth
              (cache-successors object)))))

;;; ----------------------------------------------------------------------


(defparameter *print-cache-verbose* nil)

(defmethod print-object ((object cache) stream)
  (if *print-cache-verbose*
    (print-cache object stream)
    (multiple-value-bind (n-entries average-n-successors max-n-successors depth)
                         (analyze-cache object)
      (print-unreadable-object (object stream :type t :identity t)
        (format stream "~A (size ~D) (succ ~S) (ave ~,1F) (max ~D) (depth ~D)"
                (cache-name object)
                n-entries
                (if (listp (cache-successors object))
                  (length (cache-successors object))
                  (hash-table-count (cache-successors object)))
                average-n-successors
                max-n-successors
                depth)))))

(defun print-entry (entry stream level)
  (terpri stream)
  (loop repeat level do (princ " " stream))
  (format stream "~S" entry))

(defun print-cache (cache stream)
  (when (cache-name cache)
    (format stream "~&~a" (cache-name cache)))
  (let ((successors (when (cache-successors cache)
                      (loop for value being the hash-value of (cache-successors cache)
                            collect value))))
    (loop-over-successors node successors
      do (print-successor node stream 0))))

(defun print-successor (node stream level)
  (when (cache-null-marker node)
    (print-entry (cache-entry node) stream level))
  (when (cache-successors node)
    (print-entry (cache-entry node) stream level))
  (loop-over-successors node1 (cache-successors node)
                        do 
                        (print-successor node1 stream (+ level 1))))

(defun find-key (key successors)
  (loop-over-successors node successors 
    as entry-key = (cache-entry-key node)
    do (if (eql key entry-key)
         (return-from find-key node))
    until (> entry-key key))
  nil)

(defun compute-cache-entries (cache)
  (if (null cache)
    nil
    (let* ((successors (if (listp (cache-successors cache))
                         (cache-successors cache)
                         (loop for value being the hash-value of (cache-successors cache)
                               collect value)))
           (result (loop-over-successors 
                     node successors
                     append (let ((result (compute-cache-entries node)))
                              (if (null result)
                                (list (list node))
                                (loop for elem in result
                                      collect (cons node elem)))))))
      (if (cache-null-marker cache)
        (cons nil result)
        result))))

(defun analyze-cache (cache)
  ;;; returns total no of entries, average length of successors for all entries, 
  ;;; max length of successors for all entries, max depth
  (if (null cache)
    (values 0 0 0 0)
    (let ((n-entries 0)
          (n-successors 0)
          (max-successors 0)
          (max-depth 0)
          (successors (if (listp (cache-successors cache))
                        (cache-successors cache)
                        (loop for value being the hash-value of (cache-successors cache)
                              collect value))))
      (loop-over-successors node successors
        do
        (multiple-value-bind (n-entries-1 average-n-successors max-n-successors current-depth)
                             (analyze-cache-1 node 0)
          (incf n-entries n-entries-1)
          (incf n-successors average-n-successors)
          (setf max-successors (max max-successors max-n-successors))
          (setf max-depth (max max-depth current-depth))))
      (let ((no-of-entries (length successors)))
        (values n-entries
                (if (zerop no-of-entries)
                  0
                  (/ n-successors no-of-entries))
                max-successors
                max-depth)))))

(defun analyze-cache-1 (cache depth)
  (let ((n-entries 0)
        (n-successors 0)
        (max-successors 0)
        (max-depth depth))
    (when (cache-null-marker cache)
      (incf n-entries))
    (loop-over-successors node (cache-successors cache)
      do
      (multiple-value-bind (n-entries-1 average-n-successors max-n-successors current-depth)
                           (analyze-cache-1 node (1+ depth))
        (incf n-entries n-entries-1)
        (incf n-successors average-n-successors)
        (setf max-successors (max max-successors max-n-successors))
        (setf max-depth (max max-depth current-depth))))
    (let ((no-of-entries (length (cache-successors cache))))
      (values n-entries
              (if (zerop n-successors)
                no-of-entries 
                (/ (+ no-of-entries (/ n-successors no-of-entries))
                   2))
              (max max-successors no-of-entries)
              max-depth))))



;;; ======================================================================

(race-inline (add-to-null-marker))

(defun add-to-null-marker (cache value supplied-p)
  (if supplied-p
    (if (cache-null-marker cache)
      (pushnew value (cdr (cache-null-marker cache)) :test #'equal)
      (setf (cache-null-marker cache) (cons t (list value))))
    (setf (cache-null-marker cache) (cons t value))))

(defun append-as-nodes (ordered-set value supplied-p)
  (if (null ordered-set)
    nil
    (let ((cache-node (make-cache-node (first ordered-set)
                                       (concept-hash-id (first ordered-set))
                                       (make-successors 
                                        (append-as-nodes (rest ordered-set) value supplied-p)))))
      (when (null (rest ordered-set))
        (add-to-null-marker cache-node value supplied-p))
      cache-node)))

(defun insert-into-cache (ordered-set cache &optional (value t supplied-p))
  (insert-into-cache-1 ordered-set cache value supplied-p t))

(defun insert-into-cache-1 (ordered-set cache value supplied-p top-level-p)
  #+:allegro (declare (:explain :tailmerging))
  (if (null ordered-set)
    (when cache
      (add-to-null-marker cache value supplied-p))
    (let* ((entry-key (concept-hash-id (first ordered-set)))
           (successors (cache-successors cache))
           (node (when successors
                   (if top-level-p
                     (gethash entry-key successors)
                     (find-key entry-key successors)))))
      (if node
        (if (and (null successors) (cache-null-marker node))
          (setf (cache-successors cache) 
                (list (append-as-nodes ordered-set value supplied-p)))
          (insert-into-cache-1 (rest ordered-set) node value supplied-p nil))
        (if (null ordered-set)
          (when cache
            (add-to-null-marker cache value supplied-p))
          (progn
            (if top-level-p
              (progn
                (unless successors
                  (setf successors (racer-make-hash-table))
                  (setf (cache-successors cache) successors))
                (setf (gethash entry-key successors)
                      (append-as-nodes ordered-set value supplied-p)))
              (setf (cache-successors cache) 
                    (insert-node (append-as-nodes ordered-set value supplied-p)
                                 successors)))))))))

(defvar *removed-p*)

(defun remove-from-cache (ordered-set cache)
  (let ((*removed-p* nil))
    (remove-from-cache-1 ordered-set cache t)
    *removed-p*))

(defun remove-from-cache-1 (ordered-set cache top-level-p)
  (if (null ordered-set)
    (when cache
      (if (null (cache-successors cache))
        t
        (setf (cache-null-marker cache) nil)))
    (when (cache-successors cache)
      (let ((entry-key (concept-hash-id (first ordered-set))))
        (if top-level-p
          (let ((top-entry (gethash entry-key (cache-successors cache))))
            (when top-entry
              (let ((remove-p (remove-from-cache-1 (rest ordered-set) top-entry nil)))
                (when remove-p
                  (remhash entry-key (cache-successors cache))
                  (setf *removed-p* t)
                  (and (eql (hash-table-count (cache-successors cache)) 0)
                       (not (cache-null-marker cache)))))))
          (loop-over-successors node (cache-successors cache)
            for node-key = (cache-entry-key node)
            do
            (cond ((eql entry-key node-key)
                   (let ((remove-p (remove-from-cache-1 (rest ordered-set) node nil)))
                     (if remove-p
                       (progn
                         (setf (cache-successors cache) 
                               (delete entry-key (cache-successors cache) :key #'cache-entry-key))
                         (setf *removed-p* t)
                         (return-from remove-from-cache-1
                           (and (null (cache-successors cache))
                                (not (cache-null-marker cache)))))
                       (return-from remove-from-cache-1 nil))))
                  ((> node-key entry-key)
                   (return-from remove-from-cache-1 nil)))))))))

;;; ======================================================================

(defun superset-exists-p (ordered-set cache)
  (if (null ordered-set)
    t
    (superset-exists-p-1 ordered-set cache t)))

(defun superset-exists-p-1 (ordered-set cache top-level-p)
  (if ordered-set
    (let ((entry-key (concept-hash-id (first ordered-set))))
      #+:debug (assert (not (null cache)))
      (if top-level-p
        (let ((top-entry (when (cache-successors cache)
                           (gethash entry-key (cache-successors cache)))))
          (when top-entry
            (superset-exists-p-1 (rest ordered-set) top-entry nil)))
        (loop-over-successors node (cache-successors cache)
          for node-key = (cache-entry-key node)
          thereis (cond ((< node-key entry-key)
                         (superset-exists-p-1 ordered-set node nil))
                        ((eql entry-key node-key)
                         (superset-exists-p-1 (rest ordered-set) node nil))))))
    (if cache
      (progn
        (incf-statistics *sat-cache-hits*)
        (or (cache-null-marker cache)
            (progn
              (incf-statistics *sat-cache-real-superset-hits*)
              (let ((marker (find-superset-null-marker (cache-successors cache))))
                #+:debug (assert marker)
                marker))))
      t)))

(defun find-superset-null-marker (nodes)
  #+:debug (assert (listp nodes))
  (loop-over-successors node nodes
                        thereis (or (cache-null-marker node)
                                    (find-superset-null-marker (cache-successors node)))))

(defun find-subset (ordered-set cache)
  (cdr (find-subset-2 ordered-set cache t)))

(defun find-subset-2 (ordered-set cache top-level-p)
  (if (null (cache-successors cache))
    nil
    (find-subset-1 ordered-set (cache-successors cache) top-level-p)))

(defun find-subset-1 (ordered-set successors top-level-p)
  (if (null ordered-set)
    nil
    (let ((entry-key (concept-hash-id (first ordered-set))))
      (if top-level-p
        (let ((top-entry (gethash entry-key successors)))
          (when top-entry
            (find-subset-3 ordered-set top-entry)))
        (loop-over-successors node successors
          for node-key = (cache-entry-key node)
          do
          (cond ((eql entry-key node-key)
                 (return-from find-subset-1
                   (find-subset-3 ordered-set node)))
                ((< entry-key node-key)
                 (return-from find-subset-1 
                   (find-subset-1 (rest ordered-set) successors nil)))))))))

(defun find-subset-3 (ordered-set node)
  (let ((marker (cache-null-marker node)))
    (if marker
      (progn
        (incf-statistics *unsat-cache-hits*)
        (unless (null (rest ordered-set))
          (incf-statistics *unsat-cache-real-subset-hits*))
        marker)
      (find-subset-2 (rest ordered-set) node nil))))

(defun print-cache-statistics (&key (stream t)
                                        (sat-cache *tableaux-cache-sat*)
                                        (unsat-cache *tableaux-cache-unsat*))
  (let ((total-unsat-cache-hits
         (get-total-statistics-integer-value *unsat-cache-hits*))
        (total-unsat-cache-real-subset-hits
         (get-total-statistics-integer-value *unsat-cache-real-subset-hits*))
        (total-sat-cache-hits
         (get-total-statistics-integer-value *sat-cache-hits*))
        (total-sat-cache-real-superset-hits
         (get-total-statistics-integer-value *sat-cache-real-superset-hits*))
        (added-sat-cache-entries (get-total-statistics-integer-value *added-sat-cache-entries*))
        (retracted-sat-cache-entries
         (get-total-statistics-integer-value *retracted-sat-cache-entries*)))
  (multiple-value-bind (sat-n-entries sat-average-n-successors)
                       (analyze-cache sat-cache)
    (multiple-value-bind (unsat-n-entries unsat-average-n-successors)
                         (analyze-cache unsat-cache)
      (when sat-cache
        (format stream 
                "~&;~A cache: entries=~:D, added ~D, retracted ~D (~,2F%), ~
                 hits=~:D (true superset=~:D), average successors=~,2F~%"
                (cache-name sat-cache)
                sat-n-entries
                added-sat-cache-entries
                retracted-sat-cache-entries
                (safe-percentage retracted-sat-cache-entries added-sat-cache-entries)
                total-sat-cache-hits
                total-sat-cache-real-superset-hits
                sat-average-n-successors))
      (when unsat-cache
        (format stream 
                "~&;~A cache: entries=~:D, hits=~:D (true subset=~:D), ~
                 average successors=~,2F~%"
                (cache-name unsat-cache)
                unsat-n-entries
                total-unsat-cache-hits
                total-unsat-cache-real-subset-hits
                unsat-average-n-successors))
      (format stream
              "~&;Total number of entries in subset/superset caches: ~D~%" 
              (+ sat-n-entries unsat-n-entries))
      (+ sat-n-entries unsat-n-entries)))))

