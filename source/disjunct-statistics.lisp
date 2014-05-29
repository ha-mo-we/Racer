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

(defstruct select-disjunct-record
  (no-of-pos-entries 0)
  (sum-of-pos-entries 0)
  (no-of-neg-entries 0)
  (sum-of-neg-entries 0)
  )

#+:debug
(defun copy-select-disjunct-record-slots (to from)
  (setf (select-disjunct-record-no-of-pos-entries to) (select-disjunct-record-no-of-pos-entries from))
  (setf (select-disjunct-record-sum-of-pos-entries to) (select-disjunct-record-sum-of-pos-entries from))
  (setf (select-disjunct-record-no-of-neg-entries to) (select-disjunct-record-no-of-neg-entries from))
  (setf (select-disjunct-record-sum-of-neg-entries to) (select-disjunct-record-sum-of-neg-entries from)))

#-:debug
(defmacro copy-select-disjunct-record-slots (to from)
  (let ((from-sym (gensym)))
    `(let ((,from-sym ,from))
       (setf (select-disjunct-record-no-of-pos-entries ,to)
             (select-disjunct-record-no-of-pos-entries ,from-sym))
       (setf (select-disjunct-record-sum-of-pos-entries ,to)
             (select-disjunct-record-sum-of-pos-entries ,from-sym))
       (setf (select-disjunct-record-no-of-neg-entries ,to)
             (select-disjunct-record-no-of-neg-entries ,from-sym))
       (setf (select-disjunct-record-sum-of-neg-entries ,to)
             (select-disjunct-record-sum-of-neg-entries ,from-sym)))))

(defstruct select-disjunct
  (table (make-hash-table))
  (current-history-entry nil)
  (no-of-table-changes 0)
  (no-of-changed-history-entries 0)
  (last-sum-rate 1)
  (history (make-array *disjunct-record-history-size* :initial-element nil)))

(defun clear-select-disjunct-table (table)
  (setf (select-disjunct-table table) (smart-clrhash (select-disjunct-table table) nil nil))
  (setf (select-disjunct-current-history-entry table) nil)
  (setf (select-disjunct-no-of-table-changes table) 0)
  (setf (select-disjunct-no-of-changed-history-entries table) 0)
  (setf (select-disjunct-last-sum-rate table) 1)
  table)

(defun push-select-disjunct-history (table)
  (let ((vector (select-disjunct-history table)))
    (when (eql (select-disjunct-no-of-changed-history-entries table) (array-total-size vector))
      (setf (select-disjunct-no-of-changed-history-entries table) 0)
      (multiple-value-bind (old cur)
          (get-start-end-history-records table vector)
        (setf (select-disjunct-last-sum-rate table)
              (/ (- (- (select-disjunct-record-sum-of-pos-entries cur)
                       (select-disjunct-record-sum-of-neg-entries cur))
                    (- (select-disjunct-record-sum-of-pos-entries old)
                       (select-disjunct-record-sum-of-neg-entries old)))
                 (* (array-total-size vector) *disjunct-record-sampling-rate*)))))
    (incf (select-disjunct-no-of-changed-history-entries table))
    (let ((prev-index (select-disjunct-current-history-entry table)))
      (if prev-index
          (let* ((index (mod (1+ prev-index) (array-total-size vector)))
                 (next-record (svref vector index)))
            (if next-record
                (copy-select-disjunct-record-slots next-record (svref vector prev-index))
              (setf (svref vector index) (copy-select-disjunct-record (svref vector prev-index))))
            (setf (select-disjunct-current-history-entry table) index))
        (let ((current-record (svref vector 0)))
          (if current-record
              (progn
                (setf (select-disjunct-record-no-of-pos-entries current-record) 0)
                (setf (select-disjunct-record-sum-of-pos-entries current-record) 0)
                (setf (select-disjunct-record-no-of-neg-entries current-record) 0)
                (setf (select-disjunct-record-sum-of-neg-entries current-record) 0))
            (setf (svref vector 0) (make-select-disjunct-record)))
          (setf (select-disjunct-current-history-entry table) 0))))))

#+:debug
(defun select-disjunct-total-sum (table)
  (let ((current (select-disjunct-current-history-entry table)))
    (if current
        (let ((entry (svref (select-disjunct-history table) current)))
          (- (select-disjunct-record-sum-of-pos-entries entry)
             (select-disjunct-record-sum-of-neg-entries entry)))
      0)))

#-:debug
(defmacro select-disjunct-total-sum (table)
  (let ((table-sym (gensym))
        (current-sym (gensym))
        (entry-sym (gensym)))
    `(let* ((,table-sym ,table)
            (,current-sym (select-disjunct-current-history-entry ,table-sym)))
       (if ,current-sym
           (let ((,entry-sym (svref (select-disjunct-history ,table-sym) ,current-sym)))
             (- (select-disjunct-record-sum-of-pos-entries ,entry-sym)
                (select-disjunct-record-sum-of-neg-entries ,entry-sym)))
         0))))

(defun select-disjunct-no-of-pos-entries (table)
  (let ((current (select-disjunct-current-history-entry table)))
    (if current
        (select-disjunct-record-no-of-pos-entries (svref (select-disjunct-history table) current))
      0)))

(defun (setf select-disjunct-no-of-pos-entries) (new-value table)
  (setf (select-disjunct-record-no-of-pos-entries
         (svref (select-disjunct-history table) (select-disjunct-current-history-entry table)))
        new-value))

(defun select-disjunct-sum-of-pos-entries (table)
  (let ((current (select-disjunct-current-history-entry table)))
    (if current
        (select-disjunct-record-sum-of-pos-entries (svref (select-disjunct-history table) current))
      0)))

(defun (setf select-disjunct-sum-of-pos-entries) (new-value table)
  (setf (select-disjunct-record-sum-of-pos-entries
         (svref (select-disjunct-history table) (select-disjunct-current-history-entry table)))
        new-value))

(defun select-disjunct-no-of-neg-entries (table)
  (let ((current (select-disjunct-current-history-entry table)))
    (if current
        (select-disjunct-record-no-of-neg-entries (svref (select-disjunct-history table) current))
      0)))

(defun (setf select-disjunct-no-of-neg-entries) (new-value table)
  (setf (select-disjunct-record-no-of-neg-entries
         (svref (select-disjunct-history table) (select-disjunct-current-history-entry table)))
        new-value))

(defun select-disjunct-sum-of-neg-entries (table)
  (let ((current (select-disjunct-current-history-entry table)))
    (if current
        (select-disjunct-record-sum-of-neg-entries (svref (select-disjunct-history table) current))
      0)))

(defun (setf select-disjunct-sum-of-neg-entries) (new-value table)
  (setf (select-disjunct-record-sum-of-neg-entries
         (svref (select-disjunct-history table) (select-disjunct-current-history-entry table)))
        new-value))

#+:debug
(defun select-disjunct-more-pos-entries-p (table &optional (threshold 0))
  (> (select-disjunct-total-sum table) threshold))

#-:debug
(defmacro select-disjunct-more-pos-entries-p (table &optional (threshold 0))
  `(> (select-disjunct-total-sum ,table) ,threshold))

#+:debug
(defun select-disjunct-more-neg-entries-p (table &optional (threshold 0))
  (< (select-disjunct-total-sum table) threshold))

#-:debug
(defmacro select-disjunct-more-neg-entries-p (table &optional (threshold 0))
  `(< (select-disjunct-total-sum ,table) ,threshold))

#+:debug
(defun select-disjunct-total-average (table)
  (let ((no (+ (select-disjunct-no-of-pos-entries table) (select-disjunct-no-of-neg-entries table))))
    (if (eql no 0)
        0
      (/ (select-disjunct-total-sum table) no))))

#-:debug
(defmacro select-disjunct-total-average (table)
  (let ((table-sym (gensym))
        (no-sym (gensym)))
    `(let* ((,table-sym ,table)
            (,no-sym (+ (select-disjunct-no-of-pos-entries ,table-sym)
                        (select-disjunct-no-of-neg-entries ,table-sym))))
       (if (eql ,no-sym 0)
           0
         (/ (select-disjunct-total-sum ,table-sym) ,no-sym)))))

(defun increment-disjunct-table-entry (concept table increment)
  (incf (select-disjunct-no-of-table-changes table))
  (if (>= (select-disjunct-no-of-table-changes table) *disjunct-record-sampling-rate*)
      (progn
        (setf (select-disjunct-no-of-table-changes table) 0)
        (push-select-disjunct-history table))
    (unless (select-disjunct-current-history-entry table)
      (push-select-disjunct-history table)))
  (let ((hash-table (select-disjunct-table table)))
    (multiple-value-bind (value present-p)
        (gethash concept hash-table)
      (if present-p
          (let ((new-value (+ value increment)))
            (setf (gethash concept hash-table) new-value)
            (if (>= value 0)
                (incf (select-disjunct-sum-of-pos-entries table) increment)
              (if (>= new-value 0)
                  (progn
                    (incf (select-disjunct-no-of-pos-entries table))
                    (decf (select-disjunct-no-of-neg-entries table))
                    (incf (select-disjunct-sum-of-neg-entries table) value)
                    (incf (select-disjunct-sum-of-pos-entries table) new-value))
                (decf (select-disjunct-sum-of-neg-entries table) increment)))
            #|(assert (eql (loop for val being the hash-value of hash-table
                                 sum (if (> val 0) val 0))
                           (select-disjunct-sum-of-pos-entries table)))
              (assert (eql (loop for val being the hash-value of hash-table
                                 sum (if (< val 0) (abs val) 0))
                           (select-disjunct-sum-of-neg-entries table)))|#)
        (progn
          (incf (select-disjunct-no-of-pos-entries table))
          (incf (select-disjunct-sum-of-pos-entries table) increment)
          (setf (gethash concept hash-table) increment))))))

(defun decrement-disjunct-table-entry (concept table decrement)
  (incf (select-disjunct-no-of-table-changes table))
  (if (>= (select-disjunct-no-of-table-changes table) *disjunct-record-sampling-rate*)
      (progn
        (setf (select-disjunct-no-of-table-changes table) 0)
        (push-select-disjunct-history table))
    (unless (select-disjunct-current-history-entry table)
      (push-select-disjunct-history table)))
  (let ((hash-table (select-disjunct-table table)))
    (multiple-value-bind (value present-p)
        (gethash concept hash-table)
      (if present-p
          (let ((new-value (- value decrement)))
            (setf (gethash concept hash-table) new-value)
            (if (< value 0)
                (incf (select-disjunct-sum-of-neg-entries table) decrement)
              (if (< new-value 0)
                  (progn
                    (decf (select-disjunct-no-of-pos-entries table))
                    (incf (select-disjunct-no-of-neg-entries table))
                    (decf (select-disjunct-sum-of-pos-entries table) value)
                    (decf (select-disjunct-sum-of-neg-entries table) new-value))
                (decf (select-disjunct-sum-of-pos-entries table) decrement)))
            #|(assert (eql (loop for val being the hash-value of hash-table
                                 sum (if (> val 0) val 0))
                           (select-disjunct-sum-of-pos-entries table)))
              (assert (eql (loop for val being the hash-value of hash-table
                                 sum (if (< val 0) (abs val) 0))
                           (select-disjunct-sum-of-neg-entries table)))|#)
        (progn
          (incf (select-disjunct-no-of-neg-entries table))
          (incf (select-disjunct-sum-of-neg-entries table) decrement)
          (setf (gethash concept hash-table) (- decrement)))))))

(defun get-start-end-history-records (table history-table)
  (let* ((current (select-disjunct-current-history-entry table))
         (oldest (mod (1+ current) (array-total-size history-table))))
    (values (svref history-table oldest) (svref history-table current))))
