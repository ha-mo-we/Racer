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

(defstruct (sparse-array (:constructor make-sparse-array-internal)
                         (:copier copy-sparse-array-internal))
  (x-dim nil)                           ; only for debugging purposes
  (y-dim nil)                           ; only for debugging purposes
  (initial-element nil)                 ; must be either nil or 0
                                        ; only for debugging purposes
  (table nil)                           ; vector of rows of a sparse array
                                        ; row as list with elements of the form
                                        ; (<index> . <value>) or
                                        ; ((<upper-bound> . <lower-bound>) . <value>)
  )
  
(defmethod print-object ((array sparse-array) stream)
  (let ((no-of-elements (loop for elem across (sparse-array-table array)
                              sum (length elem)))
        (size (* (sparse-array-x-dim array) (sparse-array-y-dim array))))
    (print-unreadable-object (array stream :type t :identity t)
      (format stream "((~Sx~S):~S, ~D elements, filled ~,2F%)"
              (sparse-array-x-dim array)
              (sparse-array-y-dim array)
              (sparse-array-initial-element array)
              no-of-elements
              (if (zerop size)
                0
                (/ (* no-of-elements 100.0) size))))))

(defun sparse-array-dimension (array dimension)
  (if (sparse-array-p array)
    (if (zerop dimension)
      (sparse-array-x-dim array)
      (if (eql dimension 1)
        (sparse-array-y-dim array)
        (error "cannot return the dimension ~S of ~S" dimension array)))
    (array-dimension array dimension)))

(defun saref (array x y)
  (declare (type fixnum x y))
  (if (arrayp array)
    (aref array x y)
    (progn
      #+:debug (assert (sparse-array-p array))
      (or (cdr (assoc x (aref (sparse-array-table array) y)))
          0))))

(defun (setf saref) (new-value array x y)
  (if (arrayp array)
    (setf (aref array x y) new-value)
    (progn
      #+:debug (assert (sparse-array-p array))
      (let* ((table (sparse-array-table array))
             (row (aref table y))
             (elem (assoc x row)))
        (if elem
          (if (zerop new-value)
            (setf (aref table y) (delete x row :key #'car))
            (setf (cdr elem) new-value))
          (unless (zerop new-value)
            (setf (aref table y) (insert-elem x new-value row)))))
      new-value)))

(defun insert-elem (key value list &optional (pre-list nil))
  (if (null list)
    (if (null pre-list)
      (list (cons key value))
      (setf (cdr pre-list) (list (cons key value))))
    (progn
      (if (< key (car (first list)))
        (if (null pre-list)
          (push (cons key value) list)
          (setf (cdr pre-list) (cons (cons key value) list)))
        (insert-elem key value (cdr list) list))
      list)))

(defun make-sparse-array (dims &key (element-type t) (initial-element nil))
  (if (and *simplex-use-sparse-arrays*
           (consp dims)
           (eql (length dims) 2)
           (> (* (first dims) (second dims)) 100))
    (progn
      #+:debug (assert (or (null initial-element) (zerop initial-element)))
      (make-sparse-array-internal :x-dim (first dims)
                                  :y-dim (second dims)
                                  :initial-element initial-element
                                  :table (make-array (second dims)
                                                     :initial-element nil)))
    (make-array dims
                :element-type element-type
                :initial-element initial-element)))

(defun cd-make-sparse-array (dims)
  (if (and *cd-use-sparse-arrays*
           ;(consp dims)
           ;(eql (length dims) 2)
           ;(> (* (first dims) (second dims)) 100)
           )
    (progn
      (make-sparse-array-internal :x-dim (first dims)
                                  :y-dim (second dims)
                                  :table (make-array (second dims) :initial-element nil)))
    (make-array dims :adjustable t)))

(defun copy-sparse-array (array &optional (new-dims nil))
  (if (sparse-array-p array)
    (let ((new-array (copy-sparse-array-internal array)))
      (setf (sparse-array-table new-array) (copy-seq (sparse-array-table array)))
      (when new-dims
        (setf (sparse-array-x-dim new-array) (first new-dims))
        (setf (sparse-array-y-dim new-array) (second new-dims))
        (setf (sparse-array-table new-array)
              (adjust-array (sparse-array-table new-array) (second new-dims) :initial-element nil)))
      (loop for old-entry across (sparse-array-table array)
            for index from 0 do
            (setf (aref (sparse-array-table new-array) index) (copy-tree old-entry)))
      new-array)
    (let ((dims (or new-dims (array-dimensions array))))
      (if (rest dims)
        (let ((new-array (make-array dims :initial-element 0)))
          (loop for row from 0 below (array-dimension array 0) do
                (loop for col from 0 below (array-dimension array 1)
                      for old-val = (aref array row col)
                      unless (zerop old-val)
                      do (setf (aref new-array row col) old-val)))
          new-array)
        (progn
          #+:debug (assert (null new-dims))
          (copy-seq array))))))

(defun adjust-sparse-array (array dims)
  (if (sparse-array-p array)
    (progn
      (setf (sparse-array-x-dim array) (first dims))
      (setf (sparse-array-y-dim array) (second dims))
      (setf (sparse-array-table array)
            (adjust-array (sparse-array-table array) (second dims) :initial-element nil))
      array)
    (adjust-array array dims :initial-element 0)))


