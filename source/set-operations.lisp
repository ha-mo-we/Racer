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

;;; ======================================================================

(defun racer-remove-eql (elem list)
  (loop for rest-list on list
        for prev-list = nil then rest-list
        for item = (first rest-list)
        if (eql item elem)
        do
        (if prev-list
          (return (values (nconc result (rest rest-list)) t))
          (return (values (rest list) t)))
        else
        collect item into result
        finally
        (return list)))

(defun racer-remove-test (elem list test)
  (loop for rest-list on list
        for prev-list = nil then rest-list
        for item = (first rest-list)
        if (funcall test item elem)
        do
        (if prev-list
          (return (values (nconc result (rest rest-list)) t))
          (return (values (rest list) t)))
        else
        collect item into result
        finally
        (return list)))

#+:debug
(defun racer-remove (elem list &key (test nil))
  (when list
    (if test
      (racer-remove-test elem list test)
      (racer-remove-eql elem list))))

#-:debug
(defmacro racer-remove (elem list &key (test nil))
  (let ((sym (gensym)))    
    (if test
      `(let ((,sym ,list))
         (when ,sym
           (racer-remove-test ,elem ,sym ,test)))
      `(let ((,sym ,list))
         (when ,sym
           (racer-remove-eql ,elem ,sym))))))

;;; ======================================================================

(defun mark-racer-set-count (list mark)
  (loop for elem in list
        for length from 1
        do (setf (racer-set-flag elem) mark)
        finally (return length)))

#+:debug
(defun mark-racer-set (list mark)
  (loop for elem in list do
	(setf (racer-set-flag elem) mark)))

#-:debug
(defmacro mark-racer-set (list mark)
  `(loop for elem in ,list do
         (setf (racer-set-flag elem) ,mark)))

(defmacro inc-marker-variable (variable-symbol)
  `(set ,variable-symbol (1+ (symbol-value ,variable-symbol))))

(defun racer-set-union (list1 list2 variable)
  (if (or (null list1) (eq list1 list2))
    list2
    (if (null list2)
      list1
      (let ((mark (inc-marker-variable variable)))
        (if (< (length list1) (length list2))
          (racer-set-union-1 list2 list1 mark)
          (racer-set-union-1 list1 list2 mark))))))

(defun racer-set-union-1 (list1 list2 mark)
  (mark-racer-set list1 mark)
  (loop with result = nil
        with last-cdr = nil
        for elem in list2
        unless (eql (racer-set-flag elem) mark)
        do 
        (if result
          (progn
            (setf (cdr last-cdr) (list elem))
            (setf last-cdr (cdr last-cdr)))
          (progn
            (setf result (list elem))
            (setf last-cdr result)))
        finally
        (if result
          (setf (cdr last-cdr) list1)
          (setf result list1))
        (return result)))

(defun racer-set-difference (list1 list2 variable)
  (if (or (null list2) (null list1))
      list1
    (if (rest list2)
        (loop with mark = (inc-marker-variable variable)
              with count = (mark-racer-set-count list2 mark)
              with rest = count
              with set-vector = *set-vector*
              initially (setf (fill-pointer set-vector) 0)
              for rest-list on list1
              for elem = (first rest-list)
              if (eql (racer-set-flag elem) mark)
              do (decf rest)
              else
              do
              (vector-push-extend elem set-vector 5000)
              until (eql rest 0)
              finally
              (cond ((eql count rest)
                     (return list1))
                    ((eql rest 0)
                     (if (eql (fill-pointer set-vector) 0)
                         (return (values (rest rest-list) t))
                       (return (values (nconc-vector set-vector (rest rest-list)) t))))
                    (t
                     (return (values (coerce set-vector 'list) t)))))
      (racer-remove (first list2) list1))))

(defun nconc-vector (vector list)
  (if (<= (fill-pointer vector) 10000)
      (nconc (coerce vector 'list) list)
    (loop with last-cdr = nil
          with result = nil
          for elem across vector do
          (if result
              (progn
                (setf (cdr last-cdr) (list elem))
                (setf last-cdr (cdr last-cdr)))
            (progn
              (setf result (list elem))
              (setf last-cdr result)))
          finally
          #+:debug (assert result)
          (setf (cdr last-cdr) list)
          (return result))))
      
(defun racer-set-subsetp (list1 list2 variable)
  (or (null list1)
      (if (null list2)
        nil
        (if (rest list1)
          (let ((mark (inc-marker-variable variable)))
            (mark-racer-set list2 mark)
            (loop for elem in list1
                  always (eql (racer-set-flag elem) mark)))
          (and (member (first list1) list2) t)))))

(defun racer-set-equal (list1 list2 variable)
  (cond ((null list1) (null list2))
        ((null list2) (null list1))
        ((rest list1)
         (let* ((mark (inc-marker-variable variable))
                (length-list2 (mark-racer-set-count list2 mark)))
           (loop for elem in list1
                 for length-list1 from 1
                 when (or (> length-list1 length-list2)
                          (not (eql (racer-set-flag elem) mark)))
                 do (return nil)
                 finally
                 (return (eql length-list1 length-list2)))))
        (t (and (member (first list1) list2) t))))

(defun racer-set-disjoint-p (list1 list2 variable)
  (or (null list1)
      (null list2)
      (if (rest list1)
        (if (rest list2)
          (let ((mark (inc-marker-variable variable)))
            (mark-racer-set list1 mark)
            (loop for elem in list2
                  never (eql (racer-set-flag elem) mark)))
          (not (member (first list2) list1)))
        (not (member (first list1) list2)))))

(defun racer-set-remove-duplicates (list variable)
  (if (or (null list) (null (rest list)))
    list
    (loop with mark1 = (inc-marker-variable variable)
          with changed-p = nil
          for elem in list
          if (eql (racer-set-flag elem) mark1)
          do (setf changed-p t)
          else
          do (setf (racer-set-flag elem) mark1)
          until changed-p
          finally
          (if changed-p
            (return
             (loop with mark2 = (inc-marker-variable variable)
                   for elem in list
                   unless (eql (racer-set-flag elem) mark2)
                   collect elem
                   and do (setf (racer-set-flag elem) mark2)))
            (return list)))))

(defun stable-racer-set-union (list1 list2 variable)
  (if (or (null list1) (eq list1 list2))
    list2
    (if (null list2)
      list1
      (let ((mark (inc-marker-variable variable)))
        (mark-racer-set list1 mark)
        (loop for elem in list2
              unless (eql (racer-set-flag elem) mark)
              collect elem into result
              finally
              (if result
                (return (append list1 result))
                (return list1)))))))

(defun racer-set-intersection (list1 list2 variable)
  (if (or (null list2) (null list1))
    nil
    (let ((mark (inc-marker-variable variable)))
      (mark-racer-set list1 mark)
      (loop for elem in list2
            if (eql (racer-set-flag elem) mark)
            collect elem))))

(defun racer-set-intersection-p (predicate list1 list2 variable)
  (if (or (null list2) (null list1))
      nil
    (let ((mark (inc-marker-variable variable)))
      (mark-racer-set list1 mark)
      (loop for elem in list2
            thereis (and (eql (racer-set-flag elem) mark)
                         (funcall predicate elem))))))

;;; ======================================================================

#+:debug
(defun concept-set-union (list1 list2)
  (racer-set-union list1 list2 '*concept-set-mark*))

#-:debug
(defmacro concept-set-union (list1 list2)
  `(racer-set-union ,list1 ,list2 '*concept-set-mark*))

#+:debug
(defun stable-concept-set-union (list1 list2)
  (stable-racer-set-union list1 list2 '*concept-set-mark*))

#-:debug
(defmacro stable-concept-set-union (list1 list2)
  `(stable-racer-set-union ,list1 ,list2 '*concept-set-mark*))

#+:debug
(defun concept-set-difference (list1 list2)
  (racer-set-difference list1 list2 '*concept-set-mark*))

#-:debug
(defmacro concept-set-difference (list1 list2)
  `(racer-set-difference ,list1 ,list2 '*concept-set-mark*))

#+:debug
(defun concept-set-subsetp (list1 list2)
  (racer-set-subsetp list1 list2 '*concept-set-mark*))

#-:debug
(defmacro concept-set-subsetp (list1 list2)
  `(racer-set-subsetp ,list1 ,list2 '*concept-set-mark*))

#+:debug
(defun concept-set-equal (list1 list2)
  (racer-set-equal list1 list2 '*concept-set-mark*))

#-:debug
(defmacro concept-set-equal (list1 list2)
  `(racer-set-equal ,list1 ,list2 '*concept-set-mark*))

#+:debug
(defun concept-set-disjoint-p (list1 list2)
  (racer-set-disjoint-p list1 list2 '*concept-set-mark*))

#-:debug
(defmacro concept-set-disjoint-p (list1 list2)
  `(racer-set-disjoint-p ,list1 ,list2 '*concept-set-mark*))

#+:debug
(defun concept-set-remove-duplicates (list)
  (racer-set-remove-duplicates list '*concept-set-mark*))

#-:debug
(defmacro concept-set-remove-duplicates (list)
  `(racer-set-remove-duplicates ,list '*concept-set-mark*))

#+:debug
(defun concept-set-intersection (list1 list2)
  (racer-set-intersection list1 list2 '*concept-set-mark*))

(defun concept-set-intersection-f (list1 list2)
  (racer-set-intersection list1 list2 '*concept-set-mark*))

#-:debug
(defmacro concept-set-intersection (list1 list2)
  `(racer-set-intersection ,list1 ,list2 '*concept-set-mark*))

(defun concept-set-simple-clash-p (list1 list2)
  (let ((mark (inc-marker-variable '*concept-set-mark*)))
    (or (concept-set-simple-clash-p-1 list1 mark)
        (concept-set-simple-clash-p-1 list2 mark))))

(defun concept-set-simple-clash-p-1 (list mark)
  (if (null list)
      nil
    (loop with clash = nil
          for elem in list
          do
          (if (eql (racer-set-flag (concept-negated-concept elem)) mark)
              (setf clash elem)
            (setf (racer-set-flag elem) mark))
          until clash
          finally (return clash))))

;;; ======================================================================

#+:debug
(defun individual-set-difference (list1 list2)
  (racer-set-difference list1 list2 '*individual-set-mark*))

#-:debug
(defmacro individual-set-difference (list1 list2)
  `(racer-set-difference ,list1 ,list2 '*individual-set-mark*))

#+:debug
(defun individual-set-intersection (list1 list2)
  (racer-set-intersection list1 list2 '*individual-set-mark*))

(defun individual-set-intersection-f (list1 list2)
  (racer-set-intersection list1 list2 '*individual-set-mark*))

#-:debug
(defmacro individual-set-intersection (list1 list2)
  `(racer-set-intersection ,list1 ,list2 '*individual-set-mark*))

#+:debug
(defun individual-set-remove-duplicates (list)
  (racer-set-remove-duplicates list '*individual-set-mark*))

#-:debug
(defmacro individual-set-remove-duplicates (list)
  `(racer-set-remove-duplicates ,list '*individual-set-mark*))

;;; ======================================================================

#+:debug
(defun constraint-set-difference (list1 list2)
  (racer-set-difference list1 list2 '*constraint-set-mark*))

#-:debug
(defmacro constraint-set-difference (list1 list2)
  `(racer-set-difference ,list1 ,list2 '*constraint-set-mark*))

#+:debug
(defun constraint-set-union (list1 list2)
  (racer-set-union list1 list2 '*constraint-set-mark*))

#-:debug
(defmacro constraint-set-union (list1 list2)
  `(racer-set-union ,list1 ,list2 '*constraint-set-mark*))

#+:debug
(defun constraint-set-remove-duplicates (list)
  (racer-set-remove-duplicates list '*constraint-set-mark*))

#-:debug
(defmacro constraint-set-remove-duplicates (list)
  `(racer-set-remove-duplicates ,list '*constraint-set-mark*))

;;; ======================================================================

#+:debug
(defun role-set-remove-duplicates (list)
  (racer-set-remove-duplicates list '*role-set-mark*))

#-:debug
(defmacro role-set-remove-duplicates (list)
  `(racer-set-remove-duplicates ,list '*role-set-mark*))

#+:debug
(defun role-set-subsetp (list1 list2)
  (racer-set-subsetp list1 list2 '*role-set-mark*))

#-:debug
(defmacro role-set-subsetp (list1 list2)
  `(racer-set-subsetp ,list1 ,list2 '*role-set-mark*))

#+:debug
(defun role-set-equal (list1 list2)
  (racer-set-equal list1 list2 '*role-set-mark*))

#-:debug
(defmacro role-set-equal (list1 list2)
  `(racer-set-equal ,list1 ,list2 '*role-set-mark*))

#+:debug
(defun role-set-disjoint-p (list1 list2)
  (racer-set-disjoint-p list1 list2 '*role-set-mark*))

#-:debug
(defmacro role-set-disjoint-p (list1 list2)
  `(racer-set-disjoint-p ,list1 ,list2 '*role-set-mark*))

#+:debug
(defun role-set-union (list1 list2)
  (racer-set-union list1 list2 '*role-set-mark*))

#-:debug
(defmacro role-set-union (list1 list2)
  `(racer-set-union ,list1 ,list2 '*role-set-mark*))

#+:debug
(defun role-set-intersection-p (predicate list1 list2)
  (racer-set-intersection-p predicate list1 list2 '*role-set-mark*))

#-:debug
(defmacro role-set-intersection-p (predicate list1 list2)
  `(racer-set-intersection-p ,predicate ,list1 ,list2 '*role-set-mark*))


