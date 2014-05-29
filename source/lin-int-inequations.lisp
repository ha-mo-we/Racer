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

(defun linear-cardinal-constraints-satisfiable-p (cd-constraint ind-new-cd-state)
  (linear-cardinal-predicates-satisfiable-p 
   (loop for constraint in (cons cd-constraint 
                                 (solver-state-cd-constraints 
                                  ind-new-cd-state))
         for predicate = (cd-constraint-predicate constraint)
         when (eq (predicate-type predicate) 'cardinal)
         collect predicate)
   (loop for constraint in (cons cd-constraint 
                                 (solver-state-cd-constraints 
                                  ind-new-cd-state))
         for predicate = (cd-constraint-predicate constraint)
         when (eq (predicate-type predicate) 'cardinal)
         collect (cd-constraint-varlist constraint))
   nil))
  

(defun linear-cardinal-predicates-satisfiable-p (predicates vars inequation-system)
  ;; Es koennen auch Praedikate vom Typ equal-predicate, unequal-predicate und
  ;; linear-predicate vorkommen.
  ;; Wegen der Normalisierung kommen bei linearen Praedikaten (linear-predicate)
  ;; nur Praedikate mit predicate-operator von
  ;; der Form < bzw. <= vor.
  (if (null predicates)
    (check-satisfiability-of-inequation-system inequation-system)
    (let ((predicate (first predicates)))
      (etypecase predicate
        (equal-predicate (linear-cardinal-predicates-satisfiable-p 
                          (rest predicates)
                          (rest vars)
                          (list* (cons (predicate-predicate-1 predicate) (first vars))
                                 (cons (predicate-predicate-2 predicate) (first vars))
                                 inequation-system)))
        (unequal-predicate 
         (or (linear-cardinal-predicates-satisfiable-p 
              (rest predicates)
              (rest vars)
              (cons (cons (predicate-predicate-1 predicate) (first vars))
                    inequation-system))
             (linear-cardinal-predicates-satisfiable-p 
              (rest predicates)
              (rest vars)
              (cons (cons (predicate-predicate-2 predicate) (first vars))
                    inequation-system))))
        (linear-predicate 
         (linear-cardinal-predicates-satisfiable-p 
          (rest predicates)
          (rest vars)
          (cons (cons predicate (first vars))
                inequation-system)))
        (divisible-predicate
         (linear-cardinal-predicates-satisfiable-p 
          (rest predicates)
          (rest vars)
          (cons (cons predicate (cond ((eq (predicate-operator predicate) 'divisible)
                                       (cons (gensym "VAR") (first vars)))
                                      ((eq (predicate-operator predicate) 'not-divisible) 
                                       (list* (gensym "VAR") (gensym "VAR") (first vars)))
                                      (t (error "Internal error: should not happen."))))
                inequation-system)))))))

(defun check-satisfiability-of-inequation-system (predicates-and-vars)
  (check-satisfiability-of-inequation-system-1 predicates-and-vars predicates-and-vars
                                               (make-hash-table) 0 0))

(defun check-satisfiability-of-inequation-system-1 (predicates-and-vars 
                                                            all-predicates-and-vars variable-indices
                                                            i n-slack-vars)
  (if (null predicates-and-vars)
    (check-satisfiability-of-inequation-system-2 all-predicates-and-vars variable-indices
                                                 n-slack-vars)
    (let* ((predicate-and-vars (first predicates-and-vars))
           (predicate (first predicate-and-vars))
           (vars (rest predicate-and-vars)))
      (when (eq (predicate-operator predicate) '<)
        (incf n-slack-vars))
      (loop for var in vars do
            (unless (gethash var variable-indices)
              (setf (gethash var variable-indices) i)
              (incf i)))
      (check-satisfiability-of-inequation-system-1 (rest predicates-and-vars) all-predicates-and-vars 
                                                   variable-indices i
                                                   n-slack-vars))))

(defun check-satisfiability-of-inequation-system-2 (all-predicates-and-vars 
                                                            variable-indices n-slack-vars)
  (let* ((n-variables (hash-table-count variable-indices))
         (n (+ n-variables n-slack-vars))
         ;; The width of the matrix is determined by the number of predicates,
         ;; i.e., the number of basic inequations, plus the number of inequations
         ;; for slack variables (s >= 1, or -s <= -1 in normalized form).
         ;; For divisible there are two inequations and for non-divisible there are four 
         ;; inequations
         (m (+ (loop for (predicate . nil) in all-predicates-and-vars
                     sum (cond ((eq (predicate-operator predicate) 'divisible)
                                2)
                               ((eq (predicate-operator predicate) 'not-divisible)
                                4) 
                               (t 1)))
               n-slack-vars))
         ;; The height of the matrix is determined by the number of variables 
         ;; plus the number of slack variables for dealing with < predicates.
         
         (*simplex-number-of-variables* (+ n-variables n-slack-vars))
         (*simplex-number-of-equations* (+ (length all-predicates-and-vars) n-slack-vars)))
    
    (let ((matrix (make-sparse-array (list m n) 
                                     :element-type 'rational :initial-element 0))
          (b (make-array m :element-type 'rational :initial-element 0))
          (c (make-array n :element-type 'rational :initial-element 1)))
      
      ;;(racer-warn "Incomplete reasoning for ~A" all-predicates-and-vars)

      ;; The first n-variables are for the real variables (index determined by variable-indices).
      ;; Afterwards, the slack vars are allocated one after the other.
      (loop with i = 0 ; iterate over the equations.
            with j = n-variables ; iterate over the slack vars.
            for (predicate . vars) in all-predicates-and-vars
            do  
            (let ((coefficients-list (if (divisible-predicate-p predicate)
                                       (compute-coefficients-list predicate)
                                       (list (predicate-coefficients predicate))))
                  (right-sides-list (if (divisible-predicate-p predicate)
                                     (compute-right-sides-list predicate)
                                     (list (predicate-right-side predicate)))))
              (loop for coefficients in coefficients-list
                    for right-side in right-sides-list do
                    (unless (integerp right-side)
                      (error "Currently, Racer can handle only linear cardinal ~
                              (in)equations with integer coefficients - found ~A." predicate)) 
                    (loop for coefficient across coefficients
                          for variable in vars do
                          (unless (integerp coefficient)
                            (error "Currently, Racer can handle only linear cardinal ~
                                    (in)equations with integer coefficients - found ~A." predicate)) 
                          (setf (saref matrix i (gethash variable variable-indices))
                                coefficient))
                    (setf (aref b i) right-side)
                    (incf i)))
            ;; Now we deal with slack variables required for < 
            (when (eq (predicate-operator predicate) '<)
              (setf (saref matrix (- i 1) j) 1)
              (setf (saref matrix i j) -1)
              (setf (aref b i) -1)
              (incf j)
              (incf i)))
      (let ((*simplex-number-of-iterations* 0))
        (multiple-value-bind (solvable-p) 
                             (gomory1 matrix b c)
          (push-statistics (list *simplex-number-of-variables*
                                 *simplex-number-of-equations*
                                 *simplex-number-of-iterations*)
                           *simplex-statistics-list*)
          ;(print sol)
          solvable-p)))))

(defun compute-coefficients-list (predicate)
  (let ((divisor (third (predicate-definition predicate))))
    (cond ((eq (predicate-operator predicate) 'divisible)
           (list (vector divisor -1)
                 (vector (- divisor) 1)))
          ((eq (predicate-operator predicate) 'not-divisible)
           (list (vector divisor 1 -1)
                 (vector (- divisor) -1 1)
                 (vector 0 1 0)
                 (vector 0 -1 0)))
          (t (error "Internal error: should not happen.")))))

(defun compute-right-sides-list (predicate)
  (cond ((eq (predicate-operator predicate) 'divisible)
         (list 0 0))
        ((eq (predicate-operator predicate) 'not-divisible)
         (let ((divisor (third (predicate-definition predicate))))
           (list 0 0 (- divisor 1) -1)))
        (t (error "Internal error: should not happen."))))





