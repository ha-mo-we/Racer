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

(defun added-non-linear-cd-predicate-satisfiable-p (cd-constraint
                                                            state
                                                            &optional
                                                            copy-concrete-domain-state
                                                            new-or-dependencies)
  (declare (ignore new-or-dependencies copy-concrete-domain-state))
  (let* ((state (or state (make-solver-state))))
    (values 
     (non-linear-constraint-system-satisfiable-p
      (extract-polynoms (cons cd-constraint (solver-state-cd-constraints state))))
     state)))


(defun all-constraints-satisfiable-p (new-cd-constraint ind-new-cd-state)
  (non-linear-constraint-system-satisfiable-p
   (extract-polynoms (cons new-cd-constraint
                           (solver-state-cd-constraints ind-new-cd-state)))))

(defun extract-polynoms (cd-constraints) 
  (loop for cd-constraint in cd-constraints 
        collect
        (let* ((predicate
                (if (cd-constraint-negated-p cd-constraint)
                  (predicate-negation (cd-constraint-predicate cd-constraint))
                  (cd-constraint-predicate cd-constraint)))
               (definition (predicate-definition predicate))) 
          (if (null definition) 
            (make-polynom-from-predicate 
             (predicate-definition (predicate-negation predicate))
             (predicate-parameters predicate)
             (cd-constraint-varlist cd-constraint)
             t)
            (make-polynom-from-predicate definition 
                                         (predicate-parameters predicate)
                                         (cd-constraint-varlist cd-constraint)
                                         nil)))))

(defun make-polynom-from-predicate (predicate parameters varlist negated-p)
  (let ((operator (first predicate))
        (left-hand-side (second predicate))
        (right-hand-side (third predicate))
        (var-object-association (mapcar #'(lambda (parameter object)
                                            (cons parameter object))
                                        ;; take care about synonyms.
                                        parameters varlist)))
    (case operator
      (= 
       (if negated-p
         (let ((slack-var (gensym)))
           `(+ (* ,slack-var (+ (- ,(variables-substituted left-hand-side
                                                           var-object-association))
                                   ,(variables-substituted right-hand-side
                                                           var-object-association)))
               -1))
         `(+ (- ,(variables-substituted left-hand-side var-object-association)) 
             ,(variables-substituted right-hand-side var-object-association))
         ))
      (<> 
       (if negated-p
         `(+ (- ,(variables-substituted left-hand-side var-object-association)) 
             ,(variables-substituted right-hand-side var-object-association))
         (let ((slack-var (gensym)))
           `(+ (* ,slack-var (+ (- ,(variables-substituted left-hand-side
                                                           var-object-association))
                                ,(variables-substituted right-hand-side
                                                        var-object-association)))
               -1))
         ))
      (t (error "Inequations involving order relations cannot be handled ~
by RACER in the nonlinear case -- found ~A. Note that a predicate ~
might be negated due to internal processing strategies." predicate)))))

(defun variables-substituted (form var-object-association)
  (cond ((null form)
         nil)
        ((consp form)
         (cons (variables-substituted (first form) var-object-association)
               (variables-substituted (rest form) var-object-association)))
        ((symbolp form)
         (let ((object (cdr (assoc form var-object-association))))
           (if object
             object
             form)))
        (t form)))

(defun compute-vars (form)
  (remove-duplicates (loop for elem in form 
                           if (symbolp elem)
                           collect elem
                           else 
                           if (consp elem)
                           nconc (compute-vars (rest elem)))))

(defun non-linear-constraint-system-satisfiable-p (plist)
  (declare (ignore plist))
  (racer-warn "Reasoning may be incomplete because nonlinear constraints are ignored
in this version.")
  t)


(defun unit-polynom-p (val vars)
  (destructuring-bind (exponents . val) 
                      (first (first val))
    (and (eql val 1)
         (= (length exponents) (length vars))
         (loop for exp in exponents
               always (zerop exp)))))




;;; (non-linear-constraint-system-satisfiable-p '((+ (* 2 y y) (* 3 z z) -1) (+ (* 2 y y) (* 3 z z))))
