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

(defun added-string-cd-predicate-satisfiable-p (cd-constraint
                                                       state
                                                       &optional
                                                       copy-concrete-domain-state
                                                       new-or-dependencies)
  #-(and :debug :lispworks) (declare (ignore new-or-dependencies))
  (with-race-trace-sublevel ("added-string-cd-predicate-satisfiable-p"
                             :arguments (list cd-constraint state copy-concrete-domain-state new-or-dependencies)
                             :expanded t
                             :trace-result t)
    (let* ((state-1 (if copy-concrete-domain-state
                     (or (and state (copy-solver-state state)) (make-solver-state))
                     (or state (make-solver-state)))))
      (values 
       (string-inequation-system-satisfiable 
         cd-constraint state-1)
       state-1))))


(defstruct var-entry
  constant-equation
  constant-inequations
  var-equations
  var-inequations)

(defun get-var-entry-var-equations-transitively (entry variable-values visited)
  (unless (gethash entry visited)
    (setf (gethash entry visited) t)
    (let ((vars (var-entry-var-equations entry)))
      (append vars
              (loop for var in vars
                    append (get-var-entry-var-equations-transitively (gethash var variable-values) 
                                                                     variable-values visited))))))

(defun string-inequation-system-satisfiable (cd-constraint state)
  (let ((variable-values (or (solver-state-string-var-values-mapping state) 
                             (racer-make-hash-table))))
    (setf (solver-state-string-var-values-mapping state) variable-values)
    (let* ((predicate
            (if (cd-constraint-negated-p cd-constraint)
              (predicate-negation (cd-constraint-predicate cd-constraint))
              (cd-constraint-predicate cd-constraint)))
           (parameters (predicate-parameters predicate))
           (varlist (cd-constraint-varlist cd-constraint))
           (var-object-association (mapcar #'(lambda (parameter object)
                                               (cons parameter object))
                                           ;; take care about synonyms.
                                           parameters varlist)))
      (race-trace ("~&Testing satisfiability of predicate ~A ~A ~A~%" predicate var-object-association
                   (copy-hash-table variable-values)))
      (typecase predicate
        (unary-string-predicate
         (let* ((object (cdr (assoc (first parameters) var-object-association)))
                (var (or object (first parameters)))
                (entry (gethash var variable-values)))
           (if (null entry)
             (ecase (predicate-operator predicate)
               (string= 
                ;;(break "1:~A" predicate)
                (setf (gethash var variable-values) 
                      (make-var-entry :constant-equation (predicate-string predicate)
                                      :var-equations (list var))))
               (string<> 
                ;;(break "2:~A" predicate)
                (setf (gethash var variable-values) 
                      (make-var-entry :constant-inequations (list (predicate-string predicate))
                                      :var-inequations nil))))
             (ecase (predicate-operator predicate)
               (string= 
                ;;(break "3:~A" cd-constraint)
                (cond ((loop for var-1 in (var-entry-var-inequations entry)
                             for entry-1 = (gethash var-1 variable-values)
                             thereis 
                             (string= (var-entry-constant-equation entry-1)
                                      (predicate-string predicate)))
                       (return-from string-inequation-system-satisfiable nil))
                      ((loop for var-1 in (get-var-entry-var-equations-transitively entry variable-values
                                                                                    (make-hash-table))
                             for entry-1 = (gethash var-1 variable-values)
                             thereis 
                             (and (var-entry-constant-equation entry-1)
                                  (not (string= (var-entry-constant-equation entry-1)
                                                (predicate-string predicate)))))
                       (return-from string-inequation-system-satisfiable nil))
                      ((find (predicate-string predicate)
                             (var-entry-constant-inequations entry)
                             :test #'string=)
                       (return-from string-inequation-system-satisfiable nil))
                      ((var-entry-constant-equation entry)
                       (if (not (string= (var-entry-constant-equation entry)
                                         (predicate-string predicate)))
                         (return-from string-inequation-system-satisfiable nil)))
                      (t (setf (var-entry-constant-equation entry) 
                               (predicate-string predicate)))))
               (string<> 
                ;;(break "4:~A" predicate)
                (cond ((var-entry-constant-equation entry)
                       (if (string= (var-entry-constant-equation entry)
                                    (predicate-string predicate))
                         (return-from string-inequation-system-satisfiable nil)))
                      (t (pushnew (predicate-string predicate)
                                  (var-entry-constant-inequations entry)))))))))
        (binary-string-predicate 
         (let* ((object-1 (cdr (assoc (first parameters) var-object-association)))
                (var-1 (or object-1 (first parameters)))
                (object-2 (cdr (assoc (second parameters) var-object-association)))
                (var-2 (or object-2 (second parameters)))
                (entry-1 (gethash var-1 variable-values))
                (entry-2 (gethash var-2 variable-values)))
           (race-trace ("~&Entries ~A ~A~%" (and entry-1 
                                                 (copy-var-entry entry-1))
                        (and entry-2 (copy-var-entry entry-2))))
           (ecase (predicate-operator predicate)
             (string=
              ;;(break "5:~A" predicate)
              (cond ((and (null entry-1) (null entry-2))
                     (let ((entry (make-var-entry :var-equations (list object-1 object-2))))
                       (setf (gethash var-1 variable-values) entry)
                       (setf (gethash var-2 variable-values) entry)))
                    ((null entry-2)
                     (pushnew object-2 (var-entry-var-equations entry-1))
                     (setf (gethash var-2 variable-values) entry-1))
                    ((null entry-1)
                     (pushnew object-1 (var-entry-var-equations entry-2))
                     (setf (gethash var-1 variable-values) entry-2))
                    ((find (var-entry-constant-equation entry-1)
                           (var-entry-constant-inequations entry-2)
                           :test #'string=)
                     (return-from string-inequation-system-satisfiable nil))
                    ((and (var-entry-constant-equation entry-2)
                          (find (var-entry-constant-equation entry-2)
                                (var-entry-constant-inequations entry-1) 
                                :test #'string=))
                     (return-from string-inequation-system-satisfiable nil))
                    ((and (var-entry-constant-equation entry-1)
                          (var-entry-constant-equation entry-2))
                     (if (not (string= (var-entry-constant-equation entry-1)
                                       (var-entry-constant-equation entry-2)))
                       (return-from string-inequation-system-satisfiable nil)))
                    ((intersection (cons object-1 (var-entry-var-equations entry-2))
                                   (var-entry-var-inequations entry-1))
                     (return-from string-inequation-system-satisfiable nil))
                    ((intersection (cons object-2 (var-entry-var-equations entry-1))
                                   (var-entry-var-inequations entry-1))
                     (return-from string-inequation-system-satisfiable nil))
                    ((and (find object-1 (var-entry-var-inequations entry-2))
                          (find object-2 (var-entry-var-inequations entry-2)))
                     (return-from string-inequation-system-satisfiable nil))
                    (t
                     (let ((constant-equation (or (var-entry-constant-equation entry-1)
                                                  (var-entry-constant-equation entry-2)))
                           (constant-inequations (union (var-entry-constant-inequations entry-1)
                                                        (var-entry-constant-inequations entry-2)))
                           (var-equations (union (var-entry-var-equations entry-1)
                                                 (var-entry-var-equations entry-2)))
                           (var-inequations (union (var-entry-var-inequations entry-1)
                                                   (var-entry-var-inequations entry-2))))
                       (setf (var-entry-constant-equation entry-1) constant-equation)
                       (setf (var-entry-constant-inequations entry-1) constant-inequations)
                       (setf (var-entry-var-equations entry-1) var-equations)
                       (setf (var-entry-var-inequations entry-1) var-inequations)
                       (setf (gethash var-2 variable-values) entry-1)))))
             (string<>
              ;;(break "6:~A" predicate)
              (cond ((and (null entry-1) (null entry-2))
                     (let ((entry-1 (make-var-entry :var-inequations (list (second parameters))))
                           (entry-2 (make-var-entry :var-inequations (list (first parameters)))))
                       (setf (gethash var-1 variable-values) entry-1)
                       (setf (gethash var-2 variable-values) entry-2)))
                    ((null entry-2)
                     ;;(pushnew (first parameters) (var-entry-var-inequations entry-1))
                     (pushnew (second parameters) (var-entry-var-inequations entry-1))
                     (setf (gethash var-2 variable-values) 
                           (make-var-entry :var-inequations (list (first parameters)))))
                    ((null entry-1)
                     (pushnew (first parameters) (var-entry-var-inequations entry-2))
                     ;;(pushnew (second parameters) (var-entry-var-inequations entry-2))
                     (setf (gethash var-1 variable-values) 
                           (make-var-entry :var-inequations (list (second parameters)))))
                    ((and (var-entry-constant-equation entry-2)
                          (var-entry-constant-equation entry-1))
                     (if (string= (var-entry-constant-equation entry-2)
                                  (var-entry-constant-equation entry-1))
                       (return-from string-inequation-system-satisfiable nil)))
                    ((intersection (cons (first parameters) (var-entry-var-equations entry-2))
                                   (var-entry-var-inequations entry-1))
                     (return-from string-inequation-system-satisfiable nil))
                    ((intersection (cons (second parameters) (var-entry-var-equations entry-1))
                                   (var-entry-var-inequations entry-1))
                     (return-from string-inequation-system-satisfiable nil))
                    (t
                     (pushnew (first parameters) (var-entry-var-inequations entry-2))
                     (pushnew (second parameters) (var-entry-var-inequations entry-1))
                     ))))))))
    ;;(break)
    t))
