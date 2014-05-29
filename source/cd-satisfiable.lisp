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

(defun constraint-satisfiable-p (constraint
                                     state
                                     &optional (copy-concrete-domain-state nil)
                                     (new-or-dependencies nil))
  "Ueberprueft ob der cd-constraint constraint zusammen mit dem 
   solver-state state erfuellbar ist. Falls ja werden zwei Werte
   zurueckgegeben: T und der neue solver-state nach dem Hinzufuegen
   des neuen constraints. Im Falle der Unerfuellbarkeit wird nil
   zurueckgegeben"
  ; constraint ueberpruefen:
  (let ((pred (cd-constraint-predicate constraint)))
    (unless pred
      (return-from constraint-satisfiable-p (values nil "Unknown predicate: "))) 
    (cond ((equal-predicate-p pred)
           (multiple-value-bind (satisfiable-p new-state)
                                (let ((pred-1 (predicate-predicate-1 pred)))
                                  (constraint-satisfiable-p-1 constraint
                                                              pred-1
                                                              state
                                                              copy-concrete-domain-state 
                                                              new-or-dependencies))
             (when satisfiable-p
               (let ((pred-2 (predicate-predicate-2 pred)))
                 (constraint-satisfiable-p-1 constraint
                                             pred-2
                                             new-state
                                             copy-concrete-domain-state
                                             new-or-dependencies)))))
          ((unequal-predicate-p pred)
           (multiple-value-bind (satisfiable-p new-state) 
                                (let ((pred-1 (predicate-predicate-1 pred)))
                                  (constraint-satisfiable-p-1 constraint
                                                              pred-1
                                                              state
                                                              copy-concrete-domain-state 
                                                              new-or-dependencies))
             (if satisfiable-p
               (values t new-state)
               (let ((pred-2 (predicate-predicate-2 pred)))
                 (constraint-satisfiable-p-1 constraint
                                             pred-2
                                             state
                                             copy-concrete-domain-state
                                             new-or-dependencies)))))
          (t (constraint-satisfiable-p-1 constraint
                                         pred
                                         state
                                         copy-concrete-domain-state
                                         new-or-dependencies)))))

(defun negated-constraint-entailed-p (constraint
                                      state
                                      attribute-fillers
                                      &optional (copy-concrete-domain-state nil)
                                      (new-or-dependencies nil))
  (let* ((neg-constraint
          (make-constraint (predicate-negation (concept-predicate (constraint-term constraint)))
                           attribute-fillers))
         (predicate (cd-constraint-predicate neg-constraint)))
    (unless predicate
      (return-from negated-constraint-entailed-p (values nil "Unknown predicate: ")))
    (multiple-value-bind (satisfiable new-cd-state)
        (cond ((or (equal-predicate-p predicate) (unequal-predicate-p predicate))
               ;; the original constraint is an un/equal predicate
               ;; it is only entailed if both variants clashed
               (multiple-value-bind (satisfiable-p new-state)
                   (constraint-satisfiable-p-1 neg-constraint
                                               (predicate-predicate-1 predicate)
                                               state
                                               copy-concrete-domain-state 
                                               new-or-dependencies)
                 (if satisfiable-p
                     (values t new-state)
                   (constraint-satisfiable-p-1 neg-constraint
                                               (predicate-predicate-2 predicate)
                                               state
                                               copy-concrete-domain-state
                                               new-or-dependencies))))
              (t (constraint-satisfiable-p-1 neg-constraint
                                             predicate
                                             state
                                             copy-concrete-domain-state
                                             new-or-dependencies)))
      (values (not satisfiable) new-cd-state))))

(defparameter *flag* nil)

(defun constraint-satisfiable-p-1 (constraint
                                        pred
                                        state
                                        copy-concrete-domain-state
                                        new-or-dependencies)
  (let ((pred-name (predicate-name pred))
        (vars (cd-constraint-varlist constraint))
        (neg (cd-constraint-negated-p constraint)))
        ;(pred (gethash pred-name preds)))
    (unless pred
      (return-from constraint-satisfiable-p-1 (values nil "Unknown predicate: " pred-name))) 

    (when neg (error "wrong");; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!****************************************
      (setf pred neg))



    (unless (eql (length vars) (predicate-arity pred))
      (error "Wrong number of arguments for predicate ~A " pred-name))
    (when nil ;*flag*
      (format t "~%testing satisfiability of constraint system:~%")
      (loop for cd-constraint in (append (and state (reverse (solver-state-cd-constraints state)))
                                         (list constraint)) do
            (format t "~A~%" 
                    (if (cd-constraint-negated-p cd-constraint)
                      (predicate-negation (cd-constraint-predicate cd-constraint))
                      (cd-constraint-predicate cd-constraint))))
      (format t "~A~%" pred))
    (multiple-value-bind (sat new-state)
        (cond ((linear-predicate-p pred) 
               (multiple-value-bind (satisfiable-p ind-new-cd-state)
                   (added-cd-predicate-satisfiable-p state
                                                     pred
                                                     vars
                                                     copy-concrete-domain-state
                                                     new-or-dependencies)
                 (if satisfiable-p
                     (if (eq (predicate-type pred) 'cardinal)
                         (if (linear-cardinal-constraints-satisfiable-p constraint ind-new-cd-state)
                             (if (solver-state-contains-additional-constraints ind-new-cd-state)
                                 (if (all-constraints-satisfiable-p constraint ind-new-cd-state)
                                     (values t ind-new-cd-state)
                                   nil)
                               (values satisfiable-p ind-new-cd-state))
                           nil)
                       (if (solver-state-contains-additional-constraints ind-new-cd-state)
                           (if (all-constraints-satisfiable-p constraint ind-new-cd-state)
                               (values t ind-new-cd-state)
                             nil)
                         (values satisfiable-p ind-new-cd-state)))
                   (values satisfiable-p ind-new-cd-state)))) 
              ((nonlinear-predicate-p pred)
               (added-non-linear-cd-predicate-satisfiable-p constraint
                                                            state
                                                            copy-concrete-domain-state
                                                            new-or-dependencies))
              ((divisible-predicate-p pred)
               (added-divisible-cd-predicate-satisfiable-p
                constraint
                state
                copy-concrete-domain-state
                new-or-dependencies))
              ((string-predicate-p pred)
               (added-string-cd-predicate-satisfiable-p constraint
                                                        state
                                                        copy-concrete-domain-state
                                                        new-or-dependencies))
              ((predicate-p pred)
               #+:debug
               (unless (eq (predicate-operator pred) 'top)
                 (racer-warn "Ignoring ~A." pred))
               (values t state))
              (t (error "Cannot handle this constraint ~S." constraint)))
      (if sat
          (race-trace ("~&Added predicate ~S to CD-state ~S => ~S~%"
                       pred
                       (copy-solver-state state)
                       (copy-solver-state new-state)))
        (race-trace ("~&Adding predicate ~S to CD-state ~S caused an unsat CD-state~%"
                     pred
                     (copy-solver-state state))))
      ;(when *flag* (format t "---> ~A~%" sat))
      (values sat new-state))))


(defun constraint-satisfiable-p-2 (constraint
                                    state
                                    copy-concrete-domain-state
                                    new-or-dependencies
                                    continuation)
  "Ueberprueft ob der cd-constraint constraint zusammen mit dem 
   solver-state state erfuellbar ist. Falls ja werden zwei Werte
   zurueckgegeben: T und der neue solver-state nach dem Hinzufuegen
   des neuen constraints. Im Falle der Unerfuellbarkeit wird nil
   zurueckgegeben"
  ; constraint ueberpruefen:
  (let ((pred (cd-constraint-predicate constraint))
        (cd-state (state-concrete-domain-state state)))
    (unless pred
      (error "Unknown predicate: "))
    (cond 
     ((equal-predicate-p pred)
      (multiple-value-bind (satisfiable-p-1 new-cd-state-1) 
          (let ((pred-1 (predicate-predicate-1 pred)))
            (constraint-satisfiable-p-1 constraint
                                        pred-1
                                        cd-state
                                        copy-concrete-domain-state 
                                        new-or-dependencies))
        (if satisfiable-p-1
            (multiple-value-bind (satisfiable-p-2 new-cd-state-2) 
                (let ((pred-2 (predicate-predicate-2 pred)))
                  (constraint-satisfiable-p-1 constraint
                                              pred-2
                                              new-cd-state-1
                                              copy-concrete-domain-state
                                              new-or-dependencies))
              (if satisfiable-p-2
                  (let ((new-state (changed-kernel-state state 
                                                         :concrete-domain-state
                                                         new-cd-state-2)))
                    (funcall continuation t new-state nil nil nil))
                (let ((new-state (changed-kernel-state state
                                                       :concrete-domain-state
                                                       new-cd-state-1)))
                  (handle-clash-with-backtrack-state new-state nil nil nil nil))))
          (handle-clash-with-backtrack-state state nil nil nil nil))))
     ((unequal-predicate-p pred)
      (multiple-value-bind (satisfiable-p-1 new-cd-state-1) 
          (let ((pred-1 (predicate-predicate-1 pred)))
            (constraint-satisfiable-p-1 constraint
                                        pred-1
                                        cd-state
                                        t ;copy-concrete-domain-state 
                                        new-or-dependencies))
        (if satisfiable-p-1
            (flet ((constraint-satisfiable-p-2-continuation-1 (sat-p
                                                               new-state
                                                               unused-1
                                                               unused-2
                                                               unused-3)
                     (declare (ignore unused-1 unused-2 unused-3))
                     #-(and :debug :lispworks) (declare (ignore new-state))
                     (or sat-p
                         (if t ;need to add some backjumping mechanisms
                             (let ((first-clash-dependencies *catching-clash-dependencies*))
                               (flet ((constraint-satisfiable-p-2-continuation-2 (sat-p
                                                                                  new-state
                                                                                  unused-1
                                                                                  unused-2
                                                                                  unused-3)
                                        (declare (ignore unused-1 unused-2 unused-3))
                                        #-(and :debug :lispworks) (declare (ignore new-state))
                                        (or sat-p
                                            (progn
                                              (setf *catching-clash-dependencies*
                                                    (union-dependencies *catching-clash-dependencies*
                                                                        first-clash-dependencies))
                                              (race-trace ("Alternative predicate ~S failed, state ~S"
                                                           (predicate-predicate-2 pred)
                                                           new-state))
                                              (race-trace ("Backjumping with state ~S, dependencies ~S"
                                                           state *catching-clash-dependencies*))
                                              (handle-clash-with-backtrack-state state nil nil nil nil)))))
                                 (race-trace ("Predicate ~S failed with state ~S. Trying alternative ~S"
                                              (predicate-predicate-1 pred)
                                              new-state
                                              (predicate-predicate-2 pred)))
                                 (multiple-value-bind (satisfiable-p-2 new-cd-state-2) 
                                     (let ((pred-2 (predicate-predicate-2 pred)))
                                       (constraint-satisfiable-p-1 constraint
                                                                   pred-2
                                                                   cd-state 
                                                                   copy-concrete-domain-state 
                                                                   new-or-dependencies))
                                   (if satisfiable-p-2
                                       (let* ((new-state-1 (copy-basic-kernel-state state t))
                                              (new-backtrack-stack
                                               (push-backtrack-stack #'constraint-satisfiable-p-2-continuation-2
                                                                     (state-partially-expanded-or-stack new-state-1)))
                                              (new-state-2 (changed-kernel-state new-state-1
                                                                                 :concrete-domain-state
                                                                                 new-cd-state-2
                                                                                 :partially-expanded-or-stack
                                                                                 new-backtrack-stack)))
                                         (race-trace ("~&Saving backtrack closure ~S in stack ~S of state ~S~%"
                                                      #'constraint-satisfiable-p-2-continuation-2
                                                      (copy-seq new-backtrack-stack)
                                                      new-state-2))
                                         (funcall continuation t new-state-2 nil nil nil))
                                     (let ((new-state (changed-kernel-state state
                                                                            :concrete-domain-state
                                                                            cd-state)))
                                       (handle-clash-with-backtrack-state new-state nil nil nil nil))))))))))
              (let* ((new-state-1 (copy-basic-kernel-state state t))
                     (new-backtrack-stack
                      (push-backtrack-stack #'constraint-satisfiable-p-2-continuation-1
                                            (state-partially-expanded-or-stack new-state-1)))
                     (new-state-2 (changed-kernel-state new-state-1
                                                        :concrete-domain-state new-cd-state-1
                                                        :partially-expanded-or-stack
                                                        new-backtrack-stack)))
                (race-trace ("~&Saving backtrack closure ~S in stack ~S of state ~S~%"
                             #'constraint-satisfiable-p-2-continuation-1
                             (copy-seq new-backtrack-stack)
                             new-state-2))
                (funcall continuation t new-state-2 nil nil nil)))
          (multiple-value-bind (satisfiable-p-2 new-cd-state-2) 
              (let ((pred-2 (predicate-predicate-2 pred)))
                (constraint-satisfiable-p-1 constraint
                                            pred-2
                                            cd-state
                                            copy-concrete-domain-state 
                                            new-or-dependencies))
            (if satisfiable-p-2
                (let ((new-state (changed-kernel-state state
                                                       :concrete-domain-state
                                                       new-cd-state-2)))
                  (funcall continuation t new-state nil nil nil))
              (let ((new-state (changed-kernel-state state :concrete-domain-state cd-state)))
                (handle-clash-with-backtrack-state new-state nil nil nil nil)))))))
     (t (multiple-value-bind (satisfiable-p new-cd-state) 
            (constraint-satisfiable-p-1 constraint
                                        pred
                                        cd-state
                                        copy-concrete-domain-state
                                        new-or-dependencies)
          (if satisfiable-p
              (let ((new-state (changed-kernel-state state :concrete-domain-state new-cd-state)))
                (funcall continuation t new-state nil nil nil))
            (let ((new-state (changed-kernel-state state :concrete-domain-state cd-state)))
              (handle-clash-with-backtrack-state new-state nil nil nil nil))))))))

