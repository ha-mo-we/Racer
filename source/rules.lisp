;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: ARCER; Base: 10 -*-

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

(defstruct swrl-rule
  name
  head
  body)

(defmethod print-object ((object swrl-rule) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S <- (and ~{~S~%~}" 
            (swrl-rule-head object)
            (swrl-rule-body object))))

;;; ======================================================================

(defun add-rule-axiom (abox lefthand-side righthand-side &key id (forward-rule-p t) (backward-rule-p t)
                            (fire-once-p nil))
  (setf abox (find-abox abox))
  (allow-overloaded-definitions)
  ;;(prefer-defined-queries)
  (when forward-rule-p
    (let ((negated
           (mapcar #'(lambda (x) 
                       `(neg ,x))
                   (if (and (consp lefthand-side)
                            (member (first lefthand-side) 
                                    '(and or union)))
                       lefthand-side
                     (list lefthand-side)))))
      (racer-prepare-rule (if fire-once-p 
                              `(and ,righthand-side
                                    ,@negated)
                            righthand-side)
                          (cond ((and (consp lefthand-side) 
                                      (eq (first lefthand-side) 'same-as))
                                 lefthand-side)
                                ((and (consp lefthand-side) 
                                      (member (first lefthand-side) '(and or union not neg)))
                                 (error "Cannot handle ~S in define-rule." lefthand-side))
                                ((and (consp lefthand-side) 
                                      (= (length lefthand-side) 3))
                                 `((related .,lefthand-side)))
                                ((and (consp lefthand-side) 
                                      (= (length lefthand-side) 2))
                                 `((instance .,lefthand-side)))
                                (t (error "Cannot handle ~S in define-rule." lefthand-side)))
                          :abox (abox-name abox)
                          :id id)))
  (when backward-rule-p
    (if (and (consp lefthand-side) 
             (eq (first lefthand-side) 'same-as))
        (define-query (first lefthand-side) (rest lefthand-side) righthand-side
                      :tbox (tbox-name (tbox abox)))
      (define-query (first (last lefthand-side)) (butlast lefthand-side) righthand-side
                    :tbox (tbox-name (tbox abox)))))
  t)

(defmacro define-rule (lefthand-side righthand-side &key (forward-rule-p t) (backward-rule-p t))
  `(add-rule-axiom (current-abox) ',lefthand-side ',righthand-side 
                   :forward-rule-p ,forward-rule-p 
                   :backward-rule-p ,backward-rule-p))






