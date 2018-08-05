;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

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

(defstruct (basic-label-info
            (:predicate basic-label-info-p)
            (:constructor make-basic-label-info)
            (:conc-name label-info-))
  (label-or-concept nil)                ; original label of individual
  (dependent-models nil)                ; either a list or hash table of model depending on this individual
  (role nil)                            ; incoming role
  (ind nil)                             ; own individual name 
  (model-in-progress nil)               ; list of models under construction
  (blocked-constraints nil)             ; constraints blocked by this individual
  (blocked-labels nil)                  ; labels blocked by this individual
  )

(defmethod print-object ((label basic-label-info) stream)
  (print-unreadable-object (label stream)
    (if (label-info-ind label)
      (progn
        (when (or (label-info-blocked-labels label)
                  (label-info-blocked-constraints label))
          (format stream "*"))
        (format stream "(~S ~S:(~S))"
                (label-info-role label)
                (label-info-ind label)
                (label-info-model-in-progress label)))
      (format stream "(~S:~S)"
              (label-info-role label)
              (or (label-info-label-or-concept label)
                  (label-info-model-in-progress label))))))

(defstruct (inverse-label-info
            (:include basic-label-info)
            (:predicate inverse-label-info-p)
            (:constructor make-inverse-label-info)
            (:conc-name label-info-))
  (previous-ind nil)                    ; name of predecessor
  (back-propagated-concepts nil)        ; initial label of the exists concept causing the creation of
                                        ; this individual plus current back-propagated concepts       
  (constraint nil)                      ; constraint causing the creation of this individual
  (blocked-p nil)                       ; indicates whether this individual is blocked
  )
 
(defmethod print-object ((label inverse-label-info) stream)
  (print-unreadable-object (label stream)
    (if (or (label-info-previous-ind label) (label-info-ind label))
      (progn
        (when (label-info-blocked-p label)
          (format stream "@"))
        (when (or (label-info-blocked-labels label)
                  (label-info-blocked-constraints label))
          (format stream "*"))
        (format stream "(~S ~S ~S:(~S,~S))"
                (label-info-previous-ind label)
                (label-info-role label)
                (label-info-ind label)
                (label-info-model-in-progress label)
                (label-info-back-propagated-concepts label)))
      (format stream "(~S:~S)"
              (label-info-role label)
              (or (label-info-label-or-concept label)
                  (label-info-model-in-progress label))))))

(race-inline (copy-label-info))

;;; Changed: rm
(defun copy-label-info (label-info)
  (if (inverse-label-info-p label-info)
    (copy-inverse-label-info label-info)
    (copy-basic-label-info label-info)))

(defun label-info-equal-test (label-info-1 label-info-2)
  (and (eql (label-info-ind label-info-1) (label-info-ind label-info-2))
       (eq (label-info-role label-info-1) (label-info-role label-info-2))
       (or (not (and (inverse-label-info-p label-info-1) (inverse-label-info-p label-info-2)))
           (eql (label-info-previous-ind label-info-1) (label-info-previous-ind label-info-2)))))

(defun safe-role-name (role)
  (when role
    (role-name role)))

(defun add-label-info-dependent-model (label-info model)
  (if (listp (label-info-dependent-models label-info))
      (let ((old-model (find model (label-info-dependent-models label-info)
                             :test #'label-info-equal-test)))
        (if old-model
            (setf (label-info-dependent-models label-info)
                  (cons model (delete old-model (label-info-dependent-models label-info))))
          (push model (label-info-dependent-models label-info)))
        (when (> (length (label-info-dependent-models label-info)) 20)
          (let ((table (racer-make-hash-table :test 'equal)))
            (loop for elem in (label-info-dependent-models label-info)
                  for key = (list* (label-info-ind elem)
                                   (safe-role-name (label-info-role elem))
                                   (when (inverse-label-info-p elem)
                                     (list (label-info-previous-ind elem))))
                  do (setf (gethash key table) elem))
            (setf (label-info-dependent-models label-info) table))))
    (let ((key (list* (label-info-ind model)
                      (safe-role-name (label-info-role model))
                      (when (inverse-label-info-p model)
                        (list (label-info-previous-ind model))))))
      (unless (gethash key (label-info-dependent-models label-info))
        (setf (gethash key (label-info-dependent-models label-info)) model)))))

(defun clean-label-info-dependent-models (label-info condition-fcn)
  (when (label-info-dependent-models label-info)
    (if (listp (label-info-dependent-models label-info))
      (setf (label-info-dependent-models label-info)
            (loop for elem in (label-info-dependent-models label-info)
                  unless (funcall condition-fcn elem)
                  collect elem))
      (loop with table = (label-info-dependent-models label-info)
            for elem being the hash-value of table do
            (when (funcall condition-fcn elem)
              (remhash elem table))))))

(defun get-label-info-dependent-models (label-info)
  (if (listp (label-info-dependent-models label-info))
    (label-info-dependent-models label-info)
    (loop for elem being the hash-value of (label-info-dependent-models label-info)
          collect elem)))
