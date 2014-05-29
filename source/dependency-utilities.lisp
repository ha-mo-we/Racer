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

;;;===========================================================================

#+:debug
(defun set-clash-dependencies (list)
  (setf *catching-clash-dependencies* list))

#-:debug
(defmacro set-clash-dependencies (list)
  `(setf *catching-clash-dependencies* ,list))

(defun union-dependencies (list1 list2)
  (if (or (null list1) (eq list1 list2))
      list2
    (if (null list2)
        list1
      (let ((l1 (length list1))
            (l2 (length list2)))
        (when (> l1 l2)
          (rotatef list1 list2)
          (rotatef l1 l2))
        (if (>= l2 1000)
            (append list1 list2)
          (constraint-set-union list1 list2))))))

#+:debug
(defun add-clash-dependencies (list)
  (set-clash-dependencies (union-dependencies list *catching-clash-dependencies*)))

#-:debug
(defmacro add-clash-dependencies (list)
  `(set-clash-dependencies (union-dependencies ,list *catching-clash-dependencies*)))

#+:debug
(defun remove-dependency (or-constraint)
  (set-clash-dependencies (racer-remove or-constraint *catching-clash-dependencies*)))

#-:debug
(defmacro remove-dependency (or-constraint)
  `(set-clash-dependencies (racer-remove ,or-constraint *catching-clash-dependencies*)))

#+:debug
(defun remove-dependencies (or-constraints)
  (set-clash-dependencies (constraint-set-difference *catching-clash-dependencies* or-constraints)))

#-:debug
(defmacro remove-dependencies (or-constraints)
  `(set-clash-dependencies (constraint-set-difference *catching-clash-dependencies* ,or-constraints)))

(defun union-dependency-list (list)
  (constraint-set-remove-duplicates
   (loop for elem in list
         append elem)))

(defun collect-dependencies (constraints)
  (loop for constraint in constraints
        when (constraint-common-p constraint)
        append (constraint-or-dependencies constraint) into result
        finally (return (constraint-set-remove-duplicates result))))

#+:debug
(defun backjumping-stopped-here (or-constraint)
  (member or-constraint *catching-clash-dependencies*))

#-:debug
(defmacro backjumping-stopped-here (or-constraint)
  `(member ,or-constraint *catching-clash-dependencies*))

