;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: INSPECTOR; Base: 10 -*-

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

(in-package :inspector)

(defclass cache-inspector
  (usual-inspector)
  ((entries :accessor cache-entries :initform nil)
   (length :initform nil
           :accessor cache-length)))

(defmethod inspector-class ((object race::cache))
  (if *inspector-debug-mode*
    (call-next-method)
    'cache-inspector))


(defmethod compute-line-count ((i cache-inspector))
  (let ((object (inspector-object i)))
    (setf (cache-entries i)
          (racer::compute-cache-entries object))
    (+ 1 ; object
       1 ; type
       1 ; name
       1 ; length
       1 ; Comment
       (setf (cache-length i)
             (length (cache-entries i))))))

(defstruct subset-cache-inspector-entry
  key value)

(defmethod print-object ((object subset-cache-inspector-entry) stream)
  (format stream "~A --> ~A"
          (subset-cache-inspector-entry-key object)
          (subset-cache-inspector-entry-value object)))

(defmethod line-n ((i cache-inspector) n)
  (let ((object (inspector-object i)))
    (cond ((eql n 0)
           (values object "" :static))
          ((eql n 1)
           (values (type-of object) "Type: " :static))
          ((eql n 2)
           (values (racer::cache-name object) 
                   (format nil "Name: ")
                   :static))
          ((eql n 3)
           (values (cache-length i) "Number of entries: " :static))
          ((eql n 4)
           (if (null (cache-entries i))
             (values nil "No entries" :comment)
             (values nil "Entries:" :comment)))
          ((> n 4)
           (let ((entry (nth (- n 5) (cache-entries i))))
             (values (make-subset-cache-inspector-entry
                      :key (if (and (null (rest entry))
                                    (racer::cache-p (first entry)))
                             nil
                             (mapcar #'racer::cache-entry entry))
                      :value (cdr (racer::cache-null-marker (first (last entry)))))
                     (format nil "~A: " (- n 5))
                     :static))))))
