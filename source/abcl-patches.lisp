;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SYSTEM; Base: 10 -*-

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

(in-package :system)

(defun define-reader (slot)
 (let ((accessor-name (dsd-reader slot))
       (index (dsd-index slot))
       (type (dsd-type slot)))
   (cond ((eq *dd-type* 'list)
          `((declaim (ftype (function * ,type) ,accessor-name))
            (setf (symbol-function ',accessor-name)
                  (make-list-reader ,index))))
         ((or (eq *dd-type* 'vector)
              (and (consp *dd-type*) (eq (car *dd-type*) 'vector)))
          `((declaim (ftype (function * ,type) ,accessor-name))
            (setf (symbol-function ',accessor-name)
                  (make-vector-reader ,index))
            (define-source-transform ,accessor-name (instance)
              `(aref ,instance ,,index))))
         (t
          `((declaim (ftype (function * ,type) ,accessor-name))
            (setf (symbol-function ',accessor-name)
                  (make-structure-reader ,index ',*dd-name*))
            (define-source-transform ,accessor-name (instance)
              ,(if (eq type 't)
                   ``(structure-ref ,instance ,,index)
                   ``(the ,',type
                       (structure-ref ,instance ,,index)))))))))


(defun define-writer (slot)
 (let ((accessor-name (dsd-reader slot))
       (index (dsd-index slot)))
   (cond ((eq *dd-type* 'list)
          `((setf (get ',accessor-name 'setf-function)
                  (make-list-writer ,index))))
         ((or (eq *dd-type* 'vector)
              (and (consp *dd-type*) (eq (car *dd-type*) 'vector)))
          `((setf (get ',accessor-name 'setf-function)
                  (make-vector-writer ,index))
            (define-source-transform (setf ,accessor-name) (value instance)
              `(aset ,instance ,,index ,value))))
         (t
          `((setf (get ',accessor-name 'setf-function)
                  (make-structure-writer ,index ',*dd-name*))
            (define-source-transform (setf ,accessor-name) (value instance)
              `(structure-set ,instance ,,index ,value)))))))
