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

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)

(defparameter *product-version* "2-0")

(defparameter *build-version* "2014-01-04") ;; YEAR MONTH DAY !!!!

(defparameter *build-id* 60) ; for OWLlink2, increase in case changes were made to OWLlink.

(defparameter *is-preview-version* nil)

(defparameter *is-beta-version* nil)

(defparameter *product-name*
  #+:racermaster 
  "RacerMaster"
  #+:racer-with-sirius
  "RacerPlus"
  #-(or :racer-with-sirius :racermaster)
  "Racer")

)

;;;
;;;
;;;

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (when *is-preview-version* 
    (pushnew :preview-version *features*))
  (when *is-beta-version* 
    (pushnew :beta-version *features*)))

;;;
;;;
;;;

(defun beta-version-p ()
  *is-beta-version*)

(defun preview-version-p ()
  *is-preview-version*)

(defun get-product-version ()
  (substitute #\. #\- *product-version*))

(defun get-build-version ()
  *build-version*)

(defun get-build-id ()
  *build-id*)

(defun get-product-name ()
  *product-name*)

(defun set-product-name (product-name)
  (setf *product-name* product-name))

;;;
;;;
;;;

(defparameter *porter-name* 
  #+:gtk
  "RacerPorter-GTK"
  #-:gtk
  "RacerPorter")

(defparameter *porter-version*  *product-version*)

;;;
;;;
;;;

#+:allegro
(defparameter +file-version-info+
  `(:company-name
    "Freeware Racer" 
    :file-description "Racer, a DL and OWL Reasoner"
    :product-name 
    #+:64bit
    ,(format nil "~A 64bit" *product-name*)

    #-:64bit
    ,(format nil "~A 32bit" *product-name*)
    :legal-copyright 
    "Use on your on risk, no warranties."
    :file-version 
    ,(substitute #\. #\- 
                 (format nil 
                         (if *is-preview-version*                                  
                             "Preview Version ~A / Build ~A" 
                           "Version ~A / Build ~A")
                         *product-version* 
                         *build-version*))
    :product-version 
    ,(substitute #\. #\- 
                 (format nil
                         (if *is-preview-version* 
                             "Preview Version ~A / Build ~A" 
                           "Version ~A / Build ~A")
                         *product-version*
                         *build-version*))))

