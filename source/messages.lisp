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

(in-package :thematic-substrate)

;;;
;;;
;;;

(defun ensure-string (string) 
  (if (stringp string)
      string
    (if (symbolp string)
        (symbol-name string)
      (format nil "~A" string))))

(defun nrql-warning (string &rest args)
  (when (or *really-warn-p*
            (and *warnings-p*
                 (or *tbox-verbose*
                     *abox-verbose*)))
    #|
    (apply #'format t 
           (concatenate 'string "~%nRQL Warning: " 
			(ensure-string string)
			".")
           args) |# 

    (apply #'racer:racer-warn
           (concatenate 'string 
                        (ensure-string string)
                        " (nRQL Warning).")
           args)))

(defun nrql-error (string &rest args)
  (apply #'error 
         (concatenate 'string
		      (ensure-string string)
		      " (nRQL Error).")
         args))

;;;
;;;
;;; 

(defun nrql-runtime-error (string &rest args)
  (apply #'nrql-error 
         (concatenate 'string "Runtime Error: " 
		      (ensure-string string))
         args))

(defun parser-error (string &rest args)
  (apply #'nrql-error 
         (concatenate 'string "Parser Error: " 
		      (ensure-string string))
         args))

(defun owlapi-parser-error (string &rest args)
  (apply #'nrql-error
         (concatenate 'string "OWLAPI Parser Error: " 
		      (ensure-string string))
         args))

(defun owlapi-runtime-error (string &rest args)
  (apply #'nrql-error 
         (concatenate 'string "OWLAPI Runtime Error: " 
		      (ensure-string string))
         args))

(defun minilisp-error (string &rest args)
  (apply #'nrql-error
         (concatenate 'string "MiniLisp Error: " 
		      (ensure-string string))
         args))

(defun owlapi-warning (string &rest args)
  (apply #'nrql-warning
         (concatenate 'string "OWLAPI Warning: " 
		      (ensure-string string))
         args))

;;;
;;;
;;;

(defun warn-inconsistent (query) 
  (nrql-warning "~A is inconsistent" (iterator-id query)))

(defun warn-tautological (query) 
  (nrql-warning "~A is tautological" (iterator-id query)))

(defun warn-tbox-has-changed ()
  (nrql-warning "TBox has changed - re-preparing and executing"))

;;;
;;;
;;;

(defun query-deadlock-warning (queries)
  (let ((*really-warn-p* t))
    (nrql-warning "Denied due to deadlock prevention! 
    The following queries will not terminate automatically, 
    since they have been started in lazy incremental mode:
    ~A."
                  (mapcar #'iterator-id queries))))

(defun rule-deadlock-warning (rules)
  (let ((*really-warn-p* t))  
    (nrql-warning "Denied due to deadlock prevention! 
    The following rules will not terminate automatically, 
    since they have been started in lazy incremental mode:
    ~A."
                  (mapcar #'iterator-id rules))))

