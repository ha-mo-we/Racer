;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWLAPI; Base: 10 -*-

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

(in-package :owlapi)

(defvar *process-counter* 0)

(defvar *reasoner*)

(defstruct racer-reasoner 
  name
  environment
  queue
  queue-lock
  request-counter
  processing-request
  processed-requests
  processed-requests-list

  progress-state)

(defmethod print-object ((object racer-reasoner) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (princ (racer-reasoner-name object) stream)))

(defun enqueue (racer-reasoner fn)
  (ts::with-process-lock ((racer-reasoner-queue-lock racer-reasoner))
    (setf (racer-reasoner-queue racer-reasoner) (nconc (racer-reasoner-queue racer-reasoner) (list fn)))))
  
(defun dequeue (racer-reasoner)
  (ts::process-wait (racer-reasoner-queue racer-reasoner))
  (ts::with-process-lock ((racer-reasoner-queue-lock racer-reasoner))
    (pop (racer-reasoner-queue racer-reasoner))))

(defun flush-queue (racer-reasoner)
  (ts::with-process-lock ((racer-reasoner-queue-lock racer-reasoner))
    (setf (racer-reasoner-queue racer-reasoner) nil)))

(defun release-reasoner (reasoner)
  (abort-current-request reasoner))

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (defun find-racer-parameters ()
    (let ((result nil))
      (loop for package in '(:racer :nrql-symbols :ts :owl-syntaxes :owlapi)
	  do
	    (do-symbols (sym (find-package package))
	      (when (and (eq (symbol-package sym) (find-package package))
		         (char= (aref (symbol-name sym) 0) #\*)
			 (boundp sym)
			 ;;(or (typep (symbol-value sym) 'standard-object)
			 ;;    (typep (symbol-value sym) 'structure-object))

                         ;; OWLAPI Reasoner Registry muss reasoneruebergreifend
                         ;; bereitgestellt werden!
                         (not (member sym 
                                      '(*reasoners* 
					*reasoners-lock*
                                        *cur-reasoner* 
                                        *default-reasoner* 
                                        *default-reasoner-name*))))
		(push sym result))))
      (remove-duplicates (append result 
				 #+:ignore 
				 (mapcar #'car excl::*default-lisp-listener-bindings*)
                                 #+:allegro
				 (mapcar #'car excl::*cl-default-special-bindings*))))))
				 

(defmacro make-racer-process (reasoner)
  `(let* ((pp (make-instance 'ts::pooled-process)) 
	  ;; diese Prozesse sind NICHT wirklich im Pool! 
	  ;; hier wird nur die existierende Klasse "reused"... aber ohne Pool-Logik verwendet
	  (p (ts::start-process
	      (let ((*reasoner* ,reasoner))
		(let ,(mapcar (lambda (x) (list x x)) (find-racer-parameters))
		  #| (let ((*reasoners*  *reasoners*)
			(*reasoners-lock* *reasoners-lock*)
			(*cur-reasoner* *cur-reasoner*)
			(*default-reasoner* *default-reasoner*)
			(*default-reasoner-name* *default-reasoner-name*)) |# 
		  (with-progress-state (racer-reasoner-progress-state *reasoner*)
		    (loop
		      (let* ((id-and-request (dequeue *reasoner*))
			     (id (first id-and-request))
			     (request (second id-and-request)))
			(setf (racer-reasoner-processing-request *reasoner*) id)
			(setf (ts::pprocess-function pp) request)
			(handler-case (progn
					(let* ((ts::*multiprocess-queries* nil)
					       (res (funcall (ts::pprocess-function pp))))
					  (setf (gethash id (racer-reasoner-processed-requests *reasoner*))
					    res)
					  (push id (racer-reasoner-processed-requests-list *reasoner*))))
			  (error (c)
			    (format t "~%@@@@@@@@@@@@@@@@@@ ERROR ~A" c)
			    (setf (gethash id (racer-reasoner-processed-requests *reasoner*))
			      c)
			    (setf (ts::pprocess-function pp) nil)))))))))))
     (setf (ts::process pp) p)
     pp))

(defun racer-perform (reasoner fn &optional wait-p)
  (let ((id (incf (racer-reasoner-request-counter reasoner))))
    (enqueue reasoner (list id fn))
    (when wait-p 
      ;; Hashtable lock here! -> requests-list
      (ts::process-wait (member id (racer-reasoner-processed-requests-list reasoner)))
      (prog1
          (gethash id (racer-reasoner-processed-requests reasoner))
        (setf (racer-reasoner-processed-requests-list reasoner)
              (delete id (racer-reasoner-processed-requests-list reasoner)))
        (remhash id (racer-reasoner-processed-requests reasoner))))))

(defun create-new-racer (&optional (name (gensym (symbol-name 'racer))))
  (let* ((reasoner (make-racer-reasoner 
                   :request-counter 0
                   :processed-requests (make-hash-table)
                   :processed-requests-list nil
                   :processing-request nil
		   :name name
		   :queue-lock 
                   (ts::make-lock)))
	(progress-state (make-progress-state
			 :owlapi-reasoner reasoner)))
    (setf (racer-reasoner-progress-state reasoner) progress-state)
    (setf (racer-reasoner-environment reasoner)
      #+:abcl (error "Cannot make racer process.")
      #-:abcl (make-racer-process reasoner))
    reasoner))

(defun abort-current-request (racer-reasoner)
  (when (racer-reasoner-environment racer-reasoner)
    (when (racer-reasoner-processing-request racer-reasoner)
      (setf (gethash 
             (racer-reasoner-processing-request racer-reasoner)
             (racer-reasoner-processed-requests racer-reasoner))
            :aborted)
      (push (racer-reasoner-processing-request racer-reasoner)
            (racer-reasoner-processed-requests-list racer-reasoner)))
    (ts::kill-process (ts::process (racer-reasoner-environment racer-reasoner)))
    (flush-queue racer-reasoner)
    (setf (racer-reasoner-environment racer-reasoner)
          #+:abcl (error "Cannot make racer process.")
          #-:abcl (make-racer-process racer-reasoner))))

(defmacro using-reasoner (reasoner wait-p &rest forms)
  `(racer-perform ,reasoner (lambda () .,forms) ,wait-p))

