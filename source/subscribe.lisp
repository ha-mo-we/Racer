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

(defvar *changes-for-aboxes-pending* nil)

(declaim (special *one-simple-output*))


;;; (defvar *pending-notifications* nil)

(nrql-defun (check-nrql-subscriptions) (&optional (abox (current-abox)))

  (when *check-subscriptions-inhibited* 
    (pushnew abox *changes-for-aboxes-pending*))

  (unless *check-subscriptions-inhibited* 

    (let ((substrate 
           (get-substrate-for-abox abox))
          (processed t)
          (result nil))

      (when (or (not substrate)
                (substrate-needs-reset-p substrate))
      
        (dolist (queries  
                 (with-access-to-lifecycle-lists
                   (list 
                    *processed-queries*
                    (copy-list *ready-queries*))))

          (dolist (q queries)

            (when (and (eq (substrate q) substrate)
                       (subscribers q))

              (let* ((added 
                      (if processed
                          (reexecute-query q :only-new-tuples-p t)
                        (execute-query q :only-new-tuples-p t)))

                     (removed 
                      (removed-tuples q)))

                (dolist (res (list added removed))

                  (when res
                  
                    (dolist (subscriber (subscribers q))
                    
                      (incf (fifth subscriber))

                      (destructuring-bind (subscriber-name ip port use-simplified-protocol-p counter)
                          subscriber

                        (let* ((type (if (eq res added) :add :remove))
                               (message 
                                (if (and ip port)
                                    (list subscriber-name type res :for (iterator-id q) :ip ip :port port)
                                  (list subscriber-name type res :for (iterator-id q)))))

                          (push message result)
                        
                          (when (and ip port)
                            (let ((*one-simple-output* use-simplified-protocol-p))
                              (send-to-subscriber 
                               message
                               ip port counter))))))))

                ;;; (pprint result)
                )))                  

          (setf processed nil)))

      result)))

;;;
;;;
;;;

(nrql-defmethod (subscribe-to) ((query null) subscriber-name &key ip port use-simplified-protocol-p)
  (declare (ignorable ip port use-simplified-protocol-p subscriber-name))
  :not-found)

(nrql-defmethod (subscribe-to) ((query symbol) subscriber-name &rest args &key &allow-other-keys)
  (apply #'subscribe-to (find-query query) subscriber-name args))

(nrql-defmethod (subscribe-to) ((query query) subscriber-name &rest args 
                                &key ip port use-simplified-protocol-p)
                (declare (ignorable args))
   (with-slots (subscribers) query
     (setf subscribers
       (delete-if (lambda (x) 
		    (equal (subseq x 0 3)
			   (list subscriber-name ip port)))
		  subscribers))
     (push (list subscriber-name ip port use-simplified-protocol-p 0)
	   subscribers)))

;;;
;;;
;;;

(nrql-defmethod (unsubscribe-from) ((query null) subscriber-name &key ip port use-simplified-protocol-p)
  (declare (ignorable ip port use-simplified-protocol-p subscriber-name))
  :not-found)

(nrql-defmethod (unsubscribe-from) ((query symbol) subscriber-name &rest args &key &allow-other-keys)
  (apply #'unsubscribe-from (find-query query) subscriber-name args))

(nrql-defmethod (unsubscribe-from) ((query query) subscriber-name &rest args 
                                  &key ip port use-simplified-protocol-p)
  (declare (ignorable use-simplified-protocol-p args))
  (with-slots (subscribers) query
    (setf subscribers
          (delete-if (lambda (x) 
                       (equal (subseq x 0 3)
                              (list subscriber-name ip port)))
                     subscribers))))

;;;
;;;
;;;

(nrql-defmethod (query-subscribers) ((query null))
  :not-found)

(nrql-defmethod (query-subscribers) ((query symbol))
  (query-subscribers (find-query query)))

(nrql-defmethod (query-subscribers) ((query query))
  (with-slots (subscribers) query
    subscribers))
