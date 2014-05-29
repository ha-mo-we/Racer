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

;;; The class is reused for other purposes as well, see reasoner.lisp
(defclass pooled-process ()
  ((process :accessor process :initarg :process)
   (pprocess-function :accessor pprocess-function :initform nil
                      :initarg :pprocess-function)))

#+:process-pooling
(progn

  (defmethod print-object ((process pooled-process) stream)
    (format stream "#<Pooled Process, ~A>~%" (process process)))

  (defmethod initialize-instance :after ((process pooled-process) &rest initargs)
    (declare (ignorable initargs))
    (incf *pool-counter*)
    (push process *all-processes*))

  (defun kill-all-processes ()
    (mapc #'(lambda (x)
              (kill-process (process x)))
          *all-processes*)
    (setf *pool-counter* 0
          *all-processes* nil
          *process-pool* nil))
  
  (defun make-pooled-process ()
    (let* ((pp (make-instance 'pooled-process))
           (*multiprocess-queries* t)
           (p (start-process
                (loop
                  #-:ccl
                  (progn
                        (process-wait (pprocess-function pp))
                        (funcall (pprocess-function pp))
                        (release-process pp))
                  #+:ccl
                  (restart-case 
                      (progn
                        (process-wait (pprocess-function pp))
                        (funcall (pprocess-function pp))
                        (release-process pp))
                    (abort () nil))))))
    
      (setf (process pp) p)
    
      pp))
   
  ;;;
  ;;;
  ;;;
              
  (defun acquire-process ()
    (labels ((try-to-acquire ()
               (when *process-pool*
                 (return-from acquire-process
                   (pop *process-pool*)))))

      (when (zerop *pool-counter*)
        (init-process-pool))

      (try-to-acquire)

      ;;; failed? 

      (cond ((=> *max-pool-size*
                 (< *pool-counter* *max-pool-size*))

             (push (make-pooled-process) *process-pool*)

             (acquire-process))

            (t 

             ;;; cannot create new process... 

             (cond ( (with-access-to-lifecycle-lists
                       (some #'proactive-tuple-computation-p *active-queries*))

                     ;;; wait for some other process to release pooled process... 
                   
                     (loop 
                    
                      ;;; termination granted! 
		     
                      ;;; (princ ".")
                    
                      (sleep 0.1)
                    
                      (try-to-acquire)))

                   (t 

                    (nrql-runtime-error 
                     "Max process pool size of ~a reached, please increase pool size" *max-pool-size*)))))))
          

  (defun release-process (pooled-process)
    (setf (pprocess-function pooled-process) nil)
    (push pooled-process *process-pool*))

  ;;;
  ;;;
  ;;;

  #+:lispworks 
  (defmethod abort-process ((pooled-process pooled-process))
    (unless (mp::process-dead-p (process pooled-process))
      (setf (pprocess-function pooled-process) nil) ; Added by RM: Doing this in release process is too late.
      (mp:process-reset (process pooled-process))
      (release-process pooled-process)))

  #+:allegro 
  (defmethod abort-process ((pooled-process pooled-process))
    (setf (pprocess-function pooled-process) nil) ; Added by RM: Doing this in release process is too late.
    (multiprocessing:process-reset (process pooled-process))
    (release-process pooled-process))

  #+:ccl
  (defmethod abort-process ((pooled-process pooled-process))
    ;; Need to make sure the process does not start over again with the previous task.
    ;; Thus we remove the pprocess function before resetting.
    (setf (pprocess-function pooled-process) nil)
    ;;(format t "~%Aborting ~S" pooled-process)
    (ccl:process-abort (process pooled-process))
    (push pooled-process *process-pool*))

  (defmethod abort-process ((process t))
    (kill-process process))

  ;;;
  ;;;
  ;;;

  (defmacro start-query-process (body)
    `(if (and *process-pooling*
              *multiprocess-queries*)
       
         (let ((process (acquire-process)))
           (when process
             (setf (pprocess-function process)
                   #'(lambda ()
                       ,body)))
         
           process)

       (start-process ,body)))

  ;;;
  ;;;
  ;;;

  (defun init-process-pool ()
    (kill-all-processes)
    (with-critical-section
      (setf *process-pool* 
            (loop as  i from 1 to *min-pool-size* 
                  collect (make-pooled-process)))))


  )

#-:process-pooling
(progn

  (defun init-process-pool () t)

  (defun kill-all-processes () t)

  (defmacro start-query-process (body)
    `(start-process ,body))

  (defmethod abort-process ((process t))
    (kill-process process)))




