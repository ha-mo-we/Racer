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

;;;
;;;
;;;

(define-condition continue-from-timeout () ())

(define-condition abort-execution () ())

;;;
;;;
;;;

(defmacro with-timeout ((seconds . timeout-forms) &body body)
  (let ((seconds1 (gensym)))
    #+(or :allegro :ccl :lispworks)
    `(let ((,seconds1 ,seconds))
       (if ,seconds1
	   (with-timeout-1 ,seconds1 #'(lambda () ,@timeout-forms) #'(lambda () ,@body))
	 (progn 
	   ,@body)))
    #+:sbcl
    `(let ((,seconds1 ,seconds))
       (if (gethash sb-thread:*current-thread* *processes-without-timeout*)
	   (progn .,body)
	 (handler-case 
	  (sb-ext:with-timeout ,seconds1
			       .,body)
	  (sb-ext:timeout () .,timeout-forms))))
    #+:abcl
    (let ((tag-sym (gensym))
	  (timed-thread-sym (gensym))
	  (timer-thread-sym (gensym)))
      ;; No timers in ABCL, so we use SLEEP in a separate "timer" thread:
      `(let ((,seconds1 ,seconds))
	 (if ,seconds1
	     (catch ',tag-sym
	       (let* ((,timed-thread-sym (threads:current-thread))
		      (,timer-thread-sym
		       (threads:make-thread
			#'(lambda ()
			    (sleep ,seconds1)
			    (threads:interrupt-thread
			     ,timed-thread-sym
			     #'(lambda ()
				 (ignore-errors
				   (throw ',tag-sym
				     (progn ,@timeout-forms))))))
			:name "WITH-TIMEOUT timer")))
		 (sleep 0)
		 (unwind-protect (progn ,@body)
		   (when (threads:thread-alive-p ,timer-thread-sym)
		     (threads:destroy-thread ,timer-thread-sym)
		     (sleep 0)))))
	     (progn 
	       ,@body))))
    #-(or :allegro :ccl :lispworks :sbcl :abcl)
    (declare (ignore seconds timeout-forms))
    #-(or :allegro :ccl :lispworks :sbcl :abcl)
    `(progn ,@body)))

(defmacro with-non-interruptable-timeout ((seconds . timeout-forms) &body body)
  (let ((seconds1 (gensym)))
    (declare (ignorable seconds1))
    
    #+(or :ccl :lispworks)
    `(let ((,seconds1 ,seconds))
       (if ,seconds1
	   (with-timeout-2 ,seconds1 #'(lambda () ,@timeout-forms) #'(lambda () ,@body))
	 (progn 
	   ,@body)))
    
    #+:allegro
    `(let ((,seconds1 ,seconds))
       (if ,seconds1
	   (sys:with-timeout (,seconds1 ,@timeout-forms) ,@body)
	 (progn 
	   ,@body)))

    #+:sbcl
    `(let ((,seconds1 ,seconds))
       (if ,seconds1
	   (handler-case 
	       (sb-ext:with-timeout ,seconds1
		 .,body)
	     (sb-ext:timeout () .,timeout-forms))
	 (progn 
	   ,@body)))

    #-(or :allegro :ccl :lispworks :sbcl :abcl)
    (declare (ignore seconds timeout-forms))
    
    #-(or :allegro :ccl :lispworks :sbcl :abcl)
    `(progn ,@body)))

;;;
;;;
;;;

(defmacro with-timeout-cleanup (form &rest cleanup-forms)
  `(restart-case
       ,form
     (abort-execution ()
                      ,@cleanup-forms
                      (when (find-restart 'continue-from-timeout)
                        (invoke-restart 'continue-from-timeout)))))


;;;
;;;
;;;

(defparameter *processes-without-timeout* (make-hash-table))

#+:ccl
(defmacro without-timeout (&body forms)
  `(unwind-protect
       (progn (let ((value (gethash ccl:*current-process* *processes-without-timeout*)))
		(if (null value)
		    (setf (gethash ccl:*current-process* *processes-without-timeout*) 1)
		  (setf (gethash ccl:*current-process* *processes-without-timeout*) (1+ value))))
         .,forms)
     (let ((value (gethash ccl:*current-process* *processes-without-timeout*)))
       (if (= value 1)
	   (remhash ccl:*current-process* *processes-without-timeout*)
         (setf (gethash ccl:*current-process* *processes-without-timeout*) (1- value))))))


#+:lispworks
(defmacro without-timeout (&body forms)
  `(unwind-protect
       (progn (let ((value (gethash mp:*current-process* *processes-without-timeout*)))
		(if (null value)
		    (setf (gethash mp:*current-process* *processes-without-timeout*) 1)
		  (setf (gethash mp:*current-process* *processes-without-timeout*) (1+ value))))
         .,forms)
     (let ((value (gethash mp:*current-process* *processes-without-timeout*)))
       (if (= value 1)
	   (remhash mp:*current-process* *processes-without-timeout*)
         (setf (gethash mp:*current-process* *processes-without-timeout*) (1- value))))))

#+:allegro
(defmacro without-timeout (&body forms)
  `(unwind-protect
       (progn (let ((value (gethash mp:*current-process* *processes-without-timeout*)))
		(if (null value)
		    (setf (gethash mp:*current-process* *processes-without-timeout*) 1)
		  (setf (gethash mp:*current-process* *processes-without-timeout*) (1+ value))))
         .,forms)
     (let ((value (gethash mp:*current-process* *processes-without-timeout*)))
       (if (= value 1)
	   (remhash mp:*current-process* *processes-without-timeout*)
         (setf (gethash mp:*current-process* *processes-without-timeout*) (1- value))))))

#+:sbcl
(defmacro without-timeout (&body forms)
  `(unwind-protect
       (progn (let ((value (gethash sb-thread:*current-thread* *processes-without-timeout*)))
		(if (null value)
		    (setf (gethash sb-thread:*current-thread* *processes-without-timeout*) 1)
		  (setf (gethash sb-thread:*current-thread* *processes-without-timeout*) (1+ value))))
         .,forms)
     (let ((value (gethash sb-thread:*current-thread* *processes-without-timeout*)))
       (if (= value 1)
	   (remhash sb-thread:*current-thread* *processes-without-timeout*)
         (setf (gethash sb-thread:*current-thread* *processes-without-timeout*) (1- value))))))

#-(or :ccl :lispworks :allegro :sbcl)
(defmacro without-timeout (&body forms)
  `(progn ,@forms))

;;;
;;;
;;;

#+:ccl
(defun with-timeout-1 (seconds timeout-fn body-fn)
  (let* ((tag 'tag-1)
         (started-p nil)
         (finished-p nil)
         (current-process ccl:*current-process*)
         ;;(timeout-id (gensym))
         (timeout-process 
          (when seconds
            (ccl:process-run-function
                (format nil "timeout ~d" (round seconds))
              #'(lambda ()
                  (ccl:process-wait "waiting" (lambda () started-p))
                  (sleep (round seconds))
                  (ccl:without-interrupts
                   (unless (or finished-p (gethash current-process *processes-without-timeout*))
                     (ccl:process-interrupt current-process
                                            #'(lambda ()
                                                (when (find-restart 'abort-execution)
                                                  (invoke-restart 'abort-execution)))))))))))
    (catch tag
      (unwind-protect
          (restart-case
              (let ((res 
		     (multiple-value-call #'list
		       (progn 
			 (setf started-p t)
			 (funcall body-fn)))))
                (setf finished-p t)
		(apply #'values res))
            (continue-from-timeout ()
                                   (setf finished-p t)   
                                   (throw tag
                                          (funcall timeout-fn)))
            
            (abort-execution ()
                             (setf finished-p t)
                             (throw tag
                                       (funcall timeout-fn))))
        
	(when timeout-process
          (setf finished-p t)
	  (when timeout-process
	    (ccl:process-kill timeout-process)))))))


#+:allegro
(defun with-timeout-1 (seconds timeout-fn body-fn)
  (let* ((tag 'tag-1)
	 (started-p nil)
         (current-process system:*current-process*)
         (timeout-process 
	  (when seconds
	    (mp:process-run-function
		(format nil "timeout ~d" seconds)
	      #'(lambda ()
		  (multiprocessing:process-wait "waiting"
						(lambda ()
						  started-p))
		  (sleep seconds)
		  (unless (gethash current-process *processes-without-timeout*)
		    (mp:process-interrupt current-process
					  #'(lambda ()
					      (if (find-restart 'abort-execution)
						  (invoke-restart 'abort-execution)
						(break "Can't find restart abort-execution."))))))))))
    (catch tag
      (unwind-protect
          (restart-case 
	      (let ((res 
		     (multiple-value-call #'list
		       (progn 
			 (setf started-p t)
			 (funcall body-fn)))))
		(apply #'values res))
            (continue-from-timeout () 
	      (throw tag
		(funcall timeout-fn)))
            
            (abort-execution ()  
	      (throw tag
		(funcall timeout-fn))))
        
	(when timeout-process
	  (mp:process-kill timeout-process))))))

#+:lispworks
(defun with-timeout-1 (timeout timeout-function body-fn)
  (declare (dynamic-extent body-fn))
  (let ((tag 'tag-1)
        (process mp:*current-process*))
    
    (labels ((timeout-throw ()
               (when (find-restart 'abort-execution)
                 (invoke-restart 'abort-execution)))

             (timeout-action ()
               (unless (gethash process *processes-without-timeout*)   
                 (mp:process-interrupt process #'timeout-throw))))
      
      (declare (dynamic-extent #'timeout-throw #'timeout-action))

      (let ((timer (mp:make-named-timer 'timer #'timeout-action)))

        (catch tag
          (unwind-protect
              (restart-case 
                  (progn
                    (when timeout
                      (mp:schedule-timer-relative timer timeout))
                    (return-from with-timeout-1
                      (funcall body-fn)))

                (continue-from-timeout () 
                                       (throw tag 
                                              (when timeout-function
                                                (funcall timeout-function))))

                (abort-execution () 
                                 (throw tag
                                        (when timeout-function
                                          (funcall timeout-function)))))
            
            (mp:unschedule-timer timer)))))))

;;;
;;;
;;;

#+:ccl
(defun with-timeout-2 (seconds timeout-fn body-fn) 
  (with-timeout-1 seconds timeout-fn body-fn))

#|
(defun with-timeout-2 (seconds timeout-fn body-fn)
  (let* ((tag 'tag-1)
         (current-process ccl:*current-process*)
         (timeout-process 
          (ccl:process-run-function
              (format nil "timeout ~d" (round seconds))
            #'(lambda ()
                (sleep (round seconds))
                (ccl:process-interrupt current-process
                                       #'(lambda ()
                                           (when (find-restart 'abort-execution)
                                             (invoke-restart 'abort-execution))))))))
    (catch tag
      (unwind-protect
          (restart-case (funcall body-fn)
            (continue-from-timeout () 
                                   (throw tag
                                          (funcall timeout-fn)))
            
            (abort-execution () (throw tag
                                       (funcall timeout-fn))))
        
        (ccl:process-kill timeout-process)))))
|#

#+:lispworks
(defun with-timeout-2 (timeout timeout-function body-fn)
  (declare (dynamic-extent body-fn))
  (let ((tag 'tag-1)
        (process mp:*current-process*))
    
    (labels ((timeout-throw ()
               (when (find-restart 'abort-execution)
                 (invoke-restart 'abort-execution)))

             (timeout-action ()
	       (mp:process-interrupt process #'timeout-throw)))
      
      (declare (dynamic-extent #'timeout-throw #'timeout-action))

      (let ((timer (mp:make-named-timer 'timer #'timeout-action)))

        (catch tag
          (unwind-protect
              (restart-case 
                  (progn
                    (when timeout
                      (mp:schedule-timer-relative timer timeout))
                    (return-from with-timeout-2
                      (funcall body-fn)))

                (continue-from-timeout () 
                                       (throw tag 
                                              (when timeout-function
                                                (funcall timeout-function))))

                (abort-execution () 
                                 (throw tag
                                        (when timeout-function
                                          (funcall timeout-function)))))
            
            (mp:unschedule-timer timer)))))))



;;;
;;;
;;;

#+:allegro
(let ((elapsed-gc-time-user 0)
      (elapsed-gc-time-system 0)
      (elapsed-run-time-user 0)
      (elapsed-run-time-system 0)
      (elapsed-real-time 0)
      (used-cons-cells 0)
      (used-symbols 0)
      (used-other-bytes 0))
  
  (declare (ignorable elapsed-real-time elapsed-run-time-system elapsed-run-time-user))
  
  (defun set-time-vars (pelapsed-gc-time-user
                        pelapsed-gc-time-system
                        pelapsed-run-time-user
                        pelapsed-run-time-system
                        pelapsed-real-time
                        pused-cons-cells
                        pused-symbols
                        pused-other-bytes)
    (setf elapsed-gc-time-user pelapsed-gc-time-user)
    (setf elapsed-gc-time-system pelapsed-gc-time-system)
    (setf elapsed-run-time-user pelapsed-run-time-user)
    (setf elapsed-run-time-system pelapsed-run-time-system)
    (setf elapsed-real-time pelapsed-real-time)
    (setf used-cons-cells pused-cons-cells)
    (setf used-symbols pused-symbols)
    (setf used-other-bytes pused-other-bytes))

  (defun gctime ()
    (+ elapsed-gc-time-user elapsed-gc-time-system))
  
  (defun total-bytes-allocated ()
    (+ (* 8 used-cons-cells) (* 64 used-symbols) used-other-bytes)))

#+:ccl
(defun total-bytes-allocated ()
  (ccl::total-bytes-allocated))

#+:ignore
(let (#+:ccl initial-gc-time
             #+:ccl initial-consed
             #+(or :ccl :allegro) initial-real-time
             #+(or :ccl :allegro) initial-run-time)
  
  #+:ccl
  (defun with-timed-form (thunk)
    (setf initial-gc-time (ccl:gctime))
    (setf initial-consed (total-bytes-allocated))
    (setf initial-real-time (get-internal-real-time))
    (setf initial-run-time (get-internal-run-time))
    (let* ((result (funcall thunk))
           (elapsed-real-time (/ (- (get-internal-real-time) initial-real-time)
                                 internal-time-units-per-second))
           (elapsed-run-time (/ (- (get-internal-run-time) initial-run-time)
                                internal-time-units-per-second))
           (elapsed-mf-time (max 0 (- elapsed-real-time elapsed-run-time)))
           (elapsed-gc-time (/ (- (ccl:gctime) initial-gc-time)
                               internal-time-units-per-second))
           (bytes-consed (- (total-bytes-allocated) initial-consed
                            (if (ccl:fixnump initial-consed) 0 16))))
      (values result elapsed-run-time elapsed-real-time elapsed-mf-time
              elapsed-gc-time bytes-consed)))
  
  #+:allegro
  (defun with-timed-form (thunk)
    (setf initial-real-time (get-internal-real-time))
    (setf initial-run-time (get-internal-run-time))
    (let* ((result (excl::time-a-funcall thunk #'set-time-vars))
           (elapsed-real-time (/ (- (get-internal-real-time) initial-real-time)
                                 internal-time-units-per-second))
           (elapsed-run-time (/ (- (get-internal-run-time) initial-run-time)
                                internal-time-units-per-second))
           (elapsed-mf-time (max 0 (- elapsed-real-time elapsed-run-time)))
           (elapsed-gc-time (/ (gctime) internal-time-units-per-second))
           (bytes-consed (total-bytes-allocated)))
      (values result elapsed-run-time elapsed-real-time elapsed-mf-time
              elapsed-gc-time (abs bytes-consed))))
  
  #+:ccl
  (defun timed-out-handler ()
    (let* ((elapsed-real-time (/ (- (get-internal-real-time) initial-real-time)
                                 internal-time-units-per-second))
           (elapsed-run-time (/ (- (get-internal-run-time) initial-run-time)
                                internal-time-units-per-second))
           (elapsed-mf-time (max 0 (- elapsed-real-time elapsed-run-time)))
           (elapsed-gc-time (/ (- (ccl:gctime) initial-gc-time)
                               internal-time-units-per-second))
           (bytes-consed (- (total-bytes-allocated) initial-consed
                            (if (ccl:fixnump initial-consed) 0 16))))
      (values '(?) elapsed-run-time elapsed-real-time elapsed-mf-time
              elapsed-gc-time bytes-consed)))
  
  #+:allegro
  (defun timed-out-handler ()
    (let* ((elapsed-real-time (/ (- (get-internal-real-time) initial-real-time)
                                 internal-time-units-per-second))
           (elapsed-run-time (/ (- (get-internal-run-time) initial-run-time)
                                internal-time-units-per-second))
           (elapsed-mf-time (max 0 (- elapsed-real-time elapsed-run-time)))
           (elapsed-gc-time (/ (gctime) internal-time-units-per-second))
           (bytes-consed (total-bytes-allocated)))
      (values '(?) elapsed-run-time elapsed-real-time elapsed-mf-time
              elapsed-gc-time (abs bytes-consed)))))



