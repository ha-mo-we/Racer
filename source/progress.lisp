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

(in-package racer)

(declaim (special *multiprocessing-server*))

(defvar *progress-state-counter* 0)

(defvar *abort-debugging* nil)

(defmethod owlapi::owlapi-reasoner-name ((x null))
  nil)

(defstruct progress-state 
  (owlapi-reasoner nil)
  (id (incf *progress-state-counter*))
  (progress-value 0)
  (progress-value1 0)
  (progress-100% nil) ; can be nil! then, not explicitly set! defaults to 100 then!
  (progress-certain t)
  (from-% 0)
  (to-% 100)
  (in-range nil)
  (abort-requested nil)
  (current-request nil)
  (request-processed nil)
  (request-was-aborted nil)
  
  ;; the OWLAPI uses the process slot on the OWLAPI reasoner!
  ;; but for ordinary abort... 
  (process-to-kill nil))

(defstruct (owlapi-progress-state (:include progress-state)))

(defvar *progress-value* 0)
(defvar *progress-value1* 0)
(defvar *progress-100%* nil) ; can be nil! see above!
(defvar *progress-certain* t)
(defvar *ignore-progress* nil)

(defvar *from-%* 0)
(defvar *to-%* 100)
(defvar *in-range* nil)

(defvar *progress-state* (make-progress-state))
(defvar *last-changed-progress-state* nil)
(defvar *process-progress-state* nil)

(defun substring (string n)
  (let* ((l (length string))
	 (m (min l n))
	 (string
	  (delete #\return
		  (delete #\newline
			  (subseq string 0 m)))))
    (if (= m l)
	string
      (concatenate 'string string "..."))))
;;;
;;;
;;;

(defmacro with-progress-slots (&body body)
  `(with-slots (process-to-kill
		progress-value
		progress-value1
		progress-100% 
		progress-certain
		from-% 
		to-% 
		in-range
		abort-requested
		request-was-aborted
		current-request) *progress-state*
     (declare (ignorable progress-value
			 progress-value1
			 progress-100% 
			 progress-certain
			 from-% 
			 to-% 
			 in-range
			 abort-requested
			 request-was-aborted
			 process-to-kill
			 current-request))
     ,@body))

(defmacro with-progress-state (state &body body)
  `(let ((*progress-state* ,state))
     (when (progress-state-p *progress-state*)
       (with-progress-slots 
	   (let ((*progress-value* progress-value)
		 (*progress-value1* progress-value1)
		 (*progress-100%* progress-100%)
		 (*progress-certain* progress-certain)
		 (*from-%* from-%)
		 (*to-%* to-%)
		 (*in-range* in-range))	     
	     ,@body)))))

(defun update-progress-state ()
  (unless *ignore-progress*
    (with-progress-slots
	(setf progress-value *progress-value*
	  progress-value1 *progress-value1*
	  progress-100% *progress-100%*
	  progress-certain *progress-certain*
	  from-% *from-%*
	  to-% *to-%*
	  in-range *in-range*
	  ;; abort-requested *abort-requested*
	  ;; request-was-aborted *request-was-aborted*
	  ;; abort-requested *abort-requested*
	  ;; don't set abort-requested here, 
	  ;; as the request comes over a different
	  ;; controll port and process, and hence, 
	  ;; the next update in the reasoning process
	  ;; would re-set the flag to nil! 
	  ;; current-request *current-request*
	  )
      t)))

(defun new-request (expr ;&optional Removed RM Apr. 2014
                         )
  (when (and (not *ignore-progress*)
	     
	     ;; this is required, because otherwise the OWLAPI
	     ;; new-request in with-owlapi-reasoner registers
	     ;; more than one new-request for some reason
	     ;; (or (not *progress-state*)
	     ;; (not (progress-state-current-request *progress-state*)))
	     
	     ;; no, it is not... 
	     )
    
    (clear-request (> (get-progress-value) 99))
    (clear-abort-request)
    
    (setf (progress-state-current-request *progress-state*)
      (substring
       (with-output-to-string (stream)
	 (if (stringp expr)
	     (write-string
	      expr
	      stream)
	   (write expr :stream stream)))
       80))
    
    ;; this is Racer global, for abort - we abort
    ;; the last new-request - unless owlapi request, 
    ;; in which case we abort specific to OWLAPI reasoner, 
    ;; owlapi-abort <> abort !
    
    (setf *last-changed-progress-state* *progress-state*)
    
    (setf (progress-state-process-to-kill *progress-state*)
      #+:allegro
      multiprocessing:*current-process*
      #+:lispworks
      mp:*current-process*
      #+:sbcl
      sb-thread:*current-thread*
      #+:ccl ccl:*current-process*
      #-(:or :allegro :lispworks :sbcl :ccl)
      (error "For the progress bar the lisp implementation must support processes"))
    
    ;; the is evaluat process local - for unwind protect
    ;; in the aborted process so that it can access its
    ;; progress-state
    
    (setf *process-progress-state* *progress-state*)

    (when *abort-debugging*
      (pprint `(:new-request ,expr ,(progress-state-id *progress-state*)) *trace-output*))

    (update-progress-state)))

;;;
;;;
;;; 

(defun get-progress-state (last-changed-p)
  (if last-changed-p 
      *last-changed-progress-state*
    *progress-state*))

(defun get-current-request (&optional last-changed-p)
  (values (progress-state-current-request (get-progress-state last-changed-p))
	  (progress-state-request-processed (get-progress-state last-changed-p))))

(defun get-progress-value (&optional last-changed-p)
  (let ((res
	 (with-progress-state (get-progress-state last-changed-p)
	     (when progress-value
	       (round progress-value)))))
    
    res))

(defun abort-requested-p (&optional last-changed-p)
  (progress-state-abort-requested (get-progress-state last-changed-p)))

(defun progress-certain-p (&optional last-changed-p)
  (progress-state-progress-certain (get-progress-state last-changed-p)))

(defun request-abort (&optional last-changed-p)
  (let ((progress-state 
	 (get-progress-state last-changed-p)))
    
    (when *abort-debugging*
      (pprint `(:request-abort-for ,(progress-state-id progress-state)) *trace-output*))
    
    (setf (progress-state-abort-requested progress-state)
      t)))

(defun confirm-abort (&optional last-changed-p)
  (let ((progress-state
	 (get-progress-state last-changed-p)))
    
    (when *abort-debugging*
      (pprint `(:confirm-abort-for ,(progress-state-id progress-state)) *trace-output*))

    (setf (progress-state-abort-requested progress-state)
      nil
      (progress-state-request-was-aborted progress-state)
      t)))

(defun confirm-abort-and-kill (&optional last-changed-p)
  (let ((progress-state
	 (get-progress-state last-changed-p)))
    
    (when *abort-debugging*
      (pprint `(:confirm-abort-and-kill ,(progress-state-id progress-state)) *trace-output*))
  
    (confirm-abort last-changed-p)
    
    (let ((process
	   (progress-state-process-to-kill progress-state)))
      (when process
	#+(:or :allegro :lispworks)
	(mp:process-kill process)
	#+:sbcl
	(sb-thread:terminate-thread process)
        #+:ccl
        (ccl:process-kill process)
	#-(:or :allegro :lispworks :sbcl :ccl)
	(error "For the progress bar the lisp implementation must support processes")))))

;;;
;;;
;;; 

(defun clear-progress ()
  (setf *in-range* nil
	*progress-certain* t
	*progress-value* 0
	*progress-value1* 0
	*progress-100%* nil
	*from-%* 0
	*to-%* 100
	*ignore-progress* nil))

(defun clear-request (&optional reset-progress-p)

  (when *abort-debugging*
    (pprint `(:clear-request ,(progress-state-id *progress-state*)) *trace-output*))

  (when reset-progress-p
    (clear-progress))
  
  (setf (progress-state-current-request *progress-state*) 
    nil)
  (setf (progress-state-request-processed  *progress-state*) 
    t)
  
  (update-progress-state))

(defun clear-abort-request () 
  (when *abort-debugging*
    (pprint `(:clear-abort-request ,(progress-state-id *progress-state*)) *trace-output*))

  (setf (progress-state-request-was-aborted *progress-state*)
    nil
    (progress-state-abort-requested *progress-state*)
    nil
    (progress-state-request-processed *progress-state*)
    nil))

;;;
;;;
;;;

(defun set-progress-100% (n) 
  (cond (*ignore-progress*  
	 :ignore)
	((and (numberp n)
	      (not (zerop n)))
	 (setf *progress-100%* n)
	 (update-progress-state)
	 n)))

(defmacro without-progress (&body body)
  `(let* ((*ignore-progress* t))
     ,@body))

(defmacro with-progress-range ((progress-100% (from-% to-%)) &body body)
  `(cond (*ignore-progress* 
	  ,@body)
	 (*in-range*			
	  ;; with-progress-range only works for non-overlapping range intervals (subsequent calls) 
	  (without-progress 
	   ,@body))
	 (t 
	  (let* ((*in-range* t)
		 (*progress-100%* *progress-100%*)
		 (l (- *to-%* *from-%* )) 
		 (f1 (+ *from-%* (* l (/ ,from-% 100))))
		 (t1 (+ *from-%* (* l (/ ,to-%   100))))
		 (*from-%* f1)
		 (*to-%* t1))
	    (set-progress-100% ,progress-100%)
	    (set-progress-value 0)
	    (update-progress-state)
	    ,@body))))

(defun set-progress-range (progress-100% &optional (from-% 0) (to-% 100))
  (when (and (numberp progress-100%)
	     (not (zerop progress-100%))
	     (< from-% to-%)
	     (not *ignore-progress*))
    (clear-progress)
    
    (setf *from-%* from-%)
    (setf *to-%* to-%)
    (setf *progress-100%* progress-100%)
    (set-progress-value 0)
    (update-progress-state)))

(defun set-progress-value (n)
    (cond ((and (progress-state-abort-requested *progress-state*)
	      ;; this is the cooperative abort method - 
	      ;; in case we have multiprocessing server, we
	      ;; are simply killing the evaluation thread / process! 
	      (not *multiprocessing-server*))

	 (confirm-abort)
	 (throw :abort nil))
	
	(*ignore-progress*
	 ;; *progress-100%* 
	 ;; no! can be nil! is ok! 
	 
	 :ignore)
	
	(t 
	 
	 (let ((res 
		(cond 
		 ((numberp n) 
		  (let ((n1
			 (+ *from-%*
			    (* (- *to-%* *from-%*)
			       (/ n (or *progress-100%* 100))))))
	     
		    (setf *progress-certain* t
			  *progress-value* n1
			  *progress-value1* n))
		  
		  t)
	
		 ((and (eq n :tick) *progress-value*)
		  (setf *progress-certain* nil)
		  (incf *progress-value*)
		  (incf *progress-value1*)
	   
		  :tick)
	
		 ((and (eq n :inc) *progress-value1*)
		  ;; depending on whether tick was used before or not, 
		  ;; the progress is certain! keep it unchanged! 
		  ;; false: 
		  ;; (setf *progress-certain* t)
		  (incf *progress-value1*)
	   
		   (let ((n1
			  (+ *from-%*
			     (* (- *to-%* *from-%*)
				(/ *progress-value1* (or *progress-100%* 100))))))
	     
		     (setf *progress-certain* t
			   *progress-value* n1))
		   
		   :inc))))
	   
	   #+:ignore
	   (when (and *progress-certain* 
		      (> *progress-value* 100))
	     (error "Bad set progress value!"))
	   
	   (setf *progress-value* 
	     (mod *progress-value* 101))
    
	   (update-progress-state)
	   
	   (values res *progress-certain* 
		   (round *progress-value*)
		   (round *progress-value1*))))))
