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

(declaim (special owl-syntaxes:*converter-mode*
                  racer:*use-owllink2-interface* 
                  owl-syntaxes:*owllink2-input-syntax* 
                  owl-syntaxes:*owllink2-output-syntax*))

;;; ======================================================================

(defun server-case ()
  (readtable-case *readtable*))

(defvar *exit-server-requested* nil)

;;; ======================================================================

(defun output-stream ()
  *standard-output*)

(defun print-acl-info (stream)
  (format stream ";;; ~A is based on: ~
~%;;; ~A ~A ~
~%;;; Copyright (C) 1985-2013, Franz Inc., Oakland, CA, USA.  All Rights Reserved. ~
~%" 
	  (get-product-name) 
	  (lisp-implementation-type)
	  (lisp-implementation-version))
  (fresh-line stream)
  (terpri stream)
  (terpri stream)
  )


(defun print-info (&optional (stream t))
  (print-racer-info stream)
  #+:allegro (print-acl-info stream)
  (print-wilbur-info stream)
  #+:cl-http (print-cl-http-info stream)

  #+:allegro
  (let ((locale (sys:getenv "ACL_LOCALE")))
    (when locale
      (terpri stream)
      (princ "Locale: " stream)
      (princ locale stream))))

(defconstant +max-number-of-temp-files+ 100000)
(defconstant +max-answer-lines-per-request+ 1000)

(defparameter *default-racer-port* 8088)  ; a free TCP port number
(defparameter *default-racer-http-port* 8080)

(defparameter *multiprocessing-server* t) ; evaluate each request in one thread... allows multiple connections to multiple clients 
(defparameter *multiprocessing-server-control* t)  

(defvar *socket* nil)
(defvar *server-control-socket* nil)

#+(and :lispworks :X86-64)
(setf sys:*stack-overflow-behaviour* (* 640 1000))
#+(and :lispworks (not :X86-64))
(setf sys:*stack-overflow-behaviour* (* 260 1000))

#+:lispworks
(defparameter *stack-size* system:*sg-default-size*)

(defparameter *xml-output* nil)
(defparameter *remove-namespace-prefix* nil)
(defparameter *timing* nil)
(defparameter *timing1* nil)

(defparameter *external-format* nil) ; f. native socket

(defparameter *file-external-format* ; files 
  #+:allegro (find-external-format '(:e-crlf   :latin1-base))
  #+:lispworks :latin-1
  #+:ccl :latin-1
  #+:abcl "UTF-8"
  #+:sbcl :latin-1)


(defparameter *last-answer* nil)
(defparameter *last-error* nil)
(defparameter *one-simple-output* nil)
(defparameter *communication-debugging* nil)

;;;
;;; New process for server control: 
;;;

(defvar *server-control-process* nil)

(defvar *master-listener* nil)

(defvar *case-sensitive* 
    (not (string= (symbol-name 'foo) (symbol-name 'Foo))))

;;;
;;;
;;;

(defvar *server-request* nil)

;;;
;;;
;;;

(declaim (special *cur-reasoner* racer:*proxy*))

(defparameter *log-file* nil)
(defparameter *tcp-console-logging* nil)
(defparameter *cport-logging* nil)

(defparameter *debug-racer-server* nil)

#+:mlisp
(defparameter *ensure-alisp-socket-compatibility* nil)

(defvar *read-file-line-counter* nil)


;;; ======================================================================


(defvar *local-host-name* nil)

(defun local-host-domain-name ()
  (or *local-host-name*
      (setf *local-host-name*
            #+:aserve
            (or #+:ignore
		(ignore-errors
                 (with-open-stream (s (socket:make-socket :remote-host "www.racer-systems.com"
                                                          :remote-port 80))
                   (socket:ipaddr-to-hostname (socket:local-host s))))
		#+:ignore
                (ignore-errors
                 (with-open-stream (s (socket:make-socket :remote-host "www.racer-systems.com"
                                                          :remote-port 80))
                   (socket:ipaddr-to-dotted (socket:local-host s))))
                "localhost")
            #+:cl-http
            (www-utils:local-host-domain-name)
            #+(and (not :cl-http) (not :aserve))
            "192.168.2.101" ;"localhost"
            )))


;;; ======================================================================

(defmacro with-debug-handler (&body forms)
  `(handler-bind ((condition (lambda (c)
			       (print-debug-info c)
			       (signal c))))
     .,forms))

(defun print-debug-info (condition)
  (when *debug-racer-server*
    (with-standard-io-syntax
      (let ((*print-readably* nil)
	    (*print-miser-width* 40)
	    (*print-pretty* t)
	    #+:allegro (tpl:*zoom-print-circle* t)
	    #+:allegro (tpl:*zoom-print-level* nil)
	    #+:allegro (tpl:*zoom-print-length* nil))
	(ignore-errors			;prevent recursion
	 (flet ((zoom (s)
		  (let ((*terminal-io* s)
			(*standard-output* s))
		    ;;#+:lispworks 
		    ;;(dbg::output-backtrace :full)
                    #+:ccl
                    (uiop/image:print-backtrace)
		    #+:lispworks
		    (dbg:bug-report-form condition nil)
		    #+:allegro 
		    (tpl:do-command "zoom"
		      :from-read-eval-print-loop nil
		      :count t :all t))))
	   (with-open-file (s *log-file* :direction :output
			    :if-exists :append
			    :if-does-not-exist :create)
	     (format *terminal-io* "~%An error condition has been signalled (debug handler): ~a~%"
		 condition)
	     (format s "~%An error condition has been signalled (debug handler): ~a~%"
		 condition)
	     (fresh-line s)
	     (format s ";;; ~A Version ~A ~A"
		     (get-product-name)
		     (get-product-version)
		     (get-build-version))
	     (fresh-line s)
	     #+:allegro (fresh-line s)
	     #+:allegro (excl:print-system-state s)
	     (fresh-line s)
	     (zoom s))))))))

(defun convert-string (rules string)
  (let ((string (concatenate 'string string))) ; make a copy, to be safe
    (loop as (old new) in rules do
	  (setf string (nsubstitute new old string)))
    string))

(defun convert-output-to-string (output-string-stream)
  ;; transform Racer warning messages 
  (convert-string '((#\" #\')
		    (#\Newline #\Tab)
		    (#\Return #\Tab)
		    (#\Linefeed #\Tab))
		  (get-output-stream-string output-string-stream)))

(defun convert-output (string)
  ;; transform Racer Error messages
  (convert-string '((#\" #\')
		    (#\Newline #\Tab)
		    (#\Return #\Tab)
		    (#\Linefeed #\Tab))
		  string))

;;; f. simplified output proctol (*simple-output* = t) 
(defun convert-simple-output (object)
  (typecase object
    (string
     (substitute #\Tab #\Newline object))
    (otherwise 
     object)))

(defun generate-temp-filename (directory extension)
  (loop for i from 1 to +max-number-of-temp-files+
        for filename = (concatenate 'string directory 
                                    "temp-" (format nil "~D" i) "." extension)
        unless (probe-file filename)
        do (return-from generate-temp-filename filename))
  (error "Cannot create temporary file."))
        

;;; ======================================================================

#+:lispworks
(defun make-stream-and-talk (handle verbose unsafe-mode trace-output)
  (let ((stream (make-instance 'comm:socket-stream
                               :socket handle
                               :direction :io
                               :element-type
                               'base-char)))
    (if *multiprocessing-server* 
        (let ((system:*sg-default-size* *stack-size*))
	  (mp:process-run-function (format nil "~A Server ~A" (get-product-name) stream)
	    '()
	    'talk-on-stream 
	    stream 
	    verbose
	    unsafe-mode
	    trace-output))
      (talk-on-stream stream nil nil *trace-output*))))

#+:allegro
(defun make-stream-and-talk (stream verbose unsafe-mode trace-output)
  (if *multiprocessing-server*
      ;; this process may be terminated by abort... in this case 
      ;; we need to write ":abort" to the stream... 
      (handler-case 
	  (mp:process-run-function (format nil "~A Server ~A" (get-product-name) stream)
	    'talk-on-stream
	    stream
	    verbose unsafe-mode trace-output)
	(error (error)
	  (format t "Process run function: ~A" error))) 
    (talk-on-stream stream verbose unsafe-mode trace-output)))

#+:allegro
(defun make-stream-and-talk-server-control (stream port verbose unsafe-mode trace-output)
  (if *multiprocessing-server-control*
      (setf *server-control-process* 
	(mp:process-run-function (format nil "~A ServerControl @ ~A" (get-product-name) (1+ port))
	  'talk-on-stream 
	  stream
	  verbose unsafe-mode trace-output t))
  (talk-on-stream stream verbose unsafe-mode trace-output t)))

#+:ccl
(defun make-stream-and-talk (stream verbose unsafe-mode trace-output)
  (if *multiprocessing-server*
      ;; this process may be terminated by abort... in this case 
      ;; we need to write ":abort" to the stream... 
      (handler-case 
	  (ccl:process-run-function (format nil "~A Server ~A" (get-product-name) stream)
	    'talk-on-stream
	    stream
	    verbose unsafe-mode trace-output)
	(error (error)
	  (format t "Process run function: ~A" error))) 
    (talk-on-stream stream verbose unsafe-mode trace-output)))

#+:ccl
(defun make-stream-and-talk-server-control (stream port verbose unsafe-mode trace-output)
  (if *multiprocessing-server-control*
    (setf *server-control-process* 
          (ccl:process-run-function (format nil "~A ServerControl @ ~A" (get-product-name) (1+ port))
            'talk-on-stream 
            stream
            verbose unsafe-mode trace-output t))
    (talk-on-stream stream verbose unsafe-mode trace-output t)))

;;; ======================================================================


(defparameter *n1* 0)

(defun racer-execute-expression (expression stream)
  
  (racer:enable-reader-macros)
  
  (handler-case
      (with-debug-handler
	(with-output-to-string (output-string-stream)
	  (let ((*standard-output* output-string-stream)
		(*error-output* output-string-stream)
		(*trace-output* *trace-output*)
		(*tbox-verbose* nil)
		(*abox-verbose* nil)
		(*auto-realize* ':lazy)
		(*auto-classify* ':lazy)
		(*read-eval* nil)
		(*read-default-float-format* 'double-float)
		(package *racer-user-package*)
		(*unsafe-mode* nil)
		(*print-pretty* nil)	; required for clisp (inserts newlines) 
		#+:lispworks (system:*sg-default-size* *stack-size*))

	    (with-racer-critical-section
              (process-racer-string stream expression (incf *n1*) package
                                    output-string-stream)))))

    #+:lispworks
    (conditions:stack-overflow (c)
       (handle-stack-overflow c))
    (condition (c)
           ;;(declare (ignore c))
           (format *error-output* "Error found: ~A~%~%" c)
           ))
  nil)

;;;
;;; Note that these commands write results to CPort socket stream! 
;;;

(defun write-cport-error (stream error)
  (format stream  ":cport-error ~A" error)
  (racer-return stream))

(defun write-cport-abort (stream)    
  (write-string ":abort" stream)
  (racer-return stream))

(defun owlapi-abort-toplevel (reasoner stream output-string-stream n)
  (declare (ignorable n output-string-stream))
  (handler-case
      (progn 
	(owlapi::|OWLAPI-abort| reasoner)
	(write-cport-abort stream))
    (error (c)
      (write-cport-error stream c))))

(defun owlapi-get-progress (reasoner stream output-string-stream n)
  (declare (ignorable n output-string-stream))
  (handler-case
      (owlapi::with-progress-state-of-reasoner (reasoner)
	(multiple-value-bind (res processed-p)
	    (get-progress-value)
	  (when processed-p 
	    (write-string "processed " stream))
	  (write res :stream stream)
	  (racer-return stream)
	
	  (when *abort-debugging*
	    (pprint `(:progress ,(owlapi:owlapi-reasoner-name (owlapi::find-reasoner reasoner))
				,(progress-state-id *progress-state*)
				:val ,res)
		    *trace-output*))
	  
	  res))
    (error (c)
      (write-cport-error stream c))))

(defun owlapi-progress-certain-p (reasoner stream output-string-stream n)
  (declare (ignorable n output-string-stream))
  (handler-case
      (let ((res 
	     (owlapi::with-progress-state-of-reasoner (reasoner)
	       (progress-certain-p))))
	(write res :stream stream)
	(racer-return stream)
	res)
    (error (c)
      (write-cport-error stream c))))

(defun owlapi-get-current-request (reasoner stream output-string-stream n)
  (declare (ignorable n output-string-stream))
  (handler-case
      (let ((res  
	     (owlapi::with-progress-state-of-reasoner (reasoner)
	       (if (abort-requested-p)
		   (concatenate 'string 
		     "ABORT "
		     (get-current-request))
		 (or (get-current-request) 
		     "Idle")))))
	(write-string res stream)
	(racer-return stream)
	res)
    (error (c)
      (write-cport-error stream c))))

;;;
;;;
;;;

(defun talk-on-stream (stream verbose unsafe-mode trace-output &optional server-control-only-p)
  
  (let ((*process-progress-state* nil)) ;; will later be setf in progress.lisp : new-request 

    (racer:enable-reader-macros)
 
    (handler-case
	(with-debug-handler
	    (with-output-to-string (output-string-stream)
	      (let ((*standard-output* output-string-stream)
		    (*error-output* output-string-stream)
		    (*trace-output* trace-output)
		    (*tbox-verbose* verbose)
		    (*abox-verbose* verbose)
		    (*auto-realize* ':lazy)
		    (*auto-classify* ':lazy)
		    (*read-eval* nil)
		    (*read-default-float-format* 'double-float)
		    (package *racer-user-package*)
		    (*unsafe-mode* unsafe-mode)
		    (*print-pretty* nil) ; required for clisp (inserts newlines) 
		    #+:lispworks (system:*sg-default-size* *stack-size*))

		(loop for line = (read-line-1 stream ":eof")
		    for n from 1
		    as type = nil
		    as length = nil
		    as line-length = (length line)
		    until (string= line ":eof")
		    do (unless (string= line "") ; skip additional cr or lfs.
			 
			 ;;; these special commands can be invoked at any time
			 ;;; (server control) and do not use the standard
			 ;;; Racer :answer/:ok/:error protocol 
			   
			 (cond ((find-if #'(lambda (string-and-key)
					     (let ((string (first string-and-key))
						   (key (second string-and-key)))				 
				 
					       (when (and (>= line-length (+ 1 (length string)))
							  (string-equal (subseq line 1 (+  1 (length string)))
									string))
				
						 (setf type key)
						 (setf length (1+ (length string)))
				
						 t)))
			
					 '(("get-progress-indicator"   get-progress-indicator)
					   ("|get-progress-indicator|" get-progress-indicator)
			  
					   ("progress-certain?"   progress-certain?)
					   ("|progress-certain?|" progress-certain?)
			  
					   ("abort" abort)
					   ("|abort|" abort)
			  
					   ("get-current-request"   get-current-request)
					   ("|get-current-request|" get-current-request)
			  
					   ("|OWLAPI-abortRequest|" owlapi-abort-request)
					   ("OWLAPI-abortRequest"   owlapi-abort-request)			  
					   ("owlapi-abort-request"  owlapi-abort-request)
					   ("|OWLAPI-abort|"        owlapi-abort-request)
					   ("OWLAPI-abort"          owlapi-abort-request)			  
					   ("owlapi-abort"          owlapi-abort-request)
			  
					   ("|OWLAPI-getProgress|" owlapi-get-progress)
					   ("OWLAPI-getProgress"   owlapi-get-progress)
					   ("owlapi-get-progress"  owlapi-get-progress)
			  
					   ("|OWLAPI-progressCertain?|" owlapi-progress-certain?)
					   ("OWLAPI-progressCertain?"   owlapi-progress-certain?)
					   ("owlapi-progress-certain?"  owlapi-progress-certain?)
			  
					   ("|OWLAPI-getCurrentRequest|" owlapi-get-current-request)
					   ("OWLAPI-getCurrentRequest"   owlapi-get-current-request)
					   ("owlapi-get-current-request" owlapi-get-current-request)))

				(when (or *cport-logging* *communication-debugging*)
				  (fresh-line *trace-output*)
                                    
				  #+:cl-http
				  (write-char #\[ *trace-output*)
				  #+:cl-http
				  (http::write-standard-time (get-universal-time) *trace-output*)
				  #+:cl-http
				  (write-string "]  " *trace-output*)
				  (format *trace-output* "Cport: ~S -> " type))                  
				  
				(ecase type
				  
				  (get-progress-indicator
				   (multiple-value-bind (res processed-p)
				       (get-progress-value t)
				     (when processed-p
				       (write-string "processed " stream))
				     (write res :stream stream)
				     (racer-return stream)
				     
				     (when (or *cport-logging* *communication-debugging*)
				       (write res :stream *trace-output*))))
				     
				  (progress-certain?
				   (write (progress-certain-p t) :stream stream)
				   (racer-return stream)
				     
				   (when (or *cport-logging* *communication-debugging*)
				     (write (progress-certain-p t) :stream *trace-output*)))
		    
				  (abort

				   ;; abort in RacerPorter. 
				   ;; note that this aborts the latest changed progress object request
				   
				   (write-cport-abort stream)
				     
				   ;; if not *multiprocessing-server*, then set-progress-value
				   ;; checks the flag *abort-requested* and throws 'abort then,
				   ;; aborting the evaluation request - this also calls
				   ;; confirm-abort then - otherwise, we kill the process and
				   ;; confirm-abort needs to be called manually, and also we need to
				   ;; unwind in the process such that ":abort" is written before the
				   ;; process dies
				  
				   (when (or *cport-logging* *communication-debugging*)
				     (write-string (format nil "abort ~A"
							   (get-current-request))
						   *trace-output*))

				   (request-abort t)
				      
				   (when *multiprocessing-server*
				     (confirm-abort-and-kill t)))
				     
				  (get-current-request 
				   (let ((res
					  (if (abort-requested-p t)
					      (concatenate 'string 
						"ABORT "
						(get-current-request t))
					    (or (get-current-request t) 
						"Idle"))))
				       
				     (write-string res stream)
				     (racer-return stream)
				       
				     (when (or *cport-logging* *communication-debugging*)
				       (write-string res *trace-output*))))

				  ;; 
				  ;; OWLAPI ABort
				  ;;
		    
				  (owlapi-abort-request 
				   (let* ((*package* *racer-user-package*)
					  (reasoner 
					   (read-from-string (subseq line length))))
				     (owlapi-abort-toplevel reasoner stream output-string-stream n)))
		    
				  ;; 
				  ;; OWLAPI GetProgress
				  ;;

				  (owlapi-get-progress
				   (let* ((*package* *racer-user-package*)
					  (reasoner 
					   (read-from-string (subseq line length)))
					  (res 
					   (owlapi-get-progress reasoner stream output-string-stream n)))
				       
				     (when (or *cport-logging* *communication-debugging*)
				       (write res :stream *trace-output*))))
				    
				  ;; 
				  ;; OWLAPI ProgressCertain?
				  ;;
		    
				  (owlapi-progress-certain?
				   (let* ((*package* *racer-user-package*)
					  (reasoner 
					   (read-from-string (subseq line length)))
					  (res
					   (owlapi-progress-certain-p reasoner stream output-string-stream n)))
				       
				     (when (or *cport-logging* *communication-debugging*)
				       (write res :stream *trace-output*))))

				  ;; 
				  ;; OWLAPI getCurrentRequest?
				  ;;
		    
				  (owlapi-get-current-request 
				   (let* ((*package* *racer-user-package*)
					  (reasoner 
					   (read-from-string (subseq line length)))
					  (res
					   (owlapi-get-current-request reasoner stream output-string-stream n)))
				       
				     (when (or *cport-logging* *communication-debugging*)
				       (write res :stream *trace-output*))))))
				 
			       ;;
			       ;;
			       ;;
				 
			       (server-control-only-p
				(format stream  ":error")
				(racer-return stream))
				 
			       ;;
			       ;;
			       ;; 				  
				 
			       ((not server-control-only-p)
				
				;; experimental:
				;; in principle, OWLAPI requests can be evaluated non-blocking! 
				;; due to reasoners.lisp (virtual Racer reasoners)  -> SMP 
				;; currently, this feature is not enabled
				  
				(let ((owlapi-request-p 
				       (or (and (> line-length 8)
						(string-equal (subseq line 0 8)
							      "(OWLAPI-"))
					   (and (> line-length 9)
						(string-equal (subseq line 0 9)
							      "(|OWLAPI-")))))
				  
				  (if owlapi-request-p 
				      (process-racer-owlapi-string 
				       stream line n package
				       output-string-stream)
				      
				    (with-racer-critical-section
					(process-racer-string 
					 stream line n package
					 output-string-stream)))))))))))
   
      #+:lispworks
      (conditions:stack-overflow (c)
	(handle-stack-overflow c))
    
      (condition (c)
	;;(declare (ignore c))
	(format *error-output* "~A~%~%" c))))
 
  nil)

(defun read-line-1 (input-stream &optional eof-value)
  (let ((line nil))
    (loop for char = (read-char input-stream nil t)
	do 
	  (progn 
	    (cond ((and (eq char #\#)
			(eq #\< (peek-char nil input-stream nil t)))
		   
		   (read-char input-stream nil t)
		   
		 ;;; Unreadable Object? 
		 ;;; Dann bis zum schliessenden #\> lesen, 
		 ;;; dann weiter.

		 ;;; #<....>; # entfernen

		 ;;; mach einen String daraus!
		   
		   (push #\" line)

		   (loop for  char = (read-char input-stream nil t)
		       do 
			 (progn 
			 ;;;(princ char) 
			   (cond ((char= char #\>)
				;;; und weiter lesen
				  (push #\" line)
				  (return))
				 ((eq char t)
				  (return-from read-line-1 eof-value))
				 (t (push char line))))))

		  ((eq char 't)
		   (return-from read-line-1 eof-value))
		
		  ((char= char #\\)
		   (let ((next (peek-char nil input-stream nil t)))
		     (cond ((char= next #\|)
			    (read-char input-stream nil t)
			    (push #\\ line)
			    (push #\| line))
			   ((char= next #\N)
			    (read-char input-stream nil t)
			    (push #\newline line))
			   ((or (char= next #\S)
				(char= next #\"))
			    (read-char input-stream nil t)
			    (push #\\ line)
			    (push #\" line))
			   ((char= next #\\) 
			    (read-char input-stream nil t)
			    (push #\\ line)
			    (push #\\ line)))))
		
		  ((or (char= char #\Linefeed)
		       (char= char #\Newline)
		       (char= char #\Return))
		   
		   ;; (push (coerce (reverse line) 'string) *x*)
		   
		   (return-from read-line-1 (coerce (nreverse line) 'string)))
		
		  (t
		   (push char line)))))))

(defun handle-stack-overflow (c)
  (declare (ignore c))
  (format *error-output* "~%The computational resources made available to ~A ~
                          do not suffice for the query to be answered. ~
                          See the manual for advice about what to do." (get-product-name)))

(defun process-racer-file (kb-filename &optional count-p (case-sensitive t))
  (with-open-file (kb-file kb-filename 
		   :if-does-not-exist :error 
		   :external-format *file-external-format*
		   )
    (let* ((*read-file-line-counter* 0)
           (read-case-sensitive
	    (and case-sensitive 
		 (not (string= (symbol-name 'foo) (symbol-name 'Foo)))))
           (new-read-table (when read-case-sensitive
                             (copy-readtable nil))))
      (when read-case-sensitive
        (setf (readtable-case new-read-table) :preserve))
      (loop with eof-marker = (gensym)
	  for expr = (if read-case-sensitive 
			 (owlapi-s-expr kb-file eof-marker new-read-table)
		       (read kb-file nil eof-marker))
	  until (eq expr eof-marker)
	  do	    
	    (progn
	      (when count-p (incf *read-file-line-counter*))
	      (when expr
		(process-racer-expr expr t nil nil nil)))))))

(defun owlapi-s-expr (kb-file eof-marker new-read-table)
  (multiple-value-bind (line last-p)
      (read-line kb-file nil eof-marker)
    (declare (ignore last-p))
    (if (eq line eof-marker)
        eof-marker
      (let ((first 
	     (let ((*readtable* new-read-table))
	       (read-from-string line nil eof-marker))))
	(unless (eq first eof-marker)
	  (cons (first first)
		(rest (read-from-string line nil))))))))

(defun process-in-file (kb-filename syntax verbose)
  (case syntax
    (racer (process-racer-file kb-filename))
    (xml (xml-read-tbox-file kb-filename))
    (rdfs (rdfs-read-tbox-file kb-filename))
    (owl (owl-read-file kb-filename))
    (owllink (owllink-read-file kb-filename))
    (dig (dig-read-file kb-filename))
    (otherwise (error "do not understand syntax ~A of file ~A " syntax kb-filename)))
  (when verbose
    (format t "~%")))

(defun process-in-file-run-function (in-filename syntax out-filename 
                                                 queries-filename verbose xml unsafe-mode
				     &optional (process-init-file nil))
  #+(or :ccl :allegro) (declare (ignore process-init-file))
  (handler-case
      (with-debug-handler
	  (let ((package *racer-user-package*)
		(*read-eval* nil)
		(*read-default-float-format* 'double-float)
		(*auto-realize* (if verbose ':lazy-verbose ':lazy))
		(*auto-classify* (if verbose ':lazy-verbose ':lazy))
		(*tbox-verbose* verbose)
		(*abox-verbose* verbose)
		(*xml-output* xml)
		(*unsafe-mode* unsafe-mode)
		#+:lispworks (system:*sg-default-size* *stack-size*))

	    (if out-filename
		(with-open-file (out-stream out-filename :direction :output
				 :if-exists :supersede)
		  (let ((*standard-output* out-stream)
                        (*error-output* out-stream))
		    (when *xml-output*
		      (format out-stream "<?xml version=\"1.0\"?>")
		      (terpri out-stream)
		      (terpri out-stream)
		      (format out-stream "<ANSWERS>")
		      (terpri out-stream)
		      (terpri out-stream)
		      (force-output out-stream))
		    (let ((*package* package))
		      (process-in-file in-filename syntax verbose)
		      (when queries-filename
			(let* ((extension (pathname-type queries-filename))
			       (syntax (find extension 
					     '(xml dig owllink lisp krss racer)
					     :test #'string-equal)))
			  (case syntax
			    ((racer krss lisp)
			     (process-in-file queries-filename 'racer verbose))
			    ((dig xml)
			     (let ((*standard-output* out-stream)
				   ;;(*standard-error* out-stream)
				   )
			       (dig-read-file queries-filename :init nil)))
			    (owllink
                             (owl-read-file queries-filename :init nil))
			    (t (error "Queries may be posed only in ~A syntax ~
                                              (file extension .lisp, .racer, or .krss) ~
                                              or in DIG syntax (file extension .dig .owllink)."
				      (get-product-name)))))))
		    (when *xml-output*
		      (terpri out-stream)
		      (format out-stream "</ANSWERS>")
		      (terpri out-stream)
		      (force-output out-stream))))
	      (progn
		(when *xml-output*
		  (format t "<?xml version=\"1.0\"?>")
		  (terpri)
		  (terpri)
		  (format t "<ANSWERS>")
		  (terpri)
		  (force-output t))
		(let ((*package* package))
		  (process-in-file in-filename syntax verbose)
		  (when queries-filename
		    (let* ((extension (pathname-type queries-filename))
			   (syntax (find extension 
					 '(xml dig owllink lisp krss racer)
					 :test #'string-equal)))
		      (case syntax
			((racer krss lisp)
			 (process-in-file queries-filename 'racer verbose))
			((xml dig)
			 (dig-read-file queries-filename :init nil))
			(owllink
                         (owl-read-file queries-filename :init nil))
                        (t (error "Queries may be posed only in ~A syntax ~
                            (file extension .lisp, .racer, or .krss) ~
                            or in DIG syntax (file extension .dig or .owllink)."
				  (get-product-name)))))))
		(when *xml-output*
		  (terpri)
		  (format t "</ANSWERS>")
		  (terpri)
		  (force-output t))))
	    #+:lispworks
	    (unless process-init-file
	       (lispworks:quit))))
    #+:lispworks
    (conditions:stack-overflow (c)
      (handle-stack-overflow c))
    (condition (c)
      (format *error-output* "~A~%~%" c))))

#+(or :lispworks :allegro :ccl)
(defun wait-for-racer (racer-process)
  #+:lispworks (not (mp:process-alive-p racer-process))
  #+:allegro (not (mp:process-alive-p racer-process))
  #+:ccl (ccl:process-kill-issued racer-process))

#+:lispworks 
(defmacro connection-monitor (function)
  `(lambda (handle)
     (,function handle)))

#+:allegro
(defmacro connection-monitor ((stream) form)
  (declare (ignore stream))
  `(progn 
     ,form))

#+:ccl
(defmacro connection-monitor ((stream) form)
  (declare (ignore stream))
  `(progn 
     ,form))

(defconstant +command-line-options+ 
  '("-s"
    "-t"
    "-c" 
    "-p" 
    "-proxy"
    "-host" 
    "-domain" 
    "-http" 
    "-httplogdir"
    "-l"
    "-log"
    "-logging" 
    "-cport-logging" 
    "-debug"
    "-nohttpconsolelog"
    "-init"
    "-f"
    "-xml"
    "-rdfs"
    "-owl"
    "-owllink"
    "-owllink-input-syntax"
    "-owllink-output-syntax"
    "-dig"
    "-q"
    "-o"
    "-silent"
    "-h"
    "-help"
    "-n"
    "-m"
    "-x"
    "-u"
    "-una"
    "-secure-subnet"
    "-temp"
    "-dig11"
    "-ef"
    "-fef"
    "-alisp"
    "-protocol"
    "-socket-retries"

    "-I" ; ACL intern? 
    
    "-license"
    "-dump-license-info"
    "-dump-license-string"
    "-no-patches"
    "-no-plugins"
    "-patch-directory"
    "-plugin-directory"
    "-patches"
    "-plugins"
    "-patch"
    "-plugin"
    "-check4updates"
    "-update"
    "-update-from"))


#+(or :lispworks :allegro :ccl)
(defun racer-toplevel (&optional (command-line nil))
  
  (when *master-listener* 
    #+(or :lispworks :allegro) (mp::process-kill *master-listener*)
    #+:ccl (ccl:process-kill *master-listener*))
  
  (when *server-control-process*
    #+(or :lispworks :allegro) (mp::process-kill *server-control-process*)
    #+:ccl (ccl:process-kill *server-control-process*))
  
  (when *socket*
    (close *socket*))
  
  (when *server-control-socket*
    (close *server-control-socket*))

  #+:allegro (setf excl:*global-gc-behavior* :auto)

  (racer:enable-reader-macros)

  (handler-case 
      (let* ((line-arguments-list (or command-line
				      #+:lispworks system:*line-arguments-list*
				      #+:allegro (system:command-line-arguments #+:mswindows :application #+:mswindows t)
				      #+:ccl ccl:*command-line-argument-list*))
	     #+:lispworks
	     (stack-size (member "-s" line-arguments-list :test #'string=))
	     (timeout (member "-t" line-arguments-list :test #'string=))
	     (persistent-connection-timeout 
	      (member "-c" line-arguments-list :test #'string=))
	     (port (member "-p" line-arguments-list :test #'string=))
	     (proxy (member "-proxy" line-arguments-list :test #'string=))
	     (host (member "-host" line-arguments-list :test #'string=))
	     (domain (member "-domain" line-arguments-list :test #'string=))
	     (http-port (member "-http" line-arguments-list :test #'string=))
	     (http-log-directory (member "-httplogdir" line-arguments-list :test #'string=))
	     (logging (or (member "-l" line-arguments-list :test #'string=)
			  (member "-log" line-arguments-list :test #'string=)
			  (member "-logging" line-arguments-list :test #'string=)))
             (cport-logging (member "-cport-logging" line-arguments-list :test #'string=))
	     (debug (member "-debug" line-arguments-list :test #'string=))
	     (no-http-console-log (member "-nohttpconsolelog" line-arguments-list :test #'string=))
	     (init-file (member "-init" line-arguments-list :test #'string=))
	     (in-file (member "-f" line-arguments-list :test #'string=))
	     (xml-file (member "-xml" line-arguments-list :test #'string=))
	     (rdfs-file (member "-rdfs" line-arguments-list :test #'string=))
	     (owl-file (member "-owl" line-arguments-list :test #'string=))
	     (owllink-file (member "-owllink" line-arguments-list :test #'string=))
             (owllink-input-syntax (member "-owllink-input-syntax" line-arguments-list :test #'string=))
             (owllink-output-syntax (member "-owllink-output-syntax" line-arguments-list :test #'string=))
	     (dig-file (member "-dig" line-arguments-list :test #'string=))
	     (queries-file (member "-q" line-arguments-list :test #'string=))
	     (out-file (member "-o" line-arguments-list :test #'string=))
	     (verbose (if (find "-silent" line-arguments-list :test #'string=)
			  nil
                        t))
	     (help (or (find "-h" line-arguments-list :test #'string=)
		       (find "-help" line-arguments-list :test #'string=)))
	     (namespace-prefix-removed (find "-n" line-arguments-list :test #'string=))
	     (less-tbox-memory (cond ((find "-m" line-arguments-list :test #'string=)
                                      t)
                                     ((find "-x" line-arguments-list :test #'string=)
                                      nil)
                                     (t *always-use-lean-tbox*)))
	     (unsafe-mode (or *unsafe-mode*
                              in-file
			      logging
			      debug
			      (member "-u" line-arguments-list :test #'string=)))
	     (una (member "-una" line-arguments-list :test #'string=))
	     (secure-subnet (member "-secure-subnet" line-arguments-list :test #'string=))
	     (temp-directory (member "-temp" line-arguments-list :test #'string=))
             (dig-1-1 (member "-dig11" line-arguments-list :test #'string=))
             #+:allegro
	     (external-format (or (let ((ef (second (member "-ef" line-arguments-list :test #'string=))))
				    (ignore-errors
				     (when ef
				       (cond ((char= (elt ef 0) #\@) ;; new: -ef @UTF8
					      (or (ignore-errors 
						   (find-external-format 
						    (let ((ef (subseq ef 1)))
						      (if (char= (elt ef 0) #\( ) ;; new: -ef @(:e-cr :latin1-base)
							  (read-from-string ef)
							ef))))
						  (let ((*tcp-console-logging* t))
						    (notify-log-window ";;; Warning - cannot find external format ~A" (subseq ef 1)))))
					     (t ; old : -ef japan.jis
					      (or (ignore-errors 
						   (locale-external-format 
						    (find-locale ef)))
						  (let ((*tcp-console-logging* t))
						    (notify-log-window ";;; Warning - cannot find (external format of) locale ~A" ef))))))))
				  
				  (ignore-errors
				   (when (sys:getenv "ACL_LOCALE")
				     (locale-external-format 
				      (sys:getenv "ACL_LOCALE"))))
				   
				  (find-external-format ; for Porter!
				   '(:e-cr :latin1-base)
				   ;; "latin1"
				   )))
	     #+:allegro
	     (file-external-format (or (let ((ef (second (member "-fef" line-arguments-list :test #'string=))))
					 (ignore-errors
					  (when ef
					    (cond ((char= (elt ef 0) #\@) ;; new: -ef @UTF8
						   (or (ignore-errors 
							(find-external-format (subseq ef 1)))
						       (let ((*tcp-console-logging* t))
							 (notify-log-window ";;; Warning - cannot find external format ~A" (subseq ef 1)))))
						  (t ; old : -ef japan.jis
						   (locale-external-format 
						    (find-locale ef)))))))
				       
				       external-format))
	     
	     #+:mlisp 
	     (alisp-compatibility-mode (member "-alisp" line-arguments-list :test #'string=))
             
             (protocol (member "-protocol" line-arguments-list :test #'string=))
	     #+:aserve
             (socket-retries 
	      (or (ignore-errors
		   (parse-integer (second (member "-socket-retries" line-arguments-list :test #'string=))))
		  0))
	     
	     (*break-on-signals* nil)
	     (*read-eval* nil)
	     (*read-default-float-format* 'double-float))

	;;;
	;;;
	;;;

        (let ((unknown-command-line-options
               (set-difference 
		(remove-if #'(lambda (x) 
			       (or (not (stringp x))
                                   (and (> (length x) 1)
                                        (string= "--" (subseq x 0 2)))
                                   (and (> (length x) 0)
                                        (not (string= "-" (subseq x 0 1))))))
			   line-arguments-list)
		+command-line-options+
		:test #'string-equal)))
	  
          (dolist (bad unknown-command-line-options)
            (format t "~%;;; Warning - ignoring unknown command line option ~S" bad)))
        
	;;;
	;;;
	;;;

        (when cport-logging
          (setf *cport-logging* t))

	(when logging
	  (setf *tcp-console-logging* t)
	  (setf *log-file* (second logging)))

	(when debug
	  (setf *tcp-console-logging* t)
	  (unless *log-file*
	    (setf *log-file* "racer.log"))
	  (setf *debug-racer-server* t))

	;;;
	;;;
	;;;

        (when protocol
          (cond ((string-equal (second protocol) "old-owllink")
                 (setf racer:*use-owllink-interface* t))
                ((or (string-equal (second protocol) "owllink1")
                     (string-equal (second protocol) "owllink")
                     (setf owl-syntaxes:*converter-mode*
		       (string-equal (second protocol) "owllink-converter")))

                 (setf racer:*use-owllink2-interface* t)

                 (setf owl-syntaxes:*owllink2-input-syntax* 
		   (or 
                        
		    (when owllink-input-syntax
		      (cond ((string-equal (second owllink-input-syntax)
					   "functional")
			     :owllink-functional)
			    ((string-equal (second owllink-input-syntax)
					   "sexpr")
			     :owllink-sexpr)
			    (t
			     :owllink-xml)))
                        
		    :owllink-xml))
                               
                 (setf owl-syntaxes:*owllink2-output-syntax* 

		   (or 
                        
		    (when owllink-output-syntax
		      (cond ((string-equal (second owllink-output-syntax)
					   "functional")
			     :owllink-functional)
			    ((string-equal (second owllink-output-syntax)
					   "sexpr")
			     :owllink-sexpr)
			    (t
			     :owllink-xml)))

		    :owllink-xml)))))
        
	;;;
	;;;
	;;;                             
          
	(with-debug-handler
      
	    (when help
	      (format t "~%usage: ~A [-h] [-silent] [-p port-number] [-proxy proxy] [-http http-port-number] | [(-f|-xml|-rdfs|-owl|-owllink) input-file] [-l [filename]] [-u] [-n] [-c [seconds]] [-una] ~
                   [-q queries-file]] [-o output-file] [-i init-file] [-s stack-size] [-t timeout] [-temp directory] [-nohttpconsolelog]  [-dig11] ~
                   [-host hostname] [-debug] [-m] [-ef [external-format-name]] [-fef [external-format-name]] [-alisp] [-protocol [protocol-string]] [-socket-retries [number]] ~
                   [-license [licensefile]] [-dump-license-info [filename]] [-dump-license-string [filename]] ~
                   [-no-patches] [-no-plugins] [-patch-directory [directory]]  [-plugin-directory [directory]] ~
                   [-patches [directory]] [-plugins [directory]] [-patch [filename]] [-plugin [filename]]~%~
                   -silent do not produce diagnostic output~%~
                   -f read input from specified filename~%~
                   (.xml --> XML syntax, .rdfs --> RDFS syntax, .owl --> OWL syntax, other --> RACER syntax)~%~
                   -q read queries from specified filename~%~
                   -xml read input from specified XML filename and generate XML output (experimental).~%~
                   -rdfs read input from specified RDFS filename~%~
                   -owl read input from specified OWL filename~%~
                   -owllink read input from specified OWLlink filename~%~
                   -dig read input from specified DIG filename~%~
                   -p port number used by clients (default=~D)~%~
                   -proxy to specify a proxy server, e.g. \"http://www.myproxy.com:80\" ~%~
                   -http http port number used by clients (default=~D, use 0 for no http service)~%~
                   -host specify the host name~%~
                   -nohttpconsolelog swith off console logging for http server~%~
                   -o write output to specified filename~%~
                   -s specify new stack size (~A)~%~
                   -t timeout for answer a query in seconds~%~
                   -c timeout for persistent connections in seconds (default=10)~%~
                   -u unsafe mode: enable saving of Tboxes/Aboxes to local file system~%~
                   -l -log -logging print logging information to the console or into a file~%~
                   -cport-logging print commands sent to the control port to the console~%~
                   -debug for generating bug reports~%~
                   -i -init specify to load a knowledge base before the server starts up (compare to -f)~%~
                   -m possibly use less Tbox memory (maybe slower, use with caution)~%~
                   -n remove RDF default namespace prefix~%~
                   -una apply the unique name assumption~%~
                   -dig11 enforce that dig attributes are mapped to functional roles~%~
                   -temp specify the temp directory to be used~%~
                   -ef specify the external format for socket communication, use \@utf8 for UTF8 socket connections~%~
                   -fef specify the external format for file output~%~
                   -alisp enable socket communication compatibility with Racer <= 1.9.2~%~
                   -protocol specify the http protocol (use OWLlink to enable the OWLlink protocol)~%~
                   -socket-retries (default 0) specifies the number of \"open-socket\" retries in case TCP/IP port is still in use~%~
                   -h show this usage message~%~~%"
                
		      (first line-arguments-list)
                      
		      *default-racer-port*
		      *default-racer-http-port*
		      #+:lispworks (format nil "default=~D" *stack-size*)
                      #+:allegro "ignored in this version"
                      #+:ccl "ignored in this version")
	      (print-info)
	      #+:lispworks (lispworks:quit)
	      #+:allegro (exit)
              #+:ccl (ccl:quit))

	    #+:lispworks
	    (when stack-size
	      (setf *stack-size* (read-from-string (second stack-size))))
	    #+:lispworks
	    (check-type *stack-size* integer)
	    #+:lispworks 
	    (setf system:*sg-default-size* *stack-size*)
	    (when (and (or port http-port) (or in-file xml-file rdfs-file owl-file owllink-file 
					       dig-file queries-file))
	      (error "Either -p/-http or -f/-xml/-rdfs/-owl/-owllink/-dig and/or -q may be supplied."))
	    (if port
		(setf port (read-from-string (second port)))
	      (setf port *default-racer-port*))          
	    (when proxy
	      (setf racer:*proxy* (second proxy)))
	    (if host
		(setf host (second host))
	      (setf host "127.0.0.1"))
	    (when domain
	      (setf domain (second domain)))
	    (if http-port
		(setf http-port (read-from-string (second http-port)))
	      (setf http-port *default-racer-http-port*))
	    (when http-log-directory
	      (setf http-log-directory (second http-log-directory)))
	    (when (find "-a" line-arguments-list :test #'string=)
	      (setf *multiprocessing-server* nil))
	    (when secure-subnet 
	      (setf secure-subnet (second secure-subnet)))
	    (when temp-directory
	      (setf *temp-directory* temp-directory))
            (when dig-1-1
              (setf *dig-version* +dig-1-1+))
	    (when (and (or in-file xml-file rdfs-file owl-file owllink-file dig-file)
		       (not (eql (count nil (list in-file xml-file rdfs-file  
						  owl-file owllink-file dig-file))
				 5)))
	      (error "Either -f, -xml, -rdfs, -dig, -owl or -owllink may be supplied."))
	    (setf *always-use-lean-tbox* less-tbox-memory)
	    (when persistent-connection-timeout
	      (setf persistent-connection-timeout
		(read-from-string (second persistent-connection-timeout)))
	      (assert (and (integerp persistent-connection-timeout)
			   (> persistent-connection-timeout 0)))
	      #+:cl-http
	      (setf http:*persistent-connection-timeout* 
		persistent-connection-timeout))
	    (when timeout
	      (setf *server-timeout*
		(read-from-string (second timeout)))
	      (assert (and (integerp *server-timeout*)
			   (> *server-timeout* 0))))
	    (when namespace-prefix-removed
	      (setf *remove-namespace-prefix* t))
	    (when una
	      (setf *use-unique-name-assumption* t))
	    #+:mlisp 
	    (when alisp-compatibility-mode
	      (setf *ensure-alisp-socket-compatibility* t))		
            #+:allegro
	    (when external-format
	      (setf *external-format* external-format))
	    #+:allegro
	    (when file-external-format
	      (setf *file-external-format* file-external-format))
	    (when init-file
	      (setf init-file (second init-file))
	      (let* ((extension (pathname-type init-file))
		     (syntax (or (find extension '(owl owllink xml rdfs dig) :test #'string-equal)
				 'racer))
		     (out-filename (second out-file))
		     (queries-filename (second queries-file))
		     #+:lispworks
		     (system:*sg-default-size* *stack-size*)
		     (racer-process (#+(or :lispworks :allegro)
                                       mp:process-run-function
                                       #+:ccl ccl:process-run-function
                                       "RACER"
				      #+:lispworks '()
				      'process-in-file-run-function
				      init-file syntax 
				      out-filename
				      queries-filename
				      verbose 
				      xml-file 
				      unsafe-mode
				      t)))
		#+(or :lispworks :allegro)
                (mp:process-wait "WAITING FOR RACER TO FINISH"
                  'wait-for-racer
                  racer-process)
                #+:ccl
                (ccl:process-wait "WAITING FOR RACER TO FINISH"
                  'wait-for-racer
                  racer-process)))
	    #+:cl-http
	    (unless (or in-file xml-file rdfs-file 
			owl-file owllink-file dig-file (= http-port 0))
              (http:enable-racer-server http-port 
                                           :domain domain
                                           :host host
                                           :secure-subnet secure-subnet
					   ;;:server-maintainer "mo"~
					   ;;:server-bug-list "mo@fh-wedel.de"
					   ;;:mailer-host "mail.fh-wedel.de"
                                           :log-directory http-log-directory
                                           :file-logging-p http-log-directory
                                           :console-logging-p (not no-http-console-log))
		
		#+:onlinedoc
		(publish-doc-entries :html))
	    
	    #+:aserve 
	    (unless (or in-file xml-file rdfs-file 
			owl-file owllink-file dig-file (= http-port 0)) 
		
		(let ((retry 0)
		      (done nil))
		  
		  (loop until done do
			(handler-case
			    (progn 
			      (racer:enable-racer-server http-port :console-logging-p (not no-http-console-log))
			      (setf done t))
			  (error (c)
			    (incf retry)
			    (format *error-output* ";;; Failed to acquire socket for port ~A.~%Condition was: ~A~%" http-port c)
			    (if (> retry socket-retries)
				(error c)
			      (progn 
				(format *error-output* 
					";;; Retry to acquire socket - try ~A of ~A tries~%~%" 
					retry 
					socket-retries)
				(sleep 1))))))
		  
		  #+:onlinedoc
		  (publish-doc-entries :html)))
	    
	    (unless (or in-file xml-file rdfs-file owl-file owllink-file dig-file)

	      (when (fboundp 'sirius)
		(funcall (intern (symbol-name 'sirius) :cl-user)))

	      (let* ((stream (output-stream)))
		(flet ((print-it ()
			 (unless xml-file
			   (print-info stream))
			 (terpri stream)
			 #+:cl-http
			 (let ((*tcp-console-logging* t))
			   (notify-log-window "HTTP service enabled for: http://~A:~D/"
					      (local-host-domain-name) http-port)
                           #-:nrql-dev
                           (load-documentation)
                           #-:nrql-dev
                           (notify-log-window "HTML documentation at   : http://~A:~D/reference-toplevel.html" (local-host-domain-name) http-port))
			 #+:aserve
			 (progn
			   (fresh-line stream)
			   (format stream "HTTP service enabled for: http://~A:~D/" 
                                   (local-host-domain-name) http-port)
                           #-:nrql-dev
			   (progn 
			     (fresh-line stream)
			     (load-documentation)
			     (format stream "HTML documentation at   : http://~A:~D/reference-toplevel.html" (local-host-domain-name) http-port))
			   (force-output stream))
			 (fresh-line stream)
			 #+:cl-http (write-char #\[ stream)
			 #+:cl-http
			 (http::write-standard-time (get-universal-time) stream)
			 #+:cl-http (write-string "]  " stream)
			 #+:aserve
			 (progn 
			   (format stream "TCP service enabled for : http://~A:~D/" 
				   (local-host-domain-name)
				   port)
			   (fresh-line stream)
			   (format stream "TCP control enabled for : http://~A:~D/" 
				   (local-host-domain-name)
				   (1+ port)))
                         #+(and :ccl (not :aserve))
                         (progn 
			   (format stream "TCP service enabled for : http://~A:~D/" 
				   (local-host-domain-name)
				   port)
			   (fresh-line stream)
			   (format stream "TCP control enabled for : http://~A:~D/" 
				   (local-host-domain-name)
				   (1+ port)))
			 #+(and :lispworks (not :aserve))
			 (format stream "TCP service enabled for : http://~A:~D/" 
				 (local-host-domain-name) port)
			 (fresh-line stream)
			 (force-output stream)))
		  
                  (print-it))))

	    (if (or in-file xml-file rdfs-file owl-file owllink-file dig-file)
		(let* ((new-in-file (or in-file xml-file rdfs-file owl-file owllink-file dig-file))
		       (in-filename (second new-in-file))
		       (extension (pathname-type in-filename))
		       (syntax (cond (xml-file 'xml)
				     (rdfs-file 'rdfs)
				     (owl-file 'owl)
                                     (owllink-file 'owllink)
				     (dig-file 'dig)
				     (t (or (find extension '(xml rdfs owl dig owllink) :test #'string-equal)
					    'racer)))))
		  ;;(unless out-file
		  ;;  (unless (eq syntax 'racer)
		  ;;    (format t "~&Using syntax ~A~%" syntax)))
		  (let* ((out-filename (second out-file))
			 (queries-filename (second queries-file))
			 #+:lispworks (system:*sg-default-size* *stack-size*)
			 (racer-process (#+(or :lispworks :allegro)
                                           mp:process-run-function 
                                           #+:ccl
                                           ccl:process-run-function
                                           "RACER"
					  #+:lispworks '()
					  'process-in-file-run-function
					  in-filename syntax out-filename
					  queries-filename verbose
					  xml-file
					  unsafe-mode)))
		    (if timeout
			(progn
			  (#+(or :lispworks :allegro) 
                             mp:process-wait-with-timeout
                             #+:ccl ccl:process-wait-with-timeout
                             "WAITING FOR RACER TO FINISH"
                             (read-from-string (second timeout))
                             'wait-for-racer
                             racer-process)
			  (when #+:lispworks (mp:process-alive-p racer-process)
				#+:allegro (mp:process-active-p racer-process)
                            #+:ccl (not (ccl:process-kill-issued racer-process))
				(#+(or :lispworks :allegro) mp:process-kill
                                   #+:ccl ccl:process-kill racer-process)
				(format (or out-filename t)
					"~%TIMEOUT after ~D secs~%" (read-from-string (second timeout)))))
		      (#+(or :lispworks :allegro) mp:process-wait 
                         #+:ccl ccl:process-wait
                         "WAITING FOR RACER TO FINISH"
				       'wait-for-racer
				       racer-process))))
	      (progn
		#+:lispworks
		(progn
		  (comm:start-up-server :function
					(connection-monitor
					 (lambda (port)
					   (make-stream-and-talk 
					    port 
					    verbose unsafe-mode *trace-output*)))
					:service port)                  
		  (loop 
                   (if *exit-server-requested*
                       (lispworks:quit))
                   (sleep 10)))
		
		#+:allegro
		(let ((socket nil)
		      (server-control-socket nil)
		      
		      (retry 0))
		  
		  (loop until socket do
			(handler-case
			    (setf socket 
			      (socket:make-socket 
			       :connect :passive 
			       :local-port port :reuse-address t))
			  (error (c)
			    (incf retry)
			    (format *error-output* ";;; Failed to acquire socket for port ~A.~%Condition was: ~A~%" port c)
			    (setf socket nil)
			    (if (> retry socket-retries)
				(error c)
			      (progn 
				(format *error-output* 
					";;; Retry to acquire socket - try ~A of ~A tries~%~%" 
					retry 
					socket-retries)
				(sleep 1))))))
		  
		  (setf *socket* socket)
		  
		  (loop until server-control-socket do
			(handler-case
			    (setf server-control-socket 
			      (socket:make-socket 
			       :connect :passive 
			       :local-port (1+ port) :reuse-address t))
			  (error (c)
			    (incf retry)
			    (format *error-output* ";;; Failed to acquire socket for port ~A.~%Condition was: ~A~%" (1+ port) c)
			    (setf socket nil)
			    (if (> retry socket-retries)
				(error c)
			      (progn 
				(format *error-output* 
					";;; Retry to acquire socket - try ~A of ~A tries~%~%" 
					retry 
					socket-retries)
				(sleep 1))))))
		    
		  (setf *server-control-socket* server-control-socket)			  
		    
		  (macrolet ((server-loop (socket &optional server-control-p)
			       `(unwind-protect
				    (loop
				      (let ((stream (socket:accept-connection ,socket)))
					#+:allegro 
					(if *external-format*
					    (setf (stream-external-format stream) *external-format*)
					  (let ((*tcp-console-logging* t))
					    (notify-log-window "No external format specified, using external format of socket stream")))
					#+:allegro
					(let ((*tcp-console-logging* t))
					  (notify-log-window "External format of ~A is ~A"
							     port (ef-name (stream-external-format stream))))
					(connection-monitor (stream)
							    ,(if server-control-p 
								 '(make-stream-and-talk-server-control stream port verbose unsafe-mode *trace-output*)
							       '(make-stream-and-talk stream verbose unsafe-mode *trace-output*)))))
				  (progn   
				    (close ,socket)))))
		    
		    (setf *server-control-process* 
		      (mp:process-run-function (format nil "~A ServerControl @ ~A" (get-product-name) (1+ port))
			#'(lambda ()
			    (server-loop server-control-socket t))))
		    
		    (setf (mp:process-priority *server-control-process*) 100)

		    (server-loop socket)))

                #+:ccl
                (let ((socket nil)
		      (server-control-socket nil)
		      
		      (retry 0))
		  
		  (loop until socket do
			(handler-case
			    (setf socket 
			      (ccl:make-socket 
			       :connect :passive 
			       :local-port port :reuse-address t))
			  (error (c)
			    (incf retry)
			    (format *error-output* ";;; Failed to acquire socket for port ~A.~%Condition was: ~A~%" port c)
			    (setf socket nil)
			    (if (> retry socket-retries)
				(error c)
			      (progn 
				(format *error-output* 
					";;; Retry to acquire socket - try ~A of ~A tries~%~%" 
					retry 
					socket-retries)
				(sleep 1))))))
		  
		  (setf *socket* socket)
		  
		  (loop until server-control-socket do
			(handler-case
			    (setf server-control-socket 
			      (ccl:make-socket 
			       :connect :passive 
			       :local-port (1+ port) :reuse-address t))
			  (error (c)
			    (incf retry)
			    (format *error-output* ";;; Failed to acquire socket for port ~A.~%Condition was: ~A~%" (1+ port) c)
			    (setf socket nil)
			    (if (> retry socket-retries)
				(error c)
			      (progn 
				(format *error-output* 
					";;; Retry to acquire socket - try ~A of ~A tries~%~%" 
					retry 
					socket-retries)
				(sleep 1))))))
		    
		  (setf *server-control-socket* server-control-socket)			  
		    
		  (macrolet ((server-loop (socket &optional server-control-p)
			       `(unwind-protect
				    (loop
				      (let ((stream (ccl:accept-connection ,socket)))
					#+:ignore 
                                        (if *external-format*
					    (setf (stream-external-format stream) *external-format*)
					  (let ((*tcp-console-logging* t))
					    (notify-log-window "No external format specified, using external format of socket stream")))
					#+:ignore 
                                        (let ((*tcp-console-logging* t))
					  (notify-log-window "External format of ~A is ~A"
							     port (stream-external-format stream)))
					(connection-monitor (stream)
							    ,(if server-control-p 
								 '(make-stream-and-talk-server-control stream port verbose unsafe-mode *trace-output*)
							       '(make-stream-and-talk stream verbose unsafe-mode *trace-output*)))))
				  (progn   
				    (close ,socket)))))
		    
		    (setf *server-control-process* 
		      (ccl:process-run-function (format nil "~A ServerControl @ ~A" (get-product-name) (1+ port))
			#'(lambda ()
			    (server-loop server-control-socket t))))
		    
		    (setf (ccl:process-priority *server-control-process*) 100)

		    (server-loop socket)))))))
		  
    (condition (c)
      (format *error-output* "~A~%~%" c))))

;;;
;;;
;;;

(defun toplevel-unsafe (&optional (command-line nil))
  (setf *unsafe-mode* t)
  (racer-toplevel command-line))

(defun toplevel-utf8 (&optional (command-line nil))
  ;;; LRacer: (setf *external-format* :utf-8)
  ;;; ACL 
  (setf owl-syntaxes:*converter-mode* nil)
  (racer-toplevel (append '("-ef" "@utf8") command-line)))

(defun toplevel-p4 (&optional (command-line nil))
  (toplevel-utf8 command-line))

(defun toplevel-p4-debug (&optional (command-line nil))
  (toplevel-p4 (append '("-u" "-ef" "@UTF8" "-logging" "-cport-logging") command-line)))

(defun toplevel-utf8-base-cr (&optional (command-line nil))
  ;;; LRacer: (setf *external-format* :utf-8)
  ;;; for SBCL - hangs on ACL Windows!
  (setf owl-syntaxes:*converter-mode* nil)
  (racer-toplevel (append '("-ef" "@(e-cr utf8-base)") command-line)))

(defun toplevel-latin1 (&optional (command-line nil))
  ;;; LRacer: (setf *external-format* :latin-1)
  ;;; ACL 
  (setf owl-syntaxes:*converter-mode* nil)
  (racer-toplevel (append '("-ef" "@latin1") command-line)))

(defun toplevel-latin1-base-cr (&optional (command-line nil))
  ;;; LRacer: (setf *external-format* :latin-1)
  ;;; for SBCL - hangs on ACL Windows!
  (setf owl-syntaxes:*converter-mode* nil)
  (racer-toplevel (append '("-ef" "@(e-cr latin1-base)") command-line)))

;;;
;;;
;;;

(defun toplevel-owllink (&optional (command-line nil))
  (setf owl-syntaxes:*converter-mode* nil)
  (racer-toplevel (append '("-protocol" "owllink") command-line)))

(defun toplevel-owllink-functional (&optional (command-line nil))
  (setf owl-syntaxes:*converter-mode* nil)
  (racer-toplevel (append '("-protocol" "owllink"
                                  "-owllink-input-syntax" "functional"
                                  "-owllink-output-syntax" "functional")
                    command-line)))

(defun toplevel-owllink-sexpr (&optional (command-line nil))
  (setf owl-syntaxes:*converter-mode* nil)
  (racer-toplevel (append '("-protocol" "owllink"
                                  "-owllink-input-syntax" "sexpr"
                                  "-owllink-output-syntax" "sexpr")
                    command-line)))

(defun toplevel-owllink-converter (from to &optional (command-line nil))
  (racer-toplevel (append `("-protocol" "owllink-converter"
                                  "-owllink-input-syntax" 
                                  ,(ecase from
                                     (:sexpr "sexpr")
                                     (:functional "functional")
                                     (:xml "xml"))
                                  "-owllink-output-syntax"
                                  ,(ecase to
                                     (:sexpr "sexpr")
                                     (:functional "functional")
                                     (:xml "xml")))
                    command-line)))

;;;
;;;
;;;

(defun lisp2xml (stream x &optional (i 0))
  (dotimes (n i) (princ "  " stream))
  (cond ((null x) (format stream "<LIST/>~%"))
        ((listp x)
         (format stream "<LIST>~%")
         (mapcar #'(lambda (y) (lisp2xml stream y (1+ i))) x)
         (dotimes (n i) (princ "  " stream))
         (format stream "</LIST>~%"))
        ((numberp x)
         (format stream "<NUMBER VALUE=\"~A\"/>~%" x))
        (t (format stream "<SYMBOL NAME=\"~A\"/>~%" x))))

(defun lisp2xml-1 (stream x &optional (i 0))
  (dotimes (n i) (princ "  " stream))
  (cond ((null x) (format stream "<LIST/>"))
        ((listp x)
         (format stream "<LIST>")
         (mapcar #'(lambda (y) (lisp2xml stream y (1+ i))) x)
         (dotimes (n i) (princ "  " stream))
         (format stream "</LIST>"))
        ((numberp x)
         (format stream "<NUMBER VALUE=\"~A\"/>" x))
        (t (format stream "<SYMBOL NAME=\"~A\"/>" x))))

;;;
;;;
;;; 

(defmacro answer (expr state stream n value output-string-stream)
  `(unwind-protect
       (if *server-timeout*
	   (with-timeout (*server-timeout* 
			  (if (eq ,stream t)
			      (progn 
				(format *standard-output* "~A timed out after ~A seconds." ',expr *server-timeout*)
				(terpri *standard-output*)
				(force-output *standard-output*))
			    (progn
			      (format ,stream ":answer ~D \"TIMEOUT\" \"~A\"" 
				      ,n 
				      "")
			      (racer-eol ,stream))))
	     (let* ((abort-occurred-p t)
		    (res
		     (catch :abort
		       (prog1
			   (progn
			     ,value)
			 (setf abort-occurred-p nil))))
		    (val 
		     (with-progress-state *process-progress-state*
		       (cond (abort-occurred-p 
			    (clear-request t)
			    (clear-abort-request)
			    :abort)
			   (t 
			    (clear-request)
			    res)))))
	       (answer-1 ,expr ,state ,stream ,n val ,output-string-stream)))
	 (let* ((abort-occurred-p t)
		(res
		 (catch :abort
		   (prog1
		       (progn
			 ,value)
		     (setf abort-occurred-p nil))))
		(val 
		 (with-progress-state *process-progress-state*
		   (cond (abort-occurred-p 
			(clear-request t)
			(clear-abort-request)
			:abort)
		       (t 
			(clear-request)
			res)))))
	   (answer-1 ,expr ,state ,stream ,n val ,output-string-stream)))
     (with-progress-state *process-progress-state*

       (when *abort-debugging*
	 (pprint `(:unwind-protect ,expr
		    ,(progress-state-request-was-aborted *process-progress-state*)
		    :id ,(progress-state-id *process-progress-state*)
		    :owlapi-reasoner  
		     ,(owlapi::owlapi-reasoner-name 
		       (progress-state-owlapi-reasoner *process-progress-state*)))
		 *trace-output*))
       
       ;; this only gets true if the process-kill approach was used... in this case,
       ;; confirm abort registers that, and unwind protect here runs. In case of
       ;; the "throw :abort" method, we simply return ":abort" (see macro answer and catch :abort) 
       (when (progress-state-request-was-aborted *process-progress-state*)

	 (when *abort-debugging*
	   (pprint `(:aborted 
		     :id ,(progress-state-id *process-progress-state*)
		     :owlapi-reasoner  
		     ,(owlapi::owlapi-reasoner-name 
		       (progress-state-owlapi-reasoner *process-progress-state*)))
		   *trace-output*))
	 
	 (let ((*one-simple-output* t))
	   (answer-1 nil ,state ,stream ,n :abort ,output-string-stream))
	 
	 (clear-request t)
	 (clear-abort-request)
	 (close ,stream)))))

(defun answer-1 (expr state stream n value output-string-stream)

  (setf *last-answer* value)

  (when *cur-reasoner*
    (setf (last-answer *cur-reasoner*) value))

  (when *communication-debugging* 
    (fresh-line *trace-output*)
    (format *trace-output* "RPort: ~S -> ~S" expr value))
  
  (when state
    (unless (and (consp expr)
                 (member (first expr) '(publish publish-1 subscribe subscribe-1)))
      (error "The form ~A is not allowed inside a state." expr)))
  
  (unless state
      
    (cond ((eq stream t)
	
	   ;; this branch is taken if request is within racer-read-file
	   ;; added by MW 16.01.2011: prevent answer strings becomming too long 
	
	   (cond ((and *read-file-line-counter* 
		       (= *read-file-line-counter* +max-answer-lines-per-request+))
	       
		  (let ((message 
			 "*** ERROR *** OUTPUT TOO LONG FOR A SINGLE REQUEST! The sub-requests in this request will be processed, but their sub-results will not be returned as part of this answer string. If this is unacceptable, please use single individual requests instead. Thank you."))

		    (cond (*xml-output*
			
			   (terpri *standard-output*)
			   (format *standard-output* "<ANSWER QUERY=\"~S\">" expr)
			   (terpri *standard-output*)
			   (format *standard-output* "~A" message)
			   (format *standard-output* "</ANSWER>")
			   (terpri *standard-output*)
			   (force-output stream))
		       
			  (t
		 
			   (format *standard-output* "~A --> ~A" expr message)
			   (terpri *standard-output*)
			   (force-output *standard-output*)))))
	      
		 ((and *read-file-line-counter* 
		       (> *read-file-line-counter* +max-answer-lines-per-request+))
	       
		  ;; no more sub-results are included in the answer string once this limit was exceeded
	       
		  )
	      
		 (*xml-output*
		  (terpri *standard-output*)
		  (format *standard-output* "<ANSWER QUERY=\"~S\">" expr)
		  (terpri *standard-output*)
		  (lisp2xml *standard-output* value)
		  (format *standard-output* "</ANSWER>")
		  (terpri *standard-output*)
		  (force-output stream))
	      
		 (t (format *standard-output* "~A --> ~A" expr (transform-value value))
		    (terpri *standard-output*)
		    (force-output *standard-output*))))
	  
	  (t
	   
	   (when *timing*
	     (format *standard-output*
		     "~%Evaluating ~S took ~6,4F realtime seconds and ~6,4F CPU seconds.~%~%" expr
		     (/ (- (get-internal-real-time) *timing1*) internal-time-units-per-second)
		     (/ (- (get-internal-run-time) *timing*) internal-time-units-per-second)))
      
	   (cond (*xml-output*
		  (format stream ":answer ~D \"\"" n)
		  (format stream "<ANSWER QUERY=\"~S\">" expr)
		  (lisp2xml-1 stream value)
		  (format stream "</ANSWER>")
		  (format stream " \"~A\"" 
			  (convert-output-to-string output-string-stream))
		  (terpri stream)
		  (force-output stream))

		 (t (cond (*one-simple-output*
			   (format stream "~S" (convert-simple-output value)))
                     
			  ((and *cur-reasoner*
				(simple-output *cur-reasoner*))

			   (ecase (return-policy *cur-reasoner*)
			     ((:answer-direct :smart)
			      (format stream "~S" (convert-simple-output value)))
			     (:get-last-answer))
			   
			   (let ((warning 
				  (get-output-stream-string output-string-stream)))
			     
			     (setf (last-output-stream-string *cur-reasoner*)
			       warning)
			   
			     (unless (string-equal warning "")
			       (format *trace-output* 
				       "~A" warning))))

			  (t
			   (let ((string
				  (format nil ":answer ~D \"~A\" \"~A\"" 
					  n (transform-value value)
					  (convert-output-to-string output-string-stream))))
			     (write-string string stream))))
	       
		    (racer-eol stream)))))))

(defmacro ok (expr stream n state value output-string-stream)
  `(unwind-protect
       (let* ((abort-occurred-p t)
	      (res
	       (catch :abort
		 (prog1
		     (progn
		       ,value)
		   (setf abort-occurred-p nil))))
	      (val 
	       (with-progress-state *process-progress-state*
		 (cond (abort-occurred-p 
		      (clear-request t)
		      (clear-abort-request)
		      :abort)
		     (t 
		      (clear-request)
		      res)))))
	 (declare (ignorable val))
	 (ok-1 ,expr ,stream ,n ,state ,output-string-stream))
     (with-progress-state *process-progress-state*
       
         (when *abort-debugging*
	 (pprint `(:unwind-protect ,expr
		    ,(progress-state-request-was-aborted *process-progress-state*)
		    :id ,(progress-state-id *process-progress-state*)
		    :owlapi-reasoner  
		     ,(owlapi::owlapi-reasoner-name 
		       (progress-state-owlapi-reasoner *process-progress-state*)))
		 *trace-output*))
	 
       ;; this only gets true if the process-kill approach was used... in this case,
       ;; confirm abort registers that, and unwind protect here runs. In case of
       ;; the "throw :abort" method, we simply return ":abort" (see macro answer and catch :abort) 
       (when (progress-state-request-was-aborted *process-progress-state*)

	 (when *abort-debugging*
	   (pprint `(:aborted 
		     :id ,(progress-state-id *process-progress-state*)
		     :owlapi-reasoner  
		     ,(owlapi::owlapi-reasoner-name 
		       (progress-state-owlapi-reasoner *process-progress-state*)))
		   *trace-output*))
	 
	 (let ((*one-simple-output* t))
	   (answer-1 nil ,state ,stream ,n :abort ,output-string-stream))
	 
	 (clear-request t)
	 (clear-abort-request)
	 (close ,stream)))))

(defun ok-1 (expr stream n state output-string-stream)
  (unless state
    
    (setf *last-answer* :ok)

    (when *cur-reasoner*
      (setf (last-answer *cur-reasoner*) *last-answer*))
    
    (when *timing*
      (format *standard-output*
              "~%Evaluating ~S took ~6,4F realtime seconds and ~6,4F CPU seconds.~%~%" expr
              (/ (- (get-internal-real-time) *timing1*) internal-time-units-per-second)
              (/ (- (get-internal-run-time) *timing*) internal-time-units-per-second)))
    
    (unless (eq stream t)

      (cond ((and *cur-reasoner*
                  (simple-output *cur-reasoner*))

             (ecase (return-policy *cur-reasoner*)
               (:answer-direct
                (format stream ":ok"))
               ((:get-last-answer :smart)
                ))

             (setf (last-output-stream-string *cur-reasoner*)
	       (get-output-stream-string output-string-stream)))
	    
            (t 
	     (format stream ":ok ~D \"~A\"" n 
		     (convert-output-to-string output-string-stream))))
	       
      (racer-eol stream))))

;;;
;;;
;;;

(defun transform-value (value)
  (cond ((null value)
         value)
        ((consp value)
         (transform-cons value nil))
        ((stringp value)
         (format nil "\\\"~A\\\"" (transform value)))
        ((symbolp value)
         (let ((*package* (symbol-package value)))
           (if (null *package*)
             (format nil "~A" (symbol-name value))
             (if *remove-namespace-prefix*
               (format nil "~S" (remove-prefix value *current-tbox*))
               (format nil "~S" value)))))
        (t value)))


(defun transform-cons (value acc)
  (cond ((null value)
         (nreverse acc))
        ((consp value)
         (transform-cons (rest value) (cons (transform-value (first value)) acc)))
        (t (let* ((last acc)
                  (result (nreverse acc)))
             (setf (cdr last)
                   (transform-value value))
             result))))


(defun transform (string) 
  (let* ((length (length string))
         (result (make-array length :adjustable t :element-type 'character :fill-pointer 0)))
    (loop for i from 0 below length 
          for ch = (aref string i) do
          (cond ((char= ch #\")
                 (vector-push-extend #\\ result)
                 (vector-push-extend #\S result))
                ((char= ch #\|)
                 (vector-push-extend #\\ result)
                 (vector-push-extend ch result))
                ((char= ch #\\)
                 (vector-push-extend #\\ result)
                 (vector-push-extend ch result))
                ((char= ch #\Newline)
                 (vector-push-extend #\\ result)
                 (vector-push-extend #\N result))
                ((char= ch #\Return)
                 (vector-push-extend #\\ result)
                 (vector-push-extend #\N result))
                ((char= ch #\Linefeed)
                 (vector-push-extend #\\ result)
                 (vector-push-extend #\N result))
                (t (vector-push-extend ch result))))
    
    result))

(defun racer-eol (stream)
  ;;; wird durch das external format bestimmt!
  (princ #\Newline stream)
  
  (force-output stream)) 

(defun racer-return (stream)
  ;;; wird durch das external format bestimmt!
  (princ #\Return stream)
  
  (force-output stream))

(defun write-abort-confirmation-to-stream (stream)
  (write-string ":abort" stream)
  (racer-return stream))

;;;
;;;
;;; 

(defun get-tbox (&optional default)
  (or (and default (find-tbox default))
      *current-tbox*
      (error "No current tbox set.")))

(defun get-abox (&optional default)
  (or (and default (find-abox default))
      *current-abox*
      (error "No current abox set.")))

(defun symbol-eql (sym1 sym2)
  (if (and (symbolp sym1) (symbolp sym2))
      (eql sym1 sym2)
    (error "Individuals must be symbols.")))

(defun handle-in-tbox (name &key (init t) (size *default-tbox-concept-size*)
				   (role-size *default-tbox-role-size*)
				   (signature nil))
  (in-tbox-internal name init signature size role-size))

(defun handle-in-abox (abox-name 
		       &optional (tbox-name nil tbox-name-specified-p))
  (if tbox-name-specified-p
      (in-abox-internal abox-name tbox-name t)
      (in-abox-internal abox-name (ensure-tbox-name (get-tbox)) nil)))

(defun process-racer-string (stream string n package output-string-stream)
  (let* ((*package* package)
	 (*server-request* string)
         (expr
	  (read-from-string string)))
    (new-request string ) 
    (process-racer-expr expr stream n nil output-string-stream)))

(defun process-racer-owlapi-string (stream string n package output-string-stream)
  (let* ((*package* package)
	 (*server-request* string)
	 (pos1 (position #\( string))
	 (pos (position #\space string :start pos1))
	 (pos2 (position #\) string :start pos1))
	 (op 
	  (if pos
	      (subseq string (1+ pos1) pos)
	    (subseq string (1+ pos1) pos2)))
	 (expr
	  (read-from-string string)))

    (unless *case-sensitive*
      
      (unless (char= (elt op 0) #\|)
	(setf (first expr) 
	  (intern op))))
    
    (process-racer-expr expr stream n nil output-string-stream)))

#|
(defun filter-concept-nameset (concept-nameset)
  (loop for sym in concept-nameset
        when (symbol-package sym)
        collect sym))

(defconstant +log-window-print-length+ 60)

(defun truncate-if-too-long (format-string format-args)
  (let ((string (apply #'format nil format-string format-args)))
    (if (> (length string) +log-window-print-length+)
      (setf string (concatenate 'string (subseq string 0 (1- +log-window-print-length+)) "...")))
    string))
|#

(defun notify-log-window (format-string &rest format-args)
  #-(or :cl-http :aserve)
  (declare (ignore format-string format-args))
  #+(or :cl-http :aserve)
  (when *tcp-console-logging*
    (fresh-line *trace-output*)
    #+:cl-http (write-char #\[ *trace-output*)
    #+:cl-http
    (http::write-standard-time (get-universal-time) *trace-output*)
    #+:cl-http (write-string "]  " *trace-output*)
    (apply #'format *trace-output* format-string format-args)
    (terpri *trace-output*)
    (force-output *trace-output*)))


#+:mlisp
(defun transform-expr (expr)

  (cond ((consp expr)
	 
	 ;;; MLisp also accepts upper case key words, 
	 ;;; T, NIL, and upper case Racer function names 
	 ;;; Alisp LRacer still has to UPCASE "t", "nil" results -> T, NIL 
	 
	 `( ;;; nicht alle Funktionen beginnen mit Kleinbuchstaben!
	    ;;; z.B. OWLAPI-getAxioms ... 
	    ;;; aber fast alle
	   
	   ,(let ((op (first expr)))
	      (if (fboundp op)
		  op
		(intern (string-downcase (symbol-name op)))))
	   
	   ,@(tree-replace-expr (rest expr))))
	
	(t expr)))


#+:mlisp
(defun tree-replace-expr (expr)
  (cond ((consp expr)
	 (mapcar #'tree-replace-expr expr))
	
	((keywordp expr) 
	 ;;; Pech - das Symbol wurde durch READ schon angelegt. 
	 ;;; Hier kann ich nur ein Auto-Downcase machen
	 ;;; (wird schon stimmen)

	 (intern (string-downcase (symbol-name expr)) :keyword))
	
	((symbolp expr)
	 
	 (let* ((name (symbol-name expr)))
	 
           (multiple-value-bind (sym foundp)
               (find-symbol name :racer)

             (if (member foundp '(:external :inherited))
                 
                 sym
               
               (let ((uname (string-downcase name)))
                 
                 (multiple-value-bind (sym foundp)
                     (find-symbol uname :racer)
                             
		   (if (member foundp '(:external :inherited))
                       sym
                     expr)))))))
        
	(t expr)))


(defun process-racer-expr (expr stream n state output-string-stream)

  (setf *last-error* nil)
  
  #+:mlisp
  (when *ensure-alisp-socket-compatibility*
    (setf expr (transform-expr expr)))
  
  (when (consp expr)
    ;; might be a symbol! server value!
    (when *cur-reasoner*
      (setf (last-error *cur-reasoner*) nil)
      (when (member (first expr) 
		    '(|OWLAPI-getLastOutputStreamString|
		      |OWLAPI-getLastAnswer|
		      owlapi-get-last-output-stream-string
		      owlapi-get-last-answer))
	(setf (last-output-stream-string *cur-reasoner*) nil)))

    (when *log-file*
      (unless (or (eq (first expr) 'logging-on)
		  (eq (first expr) 'logging-off))
	(with-open-file (log-stream *log-file* :direction :output
			 :if-exists :append
			 :if-does-not-exist :create)
	  (write expr :stream log-stream)
	  (terpri log-stream)
	  (force-output log-stream)))))
  
  (notify-log-window "~S" expr)
  
  (handler-case
      (with-debug-handler
	  (cond ((and (consp expr) (symbolp (first expr)))
                 (process-racer-expr0 expr stream n state output-string-stream))
		(t (if (and (symbolp expr)
			    (minilisp-server-value-p expr))
		       (answer expr
			       state
			       stream
			       n
			       (get-minilisp-value expr)
			       output-string-stream)
		     (error "Illegal syntax: ~A" expr)))))
    (condition (c)
      (progn
	(when *log-file*
	  (with-open-file (log-stream *log-file* :direction :output
			   :if-exists :append
			   :if-does-not-exist :create)
	    (format log-stream "ERROR: ~A" c)
	    (terpri log-stream)
	    (force-output log-stream)))	
	(setf *last-error* c)		
	(when *cur-reasoner*
	  (setf (last-error *cur-reasoner*) c))     
	(cond ((eq stream t) 
	       (setf stream *standard-output*)
	       (format stream 
		       (convert-output (format nil "~&Error: ~A~%~%" c)))
	       (terpri stream)
	       (force-output stream))
	      (t
	       (cond ((and *cur-reasoner*
			   (or (simple-output *cur-reasoner*)
			       *one-simple-output*))
		      
		      ;; (format stream "~S" (convert-simple-output value)))

		      ;; (break)
		      (format stream  ":error")
		      
		      (setf *last-answer* (convert-output (format nil "~A" c)))
		      (setf (last-answer *cur-reasoner*) *last-answer*)
		      
		      (setf (last-output-stream-string *cur-reasoner*)
			(convert-output-to-string output-string-stream)))
                             
		     (t 
			      
		      (let* ((output-string-stream 
			      (convert-output-to-string output-string-stream))
			     (string 
			      (format nil ":error ~D ~A \"~A\"" 
				      n 
				      (convert-output (format nil "~A" c))
				      output-string-stream)))
				     
			(setf *last-answer* string)

			(when *cur-reasoner*
			  (setf (last-answer *cur-reasoner*) string
				(last-output-stream-string *cur-reasoner*) 
				output-string-stream))
				  
			;;(format stream string)
			;; changed by MW, 14.1.2009
			;; problem with ~ in error messages -> formatter error
			(write-string string stream))))
                             
	       (racer-eol stream)))))))
    
(defun process-racer-expr0 (expr stream n state output-string-stream)
  (case (first expr)
    (without-progress
		      
     (let* ((expr (cdr expr))
	    (expr (if (cdr expr)
		      `(progn ,@expr)
		    (first expr))))
			
       (setf (progress-state-current-request *progress-state*)
	 (substring
	  (with-output-to-string (stream)
	    (if (stringp expr)
		(write-string
		 expr
		 stream)
	      (write expr :stream stream)))
	  80)))
		      
     (without-progress
      (loop for expr1 in (rest expr) do
	    (process-racer-expr expr1 stream n state output-string-stream))))
		     
    (in-tbox
     (answer expr state
	     stream n 
	     (apply #'handle-in-tbox (rest expr))
	     output-string-stream))
    (set-current-tbox
     (answer expr state
	     stream n 
	     (set-current-tbox (second expr))
	     output-string-stream))
    (init-tbox 
     (answer expr state
	     stream n 
	     (apply #'init-tbox (rest expr))
	     output-string-stream))
    ((delete-tbox forget-tbox)
     (answer expr state
	     stream n 
	     (forget-tbox (second expr))
	     output-string-stream))
    (delete-all-tboxes
     (answer expr state
	     stream n 
	     (delete-all-tboxes)
	     output-string-stream))
    (clear-default-tbox
     (answer expr state
	     stream n 
	     (clear-default-tbox)
	     output-string-stream))
    (current-tbox
     (answer expr state
	     stream n 
	     (current-tbox)
	     output-string-stream))
    (in-abox
     (answer expr state
	     stream n 
	     (apply #'handle-in-abox (rest expr))
	     output-string-stream))
    (set-current-abox
     (answer expr state
	     stream n 
	     (set-current-abox (second expr))
	     output-string-stream))
    (init-abox 
     (answer expr state
	     stream n 
	     (apply #'init-abox (rest expr))
	     output-string-stream))
    (current-abox
     (answer expr state
	     stream n 
	     (current-abox)
	     output-string-stream))
    ((delete-abox forget-abox)
     (answer expr state
	     stream n 
	     (forget-abox (second expr))
	     output-string-stream))
    (delete-all-aboxes
     (answer expr state
	     stream n 
	     (delete-all-aboxes)
	     output-string-stream))
    (in-knowledge-base 
     (let ((tbox-name (second expr))
	   abox-name
	   init
	   (args (rest (rest expr))))
       (cond ((null args) 
	      (setf abox-name tbox-name)
	      (setf init t))
	     ((null (rest args))
	      (setf abox-name (first args))
	      (setf init t))
	     ((eq (first args) :init)
	      (setf init (second args))
	      (setf abox-name tbox-name))
	     ((eq (second args) :init)
	      (setf init (third args))
	      (setf abox-name (first args)))
	     (t 
	      (error "Syntax error in (in-knowledge-base ~A~{ ~S~})" tbox-name args)))
       (answer expr state
	       stream n 
	       (list (handle-in-tbox tbox-name :init init)
		     (if init
			 (handle-in-abox abox-name
					 tbox-name)
		       (handle-in-abox abox-name)))
	       output-string-stream)))
    (ensure-tbox-signature
     (ok expr stream n state 
	 (ensure-tbox-signature 
	  (get-tbox (second expr))
	  :atomic-concepts (getf (rest (rest expr)) ':atomic-concepts)
	  :roles (getf (rest (rest expr)) ':roles)
	  :transitive-roles (getf (rest (rest expr)) ':transitive-roles)
	  :features (getf (rest (rest expr)) ':features)
	  :attributes (getf (rest (rest expr)) ':attributes))
	 output-string-stream))
    (ensure-abox-signature 
     (ok expr stream n state 
	 (ensure-abox-signature (get-abox (second expr))
				:individuals (getf (rest (rest expr))
						   ':individuals)
				:objects (getf (rest (rest expr))
					       ':objects))
	 output-string-stream))
    (signature 
     (ok expr stream n state 
	 (let ((individuals (getf (rest expr) ':individuals))
	       (atomic-concepts (getf (rest expr) ':atomic-concepts))
	       (roles (getf (rest expr) ':roles))
	       (transitive-roles (getf (rest expr) ':transitive-roles))
	       (features (getf (rest expr) ':features))
	       (attributes (getf (rest expr) ':attributes))
	       (objects (getf (rest expr) ':objects)))
	   (when (or atomic-concepts roles transitive-roles features attributes)
	     (ensure-tbox-signature 
	      (get-tbox)
	      :atomic-concepts atomic-concepts
	      :roles roles
	      :transitive-roles transitive-roles
	      :features features
	      :attributes attributes))
	   (when (or individuals objects)
	     (ensure-abox-signature (get-abox)
				    :individuals individuals
				    :objects objects)))
	 output-string-stream))
    (implies
     (ok expr stream n state 
	 (apply #'add-concept-axiom (get-tbox)
		(append (rest expr) '(:inclusion-p t)))
	 output-string-stream))
    (define-primitive-concept
	(ok expr stream n state 
	    (if (eql (length (rest expr)) 1)
		(apply #'add-concept-axiom (get-tbox)
		       (append (rest expr) '(top) '(:inclusion-p t)))
	      (apply #'add-concept-axiom (get-tbox)
		     (append (rest expr) '(:inclusion-p t))))
	    output-string-stream))
    (add-concept-axiom 
     (ok expr stream n state 
	 (apply #'add-concept-axiom (find-tbox (second expr)) (rest (rest expr)))
	 output-string-stream))
    ((add-role-axiom add-role-axioms)
     (ok expr stream n state 
	 (apply #'add-role-axioms (find-tbox (second expr)) 
		(rest (rest expr))) 
	 output-string-stream))
    ((equivalent define-concept)
     (ok expr stream n state 
	 (apply #'add-concept-axiom (get-tbox)
		(append (rest expr) '(:inclusion-p nil)))
	 output-string-stream))
    ((add-disjointness-axiom disjoint)
     (ok expr stream n state 
	 (let ((group (gensym)))
	   (loop for name in (rest expr) do
		 (add-disjointness-axiom (get-tbox) name group expr)))
	 output-string-stream))
    (define-disjoint-primitive-concept
	(ok expr stream n state 
	    (progn 
	      (loop for disjoint-list-sym in (third expr) do
		    (add-disjointness-axiom (get-tbox)
					    (second expr) disjoint-list-sym expr))
	      (add-concept-axiom (get-tbox)
				 (second expr) (fourth expr) :inclusion-p t))
	    output-string-stream))
    (define-primitive-role
	(ok expr stream n state 
	    (apply #'add-role-axioms (get-tbox) (rest expr))
	    output-string-stream))
    (define-datatype-property
	(ok expr stream n state 
	    (apply #'add-datatype-property (get-tbox) (rest expr))
	    output-string-stream))
    (add-datatype-property
     (ok expr stream n state 
	 (apply #'add-datatype-property (rest expr))
	 output-string-stream))
    (define-distinct-individual
	(ok expr stream n state 
	    (add-individual (get-abox (third expr)) (second expr) t (or (fourth expr) +top-symbol+))
	    output-string-stream))
    (define-individual
	(ok expr stream n state 
	    (add-individual (get-abox (third expr)) (second expr) nil (or (fourth expr) +top-symbol+))
	    output-string-stream))
    (define-primitive-attribute
	(ok expr stream n state 
	    (apply #'add-role-axioms (get-tbox) (second expr) :feature t (rest (rest expr)))
	    output-string-stream))
    (define-concrete-domain-attribute 
	(ok expr stream n state 
	    (let ((domain (getf (rest (rest expr)) ':domain nil))
		  (type (getf (rest (rest expr)) ':type nil))
		  (tbox (get-tbox)))
	      (ensure-cd-attribute (second expr) domain type tbox))
	    output-string-stream))
    #|(state 
     (let ((*check-subscriptions-inhibited* t)) 
       (loop for expr1 in (rest expr) do
	     (process-racer-expr expr1 stream n t output-string-stream)))
     (answer expr state
	     stream n (check-subscriptions (get-abox))
	     output-string-stream))|#
    (instance 
     (ok expr stream n state 
	 (add-concept-assertion (get-abox) (second expr) (third expr))
	 output-string-stream))
    (add-concept-assertion 
     (ok expr stream n state 
	 (apply #'add-concept-assertion (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (related 
     (ok expr stream n state 
	 (add-role-assertion (get-abox) (second expr) (third expr) (fourth expr))
	 output-string-stream))
    (unrelated 
     (ok expr stream n state 
	 (add-role-assertion (get-abox) (second expr) (third expr) (fourth expr))
	 output-string-stream))
    (add-role-assertion 
     (ok expr stream n state 
	 (apply #'add-role-assertion (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (add-negated-role-assertion 
     (ok expr stream n state 
	 (apply #'add-negated-role-assertion (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (constrained
     (ok expr stream n state 
	 (add-attribute-assertion (get-abox) (second expr) (third expr) (fourth expr))
	 output-string-stream))
    (add-attribute-assertion
     (ok expr stream n state 
	 (add-attribute-assertion (get-abox (second expr)) (third expr) (fourth expr) (fifth expr))
	 output-string-stream))
    (constraints 
     (ok expr stream n state 
	 (loop for expr1 in (rest expr) do
	       (add-constraint-assertion (get-abox)
					 expr1))
	 output-string-stream))
    (add-constraint-assertion 
     (ok expr stream n state 
	 (add-constraint-assertion (get-abox (second expr)) (third expr))
	 output-string-stream))
    ((forget)
     (ok expr stream n state 
	 (let ((tbox (getf (second expr) ':tbox *current-tbox*))
	       (abox (getf (second expr) ':abox *current-abox*)))
	   (forget-statement tbox abox (rest (rest expr))))
	 output-string-stream))
    (forget-concept-axiom
     (ok expr stream n state 
	 (apply #'forget-concept-axiom (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (forget-role-axioms 
     (ok expr stream n state 
	 (apply #'forget-role-axioms (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    ((forget-statement)
     (ok expr stream n state 
	 (apply #'forget-statement (rest expr))
	 output-string-stream))
    (forget-concept-assertion 
     (ok expr stream n state 
	 (apply #'forget-concept-assertion (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (forget-role-assertion 
     (ok expr stream n state 
	 (apply #'forget-role-assertion (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (forget-constrained-assertion 
     (ok expr stream n state 
	 (apply #'forget-constrained-assertion (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (forget-constraint 
     (ok expr stream n state 
	 (apply #'forget-constraint (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    ((forget-disjointness-axiom)
     (ok expr stream n state 
	 (forget-disjointness-axiom (get-tbox (second expr)) 
				    (third expr) (fourth expr)
				    (fifth expr))
	 output-string-stream))
    (forget-disjointness-axiom-statement
     (ok expr stream n state 
	 (forget-disjointness-axiom-statement (get-tbox (second expr)) (third expr))
	 output-string-stream))
    (forget-individual
     (ok expr stream n state 
	 (forget-individual (second expr) (get-abox (third expr)))
	 output-string-stream))
    ((concept-satisfiable? concept-satisfiable-p)
     (answer expr state
	     stream n (concept-satisfiable-p (second expr) 
					     (get-tbox (third expr)))
	     output-string-stream))
    ((concept-subsumes? concept-subsumes-p)
     (answer expr state
	     stream n (concept-subsumes-p (second expr) (third expr)
					  (get-tbox (fourth expr)))
	     output-string-stream))
    ((concept-equivalent? concept-equivalent-p)
     (answer expr state
	     stream n (concept-equivalent-p (second expr) (third expr)
					    (get-tbox (fourth expr)))
	     output-string-stream))
    ((concept-disjoint? concept-disjoint-p)
     (answer expr state
	     stream n (concept-disjoint-p (second expr) (third expr)
					  (get-tbox (fourth expr)))
	     output-string-stream))
    ((concept-p concept?) 
     (answer expr state
	     stream n (concept-p (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((concept-is-primitive-p concept-is-primitive?)
     (answer expr state
	     stream n (concept-is-primitive-p (second expr) 
					      (get-tbox (third expr)))
	     output-string-stream))
    (alc-concept-coherent 
     (answer expr state
	     stream n (apply #'alc-concept-coherent (rest expr))
	     output-string-stream))
    ((role-satisfiable? role-satisfiable-p)
     (answer expr state
	     stream n (role-satisfiable-p (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((role-subsumes? role-subsumes-p)
     (answer expr state
	     stream n (role-subsumes-p (second expr) (third expr) 
				       (get-tbox (fourth expr)))
	     output-string-stream))
    ((role-p role?) 
     (answer expr state
	     stream n (role-p (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((transitive-p transitive?)
     (answer expr state
	     stream n (transitive-p (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((feature-p feature?)
     (answer expr state
	     stream n (feature-p (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((inverse-feature-p inverse-feature?)
     (answer expr state
	     stream n (inverse-feature-p (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((cd-attribute-p cd-attribute?)
     (answer expr state
	     stream n (cd-attribute-p (second expr) (get-tbox (third expr)))
	     output-string-stream))
    (classify-tbox 
     (ok expr stream n state 
	 (classify-tbox (get-tbox (second expr)))
	 output-string-stream))
    (check-tbox-coherence
     (answer expr state
	     stream n (check-tbox-coherence (get-tbox (second expr)))
	     output-string-stream))
    ((tbox-classified-p tbox-classified?)
     (answer expr state
	     stream n (tbox-classified-p (get-tbox (second expr)))
	     output-string-stream))
    ((tbox-coherent? tbox-coherent-p)
     (answer expr state
	     stream n (tbox-coherent-p (get-tbox (second expr)))
	     output-string-stream))
    ((tbox-prepared? tbox-prepared-p)
     (answer expr state
	     stream n (tbox-prepared-p (get-tbox (second expr)))
	     output-string-stream))
    (realize-abox  
     (ok expr stream n state 
	 (realize-abox (get-abox (second expr)))
	 output-string-stream))
    ((abox-realized-p abox-realized?)
     (answer expr state
	     stream n (abox-realized-p (get-abox (second expr)))
	     output-string-stream))
    ((abox-consistent? abox-consistent-p)
     (answer expr state
	     stream n (apply #'abox-consistent-p (rest expr))
	     output-string-stream))
    ((abox-una-consistent? abox-una-consistent-p)
     (answer expr state
	     stream n (apply #'abox-una-consistent-p (rest expr))
	     output-string-stream))
    ((abox-prepared? abox-prepared-p)
     (answer expr state
	     stream n (apply #'abox-prepared-p (rest expr))
	     output-string-stream))
    (check-abox-coherence 
     (answer expr state
	     stream n (check-abox-coherence 
		       (get-abox (second expr))
		       (if (third expr)
			   (if *unsafe-mode*
			       (third expr)
			     (error "Cannot save files. ~
                                                      Please start ~A with the option -u for unsafe mode ~
                                                      (do not forget -- under Windows)." 
				    (get-product-name)))))
	     output-string-stream))
    (otherwise
     (process-racer-expr1 expr stream n state output-string-stream))))

(defun process-racer-expr1 (expr stream n state output-string-stream)
  (case (first expr)
    ((individual-p individual?)
     (answer expr state
	     stream n
	     (individual-p (second expr) (get-abox (third expr)))
	     output-string-stream))
    ((individual-instance? individual-instance-p)
     (answer expr state
	     stream n 
	     (individual-instance-p (second expr) (third expr) 
				    (get-abox (fourth expr)))
	     output-string-stream))
    ((individuals-related? individuals-related-p)
     (answer expr state
	     stream n 
	     (individuals-related-p (second expr) (third expr) (fourth expr)
				    (get-abox (fifth expr)))
	     output-string-stream))
    ((individuals-equal? individuals-equal-p)
     (answer expr state
	     stream n 
	     (symbol-eql (second expr) (third expr))
	     output-string-stream))
    ((individuals-not-equal? individuals-not-equal-p)
     (answer expr state
	     stream n 
	     (not (symbol-eql (second expr) (third expr)))
	     output-string-stream))
    ((concept-synonyms atomic-concept-synonyms)
     (answer expr state
	     stream n
	     (atomic-concept-synonyms (second expr) 
				      (get-tbox (third expr)))
	     output-string-stream))
    ((concept-descendants atomic-concept-descendants)
     (answer expr state
	     stream n
	     (atomic-concept-descendants (second expr)
					 (get-tbox (third expr)))
	     output-string-stream))
    ((concept-ancestors atomic-concept-ancestors)
     (answer expr state
	     stream n
	     (atomic-concept-ancestors (second expr) 
				       (get-tbox (third expr)))
	     output-string-stream))
    ((concept-parents atomic-concept-parents)
     (answer expr state
	     stream n
	     (atomic-concept-parents (second expr) 
				     (get-tbox (third expr)))
	     output-string-stream))
    ((concept-children atomic-concept-children)
     (answer expr state
	     stream n
	     (atomic-concept-children (second expr) 
				      (get-tbox (third expr)))
	     output-string-stream))
    ((role-descendants atomic-role-descendants)
     (answer expr state
	     stream n
	     (atomic-role-descendants (second expr) 
				      (get-tbox (third expr)))
	     output-string-stream))
    ((role-ancestors atomic-role-ancestors)
     (answer expr state
	     stream n
	     (atomic-role-ancestors (second expr) 
				    (get-tbox (third expr)))
	     output-string-stream))
    ((role-children atomic-role-children)
     (answer expr state
	     stream n
	     (apply #'atomic-role-children
		    (second expr)
		    (get-tbox (third expr))
		    (cdddr expr))
	     output-string-stream))
    ((role-parents atomic-role-parents)
     (answer expr state
	     stream n
	     (apply #'atomic-role-parents
		    (second expr)
		    (get-tbox (third expr))
		    (cdddr expr))
	     output-string-stream))
    ((role-inverse atomic-role-inverse) 
     (answer expr state
	     stream n
	     (atomic-role-inverse (second expr) 
				  (get-tbox (third expr)))
	     output-string-stream))
    (all-tboxes
     (answer expr state
	     stream n 
	     (loop for tbox-name in (all-tboxes)
		 when (find-symbol (symbol-name tbox-name) *package*)
		 collect tbox-name)
	     output-string-stream))
    (find-tbox 
     (answer expr state
	     stream n 
	     (let ((tbox (find-tbox (second expr) (third expr))))
	       (when tbox
		 (ensure-tbox-name tbox)))
	     output-string-stream))
    (set-find-tbox
     (ok expr stream n state 
	 (setf (find-tbox (second expr)) (third expr))
	 output-string-stream))
    (all-atomic-concepts 
     (answer expr state
	     stream n 
	     (apply #'all-atomic-concepts 
		    (get-tbox (second expr))
		    (rest (rest expr)))
	     output-string-stream))
    (all-equivalent-concepts 
     (answer expr state
	     stream n 
	     (apply #'all-equivalent-concepts
		    (get-tbox (second expr))
		    (rest (rest expr)))
	     output-string-stream))
    (all-roles 
     (answer expr state
	     stream n (apply #'all-roles
			     (get-tbox (second expr))
			     (rest (rest expr)))
	     output-string-stream))
    (all-features 
     (answer expr state
	     stream n (apply #'all-features (get-tbox (second expr))
			     (rest (rest expr)))
	     output-string-stream))
    (all-transitive-roles 
     (answer expr state
	     stream n (apply #'all-transitive-roles (get-tbox (second expr))
			     (rest (rest expr)))
	     output-string-stream))
    (all-attributes 
     (answer expr state
	     stream n (apply #'all-attributes (get-tbox (second expr))
			     (rest (rest expr)))
	     output-string-stream))
    (attribute-type
     (answer expr state
	     stream n (attribute-type (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((attribute-domain attribute-domain-1)
     (answer expr state
	     stream n (attribute-domain-1 (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((individual-direct-types most-specific-instantiators)
     (answer expr state
	     stream n 
	     (most-specific-instantiators (second expr) 
					  (get-abox (third expr)))
	     output-string-stream))
    ((individual-types instantiators)
     (answer expr state
	     stream n 
	     (instantiators (second expr) 
			    (get-abox (third expr)))
	     output-string-stream))
    ((concept-instances retrieve-concept-instances)
     (answer expr state
	     stream n 
	     (if (null (rest (rest (rest expr))))
		 (retrieve-concept-instances (second expr) 
					     (get-abox (third expr)))
	       (retrieve-concept-instances (second expr) 
					   (get-abox (third expr))
					   (fourth expr)))
	     output-string-stream))
    ((individual-fillers retrieve-individual-fillers)
     (answer expr state
	     stream n
	     (apply #'retrieve-individual-fillers
		    (second expr) (third expr)
		    (get-abox (fourth expr))
		    (cddddr expr))
	     output-string-stream))
    ((retrieve-related-individuals related-individuals )
     (answer expr state
	     stream n
	     (retrieve-related-individuals (second expr) 
					   (get-abox (third expr)))
	     output-string-stream))
    ((individual-filled-roles retrieve-individual-filled-roles)
     (answer expr state
	     stream n 
	     (retrieve-individual-filled-roles (second expr) (third expr)
					       (get-abox (fourth expr)))
	     output-string-stream))
    ((direct-predecessors retrieve-direct-predecessors)
     (answer expr state
	     stream n 
	     (retrieve-direct-predecessors (second expr) (third expr) 
					   (get-abox (fourth expr)))
	     output-string-stream))
    (associated-aboxes 
     (answer expr state
	     stream n 
	     (associated-aboxes (get-tbox (second expr)))
	     output-string-stream))
    (associated-tbox
     (answer expr state
	     stream n 
	     (associated-tbox (get-abox (second expr)))
	     output-string-stream))
    (all-aboxes
     (answer expr state
	     stream n 
	     (loop for abox-name in (all-aboxes)
		 when (find-symbol (symbol-name abox-name) *package*)
		 collect abox-name)
	     output-string-stream)) 
    (find-abox 
     (answer expr state
	     stream n (let ((abox (find-abox (second expr) (third expr))))
			(when abox (ensure-abox-name abox)))
	     output-string-stream))
    (set-find-abox
     (ok expr stream n state 
	 (setf (find-abox (second expr)) (third expr))
	 output-string-stream))
    (all-individuals 
     (answer expr state
	     stream n (apply #'all-individuals (get-abox (second expr))
			     (rest (rest expr)))
	     output-string-stream))
    (all-concept-assertions-for-individual 
     (answer expr state
	     stream n 
	     (apply #'all-concept-assertions-for-individual (rest expr))
	     output-string-stream))
    (all-role-assertions-for-individual-in-domain 
     (answer expr state
	     stream n
	     (apply #'all-role-assertions-for-individual-in-domain 
		    (rest expr))
	     output-string-stream))
    (all-role-assertions-for-individual-in-range 
     (answer expr state
	     stream n
	     (apply #'all-role-assertions-for-individual-in-range 
		    (rest expr))
	     output-string-stream))
    (all-concept-assertions 
     (answer expr state
	     stream n
	     (apply #'all-concept-assertions (rest expr))
	     output-string-stream))
    (all-role-assertions 
     (answer expr state
	     stream n 
	     (apply #'all-role-assertions (rest expr))
	     output-string-stream))
    (all-attribute-assertions 
     (answer expr state
	     stream n 
	     (apply #'all-attribute-assertions (rest expr))
	     output-string-stream))
    (all-constraints
     (answer expr state
	     stream n 
	     (apply #'all-constraints (rest expr))
	     output-string-stream))
    ((clone-tbox create-tbox-clone)
     (answer expr state
	     stream n 
	     (create-tbox-clone (get-tbox (second expr))
				:new-name (getf (rest (rest expr)) ':new-name)
				:overwrite (getf (rest (rest expr)) ':overwrite))
	     output-string-stream))
    ((clone-abox create-abox-clone)
     (answer expr state
	     stream n 
	     (create-abox-clone (get-abox (second expr))
				:new-name (getf (rest (rest expr)) ':new-name)
				:overwrite (getf (rest (rest expr)) ':overwrite)
				:copy-rules (getf (rest (rest expr)) ':copy-rules))
	     output-string-stream))
    ((include-kb racer-read-file)
     (ok expr stream n state 
	 (let ((*package* *racer-user-package*))
	   (process-racer-file (transform-filename (second expr)) t))
	 output-string-stream))
    (racer-read-document
     (ok expr stream n state 
	 (let ((*package* *racer-user-package*))
	   (racer-read-document (second expr)))
	 output-string-stream))
    (xml-read-tbox-file 
     (answer expr state
	     stream n 
	     (xml-read-tbox-file (second expr))
	     output-string-stream))
    (rdfs-read-tbox-file 
     (answer expr state
	     stream n 
	     (rdfs-read-tbox-file (second expr))
	     output-string-stream))
    (owl-read-file 
     (answer expr state
	     stream n 
	     (apply #'owl-read-file (rest expr))
	     output-string-stream))
    (owl-read-document
     (answer expr state
	     stream n 
	     (apply #'owl-read-document (rest expr))
	     output-string-stream))
    (owllink-read-file 
     (answer expr state
	     stream n 
	     (apply #'owllink-read-file (rest expr))
	     output-string-stream))
    (owllink-read-document
     (answer expr state
	     stream n 
	     (apply #'owllink-read-document (rest expr))
	     output-string-stream))
    (dig-read-file 
     (answer expr state
	     stream n 
	     (apply #'dig-read-file (second expr) (third expr))
	     output-string-stream))
    (dig-read-document
     (answer expr state
	     stream n 
	     (apply #'dig-read-document (second expr) (third expr))
	     output-string-stream))
    (taxonomy
     (answer expr state
	     stream n 
	     (taxonomy (get-tbox (second expr)))
	     output-string-stream))
    (print-tbox-tree
     (ok expr stream n state 
	 (print-tbox-tree (get-tbox (second expr)) output-string-stream)
	 output-string-stream))
    (save-tbox
     (ok expr stream n state 
	 (if *unsafe-mode*
	     (if (keywordp (third expr))
		 (apply #'save-tbox (second expr) (list* (get-tbox) (rest (rest expr))))
	       (apply #'save-tbox (rest expr)))
	   (error "Illegal operator save-tbox. Only allowed with option -u (do not forget -- under Windows)."))
	 output-string-stream))
    (save-abox
     (ok expr stream n state 
	 (if *unsafe-mode*
	     (if (keywordp (third expr))
		 (apply #'save-abox (second expr) (list* (get-abox) (rest (rest expr))))
	       (apply #'save-abox (rest expr)))
	   (error "Illegal operator save-abox. Only allowed with option -u (do not forget -- under Windows)."))
	 output-string-stream))
    (save-kb 
     (ok expr stream n state 
	 (if *unsafe-mode*
	     (apply #'save-kb (rest expr))
	   (error "Illegal operator save-kb. Only allowed with option -u (do not forget -- under Windows)."))
	 output-string-stream))
    (compute-index-for-instance-retrieval 
     (ok expr stream n state 
	 (compute-index-for-instance-retrieval (get-abox (second expr)))
	 output-string-stream))
    (ensure-subsumption-based-query-answering
     (ok expr stream n state 
	 (ensure-subsumption-based-query-answering (get-abox (second expr)))
	 output-string-stream))
    (ensure-small-tboxes 
     (ok expr stream n state 
	 (setf *always-use-lean-tbox* t)
	 output-string-stream))
    (describe-tbox
     (answer expr state
	     stream n 
	     (describe-tbox (get-tbox (second expr)) nil)
	     output-string-stream))
    (describe-abox
     (answer expr state
	     stream n 
	     (describe-abox (get-abox (second expr)) nil)
	     output-string-stream))
    ((describe-individual)
     (answer expr state
	     stream n 
	     (describe-individual (second expr) (get-abox (third expr)) nil)
	     output-string-stream))
    (describe-individual1
     (answer expr state
	     stream n 
	     (describe-individual1 (second expr) (get-abox (third expr)) nil)
	     output-string-stream))
    (describe-concept
     (answer expr state
	     stream n 
	     (describe-concept (second expr) (get-tbox (third expr)) nil)
	     output-string-stream))
    (describe-role 
     (answer expr state
	     stream n 
	     (describe-role (second expr) (get-tbox (third expr)) nil)
	     output-string-stream))
    ((individual-attribute-fillers retrieve-individual-attribute-fillers)
     (answer expr state
	     stream n 
	     (retrieve-individual-attribute-fillers (second expr) 
						    (third expr)
						    (get-abox (fourth expr)))
	     output-string-stream))
    (told-value
     (answer expr state
	     stream n 
	     (told-value (second expr) (get-abox (third expr)))
	     output-string-stream))
    ((individual-told-attribute-value retrieve-individual-told-attribute-value)
     (answer expr state
	     stream n 
	     (retrieve-individual-told-attribute-value (second expr) 
						       (third expr)
						       (get-abox (fourth expr)))
	     output-string-stream))
    #|((publish publish-1)
     (answer expr state
	     stream n 
	     (publish-1 (second expr) (get-abox (third expr)))
	     output-string-stream))
    ((unpublish unpublish-1)
     (answer expr state
	     stream n 
	     (unpublish-1 (second expr) (get-abox (third expr)))
	     output-string-stream))
    ((subscribe subscribe-1)
     (answer expr state
	     stream n 
	     (subscribe-1 (second expr) (third expr) 
			  (if (symbolp (fourth expr))
			      (get-abox (fourth expr))
			    (get-abox nil)))
	     output-string-stream))
    ((unsubscribe unsubscribe-1)
     (answer expr state
	     stream n 
	     (unsubscribe-1 (second expr) (get-abox (third expr)))
	     output-string-stream))
    ((init-subscriptions init-subscriptions-1)
     (ok expr stream n state
	 (init-subscriptions-1 (get-abox (third expr)))
	 output-string-stream))
    ((init-publications init-publications-1)
     (ok expr stream n state 
	 (init-publications-1 (get-abox (third expr)))
	 output-string-stream))
    (check-subscriptions
     (answer expr state
	     stream n 
	     (check-subscriptions (get-abox (third expr)))
	     output-string-stream))
    |#
    ((transitive role-is-transitive)
     (ok expr stream n state 
	 (role-is-transitive (second expr) (get-tbox (third expr)))
	 output-string-stream))
    ((reflexive role-is-reflexive)
     (ok expr stream n state 
	 (role-is-reflexive (second expr) (get-tbox (third expr)))
	 output-string-stream))
    ((irreflexive role-is-irreflexive)
     (ok expr stream n state 
	 (role-is-irreflexive (second expr) (get-tbox (third expr)))
	 output-string-stream))
    ((symmetric role-is-symmetric)
     (ok expr stream n state 
	 (role-is-symmetric (second expr) (get-tbox (third expr)))
	 output-string-stream))
    ((asymmetric role-is-asymmetric)
     (ok expr stream n state 
	 (role-is-asymmetric (second expr) (get-tbox (third expr)))
	 output-string-stream))
    ((functional role-is-functional)
     (ok expr stream n state
	 (role-is-functional (second expr) (get-tbox (third expr)))
	 output-string-stream))
    ((inverse inverse-of-role)
     (ok expr stream n state 
	 (inverse-of-role (second expr) (third expr) (get-tbox (fourth expr)))
	 output-string-stream))
    ((domain role-has-domain)
     (ok expr stream n state 
	 (role-has-domain (second expr) (third expr) (get-tbox (fourth expr))
			  (if (null (rest (rest (rest (rest expr)))))
			      t
			    (fifth expr)))
	 output-string-stream))
    ((range role-has-range)
     (ok expr stream n state 
	 (role-has-range (second expr) (third expr) (get-tbox (fourth expr))
			 (if (null (rest (rest (rest (rest expr)))))
			     t
			   (fifth expr)))
	 output-string-stream))
    ((implies-role role-has-parent)
     (ok expr stream n state 
	 (implies-role1 (second expr) (third expr) (get-tbox (fourth expr)))
	 output-string-stream))
    (declare-disjoint 
     (ok expr stream n state 
	 (declare-disjoint (second expr) (get-tbox (third expr)))
	 output-string-stream))
    (otherwise
     (process-racer-expr2 expr stream n state output-string-stream))))

(defun process-racer-expr2 (expr stream n state output-string-stream)
  (case (first expr)
    (get-tbox-language
     (answer expr state
	     stream n 
	     (get-tbox-language (get-tbox (second expr)))
	     output-string-stream))
    (get-abox-language
     (answer expr state
	     stream n 
	     (get-abox-language (get-abox (second expr)))
	     output-string-stream))
    ((get-concept-definition get-concept-definition-1)
     (answer expr state
	     stream n 
	     (get-concept-definition-1 (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((get-concept-negated-definition get-concept-negated-definition-1)
     (answer expr state
	     stream n 
	     (get-concept-negated-definition-1 (second expr) (get-tbox (third expr)))
	     output-string-stream))
    (get-meta-constraint
     (answer expr state
	     stream n 
	     (get-meta-constraint (get-tbox (second expr)))
	     output-string-stream))
    (get-individual-pmodel
     (answer expr state
	     stream n 
	     (get-individual-pmodel (second expr) (get-abox (third expr)))
	     output-string-stream))
    (get-concept-pmodel
     (answer expr state
	     stream n 
	     (get-concept-pmodel (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((role-domain atomic-role-domain)
     (answer expr state
	     stream n 
	     (atomic-role-domain (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((role-range atomic-role-range)
     (answer expr state
	     stream n 
	     (atomic-role-range (second expr) (get-tbox (third expr)))
	     output-string-stream))
    (logging-on
     (setf *tcp-console-logging* t)
     (if (third expr)
	 (setf *debug-racer-server* t))
     (if (second expr)
	 (if *unsafe-mode*
	     (progn
	       (setf *log-file* (second expr))
	       (ok expr stream n state nil output-string-stream))
	   (error "File logging is only allowed if ~A is started in unsafe mode (option -u, do not forget -- under Windows)." (get-product-name)))
       (ok expr stream n state nil output-string-stream)))
    (logging-off
     (setf *tcp-console-logging* nil)
     (setf *log-file* nil)
     (setf *debug-racer-server* nil)
     (ok expr stream n state nil output-string-stream))
    (get-namespace-prefix 
     (answer expr state
	     stream n 
	     (get-namespace-prefix (get-tbox (second expr)))
	     output-string-stream))
    (get-namespace-prefixes 
     (answer expr state
	     stream n 
	     (racer:get-prefixes)
	     output-string-stream))
    (store-tbox-image
     (ok expr stream n state 
	 (if *unsafe-mode* 
	     (store-tbox-image (second expr) (get-tbox (third expr)))
	   (error "Illegal operator store-tbox-image. Only allowed with option -u (do not forget -- under Windows)."))
	 output-string-stream))
    (store-tboxes-image
     (ok expr stream n state 
	 (if *unsafe-mode* 
	     (store-tboxes-image (second expr) (mapcar #'get-tbox (third expr)))
	   (error "Illegal operator store-tboxes-image. Only allowed with option -u (do not forget -- under Windows)."))
	 output-string-stream))
    (store-abox-image
     (ok expr stream n state 
	 (if *unsafe-mode* 
	     (store-abox-image (second expr)
			       (get-abox (third expr)))
	   (error "Illegal operator store-abox-image. Only allowed with option -u (do not forget -- under Windows)."))
	 output-string-stream))
    (store-aboxes-image
     (ok expr stream n state 
	 (if *unsafe-mode* 
	     (store-aboxes-image (second expr) (mapcar #'get-abox (third expr)))
	   (error "Illegal operator store-aboxes-image. Only allowed with option -u (do not forget -- under Windows)."))
	 output-string-stream))
    (store-kb-image
     (ok expr stream n state 
	 (if *unsafe-mode* 
	     (store-kb-image (second expr) 
			     (get-abox (third expr)))
	   (error "Illegal operator store-kb-image. Only allowed with option -u (do not forget -- under Windows)."))
	 output-string-stream))
    (store-kbs-image
     (ok expr stream n state 
	 (if *unsafe-mode* 
	     (store-kbs-image (second expr) (mapcar #'get-abox (third expr)))
	   (error "Illegal operator store-kbs-image. Only allowed with option -u (do not forget -- under Windows)."))
	 output-string-stream))
    (restore-tbox-image
     (ok expr stream n state 
	 (restore-tbox-image (second expr))
	 output-string-stream))
    (restore-tboxes-image
     (ok expr stream n state 
	 (restore-tboxes-image (second expr))
	 output-string-stream))
    (restore-abox-image
     (ok expr stream n state 
	 (restore-abox-image (second expr))
	 output-string-stream))
    (restore-aboxes-image
     (ok expr stream n state 
	 (restore-aboxes-image (second expr))
	 output-string-stream))
    (restore-kb-image
     (ok expr stream n state 
	 (restore-kb-image (second expr))
	 output-string-stream))
    (restore-kbs-image
     (ok expr stream n state 
	 (restore-kbs-image (second expr))
	 output-string-stream))
    (mirror
     (ok expr stream n state 
	 (mirror (second expr) (third expr))
	 output-string-stream))
    (kb-ontologies 
     (answer expr state
	     stream n 
	     (kb-ontologies (second expr))
	     output-string-stream))
    (compute-implicit-role-fillers
     (answer expr state
	     stream n 
	     (compute-implicit-role-fillers (second expr) (get-abox (third expr)))
	     output-string-stream))
    (compute-all-implicit-role-fillers 
     (answer expr state
	     stream n 
	     (compute-all-implicit-role-fillers (get-abox (second expr)))
	     output-string-stream))
    (get-kb-signature
     (answer expr state
	     stream n 
	     (get-kb-signature (second expr))
	     output-string-stream))
    (get-tbox-signature
     (answer expr state
	     stream n 
	     (get-tbox-signature (get-tbox (second expr)))
	     output-string-stream))
    (get-abox-signature
     (answer expr state
	     stream n 
	     (get-abox-signature (get-abox (second expr)))
	     output-string-stream))
    ((symmetric-p symmetric?)
     (answer expr state
	     stream n 
	     (symmetric-p (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((asymmetric-p asymmetric?)
     (answer expr state
	     stream n 
	     (asymmetric-p (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((reflexive-p reflexive?)
     (answer expr state
	     stream n 
	     (reflexive-p (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((irreflexive-p irreflexive?)
     (answer expr state
	     stream n 
	     (irreflexive-p (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((tbox-cyclic-p tbox-cyclic?)
     (answer expr state
	     stream n 
	     (tbox-cyclic-p (get-tbox (second expr)))
	     output-string-stream))
    ((cd-object-p cd-object?)
     (answer expr state
	     stream n 
	     (cd-object-p (second expr) (get-abox (third expr)))
	     output-string-stream))
    ((constraint-entailed-p constraint-entailed?)
     (answer expr state
	     stream n 
	     (constraint-entailed-p (second expr) (get-abox (third expr)))
	     output-string-stream))
    ((atomic-role-synonyms role-synonyms)
     (answer expr state
	     stream n 
	     (atomic-role-synonyms (second expr) (get-tbox (third expr)))
	     output-string-stream))
    ((roles-equivalent roles-equivalent-1)
     (ok expr stream n state 
	 (roles-equivalent-1 (second expr) (third expr) (get-tbox (fourth expr)))
	 output-string-stream))
    ((roles-disjoint roles-disjoint-1)
     (ok expr stream n state 
	 (roles-disjoint-1 (second expr) (third expr) (get-tbox (fourth expr)))
	 output-string-stream))
    (verify-with-concept-tree-list
     (answer expr state
	     stream n 
	     (verify-with-concept-tree-list (second expr) (get-tbox (third expr)))
	     output-string-stream))
    (verify-with-abox-individuals-list
     (answer expr state
	     stream n 
	     (verify-with-abox-individuals-list (second expr) (get-abox (third expr)))
	     output-string-stream))
    (define-tbox
	(ok expr stream n state 
	    (apply #'define-tbox-1 (first (second expr)) :axioms (rest (rest expr)) (rest (second expr)))
	    output-string-stream))
    (define-abox 
	(ok expr stream n state 
	    (apply #'define-abox-1 (first (second expr)) (second (second expr))
		   :axioms (rest (rest expr)) (rest (rest (second expr))))
	    output-string-stream))
    (print-abox-individuals
     (ok expr stream n state 
	 (apply #'print-abox-individuals (rest expr))
	 output-string-stream))
    ((individual-told-datatype-fillers retrieve-individual-told-datatype-fillers)
     (answer expr state
	     stream n 
	     (apply #'retrieve-individual-told-datatype-fillers (rest expr))
	     output-string-stream))
    ((role-equivalent? role-equivalent-p)
     (answer expr state
	     stream n (role-equivalent-p (second expr) (third expr)
					 (get-tbox (fourth expr)))
	     output-string-stream))
    ((role-disjoint? role-disjoint-p)
     (answer expr state
	     stream n (role-disjoint-p (second expr) (third expr)
				       (get-tbox (fourth expr)))
	     output-string-stream))
    ((same-individual-as same-as)
     (ok expr stream n state 
	 (add-same-individual-as-assertion (get-abox nil)
					   (second expr) (third expr))
	 output-string-stream))
    ((add-same-individual-as-assertion)
     (ok expr stream n state 
	 (add-same-individual-as-assertion (get-abox (second expr))
					   (third expr) (fourth expr))
	 output-string-stream))
    ((different-from)
     (ok expr stream n state 
	 (add-different-from-assertion (get-abox nil)
				       (second expr) (third expr))
	 output-string-stream))
    ((add-different-from-assertion)
     (ok expr stream n state 
	 (add-different-from-assertion (get-abox (second expr))
				       (third expr) (fourth expr))
	 output-string-stream))
    ((all-different)
     (ok expr stream n state 
	 (add-all-different-assertion (get-abox nil)
				      (cdr expr))
	 output-string-stream))
    ((add-all-different-assertion)
     (ok expr stream n state 
	 (add-all-different-assertion (get-abox (second expr))
				      (cddr expr))
	 output-string-stream))
    (role-used-as-datatype-property-p 
     (answer expr state
	     stream n (role-used-as-datatype-property-p (second expr) (get-tbox (third expr)))
	     output-string-stream))
    (role-is-used-as-datatype-property
     (ok expr stream n state 
	 (role-is-used-as-datatype-property (second expr) (get-tbox (third expr)))
	 output-string-stream))
    (datatype-role-range
     (answer expr state
	     stream n (datatype-role-range (second expr) (get-tbox (third expr)))
	     output-string-stream))
    (datatype-role-has-range
     (ok expr stream n state 
	 (datatype-role-has-range (second expr) (third expr) (get-tbox (fourth expr)))
	 output-string-stream))
    (set-server-timeout
     (ok expr stream n state 
	 (let ((timeout (second expr)))
	   ;; (check-type timeout 'fixnum)
	   (setf *server-timeout* timeout))
	 output-string-stream))
    (get-server-timeout
     (answer expr state
	     stream n *server-timeout*
	     output-string-stream))
    (with-unique-name-assumption
	(with-unique-name-assumption
	    (loop for expr1 in (rest expr) do
		  (process-racer-expr expr1 stream n state output-string-stream))))
    (without-unique-name-assumption
     (without-unique-name-assumption
      (loop for expr1 in (rest expr) do
	    (process-racer-expr expr1 stream n state output-string-stream))))
    (set-unique-name-assumption
     (answer expr state
	     stream n 
	     (set-unique-name-assumption (second expr))
	     output-string-stream))
    ((individual-synonyms retrieve-individual-synonyms)
     (answer expr state
	     stream n 
	     (apply #'retrieve-individual-synonyms (rest expr))
	     output-string-stream))             
    ((individual-antonyms retrieve-individual-antonyms)
     (answer expr state
	     stream n 
	     (apply #'retrieve-individual-antonyms (rest expr))
	     output-string-stream))             
    (create-tbox-internal-marker-concept
     (answer expr state
	     stream n 
	     (create-tbox-internal-marker-concept
	      (second expr) (third expr))
	     output-string-stream))
    #|
                     (parse-expression
		      (answer expr state
			      stream n 
			      (parse-expression (second expr))
			      output-string-stream))
		     |#
    (role-used-as-annotation-property-p 
     (answer expr state
	     stream n (role-used-as-annotation-property-p (second expr) (get-tbox (third expr)))
	     output-string-stream))
    (role-is-used-as-annotation-property
     (ok expr stream n state 
	 (role-is-used-as-annotation-property (second expr) (get-tbox (third expr)))
	 output-string-stream))
    (add-annotation-concept-assertion 
     (ok expr stream n state 
	 (apply #'add-annotation-concept-assertion (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (add-annotation-role-assertion 
     (ok expr stream n state 
	 (apply #'add-annotation-role-assertion (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (all-annotation-concept-assertions 
     (answer expr state
	     stream n
	     (apply #'all-annotation-concept-assertions (rest expr))
	     output-string-stream))
    (all-annotation-role-assertions 
     (answer expr state
	     stream n 
	     (apply #'all-annotation-role-assertions (rest expr))
	     output-string-stream))
    (with-critical-section
	(let ((last-expr (first (last expr))))
	  (with-racer-critical-section
	      (loop for expr1 in (butlast expr) do
		    (process-racer-expr expr1 stream n t output-string-stream))
	    (process-racer-expr last-expr stream n nil output-string-stream))))
    (racer-user::get-racer-version
     (answer expr state
	     stream n 
	     (get-product-version)
	     output-string-stream))
    (racer-user::get-build-version	; Package-Subtilitaet! 
     (answer expr state
	     stream n 
	     (get-build-version)
	     output-string-stream))
    (attribute-filler
     (ok expr stream n state 
	 (set-attribute-filler (get-abox) (second expr) (third expr) (fourth expr) (fifth expr))
	 output-string-stream))
    (set-attribute-filler
     (ok expr stream n state 
	 (set-attribute-filler (get-abox (second expr)) (third expr) (fourth expr) (fifth expr) (sixth expr))
	 output-string-stream))
    (datatype-role-filler
     (ok expr stream n state 
	 (add-datatype-role-filler (get-abox) (second expr) (third expr) (fourth expr) (fifth expr))
	 output-string-stream))
    (add-datatype-role-filler
     (ok expr stream n state 
	 (add-datatype-role-filler (get-abox (second expr)) (third expr) (fourth expr) (fifth expr) (sixth expr))
	 output-string-stream))
    (add-negative-datatype-role-filler
     (ok expr stream n state 
	 (add-negative-datatype-role-filler (get-abox (second expr)) (third expr) (fourth expr) (fifth expr) (sixth expr))
	 output-string-stream))
    (get-tbox-version
     (answer expr state
	     stream n 
	     (get-tbox-version (find-tbox (second expr)))
	     output-string-stream))
    (get-abox-version
     (answer expr state
	     stream n 
	     (get-abox-version (find-abox (second expr)))
	     output-string-stream))
    (internal-individuals-related-p 
     (answer expr state
	     stream n 
	     (internal-individuals-related-p 
	      (second expr)
	      (third expr)
	      (fourth expr)
	      (find-abox (fifth expr)))
	     output-string-stream))
    (time
     (let ((*timing* (get-internal-run-time))
	   (*timing1* (get-internal-real-time)))
       (process-racer-expr (second expr) stream n state output-string-stream)))
    (transmit-file
     (if *unsafe-mode*
	 (progn
	   (let ((temp-filename (generate-temp-filename (temp-directory) (second expr))))
	     (answer expr state
		     stream n 
		     temp-filename
		     output-string-stream)
	     (read-bytes-into-file stream (third expr) temp-filename)))
       (error "File transmission is only allowed if ~A is started in unsafe mode (option -u, do not forget -- under Windows)." 
	      (get-product-name))))
    (retrieve-individual-annotation-property-fillers 
     (answer expr state
	     stream n 
	     (retrieve-individual-annotation-property-fillers
	      (second expr)
	      (third expr)
	      (get-abox (fourth expr)))
	     output-string-stream))
    (swrl-forward-chaining
     (ok expr stream n state 
	 (apply #'swrl-forward-chaining (rest expr))
	 output-string-stream))
    (prepare-racer-engine 
     (ok expr stream n state 
	 (apply #'prepare-racer-engine (rest expr))
	 output-string-stream))
    (set-rewrite-defined-concepts 
     (ok expr stream n state 
	 (set-rewrite-defined-concepts (second expr))
	 output-string-stream))
    (enable-optimized-query-processing
     (ok expr stream n state 
	 (enable-optimized-query-processing
	  (if (rest expr)
	      (second expr)
	    t))
	 output-string-stream))
    (prepare-abox
     (ok expr stream n state 
	 (prepare-abox (get-abox (second expr)))
	 output-string-stream))                     
    (declare-current-knowledge-bases-as-persistent
     (ok expr stream n state 
	 (declare-current-knowledge-bases-as-persistent)
	 output-string-stream))
    (server-case
     (answer expr state
	     stream n 
	     (server-case)
	     output-string-stream))
    (enable-abduction
     (ok expr stream n state 
	 (enable-abduction (second expr) (third expr))
	 output-string-stream))
    (disable-abduction
     (ok expr stream n state 
	 (disable-abduction)
	 output-string-stream))
    ((retrieve-with-explanation racer-answer-query-with-explanation)
     (answer expr state
	     stream n 
	     (apply #'racer-answer-query-with-explanation (rest expr))
	     output-string-stream))
    (get-role-datatype 
     (answer expr state
	     stream n 
	     (apply #'get-role-datatype (rest expr))
	     output-string-stream))
    (define-rule
	(ok expr stream n state 
	    (apply #'add-rule-axiom (get-abox) (rest expr))
	    output-string-stream))
    (add-rule-axiom 
     (ok expr stream n state 
	 (apply #'add-rule-axiom (get-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (lcs
     (answer expr state
	     stream n 
	     (lcs (second expr) (third expr))
	     output-string-stream))
    (lcs-unfold
     (answer expr state
	     stream n 
	     (lcs-unfold (second expr) (third expr) (get-tbox (fourth expr)))
	     output-string-stream))
    (msc-k
     (answer expr state
	     stream n 
	     (apply #'msc-k (rest expr))
	     output-string-stream))
    ((add-prefix define-prefix)
     (ok expr stream n state 
	 (add-prefix (second expr) (third expr))
	 output-string-stream))
    #+:sonic
    ((ale-lcs)
     (answer expr state
	     stream n 
	     (funcall (intern (symbol-name 'ale-lcs) :sonic) (second expr))
	     output-string-stream))
    #+:sonic
    ((alen-lcs)  
     (answer expr state
	     stream n 
	     (apply #'alen-lcs (rest expr))
	     output-string-stream))
    #+:sonic
    ((alen-approx)	
     (answer expr state
	     stream n 
	     (apply #'alen-approx-to-new-concept (rest expr))
	     output-string-stream))
    #+:sonic
    ((alc-ale-approximation)
     (answer expr state
	     stream n 
	     (funcall (intern (symbol-name 'alc-ale-approximation) :sonic) (second expr))
	     output-string-stream))
    (publish-file
     (ok expr stream n state 
	 (if *unsafe-mode*
	     (publish-file (second expr) (third expr) (fourth expr))
	   (error "Illegal operator publish-file used in ~A. Only allowed with option -u (do not forget -- under Windows)." expr))
	 output-string-stream))		     
    (enable-alisp-compatibility-mode
     #+:mlisp
     (setf *ensure-alisp-socket-compatibility* t)
     (ok expr stream n state nil output-string-stream))		     
    (disable-alisp-compatibility-mode
     #+:mlisp
     (setf *ensure-alisp-socket-compatibility* nil)
     (ok expr stream n state nil output-string-stream))		     
    (abox-consistent-if-assertions-added-p
     (answer expr state
	     stream n 
	     (apply #'abox-consistent-if-assertions-added-p (rest expr))
	     output-string-stream))
    (get-object-bottom-role
     (answer expr state
	     stream n 
	     (get-object-bottom-role (get-tbox (second expr)))
	     output-string-stream))
    (get-data-bottom-role
     (answer expr state
	     stream n 
	     (get-data-bottom-role (get-tbox (second expr)))
	     output-string-stream))
    (forget-annotation-concept-assertion 
     (ok expr stream n state 
	 (apply #'forget-annotation-concept-assertion (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (forget-all-different-assertion
     (ok expr stream n state 
	 (apply #'forget-all-different-assertion (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (forget-same-individual-as-assertion
     (ok expr stream n state 
	 (apply #'forget-same-individual-as-assertion (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (forget-different-from-assertion
     (ok expr stream n state 
	 (apply #'forget-different-from-assertion (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (forget-datatype-role-filler
     (ok expr stream n state 
	 (apply #'forget-datatype-role-filler (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (forget-negative-datatype-role-filler
     (ok expr stream n state 
	 (apply #'forget-negative-datatype-role-filler (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
    (forget-negated-role-assertion
     (ok expr stream n state 
	 (apply #'forget-negated-role-assertion (find-abox (second expr)) (rest (rest expr)))
	 output-string-stream))
		     
    (otherwise 
     (process-nrql-request expr stream n state output-string-stream))))

(defun read-bytes-into-file (in-stream n-bytes temp-filename)
  ;;(read-byte in-stream) We do not skip the first byte anymore.
  (with-open-file (out-stream temp-filename :direction :output :if-does-not-exist :create
                              :element-type 'unsigned-byte)

    (set-progress-100% n-bytes)

    (loop repeat n-bytes
          for byte = (read-byte in-stream)
          for num from 0 by 1
          do (write-byte byte out-stream)
          (set-progress-value num))))

(defun transform-filename (filename)
  filename)

#+:lispworks
(defun send-to-subscriber (message ip port count)
  (with-open-stream (stream
                     (comm:open-tcp-stream 
                      ip port))
    (with-output-to-string (output-string-stream)
      (answer-1 'test nil
                stream count
                message 
                output-string-stream))))


#+:allegro
(defun send-to-subscriber (message ip port count)
  (let ((stream (socket:make-socket :remote-host ip :remote-port port)))
    (with-output-to-string (output-string-stream)
      (answer-1 'test nil
                stream count
                message 
                output-string-stream))))

#+:ccl
(defun send-to-subscriber (message ip port count)
  (let ((stream (ccl:make-socket :remote-host ip :remote-port port)))
    (with-output-to-string (output-string-stream)
      (answer-1 'test nil
                stream count
                message 
                output-string-stream))))

