;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: racer -*-

(in-package racer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (eq (readtable-case *readtable*) :preserve)
    (push :mlisp *features*)))

(pushnew :lracer *features*)

#+(and :sbcl :win32)
(if (string= "1.0.29" (lisp-implementation-version))
    
    ;;; many thanks to Nikodemus Siivola for providing this
    ;;; SBCL patch 
    
    (defun set-default-external-format (external-format)
      (assert (sb-impl::find-external-format external-format))
      (setf sb-impl::*default-external-format* external-format)
      (with-output-to-string (*error-output*)
	(setf sb-sys:*stdin*
	  (sb-sys:make-fd-stream 0 :name "standard input" :input t :buffering :line))
	(setf sb-sys:*stdout*
	  (sb-sys:make-fd-stream 1 :name "standard output" :output t :buffering :line))
	(setf sb-sys:*stderr*
	  (sb-sys:make-fd-stream 2 :name "standard error" :output t :buffering :line))
	(setf sb-sys:*tty* (make-two-way-stream sb-sys:*stdin* sb-sys:*stdout*))
	(princ (get-output-stream-string *error-output*) sb-sys:*stderr*))
      (values))
  (defun set-default-external-format (external-format)
    (declare (ignore external-format))
    (values)))

;;;
;;;
;;; 

(define-condition lracer-error (simple-error)
  ((error
    :initarg :error
    :reader error-error))
  (:report (lambda (condition stream)
             (format stream "~A: ~A"
                     (type-of condition)
                     (error-error condition)))))

(define-condition lracer-connection-error (lracer-error)
  ())

(define-condition racer-error (lracer-error)
  ())

;;;
;;;
;;;

(defparameter +answer-marker+ ":answer")

(defparameter +ok-marker+ ":ok")

(defparameter +error-marker+ ":error")

;;;
;;; Adjust to match your environment: 
;;;

(defparameter *default-racer-host* "127.0.0.1")

(defparameter *default-racer-tcp-port* 8088)

(defparameter *external-format* 

  #+:allegro
  :utf8
  ;; :latin1

  #+:sbcl
  :utf-8
  ;; :latin-1		

  #+:clisp
  :utf-8
    
  #-(or :allegro :sbcl :clisp)
  :ignored

  )

#+(and :sbcl :win32)
(set-default-external-format *external-format*)

;;;
;;;
;;; 

(defparameter *service-request-verbose* nil)

(defparameter *verbose-connection* t)

(defparameter *socket* nil)

(defparameter *keep-alive* t)

;;;
;;;
;;; 

(defparameter *strings* nil)

;;;
;;;
;;;

(defparameter *readtable-copy* (copy-readtable *readtable*))

(defparameter *old-readtable* *readtable*)

(setf (readtable-case *readtable-copy*) :preserve)

(defmacro with-lracer-readtable (&body body)
  `(let ((*readtable* *readtable-copy*))
     ,@body))

(defmacro with-standard-readtable (&body body)
  `(let ((*readtable* *old-readtable*))
     ,@body))

;;;
;;;
;;;

(defparameter *with-macro-stack* nil)

(defun remove-rest-etc (lambda)
  (mapcar #'(lambda (x) 
              (if (consp x) 
                  (first x)
                x))
          (progn 
            (dolist (rem '(&rest &key &args &body &whole &allow-other-keys &whole))
              (setf lambda (remove rem lambda)))
            lambda)))

(defmacro declare-with-macro (name &optional signature)
  (if signature
      (let ((signature 
             (mapcar #'(lambda (x) 
                         (list x nil (intern (format nil "~A-SUPPLIED-P" x))))
                     (remove-rest-etc signature))))
        `(defmacro ,name ((&key ,@signature) &body body)
           (list* 
            'let 
            (list 
             (list 
              '*with-macro-stack* 
              (list 'cons (list 'list (quote ',name) 
                                (list 'quote
                                      (remove nil 
                                              (append ,@(mapcar
                                                         #'(lambda (arg)
                                                             (let ((arg (first arg)))
                                                               `(when ,(intern (format nil "~A-SUPPLIED-P" arg))
                                                                  (list ,(intern (symbol-name arg)
                                                                                 (find-package :keyword))
                                                                        ,arg))))
                                                         signature)))))
                    '*with-macro-stack*)))
            body)))
    `(defmacro ,name (&body body)
       (list* 
        'let 
        (list 
         (list 
          '*with-macro-stack* 
          (list 'cons (list 'list (quote ',name))
                '*with-macro-stack*)))
        body))))

;;;
;;;
;;;

(defun open-socket (host port 
		    &key
		    (external-format *external-format*))
  
  (declare (ignorable external-format))
  
  #+:allegro
  (let ((external-format (excl:find-external-format external-format))
        (socket (socket:make-socket :remote-port port :remote-host host)))
    (setf (stream-external-format socket) external-format)
    socket)

  #+:lispworks
  (comm:open-tcp-stream host port)
  
  #+:clisp
  (socket:socket-connect port host)

  #+:sbcl
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
		  :external-format *external-format*
		  :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket 
				   (sb-bsd-sockets:host-ent-address
				    (sb-bsd-sockets:get-host-by-name host))
				   port
				   :external-format *external-format*
					      )

    (let ((stream 
	   (sb-bsd-sockets:socket-make-stream socket 
					      :external-format *external-format*
					      :input t :output t)))
      stream
      )))

(defun close-socket (socket)
  (when (open-stream-p *socket*)
    (close socket))
  (setf *socket* nil))

(defun open-server-connection (&key (host *default-racer-host*)
                                    (port *default-racer-tcp-port*))
  
  (close-server-connection)
  (setf *socket* (open-socket host port))
  
  *socket*)

(defun close-server-connection ()
  (when *socket*
    (when (open-stream-p *socket*)
      (princ ":eof" *socket*)
      (terpri *socket*))
    (close-socket *socket*)))

(defmacro maybe-print-results (&body forms)
  `(progn .,forms))


(defmacro with-server-connection ((&key tbox abox
                                        (host *default-racer-host*)
                                        (port *default-racer-tcp-port*))
                                  &body forms)
  `(let ((*keep-alive* t))
     
     (open-server-connection :host ,host
			     :port ,port)
     
     (unwind-protect
	 (progn ,@(if tbox `((in-tbox ,tbox :init nil)) nil)
           ,@(if abox `((in-abox ,abox)) nil)
           (maybe-print-results .,forms))
       (close-server-connection))))

;;;
;;;
;;; 

#-:clisp
(defmacro with-standard-io-syntax-1 (&body body)
  (let ((package-sym (gensym)))
    `(let ((,package-sym *package*))
       (with-standard-io-syntax 
         (let ((*package* ,package-sym))
           .,body)))))

#-:clisp 
(defun myformat (stream string &rest args)
  (apply #'format stream string args))


;;;
;;; Prevent CLisp from adding package prefixes... 
;;; 

#+:clisp
(defmacro with-standard-io-syntax-1 (&body body)
  `(let 
       ((*print-array*                t)
        (*print-base*                 10)
        (*print-case*                 :upcase)
        (*print-circle*               nil)
        (*print-escape*               t)
        (*print-gensym*               t)
        (*print-length*               nil)
        (*print-level*                nil)
        (*print-lines*                nil)
        (*print-miser-width*          nil)
        (*print-pretty*               nil)
        (*print-radix*                nil)
        (*print-readably*             nil ; t 
                                      )
        (*print-right-margin*         nil)
        (*read-base*                  10)
        (*read-default-float-format*  'single-float)
        (*read-eval*                  t)
        (*read-suppress*              nil))

     ,@body))

#+:clisp 
(defun myformat (stream string &rest args)
  ;; prevent CLisp from adding package prefixes
  ;; ugly, but what can be done? 
  (let ((args 
         (mapcar #'(lambda (x)
                     (typecase x
                       (keyword x)
                       (symbol 
                        (intern (symbol-name x)))
                       (otherwise x)))
                    args)))
    (apply #'format stream string args)))

;;;
;;;
;;;

(defun enable-alisp-compatibility-mode ()
 (declare (ignorable ))
  (with-standard-io-syntax-1
    (let ((req-string
           (concatenate 'string 
      "("
      "enable-alisp-compatibility-mode"
      ")" )))

      (service-request req-string))))

(defun disable-alisp-compatibility-mode ()
 (declare (ignorable ))
  (with-standard-io-syntax-1
    (let ((req-string
           (concatenate 'string 
      "("
      "disable-alisp-compatibility-mode"
      ")" )))

      (service-request req-string))))

(defun get-racer-version ()
 (declare (ignorable ))
  (with-standard-io-syntax-1
    (let ((req-string
           (concatenate 'string 
      "("
      "get-racer-version"
      ")" )))

   (service-request req-string))))

(defun check-for-alisp ()
  #-:mlisp
  (handler-case 
      (enable-alisp-compatibility-mode)
    (error (error)
      (with-standard-readtable
        (error 'lracer-error
               :error
               (format nil "~A = Sorry, this version of LRacer does not work with RacerPro ~A." 
                       error
                       (handler-case
                           (get-racer-version)
                         (error ()
                           "<= 1.9.1 Beta"))))))))

;;;
;;;
;;; 

(defun extract-string (message start) 
  (let ((end (find-end-position message)))
    (values (transform (subseq message (1+ start) end))
            (1+ end))))

(defun find-end-position (message)
  (let* ((pos1 (position #\" message :from-end t))
         (pos2 (position #\" message :end pos1 :from-end t)))
    (position #\" message :end pos2 :from-end t)))

(defun service-request (message &key (host *default-racer-host*) 
                                (port *default-racer-tcp-port*)
                                (sna-error-p t)
                                (sna-value ':no-connection))

 
  (dolist (with-macro *with-macro-stack*)
    (setf message
          (let ((name (first with-macro))
                (args (second with-macro)))
            (if args 
                (myformat nil "(~A ~S ~A)" 
			  name 
			  args
			  message)
              (myformat nil "(~A ~A)" 
			name
			message)))))

  (with-standard-io-syntax-1
   (cond ((null *socket*)
          (open-server-connection :host host
                                  :port port)
          (if (null *socket*)
              (if sna-error-p
                  (error 'lracer-connection-error 
                         :error "LRacer cannot connect to Racer.")
                sna-value)
            (progn 
              (check-for-alisp)
              (unwind-protect
                  (service-request message)
                (unless *keep-alive*
                  (close-server-connection))))))
	       
         (t (when *service-request-verbose*
              (format t "~A~%" message))
	    
	    (format *socket* "~A" message)
            (terpri *socket*)
	    
            (force-output *socket*)
            (parse-racer-reply (read-line-1 *socket*))))))


(defun parse-racer-reply (message)
  (let* ((answer (search +answer-marker+ message))
         (ok (search +ok-marker+ message))
         (error (search +error-marker+ message)))

    (when (eq *package* (find-package :racer))
      (error 'lracer-error 
             :error "LRacer does not work correctly if *package* = RACER. Please use *package* = RACER-USER instead."))

    (with-lracer-readtable

      (cond (answer
             (multiple-value-bind (number pos-1)
                 (read-from-string message t nil
                                   :start (length +answer-marker+))
               (declare (ignore number))
               (multiple-value-bind (result pos-2)
                   (extract-string message pos-1)
                 (let* ((final-result
                         (transform-result 
                          (read-from-string result)))
			    
                        (message
                         (when *verbose-connection*
                           (nsubstitute #\Newline #\Tab
                                        (read-from-string 
                                         (nsubstitute #\/ #\\ message)
                                         t nil :start pos-2)))))
			
                   (when (and *verbose-connection* 
                              (not (string= message "")))
                     (format t "Racer Message (STDOUT): ~A~%~%"
                             message))

                   final-result))))
		
            (ok 
             (multiple-value-bind (number pos-1)
                 (read-from-string message t nil 
                                   :start (length +ok-marker+))
               (declare (ignore number))
               (let ((message
                      (when *verbose-connection*
                        (nsubstitute #\Newline #\Tab
                                     (read-from-string 
                                      (nsubstitute #\/ #\\
                                                   message)
                                      t nil :start pos-1)))))
                 (when (and *verbose-connection* 
                            (not (string= message "")))
		   
                   (format t "Racer Message (STDOUT): ~A~%~%"
                           message))
                     
                 t)))
		
            (error
             (multiple-value-bind (number pos-1)
                 (read-from-string message t nil 
                                   :start (length +error-marker+))
               (declare (ignore number))
               (with-standard-readtable
                 (error 'racer-error :error (nsubstitute #\- #\~ (subseq message pos-1))))))
            
            (t (close-socket *socket*)
               (with-standard-readtable
                 (error 'lracer-connection-error
                        :error
                        (format nil "Unknown reply message format from Racer: ~A. ~
                         Probably the server connection is not set up properly." message))))))))


(defun transform (string)
  (let* ((length (length string))
         (result (make-array length :adjustable t :element-type 'character :fill-pointer 0)))
    (loop with skip = nil
          for i from 0 below length 
          for ch = (aref string i) do
          (if skip
              (setf skip nil)
            (if (char= ch #\\)
                (if (< i (1- length))
                    (let ((next-ch (aref string (1+ i))))
                      (cond ((char= next-ch #\")
                             (setf skip t)
                             (vector-push-extend #\" result))
                            ((char= next-ch #\S)
                             (setf skip t)
                             (vector-push-extend #\\ result)
                             (vector-push-extend #\" result))
                            ((char= next-ch #\N)
                             (setf skip t)
                             (vector-push-extend #\Newline result))
                            ((char= next-ch #\\)
                             (setf skip t)
                             (vector-push-extend #\\ result)
                             (vector-push-extend #\\ result))
                            ((char= next-ch #\|)
                             (setf skip t)
                             (vector-push-extend #\| result))
                            (t (error 'lracer-error :error 
                                      (format nil "Unknown escape sequence found in ~A." string)))))
                  (vector-push-extend ch result))
              (vector-push-extend ch result))))
    result))

(defun read-line-1 (input-stream &optional eof-value)
  (let ((line nil))
    
    (loop for char = (read-char input-stream nil t) 
	do 
          (cond ((eq char #\<)

                 ;;; Unreadable Object? 
                 ;;; Read until  #\> 
                 ;;; and remove #<....> 

                 (pop line)

                 ;;; convert to string

                 (push #\" line)

                 (loop for char = (read-char input-stream nil t)
                       do 
                       (progn 
                         (cond ((char= char #\>)
                                ;;; continue
                                (push #\" line)
                                (return))
                               ((eq char t)
                                (return-from read-line-1 eof-value))
                               (t (push char line))))))

                ((eq char 't)
                 (return-from read-line-1 eof-value))

                ((or (char= char #\Linefeed)
                     (char= char #\Newline)
                     (char= char #\Return))
		 
		 (cond (line
			#+:ignore
			(push (coerce (reverse line) 'string) *strings*)
			(return-from read-line-1 (coerce (reverse line) 'string)))
		       (t 
			;;; due to CR/LF if RacerPro uses different EOL convention
			;;; (e.g., SBCL = latin-1-unix, RacerPro = latin-1-windows) 
			;;; consume "blank lines" produced by additional CR or LF 
			;; until "real" input was read 
			(setf line nil)))
		 
		 '(return-from read-line-1 (coerce (reverse line) 'string)
		   ))
		
                (t
                 (push char line))))))


(defun transform-result (s-expr)

  (cond ((null s-expr)
         nil)
	
        ((consp s-expr)
         (mapcar #'transform-result s-expr))
	
        ((symbolp s-expr)

         (let ((name 
                (symbol-name s-expr)))
         
           (cond ((eq (symbol-package s-expr) 
                      (find-package "RACER-STRING"))
                  
                  name)

                 (t 

                  #-:mlisp 

                  (cond ((keywordp s-expr)
                         (intern (string-upcase name) :keyword))

                        ((symbolp s-expr)

                         (multiple-value-bind (sym foundp)
                             (find-symbol name :racer)
                           
                           (if (member foundp '(:external :inherited))
                               
                               sym

                             (let ((uname (string-upcase name)))

                               (multiple-value-bind (sym foundp)
                                   (find-symbol uname :racer)
                             
                                 (if  (member foundp '(:external :inherited))
                                     sym
                                   s-expr))))))

                        (t s-expr))
                    
                  #+:mlisp 
                  
                  s-expr))))
	
        (t s-expr)))

;;;
;;;
;;;

(defun transform-s-expr (s-expr)
  ;;; adjust if you need some additional conversions
  ;;; on the S-Expressions before transmitting them
  ;;; to Racer! 

  s-expr)
