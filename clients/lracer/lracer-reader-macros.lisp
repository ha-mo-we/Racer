;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: racer -*-

(in-package racer)

;;;
;;;
;;;

(defstruct (racer-boolean 
            (:predicate racer-boolean-p)
            (:conc-name boolean-))
  value)

(defmethod print-object ((object racer-boolean) stream)
  (declare (ignorable stream))
  (if (string= (boolean-value object) "false")
      (write-string "#F" stream)
    (write-string "#T" stream)))

(defparameter *true* (make-racer-boolean :value "true"))

(defparameter *false* (make-racer-boolean :value "false"))

(defmethod make-load-form ((object racer-boolean) &optional env)
  (declare (ignore env))
  (cond ((eq object *true*) '*true*)
        ((eq object *false*) '*false*)))

(defun true-value-reader (stream subchar arg)
  (declare (ignore stream subchar arg))
  *true*)

(defun false-value-reader (stream subchar arg)
  (declare (ignore stream subchar arg))
  *false*)

(defun true? (object)
  (and (racer-boolean-p object)
       (string= (boolean-value object) "true")))

(defun false? (object)
  (and (racer-boolean-p object)
       (string= (boolean-value object) "false")))

(set-dispatch-macro-character #\# #\T 'true-value-reader)

(set-dispatch-macro-character #\# #\F 'false-value-reader)

(set-dispatch-macro-character #\# #\t 'true-value-reader)

(set-dispatch-macro-character #\# #\f 'false-value-reader)

;;;
;;;
;;;

(defun whitespace-char-p (char)
  #+:clisp 
  (system::whitespacep char)
  #+:lispworks
  (lispworks:whitespace-char-p char)
  #+:allegro
  (stream::whitespace-char-p char)
  #+:sbcl
  (find char SB-FORMAT::*FORMAT-WHITESPACE-CHARS*)
  #+(and (not :allegro) (not :lispworks) (not :sbcl) (not :clisp))
  (to-be-implemented 'whitespace-char-p))


(defun resource-reader1 (stream subchar arg)
  (declare (ignore subchar arg))
  
  (let* ((prefix 
          (coerce 
           (loop as 
                 x = (read-char stream nil)
                 while (and x (not (char-equal x #\:)))
                 collect x)
           'string))
         
         (resource 
          (with-lracer-readtable
            (let ((char (peek-char nil stream nil)))
              (when (and char 
                         (not (whitespace-char-p char))
                         (not (char= char #\))))
                (read stream)))))
	 
         (namespace (if (string= prefix "")
                        (get-namespace-prefix (current-tbox))
                      (second (assoc prefix 
				     (get-namespace-prefixes)
                                     :test #'(lambda (x y) (and (not (null x))
                                                                (string-equal x y)))))))
         
	 (namespace 
	  (when namespace
            (let ((n (1- (length  namespace))))
              (if (char= (elt namespace n) #\#)
                  (subseq namespace 0 n)
                namespace)))))
    
    (values ; for CLISP 
     (if resource
        (if namespace
	    (intern (format nil "~A#~A" namespace resource))
          (intern (format nil "~A" resource)))
      (if namespace
          (intern (format nil "~A" namespace))
        nil)))))


(defun resource-reader2 (stream subchar arg)
  (declare (ignore subchar arg))
  
  (let* ((prefix 
          (coerce 
           (loop as 
                 x = (read-char stream nil)
                 while (and x (not (char-equal x #\:)))
                 collect x)
           'string))
         
         (resource 
          (with-lracer-readtable
            (let ((char (peek-char nil stream nil)))
              (when (and char 
                         (not (whitespace-char-p char))
                         (not (char= char #\))))
                (read stream)))))

	 (namespace (if (string= prefix "")
                        (get-namespace-prefix (current-tbox))
                      (second (assoc prefix 
				     (get-namespace-prefixes)
                                     :test #'string-equal))))
	 (namespace 
	  (when namespace
	    (let ((n (1- (length  namespace))))
	      (if (char= (elt namespace n) #\#)
		  (subseq namespace 0 n)
		namespace)))))
    
    (values ; for CLISP 
     (if resource
        (if namespace
	    (format nil "*~A#~A" namespace resource)
          (intern (format nil "*~A" resource)))
      (if namespace
          (intern (format nil "*~A" namespace))
        nil)))))

(set-dispatch-macro-character #\# #\! 'resource-reader1)

(set-dispatch-macro-character #\# #\& 'resource-reader2)

(defmacro enable-lracer-read-macros ()
  `(progn 
     (set-dispatch-macro-character #\# #\! 'resource-reader1)
     (set-dispatch-macro-character #\# #\& 'resource-reader2)    
     (set-dispatch-macro-character #\# #\T 'true-value-reader)
     (set-dispatch-macro-character #\# #\F 'false-value-reader)
     (set-dispatch-macro-character #\# #\t 'true-value-reader)
     (set-dispatch-macro-character #\# #\f 'false-value-reader)))

     
