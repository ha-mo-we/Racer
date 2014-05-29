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
      
(defparameter *indent* 3)
(defvar *visited-urls* nil)

(defun include-kb (pathname)
  (load pathname))

(defun racer-read-file (filename)
  (load filename))

(defun racer-read-document (url-spec &key (verbose *tbox-verbose*))
  #-(and :racer-server (or :aserve :cl-http))
  (declare (ignore url-spec))
  #+(and :racer-server :cl-http)
  (let* ((url-spec (url-convenience-transformation url-spec))
	 (real-url-spec (check-for-url-mirror url-spec))
	 (url (url:intern-url url-spec))
         (*indent* (+ *indent* 3))
         (done (gensym "DONE")))
    (push real-url-spec *visited-urls*)
    (etypecase url 
      (when verbose
        (unless (string= url-spec real-url-spec)
          (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
      (url::file-url (racer-read-file 
                      (make-pathname :directory
				     `(:absolute .,(url:path url))
				     :name (url:object url)
				     :type (url:extension url))))
      (url::http-object 
       (when verbose
	 (unless (string= url-spec real-url-spec)
	   (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
       (with-temp-file ((stream)
                        (http:show-url url :stream stream))
         (let ((*read-eval* nil))
           (loop for expr = (read stream nil done)
	       until (eq expr done) do
                 (eval expr)))))))
  #+(and :racer-server :aserve)
  (let* ((url-spec (url-convenience-transformation url-spec))
	 (real-url-spec (check-for-url-mirror url-spec))
	 (url (net.uri:parse-uri real-url-spec))
	 (done (gensym "DONE"))
	 (*indent* (+ *indent* 3)))
    (push real-url-spec *visited-urls*)
    (ecase (net.uri:uri-scheme url)
      (:file
       (let ((pathname (uri-to-pathname url)))
	 (when verbose
	   (unless (string= url-spec real-url-spec)
	     (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
	 (racer-read-file pathname)))
      (:http
       (when verbose
	 (unless (string= url-spec real-url-spec)
	   (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
		  
       (multiple-value-bind (body res headers)
	   (net.aserve.client:do-http-request url
	     :headers '(("Accept-Encoding" . "gzip"))
	     :format :binary
	     :proxy *proxy*
	     ;; :external-format :latin1-base
	     )
		    
	 #+:allegro 
         (cond ((equal (cdr (assoc :content-encoding headers)) "gzip")
			   
		(when verbose 
		  (format t "HTTP request returned ~s gzip'ed bytes, " (length body)))
			   
		(let ((so (make-string-output-stream))
		      (bis (make-buffer-input-stream body))
		      (bi))
			      
		  (util.zip::skip-gzip-header bis)

		  (setq bi (make-instance 'util.zip:inflate-stream :input-handle bis))
			     
		  (do ((byte (read-byte bi nil nil) (read-byte bi nil nil)))
		      ((null byte)
		       (setq body (get-output-stream-string so))
				  
		       (when verbose
			 (format t "~s characters after inflation~%" (length body))))
			       
		    (write-char (code-char byte) so))))
			   
	       (t (setq body (octets-to-string body))))
         
         #+(or :ccl :lispworks) (setq body (deflate body headers verbose))
		    
	 (cond ((not (eql 200 res))
		(error "HTTP request returned code ~s" res))
               
	       (t 
			   
		(with-input-from-string (stream body)
		  
		  (let ((*read-eval* nil))
		    (loop for expr = (read stream nil done)
			until (eq expr done) do
			  (eval expr))))))))))
  #-(and :racer-server (or :aserve :cl-http))
  (error "Not supported in this version."))

(defmacro import-kb (url-spec)
  `(racer-read-document ,url-spec))
