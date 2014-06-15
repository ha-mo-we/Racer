;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWL-SYNTAXES; Base: 10 -*-

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

(in-package :owl-syntaxes)

;;;
;;;;  http-stream.lisp
;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   The Original Software is 
;;;   OntoLisp (NOSA): A Semantic Web Framework for OWL 2 and OWLlink in Common Lisp
;;;
;;;   Copyright (c) 2007-2010 Michael Wessel and Racer Systems GmbH & Co. KG 
;;;   All Rights Reserved.
;;;
;;;   Contributor(s): Michael Wessel  (mailto:michael_wessel@gmx.de
;;;                                    mailto:wessel@racer-systems.com) 
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   Purpose: Opens and provides an HTTP stream for a given URL from which we can read.
;;;            

(defun clean-url (url)
  (if url 
      (let ((pos (position #\space url)))
	(if pos
	    (concatenate 'string
	      (subseq url 0 pos)
	      "%20"
	      (clean-url (subseq url (1+ pos))))
	  url))
    ""))


(defun without-file-prefix (url)
  (let ((res (search "file://" url)))
    (if (and res
             (zerop res))
        (subseq url 7)
      url)))

#+:aserve
(defmacro with-input-from-url ((stream url &key close-manually-p (verbose *tbox-verbose*)) &body body)
  (let ((orig (gensym)))

    `(let* ((,orig (if (symbolp ,url)
		       (symbol-name ,url)
		     ,url))
	    (,url 
	     #+:racer-server
	     (check-for-url-mirror ,orig)
	     #-:racer-server
	     ,orig))

       #+:racer-server
       (unless (equalp ,orig ,url)
         (when ,verbose
           (format t "~V@TURL ~A mirrored to ~A." *indent* ,orig ,url)))                    
     
       ,(if close-manually-p
	    `(if (owlapi:is-file-url-p ,url)
		 
		 (let ((,stream 
			(open (without-file-prefix ,url)
			      :direction :input)))
		   ,@body)
	       
	       (break "not yet supported"))

	  `(if (owlapi:is-file-url-p ,url)
	
	       (with-open-file (,stream (without-file-prefix ,url))
		 ,@body)
    
	     (multiple-value-bind (body res headers)
		 (let ((,url 
			(clean-url ,url)))
		   (net.aserve.client:do-http-request ,url
		     #+:racer-server
		     :proxy 
		     #+:racer-server
		     *proxy*
                     :headers '(("Accept-Encoding" . "gzip"))
		     :format :binary
		     ;; :external-format :latin1-base
		     ))
		    
	       #+:allegro 
               (cond ((equal (cdr (assoc :content-encoding headers)) "gzip")

		      #+:racer-server
		      (when *tbox-verbose*
			(format t "HTTP request returned ~s gzip'ed bytes, " (length body)))
			   
		      (let ((so (make-string-output-stream))
			    (bis (excl:make-buffer-input-stream body))
			    (bi))
			      
			(util.zip:skip-gzip-header bis)

			(setq bi (make-instance 'util.zip:inflate-stream :input-handle bis))
			     
			(do ((byte (read-byte bi nil nil) (read-byte bi nil nil)))
			    ((null byte)
			     (setq body (get-output-stream-string so))
				  
			     #+:racer-server
			     (when *tbox-verbose*
			       (format t "~s characters after inflation~%" (length body))))
			       
			  (write-char (code-char byte) so))))
			   
		     (t (setq body (excl:octets-to-string body))))
		    
               #+(or :ccl :lispworks) (setq body (deflate body headers ,verbose))
		    
	       (cond ((not (eql 200 res))
		      (error "HTTP request returned code ~s" res))
			  
		     (t 
		      (with-input-from-string (,stream body)
			,@body)))))))))

#+:cl-http
(defmacro with-input-from-url ((stream url &key close-manually-p) &body body)
  `(let* ((url-spec (if (symbolp ,url)
                        (symbol-name ,url)
                      ,url))
          (real-url-spec (check-for-url-mirror url-spec))
          (url (url:intern-url real-url-spec))
          (headers nil))

     (declare (ignorable headers url))

     (unless (equalp url-spec real-url-spec)
       (when *tbox-verbose*
         (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
     
     ,(if close-manually-p
          `(if (owlapi:is-file-url-p real-url-spec)
	
               (let ((,stream 
                      (open (without-file-prefix real-url-spec)
                            :direction :input)))
                 ,@body)

             (break "not yet supported"))

        `(if (owlapi:is-file-url-p real-url-spec)
	
             (with-open-file (,stream (without-file-prefix real-url-spec))
               ,@body)

           (progn 
             (when *tbox-verbose*
               (unless (string= url-spec real-url-spec)
                 (format t "URL ~A mirrored to ~A." url-spec real-url-spec)))

             (with-temp-file ((,stream)
                                     (http:show-url url :stream stream :headers headers))
               ,@body))))))

#+(and (not :aserve) (not :cl-http))
(defmacro with-input-from-url ((stream url &key close-manually-p) &body body)
  (declare (ignorable close-manually-p))
  `(if (owlapi:is-file-url-p ,url)
       (let ((,stream 
              (open (without-file-prefix ,url)
                    :direction :input)))
         ,@body)
     #|(let ((string 
            (s-http-client:do-http-request ,url)))
       ;; (pprint string)
       (with-input-from-string (,stream string)
         ,@body))|#
       (error "Cannot access a URL from this Lisp.")))
