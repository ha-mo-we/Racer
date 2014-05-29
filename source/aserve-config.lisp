;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

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

;;; (setf net.aserve:*http-response-timeout* 0)

#-:allegro
(defun deflate (body headers verbose)
  (cond ((or (string-equal (cdr (assoc :content-encoding headers)) "gzip")
             (string-equal (cdr (assoc :content-type headers)) "application/x-gzip"))
         
         (when verbose 
           (format t "HTTP request returned ~s gzip'ed bytes, " (length body)))
         
         
         (let ((output-stream (flex:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
           (deflate:inflate-gzip-stream (flex:make-in-memory-input-stream body)
                                        output-stream)
           (flex:octets-to-string (flex:get-output-stream-sequence output-stream))))
        (t (flex:octets-to-string body))))

(defun enable-racer-server (port &key console-logging-p)
  (setf net.aserve::*enable-logging* console-logging-p)
  (net.aserve:start :port port)
  (net.aserve:publish 
   :timeout 0
   :path "/"
   :content-type "text/html"
   :function #'(lambda (req ent)
		 (if (eq (net.aserve:request-method req) ':post)
		     (cond (racer:*use-owllink-interface*
                            (racer:owllink-eval-request req ent))
                           (racer:*use-owllink2-interface*
                            (racer:owllink2-eval-request req ent))
                           (t
                            (racer:dig-eval-request req ent)))
		   (net.aserve:with-http-response (req ent)
		     (net.aserve:with-http-body (req ent)
		       (net.html.generator:html 
			(:head 
			 (:title 
			  (:princ
			   (format nil 
				   "Racer ~A: http://racer.sts.tuhh.de"
				   *dl-prover-version*))))
			(:html
			 (:body
			  "<a href=\"http://racer.sts.tuhh.de\">Racer<a> supports only the post method.")))))))))


(defun publish-file (filename url content-type)
  (unless (member content-type '("text/html"
				 "text/xml"
				 "text/plain")
		  :test #'string=)
    (racer:racer-warn "Unknown mime type ~A. Using text/plain" content-type)
    (setf content-type "text/plain"))
  (net.aserve:publish-file 
   :path url
   :file filename
   :content-type content-type))


