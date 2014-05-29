;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: net.aserve; Base: 10 -*-

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

(in-package :net.aserve)

(defun binary-get-request-body (req)
  ;; get the guts of the body into an array of bytes
  (let ((original-ef (stream-external-format (request-socket req))))
    
    ; must read using the octets external format because the 
    ; content length is in terms of octets
    (setf (stream-external-format (request-socket req))
      (find-external-format :octets))
    
    (unwind-protect
	(if* (member (request-method req) '(:put :post))
	   then (multiple-value-bind (length believe-it)
		    (header-slot-value-integer req :content-length) 
		  (if* believe-it
		     then	; we know the length
			  (prog1 (let ((ret (make-array length 
							:element-type '(unsigned-byte 8))))
				   (read-sequence-with-timeout 
				    ret length 
				    (request-socket req)
				    *read-request-body-timeout*))
	    
			    ; netscape (at least) is buggy in that 
			    ; it sends a crlf after
			    ; the body.  We have to eat that crlf.
			    ; We could check
			    ; which browser is calling us but it's 
			    ; not clear what
			    ; is the set of buggy browsers 
			    (let ((ch (read-char-no-hang
				       (request-socket req)
				       nil nil)))
			      (if* (eq ch #\return)
				 then ; now look for linefeed
				      (setq ch (read-char-no-hang 
						(request-socket req)
						nil nil))
				      (if* (eq ch #\linefeed)
					 thenret 
					 else (unread-char 
					       ch (request-socket req)))
			       elseif ch
				 then (unread-char ch (request-socket
						       req)))))
		   elseif (equalp "chunked" (header-slot-value req
							       :transfer-encoding))
		     then ; chunked body
			  (socket:socket-control (request-socket req)
						 :input-chunking t)
			  
			  (with-timeout-local
			      (*read-request-body-timeout* nil)
			    (let ((ans (make-array 
					2048 
					:element-type 'character
					:fill-pointer 0))
				  (sock (request-socket req))
				  (ch))
			      (handler-case 
				  (loop (if* (eq :eof 
						 (setq ch
						   (read-char 
						    sock nil :eof)))
					   then ; should never happen
						(return  ans)
					   else (vector-push-extend
						 ch ans)))
				(excl:socket-chunking-end-of-file
				    (cond)
				  (declare (ignore cond))
				  (socket:socket-control (request-socket req)
							 :input-chunking nil)
				  ans))))

			  
		     else	; no content length given
			  
			  (if* (equalp "keep-alive" 
				       (header-slot-value req
							  :connection))
			     then ; must be no body
				  ""
			     else ; read until the end of file
				  (with-timeout-local
				      (*read-request-body-timeout* 
				       nil)
				    (let ((ans (make-array 
						2048 
						:element-type 'character
						:fill-pointer 0))
					  (sock (request-socket req))
					  (ch))
				      (loop (if* (eq :eof 
						     (setq ch
						       (read-char 
							sock nil :eof)))
					       then (return  ans)
					       else (vector-push-extend
						     ch ans))))))))
	   else ""		; no body
		)
      ; uwp cleanup
      (setf (stream-external-format (request-socket req)) original-ef)
      )))

