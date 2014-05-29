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

(defclass xml-nrql 
    (nox:sax-consumer)
  ((expr :accessor expr :initform nil)))

(defparameter +end-tag+ (gensym))

(defmethod nox:start-element ((self xml-nrql) (tag nox:open-tag) mode)
  (declare (ignorable mode))
  
  (let ((token-string (string-upcase (nox:token-string tag)))
        (attributes (nox:tag-attributes tag)))
    
    (with-slots (expr) self
      
      (push (to-keyword token-string) expr)
      
      (let ((value (assoc "value" attributes :test #'string-equal)))
	(when value 
	  (push (string-trim '(#\Space #\Tab #\Newline) 
			     (cdr value))
		expr))))))

(defmethod nox:end-element ((self xml-nrql) tag mode)
  (declare (ignorable mode tag))
  
  (with-slots (expr) self
    
    (push +end-tag+ expr)))

(defmethod nox:char-content ((self xml-nrql) char-content mode)
  (declare (ignorable mode))

  (with-slots (expr) self
    (push (string-trim '(#\Space #\Tab #\Newline) char-content) 
	  expr)))

;;;
;;;
;;;

(defun construct (expr)
  
  (labels ((con ()
	     
	     
	     (cond (expr
		    
		    (let ((op (first expr)))
		      
		      (cond ((eq op +end-tag+)

			     (pop expr)
			     
			     +end-tag+)
			    
			    (t
			     
			     (case op
			       
			       (:LIST
				
				(pop expr)
				
				(let ((res nil))
				  
				  (loop 
				    (let ((res1 
					   (con)))
				      
				      (if (eq res1 +end-tag+)
					  (return)
					(push res1 res))))
				  
				  (nreverse res)))
			       
			       (:STRING
				
				(pop expr)
				(prog1 
				    (pop expr)
				  (pop expr)))
			       
			       (:TRUE
				
				(pop expr)
				(prog1 
				    t
				  (pop expr)))
			       
			       (:NULL
				
				(pop expr)
				(prog1 
				    nil
				  (pop expr)))

			       (otherwise
				
				(pop expr)
				(prog1 
				    (read-from-string (pop expr))
				  (pop expr))))))))
		   
		   (t +end-tag+))))
    
    (con)))


(defun xml-to-lisp (expr)
  (with-input-from-string (stream expr)
    (let ((consumer (make-instance 'xml-nrql)))
      (nox:parse-from-stream stream nil
			     'nox:xml-parser 
			     :consumer consumer)
      
      (construct (reverse (expr consumer))))))


(defun lisp-to-xml (expr &optional (stream t) 
		    &key
                    (header-p t)
		    (use-attributes-p t)
		    (version "1.0")
		    (encoding "ISO-8859-1")
		    describe-p 
		    top-level-attributes
		    include-answer-string-p 
		    indentation-p 
		    (newlines-p indentation-p))
  
  (labels ((do-it (expr i)

             (when indentation-p
               (dotimes (i (* i 2))
                 (format stream " ")))
             
             (typecase expr
               (cons 
                (format stream 
                        (if newlines-p 
                            "<list>~%"
                          "<list>"))
                (mapc #'(lambda (x) (do-it x (1+ i)))
                      expr)
                
                (when indentation-p
                  (dotimes (i (* i 2))
                    (format stream " ")))
                
                (format stream 
                        (if newlines-p 
                            "</list>~%"
                          "</list>")))
               
               (null
                (format stream 
                        (if newlines-p 
                            "<null/>~%"
                          "<null/>")))

               (symbol 
                (cond ((eq expr t)
                       (format stream 
                               (if newlines-p 
                                   "<true/>~%"
                                 "<true/>")))
                      (t
                       (let ((type
                              (string-downcase (symbol-name
                                                (if (or (char= (elt (symbol-name expr) 0) #\?)
                                                        (char= (elt (symbol-name expr) 0) #\$))
                                                    'variable
						  (if (keywordp expr)
						      'keyword
						    'symbol))))))

                         (if use-attributes-p 
                             (format stream 
				     (if newlines-p 
					 "<~A value=\"~A\"/>~%" 
				       "<~A value=\"~A\"/>")
                                     type
                                     expr)
                           
                           (format stream 
				   (if newlines-p 
				       "<~A>~A</~A>~%" 
				     "<~A>~A</~A>")
                                   type
                                   expr
                                   type))))))
               
               (otherwise 
		
                (let ((type (if (stringp expr)
				'string
			      (string-downcase (symbol-name (type-of expr))))))

                  (if use-attributes-p 
                      (format stream 
			      (if newlines-p 
				  "<~A value=\"~A\"/>~%" 
				"<~A value=\"~A\"/>")
                              type
                              expr)
                    
                    (format stream 
			    (if newlines-p 
				"<~A>~A</~A>~%" 
			      "<~A>~A</~A>")
                            type
                            expr
                            type)))))))
    
    (when header-p
      (format stream
              (if newlines-p
                  "<?xml version=\"~A\" encoding=\"~A\" ~A?>~%"
                "<?xml version=\"~A\" encoding=\"~A\" ~A?>")
              version 
              encoding 
              (or top-level-attributes "")))
    
    (let ((expr
	   (if (and describe-p
		    (stringp expr))
	       (read-from-string expr)
	     expr)))
      
      (do-it (if include-answer-string-p 
		 (list (format nil "~S" expr )
		       expr)
	       expr)
	1))))


(defun double-backslashes (string)
  (let ((string (format nil "~S" string)))
    (subseq string 1 (1- (length string)))))


