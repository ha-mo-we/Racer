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
;;;;  owllink-xml.lisp
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
;;;   Purpose: The implementation of the OWLlink XML and OWLlink S-Expression processor. 
;;; 

(defvar *my-owllink2-parser* nil)

(defvar *error-indicator* nil)

(defvar *response-end-tag* nil)

(defvar *owllink-readtable* (copy-readtable *readtable*))

#+(and :racer-server :cl-http)
(declaim (special *tcp-console-logging* *log-file*))

;;;
;;;
;;;

;;; Functional Parser accepts (SubClassOf A B), not (SubClassOf NIL A B)
;;; However, OWLlink internal message syntax uses mandatory empty argument list: 
;;; (SubClassOf NIL A B)
;;; For converter mode required to set to t! 

(defparameter *use-empty-argument-lists-for-owl2-tags-in-sexpr-syntax-input* nil)

;;;
;;;
;;;

(define-constant +owllink-namespace+ "http://www.owllink.org/owllink-xml#") ; Prefix "ol:"

(define-constant +owllink-new-namespace+ "http://www.owllink.org/owllink#") ; Prefix "ol:"

(define-constant +racer-extension-namespace+ "http://www.racer-systems.com/owllink/ext/racer#") ; Prefix "ra:"

(define-constant +retraction-extension-namespace+ "http://www.owllink.org/ext/retraction#") ; Prefix "ret:" 

;;;
;;;
;;;

#|

(defpackage |http://www.owllink.org/owllink|
  (:use :cl)
  (:nicknames |ol|)
  (:export "Tell"
           "Prefix"
           "name"
           "kb"
           "fullIRI"))

(defpackage |http://www.w3.org/2002/07/owl|
  (:use :cl)
  (:nicknames |owl|)
  (:export "SubClassOf"))

(defpackage |http://www.owllink.org/ext/retraction|
  (:use :cl)
  (:nicknames |ret|)
  (:export "Retract"))

(defpackage |http://www.racer-systems.com/owllink/ext/racer#Racer|
  (:use :cl ; :racer
   )
  (:nicknames |racer|)
  (:export "Racer"))

|#

;;;
;;;
;;; 

(defclass owllink2-parser 
  (nox:sax-consumer)
  ((url :initarg :url :accessor parser-url)
   (stream :initarg :stream :accessor parser-stream)
   (output-stream :initarg :output-stream :accessor parser-output-stream)

   (result :accessor result :initform nil)
   (answer :accessor answer :initform nil)
   (raw-input :accessor raw-input :initform nil)))

#+:racer-server
(defun owllink2-eval-request (req ent)
  (with-racer-critical-section
      ;; (racer:new-request "Evaluating OWLlink2 request...")
    (set-progress-value 1)
    (let ((*error-indicator* "SyntaxError error"))
      (with-output-to-string (*standard-output*)
        (owllink2-eval-request1 'owllink2-parser req ent)))
    (clear-request)))

#|

(defun transform-functional-input (sexpr &optional axiom-level-p)
  (if (consp sexpr)
      (let ((op (first sexpr)))
        (if (symbolp op)
            (let ((axiom-level-p 
                   (or axiom-level-p 
                       (member op '(|Tell| |Retract|)))))
              (if axiom-level-p 
                  `(,op 
                    ,@(mapcar #'(lambda (x) 
                                  (transform-functional-input x t))
                              (rest sexpr)))
                (multiple-value-bind (attributes rest)
                    (loop as x in (cdr sexpr)
                          when (and (consp x)
                                    (eq (first x) '|Attribute|))
                          collect x into attributes
                          else collect x into others
                          finally (return (values attributes others)))
                  `(,op ,(mapcar #'(lambda (x) 
                                     (cdr x))
                                 attributes)
                        ,@(mapcar #'transform-functional-input rest)))))
          (mapcar #'transform-functional-input sexpr)))
    sexpr))

|#


(defun transform-functional-input (sexpr)
  (labels ((remove-owl-prefix (sym)
             (let* ((name (symbol-name sym))
                    (num (and name (search "owl." name))))
               (if (and num (zerop num))
                   (intern (subseq name 4))
                 sym))))
    (if (consp sexpr)
        (let ((op (first sexpr)))
          (if (symbolp op)
              (let ((owl2-tag-p 
                     (is-owl2-tag-p op)))
                (if owl2-tag-p
                    (if *converter-mode*
                        `(,op nil
                              ,@(mapcar #'transform-functional-input 
                                        (rest sexpr)))
                      `(,(remove-owl-prefix op)
                        ,@(mapcar #'transform-functional-input 
                                  (rest sexpr))))
                  (multiple-value-bind (attributes rest)
                      (loop as x in (cdr sexpr)
                            when (and (consp x)
                                      (eq (first x) '|Attribute|))
                            collect x into attributes
                            else collect x into others
                            finally (return (values attributes others)))
                    `(,op ,(mapcar #'(lambda (x) 
                                       (cdr x))
                                   attributes)
                          ,@(mapcar #'transform-functional-input rest)))))
            (mapcar #'transform-functional-input sexpr)))
      sexpr)))

(defun is-owl2-tag-p (op)
  (cond ((member op '(|owl.Literal| |OWLLiteral|)) t)
        ((member op '(|ol.Literal| |Literal|)) nil)
        (t 
         (gethash 
          (let ((*last-request-xml-and-functional-namespaces* 
                 +owl2-standard-prefixes+))
            (expand-functional-tag-name op))
          *owl2-tags-input*))))

(defun transform-sexpr-input (sexpr)
  (labels ((remove-owl-prefix (sym)
             (let* ((name (symbol-name sym))
                    (num (and name (search "owl." name))))
               (if (and num (zerop num))
                   (intern (subseq name 4))
                 sym))))

    (cond ((consp sexpr)
           (let ((op (first sexpr)))
             (cond ((symbolp op)
                    (if (is-owl2-tag-p op)
                        (let* ((op (if *converter-mode* 
                                       op
                                     (remove-owl-prefix op)))
                               (res 
                                ;; (format t "~% owl2 ~A" op)
                                ;; (SubClassOf NIL A B) oder (SubClassOf A B) im Input?
                                ;; -> map to (SubClassOf A B), since OWL2 Functional Parser
                                ;; doesnt accept attributes 
                                (if *use-empty-argument-lists-for-owl2-tags-in-sexpr-syntax-input*
                                    `(,op ,@(mapcar #'transform-sexpr-input (cddr sexpr)))
                                  `(,op ,@(mapcar #'transform-sexpr-input (cdr sexpr))))))
                          (if *converter-mode*
                              (list* (first res)
                                     nil
                                     (rest res))
                            res))
                      (let* ((attributes 
                              (second sexpr))
                             (attributes 
                              (cond ((eq attributes '|nil|)
                                     nil)
                                    ((eq attributes '|t|)
                                     t)
                                    (t attributes))))
              
                        `(,op ,(loop as key in attributes by #'cddr
                                     as val in (cdr attributes) by #'cddr
                                     when (keywordp key) 
                                     collect (list (intern (symbol-name key)) ; no Keyword arguments internally
                                                   val))

                              ,@(mapcar #'transform-sexpr-input (cddr sexpr))))))
                   (t
                    (mapcar #'transform-sexpr-input sexpr)))))

          ((symbolp sexpr)
           (cond ((eq sexpr '|nil|) nil)
                 ((eq sexpr '|t|) t)
                 (t sexpr)))
		 
          (t sexpr))))

(defmacro process-owllink-request ()
  `(progn
     (case *owllink2-output-syntax* 
       (:owllink-xml
        (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" socket-stream)))

     (#+:racer-server with-racer-critical-section
      #-:racer-server progn
       
       (#+:racer-server with-logging #+:racer-server (socket-stream)
        #-:racer-server progn

         (handler-case
             (progn 

               (ecase *owllink2-input-syntax*
                 (:owllink-xml
                                    
                  (nox:parse-from-stream 
                   stream url 
                   'nox:xml-parser 
                   :consumer 
                   (setf *my-owllink2-parser*
                         (make-instance parser-class 
                                        :url url
                                        :stream socket-stream)))

                  (let ((res
                         (setf 
                          (answer *my-owllink2-parser*)
                          (if *converter-mode* 
                              (result *my-owllink2-parser*)
                            (|owllink-parse-RequestMessage| 
                             (result *my-owllink2-parser*))))))
                    
                    (owllink-result-printer res socket-stream)))

                 (:owllink-functional

                  (setf *my-owllink2-parser*
                        (make-instance parser-class 
                                       :url url
                                       :stream socket-stream))

                  (setf (result *my-owllink2-parser*)
                        (transform-functional-input
                         (setf (raw-input *my-owllink2-parser*)
                               (parse-from-stream stream))))

                  (let ((res
                         (setf 
                          (answer *my-owllink2-parser*)
                          (if *converter-mode*
                              (result *my-owllink2-parser*)
                            (|owllink-parse-FunctionalRequestMessageWithHeader|
                             (result *my-owllink2-parser*))))))
                    
                    (owllink-result-printer res socket-stream)))

                 (:owllink-sexpr

                  (setf *my-owllink2-parser*
                        (make-instance parser-class 
                                       :url url
                                       :stream socket-stream))

                  (setf (result *my-owllink2-parser*)
                        (transform-sexpr-input 
                         (let ((*readtable* *owllink-readtable*)
                               (old-case (readtable-case *readtable*)))
                           
                           (unwind-protect
                               (progn 
                                 (setf (readtable-case *readtable*) :preserve)
                                 (setf (raw-input *my-owllink2-parser*)
                                       (loop as res = (read stream nil :eof)
                                             when (eq res :eof)
                                             return lines
                                             else collect 
                                             res into lines)))

                             (setf (readtable-case *readtable*) old-case)))))

                  (let ((res
                         (setf 
                          (answer *my-owllink2-parser*)
                          (if *converter-mode*
                              (result *my-owllink2-parser*)
                            (|owllink-parse-FunctionalRequestMessageWithHeader|
                             (result *my-owllink2-parser*))))))

                    (owllink-result-printer res socket-stream)))))
           
           (error (c) 
             (error-printer 
              
              (let ((c 
                     #+:racer-server (transform-to-html c)
                     #-:racer-server c))
                (if *error-indicator* 
                    (error-message "~A=~S"
                                   *error-indicator* c)
                  
                  (error-message "Bad request: ~S" c)))

              socket-stream)))))))
                                   
                         
#+:cl-http
(defun owllink2-eval-request1 (parser-class socket-stream url)
  (let (#+:racer-server
	(*tbox-verbose* nil)
        #+:racer-server
	(*abox-verbose* nil)
        #+:racer-server
	(*auto-classify* ':lazy)
        #+:racer-server
	(*auto-realize* ':lazy)
        #+:racer-server
	(*read-eval* nil)
        (*read-default-float-format* 'double-float)
        (*package* (find-package :owl-syntaxes))
        (*response-end-tag* nil)
	#+:racer-server
        (logging *tcp-console-logging*))

    (setf *telling-active* nil
          *last-request-xml-and-functional-namespaces* nil
          *last-request-xml-base* nil
          *last-request-rendering-options* nil)

    (let* ((length (http:get-header :content-length))
           (i 0)
           char)

      (if (and length (> length 0))

          (flet ((eval (stream)
                   (http:with-successful-response 
                       (socket-stream 
                        :xml
                        :content-location url
                        ;;:expires (url:expiration-universal-time url)
                        ;;:cache-control (url:response-cache-control-directives url)
                        ;;:content-language (languages url)
                        )

                     (process-owllink-request))))
              
            (if (< length 10000)

                (let ((input-string (and length (make-string length))))
                  (loop (setf char (code-char (read-byte socket-stream)))
                        (setf (aref input-string i) char)
                        (incf i 1)
                        (when (>= i length)
                          (return)))
		  #+:racer-server
                  (when logging 
                    (princ input-string *trace-output*)
                    (terpri *trace-output*))
                  (with-input-from-string (stream input-string)
                    (eval stream)))

              (with-temp-file ((stream)
                                      (loop (setf char (code-char (read-byte socket-stream)))
					#+:racer-server (when logging (princ char *trace-output*))
                                            (write-char char stream)
                                            (incf i 1)
                                            (when (>= i length)
                                              (return))))
                (eval stream))))

        (http:with-successful-response (socket-stream 
                                        :xml
                                        :content-location url
                                        ;;:expires (url:expiration-universal-time url)
                                        ;;:cache-control (url:response-cache-control-directives url)
                                        ;;:content-language (languages url)
                                        )

          (error-printer 
           (error-message "Message from client does not specify content length or content lenght is zero")
           socket-stream))))))


#+:aserve
(defun owllink2-eval-request1 (parser-class req ent)
  (let (#+:racer-server
	(*tbox-verbose* nil)
        #+:racer-server
	(*abox-verbose* nil)
        #+:racer-server
	(*auto-classify* ':lazy)
        #+:racer-server
	(*auto-realize* ':lazy)
	(*read-eval* nil)
        (*read-default-float-format* 'double-float)
        (*package* (find-package :owl-syntaxes))
        (*response-end-tag* nil)
        #+:racer-server
	(logging *tcp-console-logging*))
    
    (setf *telling-active* nil
          *last-request-xml-and-functional-namespaces* nil
          *last-request-xml-base* nil
          *last-request-rendering-options* nil)

    (let ((url (net.aserve:request-uri req))
          (content-encoding (net.aserve:header-slot-value 
			     req :content-encoding))
          input-string)
      
      (cond ((string= content-encoding "gzip")
	     #+:allegro
             (let* ((body (net.aserve::binary-get-request-body req))
		    (so (make-string-output-stream))
		    (bis (excl:make-buffer-input-stream body))
		    (bi))
			      
	       (util.zip::skip-gzip-header bis)
	       (setq bi (make-instance 'util.zip:inflate-stream :input-handle bis))
			 
	       (do ((byte (read-byte bi nil nil) (read-byte bi nil nil)))
		   ((null byte)
		    (setq input-string (get-output-stream-string so)))
		 (write-char (code-char byte) so)))
             #+(or :ccl :lispworks)
             (let* ((body (net.aserve:get-request-body req))
                    (output-stream (flex:make-in-memory-output-stream :element-type '(unsigned-byte 8))))
               (deflate:inflate-gzip-stream (flex:make-in-memory-input-stream body) output-stream)
               (setq input-string (flex:octets-to-string (flex:get-output-stream-sequence output-stream)))))
	    (t (setf input-string (net.aserve::get-request-body req))))

      #+:racer-server
      (when *log-file*
        (with-open-file (log-stream *log-file* :direction :output
			 :if-exists :append
			 :if-does-not-exist :create)
          (princ input-string log-stream)
          (terpri log-stream)))

      #+:racer-server
      (when logging
        (princ input-string *trace-output*)
        (terpri *trace-output*))

      (with-input-from-string (stream input-string)
	(net.aserve:with-http-response (req ent :content-type "text/xml")
	  (net.aserve:with-http-body (req ent)
	    (let ((socket-stream net.html.generator:*html-stream*))
              (process-owllink-request))))))))

;;;
;;;
;;;

(defun owllink-read-file1 (file &rest args 
                                &key
                                (input-syntax  :owllink-xml)
                                (output-syntax :owllink-xml)
                                &allow-other-keys)
  (declare (ignorable args))
  (let ((*package* (find-package :owl-syntaxes))
        (*owllink2-input-syntax* input-syntax)
        (*owllink2-output-syntax* output-syntax))

    (setf *telling-active* nil
          *last-request-xml-and-functional-namespaces* nil
          *last-request-xml-base* nil
          *last-request-rendering-options* nil)

    (with-open-file (stream file)
      (let ((url (nox:make-file-url file))
            (socket-stream *standard-output*)
            (parser-class 'owllink2-parser))
        (process-owllink-request)))))
  
(defun owllink-read-document1 (url &rest args)
  (declare (ignorable url args))
  (owlapi:to-be-implemented 'owllink-read-document))

;;;
;;;
;;;

(defun simplify-tag (string)

  (cond ((let ((pos (search +owllink-namespace+ string)))
           (and pos (= 0 pos)))
         
         (let ((res
                (subseq string (length +owllink-namespace+))))

           (if (and (string= res "Literal")
                    ;; within OWLlink Tell? -> OWL Literal
                    *telling-active*)
               "OWL***-Literal"
             res)))
        
        ((let ((pos (search +owllink-new-namespace+ string)))
           (and pos (= 0 pos)))

         (let ((res
                (subseq string (length +owllink-new-namespace+))))
           (if (and (string= res "Literal")
                    ;; within OWLlink Tell? -> OWL Literal
                    *telling-active*) 
               "OWL***-Literal"
             res)))

        ;;;
        ;;;
        ;;;

        ((let ((pos (search owlapi:+owl2-namespace+ string)))
           (and pos (= 0 pos)))
         (let ((res
                (subseq string (length owlapi:+owl2-namespace+))))
           (if (string= res "Literal")
               "OWL***-Literal"
             res)))
	
	((let ((pos (search owlapi:+old-owl2-namespace+ string)))
           (and pos (= 0 pos)))
         (let ((res
                (subseq string (length owlapi:+old-owl2-namespace+))))
           (if (string= res "Literal")
               "OWL***-Literal"
             res)))
        
        ;;;
        ;;;
        ;;; 

        #| 
        
        ((let ((pos (search +retraction-extension-namespace+ string)))
           (and pos (= 0 pos)))
         (subseq string (length +retraction-extension-namespace+)))
        
        ((let ((pos (search +racer-extension-namespace+ string)))
           (and pos (= 0 pos)))
         (subseq string (length +racer-extension-namespace+))) 

        |# 

        (t string)))


(defun transform-xml (xml)

  (let ((cdr-stack nil)
        (start nil)
        ;(last-open nil)
        (open nil))

    ;;; (declare (ignorable last-open open))

    (loop while xml do
        
          (let ((first (pop xml)))
            (when first 
              (ecase first 
                (:open 

                 (setf open 
                       (intern (owlapi:ensure-string (pop xml))))
                        
                 (let ((cell 

                        (cond ((gethash open *owl2-tags-input*)

                               (if *converter-mode*

                                   (list* ;;; tag name: 
                                          (case open
                                            (|OWL***-Literal| '|OWLLiteral|)
                                            (otherwise open))

                                          nil  
                                              
                                          (nconc 
                                           ;;; <Class IRI="xxx"/> -> 
                                           ;;; (Class "xxx")
                                           (remove nil 
                                                   (mapcar #'(lambda (assoc)
                                                               (let ((key (car assoc))
                                                                     (val (cdr assoc))

                                                                     (attributes-for-tag
                                                                      (gethash 
                                                                       open
                                                                       *owl2-tags-input-attributes*)))
                                                                   
                                                                 (when (owlapi:=> attributes-for-tag
                                                                           (member key
                                                                                   attributes-for-tag
                                                                                   :test #'string=))
                                                                   val)))
                                                           (pop xml)))
                                           ;;; content?                                  
                                           ;;; <Class>xxx</Class> ->
                                           ;;; (Class "xxx")
                                           (when (eq :content (first xml))
                                             (pop xml)
                                             (let ((content (pop xml)))
                                               (cond ((or (eq open '|IRI|)
                                                          (eq open '|abbreviatedIRI|)
                                                          (eq open '|AbbreviatedIRI|)
                                                          (eq open '|Import|))
                                                      (list content))
                                                     ((eq open '|OWL***-Literal|)
                                                      (list content))
                                                     ((eq open 
                                                          '|http://www.racer-systems.com/owllink/ext/racer#Racer|)
                                                      (list content)))))))

                                 
                                 (list* ;;; tag name: 
                                        (case open
                                          (|OWL***-Literal| 
                                           (if *owllink-mode* 
                                               '|OWLLiteral|
                                             '|Literal|))
                                          (otherwise open))
                                              
                                        (nconc 

                                         (remove nil 
                                                 (mapcar #'(lambda (assoc)
                                                             (let ((key (car assoc))
                                                                   (val (cdr assoc))
                                                                   (attributes-for-tag
                                                                    (gethash 
                                                                     open
                                                                     *owl2-tags-input-attributes*)))
                                                       
                                                               (when (owlapi:=> attributes-for-tag
                                                                         (member key
                                                                                 attributes-for-tag
                                                                                 :test #'string=))
                                                                 val)))
                                                         (pop xml)))
                                         (when (eq :content (first xml))
                                           (pop xml)
                                           (let ((content (pop xml)))
                                             (cond ((or (eq open '|IRI|)
                                                        (eq open '|abbreviatedIRI|)
                                                        (eq open '|AbbreviatedIRI|)
                                                        (eq open '|Import|))
                                                    (list content))
                                                   ((eq open '|OWL***-Literal|)
                                                    (list content))
                                                   ((eq open '|http://www.racer-systems.com/owllink/ext/racer#Racer|)
                                                    (list content)))))))))
                              
                              (t 

                               (cond ((eq open '|Literal|) ; needs value attribute... 

                                      ;;; OWLlink Literal, e.g. 
                                      ;;; <Set kb="http://www.owllink.org/examples/KB_1" key="abbreviatesIRIs">
                                      ;;;     <Literal>true</Literal>
                                      ;;; </Set>

                                      (pop xml) ; empty attributes
                                      (when (eq :content (first xml))
                                        (pop xml)
                                        (let ((content (pop xml)))
                                          (list* ;;; tag name: 
                                                 open
                                                 ;;; attributes in assoc-list 
                                                 `((|value| ,content)) nil))))

                                     (t
                                    
                                      (list* ;;; tag name: 
                                             open
                                             ;;; attributes in assoc-list 
                                             (mapcar #'(lambda (assoc)
                                                         (let ((key
                                                                (intern 
                                                                 (simplify-tag 
                                                                  (car assoc))))
                                                               (val (cdr assoc)))
                                                           `(,key ,val)))
                                                     (pop xml))
                                             ;;; content?                                  
                                             (when (eq :content (first xml))
                                               (pop xml)
                                               (let ((content (pop xml)))
                                                 (cond ((or (eq open '|IRI|)
                                                            (eq open '|abbreviatedIRI|)
                                                            (eq open '|AbbreviatedIRI|)
                                                            (eq open '|Import|))
                                                        (list content))
                                                       ((eq open 
                                                            '|http://www.racer-systems.com/owllink/ext/racer#Racer|)
                                                        (list content))))))))))))
                   
                   ;;(setf last-open open)

                   (when cdr-stack 
                     (let ((cell (list cell)))
                       (setf (cdr (last (first cdr-stack)))
                             cell)
                       ;; advance last pointer
                       ;; keep "last" efficent (small list)
                       (setf (first cdr-stack)
                             cell)))
               
                   (unless start
                     (setf start cell))
               
                   (push cell cdr-stack)))

                (:close 
                 (pop cdr-stack))
                
                (otherwise 
                 )))))

    start))
    
;;;
;;;
;;;

(defmethod nox:start-document ((self owllink2-parser) locator)
  (declare (ignorable locator))
  (setf (raw-input self) nil))

(defmethod nox:end-document ((self owllink2-parser) locator)
  (declare (ignorable locator))
  (setf (result self) 
        (list 
         (transform-xml (reverse (raw-input self))))))

(defmethod nox:char-content ((self owllink2-parser) (content string) mode)
  (declare (ignore mode))
  (with-slots (raw-input) self
    (push :content raw-input)
    (push (shrink-whitespaces content) raw-input)
    ;;;(push content raw-input)
    ))

(defmethod nox:start-element ((self owllink2-parser) (tag nox:open-tag) mode)
  (declare (ignore mode))
  (with-slots (raw-input) self
    (push :open raw-input)  

    (when (or (string-equal (nox:token-string tag) 
                            "http://www.owllink.org/owllink-xml#Tell")
              (string-equal (nox:token-string tag) 
                            "http://www.owllink.org/owllink#Tell"))
      (setf *telling-active* t))

    (when (or (string-equal (nox:token-string tag) 
                            "http://www.owllink.org/owllink-xml#RequestMessage")
              (string-equal (nox:token-string tag) 
                            "http://www.owllink.org/owllink#RequestMessage")
              (string-equal (nox:token-string tag) 
                            "http://www.w3.org/2002/07/owl#Ontology"))

      (setf *last-request-xml-and-functional-namespaces*
            (nox:tag-namespaces tag))

      (let ((xml-base
             (assoc "http://www.w3.org/XML/1998/namespace#base"
                    (nox:tag-attributes tag)
                    :test #'string=)))
        (when xml-base
          (setf *last-request-xml-base* (cdr xml-base)))

        (when (and (stringp (nox::tag-base tag)) (not xml-base))
          (setf *last-request-xml-base*
                (nox::tag-base tag)))))

    (push (simplify-tag (nox:token-string tag)) raw-input)
    (push (reverse (nox:tag-attributes tag)) raw-input)))

(defmethod nox:end-element ((self owllink2-parser) tag mode)
  (declare (ignorable mode))
  
  (with-slots (raw-input) self
    (push :close raw-input))

  (when (or (string-equal (nox:token-string tag) 
                          "http://www.owllink.org/owllink-xml#Tell")
            (string-equal (nox:token-string tag) 
                          "http://www.owllink.org/owllink#Tell"))
    (setf *telling-active* nil)))


;;;
;;;
;;;

(defun owllink-result-printer (expr stream)
  (if expr
      (ecase *owllink2-output-syntax* 
    
        (:owllink-xml
         (if *converter-mode*
             (owllink-render-as-xml expr stream)
           (owllink-xml-printer expr stream)))
        (:owllink-sexpr
         (if *converter-mode*
             (owllink-render-as-sexpr expr stream)
           (owllink-sexpr-printer expr stream)))
        (:owllink-functional
         (if *converter-mode*
             (owllink-render-as-functional expr stream)
           (owllink-functional-printer expr stream))))

    (error-printer "Empty OWLlink response (request not recognized?)" stream)))

(defun error-printer (message stream)
    
    (let ((expr
           (owllink-error-message :error message)))
      
      (ecase *owllink2-output-syntax* 
        (:owllink-xml
         (owllink-xml-printer expr stream))
        (:owllink-sexpr
         (owllink-sexpr-printer expr stream))
        (:owllink-functional
         (owllink-functional-printer expr stream)))))

;;;
;;;
;;;

(defun owllink-xml-printer (expr stream)       
  (let* ((*owllink-mode* t)
         (*dont-render-attributes* '(|INT-abbreviatesIRIs| |INT-kb| |INT-prefixes|
                                                           |xmlns| |xmlns:owl|))
         (*ensure-attributes-for-tag*
          '((|ResponseMessage| (|xmlns| |xmlns:owl|))))
         
         (*add-prefixes* nil) 
         (*abbreviate-iris* nil) 
         (*content-attributes* '((|Literal| |value|)))
         (*map-tag-attributes-to-iri-attributes* 
          '((|OneOf| |type|)
            (|List| |type|)))
         (*add-tag-content-as-attribute* 
          '((|AnonymousIndividual| |nodeID|)
            (:otherwise |IRI|)))
         (*disable-owl2-tags-for* '(|Setting| |Property|))
         (*dont-render-tags* '(|NamespacePrefix|)) 
         (*dont-abbreviate-iris-in-tags* '(|Prefix|))
         
         (additional-xml-answer-namespaces
          (loop as ns in *additional-answer-namespaces-used* 
                as entry = (find ns *last-request-xml-and-functional-namespaces* 
                                 :test #'string= :key #'cdr)
                when entry 
                collect (list (first entry) ns))))

    (setf (second expr) 
          (remove-duplicates 
           (append 
            `(("xmlns"
               "http://www.owllink.org/owllink#")

              ,@(mapcar #'(lambda (x) 
                            (list (format nil "xmlns:~A" (first x))
                                  (second x)))
                        (remove-if-not #'(lambda (x) 
                                           (member (first x) '("owl" 
                                                               #+:ignore 
                                                               "xsd") :test #'string-equal))
                                       +owl2-standard-prefixes+))

              ,@(mapcar #'(lambda (x) 
                            (list (format nil "xmlns:~A" (first x))
                                  (second x)))
                        additional-xml-answer-namespaces)
                  
              #+:ignore
              ("xmlns:xsi"
               "http://www.w3.org/2001/XMLSchema-instance")
              #+:ignore
              ("xsi:schemaLocation"
               "http://www.owllink.org/owllink# http://www.owllink.org/owllink-20091116.xsd"))
                
            (second expr))

           :test #'string=
           :key #'first))
    
    (print-xml expr stream)))

;;;
;;;
;;;

(defparameter *simplify-argument-lists* nil)

(defparameter *simplify-singleton-messages* nil)

(defparameter *simplify-boolean-and-string-messages* nil)

(defparameter *simplify-ignore-warnings* nil)

(defparameter *simplify-structuring-elements* nil)

(defmacro with-rendering-options (&body body)
  `(let ((*simplify-argument-lists* *simplify-argument-lists*)
         (*simplify-singleton-messages* *simplify-singleton-messages*)
         (*simplify-boolean-and-string-messages* *simplify-boolean-and-string-messages*)
         (*simplify-ignore-warnings* *simplify-ignore-warnings*)
         (*simplify-structuring-elements* *simplify-structuring-elements*)
         (*simplify-remove-owl-entities* *simplify-remove-owl-entities*))

     (dolist (x *last-request-rendering-options*)
       
       (let ((key (first x))
             (value (second x)))
         
         (cond ((string-equal key "useOWLEntities")
                (setf *simplify-remove-owl-entities* (not (string-to-boolean value))))
               ((string-equal key "removeEmptyArgumentLists")
                (setf *simplify-argument-lists* (string-to-boolean value)))
               ((string-equal key "simplifyBooleanAndStringAnswerMessages")
                (setf *simplify-boolean-and-string-messages* (string-to-boolean value)))
               ((string-equal key "noWarnings")
                (setf *simplify-ignore-warnings* (string-to-boolean value)))
               ((string-equal key "flattenStructuringElements")
                (setf *simplify-structuring-elements* (string-to-boolean value)))
               ((string-equal key "simplifyAtomicMessages")
                (setf *simplify-singleton-messages* (string-to-boolean value))))))

     ,@body))

;;;
;;;
;;; 
  
(defun owllink-functional-printer (expr stream)
  (let* ((*owllink-mode* t)
         (*dont-render-attributes* '(|INT-abbreviatesIRIs| 
                                     |INT-kb|
                                     |INT-prefixes|
                                     ;;|xmlns| |xmlns:owl|
                                     |http://www.w3.org/2001/XMLSchema-instance#schemaLocation|))
         (*ensure-attributes-for-tag*
          '((|ResponseMessage| (|xmlns| |xmlns:owl|))))
         (*add-prefixes* nil) 
         (*abbreviate-iris* nil)
         (*disable-owl2-tags-for* '(|Setting| |Property| |Set|))
         (*dont-abbreviate-iris-in-tags* '(|Prefix|))
         (*compress-tags-with-prefixes* 
          (mapcar #'(lambda (x) 
                      (list (car x) (cdr x)))
                  *last-request-xml-and-functional-namespaces*)))

    (with-rendering-options
      (func-printer stream expr))))

;;;
;;;
;;;

(defun owllink-sexpr-printer (s-expr stream)

  
  (let ((*owllink-mode* t)
	(*sexpr-rendering* t)

        (*dont-render-attributes*
         (if *simplify-ignore-warnings*
             '(|INT-abbreviatesIRIs| |warning|
                                     |INT-kb|
                                     |INT-prefixes| 
                                     |http://www.w3.org/2001/XMLSchema-instance#schemaLocation| )                                     

           '(|INT-abbreviatesIRIs| |INT-kb| 
                                   |INT-prefixes|
                                   |http://www.w3.org/2001/XMLSchema-instance#schemaLocation| )))
        
        (*add-prefixes* nil) 
        (*abbreviate-iris* nil)

        (*disable-owl2-tags-for* '(|Setting| |Property| |Set|))
	(*dont-abbreviate-iris-in-tags* '(|Prefix|))
        
        (*compress-tags-with-prefixes* 
         (mapcar #'(lambda (x) 
                     (list (car x) (cdr x)))
                 *last-request-xml-and-functional-namespaces*))

        (attributes nil))

    (with-rendering-options

      (macrolet ((tag-printer (tag &body cont)

                   `(progn 

                      (if (member ,tag *dont-render-tags*)

                          (progn 
                            ,@cont)

                        (if (eq ,tag :no-tag)

                            (progn 

                              (newline1 stream)
                        
                              (format stream "(")
       
                              (let* ((*indent* (+ 2 *indent*)))
                                ,@cont)
          
                              (format stream ")"))
    
                          (progn 

                            (when (toplevel-indent-p)
                              (newline1 stream))
       
                            (newline1 stream)
                        
                            (format stream "(~A" ,tag)
       
                            (let* ((*indent* (+ 2 *indent*)))
                              ,@cont)
          
                            (format stream ")")))))))
    
        (labels ((do-it (s-expr)

                   (cond ((and (consp s-expr) 
                               (eq (first s-expr) 'd-literal))

                          (let ((val (second s-expr))
                                (type (second (third s-expr))))

                            #| 
                    
                            (if type
                                (format stream "~S^^~A" 
                                        val
                                        (iri-printer nil type))
                              (format stream "~S^^xsd:string" 
                                      val)) |# 

                            (if type 
                                (tag-printer '|OWLLiteral|  
                                    (format stream " ~S ~S" val 
                                            (if (symbolp type)
                                                (symbol-name type)
                                              type)))
                              (tag-printer '|OWLLiteral|  
                                  (format stream " ~S" val)))))

                         ((consp s-expr) 
                                
                          (let ((tag (first s-expr))
                                (all-attributes (when *owllink-mode* 
                                                  (second s-expr)))
                                (rest (if *owllink-mode*
                                          (cddr s-expr)
                                        (cdr s-expr))))
                           
                            (setf attributes 
                                  (remove-if 
                                   #'(lambda (x) 
                                       (and (member (first x) *dont-render-attributes*)
                                            (not 
                                             (member (first x)
                                                     (second 
                                                      (assoc tag
                                                             *ensure-attributes-for-tag*))))))
                                   all-attributes))

                            (let* ((tag (expand-functional-tag-name tag))
                                   
                                   (*tag-stack* (cons tag *tag-stack*))
                                   
                                   (*owl2-tags-disabled* 
                                    (or *owl2-tags-disabled* 
                                        (member tag *disable-owl2-tags-for*)))

                                   (is-owl2-tag-p 
                                    (and (not *owl2-tags-disabled*)
                                         (gethash tag *owl2-tags*)))

                                   (*abbreviate-iris*
                                    (if (not *owllink-mode*)
                                        *abbreviate-iris*
                                      (and *owllink-mode*
                                           (let ((res (assoc '|INT-abbreviatesIRIs| all-attributes)))
                                             (if res
                                                 (string-to-boolean
                                                  (second res))
                                               *abbreviate-iris*)))))
                         
                                   (*add-prefixes*
                                    (if (not *owllink-mode*)
                                        *add-prefixes*
                                      (or (when *abbreviate-iris*
                                            (let ((res 
                                                   (assoc '|INT-prefixes| all-attributes))) 
                                              ;; ueberschreiben stets!
                                              (if res 
                                                  (second res)
                                                *add-prefixes*)))
                                          *add-prefixes*)))

                                   (tag
                                    (compress-functional-tag-name tag))

                                   (tag1 nil))
                              
                              #+:ignore
                              (multiple-value-bind (tag prefix)
                                  (get-tag-and-prefix tag)

                                (declare (ignoreable prefix))

                                (setf tag1
                                      (if prefix
                                          (intern (format nil "~A:~A" prefix tag))
                                        tag)))

                              (setf tag1 tag)

                              (cond ((and *simplify-argument-lists*
                                          *simplify-singleton-messages*
                                          (not attributes)
                                          (not rest))
                                   
                                     ;;; (OK) -> OK
                                     (when (toplevel-indent-p)
                                       (newline1 stream))
       
                                     (do-it tag1))

                                    ((and *simplify-boolean-and-string-messages* 
                                          (eq tag '|BooleanResponse|)
                                          (not (cdr attributes)))
                                   
                                     (when (toplevel-indent-p)
                                       (newline1 stream))

                                     (do-it (string-to-boolean (second (first attributes)))))
                                  
                                    ((and *simplify-boolean-and-string-messages* 
                                          (eq tag '|StringResponse|)
                                          (not (cdr attributes)))

                                     (when (toplevel-indent-p)
                                       (newline1 stream))

                                     (do-it (second (first attributes))))

                                    ((and *simplify-remove-owl-entities*
                                          (not (eq (second *tag-stack*) '|Declaration|))
                                          (member tag '(|Class| 
                                                        |NamedIndividual|
                                                        |AnonymousIndividual|
                                                        |ObjectProperty|
                                                        |DataProperty|
                                                        |AnnotationProperty|)))
                                   
                                     (newline1 stream) ;; gut? 
                                     (iri-printer stream (first rest)))

                                    ((and 
                                      *converter-mode*
                                      (stringp (first rest))
                                      (member tag '(|Class| 
                                                    |NamedIndividual|
                                                    |AnonymousIndividual|
                                                    |ObjectProperty|
                                                    |DataProperty|
                                                    |AnnotationProperty|)))

                                     (tag-printer tag1 
                                         (progn 
                                           (if *use-empty-argument-lists-for-owl2-tags-in-sexpr-syntax-input* 
                                               (format stream " () ")
                                             (format stream " "))
                                           (let ((iri (iri-owl (first rest))))
                                             (if (position #\: iri)
                                                 (format stream "|~A|" iri)
                                               (format stream "~A" iri))))))
                                    
                                    ((and 
                                      *converter-mode*
                                      (member tag '(|OWLLiteral|)))
                                     (tag-printer '|OWLLiteral|  
                                         (progn 
                                           (format stream " ~S ~S" (second rest) (first rest)))))
                                    
                                    (t 

                                     (let ((no-tag-p
                                          
                                            (and *simplify-structuring-elements*
                                                 (not attributes)
                                                 (member tag 
                                                         '(|ClassSynset|
                                                           |ObjectPropertySynset|
                                                           |DataPropertySynset|
                                                           |IndividualSynset| 
                                                  
                                                           |SetOfClassSynsets|
                                                           |SetOfObjectPropertySynsets|
                                                           |SetOfDataPropertySynsets|
                                                           |SetOfIndividualSynsets| 

                                                           ;;|SetOfIndividuals|
                                                           ;;|SetOfClasses|
                                                           ;;|SetOfLiterals|
                                                           ;;|SetOfObjectProperties|
                                                           ;;|SetOfDataProperties|
                                                           ;;|SetOfAnnotationProperties|

                                                           |ClassSubClassesPair|
                                                           |ClassSuperClassesPair|
                                                           |ObjectPropertySubObjectPropertiesPair|
                                                           |ObjectPropertySuperObjectPropertiesPair|
                                                           |DataPropertySubDataPropertiesPair|
                                                           |DataPropertySuperDataPropertiesPair|)))))

                                       (if no-tag-p 
                                           (if (toplevel-indent-p)
                                               (newline1 stream)))
                                               
                                       (tag-printer (if no-tag-p :no-tag tag1)
						    
                                           (progn 
                                             (if attributes 
                                                 (progn 
                                                   (format stream " (")
                                                   (loop as (key val) in attributes 
                                                         do 
                                                         (let ((name
                                                                (symbol-name key)))
                                                           (cond ((member key '(|IRI| |fullIRI| |kb|))
                                                                  (setf val
                                                                        (let ((*abbreviate-iris* nil))
                                                                          (iri-printer nil val)))
                                                                  (if (needs-bars-p name)
                                                                      (format stream ":|~A| ~A" name val)
                                                                    (format stream ":~A ~A" name val)))
                                                                 (t (if (needs-bars-p name)
                                                                        (format stream ":|~A| \"~A\" " name val)
                                                                      (format stream ":~A \"~A\" " name val))))))
                                                   (format stream ") "))
                                               
                                               (cond (is-owl2-tag-p 
                                                      (if *use-empty-argument-lists-for-owl2-tags-in-sexpr-syntax-input*
                                                          (format stream " () ")
                                                        (format stream " ")))
                                                     (*simplify-argument-lists*
                                                      (format stream " "))
                                                     (t 
                                                      (format stream " () "))))

                                             (dolist (s-expr rest)
                                               (do-it s-expr))))))))))

                         ((symbolp s-expr)
                          (newline1 stream)
                          (iri-printer stream s-expr))
        
                         ((stringp s-expr)
                          (newline1 stream)
                          (string-printer stream s-expr))

                         (t
                          (newline1 stream)
                          (item-printer stream s-expr)))))

          (do-it s-expr))))))

;;;
;;;
;;;

#|

(defun trace-owllink2 ()
  (trace nox:start-element 
         nox:end-element 
         nox:start-document 
         nox:end-document 
         owllink2-eval-request
         owllink2-eval-request1))

(defun get-kb-from-xml ()
  (pprint
   (ts::transform-xml (reverse (raw-input ts::*my-owllink2-parser*)))))

(defun owllink-xml-test ()
  (dolist (x 
           (remove-if-not #'consp  ; ontology, ontology name attributes, etc. 
                          (ts::transform-xml (reverse (raw-input ts::*my-owllink2-parser*)))))
    (terpri)
    (terpri)
    (princ "Input: ") 
    (pprint x)
    (terpri)
    (princ "Output: ") 
    (if (and (consp x) (eq (first x) '|Prefix|))
        (pprint 
         (ts::parse-prefix-declaration (list x)))
      (pprint (ts::parse-axiom (list x))))))

|# 

;;;
;;;
;;;

(defun owllink-cur-input-again-xml ()
  (owllink-xml-printer
   (setf 
    (answer *my-owllink2-parser*)
    (|owllink-parse-RequestMessage|
     (result *my-owllink2-parser*)))
   t)

  ;(owllink-get-cur-answer)
  )

(defun owllink-cur-input-again-functional ()
  (owllink-functional-printer
   (setf 
    (answer *my-owllink2-parser*)
    (|owllink-parse-FunctionalRequestMessageWithHeader| 
     ;;|owllink-parse-RequestMessage|
     (result *my-owllink2-parser*)))
   t)
  
  ;(owllink-get-cur-answer)
  )

(defun owllink-cur-input-again-sexpr ()
  (owllink-sexpr-printer 
   (setf 
    (answer *my-owllink2-parser*)
      (|owllink-parse-FunctionalRequestMessageWithHeader| 
       ;;|owllink-parse-RequestMessage|
       (result *my-owllink2-parser*)))
   t)

  ;(owllink-get-cur-answer)
  )


(defun owllink-transform-again-sexpr ()
  (let ((*converter-mode* t))
    (owllink-render-as-sexpr 
     (result *my-owllink2-parser*)
     t)))

;;;
;;;
;;;

(defun owllink-render-as-functional (exprs &optional (stream t))
  (mapcan #'(lambda (x) 
              (owllink-functional-printer x stream))

          (nconc 

           (mapcar #'(lambda (x) 
                       (make-owllink-message '|NamespacePrefix| nil (list (car x) (cdr x))))
                   *last-request-xml-and-functional-namespaces*)

           exprs)))

(defun owllink-render-as-sexpr (exprs &optional (stream t))
  (mapcan #'(lambda (x) 
              (owllink-sexpr-printer x stream))
          
          (nconc 

           (mapcar #'(lambda (x) 
                       (make-owllink-message '|NamespacePrefix| nil
                                             (list (intern (owlapi:ensure-string (car x)))
						   (intern (owlapi:ensure-string (cdr x))))))
                   *last-request-xml-and-functional-namespaces*)

           exprs)))
          
(defun owllink-render-as-xml (exprs  &optional (stream t))
  (mapcan #'(lambda (x) 
              (owllink-xml-printer x stream))
          exprs))

;;;
;;;
;;; 

(defun owllink-get-cur-input ()
  (pprint 
   (result *my-owllink2-parser*)))


(defun owllink-get-cur-raw-input ()
  (pprint 
   (raw-input *my-owllink2-parser*)))

(defun owllink-get-cur-answer ()
  (pprint 
   (answer *my-owllink2-parser*)))


;;;
;;;
;;;


(owlapi:defun1 owlapi-read-xml-ontology (uri &rest args 
                                      &key ontology-name reasoner-name kb-name &allow-other-keys)
  
        (with-input-from-url (stream uri)
          (nox:parse-from-stream stream 
                                 uri
                                 'nox:xml-parser 
                                 :consumer 
                                 (setf *my-owllink2-parser*
                                       (make-instance 'owllink2-parser
                                                      :url uri)))
                   
          (let ((*use-xml-base* *last-request-xml-base*)
                (*owl-xml-mode* t))

            (apply #'owlapi-process-functional-ontology 
                   (result *my-owllink2-parser*) 
                   :reasoner-name (or reasoner-name kb-name uri)
                   :ontology-name ontology-name
                   :allow-other-keys t
                   args))))

;;;
;;;
;;;

(owlapi:owlapi-defun (|OWLAPI-readXMLOntologyFile|) (fn &rest args
                                                         &key 
                                                         (ignore-import *ignore-import-p*)
                                                         &allow-other-keys)
  (let* ((*import-level* 0)

         (*ignore-import-p* ignore-import)

         (fn (make-url-from-filename fn))

         (*imported-ontologies* (list (intern (owlapi:ensure-string fn)))))

    (#+:racer-server without-duplicate-warnings 
     #-:racer-server progn

      (apply #'owlapi-read-xml-ontology fn 
             :parser #'parse-ontology-document 
             :allow-other-keys t
             args))))

(owlapi:owlapi-defun (|OWLAPI-readXMLOntologyDocument|) (url &rest args
                                                              &key 
                                                              (ignore-import *ignore-import-p*)
                                                              &allow-other-keys)
  (let* ((*import-level* 0)

         (*ignore-import-p* ignore-import)

         (*imported-ontologies* (list (intern (owlapi:ensure-string url)))))

    (#+:racer-server without-duplicate-warnings 
     #-:racer-server progn

      (apply #'owlapi-read-xml-ontology url
             :parser #'parse-ontology-document 
             :allow-other-keys t
             args))))

