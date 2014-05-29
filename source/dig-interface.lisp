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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (racer:enable-boolean-readers)
  (wilbur:enable-node-shorthand))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +old-dig-url-prefix+ "http://dl.kr.org/dig/lang#")
  (defconstant +dig-url-prefix+ "http://dl.kr.org/dig/2003/02/lang#")
  (defconstant +parser-states+ '(:telling :retracting :asking :normal nil))
  )

(defparameter *debug-dig* nil)

(defconstant +special-chars+ 
  (loop for ch in '(34 60 62 154 138 159 133 128 134 167 38 59 246 252 228 214 196 220 223)
        collect (code-char ch)))

(defvar *response-end-tag*)
(defparameter *error-indicator* "error message")

(defun find-kb-name-from-uri (string)
  (if (string= string "")
      'default
    (gethash string *allocated-kbs*)))

(defun (setf find-kb-name-from-uri) (new-value string)
  (setf (gethash string *allocated-kbs*) new-value))

(defun delete-kbs ()
  (loop for kb being the hash-values of *allocated-kbs* do
        (forget-tbox kb))
  (clrhash *allocated-kbs*)
  nil)

(defun delete-kb (stream attributes)
  (let ((uri (find-dig-attribute "uri" attributes nil)))
    (multiple-value-bind (kb-name found)
                         (find-kb-name-from-uri uri)
      (cond (kb-name
             (setf (find-kb-name-from-uri uri) nil)
             (format stream
                     "   <ok/>~%"))
            ((null found) 
             (error "No such knowledge base known: ~A" uri))
            (t (error "Knowledge base ~A was already released." uri))))))

(defun special-char-p (char)
  (find char +special-chars+))
  
(defun transform-to-html (condition)
  (let ((message (format nil "~A" condition))
        (transformed-message-list ()))
    (loop for char across message do
          (cond ((special-char-p char)
                 (push #\& transformed-message-list)
                 (push #\# transformed-message-list)
                 (loop for char1 across (format nil "~A" (char-code char)) do
                       (push char1 transformed-message-list))
                 (push #\; transformed-message-list))
                (t (push char transformed-message-list))))
    (coerce (reverse transformed-message-list) 'string)))

(defclass dig-racer 
  (nox:sax-consumer)
  ((url :initarg :url :accessor parser-url)
   (stream :initarg :stream :accessor parser-stream)
   (output-stream :initarg :output-stream :accessor parser-output-stream)
   (tbox :initform nil :accessor dig-racer-consumer-tbox)
   (tbox-default-name :initarg :tbox-default-name 
                      :initform nil 
                      :accessor dig-racer-consumer-tbox-default-name)
   (abox :initform nil :accessor dig-racer-consumer-abox)
   (abox-default-name :initarg :abox-default-name 
                      :initform nil 
                      :accessor dig-racer-consumer-abox-default-name)
   (tbox-individuals :initform (make-hash-table)
                     :accessor dig-racer-consumer-tbox-individuals)
   (parser-state :initform nil
                 :accessor parser-state)
   (error-during-parse :initform nil
                       :accessor error-during-parse)))

(defmethod (setf parser-state) :before (new-value (parser dig-racer))
  (unless (or (member new-value +parser-states+)
              (and (consp new-value) (member (first new-value) +parser-states+)))
    (error "Internal error: unknown parser state ~S." new-value)))

(defmethod nox:start-document ((self dig-racer) locator)
  (declare (ignore locator))
  (when (and (dig-racer-consumer-tbox-default-name self)
             (dig-racer-consumer-abox-default-name self))
    (let ((old-tbox (find-tbox (dig-racer-consumer-tbox-default-name self) nil))
	  (old-abox (find-abox (dig-racer-consumer-abox-default-name self) nil)))
      (setf (dig-racer-consumer-tbox self) (or old-tbox
					       (xml-define-tbox (dig-racer-consumer-tbox-default-name self))))
      (setf (dig-racer-consumer-abox self) (or (and old-tbox
						    old-abox
						    (eq old-tbox (abox-tbox old-abox))
						    old-abox)
					       (xml-define-abox (dig-racer-consumer-abox-default-name self)
								(dig-racer-consumer-tbox-default-name self))))
      (setf *stack* nil)
					;(format t "~&START DOCUMENT ~S" locator)
      )))


(defun find-dig-attribute (name attributes &optional (intern t))
  (let ((value (or (cdr (assoc name
                               attributes :test #'string-equal))
                   (cdr (assoc (concatenate 'string +dig-url-prefix+ name)
                               attributes :test #'string-equal))
                   (cdr (assoc (concatenate 'string +old-dig-url-prefix+ name)
                               attributes :test #'string-equal)))))
    (if (and intern (stringp value))
      (intern value)
      value)))

(defun tag-equal (token-string tag)
  (let ((tag-length (length tag)))
    (or 
     (and (loop for i from 0 below #.(length +dig-url-prefix+)
                always (char= (aref +dig-url-prefix+ i) (aref token-string i)))
          (= (+ #.(length +dig-url-prefix+) tag-length) (length token-string))
          (loop for i from 0 below tag-length
                for j from #.(length +dig-url-prefix+)
                always (char= (aref tag i) (aref token-string j))))
     (and (loop for i from 0 below #.(length +old-dig-url-prefix+)
                always (char= (aref +old-dig-url-prefix+ i) (aref token-string i)))
          (= (+ #.(length +old-dig-url-prefix+) tag-length) (length token-string))
          (loop for i from 0 below tag-length
                for j from #.(length +old-dig-url-prefix+)
                always (char= (aref tag i) (aref token-string j)))))))

(defmacro with-logging ((stream) &body forms)
  (let ((logging-var (gensym))
        (stream-var (gensym)))
    `(let ((,logging-var *tcp-console-logging*))
       (flet ((doit (new-stream)
                (let ((,stream new-stream))
                  .,forms)))
         (if ,logging-var
           (let ((result
                  (with-output-to-string (stream-1)
                    (doit stream-1))))
             (princ result *trace-output*)
             (when *log-file*
               (with-open-file (,stream-var *log-file* :direction :output
			                    :if-exists :append
			                    :if-does-not-exist :create)
	         (princ result ,stream-var)))
             (princ result ,stream))
           (doit ,stream))))))

(defmacro with-error-handling (code-body error-body)
  #+:debug
  `(flet ((do-it () ,code-body))
     (if *debug-dig*
         (do-it)
     (with-error-handling-1 (do-it) ,error-body)))
  #-:debug
  `(with-error-handling-1 ,code-body ,error-body))


(defmacro with-error-handling-1 (code-body error-body)
  #+:cl-http
  `(http::handler-case-if (not http::*debug-server*)
                          ,code-body
     ,error-body)
  #-:cl-http
  `(handler-case
     ,code-body
     ,error-body))


(defun start-parse-concept-or-role (self token-string attributes )
  (declare (ignorable self))
  (cond ((tag-equal token-string "catom")
         (let ((name (find-dig-attribute "name" attributes)))
           (push  name *stack*)))
        ((tag-equal token-string "individual")
         (let ((ind-name (find-dig-attribute "name" attributes)))
           (push ind-name
                 *stack*)))
        ((tag-equal token-string "ratom")
         (push (find-dig-attribute "name" attributes) 
               *stack*))
        ((tag-equal token-string "feature")
         (push (find-dig-attribute "name" attributes) 
               *stack*))
        ((tag-equal token-string "attribute")
         (push (find-dig-attribute "name" attributes) 
               *stack*))
        ((tag-equal token-string "and")
         (push 'end *stack*))
        ((tag-equal token-string "or")
         (push 'end *stack*))
        ((tag-equal token-string "some"))
        ((tag-equal token-string "all"))
        ((tag-equal token-string "top"))
        ((tag-equal token-string "bottom"))
        ((tag-equal token-string "not"))
        ((tag-equal token-string "inverse"))
        ((tag-equal token-string "atmost")
         (push (find-dig-attribute "num" attributes nil) *stack*))
        ((tag-equal token-string "atleast")
         (push (find-dig-attribute "num" attributes nil) *stack*))
        ((tag-equal token-string "intmin")
         (push (read-from-string (find-dig-attribute "val" attributes nil))
               *stack*))
        ((tag-equal token-string "intmax")
         (push (read-from-string (find-dig-attribute "val" attributes nil))
               *stack*))
        ((tag-equal token-string "intequals")
         (push (read-from-string (find-dig-attribute "val" attributes nil))
               *stack*))
        ((tag-equal token-string "intrange")
         (push (read-from-string (find-dig-attribute "min" attributes nil))
               *stack*)
         (push (read-from-string (find-dig-attribute "max" attributes nil))
               *stack*))
        ((tag-equal token-string "floatmin")
         (push (read-from-string (find-dig-attribute "val" attributes nil))
               *stack*))
        ((tag-equal token-string "floatmax")
         (push (read-from-string (find-dig-attribute "val" attributes nil))
               *stack*))
        ((tag-equal token-string "floatequals")
         (push (read-from-string (find-dig-attribute "val" attributes nil))
               *stack*))
        ((tag-equal token-string "floatrange")
         (push (read-from-string (find-dig-attribute "min" attributes nil))
               *stack*)
         (push (read-from-string (find-dig-attribute "max" attributes nil))
               *stack*))
        ((tag-equal token-string "stringequals")
         (push (find-dig-attribute "val" attributes nil)
               *stack*))
        ((tag-equal token-string "booleanequals")
         (push (find-dig-attribute "val" attributes nil)
               *stack*))
        ((tag-equal token-string "defined"))
        ((tag-equal token-string "iset")
         (push 'end *stack*))
        ((tag-equal token-string "ival"))
        ((tag-equal token-string "sval"))
        ((tag-equal token-string "fval"))
        ((tag-equal token-string "bval"))
        (t (error "Cannot handle tag ~A" token-string))))

(defun start-dig-management (self token-string attributes )  
  (cond ((tag-equal token-string "getIdentifier")
         (identify-racer (parser-stream self)))
        ((tag-equal token-string "useKB")
         (let ((kb-name (find-dig-attribute "name" attributes)))
           (setf (find-kb-name-from-uri (symbol-name kb-name)) 
                 (intern (symbol-name kb-name) *racer-user-package*)))
         (start-response-generation (parser-stream self)))
        ((tag-equal token-string "racer")
         (let ((expression (find-dig-attribute "expr" attributes))) 
           (start-response-generation (parser-stream self))
           (racer-execute-expression expression (parser-stream self))))
        ((tag-equal token-string "newKB")
         (start-response-generation (parser-stream self))
         (create-new-kb (parser-stream self)))
        ((tag-equal token-string "deleteKBs")
         (start-response-generation (parser-stream self))
         (delete-kbs)
         (format (parser-stream self) "<ok/>"))
        ((tag-equal token-string "deleteKB")
         (start-response-generation (parser-stream self))
         (delete-kb (parser-stream self) attributes)
         (format (parser-stream self) "<ok/>"))
        ((tag-equal token-string "releaseKB")
         (let ((stream (parser-stream self)))
           (start-response-generation stream)
           (if (eq *dig-version* :dig-1-1)
             (delete-kb stream attributes)    
             (release-kb stream attributes))
           (format stream "   <ok/>~%")))
        ((tag-equal token-string "asks")
         (setup-kb self attributes)
         (start-responses-generation (parser-stream self))
         (setf *stack* nil)
         (setf (parser-state self) ':asking))
        ((tag-equal token-string "tells")
         (setup-kb self attributes)
         (start-response-generation (parser-stream self))
         (setf *stack* nil)
         (setf (parser-state self) ':telling))
        (t (error "Cannot handle tag ~A" token-string))))
               
(defun start-dig-telling (self token-string attributes)
  (cond ((tag-equal token-string "clearKB")
         (init-tbox (dig-racer-consumer-tbox self))
         (init-abox (dig-racer-consumer-abox self) 
                    (dig-racer-consumer-tbox self)))
        ((tag-equal token-string "defconcept")
         (xml-define-concept (find-dig-attribute "name" attributes)
                             (dig-racer-consumer-tbox self))
         (setf *stack* nil))
        ((tag-equal token-string "defrole")
         (ensure-role (find-dig-attribute "name" attributes) 
                      (dig-racer-consumer-tbox self))
         (setf *stack* nil))
        ((tag-equal token-string "defannotation")
         (let ((role-name (find-dig-attribute "name" attributes)))
           (ensure-role role-name (dig-racer-consumer-tbox self))
           (role-is-used-as-annotation-property role-name 
                                                (dig-racer-consumer-tbox self))
           (setf *stack* nil)))
        ((tag-equal token-string "deffeature")
         (ensure-role (find-dig-attribute "name" attributes) 
                      (dig-racer-consumer-tbox self))
         (role-is-functional (find-dig-attribute "name" attributes)
                             (dig-racer-consumer-tbox self))
         (setf *stack* nil))
        ((tag-equal token-string "defattribute")
         (let ((role-name (find-dig-attribute "name" attributes)))
           (ensure-role role-name (dig-racer-consumer-tbox self))
           (role-is-used-as-datatype-property role-name (dig-racer-consumer-tbox self))
           (if (eq *dig-version* :dig-1-1)
               (role-is-functional (find-dig-attribute "name" attributes)
                                   (dig-racer-consumer-tbox self))))
         (setf *stack* nil))
        ((tag-equal token-string "defindividual")
         (add-individual (dig-racer-consumer-abox self) 
                         (find-dig-attribute "name" attributes)
                         nil)
         (setf *stack* nil))
        ((tag-equal token-string "impliesc")
         (setf *stack* nil))
        ((tag-equal token-string "equalc")
         (setf *stack* nil))
        ((tag-equal token-string "disjoint")
         (setf *stack* nil)
         (push 'end *stack*))
        ((tag-equal token-string "alldifferent")
         (push 'end *stack*))
        ((tag-equal token-string "differentfrom")
         (push 'end *stack*))
        ((tag-equal token-string "sameas")
         (push 'end *stack*))
        ((tag-equal token-string "impliesr")
         (setf *stack* nil))
        ((tag-equal token-string "equalr")
         (setf *stack* nil))
        ((tag-equal token-string "domain")
         (setf *stack* nil))
        ((tag-equal token-string "range")
         (setf *stack* nil))
        ((tag-equal token-string "transitive")
         (setf *stack* nil))
        ((tag-equal token-string "functional")
         (setf *stack* nil))
        ((tag-equal token-string "rangeint")
         (setf *stack* nil))
        ((tag-equal token-string "rangestring")
         (setf *stack* nil))
        ((tag-equal token-string "rangefloat")
         (setf *stack* nil))
        ((tag-equal token-string "rangeboolean")
         (setf *stack* nil))
        ((tag-equal token-string "instanceof")
         (setf *stack* nil)
         (push 'end *stack*))
        ((tag-equal token-string "related")
         (setf *stack* nil))
        ((tag-equal token-string "value")
         (setf *stack* nil))
        (t (start-parse-concept-or-role self token-string attributes))))

(defun start-dig-asking (self token-string attributes) 
  (cond ((tag-equal token-string "satisfiable")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "subsumes")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "equivalent")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "disjoint")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "parents")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "taxonomy")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "incrementalTaxonomy")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "children")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "ancestors")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "descendants")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "equivalents")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "rsubusmes")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "requivalent")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "rparents")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "rchildren")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "rancestors")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "rdescendants")
         (push (find-dig-attribute "id" attributes) 
               *stack*)) 
        ((tag-equal token-string "requivalents")
         (push (find-dig-attribute "id" attributes) 
               *stack*)) 
        ((tag-equal token-string "consistentKB")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "instances")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "types")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "roleFillers")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "relatedIndividuals")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "instance")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "toldValues")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "allConceptNames")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "allRoleNames")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((tag-equal token-string "allIndividuals")
         (push (find-dig-attribute "id" attributes) 
               *stack*))
        ((and (tag-equal token-string "retrieve")
              (eq *dig-version* +dig-1-2+))
         (push (find-dig-attribute "id" attributes) 
               *stack*)
         (push (find-dig-attribute "ntuples" attributes) 
               *stack*)
         (push 'end *stack*))
        ((tag-equal token-string "head")
         (push 'end *stack*))
        ((tag-equal token-string "body")
         )
        ((tag-equal token-string "indvar")
         (push `(variable ,(find-dig-attribute "name" attributes))
               *stack*))
        ((tag-equal token-string "ind")
         (push `(individual ,(find-dig-attribute "name" attributes))
               *stack*))
        ((tag-equal token-string "attrValue")
         (push `(attribute-value ,(find-dig-attribute "name" attributes))
               *stack*))
        ((tag-equal token-string "qand")
         (push 'end *stack*))
        ((tag-equal token-string "qunion")
         (push 'end *stack*))
        ((tag-equal token-string "qneg"))
        ((tag-equal token-string "cqatom")
         )
        ((tag-equal token-string "rqatom")
         )
        (t (start-parse-concept-or-role self token-string attributes))))



(defmethod nox:start-element ((self dig-racer) (tag nox:open-tag) mode)
    (declare (ignore mode))
    (with-error-handling 
     (let ((token-string (nox:token-string tag))
           (attributes (nox:tag-attributes tag)))
      
         (cond ((or (eq (parser-state self) ':management)
                    (null (parser-state self)))

                (start-dig-management self token-string attributes))
               
               ((eq (parser-state self) ':telling)

                (start-dig-telling self token-string attributes))

               ((eq (parser-state self) ':asking)

                (start-dig-asking self token-string attributes))
               
               (t (error "Cannot handle tag ~A" token-string))))
     (error (c)
           (generate-error-2 self c))))


(defun end-parse-concept-or-role (self token-string)
  (cond ((tag-equal token-string "catom"))
        ((tag-equal token-string "ratom"))
        ((tag-equal token-string "feature"))
        ((tag-equal token-string "attribute"))
        ((tag-equal token-string "individual"))                     
        ((tag-equal token-string "and")
         (let ((conjuncts (loop for top in *stack*
                                until (eq top 'end)
                                collect top
                                do (pop *stack*))))
           (pop *stack*)
           (push `(and .,conjuncts) *stack*)))
        ((tag-equal token-string "or")
         (let ((conjuncts (loop for top in *stack*
                                until (eq top 'end)
                                collect top
                                do (pop *stack*))))
           (pop *stack*)
           (push `(or .,conjuncts) *stack*)))
        ((tag-equal token-string "some")
         (let ((concept (pop *stack*))
               (role (pop *stack*)))
           (push `(some ,role ,concept) *stack*)))
        ((tag-equal token-string "all")
         (let ((concept (pop *stack*))
               (role (pop *stack*)))
           (push `(all ,role ,concept) *stack*)))
        ((tag-equal token-string "not")
         (let ((concept (pop *stack*)))
           (push `(not ,concept) *stack*)))
        ((tag-equal token-string "inverse")
         (let ((role (pop *stack*)))
           (push `(inv ,role) *stack*)))
        ((tag-equal token-string "top")
         (push 'top *stack*))
        ((tag-equal token-string "bottom")
         (push 'bottom *stack*))
        ((tag-equal token-string "atmost")
         (let ((concept (pop *stack*))
               (role (pop *stack*))
               (num (read-from-string (pop *stack*))))
           (push `(at-most ,num ,role ,concept) *stack*)))
        ((tag-equal token-string "atleast")
         (let ((concept (pop *stack*))
               (role (pop *stack*))
               (num (read-from-string (pop *stack*))))
           (push `(at-least ,num ,role ,concept) *stack*)))
        ((tag-equal token-string "intmin")
         (let ((attribute (pop *stack*))
               (min (pop *stack*)))
           (push `(some ,attribute (min ,+has-integer-value+ ,min)) *stack*)))
        ((tag-equal token-string "intmax")
         (let ((attribute (pop *stack*))
               (max (pop *stack*)))
           (push `(some ,attribute (max ,+has-integer-value+ ,max)) *stack*)))
        ((tag-equal token-string "intequals")
         (let ((attribute (pop *stack*))
               (value (pop *stack*)))
           (push `(some ,attribute (equal ,+has-integer-value+ ,value)) *stack*)))
        ((tag-equal token-string "intrange")
         (let ((attribute (pop *stack*))
               (max (pop *stack*))
               (min (pop *stack*)))
           (push `(some ,attribute 
                        (and (min ,+has-integer-value+ ,min)
                             (max ,+has-integer-value+ ,max)))
                 *stack*)))
        ((tag-equal token-string "floatmin")
         (let ((attribute (pop *stack*))
               (min (pop *stack*)))
           (push `(some ,attribute (>= ,+has-real-value+ ,min)) *stack*)))
        ((tag-equal token-string "floatmax")
         (let ((attribute (pop *stack*))
               (max (pop *stack*)))
           (push `(some ,attribute (<= ,+has-real-value+ ,max)) *stack*)))
        ((tag-equal token-string "floatequals")
         (let ((attribute (pop *stack*))
               (value (pop *stack*)))
           (push `(some ,attribute (= ,+has-real-value+ ,value)) *stack*)))
        ((tag-equal token-string "floatrange")
         (let ((attribute (pop *stack*))
               (max (pop *stack*))
               (min (pop *stack*)))
           (push `(and (some ,attribute
                             (>= ,+has-real-value+ ,min) 
                             (<= ,+has-real-value+ ,max)))
                 *stack*)))
        ((tag-equal token-string "stringequals")
         (let ((attribute (pop *stack*))
               (value (pop *stack*)))
           (push `(some ,attribute (string= ,+has-string-value+ ,value))
                 *stack*)))
        ((tag-equal token-string "booleanequals")
         (let ((attribute (pop *stack*))
               (value (pop *stack*)))
           (push `(some ,attribute 
                        (boolean= ,+has-boolean-value+
                                  ,(if (eq value #t)
                                       #t
                                     #f)))
                 *stack*)))
        ((tag-equal token-string "defined")
         (let ((attribute (pop *stack*)))
           (push `(some ,attribute top) *stack*)))
        ((tag-equal token-string "ival")
         (let ((value (read-from-string (pop *stack*))))
           (if (integerp value)
               (push value *stack*)
             (error "No integer found in value expression."))))
        ((tag-equal token-string "fval")
         (let ((value (read-from-string (pop *stack*))))
           (if (floatp value)
               (push value *stack*)
             (error "No float found in value expression."))))
        ((tag-equal token-string "sval"))
        ((tag-equal token-string "bval")
         (let ((value (pop *stack*)))
           (cond ((string= value "true")
                  (push #t *stack*))
                 ((string= value "false")
                  (push #f *stack*))
                 (t (error "No boolean found in value expression.")))))
        ((tag-equal token-string "iset")
         (let ((individual-concepts 
                (loop for top in *stack*
                      until (eq top 'end)
                      collect top
                      do (pop *stack*))))
           (pop *stack*)
           (loop for ind in individual-concepts
                 unless (gethash ind (dig-racer-consumer-tbox-individuals self))
                 do
                 (add-concept-assertion (dig-racer-consumer-abox self)
                                        ind ind)
                 (setf (gethash ind (dig-racer-consumer-tbox-individuals self)) t))
           (push `(or .,individual-concepts) *stack*)))
        (t (error "Cannot handle tag ~A" token-string))))
     
(defun end-dig-management (self token-string)
  (cond ((tag-equal token-string "getIdentifier"))
        ((tag-equal token-string "useKB")
         (end-response-generation (parser-stream self)))
        ((tag-equal token-string "newKB")
         (end-response-generation (parser-stream self)))
        ((tag-equal token-string "racer")
         (end-response-generation (parser-stream self)))
        ((tag-equal token-string "deleteKBs")
         (end-response-generation (parser-stream self)))
        ((tag-equal token-string "deleteKB")
         nil)
        ((tag-equal token-string "releaseKB")
         (end-response-generation (parser-stream self)))))

(defun end-dig-telling (self token-string)
  (cond ((tag-equal token-string "tells") 
         (unless (error-during-parse self)
           (write-string "<ok/>" (parser-stream self))
           (terpri (parser-stream self)))
         (end-response-generation (parser-stream self))
         (setf *stack* nil)
         (setf (parser-state self) nil))
        ((tag-equal token-string "clearKB"))
        ((tag-equal token-string "defconcept"))
        ((tag-equal token-string "defrole"))
        ((tag-equal token-string "defannotation"))
        ((tag-equal token-string "deffeature"))
        ((tag-equal token-string "defattribute"))
        ((tag-equal token-string "defindividual"))
        ((tag-equal token-string "impliesc")
         (xml-add-implication (second *stack*) (first *stack*) (dig-racer-consumer-tbox self)))
        ((tag-equal token-string "equalc")
         (xml-add-equation (second *stack*) (first *stack*) (dig-racer-consumer-tbox self)))
        ((tag-equal token-string "disjoint")
         (let ((disjoints (loop for top in *stack*
                                until (eq top 'end)
                                collect top
                                do (pop *stack*))))
           (pop *stack*)
           (if (every #'symbolp disjoints)
               (declare-disjoint disjoints (dig-racer-consumer-tbox self))
             (declare-disjoint-concepts disjoints (dig-racer-consumer-tbox self)))))
        ((tag-equal token-string "alldifferent")
         (let ((different-inds (loop for top in *stack*
                                     until (eq top 'end)
                                     collect top
                                     do (pop *stack*))))
           (pop *stack*)
           (declare-disjoint different-inds (dig-racer-consumer-tbox self))
           (add-all-different-assertion (dig-racer-consumer-abox self)
                                        different-inds)))
        ((tag-equal token-string "differentfrom")
         (let ((different-inds (loop for top in *stack*
                                     until (eq top 'end)
                                     collect top
                                     do (pop *stack*))))
           (when (rest (rest different-inds))
             (error "differentfrom contains more than 2 individual names: ~S"
                    different-inds))
           (pop *stack*)
           (declare-disjoint different-inds (dig-racer-consumer-tbox self))
           (add-different-from-assertion (dig-racer-consumer-abox self)
                                         (first different-inds)
                                         (second different-inds))))
        ((tag-equal token-string "sameas")
         (let ((same-inds (loop for top in *stack*
                                until (eq top 'end)
                                collect top
                                do (pop *stack*))))
           (when (rest (rest same-inds))
             (error "sameas contains more than 2 individual names: ~S"
                    same-inds))
           (pop *stack*)
           (xml-add-equation (first same-inds) (second same-inds)
                             (dig-racer-consumer-tbox self))
           (add-same-individual-as-assertion (dig-racer-consumer-abox self)
                                             (first same-inds)
                                             (second same-inds))))
        ((tag-equal token-string "impliesr")
         (role-has-parent (second *stack*) (first *stack*) (dig-racer-consumer-tbox self)))
        ((tag-equal token-string "equalr")
         (let ((role2 (first *stack*))
               (role1 (second *stack*))
               (tbox (dig-racer-consumer-tbox self)))
           (ensure-role role1 tbox)
           (ensure-role role2 tbox)
           (cond ((and (symbolp role1)
                       (consp role2) 
                       (eq (first role2) 'inv)) 
                  (inverse-of-role role1 (second role2) tbox))
                 ((and (symbolp role2)
                       (consp role1) 
                       (eq (first role1) 'inv))
                  (inverse-of-role role2 (second role1) tbox))
                 ((and (consp role1) 
                       (eq (first role1) 'inv)
                       (consp role2)
                       (eq (first role2) 'inv))
                  (roles-equivalent-1 (second role1) (second role2) tbox))
                 (t (roles-equivalent-1 role1 role2 tbox)))))
        ((tag-equal token-string "domain")
         (role-has-domain (second *stack*) (first *stack*) (dig-racer-consumer-tbox self) nil))
        ((tag-equal token-string "range")
         (role-has-range (second *stack*) (first *stack*) (dig-racer-consumer-tbox self) nil))
        ((tag-equal token-string "transitive")
         (role-is-transitive (first *stack*) (dig-racer-consumer-tbox self)))
        ((tag-equal token-string "functional")
         (role-is-functional (first *stack*) (dig-racer-consumer-tbox self)))
        ((tag-equal token-string "rangeint")
         (datatype-role-has-range (first *stack*) 'integer (dig-racer-consumer-tbox self)))
        ((tag-equal token-string "rangestring")
         (datatype-role-has-range (first *stack*) 'string (dig-racer-consumer-tbox self)))
        ((tag-equal token-string "rangefloat")
         (datatype-role-has-range (first *stack*) 'real (dig-racer-consumer-tbox self)))
        ((tag-equal token-string "rangeboolean")
         (datatype-role-has-range (first *stack*) 'boolean (dig-racer-consumer-tbox self)))
        ((tag-equal token-string "instanceof")
         (let* ((entries (reverse (loop for top in *stack*
                                        until (eq top 'end)
                                        collect top
                                        do (pop *stack*))))
                (instance (first entries))
                (concept `(and .,(rest entries))))
           (add-concept-assertion (dig-racer-consumer-abox self)
                                  instance
                                  concept)))
        ((tag-equal token-string "related")
         (add-role-assertion (dig-racer-consumer-abox self) 
                             (third *stack*) (first *stack*) (second *stack*)))
        ((tag-equal token-string "value")
         (let ((value (pop *stack*))
               (attribute (pop *stack*))
               (ind (pop *stack*)))
           (cond ((integerp value)
                  (add-datatype-role-filler
                   (dig-racer-consumer-abox self)
                   ind 
                   value
                   attribute))
                 ((floatp value)
                  (add-datatype-role-filler
                   (dig-racer-consumer-abox self)
                   ind 
                   value
                   attribute))
                 ((stringp value)
                  (add-datatype-role-filler
                   (dig-racer-consumer-abox self)
                   ind 
                   value
                   attribute))
                 ((eq value #t)
                  (add-datatype-role-filler
                   (dig-racer-consumer-abox self)
                   ind 
                   value
                   attribute))
                 ((eq value #f)
                  (add-datatype-role-filler
                   (dig-racer-consumer-abox self)
                   ind 
                   value
                   attribute))
                 (t (error "Only integers, floats, booleans, or strings expected as values.")))))
        (t (end-parse-concept-or-role self token-string))))
              
(defun end-dig-asking (self token-string)
  (cond ((tag-equal token-string "asks")
         (end-responses-generation (parser-stream self))
         (setf *stack* nil)
         (setf (parser-state self) nil))
        ((tag-equal token-string "satisfiable")
         (let ((concept-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (concept-satisfiable-p concept-name (dig-racer-consumer-tbox self))
                              'boolean)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "subsumes")
         (let ((concept-name2 (pop *stack*))
               (concept-name1 (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (concept-subsumes-p concept-name1
                                                     concept-name2
                                                     (dig-racer-consumer-tbox self))
                              'boolean)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "equivalent")
         (let ((concept-name2 (pop *stack*))
               (concept-name1 (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (concept-equivalent-p concept-name1
                                                       concept-name2
                                                       (dig-racer-consumer-tbox self))
                              'boolean)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "disjoint")
         (let ((concept-name2 (pop *stack*))
               (concept-name1 (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (concept-disjoint-p concept-name1
                                                     concept-name2
                                                     (dig-racer-consumer-tbox self))
                              'boolean)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "parents")
         (let ((concept-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (atomic-concept-parents concept-name 
                                                         (dig-racer-consumer-tbox self))
                              'concept-sets)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "taxonomy")
         (let ((id (pop *stack*)))
           (with-error-handling
             (generate-taxonomy (parser-stream self)
                                id
                                (dig-racer-consumer-tbox self))
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "incrementalTaxonomy")
         (let ((id (pop *stack*)))
           (let ((old-snapshot (tbox-taxonomy-snapshot (dig-racer-consumer-tbox self)))
                 (new-snapshot (taxonomy (dig-racer-consumer-tbox self))))
             (setf (tbox-taxonomy-snapshot (dig-racer-consumer-tbox self)) new-snapshot)
             (with-error-handling
               (if (null old-snapshot)
                 (generate-taxonomy (parser-stream self)
                                    id
                                    (dig-racer-consumer-tbox self))
                 (generate-incremental-taxonomy (parser-stream self)
                                                id
                                                old-snapshot new-snapshot))
               (error (c)
                      (generate-error self id c))))))
        ((tag-equal token-string "children")
         (let ((concept-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (atomic-concept-children concept-name 
                                                          (dig-racer-consumer-tbox self))
                              'concept-sets)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "ancestors")
         (let ((concept-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (atomic-concept-ancestors concept-name 
                                                           (dig-racer-consumer-tbox self))
                              'concept-sets)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "descendants")
         (let ((concept-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (atomic-concept-descendants concept-name 
                                                             (dig-racer-consumer-tbox self))
                              'concept-sets)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "equivalents")
         (let ((concept-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (atomic-concept-synonyms concept-name 
                                                          (dig-racer-consumer-tbox self))
                              'concept-set)
             (error (c) 
                    (generate-error self id c)))))
        ((tag-equal token-string "rsubsumes")
         (let ((role-term-1 (pop *stack*))
               (role-term-2 (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (role-subsumes-p role-term-2 role-term-1 
                                                  (dig-racer-consumer-tbox self))
                              'role-set)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "requivalent")
         (let ((role-term-1 (pop *stack*))
               (role-term-2 (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (role-equivalent-p role-term-2 role-term-1 
                                                    (dig-racer-consumer-tbox self))
                              'role-set)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "rparents")
         (let ((role-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (atomic-role-parents role-name 
                                                      (dig-racer-consumer-tbox self))
                              'role-set)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "rchildren")
         (let ((role-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (atomic-role-children role-name 
                                                       (dig-racer-consumer-tbox self))
                              'role-set)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "rancestors")
         (let ((role-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (atomic-role-ancestors role-name 
                                                        (dig-racer-consumer-tbox self))
                              'role-set)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "rdescendants")
         (let ((role-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (atomic-role-descendants role-name 
                                                          (dig-racer-consumer-tbox self))
                              'role-set)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "requivalents")
         (let ((role-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (atomic-role-synonyms role-name 
                                                       (dig-racer-consumer-tbox self))
                              'role-set)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "consistentKB")
         (let ((id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (abox-consistent-p 
                                  (dig-racer-consumer-abox self))
                              'boolean)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "instances")
         (let ((concept-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (retrieve-concept-instances concept-name 
                                                             (dig-racer-consumer-abox self))
                              'individual-set)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "types")
         (let ((individual-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (most-specific-instantiators individual-name 
                                                              (dig-racer-consumer-abox self))
                              'concept-sets)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "roleFillers")
         (let ((role-name (pop *stack*))
               (individual-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (retrieve-individual-fillers individual-name
                                                              role-name
                                                              (dig-racer-consumer-abox self))
                              'individual-set)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "relatedIndividuals")
         (let ((role-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (retrieve-related-individuals role-name
                                                               (dig-racer-consumer-abox self))
                              'ind-pair-set)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "instance")
         (let ((concept (pop *stack*))
               (individual-name (pop *stack*))
               (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (member-of-concept-p (dig-racer-consumer-abox self)
                                                      individual-name
                                                      concept)
                              'boolean)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "toldValues")
         (let* ((attribute (pop *stack*))
                (ind (pop *stack*))
                (id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id
                              (retrieve-individual-told-datatype-fillers
                               ind
                               attribute
                               nil
                               (dig-racer-consumer-abox self)) 
                              'values)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "allConceptNames")
         (let ((id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (all-equivalent-concepts (dig-racer-consumer-tbox self))
                              'concept-sets)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "allRoleNames")
         (let ((id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (loop for role-spec in 
                                       (all-roles (dig-racer-consumer-tbox self))
                                       when (symbolp role-spec) 
                                       collect role-spec)
                              'role-set)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "allIndividuals")
         (let ((id (pop *stack*)))
           (with-error-handling
             (generate-result (parser-stream self)
                              id (all-individuals (dig-racer-consumer-abox self))
                              'individual-set)
             (error (c)
                    (generate-error self id c)))))
        ((tag-equal token-string "retrieve")
         (let (head 
               body) 
           (unless (eq (first *stack*) 'end)
             (setf body (pop *stack*))
             (setf head (pop *stack*)))
           (pop *stack*) 
           (let ((ntuples (pop *stack*))
                 (id (pop *stack*)))
             (if (null head)
               (with-error-handling
                 (generate-result (parser-stream self)
                                  id (dig-retrieve id ntuples head body)
                                  'boolean)
                 (error (c)
                        (generate-error self id c)))  
               (with-error-handling
                 (generate-result (parser-stream self)
                                  id (list head 
                                           (dig-retrieve id ntuples head body))
                                  'query-result)
                 (error (c)
                        (generate-error self id c)))))))
        ((tag-equal token-string "head")
         (let ((head (loop for entry in *stack*
                           until (eq entry 'end) 
                           do (pop *stack*)
                           collect entry)))
           (pop *stack*)
           (push head *stack*)))
        ((tag-equal token-string "body")
         )
        ((tag-equal token-string "indvar")
         )
        ((tag-equal token-string "ind")
         )
        ((tag-equal token-string "attrValue")
         (let ((var (pop *stack*))
               (attribute-value-spec (pop *stack*)))
           (push (append attribute-value-spec (list var))
                 *stack*)))
        ((tag-equal token-string "qand") 
         )
        ((tag-equal token-string "qunion")
         (let ((query-conjunction
                (loop for entry in *stack*
                      until (eq entry 'end) 
                      do (pop *stack*)
                      collect entry)))
           (pop *stack*)
           (push `(qunion .,query-conjunction) *stack*)))
        ((tag-equal token-string "qneg")
         (push `(qneg ,(pop *stack*)) *stack*))
        ((tag-equal token-string "cqatom") 
         (let ((concept (pop *stack*))
               (var (pop *stack*)))
           (push `(cqatom ,var ,concept) *stack*)))
        ((tag-equal token-string "rqatom")
         (let ((role (pop *stack*))
               (var2 (pop *stack*))
               (var1 (pop *stack*)))
           (push `(rqatom ,var1 ,var2 ,role) *stack*)))
        (t (end-parse-concept-or-role self token-string))))

(defmethod nox:end-element ((self dig-racer) tag mode)
  (declare (ignore mode))
  (with-error-handling
   (let ((token-string (nox:token-string tag)))
        ;;(format t "~&END ~A ~S" token-string mode)
        
        (cond ((or (eq (parser-state self) ':management)
                   (null (parser-state self)))
               (end-dig-management self token-string))

              ((eq (parser-state self) ':telling)
               (end-dig-telling self token-string))
             
              ((eq (parser-state self) ':asking)
               (end-dig-asking self token-string))
             
              (t (error "Cannot handle tag ~A" token-string))))

    (error (c)
           (generate-error-2 self c))))



(defmethod nox:char-content ((self dig-racer) char-content mode)
  (declare (ignore mode))
  (push (string-trim '(#\Space #\Tab #\Newline) char-content) *stack*))

(defmethod nox:end-document ((self dig-racer) mode)
  (declare (ignorable self mode))
  ;(format t "~&END DOCUMENT ~S" mode)
  )

(defmethod nox:proc-instruction ((self dig-racer) (tag nox:proc-instruction) mode)
  (declare (ignore mode))
  ;(format t "~&Ignoring: PI ~S ~S" (nox:token-string tag) mode)
  ;(format t "~&PI ~S ~S" (nox:token-string tag) mode)
  )


(defun print-input-string (input-string)
  (let ((length (length input-string))
        (i 0))
    (loop until (> i length) do
          (loop for j from 1 to 80 
                until (> i length) do
                (princ (aref input-string i))
                (incf i)))))

#|
(defun count-chars ()
  (with-open-file (in "hd:racer-users:sean bechhofer:kb1:kb.xml" :direction :input)
    (let ((chars 0)
          (newlines 0)
          (linefeeds 0))
      (loop for char = (char-code (read-byte in nil nil)
            until (null char) do
            (cond ((char= char #\newline)
                   (incf newlines))
                  ((char= char #\linefeed)
                   (incf linefeeds))
                  (t (incf chars))))
      (values chars newlines linefeeds)))))
|#


(defun dig-eval-request (req ent)
  (with-racer-critical-section
    (racer:new-request "Evaluating DIG request...")
    (racer:set-progress-value 1)
    (dig-eval-request-internal 'dig-racer req ent)
    (racer:clear-request)))

#-:aserve
(defun dig-eval-request-internal (parser-class socket-stream url)
  #-:cl-http (declare (ignore url socket-stream))
  (let ((*tbox-verbose* nil)
        (*abox-verbose* nil)
        (*auto-classify* ':lazy)
        (*auto-realize* ':lazy)
        (*read-eval* nil)
        (*read-default-float-format* 'double-float)
        (*package* *racer-user-package*)
        (*response-end-tag* nil)
        (logging *tcp-console-logging*)
        dig-racer)
    (declare (ignorable dig-racer))
    (with-logging (socket-stream)
      #+:cl-http
      (let* ((length (http:get-header :content-length))
             (i 0)
             char)
        (if (and length (> length 0))
          (flet ((eval (stream)
                   (http:with-successful-response (socket-stream 
                                                   :xml
                                                   :content-location url
                                                   ;;:expires (url:expiration-universal-time url)
                                                   ;;:cache-control (url:response-cache-control-directives url)
                                                   ;;:content-language (languages url)
                                                   )
                     (let ((*response-end-tag* nil))
                       (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" socket-stream)
                       (terpri socket-stream)
                       (with-racer-critical-section
                         (handler-case
                           (nox:parse-from-stream stream url 
                                                  'nox:xml-parser 
                                                  :consumer (setf dig-racer
                                                                  (make-instance parser-class 
                                                                    :url url
                                                                    :stream socket-stream
                                                                    :tbox-default-name 'default
                                                                    :abox-default-name 'default)))
                           (error (c) 
                                  (format socket-stream "<~A=\"~A\"/>" *error-indicator* (transform-to-html c))
                                  (terpri socket-stream)
                                  (when *response-end-tag*
                                    (princ *response-end-tag* socket-stream)
                                    (terpri socket-stream)))))))))
            (if (< length 10000)
              (let ((input-string (and length (make-string length))))
                (loop (setf char (code-char (read-byte socket-stream)))
                      (setf (aref input-string i) char)
                      (incf i 1)
                      (when (>= i length)
                        (return)))
                (when logging 
                  (princ input-string *trace-output*)
                  (terpri *trace-output*))
                (with-input-from-string (stream input-string)
                  (eval stream)))
              (with-temp-file ((stream)
                               (loop (setf char (code-char (read-byte socket-stream)))
                                     (when logging (princ char *trace-output*))
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
            (format socket-stream "<error message=\"Message from client does not specify content length or content lenght is zero.\"/>")
            (terpri socket-stream)))))))


#+:aserve
(defun dig-eval-request-internal (parser-class req ent)
  (let ((*tbox-verbose* nil)
        (*abox-verbose* nil)
        (*auto-classify* ':lazy)
        (*auto-realize* ':lazy)
        (*read-eval* nil)
        (*read-default-float-format* 'double-float)
        (*package* *racer-user-package*)
        (*response-end-tag* nil)
        (logging *tcp-console-logging*))
    (let ((url (net.aserve:request-uri req))
          (content-encoding (net.aserve:header-slot-value 
			     req :content-encoding))
          input-string)
      (cond ((string= content-encoding "gzip")
             #+:allegro 
              (let* ((body (net.aserve:get-request-body req))
		     (so (make-string-output-stream))
		     (bis (make-buffer-input-stream body))
		     (bi))
			      
                (util.zip:skip-gzip-header bis)
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
	    (t (setf input-string (net.aserve:get-request-body req))))
      (when *log-file*
        (with-open-file (log-stream *log-file* :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
          (princ input-string log-stream)
          (terpri log-stream)))
      (when logging
        (princ input-string *trace-output*)
        (terpri *trace-output*))
      (with-input-from-string (stream input-string)
	(net.aserve:with-http-response (req ent :content-type "text/xml")
	  (net.aserve:with-http-body (req ent)
	      (let ((*response-end-tag* nil)
		    (socket-stream net.html.generator:*html-stream*))
                (with-logging (socket-stream)
		  (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" socket-stream)
		  (terpri socket-stream)
		  (with-racer-critical-section
		    (handler-case
                      (nox:parse-from-stream 
                       stream url 
                       'nox:xml-parser 
                       :consumer (make-instance parser-class
                                   :url url
                                   :stream socket-stream
                                   :tbox-default-name 'default
                                   :abox-default-name 'default))
		      (error (c) 
			     (format socket-stream 
				     "<error message=\"~A\"/>" (transform-to-html c))
			     (terpri socket-stream)
			     (when *response-end-tag*
			       (princ *response-end-tag* socket-stream)
			       (terpri socket-stream)))))))))))))

            

(defun start-responses-generation (stream) 
  (terpri stream)
  (setf *response-end-tag* "</responses>")
  (write-string "<responses
     xmlns=\"http://dl.kr.org/dig/2003/02/lang\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:schemaLocation=\"http://dl.kr.org/dig/2003/02/lang
     http://dl-web.man.ac.uk/dig/2003/02/dig.xsd\">"
                stream)
  (terpri stream))

(defun start-response-generation (stream) 
  (terpri stream)
  (setf *response-end-tag* "</response>")
  (write-string "<response
     xmlns=\"http://dl.kr.org/dig/2003/02/lang\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:schemaLocation=\"http://dl.kr.org/dig/2003/02/lang
     http://dl-web.man.ac.uk/dig/2003/02/dig.xsd\">"
                stream)
  (terpri stream))

(defun end-responses-generation (stream)
  (write-string "</responses>" stream)
  (terpri stream))

(defun end-response-generation (stream)
  (write-string "</response>" stream)
  (terpri stream))


(defun generate-atom (concept-name stream)
  (unless (or (eq concept-name '*bottom*)
              (eq concept-name '*top*)) 
          (cond ((eq concept-name 'top)
                 (write-string " <top/>" stream))
                ((eq concept-name 'bottom)
                 (write-string " <bottom/>" stream))
                (t (format stream " <catom name=\"~A\"/>"
                           (symbol-name concept-name))))))

(defun generate-taxonomy (stream id tbox)
  (format stream "<taxonomy id=\"~A\">" id)
  (terpri stream)
  (loop for concept in (tbox-encoded-concept-list tbox) do
        (loop for concept-name in (concept-name-set concept) do
              (format stream "<concept>")
              (terpri stream)
              (generate-atom concept-name stream)
              (terpri stream)
              (unless (eq (first (concept-parents-internal concept))
                          (tbox-top-node tbox))
                (format stream "<parents>")
                (loop for parent in (concept-parents-internal concept) do
                      (loop for parent-name in (concept-name-set parent) do
                            (terpri stream)
                            (generate-atom parent-name stream)))
                (terpri stream)
                (write-string "</parents>" stream)
                (terpri stream))
              (format stream "</concept>")
              (terpri stream)))
  (format stream "</taxonomy>")
  (terpri stream))

(defun generate-incremental-taxonomy (stream id taxonomy-1 taxonomy-2)
  (format stream "<incrementalTaxonomy id=\"~A\">" id)
  (terpri stream)
  (when taxonomy-1
    (loop for entry-1 in taxonomy-1
          for entry-2 in taxonomy-2
          as diff-1 = (set-difference (second entry-2)
                                      (second entry-1))
          as diff-2 = (set-difference (second entry-1)
                                      (second entry-2))
          unless (and (null diff-1) (null diff-2))
          do
          (format stream "<concept>")
          (terpri stream)
          (generate-atom (first entry-1) stream)
          (terpri stream)
          (when diff-1
            (format stream "<addedParents>")
            (terpri stream)
            (loop for added-parent in diff-1
                  do
                  (generate-atom added-parent stream))
            (terpri stream)
            (format stream "</addedParents>")
            (terpri stream))
          (when diff-2
            (format stream "<removedParents>")
            (terpri stream)
            (loop for added-parent in diff-2
                  do
                  (generate-atom added-parent stream))
            (terpri stream)
            (format stream "</removedParents>")
            (terpri stream))
          (format stream "</concept>")
          (terpri stream)))
  (format stream "</incrementalTaxonomy>")
  (terpri stream))
          

(defun generate-result (stream query-id result result-type)
  (ecase result-type
    (boolean
     (cond ((eq result t)
            (format stream "<true id=\"~A\"/>" query-id)
            (terpri stream))
           ((eq result nil)
            (format stream "<false id=\"~A\"/>" query-id)
            (terpri stream))
           (t (error "should not happen."))))
    (concept-sets
     (format stream "<conceptSet id=\"~A\">" query-id)
     (terpri stream)
     (loop for concept-set in result do
           (write-string "<synonyms>" stream)
           (loop for concept in concept-set 
                 unless (or (eq concept '*bottom*)
                            (eq concept '*top*)) do
                 (cond ((eq concept 'top)
                        (write-string " <top/>" stream))
                       ((eq concept 'bottom)
                        (write-string " <bottom/>" stream))
                       (t (format stream " <catom name=\"~A\"/>"
                                  (symbol-name concept)))))
           (write-string " </synonyms>" stream)
           (terpri stream))
     (write-string "</conceptSet>" stream)
     (terpri stream))
    (concept-set
     (format stream "<conceptSet id=\"~A\">" query-id)
     (terpri stream)
     (write-string "<synonyms>" stream)
     (loop for concept in result 
           unless (or (eq concept '*bottom*)
                      (eq concept '*top*)) do
           (cond ((eq concept 'top)
                  (write-string " <top/>" stream))
                 ((eq concept 'bottom)
                  (write-string " <bottom/>" stream))
                 (t (format stream " <catom name=\"~A\"/>"
                            (symbol-name concept)))))
     (write-string " </synonyms>" stream)
     (terpri stream)
     (write-string "</conceptSet>" stream)
     (terpri stream))
    (role-set
     (format stream "<roleSet id=\"~A\">" query-id)
     (terpri stream)
     (loop for role in result do
           (write-string "<synonyms>" stream)
           (loop for synonym in (atomic-role-synonyms role (current-tbox)) do
                 (when (symbolp synonym)
                   (format stream " <ratom name=\"~A\"/>"
                           (symbol-name synonym))))
           (write-string " </synonyms>" stream)
           (terpri stream))
     (write-string "</roleSet>" stream)
     (terpri stream))
    (individual-set
     (format stream "<individualSet id=\"~A\">" query-id)
     (loop for individual in result do
           (format stream " <individual name=\"~A\"/>"
                   (symbol-name individual)))
     (write-string " </individualSet>" stream)
     (terpri stream))
    (ind-pair-set
     (format stream "<individualPairSet id=\"~A\">" query-id)
     (loop for (individual1 individual2) in result do
           (format stream " <individualPair> <individual name=\"~A\"/> <individual name=\"~A\"/> </individualPair>"
                   (symbol-name individual1)
                   (symbol-name individual2))
           (terpri stream))
     (write-string " </individualPairSet>" stream)
     (terpri stream))
    (values
     (format stream "   <valueSet id=\"~A\">" query-id)
     (terpri stream)
     (loop for value in result do
           (etypecase value
             (integer 
              (format stream "      <ival>")
              (princ value stream) 
              (write-string "</ival>" stream)
              (terpri stream))
             (float
              (format stream "      <fval>")
              (princ value stream) 
              (write-string "</fval>" stream)
              (terpri stream))
             (racer-boolean 
              (format stream "      <bval>")
              (if (eq value #t)
                (princ "true" stream)
                (princ "false" stream))
              (write-string "</bval>" stream)
              (terpri stream))
             (string
              (format stream "      <sval>")
              (princ value stream)
              (write-string "</sval>" stream)
              (terpri stream))))
     (format stream "   </valueSet>")
     (terpri stream))
    (query-result
     (let ((head (reverse (first result)))
           (tuples (second result)))
       (format stream "<bindings id=\"~A\">" query-id)
       (terpri stream)
       (format stream "<head>")
       (terpri stream)
       (print-head head stream)
       (format stream "</head>")
       (terpri stream)
       (loop for tuple in tuples do
             (write-string "<tuple>" stream)
             (loop for (nil value) in (reverse tuple) do
                   (etypecase value
                     (integer 
                      (format stream "<ival>")
                      (princ value stream) 
                      (write-string "</ival> " stream))
                     (float
                      (format stream "<fval>")
                      (princ value stream) 
                      (write-string "</fval> " stream))
                     (racer-boolean 
                      (format stream "<bval>")
                      (if (eq value #t)
                        (princ "true" stream)
                        (princ "false" stream))
                      (write-string "</bval> " stream))
                     (string
                      (format stream "<sval>")
                      (princ value stream)
                      (write-string "</sval> " stream))
                     (null (format stream "<valueSet/>"))
                     (symbol 
                      (format stream "<individual name=\"")
                      (princ value stream)
                      (write-string "\"/>" stream))
                     (cons (format stream "<valueSet>")
                            (loop for value in value do
                                  (etypecase value
                                    (integer 
                                     (format stream "<ival>")
                                     (princ value stream) 
                                     (write-string "</ival>" stream))
                                    (float
                                     (format stream "<fval>")
                                     (princ value stream) 
                                     (write-string "</fval>" stream))
                                    (racer-boolean 
                                     (format stream "<bval>")
                                     (if (eq value #t)
                                       (princ "true" stream)
                                       (princ "false" stream))
                                     (write-string "</bval>" stream))
                                    (string
                                     (format stream "<sval>")
                                     (princ value stream)
                                     (write-string "</sval>" stream))))
                            (format stream "</valueSet>"))))
             (write-string "</tuple>" stream)
             (terpri stream))
       ;;(terpri stream)
       (write-string "</bindings>" stream)
       (terpri stream)))))

(defun print-head (head stream)
  (if (null head)
    nil
    (let ((entry (first head)))
      (ecase (first entry)
        (variable (format stream "<indvar name=\"~A\"/>" (second entry)))
        (individual (format stream "<individual name=\"~A\"/>" (second entry)))
        (attribute-value 
         (format stream "<attrValue name=\"~A\">" (second entry))
         (format stream "<indvar name=\"~A\"/>" (second (third entry)))
         (format stream "</attrValue>")))
      (terpri stream)
      (print-head (rest head) stream))))



(defun generate-error (parser query-id c)
  (let ((stream (parser-stream parser)))
    (setf (error-during-parse parser) t)
    (format stream "<error id=\"~A\" message=\"~A\"/>" query-id (transform-to-html c))
    (terpri stream)))

(defun generate-error-2 (parser c)
  (setf (error-during-parse parser) t)                        
  (let ((stream (parser-stream parser)))
    (format stream "<error message=\"~A\"/>" (transform-to-html c))
    (terpri stream)))

(defun identify-racer (stream)
  (format stream
          "<identifier 
      xmlns=\"http://dl.kr.org/dig/2003/02/lang\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"http://dl.kr.org/dig/2003/02/lang
      http://dl-web.man.ac.uk/dig/2003/02/dig.xsd\"
      name=\"Racer\"
      version=\"~A\"
      message=\"Racer running on ~A.\">
   <supports>
       <language>
          <top/> <bottom/> <catom/> <ratom/>
          <and/> <or/> <not/>
          <some/> <all/>
          <atmost/>
          <atleast/>
          <iset/>
          <inverse/>
          <feature/>
          <attribute/>
          <intmin/>
          <intmax/>
          <intrange/>
          <intequals/>
          <floatmin/>
          <floatmax/>
          <floatrange/>
          <floatequals/>
          <stringequals/>
          <booleanequals/>
          <defined/>
          <qand/>
          <qunion/>
          <qneg/>
          <project/>
          <cqatom/>
          <rqatom/>
          <var/>
          <attrValue/>
          <notifier/>
       </language>
       <racer/>
       <newKB/>
       <useKB/>
       <deleteKB/>
       <deleteKBs/>
       <releaseKB/>
       <tell>
          <defconcept/>
          <defrole/>
          <defannotation/>
          <deffeature/>
          <defattribute/>
          <defindividual/>
          <impliesc/>
          <equalc/>
          <disjoint/>
          <impliesr/>
          <equalr/>
          <domain/>
          <range/>
          <rangeint/>
          <rangefloat/>
          <rangestring/>
          <rangeboolean/>
          <transitive/>
          <functional/>
          <instanceof/>
          <related/>
          <value/>
          <alldifferent/>
          <differentfrom/>
          <sameas/>
       </tell>
       <ask>
          <allConceptNames/>
          <allRoleNames/>
          <allIndividuals/>
          <satisfiable/>
          <subsumes/>
          <equivalent/>
          <disjoint/>
          <parents/>
          <children/>
          <descendants/>
          <ancestors/>
          <equivalents/>
          <rsubsumes/>
          <requivalent/>
          <rparents/>
          <rchildren/>
          <rancestors/>
          <rdescendants/>
          <instances/>
          <types/>
          <instance/>
          <roleFillers/>
          <relatedIndividuals/>
          <toldValues/>
          <consistentKB/>
          <requivalents/>
          <retrieve/>
          <taxonomy/>
          <incrementalTaxonomy/>
       </ask>
    </supports>
</identifier>"
          (concatenate 'string (get-product-name) " " (get-product-version) 
                       " DIG " (if (eq *dig-version* +dig-1-2+)
                                 "1.2"
                                 "1.1"))
          #+:cl-http (http::local-context)
          #-:cl-http nil)
  (terpri stream))


(defun create-uuid ()
  (concatenate 'string 
               "http://dl.kr.org/dig/"
               (symbol-name (gensym "kb-"))))

(defun create-new-kb (stream)
  (let* ((uuid (create-uuid))
         (kb-name (intern uuid)))
    (setf (find-kb-name-from-uri uuid) kb-name)
    (format stream
            "   <kb uri=\"~A\"/>~%" uuid)))


(defun release-kb (stream attributes)
  (declare (ignore stream))
  (let ((uri (find-dig-attribute "uri" attributes nil)))
    (multiple-value-bind (kb-name found)
                         (find-kb-name-from-uri uri)
      (cond (kb-name
             (setf (find-kb-name-from-uri uri) nil))
            ((null found) 
             (error "No such knowledge base known: ~A" uri))
            (t (error "Knowledge base ~A was already released." uri))))))


(defun setup-kb (self attributes)
  (let ((uri (find-dig-attribute "uri" attributes nil)))
    (when uri
      (multiple-value-bind (kb-name found)
                           (find-kb-name-from-uri uri)
        (cond (kb-name
               (setf (dig-racer-consumer-tbox self) 
                     (or (find-tbox kb-name nil)
                         (xml-define-tbox kb-name)))
               (setf (dig-racer-consumer-abox self) 
                     (or (find-abox kb-name nil)
                         (xml-define-abox kb-name kb-name))))
              ((null found) 
               (error "No such knowledge base known: ~A" uri))
              (t (error "Knowledge base ~A was released." uri)))))))

;;; ======================================================================


(defun dig-retrieve (id ntuples head body &optional (kb (current-abox)))
  (declare (ignore ntuples))
  ;;(print (list id ntuples head body))
  (racer-answer-query (compute-result-args head)
                      (compute-body body)
                      :id id
                      :abox kb))

(defun compute-result-args (head)
  (loop for entry in head 
        collect 
        (ecase (first entry)
          (attribute-value `(told-value 
                             (,(second entry) ,(compute-head-variable (second (third entry))))))
          (variable (compute-head-variable (second entry)))
          (individual (compute-individual (second entry))))))

(defun compute-head-variable (dig-var)
  (intern (concatenate 'string "?" (string-upcase (symbol-name dig-var)))))

(defun compute-variable (dig-var)
  (intern (concatenate 'string "?" (string-upcase (symbol-name dig-var)))))

(defun compute-individual (dig-individual)
  dig-individual)

(defun compute-body (body)
  (case (first body)
    (qand `(and .,(mapcar #'compute-body (rest body))))
    (qunion `(union .,(mapcar #'compute-body (rest body))))
    (qneg `(neq ,(compute-body (second body))))
    (otherwise (compute-atom body))))

(defun compute-atom (query-atom)
  (ecase (first query-atom)
    (cqatom (list (compute-var-or-ind (second query-atom)) (third query-atom)))
    (rqatom (list (compute-var-or-ind (second query-atom))
                  (compute-var-or-ind (third query-atom))
                  (fourth query-atom)))))
(defun compute-var-or-ind (voi)
  (ecase (first voi)
    (variable (compute-variable (second voi)))
    (individual (compute-individual (second voi)))))


;;; ======================================================================

(defun dig-read-file (filename &key kb-name (init t))
  (with-error-handling
   (dig-read-file-internal 'dig-racer filename kb-name init)
   (error (c) 
          (format t "<error message=\"~A\"/>" (transform-to-html c))
          (terpri)
          (when *response-end-tag*
            (princ *response-end-tag*)
            (terpri)))))

(defun dig-read-file-internal (parser-class filename kb-name init &optional initargs)
  (let* ((stream *trace-output*)
         (*response-end-tag* nil)
         (*package* *racer-user-package*)
         (dig-racer (apply #'make-instance parser-class
                           :stream stream
                           :tbox-default-name (or kb-name 'default)
                           :abox-default-name (or kb-name 'default)
                           initargs)))
    (when kb-name
      (unless (find-tbox kb-name nil)
        (init-tbox kb-name)))
    (when init
      (init-tbox (or kb-name 'default)))
    (unless (probe-file filename)
      (error "File ~S does not exist." filename))
    (nox:parse-from-file filename 'nox:xml-parser 
                         :consumer dig-racer)
    nil))

(defun dig-read-document (url-spec &optional kb-name (init t))
  (handler-case
      (dig-read-document-internal 'dig-racer url-spec kb-name init)
    (error (c) 
      (format t "<error message=\"~A\"/>" (transform-to-html c))
      (terpri)
      (when *response-end-tag*
        (princ *response-end-tag*)
        (terpri)))))

(defun dig-read-document-internal (parser-class url-spec kb-name init &optional initargs)
  #-(and :racer-server (or :cl-http :aserve))
  (declare (ignore parser-class url-spec kb-name init initargs))
  #+(and :racer-server :cl-http)
  (let* ((verbose (getf initargs ':verbose *tbox-verbose*))
         (url-spec (url-convenience-transformation url-spec))
         (real-url-spec (check-for-url-mirror url-spec))
         (url (url:intern-url real-url-spec))
         (*response-end-tag* nil)
         (*package* *racer-user-package*))
    (let ((*indent* 0))
      (etypecase url 
      (url::file-url 
       (when verbose
         (unless (string= url-spec real-url-spec)
           (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
       (dig-read-file-internal
        parser-class
        (make-pathname :directory `(:absolute .,(url:path url))
                       :name (url:object url)
                       :type (url:extension url))
        kb-name
        init
        initargs))
      (url:http-object 
       (when verbose
         (unless (string= url-spec real-url-spec)
           (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
       (with-temp-file ((stream)
                          (http:show-url url :stream stream))
           (when kb-name
             (unless (find-tbox kb-name nil)
               (init-tbox kb-name)))
           (when init
             (init-tbox (or kb-name 'default)))
           (nox:parse-from-stream
            stream 
            (http::object url)
            'nox:xml-parser
            :consumer (apply #'make-instance parser-class
                             :stream *standard-output*
                             :tbox-default-name (or kb-name 
                                                    (intern (string-upcase (http::object url))))
                             :abox-default-name (or kb-name
                                                    (intern (string-upcase (http::object url))))
                             initargs)))))))
  #+(and :racer-server :aserve)
  (let* ((verbose (getf initargs ':verbose *tbox-verbose*))
         (url-spec (url-convenience-transformation url-spec))
         (real-url-spec (check-for-url-mirror url-spec))
         (url (net.uri:parse-uri real-url-spec)))
    (let ((*indent* 0))
      (ecase (net.uri:uri-scheme url)
        (:file
         (let ((pathname (uri-to-pathname url)))
           (when verbose
             (unless (string= url-spec real-url-spec)
               (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
           (dig-read-file-internal parser-class pathname kb-name init)))
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
		  
                    (when kb-name
                      (unless (find-tbox kb-name nil)
                        (init-tbox kb-name)))
                    (when init
                      (init-tbox (or kb-name 'default)))
                    (nox:parse-from-stream
                     stream 
                     (net.uri:uri-path url)
                     'nox:xml-parser
                     :consumer (apply #'make-instance parser-class
                                      :stream *standard-output*
                                      :tbox-default-name (or kb-name 
                                                             (intern (net.uri:uri-path url)))
                                      :abox-default-name (or kb-name
                                                             (intern (net.uri:uri-path url)))
                                      initargs))))))))))
  #-(and :racer-server (or :cl-http :aserve))
  (error "No supported in this version.")
  nil)


(defun print-cl-http-info (&optional (stream t))
  (format stream ";;; The HTTP server interface based on DIG as well as the ~A OWL Client ~%~
                  ;;; is implemented with CL-HTTP. CL-HTTP is developed and~%~
                  ;;; owned by John C. Mallery. For more information on CL-HTTP see ~%~
                  ;;; http://www.ai.mit.edu/projects/iiip/doc/cl-http/home-page.html.~2%"
                  (get-product-name)))

