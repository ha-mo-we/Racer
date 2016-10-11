;;; -*- package: NOX; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  xml-util.lisp
;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   The contents of this file are subject to the NOKOS License Version 1.0a (the
;;;   "License"); you may not use this file except in compliance with the License. 
;;;
;;;   Software distributed under the License is distributed on an "AS IS" basis, WITHOUT
;;;   WARRANTY OF ANY KIND, either express or implied. See the License for the specific
;;;   language governing rights and limitations under the License. 
;;;
;;;   The Original Software is 
;;;     WILBUR2: Nokia Semantic Web Toolkit for CLOS
;;;
;;;   Copyright (c) 2001-2005 Nokia and others. All Rights Reserved.
;;;   Portions Copyright (c) 1989-1992 Ora Lassila. All Rights Reserved.
;;;
;;;   Contributor(s): Ora Lassila (mailto:ora.lassila@nokia.com)
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;
;;;   Version: $Id: xml-util.lisp,v 1.13 2004/11/28 23:15:00 ora Exp $
;;;
;;;   Purpose: This file contains useful functions and other definitions for
;;;   implementing an XML parser (or some other stuff, for that matter). These
;;;   are separated from the actual parser so that one could replace the actual
;;;   parser with another implementation (not me, but someone else might do it).
;;;


(in-package :nox)


;;; ----------------------------------------------------------------------------
;;;
;;;   XML CONDITION CLASSES
;;;
;;;   XML-ERROR                           abstract
;;;     SYNTAX-ERROR                      concrete
;;;       PI-TERMINATION-PROBLEM          concrete
;;;       DTD-TERMINATION-PROBLEM         concrete
;;;       UNEXPECTED-END-TAG              concrete
;;;       UNKNOWN-DECLARATION             concrete
;;;       UNKNOWN-CHARACTER-REFERENCE     concrete
;;;       MALFORMED-URL                   concrete
;;;     FEATURE-NOT-SUPPORTED             concrete
;;;     MISSING-DEFINITION                abstract
;;;       MISSING-ENTITY-DEFINITION       concrete, continuable
;;;       MISSING-NAMESPACE-DEFINITION    concrete, continuable
;;;   XML-WARNING                         concrete, warning
;;;

(define-condition wilbur-error (simple-error)
  ())

(define-condition xml-error (wilbur-error)
  ((thing
    :initarg :thing
    :reader error-thing))
  (:report (lambda (condition stream)
             (format stream (simple-condition-format-control condition)
                     (error-thing condition)))))

(define-condition syntax-error (xml-error)
  ()
  (:default-initargs
    :format-control "XML -- syntax error (why: ~:[unknown~;~:*~A~])"))

(define-condition pi-termination-problem (syntax-error)
  ()
  (:default-initargs
    :format-control "XML -- unterminated PI ~S"))

(define-condition dtd-termination-problem (syntax-error)
  ()
  (:default-initargs
    :format-control "XML -- improperly terminated DTD"))

(define-condition unexpected-end-tag (syntax-error)
  ((expectation
    :initarg :expectation
    :initform nil
    :reader error-expectation))
  (:report (lambda (condition stream)
             (format stream "XML -- unexpected end tag ~S~@[ (looking for ~S)~]"
                     (error-thing condition)
                     (error-expectation condition)))))

(define-condition unknown-declaration (syntax-error)
  ()
  (:default-initargs
    :format-control "XML -- unknown declaration ~S"))

(define-condition unknown-character-reference (syntax-error)
  ()
  (:default-initargs
    :format-control "XML -- unknown character reference ~S"))

(define-condition malformed-url (syntax-error)
  ()
  (:default-initargs
    :format-control "XML -- unparseable URL ~S"))

(define-condition feature-not-supported (xml-error)
  ()
  (:default-initargs
    :format-control "XML -- ~S is not supported"))

(define-condition missing-definition (xml-error)
  ((definition-type
    :initarg :type
    :reader error-definition-type))
  (:report (lambda (condition stream)
             (format stream "XML -- missing ~A definition ~S"
                     (error-definition-type condition)
                     (error-thing condition)))))

(define-condition missing-entity-definition (missing-definition)
  ()
  (:default-initargs
    :type :entity))

(define-condition missing-namespace-definition (missing-definition)
  ()
  (:default-initargs
    :type :namespace))

(define-condition xml-warning (simple-warning)
  ())

(defmacro xml-warning (message &rest args)
  `(warn 'xml-warning
         :format-control "XML -- ~?"
         :format-arguments (list ,message (list ,@args))))


;;; ----------------------------------------------------------------------------
;;;
;;;   RESOURCE POOLS
;;;

#-:ccl
(defstruct (resource-pool (:conc-name pool-))
  data
  constructor
  initializer 
  destructor)

(defvar *resource-pool-lock* 
  #+(and :lispworks (or :lispworks6 :lispworks7))
  (mp:make-lock :name "NOX Resource Pool")
  #+(and :allegro :smp-macros)
  (mp:make-sharable-lock :name "NOX Resource Pool")
  #+:ccl
  (ccl:make-read-write-lock))

#-:ccl
(defmacro nox-atomic-push (thing place)
  #+(and :lispworks (or :lispworks6 :lispworks7))
  `(mp:with-lock (*resource-pool-lock*)
     (push ,thing ,place))
  #+(and :allegro :smp-macros :allegro-v8.2)
  (let ((sym (gensym)))
    `(let ((,sym nil))
       (racer-with-exclusive-lock (*resource-pool-lock* ,sym)
         (setf ,sym (push ,thing ,place)))))
  #+(and :allegro :smp-macros (not :allegro-v8.2))
  `(push-atomic ,thing ,place)
  #-(or :lispworks (and :allegro :smp-macros))
  `(racer-without-interrupts
    (push ,thing ,place)))

#-:ccl
(defmacro nox-atomic-pop (place)
  #+(and :lispworks (or :lispworks6 :lispworks7))
  `(mp:with-lock (*resource-pool-lock*)
     (pop ,place))
  #+(and :allegro :smp-macros :allegro-v8.2)
  (let ((sym (gensym)))
    `(let ((,sym nil))
       (racer-with-exclusive-lock (*resource-pool-lock* ,sym)
         (setf ,sym (pop ,place)))))
  #+(and :allegro :smp-macros (not :allegro-v8.2))
  `(pop-atomic ,place)
  #-(or :lispworks (and :allegro :smp-macros))
  `(racer-without-interrupts
    (pop ,place))
  )

(defun allocate-resource-from-pool (pool &rest args)
  #+:ccl
  (declare (ignore args))
  #+:ccl
  (ccl::allocate-resource pool)
  #-:ccl
  (let ((res (or (nox-atomic-pop (pool-data pool))
                 (apply (pool-constructor pool) args)))
        (init (pool-initializer pool)))
    (when init
      (funcall init res))
    res))

(defun free-resource-to-pool (pool resource)
  #+:ccl
  (ccl::free-resource pool resource)
  #-:ccl
  (let ((des (pool-destructor pool)))
    (when des
      (funcall des resource))
    (nox-atomic-push resource (pool-data pool))))

(defmacro define-resource-pool (name constructor &optional initializer destructor)
  #+:ccl
  (let ((r (gentemp))
        (c (gentemp))
        (i (gentemp))
        (d (gentemp)))
    `(let ((,c ,constructor)
           (,i ,initializer)
           (,d ,destructor))
       (ccl::defresource ,name
         :constructor (let (,r)
                        (prog1 (setf ,r (funcall ,c))
                          (funcall ,i ,r)))
         :initializer ,i
         :destructor ,d)))
  #-:ccl
  `(defparameter ,name (make-resource-pool :constructor ,constructor
                                           :initializer ,initializer
                                           :destructor ,destructor)))

(defmacro with-resource-from-pool ((var pool) &body body)
  #+:ccl
  `(ccl::using-resource  (,var ,pool) ,@body)
  #-:ccl
  (let ((flag (gentemp))
        (g-pool (gentemp)))
    `(let* ((,g-pool ,pool)
            (,flag (allocate-resource-from-pool ,g-pool))
            (,var ,flag))
       (unwind-protect (progn ,@body)
         (when ,flag (free-resource-to-pool ,g-pool ,flag))))))


;;; ----------------------------------------------------------------------------
;;;
;;;   HELPERS
;;;

(declaim (inline make-base-string))
   
(defun make-base-string (size &rest args)
  (declare (dynamic-extent args))
  (remf args :element-type)
  (apply #'make-array size :element-type #-:sbcl 'base-char #+:sbcl 'character args))

(defvar *current-parser* nil)

(defmacro with-loop&read-char ((char stream) &body body)
  `(loop (let ((,char (read-char ,stream t nil t))) ,@body)))

(defun read-using (readtable stream &optional recursivep)
  (let ((*readtable* readtable))
     (read stream t nil recursivep)))

(defmacro define-readtable (readtablevar copied-from &body body)
  `(defparameter ,readtablevar
     (let ((*readtable* (copy-readtable ,copied-from)))
       ,@body
       *readtable*)))

(defmacro define-macro-character (character (&rest args) &body body)
  `(set-macro-character ',character #'(lambda (,@args) ,@body)))

(defvar *ruc-buffer* nil)

(defvar *ruc-ee-buffer* nil)

(define-resource-pool *xml-parse-buffers* 
    #'(lambda () (make-base-string 2048 :adjustable t :fill-pointer 0))
    #'(lambda (vector) (setf (fill-pointer vector) 0)))

(defun read-until-char-expanding-entities (stream char save-last-p
                                           &optional (buffer *ruc-ee-buffer*))
  (setf (fill-pointer buffer) 0)
  (with-loop&read-char (c stream)
    (cond ((char= c char)
           (when save-last-p
             (unread-char c stream))
           (return (concatenate 'string buffer)))
          ((char= c #\&)
           (let ((name (read-until-char stream #\;)))
             (if (char= (char name 0) #\#)
               (multiple-value-bind (c1 c2 c3)
                                    (resolve-character-reference name)
                 (vector-push-extend c1 buffer)
                 (when c2
                   (vector-push-extend c2 buffer)
                   (when c3
                     (vector-push-extend c3 buffer))))
               (let ((def (get-entity *current-parser* name)))
                 (unless def
                   (cerror "Do not expand"
                           'missing-entity-definition :thing name)
                   (setf def name))
                 (dotimes (i (length def))
                   (vector-push-extend (char def i) buffer))))))
          (t
           (vector-push-extend c buffer)))))

(defun resolve-character-reference (ref)
  (let ((n (ignore-errors (if (char= (char ref 1) #\x)
                            (parse-integer ref :start 2 :radix 16)
                            (parse-integer ref :start 1 :radix 10)))))
    (if (and (integerp n) (< n 65536))
      (let ((b (integer-length n))) ; poor man's UTF8 enconding :-)
        (cond ((<= b 7)
               (code-char n))
              ((<= b 11)
               (values (code-char (logior #b11000000 (ash n -6)))
                       (code-char (logior #b10000000 (logand n #b00111111)))))
              ((<= b 16)
               (values (code-char (logior #b11100000 (ash n -12)))
                       (code-char (logior #b10000000 (logand (ash n -6)) #b00111111))
                       (code-char (logior #b10000000 (logand n #b00111111)))))
              (t #\Space)))  ; Changed, rm: Make sure there is a character returned.
      (error 'unknown-character-reference :thing ref))))

(defun read-until-char (stream char &optional (buffer *ruc-buffer*))
  (setf (fill-pointer buffer) 0)
  (with-loop&read-char (c stream)
    (if (char= c char)
      (return (concatenate 'string buffer))
      (vector-push-extend c buffer))))

(defun read-until-%%> (stream char &aux chars)
  (with-loop&read-char (c stream)
    (cond ((not (char= c char))
           (push c chars))
          ((or (not (char= (setf c (read-char stream t nil t)) char))
               (not (char= (peek-char nil stream t nil t) #\>)))
           (push char chars)
           (push c chars))
          (t
           (read-char stream t nil t) ; skip #\>
           (return (concatenate 'string (nreverse chars)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun whitespace-char-p (char)
    ;; let's assume this works for now :-)
    (or (char= char #\Space)
        (not (graphic-char-p char)))))

(eval-when (:compile-toplevel :load-toplevel)
  (define-constant -whitespace-chars-
    (let ((chars nil))
      (dotimes (i 256)
        (let ((c (code-char i)))
          (when ;; (whitespace-char-p c) changed, rm: consider only standard chars
            (and (standard-char-p c) (whitespace-char-p c))
            (push c chars))))
      (concatenate 'string chars))))

(defun name&prefix (string)
  (let ((i (position #\: string)))
    (cond ((null i)
           (values string nil))
          ((zerop i)
           (values (subseq string 1) nil))
          ((= i (1- (length string)))
           (values nil (subseq string 0 i)))
          (t
           (values (subseq string (1+ i)) (subseq string 0 i))))))

(defun skip-whitespace (stream &optional pop-char-p)
  (let ((char (peek-char t stream t nil t)))
    (if pop-char-p
      (read-char stream t nil t)
      char)))

(defun collapse-whitespace (string)
  (let* ((mode :start)
         (n (length string))
         (chars (make-array n :element-type 'character :fill-pointer 0)))
    (dotimes (i n)
      (let ((c (char string i)))
        (case mode
          (:collect
           (if (whitespace-char-p c)
             (setf mode :white)
             (vector-push c chars)))
          (:white
           (unless (whitespace-char-p c)
             (vector-push #\Space chars)
             (vector-push c chars)
             (setf mode :collect)))
          (:start
           (unless (whitespace-char-p c)
             (vector-push c chars)
             (setf mode :collect))))))
    (concatenate 'string chars)))


;;; ----------------------------------------------------------------------------
;;;
;;;   STRING DICTIONARY
;;;
;;;   Some care must be taken when using this, since (in the interest making
;;;   the implementation not cons so much) we have used destructive operations.
;;;

(defun string-dict-get (keys&values key)
  (cdr (assoc key keys&values :test #'string=)))

(defun string-dict-get-by-value (keys&values value)
  (car (rassoc value keys&values :test #'string=)))

(defun string-dict-add (keys&values key value)
  (acons key value keys&values))

(defun string-dict-del (keys&values key)
  (delete key keys&values :key #'car :test #'string=))

(defmacro do-string-dict ((key value dict) &body body)
  `(loop for (,key . ,value) in ,dict do (progn ,@body)))


;;; ----------------------------------------------------------------------------
;;;
;;;   URL FUNCTIONS
;;;
;;;   We currently support http, file, mailto and tel URL parsing.
;;;

(defun parse-url (u &optional (errorp nil))
  (cond ((string= u "http://" :end1 7)
         (flet ((url (host port &optional (path "/"))
                  `(:host ,host
                    :port ,port
                    :path ,(if (zerop (length path)) "/" path))))
           (let ((i (position #\: u :start 7))
                 (j (position #\/ u :start 7)))
             (cond ((and (null i) (null j))
                    (values :http (url (subseq u 7) 80 "/")))
                   ((and i (< i (or j most-positive-fixnum)))
                    (let ((h (subseq u 7 i)))
                        (multiple-value-bind (p j)
                                             (parse-integer u
                                                            :start (1+ i)
                                                            :junk-allowed t)
                          (values :http (url h p (subseq u j))))))
                   (t
                    (values :http (url (subseq u 7 j) 80 (subseq u j))))))))
        ((string= u "file://" :end1 7)
         (let ((p (subseq u 7)))
           (values :file
                   `(:path ,(translate-logical-pathname
                             (canonical->host-specific-path p))))))
        ((string= u "mailto:" :end1 7)
         (values :mailto
                 (subseq u 7)))
        ((string= u "tel:" :end1 4)
         (if (char= (char u 4) #\+)
           (values :tel
                   (subseq u 5)
                   t)
           (values :tel
                   (subseq u 4)
                   nil)))
        (errorp
         (error 'malformed-url :thing u))
        (t
         (let ((i (position #\: u)))
           (values :unknown
                   (subseq u 0 i)
                   (subseq u (1+ i)))))))

(defun host-specific->canonical-path (path)
  #+(and :ccl (not :openmcl))
  (substitute #\/ #\: (subseq path (position #\: path)))
  #+(or (not :ccl) :openmcl) ; = unix, since we do not do Windows yet (maybe never)
  path)

(defun canonical->host-specific-path (path)
  #+(and :ccl (not :openmcl))
  (let ((p (namestring (translate-logical-pathname "home:"))))
    (concatenate 'string (subseq p 0 (position #\: p)) (substitute #\: #\/ path)))
  #+(or (not :ccl) :openmcl) ; = unix, since we do not do Windows yet (maybe never)
  path)

(defun make-file-url (pathname)
  (format nil "file://~A"
          (host-specific->canonical-path
           (namestring (translate-logical-pathname (pathname pathname))))))

(defun make-http-url (host port path)
  (format nil "http://~A~@[:~S~]~A" host port (or path "/")))

(defun make-mailto-url (address)
  (format nil "mailto:~A" address))

(defun make-tel-url (number &optional (include-plus-p t))
  (format nil "tel:~[+~]~A" (not include-plus-p) number))


;;; ----------------------------------------------------------------------------
;;;
;;;   XML TOKEN CLASSES
;;;
;;;   The lexical scanner of the parser (function read-xml-token) returns
;;;   instances of XML token classes. Some of these are processed by the
;;;   parser, some are ignored.
;;;
;;;   TOKEN                       abstract
;;;     TAG                       abstract
;;;       OPEN-TAG                processed
;;;       CLOSE-TAG               processed
;;;     PROC-INSTRUCTION          ignored
;;;     DTD-DECLARATION           abstract
;;;       ENTITY-DECLARATION      processed
;;;       ELEMENT-DECLARATION     ignored w/ warning
;;;       ATTLIST-DECLARATION     ignored w/ warning
;;;       COMMENT                 ignored
;;;     CHAR-CONTENT              processed (this is a string, not an instance)
;;;     DTD-BRACKET               abstract
;;;       DTD-START               ignored w/ warning (if external DTD)
;;;       DTD-END                 ignored
;;;

(defclass token ()
  ((string
    :initarg :string
    :accessor token-string)))

(defclass tag (token)
  ((counterpart
    :initform nil
    :accessor tag-counterpart)))

(defmethod print-object ((self tag) stream)
  (print-unreadable-object (self stream :type t)
    (princ (token-string self) stream)))

(defclass open-tag (tag)
  ((original-name
    :initform nil
    :accessor tag-original-name)
   (attributes
    :initform nil
    :accessor tag-attributes)
   (emptyp
    :initform nil
    :accessor tag-empty-p)
   (namespaces
    :initarg :namespaces
    :accessor tag-namespaces)
   (base
    :initarg :base
    :initform nil
    :accessor tag-base)))

(defun tag-attribute (tag attribute) ; assuming OPEN-TAG
  (string-dict-get (tag-attributes tag) attribute))

(defun (setf tag-attribute) (value tag attribute) ; assuming OPEN-TAG
  (setf (tag-attributes tag) (string-dict-add (tag-attributes tag) attribute value))
  value)

(defclass close-tag (tag)
  ())

(defclass proc-instruction (token)
  ())

(defclass dtd-declaration (token)
  ())

(defclass entity-declaration (dtd-declaration)
  ((name
    :initarg :name
    :reader entity-name)))

(defclass element-declaration (dtd-declaration)
  ((name
    :initarg :name
    :reader element-name)
   (contentspec
    :initarg :contentspec
    :reader element-contentspec)))

(defclass attlist-declaration (dtd-declaration)
  ((name
    :initarg :name
    :reader attlist-name)
   ;; should be more
   ))

(defclass comment (dtd-declaration)
  ())

(defclass dtd-bracket (token)
  ())

(defclass dtd-start (dtd-bracket)
  ((externalp
    :initarg :externalp
    :initform nil
    :reader dtd-external-p)
   (stuff
    :initarg :stuff
    :initform nil
    :reader dtd-stuff)))

(defclass dtd-end (dtd-bracket)
  ())


;;; ----------------------------------------------------------------------------
;;;
;;;   CLASS SAX-CONSUMER
;;;   SIMPLE SAX 1 -LIKE INTERFACE ("CL-SAX")
;;;

(defclass sax-consumer ()
  ((producer
    :initarg :producer
    :initform nil
    :accessor sax-consumer-producer)
   (debugp
    :initarg :debugp
    :initform nil
    :reader sax-consumer-debug-p)))

(defgeneric start-element (consumer tag mode))

(defgeneric end-element (consumer tag mode))

(defgeneric char-content (consumer char-content mode))

(defgeneric proc-instruction (consumer instruction mode))

(defgeneric start-document (consumer locator))

(defgeneric end-document (consumer mode))

(defgeneric maybe-use-namespace (consumer prefix uri))

(defun debug-format (consumer string &rest args)
  (when (sax-consumer-debug-p consumer)
     (apply #'format *debug-io* string args)))

(defmethod find-first-producer ((consumer sax-consumer))
  (find-first-producer (sax-consumer-producer consumer)))

(defmethod sax-consumer-mode ((self sax-consumer))
  nil)

(defmethod start-element ((self sax-consumer) (tag open-tag) mode)
  (debug-format self "~&START ~A ~S ~S ~S"
                (token-string tag) (tag-attributes tag) mode (tag-base tag)))

(defmethod end-element ((self sax-consumer) (tag open-tag) mode)
  (debug-format self "~&END ~A ~S ~S" (token-string tag) mode (tag-base tag)))

(defmethod char-content ((self sax-consumer) (char-content string) mode)
  (debug-format self "~&CHARACTERS ~S ~S" char-content mode))

(defmethod proc-instruction ((self sax-consumer) (tag proc-instruction) mode)
  (debug-format self "~&PI ~S ~S" (token-string tag) mode))

(defmethod start-document ((self sax-consumer) locator)
  (debug-format self "~&START DOCUMENT ~S" locator))

(defmethod end-document ((self sax-consumer) mode)
  (debug-format self "~&END DOCUMENT ~S" mode))

(defmethod maybe-use-namespace ((self sax-consumer) prefix uri)
  (debug-format self "&NAMESPACE ~S ~S" prefix uri))


;;; ----------------------------------------------------------------------------
;;;
;;;   CLASS SAX-PRODUCER
;;;

(defclass sax-producer ()
  ((consumer
    :accessor sax-producer-consumer)))

(defmethod find-first-producer ((producer sax-producer))
  producer)

(defmethod initialize-instance :after ((self sax-producer)
                                       &key (consumer
                                             (make-instance 'sax-consumer
                                               :debugp t))
                                       &allow-other-keys)
  (setf (sax-producer-consumer self) consumer))

(defmethod (setf sax-producer-consumer) :after ((consumer sax-consumer)
                                                (producer sax-producer))
  (setf (sax-consumer-producer consumer) producer))


;;; ----------------------------------------------------------------------------
;;;
;;;   CLASS SAX-FILTER
;;;

(defclass sax-filter (sax-consumer sax-producer)
  ((blockp
    :initform nil
    :initarg :blockp
    :accessor sax-filter-block-p)))

(defmethod start-element ((self sax-filter) (tag open-tag) mode)
  (unless (sax-filter-block-p self)
    (start-element (sax-producer-consumer self) tag mode)))

(defmethod end-element ((self sax-filter) (tag open-tag) mode)
  (unless (sax-filter-block-p self)
    (end-element (sax-producer-consumer self) tag mode)))

(defmethod char-content ((self sax-filter) (content string) mode)
  (unless (sax-filter-block-p self)
    (char-content (sax-producer-consumer self) content mode)))

(defmethod proc-instruction ((self sax-filter) (tag proc-instruction) mode)
  (unless (sax-filter-block-p self)
    (proc-instruction (sax-producer-consumer self) tag mode)))

(defmethod start-document ((self sax-filter) locator)
  (unless (sax-filter-block-p self)
    (start-document (sax-producer-consumer self) locator)))

(defmethod end-document ((self sax-filter) mode)
  (unless (sax-filter-block-p self)
    (end-document (sax-producer-consumer self) mode)))


;;; ----------------------------------------------------------------------------
;;;
;;;   CLASS NODE-POSITION-TRACKER
;;;

(defclass node-position-tracker (sax-filter)
  ((path-taken
    :initform nil
    :accessor tracker-path-taken)
   (horizontal-index
    :initform 0
    :accessor tracker-horizontal-index)))

(defmethod start-element :around ((self node-position-tracker) (tag open-tag) mode)
  (declare (ignore mode))
  (push (1+ (tracker-horizontal-index self)) (tracker-path-taken self))
  (setf (tracker-horizontal-index self) 0)
  (call-next-method))

(defmethod end-element :around ((self node-position-tracker) (tag open-tag) mode)
  (declare (ignore mode))
  (call-next-method)
  (setf (tracker-horizontal-index self) (pop (tracker-path-taken self))))
