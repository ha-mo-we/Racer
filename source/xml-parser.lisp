;;; -*- package: NOX; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  xml-parser.lisp
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
;;;                   Louis Theran (mailto:theran@pobox.com)
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;
;;;   Version: $Id: xml-parser.lisp,v 1.11 2004/11/28 23:14:53 ora Exp $
;;;
;;;   Purpose: This file contains an implementation of an XML parser. This
;;;   parser was motivated by RDF, and consequently does not implement all the
;;;   features of XML 1.0. In fact, it needs a lot of work. Tough...
;;;


(in-package :nox)

;;; --------------------------------------------------------------------------------------
;;;
;;;   NAME READTABLE
;;;

(eval-when (:compile-toplevel :load-toplevel)
  (define-constant -name-start-characters-
    (let ((s (concatenate 'string
                          (loop for i from (char-code #\a) to (char-code #\z)
                                collect (code-char i)))))
      (concatenate 'string s (string-upcase s) "_:0123456789.,")))
  (define-constant -name-characters-
    (let ((v (make-array 256)))
      (dotimes (i 256)
        (setf (svref v i) nil))
      (dolist (c (concatenate 'list -name-start-characters-
			      (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\-)))
        (setf (svref v (char-code c)) t))
      v)))

(defvar *nr-buffer* (make-base-string 256 :adjustable t :fill-pointer 0))

(defun name-reader (stream char)
  (setf (fill-pointer *nr-buffer*) 0)
  (vector-push char *nr-buffer*)
  (with-loop&read-char (c stream)
    (cond ((svref -name-characters- (char-code c))
           (vector-push-extend c *nr-buffer*))
          (t
           (unread-char c stream)
           (return (concatenate 'string *nr-buffer*))))))

(defun single-character-reader (stream char)
  (declare (ignore stream))
  char)

(defun not-allowed-reader (stream char)
  (declare (ignore stream))
  (error 'syntax-error :thing char))

(define-readtable *name-reader* nil
  (dotimes (i 256)
    (let ((c (code-char i)))
      (unless (whitespace-char-p c)
        (set-macro-character c #'not-allowed-reader))))
  (set-macro-character #\/ #'single-character-reader)
  (set-macro-character #\! #'name-reader)
  (set-macro-character #\? #'name-reader)
  (map nil #'(lambda (c)
               (set-macro-character c #'name-reader))
       -name-start-characters-))


;;; --------------------------------------------------------------------------------------
;;;
;;;   XML READTABLE
;;;

(defun read-declaration (stream name)
  (declare (special *dtd-reader*))      ; forward ref
  (cond ((string= name "!DOCTYPE")
         (let ((name (read-using *name-reader* stream t))
               (next (skip-whitespace stream)))
           (cond ((not (eql next #\[))
                  (make-instance 'dtd-start
                    :string name :externalp t
                    :stuff (read-delimited-list #\> stream t)))
                 (t
                  (setf (parser-in-dtd-p *current-parser*) t
                        (parser-readtable *current-parser*) *dtd-reader*)
                  (skip-whitespace stream t) ; skip [
                  (make-instance 'dtd-start :string name)))))
        ((string= name "!")
         (let ((char (read-char stream t nil t)))
           (cond ((char= char #\[) ; CDATA, INCLUDE, IGNORE
                  (let ((name (read-until-char stream #\[)))
                    (cond ((string= name "CDATA")
                           (read-until-%%> stream #\]))
                          ((find name '("INCLUDE" "IGNORE") :test #'string=)
                           (error 'feature-not-supported :thing name))
                          (t
                           (error 'syntax-error :thing "!["))))))))
        (;;(string= name "!--")
         ;; changed, rm: there need not be a blank after the start of a comment
         (let ((comment (search "!--" name :end2 3)))
           (and comment (zerop comment)))
         (let ((comment-end (search "--" name :from-end t)))
           (if (and comment-end 
                    (> comment-end 3)
                    (= (- (length name) comment-end) 2))
             (progn
               (read-char stream)
               (make-instance 'comment :string (subseq name 3 comment-end)))
             (make-instance 'comment :string (read-until-%%> stream #\-)))))
        (t
         (error 'unknown-declaration :thing name))))

(defun open-anglebracket-reader (stream char)
  (declare (ignore char))
  (let ((name (read-using *name-reader* stream t)))
    (cond ((eql name #\/)
           (make-instance 'close-tag
             :string (first (read-delimited-list #\> stream t))))
          ((char= (char name 0) #\!)
           (read-declaration stream name))
          ((char= (char name 0) #\?)
           (let* ((stuff (read-delimited-list #\> stream t)))
             (if (eql (first (last stuff)) #\?)
               (make-instance 'proc-instruction :string name) ; ignore attrs
               (error 'pi-termination-problem :thing name))))
          (t
           (let* ((stuff (read-delimited-list #\> stream t))
                  (parent (first (parser-path *current-parser*)))
                  (tag (make-instance 'open-tag
                         :string name
                         :base (if parent
                                 (tag-base parent)
                                 (parser-base *current-parser*))))
                  (attr nil))
             (loop (cond ((null stuff)
                          (return tag))
                         ((eql (setf attr (pop stuff)) #\/)
                          (setf (tag-empty-p tag) t)
                          (return tag))
                         ((eql (pop stuff) #\=)
                          (setf (tag-attribute tag attr) (pop stuff)))
                         (t
                          (error 'syntax-error :thing "missing =")))))))))

(defun quoted-string-reader (stream char)
  (read-until-char-expanding-entities stream char nil))

(defun read-xml-token (stream &aux (char (peek-char t stream nil nil)))
  (when char
    (if (or (char= char #\<)
            (and (char= char #\])
                 (parser-in-dtd-p *current-parser*)))
      (read-using (parser-readtable *current-parser*) stream)
      (read-until-char-expanding-entities stream #\< t))))

#|

(defun read-xml-token (stream &aux 
                              (char (peek-char t stream nil nil)))  
  (when char
    (let ((chars nil)
          (done nil))
      
      (loop
       (let ((char0 (read-char stream nil nil)))
         (push char0 chars)
         (when done 
           (return))
         (when (char= char char0)
           (setf done t))))


      (let* ((next-char 
              (first chars))
             (closing-tag-p 
              (and (char= char #\<)
                   next-char
                   (char= next-char #\/)))
             (opening-tag-p 
              (and (char= char #\<)
                   (not closing-tag-p))))
   
        (pprint (list char next-char chars))

        (if (or opening-tag-p
                (and (char= char #\])
                     (parser-in-dtd-p *current-parser*)))
            (progn 
              (dolist (char chars)
                (unread-char char stream))
              
              (read-using (parser-readtable *current-parser*) stream))

          (progn 
            (if closing-tag-p
                (progn 
                  (read-using (parser-readtable *current-parser*) stream)
                  "empty"
                  )

              (progn 
                (dolist (char chars)
                  (unread-char char stream))

                (read-until-char-expanding-entities stream #\< t)))))))))

|#
          

(define-readtable *xml-reader* nil
  (dotimes (i 256)
    (let ((c (code-char i)))
      (unless (whitespace-char-p c)
        (set-macro-character c #'not-allowed-reader))))
  (set-macro-character #\< #'open-anglebracket-reader)
  (set-macro-character #\> (get-macro-character #\)))
  (set-macro-character #\= #'single-character-reader)
  (set-macro-character #\/ #'single-character-reader)
  (set-macro-character #\? #'single-character-reader)
  (set-macro-character #\' #'quoted-string-reader)
  (set-macro-character #\" #'quoted-string-reader)
  (map nil #'(lambda (c)
               (set-macro-character c #'name-reader))
       -name-start-characters-))


;;; --------------------------------------------------------------------------------------
;;;
;;;   DTD READTABLE
;;;

(defun dtd-open-anglebracket-reader (stream char)
  (declare (ignore char))
  (let ((name (read-using *name-reader* stream t))
        (stuff (read-delimited-list #\> stream t)))
    (cond ((string= name "!ENTITY")
           (make-instance 'entity-declaration
             :name (pop stuff) :string (pop stuff)))
          ((string= name "!ELEMENT")
           (make-instance 'element-declaration
             :name (pop stuff) :contentspec (pop stuff)))
          ((string= name "!ATTLIST")
           (make-instance 'attlist-declaration
             :name (pop stuff)))
          ;; new, rm: there may be comments in DTDs
          ((let ((pos (search "!--" name)))
             (and pos (zerop pos)))
           (read-xml-token stream))
          ((string= name "!NOTATION")
           (error 'feature-not-supported :thing name))
          (t
           (error 'unknown-declaration :thing name)))))

(defun dtd-parenthesis-reader (stream char)
  (declare (ignore char))
  (read-delimited-list #\) stream t))

(defun close-bracket-reader (stream char)
  (declare (ignore char))
  (cond ((not (parser-in-dtd-p *current-parser*))
         (error 'syntax-error :thing "]"))
        ((not (char= (skip-whitespace stream t) #\>))
         (error 'dtd-termination-problem))
        (t
         (setf (parser-readtable *current-parser*) *xml-reader*)
         (make-instance 'dtd-end))))

(define-readtable *dtd-reader* *xml-reader*
  (set-macro-character #\< #'dtd-open-anglebracket-reader)
  (set-macro-character #\# (get-macro-character #\A))
  (set-macro-character #\] #'close-bracket-reader)
  (set-macro-character #\( #'dtd-parenthesis-reader)
  (set-macro-character #\) (get-macro-character #\)))
  ;; new, rm: hyphens may be used in entities
  (set-macro-character #\- #'name-reader))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS XML-PARSER
;;;

(defclass xml-parser (sax-producer)
  ((expand-namespaces-p
    :initarg :expand-namespaces-p
    :initform t
    :reader parser-expand-namespaces-p)
   (entities
    :initform (make-hash-table :test #'equal)
    :reader parser-entities)
   (in-dtd-p
    :initform nil
    :accessor parser-in-dtd-p)
   (canonical-uris
    :initform (make-hash-table :test #'equal)
    :reader parser-canonical-uris)
   (readtable
    :initform nil
    :accessor parser-readtable)
   (path
    :initform nil
    :accessor parser-path)
   (base
    :initform nil
    :accessor parser-base)
   (filepos 
    :initform nil
    :accessor parser-filepos)
   ;;; namespace stack added by MW (namespace handling was incorrect)
   (namespaces-stack 
    :initform nil
    :accessor parser-namespaces-stack)))

(eval-when (:compile-toplevel :load-toplevel)
  (define-constant -standard-entities- '(("gt"   . ">")
                                     ("lt"   . "<")
                                     ("amp"  . "&")
                                     ("quot" . "\"")
                                     ("apos" . "'"))))

(defmethod initialize-instance :after ((self xml-parser) &rest args)
  (declare (ignore args))
  (dolist (pair -standard-entities-)
    (destructuring-bind (n . e) pair (setf (get-entity self n) e)))
  (setf (get-canonical-uri self -alternate-rdf-uri-) -rdf-uri-
        (get-canonical-uri self -alternate-rdfs-uri-) -rdfs-uri-
        (get-canonical-uri self (subseq -rdfs-uri- 0 (1- (length -rdfs-uri-))))
         -rdfs-uri-))

(defun get-entity (parser name)
  (gethash name (parser-entities parser)))

(defun (setf get-entity) (definition parser name)
  (setf (gethash name (parser-entities parser)) definition))

(defun get-canonical-uri (parser uri)
  (gethash uri (parser-canonical-uris parser) uri))

(defun (setf get-canonical-uri) (new-uri parser uri)
  (setf (gethash uri (parser-canonical-uris parser)) new-uri))

(defmethod parse ((self xml-parser) stream locator)
  (declare (special *xml-parse-buffers*))
  (let ((*current-parser* self)
        (consumer (sax-producer-consumer self)))

    (with-resource-from-pool (*ruc-buffer* *xml-parse-buffers*)
      (with-resource-from-pool (*ruc-ee-buffer* *xml-parse-buffers*)
        (setf (parser-readtable self) *xml-reader*
              (parser-base self) locator)
        (handler-bind ((end-of-file #'(lambda (c)
                                        (declare (ignore c))
                                        (error 'syntax-error :thing "eof"))))
          (start-document consumer locator)
          (parse-start self stream nil nil)
          (end-document consumer (sax-consumer-mode consumer)))))))

#|
(defun parse-start (parser stream end namespaces &aux continuep)
  (loop (multiple-value-setq (continuep namespaces)
	  (parse-token parser stream (read-xml-token stream) end namespaces))
	(unless continuep
	  (return-from parse-start nil))))
|#

;;; changed, rm: read-xml-token might return nil. If this is the case, return.
(defun parse-start (parser stream end namespaces &aux continuep)
  (let ((xml-token))
    (loop
      (setf xml-token (read-xml-token stream))
      (when (null xml-token)
        (return-from parse-start nil))
      (multiple-value-setq (continuep namespaces)
        (parse-token parser stream xml-token end namespaces))
      (unless continuep
        (return-from parse-start nil)))))

(defmethod parse-token ((self xml-parser)
                        stream (token string) ; char-content
                        end namespaces)
  (declare (ignore stream))
  (char-content (sax-producer-consumer self) 
                ;; (collapse-whitespace token) changed, rm: do not collapse whitespace in strings
                token
                (sax-consumer-mode (sax-producer-consumer self)))
  (values end namespaces))

(defmethod parse-token ((self xml-parser)
                        stream (token open-tag) end namespaces)
  (flet ((expand (name)
           (or (expand-name-with-namespace name namespaces)
               (progn (cerror "Do not expand"
                              'missing-namespace-definition :thing name)
                 name)))
         (expand-attribute (name token)  ; new rm 16.1.09
           (or (expand-attribute-name-with-namespace name namespaces (token-string token))
               (progn (cerror "Do not expand"
                              'missing-namespace-definition :thing name)
                 name))))
    (declare (dynamic-extent #'expand))
    (let ((consumer (sax-producer-consumer self)))

      (when (parser-expand-namespaces-p self)
        ;;; namespace stack added by MW (namespace handling was incorrect)
        (push (append 
               (add-namespaces self token nil)
               (first (parser-namespaces-stack self)))
              (parser-namespaces-stack self))
        (setf namespaces (first (parser-namespaces-stack self)))
        (shiftf (tag-original-name token)
                (token-string token)
                (expand (token-string token)))
        (dolist (k&v (tag-attributes token))
          (setf (car k&v) (expand-attribute (car k&v) token))))

      (do-string-dict (key value (tag-attributes token))
        (when (string= key "xml:base")
          (setf (tag-attributes token)
                (string-dict-del (tag-attributes token) key))
          (setf (tag-base token) value)))
      (setf (tag-namespaces token) namespaces)
      (push token (parser-path self))
      (let ((base (parser-base self))) 
        (let ((new-base (cdr (assoc nil namespaces))))
          (when new-base 
            (let ((pos-sharp (position #\# new-base :from-end t)))
              (when (and pos-sharp (= pos-sharp (1- (length new-base))))
                (setf new-base (subseq new-base 0 (1- (length new-base)))))
              (setf (parser-base self) new-base))))
        (start-element consumer token (sax-consumer-mode consumer))
        (cond ((tag-empty-p token)
               (end-element consumer token (sax-consumer-mode consumer))
               (pop (parser-path self))
               ;;; namespace stack added by MW (namespace handling was incorrect)
               (setf namespaces (pop (parser-namespaces-stack self))))
              (t
               (parse-start self stream token namespaces)))
        (setf (parser-base self) base)
        (values end namespaces)))))

(defun add-namespaces (parser tag namespaces)
  (do-string-dict (key value (tag-attributes tag))
    (multiple-value-bind (n p) (name&prefix key)
      (cond ((string= p "xmlns")
             (let ((uri (get-canonical-uri parser value)))
               (setf (tag-attributes tag)
                     (string-dict-del (tag-attributes tag) key))
               (setf namespaces
                     (string-dict-add namespaces n uri))
               (maybe-use-namespace (sax-producer-consumer parser) n uri)))
            ((and (null p) (string= n "xmlns"))
             (setf (tag-attributes tag)
                   (string-dict-del (tag-attributes tag) key))
             (setf namespaces
                   (string-dict-add namespaces
                                    nil (get-canonical-uri parser value)))))))
  namespaces)

(defun ends-in-hash-p (string)
  (declare (type string string))
  (let ((c (char string (1- (length string)))))
    (or (char= c #\#)
        (char= c #\/))))

(defun expand-name-with-namespace (string namespaces)
  (multiple-value-bind (n p) (name&prefix string)
    (or (and (null p)
             (hack-rdf-attribute-name n namespaces))
        (let ((uri (string-dict-get namespaces p)))
          (cond (uri
                 (values
                  (concatenate 'string
                               uri (and (not (ends-in-hash-p uri)) "#") n)
                  n p))
                ((or (null p) (string-equal p "xml"))
                 (values string nil nil))
                (t
                 (values nil n p)))))))

;; New function: rm, 16.1.09
;; Default namespace declarations do not apply directly to attribute names; 
;; the interpretation of unprefixed attributes is determined by the element on which they appear.
(defun expand-attribute-name-with-namespace (string namespaces context-string)
  (multiple-value-bind (n p) (name&prefix string) 
    (or (and (null p)
             (hack-rdf-attribute-name n namespaces))
        (and (null p)
             (let* ((context-prefix-sharp-pos (position #\# context-string))
                    (context-prefix (and context-prefix-sharp-pos
                                         (subseq context-string 0 (1+ context-prefix-sharp-pos)))))
               (if context-prefix
                   (concatenate 'string context-prefix string)
                 nil)))
        (let ((uri (string-dict-get namespaces p)))
          (cond (uri
                 (values
                  (concatenate 'string
                               uri (and (not (ends-in-hash-p uri)) "#") n)
                  n p))
                ((or (null p) (string-equal p "xml"))
                 (values string nil nil))
                (t
                 (values nil n p)))))))

(defun hack-rdf-attribute-name (name namespaces)
  (and (car (rassoc -rdf-uri- namespaces :test #'string=))
       (cdr (assoc name -rdf-attr-map- :test #'string=))))

(defmethod parse-token ((self xml-parser)
                        stream (token close-tag) end namespaces)
  (declare (ignore stream))
  (cond ((null end)
         (error 'unexpected-end-tag :thing (token-string end)))
        ((string= (tag-original-name end) (token-string token))
         (setf (tag-counterpart token) end
               (tag-counterpart end) token)
         (end-element (sax-producer-consumer self) end
                      (sax-consumer-mode (sax-producer-consumer self)))
         (pop (parser-path self))
         ;;; namespace stack added by MW (namespace handling was incorrect)
         (setf namespaces (pop (parser-namespaces-stack self)))
         (values nil namespaces))
        (t
         (error 'unexpected-end-tag
                :expectation (tag-original-name end)
                :thing (token-string token)))))

(defmethod parse-token ((self xml-parser)
                        stream (token proc-instruction) end namespaces)
  (declare (ignore stream end))
  (let ((consumer (sax-producer-consumer self)))
    (proc-instruction consumer token (sax-consumer-mode consumer))
    (values t namespaces)))

(defmethod parse-token ((self xml-parser)
                        stream (token entity-declaration) end namespaces)
  (declare (ignore stream end))
  (setf (get-entity self (entity-name token)) (token-string token))
  (values t namespaces))

(defmethod parse-token ((self xml-parser)
                        stream (token comment) end namespaces)
  (declare (ignore stream end))
  (values t namespaces))

#|
(defmethod parse-token ((self xml-parser)
                        stream (token dtd-start) end namespaces)
  (declare (ignore stream end))
  (when (dtd-external-p token)
    (xml-warning "External DTD ignored:~{ ~S~}" (dtd-stuff token)))
  (values t namespaces))
|#

;;; changed, rm
(defmethod parse-token ((self xml-parser)
                          stream (token dtd-start) end namespaces)
    (declare (ignore end))
    (when (dtd-external-p token) ;(break)
      (read-external-dtd ;;(sax-producer-consumer (sax-producer-consumer self))
                         (sax-producer-consumer self)
                         stream
                         (dtd-stuff token)
                         namespaces))
    (values t namespaces))

;;; new, rm
(defmethod read-external-dtd ((consumer sax-consumer) dtd-stream dtd-stuff namespaces)
  (if (string= (first dtd-stuff) "SYSTEM")
    (let* ((parser (sax-consumer-producer (sax-consumer-producer consumer)))
           (readtable (parser-readtable parser)))
      (setf (parser-in-dtd-p parser) t
            (parser-readtable parser) *dtd-reader*)
      (parse-start parser dtd-stream nil namespaces)
      (setf (parser-readtable parser) readtable))
    ;;(xml-warning "External DTD ignored:~{ ~S~}" dtd-stuff)
    ))

(defmethod parse-token ((self xml-parser)
                        stream (token dtd-end) end namespaces)
  (declare (ignore stream end))
  (values t namespaces))

(defmethod parse-token ((self xml-parser)
                        stream (token dtd-declaration) end namespaces)
  (declare (ignore stream end))
  ;;(xml-warning "~S ignored" (class-name (class-of token)))
  (values t namespaces))

(defmethod parse-token ((self xml-parser) stream token end namespaces)
  (declare (ignore stream end))
  (if token
    (error 'syntax-error :thing token)
    (values nil namespaces))) ; null token signifies eof

#|
(defun parse-from-stream (stream locator parser-class &rest options)
  (declare (dynamic-extent options))
  (let ((parser (apply #'make-instance parser-class options)))
    (handler-case (values (parse parser stream locator) parser)
      (xml-error (e)
	(let ((*readtable* (copy-readtable nil)))
	  (cerror "Keep going" e))))))
|#

(defun parse-from-stream (stream locator parser-class &rest options)
  (declare (dynamic-extent options))
  (let ((parser (apply #'make-instance parser-class options)))

    #+:racer-server
    (racer:set-progress-100% #+:ccl (ccl::stream-length stream)
                             #-:ccl (file-length stream))
    
    (values (parse parser stream locator) parser)))

(defun parse-from-file (file parser-class &rest options)
  (declare (dynamic-extent options))
  (let ((pathname (translate-logical-pathname file)))
    (with-open-file (stream pathname)
      (apply #'parse-from-stream stream (make-file-url pathname) parser-class
             options))))


;;; --------------------------------------------------------------------------------------
;;;
;;;    CLASS XML-FORMATTER
;;;

(defclass xml-formatter (sax-consumer)
  ((stream
    :initarg :stream
    :initform nil
    :reader formatter-stream)
   (level
    :initform 0
    :accessor formatter-level)
   (indent-delta
    :initarg :indent-delta
    :initform 2
    :reader formatter-indent-delta)))

(defmethod replay ((formatter xml-formatter) events)
  (dolist (event events)
    (let ((mode (sax-consumer-mode formatter)))
      (etypecase event
        (open-tag
         (start-element formatter event mode))
        (close-tag
         (end-element formatter (tag-counterpart event) mode))
        (string
         (char-content formatter event mode))))))

(defun reverse-expand-name (name namespaces &aux (nn (length name)))
  (do-string-dict (prefix uri namespaces)
    (let ((un (length uri)))
      (when (and (>= nn un)
                 (string= name uri :end1 un))
        (return-from reverse-expand-name
          (values (if (= nn un)
                    (format nil "~@[~A~]:" prefix)
                    (format nil "~@[~A:~]~A"
                            prefix
                            (let ((n (subseq name un)))
                              (if (char= (char n 0) #\#)
                                (subseq n 1) n))))
                  t)))))
  (values name nil))

(defmethod start-element ((self xml-formatter) (tag open-tag) mode)
  (declare (ignore mode))
  (let ((stream (formatter-stream self)))
    (format stream "~&~V@T<~A"
            (formatter-level self)
            (reverse-expand-name (token-string tag) (tag-namespaces tag)))
    (do-string-dict (attribute value (tag-attributes tag))
      (format stream " ~A=\"~A\""
              (reverse-expand-name attribute (tag-namespaces tag))
              value))
    (princ (if (tag-empty-p tag) "/>" #\>) stream)
    (incf (formatter-level self) (formatter-indent-delta self))))

(defmethod end-element ((self xml-formatter) tag mode)
  (declare (ignore mode))
  (decf (formatter-level self) (formatter-indent-delta self))
  (unless (tag-empty-p tag)
    (format (formatter-stream self) "~&~V@T</~A>"
            (formatter-level self)
            (reverse-expand-name (token-string tag) (tag-namespaces tag)))))

(defmethod char-content ((self xml-formatter) char-content mode)
  (declare (ignore mode))
  (princ (string-trim '(#\Space #\Tab #\Newline) char-content)
         (formatter-stream self)))

(defmethod start-document ((self xml-formatter) locator)
  (declare (ignore locator))
  (format (formatter-stream self) "~&<?xml version=\"1.0\"?>"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS TREE-PARSER
;;;

(defclass tree-parser (sax-consumer)
  ((states
    :initform nil
    :accessor parser-states)
   (package
    :initarg :package
    :initform (find-package :keyword)
    :reader parser-package)))

(defun string->keyword (string &optional (package :keyword))
  (if package (intern (string-upcase string) package) string))

(defmethod initialize-instance :after ((self tree-parser) &rest args
                                       &key producer &allow-other-keys)
  (declare (ignore args))
  (if producer
    (setf (sax-consumer-producer self) producer
          (sax-producer-consumer producer) self)
    (setf (sax-consumer-producer self) (make-instance 'xml-parser :consumer self))))

(defmethod parser-interpret-content ((parser tree-parser) (content string))
  content)

(defmethod start-element ((parser tree-parser) (tag open-tag) mode)
  (declare (ignore mode))
  (push (list (list (string->keyword (token-string tag) (parser-package parser))))
	(parser-states parser)))

(defmethod end-element ((parser tree-parser) (tag open-tag) mode)
  (declare (ignore mode))
  (push (reverse (first (pop (parser-states parser))))
	(car (first (parser-states parser)))))

(defmethod char-content ((parser tree-parser) (content string) mode)
  (declare (ignore mode))
  (push (parser-interpret-content parser content)
        (car (first (parser-states parser)))))

(defmethod parse ((parser tree-parser) stream locator)
  (setf (parser-states parser) (list (list nil)))
  (parse (find-first-producer parser) stream locator)
  (caar (pop (parser-states parser))))

;;;
;;;
;;; 

(defmethod parse-token :before ((self xml-parser)
                                stream token 
                                end namespaces)

  (declare (ignore namespaces token end))

  #+:racer-server
  (racer:set-progress-value 
   (file-position stream))

   (setf (parser-filepos self) 
         (file-position stream)))
