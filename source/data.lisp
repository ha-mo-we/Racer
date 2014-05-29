;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  rdf-data.lisp
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
;;;   Version: $Id: rdf-data.lisp,v 1.13.2.1 2004/11/30 17:11:29 ora Exp $
;;;
;;;   Purpose: This file contains functionality for managing "RDF data", namely
;;;   nodes, triples, etc.
;;;


(in-package :wilbur)


;;; --------------------------------------------------------------------------------------
;;;
;;;   TOP-LEVEL NODE API
;;;

(declaim (special *nodes*)) ; forward reference

(defvar *user-defined-nodes* nil)

(defun node (thing)
  (etypecase thing
    (string
     (or (find-node *nodes* thing)
         (and *user-defined-nodes* (find-node *user-defined-nodes* thing))
         (if *user-defined-nodes*
             (setf (find-node *user-defined-nodes* thing)
                   (dictionary-make-node *user-defined-nodes* thing))
           (setf (find-node *nodes* thing)
                 (dictionary-make-node *nodes* thing)))))
    (null
     (dictionary-make-node *nodes* nil))
    (url
     (node (url-string thing)))
    (node
     thing)))

(defun add-namespace (prefix uri)
  (dictionary-add-namespace *nodes* prefix uri))

(defun del-namespace (prefix)
  (dictionary-remove-namespace *nodes* prefix))

(defun namespaces ()
  (mapcar #'first (dictionary-namespaces *nodes*)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF CONDITION CLASSES
;;;
;;;   RDF-ERROR                      abstract
;;;     FEATURE-NOT-SUPPORTED        concrete, continuable
;;;     ABOUT-AND-ID-BOTH-PRESENT    concrete, continuable
;;;     UNKNOWN-PARSETYPE            concrete, continuable
;;;     ILLEGAL-CHARACTER-CONTENT    concrete, continuable
;;;     CONTAINER-REQUIRED           concrete, continuable
;;;     OUT-OF-SEQUENCE-INDEX        concrete
;;;     DUPLICATE-NAMESPACE-PREFIX   concrete
;;;     QUERY-SYNTAX-ERROR           concrete
;;;     DATATYPE-PARSE-ERROR         concrete, continuable
;;;

(define-condition rdf-error (nox::wilbur-error)
  ())

(define-condition feature-not-supported (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- ~S not supported"))

(define-condition feature-disabled (rdf-error)
  ()
  (:default-initargs 
    :format-control "RDF -- ~S is disabled"))

(define-condition about-and-id-both-present (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- \"about\" and \"ID\" both present"))

(define-condition unknown-parsetype (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- unknown parsetype ~S"))

(define-condition illegal-character-content (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- character content not allowed: ~S"))

(define-condition container-required (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- ~S is not a container"))

(define-condition out-of-sequence-index (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- index URI ~S allocated out of sequence"))

(define-condition duplicate-namespace-prefix (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- duplicate namespace prefix ~S"))

(define-condition cannot-merge (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- cannot merge databases (reason: ~A)"))

(define-condition query-syntax-error (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- no operands for query operator ~A"))

(define-condition cannot-invert-default-value (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- cannot invert a default value expression ~S"))

(define-condition datatype-parse-error (rdf-error)
  ()
  (:default-initargs
    :format-control "RDF -- cannot parse datatype literal ~S"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS NODE
;;;

(defclass node ()
  ((uri
    :initarg :uri
    :initform nil
    :accessor node-uri)
   (name-resolved-p
    :initarg :name-resolved-p
    :initform t
    :accessor node-name-resolved-p)
   (triple :accessor node-triple ; added, rm: support faster OWL parsing for parseType Collection
           :initform nil)
   (attributes :initform nil ; added, rm: support for faster OWL parsing: associate note with 
                             ; outgoing links
               :accessor triple-attributes)))

(defmethod print-object ((node node) stream)
  (declare (special *nodes*))           ; forward ref.
  (let ((uri (node-uri node)))
    (cond ((null uri)
           (print-unreadable-object (node stream :type t :identity t)))
          ((node-name-resolved-p node)
           (multiple-value-bind (name successp)
                                (find-short-name *nodes* uri)
             (format stream "!~:[~S~;~A~]" successp name)))
          (t
           (format stream "!~A" uri)))))

(defmethod node-name ((node node))
  (let ((uri (node-uri node)))
    (when uri
      (if (node-name-resolved-p node)
        (find-short-name *nodes* uri)
        uri))))

(defmethod make-load-form ((node node) &optional env)
  (declare (ignore env))
  (if (node-name-resolved-p node)
    `(node ,(node-uri node))
    `(unresolved-node ,(node-uri node))))

(defvar *index-uris* (make-array 32 :fill-pointer 0 :adjustable t))

(defun index-uri (index)
  (let ((delta (- (length *index-uris*) index)))
    (cond ((>= delta 0)
           (elt *index-uris* (1- index)))
          ((= delta -1)
           (let ((u (rdf-uri (format nil "_~S" index))))
             (vector-push-extend u *index-uris*)
             ;; (db-new-container-membership-property *db* (node u))
             u))
          (t
           (error 'out-of-sequence-index :thing index)))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS DICTIONARY
;;;

(defclass dictionary ()
  ((nodes
    :initform (make-hash-table :test #'equal)
    :initarg :nodes
    :reader dictionary-nodes)
   (namespaces
    :initform nil
    :accessor dictionary-namespaces)
   (unresolved-nodes
    :initform (make-hash-table :test #'equal)
    :initarg :unresolved-nodes
    :accessor dictionary-unresolved-nodes)
   (node-class
    :initarg :node-class
    :initform 'node
    :reader dictionary-node-class)))

(defmethod initialize-instance :after ((self dictionary) &rest args)
  (declare (ignore args))
  (dictionary-add-namespace self "rdf" -rdf-uri-)
  (dictionary-add-namespace self "rdfs" -rdfs-uri-)
  (dictionary-add-namespace self "xsd" -xsd-uri-))

(defmethod dictionary-add-namespace ((dictionary dictionary) prefix uri)
  (setf (dictionary-namespaces dictionary)
        (nox:string-dict-add (dictionary-namespaces dictionary) prefix uri))
  (maphash #'(lambda (name node)
               (let ((uri (find-long-name dictionary name)))
                 (when uri
                   (remhash name (dictionary-unresolved-nodes dictionary))
                   (setf (node-uri node) uri
                         (node-name-resolved-p node) t
                         (find-node dictionary uri) node))))
           (dictionary-unresolved-nodes dictionary))
  prefix)

(defmethod dictionary-remove-namespace ((dictionary dictionary) prefix)
  (setf (dictionary-namespaces dictionary)
        (nox:string-dict-del (dictionary-namespaces dictionary) prefix))
  prefix)

(defmethod dictionary-rename-namespace ((dictionary dictionary)
                                        old-prefix new-prefix)
  (if (nox:string-dict-get (dictionary-namespaces dictionary) new-prefix)
    (error 'duplicate-namespace-prefix :thing new-prefix)
    (let ((uri (nox:string-dict-get (dictionary-namespaces dictionary) old-prefix)))
      (dictionary-remove-namespace dictionary old-prefix)
      (dictionary-add-namespace dictionary new-prefix uri)
      new-prefix)))

(defmethod dictionary-make-node ((dictionary dictionary) uri)
  (make-instance (dictionary-node-class dictionary) :uri uri))

(defmethod find-node ((dictionary dictionary) uri)
  (when uri
    (gethash uri (dictionary-nodes dictionary))))

(defmethod (setf find-node) (node (dictionary dictionary) uri)
  (when uri
    (setf (gethash uri (dictionary-nodes dictionary)) node)))

(defun find-short-name (dictionary uri)
  (nox:reverse-expand-name uri (dictionary-namespaces dictionary)))

(defun find-long-name (dictionary name)
  (nox:expand-name-with-namespace name (dictionary-namespaces dictionary)))

(defun unresolved-node (name)
  (let ((uri (find-long-name *nodes* name)))
    (if uri
      (node uri)
      (let ((unresolved (dictionary-unresolved-nodes *nodes*)))
        (or (gethash name unresolved)
            (setf (gethash name unresolved)
                  (make-instance 'node :uri name :name-resolved-p nil)))))))

(defmethod find-unresolved-nodes ((dictionary dictionary))
  (let ((nodes nil))
    (maphash #'(lambda (uri node)
                 (declare (ignore uri))
                 (push node nodes))
             (dictionary-unresolved-nodes dictionary))
    nodes))

(defmethod dictionary-apropos-list ((dictionary dictionary)
                                    (pattern string))
  (let ((nodes nil))
    (maphash #'(lambda (name node)
		 (when (name-contains-pattern-p name pattern)
		   (push node nodes)))
	     (dictionary-nodes dictionary))
    (sort nodes #'string< :key #'node-uri)))

(defun name-contains-pattern-p (name pattern)
  ;; NOTE: This is a naive string search algorithm; I will switch to, say,
  ;;  Boyer-Moore when I have more time.
  (let ((nn (length name))
        (np (length pattern)))
    (cond ((= nn np)
           (string= name pattern))
          ((> nn np)
           (dotimes (i (- nn np -1))
             (when (string= name pattern :start1 i :end1 (+ i np))
               (return-from name-contains-pattern-p t)))))))

(defvar *nodes* (make-instance 'dictionary))


(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun inline-node-reader (stream char)
    (declare (ignore char))
    (if (char= (peek-char nil stream t nil t) #\")
	(node (read stream t nil t))
      (unresolved-node (nox:read-using nox:*name-reader* stream t))))

  (defun enable-node-shorthand ()
    (setf *readtable* (copy-readtable))
    (set-macro-character #\! #'inline-node-reader))

  (enable-node-shorthand)

  )

;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS TRIPLE
;;;

(defclass triple ()
  ((subject
    :initarg :subject
    :reader triple-subject)
   (predicate
    :initarg :predicate
    :reader triple-predicate)
   (object
    :initarg :object
    :accessor triple-object)
   (source
    :initarg :source
    :initform nil
    :reader triple-source)
   (filepos 
    :initarg :filepos
    :initform (when nox:*current-parser*
                (nox::parser-filepos nox:*current-parser*))
    :reader triple-filepos)))

(defmethod print-object ((triple triple) stream)
  (print-unreadable-object (triple stream :type t :identity t)
    (format stream "~S ~S ~S"
            (triple-subject triple)
            (triple-predicate triple)
            (triple-object triple))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS DB
;;;

(defclass db ()
  ((triples
    :initform nil
    :accessor db-triples)
   (literal-class
    :initarg :literal-class
    :initform 'literal ; could also be STRING or any subclass of LITERAL 
    :reader db-literal-class)))

(defmethod initialize-instance :after ((self db) &key (emptyp t) &allow-other-keys)
  (unless emptyp
    (warn "Schema loading not supported for ~S" self)))

(defmethod print-object ((db db) stream)
  (print-unreadable-object (db stream :type t :identity t)
    (format stream "size ~S" (length (db-triples db)))))

(defmethod db-make-literal ((db db) string &rest options)
  (declare (dynamic-extent options))
  (let ((class (db-literal-class db)))
    (if (eq class 'string)
      string
      (apply #'make-instance class :string string options))))

(defmethod db-make-triple ((db db) subject predicate object &optional source)
  (make-instance 'triple
                 :subject subject :predicate predicate :object object :source source))

(defmethod db-add-triple ((db db) (triple triple))
  (let ((triples (db-query db
			   (triple-subject triple)
			   (triple-predicate triple)
			   (triple-object triple))))
    (cond (triples
	   (values (first triples) nil))
	  (t
	   (push triple (db-triples db))
	   (values triple t)))))

(defmethod db-del-triple ((db db) (triple triple))
  (setf (db-triples db) (remove triple (db-triples db)))
  triple)

(defun eq~ (x y)
  (or (null x)
      (null y)
      (eq x y)))

(defmethod db-query ((db db) subject predicate object)
  (remove-if-not #'(lambda (triple)
		     (and (eq~ (triple-subject triple) subject)
			  (eq~ (triple-predicate triple) predicate)
			  (eq~ (triple-object triple) object)))
		 (db-triples db)))

(defmethod db-reify ((triple triple) (db db)
                     &optional (statement-uri nil)
                               (source nil))
  (let ((node (node statement-uri)))
    (flet ((tr (p o)
	     (db-add-triple db (db-make-triple db node (node p) o source))))
      (tr -rdf-subject-uri-   (triple-subject triple))
      (tr -rdf-predicate-uri- (triple-predicate triple))
      (tr -rdf-object-uri-    (triple-object triple))
      (tr -rdf-type-uri-      (node -rdf-statement-uri-))
      node)))

(defmethod is-container-p ((db db) (node node) &optional errorp)
  ;; We may have to extend this to handle subclasses of containers
  (let ((container-types (list (node -rdf-bag-uri-)
                               (node -rdf-seq-uri-)
                               (node -rdf-alt-uri-))))
    (dolist (triple (db-query db node (node -rdf-type-uri-) nil))
      (when (find (triple-object triple) container-types)
        (return-from is-container-p t)))
    (when errorp
      (cerror "Ignore" 'container-required :thing node))))

(defmethod db-allow-merge-p ((to db) (from db))
  t)

(defmethod db-merge ((to db) (from db))
  (multiple-value-bind (allowp fail-reason)
                       (db-allow-merge-p to from)
    (unless allowp
      (cerror "Merge anyway" 'cannot-merge :thing (or fail-reason "unknown")))
    (dolist (triple (db-triples from))
      (db-add-triple to triple))))

(defmethod db-clear ((db db))
  (setf (db-triples db) nil))

(defmethod db-find-cbd ((db db) (node node))
  ;; Calculates the Concise Bounded Description as per Patrick Stickler's spec at
  ;; http://www.w3.org/Submission/2004/SUBM-CBD-20040930/
  (cbd (list node) nil nil nil db))

(defun cbd (nodes triples cbd-nodes cbd-triples db)
  (cond (nodes
	 (let ((n (first nodes)))
	   (if (member n cbd-nodes)
	     (cbd (rest nodes) triples cbd-nodes cbd-triples db)
	     (cbd (rest nodes)
		  (append triples (db-query db n nil nil))
		  (cons n cbd-nodes)
		  cbd-triples
		  db))))
	(triples
	 (let ((tr (first triples)))
	   (if (member tr cbd-triples)
	     (cbd nil (rest triples) cbd-nodes cbd-triples db)
	     (cbd (let ((s (triple-reified-p tr db))
			(o (triple-object tr)))
		    (if (and (typep o 'node)
			     (not (typep o 'literal))
			     (null (node-uri o)))
		      (cons o s)
		      s))
		  (rest triples)
		  cbd-nodes
		  (cons tr cbd-triples)
		  db))))
	(t
	 (values cbd-triples cbd-nodes))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   "CLASS" TRIPLE-INDEX
;;;

(defun make-triple-index ()
  (make-hash-table :test #'eq))

(defun triple-index-get (index key)
  (gethash key index))

(defun triple-index-add (index key triple)
  (unless (stringp key)
    (push triple (gethash key index))))

(defun triple-index-rem (index key triple)
  (unless (stringp key)
    (setf (gethash key index) (remove triple (gethash key index)))))

(defun triple-index-clear (index)
  (clrhash index))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS INDEXED-DB
;;;

(defclass indexed-db (db)
  ((by-subject
    :initform (make-triple-index)
    :reader db-by-subject)
   (by-predicate
    :initform (make-triple-index)
    :accessor db-by-predicate)
   (by-object
    :initform (make-triple-index)
    :accessor db-by-object)
   (by-source
    :initform (make-triple-index)
    :reader db-by-source))
  (:default-initargs
    :rdf-schema-pathname "wilbur:schemata;rdf-schema.rdf"))

(defmethod initialize-instance :after ((self indexed-db)
                                       &key (emptyp nil)
                                            rdf-schema-pathname
                                       &allow-other-keys)
  (unless emptyp
    (db-load self (nox:make-file-url rdf-schema-pathname)
	     :db self :merge-results-p nil)))

(defmethod db-add-triple ((db indexed-db) (triple triple))
  (multiple-value-bind (actual-triple addedp)
                       (call-next-method)
    (cond (addedp
	   (triple-index-add (db-by-subject db)   (triple-subject triple)   triple)
	   (triple-index-add (db-by-predicate db) (triple-predicate triple) triple)
	   (triple-index-add (db-by-object db)    (triple-object triple)    triple)
	   (triple-index-add (db-by-source db)    (triple-source triple)    triple)
	   (values triple t))
	  (t
	   (values actual-triple nil)))))

(defmethod db-del-triple :after ((db indexed-db) (triple triple))
  (triple-index-rem (db-by-subject db)   (triple-subject triple)   triple)
  (triple-index-rem (db-by-predicate db) (triple-predicate triple) triple)
  (triple-index-rem (db-by-object db)    (triple-object triple)    triple)
  (triple-index-rem (db-by-source db)    (triple-source triple)    triple))

(defmethod db-query ((db indexed-db) subject predicate object)
  (flet ((filter (tr k s)
           (if k (remove k tr :test-not #'eq :key s) tr)))
    (declare (dynamic-extent #'filter))
    (cond (subject
           (filter (filter (triple-index-get (db-by-subject db) subject)
                           predicate #'triple-predicate)
                   object #'triple-object))
          (object
           (filter (triple-index-get (db-by-object db) object)
                   predicate #'triple-predicate))
          (predicate
           (triple-index-get (db-by-predicate db) predicate))
          (t
           (db-triples db)))))

(defmethod db-del-source ((db indexed-db) (source node))
  (dolist (triple (db-query-by-source db source))
    (db-del-triple db triple)))

(defmethod db-query-by-source ((db indexed-db) (source node))
  (triple-index-get (db-by-source db) source))

(defmethod db-sources ((db indexed-db))
  (let ((sources nil))
    (maphash #'(lambda (key data)
                 (declare (ignore data))
                 (push key sources))
             (db-by-source db))
    sources))

(defmethod db-clear :after ((db indexed-db))
  (triple-index-clear (db-by-subject db))
  (triple-index-clear (db-by-predicate db))
  (triple-index-clear (db-by-object db))
  (triple-index-clear (db-by-source db)))


