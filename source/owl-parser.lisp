;;; -*- package: WILBUR; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;;  owl.lisp
;;;
;;;
;;; ----------------------------------------------------------------------------
;;;
;;;   The contents of this file are subject to the NOKOS License Version 1.0a
;;;   (the "License"); you may not use this file except in compliance with
;;;   the License. 
;;;
;;;   Software distributed under the License is distributed on an "AS IS"
;;;   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
;;;   License for the specific language governing rights and limitations under
;;;   the License. 
;;;
;;;   The Original Software is 
;;;     WILBUR: Nokia RDF/XML Processor for CLOS
;;;
;;;   Copyright (c) 2001 Nokia and others. All Rights Reserved.
;;;   Portions Copyright (c) 1989-1992 Ora Lassila. All Rights Reserved.
;;;
;;;   Contributor(s): Ora Lassila (mailto:ora.lassila@nokia.com)
;;;
;;; ----------------------------------------------------------------------------
;;;
;;;
;;;   Version: $Id: owl-parser.lisp,v 1.3 2001/07/17 22:25:07 theran Exp $
;;;
;;;   Purpose: This file contains an implementation of a owl parser.
;;;


(in-package :wilbur)

(eval-when (:compile-toplevel :load-toplevel :execute)
   (enable-node-shorthand)
)

;;; ----------------------------------------------------------------------------
;;;
;;;   owl CONSTANTS
;;;

(defconstant -owl-uri-   "http://www.w3.org/2002/07/owl#")

(defconstant -owl-list-uri-  (concatenate 'string -rdf-uri- "List"))
(defconstant -owl-first-uri- (concatenate 'string -rdf-uri- "first"))
(defconstant -owl-rest-uri-  (concatenate 'string -rdf-uri- "rest"))
(defconstant -owl-nil-uri-   (concatenate 'string -rdf-uri- "nil"))


;;; ----------------------------------------------------------------------------
;;;
;;;   CLASS owl-PARSER
;;;

(defclass non-indexed-db
  (db)
  ())

(defmethod db-add-triple ((db non-indexed-db) (triple triple))
  (push triple (db-triples db))
  (values triple t))

(defclass owl-parser (rdf-parser)
  ()
  (:default-initargs :db (make-instance 'non-indexed-db)))

(defmethod parse-using-parsetype ((parser owl-parser) node property parsetype
                                  &optional statement-id language datatype)
  (declare (ignore statement-id language datatype))
  (cond ((string= parsetype "Collection")
         (let ((list-node (ensure-node parser nil t)))
           (let ((triple (add-as-triple parser node property list-node)))
             (setf (node-triple list-node) triple))
           (add-state parser :owl-collection list-node)))
        (t (call-next-method))))


(defmethod nox:start-element ((parser owl-parser)
                              (tag nox:open-tag)
                              (mode (eql :owl-collection)))
  (nox:start-element parser tag :description))

(defmethod attach-to-parent ((parser owl-parser)
                             (parent node)
                             (child node)
                             &optional statement-id)
  (declare (ignore statement-id))
  (let ((state (first (parser-states parser))))
    (cond ((eq (state-mode state) :owl-collection)
           (let ((parent (state-node state))
                 (node (ensure-node parser nil t)))
             (add-as-triple parser parent -rdf-type-uri-
                            (ensure-node parser -owl-list-uri- t))
             (add-as-triple parser parent -owl-first-uri- child)
             ;;(add-as-triple parser parent -owl-rest-uri- node)
             (let ((triple (add-as-triple parser parent -owl-rest-uri- node)))
               (setf (node-triple node) triple))
             (setf (state-node state) node)))
          (t (call-next-method)))))

(defmethod nox:end-element :before ((parser owl-parser)
                                    (tag nox:open-tag)
                                    (mode (eql :owl-collection)))
  (let* ((node (state-node (first (parser-states parser))))
         ;;(db (parser-db parser))
         (triple 
          ;;(db-del-triple db (first (db-query db nil nil node)))
          (node-triple node)))
    #+:debug
    (assert (not (null triple)))
    #|(add-as-triple parser
                   (triple-subject triple)
                   (triple-predicate triple)
                   (ensure-node parser -owl-nil-uri- t))|#
    (setf (triple-object triple) (ensure-node parser -owl-nil-uri- t))))


;;; ----------------------------------------------------------------------------
;;;
;;;   CONSTRUCTORS FOR owl COLLECTIONS
;;;

(defun owl-list (&rest items)
  (if items
    (owl-cons (first items) (apply #'owl-list (rest items)))
    !owl:nil))

(defun owl-cons (first rest &optional uri)
  (flet ((triple (subject predicate object &optional source)
           (make-instance 'triple
             :subject subject :predicate predicate :object object :source source)))
    (let ((pair (node uri)))
      (db-add-triple *db* (triple pair !rdf:type (node -owl-list-uri-)))
      (db-add-triple *db* (triple pair (node -owl-first-uri-) first))
      (db-add-triple *db* (triple pair (node -owl-rest-uri-) rest))
      pair)))

;;; ======================================================================

#|
(defmethod nox:char-content ((parser owl-parser)
                             (content string)
                             (mode (eql :property)))
  (break)
  |#

(defmethod nox:char-content ((self owl-parser) char-content (mode (eql :property)))
  (racer:racer-warn "Is this character content correct? ~S" char-content)
  char-content)