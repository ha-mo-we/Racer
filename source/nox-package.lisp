;;; -*- package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   The contents of this file are subject to the NOKOS License Version 1.0a (the
;;;   #:license); you may not use this file except in compliance with the License. 
;;;
;;;   Software distributed under the License is distributed on an #:as is basis, WITHOUT
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


(in-package :cl-user)


;;; --------------------------------------------------------------------------------------
;;;
;;;   PACKAGE NOX-RACER
;;;

(defpackage :nox-racer
  ;;(:nicknames ;:nokia-xml-cl
  ;;	      ;:wilbur-xml
  ;;            #+:allegro "NOX-RACER")
  (:use :common-lisp
	#+:ccl :ccl
	#+:excl :excl
	#+:sbcl :sb-sys)
  (:export #:xml-error                  ; from xml-util.lisp
	   #:define-constant
	   #:error-thing
	   #:syntax-error
	   #:pi-termination-problem
	   #:dtd-termination-problem
	   #:unexpected-end-tag
	   #:error-expectation
	   #:unknown-declaration
	   #:unknown-character-reference
	   #:malformed-url
	   #:feature-not-supported
	   #:missing-definition
	   #:error-definition-type
	   #:missing-entity-definition
	   #:missing-namespace-definition
	   #:xml-warning
	   #:*current-parser*
	   #:read-using
	   #:string-dict-get
	   #:string-dict-get-by-value
	   #:string-dict-add
	   #:string-dict-del
	   #:do-string-dict
	   #:make-file-url
	   #:make-http-url
	   #:parse-url
	   #:token
	   #:token-string
	   #:open-tag
	   #:close-tag
	   #:entity-declaration
	   #:entity-name
	   #:comment
	   #:char-content
	   #:tag-counterpart
	   #:tag-attribute
	   #:tag-attributes
	   #:tag-empty-p
	   #:tag-namespaces
	   #:start-element
	   #:end-element
	   #:char-content
	   #:proc-instruction
	   #:start-document
	   #:end-document
	   #:maybe-use-namespace
	   #:sax-consumer
	   #:sax-consumer-producer
	   #:sax-consumer-mode
	   #:sax-producer
	   #:sax-producer-consumer
	   #:sax-filter
	   #:find-first-producer
	   #:-whitespace-chars-
	   #:with-resource-from-pool
	   #:define-resource-pool
	   #:collapse-whitespace
	   #:*name-reader*              ; from xml-parser.lisp
	   #:xml-parser
	   #:get-entity
	   #:get-canonical-uri
	   #:parse
	   #:expand-name-with-namespace
	   #:parse-from-stream
	   #:parse-from-file
	   #:xml-formatter
	   #:replay
	   #:reverse-expand-name
	   #:tree-parser
	   #:string->keyword
	   #:parser-interpret-content
	   #:-rdf-uri-                  ; from rdf-constants.lisp
	   #:-rdfs-uri-
	   #:-xsd-uri-
	   #:rdf-uri
	   #:rdfs-uri
	   #:-rdf-attrs-
	   #:-rdf-attr-map-
	   #:-rdf-id-uri-
	   #:-rdf-resource-uri-
	   #:-rdf-about-uri-
	   #:-rdf-abouteach-uri-
	   #:-rdf-abouteachprefix-uri-
	   #:-rdf-bagid-uri-
	   #:-rdf-parsetype-uri-
	   #:-rdf-datatype-uri-
	   #:-rdf-nodeid-uri-
	   #:-xml-lang-attr-
	   #:-rdf-description-uri-
	   #:-rdf-type-uri-
	   #:-rdf-rdf-uri-
	   #:-rdf-li-uri-
	   #:-rdf-statement-uri-
	   #:-rdf-subject-uri-
	   #:-rdf-predicate-uri-
	   #:-rdf-object-uri-
	   #:-rdf-bag-uri-
	   #:-rdf-seq-uri-
	   #:-rdf-alt-uri-
	   #:-rdf-first-uri-
	   #:-rdf-rest-uri-
	   #:-rdf-nil-uri-
	   #:-rdfs-resource-uri-
	   #:-rdfs-class-uri-
	   #:-rdfs-subclassof-uri-
	   #:-rdfs-subpropertyof-uri-
	   #:-rdfs-seealso-uri-
	   #:-rdfs-isdefinedby-uri-
	   #:-rdfs-constraintresource-uri-
	   #:-rdfs-constraintproperty-uri-
	   #:-rdfs-range-uri-
	   #:-rdfs-domain-uri-
	   #:-rdfs-comment-uri-
	   #:-rdfs-label-uri-
	   #:-rdfs-literal-uri-
	   #:-rdfs-container-uri-

           #:tag-base
           #:tag-original-name
           #:parser-base
           #:parser-filepos
           #:*current-parser*
           #:wilbur-error))
