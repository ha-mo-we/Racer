;;; -*- package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

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
;;;   Purpose: Definition of the package #:wilbur.
;;;


(in-package :cl-user)


;;; --------------------------------------------------------------------------------------
;;;
;;;   PACKAGE WILBUR
;;;

(defpackage :wilbur-racer
  ;;(:nicknames ;:nokia-rdf-cl
  ;;	      ;:wilbu-rdf
  ;;	      ;; :w                    removed because this causes problem in Lispworks 4.5
  ;;            #+:allegro "WILBUR-RACER")
  (:use #:common-lisp
	#+:ccl :ccl
	#+:excl :excl
	#+:excl :socket
	#+:excl :mop
	#+:sbcl :sb-sys)
  (:import-from #:nox-racer
		#:-rdf-uri-             ; from rdf-constants.lisp
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
		#:-rdfs-container-uri-)
  (:export #:-rdf-uri-                  ; from rdf-constants.lisp
	   #:-rdfs-uri-
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
	   #:node                       ; from rdf-data.lisp
	   #:triple
	   #:add-triple
	   #:del-triple
	   #:query
	   #:reify
	   #:add-namespace
	   #:del-namespace
	   #:namespaces
	   #:rdf-error
	   #:*db*
	   #:feature-not-supported      ; this is not the one from NOX-RACER!
	   #:about-and-id-both-present
	   #:unknown-parsetype
	   #:illegal-character-content
	   #:container-required
	   #:out-of-sequence-index
	   #:duplicate-namespace-prefix
	   #:node-uri
	   #:node-triple
	   #:node-name-resolved-p
	   #:index-uri
	   #:dictionary
	   #:dictionary-nodes
	   #:dictionary-namespaces
	   #:dictionary-unresolved-nodes
	   #:dictionary-node-class
	   #:dictionary-add-namespace
	   #:dictionary-remove-namespace
	   #:dictionary-rename-namespace
	   #:find-node
	   #:find-short-name
	   #:find-long-name
	   #:dictionary-apropos-list
	   #:*nodes*
           #:triple-attributes
           #:triple-uri
           #:triple-name-resolved-p
	   #:triple-subject
	   #:triple-predicate
	   #:triple-object
	   #:triple-source
	   #:db
	   #:db-triples
	   #:*db*
	   #:db-add-triple
	   #:db-del-triple
	   #:db-query
	   #:db-reify
	   #:db-del-source
	   #:db-query-by-source
	   #:db-sources
	   #:is-container-p
	   #:db-merge
	   #:db-clear
	   #:db-find-cbd
	   #:db-load
	   #:db-load-using-source
	   #:source-locator
	   #:source-open-stream
	   #:source-close-stream
	   #:source-with-modification
	   #:source-original-stream
	   #:source-modification
	   #:indexed-db
	   #:rdf-syntax-normalizer      ; from rdf-parser.lisp
	   #:rdf-parser
	   #:parser-db
	   #:close-rdf-element
	   #:make-container
	   #:parse-db-from-stream
	   #:parse-db-from-file
	   #:attach-to-parent
	   #:parse-using-parsetype
	   #:execute-deferred-task
	   #:task
	   #:defer-task
	   #:task-type
	   #:task-node
	   #:task-parameter
	   #:parser-node
	   #:parser-property
	   #:enable-node-shorthand
	   #:url                        ; from http-client.lisp
	   #:http-url
	   #:file-url
	   #:url-string
	   #:url-path
	   #:url-host
	   #:url-port
	   #:make-url
	   #:open-http-stream
	   #:http-message
	   #:http-status
	   #:http-version
	   #:http-headers
	   #:http-body
	   #:get-header
	   #:http-head
	   #:http-get
	   #:with-http-response
	   #:parse-http-date
	   #:parse-iso8601-date
	   #:iso8601-date-string
	   #:daml-parser                ; from daml-parser.lisp
	   #:-daml+oil-uri-
	   #:-daml-list-uri-
	   #:-daml-first-uri-
	   #:-daml-rest-uri-
	   #:-daml-nil-uri-
	   #:daml-list
	   #:daml-cons
	   #:load-db                    ; from ivanhoe.lisp
	   #:load-db-from-stream
	   #-(and :openmcl :darwin :uffi) #:*http-proxy*
	   #:find-http-proxy
	   #:path
	   #:path-expression
	   #:invert-path
	   #:walk-using-fsa
	   #:collect-using-fsa
	   #:get-value
	   #:get-all-values
	   #:frame
	   #:own-slots
	   #:value
	   #:all-values
	   #:add-value
	   #:del-value
	   #:relatedp

           #:parser-base
           #:*user-defined-nodes*
           #:literal-datatype
           #:literal-string
           #:triple-filepos))
