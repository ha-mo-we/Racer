;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: NOX; Base: 10 -*-
;;;
;;;;  rdf-constants.lisp
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
;;;   Version: $Id: rdf-constants.lisp,v 1.5.2.1 2004/11/30 17:11:24 ora Exp $
;;;
;;;   Purpose: This file contains definitions for various constants used by the
;;;   RDF parser (mostly URIs). Given that the XML parser has to deal with the
;;;   issue of RDF M+S vagueness on the namespaces of RDF attributes (such as
;;;   "about"), the definitions in this file are in the NOX package.
;;;
;;;   Generally, I hate this stuff since I never seem to get the constant
;;;   definitions right vis-a-vis compile time vs. load-time. :-(
;;;


(in-package "NOX")

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HELPERS
;;;
  
(eval-when (:compile-toplevel :load-toplevel)

  (define-constant -rdf-uri-  #."http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  (define-constant -rdfs-uri- #."http://www.w3.org/2000/01/rdf-schema#")
  (define-constant -xsd-uri-  #."http://www.w3.org/2001/XMLSchema#")
  
  (defmacro rdf-uri (string)  `(concatenate 'string -rdf-uri- ,string))
  (defmacro rdfs-uri (string) `(concatenate 'string -rdfs-uri- ,string))
  (defmacro xsd-uri (string)  `(concatenate 'string -xsd-uri- ,string))

  (define-constant -alternate-rdf-uri-
    #."http://www.w3.org/TR/REC-rdf-syntax/")
  (define-constant -alternate-rdfs-uri-
    #."http://www.w3.org/TR/1999/PR-rdf-schema-19990303#"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF M+S ATTRIBUTE URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (define-constant -rdf-id-uri-              #.(rdf-uri "ID"))
  (define-constant -rdf-resource-uri-        #.(rdf-uri "resource"))
  (define-constant -rdf-about-uri-           #.(rdf-uri "about"))
  (define-constant -rdf-abouteach-uri-       #.(rdf-uri "aboutEach"))
  (define-constant -rdf-abouteachprefix-uri- #.(rdf-uri "aboutEachPrefix"))
  (define-constant -rdf-bagid-uri-           #.(rdf-uri "bagID"))
  (define-constant -rdf-parsetype-uri-       #.(rdf-uri "parseType"))
  (define-constant -rdf-datatype-uri-        #.(rdf-uri "datatype"))
  (define-constant -rdf-nodeid-uri-          #.(rdf-uri "nodeID"))
  (define-constant -xml-lang-attr-           "xml:lang"))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF M+S RESOURCE, PROPERTY, ETC. URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (define-constant -rdf-description-uri- #.(rdf-uri "Description"))
  (define-constant -rdf-type-uri-        #.(rdf-uri "type"))
  (define-constant -rdf-rdf-uri-         #.(rdf-uri "RDF"))
  (define-constant -rdf-li-uri-          #.(rdf-uri "li"))
  (define-constant -rdf-statement-uri-   #.(rdf-uri "Statement"))
  (define-constant -rdf-subject-uri-     #.(rdf-uri "subject"))
  (define-constant -rdf-predicate-uri-   #.(rdf-uri "predicate"))
  (define-constant -rdf-object-uri-      #.(rdf-uri "object"))
  (define-constant -rdf-xmlliteral-uri-  #.(rdf-uri "XMLLiteral"))
  (define-constant -rdf-bag-uri-         #.(rdf-uri "Bag"))
  (define-constant -rdf-seq-uri-         #.(rdf-uri "Seq"))
  (define-constant -rdf-alt-uri-         #.(rdf-uri "Alt"))
  (define-constant -rdf-list-uri-        #.(rdf-uri "List"))
  (define-constant -rdf-first-uri-       #.(rdf-uri "first"))
  (define-constant -rdf-rest-uri-        #.(rdf-uri "rest"))
  (define-constant -rdf-nil-uri-         #.(rdf-uri "nil")))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF SCHEMA URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (define-constant -rdfs-resource-uri-           #.(rdfs-uri "Resource"))
  (define-constant -rdfs-class-uri-              #.(rdfs-uri "Class"))
  (define-constant -rdfs-subclassof-uri-         #.(rdfs-uri "subClassOf"))
  (define-constant -rdfs-subpropertyof-uri-      #.(rdfs-uri "subPropertyOf"))
  (define-constant -rdfs-seealso-uri-            #.(rdfs-uri "seeAlso"))
  (define-constant -rdfs-isdefinedby-uri-        #.(rdfs-uri "isDefinedBy"))
  (define-constant -rdfs-constraintresource-uri- #.(rdfs-uri "ConstraintResource"))
  (define-constant -rdfs-constraintproperty-uri- #.(rdfs-uri "ConstraintProperty"))
  (define-constant -rdfs-range-uri-              #.(rdfs-uri "range"))
  (define-constant -rdfs-domain-uri-             #.(rdfs-uri "domain"))
  (define-constant -rdfs-comment-uri-            #.(rdfs-uri "comment"))
  (define-constant -rdfs-label-uri-              #.(rdfs-uri "label"))
  (define-constant -rdfs-literal-uri-            #.(rdfs-uri "Literal"))
  (define-constant -rdfs-datatype-uri-           #.(rdfs-uri "Datatype"))
  (define-constant -rdfs-container-uri-          #.(rdfs-uri "Container "))
  (define-constant -rdfs-member-uri-             #.(rdfs-uri "member")))


;;; --------------------------------------------------------------------------------------
;;;
;;;   XSD URIS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (define-constant -xsd-string-uri-             #.(xsd-uri "string"))
  (define-constant -xsd-boolean-uri-            #.(xsd-uri "boolean"))
  ;;(define-constant -xsd-decimal-uri-            #.(xsd-uri "decimal"))
  (define-constant -xsd-float-uri-              #.(xsd-uri "float"))
  ;;(define-constant -xsd-double-uri-             #.(xsd-uri "double"))
  (define-constant -xsd-datetime-uri-           #.(xsd-uri "dateTime"))
  ;;(define-constant -xsd-time-uri-               #.(xsd-uri "time"))
  (define-constant -xsd-date-uri-               #.(xsd-uri "date"))
  ;;(define-constant -xsd-gyearmonth-uri-         #.(xsd-uri "gYearMonth"))
  ;;(define-constant -xsd-gyear-uri-              #.(xsd-uri "gYear"))
  ;;(define-constant -xsd-gmonthday-uri-          #.(xsd-uri "gMonthDay"))
  ;;(define-constant -xsd-gday-uri-               #.(xsd-uri "gDay"))
  ;;(define-constant -xsd-gmonth-uri-             #.(xsd-uri "gMonth"))
  ;;(define-constant -xsd-hexbinary-uri-          #.(xsd-uri "hexBinary"))
  ;;(define-constant -xsd-base64binary-uri-       #.(xsd-uri "base64Binary"))
  ;;(define-constant -xsd-anyuri-uri-             #.(xsd-uri "anyURI"))
  (define-constant -xsd-normalizedstring-uri-   #.(xsd-uri "normalizedString"))
  ;;(define-constant -xsd-token-uri-              #.(xsd-uri "token"))
  ;;(define-constant -xsd-language-uri-           #.(xsd-uri "language"))
  ;;(define-constant -xsd-nmtoken-uri-            #.(xsd-uri "NMTOKEN"))
  ;;(define-constant -xsd-name-uri-               #.(xsd-uri "Name"))
  ;;(define-constant -xsd-ncname-uri-             #.(xsd-uri "NCName"))
  (define-constant -xsd-integer-uri-            #.(xsd-uri "integer"))
  ;;(define-constant -xsd-nonpositiveinteger-uri- #.(xsd-uri "nonPositiveInteger"))
  ;;(define-constant -xsd-negativeinteger-uri-    #.(xsd-uri "negativeInteger"))
  ;;(define-constant -xsd-long-uri-               #.(xsd-uri "long"))
  ;;(define-constant -xsd-int-uri-                #.(xsd-uri "int"))
  ;;(define-constant -xsd-short-uri-              #.(xsd-uri "short"))
  ;;(define-constant -xsd-byte-uri-               #.(xsd-uri "byte"))
  ;;(define-constant -xsd-nonnegativeinteger-uri- #.(xsd-uri "nonNegativeInteger"))
  ;;(define-constant -xsd-unsignedlong-uri-       #.(xsd-uri "unsignedLong"))
  ;;(define-constant -xsd-unsignedint-uri-        #.(xsd-uri "unsignedInt"))
  ;;(define-constant -xsd-unsignedshort-uri-      #.(xsd-uri "unsignedShort"))
  ;;(define-constant -xsd-unsignedbyte-uri-       #.(xsd-uri "unsignedByte"))
  ;;(define-constant -xsd-positiveinteger-uri-    #.(xsd-uri "positiveInteger"))
  )


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF ATTRIBUTE LISTS
;;;

(eval-when (:compile-toplevel :load-toplevel)

  (define-constant -rdf-attrs- '#.`(,-rdf-id-uri-
				,-rdf-resource-uri-
				,-rdf-about-uri-
				,-rdf-abouteach-uri-
				,-rdf-abouteachprefix-uri-
				,-rdf-bagid-uri-
				,-rdf-parsetype-uri-
				,-rdf-datatype-uri-
				,-rdf-nodeid-uri-
				,-xml-lang-attr-))

  (define-constant -rdf-attr-map- #.`'((,"ID"              . ,-rdf-id-uri-)
				   (,"resource"        . ,-rdf-resource-uri-)
				   (,"about"           . ,-rdf-about-uri-)
				   (,"aboutEach"       . ,-rdf-abouteach-uri-)
				   (,"aboutEachPrefix" . ,-rdf-abouteachprefix-uri-)
				   (,"bagID"           . ,-rdf-bagid-uri-)
				   (,"parseType"       . ,-rdf-parsetype-uri-)
				   (,"datatype"        . ,-rdf-datatype-uri-)
				   (,"nodeID"          . ,-rdf-nodeid-uri-))))
