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
;;;;  xml-renderer.lisp
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
;;;   Purpose: The XML renderer used for OWL 2 XML and OWLlink XML 
;;; 

;;;
;;; XML Renderer 
;;; 

(defvar *content-attributes* nil)

(defvar *map-tag-attributes-to-iri-attributes* nil)

(defvar *dont-abbreviate-iris-in-tags* nil)

(defvar *add-tag-content-as-attribute* nil)

(defvar *add-tag-content-as-attribute-value* nil)

;;;
;;;
;;;

(defun abbreviate-tag (tag)
  (owlapi:without-starting-# (iri-owl tag #\:)))

(defun abbreviate-value (tag)
  (iri-owl tag #\&))

;;;
;;;
;;; 

(defun indent (stream)
  (format stream "~%")
  (dotimes (i *indent*)
    (format stream " ")))  

(defmacro with-indentation (&body body)
  `(let* ((*indent* (+ *indent* 2)))
    ,@body))

(defun xml-header (stream)
  (format stream "<?xml version=\"1.0\"?>"))

(defun xml-comment (stream comment)
  (indent stream)
  (format stream "<!-- ~A -->" comment))

(defun xml-author-comment (name stream)
  (xml-comment stream (format nil "Ontology \"~A\" rendered by ~A Version ~A Build ~A, ~A"
                              name
                              *product-name*
                              *product-version*
                              *product-build*
                              (owlapi:get-current-date))))
        
(defun xml-block-comment (stream comment)
  (format stream "<!--")
  (indent stream)
  (pprint comment stream)
  (indent stream)
  (format stream "-->"))

(defun entity-definitions (stream prefixes &optional ontology-p)
  
  (indent stream)

  (if ontology-p 
      (format stream "<!DOCTYPE Ontology [")
    (format stream "<!DOCTYPE rdf:RDF ["))

  (with-indentation 
    (dolist (prefix prefixes)
      (when (and (not (owlapi:is-default-prefix-p (first prefix)))
                 (first prefix))
        (indent stream)
        (format stream "<!ENTITY ~A ~S>" (first prefix) (second prefix)))))

  (indent stream)
  (format stream "]>"))

(defmacro xml-tag-printer (stream tag attributes &body cont)
  `(xml-tag-printer1 ,stream ',tag ',attributes (lambda () ,cont)))

(defun xml-tag-printer1 (stream tag attributes1 &optional continuation)
  (let* ((tag0 tag)
         (tag 
          (abbreviate-tag 
           (cond ((stringp tag)
                  tag)
                 ((symbolp tag)
                  (symbol-name tag))
                 (t
                  (format nil "~A" tag)))))

         (attributes 
          (remove-if #'(lambda (x)
                         (and (member (first x) *dont-render-attributes*)
                              (not (member (first x) 
                                           (second (assoc tag0 *ensure-attributes-for-tag*))))))
                     attributes1))

         (*owl2-tags-disabled* 
          (or *owl2-tags-disabled* 
              (member tag0 *disable-owl2-tags-for*))))

    (unless (member tag0 *dont-render-tags*)

      (labels ((render-attribute (stream attribute)
                 (let ((attribute0 (first attribute))
                       (attribute1 (abbreviate-tag (first attribute))))
                   (multiple-value-bind (iri abbreviated-iri-p)
                       (if (third attribute)
                           (second attribute)
                         (cond (*functional-to-xml-mode*
                                (values 
                                 (iri-owl (second attribute) #\&) 
                                 ;;; dont use abbreviatedIRI -> IRI!
                                 nil))
                               (*owllink-mode* ; use XSD entities instead of abbreviatedIRIs for OWL XML 
                                (iri-owl (second attribute)))
                               (t
                                (abbreviate-value (second attribute)))))

                     (format stream " ~A=\"~A\"" 
                             (if (and abbreviated-iri-p
                                      (or (eq attribute0 '|IRI|)
                                          (member 
                                           attribute0
                                           (rest
                                            (assoc tag0 *map-tag-attributes-to-iri-attributes*)))))
                                 "abbreviatedIRI"
                             
                               (if (member 
                                    attribute0
                                    (rest
                                     (assoc tag0 *map-tag-attributes-to-iri-attributes*)))
                                   "IRI"
                                 attribute1))
                             iri)))))

        (indent stream) 

        (if (and (gethash tag0 *owl2-tags*)
                 (not *owl2-tags-disabled*))
            (format stream "<owl:~A" tag)
          (format stream "<~A" tag))
      
        (let ((*abbreviate-iris*
               (and *abbreviate-iris*
                    (not (member tag0 *dont-abbreviate-iris-in-tags*)))))
       
          (when attributes
            (render-attribute stream (first attributes))

            (let ((*indent* (+ *indent* 1 (length tag))))
              (dolist (attribute-value (rest attributes))
                (indent stream)
                (render-attribute stream attribute-value)))))

        (let* ((*abbreviate-iris*
                (if (not *owllink-mode*)
                    *abbreviate-iris*
                  (and *owllink-mode*
                       (let ((res (assoc '|INT-abbreviatesIRIs| attributes1)))
                         (if res
                             (owlapi:string-to-boolean
                              (second res))
                           *abbreviate-iris*)))))
               (*add-prefixes*
                (if (not *owllink-mode*)
                    *add-prefixes*
                  (or (when *abbreviate-iris*
                        (let ((res 
                               (assoc '|INT-prefixes| attributes1)))
                          (if res 
                              (second res)
                            *add-prefixes*)))
                      *add-prefixes*))))

          (if continuation
              (progn 
                (format stream ">")
                (unless 
                    (with-indentation
                      (funcall continuation))
                  (indent stream))
                (if (and (gethash tag0 *owl2-tags*)
                         (not *owl2-tags-disabled*))
                    (format stream "</owl:~A>" tag)
                  (format stream "</~A>" tag)))
            (format stream "/>")))))))

(defun print-xml (s-expr stream &optional (toplevel-p t))

  ;;; (tag attribute child1 ... childn) | symbol 

  (cond ((consp s-expr) 

         (cond ((and (member (first s-expr) '(|rdf:first|
                                              |owl:hasSelf|))
                     (stringp (third s-expr)))
                
                (xml-tag-printer1
                 stream 
                 (first s-expr)
                 (second s-expr) 
                 #'(lambda ()
                     (format stream "~A" (third s-expr))
                     ;;; t return -> no fresh line after content
                     t)))

               ((and (not (cdddr s-expr)) ; old OWL Literal Representation
                     (eq (first s-expr) '|Literal|)
                     (stringp (third s-expr)))

                (let* ((lit1
                        (parse-literal (list (third s-expr))))
                       (lit (second lit1))
                       (type (second (third lit1))))
                  
                  (xml-tag-printer1
                   stream 
                   '|Literal|
                   `((|datatypeIRI| ,type))
                   #'(lambda ()
                       (format stream "~A" lit)
                       ;;; t return -> no fresh line after content
                       t))))

               ((and (member (first s-expr) '(|OWLLiteral|))) ; OWL Literal 
                (let ((type (third s-expr))
                      (lit (fourth s-expr))
                      (lang-tag (fifth s-expr)))
                  (declare (ignorable lang-tag))

                  (xml-tag-printer1
                   stream 
                   '|Literal|
                   `((|datatypeIRI| ,type))
                   #'(lambda ()
                       (format stream "~A" lit)
                       ;;; t return -> no fresh line after content
                       t))))

               ((and (not (cdddr s-expr)) ; f. OWLlink OWL Literal 
                     (eq (first s-expr) '|Literal|)
                     (cddr s-expr))

                (let* ((lit1 (third s-expr))
                       (lit (second lit1))
                       (type (second (third lit1))))
                  
                  (xml-tag-printer1
                   stream 
                   '|Literal|
                   `((|datatypeIRI| ,type))
                   #'(lambda ()
                       (format stream "~A" lit)
                       ;;; t return -> no fresh line after content
                       t))))
                                    
               ((and (not (cddr s-expr))
                     (second s-expr)
                     (not (cdr (second s-expr)))
                     (member (caar (second s-expr))
                             (cdr (assoc (first s-expr) *content-attributes*))))

                (xml-tag-printer1
                 stream 
                 (first s-expr)
                 nil 
                 #'(lambda ()
                     (format stream "~A" (cadar (second s-expr)))
                     ;;; t return -> no fresh line after content
                     t)))
                
               ((and (not (cdddr s-expr))
                     (third s-expr)
                     ;;; einfacher Literal Content? (keine Children!) 
                     (symbolp (third s-expr)))

                ;;; 
                ;;; <Class>abc</Class> -> <Class IRI=\"abc\"/> 
                ;;;

                (xml-tag-printer1
                 stream 
                 (first s-expr)
                 (append (second s-expr)
                         (if (not *owllink-mode*)
                             (if toplevel-p
                                 `((|rdf:about| ,(third s-expr)))
                               `((|rdf:resource| ,(third s-expr))))
                           `((,(or (second (assoc (first s-expr)
                                                  *add-tag-content-as-attribute*))
                                   (second (assoc :otherwise
                                                  *add-tag-content-as-attribute*)))
                              ,(third s-expr)))))))
               
               ((and (not (cdddr s-expr))
                     (third s-expr)
                     (consp (third s-expr))
                     (member (first (third s-expr))
                             '(|owl:Class| 
                               ;; |owl:ObjectProperty| 
                               ;; |rdfs:Datatype| 
                               ))
                     (not (third (third s-expr)))
                     (member (first (first (second (third s-expr)))) 
                             '(|rdf:about| |rdf:resource| |rdf:ID|)))
                     
                (xml-tag-printer1
                 stream 
                 (first s-expr)
                 (append (second s-expr)
                         (if (not *owllink-mode*)
                             (if toplevel-p
                                 `((|rdf:about| ,(second (first (second (third s-expr))))))
                               `((|rdf:resource| ,(second (first (second (third s-expr)))))))
                           `((,(or (second (assoc (first s-expr)
                                                  *add-tag-content-as-attribute*))
                                   (second (assoc :otherwise
                                                  *add-tag-content-as-attribute*)))
                              ,(second (first (second (third s-expr))))))))))
               
               (t
                
                (let* ((tag (first s-expr))
                       (attributes (second s-expr))

                       (attribute-content
                        (remove-if-not #'(lambda (x) 
                                           (and (consp x)
                                                (assoc (first x) *add-tag-content-as-attribute-value*)))
                                       (cddr s-expr)))
                       (content
                        (remove-if #'(lambda (x) 
                                       (and (consp x)
                                            (assoc (first x) *add-tag-content-as-attribute-value*)))
                                   (cddr s-expr)))

                       (attributes
                        (append attributes
                                (mapcar #'(lambda (x) 
                                            (list (second (assoc (first x) 
                                                                 *add-tag-content-as-attribute-value*))
                                                  ;; content must be simple! only one element!
                                                  ;;  e.g. (|Cardinality| nil 1)
                                                  (third x)))
                                        attribute-content))))

                  (xml-tag-printer1
                   stream 
                   tag
                   attributes
                   (when content
                     (lambda () 
                       (if (or (cdr content)
                               (not (stringp (first content))))
                           (dolist (s-expr content)
                             (print-xml s-expr stream nil))
                         (progn 
                           (format stream "~A" (first content))
                           t)))))))))

        ((symbolp s-expr)
         (xml-tag-printer1
          stream s-expr nil))

        ((functionp s-expr)
         (funcall s-expr))

        ((and (stringp s-expr)
              *functional-to-xml-mode*)

         (let ((*owllink-mode* nil)) 
           (print-xml 
            `(|Literal| nil ,s-expr)
            stream 
            nil)))

        (t (format stream "~A" s-expr)
           t)))

;;;
;;; OWL XML 
;;;

(defmethod render (stream (ont owlapi:ontology) (syntax (eql :owl-xml)))
  
  (labels ((prepare-for-owl-rendering (expr)
             (if (consp expr)
                 (let ((op (first expr)))
                   (case op
                     (|HasKey| 
                      (list op nil
                            (prepare-for-owl-rendering (second expr))
                            (append
                             (mapcan #'prepare-for-owl-rendering (third expr))
                             (mapcan #'prepare-for-owl-rendering (fourth expr)))))
                     
                     (|DatatypeRestriction|
                      (append
                       (list op nil
                             (prepare-for-owl-rendering (second expr)))
                       (loop as facet in (cddr expr) by #'cddr
                             as literal in (cdddr expr) by #'cddr
                             collect 
                             `(|FacetRestriction| 
                               ((|facet| ,facet))
                               ,(prepare-for-owl-rendering literal)))))
                     
                     (otherwise
                      (list* op
                             nil
                             (mapcar #'prepare-for-owl-rendering (rest expr))))))
               expr)))

    (with-prefixes (ont prefixes default)

      (let* ((default (owlapi:without-\# default)) ; used as base!

             (*ontology* ont)
             (*axioms-rendered* 0)
             (*package* 
              (find-package #+:racer-server :ts 
                            #-:racer-server :owl-syntaxes))

             (*functional-to-xml-mode* t)
             (*owl2-tags-disabled* t)
             (*add-tag-content-as-attribute-value*
              '((|Cardinality| |cardinality|)))

             (name
              (owlapi:name ont))
             
             (axioms
              (owlapi:get-axioms-in ont))
             (axioms1
              (remove-if-not #'(lambda (x) (typep x 'owlapi:|OWLOntologyAnnotationAxiom|)) axioms))
             (axioms2
              (remove-if #'(lambda (x) (typep x 'owlapi:|OWLOntologyAnnotationAxiom|)) axioms))

             (axioms
              (mapcan #'(lambda (axioms)
                          (mapcan
                           #'(lambda (axiom)

                               ;; (pprint (type-of axiom))
                               ;; (render *standard-output* axiom :owl-functional)
             
                               (let* ((res 
                                       (with-output-to-string (stream)
                                         (let ((*comments* nil))
                                           (render stream axiom :owl-functional))))
                                      (res2 
                                       (with-input-from-string (stream res)
                                         (let (#+:racer-server 
                                               (*tbox-verbose* nil))

                                           ;; (pprint res)
                               
                                           (first (parse-from-stream stream))))))

                                 ;; (pprint res2)
                                 ;; (terpri) (terpri)

                                 (if res2
                                     (list (get-axiom-comment-closure axiom stream)
                                           (prepare-for-owl-rendering res2))                             
                                   (progn 
                                     (owlapi-warning "Warning - cannot render axioms of type ~A in syntax ~A"
                                                     (type-of axiom) 
                                                     syntax)
                                     nil))))
                           axioms))
                      (list axioms1 axioms2))))

        (xml-author-comment name stream)        

        (newline1 stream)
        
        (entity-definitions stream prefixes t)
    
        (newline1 stream)

        (owllink-xml-printer 
         (list* '|Ontology| 
                `((|INT-prefixes| ,(cons (list nil default) prefixes))
                  (|INT-abbreviatesIRIs| "true")

                  ("xmlns" ,owlapi:+owl2-namespace+)
                  ("xml:base" ,default)
                  ,@(mapcar #'(lambda (x)
                                (list (format nil "xmlns:~A" (first x))
                                      (second x)
                                      t))
                            (remove-if #'owlapi:is-default-prefix-p 
                                       prefixes
                                       :key #'first)))

                (nconc
                 (mapcar #'(lambda (prefix-namespace) 
                             (let ((prefix (first prefix-namespace))
                                   (namespace (second prefix-namespace)))
                               `(|Prefix| ((|name| ,prefix)
                                           (|IRI|  ,namespace)
                                           (|INT-abbreviatesIRIs| "false")))))
                         ;; (remove-if #'owlapi:is-default-prefix-p prefixes :key #'first)
                         nil)
      
                 axioms))

         stream)

        (newline1 stream)

        (xml-author-comment name stream)        

        (newline1 stream)

        (length axioms)))))
