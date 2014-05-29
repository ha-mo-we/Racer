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
;;;;  owl-renderer.lisp
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
;;;   Purpose: A renderer / printer for OWL 2 in RDF/XML syntax 
;;; 

(defun rdf-list (list)
  `(|rdf:List| nil
               ,@(let ((first (first (first list)))
                       (rest (rest list))
                       (type (or (second (first list))
                                 "xsd:string")))
                   (if first
                       `((|rdf:first| ((|rdf:datatype| ,type)) ,first)
                         ,(if rest 
                              `(|rdf:rest| ((|rdf:parseType| |Resource|)) ,(rdf-list rest))
                            `(|rdf:rest| ((|rdf:resource| |rdf:nil|)))))
                     `((|rdf:rest| ((|rdf:resource| |rdf:nil|))))))))

(defun rdf-list2 (list &optional id)
  `(|rdf:List| ,(when id 
                  `((|rdf:ID| ,id)))
               
               ,@(let ((first (first list))
                       (rest (rest list)))
                  
                   (if first
                       `((|rdf:first| nil ,first)
                         ,(if rest
                              `(|rdf:rest| nil ,(rdf-list2 rest))
                            `(|rdf:rest| ((|rdf:resource| |rdf:nil|)))))
                     `((|rdf:rest| ((|rdf:resource| |rdf:nil|))))))))

(defun rdf-list3 (list &optional id)
  `(|rdf:List| ,(when id 
                  `((|rdf:ID| ,id)))
               
               ,@(let* ((first (first list))
                        (rest (rest list))
                        (type  (if (consp first)
                                   `((|rdf:datatype| ,(second first)))
                                 nil))
                        (first (if (consp first)
                                   (first first)
                                 first)))
                  
                   (if first
                       `((|rdf:first| ,type ,first)
                         ,(if rest
                              `(|rdf:rest| nil ,(rdf-list3 rest))
                            `(|rdf:rest| ((|rdf:resource| |rdf:nil|)))))
                     `((|rdf:rest| ((|rdf:resource| |rdf:nil|))))))))

;;;
;;;
;;; 

(defun trans-facet-owl (facet)
  facet)

(defun trans-ind-owl (ind &optional top-level-p)
  (if (not top-level-p)
      ind
    `(|owl:NamedIndividual| ((|rdf:about| ,ind)))))

(defun trans-data-range-owl (expr)
  (cond ((symbolp expr)
         
         ;;;`(|rdfs:Datatype| nil ,expr))

         ;;; expr

         ;;; `(|rdf:Description| ((|rdf:about| ,expr))))

         `(|rdfs:Datatype| ((|rdf:about| ,expr))))

        ((consp expr)
         
         (case (first expr)
                        
           (d-base-type
            (trans-data-range-owl (second expr)))

           (d-complement
            `(|rdfs:Datatype| nil 
                              (|owl:datatypeComplementOf| nil 
                                                          ,(trans-data-range-owl (second expr)))))

           (d-and 
            `(|rdfs:Datatype| nil 
                              (|owl:intersectionOf| ((|rdf:parseType| |Collection|)) 
                                                    ,@(mapcar #'trans-data-range-owl (rest expr)))))
           
           (d-or 
            `(|rdfs:Datatype| nil 
                              (|owl:unionOf| ((|rdf:parseType| |Collection|)) 
                                             ,@(mapcar #'trans-data-range-owl (rest expr)))))
           
           (d-possible-values
            `(|rdfs:Datatype| nil 
                              (|owl:oneOf| ;((|rdf:parseType| |Resource|)) 
                               nil
                               ,(rdf-list3
                                 (mapcar #'(lambda (x) 
                                             (multiple-value-call #'list 
                                               (trans-literal-owl x)))
                                         (rest expr))))))
           
           (d-restriction
            (let* ((basetype (second expr))
                   (facets-and-values (cddr expr))
                   (facets-and-values
                    (loop as facet-and-value in facets-and-values 
                          collect (list (trans-facet-owl (second facet-and-value))
                                        (multiple-value-call #'list
                                          (trans-literal-owl (third facet-and-value)))))))

              `(|rdfs:Datatype| 
                nil 
                (|owl:onDatatype| 
                 nil 
                 ,(trans-data-range-owl basetype))
                (|owl:withRestrictions| 
                 ((|rdf:parseType| |Collection|)) 
                 ,@(mapcar #'(lambda (facet-and-value)
                               (let ((facet (first facet-and-value))
                                     (value (first (second facet-and-value)))
                                     (type (second (second facet-and-value))))
                                 `(|rdf:Description| nil 
                                                     (,facet ((|rdf:datatype| ,type))
                                                             ,value))))
                           facets-and-values)))))))))

(defun trans-object-property-owl (role)
  (if (consp role)
      (if (eq (first role) 'inv)
          `(|owl:ObjectProperty| nil 
                                 (|owl:inverseOf| nil 
                                                  ,(trans-object-property-owl (second role))))
        (owlapi-runtime-error "Cannot render role expression ~A!" role))
    
    `(|owl:ObjectProperty| ((|rdf:about| ,role)))))
    
(defun trans-data-property-owl (role)
  `(|owl:DatatypeProperty| ((|rdf:about| ,role))))

(defun trans-annotation-property-owl (role)
  `(|owl:AnnotationProperty| ((|rdf:about| ,role))))


(defun trans-datatype-owl (dtname)
  `(|rdfs:Datatype| ((|rdf:about| ,dtname))))


(defun trans-literal-owl (lit)
  (if (consp lit)
      
      ;; (d-literal "42" (d-base-type ...))
      (values (second lit)
              (second (third lit)))

    (values lit "xsd:string")))

(defun trans-concept-owl (concept)
  (cond ((stringp concept)
         
         (trans-concept-owl (intern concept)))

        ((symbolp concept)
         
         `(|owl:Class| ((|rdf:about| ,concept))))
	
        ((consp concept)
              
         (case (first concept)

           (not
            `(|owl:Class| nil
                          (|owl:complementOf| nil 
                                              ,(trans-concept-owl (second concept)))))

           (and
            `(|owl:Class| nil
                          (|owl:intersectionOf| ((|rdf:parseType| |Collection|)) 
                                                ,@(mapcar #'trans-concept-owl (rest concept)))))

           (or
            `(|owl:Class| nil
                          (|owl:unionOf| ((|rdf:parseType| |Collection|)) 
                                         ,@(mapcar #'trans-concept-owl (rest concept)))))
           
           (some 
            `(|owl:Restriction| nil
                                (|owl:onProperty| nil 
                                                  ,(trans-object-property-owl (second concept)))
                                (|owl:someValuesFrom| nil
                                                      ,(trans-concept-owl (third concept)))))
           
           (all
            `(|owl:Restriction| nil
                                (|owl:onProperty| nil 
                                                  ,(trans-object-property-owl (second concept)))
                                (|owl:allValuesFrom| nil
                                                     ,(trans-concept-owl (third concept)))))
                
           (at-least 
            (if (or (not (fourth concept))
                    (member (fourth concept) (list 'top '*top* owlapi:+owlapi-owl-top+)))
                `(|owl:Restriction| nil
                                    (|owl:onProperty| nil 
                                                      ,(trans-object-property-owl (third concept)))
                                    (|owl:minCardinality| ((|rdf:datatype| |xsd:nonNegativeInteger|))
                                                          ,(second concept)))
              `(|owl:Restriction| nil
                                  (|owl:onProperty| nil 
                                                    ,(trans-object-property-owl (third concept)))
                                  (|owl:minQualifiedCardinality| ((|rdf:datatype| |xsd:nonNegativeInteger|))
                                                                 ,(second concept))
                                  (|owl:onClass| nil 
                                                 ,(trans-concept-owl (fourth concept))))))
           
           (at-most 
            (if (or (not (fourth concept))
                    (member (fourth concept) (list 'top '*top* owlapi:+owlapi-owl-top+)))
                `(|owl:Restriction| nil
                                    (|owl:onProperty| nil 
                                                      ,(trans-object-property-owl (third concept)))
                                    (|owl:maxCardinality| ((|rdf:datatype| |xsd:nonNegativeInteger|))
                                                          ,(second concept)))
              `(|owl:Restriction| nil
                                  (|owl:onProperty| nil 
                                                    ,(trans-object-property-owl (third concept)))
                                  (|owl:maxQualifiedCardinality| ((|rdf:datatype| |xsd:nonNegativeInteger|))
                                                                 ,(second concept))
                                  (|owl:onClass| nil 
                                                 ,(trans-concept-owl (fourth concept))))))
           
           (exactly 
            (if (or (not (fourth concept))
                    (member (fourth concept) (list 'top '*top* owlapi:+owlapi-owl-top+)))
                `(|owl:Restriction| nil
                                    (|owl:onProperty| nil 
                                                      ,(trans-object-property-owl (third concept)))
                                    (|owl:cardinality| ((|rdf:datatype| |xsd:nonNegativeInteger|))
                                                       ,(second concept)))
              `(|owl:Restriction| nil
                                  (|owl:onProperty| nil 
                                                    ,(trans-object-property-owl (third concept)))
                                  (|owl:qualifiedCardinality| ((|rdf:datatype| |xsd:nonNegativeInteger|))
                                                              ,(second concept))
                                  (|owl:onClass| nil 
                                                 ,(trans-concept-owl (fourth concept))))))
           
           (one-of
            `(|owl:Class| nil 
                          (|owl:oneOf| ((|rdf:parseType| |Collection|))
                                       ,@(mapcar #'(lambda (x) 
                                                     `(|rdf:Description| nil ,x))
                                                 (rest concept)))))
           
           (has-value
            `(|owl:Restriction| nil
                                (|owl:onProperty| nil 
                                                  ,(trans-object-property-owl (second concept)))
                                (|owl:hasValue| nil 
                                                ,(trans-concept-owl (third concept)))))
           
           (self-reference
            `(|owl:Restriction| nil
                                (|owl:onProperty| nil 
                                                  ,(trans-object-property-owl (second concept)))
                                (|owl:hasSelf| nil ;; korrekt? true? 
                                               "true")))
           
           (d-some
            (let ((data-range (first (last concept)))
                  (properties (butlast (cdr concept))))

              (if (cdr properties)
                  `(|owl:Restriction| nil
                                      (|owl:onProperties| ((|rdf:parseType| |Collection|))
                                                          ,@(mapcar #'trans-data-property-owl properties))
                                      (|owl:someValuesFrom| nil 
                                                            ,(trans-data-range-owl data-range)))
                `(|owl:Restriction| nil
                                    (|owl:onProperty| nil 
                                                      ,(trans-data-property-owl (first properties)))
                                    (|owl:someValuesFrom| nil 
                                                          ,(trans-data-range-owl data-range))))))
           
           (d-all
            (let ((data-range (first (last concept)))
                  (properties (butlast (cdr concept))))

              (if (cdr properties)
                  `(|owl:Restriction| nil
                                      (|owl:onProperties| ((|rdf:parseType| |Collection|))
                                                          ,@(mapcar #'trans-data-property-owl properties))
                                      (|owl:allValuesFrom| nil 
                                                           ,(trans-data-range-owl data-range)))
                `(|owl:Restriction| nil
                                    (|owl:onProperty| nil 
                                                      ,(trans-data-property-owl (first properties)))
                                    (|owl:allValuesFrom| nil 
                                                         ,(trans-data-range-owl data-range))))))
           
           (d-at-least 
            (if (or (not (fourth concept))
                    (member (fourth concept) (list 'top '*top* owlapi:+owlapi-owl-top+))) ;; ?? CD TOP? 
                `(|owl:Restriction| nil
                                    (|owl:onProperty| nil 
                                                      ,(trans-data-property-owl (third concept)))
                                    (|owl:minCardinality| ((|rdf:datatype| |xsd:nonNegativeInteger|))
                                                          ,(second concept)))
              `(|owl:Restriction| nil
                                  (|owl:onProperty| nil 
                                                    ,(trans-data-property-owl (third concept)))
                                  (|owl:minQualifiedCardinality| ((|rdf:datatype| |xsd:nonNegativeInteger|))
                                                                 ,(second concept))
                                  (|owl:onData-Range| nil 
                                                 ,(trans-data-range-owl (fourth concept))))))
           
           (d-at-most 
            (if (or (not (fourth concept))
                    (member (fourth concept) (list 'top '*top* owlapi:+owlapi-owl-top+))) ;; ?? CD TOP? 
                `(|owl:Restriction| nil
                                    (|owl:onProperty| nil 
                                                      ,(trans-data-property-owl (third concept)))
                                    (|owl:maxCardinality| ((|rdf:datatype| |xsd:nonNegativeInteger|))
                                                          ,(second concept)))
              `(|owl:Restriction| nil
                                  (|owl:onProperty| nil 
                                                    ,(trans-data-property-owl (third concept)))
                                  (|owl:maxQualifiedCardinality| ((|rdf:datatype| |xsd:nonNegativeInteger|))
                                                                 ,(second concept))
                                  (|owl:onData-Range| nil 
                                                 ,(trans-data-range-owl (fourth concept))))))
           
           (d-exactly
            (if (or (not (fourth concept))
                    (member (fourth concept) (list 'top '*top* owlapi:+owlapi-owl-top+))) ;; ?? CD TOP? 
                `(|owl:Restriction| nil
                                    (|owl:onProperty| nil 
                                                      ,(trans-data-property-owl (third concept)))
                                    (|owl:cardinality| ((|rdf:datatype| |xsd:nonNegativeInteger|))
                                                       ,(second concept)))
              `(|owl:Restriction| nil
                                  (|owl:onProperty| nil 
                                                    ,(trans-data-property-owl (third concept)))
                                  (|owl:qualifiedCardinality| ((|rdf:datatype| |xsd:nonNegativeInteger|))
                                                              ,(second concept))
                                  (|owl:onData-Range| nil 
                                                      ,(trans-data-range-owl (fourth concept))))))
           
           (d-filler
            `(|owl:Restriction| nil
                                (|owl:onProperty| nil 
                                                  ,(trans-data-property-owl (second concept)))
                                (|owl:hasValue| nil 
                                                ,(trans-literal-owl (third concept)))))))))

;;;
;;;
;;; 

(defmethod render ((stream stream) (axiom |OWLAxiom|) (syntax (eql :owl-rdf)))
  (owlapi-warning "Warning - cannot render axioms of type ~A in syntax ~A"
                (type-of axiom) 
                syntax)
  nil)

;;;
;;;
;;; 

(defmethod render ((stream stream) (axiom |OWLOntologyVersionDeclarationAxiom|) (syntax (eql :owl-rdf)))
  `((|owl:versionIRI| ((|rdf:resource| ,(iri-owl (owlapi:uri axiom)))))))

(defmethod render ((stream stream) (axiom |OWLImportsDeclarationAxiom|) (syntax (eql :owl-rdf)))
  `((|owl:imports| ((|rdf:resource| ,(iri-owl (owlapi:uri axiom)))))))

;;;
;;;
;;;

(defun annotation-to-xml-list (annotation)
  `((,(second annotation) nil 
     ,(trans-literal-owl (third annotation)))))

(defun annotation-to-axiom-annotation (annotation source property target &optional target-is-id-p)
  (if (not target-is-id-p)

      `(|owl:Axiom| nil
                    (|owl:annotatedSource|   nil ,source)
                    (|owl:annotatedProperty| nil ,property)
                    (|owl:annotatedTarget|   nil ,target)
                    (,(second annotation) 
                     nil 
                     ,(trans-literal-owl (third annotation))))

    `(|owl:Axiom| nil
                  (|owl:annotatedSource|   nil ,source)
                  (|owl:annotatedProperty| nil ,property)
                  (|owl:annotatedTarget|   ((|rdf:resource| ,target)))
                  (,(second annotation) 
                   nil 
                   ,(trans-literal-owl (third annotation))))))

;;;
;;;
;;;    

(defmethod get-axiom-comment-closure ((axiom |OWLAxiom|) (stream stream))
  (lambda ()
    (when *comments*
                                                      
      (indent stream)
      
      (xml-comment 
       stream 
       (format nil "Axiom ID ~A"
               (owlapi:axiom-id axiom)))

      (indent stream)
      
      (xml-block-comment stream (cons (type-of axiom) 
                                      (butlast (cdr (owlapi:told axiom)))))
                                                      
      (indent stream))))


(defmethod render :around ((stream stream) (axiom |OWLAxiom|) (syntax (eql :owl-rdf)))
  (let ((res (call-next-method)))

    ;;; (pprint res)

    (funcall (get-axiom-comment-closure axiom stream))

    (when (listp res) 
      (incf *axioms-rendered*)
      (dolist (res res)
        (handler-case
            (let ((res
                   (with-output-to-string (stream)
                     (print-xml res stream))))
              (write-string res stream))
          (error (error) 
            (error error)
            (owlapi-warning "Skipping bad axiom ID ~A : ~A" 
                            (owlapi:axiom-id axiom)
                            (cons (type-of axiom) 
                                  (butlast (cdr (owlapi:told axiom)))))))))))

;;;
;;;
;;; 

(defmethod render-axiom-annotation-axioms ((axiom |OWLAxiom|) stream cont)
  (dolist (annotation-axiom (owlapi:get-annotation-axioms-for-axiom axiom))

    (when *comments*

      (indent stream)
      
      (xml-comment stream (format nil "Axiom ID ~A"
                                  (owlapi:axiom-id annotation-axiom)))
      (indent stream)
      
      (xml-block-comment stream (cons (type-of annotation-axiom) 
                                      (butlast (cdr (owlapi:told annotation-axiom)))))

      (indent stream))

    (print-xml (funcall cont annotation-axiom)
               stream)))

(defun about-attribute-from (rdf-list)
  (or (second (assoc '|rdf:about| (second rdf-list)))
      rdf-list))

;;;
;;;
;;;

(defmethod render ((stream stream) (axiom |OWLSubClassAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-concept-owl (first (owlapi:descriptions axiom)))
      '|rdfs:subClassOf|
      (trans-concept-owl (second (owlapi:descriptions axiom))))))

  (list 
   (append 
    (trans-concept-owl (first (owlapi:descriptions axiom)))
    `((|rdfs:subClassOf| nil ,(trans-concept-owl (second (owlapi:descriptions axiom))))))))

(defmethod render ((stream stream) (axiom |OWLEquivalentClassesAxiom|) (syntax (eql :owl-rdf)))
  (mapcar #'(lambda (first second)
              
              (render-axiom-annotation-axioms
               axiom stream 
               (lambda (annotation-axiom)
                 (annotation-to-axiom-annotation
                  (owlapi:annotation annotation-axiom)
                  (trans-concept-owl first)
                  '|owl:equivalentClass|
                   (trans-concept-owl second))))

              (append (trans-concept-owl first)
                      `((|owl:equivalentClass| nil ,(trans-concept-owl second)))))
          
          (owlapi:descriptions axiom)
          (rest (owlapi:descriptions axiom))))

(defmethod render ((stream stream) (axiom |OWLDisjointClassesAxiom|) (syntax (eql :owl-rdf)))
  (if (cddr (owlapi:descriptions axiom))
      `((|owl:AllDisjointClasses| nil
                                  (|owl:members| ((|rdf:parseType| |Collection|))
                                                 ,@(mapcar #'(lambda (x)
                                                               (trans-concept-owl x))
                                                           (owlapi:descriptions axiom)))

                                  ,@(mapcan #'(lambda (annotation-axiom)
                                                (list 
                                                 (get-axiom-comment-closure annotation-axiom stream)
                                                 (first (annotation-to-xml-list 
                                                         (owlapi:annotation annotation-axiom)))))

                                            (owlapi:get-annotation-axioms-for-axiom axiom))))

    (progn 

      (render-axiom-annotation-axioms 
       axiom stream 
       (lambda (annotation-axiom)
         (annotation-to-axiom-annotation
          (owlapi:annotation annotation-axiom)
          (trans-concept-owl (first (owlapi:descriptions axiom)))
          '|owl:disjointWith|
          (trans-concept-owl (second (owlapi:descriptions axiom))))))

      (list 
       (append 
        (trans-concept-owl (first (owlapi:descriptions axiom)))
        `((|owl:disjointWith| nil ,(trans-concept-owl (second (owlapi:descriptions axiom))))))))))

(defun get-blank-node-id ()
  (incf *blank-node-id*))

(defmethod render ((stream stream) (axiom |OWLDisjointUnionAxiom|) (syntax (eql :owl-rdf)))
  (let ((node-id (get-blank-node-id)))

    (render-axiom-annotation-axioms 
     axiom stream 
     (lambda (annotation-axiom)
       (annotation-to-axiom-annotation
        (owlapi:annotation annotation-axiom)
        (trans-concept-owl (first (owlapi:descriptions axiom)))
        '|owl:disjointUnionOf|
        node-id
        t)))

    (list
     (append (trans-concept-owl (first (owlapi:descriptions axiom)))
             `((|owl:disjointUnionOf| nil
                                      ,(rdf-list2 (mapcar #'trans-concept-owl 
                                                           (rest (owlapi:descriptions axiom)))
                                                   node-id)))))))

;;;
;;;
;;;

(defmethod render ((stream stream) (axiom |OWLObjectSubPropertyAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-object-property-owl (owlapi:sub-object-property axiom))
      '|rdfs:subPropertyOf|
      (trans-object-property-owl (owlapi:object-property axiom)))))

  (list 
   (append 
    (trans-object-property-owl (owlapi:sub-object-property axiom))
    `((|rdfs:subPropertyOf| nil ,(trans-object-property-owl (owlapi:object-property axiom)))))))


(defmethod render ((stream stream) (axiom |OWLObjectPropertyChainSubPropertyAxiom|) (syntax (eql :owl-rdf)))
  (let ((node-id (get-blank-node-id)))

    (render-axiom-annotation-axioms 
     axiom stream 
     (lambda (annotation-axiom)
       (annotation-to-axiom-annotation
        (owlapi:annotation annotation-axiom)
        (trans-object-property-owl (owlapi:object-property axiom))
        '|owl:propertyChainAxiom|
        node-id
        t)))

    (list 
     (append 
      (trans-object-property-owl (owlapi:object-property axiom))
      `((|owl:propertyChainAxiom| nil
                                   ,(rdf-list2 (mapcar #'trans-object-property-owl 
                                                        (owlapi:object-property-chain axiom))
                                                node-id)))))))


(defmethod render ((stream stream) (axiom |OWLEquivalentObjectPropertiesAxiom|) (syntax (eql :owl-rdf)))
  (mapcar #'(lambda (first second)
              
              (render-axiom-annotation-axioms
               axiom stream 
               (lambda (annotation-axiom)
                 (annotation-to-axiom-annotation
                  (owlapi:annotation annotation-axiom)
                  (trans-object-property-owl first)
                  '|owl:equivalentProperty|
                  (trans-object-property-owl second))))
              
              (append (trans-object-property-owl first)
                      `((|owl:equivalentProperty| nil ,(trans-object-property-owl second)))))
          
          (owlapi:object-properties axiom)
          (rest (owlapi:object-properties axiom))))
  

(defmethod render ((stream stream) (axiom |OWLDisjointObjectPropertiesAxiom|) (syntax (eql :owl-rdf)))
  (if (cddr (owlapi:object-properties axiom))

      `((|owl:AllDisjointProperties| 
         nil
         (|owl:members| nil
                        ,(rdf-list2 
                          (mapcar #'(lambda (x)
                                      (trans-object-property-owl x))
                                  (owlapi:object-properties axiom))))
         
         ,@(mapcan #'(lambda (annotation-axiom)
                       (list 
                        (get-axiom-comment-closure annotation-axiom stream)
                        (first (annotation-to-xml-list (owlapi:annotation annotation-axiom)))))

                   (owlapi:get-annotation-axioms-for-axiom axiom))))

    (progn 

      (render-axiom-annotation-axioms 
       axiom stream 
       (lambda (annotation-axiom)
         (annotation-to-axiom-annotation
          (owlapi:annotation annotation-axiom)
          (trans-object-property-owl (first (owlapi:object-properties axiom)))
          '|owl:propertyDisjointWith|
          (trans-object-property-owl (second (owlapi:object-properties axiom))))))
      
      (list 
       (append 
        (trans-object-property-owl (first (owlapi:object-properties axiom)))
        `((|owl:propertyDisjointWith| nil ,(trans-object-property-owl
                                            (second
                                             (owlapi:object-properties axiom))))))))))

(defmethod render ((stream stream) (axiom |OWLObjectPropertyDomainAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-object-property-owl (owlapi:object-property axiom))
      '|rdfs:domain|
      (trans-concept-owl (owlapi:object-property-domain axiom)))))

  (list 
   (append 
    (trans-object-property-owl (owlapi:object-property axiom))
    `((|rdfs:domain| nil ,(trans-concept-owl (owlapi:object-property-domain axiom)))))))

(defmethod render ((stream stream) (axiom |OWLObjectPropertyRangeAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
       (trans-object-property-owl (owlapi:object-property axiom))
      '|rdfs:range|
      (trans-concept-owl (owlapi:object-property-range axiom)))))

  (list 
   (append 
    (trans-object-property-owl (owlapi:object-property axiom))
    `((|rdfs:range| nil ,(trans-concept-owl (owlapi:object-property-range axiom)))))))

(defmethod render ((stream stream) (axiom |OWLInverseObjectPropertiesAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-object-property-owl (first (owlapi:object-properties axiom)))
      '|owl:inverseOf|
      (trans-object-property-owl (second (owlapi:object-properties axiom))))))

  (list 
   (append 
    (trans-object-property-owl (first (owlapi:object-properties axiom)))
    `((|owl:inverseOf| nil ,(trans-object-property-owl (second (owlapi:object-properties axiom))))))))

(defmethod render ((stream stream) (axiom |OWLFunctionalObjectPropertyAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-object-property-owl (owlapi:object-property axiom))
      '|rdf:type|
      '|owl:FunctionalProperty|)))

  `((|owl:FunctionalProperty| nil ,(about-attribute-from
                                    (trans-object-property-owl (owlapi:object-property axiom))))))

(defmethod render ((stream stream) (axiom |OWLInverseFunctionalObjectPropertyAxiom|) (syntax (eql :owl-rdf)))

 (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-object-property-owl (owlapi:object-property axiom))
      '|rdf:type|
      '|owl:InverseFunctionalProperty|)))

 `((|owl:InverseFunctionalProperty| nil 
                                    ,(about-attribute-from
                                      (trans-object-property-owl
                                       (owlapi:object-property axiom))))))

(defmethod render ((stream stream) (axiom |OWLReflexiveObjectPropertyAxiom|) (syntax (eql :owl-rdf)))

 (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-object-property-owl (owlapi:object-property axiom))
      '|rdf:type|
      '|owl:ReflexiveProperty|)))

 `((|owl:ReflexiveProperty| nil ,(about-attribute-from
                                  (trans-object-property-owl
                                   (owlapi:object-property axiom))))))

(defmethod render ((stream stream) (axiom |OWLIrreflexiveObjectPropertyAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-object-property-owl (owlapi:object-property axiom))
      '|rdf:type|
      '|owl:IrreflexiveProperty|)))

  `((|owl:IrreflexiveProperty| nil ,(about-attribute-from
                                     (trans-object-property-owl
                                      (owlapi:object-property axiom))))))

(defmethod render ((stream stream) (axiom |OWLSymmetricObjectPropertyAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-object-property-owl (owlapi:object-property axiom))
      '|rdf:type|
      '|owl:SymmetricProperty|)))

  `((|owl:SymmetricProperty| nil ,(about-attribute-from 
                                   (trans-object-property-owl
                                    (owlapi:object-property axiom))))))

(defmethod render ((stream stream) (axiom |OWLAsymmetricObjectPropertyAxiom|) (syntax (eql :owl-rdf)))
 
  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-object-property-owl (owlapi:object-property axiom))
      '|rdf:type|
      '|owl:AsymmetricProperty|)))

  `((|owl:AsymmetricProperty| nil ,(about-attribute-from
                                    (trans-object-property-owl
                                     (owlapi:object-property axiom))))))

(defmethod render ((stream stream) (axiom |OWLTransitiveObjectPropertyAxiom|) (syntax (eql :owl-rdf)))

 (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-object-property-owl (owlapi:object-property axiom))
      '|rdf:type|
      '|owl:TransitiveProperty|)))

 `((|owl:TransitiveProperty| nil ,(about-attribute-from
                                   (trans-object-property-owl
                                    (owlapi:object-property axiom))))))

;;;
;;;
;;;

(defmethod render ((stream stream) (axiom |OWLDataSubPropertyAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-data-property-owl (owlapi:sub-data-property axiom))
      '|rdfs:subPropertyOf|
      (trans-data-property-owl (owlapi:data-property axiom)))))

  (list 
   (append 
    (trans-data-property-owl (owlapi:sub-data-property axiom))
    `((|rdfs:subPropertyOf| nil ,(trans-data-property-owl (owlapi:data-property axiom)))))))

(defmethod render ((stream stream) (axiom |OWLEquivalentDataPropertiesAxiom|) (syntax (eql :owl-rdf)))
  (mapcar #'(lambda (first second)
                
              (render-axiom-annotation-axioms
               axiom stream 
               (lambda (annotation-axiom)
                 (annotation-to-axiom-annotation
                  (owlapi:annotation annotation-axiom)
                  (trans-data-property-owl first)
                  '|owl:equivalentProperty|
                  (trans-data-property-owl second))))

              (append (trans-data-property-owl first)
                      `((|owl:equivalentProperty| nil ,(trans-data-property-owl second)))))
            
          (owlapi:data-properties axiom)
          (rest (owlapi:data-properties axiom))))

(defmethod render ((stream stream) (axiom |OWLDisjointDataPropertiesAxiom|) (syntax (eql :owl-rdf)))
  (if (cddr (owlapi:data-properties axiom))
      `((|owl:AllDisjointProperties| 
         nil
         (|owl:members| nil
                        ,(rdf-list2 
                          (mapcar #'(lambda (x)
                                      (trans-data-property-owl x))
                                  (owlapi:data-properties axiom))))
                                       
         ,@(mapcan #'(lambda (annotation-axiom)
                       (list 
                        (get-axiom-comment-closure annotation-axiom stream)
                        (first (annotation-to-xml-list (owlapi:annotation annotation-axiom)))))
                   
                   (owlapi:get-annotation-axioms-for-axiom axiom))))
    (progn 
     
      (render-axiom-annotation-axioms 
       axiom stream 
       (lambda (annotation-axiom)
         (annotation-to-axiom-annotation
          (owlapi:annotation annotation-axiom)
          (trans-data-property-owl (first (owlapi:data-properties axiom)))
          '|owl:propertyDisjointWith|
          (trans-data-property-owl (second (owlapi:data-properties axiom))))))

      (list
       (append 
        (trans-data-property-owl (first (owlapi:data-properties axiom)))
        `((|owl:propertyDisjointWith| nil 
                                      ,(trans-data-property-owl
                                        (second (owlapi:data-properties axiom))))))))))

(defmethod render ((stream stream) (axiom |OWLDataPropertyDomainAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-data-property-owl (owlapi:data-property axiom))
      '|rdfs:domain|
      (trans-concept-owl (owlapi:data-property-domain axiom)))))

  (list 
   (append 
    (trans-data-property-owl (owlapi:data-property axiom))
    `((|rdfs:domain| nil ,(trans-concept-owl (owlapi:data-property-domain axiom)))))))

(defmethod render ((stream stream) (axiom |OWLDataPropertyRangeAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-data-property-owl (owlapi:data-property axiom))
      '|rdfs:range|
      (trans-data-range-owl (owlapi:data-property-range axiom)))))

  (list 
   (append 
    (trans-data-property-owl (owlapi:data-property axiom))
    `((|rdfs:range| nil ,(trans-data-range-owl (owlapi:data-property-range axiom)))))))

(defmethod render ((stream stream) (axiom |OWLFunctionalDataPropertyAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-data-property-owl (owlapi:data-property axiom))
      '|rdf:type|
      '|owl:FunctionalProperty|)))

  `((|owl:FunctionalProperty| nil ,(about-attribute-from
                                    (trans-data-property-owl
                                     (owlapi:data-property axiom))))))

(defmethod render ((stream stream) (axiom |OWLDatatypeDefinitionAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (about-attribute-from (trans-datatype-owl (owlapi:datatype-name axiom)))
      '|rdf:type|
      '|rdfs:Datatype|)))

  (list 
   (append 
    (trans-datatype-owl (owlapi:datatype-name axiom))
    `((|owl:equivalentClass| nil ,(trans-data-range-owl (owlapi:data-range axiom)))))))

;;;
;;;
;;;

(defmethod render ((stream stream) (axiom |OWLHasKeyAxiom|) (syntax (eql :owl-rdf)))

  (let ((node-id
         (format nil "#~A" (get-blank-node-id))))

    (render-axiom-annotation-axioms 
     axiom stream 
     (lambda (annotation-axiom)
       (annotation-to-axiom-annotation
        (owlapi:annotation annotation-axiom)
        (trans-concept-owl (owlapi:key-class axiom))
        '|owl:hasKey|
        node-id
        t)))

    (list
     (append (trans-concept-owl (owlapi:key-class axiom))
             `((|owl:hasKey| nil
                             ,(rdf-list2 (append 
                                          (mapcar #'trans-object-property-owl 
                                                  (owlapi:key-object-properties axiom))
                                          (mapcar #'trans-data-property-owl
                                                  (owlapi:key-data-properties axiom)))
                                         node-id)))))))

;;;
;;;
;;;

(defun simplify-role (role) 
  (labels ((do-it (role inverted-p)
             (if (symbolp role)
                 (if inverted-p 
                     `(inv ,role)
                   role)
               (if (and (consp role) 
                        (eq (first role) 'inv))
                   (do-it (second role) (not inverted-p))
                 (owlapi-runtime-error "Bad role: ~A" role)))))

    (do-it role nil)))

(defmethod render ((stream stream) (axiom |OWLClassAssertionAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-ind-owl (owlapi:ax-individual axiom))
      '|rdf:type|      
      (trans-concept-owl (owlapi:description axiom)))))

  `((|rdf:Description| ((|rdf:about| ,(trans-ind-owl (owlapi:ax-individual axiom))))
                       (|rdf:type| nil ,(trans-concept-owl (owlapi:description axiom))))))

(defmethod render ((stream stream) (axiom |OWLObjectPropertyAssertionAxiom|) (syntax (eql :owl-rdf)))

  (let ((rel (simplify-role (owlapi:rel-object-property axiom))))
    (if (consp rel) ; lt. Spec! 
        (progn 
          
          (render-axiom-annotation-axioms 
           axiom stream 
           (lambda (annotation-axiom)
             (annotation-to-axiom-annotation
              (owlapi:annotation annotation-axiom)
              (trans-ind-owl (owlapi:object axiom))
              (second rel)
              (trans-ind-owl (owlapi:subject axiom)))))

          `((|rdf:Description| ((|rdf:about| ,(trans-ind-owl (owlapi:object axiom))))
                               (,(second rel) nil ,(trans-ind-owl (owlapi:subject axiom))))))
      
      (progn 

        (render-axiom-annotation-axioms 
         axiom stream 
         (lambda (annotation-axiom)
           (annotation-to-axiom-annotation
            (owlapi:annotation annotation-axiom)
            (trans-ind-owl (owlapi:subject axiom))
            rel
            (trans-ind-owl (owlapi:object axiom)))))
        
        `((|rdf:Description| ((|rdf:about| ,(trans-ind-owl (owlapi:subject axiom))))
                             (,rel nil ,(trans-ind-owl (owlapi:object axiom)))))))))

(defmethod render ((stream stream) (axiom |OWLNegativeObjectPropertyAssertionAxiom|) (syntax (eql :owl-rdf)))
  `((|owl:NegativePropertyAssertion| nil
                                     (|owl:sourceIndividual| nil ,(trans-ind-owl (owlapi:subject axiom)))
                                     (|owl:assertionProperty| nil 
                                                              ,(trans-object-property-owl 
                                                                (owlapi:rel-object-property axiom)))
                                     (|owl:targetIndividual| nil ,(trans-ind-owl (owlapi:object axiom)))
                                     
                                     ,@(mapcan #'(lambda (annotation-axiom)
                                                   (list 
                                                    (get-axiom-comment-closure annotation-axiom stream)
                                                    (first (annotation-to-xml-list 
                                                            (owlapi:annotation annotation-axiom)))))

                                               (owlapi:get-annotation-axioms-for-axiom axiom)))))
  
(defmethod render ((stream stream) (axiom |OWLSameIndividualsAxiom|) (syntax (eql :owl-rdf)))
  (mapcar #'(lambda (first second)

              (render-axiom-annotation-axioms
               axiom stream 
               (lambda (annotation-axiom)
                 (annotation-to-axiom-annotation
                  (owlapi:annotation annotation-axiom)
                  (trans-ind-owl first)
                  '|owl:sameAs|
                  (trans-ind-owl second))))
              
              (append (trans-ind-owl first t)
                      `((|owl:sameAs| nil ,(trans-ind-owl second t)))))

          (owlapi:individuals axiom)
          (rest (owlapi:individuals axiom))))

(defmethod render ((stream stream) (axiom |OWLDifferentIndividualsAxiom|) (syntax (eql :owl-rdf)))
  (if (cddr (owlapi:individuals axiom))
      `((|owl:AllDifferent| nil
                            (|owl:members| ((|rdf:parseType| |Collection|))
                                           ,@(mapcar #'(lambda (x)
                                                         (trans-ind-owl x t))
                                                     (owlapi:individuals axiom)))
                            ,@(mapcan #'(lambda (annotation-axiom)
                                          (list 
                                           (get-axiom-comment-closure annotation-axiom stream)
                                           (first (annotation-to-xml-list
                                                   (owlapi:annotation annotation-axiom)))))

                                      (owlapi:get-annotation-axioms-for-axiom axiom))))

    (progn 

       (render-axiom-annotation-axioms 
        axiom stream 
        (lambda (annotation-axiom)
          (annotation-to-axiom-annotation
           (owlapi:annotation annotation-axiom)
           (trans-ind-owl (first (owlapi:individuals axiom)))
           '|owl:differentFrom| 
           (trans-ind-owl (second (owlapi:individuals axiom))))))

       `((|rdf:Description| ((|rdf:about| ,(trans-ind-owl (first (owlapi:individuals axiom)))))
                            (|owl:differentFrom| nil 
                                                 ,(trans-ind-owl (second (owlapi:individuals axiom)) t)))))))

;;;
;;;
;;;   

(defmethod render ((stream stream) (axiom |OWLDataPropertyAssertionAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
         axiom stream 
         (lambda (annotation-axiom)
           (annotation-to-axiom-annotation
            (owlapi:annotation annotation-axiom)
            (trans-ind-owl (owlapi:subject axiom))
            (owlapi:rel-data-property axiom)
            (trans-literal-owl (owlapi:data-literal axiom)))))

  `((|rdf:Description| ((|rdf:about| ,(trans-ind-owl (owlapi:subject axiom))))
                       (,(owlapi:rel-data-property axiom) 
                        nil ,(trans-literal-owl (owlapi:data-literal axiom))))))

(defmethod render ((stream stream) (axiom |OWLNegativeDataPropertyAssertionAxiom|) (syntax (eql :owl-rdf)))
  `((|owl:NegativePropertyAssertion| nil
                                     (|owl:sourceIndividual| nil ,(trans-ind-owl (owlapi:subject axiom)))
                                     (|owl:assertionProperty| nil 
                                                              ,(trans-data-property-owl 
                                                                (owlapi:rel-data-property axiom)))
                                     (|owl:targetValue| nil ,(trans-literal-owl (owlapi:data-literal axiom)))

                                      ,@(mapcan #'(lambda (annotation-axiom)
                                                   (list 
                                                    (get-axiom-comment-closure annotation-axiom stream)
                                                    (first (annotation-to-xml-list 
                                                            (owlapi:annotation annotation-axiom)))))

                                               (owlapi:get-annotation-axioms-for-axiom axiom)))))

;;;
;;;
;;;

(defmethod render ((stream stream) (axiom |OWLDeclarationAxiom|) (syntax (eql :owl-rdf)))
  (mapcar #'(lambda (entity)
              (let ((owl-entity
                     (ecase (first entity)
                       ((|OWLClass| |Class|) 
                        '|owl:Class|)

                       (|ObjectProperty|
                        '|owl:ObjectProperty|)

                       (|DataProperty| 
                        '|owl:DataProperty|)

                       (|Datatype| 
                        '|rdfs:Datatype|)
       
                       (|AnnotationProperty| 
                        '|owl:AnnotationProperty|)

                       ((|Individual| |NamedIndividual|)
                        '|owl:NamedIndividual|))))

                (render-axiom-annotation-axioms 
                 axiom stream 
                 (lambda (annotation-axiom)
                   (annotation-to-axiom-annotation
                    (owlapi:annotation annotation-axiom)
                    (second entity)
                    '|rdf:type|
                    owl-entity)))

                (list owl-entity nil (second entity))))

          (if (consp (first (owlapi:entity axiom)))
              (owlapi:entity axiom)

            (list (owlapi:entity axiom)))))

(defmethod render ((stream stream) (axiom |OWLImplicitDeclarationAxiom|) (syntax (eql :owl-rdf)))
  ;;; NICHT rendern!
  t) 

;;;
;;;
;;;

(defmethod render ((stream stream) (axiom |OWLAxiomAnnotationAxiom|) (syntax (eql :owl-rdf)))
  t)

;;;
;;; Old EntityAnnotationAxiom
;;; 

#|

(defmethod render ((stream stream) (axiom |OWLEntityAnnotationAxiom|) (syntax (eql :owl-rdf)))
  '(tag-printer stream |EntityAnnotation|
    (render-axiom-annotations stream axiom syntax)
    (func-printer stream (entity axiom))
    (func-printer stream 
                  `(,(case (intern (ensure-string (second (annotation axiom))))
                       ;; *implicit-annotation-properties*
                       ((|http://www.w3.org/2000/01/rdf-schema#comment| 
                         |http://www.w3.org/2000/01/rdf-schema#Comment| 
                         !rdfs:comment) 
                        '|Comment|)
                       ((|http://www.w3.org/2000/01/rdf-schema#label| 
                         |http://www.w3.org/2000/01/rdf-schema#Label| 
                         !rdfs:label) 
                        '|Label|)
                       ((|http://www.w3.org/2000/01/rdf-schema#seeAlso| 
                         |http://www.w3.org/2000/01/rdf-schema#SeeAlso| 
                         !rdfs:seeAlso)
                        '|seeAlso|)
                       ((|http://www.w3.org/2002/07/owl#isDefinedBy|
                         |http://www.w3.org/2002/07/owl#IsDefinedBy|
                         !owl:isDefinedBy)
                        '|isDefinedBy|)
                       ((|http://www.w3.org/2002/07/owl#versionInfo|
                         |http://www.w3.org/2002/07/owl#VersionInfo|
                         !owl:versionInfo)
                        '|priorVersion|)
                       (otherwise
                        (if *p4-mode*
                            '|Comment|
                          (intern (ensure-string (second (annotation axiom)))))))
                    ,(format nil "~S" (second (third (annotation axiom)))))))

  t)

|#

;;;
;;;
;;; 

(defmethod render ((stream stream) (axiom |OWLOntologyAnnotationAxiom|) (syntax (eql :owl-rdf)))
  (annotation-to-xml-list (owlapi:annotation axiom)))

(defmethod render ((stream stream) (axiom |OWLAnnotationAssertionAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-ind-owl (owlapi:annotation-subject axiom))
      (owlapi:annotation-property axiom)
      (trans-literal-owl (owlapi:annotation-value axiom)))))
       
  `((|rdf:Description| ((|rdf:about| ,(trans-ind-owl (owlapi:annotation-subject axiom))))
                       (,(owlapi:annotation-property axiom) 
                        nil 
                        ,(trans-literal-owl (owlapi:annotation-value axiom))))))

(defmethod render ((stream stream) (axiom |OWLAnnotationPropertyRangeAxiom|) (syntax (eql :owl-rdf)))

 (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-annotation-property-owl (owlapi:annotation-property2 axiom))
      '|rdfs:range|
      (trans-data-range-owl (owlapi:annotation-property-range axiom)))))

  (list 
   (append 
    (trans-annotation-property-owl (owlapi:annotation-property2 axiom))
    `((|rdfs:range| nil ,(trans-data-range-owl (owlapi:annotation-property-range axiom)))))))

(defmethod render ((stream stream) (axiom |OWLAnnotationPropertyDomainAxiom|) (syntax (eql :owl-rdf)))

 (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-annotation-property-owl (owlapi:annotation-property1 axiom))
      '|rdfs:domain|
      (trans-concept-owl (owlapi:annotation-property-domain axiom)))))

  (list 
   (append 
    (trans-annotation-property-owl (owlapi:annotation-property1 axiom))
    `((|rdfs:domain| nil ,(trans-concept-owl (owlapi:annotation-property-domain axiom)))))))

(defmethod render ((stream stream) (axiom |OWLSubAnnotationPropertyOfAxiom|) (syntax (eql :owl-rdf)))

  (render-axiom-annotation-axioms 
   axiom stream 
   (lambda (annotation-axiom)
     (annotation-to-axiom-annotation
      (owlapi:annotation annotation-axiom)
      (trans-object-property-owl (owlapi:annotation-sub-property axiom))
      '|rdfs:subPropertyOf|
      (trans-annotation-property-owl (owlapi:annotation-super-property axiom)))))

  (list 
   (append 
    (trans-annotation-property-owl (owlapi:annotation-sub-property axiom))
    `((|rdfs:subPropertyOf| nil ,(trans-annotation-property-owl
                                  (owlapi:annotation-super-property axiom)))))))

;;;
;;; OWL RDF  
;;;

(defmethod render ((stream stream) (ont owlapi:ontology) (syntax (eql :owl-rdf)))
  (let* ((*ontology* ont)
         (*axioms-rendered* 0)
         (*blank-node-id* 0)
         (all-axioms nil)
         (*dont-render-attributes* '(|INT-abbreviatesIRIs| |INT-kb| |INT-prefixes|))
         (*abbreviate-iris* nil))

    (with-prefixes (ont prefixes default)

      (let* ((default (without-# default))
             (prefixes 
              (cons (list nil default) prefixes)))

        (with-slots (owlapi:axioms owlapi:name) ont
      
          (xml-header stream)
        
          (indent stream) 
        
          (xml-author-comment owlapi:name stream)
        
          (indent stream)

          (entity-definitions stream prefixes)

          (indent stream)
        
          (print-xml `(|rdf:RDF| 
                     
                       (("xmlns" ,(with-# default))
                        ("xml:base" ,(without-# default))
                        ,@(mapcar #'(lambda (x)
                                      (list (format nil "xmlns:~A" (first x))
                                            (second x)
                                            t))
                                  (remove-if #'owlapi:is-default-prefix-p 
                                             prefixes
                                             :key #'first)))

                       (|owl:Ontology|

                        ((|rdf:about| 
                          ,(make-url-from-filename owlapi:name)))

                        ,(lambda ()
                        
                           (let ((*add-prefixes* prefixes)
                                 (*abbreviate-iris* t))

                             (dolist (type '(|OWLOntologyVersionDeclarationAxiom|
                                             |OWLImportsDeclarationAxiom|
                                             |OWLOntologyAnnotationAxiom|))
                          
                               (dolist (axiom 
                                        (owlapi:get-axioms-of-type-for owlapi:axioms type))

                                 (push axiom all-axioms)

                                 (render stream axiom syntax))))))

                       ,(lambda ()
                          
                          (let ((*add-prefixes* prefixes)
                                (*abbreviate-iris* t))                          
                      
                            (dolist (type #|
  (not (or |OWLOntologyVersionDeclarationAxiom|
           |OWLImportsDeclarationAxiom|
           |OWLOntologyAnnotationAxiom|
           |OWLPrefixDeclarationAxiom|)) |# 
                                         
                                          '( |OWLDeclarationAxiom|
                                             |OWLDatatypeDefinitionAxiom|
                                             |OWLClassAxiom|
                                             |OWLPropertyAxiom|
                                             |OWLIndividualAxiom|
                                             |OWLHasKeyAxiom|
                                             (and |OWLAnnotationAxiom| 
                                                  (not |OWLOntologyAnnotationAxiom|)
                                                  (not |OWLAxiomAnnotationAxiom|))))
          
                              (dolist (axiom 
                                       (owlapi:get-axioms-of-type-for owlapi:axioms type))

                                (push axiom all-axioms)

                                (render stream axiom syntax))))))

                     stream)

          (indent stream)

          (xml-author-comment owlapi:name stream)        

          (let ((axioms (remove-if #'(lambda (x) 
                                       (or (typep x '|OWLAxiomAnnotationAxiom|)
                                           (typep x '|OWLOntologyAnnotationAxiom|)
                                           (typep x '|OWLPrefixDeclarationAxiom|)))
                                   owlapi:axioms)))
            (when (set-difference axioms all-axioms)
              (owlapi-warning "Upps! Forgot to render axioms of types ~A!"
                              (remove-duplicates (mapcar #'type-of (set-difference axioms all-axioms))))))

          *axioms-rendered*)))))
