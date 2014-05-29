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
  (enable-boolean-readers)
  (wilbur:enable-node-shorthand))

;;; ======================================================================

(defun convert-to-xsd-datatype (datatype)
  (if (eq datatype 't)
      (setf datatype 'string)
    (let ((transformed-type (transform-type datatype)))
      (setf datatype 
            (if (consp transformed-type)
                (first transformed-type)
              transformed-type))))
  (ecase datatype
    (string (concatenate 'string nox:-xsd-uri- "string"))
    (integer (concatenate 'string nox:-xsd-uri- "int"))
    (real (concatenate 'string nox:-xsd-uri- "float"))
    (cardinal (concatenate 'string nox:-xsd-uri- "nonNegativeInteger"))
    ((racer-boolan boolean) (concatenate 'string nox:-xsd-uri- "boolean"))))

(defun convert-to-owl2-facet-name (facet-name)
  (ecase (extract-face-name facet-name)
    (|maxExclusive| "owl2:maxExclusive")
    (|maxInclusive| "owl2:maxInclusive")
    (|minExclusive| "owl2:minExclusive")
    (|minInclusive| "owl2:minInclusive")
    (|minLength| "owl2:minLength")
    (|maxLength| "owl2:maxLength")
    (|length| "owl2:length")
    (|pattern| "owl2:pattern")
    (|totalDigits| "owl2:totalDigits")
    (|fractionDigits| "owl2:factionalDigits")))

(defun extract-face-name (prefixed-facet-name)
  (let* ((prefixed-facet-name-string (symbol-name prefixed-facet-name))
         (pos-sharp (position #\# prefixed-facet-name-string)))
    (if (null pos-sharp)
        prefixed-facet-name
      (intern (subseq prefixed-facet-name-string (1+ pos-sharp)) :racer))))


;;; ======================================================================

(defun format-as-datatype (stream datatype)
  (format stream "~A"
          (convert-to-xsd-datatype datatype)))

(defun check-constant (concept)
  (unless (or (subtypep (type-of (third concept)) 'integer)
              (subtypep (type-of (third concept)) 'ratio)
              (subtypep (type-of (third concept)) 'float)
              (subtypep (type-of (third concept)) 'string))
    (error "Concept term ~A cannot be converted." concept)))



;;; ======================================================================

(defun use-internal-inverse-role-name-p (inverse tbox)
  (and inverse
       (and (not (role-internal-conjunction-p inverse))
            (or (not (role-internal-name-p inverse))
                (dl-inverse-roles (tbox-language tbox))))))

(defun print-owl-tbox (tbox stream &key (uri nil) (as-part-of-kb nil)
                            (ontology-name nil)
                            class-hashtable property-hashtable ind-hashtable)
  (declare (ignore ontology-name))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))

  (let ((namespace (get-namespace-prefix tbox)))
    (unless (null namespace)
      (unless (null uri)
        (racer-warn "Ignoring :uri argument ~A." uri))
      (setf uri namespace)))

  (if (null uri) 
      (setf uri ""))
  (let ((pos-sharp (position #\# uri)))
    (when pos-sharp
      (setf uri (subseq uri 0 pos-sharp))))
      
  (let ((*current-tbox* tbox)
        (predefined-roles (mapcar #'(lambda (role) (owl-as-role role nil)) *implicit-annotation-properties*)))
    (unless as-part-of-kb
      (setf class-hashtable (make-hash-table))
      (setf property-hashtable (make-hash-table)))
    (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~
                    ~%<rdf:RDF~
                    ~%~V@Txmlns:owl=\"~A\"~
                    ~%~V@Txmlns:owl2=\"~A\"" 
            *indent*
            +owl-version+
            *indent*
            +owl2-version+)
    (format stream "~%~V@Txmlns:rdf=\"~A\"~
                    ~%~V@Txmlns:rdfs=\"~A\"~
                    ~%~V@Txmlns:xsd=\"~A\""
            *indent* nox:-rdf-uri-
            *indent* nox:-rdfs-uri-
            *indent* nox:-xsd-uri-)

    (loop for spec in (tbox-namespaces tbox) 
          unless (or (null (car spec))
                     (member (car spec) '("base" "rdf" "rdfs" "owl" "owl2" "xsd" "swrl" "swrlb")
                             :test #'string=))
          do
          (format stream "~%~V@Txmlns:~A=~S"
                  *indent*
                  (car spec)
                  (cdr spec)))

    (when t ;(and as-part-of-kb (abox-rules as-part-of-kb))
      (format stream "~%~V@Txmlns:swrl=\"~A\"~
                      ~%~V@Txmlns:swrlb=\"~A\""
              *indent* +swrl-version+
              *indent* +swrlb-version+))

    ;; What is the default namespace of the ontology?

    (when (and uri (not (string= uri "")))
      (format stream "~%~V@Txmlns=\"~A#\"" *indent* uri))
    (if (and uri (not (string= uri "")))
        (format stream "~%~V@Txml:base=\"~A\">" *indent* uri)
      (format stream ">"))
    (if (and as-part-of-kb (abox-rules as-part-of-kb))
      (progn
          (format stream "~%~V@T<owl:Ontology rdf:about=\"\">" *indent*)
          (format stream
                  "~%~V@T<owl:imports rdf:resource=\"~A\"/>"
                  (+ *indent* *indent*) +swrl-url+)
          (format stream
                  "~%~V@T<owl:imports rdf:resource=\"~A\"/>"
                  (+ *indent* *indent*) +swrlb-url+)
          (format stream "~%~V@T</owl:Ontology>" *indent*))
      (format stream "~%~V@T<owl:Ontology rdf:about=\"\"/>"
              *indent*))
    (loop with role-printed-table = (make-hash-table)
          for role being the hash-values in (tbox-role-store tbox) using (hash-key role-name)
          unless (or (is-predefined-role-p role)
                     (role-internal-name-p role) 
                     (role-internal-conjunction-p role)
                     (find role-name predefined-roles))
          do 
          (if (eq role-name (role-name role))
              (owl-print-role-declaration stream tbox role role-name
                                          (role-cd-attribute role)
                                          (role-feature-p role)
                                          (role-transitive-p role)
                                          (mapcar #'role-name 
                                                  (remove-if #'role-internal-name-p
                                                             (role-parents-internal role)))
                                          (let ((inverse (role-inverse-internal role)))
                                            (when (use-internal-inverse-role-name-p inverse tbox)
                                              (role-name inverse)))
                                          (role-domain-concept role)
                                          (role-range-concept role)
                                          (tbox-role-synonyms tbox)
                                          (role-datatype role)
                                          (role-inverse-feature-p role)
                                          (role-reflexive-p role)
                                          (role-irreflexive-p role)
                                          (role-asymmetric-p role)
                                          (role-symmetric-p role)
                                          (role-compositions role)
                                          *indent* uri
                                          class-hashtable 
                                          property-hashtable
                                          ind-hashtable
                                          role-printed-table)
            (format stream "~%~V@T<owl:ObjectProperty ~A/>"
                    *indent* (entity-ref role-name uri property-hashtable))))
    (loop for disjoint-set being the hash-value of (tbox-disjoint-set tbox) do
          (owl-print-disjoint-axiom stream disjoint-set *indent* uri 
                                    class-hashtable))
    (loop for (name definition primitive) in (reverse (tbox-original-concept-axioms tbox)) do
          (unless (and primitive (disjoint-and-p definition))
            (owl-print-class-axiom stream
                                   tbox 
                                   name
                                   definition
                                   primitive 
                                   *indent* uri
                                   class-hashtable 
                                   property-hashtable
                                   ind-hashtable)))
    (unless as-part-of-kb
      (format stream "~%</rdf:RDF>~%"))))


(defun get-ontology-name (tbox)
  (let ((ontologies (tbox-ontologies tbox)))
    (if (null ontologies)
        ""
      (ontology-name (first ontologies)))))

(defun find-property-name (property-name property-hashtable)
  (gethash property-name property-hashtable))

(defun (setf find-property-name) (new-value property-name property-hashtable)
  (if (null new-value)
      (remhash property-name property-hashtable)
    (setf (gethash property-name property-hashtable) new-value)))

(defun find-class-name (class-name class-hashtable)
  (gethash class-name class-hashtable))

(defun (setf find-class-name) (new-value class-name class-hashtable)
  (if (null new-value)
      (remhash class-name class-hashtable)
    (setf (gethash class-name class-hashtable) new-value)))

(defun entity-ref (name base occurrence-hashtable)
  (check-type name symbol)
  (multiple-value-bind (ref descriptor)
      (cond ((or (eq name 'top) (eq name '*top*))
             (values (if (null occurrence-hashtable)
                         "rdf:resource"
                       "rdf:about")
                     +owl-thing+))
            ((or (eq name 'bottom) (eq name '*bottom*))
             (values (if (null occurrence-hashtable)
                         "rdf:resource"
                       "rdf:about")
                     +owl-nothing+))
            (t 
             (let ((symbol-name (symbol-name name)))
               (if (or (null base) (string= base ""))
                   (values (if (null occurrence-hashtable)
                               "rdf:resource"
                             "rdf:about")
                           symbol-name)
                 (if (or (search "http:" symbol-name)
                         (search "file:" symbol-name))
                     (let ((pos-sharp (position #\# symbol-name :from-end t)))
                       (if pos-sharp
                           (if (string= (subseq symbol-name 0 pos-sharp) base)
                               (if (null occurrence-hashtable)
                                   (values "rdf:resource" (subseq symbol-name pos-sharp))
                                 (if (gethash name occurrence-hashtable)
                                     (values "rdf:about" (subseq symbol-name pos-sharp))
                                   (progn 
                                     (setf (gethash name occurrence-hashtable) t)
                                     (values "rdf:ID" (subseq symbol-name (1+ pos-sharp))))))
                             (if (null occurrence-hashtable)
                                 (values "rdf:resource" symbol-name)
                               (values "rdf:about" symbol-name)))
                         (if (null occurrence-hashtable)
                             (values "rdf:resource" symbol-name)
                           (values "rdf:about" symbol-name))))
                   (if (null occurrence-hashtable)
                       (values "rdf:resource" (ensure-sharp base symbol-name))
                     (if (gethash name occurrence-hashtable)
                         (values "rdf:about" (ensure-sharp base symbol-name))
                       (progn 
                         (setf (gethash name occurrence-hashtable) t)
                         (values "rdf:ID" symbol-name)))))))))
    (concatenate 'string ref "=\"" descriptor "\"")))

(defun ensure-sharp (base symbol-name)
  (if (null base)
      (concatenate 'string "#" symbol-name)
    (let ((pos-sharp (position #\# base :from-end t)))
      (if pos-sharp
          (concatenate 'string base symbol-name)
        (concatenate 'string "#" symbol-name)))))


(defun owl-print-role-declaration (stream tbox role role-name attribute feature-p transitive-p parents inverse
                                          domain range synonyms datatype 
                                          inverse-feature-p
                                          reflexive-p
                                          irreflexive-p
                                          asymmetric-p
                                          symmetric-p
                                          compositions
                                          indent uri
                                          class-hashtable property-hashtable ind-hashtable role-printed-table)

  (unless (internal-role-p role-name)
    (let ((inverse-printed-p
           (and inverse
                (not (role-internal-name-p (role-inverse-internal role)))
                (gethash (role-name (role-inverse-internal role)) role-printed-table))))
      (setf (gethash role-name role-printed-table) t)
      (format stream "~%~V@T<owl:~A ~A>"
              indent (if (or attribute datatype) "DatatypeProperty" "ObjectProperty")
              (entity-ref role-name uri property-hashtable))
      (loop for parent in parents do
            (unless (or (eq parent +top-object-role-symbol+)
                        (eq parent +top-datatype-role-symbol+))
              (format stream "~%~V@T<rdfs:subPropertyOf ~A/>"
                      (+ indent *indent*) (entity-ref parent uri nil))))
      (when (and inverse (not inverse-printed-p) (not (role-internal-name-p (role-inverse-internal role))))
        (format stream "~%~V@T<owl:inverseOf ~A/>"
                (+ indent *indent*) (entity-ref inverse uri nil)))
      (loop for (role-name-1 . role-name-2) in synonyms 
            when (eq role-name-1 role-name) do
            (format stream "~%~V@T<owl:equivalentProperty ~A/>"
                    (+ indent *indent*) (entity-ref role-name-2 uri nil)))
      (when (and domain (not (member domain '(top *top*))))
        (format stream "~%~V@T<rdfs:domain>" (+ indent *indent*))
        (format-as-owl-class stream tbox domain (+ indent *indent* *indent*) uri
                             class-hashtable property-hashtable ind-hashtable)
        (format stream "~%~V@T</rdfs:domain>" (+ indent *indent*)))
      (when (and range (not (eq range 't)) (not (member range '(top *top*))))
        (unless datatype
          (format stream "~%~V@T<rdfs:range>" (+ indent *indent*))
          (format-as-owl-class stream tbox range (+ indent *indent* *indent*) uri
                               class-hashtable property-hashtable ind-hashtable)
          (format stream "~%~V@T</rdfs:range>" (+ indent *indent*))))
      (cond ((and datatype (and (not (eq datatype 't)) (symbolp datatype)))
             (format stream "~%~V@T<rdfs:range rdf:resource=~S/>" (+ indent *indent*) (convert-to-xsd-datatype datatype)))
            ((and datatype (and (consp datatype) (eq (first datatype) 'd-base-type)))
             (format stream "~%~V@T<rdfs:range rdf:resource=~S/>" (+ indent *indent*) (symbol-name (second datatype))))
            ((and datatype (not (eq datatype 't)))
             (format stream "~%~V@T<rdfs:range>" (+ indent *indent*))
             (format-as-datarange stream datatype (+ indent *indent* *indent*))
             (format stream "~%~V@T</rdfs:range>" (+ indent *indent*))))
      (when attribute
        (format stream "~%~V@T<rdfs:range resource=\"" (+ indent *indent*))
        (format-as-datatype stream attribute)
        (format stream "\"/>"))
      (when (and compositions (not inverse-printed-p))
        (loop for composition in compositions do
              (format stream "~%~V@T<owl:propertyChainAxiom rdf:parseType=\"Collection\">"
                      (+ indent *indent*))
              (loop for role in composition
                    for role-name = (if (role-internal-name-p role)
                                        (role-name (role-inverse-internal role))
                                      (role-name role))
                    do
                    (if (role-internal-name-p role)
                        (progn
                          (format stream "~%~V@T<rdf:Description>"
                                  (+ indent *indent* *indent*))
                          (format stream "~%~V@T<owl:inverseOf ~A/>"
                                  (+ indent *indent* *indent* *indent*)
                                  (entity-ref role-name uri nil))
                          (format stream "~%~V@T</rdf:Description>"
                                  (+ indent *indent* *indent*)))
                      (format stream "~%~V@T<rdf:Description ~A/>"
                              (+ indent *indent* *indent*)
                              (entity-ref role-name uri property-hashtable))))
              (format stream "~%~V@T</owl:propertyChainAxiom>"
                      (+ indent *indent*))))
      (format stream "~%~V@T</owl:~A>" indent
              (if (or attribute datatype) "DatatypeProperty" "ObjectProperty"))
      (when inverse-feature-p
        (format stream "~%~V@T<owl:InverseFunctionalProperty ~A/>"
                indent (entity-ref role-name uri property-hashtable)))
      (when transitive-p
        (format stream "~%~V@T<owl:TransitiveProperty ~A/>"
                indent (entity-ref role-name uri property-hashtable)))
      (when feature-p
        (format stream "~%~V@T<owl:FunctionalProperty ~A/>"
                indent (entity-ref role-name uri property-hashtable)))
      (when irreflexive-p
        (format stream "~%~V@T<owl2:IrreflexiveProperty ~A/>"
                indent (entity-ref role-name uri property-hashtable)))
      (when reflexive-p
        (format stream "~%~V@T<owl2:ReflexiveProperty ~A/>"
                indent (entity-ref role-name uri property-hashtable)))
      (when asymmetric-p
        (format stream "~%~V@T<owl2:AsymmetricProperty ~A/>"
                indent (entity-ref role-name uri property-hashtable)))
      (when symmetric-p
        (format stream "~%~V@T<owl:SymmetricProperty ~A/>"
                indent (entity-ref role-name uri property-hashtable))))))

(defun format-as-datarange (stream datatype indent) 
  (when (and (consp datatype) (eq (first datatype) 'd-base-type))
    (setf datatype `(d-datarange ,datatype)))
  (format stream "~%~V@T<owl:DataRange>" indent)
  (multiple-value-bind (other-specs facet-specs)
      (loop for item in (rest datatype) 
            if (eq (first item) 'd-facet)
            collect item into facets
            else
            collect item into other-specs
            finally (return (values other-specs facets)))
    (loop for item in other-specs do
          (format-as-datarange-item stream item (+ indent *indent*)))
    (when facet-specs 
      (format-facet-restrictions stream facet-specs (+ indent *indent*))))
  (format stream "~%~V@T</owl:DataRange>" indent))

(defun format-as-datarange-item (stream datarange-item indent) 
  (cond ((and (consp datarange-item) (eq (first datarange-item) 'd-base-type))
         (format stream "~%~V@T<owl2:onDataRange rdf:resource=\"~A\"/>" 
                 indent (second datarange-item)))
        ((and (consp datarange-item) (eq (first datarange-item) 'd-datarange))
         (format stream "~%~V@T<owl2:onDataRange>" indent)
         (format-as-datarange stream datarange-item (+ indent *indent*))
         (format stream "~%~V@T</owl2:onDataRange>" indent))
        ((and (consp datarange-item) (eq (first datarange-item) 'd-complement))
         (format stream "~%~V@T<owl2:complementOf>" indent)
         (format-as-datarange stream (second datarange-item) (+ indent *indent*))
         (format stream "~%~V@T</owl2:complemenOf>" indent))
        ((and (consp datarange-item) (eq (first datarange-item) 'd-possible-values))
         (format stream "~%~V@T<owl:oneOf>" indent)
         (format-list-of-literals stream (rest datarange-item) (+ indent *indent*))
         (format stream "~%~V@T</owl:oneOf>" indent))
        (t 
         (racer-warn "Unknown datarange specification ~A -- ignored." datarange-item))))

(defun format-facet-restrictions (stream facet-restrictions indent)
  (format stream "~%~V@T<owl2:withRestrictions rdf:parseType=\"Collection\">" indent)
  (loop for facet in facet-restrictions do
        (format-facet stream facet (+ indent *indent*)))
  (format stream "~%~V@T</owl2:withRestrictions>" indent))

(defun format-facet (stream facet indent)
  (format stream "~%~V@T<rdf:Description>" indent)
  (let ((facet-name (convert-to-owl2-facet-name (second facet))))
    (format stream "~%~V@T<~A rdf:datatype=\"~A\">"
            (+ indent *indent*)
            facet-name
            (get-literal-datatype (third facet)))
    (format stream "~%~V@T~A" 
            (+ indent *indent* *indent*)
            (get-literal-value (third facet)))
    (format stream "~%~V@T</~A>"
            (+ indent *indent*)
            facet-name)
    (format stream "~%~V@T</rdf:Description>" indent)))

(defun get-literal-datatype (literal)
  (untransform-type (third literal) (second literal)))

(defun untransform-type (type-spec &optional (value nil value-specified))
  (cond ((and (consp type-spec) (eq (first type-spec) 'd-base-type))
         (second type-spec))
        ((and (null type-spec) value-specified)
         (convert-to-xsd-datatype (type-of value)))
        (t (racer-warn "Illegal type specification -- using xsd:string")
           (concatenate 'string nox:-xsd-uri- "string"))))

(defun get-literal-value (literal)
  (second literal))

(defun format-list-of-literals (stream literals indent)
  (if (null literals)
      (format stream "~%~V@T<rdf:nil/>" indent)
    (let ((literal (first literals)))
      (format stream "~%~V@T<rdf:List>" indent)
      (format stream "~%~V@T<rdf:first rdf:datatype=\"~A\">" (+ indent *indent*)
              (convert-to-xsd-datatype (third literal)))
      (format stream "~%~V@T~A" (+ indent *indent* *indent*) (second literal))
      (format stream "~%~V@T</rdf:first>" (+ indent *indent*))
      (format stream "~%~V@T<rdf:rest>" (+ indent *indent*))
      (format-list-of-literals stream (rest literals) (+ indent *indent* *indent*))
      (format stream "~%~V@T</rdf:rest>" (+ indent *indent*))
      (format stream "~%~V@T</rdf:List>" indent))))

#|
(defun format-as-xsd-datatype (stream datatype indent)
  (format stream "~%~V@T~S" indent (convert-to-xsd-datatype datatype)))
|#

(defun owl-print-disjoint-axiom (stream disjoint-set indent uri class-hashtable)
  (loop for disjuncts on disjoint-set
        for concept-1 = (first disjuncts)
        do
        (loop for concept-2 in (rest disjuncts) do
              (format stream "~%~V@T<owl:Class ~A>~
                              ~%~V@T<owl:disjointWith>~
                              ~%~V@T<owl:Class ~A/>~
                              ~%~V@T</owl:disjointWith>~
                              ~%~V@T</owl:Class>"
                      indent
                      (entity-ref concept-1 uri class-hashtable)
                      (+ indent *indent*) 
                      (+ indent *indent* *indent*)
                      (entity-ref concept-2 uri class-hashtable)
                      (+ indent *indent*)
                      indent))))

(defun owl-print-class-axiom (stream tbox concept-1 concept-2 primitive indent uri 
                                     class-hashtable property-hashtable ind-hashtable)
  (cond ((and concept-2 (not (member concept-2 '(top *top*))))
         (if (and (symbolp concept-1) (not (or (eq concept-1 'top) (eq concept-1 '*top*))))
             (format stream "~%~V@T<owl:Class ~A>"
                     indent (entity-ref concept-1 uri class-hashtable))
           (format stream "~%~V@T<owl:Class rdf:about=\"~A\">" 
                   indent
                   +owl-thing+))
         (if primitive
             (format stream "~%~V@T<rdfs:subClassOf>" (+ indent *indent*))
           (format stream "~%~V@T<owl:equivalentClass>" (+ indent *indent*)))
         (if (symbolp concept-1)
             (format-as-owl-class stream tbox concept-2 (+ indent *indent* *indent*) uri 
                                  class-hashtable property-hashtable ind-hashtable)
           (format-as-owl-class stream tbox `(or (not ,concept-1) ,concept-2) (+ indent *indent* *indent*) 
                                uri class-hashtable property-hashtable ind-hashtable))
         (if primitive
             (format stream "~%~V@T</rdfs:subClassOf>" (+ indent *indent*))
           (format stream "~%~V@T</owl:equivalentClass>" (+ indent *indent*)))
         (format stream "~%~V@T</owl:Class>" indent))
        ((symbolp concept-1)
         (unless (gethash concept-1 class-hashtable)
           (format stream "~%~V@T<owl:Class ~A/>"
                   indent (entity-ref concept-1 uri class-hashtable))))
        (t (racer-warn "Ignoring axiom (~A ~A ~A)" (if primitive 'implies 'equivalent)
                       concept-1 concept-2))))




(defun format-owl-role-name (stream tbox role indent uri &optional no-error-if-internal)
  (if (symbolp role)
      (progn
        (unless no-error-if-internal
          (when (internal-role-p role)
            (error "Internal error: trying to write internal role name while saving tbox in OWL format.")))
        (format stream "~%~V@T<owl:onProperty ~A/>"
                indent (entity-ref role uri nil)))
    (format-owl-role-name stream tbox
                          (role-name (role-inverse-internal (get-tbox-role tbox (second role))))
                          indent uri t)))

(defun owl-name (symbol uri) 
  (cond ((or (eq symbol 'top) (eq symbol '*top*))
         +owl-thing+)
        ((or (eq symbol 'bottom) (eq symbol '*bottom*))
         +owl-nothing+)
        (t 
         (let ((symbol-name (symbol-name symbol)))
           (if (string= uri "")
               symbol-name
             (let ((pos-sharp (position #\# symbol-name :from-end t)))
               (if pos-sharp
                   (if (string= (subseq symbol-name 0 pos-sharp) uri)
                       (subseq symbol-name (1+ pos-sharp))
                     symbol-name)
                 symbol-name)))))))


(defun format-as-owl-class (stream tbox concept indent uri class-hashtable property-hashtable ind-hashtable)
  (if (symbolp concept)
      (cond ((or (eq concept 'top) (eq concept '*top*))
             (format stream "~%~V@T<owl:Class rdf:about=\"~A\"/>" indent +owl-thing+))
            ((or (eq concept 'bottom) (eq concept '*bottom*))
             (format stream "~%~V@T<owl:Class rdf:about=\"~A\"/>"
                     indent +owl-nothing+))
            (t 
             (format stream "~%~V@T<owl:Class ~A/>"
                     indent
                     (entity-ref concept uri class-hashtable))))
    (case (first concept)
      (and
       (format stream "~%~V@T<owl:Class>" indent)
       (format stream "~%~V@T<owl:intersectionOf rdf:parseType=\"Collection\">" (+ indent *indent*))
       (loop for concept-1 in (rest concept) do
             (format-as-owl-class stream tbox concept-1 (+ indent *indent* *indent*) uri
                                  class-hashtable property-hashtable ind-hashtable))
       (format stream "~%~V@T</owl:intersectionOf>" (+ indent *indent*))
       (format stream "~%~V@T</owl:Class>" indent)
       )
      (or
       (format stream "~%~V@T<owl:Class>" indent)
       (format stream "~%~V@T<owl:unionOf rdf:parseType=\"Collection\">" (+ indent *indent*))
       (loop for concept-1 in (rest concept) do
             (format-as-owl-class stream tbox concept-1 (+ indent *indent* *indent*) uri
                                  class-hashtable property-hashtable ind-hashtable))
       (format stream "~%~V@T</owl:unionOf>" (+ indent *indent*))
       (format stream "~%~V@T</owl:Class>" indent)
       )
      (not
       (format stream "~%~V@T<owl:Class>" indent)
       (format stream "~%~V@T<owl:complementOf>" (+ indent *indent*))
       (format-as-owl-class stream tbox (second concept) (+ indent *indent* *indent*) uri
                            class-hashtable property-hashtable ind-hashtable)
       (format stream "~%~V@T</owl:complementOf>" (+ indent *indent*))
       (format stream "~%~V@T</owl:Class>" indent)
       )
      (some
       (let ((role (second concept)))
         (format stream "~%~V@T<owl:Restriction>" indent)
         (if (and (symbolp role) 
                  (or (role-used-as-datatype-property-p role *current-tbox*)
                      (internal-name-used (third concept))))
             (format-cd-role-restriction "owl:someValuesFrom" role (third concept) stream indent uri
                                         class-hashtable property-hashtable)
           (progn 
             (format-owl-role-name stream tbox role (+ indent *indent*) uri)
             (format stream "~%~V@T<owl:someValuesFrom>" (+ indent *indent*))
             (format-as-owl-class stream tbox (third concept) (+ indent *indent* *indent*) uri
                                  class-hashtable property-hashtable ind-hashtable)
             (format stream "~%~V@T</owl:someValuesFrom>" (+ indent *indent*))))
         (format stream "~%~V@T</owl:Restriction>" indent)))
      (all
       (let ((role (second concept)))
         (format stream "~%~V@T<owl:Restriction>" indent)
         
         (if (and (symbolp role) 
                  (or (role-used-as-datatype-property-p role *current-tbox*)
                      (internal-name-used (third concept))))
             (format-cd-role-restriction "owl:allValuesFrom" role (third concept) stream indent uri                                         
                                         class-hashtable property-hashtable)
           (progn 
             (format-owl-role-name stream tbox role (+ indent *indent*) uri)
             (format stream "~%~V@T<owl:allValuesFrom>" (+ indent *indent*))
             (format-as-owl-class stream tbox (third concept) (+ indent *indent* *indent*) uri
                                  class-hashtable property-hashtable ind-hashtable)
             (format stream "~%~V@T</owl:allValuesFrom>" (+ indent *indent*))))
         (format stream "~%~V@T</owl:Restriction>" indent)))
      (at-least
       (cond ((fourth concept)
              (if (eql (second concept) 1)
                  (progn
                    (format stream "~%~V@T<owl:Restriction>" indent)
                    (format-owl-role-name stream tbox (third concept) (+ indent *indent*) uri)
                    (format stream "~%~V@T<owl:someValuesFrom>" (+ indent *indent*))
                    (format-as-owl-class stream tbox (fourth concept) (+ indent *indent* *indent*) uri
                                         class-hashtable property-hashtable ind-hashtable)
                    (format stream "~%~V@T</owl:someValuesFrom>" (+ indent *indent*))
                    (format stream "~%~V@T</owl:Restriction>" indent))
                (progn
                  (format stream "~%~V@T<owl:Restriction>" indent)
                  (format-owl-role-name stream tbox (third concept) (+ indent *indent*) uri)
                  (format stream "~%~V@T<owl:minCardinality rdf:datatype=\"~A\">"
                          (+ indent *indent*) 
                          (format-as-datatype nil 'integer))
                  (format stream "~%~V@T~A" (+ indent *indent* *indent*) (second concept))
                  (format stream "~%~V@T</owl:minCardinality>" (+ indent *indent*))
                  (format stream "~%~V@T<owl2:onClass>" (+ indent *indent*))
                  (format-as-owl-class stream tbox (fourth concept) (+ indent *indent* *indent*) uri
                                       class-hashtable property-hashtable ind-hashtable)
                  (format stream "~%~V@T</owl2:onClass>" (+ indent *indent*))
                  (format stream "~%~V@T</owl:Restriction>" indent))))
             (t
              (format stream "~%~V@T<owl:Restriction>" indent)
              (format-owl-role-name stream tbox (third concept) (+ indent *indent*) uri)
              (format stream "~%~V@T<owl:minCardinality rdf:datatype=\"~A\">"
                      (+ indent *indent*)
                      (format-as-datatype nil 'integer))
              (format stream "~%~V@T~A" (+ indent *indent* *indent*) (second concept))
              (format stream "~%~V@T</owl:minCardinality>" (+ indent *indent*))
              (format stream "~%~V@T</owl:Restriction>" indent))))
      (at-most
       (cond ((fourth concept)
              (if (zerop (second concept))
                  (progn
                    (format stream "~%~V@T<owl:Restriction>" indent)
                    (format-owl-role-name stream tbox (third concept) (+ indent *indent*) uri)
                    (format stream "~%~V@T<owl:allValuesFrom>" (+ indent *indent*))
                    (format-as-owl-class stream tbox (negate-concept-term (fourth concept)) 
                                         (+ indent *indent* *indent*) uri
                                         class-hashtable property-hashtable ind-hashtable)
                    (format stream "~%~V@T</owl:allValuesFrom>" (+ indent *indent*))
                    (format stream "~%~V@T</owl:Restriction>" indent))
                (progn
                  (format stream "~%~V@T<owl:Restriction>" indent)
                  (format-owl-role-name stream tbox (third concept) (+ indent *indent*) uri)
                  (format stream "~%~V@T<owl:maxCardinality rdf:datatype=\"~A\">"
                          (+ indent *indent*)
                          (format-as-datatype nil 'integer))
                  (format stream "~%~V@T~A" (+ indent *indent* *indent*) (second concept))
                  (format stream "~%~V@T</owl:maxCardinality>" (+ indent *indent*))
                  (format stream "~%~V@T<owl2:onClass>" (+ indent *indent*))
                  (format-as-owl-class stream tbox (fourth concept) (+ indent *indent* *indent*) uri
                                       class-hashtable property-hashtable ind-hashtable)
                  (format stream "~%~V@T</owl2:onClass>" (+ indent *indent*))
                  (format stream "~%~V@T</owl:Restriction>" indent))))
             (t 
              (format stream "~%~V@T<owl:Restriction>" indent)
              (format-owl-role-name stream tbox (third concept) (+ indent *indent*) uri)
              (format stream "~%~V@T<owl:maxCardinality rdf:datatype=\"~A\">"
                      (+ indent *indent*)
                      (format-as-datatype nil 'integer))
              (format stream "~%~V@T~A" (+ indent *indent* *indent*) (second concept))
              (format stream "~%~V@T</owl:maxCardinality>" (+ indent *indent*))
              (format stream "~%~V@T</owl:Restriction>" indent))))
      (exactly
       (format-as-owl-class stream tbox
                            `(and (at-least ,(second concept) ,(third concept) ,(fourth concept))
                                  (at-most ,(second concept) ,(third concept) ,(fourth concept)))
                            indent uri
                            class-hashtable property-hashtable ind-hashtable))
      ((a an) 
       (format stream "~%~V@T<owl:Restriction>" indent)
       (format-owl-role-name stream tbox (second concept) (+ indent *indent*) uri)
       (format stream "~%~V@T<owl:someValuesFrom rdf:resource=\"~A\"/>" (+ indent *indent*)
               (let ((role (get-tbox-role *current-tbox* (second concept))))
                 (format-as-datatype nil (role-cd-attribute role))))
       (format stream "~%~V@T</owl:Restriction>" indent))
      (no
       (format stream "~%~V@T<owl:Restriction>" indent)
       (format-owl-role-name stream tbox (second concept) (+ indent *indent*) uri)
       (format stream "~%~V@T<owl:allValuesFrom>" (+ indent *indent*))
       (format-as-owl-class stream tbox 'top (+ indent *indent* *indent*) uri
                            class-hashtable property-hashtable ind-hashtable)
       (format stream "~%~V@T</owl:allValuesFrom>" (+ indent *indent*))
       (format stream "~%~V@T</owl:Restriction>" indent))
      ((min >= >)
       (check-constant concept)
       (format stream "~%~V@T<owl:Restriction>" indent)
       (format-owl-role-name stream tbox (second concept) (+ indent *indent*) uri)
       (format stream "~%~V@T<owl:someValuesFrom>" (+ indent *indent*))
       (format stream "~%~V@T<xsd:simpleType>" (+ indent *indent* *indent*))
       (format stream "~%~V@T<xsd:restriction xsd:base=\"~A\">"
               (+ indent *indent* *indent* *indent*)
               (format-as-datatype nil (role-cd-attribute (get-tbox-role *current-tbox* (second concept)))))
       (format stream "~%~V@T<~A xsd:value=\"~A\"/>"
               (+ indent *indent* *indent* *indent* *indent*)
               (ecase (first concept)
                 ((min >=) "xsd:minInclusive")
                 (> "xsd:minExclusive"))
               (third concept))
       (format stream "~%~V@T</xsd:restriction>"
               (+ indent *indent* *indent* *indent*))
       (format stream "~%~V@T</xsd:simpleType>" (+ indent *indent* *indent*))
       (format stream "~%~V@T</owl:someValuesFrom>" (+ indent *indent*))
       (format stream "~%~V@T</owl:Restriction>" indent))
      ((max <= <)
       (format stream "~%~V@T<owl:Restriction>" indent)
       (format-owl-role-name stream tbox (second concept) (+ indent *indent*) uri)
       (format stream "~%~V@T<owl:someValuesFrom>" (+ indent *indent*))
       (format stream "~%~V@T<xsd:simpleType>" (+ indent *indent* *indent*))
       (format stream "~%~V@T<xsd:restriction xsd:base=\"~A\">"
               (+ indent *indent* *indent* *indent*)
               (format-as-datatype nil (role-cd-attribute (get-tbox-role *current-tbox* (second concept)))))
       (format stream "~%~V@T<~A xsd:value=\"~A\"/>"
               (+ indent *indent* *indent* *indent* *indent*)
               (ecase (first concept)
                 ((max <=) "xsd:maxInclusive")
                 (< "xsd:maxExclusive"))
               (third concept))
       (format stream "~%~V@T</xsd:restriction>"
               (+ indent *indent* *indent* *indent*))
       (format stream "~%~V@T</xsd:simpleType>" (+ indent *indent* *indent*))
       (format stream "~%~V@T</owl:someValuesFrom>" (+ indent *indent*))
       (format stream "~%~V@T</owl:Restriction>" indent))
      (range
       (format-as-owl-class stream tbox 
                            `(and (min ,(second concept) ,(third concept))
                                  (max ,(second concept) ,(fourth concept)))
                            indent uri
                            class-hashtable property-hashtable ind-hashtable))
      ((equal =)
       (format stream "~%~V@T<owl:Restriction>" indent)
       (format-owl-role-name stream tbox (second concept) (+ indent *indent*) uri)                 
       (format stream "~%~V@T<owl:hasValue rdf:datatype=\"~A\">~A</owl:hasValue>"
               (+ indent *indent*)
               (format-as-datatype nil 
                                   (role-cd-attribute (get-tbox-role *current-tbox* 
                                                                     (second concept))))
               (convert-to-xml (third concept)))
       (format stream "~%~V@T</owl:Restriction>" indent))
      (unequal
       (format-as-owl-class stream tbox `(not (equal ,(second concept) ,(third concept)))
                            indent uri
                            class-hashtable property-hashtable ind-hashtable))
      (<>
       (format-as-owl-class stream tbox `(not (= ,(second concept) ,(third concept)))
                            indent uri
                            class-hashtable property-hashtable ind-hashtable))
      (string<> 
       (format-as-owl-class stream tbox `(not (string= ,(second concept) ,(third concept)))
                            indent uri
                            class-hashtable property-hashtable ind-hashtable))
      (string= 
       (format stream "~%~V@T<owl:Restriction>" indent)
       (format-owl-role-name stream tbox (second concept) (+ indent *indent*) uri)                 
       (format stream "~%~V@T<owl:hasValue rdf:datatype=\"~A\">~A</owl:hasValue>" 
               (+ indent *indent*)
               (format-as-datatype nil 'string)
               (convert-to-xml (third concept)))
       (format stream "~%~V@T</owl:Restriction>" indent))
      (d-all
       (let ((role (second concept))
             (datarange (third concept)))
         (format stream "~%~V@T<owl:Restriction>" indent)
         (format stream "~%~V@T<owl:onProperty rdf:resource=\"~A\"/>" (+ indent *indent*) role)
         (format stream "~%~V@T<owl:allValuesFrom>" (+ indent *indent*))
         (format-as-datarange stream datarange (+ indent *indent* *indent*))
         (format stream "~%~V@T</owl:allValuesFrom>" (+ indent *indent*))
         (format stream "~%~V@T</owl:Restriction>" indent)))
      (d-some 
       (let ((role (second concept))
             (datarange (third concept)))
         (format stream "~%~V@T<owl:Restriction>" indent)
         (format stream "~%~V@T<owl:onProperty rdf:resource=\"~A\"/>" (+ indent *indent*) role)
         (format stream "~%~V@T<owl:someValuesFrom>" (+ indent *indent*))
         (format-as-datarange stream datarange (+ indent *indent* *indent*))
         (format stream "~%~V@T</owl:someValuesFrom>" (+ indent *indent*))
         (format stream "~%~V@T</owl:Restriction>" indent)))
      (d-at-least
       (let ((min-cardinality (second concept))
             (role (third concept))
             (datarange (fourth concept)))
         (format stream "~%~V@T<owl:Restriction>" indent)
         (format stream "~%~V@T<owl:onProperty rdf:resource=\"~A\"/>" (+ indent *indent*) role)
         (format stream "~%~V@T<owl:minCardinality rdf:datatype=\"~A\">" 
                 (+ indent *indent*) 
                 (format-as-datatype nil 'integer))
         (format stream "~%~V@T~A" (+ indent *indent* *indent*) min-cardinality)
         (format stream "~%~V@T</owl:minCardinality>" (+ indent *indent*))
         (format stream "~%~V@T<owl:onDataRange" (+ indent *indent*))
         (format-as-datarange stream datarange (+ indent *indent* *indent*))
         (format stream "~%~V@T</owl:onDataRange>" (+ indent *indent*))
         (format stream "~%~V@T</owl:Restriction>" indent)))
      (d-at-most
       (let ((max-cardinality (second concept))
             (role (third concept))
             (datarange (fourth concept)))
         (format stream "~%~V@T<owl:Restriction>" indent)
         (format stream "~%~V@T<owl:onProperty rdf:resource=\"~A\"/>" (+ indent *indent*) role)
         (format stream "~%~V@T<owl:maxCardinality rdf:datatype=\"~A\">" 
                 (+ indent *indent*) 
                 (format-as-datatype nil 'integer))
         (format stream "~%~V@T~A" (+ indent *indent* *indent*) max-cardinality)
         (format stream "~%~V@T</owl:maxCardinality>" (+ indent *indent*))
         (format stream "~%~V@T<owl:onDataRange" (+ indent *indent*))
         (format-as-datarange stream datarange (+ indent *indent* *indent*))
         (format stream "~%~V@T</owl:onDataRange>" (+ indent *indent*))
         (format stream "~%~V@T</owl:Restriction>" indent)))
      (d-filler
       (let ((role (second concept))
             (literal (third concept)))
         (format stream "~%~V@T<owl:Restriction>" indent)
         (format stream "~%~V@T<owl:onProperty rdf:resource=\"~A\"/>" (+ indent *indent*) role)
         (format stream "~%~V@T<owl:hasValue rdf:datatype=\"~A\">~A</owl:hasValue>" 
                 (+ indent *indent*)
                 (get-literal-datatype literal)
                 (convert-to-xml (get-literal-value literal)))
         (format stream "~%~V@T</owl:Restriction>" indent)))
      (self-reference 
       (let ((role (second concept)))
         (format stream "~%~V@T<owl2:SelfRestriction>" indent)
         (format stream "~%~V@T<owl:onProperty rdf:resource=\"~A\"/>" (+ indent *indent*) role)
         (format stream "~%~V@T</owl2:SelfRestriction>" indent)))
      (one-of 
       (format stream "~%~V@T<owl:Class>" indent)
       (format stream "~%~V@T<owl:oneOf rdf:parseType=\"Collection\">" (+ indent *indent*))
       (loop for individual in (rest concept) do
             (format stream "~%~V@T<owl:Thing ~A/>"
                     (+ indent *indent* *indent*) 
                     (entity-ref individual uri ind-hashtable)
                     ))
       (format stream "~%~V@T</owl:oneOf>" (+ indent *indent*))
       (format stream "~%~V@T</owl:Class>" indent)
       )
      (otherwise
       (when *tbox-verbose*
         (racer-warn "Unknown concept term ~A replaced by <owl:Class rdf:about=\"~A\"/>." concept
                     +owl-thing+))
       (format-as-owl-class stream tbox 'top (+ indent *indent*) uri
                            class-hashtable property-hashtable ind-hashtable)))))


(defun internal-name-used (concept)
  (cond ((symbolp concept)
         (or (search "racer-internal%" (symbol-name concept))
             (search "RACER-INTERNAL%" (symbol-name concept))))
        ((consp concept)
         (loop for expr in concept 
               thereis (internal-name-used expr)))))        

(defparameter *output-string* (make-array 1000 :fill-pointer 0 :adjustable t :element-type 'character))

(defun convert-to-xml (object) 
  #+:debug
  (assert (not (eq object *output-string*)))
  (if (stringp object)
      (progn    
        (setf (fill-pointer *output-string*) 0)
        (loop for char across object do
              (cond ((char= char #\&)
                     (vector-push-extend #\& *output-string*)
                     (vector-push-extend #\a *output-string*)
                     (vector-push-extend #\m *output-string*)
                     (vector-push-extend #\p *output-string*)
                     (vector-push-extend #\; *output-string*))
                    ((char= char #\")
                     (vector-push-extend #\& *output-string*)
                     (vector-push-extend #\# *output-string*)
                     (vector-push-extend #\3 *output-string*)
                     (vector-push-extend #\4 *output-string*)
                     (vector-push-extend #\; *output-string*))
                    ((char= char #\<)
                     (vector-push-extend #\& *output-string*)
                     (vector-push-extend #\l *output-string*)
                     (vector-push-extend #\t *output-string*)
                     (vector-push-extend #\; *output-string*))
                    ((char= char #\>)
                     (vector-push-extend #\& *output-string*)
                     (vector-push-extend #\g *output-string*)
                     (vector-push-extend #\t *output-string*)
                     (vector-push-extend #\; *output-string*))
                    (t
                     (vector-push-extend char *output-string*))))
        *output-string*)
    object))

(defun format-cd-role-restriction (owl-quantifier role restriction stream indent uri
                                                  class-hashtable property-hashtable)
  (declare (ignore class-hashtable property-hashtable))
  (format-owl-role-name stream role (+ indent *indent*) uri t)
  (cond ((member restriction '(top *top*))  ; (some r top) kommt durch domain restrictions vor.
         (format stream "~%~V@T<~A rdf:resource=~S/>" 
                 (+ indent *indent*)
                 owl-quantifier
                 (convert-to-xsd-datatype (or (datatype-role-range role *current-tbox*)
                                              'string))))
        ((and (consp restriction)
              (eq (first restriction) 'min))
         (let ((min (third restriction)))
           (if (= min 0)
               (format stream "~%~V@T<~A rdf:resource=~S/>" 
                       (+ indent *indent*)
                       owl-quantifier
                       (node-uri !xsd:positiveInteger))
             (error "NYI"))))
        ((and (consp restriction)
              (eq (first restriction) 'max))
         (let ((max (third restriction)))
           (cond ((= max 0)
                  (format stream "~%~V@T<~A rdf:resource=~S/>" 
                          (+ indent *indent*)
                          owl-quantifier
                          (node-uri !xsd:nonPositiveInteger)))
                 ((= max -1)
                  (format stream "~%~V@T<~A rdf:resource=~S/>" 
                          (+ indent *indent*)
                          owl-quantifier
                          (node-uri !xsd:negativeInteger)))
                 (t (error "NYI")))))
        
        ((symbolp restriction)
         (format stream "~%~V@T<~A rdf:resource=\"~A\"/>" 
                 (+ indent *indent*)
                 owl-quantifier
                 restriction))
        (t
         (ecase (first restriction)
           (no nil)
           ((a an)
            #+:debug
            (unless (string= owl-quantifier "owl:someValuesFrom")
              (break "Should we ignore the quantifier passed as a parameter?"))
            (format stream "~%~V@T<owl:someValuesFrom rdf:resource=~S/>" 
                    (+ indent *indent*)
                    (convert-to-xsd-datatype (or (datatype-role-range role *current-tbox*)
                                                 'string))))
           ((= equal string= boolean=)
            (format stream "~%~V@T<owl:hasValue rdf:datatype=\"~A\">~A</owl:hasValue>" 
                    (+ indent *indent*)
                    (cond ((eq (second restriction) +has-integer-value+)
			   (convert-to-xsd-datatype 'integer))
			  ((eq (second restriction) +has-real-value+)
			   (convert-to-xsd-datatype 'real))
			  ((eq (second restriction) +has-string-value+)
			   (convert-to-xsd-datatype 'string))
			  ((eq (second restriction) +has-boolean-value+)
			   (convert-to-xsd-datatype 'boolean))
			  ((datatype-role-range role *current-tbox*)
			   (convert-to-xsd-datatype (datatype-role-range role *current-tbox*)))
			  (t (convert-to-xsd-datatype 'string)))
                    (convert-to-xml (cond ((eq (third restriction) #t) "true")
                                          ((eq (third restriction) #f) "false")
                                          (t (third restriction))))))))))


(defun format-datarange-list (stream indent datatype values)
  (if (null values)
      (format stream "~%~V@T<rdf:rest rdf:resource=\"~Anil\"/>"
              indent nox:-rdf-uri-)
    (progn (format stream "~%~V@T<rdf:rest rdf:parseType=\"Resource\">"
                   indent)
      (format stream "~%~V@T<rdf:first rdf:datatype=\"~A\">" 
              (+ indent *indent*)
              (convert-to-xsd-datatype datatype))
      (format stream "~%~V@T~A"
              (+ indent *indent* *indent*)
              (first values))
      (format stream "~%~V@T</rdf:first>" (+ indent *indent*))
      (format-datarange-list stream (+ indent *indent*) datatype (rest values))
      (format stream "~%~V@T</rdf:rest>"
              indent))))

#|
(defun format-as-owl-list (stream list indent uri)
  (cond ((null list)
         (format stream "~%~V@T<rdf:nil/>" indent))
        (t
         (format stream "~%~V@T<rdf:List>" indent)
         (format stream "~%~V@T<rdf:first>" (+ indent *indent*))
         (format-as-owl-class stream (first list) (+ indent *indent* *indent*) uri)
         (format stream "~%~V@T</rdf:first>" (+ indent *indent*))
         (format stream "~%~V@T<rdf:rest>" (+ indent *indent*))
         (format-as-owl-list stream (rest list) (+ indent *indent* *indent*) uri)
         (format stream "~%~V@T</rdf:rest>" (+ indent *indent*))
         (format stream "~%~V@T</rdf:List>" indent))))
|#  


(defun print-owl-abox (abox stream &key (uri nil) (as-part-of-kb nil) (import-list :use-default)
                            (ontology-name nil)
                            (class-hashtable (make-hash-table))
                            (property-hashtable (make-hash-table))
                            (tbox-ind-hashtable (make-hash-table)))
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))

  (let ((namespace (get-namespace-prefix (tbox abox))))

    (unless (null namespace)
      (unless (null uri)
        (racer-warn "Ignoring :uri argument ~A." uri))
      (setf uri namespace)))
  
  (if (null uri) 
      (setf uri ""))

  (let ((pos-sharp (position #\# uri)))
    (when pos-sharp
      (setf uri (subseq uri 0 pos-sharp))))
  (unless as-part-of-kb
    (setf class-hashtable (make-hash-table))
    (setf property-hashtable (make-hash-table))
    (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~
                    ~%<rdf:RDF~
                    ~%~V@Txmlns:owl=\"~A\"~
                    ~%~V@Txmlns:owl2=\"~A\"" 
            *indent* +owl-version+ 
            *indent* +owl2-version+)
    (format stream "~%~V@Txmlns:rdf=\"~A\"~
                    ~%~V@Txmlns:rdfs=\"~A\"~
                    ~%~V@Txmlns:xsd=\"~A\""
            *indent* nox:-rdf-uri- 
            *indent* nox:-rdfs-uri- 
            *indent* nox:-xsd-uri-)
    
    (loop for spec in (tbox-namespaces (tbox abox)) 
          unless (or (null (car spec))
                     (member (car spec) '("base" "rdf" "rdfs" "owl" "owl2" "xsd" "swrl" "swrlb")
                             :test #'string=))
          do
          (format stream "~%~V@Txmlns:~A=~S"
                  *indent*
                  (car spec)
                  (cdr spec)))

    (when (abox-rules abox)
      (format stream "~%~V@Txmlns:swrlb=\"~A\"~
                      ~%~V@Txmlns:swrl=\"~A\""
              *indent* +swrlb-version+
              *indent* +swrl-version+))
    (when (and uri (not (string= uri "")))
      (format stream "~%~V@Txmlns=\"~A#\"" *indent* uri))
    (if (and uri (not (string= uri "")))
        (format stream "~%~V@Txml:base=\"~A\">" *indent* uri)
      (format stream ">")))

  (unless as-part-of-kb

    (when (eq import-list :use-default)
      (setf import-list
            (loop for ontology in (tbox-ontologies (tbox abox)) 
                  collect (symbol-name (ontology-name ontology)))))
    
    (cond ((null import-list)
           (format stream "~%~V@T<owl:Ontology rdf:about=\"~A\"/>" *indent* (or ontology-name
                                                                                "")))
          (t (format stream "~%~V@T<owl:Ontology rdf:about=\"~A\">" *indent* (or ontology-name
                                                                                 ""))
             (loop for import-ontology in import-list do
                   (format stream "~%~V@T<owl:imports rdf:resource=\"~A\"/>" 
                           (+ *indent* *indent*)
                           import-ontology))
             (format stream "~%~V@T</owl:Ontology>" *indent*))))
  
  (let ((ind-hashtable (make-hash-table)))
    (loop for assertion in (abox-individual-axioms abox) do
          (push (list 'concept-assertion assertion) (gethash (first assertion) ind-hashtable)))
    (loop for assertion in (abox-individual-identity-disjointness-assertions abox) do
          (when (member (first assertion) '(same-individual-as different-from))
            (push assertion (gethash (second assertion) ind-hashtable))))
    (loop for assertion in (abox-role-axioms abox) 
          unless (secret-individual-p (second (first assertion))) do
          (push (list 'role-assertion assertion) (gethash (first (first assertion)) ind-hashtable)))
    (loop for assertion in (all-annotation-concept-assertions abox)
          unless (secret-individual-p (first assertion)) do
          (push (list 'annotation-assertion assertion) (gethash (first assertion) ind-hashtable)))
    
    (loop for ind in (all-individuals abox) 
          unless (secret-individual-p ind) do
          (print-owl-individual (abox-tbox abox)
                                ind stream uri *indent* ind-hashtable class-hashtable property-hashtable
                                tbox-ind-hashtable))
    
    (loop for assertion in (abox-individual-identity-disjointness-assertions abox) do
          ;;(break)
          (case (first assertion)
            (all-different 
             (format stream 
                     "~%~V@T<owl:AllDifferent>~
                      ~%~V@T<owl:distinctMembers rdf:parseType=\"Collection\">~
                      ~{~%~V@T<owl:Thing rdf:about=\"~A\"/>~}~
                      ~%~V@T</owl:distinctMembers>~
                      ~%~V@T</owl:AllDifferent>"
                     *indent* 
                     (+ *indent* *indent*)
                     (mapcan #'(lambda (ind-name) 
                                 (list (+ *indent* *indent* *indent*)
                                       (owl-name ind-name uri)))
                             (rest assertion))                             
                     (+ *indent* *indent*) 
                     *indent*))))

    (when as-part-of-kb
      (let ((variable-ht (make-hash-table)))
        (loop for rule in (abox-rules abox) do
              (print-swrl-rule rule stream uri *indent* variable-ht))))

    (loop for constraint in (abox-constraints abox) do
          (format t "~%Ignoring constraint ~A." constraint))

    (format stream "~%</rdf:RDF>~%")))


(defun secret-individual-p (ind)
  (let ((pos (search "secret" (symbol-name ind))))
    (and (numberp pos) (zerop pos))))

(defun print-owl-individual (tbox ind stream uri indent ind-hashtable class-hashtable property-hashtable
                                 tbox-ind-hashtable)
  (let ((ind-assertions (reverse (gethash ind ind-hashtable))))
    (when ind-assertions

      ;; concept sollte ein Symbol sein!!!

      (let* ((basic-concept-assertion
              (loop for assertion in ind-assertions 
                    when (and (eq (first assertion) 'concept-assertion)
                              (symbolp (second (second assertion))))
                    do (return assertion)))
             (basic-concept (if basic-concept-assertion
                                (second (second basic-concept-assertion))
                              'top))
             (use-end-description nil))

        ;; We remove ind from the ind-hashtable in order to enforce rdf:ID being used in output 
        ;; if ind is mentioned the first time if not mentioned in tbox-ind-hashtable!
        (unless (gethash ind tbox-ind-hashtable)
          (remhash ind ind-hashtable))

        (multiple-value-bind (class-name class-name-end-token)
            (if (member basic-concept '(top *top*))
                "owl:Thing"
              (insert-namespace-if-appropriate (symbol-name basic-concept)))
          (if (and (null class-name-end-token) (find #\# class-name))

              ;; If basic-concept contains http:// etc. or a sharp even after the namespace is prefixed, we have to use 
              ;; <rdf:Description rdf:about="ind">
              ;;     <rdf:type rdf:resource="http:fsdfsd#dsfsd"/>
              ;;´</rdf:Description>
              ;; In this case, no namespace prefix can be used.

              (progn
                (format stream 
                        "~%~V@T<rdf:Description ~A>" 
                        indent 
                        (entity-ref ind uri ind-hashtable))
                (format stream
                        "~%~V@T<rdf:type ~A/>" 
                        (+ indent *indent*) 
                        (entity-ref basic-concept uri nil)) 
                (setf use-end-description t))
            (format stream 
                    "~%~V@T<~A ~A>"
                    indent 
                    class-name
                    (entity-ref ind uri ind-hashtable)))

          (loop for ind-assertion in ind-assertions
                unless (eq ind-assertion basic-concept-assertion)
                do
                (ecase (first ind-assertion)
                  (concept-assertion
                   (let ((concept (second (second ind-assertion))))
                     (multiple-value-bind (has-value-restriction-p role datatype value)
                         (concept-assertion-for-relation-to-datatype-value concept)
                       (if has-value-restriction-p
                           (format stream 
                                   "~%~V@T<~A rdf:datatype=\"~A\">~A</~A>"
                                   (+ indent *indent*)
                                   (insert-namespace-if-appropriate (owl-name role uri))
                                   datatype
                                   value
                                   (insert-namespace-if-appropriate (owl-name role uri)))
                         (unless (or (eq concept 'top) (eq concept '*top*))
                           (if (and (symbolp concept) (not (or (eq concept 'bottom) (eq concept '*bottom*))))
                               (format stream 
                                       "~%~V@T<rdf:type ~A/>"
                                       (+ indent *indent*)
                                       (entity-ref concept uri nil))
                             (progn 
                               (format stream 
                                       "~%~V@T<rdf:type>"
                                       (+ indent *indent*))
                               (format-as-owl-class stream tbox concept (+ indent *indent* *indent*) uri
                                                    class-hashtable property-hashtable ind-hashtable)
                               (format stream 
                                       "~%~V@T</rdf:type>"
                                       (+ indent *indent*)))))))))
                  (same-individual-as               
                   (let ((ind-name-2 (third ind-assertion)))
                     (format stream 
                             "~%~V@T<owl:sameAs ~A/>"
                             (+ *indent* *indent*) 
                             (entity-ref ind-name-2 uri nil))))
                  (different-from 
                   (let ((ind-name-2 (third ind-assertion)))
                     (format stream 
                             "~%~V@T<owl:differentFrom ~A/>"
                             (+ *indent* *indent*) 
                             (entity-ref ind-name-2 uri nil))))
                  (role-assertion
                   (let* ((role (second (second ind-assertion))))
                     (if (symbolp role)
                         (format stream 
                                 "~%~V@T<~A ~A/>"
                                 (+ indent *indent*)
                                 (insert-namespace-if-appropriate (owl-name role uri))
                                 (entity-ref (second (first (second ind-assertion))) uri nil))
                       (format t "~%Ignoring assertion ~A." 
                               (second ind-assertion)))))
                  (annotation-assertion
                   (let* ((role (second (second (second ind-assertion))))
                          (literal (third (second (second ind-assertion))))
                          (string (second literal))
                          (datatype (second (third literal))))
                     (if (symbolp role)
                         (multiple-value-bind (open-tag close-tag)
                             (insert-namespace-if-appropriate (owl-name role uri)) 
                           (format stream 
                                   "~%~V@T<~A rdf:datatype=\"~A\">~A</~A>"
                                   (+ indent *indent*)
                                   open-tag
                                   datatype
                                   (convert-to-xml string)
                                   (or close-tag open-tag)))
                       (format t "~%Ignoring assertion ~A." 
                               (second ind-assertion)))))))
          (if use-end-description
              (format stream 
                      "~%~V@T</rdf:Description>" indent)
            (format stream 
                    "~%~V@T</~A>"
                    indent (or class-name-end-token class-name))))))))

(defun concept-assertion-for-relation-to-datatype-value (concept)
  (let (role restriction)
    (cond ((and (consp concept)
                (eq (first concept) 'some)
                (setf role (second concept))
                (symbolp role)
                (role-used-as-datatype-property-p role *current-tbox*)
                (setf restriction (third concept))
                (member (first restriction) '(= equal string= boolean=)))
           (values t 
                   role 
                   (cond ((eq (second restriction) +has-integer-value+)
                          (convert-to-xsd-datatype 'integer))
                         ((eq (second restriction) +has-real-value+)
                          (convert-to-xsd-datatype 'real))
                         ((eq (second restriction) +has-string-value+)
                          (convert-to-xsd-datatype 'string))
                         ((eq (second restriction) +has-boolean-value+)
                          (convert-to-xsd-datatype 'boolean))
                         ((datatype-role-range role *current-tbox*)
                          (convert-to-xsd-datatype (datatype-role-range role *current-tbox*)))
                         (t (convert-to-xsd-datatype 'string)))
                   (convert-to-xml (cond ((eq (third restriction) #t) "true")
                                         ((eq (third restriction) #f) "false")
                                         (t (third restriction))))))
          ((and (consp concept)
                (eq (first concept) 'd-filler))
           (let ((literal (third concept)))
             (values t
                     (second concept)
                     (second (third literal))
                     (convert-to-xml (second literal)))))
          (t
           (values nil nil nil nil)))))

(defun insert-namespace-if-appropriate (owl-name)
  (let* ((found (or (search "http:" owl-name)
                      (search "file:" owl-name))))
    (if found
        (let ((pos-sharp (position #\# owl-name))
              (pos-slash (position #\/ owl-name :from-end t)))
          (if pos-sharp
              (let* ((prefix (subseq owl-name 0 (1+ pos-sharp)))
                     (namespace-pair (find-predefined-namespace prefix *current-tbox*)))
                (if namespace-pair
                    (let ((namespace (first namespace-pair)))
                      (if (null namespace)
                          (subseq owl-name (1+ pos-sharp))
                        (values (format nil "~A:~A"
                                        namespace
                                        (subseq owl-name (1+ pos-sharp)))
                                (format nil "~A:~A"
                                        namespace
                                        (subseq owl-name (1+ pos-sharp))))))
                  (values (format nil "ns:~A xmlns:ns=~S" 
                                  (subseq owl-name (1+ pos-sharp))
                                  (subseq owl-name 0 (1+ pos-sharp)))
                          (format nil "ns:~A" 
                                  (subseq owl-name (1+ pos-sharp))))))
            (values (format nil "ns:~A xmlns:ns=~S" 
                            (subseq owl-name (1+ pos-slash))
                            (subseq owl-name 0 (1+ pos-slash)))
                    (format nil "ns:~A" 
                            (subseq owl-name (1+ pos-slash))))))
      owl-name)))




(defun find-predefined-namespace (prefix tbox)
  (rassoc prefix (tbox-namespaces tbox) :test #'string=))

#|
(defun remove-namespace-if-appropriate (owl-name)
  (let ((found (or (search "http://" owl-name) 
		   (search "file:/" owl-name))))
    (if found
        (let ((pos-sharp (position #\# owl-name))
              (pos-slash (position #\/ owl-name :from-end t)))
          (if pos-sharp
              (let* ((prefix (subseq owl-name 0 (1+ pos-sharp)))
                     (namespace (find-predefined-namespace prefix *current-tbox*)))
                ;;(break)
                (if namespace
                    (format nil "~A:~A"
                            namespace
                            (subseq owl-name (1+ pos-sharp)))
                  (format nil "ns:~A" 
                          (subseq owl-name (1+ pos-sharp)))))
            (format nil "ns:~A" 
                    (subseq owl-name (1+ pos-slash)))))
      owl-name)))
|#

(defun print-swrl-rule (rule stream uri indent variable-ht)
  (let ((name (swrl-rule-name rule))
        (head (swrl-rule-head rule))
        (body (swrl-rule-body rule)))
    (format stream "~%~V@T<swrl:Imp rdf:ID=\"~A\">" indent (owl-name name uri))
    (print-swrl-rule-head body stream uri (+ indent *indent*) variable-ht)
    (print-swrl-rule-body head stream uri (+ indent *indent*) variable-ht)
    (format stream "~%~V@T</swrl:Imp>" indent)))

(defun print-swrl-rule-head (clauses stream uri indent variable-ht)
  (format stream "~%~V@T<swrl:body>" indent)
  (print-atom-list clauses stream uri (+ indent *indent*) variable-ht)
  (format stream "~%~V@T</swrl:body>" indent))

(defun print-swrl-rule-body (clauses stream uri indent variable-ht)
  (format stream "~%~V@T<swrl:head>" indent)
  (print-atom-list clauses stream uri (+ indent *indent*) variable-ht)
  (format stream "~%~V@T</swrl:head>" indent))

(defun print-atom-list (clauses stream uri indent variable-ht) 
  (format stream "~%~V@T<swrl:AtomList>" indent)
  (print-first (first clauses) stream uri (+ indent *indent*) variable-ht)
  (print-rest (rest clauses) stream uri (+ indent *indent*) variable-ht)
  (format stream "~%~V@T</swrl:AtomList>" indent))

(defun print-rest (clauses stream uri indent variable-ht)
  (if (null clauses)
      (format stream
              "~%~V@T<rdf:rest rdf:resource=\"~Anil\"/>"
              indent nox:-rdf-uri-)
    (progn
      (format stream "~%~V@T<rdf:rest>" indent)
      (print-atom-list clauses stream uri (+ indent *indent*) variable-ht)
      (format stream "~%~V@T</rdf:rest>" indent))))

(defun print-first (clause stream uri indent variable-ht)
  (format stream "~%~V@T<rdf:first>" indent)
  (print-atom clause stream uri (+ indent *indent*) variable-ht)
  (format stream "~%~V@T</rdf:first>" indent))

(defun print-atom (clause stream uri indent variable-ht)
  (cond ((= (length clause) 3)
         (print-role-atom clause stream uri indent variable-ht))
        ((= (length clause) 2)
         (print-class-atom clause stream uri indent variable-ht))
        (t (racer-warn "Ignoring ~A." clause))))

(defun transform-variable (var uri)
  (if (symbolp var)
      (if (> (length (symbol-name var)) 0)
          (if (char= (char (symbol-name var) 0) #\?)
              (let* ((length-uri (length uri))
                     (uri-ends-in-hash-p (and uri (char= (char uri (1- length-uri)) #\#))))
                (if uri-ends-in-hash-p
                    (intern (concatenate 'string uri
                                         (subseq (symbol-name var) 1))
                            (symbol-package var))
                  (intern (concatenate 'string uri "#"
                                       (subseq (symbol-name var) 1))
                          (symbol-package var))))
            var)
        var)
    var))

(defun print-class-atom (clause stream uri indent variable-ht)
  (format stream "~%~V@T<swrl:ClassAtom>" indent)
  (if (gethash (first clause) variable-ht)
      (format stream "~%~V@T<swrl:argument1 ~A~A/>"
              (+ indent *indent*)
              (entity-ref (transform-variable (first clause) uri) uri nil)
              (owl-name (transform-variable (first clause) uri) uri))
    (progn
      (format stream "~%~V@T<swrl:argument1>" (+ indent *indent*))
      (format stream "~%~V@T<swrl:Variable rdf:ID=\"~A\"/>" 
              (+ indent *indent* *indent*)
              (owl-name (transform-variable (first clause) uri) uri))  
      (format stream "~%~V@T</swrl:argument1>" (+ indent *indent*))))
  (setf (gethash (first clause) variable-ht) t)
  (format stream "~%~V@T<swrl:classPredicate ~A/>" 
          (+ indent *indent*)
          (entity-ref (second clause) uri nil))
  (format stream "~%~V@T</swrl:ClassAtom>" indent))
  
(defun print-role-atom (clause stream uri indent variable-ht)
  (format stream "~%~V@T<swrl:IndividualPropertyAtom>" indent)
  (if (gethash (first clause) variable-ht)
      (format stream "~%~V@T<swrl:argument1 ~A/>"
              (+ indent *indent*)
              (entity-ref (transform-variable (first clause) uri) uri nil))
    (progn
      (format stream "~%~V@T<swrl:argument1>" (+ indent *indent*))
      (format stream "~%~V@T<swrl:Variable rdf:about=\"~A\"/>" 
              (+ indent *indent* *indent*)
              (owl-name (transform-variable (first clause) uri) uri))  
      (format stream "~%~V@T</swrl:argument1>" (+ indent *indent*))))
  (setf (gethash (first clause) variable-ht) t)
  (if (gethash (second clause) variable-ht)
      (format stream "~%~V@T<swrl:argument2 ~A/>"
              (+ indent *indent*)
              (entity-ref (transform-variable (second clause) uri) uri nil))
    (progn
      (format stream "~%~V@T<swrl:argument2>" (+ indent *indent*))
      (format stream "~%~V@T<swrl:Variable rdf:about=\"~A\"/>" 
              (+ indent *indent* *indent*)
              (owl-name (transform-variable (second clause) uri) uri))  
      (format stream "~%~V@T</swrl:argument2>" (+ indent *indent*))))
  (setf (gethash (second clause) variable-ht) t)
  (format stream "~%~V@T<swrl:propertyPredicate ~A/>" 
          (+ indent *indent*)
          (entity-ref (third clause) uri nil))
  (format stream "~%~V@T</swrl:IndividualPropertyAtom>" indent))
  
  
