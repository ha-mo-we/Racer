;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

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

(in-package :thematic-substrate)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defconstant +known-aggregation-ops+ '(:count :count-distinct 
                                         :sum :sum-distinct 
                                         :substract :substract-distinct 
                                         :prod :prod-distinct 
                                         :div :div-distinct
                                         :avg :minilisp 
                                         :max :min))
  
  (defconstant +known-order-by-ops+ '(:asc :desc))

  (defconstant +known-abox-operator-projectors+ 
    '(:all-types  :types :instantiators  :all-instantiators
      :all-types-flat :all-instantiators-flat :types-flat :instantiators-flat
      
      :direct-types :most-specific-types :most-specific-instantiators :direct-instantiators
      :direct-types-flat :most-specific-types-flat :most-specific-instantiators-flat :direct-instantiators-flat
      
      :describe
      :individual-synonyms))

  (defconstant +known-tbox-operator-projectors+
    '(:concept-synonyms :describe))

  (defconstant +reserved-tokens+
    (append +known-aggregation-ops+
            +known-order-by-ops+
            +known-abox-operator-projectors+
            +known-tbox-operator-projectors+
            '(:nrql-equal-role
              :bind-individual
              :inv
              :not
              :neg
              :and
              :or
              :one-of
              
              :d-filler
              :d-all
              :d-some
              :d-at-least
              :d-at-most
              :d-exactly
              :d-facet
              :d-literal
              :d-base-type
              :d-complement
              :d-datarange
              :d-possible-values

              :new-ind

              :group-by
              :order

              :strict
              :tag

              :racer
              :satisfies
              :top
              :bottom
              :true-query
              :false-query
              :constraint
              :has-known-successor
              :substitute
              :insert
              :same-as
              :different-from
              :equal
              :=
              :intersection
              :union
              :cap
              :cup
              
              :lambda 

              :told-value
              :told-values
              
              :told-value-if-exists
              :told-values-if-exists
              
              :datatype-filler
              :datatype-fillers

              :annotation-datatype-filler
              :annotation-datatype-fillers 
              
              :unknown-operator
              :bindings-from
              :no-told-value
              :no-known-cd-objects
              :project
              :project-to
              :pi
              :real-top
              :real-bottom))))


;;;
;;; Parser Basis-Syntax 
;;;


(defpersistentclass simple-parser ()
  ((vois :reader vois :initform nil)
   (substrate :reader substrate :initarg :substrate :initform nil)

   ;;; (type-hash :initform (make-hash-table :size 1000 :test #'equalp))

   (original-description :reader original-description :initarg :original-description)

   (reserved-tokens :reader reserved-tokens :initform +reserved-tokens+)

   (query-hash :accessor query-hash :initarg :query-hash :initform nil)))

(defmethod copy ((parser simple-parser) &rest args)
  (apply #'make-instance (type-of parser)
         :substrate (substrate parser) ;;; WICHTIG!!!!
         :original-description (original-description parser)
         :allow-other-keys t
         args))

;;;
;;;
;;;


#+:dlmaps
(defpersistentclass substrate-parser (simple-parser) nil)

#+:dlmaps
(defpersistentclass racer-substrate-parser (simple-parser) nil)

#+:dlmaps
(defpersistentclass racer-descriptions-substrate-parser (simple-parser) nil)

(defpersistentclass nrql-abox-query-parser (simple-parser) nil) 

(defpersistentclass nrql-tbox-query-parser (nrql-abox-query-parser) nil) 

#+:midelora
(defpersistentclass midelora-abox-query-parser (nrql-abox-query-parser) nil) ; es ist praktisch, von nrql-... zu erben! 

#+:midelora
(defpersistentclass midelora-tbox-query-parser (nrql-abox-query-parser) nil) 

;;;
;;; Syntax von Variablen und Individuen
;;;

(defun starts-with-*-p (x &optional (n 0))
  (let ((name (if (stringp x) x (symbol-name x))))
    (when (<=  n (1- (length name)))
      (char= #\* (elt name n)))))

(defun starts-with-$-p (x &optional (n 0))
  (let ((name (if (stringp x) x (symbol-name x))))
    (when (<=  n (1- (length name)))
      (char= #\$ (elt name n)))))

(defun starts-with-?-p (x &optional (n 0))
  (let ((name (if (stringp x) x (symbol-name x))))
    (when (<=  n (1- (length name)))
      (char= #\? (elt name n)))))

;;;
;;;
;;;

(defmethod var-p ((parser simple-parser) x) 
  (and (symbolp x)
       (or (starts-with-?-p x) ; ?x 
           (and (starts-with-$-p x) ; $?x 
                (starts-with-?-p x 1)))
       (not (member x (reserved-tokens parser)))))

(defmethod aux-var-p ((parser simple-parser) x) 
  (and (var-p parser x)
       (starts-with-$-p x)))

(defmethod ind-p ((parser simple-parser) x) 
  (or (and (symbolp x)
           (let ((*treat-as-abox-inds* nil)
                 (*treat-as-substrate-inds* nil))
             (not (var-p parser x)))
           (not (member (to-keyword x) (reserved-tokens parser))))
      (and (or (member x *treat-as-abox-inds*)
               (member x *treat-as-substrate-inds*))
           (let ((*treat-as-abox-inds* nil)
                 (*treat-as-substrate-inds* nil))
             (var-p parser x)))))

(defmethod voi-p ((parser simple-parser) x) 
  (or (var-p parser x)
      (ind-p parser x)))

;;;
;;;
;;;

(defmethod get-name ((parser simple-parser) x)
  (intern
   (format nil "~A"
           (cond ((substrate-thing-p parser x)
                  (if (var-p parser x)
                      (if (aux-var-p parser x)
                          ;; $?x 
                          (subseq (format nil "~A" x) 2)
                        ;; ?x
                        (subseq (format nil "~A" x) 1))
                    x))
                 ((abox-thing-p parser x)
                  (if (var-p parser x)
                      (if (aux-var-p parser x)
                          ;; $?*x 
                          (subseq (format nil "~A" x) 3)
                        ;; ?*x
                        (subseq (format nil "~A" x) 2))
                    (subseq (format nil "~A" x) 1)))
                 (t x)))
   (symbol-package x)))

(defmethod get-name ((parser nrql-abox-query-parser) x)
  (intern (format nil "~A"
                  (cond ((abox-thing-p parser x)
                         (if (var-p parser x)
                             (if (aux-var-p parser x)
                                 ;; $?x 
                                 (subseq (format nil "~A" x) 2)
                               ;; ?x
                               (subseq (format nil "~A" x) 1))
                           x))
                        ((substrate-thing-p parser x)
                         (if (var-p parser x)
                             (if (aux-var-p parser x)
                                 ;; $?*x 
                                 (subseq (format nil "~A" x) 3)
                               ;; ?*x
                               (subseq (format nil "~A" x) 2))
                           (subseq (format nil "~A" x) 1)))
                        (t x)))
          (symbol-package x)))

;;;
;;;
;;;

(defmethod abox-var-p ((parser simple-parser) x)
  ;; ?*x, $?*x 
  (and (var-p parser x)
       (starts-with-*-p x (if (aux-var-p parser x) 2 1))))

(defmethod abox-ind-p ((parser simple-parser) x)
  (and (ind-p parser x)
       (starts-with-*-p x)))

(defmethod abox-thing-p ((parser simple-parser) x)
  (or (abox-var-p parser x)
      (abox-ind-p parser x)))

;;;
;;;
;;;

(defmethod make-abox-var ((parser simple-parser) x &optional aux-var-p)
  (intern (format nil (if *toggle-una-variables-p*
                          (if aux-var-p 
                              "?~A"
                            "$?~A")
                        (if aux-var-p 
                            "$?*~A"
                          "?*~A"))
                  x)))

(defmethod make-abox-ind ((parser simple-parser) x)
  (intern (format nil "*~A" x)))

(defmethod make-substrate-var ((parser simple-parser) x  &optional aux-var-p)
  (intern (format nil (if *toggle-una-variables-p*
                          (if aux-var-p 
                              "?~A"
                            "$?~A")
                        (if aux-var-p 
                            "$?~A"
                          "?~A"))
                  x)))

(defmethod make-substrate-ind ((parser simple-parser) x)
  (intern (format nil "*~A" x)))

;;;
;;;
;;;

(defmethod make-abox-var ((parser nrql-abox-query-parser) x &optional aux-var-p)
  (intern (format nil 
                  (if *toggle-una-variables-p*
                      (if aux-var-p 
                          "?~A"
                        "$?~A")
                    (if aux-var-p 
                        "$?~A"
                      "?~A"))
                    
                  x)))

(defmethod make-abox-ind ((parser nrql-abox-query-parser) x)
  (intern (format nil "~A" x)))

(defmethod make-substrate-var ((parser nrql-abox-query-parser) x &optional aux-var-p)
  (let ((pos (position #\* (symbol-name x))))
    (if (and pos (zerop pos))
        (intern (format nil
                        (if *toggle-una-variables-p*
                            (if aux-var-p 
                                "?~A"
                              "$?~A")
                          (if aux-var-p 
                              "$?~A"
                            "?~A"))
                        x))
      (intern (format nil 
                      (if *toggle-una-variables-p*
                          (if aux-var-p 
                              "?*~A"
                            "$?*~A")
                        (if aux-var-p 
                            "$?*~A"
                          "?*~A"))
                      x)))))

(defmethod make-substrate-ind ((parser nrql-abox-query-parser) x)
  (let ((pos (position #\* (symbol-name x))))
    (if (and pos (zerop pos))
        (intern (format nil "~A" x))
      (intern (format nil "*~A" x)))))

;;;
;;;
;;;

(defmethod make-abox-var ((parser nrql-tbox-query-parser) x &optional aux-var-p)
  (intern (format nil 
                  (if *toggle-una-variables-p*
                      (if aux-var-p 
                          "?~A"
                        "$?~A")
                    (if aux-var-p 
                        "$?~A"
                      "?~A"))
                  x)))

(defmethod abox-var-p ((parser nrql-tbox-query-parser) x)
  (var-p parser x))

(defmethod abox-ind-p ((parser nrql-tbox-query-parser) x)
  (ind-p parser x))

(defmethod substrate-var-p ((parser nrql-tbox-query-parser) x)
  (var-p parser x))

(defmethod substrate-ind-p ((parser nrql-tbox-query-parser) x)
  (ind-p parser x))

;;;
;;; Syntax der Kopf-Projektionoperatoren 
;;; 


(defmethod is-lambda-abox-projector-thing-p ((parser simple-parser) x &key relaxed-p)
  ;; ( (lambda (x y) (* x y) (told-value (width ?x)) ) (told-value (height ?x))) )
  (and (consp x)
       (let ((lambda (first x)))
         (if (consp lambda)
             (and 
              (eq (to-keyword (first lambda)) :lambda)
              (or relaxed-p
                  (and (or (listp (second lambda))
                           (parser-error "Bad lambda head entry: ~A is not a list" (second lambda)))
                       (or (= (length (second lambda)) (length (cdr x)))
                           (parser-error "Bad lambda head entry: ~A formal, but ~A actual arguments" 
                                         (length (second lambda)) (length (cdr x))))
                       (every #'(lambda (x) 
                                  (or (abox-thing-p parser x)
                                      (substrate-thing-p parser x)
                                      (is-told-value-abox-projector-thing-p parser x)
                                      (is-told-value-if-exists-abox-projector-thing-p parser x)
                                      (is-datatype-fillers-abox-projector-thing-p parser x)
                                      (is-annotation-datatype-fillers-abox-projector-thing-p parser x)
                                      (parser-error "Bad lambda argument: ~A" 
                                                    x)))
                              (cdr x)))))

           (eq (to-keyword (first x)) :lambda)))))


#+:midelora 
(defmethod is-lambda-abox-operator-projector-thing-p ((parser midelora-abox-query-parser) x &key relaxed-p)
  (declare (ignore x relaxed-p))
  nil)


;;;
;;; (:direct-types ?x) bzw. (<operator> ?x) 
;;; 

(defmethod is-abox-operator-projector-thing-p ((parser simple-parser) x &key relaxed-p)
  (and (consp x)
       (cdr x)
       (not (cddr x))
       (not (consp (second x)))
       (let ((op (to-keyword (first x))))
         (member op +known-abox-operator-projectors+))
       (or relaxed-p
           (is-abox-voi-p (second x))
           (abox-thing-p parser (second x)))))

#+:midelora 
(defmethod is-abox-operator-projector-thing-p ((parser midelora-abox-query-parser) x &key relaxed-p)
  (declare (ignore x relaxed-p))
  nil)


;;;
;;; (:concept-synonyms ?x) bzw. (<operator> ?x) 
;;; 

(defmethod is-tbox-operator-projector-thing-p ((parser simple-parser) x &key relaxed-p)
  (and (consp x)
       (cdr x)
       (not (cddr x))
       (not (consp (second x)))
       (let ((op (to-keyword (first x))))
         (member op +known-tbox-operator-projectors+))
       (or relaxed-p
           (is-abox-voi-p (second x))
           (abox-thing-p parser (second x)))))

#+:midelora 
(defmethod is-tbox-operator-projector-thing-p ((parser midelora-abox-query-parser) x &key relaxed-p)
  (declare (ignore x relaxed-p))
  nil)

;;;
;;; Attribut-Projektion (Ind -> CD-Objekte), (age ?x), (<cd-attribut> ?x) 
;;;

(defmethod is-attribute-abox-projector-thing-p ((parser simple-parser) x &key relaxed-p (check-p t))
  (and (consp x)
       (cdr x)
       (not (cddr x))

       (not (consp (second x)))

       (=> check-p
       
           (let ((attribute 
                  (convert-to-dl-prover-attribute-expression (substrate parser)
                                                             (first x))))

             (and (or (and (symbolp attribute)
                           (not (member (to-keyword attribute) +reserved-tokens+)))
                      (and (consp attribute) 
                           (eq (first attribute) 'inv)))
                  (role-p attribute (tbox (substrate parser)))
                  (cd-attribute-p attribute                             
                                  (tbox (substrate parser))))))
                    
       (or relaxed-p
           (is-abox-voi-p (second x))
           (abox-thing-p parser (second x)))))

#+:midelora 
(defmethod is-attribute-abox-projector-thing-p ((parser midelora-abox-query-parser) x &key relaxed-p check-p)
  (declare (ignore x relaxed-p check-p))
  nil)

;;;
;;; Datatype Properties (<datatype-property> ?x) 
;;; 

(defmethod is-datatype-property-abox-projector-thing-p ((parser simple-parser) x &key relaxed-p (check-p t))
  (and (consp x)
       (cdr x)
       (not (cddr x))

       (not (consp (second x)))

       (=> check-p
       
           (let ((dtp 
                  (convert-to-dl-prover-role-expression (substrate parser)
                                                        (first x))))
             (is-datatype-property-p parser dtp)))
                    
       (or relaxed-p
           (is-abox-voi-p (second x))
           (abox-thing-p parser (second x)))))


#+:midelora 
(defmethod is-datatype-property-abox-projector-thing-p ((parser midelora-abox-query-parser) x &key relaxed-p check-p)
  (declare (ignore x relaxed-p check-p))
  nil)

;;;
;;; Annotation Datatype Properties  
;;; Fuer Annotation Object Properties sind keine Kopf-Projektionsoperatoren erforderlich!!!
;;; (denn deren Fueller sind ja OWL- bzw. ABox-Individuen) 
;;; 

(defmethod is-annotation-datatype-property-abox-projector-thing-p ((parser simple-parser) x &rest args)
  (apply #'is-datatype-property-abox-projector-thing-p parser x args))

#+:midelora 
(defmethod is-annotation-datatype-property-abox-projector-thing-p ((parser midelora-abox-query-parser) x &key relaxed-p check-p)
  (declare (ignore x relaxed-p check-p))
  nil)

;;;
;;; (told-value (<cd-attribut> ?x)) 
;;;

(defmethod is-told-value-abox-projector-thing-p ((parser simple-parser) x &key relaxed-p (check-p t))
  (and (consp x)
       (cdr x)
       (not (cddr x))
       (member (to-keyword (first x))
               '(:told-value 
                 :told-values))
       (or (is-attribute-abox-projector-thing-p 
            parser (second x) :relaxed-p relaxed-p :check-p check-p)
           (unless relaxed-p
             (parser-error "~A is not a attribute" (first (second x)))))))


#+:midelora
(defmethod is-told-value-abox-projector-thing-p ((parser midelora-abox-query-parser) x &key relaxed-p check-p) 
  (declare (ignorable x relaxed-p check-p))
  nil)

;;;
;;; (told-value-if-exists (<cd-attribut> ?x)) 
;;;

(defmethod is-told-value-if-exists-abox-projector-thing-p ((parser simple-parser) x &key relaxed-p (check-p t))
  (and (consp x)
       (cdr x)
       (not (cddr x))
       (member (to-keyword (first x))
               '(:told-value-if-exists 
                 :told-values-if-exists))
       (or 
        (is-attribute-abox-projector-thing-p 
         parser (second x)
         :relaxed-p relaxed-p :check-p check-p)
        (unless relaxed-p
              (parser-error "~A is not a attribute" 
                            (if (consp (second x))
                                (first (second x))
                              (second x)))))))

#+:midelora
(defmethod is-told-value-if-exists-abox-projector-thing-p ((parser midelora-abox-query-parser) x &key relaxed-p check-p) 
  (declare (ignorable x relaxed-p check-p))
  nil)

;;;
;;; (datatype-fillers (<datatype-property> ?x))
;;;

(defmethod is-datatype-fillers-abox-projector-thing-p ((parser simple-parser) x &key relaxed-p (check-p t))
  (and (consp x)
       (cdr x)
       (not (cddr x))
       (member (to-keyword (first x))
               '(:datatype-filler
                 :datatype-fillers))
       (or 
        (is-datatype-property-abox-projector-thing-p
         parser (second x)
         :relaxed-p relaxed-p :check-p check-p)
        (unless relaxed-p
          (parser-error "~A is not a datatype property" (first (second x)))))))


#+:midelora
(defmethod is-datatype-fillers-abox-projector-thing-p ((parser midelora-abox-query-parser) x &key relaxed-p check-p) 
  (declare (ignorable x relaxed-p check-p))
  nil)

;;;
;;; (annotations (<annotation-datatype-property> ?x)) 
;;;

(defmethod is-annotation-datatype-fillers-abox-projector-thing-p ((parser simple-parser) x &key relaxed-p (check-p t))
  (and (consp x)
       (cdr x)
       (not (cddr x))
       (member (to-keyword (first x))
               '(:annotation-datatype-filler
                 :annotation-datatype-fillers))
       (or (is-annotation-datatype-property-abox-projector-thing-p 
            parser (second x)
            :relaxed-p relaxed-p :check-p check-p)
           (unless relaxed-p
             (parser-error "~A is not an (annotation) datatype property" (first (second x)))))))
  
#+:midelora
(defmethod is-annotation-datatype-fillers-abox-projector-thing-p ((parser midelora-abox-query-parser) x &key relaxed-p check-p) 
  (declare (ignorable x relaxed-p check-p))
  nil)

;;;
;;;
;;; 

(defmethod is-abox-projector-thing-p ((parser simple-parser) x &key relaxed-p)
  (or (is-abox-operator-projector-thing-p parser x  :relaxed-p relaxed-p)
      (is-attribute-abox-projector-thing-p parser x :relaxed-p relaxed-p)
      (is-told-value-abox-projector-thing-p parser x :relaxed-p relaxed-p)
      (is-told-value-if-exists-abox-projector-thing-p parser x :relaxed-p relaxed-p)
      (is-datatype-fillers-abox-projector-thing-p parser x :relaxed-p relaxed-p)
      (is-annotation-datatype-fillers-abox-projector-thing-p parser x :relaxed-p relaxed-p)))

;;;
;;;
;;; 

(defmethod is-aggregation-operator-thing-p ((parser simple-parser) x)
  (and (consp x)
       ;;; (count ?x)
       (member (to-keyword (first x))
               +known-aggregation-ops+)))

(defmethod is-group-by-operator-thing-p ((parser simple-parser) x)
  (and (consp x)
       ;;; (group-by ?x ?y) 
       (eq (to-keyword (first x)) :group-by)))

(defmethod is-order-by-operator-thing-p ((parser simple-parser) x)
  (and (consp x)
       (eq (to-keyword (first x)) :order)
       ;;; (order asc ?x ?y ?z)
       (member (to-keyword (second x)) +known-order-by-ops+)))

;;;
;;;
;;;

(defmethod substrate-ind-p ((parser simple-parser) x)
  ;; x, y, z, NICHT *x, *y, ...! 
  (and (ind-p parser x)
       (or (not (starts-with-*-p x))
           (member x *treat-as-substrate-inds*))))

(defmethod substrate-var-p ((parser simple-parser) x)
  ;; ?x, ?y, $?x, $?y, NICHT ?*x, ?*y, $?*x, $?*y, 
  (and (var-p parser x)
       (> (length (symbol-name x)) 1)
       (not (starts-with-*-p x (if (aux-var-p parser x) 2 1)))))

(defmethod substrate-thing-p ((parser simple-parser) x)
  (or (substrate-var-p parser x)
      (substrate-ind-p parser x)))

;;;
;;; Geaendert fuer nqrl-abox-query-parser!!! 
;;; ABox: ?x Substrate: ?*x 
;;;

(defmethod abox-var-p ((parser nrql-abox-query-parser) x)
  (and (var-p parser x)
       (> (length (symbol-name x)) 1)
       (not (starts-with-*-p x (if (aux-var-p parser x) 2 1)))))

(defmethod abox-ind-p ((parser nrql-abox-query-parser) x)
  (and (ind-p parser x)
       (not (starts-with-*-p x))
       (not (member x *treat-as-substrate-inds*))))

;;;
;;;
;;;

(defmethod substrate-ind-p ((parser nrql-abox-query-parser) x)
  (and (ind-p parser x)
       (or (starts-with-*-p x)
           (member x *treat-as-substrate-inds*))))

(defmethod substrate-var-p ((parser nrql-abox-query-parser) x)
  (and (var-p parser x)
       (> (length (symbol-name x)) 1)
       (starts-with-*-p x (if (aux-var-p parser x) 2 1))))

;;;
;;; Syntax von Atomen
;;;

(defmethod unary-query-p ((parser simple-parser) expr) 
  (and (consp expr)
       (cdr expr) 
       (not (cddr expr))
       (voi-p parser (first expr))))

(defmethod unary-substrate-query-p ((parser simple-parser) expr) 
  (and (unary-query-p parser expr)
       (substrate-thing-p parser (first expr))))

(defmethod unary-abox-query-p ((parser simple-parser) expr) 
  (and (unary-query-p parser expr)
       (abox-thing-p parser (first expr))))

;;;
;;;
;;;

(defmethod binary-query-p ((parser simple-parser) expr)
  (and (cddr expr) 
       (not (cdddr expr))
       (voi-p parser (first expr))
       (voi-p parser (second expr))
       (<=> (substrate-thing-p parser (first expr))
            (substrate-thing-p parser (second expr)))
       (<=> (abox-thing-p parser (first expr))
            (abox-thing-p parser (second expr)))))


(defmethod binary-substrate-query-p ((parser simple-parser) expr) 
  (and (binary-query-p parser expr)
       (substrate-thing-p parser (first expr))
       (substrate-thing-p parser (second expr))))
       
(defmethod binary-abox-query-p ((parser simple-parser) expr) 
  (and (binary-query-p parser expr)
       (abox-thing-p parser (first expr))
       (abox-thing-p parser (second expr))))

(defmethod loom-no-role-filler-query-p ((parser simple-parser) expr) 
  ;;; (?x NIL R), (NIL ?y R)
  (and (cddr expr) 
       (not (cdddr expr))
       (or (and (or (abox-thing-p parser (first expr))
                    (substrate-thing-p parser (first expr)))
                (not (second expr)))
           (and (or (abox-thing-p parser (second expr))
                    (substrate-thing-p parser (second expr)))
                (not (first expr))))))


(defmethod has-known-successor-retrieval-query-p ((parser simple-parser) expr) 
  (and (unary-query-p parser expr)
       (or (valid-has-known-successor-abox-description-p parser (second expr))
           (valid-has-known-successor-substrate-description-p parser (second expr)))))
           

;;;
;;;
;;; 
;;;   

(defmethod atomic-query-p ((parser simple-parser) expr)
  (or (unary-query-p parser expr)
      (binary-query-p parser expr)))

(defmethod atomic-abox-query-p ((parser simple-parser) expr)
  (or (unary-abox-query-p parser expr)
      (binary-abox-query-p parser expr)))

(defmethod atomic-substrate-query-p ((parser simple-parser) expr)
  (or (unary-substrate-query-p parser expr)
      (binary-substrate-query-p parser expr)))

;;;
;;;
;;; 

(defmethod or-query-p ((parser simple-parser) expr) 
  (and (consp expr)
       (member (to-keyword (first expr)) '(:or :union :cup))))

(defmethod and-query-p ((parser simple-parser) expr) 
  (and (consp expr)
       (member (to-keyword (first expr)) '(:and :intersection :cap))))

(defmethod complex-query-p ((parser simple-parser) expr) 
  (or (or-query-p parser expr)
      (and-query-p parser expr)))

(defmethod not-query-p ((parser simple-parser) expr) 
  (and (consp expr)
       (member (to-keyword (first expr)) '(:not :neg))
       (cdr expr)
       (not (cddr expr))))

(defmethod strict-query-p ((parser simple-parser) expr) 
  (and (consp expr)
       (member (to-keyword (first expr)) '(:strict))
       (cdr expr)
       (not (cddr expr))))

(defmethod taged-query-p ((parser simple-parser) expr) 
  (and (consp expr)
       (member (to-keyword (first expr)) '(:tag))
       (cdr expr)
       (cddr expr)
       (not (cdddr expr))))

(defmethod inv-query-p ((parser simple-parser) expr) 
  (and (consp expr)
       (member (to-keyword (first expr)) '(:inv :inverse))
       (cdr expr)
       (not (cddr expr))))

(defmethod bind-individual-query-p ((parser simple-parser) expr) 
  (and (consp expr)
       (member (to-keyword (first expr)) '(:bind-individual))
       (cdr expr)
       (not (cddr expr))
       (voi-p parser (second expr))))

(defmethod top-query-p ((parser simple-parser) expr) 
  (and (consp expr)
       (eq (to-keyword (first expr)) :top)
       (cdr expr)
       (not (cddr expr))
       (voi-p parser (second expr))))

(defmethod bottom-query-p ((parser simple-parser) expr) 
  (and (consp expr)
       (eq (to-keyword (first expr)) :bottom)
       (cdr expr)
       (not (cddr expr))
       (voi-p parser (second expr))))

(defmethod same-as-query-p ((parser simple-parser) expr)
  (and (consp expr)
       (cddr expr) 
       (not (cdddr expr))
       (member (to-keyword (first expr)) '(:same-as :equal :=))
       (voi-p parser (second expr))
       (voi-p parser (third expr))
       (<=> (substrate-thing-p parser (second expr))
            (substrate-thing-p parser (third expr)))
       (<=> (abox-thing-p parser (second expr))
            (abox-thing-p parser (third expr)))
       (not (and (ind-p parser (second expr))
                 (ind-p parser (third expr))))))

(defmethod true-query-p ((parser simple-parser) expr) 
  (member (to-keyword expr) '(:true-query)))

(defmethod false-query-p ((parser simple-parser) expr) 
  (member (to-keyword expr) '(:false-query)))

;;;
;;; Syntax von Descriptions a.d. Atomen
;;;

(defmethod valid-node-and-description-p ((parser simple-parser) expr)
  ;;; Symbole oder einfache negierte Atome werde von diesem Parser
  ;;; als AND interpretiert! 
  (or (symbolp expr)
      (and (consp expr)
           (or (eq (to-keyword (first expr)) :and)
               (eq (to-keyword (first expr)) :not))
           (cdr expr))))
  
(defmethod valid-node-or-description-p ((parser simple-parser) expr)
  (and (consp expr)
       (eq (to-keyword (first expr)) :or)
       (cdr expr)))

;;;
;;;
;;;

(defmethod valid-edge-and-description-p ((parser simple-parser) expr)
  ;;; Symbole oder einfache negierte Atome werde von diesem Parser
  ;;; als AND interpretiert!
  (or (symbolp expr)
      (and (consp expr)       
           (or (eq (to-keyword (first expr)) :and)
               (eq (to-keyword (first expr)) :not))
           (cdr expr))))
      
(defmethod valid-edge-or-description-p ((parser simple-parser) expr)
  (and (consp expr)       
       (eq (to-keyword (first expr)) :or)
       (cdr expr)))

;;;
;;;
;;;

(defmethod valid-has-known-successor-substrate-description-p ((parser simple-parser) expr)
  (declare (ignorable expr))
  ;;; gibt es ja nicht, wird ueberladen
  nil)

;;;
;;;
;;;

(defmethod valid-predicate-description-p ((parser simple-parser) expr)
  (and (consp expr)
       (member (to-keyword (first expr)) '(:satisfies))))

;;;
;;;
;;;

(defmethod valid-racer-concept-description-p ((parser simple-parser) expr)
  (and (consp expr)
       (eq (to-keyword (first expr)) :racer)
       (valid-concept-description-p parser (second expr))))

(defmethod valid-racer-role-description-p ((parser simple-parser) expr)
  (and (consp expr)
       (eq (to-keyword (first expr)) :racer)
       (valid-role-description-p parser (second expr))))

;;;
;;; RACER und MiDeLoRa-Syntax
;;;

(defmethod valid-concept-description-p ((parser simple-parser) expr)
  ;;; is f. simple-parser definiert, weil valid-racer-concept-description
  ;;; diese Methode ruft, fuer RACER descriptions substrate query atome!
  
  (is-valid-racer-concept-expression-p expr :tbox (tbox (substrate parser))))


#+:midelora
(defmethod valid-concept-description-p ((parser midelora-abox-query-parser) expr)
  (declare (ignore expr))
  t)

(defmethod valid-concept-description-p ((parser nrql-tbox-query-parser) expr)
  (case (to-keyword expr)
    ((:top :*top* :bottom :*bottom* :taxonomy-node) t)
    (otherwise (member expr 
                       (all-atomic-concepts (mirror-of-tbox (substrate parser)))
                       :key #'(lambda (x) (ensure-list x))
                       :test #'member))))

#+:midelora
(defmethod valid-concept-description-p ((parser midelora-tbox-query-parser) expr)
  (declare (ignore expr))
  t)

;;;
;;;
;;;

(defmethod valid-role-description-p ((parser simple-parser) expr)
  (and (is-valid-racer-role-expression-p expr
                                         :allow-negated-roles-p *allow-negated-roles-p*
                                         :check-p *racer-check-if-atoms-defined-p* 
                                         :tbox (tbox (substrate parser)))))


#+:midelora
(defmethod valid-role-description-p ((parser midelora-abox-query-parser) expr)
  (declare (ignore expr))
  t)

(defmethod valid-role-description-p ((parser nrql-tbox-query-parser) expr)
  (member (to-keyword expr) '(:has-child :has-parent :has-ancestor :has-descendant :has-synonym)))

#+:midelora
(defmethod valid-role-description-p ((parser midelora-tbox-query-parser) expr)
  (declare (ignore expr))
  t)

;;;
;;;
;;;

(defun negated-dl-role-p (textual-description)
  (and (consp textual-description)
       (eq (first textual-description) 'not)))


;;;
;;;
;;;

(defmethod valid-has-known-successor-abox-description-p ((parser simple-parser) expr)
  (and (consp expr)
       (eq (to-keyword (first expr)) :has-known-successor)
       (is-valid-racer-role-expression-p (second expr) 
                                         :allow-negated-roles-p *allow-negated-roles-p*
                                         :check-p  *racer-check-if-atoms-defined-p* 
                                         :tbox (tbox (substrate parser)))))


#+:midelora
(defmethod valid-has-known-successor-abox-description-p ((parser midelora-abox-query-parser) expr)
  (and (consp expr)
       (eq (to-keyword (first expr)) :has-known-successor)))

;;;
;;;
;;;

(defmethod valid-constraint-description-p ((parser simple-parser) expr)
  (and (consp expr)
       (eq (to-keyword (first expr)) :constraint)
       (and (or (is-valid-racer-attribute-expression-p (second expr) 
                                                       :check-p  *racer-check-if-atoms-defined-p* 
                                                       :tbox (tbox (substrate parser)))
                (is-valid-racer-datatype-role-expression-p (second expr) 
                                                           :check-p  *racer-check-if-atoms-defined-p* 
                                                           :tbox (tbox (substrate parser))))
                
            ;;; die Ketten wurden vorher "platt gemacht"!
            ;;; (second / third expr) sind einfache cd-attribute!
            (or (is-valid-racer-attribute-expression-p (third expr)
                                                       :check-p  *racer-check-if-atoms-defined-p* 
                                                       :tbox (tbox (substrate parser)))
                (is-valid-racer-datatype-role-expression-p (third expr)
                                                           :check-p  *racer-check-if-atoms-defined-p* 
                                                           :tbox (tbox (substrate parser))))
                
            (is-valid-racer-constraint-expression-p (fourth expr))
            (not (fifth expr)))))


#+:midelora
(defmethod valid-constraint-description-p ((parser midelora-abox-query-parser) expr)
  (declare (ignore expr))
  nil)

(defmethod valid-constraint-description-p ((parser nrql-tbox-query-parser) expr)
  (declare (ignore expr))
  nil)

#+:midelora
(defmethod valid-constraint-description-p ((parser midelora-tbox-query-parser) expr)
  (declare (ignore expr))
  nil)

;;;
;;;
;;;

(defmethod valid-original-constraint-description-p ((parser simple-parser) expr)
  ;; m. Role-Chains etc. 

  (or 

   ;;; alte Syntax:
   ;;; (constraint age age (< (+ age-1 20) age-2)) 
   ;;; (constraint age age =) 
      
   (and (consp expr)
        (eq (to-keyword (first expr)) :constraint)
        (cdddr expr)
        (is-valid-racer-constraint-expression-p (fourth expr))            
        (not (fifth expr))
        (every #'(lambda (chain)
                   (and 
                    (every #'(lambda (role) 
                               (is-valid-racer-role-expression-p role 
                                                                 :check-p  *racer-check-if-atoms-defined-p* 
                                                                 :tbox (tbox (substrate parser))))
                           (butlast chain))
                    (or (is-valid-racer-attribute-expression-p (first (last chain))
                                                               :check-p  *racer-check-if-atoms-defined-p* 
                                                               :tbox (tbox (substrate parser)))
                        (is-valid-racer-datatype-role-expression-p (first (last chain))
                                                                   :check-p  *racer-check-if-atoms-defined-p* 
                                                                   :tbox (tbox (substrate parser))))))
               (list (ensure-list (second expr))
                     (ensure-list (third expr)))))

   ;;; neue Syntax: 
   ;;; (constraint (< (+ (age ?x) 20) (age ?y)))
  
   (and (consp expr)
        (eq (to-keyword (first expr)) :constraint)
        (is-valid-racer-constraint-expression-p (fourth expr)) ; wird eh nicht geprueft 
        (not (cddr expr)))))

#+:midelora
(defmethod valid-original-constraint-description-p ((parser midelora-abox-query-parser) expr)
  (declare (ignore expr))
  nil)

(defmethod valid-original-constraint-description-p ((parser nrql-tbox-query-parser) expr)
  (declare (ignore expr))
  nil)

#+:midelora
(defmethod valid-original-constraint-description-p ((parser midelora-tbox-query-parser) expr)
  (declare (ignore expr))
  nil)

;;;
;;;
;;;

(defmethod probably-defined-query-p ((parser simple-parser) expr) 
  (and (not *disable-defined-queries-p*)
       (consp expr)
       (let* ((vois (butlast expr))
              (def (first (last expr))))
         (when (symbolp def)
           (let* ((defs (get-definition1 (substrate parser) 
                                         def
                                         (length vois)
                                         :error-p nil))
                  (def (if (listp defs)
                           (first defs)
                         defs)))
             
             (and (every #'(lambda (x) 
                             (=> x (voi-p parser x)))
                         vois)
                  defs
                  (or *always-prefer-defined-queries-p* 
                      (not (check-for-name-clash (substrate parser)
                                                 (name def) (head def)
                                                 :warn-p t)))))))))
  

(defmethod probably-defined-query-p ((parser nrql-tbox-query-parser) expr) 
  (declare (ignore expr))
  ;;;nil ;;; warum ??? 
  (call-next-method))

(defmethod defined-query-p ((parser simple-parser) expr) 
  (and (not *disable-defined-queries-p*)
       (consp expr)
       (cdr expr)
       (not (cddr expr))
       (consp (second expr))
       (member (to-keyword (first expr)) '(:substitute :insert))
       (let* ((name (car (second expr)))
              (args (cdr (second expr)))
              (defs (get-definition1 (substrate parser) name (length args))))
         defs)))

(defmethod defined-query-p ((parser nrql-tbox-query-parser) expr) 
  (declare (ignore expr))
  ;;;nil ;; warum ???
  (call-next-method))

;;;
;;;
;;;
;;;


(defmethod get-vois-from ((parser simple-parser) expr &key stop-at-projections-p)
  (when (consp expr)
    (cond
     ((or (not-query-p parser expr)
          (inv-query-p parser expr)
	  (strict-query-p parser expr)
          (taged-query-p parser expr))
      (get-vois-from parser (second expr) :stop-at-projections-p stop-at-projections-p))
     ((projection-operator-p parser expr)
      (if stop-at-projections-p
          (second expr)
        (get-vois-from parser (third expr) :stop-at-projections-p stop-at-projections-p)))
     ((minilisp-query-p parser expr)
      (if (consp (first expr))
          (rest expr)
        (let ((res nil))
          (tree-map #'(lambda (x)
                        (when (var-p parser x)
                          (push x res)))
                    expr)
          res)))
     ((or (bind-individual-query-p parser expr)
          (top-query-p parser expr)
          (bottom-query-p parser expr))
      (list (second expr)))
     ((unary-query-p parser expr)
      (list (first expr)))
     ((binary-query-p parser expr)
      (remove nil 
              ;; wg. Loom-no-role-filler-query!
              (list (first expr)
                    (second expr))))
     ((same-as-query-p parser expr)
      (list (second expr)
            (third expr)))
     ((complex-query-p parser expr)
      (remove-duplicates 
       (apply #'append
              (mapcar #'(lambda (expr)
                          (get-vois-from parser expr
                                         :stop-at-projections-p 
                                         stop-at-projections-p))
                      (get-subexpressions parser expr)))
       :test #'equal)))))

;;;
;;;
;;;

(defmethod get-vars-from ((parser simple-parser) expr)
  (remove-if-not #'(lambda (x) (var-p parser x)) (get-vois-from parser expr)))

(defmethod get-inds-from ((parser simple-parser) expr)
  (remove-if-not #'(lambda (x) (ind-p parser x)) (get-vois-from parser expr)))

;;;
;;;
;;;     

(defmethod get-subexpressions ((parser simple-parser) expr)
  (typecase (get-expression-type parser expr)
    (complex-query
     (remove-duplicates (rest expr) :test #'equal))
    (atomic-query
     nil)))

;;;
;;;
;;;

(defmethod get-cor-voi ((parser simple-parser) voi)
  (let ((*toggle-una-variables-p* nil))
    (intern
     (format nil "~A"
             (cond ((abox-var-p parser voi)
                    (make-substrate-var parser 
                                        (get-name parser voi) 
                                        (aux-var-p parser voi)))
                                        
                   ((abox-ind-p parser voi)
                    (make-substrate-ind parser 
                                        (get-name parser voi)))
                 
                   ((substrate-var-p parser voi)
                    (make-abox-var parser 
                                   (get-name parser voi) 
                                   (aux-var-p parser voi)))

                   ((substrate-ind-p parser voi)
                    (make-abox-ind parser 
                                   (get-name parser voi)))))
     (symbol-package voi))))

;;;
;;;
;;;

(defmethod get-corresponding-voi ((parser simple-parser) (voi symbol))
  (get-cor-voi parser voi))

;;;
;;;
;;;

(defun get-var-suffixed-symbol (ind)
  (intern (format nil "~A-~A" 
                  ind
                  (string-transform "var"))))

(defmethod get-var-for-ind ((parser simple-parser) (ind symbol))
  (cond ((substrate-ind-p parser ind)
         (make-substrate-var parser (get-var-suffixed-symbol ind) t))
        ((abox-ind-p parser ind)
         (make-abox-var parser (get-var-suffixed-symbol ind) t))
        (t ind)))

(defmethod get-var-for-ind ((parser nrql-tbox-query-parser) (ind symbol))
  (if (abox-var-p parser ind)
      ind
    (make-abox-var parser (get-var-suffixed-symbol ind) t)))

;;;
;;;
;;;

(defmethod projection-operator-p ((parser simple-parser) expr) 
  (and (consp expr)
       (cddr expr) 
       (not (cdddr expr))
       (member (to-keyword (first expr)) '(:project-to :project :pi))))

;;;
;;;
;;;


(defmethod query-reference-p ((parser simple-parser) expr) 
  (and (consp expr)
       (cddr expr) 
       (not (cdddr expr))
       (member (to-keyword (first expr)) '(:bindings-from))))


;;;
;;;
;;;


(defmethod minilisp-query-p ((parser simple-parser) expr) 
  (or 
   (and (consp expr)
        (consp (first expr))
        (member (to-keyword (first (first expr))) '(:lambda))
        (consp (second (first expr)))
        (= (length (second (first expr)))
           (length (rest expr))))

   (and (consp expr)
        (member (to-keyword (first expr)) '(:lambda)))))

;;;
;;;
;;;


(defmethod get-expression-type :around ((parser simple-parser) expr)
  ;;(or (gethash expr (slot-value parser 'type-hash))
  ;;    (setf (gethash expr (slot-value parser 'type-hash))
  (let ((res
         (call-next-method)))
    (if res
        (if (member res '(tag not inv projection-operator query-reference))
            res
          (make-dispatcher parser res :expression expr))
      (parser-error 
       "Unrecognized expression ~A" expr))))

(defmethod get-expression-type ((parser simple-parser) expr)
  (cond ((null expr)
         
         'true-query)

        ((symbolp expr)

         (cond ((true-query-p parser expr)

                'true-query)

               ((false-query-p parser expr)

                'false-query)))

        ((consp expr)

         (let ((first (first expr)))
           (cond ((taged-query-p parser expr)
                  
                  'tag)
                  
                 ((projection-operator-p parser expr)
                        
                  'projection-operator)

                 ((query-reference-p parser expr)

                  'query-reference)

                 ((minilisp-query-p parser expr)

                  'minilisp-query)
                       
                 ((not-query-p parser expr)
                  'not)
            
                 ((inv-query-p parser expr)
                  'inv)

                 ((bind-individual-query-p parser expr)
                  'bind-individual)

                 ((top-query-p parser expr)
                  'top-query)
            
                 ((bottom-query-p parser expr)
                  'bottom-query)

                 ((or-query-p parser expr)
                  'or-query)

                 ((and-query-p parser expr)
                  'and-query)

                 ((same-as-query-p parser expr)
                  'same-as-query)
            
                 ((abox-thing-p parser first)
                  (let ((second (second expr))
                        (third (third expr))
                        (fourth (fourth expr)))
                    (cond ((and (valid-has-known-successor-abox-description-p parser second)
                                (not third))
                           'has-known-successor-retrieval-query)

                          ((and (valid-concept-description-p parser second) 
                                (not third))                      
                           'instance-retrieval-query)
                     
                          ((and (abox-thing-p parser second)
                                (valid-role-description-p parser third)
                                (not fourth))
                           'edge-retrieval-query)

                          ((and (abox-thing-p parser second)
                                (valid-constraint-description-p parser third)
                                (not fourth))
                           'cd-edge-retrieval-query))))

                 ((substrate-thing-p parser first)

                  (let ((second (second expr))
                        (third (third expr))
                        (fourth (fourth expr)))

                    (cond ((and (valid-has-known-successor-substrate-description-p parser second)
                                (not third))
                           'substrate-has-known-successor-retrieval-query)

                          ((and (valid-predicate-description-p parser second)
                                (not third))
                           'substrate-predicate-node-query)
                     
                          ((and (valid-racer-concept-description-p parser second)
                                (not third))
                           'substrate-racer-node-query)

                          ((and (valid-node-and-description-p parser second)
                                (not third))
                           'substrate-simple-and-node-query)

                          ((and (valid-node-or-description-p parser second)
                                (not third))
                           'substrate-simple-or-node-query)

                     
                          ;;; Edges

                          ((and (substrate-thing-p parser second)
                                (valid-predicate-description-p parser third)
                                (not fourth))
                           'substrate-predicate-edge-query)

                          ((and (substrate-thing-p parser second)
                                (valid-racer-role-description-p parser third)
                                (not fourth))
                           'substrate-racer-edge-query)
                     
                          ((and (substrate-thing-p parser second)
                                (valid-edge-and-description-p parser third)
                                (not fourth))
                           'substrate-simple-and-edge-query)

                          ((and (substrate-thing-p parser second)
                                (valid-edge-or-description-p parser third)
                                (not fourth))
                           'substrate-simple-or-edge-query)))))))))

