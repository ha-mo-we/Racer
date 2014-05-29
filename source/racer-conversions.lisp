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
  (enable-boolean-readers))

(defconstant +reserved-roles+ '()) ; '(same-as) ? warum? 

(defconstant +racer-cd-predicates+ 
  '(:min :max :divisible :not-divisible :string= :string<>
    :> :>= :< :<= :<> := :equal :unequal
    :boolean= :boolean<>))

(defconstant +racer-facets+
  '(:minInclusive :maxInclusive
    :minExclusive :maxExclusive
    :minLength :maxLength :length))

(defconstant +racer-datatypes+
  '(:|http://www.w3.org/2002/07/owl#real|
    :|http://www.w3.org/2002/07/owl#rational|
    
    :|http://www.w3.org/2001/XMLSchema#decimal|
    :|http://www.w3.org/2001/XMLSchema#integer|
    :|http://www.w3.org/2001/XMLSchema#nonNegativeInteger|
    :|http://www.w3.org/2001/XMLSchema#nonPositiveInteger|
    :|http://www.w3.org/2001/XMLSchema#positiveInteger|
    :|http://www.w3.org/2001/XMLSchema#negativeInteger|
    :|http://www.w3.org/2001/XMLSchema#long|
    :|http://www.w3.org/2001/XMLSchema#int|
    :|http://www.w3.org/2001/XMLSchema#short|
    :|http://www.w3.org/2001/XMLSchema#byte|
    :|http://www.w3.org/2001/XMLSchema#unsignedLong|
    :|http://www.w3.org/2001/XMLSchema#unsignedInt|
    :|http://www.w3.org/2001/XMLSchema#unsignedShort|
    :|http://www.w3.org/2001/XMLSchema#unsignedByte|

    ;;; aus Racer known-datatype-p: 

    :|http://www.w3.org/2001/XMLSchema#float|
    :|http://www.w3.org/2001/XMLSchema#double|
    :|http://www.w3.org/2001/XMLSchema#boolean|
    :|http://www.w3.org/2001/XMLSchema#string|
    :|http://www.w3.org/2001/XMLSchema#normalizedString|))

;;;
;;; Diese Funktionen bestimmen, was letztlich zum RACER-Server gesendet wird!!!
;;; 

(defun convert-to-racer-individual-name (expr &optional package)
  (normalize-name
   (if package
       (change-package-of-description expr 
                                      #-:dlmaps package
                                      #+:dlmaps (or package :cl-user))
     expr)))

(defun convert-to-racer-tbox-name (expr &optional package)
  (normalize-name
   (if package
       (change-package-of-description expr 
                                      #-:dlmaps package
                                      #+:dlmaps (or package :cl-user))
     expr)))

(defun convert-to-racer-abox-name (expr &optional package)
  (normalize-name
   (if package
       (change-package-of-description expr 
                                      #-:dlmaps package
                                      #+:dlmaps (or package :cl-user))
     expr)))

(defun convert-to-racer-role-expression (expr &optional package)
  (normalize-racer-role
   (if package
       (change-package-of-description expr package)
     expr)))

(defun convert-to-racer-attribute-expression (expr &optional package)
  (if package
      (change-package-of-description expr package)
    expr))

(defun convert-to-racer-concept-expression (expr &optional package)
  (normalize-racer-concept
   (if package
       (change-package-of-description expr package)
     expr)))

(defun convert-to-racer-constraint-expression (expr &optional package)
  (if package
      (change-package-of-description expr package)
    expr))

;;;
;;;
;;;

(defun normalize-racer-concept (concept)
  (labels ((do-it (concept)
             (if (consp concept)
                 (let ((op (intern (format nil "~A" (first concept)) :keyword))
                       (rel-op (first concept)))
                   (case op 
                     ((:not)
                      `(,rel-op ,(do-it (second concept))))
                     ((:self-reference)
                      `(,rel-op ,(normalize-racer-role (second concept))))
                     ((:or :one-of :and)
                      `(,rel-op ,@(mapcar #'do-it (rest concept))))
                     ((:d-some :d-all)
                      `(,rel-op ,(normalize-racer-role (second concept))
                                ,(normalize-racer-datarange (third concept))))
                     ((:d-filler)
                      `(,rel-op ,(normalize-racer-role (second concept))
                                ,(normalize-racer-datarange (third concept))))
                     ((:some :all)
                      `(,rel-op ,(normalize-racer-role (second concept))
                                ,(do-it (third concept))))
                     ((:at-least :at-most :exactly)
                      `(,rel-op ,(second concept)
                                ,(normalize-racer-role (third concept))
                                ,@(when (fourth concept) 
                                    (list (do-it (fourth concept))))))
                     ((:d-at-least :d-at-most :d-exactly)
                      `(,rel-op ,(second concept)
                                ,(normalize-racer-role (third concept))
                                ,@(when (fourth concept) 
                                    (list (normalize-racer-datarange (fourth concept))))))
                     ((:a :an :no)
                      `(,rel-op ,(second concept)))

                     ((:min :max :divisible :not-divisible
                       :string= :string<> 
                       :boolean= :boolean<>
                       :> :>= :< :<= :<> := :equal :unequal)
                      concept)

                     (otherwise 
                      (parser-error "What is ~A?" concept))))
               concept)))

    (do-it concept)))


(defun normalize-racer-datarange (expr)
  expr)

(defun is-valid-racer-concept-expression-p (concept &key (tbox nil tbox-supplied-p))
  (labels ((do-it (concept)
             (or (and concept
                      (symbolp concept))
                 (and (consp concept)
                      (let ((op (intern (format nil "~A" (first concept)) :keyword)))
                        (case op 
                          ((:not)
                           (do-it (second concept)))
                          ((:or :and)
                           (every #'do-it (rest concept)))
                          ((:one-of) 
                           (every #'symbolp (rest concept)))
                          ((:self-reference)
                           (if tbox-supplied-p 
                               (is-valid-racer-role-expression-p (second concept)
                                                                 :tbox tbox)
                             (is-valid-racer-role-expression-p (second concept))))
                          ((:d-filler)
                           (and (if tbox-supplied-p 
                                    (is-valid-racer-datatype-role-expression-p (second concept)
                                                                      :tbox tbox)
                                  (is-valid-racer-datatype-role-expression-p (second concept)))
                                (is-valid-racer-literal-expression-p (third concept))))
                          ((:all :some)
                           (and (if tbox-supplied-p 
                                    (is-valid-racer-role-expression-p (second concept)
                                                                      :tbox tbox)
                                  (is-valid-racer-role-expression-p (second concept)))
                                (do-it (third concept))))
                          ((:d-all :d-some)
                           (and (if tbox-supplied-p 
                                    (is-valid-racer-datatype-role-expression-p (second concept)
                                                                               :tbox tbox)
                                  (is-valid-racer-datatype-role-expression-p (second concept)))
                                (or (is-valid-racer-data-range-p (fourth concept))
                                    (is-valid-racer-cd-concept-expression-p (fourth concept)))))
                          ;;(is-valid-racer-literal-expression-p (third concept))))
                          ((:at-least :at-most :exactly)
                           (and (if tbox-supplied-p 
                                    (is-valid-racer-role-expression-p (third concept)
                                                                      :tbox tbox)
                                  (is-valid-racer-role-expression-p (third concept)))
                                (=> (fourth concept) 
                                    (do-it (fourth concept)))))
                          ((:d-at-least :d-at-most :d-exactly)
                           (and (if tbox-supplied-p 
                                    (is-valid-racer-datatype-role-expression-p (third concept)
                                                                      :tbox tbox)
                                  (is-valid-racer-datatype-role-expression-p (third concept)))
                                (or (is-valid-racer-data-range-p (fourth concept))
                                    (is-valid-racer-cd-concept-expression-p (fourth concept)))))
                          ((:a :an :no
                            :min :max :divisible :not-divisible
                            :string= :string<>
                            :boolean= :boolean<>
                            :> :>= :< :<= :<> := :equal :unequal)
                           (if tbox-supplied-p 
                               (is-valid-racer-cd-concept-expression-p concept :tbox tbox)
                             (is-valid-racer-cd-concept-expression-p concept)))
                          (otherwise nil)))))))
    (do-it concept)))


(defun is-valid-racer-cd-concept-expression-p (concept &key (tbox nil tbox-supplied-p))
  (labels ((do-it (concept)
             (or #| (and concept
                      (symbolp concept)) |# 
                 (and (consp concept)
                      (let ((op (intern (format nil "~A" (first concept)) :keyword)))
                        (case op 

                          ;; the first clause for :and, :not are for 
                          ;; rewritten version of 
                          ;;
                          ;; (?x (at-least 3 |http://www.owl-ontologies.com/unnamed.owl#p1|
                          ;;              (and (min 0) (max 5) (not (equal 4))))))
                          
                          ;;
                          ;; -> (d-at-least 3 ... (and (min 0) ... )) 
                          ;; for datatype property - syntactic suger!
                          

                          ((:and :not) ;; extended syntax - syntactic sugar! 
                           (every #'(lambda (x) 
                                      (if tbox-supplied-p
                                          (is-valid-racer-cd-concept-expression-p x :tbox tbox)
                                        (is-valid-racer-cd-concept-expression-p x)))
                                  (cdr concept)))

                          ;; this is the official, non-extended cd-concept syntax of racer:  

                          ((:a :an :no)
                           (if tbox-supplied-p 
                               (is-valid-racer-attribute-expression-p (second concept) :tbox tbox)
                             (is-valid-racer-attribute-expression-p (second concept))))
                          ((:min :max :divisible :not-divisible)
                           (and (if tbox-supplied-p 
                                    (is-valid-racer-attribute-expression-p (second concept) :tbox tbox)
                                  (is-valid-racer-attribute-expression-p (second concept)))
                                (integerp (third concept))))
                          ((:string= :string<>)
                           (and (if tbox-supplied-p 
                                    (is-valid-racer-attribute-expression-p (second concept) :tbox tbox)
                                  (is-valid-racer-attribute-expression-p (second concept)))
                                (or (if tbox-supplied-p 
                                        (is-valid-racer-attribute-expression-p (third concept) :tbox tbox)
                                      (is-valid-racer-attribute-expression-p (third concept)))
                                    (stringp (third concept)))))
                          ((:boolean= :boolean<>)
                           (and (if tbox-supplied-p 
                                    (is-valid-racer-attribute-expression-p (second concept) :tbox tbox)
                                  (is-valid-racer-attribute-expression-p (second concept)))
                                (or (if tbox-supplied-p 
                                        (is-valid-racer-attribute-expression-p (third concept) :tbox tbox)
                                      (is-valid-racer-attribute-expression-p (third concept)))
                                    (member (third concept) '(#T #F)))))
                          ((:> :>= :< :<= :<> := :equal :unequal)
                           (and (is-valid-racer-aexpr-p (second concept))
                                (is-valid-racer-aexpr-p (third concept))))
                          (otherwise nil)))))))

    (do-it concept)))


(defun is-valid-racer-constraint-expression-p (expr)
  (declare (ignorable expr))

  t)

(defun is-valid-racer-literal-expression-p (expr)
  (declare (ignorable expr))
  
  (and (consp expr)
       (eq (to-keyword (first expr)) :d-literal)
       (stringp (second expr))
       (=> (third expr)  ; sonst NIL (AUTO TYPE) 
           (is-valid-racer-data-range-p (third expr)))))

(defun is-valid-racer-data-range-p (expr)
  (cond ((symbolp expr)
         (let ((expr2 (to-keyword expr)))
           (or (member expr2 '(:string :integer :boolean :real))
               (member expr2 +racer-datatypes+)
               (null expr))))

        ((consp expr)

         (let ((op (to-keyword (first expr))))

           (case op
         
             (:d-base-type
              (member (to-keyword (second expr))
                      +racer-datatypes+))

             (:d-complement
              (is-valid-racer-data-range-p (second expr)))
           
             ((:d-and :d-or)
              (every #'is-valid-racer-data-range-p (rest expr)))

             (:d-possible-values 
              (every #'is-valid-racer-data-range-p (rest expr)))

             (:d-restriction
              (let* ((basetype (is-valid-racer-data-range-p (second expr)))
                     (facets-and-values (cddr expr))
                     (owl-facets-and-values
                      (loop as facet in facets-and-values by #'cddr 
                            as value in (cdr facets-and-values) by #'cddr 
                            collect (list facet value))))
                (and basetype 
                     (every #'(lambda (x) 
                                (apply #'valid-facet-restriction-p x))
                            owl-facets-and-values))))

             (otherwise nil))))
        
        (t nil)))

(defun valid-facet-restriction-p (facet value)
  (declare (ignorable value))
  (member (to-keyword facet) +racer-facets+))
  
;;;
;;;
;;;

(defun normalize-racer-role (role)
  (labels ((do-it (role &key negated-p inverse-p)
             (if (symbolp role)      
                 (if (not negated-p)
                     (if (not inverse-p)
                         role
                       `(inv ,role))
                   (if (not inverse-p)
                       `(not ,role)
                     `(not (inv ,role))))
               (if (not (cdr role))
                   (do-it (car role) 
                          :negated-p negated-p 
                          :inverse-p inverse-p)
                 (ecase (first role)
                   (inv (do-it (second role)
                               :negated-p negated-p 
                               :inverse-p (not inverse-p)))
                   (not (do-it (second role)
                               :negated-p (not negated-p)
                               :inverse-p inverse-p)))))))

    (do-it role)))



(defun is-valid-racer-datatype-role-expression-p (role &key
                                                       (tbox nil tbox-supplied-p) 
                                                       check-p)
  
  (and (if tbox-supplied-p 
           (is-valid-racer-role-expression-p role
                                             :check-p check-p
                                             :tbox tbox)
         (is-valid-racer-role-expression-p role
                                           :check-p check-p))         
       
       (if tbox-supplied-p 
           (role-used-as-datatype-property-p role
                                             tbox)
         
         (role-used-as-datatype-property-p role (current-tbox)))))
                       
              
(defun is-valid-racer-role-expression-p (role &key 
                                              allow-negated-roles-p (tbox nil tbox-supplied-p) 
                                              check-p)
  (labels ((do-it (role) 
             (or (and role
                      (symbolp role)
                      (not (member role +reserved-roles+))
                      ;; (=> check-p 
                      ;;    (role-p role))
                      (=> (and check-p 
                               (if tbox-supplied-p
                                   (role-p role tbox)
                                 (role-p role)))
                          (not 
                           (if tbox-supplied-p 
                               (cd-attribute-p role tbox)
                             (cd-attribute-p role)))))
      
                 (when (consp role)
                   (let ((first (intern (format nil "~A" (first role))
                                        :keyword)))
                     (case first
                       (:not
                        (and allow-negated-roles-p
                             (do-it (second role))))
                       (:inv 
                        (do-it (second role)))
                       (otherwise nil)))))))
    (do-it role)))

(defun is-valid-racer-attribute-expression-p (role &key check-p (tbox nil tbox-supplied-p))
  (and role (symbolp role)
       (=> check-p 
           (if tbox-supplied-p 
               (cd-attribute-p role tbox)
             (cd-attribute-p role)))))
             

(defun is-valid-racer-aexpr-p (aexpr)
  (declare (ignore aexpr))
  ; ist mir zu kompliziert das zu checken, ist eh eigentlich unnoetig... 
  t)

(defun negated-racer-role-p (role)
  ;;; Annahme: role ist valid-racer-role-expression-p 
  (and (consp role)
       (eq (intern (format nil "~A" (first role)) :keyword) :not)))

;;;
;;;
;;;
  
(defun normalize-name (name)
  (if (symbolp name) 
      name
    (if (not (cdr name))
        (car name)
      (parser-error "Bad name ~A" name))))

