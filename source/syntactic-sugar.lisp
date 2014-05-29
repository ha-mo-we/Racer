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

(defmethod transform-datatype-expression ((parser nrql-abox-query-parser) property expr)
  ;; (and (min 3) (max 5) (or ...))

  (let ((attribute
         (get-attribute-for-datatype-property parser property)))
    
    (labels ((get-simple-expressions (list)
               (remove-if-not #'(lambda (x)
                                  (and (consp x)
                                       (not (cddr x)) ; sonst wurde schon ersetzt!
                                       (member (to-keyword (first x))
                                               +racer-cd-predicates+)))
                              list))

             (transform (expr attribute)
    
               (if (not (consp expr))
        
                   (parser-error 
                    "Unrecognized concept expression ~A" expr)
      
                 (let ((op (to-keyword (first expr))))

                   (if (member op +racer-cd-predicates+)

                       (if (cddr expr)
                  
                           expr
                
                         `(,(first expr) 
                           ,attribute
                           ,(second expr)))
            
                     (case op
                       ((:and :or :one-of)
                        (let* ((simple-expressions
                                (get-simple-expressions (rest expr)))
                               (atomic-expressions
                                (remove-if-not #'symbolp (rest expr)))
                               (other-expressions
                                (remove-if #'(lambda (x) 
                                               (or (member x simple-expressions)
                                                   (member x atomic-expressions)))
                                           (rest expr))))
                          `(,(first expr)
                            ,@(when atomic-expressions
                                (list atomic-expressions))
                            ,@(mapcar #'(lambda (simple-expr)
                                          `(,(first simple-expr) 
                                            ,attribute
                                            ,(second simple-expr)))
                                      simple-expressions)
                            ,@(mapcar #'(lambda (x) (transform x attribute))
                                      other-expressions))))

                       ((:not)
                        `(not ,(transform (second expr) attribute)))

                       ((:an :a) 

                        (if (second expr)
                            ;;; hier kann nur sowas wie INTEGER, STRING, REAL, CARDINAL STEHEN!
                            ;; TYPE CHECK:         
                            (if (eq (second expr) attribute)
                                `(an ,attribute)
                              (if (member (to-keyword (second expr))
                                          '(:integer :string :real :cardinal :boolean))
                                  (if (eq (second expr) (get-datatype-range parser property))
                                      `(an ,attribute)
                                    `(no ,attribute))
                                ;;(parser-error expr)
                                `(no ,attribute)
                                ))
                          `(an ,attribute)))
              
                       ((:no)
                        `(no ,attribute))

                       (otherwise 
                        (parser-error 
                         "Unrecognized concept expression ~A" expr))))))))


      (if attribute
          (transform expr attribute)

        ;;; kann fuer annotation datatype properties vorkommen, z.B. rdfs:comment 
        ;;; Racer registriert den Range nicht (bzw. datatype-role-range gibt TOP) zurueck
        ;;; ist aber im cache unter (some <dtp> top) -> 'top liefern 

        (progn 
          
          (setf attribute (guess-attribute-from-literal expr))
          
          (if attribute

              (progn 
                (nrql-warning "Cannot determine range of datatype property ~A - using ~A from literal" 
                              property attribute)
                (transform expr attribute))

            (progn 
              (nrql-warning "Cannot determine range of datatype property ~A - using top" 
                            property)
              
              'top)))))))

(defun guess-attribute-from-literal (expr)
  (typecase expr
    (racer:racer-boolean 
     (get-racer-internal-for-range 'boolean))
    (otherwise nil)))



(defmethod transform-new-datatype-expression ((parser nrql-abox-query-parser) property expr)
  (declare (ignorable property))
  expr)

;;;
;;;
;;; 

#-:owl-datatype-support    
(defmethod replace-syntactic-concept-expression-sugar ((parser nrql-abox-query-parser) concept)  
  concept)

#+(and :owl-datatype-support :midelora)
(defmethod replace-syntactic-concept-expression-sugar ((parser midelora-abox-query-parser) concept)
  concept)

#+:owl-datatype-support    
(defmethod replace-syntactic-concept-expression-sugar ((parser nrql-abox-query-parser) concept)
  (when concept
    (cond ((symbolp concept) concept)
          
          ((consp concept)
           (let ((op (to-keyword (first concept))))

             (if (member op +racer-cd-predicates+)
                 
                 ;;; (> DTP 30) -> (some DTP-ROLE (> DTP-ATTRIBUTE 30))
                 
                 (let ((role (second concept)))
                  
                   (cond ((and (role-p role (tbox (substrate parser)))
                               (cd-attribute-p role (tbox (substrate parser))))
                         
                          concept)
                         
                         ((is-datatype-property-p parser role)
                          
                          `(d-some ,role 
                                   ,(transform-datatype-expression parser role
                                                                   `(,(first concept)
                                                                     ,(third concept)))))
                         
                         (t 
                          (parser-error
                           "Unrecognized concept expression ~A" concept))))
               
               (case op 
               
                 ((:not)
                  `(not ,(replace-syntactic-concept-expression-sugar parser (second concept))))

                 ((:d-filler) 
                  (let ((role (second concept))
                        (qual (third concept)))
                    
                    `(,(first concept)
                      ,role
                      ,(transform-new-datatype-expression parser 
                                                          role 
                                                          qual))))

                 (:self-reference
                  (let ((role (second concept)))
                    `(,(first concept)
                      ,role)))
               
                 ((:and :or :one-of)
                  `(,(first concept)
                    ,@(mapcar #'(lambda (x) 
                                  (replace-syntactic-concept-expression-sugar parser x))
                              (rest concept))))

                 ;;;
                 ;;;
                 ;;;
               
                 ((:all :some)
                  (let ((role (second concept))
                        (qual (third concept))
                        (d-op (ecase op
                                (:all 'd-all)
                                (:some 'd-some))))

                    (if (is-datatype-property-p parser role) 

                        ;;; wegen Abwaertskompatibilitaet (altes owl-interface weiterhin unterstuetzen)

                        `(,d-op
                          ,role
                          ,(transform-datatype-expression parser 
                                                          role 
                                                          (if (symbolp qual)
                                                              `(an ,qual)
                                                            qual)))
                    
                      `(,(first concept)
                        ,role
                        ,(replace-syntactic-concept-expression-sugar parser qual)))))

                 ;;; neues owl-interface

                 ((:d-all :d-some)
                  (let ((role (second concept))
                        (qual (third concept)))
                  
                    `(,(first concept)
                      ,role
                      ,(transform-new-datatype-expression parser 
                                                          role 
                                                          qual))))
               
                 ;;; etc. 

                 ((:at-least :at-most :exactly)
                  (let* ((num (second concept))
                         (role (third concept))
                         (qual (fourth concept))
                         (d-op (ecase op
                                 (:at-least 'd-at-least)  
                                 (:at-most  'd-at-most)
                                 (:exactly  'd-exactly))))
                  
                    (if (is-datatype-property-p parser role) 
                      
                        `(,d-op
                          ,num
                          ,role                              
                          ,(transform-datatype-expression parser
                                                          role 
                                                          (if (symbolp qual)
                                                              `(an ,qual)
                                                            qual)))
                    
                      `(,(first concept)
                        ,num ,role ,(replace-syntactic-concept-expression-sugar parser qual)))))
               
                 ((:d-at-least :d-at-most :d-exactly)
                  (let* ((num (second concept))
                         (role (third concept))
                         (qual (fourth concept)))
                  
                    `(,(first concept)
                      ,num
                      ,role                              
                      ,(transform-new-datatype-expression parser
                                                          role 
                                                          qual))))
               
                 ;;; diese, eigentlich nue fuer Attribute gueltige Syntax wird 
                 ;;; nun einfach auf Datatype Properties ausgedehnt!
               
                 ((:a :an)
                
                  (let ((role (second concept)))
                  
                    (cond ((and (role-p role (tbox (substrate parser)))
                                (cd-attribute-p role (tbox (substrate parser))))
                         
                           concept)
                        
                          ((is-datatype-property-p parser role)

                           `(d-some ,role 
                                    ,(transform-datatype-expression parser role 
                                                                    `(an ,(third concept)))))
                         
                          (t 
                           (parser-error
                            "Unrecognized concept expression ~A" concept)))))

                 ((:no)
                
                  (let ((role (second concept)))
                    (cond ((and (role-p role (tbox (substrate parser)))
                                (cd-attribute-p role (tbox (substrate parser))))
                         
                           concept)
                        
                          ((is-datatype-property-p parser role)
                           `(d-all ,role ,(transform-datatype-expression parser role '(no))))

                          (t (parser-error
                              "Unrecognized concept expression ~A" concept)))))

                 (otherwise 

                  (if (member op +racer-cd-predicates+)
                    
                      (let ((role (second concept)))
                      
                        (cond ((and (role-p role (tbox (substrate parser)))
                                    (cd-attribute-p role (tbox (substrate parser))))
                             
                               concept)
                        
                              ((is-datatype-property-p parser role)
                             
                               `(d-some ,role ,(transform-datatype-expression parser 
                                                                              role (third concept))))
                              (t (parser-error 
                                  "Unrecognized concept expression ~A" concept))))
               
                    (parser-error 
                     "Unrecognized concept expression ~A" concept)))))))
          
          (t (parser-error 
              "Unrecognized concept expression ~A" concept)))))

;;;
;;; ab hier sind alles Hacks! 
;;;

(defmethod is-datatype-property-p ((parser nrql-abox-query-parser) property)
  (and (symbolp property)
       (role-p property (tbox (substrate parser)))
       (role-used-as-datatype-property-p property (tbox (substrate parser)))))


(defmethod get-attribute-for-datatype-property ((parser nrql-abox-query-parser) property)
  (let* ((range (datatype-role-range property (tbox (substrate parser))))
	 (range
	  (get-racer-internal-for-range range)))
    (unless range
      (nrql-warning "Cannot determine datarange for datatype property ~A (none specified?) - query will not work"
		    property))
    range))	   

(defun get-racer-internal-for-range2 (range)
  (let* ((range 
          (cond 
           ((symbolp range)
            (if (eq range 'cardinal)
                'integer
              range))
           (t (let* ((range
                      (racer:transform-type range))
                     (range 
                      (if (consp range)
                          (first range)
                        range)))
                range)))))
    (if (eq range t)
        nil
      (intern (format nil "~A-~A-~A"
                      (string-transform "racer-internal%has")
                      range
                      (string-transform "value"))
              (find-package :racer)))))

(defun get-racer-internal-for-range (range)
  (let ((type 
         (if (and (consp range) 
                  (eq (first range) 'd-datarange))
             (transform-data-specification range 'exists)
           (return-from get-racer-internal-for-range
             (get-racer-internal-for-range2 range))))

        (found nil))
    (tree-map (lambda (x) 
                (let ((res 
                       (member x 
                               +racer-internal-datatype-property-roles+)))
                  (when res 
                    (push x found))))
              type)
    (setf found (remove-duplicates found))

    (if (cdr found)
        (progn 
          (nrql-warning "Found more than one base datatype in ~A" 
                        range)
          nil)
      (first found))))

(defmethod get-datatype-range ((parser nrql-abox-query-parser) property)
  (when (is-datatype-property-p parser property)
    (let ((range 
           (datatype-role-range  property (tbox (substrate parser)))))
      (if (and (consp range)
               (eq (first range) 'd-datarange))
          (racer:transform-type (second range))
        range))))
     

