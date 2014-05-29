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

;;;
;;; 
;;;

(defgeneric parse-query (query parser &rest args &key &allow-other-keys))

(defgeneric unparse-query (query))

(defvar *var-counter* 0)

(defvar *negated-p* nil)

(defvar *inverse-p* nil)

;;;
;;;
;;;

(defmethod parse-query (expression (parser simple-parser) &rest args)
  (let ((type (get-expression-type parser expression)))

    ;; (format t "~A ~A~%" expression (type-of type))
  
    (typecase type
                 
      (symbol 
                  
       (case type
                 
         (projection-operator
          (let* ((*negated-p* nil)
                 (*inverse-p* nil)
                 (query
                  (apply #'parse-query (third expression) parser args)))

            (setf (slot-value query 'projection-vois)
                  (if (not (eq (projection-vois query)
                               :unspecified))
                      (intersection (retrieve-vois query 
                                                   (second expression))
                                    (projection-vois query))
                    (retrieve-vois query 
                                   (second expression))))

            query))
                    
         (query-reference
          (let ((referenced-query
                 (gethash (second expression)
                          (query-hash parser)))
                                     
                (vois 
                 (mapcar #'(lambda (voi) 
                             (make-voi parser voi))
                         (third expression))))

            (unless referenced-query
              (nrql-error
               "Can't find anonymous subquery ~A" (third expression)))

            (apply #'make-description 'query-reference
                   expression
                   :result-vois vois
                   :referenced-query referenced-query
                   :negated-p *negated-p*
                   :inverse-p *inverse-p*
                   :parser parser
                   :allow-other-keys t                                   
                   args)))

         (tag 
          (apply #'parse-query (second expression) parser 
                 :tag-id (third expression) 
                 args))
         
         (not
          (let ((*negated-p* (not *negated-p*)))
            (apply #'parse-query (second expression) parser args)))
                 
         (inv
          (let ((*inverse-p* (not *inverse-p*)))
            (apply #'parse-query (second expression) parser args)))
                    
         (otherwise                      
          (parser-error 
           "Unrecognized expression ~A" expression))))

      (complex-query 
       (let ((subexpressions 
              (mapcar #'(lambda (x) 
                          (let ((*negated-p* nil))
                            (apply #'parse-query x parser args)))
                      (get-subexpressions parser expression))))

         (when (or *negated-p* *inverse-p*)
           (nrql-error "Parser error: Query ~A is not in NNF" expression))
                         
         (apply #'make-description (type-of type)
                expression
                :allow-other-keys t                                   
                :subqueries subexpressions
                :parser parser
                :negated-p *negated-p*
                args)))
                 
      (true-query
 
       (apply #'make-description 
              (type-of type)
              nil
              :parser parser
              :allow-other-keys t                                   
              args))

      (false-query
                  
       (apply #'make-description 
              (type-of type)
              nil
              :parser parser
              :allow-other-keys t                                   
              args))
                 
      (atomic-query    
               
       (let ((vois (parse-vois parser expression)))

         (when (and *negated-p*
                    (is-same-as-query-p type))

           #| ;;; Optimierung moeglich: Forward Checking beim Binden! 

                      (pushnew (first vois) 
                               (different-from (second vois)))
                      
                      (pushnew (second vois) 
                               (different-from (first vois)))

                      |# ) 

                    
         (apply #'make-description (type-of type)
                (etypecase type
                  (minilisp-query expression)
                  (unary-query (second expression))
                  (binary-query (third expression)))
                :original-query expression
                :allow-negated-roles-p 
                *allow-negated-roles-p*
                :allow-other-keys t
                :vois vois 
                :negated-p *negated-p*
                :inverse-p *inverse-p*                                            
                :parser parser
                args)))

      (otherwise
       (parser-error 
        "Unrecognized expression ~A" expression)))))


;;;
;;;
;;;

(defmethod parse-vois ((parser simple-parser) expr)
  (mapcar #'(lambda (x)
              (make-voi parser x))
          (get-vois-from parser expr)))

;;;
;;;
;;;

(defmethod find-voi ((parser simple-parser) (voi-name symbol))
  (or (find voi-name
            (vois parser)
            :key #'(lambda (x) ;;; relaxed! correct? 
                     (symbol-name (textual-description x)))
            :test #'string-equal)

      #|
      
      (find voi-name
            (vois parser)
            :key #'aka-vois
            :test #'(lambda (x y) 
                      (find-if #'(lambda (y) 
                                   (string-equal (symbol-name y) x))
                               y)))
|# ))


(defmethod find-voi ((parser simple-parser) (voi voi))
  (find voi (vois parser)))

;;;
;;;
;;;

(defmethod corresponding-voi-p ((voi voi))
  (when (corresponding-voi voi)
    t))

;;;
;;; 
;;;

(defmethod retrieve-vois ((query query) vois) 
  (mapcar #'(lambda (x) (find-voi (parser query) x)) vois))

;;;
;;; 
;;;

(defmethod make-voi ((parser simple-parser) expr)
  (or (find-voi parser expr)
      (let* ((cor-voi (find-voi parser (get-corresponding-voi parser expr)))
             (voi (make-instance (cond ;; wichtig: ind-p-Tests zuerst, wegen 
                                       ;; *treat-as-inds* koennen Vars als
                                       ;; Individuen erkannt werden!

                                       ((abox-ind-p parser expr)
                                        'abox-individual)
                                              
                                       ((substrate-ind-p parser expr)
                                        'substrate-individual)
                                              
                                       ((abox-var-p parser expr)
                                        'abox-variable)
                                              
                                       ((substrate-var-p parser expr)
                                        'substrate-variable))
                                       
                                 :substrate (substrate parser)
                                 :una-voi-p (let ((una-p
                                                   (and (not (and (var-p parser expr)
                                                                  (aux-var-p parser expr)))
                                                        ;;; neu - f. Individuen gilt die
                                                        ;;; UNA nicht mehr!
                                                        ;;; w.g. (same-as ?x betty) etc. 
                                                        (not (ind-p parser expr)))))
                                              (if *toggle-una-variables-p*
                                                  (not una-p)
                                                una-p))
                                 :keep-*-p (is-nrql-tbox-query-parser-p parser)
                                 :textual-description expr
                                 :corresponding-voi cor-voi)))

        (when cor-voi 
          (setf (slot-value cor-voi 'corresponding-voi) voi))

        (push voi (slot-value parser 'vois))

        voi)))



(defmethod delete-voi ((parser simple-parser) (voi voi))
  (setf (slot-value parser 'vois)
        (delete voi (slot-value parser 'vois))))
  
  
