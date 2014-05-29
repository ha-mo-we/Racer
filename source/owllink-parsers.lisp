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
;;;;  owllink-parsers.lisp
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
;;;   Purpose: Parser macros for the OWLlink functional processor. 
;;;            Deliberately not very hygienic ;-) Will be improved sometime. 
;;; 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-parser-name (type)
    (intern (format nil "owllink-parse-~A" type)
            (find-package :owl-syntaxes))))

(defmacro loop-parser (type attributes-and-binders inner-parser-name before-loop-form 
                             &body validate-and-return-form)
  (let ((name (get-parser-name type))
        (attributes-and-binders
         (or attributes-and-binders
             (list nil '(progn))))
        (validate-and-return-form
         (or validate-and-return-form '((values response expressions)))))
        
    `(defun ,name (expressions1 &rest args &key (error-p t))
       (declare (ignorable error-p))

       (block parser

         (when *debug-p*
           (terpri)
           (pprint (list :loop-call ',name expressions1)))

       (if (and (consp (first expressions1))
                (eq (expand-functional-tag-name (first (first expressions1)))
                    ',type))

           (let* ((expressions (first expressions1)))

             (multiple-value-bind (attributes expressions)
                 (apply #'get-attributes expressions ',(first attributes-and-binders) args)
          
               (declare (ignorable attributes expressions))

               ,before-loop-form

               (,@(second attributes-and-binders)

                (let ((done nil)
                      (response nil)
                      (responses nil)
                      (expressions (cddr expressions))) 
                  ;; remove Tag name and attributes, e.g. of Tell  

                  (declare (ignorable done))

                  (loop while ;(and expressions (not done)) do
                         expressions do

                        (setf done t)

                        (when *debug-p*
                          (terpri)
                          (pprint (list :in-loop ',name expressions responses)))
                        
                        (multiple-value-setq (response expressions)

                            (apply (symbol-function ',(get-parser-name inner-parser-name))
                                   expressions args))

                        (unless response (setf expressions (cdr expressions)))

                        (when ;response
                            t
                          (setf done nil)
                          (push response responses)))

                  (setf response (nreverse responses))

                  (multiple-value-bind (response expressions)
                      (progn 
                        ,@validate-and-return-form)

                    (when *debug-p*
                      (terpri)
                      (pprint (list :loop-return ',name response expressions)))
                 
                    (values response (rest expressions1)))))))

         (values nil expressions1))))))


(defmacro ano-loop-parser (name inner-parser-name &body validate-and-return-form)
  (let ((name (get-parser-name name))
        (validate-and-return-form
         (or validate-and-return-form '((values response expressions)))))
        
    `(defun ,name (expressions &rest args &key (error-p t))
       (declare (ignorable error-p))

       (block parser

       (when *debug-p*
         (terpri)
         (pprint (list :ano-loop-call ',name expressions)))

       (if (consp expressions)

           (let ((done nil)
                 (response nil)
                 (responses nil))
             
             (declare (ignorable done))

             (loop while ;(and expressions (not done)) do
                   expressions do

                   (setf done t)

                   (when *debug-p*
                     (terpri)
                     (pprint (list :in-ano-loop ',name expressions responses)))

                   (multiple-value-setq (response expressions)

                       (apply (symbol-function ',(get-parser-name inner-parser-name))
                              expressions args))

                   (unless response (setf expressions (cdr expressions)))

                   (when ;response
                       t
                     (setf done nil)
                     (push response responses)))

             (setf response (nreverse responses))

             (multiple-value-bind (response expressions)
                 (progn 
                   ,@validate-and-return-form)

               (when *debug-p*
                 (terpri)
                 (pprint (list :ano-loop-return ',name response expressions)))
                 
               (values response expressions)))

         (values nil expressions))))))


(defmacro ano-optional-loop-parser (name inner-parser-name &body validate-and-return-form)
  (let ((name (get-parser-name name))
        (validate-and-return-form
         (or validate-and-return-form '((values response expressions)))))
        
    `(defun ,name (expressions &rest args &key (error-p t))
       (declare (ignorable error-p))

       (block parser

         (when *debug-p*
           (terpri)
           (pprint (list :ano-loop-call ',name expressions)))

         (if (consp expressions)

             (let ((done nil)
                   (response nil)
                   (responses nil))
             
               (declare (ignorable done))

               (loop while ;(and expressions (not done)) do
                     expressions do

                     (setf done t)

                     (when *debug-p*
                       (terpri)
                       (pprint (list :in-ano-optional-loop ',name expressions responses)))

                     (let ((orig expressions))

                       (multiple-value-setq (response expressions)

                           (apply (symbol-function ',(get-parser-name inner-parser-name))
                                  expressions args))

                       (cond ((not response)
                              (setf expressions nil)

                              (multiple-value-bind (response expressions)
                                  (progn 
                                    ,@validate-and-return-form)

                                (declare (ignorable expressions))

                                (when *debug-p*
                                  (terpri)
                                  (pprint (list :ano-optional-loop-return ',name response orig)))

                                (setf response (nreverse responses))
                            
                                (return-from ,name
                                  (values responses orig))))                          

                             (t
                              (setf done nil)
                              (push response responses)))))

               (setf response (nreverse responses))

               (multiple-value-bind (response expressions)
                   (progn 
                     ,@validate-and-return-form)

                 (when *debug-p*
                   (terpri)
                   (pprint (list :loop-return ',name response expressions)))
                 
                 (values response expressions)))

           (values nil expressions))))))

(defmacro choice-parser (type attributes-and-binders choice-parsers &body validate-and-return-form)
  (let ((name (get-parser-name type))
        (attributes-and-binders
         (or attributes-and-binders
             (list nil '(progn))))
        (validate-and-return-form
         (or validate-and-return-form '((values response expressions)))))

    `(defun ,name (expressions &rest args &key (error-p t))
       (declare (ignorable error-p))

       (block parser

         (if (consp expressions)

             (progn 
              
               (when *debug-p*
                 (terpri) 
                 (pprint (list :choice-call ',name expressions)))

               (dolist (parser ',choice-parsers)

                 (when *debug-p*
                   (terpri)
                   (pprint (list :in-choice-loop *kb* ',name expressions)))

                 (multiple-value-bind (attributes expressions1)
                     (apply #'get-attributes (first expressions)
                            ',(first attributes-and-binders) args)

                   (declare (ignorable attributes))

                   (let ((expressions (cons expressions1 (cdr expressions))))

                     (,@(second attributes-and-binders)

                      (multiple-value-bind (response expressions)
                          (apply (symbol-function (get-parser-name parser) )
                                 expressions
                                 args)

                        (when response
                        
                          (multiple-value-bind (response expressions)
                              (progn 
                                ,@validate-and-return-form)
                          
                            (when *debug-p*
                              (terpri)
                              (pprint (list :choice-return ',name response expressions)))
                          
                            (return-from ,name
                              (values response expressions)))))))))

               (when error-p
                 (return-from parser 
                   (values
                    (owllink-syntax-error-message :error 
                                                  (error-message
                                                   "No valid OWLlink ~A request: ~S" 
                                                   ',type
                                                   (expand-functional-tag-name
                                                    (first (first expressions)))))
                    (rest expressions))))

               (values nil (rest expressions)))

           (values nil expressions))))))


(defmacro choice-parser-with-timeout (type attributes-and-binders choice-parsers &body validate-and-return-form)
  (let ((name (get-parser-name type))
        (attributes-and-binders
         (or attributes-and-binders
             (list nil '(progn))))
        (validate-and-return-form
         (or validate-and-return-form '((values response expressions)))))

    `(defun ,name (expressions &rest args &key (error-p t))
       (declare (ignorable error-p))

       (block parser

         (if (consp expressions)

             (progn 
              
               (when *debug-p*
                 (terpri) 
                 (pprint (list :choice-call ',name expressions)))

               (dolist (parser ',choice-parsers)

                 (when *debug-p*
                   (terpri)
                   (pprint (list :in-choice-loop *kb* ',name expressions)))

                 (multiple-value-bind (attributes expressions1)
                     (apply #'get-attributes (first expressions)
                            ',(first attributes-and-binders) args)

                   (declare (ignorable attributes))

                   (let ((expressions (cons expressions1 (cdr expressions))))

                     (,@(second attributes-and-binders)

                      (multiple-value-bind (response expressions)
                          #+:racer-server
                        (with-timeout (*server-timeout* 
                                              (values
                                               (owllink-error-message 
                                                :error 
                                                (error-message
                                                 "Timeout after ~A seconds for request: ~S" 
                                                 *server-timeout*
                                                 (expand-functional-tag-name
                                                  (first (first expressions)))))
                                               (rest expressions)))
                          (apply (symbol-function (get-parser-name parser) )
                                 expressions
                                 args))
                        #-:racer-server
                        (apply (symbol-function (get-parser-name parser) )
                               expressions
                               args)

                        (when response
                        
                          (multiple-value-bind (response expressions)
                              (progn 
                                ,@validate-and-return-form)
                          
                            (when *debug-p*
                              (terpri)
                              (pprint (list :choice-return ',name response expressions)))
                          
                            (return-from ,name
                              (values response expressions)))))))))

               (when error-p
                 (return-from parser 
                   (values
                    (owllink-syntax-error-message :error 
                                                  (error-message
                                                   "No valid OWLlink ~A request: ~S" 
                                                   ',type
                                                   (expand-functional-tag-name
                                                    (first (first expressions)))))
                    (rest expressions))))

               (values nil (rest expressions)))

           (values nil expressions))))))



(defmacro optional-choice-parser (type attributes-and-binders choice-parsers &body validate-and-return-form)
  (let ((name (get-parser-name type))
        (attributes-and-binders
         (or attributes-and-binders
             (list nil '(progn))))
        (validate-and-return-form
         (or validate-and-return-form '((values response expressions)))))

    `(defun ,name (expressions &rest args)
       (block parser

       (if (consp expressions)

           (progn 
             
             (when *debug-p*
               (terpri) 
               (pprint (list :choice-call ',name expressions)))

             (dolist (parser ',choice-parsers)

               (when *debug-p*
                 (terpri)
                 (pprint (list :in-choice-loop *kb* ',name expressions)))

               (multiple-value-bind (attributes expressions1)
                   (apply #'get-attributes (first expressions)
                          ',(first attributes-and-binders) args)

                 (declare (ignorable attributes))

                 (let ((expressions (cons expressions1 (cdr expressions))))

                   (,@(second attributes-and-binders)

                    (multiple-value-bind (response expressions)
                        (apply (symbol-function (get-parser-name parser) )
                               expressions
                               args)

                      (when response
                        
                        (multiple-value-bind (response expressions)
                            (progn 
                              ,@validate-and-return-form)
                          
                          (when *debug-p*
                            (terpri)
                            (pprint (list :choice-return ',name response expressions)))
                          
                          (return-from ,name
                            (values response expressions)))))))))

             (values
              nil
              expressions))

         (values nil expressions))))))


(defmacro tag-parser (type attributes-and-binders &body validate-and-return-form)

    (let ((name (get-parser-name type))
          (attributes-and-binders
           (or attributes-and-binders
               (list nil '(progn))))
          (validate-and-return-form
           (or validate-and-return-form '((values response)))))

      `(defun ,name (expressions1 &rest args &key (error-p t))
         (declare (ignorable error-p))

         (block parser
  
           (when *debug-p*
           (terpri) 
           (pprint (list :tag-call ',name expressions1)))

         (if (and (consp (first expressions1))
                  (eq (expand-functional-tag-name 
                       (first (first expressions1)))
                      ',type))

             (multiple-value-bind (attributes expressions)
                 (apply #'get-attributes (first expressions1) ',(first attributes-and-binders) args)
               (declare (ignorable attributes expressions))
               
               (setf expressions (cddr expressions)) ; remove tag and attributes

               (,@(second attributes-and-binders)

                (multiple-value-bind (response)
                    (progn 
                      ,@validate-and-return-form)

                  (when *debug-p*
                    (terpri)
                    (pprint (list :tag-return ',name response (rest expressions1))))

                  (values response (rest expressions1)))))

           (values nil expressions1))))))

(defmacro dummy-tag-parser (name &body body)

  (let ((name (get-parser-name name)))

    `(defun ,name (expressions &rest args &key (error-p t))
       (declare (ignorable error-p args))

       (block parser
         
         (when *debug-p*
           (terpri) 
           (pprint (list :dummy-tag-parser-call ',name expressions)))

         ,@body))))


(defmacro with-parser-call ((parser expressions &rest args) &body body)
  `(progn
    
     (block parser

       (multiple-value-bind (response expressions)
           (apply (symbol-function (get-parser-name ',parser))
                  ,expressions
                  ,args)

         (declare (ignorable response expressions))

         ,@body))))

