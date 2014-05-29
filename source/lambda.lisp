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

(defconstant +racer-alias-functions+ 
  '((with-critical-section with-racer-critical-section)
    (add-role-axiom add-role-axioms)))


(defun get-alias-fn (fn)
  (let ((fn (if (stringp fn)
                fn
              (symbol-name fn))))
  (second (assoc fn +racer-alias-functions+
                 :key #'symbol-name
                 :test #'string-equal))))

(defun get-lambda (fn) 
  (if (consp fn)
      fn
    (let* ((fn
            (or (get-alias-fn fn)
                fn)))
      (collect-nrql-args fn))))

(defun get-lambda-parts (fn)
  (let* ((lambda (get-lambda fn)))

    (when (consp lambda) 
         
      (let* ((whole (position '&whole lambda))

             (lambda (if whole 
                         (if (zerop whole)
                             ;;; ignore whole at beginning
                             (rest (rest lambda))
                           (break "don't know how to handle ~S" lambda))

                       lambda))

             (optional (position '&optional lambda))
             (key (position '&key lambda))
             (args (or
                    (position '&args lambda)
                    (position '&rest lambda)
                    (position '&body lambda)))
                
             (n (length lambda))

             (required-args
              (subseq lambda 0 
                      (or (ts::smallest (list optional key args))
                          n)))

             (optional-args
              (when optional
                (subseq lambda (1+ optional) 
                        (or (ts::smallest (list key args))
                            n)))))

        (multiple-value-bind (key-args args-args)
            (cond (key
                   (cond (args
                          (cond ((= (1+ args) key)
                                 ;;; &args &key (&allow-other-keys) 
                                 (values (remove '&allow-other-keys 
                                                 (subseq lambda (1+ key)))
                                         nil))
                                ((< key args)
                                 ;;; &key a b c &args ... 
                                 (values (remove '&allow-other-keys 
                                                 (subseq lambda (1+ key) args))
                                         (subseq lambda (1+ args))))
                                (t 
                                 ;;; &rest args &key a b c
                                 (values 
                                  (remove '&allow-other-keys 
                                          (subseq lambda (1+ key)))
                                  (subseq lambda (1+ args) key)))))

                         (t 
                          (values (remove '&allow-other-keys
                                          (subseq lambda (1+ key)))))))
              
                  (t 
                   (cond (args
                          (values nil 
                                  (subseq lambda (1+ args))))
                         (t (values nil nil)))))

          (values required-args
                  optional-args
                  key-args
                  args-args))))))

(defun collect-nrql-args (fn &optional visited)
  (let ((fn (if (consp fn)
                (second fn) ; strip of function
              fn)))

    (unless (member fn visited)
      (let ((visited (cons fn visited)))

        (cond ((assoc fn ts::*nrql-macros*)
               (let ((fn (ts::get-function-for-macro fn)))
                 (when fn
                   (collect-nrql-args fn))))

              ((assoc fn ts::*nrql-with-macros*)
               (let ((fn (ts::get-function-for-with-macro fn)))
                 (when fn
                   (let ((lambda 
                                 (only-keys
                                  (collect-nrql-args fn))))
                     `((&key ,@lambda) 
                       &body body)))))
              
              (t 

               (let* ((keys nil)
                      (mandatory nil)
                      (no-info-p nil)
                      (methods
                       (or 
                        (let ((res
                               (second (assoc fn ts::*nrql-functions*))))
                          (when res
                            (list res)))
                        (mapcar #'second (get-all-nrql-methods fn))
                        (handler-case
                            (progn 
                              
                              (unless (cdr visited) ; Top Level? 
                                (setf no-info-p t))

                              (list 
                               (let ((x (function-lambda-list fn)))
                                 (if (consp x)
                                     (ts::tree-map #'(lambda (x) 
                                                       (if (symbolp x)
                                                           (if (eq (symbol-package x) 
                                                                   (find-package 'system))
                                                               (intern (symbol-name x) :cl-user)
                                                             x)
                                                         x))
                                                   x)
                                   x))))
                                             
                          (error (error)
                            (return-from collect-nrql-args
                              'unknown))))))

                 (cond (no-info-p 

                        ;;; Racer native API - hier muss alles so bleiben wie es ist!
                        
                        (mapcar #'(lambda (x) 
                                    (if (consp x)
                                        (first x)
                                      x))
                                (first methods)))

                       (t
                 
                        (setf mandatory
                              (without-keys-and-args (first methods)))

                        (setf keys
                              (apply #'append 
                                     (mapcar #'only-keys methods)))
                 
                        (let* ((applied 
                                (second (assoc fn ts::*nrql-apply-delegation*)))

                               (keys-for-called 
                                (apply #'append 
                                       (remove 'unknown
                                               (mapcar #'(lambda (x) 
                                                           (collect-nrql-args x visited))
                                                       applied)))))

                          (when (and (equal (first methods) '(&rest args))
                                     applied)
                     
                            (let ((first-apply 
                                   (collect-nrql-args (second (first applied)))))

                              (setf keys (only-keys first-apply)
                                    mandatory (without-keys-and-args first-apply))
                                  
                              (nrql-warning "~S: I only have a \"&rest args\" lambda list, using ~S from ~S (first apply)"
                                            fn
                                            first-apply
                                            (second (first applied)))))

                          ;;; (pprint (list applied mandatory keys keys-for-called))
                   
                          (remove-duplicates
                    
                           (mapcar #'(lambda (x) 
                                       (if (consp x)
                                           (first x)
                                         x))

                                   (remove '&allow-other-keys
                          
                                           (if (not (cdr visited))
                                               (append mandatory  
                                                       (when (or keys keys-for-called) 
                                                         '(&key))
                                                       keys
                                                       keys-for-called)
                                             (append keys keys-for-called)))))))))))))))



(defun without-rest-arg (args)
  (let ((rest (position '&rest args)))
    (if rest
        (nconc (subseq args 0 rest)
               (subseq args (+ 2 rest)))
      args)))

(defun without-keys-and-args (args)
  (let ((rest (position '&rest args)))
   (if rest
        (subseq args 0 rest)
      (let ((key (position '&key args)))
        (if key
            (subseq args 0 key)
          args)))))

(defun only-keys (args)
  (let ((key (position '&key args)))
    (if key
        (subseq args (1+ key))
      nil)))

(defun get-all-nrql-methods (fn)
  (remove-if-not #'(lambda (x)
                     (eq (first x) fn))
                 ts::*nrql-methods*))
                                  

