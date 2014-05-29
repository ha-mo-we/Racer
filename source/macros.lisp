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

(defmacro check-for-abort ()
  `(when (abort-search-p *running-query*)
     (throw 'query-execution 'abort)))


(defmacro abortable-dolist ((var list) &body body)
  `(dolist (,var ,list)
     (check-for-abort)
     ,@body))

#|
(defmacro with-timeout1 ((timeout &body timeoutforms) &body body)
  `(if ,timeout
       (with-timeout (,timeout ,@timeoutforms)
	 ,@body)
     (progn ,@body)))
|#


(defmacro with-vois-marked-as-bound (refs &rest body)
  (let ((ref (gensym "VOI"))
        (memo (gensym "MEMO")))
    `(let ((,memo nil))
       (unwind-protect 
           (progn
             (dolist (,ref ,refs)
               (unless (bound-to ,ref)
                 (push ,ref ,memo)
                 (setf (bound-to ,ref) t)
                 (let ((,ref (corresponding-voi ,ref)))
                   (when (and ,ref 
                              (not (bound-to ,ref)))
                     (push ,ref ,memo)
                     (setf (bound-to ,ref) t)))))
             ,@body)
         (dolist (,ref ,memo)
           (setf (bound-to ,ref) nil))))))

(defmacro with-saved-bindings (refs &rest body)
  (let ((memo (gensym)))
    `(let ((,memo nil))
       (unwind-protect 
           (progn
             (dolist (ref ,refs)
               (push (list ref (bound-to ref))
                     ,memo)
               (let ((cor-ref (corresponding-voi ref)))
                 (when cor-ref 
                   (push (list cor-ref (bound-to cor-ref))
                         ,memo))))
             ,@body)

         (dolist (entry ,memo)
           (setf (bound-to (first entry))
                 (second entry)))))))

(defmacro defquery-code ((type) &rest body)
  (let* ((tester-code 
          (rest (assoc :tester body)))
         (tester-compiler-code
          (rest (assoc :compiler tester-code)))
         (tester-runtime-code
          (rest (assoc :runtime tester-code)))
	 
         (enumerator-code 
          (rest (assoc :enumerator body)))
         (enumerator-compiler-code
          (rest (assoc :compiler enumerator-code)))
         (enumerator-runtime-code
          (rest (assoc :runtime enumerator-code)))        

         (from-bound-enumerator-code 
          (rest (assoc :from-bound-enumerator body)))
         (from-bound-enumerator-compiler-code
          (rest (assoc :compiler from-bound-enumerator-code)))
         (from-bound-enumerator-runtime-code
          (rest (assoc :runtime from-bound-enumerator-code)))
	 
         (to-bound-enumerator-code 
          (rest (assoc :to-bound-enumerator body)))
         (to-bound-enumerator-compiler-code
          (rest (assoc :compiler to-bound-enumerator-code)))
         (to-bound-enumerator-runtime-code
          (rest (assoc :runtime to-bound-enumerator-code))))
    
    `(progn
       ,@(when tester-compiler-code
           `((defmethod get-tester-code ((query ,type) ,@(first tester-compiler-code))
               ,@(rest tester-compiler-code))))
       ,@(when tester-runtime-code
           `((defmethod evaluate-tester ((query ,type) ,@(first tester-runtime-code))
               ,@(rest tester-runtime-code))))

       ,@(when enumerator-compiler-code
           `((defmethod get-enumerator-code ((query ,type) ,@(first enumerator-compiler-code))
               ,@(rest enumerator-compiler-code))))
       ,@(when enumerator-runtime-code
           `((defmethod evaluate-enumerator ((query ,type) ,@(first enumerator-runtime-code))
               ,@(rest enumerator-runtime-code))))
       
       ,@(when from-bound-enumerator-compiler-code
           `((defmethod get-from-bound-enumerator-code ((query ,type) ,@(first from-bound-enumerator-compiler-code))
               ,@(rest from-bound-enumerator-compiler-code))))
       ,@(when from-bound-enumerator-runtime-code
           `((defmethod evaluate-from-bound-enumerator ((query ,type) ,@(first from-bound-enumerator-runtime-code))
               ,@(rest from-bound-enumerator-runtime-code))))

       
       ,@(when to-bound-enumerator-compiler-code
           `((defmethod get-to-bound-enumerator-code ((query ,type) ,@(first to-bound-enumerator-compiler-code))
               ,@(rest to-bound-enumerator-compiler-code))))
       ,@(when to-bound-enumerator-runtime-code
           `((defmethod evaluate-to-bound-enumerator ((query ,type) ,@(first to-bound-enumerator-runtime-code))
               ,@(rest to-bound-enumerator-runtime-code)))))))

;;:
;;;
;;;

(defvar *nrql-functions* nil)

(defvar *nrql-macros* nil)

(defvar *nrql-methods* nil)

(defvar *nrql-with-macros* nil)

(defvar *nrql-apply-delegation* nil)

(defun is-with-macro-p (x)
  (or (search "with-" (symbol-name x))
      (search "WITH-" (symbol-name x))))

(defun get-function-for-macro (macro)
  (second (assoc macro *nrql-macros*)))

(defun get-function-for-with-macro (macro)
  (second (assoc macro *nrql-with-macros*)))

;;;
;;;
;;;

#+:lispworks
(dspec:define-dspec-alias nrql-defun (name)
   `(defun ,name))

#+:lispworks
(dspec:define-form-parser (nrql-defun
                           (:parser dspec:single-form-with-options-form-parser)))

#+:lispworks
(dspec:define-dspec-alias nrql-defmethod (name &rest options)
   `(defmethod ,name ,@options))

#+:lispworks
(dspec:define-dspec-alias defmethod1 (name &rest options)
   `(defmethod ,name ,@options))

#+:lispworks
(dspec:define-form-parser nrql-defmethod (name-stuff lambda-list)
  `(,nrql-defmethod ,@(cdr (dspec:parse-form-dspec
                            `(defmethod ,(car name-stuff) ,lambda-list)))))

#+:lispworks
(dspec:define-dspec-alias nrql-defmacro (name)
   `(defmacro ,name))

#+:lispworks
(dspec:define-form-parser (nrql-defmacro
                           (:parser dspec:single-form-with-options-form-parser)))


;;;
;;;
;;;

(defun walk-body (body rest)
  (let ((res nil))
    (labels ((do-it (body)
               (when (consp body)
                 (let ((car (car body))
                       (cdr (cdr body)))
                       
                   (cond ((and (eq car 'apply)
                               (tree-find (last cdr) rest))

                          (do-it (butlast cdr))

                          (let ((fn (first cdr)))
                            (when fn
                              (cond ((and (consp fn)
                                          (or (eq (first fn) 'function)
                                              (eq (first fn) 'symbol-function)))
                                     (push fn res))
                                    (t (push fn res))))))
                         (t (do-it car)
                            (do-it cdr)))))))
      (do-it body)
      res)))

(defmacro nrql-defun ((name &key doc dont-export)
                      lambda-list 
                      &body body)

  (declare (ignorable dont-export doc))

  `(let ((rest-args (member '&rest ',lambda-list))
         (calls nil))

     (declare (ignorable rest-args calls))

     #+:nrql-dev
     (when rest-args
       (setf calls (walk-body ',body (second rest-args)))
       (when calls
         (push (list ',name calls) *nrql-apply-delegation*)))

     #+:nrql-dev
     (pushnew (list ',name ',lambda-list ',doc ',dont-export)
              *nrql-functions* 
              :test #'eq
              :key #'first)

     (defun ,name ,lambda-list ,@body)))

(defmacro nrql-defmacro ((name &key nrql-function doc auto-quote-p dont-export
                               (is-with-macro-p
                                (is-with-macro-p name))))

  (if is-with-macro-p
      `(progn 
         #+:nrql-dev
         (pushnew (list ',name ',nrql-function ',doc ',dont-export)
                  *nrql-with-macros*
                  :test #'equal
                  :key #'first)
         (defmacro ,name ((&rest args) &body body)
           (let ((fn ',nrql-function))
             `(apply (symbol-function ',fn)
                     (lambda ()
                       ,@body)
                     (if ,,auto-quote-p
                         ',args
                       (list ,@args))))))
    `(progn 
       ;;; #+:nrql-dev
       ;;; sonst funktioniert MiniLisp nicht!
       (pushnew (list ',name ',nrql-function ',doc ',dont-export)
                *nrql-macros*
                :test #'equal
                :key #'first)
       (defmacro ,name (&rest args)
         (let ((fn ',nrql-function))
           `(apply (symbol-function ',fn)
                   ',args))))))

(defmacro nrql-defmethod ((name &key doc dont-export)
                          lambda-list 
                          &body body)

  (declare (ignorable dont-export doc))

  `(let ((rest-args (member '&rest ',lambda-list))
         (calls nil))

     (declare (ignorable rest-args calls))

     #+:nrql-dev
     (when rest-args
       (setf calls (walk-body ',body (second rest-args)))
       (push (list ',name calls) *nrql-apply-delegation*))
     
     #+:nrql-dev
     (push (list ',name ',lambda-list ',doc ',dont-export)
           *nrql-methods*)

     (defmethod ,name ,lambda-list ,@body)))

(defmacro defmethod1 (name lambda-list &body body)
  `(nrql-defmethod (,name :dont-export t)
     ,lambda-list
     ,@body))

(defmacro defun1 (name lambda-list &body body)
  `(nrql-defun (,name :dont-export t)
     ,lambda-list
     ,@body))

#|
(defmacro defmethod1 (name lambda-list &body body)
 `(let ((rest-args (member '&rest ',lambda-list))
         (calls nil))
     
     (when rest-args
       (setf calls (walk-body ',body (second rest-args)))
       (let ((found (assoc ',name *nrql-apply-delegation*)))
         (cond (found
                (setf (cdr found)
                      (list 
                       (append calls
                               (second found)))))
               (t
                (push (list ',name calls) *nrql-apply-delegation*)))))
     
     (let ((found (assoc ',name *nrql-methods*)))
       (cond (found
              (setf (cdr found)
                    (list (append ',lambda-list (second found))
                          (third found))))
             (t 
              (push (list ',name ',lambda-list nil)
                    *nrql-methods*))))

     (defmethod ,name ,lambda-list ,@body)))
|#

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun reset-repository ()

    (setf *nrql-functions* nil
          *nrql-macros* nil
          *nrql-methods* nil
          *nrql-with-macros* nil
          *nrql-apply-delegation* nil)))

;;;
;;;
;;;

#+:midelora
(defmacro midelora-defun (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list ,@body)))

#+:midelora
(defmacro midelora-defmacro ((name &key nrql-function))
  (if (is-with-macro-p name)
      `(progn 
         (defmacro ,name ((&rest args) &body body)
           (let ((fn ',nrql-function))
             `(apply (symbol-function ',fn)
                     (lambda ()
                       ,@body)
                     ',args))))
    `(progn 
       (defmacro ,name (&rest args)
         (let ((fn ',nrql-function))
           `(apply (symbol-function ',fn)
                   ',args))))))

#+:midelora
(defmacro midelora-defmethod (name lambda-list &body body)
  `(progn 
     (defmethod ,name ,lambda-list ,@body)))

;;;
;;;
;;;

(defconstant +standard-specials+ 
  '(*debug-io*
    *error-output*
    *print-array*
    *print-base*
    *print-case*
    *print-circle*
    *print-escape*
    *print-gensym*
    *print-length*
    *print-level*
    *print-lines*
    *print-miser-width*
    *print-pprint-dispatch*
    *print-pretty*
    *print-readably*
    *print-radix*				   
    *print-right-margin*
    *query-io*
    *read-base*
    *standard-input*
    *standard-output*
    *terminal-io*
    ))

#+:racer-server
(defmacro save-racer-state (query)
  `(setf (slot-value ,query 'state-of-racer-specials)
         (list (mapcar #'(lambda (special)
                           (if (boundp special)
                               (symbol-value special)
                             :racer-special-is-unbound))
                       +racer-specials+)
               
               (mapcar #'(lambda (special)
                           (if (boundp special)
                               (symbol-value special)
                             :racer-special-is-unbound))
                       +standard-specials+))))

#+:lracer
(defmacro save-racer-state (query)
  (declare (ignorable query))
  t)

;;;
;;;
;;;

#+:racer-server
(defmacro with-racer-state ((query) &body body)
  `(let* ((state1 (first (state-of-racer-specials ,query)))
          (state2 (second (state-of-racer-specials ,query)))
          ,@(mapcar #'(lambda (special)
                        (list special '(pop state1)))
                    +racer-specials+)
	  ,@(mapcar #'(lambda (special)
                        (list special '(pop state2)))
                    +standard-specials+))
     
     (dolist (special +racer-specials+)
       (when (eq special :racer-special-is-unbound)
         (makunbound special)))
     
     ,@body))


#+:lracer
(defmacro with-racer-state ((query) &body body)
  (declare (ignorable query))
  `(progn
     ,@body))

;;;
;;;
;;;

#-:midelora 
(defmacro with-dl-prover-state ((query) &body body)
  `(with-racer-state (,query) ,@body))

#+:midelora 
(defmacro with-dl-prover-state ((query) &body body)
  (declare (ignore query))
  `(progn ,@body))


#-:midelora
(defmacro save-dl-prover-state (query)
  `(save-racer-state ,query))

#+:midelora
(defmacro save-dl-prover-state (query)
  (declare (ignore query))
  t)

;;;
;;; with-racer-timeout ist die "globale" Timeout-Klammer 
;;; fuer die nRQL-API-Funktionen
;;; 
;;; die einzelnen zeitintensiven (prepare-substrate1, answer-query, ...)
;;; setzen entsprechende "with-timeout-cleanup"-Forms 
;;; auf dann entsp. "Aufraeumarbeiten" auszufuehren  
;;; 

(defmacro with-timeout-cleanup-dummy (form &rest cleanup-forms)
  (declare (ignorable cleanup-forms))
  form)


#+:racer-server
(defmacro with-racer-timeout (&body body)
  `(if *server-timeout*
       (let ((*queries-started* nil))	 
	 (with-timeout (*server-timeout* 
			
			(let ((q *queries-started*)) 
			  (dolist (q q)
                            
                            (substrate-needs-reset (substrate q))
                            
                            ;; (format t "~% Started: ~A" q)
                            
			    (unless (eq (query-running-p q) :not-found)
			      (abort-query q))
			     
			    (unless (eq (rule-running-p q) :not-found)
			      (abort-rule q)))
                        
                          :timeout))
	   (let ((*timeout* nil)
		 (*server-timeout* nil))	       
	     ,@body)))
     
     (let ((*timeout* nil)
           (*server-timeout* nil)
           (*queries-started* nil))
       
       ,@body)))

#-:racer-server
(defmacro with-racer-timeout (&body body)
  `(progn 
     ,@body))


;;;
;;;
;;;

#+(or :dlmaps :lracer)
(defmacro with-timeout ((&rest args) &rest body)
  (declare (ignorable args))
  `(progn
     ,@body))


#+(or :dlmaps :lracer)
(defmacro without-timeout (&rest body)
  `(progn
     ,@body))

#+:dlmaps
(defmacro without-signature-checks (&rest body)
  `(progn
     ,@body))

#+(or :dlmaps :lracer)
(defmacro with-timeout-cleanup (form &rest cleanup-forms)
  `(restart-case
       ,form
     (abort-execution ()
       ,@cleanup-forms
       (invoke-restart 'continue-from-timeout))))

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (reset-repository))

