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
  (racer:enable-boolean-readers)
  (wilbur:enable-node-shorthand))

;;;
;;;
;;;

(defvar *swrl-create-abduction-rules-if-possible* nil)

;;;
;;;
;;;

(ts:nrql-defun (swrl-create-abduction-rules-if-possible) ()
  (setf *swrl-create-abduction-rules-if-possible* t))

(ts:nrql-defun (swrl-create-forward-chainging-rules) ()
  (setf *swrl-create-abduction-rules-if-possible* nil))

;;;
;;;
;;;

(defun process-rule (rule-name rule-ht)
  (setf (gethash rule-name rule-ht) 
        (make-swrl-rule 
         :name
         #+:ignore
         (gentemp 
          (concatenate 'string
                       (node-uri rule-name)
                       "-"))
         (or 
          (intern
           (node-uri rule-name))
          (intern
           (format nil "SWRL-RULE-~A"
                   (incf *swrl-counter*)))))))

(defun process-body (rule-name body rule-ht node-ht) 
  (let ((rule (gethash rule-name rule-ht)))
    (unless rule 
      (error "SWRL syntax error."))
    (let ((atom-list (as-atom-list body node-ht)))
      (setf (swrl-rule-body rule) atom-list))))

(defun process-head (rule-name body rule-ht node-ht)
  (let ((rule (gethash rule-name rule-ht)))
    (unless rule 
      (error "SWRL syntax error."))
    (let ((atom-list (as-atom-list body node-ht)))
      (setf (swrl-rule-head rule) atom-list))))

(defun as-atom-list (node node-ht)
  (if (node-eq node !rdf:nil)
      nil
    (let* ((attributes (get-attributes node node-ht))
           (type (get-attribute-value !rdf:type attributes))
           (car (get-attribute-value !rdf:first attributes))
           (cdr (get-attribute-value !rdf:rest attributes)))
      (cond ((node-eq type !rdf:nil)
             nil)
            ((node-eq type !swrl:ClassAtom)
             (list (process-atom car node-ht)))
            ((node-eq type !swrl:IndividualPropertyAtom)
             (list (process-atom car node-ht)))
            ((node-eq type !swrl:DatavaluedPropertyAtom)
             (list (process-atom car node-ht)))
            ((or (node-eq type !swrl:DifferentIndividualAtom)
                 (node-eq type !swrl:DifferentIndividualsAtom))  ; Protege bug
             (list (process-atom car node-ht)))
            ((node-eq type !swrl:SameIndividualAtom)
             (list (process-atom car node-ht)))
            ((node-eq type !swrl:BuiltinAtom)
             (list (process-atom car node-ht)))
            ((node-eq type !swrl:DataRangeAtom)
             (list (process-atom car node-ht)))
            ((node-eq type !swrl:AtomList)
             (cons (process-atom car node-ht)
                   (as-atom-list cdr node-ht)))
            ((and car cdr)
             (cons (as-atom-list car node-ht)  ;; Macht das Sinn?
                   (as-atom-list cdr node-ht)))
            (t (error "Malformed AtomList expression."))))))

(defun as-parameter-list (node node-ht)
  (if (node-eq node !rdf:nil)
      nil
    (let* ((attributes (get-attributes node node-ht))
           (type (get-attribute-value !rdf:type attributes))
           (car (get-attribute-value !rdf:first attributes))
           (cdr (get-attribute-value !rdf:rest attributes)))
      (cond ((node-eq type !rdf:nil)
             nil)
            ((and car cdr)
             (cons (if (literal-p car)
                       car
                     (owl-as-variable car))
                   (as-parameter-list cdr node-ht)))
            (t (error "Malformed parameter list expression."))))))

(defun process-atom (node node-ht)
  ;; when this function is called, it is still unknown which ressources
  ;; are variables and which are individuals! is handeled later! 
  (let* ((attributes (get-attributes node node-ht))
         (type (get-attribute-value !rdf:type attributes)))

    (cond ((node-eq type !swrl:ClassAtom)
           (let ((argument1 (get-attribute-value !swrl:argument1 attributes))
                 (class-predicate (get-attribute-value !swrl:classPredicate attributes)))

             (list :concept-atom
                   (owl-as-variable argument1) (owl-as-concept class-predicate node-ht))))

          ((node-eq type !swrl:IndividualPropertyAtom)
           (let ((argument1 (get-attribute-value !swrl:argument1 attributes))
                 (argument2 (get-attribute-value !swrl:argument2 attributes))
                 (property-predicate (get-attribute-value !swrl:propertyPredicate attributes))) 

             (list :role-atom 
                   (owl-as-variable argument1) 
                   (owl-as-variable argument2)
                   (owl-as-role property-predicate node-ht))))

          ((or (node-eq type !swrl:DifferentIndividualAtom)
               (node-eq type !swrl:DifferentIndividualsAtom))   ; Protege bug
           (let ((argument1 (get-attribute-value !swrl:argument1 attributes))
                 (argument2 (get-attribute-value !swrl:argument2 attributes))) 

             (list :different-from-atom
                   (owl-as-variable argument1) 
                   (owl-as-variable argument2))))

          ((node-eq type !swrl:SameIndividualAtom)
           (let ((argument1 (get-attribute-value !swrl:argument1 attributes))
                 (argument2 (get-attribute-value !swrl:argument2 attributes))) 

             (list :same-as-atom 
                   (owl-as-variable argument1) 
                   (owl-as-variable argument2))))

          ((node-eq type !swrl:DatavaluedPropertyAtom)
           (let ((argument1 (get-attribute-value !swrl:argument1 attributes))
                 (argument2 (get-attribute-value !swrl:argument2 attributes))
                 (property-predicate (get-attribute-value !swrl:propertyPredicate attributes))) 

             (list :datatype-property-atom
                   (if (literal-p argument1)
                       argument1
                     (owl-as-variable argument1) )
                   (if (literal-p argument2)
                       argument2
                     (owl-as-variable argument2) )
                   (owl-as-role property-predicate node-ht))))

          ((node-eq type !swrl:BuiltinAtom)
           (let* ((arguments (get-attribute-value !swrl:arguments attributes))
                  (parameters 
                   (as-parameter-list arguments node-ht))
                  (builtin-predicate (get-attribute-value !swrl:builtin attributes)))

             (cons :builtin-atom
                   (append parameters (list builtin-predicate)))))

          ((node-eq type !swrl:DataRangeAtom)
           (let* ((argument1 (get-attribute-value !swrl:argument1 attributes))
                  (datarange (get-attribute-value !swrl:dataRange attributes))
                  ;;(concrete-domain-type (owl-transform-datatype datarange))
                  )

             (list :datarange-atom
                   (if (literal-p argument1)
                       argument1
                     (owl-as-variable argument1))
                   (owl-as-concept datarange node-ht))))

          (t (error "Malformed Atom expression.")))))

(defun swrl-var-or-ind (var var-ht)
  (if (gethash var var-ht)
      (if *toggle-una-variables-p*
          (intern (format nil "?~A" var))
        (intern (format nil "$?~A" var)))
    var))

(defun swrl-datatype-var-or-ind (var var-ht)
  (if (gethash var var-ht)
      (if *toggle-una-variables-p*
          (intern (format nil "?*~A" var))
        (intern (format nil "$?*~A" var)))
    (intern (format nil "*~A" var))))


(defun transform-swrl-rule-body (body var-ht tbox abox)
  (declare (ignorable tbox abox))

  `(and ,@(remove nil 
                  (mapcar #'(lambda (atom)
                              (ecase (first atom)
                                (:concept-atom
                                 `(,(swrl-var-or-ind (second atom) var-ht)
                                   ,(third atom)))
                                (:role-atom
                                 `(,(swrl-var-or-ind (second atom) var-ht)
                                   ,(swrl-var-or-ind (third atom) var-ht)
                                   ,(fourth atom)))
                                (:same-as-atom
                                 `(,(swrl-var-or-ind (second atom) var-ht)
                                   ,(swrl-var-or-ind (third atom) var-ht)
                                   same-as))
                                (:different-from-atom
                                 `(,(swrl-var-or-ind (second atom) var-ht)
                                   ,(swrl-var-or-ind (third atom) var-ht)
                                   different-from))
                                (:datatype-property-atom 
                                 (let ((arg1 (second atom))
                                       (arg2 (third atom))
                                       (dtp (fourth atom)))
                                   (enable-data-substrate-mirroring)
                                   (if (literal-p arg1)
                                       (if (literal-p arg2)
                                           `(,(transform-to-cl-literal arg1)
                                             ,(transform-to-cl-literal arg2)
                                             ,dtp)
                                         `(,(transform-to-cl-literal arg1)
                                           ,(swrl-datatype-var-or-ind arg2 var-ht) 
                                           ,dtp))
                                     (if (literal-p arg2)
                                         `(,(swrl-datatype-var-or-ind arg1 var-ht) 
                                           ,(transform-to-cl-literal arg2)
                                           ,dtp)
                                       `(,(swrl-datatype-var-or-ind arg1 var-ht) 
                                         ,(swrl-datatype-var-or-ind arg2 var-ht)
                                         ,dtp)))))
                                (:builtin-atom
                                 (let* ((atom (rest atom))
                                        (args (butlast atom))
                                        (pred (first (last atom))))
                                   (enable-data-substrate-mirroring)
                                   (multiple-value-bind (pred arity)
                                       (transform-swrl-predicate pred)
                                     (declare (ignorable arity))
                                     (let ((args 
                                            (mapcar #'(lambda (arg)
                                                        (if (literal-p arg)
                                                            (transform-to-cl-literal arg)
                                                          (swrl-datatype-var-or-ind arg var-ht)))
                                                    args)))
                                       `(lambda (,pred ,@args))))))))
                          body))))

(defun transform-swrl-rule-head (head var-ht tbox abox)
  (declare (ignorable tbox))

  (if (or (cdr head) ; non-Horn? 
          (not *swrl-create-abduction-rules-if-possible*))
                    
      (list 

       (remove nil 
               (loop for head in head
                     unless (null head)
                     collect 
                     (ecase (first head)
                       (:concept-atom
                        `(instance ,(swrl-var-or-ind (second head) var-ht)
                                   ,(third head)))
                       (:role-atom
                        `(related ,(swrl-var-or-ind (second head) var-ht)
                                  ,(swrl-var-or-ind (third head) var-ht)
                                  ,(fourth head)))
                       (:same-as-atom
                        `(same-as ,(swrl-var-or-ind (second head) var-ht)
                                  ,(swrl-var-or-ind (third head) var-ht)))
                       (:different-from-atom
                        `(different-from ,(swrl-var-or-ind (second head) var-ht)
                                         ,(swrl-var-or-ind (third head) var-ht)))
                       (:datatype-property-atom
                        (let ((arg1 (second head))
                              (arg2 (third head))
                              (dtp (fourth head)))
                          (unless (literal-p arg1)
                            (enable-data-substrate-mirroring)
                            `(lambda 
                                     (defer 
                                      (add-datatype-role-filler 
                                       (quote ,(abox-name abox))
                                       ,(swrl-var-or-ind arg1 var-ht)
                                       ,(if (literal-p arg2)
                                            (transform-to-cl-literal arg2)
                                          (swrl-datatype-var-or-ind arg2 var-ht))
                                       (quote ,dtp)))))))
                     (:datarange-atom)
                     (:builtin-atom))))

       ;; non-Horn:
       t)

    (let ((head (first head)))
      
      (list 

       (ecase (first head)
         (:concept-atom
          `(,(swrl-var-or-ind (second head) var-ht)
            ,(third head)))
         (:role-atom
          `(,(swrl-var-or-ind (second head) var-ht)
            ,(swrl-var-or-ind (third head) var-ht)
            ,(fourth head)))
         (:same-as-atom
          `(,(swrl-var-or-ind (second head) var-ht)
            ,(swrl-var-or-ind (third head) var-ht)
            same-as))
         (:different-from-atom
          `(,(swrl-var-or-ind (second head) var-ht)
            ,(swrl-var-or-ind (third head) var-ht)
            different-from))
         (:datatype-property-atom)
         (:datarange-atom)
         (:builtin-atom))

       nil))))


(defun build-swrl-rules (tbox abox var-ht) 
  (handler-case 

      (loop for rule in (abox-rules abox) 

	  for rule-name = (swrl-rule-name rule)  
	  for rule-body = (transform-swrl-rule-body (swrl-rule-body rule) var-ht tbox abox)
	  for (rule-head non-horn-p) = (transform-swrl-rule-head (swrl-rule-head rule) var-ht tbox abox)
				       
	  do
            
	    (when (and rule-body rule-head)
	      
	      (if non-horn-p
		    
		  (racer-prepare-rule rule-body
				      rule-head
				      :abox (abox-name abox)
				      :id rule-name)

		(add-rule-axiom (abox-name abox) 
				rule-head
				rule-body
				:id rule-name))))

    (error (error)
      (nrql-warning "Bad SWRL rule ignored: ~A" error))))


;;;
;;;
;;;

(defun transform-swrl-predicate (pred)
  (ecase pred
    
    (!swrlb:equal (values '(lambda (x y) (and (numberp x)
                                              (numberp y)
                                              (= x y)))
                          2))
    (!swrlb:notEqual (values '(lambda (x y) (and (numberp x) 
                                                 (numberp y)
                                                 (/= x y)))
                             2))
    (!swrlb:lessThan (values '(lambda (x y) (and (numberp x) 
                                                 (numberp y) 
                                                 (< x y)))
                             2))
    (!swrlb:lessThanOrEqual (values '(lambda (x y) (and (numberp x)
                                                        (numberp y) 
                                                        (<= x y)))
                                    2))
    (!swrlb:greaterThan (values '(lambda (x y) (and (numberp x) 
                                                    (numberp y)
                                                    (> x y)))
                                2))
    (!swrlb:greaterThanOrEqual (values '(lambda (x y) (and (numberp x)
                                                           (numberp y)
                                                           (>= x y)))
                                       2))

    (!swrlb:add (values '(lambda (x y z) (and (numberp x)
                                              (numberp y)
                                              (numberp z)
                                              (= x (+ y z))))
                        3))
    (!swrlb:subtract (values '(lambda (x y z) (and (numberp x)
                                                   (numberp y)
                                                   (numberp z)
                                                   (= x (- y z))))
                             3))
    (!swrlb:multiply (values '(lambda (x y z) (and (numberp x) 
                                                   (numberp y)
                                                   (numberp z)
                                                   (= x (* y z))))
                             3))
    (!swrlb:divide (values '(lambda (x y z) (and (numberp x)
                                                 (numberp y)
                                                 (numberp z)
                                                 (= x (/ y z))))
                           3))
    (!swrlb:integerDivide (values '(lambda (x y z) (and (numberp x)
                                                        (numberp y)
                                                        (numberp z)
                                                        (= x (floor y z))))
                                  3))
    (!swrlb:mod (values '(lambda (x y z) (and (numberp x)
                                              (numberp y)
                                              (numberp z)
                                              (= x (mod y z))))
                        3))
    (!swrlb:pow (values '(lambda (x y z) (and (numberp x)
                                              (numberp y)
                                              (numberp z)
                                              (= x (expt y z))))
                        3))
    (!swrlb:unaryPlus (values '(lambda (x y) (and (numberp x)
                                                  (numberp y)
                                                  (= x y))) 
                              2))    
    (!swrlb:unaryMinus (values '(lambda (x y) (and (numberp x)
                                                   (numberp y)
                                                   (= x (- y))))
                               2))
    (!swrlb:abs (values '(lambda (x y) (and (numberp x) 
                                            (numberp y)
                                            (= x (abs y)))) 
                        2))
    (!swrlb:ceiling (values '(lambda (x y) (and (numberp x) 
                                                (numberp y) 
                                                (= x (ceiling y))))
                            2))
    (!swrlb:floor (values '(lambda (x y) (and (numberp x) 
                                              (numberp y) 
                                              (= x (floor y))))
                          2))
    (!swrlb:round (values '(lambda (x y) (and (numberp x) 
                                              (numberp y)
                                              (= x (round y))))
                          2))
    (!swrlb:sin (values '(lambda (x y) (and (numberp x) 
                                            (numberp y)
                                            (= x (sin y))))
                        2))
    (!swrlb:cos (values '(lambda (x y) (and (numberp x)
                                            (numberp y)
                                            (= x (cos y))))
                        2))
    (!swrlb:tan (values '(lambda (x y) (and (numberp x)
                                            (numberp y)
                                            (= x (tan y))))
                        2))
    
    (!swrlb:booleanNot (values '(lambda (x y) (or (and x (not y))
                                                  (and (not x) y))) 2))

    (!swrlb:stringEqualIgnoreCase (values '(lambda (x y) (and (stringp x)
                                                              (stringp y)
                                                              (string-equal x y)))
                                          2))
    (!swrlb:stringConcat (values '(lambda (x y z) (and (stringp x)
                                                       (stringp y)
                                                       (stringp z)
                                                       (string= x (concatenate y z))))
                                 3))
    (!swrlb:contains (values '(lambda (x y) (and (stringp x)
                                                 (stringp y)
                                                 (search x y)))
                             2))
    (!swrlb:stringLength (values '(lambda (x y) (and (numberp x)
                                                     (stringp y)
                                                     (= x (length y))))
                                 2))
    ; normalizeSpace
    ; substring
    (!swrlb:upperCase (values '(lambda (x y) (and (stringp x)
                                                  (stringp y)
                                                  (string= x (string-upcase y))))
                              2))
    (!swrlb:lowerCase (values '(lambda (x y) (and (stringp x)
                                                  (stringp y)
                                                  (string= x (string-downcase y))))
                              2))
    (!swrlb:containsIgnoreCase (values '(lambda (x y) (and (stringp x)
                                                           (stringp y)
                                                           (search (string-upcase x) (string-upcase y))))
                                       2))
    (!swrlb:startsWith (values '(lambda (x y) (and (stringp x) 
                                                   (stringp y)
                                                   (zerop (search y x))))
                               2))
    (!swrlb:endsWith (values '(lambda (x y) (and (stringp x)
                                                 (stringp y)
                                                 (= (- (length x)
                                                       (length y))
                                                    (search y x))))
                             2))
    (!swrlb:substringBefore (values '(lambda (x y z)
                                       (and (stringp x)
                                            (stringp y)
                                            (stringp z) 
                                            (let ((pos (search z y)))
                                              (when pos
                                                (string= 
                                                 x
                                                 (subseq y 0 pos))))))
                                    2))
    (!swrlb:substringAfter (values '(lambda (x y z)
                                      (and (stringp x)
                                           (stringp y)
                                           (stringp z)
                                           (let ((pos (search z y)))
                                             (when pos
                                               (string= 
                                                x
                                                (subseq y (+ (length z pos))))))))
                                   2))
     ; matches
     ; replace
     ; tokenize
     
    ))
    

(defun transform-to-cl-literal (lit)
  (let ((lit (literal-string lit))
        (type (literal-datatype lit)))
    (ecase type
      ((or !xsd:int !xsd:integer integer)
       (parse-integer lit))
      (boolean
       (if (string-equal lit "true")
           #t 
         #f))
      (string 
       lit)
      (real
       (read-from-string lit)))))

;;;
;;;
;;;

(defun swrl-variable-p (expr var-ht)
  (gethash expr var-ht))


(defun statement-entailed-p (statement abox)
  (ecase (first statement)
    (instance (individual-instance-p (second statement) (third statement) abox))
    (related (internal-individuals-related-p (second statement) (third statement) (fourth statement) abox))))


(defun swrl-forward-chaining (&key (abox (current-abox))
                                   (verbose *abox-verbose*)
                                   (delete-rules nil))
  (check-type abox (or abox symbol))
  (setf abox (find-abox abox))
  (unless (subsetp (mapcar #'swrl-rule-name (abox-rules abox)) (all-rules))
    (error "At least some of rules of ABox ~A are deleted from nRQL engine." (abox-name abox)))
  (unwind-protect
      (progn 
        (enable-data-substrate-mirroring)
        ;; (process-tuple-at-a-time)
        (loop with stop = nil
              for applicable-rules = (applicable-rules)
              until (or stop (null applicable-rules)) do
              (setf stop t)
              (loop for rule in applicable-rules do
                    (execute-rule rule)
                    (loop for consequences = (get-next-set-of-rule-consequences rule)
                          until (eq consequences :exhausted)
                          do 
                          (unless (or (member consequences
                                              '(:exhausted :timeout
                                                :denied-due-to-deadlock-prevention
                                                :warning-kb-has-changed 
                                                :warning-expensive-phase-two-starts))
                                      #+:ignore
                                      (loop for consequence in consequences                                     
                                            always (statement-entailed-p consequence abox)))
                            (when verbose
                              (loop for consequence in consequences do 
                                    (format t "~%Execute ~S:~%~S~%"
                                            rule
                                            consequence)))
                            (choose-current-set-of-rule-consequences rule)
                            (setf stop nil))))))
    (disable-data-substrate-mirroring))
  (when delete-rules
    (loop for rule in (abox-rules abox) do
          (delete-rule (swrl-rule-name rule)))))


;;;
;;;
;;;

#| 

(progn 

  (full-reset)

  (owl-read-file "nrql:test-cases;swrl-test.owl")

  (pprint (describe-all-rules))

  (dotimes (i 10) 
    (pprint (execute-or-reexecute-all-rules)))

  (full-reset)

  (owl-read-file "nrql:test-cases;swrl-test2.owl")

  (pprint (describe-all-rules))

  (dotimes (i 10) 
    (pprint (execute-or-reexecute-all-rules))))

|# 
