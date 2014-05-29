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

(defstruct (concrete-domain (:conc-name cd-))
  (name nil)
  (objects nil)
  (domain-predicate nil)
  (predicates (make-hash-table))
  (supported-types '(cardinal integer real complex string))
  (registered-predicate-forms (racer-make-hash-table :test 'equal)))

(defun create-concrete-domain ()
  (make-concrete-domain :name 'racer-default-cd
                        :domain-predicate 'concrete-domain-object-p))



;;; ======================================================================

(defstruct (predicate
            (:include racer-structure-id))
  (name nil)
  (definition nil)
  (parameters nil)
  (operator nil)
  (arity nil)
  (negation nil)
  (type nil)
  )

(defmethod print-object ((object predicate) stream)
  (cond
   ((predicate-definition object)
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~S" (predicate-definition object))))
   ((predicate-definition (predicate-negation object))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "(NEG ~S)" (predicate-definition (predicate-negation object)))))
   (t (print-unreadable-object (object stream :type t :identity nil)
        (format stream "~S" (predicate-name object))))))

(defstruct (linear-predicate (:include predicate)
                             ;(:print-function print-predicate)
                             (:conc-name predicate-))
  (coefficients nil)
  (right-side nil))

(defvar *cd-debug* nil)

(defun print-predicate (predicate stream depth)
  ;; Ausgabefunktion fuer Objekte vom Typ predicate
  (declare (ignore depth))
  (if *cd-debug*
    (let ((name (predicate-name predicate))
	  (coefficients (predicate-coefficients predicate))
	  (right-side (predicate-right-side predicate))
	  (operator (predicate-operator predicate))
	  (negation (predicate-negation predicate)))
      (format stream "<#predicate: ~A:  " name)
      (loop for coeff across coefficients
	    and n from 1 do
	    (if (eql n 1)
	      (format stream "~A*x_~A " coeff n)
	      (format stream "+ ~A*x_~A " coeff n)))
      (format stream "~A ~A~%             Negation: ~A>" 
	      operator right-side (predicate-name negation)))
    (print-unreadable-object (predicate stream :type t :identity t)
      (format stream "Lin:~A" (predicate-name predicate)))))

(defstruct (nonlinear-predicate (:include predicate)
                                (:conc-name predicate-))
  )



(defstruct (equal-unequal-predicate (:include linear-predicate) (:conc-name predicate-))
  predicate-1
  predicate-2)

(defstruct (equal-predicate (:include equal-unequal-predicate)))
                            
(defstruct (unequal-predicate (:include equal-unequal-predicate)))


(defstruct (divisible-predicate (:include predicate)
                                (:conc-name predicate-)))

(defstruct (string-predicate (:include predicate)
                             (:conc-name predicate-)))

(defstruct (unary-string-predicate (:include string-predicate)
                                   (:conc-name predicate-))
  string)

(defstruct (binary-string-predicate (:include string-predicate)
                                    (:conc-name predicate-)))



;;; ======================================================================

(defstruct cd-constraint
  (varlist nil)
  (predicate nil)
  (negated-p nil))

(defmethod print-object ((object cd-constraint) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" 
            (if (cd-constraint-negated-p object)
                (predicate-negation (cd-constraint-predicate object))
              (cd-constraint-predicate object))
            (cd-constraint-varlist object))))

(defun make-constraint (predicate args &optional (negated nil))
  (make-cd-constraint :predicate predicate
		      :varlist args
		      :negated-p negated))


;;; ======================================================================



(defun parse-linear-constraint-body (concrete-domain parameters expr arity)
  (declare (ignore concrete-domain))
  ; expr hat folgende Struktur:
  ; expr    = (op arg arg)
  ; op      = < | <= | > | >=
  ; arg     = real | symbol | product | sum
  ; product = (* real symbol)
  ; sum     = (+ {real | symbol | product}*)
  ; > wird in < und >= in <= umgewandelt, durch Multiplikation mit -1
  (let (;(expr (first body)); falls body weitere expressions enthaelt werden diese noch ignoriert!
	(par (make-hash-table))
	(coeff (make-array arity :initial-element 0))
	(rs 0))
    (flet ((proper-var (var fac)
	     (let ((par-pos (gethash var par)))
	       ;(format t "var: ~A, fac: ~A, pos: ~A~%" var fac par-pos)
	       (if (and (symbolp var) par-pos)
		   (setf (svref coeff par-pos) 
		         (+ (svref coeff par-pos) (rationalize fac)))
		 (error "variable not in parameter-list")))))
      (flet ((parse-arg (arg fac)
	       (cond ((realp arg) 
                      (setf rs (- rs (* (rationalize fac)
                                        (rationalize arg)))))
		     ((and (listp arg)
			   (eql (length arg) 3)
			   (eql '* (first arg))
			   (realp (second arg))
			   (proper-var (third arg) (* (rationalize fac)
                                                      (rationalize 
                                                       (second arg))))))
		     ((and (listp arg)
			   (eql '+ (first arg)))
		      (dolist (a (rest arg))
			(cond ((realp a) 
                               (setf rs (- rs (* (rationalize fac) 
                                                 (rationalize a)))))
			      ((and (listp a)
				    (eql (length a) 3)
				    (eql '* (first a))
				    (realp (second a))
				    (proper-var (third a) (* (rationalize fac)
                                                             (rationalize 
                                                              (second a))))))
			      ((not (listp a)) (proper-var a fac))
			      (t (error "Syntax error: ~A" expr)))))
		     ((not (listp arg)) (proper-var arg fac))
		     (t (error "incorrect first argument")))))
	; paremeters mit position in Hash-tabelle par ablegen:
	(loop for p in parameters
	    and i from 0 below arity 
	    do
	      (setf (gethash p par) i)) 
	(if (not (and (listp expr) 
		      (eql (length expr) 3) 
		      (member (first expr) '(< <= > >=))))
	    (error "Syntax error1: ~A" expr))
	(let ((op (first expr))
	      (arg1 (second expr))
	      (arg2 (third expr)))
	  (if (or (eql op '<) (eql op '<=))
	      (progn
		(parse-arg arg1 1)
		(parse-arg arg2 -1))
	    (progn
		(parse-arg arg1 -1)
		(parse-arg arg2 1)
		(setf op (case op (> '<) (>= '<=)))))
	  (values op coeff rs))))
    ))



;;; ======================================================================

(defun add-special-predicate (op definition parameters
                                     name neg-name concrete-domain datatype)
  (let* ((pred (make-predicate :name name
                               :definition definition
                               :parameters parameters
                               :operator op
                               :arity 1
                               :negation neg-name
                               :type datatype
                               ))
         ; negiertes Praedikat ezeugen:
         (neg-op (case op
                   (top 'bottom)
                   (bottom 'top)
                   (otherwise (error "wrong operator: only TOP or BOTTOM allowed"))))
         (neg-pred (make-predicate :name neg-name
                                   :definition (if (eq neg-op 'top)
                                                 `(a . ,(rest definition))
                                                 `(no . ,(rest definition)))
                                   :parameters parameters
                                   :operator neg-op
                                   :arity 1
                                   :negation pred
                                   :type datatype
                                   ))
         (forms-table (cd-registered-predicate-forms concrete-domain)))
    (setf (gethash neg-name (cd-predicates concrete-domain)) neg-pred)
    (setf (gethash name (cd-predicates concrete-domain)) pred)
    (setf (gethash definition forms-table) pred)
    (setf (gethash (predicate-definition neg-pred) forms-table) neg-pred)
    (setf (predicate-negation pred) neg-pred)
    pred))

(defun add-integer-predicate (op definition parameters
                                     name neg-name concrete-domain)
  (unless (symbolp (second definition))
    (error "concrete domain attribute ~S is not a name in ~S."
           (second definition) definition))
  (if (third definition)
    (unless (integerp (third definition))
      (error "~S in ~S must be an integer." (third definition) definition))
    (error "A number is missing in ~S." definition))
  (let ((form (ecase op
                (min (list '>= (second definition) (third definition)))
                (max (list '<= (second definition) (third definition))))))
    (unless form
      (error "concrete domain operator MIN or MAX expected in ~S." definition))
    (multiple-value-bind (op1 coeff rs) 
                         (parse-linear-constraint-body 
                          concrete-domain parameters form 1)
      (let ((predicate (make-linear-predicate :name name
                                              :definition definition
                                              :parameters parameters
			                      :coefficients coeff
			                      :right-side rs
			                      :operator op1
			                      :arity 1
			                      :negation neg-name
                                              :type 'integer
			                      )))
        ; negiertes Praedikat ezeugen:
        (let ((neg-form (ecase op
		          (min (list '<= (second form) (- (third form) 1)))
		          (max (list '>= (second form) (+ (third form) 1))))))
          (multiple-value-bind (neg-op neg-coeff neg-rs) 
                               (parse-linear-constraint-body 
                                concrete-domain parameters neg-form 1) 
            (let ((neg-predicate
                   (make-linear-predicate :name neg-name
                                          :definition
                                          (if (eq op 'min)
                                            `(max ,(second definition) ,(1- (third definition)))
                                            `(min ,(second definition) ,(1+ (third definition))))
                                          :parameters parameters
                                          :coefficients neg-coeff
                                          :right-side neg-rs
                                          :operator neg-op
                                          :arity 1
                                          :negation predicate
                                          :type 'integer))
                  (forms-table (cd-registered-predicate-forms concrete-domain)))
              (setf (gethash neg-name (cd-predicates concrete-domain)) 
                    neg-predicate)
              (setf (gethash name (cd-predicates concrete-domain)) 
                    predicate)
              (setf (gethash definition forms-table) predicate)
              (setf (gethash (predicate-definition neg-predicate) forms-table) neg-predicate)
              (setf (predicate-negation predicate) neg-predicate))))
        predicate))))



;;; ======================================================================

(defun add-linear-predicate (name neg-name parameters form concrete-domain)
  (let ((arity (length parameters)))
    (cond ((or (eq (first form) '=) (eq (first form) 'equal))

           (let* ((predicate-1 (add-linear-predicate name neg-name parameters 
                                                     (cons '>= (rest form)) concrete-domain))
                  (predicate-2 (add-linear-predicate name neg-name parameters 
                                                     (cons '<= (rest form)) concrete-domain))
                  (predicate
                   (make-equal-predicate :name name
                                         :predicate-1 predicate-1 
                                         :predicate-2 predicate-2
                                         :type (predicate-type predicate-1)
                                         :parameters parameters
                                         :definition form
                                         :arity arity
                                         :operator (if (eq (first form) 'equal)
                                                     'equal 
                                                     '=))))
             (let* ((neg-predicate-1 (add-linear-predicate name neg-name parameters 
                                                           (cons '> (rest form)) concrete-domain))
                    (neg-predicate-2 (add-linear-predicate name neg-name parameters 
                                                           (cons '< (rest form)) concrete-domain))
                    (neg-pred (make-unequal-predicate :name neg-name
			                              :predicate-1 neg-predicate-1 
                                                      :predicate-2 neg-predicate-2
                                                      :parameters parameters
			                              :negation predicate
                                                      :definition (cons (if (eq (first form) 'equal)
                                                                          'unequal
                                                                          '<>) 
                                                                        (rest form))
                                                      :type (predicate-type neg-predicate-1)
                                                      :arity arity
                                                      :operator (if (eq (first form) 'equal)
                                                                  'unequal
                                                                  '<>)))
                    (forms-table (cd-registered-predicate-forms concrete-domain)))
               (setf (gethash neg-name (cd-predicates concrete-domain)) neg-pred)
               (setf (gethash name (cd-predicates concrete-domain)) predicate)
               (setf (predicate-negation predicate) neg-pred)
               (setf (gethash form forms-table) predicate)
               (setf (gethash (predicate-definition neg-pred) forms-table) neg-pred)
               (when (eq (first form) 'equal)
                 ;; THIS IS OK BECAUSE INTEGER PREDICATES MIN/MAX
                 ;; ARE HANDLED IN THE SAME WAY AS REAL PREDICATES
                 (setf (predicate-type predicate) 'integer)
                 (setf (predicate-type neg-pred) 'integer))
               predicate)))
          ((or (eq (first form) '<>) (eq (first form) 'unequal))
           (let* ((predicate-1 (add-linear-predicate name neg-name parameters 
                                                     (cons '> (rest form)) concrete-domain))
                  (predicate-2 (add-linear-predicate name neg-name parameters 
                                                     (cons '< (rest form)) concrete-domain))
                  (predicate
                   (make-unequal-predicate :name name
                                           :predicate-1 predicate-1 
                                           :predicate-2 predicate-2
                                           :type (predicate-type predicate-1)
                                           :parameters parameters
                                           :definition form
                                           :arity arity
                                           :operator (if (eq (first form) 'unequal)
                                                       'unequal
                                                       '<>))))
             (let* ((neg-predicate-1 (add-linear-predicate name neg-name parameters 
                                                           (cons '>= (rest form)) concrete-domain))
                    (neg-predicate-2 (add-linear-predicate name neg-name parameters 
                                                           (cons '<= (rest form)) concrete-domain))
                    (neg-pred (make-equal-predicate :name neg-name
                                                    :predicate-1 neg-predicate-1 
                                                    :predicate-2 neg-predicate-2
                                                    :parameters parameters
                                                    :definition (cons (if (eq (first form) 'unequal)
                                                                        'equal
                                                                        '=)
                                                                      (rest form))
                                                    :negation predicate
                                                    :type (predicate-type neg-predicate-1)
                                                    :arity arity
                                                    :operator (if (eq (first form) 'unequal)
                                                                'equal
                                                                '=)))
                    (forms-table (cd-registered-predicate-forms concrete-domain)))
               (setf (gethash neg-name (cd-predicates concrete-domain)) neg-pred)
               (setf (gethash name (cd-predicates concrete-domain)) predicate)
               (setf (predicate-negation predicate) neg-pred)
               (setf (gethash form forms-table) predicate)
               (setf (gethash (predicate-definition neg-pred) forms-table) neg-pred)
               (when (eq (first form) 'unequal)
                 ;; THIS IS OK BECAUSE INTEGER PREDICATES MIN/MAX
                 ;; ARE HANDLED IN THE SAME WAY AS REAL PREDICATES
                 (setf (predicate-type predicate) 'integer)
                 (setf (predicate-type neg-pred) 'integer))  
               predicate)))
          (t
           (multiple-value-bind (op coeff rs) 
                                (parse-linear-constraint-body 
                                 concrete-domain parameters form arity)
             (let ((predicate
                    (add-linear-predicate-definition form parameters
                                                     name neg-name arity op coeff rs 
                                                     concrete-domain)))
               (when (loop for parameter-name in (predicate-parameters predicate)
                           for role = (get-role parameter-name)
                           thereis
                           (and role (eq (role-cd-attribute role) 'cardinal)))
                 ;; HMM... CONVERTING THE PREDICATE TYPE TO CARDINALS WORKS ONLY FOR ROLES 
                 ;; BUT NOT FOR OBJECT-NAMES!!! So we have to deal with object names
                 ;; explicitly in the Abox code.
                 (unless (loop for parameter-name in parameters 
                               for role = (encode-role-term parameter-name :cd-attribute 'cardinal)
                               always
                               (eq (role-cd-attribute role) 'cardinal))
                   (error "All attributes must be cardinals in ~A." form))
                 ;(break "~S" predicate)
                 (setf (predicate-type predicate) 'cardinal))
               predicate))))))


(defun add-nonlinear-predicate (name neg-name parameters form concrete-domain)
  (let ((arity (length parameters)))
    (let ((predicate
           (add-nonlinear-predicate-definition form parameters
                                            name neg-name arity (first form) 
                                            concrete-domain)))
      predicate)))

(defun negated-cd-operator (operator)
  (ecase operator
    (< '>=)
    (<= '>)
    (> '<=)
    (>= '<)
    (= '<>)
    (equal 'unequal)
    (unequal 'equal)
    (<> '=)
    (string= 'string<>)
    (string<> 'string=)
    (boolean= 'boolean<>)
    (boolean<> 'boolean=)
    (a 'no)
    (no 'a)
    (divisible 'not-divisible)
    (not-divisible 'divisible)))

(defun add-linear-predicate-definition (definition parameters
                                               pred-name neg-name arity 
                                               op coeff rs concrete-domain)
  (or (gethash definition (cd-registered-predicate-forms concrete-domain))
      (let ((pred (make-linear-predicate :name pred-name
                                         :definition definition
                                         :parameters parameters
			                 :coefficients coeff
			                 :right-side rs
			                 :operator op
			                 :arity arity
			                 :negation neg-name
                                         :type 'real)))
        ; negiertes Praedikat ezeugen:
        (let ((neg-op (ecase op
		        (< '<=)
		        (<= '<) ; !!! DIESES KANN MAN NICHT VERSTEHEN!!! AENDERUNGEN SIND FATAL!!!
                        ;; Ich habe es dann doch noch verstanden: Wenn das Praedikat den Operator > (bzw. >=) verwendet,
                        ;; wird implizit mit -1 multipliziert und damit ist op dann < (bzw. <=)
                        ;; Deshalb braucht man die Faelle > und >= hier nicht zu behandeln.
                        ;; Vor dem Negieren wird dann noch mit -1 multipliziert. Daher steht eigentlich > (bzw. >=)
                        ;; an Stelle von < (bzw. >=). Und und wird wiederum klar, warum neg-or den Wert < (bzw. <=)
                        ;; bekommen muss (rm -- 26.5.03)
                        ;(= '<>)
                        ;(<> '=)
                        ))
	      (neg-coeff (make-array (array-dimension coeff 0))))
          ; alle Koeffizienten negieren
          (loop for c across coeff 
	        and pos from 0 do
	        (setf (aref neg-coeff pos) (- c)))
          (let ((neg-pred (make-linear-predicate :name neg-name
                                                 :definition
                                                 (list* (negated-cd-operator (first definition))
                                                        (rest definition))
			                         :parameters parameters
                                                 :coefficients neg-coeff
			                         :right-side (- rs)
			                         :operator neg-op
			                         :arity arity
			                         :negation pred
                                                 :type 'real))
                (forms-table (cd-registered-predicate-forms concrete-domain)))
            (setf (gethash neg-name (cd-predicates concrete-domain)) neg-pred)
            (setf (gethash pred-name (cd-predicates concrete-domain)) pred)
            (setf (predicate-negation pred) neg-pred)
            (setf (gethash definition forms-table) pred)
            (setf (gethash (predicate-definition neg-pred) forms-table) neg-pred)
            pred)))))

(defun add-nonlinear-predicate-definition (definition parameters
                                                  pred-name neg-name arity 
                                                  op concrete-domain)
  (or (gethash definition (cd-registered-predicate-forms concrete-domain))
      (let ((pred (make-nonlinear-predicate :name pred-name
                                            :definition definition
                                            :parameters parameters
			                    :operator op
			                    :arity arity
			                    :negation neg-name
                                            :type 'complex)))
        ; negiertes Praedikat ezeugen:
        (let ((neg-op (case op
                        (= '<>)
                        (<> '=)
		        ;(< '<=)
		        ;(<= '<)
		        ;(> '<=)
		        ;(>= '<)
		        (otherwise (error "Wrong operator: only = supported for nonlinear predicates.")))))
          (let ((neg-pred (make-nonlinear-predicate :name neg-name
			                            :parameters parameters
                                                    :operator neg-op
			                            :arity arity
			                            :negation pred
                                                    :type 'complex))
                (forms-table (cd-registered-predicate-forms concrete-domain)))
            (setf (gethash neg-name (cd-predicates concrete-domain)) neg-pred)
            (setf (gethash pred-name (cd-predicates concrete-domain)) pred)
            (setf (predicate-negation pred) neg-pred)
            (setf (gethash definition forms-table) pred)
            (setf (gethash (predicate-definition neg-pred) forms-table) neg-pred)
            pred)))))

(defun add-divisible-predicate (name neg-name parameters form concrete-domain)
  (let ((arity (length parameters)))
    (cond ((eq (first form) 'divisible)

           (let* ((predicate
                   (make-divisible-predicate :name name
                                             :type 'cardinal
                                             :parameters parameters
                                             :definition form
                                             :arity arity
                                             :operator 'divisible)))
             (let* ((neg-pred (make-divisible-predicate :name neg-name
                                                        :parameters parameters
                                                        :negation predicate
                                                        :definition (cons 'not-divisible (rest form))
                                                        :type 'cardinal
                                                        :arity arity
                                                        :operator 'not-divisible))
                    (forms-table (cd-registered-predicate-forms concrete-domain)))
               (setf (gethash neg-name (cd-predicates concrete-domain)) neg-pred)
               (setf (gethash name (cd-predicates concrete-domain)) predicate)
               (setf (predicate-negation predicate) neg-pred)
               (setf (gethash form forms-table) predicate)
               (setf (gethash (predicate-definition neg-pred) forms-table) neg-pred)
               predicate)))
          ((eq (first form) 'not-divisible)
           (let* ((predicate
                   (make-divisible-predicate :name name
                                                 :type 'cardinal
                                                 :parameters parameters
                                                 :definition form
                                                 :arity arity
                                                 :operator 'not-divisible)))
             (let* ((neg-pred (make-divisible-predicate :name neg-name
                                                        :parameters parameters
                                                        :definition (cons 'divisible (rest form))
                                                        :negation predicate
                                                        :type 'cardinal
                                                        :arity arity
                                                        :operator 'divisible))
                    (forms-table (cd-registered-predicate-forms concrete-domain)))
               (setf (gethash neg-name (cd-predicates concrete-domain)) neg-pred)
               (setf (gethash name (cd-predicates concrete-domain)) predicate)
               (setf (predicate-negation predicate) neg-pred)
               (setf (gethash form forms-table) predicate)
               (setf (gethash (predicate-definition neg-pred) forms-table) neg-pred)
                predicate)))
          (t
           (error "Internal error: unknown divisible predicate. This should not happen.")))))

(defun add-string-predicate (name neg-name parameters form concrete-domain)
  (or (gethash form (cd-registered-predicate-forms concrete-domain))
      (let ((arity (length parameters)))
        (cond ((stringp (third form))
               (let* ((pred (make-unary-string-predicate :name name
                                                         :parameters parameters
                                                         :definition form
                                                         :type 'string
                                                         :string (third form)
                                                         :arity arity
                                                         :operator (first form)))
                      (neg-op (ecase (first form)
                                (string= 'string<>)
                                (string<> 'string=)))
                      (neg-pred (make-unary-string-predicate :name neg-name
                                                             :parameters parameters
                                                             :definition
                                                             (list* neg-op (rest form))
                                                             :operator neg-op
                                                             :arity arity
                                                             :negation pred
                                                             :string (third form)
                                                             :type 'string))
                      (forms-table (cd-registered-predicate-forms concrete-domain)))
                 (setf (gethash neg-name (cd-predicates concrete-domain)) neg-pred)
                 (setf (gethash name (cd-predicates concrete-domain)) pred)
                 (setf (predicate-negation pred) neg-pred)
                 (setf (gethash form forms-table) pred)
                 (setf (gethash (predicate-definition neg-pred) forms-table) neg-pred)
                 pred))
              (t
               (let* ((pred (make-binary-string-predicate :name name
                                                          :parameters parameters
                                                          :definition form
                                                          :type 'string
                                                          :arity arity
                                                          :operator (first form)))
                      (neg-op (ecase (first form)
                                (string= 'string<>)
                                (string<> 'string=)))
                      (neg-pred (make-binary-string-predicate :name neg-name
                                                              :parameters parameters
                                                              :definition
                                                              (list* neg-op (rest form))
                                                              :operator neg-op
                                                              :arity arity
                                                              :negation pred
                                                              :type 'string))
                      (forms-table (cd-registered-predicate-forms concrete-domain)))
                 (setf (gethash neg-name (cd-predicates concrete-domain)) neg-pred)
                 (setf (gethash name (cd-predicates concrete-domain)) pred)
                 (setf (predicate-negation pred) neg-pred)
                 (setf (gethash form forms-table) pred)
                 (setf (gethash (predicate-definition neg-pred) forms-table) neg-pred)
                 pred))))))




;;; ======================================================================

(defstruct (racer-boolean 
            (:predicate racer-boolean-p)
            (:conc-name boolean-))
  value)

(defmethod print-object ((object racer-boolean) stream)
  (declare (ignorable stream))
  (if (string= (boolean-value object) "false")
    (write-string "#F" stream)
    (write-string "#T" stream)))

(defparameter *true* (make-racer-boolean :value "true"))
(defparameter *false* (make-racer-boolean :value "false"))

(defmethod make-load-form ((object racer-boolean) &optional env)
  (declare (ignore env))
  (cond ((eq object *true*) '*true*)
        ((eq object *false*) '*false*)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun true-value-reader (stream subchar arg)
    (declare (ignore stream subchar arg))
    *true*)
  
  (defun false-value-reader (stream subchar arg)
    (declare (ignore stream subchar arg))
    *false*))

(defun true? (object)
  (and (racer-boolean-p object)
       (string= (boolean-value object) "true")))

(defun false? (object)
  (and (racer-boolean-p object)
       (string= (boolean-value object) "false")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun enable-boolean-readers ()
    (setf *readtable* (copy-readtable))
    (set-dispatch-macro-character #\# #\T 'true-value-reader)
    (set-dispatch-macro-character #\# #\F 'false-value-reader)
    (set-dispatch-macro-character #\# #\t 'true-value-reader)
    (set-dispatch-macro-character #\# #\f 'false-value-reader))
  
  (enable-boolean-readers))

(defun boolean-value-from-string (value)
  (cond ((string= value "true") *true*)
        ((string= value "false") *false*)
        (t (error "Boolean value true or false expected."))))

(defun add-boolean-predicate (name neg-name parameters form concrete-domain)
  (let ((value (boolean-value (third form))))
    (add-string-predicate name neg-name parameters
                          (ecase (first form)
                            (boolean= `(string= ,(second form) ,value))
                            (boolean<> `(string<> ,(second form) ,value)))
                          concrete-domain)))
