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
;;; Hilfsfunktionen: Tuple-Konstruktion am Ende (evtl. mit Projektion auf
;;; Told Racer-Attribut-Werte!)
;;; 

(defun get-textual-head-entry (x &optional plural-p) 
  (declare (ignorable plural-p))
  (cond ((is-voi-p x)
         (textual-description x))
        ((consp x)
         (tree-map #'(lambda (x) 
                       (cond ((is-voi-p x)
                              (textual-description x))
                             ((keywordp x)
                              (case x
                                (:told-value :told-values)
                                (:told-value-if-exists :existing-told-values)
                                (:datatype-filler :datatype-fillers)
                                (:annotation-datatype-filler :annotation-datatype-fillers )
                                (otherwise x)))
                             (t x)))
                   x))
        (t x)))


;;;
;;;
;;;

(defun nrql-head-get-attribute-fillers (substrate ind attribute)
  (let ((x (dl-prover-retrieve-individual-attribute-fillers substrate
                                                            ind
                                                            attribute)))
    (or x :no-cd-objects)))

(defun nrql-head-get-told-value (substrate ind property-or-attribute)
  (let ((x 
         (dl-prover-retrieve-individual-attribute-fillers 
          substrate ind property-or-attribute)))

    (if x
        (mapcar #'(lambda (x) 
                    (or (dl-prover-told-value substrate x)
                        :no-told-value))
                x)
      :no-cd-objects)))

(defun nrql-head-get-told-value-if-exists (substrate ind property-or-attribute)

  (let* ((cd-objects
          (dl-prover-retrieve-individual-attribute-fillers
           substrate
           ind property-or-attribute))
                     
         (told-values
          (append

           ;;; Told Values der CD-Object (mittels constraint erzeugt) 
                             
           (remove nil
                   (mapcar #'(lambda (cd-object) 
                               (or (dl-prover-told-value substrate cd-object)
                                   cd-object))
                           cd-objects))

           ;;; Told Values ueber CD-Konzepte in Instance Assertions

           (loop as av-entry in 
                 (dl-prover-told-value substrate ind :cd-object-p nil)
                 when (eq (first av-entry) property-or-attribute)
                 collect (second av-entry)))))

    (or (remove-duplicates told-values)
        :unknown)))


(defun nrql-head-get-datatype-fillers (substrate ind property-or-attribute)
  (retrieve-datatype-values substrate ind property-or-attribute))

(defun nrql-head-get-annotation-datatype-fillers (substrate ind property-or-attribute)
  (retrieve-annotation-datatype-values substrate ind property-or-attribute))

;;;
;;;
;;; 

(defun get-binding-for (query x)  
  (with-critical-section
    (let ((parser (parser query)))
      
      (cond ((is-voi-p x)
             (bound-to x))

            ((is-lambda-abox-projector-thing-p parser x :relaxed-p t)
             (if (consp (first x))
                 (eval-expr (second (first x)) 
                            `(progn ,@(cddr (first x)))
                            (mapcar #'(lambda (x)
                                        (get-binding-for query x))
                                    (cdr x)))

               (eval-expr (mapcar #'textual-description  (all-vois query))
                          `(progn ,@(rest x))
                          (mapcar #'(lambda (x)
                                      (get-binding-for query x))
                                  (all-vois query)))))
             
            #|
             ((is-abox-operator-projector-thing-p parser x :relaxed-p t)  ; relaxed-p spart etwas Zeit
              ;;; (<op> ?x) 

              (apply-head-abox-operator-projector query (first x) x))
             |# 

            ((is-attribute-abox-projector-thing-p parser x :check-p nil :relaxed-p t) 
             ;;; (<cd-attribut> ?x) 

             (nrql-head-get-attribute-fillers (substrate query)
                                              (bound-to (second x))
                                              (first x)))

            ((is-told-value-abox-projector-thing-p parser x :check-p nil :relaxed-p t) 
             ;;; (told-value (<cd-attribut> ?x))

             (nrql-head-get-told-value (substrate query) 
                                       (bound-to (cadadr x))
                                       (caadr x)))

            ((is-told-value-if-exists-abox-projector-thing-p parser x :check-p nil :relaxed-p t) 
             ;;; (told-value-if-exists (<cd-attribut> ?x)) 

             (nrql-head-get-told-value-if-exists (substrate query)
                                                 (bound-to (cadadr x))
                                                 (caadr x)))           

            ((is-datatype-fillers-abox-projector-thing-p parser x :check-p nil :relaxed-p t)
             ;;; (datatype-fillers (<datatype-role> ?x)) 

             (nrql-head-get-datatype-fillers (substrate query)
                                             (bound-to (cadadr x))
                                             (caadr x)))

            ((is-annotation-datatype-fillers-abox-projector-thing-p parser x :check-p nil :relaxed-p t)
             ;;; (annotation-datatype-fillers (<datatype-role> ?x)) 
              
             (nrql-head-get-annotation-datatype-fillers (substrate query)
                                                        (bound-to (cadadr x))
                                                        (caadr x)))

            (t :undefined)))))

(defun get-tuple-for-pattern (query pattern)
  (mapcar #'(lambda (x) (get-binding-for query x)) pattern))

(defmethod get-binding-list-for-pattern ((query query) pattern 
                                         &key
                                         bindings
                                         (verbose-p (verbose-p query))
                                         (dont-show-variables (dont-show-variables query))
                                         (dont-show-lambdas-p (dont-show-lambdas-p query))
                                         (dont-show-head-projection-operators-p
                                          (dont-show-head-projection-operators-p query)))

  
  (delete :group-by
          (mapcar #'(lambda (x binding)
                      (if (is-group-by-operator-thing-p (parser query) x)
                          :group-by
                        (if (and verbose-p 
				 (=> (or (is-voi-p x) 
					 (symbolp x))
				     (let ((x (find-voi (parser query) x)))
				       (not (member x dont-show-variables))))
                                 (=> (or (is-aggregation-operator-thing-p (parser query) x)
                                         (is-order-by-operator-thing-p (parser query) x))
                                     (not dont-show-head-projection-operators-p))
                                 (=> (is-lambda-abox-projector-thing-p (parser query) x :relaxed-p t)
                                     (not dont-show-lambdas-p))
                                 (=> (or (is-abox-projector-thing-p (parser query) x :relaxed-p t)
                                         (is-abox-operator-projector-thing-p (parser query) x :relaxed-p t))
                                     (not dont-show-head-projection-operators-p)))
                            (list (get-textual-head-entry x (and (consp binding) 
                                                                 (cdr binding)))
                                  binding)
                          binding)))
                  pattern
                  (or bindings 
                      (mapcar #'(lambda (x) (get-binding-for query x)) pattern)))))

;;;
;;;
;;;

(defun is-url-p (x)
  (let ((name (if (symbolp x)
                  (symbol-name x)
                x)))
    (or 
     (and (> (length name) 6)
          (or (string-equal "http://" 
                            (subseq name 0 7))
              (string-equal "file://" 
                            (subseq name 0 7))))
     (and (> (length name) 7)
          (or (string-equal "https://" 
                            (subseq name 0 8)))))))
     
(defun is-file-url-p (x)
  (let ((name (if (symbolp x)
                  (symbol-name x)
                x)))
    (and (is-url-p x)
         (> (length name) 6)
         (string-equal "file://" 
                       (subseq name 0 7)))))

(defun is-http-url-p (x)
  (let ((name (if (symbolp x)
                  (symbol-name x)
                x)))
    (and (is-url-p x)
         (or (and (> (length name) 6)
                  (string-equal "http://" 
                                (subseq name 0 7)))
             (and (> (length name) 7)
                  (string-equal "https://" 
                                (subseq name 0 8)))))))

(defun url-namespace (x)
  (when (is-url-p x)
    (let* ((name (if (symbolp x)
                     (symbol-name x)
                   x))
           (pos (position #\# name)))
      (when pos 
        (subseq name 0 pos)))))


(defun remove-url-namespace (x)
  (when (is-url-p x)
    (let* ((name (if (symbolp x)
                     (symbol-name x)
                   x))
           (pos (position #\# name)))
      (when pos 
        (subseq name (1+ pos))))))
    
;;;
;;;
;;;

(defmethod construct-result-tuple ((query query) &key bindings)

  ;;; wird auf den Ergebnis-Stack f. get-next-tupel benoetigt / gerufen, s. hooks*.lisp
  ;;; die Result Bindings im Query-Object werden hingegen *ohne* textual-voi-Eintraege
  ;;; vorgenommen, damit das Ergebnis spaeter noch "variiert" werden kann (s. get-answer). 

  (with-slots (parser rule-con-pattern
                      answer-pattern-without-aggregation-ops
                      substrate) query
    (with-slots (new-inds-hash ;;racer-package
                               ) substrate

      (if (not (is-rule-p query))
          
          (get-binding-list-for-pattern query answer-pattern-without-aggregation-ops 
                                        :bindings bindings)

        ;;; Regel-Konsequenz erzeugen 

        (let ((undefined-p nil))
      
          (labels ((new-ind (key)  
                   
                     ;;; (pet-of |http://cohse.semanticweb.org/ontologies/people#Fred|)

                     (if (and (second key)
                              (every #'is-url-p (rest key)))

                         (let ((key (cons (first key)
                                          (mapcar #'remove-url-namespace (rest key))))

                               (namespace (url-namespace (second key))))

                           (intern
                            (format nil "~A#~A~{-~A~}"
                                    namespace 
                                    (first key)
                                    (rest key))
			    ;;(find-package racer-package)
			    ))

                       (intern
                        (format nil "~A~{-~A~}"
                                (first key)
                                (rest key))
			;;(find-package racer-package)
			)))
		   
		   (process (var) 
                     (cond ((is-voi-p var)
                            (bound-to var))

                           ((is-lambda-abox-projector-thing-p parser var :relaxed-p t)
                            (if (consp (first var))
                                (eval-expr (second (first var)) 
                                           `(progn ,@(cddr (first var)))
                                           (mapcar #'(lambda (x)
                                                       (get-binding-for query x))
                                                   (cdr var)))
                              (eval-expr (mapcar #'textual-description (all-vois query))
                                         `(progn ,@(rest var))
                                         (mapcar #'(lambda (x)
                                                     (get-binding-for query x))
                                                 (all-vois query)))))

                           ((and (consp var) (eq (first var) :new-ind))
                            (let* ((bindings (mapcar #'process (cddr var)))
                                   (key (cons (second var) bindings)))

                              (multiple-value-bind (entry foundp)
                                  (gethash key new-inds-hash)
                                (if foundp
                                    entry
                                  (setf (gethash key new-inds-hash)
                                        (new-ind key))))))
                       
                           ((or (stringp var)
                                (numberp var))
                        
                            var)
                         
                           ((and (consp var) 
                                 (cddr var)
                                 (not (cdddr var)))

                            ;; z.B. (+ (told-value (age ?x)) 30) 
                      
                            `(,(first var)
                              ,(process (second var))
                              ,(process (third var))))


                           (t 

                            ;; ?x, (age ?x), (told-value (age ?x)), (told-value-if-exists (age ?x)) 
                          
                            (let ((res 
                                   (first (ensure-list (get-binding-for query var)))))
                              
                              (setf undefined-p
                                    (member res '(:no-cd-objects
                                                  :no-told-value
                                                  :unknown
                                                  :undefined)))
                          
                              res))))

                   (process-concept (concept)

                     (cond ((symbolp concept) concept)

                           ((and (consp concept) (eq (first concept) :new-symbol))
                            (if (third concept)
                                (intern 
                                 (format nil "~A~{-~A~}" 
                                         (second concept)
                                         (mapcar #'bound-to (cddr concept)))
				 ;;(find-package racer-package)
				 )
                              (intern	
                               (symbol-name (bound-to (second concept)))
			       ;;(find-package racer-package)
			       )))

                           ((is-lambda-abox-projector-thing-p parser concept :relaxed-p t)
                            (if (consp (first concept))
                                (eval-expr (second (first concept))
                                           `(progn ,@(cddr (first concept)))
                                           (mapcar #'(lambda (x)
                                                       (get-binding-for query x))
                                                   (cdr concept)))
                              (eval-expr (mapcar #'textual-description (all-vois query))
                                         `(progn ,@(rest concept))
                                         (mapcar #'(lambda (x)
                                                     (get-binding-for query x))
                                                 (all-vois query)))))

                           ((consp concept)  
                            (cons (process-concept (car concept))
                                  (process-concept (cdr concept))))
                       
                           (t concept)))


                   (process-role (role)

                     (cond ((or (symbolp role)
                                (inv-role-p role))
                            role)

                           ((and (consp role) (eq (first role) :new-symbol))
                            (if (third role)
                                (intern
                                 (format nil "~A~{-~A~}" 
                                         (second role)
                                         (mapcar #'bound-to (cddr role)))
				 ;;(find-package racer-package)
				 )
                              (intern
                               (symbol-name (bound-to (second role)))
			       ;;(find-package racer-package)
			       )))

                           ((is-lambda-abox-projector-thing-p parser role :relaxed-p t)
                            (if (consp (first role))
                                (eval-expr (second (first role))
                                           `(progn ,@(cddr (first role)))
                                           (mapcar #'(lambda (x)
                                                       (get-binding-for query x))
                                                   (cdr role)))
                              (eval-expr (mapcar #'textual-description (all-vois query))
                                         `(progn ,@(rest role))
                                         (mapcar #'(lambda (x)
                                                     (get-binding-for query x))
                                                 (all-vois query)))))

                           ;;; kann eigentlich nicht passieren: 
                           (t role))))
            
            (mapcar #'(lambda (x)
                        (setf undefined-p nil)
                        (let* ((op (first x))
                               (res 
                                (case (to-keyword op)
                                  ((:instance :forget-concept-assertion)
                                   (let ((var (second x))
                                         (concept (third x)))
                                     `(,op ,(process var) 
                                           ,(process-concept concept))))

                                  ((:related :forget-role-assertion)
                                   (let ((from (second x))
                                         (to (third x))
                                         (role (fourth x)))
                                     `(,op
                                       ,(process from)
                                       ,(process to)
                                       ,(process-role role))))

                                  ((:same-as :different-from)  
                                   (let ((from (second x))
                                         (to (third x)))

                                     `(,op
                                       ,(process from)
                                       ,(process to))))
				  
				  ((:all-different)  
				   `(,op
				     ,@(mapcar #'process (rest x))))
                                  
                                  ((:constrained)
                                   (let ((from (second x))
                                         (to (third x))
                                         (role (fourth x)))
                                     `(,op
                                       ,(process from)
                                       ,(process to)
                                       ,role)))

                                  ((:constraints :constraint)
                                   `(constraints
                                      ,@(mapcar #'(lambda (constraint) 
                                                    (list (first constraint)
                                                          (process (second constraint))
                                                          (process (third constraint))))
                                                (rest x))))

                                  (otherwise

                                   (if (is-lambda-abox-projector-thing-p parser x :relaxed-p t)
                                       (if (consp (first x))
                                           (eval-expr (second (first x)) 
                                                      `(progn ,@(cddr (first x)))
                                                      (mapcar #'(lambda (x)
                                                                  (get-binding-for query x))
                                                              (cdr x)))

                                         (eval-expr (mapcar #'textual-description (all-vois query))
                                                    `(progn ,@(rest x))
                                                    (mapcar #'(lambda (x)
                                                                (get-binding-for query x))
                                                            (all-vois query))))
                                     
                                     (nrql-runtime-error 
                                      "Bad rule pattern entry ~A" x)) 


                                   #| 

                                   ;;; new: do it lazily 
                                   ;;; quatsch, dann sehe ich die Lambda-Reduktions-Ergebniss ja nicht 
                                   ;;; im Ergebnis...  -> "defer" eingefuehrt als neue Special Form in MiniLisp

                                   (if (is-lambda-abox-projector-thing-p parser x :relaxed-p t)
                                       (if (consp (first x))
                                           (progn 
                                             `((:lambda ,(second (first x))
                                                 ,@(cddr (first x)))
                                               ,(mapcar #'(lambda (x)
                                                            (get-binding-for query x))
                                                        (cdr x))))
                                         
                                         `((:lambda ,(mapcar #'textual-description (all-vois query))
                                             ,@(rest x))
                                           ,(mapcar #'(lambda (x)
                                                        (get-binding-for query x))
                                                    (all-vois query))))
                                         
                                     (nrql-runtime-error 
                                      "Bad rule pattern entry ~A" x)) 

                                   |# 
                                   
                                   ))))
                                  
                          (if undefined-p 
                              :undefined
                            res)))
                             
                    rule-con-pattern)))))))


