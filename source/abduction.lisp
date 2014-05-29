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
;;; Modi: Subset of { :always-apply-modi :new-ind :reuse-old :reuse-new :no-abduction} 
;;;

(defparameter *hypo-mode-concepts* '(:one-new-ind :reuse-old :always-apply-modi))

(defparameter *hypo-mode-roles*    '(:one-new-ind :reuse-old :always-apply-modi :same-as))

;;;
;;;
;;;

(defvar *candidate-individuals* nil)

(defvar *binding-validator* nil)

(defvar *runtime-consistency-checking-p* nil)

(defvar *final-consistency-checking-p* nil)

(defvar *cutoff-fn* nil)

(defvar *how-many-abduction-solutions* nil)

(defvar *order-by* :cae2) ; wird maximiert!

(defvar *reverse-order-p* t)

(defvar *only-best-p* t)

(defvar *ensure-permutations-p* t)

(defvar *hypo-mode-stack* nil)

(defvar *hypo-mode-concepts1*)

(defvar *hypo-mode-roles1*)

(defvar *same-as-only-p* nil)

(defvar *abduction-debugging-p* nil)

(defvar *new-inds* nil)

(defvar *new-ind-to-var* nil)

(defvar *hypothesized-assertions* nil)

(defvar *all-assertions* nil)

(defvar *res-vars* nil)

(defvar *create-new-inds-p* nil)

(defvar *ind-counter* 0)

(defvar *ind-prefix* "IND-")


;;; (defparameter *explanation* nil)

;;;
;;;
;;;

(defpersistentclass abduction-substrate (racer-dummy-substrate) nil)

(defmethod get-parser-class-for-substrate ((substrate abduction-substrate))
  'abduction-query-parser)

(defpersistentclass abduction-query-parser (nrql-abox-query-parser) nil) 

(defmethod syntactically-rewrite-atomic-query (query (parser abduction-query-parser) &rest args)
  (if (and (consp query)
           (eq (first query) :strict))
      (multiple-value-bind (query inds racer-rewritten-p)
          (apply #'call-next-method (second query) parser args)
        (values `(:strict ,query) inds racer-rewritten-p))
    (call-next-method)))


(defmethod get-vois-from ((parser abduction-query-parser) expr &rest args)
  (if (and (consp expr)
           (eq (first expr) :strict))
      (apply #'call-next-method parser (second expr) args)
    (call-next-method)))

(defmethod parse-query (query (parser abduction-query-parser) &rest args)
  (if (and (consp query)
           (eq (first query) :strict))
      (let* ((query (apply #'call-next-method (second query) parser args))
             (query 
              (change-class query 
                            (typecase query
                              (nrql-instance-retrieval-query 'abduction-instance-retrieval-query)
                              (nrql-edge-retrieval-query 'abduction-edge-retrieval-query)
                              (nrql-same-as-query 'abduction-same-as-query)
                              (otherwise (type-of query))))))
        (setf (slot-value query 'strict-p) t)
        query)
    (call-next-method)))

(defpersistentclass abduction-atomic-query (nrql-atomic-query) 
  ((strict-p :reader strict-p :initform nil)))

(defpersistentclass abduction-same-as-query (nrql-same-as-query abduction-atomic-query) nil)

(defpersistentclass abduction-instance-retrieval-query (nrql-instance-retrieval-query abduction-atomic-query) nil)

(defpersistentclass abduction-edge-retrieval-query (nrql-edge-retrieval-query abduction-atomic-query) nil)

;;;
;;;
;;;

(defmethod unparse-query ((query abduction-instance-retrieval-query))
  (if (strict-p query)
      `(:strict ,(call-next-method))
    (call-next-method)))

(defmethod unparse-query ((query abduction-edge-retrieval-query))
  (if (strict-p query)
      `(:strict ,(call-next-method))
    (call-next-method)))

(defmethod unparse-query ((query abduction-same-as-query))
  (if (strict-p query)
      `(:strict ,(call-next-method))
    (call-next-method)))

;;;
;;;
;;;

(defun print-abox (clone)
  (when *abduction-debugging-p*
    (format t "~%~%***********************************~%")
    (format t "ABox: ~A. TBox: ~A. Consistent: ~A~%" 
            clone
            (associated-tbox clone)
            (abox-consistent-p clone))
  
    (pprint (append (mapcar #'(lambda (x) 
                                `(instance ,@x))
                            (all-concept-assertions clone))
                    (mapcar #'(lambda (x) 
                                `(related ,@(first x) ,(second x)))
                            (all-role-assertions clone))))))

;;;
;;;
;;;

(nrql-defun (enable-abduction)
  (c-mode r-mode  
          &key (reset-p t) hypo-mode-stack runtime-consistency-checking-p final-consistency-checking-p
          ensure-permutations-p same-as-only-p 
          how-many
	  candidate-individuals
          binding-validator
          cutoff-fn order-by only-best-p reverse-order-p)
	    
  (setf c-mode (ensure-list c-mode))
  (setf r-mode (ensure-list r-mode))

  (dolist (mode c-mode)
    (unless (member mode 
                    '(:one-new-ind :reuse-old :reuse-new
                      :always-apply-modi :no-abduction))
      (nrql-error "Bad mode: ~A in :c-mode ~A" mode c-mode)))
	    
  (dolist (mode r-mode)
    (unless (member mode 
                    '(:one-new-ind :reuse-old :reuse-new :same-as
                      :always-apply-modi :no-abduction))
      (nrql-error "Bad mode: ~A in :r-mode ~A" mode r-mode)))
	    
  (dolist (entry hypo-mode-stack)
    (dolist (mode (ensure-list entry))

      (unless (member mode 
                      '(:one-new-ind :reuse-old :reuse-new
                        :always-apply-modi :no-abduction))
        (nrql-error "Bad mode: ~A in :cr-stack entry ~A" mode entry))))

  (setf *hypo-mode-stack* 
        (mapcar #'ensure-list 
                hypo-mode-stack))
	    
  (when reset-p
    (delete-all-substrates)
    (reset-nrql-engine))

  (setf *candidate-individuals*
        candidate-individuals
        *binding-validator*
        binding-validator
        *type-of-substrate* 
        'abduction-substrate
        *runtime-consistency-checking-p* 
        runtime-consistency-checking-p
        *final-consistency-checking-p* 
        final-consistency-checking-p
        *cutoff-fn* 
        cutoff-fn
        *order-by*
        order-by
        *only-best-p*
        only-best-p
        *reverse-order-p*
        reverse-order-p
        *ensure-permutations-p* 
        ensure-permutations-p
        *hypo-mode-concepts* c-mode
        *hypo-mode-roles* r-mode
        *same-as-only-p* same-as-only-p
        *how-many-abduction-solutions* how-many)
	    
  :ok)


(nrql-defun (disable-abduction)
  (&key (reset-p t))
  (setf *candidate-individuals*
        nil
        *binding-validator*
        nil
        *type-of-substrate* 
        'racer-dummy-substrate
        *hypo-mode-stack* nil
        *runtime-consistency-checking-p* nil
        *final-consistency-checking-p* nil
        *ensure-permutations-p* t
        *cutoff-fn* nil
        *order-by* :cae2
        *reverse-order-p* t
        *only-best-p* t
        *hypo-mode-concepts* '(:one-new-ind :reuse-old :always-apply-modi)
        *hypo-mode-roles*    '(:one-new-ind :reuse-old :always-apply-modi :same-as)
        *same-as-only-p* nil
        *how-many-abduction-solutions* nil)

  (when reset-p
    (delete-all-substrates)
    (reset-nrql-engine)))


(defun get-abduction-params ()
  (list *candidate-individuals*
        *binding-validator*
	*type-of-substrate* 
        *hypo-mode-stack* 
        *runtime-consistency-checking-p* 
        *final-consistency-checking-p* 
        *ensure-permutations-p*
        *cutoff-fn*
        *order-by*
        *only-best-p*
        *reverse-order-p* 
        *hypo-mode-concepts* 
        *hypo-mode-roles* 
        *same-as-only-p*
        *how-many-abduction-solutions*))

(defun set-abduction-params (p)
  (setf *candidate-individuals*
        (pop p)
	*binding-validator*
        (pop p)
	*type-of-substrate* 
        (pop p)
        *hypo-mode-stack* 
        (pop p)
        *runtime-consistency-checking-p* 
        (pop p)
        *final-consistency-checking-p* 
        (pop p)
        *ensure-permutations-p*
        (pop p)
        *cutoff-fn*
        (pop p)
        *order-by*
        (pop p)
        *only-best-p*
        (pop p)
        *reverse-order-p*
        (pop p)
        *hypo-mode-concepts* 
        (pop p)
        *hypo-mode-roles* 
        (pop p)
        *same-as-only-p*
        (pop p)
        *how-many-abduction-solutions*
        (pop p)))

(defun make-entailment-query (abox)

  (labels ((var-for (ind)
             (intern (format nil "$?~A" ind))))
    
    (let* ((cas 
            (remove-if-not #'(lambda (x) (eq (first x) 'instance)) abox))
           (ras 
            (remove-if-not #'(lambda (x) (eq (first x) 'related)) abox))
           (sas
            (remove-if-not #'(lambda (x) (eq (first x) 'same-as)) abox))
           (das 
            (remove-if-not #'(lambda (x) (eq (first x) 'different-from)) abox))
           (inds
            (remove-duplicates
             (append (mapcar #'second cas)
                     (mapcar #'second ras)
                     (mapcar #'third ras)
                     (mapcar #'second sas)
                     (mapcar #'third sas)
                     (mapcar #'second das)
                     (mapcar #'third das))))
           (old-inds 
            (remove-if #'really-is-new-ind-p inds))

           (body 
            `(and ,@(mapcar #'(lambda (x) 
                                `(same-as ,(var-for x) ,x))
                            old-inds)
                  ,@(mapcar #'(lambda (x) 
                                `(,(var-for (second x)) ,(third x)))
                            cas)
                  ,@(mapcar #'(lambda (x) 
                                `(,(var-for (second x))
                                  ,(var-for (third x))
                                  ,(fourth x)))
                            ras)
                  ,@(mapcar #'(lambda (x) 
                                `(,(var-for (second x))
                                  ,(var-for (third x))
                                  same-as))
                            sas)
                  ,@(mapcar #'(lambda (x) 
                                `(,(var-for (second x))
                                  ,(var-for (third x))
                                  different-from))
                            das))))

      body)))

(defun get-abox-inds (abox)
  (remove-duplicates
   (tree-flatten
    (mapcar #'(lambda (x) 
                (ecase (first x)
                  ((:instance instance) (list (second x)))
                  ((:related related) (list (second x) (third x)))
                  ((:same-as same-as) (cdr x))
                  ((:different-from different-from) (cdr x))))
            abox))))

(nrql-defun (abox-entails-abox-p)
  (a b &optional (tbox (if *cur-substrate* (tbox *cur-substrate*) (current-tbox))))

  (let ((abox1 (current-abox))
        (tbox1 (current-tbox))
        (clone (intern "some-new-tempory-abox-1414xsfef1"))
        (diff-inds
         (set-difference
          (get-abox-inds b)
          (get-abox-inds a))))

    (init-abox clone tbox)
         
    (dolist (ass a)
      (ecase (first ass)
        ((:instance instance)
         (apply #'add-concept-assertion clone (rest ass)))
        ((:related  related)
         (apply #'add-role-assertion clone (rest ass)))
        ((:same-as  same-as)
         (apply #'add-same-individual-as-assertion clone (rest ass)))
        ((:different-from  different-from)
         (apply #'add-different-from-assertion clone (rest ass)))))

    ;;; verhindere "unknown ind error"
    (dolist (ind diff-inds)
      (add-concept-assertion clone ind 'top))

    (print-abox clone)
    
    (prog1

        (cond ((abox-consistent-p clone)
               (prog1 
                   (racer-answer-query () (make-entailment-query b) :how-many 1 :abox clone)
                 (delete-query :last)))

              (t t))

      (forget-abox clone)
      (set-current-tbox tbox1)
      (set-current-abox abox1))))


(nrql-defun (get-maximum) (aboxes &key (key #'identity))
  (let ((res nil)
	(entailments nil)
	(equi nil))
    (dolist (current aboxes)
      (let ((others (remove current aboxes)))
        (unless (some #'(lambda (other)
                          (when (abox-entails-abox-p (funcall key other)
						     (funcall key current))
			    (push (list other current) entailments)
			    (when (member (list current other) entailments :test #'equal)
			      (push (list other current) equi))
			    t))
                      others)
          (push current res))))

    (append res
	    (reduce #'nconc
		   (remove-if #'(lambda (cur) 
				  (let ((x (first cur))
					(y (second cur)))
				    (or (member x res :test #'equal :key #'second)
					(member y res :test #'equal :key #'second))))
			      equi)))))


(nrql-defun (get-minimum) (aboxes &key (key #'identity))
  (let ((res nil)
	(entailments nil)
	(equi nil))
    (dolist (current aboxes)
      (let ((others (remove current aboxes)))
        (unless (some #'(lambda (other)
			  (when (abox-entails-abox-p (funcall key current)
						     (funcall key other))
			    (push (list current other) entailments)
			    (when (member (list other current) entailments :test #'equal)
			      (push (list current other) equi))
			    t))
		      others)
          (push current res))))

    (append res
	    (reduce #'nconc
		   (remove-if #'(lambda (cur) 
				  (let ((x (first cur))
					(y (second cur)))
				    (or (member x res :test #'equal :key #'second)
					(member y res :test #'equal :key #'second))))
			      equi)))))

;;;
;;;
;;;

(defmethod prepare-substrate1 :after ((substrate abduction-substrate))
  )

(defmethod evaluate-query :before ((substrate abduction-substrate) (query query))
  (dolist (query (cons query (all-subqueries query)))
    (setf (existential-vois query) nil)))


(defmethod evaluate-subquery :around ((substrate abduction-substrate) (query query) remaining-conjuncts)
  (declare (ignorable remaining-conjuncts))

  (when (=> *binding-validator* (every *binding-validator* (all-vois query)))
    
    (cond ((not (is-atomic-query-p query))
           (call-next-method))

          ((and (is-abduction-atomic-query-p query)
                (strict-p query))

           (let ((*hypo-mode-concepts1* '(:no-abduction))
                 (*hypo-mode-roles1* '(:no-abduction)))
             (call-next-method)))

          (t
      
           (let* ((*create-new-inds-p* t)

                  (*hypo-mode-concepts1* (or (first *hypo-mode-stack*)
                                             *hypo-mode-concepts*))
	     
                  (*hypo-mode-roles1* (or (first *hypo-mode-stack*)
                                          *hypo-mode-roles*))
	     
                  (*hypo-mode-stack* (rest *hypo-mode-stack*)))
	
             (when *abduction-debugging-p*
               (show-debug-info query remaining-conjuncts))

             (when (=> remaining-conjuncts ;;; sonst wird das andere still-good-p genommen!
                       (still-good-p *running-query* query remaining-conjuncts))

               ;;; calls next around method
               (call-next-method)))))))


(defun show-debug-info (query remaining-conjuncts)

  (format t "Atom        : ~A
Remaining   : ~A
Tuple       : ~A
C-Mode      : ~A
R-Mode      : ~A
Same-As Only: ~A 
New inds    : ~A
Var for ind : ~A
Assertions  : ~A
Hypothesized: ~A 
Old         : ~A 
Create      : ~A~% ~%" 
          query remaining-conjuncts

          (mapcar #'(lambda (x) 
                      `(,(get-textual-head-entry x)
                        ,(get-binding-for query x)))
                  (all-vois query))

          *hypo-mode-concepts1*
          *hypo-mode-roles1* 
          *same-as-only-p*
          (remove-duplicates *new-inds*)
          *new-ind-to-var*
          (remove-duplicates *all-assertions* :test #'equal)
          (remove-duplicates *hypothesized-assertions* :test #'equal)
          (set-difference (remove-duplicates *all-assertions* :test #'equal)
                          (remove-duplicates *hypothesized-assertions* :test #'equal)
                          :test #'equal)
          *create-new-inds-p*)

  (when (some #'(lambda (x) 
                  (find-if #'really-is-new-ind-p x))
              (set-difference (remove-duplicates *all-assertions* :test #'equal)
                              (remove-duplicates *hypothesized-assertions* :test #'equal)
                              :test #'equal))
    (break "Bad old assertions found in ~S" *new-inds*)))

(defmethod needs-filler-reasoning-p ((substrate abduction-substrate))
  ;;; damit dl-prover-internal-individuals-related-p aufgerufen wird!
  t)

(defmethod register-bindings-no-answer-pattern ((substrate abduction-substrate))
  (register-bindings substrate *running-query* nil nil)
  
  ;;; Ja, Nein? 
  ;;; (throw 'query-execution 'done)
  )



(defun check-abox (abox)  
  (=> abox
      (let ((abox1 (current-abox))
            (tbox1 (current-tbox))
            (clone (create-abox-clone (abox *cur-substrate*)
                                      :copy-rules nil)))


        (dolist (ass abox)
          (ecase (first ass)
            ((:instance instance)
             (apply #'add-concept-assertion clone (rest ass)))
            ((:related  related)
             (apply #'add-role-assertion clone (rest ass)))
            ((:same-as  same-as)
             (apply #'add-same-individual-as-assertion clone (rest ass)))
            ((:different-from  different-from)
             (apply #'add-different-from-assertion clone (rest ass)))))

        (print-abox clone)

        (prog1 (abox-consistent-p clone)
          (forget-abox clone)
          (set-current-tbox tbox1)
          (set-current-abox abox1)))))


(defun get-current-h (query)
  (let* ((hypothesized-assertions
          (reverse (remove-duplicates *hypothesized-assertions* :test #'equal)))
	   
         (all-assertions 
          (reverse (remove-duplicates *all-assertions* :test #'equal)))
	   
         (entailed-assertions
          (set-difference all-assertions hypothesized-assertions :test #'equal))
	   
         (new-inds (reverse (remove-duplicates *new-inds*)))
	   
         (old-inds (remove-duplicates
                    (remove-if (lambda (x)
                                 (member x *new-inds*))
                               (mapcar #'(lambda (x) 
                                           (get-binding-for query x))
                                       (all-vois query))))))
	   
    (let* ((h
            (list `(:tuple 
                    ,@(mapcar #'(lambda (x) 
                                  `(,x
                                    ,(get-binding-for query 
                                                      (find-voi (parser query) x))))
                              *res-vars*))
                  `(:full-tuple
                    ,@(mapcar #'(lambda (x) 
                                  `(,(get-textual-head-entry x)
                                    ,(get-binding-for query x)))
                              (all-vois query)))
                  `(:new-inds 
                    ,@new-inds)
                  `(:old-inds
                    ,@old-inds)
                  `(:hypothesized-assertions
                    ,@hypothesized-assertions)
                  `(:entailed-assertions
                    ,@entailed-assertions)
                  `(:all-assertions
                    ,@all-assertions)
                  `(:new-ind-to-var-mapping
                    ,@*new-ind-to-var*))))
          
      (list (order-fn *order-by* h)
            h))))


(defun still-good-p (query conjunct rem-conjuncts) 
  (let* ((h (get-current-h query))
         (new (cdr (assoc :hypothesized-assertions (second h))))
         (old (cdr (assoc :old-assertions (second h))))
         (new-inds (cdr (assoc :new-inds (second h))))
         (last-p (not rem-conjuncts)))
    
    (let ((val 
        
           (and (=> *cutoff-fn*
                    (if (consp *cutoff-fn*)
                        ;;; '(:hypothesized-assertions < 6)
                        (case (first *cutoff-fn*)
                          (:new-inds (funcall (second *cutoff-fn*)
                                              (length new-inds)
                                              (third *cutoff-fn*)))
                          (:hypothesized-assertions 
                           (funcall (second *cutoff-fn*)
                                    (length new)
                                    (third *cutoff-fn*)))
                          ((:old-assertions
                            :entailed-assertions)
                           (funcall (second *cutoff-fn*)
                                    (length old)
                                    (third *cutoff-fn*))))
                      (evaluate1 `(,*cutoff-fn* 
                                   (quote ,new-inds)
                                   (quote ,new)
                                   (quote ,old)))))
                
                (=> (and *runtime-consistency-checking-p*
                         (not last-p))
                    (check-abox new))
             
                (=> (and *final-consistency-checking-p*
                         last-p)
                    (check-abox new))

                (=> (and *only-best-p*
                         (not *reverse-order-p*)
                         (fourth (hypotheses query))
                         (member *order-by*
                                 '(:new-inds ; + = monoton wachsend 
                                   :hypothesized-assertions ; + 
                                   :old-assertions ; + 
                                   :entailed-assertions ; + 
                                   :old-inds ; + 
                                   )))

                    ;;; da minimiert wird, kann ich abbrechen, sobald groesser als 
                    ;;; best-so-far hypothese 
                    (if (> (first h)
                           (first (fourth (hypotheses query))))
                        nil
                      t))

                (=> (and *only-best-p*
                         *reverse-order-p*
                         (fourth (hypotheses query))
                         (member *order-by*
                                 '(:new-paper-fn
                                   :cae
                                   :cae2
                                   :rasouli-paper-fn)))

                    ;;; hier kann ich abbrechen, sobald ich 
                    ;;; weiss, dass ich die best-so-far hypothese 
                    ;;; nicht mehr ueberbieten kann, weil nicht
                    ;;; mehr genuegend Atome (die bestenfalls entailed
                    ;;; sind und somit positiv als Punkte f. :new-paper-fn
                    ;;; zaehlen) uebrig sind!

                    (let* ((cur (first h))
                           (best-so-far (first (fourth (hypotheses query))))
                           (diff (- best-so-far cur))
                           ;;; current conjunct muss dazugezaehlt werden!
                           (rem (1+ (length rem-conjuncts))))

                      ;;; 
                      ;;; CAE: TOP-Konjunkte nicht zaehlen
                      ;;; 

                      (when (or (eq *order-by* :cae) ; war falsch, aber beibehalten - cae2 nun default
                                (eq *order-by* :cae2))
                        (when (and (is-instance-retrieval-query-p conjunct)
                                   (eq (original-description conjunct) 'top))
                          (decf rem))
                        (dolist (rcon rem-conjuncts)
                          (when (and (is-instance-retrieval-query-p rcon)
                                     (eq (original-description rcon) 'top))
                            (decf rem))))

                      ;;;
                      ;;; Rasouli: Top-Konjunkte zaehlen negativ 
                      ;;; 

                      (when (eq *order-by* :rasouli-paper-fn)
                        (when (and (is-instance-retrieval-query-p conjunct)
                                   (eq (original-description conjunct) 'top))
                          (decf rem 2))
                        (dolist (rcon rem-conjuncts)
                          (when (and (is-instance-retrieval-query-p rcon)
                                     (eq (original-description rcon) 'top))
                            (decf rem 2))))

                      ;;;
                      ;;;
                      ;;;

                      (when *abduction-debugging-p*
                        (terpri)
                        (pprint `((:conjunct ,(unparse-query conjunct))
                                  (:query ,(unparse-query query))
                                  (:current ,cur ,h)
                                  (:remaining ,rem ,rem-conjuncts)
                                  (:best-so-far ,best-so-far ,(fourth (hypotheses query))))))

                      (cond ((> cur best-so-far)
                             
                             (when *abduction-debugging-p*
                               (pprint '(:keep 1)))
                             
                             t)
                            
                            ((and (zerop rem)
                                  (= cur best-so-far))

                             (when *abduction-debugging-p*
                               (pprint '(:keep 2)))

                             t)

                            ((zerop rem)

                             (when *abduction-debugging-p*
                               (pprint '(:reject 3)))

                             nil)

                            ((> diff rem)

                             (when *abduction-debugging-p*
                               (pprint '(:reject 4)))

                             nil)

                            (t

                             (when *abduction-debugging-p*
                               (pprint '(:keep 5)))
                                     
                             t)))))))

      (when val 
        h))))
  

(defmethod register-bindings :around ((substrate abduction-substrate) (query nrql-query) (answer-pattern list)
                                      (new-bindings list))


  (when *abduction-debugging-p*
    (show-debug-info query nil))  

  (when (=> *binding-validator* (every *binding-validator* (all-vois query)))

    (with-slots (hypotheses result-vois) *running-query*

      (unless hypotheses 
        (setf hypotheses 
              (list nil (mht :test #'equal) (mht :test #'equal) nil)))

      (let* ((hypothesized-assertions
              ;;; NICHT nreverse!!!! evtl. hat remove nichts zu entfernen, dann ist das keine Kopie!
              (reverse (remove-duplicates *hypothesized-assertions* :test #'equal)))
	   
             (new-inds (reverse (remove-duplicates *new-inds*)))
	   
             (result-binding 
              (mapcar #'bound-to result-vois))
	   
             (count 0)
	   
             (subst (mapcar #'(lambda (new-ind) 
                                (list new-ind
                                      (intern (format nil "NEW-IND-~A" (incf count)))))
                            new-inds))
	   
             ;;; "isomorphe" herausfiltern (-> new-inds kanonisieren!) 
	   
             (key 
              (mapcar (lambda (x) 
                        (mapcar (lambda (i)
                                  (let ((n (second (assoc i subst))))
                                    (or n i)))
                                x))
                      hypothesized-assertions))

             (key2
              (mapcar (lambda (x) 
                        (let ((n (second (assoc x subst))))
                          (or n x)))
                      result-binding))
           
             (key (sort key #'string-lessp 
                        :key #'(lambda (x) 
                                 (format nil "~S" x)))))

        (when (=> (gethash key (second hypotheses))
                  (and *ensure-permutations-p* 
                       (not 
                        (gethash key2 (third hypotheses)))))

          (setf (gethash key (second hypotheses)) t)
          (setf (gethash key2 (third hypotheses)) t)

          (let ((h (still-good-p query query nil)))
            (when h 
              (let ((best-so-far (fourth hypotheses)))
                (if (not best-so-far)
                    (setf (fourth hypotheses) h)
                  (if *reverse-order-p*
                      ;;; greatest
                      (when (> (first h) 
                               (first best-so-far))
                        (setf (fourth hypotheses) h))
                    ;;; smallest
                    (when (< (first h) 
                             (first best-so-far))
                      (setf (fourth hypotheses) h)))))

              (push h (first hypotheses))))

          (call-next-method)

          ;;; bezieht sich auf die ":only-best-p NIL", also alle Lösungen! 
          (when *how-many-abduction-solutions*
            (decf *how-many-abduction-solutions*)
            (when (zerop *how-many-abduction-solutions*)
              (with-slots (abort-search-p) *running-query*
                (setf abort-search-p t)
                (throw 'query-execution 'done)))))))))

;;;
;;;
;;;

(defmethod answer-query-int :around ((substrate abduction-substrate) (query t) (result-vois list)
				     &rest args
				     &key
                                     (only-explanations-p t)
                                     execute-p
                                     &allow-other-keys)

  (declare (ignorable args))
  
  (let ((lq *last-query*))

    (let ((res (call-next-method)))

      (cond ((or (eq res :timeout)
                 (and
                  (consp res)
                  (eq (first res) :timeout)))

             :timeout)

            ((or (eq *last-query* lq)
                 (not execute-p))
             
             res)

            (t

             (if (tuple-at-a-time-p *last-query*)
                 res

               (let ((expl 
                      (apply #'get-explanations *last-query* 
                             args)))

                 (if expl
                     (if only-explanations-p
                         (values t expl)
                       (values res expl))

                   ;;; no explanations (perhaps all inconsistent)
                   ;;; inform user if res exists 
                   
                   (if res
                       (values nil nil :warning-only-inconsistent-explanations)
                     (values nil nil))))))))))
                 

(defun mapquery (x)
  (labels ((do-it (x)
             (if (consp x)
                 (case (first x)
                   ((and or union not neg) 
                    `(,(first x)
                      ,@(mapcar #'do-it (rest x))))
                   (t (if (and (consp x)
                               (= (length x) 3))
			  (if *disable-defined-queries-p*
			      x
			    (let* ((role-p (role-p (third x)))
				   (query-p (and *cur-substrate*
						 (get-definition1 *cur-substrate*
								  (third x) 
								  2
								  :error-p nil))))
			      (cond ((and query-p (not role-p))
				     x)
				    ((and query-p role-p)
				     `(substitute (,(third x) ,(first x) ,(second x))))
				    (t x))))
                        x)))
               x)))
      
    (do-it x)))


(defmethod syntactically-rewrite-atomic-query :around (query (parser abduction-query-parser) 
                                                             &key negated-p 
                                                             &allow-other-keys)
  (declare (ignorable query))
  
  (multiple-value-bind (query inds racer-rewritten-p)
      (call-next-method)
    
    (values 
     (cond ((eq (first query) :top)
            (if negated-p 
                `(,(second query) bottom)
              `(,(second query) top)))
           ((eq (first query) :bottom)
            (if negated-p 
                `(,(second query) top)
              `(,(second query) bottom)))
           (t query))
     inds 
     racer-rewritten-p)))
  

(defun racer-answer-query-with-explanation (res-args query &rest args &key 
                                                     cutoff-fn
                                                     hypo-mode-stack

                                                     (c-mode *hypo-mode-concepts*) 
                                                     (r-mode *hypo-mode-roles*)

                                                     (only-best-p *only-best-p*)
                                                     (order-by *order-by*) 
                                                     (reverse-order-p *reverse-order-p*)
                                                     (ensure-permutations-p *ensure-permutations-p*)

                                                     (how-many *how-many-abduction-solutions*)
                                                     strategy
                                                     simple-result-p 
                                                     (runtime-consistency-checking-p *runtime-consistency-checking-p*)
                                                     (final-consistency-checking-p *final-consistency-checking-p*)
                                                     same-as-only-p 
						     
						     candidate-individuals
                                                     binding-validator
						     
                                                     &allow-other-keys)

  (let* ((accu nil)
         (last-res nil)
         (query (mapquery query))
         (*type-of-substrate* 'abduction-substrate)

	 ;;; notwendig, um Duplikatseleminierung auszutricksen! 

         (query1 (prepare-query res-args query
                                :exepute-p nil
                                :add-same-as-conjuncts-p nil
                                :replace-inds-with-vars-p nil))
         (query1 (if (or (eq query1 :timeout)
                         (equal query1 '(:timeout)))
                     (return-from racer-answer-query-with-explanation :timeout)
                   (find-query (first query1))))
         (res-vois (remove-duplicates 
                    (append (mapcar #'get-textual-head-entry
                                    (remove-if-not #'is-query-variable-p (all-vois query1)))
                            res-args)))
         (query (query-body query1))
         (params nil))

    (setf *res-vars* res-args)

    #+:ignore
    (setf *res-vois* res-vois
          *x* query)

    ;;(break)

    (delete-query :last)

    (labels ((do-it (strategy)

               (setf params (get-abduction-params))
	       
               (unwind-protect
		   (progn 
		     
		     (enable-abduction (first strategy)
				       (second strategy)
				       :candidate-individuals candidate-individuals
                                       :binding-validator binding-validator
                                       :how-many how-many
                                       :cutoff-fn cutoff-fn
				       :reset-p nil
                                       :order-by order-by
                                       :only-best-p only-best-p
                                       :reverse-order-p reverse-order-p
				       :hypo-mode-stack (third strategy)
				       :runtime-consistency-checking-p runtime-consistency-checking-p
                                       :final-consistency-checking-p final-consistency-checking-p
                                       ;;; sonst fehlen u.U. Lösungen bei only-best-p = NIL!
                                       :ensure-permutations-p (or ensure-permutations-p (not only-best-p)) 
				       :same-as-only-p same-as-only-p)

		     (setf last-res 
                           (multiple-value-list
                            (apply #'racer-prepare-query res-vois
                                   query 
                                   :how-many nil ; funktioniert hier nicht, s. enable-abduction
                                   :prevent-query-expansion-p t 
                                   ;; wurde bereits oben expandiert!
                                   ;; schaedlich bei keep-defined-query-atoms-p! 
                                   :execute-p t
                                   :strict-same-as-p t 
                                   :add-same-as-conjuncts-p nil 
                                   :replace-inds-with-vars-p nil
                                   :remove-duplicates-p nil 
                                   ;; wichtig! s. Atila-Bug

                                   args)))
                     ;;(break)
                     )
		 
                 (set-abduction-params params)

                 (cond ((or (eq last-res :timeout)
                            (equal last-res '(:timeout)))
                        
                        :timeout)

                       ((and last-res 
			     (not (cdr last-res)))
                        
                        ;;; (:query-1 :running) 

			(setf last-res (first last-res)))

		       (t
		      
			(push `((:strategy ,strategy)
				(:result ,@last-res))
			      accu)

                        ;;; (disable-abduction :reset-p nil)

			last-res))))

             (process (strategy)
	       
               (let* ((strategy (ensure-list strategy))
                      (item (first strategy))
                      (rest (rest strategy)))
		 
                 (cond ((keywordp item)
                        
                        (case item
                          (:if 
                              (let ((if (second strategy))
                                    (then (third strategy))
                                    (else (fourth strategy)))
                                (if (progn 
                                      (setf last-res nil)
                                      (process if)
                                      (first last-res))
                                    (when then
                                      (process then))
                                  (when else
                                    (process else)))))
			  
			  (:when
                              (let ((if (second strategy))
                                    (then (third strategy)))
                                (when (progn 
					(setf last-res nil)
					(process if)
					(first last-res))
				  (when then
				    (process then)))))
			  
			  (:unless
                              (let ((if (second strategy))
                                    (then (third strategy)))
                                (unless (progn 
					  (setf last-res nil)
					  (process if)
					  (first last-res))
				  (when then
				    (process then)))))
			  
                          (:no-abduction 
			   (do-it '(:no-abduction :no-abduction))
                           (process rest))
			  
			  (:default 
                           (do-it (list c-mode
                                        r-mode))
			      
                           (process rest))

                          (otherwise 
                           (nrql-error "Bad strategy item: ~S" item))))
                       
                       ((null item) t)

                       ((consp item)
                        (do-it item)
                        (process rest))

                       (t (nrql-error "Bad strategy item: ~S" item))))))

      (let ((strategy
             (or strategy
                 (list (list c-mode r-mode hypo-mode-stack)))))

        (process strategy)

        (if (cdr strategy)
            (if simple-result-p
                (mapcar #'(lambda (x) 
                            (rest (second x)))
                        (nreverse accu))
              (nreverse accu))
          last-res)))))


;;;
;;;
;;; 

(defun create-new-ind ()
  (when *create-new-inds-p*
    (let ((ind 
           (intern (format nil "~A~A" *ind-prefix* (incf *ind-counter*)))))
      (substrate-needs-reset *running-substrate*)
      ind)))

(defun is-new-ind-p (ind)
  (member ind *new-inds*))

(defun really-is-new-ind-p (ind)
  (let ((n (length *ind-prefix*))
        (name (if (stringp ind)
                  ind
                (symbol-name ind))))
  (and (> (length name) n)
       (string= (subseq name 0 n)
                *ind-prefix*))))

;;;
;;;
;;; 

(nrql-defun (set-new-ind-counter)
  (n)
  (cond ((and (integerp n)
              (plusp n))
         (setf *ind-counter* n))
        ((eq n :auto)
         (setf *ind-counter* 0
               *ind-prefix* (format nil"~A-" (get-universal-time))))
        ((eq n :default)
         (setf *ind-counter* 0
               *ind-prefix* "IND-"))
        (t
         (nrql-error "Bad argument ~A, need a cardinal" n))))
    
(nrql-defun (get-new-ind-counter)
  ()
  *ind-counter*)

;;;
;;;
;;; 

(nrql-defun (set-new-ind-prefix)
  (prefix)
  (setf *ind-counter* 0
        *ind-prefix* 
        (format nil "~A" prefix)))

(nrql-defun (get-new-ind-prefix)
  ()
  *ind-prefix*)

;;;
;;;
;;; 

(defmethod dl-prover-all-individuals ((substrate abduction-substrate))
  (or *candidate-individuals*
      (call-next-method)))

(defmethod dl-prover-individual-synonyms1 :around ((substrate abduction-substrate) ind)
  (if (is-new-ind-p ind)
      (list ind)
    (call-next-method)))

(defmethod dl-prover-individual-synonyms :around ((substrate abduction-substrate) ind)
  (if (is-new-ind-p ind)
      (list ind)
    (call-next-method)))

;;;
;;; Konzept-Atome
;;; 


(defmethod dl-prover-retrieve-known-concept-instances ((substrate abduction-substrate) concept)
  (declare (ignorable concept))
  nil)

(defmethod evaluate-dl-prover-retrieve-concept-instances :around ((substrate abduction-substrate)
                                                                  dl-concept continuation 
                                                                  &rest args 
                                                                  &key negated-p query
                                                                  &allow-other-keys)
  
  (declare (ignorable negated-p query))

  (let ((instances nil))

    (let ((*hypo-mode-concepts1* nil))
      (apply #'call-next-method substrate dl-concept
             #'(lambda (&rest args &key var &allow-other-keys)
                 (declare (ignorable args))
                 (push var instances))
             args))

    (let* ((new-ind 
            (when (member :one-new-ind *hypo-mode-concepts1*)
              (create-new-ind)))

           (abduction-p 
            (and *hypo-mode-concepts1*
                 (=> instances
                     (member :always-apply-modi *hypo-mode-concepts1*))
                 (not (member :no-abduction *hypo-mode-concepts1*))
                 (not *same-as-only-p*))))

      (dolist (domain
               (list instances 

                     (when (and abduction-p
                                (member :reuse-old *hypo-mode-concepts1*))
                       (dl-prover-all-individuals substrate))
                         
                     (when (and abduction-p 
                                (member :reuse-new *hypo-mode-concepts1*))
                       *new-inds*)
                         
                     (when (and abduction-p new-ind)
                       (list new-ind))))

        (dolist (ind domain)

          (cond ((eq domain instances)
                 
                 (let ((*all-assertions* 
                        (cons `(:instance ,ind ,dl-concept)
                              *all-assertions*)))

                   (apply continuation 
                          :var ind  
                          args)))

                ((not (member ind instances))
          
                 (let ((*hypothesized-assertions* 
                        (cons `(instance ,ind ,dl-concept)
                              *hypothesized-assertions*))

                       (*all-assertions* 
                        (cons `(instance ,ind ,dl-concept)
                              *all-assertions*))

                       (*new-inds*
                        (if (eq ind new-ind)
                            (cons ind *new-inds*)
                          *new-inds*))
	    
                       (*new-ind-to-var*
                        (if (eq ind new-ind)
                            (cons (list new-ind (voi query)) 
                                  *new-ind-to-var*)
                          *new-ind-to-var*)))
	                
                   (apply continuation 
                          :var ind  
                          args)))))))))

(defmethod evaluate-dl-prover-check-individual-instance-p :around ((substrate abduction-substrate)
                                                                   ind concept continuation &rest args  
                                                                   &key negated-p &allow-other-keys)
  (declare (ignorable negated-p))

  (let ((result-p nil))

    (when (not (is-new-ind-p ind))
      (let ((*hypo-mode-concepts1* nil))
        (apply #'call-next-method substrate ind concept
               #'(lambda (&rest args &key &allow-other-keys)
                   (declare (ignorable args))
                   (setf result-p t))
               args)))
    
    (let ((abduction-p 
           (and *hypo-mode-concepts1*
                (=> result-p
                    (member :always-apply-modi *hypo-mode-concepts1*))
                (not (member :no-abduction *hypo-mode-concepts1*))
                (not *same-as-only-p*))))
        
      (cond (result-p 
             (let ((*all-assertions*
                    (cons `(:instance ,ind ,concept)
                          *all-assertions*)))
               (apply continuation :var ind args)))

            (abduction-p
             (let ((*hypothesized-assertions*
                    (cons `(instance ,ind ,concept)
                          *hypothesized-assertions*))
                   (*all-assertions*
                    (cons `(instance ,ind ,concept)
                          *all-assertions*)))
             
               (apply continuation :var ind args)))))))


;;;
;;; Rollen-Atome
;;;


(defmethod dl-prover-role-successors-cache-complete-for-role-p ((substrate abduction-substrate) from role)
  (declare (ignorable substrate from role))
  ;;; wuerde sonst das Generieren von neuen Individuen unterbinden... 
  
  nil)    

(defmethod evaluate-dl-prover-check-individuals-related-p :around ((substrate abduction-substrate) 
                                                                   from to role continuation &rest args)

  ;;; Rollen-Tester: (?x ?y R) und beide bound

  (let* ((result-p nil)
         (same-as-p (is-equal-role-p role))
         (different-from-p (is-different-from-role-p role)))

    (when (and (not (is-new-ind-p from))
               (not (is-new-ind-p to)))
      (let ((*hypo-mode-roles1* nil))
        (apply #'call-next-method substrate from to role
               #'(lambda (&rest args &key &allow-other-keys)
                   (declare (ignorable args))
                   (setf result-p t))
               args)))
           
    (let ((abduction-p 
           (and *hypo-mode-roles1*
                (=> result-p
                    (member :always-apply-modi *hypo-mode-roles1*))
                (not (member :no-abduction *hypo-mode-roles1*))
                (=> *same-as-only-p* (or same-as-p different-from-p))
                (=> (or same-as-p different-from-p)
                    (or *same-as-only-p*
                        (member :same-as *hypo-mode-roles1*))))))

      (cond (result-p 
             (let ((*all-assertions*
                    (cons (cond (same-as-p
                                 `(:same-as ,from ,to))
                                (different-from-p 
                                 `(:different-from ,from ,to))
                                (t 
                                 `(:related ,from ,to ,role)))
                          *all-assertions*)))
               (apply continuation args)))

            (abduction-p 
             (let ((*hypothesized-assertions*
                    (cons (cond (same-as-p 
                                 `(same-as ,from ,to))
                                (different-from-p 
                                 `(different-from ,from ,to))
                                (t
                                 `(related ,from ,to ,role)))
                          *hypothesized-assertions*))
                   (*all-assertions*
                    (cons (cond (same-as-p
                                 `(same-as ,from ,to))
                                (different-from-p
                                 `(different-from ,from ,to))
                                (t
                                 `(related ,from ,to ,role)))
                          *all-assertions*)))
             
               (apply continuation args)))))))


(defmethod evaluate-dl-prover-retrieve-individual-fillers :around ((substrate abduction-substrate)
                                                                   from role continuation
                                                                   &rest args 
                                                                   &key  
                                                                   query
                                                                   only-one-p 
                                                                   negated-p
                                                                   inverse-p &allow-other-keys)
  (declare (ignorable query negated-p only-one-p))

  (let ((fillers nil)
        (same-as-p (is-equal-role-p role))
        (different-from-p (is-different-from-role-p role)))

    (when (not (is-new-ind-p from))
      (let ((*hypo-mode-roles1* nil))
        (apply #'call-next-method substrate from role 
               #'(lambda (&rest args &key from to inverse-p &allow-other-keys)
                   (declare (ignorable args))
                   (if inverse-p 
                       (push from fillers)
                     (push to fillers)))
               args)))

    (let* ((new-ind 
            (when (member :one-new-ind *hypo-mode-roles1*)
              (create-new-ind)))

           (abduction-p 
            (and *hypo-mode-roles1*
                 (=> fillers
                     (member :always-apply-modi *hypo-mode-roles1*))
                 (not (member :no-abduction *hypo-mode-roles1*))
                 (=> *same-as-only-p* (or same-as-p different-from-p))
                 (=> (or same-as-p different-from-p)
                     (or *same-as-only-p*
                         (member :same-as *hypo-mode-roles1*))))))

      (dolist (domain (list fillers 
                           
                            (when (and abduction-p
                                       (member :reuse-old *hypo-mode-roles1*))
                              (dl-prover-all-individuals substrate))
                         
                            (when (and abduction-p 
                                       (member :reuse-new *hypo-mode-roles1*))
                              *new-inds*)
                         
                            (when (and abduction-p new-ind)
                              (list new-ind))))

        (dolist (ind domain)
          
          (cond ((eq domain fillers)
                   
                 (let ((*all-assertions* 
                        (cons 
                         (if inverse-p 
                             (cond (same-as-p
                                    `(:same-as ,ind ,from))
                                   (different-from-p
                                    `(:different-from ,ind ,from))
                                   (t
                                    `(:related ,ind ,from ,role)))
                           (cond (same-as-p
                                  `(:same-as ,from ,ind))
                                 (different-from-p
                                  `(:different-from ,from ,ind))
                                 (t
                                  `(:related ,from ,ind ,role))))
                         *all-assertions*)))

                   (if inverse-p
                       (apply continuation :from ind args)
                     (apply continuation :to ind args))))

                ((not (member ind fillers))

                 (let* ((entry 
                         (if inverse-p 
                             (cond (same-as-p
                                    `(same-as ,ind ,from))
                                   (different-from-p
                                    `(different-from ,ind ,from))
                                   (t
                                    `(related ,ind ,from ,role)))
                           (cond (same-as-p
                                  `(same-as ,from ,ind))
                                 (different-from-p
                                  `(different-from ,from ,ind))
                                 (t
                                  `(related ,from ,ind ,role)))))
                          
                        (*all-assertions* 
                         (cons entry *all-assertions*))

                        (*hypothesized-assertions*
                         (cons entry *hypothesized-assertions*)))
                          
                   (let ((*new-inds*
                          (if (eq ind new-ind)
                              (cons ind *new-inds*)
                            *new-inds*))

                         (*new-ind-to-var*
                          (if (eq ind new-ind)
                              (cons 
                               (list 
                                new-ind
                                (if inverse-p
                                    (voi-from query) 
                                  (voi-to query)))
                               *new-ind-to-var*)
                            *new-ind-to-var*)))

                     (if inverse-p
                         (apply continuation :from ind args)
                       (apply continuation :to ind args)))))))))))

      
      
(defmethod evaluate-dl-prover-retrieve-related-individuals :around ((substrate abduction-substrate) role continuation &rest args &key query &allow-other-keys)
  
  (declare (ignorable query))

  ;;; Rollen-Generator: (?x ?y R), beide unbound! 

  (let ((same-as-p (is-equal-role-p role))
        (different-from-p (is-different-from-role-p role))
	(pairs nil))

    (let ((*hypo-mode-roles1* nil))
      (apply #'call-next-method substrate role 
             #'(lambda (&rest args &key from to &allow-other-keys)
                 (declare (ignorable args))
                 (push (list from to) pairs))
             args))

    (let* ((new-ind1
            (when (member :one-new-ind *hypo-mode-roles1*)
              (create-new-ind)))

           (new-ind2
            (when (member :one-new-ind *hypo-mode-roles1*)
              (create-new-ind)))

           (abduction-p 
            (and *hypo-mode-roles1*
                 (=> pairs
                     (member :always-apply-modi *hypo-mode-roles1*))
                 (not (member :no-abduction *hypo-mode-roles1*))
                 (=> *same-as-only-p* (or same-as-p different-from-p))
                 (=> (or same-as-p different-from-p)
                     (or *same-as-only-p*
                         (member :same-as *hypo-mode-roles1*))))))

      (dolist (pair pairs)
        (let* ((from (first pair))
               (to (second pair))
               (entry
                (cond (same-as-p
                       `(:same-as ,from ,to))
                      (different-from-p
                       `(:different-from ,from ,to))
                      (t
                       `(:related ,from ,to ,role))))
               (*all-assertions* 
                (cons entry *all-assertions*)))
          (apply continuation :from from :to to args)))

      (when abduction-p
        (let ((domains

               (list 

                (when (member :reuse-old *hypo-mode-roles1*)
                  (dl-prover-all-individuals substrate))

                (when (member :reuse-new *hypo-mode-roles1*)
                  *new-inds*)
                      
                (when new-ind1
                  (list new-ind1))))

              (ranges 

               (list 

                (when (member :reuse-old *hypo-mode-roles1*)
                  (dl-prover-all-individuals substrate))

                (when (member :reuse-new *hypo-mode-roles1*)
                  *new-inds*)
                      
                (when new-ind2
                  (list new-ind2)))))

          (dolist (domain domains)
            (dolist (from domain)
          
              (let ((*new-inds*
                     (if (eq from new-ind1)
                         (cons new-ind1 *new-inds*)
                       *new-inds*)))

                (dolist (range ranges)
                  (dolist (to range)
            
                    (let ((*new-inds*
                           (if (eq to new-ind2)
                               (cons new-ind2 *new-inds*)
                             *new-inds*)))

                      (unless (member (list from to) pairs :test #'equal)
                        (let* ((*hypothesized-assertions* 
                                (cons 
                                 (cond (same-as-p
                                        `(same-as ,from ,to))
                                       (different-from-p 
                                        `(different-from ,from ,to))
                                       (t
                                        `(related ,from ,to ,role)))
                                 *hypothesized-assertions*))
                               (*all-assertions* 
                                (cons 
                                 (cond (same-as-p 
                                        `(same-as ,from ,to))
                                       (different-from-p
                                        `(different-from ,from ,to))
                                       (t
                                        `(related ,from ,to ,role)))
                                 *all-assertions*))

                               (*new-ind-to-var*
                                (cond ((eq from new-ind1)
                                       (cons (list new-ind1 (voi-from query)) *new-ind-to-var*))
                                      ((eq to new-ind2)
                                       (cons (list new-ind2 (voi-to query)) *new-ind-to-var*))
                                      (t *new-ind-to-var*))))

                          (apply continuation :from from :to to args))))))))))))))


;;;
;;; API
;;; 

(nrql-defmacro (retrieve-with-explanation :nrql-function racer-answer-query-with-explanation
                                          :is-with-macro-p nil ; wg. WITH im Namen!
                                          ))


(nrql-defmethod (get-number-of-explanations) ((query t) &rest args &key)
  (apply #'get-answer-size query args))


(nrql-defmethod (get-explanations) ((query null) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (get-explanations) ((query symbol) &rest args &key  &allow-other-keys)
  (apply #'get-explanations (find-query query) args))


(defun assertion-inds (assertion)
  (case (first assertion)
    ;;; Keyword = Bewiesen 
    ((instance :instance) (list (second assertion)))
    ((related :related) (butlast (cdr assertion)))
    ((same-as :same-as) (cdr assertion))
    ((different-from :different-from) (cdr assertion))))
    

(defun apply-mapping (abox mapping)
  (labels ((psubs (ind)
             (let ((voi (second (assoc ind mapping))))
               (if voi
		   (if (symbolp voi)
		       voi
		     (textual-description voi))
                 ind))))

    (mapcar #'(lambda (ass)
                (ecase (first ass)
                  ((:instance instance)
                   `(,(first ass) ,(psubs (second ass)) ,(third ass)))
                  ((:related related)
                   `(,(first ass) ,(psubs (second ass)) ,(psubs (third ass)) ,(fourth ass)))
                  ((:same-as same-as :different-from different-from)
                   `(,(first ass) ,(psubs (second ass)) ,(psubs (third ass))))))
            abox)))


(defun order-fn (order-by h)
  (let* ((tuple
	  (cdr (assoc :tuple h)))	; cdr = remove marker symbols 
	 (full-tuple
	  (cdr (assoc :full-tuple h)))
	 (new-inds
	  (cdr (assoc :new-inds h)))
	 (old-inds
	  (cdr (assoc :old-inds h)))
	 (all-assertions
	  (cdr (assoc :all-assertions h)))
	 (hypothesized-assertions
	  (cdr (assoc :hypothesized-assertions h)))
	 (entailed-assertions
	  (cdr (assoc :entailed-assertions h))))
    
    (declare (ignorable tuple all-assertions))

    (case order-by
      (:new-inds (length new-inds))
      (:hypothesized-assertions (length hypothesized-assertions))
      ((:old-assertions
	:entailed-assertions)
       (length entailed-assertions))
      (:old-inds (length old-inds))
      (:paper-fn 
       (- (length old-inds)
	  (length new-inds)))
      (:new-paper-fn 
       (- (length entailed-assertions)
	  (length hypothesized-assertions)))
      ;;; :cae seems to be wrong?? default changed to cae2...
      (:cae
       (- (length entailed-assertions)
          (count-if #'(lambda (x)
                        (and (eq (first x) 'instance)
                             (is-new-ind-p (second x))
                             (eq (third x) 'top)))
                    hypothesized-assertions)))
      ;;; Korrektur CAE : TOP-Konjunkte sind neutral 
      (:cae2
       (- (count-if-not  #'(lambda (x)
                             (and (eq (first x) :instance)
                                  (eq (third x) 'top)))
                         entailed-assertions)
          (count-if-not  #'(lambda (x)
                             (and (eq (first x) 'instance)
                                  (eq (third x) 'top)))
                         hypothesized-assertions)))
      (:rasouli-paper-fn ;; Rasouli: Top-Konjunkte zaehlen negativ
       (- (length entailed-assertions)
          (length hypothesized-assertions)
          (count-if  #'(lambda (x)
                         (and (or (eq (first x) 'instance)
                                  (eq (first x) :instance))
                              (eq (third x) 'top)))
                     all-assertions)))
      (otherwise
       (evaluate1 `(,order-by ',full-tuple
			      ',new-inds
			      ',old-inds
			      ',hypothesized-assertions
			      ',entailed-assertions))))))


(defun equi-order-fn (equi-order-by h)
  (let* ((tuple
	  (cdr (assoc :tuple h)))	; cdr = remove marker symbols 
	 (full-tuple
	  (cdr (assoc :full-tuple h)))
	 (new-inds
	  (cdr (assoc :new-inds h)))
	 (old-inds
	  (cdr (assoc :old-inds h)))
	 (all-assertions
	  (cdr (assoc :all-assertions h)))
	 (hypothesized-assertions
	  (cdr (assoc :hypothesized-assertions h)))
	 (entailed-assertions
	  (cdr (assoc :entailed-assertions h))))    
    
    (declare (ignorable tuple all-assertions entailed-assertions
			hypothesized-assertions full-tuple))
		   
    (ecase equi-order-by
      (:prefer-old-inds  
       (length old-inds))
      (:prefer-new-inds  
       (length new-inds)))))


(nrql-defmethod (get-explanations) ((query nrql-query) &key 
                                    (from 0) to 

                                    (only-best-p *only-best-p*)
                                    (order-by *order-by*) 
				    (reverse-order-p *reverse-order-p*)
                                    (equi-order-by :prefer-old-inds)
                                    		       
                                    (remove-marker-symbols-p nil)
                                    (remove-entailed-explanations-p nil)

                                    (new-inds-p t)
                                    (tuples-p t)
                                    full-tuples-p
                                    all-assertions-p
                                    (hypothesized-assertions-p t) 
                                    show-score-p 
                                    abox-entailment  ; :most-specific or :most-general 
                                    ensure-permutations-p 
                                    &allow-other-keys)
  
  (let* ((hypotheses 
          (if ensure-permutations-p 
              (remove-duplicates 
               (first (hypotheses query))
               :test #'(lambda (x y) 
                         (and (= (first x) (first y)) ; score
                              (set-equal 
                               (cdr (assoc :full-tuple (second x)))
                               (cdr (assoc :full-tuple (second y)))
                               :test #'equal))))            
            (remove-duplicates
             (first (hypotheses query))
             :test #'(lambda (x y) 
                       (and (= (first x) (first y)) ; score
                            (set-equal 
                             (cdr (assoc :all-assertions (second x)))
                             (cdr (assoc :all-assertions (second y)))
                             :test #'equal))))))

         (hypotheses
          ;;; in Verbindung mit:  :always-apply-modi sinnvoll          
          (if remove-entailed-explanations-p 
              (remove-if-not #'(lambda (h)
                                 (cdr (assoc :hypothesized-assertions (second h))))
                             hypotheses)
            hypotheses))
		       
         (hypotheses 
          (cond (only-best-p
                 (let* ((min (if reverse-order-p
                                 (greatest hypotheses #'first)
                               (smallest hypotheses #'first)))
                        
                        (best-hypotheses
                         (remove-if-not #'(lambda (h)
                                            (= (first h)
                                               (first min)))
                                        hypotheses))
                        
                        (best-of-best-hypotheses
                         (mapcar #'(lambda (h)
                                     (list (equi-order-fn equi-order-by (second h))
                                           (second h)
                                           ;;; save old score: 
                                           (first h)))
                                 best-hypotheses))

                        (best-of-best 
                         (greatest best-of-best-hypotheses #'first))

                        (best-of-best
                         (remove-if-not #'(lambda (h)
                                            (= (first h)
                                               (first best-of-best)))
                                        best-of-best-hypotheses)))

                   (mapcar #'(lambda (h) 
                               (list (third h) (second h)))
                           best-of-best)))
                
                (t 
                 
                 (let* ((res (sort hypotheses
				   #'<= 
				   :key #'first)))
                   (if reverse-order-p
                       (nreverse res)
                     res)))))

         (hypotheses 
          (let ((st *type-of-substrate*))
            (setf *type-of-substrate* 'racer-dummy-substrate)
            (prog1
                (if (member abox-entailment  '(:most-specific  :most-general))
                    (funcall 
                     (ecase abox-entailment 
                       (:most-specific 
                        'get-maximum)
                       (:most-general 
                        'get-minimum))
                     hypotheses
                     :key #'(lambda (h)
                              (let* ((h (second h))
                                     (abox 
                                      (cdr (assoc :all-assertions h)))
                                     (new-ind-to-var-mapping 
                                      (cdr (assoc :new-ind-to-var-mapping h))))
                                (declare (ignorable new-ind-to-var-mapping))
                                ;; (apply-mapping abox new-ind-to-var-mapping)
                                abox)))
                  hypotheses)
              (setf *type-of-substrate* st))))

         (hypotheses 
          (mapcar #'(lambda (h)
		      (let ((h (second h))
			    (score (first h)))
			
                        (let* ((tuple
                                (cdr (assoc :tuple h)))
                               (full-tuple
                                (cdr (assoc :full-tuple h)))
                               (new-inds
                                (cdr (assoc :new-inds h)))
                               (old-inds
                                (cdr (assoc :old-inds h)))
                               (all-assertions
                                (cdr (assoc :all-assertions h)))
                               (hypothesized-assertions
                                (cdr (assoc :hypothesized-assertions h)))
                               (entailed-assertions
                                (cdr (assoc :entailed-assertions h))))
    
                          (declare (ignorable tuple all-assertions))
		      
                          `( ,@(when tuples-p
                                 (list (cons :tuple tuple)))
			     ,@(when full-tuples-p
				 (list (cons :full-tuple full-tuple)))
			     ,@(when new-inds-p
				 (list (cons :new-inds new-inds)))
			     ,@(when hypothesized-assertions-p
				 (list (cons :hypothesized-assertions hypothesized-assertions)))
			     ,@(when all-assertions-p
				 (list (cons :all-assertions all-assertions)))
			     ,@(when show-score-p
				 `((:score ,score
                                    ,order-by
                                    ((:old-inds 
                                      ,@(cons (length old-inds) old-inds))
                                     (:entailed-assertions 
                                      ,@(cons (length entailed-assertions) entailed-assertions))
                                     (:new-inds 
                                      ,@(cons (length new-inds) new-inds))
                                     (:hypothesized-assertions
                                      ,@(cons (length hypothesized-assertions) hypothesized-assertions))))))))))
                  hypotheses))

         (hypotheses
          (cond (remove-marker-symbols-p
                 (tree-remove #'(lambda (x) 
                                  (member x '(:tuple 
                                              :full-tuple
					      :new-inds
                                              :hypothesized-assertions
                                              :all-assertions)))
                              hypotheses))
                (t hypotheses)))

         (hypotheses 
          (subseq hypotheses 
                  from 
                  (when to
                    (min (length hypotheses)
                         to)))))

    hypotheses))


(nrql-defmethod (add-explanation-assertions) ((query null) (expl-no number) &rest args)
  (declare (ignorable args))
  :not-found)

(nrql-defmethod (add-explanation-assertions) ((query symbol) (expl-no number) &rest args)
  (apply #'add-explanation-assertions (find-query query) expl-no args))

(nrql-defmethod (add-explanation-assertions) ((query nrql-query) (expl-no number) &rest args)
  (let ((assertions
         (first 
          (first 
           (apply #'get-explanations query
                  :from expl-no :to (1+ expl-no)
                  :remove-marker-symbols-p  t
                  :new-inds-p nil
                  :tuples-p nil
                  args)))))

    (when assertions
      (add-abox-assertions (substrate query) assertions))))


;;;
;;;
;;;

(defun enable-abduction-debugging ()
  (untrace)
  (setf *abduction-debugging-p* t)
  (trace evaluate-subquery)
  (trace register-bindings)
  (trace evaluate-dl-prover-check-individual-instance-p)
  (trace evaluate-dl-prover-retrieve-concept-instances)
  (trace evaluate-dl-prover-check-individuals-related-p)
  (trace evaluate-dl-prover-retrieve-individual-fillers)
  (trace evaluate-dl-prover-retrieve-related-individuals))


(defun disable-abduction-debugging ()
  (untrace)
  (setf *abduction-debugging-p* nil))

