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
;;; Achtung - der Optimizer funktioniert nur fuer Queries in DNF!
;;; Der Compiler funktioniert natuerlich fuer beliebige Plaene...
;;; Wie optimiert man allgemeine Boolean Queries, die nicht in einer (D)NF sind?
;;; 

(defvar *current-plan* nil)

(defun bind-vars-to (vars val)
  (dolist (var vars)
    (unless (is-query-individual-p var)
      (setf (bound-to var) val))
    (when (corresponding-voi var)
      (unless (is-query-individual-p (corresponding-voi var))
        (setf (bound-to (corresponding-voi var))
              val)))))

;;;
;;;
;;;

(defmethod compute-plan ((query atomic-query)) 
  (setf (slot-value query 'score)
        (get-score query)))

(defmethod compute-plan ((query true-query)) 
  (setf (slot-value query 'score)
        (get-score query)))

(defmethod compute-plan ((query false-query)) 
  (setf (slot-value query 'score)
        (get-score query)))



(defmethod compute-plan ((query and-query))
  (with-slots (partial-order) query

    (let ((po nil))
      (dolist (constraint partial-order)
        (if (second constraint)
            (let ((a (if (consp (first constraint))
                         (first constraint)
                       (list (first constraint) (first constraint))))
                  (b (if (consp (second constraint))
                         (second constraint)
                       (list (second constraint) (second constraint)))))

              (loop as i from (first a) to (second a) do
                    (loop as j from (first b) to (second b) do
                          (push (list i j) po))))
          (push constraint po)))

      (setf partial-order po))

    (labels ((do-it (plans no-of-plans-to-consider)
             
               (when plans
               
                 (let ((plans (subseq (sort (copy-list plans) #'> :key #'third) 0 
                                      (min (length plans) no-of-plans-to-consider))))
                 
                   (if (not (second (first plans)))

                       (progn

                         (unless (fourth (first plans))
                           (nrql-warning "Sorry, I couldn't find a plan which respects your partial ordering ~A!" 
                                         partial-order))                                         
                         
                         (first plans))
                   
                     (do-it (mapcan #'(lambda (plan-rest-val)
                                        (mapcar #'(lambda (next)
                                                    (let* ((plan (first plan-rest-val))
                                                           (rem (second plan-rest-val))
                                                           (val (third plan-rest-val))
                                                           (valid-p (fourth plan-rest-val))

                                                           (n (length plan))
                                                           (*current-plan* plan)

                                                           (valid-p 
                                                            (and valid-p
                                                                 (every #'(lambda (constraint)
                                                                            (if plan
                                                                                ;;; (:a :b) = :a after :b in plan 
                                                                                (if (second constraint)
                                                                                    (=> (eq (first constraint) 
                                                                                            (tag-id next))
                                                                                        (and (member (second constraint) plan
                                                                                                     :key #'tag-id)
                                                                                             ;; ALLE mit der ID kamen bereits vor:
                                                                                             (not (member (second constraint) rem
                                                                                                          :key #'tag-id))))
                                                                                  ;;; (:a nil), aber start-plan existiert bereits -> muss ebenfalls :a sein!
                                                                                  (=> (eq (first constraint) 
                                                                                          (tag-id next))
                                                                                      (every #'(lambda (x) 
                                                                                                 (eq (first constraint) (tag-id x)))
                                                                                             plan)))
                                                                              ;;; (:a nil) means: :a must start the plan
                                                                              (=> (not (second constraint))
                                                                                  (eq (tag-id next) (first constraint)))))
                                                                        partial-order))))
                                                      
                                                      
                                                      #| (pprint (mapcar #'(lambda (x) 
                                                                          (list (tag-id x) x))
                                                                      (reverse (cons next plan) )))
                                                      (pprint valid-p)
                                                      (terpri)  |# 
  
                                                      (bind-vars-to (vois (parser next)) nil) ; reset all bindings
                                                             
                                                      (dolist (subquery plan) ; damit werden *alle* vois erfasst! 

                                                        #| 
                                                      (bind-vars-to
                                                       (if (is-query-reference-p subquery)
                                                           ;;; nur diese werden gebunden!
                                                           (result-vois subquery)
                                                         (all-vois subquery))
                                                       t)  |# 

                                                        ;;; es ist besser, die result-vois in 
                                                        ;;; query-reference nicht zu binden,
                                                        ;;; und daher query-references immer 
                                                        ;;; ans Ende zu stellen

                                                        (bind-vars-to
                                                         (if (is-query-reference-p subquery)
                                                             ;;; nur diese werden gebunden!
                                                             nil
                                                           (all-vois subquery))
                                                         t))
                                                                                                          
                                                      (compute-plan next)
                                                    
                                                      (let ((score 
                                                             (if valid-p
                                                                 (float 
                                                                  (+ val (/ (score next)
                                                                            (1+ n))))
                                                               -1)))
                                                      
                                                        (list (append plan (list next))
                                                              (remove next rem)
                                                              score
                                                              valid-p))))
                                                
                                                (second plan-rest-val)))
                                    plans)
                          
                            no-of-plans-to-consider))))))

      (cond ((eq *type-of-substrate* 'abduction-substrate)

             ;;; optimierung fuer abduction angepasst
             ;;; strikte atoms vorziehen, und insbesondere
             ;;; strikte same-as atome 
             ;;; evtl. die other-atoms dann optimieren 

             (let* ((strict-abduction-atoms
                     (remove-if-not #'(lambda (x)
                                        (and (is-abduction-atomic-query-p x)
                                             (strict-p x)))
                                    (subqueries query)))
           
                    (other-atoms
                     (set-difference (subqueries query) strict-abduction-atoms))

                    (strict-plan
                     (let ((strict-same-as-atoms (remove-if-not #'is-abduction-same-as-query-p strict-abduction-atoms))
                           (other-strict-atoms (remove-if #'is-abduction-same-as-query-p strict-abduction-atoms)))
                         
                       (first
                        (do-it (list (list strict-same-as-atoms other-strict-atoms 0 t))
                               (min (* 10 (length (all-subqueries query)))
                                    *optimizer-max-plans*)))))
                  
                    (plan-and-score
                     (do-it (list (list strict-plan other-atoms 0 t))
                            (min (* 10 (length (all-subqueries query)))
                                 *optimizer-max-plans*)))
                  
                    (plan (first plan-and-score))
                    
                    (score (second plan-and-score)))

               #+:ignore
               (unless (= (length plan) (+ (length strict-abduction-atoms)
                                           (length other-atoms)))
                 (break "!"))

               (setf (slot-value query 'subqueries) plan
                     (slot-value query 'score) score)))

            (t 

             ;;; neues Optimierungsschema - Lambda-Atome immer hinten anstellen!

             (let* ((lambda-atoms 
                     (remove-if-not #'is-minilisp-query-p (subqueries query)))
                    (atoms
                     (remove-if #'is-minilisp-query-p (subqueries query)))
                    
                    (plan-without-minilisp
                     (first
                      (do-it (list (list nil atoms 0 t))
                             (min (* 10 (length (all-subqueries query)))
                                  *optimizer-max-plans*))))
                    
                    (plan-and-score
                     (do-it (list (list plan-without-minilisp lambda-atoms 0 t))
                            (min (* 10 (length (all-subqueries query)))
                                 *optimizer-max-plans*)))

                    (plan (first plan-and-score))
                    
                    (score (second plan-and-score)))

               (setf (slot-value query 'subqueries) plan
                     (slot-value query 'score) score))))
    
      query)))


(defmethod compute-plan ((query or-query))
  (let* ((count 0)
	 (sqs (all-subqueries query))
	 (length (length sqs)))
    (dolist (sq sqs)
      (incf count)
      (when *debug-p*
	(format t "~% Optimizer OR: ~A %~%" (float (/ count length))))
      (compute-plan sq)))
  query)
  

;;;
;;; Bewertung
;;;

(defmethod get-score :around ((query query))

  (let ((qs (if (is-complex-query-p query)
               (cons query (all-subqueries query))
             (list query))))
    (dolist (q qs)
      (setf (slot-value q 'score) nil)))

  (if (or (exact-cache-reference query)
          (superset-cache-reference query)
          (subset-cache-reference query))
      
      ;;; bereits zur Compile-Zeit vorhandene Cache-Eintraege?
      ;;; Exploit "Database Statistics"! 
      
      ;;; (values (- 100000 (length (bindings (use-cached-bindings-of query)))) 'use-cache-entry)
      ;;; erzeugt fuerchterliche Kombinatorik! 
      
      (call-next-method)

    (call-next-method)))

;;;
;;;
;;;

(defmethod get-score ((query query))
  1)

(defmethod get-score ((query same-as-query))
  (if (negated-p query)       

      (if (bound-to (voi-from query))
          (if (bound-to (voi-to query))
              (values 100 :tester)
            (values 10 :from-is-bound-enumerator))
        (if (bound-to (voi-to query))
            (values 10 :to-is-bound-enumerator)
          (values 1 :enumerator)))

    (if (bound-to (voi-from query))
        (if (bound-to (voi-to query))
            (values 100 :tester)
          (values 100 :from-is-bound-enumerator))
      (if (bound-to (voi-to query))
          (values 100 :to-is-bound-enumerator)
        (values 1 :enumerator)))))


(defmethod get-score ((query top-query))
  (if (bound-to (voi query))
      (values 1 :tester)
    (values 1 :enumerator)))
    

(defmethod get-score ((query bottom-query))
  (if (bound-to (voi query))
      (values 1000 :tester)
    (values 1000 :enumerator)))
    

(defmethod get-score ((query query-reference))
  ;; (score (referenced-query query))
  1)

(defmethod get-score ((query minilisp-query))
  (if *optimizer-ensure-late-lambda-evaluation-p*
      (let* ((vois (all-vois query))
             (master 
              (subquery-of query))
             (queries
              (when master
                (remove query (all-subqueries master))))
             (relevant
              (or (relevant-queries query)
                  (setf (slot-value query 'relevant-queries)
                        (remove-if-not #'(lambda (x) 
                                           (intersection (all-vois x)
                                                         vois))
                                       queries)))))
        (if (subsetp relevant *current-plan*) ; schon alle Bedingungen gechecked? 
            100
          0))
    (let* ((n (length (all-vois query)))
           (bound 
            (count-if #'bound-to (all-vois query))))
      (if (zerop n)
          100
        (/ (* 100 (/ bound n)) (* n n))))))

(defmethod get-score ((query true-query))
  1000)

(defmethod get-score ((query false-query))
  1000)
 
;;;
;;; Achtung! muss angepasst werden, sobald "negated-p" vernuenftig funktioniert
;;;

#+:dlmaps
(defmethod get-score ((query substrate-simple-node-query))
  (if (bound-to (voi query))
      (values 100 :tester)
    (if (negated-p query) 
        (values 70 :enumerator)
      (values 80 :enumerator))))

#+:dlmaps
(defmethod get-score ((query substrate-simple-edge-query))
  (if (negated-p query)       
      (if (bound-to (voi-from query))
          (if (bound-to (voi-to query))
              (values 95 :tester)
            (values 60 :from-is-bound-enumerator))
        (if (bound-to (voi-to query))
            (values 60 :to-is-bound-enumerator)
          (values 20 :enumerator)))
    (if (bound-to (voi-from query))
        (if (bound-to (voi-to query))
            (values 95 :tester)
          (values 90 :from-is-bound-enumerator))
      (if (bound-to (voi-to query))
          (values 90 :to-is-bound-enumerator)
        (values 40 :enumerator)))))


#+:dlmaps
(defmethod get-score ((query substrate-predicate-node-query))
  (if (bound-to (voi query))
      (values 100 :tester)
    (if (negated-p query) 
        (values 70 :enumerator)
      (values 80 :enumerator))))

#+:dlmaps
(defmethod get-score ((query substrate-predicate-edge-query))
  (if (negated-p query)       
      (if (bound-to (voi-from query))
          (if (bound-to (voi-to query))
              (values 95 :tester)
            (values 60 :from-is-bound-enumerator))
        (if (bound-to (voi-to query))
            (values 60 :to-is-bound-enumerator)
          (values 20 :enumerator)))
    (if (bound-to (voi-from query))
        (if (bound-to (voi-to query))
            (values 95 :tester)
          (values 90 :from-is-bound-enumerator))
      (if (bound-to (voi-to query))
          (values 90 :to-is-bound-enumerator)
        (values 40 :enumerator)))))

;;;
;;;
;;;

(defmethod get-score ((query instance-retrieval-query))
  (if (bound-to (voi query))
      (values 99 :tester)
    
    (if (negated-p query) 
        (values 30 :enumerator)


      ;;; [51 ; 55[ 

      (values 

       (if *optimizer-use-cardinality-heuristics-p* 
           
           (float

            (loop as concept in (let ((concept (dl-concept query)))
                                  (if (and (consp concept) 
                                           (eq (first concept) 'and))
                                      (rest concept)
                                    (list concept)))
                  maximize 
            
                  (let* ((*told-information-reasoning-p* t)
                         (n (length
                             (dl-prover-retrieve-known-concept-instances (substrate (parser query))
                                                                         'top))))
              
                    (if (zerop n)
                  
                        51
                
                      (multiple-value-bind (all all-known)
                          (dl-prover-retrieve-concept-instances
                           (substrate (parser query))
                           concept)

                        (when nil 
                          (when all-known 
                            (princ concept)
                            (princ " all ") (princ (length all))
                            (terpri)))

                        (if all-known

                            ;; (+ 53 (- 2 (* 2 (/ (length all) n))))
                            (+ 51 (- 2 (* 2 (/ (length all) n))))

                          (multiple-value-bind (known known-known)
                              (dl-prover-retrieve-known-concept-instances
                               (substrate (parser query))
                               concept)

                            (when nil 
                              (when known-known 
                                (princ concept)
                                (princ " known ") (princ (length known)) (terpri)))

                            (if known-known

                                (+ 51 (- 2 (* 2 (/ (length known) n))))

                              51))))))))
         
         51)

       :enumerator))))


(defmethod get-score ((query has-known-successor-retrieval-query))
  (if (bound-to (voi query))
      (values 100 :tester)
    (if (negated-p query) 
        (values 40 :enumerator)
      (values 50 :enumerator))))


(defmethod get-score ((query edge-retrieval-query))
  (if (and (tbox (substrate query))
           (dl-prover-transitive-role-p (substrate query)
                                        (dl-role query)))
      
      (if (negated-p query) 
          (if (bound-to (voi-from query))
              (if (bound-to (voi-to query))
                  (values 94 :tester)
                (values 17 :from-is-bound-enumerator))
            (if (bound-to (voi-to query))
                (values 17 :to-is-bound-enumerator)
              (values 0 :enumerator)))

        (if (bound-to (voi-from query))
            (if (bound-to (voi-to query))
                (values 95 :tester)
              (values 55 :from-is-bound-enumerator))
          (if (bound-to (voi-to query))
              (values 55 :to-is-bound-enumerator)
            (values 5 :enumerator))))


    (if (negated-p query) 
        (if (bound-to (voi-from query))
            (if (bound-to (voi-to query))
                (values 98 :tester)
              (values 20 :from-is-bound-enumerator))
          (if (bound-to (voi-to query))
              (values 20 :to-is-bound-enumerator)
            (values 0 :enumerator)))
      (if (bound-to (voi-from query))
          (if (bound-to (voi-to query))
              (values 98 :tester)
            (values 60 :from-is-bound-enumerator))
        (if (bound-to (voi-to query))
            (values 60 :to-is-bound-enumerator)
          (values 10 :enumerator))))))


(defmethod get-score ((query racer-cd-edge-retrieval-query))
  (if (and (bound-to (voi-from query))
           (bound-to (voi-to query)))
      (values 95 :tester)
    (if (bound-to (voi-from query))
        (values 40
                :to-is-bound-enumerator)
      (if (bound-to (voi-to query))
          (values 40 
                  :from-is-bound-enumerator)
        (values 0 :enumerator)))))

;;;
;;;
;;;

(defmethod optimize-query ((query complex-query) &rest args &key show-plan-p &allow-other-keys)
  (declare (ignorable args))

  (let* ((conjuncts (slot-value query 'subqueries))
         (n (length conjuncts))
         (*optimizer-max-plans* *optimizer-max-plans*))			  

    (bind-vars-to (vois (parser query)) nil)

    (loop 
      (with-non-interruptable-timeout ;;; wichtig f. ACL, sonst Problem Windows
	  (*optimizer-spend-at-most-n-seconds*
	   (progn 
	     (setf (slot-value query 'subqueries) conjuncts)
	     (nrql-warning "Couldn't optimize query with ~A subqueries within ~A seconds - auto-reducing number of plans to consider to ~A"
                           n
			   *optimizer-spend-at-most-n-seconds*
			   (setf *optimizer-max-plans* 
			     (floor *optimizer-max-plans* 2)))))
  
	(if (zerop *optimizer-max-plans*)
	    (progn 
	      (nrql-warning "Couldn't optimize query with ~A subqueries. Query too hard, giving up. Using unoptimized query" n)
	      (setf (slot-value query 'subqueries) conjuncts)
	      (return))
	  (compute-plan query))
	
	(return)))
  
    (bind-vars-to (vois (parser query)) nil)

    (dolist (sq (cons query (all-subqueries query)))
      (setf (slot-value sq 'is-optimized-p) t))

    (recompute-roles query)
 
    (when show-plan-p 
      (format t "~%~%Computed Plan: ~A~%~%" (subqueries query)))

    query))


(defmethod optimize-query ((query query) &rest args &key &allow-other-keys)
  (declare (ignorable args))

  (compute-plan query)

  (recompute-roles query)

  query)

(defmethod optimize-query ((query true-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))

  query)

(defmethod optimize-query ((query false-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))

  query)


;;;
;;;
;;;

(defmethod recompute-roles ((query and-query))
  (let ((plan (subqueries query)))
    (let ((first (first plan)))
      (bind-vars-to (vois (parser first)) nil)
                                                             
      (dolist (subquery plan)
        (multiple-value-bind (score role)
            (get-score subquery)
          (declare (ignorable score))
          (setf (slot-value subquery 'current-role) role))

        (bind-vars-to
         (if (is-query-reference-p subquery)
             ;;; nur diese werden gebunden!
             (result-vois subquery)
           (all-vois subquery))
         t)))))


(defmethod recompute-roles ((query or-query))
  (dolist (sq (subqueries query))
    (recompute-roles sq)))


(defmethod recompute-roles ((query atomic-query))
  (multiple-value-bind (score role)
      (get-score query)
    (declare (ignorable score))
    (setf (slot-value query 'current-role) role)))

