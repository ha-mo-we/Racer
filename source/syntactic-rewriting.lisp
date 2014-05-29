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

#+:dlmaps
(defmethod syntactically-rewrite-atomic-query (query (parser simple-parser) &rest args
                                                     &key
                                                     replace-inds-with-vars-p
                                                     negated-p inverse-p &allow-other-keys)
  (declare (ignorable args))
  
  (labels ((gvi (ind)
             (if replace-inds-with-vars-p
                 (get-var-for-ind parser ind)
               ind)))
    
    (let* ((inds (get-inds-from parser query))

           (query
          
            (cond ((top-query-p parser query)
                   (prog1
                       (if negated-p                     
                           `(:bottom ,(gvi (second query)))
                         `(:top ,(gvi (second query))))
                     (setf negated-p nil)))
               
                  ((bottom-query-p parser query)
                   (prog1
                       (if negated-p                     
                           `(:top
                             ,(gvi (second query)))
                         `(:bottom
                           ,(gvi (second query))))
                     (setf negated-p nil)))

                  ((true-query-p parser query)
                   (prog1
                       (if negated-p                     
                           :false-query
                         :true-query)
                     (setf negated-p nil)))

                  ((false-query-p parser query)
                   (prog1
                       (if negated-p                     
                           :true-query
                         :false-query)
                     (setf negated-p nil)))

                  (t query))))

      (values 
       (if negated-p 
           (if inverse-p 
               `(:not (:inv ,query))
             `(:not ,query))
         (if inverse-p
             `(:inv ,query)
           query))
       inds))))



(defmethod syntactically-rewrite-atomic-query (query (parser nrql-abox-query-parser) 
                                                     &rest args 
                                                     &key negated-p inverse-p 
                                                     replace-inds-with-vars-p
                                                     additional-head-inds
                                                     rewrite-defined-concepts-p
                                                     &allow-other-keys)

  (declare (ignorable args))
  
  ;;;
  ;;; fuers RACER-Dummy-Substrate:
  ;;; 
  ;;; (?x ?y   (:constraint (has-father age) (has-mother age) =)) ->
  ;;; (and (?x $?x-has-father) 
  ;;;      (?y $?y-has-mother)
  ;;;      ($?x-has-father $?y-has-mother (:constraint age age =)))
  ;;; (?x NIL R) -> (not (?x (:has-known-successor R)))
  ;;; (NIL ?x R) -> (not (?x (:has-known-successor (INV R))))
  ;;; 

  (labels (#| (make-some-chain (feature-list)
                               (if feature-list
                                   `(some ,(first feature-list)
                                          ,(make-some-chain (rest feature-list)))
                                 'top)) |# 

           (gvi (ind)
             (if replace-inds-with-vars-p
                 (get-var-for-ind parser ind)
               ind)))

    (let* ((inds (append (get-inds-from parser query)
                         additional-head-inds))

           (racer-rewritten-p nil)
           
           (query 
         
            (cond ((top-query-p parser query)
                   (prog1
                       (if negated-p                     
                           `(:bottom
                             ,(gvi 
                               (second query)))
                         `(:top
                           ,(gvi 
                             (second query))))
                     (setf negated-p nil)))
               
                  ((bottom-query-p parser query)
                   (prog1
                       (if negated-p                     
                           `(:top
                             ,(gvi 
                               (second query)))
                         `(:bottom
                           ,(gvi 
                             (second query))))
                     (setf negated-p nil)))
                  
                  ;;;
                  ;;;
                  ;;;
                  
                  ((true-query-p parser query)
                   (prog1
                       (if negated-p                     
                           :false-query
                         :true-query)
                     (setf negated-p nil)))

                  ((false-query-p parser query)
                   (prog1
                       (if negated-p                     
                           :true-query
                         :false-query)
                     (setf negated-p nil)))

                  ;;;
                  ;;;

                  ((bind-individual-query-p parser query)
                   ;;;
                   ;;; Bind-Individual Queries werden in syntactically-rewrite-QUERY wegtransformiert!
                   ;;; hier ist nur wichtig, dass ?x -> ?*x ersetzt wird
                   ;;; 
                   `(:bind-individual ,(second query)))

                  ;;;
                  ;;;
                  ;;;

                  ((taged-query-p parser query)
                   `(:tag ,(second query) ,(third query)))

                  ;;;
                  ;;;
                  ;;;

                  ((same-as-query-p parser query)

                   (cond ((and (ind-p parser (second query))
                               (var-p parser (third query)))
                          
                          `(:same-as 
                            ,(third query)
                            ,(second query)))
                         
                         (t `(:same-as 
                              ,(second query)
                              ,(third query)))))

                  ;;;
                  ;;;
                  ;;;
                 
                  ((has-known-successor-retrieval-query-p parser query)
                   `(,(gvi 
                       (first query))
                     ,(second query)))

                  ((loom-no-role-filler-query-p parser query)
                   ;; not a binary-query-p, wegen NIL!
                   (setf negated-p (not negated-p))
                   (setf inverse-p nil)

                   (if (not (first query)) ; (NIL ?x R) ?
                    
                       `(,(gvi (second query))
                         (:has-known-successor (inv ,(third query))))
                  
                     `(,(gvi (first query))
                       (:has-known-successor ,(third query)))))

                  ;;;
                  ;;;
                  ;;;

                  ((unary-query-p parser query)
                   (if (abox-thing-p parser (first query))

                       (if (symbolp (second query))
                           
                           (let ((rewritten 
                                  (and rewrite-defined-concepts-p
                                       #-:dlmaps
                                       (rewrite-concept (second query) 
                                                        (tbox (substrate parser))
                                                        (first query)))))
                             
                             (if (or (null rewritten) (symbolp rewritten))
                                            
                                 `(,(gvi 
                                     (first query))
                                   ,(replace-syntactic-concept-expression-sugar parser (second query)))

                               
                               (progn 
                                 (setf racer-rewritten-p t)
                                 rewritten)))

                         `(,(gvi 
                             (first query))
                           ,(replace-syntactic-concept-expression-sugar parser (second query))))
                         

                     `(,(gvi 
                         (first query))
                       ,(second query))))
                     
                  ((binary-query-p parser query)
                
                   (cond ((valid-original-constraint-description-p parser (third query))

                          ;;; (?x ?y (:constraint (has-father has-name) (has-name) string=))

                          (if (cddr (third query)) ; alte Syntax (:constraint age age (= (+ age-1 20) age-2)) etc. 

                              (let* ((from 
                                      (gvi
                                       (if inverse-p (second query) (first query))))
                                     (to 
                                      (gvi
                                       (if inverse-p (first query) (second query))))

                                     (constraint (rest (third query)))

                                     (from-chain (ensure-list (if inverse-p (second constraint) (first constraint))))
                                     (to-chain (ensure-list (if inverse-p (first constraint) (second constraint))))
                                 
                                     (from-attrib (first (last from-chain)))
                                     (to-attrib (first (last to-chain)))
                                     ;; sind entweder Attributes oder Datatype Properties!

                                     (from-is-attrib-p (cd-attribute-p from-attrib (tbox (substrate parser))))
                                     (to-is-attrib-p (cd-attribute-p to-attrib (tbox (substrate parser))))

                                     ;; entweder Attribute oder Datatype-Property!
                                     ;; bei Datatype-Properties darf die letzte Rolle nicht als Role Atom 
                                     ;; angelegt werden, weil die nicht unter (all-role-assertions) von
                                     ;; Racer zurueckgegeben werden                                 
                                 
                                     (from-features 
                                      (if from-is-attrib-p
                                          (butlast from-chain)
                                        from-chain))

                                     (to-features
                                      (if to-is-attrib-p
                                          (butlast to-chain)
                                        to-chain))

                                     (from-attrib-neu
                                      (if from-is-attrib-p
                                          from-attrib
                                        (get-attribute-for-datatype-property parser from-attrib)))
                                 
                                     (to-attrib-neu
                                      (if to-is-attrib-p
                                          to-attrib
                                        (get-attribute-for-datatype-property parser to-attrib)))
                                 
                                     (from-vois (mapcar #'(lambda (from1 from2)
                                                            (declare (ignorable from1 from2))
                                                            (make-abox-var parser 
                                                                           (get-ano-var (string-transform "chain"))
                                                                           t))
                                                        (cons from from-features)
                                                        from-features))

                                     (to-vois (mapcar #'(lambda (to1 to2)
                                                          (declare (ignorable to1 to2))
                                                          (make-abox-var parser
                                                                         (get-ano-var (string-transform "chain"))
                                                                         t))
                                                      (cons to to-features)
                                                      to-features))
                                 
                                     (predicate (third constraint))
				     
                                     (predicate 

                                      (if (consp predicate)

                                          (tree-map #'(lambda (x)

                                                        ;;; alte Syntax
                                                
                                                        (or 

                                                         (cond ((eq x from-attrib)
                                                  
                                                                (cond (from-is-attrib-p 

                                                                       x)

                                                                      (t
								       
								       (if (eq from-attrib-neu to-attrib-neu)
									   (intern (format nil "~A-1" from-attrib-neu))
									 from-attrib))))

                                                               ((eq x to-attrib)
                                                                (cond (to-is-attrib-p 

                                                                       x)

                                                                      (t
								       
								       (if (eq from-attrib-neu to-attrib-neu)
									   (intern (format nil "~A-2" to-attrib-neu))
									 to-attrib-neu)))))
							 
                                                         (and (eq from-attrib to-attrib)

                                                              (let ((attrib-1
                                                                     (intern (format nil "~A-1" from-attrib)))
                                                                    (attrib-2 
                                                                     (intern (format nil "~A-2" from-attrib))))

                                                                (cond ((eq x attrib-1)
                                                  
                                                                       (cond (from-is-attrib-p 

                                                                              attrib-1)

                                                                             (t
                                                                              (intern 
                                                                               (format nil 
                                                                                       "~A-1"
                                                                                       (get-attribute-for-datatype-property parser from-attrib))))))
                                                              
                                                                      ((eq x attrib-2)
                                                  
                                                                       (cond (to-is-attrib-p 

                                                                              attrib-2)

                                                                             (t
                                                                              (intern 
                                                                               (format nil 
                                                                                       "~A-2"
                                                                                       (get-attribute-for-datatype-property parser to-attrib)))))))))

                                                         (if inverse-p 
                                                             (ecase x
                                                               (< '>)
                                                               (> '<)
                                                               (<= '>=)
                                                               (>= '<=)
                                                               (otherwise x))
                                                           x)))

                                                    predicate)

                                        (if inverse-p 
                                            (ecase predicate
                                              (< '>)
                                              (> '<)
                                              (<= '>=)
                                              (>= '<=)
                                              (otherwise predicate))
                                          predicate))))

                                
                                (declare (ignorable from-is-attrib-p to-is-attrib-p))
				
                                (prog1 
                                    (simplify-boolean-expression
                                     (let ((pred                               
                                            `(,(if from-vois
                                                   (first (last from-vois))
                                                 from)
                                              ,(if to-vois
                                                   (first (last to-vois))
                                                 to) 
                                              (:constraint ,from-attrib-neu ,to-attrib-neu ,predicate) ;; alte Syntax
                                              )))
				       
				       
                                       (if negated-p 
					   
					   (if (or from-features to-features)
					   
					       `(:not
						 (:project-to 
						  (,from ,to)
                                                  (:and
					       
                                                   ,@(mapcar #'(lambda (a b feature)
                                                                 `(,a ,b ,feature))
                                                             (cons from from-vois)
                                                             from-vois
                                                             from-features)
					       
                                                   ,@(mapcar #'(lambda (a b feature)
                                                                 `(,a ,b ,feature))
                                                             (cons to to-vois)
                                                             to-vois
                                                             to-features)
					       
                                                   ,pred)))
					     
					     
                                             `(:not  ,pred))
					 
                                         `(:and
                                    
                                           ,@(mapcar #'(lambda (a b feature)
                                                         `(,a ,b ,feature))
                                                     (cons from from-vois)
                                                     from-vois
                                                     from-features)
                                    
                                           ,@(mapcar #'(lambda (a b feature)
                                                         `(,a ,b ,feature))
                                                     (cons to to-vois)
                                                     to-vois
                                                     to-features)
                                    
                                           ,pred)))
                                     t)
                           
                                  (setf negated-p nil
                                        inverse-p nil)))

                            ;;; neue Syntax: (:constraint (= (age ?x) (age ?y))) 

                            (parser-error "New syntax for constraint query atoms will be supported in the near future")

                            ))
                      
                         (t 

			  ;; Spezial-regelung für nRQL-Equal role: Individuen 
			  ;; werden niemals gegen repraesentative ersetzt! 
			  
			  (cond ((is-top-role-p (third query))
                                 
                                 ;;; erneut normalisieren:
                                 (setf racer-rewritten-p t)

                                 `(and (,(second query) top)
                                       (,(first query) top)))

                                ((is-bottom-role-p (third query))

                                 ;;; erneut normalisieren:
                                 (setf racer-rewritten-p t)
                                 
                                 `(and (,(second query) bottom)
                                       (,(first query) bottom)))

                                ((is-equal-role-p (third query))
			      
                                 (if inverse-p 
                                     `(,(second query)
                                       ,(first query)
                                       (inv ,(third query)))
                                   query))

                                ((and (negated-dl-role-p (third query))
                                      (is-equal-role-p (second (third query))))

                                 (if inverse-p 
                                     `(,(second query)
                                       ,(first query)
                                       ,(first +different-from-roles+))
                                   `(,(first query)
                                     ,(second query)
                                     ,(first +different-from-roles+))))
                                
                                ((is-different-from-role-p (third query))
			      
                                 (if inverse-p 
                                     `(,(second query)
                                       ,(first query)
                                       (inv ,(third query)))
                                   query))

                                ((and (negated-dl-role-p (third query))
                                      (is-different-from-role-p (second (third query))))

                                 (if inverse-p 
                                     `(,(second query)
                                       ,(first query)
                                       ,(first +equal-roles+))
                                   `(,(first query)
                                     ,(second query)
                                     ,(first +equal-roles+))))
                                
                                (t
			  
                                 (if inverse-p 
                                     `(,(gvi (second query))
                                       ,(gvi (first query))
                                       (inv ,(third query)))
                                   `(,(gvi (first query))
                                     ,(gvi (second query))
                                     ,(third query))))))))
                
                  (t query))))

      (values
       (if negated-p
           `(:not ,query)
         query)
       inds
       racer-rewritten-p))))
        
;;;
;;;
;;;

(defmethod syntactically-rewrite-query (query (parser simple-parser) &rest args 
                                              &key 
                                              prevent-query-expansion-p 
                                              (remove-redundant-projections-p t)
                                              (add-same-as-conjuncts-p t)
                                              (replace-inds-with-vars-p t)
                                              (auto-add-top-conjuncts-p *auto-add-top-conjuncts-p*)
                                              (keep-expanded-predicates-p *keep-expanded-predicates-p*)
                                              strict-same-as-p ; only needed for abduction... 
                                              &allow-other-keys)

  ;;; 
  ;;; Bringt Query in NNF, sorgt dafuer dass alle Disjunkte gleiche Stelligkeit haben
  ;;; Bsp.: (OR (?X C) (?Y D)) -> (OR (AND (?X C) (TOP ?Y)) (AND (?Y D) (?X TOP)))
  ;;; 
  ;;; Zudem: (betty woman) -> (and ($?x-betty woman) (:same-as $?x-betty betty))
  ;;; (:bind-individual betty) -> (and (:same-as $?x-betty betty))
  ;;; Auffaltung von definierten Queries: 
  ;;; Wenn (defquery mother (?x ?y) (and (?x woman) (?x ?y has-child))), 
  ;;; dann (:substitute (mother ?x ?child)) -> (and (?x woman) (?x ?child has-child))
  ;;; 

  (let ((expanded-atoms nil))

    (labels ((normalize-disjunction (expr)
               (let* ((expr (simplify-boolean-expression expr t))
                      (all-vois 
                       (get-vois-from parser expr :stop-at-projections-p t))
                      (subexpressions
                       (mapcar #'(lambda (expr)
                                   (let* ((vois (get-vois-from parser expr :stop-at-projections-p t))
                                          (diff (set-difference all-vois vois)))
                                     (if diff
                                         `(:and ,@(mapcar #'(lambda (voi)
                                                              `(:top ,voi))
                                                          diff)
                                           ,expr)
                                       expr)))
                               (get-subexpressions parser expr))))

                 `(:or ,@subexpressions)))

             (gvi (ind)
               (if replace-inds-with-vars-p
                   (get-var-for-ind parser ind)
                 ind))
           
             (add-same-as-conjuncts (rexpr inds negated-p)
               (if (not negated-p) 
                   `(:and ,@(mapcar #'(lambda (ind)
                                        (if strict-same-as-p 
                                            `(:strict (:same-as ,(gvi ind) ,ind))
                                          `(:same-as ,(gvi ind) ,ind)))
                                    inds)
                     ,rexpr)
               
                 (normalize-disjunction 
                  `(:or ,@(mapcar #'(lambda (ind)
                                      (if strict-same-as-p 
                                          `(:strict (:not (:same-as ,(gvi ind) ,ind)))
                                        `(:not (:same-as ,(gvi ind) ,ind))))
                                  inds)
                    ,rexpr))))

             (do-it (expr &optional inverse-p negated-p defs pvois)
	     
               ;;; (terpri) (write expr) (terpri)
             
               (if (not (consp expr))
                 
                   (cond ((or (true-query-p parser expr)
                              (false-query-p parser expr))

                          (apply #'syntactically-rewrite-atomic-query expr parser 
                                 :negated-p negated-p 
                                 args))

                         (t expr))
               
                 (cond ((not-query-p parser expr)
                      
                        (if (projection-operator-p parser (second expr))
                            (let ((expr
                                   (do-it (second expr) inverse-p (not negated-p) defs
                                          pvois)))
                              (if (projection-operator-p parser (second expr))
                                  expr
                                ;;; redundantes project-to wurde entfernt, erneut in NNF bringen! 
                                (do-it expr inverse-p negated-p defs
                                       pvois)))

                          (do-it (second expr) inverse-p (not negated-p) defs
                                 pvois)))

                       ((inv-query-p parser expr)
                        (do-it (second expr)
                               (not inverse-p)
                               negated-p
                               defs
                               pvois))

                       ((strict-query-p parser expr)
                        `(:strict
                          ,(do-it (second expr) inverse-p negated-p defs pvois)))

                       ((taged-query-p parser expr)
                        `(:tag
                          ,(do-it (second expr) inverse-p negated-p defs pvois)
                          ,(third expr)))

                       ((projection-operator-p parser expr)
                      
                        ;; (pprint "PI: ")
                        ;; (pprint expr) (terpri)

                        (let* ((pvois
                                (second expr))
                              
                               #| (if pvois ; falsch! 
                                    (intersection 
                                     (second expr)
                                     pvois)
                                  (second expr)) |# 

                               (expr (do-it (third expr)
                                            inverse-p
                                            nil 
                                            defs
                                            pvois))

                               (res

                                (if (projection-operator-p parser expr)
                            
                                    (if (null pvois)

                                        ;;; war entweder schon (project-to () (...)), oder durch
                                        ;;; (project-to (?x) (project-to (?y) (...)))

                                        `(:project-to nil ,expr)

                                      ;;; (project-to (?x ?z) (project-to (?x ?y ?z) (...)) ->
                                      ;;; (project-to (?x ?z) (...))
                                    
                                      (let* ((vois-in-query (get-vois-from parser 
                                                                           expr
                                                                           :stop-at-projections-p t))
                                       
                                             (missing-vois (set-difference pvois vois-in-query)))

                                      
                                        (if (every #'(lambda (x) 
                                                       (some #'(lambda (y) 
                                                                 (eq y (get-corresponding-voi parser x)))
                                                             vois-in-query))
                                                   missing-vois)

                                            ;;; alles OK, automatisch TOP addieren

                                            (let ((expr
                                                   `(:project-to ,(intersection 
                                                                   (append pvois missing-vois)
                                                                   (second expr))
                                                     (:and
                                                      ,@(mapcar #'(lambda (voi)
                                                                    `(:top ,voi))
                                                                missing-vois)
                                                      ,(third expr)))))

                                              ;;; und nochmal vereinfachen: 

                                              (do-it expr inverse-p nil defs pvois)))))

                                  
                                  (let* ((vois-in-query (get-vois-from parser 
                                                                       expr
                                                                       :stop-at-projections-p t))
                                       
                                         (missing-vois (set-difference pvois vois-in-query)))

                                    ;; (format t  "now: ~S ~S ~S ~S" expr  missing-vois vois-in-query pvois)
                                    ;; (terpri)
                                  
                                    (cond ((not missing-vois)
                                         
                                           (if (set-equal vois-in-query 
                                                          pvois)

                                               ;;; (project-to (?x) (?x c)) -> (?x c)
                                               ;;; (Projektion redundant) 

                                               (if remove-redundant-projections-p
                                                   expr
                                                 `(:project-to ,pvois ,expr))
                                           
                                             (multiple-value-bind (new-body subs)
                                                 (substitute-vois-in expr
                                                                     (mapcar #'(lambda (x)
                                                                                 (list x x))
                                                                             pvois)
                                                                     :parser parser
                                                                     :dont-anonymize-inds-p t)

                                               `(:project-to ,(mapcar #'(lambda (x) 
                                                                          (second 
                                                                           (assoc 
                                                                            (gvi x)
                                                                            subs)))
                                                                      pvois)
                                                 ,new-body))))
				      
                                        
                                          ((every #'(lambda (x) 
                                                      (some #'(lambda (y) 
                                                                (eq y (get-corresponding-voi parser x)))
                                                            vois-in-query))
                                                  missing-vois)

                                           ;;; alles OK, automatisch TOP addieren

                                           (let ((expr
                                                  `(:project-to 
                                                    ,pvois
                                                    (:and
                                                     ,@(mapcar #'(lambda (voi)
                                                                   `(:top ,voi))
                                                               missing-vois)
                                                     ,expr))))
					   
                                             (do-it expr inverse-p nil defs pvois)))

                                          (t 
                                         
                                           
                                           (if (not auto-add-top-conjuncts-p)
                                               (if (cdr missing-vois)
                                                   (parser-error "Objects ~A not mentioned in query body ~A" 
                                                             missing-vois
                                                             expr)
                                                 (parser-error "Object ~A not mentioned in query body ~A" 
                                                               (first missing-vois)
                                                               expr))

                                             (do-it

                                              `(:project-to ,pvois (and ,expr
                                                                        ,@(mapcar #'(lambda (x)
                                                                                      `(:top ,x))
                                                                                  missing-vois)))
                                              inverse-p negated-p defs pvois))))))))
                          
                          (if negated-p
                              `(not ,res)
                            res)))

                       ;;;
                       ;;; Defined Queries
                       ;;; 

                       ((defined-query-p parser expr)
                        (let* ((name (first (second expr)))
                               (vars (rest (second expr)))
                               (def (get-variable-substituted-query 
                                     (substrate parser)
                                     name
                                     vars 
                                     :parser parser))
                               (sym (gensym)))

                          (push (list sym (first (second expr))) expanded-atoms)

                          (if (member name defs)
                              (unless *lazy-query-unfolding-p*
                                (parser-error 
                                 "Cyclic definition ~A detected" name))
                            
                            (let* ((*disable-defined-queries-p* 
                                    (or *disable-defined-queries-p* 
                                        *lazy-query-unfolding-p*))
                                   (expansion
                                    (do-it def inverse-p negated-p 
                                           (cons name defs)
                                           pvois)))
                            
                              (if keep-expanded-predicates-p
                                  (let ((sexpr 
                                         `(and (,@(cdr (second expr)) ,sym)
                                               ,expansion)))
                                    sexpr)
                                expansion)))))

                       ((and (probably-defined-query-p parser expr)
                             (not prevent-query-expansion-p))
                      
                        ;;; neue Syntax: (?x ?y ?z defined-query)!
                        ;;; von Ralf gewuenscht
                      
                        (do-it `(:substitute (,(first (last expr))
                                              ,@(butlast expr)))
                               inverse-p negated-p 
                               defs
                               pvois))

                       ;;;
                       ;;; Minilisp Queries
                       ;;; 
                     
                       ((minilisp-query-p parser expr)

                        (when negated-p 
                          (parser-error "Negated lambda atoms not permitted"))

                        (cond ((consp (first expr)) ;; mit Lambda-Liste

                               (let* ((lambda-vars (second (first expr)))
                                      (lambda-body (cddr (first expr)))
                                      (lambda-apply-to (rest expr))
                                      (var-vals
                                       (mapcar #'(lambda (x y)
                                                   (cond ((abox-thing-p parser y)
                                                          (list x (gvi y)))

                                                         ((substrate-thing-p parser y)
                                                          (list x (gvi y)))
                                                
                                                         ((is-attribute-abox-projector-thing-p parser y)
                                                          (list (gensym) (gvi (second y)) x y))
                                                
                                                         ((is-told-value-abox-projector-thing-p parser y)
                                                          (list (gensym) (gvi (second (second y))) x y))
                                                
                                                         ((is-told-value-if-exists-abox-projector-thing-p parser y)
                                                          (list (gensym) (gvi (second (second y))) x y))

                                                         ((is-datatype-fillers-abox-projector-thing-p parser y)
                                                          (list (gensym) (gvi (second (second y))) x y))
                                                
                                                         ((is-annotation-datatype-fillers-abox-projector-thing-p parser y)
                                                          (list (gensym) (gvi (second (second y))) x y))
                                               
                                                         (t (parser-error 
                                                             "Unknown expression ~A found" expr))))
                                     
                                               lambda-vars 
                                               lambda-apply-to)))
                        
                                 ;;; (pprint var-vals)

                                 ;;; nach MiniLisp transformieren 
                       
                                 `((:lambda ,(mapcar #'first var-vals)
                                     (let (,@(remove 
                                              nil
                                              (mapcar #'(lambda (x)
                                                          (when (third x) ; t? 
                                                          
                                                            (let ((y (fourth x)))

                                                              (cond ((is-attribute-abox-projector-thing-p parser y)
                                                                     (list (third x)
                                                                           `(attribute-fillers ,(first x) ',(first y))))
                                                                
                                                                    ((is-told-value-abox-projector-thing-p parser y)
                                                                     (list (third x)
                                                                           `(told-value ,(first x) ',(first (second y)))))
                                                                
                                                                    ((is-told-value-if-exists-abox-projector-thing-p parser y)
                                                                     (list (third x)
                                                                           `(told-value-if-exists ,(first x) ',(first (second y)))))
                                                                   
                                                                    ((is-datatype-fillers-abox-projector-thing-p parser y)
                                                                     (list (third x)
                                                                           `(datatype-fillers ,(first x) ',(first (second y)))))

                                                                    ((is-annotation-datatype-fillers-abox-projector-thing-p parser y)
                                                                     (list (third x)
                                                                           `(annotation-datatype-fillers ,(first x) ',(first (second y)))))))))
                                               
                                                      var-vals))) 
                             
                                       ,@lambda-body))

                                   ,@(mapcar #'second var-vals))))

                              (t ;; ohne Lambda-Liste

                                 (let ((body
                                        (tree-map #'(lambda (x) 
                                                      (if (var-p parser x)
                                                          (gvi x)
                                                        x))
                                                  (rest expr))))

                                   `(:lambda ,@body)))))
                                 
                     
                       ;;;
                       ;;; Spez. Syntax
                       ;;; 
                           
                       ((bind-individual-query-p parser expr)
                        (multiple-value-bind (rexpr inds)
                            (apply #'syntactically-rewrite-atomic-query expr parser 
                                   :negated-p negated-p 
                                   :inverse-p inverse-p 
                                   :replace-inds-with-vars-p 
                                   replace-inds-with-vars-p 
                                   args)
                          (declare (ignore rexpr))

                          (if negated-p
                              `(:not (:same-as 
                                      ,(gvi (first inds))
                                      ,(first inds)))
                            `(:same-as 
                              ,(gvi (first inds))
                              ,(first inds)))))

                       ((same-as-query-p parser expr)

                        (multiple-value-bind (rexpr inds)
                            (apply #'syntactically-rewrite-atomic-query expr parser 
                                   :negated-p negated-p 
                                   :inverse-p inverse-p 
                                   :replace-inds-with-vars-p 
                                   replace-inds-with-vars-p 
                                   args)
                        
                          (declare (ignore inds))
                        
                          rexpr))

                       ;;;
                       ;;;
                       ;;;
                    
                       ((or (top-query-p parser expr)
                            (bottom-query-p parser expr))
                          
                        (multiple-value-bind (rexpr inds)
                            (apply #'syntactically-rewrite-atomic-query expr parser 
                                   :negated-p negated-p 
                                   :inverse-p inverse-p 
                                   :replace-inds-with-vars-p 
                                   replace-inds-with-vars-p 
                                   args)

                          (if (and add-same-as-conjuncts-p inds)
                              (add-same-as-conjuncts rexpr inds negated-p)
                            rexpr)))

                       ;;;
                       ;;; AND/OR
                       ;;;

                       ((or (and (not negated-p) 
                                 (or-query-p parser expr))
                            (and negated-p
                                 (and-query-p parser expr)))

                        ;; OR
                           
                        (let* ((subexpressions 
                                (remove nil
                                        (mapcar #'(lambda (expr) 
                                                    (do-it expr inverse-p negated-p defs pvois))
                                                (get-subexpressions parser expr))))
                               (subexpressions 
                                (remove :false-query subexpressions)))

                          (if (not subexpressions)

                              :false-query

                            (if (member :true-query subexpressions)
                              
                                :true-query
                              
                              (let ((expr (simplify-boolean-expression
                                           `(:or ,@subexpressions))))
                              
                                (if (eq (first expr) :or)
                                    (normalize-disjunction expr)

                                  expr))))))
                          
                       ((or (and (not negated-p)
                                 (and-query-p parser expr))
                            (and negated-p
                                 (or-query-p parser expr)))
                        ;; AND
                           
                        (let* ((subexpressions 
                                (remove nil 
                                        (mapcar #'(lambda (expr) 
                                                    (do-it expr inverse-p negated-p defs pvois))
                                                (get-subexpressions parser expr))))
                               (subexpressions 
                                (remove :true-query subexpressions))

                               (same-as-subexpressions
                                (remove-if-not #'(lambda (x)  ; (same-as $?i-var i)
                                                   (and (same-as-query-p parser x)
                                                        (ind-p parser (third x))))
                                               subexpressions))

                               (subexpressions 
                                (remove-if #'(lambda (x) 
                                               (and (same-as-query-p parser x)
                                                    (ind-p parser (third x))))
                                           subexpressions)))

                          ;; hier wird sichergestellt, dass die Bindungen fuer die repraesentativen
                          ;; Variablen der Individuen bereits etabliert sind 

                          (if (not subexpressions)

                              :true-query

                            (if (member :false-query subexpressions)

                                :false-query
                            
                              (let ((expr (simplify-boolean-expression
                                           `(:and
                                             ,@same-as-subexpressions
                                             ,@subexpressions))))
                                expr)))))
                     
                       ;;;
                       ;;; Unary / Binary Atoms 
                       ;;; 

                       ((or (unary-query-p parser expr)
                            (binary-query-p parser expr))
                           
                        (multiple-value-bind (rexpr inds racer-rewritten-p)
                            (apply #'syntactically-rewrite-atomic-query expr parser 
                                   :negated-p negated-p 
                                   :inverse-p inverse-p 
                                   :replace-inds-with-vars-p 
                                   replace-inds-with-vars-p 
                                   args)

                          (if racer-rewritten-p 
                            
                              (do-it rexpr
                                     inverse-p negated-p
                                     defs
                                     pvois)

                            (if (and (binary-query-p parser expr)
                                     (is-equal-role-p (third expr)))
			      
                                ;;; keine same-as-conjuncts fuer nRQL-equal-role!
			      
                                rexpr
			    
                              (if (and add-same-as-conjuncts-p inds)
                                  (add-same-as-conjuncts rexpr inds negated-p)
                                (if (eq (first rexpr) :or)
                                    (normalize-disjunction rexpr)
                                  rexpr))))))

                       ;;;
                       ;;; unbekannt -> so lassen 
                       ;;;  
                           
                       (t (parser-error "Unknown expression ~A found" expr))))))

      (let* ((expr 
              (simplify-boolean-expression
               (do-it query)
               t))
             (res
              (if expanded-atoms
                  (tree-map #'(lambda (x) 
                                (if (symbolp x)
                                    (let ((y (assoc x expanded-atoms)))
                                      (if y
                                          (second y)
                                        x))
                                  x))
                            expr)
                expr)))

        res))))
      
;;;
;;;
;;;

(defmethod get-dnf-query ((query t))
  (parser-error 
   "Unrecognized expression ~A" query))

(defmethod get-dnf-query ((query list))
  (get-boolean-dnf query))

(defmethod get-dnf-query ((query (eql :true-query)))
  query)

(defmethod get-dnf-query ((query (eql :false-query)))
  query)

(defmethod get-dnf-query ((query query))
  (let ((query (parse-query 
                (get-boolean-dnf (unparse-query query))
                (copy (parser query)))))
    (dolist (sq (cons query (all-subqueries query)))
      (setf (slot-value sq 'in-dnf-p) t))
    query))

;;;
;;; 
;;;

(defmethod in-dnf-p ((query atomic-query))
  t)

(defmethod in-dnf-p ((query query-reference))
  t)

(defmethod in-dnf-p ((query and-query))
  (every #'(lambda (x) 
             (or (is-atomic-query-p x)
                 (is-false-query-p x)
                 (is-true-query-p x)
                 (is-query-reference-p x)))
         (subqueries query)))

(defmethod in-dnf-p ((query or-query))
  (and (every #'in-dnf-p (subqueries query))
       (every #'(lambda (x) 
                  (or (is-and-query-p x)
                      (is-false-query-p x)
                      (is-true-query-p x)
                      (is-atomic-query-p x)))
              (subqueries query))))

(defmethod in-dnf-p :around ((query atomic-query))
  (and (in-nnf-p query)
       (call-next-method)))


;;;
;;;
;;;

(defmethod in-nnf-p ((query atomic-query))
  t)

(defmethod in-nnf-p ((query complex-query))
  (and (not (negated-p query))
       (every #'in-nnf-p (subqueries query))))

;;;
;;;
;;;

;;; changed! 
;;; the original Variable name no longer shows up here, in order to 
;;; minimize the number of top-Conjuncts which are introdcued for
;;; non-shared variables. The following introduced top-Conjuncts:
;;; (define-rule (?x c) (and (?x d) (?x ?a has-part)))
;;; (define-rule (?x c) (and (?x d) (?x ?b has-part)))
;;; because ?a and ?b would be, after anonymization, still different
;;; individuals. Now, these two rules can be combined in a union query
;;; without introducing top-conjuncts, if the ano-Ind counter is reset
;;; before processing each disjunct: 
;;; (union (and (?x d) (?x ?ano-1 has-part)) (and (?x d) (?x ?ano-1 has-part)))

(defun get-ano-var (x)
  (if *keep-original-var-name-in-ano-vars-p*
      (intern (format nil "~A-~A~A" 
                      x 
                      (string-transform "ano")
                      (incf *ano-counter*)))
    (let ((x (if (stringp x) x (symbol-name x))))
      (if (starts-with-$-p x)
          (if (char= (elt x 2) #\*)
              (intern (format nil "$?*~A~A" 
                              (string-transform "ano")
                              (incf *ano-counter*)))
            (intern (format nil "$?~A~A" 
                            (string-transform "ano")
                            (incf *ano-counter*))))
        (if (char= (elt x 1) #\*)
            (intern (format nil "?*~A~A" 
                            (string-transform "ano")
                            (incf *ano-counter*)))
          (intern (format nil "?~A~A" 
                          (string-transform "ano")
                          (incf *ano-counter*))))))))


(defun substitute-vois-in (expr subs &key dont-anonymize-inds-p substitution-list-only-p parser)
  (let* ((parser (or parser (make-instance 'simple-parser)))
         (subs 
          (remove-duplicates 
           (append subs
                   (mapcar #'(lambda (x) 
                               (let ((from (first x))
                                     (to (second x)))
                                 (list (get-corresponding-voi parser from)
                                       (get-corresponding-voi parser to))))
                           subs))
           :test #'equal)))
    
    (labels ((subs (expr)             
               (let* ((expr1 (ensure-list expr))
                      (res
                       (mapcar #'(lambda (x)                                                
                                   (let ((found (assoc x subs)))
                                     (cond ((and found
                                                 ;;; NIL moeglich -> Variable wird auch
                                                 ;;; anonymisiert! 
						 (second found))
					    (second found))
					   ((and dont-anonymize-inds-p
						 (ind-p parser x))
					    x)
					   (substitution-list-only-p 
					    x)
					   (t
					    (let ((var 
						   (get-ano-var x)))
                                              ;;; natuerlich muessen die 
                                              ;;; konsistent umbenannt
                                              ;;; werden!!! 
                                              (push (list x var) subs)
                                              (push (list (get-corresponding-voi parser x)
                                                          (get-corresponding-voi parser var))
                                                    subs)
                                              var)))))
                               expr1)))
                 
                 (if (consp expr)
                     res
                   (first res))))
             
             (do-it (expr)
               ;;; ich muss hier die VOLLE SYNTAX zulassen, 
               ;;; also nicht nur NOT, sondern auch NEG etc.

               (when (consp expr)
                 (let ((op (first expr)))
		   
                   (cond
                    ((member op '(neg not inv :neg :not :inv :strict))
                     `(,op ,(do-it (second expr))))

                    ((eq op :tag)
                     `(,op ,(do-it (second expr)) ,(third expr)))

                    ((member op '(:substitute substitute))                   
                     `(,op (,(first (second expr))                            
                            ,@(subs (rest (second expr))))))
                  
                    ((member op '(bind-individual :bind-individual 
                                                  top :top
                                                  bottom :bottom))
                     `(,op ,(subs (second expr))))
                  
                    ((unary-query-p parser expr)
                     `(,(subs (first expr))
                       ,(second expr)))
                  
                    ((binary-query-p parser expr)
                     `(,(subs (first expr))
                       ,(subs (second expr))
                       ,(third expr)))

                    ((and-query-p parser expr)
                     `(,op
                       ,@(mapcar #'do-it
                                 (get-subexpressions parser expr))))
                    
                    ((or-query-p parser expr)
                     `(,op
                       ,@(mapcar #'(lambda (x) 
                                     ;; optimization: don't introduce unnecessary ano vois 
                                     ;; (let ((*ano-counter* 0))
                                     ;; this is wrong! uncomment...
                                     (do-it x))
                                 (get-subexpressions parser expr))))

                    ((projection-operator-p parser expr)
                     `(,op 
                       ,(subs (second expr))
                       ,(do-it (third expr))))
                    
                    ((minilisp-query-p parser expr)
                     (if (consp (first expr))
                         `(,(first expr)
                           ,@(mapcar #'subs (rest expr)))
                       (tree-map #'(lambda (x)
                                     (if (var-p parser x)
                                         (subs x)
                                       x))
                                 expr)))
                                                          
                    ((same-as-query-p parser expr)
                     `(,op
                       ,(subs (second expr))
                       ,(subs (third expr))))
                    
                    (t (nrql-error 
                        "Found unexpected expression ~A" expr)))))))

      (values (do-it expr) subs))))
              
                                          
