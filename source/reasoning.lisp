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

(defgeneric query-consistent-p (query &key &allow-other-keys)) ;; T, NIL, :DONT-KNOW

(defgeneric query-inconsistent-p (query &key &allow-other-keys))

(defgeneric query-tautological-p (query &key &allow-other-keys))

(defgeneric query-entails-p (query1 query2 &key &allow-other-keys))

;;;
;;;
;;;

(defun realize-abox-query-conjuncts (conjuncts &rest args)
  (let ((same-as-mapping 
         (compute-same-as-mapping conjuncts)))
    
    (when (eq same-as-mapping 'inconsistent) 
      (return-from realize-abox-query-conjuncts 'inconsistent))

    (multiple-value-bind (consistent-p new-conjuncts)
        (apply #'abox-query-conjuncts-consistent-p conjuncts
               :realize-p t 
               :same-as-mapping same-as-mapping 
               args)

      (if (not consistent-p)
          'inconsistent
        new-conjuncts))))

;;;
;;;
;;;

(defun adjust-package (sym)
  sym)

;;;
;;;
;;;

(defun abox-query-conjuncts-consistent-p (conjuncts &key 
                                                    query
                                                    realize-p 
                                                    same-as-mapping
                                                    (ignore-negated-conjuncts-p nil)
                                                    (tbox 'default)
                                                    rule-con-pattern
                                                    &allow-other-keys)

  ;;;
  ;;; Konstruktion einer ABox aus den Query-Konjunkten: 
  ;;; 
  ;;; (?X C)                         ->  ?x : C
  ;;; (?X (NOT C))                   ->  ?x : (NOT C)

  ;;;
  ;;; PER DEFAULT WERDEN NAF-ATOME IGNORIERT!
  ;;; ALSO (IGNORE-NEGATED-CONJUNCTS-P T)
  ;;; KONSITENZ-CHECK IST UNVOLLSTAENDIG!
  ;;; 
  ;;; ACHTUNG: 
  ;;; FOLGENDES IST FALSCH:
  ;;; (NOT (?X C))  ->  (?X (NOT C))
  ;;;
  ;;; GEGENBEISPIEL: (AND (NOT (?X C)) (NOT (?X (NOT C)))) IST KONSISTENT!
  ;;; WIRKLICH! ABER MIT DER KONSTRUKTION WIRD DARAUS DIE ABOX
  ;;; { ?X : C, ?X : (NOT c) } -> INKONSITENT, IST ABER FALSCH!
  ;;;
  ;;; ES GILT: 
  ;;; 
  ;;; (?X C) -> (NOT (?X (NOT C)))
  ;;; ABER NUR IN DIESER RICHTUNG!
  ;;; (NOT (?X (NOT C))) -> (?X C) IST FALSCH!!!!
  ;;; 
  ;;; ZUDEM NATUERLICH: 
  ;;; (?X (NOT C))) -> (NOT (?X C))
  ;;; Analog fuer die Role Axioms! 

  ;;; (?X ?Y R)                      ->  (?x,?y) : R
  ;;; (?X ?Y (NOT R))                ->  { ?x : (ALL R (NOT MARKER)), ?y : MARKER, (?x,?y) : R }

  ;;; 
  ;;; ACHTUNG: 
  ;;; NAF-ATOME WERDEN IGNORIERT! S.O. 
  ;;; FOLGENDES IST FALSCH: 
  ;;; (NOT (?X ?Y R))                ->  { ?x : (ALL R (NOT MARKER)), ?y : MARKER, (?x,?y) : R }
  ;;;
  
  ;;; (?X (:HAS-KNOWN-SUCCESSOR R))  ->  (?X ?TEMP R)
  ;;; (SAME-AS ?X ?Y)                ->  Replace ?X <- ?Y 
  ;;;

  (labels ((negate-cd-symbol (x) 
             (case x
               (< '>=)
               (> '<=)
               (<= '>)
               (>= '<)
               (= '<>)
               (<> '=)
               (equal 'unequal)
               (unequal 'equal)
               (string= 'string<>)
               (string<> 'string=)
               (otherwise x)))

           (get-bit-vector (n pos)
             (unless (zerop pos)
               (multiple-value-bind (rest bit)
                   (floor n 2)
                 (let ((sym 
                        (intern
                         (format nil "BIT-~A" pos))))
                   (if (zerop bit)
                       (cons sym (get-bit-vector rest (1- pos)))
                     (cons `(not ,sym) (get-bit-vector rest (1- pos))))))))

           (all-concept-assertions-for-voi (x) 
             (loop as entry in conjuncts when 
                   (and (is-instance-retrieval-query-p entry)
                        (eq (get-rep (voi entry)) x)
                        (not (negated-p entry)))
                   collect (dl-concept entry)))

           (get-rep (x) 
             (let ((found (assoc x same-as-mapping)))
               (if found
                   (second found)
                 x))))

    
    (with-critical-section
      (without-unique-name-assumption

        (let* ((tbox #+:midelora 
                     (if (prover::is-tbox-p tbox)
                         (change-package-of-description (prover::tbox-name tbox)
                                                        :racer-user)
                       tbox)
                     #-:midelora 
                     tbox)
		
               (old-abox (current-abox))
               (old-tbox (current-tbox))
		
               (consistent-p :dont-know)
               (naf-counter 0)
               (new-conjuncts nil)
		
               (orig-vois 
                (reduce #'append
                        (mapcar #'all-vois conjuncts)))
		
               (all-vois
                (remove-duplicates 
                 (mapcar #'get-rep orig-vois)))

               (una-vois 
                (remove-if-not #'una-voi-p all-vois))
		
               (non-una-vois 
                (set-difference all-vois una-vois)))

          (declare (ignorable una-vois non-una-vois))

          (unwind-protect 
              (progn
		 
                (init-abox +secret-abox+ tbox)
		 
                (let ((marker (make-hash-table :test #'equal))
                      (add-to-all-individuals nil))
		   

                  (dolist (voi all-vois)
                    (add-concept-assertion +secret-abox+ 
                                           (adjust-package 
                                            (textual-description voi))
                                           'top))
		   
                  (dolist (conjunct conjuncts)

                    (when (negated-p conjunct)
                      (incf naf-counter))

                    (when (or (not ignore-negated-conjuncts-p)
                              (not (negated-p conjunct)))
                      
                      (typecase conjunct

                        (same-as-query
                         ;; hier muss NICHTS GETAN WERDEN!
                         ;; behindert nur, dass ich SAME-AS jedesmals
                         ;; rausfiltern muss
                         )
			 
                        (query-reference
                         (to-be-implemented 'query-reference-reasoning))			 

                        (top-query
			  
                         (when (negated-p conjunct) 
                           (nrql-error "Bad negated conjunct ~A" conjunct)))                 
			 
                        (bottom-query
			  
                         (when (negated-p conjunct) 
                           (nrql-error "Bad negated conjunct ~A" conjunct))
			  
                         (add-concept-assertion +secret-abox+
                                                (adjust-package 
                                                 (textual-description (get-rep (voi conjunct))))
                                                'bottom))
			 
                        (instance-retrieval-query

                         (add-concept-assertion +secret-abox+
                                                (adjust-package 
                                                 (textual-description (get-rep (voi conjunct))))
                                                (adjust-package 
                                                 (if (negated-p conjunct) 
                                                     `(not ,(dl-concept conjunct))
                                                   (dl-concept conjunct)))))
                         
                        #+:dlmaps-full
                        ((or spatial-substrate::map-rcc-edge-query)
                          
                         ;;; hat stets nur ein Disjunkt! Basisrelation! 

                          
                         (when spatial-substrate::*add-rcc-atoms-to-abox-for-reasoning*
                            
                           (let ((rcc (cdr (original-description conjunct))))
                              
                             (when (cdr rcc) 
                               (nrql-runtime-error "RCC non-base relation for reasoning not allowed; 
set \"*unfold-rcc-disjunctions-p*\" to \"t\""))

                             (add-role-assertion
                               
                              +secret-abox+
                               
                              (adjust-package
                               (get-cor-voi (parser conjunct)
                                            (textual-description
                                             (get-rep
                                              (voi-from conjunct)))))

                              (adjust-package
                               (get-cor-voi (parser conjunct)
                                            (textual-description
                                             (get-rep
                                              (voi-to conjunct)))))
                               
                              (adjust-package (first rcc))))))
                         

                        ((or edge-retrieval-query 
                             has-known-successor-retrieval-query)

                         (let* ((role (dl-role conjunct))
                                (different-from-role-p (is-different-from-role-p (dl-role conjunct)))
                                (negated-p (negated-p conjunct))
                                (negated-p (if (negated-racer-role-p role)
                                               (not negated-p)
                                             negated-p)))


                           (let ((role (if (negated-racer-role-p role)
                                           (second role)
                                         role)))

                             (unless (is-equal-role-p role)
                               
                               (unless (gethash role marker)
                                 (setf (gethash role marker)
                                       #-:dlmaps 
                                       (create-tbox-internal-marker-concept tbox)
                                       #+(or :dlmaps  :midelora)
                                       (create-marker)))

                               (if negated-p
				  
                                   (let* ((marker (gethash role marker))
                                          (neg-marker `(racer-user::not ,marker))
                                          (concept `(all ,role ,marker)))
				    
                                     (unless marker (nrql-runtime-error "Marker concept not found"))

                                     (typecase conjunct
                                       (has-known-successor-retrieval-query
				       
                                        (add-concept-assertion 
                                         +secret-abox+  
                                         (adjust-package 
                                          (textual-description (get-rep (voi conjunct))))
                                         (adjust-package 
                                          concept))

                                        (push (list (textual-description (get-rep (voi conjunct))) neg-marker)
                                              add-to-all-individuals))
				      
                                       (edge-retrieval-query

                                        ;;; negated same-as or different-from kommen nicht vor!

                                        (add-concept-assertion 
                                         +secret-abox+  
                                         (adjust-package 
                                          (textual-description (get-rep (voi-from conjunct))))
                                         (adjust-package 
                                          concept))

                                        (add-concept-assertion 
                                         +secret-abox+  
                                         (adjust-package 
                                          (textual-description (get-rep (voi-to conjunct))))
                                         (adjust-package
                                          neg-marker)))))

                                 ;;; not negated
				
                                 (typecase conjunct
                                   
                                   (has-known-successor-retrieval-query 
                                    (add-role-assertion 
                                     +secret-abox+
                                     (adjust-package (textual-description (get-rep (voi conjunct))))
                                     (adjust-package (create-marker 'temp-ind))
                                     (adjust-package role)))

                                   
                                   (edge-retrieval-query 

                                    ;;; (?x ?y same-as/nrql-equal-role) wird bereits durch same-as mapping erschlagen

                                    (cond (different-from-role-p

                                           (add-different-from-assertion
                                            +secret-abox+
                                            (adjust-package (textual-description (get-rep (voi-from conjunct))))
                                            (adjust-package (textual-description (get-rep (voi-to conjunct))))))

                                          (t

                                           (add-role-assertion 
                                            +secret-abox+
                                            (adjust-package (textual-description (get-rep (voi-from conjunct))))
                                            (adjust-package (textual-description (get-rep (voi-to conjunct))))
                                            (adjust-package role)))))))))))
			 
                        (racer-cd-edge-retrieval-query 
                         (let* ((from-attrib (from-attribute conjunct))
                                (to-attrib (to-attribute conjunct))
                                (negated-p (negated-p conjunct))
                                (predicate (constraint conjunct))
                                (predicate (if negated-p 
                                               (if (listp predicate)
                                                   (tree-map #'negate-cd-symbol predicate)
                                                 (negate-cd-symbol predicate))
                                             predicate))

                                (from (textual-description (get-rep (voi-from conjunct))))
                                (to (textual-description (get-rep (voi-to conjunct))))

                                (from-cd-obj (intern (format nil "~A-~A" from from-attrib)))
                                (to-cd-obj (intern (format nil "~A-~A" to to-attrib)))

                                (constraint
                                 (if (consp predicate)
                                     (tree-map #'(lambda (x)
                                                   (cond ((eq x from) from-cd-obj)
                                                         ((eq x to) to-cd-obj)
                                                         (t x)))
                                               predicate)
                                   ;;; alte Syntax, einfaches Praedikatssymbol
                                   (list predicate from-cd-obj to-cd-obj))))

                           (add-attribute-assertion +secret-abox+ 
                                                    (adjust-package from)
                                                    (adjust-package from-cd-obj)
                                                    (adjust-package from-attrib))

                           (add-attribute-assertion +secret-abox+ 
                                                    (adjust-package to)
                                                    (adjust-package to-cd-obj)
                                                    (adjust-package to-attrib))

                           (add-constraint-assertion +secret-abox+                                                            
                                                     (adjust-package constraint)))))))

                  (dolist (ind-and-concept add-to-all-individuals)
                    (let ((ind (first ind-and-concept))
                          (concept (second ind-and-concept)))
                      (dolist (ind2 (all-individuals +secret-abox+))
                        (unless (same-abox-individual-p ind2 ind)
                          (add-concept-assertion +secret-abox+  
                                                 (adjust-package ind2)
                                                 (adjust-package concept))))))

                  (when rule-con-pattern
                    (evaluate-rule-consequence query rule-con-pattern
					       :same-as-mapping same-as-mapping))
		   
                  ;;; (princ (all-concept-assertions +secret-abox+))
                  ;;; (terpri)
                  ;;; (princ (all-role-assertions +secret-abox+))
		   
                  (setf consistent-p (abox-consistent-p +secret-abox+))

                  (setf consistent-p
                        (if consistent-p
                            ;;; es wird ja etwas staerkeres konstruiert... neg -> not
                            t
                          ;;; ansonsten ist das ergebnis richtig, wenn nur 1 NAF Atome
                          ;;; vorhanden war (tatsaechlich kann man das zu einem NAF-Atom
                          ;;; pro VOI verstaerken -> future work) 
                          (if (<= naf-counter 1)
                              nil
                            :dont-know)))
		   
                  #|
		   (terpri)
		   (terpri)
		   (pprint  (setf *x* (all-concept-assertions)))
		   (pprint (setf *y* (all-role-assertions)))
		   
		   (terpri) 
		   (princ consistent-p)
		   (break) |#
		   
		   
                  (setf new-conjuncts 
                        (when (and realize-p consistent-p)
                          (realize-abox +secret-abox+)

                          (let* ((msis
                                  (remove nil
                                          (mapcar #'(lambda (voi) 
                                                      (let ((msis
                                                             (mapcar #'first
                                                                     ;; eines aus jeder Aequivalenzklasse (das erste)
                                                                     ;; reicht ja!
                                                                     (most-specific-instantiators 
                                                                      (adjust-package 
                                                                       (textual-description (get-rep voi)))
                                                                      +secret-abox+)))
                                                            (acas (all-concept-assertions-for-voi voi)))

                                                        (unless (equal msis '(*top*))
                                                          (list voi 
                                                                (let* ((res 
                                                                        (remove-duplicates `(and ,@acas ,@msis)
                                                                                           :test #'equal))
                                                                       (res
                                                                        (or `(and ,@(atomic-concept-synonyms res tbox))
                                                                            res)))

                                                                  (if (cddr res)
                                                                      res
                                                                    (second res)))))))
                                                  all-vois))))
			 
                            msis)))

                  (values consistent-p
                          (if consistent-p 
                              new-conjuncts
                            (list (first orig-vois) 'bottom))
                          
                          (<= naf-counter 1))))
	     
	     
            (when (find-abox +secret-abox+)
              (forget-abox +secret-abox+))
	     
            (if (eq *package* (find-package :ts))
                (progn (set-current-tbox old-tbox) 
                  (set-current-abox old-abox))
              #+:dlmaps
              (progn
                (set-current-tbox (change-package-of-description 
                                   old-tbox
                                   :racer-user))
                (set-current-abox (change-package-of-description 
                                   old-abox
                                   :racer-user)))
              #-:dlmaps 
              (progn
                (set-current-tbox old-tbox)
                (set-current-abox old-abox)))))))))             


(defun simple-description-node-query-conjuncts-consistent-p (conjuncts &rest args 
                                                                       &key
                                                                       (tbox 'racer-user::default) 
                                                                       (package 'racer-user)
                                                                       &allow-other-keys)

  ;;;
  ;;; Hier werden simple-descriptions-queries (:and ...) (:or ...) von
  ;;; Knoten auf Konsistenz geprueft dazu wird Racer als SAT-Checker
  ;;; verwendet, MIT TBox!
  ;;;
  
  (declare (ignorable args))

  (let ((clusters nil))
    (loop while conjuncts do
          (let* ((qa (first conjuncts))
                 (cluster
                  (cons qa
                        (remove-if-not #'(lambda (qb)
                                           (equal (type-of qa) 
                                                  (type-of qb))
                                           (equal (vois qa)
                                                  (vois qb)))
                                       (rest conjuncts)))))
            (setf conjuncts
                  (set-difference conjuncts cluster))
            (push cluster clusters)))

    (every #'(lambda (cluster)
               (let ((descr
                      (change-package-of-description 
                       `(and ,@(mapcar #'(lambda (q)
                                           (if (negated-p q)
                                               (typecase q
                                                 #+:dlmaps (simple-conjunctive-description-query 
                                                  `(not (and ,@(ensure-list (textual-description q)))))
                                                 #+:dlmaps (simple-disjunctive-description-query 
                                                  `(not (or ,@(ensure-list (textual-description q))))))
                                             (typecase q
                                               #+:dlmaps (simple-conjunctive-description-query 
                                                `(and ,@(ensure-list (textual-description q))))
                                               #+:dlmaps (simple-disjunctive-description-query 
                                                `(or ,@(ensure-list (textual-description q)))))))
                                       cluster))
                       package)))

                 ;;; hier wird RACER nur als Sat-Checker fuer Boolsche Formeln verwendet

                 (racer:concept-satisfiable-p descr tbox)))

           clusters)))



(defun simple-description-edge-query-conjuncts-consistent-p (conjuncts &rest args 
                                                                       &key
                                                                       (package 'racer-user)
                                                                       &allow-other-keys)
  ;;;
  ;;; Hier werden simple-descriptions-queries (:and ...) (:or ...) von Kanten auf Konsistenz geprueft
  ;;; dazu wird Racer als SAT-Checker verwendet, aber *ohne* TBox!
  ;;; 

  (declare (ignorable args))

  (let ((clusters nil))
    (loop while conjuncts do
          (let* ((qa (first conjuncts))
                 (cluster
                  (cons qa
                        (remove-if-not #'(lambda (qb)
                                           (equal (type-of qa) 
                                                  (type-of qb))
                                           (equal (vois qa)
                                                  (vois qb)))
                                       (rest conjuncts)))))
            (setf conjuncts
                  (set-difference conjuncts cluster))
            (push cluster clusters)))

    (every #'(lambda (cluster)
               (let ((descr
                      (change-package-of-description 
                       `(and ,@(mapcar #'(lambda (q)
                                           (if (negated-p q)
                                               (typecase q
                                                 #+:dlmaps (simple-conjunctive-description-query 
                                                  `(not (and ,@(ensure-list (textual-description q)))))
                                                 #+:dlmaps (simple-disjunctive-description-query 
                                                  `(not (or ,@(ensure-list (textual-description q))))))
                                             (typecase q
                                               #+:dlmaps (simple-conjunctive-description-query 
                                                `(and ,@(ensure-list (textual-description q))))
                                               #+:dlmaps (simple-disjunctive-description-query 
                                                `(or ,@(ensure-list (textual-description q)))))))
                                       cluster))
                       package)))

                 ;;; hier wird RACER nur als Sat-Checker fuer Boolsche Formeln verwendet

                 (racer:concept-satisfiable-p descr nil)))

           clusters)))



(defun racer-description-node-query-conjuncts-consistent-p (conjuncts 
                                                            &rest args
                                                            &key (tbox 'racer-user::default) &allow-other-keys)

  ;;; Hier werden RACER-Descriptions auf Konsistenz geprueft
  ;;; (?X (:RACER (SOME R C))) (?X (:RACER (ALL R (NOT C)))) -> NIL !
  ;;; die Negationen (eigentlich Negation as Failure!) werden nach innen gezogen
  ;;; das ist eine zulaessige Inferenz (aber uvollstaedig!) (NOT (?X C)) -> (?X (NOT C)), 
  ;;; aber *NICHT* (?X (NOT C)) -> (NOT (?X C))! 

  (declare (ignorable args))

  (let ((clusters nil))

    (loop while conjuncts do
          (let* ((qa (first conjuncts))
                 (cluster
                  (cons qa
                        (remove-if-not #'(lambda (qb)
                                           (equal (type-of qa) 
                                                  (type-of qb))
                                           (equal (vois qa)
                                                  (vois qb)))
                                       (rest conjuncts)))))
            (setf conjuncts
                  (set-difference conjuncts cluster))
            (push cluster clusters)))

    ;;(setf *x* clusters) (break)

    (every #'(lambda (cluster)
               (let ((descr
                      `(and ,@(mapcar #'(lambda (q)
                                          (if (negated-p q)        
                                              `(not ,(dl-concept q))
                                            (dl-concept q)))                                      
                                      cluster))))
                 (racer:concept-satisfiable-p descr tbox)))
           clusters)))

;;;
;;;
;;;

(defmethod evaluate-rule-consequence ((query nrql-query) (consequence list) &key same-as-mapping)
  (with-slots (parser) query
    (labels ((process (var) 
	       
               (cond ((new-ind-op-p parser var)
                      (let* ((bindings (cddr var))
                             (key (cons (second var) bindings)))
                        (intern (format nil "~A~{-~A~}"
                                        (first key)
                                        (rest key)))))
                     (t 
		      (let ((found 
			     (or 
                              (second (assoc var same-as-mapping
					    :test #'(lambda (x y)
						      (eq x 
							  (textual-description y)))))
                              var)))
			
                        (if (symbolp found)
                            found
                            (textual-description found)))))))

      (let ((abox (mapcar #'(lambda (x)     
                              (let ((op (first x)))
                                (case op
                                  (racer:instance 
                                   (let ((var (second x))
                                         (concept (third x)))
                                     `(,op ,(process var) ,concept)))
                                  ((racer:related racer:constrained)
                                   (let ((from (second x))
                                         (to (third x))
                                         (role (fourth x)))

                                     (cond ((is-equal-role-p role)
                                            `(same-as ,(process from)
                                                      ,(process to)))
                                           ((is-different-from-role-p role)
                                            `(different-from ,(process from)
                                                             ,(process to)))
                                           (t
                                            `(,op
                                              ,(process from)
                                              ,(process to)
                                              ,role)))))
                                  (racer:different-from
                                   ;;; beides moeglich!
                                   (let ((from (second x))
                                         (to (third x)))
                                     `(,op
                                       ,(process from)
                                       ,(process to))))
                                  (racer:constraints 
                                    `(,op
                                      ,@(mapcar #'(lambda (constraint) 
                                                    (list (first constraint)
                                                          (process (second constraint))
                                                          (process (third constraint))))
                                                (rest x)))))))
                          consequence)))

        (dolist (assertion abox)
          ;;; (eval assertion)
          (ecase (first assertion)
            (instance 
             (apply #'add-concept-assertion +secret-abox+ 
		    (mapcar #'adjust-package (rest assertion))))
            (related 
             (apply #'add-role-assertion +secret-abox+ 
		    (mapcar #'adjust-package (rest assertion))))
            (different-from 
             (apply #'add-different-from-assertion +secret-abox+ 
		    (mapcar #'adjust-package (rest assertion))))
            (constrained
             (apply #'add-attribute-assertion +secret-abox+ 
		    (mapcar #'adjust-package (rest assertion))))
            (constraints
              (dolist (constraint (rest assertion))
                (add-constraint-assertion +secret-abox+ (adjust-package constraint))))))))))

;;;
;;;
;;;

(defmethod abox-conjuncts-consistent-p ((from-query and-query) (conjuncts list) &rest args &key &allow-other-keys)
  ;;; Instance / Edge-Retrieval-Queries werden hier geprueft
  (=> conjuncts
      (apply #'abox-query-conjuncts-consistent-p 
             conjuncts
             :tbox (tbox (substrate (parser from-query)))
             :query from-query
             args)))


#+:dlmaps
(defmethod substrate-conjuncts-consistent-p ((from-query and-query) (conjuncts list) &rest args &key &allow-other-keys)

  (and (let ((conjuncts 
              (remove-if-not #'is-substrate-simple-node-query-p conjuncts)))
         (=> conjuncts
             (apply #'simple-description-node-query-conjuncts-consistent-p  
                    conjuncts
                    args)))
       
       (let ((conjuncts 
              (remove-if-not #'is-substrate-simple-edge-query-p conjuncts)))
         (=> conjuncts
             (apply #'simple-description-edge-query-conjuncts-consistent-p  
                    conjuncts
                    args)))
       
       (let ((conjuncts (remove-if-not #'is-substrate-racer-node-query-p conjuncts)))
         (=> conjuncts  
             (apply #'racer-description-node-query-conjuncts-consistent-p  
                    conjuncts
                    args)))))

;;;
;;;
;;;       

(defmethod query-consistent-p ((query query) &key &allow-other-keys)
  (subclass-responsibility 'query-consistent-p))

(defmethod query-inconsistent-p ((query query) &rest args)
  (eq (apply #'query-consistent-p query args) nil))

;;;
;;;
;;;

(defmethod query-consistent-p :around ((query query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (with-slots (query-satisfiable) query
    (when (eq query-satisfiable :not-tested)
      (setf query-satisfiable (call-next-method)))

    query-satisfiable))

(defmethod query-consistent-p :around ((query nrql-query) &rest args &key rule-con-pattern &allow-other-keys)
  (with-slots (query-satisfiable) query
    (when (eq query-satisfiable :not-tested)
      (setf query-satisfiable 
            (apply #'call-next-method query 
                   :rule-con-pattern (or rule-con-pattern (rule-con-pattern query))
                   args)))

    query-satisfiable))

;;;
;;;
;;; 

(defmethod query-tautological-p :around ((query query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (with-slots (query-tautological) query
    (when (eq query-tautological :not-tested)
      (setf query-tautological (call-next-method)))

    query-tautological))

(defmethod query-tautological-p :around ((query nrql-query) &rest args &key rule-con-pattern &allow-other-keys)
  (with-slots (query-tautological) query
    (when (eq query-tautological :not-tested)
      (setf query-tautological 
	(apply #'call-next-method query 
	       :rule-con-pattern (or rule-con-pattern (rule-con-pattern query))
	       args))

      query-tautological)))

;;;
;;;
;;;

(defmethod query-tautological-p ((query query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (eq (query-consistent-p 
       (prep-query `(not ,(unparse-query query))
                   (substrate query)))
      nil))


;;;
;;;
;;;

(defmethod query-consistent-p :before ((query query) &key &allow-other-keys)
  (unless (in-dnf-p query)
    (nrql-error "Query consistency works only for queries in DNF")))

;;;
;;;
;;;

(defmethod query-consistent-p ((query same-as-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (let ((same-as-mapping 
	 (compute-same-as-mapping (list query))))

    (if (eq same-as-mapping 'inconsistent) 
	nil
      t)))

(defmethod query-consistent-p ((query nrql-same-as-query) &rest args &key &allow-other-keys)
  ;;; notwendig wegen rule consequences!
  (let ((same-as-mapping 
	 (compute-same-as-mapping (list query))))

    (if (eq same-as-mapping 'inconsistent) 
	nil
      (apply #'abox-query-conjuncts-consistent-p 
	     (list query) 
	     :tbox (tbox (substrate (parser query)))
	     :same-as-mapping same-as-mapping
	     :query query
	     args))))
  

#+:dlmaps
(defmethod query-consistent-p ((query atomic-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (consistent-p (if (negated-p query)
                    (get-negated-description query)
                  query)))

(defmethod query-consistent-p ((query dl-prover-query) &rest args &key &allow-other-keys)
  (apply #'abox-query-conjuncts-consistent-p 
         (list query) 
         :tbox (tbox (substrate (parser query)))
         :query query
         args))

(defmethod query-consistent-p ((query has-known-successor-retrieval-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

(defmethod query-consistent-p ((query binary-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

#+:dlmaps
(defmethod query-consistent-p ((query substrate-predicate-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :dont-know)

#+:dlmaps
(defmethod query-consistent-p ((query virtual-predicate-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  :dont-know)

(defmethod query-consistent-p ((query top-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

(defmethod query-consistent-p ((query bottom-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  nil)

(defmethod query-consistent-p ((query true-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

(defmethod query-consistent-p ((query false-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  nil)

;;;
;;; 
;;;

(defun prep-query (query substrate &rest args &key &allow-other-keys)
  (apply #'prepare-query-int query 
         nil
         substrate
         :bind-specials-p nil ; verhindert das ueberschreiben von last-query etc. 
         :optimize-p nil
         :generate-code-p nil
         :report-inconsistent-queries-p nil
         :report-tautological-queries-p nil
         :use-repository-p nil
         :rewrite-semantically-p nil
         :rewrite-to-dnf-p t 
         args))

;;;
;;; 
;;;

(defun compute-same-as-mapping (conjuncts) 
  
  (let* ((mapping nil)
	 (neg-mapping nil)
	 
	 (akku nil)
	 
	 (pos-conjuncts 
	  (remove-if-not #'(lambda (x) 
			     (and (not (negated-p x))
				  (or (is-same-as-query-p x)
                                      ;;; (?x ?y (not different-from))-Atome gibt es nicht 
                                      ;;; werden stets in (?x ?y same-as) umgeschrieben
				      (and (is-racer-edge-retrieval-query-p x)
					   (is-equal-role-p (dl-role x))))))
			 conjuncts))

	 (neg-conjuncts 
	  (remove-if-not #'(lambda (x) 
			     (or (and (negated-p x)
				      (or (is-same-as-query-p x)
					  (and (is-racer-edge-retrieval-query-p x)
					       (is-equal-role-p (dl-role x)))))
				 (and (not (negated-p x))
				      (and (is-racer-edge-retrieval-query-p x)

                                           ;; kommt nicht vor! 
                                           ;; (?x ?y (not same-as)) wurde zu (?x ?y different-from)
                                           
                                           ;; (consp (dl-role x))
                                           ;; (eq (first (dl-role x)) 'not)
					   ;; (is-equal-role-p (second (dl-role x)))

                                           (is-different-from-role-p (dl-role x))))))
			 conjuncts)))
    
    (labels ((insert-pos-edge (from to)
	       (pushnew (list from to) mapping :test #'equal))
	     
	     (insert-neg-edge (from to)
	       (pushnew (list from to) neg-mapping :test #'equal))
	     
	     (succs (node)
	       (mapcar #'second
		       (remove-if-not #'(lambda (x) 
					  (eq (first x) node))
				      mapping)))
	     (preds (node)
	       (mapcar #'first 
		       (remove-if-not #'(lambda (x) 
					  (eq (second x) node))
				      mapping)))
	     	     
	     (cluster-nodes (start)
	       (let ((succs (succs start))
		     (preds (preds start)))
		 
		 ;; (format t "~A ~A ~A~%" start succs preds)
		 
		 (dolist (succ succs)
		   (unless (member succ akku)
		     (push succ akku)
		     (cluster-nodes succ)))
		 
		 (dolist (pred preds)
		   (unless (member pred akku)
		     (push pred akku)
		     (cluster-nodes pred)))))
	     
	     (insert-neg (from to)
	       (insert-neg-edge from to)
	       (insert-neg-edge to from))

	     (insert-pos (from to)
	       (let ((from-succs (succs from))
		     (to-succs (succs to))
		     (from-preds (preds from))
		     (to-preds (preds to)))
		 
		 (insert-pos-edge from to)
		 
		 (dolist (pred from-preds)
		   (insert-pos-edge pred to))
		 (dolist (succ from-succs)
		   (insert-pos-edge succ to))
		 
		 (dolist (pred to-preds)
		   (insert-pos-edge from pred))
		 (dolist (succ to-succs)
		   (insert-pos-edge from succ)))))
      
      (dolist (x pos-conjuncts)
	(insert-pos (voi-from x) (voi-to x)))

      (dolist (x neg-conjuncts)
	(insert-neg (voi-from x) (voi-to x)))

      ;; (write mapping) (terpri)
      ;; (write neg-mapping) (terpri)
      
      (let ((same-as-mapping nil))
      
	(dolist (x mapping)
	  
	  (let ((from (first x)))
	    
	    (unless (find from same-as-mapping :test #'member)
	    
              (setf akku (list from))
	    
              (cluster-nodes from)
	  
              (let* ((cluster-nodes      
                      (remove-duplicates akku))
		
                     (bad-p (or (> (count-if #'una-voi-p cluster-nodes) 1)
                                (some #'(lambda (x) 
                                          (find (list from x) neg-mapping :test #'equal)
                                          (find (list x from) neg-mapping :test #'equal))
                                      cluster-nodes))))
	      
                ;; (princ cluster-nodes) (terpri)
	      
                (when bad-p 
                  (return-from compute-same-as-mapping 'inconsistent))
	      
                (dolist (cl cluster-nodes)
                  (push (list cl from) same-as-mapping))))))

        same-as-mapping))))

;;;
;;; Primary Method!
;;;


(defmethod query-consistent-p ((query and-query) &rest args &key
                               #+:dlmaps (check-conjuncts-also-individually-p nil)
                               &allow-other-keys)
  
  (let ((conjuncts (subqueries query)))

    (when (some #'is-bottom-query-p (subqueries query))
      (return-from query-consistent-p nil))
    
    #+:dlmaps
    (when check-conjuncts-also-individually-p 
      (when (some #'(lambda (query) 
                      (apply #'query-inconsistent-p query args))
                  conjuncts)
        (return-from query-consistent-p nil)))

    (let ((same-as-mapping 
           (compute-same-as-mapping conjuncts)))

      (when (eq same-as-mapping 'inconsistent) 
        (return-from query-consistent-p nil))

      (let* ((abox        
              (apply #'abox-conjuncts-consistent-p 
                     query
		     (remove-if-not #'is-dl-prover-query-p conjuncts)
                     ;;;(remove-if-not #'is-racer-abox-query-p conjuncts)
                     
                     :same-as-mapping same-as-mapping
                     args))

             (substrate 
              #+:dlmaps
              (apply #'substrate-conjuncts-consistent-p 
                     query
                     (remove-if-not #'is-substrate-query-p conjuncts)
                     :same-as-mapping same-as-mapping
                     args)
              #-:dlmaps
              t)
             
             #-:dlmaps
             (data
              (apply #'data-conjuncts-consistent-p 
                     query
                     (remove-if-not #'is-data-substrate-query-p conjuncts)
                     :same-as-mapping same-as-mapping
                     args)))
	
        (if (or (eq abox nil)
                (eq substrate nil)
                #-:dlmaps (eq data nil)
                #+:dlmaps nil)
            nil
          (if (or (eq abox :dont-know)
                  (eq substrate :dont-know)
                  #-:dlmaps (eq data :dont-know)
                  #+:dlmaps nil)
              :dont-know        
            t))))))


(defmethod query-consistent-p ((query or-query) &rest args &key &allow-other-keys)
  (let ((res nil))
    (or (some #'(lambda (query) 
                  (let ((subres (apply #'query-consistent-p query args)))
                    (when (eq subres :dont-known)
                      (setf res :dont-know))
                    (eq subres t)))
              (subqueries query))
        res)))

;;;
;;;
;;;

(defmethod query-entails-p :around ((query1 query) (query2 query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (when (not (and (in-dnf-p query1)
                  (in-dnf-p query2)))
    
    (nrql-error "Query entailment works only for queries in DNF"))

  (call-next-method))

;;;
;;;
;;;

(defmethod query-entails-p ((query1 query) (query2 query) &rest args &key &allow-other-keys)
  (apply #'query-entails-query-p query1 query2 args))

;;;
;;; f. Klassifikation im DAG benoetigt
;;;

(defmethod query-entails-p ((query1 query) (query2 master-top-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

(defmethod query-entails-p ((query1 master-top-query) (query2 query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  nil)

(defmethod query-entails-p ((query1 master-top-query) (query2 master-top-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

;;;
;;;
;;;

(defmethod query-entails-p ((query1 master-bottom-query) (query2 query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

(defmethod query-entails-p ((query1 query) (query2 master-bottom-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  nil)

(defmethod query-entails-p ((query1 master-bottom-query) (query2 master-bottom-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  t)

;;;
;;;
;;;


(defmethod query-equivalent-p ((query1 query) (query2 query) &rest args &key &allow-other-keys)
  (and (apply #'query-entails-p query1 query2 args)
       (apply #'query-entails-p query2 query1 args)))


;;;
;;; Reduktion auf Unerfuellbarkeit
;;;

(defmethod query-entails-query-p ((query1 query) (query2 query) &rest args
                                  &key enforce-same-arity-p &allow-other-keys)
  (declare (ignorable args))

  (let ((sq1 (cons query1 (all-subqueries query1)))
        (sq2 (cons query2 (all-subqueries query2))))

    (if (or (some #'negated-p sq1)
            (some #'is-query-reference-p sq1)
            (some #'negated-p sq2)
            (some #'is-query-reference-p sq2))

        :dont-know

      (when (and (=> enforce-same-arity-p 
                     (variable-vectors-compatible-p query2 query1 :superset))
                 
                 (and (eq (substrate query1)
                          (substrate query2))))
        
        (let* ((substrate (substrate query1))

               (substitution-list 
                (mapcar #'(lambda (sub)
                            (mapcar #'textual-description sub))
                        (remove-if #'(lambda (sub)
                                       (member nil sub))
                                   
                                   (apply #'append
                                          (mapcar #'(lambda (x y)
                                                      (let ((svoi1 (first x))
                                                            (avoi1 (second x))
                                                            (svoi2 (first y))
                                                            (avoi2 (second y)))
                                                        (list (list svoi1 svoi2)
                                                              (list avoi1 avoi2))))
                                                  
                                                  (all-paired-vois query2)
                                                  (all-paired-vois query1)
                                                  )))))

               (query1 (unparse-query query1))
               (query2 (unparse-query query2))
               
               (nquery2 (substitute-vois-in query2 substitution-list)))
          
          (when *debug-p* 
            (format t "~%---------------------------------------------------------------------------------------")
            (format t "~%ENTAILS ~A ~A?~%" query1 query2)
            (terpri) 
            (princ "Substitution list: ")
            (princ substitution-list)
            (terpri)
            (princ "Query1:")
            (pprint query1)
            (terpri)(terpri)
            (princ "Query2:")
            (pprint query2)
            (terpri)(terpri)
            (princ "nQuery2:")
            (pprint nquery2)
            (terpri)(terpri)
            (princ "(AND Query1 (NOT Query2)):")
            (pprint `(and ,query1 (not ,query2)))
            (terpri) (terpri))
          
          (let* ((query (prep-query
                         `(and ,query1 (not ,nquery2))
                         substrate
                         :add-same-as-conjuncts-p nil))
                 (res (not (query-consistent-p query :ignore-negated-conjuncts-p nil))))
	    
            (when *debug-p*
              (princ "REWRITTEN: ") 
              (terpri) (terpri)

              (pprint (unparse-query query))

              (format t "~%REDUCTION TO INCONSISTENCY CHECK - RESULT: ~A~%" res))

            res))))))

