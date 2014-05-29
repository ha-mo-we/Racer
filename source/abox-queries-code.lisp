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
;;; Code fuer DL-Prover (Racer/MiDeLoRa) - Queries
;;; 

(defvar *debugging-p* nil) 

;;;
;;; Individual Instance Checking
;;; 


(defmethod get-code-dl-prover-check-individual-instance-p ((substrate dl-prover-substrate)
                                                           ind dl-concept body &rest args  
                                                           &key negated-p 
                                                           &allow-other-keys)
  (declare (ignorable args))

  (if (not negated-p)
      `(let ((concept (quote ,dl-concept)))
         
         (declare (ignorable concept))
         
         (when (dl-prover-individual-instance-p *running-substrate* ,ind concept)

           ,body))

    `(unless (dl-prover-individual-instance-p *running-substrate* ,ind ',dl-concept)
         
       ,body)))
    

(defmethod evaluate-dl-prover-check-individual-instance-p ((substrate dl-prover-substrate)
                                                           ind concept continuation &rest args  
                                                           &key negated-p &allow-other-keys)
  (declare (ignorable args))

  (if (not negated-p)
      (when (dl-prover-individual-instance-p *running-substrate* ind concept)
        
        (apply continuation :var ind args))
    
    (unless (dl-prover-individual-instance-p *running-substrate* ind concept)

      (apply continuation :var ind args))))

;;;
;;; Individuals Related Checking
;;; 


(defmethod get-code-dl-prover-check-individuals-related-p ((substrate dl-prover-substrate)
                                                           from to role body &rest args)
  `(let ((from ,from)
         (to ,to)
         (role (quote ,role)))
     
     (declare (ignorable from to role))
                          
     ,(apply #'get-code-dl-prover-retrieve-individual-fillers 
             substrate
             from role 
             `(when (same-abox-individual-p cand-to to) 
                ,body)
             :to 'cand-to
             args)))

(defmethod evaluate-dl-prover-check-individuals-related-p ((substrate dl-prover-substrate)
                                                           from to role continuation &rest args)
  (apply #'evaluate-dl-prover-retrieve-individual-fillers 
         substrate
         from role 
         continuation
         :bound-to to  
         args))

;;; alte, langsame Implementation nur fuer TBox-Queries wg. fehlendem internal-individuals-related-p   
(defmethod evaluate-dl-prover-check-individuals-related-p ((substrate tbox-mirror-substrate)
                                                           from to role continuation &rest args)
  (let ((to2 to))    
    (apply #'evaluate-dl-prover-retrieve-individual-fillers 
           substrate
           from role 
           #'(lambda (&rest args &key to &allow-other-keys)
               (declare (ignorable args))
               (when (eq to2 to)
                 (apply continuation args)))
           args)))


;;;
;;; Concept Instances Enumeration
;;; 

(defmethod get-code-dl-prover-retrieve-concept-instances ((substrate dl-prover-substrate)
                                                          dl-concept body 
                                                          &rest args 
                                                          &key negated-p var
                                                          query
                                                          &allow-other-keys)
  (declare (ignorable args))
  
  (unless var (nrql-error "Compiler error: no var given"))

  (if (not negated-p)
      `(let ((known (dl-prover-retrieve-known-concept-instances *running-substrate* (quote ,dl-concept))))
        
         (abortable-dolist (,var known)
           (progn 
             (setf (bindings-found-p ,query) nil)
             ,body
             (when ,(when (and (is-unary-query-p query)
                               (member (voi query) (existential-vois query)))
                      t)
               (when (bindings-found-p ,query)
                 (throw 'abort-enumerator t)))))

         (if *continuation-based-instance-retrieval-p*
        
             (dl-prover-retrieve-concept-instances *running-substrate* (quote ,dl-concept)
                                                   :continuation
           
                                                   #'(lambda (,var)
                                                       
                                                       (setf (bindings-found-p ,query) nil)
                                                       ,body
                                                       (when ,(when (and (is-unary-query-p query)
                                                                         (member (voi query) (existential-vois query)))
                                                                t)
                                                         (when (bindings-found-p ,query)
                                                           (throw 'abort-enumerator t)))))

           (abortable-dolist (,var 
                              (dl-prover-retrieve-concept-instances *running-substrate* (quote ,dl-concept)))
             (progn 
               (setf (bindings-found-p ,query) nil)
               ,body
               (when ,(when (and (is-unary-query-p query)
                                 (member (voi query) (existential-vois query)))
                        t)
                 (when (bindings-found-p ,query)
                   (throw 'abort-enumerator t)))))))
      
    `(abortable-dolist (,var 
                        (dl-prover-all-individuals *running-substrate*))

       (unless (dl-prover-individual-instance-p *running-substrate* ,var (quote ,dl-concept))
         (setf (bindings-found-p ,query) nil)
         ,body
         (when ,(when (and (is-unary-query-p query) 
                           (member (voi query) (existential-vois query)))
                  t)
           (when (bindings-found-p ,query)
             (throw 'abort-enumerator t)))))))


(defmethod evaluate-dl-prover-retrieve-concept-instances ((substrate dl-prover-substrate)
                                                          dl-concept continuation 
                                                          &rest args 
                                                          &key negated-p query
                                                          &allow-other-keys)

  (labels ((pos-body (var)
             (apply continuation :var var args)))
  
    (if (not negated-p)

        (let ((known (dl-prover-retrieve-known-concept-instances *running-substrate* dl-concept)))
            
          (abortable-dolist (var known)
            (setf (bindings-found-p query) nil)
            (pos-body var)
            (when (and (is-unary-query-p query) 
                       (bindings-found-p query)
                       (member (voi query) (existential-vois query)))
              (throw 'abort-enumerator t)))
            
          (if *continuation-based-instance-retrieval-p*

              (dl-prover-retrieve-concept-instances *running-substrate* dl-concept
                                                    :continuation
                                                    #'(lambda (var)
                                                        (setf (bindings-found-p query) nil)
                                                        (pos-body var)
                                                        (when (and (is-unary-query-p query) 
                                                                   (bindings-found-p query)
                                                                   (member (voi query) (existential-vois query)))
                                                          (throw 'abort-enumerator t))))

            (abortable-dolist (var (dl-prover-retrieve-concept-instances *running-substrate* dl-concept))
              (setf (bindings-found-p query) nil)
              (pos-body var)
              (when (and (is-unary-query-p query) 
                         (bindings-found-p query)
                         (member (voi query) (existential-vois query)))
                (throw 'abort-enumerator t)))))

      
      (abortable-dolist (var (dl-prover-all-individuals *running-substrate*))
        (setf (bindings-found-p query) nil)
        (unless (dl-prover-individual-instance-p *running-substrate* var dl-concept)
          (apply continuation :var var args)
          (when (and (is-unary-query-p query) 
                     (bindings-found-p query)
                     (member (voi query) (existential-vois query)))
            (throw 'abort-enumerator t)))))))

;;;
;;; Related Individuals Enumeration (Komplex! Optimiert Dl-Provers Routinen!) 
;;; Liefert Paare! (From To) 
;;; 

(defmethod get-code-dl-prover-retrieve-related-individuals ((substrate dl-prover-substrate)
                                                            role body &rest args &key from to query &allow-other-keys)
  (unless (and from to)
    (nrql-error "Compiler error: no vars given"))
  
  `(abortable-dolist (,from (dl-prover-all-individuals *running-substrate*))
     (progn 
       (setf (bindings-found-p ,query) nil)
       ,(apply #'get-code-dl-prover-retrieve-individual-fillers 
               substrate from role 
               body
               :to to 
               args)
       (when ,(when (and (member (voi-from query) (existential-vois query))
                         (member (voi-to query) (existential-vois query)))
                t)
         (when (bindings-found-p ,query)
           (throw 'abort-enumerator t))))))
         

(defmethod evaluate-dl-prover-retrieve-related-individuals ((substrate dl-prover-substrate)
                                                            role continuation &rest args &key query &allow-other-keys)
  (declare (ignorable args))

  (abortable-dolist (from (dl-prover-all-individuals *running-substrate*))
    (setf (bindings-found-p query) nil)
    (apply #'evaluate-dl-prover-retrieve-individual-fillers 
           substrate
           from role 
           #'(lambda (&rest args &key to &allow-other-keys) 
               (apply continuation :from from :to to args))
           args)
    (when (and (member (voi-to query) (existential-vois query))
               (member (voi-from query) (existential-vois query))
               ; es muessen tatsaechlich BEIDE rein existentiell sein! 
               (bindings-found-p query))
      (throw 'abort-enumerator t))))

;;;
;;; Individual Role Successor Enumeration 
;;; Hinweis: Erlaeuterungen zu schwer verstaendlichen Code-Zeilen in der entsprechenden 
;;; runtime-Methode lesen
;;; 

(defmethod get-code-dl-prover-retrieve-individual-fillers ((substrate dl-prover-substrate)
                                                           from role body
                                                           &rest args 
                                                           &key  
                                                           query
                                                           only-one-p 
                                                           negated-p
                                                           ;;; bei (NOT (?*X ?*Y R))
                                                           ;;; AChtung: das ist nicht das Gleiche
                                                           ;;; wie (?*x ?*y (NOT R)), s. unten!

                                                           inverse-p to &allow-other-keys)

  (declare (ignorable args))

  (error "get-code-dl-prover-retrieve-individual-fillers needs update (see runtime for recent version)")

  (unless to (nrql-error "Compiler error: no var given"))

  (let ((only-one-p 
         (or only-one-p
             (and query 
                  (member (voi-to query) 
                          (existential-vois query))))))

    (if (negated-dl-role-p role)

        ;;;
        ;;; (?*x ?*y (NOT R)), (NOT (?*x ?*y (NOT R)))
        ;;; 
    
        (if negated-p

            ;;;
            ;;; (NOT (?*x ?*y (NOT R))) ?
            ;;; 

            `(let* ((inds (dl-prover-all-individuals *running-substrate*))
                    (negated-succs (copy-list inds)))

               (declare (ignorable inds negated-succs))
                 
               (abortable-dolist (to inds)
                 ,(apply #'get-code-dl-prover-check-individuals-related-p 
                         substrate
                         from 'to role
                         `(setf negated-succs (delete to negated-succs))
                         :negated-p nil
                         :inverse-p nil 
                         :query query
                         nil))

               (abortable-dolist (,to negated-succs)
                 ,body))
      

          (let ((equal-role-p (is-equal-role-p (second role)))
                (different-from-role-p (is-different-from-role-p (second role))))
            
            `(let* ((role ',(if (or equal-role-p
                                    different-from-role-p)
                                nil 
                              (second role)))
                    (role ,(if (or equal-role-p 
                                   different-from-role-p)
                               nil
                             (if inverse-p 
				 `(dl-prover-atomic-role-inverse *running-substrate* role)
			       `role))))
           
               (declare (ignorable role)) 
             
               (dolist (,to (dl-prover-all-individuals *running-substrate*))
           
                 (let ((abox-consistent-p nil)
                       (added-p nil))

                   (with-critical-section

                     ,@(cond (equal-role-p 

                              (nrql-runtime-error 'rewriting-error-found-negated-equal-role))

                             (different-from-role-p 

                              (nrql-runtime-error 'rewriting-error-found-negated-different-from-role))

                             (t
               
                              `((unless (loop as role-axiom in 
                                              (dl-prover-all-role-assertions-for-individual-in-domain 
                                               *running-substrate* ,from)
                                              as role1 = (second role-axiom)
                                              as to1 = (second (first role-axiom))
                                              thereis (and (same-abox-individual-p to1 ,to)
                                                           (same-role-p role1 role)))
                                  (setf added-p t)
                                  (dl-add-role-assertion *running-substrate* *running-abox* ,from ,to role))
                            
                                (setf abox-consistent-p
                                      (if (is-racer-substrate-p *running-substrate*)
                                          (without-unique-name-assumption
                                            (dl-prover-abox-consistent-p *running-substrate*
                                                                         *running-abox*))
                                    
                                        ;;; noch inkorrekt fuer Midelora! 
                                        (dl-prover-abox-consistent-p *running-substrate*
                                                                     *running-abox*)))
                            
                                (when added-p 
                                  (dl-forget-role-assertion *running-substrate* *running-abox* ,from ,to role))))))

                   ;;; da der Effekt ja rueckgaengig gemacht wird, 
                   ;;; muessen die Caches nicht updated werden!
             
                   (unless abox-consistent-p 
               
                     ,body))))))
       
      ;;;
      ;;; (?*x ?*y R), (NOT (?*x ?*y R))
      ;;; 

      (let ((equal-role-p (is-equal-role-p role))
            (different-from-role-p (is-different-from-role-p role)))
    
        `(let* ((role ',(if (or equal-role-p
                                different-from-role-p)
                            nil 
                          role))
                (role ,(if (or equal-role-p 
                               different-from-role-p)
                           nil
                         (if inverse-p 
			     `(dl-prover-atomic-role-inverse *running-substrate* role)
			   `role)))
                (inv-role ,(if (or equal-role-p 
                                   different-from-role-p)
                               nil
                             `(dl-prover-atomic-role-inverse *running-substrate* role))))

           (declare (ignorable role inv-role))
      
           (let* ((from ,from))

             (declare (ignorable from to role inv-role))

             ,(if negated-p
              
                  ;;;
                  ;;; (NOT (?*x ?*y R))
                  ;;; 
    
                  `(let* ((inds (dl-prover-all-individuals *running-substrate*))
                          (negated-succs (copy-list inds)))

                     (declare (ignorable inds negated-succs))
                 
                     (abortable-dolist (to inds)
                       ,(apply #'get-code-dl-prover-check-individuals-related-p 
                               substrate
                               from 'to role 
                               `(setf negated-succs (delete to negated-succs))
                               :negated-p nil
                               :inverse-p nil 
                               :query query
                               nil))

                     (abortable-dolist (,to negated-succs)
                       ,body))

                ;;;
                ;;; (?*x ?*y R)
                ;;; 
              
            
                `(let* ((role-descendants1 
                         ,(unless (or equal-role-p different-from-role-p)
                            `(dl-prover-atomic-role-descendants *running-substrate* role)))
                        
                        (role-transitive-or-transitive-subrole-p 
                         (some #'(lambda (r) 
                                   (dl-prover-transitive-role-p *running-substrate* r))
                               role-descendants1))

                        (done nil)

                        (succs `(dl-prover-all-role-successors *running-substrate* from role))

                        (orig-from nil)
		 
                        (last-explanation nil))
               
                   (declare (ignorable role-descendants1 succs))
                             
                   (labels ((role-transitive-or-transitive-superrole-p (role) 
                              (some #'(lambda (r) 
                                        (and (member r role-descendants1 
                                                     :test #'equal)
                                             (dl-prover-transitive-role-p *running-substrate* r)))
                                    (dl-prover-atomic-role-ancestors *running-substrate* role)))

                            (do-it (current &key role-descendants continue-p first-p (syntactic-inference-p t) generated-succs)

                              (setf last-explanation nil)
                              
                              ;;;
                              ;;;
                              ;;;
                              
                              (unless first-p 
                                
                                (push current generated-succs)
                                
                                (when *debugging-p* 
                                  (format t "~%~%Found successor ~A!~%" current)
                                  (format t "All successors (only if transitive): ~A~%" generated-succs))           
                                
                                (with-critical-section
                                  (unless (dl-prover-role-successors-cache-complete-for-role-p *running-substrate* from role)
                                    (unless (dl-prover-is-known-role-successor-of-p *running-substrate* current from role)
                                      (dl-prover-register-role-successor *running-substrate* current from role))))                              
                                
                                ,@(when only-one-p 
                                    `((when *debugging-p* 
                                        (format t "Done!~%" current))
                                      (setf done t)))
                                
                                ,(if inverse-p
                                     `(let ((,from current))
                                        (declare (ignorable ,to))
                                        (when *debugging-p* 
                                          (format t "Now continuation, :from ~A ~A"  current args))
                                        ,body)
                                   `(let ((,to current))
                                      (declare (ignorable ,from))
                                      (when *debugging-p* 
                                        (format t "Now continuation, :to ~A ~A"  current args))
                                      ,body)))

                              ;;;
                              ;;;
                              ;;; 

                              (when (and (or continue-p first-p)
                                         (not done))

                                ,@(cond (equal-role-p

                                         `((dolist (to1 (let ((*use-individual-synonyms-p* t))
                                                          (dl-prover-individual-synonyms *running-substrate* current)))

                                             (do-it to1 
                                                    :continue-p nil
                                                    :generated-succs generated-succs))))

                                        (different-from-role-p

                                         `((dolist (to1 (dl-prover-individual-antonyms *running-substrate* current))

                                             (do-it to1 
                                                    :continue-p nil
                                                    :generated-succs generated-succs))))
                                        

                                        (t
                                       
                                         `( (when *debugging-p* 
                                              (format t "Now: ~A, role assertions for ~A in domain ~%" current current))
                                
                                            (loop as role-axiom in 
                                                  (dl-prover-all-role-assertions-for-individual-in-domain *running-substrate* current)
                                                  as role1 = (second role-axiom)
                                                  as to1a = (second (first role-axiom))
                                                  as continue-p = nil 

                                                  do 

                                                  (let ((syntactic-inference1-p nil))
                                                    
                                                    (loop as to1 in (dl-prover-individual-synonyms1 *running-substrate* to1a)
                                                          
                                                          when 

                                                          (with-critical-section
                                                                 
                                                            (when *debugging-p* 
                                                              (format t "~%Domain assertion: ~A~%" role-axiom)
                                                              (format t "~A is in ~A: ~A~%" 
                                                                      role1 role-descendants
                                                                      (when (member role1 role-descendants :test #'equal) t)))
                                                     
                                                            (and (not (member to1 generated-succs))
                                                                      
                                                                 (or 

                                                                  (and syntactic-inference1-p 

                                                                       (member role1 role-descendants :test #'equal)

                                                                       (progn 

                                                                         (setf continue-p 
                                                                               (transitive-or-has-transitive-super-role-p role1))

                                                                         (when (and *debugging-p* continue-p)
                                                                           (format t "Continue~%"))
                                                                         
                                                                         (when *record-explanations-p*
                                                                           (unless (eq to1 to1a)
                                                                             (push 
                                                                              `((,to1 ,to1a) :same-as)
                                                                              last-explanation))
                                                                              
                                                                           (push 
                                                                            `((,current ,to1a ,role1) ,role1 :is-subrole-of ,role) 
                                                                            last-explanation))
                                                                            
                                                                         t))
                                                           
                                                                  (and (not *told-information-reasoning-p*)
                                                                          
                                                                       (or (needs-filler-reasoning-p *running-substrate*)
                                                                           ;;; role-transitive-or-transitive-subrole-p
                                                                           )
                                                                          
                                                                       (if (symbolp role) 
                                                                           (dl-prover-internal-individuals-related-p *running-substrate*
                                                                                                                     orig-from
                                                                                                                     to1
                                                                                                                     role
                                                                                                                     (abox *running-substrate*)
                                                                                                                     ;nil check-p argument
                                                                                                                     )
                                                                         (dl-prover-internal-individuals-related-p *running-substrate*
                                                                                                                   to1 
                                                                                                                   orig-from
                                                                                                                   inv-role
                                                                                                                   (abox *running-substrate*)
                                                                                                                   ;nil
                                                                                                                   ))

                                                                       (progn 
								  
                                                                         (setf continue-p role-transitive-or-transitive-subrole-p)
                                                                             
                                                                         (setf syntactic-inference1-p nil)
                                                                             
                                                                         (when *record-explanations-p*

                                                                           (unless (eq to1 to1a)
                                                                             (push 
                                                                              `((,to1 ,to1a) :same-as)
                                                                              last-explanation))

                                                                           (push `((,current ,to1a ,role1) due-to-abox-reasoning)
                                                                                 last-explanation))

                                                                         t)))))
                                                          
                                                          do 
                                            
                                                          (progn 
                                              
                                                            (if *record-explanations-p*
                                                                (let ((*explanations*
                                                                       (cons last-explanation *explanations*)))
                                                                  (do-it to1
                                                                         :generated-succs generated-succs
                                                                         :continue-p 
                                                                         continue-p
                                                                         :syntactic-inference-p
                                                                         syntactic-inference1-p
                                                                         :role-descendants
                                                                         (if (and continue-p syntactic-inference1-p)
                                                                             (progn
                                                                               (when *debugging-p*
                                                                                 (format t "Changing focus role to ~A~%" role1))
                                                                               (dl-prover-atomic-role-descendants *running-substrate* role1))
                                                                           role-descendants)))

                                                              (do-it to1
                                                                     :generated-succs generated-succs
                                                                     :continue-p
                                                                     continue-p
                                                                     :syntactic-inference-p
                                                                     syntactic-inference1-p
                                                                     :role-descendants
                                                                     (if (and continue-p syntactic-inference1-p)
                                                                         (progn
                                                                           (when *debugging-p*
                                                                             (format t "Changing focus role to ~A~%" role1))
                                                                           (dl-prover-atomic-role-descendants *running-substrate* role1))
                                                                       role-descendants)))))))

                                            ;;;
                                            ;;;
                                            ;;;

                                            (setf last-explanation nil)
                                
                                            (when *debugging-p*
                                              (format t "Now: ~A, role assertions for ~A in range ~%" current current))
                                
                                            (loop as role-axiom in 
                                                  (dl-prover-all-role-assertions-for-individual-in-range *running-substrate* current)
                                                  as role1 = (second role-axiom) 
                                                  as inv-role1 = (dl-prover-atomic-role-inverse *running-substrate* role1)
                                                  as from1a = (first (first role-axiom))

                                                  as continue-p = nil

                                                  do 

                                                  (let ((syntactic-inference1-p syntactic-inference-p))

                                                    (loop as from1 in (dl-prover-individual-synonyms1 *running-substrate* from1a)

                                                          when 

                                                          (with-critical-section

                                                            (when *debugging-p*
                                                              (format t "~%Range assertion: ~A~%" role-axioms)
                                                              (format t "~A is in ~A: ~A~%" 
                                                                      inv-role1 role-descendants
                                                                      (when (member inv-role1 role-descendants :test #'equal) t)))

                                                            (and (not (member from1 generated-succs))

                                                                 (or 

                                                                  (and syntactic-inference-p 
                                                                       
                                                                       (member inv-role1 role-descendants :test #'equal)
                                                             
                                                                       (progn 

                                                                         (setf continue-p 
                                                                               (transitive-or-has-transitive-super-role-p inv-role1))
                                                             
                                                                         (when (and *debugging-p* continue-p)
                                                                           (format t "Continue~%"))
                                                                
                                                                         (when *record-explanations-p*

                                                                           (unless (eq from1 from1a)
                                                                             (push 
                                                                              `((,from1 ,from1a) :same-as)
                                                                              last-explanation))
                                                                           
                                                                           (push `((,from1a ,current ,role1) ,role1 :is-subrole-of ,inv-role)
                                                                                 last-explanation))
                                                             
                                                                         t))

                                                                  
                                                                  (and (not *told-information-reasoning-p*)

                                                                       (or (needs-filler-reasoning-p *running-substrate*)
                                                                           ;;; role-transitive-or-transitive-subrole-p
                                                                           )
                                                                          
                                                                       (if (symbolp inv-role) 
                                                                           (dl-prover-internal-individuals-related-p *running-substrate*
                                                                                                                     from1
                                                                                                                     orig-from
                                                                                                                     inv-role
                                                                                                                     (abox *running-substrate*)
                                                                                                                     ;nil
                                                                                                                     )
                                                                         (dl-prover-internal-individuals-related-p *running-substrate*
                                                                                                                   orig-from
                                                                                                                   from1
                                                                                                                   role
                                                                                                                   (abox *running-substrate*)
                                                                                                                   ;nil
                                                                                                                   ))
								  
                                                                       (progn 
                                                                            
                                                                         (setf continue-p role-transitive-or-transitive-subrole-p)

                                                                         (setf syntactic-inference1-p nil)
                                                                               
                                                                         (when *record-explanations-p*

                                                                           (unless (eq from1 from1a)
                                                                             (push 
                                                                              `((,from1 ,from1a) :same-as)
                                                                              last-explanation))

                                                                           (push `((,from1a ,current ,role1) due-to-abox-reasoning)
                                                                                 last-explanation))

                                                                         t)))))


                                                          do 
                                                          
                                                          (progn 
                                              
                                                            (if *record-explanations-p*
                                                                (let ((*explanations*
                                                                       (cons last-explanation *explanations*)))
                                                                  (do-it from1
                                                                         :generated-succs generated-succs
                                                                         :continue-p 
                                                                         continue-p 
                                                                         :syntactic-inference-p
                                                                         syntactic-inference1-p
                                                                         :role-descendants
                                                                         (if (and continue-p syntactic-inference1-p)
                                                                             (progn 
                                                                               (when *debugging-p*
                                                                                 (format t "Changing focus role to ~A~%" inv-role1))
                                                                               (dl-prover-atomic-role-descendants *running-substrate* inv-role1))
                                                                           role-descendants)))
                                                
                                                              (do-it from1 
                                                                     :generated-succs generated-succs
                                                                     :continue-p
                                                                     continue-p
                                                                     :syntactic-inference-p 
                                                                     syntactic-inference1-p 
                                                                     :role-descendants
                                                                     (if (and continue-p syntactic-inference1-p)
                                                                         (progn 
                                                                           (when *debugging-p*
                                                                             (format t "Changing focus role to ~A~%" inv-role1))
                                                                           (dl-prover-atomic-role-descendants *running-substrate* inv-role1))
                                                                       role-descendants)))))))))))))

                     ;;;
                     ;;;
                     ;;; 
                     
                     (if (not (eq succs :unknown))
                         (dolist (to succs)
                           (setf orig-from to)
                           (if *record-explanations-p*
                               (let ((*explanations*
                                      (cons (list `((,from ,to ,role) cache-hit) )
                                            *explanations*)))
                                 (do-it to
                                        :first-p nil
                                        :role-descendants role-descendants1))
                             (do-it to :first-p nil
                                    :role-descendants role-descendants1)))
                       (prog1
                           (progn 
                             (setf orig-from from)
                             (do-it from :first-p t :role-descendants role-descendants1))
                         (when (and (not only-one-p) ; sonst nicht vollstaendig! 
                                    (=> (or (needs-filler-reasoning-p *running-substrate*)
                                            ;;; role-transitive-or-transitive-subrole-p
                                            )
                                        (not *told-information-reasoning-p*)))
                           (with-critical-section
                             (unless (dl-prover-role-successors-cache-complete-for-role-p *running-substrate* from role)
                               (dl-prover-register-role-successors-cache-is-complete-for-role *running-substrate* from role)))))))))))))))


(defmethod evaluate-dl-prover-retrieve-individual-fillers ((substrate dl-prover-substrate)
                                                           from role continuation
                                                           &rest args 
                                                           &key  
                                                           bound-to
                                                           ;;; if to specified -> check if related! 
                                                           query
                                                           only-one-p 
							   only-if-p ; function
                                                           negated-p
                                                           inverse-p &allow-other-keys)
  (declare (ignorable args))

  ;; (unless query
  ;;  (nrql-error "Runtime error: query missing"))

  ;;; (pprint (/ 3 0))

  (let ((only-one-p 
         (or only-one-p
             bound-to
             (and query
                  (member (voi-to query) 
                          (existential-vois query))))))

    (if (negated-dl-role-p role)

        ;;;
        ;;; (?*x ?*y (NOT R)), (NOT (?*x ?*y (NOT R)))
        ;;;     

        (if negated-p 

            ;;;
            ;;; (NOT (?*x ?*y (not R))) ?
            ;;; 

            (let* ((inds 
                    (ensure-list (or bound-to (dl-prover-all-individuals *running-substrate*))))
                   (negated-succs (copy-list inds)))

              (declare (ignorable inds negated-succs))
            
              (abortable-dolist (to inds)
                (apply #'evaluate-dl-prover-check-individuals-related-p 
                       *running-substrate* 
                       from to role
                       #'(lambda (&rest args)
                           (declare (ignorable args))
                           (setf negated-succs (delete to negated-succs)))
                       :negated-p nil
                       :inverse-p nil
                       :query query
                       nil))
            
              (abortable-dolist (var negated-succs)
                (if inverse-p 
                    (apply continuation :from var args)
                  (apply continuation :to var args))))
          
          ;;;
          ;;; (?*x ?*y (not R))
          ;;; 

          (let* ((role (second role))
                 (equal-role-p (is-equal-role-p role))
                 (different-from-role-p (is-different-from-role-p role))
                 (role (if inverse-p 
                           (if (consp role)
                               (second role)
                             (dl-prover-atomic-role-inverse *running-substrate* role))
                         role))
                 (inverse2-p (consp role))
                 (role (if (consp role)
                           (second role)
                         role))
                 (role (cond (equal-role-p 
                              (first +equal-roles+))
                             (different-from-role-p
                              (first +different-from-roles+))
                             (t role)))                 
                 (inds (ensure-list (or bound-to (dl-prover-all-individuals *running-substrate*)))))

            (when (consp role) (error "Bad inverse role! Should not happen!"))
        
            (dolist (to inds)
           
              (let ((abox-consistent-p nil)
                    (added-p nil)
                    (from (if inverse2-p to from))
                    (to (if inverse2-p from to)))
                   
                (with-critical-section
                
                  (cond (equal-role-p 
                         
                         (nrql-runtime-error 'rewriting-error-found-negated-equal-role))

                        (different-from-role-p 
                         
                         (nrql-runtime-error 'rewriting-error-found-negated-different-from-role))

                        (t

                         (unless (loop as role-axiom in 
                                       (dl-prover-all-role-assertions-for-individual-in-domain 
                                        *running-substrate* from)
                                       as role1 = (second role-axiom)
                                       as to1 = (second (first role-axiom))
                                       thereis (and (same-abox-individual-p to1 to)
                                                    (same-role-p role1 role)))
                           (setf added-p t)
                           (dl-add-role-assertion *running-substrate* *running-abox* from to role))

                         (setf abox-consistent-p
                               (if (is-racer-substrate-p *running-substrate*)
                                   (without-unique-name-assumption
                                     (dl-prover-abox-consistent-p *running-substrate* *running-abox*))
                                 (dl-prover-abox-consistent-p *running-substrate* *running-abox*)))
                  
                         (when added-p 
                           (dl-forget-role-assertion *running-substrate* *running-abox* from to role))

                         #+:ignore
                         (when (and (not abox-consistent-p)
                                    (not 
                                     (without-unique-name-assumption
                                       (dl-prover-abox-consistent-p *running-substrate* *running-abox*))))
                           (error "!"))
                           
                         )))
             
                (unless abox-consistent-p 
                  ;; (terpri)
                  ;; (format t "****** Found: NOT(~A) ~A ~A" role from to)
                  (apply continuation :from from :to to :inverse-p nil args))))))
          
      ;;;
      ;;; (?*x ?*y R), (NOT (?*x ?*y R))
      ;;; 
    
      (let* ((equal-role-p (is-equal-role-p role))

             (different-from-role-p (is-different-from-role-p role))

             (role (cond (equal-role-p 
                          (first +equal-roles+))
                         (different-from-role-p 
                          (first +different-from-roles+))
                         (t
                          (if inverse-p 
                              (dl-prover-atomic-role-inverse *running-substrate* role)
                            role))))

             (inv-role (cond (equal-role-p 
                              (first +equal-roles+))
                             (different-from-role-p 
                              (first +different-from-roles+))
                             (t
                              (dl-prover-atomic-role-inverse *running-substrate* role)))))

        (if negated-p
              
            ;;;
            ;;; (NOT (?*x ?*y R))
            ;;; 
          
            (let* ((inds (ensure-list (or bound-to (dl-prover-all-individuals *running-substrate*))))
                   (negated-succs (copy-list inds)))

              (declare (ignorable inds negated-succs))
                 
              (abortable-dolist (to inds)
                (apply #'evaluate-dl-prover-check-individuals-related-p 
                       *running-substrate*
                       from to role 
                       #'(lambda (&rest args)
                           (declare (ignorable args))
                           (setf negated-succs (delete to negated-succs)))
                       :negated-p nil
                       :inverse-p nil
                       :query query
                       nil))
            
              (abortable-dolist (var negated-succs)
                (if inverse-p 
                    (apply continuation :from var args)
                  (apply continuation :to var args))))

          ;;;
          ;;; (?*x ?*y R)
          ;;; 
          
          (let* ((query-role role)

                 (role-descendants1 
                  (unless (or equal-role-p different-from-role-p)
                    (dl-prover-atomic-role-descendants *running-substrate* role)))

                 (role-transitive-or-transitive-subrole-p 
                  (and (not (or equal-role-p different-from-role-p))
                       (dl-prover-role-transitive-or-transitive-subrole-p *running-substrate* role)))

                 (done nil)
                 
                 (succs (dl-prover-all-role-successors *running-substrate* from role))

                 (orig-from nil)

                 (from-syns (dl-prover-individual-synonyms1 *running-substrate* from))

                 (generated-succs (mht))
                 
                 (focus-roles (mht :test #'equalp)))
		 

	    (labels ((transitive-or-has-transitive-super-role-p (role)
                       (and (not (or equal-role-p different-from-role-p))
                            (dl-prover-role-transitive-or-transitive-superrole-of-subrole-p 
                             *running-substrate* role query-role)))

                     (get-most-general-transitive-superroles (role)
                       (when (not (or equal-role-p different-from-role-p))
                      
                         (let ((agenda (list 
                                        (list role
                                              (if (dl-prover-transitive-role-p *running-substrate* role)
                                                  role 
                                                nil))))
                               (result nil))
                          
                           (loop while agenda do 
                                 (let* ((first (pop agenda))
                                        (role (first first))
                                        (parents 
                                         (dl-prover-atomic-role-parents *running-substrate* role)))
                                   (cond ((or (equal parents (list racer:+top-object-role-symbol+))
                                              (not parents))
                                          (push first result))
                                         (t (dolist (parent parents)
                                              (unless (assoc parent agenda :test #'equal)
                                                (if (dl-prover-transitive-role-p *running-substrate* parent)
                                                    (push (list parent parent) agenda)
                                                  (push (list parent (second first)) agenda))))))))

                           (remove nil (mapcar #'second result)))))

                     (get-subroles-of-most-general-transitive-superroles-of (role)
                       (let* ((role1
                               (if (consp role)
                                   (second role)
                                 role))
                              (res
                               (or (gethash role focus-roles)
                                   (setf (gethash role focus-roles)
                                         (let ((res
                                                (apply #'append 
                                                       (mapcar #'(lambda (role)
                                                                   (dl-prover-atomic-role-descendants 
                                                                    *running-substrate* role))
                                                               (get-most-general-transitive-superroles role1)))))
                                           (if (consp role)
                                               (mapcar #'(lambda (role) 
                                                           (dl-prover-atomic-role-inverse *running-substrate* role))
                                                       res)
                                             res))))))
                                               
                         (when *debugging-p*
                           (format t "~%~% Most general transitive superroles of ~A: " role)
                           (pprint (get-most-general-transitive-superroles role))
                           (format t "~%Subroles of most general transitive superroles:")
                           (pprint res))

                         res))
                           

                     (do-it (current &key role-descendants continue-p first-p (syntactic-inference-p t))

		       ;;;
		       ;;; register successor, apply continuation for successor
		       ;;; 

                       (when  *debugging-p* 
                         (when first-p (format t "~%~%**** NEW ORIG ***************************************"))
                         (format t "~%~%Current: ~A~%Continue: ~A~%Orig from: ~A~%Role: ~A~%Inverse: ~A~%First: ~A~%" current continue-p orig-from
                                 role inverse-p first-p)
                         (pprint role-descendants)
                         (terpri))


                       (dolist (current ;(if *told-information-reasoning-p*
                                        ;    (list current)
                                (dl-prover-individual-synonyms1 *running-substrate* current))
			 
			 (let ((succ-p 
				(=> only-if-p 
				    (funcall only-if-p current))))

			   (unless first-p 
			     (when succ-p
			     
                               ;;; found a successor! 
                         
			       (when continue-p
				 (setf (gethash current generated-succs) t))

			       (when *debugging-p* 
				 (format t "~%~%!!!!!!!!! Found successor ~A!~%" current))

			       (with-critical-section
                                 (dolist (from from-syns)
                                   (unless (dl-prover-role-successors-cache-complete-for-role-p *running-substrate* from role)
                                     (unless (dl-prover-is-known-role-successor-of-p *running-substrate* current from role)
                                       (dl-prover-register-role-successor *running-substrate* current from role)))))
                         
			       (when only-one-p 
				 (when *debugging-p* 
				   (format t "Done!~%" ;;current
                                           ))
				 (setf done t)))
                       
			     (if inverse-p 
				 (progn 
				   (when *debugging-p* 
				     (format t "Now continuation, :from ~A ~A"  current args))
				   (apply continuation :from current args))

			       (progn 
				 (when *debugging-p* 
				   (format t "Now continuation, :to ~A ~A"  current args))
				 (apply continuation :to current args))))
                           ;;;
                           ;;;
                           ;;;

                           (when *debugging-p*
                             (format t
                                     "~%Back from continuation, continue: ~A. Flags: ~A"
                                     (and (or continue-p first-p)
				      (not done)
				      succ-p)
                                     (list continue-p first-p (not done) succ-p)))

                           ;;;
                           ;;;
                           ;;;
                           
			   (when (and (or continue-p first-p)
				      (not done)
				      succ-p)

			     (cond ((is-equal-role-p role)
                                
				    (dolist (to1 
                                             (dl-prover-individual-synonyms1 *running-substrate* current))

				      (do-it to1 :continue-p nil)))

				   ((is-different-from-role-p role)

				    (dolist (to1 (dl-prover-individual-antonyms *running-substrate* current))

				      (do-it to1 :continue-p nil)))

				   (t
                       
                                    ;;;
                                    ;;; Navigiere anhand von Domain Role Assertions
                                    ;;; 
                                                             
				    (when *debugging-p* 
				      (format t "~%Now: ~A, role assertions for ~A in domain. Role transitive-or-transitive-subrolep: ~A~%" 
                                              current current role-transitive-or-transitive-subrole-p))
                                
				    (loop as role-axiom in 
					  (dl-prover-all-role-assertions-for-individual-in-domain *running-substrate* current)
                                          as role1 = (second role-axiom)
                                          as to1 = (second (first role-axiom))

                                          ;;; muss ich "weitergehen" und indirekte Nachfolger berechnen? 
                                          ;;; ja, wenn die Rolle der Rollenassertion eine transitive Oberrolle hat, 
                                          ;;; und diese trans. Oberrolle auch eine Unterrolle der Anfragerolle ist 

                                          as continue-p = nil 

                                          do 

					  (let ((syntactic-inference1-p syntactic-inference-p))
                                
					    (when                                               
                                              
						(with-critical-section
                                                     
                                                  (when *debugging-p* 
                                                    (format t "~%Domain assertion: ~A~%" role-axiom)
                                                    (format t "~A is in ~A: ~A~%" 
                                                            role1 role-descendants
                                                            (when (member role1 role-descendants :test #'equal) t)))
                                                
						  (and (not (gethash to1 generated-succs))

                                                       ;;; syntaktisches Kriterium (solange nicht ein "echt" inferierter gefunden wurde): 
                                                          
						       (or 
                                                           
							(and syntactic-inference1-p
                                                                
							     (member role1 role-descendants :test #'equal)
                                                                
							     (progn 
                                                                  
							       (setf continue-p
                                                                     (transitive-or-has-transitive-super-role-p role1))

							       (when (and *debugging-p* continue-p)
								 (format t ">>>>>>>>> Continue~%"))
                                                                     
                                                               ;;; f. umgebendes AND

							       t))

                                                        ;;; semantisches Kriterium 

							(and (not *told-information-reasoning-p*)

							     (or (needs-filler-reasoning-p *running-substrate*)
                                                                 ;;; neu: aufgeweicht, gegenbeispiel von Docomo, 
                                                                 ;;; s. test-cases/docomo-test
                                                                 ;;; role-transitive-or-transitive-subrole-p
                                                                 ;;; doch nicht erforderlich! lag an Fehler in
                                                                 ;;; internal-individuals-related-p!
								 )

							     (progn 
							       (when *debugging-p*
								 (format t "Have to ask Racer whether ~A is an ~A successor of ~A...~%" to1 role orig-from))
							       t)

							     (if (symbolp role) 
								 (dl-prover-internal-individuals-related-p *running-substrate*
                                                                                                           ;;; orig-from ist richtig! 
													   orig-from
													   to1
													   role
													   (abox *running-substrate*)
													   ;nil
                                                                                                           )

							       (dl-prover-internal-individuals-related-p *running-substrate*
													 to1 
                                                                                                         ;;; orig-from ist richtig! 
													 orig-from
													 inv-role
													 (abox *running-substrate*)
													 ;nil
                                                                                                         ))
							     (progn 
								  
							       (when *debugging-p*
								 (format t " -> yes is a sucessor!~%"))

                                                               ;;; hier muss ich auf jeden Fall weitergehen, denn ich weiss nicht, 
                                                               ;;; ob nicht eine transitive Zwischenrolle fuer Ueberspringer-Kanten
                                                               ;;; verantwortlich sein koennte! (es sei denn, es gibt gar keine
                                                               ;;; transitiven Rollen -> trivial) 
                                                            
							       (setf continue-p role-transitive-or-transitive-subrole-p)
                                                            
                                                               ;;; da ich nun nicht weiss, welche Roleassertion fuer die Inferenz verantwortlich 
                                                               ;;; ist (war ein transitiver Schluss involviert?), darf ich von nun an 
                                                               ;;; keine syntaktsiche "Subrole"-Inferenz (obigen Klauseln) mehr machen, da 
                                                               ;;; ich sonst u.U. inkorrekte indirekte (transitive) Nachfolger ermittelt: 
                                                               ;;; das Gegenbeispiel hierzu ist test-cases/docomo-test.racer !
                                                            
							       (setf syntactic-inference1-p nil)
                                                            
                                                               ;;; f. umgebendes AND

							       t)))))

					      (when *debugging-p*
						(format t "Continue flag: ~A~%" continue-p))
                                              
					      (do-it to1 
                                                     :continue-p
                                                     continue-p
                                                     :syntactic-inference-p 
                                                     syntactic-inference1-p 
                                                     :role-descendants
                                                     (if (and continue-p syntactic-inference1-p)
                                                         (progn
                                                           (when *debugging-p*
                                                             (format t "Changing focus role to ~A~%" role1))
                                                           (get-subroles-of-most-general-transitive-superroles-of role1))
                                                       role-descendants)))))

                                    ;;;
                                    ;;; Navigiere anhand von Range Role Assertions
                                    ;;; 
                                
				    (when *debugging-p*
				      (format t "~%~%Now: ~A, role assertions for ~A in range. Role transitive-or-transitive-subrolep: ~A~%" 
                                              current current role-transitive-or-transitive-subrole-p))
                                
				    (loop as role-axiom in 
					  (dl-prover-all-role-assertions-for-individual-in-range *running-substrate* current)
                                          as role1 = (second role-axiom) 
                                          as inv-role1 = (dl-prover-atomic-role-inverse *running-substrate* role1)
                                          as from1 = (first (first role-axiom))

                                          as continue-p = nil

                                          do 

					  (let ((syntactic-inference1-p syntactic-inference-p))

					    (when

						(with-critical-section

                                                  (when *debugging-p*
                                                    (format t "~%Range assertion: ~A~%" role-axiom)
                                                    (format t "~A is in ~A: ~A~%" 
                                                            inv-role1 role-descendants
                                                            (when (member inv-role1 role-descendants :test #'equal) t)))
                                                     
						  (and (not (gethash from1 generated-succs))

						       (or 

							(and syntactic-inference1-p
                                                           
							     (member inv-role1 role-descendants :test #'equal)

							     (progn 

							       (setf continue-p 
                                                                     (transitive-or-has-transitive-super-role-p inv-role1))
                                                             
							       (when (and *debugging-p* continue-p)
                                                                 (format t ">>>>>>>> Continue~%"))

							       t))                                                      

							(and (not *told-information-reasoning-p*)

							     (or (needs-filler-reasoning-p *running-substrate*)
								 ;; role-transitive-or-transitive-subrole-p
								 )

							     (progn 
							       (when *debugging-p*
								 (format t "Have to ask Racer whether ~A is an ~A successor of ~A...~%" from1 role orig-from))
							       t)

							     (if (symbolp inv-role) 
								 (dl-prover-internal-individuals-related-p *running-substrate*
													   from1
													   orig-from
													   inv-role
													   (abox *running-substrate*)
													   ;nil
                                                                                                           )
							       (dl-prover-internal-individuals-related-p *running-substrate*
													 orig-from
													 from1
													 role
													 (abox *running-substrate*)
													 ;nil
                                                                                                         ))

							     (progn 
                                                             
							       (when *debugging-p*
								 (format t " -> yes is a successor!~%"))
                                                             
							       (setf continue-p role-transitive-or-transitive-subrole-p)

							       (setf syntactic-inference1-p nil)
								  
							       t)))))


					      (when *debugging-p*
						(format t "Continue flag: ~A~%" continue-p))
                                              
					      (do-it from1 
                                                     :continue-p
                                                     continue-p
                                                     :syntactic-inference-p 
                                                     syntactic-inference1-p 
                                                     :role-descendants
                                                     (if (and continue-p syntactic-inference1-p)
                                                         (progn 
                                                           (when *debugging-p*
                                                             (format t "Changing focus role to ~A~%" inv-role1))
                                                           (get-subroles-of-most-general-transitive-superroles-of inv-role1))
                                                       role-descendants))))))))))))
              
              ;;;
              ;;; Aufruf 
              ;;; 
              

              (if bound-to
                  
                  ;;; beide bereits gebunden? nur pruefen! 
                  
                  (cond (inverse-p 
                         (nrql-error "Bad inverse role found"))

                        ((not negated-p)

                         (cond ((is-equal-role-p role)
                                (when (member bound-to (dl-prover-individual-synonyms1 *running-substrate* from))
                                  (apply continuation :to bound-to args)))

                               ((is-different-from-role-p role)
                                (when (member bound-to (dl-prover-individual-antonyms *running-substrate* from))
                                  (apply continuation :to bound-to args)))

                               (t

                                (when (or (and (not (eq succs :unknown))
                                               (member bound-to succs))
                                          (dl-prover-internal-individuals-related-p *running-substrate*
                                                                                    from
                                                                                    bound-to
                                                                                    role
                                                                                    (abox *running-substrate*)
                                                                                    ;nil
                                                                                    ))
                                  (apply continuation :to bound-to args)))))
                               
                        (t
                         (when (or (and (dl-prover-role-successors-cache-complete-for-role-p *running-substrate* from role)
                                        (not (eq succs :unknown))
                                        (not (member bound-to succs))
                                        (not (is-equal-role-p role))
                                        (not (is-different-from-role-p role)))
                                   (not 
                                    (dl-prover-internal-individuals-related-p *running-substrate*
                                                                              from
                                                                              bound-to
                                                                              role
                                                                              (abox *running-substrate*)
                                                                              ;nil
                                                                              )))
                           (apply continuation :to bound-to args))))
                
                (if (not (eq succs :unknown))
                    (dolist (to succs)
                      (setf orig-from to)
                      (do-it to
                             :first-p nil
                             :role-descendants role-descendants1))
                  (prog1
                      (progn 
                        (setf orig-from from)
                        (do-it from :first-p t :role-descendants role-descendants1))
                    (when (and (not only-one-p) ; sonst nicht vollstaendig! 
			       (not only-if-p)
                               (=> (or (needs-filler-reasoning-p *running-substrate*)
                                       ;;; role-transitive-or-transitive-subrole-p
                                       ;;; nein, doch nicht, lag an Fehler in racer-internal-individuals-related-p!
                                       )
                                   ;; sonst unvollstaendig!
                                   (not *told-information-reasoning-p*)))
                      (with-critical-section
                        (dolist (from from-syns)
                          (unless (dl-prover-role-successors-cache-complete-for-role-p *running-substrate* from role)
                            (dl-prover-register-role-successors-cache-is-complete-for-role *running-substrate* from role)))))))))))))))

;;;
;;; Concrete Domain :CONSTRAINT Checking
;;; Enumeration steckt in defquery-code!!!! 
;;; Fuer den Fall, dass Queries wie (?x ?y (:constraint age age =)) ankommmen ->
;;; wenn keine Feature-Ketten muss doch enumeriert werden!
;;; hatte ich vergessen (sonst machen die Role-Atoms der Features die Enumeration)
;;; 



(defmethod get-code-racer-check-individuals-cd-related-p ((substrate dl-prover-substrate)
                                                          from to from-attrib to-attrib constraint body 
                                                          &rest args
                                                          &key negated-p 
                                                          &allow-other-keys)
  (declare (ignorable args))

  `(let* ((from ,from)
          (to ,to))

     (declare (ignorable from to))

     (if (not (and (dl-prover-individual-instance-p *running-substrate* from (quote (an ,from-attrib)))
                   (dl-prover-individual-instance-p *running-substrate* to (quote (an ,to-attrib)))))
          
         ;;; Attribute fehlen? -> negatives :constraint erfuellt!
         
         (when ,negated-p 
           ,body)
        
       ;;; Attribute implizit vorhanden

       (with-critical-section

         ,(let* ((from-name (if (equal from-attrib to-attrib) 
                                (intern (format nil "~A-1" from-attrib))
                              from-attrib))
              
                 (to-name (if (equal from-attrib to-attrib)
                              (intern (format nil "~A-2" to-attrib))
                            to-attrib)))

            `(let ((constraint 
                    (if (consp ',constraint)
                        (tree-map #'(lambda (x)
                                      (cond ((eq x ',from-name) (list ',from-attrib ,from))
                                            ((eq x ',to-name) (list ',to-attrib ,to))
                                            (t x)))
                                  ',constraint)
                      (list ',constraint
                            (list ',from-attrib ,from)
                            (list ',to-attrib  ,to)))))
         
               (,(if negated-p 'unless 'when)
              
                (dl-prover-constraint-entailed-p *running-substrate* constraint)
                
                ,body)))))))
      
(defmethod evaluate-racer-check-individuals-cd-related-p ((substrate dl-prover-substrate)
                                                          from to from-attrib to-attrib constraint continuation 
                                                          &rest args
                                                          &key query negated-p &allow-other-keys)
  (declare (ignorable args))

  
  (if (not (and (dl-prover-individual-instance-p *running-substrate* from 
                                                 (replace-syntactic-concept-expression-sugar 
                                                  ;;; notwendig, weil from-attrib auch ein
                                                  ;;; Datatype Property sein kann!
                                                  (parser query)
                                                  `(racer:an ,from-attrib)))
                (dl-prover-individual-instance-p *running-substrate* to 
                                                 (replace-syntactic-concept-expression-sugar 
                                                  (parser query)
                                                  `(racer:an ,to-attrib)))))

      (when negated-p 
        (apply continuation :from from :to to args))

    (with-critical-section
        
      (let* ((from-name (if (equal from-attrib to-attrib) 
                            (intern (format nil "~A-1" from-attrib))
                          from-attrib))
              
             (to-name (if (equal from-attrib to-attrib)
                          (intern (format nil "~A-2" to-attrib))
                        to-attrib))

             (constraint 
              (if (consp constraint)
                  (tree-map #'(lambda (x)
                                (cond ((eq x from-name) (list from-attrib from))
                                      ((eq x to-name) (list to-attrib to))
                                      (t x)))
                            constraint)
                (list constraint 
                      (list from-attrib from)
                      (list to-attrib to)))))
                     

        (if negated-p 
            (unless (dl-prover-constraint-entailed-p *running-substrate* constraint)
              (apply continuation :from from :to to args))

          (when (dl-prover-constraint-entailed-p *running-substrate* constraint)              
              
            (apply continuation :from from :to to args)))))))

;;;
;;; Has-Known-Successor Instance Checking
;;; 


(defmethod get-code-dl-prover-check-has-known-successor-p ((substrate dl-prover-substrate)
                                                           from role body &rest args &key negated-p &allow-other-keys)
  `(let ((successor-found-p nil))
     ,(apply #'get-code-dl-prover-retrieve-individual-fillers
             substrate
             from 
             role 
             `(setf successor-found-p t)
             :to 'temp
             :negated-p nil
             :only-one-p t
             args)
     (,(if negated-p 
           'unless 
         'when)
      successor-found-p 
      ,body)))


(defmethod evaluate-dl-prover-check-has-known-successor-p ((substrate dl-prover-substrate)
                                                           from role continuation &rest args &key negated-p &allow-other-keys)
  (let ((successor-found-p nil))
    (apply #'evaluate-dl-prover-retrieve-individual-fillers
           *running-substrate*
           from 
           role 
           #'(lambda (&rest args &key to &allow-other-keys)
               (declare (ignorable to args))
               (setf successor-found-p t))
           :negated-p nil
           :only-one-p t
           args)
    (if negated-p
        (unless successor-found-p 
          (apply continuation :var from args))
      (when successor-found-p 
        (apply continuation :var from args)))))

;;;
;;; Has-Known-Successor Instance Enumeration
;;; 

(defmethod get-code-dl-prover-retrieve-has-known-successor-instances ((substrate dl-prover-substrate)
                                                                      role body &rest args &key var &allow-other-keys)
  `(abortable-dolist (,var (dl-prover-all-individuals *running-substrate*))
     ,(apply #'get-code-dl-prover-check-has-known-successor-p 
             substrate 
             var role body
             args)))

(defmethod evaluate-dl-prover-retrieve-has-known-successor-instances ((substrate dl-prover-substrate)
                                                                      role continuation &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (abortable-dolist (var (dl-prover-all-individuals *running-substrate*))
    (apply #'evaluate-dl-prover-check-has-known-successor-p
           *running-substrate*
           var role continuation args)))



