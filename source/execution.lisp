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

(defmethod abox-has-changed-since-parsing-p ((query nrql-query))
  (with-slots (substrate kb-id) query
    ;; nur die Toplevel-Query hat KB-ID!
    (when kb-id
      (when (second kb-id)
        (not (= (get-abox-id substrate)
                (second kb-id)))))))

(defmethod tbox-has-changed-since-parsing-p ((query nrql-query))
  (with-slots (substrate kb-id) query
    (when (first kb-id)
      (not (= (get-tbox-id substrate)
              (first kb-id))))))

;;;
;;;
;;;

(defmethod abox-has-changed-since-parsing-p ((query query))
  nil)

(defmethod tbox-has-changed-since-parsing-p ((query query))
  nil)

;;;
;;;
;;;

(defun exclusive-queries ()
  (remove-if-not #'modifies-state-p 
                 
                 ;;; jede Query, die modifies-state-p hat, 
                 ;;; wird execute-query in with-critical-section
                 ;;; ausfuehren 
		 
		 (with-access-to-lifecycle-lists
                   (copy-list *active-queries*))))

(defun exclusive-rules ()
  (remove-if-not #'modifies-state-p 
                 
                 ;;; jede Query, die modifies-state-p hat, 
                 ;;; wird execute-query in with-critical-section
                 ;;; ausfuehren 
		 
		 (with-access-to-lifecycle-lists
                   (copy-list *active-rules*))))


(defun deadlock-queries ()
  
  ;;; wenn unter den exlusive-queries eine dabei ist, 
  ;;; die nicht pro-aktiv laeuft (also lazy-incremental), 
  ;;; dann duerfen wir keine neue Query starten! 
  ;;; 
  ;;; Denn die vorhandene Query hat Racer gelockt 
  ;;; (*process-lock*), und wartet, weil lazy, nun 
  ;;; auf den Aufruf (get-next-tuple ...). Der Lock 
  ;;; kann nicht freigegeben werden, wegen Exklusivitaet
  ;;; (die Query veraendert ja den Zustand von Racer)
  ;;; 
  ;;; Auf die Proaktiven kann man einfach warten - die 
  ;;; terminieren ja von selbst, dann wird der Lock 
  ;;; automatisch frei gegeben!  

  (if *disable-deadlock-checking* 
      nil  
  (remove-if #'proactive-tuple-computation-p (exclusive-queries))))


(defun deadlock-rules ()
  (if *disable-deadlock-checking* 
      nil
    (remove-if #'proactive-tuple-computation-p (exclusive-rules))))

;;;
;;;
;;;

(defun ensure-deadlock-prevention ()
  (let ((dq (deadlock-queries)))

    (when dq

      (query-deadlock-warning dq)

      (return-from ensure-deadlock-prevention t)))

  (let ((dr (deadlock-rules)))

    (when dr

      (rule-deadlock-warning dr)
      
      (return-from ensure-deadlock-prevention t)))

  (when (or (exclusive-queries)
            (exclusive-rules))

    ;;; diese terminieren dann von selbst 
    ;;; ALLE muessen terminieren, nicht nur
    ;;; die fuer (substrate query)!!!
    
    (wait-for-queries-to-terminate)
    (wait-for-rules-to-terminate)

    (return-from ensure-deadlock-prevention nil))


  nil)

;;;
;;;
;;;

(defmethod establish-bindings ((query query) bindings)
  (let ((parser (parser query))
        (substrate (substrate query)))
    (loop as (var val) in bindings do
          (let ((var (find-voi parser var)))
	    (when var
	      (setf (bound-to var) val)
              (let* ((cor-var (corresponding-voi var)))
                (when cor-var
                  (let ((cor-val 
                         (if (is-abox-voi-p cor-var)
                             (get-associated-abox-individual substrate val)
                           (get-associated-substrate-node substrate val))))
                    (unless cor-val 
                      (nrql-runtime-error 
                       "Can't bind ~A to ~A, since required corresponding individual does not exist!" var val))
                    (setf (bound-to cor-var) cor-val)))))))))

;;;
;;;
;;; 

(defmethod1 execute-query ((query query) &rest args
                          &key
                          (verbose-p t)
                          (dont-show-variables *dont-show-variables*)
                          (dont-show-head-projection-operators-p *dont-show-head-projections-operators-p*)
                          (dont-show-lambdas-p *dont-show-lambdas-p*)
                          
                          (how-many *how-many*)
                          (only-new-tuples-p *only-new-tuples-p*)
                          (timeout (or *timeout* #+:racer-server *server-timeout*))

                          (proactive-tuple-computation-p *proactive-tuple-computation-p*)
                          (tuple-at-a-time-p *tuple-at-a-time-p*)
                          (use-individual-synonyms-p *use-individual-synonyms-p*)
                          (check-abox-consistency-p *check-abox-consistency-p*)
                          (ensure-tbox-classification-p *ensure-tbox-classification-p*)
                          
                          (initial-abox-mirroring-p *initial-abox-mirroring-p*)
                          (initial-role-assertion-mirroring-p *initial-role-assertion-mirroring-p*)
                          (classify-concepts-in-instance-assertions-p *classify-concepts-in-instance-assertions-p*)

                          (exclude-permutations-p *exclude-permutations-p*)
                          (record-explanations-p *record-explanations-p*)
			  
                          &allow-other-keys)

  (declare (ignorable args))

  (push query *queries-started*)

  (setf (slot-value query 'special-answer) nil) ; f. reexecute!    
  
  (dolist (buq (bottom-up-evaluation-plan query))
    (prepare-to-run-query buq)
    (apply #'execute-query buq args))
  (dolist (buq (bottom-up-evaluation-plan query))
    (get-answer buq))
  

  (if (tbox-has-changed-since-parsing-p query)

      (progn 
        (warn-tbox-has-changed)
        (if (is-rule-p query)
            (reexecute-rule query)
          (reexecute-query query)))
    
    (progn

      (let ((id (iterator-id query)))

	(unless (bottom-up-component-query-p query)
          (if (is-rule-p query)
              (with-access-to-lifecycle-lists
                (setf *ready-rules* (delete query *ready-rules*))  
                (push query *active-rules*))
            (with-access-to-lifecycle-lists
              (setf *ready-queries* (delete query *ready-queries*))  
              (push query *active-queries*))))
        
        ;;; Werte, die erst zum Ausfuerhungszeitpunkt der Anfrage relevant werden 
        ;;; und auch variabel gehalten werden sollen (die gleiche Anfrage kann mit
        ;;; verschiedenen Werten ausgefuehrt werden) 

        (setf (slot-value query 'process) nil
              (slot-value query 'how-many) 

              (if (and (bottom-up-component-query-p query)
                       (negated-reference-p query))
                  nil
                ;;; denn dann muessen ALLE berechnet werden, sonst stimmt das Ergebnis
                ;;; von (neg (project-to ...)) nicht
                
                how-many)

              (slot-value query 'only-new-tuples-p) only-new-tuples-p

              (slot-value query 'verbose-p) verbose-p
              (slot-value query 'dont-show-head-projection-operators-p) dont-show-head-projection-operators-p
              (slot-value query 'dont-show-lambdas-p) dont-show-lambdas-p 
              (slot-value query 'dont-show-variables)
              (mapcar #'(lambda (x) (find-voi (parser query) x)) dont-show-variables)

              (slot-value query 'timeout) timeout
              (slot-value query 'tuple-at-a-time-p) tuple-at-a-time-p
              (slot-value query 'proactive-tuple-computation-p) proactive-tuple-computation-p
              (slot-value query 'check-abox-consistency-p) check-abox-consistency-p
              (slot-value query 'use-individual-synonyms-p) use-individual-synonyms-p
              (slot-value query 'ensure-tbox-classification-p) ensure-tbox-classification-p
	      
              (slot-value query 'initial-abox-mirroring-p) initial-abox-mirroring-p
              (slot-value query 'initial-role-assertion-mirroring-p) initial-role-assertion-mirroring-p
              (slot-value query 'classify-concepts-in-instance-assertions-p) classify-concepts-in-instance-assertions-p

              (slot-value query 'exclude-permutations-p) exclude-permutations-p
              (slot-value query 'record-explanations-p) record-explanations-p)
        
        (when (or (not only-new-tuples-p)
                  (not (slot-value query 'result-bindings-hash)))
          (setf (slot-value query 'result-bindings-hash) 
                (mht :test #'equal)))

        (when (and (not (slot-value query 'delta-bindings-hash))
                   (slot-value query 'subscribers))
          (setf (slot-value query 'delta-bindings-hash) 
                (mht :test #'equal)))
        
        (when (slot-value query 'delta-bindings-hash)
          (clrhash (slot-value query 'delta-bindings-hash)))
        (setf (slot-value query 'removed-tuples) nil)
        
        (save-dl-prover-state query)

        (establish-bindings query *established-bindings*)
	
        (setf (slot-value query 'process)
              (start-query-process
               (let ((*process-id* id))
	     
                 #+:nrql-error-handler
                 (handler-case
                     (handler-bind ((condition (lambda (c)
                                                 (declare (ignorable c))
                                                 #+(and :racer-server (or :allegro :lispworks))
                                                 (print-debug-info c))))
		   
                       (progn 
                         #+:sequential-query-scheduling 
                         (with-critical-section
                           (funcall (env-setup-fn query) query))
                         #-:sequential-query-scheduling 
                         (if (modifies-state-p query)
                             (with-critical-section                        
                               (funcall (env-setup-fn query) query))
                           (funcall (env-setup-fn query) query))))

                   (condition (condition)            
		 
                     ;;; diese Reihenfolge ist die einzige
                     ;;; die funktioniert!!! so lassen!
                     ;;; hoch komplex - process pool etc.! 
		 
                     (setf (slot-value query 'runtime-error) condition
                           (slot-value query 'process) nil)
		     
		     (if (is-rule-p query)
			 (abort-rule query)
		       (abort-query query))))
	     
                 #-:nrql-error-handler
		 (restart-case 
		     (progn 
		       #+:sequential-query-scheduling 
		       (with-critical-section
			   (funcall (env-setup-fn query) query))
		       #-:sequential-query-scheduling 
		       (if (modifies-state-p query)
			   (with-critical-section                        
			       (funcall (env-setup-fn query) query))
			 (funcall (env-setup-fn query) query)))
		   (nil nil
		       :report "Abort query / rule"
		       :interactive (lambda () 
				      (setf (slot-value query 'process) nil)
				      (if (is-rule-p query)
					  (abort-rule query)
					(abort-query query))
				      nil)
		     nil)))))

	(if (slot-value query 'process)        
	    (if (not tuple-at-a-time-p)
                (apply #'get-answer query args)
              (list id :running))
          (if *multiprocess-queries*
              (progn 
                (setf (slot-value query 'special-answer)
                      :acquire-process-failed-pool-size-exceeded)
                (list id :acquire-process-failed-pool-size-exceeded))
            (apply #'get-answer query args)))))))


(defmethod1 execute-query ((query nrql-query) 
                          &rest args
                          &key
                          (how-many *how-many*)
                          (only-new-tuples-p *only-new-tuples-p*)

                          (verbose-p t)
                          (dont-show-variables *dont-show-variables*)
                          (dont-show-head-projection-operators-p *dont-show-head-projections-operators-p*) 
                          (dont-show-lambdas-p *dont-show-lambdas-p*)
                          (dont-add-abox-duplicates-p *dont-add-abox-duplicates-p*)
                          (remove-duplicates-p *remove-duplicates-p*)
                          
                          (exclude-permutations-p *exclude-permutations-p*)
                          (record-explanations-p *record-explanations-p*)
                          (timeout (or *timeout* #+:racer-server *server-timeout*))
                          
                          (two-phase-processing-p *two-phase-processing-p*)
                          (deliver-phase-two-warning-tokens-p *deliver-phase-two-warning-tokens-p*)
                          (deliver-kb-has-changed-warning-tokens-p *deliver-kb-has-changed-warning-tokens-p*)
                          (proactive-tuple-computation-p *proactive-tuple-computation-p*)
                          (tuple-at-a-time-p *tuple-at-a-time-p*)
                          (add-rule-consequences-p *add-rule-consequences-p*)                          
                          (continuation-based-instance-retrieval-p *continuation-based-instance-retrieval-p*)
                          (use-individual-synonyms-p *use-individual-synonyms-p*)
                          (check-abox-consistency-p *check-abox-consistency-p*)
                          (ensure-tbox-classification-p *ensure-tbox-classification-p*)

                          (told-information-reasoning-p *told-information-reasoning-p*)
                          (initial-abox-mirroring-p *initial-abox-mirroring-p*)
                          (initial-role-assertion-mirroring-p *initial-role-assertion-mirroring-p*)
                          (classify-concepts-in-instance-assertions-p *classify-concepts-in-instance-assertions-p*)
			  
			  final-consistency-checking-p
			  runtime-consistency-checking-p
	    
                          &allow-other-keys)

  (declare (ignorable args))

  (push query *queries-started*)

  (setf (slot-value query 'special-answer) nil) ; f. reexecute!    

  (dolist (buq (bottom-up-evaluation-plan query))
    (prepare-to-run-query buq)
    (apply #'execute-query buq args))

  (dolist (buq (bottom-up-evaluation-plan query))
    (get-answer buq))

  ;;;
  ;;; diese Slots muessen schon hier gesetzt werden,
  ;;; wegen Deadlock-Erkennung! 
  ;;; 

  (setf (slot-value query 'modifies-state-p) 
        ;;; ensure-deadlock-prevention betrachtet nur
        ;;; modifies state, daher ist es wichtig, diesen
        ;;; Slot "strenger" zu setzen als bereits durch
        ;;; das Parsing (s. auch initialize-instance :after)
        ;;; geschehen 

        (or (modifies-state-p query) 
	    
	    final-consistency-checking-p
	    runtime-consistency-checking-p
	    
            (and add-rule-consequences-p 
                 (rule-con-pattern query))))
  
  (when (ensure-deadlock-prevention)
    
    (setf (slot-value query 'special-answer)
          :denied-due-to-deadlock-prevention)

    (return-from execute-query 
      (list (iterator-id query)
            :denied-due-to-deadlock-prevention)))

  (with-access-to-lifecycle-lists
    (when (modifies-state-p query)
      (let ((queries 
             (remove-if #'proactive-tuple-computation-p *active-queries*))
            (rules 
             (remove-if #'proactive-tuple-computation-p *active-rules*)))

        (when (or queries rules)

          (when queries
            (query-deadlock-warning queries))

          (when rules 
            (rule-deadlock-warning rules))
        
          (setf (slot-value query 'special-answer)
                :denied-due-to-deadlock-prevention)

          (return-from execute-query 
            (list (iterator-id query)
                  :denied-due-to-deadlock-prevention)))

        ;;; sonst sind nur Proaktive am Laufen

        (wait-for-queries-to-terminate)
        (wait-for-rules-to-terminate))))

  ;;;
  ;;;
  ;;;

  (if (tbox-has-changed-since-parsing-p query)
      
      (progn 
        (warn-tbox-has-changed)
        (if (is-rule-p query)
            (reexecute-rule query)
          (reexecute-query query)))

    (progn
      
      (let ((id (iterator-id query)))

        (unless (bottom-up-component-query-p query)
          (if (is-rule-p query)
              (with-access-to-lifecycle-lists
                (setf *ready-rules* (delete query *ready-rules*))
                (push query *active-rules*))

            (with-access-to-lifecycle-lists
              (setf *ready-queries* (delete query *ready-queries*))
              (push query *active-queries*))))
	
        (setf (slot-value query 'process) nil
              (slot-value query 'how-many) 
              
              (if (and (bottom-up-component-query-p query)
                       (negated-reference-p query))

                  nil
                  
                ;;; denn dann muessen ALLE berechnet werden, sonst stimmt das Ergebnis
                ;;; von (neg (project-to ...)) nicht

                how-many)

              (slot-value query 'only-new-tuples-p) only-new-tuples-p

              (slot-value query 'verbose-p) 
	      verbose-p
	      
              (slot-value query 'dont-add-abox-duplicates-p) 
	      dont-add-abox-duplicates-p
	      
              (slot-value query 'remove-duplicates-p) 
	      remove-duplicates-p
	      
              (slot-value query 'dont-show-head-projection-operators-p) 
	      dont-show-head-projection-operators-p
	      
              (slot-value query 'dont-show-lambdas-p) 
	      dont-show-lambdas-p 
	      
              (slot-value query 'dont-show-variables)
              (mapcar #'(lambda (x) (find-voi (parser query) x)) dont-show-variables)
              
              (slot-value query 'timeout) 
	      timeout
	      
              (slot-value query 'two-phase-processing-p) 
              ;;; fuer negated Queries (not (?x c)) darf
              ;;; two-phase-processing nicht verwendet werden!
              ;;; sonst kommen in Phase 1 schon zu viele Tupel
              ;;; zurueck... 
              (and two-phase-processing-p
                   (not (some #'negated-p (cons query (all-subqueries query)))))

              (slot-value query 'deliver-phase-two-warning-tokens-p) 
	      deliver-phase-two-warning-tokens-p
	      
              (slot-value query 'deliver-kb-has-changed-warning-tokens-p) 
	      deliver-kb-has-changed-warning-tokens-p 
              
              (slot-value query 'tuple-at-a-time-p) 
	      tuple-at-a-time-p

              (slot-value query 'proactive-tuple-computation-p)
	      proactive-tuple-computation-p

              (slot-value query 'add-rule-consequences-p) 
	      add-rule-consequences-p
              
	      (slot-value query 'exclude-permutations-p)
	      exclude-permutations-p
	      
              (slot-value query 'record-explanations-p) 
	      record-explanations-p
              
              (slot-value query 'check-abox-consistency-p) 
	      check-abox-consistency-p
	      
              (slot-value query 'use-individual-synonyms-p) 
	      use-individual-synonyms-p
	      
              (slot-value query 'ensure-tbox-classification-p) 
	      ensure-tbox-classification-p
              
              (slot-value query 'told-information-reasoning-p) 
              (or (is-tbox-mirror-substrate-p (substrate query)) 
                  told-information-reasoning-p)
	      
              (slot-value query 'initial-abox-mirroring-p) 
	      initial-abox-mirroring-p
	      
              (slot-value query 'initial-role-assertion-mirroring-p) 
	      initial-role-assertion-mirroring-p
              
	      (slot-value query 'classify-concepts-in-instance-assertions-p) 
	      classify-concepts-in-instance-assertions-p
              
              (slot-value query 'continuation-based-instance-retrieval-p) 
	      continuation-based-instance-retrieval-p)

        (when (or (not only-new-tuples-p)
                  (not (slot-value query 'result-bindings-hash)))
          (setf (slot-value query 'result-bindings-hash) 
                (mht :test #'equal)))

        (when (and (not (slot-value query 'delta-bindings-hash))
                   (slot-value query 'subscribers))
          (setf (slot-value query 'delta-bindings-hash) 
                (mht :test #'equal)))

        (when (slot-value query 'delta-bindings-hash)
          (clrhash (slot-value query 'delta-bindings-hash)))
        (setf (slot-value query 'removed-tuples) nil)

        (establish-bindings query *established-bindings*)

        ;;; (format t "TWO PHASE: ~A~%" (two-phase-processing-p query))

        (with-critical-section
          (save-racer-state query))

        (setf (slot-value query 'process)
              (start-query-process
               (let ((*process-id* id))

                 (let ((*runtime-evaluation-p* t))
		   
		   #+:nrql-error-handler 
                   (handler-case
                       (handler-bind ((condition (lambda (c)
                                                   (declare (ignorable c))
                                                   #+(and :racer-server (or :allegro :lispworks))
                                                   (print-debug-info c))))
                         (if (modifies-state-p query)
                             (with-critical-section
                               (funcall (env-setup-fn query) query))
                           (progn 
                             #+:sequential-query-scheduling 
                             (with-critical-section
                               (funcall (env-setup-fn query) query))
                             #-:sequential-query-scheduling 
                             (funcall (env-setup-fn query) query))))
		 
                     (condition (condition)                          
		   
                       ;;; diese Reihenfolge ist die einzige
                       ;;; die funktioniert!!! so lassen!
                       ;;; hoch komplex - process pool etc.! 
		   
                       (setf (slot-value query 'runtime-error) condition
                             (slot-value query 'process) nil)

		       (if (is-rule-p query)
			   (abort-rule query)
			 (abort-query query))))
	       
                   #-:nrql-error-handler 
		   (restart-case
		       (if (modifies-state-p query)
			   (with-critical-section
			       (funcall (env-setup-fn query) query))
			 (progn 
			   #+:sequential-query-scheduling 
			   (with-critical-section
			       (funcall (env-setup-fn query) query))
			   #-:sequential-query-scheduling 
			   (funcall (env-setup-fn query) query)))
		     (nil nil
			 :report "Abort query / rule"
			 :interactive (lambda () 
					(setf (slot-value query 'process) nil)
					(if (is-rule-p query)
					    (abort-rule query)
					  (abort-query query))
					nil)
		       nil))))))

	(if (slot-value query 'process)
	    
            (if (not tuple-at-a-time-p)
                (apply #'get-answer query args)
              (list id :running))

          (if *multiprocess-queries*
              (progn 
                (setf (slot-value query 'special-answer)
                      :acquire-process-failed-pool-size-exceeded)
                (list id :acquire-process-failed-pool-size-exceeded))
            (apply #'get-answer query args)))))))



(defmethod1 execute-rule ((query query) &rest args &key &allow-other-keys)
  (let ((*proactive-tuple-computation-p* nil))
    (apply #'execute-query query args)))

;;;
;;;
;;;       

(defmethod1 reexecute-query ((query query) &rest args)
  (multiple-value-bind (status query)
      (apply #'reprepare-query query args)
    (declare (ignorable status))
    (apply #'execute-query query args)))

(defmethod1 reexecute-rule ((query query) &rest args)
  (multiple-value-bind (status query)      
      (apply #'reprepare-rule query args)
    (declare (ignorable status))
    (apply #'execute-rule query args)))

