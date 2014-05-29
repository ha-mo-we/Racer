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


(defmethod check-abox-consistency ((substrate substrate) &key &allow-other-keys)
  t)


(defmethod check-abox-consistency ((substrate racer-substrate) &key racer-wuna-p) 
  (with-critical-section
    (with-slots (abox) substrate
      (unless (abox-consistent-p abox)
        (nrql-error "ABox ~A is inconsistent, querying denied" abox))
      (when (and racer-wuna-p
                 (not (abox-una-consistent-p abox)))
        (nrql-error "ABox ~A is UNA inconsistent! Querying denied" abox)))))

#+:midelora
(defmethod check-abox-consistency ((substrate midelora-substrate) &key &allow-other-keys)
  (with-critical-section
    (unless (prover::abox-consistent-p substrate)
      (nrql-error "ABox ~A is inconsistent, querying denied" substrate))))

;;;
;;;
;;;

(defmethod prepare-substrate1 ((substrate racer-substrate))
  (without-timeout 

    ;;; reset-substrate ist die einzige Funktion, die Zeit kostet; 
    ;;; daher soll nur dort ein Timeout erlaubt sein 

    (with-slots (new-inds-hash needs-filler-reasoning-p abox) substrate
      (clrhash new-inds-hash)
    
      (if (is-racer-tbox-mirror-substrate-p substrate)
          (setf needs-filler-reasoning-p nil)
        (let ((language (get-abox-language abox)))
          (setf needs-filler-reasoning-p
                (or needs-filler-reasoning-p
                    (find #\R language)
                    (find #\f language)
                    (find #\N language)
                    (find #\Q language)))))))
    
  (when (substrate-needs-reset-p substrate)                  

    ;;; hat sich die ABox geaendert seitdem die Substrate-Instanz
    ;;; erzeugt wurde? -> reset-substrate. 
    ;;; reset-substrate ruft compute-abox-mirror bzw.
    ;;; compute-tbox-mirror!
    
    (reset-substrate substrate)))


#+:midelora
(defmethod prepare-substrate1 ((substrate midelora-substrate))
  (without-timeout 

    ;;; reset-substrate ist die einzige Funktion, die Zeit kostet; 
    ;;; daher soll nur dort ein Timeout erlaubt sein 

    (with-slots (new-inds-hash needs-filler-reasoning-p tbox) substrate

      (clrhash new-inds-hash)
    
      (if (is-midelora-tbox-mirror-substrate-p substrate)
          (setf needs-filler-reasoning-p nil)
        (let ((language (prover::get-abox-language substrate)))
          (setf needs-filler-reasoning-p
                (or needs-filler-reasoning-p
                    (find #\f language)
                    (find #\n language)
                    (find #\q language)))))))
    
  (when (substrate-needs-reset-p substrate)                  

    ;;; hat sich die ABox geaendert seitdem die Substrate-Instanz
    ;;; erzeugt wurde? -> reset-substrate. 
    ;;; reset-substrate ruft compute-abox-mirror bzw.
    ;;; compute-tbox-mirror!
    
    (reset-substrate substrate)))


;;;
;;; Bevor die Query gestartet wird, wird (vom evtl. neuen Query-Prozess) 
;;; prepare-substrate-for-query-execution gerufen - das spiegelt dann
;;; erst Teile der ABox (oder die ganze)
;;; 

(defmethod prepare-substrate-for-query-execution ((substrate dl-prover-substrate) (query query))
  t)

(defmethod prepare-substrate-for-query-execution :after ((substrate racer-substrate) (query query))
  (with-critical-section
    (with-slots (check-abox-consistency-p
                 ensure-tbox-classification-p
                 added-premise-axioms) query
      (with-slots (tbox) substrate
	  
        (unless (is-tbox-mirror-substrate-p substrate)
          (when check-abox-consistency-p
            (handler-case
                (check-abox-consistency substrate)
              (error (error)
                (when added-premise-axioms
                  (with-critical-section
                    (forget-statement (tbox substrate) 
                                      (abox substrate)
                                      added-premise-axioms))
                  ;;; notwendig!
                  (substrate-needs-reset substrate))
                (error error))))

          (when ensure-tbox-classification-p
            (unless (dl-tbox-classified-p substrate tbox)
              (dl-classify-tbox substrate tbox)
              (unless (dl-tbox-coherent-p substrate tbox)
                (nrql-warning "TBox ~A is incoherent" tbox)))))

        (prepare-substrate1 substrate)))))


(defmethod prepare-substrate-for-query-execution ((substrate tbox-mirror-substrate) (query query))
  (with-critical-section      
    (with-slots (check-abox-consistency-p) query

      (setf check-abox-consistency-p nil))))

;;;
;;;
;;;

(defmethod prepare-substrate-for-query-execution :after ((substrate dl-prover-substrate) (query nrql-query))
  (with-critical-section
    (with-slots (added-premise-axioms
                 premise) query
       
      (setf added-premise-axioms nil)

      (when premise
        (let* ((*told-information-reasoning-p* t)
               (*dont-add-abox-duplicates-p* t))

          (setf added-premise-axioms
                (add-abox-assertions substrate premise)))

        ;;; darf nicht unter *told-information-reasoning-p* asugefuehrt werden!!!
        ;;; sonst Bugs!

        (prepare-substrate1 substrate)))))

;;;
;;;
;;;

(defmethod prepare-query-for-execution ((query query))
  (with-slots (counter abort-search-p) query
    
    (dolist (sq (cons query (all-subqueries query)))
      (with-slots (bindings-found-p bindings result-bindings) sq
        
        (setf bindings-found-p nil
              bindings nil
              result-bindings nil)))
    
    (setf counter 0)

    (setf abort-search-p nil)))


(defmethod invalidate-cached-bindings ((query query))
  (dolist (sq (cons query (all-subqueries query)))
    (setf (slot-value sq 'bindings) nil)))

;;;
;;;
;;;

(defmethod prepare-to-run-query ((query query))      
  (without-timeout
    
    (with-slots (env-setup-fn iterator-id) query
     
      (setf env-setup-fn 
            #'(lambda (query)

                (with-slots (initial-abox-mirroring-p
                             initial-role-assertion-mirroring-p
                             classify-concepts-in-instance-assertions-p 
                             
                             exclude-permutations-p
                             record-explanations-p
                             use-individual-synonyms-p
                             timeout 
                             substrate
                             answer-pattern
                             timeout-p 
                             abort-search-p
                             query-fn
                             iterator-id
                             use-repository-p
                             put-into-repository-p
                             source) query

                  (without-signature-checks
                    (with-dl-prover-state (query)

                      ;;; Specials binden - diese werden nur verwendet, 
                      ;;; um bei Modulen die nicht direkt das Query Objekt als
                      ;;; Parameter bekommen (racer-dummy-substrate... / 
                      ;;; racer-code, ...) nicht jedesmal entsp. Parameter mit-
                      ;;; uebergeben zu muessen! ansonsten sind alle relevanten
                      ;;; Parameterwerte im Query-Objekt selbst gespeichert!
	      
                      (let* ((*running-query* query)
                             (*running-substrate* substrate)                         
                             (*process-id* iterator-id)
                             (*previous-conjunct* nil)
			   
                             (*package* (if #+:midelora (is-midelora-substrate-p *running-substrate*)
                                          #-:midelora nil
                                          (find-package :prover)
					  (if (is-racer-descriptions-substrate-p *running-substrate*)
					      (racer-package *running-substrate*)
					    *package*)))

                             (*running-abox* (typecase *running-substrate*
                                               (racer-substrate
                                                (when (abox *running-substrate*)
                                                  (with-critical-section
                                                    (find-abox (abox *running-substrate*)))))
                                               #+:midelora 
                                               (midelora-substrate
                                                *running-substrate*)))
			     
                             (*running-tbox* (typecase *running-substrate* 
                                               (racer-tbox-mirror-substrate 
                                                (when (mirror-of-tbox *running-substrate*)
                                                  (with-critical-section 
                                                    (find-tbox (mirror-of-tbox *running-substrate*)))))
                                               (racer-substrate
                                                (when (tbox *running-substrate*)
                                                  (with-critical-section 
                                                    (find-tbox (tbox *running-substrate*)))))
                                               #+:midelora 
                                               (midelora-substrate
                                                (tbox *running-substrate*))))

                             (*initial-abox-mirroring-p* initial-abox-mirroring-p)
                             (*initial-role-assertion-mirroring-p* initial-role-assertion-mirroring-p)
                             (*classify-concepts-in-instance-assertions-p* classify-concepts-in-instance-assertions-p)
                             
                             (*use-individual-synonyms-p* use-individual-synonyms-p)
                             
                             (*exclude-permutations-p* exclude-permutations-p)

                             (*record-explanations-p* record-explanations-p))
			
                        (prepare-substrate-for-query-execution *running-substrate* *running-query*)
                      
                        (unless source ; ansonsten einkompiliert! 
                          (when use-repository-p
                            (compute-cache-references *running-query* put-into-repository-p)))
                      
                        (with-new-marking-context        

                          (prepare-query-for-execution query)
		  
                          ;; Call Hook 
                          (querying-started *running-substrate* query)

                          (unwind-protect
                              (catch 'query-execution 
                           
                                (with-timeout-cleanup ; f. uebergeordneten Timeout! 
				    
                                 (if timeout                                 
                                     (with-timeout
                                         ;;; zumindest die Racer-API-Funktionen
                                         ;;; werden niemals ein with-timeout 
                                         ;;; an dieser Stelle aufsetzen; s. with-racer-timeout
                                         ;;; in den racer-api-Funktionsdefinitionen
                                         ;;; das timeout hier ist ein "historisches Feature" 
                                         (timeout
                                          (setf abort-search-p t
                                                timeout-p t))
                                       (funcall query-fn))
                              
                                   ;;; Racer-API in diesen Zweig
                                   (funcall query-fn))

                                 (progn 
                                   (setf abort-search-p t
                                         timeout-p t)

                                   (substrate-needs-reset *running-substrate*))))
		    
                            ;; Call Hook 
                            (querying-ended *running-substrate* query)
		    
                            (if (or (not answer-pattern) 
                                    ;; in diesem Falle wurde nur eine Binding erzeugt!
                                    ;; nicht als Cache nutzbar! 
                                    abort-search-p)
                                (invalidate-cached-bindings query)
		      
                              (setf (slot-value query 'valid-qbox-entry-p) t))
		    
                            'done))))))))
    

      (unless (bottom-up-component-query-p query)
        (if (is-rule-p query) 
            (progn 
              (push query *all-rules*)
              (push query *ready-rules*))
          (progn 
            (push query *all-queries*)
            (push query *ready-queries*))))
    
      (list iterator-id :ready-to-run))))



(defmethod prepare-to-run-query ((query nrql-query))      
  (without-timeout
    (with-slots (iterator-id env-setup-fn) query
      (setf env-setup-fn 
            #'(lambda (query)
	  
                (with-slots (initial-abox-mirroring-p
                             initial-role-assertion-mirroring-p
                             classify-concepts-in-instance-assertions-p 
                             told-information-reasoning-p
                             dont-add-abox-duplicates-p 
                             remove-duplicates-p

                             exclude-permutations-p
                             record-explanations-p
                             use-individual-synonyms-p
                             continuation-based-instance-retrieval-p 
                             ensure-tbox-classification-p
                             
                             timeout 
                             substrate
                             answer-pattern
                             timeout-p 
                             abort-search-p
                             query-fn
                             iterator-id
                             source
                             put-into-repository-p
                             use-repository-p) query

                  (without-signature-checks
                    (with-dl-prover-state (query)
	      
                      (let* ((*running-query* query)
                             (*running-substrate* substrate)                         
                             (*process-id* iterator-id)
                             (*previous-conjunct* nil)

                             (*dont-add-abox-duplicates-p* dont-add-abox-duplicates-p)
                             (*remove-duplicates-p* remove-duplicates-p)

                             (*ensure-tbox-classification-p* ensure-tbox-classification-p)
                         
                             (*told-information-reasoning-p* told-information-reasoning-p)
			   
                             (*continuation-based-instance-retrieval-p* continuation-based-instance-retrieval-p)
			   
                             (*package* (if #+:midelora (is-midelora-substrate-p *running-substrate*)
                                          #-:midelora nil
                                          (find-package :prover)
					  (racer-package *running-substrate*)))
		     
                             (*running-abox* (typecase *running-substrate*
                                               (racer-substrate
                                                (when (abox *running-substrate*)
                                                  (with-critical-section
                                                    (find-abox (abox *running-substrate*)))))
                                               #+:midelora 
                                               (midelora-substrate
                                                *running-substrate*)))
                             (*running-tbox* (typecase *running-substrate* 
                                               (racer-tbox-mirror-substrate 
                                                (when (mirror-of-tbox *running-substrate*)
                                                  (with-critical-section 
                                                    (find-tbox (mirror-of-tbox *running-substrate*)))))
                                               (racer-substrate
                                                (when (tbox *running-substrate*)
                                                  (with-critical-section 
                                                    (find-tbox (tbox *running-substrate*)))))
                                               #+:midelora 
                                               (midelora-substrate
                                                (tbox *running-substrate*))))

                             (*initial-abox-mirroring-p* initial-abox-mirroring-p)
                             (*initial-role-assertion-mirroring-p* initial-role-assertion-mirroring-p)
                             (*classify-concepts-in-instance-assertions-p* classify-concepts-in-instance-assertions-p)
                                                          
                             (*use-individual-synonyms-p* use-individual-synonyms-p)
                             (*exclude-permutations-p* exclude-permutations-p)
                                               
                             (*record-explanations-p* record-explanations-p))

                        (prepare-substrate-for-query-execution *running-substrate* *running-query*)

                        (unless source ;; ansonsten einkompiliert
                          (when use-repository-p
                            (compute-cache-references *running-query* put-into-repository-p)))
		
                        (with-new-marking-context                     

                          (prepare-query-for-execution query)
		  
                          ;; Call Hook 
                          (querying-started *running-substrate* query)
		  
                          (unwind-protect
                              (catch 'query-execution 
                           
                                (with-timeout-cleanup
                            
                                 (if timeout       
                                     (with-timeout 
                                         (timeout
                                          (when (iterator-id query)
                                            (nrql-warning "Timeout for query ~A" (iterator-id query)))
                                          (setf abort-search-p t
                                                timeout-p t))
                                       (funcall query-fn))
                              
                                   (funcall query-fn))
                            
                                 (progn 
                                   (setf abort-search-p t
                                         timeout-p t)

                                   (substrate-needs-reset *running-substrate*))))
                            
                        
                            ;; Call Hook 
                            (querying-ended *running-substrate* query)
		    
                            (if (or (not answer-pattern) 
                                    abort-search-p)
                                (invalidate-cached-bindings query)
                              (setf (slot-value query 'valid-qbox-entry-p) t))
		    
                            'done))))))))
    
      (unless (bottom-up-component-query-p query)
        (if (is-rule-p query)
            (progn 
              (push query *all-rules*)
              (push query *ready-rules*))
          (progn 
            (push query *all-queries*)
            (push query *ready-queries*))))
    
      (list iterator-id :ready-to-run))))

(defmethod prepare-to-run-rule ((query query))      
  (prepare-to-run-query query))

;;;
;;;
;;;      

(defmethod new-ind-op-p ((parser simple-parser) op)
  (and (consp op)
       (member (to-keyword (first op))
               '(:new-ind :new-individual :individual :create-individual
                 :new-symbol :new-atom :new-concept))
       (if (third op)
           (and (symbolp (second op))
                (every #'(lambda (x) 
                           (voi-p parser x))
                       (cddr op)))
         (voi-p parser (second op)))))

;;;
;;;
;;;

(defmethod semantically-rewrite-query ((query query) (parser simple-parser) 
                                       &rest args &key &allow-other-keys)
  (declare (ignorable args))

  query)

;;;
;;;
;;;


(defmethod register-kb-id ((query query))
  t)

(defmethod register-kb-id ((query nrql-query))
  (with-slots (substrate) query
    (setf (slot-value query 'kb-id) 
          (get-kb-id substrate))))

;;;
;;;
;;;

(defmethod get-kb-id ((substrate dl-prover-substrate))
  (list (get-tbox-id substrate)
        (get-abox-id substrate)))

(defmethod get-kb-id ((substrate tbox-mirror-substrate))
  (when (slot-boundp substrate 'mirror-of-tbox)
    (list (get-tbox-id substrate)
          nil)))

;;;
;;;
;;;

(defmethod get-abox-id ((substrate racer-substrate))
  (get-abox-version (abox substrate)))

#+:midelora 
(defmethod get-abox-id ((substrate midelora-substrate))
  (prover::get-abox-version (abox substrate)))

;;;
;;;
;;;


(defmethod get-tbox-id ((substrate racer-substrate))
  (get-tbox-version (tbox substrate)))

(defmethod get-tbox-id ((substrate racer-tbox-mirror-substrate))
  (when (slot-boundp substrate 'mirror-of-tbox)
    (get-tbox-version (mirror-of-tbox substrate))))

#+:midelora 
(defmethod get-tbox-id ((substrate midelora-substrate))
  (prover::get-tbox-version (tbox substrate)))

#+:midelora
(defmethod get-tbox-id ((substrate midelora-tbox-mirror-substrate))
  (prover::get-tbox-version (mirror-of-tbox substrate)))
      
;;;
;;;
;;;

(defmethod prepare-query-int :around ((query t) (result-vois list) (substrate substrate) &rest args
                                      &key
                                      (established-bindings *established-bindings*)
                                      parser 
                                      (parser-class (get-parser-class-for-substrate substrate)) 
                                      &allow-other-keys)

  (with-critical-section

    (let* ((*ano-counter* 0)
           (parser (or parser 
                       (apply #'make-instance parser-class
                              :original-description (copy-tree (list result-vois query args))
                              :substrate substrate
                              :allow-other-keys t
                              args)))
           (treat-as-inds
            (intersection
             (get-vois-from parser query)
             (append (mapcar #'first established-bindings)
                     #| 
                    (mapcar #'(lambda (x) 
                                (get-cor-voi parser (first x)))
                            established-bindings)) |# 
                     )))

           (*treat-as-abox-inds* (remove-if #'(lambda (x) (substrate-thing-p parser x)) treat-as-inds))
           (*treat-as-substrate-inds* (remove-if-not #'(lambda (x) (substrate-thing-p parser x)) treat-as-inds))
           
           (inds nil)
           (result-vois
            (mapcar #'(lambda (x)                       
                        (if (ind-p parser x)
                            (progn 
                              (push x inds)
                              (get-var-for-ind parser x))
                          x))
                    result-vois)))

      (apply #'call-next-method 
             query
             result-vois
             substrate
             :parser parser
             :additional-head-inds (remove-duplicates 
                                    (append inds
                                            *treat-as-substrate-inds*
                                            *treat-as-abox-inds*))
             args))))


(defmethod prepare-query-int :around ((query t) (result-vois list) (substrate racer-dummy-substrate) &rest args
                                      &key
                                      (established-bindings *established-bindings*)
                                      parser 
                                      (parser-class (get-parser-class-for-substrate substrate)) rule-p 
                                      &allow-other-keys)
  (with-critical-section

    (if rule-p 
        (call-next-method)
      (let* ((*ano-counter* 0)
             (parser (or parser 
                         (apply #'make-instance parser-class
                                :original-description (copy-tree (list result-vois query args))
                                :substrate substrate
                                :allow-other-keys t 
                                args)))
             (treat-as-inds
              (intersection
               (get-vois-from parser query)
               (append (mapcar #'first established-bindings)
                       #|
                      (mapcar #'(lambda (x) 
                                  (get-cor-voi parser (first x)))
                              established-bindings) |#
                       )))
             
             (*treat-as-abox-inds* (remove-if #'(lambda (x) (substrate-thing-p parser x)) treat-as-inds))
             (*treat-as-substrate-inds* (remove-if-not #'(lambda (x) (substrate-thing-p parser x)) treat-as-inds))
           
             (inds nil)
             
             (order-by-ops nil)
             (agg-ops nil)
             (group-by-ops nil))

        (labels ((process-entry (x) 
                   
                   (when (ind-p parser x)
                     (push x inds))
				 
                   (cond ((is-abox-operator-projector-thing-p parser x) 

                          ;; (<op> ?x)  

                          #|
                          `(,(first x)
                            ,(if (ind-p parser (second x))
                                 (progn
                                   (push (second x) inds)
                                   (get-var-for-ind parser (second x)))
                               (second x))) |# 

                          (let ((var 
                                 (if (ind-p parser (second x))
                                     (progn
                                       (push (second x) inds)
                                       (get-var-for-ind parser (second x)))
                                   (second x))))

                            `((:lambda (,(second x) )
                                ,(ecase (to-keyword (first x))
                                   ((:all-types  :types 
                                     :instantiators  :all-instantiators)

                                    `(sort (maplist ensure-list (instantiators ,(second x) *current-abox*))
                                           'string-lessp :key 'first))

                                   ((:all-types-flat :all-instantiators-flat 
                                     :types-flat :instantiators-flat)

                                    `(sort-symbol-name-lessp
                                      (flatten
                                       (instantiators ,(second x) *current-abox*))))

                                   ((:direct-types :most-specific-types :most-specific-instantiators :direct-instantiators)
                                   
                                    `(sort (maplist ensure-list (most-specific-instantiators ,(second x) *current-abox*))
                                           'string-lessp :key 'first))

                                   ((:direct-types-flat :most-specific-types-flat 
                                     :most-specific-instantiators-flat :direct-instantiators-flat)

                                    `(sort-symbol-name-lessp
                                      (flatten 
                                       (most-specific-instantiators ,(second x) *current-abox*))))

                                   ((:describe) 

                                    `(describe-ind ,(second x) *current-abox*))

                                   ((:individual-synonyms)
                                    `(sort-symbol-name-lessp
                                      (retrieve-individual-synonyms ,(second x) nil *current-abox*)))

                                   ((:concept-synonyms)
                                    `(sort-symbol-name-lessp
                                      (atomic-concept-synonyms ,(second x) *current-abox*)))))
                                   
                              ,var)))

                         ((is-group-by-operator-thing-p parser x)
                          (let* ((vars 
                                  (mapcar #'(lambda (x)
                                              (if (ind-p parser x)
                                                  (progn
                                                    (push x inds)
                                                    (get-var-for-ind parser x))
                                                x))
                                          (rest x)))
                                 (op 
                                  `(:group-by ,@vars)))

                            (push op group-by-ops)
                            op))
                         
                         ((is-aggregation-operator-thing-p parser x)
                          (let* ((vars 
                                  (mapcar #'(lambda (x)
                                              (if (ind-p parser x)
                                                  (progn
                                                    (push x inds)
                                                    (get-var-for-ind parser x))
                                                x))
                                          (rest x)))
                                 (op 
                                  `(,(to-keyword (first x)) ,@vars)))

                            (push op agg-ops)
                            op))

                         ((is-order-by-operator-thing-p parser x)
                          (let* ((vars 
                                  (mapcar #'(lambda (x)
                                              (if (ind-p parser x)
                                                  (progn
                                                    (push x inds)
                                                    (get-var-for-ind parser x))
                                                x))
                                          (cddr x)))
                                 (op
                                  `(:order ,(to-keyword (second x)) ,@vars)))

                            (push op order-by-ops)
                            op))
                         
                         ((is-attribute-abox-projector-thing-p parser x) ; (<cd-attribut> ?x)  

                          (let ((attrib (convert-to-dl-prover-attribute-expression substrate
                                                                                   (first x))))
                            `(,attrib
                              ,(if (ind-p parser (second x))
                                   (progn
                                     (push (second x) inds)
                                     (get-var-for-ind parser (second x)))
                                 (second x)))))

                         ((is-told-value-abox-projector-thing-p parser x)
                                      
                          ;;; (told-value (<cd-attribut> ?x))
                                      
                          (let ((attrib (convert-to-dl-prover-attribute-expression substrate
                                                                                   (first (second x)))))
                            `(:told-value
                              (,attrib
                               ,(if (ind-p parser (second (second x)))
                                    (progn 
                                      (push (second (second x)) inds)
                                      (get-var-for-ind parser (second (second x))))
                                  (second (second x)))))))

                         ((is-told-value-if-exists-abox-projector-thing-p parser x)
                                      
                          ;;; (told-value-if-exists (<cd-attribut> ?x))
                                      
                          (let ((attrib (convert-to-dl-prover-attribute-expression substrate
                                                                                   (first (second x)))))
                            `(:told-value-if-exists
                              (,attrib
                               ,(if (ind-p parser (second (second x)))
                                    (progn 
                                      (push (second (second x)) inds)
                                      (get-var-for-ind parser (second (second x))))
                                  (second (second x)))))))

                         ((is-datatype-fillers-abox-projector-thing-p parser x)
                                      
                          ;;; (datatype-fillers (<datatype-role> ?x)) 

                          (let ((role (convert-to-dl-prover-role-expression substrate
                                                                            (first (second x)))))
                            `(:datatype-filler
                              (,role
                               ,(if (ind-p parser (second (second x)))
                                    (progn 
                                      (push (second (second x)) inds)
                                      (get-var-for-ind parser (second (second x))))
                                  (second (second x)))))))

                         ((is-annotation-datatype-fillers-abox-projector-thing-p parser x)
                                      
                          ;;; (annotation-fillers (<datatype-role> ?x)) 

                          (let ((role (convert-to-dl-prover-role-expression substrate
                                                                            (first (second x)))))
                            `(:annotation-datatype-filler
                              (,role
                               ,(if (ind-p parser (second (second x)))
                                    (progn 
                                      (push (second (second x)) inds)
                                      (get-var-for-ind parser (second (second x))))
                                  (second (second x)))))))

                         ((is-lambda-abox-projector-thing-p parser x) 
                          
                          ;;; ((<lambda> (x) ...) ?x)

                          (if (consp (first x))
                              `(,(first x)
                                ,@(mapcar #'process-entry (cdr x)))

                           
                            ;;; (<lambda> ...)
                           
                            x))
                                     
                         ((voi-p parser x) 
                          x)
				       
                         (t (parser-error 
                             "Bad head entry ~A" x)))))
             

          ;;; Symbole fuer Vars im Kopf gegen Variablen-Objekte ersetzen und
          ;;; Projektionsoperatoren vereinheitlichen (kanonisieren)  

          (let ((head (mapcar #'process-entry result-vois))
                (body #| (remove nil
                                 `(and ,query
                                       ,@(when inds
                                           (mapcar #'(lambda (ind)
                                                       `(:same-as ,(get-var-for-ind parser ind) ,ind))
                                                   inds))))
	                            ;;; aendert die Semantik!!!
				    |# 
		     
                      query))
	  
            (apply #'call-next-method 
                   body 
                   head
                   substrate
                   :additional-head-inds (remove-duplicates
                                          (append inds
                                                  *treat-as-substrate-inds*
                                                  *treat-as-abox-inds*))
                   :parser parser
                   :group-by-ops group-by-ops 
                   :order-by-ops order-by-ops 
                   :agg-ops agg-ops 
                   args)))))))

(defmethod prepare-query-int :around ((query t) (result-vois list) (substrate tbox-mirror-substrate) &rest args
                                      &key 
                                      (established-bindings *established-bindings*)
                                      parser (parser-class (get-parser-class-for-substrate substrate)) &allow-other-keys)
  (with-critical-section
    (let ((*racer-check-if-atoms-defined-p* nil))    
      (let* ((*ano-counter* 0)
             (parser (or parser 
                         (apply #'make-instance parser-class 
                                :original-description (copy-tree (list result-vois query args))
                                :substrate substrate 
                                :allow-other-keys t 
                                args)))
             (treat-as-inds
              (intersection
               (get-vois-from parser query)	       
               (append (mapcar #'first established-bindings)
                       #| (mapcar #'(lambda (x) 
                                      (get-cor-voi parser (first x)))
                                  established-bindings) |# 
                       )))
             
             (*treat-as-abox-inds* (remove-if-not #'(lambda (x) (substrate-thing-p parser x)) treat-as-inds))
             (*treat-as-substrate-inds* (remove-if #'(lambda (x) (substrate-thing-p parser x)) treat-as-inds))
             (inds nil))

        
        (labels ((process-entry (x) 
                   
                   (cond ((is-tbox-operator-projector-thing-p parser x)

                          (let ((var 
                                 (if (ind-p parser (second x))
                                     (progn
                                       (push (second x) inds)
                                       (get-var-for-ind parser (second x)))
                                   (second x))))

                            `((:lambda (,(second x) )
                                ,(ecase (to-keyword (first x))
                                   ((:concept-synonyms)
                                    `(sort-symbol-name-lessp
                                      (atomic-concept-synonyms ,(second x) *current-tbox*)))
				  
                                   ((:describe) 
                                    `(describe-concept ,(second x) *current-tbox* nil))))
			       
                              ,var)))                              
                         
                         ((voi-p parser x) 
                          
                          x)

                         ((is-lambda-abox-projector-thing-p parser x) 
                         
                          ;;; (<lambda> (x) ...) 
                         
                          (if (consp (first x))
                              `(,(first x)
                                ,@(mapcar #'process-entry (cdr x)))
                            x))
                         
                         (t (parser-error 
                             "Bad head entry ~A" x)))))             

          (let ((head (mapcar #'process-entry result-vois)))

            (apply #'call-next-method 
                   query
                   head
                   substrate
                   :parser parser
                   :additional-head-inds (remove-duplicates
                                          (append inds
                                                  *treat-as-substrate-inds*
                                                  *treat-as-abox-inds*))
                   args)))))))

;;;
;;;
;;;       

(defmethod1 reprepare-query ((query query) &rest args
                            &key (to-substrate (substrate query))
                            copy-p new-id &allow-other-keys)

  (declare (ignorable args))

  (with-critical-section

    (unless copy-p 
      (delete-query query)
      (delete-rule query))
    
    (when (substrate-needs-reset-p to-substrate)
      (reset-substrate to-substrate))

    (if (or copy-p (tbox-has-changed-since-parsing-p query))

        (let ((copy-id 
               (if new-id
                   new-id
                 (if copy-p
                     (get-copy-id query :prefix (abox (substrate query)))
                   (iterator-id query)))))
       
          (if (is-rule-p query)
              (progn
                (unless copy-p
                  (warn-tbox-has-changed))
                (let* ((orig (original-description (parser query)))
                       (rule2
                        (apply #'prepare-rule-int
                               (second orig) 
                               (first orig) 
                               to-substrate
                               :dont-check-id-p (or copy-p
                                                    (eq copy-id (iterator-id query)))
                               (cons :id 
                                     (cons copy-id 
                                           (third orig))))))
                           
                  (values (prepare-to-run-rule rule2)
                          rule2)))

            (progn
              (unless copy-p 
                (warn-tbox-has-changed))
              (let* ((orig (original-description (parser query)))
                     (query2
                      (apply #'prepare-query-int 
                             (second orig) 
                             (first orig) 
                             to-substrate
                             :subscribers (subscribers query)
                             :dont-check-id-p (or copy-p
                                                  (eq copy-id (iterator-id query)))
                             (cons :id 
                                   (cons copy-id 
                                         (third orig))))))

                (when (subscribers query)
                  ;;; sonst sind alle Deltas verloren
                  (setf (slot-value query2 'result-bindings-hash)
                        (slot-value query 'result-bindings-hash)))

                (values (prepare-to-run-query query2)
                        query2)))))
      
      (progn
        (dolist (q (cons query (all-subqueries query)))
          (dolist (slot '(bindings-queue 
                          last-queue-item 
                          get-next-tuple-p
                          result-bindings 
                          last-result-bindings-item))
            (setf (slot-value q slot) nil))

          (when (is-nrql-query-p q)
            (dolist (slot '(new-abox-assertions
                            abox-assertions-to-add
                            kb-changed-token-delivered-p))
              (setf (slot-value q slot) nil))))
       
        (register-kb-id query)

        (values (prepare-to-run-query query)
                query)))))

;;;
;;;
;;;

(defmethod reprepare-rule ((query query) &rest args)  
  (with-critical-section
    (apply #'reprepare-query query args)))


(defun process-rule-con-pattern (rule-con-pattern parser &key replace-p)
  (let ((result-vois nil)
        (new-ind-ops nil))

    (labels ((process (var x &optional no-new-ind-p cd-p embedded-p)

               (cond ((voi-p parser var)   

                      (if replace-p
                          (find-voi parser var)
                        
                        (progn
                          (pushnew var result-vois)
                          var)))
                     
                     ((and (not no-new-ind-p)
                           (new-ind-op-p parser var))

                      (if (third var)

                          (let* ((inds (mapcar #'(lambda (var )
                                                   (process var x no-new-ind-p cd-p embedded-p))
                                               (cddr var)))
                                 (var `(:new-ind ,(second var)
                                        ,@inds)))
                            
                            (pushnew var new-ind-ops :test #'equal)
                            var)
                        
                        (let* ((var
				`(:new-ind
				  ,(if (var-p parser (second var))

                                       ;;; soll so! nicht (voi-p), damit
                                       ;;; Regeln der Art (firerule () ((instance (new-ind a) xxx)))
                                       ;;; funktionieren 

                                       (process (second var) x no-new-ind-p cd-p embedded-p)
                                     (second var)))))

                          (pushnew var new-ind-ops :test #'equal)
                          var)))

                     ((is-lambda-abox-projector-thing-p parser var)
                      (if (consp (first var))
                          `(,(first var)
                            ,@(mapcar #'(lambda (var) (process var nil t t))
                                      (cdr var)))
                        var))                        

                     ((and cd-p 
                           (is-attribute-abox-projector-thing-p parser var))
                      `(,(first var) 
                        ,(process (second var) x no-new-ind-p cd-p embedded-p)))

                     ((and cd-p embedded-p ; (datatype-fillers (.... )) ?
                           (is-datatype-property-abox-projector-thing-p parser var))
                      `(,(first var) 
                        ,(process (second var) x no-new-ind-p cd-p embedded-p)))

                     ((and cd-p 
                           (is-told-value-abox-projector-thing-p parser var))
                      `(:told-value
                        ,(process (second var) x no-new-ind-p cd-p embedded-p)))

                     ((and cd-p 
                           (is-told-value-if-exists-abox-projector-thing-p parser var))
                      `(:told-value-if-exists
                        ,(process (second var) x no-new-ind-p cd-p embedded-p)))
                     
                     ((and cd-p 
                           (is-datatype-fillers-abox-projector-thing-p parser var))
                      `(:datatype-filler
                        ,(process (second var) x no-new-ind-p cd-p t)))
                  
                     ((or (numberp var) 
                          (stringp var))

                      var)

                     ((and (consp var) 
                           (not (cdddr var))
                           (member (to-keyword (first x))
                                   +racer-cd-predicates+))

                      ;; z.B. (+ (told-value (age ?x)) 30) 
                      
                      `(,(first var)
                        ,(process (second var) var no-new-ind-p cd-p embedded-p)
                        ,(process (third var) var no-new-ind-p cd-p embedded-p)))

                     (t (parser-error 
                         "Bad rule pattern entry ~A" x))))

             (process-concept (concept) 

               (cond ((or (symbolp concept) 
                          (numberp concept) 
                          (stringp concept)
			  (typep concept 'racer-boolean))

                      concept)

                     ((consp concept) 
                      
                      (cond ((new-ind-op-p parser concept)
                             (if (third concept)
                                 `(:new-symbol 
                                   ,(second concept)
                                   ,@(let* ((inds (mapcar #'(lambda (var)
                                                              (process var nil t t))
                                                          (cddr concept))))
                                       inds))
                               `(:new-symbol 
				 ,(if (voi-p parser (second concept))
				      (process (second concept) nil t t)
				    (second concept)))))
			    
                            ((is-lambda-abox-projector-thing-p parser concept)

                             (if (consp (first concept))
                                 `(,(first concept)
                                   ,@(mapcar #'(lambda (var) (process var nil t t))
                                             (cdr concept)))
                               concept))
                            
                            (t
                             (cons (process-concept (car concept))
                                   (process-concept (cdr concept))))))))

             (process-role (role) 

               (cond ((or (symbolp role) 
                          (inv-role-p  role))

                      role)

                     ((new-ind-op-p parser role)
                      (if (third role)
                          `(:new-symbol 
                            ,(second role)
                            ,@(let* ((inds (mapcar #'(lambda (var)
                                                       (process var nil t t))
                                                   (cddr role))))
                                inds))
                        `(:new-symbol 
                          ,(if (voi-p parser (second role))
                               (process (second role) nil t t)
                             (second role)))))
			    
                     ((is-lambda-abox-projector-thing-p parser role)

                      (if (consp (first role))
                          `(,(first role)
                            ,@(mapcar #'(lambda (var) (process var nil t t))
                                      (cdr role)))
                        role))

                     (t
                      
                      (parser-error 
                       "Bad rule pattern entry ~A" role)))))


      (let ((res

             (mapcar #'(lambda (x)     

                         (let ((op
                                (intern (format nil "~A" (first x)) 
                                        #-:midelora :racer
                                        #+:midelora :prover)))

                           (case (to-keyword op)
                              
                             ((:instance)
                              (let ((var (second x))
                                    (concept (third x)))
                                 
                                `(,op ,(process var x)
                                      ,(process-concept concept))))                             

                             ((:forget-concept-assertion)
                              (let ((var (second x))
                                    (concept (third x)))
                                 
                                `(,op ,(process var x t)
                                      ,(process-concept concept))))
                              
                             ((:related)
                              (let ((from (second x))
                                    (to (third x))
                                    (role (fourth x)))
				
				(cond ((is-equal-role-p role)
				       `(same-as
					 ,(process from x)
					 ,(process   to x)))
				      
				      ((is-different-from-role-p role)
				       `(different-from
					 ,(process from x)
					 ,(process   to x)))
				      
				      (t 
				       `(,op
					 ,(process from x)
					 ,(process   to x)
					 ,(process-role role))))))
                             
                             ((:same-as)
                              (let ((from (second x))
                                    (to (third x)))
                                `(,op
                                  ,(process from x)
                                  ,(process   to x))))

                             ((:different-from)
                              (let ((from (second x))
                                    (to (third x)))
                                `(,op
                                  ,(process from x)
                                  ,(process   to x))))
			     
			     ((:all-different)
                              (let ((rest (rest x)))
                                `(,op
                                  ,@(mapcar #'(lambda (voi)
						(process voi x)) 
					    rest))))

                             ((:forget-role-assertion)
                              (let ((from (second x))
                                    (to (third x))
                                    (attrib (fourth x)))
                                `(,op
                                  ,(process from x t)
                                  ,(process   to x t)
                                  ,(process-role attrib))))

                             #-:midelora 
                             ((:constrained)
                              (let ((ind (second x))
                                    (object (third x))
                                    (attrib (fourth x)))
                                `(,op
                                  ,(process ind x)
                                  ,(process object x)
                                  ,attrib)))

                             #-:midelora 
                             ((:forget-constrained-assertion)
                              (let ((ind (second x))
                                    (object (third x))
                                    (attrib (fourth x)))
                                `(,op
                                  ,(process ind x t)
                                  ,(process object x t)
                                  ,attrib)))

                             #-:midelora 
                             ((:forget-constraint)
                              x)

                             #-:midelora 
                             ((:constraints constraint)
                              `(,op
                                ,@(mapcar #'(lambda (constraint) 
                                              (list (first constraint)
                                                    (process (second constraint) constraint nil t)
                                                    (process  (third constraint) constraint nil t)))
                                          (rest x))))

                             (otherwise 

                              (if (is-lambda-abox-projector-thing-p parser x) 
                                  (if (consp (first x))
                                      `(,(first x)
                                        ,@(mapcar #'(lambda (x) 
                                                      (process x x nil t))
                                                  (cdr x)))

                                    (let ((res nil))
                                      (tree-map #'(lambda (x)
                                                    (when (var-p parser x)
                                                      (push x res)))
                                                x)

                                      (setf res (delete-duplicates res))
                                      
                                      `((:lambda ,res ,@(rest x))
                                        ,@(mapcar #'(lambda (x) 
                                                      (process x x nil t))
                                                  res))))
                                           
                                (parser-error 
                                 "Bad rule pattern entry ~A" x))))))

                     rule-con-pattern)))

        (values res result-vois new-ind-ops)))))


(defmethod1 prepare-rule-int ((query t) (rule-con-pattern list) (substrate dl-prover-substrate) &rest args
                             &key parser (parser-class (get-parser-class-for-substrate substrate)) &allow-other-keys)
  
  (with-critical-section
      
    (let* ((parser (or parser
                       (apply #'make-instance parser-class
                              :substrate substrate 
                              :allow-other-keys t 
                              args))))

      (multiple-value-bind (rule-con-pattern result-vois new-ind-ops)
          (process-rule-con-pattern rule-con-pattern parser)                                   

        (setf (slot-value parser 'original-description)
              (list rule-con-pattern query args))
	
        (unless rule-con-pattern
          (nrql-error "Parser error: no rule consequence given"))

        (apply #'prepare-query-int
               query result-vois 
               substrate
               :parser parser
               :rule-p t
               :new-ind-ops new-ind-ops
               :rule-con-pattern rule-con-pattern
               args)))))

;;;
;;;
;;;

(defmethod1 prepare-query-int ((query t) (result-vois list) (substrate substrate) &rest args
                              &key
			  
                              (parser-class (get-parser-class-for-substrate substrate))

                              parser
                          
                              (bind-specials-p t)

                              (rewrite-defined-concepts-p *rewrite-defined-concepts-p*)

                              group-by-ops 

                              &allow-other-keys)

  (when bind-specials-p
    (setf *last-query* nil))

  (let* ((parser (or parser 
                     (apply #'make-instance parser-class
                            :original-description (list result-vois query args)
                            :substrate substrate
                            :allow-other-keys t
                            args)))

         (p-vois (get-vois-from-answer-pattern result-vois parser)))

    (cond ((and group-by-ops (cdr group-by-ops))
           (parser-error "More than one group-by found: ~A" group-by-ops))


          (t 

           (let* ((query1 `(project-to ,p-vois ,query))

                  ;; die oberste Projektion entspricht genau den result-vois

                  (query2 (apply #'syntactically-rewrite-query query1 parser 
                                 :rewrite-defined-concepts-p rewrite-defined-concepts-p 
                                 args))

                  (query3 
                   (if (projection-operator-p parser query2)
                       query2
                     `(project-to ,p-vois ,query2))))

             (apply #'prepare-query-bunch
                    (get-toplevel-queries parser query3)
                    :parser parser
                    :result-vois result-vois
                    :original-query query3
                    args))))))

;;;
;;;
;;;
    

(defun1 prepare-query-bunch (query-bunch &rest args
                                        &key
			    
                                        (bind-specials-p t)
                                        
                                        original-query 

                                        rule-con-pattern
                                        new-ind-ops 

                                        premise
			    
                                        (generate-code-p *generate-code-p*)
			    
                                        (optimize-p *optimize-p*)
			    
                                        (rewrite-semantically-p *rewrite-semantically-p*)
			    
                                        (rewrite-to-dnf-p *rewrite-to-dnf-p*)
			    
                                        (report-inconsistent-queries-p *report-inconsistent-queries-p*)

                                        (report-tautological-queries-p *report-tautological-queries-p*)
			    
                                        (use-repository-p *use-repository-p*)
			    
                                        (put-into-repository-p *put-into-repository-p*)
			    
                                        id 
                                        dont-check-id-p 

                                        parser

                                        result-vois

                                        &allow-other-keys)


  (when (cdr query-bunch)
    (setf (query-hash parser)
          (make-hash-table :size 10 
                           :rehash-size 10)))

  (let* ((toplevel-query 
          (first (first (last query-bunch))))
         (queries
          (mapcar #'(lambda (oquery)

                      (let* ((oquery (first oquery))
                             (negated-p (second oquery))
                             
                             (query-name 
                              (first oquery))
			       
                             (result-vois  
                              (second oquery))
			       
                             (query (third oquery))

                             (query0 
                              (if rewrite-to-dnf-p
                                  (get-dnf-query query)
                                query))
			       
                             (query (apply #'parse-query
                                           query0 
                                           parser
					;:original-query query
                                           args))

                             ;;; es macht Sinn, die BUQ-Query-Komponennten
                             ;;; individuell semantisch umzuschreiben (bzw.
                             ;;; den Query Realizer darauf laufen zu lassen, 
                             ;;; denn die werden ja individuell als Queries
                             ;;; ablaufen!
                             ;;; das gleiche gilt fuer die Berechnung von 
                             ;;; Cache-Referenzen

                             (query 
                              (if rewrite-semantically-p
				    
                                  (apply #'semantically-rewrite-query query parser 
                                         :combine-disjunctive-atoms-p nil
                                         :combine-conjunctive-atoms-p t
                                         :reasoning-p t
                                         args)

                                query)))

                        (setf (slot-value query 'original-query)
                              query0

                              (slot-value query 'negated-reference-p)
                              negated-p)
			  
                        (when (query-hash parser)
                          (setf (gethash query-name
                                         (query-hash parser))
                                query))

                        (register-answer-pattern query result-vois)
			  
                        ;;;
                        ;;; Optimierung
                        ;;;

                        ;;; wichtig: optimize-query darf nicht nach register-answer-pattern
                        ;;; gerufen werden, da register-answer-pattern-p (result-voi-p voi) -> T setzt
                        ;;; fuer die result-vois 

                        (when optimize-p
			  
                          (unless (in-dnf-p query)
                            (nrql-error "Optimizer works only for queries in DNF, please enable DNF"))
			  
			  (apply #'optimize-query query args)
			  
                          ;;; kann ebenfalls nur fuer Queries in DNF funktionieren! 

                          (unless (partial-order query)
                            (compute-existential-variables query)))
                         
                        (mark-last-conjuncts-of-ands query)

                        ;;;
                        ;;; Code Generation
                        ;;;
			  
                        (when generate-code-p       
			    
                          ;;; 
                          ;;; dieses Flag wird von Callern gesetzt
                          ;;; wenn nicht beabsichtigt ist, dass die
                          ;;; Query auch wirklich ausgefuehrt wird
                          ;;; 

                          (setf (slot-value query 'use-repository-p) use-repository-p
                                (slot-value query 'put-into-repository-p) put-into-repository-p)

                          (when (and (not *runtime-evaluation-p*)
                                     (eq oquery toplevel-query)
                                     use-repository-p)

                            #+:racer-server (when *server-timeout* 
                                              (nrql-error "Bad *server-timeout* detected"))
			      
                            ;;;
                            ;;; wenn compiliert wird, muss 
                            ;;; ich schon hier die Cache-Referenzen
                            ;;; brechnen, da sonst der generierte 
                            ;;; Code das nicht beruecksichtigt!
                            ;;; fuer *runtime-evaluation-p* 
                            ;;; werden die Cache-Referenzen erst direkt
                            ;;; in prepare-to-run-query berechnet
                            ;;;

                            (prepare-substrate-for-query-execution 
                             (substrate query) query)
                      
                            (compute-cache-references query put-into-repository-p))

                          ;;; (setf *x* query)
			    
                          (if *runtime-evaluation-p* 
                              (setf (slot-value query 'query-fn)
                                    #'(lambda ()
                                        (when (slot-value query 'query-satisfiable)
                                          (evaluate-query *running-substrate* query))))
                            (apply #'compile-query (substrate query) query args)))

                        query))

                  query-bunch)))

    (let* ((toplevel-query (first (last queries)))

           (id 
            (cond (id

                   (if dont-check-id-p

                       id
                     
                     (cond ((if rule-con-pattern
                                (find-rule id)
                              (find-query id))
                          
                            (nrql-error "~A ~A already exists! Please use a different ID" 
                                        (if rule-con-pattern "Rule" "Query") id))

                           ((string-equal (string-upcase
                                           (subseq (symbol-name id) 0 (min 5 (length (symbol-name id)))))
                                          "QUERY")
                          
                            (nrql-error "ID prefix \"query\" is reserved for nRQL. Please use a different ID" ))

                           ((string-equal (string-upcase 
                                           (subseq (symbol-name id) 0 (min 4 (length (symbol-name id)))))
                                          "RULE")

                            (nrql-error "ID prefix \"rule\" is reserved for nRQL. Please use a different ID" ))

                           (t id))))
                  (t
                   (get-query-iterator-id toplevel-query))))

           (tq-for-reasoning 
            (when (or report-inconsistent-queries-p
                      report-tautological-queries-p)
              (apply #'parse-query
                     (get-dnf-query 
                      (apply #'syntactically-rewrite-query
                             (remove-projections 
                              parser
                              original-query)
                             parser 
                             args))
                     parser
                     args))))

      (when bind-specials-p
        (if (is-rule-p toplevel-query)
            (setf *last-rule-id* id)
          (setf *last-query-id* id)))

      (setf (slot-value toplevel-query 'iterator-id) id)
      (setf (slot-value toplevel-query 'original-query)
            original-query)


      ;;;
      ;;;
      ;;;

      (check-for-cross-products toplevel-query)


      ;;;
      ;;; Reasoning
      ;;; 
	
      (when report-inconsistent-queries-p
        (when (query-inconsistent-p tq-for-reasoning
                                    :rule-con-pattern
                                    rule-con-pattern)
	    
          (setf (slot-value toplevel-query 'query-satisfiable) nil)
          (warn-inconsistent toplevel-query)))
	
      (when (and report-tautological-queries-p 
                 (query-tautological-p tq-for-reasoning))

        (setf (slot-value toplevel-query 'query-tautological) t)
        (warn-tautological toplevel-query))

      ;;; 
      ;;; KB-Ids setzen
      ;;; Achtung: da prepare-query-int in with-critical-section ausgefuehrt wird
      ;;; (s. around-Methoden!), kann sich TBox/ABox version waehrend des
      ;;; Parsens nicht geaendert haben! 
      ;;; 
	
      ;;; im wesentlichen muss die TBox-Id gespeichert werden - 
      ;;; wenn sich z.B. eine Rollendefinition aendert, muss
      ;;; die Query bei Ausfuehrung reprepared werden
      ;;; 

      (register-kb-id toplevel-query)

      (register-answer-pattern toplevel-query result-vois)         
	
      ;;;
      ;;; Regel-Pattern / Premisse etc. registrieren
      ;;;

      (dolist (q queries) ;; wichtig!
        (when (typep q 'nrql-query)
          (setf (slot-value q 'rule-con-pattern)  nil)))
         
      (when (typep toplevel-query 'nrql-query)
        
        (setf (slot-value toplevel-query 'rule-con-pattern) 
              ;; hier werden lediglich die VOI-Symbole gegen VOI-Objekte ersetzt
              (process-rule-con-pattern rule-con-pattern parser 
                                        :replace-p t))
                                        
        (setf (slot-value toplevel-query 'new-ind-ops) new-ind-ops)
        (setf (slot-value toplevel-query 'premise) premise))

      ;;;
      ;;; Bottum-Up Evaluation Plan registrieren
      ;;;

      (when (cdr queries)
        (setf (slot-value toplevel-query 'bottom-up-evaluation-plan)
              (butlast queries)))

      (let ((count 0))
        (dolist (buq (butlast queries))
          (incf count)
          ;; f. (not (project-to ...)) -> Bindungen muessen aufbewahrt werden
          (setf (slot-value buq 'bottom-up-component-query-p) t
                (slot-value buq 'iterator-id) 
                (to-keyword (format nil "~A-ANO-BUQ-~A" id count)))))
	
      (when bind-specials-p
        (setf *last-query* 
              toplevel-query))

      toplevel-query)))

;;;
;;;
;;;      

(defmethod mark-last-conjuncts-of-ands ((query query))
  (dolist (and-query (remove-if-not #'is-and-query-p (cons query (all-subqueries query))))
    (setf (slot-value (first (last (subqueries and-query))) 'last-conjunct-p) t)))


(defmethod check-for-cross-products ((query query))
  (dolist (and-query (remove-if-not #'is-and-query-p (cons query (all-subqueries query))))
    (let ((comps nil))
      (dolist (q (all-subqueries and-query))
        (let* ((vois (set-difference
                      (all-vois q)
                      (existential-vois and-query)))
               (voi-comps
                (remove-if-not #'(lambda (comp)
                                   (find-if (lambda (voi) 
                                              (or (member voi comp)
                                                  (member (corresponding-voi voi) comp)))
                                            vois))
                               comps))
               (n (length voi-comps)))

          (when vois
            (cond ((zerop n)
                   (push vois comps))
                  (t
                   (setf comps
                         (set-difference comps  voi-comps))
                   (push (remove-duplicates 
                          (append vois (apply #'append voi-comps)))
                         comps))))))

      (when (cdr comps)
        (nrql-warning "Cross-product enumerator detected in ~A, separated variables are: ~A" 
                      (iterator-id query) 
                      comps))))

  (dolist (and-query (or (remove-if-not #'is-and-query-p (cons query (all-subqueries query)))
                         (when (is-atomic-query-p query)
                           (list query))))
    (dolist (voi (all-vois and-query))
      (let ((relevant-subqueries
             (intersection (used-by voi)
                           (all-subqueries and-query))))
        (when (and relevant-subqueries
                   (every #'(lambda (x) 
                              (is-minilisp-query-p x))
                          relevant-subqueries))
          (nrql-warning "~In ~A, the variable ~A is only used in lambda atom(s)! Lambda atom will act as enumerator for ~A - this may be very expensive" (iterator-id query) voi voi))))))

;;;
;;;
;;;             

(defmethod get-vois-from-answer-pattern ((pattern list) (parser simple-parser))
  (remove-duplicates
   (apply #'append
          (loop as voi in pattern 
                collect
                (cond ((or (is-group-by-operator-thing-p parser voi)
                           (is-aggregation-operator-thing-p parser voi))
                       (get-vois-from-answer-pattern (cdr voi) parser))
                     
                      ((is-order-by-operator-thing-p parser voi)
                       (get-vois-from-answer-pattern (cddr voi) parser))
                     
                      ((or (is-attribute-abox-projector-thing-p parser voi)
                           ;;; (is-abox-operator-projector-thing-p parser voi)
                           ;;; werden jetzt ja auf Lambdas umgeschrieben, 
                           ;;; nicht mehr erforderlich hier!
                           )
                       (list (second voi)))

                      ((or (is-told-value-abox-projector-thing-p parser voi)
                           (is-told-value-if-exists-abox-projector-thing-p parser voi)
                           (is-datatype-fillers-abox-projector-thing-p parser voi)
                           (is-annotation-datatype-fillers-abox-projector-thing-p parser voi))
               
                       (list (second (second voi))))

                      ((or (abox-thing-p parser voi)                            
                           (substrate-thing-p parser voi))

                       (list voi))

                      ((is-lambda-abox-projector-thing-p parser voi)
                       (if (consp (first voi))
                           (get-vois-from-answer-pattern (cdr voi) parser)
                         (let ((res nil))
                           (tree-map #'(lambda (x)
                                         (when (var-p parser x)
                                           (push x res)))
                                     voi)
                           res)))

                      (t 
                       (parser-error "Bad head entry ~A" voi)))))

   :test #'equal))

;;;
;;;
;;;             

(defmethod register-answer-pattern ((query query) (result-vois list))


  ;;; in bestimmten Situation (MiniLisp-Lambda-Head-Entries!)
  ;;; sind noch nicht all VOIS geparsed - hier nachholen, 
  ;;; sonst knallt's 

  (let ((res nil))
    (tree-map #'(lambda (x)
                  (when (var-p (parser query) x)
                    (push x res)))
              result-vois)
    (mapcar #'(lambda (x)
                (make-voi (parser query) x))
            res))

  ;;;
  ;;;
  ;;;

  (let ((parser (parser query))
        (vois nil)
        
        (group-by-ops nil)
        (group-by-ops-encoded nil)
        (order-by-ops nil)
        (order-by-ops-encoded nil)
        (agg-ops nil)
        (agg-ops-encoded nil)

        (result-vois-without-aggregation-ops
         (remove-if #'(lambda (x) 
                        (or (is-group-by-operator-thing-p (parser query) x)
                            (is-aggregation-operator-thing-p (parser query) x)
                            (is-order-by-operator-thing-p (parser query) x)))
                    result-vois)))

    (labels ((process-entry (voi &optional (error-p t))
               (cond 
                
                ;;;
                ;;;
                ;;;
                
                ((or (abox-thing-p parser voi)                            
                     (substrate-thing-p parser voi))
                 
                 (let ((x (find-voi parser voi)))

                   (cond (error-p

                          ;;; "normaler Modus"
                   
                          (unless x
                            (parser-error 
                             "Bad head entry ~A" voi))
                          
                          (setf (result-voi-p x) t)
                          
                          (push x vois)
                          
                          x)

                         (t ;;; "MiniLisp Code Walking" Modus

                            (when (and x (var-p parser voi))

                              ;;; Ind-Vois werden nicht automatisch 
                              ;;; erkannt im MiniLisp-Lambda 
                              
                              (setf (result-voi-p x) t)
                          
                              (push x vois))
                          
                            x))))
                
                ;;;
                ;;;
                ;;;
                
                
                ((is-group-by-operator-thing-p parser voi)
                 (push voi group-by-ops)
                 (let ((op `(,(first voi)
                             ,@(mapcar #'(lambda (x) 
                                           (process-entry x error-p))
                                       (cdr voi)))))
                   (push op group-by-ops-encoded)
                   op))

                ((is-aggregation-operator-thing-p parser voi)
                 (push voi agg-ops)
                 (let ((op `(,(first voi)
                             ,@(mapcar #'(lambda (x) 
                                           (process-entry x error-p))
                                       (cdr voi)))))
                   (push op agg-ops-encoded)
                   op))
                
                ((is-order-by-operator-thing-p parser voi)
                 (push voi order-by-ops)
                 (let ((op `(,(first voi)
                             ,(second voi)
                             ,@(mapcar #'(lambda (x) 
                                           (process-entry x error-p))
                                       (cddr voi)))))
                   (push op order-by-ops-encoded)
                   op))

                ;;;
                ;;;
                ;;;                

                #| 
                ((is-abox-operator-projector-thing-p parser voi)
                             
                 ;;; (<operator> ?x)
                             
                 (let ((x (find-voi parser (second voi))))

                   (unless x
                     (parser-error 
                      "Bad head entry ~A" voi))

                   (setf (result-voi-p x) t)         
			   
                   (push x vois)
			   
                   (list (to-keyword (first voi))
                         x)))
                |# 

                ((is-attribute-abox-projector-thing-p parser voi)

                 ;;; (<cd-attribut> ?x) 

                 (let ((x (find-voi parser (second voi))))

                   (unless x
                     (parser-error 
                      "Bad head entry ~A" voi))
			   
                   (setf (result-voi-p x) t)     

                   (push x vois)
			   
                   (list (convert-to-dl-prover-attribute-expression (substrate parser)
                                                                    (first voi))
                         x)))

                ((is-told-value-abox-projector-thing-p parser voi)

                 ;;; (told-value (<cd-attribut> ?x)) 
                             
                 (let ((x (find-voi parser (second (second voi)))))

                   (unless x
                     (parser-error 
                      "Bad head entry ~A" voi))

                   (setf (result-voi-p x) t)         
			   
                   (push x vois)
			   
                   (list :told-value
                         (list (convert-to-dl-prover-attribute-expression (substrate parser)
                                                                          (first (second voi)))
                               x))))

                ((is-told-value-if-exists-abox-projector-thing-p parser voi)

                 ;;; (told-value-if-exists (<cd-attribut> ?x)) 

                 (let ((x (find-voi parser (second (second voi)))))

                   (unless x
                     (parser-error 
                      "Bad head entry ~A" voi))

                   (setf (result-voi-p x) t)         
			   
                   (push x vois)
			   
                   (list :told-value-if-exists
                         (list (convert-to-dl-prover-attribute-expression (substrate parser)
                                                                          (first (second voi)))
                               x))))
                            
                ((is-datatype-fillers-abox-projector-thing-p parser voi)

                 ;;; (datatype-fillers (<datatype-role> ?x)) 

                 (let ((x (find-voi parser (second (second voi)))))

                   (unless x
                     (parser-error 
                      "Bad head entry ~A" voi))

                   (setf (result-voi-p x) t)         
			   
                   (push x vois)
			   
                   (list :datatype-filler
                         (list (convert-to-dl-prover-role-expression (substrate parser)
                                                                     (first (second voi)))
                               x))))

                ((is-annotation-datatype-fillers-abox-projector-thing-p parser voi)

                 ;;; (annotation-fillers (<datatype-role> ?x)) 

                 (let ((x (find-voi parser (second (second voi)))))

                   (unless x
                     (parser-error 
                      "Bad head entry ~A" voi))

                   (setf (result-voi-p x) t)         
			   
                   (push x vois)
			   
                   (list :annotation-datatype-filler
                         (list (convert-to-dl-prover-role-expression (substrate parser)
                                                                     (first (second voi)))
                               x))))

                ;;;
                ;;;
                ;;;
                
                ((is-lambda-abox-projector-thing-p parser voi)
                 (cond ((consp (first voi))
                        ;;; ((lambda (x) (+ x x)) ?x)

                        ;;; alle Vois im Body registrieren:
                        ;;; (AKtual-Parameter-Liste evtl. 
                        ;;; unvollstaendig?) 

                        (mapc #'(lambda (x) 
                                  ;;; Fehler vermeiden durch MiniLisp-Code (nil f. error-p)
                                  (process-entry x nil))
                              (first voi))

                        `(;;; im Lambda Body IMMER nur Symbole!
                          ,(first voi)
                          ;;; Aktual-Parameter registrieren: 
                          ,@(mapcar #'(lambda (x) 
                                        (process-entry x error-p))
                                    (cdr voi))))
                       
                       (t

                        ;;; (lambda (+ ?x ?x)) 

                        (mapc #'(lambda (x)
                                  ;;; Fehler vermeiden durch MiniLisp-Code (nil f. error-p)
                                  (process-entry x nil))
                              (cdr voi))

                        voi)))
                
                ;;;
                ;;;
                ;;;                
                
                (t 
			 
                 (if error-p
                     (parser-error "Bad head entry ~A" voi)
                   ;;; sonst rekursiv durchsuchen:
                   (if (listp voi)
                       (mapcar #'(lambda (x) 
                                   (process-entry x nil))
                               voi)
                     voi))))))

      (mapc #'process-entry result-vois) ; Seiteneffekte

      (setf (slot-value query 'answer-pattern)
            ;;(mapcar #'process-entry result-vois)
            ;; SExpressions OHNE voi-Objekte!
            result-vois

            (slot-value query 'answer-pattern-without-aggregation-ops)
            ;; SExpressions MIT voi-Objekten, aber nicht in MiniLisp Lambda Bodies 
            ;; Auch f. Variablen, die nur in Aggregations-Operatoren vorkommen
            (mapcar #'process-entry 
                    (append result-vois-without-aggregation-ops
                            (set-difference 
                             (reduce #'append
                                     (append (mapcar #'cdr group-by-ops)
                                             (mapcar #'cdr agg-ops)
                                             (mapcar #'cddr order-by-ops)))
                             result-vois-without-aggregation-ops)))

            (slot-value query 'answer-pattern-without-aggregation-ops-not-encoded)
            (append result-vois-without-aggregation-ops
                            (set-difference 
                             (reduce #'append
                                     (append (mapcar #'cdr group-by-ops)
                                             (mapcar #'cdr agg-ops)
                                             (mapcar #'cddr order-by-ops)))
                             result-vois-without-aggregation-ops)))

      (setf (slot-value query 'agg-ops) agg-ops
            (slot-value query 'group-by-ops) group-by-ops
            (slot-value query 'order-by-ops) order-by-ops

            (slot-value query 'result-vois)
            (sort-vois (remove-duplicates vois))))))


;;;
;;; existential Variables sollen solche sein, fuer die ich nur
;;; eine Bindungsmoeglichkeit enummerieren muss...
;;; das nur fuer Variablen, die $?x sind, und nicht in Role 
;;; Chains in einer Konjunktion auftauchen, etc.
;;; Hier ginge es: (project-to (?y) (and ($?x) (?y ?z r) (?z ?u s)))
;;; $?x, ?z, ?u sind existentiell... 
;;;

(defmethod compute-existential-variables ((query true-query))
  t)

(defmethod compute-existential-variables ((query false-query))
  t)

(defmethod compute-existential-variables ((query or-query))
  (dolist (subquery (subqueries query))
    (compute-existential-variables subquery))

  ;; after this, we need to remove some existential vois:
  ;; if some existential voi of some conjunct in the disjunction
  ;; is used negated in another disjunct, then it is not existential! 

  (let ((all (subqueries query)))
    (dolist (subquery all)
      (let ((res nil))
        (when (is-or-query-p subquery)
          (parser-error "Should be in DNF!"))
        (dolist (voi (existential-vois subquery))
          (let ((used-in-another-disjunct  
                 (remove-if-not #'(lambda (x)
                                    (some #'(lambda (y) 
                                              (and (not (eq y subquery))
                                                   (member x (all-subqueries y))))
                                          all))
                                (used-by voi))))
            (when (some #'negated-p used-in-another-disjunct)
              (push voi res))))
        ;; (pprint (list res (existential-vois subquery)))
        (dolist (subquery (cons subquery (all-subqueries subquery)))
          (setf (existential-vois subquery) 
                (set-difference (existential-vois subquery) res)))))))

(defmethod compute-existential-variables ((query atomic-query))
  (with-slots (existential-vois) query
    (case (current-role query)
      ((:enumerator 
        :from-is-bound-enumerator
        :to-is-bound-enumerator)
     
       (dolist (voi (get-all-vois query))
         (when (and (not (result-voi-p voi))
                    (=> (corresponding-voi voi)
                        (not (result-voi-p (corresponding-voi voi)))))
           (push voi existential-vois)))))))

(defmethod compute-existential-variables ((query and-query))

  (with-slots (existential-vois) query
    
    (let ((existential-subqueries nil))
      
      (dolist (subquery (subqueries query))

        ;; (pprint subquery)
      
        (when (and (is-active-p subquery)
                   (not (negated-p subquery))
                    
                   (not (is-same-as-query-p subquery))
                   (not (is-same-as-edge-retrieval-query-p subquery)))

          ;; (pprint "*")

          (case (current-role subquery)
            (:tester)
            ((:enumerator
              :from-is-bound-enumerator
              :to-is-bound-enumerator)

             (dolist (voi (get-all-vois subquery))
               (when (and (not (result-voi-p voi))
                          (=> (corresponding-voi voi)
                              (not (result-voi-p (corresponding-voi voi)))))

                 (let ((using-queries 
                        (remove-if-not 
                         #'is-active-p
                         (remove subquery
                                 (intersection 
                                  (remove nil 
                                          (append (used-by voi)
                                                  (when (corresponding-voi voi)
                                                    (used-by (corresponding-voi voi)))))
                                  (subqueries query)
                                  )))))
		   
                   (when (every #'is-top-query-p using-queries)

		     ;;; kein anderes NICHT-TRIVIALS Atom im selben And benutzt das Voi! 
		     ;;; -> existentiell
               
                     (push voi existential-vois)

                     (push voi (existential-vois subquery))

                     (push (list subquery (cons subquery using-queries))
			   existential-subqueries)))))))))

      ;;; 
      ;;; reordering required!!!
      ;;; sonst kann man die Optimierung nicht ausnutzen.
      ;;; Bsp.: (retrieve (?x) (and (?x woman) (?y woman)))
      ;;; evtl. erzeugt der Optimierer den Plan 
      ;;; ((?y woman) (?x woman))
      ;;; nun wird (?y woman) als existentiell (= nur ein Kandidate wird gesucht!)
      ;;; markiert. Da ?y nun nur auf eine Art und Weise gebunden wird,
      ;;; fehlen der entsp. Kandidate fuer (?x woman)! 
      ;;; Loesung: die existentiellen Konjunkte muessen ans Ende des Planes,
      ;;; sonst werden die "vollen enumeratoren" behindert.
      ;;; 
      ;;; ABER - dann muessen auch etwaige "TOP"-Konjunkte DAHINTER GANZ NACH HINTEN, 
      ;;; SONST WIRD STATT DES EXISTENTIELLEN ROLLEN-NACHFOLGER-ENUMERATORS DER TOP-GENERATOR
      ;;; GENOMMEN; UND DAS IST MIST HOCH DREI!
      ;;; 
      ;;; SONST: (OR (AND (?X ?Y R) (TOP ?Y)) (AND ...) ...) -> 
      ;;;        (OR (AND (TOP ?Y) (?X ?Y R)) (AND ...) ...) 
      ;;; IST SCHLECHT! VERHINDERN! -> ESQ-BLOCK
      
      (dolist (esq existential-subqueries)
	(let ((esq-block (second esq)))
	  (dolist (esq esq-block)
	    ;;; stable: 
	    (setf (slot-value query 'subqueries)
              (delete esq (slot-value query 'subqueries))))

	  (setf (slot-value query 'subqueries)
            (append (subqueries query)
                    (reverse esq-block))))))))

;;;
;;;
;;;

(defmethod get-toplevel-queries ((parser simple-parser) (query symbol))
  query)

(defmethod get-toplevel-queries ((parser simple-parser) (query list))
  (let ((queries nil)
        (counter 0))
    
    (labels ((do-it (query &optional negated-p)
               (cond ((projection-operator-p parser query)

                      (let* ((res (do-it (third query)))
                             (name (to-keyword
                                    (format nil "ANO-QUERY-~A" (incf counter))))

                             (new-query 
                              `( ,name
                                 ,(second query)
                                 ,res))
                             (ref-query 
                              `(:bindings-from
                                ,name
                                ,(second query))))

                        (push (list new-query negated-p) queries)

                        ref-query))
                     
                     ((complex-query-p parser query)
                      `(,(first query)
                        ,@(mapcar #'(lambda (x) 
                                      (do-it x negated-p))
                                  (rest query))))

                     ((not-query-p parser query)
                      `(not 
                        ,(do-it (second query) (not negated-p))))
                     
                     ((inv-query-p parser query)
                      `(inv 
                        ,(do-it (second query) negated-p)))
                     
                     (t query))))
      
      (do-it query)

      (reverse queries))))

;;;
;;;
;;;

(defmethod remove-projections ((parser simple-parser) (query symbol))
  query)

(defmethod remove-projections ((parser simple-parser) (query list))
  (labels ((do-it (query)
             (cond ((projection-operator-p parser query)
                    (do-it (third query)))
		   
                   ((complex-query-p parser query)
                    `(,(first query)
                      ,@(mapcar #'do-it (rest query))))

                   ((not-query-p parser query)
                    `(not 
                      ,(do-it (second query))))
		   
                   ((inv-query-p parser query)
                    `(inv 
                      ,(do-it (second query))))
		   
                   (t query))))
    
    (do-it query)))




