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
;;; Abstrakte Queries
;;;

(defpersistentclass query (semantic-entity dag-node)
  (
   (substrate :reader substrate :initform nil :initarg :substrate)

   (original-query :reader original-query :initarg :original-query)

   (subquery-id :accessor subquery-id :initform 0)

   ;;; Werte der correspondierenden Specials zum Zeitpunkt der Objekterzeugung 
                          
   (check-abox-consistency-p :reader check-abox-consistency-p :initform nil)
   (ensure-tbox-classification-p :reader ensure-tbox-classification-p :initform nil)

   (initial-abox-mirroring-p :reader initial-abox-mirroring-p :initform nil)
   (initial-role-assertion-mirroring-p :reader initial-role-assertion-mirroring-p :initform nil)
   (classify-concepts-in-instance-assertions-p :reader classify-concepts-in-instance-assertions-p :initform nil)

   (exclude-permutations-p :reader exclude-permutations-p :initform nil)
   (continuation-based-instance-retrieval-p :reader continuation-based-instance-retrieval-p :initform nil)

   (use-individual-synonyms-p :reader use-individual-synonyms-p :initform nil)
                    
   ;;; Parser   

   (parser :reader parser :initarg :parser)

   (active-p :reader is-active-p :initform t)
   (in-dnf-p :reader in-dnf-p :initform nil)
   (negated-p :reader negated-p :initarg :negated-p :initform nil)
   (negated-query :reader negated-query :initform nil)
   (is-optimized-p :reader is-optimized-p :initform nil)   

   (all-vois :reader all-vois :initform nil)
   (result-vois :reader result-vois :initform nil)

   (projection-vois :reader projection-vois :initform :unspecified)

   (existential-vois :accessor existential-vois :initform nil) 

   (verbose-p :reader verbose-p :initform t) 
   (dont-show-variables :reader dont-show-variables :initform nil)
   (dont-show-head-projection-operators-p :reader dont-show-head-projection-operators-p :initform nil)
   (dont-show-lambdas-p :reader dont-show-lambdas-p :initform nil)

   ;;; das sind die, fuer die nur eine Bindungsmoeglichkeit berechnet werden muss
   ;;; (also keine vollen Enumeratoren) 

   ;;; Achtung! komplexe Queries haben immer die gleichen "Variablen-Pattern!"
   ;;; (or (?X C) (?Y D)) -> (OR (AND (?X C) (?Y TOP)) (AND (?X TOP) (?Y D))) -> all-vois = (?X ?Y)

   (all-paired-vois :reader all-paired-vois :initform nil)

   (constraining-same-as-conjuncts :accessor constraining-same-as-conjuncts :initform nil)

   (subquery-of :reader subquery-of :initform nil)   
   (top-level-query :reader top-level-query :initform nil)

   ;;; Optimizer

   (score :reader score :initform -1)
   (current-role :reader current-role :initform nil)
   (partial-order :reader partial-order :initform nil :initarg :partial-order) 
   (tag-id :reader tag-id :initform nil :initarg :tag-id)

   ;;; Caching / Query Repository 

   ;;; dieses Flag wird nach erfolgreicher Query-Beantwortung gesetzt: 
   (valid-qbox-entry-p :reader valid-qbox-entry-p :initform nil)

   (put-into-repository-p :reader put-into-repository-p :initform nil)
   (use-repository-p :reader use-repository-p :initform nil)
   (cache-bindings-p :reader cache-bindings-p :initform nil)

   (superset-cache-reference :reader superset-cache-reference :initform nil)
   (subset-cache-reference :reader subset-cache-reference :initform nil)
   (exact-cache-reference :reader exact-cache-reference :initform nil)

   ;;;
   ;;; Aggregation
   ;;; 
   
   (agg-ops :accessor agg-ops :initform nil)
   (group-by-ops :accessor group-by-ops :initform nil)
   (order-by-ops :accessor order-by-ops :initform nil)
  
   ;;;
   ;;; Subscribers
   ;;;

   (subscribers :accessor subscribers :initform nil :initarg :subscribers)
   (removed-tuples :accessor removed-tuples :initform nil :initarg :removed-tuples)

   ;;;
   ;;;
   ;;;

   (time-stamp :reader time-stamp :initform nil :initarg :time-stamp :not-persistent)

   (bindings :accessor bindings :initform nil)  
   (bindings-hash :accessor bindings-hash :initform nil)  
   
   (bindings-found-p :accessor bindings-found-p :initform :unknown)

   ;;; Fuer Bottom-Up Auswertung reference-queries (Projektionsimplementierung)

   (bottom-up-evaluation-plan :reader bottom-up-evaluation-plan
                              :initform nil)

   (bottom-up-component-query-p :reader bottom-up-component-query-p
                                :initform nil)

   (negated-reference-p :reader negated-reference-p
                        :initform nil)

   ;;; Execution

   (query-fn :reader query-fn 
             :initform #'(lambda ())
             :initarg :query-fn
             :not-persistent)

   (env-setup-fn :reader env-setup-fn
                 :not-persistent)

   (source   :reader source :initform nil :initarg :source)
   (process :reader process :initform nil :initarg :process)
   (iterator-id :reader iterator-id :initform nil :initarg :iterator-id)
   
   ;;; Queries die den Zustand vom Sustrate veraendern (bzw. Racer)
   ;;; koennen erst dann ausgefuehrt werden, wenn alle anderen 
   ;;; Queries die dieses Substrate befragen terminiert sind!
   ;;; F. Racer ABox Queries: betr. :constraint-Query Atoms & negated Roles!

   (modifies-state-p :reader modifies-state-p :initform nil) 

   ;;; Tuple Construction / Bindings etc. 

   (answer-pattern :reader answer-pattern :initform nil)
   (answer-pattern-without-aggregation-ops :reader answer-pattern-without-aggregation-ops :initform nil)
   (answer-pattern-without-aggregation-ops-not-encoded :reader answer-pattern-without-aggregation-ops-not-encoded :initform nil)

   ;;;
   ;;;
   ;;;

   (counter :reader counter :initform 0)
   (how-many :reader how-many :initform nil :initarg :how-many)
   (only-new-tuples-p :reader only-new-tuples-p :initform nil :initarg :only-new-tuples-p)
   
   (special-answer :reader special-answer :initform nil)

   (result-bindings :reader result-bindings :initform nil)
   (result-bindings-hash :reader result-bindings-hash :initform nil)
   (delta-bindings-hash :reader delta-bindings-hash :initform nil)

   ;;;
   ;;;
   ;;;

   (queue-lock :reader queue-lock :initform 
	       #+:multiprocess-queries (make-lock)
	       #-:multiprocess-queries nil)

   (last-result-bindings-item :reader last-result-bindings-item :initform nil)

   (bindings-queue :reader bindings-queue :initform nil) ; f. get-next-tuple
   (last-queue-item :reader last-queue-item :initform nil)
   (current-bindings :reader current-bindings :initform nil)
   (get-next-tuple-p :reader get-next-tuple-p :initform nil)


   ;; (all-bindings :accessor all-bindings :initform nil)

   ;;;
   ;;;
   ;;;

   (record-explanations-p :reader record-explanations-p :initform nil)
   
   (explanations :reader explanations :initform nil)
   
   ;;;
   ;;;
   ;;;

   ;;; Tuple Computation Mode

   (tuple-at-a-time-p :reader tuple-at-a-time-p :initform nil)
   (proactive-tuple-computation-p :reader proactive-tuple-computation-p :initform nil)
   
   ;;; Runtime 
   
   (abort-search-p :accessor abort-search-p :initform nil)
   (timeout :reader timeout)
   (timeout-p :reader timeout-p :initform nil)
   (runtime-error :reader runtime-error :initform nil)

   ;;;

   (state-of-racer-specials :accessor state-of-racer-specials :not-persistent)

   ;;; Reasoning
    
   (query-satisfiable  :initform :not-tested :initarg :query-satisfiable)
   (query-tautological :initform :not-tested :initarg :query-tautological)
   
   (entails     :accessor entails :initform nil)
   (entailed-by :accessor entailed-by :initform nil)
   
   (equivalents :reader equivalents :initform nil)))

;;;
;;;
;;;

#+:multiprocess-queries
(defmacro with-queue-synchronization ((query) &body body)
  `(with-slots (queue-lock) ,query 
     (with-process-lock (queue-lock)
       ,@body)))

#-:multiprocess-queries
(defmacro with-queue-synchronization ((query) &body body)
  (declare (ignorable query))
  `(progn 
     ,@body))


#+(and :multiprocess-queries :lispworks)
(defmacro release-queue-lock-if-already-mine ((query) &body body)
  `(with-slots (queue-lock) ,query 
     (when (eq (mp:lock-owner queue-lock) mp:*current-process*)
       (mp:process-unlock queue-lock)
       ;; (princ "X")
       (mp:yield))
     ,@body))

#+(and :multiprocess-queries :allegro)
(defmacro release-queue-lock-if-already-mine ((query) &body body)
  `(with-slots (queue-lock) ,query 
     (when (eq (multiprocessing:process-lock-locker queue-lock) system:*current-process*)
       (mp:process-unlock queue-lock)
       ;; (princ "X")
       )
     ,@body))

#+(and :multiprocess-queries :ccl)
(defmacro release-queue-lock-if-already-mine ((query) &body body)
  `(with-slots (queue-lock) ,query 
     ;;(print (list (ccl::%%lock-owner queue-lock) ccl:*current-process*))
     ;; If the current process is not the lock owner, ccl:release-lock 
     ;; throws an error and does nothing. So we catch the error.
     (ccl:without-interrupts
      (when (eq (ccl::%%lock-owner queue-lock) ccl:*current-process*)
        (ccl:release-lock queue-lock)
        (ccl:process-allow-schedule)
                 ;;(princ "X")
        ))
     ,@body))

#+(and :multiprocess-queries (not (or :lispworks :allegro :ccl)))
(defmacro release-queue-lock-if-already-mine ((query) &body body)
  (break "Please implement release-queue-lock-if-already-mine for this platform. Thanks."))

#-:multiprocess-queries
(defmacro release-queue-lock-if-already-mine ((query) &body body)
  (declare (ignorable query))
  `(progn 
     ,@body))

;;;
;;;
;;;

(defmethod get-all-vois ((query query))
  (remove nil
          (append (all-vois query)
                  (mapcar #'corresponding-voi (all-vois query)))))


;;;
;;; n.f. DAG
;;; abstrakt!
;;; 

(defpersistentclass master-top-query (query)
  ; nur fuers Query Repository verwendet!
  ((original-query :initform 'master-top-query)
   (valid-qbox-entry-p :initform t)))


(defpersistentclass master-bottom-query (query)
  ; nur fuers Query Repository verwendet!
  ((original-query :initform 'master-bottom-query)
   (valid-qbox-entry-p :initform t)))


(defmethod in-dnf-p ((q master-top-query))
  t)

(defmethod in-dnf-p ((q master-bottom-query))
  t)

(defmethod unparse-query ((q master-top-query))
  'master-top-query)

(defmethod unparse-query ((q master-bottom-query))
  'master-bottom-query)

;;;
;;;
;;;

(defun show-source (&optional (query *last-query*))
  (when query 
    (pprint (mapcar #'source (or (subqueries query)
                                 (ensure-list query))))))

;;;
;;;
;;;

(defmethod print-object :before ((query query) stream)
  (format stream "Q: ~A " (iterator-id query))
  (when (negated-p query)
    (format stream "(NOT ")))
 
(defmethod print-object :after ((query query) stream)
  (when (negated-p query)
    (format stream ")")))

(defmethod unparse-query :around ((query query))
  ;(or (unparsed-query query)
  ;; (setf (slot-value query 'unparsed-query)
  (if (negated-p query)
      `(,(if *use-new-syntax-for-unparse-p*
             'neg
           'not)
        ,(call-next-method))
    (call-next-method)))

;;;
;;;
;;;

(defmethod make-top-level-query ((query query))
  (setf (slot-value query 'top-level-query) nil))

;;;
;;;
;;;

(defmethod mark-all-subqueries ((query query) &key self-p)
  (when self-p (mark-dag-node query))
  (mapc #'mark-dag-node (all-subqueries query)))

(defmethod unmark-all-subqueries ((query query) &key self-p)
  (when self-p (mark-dag-node query))
  (mapc #'mark-dag-node (all-subqueries query)))

;;;
;;;
;;;
;;;

(defmethod activate ((query query))
  (setf (slot-value query 'active-p) t))

(defmethod deactivate ((query query))
  (setf (slot-value query 'active-p) nil))

;;;
;;;
;;;

(defmethod activate-all-subqueries ((query query) &key self-p)
  (when self-p (activate query))
  (mapc #'activate (all-subqueries query)))

(defmethod deactivate-all-subqueries ((query query) &key self-p)
  (when self-p (deactivate query))
  (mapc #'deactivate (all-subqueries query)))

;;;
;;;
;;;

(defmethod initialize-description :after ((query query))
  (setf (slot-value query 'substrate)
        (substrate (parser query))))

;;;
;;;
;;;
            

(defmethod top-level-query-p ((query query))
  (not (top-level-query query)))

;;;
;;;
;;;

(defpersistentclass homogeneous-query (query) nil)

(defpersistentclass hybrid-query (query) nil)

(defpersistentclass atomic-query (homogeneous-query)
  ((vois :reader vois :initarg :vois :initform nil)
   (last-conjunct-p :reader last-conjunct-p :initform nil)))


;;;
;;; spezielle Queries
;;; 

(defpersistentclass true-query (atomic-query)
  ; nur fuers syntaktische Query Repository verwendet!
  ((original-query :initform 'true-query)))

(defpersistentclass false-query (query)
  ; nur fuers syntaktische Query Repository verwendet!
  ((original-query :initform 'false-query)))

(defmethod in-dnf-p ((q true-query))
  t)

(defmethod in-dnf-p ((q false-query))
  t)

(defmethod in-nnf-p ((q true-query))
  t)

(defmethod in-nnf-p ((q false-query))
  t)

(defmethod unparse-query ((q true-query))
  'true-query)

(defmethod unparse-query ((q false-query))
  'false-query)


;;;
;;;
;;;

(defmethod negate ((query query))
  (setf (slot-value query 'negated-p)
        (not (slot-value query 'negated-p)))
  query)

;;;
;;; Complex Queries
;;;    

(defpersistentclass complex-query (query)
  ((subqueries :reader subqueries :initarg :subqueries :initform nil)

   (all-subqueries :initform nil)))

(defpersistentclass homogeneous-complex-query (complex-query homogeneous-query) nil)

;;;
;;; 
;;;

(defpersistentclass and-query (complex-query) nil)

(defpersistentclass or-query (complex-query) nil)

;;;
;;;
;;; 

(defpersistentclass query-reference (atomic-query)
  ((referenced-query :reader referenced-query :initarg :referenced-query)
   (result-vois :initarg :result-vois)))

(defmethod initialize-description :after ((query query-reference))
  
  (with-slots (result-vois all-vois modifies-state-p) query
    (setf all-vois
          (result-vois (referenced-query query)))
    
    (setf result-vois
          (sort-vois (sort-vois result-vois)))
  
    (setf modifies-state-p
          (modifies-state-p (referenced-query query))))
  
  (compute-paired-vois query))

;;;
;;;
;;;

(defpersistentclass minilisp-query (atomic-query)
  ((referenced-query :reader referenced-query :initarg :referenced-query)
   (result-vois :initarg :result-vois)
   (relevant-queries :reader relevant-queries :initform nil)))

(defmethod initialize-description :after ((query minilisp-query))
  (with-slots (vois all-vois) query

    (setf all-vois (sort-vois vois))

    (dolist (voi all-vois)
      (pushnew query (used-by voi)))
    
    (compute-paired-vois query)

    (setf (dag-node-name query)
          (unparse-query query))))


;;;
;;;
;;;

(defmethod subqueries ((query true-query))
  nil)

(defmethod subqueries ((query false-query))
  nil)

(defmethod subqueries ((query atomic-query))
  nil)

;;;
;;;
;;;

(defmethod all-subqueries ((query true-query))
  nil)

(defmethod all-subqueries ((query false-query))
  nil)

(defmethod all-subqueries ((query atomic-query))
  nil)

(defmethod all-subqueries ((query query-reference))
  nil)

(defmethod all-subqueries ((query complex-query))
  (or (slot-value query 'all-subqueries)
      (setf (slot-value query 'all-subqueries)
            (append (subqueries query)
                    (reduce #'append 
                            (mapcar #'all-subqueries (subqueries query)))))))

;;;
;;;
;;;

(defmethod print-object ((query complex-query) stream)
  (let ((*print-pretty* nil))
    (format stream "(~A~{ ~A~})" 
            (etypecase query
              (or-query 'or)
              (and-query 'and))
            (subqueries query))))

(defmethod unparse-query ((query complex-query))
  `(,(etypecase query
       (or-query 
        (if *use-new-syntax-for-unparse-p*
            'union
          'or))
       (and-query 'and))
    ,@(mapcar #'unparse-query (subqueries query))))


(defmethod unparse-query ((query query-reference))
  `(:project-to 
    ,(mapcar #'textual-description (result-vois query))
    ,(unparse-query (referenced-query query))))


(defmethod print-object ((query query-reference) stream)
  (let ((*print-pretty* nil))
    (format stream "(:PROJECT-TO ~A ~A)" 
            (result-vois query)
            (referenced-query query))))


(defmethod unparse-query ((query minilisp-query))
  (if (consp (first (original-description query )))
      `((:lambda ,@(rest (first (original-description query ))))
        ,(mapcar #'textual-description (all-vois query)))
    `(:lambda ,@(rest (original-description query )))))


(defmethod print-object ((query minilisp-query) stream)
  (let ((*print-pretty* nil))
    (if (consp (first (original-description query )))
        (format stream "((:LAMBDA ~S~{ ~S~})~{ ~A~})"  
                (second (first (original-description query)))
                (cddr (first (original-description query)))
                (vois query))
      (format stream "(:LAMBDA~{ ~S~})"  
              (rest (original-description query))))))

;;;
;;; Atomic Queries sind unaere oder binary (evtl. negierte) Atome
;;;

(defpersistentclass unary-query (atomic-query)
  ((voi      :reader   voi :initform nil)))


(defmethod initialize-description :after ((query unary-query))
  (with-slots (voi all-vois vois) query 
    (setf voi (first vois))
    (setf all-vois vois)

    (compute-paired-vois query)

    (pushnew query (used-by voi))

    (setf (dag-node-name query)
          (unparse-query query))))

(defmethod print-object ((query unary-query) stream)
  (format stream "(~A ~S)" 
          (voi query)
          (original-description query)))

(defmethod unparse-query ((query unary-query))
  (list (textual-description (voi query))
        (original-description query)))

;;;
;;;
;;;

#|

(defpersistentclass bind-individual (unary-query)
  ((in-dnf-p :initform t)))

(defmethod initialize-description :after ((query bind-individual))
  (setf (slot-value query 'textual-description) 
        (textual-description (voi query)))
  (setf (dag-node-name query)
        (unparse-query query)))

(defmethod print-object ((query bind-individual) stream)
  (format stream "(BIND-INDIVIDUAL ~A)" 
          (voi query)))

(defmethod unparse-query ((query bind-individual))
  `(bind-individual ,(textual-description (voi query))))

|#

;;;
;;;
;;;

(defpersistentclass top-query (unary-query)
  (;; (query-satisfiable :initform t)
   ;; (query-tautological :initform t)
   ;; nicht setzen, sonst gibt es Meldungen, auch wenn report-... 
   ;; nicht eingeschaltet ist! wird in reasoning.lisp definiert! 
   (query-fn :initform 
             #'(lambda () 
                 (nrql-warning "This is the top query, returning NIL")
                 nil)
             :not-persistent)))

(defmethod initialize-description :after ((query top-query))
  (setf (slot-value query 'textual-description) 
        (textual-description (voi query)))
  (setf (dag-node-name query)
        (unparse-query query)))

(defmethod print-object ((query top-query) stream)
  (format stream "(TOP ~A)" 
          (voi query)))

(defmethod unparse-query ((query top-query))
  `(top ,(textual-description (voi query))))

;;;
;;;
;;;

(defpersistentclass bottom-query (unary-query)
  (;; (query-satisfiable :initform nil)
   ;; (query-tautological :initform nil)
   ;; s.o.! 
   (query-fn :initform 
             #'(lambda () 
                 (nrql-warning "This is the bottom query, returning NIL")
                 nil)
             :not-persistent)))

(defmethod initialize-description :after ((query bottom-query))
  (setf (slot-value query 'textual-description) 
        (textual-description (voi query)))
  (setf (dag-node-name query)
        (unparse-query query)))

(defmethod print-object ((query bottom-query) stream)
  (format stream "(BOTTOM ~A)" 
          (voi query)))

(defmethod unparse-query ((query bottom-query))
  `(bottom ,(textual-description (voi query))))

;;;
;;;
;;;

(defpersistentclass binary-query (atomic-query) 
  ((voi-from :reader voi-from :initform nil)
   (voi-to   :reader voi-to   :initform nil)

   (inverse-p  :reader inverse-p :initform nil :initarg :inverse-p)

   (domain-restrictions :reader domain-restrictions :initform nil)
   (range-restrictions :reader range-restrictions :initform nil)))


(defmethod initialize-description :after ((query binary-query))
  (with-slots (vois inverse-p all-vois voi-from voi-to) query

    (when inverse-p 
      (setf vois (nreverse vois)))

    (setf voi-from (first vois)
          voi-to (second vois))
    
    (pushnew query (used-by voi-from))
    (pushnew query (used-by voi-to))

    (setf all-vois (sort-vois vois))

    (compute-paired-vois query)

    (setf (dag-node-name query)
          (unparse-query query))))


(defmethod print-object ((query binary-query) stream)
  (when (inverse-p query) 
    (format stream "(INV "))
  (format stream "(~A ~A ~S)"
          (voi-from query)
          (voi-to query)          
          (original-description query))
  (when (inverse-p query)
    (format stream ")")))

(defmethod unparse-query ((query binary-query))
  (let ((q (list (textual-description (voi-from query))
                 (textual-description (voi-to query))
                 (original-description query))))
    (if (inverse-p query)
        `(inv ,q)
      q)))

;;;
;;;
;;;

(defpersistentclass same-as-query (binary-query) nil)

(defmethod initialize-description :after ((query same-as-query))
  (with-slots (voi-from voi-to) query

    (when (is-query-individual-p voi-from)
      (psetf voi-from voi-to
             voi-to voi-from))))


;;;
;;;

(defmethod unparse-query ((query same-as-query))
  `(same-as 
    ,(textual-description (voi-from query))
    ,(textual-description (voi-to query))))


(defmethod print-object ((query same-as-query) stream)
  (format stream "(SAME-AS ~A ~A)"
          (voi-from query)
          (voi-to query)))

;;;
;;;
;;;

(defmethod initialize-description :after ((query complex-query))
  (let ((all-subqueries 
         (all-subqueries query))
        (counter 0))

    (when (cdr (remove-duplicates (mapcar #'substrate all-subqueries)))
      (parser-error "Subqueries reference different substrates ~A" 
                  (list all-subqueries
                        (remove-duplicates (mapcar #'substrate all-subqueries)))))

    (dolist (subquery (subqueries query))
      (setf (subquery-id subquery) (incf counter))
      (setf (slot-value subquery 'subquery-of) query))
  
    (dolist (sq all-subqueries)
      (setf (slot-value sq 'top-level-query) query)
      (when (modifies-state-p sq)
        (setf (slot-value query 'modifies-state-p) t)))
    
    (let ((descr (unparse-query query)))
      (setf (slot-value query 'textual-description) ; nur, damit nicht unbound... 
            descr
            (slot-value query 'original-description) 
            descr)

      (setf (dag-node-name query) 
            descr))))


(defmethod initialize-description :after ((query and-query))
  (when (negated-p query)
    (parser-error "Query ~A is not in NNF" query))

  (let* ((subqueries (subqueries query))
         (saqs (remove-if-not #'is-same-as-query-p subqueries))
         (non-saqs (set-difference subqueries saqs)))

    (setf (slot-value query 'all-vois) 
          (sort-vois
           (apply #'append (mapcar #'all-vois subqueries))))

    (dolist (other non-saqs)
      (dolist (saq saqs)
        (when (member (voi-from saq) 
                      (all-vois other))
          (pushnew saq (constraining-same-as-conjuncts other))))))
  

  (compute-paired-vois query))


(defmethod initialize-description :after ((query or-query))
  (when (negated-p query)
    (parser-error "Query ~A is not in NNF" query))
  
  (let ((all-vois
         (remove-duplicates (mapcar #'all-paired-vois (subqueries query))
                            :test #'(lambda (a b) (set-equal a b :test #'equal))))

        (all-vois2
         (remove-duplicates (mapcar #'all-vois (subqueries query))
                            :test #'set-equal)))

    (when (cdr all-vois)
      (parser-error "Bad disjuncts: ~A" 
                  (mapcar #'(lambda (x) (list x (all-paired-vois x))) (subqueries query))))

    (setf (slot-value query 'all-vois) 
          (sort-vois 
           (first all-vois2)))

    (compute-paired-vois query)))

;;;
;;; Variablen und Individuen
;;;  

(defpersistentclass voi (semantic-entity)
  ((bound-to :accessor bound-to :initform nil)
   (corresponding-voi :accessor corresponding-voi :initarg :corresponding-voi :initform nil)
   
   (result-voi-p :accessor result-voi-p :initarg :result-voi-p :initform nil)

   (una-voi-p :accessor una-voi-p :initarg :una-voi-p :initform t)
 
   (used-by :accessor used-by :initform nil)

   (lexical-name :reader get-lexical-name :initform nil)

   (substrate :initarg :substrate)))

;;;
;;;
;;;

(defmethod print-object ((voi voi) stream)
  (format stream "~S" (textual-description voi)))

;;;
;;;
;;;

(defpersistentclass substrate-voi (voi) nil)

(defmethod initialize-instance :after ((voi substrate-voi) &rest initargs)
  (declare (ignorable initargs))
  (setf (slot-value voi 'lexical-name)
        (textual-description voi)))

(defpersistentclass abox-voi (voi) nil)

(defpersistentclass query-variable (voi) nil)
  
(defpersistentclass query-individual (voi)
  ((bound-to :initform t)
   (individual :reader individual)))

;;;
;;;
;;;

(defmethod get-pretty-name ((voi query-variable))
  ;;; "*" entfernen 
  (with-slots (textual-description) voi
    (let* ((string (symbol-name textual-description))
           (pos-* (position #\* string))
           (pos-$ (position #\$ string)))
      
      (to-symbol
       (if (and pos-$ (zerop pos-$))
           (if (and pos-* (= pos-* 2))
               ;;; $?*X
               (format nil "$?~A" (subseq string 3))
             ;;; $?X
             string)
         (if (and pos-* (= pos-* 1))
             ;;; ?*X
             (format nil "?~A" (subseq string 2))
           ;;; ?X
           string))))))


(defmethod get-pretty-name ((voi query-individual))
  ;;; "*" entfernen 
  (with-slots (textual-description) voi
    (let* ((string (symbol-name textual-description))
           (pos-* (position #\* string)))
      
      (if (and pos-* (= pos-* 0))
           ;;; *X
           (to-symbol
            (format nil "~A" (subseq string 1)))
        textual-description))))

;;;
;;;
;;;

(defpersistentclass substrate-variable (query-variable substrate-voi) nil)

(defmethod initialize-instance :after ((voi substrate-variable) &rest initargs &key keep-*-p &allow-other-keys)
  (declare (ignorable initargs))
  (with-slots (lexical-name textual-description) voi
    (setf lexical-name
          (if keep-*-p 
              textual-description
            (get-pretty-name voi)))))


(defpersistentclass substrate-individual (query-individual substrate-voi) nil)

(defmethod initialize-instance :after ((voi substrate-individual) &rest initargs &key substrate keep-*-p &allow-other-keys)
  (declare (ignorable initargs keep-*-p))
  (with-slots (lexical-name textual-description bound-to individual) voi
    (setf lexical-name
          (if keep-*-p 
              textual-description
            (get-pretty-name voi)))

    (let ((ind (find-node substrate lexical-name :error-p t)))
      (setf individual ind
            bound-to ind))))


(defpersistentclass abox-variable (query-variable abox-voi) nil)
 
(defmethod initialize-instance :after ((voi abox-variable) &rest initargs &key keep-*-p &allow-other-keys)
  (declare (ignorable initargs))
  (with-slots (lexical-name textual-description) voi
    (setf lexical-name
          (if keep-*-p 
              textual-description
            (get-pretty-name voi)))))


(defpersistentclass abox-individual (query-individual abox-voi) nil)

(defmethod initialize-instance :after ((voi abox-individual) &rest initargs &key substrate keep-*-p &allow-other-keys)
  (declare (ignorable initargs))
  (with-slots (lexical-name textual-description bound-to individual) voi
    (setf lexical-name
          (if keep-*-p 
              textual-description
            (get-pretty-name voi)))
    
    (let ((ind (convert-to-dl-prover-individual-name substrate
                                                     lexical-name)))
                                                     
      (setf individual ind
            bound-to ind))))


;;;
;;;
;;;

(defun sort-vois (vois)
  (remove-duplicates 
   (sort (copy-list vois)
         #'string-lessp
         :key #'(lambda (x) 
                  (get-lexical-name x)))
   :key #'get-lexical-name))

(defun compute-paired-vois (query)
  (let ((vars (all-vois query))
        (newvars nil))
    (loop while vars do
          (let ((var (first vars)))
            (push 
             (if (is-substrate-voi-p var)                          
                 (if (corresponding-voi var)
                     (list var (corresponding-voi var))
                   (list var nil))
               (if (corresponding-voi var)
                   (list (corresponding-voi var) var)
                 (list nil var)))
             newvars)
            (setf vars (remove var vars))
            (setf vars (remove (corresponding-voi var) vars))))
    (setf (slot-value query 'all-paired-vois)
          (nreverse newvars))))

;;;
;;; Aufschluesselung der atomaren Queries nach Art der Beschreibung!
;;; 

#+:dlmaps (defpersistentclass simple-description-query (atomic-query simple-description)) 

#+:dlmaps (defpersistentclass simple-conjunctive-description-query (simple-description-query simple-conjunctive-description)) 

#+:dlmaps (defpersistentclass simple-disjunctive-description-query (simple-description-query simple-disjunctive-description))

(defpersistentclass racer-description-query (atomic-query racer-description) nil)
;;; auch instance/edge-Retrieval-Queries erben hiervon!

#+:dlmaps (defpersistentclass predicate-description-query (atomic-query)
            ((predicate :reader predicate)))
  ;; (:SATISFIES <lisp-fn>) f. Substrate Objekte

;;;
;;;
;;;

#+:dlmaps
(defmethod initialize-description :after ((query predicate-description-query))
  (with-slots (predicate negated-p textual-description) query
    (setf textual-description textual-description)
    (let ((pred `(lambda (object)
                   ,(tree-map #'(lambda (x) 
                                  (if (and (symbolp x)
                                           (or (string= (symbol-name x) "OBJECT")
                                               (string= (symbol-name x) "NODE")
                                               (string= (symbol-name x) "EDGE")
                                               (string= (symbol-name x) "X")))
                                      'object
                                    x))
                              (if negated-p 
                                  `(not ,(second textual-description))
                                (second textual-description))))))
      (setf negated-p nil) 
      (setf predicate
            (if *compile-queries-p* 
                (compile nil pred)
              (eval pred))))))

;;;
;;; Aufschluesselung nach Art des Mediums: Substrate, RACER-ABox, oder "virtuelles Medium"
;;;

#+:dlmaps
(defpersistentclass substrate-query (query)) ;;; Substrate-Retrieval Queries

(defpersistentclass dl-prover-query (query))

(defpersistentclass racer-abox-query (dl-prover-query))

#+:midelora 
(defpersistentclass midelora-abox-query (dl-prover-query)
  ;;; momentan noch kein "mult-query"-Betrieb in MiDeLoRa! 
  ((modifies-state-p :initform t)

   (continuation-based-instance-retrieval-p :initform t)))


#+:dlmaps
(defpersistentclass virtual-query (query)) ;;; f. implicit Edge / (node?) Substrate Queries! 

;;;
;;; atomic Queries
;;;

#+:dlmaps
(defpersistentclass atomic-substrate-query (substrate-query atomic-query))

(defpersistentclass atomic-racer-abox-query (racer-abox-query racer-description-query atomic-query))

#+:midelora
(defpersistentclass atomic-midelora-abox-query (midelora-abox-query atomic-query))

#+:dlmaps
(defpersistentclass atomic-virtual-query (virtual-query atomic-query))

;;;
;;; Complexe Queries
;;;

#+:dlmaps
(defpersistentclass complex-substrate-query (substrate-query homogeneous-complex-query))

(defpersistentclass complex-racer-abox-query (racer-abox-query homogeneous-complex-query))

#+:midelora
(defpersistentclass complex-midelora-abox-query (midelora-abox-query homogeneous-complex-query))

;;;
;;; weiter Aufschluesseln nach Stelligkeit 
;;;


#+:dlmaps
(defpersistentclass substrate-node-query (atomic-substrate-query unary-query))

(defpersistentclass instance-retrieval-query (unary-query))

(defpersistentclass racer-instance-retrieval-query (atomic-racer-abox-query racer-node-description instance-retrieval-query)
  ((racer-concept :accessor dl-concept)))

#+:midelora
(defpersistentclass midelora-instance-retrieval-query (atomic-midelora-abox-query instance-retrieval-query) 
  ((dl-concept :accessor dl-concept)))

;;;
;;;
;;;

#+:dlmaps
(defpersistentclass substrate-edge-query (atomic-substrate-query binary-query))

(defpersistentclass edge-retrieval-query (binary-query))

(defpersistentclass racer-edge-retrieval-query (atomic-racer-abox-query racer-edge-description edge-retrieval-query)
  ((racer-role :accessor dl-role)))

#+:midelora
(defpersistentclass midelora-edge-retrieval-query (atomic-midelora-abox-query edge-retrieval-query)
  ((dl-role :accessor dl-role)))

;;;
;;;
;;;

(defpersistentclass racer-cd-edge-retrieval-query (atomic-racer-abox-query binary-query racer-description-query)
  ((racer-tbox :reader racer-tbox :initarg :racer-tbox :initform nil)
   (from-attribute :reader from-attribute)
   (to-attribute :reader to-attribute)
   (from-is-datatype-property-p :reader from-is-datatype-property-p  :initform nil)
   (to-is-datatype-property-p :reader to-is-datatype-property-p  :initform nil)
   
   (constraint :reader constraint)))

;;;
;;;
;;;

(defpersistentclass has-known-successor-retrieval-query (unary-query))

(defpersistentclass racer-has-known-successor-retrieval-query (atomic-racer-abox-query racer-edge-description has-known-successor-retrieval-query)
  ((racer-role :accessor dl-role)))


#+:midelora
(defpersistentclass midelora-has-known-successor-retrieval-query (atomic-midelora-abox-query has-known-successor-retrieval-query)
  ((dl-role :accessor dl-role)))

;;;
;;; 
;;;   


#+:dlmaps
(defpersistentclass virtual-node-query (atomic-virtual-query unary-query))

#+:dlmaps
(defpersistentclass virtual-edge-query (atomic-virtual-query binary-query))

;;;
;;; Medium = Substrate; Basisklassen fuer unterschiedliche Basis-Query-Arten
;;; Simple Queries fuers Substrate
;;;

#+:dlmaps
(defpersistentclass substrate-simple-query (substrate-query simple-description-query)) 

#+:dlmaps
(defpersistentclass substrate-predicate-query (substrate-query predicate-description-query))

#+:dlmaps
(defpersistentclass substrate-racer-query (substrate-query racer-description-query))

#+:dlmaps
(defpersistentclass virtual-simple-query (virtual-query simple-description-query)) 

;;;
;;; Achtung: die folgenden virtual-predicate-queries sind abstrakt 
;;; Der Benutzer muss Unterklassen bilden! s. z.B. epsilon-queries 
;;; in spatial-substrate
;;;
;;; Hier kann nicht die Klasse predicate-description-query als Oberklasse
;;; verwendet werden, weil die Praedikate unterschiedliche Stelligkeit haben
;;; 

#+:dlmaps
(defpersistentclass virtual-predicate-query (atomic-virtual-query)
  ;;; Achtung! hier muessen die Objekte gar nicht existieren; 
  ;;; im Falle von Kanten wird statt "(funcall predicate object)"
  ;;; "(funcall predicate from to)" aufgerufen! 
  ;;; 
  ;;; Syntax: (:user-defined-keyword s-expression)
  ;;; 
  ((predicate :reader predicate)))

#+:dlmaps
(defpersistentclass virtual-predicate-node-query (virtual-predicate-query virtual-node-query))

#+:dlmaps
(defpersistentclass virtual-predicate-edge-query (virtual-predicate-query virtual-edge-query))

;;;
;;; Beachte unterschiedliche Signaturen der Praedikate
;;;

#+:dlmaps
(defmethod initialize-description :after ((query virtual-predicate-node-query))
  (with-slots (predicate negated-p textual-description) query
    (setf textual-description textual-description)
    (let ((pred `(lambda () ; *keine* Argumente! 
                   ,(if negated-p 
                        `(not ,(second textual-description))
                      (second textual-description)))))
      (setf negated-p nil) 
      (setf predicate
            (if *compile-queries-p* 
                (compile nil pred)
              (eval pred))))))


#+:dlmaps
(defmethod initialize-description :after ((query virtual-predicate-edge-query))
  (with-slots (predicate negated-p textual-description) query
    (setf textual-description textual-description)
    (let ((pred `(lambda (a b) 
                   (declare (ignorable a b))
                   ,(tree-map #'(lambda (x) 
                                  (cond ((and (symbolp x)
                                              (or (string= (symbol-name x) "A")
                                                  (string= (symbol-name x) "FROM")
                                                  (string= (symbol-name x) "X")))
                                         'a)
                                        ((and (symbolp x)
                                              (or (string= (symbol-name x) "B")
                                                  (string= (symbol-name x) "TO")
                                                  (string= (symbol-name x) "Y")))
                                         'b)
                                        (t x)))
                              (if negated-p 
                                  `(not ,(second textual-description))
                                (second textual-description))))))
      (setf negated-p nil) 
      (setf predicate
            (if *compile-queries-p* 
                (compile nil pred)
              (eval pred))))))

;;;
;;;
;;;

#+:dlmaps
(defmethod initialize-description :before ((query substrate-simple-query))
  (with-slots (textual-description original-description) query
    
    ;;; die textual-description muss bereinigt werden, 
    ;;; weil die "simple-conjunctive/disjunctive-descriptions"
    ;;; nicht mit ":and", ":or" umgehen koennen, die ja hier
    ;;; nur fuer die Erkennung durch den Parser eingefuehrt wurden

    ;;; irgendwann wird es mal echte boolean-descriptions geben, 
    ;;; dann brauch man die keywords natuerlich wieder...

    (setf textual-description 
          (if (consp textual-description)
              (case (first textual-description)
                ((:and :or) 
                 (rest textual-description))
                (:not ; einzelnes negiertes Atom!
                 (list textual-description))
                (otherwise textual-description))
            textual-description))

    ;;;
    ;;; damit solche Queries "ungeparsed" und auch wieder "geparsed"
    ;;; werden koennen (wird oft verwendet!), muss textuell die Query
    ;;; wieder mit :and, :or annotiert werden; s. print-object verwendet
    ;;; die original-description; unparse-query geht ueber print-object
    ;;;
    
    (setf original-description 
          `( ,(etypecase query
                (simple-conjunctive-description :and)
                (simple-disjunctive-description :or))
             ,@(ensure-list textual-description)))))

;;;
;;;
;;;


(defmethod initialize-description :before ((query atomic-racer-abox-query))
  (with-slots (racer-tbox racer-package parser) query

    (setf racer-tbox (tbox (substrate parser))
          racer-package (racer-package (substrate parser)))))


#+:midelora
(defmethod initialize-description :after ((query midelora-instance-retrieval-query))
  (with-slots (dl-concept textual-description) query
    (setf dl-concept 
          (change-package-of-description 
           textual-description
           :prover
           t 
           nil))))


(defmethod initialize-description :after ((query racer-edge-retrieval-query))
  (with-slots (modifies-state-p textual-description) query
    (setf modifies-state-p
          (negated-racer-role-p textual-description))))

#+:midelora
(defmethod initialize-description :after ((query midelora-edge-retrieval-query))
  (with-slots (modifies-state-p textual-description dl-role) query
    (setf dl-role 
          (change-package-of-description 
           textual-description
           :prover
           t
           nil)
          modifies-state-p
          (negated-dl-role-p textual-description))))


(defmethod initialize-description :before ((query has-known-successor-retrieval-query))
  (with-slots (textual-description) query
    (setf textual-description 
          (second textual-description ))))

#+:midelora
(defmethod initialize-description :after ((query midelora-has-known-successor-retrieval-query))
  (with-slots (modifies-state-p textual-description dl-role) query
    (setf dl-role textual-description
          modifies-state-p
          (negated-dl-role-p textual-description))))


(defmethod initialize-description :after ((query racer-cd-edge-retrieval-query))
  (with-slots (from-attribute to-attribute constraint textual-description racer-package
                              from-is-datatype-property-p 
                              to-is-datatype-property-p) query
    (let ((attribute1 (second textual-description))
          (attribute2 (third textual-description))
          (expr (fourth textual-description)))     

      (setf constraint           
            (convert-to-racer-constraint-expression expr racer-package)

            from-attribute
            (convert-to-racer-attribute-expression attribute1 racer-package)

            to-attribute
            (convert-to-racer-attribute-expression attribute2 racer-package)

            from-is-datatype-property-p
            (is-valid-racer-datatype-role-expression-p from-attribute 
                                                       :tbox (tbox (substrate query))
                                                       :check-p t)
            to-is-datatype-property-p
            (is-valid-racer-datatype-role-expression-p to-attribute 
                                                       :tbox (tbox (substrate query))
                                                       :check-p t))
      
      #|
      (when (or (from-is-datatype-property-p query)
                (to-is-datatype-property-p query))
        (to-be-implemented 'owl-datatype-properties-in-constraint-queries))
        |# )))
                      
;;;
;;;
;;;
                  
#+:dlmaps
(defpersistentclass substrate-simple-and-query (substrate-simple-query simple-conjunctive-description-query))
  ;; (:AND ...)

#+:dlmaps
(defpersistentclass substrate-simple-or-query (substrate-simple-query simple-disjunctive-description-query))
  ;; (:OR ...) 

                  
#+:dlmaps
(defpersistentclass virtual-simple-and-query (virtual-simple-query simple-conjunctive-description-query))
  ;; (:AND ...)

#+:dlmaps
(defpersistentclass virtual-simple-or-query (virtual-simple-query simple-disjunctive-description-query))
  ;; (:OR ...) 

;;;
;;;
;;;

#+:dlmaps
(defpersistentclass substrate-simple-node-query (substrate-simple-query substrate-node-query))

#+:dlmaps
(defpersistentclass substrate-simple-and-node-query (substrate-simple-and-query substrate-simple-node-query))

#+:dlmaps
(defpersistentclass substrate-simple-or-node-query (substrate-simple-or-query substrate-simple-node-query))

;;;
;;;
;;;

#+:dlmaps
(defpersistentclass substrate-simple-edge-query (substrate-simple-query substrate-edge-query))

#+:dlmaps
(defpersistentclass substrate-simple-and-edge-query (substrate-simple-and-query substrate-simple-edge-query))

#+:dlmaps
(defpersistentclass substrate-simple-or-edge-query (substrate-simple-or-query substrate-simple-edge-query))

;;;
;;; die virtuellen sind *keine* Substrate Queries!!!!
;;;

#+:dlmaps
(defpersistentclass virtual-simple-node-query (virtual-simple-query virtual-node-query))

#+:dlmaps
(defpersistentclass virtual-simple-and-node-query (virtual-simple-and-query virtual-simple-node-query))

#+:dlmaps
(defpersistentclass virtual-simple-or-node-query (virtual-simple-or-query virtual-simple-node-query))

;;;
;;;
;;;

#+:dlmaps
(defpersistentclass virtual-simple-edge-query (virtual-simple-query virtual-edge-query))

#+:dlmaps
(defpersistentclass virtual-simple-and-edge-query (virtual-simple-and-query virtual-simple-edge-query))

#+:dlmaps
(defpersistentclass virtual-simple-or-edge-query (virtual-simple-or-query virtual-simple-edge-query))


;;;
;;; Predicate Queries sind *nicht* virtuell, sondern Substrate oder Racer-Queries
;;;


#+:dlmaps
(defpersistentclass substrate-predicate-node-query (substrate-predicate-query substrate-node-query))

#+:dlmaps
(defpersistentclass substrate-predicate-edge-query (substrate-predicate-query substrate-edge-query))

;;;
;;; 
;;; 

#+:dlmaps
(defpersistentclass substrate-racer-node-query (substrate-racer-query substrate-node-query racer-node-description))

#+:dlmaps
(defpersistentclass substrate-racer-edge-query (substrate-racer-query substrate-edge-query racer-edge-description))

;;; 
;;;
;;;

#+:dlmaps
(defmethod initialize-description :before ((query substrate-racer-node-query))
  (with-slots (original-description textual-description racer-tbox racer-package parser) query

    ;;; (:racer ) entfernen!
    
    (unless (and (consp textual-description)
                 (eq (first textual-description) :racer))
      (parser-error "Bad Racer description ~A given" textual-description))

    (setf textual-description (second textual-description)
          racer-tbox (tbox (substrate parser))
          racer-package (racer-package (substrate parser))
          original-description ;; fuer Ausgabe, reparsing etc. 
          (cons :racer (list textual-description)))))

#+:dlmaps
(defmethod initialize-description :before ((query substrate-racer-edge-query))
  (with-slots (original-description textual-description racer-tbox racer-package parser) query

    ;;; (:racer ) entfernen!
    
    (unless (and (consp textual-description)
                 (eq (first textual-description) :racer))
      (parser-error "Bad Racer description ~A given" textual-description))
    
    (setf textual-description (second textual-description)
          racer-tbox (tbox (substrate parser))
          racer-package (racer-package (substrate parser))
          original-description ;; fuer Ausgabe, reparsing etc. 
          (cons :racer (list textual-description)))))

;;;
;;; Komplexe homogene Substrat-Queries: 
;;; 
  
#+:dlmaps
(defpersistentclass complex-substrate-and-query (complex-substrate-query and-query))

#+:dlmaps
(defpersistentclass complex-substrate-or-query (complex-substrate-query or-query))

;;;
;;; Komplexe hybride Substrat-Queries: 
;;; 

#+:dlmaps
(defpersistentclass hybrid-and-query (and-query hybrid-query))

#+:dlmaps
(defpersistentclass hybrid-or-query (or-query hybrid-query))



