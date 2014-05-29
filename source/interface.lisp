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

(defmethod wait-for-request-or-abort ((query query))
  (with-slots (process get-next-tuple-p abort-search-p) query
    (when process
      (unless (proactive-tuple-computation-p query)
        (setf get-next-tuple-p nil)
        (process-wait (or get-next-tuple-p abort-search-p))))))


(defmethod note-phase-two-starts ((query nrql-query))
  (with-slots (phase-two-started-p
               bindings-queue last-queue-item) query
    
    (setf phase-two-started-p t)

    (let ((tuple :warning-expensive-phase-two-starts))

      ;;; zu diesem Zeitpunkt ist die Queue leer! 
      ;;; darf NICHT gelockt werden, s. get-next-tuple

      (setf bindings-queue (list tuple))
      (setf last-queue-item (last bindings-queue)))))

;;;
;;;
;;;

(defmethod1 cheap-query-p ((query nrql-query))
  (and (query-active-p query)
       (not (phase-two-started-p query))))

(defmethod1 cheap-rule-p ((query nrql-query))
  (and (rule-active-p query)
       (not (phase-two-started-p query))))

;;;
;;;
;;; 

(defmethod1 expensive-query-p ((query nrql-query))
  (and (query-active-p query)
       (phase-two-started-p query)))

(defmethod1 expensive-rule-p ((query nrql-query))
  (and (query-active-p query)
       (phase-two-started-p query)))

;;;
;;;
;;;

(defun make-rule-id (id)
  (intern (format nil "~A-~A" (string-transform "rule") id) :keyword))

(defun make-query-id (id)
  (intern (format nil "~A-~A" (string-transform "query") id) :keyword))

(defmethod get-query-iterator-id ((query query))
  (if (is-rule-p query)
      (make-rule-id (incf *iterator-id*))
    (make-query-id (incf *iterator-id*))))

(defmethod get-copy-id ((query query) &key prefix)
  (intern (string-transform 
           (if prefix
               (format nil "copy-of-~A-in-~A" (iterator-id query) prefix)
             (format nil "copy-of-~A" (iterator-id query))))
          :keyword))

(defun find-query (id &optional (in *all-queries*))
  (with-access-to-lifecycle-lists
    (if (or (eq id :last)
            (eq id :last-query))
        ;;(first (remove-if #'is-rule-p in))
        (find *last-query-id* in :key #'iterator-id)
      (find id in :key #'iterator-id))))

(defun find-rule (id &optional (in *all-rules*))
  (with-access-to-lifecycle-lists
    (if (or (eq id :last)
            (eq id :last-rule))
        ;;(first (remove-if-not #'is-rule-p in))
        (find *last-rule-id* in :key #'iterator-id)
      (find id in :key #'iterator-id))))

;;;
;;;
;;;

(defmethod1 delete-query ((query query))
  (if (is-rule-p query) 
      :use-delete-rule
    (with-access-to-lifecycle-lists
      (setf *all-queries*
            (delete query *all-queries*)
            *active-queries*
            (delete query *active-queries*)
            *processed-queries*
            (delete query *processed-queries*)
            *ready-queries*
            (delete query *ready-queries*))
        
      (dolist (query (cons query 
                           (all-subqueries query)))
        (unregister-query query))

      :okay-query-deleted)))

(defmethod1 delete-rule ((query query))
  (if (not (is-rule-p query))
      :use-delete-query
    (with-access-to-lifecycle-lists
      (setf *all-rules*
            (delete query *all-rules*)
            *active-rules*
            (delete query *active-rules*)
            *processed-rules*
            (delete query *processed-rules*)
            *ready-rules*
            (delete query *ready-rules*))

      (dolist (query (cons query 
                           (all-subqueries query)))
        (unregister-query query))

      :okay-rule-deleted)))

;;;
;;;
;;;

(defmethod1 query-accurate-p ((query query))
  (and (not (abox-has-changed-since-parsing-p query))
       (not (tbox-has-changed-since-parsing-p query))))

;;;
;;;
;;;

(defmethod1 query-waiting-p ((query query))
  (and (not (proactive-tuple-computation-p query))
       (process query)
       (next-tuple-available-p query)))

(defmethod1 rule-waiting-p ((query query))
  (query-waiting-p query))

;;;
;;;
;;;

(defmethod1 query-active-p ((query query))
  (when (process query)
    t))

(defmethod1 rule-active-p ((query query))
  (when (query-active-p query)
    t))

;;;
;;;
;;;

(defmethod1 query-running-p ((query query))
  (and (query-active-p query)
       (not (query-waiting-p query))))

(defmethod1 rule-running-p ((query query))
  (query-running-p query))

;;;
;;;
;;;

(defmethod1 query-ready-p ((query query))
  (with-access-to-lifecycle-lists
    (when (find query *ready-queries*)
      t)))

(defmethod1 rule-ready-p ((query query))
  (with-access-to-lifecycle-lists
    (when (find query *ready-rules*)
      t)))

;;;
;;;
;;;

(defmethod1 get-next-n-remaining-tuples ((query query) &optional n &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (let ((res nil)
        (count 0))
    (loop 
     (let ((tuple (get-next-tuple query)))
       (if (or (eq tuple :exhausted)                 
               (eq tuple :warning-expensive-phase-two-starts)
               (not tuple))
           (return-from get-next-n-remaining-tuples 
             (reverse res))
         (if (eq tuple :timeout)
             (progn 
               (push tuple res) 
               (return-from get-next-n-remaining-tuples 
                 (reverse res)))
           (progn 
             (incf count)
             (push tuple res)
             (when (and n (= count n))
               (return-from get-next-n-remaining-tuples (reverse res))))))))))

;;;
;;;
;;;

(defmethod1 get-all-remaining-tuples ((query query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (get-next-n-remaining-tuples query))

(defmethod1 get-all-remaining-tuples ((query nrql-query) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (with-slots (deliver-phase-two-warning-tokens-p) query
    (let ((old deliver-phase-two-warning-tokens-p))
      (setf deliver-phase-two-warning-tokens-p nil)
      (prog1
          (get-next-n-remaining-tuples query)
        (setf deliver-phase-two-warning-tokens-p old)))))

;;;
;;;
;;;



(defun project-tuples (tuples head-entries)
  (if head-entries
      (mapcar #'(lambda (tuple)
                  (if (consp head-entries)
                      (if (cdr head-entries)
                          (mapcar #'(lambda (head)
                                      (nth head tuple))
                                  head-entries)
                        (nth (first head-entries) tuple))
                    (nth head-entries tuple)))
              tuples)
    tuples))

(defun group-by (rel vec)
  
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (tuple rel)
      (let ((key
             (mapcar #'(lambda (pos)
                         (nth pos tuple))
                     vec)))
        (push tuple (gethash key hash))))
    
    (let ((res nil))

      (maphash #'(lambda (key val)
                   (push (list key val) res))
               hash)

      res)))


;;;
;;;
;;;


(defmethod1 get-answer ((query query) 
                       &rest args &key dont-show-variables &allow-other-keys)
  
  (let* ((dont-show-variables
	  (remove nil 
		  (mapcar #'(lambda (x) (find-voi (parser query) x)) 
			  (or dont-show-variables
			      (dont-show-variables query)))))
         (answer-pattern (answer-pattern query))
         (answer-pattern-without-aggregation-ops (answer-pattern-without-aggregation-ops query))
         (answer-pattern-without-aggregation-ops-not-encoded (answer-pattern-without-aggregation-ops-not-encoded query))
         (process (slot-value query 'process))
         (agg-ops (agg-ops query))
         (group-by-vars (rest (first (group-by-ops query))))
         (order-by-ops (order-by-ops query)))

    (declare (ignorable order-by-ops))
    
    (labels ((distinct (tuples) 
               (remove-duplicates tuples :test #'equal)))

      (if (not (slot-value query 'query-satisfiable))

          (progn 

            ;;; wichtig: 
            (when process
              (process-wait 
               (not (slot-value query 'process))))

            :inconsistent)

        (progn
          (get-all-remaining-tuples query)

          (or (special-answer query)
      
              (if (is-rule-p query) 

                  (if (timeout-p query) 
                      (cons :timeout 
                            (new-abox-assertions query))

                    (new-abox-assertions query))

                (if answer-pattern 
                    (let* ((base-tuples 
                            (remove-if #'(lambda (x) 
                                           (member +reject-tuple+ x))
                                       (result-bindings query)))
                         
                           (tuples 
                            (if (not agg-ops)
                                base-tuples
                              (let* ((res
                                      (mapcar #'(lambda (bindings)
                                                  (get-binding-list-for-pattern 
                                                   query 
                                                   answer-pattern-without-aggregation-ops
                                                   :allow-other-keys t
                                                   :bindings bindings
                                                   :verbose-p nil))
                                              base-tuples))
                                    
                                     (res
                                      (if group-by-vars
                                          (group-by res 
                                                    (mapcar #'(lambda (var)
                                                                (position 
                                                                 var 
                                                                 answer-pattern-without-aggregation-ops-not-encoded
                                                                 :test #'equal))
                                                            group-by-vars))
                                        (list (list nil res)))))
                              
                                (mapcar #'(lambda (group-and-tuples) 
                                
                                            (let ((tuples (second group-and-tuples))
                                                  (group (first group-and-tuples)))

                                            ;(pprint group-and-tuples)

                                              (declare (ignorable group))

                                              (mapcar #'(lambda (head-entry)

                                                          (cond ((consp head-entry)

                                                                 (let ((op (first head-entry)))
                                                         
                                                                   (cond ((eq op :group-by) :group-by)

                                                                         ((member op +known-aggregation-ops+)
                                                                                       
                                                                          (let* ((tuples
                                                                                  (project-tuples 
                                                                                   tuples 
                                                                                   (mapcar 
                                                                                    #'(lambda (head-entry)
                                                                                        (position 
                                                                                         head-entry 
                                                                                         answer-pattern-without-aggregation-ops-not-encoded
                                                                                         :test #'equal))
                                                                                    (if (eq op 'minilisp)
                                                                                        (cddr head-entry)
                                                                                      (cdr head-entry)))))
                                                                                 (res
                                                                                  (ecase op 
                                                                                    (:count 
                                                                                     (length tuples))
                                                                                    (:count-distinct
                                                                                     (length (distinct tuples)))
                                                                                    (:max 
                                                                                     (reduce #'max tuples))
                                                                                    (:min 
                                                                                     (reduce #'min tuples))
                                                                                    (:sum 
                                                                                     (reduce #'+ tuples))
                                                                                    (:sum-distinct
                                                                                     (reduce #'+ (distinct tuples)))
                                                                                    (:substract 
                                                                                     (reduce #'- tuples))
                                                                                    (:substract-distinct
                                                                                     (reduce #'- (distinct tuples)))
                                                                                    (:prod 
                                                                                     (reduce #'* tuples))
                                                                                    (:prod-distinct
                                                                                     (reduce #'* (distinct tuples)))
                                                                                    (:div 
                                                                                     (reduce #'/ tuples))
                                                                                    (:div-distinct 
                                                                                     (reduce #'/ (distinct tuples)))
                                                                                    (:avg 
                                                                                     (float 
                                                                                      (/ (reduce #'+ tuples)
                                                                                         (length tuples))))
                                                                                    (:minilisp 
                                                                                     (let ((*package* (find-package :racer-user)))
                                                                                       ;;; MiniLisp funktioniert nur in racer-user!
                                                                                       (eval-expr nil
                                                                                                  `(,(second head-entry)
                                                                                                    (quote ,tuples))
                                                                                                  nil))))))

                                                                            res))
                                                                               
                                                                         (t 
                                                                          (nrql-runtime-error "Sorry, cannot select ~A from group ~A"
                                                                                              head-entry group-by-vars)))))
                                                                (t 
                                                                 (let ((pos 
                                                                        (or (position 
                                                                             head-entry 
                                                                             group-by-vars 
                                                                             :test #'equal)
                                                                            (nrql-runtime-error "Sorry, cannot select ~A from group ~A"
                                                                                                head-entry group-by-vars))))

                                                                   (nth pos group)))))
                                                                     
                                                      answer-pattern)))
                                        res))))
                         
                           (tuples
                            (mapcar #'(lambda (bindings)
                                        (apply #'get-binding-list-for-pattern query answer-pattern
                                               :dont-show-variables dont-show-variables
                                               :bindings bindings
                                               :verbose-p t 
                                               :allow-other-keys t
                                               args))
                                    tuples)))
                           
                      (if (timeout-p query)
                          (cons :timeout tuples)
                        tuples))

                  (or (eq (bindings-found-p query) t)
                      (when (timeout-p query)
                        :timeout))))))))))

;;;
;;;
;;;

(defmethod1 abort-query ((query query))
  (setf (abort-search-p query) t) 

  (when (process query)
    (abort-process (process query)))
  
  ;; wird evtl. doppelt aufgerufen 
  (invalidate-cached-bindings query)
  
  (with-access-to-lifecycle-lists
    (setf *active-queries*
          (delete query *active-queries*))
    (setf *ready-queries*
          (delete query *ready-queries*))
    (push query *processed-queries*))
   
  :okay-query-aborted)

(defmethod1 abort-rule ((query query))
  (when (is-rule-p query)
    (setf (abort-search-p query) t)
    
    (when (process query)
      (abort-process (process query))
      (invalidate-cached-bindings query))

    (with-access-to-lifecycle-lists
      (setf *active-rules*
            (delete query *active-rules*))
      (setf *ready-rules*
            (delete query *ready-rules*)))

    :okay-rule-aborted))

;;;
;;; 
;;;


(defmethod1 describe-query-status ((query query))
  (remove nil
          (cond ((query-ready-p query)
                 (list :ready-to-run))
                ((query-running-p query)
                 (list :running))
                ((query-waiting-p query)
                 (list :waiting-for-get-next-tuple))
                ((query-processed-p query)
                 `(:processed 
                   ,@(cond ((runtime-error query)
                            '(:runtime-error))
                           ((abort-search-p query) 
                            '(:aborted))))))))

(defmethod1 describe-query-status ((query nrql-query))
  (remove nil
          (cons (if (query-accurate-p query)
                    :accurate
                  :not-accurate)
                (cond ((query-ready-p query)
                       (list :ready-to-run))
                      ((query-running-p query)
                       (list :running
                             (when (two-phase-processing-p query)
                               (if (phase-two-started-p query)
                                   :phase-two
                                 :phase-one))))
                      ((query-waiting-p query)
                       (list :waiting-for-get-next-tuple
                             (when (two-phase-processing-p query)
                               (if (phase-two-started-p query)
                                   :phase-two
                                 :phase-one))))
                      ((query-processed-p query)
                       `(:processed 
                         ,@(cond ((runtime-error query)
                                  '(:runtime-error))
                                 ((abort-search-p query) 
                                  '(:aborted)))))))))

;;;
;;;
;;;

(defmethod1 describe-rule-status ((query nrql-query))
  (remove nil
          (cons (if (rule-accurate-p query)
                    :accurate
                  :not-accurate)
                (cond ((rule-ready-p query)
                       (list :ready-to-run))
                      ((rule-running-p query)
                       (list :running
                             (when (two-phase-processing-p query)
                               (if (phase-two-started-p query)
                                   :phase-two
                                 :phase-one))))
                      ((rule-waiting-p query)
                       (list :waiting-for-get-next-tuple
                             (when (two-phase-processing-p query)
                               (if (phase-two-started-p query)
                                   :phase-two
                                 :phase-one))))
                      ((rule-processed-p query)
                       (list :processed))))))

;;;
;;;
;;;

(defmethod1 describe-query ((query query) &optional rewritten-p)
  (let* ((substrate (substrate query))
         (tbox-query-p (is-racer-tbox-mirror-substrate-p substrate)))
    
    `(,(iterator-id query)
      ,(describe-query-status query)
      (,(if tbox-query-p
            'tbox-retrieve 
          'retrieve)
      
       ,(if (not rewritten-p)
            (original-query-head query)
          (query-head query))

       ,(if (not rewritten-p)
            (original-query-body query)
          (query-body query))

       ,@(when tbox-query-p 
           (list :tbox
                 (mirror-of-tbox substrate)))
                     
       ,@(unless tbox-query-p
           (list :abox
                 (abox substrate)))))))

(defmethod1 describe-rule ((query query) &optional rewritten-p)
  `(,(iterator-id query)
    ,(describe-rule-status query)
    ,(list 'prepare-abox-rule
           (if (not rewritten-p)
               (original-rule-antecedence query)
             (rule-antecedence query))
           (if (not rewritten-p)
               (first (original-description (parser query)))
             (rule-con-pattern query))
           :abox (abox (substrate query)))))


(defmethod1 rule-applicable-p ((query query) &rest args)  
  (unless (rule-active-p query)
    (multiple-value-bind (status query)
        (apply #'reprepare-query query args)
      (declare (ignorable status))
      (prog1
          (let ((*add-rule-consequences-p* nil)
                (*proactive-tuple-computation-p* t)
                (*tuple-at-a-time-p* nil)
                (*how-many* 1)
                (*two-phase-processing-p* nil))
	    
            (setf (slot-value query 'modifies-state-p) nil)

            (apply #'execute-query query :how-many 1 args)

            (setf (slot-value query 'modifies-state-p) t)
            ;;; WICHTIG! NICHT EXECUTE-RULE!!!
            ;;; umschaltung auf proactive wichtig hier!
            (bindings-found-p query))
        (apply #'reprepare-query query args)))))

;;;
;;;
;;;       

#|

(defmethod1 answer-query-int :around ((substrate substrate) 
                                     (body t) (head list)
                                     &rest args
                                     &key dont-show-aggregation-operators-p
                                     (verbose-p t)
                                     &allow-other-keys)

  ;;; Neu: Sonderbehandlung fuer Aggregations-Operatoren... etwas unschoen!

  (let ((group-by (head-group-by-ops head))
        (agg-ops (head-aggregation-ops head)))

    (cond ((and group-by (cdr group-by))
           (nrql-error "More than one group-by found: ~A" group-by))

          ((and agg-ops (not group-by))
           (nrql-error "Aggregation operator without group-by found: ~A" agg-ops))

          ((and (not agg-ops) (not group-by))
           (call-next-method))

          (t 

           (when (or (second (member :tuple-at-a-time-p args))
                     (member :tuple-at-a-time-mode (describe-query-processing-mode)))
             (nrql-error "Sorry, projection and group-by require set-at-a-time mode"))

           (let* ((head1 (head-without-group-by-etc head))
                  
                  (group-by-vars (rest (first group-by)))

                  (head-ops (head-aggregation-ops head))

                  (op-vars (reduce #'append (mapcar #'cdr head-ops)))
          
                  (head1 (remove-duplicates (append head1 group-by-vars op-vars))))

             (let* ((ans
                     (apply #'call-next-method
                            substrate
                            body 
                            head1 
                            ;;; damit Zuordnung moeglich ist
                            :verbose-p t
                            :dont-show-variables nil
                            :dont-show-head-projection-operators-p nil
                            :dont-show-lambdas-p nil
                            args))
                    
                    (ans
                     (group-by ans 
                               (mapcar #'(lambda (var)
                                           (position var head1))
                                       group-by-vars)))
                  
                    (ans
                     (mapcar #'(lambda (group-and-tuples) 
                                
                                 (let ((tuples (second group-and-tuples))
                                       (group (first group-and-tuples)))

                                   (declare (ignorable group))

                                   (remove :group-by 
                                           (mapcar #'(lambda (head-entry)

                                                       (cond ((consp head-entry)
                                                              (let ((op (first head-entry)))
                                                                (cond ((eq op 'group-by)
                                                                       :group-by)
                                                                      ((member op +known-aggregation-ops+)
                                                                       (let* ((tuples
                                                                               (project-tuples tuples 
                                                                                               (if (eq op 'minilisp)
                                                                                                   (cddr head-entry)
                                                                                                 (cdr head-entry))))
                                                                              (res
                                                                               (ecase op 
                                                                                 (count 
                                                                                  (length tuples))
                                                                                 (sum 
                                                                                  (reduce #'+ tuples))
                                                                                 (substract 
                                                                                  (reduce #'- tuples))
                                                                                 (prod 
                                                                                  (reduce #'* tuples))
                                                                                 (div 
                                                                                  (reduce #'/ tuples))
                                                                                 (avg 
                                                                                  (float 
                                                                                   (/ (reduce #'+ tuples)
                                                                                      (length tuples))))
                                                                                 (minilisp 
                                                                                  (let ((*package* (find-package :racer-user)))
                                                                                    ;;; MiniLisp funktioniert nur in racer-user!
                                                                                    (eval-expr nil
                                                                                               `(,(second head-entry)
                                                                                                 (quote ,tuples))
                                                                                               nil))))))
                                                                     
                                                                         (if (and verbose-p
                                                                                  (not dont-show-aggregation-operators-p))
                                                                             (list head-entry res)
                                                                           res))))))

                                                             (t
                                                                  
                                                              (apply #'get-result-entry 
                                                                     *last-query*
                                                                     head-entry 
                                                                     (second 
                                                                      (assoc head-entry 
                                                                             group 
                                                                             :test #'equal))
                                                                     :verbose-p verbose-p
                                                                     args))))
                                                   
                                                   head))))
                             ans)))

               (with-slots (kb-id original-description parser) *last-query*
                 (setf kb-id '(0)
                       ;; damit neu geparsed wird! notwendig, weil "Hack"
                       ;; funktioniert noch nicht, da 
                       original-description (list head body))
                 (with-slots (original-description) parser
                   (setf original-description (list head body))))
                    
               ans))))))


|#  


(defmethod1 answer-query-int ((substrate substrate) 
                             (query t) (result-vois t)
                             &rest args
                             &key &allow-other-keys)
  (declare (ignorable args))
  (parser-error (format nil "Bad query head: ~A" result-vois)))



(defmethod1 answer-query-int ((substrate substrate) 
                             (query t) (result-vois list)
                             &rest args
                             &key 
                             (execute-p t) 
                             ;(substrate *cur-substrate*) 
                             &allow-other-keys)
  
  (unless substrate
    (nrql-runtime-error "No substrate given"))

  (let ((query2 nil))

    (with-timeout-cleanup
     (setf query2
           (apply #'prepare-query-int query result-vois substrate args))
     
     (progn 
       ;;; hier muss tatsaechlich nicht mehr gemacht
       ;;; werden, habe ich analysiert! 
       (return-from answer-query-int :timeout)))

    (when query2

      (if execute-p 
          (progn
            ;;; wird in without-timeout ausgefuehrt, 
            (prepare-to-run-query query2)
            
            ;;; timeout aktiv! 
            (apply #'eval-nrql-settings2 
                   #'(lambda ()
                       (if (is-rule-p query2)
                           (apply #'execute-rule query2 args)
                         (apply #'execute-query query2 args)))
                   :allow-other-keys t
                   args))

        ;;; wird in without-timeout ausgefuehrt
        (prepare-to-run-query query2)))))


(defmethod1 apply-rule-int ((substrate substrate)
                           (query t) (rule-con-pattern list)
                           &rest args
                           &key (execute-p t)
                           &allow-other-keys)
  
  (unless substrate
    (nrql-runtime-error "No substrate given"))

  (let ((query2 nil))

    (with-timeout-cleanup
     (setf query2
           (apply #'prepare-rule-int query rule-con-pattern substrate args))
     
     (progn 
       (return-from apply-rule-int :timeout)))
    
    (when query2 

      (if execute-p
          (progn 
            (prepare-to-run-rule query2)
            (apply #'eval-nrql-settings2
                   #'(lambda () 
                       (apply #'execute-rule query2 args))
                   :allow-other-keys t
                   args))
      
        (prepare-to-run-rule query2)))))

;;;
;;;
;;;

(defun copy-rules1 (from-abox to-abox 
                              &rest args
                              &key
                              (type-of-substrate
                               *type-of-substrate*)
                              (keep-old-names-p nil))

  (let* ((from-tbox (associated-tbox from-abox))
         (from-substrate (find-racer-substrate from-abox type-of-substrate from-abox from-tbox))
         (old-substrate *cur-substrate*))
         
    (apply #'racer-prepare-substrate 
           :abox to-abox
           args)
     
    (with-access-to-lifecycle-lists
      (dolist (rule *all-rules*)
        (when (eq (substrate rule) from-substrate)
          (reprepare-rule rule
                          :to-substrate *cur-substrate* 
                          :copy-p t
                          :new-id (when keep-old-names-p 
                                    (iterator-id rule))))))

    (setf *cur-substrate* old-substrate)

    (describe-all-rules to-abox)))


;;;
;;;
;;;

(defun move-rules1 (from-abox to-abox 
                              &rest args
                              &key
                              (type-of-substrate
                               *type-of-substrate*))

  (let* ((from-tbox (associated-tbox from-abox))
         (from-substrate (find-racer-substrate from-abox type-of-substrate from-abox from-tbox))
         (old-substrate *cur-substrate*))
         
    (apply #'racer-prepare-substrate 
           :abox to-abox
           args)

    (let ((to-substrate 
           (ts::find-racer-substrate to-abox type-of-substrate)))
      (with-access-to-lifecycle-lists
        (dolist (rule *all-rules*)
          (when (eq (substrate rule) from-substrate)
            (setf (slot-value rule 'substrate) to-substrate)))))

    (setf *cur-substrate* old-substrate)

    (describe-all-rules to-abox)))

