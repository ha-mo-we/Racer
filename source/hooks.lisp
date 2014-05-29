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

(defmethod register-bindings :before ((substrate substrate) (query query) (answer-pattern list)
                                      (new-bindings list))
  
  ;; (princ 'a)

  (with-queue-synchronization (query)

    ;; (princ 'b)

    (let ((tuple  
           (construct-result-tuple query :bindings new-bindings)))
      
      (racer:set-progress-value :tick)
    
      (when tuple
        (with-slots (;; all-bindings

                     bindings-queue
                     new-abox-assertions
                     explanations
                     record-explanations-p
                     abox-assertions-to-add
                     last-queue-item
                     tuple-at-a-time-p) query

          ;; (push tuple all-bindings)
                  
          (when (is-rule-p query) 
            (push tuple new-abox-assertions)
            (unless tuple-at-a-time-p
              (push tuple abox-assertions-to-add)))

          (when record-explanations-p
            (push *explanations* explanations))

          (if (not last-queue-item)
              (progn 
                (setf bindings-queue (list tuple))
                (setf last-queue-item (last bindings-queue)))
            (progn 
              (setf (cdr last-queue-item)
                    (list tuple))
              (setf last-queue-item
                    (cdr last-queue-item))))))))

  ;; (princ 'c)

  (wait-for-request-or-abort query)
  
  ;; (princ 'd)
  )


(defmethod register-bindings ((substrate substrate) (query query) (answer-pattern list) (new-bindings list))
  'done)

;;;
;;;
;;;

(defmethod querying-started ((substrate substrate) (query query))
  t)

(defmethod querying-started :before ((substrate substrate) (query query))
  t)

;;;
;;;
;;;

(defmethod querying-ended ((substrate substrate) (query query)))


(defmethod querying-ended :before ((substrate substrate) (query query))
  (with-access-to-lifecycle-lists 

    (unless (bottom-up-component-query-p query)
      (if (is-rule-p query) 
          (progn 
            (setf *active-rules* (delete query *active-rules*))
            (pushnew query *processed-rules*))
        (progn 
          (setf *active-queries* (delete query *active-queries*))
          (pushnew query *processed-queries*)))))
   
  ;;; etwas Platz schaffen fuer den Garbage Collector!
   
  (with-slots (bottom-up-component-query-p
               subscribers
               result-bindings-hash
               delta-bindings-hash
               removed-tuples
               process
               explanations
               record-explanations-p
               env-setup-fn) query

    (when record-explanations-p
      (setf explanations
            (nreverse explanations)))

    (when (and (not bottom-up-component-query-p)
               (not subscribers))
      (setf result-bindings-hash nil)
      (dolist (buq (bottom-up-evaluation-plan query))
        (setf (slot-value buq 'result-bindings-hash) nil)))

    (when subscribers
      (let ((removed nil))
        (maphash #'(lambda (key val)
                     (declare (ignorable val))
                     (unless (gethash key delta-bindings-hash)
                       (push key removed)))
                 result-bindings-hash)
        (dolist (tuple removed) 
          (remhash tuple result-bindings-hash)
          (push (construct-result-tuple query :bindings tuple)
                removed-tuples))))

    (setf (slot-value (parser query) 'query-hash) nil)
     
    (setf env-setup-fn nil
          process nil)))

;;;
;;;
;;;

(defmethod querying-ended :before ((substrate substrate) (query nrql-query))
  (setf (slot-value query 'phase-two-started-p) nil))

(defmethod querying-ended ((substrate racer-dummy-substrate) (query nrql-query))
  (with-slots (added-premise-axioms substrate) query

    ;;; wichtig! das muss hier stattfinden! nicht in 
    ;;; last-tuple-delivered! kompliziert! nicht aendern!

    (with-critical-section
      (when added-premise-axioms
        (forget-statement (tbox substrate) 
                          (abox substrate)
                          added-premise-axioms)
        ;;; notwendig!
        (substrate-needs-reset substrate)))))

;;;
;;;
;;;

(defmethod last-tuple-has-been-delivered ((query query))
  t)

(defmethod last-tuple-has-been-delivered ((query nrql-query))
  (with-slots (added-premise-axioms 
               dont-add-abox-duplicates-p
               add-rule-consequences-p) query

    
    (when add-rule-consequences-p
      (let ((*dont-add-abox-duplicates-p* dont-add-abox-duplicates-p))
        ;;; wichtig! bindung aus dem query process nicht mehr aktiv! 

        (add-chosen-sets-of-rule-consequences query)))
    
    (setf added-premise-axioms nil)))

;;;
;;;
;;;

(defmethod1 add-chosen-sets-of-rule-consequences ((query nrql-query) &rest args &key (dont-add-abox-duplicates-p *dont-add-abox-duplicates-p*))
  (declare (ignore args))
  (with-slots (abox-assertions-to-add substrate) query

    (dolist (pat abox-assertions-to-add)
      (let ((*dont-add-abox-duplicates-p* 
             dont-add-abox-duplicates-p))
        (add-abox-assertions substrate pat)))

    (prog1
        abox-assertions-to-add
      (setf abox-assertions-to-add nil))))


(defmethod1 choose-current-set-of-rule-consequences ((query nrql-query) &rest args)
  (declare (ignore args))
  (with-slots (current-bindings abox-assertions-to-add) query
    (unless (member current-bindings
                    '(:exhausted :timeout
                      :denied-due-to-deadlock-prevention
                      :warning-kb-has-changed 
                      :warning-expensive-phase-two-starts))
      (push current-bindings abox-assertions-to-add))))


(defmethod1 get-chosen-sets-of-rule-consequences ((query nrql-query) &rest args)
  (declare (ignore args))
  (with-slots (abox-assertions-to-add) query
    abox-assertions-to-add))

;;;
;;;
;;;

(defmethod add-abox-assertions ((substrate dl-prover-substrate) (assertions list))
  (let ((added nil))

    (dolist (assertion assertions)

      (cond ((eq assertion :undefined)
             (list :undefined))

            ((consp assertion)

             (etypecase (first assertion)

               (symbol
                 
                (ecase (to-keyword (first assertion))
                   (:progn 
                     (add-abox-assertions substrate (rest assertion)))
                 
                   (:instance 
                    (when (=> *dont-add-abox-duplicates-p*
                              (not (member (list (second assertion) (third assertion))
                                           (dl-prover-all-concept-assertions-for-individual 
                                            substrate 
                                            (second assertion))
                                           :test #'equal)))

                      (push assertion added)
                      (apply #'dl-prover-add-concept-assertion substrate (rest assertion))))

                   (:related 
                    (when (=> *dont-add-abox-duplicates-p*
                              (not (member (list (list (second assertion)
                                                       (third assertion))
                                                 (fourth assertion))
                                           (dl-prover-all-role-assertions-for-individual-in-domain
                                            substrate 
                                            (second assertion))
                                           :test #'equal)))
                      (push assertion added)
                      (apply #'dl-prover-add-role-assertion substrate (rest assertion))))

                   (:same-as 
                    (when (=> *dont-add-abox-duplicates-p*
                              (not (find (rest assertion)
                                         (dl-prover-all-same-as-assertions substrate)
                                         :key #'rest ; same-as / same-individual-as entfernen
                                         :test #'equal)))
                      (push assertion added)
                      (dl-prover-add-same-as-assertion substrate (second assertion) (third assertion))))

                   (:different-from 
                    (when (=> *dont-add-abox-duplicates-p*
                              (not (find (rest assertion)
                                         (dl-prover-all-different-from-assertions substrate)
                                         :key #'rest ; same-as / same-individual-as entfernen
                                         :test #'equal)))
                      (push assertion added)
                      (dl-prover-add-different-from-assertion 
                       substrate
                       (second assertion) (third assertion))))
		   
		   (:all-different
                    (when (=> *dont-add-abox-duplicates-p*
                              (not (find (rest assertion)
                                         (dl-prover-all-different-from-assertions substrate)
                                         :key #'rest ; same-as / same-individual-as entfernen
                                         :test #'equal)))
                      (push assertion added)
                      (dl-prover-add-all-different-assertion 
                       substrate
                       (rest assertion))))

                   (:constrained
                    (when (=> *dont-add-abox-duplicates-p*
                              (not (member (third assertion)
                                           (dl-prover-retrieve-individual-attribute-fillers
                                            substrate
                                            (second assertion)
                                            (fourth assertion))
                                           :test #'same-abox-individual-p)))
                      (push assertion added)
                      (apply #'dl-prover-add-attribute-assertion substrate (rest assertion))))

                   (:constraints
                    (dolist (constraint (rest assertion))
                      (when (=> *dont-add-abox-duplicates-p*
                                (not (member constraint (dl-prover-all-constraints substrate)
                                             :test #'equal)))
                        (push assertion added)
                        (dl-prover-add-constraint-assertion substrate constraint))))

                   ;;;
                   ;;; 
                   ;;; 
        
                   (:forget-concept-assertion
                    (dl-prover-forget-concept-assertion 
                     substrate 
                     (abox substrate) 
                     (second assertion) (third assertion)))

                   (:forget-role-assertion
                    (dl-prover-forget-role-assertion 
                     substrate 
                     (abox substrate)
                     (second assertion) (third assertion) (fourth assertion)))

                   (:forget-constrained-assertion
                    (dl-prover-forget-constrained-assertion 
                     substrate 
                     (abox substrate)
                     (second assertion) (third assertion) (fourth assertion)))
          
                   (:forget-constraint
                    (dl-prover-forget-constraint substrate 
                                                 (abox substrate)
                                                 (second assertion)))))
               

               (list  
                (ecase (to-keyword (first (first assertion)))
                
                  (:lambda 
                          (eval-expr (second (first assertion))
                                     (cons 'progn (cddr (first assertion)))
                                     (second assertion)))))))

            ((symbolp assertion)
             (let ((sym (to-keyword assertion)))
               (when (or (eq sym :ignore)
                         (eq sym +reject-tuple+))
                 t)))))

    added))

