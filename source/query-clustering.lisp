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

(defmethod and-query-available-p :around ((qa query) (qb query))
  (and (eq (type-of qa)
           (type-of qb))
       (equal (vois qa)
              (vois qb))
       (<=> (negated-p qa)
            (negated-p qb))
       (call-next-method)))

(defmethod or-query-available-p :around ((qa query) (qb query))
  (and (eq (type-of qa)
           (type-of qb))
       (equal (vois qa)
              (vois qb))
       (<=> (negated-p qa)
            (negated-p qb))       
       (call-next-method)))

;;;
;;; Around-Method needs to be relaxed for TOP-Query
;;;

(defmethod and-query-available-p :around ((a top-query) (b query))
  (and (not (is-same-as-query-p b))
       (equal (vois a) (vois b))))

(defmethod or-query-available-p :around ((a top-query) (b query))
  (and (not (is-same-as-query-p b))
       (equal (vois a) (vois b))))

(defmethod and-query-available-p :around ((a query) (b top-query))
  (and (not (is-same-as-query-p a))
       (equal (vois a) (vois b))))

(defmethod or-query-available-p :around ((a query) (b top-query))
  (and (not (is-same-as-query-p a))
       (equal (vois a) (vois b))))

;;;
;;;
;;;

(defmethod and-query-available-p :around ((a bottom-query) (b query))
  (equal (vois a) (vois b)))

(defmethod or-query-available-p :around ((a bottom-query) (b query))
  (equal (vois a) (vois b)))

(defmethod and-query-available-p :around ((a query) (b bottom-query))
  (equal (vois a) (vois b)))

(defmethod or-query-available-p :around ((a query) (b bottom-query))
  (equal (vois a) (vois b)))

;;;
;;;
;;;

(defmethod and-query-available-p ((a query) (b query))
  nil)

(defmethod or-query-available-p ((a query) (b query))
  nil)

;;;
;;;
;;;

(defmethod and-query-available-p ((a same-as-query) (b top-query))
  nil)

(defmethod and-query-available-p ((a same-as-query) (b bottom-query))
  nil)


(defmethod and-query-available-p ((b top-query) (a same-as-query))
  nil)

(defmethod and-query-available-p ((b bottom-query) (a same-as-query))
  nil)

;;;
;;;
;;;

(defmethod or-query-available-p ((a same-as-query) (b top-query))
  nil)

(defmethod or-query-available-p ((a same-as-query) (b bottom-query))
  nil)


(defmethod or-query-available-p ((b top-query) (a same-as-query))
  nil)

(defmethod or-query-available-p ((b bottom-query) (a same-as-query))
  nil)

;;;
;;;
;;;

#+:dlmaps (defmethod and-query-available-p ((a simple-conjunctive-description-query) (b simple-conjunctive-description-query))
  t)

#+:dlmaps (defmethod or-query-available-p ((a simple-disjunctive-description-query) (b simple-disjunctive-description-query))
  t)

;;;
;;; 
;;;

(defmethod and-query-available-p ((a substrate-racer-node-query) (b substrate-racer-node-query))
  t)

(defmethod or-query-available-p  ((a substrate-racer-node-query) (b substrate-racer-node-query))
  t)

(defmethod and-query-available-p ((a instance-retrieval-query) (b instance-retrieval-query))
  t)

(defmethod or-query-available-p  ((a instance-retrieval-query) (b instance-retrieval-query))
  t)

;;;
;;;
;;;

(defmethod make-and-query :before ((query query) &rest args)
  (unless (every #'(lambda (x) 
                     (<=> (negated-p x)
                          (negated-p query)))                       
                 args)
    (nrql-error "Semantic rewriting error: bad queries ~A" (cons query args))))

(defmethod make-and-query :before ((query binary-query) &rest args)
  (unless (every #'(lambda (x) 
                     (<=> (inverse-p x)
                          (inverse-p query)))                       
                 args)
    (nrql-error "Semantic rewriting error: bad queries ~A" (cons query args))))

(defmethod make-and-query ((query query) &rest args)
  (if args

      (make-and-description query
                            :negated-p (negated-p query)
                            :descriptions args
                            :vois (vois query)
                            :all-vois (all-vois query)
                            :parser (parser query))
    query))

(defmethod make-or-query :before ((query query) &rest args)
  (unless (every #'(lambda (x) 
                     (<=> (negated-p x)
                          (negated-p query)))                       
                 args)
    (nrql-error "Semantic rewriting error: bad queries ~A" (cons query args))))

(defmethod make-or-query :before ((query binary-query) &rest args)
  (unless (every #'(lambda (x) 
                     (<=> (inverse-p x)
                          (inverse-p query)))                       
                 args)
    (nrql-error "Semantic rewriting error: bad queries ~A" (cons query args))))

        
(defmethod make-or-query ((query query) &rest args)
  (if args

      (make-or-description query
                           :negated-p (negated-p query)
                           :descriptions args
                           :vois (vois query)
                           :all-vois (all-vois query)
                           :parser (parser query))

    query))

;;;
;;; 
;;; 

(defmethod make-and-description ((descr substrate-racer-node-query) &rest args &key descriptions &allow-other-keys)  
  (apply #'make-description 
         (type-of descr)
         `(:racer (and ,@(mapcar #'racer-concept (cons descr descriptions))))
         args))

(defmethod make-or-description ((descr substrate-racer-node-query) &rest args &key descriptions &allow-other-keys)  
  (apply #'make-description 
         (type-of descr)
         `(:racer (or ,@(mapcar #'racer-concept (cons descr descriptions))))
         args))

;;;
;;; 
;;; 


(defmethod note-inconsistent ((query query))
  (with-slots (query-satisfiable query-tautological) query 
    (setf query-satisfiable 
          nil
          query-tautological
          nil)))

;;;
;;; Generische Methoden: 
;;;

(defmethod semantically-rewrite-query ((query atomic-query) (parser simple-parser) 
                                       &key reasoning-p &allow-other-keys)
  (if reasoning-p
      (progn
        ;;; diese Berechnungen werden gecached!
        ;;; nur der Seiteneffekte wegen!
        ;(query-tautological-p query) 
        (query-inconsistent-p query) 
        query)
    query))


(defmethod semantically-rewrite-query ((query and-query) (parser simple-parser)
                                       &key reasoning-p (combine-conjunctive-atoms-p t) &allow-other-keys)
  (let* ((subqueries (mapcar #'(lambda (sq)
                                 (semantically-rewrite-query sq parser 
                                                             :reasoning-p 
                                                             reasoning-p))
                             (subqueries query)))
         (atomic-subqueries (remove-if #'is-complex-query-p subqueries))
         (complex-subqueries (set-difference subqueries atomic-subqueries))
         (clusters nil))

    (cond ((and reasoning-p 
                (some #'query-inconsistent-p atomic-subqueries))

           (note-inconsistent query)
           
           query)

          ((not (cdr subqueries))

           ;;; wird ja evtl. dann wieder aggregiert!
           
           (make-top-level-query (first subqueries))
           
           (first subqueries))

          (combine-conjunctive-atoms-p
           
           (loop while atomic-subqueries do
                 (let* ((qa (first atomic-subqueries))
                        (cluster
                         (cons qa
                               (remove-if-not #'(lambda (qb) 
                                                  (and-query-available-p qa qb))
                                              (rest atomic-subqueries)))))
                   (setf atomic-subqueries 
                         (set-difference atomic-subqueries cluster))
                   (push cluster clusters)))

           (when (every #'(lambda (cluster)
                            (not (cdr cluster)))
                        clusters)
             
             ;;; 
             ;;; alle Cluster singulaer -> keine (?x C) (?x D) -> (?x (and C D))-Zusammenfassungen moeglich
             ;;; 
             
             (return-from semantically-rewrite-query 
               (make-description (type-of query) 
                                 nil
                                 :subqueries subqueries
                                 :parser parser)))
           
           ;;; 
           ;;; Cluster zusammenfassen
           ;;; 

           (let ((new-atoms
                  (mapcar #'(lambda (cluster) 
                              (if (cdr cluster) 
                                  (apply #'make-and-query cluster)
                                (first cluster)))
                          clusters)))

             (when reasoning-p 
               (dolist (atom new-atoms)
                 ;(query-tautological-p atom) 
                 (query-inconsistent-p atom)))

             (cond ((and reasoning-p 
                         (some #'query-inconsistent-p new-atoms))
        
                    (note-inconsistent query)

                    query)
                   
                   (t 
                    (let ((new-subqueries (append complex-subqueries new-atoms)))
                      (if (cdr new-subqueries)              
                          (make-description (type-of query) 
                                            nil
                                            :subqueries new-subqueries
                                            :parser parser)
                        (progn 
                          (make-top-level-query (first new-subqueries))
                          (first new-subqueries))))))))
          
          (t query))))


    
(defmethod semantically-rewrite-query ((query or-query) (parser simple-parser) 
                                       &key reasoning-p (combine-disjunctive-atoms-p nil) &allow-other-keys)
  (let* ((subqueries (mapcar #'(lambda (sq) 
                                 (semantically-rewrite-query sq parser 
                                                             :reasoning-p 
                                                             reasoning-p))
                             (subqueries query)))
         
         (subqueries 
          (if reasoning-p
              (remove-if #'query-inconsistent-p subqueries)
            subqueries))

         (atomic-subqueries (remove-if #'is-complex-query-p subqueries))
         (complex-subqueries (set-difference subqueries atomic-subqueries))
         (clusters nil))

    

    (cond ((not subqueries)
        
           (note-inconsistent query) 

           query)

          ((not (cdr subqueries))
           
           (make-top-level-query (first subqueries))
           
           (first subqueries))

          (combine-disjunctive-atoms-p 

           (loop while atomic-subqueries do
                 (let* ((qa (first atomic-subqueries))
                        (cluster
                         (cons qa
                               (remove-if-not #'(lambda (qb)
                                                  (or-query-available-p qa qb))
                                              (rest atomic-subqueries)))))
                   (setf atomic-subqueries 
                         (set-difference atomic-subqueries cluster))
                   (push cluster clusters)))

           ;; (princ clusters) (terpri)

           (when (every #'(lambda (cluster)
                            (not (cdr cluster)))
                        clusters)
             
             ;;; 
             ;;; alle Cluster singulaer -> keine (?x C) (?x D) -> (?x (and C D))-Zusammenfassungen moeglich
             ;;; 
             
             ;; (princ "***")
             (return-from semantically-rewrite-query 
               (make-description (type-of query) 
                                 nil
                                 :subqueries subqueries
                                 :parser parser)))
           
           ;;; 
           ;;; Cluster zusammenfassen
           ;;; 

           (let* ((new-atoms
                   (mapcar #'(lambda (cluster) 
                               (if (cdr cluster) 
                                   (apply #'make-or-query cluster)
                                 (first cluster)))
                           clusters))
                  
                  (new-subqueries (append new-atoms complex-subqueries)))

             (if (cdr new-subqueries)
                 (make-description (type-of query) 
                                   nil
                                   :subqueries new-subqueries
                                   :parser parser)

               (progn
                 (make-top-level-query (first new-subqueries))
                 (first new-subqueries)))))

          (t  (make-description (type-of query) 
                                nil
                                :subqueries subqueries ; sind ja evtl. neu! 
                                :parser parser)))))
