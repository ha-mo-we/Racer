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
;;; FULL-ENUMERATOR ist nur ein anderes Wort fuer "COMPLETE VIEW WILL BE COMPUTED"
;;;

(defmethod full-enumerator-p :around ((query query))
  (unless (in-dnf-p query)
    (nrql-error "Query ~A is not in DNF" query))

  (and (not (existential-vois query))
  
       (if (top-level-query-p query)
      
           (answer-pattern query) ; sonst werden nur J/N-Antworten generiert!
         
         (or (is-or-query-p query)

             (let ((master (subquery-of query)))

               ;;; Disjunct in Oder -> Unabhaengige Bindungen

               (is-or-query-p master)) ;; -> query ist AND-Query or. ATOMIC-Query, wg. DNF!
        
             (and (=> (some #'una-voi-p (all-vois query))
                      (equal (all-vois query) 
                             (all-vois (top-level-query query))))

                  ;;; wenn die Query ein Atom ist und 
                  ;;; Teil eines ANDs, ist sie nur dann unabhaengig
                  ;;; bzw. full-enumerator, wenn *NOT UNA*, denn
                  ;;; sonst haengen die Bindungen von den bereits 
                  ;;; anderen vorhandenen Bindungen im AND ab, 
                  ;;; und die Cached-Bindings koennen nicht ohne
                  ;;; den AND-Kontext vollstaendig verwendet werden, 
                  ;;; waere also ein incomplete view!

                  (call-next-method))))))

(defmethod full-enumerator-p ((query unary-query))
  (and (not (bound-to (voi query)))
       (not (constraining-same-as-conjuncts query))))

(defmethod full-enumerator-p ((query binary-query))
  (and (not (bound-to (voi-from query)))
       (not (bound-to (voi-to query)))
       (not (constraining-same-as-conjuncts query))))


(defmethod full-enumerator-p ((query and-query))
  t)
             
(defmethod full-enumerator-p ((query or-query))
  t)


(defmethod full-enumerator-p ((query false-query))
  nil)

(defmethod full-enumerator-p ((query true-query))
  nil)

;;;
;;;
;;;

(defmethod get-upper-cache-candidates :around ((query query) (type symbol))
  (let ((sqs (subqueries query)))
    (unless (or (some #'negated-p sqs)
                (some #'is-query-reference-p sqs))
      (call-next-method))))

;;;
;;; funktioniert nur fuer DNF, aber das ist ausreichend!!! 
;;;

(defmethod in-same-conjunction-p ((candidate query) (reference query))
  (and (is-atomic-query-p candidate)
       (is-atomic-query-p reference)
       (eq (subquery-of candidate)
           (subquery-of reference))
       (is-and-query-p (subquery-of candidate))))

;;;
;;;
;;;       

(defmethod bad-cache-candidate-p ((candidate query) (reference query) (type symbol))
  (or (is-master-top-query-p candidate)
      
      ;;; (not (valid-qbox-entry-p candidate))

      (in-same-conjunction-p candidate reference) ; Bindungen noch nicht vollstaendig!

      ;;; Allgemeine Anmerkung: 
      ;;; die Bindungen von Eintraegen in der QBox sind immer vollstaendig!
      ;;; denn bei ABORT etc. werden eventuell in der QBox registrierte 
      ;;; Queries wieder ausgetragen, damit diese Queries mit unvollstaendigen
      ;;; Bindungslisten nicht als Kandidaten verwendent werden koennen!

      (not (variable-vectors-compatible-p candidate reference type))))

;;;
;;;
;;;

(defmethod get-exact-cache-candidates ((query query))
  (classify query) 
  (remove-if #'(lambda (x) (bad-cache-candidate-p x query :exact)) (equivalents query)))

(defmethod get-superset-cache-candidates ((query query))
  (classify query)   
  (sort (remove-if #'(lambda (x) (bad-cache-candidate-p x query :superset)) 
                   (dag-node-parents query))
        #'< ; je weniger Bindings, desto besser!
        :key #'(lambda (x) (length (bindings x)))))

;;;
;;;
;;;

(defmethod get-subset-cache-candidates ((query query))
  (classify query) 
  (sort (remove-if #'(lambda (x) (bad-cache-candidate-p x query :subset)) (dag-node-children query))
        #'> ; je mehr Bindings, desto besser!
        :key #'(lambda (x) (length (bindings x)))))

;;;
;;;
;;;

(defmethod same-kind-of-voi-p ((a null) (b null))
  t)

(defmethod same-kind-of-voi-p ((a voi) (b voi))
  (and (eq (type-of a) (type-of b))
       (<=> (una-voi-p a) (una-voi-p b))
       (=> (is-query-individual-p a) 
           (equal (textual-description a)
                  (textual-description b)))))

;;;
;;;
;;;

(defmethod variable-vectors-compatible-p ((exact-query query) (query query) (mode (eql :exact)))
  (and (variable-vectors-compatible-p exact-query query :superset)
       (variable-vectors-compatible-p query exact-query :superset)))

(defmethod variable-vectors-compatible-p ((subset-query query) (query query) (mode (eql :subset)))
  (variable-vectors-compatible-p query subset-query :superset))

(defmethod variable-vectors-compatible-p ((superset-query query) (query query) (mode (eql :superset)))
  (let ((superset-vois (all-paired-vois superset-query))
        (vois (all-paired-vois query)))
    (variable-vectors-compatible-p superset-vois vois mode)))

;;;
;;;
;;;

(defmethod variable-vectors-compatible-p ((superset-vois list) (vois list) (mode (eql :superset)))
  (and (= (length superset-vois) 
          (length vois))

       ;;; wenn Superset-Vois korrespondierende Bindung fordern, so muessen dies
       ;;; auch die Subset-Vois tun! 

       (every #'(lambda (pair1 pair2)                    
                  (and
                   (=> (first pair1) 
                       (and (first pair2)
                            (=> (second pair1)
                                (and (second pair2)
                                     (same-kind-of-voi-p (second pair1) 
                                                         (second pair2))))))
                   (=> (second pair1) 
                       (and (second pair2)
                            (=> (first pair1)
                                (and (first pair2)
                                     (same-kind-of-voi-p (second pair1) 
                                                         (second pair2))))))))
              superset-vois vois)))
;;;
;;;
;;;

(defmethod compute-cache-references ((query query) put-into-repository-p)
  (if (in-dnf-p query)
      (when (and (not (some #'negated-p (all-subqueries query)))
                 (not (query-inconsistent-p query)))
        (compute-cache-references1 query nil put-into-repository-p))

    (nrql-error "Query ~A is not in DNF" query)))

;;;
;;;
;;;

(defmethod compute-cache-references1 ((query null) rem put-into-repository-p)
  (declare (ignorable rem put-into-repository-p))
  t)

(defmethod compute-cache-references1 ((query query) rem put-into-repository-p)
  ; skip to next 
  (declare (ignorable rem))
  (compute-cache-references1 (first rem) (rest rem) put-into-repository-p))

(defmethod compute-cache-references1 ((query query-reference) rem put-into-repository-p)
  (declare (ignorable rem))
  (let ((rem (cons (referenced-query query)
                   rem)))
    (compute-cache-references1 (first rem) (rest rem) put-into-repository-p)))

(defmethod compute-cache-references1 ((query atomic-query) rem put-into-repository-p)
  (with-slots (cache-bindings-p
               superset-cache-reference
               subset-cache-reference 
               exact-cache-reference
               vois) query

    (let ((e-candidate
           (first
            (get-exact-cache-candidates query)))
          
          (s-candidate
           (first
            (get-superset-cache-candidates query)))

          (su-candidate
           (first
            (get-subset-cache-candidates query)))
          
          (put-into-repository-p 
           (and put-into-repository-p
                (full-enumerator-p query))))

      (cond (e-candidate
             
             (setf exact-cache-reference e-candidate
                   put-into-repository-p nil)

             ;; da ein e-Candidate gefunden wurde,
             ;; macht es natuerlich keinen Sinn,
             ;; die Query auch zu registrieren in 
             ;; der QBox! 
            
             )

            (t

             (setf superset-cache-reference s-candidate
                   subset-cache-reference su-candidate)))
      
      (when put-into-repository-p 
        (register-query query)
        (setf cache-bindings-p t))

      (with-vois-marked-as-bound vois
        (compute-cache-references1 (first rem) (rest rem) put-into-repository-p)))))


(defmethod compute-cache-references1 ((query and-query) rem put-into-repository-p)
  (with-slots (cache-bindings-p
               superset-cache-reference
               subset-cache-reference 
               exact-cache-reference) query
    
    (let ((e-candidate
           (first (get-exact-cache-candidates query)))

          (s-candidate 
           (first (get-superset-cache-candidates query)))

          (su-candidate 
           (first (get-subset-cache-candidates query)))

          (put-into-repository-p  
           (and put-into-repository-p
                (full-enumerator-p query))))

      (cond (e-candidate
             (setf exact-cache-reference e-candidate
                   put-into-repository-p nil))

            (t
             (setf superset-cache-reference s-candidate
                   subset-cache-reference su-candidate)))
    
      (let ((first (first (subqueries query)))
            (rem (append (rest (subqueries query)) rem)))
        
        (compute-cache-references1 first rem put-into-repository-p))

      (when put-into-repository-p 
        (register-query query)
        (setf cache-bindings-p t)))))


(defmethod compute-cache-references1 ((query or-query) rem put-into-repository-p)
  (declare (ignorable rem))
  
  (with-slots (;superset-cache-reference
               ;subset-cache-reference 
               ;exact-cache-reference
               ) query
    
    ;;; in DNF!
    
    ;;; wichtig - erst die Disjunkte bearbeiten! 
    ;;; Sonst kriegen die Disjunkte die query als
    ;;; Superset-Cache eingetragen! 
    
    (dolist (disjunct (subqueries query))
      (compute-cache-references1 disjunct nil put-into-repository-p))
    
    (when (and nil
               ;;; weil vom Compiler noch nicht unterstuetzt!
               
               put-into-repository-p 
               (full-enumerator-p query))
      
      (with-slots (cache-bindings-p) query
      
        (setf cache-bindings-p t)
        (register-query query)
        
        (dolist (disjunct (subqueries query))
          (setf (slot-value disjunct 'cache-bindings-p) t))))))
      

