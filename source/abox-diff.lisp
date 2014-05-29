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
;;; ABox Difference Operator
;;; 

(nrql-defun (compute-subgraph-aboxes) 
  (abox-or-name)
  (let* ((cas 
          (if (consp abox-or-name) 
              (mapcar #'(lambda (x) 
                          (list (second x) (third x)))
                      (remove-if-not #'(lambda (x) (member (first x) '(instance :instance))) abox-or-name))
            (all-concept-assertions abox-or-name)))
         (ras 
          (if (consp abox-or-name)
              (mapcar #'(lambda (x) 
                          (list (list (second x) (third x)) (fourth x)))
                      (remove-if-not #'(lambda (x) (member (first x) '(related :related))) abox-or-name))
            (all-role-assertions abox-or-name)))
         (sas
          (if (consp abox-or-name)
              (remove-if-not #'(lambda (x) (member (first x) '(same-as :same-as))) abox-or-name)
            (all-same-as-assertions abox-or-name)))
         (das 
          (if (consp abox-or-name)
              (remove-if-not #'(lambda (x) (member (first x) '(different-from :different-from))) abox-or-name)
            (all-different-from-assertions abox-or-name))))

    (let ((hash (make-hash-table))
          (assertions
           (append cas ras sas das)))

      (labels ((register-assertion (ass inds)
                 (let* ((graphs-and-inds 
                         (mapcar #'(lambda (ind)
                                     (gethash ind hash))
                                 inds))

                        (graph
                         (remove-duplicates
                          (cons ass 
                                (apply #'append (mapcar #'first graphs-and-inds)))
                          :test #'equal))

                        (inds
                         (remove-duplicates
                          (append inds 
                                  (apply #'append (mapcar #'second graphs-and-inds)))))

                        (new-graph-and-inds
                         (list graph inds)))

                   (dolist (ind inds)
                     (setf (gethash ind hash)  new-graph-and-inds)))))
          
        (loop while assertions do
              (let* ((ass (pop assertions)))
                (if (not (cddr ass))
                    (if (consp (first ass))
                        ;;; role assertion
                        (register-assertion ass (first ass))
                      ;;; concept assertion
                      (register-assertion ass (list (first ass))))
                  ;;; same-as / different-from
                  (register-assertion ass (rest ass)))))

        (let ((res nil))
            
          (maphash #'(lambda (key val)
                       (declare (ignorable key))
                       (pushnew val res))
                   hash)
          
          res)))))
                  
(nrql-defun (create-subgraph-aboxes) 
  (abox-or-name  &optional 
                 (new-name (if (symbolp abox-or-name)
                               abox-or-name
                             (current-abox)))
                 (tbox
                  (if (symbolp abox-or-name)
                      (associated-tbox abox-or-name)
                    (current-tbox))))
  (let ((aboxes 
         (mapcar #'first ; ohne inds
                 (compute-subgraph-aboxes abox-or-name))))
    (loop as abox in aboxes
          as index from 1 by 1 do
          (init-abox (intern (format nil "~A~A" new-name index)) tbox)
          (loop while abox do 
                (let* ((ass (pop abox)))
                  (if (not (cddr ass))
                      (if (consp (first ass))
                          ;;; role assertion
                          (add-role-assertion (current-abox) 
                                              (first (first ass))
                                              (second (first ass))
                                              (second ass))
                        ;;; concept assertion
                        (add-concept-assertion (current-abox)
                                               (first ass)
                                               (second ass)))
                    (if (eq (first ass) 'same-as)
                        (add-same-individual-as-assertion (current-abox)
                                                          (second ass) (third ass))
                      (add-different-from-assertion (current-abox)
                                                    (second ass) (third ass)))))))))


(nrql-defun (make-query-from-abox)
  (abox-or-name  &key
                 known-correspondences 
                 common-concept-assertions
                 common-role-assertions
                 common-same-as-assertions
                 common-different-from-assertions
                 (common-as-strict-atoms-p t)
                 (injective-variables-p t) 
                 forward-rule-consequence-p
                 &allow-other-keys)

  (labels ((var-for (ind)
             (if injective-variables-p 
                 (intern (format nil "$?~A" ind))
               (intern (format nil "?~A" ind)))))
    
    (let* ((cas 
            (set-difference 
             (if (consp abox-or-name) 
                 (mapcar #'(lambda (x) 
                             (list (second x) (third x)))
                         (remove-if-not #'(lambda (x) (member (first x) '(instance :instance))) abox-or-name))
               (all-concept-assertions abox-or-name))
             common-concept-assertions
             :test #'equal))
           
           (ras 
            (set-difference
             (if (consp abox-or-name)
                 (mapcar #'(lambda (x) 
                             (list (list (second x) (third x)) (fourth x)))
                         (remove-if-not #'(lambda (x) (member (first x) '(related :related))) abox-or-name))
               (all-role-assertions abox-or-name))
             common-role-assertions
             :test #'equal))

           (sas
            (set-difference
             (if (consp abox-or-name)
                 (remove-if-not #'(lambda (x) (member (first x) '(same-as :same-as))) abox-or-name)
               (all-same-as-assertions abox-or-name))
             common-same-as-assertions
             :test #'equal))

           (das 
            (set-difference 
             (if (consp abox-or-name)
                 (remove-if-not #'(lambda (x) 
                                    (member (first x)
                                            '(different-from :different-from)))
                                abox-or-name)
               (all-different-from-assertions abox-or-name))
             common-different-from-assertions
             :test #'equal))

           (body 

            (if forward-rule-consequence-p 
                `(,@(mapcar #'(lambda (x) 
                                `(same-as ,(var-for (first x)) ,(second x)))
                            known-correspondences)
                  ,@(mapcar #'(lambda (x) 
                                `(instance ,(var-for (first x)) ,(second x)))
                            cas)
                  ,@(mapcar #'(lambda (x) 
                                `(related 
                                  ,(var-for (first (first x)))
                                  ,(var-for (second (first x)))
                                  ,(second x)))
                            ras)
                  ,@(mapcar #'(lambda (x) 
                                `(same-as 
                                  ,(var-for (second x))
                                  ,(var-for (third x))))
                            sas)
                  ,@(mapcar #'(lambda (x) 
                                `(different-from
                                  ,(var-for (second x))
                                  ,(var-for (third x))))
                            das))

              `(and ,@(mapcar #'(lambda (x) 
                                  `(:strict (same-as ,(var-for (first x)) ,(second x))))
                              known-correspondences)

                    ;;;
                    ;;; Common Intersection: 
                    ;;;

                    ,@(mapcar #'(lambda (x) 
                                  `(:strict 
                                    (,(var-for (first x)) ,(second x))))
                              (and common-as-strict-atoms-p 
                                   common-concept-assertions))

                    ,@(mapcar #'(lambda (x) 
                                  `(:strict  
                                    (,(var-for (first (first x)))
                                     ,(var-for (second (first x)))
                                     ,(second x))))
                              (and common-as-strict-atoms-p 
                                   common-role-assertions))
                    
                    ,@(mapcar #'(lambda (x) 
                                  `(:strict (same-as ,(var-for (second x)) ,(third x))))
                              (and common-as-strict-atoms-p 
                                   common-same-as-assertions))

                    ,@(mapcar #'(lambda (x) 
                                  `(:strict (different-from ,(var-for (second x)) ,(third x))))
                              (and common-as-strict-atoms-p
                                   common-different-from-assertions))

                    ;;;
                    ;;;
                    ;;;

                    ,@(mapcar #'(lambda (x) 
                                  `(,(var-for (first x)) ,(second x)))
                              cas)

                    ,@(mapcar #'(lambda (x) 
                                  `(,(var-for (first (first x)))
                                    ,(var-for (second (first x)))
                                    ,(second x)))
                              ras)

                    ,@(mapcar #'(lambda (x) 
                                  `(,(var-for (second x))
                                    ,(var-for (third x))
                                    same-as))
                              sas)

                    ,@(mapcar #'(lambda (x) 
                                  `(,(var-for (second x))
                                    ,(var-for (third x))
                                    different-from))
                              das)))))

      body)))


(defun build-query-from-abox (abox known-correspondences 
                                   common-concept-assertions 
                                   common-role-assertions
                                   common-same-as-assertions
                                   common-different-from-assertions
                                   common-as-strict-atoms-p)
  (make-query-from-abox abox
                        :known-correspondences known-correspondences
                        :common-concept-assertions common-concept-assertions
                        :common-role-assertions common-role-assertions
                        :common-same-as-assertions common-same-as-assertions
                        :common-different-from-assertions common-different-from-assertions
                        :common-as-strict-atoms-p common-as-strict-atoms-p))


(defun get-vars-in (expr)
  (let ((vars nil)
        (parser (make-instance 'nrql-abox-query-parser)))

    (labels ((do-it (expr)
               (if (symbolp expr)
                   (when (var-p parser expr)
                     (push expr vars))
                 (progn 
                   (do-it (car expr))
                   (do-it (cdr expr))))))

      (do-it expr))
    
    vars))


(nrql-defun (make-forward-rule-from-aboxes)
  (precond-abox postcond-abox for-abox &rest args &key &allow-other-keys)
  (let* ((pre
          (apply #'make-query-from-abox precond-abox args))
         (post 
          (apply #'make-query-from-abox postcond-abox 
                 :forward-rule-consequence-p t
                 args))

         (pre-vars (remove-duplicates
                    (get-vars-in pre)))
         (post-vars (remove-duplicates
                     (get-vars-in post)))
         (diff (set-difference post-vars pre-vars)))

    (dolist (var diff)
      (setf post
            (tree-substitute post
                             var `(new-ind ,@pre-vars))))

    (apply #'racer-prepare-rule pre post :abox for-abox args)))


(nrql-defun (make-backward-rule-from-aboxes)
  (precond-abox postcond-abox  for-abox &rest args &key &allow-other-keys)
  (let ((pre
         (apply #'make-query-from-abox precond-abox args))
        (post 
         (cdr ; remove and
          (apply #'make-query-from-abox postcond-abox 
                 :forward-rule-consequence-p nil
                 args))))
    (when (or (not (consp post)) (cdr post))
      (nrql-error "Postcondition is ~S. Sorry, cannot handle non-horn rules" 
                  post))

    (let ((post (first post)))
    
      (if (member (first post) '(same-as different-from))
          (define-query (first post) 
                        (rest post) 
                        pre
                        :tbox (associated-tbox for-abox))
        (define-query (first (last post)) 
                      (butlast post) 
                      pre
                      :tbox (associated-tbox for-abox))))))


(nrql-defun (make-abduction-rule-from-aboxes)
  (precond-abox postcond-abox for-abox &rest args
                &key (forward-rule-p t) (backward-rule-p t) 
                &allow-other-keys)
  (let ((pre
         (apply #'make-query-from-abox precond-abox args))
        (post 
         (cdr ; remove and
          (apply #'make-query-from-abox postcond-abox 
                 :forward-rule-consequence-p nil
                 args))))
    (when (or (not (consp post)) (cdr post))
      (nrql-error "Postcondition is ~S. Sorry, cannot handle non-horn rules" 
                  post))

    (let ((post (first post)))
      (add-rule-axiom for-abox
                      post pre
                      :forward-rule-p forward-rule-p 
                      :backward-rule-p backward-rule-p))))


(nrql-defun (remove-implied-concept-assertions)
  (abox)
  (let ((inds (all-individuals abox))
        (new nil)
        (old (all-concept-assertions abox)))

    (dolist (ind inds)
      (let ((is (mapcar #'first (most-specific-instantiators ind abox))))
        (if (cdr is)
            (push `(,ind (and ,@is)) new)
          (push `(,ind ,@is) new))))
    
    (dolist (ca old)
      (forget-concept-assertion abox (first ca) (second ca)))

    (dolist (ca new)
      (add-concept-assertion abox (first ca) (second ca)))

    (list old new)))
    

(nrql-defun (compute-abox-difference1)
  (a b &rest args
     &key 

     also-unmapped-differences-p 
     (remove-redundant-diffs-p t)

     (optimizer-max-plans 3)
     known-correspondances 
     (auto-correspondances-p t)
     (cutoff-fn nil) 
     ;;; '(:hypothesized-assertions < 6)
     (only-difference-p t)
     (full-tuples-p t)
     (show-score-p t)
     (only-best-p t)
     (equi-order-by :prefer-old-inds)

     (remove-implied-concept-assertions-p t)

     (remove-common-assertions-p t)
     common-assertions-as-strict-atoms-p 
                                   
     (order-by :new-paper-fn) ; maximize! 
     (reverse-order-p t)

     map-new-inds-to-new-inds-p

     (c-mode '(:reuse-old 
               :one-new-ind 
               :always-apply-modi))
     (r-mode '(:reuse-old 
               :one-new-ind 
               :always-apply-modi
               ))
     &allow-other-keys)

  (labels ((pretty (ind)
             (if (> (length (symbol-name ind))2) 
                 (let* ((name (subseq (symbol-name ind) 2))
                        (pos (or (search "-ano" name)
                                 (search "-ANO" name))))
                   (if pos
                       (intern (subseq name 0 pos))
                     (intern name)))
               ind)))

    (let ((old-new nil)
          (*keep-original-var-name-in-ano-vars-p* t))

      (when remove-implied-concept-assertions-p
        (dolist (abox (list a b))
          (push (cons abox 
                      (remove-implied-concept-assertions abox))
                old-new)))              
    
      (let* ((*optimizer-max-plans* optimizer-max-plans)
	     (*disable-defined-queries-p* t)
                 
             (common-concept-assertions
              (intersection (all-concept-assertions a)
                            (all-concept-assertions b)
                            :test #'equal))

             (common-role-assertions
              (intersection (all-role-assertions a)
                            (all-role-assertions b)
                            :test #'equal))

             (common-same-as-assertions
              (intersection (all-same-as-assertions a)
                            (all-same-as-assertions b)
                            :test #'equal))
	   
             (common-different-from-assertions
              (intersection (all-different-from-assertions a)
                            (all-different-from-assertions)
                            :test #'equal)))

        (when remove-common-assertions-p
          (dolist (abox (list b))
            (dolist (ca common-concept-assertions)
              (forget-concept-assertion abox (first ca) (second ca)))
            (dolist (ra common-role-assertions)
              (forget-role-assertion abox
                                     (first (first ra))
                                     (second (first ra))
                                     (second ra)))
            (dolist (sa common-same-as-assertions)
              (forget-same-individual-as-assertion abox (second sa) (third sa)))
            (dolist (da common-different-from-assertions)
              (forget-different-from-assertion abox (second da) (third da)))))

        (let* ((inds-a (when (and auto-correspondances-p 
                                  (not known-correspondances))
                         (all-individuals a)))
             
               (inds-b (when (and auto-correspondances-p
                                  (not known-correspondances))
                         (all-individuals b)))
               (known-correspondances 
                (if (and inds-a inds-b)
                    (mapcar #'(lambda (x) (list x x))
                            (intersection inds-a inds-b))
                  known-correspondances))

               (body (build-query-from-abox a 
                                            known-correspondances
                                            common-concept-assertions
                                            common-role-assertions
                                            common-same-as-assertions
                                            common-different-from-assertions
                                            (and (not remove-common-assertions-p)
                                                 common-assertions-as-strict-atoms-p)))

               (body (if (equal body '(and))
                         (return-from
                             compute-abox-difference1
                           (if only-difference-p
                               '(nil)
                             (error "Difference is empty, since ABox ~A is empty. Cannot say more with \":only-difference-p NIL\", sorry" a)))
                                
                       body))
	   
               (res 
                (apply #'racer-answer-query-with-explanation 
                       nil
                       body
                       :binding-validator 
                       (when map-new-inds-to-new-inds-p 
                         (if (eq map-new-inds-to-new-inds-p 
                                 :strict)
                             #'(lambda (voi)
                                 (=> (bound-to voi)
                                     (<=> (really-is-new-ind-p
                                           (let ((name 
                                                  (symbol-name (textual-description voi))))
                                             (if (char= #\$ (elt name 0))
                                                 (subseq name 2)
                                               (subseq name 1))))
                                          (really-is-new-ind-p
                                           (symbol-name (bound-to voi))))))
                           #'(lambda (voi)
                               (=> (bound-to voi)
                                   (=> (really-is-new-ind-p
                                        (let ((name 
                                               (symbol-name (textual-description voi))))
                                          (if (char= #\$ (elt name 0))
                                              (subseq name 2)
                                            (subseq name 1))))
                                       (really-is-new-ind-p
                                        (symbol-name (bound-to voi))))))))
                       :abox b
                       :cutoff-fn cutoff-fn
                       :ensure-permutations-p (not only-difference-p)
                       :full-tuples-p full-tuples-p
                       :show-score-p show-score-p 
                       :only-best-p only-best-p 
                       :reverse-order-p reverse-order-p 
                       :equi-order-by equi-order-by
                       :order-by order-by
                       :c-mode c-mode
                       :r-mode r-mode
                       args)))
        
          (when remove-common-assertions-p
            (dolist (abox (list b))
              (dolist (ca common-concept-assertions)
                (add-concept-assertion abox (first ca) (second ca)))
              (dolist (ra common-role-assertions)
                (add-role-assertion abox (first (first ra)) (second (first ra)) (second ra)))
              (dolist (sa common-same-as-assertions)
                (add-same-individual-as-assertion abox (second sa) (third sa)))
              (dolist (da common-different-from-assertions)
                (add-different-from-assertion abox (second da) (third da)))))

          (loop as (abox old new) in old-new do
                (loop as (ind ca) in new do
                      (forget-concept-assertion abox ind ca))
                (loop as (ind ca) in old do
                      (add-concept-assertion abox ind ca)))

          (if (equal res '(:timeout))
              
              :timeout

            (let* ((diff (mapcar #'(lambda (res)
                                     (let* ((new-inds (cdr (assoc :new-inds res)))
                                            (hypo (assoc :hypothesized-assertions res))
                                            (full-tuple (cdr (assoc :full-tuple res)))
                                            ;; (score (fourth (assoc :score res)))
                                            ;; (hypo2 (assoc :hypothesized-assertions score))
                                            (hypo1 (copy-tree hypo))
                                            (hypo3  
                                             (let* ((first 
                                                     (apply-mapping (rest hypo)
                                                                    (remove nil
                                                                            (mapcar #'(lambda (x) 
                                                                                        (unless (member (second x) new-inds)
                                                                                          (list (second x)
                                                                                                (pretty (first x)))))
                                                                                    full-tuple)))))
				     
                                               (apply-mapping first
                                                              (let ((rev-full (mapcar #'reverse full-tuple)))
                                                                (remove nil
                                                                        (mapcar #'(lambda (x) 
                                                                                    (when (member (second x) new-inds)
                                                                                      (list (second x)
                                                                                            (let* ((key (pretty (first x)))
                                                                                                   (found (second (assoc key rev-full))))
                                                                                              (if found
                                                                                                  (pretty found)
                                                                                                key)))))
                                                                                full-tuple)))))))
			       
                                       (setf (cdr hypo) hypo3
					;; (cdr hypo2) hypo3
                                        )
			       			       
                                       (if also-unmapped-differences-p
                                           (append res
                                                   `((:original-assertions ,@(rest hypo1))))
                                         res)))
			 
                                 (second res)))
	   
                   (diff (if (not only-difference-p)
                             diff
                           (if remove-redundant-diffs-p
                               (remove-duplicates diff :test #'(lambda (x y) 
                                                                 (set-equal (cdr (assoc :hypothesized-assertions x))
                                                                            (cdr (assoc :hypothesized-assertions y))
                                                                            :test #'equal)))
                             diff))))
      
              (if only-difference-p
                  (mapcar #'(lambda (x) 
                              (if also-unmapped-differences-p
                                  (list (rest (assoc :hypothesized-assertions x))
                                        (rest (assoc :original-assertions x)))
                                (rest (assoc :hypothesized-assertions x))))
                          diff)
                (list (first res) diff)))))))))

(nrql-defmacro (compute-abox-difference :nrql-function compute-abox-difference1))


(nrql-defun (compute-abox-difference2) (a b &rest args)
  (apply #'compute-abox-difference1 a b
         :order-by :hypothesized-assertions 
         :reverse-order-p nil  ; minimieren! 
         args))

(nrql-defmacro (compute-abox-difference-alternative :nrql-function compute-abox-difference2))

#|

(defun test1 ()
  (full-reset)

  (in-abox a)
  ;(instance j c)
  ;(instance k c)
  (related i j r)
  (related j k r)
  (related k l r)
  (related l i r)

  (in-abox b)
  ;(instance a c)
  ;(instance b c)
  (related a b r)
  (related b c r)
  (related c d r)
  (related d a r)

  (abox-consistent?)

  (pprint (compute-abox-difference a b
                                   :only-difference-p t
                                   :only-best-p t
                                   )))

(defun test1b ()
  (full-reset)

  (in-abox a)
  (instance j c)
  (instance k c)
  (related i j r)
  (related j k r)
  (related k l r)
  (related l i r)

  (in-abox b)
  (instance a c)
  ;(instance b c)
  (related a b r)
  (related b c r)
  (related c d r)
  (related d a r)

  (abox-consistent?)

  (pprint (compute-abox-difference a b
                                   :only-difference-p nil
				   :known-correspondances ((k a))
                                   )))

(defun test2 ()
  (full-reset)
  
  ;;(enable-abduction-debugging)
  
  (in-abox a)
  (related i j r)
  (related j i r)

  (in-abox b)
  (related a b r)
  (related b a r)

  (abox-consistent?)

  (pprint (compute-abox-difference a b
                                   :only-best-p t
                                   :show-score-p t
                                   :only-difference-p nil
                                   )))

                                 
(defun test3 ()
  (full-reset)

  (in-abox a)

  (related i j r)
  (related j i r)
  
  (related j k r)
  (related k j r)

  (related k i r)
  (related i k r)

  (in-abox b)

  (related a b r)
  (related b a r)
  
  (related b c r)
  (related c b r)

  (related c a r)
  (related a c r)

  (related a m r)
  (related b m r)
  (related c m r)

  (pprint 
   (compute-abox-difference b a
                            :show-score-p t
                            :only-difference-p t
                            )))


(defun test4 (&optional with-marker-p)
  (loop as n from 3 to 15 by 1 do
        (labels ((ind (n)
                   (intern (format nil "ABC-~A" n)))
           
                 (xind (n)
                   (intern (format nil "DEF-~A" n))))
           
    
          (full-reset)

          (in-abox a)

          (eval 
           (cons 'progn
                 (cons 
                  `(related ,(ind n) ,(ind 1) r)
                  (loop as i from 1 to (1- n)
                        collect
                        `(related ,(ind i) ,(ind (1+ i)) r)))))

          (when with-marker-p 
            (eval `(instance ,(ind (1+ (random n)))
                             marker)))
          
          (in-abox b)

          (eval 
           (cons 'progn
                 (cons 
                  `(related ,(xind n) ,(xind 1) r)
                  (loop as i from 1 to (1- n)
                        collect
                        `(related ,(xind i) ,(xind (1+ i)) r)))))

          (when with-marker-p 
            (eval `(instance ,(xind (1+ (random n)))
                             marker)))

          (define x (new-inds new-ass old-ass)
                  (< (length new-ass) 3))

          (let* ((t1 (get-internal-real-time))
                 (diff 
                  (compute-abox-difference a b)))
            (format t "~%~A : ~,5f ~S"
                    n
                    (float (/ (- (get-internal-real-time) t1)
                              internal-time-units-per-second))
                    diff)))))


(defun test5 ()

  (full-reset)

  (in-abox a)

  (instance i1 e)
  (instance j1 c)
  (instance k1 d)
  
  (related i1 j1 r)
  (related i1 k1 r)

  (in-abox b)
  
  (instance i2 f)
  (instance j2 c)
  (instance k2 d)
  
  (related i2 j2 r)
  (related i2 k2 r)
  
  (pprint (compute-abox-difference a b))

  (pprint (compute-abox-difference b a)))


|# 


