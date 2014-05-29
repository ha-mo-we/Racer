;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: RACER; Base: 10 -*-

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

(in-package :racer)

(defun lcs (concept1 concept2)
  (cond ((not (concept-satisfiable-p concept1 *current-tbox*))
         concept2)
        ((not (concept-satisfiable-p concept2 *current-tbox*))
         concept1)
	((and (symbolp concept1) (symbolp concept2))         
	 (if (eq concept1 concept2)
	     concept1
	   '*top*))
        ((and (consp concept1) (consp concept2)
	      (eq (first concept1) 'at-least) 
	      (eq (first concept2) 'at-least)
	      (eq (third concept1) (third concept2)))
	 (list 'at-least (min (second concept1) (second concept2))))
	((and (consp concept1) (consp concept2)
	      (eq (first concept1) 'at-most) 
	      (eq (first concept2) 'at-most)
	      (eq (third concept1) (third concept2)))
	 (list 'at-least (max (second concept1) (second concept2))))
	((and (consp concept1) (consp concept2)
	      (eq (first concept1) 'some) 
	      (eq (first concept2) 'some)
	      (eq (second concept1) (second concept2)))
	 (list 'some (second concept1) 
	       (lcs (third concept1) (third concept2))))
	((and (consp concept1) (consp concept2)
	      (eq (first concept1) 'all) 
	      (eq (first concept2) 'all)
	      (eq (second concept1) (second concept2)))
	 (list 'all (second concept1)
	       (lcs (third concept1) (third concept2))))
	((and (consp concept1) (consp concept2)
	      (eq (first concept1) 'not) 
	      (eq (first concept2) 'not))
         (unless (and (symbolp (second concept1)) (symbolp (second concept2)))
           (error "Not can be applied only to concept names."))
	 (if (eq (second concept1) (second concept2))
           concept1
           '*top*))
	((and (consp concept1) (eq (first concept1) 'and))
	 (let ((term (remove-duplicates (remove '*top* 
						(loop for x in (normalize-conjunction 
                                                                (flatten-conjuncts (cdr concept1)))
						    when (not (eq x '*top*)) 
						    collect (lcs x concept2))))))
	   (if (not (cdr term))
	       (if (first term) 
		   (first term) 
		 '*top*)
	     (cons 'and term))))
	((and (consp concept2) (eq (first concept2) 'and))
	 (lcs concept2 concept1))
	(t '*top*)))

(defun flatten-conjuncts (conjuncts)
  (cond ((null conjuncts)
         nil)
        ((and (consp (first conjuncts))
              (eq (first (first conjuncts)) 'and))
         (flatten-conjuncts (append (rest (first conjuncts)) (rest conjuncts))))
        (t (cons (first conjuncts)
                 (flatten-conjuncts (rest conjuncts))))))

(defun normalize-conjunction (conjuncts)
  (cond ((null conjuncts)
         nil)
        ((and (consp (first conjuncts))
              (eq (first (first conjuncts)) 'all))
         (cons (first conjuncts) (normalize-conjunction (rewrite-concepts (second (first conjuncts))
                                                                          (third (first conjuncts))
                                                                          (rest conjuncts)))))
        (t (cons (first conjuncts)
                 (normalize-conjunction (rest conjuncts))))))

(defun rewrite-concepts (role-name qualification-concept conjuncts)
  (cond ((null conjuncts)
         nil)
        ((and (consp (first conjuncts))
              (eq (first (first conjuncts)) 'some)
              (eq (second (first conjuncts)) role-name))
         (cons `(some ,role-name (and ,qualification-concept ,(third (first conjuncts))))
               (rewrite-concepts role-name qualification-concept (rest conjuncts))))
        (t (cons (first conjuncts)
                 (rewrite-concepts role-name qualification-concept (rest conjuncts))))))

(defun lcs-unfold (concept-1 concept-2 &optional (tbox (current-tbox)))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (when (tbox-cyclic-p tbox)
    (error "Tbox cannot be unfolded."))
  (with-alc-environment (:tbox
                         tbox
                         :id-variable (tbox-structure-id-counter tbox)
                         :concept-store (tbox-concept-store tbox)
                         :tableaux-cache (tbox-tableaux-cache tbox)
                         :tableaux-sat-cache (tbox-tableaux-sat-cache tbox)
                         :tableaux-unsat-cache (tbox-tableaux-unsat-cache tbox)
                         :role-store (tbox-role-store tbox)
                         :concrete-domain (tbox-concrete-domain tbox)
                         :stable-set-difference-table (tbox-stable-set-difference-table tbox)
                         :stable-set-difference-last-list2 (tbox-stable-set-difference-last-list2 tbox)
                         :racer-remove-duplicates-table (tbox-racer-remove-duplicates-table tbox)
                         :racer-remove-constraint-duplicates-table
                         (tbox-racer-remove-constraint-duplicates-table tbox)
                         :possible-subsumees-vector (tbox-possible-subsumees-vector tbox)
                         :expanded-constraints-ind-table (tbox-expanded-constraints-ind-table tbox)
                         :live-inds-table (tbox-live-inds-table tbox)
                         :obsolete-inds-table (tbox-obsolete-inds-table tbox)
                         :label-inds-table (tbox-label-inds-table tbox)
                         :new-inds-table (tbox-new-inds-table tbox)
                         :concept-set-mark (tbox-concept-set-mark tbox)
                         :role-set-mark (tbox-role-set-mark tbox)
                         :individual-set-mark (tbox-individual-set-mark tbox)
                         :constraint-set-mark (tbox-constraint-set-mark tbox)
                         :classification-counter (tbox-classification-counter tbox)
                         :obsolete-eql-tables (tbox-obsolete-eql-tables tbox)
                         :obsolete-equal-tables (tbox-obsolete-equal-tables tbox)
                         :obsolete-equal-concept-tables (tbox-obsolete-equal-concept-tables tbox)
                         :initial-hash-table-size (tbox-initial-hash-table-size tbox)
                         :signatures-equal-table (tbox-signatures-equal-table tbox)
                         :partitions-table (tbox-partitions-table tbox)
                         :use-less-tbox-memory (tbox-use-less-memory tbox)
                         :set-vector (tbox-set-vector tbox)
                         :select-disjunct-table (tbox-select-disjunct-table tbox)
                         )
    (with-alc-bindings
      (lcs (if (symbolp concept-1)
             (decode-tbox-concept concept-1 tbox t)
             (make-normalized-term (decode-concept (encode-concept-term concept-1) t)))
           (if (symbolp concept-2)
             (decode-tbox-concept concept-2 tbox t)
             (make-normalized-term (decode-concept (encode-concept-term concept-2) t)))))))

(defmacro lcs-unfold-1 (concept-1 concept-2 &optional (tbox (current-tbox) tbox-supplied-p))
  (if tbox-supplied-p
    `(lcs-unfold ',concept-1 ',concept-2 ',tbox)
    `(lcs-unfold ',concept-1 ',concept-2)))




(defun msc-k (individual k &rest args
	      &key (include-direct-types nil) (abox (current-abox)) (name nil name-supplied-p)
	      &allow-other-keys)

  (declare (ignorable name))
  
  (setf abox (find-abox abox))
  (check-type individual symbol)
  (ensure-knowledge-base-state ':abox-prepared abox)
  (let ((result 
	 `(and .,(msc-k-1 (find-individual abox individual) abox k 0 include-direct-types))))
    
    (when name-supplied-p
      (apply #'add-result-concept-axiom result 
	     :tbox (associated-tbox abox) 
	     args))
    
    result))

(defun msc-k-1 (ind abox k n include-direct-types)
  (if (> n k)
    '(top)
    (loop for constraint in (individual-outgoing-role-assertions ind)
          as ind-2 = (find-individual abox (constraint-ind-2 constraint))
          collect `(and ,@(if include-direct-types
                            (mapcar #'first (most-specific-instantiators 
                                             (first (individual-name-set ind)) abox))
                            ())
                        
                        (some ,(constraint-term constraint)
                              (and ,@(mapcar #'second (individual-assertions ind-2))
                                   ,@(msc-k-1 ind-2 abox k (1+ n) include-direct-types)))))))


(defun alen-approx-to-new-concept (concept &rest args 
				   &key (name nil name-supplied-p) 
				   &allow-other-keys)
  (declare (ignorable args name name-supplied-p concept))
  
  #+:sonic
  (let ((result (funcall (intern (symbol-name 'alen-approx) :sonic) concept)))
    
    (when name-supplied-p
      (apply #'add-result-concept-axiom result args))
    
    result)
  #-:sonic
  (error "Approximation is not supported in this version of Racer."))


(defun alen-lcs (concept &rest args 
		 &key (name nil name-supplied-p) 
		 &allow-other-keys)
  
  (declare (ignorable name name-supplied-p concept args))
  
  #+:sonic
  (let ((result (funcall (intern (symbol-name 'alen-lcs) :sonic) concept)))
    
    (when name-supplied-p
      (apply #'add-result-concept-axiom result args))
    
    result)
  #-:sonic
  (error "Approximation is not supported in this version of RacerPro."))



(defun add-result-concept-axiom (result &key (tbox (current-tbox)) name &allow-other-keys)
  (add-concept-axiom tbox
		     (or name (gentemp))
		     result
		     :inclusion-p nil))
