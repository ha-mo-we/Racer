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

#+:debug
(defvar *clash-culprits-required-p*)

;;;===========================================================================
;;; Clash detection
;;;===========================================================================

(race-inline (true-concept-term constraint-equal-test))

(defun true-concept-term (exists-concept)
  #+:debug (assert (exists-concept-p exists-concept))
  (or (and *use-tbox* (concept-role-range exists-concept))
      (concept-term exists-concept)))

(defun constraint-equal-test (constraint-1 constraint-2)
  "Test relaxed equality of two constraints. They are considered as equal
if their ids, terms, and negation flags match."
  (and (eq (constraint-term constraint-1) (constraint-term constraint-2))
       (eq (constraint-negated-p constraint-1) (constraint-negated-p constraint-2))
       (eql (constraint-ind constraint-1) (constraint-ind constraint-2))))
        
(defun constraint-equal-synonym-test (constraint-1 constraint-2)
  (and (eq (constraint-term constraint-1) (constraint-term constraint-2))
       (eq (constraint-negated-p constraint-1) (constraint-negated-p constraint-2))
       (let ((ind-1 (constraint-ind constraint-1))
             (ind-2 (constraint-ind constraint-2)))
         (or (eql ind-1 ind-2)
             (member ind-1 (constraint-ind-synonyms constraint-2))
             (member ind-2 (constraint-ind-synonyms constraint-1))))))
        
(defun same-ind-constraint-equal-test (constraint-1 constraint-2)
  (and (eq (constraint-term constraint-1) (constraint-term constraint-2))
       (eq (constraint-negated-p constraint-1) (constraint-negated-p constraint-2))))
        
(defun subsumed-number-restrictions-p (subsumee subsumer)
  (when (and (number-restriction-concept-p subsumer) (number-restriction-concept-p subsumee))
    (cond ((and (at-least-concept-p subsumer) (at-least-concept-p subsumee))
           (and (if *use-tbox*
                  (member (concept-role subsumer) (role-ancestors-internal (concept-role subsumee)))
                  (eq (concept-role subsumer) (concept-role subsumee)))
                (and (>= (concept-number-restriction subsumee)
                         (concept-number-restriction subsumer))
                     (or (eq (true-concept-term subsumer) *top-concept*)
                         (eq (true-concept-term subsumee) (true-concept-term subsumer))))))
          ((and (at-most-concept-p subsumer) (at-most-concept-p subsumee))
           (and (if *use-tbox*
                  (member (concept-role subsumee) (role-ancestors-internal (concept-role subsumer)))
                  (eq (concept-role subsumer) (concept-role subsumee)))
                (and (<= (concept-number-restriction subsumee)
                         (concept-number-restriction subsumer))
                     (or (eq (concept-term subsumee) *top-concept*)
                         (eq (concept-term subsumee) (concept-term subsumer))))))
          (t nil))))

(race-inline (alc-subsumed-concept-p 
              alcn-subsumed-concept-p
              alc-subsumes-concept-p
              alcn-subsumes-concept-p
              alc-concept-clash-p
              elh-concept-clash-p))

(defun alc-subsumed-concept-p (subsumee subsumer)
  (eq subsumee subsumer))

(defun alcn-subsumed-concept-p (subsumee subsumer)
  (or (eq subsumee subsumer)
      (subsumed-number-restrictions-p subsumee subsumer)))

(defun alc-subsumes-concept-p (subsumer subsumee)
  (eq subsumee subsumer))

(defun alcn-subsumes-concept-p (subsumer subsumee)
  (or (eq subsumee subsumer)
      (subsumed-number-restrictions-p subsumee subsumer)))

(defun alc-concept-clash-p (concept-1 concept-2)
  (eq concept-1 (concept-negated-concept concept-2)))

(defun elh-concept-clash-p (concept-1 concept-2)
  #-:debug (declare (ignore concept-1 concept-2))
  #+:debug (assert (and (subset-el+-p (concept-language concept-1))
                        (subset-el+-p (concept-language concept-2))))
  nil)

(defun alcn-concept-clash-p (concept-1 concept-2)
  (or (eq concept-1 (concept-negated-concept concept-2))
      (cond ((exists-concept-p concept-1)
             (let* ((role-1 (concept-role concept-1))
                    (ancestors-p (role-has-ancestors-p role-1)))
               (cond
                ((at-most-concept-p concept-2)
                 (let ((role-2 (concept-role concept-2)))
                   (when (or (eq role-1 role-2)
                             (and ancestors-p (member role-2 (role-ancestors-internal role-1))))
                     (and (> (concept-number-restriction concept-1)
                             (concept-number-restriction concept-2))
                          (or (eq (concept-term concept-2) *top-concept*)
                              (eq (true-concept-term concept-1) (concept-term concept-2)))))))
                ((all-concept-p concept-2)
                 (let ((role-2 (concept-role concept-2)))
                   (when (or (eq role-1 role-2)
                             (and ancestors-p (member role-2 (role-ancestors-internal role-1))))
                     (let ((term (concept-term concept-2)))
                       (or (eq term *bottom-concept*)
                           (eq (true-concept-term concept-1)
                               (concept-negated-concept term)))))))
                (t nil))))
            ((exists-concept-p concept-2)
             (let* ((role-2 (concept-role concept-2))
                    (ancestors-p (role-has-ancestors-p role-2)))
               (cond
                ((at-most-concept-p concept-1)
                 (let ((role-1 (concept-role concept-1)))
                   (when (or (eq role-2 role-1)
                             (and ancestors-p (member role-1 (role-ancestors-internal role-2))))
                     (and (> (concept-number-restriction concept-2)
                             (concept-number-restriction concept-1))
                          (or (eq (concept-term concept-1) *top-concept*)
                              (eq (concept-term concept-1) (true-concept-term concept-2)))))))
                ((all-concept-p concept-1)
                 (let ((role-1 (concept-role concept-1)))
                   (when (or (eq role-2 role-1)
                             (and ancestors-p (member role-1 (role-ancestors-internal role-2))))
                     (let ((term (concept-term concept-1)))
                       (or (eq term *bottom-concept*)
                           (eq (true-concept-term concept-2)
                               (concept-negated-concept term)))))))
                (t nil))))
            (t nil))))

(race-inline (alc-matching-constraint-test))

(defun alc-matching-constraint-test (constraint-1 constraint-2)
  (and (eq (constraint-term constraint-2) (constraint-term constraint-1))
       (eql (constraint-ind constraint-1) (constraint-ind constraint-2))))

(defun alcn-matching-constraint-test (new-constraint old-constraint)
  (when (eql (constraint-ind new-constraint) (constraint-ind old-constraint))
    (let ((new-concept (constraint-term new-constraint))
          (old-concept (constraint-term old-constraint)))
      (or (eq new-concept old-concept)
          (and (exists-concept-p new-concept) (exists-concept-p old-concept)
               (let* ((new-role (concept-role new-concept))
                      (old-role (concept-role old-concept))
                      (roles-eq-p (eq new-role old-role))
                      (new-role-superrole-p
                       (and (not roles-eq-p)
                            *use-tbox*
                            (role-has-ancestors-p old-role)
                            (member new-role (role-ancestors-internal old-role)))))
                 (cond
                  (roles-eq-p
                   (if (constraint-negated-p old-constraint)
                     (and (>= (concept-number-restriction new-concept)
                              (concept-number-restriction old-concept))
                          (or (eq (true-concept-term old-concept) *top-concept*)
                              (eq (true-concept-term new-concept)
                                  (true-concept-term old-concept))))
                     (and (<= (concept-number-restriction new-concept)
                              (concept-number-restriction old-concept))
                          (or (eq (true-concept-term new-concept) *top-concept*)
                              (eq (true-concept-term new-concept)
                                  (true-concept-term old-concept))))))
                  (new-role-superrole-p
                   (unless (constraint-negated-p old-constraint)
                     (and (<= (concept-number-restriction new-concept)
                              (concept-number-restriction old-concept))
                          (or (eq (true-concept-term new-concept) *top-concept*)
                              (eq (true-concept-term new-concept)
                                  (true-concept-term old-concept))))))
                  ((and *use-tbox*
                        (role-has-ancestors-p new-role)
                        (member old-role (role-ancestors-internal new-role)))
                   (and (constraint-negated-p old-constraint)
                        (>= (concept-number-restriction new-concept)
                            (concept-number-restriction old-concept))
                        (or (eq (true-concept-term old-concept) *top-concept*)
                            (eq (true-concept-term new-concept)
                                (true-concept-term old-concept)))))
                  (t nil))))))))

(defun simple-constraint-clash-test (new-constraint old-constraint)
  (and (eq (constraint-term new-constraint) (constraint-term old-constraint))
       (not (eq (constraint-negated-p new-constraint)
                (constraint-negated-p old-constraint)))
       (eql (constraint-ind new-constraint) (constraint-ind old-constraint))))

(defun find-simple-constraint-clash (new-constraint unexpanded-disjunctive-constraints)
  (loop with new-ind = (constraint-ind new-constraint)
        with new-negated-p = (constraint-negated-p new-constraint)
        with new-concept = (constraint-term new-constraint)
        for old-constraint in unexpanded-disjunctive-constraints do
        (when (and (eq new-concept (constraint-term old-constraint))
                   (not (eq new-negated-p (constraint-negated-p old-constraint)))
                   (eql new-ind (constraint-ind old-constraint)))
          (return old-constraint))))

(defun constraint-clash-test (new-constraint old-constraint)
  (and (not (eq (constraint-negated-p new-constraint)
                (constraint-negated-p old-constraint)))
       (funcall *matching-constraint-test* new-constraint old-constraint)))

(defun find-constraint-clash (new-constraint unexpanded-exists-constraints)
  (loop with new-negated-p = (constraint-negated-p new-constraint)
        for old-constraint in unexpanded-exists-constraints do
        (when (and (not (eq new-negated-p (constraint-negated-p old-constraint)))
                   (funcall *matching-constraint-test* new-constraint old-constraint))
          (return old-constraint))))

(defun constraint-clash-p (old-constraint new-constraint new-negated-p)
  (when (and (not (eq new-negated-p (constraint-negated-p old-constraint)))
             (funcall *matching-constraint-test* new-constraint old-constraint))
    old-constraint))

(defvar *clash-reasons*)
(defvar *interesting-clash-candidates*)
(defvar *clash-culprits*)

(defun set-clash-reasons (reasons)
  (when (boundp '*clash-reasons*)
    (push reasons *clash-reasons*))
  (when (and *use-dependency-based-instance-retrieval* (boundp '*clash-culprits*))
    (let ((interesting-clash-candidates *interesting-clash-candidates*))
      #+:debug (assert (and (not (null reasons)) (not (equal '(nil) reasons))))
      (setf *clash-culprits* 
            (union *clash-culprits*
                   (involved-clash-culprits interesting-clash-candidates reasons)
                   :test #'constraint-equal-test))
      #+:debug
      (when (and *clash-culprits-required-p* (null *clash-culprits*))
        (cerror "ignore" "Potential error for ~S and ~S" reasons interesting-clash-candidates))
      ;(break "~S" *clash-culprits*)
      )))

(defun set-signature-clash-reasons (reasons clash-dependencies)
  (when (and (consp *signature-clash-reasons*)
             (loop for constraint in clash-dependencies
                   thereis (and (concept-constraint-p constraint)
                                (constraint-signature constraint))))
    (setf (first *signature-clash-reasons*) reasons)
    ;(break)
    ))
#|
(defun refine-signature-clash-dependencies (reasons clash-dependencies)
  (let ((clashing-concepts
         (concept-set-remove-duplicates
          (nconc (loop for constraint in reasons
                       when (concept-constraint-p constraint)
                       collect (constraint-concept constraint))
                 #|(loop for all-reasons = reasons then new-reasons
                       for new-reasons = 
                       (loop for constraint in all-reasons
                             for concept = (when (concept-constraint-p constraint)
                                             (constraint-concept constraint))
                             for type = (when concept
                                          (type-of concept))
                             when (and type (member type '(atomic-concept negated-concept and-concept or-concept)))
                             nconc (loop for dependency in (constraint-dependencies constraint)
                                         unless (or (eq dependency constraint)
                                                    (not (concept-constraint-p dependency))
                                                    (not (atomic-constraint-p dependency)))
                                         collect dependency))
                       while new-reasons
                       nconc (loop for constraint in new-reasons
                                   append (concept-told-subsumers (constraint-concept constraint)))
                       into result
                       finally (return (nconc (loop for constraint in reasons
                                                    when (or-constraint-p constraint)
                                                    collect (constraint-concept constraint))
                                              result)))|#))))
    (if clashing-concepts
        (constraint-set-remove-duplicates
         (loop for dependency in clash-dependencies
               for signature = (when (and (concept-constraint-p dependency)
                                          (exists-constraint-p dependency))
                                 (constraint-signature dependency))
               for concept-dependencies = (when signature
                                            (signature-concept-dependencies signature))
               for (refined-p refined-dependencies) = 
               (multiple-value-list
                (if concept-dependencies
                    (refine-signature-dependency concept-dependencies clashing-concepts)
                  (values nil nil)))
               if refined-p
               nconc refined-dependencies
               and do (princ "+") ;(break)
               else collect dependency))
      clash-dependencies)))
|#

(defun refine-signature-clash-dependencies (clashing-concept-1
                                            clashing-concept-2
                                            culprit-1
                                            culprit-2
                                            clash-dependencies-1
                                            clash-dependencies-2
                                            &optional
                                            (extra-dependencies nil))
  (declare (ignore culprit-1 culprit-2))
  (constraint-set-remove-duplicates
   (if clashing-concept-1
       (loop for clashing-concept = clashing-concept-1 then clashing-concept-2
             for clash-dependencies = clash-dependencies-1 then clash-dependencies-2
             while clashing-concept
             append 
             (union-dependencies (when clash-dependencies
                                   (refine-signature-clash-dependencies-1 clashing-concept clash-dependencies))
                                 (when extra-dependencies 
                                   (refine-signature-clash-dependencies-1 clashing-concept extra-dependencies)))
             until (eq clashing-concept clashing-concept-2))
     (union-dependencies clash-dependencies-1
                         (union-dependencies clash-dependencies-2 extra-dependencies)))))

(defun refine-signature-clash-dependencies-1 (clashing-concept clash-dependencies)
  (loop for dependency in clash-dependencies
        for signature = (if (and (concept-constraint-p dependency)
                                 (or (exists-constraint-p dependency)
                                     (true-old-individual-p (constraint-ind dependency))))
                            (constraint-signature dependency)
                          (when (qualified-role-signature-p dependency)
                            dependency))
        for concept-dependencies = (when signature
                                     (signature-concept-dependencies signature))
        for (refined-p refined-dependencies) = 
        (multiple-value-list
         (if concept-dependencies
             (refine-signature-dependency concept-dependencies clashing-concept)
           (values nil nil)))
        if refined-p
        append refined-dependencies
        else collect dependency))

(defun refine-signature-dependency (concept-dependencies clashing-concept)
  (loop with top = *top-concept*
        with refined-p = nil
        with clashing-concepts = (if (or-concept-p clashing-concept)
                                     (list clashing-concept (concept-term clashing-concept))
                                   (list clashing-concept))
        for (concept . dependencies) in concept-dependencies
        when (clash-contained-p clashing-concepts concept)
        unless refined-p
        do (setf refined-p t)
        and
        #+:debug append #-:debug nconc (cdr (assoc top concept-dependencies)) into result
        end
        and
        append dependencies into result
        end
        finally 
        (return (values refined-p result))))

(defun clash-contained-p (clashing-concepts signature-concept)
  (loop for clashing-concept in clashing-concepts
        thereis
        (or (eq clashing-concept signature-concept)
            (if (negated-concept-p clashing-concept)
                (when (member (concept-term clashing-concept) (concept-told-disjoints signature-concept))
                  ;(break "1")
                  t)
              (when (member clashing-concept (concept-told-subsumers signature-concept))
                ;(break "2")
                t)))))

(defun handle-clash (culprit-1
                     clash-dependencies-1
                     relation-constraints
                     &optional
                     (culprit-2 nil)
                     (clash-dependencies-2 nil)
                     (extra-clash-dependencies nil))
  "Setup various variables in case of a primitive clash"
  (when-statistics
    (incf-statistics *number-of-clashes*))
  (let ((clash-reasons
         (if culprit-2 
             (list culprit-1 culprit-2)
           (list culprit-1)))
        (clash-dependencies
         (union-dependencies clash-dependencies-1 
                             (union-dependencies clash-dependencies-2 extra-clash-dependencies))))
    (set-signature-clash-reasons clash-reasons clash-dependencies)
    (set-clash-reasons (append clash-reasons (list relation-constraints)))
    (if *use-refined-signature-clash-dependencies*
        (set-clash-dependencies
         (refine-signature-clash-dependencies (when (concept-constraint-p culprit-1)
                                                (constraint-concept culprit-1))
                                              (when (and culprit-2 (concept-constraint-p culprit-2))
                                                (constraint-concept culprit-2))
                                              culprit-1
                                              culprit-2
                                              clash-dependencies-1
                                              clash-dependencies-2
                                              extra-clash-dependencies))
      (set-clash-dependencies clash-dependencies))
    (setf clash-dependencies *catching-clash-dependencies*)
    
    (when-debug t
      (let ((signature-clash-reasons
             (when (consp *signature-clash-reasons*)
               (copy-list (first *signature-clash-reasons*))))
            (collected-dependencies
             (when (boundp '*collected-dependencies*)
               (copy-list *collected-dependencies*))))
        (if culprit-2
            (race-trace ("~&Dep = ~S, (first *signature-clash-reasons*)=~S, coll-dep=~S~%"
                         clash-dependencies
                         signature-clash-reasons
                         collected-dependencies)
                        ("~&Clash between ~S and ~S detected.~%"
                         culprit-1 culprit-2))
          (race-trace ("~&Dep = ~S, (first *signature-clash-reasons*)=~S, coll-dep=~S~%"
                       clash-dependencies
                       signature-clash-reasons
                       collected-dependencies)
                      ("~&Clash for ~S detected.~%" culprit-1)))))
  ;(break)
    t))

(defun handle-cd-clash (added-constraint
                           clash-constraints
                           cd-clash-constraints
                           clash-dependencies
                           relation-constraints)
  (when-statistics
    (incf-statistics *number-of-clashes*))
  (set-clash-reasons (append (cons added-constraint clash-constraints)
                             cd-clash-constraints
                             relation-constraints))
  (set-clash-dependencies clash-dependencies)
  (race-trace ("~&Dep = ~S~%" clash-dependencies)
              ("~&CD clash between ~S and ~S detected due to CD constraints ~S.~%"
               added-constraint clash-constraints cd-clash-constraints))
  ;(break)
  t)

(defun involved-clash-culprits (interesting-clash-candidates clash-reasons)
  (when clash-reasons
    (involved-clash-culprits-1 interesting-clash-candidates
                               clash-reasons
                               nil
                               (make-constraints-table nil))))

(defun involved-clash-culprits-1 (interesting-clash-candidates clash-reasons culprits visited)
  (if clash-reasons
    (let ((clash-culprit (first clash-reasons)))
      ;(when (qualified-role-signature-p clash-culprit) (break))
      ;(race-trace ("interesting-clash-candidates=~S clash-reasons=~S culprits=~S visited=~S"
      ;             interesting-clash-candidates clash-reasons culprits (copy-hash-table visited)))
      (if (concept-constraint-p clash-culprit)
        (if (constraint-or-synonym-found-p clash-culprit visited)
          (involved-clash-culprits-1 interesting-clash-candidates
                                     (rest clash-reasons)
                                     culprits
                                     visited)
          (let ((matching-clash-candidate
                 (find clash-culprit interesting-clash-candidates
                       :test #'constraint-equal-synonym-test)))
            ;;; match can be different but compatible due to non-UNA merged constrains
            (add-to-constraints-table clash-culprit visited)
            (if matching-clash-candidate
              (involved-clash-culprits-1 interesting-clash-candidates
                                         (rest clash-reasons)
                                         (adjoin matching-clash-candidate culprits)
                                         visited)
              (let* ((dependencies-1 (when (constraint-signature clash-culprit)
                                       (signature-dependencies
                                        (constraint-signature clash-culprit))))
                     (dependencies-2 (constraint-dependencies clash-culprit))
                     (dependencies-3 (if (and dependencies-1 dependencies-2)
                                       (append dependencies-1 dependencies-2)
                                       (or dependencies-1 dependencies-2))))
                
                (involved-clash-culprits-1 interesting-clash-candidates
                                           (if dependencies-3
                                             (append dependencies-3 (rest clash-reasons))
                                             (rest clash-reasons))
                                           culprits
                                           visited)))))
        (let ((dependencies
               (cond ((qualified-role-signature-p clash-culprit)
                      (signature-dependencies clash-culprit))
                     ((relation-constraint-p clash-culprit)
                      (when (constraint-signature clash-culprit)
                        (signature-dependencies (constraint-signature clash-culprit))))
                     (t nil))))
          (involved-clash-culprits-1 interesting-clash-candidates
                                     (if dependencies
                                       (append dependencies (rest clash-reasons))
                                       (rest clash-reasons))
                                     culprits
                                     visited))))
    culprits))

(defun alc-find-matching-constraint (new-constraint expanded-concept-constraints all-inds-eql-p)
  (loop with new-ind = (constraint-ind new-constraint)
        with new-concept = (constraint-term new-constraint)
        for constraint in expanded-concept-constraints do
        (when (and (eq new-concept (constraint-term constraint))
                   (or all-inds-eql-p (eql new-ind (constraint-ind constraint))))
          (return constraint))))

(defun alcn-find-matching-constraint (new-constraint expanded-concept-constraints all-inds-eql-p)
  (let* ((new-term (constraint-term new-constraint))
         (new-exists-term-p (exists-concept-p new-term)))
    (if new-exists-term-p
      (loop with new-ind = (constraint-ind new-constraint)
            with new-concept = (and new-exists-term-p
                                    (if (constraint-negated-p new-constraint)
                                      (concept-term new-term)
                                      (true-concept-term new-term)))
            with new-role = (and new-exists-term-p (concept-role new-term))
            with new-role-has-ancestors-p = (and new-exists-term-p
                                                 (role-has-ancestors-p new-role))
            with new-role-ancestors = (and new-role-has-ancestors-p
                                           (role-ancestors-internal new-role))
            with new-number-restriction = (and new-exists-term-p
                                               (concept-number-restriction new-term))
            with top-concept = *top-concept*
            with use-tbox = *use-tbox*
            for old-constraint in expanded-concept-constraints do
            (when (or all-inds-eql-p (eql new-ind (constraint-ind old-constraint)))
              (let ((old-term (constraint-term old-constraint)))
                (when (or (eq new-term old-term)
                          (and new-exists-term-p (exists-concept-p old-term)
                               (let* ((old-role (concept-role old-term))
                                      (old-concept (if (constraint-negated-p old-constraint)
                                                     (concept-term old-term)
                                                     (true-concept-term old-term)))
                                      (roles-eq-p (eq new-role old-role))
                                      (new-role-superrole-p
                                       (and (not roles-eq-p)
                                            use-tbox
                                            (role-has-ancestors-p old-role)
                                            (member new-role (role-ancestors-internal old-role)))))
                                 (cond
                                  (roles-eq-p
                                   (if (constraint-negated-p old-constraint)
                                     (and (>= new-number-restriction
                                              (concept-number-restriction old-term))
                                          (or (eq old-concept top-concept)
                                              (eq new-concept old-concept)))
                                     (and (<= new-number-restriction
                                              (concept-number-restriction old-term))
                                          (or (eq new-concept top-concept)
                                              (eq new-concept old-concept)))))
                                  (new-role-superrole-p
                                   (unless (constraint-negated-p old-constraint)
                                     (and (<= new-number-restriction
                                              (concept-number-restriction old-term))
                                          (or (eq new-concept top-concept)
                                              (eq new-concept old-concept)))))
                                  ((and *use-tbox*
                                        new-role-has-ancestors-p
                                        (member old-role new-role-ancestors))
                                   (and (constraint-negated-p old-constraint)
                                        (>= new-number-restriction
                                            (concept-number-restriction old-term))
                                        (or (eq old-concept top-concept)
                                            (eq new-concept old-concept))))
                                  (t nil)))))
                  (return old-constraint)))))
      (loop with new-ind = (constraint-ind new-constraint)
            for constraint in expanded-concept-constraints do
            (when (and (eq new-term (constraint-term constraint))
                       (or all-inds-eql-p
                           (eql new-ind (constraint-ind constraint))))
              (return constraint))))))

(defun find-matching-constraint-in-expanded-store (new-constraint expanded-store state)
  (loop with ind = (constraint-ind new-constraint)
        with find-matching-constraint = *find-matching-constraint*
        with expanded-store-index = (state-expanded-store-index state)
        for store in (if expanded-store-index
			 (get-exp-index-entry ind expanded-store expanded-store-index)
		       expanded-store)
        for table = (cs-table store)
        for expanded-ind-concept-constraints = (get-constraint-store-entry ind table)
        do
        (when expanded-ind-concept-constraints
          (let ((constraint (funcall find-matching-constraint
                                     new-constraint expanded-ind-concept-constraints t)))
            (when constraint
              (return constraint))))))

(defun clash-in-cs-p (new-constraint expanded-concept-constraints state use-expanded-store-p)
  "Find a constraint that is either a clash or identical to new-constraint. 
   Returns t in case of a clash, nil if an identical constraint was found, 
   new-constraint otherwise. The list of expanded concept constraints is 
   assumed to be consistent.
   Returns second value in case of duplicate: T => new replaces old, nil => ignore new"
  (when-statistics
    (incf-statistics *number-of-clash-tests*))
  (let ((relation-constraints (state-relation-constraints state)))
    (if (eq (constraint-term new-constraint) *bottom-concept*)
      (handle-clash new-constraint
                    (constraint-or-dependencies new-constraint)
                    relation-constraints)
      (let* ((expanded-store (state-expanded-store state))
             (unexpanded-disjunctive-constraints (state-unexpanded-disjunctive-constraints state))
             (unexpanded-exists-constraints (state-unexpanded-exists-constraints state))
             (unexpanded-exists-constraints-store (state-unexpanded-exists-constraints-store state))
             (matched-constraint-1
              (when (or expanded-concept-constraints (consp expanded-store))
                (or (funcall *find-matching-constraint* new-constraint expanded-concept-constraints nil)
                    (and expanded-store
                         use-expanded-store-p
                         (not (constraint-store-unused-p expanded-store))
                         (find-matching-constraint-in-expanded-store new-constraint
								     expanded-store
								     state)))))
             (matched-constraint-2
              (or (when (and unexpanded-exists-constraints
                             (restrict-constraint-p new-constraint))
                    (if (and (consp unexpanded-exists-constraints-store)
                             (not (constraint-store-unused-p unexpanded-exists-constraints-store)))
                      (find-if-selected-constraints (constraint-ind new-constraint)
                                                    (lambda (old-constraint)
                                                      (constraint-clash-p old-constraint
                                                                          new-constraint
                                                                          (constraint-negated-p
                                                                           new-constraint)))
                                                    unexpanded-exists-constraints
                                                    unexpanded-exists-constraints-store)
                      (find-constraint-clash new-constraint unexpanded-exists-constraints)))
                  (when (and unexpanded-disjunctive-constraints
                             (and-constraint-p new-constraint))
                    (find-simple-constraint-clash new-constraint
                                                  unexpanded-disjunctive-constraints)))))
        (cond
         ((null (or matched-constraint-1 matched-constraint-2))
          (when-statistics
            (incf-statistics *number-of-new-constraints-during-clash-test*))
          new-constraint)
         ((and matched-constraint-1 (null matched-constraint-2))
          (if (eq (constraint-negated-p new-constraint)
                  (constraint-negated-p matched-constraint-1))
              (let ((replace-duplicate nil))
                (when-statistics
                  (incf-statistics *number-of-duplicates-during-clash-test*))
                (let ((synonyms (constraint-ind-synonyms new-constraint)))
                  (when synonyms
                    (setf (constraint-ind-synonyms new-constraint)
                          (stable-union synonyms (constraint-ind-synonyms matched-constraint-1)))
                    (setf replace-duplicate matched-constraint-1))
                  (when (and *model-merging*
                             (or *use-alternate-models* *use-alternate-ind-models*))
                    (setf (constraint-derived-from matched-constraint-1)
                          (union (constraint-derived-from new-constraint)
                                 (constraint-derived-from matched-constraint-1)))))
                (when (and (null (constraint-or-dependencies new-constraint))
                           (constraint-or-dependencies matched-constraint-1)
                           (eq (constraint-term matched-constraint-1)
                               (constraint-term new-constraint)))
                  (race-trace ("Removing or-dependencies of old constraint ~S ~
                              due to duplicate ~S. Old dep= ~S, new dep=NIL"
                               matched-constraint-1
                               new-constraint
                               (constraint-or-dependencies matched-constraint-1)))
                  (setf (constraint-or-dependencies matched-constraint-1) nil))
                (values nil replace-duplicate))
            (handle-clash new-constraint
                          (constraint-or-dependencies new-constraint)
                          relation-constraints
                          matched-constraint-1
                          (constraint-or-dependencies matched-constraint-1))))
         ((and (null matched-constraint-1) matched-constraint-2)
          (handle-clash new-constraint
                        (constraint-or-dependencies new-constraint)
                        relation-constraints
                        matched-constraint-2
                        (constraint-or-dependencies matched-constraint-2)))
         (t (handle-clash matched-constraint-1
                          (constraint-or-dependencies new-constraint)
                          relation-constraints
                          matched-constraint-2
                          (constraint-or-dependencies matched-constraint-2))))))))

(defun clash-in-relation-constraints-p (abox relation-constraints relation-store)
  (if *use-relation-store*
    (clash-in-relation-store-p abox relation-store)
    (clash-in-relation-constraints-p-no-relation-store abox relation-constraints)))

(defun clash-in-relation-constraints-p-no-relation-store (abox relation-constraints)
  (or (loop with use-unique-name-assumption = *use-unique-name-assumption*
            with ind-table = (when use-unique-name-assumption
                               (racer-make-hash-table))
            for relation-constraint in relation-constraints
            for ind-1 = (constraint-ind-1 relation-constraint)
            for ind-2 = (constraint-ind-2 relation-constraint)
            for role = (constraint-term relation-constraint)
            do
            (when (clash-in-relation-constraint-p-1 abox
                                                    ind-1
                                                    ind-2
                                                    (when (true-old-individual-p ind-2)
                                                      (find-individual abox ind-2))
                                                    role)
              (return t))
            (when use-unique-name-assumption
              (let* ((ind-1-feature-set (gethash ind-1 ind-table))
                     (old-entry (find role ind-1-feature-set :key #'constraint-term)))
                (if old-entry
                    (progn
                      (handle-clash relation-constraint
                                    (compute-constraint-or-dependencies relation-constraint)
                                    relation-constraints
                                    old-entry
                                    (compute-constraint-or-dependencies old-entry))
                      (return t))
                  (when (role-feature-p role)
                    (pushnew relation-constraint (gethash ind-1 ind-table)))))))
      (disjoint-roles-clash-in-relation-constraints-p abox relation-constraints)
      (asymmetric-role-clash-in-relation-constraints-p abox relation-constraints)))

(defun same-as-clash-in-abox-individuals-p (abox)
  (if (or *use-unique-name-assumption*
          (abox-current-una-assumption abox))
      (loop for ind in (abox-individuals-list abox)
            thereis (rest (individual-name-set ind)))
    nil))

(defun clash-in-relation-constraint-p (abox relation-constraint)
  (let ((ind-name-2 (constraint-ind-2 relation-constraint)))
    (clash-in-relation-constraint-p-1 abox
                                      (constraint-ind-1 relation-constraint)
                                      (constraint-ind-2 relation-constraint)
                                      (when (true-old-individual-p ind-name-2)
                                        (find-individual abox ind-name-2))
                                      (constraint-term relation-constraint))))

(defun clash-in-relation-constraint-p-1 (abox ind-1-name ind-2-name ind-2 role)
  (or (is-bottom-object-role-p role)
      (is-bottom-datatype-role-p role)
      (and ind-2
           (role-datatype role)
           (not (or (individual-attribute-assertions ind-2)
                    (and (individual-encoded-axioms ind-2)
                         (member-if #'cd-concept-p (individual-encoded-axioms ind-2)
                                    :key #'second)))))
      (and (eql ind-1-name ind-2-name)
           (or (role-asymmetric-p role)
               (role-irreflexive-p role)
               (and (dl-disjoint-roles (abox-language abox))
                    (dl-reflexive-roles (abox-language abox))
                    (member-if (lambda (roles)
                                 (some #'role-reflexive-p roles))
                               (role-ancestors-internal role)
                               :key #'role-disjoint-roles))))))

(defun disjoint-roles-clash-in-relation-constraints-p (abox relation-constraints)
  (when (dl-disjoint-roles (abox-language abox))
    (let ((table (smart-clrhash *racer-remove-constraint-duplicates-table*
                                relation-constraints
                                '*racer-remove-constraint-duplicates-table*)))
      (loop for constraint in relation-constraints
            for role = (constraint-term constraint)
            for ind-1 = (constraint-ind-1 constraint)
            for ind-2 = (constraint-ind-2 constraint)
            do
            (loop for ancestor in (role-ancestors-internal role)
                  when (role-disjoint-roles ancestor) do
                  (push constraint (gethash (list ind-1 ind-2) table))))
      (let ((same-ind-pair-constraints
             (loop for same-pair-list being the hash-value of table
                   when (rest same-pair-list)
                   collect same-pair-list)))
        (when same-ind-pair-constraints
          (loop with tbox = (abox-tbox abox)
                for constraint-set in same-ind-pair-constraints
                for all-roles = (mapcar #'constraint-term constraint-set)
                thereis
                (or (disjoint-roles-in-role-conjunction-clash-p tbox all-roles)
                    (loop for constraint in constraint-set
                          thereis
                          (loop for ancestor in (role-ancestors-internal (constraint-term constraint))
                                thereis
                                (not (role-set-disjoint-p (role-disjoint-roles ancestor)
                                                          all-roles)))))))))))

(defun asymmetric-role-clash-in-relation-constraints-p (abox relation-constraints)
  (when (dl-asymmetric-roles (or (and abox (abox-language abox)) *dl-prover-language*))
    (let ((table (smart-clrhash *racer-remove-constraint-duplicates-table*
                                relation-constraints
                                '*racer-remove-constraint-duplicates-table*))) 
      (loop for constraint in relation-constraints do
            (when (some #'role-asymmetric-p (role-ancestors-internal (constraint-term constraint)))
              (push constraint
                    (gethash (list (constraint-ind-1 constraint) (constraint-ind-2 constraint)) table))))
      (loop with culprit-1 = nil
            with culprit-2 = nil
            for same-pair-list being the hash-value of table
            for first-elem = (first same-pair-list)
            for inverse-matches = (gethash (list (constraint-ind-2 first-elem)
                                                 (constraint-ind-1 first-elem))
                                           table)
            when (and inverse-matches
                      (loop for constraint-1 in same-pair-list
                            for ancestors-1 = (role-ancestors-internal (constraint-term constraint-1))
                            thereis
                            (loop for constraint-2 in inverse-matches
                                  when (role-set-intersection-p #'role-asymmetric-p
                                                                ancestors-1
                                                                (role-ancestors-internal
                                                                 (constraint-term constraint-2)))
                                  do
                                  (setf culprit-1 constraint-1)
                                  (setf culprit-2 constraint-2)
                                  (return t))))
            do (return (values t culprit-1 culprit-2))))))

(defun disjoint-roles-clash-in-feature-exists-constraints-p (tbox feature-constraints)
  (when (dl-disjoint-roles (tbox-language tbox))
    (loop with all-roles-with-disjoint-ancestors = nil
          with all-disjoints = nil
          for constraint in feature-constraints do
          (loop for ancestor in (role-ancestors-internal
                                 (concept-role (constraint-term constraint)))
                for disjoints = (role-disjoint-roles ancestor)
                when disjoints do
                (setf all-disjoints (append disjoints all-disjoints))
                (push ancestor all-roles-with-disjoint-ancestors))
          finally
          (unless (role-set-disjoint-p all-roles-with-disjoint-ancestors all-disjoints)
            (return t)))))

(defun disjoint-roles-clash-in-feature-exists-concepts-p (tbox feature-concepts)
  (when (dl-disjoint-roles (tbox-language tbox))
    (loop with all-roles-with-disjoint-ancestors = nil
          with all-disjoints = nil
          for concept in feature-concepts do
          (loop for ancestor in (role-ancestors-internal (concept-role concept))
                for disjoints = (role-disjoint-roles ancestor)
                when disjoints do
                (setf all-disjoints (append disjoints all-disjoints))
                (push ancestor all-roles-with-disjoint-ancestors))
          finally
          (unless (role-set-disjoint-p all-roles-with-disjoint-ancestors all-disjoints)
            (return t)))))

(defun disjoint-roles-in-role-conjunction-clash-p (tbox role-conjunction)
  (when (dl-disjoint-roles (tbox-language tbox))
    (loop with all-roles-with-disjoint-ancestors = nil
          with all-disjoints = nil
          for role in role-conjunction do
          (loop for ancestor in (role-ancestors-internal role)
                for disjoints = (role-disjoint-roles ancestor)
                when disjoints do
                (setf all-disjoints (append disjoints all-disjoints))
                (push ancestor all-roles-with-disjoint-ancestors))
          finally
          (unless (role-set-disjoint-p all-roles-with-disjoint-ancestors all-disjoints)
            (return t)))))
