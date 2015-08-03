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

(defun role-successor-satisfiable-p (role-list concept-list successor-ind unused-1 unused-2)
  #+:allegro (declare (:explain :tailmerging))
  (declare (ignore unused-1 unused-2))
  (with-race-trace-sublevel ("role-successor-satisfiable-p"
                             :expanded t
                             :arguments (list role-list
                                              concept-list
                                              successor-ind
                                              *saved-ind*
                                              *saved-relation-constraints*
                                              *saved-state*)
                             :trace-result t)
    (let* ((flat-concept-list (reduce #'append concept-list :initial-value nil))
           (new-concept `(and . ,flat-concept-list))
           (qualification (with-flatten-encodings
                            (encode-concept-term new-concept)))
           (roles (mapcar #'(lambda (srole)
                              (get-role (srole-name srole)))
                          role-list))
           (role (if (rest roles)
                     (make-nary-and-role roles)
                   (first roles)))
           (predecessor-ind (first *saved-ind*))
           (state (first *saved-state*))
           (params (state-parameters state))
           (bottom-concept (state-bottom-concept params)))
      (when-race-trace t
        (if successor-ind
            (race-trace ("~&Testing ~S-successor ~S of ~S for qualification ~S~%"
                         role successor-ind predecessor-ind new-concept))
          (race-trace ("~&Testing ~S-successor of ~S for qualification ~S~%"
                       role predecessor-ind new-concept))))
      (if (eq qualification bottom-concept)
          nil
        (let ((relation-constraints (first *saved-relation-constraints*))
              (all-relation-constraints (state-relation-constraints state))
              (top-concept (state-top-concept params))
              (relation-store (state-relation-store state)))
          (if (and (or all-relation-constraints
                       (and *use-relation-store*
                            (not (relation-store-empty-p relation-store))))
                   (null successor-ind)
                   (eq qualification top-concept)
                   (if (and *use-relation-store*
                            (not (relation-store-empty-p relation-store)))
                       (role-find-if-in-relation-store predecessor-ind role relation-store)
                     (role-find-if-in-relation-constraints predecessor-ind role all-relation-constraints)))
              t
            (let ((unexpanded-exists-constraints (state-unexpanded-exists-constraints state))
                  (attribute-constraints (state-attribute-constraints state))
                  (concrete-domain-state (state-concrete-domain-state state))
                  (labels (state-labels state))
                  (indirectly-blocked-individuals (state-indirectly-blocked-individuals state))
                  (concept-constraint 
                   (if successor-ind
                       (new-concept-constraint successor-ind qualification)
                     (new-concept-constraint predecessor-ind
                                             (if (null flat-concept-list)
                                                 (encode-concept-term
                                                  (if (or (role-datatype role) (role-cd-attribute role))
                                                      `(d-at-least 1 ,role)
                                                    `(at-least 1 ,role)))
                                               (encode-concept-term
                                                (if (or (role-datatype role) (role-cd-attribute role))
                                                    `(d-some ,role ,qualification)
                                                  `(some ,role ,qualification)))))))
                  (relation-constraint (when successor-ind
                                         (list (new-relation-constraint predecessor-ind
                                                                        successor-ind
                                                                        role))))
                  (expanded-constraints (state-expanded-constraints state))
                  (unexpanded-exists-constraints-store
                   (state-unexpanded-exists-constraints-store state))
                  (new-state-1 (copy-signature-kernel-state state t)))
              (multiple-value-bind (remaining-exists-constraints new-unexpanded-exists-constraint-store)
                  (if successor-ind
                      (values unexpanded-exists-constraints
                              unexpanded-exists-constraints-store)
                    (if (or relation-constraints
                            (not (relation-store-empty-p relation-store)))
                        (remove-selected-constraints-from-constraint-store
                         (lambda (constraint)
                           (not (eql predecessor-ind (constraint-ind constraint))))
                         predecessor-ind
                         unexpanded-exists-constraints
                         unexpanded-exists-constraints-store
                         (state-copy-unexpanded-exists-constraints-store-p new-state-1)
                         new-state-1
                         #'reset-exists-copy)
                      (values nil nil)))
                (let ((dependencies (reduce #'union role-list
                                            :key #'srole-constraints :initial-value nil)))
                  (setf (constraint-dependencies concept-constraint) dependencies)
                  (when relation-constraint
                    (setf (constraint-dependencies (first relation-constraint)) dependencies)))
                (unless (and (eq remaining-exists-constraints unexpanded-exists-constraints)
                             (eq unexpanded-exists-constraints-store new-unexpanded-exists-constraint-store))
                  (race-trace ("~&Testing with reduced exists constraints ~S / ~S, all=~S / ~S~%"
                               remaining-exists-constraints unexpanded-exists-constraints
                               new-unexpanded-exists-constraint-store unexpanded-exists-constraints-store)))
                (let ((old-merging-partitions-caching *merging-partitions-caching*))
                  (flet ((role-successor-satisfiable-p-continuation (sat-p
                                                                     xstate
                                                                     ignore-1 ignore-2 ignore-3)
                           (declare (ignore xstate ignore-1 ignore-2 ignore-3))
                           (setf *merging-partitions-caching* old-merging-partitions-caching)
                           (if sat-p
                               t
                             (progn
                               #+:debug (assert (or (null *catching-clash-dependencies*)
                                                    (and (consp *collected-dependencies*)
                                                         (listp (first *collected-dependencies*)))))
                               (when (and *catching-clash-dependencies* (consp *collected-dependencies*))
                                 (setf (first *collected-dependencies*)
                                       (union-dependencies *catching-clash-dependencies*
                                                           (first *collected-dependencies*)))
                                 (race-trace ("~&New *collected-dependencies*=~S~%" *collected-dependencies*)))
                               nil))))
                    (let* ((new-state-2 (copy-to-basic-kernel-state new-state-1))
                           (new-partially-expanded-or-stack
                            (push-backtrack-stack #'role-successor-satisfiable-p-continuation
                                                  nil))
                           (new-state-3
                            (changed-kernel-state new-state-2
                                                  :unexpanded-exists-constraints
                                                  remaining-exists-constraints
                                                  :expanded-constraints expanded-constraints
                                                  :relation-constraints relation-constraints
                                                  :attribute-constraints attribute-constraints
                                                  :concrete-domain-state concrete-domain-state
                                                  :labels labels
                                                  :indirectly-blocked-individuals
                                                  indirectly-blocked-individuals
                                                  :unexpanded-exists-constraints-store
                                                  new-unexpanded-exists-constraint-store
                                                  :save-completion nil
                                                  :partially-expanded-or-stack
                                                  new-partially-expanded-or-stack)))
                      (setf *merging-partitions-caching* nil) ; do not cache intermediate results!!!
                      (with-race-trace-sublevel ("added-constraints-satisfiable"
                                                 :expanded nil
                                                 :arguments (list concept-constraint
                                                                  relation-constraint
                                                                  new-state-3)
                                                 :trace-result t)
                        (added-constraints-satisfiable concept-constraint
                                                       relation-constraint
                                                       new-state-3
                                                       nil ; do not expand role-domain
                                                       nil)))))))))))))

(defun role-find-if-in-relation-constraints (predecessor-ind role all-relation-constraints)
  (loop for rel-constraint in all-relation-constraints
        thereis 
        (and (eql predecessor-ind (constraint-ind-1 rel-constraint))
             (member role (role-ancestors-internal (constraint-term rel-constraint))))))

(defun compute-relation-role-list (signatures top-concept at-most-bounds)
  (loop with rel-list = nil
        for signature in signatures
        for role = (signature-role signature)
        for ind-name = (first (signature-successor-ind-set signature))
        do
        #+:debug (assert (null (rest (signature-successor-ind-set signature))))
        (when ind-name
          (loop with parents = (if (role-internal-conjunction-p role)
                                   (role-parents-internal role)
                                 (list role))
                for parent in parents
                for parent-name = (role-name parent)
                for old-srole = (find parent-name rel-list :key #'srole-name)
                do
                (if old-srole
                    (pushnew ind-name (srole-individuals old-srole))
                  (multiple-value-bind (value bound)
                      (get-at-most-bound parent
                                         (list top-concept)
                                         at-most-bounds)
                    (push (make-srole :name parent-name
                                      :at-most value
                                      :at-least nil
                                      :ancestors
                                      (mapcar #'role-name
                                              (true-role-ancestors parent))
                                      :descendants
                                      (mapcar #'role-name
                                              (true-role-descendants parent))
                                      :individuals (list ind-name)
                                      :constraints
                                      (when bound
                                        (list (bound-constraint bound))))
                          rel-list)))))
        finally (return rel-list)))

(defun compute-srole-list (signatures relation-role-list top-concept)
  (loop with at-least-list = nil
        with some-list = relation-role-list
        for signature in (remove-if #'signature-successor-ind-set signatures)
        for role = (signature-role signature)
        for role-name = (role-name (signature-role signature))
        for some-constraint = (find-if #'(lambda (constraint)
                                           (when (concept-constraint-p constraint)
                                             (let ((term (constraint-term constraint)))
                                               (and (not (constraint-negated-p constraint))
                                                    (exists-concept-p term)
                                                    (not (eq (concept-term term)
                                                             top-concept))))))
                                       (signature-dependencies signature))
        for some-no = (when some-constraint
                        (concept-number-restriction (constraint-term some-constraint)))
        for old-srole = (or (find role-name some-list :key #'srole-name)
                            (find role-name at-least-list :key #'srole-name))
        do
        (if some-constraint
            (if old-srole
                (progn
                  (pushnew (list some-no (concept-term (constraint-term some-constraint)))
                           (srole-qualified-at-least old-srole)
                           :test #'equal)
                  (pushnew some-constraint (srole-constraints old-srole)))
              (push (make-srole :name role-name
                                #|:at-most
                                      (get-at-most-bound (signature-role signature)
                                                         (list top-concept)
                                                         at-most-bounds)|#
                                :at-least nil
                                :ancestors
                                (mapcar #'role-name (true-role-ancestors role))
                                :descendants
                                (mapcar #'role-name (true-role-descendants role))
                                :qualified-at-least
                                (list
                                 (list some-no
                                       (concept-term
                                        (constraint-term some-constraint))))
                                :constraints (list some-constraint))
                    some-list))
          (if old-srole
              (progn
                (unless (srole-at-least old-srole)
                  (setf (srole-at-least old-srole) (signature-cardinality signature)))
                (setf (srole-all-qualifications old-srole)
                      (append (signature-concepts signature)
                              (srole-all-qualifications old-srole)))
                (setf (srole-constraints old-srole)
                      (constraint-set-union (signature-dependencies signature)
                                            (srole-constraints old-srole))))
            (push (make-srole :name role-name
                              #|:at-most
                                      (get-at-most-bound (signature-role signature)
                                                         (list top-concept)
                                                         at-most-bounds)|#
                              :at-least (signature-cardinality signature)
                              :ancestors
                              (mapcar #'role-name (true-role-ancestors role))
                              :descendants
                              (mapcar #'role-name (true-role-descendants role))
                              :all-qualifications (signature-concepts signature)
                              :constraints (signature-dependencies signature))
                  at-least-list)))
                ;(break)
        finally (return (append some-list at-least-list))))

(defun compute-at-most-role-list (at-most-bounds srole-list allowed-roles top-concept)
  (loop for at-most-bound in at-most-bounds
        for role = (bound-role at-most-bound)
        for bound = (bound-number at-most-bound)
        for role-name = (role-name role)
        for old-entry = (find role-name srole-list :key #'srole-name)
        if old-entry
        if (eq (bound-qualification at-most-bound) top-concept)
        when (null (srole-at-most old-entry))
        do (setf (srole-at-most old-entry) bound)
        end
        else
        do (pushnew (list (bound-number at-most-bound)
                          (bound-qualification at-most-bound))
                    (srole-qualified-at-most old-entry)
                    :test #'equal)
        end
        else
        if (eq (bound-qualification at-most-bound) top-concept)
        collect
        (make-srole :name role-name
                    :at-most bound
                    :ancestors (filter-role-set (true-role-ancestors role)
                                                allowed-roles)
                    :descendants (filter-role-set (true-role-descendants role)
                                                  allowed-roles)
                    :all-qualifications (list top-concept)
                    :constraints (list (bound-constraint at-most-bound)))
        else collect
        (make-srole :name role-name
                    :qualified-at-most (collect-at-most-bounds role at-most-bounds top-concept)
                    :ancestors (filter-role-set (true-role-ancestors role)
                                                allowed-roles)
                    :descendants (filter-role-set (true-role-descendants role)
                                                  allowed-roles)
                    :all-qualifications (list top-concept)
                    :constraints (list (bound-constraint at-most-bound)))))

(defun compute-remaining-role-list (allowed-roles all-roles some-ind state top-concept bottom-concept)
  (loop for role in allowed-roles
        unless (member (role-name role) all-roles :key #'srole-name)
        collect
        (let* ((concept (without-taxonomic-encoding (encode-concept-term `(at-least 1 ,role))))
               (constraint (new-concept-constraint some-ind concept)))
          (unless (or (some-constraint-p constraint) (eq concept bottom-concept))
            (error "some constraint expected, found: ~S" constraint))
          (let* ((some-all-constraints
                  (unless (eq concept bottom-concept)
                    (get-related-all-or-some-constraints (list constraint)
                                                         state)))
                 (all-constraints (second (first some-all-constraints)))
                 (concept-list (if (eq concept bottom-concept)
                                   (list bottom-concept)
                                 (and some-all-constraints
                                      (construct-label constraint all-constraints)))))
            (make-srole :name (role-name role)
                        :at-most nil
                        :at-least nil
                        :ancestors (filter-role-set (true-role-ancestors role)
                                                    allowed-roles)
                        :descendants (filter-role-set (true-role-descendants role)
                                                      allowed-roles)
                        :all-qualifications (or concept-list
                                                (list top-concept))
                        :constraints all-constraints)))))

(defun compute-saved-relation-constraints (relation-constraints some-ind allowed-roles)
  (loop for constraint in relation-constraints
        unless (and (eql (constraint-ind-1 constraint) some-ind)
                    (member (constraint-term constraint) allowed-roles))
        collect constraint))

(defun inequations-satisfied-p (signatures
                                allowed-roles
                                remaining-exists-constraints
                                relation-constraints
                                state)
  #+:allegro (declare (:explain :tailmerging))
  (let* ((expanded-constraints (state-expanded-constraints state))
         (attribute-constraints (state-attribute-constraints state))
         (concrete-domain-state (state-concrete-domain-state state))
         (labels (state-labels state))
         (indirectly-blocked-individuals (state-indirectly-blocked-individuals state))
         (some-ind (signature-ind (first signatures)))
         (at-most-bounds (state-at-most-bounds state))
         (params (state-parameters state))
         (top-concept (state-top-concept params))
         (bottom-concept (state-bottom-concept params))
         (relation-role-list
          (when (state-tbox params)
            (compute-relation-role-list signatures top-concept at-most-bounds)))
         (srole-list (compute-srole-list signatures relation-role-list top-concept))
         (at-most-role-list
          (compute-at-most-role-list at-most-bounds srole-list allowed-roles top-concept))
         (all-roles (append at-most-role-list srole-list))
         (remaining-role-list
          (compute-remaining-role-list allowed-roles all-roles some-ind state top-concept bottom-concept))
         (complete-role-list (append remaining-role-list all-roles))
         (satisfiable nil)
         (model-list nil))
    (push some-ind *saved-ind*)
    (push (compute-saved-relation-constraints relation-constraints some-ind allowed-roles)
          *saved-relation-constraints*)
    (push state *saved-state*)
    (loop for role in all-roles
          unless (member (srole-name role) at-most-bounds
                         :key (lambda (x) (role-name (bound-role x))))
          do (setf (srole-at-most role) nil))
    #+:debug
    (assert (every (lambda (x)
                     (and (member (srole-name x) (srole-ancestors x))
                          (member (srole-name x) (srole-descendants x))))
                   complete-role-list))
    (race-trace ("~&Simplex: testing satisfiability of role set ~S derived from signatures ~S~%"
                 complete-role-list signatures))
    (multiple-value-setq (satisfiable model-list)
        (let ((*simplex-number-of-variables* 0)
              (*simplex-number-of-equations* 0)
              (*simplex-number-of-iterations* 0))
          (multiple-value-bind (solvable model)
              (check-number-restrictions complete-role-list nil nil nil nil)
            (push-statistics (list *simplex-number-of-variables*
                                   *simplex-number-of-equations*
                                   *simplex-number-of-iterations*)
                             *simplex-statistics-list*)
            (pop *saved-ind*)
            (pop *saved-relation-constraints*)
            (pop *saved-state*)
            (if solvable
                (progn
                  (race-trace ("~&Simplex: role set satisfied: ~S, model=~S~%"
                               complete-role-list model))
                  (values t model))
              (progn
                (add-clash-dependencies (collect-dependencies (mapcar #'bound-constraint
                                                                      at-most-bounds)))
                (add-signature-dependencies signatures)
                (set-clash-reasons (nconc (reduce #'append
                                                  (mapcar #'signature-dependencies
                                                          signatures))
                                          (mapcar #'bound-constraint at-most-bounds)
                                          (list relation-constraints)))
                nil)))))
    (if satisfiable
        (let ((new-concept-constraints nil)
              (new-relation-constraints nil))
          (multiple-value-setq (new-concept-constraints new-relation-constraints)
              (generate-model-constraints some-ind model-list signatures at-most-bounds))
          (flet ((inequations-satisfied-p-continuation (sat-p
                                                        xstate
                                                        unused-1 unused-2 unused-3)
                   (declare (ignore unused-1 unused-2 unused-3))
                   (if sat-p
                       (progn
                         (race-trace ("~&Remaining exists constraints satisfied: ~S ~
                                     derived constraints= ~S ~S~%"
                                      remaining-exists-constraints
                                      new-concept-constraints
                                      new-relation-constraints))
                       ;(break)
                         (added-constraints-satisfiable nil nil xstate t nil))
                     (progn
                       (race-trace ("~&Remaining exists constraints failed: ~S~% failed rel-constraints ~S,~
                                     dep=~S~%"
                                    remaining-exists-constraints
                                    relation-constraints
                                    *catching-clash-dependencies*))
                       (setf (first *collected-dependencies*)
                             (union-dependencies *catching-clash-dependencies*
                                                 (first *collected-dependencies*)))
                       (handle-clash-with-backtrack-state state nil nil nil nil)))))
            (let* ((new-state-1 (copy-to-basic-kernel-state state))
                   (new-partially-expanded-or-stack
                    (push-backtrack-stack #'inequations-satisfied-p-continuation
                                          (state-partially-expanded-or-stack new-state-1)))
                   (new-state-2
                    (changed-kernel-state new-state-1
                                          :unexpanded-exists-constraints remaining-exists-constraints
                                          :expanded-constraints expanded-constraints
                                          :relation-constraints relation-constraints
                                          :attribute-constraints attribute-constraints
                                          :concrete-domain-state concrete-domain-state
                                          :labels labels
                                          :indirectly-blocked-individuals
                                          indirectly-blocked-individuals
                                          :partially-expanded-or-stack
                                          new-partially-expanded-or-stack)))
              (race-trace ("~&Testing satisfiability of transformed constraints ~S ~S, state=~S~%"
                           new-concept-constraints new-relation-constraints
                           (copy-basic-kernel-state-internal new-state-2)))
              (added-constraints-satisfiable new-concept-constraints
                                             new-relation-constraints
                                             new-state-2
                                             nil ;do not expand role-domain
                                             nil))))
      (handle-clash-with-backtrack-state state nil nil nil nil))))

(defun generate-model-constraints (predecessor-ind model-list signatures at-most-bounds)
  (loop with concept-constraints = nil
        with relation-constraints = nil
        with at-most-constraints = (mapcar #'bound-constraint at-most-bounds)
        for (role-list at-least-value concept-list individual-list) in model-list
        for successor-ind = (first individual-list)
        do
        #+:debug (assert (null (rest individual-list)))
        #+:debug (and successor-ind (eql at-least-value 1))
        ;#-:debug (declare (ignore at-least-value))
        (let* ((new-concept-list (reduce #'append concept-list :initial-value nil))
               (qualification (with-flatten-encodings
                                (encode-concept-term `(and ,@new-concept-list))))
               (roles (mapcar #'(lambda (srole)
                                  (get-role (srole-name srole)))
                              role-list))
               (role (if (rest roles)
                       (make-nary-and-role roles)
                       (first roles)))
               (dependencies
                (loop with result = nil
                      for signature in signatures do
                      (loop for dependency in (signature-dependencies signature) do
                            (setf result (union (constraint-derived-from dependency)
                                                result)))
                      finally (return result)))
               (concept-constraint
                (if successor-ind
                    (concluded-concept-constraint successor-ind
                                                  qualification
                                                  (first at-most-constraints)
                                                  (rest at-most-constraints))
                  (concluded-concept-constraint predecessor-ind
                                                (encode-concept-term
                                                 (if (or (role-datatype role) (role-cd-attribute role))
                                                     `(d-at-least ,at-least-value ,role ,qualification)
                                                   `(at-least ,at-least-value ,role ,qualification)))
                                                (first at-most-constraints)
                                                (rest at-most-constraints))))
               (relation-constraint (when successor-ind
                                      (let ((result
                                             (new-relation-constraint predecessor-ind
                                                                      successor-ind
                                                                      role)))
                                        (setf (constraint-dependencies result) at-most-constraints)
                                        result))))
          (setf (constraint-derived-from concept-constraint) dependencies)
          (push concept-constraint concept-constraints)
          (when relation-constraint
            (setf (constraint-derived-from relation-constraint) dependencies)
            (push relation-constraint relation-constraints)))
        finally (return (values concept-constraints relation-constraints))))

(defun collect-at-most-bounds (role bound-list top-concept)
  (loop for elem in bound-list
        when (and (subrole-p role (bound-role elem))
                  (not (eq (bound-qualification elem) top-concept)))
        collect (list (bound-number elem) (bound-qualification elem))))

(defun get-allowed-roles (at-most-bounds exists-constraints relation-constraints)
  (let* ((bound-subroles
          (loop with result = nil
                for bound in at-most-bounds
                do (setf result (union (remove-if #'role-internal-conjunction-p
                                                  (true-role-descendants (bound-role bound)))
                                       result))
                finally (return result)))
         (constraint-superroles
          (loop with result = nil
                for constraint in exists-constraints
                do (setf result (union (remove-if #'role-internal-conjunction-p
                                                  (true-role-ancestors
                                                   (concept-role
                                                    (constraint-term constraint))))
                                       result))
                finally (return result)))
         (all-superroles
          (loop with result = constraint-superroles
                for constraint in relation-constraints
                do (setf result (union (remove-if #'role-internal-conjunction-p
                                                  (true-role-ancestors
                                                   (constraint-term constraint)))
                                       result))
                finally (return result))))
    (intersection bound-subroles all-superroles)))

(defun true-role-ancestors (role)
  (adjoin role (remove-if #'role-internal-conjunction-p (role-ancestors-internal role))))

(defun true-role-descendants (role)
  (adjoin role (remove-if #'role-internal-conjunction-p (role-descendants-internal role))))

(defun filter-role-set (role-set allowed-roles)
  (loop for role in role-set
        when (member role allowed-roles)
      collect (role-name role)))

(defun compute-added-exists-constraints (ind remaining-exists-constraints allowed-roles)
  (loop for constraint in remaining-exists-constraints
      when (and (eql ind (constraint-ind constraint))
		(member (concept-role (constraint-term constraint)) allowed-roles))
      collect constraint))

(defun merge-constraints-simplex (ind state unused-1 unused-2 unused-3)
  #+:allegro (declare (:explain :tailmerging))
  (declare (ignore unused-1 unused-2 unused-3))
  (with-race-trace-sublevel ("merge-constraints-simplex"
                             :arguments (list ind state)
                             :expanded nil
                             :trace-result t)
    ;(break)
    (let* ((relation-constraints
            (if (and *use-relation-store*
                     (not (relation-store-empty-p (state-relation-store state))))
              (get-all-rel-constraints (state-relation-store state))
              (state-relation-constraints state)))
           (at-most-bounds (state-at-most-bounds state))
           (true-some-constraints (state-true-some-constraints state))
           (remaining-exists-constraints
            (get-all-expanded-constraints (state-unexpanded-exists-constraints state)
                                          (state-unexpanded-exists-constraints-store state)))
           (label (list* (construct-label-from-constraints (append true-some-constraints
                                                                   (mapcar #'bound-constraint
                                                                           at-most-bounds)))
                         (construct-label-from-constraints remaining-exists-constraints)
                         relation-constraints))
           (allowed-roles (get-allowed-roles at-most-bounds
                                             true-some-constraints
                                             relation-constraints))
           (added-exists-constraints
            (compute-added-exists-constraints ind remaining-exists-constraints allowed-roles))
           (related-exists-constraints (append added-exists-constraints true-some-constraints)))
      (if (member label *simplex-labels* :test #'equal)
        (progn
          (race-trace ("~&Blocking merge-constraints-simplex: label=~S, *simplex-labels*=~S~%"
                       label *simplex-labels*))
          ;(break)
          t)
        (progn
          (push label *simplex-labels*)
          (let ((signatures (create-signatures ind
                                               state
                                               related-exists-constraints
                                               relation-constraints
                                               (state-at-least-bounds state)
                                               at-most-bounds
                                               (state-relation-store state))))
            (if (signature-violated-p signatures at-most-bounds)
              (handle-clash-with-backtrack-state state nil nil nil nil)
              (multiple-value-bind
                (new-unexpanded-exists-constraints new-unexpanded-exists-constraint-store)
                (if added-exists-constraints
                  (remove-constraints-from-constraint-store added-exists-constraints
                                                            (state-unexpanded-exists-constraints state)
                                                            (state-unexpanded-exists-constraints-store state)
                                                            (state-copy-unexpanded-exists-constraints-store-p
                                                             state)
                                                            state
                                                            #'reset-exists-copy)
                  (values (state-unexpanded-exists-constraints state)
                          (state-unexpanded-exists-constraints-store state)))
                (when-debug added-exists-constraints
                  (race-trace ("~&Extending related exists-constraints by ~S~%"
                               added-exists-constraints)))
                (race-trace ("~&Starting with signatures ~S~%" signatures))
                (flet ((merge-constraints-simplex-continuation (sat-p
                                                                xstate
                                                                ignore-1
                                                                ignore-2
                                                                ignore-3)
                         (declare (ignore xstate ignore-1 ignore-2 ignore-3))
                         (pop *simplex-labels*)
                         (if sat-p
                           t
                           (handle-clash-with-backtrack-state state nil nil nil nil))))
                  (let* ((new-partially-expanded-or-stack
                          (push-backtrack-stack #'merge-constraints-simplex-continuation
                                                (state-partially-expanded-or-stack state)))
                         (new-state (changed-signature-state state
                                                             :unexpanded-exists-constraints
                                                             new-unexpanded-exists-constraints
                                                             :unexpanded-exists-constraints-store
                                                             new-unexpanded-exists-constraint-store
                                                             :partially-expanded-or-stack
                                                             new-partially-expanded-or-stack)))
                    (inequations-satisfied-p signatures
                                             allowed-roles
                                             new-unexpanded-exists-constraints
                                             relation-constraints
                                             new-state)))))))))))
