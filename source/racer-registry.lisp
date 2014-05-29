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

;;;
;;;
;;; 

(defconstant +server-case+
    
  ;;; per Copy&Past aus racer-server! 
  ;;; Stand: 12.12.2010
  
  ;;; 
  ;;; WICHTIG: ALLEGROGRAPH KEYWORD AUS DEM CASE ENTFERNEN!
  ;;; FEHLEN SONST, STUBS WERDEN JA MIT LISPWORKS GENERIERT!
  ;;; 
  
  `((time)
    (in-tbox
     (answer expr state
             stream n 
             (apply #'handle-in-tbox (rest expr))
             output-string-stream))
    (set-current-tbox
     (answer expr state
             stream n 
             (set-current-tbox (second expr))
             output-string-stream))
    (init-tbox 
     (answer expr state
             stream n 
             (apply #'init-tbox (rest expr))
             output-string-stream))
    ((delete-tbox forget-tbox)
     (answer expr state
             stream n 
             (forget-tbox (second expr))
             output-string-stream))
    (delete-all-tboxes
     (answer expr state
             stream n 
             (delete-all-tboxes)
             output-string-stream))
    (clear-default-tbox
     (answer expr state
             stream n 
             (clear-default-tbox)
             output-string-stream))
    (current-tbox
     (answer expr state
             stream n 
             (current-tbox)
             output-string-stream))
    (in-abox
     (answer expr state
             stream n 
             (apply #'handle-in-abox (rest expr))
             output-string-stream))
    (set-current-abox
     (answer expr state
             stream n 
             (set-current-abox (second expr))
             output-string-stream))
    (init-abox 
     (answer expr state
             stream n 
             (apply #'init-abox (rest expr))
             output-string-stream))
    (current-abox
     (answer expr state
             stream n 
             (current-abox)
             output-string-stream))
    ((delete-abox forget-abox)
     (answer expr state
             stream n 
             (forget-abox (second expr))
             output-string-stream))
    (delete-all-aboxes
     (answer expr state
             stream n 
             (delete-all-aboxes)
             output-string-stream))
    (in-knowledge-base 
     (let ((tbox-name (second expr))
           abox-name
           init
           (args (rest (rest expr))))
       (cond ((null args) 
              (setf abox-name tbox-name)
              (setf init t))
             ((null (rest args))
              (setf abox-name (first args))
              (setf init t))
             ((eq (first args) :init)
              (setf init (second args))
              (setf abox-name tbox-name))
             ((eq (second args) :init)
              (setf init (third args))
              (setf abox-name (first args)))
             (t 
              (error "Syntax error in (in-knowledge-base ~A~{ ~S~})" tbox-name args)))
       (answer expr state
               stream n 
               (list (handle-in-tbox tbox-name :init init)
                     (if init
                         (handle-in-abox abox-name
                                         tbox-name)
                       (handle-in-abox abox-name)))
               output-string-stream)))
    (ensure-tbox-signature
     (ensure-tbox-signature 
      (get-tbox (second expr))
      :atomic-concepts (getf (rest (rest expr)) ':atomic-concepts)
      :roles (getf (rest (rest expr)) ':roles)
      :transitive-roles (getf (rest (rest expr)) ':transitive-roles)
      :features (getf (rest (rest expr)) ':features)
      :attributes (getf (rest (rest expr)) ':attributes))
     (ok expr stream n state output-string-stream))
    (ensure-abox-signature 
     (ensure-abox-signature (get-abox (second expr))
                            :individuals (getf (rest (rest expr))
                                               ':individuals)
                            :objects (getf (rest (rest expr))
                                           ':objects))
     (ok expr stream n state output-string-stream))
    (signature 
     (let ((individuals (getf (rest expr) ':individuals))
           (atomic-concepts (getf (rest expr) ':atomic-concepts))
           (roles (getf (rest expr) ':roles))
           (transitive-roles (getf (rest expr) ':transitive-roles))
           (features (getf (rest expr) ':features))
           (attributes (getf (rest expr) ':attributes))
           (objects (getf (rest expr) ':objects)))
       (when (or atomic-concepts roles transitive-roles features attributes)
         (ensure-tbox-signature 
          (get-tbox)
          :atomic-concepts atomic-concepts
          :roles roles
          :transitive-roles transitive-roles
          :features features
          :attributes attributes))
       (when (or individuals objects)
         (ensure-abox-signature (get-abox)
                                :individuals individuals
                                :objects objects)))
     (ok expr stream n state output-string-stream))
    (implies
     (apply #'add-concept-axiom (get-tbox)
            (append (rest expr) '(:inclusion-p t)))
     (ok expr stream n state output-string-stream))
    (define-primitive-concept
     (if (eql (length (rest expr)) 1)
         (apply #'add-concept-axiom (get-tbox)
                (append (rest expr) '(top) '(:inclusion-p t)))
       (apply #'add-concept-axiom (get-tbox)
              (append (rest expr) '(:inclusion-p t))))
     (ok expr stream n state output-string-stream))
    (add-concept-axiom 
     (apply #'add-concept-axiom (find-tbox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))
    ((add-role-axiom add-role-axioms)
     (apply #'add-role-axioms (find-tbox (second expr)) 
            (rest (rest expr))) 
     (ok expr stream n state output-string-stream))
    ((equivalent define-concept)
     (apply #'add-concept-axiom (get-tbox)
            (append (rest expr) '(:inclusion-p nil)))
     (ok expr stream n state output-string-stream))
    ((add-disjointness-axiom disjoint)
     (let ((group (gensym)))
       (loop for name in (rest expr) do
             (add-disjointness-axiom (get-tbox) name group expr)))
     (ok expr stream n state output-string-stream))
    (define-disjoint-primitive-concept
     (loop for disjoint-list-sym in (third expr) do
           (add-disjointness-axiom (get-tbox)
                                   (second expr) disjoint-list-sym expr))
     (add-concept-axiom (get-tbox)
                        (second expr) (fourth expr) :inclusion-p t)
     (ok expr stream n state output-string-stream))
    (define-primitive-role
     (apply #'add-role-axioms (get-tbox) (rest expr))
     (ok expr stream n state output-string-stream))
    (define-datatype-property
     (apply #'add-datatype-property (get-tbox) (rest expr))
     (ok expr stream n state output-string-stream))
    (add-datatype-property
     (apply #'add-datatype-property (rest expr))
     (ok expr stream n state output-string-stream))
    (define-distinct-individual
     (add-individual (get-abox (third expr)) (second expr) t (or (fourth expr) +top-symbol+))
     (ok expr stream n state output-string-stream))
    (define-individual
     (add-individual (get-abox (third expr)) (second expr) nil (or (fourth expr) +top-symbol+))
     (ok expr stream n state output-string-stream))
    (define-primitive-attribute
     (apply #'add-role-axioms (get-tbox) (second expr) :feature t (rest (rest expr)))
     (ok expr stream n state output-string-stream))
    (define-concrete-domain-attribute 
     (let ((domain (getf (rest (rest expr)) ':domain nil))
           (type (getf (rest (rest expr)) ':type nil))
           (tbox (get-tbox)))
       (ensure-cd-attribute (second expr) domain type tbox))
     (ok expr stream n state output-string-stream))
    (state 
      (let ((*check-subscriptions-inhibited* t)) 
        (loop for expr1 in (rest expr) do
              (process-racer-expr expr1 stream n t output-string-stream)))
      (answer expr state
              stream n (check-subscriptions (get-abox))
              output-string-stream))
    (instance 
     (add-concept-assertion (get-abox) (second expr) (third expr))
     (ok expr stream n state output-string-stream))
    (add-concept-assertion 
     (apply #'add-concept-assertion (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))
    (related 
     (add-role-assertion (get-abox) (second expr) (third expr) (fourth expr))
     (ok expr stream n state output-string-stream))
    (unrelated 
     (add-role-assertion (get-abox) (second expr) (third expr) (fourth expr))
     (ok expr stream n state output-string-stream))
    (add-role-assertion 
     (apply #'add-role-assertion (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))
    (add-negated-role-assertion 
     (apply #'add-negated-role-assertion (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))
    (constrained
     (add-attribute-assertion (get-abox) (second expr) (third expr) (fourth expr))
     (ok expr stream n state output-string-stream))
    (add-attribute-assertion
     (add-attribute-assertion (get-abox (second expr)) (third expr) (fourth expr) (fifth expr))
     (ok expr stream n state output-string-stream))
    (constraints 
      (loop for expr1 in (rest expr) do
            (add-constraint-assertion (get-abox)
                                      expr1))
      (ok expr stream n state output-string-stream))
    (add-constraint-assertion 
     (add-constraint-assertion (get-abox (second expr)) (third expr))
     (ok expr stream n state output-string-stream))
    ((forget)
     (let ((tbox (getf (second expr) ':tbox *current-tbox*))
           (abox (getf (second expr) ':abox *current-abox*)))
       (forget-statement tbox abox (rest (rest expr))))
     (ok expr stream n state output-string-stream))
    (forget-concept-axiom
     (apply #'forget-concept-axiom (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))
    (forget-role-axioms 
     (apply #'forget-role-axioms (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))
    ((forget-statement)
     (apply #'forget-statement (rest expr))
     (ok expr stream n state output-string-stream))
    (forget-concept-assertion 
     (apply #'forget-concept-assertion (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))
    (forget-role-assertion 
     (apply #'forget-role-assertion (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))
    (forget-constrained-assertion 
     (apply #'forget-constrained-assertion (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))
    (forget-constraint 
     (apply #'forget-constraint (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))
    ((forget-disjointness-axiom)
     (forget-disjointness-axiom (get-tbox (second expr)) 
                                (third expr) (fourth expr)
                                (fifth expr))
     (ok expr stream n state output-string-stream))
    (forget-disjointness-axiom-statement
     (forget-disjointness-axiom-statement (get-tbox (second expr)) (third expr))
     (ok expr stream n state output-string-stream))
    (forget-individual
     (forget-individual (second expr) (get-abox (third expr)))
     (ok expr stream n state output-string-stream))
    ((concept-satisfiable? concept-satisfiable-p)
     (answer expr state
             stream n (concept-satisfiable-p (second expr) 
                                             (get-tbox (third expr)))
             output-string-stream))
    ((concept-subsumes? concept-subsumes-p)
     (answer expr state
             stream n (concept-subsumes-p (second expr) (third expr)
                                          (get-tbox (fourth expr)))
             output-string-stream))
    ((concept-equivalent? concept-equivalent-p)
     (answer expr state
             stream n (concept-equivalent-p (second expr) (third expr)
                                            (get-tbox (fourth expr)))
             output-string-stream))
    ((concept-disjoint? concept-disjoint-p)
     (answer expr state
             stream n (concept-disjoint-p (second expr) (third expr)
                                          (get-tbox (fourth expr)))
             output-string-stream))
    ((concept-p concept?) 
     (answer expr state
             stream n (concept-p (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((concept-is-primitive-p concept-is-primitive?)
     (answer expr state
             stream n (concept-is-primitive-p (second expr) 
                                              (get-tbox (third expr)))
             output-string-stream))
    (alc-concept-coherent 
     (answer expr state
             stream n (apply #'alc-concept-coherent (rest expr))
             output-string-stream))
    ((role-satisfiable? role-satisfiable-p)
     (answer expr state
             stream n (role-satisfiable-p (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((role-subsumes? role-subsumes-p)
     (answer expr state
             stream n (role-subsumes-p (second expr) (third expr) 
                                       (get-tbox (fourth expr)))
             output-string-stream))
    ((role-p role?) 
     (answer expr state
             stream n (role-p (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((transitive-p transitive?)
     (answer expr state
             stream n (transitive-p (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((feature-p feature?)
     (answer expr state
             stream n (feature-p (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((inverse-feature-p feature?)
     (answer expr state
             stream n (inverse-feature-p (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((cd-attribute-p cd-attribute?)
     (answer expr state
             stream n (cd-attribute-p (second expr) (get-tbox (third expr)))
             output-string-stream))
    (classify-tbox 
     (classify-tbox (get-tbox (second expr)))
     (ok expr stream n state output-string-stream))
    (check-tbox-coherence
     (answer expr state
             stream n (check-tbox-coherence (get-tbox (second expr)))
             output-string-stream))
    ((tbox-classified-p tbox-classified?)
     (answer expr state
             stream n (tbox-classified-p (get-tbox (second expr)))
             output-string-stream))
    ((tbox-coherent? tbox-coherent-p)
     (answer expr state
             stream n (tbox-coherent-p (get-tbox (second expr)))
             output-string-stream))
    ((tbox-prepared? tbox-prepared-p)
     (answer expr state
             stream n (tbox-prepared-p (get-tbox (second expr)))
             output-string-stream))
    (realize-abox  
     (realize-abox (get-abox (second expr)))
     (ok expr stream n state output-string-stream))
    ((abox-realized-p abox-realized?)
     (answer expr state
             stream n (abox-realized-p (get-abox (second expr)))
             output-string-stream))
    ((abox-consistent? abox-consistent-p)
     (answer expr state
             stream n (apply #'abox-consistent-p (rest expr))
             output-string-stream))
    ((abox-una-consistent? abox-una-consistent-p)
     (answer expr state
             stream n (apply #'abox-una-consistent-p (rest expr))
             output-string-stream))
    ((abox-prepared? abox-prepared-p)
     (answer expr state
             stream n (apply #'abox-prepared-p (rest expr))
             output-string-stream))
    (check-abox-coherence 
     (answer expr state
             stream n (check-abox-coherence 
                       (get-abox (second expr))
                       (if (third expr)
                           (if *unsafe-mode*
                               (third expr)
                             (error "Cannot save files. ~
                                                      Please start ~A with the option -u for unsafe mode ~
                                                      (do not forget -- under Windows)." 
                                    *dl-prover-name*))))
             output-string-stream))
    ((individual-p individual?)
     (answer expr state
             stream n
             (individual-p (second expr) (get-abox (third expr)))
             output-string-stream))
    ((individual-instance? individual-instance-p)
     (answer expr state
             stream n 
             (individual-instance-p (second expr) (third expr) 
                                    (get-abox (fourth expr)))
             output-string-stream))
    ((individuals-related? individuals-related-p)
     (answer expr state
             stream n 
             (individuals-related-p (second expr) (third expr) (fourth expr)
                                    (get-abox (fifth expr)))
             output-string-stream))
    ((individuals-equal? individuals-equal-p)
     (answer expr state
             stream n 
             (symbol-eql (second expr) (third expr))
             output-string-stream))
    ((individuals-not-equal? individuals-not-equal-p)
     (answer expr state
             stream n 
             (not (symbol-eql (second expr) (third expr)))
             output-string-stream))
    ((concept-synonyms atomic-concept-synonyms)
     (answer expr state
             stream n
             (atomic-concept-synonyms (second expr) 
                                      (get-tbox (third expr)))
             output-string-stream))
    ((concept-descendants atomic-concept-descendants)
     (answer expr state
             stream n
             (atomic-concept-descendants (second expr)
                                         (get-tbox (third expr)))
             output-string-stream))
    ((concept-ancestors atomic-concept-ancestors)
     (answer expr state
             stream n
             (atomic-concept-ancestors (second expr) 
                                       (get-tbox (third expr)))
             output-string-stream))
    ((concept-parents atomic-concept-parents)
     (answer expr state
             stream n
             (atomic-concept-parents (second expr) 
                                     (get-tbox (third expr)))
             output-string-stream))
    ((concept-children atomic-concept-children)
     (answer expr state
             stream n
             (atomic-concept-children (second expr) 
                                      (get-tbox (third expr)))
             output-string-stream))
    ((role-descendants atomic-role-descendants)
     (answer expr state
             stream n
             (atomic-role-descendants (second expr) 
                                      (get-tbox (third expr)))
             output-string-stream))
    ((role-ancestors atomic-role-ancestors)
     (answer expr state
             stream n
             (atomic-role-ancestors (second expr) 
                                    (get-tbox (third expr)))
             output-string-stream))
    ((role-children atomic-role-children)
     (answer expr state
             stream n
             (apply #'atomic-role-children
                    (second expr)
                    (third expr)
                    (cdddr expr))
             output-string-stream))
    ((role-parents atomic-role-parents)
     (answer expr state
             stream n
             (apply #'atomic-role-parents
                    (second expr)
                    (third expr)
                    (cdddr expr))
             output-string-stream))
    ((role-inverse atomic-role-inverse) 
     (answer expr state
             stream n
             (atomic-role-inverse (second expr) 
                                  (get-tbox (third expr)))
             output-string-stream))
    (all-tboxes
     (answer expr state
             stream n 
             (loop for tbox-name in (all-tboxes)
                   when (find-symbol (symbol-name tbox-name) *package*)
                   collect tbox-name)
             output-string-stream))
    (find-tbox 
     (answer expr state
             stream n (let ((tbox (find-tbox (second expr) (third expr))))
                        (when tbox
                          (ensure-tbox-name tbox)))
             output-string-stream))
    (set-find-tbox
     (setf (find-tbox (second expr)) (third expr))
     (ok expr stream n state output-string-stream))
    (all-atomic-concepts 
     (answer expr state
             stream n 
             (apply #'all-atomic-concepts (get-tbox (second expr))
                    (rest (rest expr)))
             output-string-stream))
    (all-equivalent-concepts 
     (answer expr state
             stream n 
             (apply #'all-equivalent-concepts (get-tbox (second expr))
                    (rest (rest expr)))
             output-string-stream))
    (all-roles 
     (answer expr state
             stream n (apply #'all-roles (get-tbox (second expr))
                             (rest (rest expr)))
             output-string-stream))
    (all-features 
     (answer expr state
             stream n (apply #'all-features (get-tbox (second expr))
                             (rest (rest expr)))
             output-string-stream))
    (all-transitive-roles 
     (answer expr state
             stream n (apply #'all-transitive-roles (get-tbox (second expr))
                             (rest (rest expr)))
             output-string-stream))
    (all-attributes 
     (answer expr state
             stream n (apply #'all-attributes (get-tbox (second expr))
                             (rest (rest expr)))
             output-string-stream))
    (attribute-type
     (answer expr state
             stream n (attribute-type (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((attribute-domain attribute-domain-1)
     (answer expr state
             stream n (attribute-domain-1 (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((individual-direct-types most-specific-instantiators)
     (answer expr state
             stream n 
             (most-specific-instantiators (second expr) 
                                          (get-abox (third expr)))
             output-string-stream))
    ((individual-types instantiators)
     (answer expr state
             stream n 
             (instantiators (second expr) 
                            (get-abox (third expr)))
             output-string-stream))
    ((concept-instances retrieve-concept-instances)
     (answer expr state
             stream n 
             (if (null (rest (rest (rest expr))))
                 (retrieve-concept-instances (second expr) 
                                             (get-abox (third expr)))
               (retrieve-concept-instances (second expr) 
                                           (get-abox (third expr))
                                           (fourth expr)))
             output-string-stream))
    ((individual-fillers retrieve-individual-fillers)
     (answer expr state
             stream n
             (apply #'retrieve-individual-fillers
                    (second expr) (third expr)
                    (get-abox (fourth expr))
                    (cddddr expr))
             output-string-stream))
    ((retrieve-related-individuals related-individuals )
     (answer expr state
             stream n
             (retrieve-related-individuals (second expr) 
                                           (get-abox (third expr)))
             output-string-stream))
    ((individual-filled-roles retrieve-individual-filled-roles)
     (answer expr state
             stream n 
             (retrieve-individual-filled-roles (second expr) (third expr)
                                               (get-abox (fourth expr)))
             output-string-stream))
    ((direct-predecessors retrieve-direct-predecessors)
     (answer expr state
             stream n 
             (retrieve-direct-predecessors (second expr) (third expr) 
                                           (get-abox (fourth expr)))
             output-string-stream))
    (associated-aboxes 
     (answer expr state
             stream n 
             (associated-aboxes (get-tbox (second expr)))
             output-string-stream))
    (associated-tbox
     (answer expr state
             stream n 
             (associated-tbox (get-abox (second expr)))
             output-string-stream))
    (all-aboxes
     (answer expr state
             stream n 
             (loop for abox-name in (all-aboxes)
                   when (find-symbol (symbol-name abox-name) *package*)
                   collect abox-name)
             output-string-stream)) 
    (find-abox 
     (answer expr state
             stream n (let ((abox (find-abox (second expr) (third expr))))
                        (when abox (ensure-abox-name abox)))
             output-string-stream))
    (set-find-abox
     (setf (find-abox (second expr)) (third expr))
     (ok expr stream n state output-string-stream))
    (all-individuals 
     (answer expr state
             stream n (apply #'all-individuals (get-abox (second expr))
                             (rest (rest expr)))
             output-string-stream))
    (all-concept-assertions-for-individual 
     (answer expr state
             stream n 
             (apply #'all-concept-assertions-for-individual (rest expr))
             output-string-stream))
    (all-role-assertions-for-individual-in-domain 
     (answer expr state
             stream n
             (apply #'all-role-assertions-for-individual-in-domain 
                    (rest expr))
             output-string-stream))
    (all-role-assertions-for-individual-in-range 
     (answer expr state
             stream n
             (apply #'all-role-assertions-for-individual-in-range 
                    (rest expr))
             output-string-stream))
    (all-concept-assertions 
     (answer expr state
             stream n
             (apply #'all-concept-assertions (rest expr))
             output-string-stream))
    (all-role-assertions 
     (answer expr state
             stream n 
             (apply #'all-role-assertions (rest expr))
             output-string-stream))
    (all-attribute-assertions 
     (answer expr state
             stream n 
             (apply #'all-attribute-assertions (rest expr))
             output-string-stream))
    (all-constraints
     (answer expr state
             stream n 
             (apply #'all-constraints (rest expr))
             output-string-stream))
    ((clone-tbox create-tbox-clone)
     (answer expr state
             stream n 
             (create-tbox-clone (get-tbox (second expr))
                                :new-name (getf (rest (rest expr)) ':new-name)
                                :overwrite (getf (rest (rest expr)) ':overwrite))
             output-string-stream))
    ((clone-abox create-abox-clone)
     (answer expr state
             stream n 
             (create-abox-clone (get-abox (second expr))
                                :new-name (getf (rest (rest expr)) ':new-name)
                                :overwrite (getf (rest (rest expr)) ':overwrite)
                                :copy-rules (getf (rest (rest expr)) ':copy-rules))
             output-string-stream))
    ((include-kb racer-read-file)
     (let ((*package* *racer-user-package*))
       (process-racer-file (transform-filename (second expr))))
     (ok expr stream n state output-string-stream))
    (racer-read-document
     (let ((*package* *racer-user-package*))
       (racer-read-document (second expr)))
     (ok expr stream n state output-string-stream))
    (xml-read-tbox-file 
     (answer expr state
             stream n 
             (xml-read-tbox-file (second expr))
             output-string-stream))
    (rdfs-read-tbox-file 
     (answer expr state
             stream n 
             (rdfs-read-tbox-file (second expr))
             output-string-stream))
    (owl-read-file 
     (answer expr state
             stream n 
             (apply #'owl-read-file (rest expr))
             output-string-stream))
    (owl-read-document
     (answer expr state
             stream n 
             (apply #'owl-read-document (rest expr))
             output-string-stream))
    (owllink-read-file 
     (answer expr state
             stream n 
             (apply #'owllink-read-file (rest expr))
             output-string-stream))
    (owllink-read-document
     (answer expr state
             stream n 
             (apply #'owllink-read-document (rest expr))
             output-string-stream))
    (dig-read-file 
     (answer expr state
             stream n 
             (dig-read-file (second expr) (third expr))
             output-string-stream))
    (dig-read-document
     (answer expr state
             stream n 
             (dig-read-document (second expr) (third expr))
             output-string-stream))
    (taxonomy
     (answer expr state
             stream n 
             (taxonomy (get-tbox (second expr)))
             output-string-stream))
    (print-tbox-tree
     (print-tbox-tree (get-tbox (second expr)) output-string-stream)
     (ok expr stream n state output-string-stream))
    (save-tbox
     (if *unsafe-mode*
         (progn
           (if (keywordp (third expr))
               (apply #'save-tbox (second expr) (list* (get-tbox) (rest (rest expr))))
             (apply #'save-tbox (rest expr)))
           (ok expr stream n state output-string-stream))
       (error "Illegal operator save-tbox. Only allowed with option -u (do not forget -- under Windows).")))
    (save-abox
     (if *unsafe-mode*
         (progn
           (if (keywordp (third expr))
               (apply #'save-abox (second expr) (list* (get-abox) (rest (rest expr))))
             (apply #'save-abox (rest expr)))
           (ok expr stream n state output-string-stream))
       (error "Illegal operator save-abox. Only allowed with option -u (do not forget -- under Windows).")))
    (save-kb 
     (if *unsafe-mode*
         (progn
           (apply #'save-kb (rest expr))
           (ok expr stream n state output-string-stream))
       (error "Illegal operator save-kb. Only allowed with option -u (do not forget -- under Windows).")))
    (compute-index-for-instance-retrieval 
     (compute-index-for-instance-retrieval (get-abox (second expr)))
     (ok expr stream n state output-string-stream))
    (ensure-subsumption-based-query-answering
     (ensure-subsumption-based-query-answering (get-abox (second expr)))
     (ok expr stream n state output-string-stream))
    (ensure-small-tboxes 
     (setf *always-use-lean-tbox* t)
     (ok expr stream n state output-string-stream))
    (describe-tbox
     (answer expr state
             stream n 
             (describe-tbox (get-tbox (second expr)) nil)
             output-string-stream))
    (describe-abox
     (answer expr state
             stream n 
             (describe-abox (get-abox (second expr)) nil)
             output-string-stream))
    ((describe-individual)
     (answer expr state
             stream n 
             (describe-individual (second expr) (get-abox (third expr)) nil)
             output-string-stream))
    (describe-individual1
     (answer expr state
             stream n 
             (describe-individual1 (second expr) (get-abox (third expr)) nil)
             output-string-stream))
    (describe-concept
     (answer expr state
             stream n 
             (describe-concept (second expr) (get-tbox (third expr)) nil)
             output-string-stream))
    (describe-role 
     (answer expr state
             stream n 
             (describe-role (second expr) (get-tbox (third expr)) nil)
             output-string-stream))
    ((individual-attribute-fillers retrieve-individual-attribute-fillers)
     (answer expr state
             stream n 
             (retrieve-individual-attribute-fillers (second expr) 
                                                    (third expr)
                                                    (get-abox (fourth expr)))
             output-string-stream))
    (told-value
     (answer expr state
             stream n 
             (told-value (second expr) (get-abox (third expr)))
             output-string-stream))
    ((individual-told-attribute-value retrieve-individual-told-attribute-value)
     (answer expr state
             stream n 
             (retrieve-individual-told-attribute-value (second expr) 
                                                       (third expr)
                                                       (get-abox (fourth expr)))
             output-string-stream))
    ((publish publish-1)
     (answer expr state
             stream n 
             (publish-1 (second expr) (get-abox (third expr)))
             output-string-stream))
    ((unpublish unpublish-1)
     (answer expr state
             stream n 
             (unpublish-1 (second expr) (get-abox (third expr)))
             output-string-stream))
    ((subscribe subscribe-1)
     (answer expr state
             stream n 
             (subscribe-1 (second expr) (third expr) 
                          (if (symbolp (fourth expr))
                              (get-abox (fourth expr))
                            (get-abox nil)))
             output-string-stream))
    ((unsubscribe unsubscribe-1)
     (answer expr state
             stream n 
             (unsubscribe-1 (second expr) (get-abox (third expr)))
             output-string-stream))
    ((init-subscriptions init-subscriptions-1)
     (init-subscriptions-1 (get-abox (third expr)))
     (ok expr stream n state output-string-stream))
    ((init-publications init-publications-1)
     (init-publications-1 (get-abox (third expr)))
     (ok expr stream n state output-string-stream))
    (check-subscriptions
     (answer expr state
             stream n 
             (check-subscriptions (get-abox (third expr)))
             output-string-stream))
    ((transitive role-is-transitive)
     (role-is-transitive (second expr) (get-tbox (third expr)))
     (ok expr stream n state output-string-stream))
    ((reflexive role-is-reflexive)
     (role-is-reflexive (second expr) (get-tbox (third expr)))
     (ok expr stream n state output-string-stream))
    ((irreflexive role-is-irreflexive)
     (role-is-irreflexive (second expr) (get-tbox (third expr)))
     (ok expr stream n state output-string-stream))
    ((symmetric role-is-symmetric)
     (role-is-symmetric (second expr) (get-tbox (third expr)))
     (ok expr stream n state output-string-stream))
    ((asymmetric role-is-asymmetric)
     (role-is-asymmetric (second expr) (get-tbox (third expr)))
     (ok expr stream n state output-string-stream))
    ((functional role-is-functional)
     (role-is-functional (second expr) (get-tbox (third expr)))
     (ok expr stream n state output-string-stream))
    ((inverse inverse-of-role)
     (inverse-of-role (second expr) (third expr) (get-tbox (fourth expr)))
     (ok expr stream n state output-string-stream))
    ((domain role-has-domain)
     (role-has-domain (second expr) (third expr) (get-tbox (fourth expr))
                      (if (null (rest (rest (rest (rest expr)))))
                          t
                        (fifth expr)))
     (ok expr stream n state output-string-stream))
    ((range role-has-range)
     (role-has-range (second expr) (third expr) (get-tbox (fourth expr))
                     (if (null (rest (rest (rest (rest expr)))))
                         t
                       (fifth expr)))
     (ok expr stream n state output-string-stream))
    ((implies-role role-has-parent)
     (role-has-parent (second expr) (third expr) (get-tbox (fourth expr)))
     (ok expr stream n state output-string-stream))
    (declare-disjoint 
     (declare-disjoint (second expr) (get-tbox (third expr)))
     (ok expr stream n state output-string-stream))
    (get-tbox-language
     (answer expr state
             stream n 
             (get-tbox-language (get-tbox (second expr)))
             output-string-stream))
    (get-abox-language
     (answer expr state
             stream n 
             (get-abox-language (get-abox (second expr)))
             output-string-stream))
    ((get-concept-definition get-concept-definition-1)
     (answer expr state
             stream n 
             (get-concept-definition-1 (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((get-concept-negated-definition get-concept-negated-definition-1)
     (answer expr state
             stream n 
             (get-concept-negated-definition-1 (second expr) (get-tbox (third expr)))
             output-string-stream))
    (get-meta-constraint
     (answer expr state
             stream n 
             (get-meta-constraint (get-tbox (second expr)))
             output-string-stream))
    (get-individual-pmodel
     (answer expr state
             stream n 
             (get-individual-pmodel (second expr) (get-abox (third expr)))
             output-string-stream))
    (get-concept-pmodel
     (answer expr state
             stream n 
             (get-concept-pmodel (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((role-domain atomic-role-domain)
     (answer expr state
             stream n 
             (atomic-role-domain (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((role-range atomic-role-range)
     (answer expr state
             stream n 
             (atomic-role-range (second expr) (get-tbox (third expr)))
             output-string-stream))
    (logging-on
     (setf *tcp-console-logging* t)
     (if (third expr)
         (setf *debug-racer-server* t))
     (if (second expr)
         (if *unsafe-mode*
             (progn
               (setf *log-file* (second expr))
               (ok expr stream n state output-string-stream))
           (error "File logging is only allowed if ~A is started in unsafe mode (option -u, do not forget -- under Windows)." *dl-prover-name*))
       (ok expr stream n state output-string-stream)))
    (logging-off
     (setf *tcp-console-logging* nil)
     (setf *log-file* nil)
     (setf *debug-racer-server* nil)
     (ok expr stream n state output-string-stream))
    (get-namespace-prefix 
     (answer expr state
             stream n 
             (get-namespace-prefix (get-tbox (second expr)))
             output-string-stream))
    (get-namespace-prefixes 
     (answer expr state
             stream n 
             (get-prefixes)
             output-string-stream))
    (store-tbox-image
     (if *unsafe-mode* 
         (progn 
           (store-tbox-image (second expr) (get-tbox (third expr)))
           (ok expr stream n state output-string-stream))
       (error "Illegal operator store-tbox-image. Only allowed with option -u (do not forget -- under Windows).")))
    (store-tboxes-image
     (if *unsafe-mode* 
         (progn 
           (store-tboxes-image (second expr) (mapcar #'get-tbox (third expr)))
           (ok expr stream n state output-string-stream))
       (error "Illegal operator store-tboxes-image. Only allowed with option -u (do not forget -- under Windows).")))
    (store-abox-image
     (if *unsafe-mode* 
         (progn 
           (store-abox-image (second expr)
                             (get-abox (third expr)))
           (ok expr stream n state output-string-stream))
       (error "Illegal operator store-abox-image. Only allowed with option -u (do not forget -- under Windows).")))
    (store-aboxes-image
     (if *unsafe-mode* 
         (progn 
           (store-aboxes-image (second expr) (mapcar #'get-abox (third expr)))
           (ok expr stream n state output-string-stream))
       (error "Illegal operator store-aboxes-image. Only allowed with option -u (do not forget -- under Windows).")))
    (store-kb-image
     (if *unsafe-mode* 
         (progn 
           (store-kb-image (second expr) 
                           (get-abox (third expr)))
           (ok expr stream n state output-string-stream))
       (error "Illegal operator store-kb-image. Only allowed with option -u (do not forget -- under Windows).")))
    (store-kbs-image
     (if *unsafe-mode* 
         (progn 
           (store-kbs-image (second expr) (mapcar #'get-abox (third expr)))
           (ok expr stream n state output-string-stream))
       (error "Illegal operator store-kbs-image. Only allowed with option -u (do not forget -- under Windows).")))
    (restore-tbox-image
     (restore-tbox-image (second expr))
     (ok expr stream n state output-string-stream))
    (restore-tboxes-image
     (restore-tboxes-image (second expr))
     (ok expr stream n state output-string-stream))
    (restore-abox-image
     (restore-abox-image (second expr))
     (ok expr stream n state output-string-stream))
    (restore-aboxes-image
     (restore-aboxes-image (second expr))
     (ok expr stream n state output-string-stream))
    (restore-kb-image
     (restore-kb-image (second expr))
     (ok expr stream n state output-string-stream))
    (restore-kbs-image
     (restore-kbs-image (second expr))
     (ok expr stream n state output-string-stream))
    (mirror
     (mirror (second expr) (third expr))
     (ok expr stream n state output-string-stream))
    (kb-ontologies 
     (answer expr state
             stream n 
             (kb-ontologies (second expr))
             output-string-stream))
    (compute-implicit-role-fillers
     (answer expr state
             stream n 
             (compute-implicit-role-fillers (second expr) (get-abox (third expr)))
             output-string-stream))
    (compute-all-implicit-role-fillers 
     (answer expr state
             stream n 
             (compute-all-implicit-role-fillers (get-abox (second expr)))
             output-string-stream))
    (get-kb-signature
     (answer expr state
             stream n 
             (get-kb-signature (second expr))
             output-string-stream))
    (get-tbox-signature
     (answer expr state
             stream n 
             (get-tbox-signature (get-tbox (second expr)))
             output-string-stream))
    (get-abox-signature
     (answer expr state
             stream n 
             (get-abox-signature (get-abox (second expr)))
             output-string-stream))
    ((symmetric-p symmetric?)
     (answer expr state
             stream n 
             (symmetric-p (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((asymmetric-p asymmetric?)
     (answer expr state
             stream n 
             (asymmetric-p (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((reflexive-p reflexive?)
     (answer expr state
             stream n 
             (reflexive-p (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((irreflexive-p irreflexive?)
     (answer expr state
             stream n 
             (irreflexive-p (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((tbox-cyclic-p tbox-cyclic?)
     (answer expr state
             stream n 
             (tbox-cyclic-p (get-tbox (second expr)))
             output-string-stream))
    ((cd-object-p cd-object?)
     (answer expr state
             stream n 
             (cd-object-p (second expr) (get-abox (third expr)))
             output-string-stream))
    ((constraint-entailed-p constraint-entailed?)
     (answer expr state
             stream n 
             (constraint-entailed-p (second expr) (get-abox (third expr)))
             output-string-stream))
    ((atomic-role-synonyms role-synonyms)
     (answer expr state
             stream n 
             (atomic-role-synonyms (second expr) (get-tbox (third expr)))
             output-string-stream))
    ((roles-equivalent roles-equivalent-1)
     (roles-equivalent-1 (second expr) (third expr) (get-tbox (fourth expr)))
     (ok expr stream n state output-string-stream))
    ((roles-disjoint roles-disjoint-1)
     (roles-disjoint-1 (second expr) (third expr) (get-tbox (fourth expr)))
     (ok expr stream n state output-string-stream))
    (verify-with-concept-tree-list
     (answer expr state
             stream n 
             (verify-with-concept-tree-list (second expr) (get-tbox (third expr)))
             output-string-stream))
    (verify-with-abox-individuals-list
     (answer expr state
             stream n 
             (verify-with-abox-individuals-list (second expr) (get-abox (third expr)))
             output-string-stream))
    (define-tbox
     (apply #'define-tbox-1 (first (second expr)) :axioms (rest (rest expr)) (rest (second expr)))
     (ok expr stream n state output-string-stream))
    (define-abox 
        (apply #'define-abox-1 (first (second expr)) (second (second expr))
               :axioms (rest (rest expr)) (rest (rest (second expr))))
      (ok expr stream n state output-string-stream))
    (print-abox-individuals
     (apply #'print-abox-individuals (rest expr))
     (ok expr stream n state output-string-stream))
    ((individual-told-datatype-fillers retrieve-individual-told-datatype-fillers)
     (answer expr state
             stream n 
             (apply #'retrieve-individual-told-datatype-fillers (rest expr))
             output-string-stream))
    ((role-equivalent? role-equivalent-p)
     (answer expr state
             stream n (role-equivalent-p (second expr) (third expr)
                                         (get-tbox (fourth expr)))
             output-string-stream))

    ((role-disjoint? role-disjoint-p)
     (answer expr state
             stream n (role-disjoint-p (second expr) (third expr)
                                       (get-tbox (fourth expr)))
             output-string-stream))

    ((same-individual-as same-as)
     (add-same-individual-as-assertion (get-abox nil)
                                       (second expr) (third expr))
     (ok expr stream n state output-string-stream))
    ((add-same-individual-as-assertion)
     (add-same-individual-as-assertion (get-abox (second expr))
                                       (third expr) (fourth expr))
     (ok expr stream n state output-string-stream))
    ((different-from)
     (add-different-from-assertion (get-abox nil)
                                   (second expr) (third expr))
     (ok expr stream n state output-string-stream))
    ((add-different-from-assertion)
     (add-different-from-assertion (get-abox (second expr))
                                   (third expr) (fourth expr))
     (ok expr stream n state output-string-stream))
    ((all-different)
     (add-all-different-assertion (get-abox nil)
                                  (cdr expr))
     (ok expr stream n state output-string-stream))
    ((add-all-different-assertion)
     (add-all-different-assertion (get-abox (second expr))
                                  (cddr expr))
     (ok expr stream n state output-string-stream))
    (role-used-as-datatype-property-p 
     (answer expr state
             stream n (role-used-as-datatype-property-p (second expr) (get-tbox (third expr)))
             output-string-stream))
    (role-is-used-as-datatype-property
     (role-is-used-as-datatype-property (second expr) (get-tbox (third expr)))
     (ok expr stream n state output-string-stream))
    (datatype-role-range
     (answer expr state
             stream n (datatype-role-range (second expr) (get-tbox (third expr)))
             output-string-stream))
    (datatype-role-has-range
     (datatype-role-has-range (second expr) (third expr) (get-tbox (fourth expr)))
     (ok expr stream n state output-string-stream))
    (set-server-timeout
     (let ((timeout (second expr)))
					;(check-type timeout 'fixnum)
       (setf *server-timeout* timeout))
     (ok expr stream n state output-string-stream))
    (get-server-timeout
     (answer expr state
             stream n *server-timeout*
             output-string-stream))
    (with-unique-name-assumption
      (with-unique-name-assumption
        (loop for expr1 in (rest expr) do
              (process-racer-expr expr1 stream n state output-string-stream))))
    (without-unique-name-assumption
      (without-unique-name-assumption
        (loop for expr1 in (rest expr) do
              (process-racer-expr expr1 stream n state output-string-stream))))
    (set-unique-name-assumption
     (answer expr state
             stream n 
             (set-unique-name-assumption (second expr))
             output-string-stream))
    ((individual-synonyms retrieve-individual-synonyms)
     (answer expr state
             stream n 
             (apply #'retrieve-individual-synonyms (rest expr))
             output-string-stream))             
            
    ((individual-antonyms retrieve-individual-antonyms)
     (answer expr state
             stream n 
             (apply #'retrieve-individual-antonyms (rest expr))
             output-string-stream))             
            
    (create-tbox-internal-marker-concept
     (answer expr state
             stream n 
             (create-tbox-internal-marker-concept
              (second expr) (third expr))
             output-string-stream))
    #|
                     (parse-expression
		      (answer expr state
			      stream n 
			      (parse-expression (second expr))
			      output-string-stream))
		     |#
    (role-used-as-annotation-property-p 
     (answer expr state
             stream n (role-used-as-annotation-property-p (second expr) (get-tbox (third expr)))
             output-string-stream))
    (role-is-used-as-annotation-property
     (role-is-used-as-annotation-property (second expr) (get-tbox (third expr)))
     (ok expr stream n state output-string-stream))
    (add-annotation-concept-assertion 
     (apply #'add-annotation-concept-assertion (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))
    (add-annotation-role-assertion 
     (apply #'add-annotation-role-assertion (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream)) 
    (all-annotation-concept-assertions 
     (answer expr state
             stream n
             (apply #'all-annotation-concept-assertions (rest expr))
             output-string-stream))
    (all-annotation-role-assertions 
     (answer expr state
             stream n 
             (apply #'all-annotation-role-assertions (rest expr))
             output-string-stream))
    (with-critical-section
     (let ((last-expr (first (last expr))))
       (with-racer-critical-section
         (loop for expr1 in (butlast expr) do
               (process-racer-expr expr1 stream n t output-string-stream))
         (process-racer-expr last-expr stream n nil output-string-stream))))
    (get-racer-version
     (answer expr state
             stream n 
             (get-racer-version)
             output-string-stream))
    (get-build-version
     (answer expr state
             stream n 
             (get-build-version)
             output-string-stream))
    (attribute-filler
     (set-attribute-filler (get-abox) (second expr) (third expr) (fourth expr) (fifth expr))
     (ok expr stream n state output-string-stream))
    (set-attribute-filler
     (set-attribute-filler (get-abox (second expr)) (third expr) (fourth expr) (fifth expr) (sixth expr))
     (ok expr stream n state output-string-stream))
    (datatype-role-filler
     (add-datatype-role-filler (get-abox) (second expr) (third expr) (fourth expr) (fifth expr))
     (ok expr stream n state output-string-stream))
    (add-datatype-role-filler
     (add-datatype-role-filler (get-abox (second expr)) (third expr) (fourth expr) (fifth expr) (sixth expr))
     (ok expr stream n state output-string-stream))
    (add-negative-datatype-role-filler
     (add-negative-datatype-role-filler (get-abox (second expr)) (third expr) (fourth expr) (fifth expr) (sixth expr))
     (ok expr stream n state output-string-stream))

    (get-tbox-version
     (answer expr state
             stream n 
             (get-tbox-version (find-tbox (second expr)))
             output-string-stream))
    (get-abox-version
     (answer expr state
             stream n 
             (get-abox-version (find-abox (second expr)))
             output-string-stream))
    (internal-individuals-related-p 
     (answer expr state
             stream n 
             (internal-individuals-related-p 
              (second expr)
              (third expr)
              (fourth expr)
              (find-abox (fifth expr)))
             output-string-stream))
    (time
     (let ((*timing* (get-internal-run-time)))
       (process-racer-expr (second expr) stream n state output-string-stream)))

    (transmit-file
     (if *unsafe-mode*
         (progn
           (let ((temp-filename (generate-temp-filename (temp-directory) (second expr))))
             (answer expr state
                     stream n 
                     temp-filename
                     output-string-stream)
             (read-bytes-into-file stream (third expr) temp-filename)))
       (error "File transmission is only allowed if ~A is started in unsafe mode (option -u, do not forget -- under Windows)." 
              *dl-prover-name*)))


    (retrieve-individual-annotation-property-fillers 
     (answer expr state
             stream n 
             (retrieve-individual-annotation-property-fillers
              (second expr)
              (third expr)
              (get-abox (fourth expr)))
             output-string-stream))
    (swrl-forward-chaining
     (apply #'swrl-forward-chaining (rest expr))
     (ok expr stream n state output-string-stream))
    (prepare-racer-engine 
     (apply #'prepare-racer-engine (rest expr))
     (ok expr stream n state output-string-stream))
    (set-rewrite-defined-concepts 
     (set-rewrite-defined-concepts (second expr))
     (ok expr stream n state output-string-stream))
    (enable-optimized-query-processing
     (enable-optimized-query-processing
      (if (rest expr)
          (second expr)
        t))
     (ok expr stream n state output-string-stream))
    (prepare-abox
     (prepare-abox (get-abox (second expr)))
     (ok expr stream n state output-string-stream))
                     
    (triple-store-read-file
     (answer expr state
             stream n 
             (apply #'triple-store-read-file (rest expr))
             output-string-stream))

    (use-triple-store
     (answer expr state
             stream n 
             (apply #'use-triple-store (rest expr))
             output-string-stream))

    ((materialize-inferences save-ontology-to-triple-store)
     (apply #'materialize-inferences (rest expr))
     (ok expr stream n state output-string-stream))
    (triple-store-graphs
     (answer expr state
             stream n 
             (apply #'triple-store-graphs (rest expr))
             output-string-stream))

    ((pretrieve pracer-answer-query)
     (answer expr state
             stream n 
             (apply #'pracer-answer-query (rest expr))
             output-string-stream))
		     
    (triple-store-open-p
     (answer expr state
             stream n 
             (apply #'triple-store-open-p (rest expr))
             output-string-stream))
    (close-triple-store
     (answer expr state
             stream n 
             (apply #'close-triple-store (rest expr))
             output-string-stream))
    (open-triple-store
     (answer expr state
             stream n 
             (apply #'open-triple-store (rest expr))
             output-string-stream))
    (create-triple-store
     (answer expr state
             stream n 
             (apply #'create-triple-store (rest expr))
             output-string-stream))
    (index-all-triples
     (answer expr state
             stream n 
             (apply #'index-all-triples (rest expr))
             output-string-stream))
                     
    (declare-current-knowledge-bases-as-persistent
     (declare-current-knowledge-bases-as-persistent)
     (ok expr stream n state output-string-stream))
    (server-case
     (answer expr state
             stream n 
             (server-case)
             output-string-stream))
    (define-event-assertion
     (add-event-assertion (second expr))
     (ok expr stream n state output-string-stream))
    (add-event-assertion
     (add-event-assertion (second expr) (get-abox (third expr)))
     (ok expr stream n state output-string-stream))
    (define-event-rule
     (add-event-rule (second expr)
                     (rest (rest expr)))
     (ok expr stream n state output-string-stream))
    (add-event-rule
     (add-event-rule (second expr) (third expr) (get-abox (fourth expr)))
     (ok expr stream n state output-string-stream))
    ((timenet-retrieve timenet-answer-query)
     (answer expr state
             stream n 
             (timenet-answer-query (second expr) :abox (get-abox (getf expr ':abox)))
             output-string-stream))
    (recognize-events
     (answer expr state
             stream n 
             (recognize-events (get-abox (second expr)))
             output-string-stream))
    (convert-event-specs
     (apply #'convert-event-specs (rest expr))
     (ok expr stream n state output-string-stream))
    (enable-abduction
     (enable-abduction (second expr) (third expr))
     (ok expr stream n state output-string-stream))
    (disable-abduction
     (disable-abduction)
     (ok expr stream n state output-string-stream))
    ((retrieve-with-explanation racer-answer-query-with-explanation)
     (answer expr state
             stream n 
             (apply #'racer-answer-query-with-explanation (rest expr))
             output-string-stream))
    (get-role-datatype 
     (answer expr state
             stream n 
             (apply #'get-role-datatype (rest expr))
             output-string-stream))
    (define-rule
     (apply #'add-rule-axiom (get-abox) (rest expr))
     (ok expr stream n state output-string-stream))
    (add-rule-axiom 
     (apply #'add-rule-axiom (get-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))
                      
    ((sparql-retrieve sparql-answer-query)
     (answer expr state
             stream n 
             (apply #'sparql-answer-query (rest expr))
             output-string-stream))
    (lcs
     (answer expr state
             stream n 
             (lcs (second expr) (third expr))
             output-string-stream))
    (lcs-unfold
     (answer expr state
             stream n 
             (lcs-unfold (second expr) (third expr) (get-tbox (fourth expr)))
             output-string-stream))
    (msc-k
     (answer expr state
             stream n 
             (apply #'msc-k (rest expr))
             output-string-stream))
    ((add-prefix define-prefix)
     (add-prefix (second expr) (third expr))
     (ok expr stream n state output-string-stream))
    #+:sonic
    ((ale-lcs)
     (answer expr state
             stream n 
             (funcall (intern (symbol-name 'ale-lcs) :sonic) (second expr))
             output-string-stream))
    #+:sonic
    ((alen-lcs) ; !
     (answer expr state
             stream n 
             (apply #'alen-lcs (rest expr))
             output-string-stream))
    #+:sonic
    ((alen-approx)	; !
     (answer expr state
             stream n 
             (apply #'alen-approx-to-new-concept (rest expr))
             output-string-stream))
    #+:sonic
    ((alc-ale-approximation)
     (answer expr state
             stream n 
             (funcall (intern (symbol-name 'alc-ale-approximation) :sonic) (second expr))
             output-string-stream))

    (publish-file
     (if *unsafe-mode*
         (progn
           (publish-file (second expr) (third expr) (fourth expr))
           (ok expr stream n state output-string-stream))
       (error "Illegal operator publish-file used in ~A. Only allowed with option -u (do not forget -- under Windows)." expr)))
		     
    (enable-alisp-compatibility-mode
     #+:mlisp
     (setf *ensure-alisp-socket-compatibility* t)
     (ok expr stream n state output-string-stream))
		     
    (disable-alisp-compatibility-mode
     #+:mlisp
     (setf *ensure-alisp-socket-compatibility* nil)
     (ok expr stream n state output-string-stream))
		     
    (abox-consistent-if-assertions-added-p
     (answer expr state
             stream n 
             (apply #'abox-consistent-if-assertions-added-p (rest expr))
             output-string-stream))

    (get-object-bottom-role
     (answer expr state
             stream n 
             (get-object-bottom-role (get-tbox (second expr)))
             output-string-stream))

    (get-data-bottom-role
     (answer expr state
             stream n 
             (get-data-bottom-role (get-tbox (second expr)))
             output-string-stream))

    (forget-annotation-concept-assertion 
     (apply #'forget-annotation-concept-assertion (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))

    (forget-all-different-assertion
     (apply #'forget-all-different-assertion (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))

    (forget-same-individual-as-assertion
     (apply #'forget-same-individual-as-assertion (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))

    (forget-different-from-assertion
     (apply #'forget-different-from-assertion (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))

    (forget-datatype-role-filler
     (apply #'forget-datatype-role-filler (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))

    (forget-negative-datatype-role-filler
     (apply #'forget-negative-datatype-role-filler (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))

    (forget-negated-role-assertion
     (apply #'forget-negated-role-assertion (find-abox (second expr)) (rest (rest expr)))
     (ok expr stream n state output-string-stream))

    ;;; aus nrql-server-case: 
    ;;; Stand: 07.08.2011

    ((xml-output xml-native-output)
     (process-racer-expr (second expr)
                         nil
                         n
                         state
                         output-string-stream)
     (let* ((native (eq (first expr) 'xml-native-output))
            (value
             (with-output-to-string (sstream)
               (apply #'lisp-to-xml
                      (if *cur-reasoner*
                          (if (last-error *cur-reasoner*)
                              (last-error *cur-reasoner*)
                            (last-answer *cur-reasoner*))
                        (if *last-error* *last-error* *last-answer*))
                      sstream
                      :include-answer-string-p
                      native
                      :top-level-attributes
                      (format nil
                              "id=\"~d\" type=\"~A\""
                              n
                              (if (and *cur-reasoner*
                                       (last-error *cur-reasoner*))
                                  'error
                                'answer))
                      (cddr expr)))))
       (format stream
               ":answer ~D \"~A\" \"~A\""
               n
               (transform-value value)
               (convert-output-to-string output-string-stream))
       (racer-eol stream)))
    ((xml-input)
     (let* ((string (second expr)) (expr (xml-to-lisp string)))
       (process-racer-expr expr stream n state output-string-stream)))
    ((|OWLAPI-getLastOutputStreamString|)
     (with-reasoner ((if (cdr expr) (second expr) *cur-reasoner*))
       (let ((old-policy (return-policy *cur-reasoner*)))
         (unwind-protect
             (progn
               (setf (return-policy *cur-reasoner*) :answer-direct)
               (answer expr
                       state
                       stream
                       n
                       (last-output-stream-string *cur-reasoner*)
                       output-string-stream))
           (setf (return-policy *cur-reasoner*) old-policy)))))
    ((|OWLAPI-getLastAnswer|)
     (with-reasoner ((if (cdr expr) (second expr) *cur-reasoner*))
       (let ((old-policy (return-policy *cur-reasoner*)))
         (unwind-protect
             (progn
               (setf (return-policy *cur-reasoner*) :answer-direct)
               (answer expr
                       state
                       stream
                       n
                       (last-answer *cur-reasoner*)
                       output-string-stream))
           (setf (return-policy *cur-reasoner*) old-policy)))))
    ((|OWLAPI-getIDsOfLastAnswer|)
     (labels ((do-it (obj)
                (or (|OWLAPI-findIDFromObject| obj)
                    (if (listp obj) (mapcar #'do-it obj) obj))))
       (with-reasoner ((if (cdr expr) (second expr) *cur-reasoner*))
         (let ((old-policy (return-policy *cur-reasoner*)))
           (unwind-protect
               (progn
                 (setf (return-policy *cur-reasoner*) :answer-direct)
                 (answer expr
                         state
                         stream
                         n
                         (do-it (last-answer *cur-reasoner*))
                         output-string-stream))
             (setf (return-policy *cur-reasoner*) old-policy))))))
    ((add-doc-phrase)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'add-doc-phrase1)
                        (rest expr)))
               output-string-stream)))
    ((add-doc-image-data)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'add-doc-image-data1)
                        (rest expr)))
               output-string-stream)))
    ((add-doc-image-data-from-file)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'add-doc-image-data-from-file1)
                        (rest expr)))
               output-string-stream)))
    ((add-doc-image-file)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'add-doc-image-file1)
                        (rest expr)))
               output-string-stream)))
    ((del-doc-entry)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'del-doc-entry1) (rest expr)))
               output-string-stream)))
    ((add-doc-entry)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'add-doc-entry1) (rest expr)))
               output-string-stream)))
    ((compute-abox-difference-alternative)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'compute-abox-difference2)
                        (rest expr)))
               output-string-stream)))
    ((compute-abox-difference)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'compute-abox-difference1)
                        (rest expr)))
               output-string-stream)))
    ((retrieve-with-explanation)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-query-with-explanation)
                        (rest expr)))
               output-string-stream)))
    ((del-rcc-edge)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'del-rcc-edge1) (rest expr)))
               output-string-stream)))
    ((del-rcc-node)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'del-rcc-node1) (rest expr)))
               output-string-stream)))
    ((rcc-edge-description)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-edge-description1)
                        (rest expr)))
               output-string-stream)))
    ((rcc-node-description)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-node-description1)
                        (rest expr)))
               output-string-stream)))
    ((rcc-edge-label)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-edge-label1)
                        (rest expr)))
               output-string-stream)))
    ((rcc-node-label)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-node-label1)
                        (rest expr)))
               output-string-stream)))
    ((rcc-edge)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-edge1) (rest expr)))
               output-string-stream)))
    ((rcc-node)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-node1) (rest expr)))
               output-string-stream)))
    ((rcc-related)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-related1) (rest expr)))
               output-string-stream)))
    ((rcc-instance)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-instance1) (rest expr)))
               output-string-stream)))
    ((rcc-synonym)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'register-rcc-synonym)
                        (rest expr)))
               output-string-stream)))
    ((rcc-consistent?)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'rcc-consistent-p)
                        (rest expr)))
               output-string-stream)))
    ((in-rcc-box)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'set-rcc-box) (rest expr)))
               output-string-stream)))
    ((in-mirror-data-box)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'set-mirror-data-box)
                        (rest expr)))
               output-string-stream)))
    ((description-implies?)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'description-implies-p)
                        (rest expr)))
               output-string-stream)))
    ((edge-description)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'edge-description1)
                        (rest expr)))
               output-string-stream)))
    ((node-description)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'node-description1)
                        (rest expr)))
               output-string-stream)))
    ((edge-label)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'edge-label1) (rest expr)))
               output-string-stream)))
    ((node-label)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'node-label1) (rest expr)))
               output-string-stream)))
    ((del-data-edge)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'del-data-edge1) (rest expr)))
               output-string-stream)))
    ((del-data-node)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'del-data-node1) (rest expr)))
               output-string-stream)))
    ((data-edge)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'data-edge1) (rest expr)))
               output-string-stream)))
    ((data-node)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'data-node1) (rest expr)))
               output-string-stream)))
    ((in-data-box)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'set-data-box) (rest expr)))
               output-string-stream)))
    ((undefquery)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'undefine-query) (rest expr)))
               output-string-stream)))
    ((def-and-exec-query)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'define-and-execute-query)
                        (rest expr)))
               output-string-stream)))
    ((def-and-prep-query)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'define-and-prepare-query)
                        (rest expr)))
               output-string-stream)))
    ((defquery)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'define-query) (rest expr)))
               output-string-stream)))
    ((tbox-retrieve1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-tbox-query1)
                        (rest expr)))
               output-string-stream)))
    ((tbox-retrieve)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-tbox-query)
                        (rest expr)))
               output-string-stream)))
    ((prepare-tbox-query1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-tbox-query1)
                        (rest expr)))
               output-string-stream)))
    ((prepare-tbox-query)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-tbox-query)
                        (rest expr)))
               output-string-stream)))
    ((firerule-under-premise1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule-under-premise1)
                        (rest expr)))
               output-string-stream)))
    ((firerule-under-premise)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule-under-premise)
                        (rest expr)))
               output-string-stream)))
    ((firerule1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule1)
                        (rest expr)))
               output-string-stream)))
    ((firerule)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule)
                        (rest expr)))
               output-string-stream)))
    ((apply-abox-rule-under-premise1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule-under-premise1)
                        (rest expr)))
               output-string-stream)))
    ((apply-abox-rule-under-premise)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule-under-premise)
                        (rest expr)))
               output-string-stream)))
    ((apply-abox-rule1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule1)
                        (rest expr)))
               output-string-stream)))
    ((apply-abox-rule)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-apply-rule)
                        (rest expr)))
               output-string-stream)))
    ((preprule1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-rule1)
                        (rest expr)))
               output-string-stream)))
    ((preprule)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-rule)
                        (rest expr)))
               output-string-stream)))
    ((prepare-abox-rule1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-rule1)
                        (rest expr)))
               output-string-stream)))
    ((prepare-abox-rule)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-rule)
                        (rest expr)))
               output-string-stream)))
    ((retrieve-under-premise1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-query-under-premise1)
                        (rest expr)))
               output-string-stream)))
    ((retrieve-under-premise)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-query-under-premise)
                        (rest expr)))
               output-string-stream)))
    ((retrieve1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-query1)
                        (rest expr)))
               output-string-stream)))
    ((retrieve)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-answer-query)
                        (rest expr)))
               output-string-stream)))
    ((prepare-abox-query1)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-query1)
                        (rest expr)))
               output-string-stream)))
    ((prepare-abox-query)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'racer-prepare-query)
                        (rest expr)))
               output-string-stream)))
    ((unbind)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'unbind1) (rest expr)))
               output-string-stream)))
    ((defpar)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'defpar1) (rest expr)))
               output-string-stream)))
    ((defcon)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'defcon1) (rest expr)))
               output-string-stream)))
    ((undefine)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'undefine1) (rest expr)))
               output-string-stream)))
    ((define)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'define1) (rest expr)))
               output-string-stream)))
    ((evaluate)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function 'evaluate1) (rest expr)))
               output-string-stream)))
    ((sequence)
     (let ((*server-timeout* nil)
           (reasoner *cur-reasoner*)
           (*check-subscriptions-inhibited* t)
           (thematic-substrate::*changes-for-aboxes-pending* nil))
       (with-output-to-string (string-stream)
         (prog1
             (mapl #'(lambda (expressions)
                       (let ((expr1 (first expressions)))
                         (prog1
                             (process-racer-expr expr1
                                                 (if (cdr expressions)
                                                     *null-stream*
                                                   stream)
                                                 n
                                                 state
                                                 (if (cdr expressions)
                                                     string-stream
                                                   output-string-stream))
                           (when (and (last-error reasoner)
                                      (cdr expressions))
                             (error (last-error reasoner))))))
                   (rest expr))
           (let ((*check-subscriptions-inhibited* nil))
             (dolist (abox
                      thematic-substrate::*changes-for-aboxes-pending*)
               (check-nrql-subscriptions abox)))))))
    ((|OWLAPI-sequence| owlapi-sequence)
     (let ((*server-timeout* nil)
           (reasoner *cur-reasoner*)
           (*check-subscriptions-inhibited* t)
           (thematic-substrate::*changes-for-aboxes-pending* nil)
           (count 0))
       (with-reasoner ((second expr))
         (let ((*progress-value* 0))
           (with-progress-range ((length (cddr expr)) (0 100))
             (with-output-to-string (string-stream)
               (prog1
                   (mapl #'(lambda (expressions)
                             (let ((expr1 (first expressions)))
                               (set-progress-value (incf count))
                               (prog1
                                   (process-racer-expr expr1
                                                       (if (cdr expressions)
                                                           *null-stream*
                                                         stream)
                                                       n
                                                       state
                                                       (if (cdr expressions)
                                                           string-stream
                                                         output-string-stream))
                                 (when (and (last-error reasoner)
                                            (cdr expressions))
                                   (error (last-error reasoner))))))
                         (cddr expr))
                 (let ((*check-subscriptions-inhibited* nil))
                   (dolist (abox
                            thematic-substrate::*changes-for-aboxes-pending*)
                     (check-nrql-subscriptions abox))))))))))
    ((answer-sequence)
     (let ((*server-timeout* nil)
           (reasoner *cur-reasoner*)
           (*check-subscriptions-inhibited* t)
           (thematic-substrate::*changes-for-aboxes-pending* nil))
       (prog1
           (answer expr
                   state
                   stream
                   n
                   (let ((x nil))
                     (with-output-to-string (string-stream)
                       (setf x
                             (mapcar #'(lambda (expr1)
                                         (process-racer-expr expr1
                                                             *null-stream*
                                                             n
                                                             state
                                                             output-string-stream)
                                         (if (last-error reasoner)
                                             (error (last-error reasoner))
                                           (last-answer reasoner)))
                                     (rest expr))))
                     x)
                   output-string-stream)
         (let ((*check-subscriptions-inhibited* nil))
           (dolist (abox
                    thematic-substrate::*changes-for-aboxes-pending*)
             (check-nrql-subscriptions abox))))))
    ((|OWLAPI-answerSequence| owlapi-answer-sequence)
     (let ((*server-timeout* nil)
           (reasoner *cur-reasoner*)
           (*check-subscriptions-inhibited* t)
           (thematic-substrate::*changes-for-aboxes-pending* nil)
           (count 0))
       (prog1
           (with-reasoner ((second expr))
             (let ((*progress-value* 0))
               (with-progress-range ((length (cddr expr)) (0 100))
                 (answer expr
                         state
                         stream
                         n
                         (let ((x nil))
                           (with-output-to-string (string-stream)
                             (setf x
                                   (mapcar #'(lambda (expr1)
                                               (set-progress-value (incf count))
                                               (process-racer-expr expr1
                                                                   *null-stream*
                                                                   n
                                                                   state
                                                                   output-string-stream)
                                               (if (last-error reasoner)
                                                   (error (last-error reasoner))
                                                 (last-answer reasoner)))
                                           (cddr expr))))
                           x)
                         output-string-stream)
                 (let ((*check-subscriptions-inhibited* nil))
                   (dolist (abox
                            thematic-substrate::*changes-for-aboxes-pending*)
                     (check-nrql-subscriptions abox)))))))))
    ((quiet-sequence)
     (let ((*server-timeout* nil)
           (reasoner *cur-reasoner*)
           (*check-subscriptions-inhibited* t)
           (thematic-substrate::*changes-for-aboxes-pending* nil))
       (with-output-to-string (string-stream)
         (mapc #'(lambda (expr1)
                   (prog1
                       (process-racer-expr expr1
                                           *null-stream*
                                           n
                                           state
                                           output-string-stream)
                     (when (last-error reasoner)
                       (error (last-error reasoner)))))
               (rest expr)))
       (prog1
           (ok expr stream n state output-string-stream)
         (let ((*check-subscriptions-inhibited* nil))
           (dolist (abox
                    thematic-substrate::*changes-for-aboxes-pending*)
             (check-nrql-subscriptions abox))))))
    ((|OWLAPI-quietSequence| owlapi-quiet-sequence)
     (let ((*server-timeout* nil)
           (reasoner *cur-reasoner*)
           (*check-subscriptions-inhibited* t)
           (thematic-substrate::*changes-for-aboxes-pending* nil)
           (count 0))
       (with-reasoner ((second expr))
         (let ((*progress-value* 0))
           (with-progress-range ((length (cddr expr)) (0 100))
             (with-output-to-string (string-stream)
               (mapc #'(lambda (expr1)
                         (set-progress-value (incf count))
                         (prog1
                             (process-racer-expr expr1
                                                 *null-stream*
                                                 n
                                                 state
                                                 output-string-stream)
                           (when (last-error reasoner)
                             (error (last-error reasoner)))))
                     (cddr expr)))
             (prog1
                 (ok expr stream n state output-string-stream)
               (let ((*check-subscriptions-inhibited* nil))
                 (dolist (abox
                          thematic-substrate::*changes-for-aboxes-pending*)
                   (check-nrql-subscriptions abox)))))))))
    ((with-future-bindings-evaluated)
     (let ((*server-timeout* nil))
       (apply (symbol-function 'thematic-substrate::eval-with-future-bindings)
              (lambda ()
                (loop for expr1 in (cddr expr)
                      do (process-racer-expr expr1
                                             stream
                                             n
                                             state
                                             output-string-stream)))
              (second expr))))
    ((with-future-bindings)
     (let ((*server-timeout* nil))
       (apply (symbol-function 'thematic-substrate::eval-with-future-bindings)
              (lambda ()
                (loop for expr1 in (cddr expr)
                      do (process-racer-expr expr1
                                             stream
                                             n
                                             state
                                             output-string-stream)))
              (second expr))))
    ((with-bindings-evaluated)
     (let ((*server-timeout* nil))
       (apply (symbol-function 'thematic-substrate::eval-with-bindings)
              (lambda ()
                (loop for expr1 in (cddr expr)
                      do (process-racer-expr expr1
                                             stream
                                             n
                                             state
                                             output-string-stream)))
              (second expr))))
    ((with-bindings)
     (let ((*server-timeout* nil))
       (apply (symbol-function 'thematic-substrate::eval-with-bindings)
              (lambda ()
                (loop for expr1 in (cddr expr)
                      do (process-racer-expr expr1
                                             stream
                                             n
                                             state
                                             output-string-stream)))
              (second expr))))
    ((with-nrql-settings-evaluated)
     (let ((*server-timeout* nil))
       (apply (symbol-function 'thematic-substrate::eval-nrql-settings)
              (lambda ()
                (loop for expr1 in (cddr expr)
                      do (process-racer-expr expr1
                                             stream
                                             n
                                             state
                                             output-string-stream)))
              (second expr))))
    ((with-nrql-settings)
     (let ((*server-timeout* nil))
       (apply (symbol-function 'thematic-substrate::eval-nrql-settings)
              (lambda ()
                (loop for expr1 in (cddr expr)
                      do (process-racer-expr expr1
                                             stream
                                             n
                                             state
                                             output-string-stream)))
              (second expr))))
    ((add-explanation-assertions get-explanations
                                 get-number-of-explanations
                                 query-subscribers
                                 unsubscribe-from
                                 subscribe-to
                                 get-nodes-in-qbox-for-abox
                                 get-dag-of-qbox-for-abox
                                 show-qbox-for-abox
                                 query-equivalents
                                 query-descendants
                                 query-children
                                 query-ancestors
                                 query-parents
                                 query-equivalent-p
                                 query-entails-p
                                 classify-query
                                 rule-consistent-p
                                 query-consistent-p
                                 add-chosen-sets-of-rule-consequences
                                 get-chosen-sets-of-rule-consequences
                                 choose-current-set-of-rule-consequences
                                 execute-applicable-rules
                                 unapplicable-rules
                                 applicable-rules
                                 rule-unapplicable-p
                                 rule-applicable-p
                                 execute-or-reexecute-rule
                                 execute-or-reexecute-query
                                 reexecute-rule
                                 reexecute-query
                                 reprepare-rule
                                 reprepare-query
                                 rule-accurate-p
                                 query-accurate-p
                                 expensive-rule-p
                                 expensive-query-p
                                 cheap-rule-p
                                 cheap-query-p
                                 rule-terminated-p
                                 query-terminated-p
                                 rule-processed-p
                                 query-processed-p
                                 rule-sleeping-p
                                 query-sleeping-p
                                 rule-waiting-p
                                 query-waiting-p
                                 rule-running-p
                                 query-running-p
                                 rule-active-p
                                 query-active-p
                                 rule-prepared-p
                                 query-prepared-p
                                 rule-ready-p
                                 query-ready-p
                                 execute-rule
                                 get-all-remaining-sets-of-rule-consequences
                                 get-next-n-remaining-sets-of-rule-consequences
                                 get-next-set-of-rule-consequences
                                 next-set-of-rule-consequences-available-p
                                 get-current-set-of-rule-consequences
                                 get-all-remaining-tuples
                                 get-next-n-remaining-tuples
                                 get-next-tuple
                                 next-tuple-available-p
                                 get-current-tuple
                                 get-answer
                                 abort-rule
                                 abort-query
                                 execute-query
                                 delete-rule
                                 delete-query
                                 original-rule-antecedence
                                 original-query-body
                                 rule-antecedence
                                 query-body
                                 original-rule-consequence
                                 original-query-head
                                 rule-consequence
                                 query-head
                                 describe-rule
                                 describe-query
                                 describe-rule-status
                                 describe-query-status
                                 owlapi-sleep
                                 add-doc-phrase1
                                 add-doc-image-data1
                                 add-doc-image-data-from-file1
                                 add-doc-image-file1
                                 del-doc-entry1
                                 add-doc-entry1
                                 clear-all-documentation
                                 compute-abox-difference2
                                 compute-abox-difference1
                                 remove-implied-concept-assertions
                                 make-abduction-rule-from-aboxes
                                 make-backward-rule-from-aboxes
                                 make-forward-rule-from-aboxes
                                 make-query-from-abox
                                 create-subgraph-aboxes
                                 compute-subgraph-aboxes
                                 get-new-ind-prefix
                                 set-new-ind-prefix
                                 get-new-ind-counter
                                 set-new-ind-counter
                                 get-minimum
                                 get-maximum
                                 abox-entails-abox-p
                                 disable-abduction
                                 enable-abduction
                                 |OWLAPI-readOntology|
                                 |OWLAPI-saveOntology|
                                 |OWLAPI-writeXMLOntologyFile|
                                 |OWLAPI-writeFunctionalOntologyFile|
                                 |OWLAPI-writeOntologyFile|
                                 |OWLAPI-readXMLOntologyDocument|
                                 |OWLAPI-readXMLOntologyFile|
                                 |OWLAPI-readFunctionalOntologyDocument|
                                 |OWLAPI-readFunctionalOntologyFile|
                                 |OWLAPI-parseNative|
                                 |OWLAPI-parse|
                                 update-racer
                                 check-for-updates
                                 load-racer-plugins
                                 load-racer-patches
                                 installed-plugins
                                 installed-patches
                                 load-racer-plugin
                                 load-racer-patch
                                 make-plugin-from-fasl-file
                                 swrl-create-forward-chainging-rules
                                 swrl-create-abduction-rules-if-possible
                                 get-prefixes
                                 delete-prefix-mappings
                                 |OWLAPI-restoreImage|
                                 |OWLAPI-storeImage|
                                 owlapi-add-axiom
                                 owlapi-add-axioms
                                 owlapi-axiom-loaded?
                                 owlapi-axiom-to-id
                                 owlapi-id-to-axiom
                                 owlapi-remove-axiom
                                 owlapi-remove-axioms
                                 owlapi-set-ontology-uri
                                 owlapi-abort
                                 owlapi-add-prefix
                                 owlapi-apply-changes
                                 owlapi-auto-add-axioms-to
                                 owlapi-auto-apply-changes
                                 owlapi-auto-batch-add-axioms-to
                                 owlapi-auto-batch-remove-axioms-from
                                 owlapi-auto-remove-axioms-from
                                 owlapi-batch-synchronize
                                 owlapi-classify
                                 owlapi-clear-changes
                                 owlapi-clear-ontologies
                                 owlapi-clear-registry
                                 owlapi-consider-declarations
                                 owlapi-contains
                                 owlapi-describe-ontologies
                                 owlapi-describe-ontology
                                 owlapi-describe-reasoner
                                 owlapi-describe-reasoners
                                 owlapi-disable-auto-mode
                                 owlapi-disable-incremental-updates
                                 owlapi-disable-lookup-mode
                                 owlapi-disable-memory-saving-mode
                                 owlapi-disable-simplified-protocol
                                 owlapi-disable-transient-axiom-mode
                                 owlapi-dispose
                                 owlapi-dispose-axiom
                                 owlapi-dispose-axioms
                                 owlapi-dispose-ontologies
                                 owlapi-dispose-ontology
                                 owlapi-dispose-reasoner
                                 owlapi-dont-register-declared-entities
                                 owlapi-dont-register-referenced-entities
                                 owlapi-enable-incremental-updates
                                 owlapi-enable-lookup-mode
                                 owlapi-enable-memory-saving-mode
                                 owlapi-enable-simplified-protocol
                                 owlapi-enable-transient-axiom-mode
                                 owlapi-export-ontology
                                 owlapi-export-reasoner
                                 owlapi-find-id-from-object
                                 owlapi-find-object-from-id
                                 owlapi-get-all-ontologies
                                 owlapi-get-ancestor-classes
                                 owlapi-get-ancestor-properties
                                 owlapi-get-annotation-axioms-for-axiom
                                 owlapi-get-auto-declare-data-properties
                                 owlapi-get-auto-ontology
                                 owlapi-get-axiom-counter
                                 owlapi-get-axioms
                                 owlapi-get-axioms-in
                                 owlapi-get-axioms-of-type
                                 owlapi-get-axioms-of-type-in
                                 owlapi-get-axioms-per-ontology
                                 owlapi-get-changes
                                 owlapi-get-current-reasoner
                                 owlapi-get-data-property-relationships
                                 owlapi-get-data-property-values
                                 owlapi-get-descendant-classes
                                 owlapi-get-descendant-properties
                                 owlapi-get-different-individuals
                                 owlapi-get-disjoint-classes
                                 owlapi-get-disjoint-data-properties
                                 owlapi-get-disjoint-object-properties
                                 owlapi-get-domains
                                 owlapi-get-equivalent-classes
                                 owlapi-get-equivalent-properties
                                 owlapi-get-inconsistent-classes
                                 owlapi-get-individuals
                                 owlapi-get-instances
                                 owlapi-get-inverse-properties
                                 owlapi-get-loaded-ontologies
                                 owlapi-get-owl-annotation-assertion-axiom
                                 owlapi-get-owl-annotation-property-domain-axiom
                                 owlapi-get-owl-annotation-property-range-axiom
                                 owlapi-get-owl-asymmetric-object-property-axiom
                                 owlapi-get-owl-axiom-annotation-axiom
                                 owlapi-get-owl-class-assertion-axiom
                                 owlapi-get-owl-data-property-assertion-axiom
                                 owlapi-get-owl-data-property-domain-axiom
                                 owlapi-get-owl-data-property-range-axiom
                                 owlapi-get-owl-data-sub-property-axiom
                                 owlapi-get-owl-datatype-definition-axiom
                                 owlapi-get-owl-declaration-axiom
                                 owlapi-get-owl-different-individuals-axiom
                                 owlapi-get-owl-disjoint-classes-axiom
                                 owlapi-get-owl-disjoint-data-properties-axiom
                                 owlapi-get-owl-disjoint-object-properties-axiom
                                 owlapi-get-owl-disjoint-union-axiom
                                 owlapi-get-owl-entity-annotation-axiom
                                 owlapi-get-owl-equivalent-classes-axiom
                                 owlapi-get-owl-equivalent-data-properties-axiom
                                 owlapi-get-owl-equivalent-object-properties-axiom
                                 owlapi-get-owl-functional-data-property-axiom
                                 owlapi-get-owl-functional-object-property-axiom
                                 owlapi-get-owl-has-key-axiom
                                 owlapi-get-owl-implicit-declaration-axiom
                                 owlapi-get-owl-imports-declaration-axiom
                                 owlapi-get-owl-inverse-functional-object-property-axiom
                                 owlapi-get-owl-inverse-object-properties-axiom
                                 owlapi-get-owl-irreflexive-object-property-axiom
                                 owlapi-get-owl-negative-data-property-assertion-axiom
                                 owlapi-get-owl-negative-object-property-assertion-axiom
                                 owlapi-get-owl-object-property-assertion-axiom
                                 owlapi-get-owl-object-property-chain-sub-property-axiom
                                 owlapi-get-owl-object-property-domain-axiom
                                 owlapi-get-owl-object-property-range-axiom
                                 owlapi-get-owl-object-sub-property-axiom
                                 owlapi-get-owl-ontology-annotation-axiom
                                 owlapi-get-owl-ontology-version-declaration-axiom
                                 owlapi-get-owl-prefix-declaration-axiom
                                 owlapi-get-owl-really-implicit-declaration-axiom
                                 owlapi-get-owl-reflexive-object-property-axiom
                                 owlapi-get-owl-same-individuals-axiom
                                 owlapi-get-owl-sub-annotation-property-axiom
                                 owlapi-get-owl-sub-annotation-property-of-axiom
                                 owlapi-get-owl-sub-class-axiom
                                 owlapi-get-owl-symmetric-object-property-axiom
                                 owlapi-get-owl-transitive-object-property-axiom
                                 owlapi-get-object-property-relationships
                                 owlapi-get-object-property-values
                                 owlapi-get-ontologies
                                 owlapi-get-prefixes
                                 owlapi-get-ranges
                                 owlapi-get-reasoners
                                 owlapi-get-related-individuals
                                 owlapi-get-related-values
                                 owlapi-get-same-individuals
                                 owlapi-get-sub-classes
                                 owlapi-get-sub-properties
                                 owlapi-get-super-classes
                                 owlapi-get-super-properties
                                 owlapi-get-types
                                 owlapi-has-data-property-relationship
                                 owlapi-has-object-property-relationship
                                 owlapi-has-type
                                 owlapi-ignore-annotations
                                 owlapi-ignore-declarations
                                 owlapi-init
                                 owlapi-is-asymmetric
                                 owlapi-is-class
                                 owlapi-is-classified
                                 owlapi-is-consistent
                                 owlapi-is-defined-class
                                 owlapi-is-defined-data-property
                                 owlapi-is-defined-individual
                                 owlapi-is-defined-object-property
                                 owlapi-is-different-individual
                                 owlapi-is-entailed
                                 owlapi-is-equivalent-class
                                 owlapi-is-functional
                                 owlapi-is-inverse-functional
                                 owlapi-is-irreflexive
                                 owlapi-is-realised
                                 owlapi-is-reflexive
                                 owlapi-is-same-individual
                                 owlapi-is-satisfiable
                                 owlapi-is-sub-class-of
                                 owlapi-is-symmetric
                                 owlapi-is-transitive
                                 owlapi-keep-annotations
                                 owlapi-load-axiom
                                 owlapi-load-axioms
                                 owlapi-load-ontologies
                                 owlapi-load-ontology
                                 owlapi-manually-apply-changes
                                 owlapi-merge-ontologies
                                 owlapi-new-ontology
                                 owlapi-new-reasoner
                                 owlapi-new-reasoner1
                                 owlapi-next-axiom-use-id
                                 owlapi-parse
                                 owlapi-parse-native
                                 owlapi-read-functional-ontology-document
                                 owlapi-read-functional-ontology-file
                                 owlapi-read-ontology
                                 owlapi-read-xml-ontology-document
                                 owlapi-read-xml-ontology-file
                                 owlapi-realize
                                 owlapi-register-declared-entities
                                 owlapi-register-last-answer
                                 owlapi-register-object
                                 owlapi-register-referenced-entities
                                 owlapi-reload-loaded-ontologies
                                 owlapi-remove-prefix
                                 owlapi-reset-axiom-counter
                                 owlapi-restore-image
                                 owlapi-save-ontology
                                 owlapi-set-auto-declare-data-properties
                                 owlapi-set-axiom-counter
                                 owlapi-set-current-reasoner
                                 owlapi-set-return-policy
                                 owlapi-store-image
                                 owlapi-unload-axiom
                                 owlapi-unload-axioms
                                 owlapi-unload-ontologies
                                 owlapi-unload-ontology
                                 owlapi-uses-incremental-updates
                                 owlapi-uses-simplified-protocol
                                 owlapi-write-functional-ontology-file
                                 owlapi-write-ontology-file
                                 owlapi-write-xml-ontology-file
                                 |OWLAPI-exportReasoner|
                                 |OWLAPI-exportOntology|
                                 |OWLAPI-SetOntologyURI|
                                 |OWLAPI-RemoveAxioms|
                                 |OWLAPI-removeAxioms|
                                 |OWLAPI-RemoveAxiom|
                                 |OWLAPI-removeAxiom|
                                 |OWLAPI-AddAxioms|
                                 |OWLAPI-addAxioms|
                                 |OWLAPI-AddAxiom|
                                 |OWLAPI-addAxiom|
                                 |OWLAPI-applyChanges|
                                 |OWLAPI-manuallyApplyChanges|
                                 |OWLAPI-autoApplyChanges|
                                 |OWLAPI-getChanges|
                                 |OWLAPI-clearChanges|
                                 |OWLAPI-getOWLObjectPropertyRangeAxiom|
                                 |OWLAPI-getOWLDataPropertyRangeAxiom|
                                 |OWLAPI-getOWLObjectPropertyDomainAxiom|
                                 |OWLAPI-getOWLDataPropertyDomainAxiom|
                                 |OWLAPI-getOWLDataSubPropertyAxiom|
                                 |OWLAPI-getOWLObjectSubPropertyAxiom|
                                 |OWLAPI-getOWLTransitiveObjectPropertyAxiom|
                                 |OWLAPI-getOWLSymmetricObjectPropertyAxiom|
                                 |OWLAPI-getOWLReflexiveObjectPropertyAxiom|
                                 |OWLAPI-getOWLIrreflexiveObjectPropertyAxiom|
                                 |OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom|
                                 |OWLAPI-getOWLAsymmetricObjectPropertyAxiom|
                                 |OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom|
                                 |OWLAPI-getOWLInverseObjectPropertiesAxiom|
                                 |OWLAPI-getOWLEquivalentObjectPropertiesAxiom|
                                 |OWLAPI-getOWLEquivalentDataPropertiesAxiom|
                                 |OWLAPI-getOWLDisjointObjectPropertiesAxiom|
                                 |OWLAPI-getOWLDisjointDataPropertiesAxiom|
                                 |OWLAPI-getOWLFunctionalObjectPropertyAxiom|
                                 |OWLAPI-getOWLFunctionalDataPropertyAxiom|
                                 |OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom|
                                 |OWLAPI-getOWLObjectPropertyAssertionAxiom|
                                 |OWLAPI-getOWLNegativeDataPropertyAssertionAxiom|
                                 |OWLAPI-getOWLDataPropertyAssertionAxiom|
                                 |OWLAPI-getOWLSameIndividualsAxiom|
                                 |OWLAPI-getOWLDifferentIndividualsAxiom|
                                 |OWLAPI-getOWLClassAssertionAxiom|
                                 |OWLAPI-getOWLSubClassAxiom|
                                 |OWLAPI-getOWLEquivalentClassesAxiom|
                                 |OWLAPI-getOWLDisjointUnionAxiom|
                                 |OWLAPI-getOWLDisjointClassesAxiom|
                                 |OWLAPI-getOWLPrefixDeclarationAxiom|
                                 |OWLAPI-getOWLOntologyVersionDeclarationAxiom|
                                 |OWLAPI-getOWLImportsDeclarationAxiom|
                                 |OWLAPI-getOWLReallyImplicitDeclarationAxiom|
                                 |OWLAPI-getOWLImplicitDeclarationAxiom|
                                 |OWLAPI-getOWLDeclarationAxiom|
                                 |OWLAPI-getOWLAnnotationPropertyRangeAxiom|
                                 |OWLAPI-getOWLAnnotationPropertyDomainAxiom|
                                 |OWLAPI-getOWLSubAnnotationPropertyAxiom|
                                 |OWLAPI-getOWLSubAnnotationPropertyOfAxiom|
                                 |OWLAPI-getOWLAnnotationAssertionAxiom|
                                 |OWLAPI-getOWLOntologyAnnotationAxiom|
                                 |OWLAPI-getOWLEntityAnnotationAxiom|
                                 |OWLAPI-getOWLAxiomAnnotationAxiom|
                                 |OWLAPI-getOWLHasKeyAxiom|
                                 |OWLAPI-getOWLDatatypeDefinitionAxiom|
                                 |OWLAPI-getPrefixes|
                                 |OWLAPI-removePrefix|
                                 |OWLAPI-addPrefix|
                                 |OWLAPI-getAnnotationAxiomsForAxiom|
                                 |OWLAPI-getAxiomsOfTypeIn|
                                 |OWLAPI-getAxiomsOfType|
                                 |OWLAPI-getAxiomsIn|
                                 |OWLAPI-getAxiomsPerOntology|
                                 |OWLAPI-getAxioms|
                                 |OWLAPI-AxiomLoaded?|
                                 |OWLAPI-AxiomToID|
                                 |OWLAPI-IDToAxiom|
                                 |OWLAPI-disposeAxioms|
                                 |OWLAPI-disposeAxiom|
                                 |OWLAPI-unloadAxioms|
                                 |OWLAPI-unloadAxiom|
                                 |OWLAPI-loadAxioms|
                                 |OWLAPI-loadAxiom|
                                 |OWLAPI-resetAxiomCounter|
                                 |OWLAPI-setAxiomCounter|
                                 |OWLAPI-getAxiomCounter|
                                 |OWLAPI-isSatisfiable|
                                 |OWLAPI-dispose|
                                 |OWLAPI-clearOntologies|
                                 |OWLAPI-unloadOntology|
                                 |OWLAPI-unloadOntologies|
                                 |OWLAPI-isDefinedIndividual|
                                 |OWLAPI-isDefinedDataProperty|
                                 |OWLAPI-isDefinedObjectProperty|
                                 |OWLAPI-isDefinedClass|
                                 |OWLAPI-realize|
                                 |OWLAPI-isRealised|
                                 |OWLAPI-classify|
                                 |OWLAPI-isClassified|
                                 |OWLAPI-considerDeclarations|
                                 |OWLAPI-ignoreDeclarations|
                                 |OWLAPI-keepAnnotations|
                                 |OWLAPI-ignoreAnnotations|
                                 |OWLAPI-disableMemorySavingMode|
                                 |OWLAPI-enableMemorySavingMode|
                                 |OWLAPI-batchSynchronize|
                                 |OWLAPI-isEntailed|
                                 |OWLAPI-disableTransientAxiomMode|
                                 |OWLAPI-enableTransientAxiomMode|
                                 |OWLAPI-disableLookupMode|
                                 |OWLAPI-enableLookupMode|
                                 |OWLAPI-disableAutoMode|
                                 |OWLAPI-autoBatchRemoveAxiomsFrom|
                                 |OWLAPI-autoRemoveAxiomsFrom|
                                 |OWLAPI-autoBatchAddAxiomsTo|
                                 |OWLAPI-autoAddAxiomsTo|
                                 |OWLAPI-getAutoOntology|
                                 |OWLAPI-getAutoDeclareDataProperties|
                                 |OWLAPI-setAutoDeclareDataProperties|
                                 |OWLAPI-reloadLoadedOntologies|
                                 |OWLAPI-disposeOntologies|
                                 |OWLAPI-loadOntologies|
                                 |OWLAPI-getAllOntologies|
                                 |OWLAPI-contains|
                                 |OWLAPI-getLoadedOntologies|
                                 |OWLAPI-getOntologies|
                                 |OWLAPI-loadOntology|
                                 |OWLAPI-disposeOntology|
                                 |OWLAPI-mergeOntologies|
                                 |OWLAPI-newOntology|
                                 |OWLAPI-describeOntologies|
                                 |OWLAPI-describeOntology|
                                 |OWLAPI-describeReasoners|
                                 |OWLAPI-describeReasoner|
                                 |OWLAPI-isAsymmetric|
                                 |OWLAPI-isIrreflexive|
                                 |OWLAPI-isReflexive|
                                 |OWLAPI-isTransitive|
                                 |OWLAPI-isSymmetric|
                                 |OWLAPI-isInverseFunctional|
                                 |OWLAPI-getInverseProperties|
                                 |OWLAPI-getDisjointDataProperties|
                                 |OWLAPI-getDisjointObjectProperties|
                                 |OWLAPI-getDisjointClasses|
                                 |OWLAPI-isFunctional|
                                 |OWLAPI-getRanges|
                                 |OWLAPI-getDomains|
                                 |OWLAPI-getEquivalentProperties|
                                 |OWLAPI-getDescendantProperties|
                                 |OWLAPI-getAncestorProperties|
                                 |OWLAPI-getSubProperties|
                                 |OWLAPI-getSuperProperties|
                                 |OWLAPI-getDifferentIndividuals|
                                 |OWLAPI-getDataPropertyValues|
                                 |OWLAPI-getObjectPropertyValues|
                                 |OWLAPI-getInstances|
                                 |OWLAPI-isDifferentIndividual|
                                 |OWLAPI-isSameIndividual|
                                 |OWLAPI-getSameIndividuals|
                                 |OWLAPI-getRelatedValues|
                                 |OWLAPI-getRelatedIndividuals|
                                 |OWLAPI-hasDataPropertyRelationship|
                                 |OWLAPI-hasObjectPropertyRelationship|
                                 |OWLAPI-hasType|
                                 |OWLAPI-getDataPropertyRelationships|
                                 |OWLAPI-getObjectPropertyRelationships|
                                 |OWLAPI-getIndividuals|
                                 |OWLAPI-getTypes|
                                 |OWLAPI-isConsistent|
                                 |OWLAPI-getInconsistentClasses|
                                 |OWLAPI-getEquivalentClasses|
                                 |OWLAPI-getDescendantClasses|
                                 |OWLAPI-getSubClasses|
                                 |OWLAPI-getAncestorClasses|
                                 |OWLAPI-getSuperClasses|
                                 |OWLAPI-isEquivalentClass|
                                 |OWLAPI-isSubClassOf|
                                 |OWLAPI-isClass|
                                 |OWLAPI-findIDFromObject|
                                 |OWLAPI-findObjectFromID|
                                 |OWLAPI-registerObject|
                                 |OWLAPI-registerLastAnswer|
                                 |OWLAPI-clearRegistry|
                                 |OWLAPI-setReturnPolicy|
                                 |OWLAPI-nextAxiomUseID|
                                 |OWLAPI-dontRegisterDeclaredEntities|
                                 |OWLAPI-registerDeclaredEntities|
                                 |OWLAPI-dontRegisterReferencedEntities|
                                 |OWLAPI-registerReferencedEntities|
                                 |OWLAPI-usesIncrementalUpdates|
                                 |OWLAPI-disableIncrementalUpdates|
                                 |OWLAPI-enableIncrementalUpdates|
                                 |OWLAPI-usesSimplifiedProtocol|
                                 |OWLAPI-disableSimplifiedProtocol|
                                 |OWLAPI-enableSimplifiedProtocol|
                                 |OWLAPI-init|
                                 |OWLAPI-abort|
                                 |OWLAPI-sleep|
                                 |OWLAPI-getReasoners|
                                 |OWLAPI-getCurrentReasoner|
                                 |OWLAPI-setCurrentReasoner|
                                 |OWLAPI-disposeReasoner|
                                 |OWLAPI-newReasoner|
                                 |OWLAPI-newReasoner1|
                                 get-agraph-version
                                 get-nrql-version
                                 restore-server-image
                                 store-server-image
                                 restore-all-substrates
                                 restore-substrate
                                 store-all-substrates
                                 store-substrate-for-abox
                                 check-nrql-subscriptions
                                 disable-rcc-substrate-mirroring
                                 enable-rcc-substrate-mirroring
                                 del-rcc-edge1
                                 del-rcc-node1
                                 rcc-edge-description1
                                 rcc-node-description1
                                 rcc-edge-label1
                                 rcc-node-label1
                                 rcc-edge1
                                 rcc-node1
                                 rcc-related1
                                 rcc-instance1
                                 delete-rcc-synonyms
                                 register-rcc-synonym
                                 rcc-consistent-p
                                 create-rcc-edge
                                 create-rcc-node
                                 set-rcc-box
                                 set-mirror-data-box
                                 get-edge-label-for-non-existent-edges
                                 set-edge-label-for-non-existent-edges
                                 description-implies-p
                                 edge-description1
                                 node-description1
                                 edge-label1
                                 node-label1
                                 del-data-edge1
                                 del-data-node1
                                 data-edge1
                                 data-node1
                                 set-data-box
                                 get-data-edge-description
                                 get-data-edge-label
                                 get-data-node-description
                                 get-data-node-label
                                 delete-data-edge
                                 delete-data-node
                                 create-data-edge
                                 create-data-node
                                 describe-all-edges
                                 get-substrate-edges
                                 describe-all-nodes
                                 get-substrate-nodes
                                 describe-all-substrates
                                 describe-substrate
                                 rmi
                                 get-proxy-server
                                 set-proxy-server
                                 check-ontology
                                 check-concept-coherence
                                 restore-standard-settings
                                 set-nrql-mode
                                 disable-query-realization
                                 enable-query-realization
                                 disable-query-repository
                                 enable-query-repository
                                 dont-report-inconsistent-queries-and-rules
                                 report-inconsistent-queries-and-rules
                                 describe-query-processing-mode
                                 describe-current-substrate
                                 get-process-pool-size
                                 get-maximum-size-of-process-pool
                                 get-initial-size-of-process-pool
                                 set-maximum-size-of-process-pool
                                 set-initial-size-of-process-pool
                                 set-rewrite-defined-concepts
                                 optimizer-dont-ensure-late-lambda-evaluation
                                 optimizer-ensure-late-lambda-evaluation
                                 optimizer-dont-use-cardinality-heuristics
                                 optimizer-use-cardinality-heuristics
                                 optimizer-get-time-bound
                                 optimizer-set-time-bound
                                 optimizer-get-no-of-plans-upper-bound
                                 optimizer-set-no-of-plans-upper-bound
                                 disable-query-optimization
                                 enable-query-optimization
                                 include-permutations
                                 exclude-permutations
                                 dont-add-rule-consequences-automatically
                                 add-rule-consequences-automatically
                                 process-set-at-a-time
                                 process-tuple-at-a-time
                                 get-max-no-of-tuples-bound
                                 set-max-no-of-tuples-bound
                                 dont-check-abox-consistency-before-querying
                                 check-abox-consistency-before-querying
                                 enable-lazy-tuple-computation
                                 enable-eager-tuple-computation
                                 dont-add-role-assertions-for-datatype-properties
                                 add-role-assertions-for-datatype-properties
                                 disable-told-information-querying
                                 enable-told-information-querying
                                 disable-nrql-warnings
                                 enable-nrql-warnings
                                 disable-kb-has-changed-warning-tokens
                                 enable-kb-has-changed-warning-tokens
                                 disable-phase-two-starts-warning-tokens
                                 enable-phase-two-starts-warning-tokens
                                 disable-two-phase-query-processing-mode
                                 enable-two-phase-query-processing-mode
                                 enable-very-smart-abox-mirroring
                                 enable-smart-abox-mirroring
                                 disable-abox-mirroring
                                 enable-abox-mirroring
                                 dont-use-injective-variables-by-default
                                 use-injective-variables-by-default
                                 dont-use-individual-synonym-equivalence-classes
                                 use-individual-synonym-equivalence-classes
                                 dont-add-missing-top-conjuncts
                                 add-missing-top-conjuncts
                                 disable-data-substrate-mirroring
                                 enable-data-substrate-mirroring
                                 wait-for-rules-to-terminate
                                 wait-for-queries-to-terminate
                                 get-all-answers
                                 execute-or-reexecute-all-rules
                                 run-all-rules
                                 reexecute-all-rules
                                 execute-all-rules
                                 execute-or-reexecute-all-queries
                                 reexecute-all-queries
                                 run-all-queries
                                 execute-all-queries
                                 abort-all-rules
                                 abort-all-queries
                                 describe-all-definitions
                                 describe-definition
                                 delete-all-definitions
                                 activate-defined-query
                                 deactivate-defined-query
                                 dont-keep-defined-query-atoms
                                 keep-defined-query-atoms
                                 enable-lazy-unfolding-of-defined-queries
                                 disable-lazy-unfolding-of-defined-queries
                                 enable-defined-queries
                                 disable-defined-queries
                                 dont-prefer-defined-queries
                                 prefer-defined-queries
                                 dont-allow-overloaded-definitions
                                 allow-overloaded-definitions
                                 undefine-query
                                 define-and-prepare-query
                                 define-and-execute-query
                                 define-query
                                 inaccurate-rules
                                 accurate-rules
                                 inaccurate-queries
                                 accurate-queries
                                 sleeping-expensive-rules
                                 sleeping-cheap-rules
                                 waiting-expensive-rules
                                 waiting-cheap-rules
                                 sleeping-expensive-queries
                                 sleeping-cheap-queries
                                 waiting-expensive-queries
                                 waiting-cheap-queries
                                 running-expensive-rules
                                 running-cheap-rules
                                 running-expensive-queries
                                 running-cheap-queries
                                 expensive-rules
                                 cheap-rules
                                 expensive-queries
                                 cheap-queries
                                 terminated-rules
                                 processed-rules
                                 terminated-queries
                                 processed-queries
                                 sleeping-rules
                                 waiting-rules
                                 sleeping-queries
                                 waiting-queries
                                 running-rules
                                 running-queries
                                 active-rules
                                 active-queries
                                 prepared-rules
                                 ready-rules
                                 prepared-queries
                                 ready-queries
                                 answer-tbox-query1
                                 answer-tbox-query
                                 racer-answer-tbox-query1
                                 racer-answer-tbox-query
                                 racer-prepare-tbox-query1
                                 racer-prepare-tbox-query
                                 move-rules
                                 copy-rules
                                 apply-rule-under-premise1
                                 apply-rule-under-premise
                                 apply-rule
                                 racer-apply-rule-under-premise1
                                 racer-apply-rule1
                                 racer-apply-rule-under-premise
                                 racer-apply-rule
                                 prepare-rule1
                                 prepare-rule
                                 racer-prepare-rule1
                                 racer-prepare-rule
                                 answer-query-under-premise1
                                 answer-query-under-premise
                                 racer-answer-query-under-premise1
                                 racer-answer-query-under-premise
                                 answer-query1
                                 answer-query
                                 racer-answer-query1
                                 racer-answer-query
                                 get-answer-size
                                 prepare-query1
                                 prepare-query
                                 racer-prepare-query1
                                 racer-prepare-query
                                 delete-all-rules
                                 delete-all-queries
                                 describe-all-rules
                                 describe-all-queries
                                 all-rules
                                 all-queries
                                 all-different-from-assertions
                                 all-same-as-assertions
                                 get-abox-graph
                                 get-role-hierarchy
                                 get-concept-properties
                                 get-individual-annotation-fillers
                                 get-individual-annotation-datatype-fillers
                                 get-individual-datatype-fillers
                                 get-individual-successors
                                 prepare-nrql-engine
                                 in-unsafe-mode?
                                 exit-server
                                 full-reset
                                 reset-nrql-engine
                                 all-substrates
                                 delete-all-substrates
                                 get-substrate-type
                                 set-substrate-type
                                 reset-all-substrates
                                 server-value
                                 get-all-server-values
                                 get-all-values
                                 unbind-all
                                 unbind1
                                 defpar1
                                 defcon1
                                 server-function
                                 get-all-server-functions
                                 get-all-functions
                                 fcall
                                 undefine-all
                                 undefine1
                                 define1
                                 evaluate1
                                 set-racer-parameter)
     (let* ((saved-timeout *server-timeout*) (*server-timeout* nil))
       (answer expr
               state
               stream
               n
               (let ((*server-timeout* saved-timeout))
                 (apply (symbol-function (first expr)) (rest expr)))
               output-string-stream)))))

    
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun sort-symbols (list)
    (sort (copy-list list)
          #'string<
          :key #'symbol-name))

  )

(defun my-set-difference (a b)
  (set-difference a b 
                  :key #'symbol-name
                  :test #'string-equal))

(defun my-set-intersection (a b)
  (intersection a b 
                :key #'symbol-name
                :test #'string-equal))


(defconstant +cur-server-functions+
  ;;; alles, was process-racer-expr versteht: 
  (sort-symbols
   (remove-duplicates
     (reduce #'append
             (mapcar #'ensure-list 
                     (mapcar #'car +server-case+))))))

(defconstant +excluded+ nil)

;;;
;;;
;;;

(defun get-racer-symbols ()
  (sort-symbols
   (loop as p being the external-symbol in :racer 
         as loc =  (dspec:find-dspec-locations p)
         when (and (fboundp p)
                   (consp loc) 
                   (let ((x (second (first loc))))
                     (or (eq x :implicit)
                         (and (typep x 'pathname)
                              (member (pathname-host x)
                                      '("RACER" 
                                        "NRQL"
                                        "NRQL-DEV"
                                        "OWLAPI"
                                        "OWL-SYNTAXES"
                                        "ONLINE-DOC"
                                        "ABDUCTION"
                                        "ABOX-DIFF"
                                        "LAMBDA-REGISTRY"
                                        "PATCHES")
                                      :test #'string-equal)))))
         collect p)))

(defun get-all-racer-symbols ()
  (sort-symbols
   (append 
    (loop as p being the external-symbol in :racer 
          collect p)
    ts::+reserved-tokens+
    ;;; needed for sirius: new-ind -> NEW-IND, t -> T in ALisp, etc. 
    '(t nil))))

;;;
;;;
;;; 

(defun get-racer-api ()
  (sort-symbols +cur-server-functions+))

(defun get-racer-api-without-nrql ()
  (sort-symbols
   (set-difference +cur-server-functions+
                   (mapcar #'first 
                           (remove-if #'fourth
                                      (append ts::*nrql-functions*
                                              ts::*nrql-methods*
                                              ts::*nrql-macros*
                                              ts::*nrql-with-macros*))))))

(defun get-racer-functions ()
  (let ((with-macros (get-racer-with-macros)))
    (sort-symbols
     (remove-duplicates
      (append 
       ;;; nRQL: erforderlich fuer Bootstrapping!
       (mapcar #'first 
               (remove-if #'fourth
                          (append ts::*nrql-functions*
                                  ts::*nrql-methods*)))
       (remove-if #'(lambda (x) 
                      (or (macro-function x)
                          (member x with-macros)))
                  +cur-server-functions+))))))

(defun get-racer-macros ()
  (let ((with-macros (get-racer-with-macros)))
    (sort-symbols
     (remove-duplicates
      (append 
       ;;; nRQL: erforderlich fuer Bootstrapping!
       (mapcar #'first 
               (remove-if #'fourth 
                          ts::*nrql-macros*))
       (remove-if #'(lambda (x) 
                      (or (not (macro-function x))
                          (member x with-macros)))
                  +cur-server-functions+))))))

(defun get-racer-with-macros ()
  (sort-symbols
   (remove-duplicates
    (append
     ;;; nRQL: erforderlich fuer Bootstrapping!
     (mapcar #'first 
             (remove-if #'fourth ts::*nrql-with-macros*))
     (remove-if-not
      #'(lambda (x) 
          (or (and (> (length (symbol-name x)) 5)
                   (string-equal "with-" (subseq (symbol-name x) 0 5)))
              (and (> (length (symbol-name x)) 9)
                   (string-equal "without-" (subseq (symbol-name x) 0 8)))))
      +cur-server-functions+)))))

;;;
;;;
;;;
 
(defun additional-functions ()
  (format t "~%~%These functions are exported (fbound) RACER functions, but are not recognized by the server interface:~%")
  (let ((res
         (sort-symbols
          (reduce #'my-set-difference 
                  (list (get-racer-symbols)
                        (get-racer-api)
                        +excluded+)))))
    (pprint res)
    (terpri)
    res))

(defun missing-functions ()
  (format t "~%~%These functions are recognized by the server interface, but are not exported as RACER functions:~%")
  (let ((res  
         (sort-symbols
          (reduce #'my-set-difference 
                  (list (get-racer-api) 
                        (get-racer-symbols)
                        (mapcar #'get-owlapi-synonym
                                (get-racer-symbols))
                        +excluded+)))))
    (pprint res)
    (terpri)
    res))

(defun undefined-server-functions ()
  (format t "~%~%These functions are recognized by the server interface, but have no implementation as functions / macros (not fboundp):~%")
  (let ((res  
         (sort-symbols
          (remove-if #'(lambda (x)
                         (or (fboundp x)
                             (fboundp (get-alias-fn x))))
                     (get-racer-api)))))
    (pprint res)
    (terpri)
    res))

(defun check-server-interface ()
  (additional-functions)
  (undefined-server-functions)
  (missing-functions)
  (format t "~%Difference of last two: ~A / ~A~%~%" 
          (set-difference (undefined-server-functions)
                          (missing-functions))
          (set-difference (missing-functions)
                          (undefined-server-functions)))
  :okay)

;;;
;;;
;;;

(defun make-standard-name (string)
  (let ((parts (split-sequence "-" string))
        (new-parts nil))

    (loop while parts do
          (let ((part (pop parts)))

            (if (not (and (some #'upper-case-p part)
                          (some #'lower-case-p part)))
                (if parts
                    (push (concatenate 'string (string-downcase part) "-") new-parts)
                  (push (string-downcase part) new-parts))
              (let ((chars nil))

                (dolist (char (coerce part 'list))
                  (let ((ucase (upper-case-p char)))
                    (cond (ucase
                           (when chars
                             (push (string-downcase (coerce (nreverse (cons #\- chars)) 'string)) new-parts)
                             (setf chars nil))
                           (push char chars))
                 
                          (t (push char chars)))))

                (when chars (push (string-downcase (coerce (nreverse chars) 'string)) new-parts))))))

    (ts:string-substitute
     (apply #'concatenate 'string (nreverse new-parts))
     `(("o-w-l" "OWL" nil)
       ("owlapi-d" "owlapi-d" nil)
       ("i-d" "ID" nil)
       ("X-M-L" "XML" nil)
       ("x-m-l" "xml" nil)
       ("u-r-i" "URI" nil)))))

        

(defun get-owlapi-synonym (sym)
  (let ((name (symbol-name sym)))
    (when (and (search "OWLAPI" name)
               (some #'upper-case-p name)
               (some #'lower-case-p name))
      (intern (string-upcase (make-standard-name name)) :ts))))


(defun get-owlapi-synonyms ()
  (let ((syns nil)
        (new-names nil))
    (dolist (sym (get-racer-symbols))
      (let ((new-name (get-owlapi-synonym sym)))
        (when new-name
          (if (member new-name new-names)
              (nrql-warning "I already have symbol ~A! Make sure it is a synonym" new-name)
            (unless (eq new-name sym)
              (push `(owlapi:owlapi-defun (,new-name) (&rest args)
                     (apply #',sym args))
                    syns)
              (push new-name new-names))))))
    (cons 'progn syns)))

