;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: RACER; Base: 10; readtable: racer -*-

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

(defparameter *print-where-am-i* nil)
(defparameter *mark-primitive-concept-names* t)
(defparameter *tick-function* 
  #'(lambda (ratio)
      (racer:set-progress-value (* 100 ratio))))
(defparameter *minimum-n-concepts-for-classification-progress-indication* 20)

(declaim (special *file-external-format*))

(defun make-tick-function ()
  (let ((n 0) (m 0))
    (lambda (ratio) 
      (let ((n1 (round (* 100 ratio))))
	(racer:set-progress-value n1)
	(when (> n1 n)
	  (loop for i from n below n1 do 
                (princ ".")
                (incf m)
                (when (zerop (mod m 10)) (princ m)))
	  (setf n n1))))))
		     
		     
(defparameter *auto-install-primitives* t
  "If this variable is true, concepts not mentioned as the lefthand side of 
   any axiom are assumed to be primitive subconcepts of top.")

(defparameter *auto-install-roles* t
  "If this variable is true, roles not mentioned in the :roles declaration 
are automatically installed.")

(defparameter *auto-classify* :lazy-verbose
  "Controls whether a TBox is automatically classified
   after it is defined with define-tbox.")

(defvar *tbox-table* (racer-make-hash-table))

(defparameter *default-tbox-concept-size* 100)
(defparameter *default-tbox-role-size* 100)

(defun all-tboxes ()
  (loop for tbox being the hash-values of *tbox-table*  
        collect (tbox-name tbox)))

(defmacro loop-over-tboxes ((tbox-sym) &rest forms)
  (check-type tbox-sym symbol)
  `(loop for ,tbox-sym being the hash-values of *tbox-table* .,forms))

(defvar *visited-nodes*)

(defvar *n-inserted-concepts*)

(defvar *tbox-node-currently-inserting* nil)

(defvar *disjoints-table*)

(defparameter *initial-disjoints-table-size* 20)

(defvar *sort-counter*)

(defparameter *fully-sorted-concept-list* t)
(defparameter *told-subsumer-sorted-concept-list* nil) ;;vh: still experimental
(defparameter *simple-sorted-concept-list* t)
(defparameter *ignore-told-disjoint-information-during-bottom-search* nil)

;;; ======================================================================

(defstruct (el+-environment (:conc-name env-))
  (tbox-language nil)
  (dag nil)
  (role-members nil)
  (nary-absorptions nil)
  (inverse-role-compositions nil)
  (concepts-used-in-domain-qualification (racer-make-hash-table))
  (role-chain-member-list nil)
  (composed-roles nil)
  )

;;; ======================================================================

(defun ensure-tbox-name (abox)
  (if (symbolp abox)
    abox
    (tbox-name abox)))


;;; ======================================================================
;;;
;;; SAT tester interface

(defun create-tbox-internal-marker-concept (tbox &optional (marker-name (gensym)))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (setf (tbox-internal-marker-concept marker-name tbox) t)
  marker-name)

(defun ensure-atomic-concept-in-tbox (tbox concept-name)
  (if (eq concept-name *tbox-node-currently-inserting*)
    (make-atomic-concept :term concept-name)
    (let ((found-concept (get-tbox-concept tbox concept-name nil))
          (internal-marker-concept (tbox-internal-marker-concept concept-name tbox)))
      (when (null found-concept)
        (unless internal-marker-concept
          (unless (symbol-package concept-name)
            (error "Racer cannot handle symbols without package: ~S"
                   concept-name))
          (if (and (tbox-atomic-concepts tbox)
                   (not (member concept-name
                                (tbox-atomic-concepts tbox))))
            (error "Concept ~S not mentioned in signature of TBox ~S."
                   concept-name (tbox-name tbox))
            (unless *auto-install-primitives*
              (error "Concept ~S not yet defined in TBox ~S."
                     concept-name
                     (tbox-name tbox)))))
        (let ((concept (make-atomic-concept :term concept-name)))
          (setf (concept-name-set concept) (list concept-name))
          (setf (concept-told-subsumers concept) (list concept))
          (setf (concept-primitive-p concept) t)
          (when internal-marker-concept
            (setf (concept-visible-p concept) nil))
          (unless internal-marker-concept
            (cond ((tbox-classified-p-internal tbox)
                   (insert-concept-node tbox
                                        concept
                                        (list (tbox-top-node tbox))
                                        (list (tbox-bottom-node tbox)))
                   (push concept (tbox-auto-installed-primitives tbox)))
                  ((tbox-index-structures-complete-p tbox)
                   (push concept (tbox-encoded-concept-list tbox))
                   (push concept (tbox-auto-installed-primitives tbox)))
                  (t (if (boundp '*provisionally-inserted-atomic-concepts*)
                       (push concept *provisionally-inserted-atomic-concepts*)
                       (error "Ignoring newly added atomic concept ~S" concept)))))
          concept)))))

(defun ensure-role-in-tbox (tbox role-name)
  (when (and (tbox-signature tbox)
             (not (get-role role-name)))
    (error "Role ~S not mentioned in signature of TBox ~S."
           role-name 
           (tbox-name tbox)))
  (unless (get-tbox-role tbox role-name nil)
    (push role-name (tbox-all-roles tbox))
    (push (list 'inv role-name) (tbox-all-roles tbox)))
  role-name)

;;; ======================================================================

(defstruct (concept-axiom-info (:type list))
  left
  right
  (primitive-marker nil))

(defstruct (role-info (:type list)
                      (:constructor make-role-info
                                    (role-name cd-attribute 
                                               feature-p
                                               transitive-p
                                               parents
                                               inverse
                                               inverse-feature-p
                                               domain
                                               range
                                               reflexive-p
                                               datatype
                                               annotation-p
					       irreflexive-p
					       symmetric-p
					       asymmetric-p
                                               compositions)))
  (role-name nil)
  (cd-attribute nil)
  (feature-p nil)
  (transitive-p nil)
  (parents nil)
  (inverse nil)
  (inverse-feature-p nil)
  (domain nil)
  (range nil)
  (reflexive-p nil)
  (datatype nil)
  (annotation-p nil)
  (irreflexive-p nil)
  (symmetric-p nil)
  (asymmetric-p nil)
  (compositions nil))

;;; ======================================================================

(race-inline (inclusion-axiom-p parse-axiom))

(defun inclusion-axiom-p (axiom)
  (member (concept-axiom-info-primitive-marker axiom) '(:prim :primitive :inclusion t)))

(defun parse-axiom (axiom-spec)
  (if (inclusion-axiom-p axiom-spec)
    (setf (third axiom-spec) :inclusion)
    (when (third axiom-spec)
      (error "Illegal tbox axiom ~S" axiom-spec)))
  axiom-spec)

;;; **********************************************************************

(defun make-concept-axioms-index (tbox &optional (size nil))
  (racer-make-hash-table :size (or size (tbox-expected-concept-size tbox))))

(defun make-role-axioms-index (tbox &optional (size nil))
  (racer-make-hash-table :size (or size (tbox-expected-role-size tbox))))

(defun make-taxonomic-encoding-table ()
  (racer-make-hash-table))


;;; **********************************************************************

(defmacro with-tbox-defined-check (tbox &body body)
  `(if ,tbox
     (progn ,@body)
     (error "No current TBox set")))

(defun tbox-classified-p (&optional (tbox *current-tbox*))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (tbox-classified-p-internal tbox))

(defmacro tbox-classified? (&optional (tbox-name nil tbox-name-specified-p))
  (if tbox-name-specified-p
    (progn
      (check-type tbox-name symbol)
      `(tbox-classified-p-internal (find-tbox ',tbox-name)))
    `(with-tbox-defined-check *current-tbox*
       (tbox-classified-p-internal *current-tbox*))))

(defun all-roles (&optional (tbox *current-tbox*)
                            &key (count nil)
                            (test nil)
                            (inverse-test nil)
                            (default nil))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  ;; We sort the output such that inverse roles appear first.
  ;; This is used by some software.
  (possibly-count-only
   (loop for role in (tbox-all-roles tbox)
         when (and (symbolp role)
                   (not (role-internal-name-p (get-tbox-role tbox role)))
                   (or (null test) (funcall test role)))
         collect role into roles
         when (and (and (consp role) (eq (first role) 'inv))
                   (not (or (eq (second role) +top-datatype-role-symbol+)
                            (eq (second role) +bottom-datatype-role-symbol+)))
                   (or (null inverse-test)
                       (funcall inverse-test role)))
         collect role into inv-roles
         finally (return (or (append inv-roles roles) default)))
   count)
  ;; Rice relies on (inv r) being included, so the following is not used.
  #|(loop for role in (tbox-all-roles tbox)
        when (symbolp role)
        collect role)|#)

(defun all-features (&optional (tbox *current-tbox*)
                                 &key (count nil))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (possibly-count-only (tbox-all-features tbox) count))

(defun all-transitive-roles (&optional (tbox *current-tbox*)
                                           &key (count nil))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (possibly-count-only (tbox-all-transitive-roles tbox) count))

(defun all-attributes (&optional (tbox *current-tbox*)
                                    &key (count nil))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (possibly-count-only
   (loop for attribute in (tbox-all-attributes tbox)
         unless (member attribute +internal-roles+)
         collect attribute)
   count))

(defun attribute-type (attribute-name &optional (tbox *current-tbox*))
  (check-type attribute-name symbol)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (unless (role-p attribute-name tbox)
    (error "Attribute ~S is undefined in TBox ~A." attribute-name (tbox-name tbox)))
  (role-cd-attribute (get-tbox-role tbox attribute-name)))

(defun get-tbox-language (&optional (tbox *current-tbox*))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (with-output-to-string (stream)
    (print-dl-description (tbox-language tbox) stream)))


(defun get-concept-definition-1 (concept-name tbox)
  (check-type concept-name symbol)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (concept-definition (get-tbox-concept tbox concept-name)))

(defmacro get-concept-definition (concept-name &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(get-concept-definition-1 ',concept-name ',tbox)
    `(get-concept-definition-1 ',concept-name *current-tbox*)))

(defun get-concept-negated-definition-1 (concept-name tbox)
  (check-type concept-name symbol)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (let ((result (concept-encoded-negated-definition (get-tbox-concept tbox concept-name))))
    (if result
      (decode-concept result)
      nil)))

(defmacro get-concept-negated-definition (concept-name &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(get-concept-negated-definition-1 ',concept-name ',tbox)
    `(get-concept-negated-definition-1 ',concept-name *current-tbox*)))

(defun get-meta-constraint (&optional (tbox *current-tbox*))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (let ((meta-constraints (tbox-meta-constraint-concepts tbox)))
    (if (null meta-constraints)
      'top
      `(and .,(loop for concept in meta-constraints
                    collect (decode-concept concept))))))

;;; ======================================================================


(defmacro with-concept-definition-mapping (tbox &body forms)
  (let ((tbox-var (gensym)))
    `(let ((,tbox-var ,tbox))
       (with-alc-environment
         (:tbox
          ,tbox
          :id-variable (tbox-structure-id-counter ,tbox-var)
          :concept-store (tbox-concept-store ,tbox-var)
          :tableaux-cache (tbox-tableaux-cache ,tbox-var)
          :tableaux-sat-cache (tbox-tableaux-sat-cache ,tbox-var)
          :tableaux-unsat-cache (tbox-tableaux-unsat-cache ,tbox-var)
          :role-store (tbox-role-store ,tbox-var)
          :concrete-domain (tbox-concrete-domain ,tbox-var)
          :stable-set-difference-table (tbox-stable-set-difference-table ,tbox-var)
          :stable-set-difference-last-list2 (tbox-stable-set-difference-last-list2 ,tbox-var)
          :racer-remove-duplicates-table (tbox-racer-remove-duplicates-table ,tbox-var)
          :racer-remove-constraint-duplicates-table (tbox-racer-remove-constraint-duplicates-table ,tbox-var)
          :possible-subsumees-vector (tbox-possible-subsumees-vector ,tbox-var)
          :expanded-constraints-ind-table (tbox-expanded-constraints-ind-table ,tbox-var)
          :live-inds-table (tbox-live-inds-table ,tbox-var)
          :obsolete-inds-table (tbox-obsolete-inds-table ,tbox-var)
          :label-inds-table (tbox-label-inds-table ,tbox-var)
          :new-inds-table (tbox-new-inds-table ,tbox-var)
          :concept-set-mark (tbox-concept-set-mark ,tbox-var)
          :role-set-mark (tbox-role-set-mark ,tbox-var)
          :individual-set-mark (tbox-individual-set-mark ,tbox-var)
          :constraint-set-mark (tbox-constraint-set-mark ,tbox-var)
          :classification-counter (tbox-classification-counter ,tbox-var)
          :obsolete-eql-tables (tbox-obsolete-eql-tables ,tbox-var)
          :obsolete-equal-tables (tbox-obsolete-equal-tables ,tbox-var)
          :obsolete-equal-concept-tables (tbox-obsolete-equal-concept-tables ,tbox-var)
          :initial-hash-table-size (tbox-initial-hash-table-size ,tbox-var)
          :signatures-equal-table (tbox-signatures-equal-table ,tbox-var)
          :partitions-table (tbox-partitions-table ,tbox-var)
          :use-less-tbox-memory (tbox-use-less-memory ,tbox-var)
          :set-vector (tbox-set-vector ,tbox-var)
          :select-disjunct-table (tbox-select-disjunct-table ,tbox-var)
          )
         (with-alc-bindings
           . ,forms)))))

(defmethod print-object ((object tbox) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (tbox-name object))))

(defun copy-tbox (tbox)
  (let ((clone (copy-tbox-internal tbox))
        (*current-tbox* *current-tbox*))
    (init-tbox clone :original tbox)
    (setf (tbox-original-concept-axioms clone) (tbox-original-concept-axioms tbox))
    (setf (tbox-concept-axioms clone) (copy-list (tbox-concept-axioms tbox)))
    (setf (tbox-concept-axioms-index clone) (copy-hash-table (tbox-concept-axioms-index tbox)))
    (setf (tbox-generalized-concept-inclusions clone) (tbox-generalized-concept-inclusions tbox))
    (setf (tbox-role-axioms clone) (tbox-role-axioms tbox))
    (setf (tbox-role-axioms-index clone) (copy-hash-table (tbox-role-axioms-index tbox)))
    (setf (tbox-disjoint-set clone) (copy-hash-table (tbox-disjoint-set tbox)))
    (setf (tbox-ontologies clone) (copy-list (tbox-ontologies tbox)))
    (setf (tbox-role-synonyms clone) (copy-list (tbox-role-synonyms tbox)))
    (setf (tbox-role-antonyms clone) (copy-list (tbox-role-antonyms tbox)))
    (setf (tbox-object-bottom-role clone) (tbox-object-bottom-role tbox))
    (setf (tbox-datatype-bottom-role clone) (tbox-datatype-bottom-role tbox))
    (setf (find-tbox (tbox-name tbox)) tbox)
    (setf (tbox-nominal-table clone) (copy-hash-table (tbox-nominal-table tbox)))
    (setf (tbox-role-characteristics-checked-p clone)
          (copy-hash-table (tbox-role-characteristics-checked-p tbox)))
    clone))

(defun initialize-tbox (tbox)
  (setf (tbox-top-node tbox) (make-top-concept))
  (setf (tbox-bottom-node tbox) (make-bottom-concept))
  (setf (tbox-datatype-top-node tbox) (make-top-datatype-concept))
  (setf (tbox-datatype-bottom-node tbox) (make-bottom-datatype-concept))
  (setf (tbox-object-top-role tbox) (make-top-object-role))
  (setf (tbox-object-bottom-role tbox) (make-bottom-object-role))
  (setf (tbox-datatype-top-role tbox) (make-top-datatype-role)) 
  (setf (tbox-datatype-bottom-role tbox) (make-bottom-datatype-role)) 
  (setf (tbox-concept-store tbox) nil)
  (setf (tbox-tableaux-cache tbox) (make-new-tableaux-cache))
  (setf (tbox-tableaux-sat-cache tbox) (make-new-sat-tableaux-cache))
  (setf (tbox-tableaux-unsat-cache tbox) (make-new-unsat-tableaux-cache))
  (setf (tbox-concrete-domain tbox) (create-concrete-domain))
  (setf (tbox-nominal-table tbox) (racer-make-hash-table))
  tbox)

(defun initialize-concept-role-store (tbox)
  (setf (tbox-concept-store tbox)
        (make-new-concept-store (tbox-top-node tbox)
                                (tbox-bottom-node tbox)
                                (tbox-datatype-top-node tbox)
                                (tbox-datatype-bottom-node tbox)
                                tbox))
  (setf (tbox-role-store tbox) (make-new-role-store tbox)))

(defun make-tbox (name roles transitive-roles features axioms expected-concept-size expected-role-size)
  (let ((tbox (make-tbox-internal :name name
                                  :expected-concept-size expected-concept-size
                                  :expected-role-size expected-role-size)))
    (initialize-tbox tbox)
    (setf (tbox-concept-axioms-index tbox) (make-concept-axioms-index tbox))
    (setf (tbox-role-axioms-index tbox) (make-role-axioms-index tbox))
    (initialize-roles-and-concepts tbox roles transitive-roles features nil axioms nil)
    tbox))

(defun initialize-roles-and-concepts (tbox 
                                           roles
                                           transitive-roles
                                           features
                                           attributes
                                           axioms
                                           atomic-concepts)
  (loop for atomic-concept in atomic-concepts do
        (assert (symbolp atomic-concept)
                (atomic-concepts)
                "Specifications for atomic concepts must be symbols."))
  (setf (tbox-atomic-concepts tbox) atomic-concepts)
  (loop for axiom in axioms do
        (if (inclusion-axiom-p axiom)
          (add-concept-axiom tbox
                             (concept-axiom-info-left axiom) 
                             (concept-axiom-info-right axiom)
                             :inclusion-p t)
          (add-concept-axiom tbox
                             (concept-axiom-info-left axiom) 
                             (concept-axiom-info-right axiom)
                             :inclusion-p nil)))
  (loop for (type name) in attributes do
        (add-role-axioms tbox name :cd-attribute type))
  (loop with t-roles = transitive-roles
        with f-roles = features
        for role-axiom in roles
        for role = (if (consp role-axiom)
                     (first role-axiom)
                     role-axiom)
        for t-flag = (or (and (member role t-roles) '(:transitive-p t))
                         (and (member role t-roles) '(:transitive t)))
        for f-flag = (or (and (member role f-roles) '(:feature-p t))
                         (and (member role f-roles) '(:feature t)))
        do
        (when t-flag
          (setf t-roles (remove role t-roles)))
        (when f-flag
          (setf f-roles (remove role f-roles)))
        (if (consp role-axiom)
          (apply #'add-role-axioms tbox (append role-axiom t-flag f-flag))
          (apply #'add-role-axioms tbox (append (list role-axiom) t-flag f-flag)))
        finally
        (loop for role in t-roles do
              (if (consp role)
                (apply #'add-role-axioms tbox (append role '(:transitive-p t)))
                (add-role-axioms tbox role :transitive-p t)))
        (loop for feature in f-roles do
              (if (consp feature)
                (apply #'add-role-axioms tbox (append feature '(:feature-p t)))
                (add-role-axioms tbox feature :feature-p t)))))

(defun add-concept-axiom (tbox left right &key (inclusion-p nil))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (let ((use-less-tbox-memory (if (boundp '*use-less-tbox-memory*)
				  *use-less-tbox-memory*
                                (tbox-use-less-memory tbox))))
    (when (or (tbox-classified-p-internal tbox) 
              (tbox-coherence-checked-p tbox)
              (tbox-index-structures-complete-p tbox))
      (when (tbox-use-less-memory tbox)
	(racer-warn "~&Updating of Tbox ~A only partially supported due to the ~
                   current memory management strategy. Start Racer with option -x to ~
                   enable these services.~%"
		    (tbox-name tbox))
	(return-from add-concept-axiom))
      (reset-tbox tbox))
    (when (and (eq left right) (symbolp left))
      (setf right nil)
      (setf inclusion-p t))
    (let ((right (or right +top-symbol+))
          (top-name-set (concept-name-set (tbox-top-node tbox)))
          (bottom-name-set (concept-name-set (tbox-bottom-node tbox))))
      (if (and (symbolp left)
               (not (or (member left top-name-set) (member left bottom-name-set))))
        (let ((index (tbox-concept-axioms-index tbox)))
          (multiple-value-bind (found-entry found)
                               (gethash left index)
            (unless use-less-tbox-memory
              (unless (and (member right top-name-set)
                           found)
                ;; Even ignore (implies <name> top) in the original concept axioms if already present
                (push (list left right inclusion-p) (tbox-original-concept-axioms tbox))))
            (if found
              (if inclusion-p
                (unless (member right top-name-set)
                  (if (and (member (first found-entry) top-name-set) ; only top as definition
                           (second found-entry) ; inclusion!
                           ) ;absorb GCI immediately
                    (setf (first found-entry) right)
                    (if (equal (first found-entry) right)
                      (racer-warn "Redundant definition ~S for ~S ignored." right left)
                      (push (list left right) (tbox-generalized-concept-inclusions tbox)))))
                (if (equal (first found-entry) right)
                  (if (second found-entry)
                    (progn
                      (racer-warn "Superseding redundant definition ~S for ~S." right left)
                      (setf (second (gethash left index)) t) ;replace inclusion by equivalent definition
                      )
                    (racer-warn "Redundant definition ~S for ~S ignored." right left))
                  (if (second found-entry) ; inclusion!
                    (progn
                      (setf (second found-entry) nil) ; now a definition!
                      (unless (member (first found-entry) top-name-set) ; not top as definition
                        (push (list left (first found-entry))
                              (tbox-generalized-concept-inclusions tbox)))
                      (setf (first found-entry) right))
                    (progn
                      #|(racer-warn "Duplicate definition ~S for ~S transformed into two GCIs."
                                  right left)|#
                      (push (list left right) (tbox-generalized-concept-inclusions tbox))
                      (push (list right left) (tbox-generalized-concept-inclusions tbox))))))
              (let ((index-entry (list right inclusion-p)))
                (setf (gethash left index) index-entry)
                (push (cons left index-entry) (tbox-concept-axioms tbox))))))
        (progn
          (unless use-less-tbox-memory
            (push (list left right inclusion-p) (tbox-original-concept-axioms tbox)))
          (unless (equal left right)
            (if inclusion-p
              (push (list left right) (tbox-generalized-concept-inclusions tbox))
              (progn
                (unless (member left bottom-name-set)
                  (push (list left right) (tbox-generalized-concept-inclusions tbox)))
                (unless (member left top-name-set)
                  ;; **************************************
                  ;; we add this condition to prevent (equiv top a) to be absorbed into
                  ;; an erroneous concept definition (non-primitive, definition is top).
                  (push (list right left) (tbox-generalized-concept-inclusions tbox))))))))))
  tbox)

;;; ======================================================================

(defparameter *get-tbox-concept-error-p* t)

(race-inline (get-tbox-concept))

(defun get-tbox-concept (tbox
                            concept-name
                            &optional (errorp *get-tbox-concept-error-p*))

  ;; MW 6.1.20010: this already has to happen at parse time (TOLD)
  ;; (setf concept-name (prepend-prefix concept-name tbox))
  
  (let* ((concept-store (tbox-concept-store tbox))
         (concept (and concept-store (gethash concept-name concept-store))))
    (if concept
      concept
      (if errorp
        (error "Undefined concept name ~A in TBox ~A"
               concept-name
               (tbox-name tbox))
        nil))))

;;; **********************************************************************

(defstruct (concepts-bucket (:conc-name bucket-))
  (concept-internal nil)                         ; super concept subsuming bucket members
  (parent nil)                          ; parent node of the bucket members
  (members nil)                         ; list of atomic concepts
  (mark1 nil)                           ; used for marking
  (members-count 0)                     ; length of members
  (members-language *dl-empty*)         ; dl language of bucket members
  (member-defined-p nil)                ; is at least one member a defined concept (used for EL+ models)
  )

(defmethod print-object ((object concepts-bucket) stream)
  (if *print-internals*
    (print-unreadable-object (object stream :type nil :identity t)
      (call-next-method))
    (print-unreadable-object (object stream :type t :identity nil)
      (format stream "[~D:~S->~S]"
              (bucket-members-count object)
              (bucket-parent object)
              (bucket-members object)))))

(defun bucket-concept (bucket)
  (or (bucket-concept-internal bucket)
      (let ((concept
	     (without-taxonomic-encoding
	      (without-optimized-encoding
	       (encode-concept-term `(or ,@(bucket-members bucket)))))))
            (assert (or-concept-p concept))
	(setf (bucket-concept-internal bucket) concept)
	(setf (concept-bucket concept) bucket)
	concept)))

(race-inline (bucket-concept-exists-p))

(defun bucket-concept-exists-p (bucket)
  (bucket-concept-internal bucket))

(defun create-new-buckets (tbox current-concept subsumer-mark)
  (loop with members = nil
        with mark = nil
        with children-not-in-bucket = (concept-children-not-in-bucket current-concept)
        with member-defined-p = nil
        for concept in (concept-children-not-in-bucket current-concept) do
        (push concept members)
        (unless member-defined-p
          (setf member-defined-p (not (concept-primitive-p concept))))
        (when (eql (concept-mark1 concept) subsumer-mark)
          #+:debug (assert (or (null mark) (eql mark subsumer-mark)))
          (setf mark subsumer-mark))
        (when (eql (length members) *bucket-size-threshold*)
          (let ((bucket (or (and *merge-buckets* (merge-two-buckets tbox current-concept))
                            (make-concepts-bucket :parent current-concept))))
            (setf (bucket-members-count bucket) *bucket-size-threshold*)
            (setf (bucket-members bucket) members)
            (setf (bucket-member-defined-p bucket) member-defined-p)
            (setf (bucket-members-language bucket) (get-language-from-concepts members))
            (when mark
              (setf (bucket-mark1 bucket) mark))
            (loop for member in members do
                  (pop children-not-in-bucket)
                  (push bucket (concept-buckets member)))
            (pushnew bucket (concept-bucket-children current-concept))
            (setf member-defined-p nil)
            (setf members nil)
            (setf mark nil)))
        finally (setf (concept-children-not-in-bucket current-concept)
                      children-not-in-bucket)))

(defun merge-two-buckets (tbox current-concept)
  (multiple-value-bind (bucket-1 bucket-2)
                       (get-min-bucket-pair current-concept)
    (when bucket-2
      (incf-statistics *merged-buckets*)
      (let ((bucket-2-members (bucket-members bucket-2)))
        (when (tbox-use-less-memory tbox)
          (when (bucket-concept-exists-p bucket-1)
            (decf (concept-reference-counter (bucket-concept bucket-1)))
            (delete-concept (bucket-concept bucket-1)))
          (when (bucket-concept-exists-p bucket-2)
            (decf (concept-reference-counter (bucket-concept bucket-2)))
            (delete-concept (bucket-concept bucket-2))))
        (setf (bucket-mark1 bucket-2) nil)
	(setf (bucket-concept-internal bucket-1) nil)
        (setf (bucket-members bucket-1) (append (bucket-members bucket-1) bucket-2-members))
        (incf (bucket-members-count bucket-1) (bucket-members-count bucket-2))
        (setf (bucket-member-defined-p bucket-1)
              (or (bucket-member-defined-p bucket-1) (bucket-member-defined-p bucket-2)))
        (setf (bucket-members-language bucket-1)
              (union-dl-descriptors (bucket-members-language bucket-1)
                                    (bucket-members-language bucket-2)))
        (loop for member in bucket-2-members do
              (setf (concept-buckets member) (delete bucket-2 (concept-buckets member)))
              (push bucket-1 (concept-buckets member)))
        (setf (bucket-concept-internal bucket-2) nil)
        bucket-2))))

(defun get-min-bucket-pair (current-concept)
  (let ((buckets (concept-bucket-children current-concept)))
    (when (>= (length buckets) *merge-buckets-threshold*)
      (loop with min-bucket = (first buckets)
            with min-bucket-key = (bucket-members-count min-bucket)
            with second-bucket = (second buckets)
            with second-bucket-key = (bucket-members-count second-bucket)
            for bucket in (cddr buckets)
            for bucket-key = (bucket-members-count bucket)
            do
            (if (< bucket-key min-bucket-key)
              (progn
                (setf min-bucket bucket)
                (setf min-bucket-key bucket-key))
              (when (< bucket-key second-bucket-key)
                (setf second-bucket bucket)
                (setf second-bucket-key bucket-key)))
            finally (return (values min-bucket second-bucket))))))
         

(defun delete-concept-from-bucket (tbox concept)
  (incf-statistics *deleted-buckets* (length (concept-buckets concept)))
  (loop for bucket in (concept-buckets concept)
        for parent = (bucket-parent bucket) do
        (setf (concept-bucket-children parent)
              (delete bucket (concept-bucket-children parent)))
        (loop for member in (bucket-members bucket) do
              (setf (concept-buckets member)
                    (delete bucket (concept-buckets member))))
        (setf (concept-children-not-in-bucket parent)
              (append (concept-children-not-in-bucket parent)
                      (bucket-members bucket)))
        (setf (bucket-member-defined-p bucket) 
              (notevery #'concept-primitive-p (bucket-members bucket)))
        (when (tbox-use-less-memory tbox)
          (when (bucket-concept-exists-p bucket)
            (decf (concept-reference-counter (bucket-concept bucket)))
            (delete-concept (bucket-concept bucket))))))

;;; ======================================================================

(defun insert-concept-node (tbox node parents children)
  (if *use-optimized-tbox-traversal*
    (insert-concept-node-2 tbox node parents children)
    (insert-concept-node-1 tbox node parents children)))


(defun insert-concept-node-1 (tbox node parents children)
  #+:debug
  (progn
    (when (or (null parents) (null children))
      (error "Insert concept node: either the parents or the children are nil. Not expected."))
    (unless (lists-disjoint-p parents children)
      (error "Insert concept node: parents and children are not disjoint. Not expected.")))
  (when *debug*
    (format t 
            "~%Insertion of ~S done (~S expensive subsumption tests).~%Superconcepts: ~S,~%Subconcepts: ~S.~%"
            node
            (- *subsumption-tests* *old-subsumption-tests*)
            parents
            children)
    (format t "~%Testing for cycles."))
  #+:debug
  (when (or (member node parents)
            (member node children)
            (concept-children-internal node)
            (concept-parents-internal node))
    (error "Confusion in insert-concept-node."))
  (when *debug*
    (format t "OK"))
  (loop for child in children do
        (when (and *tbox-clustering* (concept-buckets child))
          (delete-concept-from-bucket tbox child)))
  (loop for child in children do
        (loop for parent in parents
              when (member child (concept-children-internal parent))
              collect parent into removed
              finally
              (when removed
                (setf (concept-parents-internal child)
                      (concept-set-difference (concept-parents-internal child) removed))))
        (push node (concept-parents-internal child))
        (push child (concept-children-internal node))
        (when *tbox-clustering*
          (push child (concept-children-not-in-bucket node))))
  (loop for parent in parents do
        (setf (concept-children-internal parent)
              (concept-set-difference (concept-children-internal parent) children))
        (when *tbox-clustering*
          (setf (concept-children-not-in-bucket parent)
                (concept-set-difference (concept-children-not-in-bucket parent) children))
          (push node (concept-children-not-in-bucket parent)))
        (push node (concept-children-internal parent))
        (push parent (concept-parents-internal node)))
  node)

(defun insert-concept-node-2 (tbox node parents children)
  #+:debug
  (progn
    (when (or (null parents) (null children))
      (error "Insert concept node: either the parents or the children are nil. Not expected."))
    (unless (lists-disjoint-p parents children)
      (error "Insert concept node: parents and children are not disjoint. Not expected.")))
  (when *debug*
    (format t 
            "~%Insertion of ~S done (~S expensive subsumption tests).~%Superconcepts: ~S,~%Subconcepts: ~S.~%"
            node
            (- *subsumption-tests* *old-subsumption-tests*)
            parents
            children)
    (format t "~%Testing for cycles."))
  #+:debug
  (when (or (member node parents)
            (member node children)
            (concept-children-internal node)
            (concept-parents-internal node))
    (error "Confusion in insert-concept-node."))
  (when *debug*
    (format t "OK"))
  (when *tbox-clustering*
    (loop for child in children do
          (when (and (not (eq child *bottom-concept*)) (concept-buckets child))
            (delete-concept-from-bucket tbox child))))
  (loop for child in children
        with bottom = *bottom-concept*
        do
        (loop for parent in parents
              for parent-children = (concept-children-internal parent)
              when (and (not (and (eq child bottom) (rest parent-children)))
                        (member child parent-children))
              collect parent into removed
              finally
              (when removed
                (setf (concept-parents-internal child)
                      (concept-set-difference (concept-parents-internal child) removed))))
        (push node (concept-parents-internal child))
        (push child (concept-children-internal node))
        (when *tbox-clustering*
          (push child (concept-children-not-in-bucket node))))
  (loop for parent in parents do
        (setf (concept-children-internal parent)
              (concept-set-difference (concept-children-internal parent) children))
        (when *tbox-clustering*
          (setf (concept-children-not-in-bucket parent)
                (concept-set-difference (concept-children-not-in-bucket parent) children))
          (push node (concept-children-not-in-bucket parent)))
        (push node (concept-children-internal parent))
        (push parent (concept-parents-internal node)))
  node)

(defun insert-concept-node-without-bottom-search (node parents children tbox-el+-p)
  (if (or tbox-el+-p *use-optimized-tbox-traversal-no-bottom-search*)
    (insert-concept-node-without-bottom-search-2 node parents children)
    (insert-concept-node-without-bottom-search-1 node parents children)))


(defun insert-concept-node-without-bottom-search-1 (node parents children)
  #+:debug
  (progn
    (when (or (null parents) (null children))
      (error "Insert concept node: either the parents or the children are nil. Not expected."))
    (unless (lists-disjoint-p parents children)
      (error "Insert concept node: parents and children are not disjoint. Not expected.")))
  #+:debug (assert (and (null (rest children)) (eq (first children) *bottom-concept*)))
  (when *debug*
    (format t 
            "~%Insertion of ~S done (~S expensive subsumption tests).~%Superconcepts: ~S,~%Subconcepts: ~S.~%"
            node
            (- *subsumption-tests* *old-subsumption-tests*)
            parents
            children)
    (format t "~%Testing for cycles."))
  #+:debug
  (when (or (member node parents) (member node children))
    (error "Confusion in insert-concept-node."))
  (when *debug*
    (format t "OK"))
  (loop for parent in parents
        unless (rest (concept-children-internal parent))
        collect parent into removed
        and do
        (when (eq (first (concept-children-internal parent)) *bottom-concept*)
          (setf (concept-children-internal parent) nil)
          (when *tbox-clustering*
            (setf (concept-children-not-in-bucket parent) nil)))
        end
        do
        (when *tbox-clustering*
          (push node (concept-children-not-in-bucket parent)))
        (push node (concept-children-internal parent))
        (push parent (concept-parents-internal node))
        finally
        (when removed
          (setf (concept-parents-internal *bottom-concept*)
                (concept-set-difference (concept-parents-internal *bottom-concept*) removed))))
  (push node (concept-parents-internal *bottom-concept*))
  (push *bottom-concept* (concept-children-internal node))
  (when *tbox-clustering*
    (push *bottom-concept* (concept-children-not-in-bucket node)))
  node)

(defun insert-concept-node-without-bottom-search-2 (node parents children)
  #+:debug
  (progn
    (when (or (null parents) (null children))
      (error "Insert concept node: either the parents or the children are nil. Not expected."))
    (unless (lists-disjoint-p parents children)
      (error "Insert concept node: parents and children are not disjoint. Not expected.")))
  #+:debug
  (assert (and (null (rest children)) (eq (first children) *bottom-concept*)))
  (when *debug*
    (format t 
            "~%Insertion of ~S done (~S expensive subsumption tests).~%Superconcepts: ~S,~%Subconcepts: ~S.~%"
            node
            (- *subsumption-tests* *old-subsumption-tests*)
            parents
            children)
    (format t "~%Testing for cycles."))
  #+:debug
  (when (or (member node parents) (member node children))
    (error "Confusion in insert-concept-node."))
  (when *debug*
    (format t "OK"))
  (loop with clustering = *tbox-clustering*
        for parent in parents do
        (when clustering
          (push node (concept-children-not-in-bucket parent)))
        (push node (concept-children-internal parent)))
  (setf (concept-parents-internal node)
        (nconc parents (concept-parents-internal node)))
  node)

(defun insert-anonymous-concept-node (node parents children bidirectional-p)
  (cond (bidirectional-p
         (loop for parent in parents do
               (setf (concept-children-internal parent)
                     (concept-set-difference (concept-children-internal parent) children))
               (pushnew node (concept-children-internal parent))
               (pushnew parent (concept-parents-internal node)))
         (loop for child in children do
               (setf (concept-parents-internal child)
                     (concept-set-difference (concept-parents-internal child) parents))
               (pushnew node (concept-parents-internal child))
               (pushnew child (concept-children-internal node))))
        (t
         (loop for parent in parents do 
               (pushnew parent (concept-parents-internal node)))
         (loop for child in children do
               (pushnew child (concept-children-internal node)))))
  (setf (concept-visible-p node) nil)
  node)

;;; ----------------------------------------------------------------------

(defun get-tbox-role (tbox role-name &optional (errorp t))
  #+:debug (assert (symbolp role-name))

  ;; MW 6.1.20010: this already has to happen at parse time (TOLD)
  ;; (setf role-name (prepend-prefix role-name tbox))

  (let* ((role-store (tbox-role-store tbox))
         (role (and role-store (gethash role-name role-store))))
    (when (and (null role) errorp)
      (error "Undefined role name ~A in TBox ~S" role-name tbox))
    role))

(defun check-role-term (role-term)
  (check-type role-term (or symbol cons))
  (when (and (consp role-term)
             (not (and (eq (first role-term) 'inv)
                       (symbolp (second role-term))
                       (eql (length role-term) 2))))
    (error "value ~S must be of form <symbol> or (INV <symbol>)" role-term)))

(defun role-p (role-term &optional (tbox *current-tbox*))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (if (consp role-term)
    (let ((role (get-tbox-role tbox (second role-term) nil)))
      (when role
        (not (null (get-tbox-role tbox (role-name (role-inverse-internal role)) nil)))))
    (not (null (get-tbox-role tbox role-term nil)))))

(defmacro role? (role-term &optional (tbox-name nil tbox-name-specified-p))
  (if tbox-name-specified-p
    `(role-p ',role-term ',tbox-name)
    `(with-tbox-defined-check *current-tbox*
       (role-p ',role-term *current-tbox*))))

(race-inline (ensure-role-is-known))

(defun ensure-role-is-known (role-term tbox)
  (unless (role-p role-term tbox)
    (error "Role ~S is undefined." role-term)))

(defun ensure-no-cd-attribute (role-term &optional tbox)
  (when (and (symbolp role-term) (cd-attribute-p role-term tbox))
    (error "No attribute expected - found ~S." role-term)))

(defun add-role-axioms (tbox
			role-name 
			&key
			(cd-attribute nil)
			(feature-p nil)
			(feature nil)
			(transitive-p nil)
			(transitive nil)
			(parents nil)
			(parent nil)
			(inverse nil)
			(inverse-feature-p nil)
			(domain nil)
			(range nil)
			(symmetric nil)
			(symmetric-p nil)
			(reflexive nil)
			(reflexive-p nil)
			(datatype nil)
			(annotation-p nil)
			(irreflexive nil)
			(irreflexive-p nil)
			(asymmetric nil)
			(asymmetric-p nil)
                        (compositions nil))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (setf transitive-p (or transitive-p transitive))
  (setf feature-p (or feature-p feature))
  (setf symmetric (or symmetric-p symmetric))
  (setf asymmetric (or asymmetric-p asymmetric))
  (setf reflexive (or reflexive reflexive-p))
  (setf irreflexive (or irreflexive irreflexive-p))
  (setf parents (or parents (and parent (list parent))))
  (unless (listp parents)
    (setf parents (list parents)))
  (when (member role-name parents)
    (racer-warn "Redundant role axiom in Tbox ~A ignored: ~A has_parent ~A"
                (tbox-name tbox) role-name role-name)
    (setf parents (remove role-name parents)))
  (when symmetric
    (if (and inverse (not (eq role-name inverse)))
        ;; Instead of a warning it would be better to generate
        ;; an implicit equivalent declaration!!! -- rm 19.1.2008
        (racer-warn "symmetric declaration in TBox ~A for role ~A ignored due to specified inverse ~A"
                    tbox role-name inverse)
      (setf inverse role-name)))
  (when cd-attribute
    (if (eq cd-attribute 't)
      (setf cd-attribute 'integer))
    (unless (member cd-attribute '(cardinal integer real complex string boolean))
      (error "concrete domain attribute type ~A of ~A must be CARDINAL, INTEGER, REAL, COMPLEX, STRING or BOOLEAN"
             cd-attribute role-name)))
  (when (and cd-attribute
             (or transitive-p feature-p parents inverse inverse-feature-p
                 domain range symmetric reflexive irreflexive asymmetric))
    (error "~A is declared as a concrete domain attribute in TBox ~A. If :cd-attribute is given, ~
            no other option may be specified in add-role-axioms."
           role-name (tbox-name tbox)))
  (let ((index (tbox-role-axioms-index tbox)))
    (if (gethash role-name index)
      (racer-warn "Duplicate axiom for role ~A in TBox ~A found - ~
                   Ignoring duplicate axiom."
                  role-name (tbox-name tbox))
      (let ((entry (make-role-info role-name 
                                   cd-attribute
                                   feature-p
                                   transitive-p
                                   (if (listp parents)
                                     parents
                                     (list parents))
                                   inverse
                                   inverse-feature-p
                                   domain
                                   range
                                   reflexive
                                   datatype
                                   annotation-p
				   irreflexive
				   symmetric
				   asymmetric
                                   compositions)))
        (push entry (tbox-role-axioms tbox))
        (setf (gethash role-name index) entry))))
  tbox)

(defun ensure-role (role-name tbox &optional (cd-attribute nil) (datatype nil))
  (if (consp role-name)
      (ensure-role (second role-name) tbox cd-attribute)
    
    (progn 
               
      (when cd-attribute
        (unless (or (eq cd-attribute t)
                    (member cd-attribute '(cardinal integer real complex string boolean)))
          (error "Only cardinal, integer, real, complex, string, or boolean is supported as a concrete domain type - found ~S." cd-attribute)))
      (let* ((index (tbox-role-axioms-index tbox))
             (entry (gethash role-name index)))
        (cond ((null entry)
               (setf entry (make-role-info role-name 
                                           cd-attribute
                                           nil ;feature-p
                                           nil ;transitive-p
                                           nil ;parents
                                           nil ;inverse
                                           nil ;inverse-feature-p
                                           nil ;domain 
                                           nil ;range
                                           nil ;reflexive
                                           datatype
                                           nil ;annotation-p
					   nil ;irreflexive-p
					   nil ;symmetric-p
					   nil ;asymmetric-p
                                           nil ;compositions
                                           ))
               (push entry (tbox-role-axioms tbox))
               (setf (gethash role-name index) entry))
              (t (when cd-attribute
                   (unless (or (null (role-info-cd-attribute entry))
                               (eq (role-info-cd-attribute entry) t)
                               (eq cd-attribute t)
                               (eq cd-attribute (role-info-cd-attribute entry)))
                     (error "The range for attribute ~A has already been declared as ~A. ~
                             Now it is declared to be be ~A. ~
                             Racer cannot derive the intersection of the two domains."
                            role-name (role-info-cd-attribute entry) cd-attribute))
                   (unless (eq cd-attribute t)
                     (setf (role-info-cd-attribute entry) cd-attribute)))
                 (when datatype
                   (unless (or (null (role-info-datatype entry))
                               (eq (role-info-datatype entry) t)
                               (eq datatype t)
                               (equal datatype (role-info-datatype entry)))
                     (error "The range for datatype role ~A has already been declared as ~A. ~
                             Now it is declared to be be ~A. ~
                             Racer cannot derive the intersection of the two domains."
                            role-name (role-info-datatype entry) datatype))
                   (unless (eq datatype t)
                     (setf (role-info-datatype entry) datatype)))))))))

(defun attribute-has-range (rolename range tbox)
  (check-type rolename symbol)
  (check-type tbox (or tbox symbol))
  (unless (member range '(cardinal integer real complex string boolean))
    (error "concrete domain attribute type ~A of ~A must be CARDINAL, INTEGER, REAL, COMPLEX, STRING OR BOOLEAN"
           range rolename))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (ensure-role rolename tbox)
  (let* ((index (tbox-role-axioms-index tbox))
         (entry (gethash rolename index)))
    (unless entry
      (error "Declaration for role ~A in TBox ~A not found."
             rolename (tbox-name tbox)))
    (when (role-info-cd-attribute entry)
      (unless (eq (role-info-cd-attribute entry) 't)
        (if (member range '(cardinal integer real complex string boolean))
          (unless (eq (role-info-cd-attribute entry) range)
            (error "The range for attribute ~A has already been declared as ~A. ~
                    Racer cannot derive the intersection of the two domains."
                   rolename (role-info-cd-attribute entry)))
          (error "Role ~A was used as an attribute with range ~A. ~
                  Now the range is declared to be ~A, which is a domain mismatch."
                 rolename
                 (role-info-cd-attribute entry)
                 range))))
    (setf (role-info-cd-attribute entry) range)))

(defun attribute-has-domain (rolename domain tbox)
  (check-type rolename symbol)
  (check-type tbox (or tbox symbol))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (add-concept-axiom tbox `(a ,rolename) domain :inclusion-p t))

(defun role-is-transitive (rolename tbox)
  (check-type rolename symbol)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (ensure-role rolename tbox)
  (let* ((index (tbox-role-axioms-index tbox))
         (entry (gethash rolename index)))
    (if entry
      (if (role-info-feature-p entry)
        (racer-warn "Feature ~A in TBox ~A cannot be transitive. ~
                       Ignoring transitivity declaration for ~A"
                    rolename (tbox-name tbox) rolename)	
        (setf (role-info-transitive-p entry) t))
      (error "Declaration for role ~A in TBox ~A not found."
             rolename (tbox-name tbox)))))

(defmacro transitive (rolename &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(role-is-transitive ',rolename ',tbox)
    `(role-is-transitive ',rolename *current-tbox*)))

(defun role-is-reflexive (rolename tbox)
  (check-type rolename symbol)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (ensure-role rolename tbox)
  (let* ((index (tbox-role-axioms-index tbox))
         (entry (gethash rolename index)))
    (if entry        
      (setf (role-info-reflexive-p entry) t)
      (error "Declaration for role ~A in TBox ~A not found."
	     rolename (tbox-name tbox)))))

(defmacro reflexive (rolename &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(role-is-reflexive ',rolename ',tbox)
    `(role-is-reflexive ',rolename *current-tbox*)))


(defun role-is-irreflexive (rolename tbox)
  (check-type rolename symbol)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (ensure-role rolename tbox)
  (let* ((index (tbox-role-axioms-index tbox))
         (entry (gethash rolename index)))
    (if entry
        (setf (role-info-irreflexive-p entry) t)
      (error "Declaration for role ~A in TBox ~A not found."
             rolename (tbox-name tbox)))))

(defmacro irreflexive (rolename &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(role-is-irreflexive ',rolename ',tbox)
    `(role-is-irreflexive ',rolename *current-tbox*)))

(defun role-is-symmetric (rolename tbox)
  (check-type rolename symbol)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (ensure-role rolename tbox)
  (let* ((index (tbox-role-axioms-index tbox))
         (entry (gethash rolename index)))
    (if entry        
        (setf (role-info-symmetric-p entry) t)
      (error "Declaration for role ~A in TBox ~A not found."
	     rolename (tbox-name tbox))))
  (inverse-of-role rolename rolename tbox))

(defmacro symmetric (rolename &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(role-is-symmetric ',rolename ',tbox)
    `(role-is-symmetric ',rolename *current-tbox*)))

(defun role-is-asymmetric (rolename tbox)
  (check-type rolename symbol)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (ensure-role rolename tbox)
  (let* ((inv-used (consp rolename))
         (rolename (if inv-used
                       (second rolename)
                     rolename)))
    (ensure-role rolename tbox)
    (let* ((index (tbox-role-axioms-index tbox))
           (entry (gethash rolename index)))
      (if entry
          (progn
            (setf (role-info-asymmetric-p entry) t)
            (setf (role-info-irreflexive-p entry) t))
        (error "Declaration for role ~A in TBox ~A not found."
               rolename (tbox-name tbox))))))

(defmacro asymmetric (rolename &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(role-is-asymmetric ',rolename ',tbox)
    `(role-is-asymmetric ',rolename *current-tbox*)))

(defun role-is-functional (rolename tbox)
  (check-type rolename (or symbol cons))
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (let* ((inv-used (consp rolename))
         (rolename (if inv-used
                     (second rolename)
                     rolename)))
    (ensure-role rolename tbox)
    (let* ((index (tbox-role-axioms-index tbox))
           (entry (gethash rolename index)))
      (if entry
        (if (role-info-transitive-p entry)
          (racer-warn "Transitive role ~A in TBox ~A cannot be a feature. ~
                       Ignoring feature declaration for ~A"
                      rolename (tbox-name tbox) rolename)
          (if inv-used
	      (setf (role-info-inverse-feature-p entry) t)
            (setf (role-info-feature-p entry) t)))
        (error "Declaration for role ~A in TBox ~A not found."
               rolename (tbox-name tbox))))))

(defmacro functional (rolename &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(role-is-functional ',rolename ',tbox)
    `(role-is-functional ',rolename *current-tbox*)))


(defun role-has-domain (rolename concept tbox &optional (errorp t))
  (check-type rolename symbol)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (ensure-role rolename tbox)
  (let* ((index (tbox-role-axioms-index tbox))
         (entry (gethash rolename index)))
    (unless entry
      (format t "Cannot represent role domain ~A for role ~A appropriately."
              concept rolename))
    (and entry
         (let ((domain (role-info-domain entry)))
           (if domain
             (if errorp 
               (error "Domain declaration for role ~A already exists in TBox ~A."
                      rolename
                      (tbox-name tbox))
               nil)
             (setf (role-info-domain entry) concept))))))

(defmacro domain (rolename concept &optional (tbox nil tbox-supplied-p) (errorp t))
  (if tbox-supplied-p
    `(role-has-domain ',rolename ',concept ',tbox ',errorp)
    `(role-has-domain ',rolename ',concept *current-tbox* ',errorp)))


(defun role-has-range (rolename concept tbox &optional (errorp t))
  (if (member concept '(cardinal integer real complex string boolean))
    (attribute-has-range rolename concept tbox)
    (progn
      (check-type rolename symbol)
      (check-type tbox (or tbox symbol))
      (setf tbox (find-tbox tbox))
      (when (or (tbox-classified-p-internal tbox) 
                (tbox-coherence-checked-p tbox)
                (tbox-index-structures-complete-p tbox))
        (reset-tbox tbox))
      (ensure-role rolename tbox)
      (let* ((index (tbox-role-axioms-index tbox))
             (entry (gethash rolename index)))
        (unless entry
          (error "Cannot represent role range ~A for role ~A appropriately."
                 concept rolename))
        (and entry
             (let ((range (role-info-range entry)))
               (if (and range (not (equal range concept)))
                 (if errorp 
                   (error "Range declaration for role ~A already exists in TBox ~A."
                          rolename
                          (tbox-name tbox))
                   nil)
                 (setf (role-info-range entry) concept))))))))

(defmacro range (rolename concept &optional (tbox nil tbox-supplied-p) (errorp t))
  (if tbox-supplied-p
    `(role-has-range ',rolename ',concept ',tbox ',errorp)
    `(role-has-range ',rolename ',concept *current-tbox* ',errorp)))

(defun inverse-of-role (rolename inverse-role tbox)
  (check-type rolename symbol)
  (check-type inverse-role symbol)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (ensure-role rolename tbox)
  (let* ((index (tbox-role-axioms-index tbox))
         (entry (gethash rolename index)))
    (unless entry
      (error "Declaration for role ~A in TBox ~A not found."
             rolename (tbox-name tbox)))
    (cond ((role-info-inverse entry)
           (roles-equivalent-1 (role-info-inverse entry) inverse-role tbox))
          (t 
           (setf (role-info-inverse entry) inverse-role)
           (let ((inv-entry (gethash inverse-role index)))
             (unless inv-entry
               (error "Declaration for role ~A in TBox ~A not found."
                      inverse-role (tbox-name tbox)))
             (setf (role-info-inverse inv-entry) rolename))))))

(defmacro inverse (rolename inverse-role &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(inverse-of-role ',rolename ',inverse-role ',tbox )
    `(inverse-of-role ',rolename ',inverse-role *current-tbox*)))


(defun declare-disjoint (concepts tbox)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (if (null (rest concepts))
      (racer-warn "Disjointness declaration with a single element is useless and will be ignored: ~S."
                  `(disjoint . ,concepts))
    (if (loop for concept in concepts always (symbolp concept)) 
        (let ((group (gensym)))
          (loop for name in concepts do
                (add-disjointness-axiom tbox name group `(disjoint . ,concepts))))
      (loop for concept-set on concepts
            while (rest concept-set) do
            (add-concept-axiom tbox
                               (first concept-set)
                               `(not (or .,(rest concept-set)))
                               :inclusion-p t)))))


;; Used in the DIG1.1 implementation
(defun declare-disjoint-concepts (concepts tbox)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (if (null (rest concepts))
    (racer-warn "Disjointness declaration with a single element is useless and will be ignored: ~S."
                `(disjoint . ,concepts))
    (loop for concept-set on concepts
              while (rest concept-set) do
              (add-concept-axiom tbox
                                 (first concept-set) 
                                 `(not (or . ,(rest concept-set)))
                                 :inclusion-p t))))


(defun role-has-parent (rolename-1 rolename-2 tbox)
     (check-type rolename-1 symbol)
  (check-type rolename-2 symbol)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (if (equal rolename-1 rolename-2)
      (racer-warn "redundant role axiom in Tbox ~A ignored: ~A has_parent ~A"
                  (tbox-name tbox) rolename-1 rolename-2)
      (progn
	(when (or (tbox-classified-p-internal tbox) 
		  (tbox-coherence-checked-p tbox)
		  (tbox-index-structures-complete-p tbox))
	  (reset-tbox tbox))
	(ensure-role rolename-1 tbox)
	(ensure-role rolename-2 tbox)
	(let* ((index (tbox-role-axioms-index tbox))
	       (entry (gethash rolename-1 index)))
	  (unless entry
	    (error "Declaration for role ~A in TBox ~A not found."
		   rolename-1 (tbox-name tbox)))
	  (push rolename-2 (role-info-parents entry))))))

(defmacro implies-role (rolename-1 rolename-2 &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p 
      `(implies-role1 (quote ,rolename-1) (quote ,rolename-2) ,tbox))
  `(implies-role1 (quote ,rolename-1) (quote ,rolename-2)))

(defun implies-role1 (rolename-1 rolename-2 &optional (tbox nil tbox-supplied-p))
  (if (equal rolename-1 rolename-2)
      (racer-warn "redundant role axiom in Tbox ~A ignored: ~A has_parent ~A"
                  (tbox-name (or tbox *current-tbox*)) rolename-1 rolename-2)
    (if tbox-supplied-p
	(if (consp rolename-1)
	    (declare-role-axiom rolename-1 rolename-2 tbox)
          (role-has-parent rolename-1 rolename-2 tbox))
      (if (consp rolename-1)
          (declare-role-axiom rolename-1 rolename-2 *current-tbox*)
        (role-has-parent rolename-1 rolename-2 *current-tbox*)))))

(defun declare-role-axiom (role-composition role &optional (tbox (current-tbox)))
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (let ((absorbed-p nil))
    (when (and (consp role-composition) (= (length role-composition) 2))
      (let ((lhs (first role-composition))
            (rhs (second role-composition)))
        (if (and (eq lhs role) (eq lhs rhs))
            (progn
              (role-is-transitive role tbox)
              (setf absorbed-p t))
          (when (and (eq lhs 'inv) (eq rhs role))
            (progn
              (role-is-symmetric role tbox)
              (setf absorbed-p t))))))
    (unless absorbed-p
      (role-has-composition role tbox role-composition))))



(defun ensure-no-builtin-role (role role-composition)
  (when (or (eq role +top-object-role-symbol+)
            (eq role +krss-top-object-role-symbol+)
            (eq role +bottom-object-role-symbol+)
            (eq role +krss-bottom-object-role-symbol+)
            (eq role +top-datatype-role-symbol+)
            (eq role +bottom-datatype-role-symbol+))
     (error "Built-in role found in role composition ~A."
            (list 'implies-role role-composition role)))
  role)

(defun role-has-composition (role tbox role-composition)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (flet ((handle-role (role)
           (cond ((symbolp role)
                  (ensure-role role tbox)
                  role)
                 ((and (consp role) (eq (first role) 'inv) (symbolp (second role)) (null (cddr role)))
                  (ensure-role (second role) tbox)
                  (let ((inv-role (gensym (symbol-name (second role)))))
                    (push inv-role (tbox-internal-roles tbox))
                    (inverse-of-role inv-role (second role) tbox)
                    inv-role))
                 (t (error "Illegal role in role composition -- found ~A." role)))))
    (let (rolename
          composition)
      (setf rolename (ensure-no-builtin-role (handle-role role) role-composition))
      (loop for role in role-composition do
            (push (ensure-no-builtin-role (handle-role role) role-composition) composition))
      (setf composition (reverse composition))

      (when (or (tbox-classified-p-internal tbox) 
                (tbox-coherence-checked-p tbox)
                (tbox-index-structures-complete-p tbox))
        (reset-tbox tbox))
      (let* ((index (tbox-role-axioms-index tbox))
             (entry (gethash rolename index)))
        (if entry
            (if (role-info-feature-p entry)
                (racer-warn "Feature ~A in TBox ~A cannot have composition ~A ~
                       Ignoring composition declaration for ~A"
                            rolename (tbox-name tbox) composition rolename)	
              (push composition (role-info-compositions entry)))
          (error "Declaration for role ~A in TBox ~A not found."
                 rolename (tbox-name tbox)))))))

;;; ======================================================================

(defun find-tbox (tbox &optional (errorp t))
  (let ((result nil))
    (etypecase tbox
      (symbol (setf result (gethash tbox *tbox-table*)))
      (tbox (setf result tbox)))
    (when (and errorp (not result))
      (error "Can't find TBox with name ~S" tbox))
    result))

(defun (setf find-tbox) (new-tbox tbox-name)
  (unless (null new-tbox)
    (setf new-tbox (find-tbox new-tbox)))
  (check-type new-tbox (or null tbox))
  (check-type tbox-name symbol)
  (cond ((null new-tbox)
         (remhash tbox-name *tbox-table*) )
        (t (setf (gethash tbox-name *tbox-table*) new-tbox)
           (setf (tbox-name new-tbox) tbox-name)))
  new-tbox)

(defmacro define-tbox ((name &key disjoint-sets roles transitive-roles features verbose debug)
                       &rest axioms)
  `(define-tbox-1 ',name
     :disjoint-sets ',disjoint-sets
     :roles ',roles
     :transitive-roles ',transitive-roles
     :features ',features
     :verbose ',verbose
     :debug ',debug
     :axioms ',axioms))

(defun define-tbox-1 (name &key disjoint-sets roles transitive-roles features verbose debug axioms)
  (declare (ignore verbose debug))
  (let ((*use-less-tbox-memory* nil))
    (setf (find-tbox name)
          (make-tbox name
                     roles
                     transitive-roles
                     features
                     (mapcar #'parse-axiom axioms)
                     *default-tbox-concept-size*
                     *default-tbox-role-size*))
    (setf *current-tbox* (find-tbox name))
    (when disjoint-sets
      (loop for disjoint-set in disjoint-sets
            for group = (gensym) do
            (loop for name in disjoint-set
                  unless (symbolp name)
                  do (error "Element ~S of disjoint concept set ~S must be atomic"
                            name disjoint-set)
                  do (add-disjointness-axiom *current-tbox* name group)))))
  name)


(defun ensure-tbox-signature (tbox
                                  &key
                                  atomic-concepts
                                  roles
                                  transitive-roles
                                  features
                                  attributes)
  (let* ((tbox (find-tbox tbox))
         (attribute-names ())
         (normalized-attributes (loop for attribute in attributes 
                                      do (push (if (consp attribute)
                                                 (second attribute)
                                                 attribute)
                                               attribute-names)
                                      collect (if (consp attribute)
                                                (ecase (first attribute)
                                                  ((cardinal integer real complex string boolean)
                                                   attribute))
                                                (list 'complex attribute)))))
    (init-tbox tbox)
    
    (ensure-disjointness "atomic concepts" atomic-concepts "roles" roles)
    (ensure-disjointness "atomic concepts" atomic-concepts "features" features)
    (ensure-disjointness "atomic concepts" atomic-concepts "attributes" attributes)
    (ensure-disjointness "atomic concepts" atomic-concepts "transitive-roles" transitive-roles)
    (ensure-disjointness "roles" roles "features" features)
    (ensure-disjointness "roles" roles "attributes" attributes)
    (ensure-disjointness "roles" roles "transitive-roles" transitive-roles)
    (ensure-disjointness "transitive-roles" transitive-roles "attributes" attributes)
    (ensure-disjointness "transitive-roles" transitive-roles "features" features)
    (ensure-disjointness "features" features "attributes" attribute-names)
    (setf (tbox-signature tbox)
          (list atomic-concepts roles transitive-roles features normalized-attributes))
    (initialize-roles-and-concepts tbox roles transitive-roles features normalized-attributes nil atomic-concepts)))

(defun ensure-disjointness (name-1 list-1 name-2 list-2)
  (loop for elem in list-1 do
        (unless (symbolp elem)
          (setf elem (first elem)))
        (when (find elem list-2
                    :key #'(lambda (elem2)
                             (if (symbolp elem2)
                               elem2
                               (first elem2))))
          (error "Name conflict: ~A and ~A must be disjoint: common element is ~A"
                 name-1 name-2 elem))))


(defun get-tbox-signature (&optional (tbox *current-tbox*))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (if (tbox-signature tbox)
    (destructuring-bind (atomic-concepts
                         roles
                         transitive-roles
                         features
                         attributes)
                        (tbox-signature tbox)
      `(signature :atomic-concepts ,atomic-concepts
                  :roles ,roles
                  :transitive-roles ,transitive-roles
                  :features ,features
                  :attributes ,attributes))
    nil))

;;; ----------------------------------------------------------------------

(defun current-tbox ()
  (if *current-tbox*
    (tbox-name *current-tbox*)
    (error "No current TBox set")))

(defun set-current-tbox (tbox)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (setf *current-tbox* tbox)
  (current-tbox))



(defmacro in-tbox (name &key (init t) (size '*default-tbox-concept-size*)
                        (role-size '*default-tbox-role-size*)
                        (signature nil))
  `(in-tbox-internal ',name ,init ',signature ,size ,role-size))

(defun in-tbox-internal (name init signature expected-concept-size expected-role-size)
  (setf *current-tbox* (or (find-tbox name (if init nil t))
                           (make-tbox name nil nil nil nil expected-concept-size expected-role-size)))
  (setf (tbox-expected-concept-size *current-tbox*) expected-concept-size)
  (setf (tbox-expected-role-size *current-tbox*) expected-role-size)
  (setf (find-tbox name) *current-tbox*)
  (cond (signature
         (apply #'ensure-tbox-signature *current-tbox*
                signature))
        (init 
         (setf (tbox-signature *current-tbox*) nil)
         (init-tbox *current-tbox*)))
  (current-tbox))


(defun init-tbox (tbox &key (original nil) (reset nil))
  (check-type tbox (or tbox symbol))
  (let ((tbox1 tbox)
        (tbox (find-tbox tbox nil)))
    (cond ((null tbox)
           (setf tbox (make-tbox tbox1 nil nil nil nil *default-tbox-concept-size* *default-tbox-role-size*))
           (setf (find-tbox tbox1) tbox))
          (t (setf (find-tbox (tbox-name tbox)) tbox)
	     (unless reset
	       (setf (tbox-original-concept-axioms tbox) nil
		     (tbox-concept-axioms tbox) nil
		     (tbox-concept-axioms-index tbox)
		     (make-concept-axioms-index tbox
						(when original
						  (hash-table-count 
						   (tbox-concept-axioms-index original))))
		     (tbox-role-axioms tbox) nil
		     (tbox-role-axioms-index tbox)
		     (make-role-axioms-index tbox
					     (when original
                                               (hash-table-count 
                                                (tbox-role-axioms-index original))))
		     (tbox-generalized-concept-inclusions tbox) nil
		     (tbox-removed-generalized-concept-inclusions tbox) nil)
	       (clrhash (tbox-disjoint-set tbox))
               )
	     (setf (tbox-encoded-concept-list tbox) nil
		   (tbox-added-generalized-concept-inclusions tbox) nil
                   (tbox-auto-installed-primitives tbox) nil
                   (tbox-literal-meta-constraints tbox) nil
                   (tbox-meta-constraint-concepts tbox) nil
                   (tbox-meta-constraint-concept tbox) nil
                   (tbox-blocking-possibly-required tbox) nil
                   (tbox-empty-taxonomic-encoding-table tbox) t
                   (tbox-coherent-p-internal tbox) ':dont-know
                   (tbox-all-features tbox) nil
                   (tbox-all-roles tbox) nil
                   (tbox-all-transitive-roles tbox) nil
                   (tbox-all-attributes tbox) nil
                   (tbox-structure-id-counter tbox) +racer-structure-id-counter-init+
                   (tbox-coherence-checked-p tbox) nil
                   (tbox-index-structures-complete-p tbox) nil
                   (tbox-identification tbox) nil
                   (tbox-classified-p-internal tbox) nil
                   (tbox-atomic-concepts tbox) nil
                   (tbox-signature tbox) nil
                   (tbox-language tbox) *dl-empty*
                   (tbox-encoded-role-list tbox) nil
                   (tbox-ontologies tbox) nil
                   (tbox-end-of-no-bottom-search tbox) nil
                   (tbox-role-synonyms tbox) nil
                   (tbox-role-antonyms tbox) nil
                   (tbox-namespaces tbox) nil
                   (tbox-taxonomy-snapshot tbox) nil
                   (tbox-stable-set-difference-last-list2 tbox) nil
                   (tbox-concept-set-mark tbox) 0
                   (tbox-role-set-mark tbox) 0
                   (tbox-individual-set-mark tbox) 0
                   (tbox-constraint-set-mark tbox) 0
                   (tbox-classification-counter tbox) 0
                   (tbox-use-less-memory tbox) nil
                   (tbox-initial-hash-table-size tbox) (hash-table-size (make-hash-table))
                   (tbox-internal-roles tbox) nil
                   (tbox-role-hierarchy-classified-p tbox) nil
                   (tbox-role-range-concepts tbox) nil
                   (tbox-el+-transformed-table tbox) nil
                   (tbox-el+-environment tbox) nil
                   (tbox-role-characteristics-checked-p tbox) nil
                   )
             (unless (and (hash-table-p (tbox-internal-marker-concepts tbox))
                          (eql (hash-table-count (tbox-internal-marker-concepts tbox)) 0))
               (setf (tbox-internal-marker-concepts tbox) (racer-make-hash-table)))
             (if (tbox-taxonomic-encoding-table tbox)
               (clrhash (tbox-taxonomic-encoding-table tbox))
               (setf (tbox-taxonomic-encoding-table tbox) (make-taxonomic-encoding-table)))
             (if (tbox-stable-set-difference-table tbox)
               (clrhash (tbox-stable-set-difference-table tbox))
               (setf (tbox-stable-set-difference-table tbox) (racer-make-hash-table)))
             (if (tbox-racer-remove-duplicates-table tbox)
               (clrhash (tbox-racer-remove-duplicates-table tbox))
               (setf (tbox-racer-remove-duplicates-table tbox)
                     (racer-make-hash-table :test 'equal :structure-p t)))
             (if (tbox-racer-remove-constraint-duplicates-table tbox)
               (clrhash (tbox-racer-remove-constraint-duplicates-table tbox))
               (setf (tbox-racer-remove-constraint-duplicates-table tbox)
                     (racer-make-hash-table :test 'equal)))
             (unless (and (arrayp (tbox-possible-subsumees-vector tbox))
                          (eql (fill-pointer (tbox-possible-subsumees-vector tbox)) 0))
               (setf (tbox-possible-subsumees-vector tbox) (make-possible-subsumees-vector)))
             (if (tbox-expanded-constraints-ind-table tbox)
               (clrhash (tbox-expanded-constraints-ind-table tbox))
               (setf (tbox-expanded-constraints-ind-table tbox) (racer-make-hash-table)))
             (if (tbox-live-inds-table tbox)
               (clrhash (tbox-live-inds-table tbox))
               (setf (tbox-live-inds-table tbox) (racer-make-hash-table)))
             (if (tbox-obsolete-inds-table tbox)
               (clrhash (tbox-obsolete-inds-table tbox))
               (setf (tbox-obsolete-inds-table tbox) (racer-make-hash-table)))
             (if (tbox-label-inds-table tbox)
               (clrhash (tbox-label-inds-table tbox))
               (setf (tbox-label-inds-table tbox) (racer-make-hash-table)))
             (if (tbox-new-inds-table tbox)
               (clrhash (tbox-new-inds-table tbox))
               (setf (tbox-new-inds-table tbox) (racer-make-hash-table)))
             (if (tbox-obsolete-eql-tables tbox)
               (clrhash (tbox-obsolete-eql-tables tbox))
               (setf (tbox-obsolete-eql-tables tbox) (make-hash-table)))
             (if (tbox-obsolete-equal-tables tbox)
               (clrhash (tbox-obsolete-equal-tables tbox))
               (setf (tbox-obsolete-equal-tables tbox) (make-hash-table)))
             (if (tbox-obsolete-equal-concept-tables tbox)
               (clrhash (tbox-obsolete-equal-concept-tables tbox))
               (setf (tbox-obsolete-equal-concept-tables tbox) (make-hash-table)))
             (if (tbox-signatures-equal-table tbox)
               (clrhash (tbox-signatures-equal-table tbox))
               (setf (tbox-signatures-equal-table tbox) (racer-make-hash-table :test 'equal)))
             (if (tbox-partitions-table tbox)
               (clrhash (tbox-partitions-table tbox))
               (setf (tbox-partitions-table tbox) (racer-make-hash-table)))
             (unless (and (arrayp (tbox-set-vector tbox))
                          (eql (fill-pointer (tbox-set-vector tbox)) 0))
               (setf (tbox-set-vector tbox) (make-set-vector)))
             (when (tbox-nary-absorption-table tbox)
               (setf (tbox-nary-absorption-table tbox) nil))
             (when (tbox-inverse-nary-absorption-table tbox)
               (setf (tbox-inverse-nary-absorption-table tbox) nil))
             (when (tbox-domain-qualifications-table tbox)
               (setf (tbox-domain-qualifications-table tbox) nil))
             (if (tbox-select-disjunct-table tbox)
                 (setf (tbox-select-disjunct-table tbox)
                       (clear-select-disjunct-table (tbox-select-disjunct-table tbox)))
               (setf (tbox-select-disjunct-table tbox) (make-select-disjunct)))
             
             (setf (tbox-top-node tbox) (make-top-concept))
             (setf (tbox-bottom-node tbox) (make-bottom-concept))
             (setf (tbox-datatype-top-node tbox) (make-top-datatype-concept))
             (setf (tbox-datatype-bottom-node tbox) (make-bottom-datatype-concept))
             (setf (tbox-object-top-role tbox) (make-top-object-role))
             (setf (tbox-object-bottom-role tbox) (make-bottom-object-role))
             (setf (tbox-datatype-top-role tbox) (make-top-datatype-role))
             (setf (tbox-datatype-bottom-role tbox) (make-bottom-datatype-role))
             (setf (tbox-role-store tbox) (make-new-role-store))
             (setf (tbox-concept-store tbox) nil)
             (setf (tbox-tableaux-cache tbox) (make-new-tableaux-cache))
             (setf (tbox-tableaux-sat-cache tbox) (make-new-sat-tableaux-cache))
             (setf (tbox-tableaux-unsat-cache tbox) (make-new-unsat-tableaux-cache))
             (setf (tbox-concrete-domain tbox) (create-concrete-domain))
             (unless (and (hash-table-p (tbox-nominal-table tbox))
                          (eql (hash-table-count (tbox-nominal-table tbox)) 0))
               (setf (tbox-nominal-table tbox) (racer-make-hash-table)))))
    (setf *current-tbox* tbox)
    (setf (tbox-version tbox) (new-tbox-version))
    (tbox-name *current-tbox*)))

(defun reset-tbox (tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (let ((tbox-original-concept-axioms (tbox-original-concept-axioms tbox))
        (tbox-concept-axioms (tbox-concept-axioms tbox))
	;;(tbox-concept-axioms-index (copy-hash-table (tbox-concept-axioms-index tbox)))
        (tbox-concept-axioms-index (tbox-concept-axioms-index tbox))
        (tbox-generalized-concept-inclusions (tbox-generalized-concept-inclusions tbox))
        (tbox-removed-generalized-concept-inclusions 
         (tbox-removed-generalized-concept-inclusions tbox))
        (tbox-role-axioms (tbox-role-axioms tbox))
        ;;(tbox-role-axioms-index (copy-hash-table (tbox-role-axioms-index tbox)))
        (tbox-role-axioms-index (tbox-role-axioms-index tbox))
        ;;(tbox-disjoint-set (copy-hash-table (tbox-disjoint-set tbox)))
        (tbox-disjoint-set (tbox-disjoint-set tbox)))
    (loop for abox in (associated-aboxes tbox) do
          (initialize-abox (find-abox abox)))
    (init-tbox tbox :reset t)
    (if (tbox-use-less-memory tbox)
      (racer-warn "~&Resetting or updating of Tbox ~A only partially supported due to the ~
                   current memory management strategy. Start Racer with option -x to ~
                   enable these services.~%"
                  (tbox-name tbox))
      (progn
        (setf (tbox-original-concept-axioms tbox) tbox-original-concept-axioms)
        (setf (tbox-concept-axioms tbox) tbox-concept-axioms)
        (setf (tbox-concept-axioms-index tbox) tbox-concept-axioms-index)
        (setf (tbox-role-axioms tbox) tbox-role-axioms)
        (setf (tbox-role-axioms-index tbox) tbox-role-axioms-index)))

    ;; Absorb-domain-range-restrictions modifies
    ;; tbox-generalized-concept-inclusions. Thus reset-tbox does not work
    ;; if a domain restriction is absorbed. We restore the initial
    ;; set of generalized-concept-inclusions 
    (setf (tbox-generalized-concept-inclusions tbox)
          (append (decode-generalized-concept-inclusions tbox-removed-generalized-concept-inclusions)
                  (decode-generalized-concept-inclusions tbox-generalized-concept-inclusions)))
    (setf (tbox-removed-generalized-concept-inclusions tbox) nil)
    (setf (tbox-disjoint-set tbox) tbox-disjoint-set)
    tbox))

(defun decode-generalized-concept-inclusions (list)
  (loop for elem in list
        collect
        (if (listp elem)
          (loop for concept in elem
                collect (decode-concept concept))
          (list +top-symbol+ (decode-concept elem)))))

;;; ======================================================================

(defun get-tbox-version (tbox)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (tbox-version tbox))

;;; ======================================================================


(defmacro defprimconcept (name &optional (definition nil))
  (check-type name symbol)
  `(with-tbox-defined-check *current-tbox*   
     (add-concept-axiom *current-tbox* ',name ',definition :inclusion-p t)
     ',name))

(defmacro defconcept (name definition)
  (check-type name symbol)
  `(with-tbox-defined-check *current-tbox*
     (add-concept-axiom *current-tbox* ',name ',definition :inclusion-p nil)
     ',name))

(defmacro implies (left right)
  `(with-tbox-defined-check *current-tbox*
     (add-concept-axiom *current-tbox* ',left ',right :inclusion-p t)
     ',left))

(defmacro equivalent (left right)
  (if (symbolp left)
    (if (and (symbolp right)
             (member left (list +top-symbol+ +krss-top-symbol+ +bottom-symbol+ +krss-bottom-symbol+)))
      `(with-tbox-defined-check *current-tbox*
         (add-concept-axiom *current-tbox* ',right ',left :inclusion-p nil)
         ',right)
      `(with-tbox-defined-check *current-tbox*
         (add-concept-axiom *current-tbox* ',left ',right :inclusion-p nil)
         ',left))
    (if (symbolp right)
      `(with-tbox-defined-check *current-tbox*
         (add-concept-axiom *current-tbox* ',right ',left :inclusion-p nil)
         ',right)
      `(with-tbox-defined-check *current-tbox*
         (add-concept-axiom *current-tbox* ',left ',right :inclusion-p t)
         (add-concept-axiom *current-tbox* ',right ',left :inclusion-p t)
         ',left))))

(defmacro defprimrole (name &key
                            (transitive nil)
                            (parents nil)
                            (inverse nil)
                            (inverse-feature-p nil)
                            (domain nil)
                            (range nil)
                            (symmetric nil)
                            (reflexive nil)
			    (irreflexive nil)
			    (asymmetric nil)
                            (compositions nil))
  (when (and (not (null parents))
             (not (consp parents)))
    (setf parents (list parents)))
  (unless (symbolp inverse)
    (error "Invalid inverse role specification."))
  `(with-tbox-defined-check *current-tbox*
     (if *absorb-domains-ranges*
       (add-role-axioms *current-tbox* ',name 
                        :transitive-p ',transitive
                        :parents ',parents 
                        :inverse ',inverse
                        :inverse-feature-p ',inverse-feature-p
                        :domain ',domain
                        :range ',range
                        :symmetric ',symmetric
                        :reflexive ',reflexive
			:irreflexive ',irreflexive
			:asymmetric ',asymmetric
                        :compositions ',compositions)
       (progn
         ,(when domain
            `(add-concept-axiom *current-tbox* '(at-least 1 ,name) ',domain :inclusion-p t))
         ,(when range
            `(add-concept-axiom *current-tbox* '*top* '(all ,name ,range) :inclusion-p t))
         (add-role-axioms *current-tbox* ',name 
                          :transitive-p ',transitive
                          :parents ',parents 
                          :inverse ',inverse
                          :inverse-feature-p ',inverse-feature-p
                          :symmetric ',symmetric
                          :reflexive ',reflexive
			  :irreflexive ',irreflexive
			  :asymmetric ',asymmetric
                          :compositions ',compositions)))
     ',name))

(defmacro defprimattribute (name &key
                                 (parents nil)
                                 (inverse nil)
                                 (inverse-feature-p nil)
                                 (domain nil)
                                 (range nil)
                                 (symmetric nil)
				 (asymmetric nil)
				 (reflexive nil)
				 (irreflexive nil))
  (when (and (not (null parents))
             (not (consp parents)))
    (setf parents (list parents)))
  (unless (symbolp inverse)
    (error "Invalid inverse role specification."))
  `(with-tbox-defined-check *current-tbox*
     (if *absorb-domains-ranges*
       (add-role-axioms *current-tbox* ',name 
                        :feature-p t
                        :parents ',parents 
                        :inverse ',inverse
                        :inverse-feature-p ',inverse-feature-p
                        :domain ',domain
                        :range ',range
                        :symmetric ',symmetric
			:asymmetric ',asymmetric
			:reflexive ',reflexive
			:irreflexive ',irreflexive)
       (progn
         ,(when domain
            `(add-concept-axiom *current-tbox* '(at-least 1 ,name) ',domain :inclusion-p t))
         ,(when range
            `(add-concept-axiom *current-tbox* '*top* '(all ,name ,range) :inclusion-p t))
         (add-role-axioms *current-tbox* ',name 
                          :feature-p t
                          :parents ',parents 
                          :inverse ',inverse
                          :inverse-feature-p ',inverse-feature-p
                          :symmetric ',symmetric
			  :asymmetric ',asymmetric
			  :reflexive ',reflexive
			  :irreflexive ',irreflexive)))
     ',name))

(defmacro defcdattribute (name &key domain type)
  `(ensure-cd-attribute ',name ',domain ',type *current-tbox*))

(defun ensure-cd-attribute (name domain type tbox)
  (check-type name symbol)
  (check-type type symbol)
  (unless (member type '(t cardinal integer real complex string boolean))
    (if (null type)
      (error "No type specified for concrete domain attribute ~S." name)
      (error "concrete domain attribute type ~A of ~A must be CARDINAL, INTEGER, REAL, COMPLEX, STRING or BOOLEAN."
             type name)))
  (add-role-axioms tbox name :cd-attribute type)
  (when domain
    (add-concept-axiom tbox `(a ,name) domain :inclusion-p t))
  name)

(defmacro disjoint (&whole form &rest concept-names)
  `(with-tbox-defined-check *current-tbox*
     (if ,(null (rest concept-names))
       (racer-warn "Disjointness declaration with a single element is useless and will be ignored: ~S."
                   ',form)
       (let ((group (gensym)))
         (loop for name in ',concept-names do
               (add-disjointness-axiom *current-tbox* name group ',form))))))

(defun add-disjointness-axiom (tbox concept-name group-name
                                        &optional (form nil))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (assert (symbolp group-name)
          (group-name)
          "Group names must be symbols. Found ~S in ~S." 
          group-name
          (or form "call to add-disjointness-axiom"))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (if (symbolp concept-name)
    (push concept-name (gethash group-name (tbox-disjoint-set tbox)))
    (error "Disjointness axioms can only be stated for concept names. ~
            Found ~S in ~S."
           concept-name (or form "call to add-disjointness-axiom")))
  tbox)



;;; KRSS style macros for Tbox definitions:

;;; Macro for primitive concept definitions:
(defmacro define-primitive-concept (name &optional (definition nil))
  (check-type name symbol)
  `(with-tbox-defined-check *current-tbox*   
     (add-concept-axiom *current-tbox* ',name ',definition :inclusion-p t)
     ',name))

;;; Macro for non-primitive concept definitions:
(defmacro define-concept (name definition)
  (check-type name symbol)
  `(with-tbox-defined-check *current-tbox*
     (add-concept-axiom *current-tbox* ',name ',definition :inclusion-p nil)
     ',name))

(defmacro define-disjoint-primitive-concept (&whole form
                                                  name disjoint-list definition)
  (check-type name symbol)
  (let ((disjoint-list-sym (gensym)))
    `(with-tbox-defined-check *current-tbox*
       (loop for ,disjoint-list-sym in ',disjoint-list do
                  (add-disjointness-axiom *current-tbox*
                                          ',name ,disjoint-list-sym ',form))
            (add-concept-axiom *current-tbox* 
                               ',name ',definition :inclusion-p t)
            ',name)))

;;; Macro for primitive role definitions:
(defmacro define-primitive-role (name 
				 &key (parents nil) 
				      (parent nil)
				      (transitive nil)
				      (feature nil)
				      (domain nil)
				      (range nil)
				      (inverse nil)
				      (symmetric nil)
				      (reflexive nil)
				      (irreflexive nil)
				      (asymmetric nil)
                                      (compositions nil))
  (setf parents
    (or parents (and parent (cond ((symbolp parent) (list parent))
				  ((and (symbolp (first parent))
					(null (rest parent)))
				   parent)
				  (t (error "symbol expected instead of ~S" parent))))))
  (if feature
      (progn
	(when transitive
	  (racer-warn "Feature ~A cannot be transitive. Declaration ignored." name))
        (when compositions
	  (racer-warn "Feature ~A cannot have role composition axioms. Declaration ignored." name))
	`(defprimattribute ,name
	     :parents ,parents 
	     :domain ,domain 
	     :range ,range
	     :inverse ,inverse
	     :reflexive ,reflexive
	     :irreflexive ,irreflexive
	     :symmetric ,symmetric
	     :asymmetric ,asymmetric))
    `(defprimrole ,name
	 :parents ,parents 
	 :transitive ,transitive
	 :inverse ,inverse
	 :domain ,domain 
	 :range ,range
	 :symmetric ,symmetric
	 :asymmetric ,asymmetric
	 :reflexive ,reflexive
	 :irreflexive ,irreflexive
         :compositions ,compositions)))

;;; Macro for primitive attribute definitions:
(defmacro define-primitive-attribute (name &key 
                                          (parent nil) 
                                          (parents nil)
                                          (domain nil)
                                          (range nil)
				          (inverse nil)
                                          (symmetric nil)
					  (asymmetric nil)
					  (reflexive nil)
					  (irreflexive nil))
  (setf parents
        (or parents (and parent (cond ((symbolp parent) (list parent))
                                      ((and (symbolp (first parent))
                                            (null (rest parent)))
                                       parent)
                                      (t (error "symbol expected instead of ~S" parent))))))
  `(defprimattribute ,name
     :parents ,parents
     :inverse ,inverse
     :domain ,domain
     :range ,range
     :symmetric ,symmetric
     :asymmetric ,asymmetric
     :reflexive ,reflexive
     :irreflexive ,irreflexive))

(defmacro define-concrete-domain-attribute (name &key domain type)
  `(defcdattribute ,name :type ,type :domain ,domain))


(defmacro define-datatype-property (&rest args)
  `(apply #'add-datatype-property (current-tbox) ',args))

(defun add-datatype-property (tbox name &rest args)
  (let ((range (getf args :range)))
    (remf args :range)
    (apply #'add-role-axioms tbox name args)
    (role-is-used-as-datatype-property name tbox)
    (when range
      (datatype-role-has-range name range tbox))
    name))

(defun get-role-datatype (role-name &optional (tbox (current-tbox)))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (check-type role-name symbol)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (role-datatype (get-tbox-role tbox role-name)))

;;; ======================================================================

(defun make-internal-role-name (role-1 role-2)
  "Returns two values: new role name, list of parents. 
If a role is already an internal conjunction, then create a flattened name. 
Always create a canonical name regardless of the order of the role parents."
  (loop for role = role-1 then role-2
        if (role-internal-conjunction-p role)
        append (role-parent-names role) into name-list
        else collect (role-name role) into name-list
        end
        until (eq role role-2)
        finally
        (setf name-list (sort (remove-duplicates name-list) #'string-lessp))
        (loop with role-name = "%"
              for name in name-list
              for hyphen = "" then "-"
              do (setf role-name (concatenate 'string role-name hyphen (symbol-name name)))
              finally (return-from make-internal-role-name
                        (values (intern role-name) name-list)))))

(let (#+:allegro
      (excl:*redefinition-warnings* nil)
      #+:lispworks
      (lw:*handle-warn-on-redefinition* nil)
      #+:lispworks
      (lw:*redefinition-action* nil)
      #+:ccl
      (ccl:*warn-if-redefine* nil)
      )
  
  (setf (symbol-function 'create-internal-and-role)
        (lambda (tbox role1 role2)
          "Creates an internal role as a conjunction of role1 and role2"
          ;;; The new role is not properly inserted into the role hierarchy.
          ;;; Especially the ancestors and descendants of the other roles are not updated.
          ;;; Nevertheless, it should work!
          (multiple-value-bind (role-name role-parents)
                               (make-internal-role-name role1 role2)
            (or (get-tbox-role tbox role-name nil)
                (and (disjoint-roles-in-role-conjunction-clash-p tbox (list role1 role2))
                     (tbox-object-bottom-role tbox))
                (multiple-value-bind (inverse-role-name inverse-role-parents)
                                     (make-internal-role-name (role-inverse-internal role1) (role-inverse-internal role2))
                  (let* ((domain (delete nil (list (role-domain-concept role1)
                                                   (role-domain-concept role2))))
                         (range (delete nil (list (role-range-concept role1)
                                                  (role-range-concept role2))))
                         (reflexive-p (or (role-reflexive-p role1)
                                          (role-reflexive-p role2)))
                         (role (encode-tbox-role-term role-name
                                                      nil nil nil inverse-role-name
                                                      nil
                                                      (when domain
                                                        `(and . ,domain))
                                                      (when range
                                                        `(and . ,range))
                                                      reflexive-p
                                                      nil
                                                      nil
                                                      nil
                                                      nil
                                                      nil
                                                      nil))
                         (inverse-role (if (eq role-name inverse-role-name)
                                         role
                                         (encode-tbox-role-term inverse-role-name 
                                                                nil nil nil role
                                                                nil
                                                                (when range
                                                                  `(and . ,range))
                                                                (when domain
                                                                  `(and . ,domain))
                                                                reflexive-p 
                                                                nil
                                                                nil
                                                                nil
                                                                nil
                                                                nil
                                                                nil))))
                    (setf (role-inverse-internal role) inverse-role)
                    
                    (setf (role-parents-internal role)
                          (mapcar #'(lambda (pname) (get-tbox-role tbox pname)) role-parents))
                    (loop for parent in (role-parents-internal role) do
                      (pushnew role (role-children-internal parent)))
                    
                    (setf (role-parents-internal inverse-role)
                          (mapcar #'(lambda (pname) (get-tbox-role tbox pname)) inverse-role-parents))
                    (loop for parent in (role-parents-internal inverse-role) do
                      (pushnew inverse-role (role-children-internal parent)))
                    
                    (setf (role-internal-conjunction-p role) t)
                    (setf (role-internal-conjunction-p inverse-role) t)
                    (encode-role tbox role t)
                    (encode-role tbox inverse-role t)
                    (compute-role-domain-range-1 role tbox (tbox-top-node tbox))
                    (compute-role-domain-range-2 role)
                    (compute-role-domain-range-1 inverse-role tbox (tbox-top-node tbox))
                    (compute-role-domain-range-2 inverse-role)
                    
                    (setf (role-descendants-internal role)
                          (union (remove role1 (role-descendants-internal role1))
                                 (remove role2 (role-descendants-internal role2))))
                    (setf (role-descendants-internal inverse-role)
                          (union (remove (role-inverse-internal role1)
                                         (role-descendants-internal (role-inverse-internal role1)))
                                 (remove (role-inverse-internal role2)
                                         (role-descendants-internal (role-inverse-internal role2)))))
                    
                    role))))))
  )

(race-inline (subrole-p safe-get-tbox-role))

(defun subrole-p (role1 role2)
  (or (eq role1 role2)
      (if (role-internal-conjunction-p role2)
        (role-set-subsetp (rest (role-ancestors-internal role2))
                          (role-ancestors-internal role1))
        (and (member role2 (role-ancestors-internal role1))
             t))))

(defun roles-intersect-p (role1 role2)
  (or (eq role1 role2)
      (not (lists-disjoint-p (role-ancestors-internal role1)
                             (role-ancestors-internal role2)))))


(defun safe-get-tbox-role (tbox role-or-name &optional (errorp t))
  (if (symbolp role-or-name)
    (get-tbox-role tbox role-or-name errorp)
   role-or-name))

(defparameter *debug-r* nil)
            
(defun duplicates-p (list)
  (if (null list)
    nil
    (or (duplicates-1-p (first list) (rest list))
        (duplicates-p (rest list)))))

(defun duplicates-1-p (x list)
  (if (null list)
    nil
    (if (eq x (first list))
      t
      (duplicates-1-p x (rest list)))))

(defun preencode-roles (tbox)
  (ensure-role +has-integer-value+ tbox 'integer 'integer)
  (ensure-role +has-real-value+ tbox 'real 'real)
  (ensure-role +has-string-value+ tbox 'string 'string)
  (ensure-role +has-boolean-value+ tbox 'boolean 'boolean)
  (loop with role-store = (tbox-role-store tbox)
        with top = (tbox-object-top-role tbox)
        for role-name in (list +top-object-role-symbol+ +krss-top-object-role-symbol+) do
        (setf (gethash role-name role-store) top)
        (setf (gethash `(inv ,role-name) role-store) top))
  (loop with role-store = (tbox-role-store tbox)
        with bottom = (tbox-object-bottom-role tbox)
        for role-name in (list +bottom-object-role-symbol+ +krss-bottom-object-role-symbol+) do
        (setf (gethash role-name role-store) bottom)
        (setf (gethash `(inv ,role-name) role-store) bottom))
  (let ((role-store (tbox-role-store tbox)))
    (setf (gethash +top-datatype-role-symbol+ role-store) (tbox-datatype-top-role tbox))
    (setf (gethash `(inv ,+top-datatype-role-symbol+) role-store)
          (role-inverse-internal (tbox-datatype-top-role tbox)))
    (setf (gethash +bottom-datatype-role-symbol+ role-store) (tbox-datatype-bottom-role tbox))
    (setf (gethash `(inv ,+bottom-datatype-role-symbol+) role-store)
          (role-inverse-internal (tbox-datatype-bottom-role tbox))))
  (setf (role-domain-concept (tbox-object-bottom-role tbox)) +bottom-symbol+)
  (setf (role-domain-restriction (tbox-object-bottom-role tbox)) (tbox-bottom-node tbox))
  (setf (role-range-concept (tbox-object-bottom-role tbox)) +bottom-symbol+)
  (setf (role-range-restriction (tbox-object-bottom-role tbox)) (tbox-bottom-node tbox))
  (setf (role-domain-concept (tbox-datatype-bottom-role tbox)) +bottom-symbol+)
  (setf (role-domain-restriction (tbox-datatype-bottom-role tbox)) (tbox-bottom-node tbox))
  (setf (role-range-concept (tbox-datatype-bottom-role tbox)) +datatype-bottom-symbol+)
  (setf (role-range-restriction (tbox-datatype-bottom-role tbox)) (tbox-datatype-bottom-node tbox))
  (preencode-roles-1 tbox)
  (preencode-roles-2 tbox (tbox-encoded-role-list tbox))
  (preencode-roles-3 tbox (tbox-encoded-role-list tbox))
  (preencode-roles-4 tbox))

(defun preencode-roles-1 (tbox)
  (let ((role-index (tbox-role-axioms-index tbox)))
    (loop for description in (tbox-role-axioms tbox)
          for (role-name cd-attribute nil transitive-p parents inverse
                         nil domain range reflexive-p
			 nil nil
			 irreflexive-p
                         nil ; symmetric-p ;currently unused
                         asymmetric-p
                         compositions
                         ) = description
          do
          (when compositions
            (loop for composition in compositions
                  for count = (count role-name composition)
                  when (or (> count 1)
                           (and (eql count 1)
                                (not (or (eql role-name (first composition))
                                         (eql role-name (first (last composition)))))))
                  do (racer-warn "Role composition ~A for role ~A contains ~A illegally -- role composition declaration is ignored."
                                 composition role-name role-name)
                  and collect composition into removed
                  finally 
                  (when removed
                    (setf (role-info-compositions description) (nset-difference compositions removed)))))
          (if irreflexive-p
              (progn
                (when transitive-p
                  (racer-warn "Irreflexive role ~A cannot be transitive -- transitivity declaration is ignored."
                              role-name)
                  (setf (role-info-transitive-p description) nil))
                (when compositions
                  (racer-warn "Irreflexive role ~A cannot be result of role composition axioms -- irreflexivity declaration is ignored."
                              role-name)
                  (setf  (role-info-irreflexive-p description) nil)))
            (when reflexive-p
              (when compositions
                (racer-warn "Reflexive role ~A cannot be result of role composition axioms -- reflexivity declaration is ignored."
                            role-name)
                (setf  (role-info-reflexive-p description) nil))))
          (when asymmetric-p
            (when transitive-p
              (racer-warn "Asymmetric role ~A cannot be transitive -- transitivity declaration is ignored."
                          role-name)
              (setf (role-info-transitive-p description) nil))
            (when compositions
              (racer-warn "Asymmetric role ~A cannot be result of role composition axioms -- asymmetry declaration is ignored."
                          role-name)
              (setf  (role-info-asymmetric-p description) nil)))
          (if cd-attribute
              (push role-name (tbox-all-attributes tbox))
            (progn 
              (when inverse
                (let ((inverse-description (gethash inverse role-index)))
                  (if inverse-description
                      (progn
                        (unless (role-info-inverse inverse-description)
                          (setf (role-info-inverse inverse-description) role-name)
                          (when *debug*
                            (format *trace-output* "~&Autosetting inverse ~A of role ~A.~%"
                                    role-name inverse)))
                        (when (and transitive-p (not (role-info-transitive-p inverse-description)))
                          (setf (role-info-transitive-p inverse-description) t)
                          (when *debug*
                            (format *trace-output* "~&Autosetting transitivity of role ~A as ~
                                                  inverse of transitive role ~A.~%"
                                    inverse role-name)))
                        (when (and (not transitive-p) (role-info-transitive-p inverse-description))
                          (setf (role-info-transitive-p description) t)
                          (when *debug*
                            (format *trace-output* "~&Autosetting transitivity of role ~A as ~
                                                  inverse of transitive role ~A.~%"
                                    role-name inverse)))
                        (when (and (not domain) (role-info-range inverse-description))
                          (setf (role-info-domain description) (role-info-range inverse-description))
                          (when *debug*
                            (format *trace-output* "~&Autosetting domain of role ~A as ~
                                                  range of inverse role ~A.~%"
                                    role-name inverse)))
                        (when (and domain (not (role-info-range inverse-description)))
                          (setf (role-info-range inverse-description) domain)
                          (when *debug*
                            (format *trace-output* "~&Autosetting range of role ~A as ~
                                                  domain of inverse role ~A.~%"
                                    inverse role-name)))
                        (when (and (not range) (role-info-domain inverse-description))
                          (setf (role-info-range description) (role-info-domain inverse-description))
                          (when *debug*
                            (format *trace-output* "~&Autosetting range of role ~A as ~
                                                  domain of inverse role ~A.~%"
                                    role-name inverse)))
                        (when (and range (not (role-info-domain inverse-description)))
                          (setf (role-info-domain inverse-description) range)
                          (when *debug*
                            (format *trace-output* "~&Autosetting domain of role ~A as ~
                                                  range of inverse role ~A.~%"
                                    inverse role-name))))
                    (progn
                      (when *debug*
                        (format *trace-output* "~&Autoinstalling role ~A.~%" inverse))
                      (let ((entry (make-role-info inverse 
                                                   nil ;cd-attribute
                                                   nil ;feature-p
                                                   transitive-p
                                                   nil ;parents
                                                   role-name ;inverse
                                                   nil ;inverse-feature-p
                                                   range ;domain 
                                                   domain ;range
                                                   reflexive-p ;reflexive
                                                   nil ;datatype
                                                   nil ;annotation-p
                                                   nil ;irreflexive-p
                                                   nil ;symmetric-p
                                                   nil ;asymmetric-p
                                                   nil ;compositions
                                                   )))
                        (push entry (tbox-role-axioms tbox))
                        (setf (gethash inverse role-index) entry))))))
              (loop for parent in parents do
                    (unless (gethash parent role-index)
                      (when *debug*
                        (format *trace-output* "~&Autoinstalling role ~A.~%" parent))
                      (let ((entry (make-role-info parent 
                                                   nil ;cd-attribute
                                                   nil ;feature-p
                                                   nil ;transitive-p
                                                   nil ;parents
                                                   nil ;inverse
                                                   nil ;inverse-feature-p
                                                   nil ;domain 
                                                   nil ;range
                                                   nil ;reflexive
                                                   nil ;datatype
                                                   nil ;annotation-p
                                                   nil ;irreflexive-p
                                                   nil ;symmetric-p
                                                   nil ;asymmetric-p
                                                   nil ;compositions
                                                   )))
                        (push entry (tbox-role-axioms tbox))
                        (setf (gethash parent role-index) entry))))
              (loop for composition in compositions do
                    (loop for comp-role in composition do
                          (let ((role-name (if (consp comp-role) (second comp-role) comp-role)))
                            (unless (gethash role-name role-index)
                              (when *debug*
                                (format *trace-output* "~&Autoinstalling role ~A.~%" comp-role))
                              (let ((entry (make-role-info role-name 
                                                           nil ;cd-attribute
                                                           nil ;feature-p
                                                           nil ;transitive-p
                                                           nil ;parents
                                                           nil ;inverse
                                                           nil ;inverse-feature-p
                                                           nil ;domain 
                                                           nil ;range
                                                           nil ;reflexive
                                                           nil ;datatype
                                                           nil ;annotation-p
                                                           nil ;irreflexive-p
                                                           nil ;symmetric-p
                                                           nil ;asymmetric-p
                                                           nil ;compositions
                                                           )))
                                (push entry (tbox-role-axioms tbox))
                                (setf (gethash role-name role-index) entry))))))))))
  (setf (tbox-encoded-role-list tbox)
        (loop for (role-name cd-attribute feature-p transitive-p parents
                             inverse inverse-feature-p domain range reflexive-p datatype annotation-p
			     irreflexive-p symmetric-p asymmetric-p compositions)
              in (tbox-role-axioms tbox)
              for role-temp = (get-tbox-role tbox role-name nil)
              for role = (unless (and role-temp (is-predefined-role-p role-temp))
                           (encode-tbox-role-term role-name 
                                                  cd-attribute
                                                  feature-p
                                                  transitive-p 
                                                  inverse
                                                  inverse-feature-p
                                                  domain
                                                  range
                                                  reflexive-p
                                                  datatype
                                                  annotation-p
                                                  irreflexive-p
                                                  symmetric-p
                                                  asymmetric-p
                                                  compositions))
              when role
              collect role
              and do
              (when feature-p
                (setf (tbox-all-features tbox) (cons role-name (tbox-all-features tbox))))
              (when (and transitive-p (not feature-p))
                (setf (tbox-all-transitive-roles tbox) (cons role-name (tbox-all-transitive-roles tbox))))
              (unless cd-attribute 
                (setf (tbox-all-roles tbox) (cons role-name (tbox-all-roles tbox))))
              (setf (role-parents-internal role) 
                    parents))) ; At this time we have symbols as the parents entries.

  (loop for role in (tbox-encoded-role-list tbox) do
        (loop with composition-changed-p = nil
              for composition in (role-compositions role)
              collect
              (loop with changed-p = nil
                    for predecessor-name = nil then role-name
                    for role-name in composition
                    if (and (eq predecessor-name role-name) (role-transitive-p (get-tbox-role tbox role-name)))
                    do (racer-warn "Redundant transitive role ~A removed from role composition axiom ~A for role ~A"
                                   role-name composition (role-name role))
                    (setf changed-p t)
                    and
                    unless composition-changed-p
                    do (setf composition-changed-p t)
                    end
                    else collect role-name into new-composition
                    finally
                    (if changed-p
                        (progn
                          (racer-warn "New simplified role axiom for role ~A is ~A"
                                      (role-name role) new-composition)
                          (return new-composition))
                      (return composition)))
              into new-compositions
              finally
              (when composition-changed-p
                (setf (role-compositions role) new-compositions))))

  ;; Make sure only the direct superroles are stored in the internal-parents slot.
  (loop for role in (tbox-encoded-role-list tbox) do
        (setf (role-parents-internal role) 
              (compute-direct-superroles tbox (role-name role) (role-parents-internal role))))

  (loop for role in (tbox-internal-roles tbox) do
        (setf (role-internal-name-p (get-tbox-role tbox role)) t))

  ;; We still have symbols as parent entries, but only the direct superroles are now stored.

)

(defun compute-direct-superroles (tbox rolename parents)
  (let ((visited #+:allegro (racer-make-hash-table
                             :size 100 :rehash-size 2.0 :structure-p t)
                 #-:allegro (racer-make-hash-table :size 100)))
    (remove-duplicates 
     (set-difference parents
                     (loop for parent in parents 
                           append (progn (clrhash visited) 
                                         (setf (gethash rolename visited) t)
                                         (compute-ancestors-preliminary tbox parent visited)))))))



(defun compute-ancestors-preliminary (tbox rolename visited)
  ;;(break "~S" role)
  ;; The goal of this function is to compute the ancestors without following cycles!!!
  (if (gethash rolename visited)
    nil
    (let* ((role  (get-tbox-role tbox rolename))
           (parents  (role-parents-internal role))
           (ancestors (loop for parent in parents 
                            unless (gethash parent visited)
                            collect parent)))
      (setf (gethash rolename visited) t)
      (loop for parent in parents do
            (setf ancestors (append ancestors (compute-ancestors-preliminary tbox parent visited)))
            finally
            (return ancestors)))))



(defun preencode-roles-2 (tbox encoded-role-list)
  ;; The goal is to setup the role hierarchy appropriately.
  ;; In particular, the inverses have to be defined.
  ;; Partial information is given by role axioms. 
  ;; Information coming from role axioms is defined wrt. role NAMES.
  ;; Hence, we must transform names into structure instances.
  ;; If some role axioms specifies the inverse, (role-inverse-internal role)
  ;; contains a symbol for the role name.
  (loop for role in encoded-role-list do
        (unless (internal-role-p (role-name role))
          ;; For internal cd attributes, the inverse is not defined.
          ;; So, the following is skipped in this case.
          (if (role-inverse-internal role)
            ;; The name of the inverse is specified by role axioms.
            (let* ((inverse-role-name (role-inverse-internal role))
                   (inverse-role (get-tbox-role tbox inverse-role-name nil)))
              (if (null inverse-role)
                ;; Although the name of an inverse role is specified in role axioms,
                ;; the role might not be encoded. Hence,
                ;; encode the name of the inverse just in case.
                ;; Inverse data structure pointer setup is done later.
                (push (encode-tbox-role-term inverse-role-name
                                             nil ; no cd-attribute
                                             nil
                                             (role-transitive-p role)
                                             (role-name role)
					     nil
                                             (role-range-concept role)
                                             (role-domain-concept role)
                                             (role-reflexive-p role)
                                             nil
                                             (role-annotation-p role)
					     nil
					     nil
					     nil
                                             nil)
                      encoded-role-list)
                ;; The inverse is already encoded as a structure instance:
                ;; However, the inverse of the inverse might not be setup correctly.
                (let ((inverse-inverse-role (role-inverse-internal inverse-role)))
                  (cond ((null inverse-inverse-role)
                         ;; Initialize the inverse of the inverse appropriately.
                         (setf (role-inverse-internal inverse-role) (role-name role)))
                        ((not (eq inverse-inverse-role (role-name role)))
                         ;; The inverse of the inverse is not the original.
                         ;; So, we enforce an equivalence, but do not setup
                         ;; the data structures right now. This must be done later.
                         ;; Note that inverse-inverse-role must refer
                         ;; to a symbol in this stage.
                         (roles-equivalent-1 inverse-inverse-role (role-name role) tbox))))))
            ;; In this case, the inverse of a role is not defined (nil).
            ;; An internal name is generated and pointers between data structure 
            ;; objects are setup by referring to names. This must be changed 
            ;; later on.
            (let ((role-info (gethash (role-name role) (tbox-role-axioms-index tbox))))
              #+:debug (assert role-info)
              (let ((inverse-role (encode-tbox-role-term  
                                   (generate-inverse-role (role-name role))
                                   nil ; no cd-attribute
                                   (role-info-inverse-feature-p role-info)
                                   (role-transitive-p role)
                                   (role-name role)
                                   nil
                                   (role-range-concept role)
                                   (role-domain-concept role)
                                   (role-reflexive-p role)
                                   nil
                                   (role-annotation-p role)
                                   nil
                                   nil
                                   nil
                                   nil)))
                (setf (role-internal-name-p inverse-role) t)
                (push inverse-role encoded-role-list)
                (setf (role-inverse-internal role) (role-name inverse-role)))))))

  (setf (tbox-encoded-role-list tbox) encoded-role-list)
  
  ;; At this stage, reference to inverses and parents etc. are still encoded as symbols.
  ;; Another scan through the roles is necessary (note that encoded-role-list
  ;; might have been extended in the previous step).
  ;; The goal is to replace references by names with reference by structure instances.
  ;; In addition, the parents and children slots are initialized.

  (loop for role in (tbox-encoded-role-list tbox) do
        (unless (internal-role-p (role-name role))
          ;; internal CDS attributes do not have inverses, children or parents,
          ;; So we skip this step for internal cd attributes.
          (setf (role-inverse-internal role) (get-tbox-role tbox (role-inverse-internal role)))
          (let* ((role-inverse (role-inverse-internal role))
                 (role-parents (role-parents-internal role))
                 (role-inverse-parents (role-parents-internal role-inverse)))

            ;; Here we move from symbols to structures.
            (setf (role-parents-internal role)
                  (mapcar #'(lambda (pname) 
                              (safe-get-tbox-role tbox pname))
                          role-parents))

            ;; There might be role structures accessible via role-children-internal due
            ;; to the fact the  current role was already "touched" by processing the inverse role!
	    ;; So, we use pushnew.
	    ;; pushnew is too expensive in case of many children. We'll later remove duplicates
            (loop for parent in (role-parents-internal role) do
                  (push role (role-children-internal parent)))
            (setf (role-parents-internal role-inverse)
                  (mapcar #'(lambda (pname) 
                              (safe-get-tbox-role tbox pname))
                          role-inverse-parents))
            (loop for parent in (role-parents-internal role-inverse) do
                  (push role-inverse (role-children-internal parent)))
            (if (null role-parents)
              (when role-inverse-parents
                (setf (role-parents-internal role)
                      (mapcar #'(lambda (rname)
                                  (safe-get-tbox-role tbox
                                                      (role-inverse-internal (safe-get-tbox-role tbox rname))))
                              role-inverse-parents)))
              (if (null role-inverse-parents)
                (setf (role-parents-internal role-inverse)
                      (mapcar #'(lambda (rname)
                                  (safe-get-tbox-role tbox
                                                      (role-inverse-internal (safe-get-tbox-role tbox rname))))
                              role-parents)))))
          (pre-encode-role-compositions tbox role))))

(defun pre-encode-role-compositions (tbox role)
  (let ((compositions (role-compositions role))
        (inverse (role-inverse-internal role)))
    (when (and compositions inverse)
      (unless (role-compositions inverse)
        (setf (role-compositions inverse)
              (loop with store = (tbox-role-store tbox)
                    for composition in compositions
                    collect
                    (nreverse
                     (loop for role-spec in composition
                           collect (if (symbolp role-spec)
                                       (role-inverse-internal (gethash role-spec store))
                                     (if (role-node-p role-spec)
                                         (role-inverse-internal role-spec)
                                       (second role-spec)))))))))))

(defun add-provisionally-encoded-roles (tbox role-list)
  (loop for role in role-list
        for role-name = (role-name role) do
        (when (role-feature-p role)
          (setf (tbox-all-features tbox) (cons role-name (tbox-all-features tbox))))
        (when (and (role-transitive-p role) (not (role-feature-p role)))
          (setf (tbox-all-transitive-roles tbox) 
                (cons role-name (tbox-all-transitive-roles tbox))))
        (unless (role-cd-attribute role)
          (unless (role-internal-name-p role) 
            (setf (tbox-all-roles tbox) (cons role-name (tbox-all-roles tbox)))))
        (setf (tbox-encoded-role-list tbox)
              (cons role (tbox-encoded-role-list tbox)))))

  
(defun preencode-roles-3 (tbox encoded-role-list) 
  ;; From now on data structures are setup appropriately (thus, references no longer 
  ;; use names. However, role synonyms might still exist.

  ;; There might be different named parents for a role and its inverse.
  ;; Here, the role hierarchie is setup correctly w.r.t. inverse roles.

  (loop for role in encoded-role-list
        for role-inverse = (role-inverse-internal role)
        do
	(unless (role-cd-attribute role)
          (loop for parent in (role-parents-internal role) 
		unless (member parent (role-parents-internal role-inverse)) do
                (push role-inverse (role-children-internal (role-inverse-internal parent)))
                (push (role-inverse-internal parent) (role-parents-internal role-inverse)))
          (loop for inverse-parent in (role-parents-internal role-inverse) 
		unless (member inverse-parent (role-parents-internal role)) do
                (push role (role-children-internal (role-inverse-internal inverse-parent)))
                (push (role-inverse-internal inverse-parent) (role-parents-internal role)))
          (loop with parents = (role-parents-internal role)
                with changed-p = nil
                for inverse-parent in (role-parents-internal role-inverse)
                for inverse-inverse-parent = (role-inverse-internal inverse-parent)
                unless (member inverse-inverse-parent parents) do
                (push inverse-inverse-parent parents)
                (push role (role-children-internal inverse-inverse-parent))
                (unless changed-p
                  (setf changed-p t))
                finally 
                (when changed-p 
                  (setf (role-parents-internal role) parents)))
          (loop with inverse-parents = (role-parents-internal role-inverse)
                with changed-p = nil
                for parent in (role-parents-internal role)
                for inverse-parent = (role-inverse-internal parent)
                unless (member inverse-parent inverse-parents) do
                (push inverse-parent inverse-parents)
                (push role-inverse (role-children-internal inverse-parent))
                (unless changed-p
                  (setf changed-p t))
                finally 
                (when changed-p 
                  (setf (role-parents-internal role-inverse) inverse-parents)))))

  (remove-role-synonyms tbox encoded-role-list) ; this might change tbox-encoded-role-list

  ;; Due to synonym detection, the role hiearchy might not be set up correctly.
  ;; Not only the direct parents/children are stored in role-parents-internal/role-children-internal
  ;; This is fixed in encode-role finally.
  
  ;; From now on all role synonyms must have been detected.
  ;; Due to role synonym computation, new features or transitives roles
  ;; might have shown up. So we scan the roles again in order to detect this.
  
  (let ((encoded-role-list (tbox-encoded-role-list tbox))
        (all-features nil)
        (all-transitive-roles nil)
        (all-roles (list (role-name (tbox-object-top-role tbox))
                         (role-name (tbox-object-bottom-role tbox))
                         (role-name (tbox-datatype-top-role tbox))
                         (role-name (tbox-datatype-bottom-role tbox)))))
    
    (loop for role in encoded-role-list
          unless (role-removed-p role)
          do
          ;; IS THIS STEP REALLY NECESSARY??? 
          ;; Yes, we can't use pushnew for role-children-internal due to possibly numerous role children
          (setf (role-children-internal role)
                (role-set-remove-duplicates (role-children-internal role)))
          (setf (role-parents-internal role)
                (role-set-remove-duplicates (role-parents-internal role)))
          (unless (role-cd-attribute role)
            #+:debug
            (let ((role-inverse (role-inverse-internal role)))
              #|
	      The condition checked here can indeed happen.
	      Example:
	      (implies-role s r)
	      (implies-role t r)
	      (inverse s t)

	      (unless (or (eq role role-inverse)
                          (and (role-set-subsetp (role-parents-internal role)
                                                 (mapcar #'role-inverse-internal
                                                         (role-parents-internal role-inverse)))
                               (role-set-subsetp (mapcar #'role-inverse-internal
                                                         (role-parents-internal role))
                                                 (role-parents-internal role-inverse))))
                (error "Parents ~S of role ~S and parents ~S of its inverse ~S do not match."
                       (role-parents-internal role)
                       role
                       (role-parents-internal role-inverse)
		       role-inverse))
		       |#
              (unless (eq (role-transitive-p role) (role-transitive-p role-inverse))
                (error "Transitivity of role ~S and its inverse ~S do not match."
                       role role-inverse)))
            (let* ((inverse (role-inverse-internal role))
                   (role-term (decode-role inverse))
                   (synonyms (loop for synonym in (role-synonyms-internal role)
                                   unless (eq synonym role)
                                   collect (decode-role synonym))))
              (if (role-feature-p inverse)
                  (progn
                    (push role-term all-features)
                    (setf all-features (append synonyms all-features)))
                (when (role-transitive-p inverse)
                  (push role-term all-transitive-roles)
                  (setf all-transitive-roles (append synonyms all-transitive-roles))))
              (push role-term all-roles)
              (setf all-roles (nconc synonyms all-roles))))
          finally
          (setf (tbox-all-features tbox) (racer-remove-duplicates all-features :test 'equal))
          (setf (tbox-all-transitive-roles tbox)
                (racer-remove-duplicates all-transitive-roles :test 'equal))
          (setf (tbox-all-roles tbox) (racer-remove-duplicates all-roles :test 'equal)))))

(defun preencode-roles-4 (tbox)
  (push (tbox-object-bottom-role tbox) (tbox-encoded-role-list tbox))
  (push (tbox-object-top-role tbox) (tbox-encoded-role-list tbox))
  (push (tbox-datatype-bottom-role tbox) (tbox-encoded-role-list tbox))
  (push (tbox-datatype-top-role tbox) (tbox-encoded-role-list tbox))
  (loop with top = (tbox-object-top-role tbox)
        with bottom = (tbox-object-bottom-role tbox)
        with datatype-top = (tbox-datatype-top-role tbox)
        with datatype-bottom = (tbox-datatype-bottom-role tbox)
        for role being the hash-value of (tbox-role-store tbox)
        do
        (unless (or (role-cd-attribute role)
                    (eq role top)
                    (eq role bottom)
                    (eq role datatype-top)
                    (eq role datatype-bottom))
          (if (role-datatype role)
              (progn
                (unless (role-parents-internal role)
                  (setf (role-parents-internal role) (list datatype-top))
                  (push role (role-children-internal datatype-top)))
                (unless (role-children-internal role)
                  (setf (role-children-internal role) (list datatype-bottom))
                  (push role (role-parents-internal datatype-bottom))))
            (progn
              (if (role-asymmetric-p role)
                  (let ((inverse (role-inverse-internal role)))
                    (setf (role-asymmetric-p inverse) t)
                    #+:debug (assert (role-irreflexive-p role))
                    (setf (role-irreflexive-p inverse) t))
                (if (role-irreflexive-p role)
                    (setf (role-irreflexive-p (role-inverse-internal role)) t)
                  (when (role-reflexive-p role)
                    (setf (role-reflexive-p (role-inverse-internal role)) t))))
              (unless (role-parents-internal role)
                (setf (role-parents-internal role) (list top))
                (push role (role-children-internal top)))
              (unless (role-children-internal role)
                (setf (role-children-internal role) (list bottom))
                (push role (role-parents-internal bottom)))
              (when (role-transitive-p role)
                (setf (role-simple-p role) nil)
                (setf (role-simple-p (role-inverse-internal role)) nil))
              (when (role-compositions role)
                (setf (role-simple-p role) nil)
                (setf (role-compositions role)
                      (loop with store = (tbox-role-store tbox)
                            for composition in (role-compositions role)
                            collect
                            (loop for role-spec in composition
                                  collect (if (role-node-p role-spec)
                                              role-spec
                                            (if (symbolp role-spec) 
                                                (gethash role-spec store)
                                              (role-inverse-internal (gethash (second role-spec) store)))))))))))
        finally
        (unless (role-children-internal datatype-top)
          (setf (role-children-internal datatype-top) (list datatype-bottom))
          (push datatype-top (role-parents-internal datatype-bottom)))
        (unless (role-parents-internal datatype-bottom)
          (setf (role-parents-internal datatype-bottom) (list datatype-top))
          (push datatype-bottom (role-children-internal datatype-top)))
        (unless (role-children-internal top)
          (setf (role-children-internal top) (list bottom))
          (push top (role-parents-internal bottom)))
        (unless (role-parents-internal bottom)
          (setf (role-parents-internal bottom) (list top))
          (push bottom (role-children-internal top))))
  (when (tbox-role-antonyms tbox)
    (setf (tbox-language tbox) (add-dl-disjoint-roles (tbox-language tbox)))
    (loop for (role-1-name . role-2-name) in (tbox-role-antonyms tbox)
          for role-1 = (get-tbox-role tbox role-1-name)
          for role-2 = (get-tbox-role tbox role-2-name)
          for role-1-inv = (role-inverse-internal role-1)
          for role-2-inv = (role-inverse-internal role-2)
          do
          (if (not (role-simple-p role-1))
              (racer-warn "Non-simple role ~A cannot be declared as disjoint with role ~A -- ~
                             disjointness declaration is ignored."
                          role-1 role-2)
            (if (not (role-simple-p role-2))
                (racer-warn "Non-simple role ~A cannot be declared as disjoint with role ~A -- ~
                               disjointness declaration is ignored."
                            role-2 role-1)
              (progn
                (pushnew role-1 (role-disjoint-roles role-2))
                (pushnew role-2 (role-disjoint-roles role-1))
                (pushnew role-1-inv (role-disjoint-roles role-2-inv))
                (pushnew role-2-inv (role-disjoint-roles role-1-inv)))))))
  (propagate-role-characteristics-to-children (tbox-object-top-role tbox))
  (propagate-role-characteristics-to-ancestors tbox)
  (check-ria-regularity tbox)
  (loop with tbox-language = (tbox-language tbox)
        with reflexive = nil
        with irreflexive = nil
        with symmetric = nil
        with asymmetric = nil
        with disjoint = nil
        with complex = nil
        for role in (tbox-encoded-role-list tbox) do
        (unless (or (is-predefined-role-p role) (role-datatype role))
          (when (and (not reflexive) (role-reflexive-p role))
            (setf tbox-language (add-dl-reflexive tbox-language))
            (setf reflexive t))
          (when (and (not irreflexive) (role-irreflexive-p role))
            (setf tbox-language (add-dl-irreflexive tbox-language))
            (setf irreflexive t))
          (when (and (not symmetric) (role-symmetric-p role))
            (setf tbox-language (add-dl-symmetric tbox-language))
            (setf symmetric t))
          (when (and (not asymmetric) (role-asymmetric-p role))
            (setf tbox-language (add-dl-asymmetric tbox-language))
            (setf asymmetric t)
            (setf tbox-language (add-dl-irreflexive tbox-language))
            (setf irreflexive t))
          (when (and (not disjoint) (role-disjoint-roles role))
            (setf tbox-language (add-dl-disjoint-roles tbox-language))
            (setf disjoint t))
          (when (and (not complex) (role-compositions role))
            (setf tbox-language (add-dl-complex-role-inclusions tbox-language))
            (setf complex t)))
        until (and reflexive irreflexive symmetric asymmetric disjoint complex)
        finally (setf (tbox-language tbox) tbox-language))
  nil)

(defun propagate-role-characteristics-to-children (parent)
  (loop with disjoint-roles = (role-disjoint-roles parent)
        with inverse-disjoint-roles = (role-disjoint-roles (role-inverse-internal parent))
        with asymmetric = (role-asymmetric-p parent)
        with irreflexive = (or asymmetric (role-irreflexive-p parent))
        for child in (role-children-internal parent)
        for child-inverse = (role-inverse-internal child)
        do
        (unless (is-bottom-object-role-p child)
          (when disjoint-roles
            (let ((child-disjoint-roles (role-disjoint-roles child)))
              (when (or (null child-disjoint-roles)
                        (not (role-set-subsetp disjoint-roles child-disjoint-roles)))
                (setf (role-disjoint-roles child)
                      (role-set-union disjoint-roles child-disjoint-roles))
                (setf (role-disjoint-roles child-inverse)
                      (role-set-union inverse-disjoint-roles (role-disjoint-roles child-inverse)))
                (loop for disjoint in disjoint-roles do
                      (pushnew child (role-disjoint-roles disjoint))
                      (pushnew child-inverse (role-disjoint-roles (role-inverse-internal disjoint)))))))
          (if asymmetric
              (progn
                (setf (role-asymmetric-p child) t)
                (setf (role-irreflexive-p child) t)
                (setf (role-asymmetric-p child-inverse) t)
                (setf (role-irreflexive-p child-inverse) t))
            (when irreflexive
              (setf (role-irreflexive-p child) t)
              (setf (role-irreflexive-p child-inverse) t)))
          (propagate-role-characteristics-to-children child))))

(defun propagate-role-characteristics-to-parents (child reflexive composition)
  (loop for parent in (role-parents-internal child) do
        (unless (is-top-object-role-p parent)
          (when (and reflexive (not (role-reflexive-p parent)))
            (setf (role-reflexive-p parent) t)
            (setf (role-reflexive-p (role-inverse-internal parent)) t))
          (when (and (or reflexive composition) (role-simple-p parent))
            (setf (role-simple-p parent) nil)
            (setf (role-simple-p (role-inverse-internal parent)) nil))
          (propagate-role-characteristics-to-parents parent reflexive composition))))

(defun propagate-role-characteristics-to-ancestors (tbox)
  (loop for role in (tbox-encoded-role-list tbox) do
        (unless (or (is-predefined-role-p role) (role-datatype role))
          (let ((reflexive (role-reflexive-p role))
                (composition (role-compositions role)))
            (when (or reflexive composition)
              (propagate-role-characteristics-to-parents role reflexive composition))))))

(defun check-ria-regularity (tbox)
  (flet ((mark-roles-for-cycle-check (tbox)
           (let ((initial-flag (incf *role-set-mark*)))
             (loop for role in (tbox-encoded-role-list tbox) do
                   (unless (ignore-role-in-check-ria-regularity-p role)
                     (setf (role-flag role) initial-flag)))
             (loop with visited-roles = (make-hash-table)
                   for child in (role-children-internal (tbox-object-top-role tbox))
                   do
                   (when (and (not (ignore-role-in-check-ria-regularity-p child))
                              (<= (role-flag child) initial-flag))
                     (traverse-roles-for-ria-regularity initial-flag child visited-roles))))))
    ;(print-compositions tbox)
    (loop with visited-roles = (make-hash-table)
          for iteration from 0
          do (mark-roles-for-cycle-check tbox)
          until (not (check-for-ria-cycle tbox visited-roles))
          finally
          (when (> iteration 0)
            (racer-warn "In total ~D role chain and/or subrole inclusion axioms were ignored because they caused cycles."
                        iteration)))))

(defun traverse-roles-for-ria-regularity (initial-flag role visited-roles)
  (unless (gethash (role-name role) visited-roles)
    (setf (gethash (role-name role) visited-roles) t)
    #+:debug (assert (<= (role-flag role) initial-flag))
    (loop for composition in (role-compositions role) do
          (loop for comp-role in composition do
                (unless (eq comp-role role)
                  (traverse-roles-for-ria-regularity initial-flag comp-role visited-roles))))
    (loop for child in (role-children-internal role) do
          (unless (ignore-role-in-check-ria-regularity-p child)
            (traverse-roles-for-ria-regularity initial-flag child visited-roles)))
    (setf (role-flag role) (incf *role-set-mark*))))

(defun check-for-ria-cycle (tbox visited-roles)
  (loop for role in (tbox-encoded-role-list tbox)
        do
        (unless (ignore-role-in-check-ria-regularity-p role)
          (multiple-value-bind (culprit reason parent)
              (check-for-ria-cycle-1 role (clrhash visited-roles))
            (when culprit
              (report-cycle-in-ria culprit reason parent)
              (return t))))))

(defun check-for-ria-cycle-1 (role visited-roles)
  (unless (or (ignore-role-in-check-ria-regularity-p role)
              (gethash (role-name role) visited-roles))
    (setf (gethash (role-name role) visited-roles) t)
    (let ((role-flag (role-flag role)))
      (when (role-compositions role)
        (loop for composition in (role-compositions role) do
              (loop for comp-role in composition do
                    (unless (eq comp-role role)
                      (if (> (role-flag comp-role) role-flag)
                          (return-from check-for-ria-cycle-1 (values role ':composition comp-role))
                        (multiple-value-bind (culprit reason other-role)
                            (check-for-ria-cycle-1 comp-role visited-roles)
                          (when culprit
                            (return-from check-for-ria-cycle-1 (values culprit reason other-role)))))))))
      (loop for parent in (role-parents-internal role) do
            (unless (ignore-role-in-check-ria-regularity-p parent)
              (if (< (role-flag parent) role-flag)
                  (return-from check-for-ria-cycle-1 (values role ':parent parent))
                (multiple-value-bind (culprit reason parent)
                    (check-for-ria-cycle-1 parent visited-roles)
                  (when culprit
                    (return-from check-for-ria-cycle-1 (values culprit reason parent))))))))))

(defun ignore-role-in-check-ria-regularity-p (role)
  (or (is-predefined-role-p role)
      (role-internal-name-p role)
      (role-datatype role)
      (role-internal-conjunction-p role)))

(defun report-cycle-in-ria (role reason other-role)
  #+:debug (assert other-role)
  (let ((role-name (nice-role-name role)))
    ;(print-compositions tbox)
    ;(break)
    (let ((role-inverse (role-inverse-internal role))
          (other-role-inverse (role-inverse-internal other-role)))
      (if (eq reason ':composition)
          (progn
            (racer-warn "~&Ignoring role chain inclusion axioms for ~A because they cause a cycle for ~A."
                        role-name role-name)
            (setf (role-compositions role)
                  (loop for composition in (role-compositions role)
                        unless (member other-role composition)
                        collect composition))
            (setf (role-compositions role-inverse)
                  (loop for composition in (role-compositions role-inverse)
                        unless (member other-role-inverse composition)
                        collect composition)))
        (progn
          (racer-warn "~&Ignoring subrole inclusion axioms for ~A because they cause a cycle for ~A."
                      role-name role-name)
          (setf (role-parents-internal role) (remove other-role (role-parents-internal role)))
          (setf (role-parents-internal role-inverse)
                (remove other-role-inverse (role-parents-internal role-inverse)))
          (setf (role-children-internal other-role) (remove role (role-children-internal other-role)))
          (setf (role-children-internal other-role-inverse)
                (remove role-inverse (role-children-internal other-role-inverse)))))))
  role)

#+:debug
(defun print-compositions (tbox)
  ;; only used for debugging cycle detection
  (pprint (loop with result = nil
                for role in (tbox-encoded-role-list tbox)
                for compositions = (role-compositions role)
                do
                (unless (ignore-role-in-check-ria-regularity-p role)
                  (when compositions
                    (push (list (mapcar (lambda (composition)
                                          (mapcar (lambda (comp-role)
                                                    (list comp-role (role-flag comp-role)))
                                                  composition))
                                        compositions)
                                (list role (role-flag role)))
                          result))
                  (if (every #'ignore-role-in-check-ria-regularity-p (role-parents-internal role))
                      (push (list role (role-flag role)) result)
                    (push (list (list role (role-flag role))
                                (loop for parent in (role-parents-internal role)
                                      unless (ignore-role-in-check-ria-regularity-p parent)
                                      collect (list parent (role-flag parent))))
                          result)))
                finally (return (reverse result))))
  (fresh-line *standard-output*))

(defun remove-role-from-compositions (role compositions)
  (loop for composition in compositions
        collect
        (if (eql role (first composition))
            (rest composition)
          (if (eql role (first (last composition)))
              (butlast composition)
            composition))))

(defun nice-role-name (role)
  (decode-role (if (and (role-internal-name-p role) (role-inverse-internal role))
                   (role-inverse-internal role)
                 role)))

(defun role-removed-p (role)
  (null (role-synonyms-internal role)))

(defun collapse-roles (role removed-role tbox)
  #+:debug
  (when (role-removed-p role)
    (error "Not expected: role ~S already removed." role))
  #+:debug
  (when *debug-r*
    ;(break "Collapse: ~S" (list (role-name role) (role-name removed-role)))
    )
  #+:debug
  (when (eq role removed-role)
    (error "Not expected: role ~S and removed role ~S equal" role removed-role))

  (unless (or (role-internal-name-p removed-role) (internal-role-p (role-name removed-role)))
    (tbox-classification-report t ':roles-equivalent (role-name role) (role-name removed-role)))

  #+:debug
  (when *debug-r*
    (format t "~%Role ~S is removed."
            (role-name removed-role)))
               
  ;(push removed-role removed-roles)
               
  (pushnew removed-role (role-synonyms-internal role))
               
  ;; Establish correct name mappings.
  (loop for role-1 in (role-synonyms-internal removed-role) do
        (setf (gethash (role-name role-1) (tbox-role-store tbox)) role))
               
  (setf (role-synonyms-internal role)
        (union (role-synonyms-internal role)
               (remove removed-role (role-synonyms-internal removed-role))))
  (setf (role-synonyms-internal removed-role) nil) ; Marker for being removed.

  (cond ((and (null (role-cd-attribute role)) 
              (not (null (role-cd-attribute removed-role))))
         (setf (role-cd-attribute role) (role-cd-attribute removed-role)))
        ((and (null (role-cd-attribute removed-role)) 
              (not (null (role-cd-attribute role))))
         nil)
        (t (unless (or (is-bottom-datatype-role-p role)
                       (equal (role-cd-attribute role) (role-cd-attribute removed-role)))
             (error "Equality of concrete domain attributes ~a and ~a ~
                     cannot be enforced because the domain are disjoint - ~
                     found ~a and ~a, respectively."
                    role removed-role (role-cd-attribute role)
                    (role-cd-attribute removed-role)))))

  (unless (role-cd-attribute role)
    ;; Deal with parents and children and take care of removed role being
    ;; a subrole of role.
    (let ((bottom-role-p (is-bottom-object-role-p role)))
      (setf (role-parents-internal role)
            (role-set-union (remove removed-role (role-parents-internal role))
                            (loop for role-1 in (role-parents-internal removed-role)
                                  unless (or (eq role-1 role) (is-top-object-role-p role-1))
                                  collect role-1)))
      (unless bottom-role-p
        (setf (role-children-internal role)
              (role-set-union (remove removed-role (role-children-internal role))
                              (loop for role-1 in (role-children-internal removed-role)
                                    unless (or (eq role-1 role) (is-bottom-object-role-p role-1))
                                    collect role-1))))
                 
      (loop for parent in (role-parents-internal removed-role)
            unless (eq parent role) do
            (setf (role-children-internal parent)
                  (remove removed-role (role-children-internal parent)))
            (unless (is-top-object-role-p parent)
              (pushnew role (role-children-internal parent))))

      (loop for child in (role-children-internal removed-role)
            unless (eq child role) do
            (setf (role-parents-internal child)
                  (remove removed-role (role-parents-internal child)))
            (unless (is-bottom-object-role-p child)            
              (pushnew role (role-parents-internal child))))
      ;; Note that changing the parents and children this way might
      ;; result in the fact the role-parents-internal and
      ;; role-children-internal do not any longer contain only the direct parents
      ;; and children, respectively!!!

      (if bottom-role-p
          (loop for child in (role-children-internal removed-role)
                unless (eq child role) do
                (collapse-roles role child tbox))
        (progn
          (when (role-transitive-p removed-role)
            (setf (role-transitive-p role) t))
                 
          (when (role-feature-p removed-role)
            (setf (role-feature-p role) t))
                 
          (when (role-reflexive-p removed-role)
            (setf (role-reflexive-p role) t))
                 
          (when (role-inverse-feature-p removed-role)
            (setf (role-inverse-feature-p role) t))

          (when (role-annotation-p removed-role)
            (setf (role-annotation-p role) t))

          (when (role-irreflexive-p removed-role)
            (setf (role-irreflexive-p role) t))

          (when (role-symmetric-p removed-role)
            (setf (role-symmetric-p role) t))

          (when (role-asymmetric-p removed-role)
            (setf (role-asymmetric-p role) t))

          (let ((datatype (role-datatype role))
                (removed-datatype (role-datatype removed-role)))
            (when removed-datatype
              (if (and datatype (not (eq datatype t)))
                  (unless (or (eq removed-datatype t) (equal removed-datatype datatype))
                    (error "cannot intersect the datatypes ~A and ~A of datatype properties ~A and ~A"
                           datatype removed-datatype (role-name role) (role-name removed-role)))
		(unless (is-bottom-datatype-role-p role)
		  (setf (role-datatype role) removed-datatype)))))

          (unless (role-simple-p removed-role)
            (setf (role-simple-p role) nil))
          
          (let ((compositions (role-compositions role))
                (removed-compositions (role-compositions removed-role)))
            (when removed-compositions
              (setf (role-compositions role)
                    (union compositions removed-compositions :test 'equal))))
              
                 
          ;; It might be possible here that a role is its own inverse.
          ;; This has consequences for the role hierarchy. We detect these
          ;; cases using specific code.
          (when (eq (role-inverse-internal role) removed-role)
            (setf (role-inverse-internal role) role)
            (racer-warn "Role ~S found to be symmetric." role))
                 
          (when (eq (role-inverse-internal (role-inverse-internal removed-role))
                    removed-role)
            (setf (role-inverse-internal (role-inverse-internal removed-role))
                  role));(break "2")
                 
          (when (role-removed-p (role-inverse-internal role))
            #+:debug
            (when *debug-r*
              (when (role-removed-p (role-inverse-internal removed-role))
                (error "another removed role is used -- not expected.")))
            (setf (role-inverse-internal role) (role-inverse-internal removed-role)))))

      #+:debug
      (when *debug-r*
        (print (role-parents-internal role))
        (print (role-children-internal role))
        (print (role-inverse-internal role))
        (print (role-inverse-internal (role-inverse-internal removed-role)))))))

(defun collapse-roles-new (role removed-role tbox stream &optional (report-p t))
  (let ((role-inverse (role-inverse-internal role))
        (removed-role-inverse (role-inverse-internal removed-role)))
    #+:debug (assert (not (role-removed-p role)))
    #+:debug (assert (not (role-removed-p removed-role)))
    #+:debug (assert (not (eq role removed-role)))
    #+:debug (assert (or (is-bottom-object-role-p role)
                         (not (or (role-parents-internal role) (role-children-internal role)))))
               
    (when report-p
      (unless (or (role-internal-name-p removed-role) (internal-role-p (role-name removed-role)))
        (unless (role-internal-name-p role)
          (tbox-classification-report stream ':roles-equivalent (role-name role) (role-name removed-role))))
      (unless (or (role-internal-name-p removed-role-inverse) (eq role role-inverse))
        (unless (role-internal-name-p role-inverse)
          (tbox-classification-report stream ':roles-equivalent
                                      (role-name role-inverse) (role-name removed-role-inverse)))))
               
    (loop for role-1 in (role-synonyms-internal removed-role) do
          (setf (gethash (role-name role-1) (tbox-role-store tbox)) role))
    (unless (eq removed-role removed-role-inverse)
      (loop for role-1 in (role-synonyms-internal removed-role-inverse) do
            (setf (gethash (role-name role-1) (tbox-role-store tbox)) role-inverse)))
    (setf (role-synonyms-internal role)
          (role-set-union (role-synonyms-internal role) (role-synonyms-internal removed-role)))
    (unless (eq role role-inverse)
      (setf (role-synonyms-internal role-inverse)
            (mapcar #'role-inverse-internal (role-synonyms-internal role)))
      (setf (role-synonyms-internal removed-role-inverse) nil)) ; Marker for being removed.
    (setf (role-synonyms-internal removed-role) nil)

    (cond ((and (null (role-cd-attribute role)) 
                (not (null (role-cd-attribute removed-role))))
           (setf (role-cd-attribute role) (role-cd-attribute removed-role)))
          ((and (null (role-cd-attribute removed-role)) 
                (not (null (role-cd-attribute role))))
           nil)
          (t (unless (equal (role-cd-attribute role) (role-cd-attribute removed-role))
               (error "Equality of concrete domain attributes ~a dn ~a ~
                                  cannot be enforced because the domain are disjoint - ~
                                  found ~a and ~a, respectively."
                      role removed-role (role-cd-attribute role)
                      (role-cd-attribute removed-role)))))
    (unless (role-cd-attribute role)
      (unless (is-bottom-object-role-p role)
        (when (role-transitive-p removed-role)
          (setf (role-transitive-p role) t)
          (setf (role-transitive-p role-inverse) t))
        (when (role-feature-p removed-role)
          (setf (role-feature-p role) t)
          (setf (role-inverse-feature-p role-inverse) t))
        (when (role-reflexive-p removed-role)
          (setf (role-reflexive-p role) t)
          (setf (role-reflexive-p role-inverse) t))
        (when (role-inverse-feature-p removed-role)
          (setf (role-inverse-feature-p role) t)
          (setf (role-feature-p role-inverse) t))
        (when (role-annotation-p removed-role)
          (setf (role-annotation-p role) t))
        (when (role-irreflexive-p removed-role)
          (setf (role-irreflexive-p role) t)
          (setf (role-irreflexive-p role-inverse) t))
        (when (role-symmetric-p removed-role)
          (setf (role-symmetric-p role) t)
          (setf (role-symmetric-p role-inverse) t))
        (when (role-asymmetric-p removed-role)
          (setf (role-asymmetric-p role) t)
          (setf (role-asymmetric-p role-inverse) t))
        (unless (role-simple-p removed-role)
          (setf (role-simple-p role) nil)
          (setf (role-simple-p role-inverse) nil))
        (let ((removed-compositions (role-compositions removed-role)))
          (when removed-compositions
            (setf (role-compositions role)
                  (union (role-compositions role) removed-compositions :test 'equal))
            (setf (role-compositions role-inverse)
                  (loop for composition in (role-compositions role)
                        collect
                        (nreverse (mapcar #'role-inverse-internal composition)))))))
      (unless (is-bottom-datatype-role-p role)
        (let ((datatype (role-datatype role))
              (removed-datatype (role-datatype removed-role)))
          (when removed-datatype
            (if (and datatype (not (eq datatype t)))
                (unless (or (eq removed-datatype t) (equal removed-datatype datatype))
                  (error "cannot intersect the datatypes ~A and ~A of datatype properties ~A and ~A"
                         datatype removed-datatype (role-name role) (role-name removed-role)))
              (setf (role-datatype role) removed-datatype)))))
                 
      #+:debug
      (when *debug-r*
        (print (role-parents-internal role))
        (print (role-children-internal role))
        (print (role-inverse-internal role))
        (print (role-inverse-internal (role-inverse-internal removed-role)))))))

(defun remove-role-synonyms (tbox encoded-role-list)
  ;; This function deals with role synonyms.
  ;; It modifies the set tbox-encoded-role of the tbox.
  ;; References to parents, children, and inverse are now assumed to be correctly
  ;; established as structure instances (no longer names).
  
  (loop for role in encoded-role-list 
        do (setf (role-synonyms-internal role) (list role)))
  ;; If a role is removed, we set its synonyms to nil!!!
  
  (let (;(removed-roles nil)
        ;(counter 0)
        (role-synonyms (tbox-role-synonyms tbox)))    

    (labels ((roles-are-synonyms (role1 role2)
               (when *debug-r*
                 (print (list (role-name role1) (role-name role2)))
                 ;;(break "role synonyms")
                 (when (eq role1 role2)
                   (error "eq roles are declared as synonyms.")))
               (push (cons (role-name role1) (role-name role2)) role-synonyms))
             )
      
      (loop ;; repeat until no more role synoyms are revealed.
        do
        (progn
          ;; First, collapse roles with its synonyms
          #+:debug
          (when *debug-r*
            (print "Collapse synonymous roles"))
          (loop for (role-name-1 . role-name-2) in role-synonyms do
                (let ((role1 (get-tbox-role tbox role-name-1))
                      (role2 (get-tbox-role tbox role-name-2)))
                  (when (or (and (role-datatype role1) (not (role-datatype role2)))
                            (and (role-datatype role2) (not (role-datatype role1))))
                    (error "properties ~A and ~A cannot be equivalents ~
                            because one is a datatype and the other one an object property"
                           role-name-1 role-name-2))

                  ;; It may happen that two roles are collapsed after they are put
                  ;; on the list role-synonyms. Thus, we might find two roles
                  ;; with different names that turn out to be eq here.
                  ;; In this case, they are not collapsed.
                  (unless (eq role1 role2)
                    (if (role-internal-name-p role1)
                      (collapse-roles role2 role1 tbox)
                      (collapse-roles role1 role2 tbox)))))
          (setf role-synonyms nil)
          
          ;; Check for cycles in the role hierarchy.
          #+:debug
          (when *debug-r*
            (print "Check for cycles."))
          (loop for role in encoded-role-list 
                unless (or (role-cd-attribute role)
                           (role-removed-p role))
                do
                (let ((path (find-cycle role)))
                  ;;(when (and path *debug-r*)
                  ;;  (break "Cycle found: ~A" path))                
                  (loop for (role1 role2) on path
                        unless (null role2) do ;;(break "1")
                        (roles-are-synonyms role1 role2)))
                until (not (null role-synonyms)))
          
          (when (null role-synonyms)
            #+:debug
            (when *debug-r*
              (print "Check for implied synonyms due to inverse->inverse"))
            ;; Check for implied synonyms due to inverse->inverse links
            ;; The inverse of the inverse must be the same role.
            (loop for role in encoded-role-list 
                  unless (or (role-cd-attribute role)
                             (role-removed-p role))
                  do (let ((inverse-inverse-role (role-inverse-internal (role-inverse-internal role))))
                       (unless (eq role inverse-inverse-role)
                         (roles-are-synonyms role inverse-inverse-role)))
                  until (not (null role-synonyms))))
          #|
          (when (null role-synonyms)
            #+:debug
            (when *debug-r*
              (print "Check for implied synonyms due to symmetry."))
            ;; Check for implied synonyms due to symmetry
            ;; If the inverse of a role is the role itself, this must be propagated downwards
            (loop for role in encoded-role-list 
                  unless (or (role-cd-attribute role)
                             (role-removed-p role))
                  when (eq role (role-inverse-internal role))
                  do 
                  (loop for child in (role-children-internal role)
                        unless (eq child (role-inverse-internal child))
                        do (roles-are-synonyms child (role-inverse-internal child)))
                  until (not (null role-synonyms))))|#
          
          #|
	  The following idea is buggy. rm -- 23.4.08
	  (when (null role-synonyms)
            #+:debug
            (when *debug-r*
              (print "Check for implied synonyms due to a role and its inverse having the same parent."))
            ;; Check for a role and its inverse having the same parent
            ;; If this is the case, the parent and its inverse must be equivalent
            ;; (the parent is symmetric).
            (loop for role in encoded-role-list 
                  unless (or (role-removed-p role)
                             (role-cd-attribute role)
                             (eq role (role-inverse-internal role))) 
                  do 
                  (loop for parent in (role-parents-internal role) 
                        do
                        (loop for inv-parent in (role-parents-internal
                                                 (role-inverse-internal role)) 
                              when (eq inv-parent parent)
                              do
                              (unless (eq parent (role-inverse-internal parent))
                                ;;(break "3")
                                (roles-are-synonyms parent (role-inverse-internal parent)))))
                  until (not (null role-synonyms))))|#
          )
        until (null role-synonyms)))
    
    ;; encoded-role-list might be not equal to (tbox-encoded-role-list tbox)
    (setf (tbox-encoded-role-list tbox)
          (loop for role in (tbox-encoded-role-list tbox) 
                unless (role-removed-p role)
                collect role))
    nil))

(defun find-cycle (role)
  (let ((cycle-p (find-cycle-1 role role (incf *role-set-mark*))))
    (when cycle-p
      (compute-cycle role (list role)))))

(defun find-cycle-1 (start-role role mark)
  (if (eql (role-flag role) mark)
      (when (eq start-role role)
        mark)
    (progn
      (setf (role-flag role) mark)
      (loop for parent in (role-parents-internal role)
            for found-mark = (find-cycle-1 start-role parent mark)
            when (eql mark found-mark)
            do (return mark)))))

(defun compute-cycle (role path)
  (loop for parent in (role-parents-internal (first path)) do
        (cond ((eq parent role)
               (return path))
              ((member parent path)
               ;; We are on a cycle but not one containing the initial role.
               nil)
              (t
               (let ((result (compute-cycle role (cons parent path))))
                 (unless (null result)
                   (return result)))))))

(defun ensure-only-direct-parents-and-children-are-set-up-correctly (role)
  (let ((ancestors-of-parents
	 (mapcan #'(lambda (role) 
		     (unless (role-internal-name-p role)
		       (copy-list (remove role (role-ancestors-internal role)))))
		 (role-parents-internal role))))
    (setf (role-parents-internal role)
          (loop for parent in (role-parents-internal role)
                when (and ;(not (role-internal-name-p parent)) 
                          (not (find parent ancestors-of-parents)))
                collect parent)))
  (let ((descendants-of-children
	 (mapcan #'(lambda (role) 
		     (unless (role-internal-name-p role)
                       (if (listp (role-descendants-internal role))
                           (copy-list (remove role (role-descendants-internal role)))
                         (loop for key being the hash-key of (role-descendants-internal role)
                               unless (eq key role)
                               collect key))))
		 (role-children-internal role))))
    (setf (role-children-internal role)
          (loop for child in (role-children-internal role)
                when (and ;(not (role-internal-name-p child))
                          (not (find child descendants-of-children)))
                collect child))))

;;; =================================================================================

(defun roles-equivalent-1 (role1 role2 tbox)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (check-type role1 symbol)
  (check-type role2 symbol)
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (unless (eq role1 role2) 
    (ensure-role role1 tbox)
    (ensure-role role2 tbox)
    (push (cons role1 role2) (tbox-role-synonyms tbox)))
  (values))

(defmacro roles-equivalent (role1 role2 &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(roles-equivalent-1 ',role1 ',role2 ',tbox)
    `(roles-equivalent-1 ',role1 ',role2 *current-tbox*)))

(defun roles-disjoint-1 (role1 role2 tbox)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (if (consp role1)
      (ensure-role (second role1) tbox)
    (ensure-role role1 tbox))
  (if (consp role2)
      (ensure-role (second role2) tbox)
    (ensure-role role2 tbox))
  (push (cons role1 role2) (tbox-role-antonyms tbox))
  (values))

(defmacro roles-disjoint (role1 role2 &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(roles-disjoint-1 ',role1 ',role2 ',tbox)
    `(roles-disjoint-1 ',role1 ',role2 *current-tbox*)))





(race-inline (get-role-domain-concept get-role-range-concept))

(defun get-role-domain-concept (role)
  (or (role-domain-concept role)
      (and (role-inverse-internal role)
           (role-range-concept (role-inverse-internal role)))))

(defun get-role-range-concept (role)
  (or (role-range-concept role)
      (and (role-inverse-internal role)
           (role-domain-concept (role-inverse-internal role)))))

(defun encode-roles (tbox encoded-role-list lists-only-p)
  (loop for role in encoded-role-list
        with blocking-possibly-required = nil
        with hierarchy-p = nil
        with complex-roles-p = nil
        with top = (tbox-object-top-role tbox)
        with bottom = (tbox-object-bottom-role tbox)
        with datatype-top = (tbox-datatype-top-role tbox)
        with datatype-bottom = (tbox-datatype-bottom-role tbox)
        do
        (unless (role-cd-attribute role)
          (unless (or (member role (role-ancestors-internal role))
                      (eq role top)
                      (eq role bottom)
                      (eq role datatype-top)
                      (eq role datatype-bottom))
            (encode-role tbox role lists-only-p))
          (when (and (not blocking-possibly-required)
                     (or (user-defined-role-transitive-p role)
                         (role-range-concept role)
                         ))
            (setf blocking-possibly-required t)))
	(when (and (not hierarchy-p) (role-has-ancestors-p role))
	  (setf hierarchy-p t))
	(when (and (not complex-roles-p) (role-compositions role))
	  (setf complex-roles-p t))
        finally
        (when blocking-possibly-required
          (setf (tbox-blocking-possibly-required tbox) t))
	(when hierarchy-p
	  (setf (tbox-language tbox) (add-dl-simple-role-inclusions (tbox-language tbox))))
	(when complex-roles-p
	  (setf (tbox-language tbox) (add-dl-complex-role-inclusions (tbox-language tbox)))))
  (loop for role in encoded-role-list
        with top = (tbox-object-top-role tbox)
        with bottom = (tbox-object-bottom-role tbox)
        with datatype-top = (tbox-datatype-top-role tbox)
        with datatype-bottom = (tbox-datatype-bottom-role tbox)
        do
        (unless (or (eq role top) (eq role bottom)
                    (eq role datatype-top) (eq role datatype-bottom))
          (ensure-only-direct-parents-and-children-are-set-up-correctly role))))

#+:debug
(defun encode-domain-range-of-roles (tbox)
  (let ((all-roles
         (sort-role-list
          (role-set-remove-duplicates
           (loop for role being the hash-values of (tbox-role-store tbox)
                 collect role)))))
    (loop with mark = (incf *role-set-mark*)
          for role in all-roles
          when (and (not (role-cd-attribute role))
                    (not (eql (racer-set-flag role) mark)))
          do
          (compute-inverse-role-domain-range role mark))
    (loop with top = (tbox-top-node tbox)
          for role in all-roles do
          (compute-role-domain-range-1 role tbox top))
    (loop for role in all-roles do
          (compute-role-domain-range-2 role))
    (loop with language = (tbox-language tbox)
          for role in all-roles do
          (when (role-domain-restriction role)
            (setf language (union-dl-descriptors language
                                                 (concept-language (role-domain-restriction role)))))
          (when (role-range-restriction role)
            (setf language (union-dl-descriptors language
                                                 (concept-language (role-range-restriction role)))))
          finally
          (setf (tbox-language tbox) (union-dl-descriptors language (tbox-language tbox))))
    (loop for role in all-roles
          with blocking-possibly-required = (tbox-blocking-possibly-required tbox)
          while (not blocking-possibly-required)
          do
          (when (and (not (role-cd-attribute role))
                     (or (user-defined-role-transitive-p role)
                         (role-range-concept role)
                         (role-domain-concept role)))
            (setf blocking-possibly-required t))
          finally
          (when blocking-possibly-required
            (setf (tbox-blocking-possibly-required tbox) t)))))

#-:debug
(defun encode-domain-range-of-roles (tbox)
  (let ((role-store (tbox-role-store tbox)))
    (loop with mark = (incf *role-set-mark*)
          for role being the hash-values of role-store
          when (and (not (role-cd-attribute role))
                    (not (eql (racer-set-flag role) mark)))
          do
          (compute-inverse-role-domain-range role mark))
    (loop with mark = (incf *role-set-mark*)
          with top  = (tbox-top-node tbox)
          for role being the hash-values of role-store
          unless (eql (racer-set-flag role) mark)
          do
          (compute-role-domain-range-1 role tbox top)
          (setf (racer-set-flag role) mark))
    (loop with mark = (incf *role-set-mark*)
          for role being the hash-values of role-store
          unless (eql (racer-set-flag role) mark)
          do
          (compute-role-domain-range-2 role)
          (setf (racer-set-flag role) mark))
    (loop with language = (tbox-language tbox)
          for role being the hash-values of role-store do
          (when (role-domain-restriction role)
            (setf language (union-dl-descriptors language
                                                 (concept-language (role-domain-restriction role)))))
          (when (role-range-restriction role)
            (setf language (union-dl-descriptors language
                                                 (concept-language (role-range-restriction role)))))
          finally
          (setf (tbox-language tbox) (union-dl-descriptors language (tbox-language tbox))))
    (loop for role being the hash-values of role-store
          with blocking-possibly-required = (tbox-blocking-possibly-required tbox)
          while (not blocking-possibly-required)
          do
          (when (and (not (role-cd-attribute role))
                     (or (user-defined-role-transitive-p role)
                         (role-range-concept role)
                         (role-domain-concept role)))
            (setf blocking-possibly-required t))
          finally
          (when blocking-possibly-required
            (setf (tbox-blocking-possibly-required tbox) t)))))

(defun compute-inverse-role-domain-range (role mark)
  (let* ((inverse (role-inverse-internal role)))
    (when inverse
      (let ((domain (role-domain-concept role))
            (range (role-range-concept role))
            (inv-domain (role-domain-concept inverse))
            (inv-range (role-range-concept inverse)))
        (when (or domain inv-range)
          (unless (eq domain inv-range)
            (let ((concept `(and ,@(remove nil (list domain inv-range)))))
              (setf (role-domain-concept role) concept)
              (setf (role-range-concept inverse) concept))))
        (when (or range inv-domain)
          (unless (eq range inv-domain)
            (let ((concept `(and ,@(remove nil (list range inv-domain)))))
              (setf (role-range-concept role) concept)
              (setf (role-domain-concept inverse) concept))))
        (setf (racer-set-flag inverse) mark))))
  (setf (racer-set-flag role) mark))

(defun compute-role-domain-range-1 (role tbox top)
  (unless (listp (role-descendants-internal role))
    (setf (role-descendants-internal role)
          (#+:debug sort-role-list
                    #-:debug progn
                    (loop for key being the hash-key of (role-descendants-internal role)
                          collect key))))
  (when (role-cd-attribute role)
    (when (rest (role-ancestors-internal role))
      (error "role ~A in TBox ~A cannot be used as a concrete domain attribute and an abstract role"
             (role-name role) (tbox-name tbox)))
    (setf (role-ancestors-internal role) (list role)))
  (loop for ancestor in (role-ancestors-internal role)
        for domain = (get-role-domain-concept ancestor)
        for range = (get-role-range-concept ancestor)
        when domain
        collect domain into domain-concepts
        when range
        collect range into range-concepts
        finally
        (when (or domain-concepts range-concepts)
          (with-ignored-role-domain-range-encodings
            (let ((domain-concept (and domain-concepts
                                       (encode-concept-term 
                                        `(and .,domain-concepts))))
                  (range-concept (and range-concepts
                                      (encode-concept-term 
                                       `(and .,range-concepts)))))
              (when (and domain-concept (not (eq domain-concept top)))
                (setf (role-domain-restriction role) domain-concept))
              (when (and range-concept (not (eq range-concept top)))
                (setf (role-range-restriction role) range-concept)))))))

(defun compute-role-domain-range-2 (role)
  (let ((domain (role-domain-restriction role))
        (range (role-range-restriction role)))
    (when (and domain (exists-concept-p domain))
      (when (and (role-domain-restriction (concept-role domain))
                 (null (concept-role-domain domain)))
        (setf (concept-role-domain domain)
              (role-domain-restriction (concept-role domain))))
      (when (and (role-range-restriction (concept-role domain))
                 (null (concept-role-range domain)))
        (setf (concept-role-range domain)
              (role-range-restriction (concept-role domain)))))
    (when (and range (exists-concept-p range))
      (when (and (role-domain-restriction (concept-role range))
                 (null (concept-role-domain range)))
        (setf (concept-role-domain range)
              (role-domain-restriction (concept-role range))))
      (when (and (role-range-restriction (concept-role range))
                 (null (concept-role-range range)))
        (setf (concept-role-range range)
              (role-range-restriction (concept-role range)))))))

(defun encode-role (tbox role &optional (only-lists nil))
  (let ((ancestors (compute-ancestors role)))
    (when (member role ancestors)
      (error "While considering role ~S: ~
              Cycle detected in role axioms."
             (role-name role)))
    (setf (role-has-ancestors-p role) (consp ancestors))
    (pushnew role ancestors)
    (setf (role-ancestors-internal role) ancestors)
    (loop for ancestor in ancestors
          with top = (tbox-object-top-role tbox)
          with datatype-top = (tbox-datatype-top-role tbox)
          do
          (unless (or (eq ancestor top) (eq ancestor datatype-top))
            (add-to-role-descendants ancestor role only-lists)))
    (when (and (role-transitive-p role) 
               (role-feature-p role))
      (unless (role-internal-conjunction-p role)
        (racer-warn "While considering ~S: ~
                       Features cannot be transitive - Ignoring transitivity declaration."
                    role))
      (setf (role-transitive-p role) nil))
    (when (some #'role-feature-p ancestors)
      (setf (tbox-language tbox) (add-dl-features (tbox-language tbox)))
      (unless (role-feature-p role)
        (unless (role-internal-conjunction-p role)
          (racer-warn "While considering ~S: ~
                         Role contains a feature as a super role and is converted into a feature."
                      role))
        (setf (role-feature-p role) t))
      (when (role-transitive-p role)
        (unless (role-internal-conjunction-p role)
          (racer-warn "While considering ~S: ~
                         Role contains a feature as a super role but is declared to be transitive - ~
                         Ignoring transitivity declaration."
                      role))
        (setf (role-transitive-p role) nil)))
    (if (role-datatype role)
        (progn
          (loop with bottom = (tbox-bottom-node tbox)
                with datatype-bottom = (tbox-datatype-bottom-node tbox)
                for ancestor in ancestors
                for datatype = (role-datatype role)
                for ancestor-datatype = (role-datatype ancestor)
                do
                (when (and (not (is-top-datatype-role-p ancestor))
                           (not (is-top-object-role-p ancestor))
                           (not ancestor-datatype))
                  (racer-warn "While considering ~S (~S): ~
                     Role contains a non-datatype role ~S as a super role and is declared as unsatisfiable."
                              role datatype ancestor)
                ;(setf (role-datatype ancestor) datatype)
                  (setf (role-domain-concept role) +bottom-symbol+)
                  (setf (role-domain-restriction role) bottom)
                  (setf (role-range-concept role) +datatype-bottom-symbol+)
                  (setf (role-range-restriction role) datatype-bottom)
                  )
                (if (or (eq datatype t) (eq ancestor-datatype t))
                    (unless (eq ancestor-datatype t)
                      (setf (role-datatype role) ancestor-datatype))
                  (unless (datatype-compatible-p datatype ancestor-datatype nil)
                    (racer-warn "The range ~S of datatype role ~S is incompatible with the range ~S of super datatype role ~S. Role ~S is unsatisfiable." datatype role ancestor-datatype ancestor role)
                    (setf (role-domain-concept role) +bottom-symbol+)
                    (setf (role-domain-restriction role) bottom)
                    (setf (role-range-concept role) +datatype-bottom-symbol+)
                    (setf (role-range-restriction role) datatype-bottom))))
          (let ((inverse (role-inverse-internal role)))
            (when (role-datatype inverse)
              (error "Role ~S and its inverse ~S are declared as datatype roles ~
                    (this is OWL Full, which is not supported)."
                     role inverse))))
      (let ((datatype-ancestor (loop for ancestor in ancestors
                                     when (role-datatype ancestor)
                                     do (return ancestor))))
        (when (and datatype-ancestor (not (is-bottom-object-role-p role)))
          (racer-warn "While considering ~S: ~
                 Role contains the datatype role ~S (~S) as a super role and is declared as unsatisfiable."
                      role datatype-ancestor (role-datatype datatype-ancestor))
          ;(setf (role-datatype role) (role-datatype datatype-ancestor))
          (setf (role-domain-concept role) +bottom-symbol+)
          (setf (role-domain-restriction role) (tbox-bottom-node tbox))
          (setf (role-range-concept role) +bottom-symbol+)
          (setf (role-range-restriction role) (tbox-bottom-node tbox))
          )))
    (when (role-feature-p role)
      (let* ((true-ancestors (remove role (role-ancestors-internal role)))
             (feature-ancestors (delete-if-not #'role-feature-p true-ancestors)))
        (setf (role-has-feature-ancestors-p role) (consp feature-ancestors))
        (setf (role-feature-ancestors role) (cons role feature-ancestors))))
    role))

(defun add-to-role-descendants (ancestor descendant only-lists)
  (let ((descendants (role-descendants-internal ancestor)))
    (if (or (and (listp descendants) (< (length descendants) 50))
            (and only-lists (not (hash-table-p descendants))))
      (pushnew descendant (role-descendants-internal ancestor))
      (progn
        (when (listp descendants)
          (setf descendants
                #+:allegro (racer-make-hash-table :size 100 :rehash-size 2.0 :structure-p t)
                #-:allegro (racer-make-hash-table :size 100))
          (loop for elem in (role-descendants-internal ancestor) do
                (setf (gethash elem descendants) t))
          (setf (role-descendants-internal ancestor) descendants))
        (unless (gethash descendant descendants)
          (setf (gethash descendant descendants) t))))))

(defun compute-ancestors (role &optional 
                                  (visited
                                   #+:allegro (racer-make-hash-table :size 100 :rehash-size 2.0 :structure-p t)
                                   #-:allegro (racer-make-hash-table :size 100)))
  ;;(break "~S" role)
  (or (gethash role visited)
      (progn
        (setf (gethash role visited) (list role))
        (loop with parents = (role-parents-internal role)
              with ancestors = parents
              for parent in parents do
              (setf ancestors (union (or (role-ancestors-internal parent)
                                         (compute-ancestors parent visited))
                                     ancestors))
              finally
              (setf (role-ancestors-internal role) ancestors) ;temporarily used as a cache
              (return ancestors)))))

(defun filter-visible-roles (roles)
  (loop for role in roles 
        unless (or (role-internal-name-p role) (role-internal-conjunction-p role))
        collect role))

(defun atomic-role-parents-synsets (role-term tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (if (consp role-term)
      (transform-into-role-synsets (filter-visible-roles 
                                    (role-parents-internal 
                                     (role-inverse-internal (get-tbox-role tbox (second role-term)))))
                                   tbox)
    (transform-into-role-synsets (filter-visible-roles 
                                  (role-parents-internal (get-tbox-role tbox role-term)))
                                 tbox)))

(defun transform-into-role-synsets (roles kb)
  (when (member +owl-bottom-object-role+ roles)
    (setf roles (remove (get-object-bottom-role kb) roles)))
  (when (member +owl-bottom-data-role+ roles)
    (setf roles (remove (get-datatype-bottom-role kb) roles)))
  (mapcar #'(lambda (role) 
              (cond ((eq role +owl-bottom-object-role+)
                     (if (get-object-bottom-role kb)
                         (cons +owl-bottom-object-role+ (atomic-role-synonyms (decode-role role) kb))
                       (list +owl-bottom-object-role+)))
                    ((eq role +owl-bottom-data-role+)
                     (if (get-datatype-bottom-role kb)
                         (cons +owl-bottom-data-role+ (atomic-role-synonyms (decode-role role) kb))
                       (list +owl-bottom-data-role+)))
                    ((eq role +owl-top-object-role+)
                     (list +owl-top-object-role+))
                    ((eq role +owl-top-data-role+)
                     (list +owl-top-data-role+))
                    (t (mapcar #'decode-role (atomic-role-synonyms (decode-role role) kb)))))
          roles))


(defun atomic-role-parents (role-term tbox &key synsets-p)
  (cond (synsets-p
         (atomic-role-parents-synsets role-term tbox))
        (t 
         (setf tbox (find-tbox tbox))
         (check-type tbox tbox)
         (check-role-term role-term)
         (ensure-knowledge-base-state ':tbox-prepared tbox)
         (ensure-role-is-known role-term tbox)
         (if (consp role-term)
             (mapcar #'decode-role (filter-visible-roles 
                                    (role-parents-internal (role-inverse-internal (get-tbox-role tbox (second role-term))))))
           (mapcar #'decode-role (filter-visible-roles 
                                  (role-parents-internal (get-tbox-role tbox role-term))))))))




(defun compute-parents (role ancestors tbox)
  (let* ((ancestors (remove role ancestors :test #'equal))
         (all nil))
    (dolist (anc ancestors)
      (dolist (anc2 (atomic-role-ancestors2 anc tbox))
        (unless (equal anc2 anc)
          (push anc2 all))))

    (setf all (delete-duplicates all))
    
    (let ((syns (atomic-role-synonyms role tbox)))
      (set-difference 
       (set-difference ancestors all :test #'equal)
       syns
       :test #'equal))))

(defun atomic-role-ancestors2 (role-term tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (if (consp role-term)
      (mapcar #'decode-role (role-ancestors-internal
                             (role-inverse-internal (get-tbox-role tbox (second role-term)))))
    (mapcar #'decode-role (role-ancestors-internal
                           (get-tbox-role tbox role-term)))))


(defun atomic-role-parents2 (role-term tbox)
  (let ((ancestors (atomic-role-ancestors2 role-term tbox)))
    (compute-parents role-term ancestors tbox)))


(defmacro role-parents (role-term &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(atomic-role-parents ',role-term ',tbox)
    `(atomic-role-parents ',role-term *current-tbox*)))

(defun atomic-role-synonyms (role-term tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (if (consp role-term)
    (mapcar #'decode-role (filter-visible-roles 
                           (role-synonyms-internal (role-inverse-internal (get-tbox-role tbox (second role-term))))))
    (mapcar #'decode-role (filter-visible-roles 
                           (role-synonyms-internal (get-tbox-role tbox role-term))))))

(defmacro role-synonyms (role-term &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(atomic-role-synonyms ',role-term ',tbox)
    `(atomic-role-synonyms ',role-term *current-tbox*)))

(defun atomic-role-children-synsets (role-term tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (if (consp role-term)
      (transform-into-role-synsets (filter-visible-roles 
                                    (role-children-internal 
                                     (role-inverse-internal (get-tbox-role tbox (second role-term)))))
                                   tbox)
    (transform-into-role-synsets (filter-visible-roles 
                                  (role-children-internal (get-tbox-role tbox role-term)))
                                 tbox)))

(defun atomic-role-children (role-term tbox &key synsets-p)
  (cond (synsets-p
         (atomic-role-children-synsets role-term tbox))
        (t 
         (setf tbox (find-tbox tbox))
         (check-type tbox tbox)
         (check-role-term role-term)
         (ensure-knowledge-base-state ':tbox-prepared tbox)
         (ensure-role-is-known role-term tbox)
         (if (consp role-term)
             (mapcar #'decode-role (filter-visible-roles 
                                    (role-children-internal (role-inverse-internal (get-tbox-role tbox (second role-term))))))
           (mapcar #'decode-role (filter-visible-roles 
                                  (role-children-internal (get-tbox-role tbox role-term))))))))


(defun compute-children (role descendants tbox)
  (let* ((descendants (remove role descendants :test #'equal))
         (all nil))
    (dolist (anc descendants)
      (dolist (anc2 (atomic-role-descendants2 anc tbox))
        (unless (equal anc2 anc)
          (push anc2 all))))

    (setf all (delete-duplicates all))
    
    (let ((syns (atomic-role-synonyms role tbox)))
      (set-difference 
       (set-difference descendants all :test #'equal)
       syns
       :test #'equal))))

(defun atomic-role-descendants2 (role-term tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (if (consp role-term)
      (mapcar #'decode-role (role-descendants-internal
                             (role-inverse-internal (get-tbox-role tbox (second role-term)))))
    (mapcar #'decode-role (role-descendants-internal
                           (get-tbox-role tbox role-term)))))


(defun atomic-role-children2 (role-term tbox)
  (let ((descendants (atomic-role-descendants2 role-term tbox)))
    (compute-children role-term descendants tbox)))


(defmacro role-children (role-term &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(atomic-role-children ',role-term ',tbox)
    `(atomic-role-children ',role-term *current-tbox*)))

(defmacro role-offspring (role-term &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(atomic-role-children ',role-term ',tbox)
    `(atomic-role-children ',role-term *current-tbox*)))

(defun atomic-role-ancestors (role-term tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (if (consp role-term)
    (mapcar #'decode-role
            (filter-visible-roles 
             (role-ancestors-internal (role-inverse-internal (get-tbox-role tbox (second role-term))))))
    (mapcar #'decode-role (filter-visible-roles 
                           (role-ancestors-internal (get-tbox-role tbox role-term))))))

(defun atomic-role-ancestors-internal (role-term tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (if (consp role-term)
    (mapcar #'decode-role
            (role-ancestors-internal (role-inverse-internal (get-tbox-role tbox (second role-term)))))
    (mapcar #'decode-role (role-ancestors-internal (get-tbox-role tbox role-term)))))

(defmacro role-ancestors (role-term &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(atomic-role-ancestors ',role-term ',tbox)
    `(atomic-role-ancestors ',role-term *current-tbox*)))

(defun atomic-role-descendants (role-term tbox) 
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (if (consp role-term)
    (mapcar #'decode-role
            (filter-visible-roles 
             (role-descendants-internal (role-inverse-internal (get-tbox-role tbox (second role-term))))))
    (mapcar #'decode-role 
            (filter-visible-roles 
             (role-descendants-internal (get-tbox-role tbox role-term))))))

(defun atomic-role-descendants-internal (role-term tbox) 
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (if (consp role-term)
    (mapcar #'decode-role
            (role-descendants-internal (role-inverse-internal (get-tbox-role tbox (second role-term)))))
    (mapcar #'decode-role 
            (role-descendants-internal (get-tbox-role tbox role-term)))))


(defmacro role-descendants (role-term &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(atomic-role-descendants ',role-term ',tbox)
    `(atomic-role-descendants ',role-term *current-tbox*)))
  
(defun atomic-role-inverse (role-term &optional (tbox *current-tbox*))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (if (consp role-term)
    (decode-role (get-tbox-role tbox (second role-term)))
    (decode-role (role-inverse-internal (get-tbox-role tbox role-term)))))

(defmacro role-inverse (role-term &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(atomic-role-inverse ',role-term ',tbox)
    `(atomic-role-inverse ',role-term *current-tbox*)))

(defun atomic-role-domain (role-term tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (let ((concept (if (consp role-term)
                   (role-domain-restriction (role-inverse-internal 
                                             (get-tbox-role tbox (second role-term))))
                   (role-domain-restriction (get-tbox-role tbox role-term)))))
    (if (null concept)
      'top
      (decode-concept concept))))

(defmacro role-domain (role-term &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(atomic-role-domain ',role-term ',tbox)
    `(atomic-role-domain ',role-term *current-tbox*)))

(defun atomic-role-range (role-term tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (let* ((role (if (consp role-term)
                   (role-inverse-internal (get-tbox-role tbox (second role-term)))
                 (get-tbox-role tbox role-term)))
         (concept (role-range-restriction role)))
    (if (null concept)
        (if (is-top-datatype-role-p role)
            +datatype-top-symbol+
          +krss-top-symbol+)
      (decode-concept concept))))

(defmacro role-range (role-term &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(atomic-role-range ',role-term ',tbox)
    `(atomic-role-range ',role-term *current-tbox*)))


(defmacro attribute-domain (attribute-name &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(atomic-role-domain ',attribute-name ',tbox)
    `(atomic-role-domain ',attribute-name *current-tbox*)))

(defun attribute-domain-1 (attribute-name &optional (tbox *current-tbox*))
  (atomic-role-domain attribute-name tbox))

(defun role-characteristics-checked-p (tbox role-name characteristics)
  (let ((role-characteristics-checked-p (tbox-role-characteristics-checked-p tbox)))
    (when role-characteristics-checked-p
      (gethash (cons role-name characteristics) role-characteristics-checked-p))))

(defun set-role-characteristics-checked-p (tbox role-name characteristics)
  (unless (tbox-role-characteristics-checked-p tbox)
    (setf (tbox-role-characteristics-checked-p tbox) (racer-make-hash-table :test 'equal)))
  (setf (gethash (cons role-name characteristics) (tbox-role-characteristics-checked-p tbox)) t))
  
(defun transitive-p (role-term &optional (tbox *current-tbox*))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (let* ((role (if (consp role-term)
                   (role-inverse-internal (get-tbox-role tbox (second role-term)))
                 (get-tbox-role tbox role-term)))
         (role-name (role-name role)))
    (if (role-characteristics-checked-p tbox role-name ':transitive)
        (role-transitive-p role)
      (let ((inverse (role-inverse-internal role)))
        (set-role-characteristics-checked-p tbox role-name ':transitive)
        (set-role-characteristics-checked-p tbox (role-name inverse) ':transitive)
        (or (role-transitive-p role)
            (when (test-role-characteristics tbox role-term ':transitive)
              (setf (role-transitive-p role) t)
              (setf (role-transitive-p inverse) t)))))))

(defmacro transitive? (role-term &optional (tbox-name nil tbox-name-specified-p))
  (if tbox-name-specified-p
    `(transitive-p ',role-term ',tbox-name)
    `(with-tbox-defined-check *current-tbox*
       (transitive-p ',role-term *current-tbox*))))

(defun feature-p (role-term &optional (tbox *current-tbox*))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (if (subset-el+-owl-p (tbox-language tbox))
      nil
    (let* ((role (if (consp role-term)
                     (role-inverse-internal (get-tbox-role tbox (second role-term)))
                   (get-tbox-role tbox role-term)))
           (role-name (role-name role)))
      (if (role-characteristics-checked-p tbox role-name ':feature)
          (role-feature-p role)
        (progn
          (set-role-characteristics-checked-p tbox role-name ':feature)
          (or (role-feature-p role)
              (when (test-role-characteristics tbox role-term ':feature)
                (setf (role-feature-p role) t))))))))

(defmacro feature? (role-term &optional (tbox-name nil tbox-name-specified-p))
  (if tbox-name-specified-p
    `(feature-p ',role-term ',tbox-name)
    `(with-tbox-defined-check *current-tbox*
       (feature-p ',role-term *current-tbox*))))

(defun inverse-feature-p (role-term &optional (tbox *current-tbox*))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (if (subset-el+-owl-p (tbox-language tbox))
      nil
    (let* ((new-role-term (if (consp role-term)
                              (second role-term)
                            `(inv ,role-term)))
           (role (get-tbox-role tbox new-role-term))
           (role-name (role-name role)))
      (if (role-characteristics-checked-p tbox role-name ':feature)
          (role-feature-p role)
        (progn
          (set-role-characteristics-checked-p tbox role-name ':feature)
          (or (role-feature-p role)
              (when (test-role-characteristics tbox new-role-term ':feature)
                (setf (role-feature-p role) t))))))))

(defmacro inverse-feature? (role-term &optional (tbox-name nil tbox-name-specified-p))
  (if tbox-name-specified-p
    `(inverse-feature-p ',role-term ',tbox-name)
    `(with-tbox-defined-check *current-tbox*
       (inverse-feature-p ',role-term *current-tbox*))))

(defun cd-attribute-p (attribute &optional (tbox *current-tbox*))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term attribute)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known attribute tbox)
  (when (role-cd-attribute (get-tbox-role tbox attribute))
    t))

(defmacro cd-attribute? (attribute &optional (tbox-name nil tbox-name-specified-p))
  (if tbox-name-specified-p
    `(cd-attribute-p ',attribute ',tbox-name)
    `(with-tbox-defined-check *current-tbox*
       (cd-attribute-p ',attribute *current-tbox*))))


(defun symmetric-p (role-term &optional (tbox *current-tbox*))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (let ((role (if (consp role-term)
                  (role-inverse-internal (get-tbox-role tbox (second role-term)))
                (get-tbox-role tbox role-term))))
    (cond ((role-symmetric-p role) t)
          ((role-asymmetric-p role) nil)
          ((eq role (role-inverse-internal role))
           (setf (role-symmetric-p role) t))
          (t nil))))

(defmacro symmetric? (role-term &optional (tbox-name nil tbox-name-specified-p))
  (if tbox-name-specified-p
    `(symmetric-p ',role-term ',tbox-name)
    `(with-tbox-defined-check *current-tbox*
       (symmetric-p ',role-term *current-tbox*))))


(defun asymmetric-p (role-term &optional (tbox *current-tbox*))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (let* ((role (if (consp role-term)
                   (role-inverse-internal (get-tbox-role tbox (second role-term)))
                 (get-tbox-role tbox role-term)))
         (role-name (role-name role)))
    (if (role-characteristics-checked-p tbox role-name ':asymmetric)
        (role-asymmetric-p role)
      (let* ((inverse (role-inverse-internal role))
             (inverse-name (role-name inverse)))
        (set-role-characteristics-checked-p tbox role-name ':asymmetric)
        (set-role-characteristics-checked-p tbox inverse-name ':asymmetric)
        (let ((result
               (cond ((or (role-datatype role) (role-cd-attribute role)) nil)
                     ((role-asymmetric-p role) t)
                     ((role-symmetric-p role) nil)
                     ((subset-el+-owl-p (tbox-language tbox)) nil)
                     ((test-role-characteristics tbox role-term ':asymmetric)
                      (setf (role-asymmetric-p role) t)
                      (setf (role-irreflexive-p role) t)
                      (setf (role-asymmetric-p inverse) t)
                      (setf (role-irreflexive-p inverse) t))
                     (t nil))))
          (when (and result (not (role-characteristics-checked-p tbox role-name ':irreflexive)))
            (set-role-characteristics-checked-p tbox role-name ':irreflexive)
            (set-role-characteristics-checked-p tbox inverse-name ':irreflexive))
          result)))))

(defmacro asymmetric? (role-term &optional (tbox-name nil tbox-name-specified-p))
  (if tbox-name-specified-p
    `(asymmetric-p ',role-term ',tbox-name)
    `(with-tbox-defined-check *current-tbox*
       (asymmetric-p ',role-term *current-tbox*))))


(defun reflexive-p (role-term &optional (tbox *current-tbox*))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (let* ((role (if (consp role-term)
                   (role-inverse-internal (get-tbox-role tbox (second role-term)))
                 (get-tbox-role tbox role-term)))
         (role-name (role-name role)))
    (if (role-characteristics-checked-p tbox role-name ':reflexive)
        (role-reflexive-p role)
      (let ((inverse (role-inverse-internal role)))
        (set-role-characteristics-checked-p tbox role-name ':reflexive)
        (set-role-characteristics-checked-p tbox (role-name inverse) ':reflexive)
        (cond ((or (role-datatype role) (role-cd-attribute role)) nil)
              ((role-reflexive-p role) t)
              ((or (role-irreflexive-p role) (role-asymmetric-p role)) nil)
              ((subset-el+-owl-p (tbox-language tbox)) nil)
              ((test-role-characteristics tbox role-term ':reflexive)
               (setf (role-reflexive-p role) t)
               (setf (role-reflexive-p inverse) t))
              (t nil))))))

(defmacro reflexive? (role-term &optional (tbox-name nil tbox-name-specified-p))
  (if tbox-name-specified-p
    `(reflexive-p ',role-term ',tbox-name)
    `(with-tbox-defined-check *current-tbox*
       (reflexive-p ',role-term *current-tbox*))))

(defun irreflexive-p (role-term &optional (tbox *current-tbox*))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term tbox)
  (let* ((role (if (consp role-term)
                   (role-inverse-internal (get-tbox-role tbox (second role-term)))
                 (get-tbox-role tbox role-term)))
         (role-name (role-name role)))
    (if (role-characteristics-checked-p tbox role-name ':irreflexive)
        (role-irreflexive-p role)
      (let* ((inverse (role-inverse-internal role))
             (inverse-name (role-name inverse)))
        (set-role-characteristics-checked-p tbox role-name ':irreflexive)
        (set-role-characteristics-checked-p tbox inverse-name ':irreflexive)
        (let ((result
               (cond ((or (role-datatype role) (role-cd-attribute role)) nil)
                     ((role-irreflexive-p role) t)
                     ((role-asymmetric-p role)
                      (setf (role-irreflexive-p role) t)
                      (setf (role-irreflexive-p inverse) t))
                     ((role-reflexive-p role) nil)
                     ((subset-el+-owl-p (tbox-language tbox)) nil)
                     ((test-role-characteristics tbox role-term ':irreflexive)
                      (setf (role-irreflexive-p role) t)
                      (setf (role-irreflexive-p inverse) t))
                     (t nil))))
          (unless (or result (role-characteristics-checked-p tbox role-name ':asymmetric))
            (set-role-characteristics-checked-p tbox role-name ':asymmetric)
            (set-role-characteristics-checked-p tbox inverse-name ':asymmetric))
          result)))))

(defmacro irreflexive? (role-term &optional (tbox-name nil tbox-name-specified-p))
  (if tbox-name-specified-p
    `(irreflexive-p ',role-term ',tbox-name)
    `(with-tbox-defined-check *current-tbox*
       (irreflexive-p ',role-term *current-tbox*))))

(defun all-atomic-concepts (&optional (tbox *current-tbox*)
                                          &key (count nil))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (possibly-count-only
   (let ((result nil))
     (loop for concept being the hash-values of (tbox-concept-store tbox)
           when (and (atomic-concept-p concept)
                     (concept-visible-p concept))
           do (loop for name in (concept-name-set concept)
                    unless (member name '(top bottom *top* *bottom*))
                    do (push name result)))
     (remove-duplicates (list* 'top 'bottom (nreverse result))))
   count))

(defun all-equivalent-concepts (&optional (tbox *current-tbox*)
                                              &key (count nil))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (ensure-knowledge-base-state ':tbox-classified tbox)
  (possibly-count-only
   (let ((tbox-top-node (tbox-top-node tbox))
         (tbox-bottom-node (tbox-bottom-node tbox)))
     (list* (concept-name-set tbox-top-node)
            (concept-name-set tbox-bottom-node)
            (loop for concept being the hash-values of (tbox-concept-store tbox)
                  when (and (atomic-concept-p concept)
                            (concept-visible-p concept)
                            (not (eq concept tbox-top-node))
                            (not (eq concept tbox-bottom-node)))
                  collect (concept-name-set concept))))
   count))


;;; ======================================================================

(defun role-is-used-as-annotation-property (rolename tbox)
  (check-type rolename symbol)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  
  (ensure-role rolename tbox)
  (let* ((index (tbox-role-axioms-index tbox))
         (entry (gethash rolename index)))
    (unless (role-info-annotation-p entry) ; We might have entered a dat
      (when (or (tbox-classified-p-internal tbox) 
		(tbox-coherence-checked-p tbox)
		(tbox-index-structures-complete-p tbox))
	(reset-tbox tbox))
      (setf (role-info-annotation-p entry) t))))

(defun role-is-used-as-datatype-property (rolename tbox)
  (check-type rolename symbol)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  
  (ensure-role rolename tbox)
  (let* ((index (tbox-role-axioms-index tbox))
         (entry (gethash rolename index)))
    (unless (role-info-datatype entry)	; We might have entered a dat
      (when (or (tbox-classified-p-internal tbox) 
		(tbox-coherence-checked-p tbox)
		(tbox-index-structures-complete-p tbox))
	(reset-tbox tbox))
      (setf (role-info-datatype entry) t))))

(defun datatype-role-range (role-name tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-type role-name symbol)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-name tbox)
  (let ((datatype (role-datatype (get-tbox-role tbox role-name)))) 
    (if (eq datatype 't)
      't ;;'string
      datatype)))


;;; ======================================================================

(defun role-used-as-annotation-property-p (role-name tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-type role-name symbol)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-name tbox)
  (and (role-annotation-p (get-tbox-role tbox role-name)) t))

(defun role-used-as-datatype-property-p (role-name tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-type role-name symbol)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-name tbox)
  (and (role-datatype (get-tbox-role tbox role-name)) t))

(defun datatype-role-has-range (rolename range tbox)
  (check-type rolename symbol)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (ensure-role rolename tbox nil range)
  #| (xml-add-implication 'top `(d-all ,rolename ,range) tbox))
     verlorengegangen Aenderung? tbox-t-9-150.lisp! 
  |# 
  (xml-add-implication 'top `(d-all ,rolename ,(if (symbolp range)
                                                  `(d-base-type ,range)
                                                 range))
                       tbox))


;;; ======================================================================


  
(defun concept-p (concept-name &optional (tbox *current-tbox*))
  (check-type concept-name symbol)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (and (get-tbox-concept tbox concept-name nil) 
       t))

(defmacro concept? (concept-name &optional (tbox-name nil tbox-name-specified-p))
  (if tbox-name-specified-p
    `(concept-p ',concept-name ',tbox-name)
    `(with-tbox-defined-check *current-tbox*
       (concept-p ',concept-name *current-tbox*))))

(defun describe-tbox (&optional (tbox *current-tbox*)
                                   (stream *standard-output*))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (let ((result `(signature
                  :atomic-concepts ,(if (tbox-signature tbox)
                                      (first (tbox-signature tbox))
                                      (loop for concept in (tbox-encoded-concept-list tbox)
                                            nconc (copy-list (concept-name-set concept))))
                  :roles ,(if (tbox-signature tbox)
                            (second (tbox-signature tbox))
                            (tbox-all-roles tbox))))) 
    (if (null stream)
      (format nil "~S" result)
      (pprint result stream))))

(defun describe-concept (concept-name 
                            &optional (tbox *current-tbox*)
                            (stream *standard-output*))
  (check-type concept-name symbol)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (let* ((synonyms (if (tbox-classified-p tbox)
                    (atomic-concept-synonyms concept-name tbox)
                    ':to-be-computed))
         (parents (if (tbox-classified-p tbox)
                    (atomic-concept-parents concept-name tbox)
                    ':to-be-computed))
         (children (if (tbox-classified-p tbox)
                    (atomic-concept-children concept-name tbox)
                    ':to-be-computed))
         (concept (internalize-concept tbox concept-name))
         (result `(,concept-name
                  ,(if (concept-primitive-p concept)
                     :told-primitive-definition
                     :told-definition)
                  ,(concept-definition concept)
                  :synonyms ,synonyms
                  :parents ,parents
                  :children ,children)))
    (if (null stream)
      (format nil "~S" result)
      (pprint result stream))))


(defun describe-role (role-term
                        &optional (tbox *current-tbox*)
                        (stream *standard-output*))
 (check-role-term role-term)
 (check-type tbox (or symbol tbox))
 (setf tbox (find-tbox tbox))
 (ensure-knowledge-base-state ':tbox-prepared tbox)
 (let ((result `(,role-term
		  :synonyms ,(atomic-role-synonyms role-term tbox)
		  :domain ,(atomic-role-domain role-term tbox)
		  :range ,(atomic-role-range role-term tbox)		  
                 :parents ,(atomic-role-parents role-term tbox)
                 :children ,(atomic-role-children role-term tbox)
                 :inverse ,(atomic-role-inverse role-term tbox)
                 :feature ,(feature-p role-term tbox)
                 :transitive-p ,(transitive-p role-term tbox))))
   (if (null stream)
     (format nil "~S" result)
     (pprint result stream))))


;;; =======================================================================

(defun mark-all-tbox-concepts (tbox mark)
  (loop for concept being the hash-value of (tbox-concept-store tbox)
        do (setf (concept-visited-mark concept) mark)))

(defun initialize-atomic-concept (tbox name definition primitive-p)
  #+:debug
  (if (symbolp name)
    (when (eq name +top-symbol+)
      (error "Concept ~A may not be re-initialized" +top-symbol+))
    (error "Symbol expected for concept name."))
  (let ((internal-marker-concept (tbox-internal-marker-concept name tbox)))
    (unless internal-marker-concept
      (unless (symbol-package name)
        (error "Racer cannot handle symbols without package: ~S"
               name)))
    (when (and (not internal-marker-concept) (tbox-signature tbox))
      (unless (find name (first (tbox-signature tbox)))
        (error "Concept name ~S not mentioned in signature of TBox ~S."
               name
               (tbox-name tbox))))
    (let* ((*tbox-node-currently-inserting* name)
           (concept (encode-concept-term name)))
      (setf (concept-name-set concept) (list name))
      (setf (concept-definition concept) definition)
      (setf (concept-primitive-p concept) primitive-p)
      concept)))


(defun internalize-concept (tbox concept-term)
  (if (symbolp concept-term)
    (get-tbox-concept tbox concept-term)
    (with-concept-definition-mapping tbox
      (with-alc-bindings
        (let ((encoded-concept (encode-concept-term concept-term)))
          (if (atomic-concept-p encoded-concept)
            encoded-concept
            (let ((name (gensym "%ANONYMOUS")))
              (create-tbox-internal-marker-concept tbox name)
              (let ((concept-1 (initialize-atomic-concept tbox
                                                          name
                                                          concept-term 
                                                          nil)))
                (encode-anonymous-concept tbox concept-1 concept-term encoded-concept)
                (insert-anonymous-concept-into-tbox concept-1 tbox t nil)
                ;; The anonymous concept might be a synonym for another concept.
                ;; Therefore, it is necessary to get the correct node object!!
                (let ((concept-2 (get-tbox-concept tbox name)))
                  (setf (tbox-empty-taxonomic-encoding-table tbox) nil)
                  (setf (gethash encoded-concept (tbox-taxonomic-encoding-table tbox)) concept-2)
                  concept-2)))))))))


(defun encode-anonymous-concept (tbox
                                 concept
                                 concept-term
                                 &optional (encoded-concept-term nil))
  (let ((encoded-definition 
         (encode-concept-term (or encoded-concept-term concept-term) concept))
        (meta-constraint-concept (tbox-meta-constraint-concept tbox)))
    (setf (concept-encoded-definition concept) encoded-definition)
    (setf (concept-told-subsumers concept)
          (concept-set-union (concept-told-subsumers concept)
                             (if meta-constraint-concept
                                 (concept-set-union (concept-told-subsumers encoded-definition)
                                                    (concept-told-subsumers meta-constraint-concept))
                               (concept-told-subsumers encoded-definition))))
    (setf (concept-told-disjoints concept)
          (concept-set-union (concept-told-disjoints concept)
                             (if meta-constraint-concept
                                 (concept-set-union (concept-told-disjoints encoded-definition)
                                                    (concept-told-disjoints meta-constraint-concept))
                               (concept-told-disjoints encoded-definition))))
    (set-language concept (incf *tbox-classification-counter*))
    (update-encoded-definition tbox 
                               concept
                               (tbox-top-node tbox)
                               (tbox-bottom-node tbox)
                               (incf *tbox-classification-counter*)
                               t
                               (incf *tbox-classification-counter*))))

(defun encode-update-concept-term (tbox concept-term)
  (if (concept-p-internal concept-term)
    (progn
      (set-language concept-term (incf *tbox-classification-counter*))
      concept-term)
    (progn
      (when (symbolp concept-term)
        (let ((concept (get-tbox-concept tbox concept-term nil)))
          (when concept
            (set-language concept (incf *tbox-classification-counter*))
            (return-from encode-update-concept-term concept))))
      (let ((concept (encode-concept-term concept-term)))
        (set-language concept (incf *tbox-classification-counter*))
        (update-encoded-definition tbox 
                                   concept
                                   (tbox-top-node tbox)
                                   (tbox-bottom-node tbox)
                                   (incf *tbox-classification-counter*)
                                   t
                                   (incf *tbox-classification-counter*))
        (set-language concept (incf *tbox-classification-counter*))
        concept))))

(defun add-to-told-disjoints (concept new-disjoints)
  (setf (concept-told-disjoints concept)
        (concept-set-union new-disjoints (concept-told-disjoints concept)))
  (when (boundp '*disjoints-table*)
    (let ((table (gethash concept *disjoints-table*)))
      (if table
        (loop for elem in new-disjoints do
              (unless (gethash elem table)
                (setf (gethash elem table) t)))
        (let ((disjoints (concept-told-disjoints concept)))
          (when (> (length disjoints) *initial-disjoints-table-size*)
            (setf table (racer-make-hash-table :size (max 100 (length disjoints)) :rehash-size 2.0
                                               :structure-p t))
            (loop for elem in disjoints do
                  (setf (gethash elem table) t))
            (loop for elem in new-disjoints do
                  (unless (gethash elem table)
                    (setf (gethash elem table) t)))
            (setf (gethash concept *disjoints-table*) table)))))))

(defun told-disjoints-member (concept disjoint)
  (if (boundp '*disjoints-table*)
    (let ((table (gethash disjoint *disjoints-table*))
          (disjoints (concept-told-disjoints disjoint)))
      (when (and (null table) (> (length disjoints) *initial-disjoints-table-size*))
        (setf table (racer-make-hash-table :size (max 100 (length disjoints)) :rehash-size 2.0
                                           :structure-p t))
        (loop for elem in disjoints do
              (setf (gethash elem table) t)
              finally (setf (gethash disjoint *disjoints-table*) table))
        ;(break)
        )
      (if table
        (let ((result (gethash concept table)))
          (unless result
            (setf (gethash concept table) t))
          result)
        (member concept disjoints)))
    (member concept (concept-told-disjoints disjoint))))

(defun propagate-told-disjoints (concept
                                     new-disjoints
                                     &optional (changed-mark (incf *tbox-classification-counter*)))
  (when (concept-visible-p concept)
    (loop for disjoint in new-disjoints do
          (if changed-mark
            (unless (told-disjoints-member concept disjoint)
              (push concept (concept-told-disjoints disjoint))
              (mark-concept-as-changed disjoint changed-mark +told-disjoints+))
            (setf (concept-told-disjoints disjoint)
                  (concept-set-union new-disjoints (concept-told-disjoints disjoint)))))))

(defun encode-definitions-and-record-disjoints (concept-list)
  (loop for concept in concept-list
        for definition = (concept-definition concept)
        when definition
        do
        (let ((encoded-definition (encode-concept-term definition concept)))
          (if (eq encoded-definition concept)
              (progn
                (setf (concept-encoded-definition concept) nil)
                (setf (concept-primitive-p concept) t))
            (let ((disjoints (and (concept-primitive-p concept)
                                  (asserted-disjoints encoded-definition))))
              (setf (concept-encoded-definition concept) encoded-definition)
              (when disjoints
                (if (concept-asserted-disjoints concept)
                    (setf (concept-asserted-disjoints concept)
                          (concept-set-union disjoints (concept-asserted-disjoints concept)))
                  (setf (concept-asserted-disjoints concept) disjoints))))))))

(defun compute-encoded-concept-list (tbox axioms)
  (let ((concept-list
         (loop for (left definition primitive-p) in axioms
               when (symbolp left)
               if (eq left +top-symbol+)
               when definition
               do (error "Concept ~A may not have a definition" +top-symbol+)
               end
               else
               collect (initialize-atomic-concept tbox left definition primitive-p)
               end
               end)))
    (setf concept-list (stable-concept-set-union *provisionally-inserted-atomic-concepts*
                                                 concept-list))
    (setf *provisionally-inserted-atomic-concepts* nil)
    (race-time (encode-definitions-and-record-disjoints concept-list))
    (when *provisionally-inserted-atomic-concepts*
      (race-time (encode-definitions-and-record-disjoints *provisionally-inserted-atomic-concepts*))
      (setf concept-list (stable-concept-set-union *provisionally-inserted-atomic-concepts*
                                                   concept-list))
      (setf *provisionally-inserted-atomic-concepts* nil))
    (let ((new-atomic-concept-names
           #+:debug
           (sort (racer-remove-duplicates
                  (loop for disjoint-set being the hash-values of (tbox-disjoint-set tbox)
                        append disjoint-set))
                 #'string-lessp)
           #-:debug
           (racer-remove-duplicates
            (loop for disjoint-set being the hash-values of (tbox-disjoint-set tbox)
                  append disjoint-set))))
      (when new-atomic-concept-names
        (let ((new-atomic-concepts
               (loop for name in new-atomic-concept-names
                     unless (get-tbox-concept tbox name nil)
                     collect (initialize-atomic-concept tbox name nil t))))
          (setf concept-list (stable-concept-set-union new-atomic-concepts concept-list)))))
    (loop for disjoint-set being the hash-values of (tbox-disjoint-set tbox)
          for disjoint-list = (mapcar #'(lambda (name)
                                          (get-tbox-concept tbox name))
                                      disjoint-set)
          do
          (loop for concept in disjoint-list do
                (setf (concept-asserted-disjoints concept)
                      (concept-set-union (concept-asserted-disjoints concept)
                                         (remove concept disjoint-list :count 1)))))
    (when *provisionally-inserted-atomic-concepts*
      (setf concept-list (stable-concept-set-union *provisionally-inserted-atomic-concepts*
                                                   concept-list))
      (setf *provisionally-inserted-atomic-concepts* nil))
    concept-list))

(race-inline (concept-is-changed told-subsumers-changed-p told-disjoints-changed-p
                                   referencing-changed-p full-referencing-changed-p
                                   language-or-inverse-roles-changed-p))

(defun concept-is-changed (concept changed-mark)
  (let ((concept-changed (concept-changed-p concept)))
    (and (concept-changed-info-p concept-changed)
         (eql (concept-changed-mark concept-changed) changed-mark))))

(defun told-subsumers-changed-p (concept)
  (let ((concept-changed (concept-changed-p concept)))
    (and (concept-changed-info-p concept-changed)
         (concept-changed-told-subsumers concept-changed))))

(defun told-disjoints-changed-p (concept)
  (let ((concept-changed (concept-changed-p concept)))
    (and (concept-changed-info-p concept-changed)
         (concept-changed-told-disjoints concept-changed))))

(defun referenced-disjoints-changed-p (concept)
  (let ((concept-changed (concept-changed-p concept)))
    (and (concept-changed-info-p concept-changed)
         (concept-changed-referenced-disjoints concept-changed))))

(defun referencing-changed-p (concept)
  (let ((concept-changed (concept-changed-p concept)))
    (and (concept-changed-info-p concept-changed)
         (concept-changed-referencing concept-changed))))

(defun full-referencing-changed-p (concept)
  (let ((concept-changed (concept-changed-p concept)))
    (and (concept-changed-info-p concept-changed)
         (concept-changed-full-referencing concept-changed))))

(defun language-or-inverse-roles-changed-p (concept)
  (let ((concept-changed (concept-changed-p concept)))
    (and (concept-changed-info-p concept-changed)
         (or (concept-changed-language concept-changed)
             (concept-changed-inverse-roles concept-changed)))))

(defun changed-concepts (tbox visited-mark)
  (loop for concept being the hash-value of (tbox-concept-store tbox)
        when (concept-is-changed concept visited-mark)
        collect concept))

(defun no-of-changed-concepts (tbox visited-mark)
  (loop for concept being the hash-value of (tbox-concept-store tbox)
        count (concept-is-changed concept visited-mark)))

(defun update-encoded-definitions (tbox)
  (loop with visited-mark = (incf *tbox-classification-counter*)
        for concept in (tbox-encoded-concept-list tbox)
        for asserted-disjoints = (concept-asserted-disjoints concept)
        when (and asserted-disjoints
                  (not (disjoint-and-concept-p (concept-encoded-definition concept))))
        do
        (setf (concept-told-disjoints concept)
              (concept-set-union asserted-disjoints (concept-told-disjoints concept)))
        (when *propagate-told-disjoints*
          (propagate-told-disjoints concept asserted-disjoints visited-mark)))
  (let* ((counter 1)
         (top (tbox-top-node tbox))
         (bottom (tbox-bottom-node tbox))
         (first-visited-mark (incf *tbox-classification-counter*))
         (*concept-changed-p* nil)
         (changed-concepts nil)
         (no-of-changed-concepts (length (tbox-encoded-concept-list tbox)))
         (start (when (or *print-where-am-i* *show-time*)
                  (get-internal-run-time)))
         (*disjoints-table* (racer-make-hash-table :size 100 :rehash-size 2.0 :structure-p t))
         )
    (when (or *print-where-am-i* *show-time*)
      (format t "~&Traversal Pass 1"))
    (loop initially
          (race-trace ("~&1. traversal through concept list ~S with marker ~A~%"
                       (tbox-encoded-concept-list tbox) visited-mark))
          with visited-mark = first-visited-mark
          for concept in (tbox-encoded-concept-list tbox)
          do (update-encoded-definition tbox
                                        concept
                                        top
                                        bottom
                                        visited-mark
                                        t
                                        t
                                        +all-features-no-referencing+))
    (loop with concept-list = (tbox-encoded-concept-list tbox)
          for previous-visited-mark = first-visited-mark then visited-mark
          for visited-mark = (incf *tbox-classification-counter*)
          do
          (progn
            (incf counter)
            (when (or *print-where-am-i* *show-time*)
              (format t "(~D,~,3F), ~D"
                      no-of-changed-concepts
                      (/ (- (get-internal-run-time) start) internal-time-units-per-second)
                      counter))
            (race-trace ("~&~D. traversal through concept list ~S with marker ~A (previous=~A)~%"
                         counter concept-list visited-mark previous-visited-mark))
            ;(when (> counter 26) (break "Pass ~D" counter))
            )
          (setf *concept-changed-p* nil)
          (when (or *print-where-am-i* *show-time*)
            (setf start (get-internal-run-time)))
          (loop for concept in concept-list
                do
                (update-encoded-definition tbox
                                           concept
                                           top
                                           bottom
                                           visited-mark
                                           nil
                                           previous-visited-mark
                                           +all-features-no-full-referencing+))
          (when (or *race-trace* *print-where-am-i* *show-time*)
            (if *race-trace*
              (progn
                (setf changed-concepts (changed-concepts tbox visited-mark))
                (setf no-of-changed-concepts (length changed-concepts)))
              (when (or *print-where-am-i* *show-time*)
                (setf changed-concepts nil)
                (setf no-of-changed-concepts (no-of-changed-concepts tbox visited-mark)))))
          (race-trace ("~&~D. traversal changed ~D concepts: ~S~%"
                       counter no-of-changed-concepts changed-concepts))
          ;(when (> counter 26) (break "no-of-changed-concepts: ~D" no-of-changed-concepts))
          (when (and *propagate-told-disjoints* (not *concept-changed-p*))
              (loop with told-disjoints-length = 
                    (mapcar (lambda (concept)
                              (length (concept-told-disjoints concept)))
                            (tbox-encoded-concept-list tbox))
                    for concept in (tbox-encoded-concept-list tbox)
                    for told-disjoints = (concept-told-disjoints concept)
                    unless (disjoint-and-concept-p (concept-encoded-definition concept))
                    do
                    (propagate-told-disjoints concept told-disjoints visited-mark)
                    finally
                    (loop for old-length in told-disjoints-length
                          for concept in (tbox-encoded-concept-list tbox)
                          do
                          (unless (eql old-length (length (concept-told-disjoints concept)))
                            (mark-concept-as-changed concept visited-mark +told-disjoints+)))))
          until (not *concept-changed-p*)
          #+:debug 
          finally
          #+:debug (assert (loop for concept being the hash-value of (tbox-concept-store tbox)
                                 always (concept-visited-mark concept))))
    (when (or *print-where-am-i* *show-time*)
      (format t "(~D,~,3F), End~%"
              no-of-changed-concepts
               (/ (- (get-internal-run-time) start) internal-time-units-per-second)))
    ;(break)
    ))

(defun propagate-full-referencing (to from &optional (referencing-p nil))
  (case (type-of from)
    (atomic-concept
     (unless (or (eq from *top-concept*) (eq from *bottom-concept*))
       (pushnew from (concept-full-referencing to))))
    (negated-concept
     (pushnew (concept-term from) (concept-full-referencing to)))
    ((and-concept or-concept some-concept all-concept at-least-concept at-most-concept disjoint-and-concept)
     (setf (concept-full-referencing to)
           (concept-set-union (concept-full-referencing from) (concept-full-referencing to))))
    (t ;(break)
     (setf (concept-full-referencing to)
           (concept-set-union (concept-full-referencing from) (concept-full-referencing to)))))
  (when (and referencing-p (negated-concept-p to))
    (setf (concept-full-referencing to)
          (concept-set-union (concept-referenced-disjoints from) (concept-full-referencing to))))
  #+:debug (concept-full-referencing to))

(defun propagate-referencing (to from &optional (referencing-p nil))
  (case (type-of from)
    (atomic-concept
     (unless (or (eq from *top-concept*) (eq from *bottom-concept*))
       (pushnew from (concept-referencing to))))
    ((and-concept or-concept)
     (setf (concept-referencing to)
           (concept-set-union (concept-referencing from) (concept-referencing to))))
    (negated-concept
     (pushnew (concept-term from) (concept-referencing to))))
  (when (and referencing-p (negated-concept-p to))
    (setf (concept-referencing to)
          (concept-set-union (concept-referenced-disjoints from) (concept-referencing to))))
  #+:debug (concept-referencing to))

(defun propagate-atomic-concept-info (concept
                                           encoded-definition
                                           changed-mark
                                           first-pass
                                           what
                                           &optional (ignore-told-subsumers nil))
  (when (update-referencing-p what)
    (propagate-referencing concept encoded-definition))
  (when (update-full-referencing-p what)
    (propagate-full-referencing concept encoded-definition))
  (when (update-told-subsumers-p what)
    (unless (or ignore-told-subsumers
                (concept-primitive-p concept)
                (member concept (concept-told-subsumers encoded-definition)))
      (push concept (concept-told-subsumers encoded-definition))
      (mark-concept-as-changed encoded-definition changed-mark +told-subsumers+))
    (when (or first-pass (told-subsumers-changed-p encoded-definition))
      (let ((old-told-subsumers (concept-told-subsumers concept)))
        (setf (concept-told-subsumers concept)
              (concept-set-union (concept-told-subsumers encoded-definition)
                                 (concept-told-subsumers concept)))
        (unless (eql (length old-told-subsumers) (length (concept-told-subsumers concept)))
          (mark-concept-as-changed concept changed-mark +told-subsumers+)))))  
  (when (update-told-disjoints-p what)
    (when (or first-pass (told-disjoints-changed-p encoded-definition))
      (let ((old-told-disjoints (concept-told-disjoints concept)))
        (add-to-told-disjoints concept (concept-told-disjoints encoded-definition))
        (unless (eql (length old-told-disjoints) (length (concept-told-disjoints concept)))
          (mark-concept-as-changed concept changed-mark +told-disjoints+)))))
  (when (update-referenced-disjoints-p what)
    (when (or first-pass (referenced-disjoints-changed-p encoded-definition))
      (let ((old-referenced-disjoints (concept-referenced-disjoints concept)))
        (setf (concept-referenced-disjoints concept)
              (concept-set-union (concept-referenced-disjoints encoded-definition)
                                 (concept-referenced-disjoints concept)))
        (unless (eql (length old-referenced-disjoints) (length (concept-referenced-disjoints concept)))
          (mark-concept-as-changed concept changed-mark +referenced-disjoints+))))))


(defun propagate-info (to
                          from
                          changed-mark
                          first-pass
                          what
                          &key
                          (told-subsumers-p t)
                          (told-disjoints-p nil)
                          (referencing-p t))
  (when (update-referencing-p what)
    (propagate-referencing to from referencing-p))
  (when (update-full-referencing-p what)
    (propagate-full-referencing to from))
  (when (update-referenced-disjoints-p what)
    (when (and referencing-p (or first-pass (referenced-disjoints-changed-p from)))
      (let ((old-concept-referenced-disjoints (concept-referenced-disjoints to)))
        (setf (concept-referenced-disjoints to)
              (concept-set-union (concept-referenced-disjoints from)
                                 (concept-referenced-disjoints to)))
        (unless (eql (length old-concept-referenced-disjoints)
                     (length (concept-referenced-disjoints to)))
          (mark-concept-as-changed to changed-mark +referenced-disjoints+)))))
  (when (update-told-subsumers-p what)
    (when (and told-subsumers-p (not (or-concept-p to)))
      (when (or first-pass (told-subsumers-changed-p from))
        (let ((old-told-subsumers (concept-told-subsumers to)))
          (setf (concept-told-subsumers to)
                (concept-set-union (concept-told-subsumers from)
                                   (concept-told-subsumers to)))
          (unless (eql (length old-told-subsumers) (length (concept-told-subsumers to)))
            (mark-concept-as-changed to changed-mark +told-subsumers+))))
      (when (update-told-disjoints-p what)
        (when (and told-disjoints-p
                   (not (disjoint-and-concept-p to))
                   (or (atomic-concept-p to) (and-concept-p to))
                   (or (atomic-concept-p from) (and-concept-p from))
                   (or first-pass (told-disjoints-changed-p from)))
          (let ((old-told-disjoints (concept-told-disjoints to)))
            (add-to-told-disjoints to (concept-told-disjoints from))
            (unless (eql (length old-told-disjoints) (length (concept-told-disjoints to)))
              (mark-concept-as-changed to changed-mark +told-disjoints+))))))))

(defun update-encoded-definition (tbox
                                       concept
                                       top
                                       bottom
                                       visited-mark
                                       first-pass
                                       changed-mark
                                       &optional
                                       (what +all-changed-features+))
  (setf (concept-visited-mark concept) visited-mark)
  (if (atomic-concept-p concept)
    (update-atomic-concept tbox concept top bottom visited-mark first-pass changed-mark what)
    (let ((concept-term (concept-term concept)))
      (ecase (type-of concept)
        ((and-concept or-concept)
         (loop with changed-p = first-pass
               for elem in concept-term do
               (unless (eql (concept-visited-mark elem) visited-mark)
                 (update-encoded-definition tbox
                                            elem
                                            top
                                            bottom
                                            visited-mark
                                            first-pass
                                            changed-mark
                                            what))
               (when (or first-pass
                         (concept-is-changed elem changed-mark)
                         (update-full-referencing-p what)
                         (update-referencing-p what))
                 (propagate-info concept elem visited-mark first-pass what :told-disjoints-p t)
                 (when (and (not changed-p)
                            (language-or-inverse-roles-changed-p elem))
                   (setf changed-p t)))
               finally (when changed-p
                         (set-language concept visited-mark))))
        ((some-concept at-least-concept)
         (let ((changed-p first-pass))
           (let ((role-domain (concept-role-domain concept)))
             (when role-domain
               (unless (eql (concept-visited-mark role-domain) visited-mark)
                 (update-encoded-definition tbox
                                            role-domain
                                            top
                                            bottom
                                            visited-mark
                                            first-pass
                                            changed-mark
                                            what))
               (when (or first-pass
                         (concept-is-changed role-domain changed-mark)
                         (update-full-referencing-p what)
                         (update-referencing-p what))
                 (propagate-info concept role-domain visited-mark first-pass what)
                 (when (and (not changed-p) (language-or-inverse-roles-changed-p role-domain))
                   (setf changed-p t)))))
           (let ((role-range (concept-role-range concept)))
             (when role-range
               (unless (eql (concept-visited-mark role-range) visited-mark)
                 (update-encoded-definition tbox
                                            role-range
                                            top
                                            bottom
                                            visited-mark
                                            first-pass
                                            changed-mark
                                            what))
               (when (or first-pass
                         (concept-is-changed role-range changed-mark)
                         (update-full-referencing-p what)
                         (update-referencing-p what))
                 (when (update-referencing-p what)
                   (propagate-referencing concept role-range))
                 (when (update-full-referencing-p what)
                   (propagate-full-referencing concept role-range))
                 (when (and (not changed-p)
                            (language-or-inverse-roles-changed-p role-range))
                   (setf changed-p t)))))
           (unless (eql (concept-visited-mark concept-term) visited-mark)
             (update-encoded-definition tbox
                                        concept-term
                                        top
                                        bottom
                                        visited-mark
                                        first-pass
                                        changed-mark
                                        what))
           (when (and (not (eq concept-term top))
                      (or first-pass 
                          (concept-is-changed concept-term changed-mark)
                          (update-full-referencing-p what)
                          (update-referencing-p what)))
             (when (update-referencing-p what)
               (propagate-referencing concept concept-term))
             (when (update-full-referencing-p what)
               (propagate-full-referencing concept concept-term))
             (when (and (not changed-p) (language-or-inverse-roles-changed-p concept-term))
               (setf changed-p t)))
           (when changed-p
             (set-language concept visited-mark))))
        ((all-concept at-most-concept)
         (unless (eql (concept-visited-mark concept-term) visited-mark)
           (update-encoded-definition tbox
                                      concept-term
                                      top
                                      bottom
                                      visited-mark
                                      first-pass
                                      changed-mark
                                      what))
         (when (and (not (eq concept-term top))
                    (not (eq concept-term bottom))
                    (or first-pass 
                        (concept-is-changed concept-term changed-mark)
                        (update-full-referencing-p what)
                        (update-referencing-p what)))
           (when (update-referencing-p what)
             (propagate-referencing concept concept-term))
           (when (update-full-referencing-p what)
             (propagate-full-referencing concept concept-term))
           (when (or first-pass (language-or-inverse-roles-changed-p concept-term))
             (set-language concept visited-mark))))
        (negated-concept
         (unless (eql (concept-visited-mark concept-term) visited-mark)
           (update-encoded-definition tbox
                                      concept-term
                                      top
                                      bottom
                                      visited-mark
                                      first-pass
                                      changed-mark
                                      what)
           (when (or first-pass 
                     (concept-is-changed concept-term changed-mark)
                     (update-full-referencing-p what)
                     (update-referencing-p what))
             (when (update-referencing-p what)
               (propagate-referencing concept concept-term t))
             (when (update-full-referencing-p what)
               (propagate-full-referencing concept concept-term))
             (when (or first-pass (referenced-disjoints-changed-p concept-term))
               (let ((old-concept-referencing (concept-referencing concept)))
                 (setf (concept-referencing concept)
                       (concept-set-union (concept-referenced-disjoints concept-term)
                                          (concept-referencing concept)))
                 (unless (eql (length old-concept-referencing)
                              (length (concept-referencing concept)))
                   (mark-concept-as-changed concept changed-mark +referencing+))))
             (when (or first-pass (language-or-inverse-roles-changed-p concept-term))
               (set-language concept visited-mark))))
         (let* ((definition (concept-encoded-definition concept-term))
                (negated-definition (and definition (concept-negated-concept-internal definition))))
           (when (and negated-definition
                      (not (concept-primitive-p concept-term))
                      (or ;; first-pass
                          ;; first-pass eliminated because of tight loop with some example test-tboxes
                          ;; rm 2007/05/03
                          (not (eql (concept-visited-mark negated-definition) visited-mark))))
             (update-encoded-definition tbox
                                        negated-definition
                                        top
                                        bottom
                                        visited-mark
                                        first-pass
                                        changed-mark
                                        what)
             (unless (or (eq negated-definition top)
                         (eq negated-definition bottom))
               (when (or first-pass 
                         (and (concept-is-changed negated-definition changed-mark))
                         (update-full-referencing-p what)
                         (update-referencing-p what))
                 (propagate-info concept negated-definition visited-mark first-pass what
                                 :told-subsumers-p nil))))))
        (disjoint-and-concept
         (loop for elem in concept-term do
               (unless (eql (concept-visited-mark elem) visited-mark)
                 (update-encoded-definition tbox
                                            elem
                                            top
                                            bottom
                                            visited-mark
                                            first-pass
                                            changed-mark
                                            what))))
        (cd-concept
         (let ((changed-p first-pass)
               (attribute-domain (concept-attribute-domain concept)))
           (when attribute-domain
             (unless (eql (concept-visited-mark attribute-domain) visited-mark)
               (update-encoded-definition tbox
                                          attribute-domain
                                          top
                                          bottom
                                          visited-mark
                                          first-pass
                                          changed-mark
                                          what))
             (when (or first-pass
                       (concept-is-changed attribute-domain changed-mark)
                       (update-full-referencing-p what)
                       (update-referencing-p what))
               (propagate-info concept attribute-domain visited-mark first-pass what)
               (when (and (not changed-p) 
                          (language-or-inverse-roles-changed-p attribute-domain))
                 (setf changed-p t))))
           (when changed-p
             (set-language concept visited-mark)))
         nil)
        (ensure-cd-concept
         (let ((changed-p first-pass)
               (cd-concept (concept-term concept)))
           (unless (eql (concept-visited-mark cd-concept) visited-mark)
             (update-encoded-definition tbox
                                        cd-concept
                                        top
                                        bottom
                                        visited-mark
                                        first-pass
                                        changed-mark
                                        what))
           (when (or first-pass
                     (concept-is-changed cd-concept changed-mark)
                     (update-full-referencing-p what)
                     (update-referencing-p what))
             (propagate-info concept cd-concept visited-mark first-pass what)
             (when (and (not changed-p) 
                        (language-or-inverse-roles-changed-p cd-concept))
               (setf changed-p t)))
           (when changed-p
             (set-language concept visited-mark)))
         nil)
        (general-cd-concept nil))
      (let ((negated-concept (concept-negated-concept-internal concept)))
        (when (and negated-concept
                   (not (eql (concept-visited-mark negated-concept) visited-mark)))
          (update-encoded-definition tbox
                                     negated-concept
                                     top
                                     bottom
                                     visited-mark
                                     first-pass
                                     changed-mark
                                     what)))))
  #+:debug (concept-language concept)
  )

(defun update-atomic-concept (tbox
                                  concept
                                  top
                                  bottom
                                  visited-mark
                                  first-pass
                                  changed-mark
                                  what)
  (progn
    (let ((encoded-definition (concept-encoded-definition concept)))
      (unless (or (null encoded-definition)
                  (eq encoded-definition top)
                  (eq encoded-definition bottom))
        (unless (eql (concept-visited-mark encoded-definition) visited-mark)
          (update-encoded-definition tbox
                                     encoded-definition
                                     top
                                     bottom
                                     visited-mark
                                     first-pass
                                     changed-mark
                                     what))
        (unless (concept-primitive-p concept)
          (let ((negated-definition (concept-negated-concept-internal encoded-definition)))
            (unless (or (null negated-definition)
                        (eql (concept-visited-mark negated-definition) visited-mark))
              (update-encoded-definition tbox
                                         negated-definition
                                         top
                                         bottom
                                         visited-mark
                                         first-pass
                                         changed-mark
                                         what))))
        (when (or first-pass
                  (concept-is-changed encoded-definition changed-mark)
                  (update-full-referencing-p what)
                  (update-referencing-p what))
          (propagate-atomic-concept-info concept encoded-definition visited-mark first-pass what)
          (when (or first-pass (language-or-inverse-roles-changed-p encoded-definition))
            (set-language concept visited-mark)))))
    (let ((nary-unfold-sets (concept-nary-unfold-sets concept)))
      (when nary-unfold-sets
        (let ((unfolding-table (tbox-nary-absorption-table tbox)))
          (loop for unfold-set in nary-unfold-sets
                for unfolding = (gethash unfold-set unfolding-table)
                do
                (unless (eql (concept-visited-mark unfolding) visited-mark)
                  (update-encoded-definition tbox
                                             unfolding
                                             top
                                             bottom
                                             visited-mark
                                             first-pass
                                             changed-mark
                                             what)
                  (when (concept-is-changed unfolding changed-mark)
                    (if (update-full-referencing-p what)
                      (propagate-full-referencing concept unfolding)
                      (when (update-referencing-p what)
                        (propagate-referencing concept unfolding)))
                    (when (language-or-inverse-roles-changed-p unfolding)
                      (set-language concept visited-mark))))))))
    (let ((encoded-negated-definition (concept-encoded-negated-definition concept))
          (negated-concept (concept-negated-concept-internal concept)))
      (when encoded-negated-definition
        #+:debug (assert (concept-p-internal encoded-negated-definition))
        (unless (eql (concept-visited-mark encoded-negated-definition) visited-mark)
          (update-encoded-definition tbox
                                     encoded-negated-definition
                                     top
                                     bottom
                                     visited-mark
                                     first-pass
                                     changed-mark
                                     what))
        (when (or first-pass
                  (concept-is-changed encoded-negated-definition changed-mark)
                  (update-full-referencing-p what)
                  (update-referencing-p what))
          (unless (or (eq encoded-negated-definition top)
                      (eq encoded-negated-definition bottom))
            (propagate-info concept
                            encoded-negated-definition
                            visited-mark
                            first-pass
                            what
                            :told-subsumers-p nil
                            :referencing-p nil)
            (when negated-concept
              (propagate-info negated-concept
                              encoded-negated-definition
                              visited-mark
                              first-pass
                              what
                              :told-subsumers-p nil)))
          (when (or first-pass (language-or-inverse-roles-changed-p encoded-negated-definition))
            (set-language concept visited-mark)
            (when negated-concept
              (set-language negated-concept visited-mark))))
        (when (and (negated-concept-p encoded-negated-definition)
                   (or first-pass
                       (concept-is-changed concept changed-mark)
                       (update-full-referencing-p what)
                       (update-referencing-p what)))
          (propagate-atomic-concept-info (concept-term encoded-negated-definition)
                                         concept
                                         visited-mark
                                         first-pass
                                         what
                                         t))))))

#+:debug
(defun reencode-all-exists-cd-concepts (tbox &optional (exists-only nil) (cd-only nil))
  (let ((exists-concepts
         (sort-concept-list
          (if *use-less-tbox-memory*
              (loop for concept being the hash-value of (tbox-concept-store tbox)
                    for at-most-concept-p = (at-most-concept-p concept)
                    when (or (and (not cd-only)
                                  (or at-most-concept-p (exists-concept-p concept)))
                             (and (not exists-only) (cd-concept-p concept)))
                    collect (if at-most-concept-p
                                (concept-negated-concept concept)
                              concept))
            (loop for concept being the hash-value of (tbox-concept-store tbox)
                  when (or (and (not cd-only) (exists-concept-p concept))
                           (and (not exists-only) (cd-concept-p concept)))
                  collect concept)))))
    (with-enforced-reencoding
      (loop for concept in exists-concepts do
            (if (exists-concept-p concept)
              (reencode-exists-concept tbox concept)
              (reencode-cd-concept concept))
            (set-language concept)))))

#-:debug
(defun reencode-all-exists-cd-concepts (tbox &optional (exists-only nil) (cd-only nil))
  (with-enforced-reencoding
    (loop with lean-tbox = *use-less-tbox-memory*
          for concept being the hash-value of (tbox-concept-store tbox)
          for at-most-concept-p = (and lean-tbox (at-most-concept-p concept))
          when (or (and (not cd-only)
                        (or at-most-concept-p (exists-concept-p concept)))
                   (and (not exists-only) (cd-concept-p concept)))
          do
          (if (exists-concept-p concept)
              (reencode-exists-concept tbox concept)
            (if (and lean-tbox at-most-concept-p)
                (reencode-exists-concept tbox (concept-negated-concept concept))
              (reencode-cd-concept concept)))
          (set-language concept))))

#+:debug
(defun reencode-all-quantification-concepts (tbox)
  (let ((quantification-concepts
         (sort-concept-list
          (if *use-less-tbox-memory*
              (loop for concept being the hash-value of (tbox-concept-store tbox)
                    when (quantification-concept-p concept)
                    collect (if (exists-concept-p concept)
                                concept
                              (concept-negated-concept concept)))
            (loop for concept being the hash-value of (tbox-concept-store tbox)
                  when (quantification-concept-p concept)
                  collect concept)))))
    (with-enforced-reencoding
      (loop for concept in quantification-concepts do
            (reencode-quantification-concept tbox concept)
            (set-language concept)))))

#-:debug
(defun reencode-all-quantification-concepts (tbox)
  (with-enforced-reencoding
    (if *use-less-tbox-memory*
        (loop for concept being the hash-value of (tbox-concept-store tbox)
              when (quantification-concept-p concept)
              do
              (if (exists-concept-p concept)
                  (reencode-quantification-concept tbox concept)
                (reencode-quantification-concept tbox (concept-negated-concept concept)))
              (set-language concept))
      (loop for concept being the hash-value of (tbox-concept-store tbox)
            when (quantification-concept-p concept)
            do
            (reencode-quantification-concept tbox concept)
            (set-language concept)))))

#+:debug
(defun encode-disjoint-sets (tbox)
  (let ((disjoint-sets
         (sort (loop for disjoint-set being the hash-values of (tbox-disjoint-set tbox)
                     collect disjoint-set)
               #'string-lessp
               :key #'first)))
    (loop for disjoint-set in disjoint-sets do
          (loop with concept = (if (and *optimize-disjoint-ands*
                                        (> (length disjoint-set) *disjoint-ands-threshold*))
                                 'disjoint-and
                                 'and)
                for set on disjoint-set
                while (rest set) do
                (add-concept-axiom tbox
                                   (first set)
                                   (list* concept (mapcar #'(lambda (name)
                                                              `(not ,name))
                                                          (rest set)))
                                   :inclusion-p t)))))

#-:debug
(defun encode-disjoint-sets (tbox)
  (loop for disjoint-set being the hash-values of (tbox-disjoint-set tbox) do
        (loop with concept = (if (and *optimize-disjoint-ands*
                                      (> (length disjoint-set) *disjoint-ands-threshold*))
                               'disjoint-and
                               'and)
              for set on disjoint-set
              while (rest set) do
              (add-concept-axiom tbox
                                 (first set)
                                 (list* concept (mapcar #'(lambda (name)
                                                            `(not ,name))
                                                        (rest set)))
                                 :inclusion-p t))))

(defun preencode-concepts (tbox stream)
  (encode-disjoint-sets tbox)
  (when (tbox-use-less-memory tbox)
    (setf (tbox-concept-axioms-index tbox)
      ;; Should it suffice to do a clrhash?
      ;; Maybe not (in case the data structure is not shrunk during clrhash). -- rm 01-11-08
      (make-concept-axioms-index tbox)))
  ;; The following two nreverses assume that axioms have just been set up.
  ;; I see a problem with tbox that are just reset. In this case the reverse might 
  ;; not have the desired effect (on the contrary). -- rm 01-11-08
  (setf (tbox-concept-axioms tbox) (nreverse (tbox-concept-axioms tbox)))
  (setf (tbox-generalized-concept-inclusions tbox)
        (nreverse (tbox-generalized-concept-inclusions tbox)))
  (let ((encoded-concept-list (race-time (compute-encoded-concept-list tbox (tbox-concept-axioms tbox)))))
    (when (tbox-use-less-memory tbox)
      (setf (tbox-concept-axioms tbox) nil))
    (setf (tbox-encoded-concept-list tbox) encoded-concept-list)
    (multiple-value-bind (added-gcis cyclic-concepts)
                         (race-time (transform-cyclical-concepts tbox encoded-concept-list))
      (setf (tbox-added-generalized-concept-inclusions tbox) added-gcis)
      (multiple-value-bind (meta-constraint-concepts
                            nary-absorption-table
                            domain-qualifications-table
                            true-tbox-el+-p
                            new-reflexive-roles
                            new-irreflexive-roles)
                           (absorb-gcis tbox
                                        (append added-gcis
                                                (tbox-meta-constraint-concepts tbox)
                                                (mapcar #'(lambda (gci)
                                                            (list (encode-concept-term (first gci))
                                                                  (encode-concept-term (second gci))))
                                                        (tbox-generalized-concept-inclusions tbox)))
                                        cyclic-concepts)
        (when domain-qualifications-table
          (setf (tbox-domain-qualifications-table tbox) domain-qualifications-table))
        (when nary-absorption-table
          (setf (tbox-nary-absorption-table tbox) nary-absorption-table)
          (loop for key being the hash-key of nary-absorption-table using (hash-value value) do
                (setf (gethash key nary-absorption-table)
                      (encode-concept-term `(and ,.(loop for elem in value
                                                         if (listp elem)
                                                         collect `(or .,elem)
                                                         else collect elem)))))
          (setf (tbox-inverse-nary-absorption-table tbox) (racer-make-hash-table))
          (loop with inverse-nary-absorption-table = (tbox-inverse-nary-absorption-table tbox)
                for concept being the hash-value of nary-absorption-table
                do
                (if (and-concept-p concept)
                    (loop for concept in (concept-term concept)
                          for entry = (gethash concept inverse-nary-absorption-table)
                          do
                          (unless entry
                            (setf (gethash concept inverse-nary-absorption-table) t)))
                  (unless (gethash concept inverse-nary-absorption-table)
                    (setf (gethash concept inverse-nary-absorption-table) t)))))
        (loop for role in new-reflexive-roles do
              (propagate-role-characteristics-to-parents role t nil))
        (loop for role in new-irreflexive-roles do
              (propagate-role-characteristics-to-children role))
        (setf (tbox-meta-constraint-concepts tbox) meta-constraint-concepts)
        (setf (tbox-literal-meta-constraints tbox)
              (mapcar #'decode-concept meta-constraint-concepts))
        (when *provisionally-inserted-atomic-concepts*
          (setf encoded-concept-list
                (stable-concept-set-union *provisionally-inserted-atomic-concepts*
                                          encoded-concept-list))
          (setf *provisionally-inserted-atomic-concepts* nil))
        (setf (tbox-encoded-concept-list tbox) encoded-concept-list)
        (unless true-tbox-el+-p
          (race-time (reencode-all-exists-cd-concepts tbox)))
        (loop for concept in (cons (tbox-bottom-node tbox) encoded-concept-list)
              when (concept-nary-unfold-sets concept)
              do (setf (concept-nary-unfold-sets concept) nil))
        (when nary-absorption-table
          (loop for lhs being the hash-key of nary-absorption-table do
                (loop for concept in lhs do
                      (push lhs (concept-nary-unfold-sets concept)))))
        (if (and *use-elh-transformation* *use-elh-model-embedding* true-tbox-el+-p)
            (progn
              (when *provisionally-inserted-roles*
                (add-provisionally-encoded-roles tbox *provisionally-inserted-roles*)
                (preencode-roles-3 tbox *provisionally-inserted-roles*)
                (encode-roles tbox *provisionally-inserted-roles* t)
                (setf *provisionally-inserted-roles* nil))
              (race-time (update-encoded-definitions tbox))
              (race-time (tbox-el+-saturation tbox)))
          (race-time (update-encoded-definitions tbox)))
        (setf (tbox-meta-constraint-concepts tbox)
              (let ((*concept-changed-p* nil))
                (loop for concept in meta-constraint-concepts
                      with top = (tbox-top-node tbox)
                      with bottom = (tbox-bottom-node tbox)
                      with mark = (incf *tbox-classification-counter*)
                      if (eq concept bottom)
                      do
                      (return (list bottom))
                      else
                      unless (eq concept top)
                      collect concept and
                      do (update-encoded-definition tbox concept top bottom mark t t)
                      end
                      do (setf (tbox-language tbox) 
                               (union-dl-descriptors (concept-language concept)
                                                     (tbox-language tbox))))))
        (when (tbox-meta-constraint-concepts tbox)
          (setf (tbox-meta-constraint-concept tbox)
                (encode-concept-term `(and .,(tbox-meta-constraint-concepts tbox)))))
        (loop for concept in encoded-concept-list
              for encoded-definition = (concept-encoded-definition concept)
              with table = (tbox-taxonomic-encoding-table tbox)
              with empty-p = t
              with contains-cyclic-concepts = nil
              with language = (tbox-language tbox)
              when encoded-definition do
              (unless (concept-primitive-p concept)
                (when empty-p
                  (setf empty-p nil))
                (if (atomic-concept-p encoded-definition)
                  (setf (gethash concept table) encoded-definition)
                  (setf (gethash encoded-definition table) concept)))
              do
              (when (member concept (concept-full-referencing concept))
                (setf (concept-self-referencing-p concept) t)
                (setf contains-cyclic-concepts t))
              (if (concept-self-referencing-p concept)
                (tbox-classification-report
                 stream
                 :concept-cyclic #+:debug concept #-:debug (concept-name-set concept)
                 (tbox-name tbox))
                (when (member concept (tbox-meta-constraint-concepts tbox)
                              :key #'concept-full-referencing :test #'member)
                  (setf (concept-self-referencing-p concept) t)
                  (setf contains-cyclic-concepts t)
                  (tbox-classification-report
                   stream
                   :concept-cyclic-gci #+:debug concept #-:debug (concept-name-set concept)
                   (tbox-name tbox))))
              (setf language (union-dl-descriptors (concept-language concept) language))
              finally
              (setf (tbox-language tbox) language)
              (setf (tbox-empty-taxonomic-encoding-table tbox) empty-p)
              (when contains-cyclic-concepts
                (setf (tbox-blocking-possibly-required tbox) t)))
        (when (and (tbox-use-less-memory tbox) (tbox-el+-transformed-table tbox))
          (clean-el+-tbox-fragment tbox))))))

(defun transform-cyclical-concepts (tbox concept-list)
  "Returns 2 values: added-gcis, cyclic-concepts"
  (loop for concept in concept-list
        for encoded-definition = (concept-encoded-definition concept)
        with added-gcis = nil
        with cyclic-concepts = nil
        with top = (tbox-top-node tbox)
        with bottom = (tbox-bottom-node tbox)
        do
        (cond ((eq concept encoded-definition)          ; transform to atomic concept
               (setf (concept-definition concept) nil)
               (setf (concept-encoded-definition concept) nil)
               (setf (concept-primitive-p concept) t))
              ((and (not (concept-primitive-p concept))         ; split into primitive and gci
                    (refers-to encoded-definition concept))
               (setf (concept-primitive-p concept) t)
               (push concept cyclic-concepts)
               (when (or (not (atomic-concept-p encoded-definition))
                         (and (not (eq encoded-definition top))
                              (not (eq encoded-definition bottom))))
                 (push (list encoded-definition concept) added-gcis))))
        finally (return (values (nreverse added-gcis) cyclic-concepts))))

(defun refers-to (concept1 
                    concept2 
                    &optional (traverse-some-and-all nil)
                    (visited-concepts nil))
  (refers-to-1 concept1 concept2 traverse-some-and-all visited-concepts nil))


(defun refers-to-1 (concept1 
                      concept2 
                      traverse-some-and-all
                      visited-concepts
                      unused)
  (declare (ignore unused))
  (if (atomic-concept-p concept1)
    (or (member concept1 visited-concepts)
        (eq concept1 concept2)
        (and (or (not (concept-primitive-p concept1))
                 traverse-some-and-all)
             (or (refers-to-1 (concept-encoded-definition concept1)
                              concept2
                              traverse-some-and-all
                              (cons concept1 visited-concepts)
                              nil)
                 (when (concept-encoded-negated-definition concept1)
                   (refers-to-1 (concept-encoded-negated-definition concept1)
                                concept2
                                traverse-some-and-all
                                (cons concept1 visited-concepts)
                                nil)))))
    (cond ((exists-concept-p concept1)
           (when (and traverse-some-and-all
                      (not (member concept1 visited-concepts)))
             (or (and (concept-role-domain concept1)
                      (refers-to-1 (concept-role-domain concept1)
                                   concept2
                                   traverse-some-and-all
                                   (cons concept1 visited-concepts)
                                   nil))
                 (and (concept-role-range concept1)
                      (refers-to-1 (concept-role-range concept1)
                                   concept2
                                   traverse-some-and-all
                                   (cons concept1 visited-concepts)
                                   nil))
                 (refers-to-1 
                  (concept-term concept1)
                  concept2
                  traverse-some-and-all
                  (cons concept1 visited-concepts)
                  nil))))
          ((cd-concept-p concept1)
           (unless (member concept1 visited-concepts)
             (and (concept-attribute-domain concept1)
                  (refers-to-1 (concept-attribute-domain concept1)
                               concept2
                               traverse-some-and-all
                               (cons concept1 visited-concepts)
                               nil))))
          ((and traverse-some-and-all
                (or (some-concept-p concept1) 
                    (all-concept-p concept1)))
           (refers-to-1 (concept-term concept1)
                        concept2
                        traverse-some-and-all
                        (cons concept1 visited-concepts)
                        nil))
          ((or (and-concept-p concept1)
               (or-concept-p concept1))
           (loop for concept in (concept-term concept1)
                 thereis (refers-to-1 concept
                                      concept2
                                      traverse-some-and-all
                                      (cons concept1 visited-concepts)
                                      nil)))
          ((negated-concept-p concept1)
           (refers-to-1 (concept-term concept1)
                        concept2 
                        traverse-some-and-all
                        (cons concept1 visited-concepts)
                        nil)))))

(defun sort-encoded-concept-list (tbox mirrored-concept-table &optional (tbox-el+-p nil))
  (multiple-value-bind (visible hidden)
      (if mirrored-concept-table
          (loop for concept in (tbox-encoded-concept-list tbox)
                if (gethash concept mirrored-concept-table)
                collect concept into visible
                finally (return visible))
        (loop for concept in (tbox-encoded-concept-list tbox)
              if (or (concept-visible-p concept)
                     (concept-encoded-definition concept))
              collect concept into visible
              else collect concept into hidden
              finally (return (values visible hidden))))
    (when (or hidden mirrored-concept-table)
      (setf (tbox-encoded-concept-list tbox) visible))
    (cond
     (*random-concept-list* (sort-concept-list-random tbox))
     ((or *told-subsumer-sorted-concept-list*
          (and tbox-el+-p *use-elh-transformation* *use-elh-model-embedding*))
      (sort-concept-list-told-subsumers tbox))
     (*fully-sorted-concept-list*
      (if tbox-el+-p
        (sort-elh-concept-list tbox)
        (sort-concept-list-full-referencing tbox)))
     (*sorted-concept-list*
      (sort-concept-list-referencing tbox)))
    (when hidden
      (setf (tbox-encoded-concept-list tbox) (nconc hidden (tbox-encoded-concept-list tbox)))))
  #+:debug (tbox-encoded-concept-list tbox))

(defun sort-concept-list-random (tbox)
  #+:debug (assert (not *sorted-concept-list*))
  (when (tbox-encoded-concept-list tbox)
    (loop with concept-vector = (apply #'vector (tbox-encoded-concept-list tbox))
          with max = (length concept-vector)
          for index from 0 to (1- max)
          for value = (svref concept-vector index)
          for partner = (random max)
          unless (eql index partner)
          do
          (setf (svref concept-vector index) (svref concept-vector partner))
          (setf (svref concept-vector partner) value)
          finally
          (return (setf (tbox-encoded-concept-list tbox)
                        (loop for elem across concept-vector
                              collect elem))))))
(defun mark-concept-nodes (ancestor)
  #+:debug
  (assert (null (concept-sort-no ancestor)))
  (setf (concept-sort-no ancestor) t)
  (let ((referenced-by (concept-referenced-by ancestor)))
    (if (listp referenced-by)
        (loop for concept in referenced-by do
              (unless (concept-sort-no concept)
                (mark-concept-nodes concept)))
      (loop for concept being the hash-key of referenced-by do
            (unless (concept-sort-no concept)
              (mark-concept-nodes concept)))))
  (setf (concept-sort-no ancestor) (incf *sort-counter*)))

(defun mark-concepts-for-sorting (tbox root-nodes)
  (let ((*sort-counter* 0))
    (loop for root-node in root-nodes do
          (unless (concept-sort-no root-node)
            (mark-concept-nodes root-node)))
    (loop for concept in (tbox-encoded-concept-list tbox) do
          (unless (concept-sort-no concept)
            (mark-concept-nodes concept))))
  #+:debug
  (assert (not (find-if-not #'numberp (tbox-encoded-concept-list tbox) :key #'concept-sort-no))))

(defun sort-marked-concepts (tbox)
  (setf (tbox-encoded-concept-list tbox)
        #-:debug
        (sort (tbox-encoded-concept-list tbox) #'> :key #'concept-sort-no)
        #+:debug
        (sort (copy-list (tbox-encoded-concept-list tbox)) #'> :key #'concept-sort-no)
        ))

(defun compute-root-nodes-referencing (tbox)
  (loop for concept in (tbox-encoded-concept-list tbox)
        unless (concept-referencing concept)
        collect concept))

(defun sort-concept-list-referencing (tbox)
  (let ((root-nodes (compute-root-nodes-referencing tbox)))
    (loop for concept in (tbox-encoded-concept-list tbox)
          for referencing = (concept-referencing concept)
          when referencing do
          (loop for reference in referencing do
                (add-to-referenced-by concept reference)))
    #+:debug (assert (null (concept-referenced-by (tbox-top-node tbox))))
    (mark-concepts-for-sorting tbox root-nodes))
  (sort-marked-concepts tbox))

(defun sort-for-no-bottom-search (tbox)
  (when (and *sorted-concept-list*
             *fully-sorted-concept-list*
             (null (tbox-meta-constraint-concepts tbox)))
    (setf (tbox-encoded-concept-list tbox)
          (loop with last-no-bottom-search = nil
                for concept in (tbox-encoded-concept-list tbox)
                if (concept-for-no-bottom-search-p tbox concept)
                collect concept into no-bottom-search
                and 
                do 
                (when (concept-visible-p concept)
                  (setf last-no-bottom-search concept))
                else collect concept into bottom-search
                finally
                (when (or *use-optimized-tbox-traversal-no-bottom-search*
                          (tbox-el+-transformed-table tbox))
                  (setf (tbox-end-of-no-bottom-search tbox) last-no-bottom-search))
                (return (nconc no-bottom-search bottom-search))))))

(defun concept-for-no-bottom-search-p (tbox concept)
  (and (concept-primitive-p concept)
       (not (dl-inverse (concept-language concept)))
       (null (concept-encoded-negated-definition concept))
       (concept-visible-p concept)
       (or (not (concept-self-referencing-p concept))
           (null (concept-encoded-definition concept))
           (and *use-elh-model-embedding*
                *use-elh-transformation*
                (tbox-el+-transformed-table tbox)))
       (not (or (sufficient-condition-in-nary-absorption-table-p tbox concept)
                (sufficient-condition-in-domain-qualifications-table-p tbox concept)))))

(defun sufficient-condition-in-nary-absorption-table-p (tbox concept)
  (when (and *use-nary-absorption* tbox)
    (let ((inverse-nary-absorption-table (tbox-inverse-nary-absorption-table tbox)))
      (when inverse-nary-absorption-table
        (gethash concept inverse-nary-absorption-table)))))

(defun sufficient-condition-in-domain-qualifications-table-p (tbox concept)
  (when (and *use-elh-model-embedding* *use-elh-transformation* (tbox-el+-transformed-table tbox))
    (let ((domain-qualifications-table (tbox-domain-qualifications-table tbox)))
      (when domain-qualifications-table
        (gethash concept domain-qualifications-table)))))

(defun compute-root-nodes-full-referencing (tbox)
  (loop for concept in (tbox-encoded-concept-list tbox)
        unless (concept-full-referencing concept)
        collect concept))

(defun sort-concept-list-full-referencing (tbox)
  (let ((root-nodes
         (compute-root-nodes-full-referencing tbox)))
    (loop for concept in (tbox-encoded-concept-list tbox)
          for full-referencing = (concept-full-referencing concept)
          when full-referencing do
          (loop for reference in full-referencing do
                (add-to-referenced-by concept reference)))
    #+:debug (assert (null (concept-referenced-by (tbox-top-node tbox))))
    (mark-concepts-for-sorting tbox root-nodes))
  (sort-marked-concepts tbox))

(defun sort-concept-list-told-subsumers (tbox)
  (let ((root-nodes
         (compute-root-nodes-full-referencing tbox)))
    (loop for concept in (tbox-encoded-concept-list tbox)
          ;for full-referencing = (concept-full-referencing concept)
          for told-subsumers = (concept-told-subsumers concept)
          ;when full-referencing do
          ;(loop for reference in full-referencing do
          ;      (add-to-referenced-by concept reference))
          when (rest told-subsumers) do
          (loop for subsumer in told-subsumers do
                (unless (eq subsumer concept)
                  (add-to-referenced-by concept subsumer))))
    #+:debug (assert (null (concept-referenced-by (tbox-top-node tbox))))
    (mark-concepts-for-sorting tbox root-nodes))
  (sort-marked-concepts tbox))

(defun sort-elh-concept-list (tbox)
  (loop for concept in (tbox-encoded-concept-list tbox)
        for model = (concept-model concept)
        for ancestors = (when (model-info-p model)
                          (model-det-positive-literals model))
        if (rest ancestors)
        do
        (loop for ancestor in ancestors do
              (unless (eq ancestor concept)
                (add-to-referenced-by concept ancestor)))
        else
        collect concept into root-nodes
        finally
        #+:debug (assert (null (concept-referenced-by (tbox-top-node tbox))))
        (mark-concepts-for-sorting tbox root-nodes)
        (sort-marked-concepts tbox)))

(defun add-to-referenced-by (concept reference)
  #+:debug (assert (not (eq reference *top-concept*)))
  (let ((referenced-by (concept-referenced-by reference)))
    (if (and (listp referenced-by) (< (length referenced-by) 50))
      (pushnew concept (concept-referenced-by reference))
      (progn
        (when (listp referenced-by)
          (setf referenced-by
                #+:allegro (racer-make-hash-table :size 100 :rehash-size 2.0 :structure-p t)
                #-:allegro (racer-make-hash-table :size 100))
          (loop for elem in (concept-referenced-by reference) do
                (setf (gethash elem referenced-by) t))
          (setf (concept-referenced-by reference) referenced-by))
        (unless (gethash concept referenced-by)
          (setf (gethash concept referenced-by) t))))))

(defun determine-cyclic-atomic-concepts (tbox)
  (if (and *fully-sorted-concept-list* (not *random-concept-list*))
    (determine-cyclic-atomic-concepts-1 tbox)
    (determine-cyclic-atomic-concepts-2 tbox)))

(defun determine-meta-cyclic-atomic-concepts (meta-concepts)
  (let ((mark (incf *tbox-classification-counter*)))
    (loop for meta-concept in meta-concepts do
          (loop for concept in (concept-full-referencing meta-concept) do
                (determine-meta-cyclic-atomic-concepts-1 concept mark)))))

(defun determine-meta-cyclic-atomic-concepts-1 (concept mark)
  (unless (eql (concept-visited-mark concept) mark)
    (setf (concept-visited-mark concept) mark)
    (setf (concept-self-referencing-p concept) t)
    (loop for concept in (concept-full-referencing concept) do
          (determine-meta-cyclic-atomic-concepts-1 concept mark))))

(defun determine-cyclic-atomic-concepts-1 (tbox)
  (let ((cyclic-concepts nil))
    (let ((meta-concepts (tbox-meta-constraint-concepts tbox)))
      (when meta-concepts
        (when (some #'concept-full-referencing meta-concepts)
          (setf cyclic-concepts t))
        (determine-meta-cyclic-atomic-concepts meta-concepts)))
    (loop with language = (tbox-language tbox)
          for concept in (tbox-encoded-concept-list tbox)
          for referenced-by = (concept-referenced-by concept)
          for self-p = (or (concept-self-referencing-p concept)
                           (when referenced-by
		             (find-referenced-by-concept concept
                                                         referenced-by                                                   
                                                         (incf *tbox-classification-counter*))))
          do
	  (setf language (union-dl-descriptors (concept-language concept) language))
	  (when self-p
	    (unless cyclic-concepts
	      (setf cyclic-concepts t))
	    (setf (concept-self-referencing-p concept) t))
          finally
	  (setf (tbox-language tbox) language)
	  (when cyclic-concepts
	    (setf (tbox-blocking-possibly-required tbox) t)))))

(defmacro find-referenced-by-concept-body (elem concept mark)
  `(unless (eql (concept-visited-mark ,elem) ,mark)
     (setf (concept-visited-mark ,elem) ,mark)
     (let ((elem-referenced-by (concept-referenced-by ,elem)))
       (when elem-referenced-by
         (or (if (listp elem-referenced-by)
               (member ,concept elem-referenced-by)
               (gethash ,concept elem-referenced-by))
             (find-referenced-by-concept ,concept elem-referenced-by ,mark))))))

(defun find-referenced-by-concept (concept referenced-by mark)
  (when referenced-by
    (if (listp referenced-by)
      (loop for elem in referenced-by
            thereis
            (find-referenced-by-concept-body elem concept mark))
      (loop for elem being the hash-key of referenced-by
            thereis
            (find-referenced-by-concept-body elem concept mark)))))

(defun determine-cyclic-atomic-concepts-2 (tbox)
  (let* ((referencing-only (not *fully-sorted-concept-list*))
         (meta-constraint-concepts-referencing
          (concept-set-remove-duplicates
           (loop for concept in (tbox-meta-constraint-concepts tbox)
                 append (if referencing-only
                          (concept-referencing concept)
                          (concept-full-referencing concept))))))
    (loop with language = (tbox-language tbox)
          with cyclic-concepts = nil
          for concept in (tbox-encoded-concept-list tbox)
          do (setf language (union-dl-descriptors (concept-language concept) language))
          if (concept-self-referencing-p concept)
          do (unless cyclic-concepts
               (setf cyclic-concepts t))
          else
          when (if referencing-only
                 (let ((elem-referencing
                        (concept-set-union (concept-referencing concept)
                                           meta-constraint-concepts-referencing)))
                   (when elem-referencing
                     (or (member concept elem-referencing)
                         (find-referenced-concept concept
                                                  elem-referencing
                                                  (incf *tbox-classification-counter*)))))
                 (let ((elem-fully-referencing
                        (concept-set-union (concept-full-referencing concept)
                                           meta-constraint-concepts-referencing)))
                   (when elem-fully-referencing
                     (or (member concept elem-fully-referencing)
                         (find-fully-referenced-concept concept
                                                        elem-fully-referencing
                                                        (incf *tbox-classification-counter*))))))
          do
          (setf (concept-self-referencing-p concept) t)
          (unless cyclic-concepts
            (setf cyclic-concepts t))
          finally
          (when cyclic-concepts
            (setf (tbox-blocking-possibly-required tbox) t))
          (setf (tbox-language tbox) language))))

(defun find-referenced-concept (concept referencing mark)
  (loop for elem in referencing
        thereis
        (unless (eql (concept-visited-mark elem) mark)
          (setf (concept-visited-mark elem) mark)
          (let ((elem-referencing (concept-referencing elem)))
            (when elem-referencing
              (or (member concept elem-referencing)
                  (find-referenced-concept concept elem-referencing mark)))))))

(defun find-fully-referenced-concept (concept referencing mark)
  (loop for elem in referencing
        thereis
        (unless (eql (concept-visited-mark elem) mark)
          (setf (concept-visited-mark elem) mark)
          (let ((elem-fully-referencing (concept-full-referencing elem)))
            (when elem-fully-referencing
              (or (member concept elem-fully-referencing)
                  (find-fully-referenced-concept concept elem-fully-referencing mark)))))))

(defun build-tbox-structures (tbox stream reasoning-mode)
  (let* ((*sorted-concept-list*
          (or (and *sorted-concept-list* (not *random-concept-list*))
              (or *told-subsumer-sorted-concept-list*
                  (and (tbox-el+-transformed-table tbox)
                       *use-elh-transformation*
                       *use-elh-model-embedding*))))
         (*concept-changed-p* nil)
         (incoherent-p
          (unless (tbox-index-structures-complete-p tbox)
            (setf (tbox-identification tbox) (gensym))
            (unless (tbox-coherence-checked-p tbox)
              (race-time (pre-build-tbox-structures tbox stream reasoning-mode))))))
    (convert-role-descendants tbox)
    (race-time (classify-tbox-concepts tbox stream reasoning-mode incoherent-p))
    (tbox-coherent-p-internal tbox)))

(defun convert-role-descendants (tbox)
  (loop for role in (tbox-encoded-role-list tbox)
        for role-inverse = (role-inverse-internal role)
        do
        (when (hash-table-p (role-descendants-internal role))
          (setf (role-descendants-internal role)
                (loop for key being the hash-key of (role-descendants-internal role)
                      collect key)))
        (when (and role-inverse (hash-table-p (role-descendants-internal role-inverse)))
          (setf (role-descendants-internal role-inverse)
                (loop for key being the hash-key of (role-descendants-internal role-inverse)
                      collect key)))))

(defun pre-build-tbox-structures (tbox stream reasoning-mode)
  (let ((top-bottom-p (check-meta-concepts tbox stream))
        (saved-concept-list (tbox-encoded-concept-list tbox)))
    (multiple-value-bind (incoherent-p roles-classified-p role-synonyms-changed-p)
        (classify-role-hierarchy tbox stream reasoning-mode top-bottom-p)
      (unless incoherent-p
        (when roles-classified-p
          (setf (tbox-encoded-concept-list tbox)
                (append (tbox-encoded-concept-list tbox) saved-concept-list))
          (when role-synonyms-changed-p
            (convert-role-descendants tbox)
            (reencode-all-quantification-concepts tbox)))
        (loop with language = (tbox-language tbox)
              for concept in (tbox-encoded-concept-list tbox) do
              (set-language concept)
              (setf language (union-dl-descriptors (concept-language concept) language))
              finally 
              (setf (tbox-language tbox) language)
              (when (tbox-el+-environment tbox)
                (setf (env-tbox-language (tbox-el+-environment tbox)) language)))
        (sort-concepts tbox reasoning-mode))
      incoherent-p)))

(defun check-meta-concepts (tbox stream)
  (when *debug*
    (format stream "~&Testing satisfiability of meta constraints..."))
  (setf (concept-model (tbox-top-node tbox)) nil)
  (setf (tbox-meta-constraint-concepts tbox)
        (loop with mark = (incf *tbox-classification-counter*)
              with top = (tbox-top-node tbox)
              with bottom = (tbox-bottom-node tbox)
              for concept-term in (tbox-literal-meta-constraints tbox)
              for concept = (encode-concept-term concept-term)
              if (eq concept bottom)
              do
              (return (list bottom))
              else
              unless (eq concept top)
              collect concept
              and
              do (update-encoded-definition tbox concept top bottom mark t t)
              end
              end
              do (setf (tbox-language tbox) 
                       (union-dl-descriptors (concept-language concept)
                                             (tbox-language tbox)))))
  (setf (tbox-literal-meta-constraints tbox)
        (mapcar #'decode-concept (tbox-meta-constraint-concepts tbox)))
  (when (tbox-meta-constraint-concepts tbox)
    (setf (tbox-meta-constraint-concept tbox)
          (encode-concept-term `(and .,(tbox-meta-constraint-concepts tbox)))))
  (if (eq (first (tbox-meta-constraint-concepts tbox)) (tbox-bottom-node tbox))
      t
    (with-alc-bindings
      (let* ((*meta-constraint-concepts* (tbox-meta-constraint-concepts tbox))
             (*blocking-possibly-required*
              (or *encode-roles-as-transitive*
                  *meta-constraint-concepts*
                  (tbox-blocking-possibly-required tbox)))
             (start (when *debug*
                      (get-internal-run-time)))
             (top-bottom-p
              (unless (tbox-el+-transformed-table tbox)
                (incoherent-model-p (race-time (get-cached-concept-model (tbox-top-node tbox))))))
             (time (when *debug*
                     (/ (- (get-internal-run-time) start)
                        internal-time-units-per-second))))
        (when *debug*
          (format stream "~S (~,3F secs)" (not top-bottom-p) time))
        top-bottom-p))))

(defun use-new-role-classification-p (tbox)
  (when (and *use-new-role-hierarchy-classification*
             (not (tbox-el+-transformed-table tbox)))
    (let ((language (tbox-language tbox)))
      (and (dl-simple-role-inclusions language)
           (or (dl-complex-role-inclusions language)
               (and (or (dl-features language)
                        (dl-merging language))
                    (at-least-two-feature-children-p tbox)))))))

(defun at-least-two-feature-children-p (tbox)
  (loop for role in (tbox-encoded-role-list tbox)
        for role-inverse = (role-inverse-internal role)
        thereis 
        (and (not (or (is-predefined-role-p role)
                      (role-datatype role)
                      (role-cd-attribute role)
                      (role-internal-conjunction-p role)))
             (or (role-feature-p role)
                 (and role-inverse (role-feature-p role-inverse)))
             (rest (role-children-internal role))
             role)))

(defun has-feature-as-sibling (role)
  (loop for parent in (role-ancestors-internal role)
        unless (or (eq parent role) (is-predefined-role-p parent)) do
        (loop for child in (role-descendants-internal parent)
              when (and (not (eq child role))
                        (not (eq child parent))
                        (role-feature-p child))
              do (return-from has-feature-as-sibling t))))

(defun classify-role-hierarchy (tbox stream reasoning-mode top-bottom-p)
  (with-race-trace-sublevel ("classify-role-hierarchy"
                             :arguments (list tbox reasoning-mode top-bottom-p)
                             :trace-result t
                             :expanded nil)
    (when *debug*
      (format stream "~&Classifying role hierarchy..."))
    (with-alc-bindings
      (let* ((*meta-constraint-concepts* (tbox-meta-constraint-concepts tbox))
             (*blocking-possibly-required*
              (or *encode-roles-as-transitive*
                  *meta-constraint-concepts*
                  (tbox-blocking-possibly-required tbox)))
             (roles-classified-p nil)
             (synonyms-changed-p nil)
             (start (when *debug*
                      (get-internal-run-time)))
             (top-role-bottom-p
              (progn
                (unless top-bottom-p
                  (without-taxonomic-encoding
                    (let ((*tbox-clustering* nil))
                      (multiple-value-bind (concept-name-to-role
                                            concept-to-concept-name
                                            role-to-concept-name
                                            marker-name
                                            marker-concept)
                          (mirror-role-to-concept-hierarchy-1 tbox)
                        (test-satisfiability-of-roles tbox
                                                      stream
                                                      reasoning-mode
                                                      role-to-concept-name
                                                      t)
                        (let ((tbox-inverse-roles-p (or (dl-inverse-roles (tbox-language tbox))
                                                        (critical-inverse-role-in-tbox-p tbox))))
                          (mirror-role-to-concept-hierarchy-2 tbox
                                                              tbox-inverse-roles-p
                                                              concept-name-to-role
                                                              role-to-concept-name
                                                              concept-to-concept-name
                                                              marker-concept
                                                              marker-name)
                          (test-satisfiability-of-roles tbox
                                                        stream
                                                        reasoning-mode
                                                        role-to-concept-name
                                                        nil)
                          (if (and concept-name-to-role (use-new-role-classification-p tbox))
                              (progn
                                (sort-concepts tbox reasoning-mode concept-to-concept-name)
                                (race-time (classify-tbox-concepts tbox
                                                                   stream 
                                                                   ':classify-internal-concept-only
                                                                   nil
                                                                   concept-to-concept-name))
                                (setf synonyms-changed-p
                                      (mirror-concept-to-role-hierarchy tbox
                                                                        stream
                                                                        concept-to-concept-name
                                                                        concept-name-to-role
                                                                        tbox-inverse-roles-p))
                                (setf roles-classified-p t))
                            (progn
                              (add-missing-role-parents-children tbox)
                              (when concept-name-to-role
                                (remove-obsolete-concepts tbox concept-name-to-role concept-to-concept-name)))))))))
                (setf (tbox-role-hierarchy-classified-p tbox) t)
                (let ((new-object-bottoms
                       (rest (role-synonyms-internal (tbox-object-bottom-role tbox))))
                      (new-datatype-bottoms
                       (rest (role-synonyms-internal (tbox-datatype-bottom-role tbox)))))
                  (when (or new-object-bottoms new-datatype-bottoms)
                    (reencode-all-exists-cd-concepts tbox
                                                     (null new-datatype-bottoms)
                                                     (null new-object-bottoms))))
                (member (tbox-object-top-role tbox)
                        (role-synonyms-internal (tbox-object-bottom-role tbox)))))
             (time (when *debug*
                     (/ (- (get-internal-run-time) start)
                        internal-time-units-per-second))))
        (when *debug*
          (format stream "~S (~,3F secs)" (not top-role-bottom-p) time))
        (if top-role-bottom-p
            (progn
              (setf (tbox-meta-constraint-concepts tbox) (list (tbox-bottom-node tbox)))
              (tbox-classification-report stream ':top-object-role-incoherent (tbox-name tbox)))
          (when top-bottom-p
            (tbox-classification-report stream ':all-concepts-incoherent (tbox-name tbox))))
        (values (or top-bottom-p top-role-bottom-p) roles-classified-p synonyms-changed-p)))))

(defun add-missing-role-parents-children (tbox)
  (loop with object-top = (tbox-object-top-role tbox)
        with object-bottom = (tbox-object-bottom-role tbox)
        with data-top = (tbox-datatype-top-role tbox)
        with data-bottom = (tbox-datatype-bottom-role tbox)
        with new-object-top-children = nil
        with new-object-bottom-parents = nil
        with new-data-top-children = nil
        with new-data-bottom-parents = nil
        for role in (tbox-encoded-role-list tbox)
        for parents = (role-parents-internal role)
        for children = (role-children-internal role)
        do
        (unless (or (is-top-object-role-p role) (is-bottom-object-role-p role)
                    (is-top-datatype-role-p role) (is-bottom-datatype-role-p role))
          (if (or (role-datatype role) (role-cd-attribute role))
              (progn
                (unless parents
                  (setf (role-parents-internal role) (list data-top))
                  (push role new-data-top-children))
                (unless children
                  (setf (role-children-internal role) (list data-bottom))
                  (push role new-data-bottom-parents)))
            (progn
              (unless parents
                (setf (role-parents-internal role) (list object-top))
                (push role new-object-top-children))
              (unless children
                (setf (role-children-internal role) (list object-bottom))
                (push role new-object-bottom-parents)))))
        finally
        (when new-object-top-children
          (setf (role-children-internal object-top)
                (role-set-union new-object-top-children (role-children-internal object-top))))
        (when new-object-bottom-parents
          (setf (role-parents-internal object-bottom)
                (role-set-union new-object-bottom-parents (role-parents-internal object-bottom))))
        (when new-data-top-children
          (setf (role-children-internal data-top)
                (role-set-union new-data-top-children (role-children-internal data-top))))
        (when new-data-bottom-parents
          (setf (role-parents-internal data-bottom)
                (role-set-union new-data-bottom-parents (role-parents-internal data-bottom))))))

(defun sort-concepts (tbox reasoning-mode &optional (mirrored-concept-table nil))
  (let ((tbox-el+-p (and *use-elh-model-embedding*
                         *fully-sorted-concept-list* 
                         (subset-el+-p (tbox-language tbox)))))
    (if (and tbox-el+-p
             *use-elh-transformation*
             *use-elh-model-embedding*
             (eq reasoning-mode ':classification))
        (with-alc-bindings
          (let* ((*meta-constraint-concepts* (tbox-meta-constraint-concepts tbox))
                 (*blocking-possibly-required*
                  (or *encode-roles-as-transitive*
                      *meta-constraint-concepts*
                      (tbox-blocking-possibly-required tbox)))
                 (concepts (tbox-encoded-concept-list tbox))
                 #+:ignore
                 (n-concepts (length concepts)))
            (loop for concept in concepts 
                  when (or (not mirrored-concept-table)
                           (gethash (concept-hash-id concept) mirrored-concept-table))
                  do
                  (setf (concept-sort-no concept) nil)))
          (race-time (sort-encoded-concept-list tbox mirrored-concept-table t)))
      (progn
        (loop with nominal-table = (tbox-nominal-table tbox)
              for concept in (tbox-encoded-concept-list tbox)
              do
              (when (loop for name in (concept-name-set concept) 
                          thereis
                          (eq (gethash name nominal-table) ':nominal))
                (setf (concept-visible-p concept) nil)))
        (race-time (sort-encoded-concept-list tbox mirrored-concept-table))))
    (race-time (determine-cyclic-atomic-concepts tbox))
    (when (and (not mirrored-concept-table) (tbox-use-less-memory tbox))
      (clean-tbox tbox)))
  #+:debug (tbox-encoded-concept-list tbox))

(defun classify-tbox-concepts (tbox stream reasoning-mode incoherent-p &optional (mirrored-concept-table nil))
  (let ((internal-only (and mirrored-concept-table (eq reasoning-mode :classify-internal-concept-only))))
    (when (or incoherent-p
              (and (eq reasoning-mode :classification) (not (tbox-classified-p-internal tbox)))
              (and (or (eq reasoning-mode :coherence-only) internal-only)
                   (not (tbox-coherence-checked-p tbox))))
      (with-race-trace-sublevel ("classify-tbox-concepts"
                                 :arguments (list tbox reasoning-mode incoherent-p mirrored-concept-table)
                                 :expanded nil)
        (let ((top (tbox-top-node tbox))
              (bottom (tbox-bottom-node tbox))
              (tbox-index-structures-complete-p (tbox-index-structures-complete-p tbox))
              (use-less-tbox-memory (tbox-use-less-memory tbox)))
          (if incoherent-p
              (progn
	    ;(add-concept-synonym tbox (concept-name-set bottom) top t t) ;we don't need this really
                (add-concept-synonym tbox (concept-name-set top) bottom t t)
                (loop for concept in (tbox-encoded-concept-list tbox) do
                      (add-concept-synonym tbox (concept-name-set concept) bottom t t))
                (setf (tbox-coherent-p-internal tbox) nil)
                (race-trace ("~A" (print-tbox-info tbox nil))))
            (let ((tbox-el+-possible (and *use-elh-transformation*
                                          *use-elh-model-embedding*
                                          *fully-sorted-concept-list*
                                          (not *random-concept-list*)
                                          (tbox-el+-transformed-table tbox))))
              (if (and (not tbox-el+-possible)
                       (member reasoning-mode '(:classification :classify-internal-concept-only)))
                  (sort-for-no-bottom-search tbox)
                (when (tbox-encoded-concept-list tbox)
                  (loop for concept in (reverse (tbox-encoded-concept-list tbox)) do
                        (when (concept-visible-p concept)
                          (setf (tbox-end-of-no-bottom-search tbox) concept)
                          (return))
                        finally
                        (when internal-only
                          (error "no end-of-no-bottom-search concept encountered")))))
              (race-trace ("~A" (print-tbox-info tbox nil)))
              (loop with n-concepts = (if internal-only 
                                          0
                                        (loop with count = 0
                                              for concept in (tbox-encoded-concept-list tbox)
                                              when (or (concept-visible-p concept)
                                                       (concept-encoded-definition concept))
                                              do (incf count)
                                              finally (return count)))
                    with show-progress = (> n-concepts *minimum-n-concepts-for-classification-progress-indication*)
                    with reencoding-necessary = (or (not (tbox-empty-taxonomic-encoding-table tbox))
                                                    (tbox-meta-constraint-concepts tbox))
                    with changed-mark = (incf *tbox-classification-counter*)
                    with tbox-l-minus-possible = (and *use-completely-defined-tbox-traversal*
                                                      *sorted-concept-list*
                                                      (not *random-concept-list*)
                                                      *fully-sorted-concept-list*
                                                      (null (tbox-meta-constraint-concepts tbox)))
                    for concept in (tbox-encoded-concept-list tbox)
                    for mirrored-role-concept-p = (and internal-only
                                                       (gethash concept mirrored-concept-table))
                    when (or (and (not internal-only) (concept-visible-p concept))
                             mirrored-role-concept-p)
                    do
                    (when (and (not tbox-index-structures-complete-p) reencoding-necessary)
                      (let* ((definition (concept-definition concept))
                             (encoded-definition (if definition
                                                     (with-enforced-reencoding
                                                       (encode-concept-term definition concept))
                                                   top)))
                        (setf (concept-told-subsumers concept) ;  BEWARE OF DUPLICATES!!!
                              (remove-duplicates 
                               (safe-append
                                (list*
                                 (concept-told-subsumers concept)
                                 (concept-told-subsumers encoded-definition)
                                 (mapcar #'concept-told-subsumers
                                         (tbox-meta-constraint-concepts tbox))))))
                        (setf (concept-told-disjoints concept)
                              (concept-set-union (concept-asserted-disjoints concept)
                                                 (concept-told-disjoints concept)))
                        (unless (concept-primitive-p concept)
                          (pushnew concept (concept-told-subsumers encoded-definition)))))
                    (set-language concept changed-mark)
                    (setf (tbox-language tbox)
                          (union-dl-descriptors (concept-language concept) (tbox-language tbox)))
                    (when (or mirrored-role-concept-p
                              (and (not internal-only) 
                                   (or (concept-visible-p concept) (concept-encoded-definition concept))))
                      (case reasoning-mode
                        (:coherence-only
                         (unless internal-only
                           (incf *n-inserted-concepts*)
                           (when show-progress
                             (funcall *tick-function* (/ *n-inserted-concepts* n-concepts))))
                         (unless (test-atomic-concept-satisfiable tbox concept)
                           (when (concept-visible-p concept)
                             (tbox-classification-report stream ':concept-incoherent 
                                                         (concept-name-set concept)
                                                         (tbox-name tbox)))
                           (add-concept-synonym tbox (concept-name-set concept) bottom nil t)
                           (setf incoherent-p t)))
                        ((:classification :classify-internal-concept-only)
                         (unless internal-only
                           (incf *n-inserted-concepts*)
                           (when show-progress
                             (funcall *tick-function* (/ *n-inserted-concepts* n-concepts))))
                         (insert-concept-into-tbox concept
                                                       tbox
                                                       tbox-el+-possible
                                                       tbox-l-minus-possible
                                                       (eq reasoning-mode ':classify-internal-concept-only)
                                                       stream)
                         (when use-less-tbox-memory
                           (setf (concept-definition concept) nil))
                         (when (eq (get-tbox-concept tbox (concept-term concept))
                                   bottom)
                           (setf incoherent-p t)))
                        (:setup
                         (setf incoherent-p ':dont-know))))
                    finally
                    (setf (concept-name-set top) (racer-remove-duplicates (concept-name-set top)))
                    (setf (concept-name-set bottom) (racer-remove-duplicates (concept-name-set bottom)))
                    (if (eq incoherent-p ':dont-know)
                        (setf (tbox-coherent-p-internal tbox) ':dont-know)
                      (setf (tbox-coherent-p-internal tbox) (not incoherent-p)))))))))))

(defun create-role-mirror-concept (tbox role marker-name concept-name-to-role role-to-concept-name)
  (let ((mirror-concept-name
         (intern (concatenate 'string (symbol-name (role-name role)) "-" marker-name)))
        (synonyms (role-synonyms-internal role)))
    (create-tbox-internal-marker-concept tbox mirror-concept-name)
    (setf (gethash role role-to-concept-name) mirror-concept-name)
    (setf (gethash mirror-concept-name concept-name-to-role) role)
    (when (rest synonyms)
      (loop for synonym in synonyms do
            (unless (eq synonym role)
              (let ((synonym-mirror-concept-name
                     (intern (concatenate 'string (symbol-name (role-name synonym)) "-" marker-name))))
                (create-tbox-internal-marker-concept tbox synonym-mirror-concept-name)
                (setf (gethash synonym role-to-concept-name) synonym-mirror-concept-name)
                (setf (gethash synonym-mirror-concept-name concept-name-to-role) synonym)))))))

(defun mirror-role-to-concept-hierarchy-1 (tbox)
  (with-race-trace-sublevel ("mirror-role-to-concept-hierarchy-1"
                             :arguments (list tbox)
                             :trace-result t
                             :expanded t)
    (without-taxonomic-encoding
      (let* ((table-size (length (tbox-encoded-role-list tbox)))
             (concept-name-to-role (racer-make-hash-table :size table-size))
             (role-to-concept-name (racer-make-hash-table :size table-size))
             (concept-to-concept-name (racer-make-hash-table :size table-size))
             (marker-concept (create-tbox-internal-marker-concept tbox))
             (marker-name (symbol-name (gensym))))
        (loop for role in (tbox-encoded-role-list tbox) do
              (unless (ignore-role-p role nil)
                (create-role-mirror-concept tbox role marker-name concept-name-to-role role-to-concept-name)))
        (internalize-role-mirror-concepts tbox nil marker-concept concept-to-concept-name role-to-concept-name)
        (values concept-name-to-role
                concept-to-concept-name
                role-to-concept-name
                marker-name
                marker-concept)))))

(defun mirror-role-to-concept-hierarchy-2 (tbox
                                           tbox-inverse-roles-p
                                           concept-name-to-role
                                           role-to-concept-name
                                           concept-to-concept-name
                                           marker-concept
                                           marker-name)
  (with-race-trace-sublevel ("mirror-role-to-concept-hierarchy-2"
                             :arguments (list tbox
                                              tbox-inverse-roles-p
                                              concept-name-to-role
                                              role-to-concept-name
                                              concept-to-concept-name
                                              marker-concept
                                              marker-name)
                             :trace-result t
                             :expanded t)
    (without-taxonomic-encoding
      (loop for role in (tbox-encoded-role-list tbox) do
            (unless (or (gethash role role-to-concept-name)
                        (ignore-role-p role tbox-inverse-roles-p))
              (create-role-mirror-concept tbox role marker-name concept-name-to-role role-to-concept-name)))
      (internalize-role-mirror-concepts tbox
                                        tbox-inverse-roles-p
                                        marker-concept
                                        concept-to-concept-name
                                        role-to-concept-name)
      (when (> (hash-table-count concept-to-concept-name) 0)
        (setf (tbox-encoded-concept-list tbox)
              (nconc (loop for concept being the hash-key of concept-to-concept-name
                           collect concept)
                     (tbox-encoded-concept-list tbox)))))))

(defun ignore-role-p (role tbox-inverse-roles-p)
  (or (is-predefined-role-p role)
      (role-datatype role)
      (role-cd-attribute role)
      (role-annotation-p role)
      (ignore-inverse-p role tbox-inverse-roles-p)))

(defun ignore-inverse-p (role tbox-inverse-roles-p)
  (and (role-internal-name-p role)
       (or (not tbox-inverse-roles-p)
           (let ((inverse-role (role-inverse-internal role)))
             (and inverse-role
                  (or (role-datatype inverse-role)
                      (role-cd-attribute inverse-role)))))))

(defun critical-inverse-role-in-tbox-p (tbox)
  (loop for role in (tbox-encoded-role-list tbox)
        thereis
        (and (not (role-internal-name-p role))
             (not (or (is-predefined-role-p role)
                      (role-datatype role)
                      (role-cd-attribute role)
                      (role-annotation-p role)))
             (let ((inverse-role (role-inverse-internal role)))
               (and inverse-role
                    (not (eq role inverse-role))
                    (not (role-internal-name-p inverse-role))
                    (or (role-compositions role)
                        (and (or (role-feature-p role)
                                 (role-feature-p inverse-role))
                             (rest (role-children-internal role))))
                    #+:debug
                    (cons role inverse-role))))))

(defun internalize-role-mirror-concepts (tbox
                                         tbox-inverse-roles-p
                                         marker-concept
                                         concept-to-concept-name
                                         role-to-concept-name)
  (loop for role in (tbox-encoded-role-list tbox) do
        (unless (or (role-removed-p role)
                    (get-tbox-concept tbox (gethash role role-to-concept-name) nil)
                    (ignore-role-p role tbox-inverse-roles-p))
          (internalize-role-mirror-concept tbox
                                           role
                                           marker-concept
                                           concept-to-concept-name
                                           role-to-concept-name))))

(defun internalize-role-mirror-concept (tbox
                                        role
                                        marker-concept
                                        concept-to-concept-name
                                        role-to-concept-name)
  (let* ((synonyms (role-synonyms-internal role))
         (mirror-concept-name (gethash role role-to-concept-name))
         (definition `(some ,role ,marker-concept))
         (concept (initialize-atomic-concept tbox mirror-concept-name definition nil)))
    #+:debug (assert mirror-concept-name)
    (setf (concept-encoded-definition concept) (encode-concept-term definition concept))
    (update-encoded-definition tbox
                               concept
                               (tbox-top-node tbox)
                               (tbox-bottom-node tbox)
                               (incf *tbox-classification-counter*)
                               t
                               (incf *tbox-classification-counter*))
    (setf (concept-visible-p concept) nil)
    (setf (gethash concept concept-to-concept-name) mirror-concept-name)
    (when (rest synonyms)
      (loop with top = (tbox-top-node tbox)
            with bottom = (tbox-bottom-node tbox)
            for synonym in synonyms do
            (unless (eq synonym role)
              (let* ((synonym-mirror-concept-name (gethash synonym role-to-concept-name))
                     (synonym-concept
                      (initialize-atomic-concept tbox synonym-mirror-concept-name mirror-concept-name nil)))
                (setf (concept-encoded-definition synonym-concept)
                      (encode-concept-term mirror-concept-name synonym-mirror-concept-name))
                (update-encoded-definition tbox
                                           synonym-concept
                                           top
                                           bottom
                                           (incf *tbox-classification-counter*)
                                           t
                                           (incf *tbox-classification-counter*))
                (setf (concept-visible-p synonym-concept) nil)
                (setf (gethash synonym-concept concept-to-concept-name) synonym-mirror-concept-name)))))
    concept))

(defun partially-reset-role (role)
  (setf (role-parents-internal role) nil)
  (setf (role-children-internal role) nil)
  (setf (role-ancestors-internal role) nil)
  (setf (role-descendants-internal role) nil)
  (setf (role-feature-ancestors role) nil)
  (setf (role-has-feature-ancestors-p role) nil)
  (setf (role-has-ancestors-p role) nil))

(defun mirror-concept-to-role-hierarchy (tbox
                                         stream
                                         concept-to-concept-name
                                         concept-name-to-role
                                         tbox-inverse-roles-p)
  (with-race-trace-sublevel ("mirror-concept-to-role-hierarchy"
                             :arguments (list tbox
                                              stream
                                              concept-to-concept-name
                                              concept-name-to-role
                                              tbox-inverse-roles-p)
                             :trace-result t
                             :expanded t)
    (let ((synonyms-changed-p nil))
      (loop for mirror-concept-name being the hash-value of concept-to-concept-name
            for role = (gethash mirror-concept-name concept-name-to-role)
            for inverse-role = (role-inverse-internal role)
            do
            #+:debug (assert role)
            (partially-reset-role role)
            (unless (or (null inverse-role) (eq role inverse-role))
              (partially-reset-role inverse-role)))
      (loop with top = (tbox-object-top-role tbox)
            with bottom = (tbox-object-bottom-role tbox)
            for mirror-concept-name being the hash-value of concept-to-concept-name
            for mirror-concept = (get-tbox-concept tbox mirror-concept-name)
            for synonyms-p = (rest (concept-name-set mirror-concept))
            for synonyms = (when synonyms-p
                             (racer-remove-duplicates
                              (mapcar (lambda (symbol)
                                        (or (gethash symbol concept-name-to-role)
                                            (and (member symbol (list +bottom-symbol+ +krss-bottom-symbol+))
                                                 bottom)
                                            (and (member symbol (list +top-symbol+ +krss-top-symbol+))
                                                 top)
                                            (error "unexpected")))
                                      (concept-name-set mirror-concept))))
            for synomym-proxy = (first synonyms)
            do
            (when synonyms-p
              (unless (or synonyms-changed-p (role-removed-p synomym-proxy))
                (setf synonyms-changed-p
                      (not (and (eql (length synonyms)
                                     (length (role-synonyms-internal synomym-proxy)))
                                (role-set-subsetp synonyms
                                                  (role-synonyms-internal synomym-proxy))))))
              (loop while (not (role-removed-p synomym-proxy))
                    for synonym in (rest synonyms) do
                    (unless (role-removed-p synonym)
                      (collapse-roles-new synomym-proxy synonym tbox stream nil)))))
      (mirror-concepts-to-roles tbox concept-to-concept-name concept-name-to-role tbox-inverse-roles-p)
      (loop for role being the hash-value of concept-name-to-role
            for inverse-role = (role-inverse-internal role)
            do
            (encode-role tbox role)
            (when (role-datatype inverse-role)
              (setf (role-children-internal inverse-role)
                    (mapcar #'role-inverse-internal (role-children-internal role)))
              (setf (role-parents-internal inverse-role)
                    (mapcar #'role-inverse-internal (role-parents-internal role))))
            (encode-role tbox inverse-role))
      (remove-obsolete-concepts tbox concept-name-to-role concept-to-concept-name)
      synonyms-changed-p)))

(defun mirror-concepts-to-roles (tbox concept-to-concept-name concept-name-to-role tbox-inverse-roles-p)
  (flet ((concept-to-role (concept)
           (let ((result (gethash (gethash concept concept-to-concept-name) concept-name-to-role)))
             #+:debug (assert result)
             result)))
    (loop with top-role = (tbox-object-top-role tbox)
          with top-children = nil
          with bottom-role = (tbox-object-bottom-role tbox)
          with bottom-parents = nil
          for mirror-concept-name being the hash-value of concept-to-concept-name
          for mirror-concept = (get-tbox-concept tbox mirror-concept-name)
          for role = (gethash mirror-concept-name concept-name-to-role)
          unless (or (role-removed-p role) (ignore-inverse-p role tbox-inverse-roles-p))
          do
          #+:debug 
          (assert (or (role-removed-p role)
                      (and (eql (length (concept-name-set mirror-concept))
                                (length (role-synonyms-internal role)))
                           (subsetp (mapcar (lambda (symbol) (gethash symbol concept-name-to-role))
                                            (concept-name-set mirror-concept))
                                    (role-synonyms-internal role))))
              (role mirror-concept)
            "synonyms ~S of mirror concept ~S do not match synonyms ~S of corresponding role ~S"
            (concept-name-set mirror-concept) mirror-concept (role-synonyms-internal role) role)
          (loop with parents = (racer-remove top-role (role-parents-internal role))
                for parent in (concept-parents-internal mirror-concept) do
                (unless (is-top-concept-p parent)
                  (push (concept-to-role parent) parents))
                (when (member nil parents) (break))
                finally
                (let ((inverse-role (role-inverse-internal role)))
                  (if parents
                      (progn
                        (setf parents (role-set-remove-duplicates parents))
                        (setf (role-parents-internal role) parents)
                        (unless (or tbox-inverse-roles-p (eq role inverse-role))
                          (setf (role-parents-internal inverse-role)
                                (mapcar #'role-inverse-internal parents)))
                        #+:debug 
                        (assert (or (null (rest parents))
                                    (and (eql (length (concept-parents-internal mirror-concept))
                                              (length (role-parents-internal role)))
                                         (subsetp (mapcar #'concept-to-role
                                                          (concept-parents-internal mirror-concept))
                                                  (role-parents-internal role))))
                            (role mirror-concept)
                          "parents ~S of mirror concept ~S do not match parents ~S of corresponding role ~S"
                          (concept-parents-internal mirror-concept) mirror-concept (role-parents-internal role) role)
                        #+:debug (assert (or (not (rest parents)) (not (member top-role (role-parents-internal role)))))
                        )
                    (progn
                      (setf (role-parents-internal role) (list top-role))
                      (push role top-children)
                      (unless (or tbox-inverse-roles-p (eq role inverse-role))
                        (setf (role-parents-internal (role-inverse-internal role)) (list top-role))
                        (push inverse-role top-children))))))
          (loop with children = (racer-remove bottom-role (role-children-internal role))
                for child in (concept-children-internal mirror-concept) do
                (unless (is-bottom-concept-p child)
                  (push (concept-to-role child) children))
                finally
                (let ((inverse-role (role-inverse-internal role)))
                  (if children
                      (progn
                        (setf children (role-set-remove-duplicates children))
                        (setf (role-children-internal role) children)
                        (unless (or tbox-inverse-roles-p (eq role inverse-role))
                          (setf (role-children-internal inverse-role) 
                                (mapcar #'role-inverse-internal children)))
                        #+:debug 
                        (assert (or (null (rest children))
                                    (and (eql (length (concept-children-internal mirror-concept))
                                              (length (role-children-internal role)))
                                         (subsetp (mapcar #'concept-to-role
                                                          (concept-children-internal mirror-concept))
                                                  (role-children-internal role))))
                            (role mirror-concept)
                          "children ~S of mirror concept ~S do not match children ~S of corresponding role ~S"
                          (concept-children-internal mirror-concept) mirror-concept (role-children-internal role) role)
                        #+:debug (assert (or (not (rest children)) (not (member bottom-role (role-children-internal role)))))
                        )
                    (progn
                      (setf (role-children-internal role) (list bottom-role))
                      (push role bottom-parents)
                      (unless (or tbox-inverse-roles-p (eq role inverse-role))
                        (setf (role-children-internal (role-inverse-internal role)) (list bottom-role))
                        (push inverse-role bottom-parents))))))
          #+:debug (assert (and (role-parents-internal role) (role-children-internal role)))
          finally
          (setf (role-children-internal top-role) (role-set-remove-duplicates top-children))
          (setf (role-parents-internal bottom-role) (role-set-remove-duplicates bottom-parents)))))

(defun remove-obsolete-concepts (tbox concept-name-to-role concept-to-concept-name)
  (setf (tbox-encoded-concept-list tbox)
        (loop with internal-marker-concepts = (tbox-internal-marker-concepts tbox)
              for concept in (tbox-encoded-concept-list tbox)
              when (or (concept-visible-p concept)
                       ;; an invisible concept created by the EL+ transformation needs to be preserved
                       ;; it can be recognized because it is not recorded as internal marker concept
                       (not (gethash (first (concept-name-set concept)) internal-marker-concepts)))
              collect concept))
  (clean-top-bottom-concepts tbox concept-name-to-role)
  (remove-obsolete-children tbox (tbox-top-node tbox) concept-to-concept-name)
  (remove-obsolete-parents tbox (tbox-bottom-node tbox) concept-to-concept-name))

(defun clean-top-bottom-concepts (tbox concept-name-to-role)
  (loop for concept in (list (tbox-top-node tbox) (tbox-bottom-node tbox)) do
        (when (rest (concept-name-set concept))
          (setf (concept-name-set concept)
                (loop for name in (concept-name-set concept)
                      unless (gethash name concept-name-to-role)
                      collect name)))
        (setf (concept-told-subsumers concept) nil)
        (setf (concept-told-disjoints concept) nil)))

(defun remove-obsolete-children (tbox parent obsolete-concept-table)
  #+:debug (assert (not (gethash parent obsolete-concept-table)))
  (unless (is-bottom-concept-p parent)
    (loop with new-children = nil
          with bottom = (tbox-bottom-node tbox)
          for children = (concept-children-internal parent) then added-children
          for added-children = nil
          do
          (loop for child in children do
                (if (gethash child obsolete-concept-table)
                    (let ((child-children (remove bottom (concept-children-internal child))))
                      (when child-children
                        (setf added-children (append added-children child-children))))
                  (progn
                    (push child new-children)
                    (remove-obsolete-children tbox child obsolete-concept-table))))
          until (null added-children)
          finally
          (if new-children
              (setf (concept-children-internal parent) (nreverse new-children))
            (setf (concept-children-internal parent) (list bottom)))
          (setf (concept-children-not-in-bucket parent)
                (loop for child in (concept-children-not-in-bucket parent)
                      unless (gethash child obsolete-concept-table)
                      collect child)))))

(defun remove-obsolete-parents (tbox child obsolete-concept-table)
  #+:debug (assert (not (gethash child obsolete-concept-table)))
  (unless (is-top-concept-p child)
    (loop with new-parents = nil
          with top = (tbox-top-node tbox)
          for parents = (concept-parents-internal child) then added-parents
          for added-parents = nil
          do
          (loop for parent in parents do
                (if (gethash parent obsolete-concept-table)
                    (let ((parent-parents (remove top (concept-parents-internal parent))))
                      (when parent-parents
                        (setf added-parents (append added-parents parent-parents))))
                  (progn
                    (push parent new-parents)
                    (remove-obsolete-parents tbox parent obsolete-concept-table))))
          until (null added-parents)
          finally
          (if new-parents
              (setf (concept-parents-internal child) (nreverse new-parents))
            (setf (concept-parents-internal child) (list top))))))

(defun test-satisfiability-of-role-concept (tbox role-concept)
  (without-taxonomic-encoding
    (test-satisfiable tbox (encode-update-concept-term tbox role-concept))))

(defun test-satisfiability-of-role (tbox role)
  (let* ((role-sat-p t)
         (meta-concept (tbox-meta-constraint-concept tbox))
         (meta-concept-merging-p (and meta-concept
                                      (dl-merging (concept-language meta-concept)))))
    (unless (or (not (dl-clash-possible-p (tbox-language tbox)))
                (role-internal-name-p role)
                (role-internal-conjunction-p role)
                (eq role (tbox-object-bottom-role tbox))
                (eq role (tbox-datatype-bottom-role tbox))
                (eq role (tbox-datatype-top-role tbox))
                (role-cd-attribute role)
                (role-annotation-p role)
                (role-satisfiability-checked-p role))
      (with-concept-definition-mapping tbox
        (when (and (not (or (is-top-object-role-p role) 
                            (role-datatype role)))
                   (not (role-feature-p role))
                   (not (role-transitive-p role))
                   (or meta-concept-merging-p 
                       (dl-qualified-number-restrictions (role-language-context role))
                       (and (dl-merging (role-language-context role))
                            (has-feature-as-sibling role))))
          (let ((start (when-print-statistics
                         (when-sat-statistics
                           (get-internal-run-time)))))
            (setf (role-satisfiability-checked-p role) t)
            (unless (test-satisfiability-of-role-concept tbox `(at-least 2 ,(role-name role)))
              (setf role-sat-p (test-satisfiability-of-role-concept tbox `(at-least 1 ,(role-name role)))))
            (when-print-statistics
              (when-sat-statistics
                (print-sat-statistics *standard-output*
                                      *race-statistics-stream*
                                      start
                                      (get-internal-run-time))))))
        (unless (role-internal-name-p (role-inverse-internal role))
          (setf (role-satisfiability-checked-p (role-inverse-internal role)) t))
        (let ((start (when-print-statistics
                       (when-sat-statistics
                         (get-internal-run-time)))))
          (if (role-datatype role)
              (setf role-sat-p
                    (test-satisfiability-of-role-concept tbox `(d-at-least 1 ,(role-name role))))
            (if (is-top-object-role-p role)
                (setf role-sat-p
                      (test-satisfiability-of-role-concept tbox
                                                           `(at-least 1 ,+top-object-role-symbol+)))
              (setf role-sat-p
                    (test-satisfiability-of-role-concept tbox
                                                         `(at-least 1 ,(role-name role))))))
          (when-print-statistics
            (when-sat-statistics
              (print-sat-statistics *standard-output*
                                    *race-statistics-stream*
                                    start
                                    (get-internal-run-time))))))
      (unless role-sat-p
        (setf (role-domain-concept role) +bottom-symbol+)
        (setf (role-domain-restriction role) (tbox-object-bottom-role tbox))
        (if (role-datatype role)
            (progn
              (setf (role-range-concept role) +datatype-bottom-symbol+)
              (setf (role-range-concept role) (tbox-datatype-bottom-role tbox)))
          (progn
            (setf (role-range-concept role) +bottom-symbol+)
            (setf (role-range-concept role) (tbox-object-bottom-role tbox))))))
    role-sat-p))

(defun test-satisfiability-of-roles (tbox
                                     stream
                                     reasoning-mode
                                     role-to-concept-name
                                     first-pass)
  (with-race-trace-sublevel ("test-satisfiability-of-roles"
                             :arguments (list tbox
                                              reasoning-mode
                                              role-to-concept-name
                                              first-pass)
                             :trace-result t
                             :expanded t)
    (flet ((test-mirror-concept (role)
             (test-atomic-concept-satisfiable
              tbox
              (get-tbox-concept tbox (gethash role role-to-concept-name)))))
      (when (and (member reasoning-mode '(:setup :coherence-only :classification))
                 (not (tbox-role-hierarchy-classified-p tbox)))
        (let ((bottom-role (tbox-object-bottom-role tbox))
              (data-bottom-roles nil)
              (object-bottom-roles nil)
              (language (tbox-language tbox)))
          (when (and (or (not first-pass) (dl-merging language))
                     (dl-clash-possible-p language))
            (loop with datatype-bottom = (tbox-datatype-bottom-role tbox)
                  with datatype-top = (tbox-datatype-top-role tbox)
                  with top-role-bottom = nil
                  with meta-concept = (tbox-meta-constraint-concept tbox)
                  with meta-concept-merging-p = (and meta-concept
                                                     (dl-merging (concept-language meta-concept)))
                  for role in (sort (copy-list (tbox-encoded-role-list tbox))
                                    #'>
                                    :key (lambda (role) (length (role-ancestors-internal role))))
                  for role-synonym-to-bottom = (member role object-bottom-roles)
                  for role-sat-p = t
                  do
                  (unless (or (role-internal-name-p role)
                              (role-internal-conjunction-p role)
                              (eq role bottom-role)
                              (eq role datatype-bottom)
                              (eq role datatype-top)
                              (role-cd-attribute role)
                              (role-annotation-p role)
                              role-synonym-to-bottom
                              (and (not first-pass) (role-satisfiability-checked-p role)))
                    (multiple-value-setq (role-sat-p top-role-bottom)
                        (check-complex-role tbox role))
                    (when (and (not top-role-bottom)
                               role-sat-p
                               (not (role-satisfiability-checked-p role)))
                      (with-concept-definition-mapping tbox
                        (if first-pass
                            (when (and (not (or (is-top-object-role-p role) 
                                                (role-datatype role)))
                                       (not (role-feature-p role))
                                       (not (role-transitive-p role))
                                       (or meta-concept-merging-p 
                                           (dl-qualified-number-restrictions (role-language-context role))
                                           (and (dl-merging (role-language-context role))
                                                (has-feature-as-sibling role))))
                              (let ((start (when-print-statistics
                                             (when-sat-statistics
                                               (get-internal-run-time)))))
                                (setf (role-satisfiability-checked-p role) t)
                                (unless (test-satisfiability-of-role-concept tbox `(at-least 2 ,(role-name role)))
                                  (if (and role-to-concept-name (gethash role role-to-concept-name))
                                      (setf role-sat-p (test-mirror-concept role))
                                    (setf role-sat-p
                                          (test-satisfiability-of-role-concept tbox `(at-least 1 ,(role-name role)))))
                                  (when role-sat-p
                                    (convert-role-descendants-to-features tbox role role-to-concept-name)))
                                (when-print-statistics
                                  (when-sat-statistics
                                    (print-sat-statistics *standard-output*
                                                          *race-statistics-stream*
                                                          start
                                                          (get-internal-run-time))))))
                          (progn
                            (setf (role-satisfiability-checked-p role) t)
                            (unless (role-internal-name-p (role-inverse-internal role))
                              (setf (role-satisfiability-checked-p (role-inverse-internal role)) t))
                            (let ((start (when-print-statistics
                                           (when-sat-statistics
                                             (get-internal-run-time)))))
                              (if (role-datatype role)
                                  (setf role-sat-p
                                        (test-satisfiability-of-role-concept tbox `(d-at-least 1 ,(role-name role))))
                                (if (is-top-object-role-p role)
                                    (setf role-sat-p
                                          (test-satisfiability-of-role-concept tbox
                                                                               `(at-least 1 ,+top-object-role-symbol+)))
                                  (progn
                                    (if (and role-to-concept-name (gethash role role-to-concept-name))
                                        (setf role-sat-p (test-mirror-concept role))
                                      (setf role-sat-p
                                            (test-satisfiability-of-role-concept tbox
                                                                                 `(at-least 1 ,(role-name role))))))))
                              (when-print-statistics
                                (when-sat-statistics
                                  (print-sat-statistics *standard-output*
                                                        *race-statistics-stream*
                                                        start
                                                        (get-internal-run-time))))))))))
                  if top-role-bottom
                  do
                  (loop for role in (tbox-encoded-role-list tbox)
                        if (or (role-datatype role) (role-cd-attribute role))
                        collect role into data-roles
                        else
                        collect role into object-roles
                        finally
                        (setf object-bottom-roles object-roles)
                        (setf data-bottom-roles data-roles))
                  until top-role-bottom
                  unless role-synonym-to-bottom
                  if role-sat-p
                  do
                  '(loop for ancestor in (role-ancestors-internal role) do
                        (unless (or (eq role ancestor)
                                    (role-satisfiability-checked-p ancestor))
                          (setf (role-satisfiability-checked-p ancestor) t)))
                  else
                  do
                  (if (or (role-datatype role) (role-cd-attribute role))
                      (progn
                        (push role data-bottom-roles)
                        (racer-warn "Datatype property ~A is unsatisfiable" (role-name role)))
                    (progn
                      ;(push role object-bottom-roles)
                      (setf object-bottom-roles (append (role-descendants-internal role) object-bottom-roles)))))
            (when object-bottom-roles
              (setf object-bottom-roles (role-set-remove-duplicates object-bottom-roles))
              (loop for role in object-bottom-roles do
                    (unless (or (role-removed-p role) (eq role bottom-role))
                      (collapse-roles-new bottom-role role tbox stream))))
            (when data-bottom-roles
              (loop with bottom = (tbox-bottom-node tbox)
                    with datatype-bottom = (tbox-datatype-bottom-node tbox)
                    for role in data-bottom-roles do
                    (setf (role-domain-concept role) +bottom-symbol+)
                    (setf (role-domain-restriction role) bottom)
                    (setf (role-range-concept role) +datatype-bottom-symbol+)
                    (setf (role-range-concept role) datatype-bottom))
              (loop with bottom-role = (tbox-datatype-bottom-role tbox)
                    for role in data-bottom-roles
                    unless (eq role bottom-role) do
                    (collapse-roles bottom-role role tbox))))
          #+:debug
          (values object-bottom-roles data-bottom-roles))))))

(defun check-complex-role (tbox role)
  (let* ((language (tbox-language tbox))
         (asymmetric-roles-p (dl-asymmetric-roles language))
         (disjoint-roles-p (dl-disjoint-roles language))
         (check-reflexive-roles-p (and (dl-reflexive-roles language) 
                                       (or (dl-irreflexive-roles language) asymmetric-roles-p)))
         (check-symmetric-roles-p (and (dl-symmetric-roles language) asymmetric-roles-p))
         (top-role-bottom nil)
         (role-sat-p t))
    (when (role-compositions role)
      (when (role-reflexive-p role)
        (racer-warn "Reflexive role ~A cannot be result of role composition axioms -- ~
                                   reflexivity declaration is ignored."
                    (nice-role-name role))
        (setf (role-reflexive-p role) nil))
      (when (role-irreflexive-p role)
        (racer-warn "Irreflexive role ~A cannot be result of role composition axioms -- ~
                                   irreflexivity declaration is ignored."
                    (nice-role-name role))
        (setf (role-irreflexive-p role) nil)))
    (when (and check-reflexive-roles-p (role-reflexive-p role))
      (loop for ancestor in (role-ancestors-internal role)
            while (not top-role-bottom)
            when (or (role-irreflexive-p ancestor)
                     (role-asymmetric-p ancestor))
            do (setf top-role-bottom t)))
    (when (and check-symmetric-roles-p (not top-role-bottom) (role-symmetric-p role))
      (loop for ancestor in (role-ancestors-internal role)
            when (role-asymmetric-p ancestor)
            do (setf role-sat-p nil)
            until (not role-sat-p)))
    (when (and disjoint-roles-p (not top-role-bottom) role-sat-p)
      (let ((disjoint-roles (role-disjoint-roles role))
            (ancestors (role-ancestors-internal role)))
        (if (and disjoint-roles
                 (not (role-set-disjoint-p disjoint-roles ancestors)))
            (setf role-sat-p nil)
          (let ((ancestors-with-disjoints
                 (loop for ancestor in (role-ancestors-internal role)
                       when (and (not (eq ancestor role))
                                 (role-disjoint-roles ancestor))
                       collect ancestor)))
            (when (rest ancestors-with-disjoints)
              (loop for ancestor in ancestors-with-disjoints
                    unless (role-set-disjoint-p ancestors-with-disjoints
                                                (role-disjoint-roles ancestor))
                    do (setf role-sat-p nil)
                    until (not role-sat-p)))))))
    (values role-sat-p top-role-bottom)))


(defun convert-role-descendants-to-features (tbox role role-to-concept-name)
  (let ((language (tbox-language tbox)))
    (unless (dl-features language)
      (setf (tbox-language tbox) (add-dl-features language))))
  (loop for descendant in (role-descendants-internal role)
        for mirror-concept =
        (when role-to-concept-name
          (get-tbox-concept tbox (or (gethash descendant role-to-concept-name)
                                     (gethash (role-inverse-internal descendant)
                                              role-to-concept-name))
                            nil))
        do
        (setf (role-feature-p descendant) t)
        (setf (role-inverse-feature-p (role-inverse-internal descendant)) t)
        (setf (role-language-context role) (add-dl-features (role-language-context role)))
        when mirror-concept do
        (setf (concept-model mirror-concept) nil)
        (when (concept-negated-concept-internal mirror-concept)
          (setf (concept-model (concept-negated-concept-internal mirror-concept)) nil)))
  (loop for descendant in (role-descendants-internal role) do
        (setf (role-feature-ancestors descendant)
              (remove-if-not #'role-feature-p (role-ancestors-internal descendant)))
        (when (rest (role-feature-ancestors descendant))
          (setf (role-has-feature-ancestors-p descendant) t))))

(defun tbox-prepared-p (&optional (tbox *current-tbox*))
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (tbox-index-structures-complete-p tbox))

(defmacro tbox-prepared? (&optional (tbox-name nil tbox-name-supplied-p))
  (if (and tbox-name-supplied-p tbox-name)
    `(tbox-prepared-p ',tbox-name)
    `(tbox-prepared-p *current-tbox*)))

(defun cyclic-concepts-in-tbox (tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (ensure-tbox-state tbox :setup)
  (loop for concept in (tbox-encoded-concept-list tbox)
        when (and concept (concept-self-referencing-p concept))
        collect (concept-name-set concept)))

(defun tbox-cyclic-p (&optional (tbox *current-tbox*))
  (cyclic-concepts-in-tbox tbox))

(defmacro tbox-cyclic? (&optional (tbox-name nil tbox-name-supplied-p))
  (if (and tbox-name-supplied-p tbox-name)
    `(tbox-cyclic-p ',tbox-name)
    `(tbox-cyclic-p *current-tbox*)))

(defun clean-tbox (tbox)
  (setf (tbox-original-concept-axioms tbox) nil)
  (setf (tbox-concept-axioms tbox) nil)
  (setf (tbox-concept-axioms-index tbox) (make-concept-axioms-index tbox 0))
  (setf (tbox-role-axioms tbox) nil)
  (setf (tbox-role-axioms-index tbox) (make-role-axioms-index tbox 0))
  (setf (tbox-atomic-concepts tbox) nil)
  (setf (tbox-generalized-concept-inclusions tbox) nil)
  (setf (tbox-added-generalized-concept-inclusions tbox) nil)
  (setf (tbox-removed-generalized-concept-inclusions tbox) nil)
  (loop for concept in (tbox-encoded-concept-list tbox) do
        (setf (concept-referenced-by concept) nil))
  (loop for concept being the hash-value of (tbox-concept-store tbox)
        for neg-concept = (concept-negated-concept-internal concept) do
        (setf (concept-referencing concept) nil)
        (setf (concept-full-referencing concept) nil)
        (when neg-concept
          (setf (concept-referencing neg-concept) nil)
          (setf (concept-full-referencing neg-concept) nil))))

(defun clean-el+-tbox-fragment (tbox)
  (let ((abox *current-abox*))
    (when (and (not *prevent-lean-tbox*)
	       (tbox-use-less-memory tbox)
	       (tbox-el+-transformed-table tbox)
               (subset-el+-p (tbox-language tbox))
               (or (null abox)
                   (not (eq (abox-tbox abox) tbox))
                   (not (eql (tbox-version (abox-tbox abox)) (tbox-version tbox)))
                   (null (or (abox-individuals-list abox) (abox-role-axioms abox)))))
      (clean-el+-tbox-fragment-1 tbox))))

(defun clean-el+-tbox-fragment-1 (tbox)
  (let ((concept-store (tbox-concept-store tbox))
        (removals 0)
	(always-use-lean-tbox *always-use-lean-tbox*))
    (loop for concept being the hash-value of concept-store using (hash-key key)
          for neg-concept = (concept-negated-concept-internal concept)
          for concept-type = (type-of concept)
          do
          (if (or (member concept-type '(some-concept and-concept))
                  (and (eq concept-type 'atomic-concept)
                       (not (is-predefined-concept-p concept))))
              (progn
                (setf (concept-negated-concept-internal concept) nil)
                (when neg-concept
                  (setf (concept-negated-concept-internal neg-concept) nil))
                (when (and (eq concept-type 'and-concept)
                           (loop for conjunct in (concept-term concept)
                                 thereis (or (all-concept-p conjunct)
                                             (negated-concept-p conjunct))))
                  (incf removals)
                  (remhash key concept-store)))
            (when (member concept-type '(all-concept or-concept negated-concept))
              (setf (concept-negated-concept-internal concept) nil)
              (when neg-concept
                (setf (concept-negated-concept-internal neg-concept) nil))
              (incf removals)
              (remhash key concept-store))))
    (when (or always-use-lean-tbox (> removals 100))
      (let ((size (hash-table-size concept-store))
	    (count (hash-table-count concept-store)))
	(when (or always-use-lean-tbox (and (> size 1000) (< (/ count size) 0.7)))
	  (let ((new-concept-store (copy-hash-table concept-store (max (- 100 count) (round (* 0.1 count))))))
	    (when (boundp '*concept-store*)
	      (setf *concept-store* new-concept-store))
	    (setf (tbox-concept-store tbox) new-concept-store)
	    (let ((new-size (hash-table-size new-concept-store)))
	      (declare (ignorable new-size))
              ;(format t "~%Removals=~D,old=~D,new=~D~%" removals size new-size)
	      (race-trace ("~&EL+ tbox cleanup: removed=~D, old-size=~D, new-size=~D~%"
			   removals size new-size)))))))))

;;; ======================================================================

(defun classify-tbox (&optional (tbox *current-tbox*))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (with-race-trace-sublevel ("classify-tbox"
                             :arguments (list tbox)
                             :expanded t)
    (ensure-tbox-state tbox :classification)))

(defun check-tbox-coherence (&optional (tbox *current-tbox*) 
                                           &key (stream *standard-output*))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (with-race-trace-sublevel ("check-tbox-coherence"
                             :arguments (list tbox)
                             :expanded t
                             :trace-result t)
    (ensure-tbox-state tbox :coherence-only :stream stream)
    (let ((unsatisfiable-concepts
           (loop for name in (concept-name-set (tbox-bottom-node tbox))
                 unless (or (eq name +krss-bottom-symbol+) (eq name +bottom-symbol+))
                 collect name))
          (unsatisfiable-roles
           (loop with bottom = (tbox-object-bottom-role tbox)
                 for role in (role-synonyms-internal bottom)
                 unless (eq role bottom)
                 collect role)))
      (setf (tbox-coherent-p-internal tbox) (and (null unsatisfiable-concepts) (null unsatisfiable-roles)))
      (list unsatisfiable-concepts
            unsatisfiable-roles))))

(defun tbox-coherent-p (&optional (tbox *current-tbox*))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (unless (tbox-coherence-checked-p tbox)
    (check-tbox-coherence tbox))
  (tbox-coherent-p-internal tbox))

(defmacro tbox-coherent? (&optional (tbox-name nil tbox-name-supplied-p))
  (if (and tbox-name-supplied-p tbox-name)
    `(tbox-coherent-p ',tbox-name)
    `(tbox-coherent-p *current-tbox*)))

(defun ensure-tbox-state (tbox
                             state
                             &key
                             (stream *standard-output*)
                             (data-stream *race-statistics-stream*))
  (assert (member state '(:classification :coherence-only :setup)))
  (let ((start (when-print-statistics-plus data-stream
                 (get-internal-run-time))))
    (unless (tbox-classified-p-internal tbox)
      (let ((*n-inserted-concepts* 0)
            (tbox-index-structures-complete-p (tbox-index-structures-complete-p tbox))
            (use-less-memory (tbox-use-less-memory tbox)))
        (unless tbox-index-structures-complete-p
          (initialize-concept-role-store tbox))
        (when (and (not *prevent-lean-tbox*)
                   (or *always-use-lean-tbox*
                        (and (not use-less-memory)
                             (> (hash-table-count (tbox-concept-axioms-index tbox))
                                *enforce-lean-tbox-threshold*))))
          (setf (tbox-use-less-memory tbox) t)
          (when (and *tbox-verbose* (not *always-use-lean-tbox*))
	    (racer-warn "~&Automatically switching to lean ontology mode because the number of axioms (~D) ~
                         in ontology ~A exceeds ~D~%"
			(hash-table-count (tbox-concept-axioms-index tbox))
                        (tbox-name tbox)
                        *enforce-lean-tbox-threshold*)))

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
                               :racer-remove-constraint-duplicates-table (tbox-racer-remove-constraint-duplicates-table tbox)
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
            (unless tbox-index-structures-complete-p
	      (race-time (preencode-roles tbox))
              (when (tbox-use-less-memory tbox)
                (setf (tbox-role-axioms tbox) nil)
                (setf (tbox-role-axioms-index tbox) (make-role-axioms-index tbox)))
              (when *absorb-domains-ranges*
                (let ((*provisionally-inserted-atomic-concepts* nil)
                      (*provisionally-inserted-roles* nil))
                  (multiple-value-bind (reflexivity-added new-irreflexive-roles)
                      (race-time (absorb-simple-domain-range-restrictions tbox))
                    (when *provisionally-inserted-roles*
                      (add-provisionally-encoded-roles tbox *provisionally-inserted-roles*)
                      (preencode-roles-3 tbox *provisionally-inserted-roles*))
                    (loop for role in reflexivity-added do
                          (propagate-role-characteristics-to-parents role t nil))
                    (loop for role in new-irreflexive-roles do
                          (propagate-role-characteristics-to-children role)))))
              (let ((*provisionally-inserted-atomic-concepts* nil)
                    (*provisionally-inserted-roles* nil))
                (race-time (encode-roles tbox (tbox-encoded-role-list tbox) nil))
                (race-time (encode-domain-range-of-roles tbox))
                (race-time (preencode-concepts tbox stream))
                #+:debug (assert (null *provisionally-inserted-atomic-concepts*))
                (when *provisionally-inserted-roles*
                  (add-provisionally-encoded-roles tbox *provisionally-inserted-roles*)
		  (preencode-roles-3 tbox *provisionally-inserted-roles*)
                  (encode-roles tbox *provisionally-inserted-roles* t))))
            (setf-statistics *taxonomic-encoding-hits* 0) ;we don't want the preencoding results
            (prog2
             (when (or (eq state :classification)
                       (and (eq state :coherence-only)
                            (not (tbox-coherence-checked-p tbox)))
                       (and (eq state :setup)
                            (not (tbox-index-structures-complete-p tbox))))
               (when-print-statistics
                 (print-tbox-statistics-name tbox data-stream)))
             (unwind-protect
                 (build-tbox-structures tbox stream state)
               (when-print-statistics-plus data-stream
                 (when (or (eq state :classification)
                           (and (eq state :coherence-only)
                                (not (tbox-coherence-checked-p tbox)))
                           (and (eq state :setup)
                                (or *sat-statistics*
                                    (not (tbox-index-structures-complete-p tbox)))))
                   (if-sat-statistics (eq state :setup)
                                      (print-sat-statistics stream
                                                            data-stream
                                                            start
                                                            (get-internal-run-time))
                                      (print-tbox-statistics tbox
                                                             (and *statistics* stream)
                                                             data-stream
                                                             start
                                                             (get-internal-run-time))))))
             (setf (tbox-index-structures-complete-p tbox) t)
             (if (eq state :classification)
               (progn
                 (setf (tbox-classified-p-internal tbox) t)
                 (setf (tbox-coherence-checked-p tbox) t))
               (when (eq state :coherence-only) 
                 (setf (tbox-coherence-checked-p tbox) t))))))))))

(defun absorb-simple-domain-range-restrictions (tbox)
  (let ((inclusions (tbox-generalized-concept-inclusions tbox)))
    (when inclusions
      (multiple-value-bind (new-gcis absorbed-p reflexivity-added irreflexivity-added)
          (absorb-domain-range-restrictions tbox
                                            inclusions
                                            :pre-absorption t
                                            :functional-only t
                                            :qualified-domain-restrictions nil)
        (when absorbed-p
          (setf new-gcis
                (loop for concept in new-gcis
                      collect (list +top-symbol+ (decode-concept concept)))))
        (clrhash (tbox-concept-store tbox))
        (make-predefined-concepts (tbox-concept-store tbox)
                                  (tbox-top-node tbox)
                                  (tbox-bottom-node tbox)
                                  (tbox-datatype-top-node tbox)
                                  (tbox-datatype-bottom-node tbox))
        (when absorbed-p
          (setf (tbox-generalized-concept-inclusions tbox) new-gcis))
        (values reflexivity-added irreflexivity-added)))))

(defun process-synonym (tbox concept synonym-concept)
  (when (and (not (concept-primitive-p synonym-concept))
             (concept-encoded-definition synonym-concept))
    (when (and *taxonomic-encoding*
               (not (tbox-empty-taxonomic-encoding-table tbox)))
      (let ((table (tbox-taxonomic-encoding-table tbox)))
        (let ((key (if (atomic-concept-p (concept-encoded-definition synonym-concept))
                     synonym-concept
                     (concept-encoded-definition synonym-concept))))
          (when (gethash key table)
            (setf (gethash key table) concept)))
        (when (gethash synonym-concept table)
          (setf (gethash synonym-concept table) concept))
        (when (gethash concept table)
          (remhash concept table))
        (loop for key being the hash-key of table using (hash-value value)
              when (eq value synonym-concept)
              do (setf (gethash key table) concept))))))

(defun add-concept-synonym (tbox
                                synonym-names
			        concept
			        &optional (all-bottom-p nil) (coherence-p nil))
  (let ((synonym-concept (get-tbox-concept tbox (first synonym-names))))
    ;(break "1:~S ~S ~S" synonym-names concept synonym-concept)
    (unless (eq concept synonym-concept)
      #+:debug (assert (eq concept (get-tbox-concept tbox (first (concept-name-set concept)))))
      (process-synonym tbox concept synonym-concept)
      (setf (concept-told-subsumers concept)
            (concept-set-union (concept-told-subsumers synonym-concept)
                               (concept-told-subsumers concept)))
      (setf (concept-told-disjoints concept)
            (concept-set-union (concept-told-disjoints synonym-concept)
                               (concept-told-disjoints concept)))
      (unless (or all-bottom-p
                  (not *propagate-told-disjoints*)
                  (member +bottom-symbol+ synonym-names)
                  (member +top-symbol+ synonym-names)
                  (member +bottom-symbol+ (concept-name-set concept))
                  (member +top-symbol+ (concept-name-set concept)))
        (propagate-told-disjoints concept (concept-told-disjoints synonym-concept)))
      (setf (concept-asserted-disjoints concept)
            (concept-set-union (concept-asserted-disjoints synonym-concept)
                               (concept-asserted-disjoints concept)))
      (setf (concept-referenced-disjoints concept)
            (concept-set-union (concept-referenced-disjoints synonym-concept)
                               (concept-referenced-disjoints concept))))
    (if (and coherence-p (eq concept *bottom-concept*))
      (setf (concept-name-set concept)
            (append (concept-name-set concept) synonym-names))
      (setf (concept-name-set concept)
            (stable-union (concept-name-set concept) synonym-names)))
    (loop for synonym in (concept-name-set concept) do
          (setf (get-concept synonym) concept))
    ;(break "2:~S" (list synonym-names concept))
    ))

(defun add-anonymous-concept-synonym (synonym-names concept)
  (let ((synonym-concept (get-concept (first synonym-names))))
    (unless (eq concept synonym-concept)
      (loop for synonym in synonym-names do
            (setf (get-concept synonym) concept))
      (setf (concept-told-subsumers concept)
            (concept-set-union (concept-told-subsumers synonym-concept)
                               (concept-told-subsumers concept)))
      (setf (concept-told-disjoints concept)
            (concept-set-union (concept-told-disjoints synonym-concept)
                               (concept-told-disjoints concept)))
      (setf (concept-asserted-disjoints concept)
            (concept-set-union (concept-asserted-disjoints synonym-concept)
                               (concept-asserted-disjoints concept)))
      (setf (concept-referenced-disjoints concept)
            (concept-set-union (concept-referenced-disjoints synonym-concept)
                               (concept-referenced-disjoints concept))))))

;;; ======================================================================

(defun mark-all-ancestors (concept mark)
  (unless (eql (concept-mark1 concept) mark)
    (setf (concept-mark1 concept) mark)
    (when (and *tbox-clustering* (concept-buckets concept))
      (loop for bucket in (concept-buckets concept) do
            (setf (bucket-mark1 bucket) mark)))
    (loop for parent-concept in (concept-parents-internal concept) do
          (mark-all-ancestors parent-concept mark))))

(defun mark-all-descendants (concept mark &optional (previous-mark nil))
  (mark-all-descendants-1 concept mark previous-mark)
  #+:debug
  (when previous-mark
    *possible-subsumees*))

(defun mark-all-descendants-1 (concept mark previous-mark)
  (unless (eql (concept-mark1 concept) mark)
    (let ((old-mark (concept-mark1 concept)))
      (setf (concept-mark1 concept) mark)
      (let ((children (concept-children-internal concept)))
        (when children
          (when (and previous-mark
                     (eq (first children) *bottom-concept*)
                     (not (eql old-mark previous-mark)))
            (vector-push-extend concept *possible-subsumees* 500))
          (loop for child-concept in children do
                (mark-all-descendants-1 child-concept mark previous-mark)))))))

(defun mark-possible-subsumees (concepts)
  (let ((possible-subsumee-mark (incf *tbox-classification-counter*)))
    (setf (fill-pointer *possible-subsumees*) 0)
    (loop for concept in (concept-children-internal (first concepts)) do
          (mark-all-descendants-1 concept possible-subsumee-mark t))
    (loop for concept in (rest concepts) do
          (setf possible-subsumee-mark
	        (mark-possible-subsumees-1 concept
					   possible-subsumee-mark
					   (incf *tbox-classification-counter*)
					   (incf *tbox-classification-counter*))))
    (values possible-subsumee-mark *possible-subsumees*)))

(defun mark-possible-subsumees-1 (concept 
                                       previous-possible-subsumee-mark
                                       visited-mark
                                       new-possible-subsumee-mark)
  (let ((mark (concept-mark1 concept)))
    (cond ((or (eql mark visited-mark)
	       (eql mark new-possible-subsumee-mark))
	   new-possible-subsumee-mark)
	  ((eql mark previous-possible-subsumee-mark)
	   (mark-all-descendants-1 concept new-possible-subsumee-mark previous-possible-subsumee-mark))
	  (t (setf (concept-mark1 concept) visited-mark)
             (let ((children (concept-children-internal concept)))
               (if children
	         (loop for concept-1 in children do
		       (mark-possible-subsumees-1 concept-1 
					          previous-possible-subsumee-mark
					          visited-mark
					          new-possible-subsumee-mark))
                 (unless (eq concept *bottom-concept*)
                   (vector-push-extend concept *possible-subsumees* 500))
                 )))))
  new-possible-subsumee-mark)

(defun unmark-possible-subsumees (concept possible-subsumee-mark)
  (when (eql (concept-mark1 concept) possible-subsumee-mark)
    (setf (concept-mark1 concept) nil)
    (loop for parent-concept in (concept-parents-internal concept) do
          (unmark-possible-subsumees parent-concept possible-subsumee-mark))))

(defun concept-all-disjoints (concept)
  (concept-set-remove-duplicates (concept-all-disjoints-1 concept)))

(defun concept-all-disjoints-1 (concept)
  (append (concept-told-disjoints concept)
          (loop for parent in (concept-parents-internal concept)
                for disjoints = (concept-all-disjoints-1 parent)
                when disjoints
                append disjoints)))

(defun unmark-possible-subsumees-if-disjoint (tbox 
                                                     concept
                                                     possible-subsumee-mark
                                                     bottom-node)
  (unless (or *ignore-told-disjoint-information-during-bottom-search*
              (eq concept bottom-node))
    (loop for concept-1 in (concept-all-disjoints concept) do
          (let ((real-concept-1 (get-tbox-concept tbox (first (concept-name-set concept-1)) nil)))
            
            (when (and real-concept-1 
                       (eql (concept-mark1 real-concept-1) possible-subsumee-mark))
              #+:debug
              (when *debug*
                (format *trace-output*
                        "~%While inserting: ~S" concept))
              (unmark-possible-subsumees-if-disjoint-1 real-concept-1
                                                       possible-subsumee-mark
                                                       bottom-node))))))

(defun unmark-possible-subsumees-if-disjoint-1 (concept possible-subsumee-mark bottom-node)
  (unless (eq concept bottom-node)
    (setf (concept-mark1 concept) nil)
    #+:debug
    (when *debug*
      (format *trace-output*
              "~%Unmarking possible subsumee: ~S" concept))
    (loop for child-concept in (concept-children-internal concept) do
          (unmark-possible-subsumees-if-disjoint-1 child-concept 
                                                   possible-subsumee-mark
                                                   bottom-node))))

;;; **********************************************************************

(defparameter *use-et-model* nil)

(defun insert-concept-into-tbox (concept tbox tbox-el+-p tbox-l-minus-possible-p internal-only stream)
  (let ((concept-name (concept-name-set concept)))
    
    (when *debug*
      (format stream "~%Inserting ~S (~D) into TBox ~S ..."
              concept *n-inserted-concepts* (tbox-name tbox)))
    (when *print-where-am-i*
      (format  stream " ~S" concept))
    
    (let* ((*old-subsumption-tests* *subsumption-tests*)
           (immediate-predecessors nil)
           (immediate-sucessors nil)
           (no-meta-constraints (null (tbox-meta-constraint-concepts tbox)))
           (primitive-p (concept-primitive-p concept))
           (encoded-definition (concept-encoded-definition concept))
           (concept-language (concept-language concept))
           (l-minus-p (and tbox-l-minus-possible-p (subset-l-minus-p concept-language)))
           (not-self-refencing-p (and (not (concept-self-referencing-p concept))
                                      (or (null encoded-definition)
					  (not (some #'concept-self-referencing-p
                                                     (concept-told-subsumers encoded-definition))))))
           (completely-defined-p (and l-minus-p
                                      primitive-p
                                      not-self-refencing-p))
           (subsumer-mark (unless completely-defined-p
                            (incf *tbox-classification-counter*)))
           (non-subsumer-mark (unless l-minus-p
                                (incf *tbox-classification-counter*)))
           (possible-subsumee-mark nil)
           (el+-synonyms (when tbox-el+-p
                           (extract-concept-synonyms-from-ancestors tbox concept internal-only))))
      (cond
       ((and tbox-el+-p el+-synonyms (some #'concept-parents-internal el+-synonyms))
        (loop for synonym in el+-synonyms do
              (when (concept-parents-internal synonym)
                (when (and (concept-visible-p concept) (concept-visible-p synonym))
                  (tbox-classification-report 
                   stream
                   ':concepts-equal
                   (concept-name-set concept)
                   (concept-name-set synonym)
                   (tbox-name tbox)))
                (add-concept-synonym tbox (concept-name-set concept) synonym))))
       ((and (not primitive-p) (atomic-concept-p encoded-definition))
        (let* ((true-definition-concept-1
                (get-tbox-concept tbox (first (concept-name-set encoded-definition))))
               (true-definition-concept-2
                (get-tbox-concept tbox (first (concept-name-set true-definition-concept-1)))))
          ;;; the encoded-definition can point to a concept name that has already been declared
          ;;; a synonum for another concept name
          (when (and (concept-visible-p concept) (concept-visible-p true-definition-concept-2))
            (tbox-classification-report 
             stream
             ':concepts-equal
             (concept-name-set concept)
             (concept-name-set true-definition-concept-2)
             (tbox-name tbox)))
          (add-concept-synonym tbox (concept-name-set concept) true-definition-concept-2)))
       (t
        (when *debug*
          (format *trace-output*
                  "~%Testing subsumption for ~A in TBox ~A ."
                  concept-name (tbox-name tbox)))
        (let ((satisfiable
               (if tbox-el+-p
                   (not (member *bottom-concept* (concept-told-subsumers concept)))
                 (or completely-defined-p
                     (and not-self-refencing-p
                          no-meta-constraints
                          (subset-el+-p concept-language))
                     (test-atomic-concept-satisfiable tbox
                                                      (get-tbox-concept tbox
                                                                        (first (concept-name-set
                                                                                concept))))))))
          (if satisfiable
              (let ((top (tbox-top-node tbox)))
                (when *debug*
                  (format stream "~%Starting top search for concept ~S~%" concept))
                (if (and completely-defined-p (not tbox-el+-p))
                    (if encoded-definition
                        (if (and-concept-p encoded-definition)
                            (let ((concepts (concept-term encoded-definition)))
                              (setf immediate-predecessors
                                    (extract-concept-parents-from-told-subsumers concepts))
                              #+:debug
                              (assert (and immediate-predecessors
                                           (every #'atomic-concept-p immediate-predecessors))))
                          (progn
                            #+:debug (assert (atomic-concept-p encoded-definition))
                            (setf immediate-predecessors (list encoded-definition))))
                      (setf immediate-predecessors (list top)))
                  (if (and tbox-el+-p
                           (or (null el+-synonyms)
                               (not (some #'concept-parents-internal el+-synonyms))))
                      (let* ((parents (extract-concept-parents-from-ancestors tbox concept internal-only))
                             (true-parents 
                              (loop with dag = (env-dag (tbox-el+-environment tbox))
                                    with parent-synonyms-found = nil
                                    for parent in parents
                                    for true-parent = (if (el+-has-synonyms parent dag)
                                                          (progn
                                                            (unless parent-synonyms-found
                                                              (setf parent-synonyms-found t))
                                                            (get-tbox-concept tbox (first (concept-name-set parent))))
                                                        parent)
                                    when (or (null el+-synonyms) (not (member true-parent el+-synonyms)))
                                    collect true-parent into result
                                    finally
                                    (if parent-synonyms-found
                                        ; parents can be synonyms to one another
                                        (return (concept-set-remove-duplicates result))
                                      (return result)))))
                        (if true-parents
                            (setf immediate-predecessors true-parents)
                          (setf immediate-predecessors (list top))))
                    (progn
                      (let ((model (concept-model concept)))
                        (when (and (model-info-p model)
                                   (not (concept-set-subsetp (model-det-positive-literals model)
                                                             (concept-told-subsumers concept))))
                          #|(format t "+S:~D " (length (concept-set-difference (model-det-positive-literals model)
                                                                             (concept-told-subsumers concept))))
                            (format t "Subs of ~S: ~S~%" concept (concept-set-difference (model-det-positive-literals model)
                                                                                       (concept-told-subsumers concept)))|#
                          (when *use-et-model*
                            (setf (concept-told-subsumers concept)
                                  (concept-set-union (model-det-positive-literals model)
                                                     (concept-told-subsumers concept)))))
                        (when (and (full-model-info-p model)
                                   (not (concept-set-subsetp (model-det-negative-literals model)
                                                             (concept-told-disjoints concept))))
                          #|(format t "+D:~D " (length (concept-set-difference (model-det-negative-literals model)
                                                                             (concept-told-disjoints concept))))
                            (format t "Disj of ~S: ~S~%" concept (concept-set-difference (model-det-negative-literals model)
                                                                                        (concept-told-disjoints concept)))|#
                          (when *use-et-model*
                            (setf (concept-told-disjoints concept)
                                  (concept-set-union (model-det-negative-literals model)
                                                     (concept-told-disjoints concept))))))
                      (when encoded-definition
                        (loop for concept-1 in (concept-told-subsumers concept) 
                              for real-concept-1 = (get-tbox-concept tbox (first (concept-name-set concept-1)))
                              unless (eq concept-1 concept)
                              do (mark-all-ancestors real-concept-1 subsumer-mark)))
                      (unless l-minus-p
                        (loop for concept-2 in (concept-told-subsumers concept) do
                              (loop for concept-1 in (concept-all-disjoints concept-2)
                                    for real-concept-1 = (get-tbox-concept ; take synonyms into account
                                                          tbox 
                                                          (first (concept-name-set concept-1)))
                                    do (setf (concept-mark1 real-concept-1) non-subsumer-mark))))
                      (setf immediate-predecessors
                            (top-search tbox concept top 
                                        subsumer-mark
                                        non-subsumer-mark)))))
            
                (cond ((and (not tbox-el+-p)
                            (null (rest immediate-predecessors))
                            (enhanced-top-subsumes-p tbox
                                                     concept 
                                                     (first immediate-predecessors)
                                                     (incf *tbox-classification-counter*) ; RIGHT????
                                                     (incf *tbox-classification-counter*))) 
                       (when (and (concept-visible-p concept)
                                  (concept-visible-p (first immediate-predecessors)))
                         (tbox-classification-report stream ':concepts-equal
                                                     (concept-name-set concept)
                                                     (concept-name-set (first immediate-predecessors))
                                                     (tbox-name tbox)))
                       (add-concept-synonym tbox (concept-name-set concept) (first immediate-predecessors)))
                      ((if (and *sorted-concept-list* *fully-sorted-concept-list*)
                           (and (null (tbox-meta-constraint-concepts tbox))
                                (or tbox-el+-p (concept-for-no-bottom-search-p tbox concept)))
                         (and primitive-p
                              (not (dl-inverse (concept-language concept)))
                              (not *auto-install-primitives*)
                              (not (tbox-use-less-memory tbox))
                              (null (tbox-generalized-concept-inclusions tbox))))
                       (setf immediate-sucessors (list (tbox-bottom-node tbox)))
                   
                       (when (and *always-do-bottom-search* (not tbox-el+-p))
                         (let ((new-immediate-sucessors nil)
                               (possible-subsumees nil))
                           (multiple-value-setq (possible-subsumee-mark possible-subsumees)
                               (mark-possible-subsumees immediate-predecessors))
                           (unmark-possible-subsumees-if-disjoint tbox
                                                                  concept
                                                                  possible-subsumee-mark
                                                                  (tbox-bottom-node tbox))
                           (setf new-immediate-sucessors 
                                 (bottom-search tbox
                                                concept 
                                                (tbox-bottom-node tbox)
                                                possible-subsumee-mark
                                                possible-subsumees))
                           (unless (equal immediate-sucessors new-immediate-sucessors)
                             (error "differences in children for ~S found: expected ~S, found ~S"
                                    concept immediate-sucessors new-immediate-sucessors)
                               ;(setf immediate-sucessors new-immediate-sucessors)
                             )))
                   
                       (when *debug*
                         (format stream "~%Skipping bottom search for concept ~S~%" concept))
                       (insert-concept-node-without-bottom-search concept 
                                                                  immediate-predecessors
                                                                  immediate-sucessors
                                                                  tbox-el+-p))
                      (t
                       (incf-statistics *bottom-search-tests*)
                       (when *debug*
                         (format stream "~%Starting bottom search for concept ~S~%" concept))
                       (let ((possible-subsumees nil))
                         (multiple-value-setq (possible-subsumee-mark possible-subsumees)
                             (mark-possible-subsumees immediate-predecessors))
                         (unless tbox-el+-p
                           (unmark-possible-subsumees-if-disjoint tbox
                                                                  concept
                                                                  possible-subsumee-mark
                                                                  (tbox-bottom-node tbox)))
                         (setf immediate-sucessors 
                               (bottom-search tbox
                                              concept 
                                              (tbox-bottom-node tbox)
                                              possible-subsumee-mark
                                              possible-subsumees))
                     
                         (insert-concept-node tbox
                                              concept 
                                              immediate-predecessors
                                              immediate-sucessors)))))
            (progn
              (when (concept-visible-p concept)
                (tbox-classification-report stream ':concept-incoherent 
                                            (concept-name-set concept)
                                            (tbox-name tbox)))
              (add-concept-synonym tbox (concept-name-set concept) (tbox-bottom-node tbox)))))
        (when *print-where-am-i*
          (format stream "(~D,~D)"
                  (length immediate-predecessors)
                  (length immediate-sucessors)))))
      (when (and (or *use-optimized-tbox-traversal-no-bottom-search* tbox-el+-p)
                 *sorted-concept-list*
                 (eq (tbox-end-of-no-bottom-search tbox) concept))
        (when (rest (concept-children-internal *top-concept*))
          (setf (concept-parents-internal *bottom-concept*)
                (delete *top-concept* (concept-parents-internal *bottom-concept*)))
          (setf (concept-children-internal *top-concept*)
                (delete *bottom-concept* (concept-children-internal *top-concept*))))
        (loop with end-concept = (tbox-end-of-no-bottom-search tbox)
              for elem in (tbox-encoded-concept-list tbox)
              for concept = (get-tbox-concept tbox (concept-term elem))
              when (and (not (eq concept *top-concept*))
                        (not (eq concept *bottom-concept*))
                        (null (concept-children-internal concept)))
              do (setf (concept-children-internal concept) (list *bottom-concept*))
              and collect concept into concepts
              until (eq end-concept elem)
              finally
              (setf (concept-parents-internal *bottom-concept*)
                    (nconc concepts (concept-parents-internal *bottom-concept*)))))
      nil)))

(defun extract-concept-parents-from-told-subsumers (subsumers)
  ;;; only correct for completely defined concepts
  (if (rest subsumers)
    (let ((mark (inc-marker-variable '*concept-set-mark*)))
      (loop for candidate in subsumers
            for definition = (concept-encoded-definition candidate)
            for subsumers = (when definition
                              (concept-told-subsumers definition))
            do (mark-racer-set subsumers mark))
      (loop for candidate in subsumers
            when (eql (racer-set-flag candidate) mark)
            collect candidate into redundant-ancestors
            finally
            (return (if redundant-ancestors
                      (concept-set-difference subsumers redundant-ancestors)
                      subsumers))))
    subsumers))

;;; ======================================================================

(defstruct (el+-dag (:constructor make-el+-dag-internal))
  (outgoing (racer-make-hash-table)) ; for each vertex all adjacent vertices of its outgoing edges
  (incoming (racer-make-hash-table)) ; for each vertex all adjacent vertices of its incoming edges
  (cyclics nil) ; vertices that are part of a cycle
  (transitive-p nil) ; if T incrementally complete transitive closure after an edge has been added
  )

(defun make-el+-dag (&key (transitive-p nil))
  (make-el+-dag-internal :transitive-p transitive-p))

(defun make-el+-subsumption-dag (tbox)
  (let ((no-of-concepts (length (tbox-encoded-concept-list tbox))))
    (make-el+-dag-internal :outgoing (racer-make-hash-table 
                                      :size (max (round (* 0.75 no-of-concepts)) 100))
                           :incoming (racer-make-hash-table 
                                      :size (max (round (/ no-of-concepts 2)) 100))
                           :transitive-p t)))

(defstruct el+-dag-adjacent
  (table nil)             ; hash table of vertices
  (vertices nil)          ; list of vertices
  )

(defun print-el+-dag-statistics (&optional
                                 (dag (env-dag (tbox-el+-environment *current-tbox*)))
                                 (stream *standard-output*))
  (labels ((true-values (vertices)
             (if (listp vertices)
                 vertices
               (el+-dag-adjacent-vertices vertices)))
           (maximize (table)
             (loop for values being the hash-value of table
                   maximize (length (true-values values))))
           (average (table)
             (loop for values being the hash-value of table
                   sum (length (true-values values)) into sum
                   finally (return (/ sum (hash-table-count table)))))
           (deviation (table)
             (loop with mean = (average table)
                   for values being the hash-value of table
                   for diff = (- mean (length (true-values values)))
                   sum (* diff diff) into sum
                   finally (return (sqrt (/ sum (hash-table-count table)))))))
    (when dag
      (format stream "~&;#outgoing=~D, #incoming=~D, #cyclics=~D~%"
              (hash-table-count (el+-dag-outgoing dag))
              (hash-table-count (el+-dag-incoming dag))
              (if (el+-dag-cyclics dag) 
                  (hash-table-count (el+-dag-cyclics dag))
                0))
      (format stream ";outgoing-length: max=~D ave=~,1F dev=~,1F, incoming-length: max=~D ave=~,1F dev=~,1F~%"
              (maximize (el+-dag-outgoing dag)) (average (el+-dag-outgoing dag)) (deviation (el+-dag-outgoing dag))
              (maximize (el+-dag-incoming dag)) (average (el+-dag-incoming dag)) (deviation (el+-dag-incoming dag)))
      (when (el+-dag-cyclics dag)
        (multiple-value-bind (max mean count)
            (loop for concept being the hash-key of (el+-dag-cyclics dag)
                  for length = (length (concept-name-set concept))
                  sum length into sum
                  maximize length into max
                  count (> length 100) into count
                  finally (return (values max (/ sum (hash-table-count (el+-dag-cyclics dag))) count)))
          (let ((deviation
                 (loop for concept being the hash-key of (el+-dag-cyclics dag)
                       for diff = (- mean (length (concept-name-set concept)))
                       sum (* diff diff) into sum
                       finally (return (sqrt (/ sum (hash-table-count (el+-dag-cyclics dag))))))))
            (format stream ";card of cyclics sets: max=~D ave=~,1F dev=~,1F card>100=~D~%"
                    max mean deviation count)))))))

#+:debug
(defun el+-dag-get-edge (lhs rhs dag)
  (let ((outgoing (gethash lhs (el+-dag-outgoing dag))))
    (if (listp outgoing)
        (member rhs outgoing)
      (gethash rhs (el+-dag-adjacent-table outgoing)))))

#-:debug 
(defmacro el+-dag-get-edge (lhs rhs dag)
  (let ((outgoing-sym (gensym)))
    `(let ((,outgoing-sym (gethash ,lhs (el+-dag-outgoing ,dag))))
       (if (listp ,outgoing-sym)
           (member ,rhs ,outgoing-sym)
         (gethash ,rhs (el+-dag-adjacent-table ,outgoing-sym))))))

(defparameter *el+-dag-vertices-initial-table-size* 50)

(defun el+-dag-insert-edge (lhs rhs dag label &optional (no-cycle-check nil))
  (declare (ignore label))
  (assert (or no-cycle-check (not (eq lhs rhs))))
  (push lhs (gethash rhs (el+-dag-incoming dag)))
  (let ((outgoing (gethash lhs (el+-dag-outgoing dag))))
    (if (listp outgoing)
        (progn
          (if (> (length outgoing) *el+-dag-vertices-initial-table-size*)
              (progn
                (push rhs outgoing)
                (setf (gethash lhs (el+-dag-outgoing dag)) (create-el+-dag-adjacency-table outgoing)))
            (push rhs (gethash lhs (el+-dag-outgoing dag)))))
      (progn
        (setf (gethash rhs (el+-dag-adjacent-table outgoing)) t)
        (push rhs (el+-dag-adjacent-vertices outgoing)))))
  #+:debug (cons lhs rhs))

(defun create-el+-dag-adjacency-table (concepts)
  (let ((table (racer-make-hash-table :size (round (* 1.2 *el+-dag-vertices-initial-table-size*)))))
    (loop for concept in concepts do
          (setf (gethash concept table) t))
    (make-el+-dag-adjacent :vertices concepts :table table)))

#+:debug 
(defun el+-dag-insert-new-edge (lhs rhs dag label)
  (unless (el+-dag-get-edge lhs rhs dag)
    (el+-dag-insert-edge lhs rhs dag label)
    t))

#-:debug 
(defmacro el+-dag-insert-new-edge (lhs rhs dag label)
  `(unless (el+-dag-get-edge ,lhs ,rhs ,dag)
     (el+-dag-insert-edge ,lhs ,rhs ,dag ,label)
     t))

#+:debug 
(defun el+-dag-outgoing-vertices (lhs dag)
  (let ((outgoing (gethash lhs (el+-dag-outgoing dag))))
    (if (listp outgoing)
        outgoing
      (el+-dag-adjacent-vertices outgoing))))
      
#-:debug 
(defmacro el+-dag-outgoing-vertices (lhs dag)
  (let ((outgoing-sym (gensym)))
    `(let ((,outgoing-sym (gethash ,lhs (el+-dag-outgoing ,dag))))
       (if (listp ,outgoing-sym)
           ,outgoing-sym
         (el+-dag-adjacent-vertices ,outgoing-sym)))))

#+:debug 
(defun el+-dag-incoming-vertices (rhs dag)
  (gethash rhs (el+-dag-incoming dag)))

#-:debug 
(defmacro el+-dag-incoming-vertices (rhs dag)
  `(gethash ,rhs (el+-dag-incoming ,dag)))

#+:debug
(defun el+-dag-is-cyclic (vertex dag)
  (when (el+-dag-cyclics dag)
    (gethash vertex (el+-dag-cyclics dag))))

#-:debug
(defmacro el+-dag-is-cyclic (vertex dag)
  `(when (el+-dag-cyclics ,dag)
     (gethash ,vertex (el+-dag-cyclics ,dag))))

#+:debug
(defun el+-dag-add-cyclic-vertex (vertex dag)
  (unless (el+-dag-cyclics dag)
    (setf (el+-dag-cyclics dag) (racer-make-hash-table)))
  (unless (gethash vertex (el+-dag-cyclics dag))
    (setf (gethash vertex (el+-dag-cyclics dag)) t)))

#-:debug
(defmacro el+-dag-add-cyclic-vertex (vertex dag)
  `(progn
     (unless (el+-dag-cyclics ,dag)
       (setf (el+-dag-cyclics ,dag) (racer-make-hash-table)))
     (unless (gethash ,vertex (el+-dag-cyclics ,dag))
       (setf (gethash ,vertex (el+-dag-cyclics ,dag)) t))))

(defun el+-dag-complete-transitive-closure (lhs rhs dag)
  ;; incrementally complete the transitive closure for the edge (lhs rhs) added to dag
  ;; we transitively connect all paths leading to lhs with all paths starting from rhs
  (let ((edge-keys-to-be-added nil)
        (lhs-incoming (el+-dag-incoming-vertices lhs dag))
        (rhs-outgoing (el+-dag-outgoing-vertices rhs dag))
        (cyclic-p nil))
    (when (or lhs-incoming rhs-outgoing)
      (flet ((push-edge-key (l-lhs l-rhs dag)
               (if (eq l-lhs l-rhs)
                   (progn
                     (unless cyclic-p
                       (el+-dag-add-cyclic-vertex lhs dag)
                       (el+-dag-add-cyclic-vertex rhs dag)
                       (setf cyclic-p t))
                     (unless (or (eq l-lhs lhs) (eq l-lhs rhs))
                       (el+-dag-add-cyclic-vertex l-lhs dag)))
                 (unless (el+-dag-get-edge l-lhs l-rhs dag)
                   (push (cons l-lhs l-rhs) edge-keys-to-be-added)))))
        (loop for lhs-vertex in lhs-incoming do
              (push-edge-key lhs-vertex rhs dag))
        (loop for rhs-vertex in rhs-outgoing do
              (push-edge-key lhs rhs-vertex dag))
        (when (and lhs-incoming rhs-outgoing)
          (loop for lhs-vertex in lhs-incoming do
                (loop for rhs-vertex in rhs-outgoing do
                      (push-edge-key lhs-vertex rhs-vertex dag)))))
      (loop for pair in edge-keys-to-be-added do
            (el+-dag-insert-edge (car pair) (cdr pair) dag ':transitive))
      edge-keys-to-be-added)))

(defun el+-dag-direct-edges (lhs dag member-fn)
  (loop with parents = nil
        with synonyms = nil
        for rhs in (el+-dag-outgoing-vertices lhs dag)
        do
        (when (funcall member-fn rhs)
          (if (el+-dag-get-edge rhs lhs dag)
              (push rhs synonyms)
            (let ((parent-p t)
                  (removed nil))
              (loop for parent in parents do
                    (when (el+-dag-get-edge parent rhs dag)
                      (setf parent-p nil)
                      (return))
                    (when (el+-dag-get-edge rhs parent dag)
                      (push parent removed)))
              (when removed
                (if (rest removed)
                    (setf parents (concept-set-difference parents removed))
                  (setf parents (delete (first removed) parents))))
              (when parent-p
                (push rhs parents)))))
        finally (return (values parents synonyms))))

;;; ======================================================================

(defun el+-has-synonyms (concept dag)
  (el+-dag-is-cyclic concept dag))

(defun add-subsumption-to-dag (lhs rhs dag)
  #+:debug
  (assert (or (and (atomic-concept-p lhs) (atomic-concept-p rhs))
              (and (individual-p-internal lhs)
                   (or (atomic-concept-p rhs) (individual-p-internal rhs)))))
  (when (el+-dag-insert-new-edge lhs rhs dag t)
    (if (el+-dag-transitive-p dag)
        (values t (el+-dag-complete-transitive-closure lhs rhs dag))
      t)))

(defun el+-transitive-subsumers (concept dag &optional (keep-subsumer-fn nil))
  #+:debug (assert (or (atomic-concept-p concept) (individual-p-internal concept)))
  (if (eq concept *top-concept*)
      (list concept)
    (let ((subsumers 
           (if (concept-p-internal concept)
               (cons concept (el+-dag-outgoing-vertices concept dag))
             (el+-dag-outgoing-vertices concept dag))))
      (if keep-subsumer-fn
          (loop for subsumer in subsumers
                when (funcall keep-subsumer-fn subsumer)
                collect subsumer)
        subsumers))))

(defun el+-is-subsumed-by (concept subsumer dag)
  #+:debug 
  (assert (and (or (atomic-concept-p concept) (individual-p-internal concept))
               (atomic-concept-p subsumer)))
  (or (eq concept subsumer)
      (eq subsumer *top-concept*)
      (el+-dag-get-edge concept subsumer dag)))

(defun el+-is-subsumed-by-subset (concept subset dag)
  #+:debug (assert (and (or (atomic-concept-p concept) (individual-p-internal concept))
                        (consp subset)))
  (let ((outgoing (gethash concept (el+-dag-outgoing dag))))
    (if (individual-p-internal concept)
        (if (listp outgoing)
            (concept-set-subsetp subset outgoing)
          (loop with subsumer-table = (el+-dag-adjacent-table outgoing)
                for elem in subset
                always (gethash elem subsumer-table)))
      (if (listp outgoing)
          (concept-set-subsetp subset (cons concept outgoing))
        (loop with subsumer-table = (el+-dag-adjacent-table outgoing)
              for elem in subset
              always (or (eq concept elem) (gethash elem subsumer-table)))))))

;;; ======================================================================

(defun tbox-el+-saturation (tbox &optional environment added-concepts)
  (with-race-trace-sublevel ("tbox-el+-saturation"
                             :arguments (list tbox
                                              environment
                                              added-concepts)
                             :expanded nil
                             :trace-result t)
    (let* ((old-environment (or environment (tbox-el+-environment tbox)))
           (init-p (not old-environment))
           (environment
            (or old-environment
                (make-el+-environment :tbox-language (tbox-language tbox)
                                      :dag (make-el+-subsumption-dag tbox)
                                      :role-members 
                                      (racer-make-hash-table :size (length (tbox-encoded-role-list tbox)))
                                      :nary-absorptions (tbox-nary-absorption-table tbox)
                                      :inverse-role-compositions
                                      (when (dl-complex-role-inclusions (tbox-language tbox))
                                        (racer-make-hash-table))))))
      (unless old-environment
        (setf (tbox-el+-environment tbox) environment))
      (if init-p
          (progn
            (race-trace ("~&Initializing Tbox EL+ tables~%"))
            (initialize-el+-tbox-tables environment
                                        (tbox-encoded-concept-list tbox)
                                        (tbox-encoded-role-list tbox))
            (loop for concept in (tbox-encoded-concept-list tbox) do
                  (process-concept-definition concept environment))
            (loop with bottom = (tbox-bottom-node tbox)
                  with subsumption-dag = (env-dag environment)
                  for concept in (tbox-encoded-concept-list tbox)
                  for told-subsumers = (el+-transitive-subsumers concept subsumption-dag)
                  do
                  (if (member bottom told-subsumers)
                      (progn
                        (setf (concept-told-subsumers concept) (list bottom))
                        (setf (concept-model concept) bottom)
                        (unless (member (first (concept-name-set concept)) (concept-name-set bottom))
                          (add-concept-synonym tbox (concept-name-set concept) bottom)))
                    (setf (concept-told-subsumers concept) told-subsumers))))
        (progn
          (race-trace ("~&Extending existing Tbox EL+ tables~%"))
          (when added-concepts
            (race-trace ("~&Updating newly added concepts ~S~%" added-concepts))
            (initialize-el+-tbox-tables environment added-concepts)
            (loop for concept in added-concepts do
                  (process-concept-definition concept environment))
            (loop with subsumption-dag = (env-dag environment)
                  for concept in added-concepts do
                  (setf (concept-told-subsumers concept)
                        (el+-transitive-subsumers concept subsumption-dag))))))
      (race-trace ("~&EL+ environemnt ~S" environment))
      environment)))

(defun initialize-el+-tbox-tables (environment concepts &optional (roles nil))
  (let ((concepts-used-in-domain-qualification (env-concepts-used-in-domain-qualification environment))
        (inverse-role-compositions (env-inverse-role-compositions environment))
        (tbox-language (env-tbox-language environment)))
    (loop for concept in concepts do
          (loop for domain-qualification in (concept-elh-role-domain-qualifications concept)
                for role = (car domain-qualification)
                for encoded-definition = (concept-encoded-definition concept)
                for definition-language = (when encoded-definition
                                            (concept-language encoded-definition))
                do
                (when definition-language
                  (setf (concept-language concept)
                        (union-dl-descriptors definition-language (concept-language concept)))
                  (setf tbox-language (union-dl-descriptors (concept-language concept) tbox-language)))
                (when domain-qualification
                  (loop for concept in (cdr domain-qualification) do
                        (setf tbox-language (union-dl-descriptors (concept-language concept) tbox-language))))
                ;(push concept (gethash role concepts-used-in-domain-qualification))
                (unless (gethash role concepts-used-in-domain-qualification)
                  (setf (gethash role concepts-used-in-domain-qualification) t))))
    (let ((nary-absorptions (env-nary-absorptions environment)))
      (when nary-absorptions
        (loop for concept being the hash-value of nary-absorptions 
              do
              (setf tbox-language (union-dl-descriptors (concept-language concept) tbox-language)))))
    (loop with processed = (racer-make-hash-table :size (length roles))
          with transitive-p = nil
          with reflexive-p = nil
          for role in roles
          do
          (unless (is-predefined-role-p role)
            (when (and (not transitive-p) (role-transitive-p role))
              (setf transitive-p t))
            (when (and (not reflexive-p) (role-reflexive-p role))
              (setf reflexive-p t))
            (loop for (role-1 role-2) in (role-compositions role) do
                  (push role (gethash role-1 inverse-role-compositions))
                  (push role (gethash role-2 inverse-role-compositions)))
            (when (gethash role concepts-used-in-domain-qualification)
              (loop for descendant in (role-descendants-internal role) do
                    (unless (or (eq descendant role)
                                (is-predefined-role-p descendant)
                                (gethash descendant processed))
                      (setf (gethash descendant processed) t)
                      (setf (gethash descendant concepts-used-in-domain-qualification) t))))
            #|(let ((concepts (gethash role concepts-used-in-domain-qualification)))
              (when concepts
                (setf (gethash role concepts-used-in-domain-qualification)
                      (concept-set-remove-duplicates concepts))))|#
            #|(loop with trigger-concepts = (gethash role concepts-used-in-domain-qualification)
                  for ancestor in (role-ancestors-internal role)
                  do
                  (unless (or (eq ancestor role) (is-predefined-role-p ancestor))
                    (let ((concepts (gethash ancestor concepts-used-in-domain-qualification)))
                      (when concepts
                        (setf trigger-concepts (concept-set-union concepts trigger-concepts)))))
                  finally
                  (when trigger-concepts
                    (setf (gethash role concepts-used-in-domain-qualification) trigger-concepts)))|#)
          finally
          (when transitive-p
            (setf tbox-language (add-dl-transitive tbox-language)))
          (when reflexive-p
            (setf tbox-language (add-dl-reflexive tbox-language))))
    (when inverse-role-compositions
      (setf tbox-language (add-dl-complex-role-inclusions tbox-language))
      (loop for role being the hash-key of inverse-role-compositions using (hash-value roles) do
            (setf (gethash role inverse-role-compositions) (role-set-remove-duplicates roles))))
    (let ((referenced-role-has-domain-table (racer-make-hash-table)))
      (when inverse-role-compositions
        (loop for role in roles do
              (when (referenced-role-has-domain-p role
                                                  inverse-role-compositions
                                                  concepts-used-in-domain-qualification)
                (setf (gethash role referenced-role-has-domain-table) t))))
      (loop with role-chain-member-list = nil
            with composed-roles = (racer-make-hash-table)
            for role in roles
            do
            (when (and (not (or (is-predefined-role-p role) (role-internal-name-p role)))
                       (or (and (role-transitive-p role) (gethash role concepts-used-in-domain-qualification))
                           (and inverse-role-compositions
                                (loop for defined-role in (gethash role inverse-role-compositions)
                                      thereis
                                      (loop for composition in (role-compositions defined-role)
                                            thereis (eq (first composition) role)))
                                (gethash role referenced-role-has-domain-table))))
              (push role role-chain-member-list))
            (when (and inverse-role-compositions
                       (or (gethash role referenced-role-has-domain-table)
                           (loop for composition in (role-compositions role)
                                 thereis
                                 (loop for comp-role in composition
                                       thereis
                                       (gethash comp-role referenced-role-has-domain-table)))))
              (unless (gethash role composed-roles)
                (setf (gethash role composed-roles) t)))
            finally
            (when role-chain-member-list
              (setf (env-role-chain-member-list environment) role-chain-member-list))
            (when composed-roles
              (setf (env-composed-roles environment) composed-roles))))
    (setf (env-tbox-language environment) tbox-language)))

(defun referenced-role-has-domain-p (role inverse-role-compositions concepts-used-in-domain-qualification)
  (unless (is-predefined-role-p role)
    (or (role-domain-restriction role)
        (gethash role concepts-used-in-domain-qualification)
        (loop for defined-role in (gethash role inverse-role-compositions)
              thereis
              (and (not (eq role defined-role))
                   (referenced-role-has-domain-p defined-role
                                                 inverse-role-compositions
                                                 concepts-used-in-domain-qualification))))))

(defun process-concept-definition (concept environment)
  #+:debug (assert (atomic-concept-p concept))
  (loop for told-subsumer in (concept-told-subsumers concept) do
        (unless (eq concept told-subsumer)
          (process-subsumption-axiom concept told-subsumer environment)))
  (let ((encoded-definition (concept-encoded-definition concept)))
    (when encoded-definition
      #+:debug 
      (assert (or (atomic-concept-p encoded-definition)
                  (some-concept-p encoded-definition)
                  (and-concept-p encoded-definition)
                  (negated-concept-p encoded-definition)))
      (if (is-bottom-concept-p encoded-definition)
          (process-subsumption-axiom concept encoded-definition environment)
        (if (some-concept-p encoded-definition)
            (process-edge-axiom concept encoded-definition environment)
          (when (and (contains-some-p (concept-language encoded-definition))
                     (and-concept-p encoded-definition))
            (loop for conjunct in (concept-term encoded-definition) do 
                  (when (some-concept-p conjunct)
                    (process-edge-axiom concept conjunct environment)))))))))

(defun process-edge-axiom (lhs rhs environment)
  #+:debug (assert (and (atomic-concept-p lhs) (some-concept-p rhs)))
  (process-edge lhs (concept-role rhs) (concept-term rhs) environment))

(defun process-edge (lhs role rhs environment)
  #+:debug
  (assert (or (and (atomic-concept-p lhs) (atomic-concept-p rhs))
              (and (individual-p-internal lhs)
                   (or (atomic-concept-p rhs) (individual-p-internal rhs)))))
  (let ((role-members (env-role-members environment)))
    (multiple-value-bind (added-p added-transitive-edges)
        (add-to-role-members lhs role rhs role-members)
      (when added-p
        (when (and (dl-bottom (env-tbox-language environment))
                   (el+-is-subsumed-by rhs *bottom-concept* (env-dag environment)))
          (process-subsumption-axiom lhs *bottom-concept* environment))
        (process-subsumption-implied-edges lhs role rhs environment)
        (process-role-domains lhs role rhs environment)
        (process-role-compositions lhs role rhs environment)
        (loop for (l-lhs . l-rhs) in added-transitive-edges do
              (process-role-domains l-lhs role l-rhs environment)
              (process-role-compositions l-lhs role l-rhs environment))
        (process-super-role-edges (cons (cons lhs rhs) added-transitive-edges) role environment)))))

(defun process-subsumption-implied-edges (lhs role rhs environment)
  (when (and (role-transitive-p role) (gethash role (env-concepts-used-in-domain-qualification environment)))
    (let* ((subsumption-dag (env-dag environment))
           (role-members (env-role-members environment))
           (role-dag (gethash role role-members)))
      (when (atomic-concept-p lhs)
        (loop for new-lhs in (matching-subsumed-lhs-in-role-members lhs rhs role-dag subsumption-dag) do
              (process-edge new-lhs role rhs environment)))
      (loop for new-rhs in (matching-subsumed-rhs-in-role-members lhs rhs role-dag subsumption-dag) do
            (process-edge lhs role new-rhs environment)))))

(defun process-super-role-edges (added-transitive-edges role environment)
  (loop for key in added-transitive-edges
        for (lhs . rhs) = key
        do
        (loop with internal-name-p = (role-internal-name-p role)
              for super-role in (role-ancestors-internal role)
              do
              (unless (or (eq super-role role)
                          (is-predefined-role-p super-role)
                          (and (not internal-name-p) (role-internal-name-p super-role)))
                (process-edge lhs super-role rhs environment)))))

(defun process-role-domains (lhs role rhs environment)
  #+:debug 
  (assert (or (and (atomic-concept-p lhs) (atomic-concept-p rhs))
              (and (individual-p-internal lhs)
                   (or (atomic-concept-p rhs) (individual-p-internal rhs)))))
  (let ((domain (role-domain-restriction role)))
    (when domain
      #+:debug (or (atomic-concept-p domain) (and-concept-p domain))
      (process-subsumption-axioms lhs domain environment)))
  (let ((domain-trigger-concepts (gethash role (env-concepts-used-in-domain-qualification environment))))
    (when domain-trigger-concepts
      (loop with top = *top-concept*
            with role-ancestors = (role-ancestors-internal role)
            with rhs-subsumers = (if (concept-p-internal rhs)
                                     (cons rhs (el+-dag-outgoing-vertices rhs (env-dag environment)))
                                   (el+-dag-outgoing-vertices rhs (env-dag environment)))
            ;for rhs-subsumer in (concept-set-intersection domain-trigger-concepts rhs-subsumers)
            for rhs-subsumer in rhs-subsumers
            do
            (unless (eq rhs-subsumer top)
              (loop for (domain-role . domains) in (concept-elh-role-domain-qualifications rhs-subsumer) do
                    (when (member domain-role role-ancestors)
                      (process-subsumption-axioms lhs domains environment))))))))

(defun process-role-compositions (lhs role rhs environment)
  #+:debug 
  (assert (or (and (atomic-concept-p lhs) (atomic-concept-p rhs))
              (and (individual-p-internal lhs)
                   (or (atomic-concept-p rhs) (individual-p-internal rhs)))))
  (let ((inverse-role-compositions (env-inverse-role-compositions environment)))
    (when inverse-role-compositions
      (loop with role-members = (env-role-members environment)
            with subsumption-dag = (env-dag environment)
            with composed-roles = (env-composed-roles environment)
            with lhs-individual-p = (individual-p-internal lhs)
            for defined-role in (gethash role inverse-role-compositions)
            do
            (when (or lhs-individual-p (gethash defined-role composed-roles))
              (loop for (role-1 role-2) in (role-compositions defined-role) do
                    (when (eq role-1 role)
                      (let ((role-2-dag (gethash role-2 role-members)))
                        (when role-2-dag
                          (loop for l-rhs in (matching-rhs-in-role-members lhs
                                                                           rhs
                                                                           defined-role
                                                                           role-members
                                                                           role-2-dag
                                                                           subsumption-dag)
                                do
                                (process-edge lhs defined-role l-rhs environment)))))
                    (when (eq role-2 role)
                      (let ((role-1-dag (gethash role-1 role-members)))
                        (when role-1-dag                    
                          (loop for l-lhs in (matching-lhs-in-role-members lhs
                                                                           rhs
                                                                           defined-role
                                                                           role-members
                                                                           role-1-dag
                                                                           subsumption-dag)
                                do
                                (process-edge l-lhs defined-role rhs environment)))))))))))

(defun process-nary-subsumptions (lhs rhs environment)
  #+:debug (assert (and (or (atomic-concept-p lhs) (individual-p-internal lhs))
                        (atomic-concept-p rhs)))
  (loop with dag = (env-dag environment)
        with nary-absorptions = (env-nary-absorptions environment)
        for nary-unfold-set in (concept-nary-unfold-sets rhs)
        do
        (when (el+-is-subsumed-by-subset lhs nary-unfold-set dag)
          (let ((nary-conclusion (gethash nary-unfold-set nary-absorptions)))
            #+:debug (assert (or (atomic-concept-p nary-conclusion) (and-concept-p nary-conclusion)))
            (unless (if (atomic-concept-p nary-conclusion) 
                        (el+-is-subsumed-by lhs nary-conclusion dag)
                      (el+-is-subsumed-by-subset lhs (concept-term nary-conclusion) dag))
              (process-subsumption-axioms lhs nary-conclusion environment))))))

(defun process-role-domain-implied-edges (concept subsumer environment)
  #+:debug
  (assert (and (or (atomic-concept-p concept) (individual-p-internal concept))
               (atomic-concept-p subsumer)))
  (loop with dag = (env-dag environment)
        with role-members = (env-role-members environment)
        for elh-role-domain-qualification in (concept-elh-role-domain-qualifications subsumer)
        for role = (car elh-role-domain-qualification)
        for role-dag = (gethash role role-members)
        do
        (when role-dag
          (loop for new-concept in (matching-unsubsumed-lhs-in-role-members concept
                                                                            (cdr elh-role-domain-qualification)
                                                                            role-dag
                                                                            dag)
                do
                (process-edge new-concept role subsumer environment)))))

(defun process-subsumer-implied-edges (concept subsumer environment)
  #+:debug
  (assert (and (or (atomic-concept-p concept) (individual-p-internal concept))
               (atomic-concept-p subsumer)))
  (when (atomic-concept-p concept)
    (loop with role-members = (env-role-members environment)
	  with inverse-role-compositions = (env-inverse-role-compositions environment)
          for role in (env-role-chain-member-list environment)
          for dag = (gethash role role-members)
          do
          (when (and dag
                     (or (and (role-transitive-p role)
			      (el+-dag-outgoing-vertices subsumer dag))
			 (and inverse-role-compositions 
                              (gethash role inverse-role-compositions))))
            (loop for lhs in (el+-dag-incoming-vertices concept dag) do
                  (process-edge lhs role subsumer environment))))))

(defun process-subsumption-axiom (concept subsumer environment)
  #+:debug 
  (assert (and (or (atomic-concept-p concept) (individual-p-internal concept))
               (atomic-concept-p subsumer)))
  (unless (eq concept subsumer)
    (multiple-value-bind (added-p added-transitive-edge-keys)
        (add-subsumption-to-dag concept subsumer (env-dag environment))
      (when added-p
        (let ((nary-absorptions-p (env-nary-absorptions environment))
              (role-chain-member-list (env-role-chain-member-list environment)))
          (when nary-absorptions-p
            (process-nary-subsumptions concept subsumer environment))
          (when role-chain-member-list
            (process-subsumer-implied-edges concept subsumer environment))
          (process-role-domain-implied-edges concept subsumer environment)
          (loop for (l-concept . l-subsumer) in added-transitive-edge-keys do
                (when nary-absorptions-p
                  (process-nary-subsumptions l-concept l-subsumer environment))
                (when role-chain-member-list
                  (process-subsumer-implied-edges l-concept l-subsumer environment))
                (process-role-domain-implied-edges l-concept l-subsumer environment)))))))

(defun process-subsumption-axioms (lhs rhs-list environment)
  #+:debug 
  (assert (and (or (atomic-concept-p lhs) (individual-p-internal lhs))
               (or (consp rhs-list) (and-concept-p rhs-list) (atomic-concept-p rhs-list))))
  (let ((concepts 
         (cond ((consp rhs-list) rhs-list)
               ((and-concept-p rhs-list) (concept-term rhs-list))
               ((atomic-concept-p rhs-list) (list rhs-list)))))
    (loop for rhs in concepts do
          (process-subsumption-axiom lhs rhs environment))))

;;; ======================================================================

(defun find-pair-in-role-members (lhs rhs role role-members)
  (let ((dag (gethash role role-members)))
    (when dag
      (el+-dag-get-edge lhs rhs dag))))

(defun add-to-role-members (lhs role rhs role-members)
  (let* ((dag (gethash role role-members))
         (found-p (and dag (el+-dag-get-edge lhs rhs dag))))
    (unless found-p
      (let ((transitive-p (role-transitive-p role)))
        (unless dag
          (setf dag (make-el+-dag :transitive-p transitive-p))
          (setf (gethash role role-members) dag))
        (el+-dag-insert-edge lhs rhs dag t t)
        (if transitive-p
            (values t (el+-dag-complete-transitive-closure lhs rhs dag))
          t)))))

(defun matching-unsubsumed-lhs-in-role-members (concept subsumers role-dag dag)
  (loop for lhs in (el+-dag-incoming-vertices concept role-dag)
        unless (el+-is-subsumed-by-subset lhs subsumers dag)
        collect lhs))

(defun matching-lhs-in-role-members (lhs rhs defined-role role-members role-1-dag subsumption-dag)
  (loop with all-incoming-subsumees = nil
        for subsumee in (if (atomic-concept-p lhs)
                            (cons lhs (el+-dag-incoming-vertices lhs subsumption-dag))
                          (list lhs))
        for incoming-subsumees = (el+-dag-incoming-vertices subsumee role-1-dag)
        do
        (when incoming-subsumees
          (setf all-incoming-subsumees (concept-set-union incoming-subsumees all-incoming-subsumees)))
        finally
        (when all-incoming-subsumees
          (let ((defined-role-dag (gethash defined-role role-members)))
            (if defined-role-dag
                (return (concept-set-difference all-incoming-subsumees
                                                (el+-dag-incoming-vertices rhs defined-role-dag)))
              (return all-incoming-subsumees))))))

(defun matching-rhs-in-role-members (lhs rhs defined-role role-members role-2-dag subsumption-dag)
  (loop with all-outgoing-subsumers = nil
        for subsumer in (if (atomic-concept-p rhs)
                            (cons rhs (el+-dag-outgoing-vertices rhs subsumption-dag))
                          (list rhs))
        for outgoing-subsumer = (el+-dag-outgoing-vertices subsumer role-2-dag)
        do
        (when outgoing-subsumer
          (setf all-outgoing-subsumers (concept-set-union outgoing-subsumer all-outgoing-subsumers)))
        finally
        (when all-outgoing-subsumers
          (let ((defined-role-dag (gethash defined-role role-members)))
            (if defined-role-dag
                (return (concept-set-difference all-outgoing-subsumers
                                                (el+-dag-outgoing-vertices lhs defined-role-dag)))
              (return all-outgoing-subsumers))))))

(defun matching-subsumed-lhs-in-role-members (lhs rhs role-dag subsumption-dag)
  (loop for subsumee in (el+-dag-incoming-vertices lhs subsumption-dag)
        when (el+-dag-incoming-vertices subsumee role-dag)
        collect subsumee into result
        finally
        (when result
          (return (concept-set-difference result (el+-dag-incoming-vertices rhs role-dag))))))

(defun matching-subsumed-rhs-in-role-members (lhs rhs role-dag subsumption-dag)
  (loop for subsumer in (el+-dag-outgoing-vertices rhs subsumption-dag)
        when (el+-dag-outgoing-vertices subsumer role-dag)
        collect subsumer into result
        finally
        (return (concept-set-difference result (el+-dag-outgoing-vertices lhs role-dag)))))

(defun el+-collect-some-concepts-from-role-members (ind-or-concept role-members-table)
  (loop for role being the hash-key of role-members-table using (hash-value dag)
        nconc
        (let ((exists-top-added nil)
              (result nil))
          (loop for rhs in (el+-dag-outgoing-vertices ind-or-concept dag)
                do
                (if (individual-p-internal rhs)
                    (unless exists-top-added
                      (push (encode-concept-term `(some ,role ,+top-symbol+)) result)
                      (setf exists-top-added t))
                  (push (encode-concept-term `(some ,role ,rhs)) result)))
          (loop with inv-exists-top-added = nil
                for lhs in (el+-dag-incoming-vertices ind-or-concept dag)
                do
                (if (individual-p-internal lhs)
                    (unless inv-exists-top-added
                      (push (encode-concept-term `(some ,(role-inverse-internal role) ,+top-symbol+))
                            result)
                      (setf inv-exists-top-added t))
                  (push (encode-concept-term `(some ,(role-inverse-internal role) ,ind-or-concept)) result)))
          result)))

(defun print-el+-role-members-statistics (&optional 
                                          (role-members (env-role-members (tbox-el+-environment *current-tbox*)))
                                          (stream *standard-output*))
  (loop for role being the hash-key of role-members using (hash-value dag) do
        (print-el+-role-statistics role dag stream)))

(defun print-el+-role-statistics (role
                                  &optional 
                                  (dag (gethash role (env-role-members (tbox-el+-environment *current-tbox*))))
                                  (stream *standard-output*))
  (when dag
    (format stream "~&Role ~S" role)
    (when (role-compositions role)
      (format stream "@"))
    (when (role-transitive-p role)
      (format stream "+"))
    (format stream "~%")
    (print-el+-dag-statistics dag stream)))

;;; ======================================================================

(defun extract-concept-parents-from-ancestors (tbox concept-or-individual internal-only)
  (let ((select-p-fn (if internal-only 
                         (lambda (concept) (not (concept-visible-p concept)))
                       #'concept-visible-p))
        (dag (env-dag (tbox-el+-environment tbox))))
    (multiple-value-bind (parents synonyms)
        (el+-dag-direct-edges concept-or-individual dag select-p-fn)
      (let ((result
          (if synonyms
              (concept-set-remove-duplicates
               (append parents 
                       (loop for synonym in synonyms
                             append (el+-dag-direct-edges synonym dag select-p-fn))))
            parents)))
        result))))

(defun extract-concept-synonyms-from-ancestors (tbox concept internal-only)
  (let ((dag (env-dag (tbox-el+-environment tbox))))
    (when (el+-has-synonyms concept dag)
      (let ((subsumers (concept-told-subsumers concept)))
        (when (rest subsumers)
          (loop with synonyms = nil
                for candidate in subsumers
                when (and (not (eq candidate concept))
                          (el+-has-synonyms candidate dag)
                          (if internal-only
                              (not (concept-visible-p candidate))
                            (concept-visible-p candidate))
                          (el+-dag-get-edge candidate concept dag))
                do (push candidate synonyms)
                finally
                (when synonyms
                  (setf synonyms (concept-set-remove-duplicates synonyms)))
                (return synonyms)))))))
       
;;; ======================================================================

(defun insert-anonymous-concept-into-tbox (concept tbox stream bidirectional-p)
  (when *debug*
    (format stream "~%Inserting ~S (~D) into TBox ~S ..."
            concept *n-inserted-concepts* (tbox-name tbox)))
  
  (let* ((*old-subsumption-tests* *subsumption-tests*)
         immediate-predecessors
         immediate-sucessors
         (subsumer-mark (incf *tbox-classification-counter*))
         possible-subsumee-mark
         (non-subsumer-mark (incf *tbox-classification-counter*)))
    
    (when *debug*
      (format *trace-output*
              "~%Testing subsumption for ~S in TBox ~A ."
              concept (tbox-name tbox)))
    (let ((satisfiable (test-atomic-concept-satisfiable tbox concept)))
      (when satisfiable
        (loop for concept-2 in (concept-told-subsumers concept) do
              (loop for concept-1 in (concept-all-disjoints concept-2) 
                    do
                    (let ((real-concept-1 (get-tbox-concept ; take synonyms into account
                                           tbox 
                                           (first (concept-name-set concept-1)))))
                      (setf (concept-mark1 real-concept-1) non-subsumer-mark))))
        
        (setf immediate-predecessors
              (top-search tbox concept (tbox-top-node tbox) 
                          subsumer-mark
                          non-subsumer-mark)))
      
      ;; Synonym for bottom -> concept is inconsistent.
      (cond ((not satisfiable)
             (add-anonymous-concept-synonym (concept-name-set concept) (tbox-bottom-node tbox)))
            ((and (null (rest immediate-predecessors))
                  (enhanced-top-subsumes-p tbox
                                           concept 
                                           (first immediate-predecessors)
                                           (incf *tbox-classification-counter*) ; RIGHT????
                                           (incf *tbox-classification-counter*))) 
             (add-anonymous-concept-synonym (concept-name-set concept)
                                            (first immediate-predecessors)))
            (t (let ((possible-subsumees nil))
                 (multiple-value-setq (possible-subsumee-mark possible-subsumees)
                   (mark-possible-subsumees immediate-predecessors))
                 
                 (setf immediate-sucessors 
		       (bottom-search tbox
				      concept 
                                      (tbox-bottom-node tbox)
                                      possible-subsumee-mark
                                      possible-subsumees))
                 
                 (insert-anonymous-concept-node concept 
                                                immediate-predecessors
                                                immediate-sucessors
                                                bidirectional-p)))))))

;;; ======================================================================
;;;
;;; Top search:
;;;
;;; ======================================================================

(defun enhanced-top-subsumes-p (tbox 
                                    subsumer-concept 
                                    inserted-concept
                                    subsumer-mark
				    non-subsumer-mark)
  (let ((mark (concept-mark1 subsumer-concept)))
    (cond ((eql mark non-subsumer-mark)
           nil)
          ((eql mark subsumer-mark)
           t)
          ((loop for concept in (concept-parents-internal subsumer-concept) 
                 always (enhanced-top-subsumes-p tbox concept inserted-concept
                                                 subsumer-mark non-subsumer-mark))
           (cond ((test-subsumes subsumer-concept inserted-concept)
                  (loop for concept-1 in (concept-all-disjoints subsumer-concept)
                        for real-concept-1 = (get-tbox-concept tbox 
                                                               (first (concept-name-set concept-1)))
                        do
                        (setf (concept-mark1 real-concept-1) non-subsumer-mark))
                  (setf (concept-mark1 subsumer-concept) subsumer-mark)
                  t)
                 (t
                  (setf (concept-mark1 subsumer-concept) non-subsumer-mark)
                  nil))))))

(defun top-search (tbox
                     inserted-concept 
                     current-concept
                     subsumer-mark
                     &optional 
                     (non-subsumer-mark (incf *tbox-classification-counter*))
                     (visited-mark (incf *tbox-classification-counter*)))
  (unless (eql (concept-mark1 current-concept) non-subsumer-mark)
    (setf (concept-visited-mark current-concept) visited-mark))
  (let* ((clustering *tbox-clustering*)
         (create-new-bucket-p (and clustering
                                   (>= (length (concept-children-not-in-bucket current-concept))
                                       *bucket-size-threshold*)))
         (positive-successors
          (if (and clustering
                   (or (concept-bucket-children current-concept)
                       create-new-bucket-p))
            (progn
              (when create-new-bucket-p
                 (create-new-buckets tbox current-concept subsumer-mark))
              (loop for bucket in (concept-bucket-children current-concept)
                    for members = (bucket-members bucket)
                    when (or (eql (bucket-mark1 bucket) subsumer-mark)
                             (test-concept-models-subsume tbox
                                                          bucket
                                                          inserted-concept))
                    nconc
                    (loop for concept in members
                          when (enhanced-top-subsumes-p tbox concept inserted-concept
                                                        subsumer-mark
                                                        non-subsumer-mark)
                          collect concept)
                    into successors
                    finally
                    (return
                     (if (concept-children-not-in-bucket current-concept)
                       (nconc (loop for concept in (concept-children-not-in-bucket current-concept)
                                    when (enhanced-top-subsumes-p tbox concept inserted-concept
                                                                  subsumer-mark
                                                                  non-subsumer-mark)
                                    collect concept)
                              successors)
                       successors))))
            (loop for concept in (concept-children-internal current-concept)
                  when (enhanced-top-subsumes-p tbox concept inserted-concept
                                                subsumer-mark
                                                non-subsumer-mark)
                  collect concept))))
    (if (null positive-successors)
      (list current-concept)
      (loop for concept in positive-successors 
            unless (or (eql (concept-visited-mark concept) visited-mark)
                       (eql (concept-mark1 current-concept) non-subsumer-mark))
            nconc (top-search tbox inserted-concept concept
			      subsumer-mark non-subsumer-mark visited-mark)))))


;;; ======================================================================
;;;
;;; Bottom search:
;;;
;;; ======================================================================

(defun bottom-search-subsumes-term-p (inserted-concept
                                           subsumee-concept
                                           possible-subsumee-mark
				           subsumee-mark)
  (if (test-subsumes inserted-concept subsumee-concept)
    (progn
      (mark-all-descendants subsumee-concept subsumee-mark)
      (setf (concept-mark1 subsumee-concept) subsumee-mark)
      t)
    (progn 
      (unmark-possible-subsumees subsumee-concept possible-subsumee-mark) 
      nil)))

(defun bottom-search (tbox
                         inserted-concept 
                         current-concept
                         possible-subsumee-mark
                         possible-subsumees)
  (let* ((*meta-constraint-concepts*
          (tbox-meta-constraint-concepts tbox))
         (*blocking-possibly-required*
          (or *encode-roles-as-transitive*
              (or *meta-constraint-concepts*
                  (tbox-blocking-possibly-required tbox))))
         (*blocking-used* nil))
    (bottom-search-1 inserted-concept 
                     current-concept
                     possible-subsumee-mark
                     (incf *tbox-classification-counter*)
                     (incf *tbox-classification-counter*)
                     possible-subsumees)))

(defun bottom-search-1 (inserted-concept 
                           current-concept
                           possible-subsumee-mark
                           subsumee-mark
                           visited-mark
                           possible-subsumees)
  (setf (concept-visited-mark current-concept) visited-mark)
  (let ((positive-predecessors 
         (compute-positive-predecessors possible-subsumees
                                        inserted-concept
                                        possible-subsumee-mark
                                        subsumee-mark)))
    (if (null positive-predecessors)
      (list current-concept)
      (loop for concept in positive-predecessors 
            unless (eql (concept-visited-mark concept) visited-mark)
	    nconc (bottom-search-1 inserted-concept 
				   concept
				   possible-subsumee-mark 
				   subsumee-mark 
				   visited-mark
                                   (concept-parents-internal concept))))))

(defun compute-positive-predecessors (possible-subsumees
				           inserted-concept
				           possible-subsumee-mark
				           subsumee-mark)
  (if (listp possible-subsumees)
    (loop for concept in possible-subsumees
          for mark1 = (concept-mark1 concept)
          when (or (eql mark1 subsumee-mark)
                   (and (eql mark1 possible-subsumee-mark)
                        (bottom-search-subsumes-term-p inserted-concept
                                                       concept
                                                       possible-subsumee-mark
                                                       subsumee-mark)))
          collect concept)
    (loop for concept across possible-subsumees
          for mark1 = (concept-mark1 concept)
          when (or (eql mark1 subsumee-mark)
                   (and (eql mark1 possible-subsumee-mark)
                        (bottom-search-subsumes-term-p inserted-concept
                                                       concept
                                                       possible-subsumee-mark
                                                       subsumee-mark)))
          collect concept)))

;;; ======================================================================
;;; Classification messages
;;; 

(defun tbox-classification-report (stream format-description &rest format-args)
  (when *tbox-verbose*
    (case format-description
      (:concept-incoherent
       (apply #'format
              stream
              "~%Concept ~S is incoherent in TBox ~S."
              format-args))
      (:concepts-equal
       (apply #'format
              stream
              "~%Concept ~S is equivalent to concept ~S in TBox ~S."
              format-args))
      (:cycles-detected
       (apply #'format
              stream 
              "~%Cycles detected in TBox ~S. ~
               Concept ~S causes a cycle while unfolding ~S." 
              format-args))
      (:all-concepts-incoherent
       (apply #'format
              stream 
              "~%All concepts are incoherent in TBox ~S. ~
               General concept inclusions cause the contradiction." 
              format-args))
      (:top-object-role-incoherent
       (apply #'format
              stream 
              "~%All concepts and roles are incoherent in TBox ~S. ~
               General concept inclusions and/or role declarations cause the contradiction." 
              format-args))
      (:concept-cyclic
       (apply #'format
              stream
              "~%Concept ~S causes a cycle in TBox ~S."
              format-args))
      (:concept-cyclic-gci
       (apply #'format
              stream
              "~%Concept ~S causes a cycle in TBox ~S due to global axioms."
              format-args))
      (:roles-equivalent
       (if (member +bottom-object-role-symbol+ format-args)
           (apply #'format
                  stream
                  "~%Role ~S is unsatisfiable."
                  (remove +bottom-object-role-symbol+ format-args))
         (apply #'format
                stream
                "~%Role ~S and role ~S are equivalents."
                format-args)))
      (t (apply #'format
                stream
                format-description
                format-args)))))


;;; ======================================================================
;;; External interface
;;;

(defun get-object-bottom-role (tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (ensure-tbox-state tbox :setup)
  (role-name (tbox-object-bottom-role tbox)))

(defun get-data-bottom-role (tbox)
  (get-datatype-bottom-role tbox))

(defun get-datatype-bottom-role (tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (ensure-tbox-state tbox :setup)
  (role-name (tbox-datatype-bottom-role tbox)))


(defun get-object-top-role (tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (ensure-tbox-state tbox :setup)
  (role-name (tbox-object-top-role tbox)))

(defun get-datatype-top-role (tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (ensure-tbox-state tbox :setup)
  (role-name (tbox-datatype-top-role tbox)))



(defmacro concept-satisfiable? (concept-1 
                               &optional (tbox-name nil tbox-name-supplied-p))
  (if tbox-name-supplied-p
    `(concept-satisfiable-p ',concept-1 ',tbox-name)
    `(concept-satisfiable-p ',concept-1 *current-tbox*)))

(defmacro concept-subsumes? (concept-1 
                              concept-2 
			      &optional (tbox-name nil tbox-name-supplied-p))
  (if tbox-name-supplied-p
    `(concept-subsumes-p ',concept-1 ',concept-2 ',tbox-name)
    `(concept-subsumes-p ',concept-1 ',concept-2 *current-tbox*)))

(defmacro concept-equivalent? (concept-1
                               concept-2
                               &optional (tbox-name nil tbox-name-supplied-p))
  (if tbox-name-supplied-p
    `(concept-equivalent-p ',concept-1 ',concept-2 ',tbox-name)
    `(concept-equivalent-p ',concept-1 ',concept-2 *current-tbox*)))

(defmacro concept-disjoint? (concept-1 
                            concept-2 
                            &optional (tbox-name nil tbox-name-supplied-p))
  (if tbox-name-supplied-p
    `(concept-disjoint-p ',concept-1 ',concept-2 ',tbox-name)
    `(concept-disjoint-p ',concept-1 ',concept-2 *current-tbox*)))


(defmacro role-satisfiable? (role &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(role-satisfiable-p ',role ',tbox)
    `(role-satisfiable-p ',role *current-tbox*)))

(defun role-satisfiable-p (role tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (unless (role-node-p role)
    (check-role-term role)
    (ensure-role-is-known role tbox))
  #+:debug (assert (tbox-role-hierarchy-classified-p tbox))
  (let ((true-role (if (role-node-p role) 
                       role
                     (get-tbox-role tbox role))))
    (if (role-satisfiability-checked-p true-role)
        (not (or (if (role-datatype true-role)
                     (eq (tbox-datatype-bottom-role tbox) true-role)
                   (eq (tbox-object-bottom-role tbox) true-role))
                 (eq (role-domain-restriction true-role) (tbox-bottom-node tbox))))
      (test-satisfiability-of-role tbox true-role))))

#|
(defun get-object-bottom-role (tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (tbox-object-bottom-role tbox))

(defun get-data-bottom-role (tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (tbox-datatype-bottom-role tbox))
|#

(defmacro role-subsumes? (role-term-1 
                           role-term-2
			   &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(role-subsumes-p ',role-term-1 ',role-term-2 ',tbox)
    `(role-subsumes-p ',role-term-1 ',role-term-2 *current-tbox*)))

(defun role-subsumes-p (role-term-1 role-term-2 tbox)
  "Test whether role-1 subsumes role-2"
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term-1)
  (check-role-term role-term-2)
  (with-race-trace-sublevel ("role-subsumes-p"
                             :arguments (list role-term-1 role-term-2 tbox)
                             :trace-result t)
    (ensure-knowledge-base-state ':tbox-prepared tbox)
    (ensure-role-is-known role-term-1 tbox)
    (ensure-role-is-known role-term-2 tbox)
    (let* ((role-name-1 (if (consp role-term-1)
                            (role-name (role-inverse-internal (get-tbox-role tbox (second role-term-1))))
                          role-term-1))
           (role-name-2 (if (consp role-term-2)
                            (role-name (role-inverse-internal (get-tbox-role tbox (second role-term-2))))
                          role-term-2))
           (role-1 (get-tbox-role tbox role-name-1))
           (role-2 (get-tbox-role tbox role-name-2))
           (language (tbox-language tbox)))
      (if (eql role-name-1 role-name-2)
          t
        (if (or (tbox-role-hierarchy-classified-p tbox)
                (not (or (dl-merging language) (dl-features language) (dl-complex-role-inclusions language))))
            (and (member role-1 (role-ancestors-internal role-2))
                 t)
          (let ((marker (create-tbox-internal-marker-concept tbox)))
            (concept-subsumes-p `(some ,role-name-1 ,marker)
                                `(some ,role-name-2 ,marker)
                                tbox)))))))
	     
(defmacro role-equivalent? (role-term-1 
                           role-term-2
			   &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(role-equivalent-p ',role-term-1 ',role-term-2 ',tbox)
    `(role-equivalent-p ',role-term-1 ',role-term-2 *current-tbox*)))

(defun role-equivalent-p (role-1 role-2 tbox)
  "Test whether role-1 is equivalent to role-2"
  (and (role-subsumes-p role-1 role-2 tbox)
       (role-subsumes-p role-2 role-1 tbox)))

(defmacro role-disjoint? (role-term-1 
                           role-term-2
			   &optional (tbox nil tbox-supplied-p))
  (if tbox-supplied-p
    `(role-disjoint-p ',role-term-1 ',role-term-2 ',tbox)
    `(role-disjoint-p ',role-term-1 ',role-term-2 *current-tbox*)))

(defun role-disjoint-p (role-term-1 role-term-2 tbox)
  "Test whether role-1 is disjoint to role-2"
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-role-term role-term-1)
  (check-role-term role-term-2)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (ensure-role-is-known role-term-1 tbox)
  (ensure-role-is-known role-term-2 tbox)
  (let* ((role-1 (if (consp role-term-1)
                     (role-inverse-internal (get-tbox-role tbox (second role-term-1)))
                   (get-tbox-role tbox role-term-1)))
         (role-2 (if (consp role-term-2)
                     (role-inverse-internal (get-tbox-role tbox (second role-term-2)))
                   (get-tbox-role tbox role-term-2)))
         (role-1-datatype (role-datatype role-1))
         (role-2-datatype (role-datatype role-2)))
    (let ((result
           (and (or (and role-1-datatype (not role-2-datatype))
                    (and (not role-1-datatype) role-2-datatype)
                    (if (and role-1-datatype role-2-datatype)
                        (not (datatype-compatible-p role-1-datatype role-2-datatype nil))
                      (or (member role-2 (role-disjoint-roles role-1))
                          (let ((role-1-name (role-name role-1)))
                            (if (role-characteristics-checked-p tbox role-1-name ':disjoint)
                                nil
                              (let ((role-2-name (role-name role-2)))
                                (set-role-characteristics-checked-p tbox role-1-name ':disjoint)
                                (set-role-characteristics-checked-p tbox role-2-name ':disjoint)
                                (when (test-roles-characteristics tbox role-1-name role-2-name ':disjoint)
                                  (push role-2 (role-disjoint-roles role-1))
                                  (push role-1 (role-disjoint-roles role-2))
                                  (push (role-inverse-internal role-2)
                                        (role-disjoint-roles (role-inverse-internal role-1)))
                                  (push (role-inverse-internal role-1)
                                        (role-disjoint-roles (role-inverse-internal role-2)))
                                  t)))))))
                t)))
      
      (when result
        (let* ((role-1-inverse (role-inverse-internal role-1))
               (role-1-inverse-name (role-name role-1-inverse)))
          (unless (role-characteristics-checked-p tbox role-1-inverse-name ':disjoint)
            (set-role-characteristics-checked-p tbox role-1-inverse-name ':disjoint)
            (set-role-characteristics-checked-p tbox (role-name (role-inverse-internal role-2)) ':disjoint))))
      result)))

(defun concept-subsumes-p (subsumer subsumee tbox)
  "Test whether subsumer subsumes subsumee"
  (let ((tbox (and tbox (find-tbox tbox))))
    (if tbox
      (with-race-trace-sublevel ("concept-subsumes-p"
                                 :arguments (list subsumer subsumee tbox)
                                 :trace-result t)
        (ensure-knowledge-base-state ':tbox-prepared tbox)
        (with-concept-definition-mapping tbox
          (let ((start (when-print-statistics
                        (when-sat-statistics
                         (get-internal-run-time)))))
            (multiple-value-prog1
                (let ((tbox-el+-p (and *use-elh-transformation*
                                       *use-elh-model-embedding*
                                       (tbox-el+-transformed-table tbox))))
                  (if (and (symbolp subsumer)
                           (symbolp subsumee)
                           (or (tbox-classified-p-internal tbox) tbox-el+-p))
                      (let ((subsumer-concept (get-tbox-concept tbox subsumer))
                            (subsumee-concept (get-tbox-concept tbox subsumee)))
                        (and (or (eq subsumer-concept subsumee-concept)
                                 (if tbox-el+-p
                                     (member subsumer-concept (concept-told-subsumers subsumee-concept))
                                   (member subsumer-concept (get-concept-ancestors subsumee-concept))))
                             t))
                    (let ((subsumer-concept (encode-update-concept-term tbox subsumer))
                          (subsumee-concept (encode-update-concept-term tbox subsumee)))
                      (or (eq subsumer-concept subsumee-concept)
                          (and tbox-el+-p
			       (symbolp subsumer)
			       (symbolp subsumee)
                               (member subsumer-concept (concept-told-subsumers subsumee-concept))
                               t)
                          (test-subsumes subsumer-concept subsumee-concept)))))
              (when-print-statistics
                (when-sat-statistics
                  (print-sat-statistics *standard-output*
                                        *race-statistics-stream*
                                        start
                                        (get-internal-run-time))))))))
      (with-race-trace-sublevel ("concept-subsumes-p"
                                 :arguments (list subsumer subsumee)
                                 :trace-result t)
        (subsumed-by subsumee subsumer)))))

(defun concept-satisfiable-p (concept-term tbox)
  (let ((tbox (and tbox (find-tbox tbox))))
    (if tbox
        (with-race-trace-sublevel ("concept-satisfiable-p"
                                   :arguments (list concept-term tbox)
                                   :trace-result t)
          (ensure-knowledge-base-state ':tbox-prepared tbox)
          (with-concept-definition-mapping tbox
            (let ((start (when-print-statistics
                           (when-sat-statistics
                             (get-internal-run-time)))))
              (multiple-value-prog1
                  (if (symbolp concept-term)
                      (let ((concept (get-tbox-concept tbox concept-term)))
                        (if (and (tbox-el+-transformed-table tbox)
                                 (subset-el+-p (concept-language concept)))
                            (if (eq (concept-model concept) (tbox-bottom-node tbox))
                                (progn
                                  (unless (member concept-term (concept-name-set (tbox-bottom-node tbox)))
                                    (add-concept-synonym tbox
                                                         (concept-name-set concept)
                                                         (tbox-bottom-node tbox)))
                                  nil)
                              t)
                          (or (test-atomic-concept-satisfiable tbox concept)
                              (unless (member concept-term (concept-name-set (tbox-bottom-node tbox)))
                                (add-concept-synonym tbox
                                                     (concept-name-set concept)
                                                     (tbox-bottom-node tbox)))
                              nil)))
                    (and (test-satisfiable tbox (encode-update-concept-term tbox concept-term)) t))
                (when-print-statistics
                  (when-sat-statistics
                    (print-sat-statistics *standard-output*
                                          *race-statistics-stream*
                                          start
                                          (get-internal-run-time))))))))
      (with-race-trace-sublevel ("concept-satisfiable-p"
                                 :arguments (list concept-term)
                                 :trace-result t)
        (and (satisfiable concept-term) t)))))

(defun concept-equivalent-p (concept-1 concept-2 tbox)
  "Test whether concept-1 is equivalent to concept-2"
  (and (concept-subsumes-p concept-1 concept-2 tbox)
       (concept-subsumes-p concept-2 concept-1 tbox)))

(defun concept-disjoint-p (concept-1 concept-2 tbox)
  "Test whether concept-1 and concept-2 are disjoint"
  (not (concept-satisfiable-p `(and ,concept-2 ,concept-1) tbox)))

(defun atomic-concept-children (concept-term tbox)
  (check-type tbox (or symbol tbox))
  (let* ((tbox (find-tbox tbox)))
    (with-race-trace-sublevel ("atomic-concept-children"
                               :arguments (list concept-term tbox)
                               :trace-result t)
      (ensure-knowledge-base-state ':tbox-classified tbox)
      (with-concept-definition-mapping tbox
        (filter-visibles (internalize-concept tbox concept-term)
                         #'concept-children-internal
                         #'concept-name-set)))))

(defun filter-visibles (concept accessor transformer)
  #+:debug (assert (concept-p-internal concept))
  (let ((values (funcall accessor concept)))
    (if (every #'concept-visible-p values)
      (if (eq transformer #'identity)
        values
        (mapcar transformer values))
      (let ((shadow-mark (gensym "S")))
        (mark-shadowing-concepts concept accessor shadow-mark)
        (let ((result (racer-remove-duplicates (filter-visibles-1 concept accessor shadow-mark))))
          (if (eq transformer #'identity)
            result
            (mapcar transformer result)))))))
          

(defun filter-visibles-1 (concept accessor shadow-mark)
  (let ((result nil))
    (loop for concept-1 in (funcall accessor concept) do
          (if (concept-visible-p concept-1)
            (if (not (eql (concept-mark1 concept-1) shadow-mark))
              (push concept-1 result))
            (setf result (nconc (filter-visibles-1 concept-1 accessor shadow-mark)
                                result))))
    result))

(defun mark-shadowing-concepts (concept accessor shadow-mark)
  (loop for concept-1 in (funcall accessor concept) do
        (if (concept-visible-p concept-1)
          (if (not (eql (concept-mark1 concept-1) shadow-mark))
            (propagate-shadow-mark concept-1 shadow-mark accessor))
          (mark-shadowing-concepts concept-1 accessor shadow-mark))))

(defun propagate-shadow-mark (concept shadow-mark accessor)
  (loop for concept-1 in (funcall accessor concept) 
        unless (eql (concept-mark1 concept-1) shadow-mark) 
        do
        (when (concept-visible-p concept-1)
          (setf (concept-mark1 concept-1) shadow-mark))
        (propagate-shadow-mark concept-1 shadow-mark accessor)))

(defmacro concept-children (concept-term &optional (tbox nil tbox-specified-p))
  (if tbox-specified-p
    `(atomic-concept-children ',concept-term
                              ',tbox)
    `(atomic-concept-children ',concept-term
                              (with-tbox-defined-check *current-tbox*
                                *current-tbox*))))

(defmacro concept-offspring (concept-term &optional (tbox nil tbox-specified-p))
  (if tbox-specified-p
    `(atomic-concept-children ',concept-term
                              ',tbox)
    `(atomic-concept-children ',concept-term
                              (with-tbox-defined-check *current-tbox*
                                *current-tbox*))))

(defun atomic-concept-parents (concept-term tbox)
  (check-type tbox (or symbol tbox))
  (let ((tbox (find-tbox tbox)))
    (with-race-trace-sublevel ("atomic-concept-parents"
                               :arguments (list concept-term tbox)
                               :trace-result t)
      (ensure-knowledge-base-state ':tbox-classified tbox)
      (with-concept-definition-mapping tbox
        (filter-visibles (internalize-concept tbox concept-term)
                         #'concept-parents-internal
                         #'concept-name-set)))))

(defmacro concept-parents (concept-term &optional (tbox nil tbox-specified-p))
  (if tbox-specified-p
    `(atomic-concept-parents ',concept-term
                             ',tbox)
    `(atomic-concept-parents ',concept-term
                             (with-tbox-defined-check *current-tbox*
                               *current-tbox*))))

(defun atomic-concept-synonyms (concept-term tbox)
  (check-type tbox (or symbol tbox))
  (let ((tbox (find-tbox tbox)))
    (with-race-trace-sublevel ("atomic-concept-synonyms"
                               :arguments (list concept-term tbox)
                               :trace-result t)
      (if (and (symbolp concept-term)
               (or (eq concept-term +bottom-symbol+)
                   (eq concept-term +krss-bottom-symbol+)))
        (progn
          (check-tbox-coherence tbox)
          (concept-name-set (tbox-bottom-node tbox)))
        (progn
          (ensure-knowledge-base-state ':tbox-classified tbox)
          (let ((concept (internalize-concept tbox concept-term)))
            (if (concept-visible-p concept)
              (concept-name-set concept)
              nil)))))))

(defmacro concept-synonyms (concept-term &optional (tbox nil tbox-specified-p))
  (if tbox-specified-p
    `(atomic-concept-synonyms ',concept-term
                              ',tbox)
    `(atomic-concept-synonyms ',concept-term
                              (with-tbox-defined-check *current-tbox*
                                *current-tbox*))))

(defun atomic-concept-descendants (concept-term tbox)
  (check-type tbox (or symbol tbox))
  (let ((tbox (find-tbox tbox)))
    (with-race-trace-sublevel ("atomic-concept-descendants"
                               :arguments (list concept-term tbox)
                               :trace-result t)
      (ensure-knowledge-base-state ':tbox-classified tbox)
      (with-concept-definition-mapping tbox
        (mapcar #'concept-name-set 
                (get-concept-offspring (internalize-concept tbox concept-term)))))))

(defmacro concept-descendants (concept-term &optional (tbox nil tbox-specified-p))
  (if tbox-specified-p
    `(atomic-concept-descendants ',concept-term
                                 ',tbox)
    `(atomic-concept-descendants ',concept-term
                                 (with-tbox-defined-check *current-tbox*
                                   *current-tbox*))))

(defun get-concept-offspring (concept)
  (let ((children (concept-children-internal concept)))
    (when children
      (loop for child in children
            for new-offspring = (concept-set-union new-offspring
                                                   (get-concept-offspring child))
            finally (return (concept-set-union (remove-if-not #'concept-visible-p
                                                              children)
                                               new-offspring))))))

(defun atomic-concept-ancestors (concept-term tbox)
  (check-type tbox (or symbol tbox))
  (let ((tbox (find-tbox tbox)))
    (with-race-trace-sublevel ("atomic-concept-ancestors"
                               :arguments (list concept-term tbox)
                               :trace-result t)
      (ensure-knowledge-base-state ':tbox-classified tbox)
      (with-concept-definition-mapping tbox
        (mapcar #'concept-name-set 
                (get-concept-ancestors (internalize-concept tbox concept-term)))))))

(defmacro concept-ancestors (concept-term &optional (tbox nil tbox-specified-p))
  (if tbox-specified-p
    `(atomic-concept-ancestors ',concept-term ',tbox)
    `(atomic-concept-ancestors ',concept-term
                                 (with-tbox-defined-check *current-tbox*
                                   *current-tbox*))))

(defun get-concept-ancestors (concept)
  (let ((parents (concept-parents-internal concept)))
    (when parents
      (loop for parent in parents
	    for new-offspring = (concept-set-union new-offspring
                                                   (get-concept-ancestors parent))
	    finally (return (concept-set-union (remove-if-not #'concept-visible-p
							      parents)
					       new-offspring))))))


(defun concept-is-primitive-p (concept-name &optional (tbox *current-tbox*))
  (setf tbox (find-tbox tbox))
  (check-type concept-name symbol)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (concept-primitive-p (get-tbox-concept tbox concept-name)))

(defmacro concept-is-primitive? (concept-name &optional (tbox nil tbox-specified-p))
  (if tbox-specified-p
    `(concept-is-primitive-p ',concept-name ',tbox)
    `(concept-is-primitive-p ',concept-name (with-tbox-defined-check *current-tbox*
                                              *current-tbox*))))

;;; ======================================================================

(defun decode-tbox-concept (concept-name tbox &optional (unfold nil))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-type concept-name symbol)
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (make-normalized-term (decode-concept (get-tbox-concept tbox concept-name) unfold)))


;;; ======================================================================

(defun create-tbox-clone (tbox &key (new-name nil) (overwrite nil))
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (check-type new-name (or symbol null))
  (when (and new-name (not overwrite) (find-tbox new-name nil))
    (error "TBox ~A already known: cannot clone TBox ~A using the same name"
           new-name (tbox-name tbox)))
  (with-race-trace-sublevel ("create-tbox-clone"
                             :arguments (list tbox new-name overwrite)
                             :trace-result t)
    (let ((clone (copy-tbox tbox))
          (new-name (or new-name (gentemp (symbol-name (tbox-name tbox))))))
      (setf (tbox-name clone) new-name)
      (setf (find-tbox new-name) clone)
      ;; (clone-dbox (find-dbox (tbox-name tbox)) new-name)
      (setf *current-tbox* clone)
      (tbox-name clone))))

(defmacro clone-tbox (tbox &key
                           (new-name nil new-name-specified-p)
                           (overwrite nil))
  `(if ,new-name-specified-p
     (create-tbox-clone ',tbox :new-name ',new-name :overwrite ,overwrite)
     (create-tbox-clone ',tbox :overwrite ,overwrite)))

(defun forget-tbox (tbox)
  (setf tbox (find-tbox tbox))
  (check-type tbox tbox)
  (when (find-tbox tbox nil)
    (when (eq *current-tbox* tbox)
      (setf *current-tbox* (find-tbox 'default)))
    (if (eq tbox (find-tbox 'default))
      (setf tbox (clear-default-tbox))
      (setf (find-tbox (tbox-name tbox)) nil))
    (list (tbox-name tbox)
          (loop for abox-name in (all-aboxes)
                for abox = (find-abox abox-name nil)
                when (and abox (eq (tbox-name tbox) (tbox-name (tbox abox))))
                collect abox-name and
                do (forget-abox abox)))))

(defun clear-default-tbox ()
  (setf *current-tbox*
        (make-tbox 'default nil nil nil nil
                   *default-tbox-concept-size* *default-tbox-role-size*))
  (setf (find-tbox 'default) *current-tbox*)
  *current-tbox*)

(defmacro delete-tbox (tbox)
  `(forget-tbox ',tbox))

(defun delete-all-tboxes ()
  (prog1
    (list (remove 'default (all-tboxes)) (all-aboxes))
    (setf *tbox-table* (racer-make-hash-table :size 50 :test #'equal))
    (setf *current-tbox*
          (make-tbox 'default nil nil nil nil
                     *default-tbox-concept-size* *default-tbox-role-size*))
    (setf (find-tbox 'default) *current-tbox*)
    (delete-all-aboxes)))

;;; ======================================================================

(defun initialize-default-tbox ()
  (setf (find-tbox 'default) 
    (make-tbox 'default nil nil nil nil
	       *default-tbox-concept-size* *default-tbox-role-size*))
  (setf *current-tbox* (find-tbox 'default)))

;;; ======================================================================

(defun concept-name-filter (concept)
  (loop for name in (concept-name-set concept)
        for count from 1
        collect (cond
                 ((eq name +top-symbol+) +krss-top-symbol+)
                 ((eq name +bottom-symbol+) +krss-bottom-symbol+)
                 ((eq name 'cl-user::|nil|) nil)
                 ((eq name 'cl-user::|top|) +krss-top-symbol+)
                 ((eq name 'cl-user::|bottom|) +krss-bottom-symbol+)
                 (t name))
        into result
        finally
        (when (rest result)
          (setf result (racer-remove-duplicates result)))
        (return (if (rest result)
                    result
                  (first result)))))

(defun concept-name-reverse-filter (name)
  (cond
   ((consp name) (concept-name-reverse-filter (first name)))
   ((eq name +krss-top-symbol+) +top-symbol+) 
   ((eq name +krss-bottom-symbol+) +bottom-symbol+)
   (t name)))

(defun verify-with-concept-tree (filename
                                 &key
                                 (tbox *current-tbox*)
                                 (verbose t)
                                 (children-p nil))
  (let ((start (get-internal-run-time))
        time
        (tree-file-case-sensitive
         (tree-file-case-sensitive-p filename
                                     tbox
                                     (lambda (name tbox)
                                       (symbol-name-case-sensitive-p name tbox #'get-tbox-concept)))))
    (ensure-knowledge-base-state ':tbox-classified tbox)
    (setf time (- (get-internal-run-time) start))
    (let ((error nil))
      (with-open-file (tree-file filename :direction :input)
        (when verbose
          (format t "~&Verifying TBox ~S with concept tree in file ~A..."
                  tbox filename))
        (labels ((filter-elem (elem)
                   (cond ((eq elem 'cl-user::|nil|) nil)
                         ((eq elem 'cl-user::|top|) +krss-top-symbol+)
                         ((eq elem 'cl-user::|bottom|) +krss-bottom-symbol+)
                         ((consp elem) (filter elem))
                         (t elem)))
               (filter (elem)
                 (if tree-file-case-sensitive
                     (if (consp elem)
                         (mapcar #'filter-elem elem)
                       (filter-elem elem))
                   elem)))
          (let ((readtable (if tree-file-case-sensitive 
                               (copy-readtable nil)
                             *readtable*)))
            (when tree-file-case-sensitive 
              (setf (readtable-case readtable) :preserve))
            (loop with n-concepts = (+ (length (tbox-encoded-concept-list tbox)) 2)
                  for (orig-concept-name parents children) = (let ((*readtable* readtable))
                                                               (read tree-file nil '(done done)))
                  for counter = 1 then (1+ counter)
                  until (and (eq orig-concept-name 'done) (eq parents 'done))
                  when (and verbose (> n-concepts 2000))
                  do (funcall *tick-function* (/ counter n-concepts))
                  unless (verify-concept-parents-children tbox
                                                          (filter orig-concept-name)
                                                          (filter parents)
                                                          (and children-p (filter children)))
                  do
                  #+:macosx (break)
                  (setf error t))))
        (if error
            (format t "~&Problem: Failed to verify TBox ~S with concept tree in file ~A"
                    tbox filename)
          (when verbose
            (format t "~&Successfully verified TBox ~S with concept tree in file ~A"
                    tbox filename)))))
    time))

(defun safe-set-xor-test (set-1 set-2)
  (cond ((symbolp set-1)
         (if (listp set-2)
           (member set-1 set-2)
           (eql set-1 set-2)))
        ((symbolp set-2)
         (if (listp set-1)
           (member set-2 set-1)
           (eql set-1 set-2)))
        (t (null (set-exclusive-or set-1 set-2)))))

(defun verify-concept-parents-children (tbox orig-concept-name parents children &optional (ignore-error nil))
  (with-concept-definition-mapping tbox
    (let* ((new-name (if (consp orig-concept-name)
                         (first orig-concept-name)
                       orig-concept-name))
           (concept (get-tbox-concept tbox new-name (not ignore-error))))
      (if (and ignore-error (null concept))
          nil
        (let* ((own-parents (filter-visibles concept #'concept-parents-internal #'concept-name-filter))
               (own-children (and children
                                  (filter-visibles concept 
                                                   #'concept-children-internal 
                                                   #'concept-name-filter)))
               (parent-diffs (not (name-set-sets-equal-p parents own-parents)))
               (children-diffs (and children (not (name-set-sets-equal-p children own-children)))))
          (if (or parent-diffs children-diffs)
              (progn
                (unless ignore-error
                  (when parent-diffs
                    (format t "~%Error: Difference in concept parents for ~S found:~%own parents: ~S~%~
                               asserted parents: ~S~%~
                               parents difference: ~S~%"
                            (concept-name-reverse-filter orig-concept-name)
                            own-parents parents 
                            (and parents (set-exclusive-or parents own-parents :test #'safe-set-xor-test))))
                  (when children-diffs
                    (format t "~%Error: Difference in concept children for ~S found:~%own children: ~S~%~
                               asserted children ~S~%~
                               children difference: ~S"
                            (concept-name-reverse-filter orig-concept-name)
                            own-children children
                            (and children (set-exclusive-or children own-children :test #'safe-set-xor-test)))))
                (break)
                nil)
            t))))))

(defun name-set-sets-equal-p (set-1 set-2)
  (and (eql (length set-1) (length set-2))
       (subsetp set-1 set-2 :test #'name-sets-equal-p)))

(defun name-sets-equal-p (set-1 set-2)
  (cond ((symbolp set-1)
         (if (listp set-2)
           (member set-1 set-2)
           (eql set-1 set-2)))
        ((symbolp set-2)
         (if (listp set-1)
           (member set-2 set-1)
           (eql set-1 set-2)))
        (t (and (eql (length set-1) (length set-2))
                (subsetp set-1 set-2)))))

(defun verify-with-concept-tree-list (tree-list &optional (tbox *current-tbox*) (ignore-error nil))
  (ensure-knowledge-base-state ':tbox-classified tbox)
  (loop with error-p = nil
        for (orig-concept-name parents children) in tree-list
        unless (verify-concept-parents-children tbox orig-concept-name parents children ignore-error)
        unless error-p
        do (setf error-p t)
        finally (return (not error-p))))

(defun print-concept-tree (&key (stream t) (tbox *current-tbox*) (mapping nil) (only-parents t)
                                   (as-list t))
  (ensure-knowledge-base-state ':tbox-classified tbox)
  (with-concept-definition-mapping tbox
    (let ((top
           (print-concept-relatives (tbox-top-node tbox) mapping stream only-parents))
          (main
           (loop for concept in (sort (copy-list (tbox-encoded-concept-list tbox)) #'string-lessp
                                      :key (lambda (x)
                                             (first (sort (copy-list (concept-name-set x))
                                                          #'string-lessp))))
                 
                 when (and (concept-visible-p concept)
                           (eq concept
                               (get-tbox-concept tbox
                                                 (first (sort (copy-list (concept-name-set concept))
                                                              #'string-lessp)))))
                 collect (print-concept-relatives concept mapping stream only-parents)))
          (bottom (print-concept-relatives (tbox-bottom-node tbox) mapping stream only-parents)))
      (if as-list
        (if stream
          (progn
            (format stream "~&~S~%" (append (list top) main (list bottom)))
            (values))
          (append (list top) main (list bottom)))
        (values)))))

(defun safe-sort (sequence)
  (sort (mapcar (lambda (x) (if (listp x)
                              (sort (copy-list x) #'string-lessp)
                              x))
                sequence)
        (lambda (x1 x2)
          (let ((new-x1 (if (symbolp x1)
                          x1
                          (first x1)))
                (new-x2 (if (symbolp x2)
                          x2
                          (first x2))))
            (string-lessp new-x1 new-x2)))))

(defun print-concept-relatives (concept mapping stream only-parents)
  (let ((relatives
         (if only-parents
           (map-names (list (concept-name-filter concept)
                            (safe-sort (filter-visibles concept
                                                         #'concept-parents-internal
                                                         #'concept-name-filter)))
                      mapping)
           (map-names (list (concept-name-filter concept)
                            (safe-sort (filter-visibles concept
                                                         #'concept-parents-internal
                                                         #'concept-name-filter))
                            (safe-sort (filter-visibles concept
                                                         #'concept-children-internal
                                                         #'concept-name-filter)))
                      mapping))))
    (if stream
      (print relatives stream)
      relatives)))

(defun dump-concept-tree (filename &key (tbox *current-tbox*) (only-parents t))
  (let ((*print-pretty* nil))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (print-concept-tree :stream stream :tbox tbox :only-parents only-parents :as-list nil))))

(defun dump-full-concept-tree (filename &key (tbox *current-tbox*))
  (let ((*print-pretty* nil))
    (with-concept-definition-mapping tbox
      (with-open-file (stream filename :direction :output :if-exists :supersede)
        (loop for (name nil nil) in (tbox-concept-axioms tbox)
              for concept = (get-tbox-concept tbox name) do
              (print (list name
                           (filter-visibles concept 
                                            #'concept-parents-internal
                                            #'concept-name-filter)
                           (filter-visibles concept 
                                            #'concept-children-internal
                                            #'concept-name-filter))
                     stream))))))

(defun print-concept-definition (stream name definition primitive)
  (let ((definition (if primitive
                        (if definition
                            `(implies ,name ,definition)
                          `(define-primitive-concept ,name))
                      `(define-concept ,name ,definition))))
    (if stream
        (print definition stream))
    definition))

(defun print-role-definition (stream role-name cd-attribute feature-p transitive-p parents
                                     inverse domain range datatype annotation-p compositions
                                     reflexive-p irreflexive-p asymmetric-p)
  (declare (ignore annotation-p))
  (let* ((internal-p (member role-name +internal-roles+))
         (definition
          (cond ((and datatype (not internal-p))
                 (if (eq datatype 't)
                     `(define-datatype-property ,role-name)
                   `(define-datatype-property ,role-name :range ,datatype)))
                ((and cd-attribute (not internal-p))
                 (if (eq cd-attribute 't)
                     `(define-concrete-domain-attribute ,role-name)
                   `(define-concrete-domain-attribute ,role-name :type ,cd-attribute)))
                (feature-p
                 `(define-primitive-attribute ,role-name
                                              .,(append (and transitive-p '(:transitive t))
                                                        (and parents
                                                             `(:parents ,(mapcar #'decode-role parents)))
                                                        (and inverse `(:inverse ,inverse))
                                                        (and domain `(:domain ,domain))
                                                        (and range `(:range ,range))
                                                        (and compositions
                                                             `(:compositions ,(mapcar (lambda (composition)
                                                                                        (mapcar #'decode-role 
                                                                                                composition))
                                                                                      compositions)))
                                                        (and reflexive-p '(:reflexive t))
                                                        (and irreflexive-p '(:irreflexive t))
                                                        (and asymmetric-p '(:asymmetric t)))))
                #|(annotation-p 
                            (if (eq datatype 't)
                              `(define-annotation-property ,role-name)
                              `(define-annotation-property ,role-name :range ,datatype)))|#
                ;; define-annotation-property not defined anywhere
                ((not internal-p)
                 `(define-primitive-role ,role-name
                                         .,(append (and transitive-p '(:transitive t))
                                                   (and parents
                                                        `(:parents ,(mapcar #'decode-role parents)))
                                                   (and inverse `(:inverse ,inverse))
                                                   (and domain `(:domain ,domain))
                                                   (and range `(:range ,range))
                                                   (and compositions
                                                        `(:compositions ,(mapcar (lambda (composition)
                                                                                   (mapcar #'decode-role 
                                                                                           composition))
                                                                                 compositions)))
                                                   (and reflexive-p '(:reflexive t))
                                                   (and irreflexive-p '(:irreflexive t))
                                                   (and asymmetric-p '(:asymmetric t))))))))
    (if stream
        (unless (null definition) 
          (print definition stream))
      definition)))


(defun print-roles-equivalent-declaration (stream role-name-1 role-name-2)
  (let ((definition `(roles-equivalent ,role-name-1 ,role-name-2)))
    (if stream
      (print definition stream)
      definition)))


(defun print-racer-tbox (&key (stream t) (tbox *current-tbox*) (original t)
                                 (avoid-duplicate-definitions nil)
                                 (test nil)
                                 (pretty nil) (anonymized nil)
                                 (header t))
  (let* ((orig-name (tbox-name tbox))
         (new-name (if (stringp orig-name)
                     (intern (string-upcase orig-name))
                     orig-name))
         (*print-pretty* pretty))
    (cond (test (print-test-tbox tbox new-name stream))
          (original (if anonymized
                      (print-anonymized-tbox tbox new-name stream :header header)
                      (print-original-tbox tbox new-name stream avoid-duplicate-definitions :header header)))
          (t (print-encoded-tbox tbox new-name stream :header header)))))

(race-inline (disjoint-and-p))

(defun disjoint-and-p (term)
  (and (listp term) (eq (first term) 'disjoint-and)))

(defun print-original-tbox (tbox tbox-name stream avoid-duplicate-definitions &key (header t))
  (when header
    (print `(in-tbox ,tbox-name
                     :size ,(length (tbox-original-concept-axioms tbox))
                     :role-size ,(length (tbox-role-axioms tbox)))
           stream))
  (if (tbox-signature tbox)
      (let ((signature (tbox-signature tbox)))
        (print `(signature :atomic-concepts ,(first signature) :roles ,(second signature)
                           :transitive-roles ,(third signature) :features ,(fourth signature)
                           :attributes ,(fifth signature))
               stream))
    #|(loop for role-axiom in (reverse (tbox-role-axioms tbox))
          do (print-role-definition stream
                                    (role-info-role-name role-axiom)
                                    (role-info-cd-attribute role-axiom)
                                    (role-info-feature-p role-axiom)
                                    (role-info-transitive-p role-axiom)
                                    (role-info-parents role-axiom)
                                    (role-info-inverse role-axiom)
                                    (role-info-domain role-axiom)
                                    (role-info-range role-axiom)
                                    (role-info-datatype role-axiom)
                                    (role-info-annotation-p role-axiom)
                                    (role-info-compositions role-axiom)
                                    (role-info-reflexive-p role-axiom)
                                    (role-info-irreflexive-p role-axiom)
                                    (role-info-asymmetric-p role-axiom)))|#)
  (loop with top-role = (tbox-object-top-role tbox)
        with bottom-role = (tbox-object-bottom-role tbox)
        with top-datatype-role = (tbox-datatype-top-role tbox)
        with bottom-datatype-role = (tbox-datatype-bottom-role tbox)
        for role being the hash-values in (tbox-role-store tbox) using (hash-key role-name)
        unless (or ;(gethash role-name (tbox-role-axioms-index tbox))
                   (role-internal-name-p role)
                   (role-internal-conjunction-p role)
                   (or (eq role top-role) (eq role bottom-role)
                       (eq role top-datatype-role) (eq role bottom-datatype-role)))
        do (print-role-definition stream role-name
                                  (role-cd-attribute role)
                                  (role-feature-p role)
                                  (role-transitive-p role)
                                  (mapcar #'role-name
                                          (remove-if #'is-predefined-role-p
                                                     (role-parents-internal role)))
                                  (let ((inverse (role-inverse-internal role)))
                                    (when (and inverse
                                               (not (role-internal-name-p inverse)))
                                      (role-name inverse)))
                                  (and (role-domain-restriction role)
                                       (decode-concept (role-domain-restriction role)))
                                  (and (role-range-restriction role)
                                       (decode-concept (role-range-restriction role)))
                                  (role-datatype role)
                                  (role-annotation-p role)
                                  (role-compositions role)
                                  (role-reflexive-p role)
                                  (role-irreflexive-p role)
                                  (role-asymmetric-p role)))
  (loop for (role-name-1 . role-name-2) in (tbox-role-synonyms tbox) do
        (print-roles-equivalent-declaration stream role-name-1 role-name-2))                
  (loop for disjoint-set being the hash-value of (tbox-disjoint-set tbox) do
        (print `(disjoint ,@disjoint-set) stream))
  (cond (avoid-duplicate-definitions
         (loop for (name definition primitive) in (reverse (tbox-concept-axioms tbox)) do
               (unless (and primitive (disjoint-and-p definition))
                 (print-concept-definition stream
                                           name
                                           definition
                                           primitive)))
         (loop for (left right) in (reverse (tbox-generalized-concept-inclusions tbox)) do
               (unless (disjoint-and-p right)
                 (print-concept-definition stream
                                           left
                                           right
                                           t))))
        (t (loop for (left right primitive) in (reverse (tbox-original-concept-axioms tbox)) do
                 (unless (and primitive (disjoint-and-p right))
                   (if (consp left)
                       (progn
                         (print-concept-definition stream
                                                   left
                                                   right
                                                   t)
                         (unless primitive
                           (print-concept-definition stream
                                                     right
                                                     left
                                                     t)))
                     (print-concept-definition stream
                                               left
                                               right
                                               primitive)))))))


(defun print-anonymized-tbox (tbox tbox-name stream &key (header t))
  (let* ((ordered-role-names
          (stable-union (loop with result = nil
			      for role-axiom in (reverse (tbox-role-axioms tbox))
			      for inverse = (role-info-inverse role-axiom)
                              do (pushnew (role-info-role-name role-axiom) result)
                              when inverse
                              do (pushnew inverse result)
                              finally (return result))
                        (loop with result = nil
                              for role being the hash-value of (tbox-role-store tbox) using (hash-key role-name)
                              for inverse = (role-inverse-internal role)
                              unless (or (gethash role-name (tbox-role-axioms-index tbox))
                                         (role-internal-name-p role)
                                         (role-internal-conjunction-p role))
                              do (pushnew (role-name role) result)
                              and
                              when inverse
                              do (pushnew (role-name inverse) result)
                              finally (return result))))
         (ordered-concept-names
          (sort (loop with result = nil
                  for concept being the hash-value of (tbox-concept-store tbox)
                      when (atomic-concept-p concept)
                      do (setf result (stable-union (concept-name-set concept) result))
                      finally (return result))
                #'<
                :key #'(lambda (cname)
                         (concept-hash-id (get-tbox-concept tbox cname)))))
         (role-mapping (racer-make-hash-table :size (length ordered-role-names)))
         (concept-mapping (racer-make-hash-table :size (length ordered-concept-names))))
    (let ((intersect (intersection ordered-role-names ordered-concept-names)))
      (when intersect
        (error "cannot anonymize TBox ~A since role and concept names are overlapping: ~S"
               tbox-name intersect)))
    (loop for role-name in ordered-role-names
          for count = 1 then (1+ count)
          do (setf (gethash role-name role-mapping) (intern (format nil "R~D" count))))
    (loop with specials = (list +top-symbol+ +krss-top-symbol+ +bottom-symbol+ +krss-bottom-symbol+)
          for concept-name in ordered-concept-names
          with count = 1 do
          (if (member concept-name specials)
            (setf (gethash concept-name concept-mapping) concept-name)
            (progn
              (setf (gethash concept-name concept-mapping) (intern (format nil "C~D" count)))
              (incf count))))
    (when header
      (print `(in-tbox ,tbox-name
                       :size ,(length (tbox-original-concept-axioms tbox))
                       :role-size ,(length (tbox-role-axioms tbox)))
             stream))
    (loop with top-role = (tbox-object-top-role tbox)
          with bottom-role = (tbox-object-bottom-role tbox)
          with top-datatype-role = (tbox-datatype-top-role tbox)
          with bottom-datatype-role = (tbox-datatype-bottom-role tbox)
          for role-name in ordered-role-names
          for role = (get-tbox-role tbox role-name)
          for inverse = (role-inverse-internal role)
          for domain = (role-domain-concept role)
          for range = (role-range-concept role)
          for datatype = (role-datatype role)
          for annotation-p = (role-annotation-p role)
          unless (or (eq role top-role) (eq role bottom-role)
                     (eq role top-datatype-role) (eq role bottom-datatype-role))
          do
          (print (map-names (print-role-definition nil
                                                   role-name
                                                   (role-cd-attribute role)
                                                   (role-feature-p role)
                                                   (role-transitive-p role)
                                                   (mapcar #'role-name
                                                           (remove-if #'is-predefined-role-p
                                                                      (role-parents-internal role)))
                                                   (when (and inverse
                                                              (not (role-internal-name-p inverse)))
                                                     (role-name (role-inverse-internal role)))
                                                   nil
                                                   nil
                                                   datatype
                                                   annotation-p
                                                   (role-compositions role)
                                                   (role-reflexive-p role)
                                                   (role-irreflexive-p role)
                                                   (role-asymmetric-p role))
                            role-mapping)
                 stream)
          (when domain
            (print (map-names (map-names `(implies (some ,role-name ,+top-symbol+) domain)
                                         concept-mapping)
                              role-mapping)
                   stream))
          (when range
            (print (map-names (map-names `(implies ,+top-symbol+ (all ,role-name ,range))
                                         concept-mapping)
                              role-mapping)
                   stream)))
    (loop for (role-name-1 . role-name-2) in (tbox-role-synonyms tbox) do
          (print-roles-equivalent-declaration stream role-name-1 role-name-2))                

    (loop for (name definition primitive) in (reverse (tbox-original-concept-axioms tbox)) do
          (print (map-names (map-names (print-concept-definition nil
                                                                 name
                                                                 definition
                                                                 primitive)
                                       concept-mapping)
                            role-mapping)
                 stream))

    (values concept-mapping role-mapping)))

(defun map-names (list mapping)
  (if mapping
    (mapcar #'(lambda (elem)
                (cond ((symbolp elem) (or (gethash elem mapping) elem))
                      ((listp elem) (map-names elem mapping))
                      (t elem)))
            list)
    list))

(defun print-test-tbox (tbox tbox-name stream)
  (let ((transitive-list (generate-transitive-roles-list tbox))
        (features-list (generate-features-list tbox))
        (roles-list (generate-roles-list tbox)))
    (print
     `(define-tbox ,(append
                     (list tbox-name)
                     (when transitive-list
                       (list :transitive-roles transitive-list))
                     (when features-list
                       (list :features features-list))
                     (when roles-list
                       (list :roles roles-list))
                     '(:debug t))
        ,@(append
           (loop for (role-name-1 . role-name-2) in (tbox-role-synonyms tbox)
                 collect (print-roles-equivalent-declaration stream role-name-1 role-name-2))
           (loop for (name definition nil) in (tbox-generalized-concept-inclusions tbox)
                 collect (list name definition ':inclusion))
           (loop for (name definition primitive) in (tbox-concept-axioms tbox) 
                 if primitive
                 collect (list name definition ':inclusion)
                 else collect (list name definition))))
     stream)))

(defun generate-transitive-roles-list (tbox)
  (loop for role-axiom in (tbox-role-axioms tbox)
        for role-name = (role-info-role-name role-axiom)
        for role = (get-tbox-role tbox role-name)
        when (user-defined-role-transitive-p role)
        collect role-name))

(defun generate-features-list (tbox)
  (loop with bottom-role = (tbox-object-bottom-role tbox)
        for role-axiom in (tbox-role-axioms tbox)
        for role-name = (role-info-role-name role-axiom)
        for role = (get-tbox-role tbox role-name)
        when (and (role-feature-p role) (not (eq role bottom-role)))
        collect role-name))

(defun generate-roles-list (tbox)
  (loop with top-role = (tbox-object-top-role tbox)
        with bottom-role = (tbox-object-bottom-role tbox)
        with top-datatype-role = (tbox-datatype-top-role tbox)
        with bottom-datatype-role = (tbox-datatype-bottom-role tbox)
        with internale-role-names = (list +top-object-role-symbol+
                                          +krss-top-object-role-symbol+
                                          +bottom-object-role-symbol+
                                          +krss-bottom-object-role-symbol+
                                          +top-datatype-role-symbol+
                                          +bottom-datatype-role-symbol+)
        for role-axiom in (tbox-role-axioms tbox)
        for role-name = (role-info-role-name role-axiom)
        for role = (get-tbox-role tbox role-name)
        for parents = (loop for parent-name in (role-parent-names role)
                            unless (member parent-name internale-role-names)
                            collect parent-name)
        unless (or (eq role top-role) (eq role bottom-role)
                   (eq role top-datatype-role) (eq role bottom-datatype-role))
        if parents
        collect `(,role-name :parents ,(if (rest parents)
                                        parents
                                        (first parents)))
        else collect role-name))



(defun print-encoded-tbox (tbox tbox-name stream &key (header t))
  (when header 
    (print `(in-tbox ,tbox-name
                     :size ,(length (tbox-encoded-concept-list tbox))
                     :role-size ,(length (tbox-role-axioms tbox)))
           stream))
  (loop for role in (tbox-encoded-role-list tbox)
        unless (or (is-predefined-role-p role) (role-internal-name-p role))
        do 
        (print-role-definition stream
                               (role-name role)
                               (role-cd-attribute role)
                               (role-feature-p role)
                               (role-transitive-p role)
                               (mapcar #'role-name
                                       (remove-if #'is-predefined-role-p
                                                  (role-parents-internal role)))
                               (when (and (role-inverse-internal role)
                                          (not (role-internal-name-p 
                                                (role-inverse-internal role))))
                                 (role-name (role-inverse-internal role)))
                               (role-domain-restriction role)
                               (role-range-restriction role)
                               (role-datatype role)
                               (role-annotation-p role)
                               (role-compositions role)
                               (role-reflexive-p role)
                               (role-irreflexive-p role)
                               (role-asymmetric-p role)))
  (loop for (role-name-1 . role-name-2) in (tbox-role-synonyms tbox) do
        (print-roles-equivalent-declaration stream role-name-1 role-name-2))                
  (loop for concept in (tbox-encoded-concept-list tbox)
        for name-set = (concept-name-set concept)
        for definition = (concept-encoded-definition concept)
        for negated-definition = (unless definition
                                   (concept-encoded-negated-definition concept))
        do
        (if definition
            (print-concept-definition stream
                                      (first name-set)
                                      (decode-concept definition)
                                      (concept-primitive-p concept))
          (if negated-definition
              (print `(implies (not ,(first name-set)) ,(decode-concept negated-definition)) stream)
            (print-concept-definition stream
                                      (first name-set)
                                      nil
                                      (concept-primitive-p concept))))
        (when (concept-elh-role-domain-qualifications concept)
          (loop for (role . qualifications) in (concept-elh-role-domain-qualifications concept) do
                (print `(implies (some ,(decode-role role) ,(decode-concept concept))
                                 ,(if (rest qualifications)
                                      `(and ,@(mapcar #'decode-concept qualifications))
                                    (decode-concept (first qualifications))))
                       stream)))
        (when (rest name-set)
          (loop for name in (rest name-set)
                with representative = (first name-set)
                do (print-concept-definition stream name representative nil))))
  (loop for meta-constraint-concept in (tbox-meta-constraint-concepts tbox) do
        (print `(implies ,+top-symbol+ ,(decode-concept meta-constraint-concept)) stream))
  (when (tbox-nary-absorption-table tbox)
    (loop for left being the hash-key of (tbox-nary-absorption-table tbox) using (hash-value right) do
          (print `(implies (and .,(mapcar #'decode-concept left)) ,(decode-concept right)) stream))))

(defun dump-tbox (filename &rest other-args 
                           &key (tbox *current-tbox*) (original t) (anonymized nil) (test nil)
                           (tree-filename nil) (pretty nil) &allow-other-keys)
  (with-concept-definition-mapping tbox
    (let ((if-exists-arg (second (member ':if-exists other-args))))
      (with-open-file (stream filename :direction :output
                              :if-exists (or if-exists-arg :supersede)
                              :if-does-not-exist :create)
        (unless anonymized
          (print-racer-info stream))
        (multiple-value-bind (concept-mapping role-mapping)
            (print-racer-tbox :stream stream :tbox tbox :original original
                              :anonymized anonymized :test test :pretty pretty)
          (fresh-line stream)
          (when tree-filename
            (let ((*print-pretty* nil))
              (with-open-file (stream tree-filename :direction :output
                                      :if-exists :supersede)
                (print-concept-tree :stream stream :tbox tbox :only-parents t
                                    :mapping concept-mapping :as-list nil)
                (fresh-line stream))))
          (values concept-mapping role-mapping))))))

(defun save-tbox (pathname-or-stream
                    &optional (tbox *current-tbox*)
                    &key (syntax :krss-like)
                    (transformed nil)
                    (avoid-duplicate-definitions nil)
                    (if-exists :supersede)
                    (if-does-not-exist :create)
                    (uri nil)
                    (anonymized nil)
                    (header t))
  (check-type tbox (or symbol tbox))
  (check-type pathname-or-stream (or pathname string file-stream))
  (check-type syntax (member :krss :krss-like :racer :test :shiq :xml :owl))
  (setf tbox (find-tbox tbox))
  (if (tbox-use-less-memory tbox)
    (racer-warn "~&Saving Tbox axioms ~A is only partially supported due to the ~
                 current memory management strategy. Some axioms might be missing. ~
                 Start Racer with option -x to enable these services.~%"
                (tbox-name tbox))      
    (flet ((save-tbox-1 (tbox stream)
             (case syntax
               ((:test :racer :krss :krss-like)
                (print-racer-tbox :tbox tbox
                                  :stream stream
                                  :original (not transformed)
                                  :avoid-duplicate-definitions avoid-duplicate-definitions
                                  :anonymized anonymized
                                  :test (eq syntax ':test)
                                  :header header))
               (:xml
                (print-xml-tbox tbox stream))
               (:shiq
                (print-shiq-tbox tbox stream))
               (:owl
                (print-owl-tbox tbox stream :uri uri)))))
      (let ((tbox (find-tbox tbox)))
        (ensure-knowledge-base-state ':tbox-prepared tbox)
        (with-concept-definition-mapping tbox
          (etypecase pathname-or-stream
            (file-stream (save-tbox-1 tbox pathname-or-stream))
            (pathname
             (with-open-file (stream pathname-or-stream
                                     ;; :element-type :character
                                     :external-format *file-external-format*
                                     :direction :output
                                     :if-exists if-exists 
                                     :if-does-not-exist if-does-not-exist)
               (save-tbox-1 tbox stream)))
            (string
             (with-open-file (stream pathname-or-stream
                                     ;; :element-type :character
                                     :external-format *file-external-format*
                                     :direction :output
                                     :if-exists if-exists 
                                     :if-does-not-exist if-does-not-exist)
               (save-tbox-1 tbox stream)))))))))

(defun taxonomy (&optional (tbox *current-tbox*))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-classified tbox)
  (let ((*print-pretty* nil))
    (print-concept-tree :stream nil :only-parents nil :tbox tbox)))

(defun dump-role-tree (filename &key (tbox *current-tbox*) (only-parents t))
  (let ((*print-pretty* nil))
    (with-open-file (stream filename :direction :output :if-exists :supersede)
      (print-role-tree :stream stream :tbox tbox :only-parents only-parents :as-list nil))))

(defun symbol-name-case-sensitive-p (name tbox accessor)
  (let* ((name (if (consp name)
                   (first name)
                 name))
         (concept-or-role (funcall accessor tbox name nil))
         (upper-case-p (string= (symbol-name name) (string-upcase (symbol-name name))))
         (lower-case-p (string= (symbol-name name) (string-downcase (symbol-name name)))))
    (if concept-or-role
        (or lower-case-p (not upper-case-p))
      (cond (concept-or-role nil)
            (upper-case-p
             (not (funcall accessor 
                           tbox
                           (intern (string-downcase (symbol-name name))
                                   (symbol-package name))
                           nil)))
            (lower-case-p
             (not (funcall accessor
                           tbox
                           (intern (string-upcase (symbol-name name))
                                   (symbol-package name))
                           nil)))
            (t (error "symbol ~A not found in TBox ~A" name (tbox-name tbox)))))))

(defun tree-file-case-sensitive-p (filename tbox case-sensitive-p)
  (when (string= (symbol-name 'foo) (symbol-name 'Foo))
    (with-open-file (tree-file filename :direction :input)
      (loop with count = 0
            with readtable = (copy-readtable nil)
            initially
            (setf (readtable-case readtable) :preserve)
            for entry = (let ((*readtable* readtable))
                          (read tree-file nil 'done))
            while (and (<= count 10) (not (eq entry 'done)))
            for name-1 = (first entry)
            for name-2 = (if (consp name-1)
                             (first name-1)
                           name-1)
            do
            (unless (eq (char (symbol-name name-2) 0) #\*)
              (incf count)
              (when (funcall case-sensitive-p name-2 tbox)
                (return t)))))))

(defun verify-with-role-tree (filename
                              &key
                              (tbox *current-tbox*)
                              (verbose t)
                              (children-p nil))
  (let ((start (get-internal-run-time))
        time)
    (ensure-knowledge-base-state ':tbox-prepared tbox)
    (setf time (- (get-internal-run-time) start))
    (let ((error nil)
          (role-problem nil)
          (tree-file-case-sensitive
           (tree-file-case-sensitive-p filename
                                       tbox
                                       (lambda (name tbox)
                                         (symbol-name-case-sensitive-p name tbox #'get-tbox-role)))))
      (with-open-file (tree-file filename :direction :input)
        (when verbose
          (format t "~&Verifying TBox ~S with role tree in file ~A..."
                  tbox filename))
        (let ((readtable (if tree-file-case-sensitive 
                             (copy-readtable nil)
                           *readtable*)))
          (when tree-file-case-sensitive 
            (setf (readtable-case readtable) :preserve))
          (loop with n-concepts = (+ (length (tbox-encoded-role-list tbox)) 2)
                for (role-name parents children) = (let ((*readtable* readtable))
                                                     (read tree-file nil '(done done)))
                for counter = 1 then (1+ counter)
                until (and (eq role-name 'done) (eq parents 'done))
                do
                (when (and verbose (> n-concepts 2000))
                  (funcall *tick-function* (/ counter n-concepts)))
                (multiple-value-bind (passed missing-role)
                    (verify-role-parents-children tbox
                                                  role-name
                                                  parents
                                                  (and children-p children)
                                                  (not verbose)
                                                  tree-file-case-sensitive)
                  (unless passed
                    (when (and (not role-problem) missing-role)
                      (setf role-problem missing-role))
                    (setf error t)))
                until role-problem))
        (if error
            (if role-problem
                (format t "~&Problem with unknown role ~A: Failed to verify TBox ~S with role tree in file ~A"
                        role-problem tbox filename)
              (format t "~&Problem: Failed to verify TBox ~S with role tree in file ~A"
                      tbox filename))
          (when verbose
            (format t "~&Successfully verified TBox ~S with role tree in file ~A"
                    tbox filename)))))
    time))

(defun verify-with-role-tree-list (role-list &optional (tbox *current-tbox*) (ignore-error nil))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (loop with error-p = nil
        with role-problem = nil
        for (orig-concept-name parents children) in role-list
        do
        (multiple-value-bind (passed missing-role)
            (verify-role-parents-children tbox orig-concept-name parents children ignore-error)
          (unless passed
            (when (and (not role-problem) missing-role)
              (setf role-problem missing-role))
            (unless error-p
              (setf error-p t))))
        until role-problem
        finally
        (return (values (not error-p) role-problem))))

(defun verify-role-parents-children (tbox
                                     orig-role-name
                                     parents
                                     children
                                     &optional
                                     (ignore-error nil)
                                     (case-sensitive-p nil))
  (labels ((safe-get-tbox-role (tbox role-name error-p)
             (let ((name (symbol-name role-name)))
               (or (get-tbox-role tbox role-name nil)
                   (and case-sensitive-p
                        (eq (char name 0) #\*)
                        (get-tbox-role tbox (intern (string-upcase (symbol-name role-name))
                                                    (symbol-package role-name))
                                       error-p)))))
           (normalize-roles (roles)
             (if case-sensitive-p
                 (if (symbolp roles)
                     (cond ((string= (symbol-name roles) "nil")
                            nil)
                           ((eq (char (symbol-name roles) 0) #\*)
                            (intern (string-upcase (symbol-name roles))
                                    (symbol-package roles)))
                           (t roles))
                   (mapcar #'normalize-roles roles))
               roles)))
    (let ((parents (normalize-roles parents))
          (children (normalize-roles children)))
      (with-concept-definition-mapping tbox
        (let* ((new-name (if (consp orig-role-name)
                             (if (eq (first orig-role-name) 'inv)
                                 orig-role-name
                               (first orig-role-name))
                           orig-role-name))
               (role-tmp (if (consp new-name)
                             (safe-get-tbox-role tbox (second new-name) (not ignore-error))
                           (safe-get-tbox-role tbox new-name (not ignore-error))))
               (role (if (and role-tmp (consp new-name))
                         (role-inverse-internal role-tmp)
                       role-tmp)))
          (if (and ignore-error (null role))
              (values nil new-name)
            (let* ((own-parents (mapcar #'decode-role (filter-parents tbox role)))
                   (own-children 
                    (and children (mapcar #'decode-role (filter-children tbox role))))
                   (parent-diffs (not (name-set-sets-equal-p parents own-parents)))
                   (children-diffs (and children (not (name-set-sets-equal-p children own-children)))))
              (if (or parent-diffs children-diffs)
                  (progn
                    (unless ignore-error
                      (when parent-diffs
                        (format t "~%Error: Difference in role parents for ~S found:~%own parents: ~S~%~
                               asserted parents: ~S~%~
                               parents difference: ~S~%"
                                orig-role-name
                                own-parents parents 
                                (and parents (set-exclusive-or parents own-parents :test #'safe-set-xor-test))))
                      (when children-diffs
                        (format t "~%Error: Difference in role children for ~S found:~%own children: ~S~%~
                               asserted children ~S~%~
                               children difference: ~S"
                                orig-role-name
                                own-children children
                                (and children (set-exclusive-or children own-children :test #'safe-set-xor-test)))))
                    nil)
                t))))))))

(defun print-role-tree (&key (stream t) (tbox *current-tbox*) (only-parents t) (as-list t))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (with-concept-definition-mapping tbox
    (let ((top
           (print-role-relatives tbox (tbox-object-top-role tbox) stream only-parents))
          (main
           (loop for role in (sort (copy-list (tbox-encoded-role-list tbox)) #'string-lessp
                                   :key #'role-name)
                 when (and (role-synonyms-internal role)
                           (not (is-predefined-role-p role))
                           (not (role-datatype role))
                           (not (role-internal-name-p role)))
                 collect (print-role-relatives tbox role stream only-parents)))
          (bottom (print-role-relatives tbox (tbox-object-bottom-role tbox) stream only-parents)))
      (if as-list
        (if stream
          (progn
            (format stream "~&~S~%" (append (list top) main (list bottom)))
            (values))
          (append (list top) main (list bottom)))
        (values)))))

(defun filter-roles (roles keep)
  (loop for role in roles
        when (or (eq role keep)
                 (and (not (is-predefined-role-p role))
                      (not (role-internal-name-p role))))
        collect role))

(defun filter-parents (tbox role)
  (if (or (role-datatype role) (role-cd-attribute role))
      (filter-roles (role-parents-internal role) (tbox-datatype-top-role tbox))
    (filter-roles (role-parents-internal role) (tbox-object-top-role tbox))))

(defun filter-children (tbox role)
  (if (or (role-datatype role) (role-cd-attribute role))
      (filter-roles (role-children-internal role) (tbox-datatype-bottom-role tbox))
    (filter-roles (role-children-internal role) (tbox-object-bottom-role tbox))))

(defun print-role-relatives (tbox role stream only-parents)
  (labels ((pretty-role (role)
             (if (rest (role-synonyms-internal role))
                 (mapcar #'decode-role (role-synonyms-internal role))
               (decode-role role))))
    (let ((relatives
           (if only-parents
               (list (pretty-role role)
                     (mapcar #'pretty-role (filter-parents tbox role)))
             (list (pretty-role role)
                   (mapcar #'pretty-role (filter-parents tbox role))
                   (mapcar #'pretty-role (filter-children tbox role))))))
      (if stream
          (print relatives stream)
        relatives))))

(defun print-tbox-tree (&optional (tbox *current-tbox*) (stream *standard-output*) (hide-role-inverses t))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-classified tbox)
  (with-concept-definition-mapping tbox
    (labels ((children (concept level)
               (if (zerop level)
                   (remove-duplicates
                    (append (filter-visibles concept 
                                             #'concept-children-internal
                                             #'identity)
                            (list (tbox-bottom-node tbox)))
                    :test 'equal)
                 (remove (tbox-bottom-node tbox)
                         (filter-visibles concept
                                          #'concept-children-internal
                                          #'identity))))
             (do-it (concept &optional (level 0) next)	     
	       (format stream "~&;;;")
	       (if (zerop level)
                 (format stream "  ~A~%" (reverse (concept-name-set (tbox-top-node tbox))))
	         (progn 
		   (dolist (item (butlast next))
		     (if item 
                       (format stream "   |")
		       (format   stream "    ")))
		   (let ((last (first (last next))))
		     (if last
                       (format stream "   |___~A~%"
                               (reverse (concept-name-set concept)))
		       (format stream   "   \\___~A~%"
                               (reverse (concept-name-set concept)))))))
	       (let ((last (first (last (children concept level)))))
	         (dolist (child (children concept level))
		   (do-it child (1+ level) (append next (list (not (eq child last)))))))))
      (format stream "~&;;; Concept tree for TBox ~A~%" (tbox-name tbox))
      (do-it (tbox-top-node tbox))
      (format stream "~&;;;~%")
      (print-tbox-role-tree tbox stream hide-role-inverses)
      (values))))


(defun print-tbox-role-tree (&optional (tbox *current-tbox*) (stream *standard-output*) (hide-inverses t))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state ':tbox-prepared tbox)
  (with-concept-definition-mapping tbox
    (labels ((filter-roles (list hide-inverses &optional (allow-removed-roles nil))
               (loop for role in list
                     unless (or ;(is-predefined-role-p role)
                                (role-internal-conjunction-p role)
                                (role-datatype role)
                                (and (not allow-removed-roles)
                                     (role-removed-p role))
                                (and hide-inverses (role-internal-name-p role)))
                     collect role))
             (pretty-decode (role)
               (let ((flag
                      (cond ((role-transitive-p role) "+")
                            ((role-feature-p role) "!")
                            (t ""))))
                 (format nil "~A~A" (decode-role role) flag)))
             (pretty-role (role)
               (if (rest (role-synonyms-internal role))
                   (mapcar #'pretty-decode (filter-roles (role-synonyms-internal role) hide-inverses t))
                 (pretty-decode role)))
             (children (role level)
               (if (zerop level)
                 (append (filter-roles (role-children-internal role) hide-inverses)
                         (list (tbox-object-bottom-role tbox)))
                 (remove (tbox-object-bottom-role tbox)
                         (filter-roles (role-children-internal role) hide-inverses))))
             (do-it (role &optional (level 0) next)	     
	       (format stream "~&;;;")
	       (if (zerop level)
                   (format stream "  ~A~%" (pretty-role (tbox-object-top-role tbox)))
	         (progn 
		   (dolist (item (butlast next))
		     (if item 
                       (format stream "   |")
		       (format   stream "    ")))
		   (let ((last (first (last next))))
		     (if last
                       (format stream "   |___~A~%" (pretty-role role))
		       (format stream   "   \\___~A~%" (pretty-role role))))))
	       (let ((last (first (last (children role level)))))
	         (dolist (child (children role level))
		   (do-it child (1+ level) (append next (list (not (eq child last)))))))))
      (format stream "~&;;; Role tree for TBox ~A~%" (tbox-name tbox))
      (do-it (tbox-object-top-role tbox))
      (values))))


;;; ======================================================================

(defun print-shiq-tbox (tbox stream)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (loop for role-axiom in (reverse (tbox-role-axioms tbox))
      do (shiq-print-role-declarations stream
				       (role-info-role-name role-axiom)
				       (role-info-feature-p role-axiom)
				       (role-info-transitive-p role-axiom)
				       (role-info-parents role-axiom)
				       (role-info-inverse role-axiom)))
  (loop for disjoint-set being the hash-value of (tbox-disjoint-set tbox) do
        (shiq-print-disjoint-axiom stream disjoint-set))
  (let ((defined-concepts nil))
    (loop for (name-or-concept definition primitive) in (reverse (tbox-original-concept-axioms tbox)) do
          (unless (and primitive (disjoint-and-p definition))
            (shiq-print-concept-definition stream
                                           name-or-concept
                                           definition
                                           primitive
                                           (and (symbolp name-or-concept)
                                                (find name-or-concept defined-concepts)))
            (if (symbolp name-or-concept)
		(push name-or-concept defined-concepts)))))
  (loop for role-axiom in (reverse (tbox-role-axioms tbox))
      do (shiq-print-domain-and-range-declarations stream
						   (role-info-role-name role-axiom)
						   (role-info-domain role-axiom)
						   (role-info-range role-axiom)))
  (loop for (role-name-1 . role-name-2) in (tbox-role-synonyms tbox) do
        (print-roles-equivalent-declaration stream role-name-1 role-name-2)))

(defun shiq-print-role-declarations (stream role-name feature-p transitive-p parents inverse)
  (cond (feature-p
         (format stream "~%(defprimattribute ~A" role-name)
         (if parents
             (format stream " :parents ~A" parents))
         (format stream ")"))
        (t (format stream "~%(defprimrole ~A" role-name)
           (if parents
             (format stream " :parents ~A" parents))
           (if transitive-p 
             (format stream " :transitive t"))
           (format stream ")")))
  (if inverse
    (error "TBoxes with inverse declarations cannot be saved in SHIQ format.")))

(defun shiq-print-concept-definition (stream name-or-concept definition primitive already-defined)
  (cond (primitive
         (if (or (not (symbolp name-or-concept)) already-defined)
           (format stream "~%(implies ~A" name-or-concept)
           (format stream "~%(defprimconcept ~A" name-or-concept))
         (if definition
           (format stream " ~A" definition))
         (format stream ")"))
        ((and (symbolp name-or-concept) (not already-defined))
         (format stream "~%(defconcept ~A ~A)" name-or-concept definition))
        (t (format stream "~%(implies ~A ~A)~
                           ~%(implies ~A ~A)" 
                   name-or-concept definition
                   definition name-or-concept))))

(defun shiq-print-disjoint-axiom (stream concepts)
  (loop for concepts1 on concepts
        when (rest concepts1) do
        (format stream "~%(implies ~A ~A)"
                (first concepts1)
                `(not (or .,(rest concepts1))))))

(defun shiq-print-domain-and-range-declarations (stream role-name domain range)
  (when domain
    (format stream "~%(implies (some ~A top) ~A)"
            role-name 
            domain))
  (when range
    (format stream "~%(implies top (all ~A ~A))"
            role-name 
            range)))

;;; ======================================================================

(defun get-concept-pmodel (concept-expr tbox)
  (check-type concept-expr (or cons symbol))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (ensure-knowledge-base-state :tbox-prepared tbox)
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
                         :racer-remove-constraint-duplicates-table (tbox-racer-remove-constraint-duplicates-table tbox)
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
      (let* ((*meta-constraint-concepts* (tbox-meta-constraint-concepts tbox))
             (*blocking-possibly-required*
              (or *encode-roles-as-transitive*
                  *meta-constraint-concepts*
                  (tbox-blocking-possibly-required tbox))))
        (get-concept-pmodel-internal (encode-concept-term concept-expr))))))

(defun get-concept-pmodel-internal (concept)
  (let ((model (get-cached-concept-model concept)))
    (if (model-info-p model)
        (if (full-model-info-p model)
            (list (when (atomic-concept-p (model-concept model))
                    (concept-term (model-concept model)))
                  (nconc (mapcar #'concept-term (model-det-positive-literals model))
                         (mapcar #'concept-term (model-positive-literals model)))
                  (nconc (mapcar #'concept-term (model-det-negative-literals model))
                         (mapcar #'concept-term (model-negative-literals model)))
                  (racer-remove-duplicates (mapcar (lambda (concept)
                                                     (decode-role (concept-role concept)))
                                                   (model-exists-models model)))
                  (racer-remove-duplicates (mapcar (lambda (concept)
                                                     (decode-role (concept-role concept)))
                                                   (model-restrict-models model)))
                  nil ;attributes
                  nil ; ensured attributes
                  (not (model-non-deterministic-p model))
                  )
          (list (when (atomic-concept-p (model-concept model))
                  (concept-term (model-concept model)))
                (mapcar #'concept-term (model-det-positive-literals model))
                nil ; no negated names
                (racer-remove-duplicates (mapcar (lambda (concept)
                                                   (decode-role (concept-role concept)))
                                                 (model-exists-models model)))
                nil ; no universal restrictions
                nil ;attributes
                nil ; ensured attributes
                t ; deterministic
                ))
      (if (incoherent-model-p model)
          (list :incoherent)
        (progn
          #+:debug (assert (or (atomic-concept-p model) (concept-negated-concept model)))
          (if (atomic-concept-p model)
              (let ((name (concept-term model)))
                (list name ;name
                      (list name) ;pos lits
                      nil ;neg lits
                      nil ;exists
                      nil ;restricts
                      nil ;attributes
                      nil ; ensured attributes
                      t ;unique
                      ))
            (let ((name (concept-term (concept-negated-concept model))))
              (list nil ;name
                    nil ;pos lits
                    (list name) ;neg lits
                    nil ;exists
                    nil ;restricts
                    nil ;attributes
                    nil ; ensured attributes
                    t ;unique
                    ))))))))

(defun remove-prefixes (model-as-list tbox &optional (result nil))
  (cond ((null model-as-list)
         (reverse result))
        ((listp (car model-as-list))
         (remove-prefixes (cdr model-as-list)
                          tbox
                          (cons (remove-prefixes (car model-as-list) tbox nil) result)))
        ((symbolp (car model-as-list))
         (let ((stripped-uri (remove-prefix (car model-as-list) tbox)))
           (if (string= (car model-as-list) stripped-uri)
               (remove-prefixes (cdr model-as-list)
                                tbox
                                (cons (car model-as-list) result))
             (remove-prefixes (cdr model-as-list)
                              tbox
                              (cons (intern (subseq (symbol-name stripped-uri) 1)) result)))))
        (t (remove-prefixes (cdr model-as-list)
                            tbox
                            (cons (car model-as-list) result)))))

(defun print-extended-concept-tree (&key (stream t) (tbox *current-tbox*) (as-list t) (remove-uri-prefix t))
  (ensure-knowledge-base-state ':tbox-classified tbox)
  (with-concept-definition-mapping tbox
    (let* ((*meta-constraint-concepts* (tbox-meta-constraint-concepts tbox))
           (*blocking-possibly-required*
            (or *encode-roles-as-transitive*
                *meta-constraint-concepts*
                (tbox-blocking-possibly-required tbox)))
           (top
            (print-extended-concept-relatives (tbox-top-node tbox) tbox stream remove-uri-prefix))
           (main
            (loop for concept in (sort (copy-list (tbox-encoded-concept-list tbox)) #'string-lessp
                                       :key (lambda (x)
                                              (first (sort (copy-list (concept-name-set x))
                                                           #'string-lessp))))
                  when (and (concept-visible-p concept)
                            (eq concept
                                (get-tbox-concept tbox
                                                  (first (sort (copy-list (concept-name-set concept))
                                                               #'string-lessp)))))
                  collect (print-extended-concept-relatives concept tbox stream remove-uri-prefix)))
           (bottom (print-extended-concept-relatives (tbox-bottom-node tbox) tbox stream remove-uri-prefix)))
      (if as-list
          (if stream
              (progn
                (format stream "~&~S~%" (append (list top) main (list bottom)))
                (values))
            (append (list top) main (list bottom)))
        (values)))))

(defun print-extended-concept-relatives (concept tbox stream remove-uri-prefix)
  (let* ((relatives-1
          (list (concept-name-filter concept)
                (safe-sort (filter-visibles concept
                                            #'concept-parents-internal
                                            #'concept-name-filter))
                (safe-sort (filter-visibles concept
                                            #'concept-told-subsumers
                                            #'concept-name-filter))
                (safe-sort (filter-visibles concept
                                            #'concept-told-disjoints
                                            #'concept-name-filter))
                (get-concept-pmodel-internal concept)
                (get-concept-pmodel-internal (concept-negated-concept concept))
                ))
         (relatives-2 (if remove-uri-prefix 
                          (remove-prefixes relatives-1 tbox)
                        relatives-1)))
    (if stream
        (print relatives-2 stream)
      relatives-2)))

(defun save-extended-concept-tree-tbox (tbox-name tree-filename xmas-tree-filename)
  (let ((tbox (find-tbox tbox-name))
        (*tbox-verbose* nil)
        (*print-pretty* nil))
    (when tree-filename
      (with-open-file (stream tree-filename
                              :direction :output 
                              :if-does-not-exist :create 
                              :if-exists :supersede)
        (format t "~&Writing file ~A..." tree-filename)
        (print-extended-concept-tree :tbox tbox :stream stream :as-list nil)))
    (when xmas-tree-filename
      (print-tbox-xmas-tree :tbox tbox :format :tex :tex-filename xmas-tree-filename))))

(defun save-owl-extended-concept-tree (owl-filename tree-filename xmas-tree-filename)
  (let ((filename-type (string-upcase (pathname-type owl-filename))))
    (cond ((equal filename-type "OWL")
           (owl-read-file owl-filename :kb-name 'save-pmodels-kb)
           (save-extended-concept-tree-tbox 'save-pmodels-kb tree-filename xmas-tree-filename))
          ((equal filename-type "IMG")
           (restore-tbox-image owl-filename)
           (save-extended-concept-tree-tbox *current-tbox* tree-filename xmas-tree-filename))
          (t (error "Cannot load file ~A" owl-filename)))))

(defun save-lisp-extended-concept-tree (lisp-filename tree-filename xmas-tree-filename)
  (load lisp-filename :verbose nil)
  (save-extended-concept-tree-tbox (current-tbox) tree-filename xmas-tree-filename))

(defun save-current-extended-concept-tree (filename)
  (let* ((type (string-upcase (pathname-type filename)))
         (type-length (length type))
         (tree-filename (concatenate 'string (subseq filename 0 (- (length filename) type-length)) "tree"))
         (xmas-tree-filename
          (concatenate 'string (subseq filename 0 (- (length filename) type-length)) "tex")))
    (save-extended-concept-tree-tbox (current-tbox) tree-filename xmas-tree-filename)))

(defun save-extended-concept-trees (folder-name &key (excluded-files nil) (supersede nil) (xmas-tree t))
  (let ((files (if excluded-files
                   (set-difference (directory folder-name) excluded-files :test 'equal)
                 (directory folder-name))))
    (loop for file in files do
          (save-extended-concept-tree (namestring file) supersede xmas-tree))))

(defun save-extended-concept-tree (filename &optional (supersede nil) (xmas-tree t))
  (let* ((type (string-upcase (pathname-type filename)))
         (type-length (length type))
         (tree-filename-1 (concatenate 'string (subseq filename 0 (- (length filename) type-length)) "tree"))
         (xmas-tree-filename-1
          (when xmas-tree
            (concatenate 'string (subseq filename 0 (- (length filename) type-length)) "tex")))
         (tree-filename-2
          (when (or supersede (not (probe-file tree-filename-1)))
            tree-filename-1))
         (xmas-tree-filename-2
          (when (or supersede (and xmas-tree-filename-1 (not (probe-file xmas-tree-filename-1))))
            xmas-tree-filename-1)))
    (when (or supersede tree-filename-2 xmas-tree-filename-2)
      (handler-case
          (cond ((equal type "OWL")
                 (save-owl-extended-concept-tree filename tree-filename-2 xmas-tree-filename-2))
                ((equal type "LISP")
                 (save-lisp-extended-concept-tree filename tree-filename-2 xmas-tree-filename-2))
                (t (format t "~%Unknown file type ~A for file ~A~%" type filename)))
        (error ()
          (format t "~%Error in processing ~A.~%File is ignored.~%" filename))))))

(defun create-tbox-role-range-concept-list (tbox)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (unless (tbox-index-structures-complete-p tbox)
    (ensure-knowledge-base-state ':tbox-prepared tbox))
  (with-concept-definition-mapping tbox
    (unless (or (tbox-role-range-concepts tbox)
                (not (superset-some-all-p (tbox-language tbox))))
      (let ((normalized-concept-axioms
             (nconc
              (loop for concept in (tbox-encoded-concept-list tbox)
                    for definition = (concept-encoded-definition concept)
                    for new-concept-1 = nil
                    for new-concept-2 = nil
                    do
                    (if (concept-primitive-p concept)
                        (if definition
                            (when (superset-all-p (concept-language definition))
                              (setf new-concept-1 `(or ,(concept-negated-concept concept) ,definition)))
                          (let ((negated-definition (concept-encoded-negated-definition concept)))
                            (when (and negated-definition (superset-all-p (concept-language negated-definition)))
                              (setf new-concept-1 `(or ,concept ,negated-definition)))))
                      (progn
                        (when (superset-all-p (concept-language definition))
                          (setf new-concept-1 `(or ,(concept-negated-concept concept) ,definition)))
                        (when (superset-all-p (concept-language (concept-negated-concept definition)))
                          (setf new-concept-2 `(or ,(concept-negated-concept definition) ,concept)))))
                    (when new-concept-1
                      (setf new-concept-1 (encode-concept-term new-concept-1)))
                    (when new-concept-2
                      (setf new-concept-2 (encode-concept-term new-concept-2)))
                    when (and new-concept-1 (not (is-top-concept-p new-concept-1)))
                    collect new-concept-1
                    when (and new-concept-2 (not (is-top-concept-p new-concept-2)))
                    collect new-concept-2)
              (loop with processed-p = (racer-make-hash-table :size (length (tbox-encoded-role-list tbox)))
                    with bottom = (tbox-bottom-node tbox)
                    for role in (tbox-encoded-role-list tbox)
                    for role-inverse = (role-inverse-internal role)
                    for domain-concept = nil
                    for range-concept = nil
                    unless (or (is-predefined-role-p role)
                               (role-datatype role)
                               (role-cd-attribute role)
                               (role-internal-name-p role)
                               (not (or (role-domain-restriction role) (role-range-restriction role)))
                               (gethash role processed-p)
                               (gethash role-inverse processed-p))
                    do
                    (setf (gethash role processed-p) t)
                    (setf (gethash role-inverse processed-p) t)
                    and
                    when (role-domain-restriction role)
                    do 
                    (setf domain-concept 
                          (encode-concept-term `(or (all ,role ,bottom) ,(role-domain-restriction role))))
                    and
                    unless (is-top-concept-p domain-concept)
                    collect domain-concept
                    end
                    end
                    and
                    when (role-range-restriction role)
                    do
                    (setf range-concept (encode-concept-term `(all ,role ,(role-range-restriction role))))
                    and
                    unless (is-top-concept-p range-concept)
                    collect range-concept)
              (when (tbox-nary-absorption-table tbox)
                (loop for lhs being the hash-key of (tbox-nary-absorption-table tbox) using (hash-value rhs)
                      when (superset-all-p (concept-language rhs))
                      collect (encode-concept-term `(or (not (and ,@lhs)) ,rhs))))
              (loop for concept in (tbox-meta-constraint-concepts tbox)
                    when (superset-all-p (concept-language concept))
                    collect concept))))
        (when normalized-concept-axioms
          (loop with changed = nil
                for concept in normalized-concept-axioms
                if (and-concept-p concept)
                append (flattened-and-concept-list concept) into new-normalized-concept-axioms
                and
                do (setf changed t)
                else
                collect concept into new-normalized-concept-axioms
                finally
                (when changed
                  (setf normalized-concept-axioms new-normalized-concept-axioms))))
        ;;(setf (tbox-role-range-concepts tbox) normalized-concept-axioms)
        (setf (tbox-role-range-concepts tbox) (racer-make-hash-table :size (length (tbox-encoded-role-list tbox))))
        (traverse-role-range-concepts (tbox-role-range-concepts tbox) normalized-concept-axioms)))))

(defun traverse-role-range-concepts (ht concepts)
  (loop for concept in concepts do
        (unless (atomic-concept-p concept)
          (if (all-concept-p concept)
              (pushnew (concept-term concept) (gethash (concept-role concept) ht))
            (unless (atomic-concept-p (concept-term concept))
              (traverse-role-range-concepts ht (concept-term concept)))))))
                                                   
    

;;; ======================================================================
;;;
;;; Debugging support.
;;;

#|
(defvar *visited-concepts* nil)

(defun show-tbox (name &optional (show-bottom nil))
  (defclass *bottom* () ())
  (defclass *r* () ())
  (let* ((tbox (find-tbox name))
         (*visited-concepts* (unless show-bottom
                               (list (tbox-bottom-node tbox))))
         (top (tbox-top-node tbox))
         (*current-tbox* tbox))
    (loop for role being the hash-value of (tbox-role-store tbox) do
          (eval `(defclass ,(mark-as-role (role-name role))
                   ,(or (mapcar #'mark-as-role
                                (sort-supers (role-parent-names role)))
                        '(*r*))
                   ())))
    (show-tbox-1 (list top)))
  (make-instance '*bottom*))

(defclass deleted ()())

(defun mark-as-role (role-name)
  (let* ((role (get-tbox-role *current-tbox* role-name))
         (suffix (cond ((role-transitive-p role) "*")
                       ((role-feature-p role) "!")
                       (t ""))))
    (intern (concatenate 'string (symbol-name role-name) suffix))))

(defun sort-supers (names)
  (sort (copy-list names)
        #'(lambda (name1 name2)
           (string< (symbol-name name1)
                    (symbol-name name2)))))

(defun show-tbox-1 (concepts)
  (loop for concept in concepts 
        unless (member concept *visited-concepts*) do
        (push concept *visited-concepts*)
        (eval `(defclass ,(first (concept-name-set concept))
                 ,(sort-supers 
                   (mapcar #'(lambda (parent)
                               (first (concept-name-set parent)))
                           (concept-parents-internal concept))) 
                 ()))
        #|(loop for synonym in (concept-synonyms concept) do
              `(defclass ,(concept-name synonym)
                 (deleted) 
                 ()))|#
        (show-tbox-1 (concept-children-internal concept))))

|#

;;; ======================================================================

(defun print-tbox-xmas-tree (&key (tbox *current-tbox*)
                                  (format ':ascii)
                                  (tex-filename nil)
                                  (max-width 40)
                                  (print-width nil))
  (with-tbox-defined-check *current-tbox*
    (ensure-knowledge-base-state :tbox-classified tbox)
    (let ((table (make-array 100 :initial-element nil :adjustable t :fill-pointer 0)))
      (let ((children (concept-children-internal (tbox-top-node tbox))))
        (vector-push-extend 1 table)
        (when children
          (collect-tbox-width-info table children)))
      (let ((max-tree-width (loop for width across table
                                  maximize width))
            (height (length table)))
        (if (eq format ':tex)
            (with-open-file (stream
                             (or tex-filename (format nil "~A.tex" (tbox-name tbox)))
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists :supersede)
              (format stream "\\documentstyle{article}~%")
              (format stream "\\begin{document}~%")
              (format stream "% Tbox ~A ~%"
                      (if tex-filename 
                          (pathname-name tex-filename)
                          (tbox-name tbox)))
              (format stream "%~A" (print-tbox-info tbox nil))
              (format stream "\\setlength{\\unitlength}{1mm}~%")
              (format stream "\\begin{picture}(~D,~D)~%" (+ max-width 4) (+ 4 (* 2 height)))
              (format stream "\\put(-1,-2){\\framebox(~D,~D){}}~%" (+ max-width 4) (+ 4 (* 2 height)))
              (loop for width across table
                for number-of-chars = (max 1 (round (* max-width (/ width max-tree-width))))
                for space-chars = (truncate (/ (- max-width number-of-chars) 2))
                for y from (* 2 (1- height)) downto 0 by 2
                do
                (format stream "\\put(~D,~D){\\line(1,0){~D}}~%" (1+ space-chars) y number-of-chars)
                (when print-width
                  (format stream "\\put(~D,~D){\\scriptsize ~D}~%" (+ max-width 4) (1- y) width)))
              (format stream "\\end{picture}~%")
              (format stream "\\end{document}~%"))
          (loop for width across table
                for number-of-chars = (round (* max-width (/ width max-tree-width)))
                for space-chars = (truncate (/ (- max-width number-of-chars) 2))
                do
                (format t "~%")
                (loop for i from 1 to space-chars do
                      (format t " "))
                (loop for i from 1 to number-of-chars do
                      (format t "*"))))))))

(defun collect-tbox-width-info (table nodes)
  (vector-push-extend (length nodes) table)
  (loop for node in nodes
        append (concept-children-internal node) into children
        finally
        (when children
          (setf children (remove-duplicates children))
          (collect-tbox-width-info table children))))

;;; ======================================================================

(defun find-tbox-no-error (sym)
  (find-tbox sym nil))

(defun find-concept (sym)
  (get-tbox-concept *current-tbox* sym nil))

(defun find-role (sym)
  (get-tbox-role *current-tbox* sym nil))
