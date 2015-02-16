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

(defparameter *check-subscriptions-inhibited* nil)

(defparameter *realization-in-progress* nil)

(defparameter *minimum-n-individuals-for-realization-progress-indication* 20)

(defvar *abox-version* 0)

(declaim (special *file-external-format*))

(defun new-abox-version ()
  (incf *abox-version*))

(defun transform-concepts (concepts)
  (if (null (rest concepts))
    (first concepts)
    `(and . ,concepts)))

(defparameter *no-signature-checks* nil)

(defmacro without-signature-checks (&body forms)
  `(let ((*no-signature-checks* t))
     .,forms))

;;; ======================================================================

(defstruct (abox (:constructor make-abox-internal)
                 (:copier copy-abox-internal))
  (name nil)
  (tbox nil)
  (tbox-identification nil)
  (individuals-table (racer-make-hash-table))
  (individuals-list nil)
  (objects-table (racer-make-hash-table))
  (objects-list nil)
  (object-synonym-table nil)
  (subgraphs nil)
  (index-structures-complete-p nil)
  (concept-individuals (racer-make-hash-table))
  (concept-non-individuals (racer-make-hash-table))
  (language *dl-empty*)
  (realized-p-internal nil)
  (coherent-p ':dont-know)
  (una-coherent-p ':dont-know)
  (individual-axioms nil)
  (individual-axioms-index (make-abox-individual-axioms-index))
  (role-axioms-index (make-abox-role-axioms-index))
  (role-axioms nil)
  (negated-role-axioms nil)
  (negated-role-axioms-index (make-abox-role-axioms-index))
  (role-axioms-not-holding nil)
  (constraints nil)
  (encoded-constraints nil)
  (initial-constraint-state nil)
  (attribute-assertions nil)     ; before fork elimination
  (attribute-constraints nil)    ; after fork elimination
  (signature nil)
  (query-subsumption nil)
  (subscriptions)
  (published-individuals nil)
  (ontologies nil)
  (complete-index-entries (racer-make-hash-table))
  (individual-identity-disjointness-assertions nil)
  (model-individual-synonyms (racer-make-hash-table))
  (reverse-model-individual-synonyms (racer-make-hash-table))
  (current-una-assumption nil)
  (version (new-abox-version))
  (annotation-individual-axioms nil)
  (annotation-role-axioms nil)
  (last-tbox-changed-mark nil)
  (rules nil)
  (distribute-individual-constraints-p t)
  (concrete-domain-subgraph nil)
  (timenet-rules 'empty-stream)
  (timenet-assertions 'empty-stream)
  (el+-transformed-table nil))

(defun abox-coherence-checked-p (abox)
  (or (eq (abox-una-coherent-p abox) t)
      (not (eq (abox-coherent-p abox) ':dont-know))))

(defmethod print-object ((object abox) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (abox-name object))))

(defun ensure-abox-name (abox)
  (if (symbolp abox)
    abox
    (abox-name abox)))

(defun make-abox-individual-axioms-index ()
  (racer-make-hash-table))

(defun add-role-axiom-to-abox (abox axiom &optional (role-axioms-index nil))
  (let ((index (or role-axioms-index (abox-role-axioms-index abox)))
        (role (second axiom)))
    (unless (and index
		 (or (member role (gethash (first axiom) index))
		     (let ((role-inverse (role-info-inverse (gethash role (tbox-role-axioms-index (abox-tbox abox))))))
		       (and role-inverse
			    (member role-inverse (gethash (reverse (first axiom)) index))))))
      (let ((ind-index (abox-individual-axioms-index abox)))
        (when ind-index
          (loop for ind in (first axiom) do
                (multiple-value-bind (value found)
                                     (gethash ind ind-index)
                  (declare (ignore value))
                  (unless found
                    (setf (gethash ind ind-index) nil)))))
	(when index
	  (push role (gethash (first axiom) index)))
        (push axiom (abox-role-axioms abox))))))

(defun add-negated-role-axiom-to-abox (abox axiom &optional (role-axioms-index nil))
  (let ((index (or role-axioms-index (abox-negated-role-axioms-index abox)))
        (role (second axiom)))
    (unless (and index
		 (or (member role (gethash (first axiom) index))
		     (let ((role-inverse (role-info-inverse (gethash role (tbox-role-axioms-index (abox-tbox abox))))))
		       (and role-inverse
			    (member role-inverse (gethash (reverse (first axiom)) index))))))
      (let ((ind-index (abox-individual-axioms-index abox)))
        (when ind-index
          (loop for ind in (first axiom) do
                (multiple-value-bind (value found)
                                     (gethash ind ind-index)
                  (declare (ignore value))
                  (unless found
                    (setf (gethash ind ind-index) nil)))))
	(when index
	  (push role (gethash (first axiom) index)))
        (push axiom (abox-negated-role-axioms abox))))))

(defun make-abox-role-axioms-index ()
  (racer-make-hash-table :test 'equal))

(defun add-individual-axiom-to-abox (abox axiom &optional (ind-axioms-index nil))
  (let ((index (or ind-axioms-index (abox-individual-axioms-index abox))))
    (unless (and index (member (second axiom) (gethash (first axiom) index)))
      (if (null index)
          (push axiom (abox-individual-axioms abox))
        (let ((found (gethash (first axiom) index)))
          (cond (found
                 (unless (eq (second axiom) 'top)
                   (push (second axiom) (gethash (first axiom) index))
                   (push axiom (abox-individual-axioms abox))))
                (t (push (second axiom) (gethash (first axiom) index))
                   (push axiom (abox-individual-axioms abox)))))))))

(defun copy-abox (abox)    
  ;(break "hier fehlt was")
  (let ((clone (copy-abox-internal abox)))
    ;;(copy-abox-individual-structures clone abox)
    (when (abox-index-structures-complete-p abox)
      (initialize-abox clone))
    #|
    (setf (abox-objects-table clone) (copy-hash-table (abox-objects-table abox)))
    (setf (abox-object-synonym-table clone) (copy-hash-table (abox-object-synonym-table abox)))
    (setf (abox-concept-individuals clone) (copy-hash-table (abox-concept-individuals abox)))
    (setf (abox-concept-non-individuals clone) (copy-hash-table (abox-concept-non-individuals abox)))
    (setf (abox-complete-index-entries clone) (copy-hash-table (abox-complete-index-entries abox)))
    |#
    (setf (abox-individual-axioms clone) (copy-list (abox-individual-axioms abox)))
    (setf (abox-individual-axioms-index clone) (copy-hash-table (abox-individual-axioms-index abox)))
    (setf (abox-ontologies clone) (copy-list (abox-ontologies abox)))
    (setf (abox-role-axioms clone) (copy-list (abox-role-axioms abox)))
    (setf (abox-role-axioms-index clone) (copy-hash-table (abox-role-axioms-index abox)))
    (setf (abox-negated-role-axioms clone) (copy-list (abox-negated-role-axioms abox)))
    (setf (abox-negated-role-axioms-index clone) (copy-hash-table (abox-negated-role-axioms-index abox)))
    (setf (abox-individual-identity-disjointness-assertions clone)
          (copy-list (abox-individual-identity-disjointness-assertions abox)))
    (setf (abox-annotation-individual-axioms clone)
          (copy-list (abox-annotation-individual-axioms abox)))
    (setf (abox-annotation-role-axioms clone)
          (copy-list (abox-annotation-role-axioms abox)))
    #|(when (abox-initial-constraint-state abox)
      (setf (abox-initial-constraint-state clone)
            (copy-solver-state (abox-initial-constraint-state abox))))
    |#
    clone))

(defun copy-abox-individual-structures (clone original)
  (let ((ind-table (racer-make-hash-table :size (length (abox-individuals-list original)))))
    (setf (abox-individuals-list clone)
          (loop for individual in (abox-individuals-list original)
                for new-individual = (copy-individual individual)
                collect new-individual
                do (setf (gethash individual ind-table) new-individual)))
    (setf (abox-individuals-table clone)
          (racer-make-hash-table :size (hash-table-count (abox-individuals-table original))))
    (loop with table = (abox-individuals-table clone)
          for ind-name being the hash-key of (abox-individuals-table original)
          using (hash-value individual)
          do (setf (gethash ind-name table) (gethash individual ind-table)))
    (setf (abox-subgraphs clone)
          (loop for subgraph in (abox-subgraphs original)
                for new-subgraph = (copy-subgraph subgraph)
                collect new-subgraph
                do
                (setf (subgraph-individuals new-subgraph)
                      (loop for individual in (subgraph-individuals new-subgraph)
                            for new-individual = (gethash individual ind-table)
                            collect new-individual
                            do
                            (setf (individual-subgraph new-individual) new-subgraph)))))
    (setf (abox-model-individual-synonyms clone)
          (copy-hash-table (abox-model-individual-synonyms original)))
    (setf (abox-reverse-model-individual-synonyms clone)
          (copy-hash-table (abox-reverse-model-individual-synonyms original)))))
          

(defun find-individual (abox individual-name &optional (errorp t) (tbox nil))
  (declare (ignorable tbox))
  
  ;; MW 6.1.20010: this already has to happen at parse time (TOLD)
  ;; (setf individual-name (prepend-prefix individual-name (or tbox (tbox abox))))

  (or (gethash individual-name (abox-individuals-table abox))
      (when errorp
        (error "Undefined individual name ~S in ABox ~S"
               individual-name (abox-name abox)))))

(defun set-individual-synonym (abox individual synonym-name)

  ;; MW 6.1.20010: this already has to happen at parse time (TOLD)
  ;; (setf synonym-name (prepend-prefix synonym-name (tbox abox))) 

  #+:debug (assert (symbolp synonym-name))
  (setf (gethash synonym-name (abox-individuals-table abox)) individual))

(defun add-individual-internal (new-individual abox ind-name)
  #+:debug (assert (symbolp ind-name))
  (setf (gethash ind-name (abox-individuals-table abox)) new-individual)
  (push new-individual (abox-individuals-list abox))
  new-individual)

(defun ensure-individual (abox ind-name)
  (or (find-individual abox ind-name nil)
      (add-individual-internal (make-individual (list ind-name) abox) abox ind-name)))

(defmacro loop-over-individuals ((abox ind) &body body)
  `(loop for ,ind in (abox-individuals-list ,abox)
         ,@body))

(defun sort-individuals (list)
  (sort list #'string<
        :key (lambda (ind)
               (first (individual-name-set ind)))))

(defun ensure-subsumption-based-query-answering (&optional (abox *current-abox*))
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (setf (abox-query-subsumption abox) t))


(defun get-abox-language (&optional (abox *current-abox*))
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (ensure-knowledge-base-state ':abox-prepared abox)
  (with-output-to-string (stream)
    (print-dl-description (abox-language abox) stream)))


;;; ======================================================================

(defstruct (cd-object (:constructor make-cd-object (name abox type))
                      (:predicate cd-object-p-internal))
  (name nil)
  (abox nil)
  (type nil)
  )

(defmethod print-object :around ((object cd-object) stream)
  (if *print-internals*
    (print-unreadable-object (object stream :type nil :identity t)
      (call-next-method))
    (call-next-method)))

(defmethod print-object ((object cd-object) stream)
  (format stream "~A:~S" (cd-object-name object) (cd-object-type object)))

(defun find-cd-object (abox object-name &optional (errorp t))
  (or (gethash object-name (abox-objects-table abox))
      (when errorp
        (error "Undefined concrete domain object name ~A in ABox ~A"
               object-name (abox-name abox)))))

(defun add-cd-object-internal (new-object abox object-name)
  (setf (gethash object-name (abox-objects-table abox)) new-object)
  (push new-object (abox-objects-list abox))
  new-object)

(defun ensure-no-cd-object (abox object-name &optional (attribute-name nil))
  (or (find-cd-object abox object-name nil)
      (progn
        (if attribute-name
          (progn
            (ensure-role-in-tbox (tbox abox) attribute-name)
            (add-cd-object-internal
             (make-cd-object object-name
                             abox
                             (role-cd-attribute (get-tbox-role (tbox abox) attribute-name)))
             abox
             object-name))
          (add-cd-object-internal (make-cd-object object-name abox nil)
                                  abox
                                  object-name)))))

(defun encode-cd-object (abox concrete-domain list-term-predicate)
  (let ((list-term (transform-predicate list-term-predicate)))
    (multiple-value-bind (predicate object-names)
                         (ensure-predicate list-term concrete-domain)
      (when abox
        (ensure-no-cd-objects-are-declared abox object-names list-term))
      (set-language
       (register-new-concept 'cd-predicate predicate :cd-term list-term)))))

(defun ensure-no-cd-objects-are-declared (abox object-names list-term)
  (let ((signature (abox-signature abox)))
    (when signature 
      (destructuring-bind (individuals cd-objects) signature
        (declare (ignore individuals))
        (loop for object-name in object-names do
              (let ((result (find object-name cd-objects)))
                (cond ((and (not *no-signature-checks*)
                            (null result))
                       (error "Concrete domain object ~A is not mentioned in the signature of ABox ~A"
                              object-name 
                              (abox-name abox)))
                      ((or (eq (first list-term) 'min)
                           (eq (first list-term) 'max))
                       (unless (integerp (third list-term))
                         (error "The number ~S in ~S must be an integer."
                                (third list-term) list-term))))))))))

;;; ======================================================================

(defun push-individual-on-subgraph (ind subgraph)
  (if (subgraph-last-cdr-individuals subgraph)
      (push ind (subgraph-individuals subgraph))
    (progn
      (setf (subgraph-last-cdr-individuals subgraph) (list ind))
      (setf (subgraph-individuals subgraph) (subgraph-last-cdr-individuals subgraph)))))

(defun nconc-subgraph-individuals (subgraph-1 subgraph-2)
  (if (null (subgraph-last-cdr-individuals subgraph-1))
      (setf (subgraph-individuals subgraph-1)
            (subgraph-individuals subgraph-2))
    (setf (cdr (subgraph-last-cdr-individuals subgraph-1))
          (subgraph-individuals subgraph-2)))
  (setf (subgraph-last-cdr-individuals subgraph-1)
        (subgraph-last-cdr-individuals subgraph-2)))

(defun nconc-subgraph-individual-names (subgraph-1 subgraph-2)
  (if (null (subgraph-last-cdr-individual-names subgraph-1))
      (setf (subgraph-individual-names subgraph-1)
            (subgraph-individual-names subgraph-2))
    (setf (cdr (subgraph-last-cdr-individual-names subgraph-1))
          (subgraph-individual-names subgraph-2)))
  (setf (subgraph-last-cdr-individual-names subgraph-1)
        (subgraph-last-cdr-individual-names subgraph-2)))

(defun push-encoded-role-axiom-on-subgraph (encoded-role-axiom subgraph)
  (if (subgraph-last-cdr-encoded-role-axioms subgraph)
      (push encoded-role-axiom (subgraph-encoded-role-axioms subgraph))
    (progn
      (setf (subgraph-last-cdr-encoded-role-axioms subgraph) (list encoded-role-axiom))
      (setf (subgraph-encoded-role-axioms subgraph) (subgraph-last-cdr-encoded-role-axioms subgraph)))))


(defun nconc-subgraph-encoded-role-axioms (subgraph-1 subgraph-2)
  (if (null (subgraph-last-cdr-encoded-role-axioms subgraph-1))
      (setf (subgraph-encoded-role-axioms subgraph-1)
            (subgraph-encoded-role-axioms subgraph-2))
    (setf (cdr (subgraph-last-cdr-encoded-role-axioms subgraph-1))
          (subgraph-encoded-role-axioms subgraph-2)))
  (setf (subgraph-last-cdr-encoded-role-axioms subgraph-1)
        (subgraph-last-cdr-encoded-role-axioms subgraph-2)))

;;; ======================================================================

(defun traverse-role-assertions (abox subgraph)
  (loop for role-assertion in (subgraph-encoded-role-axioms subgraph)
        for ind-1 = (find-individual abox (constraint-ind-1 role-assertion))
        for ind-2 = (find-individual abox (constraint-ind-2 role-assertion)) do
        (push role-assertion (individual-incoming-role-assertions ind-2))
        (push role-assertion (individual-outgoing-role-assertions ind-1))))

;;; ======================================================================

(defun prepare-individual-model (individual abox)
  (when (abox-distribute-individual-constraints-p abox)
    (loop for subgraph in (abox-subgraphs abox)
          for completion-state = (precompletion-state (subgraph-completion subgraph)) do
          (let ((*use-relation-store*
                 (use-relation-store-p (state-relation-store completion-state))))
            (store-individual-constraints abox
                                          (state-expanded-constraints completion-state)
                                          (state-expanded-store completion-state)
                                          (reduced-stable-rel-constraint-set-difference
                                           abox
                                           (if *use-relation-store*
                                             (get-all-rel-constraints
                                              (state-relation-store completion-state))
                                             (state-relation-constraints completion-state))
                                           (subgraph-encoded-role-axioms subgraph)))
            (set-language-of-subgraph-individuals subgraph)))
    (setf (abox-distribute-individual-constraints-p abox) nil))
  (without-taxonomic-encoding
    ;; otherwise the at-least or some terms might get lost
    (let* ((individual-name-set (individual-name-set individual))
           (individual-name (first individual-name-set))
           (tbox (abox-tbox (individual-abox individual)))
           (*meta-constraint-concepts*
            (tbox-meta-constraint-concepts tbox))
           (*blocking-possibly-required*
            (or *encode-roles-as-transitive*
                *meta-constraint-concepts*
                (tbox-blocking-possibly-required tbox))))
      (store-constraints-in-individual-model-1 individual
                                               individual-name
                                               nil
                                               (individual-outgoing-role-assertions individual))
      (store-constraints-in-individual-model-1 individual
                                               individual-name
                                               t
                                               (individual-incoming-role-assertions individual))
      (store-constraints-in-individual-model-2 individual
                                               individual-name-set
                                               (individual-added-relation-constraints individual))
      (loop for attribute-constraint in (abox-attribute-constraints (individual-abox individual))
            for attribute = (second attribute-constraint)
            for new-constraint = (encode-constraint `(,(first individual-name-set) (a ,attribute)))
            do
            (with-alc-language-settings (:constraints new-constraint)
              (pushnew new-constraint (individual-concept-constraints individual)
                       :test *matching-constraint-test*)
              (pushnew (constraint-term new-constraint) (individual-ind-concept-list individual))))))
  (setf (individual-model-built-p individual) t))

(defun set-language-of-subgraph-individuals (subgraph)
  (loop for individual in (subgraph-individuals subgraph)
        for individual-language = (individual-language individual)
        do
        (with-dl-language-set (individual-language-set individual-language)
          (loop for constraint in (individual-concept-constraints individual)
                for last-language = nil then other-language
                for other-language = (concept-language
                                      (if (constraint-negated-p constraint)
                                        (concept-negated-concept (constraint-term constraint))
                                        (constraint-term constraint)))
                unless (eq last-language other-language)
                do
                (with-dl-language-set (other-language-set other-language)
                  (unless (subsumes-dl-language-set-p individual-language-set
                                                      other-language-set)
                    (if (subsumes-dl-language-set-p other-language-set
                                                    individual-language-set)
                      (progn
                        (setf individual-language other-language)
                        (setf individual-language-set other-language-set))
                      (progn
                        (setf individual-language
			      (union-dl-descriptors individual-language other-language))
                        (set-language-set individual-language-set individual-language))))))
          (unless (dl-features individual-language)
            (loop for constraint in (individual-outgoing-role-assertions individual)
                  for role = (constraint-term constraint)
                  when (or (role-feature-p role) (role-feature-p (role-inverse-internal role)))
                  do
                  (setf individual-language (add-dl-features individual-language))
                  (return)))
          (unless (dl-features individual-language)
            (loop for constraint in (individual-incoming-role-assertions individual)
                  for role = (constraint-term constraint)
                  when (or (role-feature-p role) (role-feature-p (role-inverse-internal role)))
                  do
                  (setf individual-language (add-dl-features individual-language))
                  (return))))
        (setf (individual-language individual) individual-language)))

(defun store-constraints-in-individual-model-1 (individual individual-name inverse relation-constraints)
  (loop for constraint in relation-constraints
        for role = (if inverse
                     (role-inverse-internal (constraint-term constraint))
                     (constraint-term constraint))
        for new-constraint = (if (or (role-datatype role) (role-cd-attribute role))
                                 (encode-constraint `(,individual-name (d-at-least 1 ,role)))
                               (encode-constraint `(,individual-name (at-least 1 ,role))))
        do (add-constraint-to-individual-model individual new-constraint t)))

(defun store-constraints-in-individual-model-2 (individual individual-name-set relation-constraints)
  (loop for constraint in relation-constraints
        for role = (if (member (constraint-ind-1 constraint) individual-name-set)
                     (constraint-term constraint)
                     (progn
                       #+:debug (assert (member (constraint-ind-2 constraint) individual-name-set))
                       (role-inverse-internal (constraint-term constraint))))
        for new-constraint = (if (or (role-datatype role) (role-cd-attribute role))
                                 (encode-constraint `(,(first individual-name-set) (d-at-least 1 ,role)))
                               (encode-constraint `(,(first individual-name-set) (at-least 1 ,role))))
        do (add-constraint-to-individual-model individual new-constraint nil)))

(defun add-constraint-to-individual-model (individual constraint deterministic-p)
  (with-alc-language-settings (:constraints constraint)
    (pushnew constraint (individual-concept-constraints individual)
             :test *matching-constraint-test*)
    (when deterministic-p
      (pushnew (constraint-term constraint) (individual-ind-concept-list individual)))))

(defun internalize-individual-concept (tbox individual)
  (with-concept-definition-mapping tbox
    (let* ((name (individual-concept-name individual))
           (abox (individual-abox individual))
           (visited-mark (abox-last-tbox-changed-mark abox)))
      (unless visited-mark
        (setf visited-mark (incf *tbox-classification-counter*))
        (setf (abox-last-tbox-changed-mark abox) visited-mark)
        (mark-all-tbox-concepts tbox visited-mark))
      (create-tbox-internal-marker-concept tbox name) 
      (let* ((definition
               `(and ,@(get-concepts-from-ind-constraints
                        (individual-ind-concept-constraints individual))
                     .,(individual-ind-concept-list individual)))
             (concept (initialize-atomic-concept tbox
                                                 name
                                                 definition 
                                                 nil))
             (*concept-changed-p* nil))
        (with-flatten-encodings
          (encode-anonymous-concept tbox concept definition))
        (update-encoded-definition tbox
                                   concept
                                   (tbox-top-node tbox)
                                   (tbox-bottom-node tbox)
                                   visited-mark
                                   nil
                                   t)
        (setf (concept-visible-p concept) nil)
        ;; the following is not valid anymore in the case of merged relation constraints
        ;;(when (individual-model individual)
        ;;  (setf (concept-model concept) (individual-model individual)))
        ))))

(defun get-individual-role-successors (role relation-constraints)
  (loop for constraint in relation-constraints
        when (eq role (constraint-term constraint))
        collect constraint))

(defun get-concepts-from-ind-constraints (constraints)
  (loop for constraint in constraints
        for term = (constraint-term constraint)
        collect (if (constraint-negated-p constraint)
                  (concept-negated-concept term)
                  term)))

(defun get-individual-concept (ind)
  (if (individual-concept-name ind)
    (individual-completion-concept ind)
    (let ((name (first (individual-name-set ind))))
      (setf (individual-concept-name ind) (gensym (concatenate 'string
                                                               "%"
                                                               (if name
                                                                 (if (symbolp name)
                                                                   (symbol-name name)
                                                                   (format nil "~D" name))
                                                                 "CONCEPT")
                                                               "-")))
      (internalize-individual-concept (abox-tbox (individual-abox ind)) ind)
      (setf (individual-completion-concept ind)
            (get-tbox-concept (abox-tbox (individual-abox ind)) (individual-concept-name ind))))))


;;; ======================================================================

(defun use-abox-el+-saturation (abox tbox)
  (and *use-elh-transformation*
       *use-elh-model-embedding*
       (tbox-el+-transformed-table tbox)
       (subset-el+-p (abox-language abox))
       (null (abox-negated-role-axioms abox))
       (loop with use-unique-name-assumption = *use-unique-name-assumption*
             for individual in (abox-individuals-list abox)
             never (or (individual-told-disjoints individual)
                       (and use-unique-name-assumption
                            (rest (individual-name-set individual)))))
       (with-ignored-tbox-signature
         (loop for axiom in (abox-individual-axioms abox)
               always
               (subset-el+-p (concept-language (encode-concept-term (second axiom))))))
       (loop for role-axiom in (abox-role-axioms abox)
             never (eq (second role-axiom) +bottom-object-role-symbol+))))

(defun build-abox-index-structures (abox)
  (setf (abox-language abox) (tbox-language (abox-tbox abox)))
  (unless (dl-merging (abox-language abox))
    (loop for concept in (tbox-meta-constraint-concepts (abox-tbox abox)) do
          (setf (abox-language abox)
                (union-dl-descriptors (concept-language concept) (abox-language abox)))))
  (let ((ind-hashtable (and (abox-signature abox)
                            (racer-make-hash-table :size (length (first (abox-signature abox))))))
        (object-hashtable (and (abox-signature abox)
                               (racer-make-hash-table :size (length (second (abox-signature abox))))))
        (tbox (abox-tbox abox)))
    (when (abox-signature abox)
      (loop for individual in (first (abox-signature abox)) do
            (setf (gethash individual ind-hashtable) t)))
    (when (abox-signature abox)
      (loop for object in (second (abox-signature abox)) do
            (setf (gethash object object-hashtable) t)))

    (race-time
     (process-individual-identity-declarations abox ind-hashtable)
     "Process individual identity declarations")

    (race-time
     (process-individual-disjointness-declarations abox ind-hashtable)
     "Process individual disjointness declarations")

    (when (use-abox-el+-saturation abox tbox)
      (let ((*provisionally-inserted-atomic-concepts* nil))
        (race-time (abox-el+-normalization abox))
        (when *provisionally-inserted-atomic-concepts*
          (setf (abox-el+-transformed-table abox) *provisionally-inserted-atomic-concepts*)
          (setf (tbox-encoded-concept-list tbox)
                (stable-concept-set-union *provisionally-inserted-atomic-concepts*
                                          (tbox-encoded-concept-list tbox))))))

    (race-time
       (encode-role-assertions abox ind-hashtable)
     "Encode role assertions")
     
    (race-time
       (encode-individual-assertions abox ind-hashtable)
     "Encode individual assertions")
    
    ;; in case individuals are only mentioned in all-diffierent, same-individual-as, different-from
    (if *abox-partitioning-p*
      (loop for assertion in (abox-individual-identity-disjointness-assertions abox) do
            (loop for ind-name in (rest assertion)
                  for ind = (find-individual abox ind-name) do
                  (unless (individual-subgraph ind)
                    (let ((new-subgraph (make-subgraph :individuals (list ind))))
                      (setf (individual-subgraph ind) new-subgraph)
                      #+:debug (assert (null (individual-encoded-axioms ind)))
                      (push new-subgraph (abox-subgraphs abox))))))
      (let ((subgraph (first (abox-subgraphs abox))))
        (loop for assertion in (abox-individual-identity-disjointness-assertions abox) do
              (loop for ind-name in (rest assertion)
                    for ind = (find-individual abox ind-name) do
                    (unless (individual-subgraph ind)
                      (push-individual-on-subgraph ind subgraph)
                      (setf (individual-subgraph ind) subgraph))))))

    (race-time
     (loop for subgraph in (abox-subgraphs abox) do
           (loop for role-axiom in (subgraph-encoded-role-axioms subgraph) do
                 (incf (individual-has-fillers (find-individual abox (constraint-ind-1 role-axiom))))
                 (incf (individual-used-as-filler (find-individual abox (constraint-ind-2 role-axiom))))))
     "Compute number of predecessors and successors")

    (race-time
     (encode-cd-attribute-assertions abox ind-hashtable object-hashtable)
     "Encode CD attribute assertions")

    (race-time
     (process-cd-object-names abox)
     "Process CD object names")

    (race-time
     (merge-cd-attribute-assertions abox)
     "Merge CD attribute assertions")

    (let ((meta-constraint-concepts (tbox-meta-constraint-concepts (abox-tbox abox))))
      (when meta-constraint-concepts
        (race-time
         (loop for ind in (abox-individuals-list abox) do
               (loop for meta-constraint-concept in meta-constraint-concepts do
                     (setf (individual-told-subsumers ind)
                           (concept-set-union (concept-told-subsumers meta-constraint-concept)
					      (individual-told-subsumers ind)))))
         "Propagate told subsumers of meta concepts")))

    (race-time
     (loop for ind in (abox-individuals-list abox) do
           (setf (individual-told-non-subsumers ind)
                 (concept-set-union (individual-told-non-subsumers ind)
                                    (loop with subsumers = (individual-told-subsumers ind)
                                          with result = (when subsumers
                                                          (concept-told-disjoints (first subsumers)))
                                          for subsumer in (rest subsumers)
                                          for disjoints = (concept-told-disjoints subsumer)
                                          when disjoints
                                          do (setf result (concept-set-union result disjoints))
                                          finally (return result)))))
     "Propagate told non-subsumers from told disjoints")

    (race-time
     (create-concrete-domain-subgraph abox)
     "Generate Abox CD subgraph")

    (race-time
     (loop for subgraph in (abox-subgraphs abox)
           do
           (setf (subgraph-individual-names subgraph)
                 (mapcar (lambda (ind)
                           (first (individual-name-set ind)))
                         (subgraph-individuals subgraph)))
           (setf (subgraph-last-cdr-individual-names subgraph)
                 (last (subgraph-individual-names subgraph)))
           (traverse-role-assertions abox subgraph))
     "Collect subgraph individual names")

    (race-time
     (loop for subgraph in (abox-subgraphs abox) do
           (setf (subgraph-encoded-individual-axioms subgraph)
                 (racer-remove-duplicates (subgraph-encoded-individual-axioms subgraph) :test 'equal)))
     "Remove duplicate individual assertions")

    (race-time
     (loop for individual in (abox-individuals-list abox)
	 for individual-language = (individual-language individual)
	 do
	   (with-dl-language-set (individual-language-set individual-language)
	     (loop for assertion in (individual-encoded-axioms individual)
		 for last-language = nil then other-language
		 for other-language = (concept-language (second assertion))
		 unless (eq last-language other-language)
		 do
		   (with-dl-language-set (other-language-set other-language)
		     (unless (subsumes-dl-language-set-p individual-language-set
							 other-language-set)
		       (if (subsumes-dl-language-set-p other-language-set
						       individual-language-set)
			   (progn
			     (setf individual-language other-language)
			     (setf individual-language-set other-language-set))
			 (progn
			   (setf individual-language
			     (union-dl-descriptors individual-language other-language))
			   (set-language-set individual-language-set individual-language)))))))
	   (setf (individual-language individual) individual-language))
     "Compute individual DL language from individual assertions")

    (race-time
     (setf (abox-subgraphs abox)
           (sort (abox-subgraphs abox) #'<= :key (lambda (subgraph)
                                                   (length (subgraph-individuals subgraph)))))
     "Sort A-box subgraphs")
    
    (when *use-less-abox-memory*
      (clean-abox abox))
    
    (setf (abox-index-structures-complete-p abox) t)))

(defun create-concrete-domain-subgraph (abox &optional (additional-attribute-constraints nil))
  (when (or additional-attribute-constraints 
            (and *abox-partitioning-p*
                 (dl-any-concrete-domain (abox-language abox))))
    (let ((subgraphs-to-be-merged nil))
      (loop for subgraph in (abox-subgraphs abox)
            when (or (loop for (nil concept) in (subgraph-encoded-individual-axioms subgraph)
                           thereis (dl-full-concrete-domains (concept-language concept)))
                     (loop for ind in (subgraph-individuals subgraph)
                           thereis (or (loop for ((individual nil) nil)
                                             in (abox-attribute-assertions abox)
                                             thereis (member individual (individual-name-set ind)))
                                       (loop for assertion in additional-attribute-constraints
                                             for individual = (first assertion)
                                             thereis (member individual (individual-name-set ind))))))
            do (push subgraph subgraphs-to-be-merged))
      (when subgraphs-to-be-merged
	(setf subgraphs-to-be-merged (sort subgraphs-to-be-merged #'<
					   :key (lambda (subgraph)
						  (length (subgraph-individuals subgraph)))))
        (let ((subgraph (first subgraphs-to-be-merged)))
	  (loop for subgraph-1 in (rest subgraphs-to-be-merged)
                do (merge-subgraphs-1 subgraph subgraph-1))
          (setf (abox-concrete-domain-subgraph abox) subgraph)
          (setf (abox-subgraphs abox)
                (cons subgraph (stable-set-difference (abox-subgraphs abox) subgraphs-to-be-merged)))))
      #+:debug (abox-concrete-domain-subgraph abox))))

(defun merge-subgraphs-1 (subgraph-1 subgraph-2)
  ;;subgraph-1 is kept
  (nconc-subgraph-individuals subgraph-1 subgraph-2)
  (nconc-subgraph-individual-names subgraph-1 subgraph-2)
  (nconc-subgraph-encoded-role-axioms subgraph-1 subgraph-2)
  (setf (subgraph-encoded-individual-axioms subgraph-1)
        (nconc (subgraph-encoded-individual-axioms subgraph-2)
               (subgraph-encoded-individual-axioms subgraph-1)))
  (loop for ind in (subgraph-individuals subgraph-2) do
	(setf (individual-subgraph ind) subgraph-1))
  (setf (subgraph-precompletions-from-merger subgraph-1)
        (racer-remove-duplicates
         (nconc (when (subgraph-precompletion subgraph-1)
                  (list (subgraph-precompletion subgraph-1)))
                (when (subgraph-precompletion subgraph-2)
                  (list (subgraph-precompletion subgraph-2)))
                (subgraph-precompletions-from-merger subgraph-2)
                (subgraph-precompletions-from-merger subgraph-1)))))

(defun clean-abox (abox)
  (setf (abox-role-axioms-index abox) nil)
  (setf (abox-negated-role-axioms-index abox) nil)
  (setf (abox-role-axioms abox) nil)
  (setf (abox-negated-role-axioms abox) nil)
  (setf (abox-individual-axioms-index abox) nil)
  (setf (abox-individual-axioms abox) nil)
  )

(defun process-individual-identity-declarations (abox ind-hashtable)
  (when (abox-individual-identity-disjointness-assertions abox)
    (loop with obsolete-individuals = (racer-make-hash-table)
          with check = (and (abox-signature abox) (not *no-signature-checks*))
          for assertion in (abox-individual-identity-disjointness-assertions abox) do
          (when (eq (first assertion) 'same-individual-as)
            (let ((ind-name-1 (second assertion))
                  (ind-name-2 (third assertion)))
              (when check
                (unless (gethash ind-name-1 ind-hashtable)
                  (error "Individual ~A not mentioned in signature of ABox ~A"
                         ind-name-1
                         (abox-name abox)))
                (unless (gethash ind-name-2 ind-hashtable)
                  (error "Individual ~A not mentioned in signature of ABox ~A"
                         ind-name-2
                         (abox-name abox))))
              (let* ((ind-1 (ensure-individual abox ind-name-1))
                     (obsolete-ind-1 (find-individual abox ind-name-2 nil))
                     (obsolete-ind-2 (unless (eql ind-1 obsolete-ind-1)
                                       obsolete-ind-1))
                     (obsolete-ind-names (if obsolete-ind-2
                                           (individual-name-set obsolete-ind-2)
                                           (list ind-name-2))))
                (setf (individual-name-set ind-1) 
                      (stable-union (individual-name-set ind-1) obsolete-ind-names))
                (when obsolete-ind-2
                  (loop for ind-name in obsolete-ind-names
                        for ind = (find-individual abox ind-name nil)
                        when (and ind (not (gethash ind obsolete-individuals)))
                        do (setf (gethash ind obsolete-individuals) t)))
                (loop for ind-name in obsolete-ind-names do
                      (set-individual-synonym abox ind-1 ind-name)))))
          finally
	  (setf (abox-individuals-list abox)
	        (individual-set-difference (abox-individuals-list abox)
                                           (loop for ind being the hash-key of obsolete-individuals
                                             collect ind))))))

(defun process-individual-disjointness-declarations (abox ind-hashtable)
  (when (abox-individual-identity-disjointness-assertions abox)
    (let ((changed-individual-table (racer-make-hash-table)))
      (loop for assertion in (abox-individual-identity-disjointness-assertions abox) do
            (if (eq (first assertion) 'all-different)
              (let ((all-inds (mapcar (lambda (ind-name)
                                        (ensure-individual abox ind-name))
                                      (rest assertion))))
                (loop for inds on all-inds
                      for ind-1 = (first inds) do
                      (unless (or (null (rest inds)) (gethash ind-1 changed-individual-table))
                        (setf (gethash ind-1 changed-individual-table) t))
                      (loop for ind-2 in (rest inds) do
                            (unless (gethash ind-2 changed-individual-table)
                              (setf (gethash ind-2 changed-individual-table) t))
                            (push ind-2 (individual-told-disjoints ind-1))
                            (push ind-1 (individual-told-disjoints ind-2)))))
              (when (eq (first assertion) 'different-from)
                (let ((ind-name-1 (second assertion))
                      (ind-name-2 (third assertion)))
                  (when (and (abox-signature abox)
                             (not *no-signature-checks*)
                             (not (gethash ind-name-1 ind-hashtable)))
                    (error "Individual ~A not mentioned in signature of ABox ~A"
                           ind-name-1
                           (abox-name abox)))
                  (when (and (abox-signature abox)
                             (not *no-signature-checks*)
                             (not (gethash ind-name-2 ind-hashtable)))
                    (error "Individual ~A not mentioned in signature of ABox ~A"
                           ind-name-2
                           (abox-name abox)))
                  (let ((ind-1 (ensure-individual abox ind-name-1)) 
                        (ind-2 (ensure-individual abox ind-name-2)))
                    (unless (gethash ind-1 changed-individual-table)
                      (setf (gethash ind-1 changed-individual-table) t))
                    (unless (gethash ind-2 changed-individual-table)
                      (setf (gethash ind-2 changed-individual-table) t))
                    (push ind-2 (individual-told-disjoints ind-1))
                    (push ind-1 (individual-told-disjoints ind-2)))))))
      (loop for ind being the hash-key of changed-individual-table do
            (setf (individual-told-disjoints ind)
                  (individual-set-remove-duplicates (individual-told-disjoints ind)))))))

(defun encode-role-assertions (abox ind-hashtable)
  (let ((encoded-role-table
         (racer-make-hash-table :size (length (tbox-encoded-role-list (abox-tbox abox)))))
        (subgraph-id -1))
    (if *abox-partitioning-p*
      (let ((subgraph-table (racer-make-hash-table)))
        (loop with signature = (abox-signature abox)
	      with tbox = (abox-tbox abox)
	      with transitive-p = (not (tbox-all-transitive-roles tbox))
	      with hierarchy-p = (dl-simple-role-inclusions (tbox-language tbox))
              with complex-roles-p = (dl-complex-role-inclusions (tbox-language tbox))
              with signature-check = (and signature
                                          (not *no-signature-checks*))
              with propagate-told-subsumers = *propagate-told-subsumers*
	      for role-assertion in (abox-role-axioms abox)
	      for ((ind-name-1 ind-name-2) role-name) = role-assertion
	      do
              
              (unless (gethash role-name encoded-role-table)
                (encode-role-term role-name)
                (setf (gethash role-name encoded-role-table) t))
              
              (unless (and transitive-p hierarchy-p complex-roles-p)
		(let ((role nil))
		  (unless transitive-p
		    (setf role (get-tbox-role tbox role-name))
		    (when (role-transitive-p role)
		      (setf transitive-p t)
		      (setf (abox-language abox) (add-dl-transitive (abox-language abox)))))
		  (unless hierarchy-p
		    (unless role
		      (setf role (get-tbox-role tbox role-name)))
		    (when (role-has-ancestors-p role)
		      (setf hierarchy-p t)
		      (setf (abox-language abox) (add-dl-simple-role-inclusions (abox-language abox)))))
		  (unless complex-roles-p
		    (unless role
		      (setf role (get-tbox-role tbox role-name)))
		    (when (or (role-compositions role)
                              (and (role-has-ancestors-p role)
                                   (member-if #'role-compositions (role-ancestors-internal role))))
		      (setf complex-roles-p t)
		      (setf (abox-language abox) (add-dl-complex-role-inclusions (abox-language abox)))))))
	      
              (when signature-check
                (when (not (gethash ind-name-1 ind-hashtable))
                  (error "Individual ~S not mentioned in signature of ABox ~A"
                         ind-name-1
                         (abox-name abox)))
                (when (not (gethash ind-name-2 ind-hashtable))
                  (error "Individual ~S not mentioned in signature of ABox ~A"
                         ind-name-2
                         (abox-name abox))))
              (let* ((ind-1 (ensure-individual abox ind-name-1))
                     (ind-2 (ensure-individual abox ind-name-2))
                     (subgraph-1 (individual-subgraph ind-1))
                     (subgraph-2 (individual-subgraph ind-2)))
                (when propagate-told-subsumers
                  (propagate-told-subsumers-from-all-restrictions ind-1 ind-2 role-name))
                (cond ((and (null subgraph-1) (null subgraph-2))
                       (let ((new-subgraph (incf subgraph-id)))
                         (setf (individual-subgraph ind-1) new-subgraph)
                         (setf (individual-subgraph ind-2) new-subgraph)))
                      ((null subgraph-1)
		       (setf subgraph-2 (get-merged-subgraph-entry subgraph-2 subgraph-table))
                       (setf (individual-subgraph ind-1) subgraph-2))
                      ((null subgraph-2)
		       (setf subgraph-1 (get-merged-subgraph-entry subgraph-1 subgraph-table))
                       (setf (individual-subgraph ind-2) subgraph-1))
                      ((not (eql subgraph-1 subgraph-2))
		       (setf subgraph-1 (get-merged-subgraph-entry subgraph-1 subgraph-table))
		       (setf subgraph-2 (get-merged-subgraph-entry subgraph-2 subgraph-table))
		       (if (eql subgraph-1 subgraph-2)
                         (progn
                           (setf (individual-subgraph ind-1) subgraph-1)
                           (setf (individual-subgraph ind-2) subgraph-1))
		         (if (< subgraph-1 subgraph-2)
			   (progn
			     (setf (individual-subgraph ind-2) subgraph-1)
			     (add-merged-subgraph-entry subgraph-2 subgraph-1 subgraph-table))
			   (progn
			     (setf (individual-subgraph ind-1) subgraph-2)
			     (add-merged-subgraph-entry subgraph-1 subgraph-2 subgraph-table))))))))
	(let ((true-subgraph-table (racer-make-hash-table)))
          (unless (eql subgraph-id -1)
            (setf (gethash 0 true-subgraph-table) (make-subgraph)))
	  (loop for number from 1 to subgraph-id
	        for subgraph = (gethash (get-true-graph number subgraph-table) true-subgraph-table)
	        unless subgraph
                do (setf (gethash number true-subgraph-table) (make-subgraph)))
	  (loop for individual in (abox-individuals-list abox)
	        for subgraph = (gethash (get-true-graph (individual-subgraph individual)
                                                        subgraph-table)
                                        true-subgraph-table)
	        when subgraph
                do
		(push-individual-on-subgraph individual subgraph)
		(setf (individual-subgraph individual) subgraph))
	  (loop for role-assertion in (abox-role-axioms abox)
	        for ind-pair = (first role-assertion)
	        for ind-name-1 = (first ind-pair)
	        for ind-name-2 = (second ind-pair)
                for ind-1 = (find-individual abox ind-name-1)
	        for ind-name-set-1 = (individual-name-set ind-1)
                for ind-name-set-2 = (individual-name-set (find-individual abox ind-name-2))
                for have-synonyms-p = (or (rest ind-name-set-1) (rest ind-name-set-2))
	        for constraint-term = (if have-synonyms-p
					(list (list (first ind-name-set-1) (first ind-name-set-2))
                                              (second role-assertion))
				        role-assertion)
	        do 
                (push-encoded-role-axiom-on-subgraph (encode-constraint constraint-term)
                                                     (individual-subgraph ind-1)))
	  (setf (abox-subgraphs abox)
	        (loop for subgraph being the hash-values of true-subgraph-table
		      collect subgraph))))
      (let ((synonym-assertions-table (racer-make-hash-table :test 'equal))
            (subgraph (make-subgraph)))
        (loop with signature = (abox-signature abox)
              with signature-check = (and signature
                                          (not *no-signature-checks*))
              with propagate-told-subsumers = *propagate-told-subsumers*
              for ((ind-name-1 ind-name-2) role-name) in (abox-role-axioms abox) do
              
              (unless (gethash role-name encoded-role-table)
                (encode-role-term role-name)
                (setf (gethash role-name encoded-role-table) t))
              
              (when signature-check
                (when (not (gethash ind-name-1 ind-hashtable))
                  (error "Individual ~S not mentioned in signature of ABox ~A"
                         ind-name-1
                         (abox-name abox)))
                (when (not (gethash ind-name-2 ind-hashtable))
                  (error "Individual ~S not mentioned in signature of ABox ~A"
                         ind-name-2
                         (abox-name abox))))
              (let* ((ind-1 (ensure-individual abox ind-name-1))
                     (ind-2 (ensure-individual abox ind-name-2)))
                (when propagate-told-subsumers
                  (propagate-told-subsumers-from-all-restrictions ind-1 ind-2 role-name))
                (setf (individual-subgraph ind-1) subgraph)
                (setf (individual-subgraph ind-2) subgraph)
                (if (eql ind-1 ind-2)
                  (push-individual-on-subgraph ind-1 subgraph)
                  (progn 
                    (push-individual-on-subgraph ind-1 subgraph)
                    (push-individual-on-subgraph ind-2 subgraph)))
                (let* ((ind-name-set-1 (individual-name-set (find-individual abox ind-name-1)))
                       (ind-name-set-2 (individual-name-set (find-individual abox ind-name-2)))
                       (constraint-term
                        (list (list (first ind-name-set-1) (first ind-name-set-2)) role-name)))
                  (if (or (rest ind-name-set-1) (rest ind-name-set-2))
                    (unless (gethash constraint-term synonym-assertions-table)
                      (push-encoded-role-axiom-on-subgraph (encode-constraint constraint-term)
                                                           (individual-subgraph ind-1))
                      (setf (gethash constraint-term synonym-assertions-table) t))
                    (push-encoded-role-axiom-on-subgraph (encode-constraint constraint-term)
                                                         (individual-subgraph ind-1))))))
        (setf (abox-subgraphs abox) (list subgraph))))))

(defun get-true-graph (number subgraph-table)
  (let ((alias (get-true-graph-1 number subgraph-table number)))
    (unless (eql number alias)
      (setf (gethash number subgraph-table) alias))
    alias))

(defun get-true-graph-1 (number subgraph-table alias)
  (if alias
    (get-true-graph-1 alias subgraph-table (gethash alias subgraph-table))
    number))

(defun add-merged-subgraph-entry (from to subgraph-table)
  (let ((true-from (get-true-graph from subgraph-table))
	(true-to (get-true-graph to subgraph-table)))
    (setf (gethash true-from subgraph-table) true-to)))

(defun get-merged-subgraph-entry (number subgraph-table)
  (get-true-graph number subgraph-table))

(defun merge-subgraphs-local (subgraph-1 ind-2 subgraph-2 subgraph-table)
  ;;subgraph-1 is kept
  (setf (individual-subgraph ind-2) subgraph-1)
  (nconc-subgraph-individuals subgraph-1 subgraph-2)
  (nconc-subgraph-individual-names subgraph-1 subgraph-2)
  (nconc-subgraph-encoded-role-axioms subgraph-1 subgraph-2)
  (loop for ind-3 in (subgraph-individuals subgraph-2) do
	(setf (individual-subgraph ind-3) subgraph-1))
  (setf (gethash subgraph-2 subgraph-table) nil))

(defun abox-el+-normalization (abox)
  (with-race-trace-sublevel ("abox-el+-normalization"
                             :arguments (list abox)
                             :expanded nil
                             :trace-result nil)
    (when (and *use-elh-transformation* *use-elh-model-embedding*)
      (loop with el+-transformation-table = nil
            with tbox = (abox-tbox abox)
            for old-assertions = (abox-individual-axioms abox) then new-assertions
            for new-assertions = nil
            do
            (multiple-value-setq (new-assertions el+-transformation-table)
                (replace-rhs-complex-some-concepts tbox old-assertions el+-transformation-table))
            until (eq old-assertions new-assertions)
            finally
            (setf (abox-el+-transformed-table abox) t)
            (unless (eq new-assertions (abox-individual-axioms abox))
              (setf (abox-individual-axioms abox) new-assertions)
              (rebuild-individual-axioms-index abox))))))

(defun rebuild-individual-axioms-index (abox)
  (let ((individual-axioms-index (clrhash (abox-individual-axioms-index abox))))
    (loop for assertion in (abox-individual-axioms abox) do
          (push (second assertion) (gethash (first assertion) individual-axioms-index)))))
   
(defun replace-rhs-complex-some-concepts (tbox old-individual-axioms el+-transformation-table)
  ;;; (instance i (and d (some r c) e (some r e) ...) --> 
  ;;; (instance i (and d e ...), (instance i (some r a)) (instance i (some r b)), (implies a c), (implies b e)
  (let* ((changed-p nil)
         (new-individual-axioms
          (loop for assertion in old-individual-axioms
                for (individual orig-term) = assertion
                for term = (with-flatten-encodings
                             (encode-concept-term orig-term))
                for new-assertion-terms = nil
                if (dl-some (concept-language term))
                do
                (multiple-value-setq (new-assertion-terms el+-transformation-table)
                    (replace-rhs-complex-some-concepts-1 tbox term orig-term el+-transformation-table))
                (when (and (not changed-p) new-assertion-terms)
                  (setf changed-p t))
                if new-assertion-terms
                nconc (loop for new-assertion-term in new-assertion-terms
                            collect (list individual new-assertion-term))
                and do
                (unless changed-p
                  (setf changed-p t))
                else collect assertion)))
    (values (if changed-p
                new-individual-axioms
              old-individual-axioms)
            el+-transformation-table)))

(defun replace-rhs-complex-some-concepts-1 (tbox term orig-term el+-transformation-table)
  (if (some-concept-p term)
      (let ((qualification (concept-term term)))
        (if (atomic-concept-p qualification)
            (values nil el+-transformation-table)
          (multiple-value-bind (added-assertion-term new-el+-transformation-table)
              (replace-rhs-complex-some-concept tbox term orig-term el+-transformation-table)
            (values (list added-assertion-term) new-el+-transformation-table))))
    (if (and-concept-p term)
        (loop for conjunct in (concept-term term)
              for added-assertion-term = nil
              when (and (some-concept-p term) (not (atomic-concept-p (concept-term term))))
              do
              (multiple-value-setq (added-assertion-term el+-transformation-table)
                  (replace-rhs-complex-some-concept tbox conjunct orig-term el+-transformation-table))
              when added-assertion-term
              collect added-assertion-term into added-assertion-terms
              and
              collect conjunct into removed-concepts
              finally
              (if added-assertion-terms
                  (let ((remaining-conjuncts (concept-set-difference (concept-term term) removed-concepts)))
                    (if remaining-conjuncts
                        (return (values (cons `(and ., remaining-conjuncts) added-assertion-terms)
                                        el+-transformation-table))
                      (return (values added-assertion-terms el+-transformation-table))))
                (return (values nil el+-transformation-table))))
      (values nil el+-transformation-table))))

(defun replace-rhs-complex-some-concept (tbox concept orig-term el+-transformation-table)
  ;;; (instance i (some r c)) --> (instance i (some r a)), (implies a c)
  (unless el+-transformation-table
    (setf el+-transformation-table (racer-make-hash-table :test 'equal :structure-p t)))
  (let* ((qualification (concept-term concept))
         (old-concept (gethash qualification el+-transformation-table))
         (concept-name (if old-concept
                           (decode-concept old-concept)
                         (gentemp)))
         (new-term `(some ,(second orig-term) ,concept-name)))
    (unless old-concept
      (let ((concept (with-ignored-tbox-signature
                       (encode-update-concept-term tbox concept-name))))
        (setf (concept-told-subsumers concept) (list concept))
        (when (boundp '*provisionally-inserted-atomic-concepts*)
          (push concept *provisionally-inserted-atomic-concepts*))
        (setf (gethash qualification el+-transformation-table) concept)
        (setf (concept-visible-p concept) nil)
        (setf (concept-definition concept) (decode-concept qualification))
        (setf (concept-encoded-definition concept) qualification)
        (setf (concept-primitive-p concept) t)))
    (values new-term el+-transformation-table)))

(defun abox-el+-saturation (abox)
  (with-race-trace-sublevel ("abox-el+-saturation"
                             :arguments (list abox)
                             :expanded nil
                             :trace-result t)
    #+:debug (assert (abox-el+-transformed-table abox))
    (let* ((tbox (abox-tbox abox))
           (environment (tbox-el+-environment tbox)))
      (if environment
          (let ((added-concepts (abox-el+-transformed-table abox)))
            (race-trace ("~&Extending Tbox EL+ tables~%"))
            (tbox-el+-saturation tbox
                                 environment
                                 (when (listp added-concepts)
                                   added-concepts)))
        (setf environment (tbox-el+-saturation tbox)))
      (loop for individual in (abox-individuals-list abox) do
            (loop for told-subsumer in (individual-told-subsumers individual) do
                  (process-subsumption-axiom individual told-subsumer environment)))
      (loop for individual in (abox-individuals-list abox)
            for encoded-axioms = (individual-encoded-axioms individual)
            do
            (when encoded-axioms
              (loop for encoded-axiom in encoded-axioms
                    for concept = (second encoded-axiom)
                    do
                    #+:debug 
                    (assert (or (atomic-concept-p concept)
                                (some-concept-p concept)
                                (and-concept-p concept)))
                    (if (some-concept-p concept)
                        (process-edge individual (concept-role concept) (concept-term concept) environment)
                      (when (and-concept-p concept)
                        (loop for conjunct in (concept-term concept) do
                              (when (some-concept-p conjunct)
                                (process-edge individual
                                              (concept-role conjunct)
                                              (concept-term conjunct)
                                              environment))))))))
      (loop with top = (tbox-top-node (abox-tbox abox))
            for ((ind-1-name ind-2-name) role-name) in (abox-role-axioms abox)
            for ind-1 = (find-individual abox ind-1-name)
            for ind-2 = (find-individual abox ind-2-name)
            for role = (get-tbox-role tbox role-name)
            do
            (process-edge ind-1 role ind-2 environment)
            (process-edge ind-1 role top environment))
      (loop with dag = (env-dag environment)
            for individual in (abox-individuals-list abox)
            for told-subsumers = (el+-transitive-subsumers individual dag #'concept-p-internal)
            do
            (setf (individual-told-subsumers individual) told-subsumers)
            #|(setf (individual-ancestor-concepts individual) told-subsumers)
            (setf (individual-parent-concepts individual)
                  (loop for subsumer in told-subsumers
                        for children = (concept-children-internal subsumer)
                        when (and children (null (rest children)) (is-bottom-concept-p (first children)))
                        collect subsumer into parents
                        finally
                        (return (or parents (list top)))))
            (setf (individual-realized-p individual) t)|#)
      ;(setf (abox-realized-p-internal abox) t)
      (race-trace ("~&EL+ environment ~S~%" environment))
      environment)))

(defun el+-concept-instances (concept-term abox)
  #+:debug (assert (symbolp concept-term))
  (let* ((tbox (abox-tbox abox))
         (concept (get-tbox-concept tbox concept-term))
         (dag (env-dag (tbox-el+-environment tbox))))
    (flet ((individuals (concept dag)
             (remove-if #'concept-p-internal (el+-dag-incoming-vertices concept dag))))
      #+:debug (assert (or (atomic-concept-p concept) (and-concept-p concept)))
      (if (atomic-concept-p concept)
          (individuals concept dag)
        (when (and-concept-p concept)
          (loop with result = (individuals (first (concept-term concept)) dag)
                for conjunct in (rest (concept-term concept))
                do
                #+:debug (assert (atomic-concept-p conjunct))
                (setf result (concept-set-intersection (individuals conjunct abox) result))
                finally (return result)))))))

(defun encode-individual-assertions (abox ind-hashtable)
  (loop with signature = (abox-signature abox)
        with classified-p = (tbox-classified-p (abox-tbox abox))
        with language = (abox-language abox)
        with ind-language = nil
        with old-ind-language = nil
        with signature-check = (and signature
                                    (not *no-signature-checks*))
        with top = *top-concept*
        with abox-partitioning-p = *abox-partitioning-p*
        for assertion in (abox-individual-axioms abox) 
        for (name concept-term) = assertion do
        (when (and signature-check
                   (not (gethash name ind-hashtable)))
          (error "Individual ~S not mentioned in signature of ABox ~A"
                 name
                 (abox-name abox)))
        (let ((ind (ensure-individual abox name))
	      (encoded-concept-term nil)
	      (encoded-axiom nil))
	  (unless (and (symbolp concept-term)
		       (or (eq concept-term +krss-top-symbol+) (eq concept-term +top-symbol+)))
	    (setf encoded-concept-term (encode-concept-term concept-term))
	    (unless (eq encoded-concept-term top)
	      (setf encoded-axiom (list (first (individual-name-set ind)) encoded-concept-term))
	      (setf (individual-told-subsumers ind)
                (concept-set-union (concept-told-subsumers encoded-concept-term)
				   (individual-told-subsumers ind)))
	      (when (negated-concept-p encoded-concept-term)
		(push (concept-term encoded-concept-term)
		      (individual-told-non-subsumers ind)))
	      ;; Due to taxonomic encoding equalities might occur even here!
	      (push encoded-axiom (individual-encoded-axioms ind))
	      (push assertion (individual-assertions ind))
	      (setf old-ind-language ind-language)
	      (setf ind-language (concept-language encoded-concept-term))
	      (unless (eq old-ind-language ind-language)
		(setf language (union-dl-descriptors ind-language language)))))
          (unless (individual-subgraph ind)
            (if abox-partitioning-p
              (let ((new-subgraph (make-subgraph)))
                (push-individual-on-subgraph ind new-subgraph)
                (setf (individual-subgraph ind) new-subgraph)
                (setf (subgraph-encoded-individual-axioms new-subgraph)
                      ;; We need copy-list here because subgraph-encoded-individual-axioms is modified destructively.
                      ;; RM -- 29.7.2008
                      (copy-list (individual-encoded-axioms ind)))
                (push new-subgraph (abox-subgraphs abox)))
              (let ((subgraph (first (abox-subgraphs abox))))
                (setf (individual-subgraph ind) subgraph)
                (push-individual-on-subgraph ind subgraph))))
          (when encoded-axiom
	    (push encoded-axiom 
		  (subgraph-encoded-individual-axioms (individual-subgraph ind))))
          ;; Fills caches for subsumption-based query answering:
          (when (and classified-p encoded-axiom)
            (if (atomic-concept-p encoded-concept-term)
              (add-associated-individuals abox encoded-concept-term ind)
              (loop for concept in (concept-told-subsumers encoded-concept-term) do
                    (add-associated-individuals abox concept ind)))))
        finally (setf (abox-language abox) language)))

(defun encode-cd-attribute-assertions (abox ind-hashtable object-hashtable)
  (loop for ((ind-name obj-name) attribute) in (abox-attribute-assertions abox) do
        (when (and (abox-signature abox)
                   (not *no-signature-checks*)
                   (not (gethash ind-name ind-hashtable)))
          (error "Individual ~A not mentioned in signature of ABox ~A"
                 ind-name
                 (abox-name abox)))
        (when (and (abox-signature abox)
                   (not *no-signature-checks*)
                   (not (gethash obj-name object-hashtable)))
          (error "Object ~A not mentioned in signature of ABox ~A"
                 obj-name
                 (abox-name abox)))
        (when (and (tbox-signature (tbox abox))
                   (not *no-signature-checks*)
                   (not (member attribute (fifth (tbox-signature (tbox abox))) :key #'second)))
          (error "Attribute ~A not mentioned in signature of TBox ~A"
                 attribute
                 (tbox-name (tbox abox))))
        (let ((ind (ensure-individual abox ind-name)))
          (encode-role-term attribute :cd-attribute nil)
          (ensure-no-cd-object abox obj-name attribute)
          (push `(constrained ,ind-name ,obj-name ,attribute) 
                (individual-attribute-assertions ind))
          (setf (abox-language abox)
                (add-dl-simple-concrete-domains (abox-language abox)))
          (when (null (individual-subgraph ind))
            (if *abox-partitioning-p*
              (let ((new-subgraph (make-subgraph)))
                (push-individual-on-subgraph ind new-subgraph)
                (setf (individual-subgraph ind) new-subgraph)
                (push new-subgraph (abox-subgraphs abox)))
              (let ((subgraph (first (abox-subgraphs abox))))
                (push-individual-on-subgraph ind subgraph)
                (setf (individual-subgraph ind) subgraph)))))))

(defun process-cd-object-names (abox)
  (loop with signature = (abox-signature abox)
        with language = (abox-language abox)
        for pred in (abox-constraints abox)
        for concept = (encode-cd-object abox (tbox-concrete-domain (tbox abox)) pred)
        for predicate = (concept-predicate concept)
        for object-names = (predicate-parameters predicate)
        do
        (loop for object-name in object-names
              do
              (ensure-no-cd-object abox object-name)
              (let ((object (find-cd-object abox object-name)))
                (when (and signature (not *no-signature-checks*)
                           (not (member object-name (second (abox-signature abox)))))
                  (error "Object ~A not mentioned in signature of ABox ~A"
                         object-name (abox-name abox)))
                (unless (type-compatible-p (predicate-type predicate) (cd-object-type object))
                  (cond ((null (cd-object-type object))
                         (setf (cd-object-type object) (predicate-type predicate)))
                        ((and (eq (cd-object-type object) 'cardinal)
                              (eq (predicate-type predicate) 'real))
                         (propagate-predicate-type predicate 'cardinal))
                        (t (error "Type ~A of concrete domain predicate ~S and type ~A of concrete domain object ~A do not match"
                                  (predicate-type predicate) (predicate-definition predicate)
                                  (cd-object-type object) (cd-object-name object)))))))
        (setf language (union-dl-descriptors (concept-language concept) language))
        (push concept (abox-encoded-constraints abox))
        finally (setf (abox-language abox) language)))

;;;

(defun merge-subgraphs (abox ind-1 ind-2)
  (let ((subgraph-1 (individual-subgraph ind-1))
        (subgraph-2 (individual-subgraph ind-2)))
    (when (not (eq subgraph-1 subgraph-2))
      ;;subgraph-1 is kept
      (setf (individual-subgraph ind-2) subgraph-1)
      (nconc-subgraph-individuals subgraph-1 subgraph-2)
      (nconc-subgraph-individual-names subgraph-1 subgraph-2)
      (setf (subgraph-encoded-individual-axioms subgraph-1)
            (nconc (subgraph-encoded-individual-axioms subgraph-2)
                   (subgraph-encoded-individual-axioms subgraph-1)))
      (nconc-subgraph-encoded-role-axioms subgraph-1 subgraph-2)
      (loop for ind-3 in (subgraph-individuals subgraph-2) do
            (setf (individual-subgraph ind-3) subgraph-1))
      (setf (abox-subgraphs abox)
            (delete subgraph-2 (abox-subgraphs abox))))))

(defun merge-cd-attribute-assertions (abox)
  (let ((obj-ind-map (racer-make-hash-table)))
    (loop for ((ind-name obj-name) nil) in (abox-attribute-assertions abox) do
          (push (find-individual abox ind-name) (gethash obj-name obj-ind-map)))
    (loop for inds being the hash-values of obj-ind-map do
          (loop for (ind-1 ind-2) on inds 
                unless (null ind-2) do
                (merge-subgraphs abox ind-1 ind-2)))
    (loop for constraint in (abox-constraints abox) do
          (let ((objs (find-objects-in-constraint constraint (abox-objects-table abox))))
            (loop for (ind-1 ind-2) on (loop for obj in objs nconc (copy-list (gethash obj obj-ind-map)))
                  unless (null ind-2) do
                  (merge-subgraphs abox ind-1 ind-2))))))

(defun find-objects-in-constraint (constraint objects-table)
  (if (listp constraint)
    (find-objects-in-exprs (rest constraint) objects-table)
    nil))

(defun find-objects-in-exprs (exprs objects-table)
  (loop for expr in exprs 
        nconc (if (symbolp expr)
                (if (gethash expr objects-table)
                  (list expr)
                  nil)
                (find-objects-in-constraint expr objects-table))))
           
;;; 

#|
(defun merge-cd-attribute-assertions (abox)
  (loop for ((ind-1-name obj-1-name) nil) in (abox-attribute-assertions abox) 
        for ind-1 = (find-individual abox ind-1-name) do
        ;; If there exists another attribute assertion ((ind-name-2 obj-name) attribute-2) in the ABox
        ;; or there exists another attribute assertion ((ind-name-2 obj-name-2) attribute-2)
        ;; such that obj-name and obj-name-2 are involved in some constraint of the ABox,
        ;; then merge the subgraphs of ind-name and ind-name-2 (if they are not already identical)!!!
        
        (loop for ((ind-2-name obj-2-name) nil) in (abox-attribute-assertions abox)
              for ind-2 = (find-individual abox ind-2-name) do
              (unless (eq ind-1 ind-2)
                (when (or (eq obj-1-name obj-2-name)
                          (object-names-mentioned-in-constraint-p obj-1-name obj-2-name abox))
                  (let ((subgraph-1 (individual-subgraph ind-1))
                        (subgraph-2 (individual-subgraph ind-2)))
                    (when (not (eq subgraph-1 subgraph-2))
                      ;;subgraph-1 is kept
                      (setf (individual-subgraph ind-2) subgraph-1)
                      (nconc-subgraph-individuals subgraph-1 subgraph-2)
                      (setf (subgraph-encoded-individual-axioms subgraph-1)
                            (nconc (subgraph-encoded-individual-axioms subgraph-2)
                                   (subgraph-encoded-individual-axioms subgraph-1)))
                      (nconc-subgraph-encoded-role-axioms subgraph-1 subgraph-2)
                      (loop for ind-3 in (subgraph-individuals subgraph-2) do
                            (setf (individual-subgraph ind-3) subgraph-1))
                      (setf (abox-subgraphs abox)
                            (delete subgraph-2 (abox-subgraphs abox))))))))))
|#


(defun propagate-predicate-type (predicate type)
  (typecase (predicate-type predicate)
    (equal-predicate
     (propagate-predicate-type (predicate-predicate-1 predicate) type)
     (propagate-predicate-type (predicate-predicate-2 predicate) type))
    (unequal-predicate
     (propagate-predicate-type (predicate-predicate-1 predicate) type)
     (propagate-predicate-type (predicate-predicate-2 predicate) type))
    (otherwise (setf (predicate-type predicate) type))))
     

(defun object-names-mentioned-in-constraint-p (obj-1-name obj-2-name abox)
  (loop for constraint in (abox-constraints abox)
        thereis (and (object-name-mentioned-in-constraint-p obj-1-name constraint) 
                     (object-name-mentioned-in-constraint-p obj-2-name constraint))))

(defun object-name-mentioned-in-constraint-p (object-name expression)
  (if (listp expression)
    (loop for expr in expression
          thereis (object-name-mentioned-in-constraint-p object-name expr))
    (eq expression object-name)))

(defun abox-prepared-p (&optional (abox *current-abox*))
  (check-type abox (or abox symbol))
  (setf abox (find-abox abox))
  (abox-index-structures-complete-p abox))

(defmacro abox-prepared? (&optional (abox-name nil abox-name-supplied-p))
  (if (and abox-name-supplied-p abox-name)
    `(abox-prepared-p ',abox-name)
    `(abox-prepared-p *current-abox*)))


(defun propagate-told-subsumers-from-all-restrictions (ind-1 ind-2 role-name)
  (loop for axiom in (individual-encoded-axioms ind-1) do
        (let ((concept (second axiom)))
          (loop for concept-1 in (find-matching-all-concepts concept role-name) do
                (setf (individual-told-subsumers ind-2)
                      (concept-set-union (concept-told-subsumers concept-1)
					 (individual-told-subsumers ind-2)))
                
                (when (negated-concept-p concept-1)
                  (pushnew (concept-term concept-1)
                           (individual-told-non-subsumers ind-2)))))))

(defun find-matching-all-concepts (concept role-name)
  (cond ((and-concept-p concept)
         (let ((result nil))
           (loop for conjunct in (concept-term concept) do
                 (cond ((atomic-concept-p conjunct)
                        ;(format t "~%Expanding ~S." conjunct)
                        (setf result
                              (nconc (find-matching-all-concepts
                                      (concept-encoded-definition conjunct) 
                                      role-name)
                                     result)))
                       ((and (all-concept-p conjunct)
                             (eq role-name (role-name (concept-role conjunct))))
                        (setf result (cons (concept-term conjunct) result)))))
           result))
        ((atomic-concept-p concept)
         ;(format t "~%Expanding ~S." concept)
         (find-matching-all-concepts (concept-encoded-definition concept) role-name))
        ((all-concept-p concept)
         (if (eq role-name (role-name (concept-role concept)))
           (list (concept-term concept))
           nil))))


(defun make-abox (name tbox)
  (make-abox-internal :name name :tbox tbox))

(defun role-axiom-p (axiom-spec)
  (and (consp axiom-spec) (consp (first axiom-spec))))

(defun parse-ind-axiom (axiom-spec)
  (let ((ind-axiom-name (first axiom-spec))
        (ind-axiom-term (second axiom-spec)))
    (declare (ignore ind-axiom-term))
    (check-type ind-axiom-name symbol)
    axiom-spec))

(defun parse-role-axiom (axiom-spec)
  (let ((role-axiom-left-name (first (first axiom-spec)))
        (role-axiom-right-name (second (first axiom-spec)))
        (role-term (second axiom-spec)))
    (check-type role-axiom-left-name symbol)
    (check-type role-axiom-right-name symbol)
    (if (and (consp role-term) (eq (first role-term) 'inv))
      (check-type (second role-term) symbol)
      (check-type role-term symbol))
    axiom-spec))

(defun parse-role-or-ind-axiom (axiom-spec)
  (if (role-axiom-p axiom-spec)
    (parse-role-axiom axiom-spec)
    (parse-ind-axiom axiom-spec)))

(defun parse-attribute-axiom (attribute-axiom)
  (destructuring-bind ((individual object) attribute)
                      attribute-axiom
    (check-type individual symbol)
    (check-type object symbol)
    (check-type attribute symbol)
    attribute-axiom))

(defun parse-constraint (constraint)     ; NO SYNTAX CHECK YET.
  constraint)

;;; ----------------------------------------------------------------------

(defparameter *auto-realize* :lazy-verbose)

(defvar *abox-table* (racer-make-hash-table))

(defun all-aboxes ()
  (loop for abox being the hash-values of *abox-table*  
        collect (abox-name abox)))

(defmacro loop-over-aboxes ((abox-sym) &rest forms)
  (check-type abox-sym symbol)
  `(loop for ,abox-sym being the hash-values of *abox-table* .,forms))

(defmacro with-abox-defined-check (abox &body body)
  `(if ,abox
     (progn ,@body)
     (error "No current ABox set")))

(defun tbox (abox)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (abox-tbox abox))

(defun tbox-associated-with-abox (abox) ; This function is exported for TS, not tbox.
  (tbox abox))

(defun abox-realized-p (&optional (abox *current-abox*))
  (setf abox (find-abox abox))
  (check-type abox abox)
  (abox-realized-p-internal abox))

(defmacro abox-realized? (&optional (abox-name nil abox-specfied-p))
  (if abox-specfied-p
    (progn
      (check-type abox-name symbol)
      `(abox-realized-p-internal (find-abox ',abox-name)))
    `(with-abox-defined-check *current-abox*
       (abox-realized-p-internal *current-abox*))))

(defun current-abox ()
  (if *current-abox*
    (abox-name *current-abox*)
    (error "No current ABox set")))

(defun set-current-abox (abox)
  (check-type abox (or abox symbol))
  (setf abox (find-abox abox))
  (setf *current-abox* abox)
  (current-abox))


(defmacro in-abox (abox-name 
		   &optional (tbox-name nil tbox-name-specified-p))
  (if tbox-name-specified-p
    `(in-abox-internal ',abox-name ',tbox-name t)
    `(in-abox-internal ',abox-name 
                       (with-tbox-defined-check *current-tbox*
                         (tbox-name *current-tbox*))
                       nil)))

(defun in-abox-internal (abox-name 
			    tbox-name
			    init)
  (check-type abox-name symbol)
  (check-type tbox-name symbol)
  (setf *current-abox* (or (find-abox abox-name nil)
			   (make-abox abox-name 
				      (find-tbox tbox-name))))
  (setf (abox-tbox *current-abox*) (find-tbox tbox-name))
  (setf (find-abox abox-name) *current-abox*)
  (setf *current-tbox* (find-tbox tbox-name))
  (when init
    (init-abox *current-abox*))
  (current-abox))

(defun init-abox (abox &optional (tbox *current-tbox* tbox-supplied-p))
  ;;; completely initialize / reset Abox
  (check-type abox (or symbol abox))
  (check-type tbox (or symbol tbox))
  (let* ((old-abox (find-abox abox nil))
         (tbox1 (if tbox-supplied-p 
                  (find-tbox tbox)
                  (if old-abox
                    (abox-tbox old-abox)
                    (find-tbox tbox)))))
    (cond ((null old-abox)
           (setf *current-abox* (make-abox abox tbox1))
	   (setf (find-abox abox) *current-abox*))
          (t (setf *current-abox* old-abox) 
             (initialize-abox old-abox t) 
             (setf (abox-tbox old-abox) tbox1)
             (setf (abox-individual-axioms old-abox) nil
                   (abox-individual-axioms-index old-abox) (make-abox-individual-axioms-index)
                   (abox-role-axioms old-abox) nil
                   (abox-role-axioms-index old-abox) (make-abox-role-axioms-index)
                   (abox-negated-role-axioms old-abox) nil
                   (abox-negated-role-axioms-index old-abox) (make-abox-role-axioms-index)
                   (abox-role-axioms-not-holding old-abox) nil
                   (abox-annotation-individual-axioms old-abox) nil
                   (abox-annotation-role-axioms old-abox) nil
                   (abox-constraints old-abox) nil
                   (abox-encoded-constraints old-abox) nil
                   (abox-attribute-assertions old-abox) nil
                   (abox-attribute-constraints old-abox) nil
                   (abox-initial-constraint-state old-abox) nil
                   (abox-signature old-abox) nil
                   (abox-subscriptions old-abox) nil
                   (abox-published-individuals old-abox) nil
                   (abox-query-subsumption old-abox) nil
                   (abox-ontologies old-abox) nil
                   (abox-individual-identity-disjointness-assertions old-abox) nil
                   (abox-current-una-assumption old-abox) nil
                   (abox-last-tbox-changed-mark old-abox) nil
                   (abox-distribute-individual-constraints-p old-abox) nil
                   (abox-concrete-domain-subgraph old-abox) nil
                   (abox-timenet-rules old-abox) 'empty-stream
                   (abox-timenet-assertions old-abox) 'empty-stream
                   (abox-el+-transformed-table old-abox) nil)
             (loop for rule in (abox-rules old-abox) do (delete-rule (swrl-rule-name rule)))
             (setf (abox-rules old-abox) nil)
	     (setf (find-abox (abox-name old-abox)) *current-abox*)))
    (abox-name *current-abox*)))

(defun initialize-abox (old-abox &optional (new-p nil))
  (declare (ignorable new-p))
  (let ((tbox (abox-tbox old-abox)))
    ;;; reset Abox caches but preserve the original information
    (when (and nil (not new-p) *use-less-abox-memory*)
      (setf (abox-role-axioms old-abox)
            (loop for subgraph in (abox-subgraphs old-abox)
                  nconc
                  (loop for assertion in (subgraph-encoded-role-axioms subgraph)
                        collect (list (list (constraint-ind-1 assertion)
                                            (constraint-ind-2 assertion))
                                      (role-name (constraint-term assertion))))))
      (setf (abox-individual-axioms old-abox)
            (loop for subraph in (abox-subgraphs old-abox)
                  nconc
                  (loop for assertion in (subgraph-encoded-individual-axioms subraph)
                        collect (list (first assertion)
                                      (decode-concept (second assertion) nil t))))))
    (setf (abox-individuals-table old-abox) (racer-make-hash-table))
    (setf (abox-individuals-list old-abox) nil)
    (setf (abox-objects-table old-abox) (racer-make-hash-table))
    (setf (abox-objects-list old-abox) nil)
    (setf (abox-object-synonym-table old-abox) (racer-make-hash-table))
    (setf (abox-subgraphs old-abox) nil
          (abox-index-structures-complete-p old-abox) nil)
    (setf (abox-model-individual-synonyms old-abox) (racer-make-hash-table))
    (setf (abox-reverse-model-individual-synonyms old-abox) (racer-make-hash-table))
    (setf (abox-concept-individuals old-abox) (racer-make-hash-table))
    (setf (abox-concept-non-individuals old-abox) (racer-make-hash-table))
    (setf (abox-complete-index-entries old-abox) (racer-make-hash-table))
    (setf (abox-realized-p-internal old-abox) nil
          (abox-coherent-p old-abox) ':dont-know
          (abox-una-coherent-p old-abox) ':dont-know
          (abox-role-axioms-not-holding old-abox) nil
          (abox-language old-abox) *dl-empty*
          (abox-last-tbox-changed-mark old-abox) nil
          (abox-el+-transformed-table old-abox) nil)
    (setf (abox-version old-abox) (new-abox-version))
    (setf (abox-tbox-identification old-abox) (tbox-identification tbox))
    old-abox))

(defun reset-abox (abox-name &optional (tbox-name nil))
  (racer-warn "The use of reset-abox is depricated. Use init-abox instead.")
  (init-abox abox-name tbox-name))

(defun find-abox (abox-name-or-abox &optional (errorp t))
  (let ((abox (etypecase abox-name-or-abox
                (symbol (gethash abox-name-or-abox *abox-table*))
                (abox abox-name-or-abox))))
    (when (and errorp (not abox))
      (error "Can't find ABox with name ~S" abox-name-or-abox))
    abox))

(defun (setf find-abox) (new-abox abox-name)
  (unless (null new-abox)
    (setf new-abox (find-abox new-abox)))
  (check-type abox-name symbol)
  (check-type new-abox (or null abox))
  (cond ((null new-abox)
         (remhash abox-name *abox-table*))
        (t (setf (gethash abox-name *abox-table*) new-abox)
           (setf (abox-name new-abox) abox-name)))
  new-abox)

(defun associated-aboxes (tbox)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (loop for abox being the hash-values of *abox-table*
        when (eq (tbox abox) tbox)
        collect (abox-name abox)))

(defun get-abox-version (abox)
  (check-type abox (or abox symbol))
  (setf abox (find-abox abox))
  (abox-version abox))


;;; ======================================================================

(defmacro define-abox ((abox-name tbox-name
                                  &key (verbose nil) (debug nil))
                       &body axioms)
  `(define-abox-1 ',abox-name ',tbox-name
     :verbose ',verbose
     :debug ',debug
     :axioms ',axioms))

(defun define-abox-1 (abox-name tbox-name &key (verbose nil) (debug nil) axioms)
  (declare (ignore verbose debug))
  (check-type abox-name symbol)
  (check-type tbox-name symbol)
  (setf *current-abox*
        (setf (find-abox abox-name)
              (make-abox abox-name (find-tbox tbox-name))))
  (init-abox *current-abox* (find-tbox tbox-name))
  (loop with abox = *current-abox*
        with role-axioms-index = (abox-role-axioms-index abox)
        with ind-axioms-index = (abox-individual-axioms-index abox)
        for axiom in (mapcar #'parse-role-or-ind-axiom axioms) do
        (if (role-axiom-p axiom)
          (add-role-axiom-to-abox abox axiom role-axioms-index)
          (add-individual-axiom-to-abox abox axiom ind-axioms-index)))
  abox-name)


(defmacro signature (&whole signature-form
                            &key atomic-concepts 
                            roles
                            transitive-roles
                            features
                            attributes
                            individuals
                            objects)
  (declare (ignore signature-form))
  `(progn
     ,@(when (or atomic-concepts roles transitive-roles features attributes)
         `((ensure-tbox-signature (with-tbox-defined-check *current-tbox*
                                    *current-tbox*)
                                  :atomic-concepts ',atomic-concepts
                                  :roles ',roles
                                  :transitive-roles ',transitive-roles
                                  :features ',features
                                  :attributes ',attributes)))
     ,@(when (or individuals objects)
         `((ensure-abox-signature (with-abox-defined-check *current-abox*
                                    *current-abox*)
                                  :individuals ',individuals
                                  :objects ',objects)))
     (values)))

(defun ensure-abox-signature (abox-name-or-abox
                                  &key individuals objects)
  (let ((abox (find-abox abox-name-or-abox)))
    (init-abox abox)
    (let* ((tbox (abox-tbox abox))
           (tbox-signature (tbox-signature tbox)))
      (ensure-disjointness "individuals" individuals "objects" objects)
      (when tbox-signature
        (destructuring-bind (atomic-concepts roles transitive-roles features attributes)
                            tbox-signature
          (let ((attribute-names (mapcar #'second attributes)))
            (ensure-disjointness "individuals" individuals "atomic concepts" atomic-concepts)
            (ensure-disjointness "individuals" individuals "roles" roles)
            (ensure-disjointness "individuals" individuals "features" features)
            (ensure-disjointness "individuals" individuals 
                                 "attributes" attribute-names)
            (ensure-disjointness "individuals" individuals "transitive-roles" transitive-roles)
            (ensure-disjointness "objects" objects "atomic concepts" atomic-concepts)
            (ensure-disjointness "objects" objects "roles" roles)
            (ensure-disjointness "objects" objects "features" features)
            (ensure-disjointness "objects" objects "attributes" 
                                 attribute-names)
            (ensure-disjointness "objects" objects "transitive-roles" transitive-roles))))
      (setf (abox-signature abox)
            (list individuals objects)))))

(defun get-abox-signature (&optional (abox *current-abox*))
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (if (abox-signature abox)
    (destructuring-bind (individuals objects)
                        (abox-signature abox)
      `(signature :individuals ,individuals :objects ,objects))
    nil))

(defmacro in-knowledge-base (tbox-name &rest args)
  (let (abox-name 
        init)
    (cond ((null args) 
           (setf abox-name tbox-name)
           (setf init t))
          ((null (rest args))
           (setf abox-name (first args))
           (setf init t))
          ((> (length args) 2)
           (error "Syntax error in (in-knowledge-base ~A~{ ~S~})" tbox-name args))
          ((eq (first args) :init)
           (setf init (second args))
           (setf abox-name tbox-name))
          (t 
           (error "Syntax error in (in-knowledge-base ~A~{ ~S~})" tbox-name args)))
    `(progn (in-tbox ,tbox-name :init ,init)
            (in-abox ,abox-name ,tbox-name)
            nil)))

;;; ======================================================================

(defun add-concept-assertion (abox individual-name concept)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type individual-name symbol)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (add-individual-axiom-to-abox abox (parse-ind-axiom (list individual-name concept)))

  (check-nrql-subscriptions (abox-name abox))

  (values))

(defun add-annotation-concept-assertion (abox individual-name concept)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type individual-name symbol)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (push (parse-ind-axiom (list individual-name concept))
        (abox-annotation-individual-axioms abox))

  (check-nrql-subscriptions (abox-name abox))
   
  (values))

(defun add-role-assertion (abox predecessor-name filler-name role-term)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type predecessor-name symbol)
  (check-type filler-name symbol)
  (check-role-term role-term)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (let* ((tbox (tbox abox))
         (index (tbox-role-axioms-index tbox))
         (role (if (consp role-term) 
                 (second role-term)
                 role-term))
         (entry (gethash role index)))
    (if (and entry (role-info-annotation-p entry))
      (add-annotation-role-assertion abox predecessor-name filler-name role-term)
      (if (and (consp role-term) (eq (first role-term) 'inv) (symbolp (second role-term)))
        (add-role-axiom-to-abox abox 
                                (parse-role-axiom (list (list filler-name predecessor-name)
                                                        (second role-term))))
        (add-role-axiom-to-abox abox
                                (parse-role-axiom (list (list predecessor-name filler-name)
                                                        role-term))))))

  (check-nrql-subscriptions (abox-name abox))

  (values))

(defun add-negated-role-assertion (abox predecessor-name filler-name role-term)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type predecessor-name symbol)
  (check-type filler-name symbol)
  (check-role-term role-term)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (let* ((tbox (tbox abox))
         (index (tbox-role-axioms-index tbox))
         (role (if (consp role-term) 
                 (second role-term)
                 role-term))
         (entry (gethash role index)))
    (if (and entry (role-info-annotation-p entry))
      (error "Cannot negate annotation properties.")
      (if (and (consp role-term) (eq (first role-term) 'inv) (symbolp (second role-term)))
        (add-negated-role-axiom-to-abox abox 
					(parse-role-axiom (list (list filler-name predecessor-name)
								(second role-term))))
        (add-negated-role-axiom-to-abox abox
					(parse-role-axiom (list (list predecessor-name filler-name)
								role-term))))))

  (check-nrql-subscriptions (abox-name abox))

  (values))

(defun add-annotation-role-assertion (abox predecessor-name filler-name role-term)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type predecessor-name symbol)
  (check-type filler-name symbol)
  (check-role-term role-term)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (if (and (consp role-term) (eq (first role-term) 'inv) (symbolp (second role-term)))
    (push (parse-role-axiom (list (list filler-name predecessor-name) (second role-term)))
          (abox-annotation-role-axioms abox))
    (push (parse-role-axiom (list (list predecessor-name filler-name) role-term))
          (abox-annotation-role-axioms abox)))

  (check-nrql-subscriptions (abox-name abox))

  (values))

(defun add-attribute-assertion (abox
                                    individual
                                    object
                                    attribute)
  (check-type abox (or abox symbol))
  (check-type attribute symbol)
  (setf abox (find-abox abox))
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (push (parse-attribute-axiom (list (list individual object) attribute))
        (abox-attribute-assertions abox))

  (check-nrql-subscriptions (abox-name abox))

  (values))


(defun set-attribute-filler (abox individual value attribute &optional type)
  (check-type abox (or abox symbol))
  (check-type attribute symbol)
  (setf abox (find-abox abox))
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (let ((object-name (gensym)))
    (add-attribute-assertion abox individual object-name attribute)
    (if (null type)
      (etypecase value
        (integer (add-constraint-assertion abox `(equal ,object-name ,value)))
        (boolean (add-constraint-assertion abox `(boolean= ,object-name ,value)))
        (string (add-constraint-assertion abox `(string= ,object-name ,value)))
        (real (add-constraint-assertion abox `(= ,object-name ,value))))
      (ecase type
        (integer (add-constraint-assertion abox `(equal ,object-name ,value)))
        (racer-boolean (add-constraint-assertion abox `(boolean= ,object-name ,value)))
        (string (add-constraint-assertion abox `(string= ,object-name ,value)))
        (real (add-constraint-assertion abox `(= ,object-name ,value))))))

  (check-nrql-subscriptions (abox-name abox))

  (values))

(defun add-datatype-role-filler (abox individual value role &optional type)
  (check-type abox (or abox symbol))
  (check-type role symbol)
  (setf abox (find-abox abox))
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (let* ((tbox (tbox abox))
         (index (tbox-role-axioms-index tbox))
         (entry (gethash role index)))
    (unless (consp value)
      (setf value `(d-literal ,value ,type)))
    (if (and entry (role-info-annotation-p entry))
        (add-annotation-concept-assertion abox individual
                                          `(d-filler ,role ,value))
      (add-concept-assertion abox individual
                             `(d-filler ,role ,value)))))
                                                              
(defun add-negative-datatype-role-filler (abox individual value role &optional type)
  (check-type abox (or abox symbol))
  (check-type role symbol)
  (setf abox (find-abox abox))
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (let* ((tbox (tbox abox))
         (index (tbox-role-axioms-index tbox))
         (entry (gethash role index)))
    (unless (consp value)
      (setf value `(d-literal ,value ,type))) 
    (if (and entry (role-info-annotation-p entry))
        (error "Negative annotation property assertions are not supported.")
      (add-concept-assertion abox individual
                             `(not (d-filler ,role ,value))))))

(defun add-constraint-assertion (abox constraint)
  (check-type abox (or abox symbol))
  (setf abox (find-abox abox))
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (cond ((eq (first constraint) '=)
         (push (parse-constraint constraint)
               (abox-constraints abox)))
        ((eq (first constraint) 'equal)
         (push (parse-constraint constraint)
                  (abox-constraints abox)))
        ((eq (first constraint) 'range)
         (push (parse-constraint (list 'min (second constraint) (third constraint)))
               (abox-constraints abox))
         (push (parse-constraint (list 'max (second constraint) (fourth constraint)))
                  (abox-constraints abox)))
        (t (push (parse-constraint constraint)
                    (abox-constraints abox))))

  (check-nrql-subscriptions (abox-name abox))

  (values))

(defun add-same-individual-as-assertion (abox individual-name-1 individual-name-2)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type individual-name-1 symbol)
  (check-type individual-name-2 symbol)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (push `(same-individual-as ,individual-name-1 ,individual-name-2)
        (abox-individual-identity-disjointness-assertions abox))

  (check-nrql-subscriptions (abox-name abox))
  
  (values))

(defun add-different-from-assertion (abox individual-name-1 individual-name-2)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type individual-name-1 symbol)
  (check-type individual-name-2 symbol)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (push `(different-from ,individual-name-1 ,individual-name-2)
        (abox-individual-identity-disjointness-assertions abox))

  (check-nrql-subscriptions (abox-name abox))
  
  (values))

(defun add-all-different-assertion (abox individual-name-set)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (loop for name in individual-name-set do
        (check-type name symbol))
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (push `(all-different ,@individual-name-set)
        (abox-individual-identity-disjointness-assertions abox))

  (check-nrql-subscriptions (abox-name abox))
  
  (values))


;;; KRSS style macro for ABox instance axioms

;;; Macro for concept membership axiom
(defmacro instance (name concept)
  `(with-abox-defined-check *current-abox*
     (add-concept-assertion *current-abox* ',name ',concept)))

;;; Macro for role axiom
(defmacro related (left-name right-name role-name)
  `(with-abox-defined-check *current-abox*
     (add-role-assertion *current-abox*
                         ',left-name
                         ',right-name
                         ',role-name)))

;;; Macro for negated role axiom
(defmacro unrelated (left-name right-name role-name)
  `(with-abox-defined-check *current-abox*
     (add-negated-role-assertion *current-abox*
				 ',left-name
				 ',right-name
				 ',role-name)))

(defmacro datatype-role-filler (individual value role &optional type)
  `(add-datatype-role-filler *current-abox* ',individual ',value ',role ',type))

(defmacro attribute-filler (individual value attribute &optional type)
  `(set-attribute-filler *current-abox* ',individual ',value ',attribute ',type))



(defmacro constrained (individual object attribute)
  `(with-abox-defined-check *current-abox*
     (add-attribute-assertion *current-abox*
                              ',individual
                              ',object
                              ',attribute)))

(defmacro constraints (&body forms)
  `(with-abox-defined-check *current-abox*
     (loop for constraint in ',forms do
           (add-constraint-assertion *current-abox*
                                     constraint))))


(defmacro state (&body forms)
  `(progn 
     (let ((*check-subscriptions-inhibited* t)) 
       .,forms)
     (check-subscriptions *current-abox*)))
  
(defmacro define-distinct-individual (individual-name &optional (concept +top-symbol+))
  `(add-individual *current-abox* ',individual-name t ',concept))

(defmacro define-individual (individual-name &optional (concept +top-symbol+))
  `(add-individual *current-abox* ',individual-name nil ',concept))

(defmacro same-individual-as (individual-name-1 individual-name-2)
  `(add-same-individual-as-assertion *current-abox* ',individual-name-1 ',individual-name-2))

(defmacro same-as (individual-name-1 individual-name-2)
  `(same-individual-as ,individual-name-1 ,individual-name-2))

(defmacro different-from (individual-name-1 individual-name-2)
  `(add-different-from-assertion *current-abox* ',individual-name-1 ',individual-name-2))

(defmacro all-different (&rest individual-name-set)
  `(add-all-different-assertion *current-abox* ',individual-name-set))

(defun add-individual (abox individual-name distinct-p &optional (concept +top-symbol+))
  (check-type abox abox)
  (check-type individual-name symbol)
  (when (and distinct-p (not *use-unique-name-assumption*))
    (racer-warn "Attempt to declare a distinct individual ~S in ABox ~S. ~
                 RACER does not assume the unique name assumption for ABox individuals (by default). ~
                 The individual ~S might be identified with other named individuals."
                individual-name abox individual-name))
  (add-concept-assertion abox individual-name concept)
  individual-name)

(defun all-individuals (&optional (abox *current-abox*)
                                     &key (count nil))
  (setf abox (find-abox abox))
  (check-type abox abox)
  (ensure-knowledge-base-state ':abox-prepared abox)
  (possibly-count-only
   (let ((result nil))
     (loop for ind in (abox-individuals-list abox) do
           (loop for ind-name in (individual-name-set ind) do
                 (push ind-name result)))
     result)
   count))

(defun individual-p (individual-name &optional (abox *current-abox*))
  (setf abox (find-abox abox))
  (check-type individual-name symbol)
  (check-type abox abox)
  (when (find-individual abox individual-name nil)
    t))

(defmacro individual? (individual-name &optional (abox-name nil abox-name-specifiied-p))
  (if abox-name-specifiied-p
    `(individual-p ',individual-name ',abox-name)
    `(with-abox-defined-check *current-abox*
       (individual-p ',individual-name *current-abox*))))

(defun cd-object-p (object-name &optional (abox *current-abox*))
  (setf abox (find-abox abox))
  (check-type object-name symbol)
  (check-type abox abox)
  (error "Not yet implemented.")
  #|
  (or (loop for object-assertion in (abox-attribute-assertions abox) 
            thereis (eql (second (first object-assertion)) object-name))
      (multiple-value-bind (satisfiable state)
                           (compute-initial-constraint-state abox)
        (declare (ignore satisfiable))
        (when (member object-name (solver-state-free-vars state))
          t)))
   |#
  )

(defmacro cd-object? (object-name &optional (abox-name nil abox-name-specifiied-p))
  (if abox-name-specifiied-p
    `(cd-object-p ',object-name ',abox-name)
    `(with-abox-defined-check *current-abox*
       (cd-object-p ',object-name *current-abox*))))

(defun describe-abox (&optional (abox *current-abox*)
                                   (stream *standard-output*))
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (ensure-knowledge-base-state ':abox-prepared abox)
  
  (let ((result  `(signature
                   :individuals ,(if (abox-signature abox)
                                   (first (abox-signature abox))
                                   (loop for ind in (abox-individuals-list abox)
                                         append (individual-name-set ind)))
                   :objects ,(if (abox-signature abox)
                               (second (abox-signature abox))
                               (abox-objects-list abox)))))
    (if (null stream)
      (format nil "~S" result)
      (pprint result stream))))


(defun describe-individual (individual-name 
                               &optional (abox *current-abox*)
                               (stream *standard-output*))
 (check-type abox (or symbol abox))
 (setf abox (find-abox abox))
 (ensure-knowledge-base-state ':abox-prepared abox)
 (let* ((tbox (tbox abox))
        (roles (all-roles tbox))
        (attributes (all-attributes tbox))
        fillers
        value
        (result (list
                 individual-name
		  ':synonyms (retrieve-individual-synonyms individual-name nil abox)
		  ':antonyms (retrieve-individual-antonyms individual-name nil abox)		  
                 ':assertions (all-concept-assertions-for-individual individual-name abox)
                 ':role-fillers
                 (loop for role in roles 
                       when (and (symbolp role)
                                 (not (role-used-as-datatype-property-p role tbox))
                                 (not (role-used-as-annotation-property-p role tbox))
                                 (setf fillers
                                       (retrieve-individual-fillers individual-name role abox :told t)))
                       collect `(,role ,fillers))
                 ':told-attribute-fillers
                 (loop for attribute in attributes
                       when (setf fillers 
                                  (loop for object in 
                                        (retrieve-individual-attribute-fillers individual-name
                                                                               attribute
                                                                               abox)
                                        when (setf value (told-value object abox))
                                        do (return value)))
                       collect `(,attribute ,fillers))
                 ':told-datatype-fillers
                 (loop for role in roles 
                       when (setf fillers
                                  (and (symbolp role)
                                       (role-used-as-datatype-property-p role tbox)
                                       (setf fillers
                                             (retrieve-individual-told-datatype-fillers 
                                              individual-name role abox))))
                       collect `(,role ,fillers))
                 ':annotation-datatype-property-fillers
                 (loop for role in roles 
                       when (and (symbolp role)
                                 (role-used-as-datatype-property-p role tbox)
                                 (role-used-as-annotation-property-p role tbox)
                                 (setf fillers
                                       (retrieve-individual-annotation-property-fillers
                                        individual-name role abox)))
                       collect `(,role ,fillers))
                 ':annotation-property-fillers
                 (loop for role in roles 
                       when (and (symbolp role)
                                 (not (role-used-as-datatype-property-p role tbox))
                                 (role-used-as-annotation-property-p role tbox)
                                 (setf fillers
                                       (retrieve-individual-fillers individual-name role abox :told t)))
                       collect `(,role ,fillers))
                 ':direct-types
                 (if (individual-realized-p (find-individual abox individual-name))
                   (most-specific-instantiators individual-name abox)
                   ':to-be-computed))))
   (if (null stream)
     (format nil "~S" result)
     (pprint result stream))))


(defun describe-individual1 (individual-name 
                               &optional (abox *current-abox*)
                               (stream *standard-output*))
 (check-type abox (or symbol abox))
 (setf abox (find-abox abox))
 (ensure-knowledge-base-state ':abox-prepared abox)
 (let* ((tbox (tbox abox))
        (roles (all-roles tbox))
        (attributes (all-attributes tbox))
        fillers
        value
        (result (list
                 individual-name
                 ':assertions (all-concept-assertions-for-individual individual-name abox)
                 ':role-fillers
                 (loop for role in roles 
                       when (and (symbolp role)
                                 (not (role-used-as-datatype-property-p role tbox))
                                 (not (role-used-as-annotation-property-p role tbox))
                                 (setf fillers
                                       (retrieve-individual-fillers individual-name role abox :told t)))
                       collect `(,role ,fillers))
                 ':told-attribute-fillers
                 (loop for attribute in attributes
                       when (setf fillers 
                                  (loop for object in 
                                        (retrieve-individual-attribute-fillers individual-name
                                                                               attribute
                                                                               abox)
                                        when (setf value (told-value object abox))
                                        do (return value)))
                       collect `(,attribute ,fillers))
                 ':told-datatype-fillers
                 (loop for role in roles 
                       when (setf fillers
                                  (and (symbolp role)
                                       (role-used-as-datatype-property-p role tbox)
                                       (setf fillers
                                             (retrieve-individual-told-datatype-fillers 
                                              individual-name role abox))))
                       collect `(,role ,fillers))
                 ':annotation-datatype-property-fillers
                 (loop for role in roles 
                       when (and (symbolp role)
                                 (role-used-as-datatype-property-p role tbox)
                                 (role-used-as-annotation-property-p role tbox)
                                 (setf fillers
                                       (retrieve-individual-annotation-property-fillers
                                        individual-name role abox)))
                       collect `(,role ,fillers))
                 ':annotation-property-fillers
                 (loop for role in roles 
                       when (and (symbolp role)
                                 (not (role-used-as-datatype-property-p role tbox))
                                 (role-used-as-annotation-property-p role tbox)
                                 (setf fillers
                                       (retrieve-individual-fillers individual-name role abox :told t)))
                       collect `(,role ,fillers))
                 ':direct-types
                 (if (individual-realized-p (find-individual abox individual-name))
                   (most-specific-instantiators individual-name abox)
                   ':to-be-computed))))
   (if (null stream)
     (format nil "~S" result)
     (pprint result stream))))



(defun compute-implicit-role-fillers (individual-name &optional (abox *current-abox*))
  (let* ((tbox (tbox abox))
         (roles (all-roles tbox)))
    (loop for role in roles do
          (retrieve-individual-fillers individual-name role abox))))

(defun compute-all-implicit-role-fillers (&optional (abox *current-abox*))
  (loop for ind in (all-individuals abox) do
        (compute-implicit-role-fillers ind abox)))



;;; ======================================================================

(defun realize-abox (&optional (abox *current-abox*) (individual-name nil))
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (with-race-trace-sublevel ("realize-abox"
                             :arguments (list abox individual-name))
    (realize-internal abox individual-name)))

;;; ======================================================================


(defun abox-realization-report (abox
                                    stream 
                                    format-string
                                    &rest format-args)
  ;;(declare (ignore clash-type))
  (when *abox-clash-verbose* 
    (case format-string
      (:abox-incoherent
       (loop for clash-report in (first format-args) do
             (if (<= (length clash-report) 3)
               (destructuring-bind (&optional clash-1 clash-2 cs)
                                   clash-report
                 (if clash-1
                   (cond
                    ((concept-constraint-p clash-1)
                     (report-primitive-clash abox stream clash-1 clash-2 cs))
                    ((and (relation-constraint-p clash-1) (relation-constraint-p clash-2))
                     (report-role-clash abox stream clash-1 clash-2 cs))
                    ((consp clash-1)
                     (format stream "~&ABox ~S is incoherent due to concrete domain clash ~S.~%"
                             (abox-name abox)
                             clash-1))
                    (t (format stream "~&ABox ~S is incoherent: ~S~%" 
                               (abox-name abox) format-args)))
                   (format stream "~&ABox ~A is incoherent~%" (abox-name abox))))
               (let ((concept-constraints (butlast clash-report))
                     (relation-constraints (first (last clash-report))))
                 ;(break)
                 (format stream "~&ABox ~S is incoherent.~%" 
                         (abox-name abox))
                 (format stream "~&The clash is caused by the following constraints:~&")
                 (loop for constraint in (append concept-constraints relation-constraints) do
                       (format stream "~&Clashing constraint ~S:~&" constraint)
                       (when (if (qualified-role-signature-p (constraint-signature constraint))
                               (signature-dependencies (constraint-signature constraint))
                               (constraint-dependencies constraint))
                         (format stream "~&Dependencies:~&")
                         (print-dependency-trace-back stream constraint 0)))))
             (format stream "~&=====================~&")))
      (t (apply #'format
	        stream
	        format-string
	        format-args)))))

(defun report-primitive-clash (abox stream clash-1 clash-2 cs) 
  (if cs
      (let* ((clash-id (constraint-ind clash-1))
             (clash-concept (constraint-term clash-1))
             (clash-id-chain
              (loop for id = clash-id then (constraint-ind-1 id-used)
                    for id-used = (find id cs :key #'constraint-ind-2)
                    while (and id-used (not (eql id (constraint-ind-2 id-used))))
                    collect id-used)))
        (format stream
                "~&ABox ~A is incoherent~%~
                   Individual ~S contains a primitive clash for concept ~%~S"
                (abox-name abox) clash-id clash-concept)
        (when clash-id-chain (format stream "~%In chain "))
        (loop for chain-element in (reverse clash-id-chain) do
              (format stream "~S " chain-element))
        (format stream "~%In CS ")
        (loop for cs-element in cs do
              (format stream "~%~S " cs-element)))
    (format stream
            "~&ABox ~S is incoherent~%~
             Individual ~A contains a primitive clash for concept ~%~S~%"
            (abox-name abox) (constraint-ind clash-1) (constraint-term clash-1)))
  (when *abox-clash-verbose*
    (print-dependencies-trace-back stream clash-1 clash-2)))

(defun report-role-clash (abox stream clash-1 clash-2 cs)
  (if cs
      (let* ((clash-id (constraint-ind-1 clash-1))
             (clash-role (constraint-term clash-1))
             (clash-id-list
              (loop for constraint in cs
                    when (and (eq clash-role (constraint-term constraint))
                              (eql clash-id (constraint-ind-1 constraint)))
                    collect constraint)))
        (format stream "~&ABox ~S is incoherent~%~
                      Individual ~S contains a clash for role ~S"
                (abox-name abox) clash-id (role-name clash-role))
        (when clash-id-list (format stream "~%In chain "))
        (loop for chain-element in clash-id-list do
              (format stream "~S " chain-element))
        (format stream "~%In CS ")
        (loop for cs-element in cs do
              (format stream "~%~S " cs-element)))
    (format stream "~&ABox ~S is incoherent~%~
                    Individual ~S contains a clash for role ~S~%"
            (abox-name abox) (constraint-ind-1 clash-1)
            (role-name (constraint-term clash-1))))
  (when *debug*
    (print-dependencies-trace-back stream clash-1 clash-2)))

(defun print-dependencies-trace-back (stream clash-1 clash-2)
  (let ((*print-pretty* t))
    (format stream "~&Dependencies for clashing constraint ~%~S:~%" clash-1)
    (print-dependency-trace-back stream clash-1 0)
    (unless (eq clash-1 clash-2)
      (when clash-2
        (format stream "~2&Dependencies for clashing constraint ~%~S:~%" clash-2)
        (print-dependency-trace-back stream clash-2 0)))))

(defun print-dependency-trace-back (stream clash level)
  (let ((dependency-list (if (qualified-role-signature-p (constraint-signature clash))
                           (signature-dependencies (constraint-signature clash))
                           (constraint-dependencies clash))))
    (if dependency-list
      (progn
        (format stream "~&")
        (loop for indent from 1 to level do (princ #\space stream))
        (format stream "--> ")
        (loop for constraint in (constraints-to-list dependency-list) do
              (format stream "~S" constraint)
              (format stream " ~%"))
        (loop for dependency in dependency-list do
              (print-dependency-trace-back stream dependency (1+ level))))
      (when (zerop level)
        (format stream " NIL~%")))))

(defun constraints-to-list (constraints)
  (mapcar #'(lambda (constraint)
              (if (concept-constraint-p constraint)
                  (list (constraint-ind constraint)
                        (decode-concept
                         (if (constraint-negated-p constraint)
                             (concept-negated-concept (constraint-term constraint))
                           (constraint-term constraint))))
                constraint))
          constraints))

;;; ======================================================================

(defvar *clash-type*)

(defmacro with-abox-settings (abox &body body)
  `(let ((*current-abox* ,abox))
     ,@body))

(defun realize-internal (abox individual-name
                                 &optional (stream t))
  (ensure-tbox-and-abox-synchronization abox)
  (ensure-abox-caches-are-correct abox)
  (if (abox-realized-p-internal abox)
    nil
    (let ((*clash-reasons* nil)
          (tbox (abox-tbox abox))
          (start (when-print-statistics
                   (get-internal-run-time)))
          (data-stream *race-statistics-stream*))
      (ensure-knowledge-base-state ':tbox-classified tbox)
      (with-abox-settings abox
        (with-concept-definition-mapping (abox-tbox abox) 
          (when-statistics
            (print-abox-statistics-name abox data-stream))
          (unless (abox-index-structures-complete-p abox)
            (race-time (build-abox-index-structures abox)))
          (prog1
            (cond ((abox-consistent-p abox)
                   (if (null individual-name)
                     (progn
                       (realize-individuals abox)
                       (setf (abox-realized-p-internal abox) t))
                     (progn 
                       (realize-individual abox
                                           tbox
                                           (find-individual abox individual-name)
                                           (and (subset-el+-p (tbox-language tbox))
                                                (subset-el+-p (abox-language abox))))
                       (when-collect-statistics
                         (collect-statistics-data))
                       (when (every #'individual-realized-p (abox-individuals-list abox))
                         (setf (abox-realized-p-internal abox) t)))))
                  (t (abox-realization-report abox
                                              stream
                                              ':abox-incoherent
                                              :clash-1 (first *clash-reasons*)
                                              :clash-2 (second *clash-reasons*)
                                              :cs (third *clash-reasons*))
                     nil))
            (when-print-statistics
              (print-abox-statistics abox
                                     (and *statistics* stream)
                                     data-stream
                                     start
                                     (get-internal-run-time)))))))))

(defun check-abox-coherence (&optional (abox *current-abox*)
                                           (filename-or-stream nil))
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (cond ((null filename-or-stream)
         (check-abox-coherence-1 abox t))
        ((streamp filename-or-stream)
         (check-abox-coherence-1 abox filename-or-stream))
        ((stringp filename-or-stream)
         (with-open-file (stream filename-or-stream :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
           (check-abox-coherence-1 abox stream)))
        (t (error "Illegal filename or stream."))))

(defparameter *abox-clash-culprits* nil)

(defun check-abox-coherence-1 (&optional (abox *current-abox*)
                                             (stream t))
  (with-race-trace-sublevel ("check-abox-coherence"
                             :arguments (list abox)
                             :trace-result t)
    (let ((start (when-print-statistics
                   (get-internal-run-time)))
          (data-stream *race-statistics-stream*))
      (initialize-abox abox)
      (when-statistics
        (print-abox-statistics-name abox data-stream))
      (setf *abox-clash-culprits* nil)
      (let ((*abox-clash-verbose* t)
            (*clash-reasons* nil))
        (let ((result (abox-consistent-p abox)))
          (unless result
            ;;(inspect *clash-reasons*)
            (setf *abox-clash-culprits*
                  (compute-abox-clash-culprits abox *clash-reasons*))
            #|(abox-realization-report abox
                                      stream
                                      ':abox-incoherent
                                      (print *clash-reasons*))|#
            ;;(pprint *abox-clash-culprits*)
            )
          (when-print-statistics
            (print-abox-statistics abox
                                   (and *statistics* stream)
                                   data-stream
                                   start
                                   (get-internal-run-time)))
          (if result
              (list t nil)
            (list nil *abox-clash-culprits*)))))))

(defun flatten (list)
  (cond ((null list)
         nil)
        ((consp (first list))
         (nconc (flatten (first list)) (flatten (rest list))))
        ((null (first list))
         (flatten (rest list)))
        (t (cons (first list) (flatten (rest list))))))



(defun compute-abox-clash-culprits (abox clash-reasons) 
  (let ((result nil)
        (candidates (flatten clash-reasons))
        new-candidates)
    (loop until (null candidates) do
          (setf new-candidates nil)
          (loop for candidate in (reverse candidates) 
                when candidate do       
                
                ;;(terpri) (terpri) (print candidate)
                ;;(break)
                (typecase candidate
                  (constraint-common
                   (if (null (constraint-dependencies candidate))
                       (pushnew (transform-constraint-2 candidate) result :test #'equal)
                     (progn
                       (loop for dep in (constraint-dependencies candidate) do
                             ;;(print (list dep (find dep result)))
                             ;;(print result)
                             (unless (find (transform-constraint-2 dep) result :test #'equal)
                               (typecase dep
                                 (concept-constraint 
                                  ;;(break)
                                  (typecase (constraint-term dep)
                                    (atomic-concept 
                                     (when (if (constraint-negated-p dep)
                                               (if (concept-primitive-p (constraint-term dep))
                                                   (concept-encoded-negated-definition (constraint-term dep))
                                                 (concept-encoded-definition (constraint-term dep)))
                                             (concept-encoded-definition (constraint-term dep)))
                                       (let ((deduction-step-desc
                                              (list :inference-step
                                                    :assertion (transform-constraint-2 dep)
                                                    :axiom
                                                    (transform-axiom 
                                                     (list (if (concept-primitive-p (constraint-term dep))
                                                               'implies
                                                             'equivalent)
                                                           (if (constraint-negated-p dep)
                                                               (concept-negated-concept (constraint-term dep))
                                                             (constraint-term dep))
                                                           (if (constraint-negated-p dep)
                                                               (if (concept-primitive-p (constraint-term dep))
                                                                   (concept-encoded-negated-definition (constraint-term dep))
                                                                 `(not ,(concept-encoded-definition (constraint-term dep))))
                                                             (concept-encoded-definition (constraint-term dep))))))))
                                         (pushnew (if (concept-gci-dependencies (constraint-term dep))
                                                      (append deduction-step-desc
                                                              (list :provenance
                                                                    (transform-gci-dependencies 
                                                                     (concept-gci-dependencies (constraint-term dep)))))
                                                    deduction-step-desc)
                                                  result
                                                  :test #'equal))))
                                    (some-concept
                                     (let ((role-domain (role-domain-restriction (concept-role (constraint-term dep))))
                                           (role-range (role-range-restriction (concept-role (constraint-term dep)))))
                                       ;;(break)
                                       (when role-domain
                                         (pushnew (list :inference-step
                                                        :assertion (transform-constraint-2 dep) 
                                                        ':domain
                                                        role-domain)
                                                  result
                                                  :test #'equal))
                                       (when role-range
                                         (pushnew (list :inference-step 
                                                        :assertion (transform-constraint-2 dep) 
                                                        ':range 
                                                        role-range)
                                                  result
                                                  :test #'equal))
                                       ))))))
                             (push dep new-candidates)))))
                  (cd-constraint
                   (pushnew `(:inference-step 
                              :constraints
                              ,(loop for (ind attribute object) in (abox-attribute-constraints abox)
                                     when (member object (cd-constraint-varlist candidate))
                                     collect `((,ind ,object) ,attribute))
                              :cd-predicate ,(transform-cd-predicate (cd-constraint-predicate candidate)))
                            result
                            :test #'equal))
                  (qualified-role-signature
                   (if (null (signature-dependencies candidate))
                       (pushnew (transform-constraint-2 candidate) result) ;; should not happen
                       (let ((axioms (loop for disjointness-assertion 
                                               in (abox-individual-identity-disjointness-assertions abox)
                                               collect ;(break)
                                               (when (subsetp (signature-successor-ind-set candidate) 
                                                              (rest disjointness-assertion))
                                                 disjointness-assertion))))
                         (pushnew `(:inference-step
                                    :assertion ,(transform-constraint-2 candidate)
                                    :axioms ,(append axioms
                                                     (loop for dep in (signature-dependencies candidate)
                                                           collect
                                                           (progn (push dep new-candidates)
                                                             (transform-constraint-2 dep)))))
                                  result :test #'equal))))))
          (setf candidates new-candidates))
    (let ((mcs (tbox-meta-constraint-concepts (tbox abox))))
      (if mcs
	  (cons (list :meta-constaint `(and .,mcs)) result)
	result))))

(defun transform-constraint-2 (constraint)
  (etypecase constraint
    (concept-constraint
     (list (constraint-ind constraint) 
           (if (constraint-negated-p constraint)
               (decode-concept (concept-negated-concept (constraint-term constraint)))
             (decode-concept (constraint-term constraint)))))
    (qualified-role-signature 
     (list (list (signature-ind constraint) (signature-successor-ind-set constraint)) 
           (list (signature-cardinality constraint) (signature-role constraint))))
    (cd-constraint (transform-cd-predicate (cd-constraint-predicate constraint)))
    (relation-constraint
     (list (list (constraint-ind-1 constraint) (constraint-ind-2 constraint)) (constraint-term constraint)))))

(defun transform-cd-predicate (pred)
  (predicate-definition pred))

(defun transform-axiom (axiom)
  (ecase (first axiom)
    (implies (list (first axiom) (decode-concept (second axiom)) (decode-concept (third axiom))))
    (equivalent (list (first axiom) (decode-concept (second axiom)) (decode-concept (third axiom))))))


(defun transform-gci-dependencies (gci-dependencies)
  (loop for gci-dependency in gci-dependencies collect
        (let ((type (intern (symbol-name (gci-dependency-type gci-dependency)) :keyword))
              (removed (gci-dependency-removed gci-dependency))
              (added (gci-dependency-added gci-dependency)))
          (if removed
              (list :original-axiom removed :type type)
            (list :added-gci added :type type)))))


;;; ===============================================================================

;;; Use *taxonomic-encoding-dependencies* in post-mortem analysis.

;;; ===============================================================================

(defun abox-satisfiable (abox)
  (with-race-trace-sublevel ("abox-satisfiable"
                             :arguments (list abox)
                             :trace-result t)
    (when *debug*
      (format t "~&Testing (abox-satisfiable ~S)..." abox))
    (let* ((tbox (abox-tbox abox))
           (*meta-constraint-concepts* (tbox-meta-constraint-concepts tbox))
           (*blocking-possibly-required* (or *encode-roles-as-transitive*
                                             *meta-constraint-concepts*
                                             (tbox-blocking-possibly-required tbox)))
           (start (when *debug*
                    (get-internal-run-time)))
           (result
            (if (and *use-elh-model-embedding*
                     *use-elh-transformation*
                     (abox-el+-transformed-table abox)
                     (subset-el+-p (abox-language abox)))
                (progn
                  (abox-el+-saturation abox)
                  t)
              (and (or (tbox-coherent-p-internal tbox)
                       (not (incoherent-model-p (get-cached-concept-model (tbox-top-node tbox)))))
                   (progn
                     (when (and *use-elh-model-embedding*
                                *use-elh-transformation*
                                (abox-el+-transformed-table abox))
                       (setf (abox-el+-transformed-table abox) nil))
                     t)
                   (compute-initial-constraint-state abox)
                   (loop with synonym-table = (abox-model-individual-synonyms abox)
                         with reverse-synonym-table = (abox-reverse-model-individual-synonyms abox)
                         with use-unique-name-assumption = *use-unique-name-assumption*
                         for subgraph in (abox-subgraphs abox)
                         for start = (when *debug*
                                       (get-internal-run-time))
                         do (when *debug*
                              (format t "~&Testing (abox-satisfiable for subgraph (#inds=~D)..."
                                      (length (subgraph-individuals subgraph))))
                         always 
                         (and (loop for ind in (subgraph-individuals subgraph)
                                    when (member ind (individual-told-disjoints ind))
                                    do
                                    (race-trace ("individual ~S is disjoint to itself due to its disjoints ~S"
                                                 ind (individual-told-disjoints ind)))
                                    (return nil)
                                    finally (return t))
                              (let ((*expanded-model* nil)
                                    (*save-expanded-model* t)
                                    (concept-assertions 
                                     (mapcar #'encode-constraint
                                             (subgraph-encoded-individual-axioms subgraph))))
                                (multiple-value-bind
                                    (satisfiable precompletion)
                                    (test-abox-satisfiable abox
                                                           concept-assertions
                                                           (subgraph-encoded-role-axioms subgraph)
                                                           (subgraph-individual-names subgraph)
                                                           (abox-attribute-constraints abox)
                                                           (abox-initial-constraint-state abox)
                                                           nil
                                                           t)
                                  (when *debug*
                                    (format t "~S (~,3F secs)~%" 
                                            satisfiable
                                            (/ (- (get-internal-run-time) start) internal-time-units-per-second)))
                                  (when satisfiable
                                    (when precompletion
                                      #+:debug (assert (null (subgraph-precompletion subgraph)))
                                      (setf (subgraph-precompletion subgraph) precompletion)
                                      (setf (abox-initial-constraint-state abox)
                                            (state-concrete-domain-state
                                             (precompletion-state precompletion))))
                                    (setf (subgraph-completion subgraph) *expanded-model*)
                                    (let ((subgraph-synonym-table
                                           (precompletion-individual-synonyms
                                            (subgraph-completion subgraph))))
                                      (unless (or use-unique-name-assumption
                                                  (null subgraph-synonym-table))
                                        (add-subgraph-synonyms abox
                                                               subgraph-synonym-table
                                                               synonym-table
                                                               reverse-synonym-table)))
                                    t))))
                         finally (setf (abox-distribute-individual-constraints-p abox) t)))))
           (time (when *debug*
                   (/ (- (get-internal-run-time) start) internal-time-units-per-second))))
      (when *debug*
        (format t "~S (~,3F secs)~%" result time))
      result)))

(defun add-subgraph-synonyms (abox completion-synonym-table abox-synonym-table abox-reverse-synonym-table)
  (loop for ind-name being the hash-key of completion-synonym-table
        using (hash-value synonym-name)
        when (and (true-old-individual-p ind-name) (true-old-individual-p synonym-name))
        do
        (setf (gethash ind-name abox-synonym-table) (find-individual abox synonym-name))
        (setf (gethash synonym-name abox-reverse-synonym-table) (find-individual abox ind-name))))

(defun compute-initial-constraint-state (abox)
  "The initial constraint state encompasses the constraints for object names.
   If the initial constraint state is null, other parts of Racer assume that there
   are no object names mentioned in an ABox. This is important for building subgraphs,
   for constracting subgraphs etc. Thus, the state should not be null if there are any
   object names mentioned in an ABox."
  (let ((individual-object-table (racer-make-hash-table :test #'equal))
        (object-synonym-table (racer-make-hash-table))
        ;; The following variable is used to distinguish between an ABox where there
        ;; are no object names at all and an ABox in which object names are used to
        ;; enforce coreference but no concrete domain constraints on the object names
        ;; are given. In the latter case NIL must not be returned but an 
        ;; empty constraint state.
        (do-not-return-nil nil))
    (loop for ((individual object) attribute) in (abox-attribute-assertions abox) do
          (setf do-not-return-nil t)
          (pushnew object (gethash (list individual attribute) individual-object-table)))
    (loop for objects being the hash-values in individual-object-table using (hash-key key)
          as object = (first objects) do
          (pushnew (list (first key) (second key) object) (abox-attribute-constraints abox)
                   :test #'equal)
          (loop for object2  in (rest objects) do
                (setf (gethash object2 object-synonym-table) object)))
    (setf (abox-object-synonym-table abox) object-synonym-table)
    (let* ((state nil)
           (tbox (abox-tbox abox))
           (concrete-domain (tbox-concrete-domain tbox))
           cd-constraint)
      (loop for constraint in (abox-constraints abox) 
            as transformed-constraint = (eliminate-forks constraint object-synonym-table)
            do
            (multiple-value-bind (satisfiable-p new-state)
                                 (multiple-value-bind (predicate attributes)
                                                      (ensure-predicate transformed-constraint
                                                                        concrete-domain)
                                   (constraint-satisfiable-p 
                                    (setf cd-constraint (make-constraint predicate attributes))
                                    state))
              (unless satisfiable-p
                (return-from compute-initial-constraint-state (values nil state)))
              (cond ((unequal-predicate-p (cd-constraint-predicate cd-constraint))
                     (push cd-constraint (solver-state-nondeterministic-constraints new-state)))
                    (t
                     (push cd-constraint (solver-state-cd-constraints new-state))
                     (setf state new-state)))))
      (when do-not-return-nil
        (if (null state)
          ;; Other parts of Racer rely on the fact that a null solver state is returned
          ;; in case there are object names (but no constraints) mentioned in an ABox.
          (setf state (make-solver-state))))
      (setf (abox-initial-constraint-state abox) state)
      (values t state))))

(defun eliminate-forks (constraint object-synonym-table)
  (cons (first constraint)
        (loop for term in (rest constraint)
              collect (eliminate-forks-1 term object-synonym-table))))

(defun eliminate-forks-1 (term object-synonym-table)
  (cond ((symbolp term)
         (or (gethash term object-synonym-table) term))
        ((consp term)
         (cons (first term)
               (loop for term in (rest term)
                     collect (eliminate-forks-1 term object-synonym-table))))
        (t term)))

(defun abox-consistent-p (&optional (abox *current-abox*))
  (if *use-unique-name-assumption*
    (abox-una-consistent-p abox)
    (progn
      (check-type abox (or symbol abox))
      (setf abox (find-abox abox))
      (with-race-trace-sublevel ("abox-consistent-p"
                                 :arguments (list abox)
                                 :trace-result t)
        (ensure-knowledge-base-state ':abox-prepared abox)
        (let ((abox-coherent-p (abox-coherent-p abox))
              (abox-una-coherent-p (abox-una-coherent-p abox)))
          (cond ((eq abox-una-coherent-p t)
                 t)
                ((eq abox-coherent-p ':dont-know)
                 (with-abox-settings abox
                   (with-concept-definition-mapping (abox-tbox abox) 
                     (unless abox-una-coherent-p
                       (loop for individual in (abox-individuals-list abox) do
                             (setf (individual-distinct-p individual) nil)))
                     (prog1
                       (setf (abox-coherent-p abox) (abox-satisfiable abox))
                       (unless (abox-coherent-p abox)
                         (setf (abox-una-coherent-p abox) nil))))))
                (t abox-coherent-p)))))))

(defmacro abox-consistent? (&optional (abox-name nil abox-name-supplied-p))
  (if (and abox-name-supplied-p abox-name)
    `(abox-consistent-p ',abox-name)
    `(abox-consistent-p *current-abox*)))

(defun ensure-abox-is-coherent (abox)
  (abox-consistent-p abox))


(defun abox-consistent-if-assertions-added-p (abox assertions)
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (cond ((abox-consistent-p abox)
         (if (null assertions)
           t
           (with-race-trace-sublevel ("abox-consistent-if-assertions-added-p"
                                      :arguments (list abox assertions)
                                      :trace-result t)
             (with-abox-settings abox
               (with-concept-definition-mapping (abox-tbox abox) 
                 (multiple-value-bind (other-assertions same-as-assertions)
                                      (filter-assertions assertions)
                   (abox-consistent-if-assertions-added-p-1 (append other-assertions same-as-assertions)
                                                            abox
                                                            (make-hash-table :test #'eq)
                                                            nil nil nil 
                                                            (abox-attribute-constraints abox)
                                                            (copy-solver-state (abox-initial-constraint-state abox)))))))))
        (t 
         (racer-warn "Initial Abox is already inconsistent.")
         nil)))

(defun filter-assertions (assertions)
  (filter-assertions-1 assertions nil nil))

(defun filter-assertions-1 (assertions other-assertions same-as-assertions)
  (cond ((null assertions)
         (values other-assertions same-as-assertions))
        ((or (eq (first (first assertions)) 'same-as) (eq (first (first assertions)) 'same-as))
         (filter-assertions-1 (rest assertions) other-assertions (cons (first assertions) same-as-assertions)))
        (t (filter-assertions-1 (rest assertions) (cons (first assertions) other-assertions) same-as-assertions))))

(defun abox-consistent-if-assertions-added-p-1 (assertions 
                                                       abox
                                                       subgraph-ht
                                                       concept-constraints
                                                       relation-constraints
                                                       ind-or-inds
                                                       attribute-constraints
                                                       cd-state)
  (if (null assertions)
    (test-abox-satisfiable abox 
                           (mapcar #'encode-constraint concept-constraints)
                           relation-constraints
                           ind-or-inds
                           attribute-constraints
                           cd-state)
    (let ((assertion (first assertions)))
      (ecase (first assertion)
        ((instance  :instance)
         (let* ((ind (find-individual abox (second assertion) nil))
                (ind-subgraph (and ind (individual-subgraph ind))))
           (push (list (second assertion) (third assertion)) concept-constraints)
           (if (null ind)
             (push (second assertion) ind-or-inds)
             (unless (gethash ind-subgraph subgraph-ht)
               (setf concept-constraints
                     (append concept-constraints
                             (subgraph-encoded-individual-axioms ind-subgraph)))
               (setf relation-constraints
                     (append relation-constraints
                             (subgraph-encoded-role-axioms ind-subgraph)))
               (setf ind-or-inds
                     (append ind-or-inds
                             (subgraph-individual-names ind-subgraph)))
               (setf (gethash ind-subgraph subgraph-ht) t)))))
        
        ((related :related)
         (let* ((ind-1 (find-individual abox (second assertion) nil))
                (ind-2 (find-individual abox (third assertion) nil))
                (ind-1-subgraph (and ind-1 (individual-subgraph ind-1)))
                (ind-2-subgraph (and ind-2 (individual-subgraph ind-2))))
           (push (encode-constraint (list (list (second assertion) (third assertion)) (fourth assertion)))
                 relation-constraints)
           (if (null ind-1)
             (push (second assertion) ind-or-inds)
             (unless (gethash ind-1-subgraph subgraph-ht)
               (setf concept-constraints
                     (append concept-constraints
                             (subgraph-encoded-individual-axioms ind-1-subgraph)))
               (setf relation-constraints
                     (append relation-constraints
                             (subgraph-encoded-role-axioms ind-1-subgraph)))
               (setf ind-or-inds
                     (append ind-or-inds
                             (subgraph-individual-names ind-1-subgraph)))
               (setf (gethash ind-1-subgraph subgraph-ht) t)))
           (if (null ind-2)
             (push (third assertion) ind-or-inds)
             (unless (gethash ind-2-subgraph subgraph-ht)
               (setf concept-constraints
                     (append concept-constraints
                             (subgraph-encoded-individual-axioms ind-2-subgraph)))
               (setf relation-constraints
                     (append relation-constraints
                             (subgraph-encoded-role-axioms ind-2-subgraph)))
               (setf ind-or-inds
                     (append ind-or-inds
                             (subgraph-individual-names ind-2-subgraph)))
               (setf (gethash ind-2-subgraph subgraph-ht) t)))))
        ((different-from :different-from)
         (let* ((marker-concept (gensym))
                (ind-1 (find-individual abox (second assertion) nil))
                (ind-2 (find-individual abox (third assertion) nil))
                (ind-1-subgraph (and ind-1 (individual-subgraph ind-1)))
                (ind-2-subgraph (and ind-2 (individual-subgraph ind-2))))
           (create-tbox-internal-marker-concept (abox-tbox abox) marker-concept)
           (push (list (second assertion) marker-concept) concept-constraints)
           (push (list (third assertion) `(not ,marker-concept)) concept-constraints)
           (if (null ind-1)
             (push (second assertion) ind-or-inds)
             (unless (gethash ind-1-subgraph subgraph-ht)
               (setf concept-constraints
                     (append concept-constraints
                             (subgraph-encoded-individual-axioms ind-1-subgraph)))
               (setf relation-constraints
                     (append relation-constraints
                             (subgraph-encoded-role-axioms ind-1-subgraph)))
               (setf ind-or-inds
                     (append ind-or-inds
                             (subgraph-individual-names ind-1-subgraph)))
               (setf (gethash ind-1-subgraph subgraph-ht) t)))
           (if (null ind-2)
             (push (third assertion) ind-or-inds)
             (unless (gethash ind-2-subgraph subgraph-ht)
               (setf concept-constraints
                     (append concept-constraints
                             (subgraph-encoded-individual-axioms ind-2-subgraph)))
               (setf relation-constraints
                     (append relation-constraints
                             (subgraph-encoded-role-axioms ind-2-subgraph)))
               (setf ind-or-inds
                     (append ind-or-inds
                             (subgraph-individual-names ind-2-subgraph)))
               (setf (gethash ind-2-subgraph subgraph-ht) t)))))
        ((same-as :same-as)
         (let* ((ind-1 (find-individual abox (second assertion) nil))
                (ind-2 (find-individual abox (third assertion) nil))
                (ind-1-subgraph (and ind-1 (individual-subgraph ind-1)))
                (ind-2-subgraph (and ind-2 (individual-subgraph ind-2))))
           (if (null ind-1)
             (push (second assertion) ind-or-inds)
             (unless (gethash ind-1-subgraph subgraph-ht)
               (setf concept-constraints
                     (mapcar #'(lambda (assertion)
                                 (replace-ind-name-in-concept-assertion assertion ind-1 ind-2))
                             (append concept-constraints
                                     (subgraph-encoded-individual-axioms ind-1-subgraph))))
               (setf relation-constraints
                     (mapcar #'(lambda (axiom) 
                                 (encode-constraint
                                  (replace-ind-name-in-role-assertion axiom ind-1 ind-2)))
                             (append relation-constraints
                                     (subgraph-encoded-role-axioms ind-1-subgraph))))
               (setf ind-or-inds
                     (remove (first (individual-name-set ind-1))
                             (append ind-or-inds
                                     (subgraph-individual-names ind-1-subgraph))))
               (setf (gethash ind-1-subgraph subgraph-ht) t)))
           (if (null ind-2)
             (push (third assertion) ind-or-inds)
             (unless (gethash ind-2-subgraph subgraph-ht)
               (setf concept-constraints
                     (mapcar #'(lambda (assertion)
                                 (replace-ind-name-in-concept-assertion assertion ind-1 ind-2))
                             (append concept-constraints
                                     (subgraph-encoded-individual-axioms ind-2-subgraph))))
               (setf relation-constraints
                     (mapcar #'(lambda (axiom) 
                                 (encode-constraint
                                  (replace-ind-name-in-role-assertion axiom ind-1 ind-2)))
                             (append relation-constraints
                                     (subgraph-encoded-role-axioms ind-2-subgraph))))
               (setf ind-or-inds
                     (remove (first (individual-name-set ind-1))
                             (append ind-or-inds
                                     (subgraph-individual-names ind-2-subgraph))))
               (setf (gethash ind-2-subgraph subgraph-ht) t))))))
      (abox-consistent-if-assertions-added-p-1 (rest assertions)
                                               abox
                                               subgraph-ht
                                               concept-constraints
                                               relation-constraints
                                               ind-or-inds
                                               attribute-constraints
                                               cd-state))))



(defun prepare-abox (&optional (abox (current-abox)))
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (ensure-knowledge-base-state ':abox-prepared abox))


(defun abox-una-consistent-p (&optional (abox *current-abox*))
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (with-race-trace-sublevel ("abox-una-consistent-p"
                             :arguments (list abox)
                             :trace-result t)
    (with-unique-name-assumption
      (ensure-knowledge-base-state ':abox-prepared abox)
      (let ((abox-una-coherent-p (abox-una-coherent-p abox)))
        (cond ((eq abox-una-coherent-p ':dont-know)
               (with-abox-settings abox
                 (with-concept-definition-mapping (abox-tbox abox) 
                   (prog1
                     (setf (abox-una-coherent-p abox) (abox-satisfiable abox))
                     (when (abox-una-coherent-p abox)
                       (setf (abox-coherent-p abox) t))))))
              (t abox-una-coherent-p))))))

(defmacro abox-una-consistent? (&optional (abox-name nil abox-name-supplied-p))
  (if (and abox-name-supplied-p abox-name)
    `(abox-una-consistent-p ',abox-name)
    `(abox-una-consistent-p *current-abox*)))


;;; ======================================================================

(defmacro store-individual-constraint (constraint table)
  `(multiple-value-bind (ind found)
                        (gethash (constraint-ind constraint) ,table)
     (when found
       (unless (constraint-or-dependencies ,constraint)
         (push ,constraint
               (individual-ind-concept-constraints ind)))
       (push ,constraint
             (individual-concept-constraints ind)))))

(defun store-individual-constraints (abox
                                          concept-constraints
                                          expanded-store
                                          new-relation-constraints)
  (let ((table (abox-individuals-table abox)))
    (loop for relation-constraint in new-relation-constraints do
          (multiple-value-bind (ind-1 found-1)
              (gethash (constraint-ind-1 relation-constraint) table)
            (multiple-value-bind (ind-2 found-2)
                (gethash (constraint-ind-2 relation-constraint) table)
              (when (and found-1 found-2)
                (push relation-constraint
                      (individual-added-relation-constraints ind-1))
                (push relation-constraint
                      (individual-added-relation-constraints ind-2))))))
    (loop for constraint in concept-constraints
          do (store-individual-constraint constraint table))
    (when (consp expanded-store)
      (iterate-over-all-constraints (constraint expanded-store)
        do (store-individual-constraint constraint table)))))


;;; ======================================================================
;;;
;;; Preparation of knowledge bases.
;;;
;;;

(defconstant +100%-n-dots+ 70)

(defvar *largest-ratio*)

(defun advance-progress-bar (n-of-new-ticks stream)
  (loop repeat n-of-new-ticks do
        (princ "|" stream)
	(set-progress-value :inc)
        (force-output stream)))

(defun ensure-rest-of-progress-bar-is-shown (n-of-ticks stream)
  (loop repeat (- +100%-n-dots+ n-of-ticks) do 
        (princ "|" stream)
	(set-progress-value :inc)
        (force-output stream))
  (terpri stream)
  (force-output stream))

(defmacro with-progress-indication ((activity-title &key (stream '*standard-output*)
                                                    activate-if)
                                    &body forms)
  (let ((stream-var (gensym))
        (activity-title-var (gensym))
        (activate-if-var (gensym)))
    `(progn (let ((,stream-var ,stream)
                  (n-of-ticks 0)
                  (title-printed nil)
                  (,activate-if-var ,activate-if))
              (let* ((*largest-ratio* 0)
                     (,activity-title-var ,activity-title)
                     (*tick-function* 
                      #'(lambda (current-ratio)
                          (when (> current-ratio *largest-ratio*)
                            (let ((n-of-new-ticks (floor (- (* current-ratio 
                                                               +100%-n-dots+)
                                                            n-of-ticks))))
                              (incf n-of-ticks n-of-new-ticks)
			      (when ,activate-if-var 
                                (unless title-printed
                                  (setf title-printed t)
                                  (format ,stream-var "~%~A" ,activity-title)
                                  (loop repeat (- +100%-n-dots+ 
                                                  (length ,activity-title-var))
				      do (princ "." ,stream-var))
                                  (terpri ,stream-var)
                                  (force-output ,stream-var))
                                (advance-progress-bar n-of-new-ticks ,stream-var))
                              (setf *largest-ratio* current-ratio))))))
		(with-progress-range (+100%-n-dots+ (0 100))
		  (unwind-protect
		      (progn .,forms)
		    (unless (zerop n-of-ticks)
		      (when ,activate-if-var
			(ensure-rest-of-progress-bar-is-shown n-of-ticks 
							      ,stream-var))))))))))

(defun ensure-knowledge-base-state (required-state tbox-or-abox)
  (let ((individual-name (and (consp required-state) (second required-state))))
    (ecase (if (consp required-state)
             (first required-state)
             required-state)
      (:tbox-prepared 
       (case *auto-classify*
         (:lazy (ensure-tbox-state tbox-or-abox :setup))
         (:lazy-verbose
          (ensure-tbox-state tbox-or-abox :setup))
         (:eager 
          (ensure-tbox-state tbox-or-abox :classification))
         (:eager-verbose 
          (with-progress-indication ("Classifying TBox" :activate-if *tbox-verbose*)
            (ensure-tbox-state tbox-or-abox :classification)))
         (otherwise 
          (if (null *auto-classify*)
            (error "TBox ~S not classified."
                   (tbox-name tbox-or-abox))
            (error "*auto-classify*'s value is ~S but must be one of ~
                    :lazy :lazy-verbose :eager :eager-verbose or nil."
                   *auto-classify*)))))
      (:tbox-classified
       (case *auto-classify*
         ((:lazy :eager)
          (ensure-tbox-state tbox-or-abox :classification))
         ((:lazy-verbose :eager-verbose)
          (with-progress-indication ("Classifying TBox" :activate-if *tbox-verbose*)
            (ensure-tbox-state tbox-or-abox :classification)))
         (otherwise 
          (if (null *auto-classify*)
            (error "TBox ~S not classified."
                   (tbox-name tbox-or-abox))
            (error "*auto-classify*'s value is ~S but must be one of ~
                    :lazy :lazy-verbose :eager :eager-verbose or nil."
                   *auto-classify*)))))
      (:abox-prepared
       (case *auto-realize*
         ((:lazy :eager)
          (ensure-tbox-and-abox-synchronization tbox-or-abox)
          (ensure-abox-caches-are-correct tbox-or-abox)
          (ensure-knowledge-base-state (if (abox-query-subsumption tbox-or-abox)
                                         ':tbox-classified
                                         ':tbox-prepared)
                                       (abox-tbox tbox-or-abox))
          (unless (abox-index-structures-complete-p tbox-or-abox)
            (when-statistics *race-statistics-stream*
                             (print-abox-statistics-name tbox-or-abox *race-statistics-stream*))
            (with-abox-settings tbox-or-abox
              (with-concept-definition-mapping (abox-tbox tbox-or-abox) 
                (race-time (build-abox-index-structures tbox-or-abox))))))
         ((:lazy-verbose :eager-verbose)
          (ensure-tbox-and-abox-synchronization tbox-or-abox)
          (ensure-abox-caches-are-correct tbox-or-abox)
          (ensure-knowledge-base-state (if (abox-query-subsumption tbox-or-abox)
                                         ':tbox-classified
                                         ':tbox-prepared)
                                       (abox-tbox tbox-or-abox))
          (unless (abox-index-structures-complete-p tbox-or-abox)
            (when-statistics *race-statistics-stream*
                             (print-abox-statistics-name tbox-or-abox *race-statistics-stream*))
            (with-abox-settings tbox-or-abox
              (with-concept-definition-mapping (abox-tbox tbox-or-abox) 
                (race-time (build-abox-index-structures tbox-or-abox))))))
         (otherwise 
          (unless (null *auto-realize*)
            (error "*auto-realize*'s value is ~S but must be one of ~
                    :lazy :lazy-verbose :eager :eager-verbose or nil."
                   *auto-realize*)))))
      (:abox-realized
       (case *auto-realize*
         ((:eager :lazy)
          (ensure-tbox-and-abox-synchronization tbox-or-abox)
          (ensure-abox-caches-are-correct tbox-or-abox)
          (ensure-knowledge-base-state (if (abox-query-subsumption tbox-or-abox)
                                         ':tbox-classified
                                         ':tbox-prepared)
                                       (abox-tbox tbox-or-abox))
          (unless (or (boundp '*clash-reasons*) ;; realize-abox already in progress
                      (abox-realized-p-internal tbox-or-abox))
            (realize-abox tbox-or-abox (if (eq *auto-realize* ':lazy)
                                         individual-name
                                         nil))))
         ((:eager-verbose :lazy-verbose)
          (ensure-tbox-and-abox-synchronization tbox-or-abox)
          (ensure-abox-caches-are-correct tbox-or-abox)
          (unless (or (boundp '*clash-reasons*) ;; realize-abox already in progress
                      (abox-realized-p-internal tbox-or-abox))
            (ensure-knowledge-base-state ':tbox-classified (abox-tbox tbox-or-abox))
            (with-progress-indication ("Realizing ABox" :activate-if *abox-verbose*)
              (realize-abox tbox-or-abox (if (eq *auto-realize* ':lazy-verbose)
                                           individual-name
                                           nil)))))
         (otherwise 
          (unless (null *auto-realize*)
            (error "*auto-realize*'s value is ~S but must be one of ~
                    :lazy :lazy-verbose :eager :eager-verbose or nil."
                   *auto-realize*))))))))

(defun ensure-tbox-and-abox-synchronization (abox)
  (let ((tbox (abox-tbox abox)))
    (cond ((null (abox-tbox-identification abox))
           (setf (abox-tbox-identification abox) (tbox-identification tbox)))
          ((not (eq (abox-tbox-identification abox) (tbox-identification tbox)))
           (initialize-abox abox)))))

(defun ensure-abox-caches-are-correct (abox)
  (when (and (abox-index-structures-complete-p abox)
             (not (eq (abox-current-una-assumption abox) *use-unique-name-assumption*)))
    (initialize-abox abox))
  (setf (abox-current-una-assumption abox) *use-unique-name-assumption*))


;;; ======================================================================



(defun member-of-named-concept-p (abox ind-name concept-name &optional (errorp t)) 
  (setf abox (find-abox abox))
  (with-race-trace-sublevel ("member-of-named-concept-p"
                             :arguments (list abox ind-name concept-name errorp)
                             :trace-result t)
    (unless (ensure-abox-is-coherent abox)
      (error "ABox ~A is incoherent." (abox-name abox)))
    (unless (symbolp concept-name)
      (error "Concept name expected."))
    (ensure-knowledge-base-state `(:abox-realized ,ind-name) abox)
    (with-concept-definition-mapping (abox-tbox abox) 
      (with-abox-settings abox
        (and (find (get-tbox-concept (abox-tbox abox) concept-name errorp)
                   (individual-ancestor-concepts (find-individual abox ind-name)))
             t)))))


(defun member-of-concept-p (abox ind-name concept-term)
  (setf abox (find-abox abox))
  (with-race-trace-sublevel ("member-of-concept-p"
                             :arguments (list abox ind-name concept-term)
                             :trace-result t)
    (ensure-knowledge-base-state ':abox-prepared abox)
    (let ((tbox (abox-tbox abox)))
      (with-concept-definition-mapping tbox
        (with-abox-settings abox
          (unless (ensure-abox-is-coherent abox)
            (error "ABox ~A is incoherent." (abox-name abox)))
          (test-ind-subsumes (find-individual abox ind-name)
                             (encode-update-concept-term tbox concept-term)
                             abox))))))

;;; ======================================================================


(defun realize-individuals (abox)
  (if *smart-realization*
      (realize-individuals-2 abox)
    (realize-individuals-1 abox)))

(defun realize-individuals-1 (abox)
  (let* ((tbox (abox-tbox abox))
         (n-of-realized-individuals 0)
         (n-individuals (hash-table-count (abox-individuals-table abox)))
         (n-concepts (length (tbox-encoded-concept-list tbox)))
	 (abox-inds (abox-individuals-list abox))
	 (n (length abox-inds)))
    (with-progress-range (n (0 100))
      (loop with abox-elh-p = (and (subset-el+-p (tbox-language tbox))
				   (subset-el+-p (abox-language abox)))

	  for ind in abox-inds do
	    (set-progress-value :inc)
	    (without-progress		; in order to keep abox-consistent-p from interfering...
	     (unless (individual-realized-p ind)
	       (when-debug *print-where-am-i*
			   (format *trace-output* " ~S" ind))
	       (incf n-of-realized-individuals)
	       (when (and (not (or *debug* *print-where-am-i*))
			  (or (> n-concepts 
				 *minimum-n-concepts-for-classification-progress-indication*)
			      (> n-individuals 
				 *minimum-n-individuals-for-realization-progress-indication*)))
		 (funcall *tick-function* (/ n-of-realized-individuals n-individuals)))
            
	       (realize-individual abox tbox ind abox-elh-p t)))))))

(defun realize-individuals-2 (abox)
  (let* ((tbox (abox-tbox abox))
         (tbox-top-node (tbox-top-node tbox))
         (individuals (remove-if #'individual-realized-p 
                                 (abox-individuals-list abox)))
         (untouched-mark (incf *tbox-classification-counter*))
	 (n (length individuals)))
    (traverse-terminology-lattice-0 tbox-top-node untouched-mark)
    (setf (concept-mark2 tbox-top-node) individuals)
    (traverse-terminology-lattice-1 tbox-top-node individuals abox untouched-mark)
    (traverse-terminology-lattice-2 tbox-top-node abox)
    (with-progress-range (n (0 100))
      (loop for ind in individuals
	  unless (individual-realized-p ind) do
	    (set-progress-value :inc)
	    (without-progress		; in order to keep abox-consistent-p from interfering...
	     (when-debug *print-where-am-i*
			 (format *trace-output* " ~S" ind))
	     (setf (individual-ancestor-concepts ind)
	       (compute-individual-ancestors (individual-parent-concepts ind)))
	     (setf (individual-realized-p ind) t))))))

(defun traverse-terminology-lattice-0 (concept untouched-mark)
  (setf (concept-mark2 concept) untouched-mark)
  (let ((concepts (filter-visibles concept #'concept-children-internal #'identity)))
    (with-progress-range ((length concepts) (0 100))
      (loop for concept-1 in concepts
	  do 
	    (set-progress-value :inc)
	    (unless (eql (concept-mark2 concept-1) untouched-mark)
	      (traverse-terminology-lattice-0 concept-1 untouched-mark))))))

(defun traverse-terminology-lattice-1 (concept individuals abox untouched-mark)
  (when (not (null individuals))
    (let ((concepts (filter-visibles concept #'concept-children-internal #'identity)))
      (with-progress-range ((length concepts) (0 100))
	(loop with top = (tbox-bottom-node (tbox abox))
	    for concept-1 in concepts
	    when (progn 
		   (set-progress-value :inc)
		   (eql (concept-mark2 concept-1) untouched-mark))
	    do 
	      (let ((instances (cond ((eq concept-1 top)
                                  nil)
                                 (*use-dependency-based-instance-retrieval*
                                  (dependency-based-instance-retrieval concept-1 abox individuals))
                                 (*use-binary-instance-retrieval*
                                  (binary-instance-retrieval concept-1 abox individuals))
                                 (t (linear-instance-retrieval concept-1 abox individuals)))))
		(if (> (length instances) 10)
		    (loop with table = (racer-make-hash-table :size (length instances))
			for ind in instances do
			  (setf (gethash ind table) t)
			finally (setf (concept-mark2 concept-1) table))
		  (setf (concept-mark2 concept-1) instances))
		(traverse-terminology-lattice-1 concept-1 instances abox untouched-mark)))))))

(defun traverse-terminology-lattice-2 (concept abox)
  (unless (null (concept-mark2 concept))
    (let ((children (filter-visibles concept #'concept-children-internal #'identity)))
      (with-progress-range ((length children) (0 100))
	(if (consp (concept-mark2 concept))
	    (loop for ind in (concept-mark2 concept)
		unless (loop for concept in children
			   thereis (smart-find ind (concept-mark2 concept)))
		do 
		  (add-associated-individuals abox concept ind t)
		  (pushnew concept (individual-parent-concepts ind)))
	  (loop for ind being the hash-key of (concept-mark2 concept)
	      unless (loop for concept in children
			 thereis (smart-find ind (concept-mark2 concept)))
	      do 
		(add-associated-individuals abox concept ind t)
		(pushnew concept (individual-parent-concepts ind))))
	(loop for concept-1 in children do 
	      (set-progress-value :inc)
	      (traverse-terminology-lattice-2 concept-1 abox))))))

(defun smart-find (ind list-or-table)
  (if (listp list-or-table)
      (member ind list-or-table)
    (gethash ind list-or-table)))

(defun realize-individual (abox tbox ind abox-elh-p &optional (as-part-of-abox-realization nil))  
  (unless (individual-realized-p ind)
    (let ((*realization-in-progress* t))
      (when *debug*
        (format *trace-output*
                "~%Realizing ~S..." ind))
      (when *print-where-am-i*
        (format *trace-output* " ~S" ind))
      (if (and abox-elh-p 
               *use-elh-model-embedding*
               *use-elh-transformation*
               (tbox-el+-transformed-table tbox))
          (let ((ancestors (individual-told-subsumers ind))
                (top-list (list (tbox-top-node tbox))))
            (setf (individual-ancestor-concepts ind) (or ancestors top-list))
            (setf (individual-parent-concepts ind)
                  (or (extract-concept-parents-from-ancestors tbox ind nil) top-list))
            (loop for parent in (individual-parent-concepts ind) do
                  (add-associated-individuals abox parent ind as-part-of-abox-realization)))
        (let* ((*auto-install-primitives* nil)
               (subsumer-mark (incf *tbox-classification-counter*))
               (non-subsumer-mark (incf *tbox-classification-counter*)))
          (if (null (individual-told-subsumers ind))
            (mark-all-ancestors (tbox-top-node tbox) subsumer-mark)
            (loop for subsumer in (individual-told-subsumers ind) do
                  (mark-all-ancestors subsumer subsumer-mark)))
          (loop for non-subsumer in (individual-told-non-subsumers ind) do
                (mark-all-descendants non-subsumer non-subsumer-mark))
          (setf (individual-parent-concepts ind)
                (abox-top-search ind (tbox-top-node tbox) abox subsumer-mark non-subsumer-mark))
          (setf (individual-ancestor-concepts ind)
                (compute-individual-ancestors (individual-parent-concepts ind)))
          (loop for parent in (individual-parent-concepts ind) do
                (add-associated-individuals abox parent ind as-part-of-abox-realization))))
      (setf (individual-realized-p ind) t)
      (when *debug*
        (format *trace-output*
                "parents: ~S"
                (individual-parent-concepts ind))
        (format *trace-output*
                " done.")))))

(defun compute-individual-ancestors (concepts)
  (let ((*visited-concepts* nil))
    (loop for concept in concepts 
          nconc (compute-individual-ancestors-1 concept))))

(defun compute-individual-ancestors-1 (concept)
  (unless (member concept *visited-concepts*)
    (push concept *visited-concepts*)
    (cons concept 
          (loop for concept-1 in (concept-parents-internal concept)
                nconc (compute-individual-ancestors-1 concept-1)))))

(defun update-ind-pseudo-model-det-pos-literals (ind concept abox)
  (let* ((model (get-cached-individual-model ind abox))
         (ind-name (first (individual-name-set ind)))
         (synonym-ind (or (gethash ind-name (abox-model-individual-synonyms abox))
                          (gethash ind-name (abox-reverse-model-individual-synonyms abox))))
         (synonym-ind-model (when synonym-ind
                              (get-cached-individual-model synonym-ind abox))))
      (when (and synonym-ind-model (eq synonym-ind-model model))
        ; we need to copy the model before modifying it
        (if (full-model-info-p model)
            (progn
              (setf (individual-model ind) (copy-full-model-info model))
              (setf (model-individual (individual-model ind)) ind))
          (setf (individual-model ind) (copy-model-info model)))))
  ;; ind model might have been changed
  (let ((model (get-cached-individual-model ind abox)))
    (when (model-info-p model)
      #+:debug (assert (atomic-concept-p concept))
      #+:debug (assert (not (member concept (model-det-positive-literals model))))
      (push concept (model-det-positive-literals model))
      (when (full-model-info-p model)
        (multiple-value-bind (new-pos-literals removed-p)
            (racer-remove concept (model-positive-literals model))
          (if removed-p
              (setf (model-positive-literals model) new-pos-literals)
            (multiple-value-bind (new-neg-literals removed-p)
                (racer-remove concept (model-negative-literals model))
              (when removed-p
                (setf (model-negative-literals model) new-neg-literals))))))
      #+:debug model)))

(defun add-pending-new-parent (ind concept subgraph abox)
  (push (encode-constraint (list (first (individual-name-set ind)) concept))
        (subgraph-pending-new-parents subgraph))
  (update-ind-pseudo-model-det-pos-literals ind concept abox))

(defparameter *use-et-ind-model* nil)

(defun get-cached-individual-model (ind abox)
  (or (prog1
          (individual-model ind)
        (incf-statistics *ind-model-cache-hits*))
      (progn
        (unless (or (individual-model-built-p ind) (abox-el+-transformed-table abox))
          (prepare-individual-model ind abox))
        (let ((synonym-ind (unless *use-unique-name-assumption*
                             (gethash (first (individual-name-set ind))
                                      (abox-model-individual-synonyms abox)))))
          (if (and synonym-ind (not (eq ind synonym-ind)))
              (let ((model (get-cached-individual-model synonym-ind abox)))
                (incf-statistics *ind-model-cache-misses*)
                (setf (individual-model ind) model))
            (let* ((model (create-individual-model ind
                                                   abox
                                                   (individual-concept-constraints ind)))
                   (subsumers (when (model-info-p model)
                                (model-det-positive-literals model)))
                   (disjoints (when (full-model-info-p model)
                                (model-det-negative-literals model))))
              (incf-statistics *ind-model-cache-misses*)
              (when *use-et-ind-model*
                (unless (concept-set-subsetp subsumers (individual-told-subsumers ind))
                ;(format t "+IS:~D " (length (concept-set-difference subsumers (individual-told-subsumers ind))))
                  (setf (individual-told-subsumers ind)
                        (concept-set-union subsumers (individual-told-subsumers ind))))
                (unless (concept-set-subsetp disjoints (individual-told-disjoints ind))
                ;(format t "+ID:~D " (length (concept-set-difference disjoints (individual-told-disjoints ind))))
                  (setf (individual-told-disjoints ind)
                        (concept-set-union disjoints (individual-told-disjoints ind)))))
              (when (individual-concept-name ind)
                (setf (concept-model (get-tbox-concept (abox-tbox abox) (individual-concept-name ind)))
                      model))
              (when (full-model-info-p model)
                (setf (model-individual model) ind))
              (setf (individual-model ind) model)))))))

(defun elh-ind-merging-test-p (model-list use-relation-store-p)
  ;; works only for true ELH fragment
  (and *use-elh-model-embedding*
       use-relation-store-p
       (null *meta-constraint-concepts*)
       (with-alc-language-settings (:models model-list)
         (subset-elh-p *dl-prover-language*))))

(defun embeddable-into-ind-model-p (concept-model ind)
  (let ((ind-model (individual-model ind)))
    (or (eq concept-model ind-model)
        (progn
          #+:debug
          (assert (not (and (full-model-info-p ind-model)
                            (or (model-non-deterministic-p ind-model)
                                (model-negative-literals ind-model)
                                (model-restrict-models ind-model)
                                (model-det-negative-literals ind-model)
                                (model-attributes ind-model)
                                (model-ensured-attributes ind-model)))))
          (cond
           ((atomic-concept-p ind-model) nil)
           ((atomic-concept-p concept-model)
            #+:debug (assert (not (or (and (full-model-info-p ind-model)
                                           (model-non-deterministic-p ind-model))
                                      (not (concept-primitive-p concept-model)))))
            (member concept-model (model-det-positive-literals ind-model)))
           (t
            #+:debug
            (assert (not (and (full-model-info-p concept-model)
                              (or (model-non-deterministic-p concept-model)
                                  (model-negative-literals concept-model)
                                  (model-restrict-models concept-model)
                                  (model-det-negative-literals concept-model)
                                  (model-attributes concept-model)
                                  (model-ensured-attributes concept-model)))))
            (cond
             ((and (some-concept-p concept-model)
                   (some-concept-p ind-model))
              (and (elh-concept-set-subsetp (concept-told-subsumers concept-model)
                                            (concept-told-subsumers ind-model))
                   (embeddable-into-ind-neighbors-p ind (list concept-model))))
             ((some-concept-p concept-model)
              (and (elh-concept-set-subsetp (concept-told-subsumers concept-model)
                                            (model-det-positive-literals ind-model))
                   (embeddable-into-ind-neighbors-p ind (list concept-model))))
             ((some-concept-p ind-model)
              (and (elh-concept-set-subsetp (model-det-positive-literals concept-model)
                                            (concept-told-subsumers ind-model))
                   (embeddable-into-ind-neighbors-p ind (model-exists-models concept-model))))
             (t
              (and (elh-concept-set-subsetp (model-det-positive-literals concept-model)
                                            (model-det-positive-literals ind-model))
                   (embeddable-into-ind-neighbors-p ind (model-exists-models concept-model)))))))))))

(defun embeddable-into-ind-neighbors-p (ind exists-models)
  (loop with state = (precompletion-state (subgraph-completion (individual-subgraph ind)))
        with expanded-constraints = (state-expanded-constraints state)
        with expanded-store = (state-expanded-store state)
        with expanded-store-index = (state-expanded-store-index state)
        with relation-store = (state-relation-store state)
        with ind-name = (first (individual-name-set ind))
        with ind-model-exists-models = (model-exists-models (individual-model ind))
        with trans-p = (dl-transitive-roles *dl-prover-language*)
        for exists-model in exists-models
        always 
        (or (role-successor-exists-p ind-name
                                     (concept-role exists-model)
                                     (concept-term exists-model)
                                     expanded-constraints
                                     expanded-store
                                     expanded-store-index
                                     relation-store)
            (loop with complex-p = (dl-complex-role-inclusions (concept-language exists-model))
                  with new-exists-models = (if (and (or trans-p complex-p) 
                                                    (some-concept-p exists-model))
                                               (expand-complex-some-concept exists-model complex-p trans-p)
                                             (list exists-model))
                  for new-exists-model in new-exists-models
                  thereis
                  (loop for ind-exists-model in ind-model-exists-models
                        thereis
                        (exists-models-embeddable new-exists-model
                                                  ind-exists-model))))))

(defun obviously-ind-not-subsumes (ind concept abox)
  (let* ((*deep-model-merging* nil)
         (*meta-constraint-concepts*
          (and *use-tbox* (tbox-meta-constraint-concepts *use-tbox*)))
         (*blocking-possibly-required*
          (or *encode-roles-as-transitive*
              (and *use-tbox*
                   (or *meta-constraint-concepts*
                       (tbox-blocking-possibly-required *use-tbox*)))))
         (*blocking-used* nil)
         (model-2 (get-cached-individual-model ind abox)))
    (unless (incoherent-model-p model-2)
      (let ((top *top-concept*)
	    (bottom *bottom-concept*))
	(if (or (eq concept top)
		(and (concept-p-internal concept)
		     (and (atomic-concept-p concept)
		          (eq (concept-encoded-negated-definition concept) bottom))
		     (and (negated-concept-p concept)
		          (eq (concept-encoded-definition (concept-term concept)) top))))
          nil
	  (if (eq concept bottom)
	    t
	    (let ((elh-model-1 (get-cached-concept-model concept))
		  (model-2-list (list model-2)))
	      (if (and (not (and (full-model-info-p model-2) (model-non-deterministic-p model-2)))
                       (let* ((subgraph (individual-subgraph ind))
                              (precompletion (subgraph-precompletion subgraph)))
                         (elh-ind-merging-test-p (cons elh-model-1 model-2-list)
                                                 (use-relation-store-p
                                                  (when precompletion
                                                    (state-relation-store
                                                     (precompletion-state precompletion)))
                                                  precompletion
                                                  (unless precompletion
                                                    (subgraph-encoded-role-axioms subgraph))))))
                (not (embeddable-into-ind-model-p elh-model-1 ind))
		(let ((model-1 (get-cached-concept-model (concept-negated-concept concept))))
		  (unless (incoherent-model-p model-1)
		    (let ((model-list (cons model-1 model-2-list)))
		      (with-alc-language-settings (:models model-list)
			(multiple-value-bind (mergable partial)
			                     (with-alternate-models-mergable-p model-1 model-2)
			  (if mergable
                            (incf-statistics *ind-mergable-models*)
			    (if partial
                              (incf-statistics *ind-unmergable-partial-models*)
			      (incf-statistics *ind-unmergable-det-models*)))
			  (values mergable partial))))))))))))))

(defun obviously-inds-not-interact (ind-1 ind-2 abox)
  (let* ((*deep-model-merging* nil)
	 (*meta-constraint-concepts*
	  (and *use-tbox* (tbox-meta-constraint-concepts *use-tbox*)))
	 (*blocking-possibly-required*
	  (or *encode-roles-as-transitive*
	      (and *use-tbox*
		   (or *meta-constraint-concepts*
		       (tbox-blocking-possibly-required *use-tbox*)))))
	 (*blocking-used* nil)
	 (model-1 (get-cached-individual-model ind-1 abox))
	 (model-2 (get-cached-individual-model ind-2 abox)))
    (unless (or (incoherent-model-p model-1)
		(incoherent-model-p model-2))
      (if (eq model-1 model-2)
        (values nil
                (or (and (full-model-info-p model-1) (model-non-deterministic-p model-1))
                    (and (full-model-info-p model-2) (model-non-deterministic-p model-2))))
	(let ((model-list (list model-1 model-2)))
	  (with-alc-language-settings (:models model-list)
	    (multiple-value-bind (mergable partial)
		                 (with-alternate-models-mergable-p model-1 model-2)
	      (if mergable
                (incf-statistics *ind-mergable-models*)
		(if partial
                  (incf-statistics *ind-unmergable-partial-models*)
		  (incf-statistics *ind-unmergable-det-models*)))
	      (values mergable partial))))))))

(defun non-deterministic-clash-dependencies-p (dependencies)
  (when dependencies
    (or (loop for constraint in dependencies
              thereis (or (or-constraint-p constraint)
                          (qualified-role-signature-p constraint)))
        (collect-dependencies dependencies))))

(defun test-ind-subsumes (ind concept abox)
  (with-race-trace-sublevel ("test-ind-subsumes"
                             :arguments (list ind concept abox)
                             :trace-result t)
    (when *debug*
      (format t "~&Testing (test-ind-subsumes ~S ~S)..." ind concept))
    
    (let ((contraction-useful-p (null (abox-initial-constraint-state abox)))
	  (atomic-p (atomic-concept-p concept))
          (classified-p (tbox-classified-p-internal (abox-tbox abox)))
          (start (when *debug*
                   (get-internal-run-time))))
      (cond
       ((and atomic-p 
             (or (member concept (model-det-positive-literals (get-cached-individual-model ind abox)))
                 (loop for subsumer in (individual-told-subsumers ind)
                       thereis (or (eq concept subsumer)
                                   (and classified-p
                                        (member concept (get-concept-ancestors subsumer)))))))
        (when *debug*
          (format t "~S~%" t))
        t)
       ((and atomic-p
             (or (member concept (individual-told-non-subsumers ind))
                 (and classified-p
                      (lists-not-disjoint-p (get-concept-ancestors concept)
                                            (individual-told-non-subsumers ind)))))
        (when *debug*
          (format t "~S~%" nil))
        nil)
       (t
        (multiple-value-bind (not-subsumes-p unknown-p)
            (if (and *model-merging* contraction-useful-p *abox-model-merging*)
                (obviously-ind-not-subsumes ind concept abox)
              (values nil t))
          (let* ((result-1
                  (cond
                   (not-subsumes-p nil)
                   ((not unknown-p) t)
                   (t 'unknown)))
                 (result-2
                  (if (eq result-1 'unknown)
                      (let ((subgraph (individual-subgraph ind)) 
                            (individual-concept 
                             (when (and contraction-useful-p
                                        (not classified-p)
                                        (null (tbox-meta-constraint-concepts (abox-tbox abox))))
                               (get-individual-concept ind))))
                        (if (and individual-concept
                                 (or (eq concept individual-concept)
                                     (progn
                                       (incf-statistics *ind-concept-subsumption-tests*)
                                       (test-subsumes concept individual-concept))))
                            (progn
                              (when (and (not (eq concept individual-concept)) (atomic-concept-p concept))
                                (add-pending-new-parent ind concept subgraph abox)
                                (incf-statistics *ind-concept-subsumptions-found*))
                              t)
                          (if *use-abox-completion*
                              (let ((completion (subgraph-completion subgraph)))
                                (if completion
                                    (let ((result
                                           (test-abox-satisfiable 
                                            abox
                                            (list (with-constraint-ignore-determinism
                                                   (encode-constraint (list (first (individual-name-set ind))
                                                                            `(not ,concept)))))
                                            nil
                                            nil
                                            nil
                                            nil
                                            completion)))
                                      (incf-statistics *ind-abox-completion-tests*)
                                      (if result
                                          (progn
                                            (incf-statistics *ind-abox-completion-non-subsumption*)
                                            nil)
                                        (if (non-deterministic-clash-dependencies-p *catching-clash-dependencies*)
                                            'unknown
                                          (progn
                                            (when (atomic-concept-p concept)
                                              (add-pending-new-parent ind concept subgraph abox)
                                              (incf-statistics *ind-concept-subsumptions-found*))
                                            t))))
                                  'unknown))
                            'unknown)))
                    result-1))
                 (result-3
                  (if (eq result-2 'unknown)
                      (test-ind-subsumes-1 ind concept abox)
                    result-2))
                 (time (when *debug*
                         (/ (- (get-internal-run-time) start)
                            internal-time-units-per-second))))
            (when-print-statistics
              (print-sat-local-statistics))
            (when-collect-statistics
              (collect-statistics-data))
            (cond ((not (eq result-1 'unknown))
                   (when *debug*
                     (format t "~S (~,3F secs)~%" result-1 time))
                   result-1)
                  ((not (eq result-2 'unknown))
                   (when-statistics (and *model-merging* contraction-useful-p *abox-model-merging*)
                     (if result-2
                         (incf-statistics *ind-unmergable-models-unsatisfiable*)
                       (incf-statistics *ind-unmergable-models-satisfiable*)))
                   (when *debug*
                     (format t "~S (~,3F secs)~%" result-2 time))
                   result-2)
                  (t 
                   (when *debug*
                     (format t "~S (~,3F secs)~%" result-3 time))
                   result-3)))))))))


(defvar *new-attribute-constraints*)

(defun constraint-entailed-p (constraint &optional (abox *current-abox*))
  "The initial constraint state encompasses the constraints for object names.
   If the initial constraint state is null, other parts of Racer assume that there
   are no object names mentioned in an ABox. This is important for building subgraphs,
   for constracting subgraphs etc. Thus, the state should not be null if there are any
   object names mentioned in an ABox."
  (setf abox (find-abox abox))
  (check-type abox abox)
  (unless (ensure-abox-is-coherent abox)
    (error "ABox ~A is incoherent." (abox-name abox)))
  
  (with-race-trace-sublevel ("constraint-entailed-p"
                             :arguments (list constraint abox)
                             :trace-result t)
    (when *debug*
      (format t "~&Testing (constraint-entailed-p ~S ~S)..." constraint abox))
    (flet ((constraint-entailed-p-continuation-2 (sat unused-1 unused-2 unused-3 unused-4)
             (declare (ignore unused-1 unused-2 unused-3 unused-4))
             sat))
      (let* ((*meta-constraint-concepts* (tbox-meta-constraint-concepts (abox-tbox abox)))
             (*blocking-possibly-required* (or *encode-roles-as-transitive*
                                               *meta-constraint-concepts*
                                               (tbox-blocking-possibly-required (abox-tbox abox))))
             (*new-attribute-constraints* nil)
             (start (when *debug*
                      (get-internal-run-time))))
        (with-abox-settings abox
          (with-concept-definition-mapping (abox-tbox abox) 
            (let* ((cd-state (abox-initial-constraint-state abox))
                   (tbox (abox-tbox abox))
                   (concrete-domain (tbox-concrete-domain tbox))
                   (transformed-constraint (transform-constraint constraint abox)))
              (multiple-value-bind (predicate attributes)
                  (ensure-predicate transformed-constraint concrete-domain)
                (declare (ignore attributes))
                #+:debug
                (when (stringp predicate)
                  (error predicate))
                #+:debug 
                (unless predicate
                  (error "Unknown predicate: "))
                ;;; This is an chicken-and-egg problem
                ;;; We possibly need to merge subgraphs into the cd-subgraph caused by connecting
                ;;; attribute constraints. Afterwards we have to recompte the predicate and its attributes 
                (let* ((new-attribute-constraints *new-attribute-constraints*)
                       (old-cd-subgraph (when (or (dl-full-concrete-domains (abox-language abox))
                                                  (not (eql (predicate-arity predicate) 1)))
                                          (abox-concrete-domain-subgraph abox)))
                       (cd-subgraph (when (or (dl-full-concrete-domains (abox-language abox))
                                              (not (eql (predicate-arity predicate) 1)))
                                      (when new-attribute-constraints
                                        (create-concrete-domain-subgraph abox new-attribute-constraints))
                                      (abox-concrete-domain-subgraph abox)))
                       (subgraphs (if cd-subgraph
                                      (list cd-subgraph)
                                    (abox-subgraphs abox)))
                       (*new-attribute-constraints* nil)
                       (transformed-constraint (transform-constraint constraint abox)))
                  (multiple-value-bind (predicate attributes)
                      (ensure-predicate transformed-constraint concrete-domain)
                    (let* ((constraint (make-constraint (predicate-negation predicate) attributes))
                           (new-attribute-constraints *new-attribute-constraints*)
                           (result
                            (progn
                              #+:debug
                              (if cd-subgraph
                                  (assert (or (dl-full-concrete-domains (abox-language abox))
                                              (not (eql (predicate-arity predicate) 1))))
                                (assert (or (null (abox-subgraphs abox))
                                            (not (dl-full-concrete-domains (abox-language abox))))))
                              (if subgraphs
                                  (loop for subgraph in subgraphs ;; ex (abox-subgraphs abox)
                                        thereis
                                        (flet ((constraint-entailed-p-continuation-1 (sat
                                                                                      state
                                                                                      unused-1
                                                                                      unused-2
                                                                                      unused-3)
                                                 (declare (ignore unused-1 unused-2 unused-3))
                                                 (with-race-trace-sublevel ("continuation of constraint-entailed-p"
                                                                            :arguments (list sat state)
                                                                            :trace-result t)
                                                   (when sat
                                                     (let* ((new-cd-state (state-concrete-domain-state state)))
                                                       (push constraint
                                                             (solver-state-cd-constraints new-cd-state))
                                                       (when (nonlinear-predicate-p predicate)
                                                         (setf (solver-state-contains-additional-constraints
                                                                new-cd-state)
                                                               t))
                                                       (race-trace ("~&Adding cd-constraint ~S to CD-state ~S => ~S~%"
                                                                    constraint 
                                                                    (copy-solver-state cd-state)
                                                                    (copy-solver-state new-cd-state)))
                                                       (if (and *use-abox-precompletion*
                                                                (subgraph-precompletion subgraph))
                                                           (if new-attribute-constraints
                                                               (let* ((state-1
                                                                       (if new-attribute-constraints
                                                                           (copy-basic-kernel-state
                                                                            (precompletion-state 
                                                                             (subgraph-precompletion subgraph))
                                                                            t)
                                                                         (subgraph-precompletion subgraph)))
                                                                      (new-attributes
                                                                       (append new-attribute-constraints
                                                                               (state-attribute-constraints
                                                                                state-1)))
                                                                      (state-2
                                                                       (changed-kernel-state 
                                                                        state-1
                                                                        :attribute-constraints
                                                                        new-attributes))
                                                                      (new-precompletion
                                                                       (copy-precompletion 
                                                                        (subgraph-precompletion subgraph))))
                                                                 (setf (precompletion-state new-precompletion)
                                                                       state-2)
                                                                 (test-abox-satisfiable 
                                                                  abox
                                                                  nil
                                                                  nil
                                                                  nil
                                                                  nil
                                                                  new-cd-state
                                                                  new-precompletion
                                                                  nil
                                                                  (state-partially-expanded-or-stack state)))
                                                             (test-abox-satisfiable 
                                                              abox
                                                              nil
                                                              nil
                                                              nil
                                                              nil
                                                              new-cd-state
                                                              (subgraph-precompletion subgraph)
                                                              nil
                                                              (state-partially-expanded-or-stack state)))
                                                         (test-abox-satisfiable 
                                                          abox
                                                          (loop for axiom in (subgraph-encoded-individual-axioms
                                                                              subgraph)
                                                                collect (encode-constraint axiom))
                                                          (subgraph-encoded-role-axioms subgraph)
                                                          (subgraph-individual-names subgraph)
                                                          (append *new-attribute-constraints*
                                                                  (abox-attribute-constraints abox))
                                                          new-cd-state
                                                          nil
                                                          nil
                                                          (state-partially-expanded-or-stack state))))))))
                                          (let* ((new-backtrack-stack
                                                  (push-backtrack-stack #'constraint-entailed-p-continuation-2
                                                                        nil))
                                                 (state (make-basic-kernel-state :concrete-domain-state 
                                                                                 cd-state
                                                                                 :partially-expanded-or-stack
                                                                                 new-backtrack-stack
                                                                                 )))
                                            (not
                                             (constraint-satisfiable-p-2 constraint
                                                                         state
                                                                         t   ; copy-solver-state
                                                                         nil ; new-or-dependencies
                                                                         #'constraint-entailed-p-continuation-1)))))
                                (let ((state (make-basic-kernel-state :concrete-domain-state cd-state)))
                                  (not 
                                   (constraint-satisfiable-p-2 constraint
                                                               state
                                                               t   ; copy-solver-state
                                                               nil ; new-or-dependencies
                                                               #'constraint-entailed-p-continuation-2))))))
                           (time (when *debug*
                                   (/ (- (get-internal-run-time) start) internal-time-units-per-second))))
                      (setf (abox-concrete-domain-subgraph abox) old-cd-subgraph)
                      (when *debug*
                        (format t "~S (~,3F secs)~%" result time))
                      result)))))))))))

(defun transform-constraint (constraint abox)
  (if (null (abox-subgraphs abox)) ; there are only constraints in the Abox
    (cons (first constraint)
          (transform-constraint-1 (rest constraint) nil abox))
    (let* ((subgraph (abox-concrete-domain-subgraph abox))
           (attribute-constraints
            (if subgraph
                (if (and *use-abox-precompletion*
                         (or (subgraph-precompletion subgraph)
                             (subgraph-precompletions-from-merger subgraph)))
                    (if (subgraph-precompletions-from-merger subgraph)
                        (loop for precompletion in (subgraph-precompletions-from-merger subgraph)
                              append (state-attribute-constraints (precompletion-state precompletion)))
                      (when (subgraph-precompletion subgraph)
                        (state-attribute-constraints
                         (precompletion-state (subgraph-precompletion subgraph)))))
                  nil)
              (abox-attribute-constraints abox))))
      (cons (first constraint)
            (transform-constraint-1 (rest constraint)
                                    attribute-constraints                
                                    abox)))))

(defun transform-constraint-1 (expr attribute-constraints abox)
  (cond ((null expr)
         nil)
        ((consp (first expr))
         (cons (let ((expr1 (first expr)))
                 (if (get-tbox-role (tbox abox) (first expr1) nil)
                   (find-object (first expr1) (second expr1) attribute-constraints)
                   (cons (first expr1)
                         (transform-constraint-1 (rest expr1) 
                                                 attribute-constraints
                                                 abox))))
               (transform-constraint-1 (rest expr)
                                       attribute-constraints 
                                       abox)))
        (t (cons (first expr) (transform-constraint-1 (rest expr) 
                                                      attribute-constraints
                                                      abox)))))

(defun find-object (attribute individual attribute-constraints)
  ;; Performance warning: we linearly iterate over all attribute constraints.
  (loop for (individual1 attribute1 object) in attribute-constraints
        when (and (eq individual individual1)
                  (eq attribute attribute1))
        do (return-from find-object object))
  (loop for (individual1 attribute1 object) in *new-attribute-constraints*
        when (and (eq individual individual1)
                  (eq attribute attribute1))
        do (return-from find-object object))
  (let ((object (gensym "O")))
    (push (list individual attribute object) *new-attribute-constraints*)
    object))

(defmacro constraint-entailed? (constraint &optional (abox-name nil abox-name-supplied-p))
  (if (and abox-name-supplied-p abox-name)
    `(constraint-entailed-p ',constraint ',abox-name)
    `(constraint-entailed-p ',constraint *current-abox*)))




(defun test-ind-subsumes-incomplete (ind concept abox)
  (with-race-trace-sublevel ("test-ind-subsumes-incomplete"
                             :arguments (list ind concept abox)
                             :trace-result t)
    (when *debug*
      (format t "~&Testing (test-ind-subsumes-incomplete ~S ~S)..." ind concept))
    
    (let ((contraction-useful-p (null (abox-initial-constraint-state abox)))
	  (atomic-p (atomic-concept-p concept))
          (classified-p (tbox-classified-p-internal (abox-tbox abox)))
          (start (when *debug*
                   (get-internal-run-time))))
      (cond
       ((and atomic-p
             (or (member concept (model-det-positive-literals (get-cached-individual-model ind abox)))
                 (loop for subsumer in (individual-told-subsumers ind)
                       thereis (or (eq concept subsumer)
                                   (and classified-p
                                        (member concept (get-concept-ancestors subsumer)))))))
        ;(princ "+")
        (when *debug*
          (format t "~S~%" t))
        t)
       ((and atomic-p
	     (if classified-p
		 (lists-not-disjoint-p (get-concept-ancestors concept) (individual-told-non-subsumers ind))
	       (member concept (individual-told-non-subsumers ind))))
        ;(princ "-")
        (when *debug*
          (format t "~S~%" nil))
        nil)
       (t
        (multiple-value-bind (not-subsumes-p unknown-p)
                             (if (and *model-merging* contraction-useful-p *abox-model-merging*)
                               (obviously-ind-not-subsumes ind concept abox)
                               (values nil t))
          (let* ((result-1
                  (cond
                   (not-subsumes-p nil)
                   ((not unknown-p) t)
                   (t 'unknown)))
                 (result-2
                  (if (eq result-1 'unknown)
                      (let ((subgraph (individual-subgraph ind))
                            (individual-concept 
                             (when (and (not classified-p)
                                        contraction-useful-p
                                        (null (tbox-meta-constraint-concepts (abox-tbox abox))))
                               (get-individual-concept ind))))
                        (if (and individual-concept
                                 (or (eq concept individual-concept)
                                     (progn
                                       (incf-statistics *ind-concept-subsumption-tests*)
                                       (test-subsumes concept individual-concept))))
                            (progn
                              (when (and (not (eq concept individual-concept)) (atomic-concept-p concept))
                                (add-pending-new-parent ind concept subgraph abox)
                                (incf-statistics *ind-concept-subsumptions-found*))
                              t)
                          (if *use-abox-completion*
                              (let ((completion (subgraph-completion subgraph)))
                                (if completion
                                    (let ((result-3
                                           (test-abox-satisfiable 
                                            abox
                                            (list (with-constraint-ignore-determinism
                                                   (encode-constraint (list (first (individual-name-set ind))
                                                                            `(not ,concept)))))
                                            nil
                                            nil
                                            nil
                                            nil
                                            completion)))
                                      (incf-statistics *ind-abox-completion-tests*)
                                      (if result-3
                                          (progn
                                            (incf-statistics *ind-abox-completion-non-subsumption*)
                                            nil)
                                        (if (non-deterministic-clash-dependencies-p *catching-clash-dependencies*)
                                            'unknown
                                          (progn
                                            (when (atomic-concept-p concept)
                                              (add-pending-new-parent ind concept subgraph abox)
                                              (incf-statistics *ind-concept-subsumptions-found*))
                                            t))))
                                  'unknown))
                            'unknown)))
                    result-1))
                 (time (when *debug*
                         (/ (- (get-internal-run-time) start)
                            internal-time-units-per-second))))
            (when-collect-statistics
              (collect-statistics-data))
            (cond ((not (eq result-1 'unknown))
                   (when *debug*
                     (format t "~S (~,3F secs)~%" result-1 time))
                   result-1)
                  ((not (eq result-2 'unknown))
                   (when (and *model-merging* contraction-useful-p *abox-model-merging*)
                     (if result-2
                       (incf-statistics *ind-unmergable-models-unsatisfiable*)
                       (incf-statistics *ind-unmergable-models-satisfiable*)))
                   (when *debug*
                     (format t "~S (~,3F secs)~%" result-2 time))
                   result-2)
                  (t 
                   (when *debug*
                     (format t "~S (~,3F secs)~%" 'unknown time))
                   'unknown)))))))))

(defun test-ind-subsumes-1 (ind concept abox)
  (let* ((ind-name (first (individual-name-set ind)))
         (subgraph (individual-subgraph ind))
         (result 
          (if (and *use-abox-precompletion*
                   (subgraph-precompletion subgraph))
              (let ((satisfiable
                     (test-abox-satisfiable 
                      abox
                      (list* (encode-constraint (list ind-name `(not ,concept)))
                             (subgraph-pending-new-parents subgraph))
                      nil
                      nil
                      nil
                      nil
                      (subgraph-precompletion subgraph))))
                (when (and (not satisfiable) (atomic-concept-p concept))
                  (add-pending-new-parent ind concept subgraph abox))
                satisfiable)
            (test-abox-satisfiable
             abox
             (list* (encode-constraint (list ind-name `(not ,concept)))
                    (loop for axiom in (subgraph-encoded-individual-axioms
                                        subgraph)
                          collect (encode-constraint axiom)))
             (subgraph-encoded-role-axioms subgraph)
             (subgraph-individual-names subgraph)
             (abox-attribute-constraints abox)
             (copy-solver-state (abox-initial-constraint-state abox))))))
    (incf-statistics *ind-subsumption-tests*)
    (if result
        (incf-statistics *ind-unmergable-models-satisfiable*)
      (progn
        (incf-statistics *ind-unmergable-models-unsatisfiable*)
        (incf-statistics *ind-subsumptions-found*)))
    (not result)))


(defun instance-test-not-sucessful (inds concept abox &optional analyze-dependencies-p)
  (when *debug*
    (format t "~&Trying (instance-test-not-sucessful ~S ~S)..." inds concept))
  
  ;; All inds are part of the same subgraph, therefore considering one suffices.
  (let* ((ind (first inds))
         (start (when *debug*
                  (get-internal-run-time)))
         (*clash-reasons* nil)
         (*clash-culprits* nil)
         (subgraph (individual-subgraph ind))
         (new-constraints (loop for ind1 in inds
                                collect
                                (encode-constraint (list (first (individual-name-set ind1))
                                                         `(not ,concept)))))
         (*interesting-clash-candidates* new-constraints)
         #+:debug
         (*clash-culprits-required-p* (and analyze-dependencies-p nil)) ;;only for debugging!!!
         (result (if (and *use-abox-precompletion*
                          (subgraph-precompletion subgraph))
                   (test-abox-satisfiable 
                    abox
                    (append new-constraints (subgraph-pending-new-parents subgraph))
                    nil
                    nil
                    nil
                    nil
                    (subgraph-precompletion subgraph))
                   (test-abox-satisfiable 
                    abox
                    (append new-constraints
                            (loop for axiom in (subgraph-encoded-individual-axioms
                                                subgraph)
                                  collect (encode-constraint axiom)))
                    (subgraph-encoded-role-axioms subgraph)
                    (subgraph-individual-names subgraph)
                    (abox-attribute-constraints abox)
                    (copy-solver-state (abox-initial-constraint-state abox)))))
         (time (when *debug*
                 (/ (- (get-internal-run-time) start)
                    internal-time-units-per-second))))
    (when *debug*
      (format t "~S (~,3F secs)~%" result time))
    (incf-statistics *ind-subsumption-tests*)
    (if result
      (incf-statistics *ind-unmergable-models-satisfiable*)
      (progn
        (incf-statistics *ind-subsumptions-found*)
        (incf-statistics *ind-unmergable-models-unsatisfiable*)))
    (if analyze-dependencies-p
      (if (null result)
        (let ((culprits (remove-duplicates *clash-culprits*)))
          #+:debug
          (unless culprits
            (cerror "Ignore missing clash culprits"
                    "No clash culprits found for inds ~S, concept ~S, ABox ~S"
                    inds concept abox))
          (values result culprits))
        (values result nil))
      result)))

(defun top-search-concept-subsumes-ind-p (ind
                                                concept
                                                subsumer-mark
                                                non-subsumer-mark
                                                abox)
  (cond ((eql (concept-mark1 concept) non-subsumer-mark)
         nil)
        ((eql (concept-mark1 concept) subsumer-mark)
         t)
        ((loop for concept-1 in (concept-parents-internal concept) 
               always (or (not (concept-visible-p concept-1))
                          (top-search-concept-subsumes-ind-p ind
                                                             concept-1
                                                             subsumer-mark
                                                             non-subsumer-mark
                                                             abox)))
         (cond ((test-ind-subsumes ind concept abox)
                (setf (concept-mark1 concept) subsumer-mark)
                (let ((new-disjoints (concept-told-disjoints concept)))
                  (when new-disjoints
                    (setf (individual-told-non-subsumers ind)
                          (concept-set-union (individual-told-non-subsumers ind) new-disjoints)))
                  (loop for non-subsumer in new-disjoints do
                        (mark-all-descendants non-subsumer non-subsumer-mark)))
                t)
               (t
                (setf (concept-mark1 concept) non-subsumer-mark)
                nil)))))

(defun abox-top-search (ind
                           current-concept
                           abox
                           subsumer-mark
                           &optional 
                           (non-subsumer-mark (incf *tbox-classification-counter*))
                           (visited-mark (incf *tbox-classification-counter*)))
  (setf (concept-visited-mark current-concept) visited-mark)
  (let ((positive-successors 
         (loop for concept in (prefer-subsumers-from-model
                               ind
                               (filter-visibles current-concept #'concept-children-internal
                                                 #'identity)
                               abox)
               when (and (concept-visible-p concept)
                         (top-search-concept-subsumes-ind-p ind
                                                            concept
                                                            subsumer-mark 
                                                            non-subsumer-mark
                                                            abox))
               collect concept)))
    (if (null positive-successors)
      (list current-concept)
      (loop for concept in (prefer-subsumers-from-model ind positive-successors abox)
            unless (or (not (concept-visible-p concept))
                       (eql (concept-visited-mark concept) visited-mark))
            append (abox-top-search ind
                                    concept
                                    abox
                                    subsumer-mark
                                    non-subsumer-mark
                                    visited-mark)))))

(defun prefer-subsumers-from-model (ind concept-list abox)
  (loop for model-subsumer in (model-det-positive-literals
                               (get-cached-individual-model ind abox))
        when (member model-subsumer concept-list)
        collect model-subsumer into preferred-concepts
        finally
        (return (nconc preferred-concepts
                       (concept-set-difference concept-list preferred-concepts)))))

;;; ======================================================================


(defun compute-index-for-instance-retrieval (&optional (abox *current-abox*))
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (realize-abox abox))

(race-inline (collect-individual-names))

(defun collect-individual-names (individual-list)
  (loop for ind in individual-list
        append (individual-name-set ind)))

(defun retrieve-concept-instances (concept-term 
                                   abox
                                   &optional (candidates nil candidates-supplied-p))
  (setf abox (find-abox abox))
  (check-type abox abox)
  (with-race-trace-sublevel ("retrieve-concept-instances"
                             :arguments (list concept-term abox candidates candidates-supplied-p)
                             :trace-result t)
    (if (or (eq concept-term +krss-top-symbol+) (eq concept-term +top-symbol+))
        (all-individuals abox)
      (if (and (symbolp concept-term) (abox-el+-transformed-table abox))
          (progn
            (ensure-knowledge-base-state ':abox-realized abox)
            (collect-individual-names (el+-concept-instances concept-term abox)))
        (progn
          (unless (ensure-abox-is-coherent abox)
            (error "ABox ~A is incoherent." (abox-name abox)))
          (setf (abox-last-tbox-changed-mark abox) nil)
          (cond ((or *use-realization-based-instance-retrieval*
                     (abox-realized-p-internal abox))
                 (when (abox-el+-transformed-table abox)
                   (ensure-knowledge-base-state ':tbox-classified (abox-tbox abox)))
                 (ensure-knowledge-base-state ':abox-realized abox)
                 (if candidates-supplied-p
                     (collect-individual-names 
                      (realization-based-instance-retrieval concept-term abox 
                                                            (find-candidates candidates abox)))
                   (collect-individual-names 
                    (realization-based-instance-retrieval concept-term abox))))
                ((or *use-subsumption-based-instance-retrieval*
                     ;;(tbox-classified-p (abox-tbox abox)) 
                     ;; We do not activate this any longer if the Tbox is classified.
                     ;; The facility must be explicitly activated.
                     )
                 (ensure-knowledge-base-state ':tbox-classified (abox-tbox abox))
                 (ensure-knowledge-base-state ':abox-prepared abox)
                 (if candidates-supplied-p
                     (collect-individual-names 
                      (subsumption-based-instance-retrieval concept-term abox 
                                                            (find-candidates candidates abox)))
                   (collect-individual-names 
                    (subsumption-based-instance-retrieval concept-term abox))))
                (t
                 (ensure-knowledge-base-state ':abox-prepared abox)
                 (cond (*use-dependency-based-instance-retrieval*
                        (collect-individual-names 
                         (if candidates-supplied-p
                             (dependency-based-instance-retrieval concept-term abox 
                                                                  (find-candidates candidates abox))
                           (dependency-based-instance-retrieval concept-term abox))))
                       (*use-binary-instance-retrieval*
                        (collect-individual-names
                         (if candidates-supplied-p
                             (binary-instance-retrieval concept-term abox 
                                                        (find-candidates candidates abox))
                           (binary-instance-retrieval concept-term abox))))
                       (t (collect-individual-names
                           (if candidates-supplied-p
                               (linear-instance-retrieval concept-term abox 
                                                          (find-candidates candidates abox))
                             (linear-instance-retrieval concept-term abox))))))))))))

(defun find-candidates (candidates abox)
  (loop for candidate in candidates 
        collect (find-individual abox candidate)))

(defmacro concept-instances (concept-term &optional (abox nil abox-supplied-p)
                                          (candidates nil candicates-supplied-p))
  (check-type abox (or symbol null))
  (if candicates-supplied-p
    `(retrieve-concept-instances ',concept-term ',abox ',candidates)
    (if abox-supplied-p
      `(retrieve-concept-instances ',concept-term ',abox)
      `(with-abox-defined-check *current-abox*
         (retrieve-concept-instances ',concept-term *current-abox*)))))

(defun transform-into-individual-synsets (individuals abox)
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (let ((individual-ht (make-hash-table))
        (result nil))
    (loop for individual in individuals
          unless (gethash individual individual-ht)
          do
          (let ((individual-synonyms (retrieve-individual-synonyms individual nil abox)))
          (loop for ind in  individual-synonyms
                do
                (setf (gethash ind individual-ht) t))
          (push individual-synonyms result)))
    result))

(defun linear-instance-retrieval (concept-term 
                                       abox 
                                       &optional
                                       (candidates nil candidates-supplied))
  (unless (ensure-abox-is-coherent abox)
    (error "ABox ~A is incoherent." (abox-name abox)))
  (with-abox-defined-check abox
    (let ((tbox (abox-tbox abox))
          (result nil))
      (with-abox-settings abox
        (with-concept-definition-mapping tbox
          (let ((concept (encode-update-concept-term tbox concept-term)))
            (loop for ind in (if candidates-supplied
                               candidates
                               (abox-individuals-list abox)) do
                  (when (test-ind-subsumes ind concept abox)
                    (push ind result))))))
      result)))

(defun binary-instance-retrieval (concept-term 
                                       abox
                                       &optional
                                       (candidates nil candidates-supplied))
  (unless (ensure-abox-is-coherent abox)
    (error "ABox ~A is incoherent." (abox-name abox)))
  (with-abox-defined-check abox
    (let ((tbox (abox-tbox abox)))
      (with-abox-settings abox
        (with-concept-definition-mapping tbox
          (let ((concept (encode-update-concept-term tbox concept-term)))
            (if candidates-supplied
              (binary-instance-retrieval-with-candidates concept abox candidates)
              (loop for subgraph in (abox-subgraphs abox) 
                    for instances =
                    (let ((inds-to-be-tested nil)
                          (result nil))
                      (loop for ind in (subgraph-individuals subgraph) 
                            for pre-result = (test-ind-subsumes-incomplete ind concept abox)
                            do
                            (when pre-result
                              (if (eq pre-result 'unknown)
                                (push ind inds-to-be-tested)
                                (push ind result))))
                      (incf-statistics *number-obv-inds-in-binary-retrieval* (length result))
                      (incf-statistics *number-of-inds-to-be-tested-in-binary-retrieval*
                                       (length inds-to-be-tested))
                      (incf-statistics *number-of-binary-retrievals*)
                      (nconc result
                             (binary-partition-based-instance-retrieval inds-to-be-tested
                                                                        concept
                                                                        abox)))
                    when instances
                    nconc instances))))))))

(defun group-candidates (candidates)
  (if (rest candidates)
    (let ((table (racer-make-hash-table)))
      (loop for ind in candidates do
            (push ind (gethash (individual-subgraph ind) table)))
      (loop for set being the hash-value of table
            collect set))
    (list candidates)))

(defun binary-instance-retrieval-with-candidates (concept abox candidates)
  (incf-statistics *number-of-binary-retrieval-candidates* (length candidates))
  (loop for candidates in (group-candidates candidates)
        for instances =
        (loop for ind in candidates
              for pre-result = (test-ind-subsumes-incomplete ind concept abox)
              when pre-result
              if (eq pre-result 'unknown)
              collect ind into inds-to-be-tested
              else collect ind into result
              finally
              (incf-statistics *number-obv-inds-in-binary-retrieval* (length result))
              (incf-statistics *number-of-inds-to-be-tested-in-binary-retrieval*
                               (length inds-to-be-tested))
              (incf-statistics *number-of-binary-retrievals*)
              (if inds-to-be-tested
                (return
                 (nconc result
                        (binary-partition-based-instance-retrieval inds-to-be-tested
                                                                   concept
                                                                   abox)))
                (return result)))
        when instances
        nconc instances))

(defun binary-partition-based-instance-retrieval (candidates concept abox)
  (when candidates
    (multiple-value-bind (set1 set2)
                         (binary-partition candidates)
      (incf-statistics *number-of-binary-partition-retrievals*)
      (cond ((and set1 (null (rest set1)))
             (if (test-ind-subsumes (first set1) concept abox)
               (if (null set2)
                 (list (first set1))
                 (cons (first set1)
                       (binary-partition-based-instance-retrieval set2 concept abox)))
               (if (null set2)
                 nil
                 (binary-partition-based-instance-retrieval set2 concept abox))))
            ((null (rest set1))
             (if (test-ind-subsumes (first set2) concept abox)
               (if (null set1)
                 (list (first set2))
                 (cons (first set2)
                       (binary-partition-based-instance-retrieval set1 concept abox)))
               (if (null set1)
                 nil
                 (binary-partition-based-instance-retrieval set1 concept abox))))
            ((and (cddr set1)
                  (instance-test-not-sucessful set1 concept abox))
             (binary-partition-based-instance-retrieval set2 concept abox))
            ((and (cddr set2)
                  (instance-test-not-sucessful set2 concept abox))
             (binary-partition-based-instance-retrieval set1 concept abox))
            (t (append (binary-partition-based-instance-retrieval set1 concept abox)
                       (binary-partition-based-instance-retrieval set2 concept abox)))))))

(defun binary-partition (set)
  (binary-partition-1 set (floor (length set) 2) nil nil nil))

(defun binary-partition-1 (set n acc unused-1 unused-2)
  (declare (ignore unused-1 unused-2))
  (if (= n 0)
    (values (nreverse acc) set)
    (binary-partition-1 (rest set) (- n 1) (cons (first set) acc) nil nil)))
    

(defun dependency-based-instance-retrieval (concept-term 
                                                  abox
                                                  &optional
                                                  (candidates nil candidates-supplied))
  (unless (ensure-abox-is-coherent abox)
    (error "ABox ~A is incoherent." (abox-name abox)))
  (with-abox-defined-check abox
    (let ((tbox (abox-tbox abox)))
      (with-abox-settings abox
        (with-concept-definition-mapping tbox
          (let ((concept (encode-update-concept-term tbox concept-term)))
            (if candidates-supplied
              (dependency-based-instance-retrieval-with-candidates concept abox candidates)
              (loop for subgraph in (abox-subgraphs abox)
                    for instances =
                    (let ((inds-to-be-tested nil)
                          (result nil))
                      (loop for ind in (subgraph-individuals subgraph)
                            for pre-result = (test-ind-subsumes-incomplete ind concept abox)
                            do
                            (when pre-result
                              (if (eq pre-result 'unknown)
                                (push ind inds-to-be-tested)
                                (push ind result))))
                      (let ((true-inds-to-be-tested ;; take care of synonyms
                             (loop for old-ind in inds-to-be-tested
                                   collect (find-individual abox
                                                            (first
                                                             (individual-name-set
                                                              old-ind))))))
                        (incf-statistics *number-obv-inds-in-dependency-retrieval*
                                         (length result))
                        (incf-statistics *number-of-inds-to-be-tested-in-dependency-retrieval*
                                         (length inds-to-be-tested))
                        (incf-statistics *number-of-dependency-retrievals*)
                        (if true-inds-to-be-tested
                          (nconc result
                                 (dependency-partition-based-instance-retrieval true-inds-to-be-tested
                                                                                concept
                                                                                abox))
                          result)))
                    when instances
                    nconc instances))))))))

(defun dependency-based-instance-retrieval-with-candidates (concept abox candidates)
  (incf-statistics *number-of-dependency-retrieval-candidates* (length candidates))
  (loop for candidates in (group-candidates candidates)
        for instances =
        (loop for ind in candidates
              for pre-result = (test-ind-subsumes-incomplete ind concept abox)
              when pre-result
              if (eq pre-result 'unknown)
              collect ind into inds-to-be-tested
              else collect ind into result
              finally
              (incf-statistics *number-obv-inds-in-dependency-retrieval* (length result))
              (incf-statistics *number-of-inds-to-be-tested-in-dependency-retrieval*
                               (length inds-to-be-tested))
              (incf-statistics *number-of-dependency-retrievals*)
              (let ((true-inds-to-be-tested ;; take care of synonyms
                     (loop for old-ind in inds-to-be-tested
                           collect (find-individual abox (first (individual-name-set old-ind))))))
                (if true-inds-to-be-tested
                  (return
                   (nconc result
                          (dependency-partition-based-instance-retrieval true-inds-to-be-tested
                                                                         concept
                                                                         abox)))
                  (return result))))
        when instances
        nconc instances))

(defun dependency-partition-based-instance-retrieval (candidates concept abox)
  (when candidates
    (incf-statistics *number-of-dependency-partition-retrievals*)
    (if (null (rest candidates))
      (when (test-ind-subsumes (first candidates) concept abox)
        (list (first candidates)))
      (multiple-value-bind (result culprits)
                           (instance-test-not-sucessful candidates concept abox t)
        (cond ((null result)
               (let ((culprit-inds 
                      (remove-duplicates 
                       (mapcar #'(lambda (constraint)
                                   (find-individual 
                                    abox (constraint-ind constraint)))
                               culprits))))
                 (if (null culprit-inds)
                     (progn
                       (racer-warn "Possible performance problem: temporarily switching off dependency-based partition instance retrieval")
                       (binary-partition-based-instance-retrieval candidates concept abox))
                   (if (null (rest culprit-inds))
                       (cons (first culprit-inds)
                             (dependency-partition-based-instance-retrieval 
                              (remove (first culprit-inds) candidates)
                              concept
                              abox))
                     (multiple-value-bind (set1 set2)
                         (binary-partition culprit-inds)
                       (append (dependency-partition-based-instance-retrieval 
                                (individual-set-difference candidates culprit-inds)
                                concept
                                abox)
                               (dependency-partition-based-instance-retrieval 
                                set1
                                concept
                                abox)
                               (dependency-partition-based-instance-retrieval 
                                set2
                                concept
                                abox)))))))
              (t nil))))))


(defun realization-based-instance-retrieval (concept-term 
                                                   abox 
                                                   &optional (initial-candidates nil candicates-supplied-p))

  (with-abox-defined-check abox
    (let ((tbox (abox-tbox abox)))
      (with-abox-settings abox
        (with-concept-definition-mapping tbox
          (let* ((*n-inserted-concepts* 0)
                 (encoded-concept (encode-update-concept-term tbox concept-term))
                 (name (if (atomic-concept-p encoded-concept)
                         nil
                         (gensym "ANONYMOUS"))))
            (create-tbox-internal-marker-concept tbox name)
            (let ((concept (if (null name)
                             encoded-concept
                             (initialize-atomic-concept tbox 
                                                        name
                                                        concept-term 
                                                        nil))))
              (if (atomic-concept-p encoded-concept)
                (collect-individuals-downward (list concept) abox)
                (progn
                  (encode-anonymous-concept tbox concept concept-term)
                  (insert-anonymous-concept-into-tbox concept tbox t nil)                  
                  ;; The anonymous concept might be a synonym for another concept.
                  ;; Therefore, it is necessary to get the correct node object!!
                  (setf concept (get-tbox-concept tbox name))
                  (if (not (find name (concept-name-set concept))) ; synonym to named concept in tbox?
                    (collect-individuals-downward (list concept) abox) ; result already known!
                    (let* ((known-results (collect-individuals-downward
                                           (concept-children-internal concept) abox))
                           (candidates 
                            (individual-set-difference
                             (if candicates-supplied-p
                               (individual-set-intersection
                                initial-candidates
                                (collect-individuals-downward
                                 (concept-parents-internal concept) abox))
                               (collect-individuals-downward
                                (concept-parents-internal concept) abox))
                             known-results)))
                      (append known-results
                              (if *use-binary-instance-retrieval*
                                (binary-instance-retrieval concept-term (find-abox abox)
                                                           candidates)
                                (linear-instance-retrieval concept-term (find-abox abox)
                                                           candidates))))))))))))))

;;; ======================================================================

(defun add-associated-individuals (abox concept ind-or-inds &optional (complete-p nil))
  (let* ((index-table (abox-concept-individuals abox))
         (entry (gethash concept index-table)))
    (when (null entry)
      (setf entry (setf (gethash concept index-table) (racer-make-hash-table))))
    (when complete-p
      (setf (gethash entry (abox-complete-index-entries abox)) t))
    (loop for ind in (if (or (null ind-or-inds) (consp ind-or-inds))
                       ind-or-inds 
                       (list ind-or-inds))
          for ind-entry = (gethash ind entry)
          do
          (unless ind-entry
            (remove-from-parent-concepts concept ind abox)
            (setf (gethash ind entry) ind)))))

#|
(defun get-associated-individuals (abox concept)
  (let ((entry (gethash concept (abox-concept-individuals abox))))
    (if (null entry)
      nil
      (loop for ind being the hash-values of entry collect ind))))
|#

(defun add-associated-non-individuals (abox concept ind-or-inds)
  (let* ((index-table (abox-concept-non-individuals abox))
         (entry (gethash concept index-table)))
    (loop for ind in (if (or (null ind-or-inds) (consp ind-or-inds))
                         ind-or-inds 
                         (list ind-or-inds))
          do 
          (let ((enter-here t))
            (traverse-index (list concept)
                            #'concept-parents-internal
                            #'(lambda (concept)
                                (let ((index (gethash concept index-table)))
                                  (when index
                                    (when (gethash ind index)
                                      (setf enter-here nil))))))
            (when enter-here
              (when (null entry)
                (setf entry (setf (gethash concept index-table) (racer-make-hash-table))))
              (setf (gethash ind entry) ind))))))

#|
(defun get-associated-non-individuals (abox concept)
  (let ((entry (gethash concept (abox-concept-non-individuals abox))))
    (if (null entry)
      nil
      (loop for ind being the hash-values of entry collect ind))))
|#

(defun traverse-index (concepts next-function concept-function 
                                   &optional (visited-concepts (racer-make-hash-table)))
  (let ((concepts-to-traverse nil))
    (loop for concept in concepts do
          (unless (gethash concept visited-concepts)
            (funcall concept-function concept)
            (push concept concepts-to-traverse)
            (setf (gethash concept visited-concepts) t)))
    (loop for concept in concepts-to-traverse do
          (traverse-index (funcall next-function concept)
                          next-function
                          concept-function
                          visited-concepts))))

(defun remove-from-parent-concepts (concept ind abox)
  (let ((index-table (abox-concept-individuals abox)))
    (traverse-index (concept-parents-internal concept)
                    #'concept-parents-internal
                    #'(lambda (concept-1)
                        (let ((index (gethash concept-1 index-table)))
                          (when index
                            (remhash ind index)))))))

(defun collect-individuals-downward (concepts abox)
  (let ((result nil)
        (index-table (abox-concept-individuals abox)))
    (traverse-index concepts #'concept-children-internal
                    #'(lambda (concept)
                        (let ((index (gethash concept index-table)))
                          (when index
                            (loop for ind being the hash-values of index
                                  do
                                  (push ind result))))))
    ;; If an individuals has several direct types, it will appear many times in the result list.
    ;; We postpone duplicate removal, but now it is time to do it.
    (sort-individuals (individual-set-remove-duplicates result))))

(defun most-specific-individual-p (ind concept)
  (and (individual-realized-p ind)
       (find concept (individual-parent-concepts ind))))

(defun collect-most-specific-individuals-upward (concepts abox)
  (let ((result nil)
        (index-table (abox-concept-individuals abox)))
    (traverse-index concepts #'concept-parents-internal
                    #'(lambda (concept)
                        (let ((index (gethash concept index-table)))
                          (when index
                            (loop for ind being the hash-values of index
                                  when (most-specific-individual-p ind concept)
                                  do (push ind result))))))
    (sort-individuals (individual-set-remove-duplicates result))))

(defun collect-non-individuals-upward (concepts abox)
  (let ((result nil)
        (index-table (abox-concept-non-individuals abox)))
    (traverse-index concepts #'concept-parents-internal
                    #'(lambda (concept)
                        (let ((index (gethash concept index-table)))
                          (when index
                            (loop for ind being the hash-values of index
                                  do (push ind result))))))
    (sort-individuals (individual-set-remove-duplicates result))))

(defun collect-non-individuals-from-parents (concept abox)
  (declare (ignore concept abox))
  nil)

#|
VH 01.04.2008
the following code is buggy. Use of complete-p makes no sense.
this is the counter example:
(let ((*use-subsumption-based-instance-retrieval* t))
  (in-knowledge-base test)
  (instance i (some r1 (and c1 c2)))
  (test-assert (equal (concept-instances (some r1 c2)) '(i)))
  (test-assert (equal (concept-instances (some r1 c1)) '(i)))
  (test-assert (equal (concept-instances (some r1 (and c1 c2))) '(i)))
  ; result will be nil but should be '(i)
  )

(defun collect-non-individuals-from-parents (concept abox)
  (let ((traverse-result nil)
        (result-sets nil)
        (index-table (abox-concept-individuals abox))
        (complete-index-entries (abox-complete-index-entries abox)))
    (loop for concept-1 in (concept-parents-internal concept) do
          (let* ((index (gethash concept-1 index-table))
                 (complete-p (gethash index complete-index-entries)))
            (when complete-p
              (let ((result-set nil))
                (traverse-index (list concept-1)
                                #'concept-children-internal
                                #'(lambda (concept-2)
                                    (let ((index (gethash concept-2 index-table)))
                                      (when index
                                        (loop for ind being the hash-values of index do 
                                              (push ind result-set)
                                              (push ind traverse-result))))))
                (push result-set result-sets)))))
    (if result-sets
      (sort-individuals (individual-set-difference
                         traverse-result
                         (reduce #'individual-set-intersection-f result-sets)))
      nil)))
|#

(defun collect-individuals-from-disjoint-concepts (concept abox)
  (let ((result nil)
        (index-table (abox-concept-individuals abox)))
    (traverse-index (concept-told-disjoints concept)
                    #'concept-children-internal
                    #'(lambda (concept-1)
                        (let ((index (gethash concept-1 index-table)))
                          (when index
                            (loop for ind being the hash-values of index do 
                                  (push ind result))))))
    (sort-individuals (individual-set-remove-duplicates result))))


(defun subsumption-based-instance-retrieval (concept-term 
                                                   abox 
                                                   &optional (initial-candidates nil candicates-supplied-p))
  (with-abox-defined-check abox
    (let ((tbox (abox-tbox abox)))
      (with-abox-settings abox
        (with-concept-definition-mapping tbox
          (let* ((*n-inserted-concepts* 0)
                 (encoded-concept (encode-update-concept-term tbox concept-term))
                 (name (if (atomic-concept-p encoded-concept)
                         nil
                         (gensym "QUERY"))))
            (create-tbox-internal-marker-concept tbox name)
            (let ((concept (if (null name)
                             encoded-concept
                             (initialize-atomic-concept tbox 
                                                        name
                                                        concept-term 
                                                        nil))))
              (unless (null name) 
                (encode-anonymous-concept tbox concept concept-term)
                (insert-anonymous-concept-into-tbox concept tbox t t)                  
                ;; The anonymous concept might be a synonym for another concept.
                ;; Therefore, it is necessary to get the correct node object!!
                (setf concept (get-tbox-concept tbox name)))
              (let* ((known-individuals (collect-individuals-downward (list concept) abox))
                     (known-non-candidates (append 
                                            (collect-most-specific-individuals-upward 
                                             (mapcan #'(lambda (concept) 
                                                         (copy-list (concept-parents-internal concept)))
                                                     (concept-parents-internal concept)) abox)
                                            (collect-non-individuals-from-parents concept abox)
                                            (collect-non-individuals-upward
                                             (list concept) abox)
                                            (collect-individuals-from-disjoint-concepts
                                             concept abox)))
                     (candidates (individual-set-difference (if candicates-supplied-p
                                                              initial-candidates
                                                              (abox-individuals-list abox))
                                                            (append known-individuals
                                                                    known-non-candidates)))
                     (additional-results 
                      (cond (*use-dependency-based-instance-retrieval*
                             (dependency-based-instance-retrieval concept-term abox
                                                                  candidates))
                            (*use-binary-instance-retrieval*
                             (binary-instance-retrieval concept-term (find-abox abox)
                                                        candidates))
                            (t (linear-instance-retrieval concept-term (find-abox abox)
                                                          candidates)))))
                (add-associated-individuals abox concept additional-results t)
                (add-associated-non-individuals abox concept 
                                                (individual-set-difference
                                                 candidates additional-results))
                (append known-individuals additional-results)))))))))



;;; ======================================================================

(defmacro individuals-equal? (individual-1 
                            individual-2
                            &optional (abox nil abox-supplied-p))
  (declare (ignore abox-supplied-p))
  (check-type abox (or symbol null))
  (check-type individual-1 symbol)
  (check-type individual-2 symbol)
  `(eql ',individual-1 ',individual-2))

(defmacro individuals-not-equal? (individual-1 
                                individual-2
                                &optional (abox nil abox-supplied-p))
  (declare (ignore abox-supplied-p))
  (check-type abox (or symbol null))
  (check-type individual-1 symbol)
  (check-type individual-2 symbol)
  `(not (eql ',individual-1 ',individual-2)))

(defun individuals-equal-p (individual-1 
                            individual-2
                            &optional (abox nil abox-supplied-p))
  (declare (ignore abox-supplied-p))
  (check-type abox (or symbol null))
  (check-type individual-1 symbol)
  (check-type individual-2 symbol)
  (eql individual-1 individual-2))

(defun individuals-not-equal-p (individual-1 
                                individual-2
                                &optional (abox nil abox-supplied-p))
  (declare (ignore abox-supplied-p))
  (check-type abox (or symbol null))
  (check-type individual-1 symbol)
  (check-type individual-2 symbol)
  (not (eql individual-1 individual-2)))

;;; Macro for concept membership test
(defmacro individual-instance? (individual
                               concept
                               &optional (abox nil abox-supplied-p))
  (check-type individual symbol)
  (check-type abox (or symbol null))
  (if abox-supplied-p
    `(individual-instance-p ',individual ',concept ',abox)
    `(with-abox-defined-check *current-abox*
       (individual-instance-p ',individual ',concept *current-abox*))))

(defun individual-instance-p (individual-name concept abox)
  (setf abox (find-abox abox))
  (check-type individual-name symbol)
  (check-type abox abox)
  (with-race-trace-sublevel ("individual-instance-p"
                             :arguments (list individual-name concept abox)
                             :trace-result t)
    (ensure-knowledge-base-state ':abox-prepared (find-abox abox))
    (if (and (symbolp concept)
             (get-tbox-concept (abox-tbox abox) concept nil) ; make sure the concept already exists in the tbox
             (or (member *auto-realize* '(:eager :eager-verbose))
                 (individual-realized-p (find-individual abox individual-name))))
      (member-of-named-concept-p abox individual-name concept)
      (member-of-concept-p abox individual-name concept))))

(defun most-specific-instantiators (individual-name abox)
  (setf abox (find-abox abox))
  (check-type individual-name symbol)
  (check-type abox abox)
  (with-race-trace-sublevel ("most-specific-instantiators"
                             :arguments (list individual-name abox)
                             :trace-result t)
    (ensure-knowledge-base-state `(:abox-realized ,individual-name)
                                 (find-abox abox))
    (unless (ensure-abox-is-coherent abox)
      (error "ABox ~A is incoherent." (abox-name abox)))
    (mapcar #'concept-name-set
            (individual-parent-concepts (find-individual abox individual-name)))))

(defmacro individual-direct-types (individual-name 
                                  &optional (abox nil abox-supplied-p))
  (check-type abox (or symbol null))
  (if abox-supplied-p
    `(most-specific-instantiators ',individual-name ',abox)
    `(with-abox-defined-check *current-abox*
       (most-specific-instantiators ',individual-name *current-abox*))))

(defun instantiators (individual-name abox)
  (setf abox (find-abox abox))
  (check-type individual-name symbol)
  (check-type abox abox)
  (with-race-trace-sublevel ("instantiators"
                             :arguments (list individual-name abox)
                             :trace-result t)
    (ensure-knowledge-base-state `(:abox-realized ,individual-name)
                                 (find-abox abox))
    (unless (ensure-abox-is-coherent abox)
      (error "ABox ~A is incoherent." (abox-name abox)))
    (mapcar #'concept-name-set
            (individual-ancestor-concepts (find-individual abox individual-name)))))

(defmacro individual-types (individual-name &optional (abox nil abox-supplied-p))
  (check-type abox (or symbol null))
  (if abox-supplied-p
    `(instantiators ',individual-name ',abox)
    `(with-abox-defined-check *current-abox*
       (instantiators ',individual-name *current-abox*))))


;;; ======================================================================

(defmacro individual-attribute-fillers (ind
                                     attribute 
                                     &optional (abox nil abox-supplied-p))
  (check-type abox (or symbol null))
  (if abox-supplied-p
    `(retrieve-individual-attribute-fillers ',ind ',attribute ',abox)
    `(with-abox-defined-check *current-abox*
       (retrieve-individual-attribute-fillers ',ind
                                          ',attribute
                                          *current-abox*))))

(defun retrieve-individual-attribute-fillers (ind attribute abox)
  (check-type ind symbol)
  (check-type attribute symbol)
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (loop for ((ind-1 object) attribute-1) in (abox-attribute-assertions abox) 
        when (and (eq ind-1 ind) (eq attribute-1 attribute))
        collect object))
  




(defun told-value (object &optional (abox *current-abox*))
  (check-type object symbol)
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (loop for constraint in (abox-constraints abox) do
        (cond ((or (eq (first constraint) '=)
                   (eq (first constraint) 'equal)
                   (eq (first constraint) 'string=))
               (cond ((eq (second constraint) object)
                      (return (third constraint)))
                     ((eq (third constraint) object)
                      (return (second constraint)))))
              ((or (eq (first constraint) '>=)
                   (eq (first constraint) 'min))
               (if (eq (second constraint) object)
                 (let ((value (third constraint)))
                   (if (loop for constraint in (abox-constraints abox) do
                             (cond ((or (eq (first constraint) '<=)
                                        (eq (first constraint) 'max))
                                    (if (eq (second constraint) object)
                                      (if (eq (third constraint) value)
                                        (return t))))))
                     (return value)))))
              ((eq (first constraint) '=constant)
               (when (eq object (third constraint))
                 (let* ((attribute (second constraint))
                        (value (fourth constraint))
                        (domain-of-value (second (gethash attribute (tbox-role-axioms-index (tbox abox))))))
                   (return
                    (case domain-of-value
                      (integer (read-from-string value))
                      ((cardinal real complex) (read-from-string value))
                      (string (transform-predicate-1 value))
                      (otherwise (error "No range restriction for attribute ~A declared. Thus the datatype of
the constant ~A cannot be determined."
                                        attribute value))))))))))



(defmacro individual-told-datatype-fillers (ind
                                         datatype-role
                                         &optional 
                                         (abox nil abox-supplied-p))
  (check-type abox (or symbol null))
  (if abox-supplied-p
    `(retrieve-individual-told-datatype-fillers ',ind ',datatype-role nil ',abox)
    `(with-abox-defined-check *current-abox*
       (retrieve-individual-told-datatype-fillers ',ind
                                                  ',datatype-role
                                                  nil
                                                  *current-abox*))))

(defun retrieve-individual-told-datatype-fillers (ind
						  datatype-role 
						  &optional (direct-p nil) 
							    (abox *current-abox*)
							    with-types-p)
  (check-type ind symbol)
  (check-type datatype-role symbol)
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (ensure-knowledge-base-state ':abox-prepared abox)
  
  ;;; changed by MW: should be possible even with incoherent ABox!
  ;; e.g., for ABox Graph told-only = T display in RacerPorter
  #+:ignore
  (unless (ensure-abox-is-coherent abox)
    (error "ABox ~A is incoherent." (abox-name abox)))
  
  (if (role-used-as-annotation-property-p datatype-role (tbox abox))
    (retrieve-individual-annotation-property-fillers ind datatype-role abox with-types-p)
    (let* ((ind1 (find-individual abox ind))
           (role (get-tbox-role (tbox abox) datatype-role))
           (fillers
            (loop with term 
                  for (nil encoded-concept) in (individual-encoded-axioms ind1)
                  when (and (some-concept-p encoded-concept)
                            (eq role (concept-role encoded-concept))
                            (cd-concept-p (concept-term encoded-concept))
                            (equality-constraint-p 
                             (setf term (predicate-definition (concept-predicate (concept-term encoded-concept))))))
                  collect 
                  (if with-types-p 
                      (third (decode-concept encoded-concept))
                    (let ((value (third (predicate-definition (concept-predicate (concept-term encoded-concept))))))
                      (if (eq (second term) +has-boolean-value+)
                          (if (string= value "true")
                              *true*
                            *false*)
                        value))))))
      (unless direct-p       
        (loop for parent in
              ;;; (role-parents-internal role)  
              ;;; ?? MW 4.11.2009 - changed to: 
              (role-children-internal role) ; 
              do
              (setf fillers
                    (nconc fillers
                           (retrieve-individual-told-datatype-fillers ind (role-name parent) nil abox with-types-p)))))
      (remove-duplicates fillers))))

(defun equality-constraint-p (spec)
  (and (consp spec)
       (or (and (eq (first spec) 'equal)
                (eq (second spec) +has-integer-value+))
           (and (eq (first spec) '=)
                (eq (second spec) +has-real-value+))
           (and (eq (first spec) 'string=)
                (eq (second spec) +has-string-value+))
           (and (eq (first spec) 'string=)
                (eq (second spec) +has-boolean-value+)))))




;;; ======================================================================
 
(defun retrieve-individual-synonyms (individual &optional (told-only nil) (abox *current-abox*))
  (check-type individual symbol)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (unless (ensure-abox-is-coherent abox)
    (error "ABox ~A is incoherent." (abox-name abox)))
  (with-race-trace-sublevel ("individual-synonyms"
                             :arguments (list individual abox)
                             :trace-result t)
    (let ((ind (find-individual abox individual)))
      (with-abox-settings abox
        (with-concept-definition-mapping (abox-tbox abox) 
          (let* ((subgraph (individual-subgraph ind))
                 (synonyms (individual-name-set ind)))
            (if (or told-only
                    *use-unique-name-assumption*
                    (eq (abox-una-coherent-p abox) t)
                    (abox-el+-transformed-table abox))
              synonyms
              (let ((synonym-table (precompletion-individual-synonyms
                                    (subgraph-completion subgraph))))
                (if (or (null synonym-table)
                        (eql (hash-table-count synonym-table) 0))
                  synonyms
                  (let* ((synonym-candidates (subgraph-synonym-candidates subgraph))
                         (candidates
                          (if (eq synonym-candidates 'unknown)
                            (setf (subgraph-synonym-candidates subgraph)
                                  (individual-set-remove-duplicates
                                   (loop for ind-name being the hash-key of synonym-table
                                         using (hash-value synonym-name)
                                         collect (find-individual abox ind-name)
                                         collect (find-individual abox synonym-name))))
                            synonym-candidates)))
                    (if candidates
                      (if (loop for candidate in candidates
                                always (subsetp (individual-name-set candidate) synonyms))
                        synonyms
                        (let ((inferred-synonyms
                               (retrieve-individual-synonyms-2 ind synonyms subgraph abox candidates)))
                          (unless (subsetp inferred-synonyms synonyms)
                            (loop with preferred-ind-name = (or (gethash individual synonym-table)
                                                                individual)
                                  with preferred-ind = (if (eql preferred-ind-name individual)
                                                         ind
                                                         (find-individual abox preferred-ind-name))
                                  for synonym-name in inferred-synonyms
                                  for synonym-ind = (find-individual abox synonym-name)
                                  do
                                  (setf (individual-name-set synonym-ind)
                                        (stable-union (individual-name-set synonym-ind)
                                                      inferred-synonyms))
                                  (unless (eql preferred-ind-name synonym-name)
                                    (set-individual-synonym abox preferred-ind synonym-name))))
                          inferred-synonyms))
                      synonyms)))))))))))

(defun retrieve-individual-synonyms-2 (ind synonyms subgraph abox candidates)
  (let ((marker-concept (gensym)))
    (create-tbox-internal-marker-concept (abox-tbox abox) marker-concept)
    (loop with use-precompletion = (and *use-abox-precompletion*
                                        (subgraph-precompletion subgraph))
          with use-model-merging = (and *model-merging* *abox-model-merging*)
	  with synonym-table = (abox-model-individual-synonyms abox)
	  with ind-synonym = (gethash (first (individual-name-set ind)) synonym-table)
	  for ind1 in candidates
	  unless (or (eql ind1 ind)
                     (member ind1 synonyms)
		     (obviously-no-ind-synonyms ind ind-synonym ind1 synonym-table))
          do
          (multiple-value-bind
	    (not-interacts-p unknown-p)
            (if use-model-merging
              (obviously-inds-not-interact ind ind1 abox)
              (values nil t))
            (when (or (not not-interacts-p) unknown-p)
              (unless (if use-precompletion
                        (test-abox-satisfiable
                         abox
                         (list (encode-constraint (list (first (individual-name-set ind))
                                                        `(not ,marker-concept)))
                               (encode-constraint (list (first (individual-name-set ind1))
                                                        marker-concept)))
                         nil
                         nil
                         nil
                         nil
                         (subgraph-precompletion subgraph))
                        (test-abox-satisfiable
                         abox
                         (list* (encode-constraint (list (first (individual-name-set ind))
                                                         `(not ,marker-concept)))
                                (encode-constraint (list (first (individual-name-set ind1))
                                                         marker-concept))
                                (loop for axiom in (subgraph-encoded-individual-axioms
                                                    subgraph)
                                      collect (encode-constraint axiom)))
                         (subgraph-encoded-role-axioms subgraph)
                         (subgraph-individual-names subgraph)
                         (abox-attribute-constraints abox)
                         (copy-solver-state (abox-initial-constraint-state abox))))
                (setf synonyms (stable-union (individual-name-set ind1) synonyms)))))))
  synonyms)

(defun obviously-no-ind-synonyms (ind-1 ind-1-synonym ind-2 synonym-table)
  (let ((ind-2-synonym (gethash (first (individual-name-set ind-2)) synonym-table)))
    (or (null (or ind-1-synonym ind-2-synonym))
	(not (or (eql ind-1-synonym ind-2-synonym)
		 (eql ind-1 ind-2-synonym)
		 (eql ind-2 ind-1-synonym))))))

(defmacro individual-synonyms (individual &optional (told-only nil)
                                          (abox-name *current-abox* abox-name-supplied-p))
  (if abox-name-supplied-p
    `(retrieve-individual-synonyms ',individual ',told-only ',abox-name)
    `(with-abox-defined-check *current-abox*
       (retrieve-individual-synonyms ',individual ',told-only *current-abox*))))



(defun retrieve-individual-antonyms (individual &optional (told-only nil) (abox *current-abox*))
  (check-type individual symbol)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (unless (ensure-abox-is-coherent abox)
    (error "ABox ~A is incoherent." (abox-name abox)))
  (with-race-trace-sublevel ("retrieve-individual-antonyms"
                             :arguments (list individual told-only abox)
                             :trace-result t)
    (with-abox-settings abox
      (with-concept-definition-mapping (abox-tbox abox) 
	(cond 
         (told-only 
          (loop with antonyms = nil
                for entry in (abox-individual-identity-disjointness-assertions abox)
                for what = (first entry)
                do
                (if (eq what 'different-from)
                    (let ((individual-name-1 (second entry))
                          (individual-name-2 (third entry)))
                      (cond ((eql individual-name-1 individual) 
                             (push individual-name-2 antonyms))
                            ((eql individual-name-1 individual) 
                             (push individual-name-2 antonyms))))
                  (when (and (eq what 'all-different) (member individual (rest entry)))
                    (setf antonyms (nconc (remove individual (rest entry)) antonyms))))
                finally (return (racer-remove-duplicates antonyms))))
         (*use-unique-name-assumption*
          (unless (abox-una-coherent-p abox)
            (error "ABox ~A is not coherent w.r.t. the unique name assumption." (abox-name abox)))
          (racer-remove individual (all-individuals abox)))
         (t (let* ((antonyms nil)
                   (ind (find-individual abox individual))
                   (subgraph (individual-subgraph ind)))
              (loop for entry in (abox-individual-identity-disjointness-assertions abox)
                    for what = (first entry)
                    do
                    (if (eq what 'different-from)
                        (let ((individual-name-1 (second entry))
                              (individual-name-2 (third entry)))
                          (cond ((eql individual-name-1 individual) 
                                 (push (find-individual abox individual-name-2) antonyms))
                                ((eql individual-name-2 individual) 
                                 (push (find-individual abox individual-name-1) antonyms))))
                      (when (and (eq what 'all-different) (member individual (rest entry)))
                        (loop for ind in (rest entry)
                              unless (eql ind individual) do
                              (push (find-individual abox ind) antonyms)))))
              (loop with use-precompletion = (and *use-abox-precompletion*
                                                  (subgraph-precompletion subgraph))
                    for ind1 in (abox-individuals-list abox)
                    unless (or (eql ind1 ind) (member ind1 antonyms)) do
                    (let* ((subgraph1 (individual-subgraph ind1))
                           (satisfiable
                            (if (eq subgraph1 subgraph)
                                (if use-precompletion
                                    (test-abox-satisfiable
                                     abox
                                     (loop for axiom in (subgraph-encoded-individual-axioms subgraph1)
                                           collect 
                                           (encode-constraint (replace-ind-name-in-concept-assertion 
                                                               axiom ind ind1)))
                                     (mapcar #'(lambda (axiom) 
                                                 (encode-constraint
                                                  (replace-ind-name-in-role-assertion axiom ind ind1)))
                                             (subgraph-encoded-role-axioms subgraph1))
                                     (append (subgraph-individual-names subgraph)
                                             (remove (first (individual-name-set ind1))
                                                     (subgraph-individual-names subgraph1)))
                                     nil
                                     nil
                                     (subgraph-precompletion subgraph))
                                  (test-abox-satisfiable
                                   abox 
                                   (loop for axiom in (subgraph-encoded-individual-axioms subgraph)
                                         collect 
                                         (encode-constraint (replace-ind-name-in-concept-assertion 
                                                             axiom ind ind1)))
                                   (mapcar #'(lambda (axiom) 
                                               (encode-constraint
                                                (replace-ind-name-in-role-assertion axiom ind ind1)))
                                           (subgraph-encoded-role-axioms subgraph))
                                   (remove (first (individual-name-set ind1))
                                           (subgraph-individual-names subgraph1))
                                   (abox-attribute-constraints abox)
                                   (copy-solver-state (abox-initial-constraint-state abox))))
                              (test-abox-satisfiable
                               abox
                               (nconc (loop for axiom in (subgraph-encoded-individual-axioms subgraph)
                                            collect (encode-constraint axiom))
                                      (loop for axiom in (subgraph-encoded-individual-axioms
                                                          subgraph1)
                                            collect 
                                            (encode-constraint (replace-ind-name-in-concept-assertion 
                                                                axiom ind ind1))))
                               (append (subgraph-encoded-role-axioms subgraph)
                                       (mapcar #'(lambda (axiom) 
                                                   (encode-constraint
                                                    (replace-ind-name-in-role-assertion axiom ind ind1)))
                                               (subgraph-encoded-role-axioms subgraph1)))
                               (append (subgraph-individual-names subgraph)
                                       (remove (first (individual-name-set ind1))
                                               (subgraph-individual-names subgraph1)))
                               (abox-attribute-constraints abox)
                               (copy-solver-state (abox-initial-constraint-state abox))))))
                      (unless satisfiable
                        (push ind1 antonyms))))
              (loop for antonym in antonyms
                    for name-set = (individual-name-set antonym)
                    if (rest name-set)
                    append name-set into all-antonyms
                    else
                    collect (first name-set) into all-antonyms
                    finally
                    (return (racer-remove-duplicates all-antonyms))))))))))
		 
(defun replace-ind-name-in-concept-assertion (axiom ind ind1)
  (if (member (first axiom) (individual-name-set ind1))
      (list (first (individual-name-set ind)) (second axiom))
    axiom))

(defun replace-ind-name-in-role-assertion (axiom ind ind1)
  (setf axiom (list (list (constraint-ind-1 axiom) (constraint-ind-2 axiom)) (constraint-term axiom)))
  (when (member (first (first axiom)) (individual-name-set ind1))
        (setf (first (first axiom))
              (first (individual-name-set ind))))
  (when (member (second (first axiom)) (individual-name-set ind1))
        (setf (second (first axiom))
              (first (individual-name-set ind))))
  axiom)

(defmacro individual-antonyms (individual &optional (told-only nil)
                                          (abox-name *current-abox* abox-name-supplied-p))
  (if abox-name-supplied-p
    `(retrieve-individual-antonyms ',individual ',told-only ',abox-name)
    `(with-abox-defined-check *current-abox*
       (retrieve-individual-antonyms ',individual ',told-only *current-abox*))))

(defun temporary-abox-satisfiable (tbox ind-assertions role-assertions)
  (let* ((abox-name (gensym))
         (abox (make-abox abox-name tbox))
         (*current-abox* abox))
    (setf (find-abox abox-name) *current-abox*)
    (loop for (ind concept) in ind-assertions do
          (add-concept-assertion abox ind concept))
    (loop for (ind-1 ind-2 role) in role-assertions do
          (add-role-assertion abox ind-1 ind-2 role))
    (prog1
        (abox-consistent-p abox)
      (forget-abox abox))))

;;; ======================================================================

(defun all-concept-assertions-for-individual (individual 
                                                     &optional
                                                     (abox *current-abox*)
                                                     &key (count nil))
  (check-type individual symbol)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (ensure-knowledge-base-state ':abox-prepared abox)
  (possibly-count-only (individual-assertions (find-individual abox individual)) count))

(defun all-role-assertions-for-individual-in-domain (individual 
                                                             &optional
                                                             (abox *current-abox*)
                                                             &key (count nil))
  (check-type individual symbol)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (ensure-knowledge-base-state ':abox-prepared abox)
  (possibly-count-only
   (loop for constraint in (individual-outgoing-role-assertions
                            (find-individual abox individual)) 
         collect
         `((,(constraint-ind-1 constraint) ,(constraint-ind-2 constraint))
           ,(role-name (constraint-term constraint))))
   count))
        

(defun all-role-assertions-for-individual-in-range (individual 
                                                            &optional
                                                            (abox *current-abox*)
                                                            &key (count nil))
  (check-type individual symbol)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (ensure-knowledge-base-state ':abox-prepared abox)
  (possibly-count-only
   (loop for constraint in (individual-incoming-role-assertions
                            (find-individual abox individual)) 
         collect
         `((,(constraint-ind-1 constraint) ,(constraint-ind-2 constraint))
           ,(role-name (constraint-term constraint))))
   count))

(defun all-role-assertions (&optional (abox *current-abox*) &key (count nil))
  (setf abox (find-abox abox))
  (check-type abox abox)
  (ensure-knowledge-base-state ':abox-prepared abox)
  (possibly-count-only
   (if nil ;*use-less-abox-memory*
     (append (abox-role-axioms abox)
             (loop for subrgaph in (abox-subgraphs abox)
                   nconc
                   (loop for assertion in (subgraph-encoded-role-axioms subrgaph)
                         collect (list (list (constraint-ind-1 assertion)
                                             (constraint-ind-2 assertion))
                                       (role-name (constraint-term assertion))))))
     (abox-role-axioms abox))
   count))

(defun all-annotation-role-assertions (&optional (abox *current-abox*)
                                                      &key (count nil))
  (setf abox (find-abox abox))
  (check-type abox abox)
  (ensure-knowledge-base-state ':abox-prepared abox)
  (possibly-count-only (abox-annotation-role-axioms abox) count))


(defun all-attribute-assertions (&optional (abox *current-abox*) 
                                               (individual nil individual-supplied-p)
                                               &key (count nil))
  (setf abox (find-abox abox))
  (check-type abox abox)
  (ensure-knowledge-base-state ':abox-prepared abox)
  (possibly-count-only
   (if individual-supplied-p
     (individual-attribute-assertions (find-individual abox individual))
     (reverse 
      (loop for ((ind object) attribute) in (abox-attribute-assertions abox)
            collect `(constrained ,ind ,object ,attribute))))
   count))

(defun all-concept-assertions (&optional (abox *current-abox*)
                                             &key (count nil))
  (setf abox (find-abox abox))
  (check-type abox abox)
  (ensure-knowledge-base-state ':abox-prepared abox)
  (possibly-count-only
   (if nil ;*use-less-abox-memory*
     (loop for subraph in (abox-subgraphs abox)
           nconc
           (loop for assertion in (subgraph-encoded-individual-axioms subraph)
                 collect (list (first assertion)
                               (decode-concept (second assertion) nil t))))
     (abox-individual-axioms abox))
   count))

(defun all-annotation-concept-assertions (&optional (abox *current-abox*)
                                                          &key (count nil))
  (setf abox (find-abox abox))
  (check-type abox abox)
  (ensure-knowledge-base-state ':abox-prepared abox)
  (possibly-count-only (abox-annotation-individual-axioms abox) count))

(defun all-constraints (&optional (abox *current-abox*) 
                                     (object-names nil object-names-supplied-p)
                                     &key (count nil))
  (setf abox (find-abox abox))
  (check-type abox abox)
  (ensure-knowledge-base-state ':abox-prepared abox)
  (possibly-count-only 
   (let ((constraints (abox-constraints abox)))
     (if object-names-supplied-p
       (loop for constraint in constraints
             when (loop for object-name in object-names
                        thereis (object-name-mentioned-in-constraint-p object-name constraint))
             collect constraint)
       constraints))
   count))



;;; ======================================================================

(defmacro validate-true (query-term)
  `(unless ,query-term
     (error "Query term ~S should evaluate to non-nil."
            ',query-term)))

(defmacro validate-false (query-term)
  `(when ,query-term
     (error "Query term ~S should evaluate to nil."
            ',query-term)))

(defmacro validate-set (query-term &rest set)
  (let ((result-sym (gensym))
        (set-sym (gensym)))
  `(let ((,result-sym ,query-term)
         (,set-sym ',set))
     (unless (and (subsetp ,result-sym ,set-sym) 
                  (subsetp ,set-sym ,result-sym))
       (error "Query term ~S should evaluate to ~S."
              ',query-term ,set-sym)))))

;;; ======================================================================

(defun create-abox-clone (abox &key (new-name nil) (overwrite nil) (copy-rules t))
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type new-name (or symbol null))
  (when (and new-name (not overwrite) (find-abox new-name nil))
    (error "ABox ~A already known: cannot clone ABox ~A using the same name"
           new-name (abox-name abox)))
  (with-race-trace-sublevel ("create-abox-clone"
                             :arguments (list abox new-name overwrite)
                             :trace-result t)
    (ensure-knowledge-base-state ':abox-prepared abox)
    (let ((clone (copy-abox abox))
          (new-name (or new-name (gentemp (symbol-name (abox-name abox))))))
      (setf (abox-name clone) new-name)
      (setf (find-abox new-name) clone)
      (when copy-rules
        (loop for rule in (abox-rules clone) do
              (add-rule-axiom clone (first (swrl-rule-head rule)) `(and .,(swrl-rule-body rule))))
        (copy-rules (abox-name abox) (abox-name clone)))
      (setf *current-abox* clone)
      (abox-name clone))))

(defmacro clone-abox (abox &key
                           (new-name nil new-name-specified-p)
                           (overwrite nil))
  `(if ,new-name-specified-p
     (create-abox-clone ',abox :new-name ',new-name :overwrite ,overwrite)
     (create-abox-clone ',abox :overwrite ,overwrite)))

(defun set-associated-tbox (abox new-tbox)
  (check-type abox (or symbol abox))
  (check-type new-tbox (or symbol tbox))
  (setf abox (find-abox abox))
  (setf new-tbox (find-tbox new-tbox))
  (initialize-abox abox)
  (setf (abox-tbox abox) new-tbox)
  nil)

(defun associated-tbox (abox)
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (tbox-name (abox-tbox abox)))

(defun forget-abox (abox)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (when (find-abox abox nil)
    (when (eq *current-abox* abox)
      (setf *current-abox* (find-abox 'default)))
    (if (eq abox (find-abox 'default))
      (setf abox (clear-default-abox))
      (setf (find-abox (abox-name abox)) nil))
    (abox-name abox)))

(defun clear-default-abox ()
  (setf *current-abox*
        (make-abox 'default (find-tbox 'default)))
  (setf (find-abox 'default) *current-abox*)
  *current-abox*)

(defmacro delete-abox (abox)
  `(forget-abox ',abox))

(defun delete-all-aboxes ()
  (prog1
    (list (remove 'default (all-aboxes)) (all-aboxes))
    (setf *abox-table* (racer-make-hash-table :test #'equal))
    (setf *current-abox* (make-abox 'default (find-tbox 'default)))
    (setf (find-abox 'default) *current-abox*)
    nil))

;;; ======================================================================

(defun initialize-default-abox ()
  (setf (find-abox 'default) (make-abox 'default (find-tbox 'default)))
  (setf *current-abox* (find-abox 'default)))


;;; ======================================================================

(defun print-concept-assertion (stream ind-name concept)
  (if (symbol-package ind-name)
      (print `(instance ,ind-name ,concept) stream)
    (format stream "~&(instance ~A ~A)" (symbol-name ind-name) concept)))

(defun print-role-assertion (stream ind-name-1 ind-name-2 role)
  #+:debug (assert (and (symbol-package ind-name-1) (symbol-package ind-name-2)))
  (print `(related ,ind-name-1 ,ind-name-2 ,role) stream))

(defun print-identity-disjointness-assertion (stream assertion)
  #+:debug (every #'symbol-package (rest assertion))
  (print assertion stream))

(defun print-attribute-assertion (stream ind-name obj-name attribute)
  #+:debug (assert (symbol-package ind-name))
  (print `(constrained ,ind-name ,obj-name ,attribute) stream))

(defun print-abox-rule (stream rule)
  (if (> (length (swrl-rule-head rule)))
    (racer-warn "Ignoring rule while saving Abox.")
    (print `(define-rule ,(first (swrl-rule-head rule)) (and .,(swrl-rule-body rule)))
           stream)))

(defun print-abox (&key (stream t) (abox *current-abox*) (test nil)
                          (pretty nil) (anonymized nil)
                          (concept-mapping nil) (role-mapping nil) (header t))
  (let* ((tbox (abox-tbox abox))
         (orig-tbox-name (tbox-name tbox))
         (new-tbox-name (if (stringp orig-tbox-name)
                          (intern (string-upcase orig-tbox-name))
                          orig-tbox-name))
         (orig-abox-name (abox-name abox))
         (new-abox-name (if (stringp orig-abox-name)
                          (intern (string-upcase orig-abox-name))
                          orig-abox-name))
         (*print-pretty* pretty))
    (cond (test
           (print-test-abox abox new-tbox-name new-abox-name stream))
          (anonymized
           (if (null (or concept-mapping role-mapping))
             (racer-warn "No mapping given. Ignoring call of print-abox.")
             (print-anonymized-abox abox stream concept-mapping role-mapping :header header)))
          (t
           (print-original-abox abox new-tbox-name new-abox-name stream :header header)))))

(defun print-original-abox (abox tbox-name abox-name stream &key (header t))
  (when nil ;*use-less-abox-memory*
    (racer-warn "Abox memory savings option is enabled: no information is available for Abox ~S" abox))
  (when header
    (print `(in-abox ,abox-name ,tbox-name) stream))
  (when (abox-signature abox)
    (destructuring-bind (individuals objects)
                        (abox-signature abox)
      (print `(signature :individuals ,individuals :objects ,objects) stream)))
  (loop for (name concept) in (abox-individual-axioms abox) do
        (print-concept-assertion stream name concept))
  (loop for ((name-1 name-2) role) in (abox-role-axioms abox) do
        (print-role-assertion stream name-1 name-2 role))
  (loop for assertion in (abox-individual-identity-disjointness-assertions abox) do
        (print-identity-disjointness-assertion stream assertion))
  (loop for ((ind-name obj-name) attribute) in (abox-attribute-assertions abox) do
        (print-attribute-assertion stream ind-name obj-name attribute))
  (loop for rule in (abox-rules abox) do
        (print-abox-rule stream rule))
  (let ((constraints (abox-constraints abox)))
    (when constraints
      (print `(constraints . ,constraints) stream)))
  nil)

(defun print-test-abox (abox tbox-name abox-name stream)
  (print `(define-abox (,abox-name ,tbox-name :debug t)
            ,@(append (abox-individual-axioms abox)
                      (abox-role-axioms abox)))
         stream)
  nil)

(defun print-anonymized-abox (abox stream concept-mapping role-mapping &key (header t))
  (when nil ;*use-less-abox-memory*
    (racer-warn "Abox memory savings option is enabled: no information is available for Abox ~S" abox))
  (let* ((ordered-ind-names (mapcar (lambda (ind)
                                      (first (individual-name-set ind)))
                                    (abox-individuals-list abox)))
         (objects (abox-objects-list abox))
         (ind-mapping (racer-make-hash-table :size (length ordered-ind-names)))
         (obj-mapping (racer-make-hash-table :size (length objects))))
    (loop for ind-name in ordered-ind-names
          for count = 1 then (1+ count)
          do (setf (gethash ind-name ind-mapping) (intern (format nil "I~D" count))))
    (loop for obj-name in objects
          for count = 1 then (1+ count)
          do (setf (gethash obj-name obj-mapping) (intern (format nil "O~D" count))))
    (when header
      (print `(in-abox ,(abox-name abox) ,(tbox-name (tbox abox))) stream))
    (when (abox-signature abox)
      (print `(signature :individuals ,(map-names (first (abox-signature abox)) ind-mapping)
                         :objects ,(map-names (second (abox-signature abox)) obj-mapping))
             stream))
    (loop for assertion in (abox-individual-axioms abox)
          for (name concept) = (map-names (map-names (map-names assertion concept-mapping)
                                                     role-mapping)
                                          ind-mapping)
          do (print-concept-assertion stream name concept))
    (loop for assertion in (abox-role-axioms abox)
          for ((name-1 name-2) role) = (map-names (map-names assertion role-mapping) ind-mapping)
          do (print-role-assertion stream name-1 name-2 role))
    (loop for assertion in (abox-individual-identity-disjointness-assertions abox) do
          (print-identity-disjointness-assertion stream (map-names assertion ind-mapping)))
    (when (abox-attribute-assertions abox)
      (error "Abox contains attribute assertions. Anonymizing not yet supported."))
    (loop for rule in (abox-rules abox) do
          (print-abox-rule stream rule))
    (when (abox-constraints abox)
      (error "Abox contains constraints. Anonymizing not yet supported."))
    #|
    (loop for ((ind-name obj-name) attribute) in (abox-attribute-assertions abox) do
          (print-attribute-assertion stream ind-name obj-name attribute))
    (let ((constraints (abox-constraints abox)))
      (when constraints
        (print `(constraints . ,constraints) stream)))
    |#
    ind-mapping))

(defun dump-abox (filename &rest other-args 
                             &key (aboxes (list *current-abox*)) (anonymized nil) (test nil)
                             (dump-tbox nil) (tree-filename nil) (pretty nil) (add-non-subsumers nil)
                             &allow-other-keys)
  (let ((if-exists-arg (second (member ':if-exists other-args)))
        (original-arg (second (member ':original other-args)))
        (tbox (abox-tbox (first aboxes))))
    (multiple-value-bind
      (concept-mapping role-mapping)
      (when dump-tbox
        (when (and (or (null if-exists-arg) (eq if-exists-arg ':supersede))
                   (probe-file filename))
          (delete-file filename))
        (with-open-file (stream filename :direction :output :if-exists :append :if-does-not-exist :create)
          (format stream "~%(~%("))
        (dump-tbox filename :tbox tbox :anonymized anonymized :test test
                   :tree-filename tree-filename :pretty pretty
                   :if-exists ':append
                   :original (not original-arg))
        (with-open-file (stream filename :direction :output :if-exists :append :if-does-not-exist nil)
          (format stream "~%)")))
      (with-open-file (stream filename :direction :output
                              :if-exists (or if-exists-arg
                                             (if dump-tbox
                                               :append
                                               :supersede))
                              :if-does-not-exist nil)
        (loop for abox in aboxes do
              (unless (eq tbox (abox-tbox abox))
                (error "confusion in dump-abox"))
              (when dump-tbox
                (format stream "~%(~%("))
              (let ((ind-mapping
                     (print-abox :stream stream :abox abox
                                 :anonymized anonymized :test test :pretty pretty
                                 :concept-mapping concept-mapping :role-mapping role-mapping)))
                (when dump-tbox
                  (format stream "~%)~%(")
                  (loop with ind-info-list = (abox-individuals-types abox
                                                                     concept-mapping
                                                                     ind-mapping
                                                                     add-non-subsumers)
                        for (ind-name type-list no-type-list) in ind-info-list
                        do
                        (loop for types in type-list do
                              (loop for type in types do
                                    (print `(individual-instance? ,ind-name ,type) stream)
                                    (print `(not (individual-instance? ,ind-name (not ,type))) stream)))
                        (loop for type in no-type-list do
                              (print `(not (individual-instance? ,ind-name ,type)) stream)))
                  (format stream "~%)~%)"))))
        (when dump-tbox
          (format stream "~%)"))))))

(defun abox-individuals-types (abox concept-mapping ind-mapping add-non-subsumers)
  (ensure-knowledge-base-state ':abox-realized abox)
  (loop for individual in (abox-individuals-list abox)
        for ind-name = (first (individual-name-set individual))
        when (symbol-package ind-name)
        collect (if add-non-subsumers
                  (list ind-name
                        (most-specific-instantiators ind-name abox)
                        (model-negative-literals (get-cached-individual-model individual abox)))
                  
                  (list ind-name (most-specific-instantiators ind-name abox)))
        into ind-list
        finally
        (when (and concept-mapping ind-mapping)
          (setf ind-list (map-names (map-names ind-list concept-mapping) ind-mapping)))
        (return (sort ind-list #'string-lessp :key #'first))))

(defun print-abox-individuals (&key (stream *standard-output*) (abox *current-abox*)
                                    (concept-mapping nil) (ind-mapping nil) (pretty t))
  (let ((*print-pretty* pretty))
    (print (abox-individuals-types abox concept-mapping ind-mapping nil) stream))
  (values))

(defun dump-abox-individuals-types (filename &key (abox *current-abox*))
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (print-abox-individuals :stream stream :abox abox :pretty nil)))

(defun verify-with-abox-individuals-types (filename &key (abox *current-abox*) (verbose t))
  (let ((start (get-internal-run-time))
        time)
    (ensure-knowledge-base-state ':abox-realized abox)
    (setf time (- (get-internal-run-time) start))
    (let ((error nil))
      (with-open-file (individuals-file filename :direction :input)
	(when verbose
	  (format t "~&Verifying ABox ~S with individuals types in file ~A..."
		  abox filename))
	(loop with n-inds = (length (abox-individuals-list abox))
              with individuals-list = (read individuals-file nil '(done))
              initially
	      (when (eq individuals-list 'done)
		(setf error t)
		(return))
              for (ind-name parents) in individuals-list
              for counter from 1
              when (and verbose (> n-inds 10))
              do (funcall *tick-function* (/ counter n-inds))
              unless (verify-individual-parents abox ind-name parents)
              do (setf error t))
	(if error
          (format t "~&Problem: Failed to verify ABox ~S with individuals types in file ~A"
                  abox filename)
	  (when verbose
	    (format t "~&Successfully verified ABox ~S with individuals types in file ~A"
		    abox filename)))))
    time))

(defun verify-with-abox-individuals-list (individuals-list
                                                &optional (abox *current-abox*))
  (let ((start (get-internal-run-time))
        time)
    (ensure-knowledge-base-state ':abox-realized abox)
    (setf time (- (get-internal-run-time) start))
    (loop for (ind-name ind-concept-parents) in individuals-list do
          (verify-individual-parents abox ind-name ind-concept-parents))
    time))

(defun verify-individual-parents (abox ind-name asserted-parents)
  (let* ((ind-parents (most-specific-instantiators ind-name abox))
         (diffs (set-exclusive-or asserted-parents ind-parents :test #'safe-set-xor-test)))
    (if diffs
      (progn
        #+:lispworks
        (cerror "Proceed with verification"
                "Difference ~S in most specific concepts for ~S found:~%~
                 own: ~S~%asserted: ~S"
                diffs ind-name ind-parents asserted-parents)
        #-:lispworks
        (format t "~%Error: Difference ~S in most specific concepts for ~S found:~%~
                   own: ~S~%asserted: ~S"
                diffs ind-name ind-parents asserted-parents)
        nil)
      t)))

(defun verify-related-individuals (related-individuals-list &optional (abox *current-abox*))
  (loop for (ind-1-synonyms-1 ind-2-synonyms-1 filled-role-names-1) in related-individuals-list
        for ind-1 = (first ind-1-synonyms-1)
        for ind-2 = (first ind-2-synonyms-1)
        for ind-1-synonyms-2 = (retrieve-individual-synonyms ind-1)
        for ind-2-synonyms-2 = (retrieve-individual-synonyms ind-2)
        for filled-role-names-2 = (retrieve-individual-filled-roles ind-1 ind-2 abox)
        unless (and (set-equal-p ind-1-synonyms-1 ind-1-synonyms-2)
                    (set-equal-p ind-2-synonyms-1 ind-2-synonyms-2)
                    (subsetp filled-role-names-1 filled-role-names-2 :test 'equal))
        do 
        (progn
          #+:lispworks
          (cerror "Proceed with verification"
                  "Difference in related individuals list for (~S ~S) found:~%~
                   own: (~S ~S ~S)~%asserted: (~S ~S ~S)"
                  ind-1 ind-2 ind-1-synonyms-2 ind-2-synonyms-2 filled-role-names-2
                  ind-1-synonyms-1 ind-2-synonyms-1 filled-role-names-1)
          #-:lispworks
          (format t "Difference in related individuals list for (~S ~S) found:~%~
                   own: (~S ~S ~S)~%asserted: (~S ~S ~S)"
                  ind-1 ind-2 ind-1-synonyms-2 ind-2-synonyms-2 filled-role-names-2
                  ind-1-synonyms-1 ind-2-synonyms-1 filled-role-names-1)
          nil)
        finally (return t)))

(defun save-abox (pathname-or-stream
                    &optional (abox *current-abox*)
                    &key (syntax :krss) (transformed nil)
                    (if-exists :supersede) (if-does-not-exist :create)
                    (header t) (uri nil) (import-list nil)
                    (ontology-name nil))
  (check-type abox (or symbol abox))
  (check-type syntax (member :owl :krss :race :test))
  (check-type pathname-or-stream (or pathname string file-stream))
  (flet ((save-abox-1 (abox stream)
           (case syntax
             ((:race :krss) 
              (print-abox :abox abox
                          :stream stream
                          :test nil
                          :header header))
             (:owl 
              (print-owl-abox abox stream :uri uri :import-list import-list :ontology-name ontology-name))
             (:test 
              (print-abox :abox abox
                          :stream stream
                          :test t
                          :header header)))))
    (let ((abox (find-abox abox)))
      (when transformed
        (racer-warn "Saving the ABox ~S in the transformed mode is currently not supported."
                    abox))
      (ensure-knowledge-base-state :abox-prepared abox)
      (with-abox-settings abox
        (with-concept-definition-mapping (abox-tbox abox) 
          (etypecase pathname-or-stream
            (file-stream (save-abox-1 abox pathname-or-stream))
            (pathname
             (with-open-file (stream pathname-or-stream
                                     ;; :element-type :character
                                     :external-format *file-external-format*
                                     :direction :output
                                     :if-exists if-exists 
                                     :if-does-not-exist if-does-not-exist)
               (save-abox-1 abox stream)))
            (string 
             (with-open-file (stream pathname-or-stream
                                     ;; :element-type :character
                                     :external-format *file-external-format*
                                     :direction :output
                                     :if-exists if-exists 
                                     :if-does-not-exist if-does-not-exist)
               (save-abox-1 abox stream)))))))))

(defun print-ore-individual-instantiators (sat-p &key (filename nil) (stream t) (abox *current-abox*))
  (ensure-knowledge-base-state :abox-prepared abox)
  (with-abox-settings abox
    (with-concept-definition-mapping (abox-tbox abox) 
      (let ((namespace-prefix (when sat-p
                                (get-namespace-prefix (abox-tbox abox)))))
        (print-ore-prefixes stream namespace-prefix)
        (format stream "Ontology(")
        (if filename
            (format stream "<file:~A>~2%" filename)
          (format stream "<http://a.com>~2%"))
        (if (not sat-p)
            (format stream "SubClassOf(owl:Thing owl:Nothing)~%")
          (progn
            (ensure-knowledge-base-state :abox-realized abox)
            (loop for individual in (abox-individuals-list abox)
                  for individual-name = (first (individual-name-set individual))
                  for true-individual = (find-individual abox individual-name)
                  do
                  (loop for instantiator in (individual-ancestor-concepts true-individual) do
                        (unless (is-top-concept-p instantiator)
                          (format stream "ClassAssertion(~A ~A)~%"
                                  (ore-name (first (concept-name-set instantiator)) namespace-prefix)
                                  (ore-name individual-name namespace-prefix)))))))
        (format stream ")~%")))))

;;; ======================================================================

(defun get-individual-pmodel (individual-name abox)
  (check-type individual-name symbol)
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (ensure-knowledge-base-state :abox-prepared abox)
  (if (abox-consistent-p abox)
    (let ((tbox (abox-tbox abox)))
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
          (let* ((*meta-constraint-concepts* (tbox-meta-constraint-concepts tbox))
                 (*blocking-possibly-required*
                  (or *encode-roles-as-transitive*
                      *meta-constraint-concepts*
                      (tbox-blocking-possibly-required tbox))))
            (let* ((individual (find-individual abox individual-name))
                   (model (get-cached-individual-model individual abox)))
              (if (model-info-p model)
                (if (full-model-info-p model)
                  (list (individual-name-set individual)
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
                        (model-exists-models model)
                        (model-restrict-models model))
                  (list (individual-name-set individual)
                        (mapcar #'concept-term (model-det-positive-literals model))
                        nil ; no negated names
                        (racer-remove-duplicates (mapcar (lambda (concept)
                                                           (decode-role (concept-role concept)))
                                                         (model-exists-models model)))
                        nil ; no universal restrictions
                        nil ;attributes
                        nil ; ensured attributes
                        t ; deterministic
                        nil
                        nil))
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
                              nil
                              nil))
                      (let ((name (concept-term (concept-negated-concept model))))
                        (list nil ;name
                              nil ;pos lits
                              (list name) ;neg lits
                              nil ;exists
                              nil ;restricts
                              nil ;attributes
                              nil ; ensured attributes
                              t ;unique
                              nil
                              nil)))))))))))
    (list :abox-incoherent)))

;;; ======================================================================

#|
(defun show-abox (name &optional (show-bottom nil))
  (let* ((abox (find-abox name))
         (tbox (abox-tbox abox))
         (*visited-concepts* nil))
    (show-tbox (tbox-name tbox) show-bottom)
    (loop for individual in (all-individuals) do
          (eval
           `(defclass ,individual
              ,(sort-supers 
                (mapcar #'(lambda (name-set)
                            (first (concept-name-set
                                    (get-tbox-concept tbox (first name-set)))))
                        (most-specific-instantiators individual abox)))
              ())))))
|#

;;;  ======================================================================

(defun find-abox-no-error (sym)
  (find-abox sym nil))

(defun find-individual-in-current-abox (sym)
  (and *current-abox*
       (find-individual *current-abox* sym nil)))

(defun find-cd-object-in-current-abox (sym)
  (and *current-abox*
       (find-cd-object *current-abox* sym nil)))

