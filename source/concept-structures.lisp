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

;;;===========================================================================

(defvar *use-tbox* nil)                 ; is TBox available?
(defvar *use-abox* nil)                 ; is ABox available?

(defconstant +top-symbol+ '*top*
  "Used as unique id for symbol-name of the top concept")

(defconstant +bottom-symbol+ '*bottom*
  "Used as unique id for symbol-name of the bottom concept")

(defconstant +krss-top-symbol+ 'top
  "Used as synonym to *top*")

(defconstant +krss-bottom-symbol+ 'bottom
  "Used as synonym to *bottom*")

(defconstant +datatype-top-symbol+ '*datatype-top*
  "Used as unique id for symbol-name of the top datatype concept")

(defconstant +datatype-bottom-symbol+ '*datatype-bottom*
  "Used as unique id for symbol-name of the bottom datatype concept")

(defconstant +top-concept-hash-id+ 0)
(defconstant +bottom-concept-hash-id+ 1)

(defconstant +top-datatype-concept-hash-id+ 2)
(defconstant +bottom-datatype-concept-hash-id+ 3)

(race-inline (is-top-concept-p
              is-bottom-concept-p
              is-top-datatype-concept-p
              is-bottom-datatype-concept-p
              is-any-top-concept-p
              is-any-bottom-concept-p
              is-predefined-concept-p))

(defun is-top-concept-p (concept)
  (eql (concept-hash-id concept) +top-concept-hash-id+))

(defun is-bottom-concept-p (concept)
  (eql (concept-hash-id concept) +bottom-concept-hash-id+))

(defun is-top-datatype-concept-p (concept)
  (eql (concept-hash-id concept) +top-datatype-concept-hash-id+))

(defun is-bottom-datatype-concept-p (concept)
  (eql (concept-hash-id concept) +bottom-datatype-concept-hash-id+))

(defun is-any-top-concept-p (concept)
  (let ((hash-id (concept-hash-id concept)))
    (or (eql hash-id +top-concept-hash-id+)
        (eql hash-id +top-datatype-concept-hash-id+))))

(defun is-any-bottom-concept-p (concept)
  (let ((hash-id (concept-hash-id concept)))
    (or (eql hash-id +bottom-concept-hash-id+)
        (eql hash-id +bottom-datatype-concept-hash-id+))))

(defun is-predefined-concept-p (concept)
  (<= +top-concept-hash-id+ (concept-hash-id concept) +bottom-datatype-concept-hash-id+))

;;;===========================================================================
;;; Structures and accessors for concept terms
;;;===========================================================================

(defparameter *print-internals* nil
  "For debugging purposes the identity of objects might be interesting.")

(defstruct (concept
            (:include racer-structure-id)
            (:constructor make-concept (term))
            (:predicate concept-p-internal))
  "Represents the common part of an encoded concept."
  (term nil)                            ; subterm of concept
  (model nil)                           ; cached model of concept
  (told-subsumers nil)                  ; structurally told subsumers
  (negated-concept-internal nil)        ; link to negated counterpart
  (language *dl-empty*)                 ; marks DL language dialect
  (told-disjoints nil)                  ; known disjoints
  (visited-mark t)                      ; visited mark for traversals
  (referencing nil)                     ; non-modally referenced atomic concepts
  (full-referencing nil)                ; all referenced atomic concepts
  (inverse-roles nil)                   ; inverse roles referenced by direct successors
  (changed-p nil)                       ; if T concept information has been changed
  (referenced-disjoints nil)		; non-modally referenced negated atomic concepts
  (gci-dependencies nil)                ; list of gci-absorption-dependency elements (reverse chronological order)
  )

(defun concept-negated-concept (concept)
  (declare (ignore concept))
  (error "wrong function"))

(defun (setf concept-negated-concept) (new-value concept)
  (setf (concept-negated-concept-internal concept) new-value))

(defmethod print-object :around ((object concept) stream)
  (if *print-internals*
    (print-unreadable-object (object stream :type nil :identity t)
      (call-next-method))
    (call-next-method)))

(defun copy-concept-internal (concept)
  (let ((new-concept (copy-concept concept)))
    (setf (concept-hash-id new-concept)
          (racer-atomic-incf *structure-id-counter*))
    new-concept))

(defstruct (concept-node
            (:include concept)
            (:conc-name concept-))
  "Structure for TBox part of a concept. Only for inclusion in atomic-concept."
  (name-set nil)                        ; name of concept and its synonyms
  (definition nil)                      ; unencoded original definition
  (primitive-p nil)                     ; defined as primitive?
  (encoded-definition nil)              ; encoded concept definition
  (parents-internal nil)                ; list of atomic parents
  (children-internal nil)               ; list of atomic children
  (mark1 nil)                           ; used for TBox insertion
  (mark2 nil)                           ; used for ABox operations
  (visible-p t)                         ; should concept printed in tree?
  (asserted-disjoints nil)              ; atomic concepts asserted to be disjoint
  (sort-no nil)                         ; used for sorting in definition order
  (referenced-by nil)                   ; list of atomic concepts referencing this concepts
  (buckets nil)                         ; this concept is a member of the referenced buckets
  (bucket-children nil)                 ; list of buckets clustering children
  (children-not-in-bucket nil)          ; children currently not member of a bucket
  (encoded-negated-definition nil)      ; expansion of (not c) for lazy unfolding
  (self-referencing-p nil)              ; cyclic concept?
  (elh-role-domain-qualifications nil)  ; list of role / domain pairs (r (d1 ... dn)) used to encode axioms
                                        ; (implies (some r c) di) where this named concept is c
                                        ; Unfolding of c adds to all r-predecessors d
  (nary-unfold-sets nil)                ; list of sets in which this concept occurs for nary unfolding
  )

(defstruct (atomic-concept
            (:include concept-node)
            (:conc-name concept-))
  "Represents an atomic or defined concept, includes concept")

(defmethod print-object ((object atomic-concept) stream)
  #+:debug
  (when (and *use-tbox*
             (or (concept-encoded-definition object)
                 (concept-definition object)))
    (format stream "@"))
  (format stream "~S" (concept-term object))
  #+:debug
  (when (and *use-tbox* (concept-primitive-p object))
    (format stream "-*")))

(defstruct (negated-concept
            (:include concept)
            (:conc-name concept-)
            (:constructor make-negated-concept (term)))
  "Represents a negated concept, includes concept")

(defmethod print-object ((object negated-concept) stream)
  (format stream "(NOT ~S)" (concept-term object)))

(defstruct (quantification-concept
            (:include concept)
            (:conc-name concept-))
  "Represents the common part of quantified concepts, includes concept.
For inclusion by some-concept, at-least-, at-most- and all-concept."
  role
  decoded-list-term
  (self-reference-p nil) ; if T represents represent (some r self)
  )

#|
 (defmethod print-object :around ((object quantification-concept) stream)
  (cond ((or *print-internals* (null (concept-decoded-list-term object))
             )
         (call-next-method))
        (t (format stream "~A" (concept-decoded-list-term object)))))
 |#

(defun not-quantification-concept-p (concept)
  (not (quantification-concept-p concept)))

(defstruct (number-restriction-concept
            (:include quantification-concept)
            (:conc-name concept-))
  "Represents a number-restriction-concept, includes quantification-concept"
  number-restriction)

(defstruct (exists-concept
            (:include number-restriction-concept)
            (:conc-name concept-))
  "Represents a some- or at-least concept, includes number-restriction-concept"
  (role-domain nil)
  (role-range nil))

(defun not-exists-concept-p (concept)
  (not (exists-concept-p concept)))

(defstruct (at-least-concept
            (:include exists-concept)
            (:conc-name concept-)
            (:constructor make-at-least-concept (number-restriction role term)))
  "Represents an at-least-restriction, includes exists-concept")

(defmethod print-object ((object at-least-concept) stream)
  (let ((concept (concept-term object))
        (role (concept-role object)))
    (if (or (role-datatype role) (role-cd-attribute role))
        (if (is-top-datatype-concept-p concept)
            (format stream "(D-AT-LEAST ~D ~S)"
                    (concept-number-restriction object) role)
          (format stream "(D-AT-LEAST ~D ~S ~S)"
                  (concept-number-restriction object) role concept))
      (if (is-top-concept-p concept)
          (format stream "(AT-LEAST ~D ~S)"
                  (concept-number-restriction object) role)
        (format stream "(AT-LEAST ~D ~S ~S)"
                (concept-number-restriction object) role concept)))))

(defstruct (at-most-concept
            (:include number-restriction-concept)
            (:conc-name concept-)
            (:constructor make-at-most-concept (number-restriction role term)))
  "Represents an at-most-restriction, includes number-restriction-concept")

(defmethod print-object ((object at-most-concept) stream)
  (let ((concept (concept-term object))
        (role (concept-role object)))
    (if (or (role-datatype role) (role-cd-attribute role))
        (if (is-top-datatype-concept-p concept)
            (format stream "(D-AT-MOST ~D ~S)"
                    (concept-number-restriction object) role)
          (format stream "(D-AT-MOST ~D ~S ~S)"
                  (concept-number-restriction object) role concept))
      (if (is-top-concept-p concept)
          (format stream "(AT-MOST ~D ~S)"
                  (concept-number-restriction object) role)
        (format stream "(AT-MOST ~D ~S ~S)"
                (concept-number-restriction object) role concept)))))

(defstruct (some-concept
            (:include exists-concept (number-restriction 1))
            (:conc-name concept-)
            (:constructor make-some-concept (role term)))
  "Represents a some-concept, includes exists-concept")

(defmethod print-object ((object some-concept) stream)
  (if (concept-self-reference-p object)
      (format stream "(SELF-REFERENCE ~S)" (concept-role object))
    (let ((concept (concept-term object))
          (role (concept-role object)))
      (if (or (role-datatype role) (role-cd-attribute role))
          (format stream "(D-SOME ~S ~S)" role concept)
        (format stream "(SOME ~S ~S)" role concept)))))

(defstruct (all-concept
            (:include quantification-concept)
            (:conc-name concept-)
            (:constructor make-all-concept (role term)))
  "Represents an all-concept, includes quantification-concept")

(defmethod print-object ((object all-concept) stream)
  (if (concept-self-reference-p object)
      (format stream "(NOT (SELF-REFERENCE ~S))" (concept-role object))
    (let ((concept (concept-term object))
          (role (concept-role object)))
      (if (or (role-datatype role) (role-cd-attribute role))
          (format stream "(D-ALL ~S ~S)" role concept)
        (format stream "(ALL ~S ~S)" role concept)))))

(defstruct (ria-initial-concept
            (:include quantification-concept)
            (:conc-name concept-)
            (:constructor make-ria-initial-concept (role term)))
  "Represents an initial RIA concept, includes quantification-concept")

(defmethod print-object ((object ria-initial-concept) stream)
  (format stream "(I(ALL ~S ~S))" (concept-role object) (concept-term object)))

(defstruct (neg-ria-initial-concept
            (:include quantification-concept)
            (:conc-name concept-)
            (:constructor make-neg-ria-initial-concept (role term)))
  "Represents a negated initial RIA concept, only needed as linked negation")

(defmethod print-object ((object neg-ria-initial-concept) stream)
  (format stream "(I(SOME ~S ~S))" (concept-role object) (concept-term object)))

(defstruct (ria-final-concept
            (:include quantification-concept)
            (:conc-name concept-)
            (:constructor make-ria-final-concept (role term)))
  "Represents a final RIA concept, includes quantification-concept")

(defmethod print-object ((object ria-final-concept) stream)
  (format stream "(F(ALL ~S ~S))" (concept-role object) (concept-term object)))

(defstruct (neg-ria-final-concept
            (:include quantification-concept)
            (:conc-name concept-)
            (:constructor make-neg-ria-final-concept (role term)))
  "Represents a final RIA concept, includes quantification-concept")

(defmethod print-object ((object neg-ria-final-concept) stream)
  (format stream "(F(SOME ~S ~S))" (concept-role object) (concept-term object)))

(defstruct (and-concept
            (:include concept)
            (:conc-name concept-)
            (:constructor make-and-concept (term)))
  "Represents an and-concept, includes concept. The slot term contains
the list of subterms.")

(defmethod print-object ((object and-concept) stream)
  (if (consp (concept-term object))
    (format stream "(AND ~S~{ ~S~})"
            (first (concept-term object)) (rest (concept-term object)))
    (format stream "(AND ~A)"
            (concept-term object))))

(defstruct (disjoint-and-concept
            (:include and-concept)
            (:conc-name concept-)
            (:constructor make-disjoint-and-concept (term)))
  "Represents an special and-concept which results from disjointness declarations, 
includes concept. The slot term contains the list of negated atomic concepts")

(defstruct (or-concept
            (:include concept)
            (:conc-name concept-)
            (:constructor make-or-concept (term)))
  "Represents an or-concept, includes concept. The slot term contains
the list of subterms."
  (reference-counter 1)
  (bucket nil))

(defmethod print-object ((object or-concept) stream)
  (if (consp (concept-term object))
    (format stream "(OR ~S~{ ~S~})"
            (first (concept-term object)) (rest (concept-term object)))
    (format stream "(OR ~S)"
            (concept-term object))))


(defstruct (general-cd-concept
            (:include concept)
            (:conc-name concept-)))

(defmethod print-object ((object general-cd-concept) stream)
  (if (or (is-top-datatype-concept-p object) (is-bottom-datatype-concept-p object))
      (format stream "~S" (concept-term object))
    (call-next-method)))

(defstruct (cd-concept
            (:include general-cd-concept)
            (:conc-name concept-)
            (:constructor make-cd-concept (predicate)))
  (predicate nil)
  (attribute-domain nil)
  (attribute-range nil))

(defmethod print-object ((object cd-concept) stream)
  (handler-case 
    (format stream "~S" (concept-predicate object))
    (error (c) 
           (declare (ignore c))
           (format stream "#<Predicate ~S>" 
                   (predicate-name (concept-predicate object))))))

(defstruct (ensure-cd-concept
            (:include general-cd-concept)
            (:conc-name concept-)
            (:constructor make-ensure-cd-concept (term))))

(defmethod print-object ((object ensure-cd-concept) stream)
  (format stream "(ENSURE ~S)" (concept-term object)))

;;;===========================================================================

(defun make-top-concept ()
  (let ((top (make-atomic-concept :term +top-symbol+
                                  :hash-id +top-concept-hash-id+
                                  :name-set (list +top-symbol+ +krss-top-symbol+)
                                  :primitive-p t)))
    (setf (concept-model top) top)
    top))

(defun make-bottom-concept ()
  (let ((bottom (make-atomic-concept :term +bottom-symbol+
                                     :hash-id +bottom-concept-hash-id+
                                     :name-set (list +bottom-symbol+ +krss-bottom-symbol+)
                                     :primitive-p t)))
    (setf (concept-model bottom) bottom)
    bottom))

;;;===========================================================================

(defun make-top-datatype-concept ()
  (let ((top (make-general-cd-concept :term +datatype-top-symbol+
                                      :hash-id +top-datatype-concept-hash-id+)))
    (setf (concept-model top) top)
    top))

(defun make-bottom-datatype-concept ()
  (let ((bottom (make-general-cd-concept :term +datatype-bottom-symbol+
                                         :hash-id +bottom-datatype-concept-hash-id+)))
    (setf (concept-model bottom) bottom)
    bottom))
