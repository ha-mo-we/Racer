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

(defvar *tbox-version* 0)

(defun new-tbox-version ()
  (incf *tbox-version*))

(defstruct (tbox (:constructor make-tbox-internal)
                  (:copier copy-tbox-internal))
  (name nil)
  (identification nil)
  (original-concept-axioms nil)
  (concept-axioms nil)
  (concept-axioms-index nil)
  (expected-concept-size 100)
  (expected-role-size 100)
  (generalized-concept-inclusions nil)
  (added-generalized-concept-inclusions nil)
  (removed-generalized-concept-inclusions nil)
  (encoded-concept-list nil)
  (encoded-role-list nil)
  (literal-meta-constraints nil)
  (meta-constraint-concepts nil)
  (role-axioms nil)
  (role-axioms-index nil)
  (taxonomic-encoding-table (make-taxonomic-encoding-table))
  (empty-taxonomic-encoding-table t)
  (role-store nil)
  (all-roles nil)
  (all-features nil)
  (all-transitive-roles nil)
  (all-attributes nil)
  (top-node nil)
  (bottom-node nil)
  (structure-id-counter +racer-structure-id-counter-init+)
  (concept-store nil)
  (tableaux-cache nil)
  (tableaux-sat-cache nil)
  (tableaux-unsat-cache nil)
  (coherence-checked-p nil)
  (coherent-p-internal ':dont-know)
  (index-structures-complete-p nil)
  (classified-p-internal nil)
  (disjoint-set (racer-make-hash-table))
  (language *dl-empty*)
  (atomic-concepts nil)
  (signature nil)
  (blocking-possibly-required nil)
  (auto-installed-primitives nil)
  (concrete-domain nil)
  (end-of-no-bottom-search nil)
  (ontologies nil)
  (role-synonyms nil)
  (role-antonyms nil)  ; List of role names or (inv role-name) terms
  (version (new-tbox-version))
  (internal-marker-concepts (racer-make-hash-table))
  (taxonomy-snapshot nil)
  (meta-constraint-concept nil)
  (stable-set-difference-table (racer-make-hash-table))
  (stable-set-difference-last-list2 nil)
  (racer-remove-duplicates-table (racer-make-hash-table :test 'equal :structure-p t))
  (racer-remove-constraint-duplicates-table (racer-make-hash-table :test 'equal))
  (possible-subsumees-vector (make-possible-subsumees-vector))
  (expanded-constraints-ind-table (racer-make-hash-table))
  (live-inds-table (racer-make-hash-table))
  (obsolete-inds-table (racer-make-hash-table))
  (label-inds-table (racer-make-hash-table))
  (new-inds-table (racer-make-hash-table))
  (concept-set-mark 0)
  (role-set-mark 0)
  (individual-set-mark 0)
  (constraint-set-mark 0)
  (classification-counter 0)
  (obsolete-eql-tables (make-hash-table))
  (obsolete-equal-tables (make-hash-table))
  (obsolete-equal-concept-tables (make-hash-table))
  (initial-hash-table-size (hash-table-size (make-hash-table)))
  (signatures-equal-table (racer-make-hash-table :test 'equal))
  (partitions-table (racer-make-hash-table))
  (use-less-memory nil)
  (set-vector (make-set-vector))
  (nary-absorption-table nil)
  (inverse-nary-absorption-table nil)
  (domain-qualifications-table nil)
  (select-disjunct-table (make-select-disjunct))
  (object-top-role nil)
  (object-bottom-role nil)
  (datatype-top-role nil)
  (datatype-bottom-role nil)
  (nominal-table (make-hash-table))
  (internal-roles nil)
  (datatype-top-node nil)
  (datatype-bottom-node nil)
  (role-hierarchy-classified-p nil)
  (role-range-concepts nil)
  (el+-transformed-table nil)
  (el+-environment nil)
  (role-characteristics-checked-p nil)
  )

(defun make-set-vector ()
  (make-array 5000
              :element-type '(or racer-set-element null)
              :adjustable t
              :fill-pointer 0
              :initial-element nil))

(defun make-possible-subsumees-vector ()
  (make-array 500
              :element-type '(or concept-node null)
              :adjustable t
              :fill-pointer 0
              :initial-element nil))

(defun tbox-internal-marker-concept (concept-name tbox)
  (gethash concept-name (tbox-internal-marker-concepts tbox)))

(defun (setf tbox-internal-marker-concept) (new-value concept-name tbox)
  (if (null new-value)
    (remhash concept-name (tbox-internal-marker-concepts tbox))
    (setf (gethash concept-name (tbox-internal-marker-concepts tbox)) concept-name)))

