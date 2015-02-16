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

(defconstant +top-object-role-symbol+ '*top-object-role*
  "Used as unique id for symbol-name of the top object role")
(defconstant +bottom-object-role-symbol+ '*bottom-object-role*
  "Used as unique id for symbol-name of the bottom object role")

(defconstant +krss-top-object-role-symbol+ 'top-object-role
  "Used as synonym to *top-object-role*")
(defconstant +krss-bottom-object-role-symbol+ 'bottom-object-role
  "Used as synonym to *bottom-object-role*")

(defconstant +top-datatype-role-symbol+ '*top-datatype-role*
  "Used as unique id for symbol-name of the top datatype role")
(defconstant +bottom-datatype-role-symbol+ '*bottom-datatype-role*
  "Used as unique id for symbol-name of the bottom datatype role")
(defconstant +inv-top-datatype-role-symbol+ '*inv-top-datatype-role*)
(defconstant +inv-bottom-datatype-role-symbol+ '*inv-bottom-datatype-role*)

(defconstant +top-object-role-hash-id+ 4)
(defconstant +bottom-object-role-hash-id+ 5)
(defconstant +top-datatype-role-hash-id+ 6)
(defconstant +bottom-datatype-role-hash-id+ 7)
(defconstant +inv-top-datatype-role-hash-id+ 8)
(defconstant +inv-bottom-datatype-role-hash-id+ 9)

(defparameter *encode-roles-as-transitive* nil)
(defparameter *encode-roles-as-reflexive* nil)

(defstruct (role-node (:include racer-structure-id)
                      (:conc-name role-)
                      (:predicate role-node-p))
  "structure for an internal role or a feature"
  (name nil)
  (transitive-p *encode-roles-as-transitive*)
  (reflexive-p *encode-roles-as-reflexive*)
  (inverse-internal nil)
  (feature-p nil)
  (parents-internal nil)
  (children-internal nil)
  (ancestors-internal nil)
  (has-ancestors-p nil)
  (feature-ancestors nil)
  (has-feature-ancestors-p nil)
  (descendants-internal nil)
  (internal-conjunction-p nil)
  (domain-concept nil)
  (domain-restriction nil)
  (range-concept nil)
  (range-restriction nil)
  (internal-name-p nil)
  (cd-attribute nil)
  (synonyms-internal nil)
  (datatype nil)                        ; t = used as OWL datatype property, 
                                        ; 'integer, 'real, 'string = range
  (annotation-p nil)
  (gci-dependencies nil)
  (inverse-feature-p nil)
  (irreflexive-p nil)
  (asymmetric-p nil)
  (symmetric-p nil)
  (disjoint-roles nil)
  (compositions nil)
  (simple-p t)
  (language-context *dl-empty*)
  (satisfiability-checked-p nil)
  )

(race-inline (role-parent-names role-not-transitive-p))

(defun role-parent-names (role)
  (mapcar #'role-name (role-parents-internal role)))

(defmethod print-object :around ((object role-node) stream)
  (if *print-internals*
    (print-unreadable-object (object stream :type nil :identity t)
      (call-next-method))
    (call-next-method)))

(defmethod print-object ((object role-node) stream)
  (cond
   #+:debug ((role-feature-p object) (format stream "~S!" (decode-role object)))
   #+:debug ((or (role-transitive-p object) (role-compositions object))
             (format stream "~S" (decode-role object))
             (when (role-compositions object)
               (format stream "@"))
             (when (role-transitive-p object)
               (format stream "+")))
   (t (format stream "~S" (decode-role object)))))

(defun role-is-satisfiable (role)
  (let ((domain (role-domain-restriction role)))
    (or (null domain) (not (is-bottom-concept-p domain)))))

(defun decode-role (role)
  (if (role-node-p role)
    (let ((inverse (role-inverse-internal role)))
      (if (and (role-internal-name-p role) inverse)
        (if (role-node-p inverse)
          `(inv ,(role-name inverse))
          `(inv ,inverse))
        (role-name role)))
    role))

(defun role-not-transitive-p (role)
  (not (role-transitive-p role)))

;;; ======================================================================

(declaim (notinline create-internal-and-role))

#-:cmu
(defun create-internal-and-role (tbox role1 role2)
  (declare (ignore tbox role1 role2))
  (if *use-tbox*
    (error "wrong definition called")
    (error "this should never happen")))

;;;===========================================================================

(race-inline (make-top-object-role
              make-bottom-object-role
              is-top-object-role-p
              is-bottom-object-role-p
              is-predefined-role-p))

(defun make-top-object-role ()
  (let ((role (make-role-node :name +top-object-role-symbol+
                              :internal-name-p nil
                              :hash-id +top-object-role-hash-id+
                              :transitive-p t
                              :reflexive-p t
                              :symmetric-p t)))
    (setf (role-inverse-internal role) role)
    (setf (role-synonyms-internal role) (list role))
    role))

(defun make-bottom-object-role ()
  (let ((role (make-role-node :name +bottom-object-role-symbol+
                              :internal-name-p nil
                              :hash-id +bottom-object-role-hash-id+
                              :transitive-p t
                              :reflexive-p nil
                              ; reflexive-p cannot be set to T for unsatisfiable roles
                              ; because otherwise (all r bottom) would expand to bottom
                              :irreflexive-p t
                              :symmetric-p t
                              :asymmetric-p t
                              :satisfiability-checked-p t)))
    (setf (role-inverse-internal role) role)
    (setf (role-synonyms-internal role) (list role))
    role))

(defun make-top-datatype-role ()
  (let ((role (make-role-node :name +top-datatype-role-symbol+
                              :internal-name-p nil
                              :hash-id +top-datatype-role-hash-id+
                              :datatype t
                              :satisfiability-checked-p t))
        (inv-role (make-role-node :name (gensym (symbol-name +inv-top-datatype-role-symbol+))
                                  :internal-name-p t
                                  :hash-id +inv-top-datatype-role-hash-id+
                                  :satisfiability-checked-p t)))
    (setf (role-inverse-internal role) inv-role)
    (setf (role-inverse-internal inv-role) role)
    (setf (role-synonyms-internal role) (list role))
    (setf (role-synonyms-internal inv-role) (list inv-role))
    role))

(defun make-bottom-datatype-role ()
  (let ((role (make-role-node :name +bottom-datatype-role-symbol+
                              :internal-name-p nil
                              :hash-id +bottom-datatype-role-hash-id+
                              :datatype t
                              :satisfiability-checked-p t))
        (inv-role (make-role-node :name (gensym (symbol-name +inv-bottom-datatype-role-symbol+))
                                  :internal-name-p t
                                  :hash-id +inv-bottom-datatype-role-hash-id+
                                  :satisfiability-checked-p t)))
    (setf (role-inverse-internal role) inv-role)
    (setf (role-inverse-internal inv-role) role)
    (setf (role-synonyms-internal role) (list role))
    (setf (role-synonyms-internal inv-role) (list inv-role))
    role))

(race-inline (is-top-object-role-p
              is-bottom-object-role-p
              is-top-datatype-role-p
              is-bottom-datatype-role-p
              is-predefined-role-p
              user-defined-role-transitive-p))

(defun is-top-object-role-p (role)
  (eql (role-hash-id role) +top-object-role-hash-id+))

(defun is-bottom-object-role-p (role)
  (eql (role-hash-id role) +bottom-object-role-hash-id+))

(defun is-top-datatype-role-p (role)
  (eql (role-hash-id role) +top-datatype-role-hash-id+))

(defun is-bottom-datatype-role-p (role)
  (eql (role-hash-id role) +bottom-datatype-role-hash-id+))

(defun is-predefined-role-p (role)
  (let* ((new-role (if (role-internal-name-p role)
                       (or (role-inverse-internal role) role)
                     role))
         (id (role-hash-id new-role)))
    (or (eql id +top-object-role-hash-id+)
        (eql id +bottom-object-role-hash-id+)
        (eql id +top-datatype-role-hash-id+)
        (eql id +bottom-datatype-role-hash-id+))))

(defun remove-top-object-role (role-list)
  (loop for role in role-list
        unless (eql (role-hash-id role) +top-object-role-hash-id+)
        collect role))


;;;===========================================================================

(defun user-defined-role-transitive-p (role)
  (and (role-transitive-p role) (not (is-predefined-role-p role))))

(defun some-user-defined-role-transitive-p (role-list &optional (ignore nil))
  (when role-list
    (loop for role in role-list
          thereis (and (not (eq role ignore))
                       (role-transitive-p role)
                       (not (is-predefined-role-p role))))))

(defun find-user-defined-transitive-role (role-list)
  (when role-list
    (loop for role in role-list
          when (and (role-transitive-p role)
                    (not (is-predefined-role-p role)))
          do (return role))))

(defun user-defined-role-reflexive-p (role)
  (and (role-reflexive-p role) (not (is-predefined-role-p role))))

(defun user-defined-role-reflexive-feature-p (role)
  (and (user-defined-role-reflexive-p role) (role-feature-p role)))
