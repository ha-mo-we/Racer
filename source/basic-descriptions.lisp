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
;;;
;;;

(defpersistentclass semantic-entity () ; abstrakt
  ;;; 
  ;;; die textual-description is sozusagen die Basis-Beschreibung! 
  ;;; Bei einer edge-description z.B. die Liste der Rollen-Symbole, als Disjunktion interpretiert
  ;;; nur die description selbst weiss wie die "rohe" textual-description zu interpretieren ist
  ;;; (ob eine textual-descritpion vom Typ Liste z.B. eine Disjunktion oder Konjunktion repraesentiert)
  ;;;
  ((textual-description :accessor textual-description :initarg :textual-description)
   (original-description :accessor original-description :initarg :original-description)
   
   (constructor-sym :reader constructor-sym :initarg :constructor-sym :initform nil)))

(defmethod initialize-description ((descr semantic-entity))
  t)

(defmethod initialize-instance :after ((object semantic-entity) &rest initargs &key dont-initialize-p)
  (declare (ignorable initargs))
  (when (and (slot-boundp object 'textual-description)
             (not (slot-boundp object 'original-description)))
    (setf (original-description object) 
          (textual-description object)))
  (unless dont-initialize-p
    ;;; Idee: hier laeuft der Parser an, um die urspruengliche textual-description zu
    ;;; bearbeiten; original-description memoriert den originalen Wert der Descr. 
    (initialize-description object)))


;;;
;;;
;;;

(defpersistentclass description (semantic-entity)
  ((satisfiable :reader satisfiable :initform :not-tested)
   (inconsistent :reader inconsistent :initform :not-tested)
   (tautological :reader tautological :initform :not-tested)
   (entails :reader entails :initform nil)
   (not-entails :reader not-entails :initform nil)
   (entailed-by :reader entailed-by :initform nil)
   (not-entailed-by :reader not-entailed-by :initform nil)))
   

(defpersistentclass racer-description (description)
  ((racer-package :reader racer-package :initarg :racer-package :initform *package*)))

(defpersistentclass racer-node-description (racer-description racer-concept-mixin) nil)

(defpersistentclass racer-edge-description (racer-description racer-role-mixin) nil)

;;;
;;; f. Graph-Visualizer
;;;

(defmethod get-textual-description ((object semantic-entity))
  (format nil "~A" (textual-description object)))

;;;
;;; Mixins - keine Descriptions!
;;;

(defpersistentclass racer-concept-mixin ()
  ((racer-concept :reader racer-concept)
   (racer-tbox :reader racer-tbox :initarg :racer-tbox :initform nil)))

(defpersistentclass racer-role-mixin ()
  ((racer-role :reader racer-role)
   (racer-tbox :reader racer-tbox :initarg :racer-tbox :initform nil)

   (allow-negated-roles-p :initarg :allow-negated-roles-p :initform nil)))

;;;
;;; 
;;;

(defmethod initialize-description :after ((descr racer-concept-mixin))
  (with-slots (racer-package racer-concept racer-tbox textual-description) descr
    (setf racer-concept 
          (convert-to-racer-concept-expression textual-description racer-package)

          racer-tbox 
          (convert-to-racer-tbox-name racer-tbox racer-package))))

(defmethod initialize-description :after ((descr racer-role-mixin))
  (with-slots (racer-package racer-role racer-tbox textual-description) descr

    (setf racer-role
          (convert-to-racer-role-expression textual-description racer-package)

          racer-tbox
          (convert-to-racer-tbox-name racer-tbox racer-package))))

;;;
;;;
;;;

(defmethod make-description ((class symbol) descr &rest args &key type &allow-other-keys)
  (apply #'make-instance (or type class) 
         :textual-description descr 
         :constructor-sym class 
         :allow-other-keys t 
         args))
