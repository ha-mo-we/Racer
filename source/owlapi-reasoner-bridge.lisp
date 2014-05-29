;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWLAPI; Base: 10 -*-

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

(in-package :owlapi)

;;;
;;;;  owlapi-rasoner-bridge.lisp
;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   The Original Software is 
;;;   OntoLisp (NOSA): A Semantic Web Framework for OWL 2 and OWLlink in Common Lisp
;;;
;;;   Copyright (c) 2007-2010 Michael Wessel and Racer Systems GmbH & Co. KG 
;;;   All Rights Reserved.
;;;
;;;   Contributor(s): Michael Wessel  (mailto:michael_wessel@gmx.de
;;;                                    mailto:wessel@racer-systems.com) 
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   Purpose: Example of a concrete OWLAPI reasoner bridge (for Racer). 
;;;            Supplied for illustration purposes only. 
;;; 

(declaim (special owl-syntaxes::*owllink-kb-and-reasoner*))

(defmacro with-current-owllink-kb  (&body body)
  `(let ((owl-syntaxes::*owllink-kb-and-reasoner* (owlapi-tbox *cur-reasoner*)))
     ,@body))

;;;
;;; Reasoner Synchronization
;;;

(defun reasoner-tbox-sync (tbox)
  #+:racer-server
  (ensure-knowledge-base-state ':tbox-prepared 
				      (find-tbox tbox)))

(defun reasoner-sync (kb)
  (ensure-knowledge-base-state ':abox-prepared 
				      (find-abox kb)))
  
;;;
;;; Filter
;;;

(defun without-bottom (x)
  (remove-if (lambda (x)
               (find-if #'reasoner-bottom-concept-p 
                        (ensure-list x)))
             x))

(defun without-top (x)
  (remove-if (lambda (x)
               (find-if #'reasoner-top-concept-p
                        (ensure-list x)))
             x))

(defun without-top-and-bottom (x)
  (remove-if (lambda (x)
               (find-if #'(lambda (x) 
                            (or (reasoner-top-concept-p x)
                                (reasoner-bottom-concept-p x)))
                        (ensure-list x)))
             x))

(defun without-top-object-property (x tbox)
  (remove-if (lambda (x)
               (find-if #'(lambda (x) 
                            (reasoner-top-object-property-p x tbox))
                        (ensure-list x)))
             x))

(defun without-bottom-object-property (x tbox)
  (remove-if (lambda (x)
               (find-if #'(lambda (x) 
                            (reasoner-bottom-object-property-p x tbox))
                        (ensure-list x)))
             x))

(defun without-top-and-bottom-object-properties (x tbox)
  (remove-if (lambda (x)
               (find-if #'(lambda (x) 
                            (or (reasoner-top-object-property-p x tbox)
                                (reasoner-bottom-object-property-p x tbox)))
                        (ensure-list x)))
             x))

(defun without-top-data-property (x tbox)
  (remove-if (lambda (x)
               (find-if #'(lambda (x) 
                            (reasoner-top-data-property-p x tbox))
                        (ensure-list x)))
             x))               
                          
(defun without-bottom-data-property (x tbox)
  (remove-if (lambda (x)
               (find-if #'(lambda (x) 
                            (reasoner-bottom-data-property-p x tbox))
                        (ensure-list x)))
             x))               
                             
(defun without-top-and-bottom-data-properties (x tbox)
  (remove-if (lambda (x)
               (find-if #'(lambda (x) 
                            (or (reasoner-top-data-property-p x tbox)
                                (reasoner-bottom-data-property-p x tbox)))
                        (ensure-list x)))
             x))

(defun reasoner-only-object-properties (roles tbox)
  (remove-if #'(lambda (x) 
                 (or (consp x)
                     (eq x '*top-object-role*)
                     (eq x '*bottom-object-role*)                     
                     (eq x '*top-datatype-role*)
                     (eq x '*bottom-datatype-role*)                     
                     (reasoner-data-property-p x tbox)
                     (reasoner-annotation-property-p x tbox)))
             roles))

(defun reasoner-no-data-properties (roles tbox)
  (remove-if #'(lambda (x) 
                 (or (consp x)
                     (eq x '*top-datatype-role*)
                     (eq x '*bottom-datatype-role*)                     
                     (reasoner-data-property-p x tbox)
                     (reasoner-annotation-property-p x tbox)))
             roles))

(defun reasoner-only-data-properties (roles tbox)
  (remove-if #'(lambda (x) 
                 (or (consp x)
                     (eq x '*top-object-role*)
                     (eq x '*bottom-object-role*)                     
                     (eq x '*top-datatype-role*)
                     (eq x '*bottom-datatype-role*)                     
                     (not (or (reasoner-data-property-p x tbox)
                              (reasoner-annotation-property-p x tbox)))))
             roles))

(defun reasoner-no-object-properties (roles tbox)
  (remove-if #'(lambda (x) 
                 (or (consp x)
                     (eq x '*top-object-role*)
                     (eq x '*bottom-object-role*)                     
                     (not (or (reasoner-data-property-p x tbox)
                              (reasoner-annotation-property-p x tbox)))))
             roles))

(defun reasoner-only-annotation-properties (roles tbox)
  (remove-if-not #'(lambda (x) (reasoner-annotation-property-p x tbox))
             roles))

;;;
;;; Basic Tells
;;; 

(defun reasoner-clear-tbox-and-abox (tbox abox)
  (let ((namespaces
         (tbox-namespaces (find-tbox tbox))))

    (when (find-abox abox nil)
      (forget-abox abox))
         
    (when (find-tbox tbox nil)
      (forget-tbox tbox))

    (in-tbox-internal tbox
                             t
                             nil
                             *default-tbox-concept-size*
                             *default-tbox-role-size*)
    
    (in-abox-internal abox tbox t)

    (setf (tbox-namespaces (find-tbox tbox))
          namespaces)))

(defun reasoner-clear-abox (tbox abox)
  (when (find-abox abox nil)
    (forget-abox abox))

  (in-abox-internal abox tbox t))

(defun reasoner-new-tbox-and-abox (tbox abox init)
  (in-tbox-internal tbox
                           (or init 
                               (not (find-tbox tbox nil)))
                           nil
                           *default-tbox-concept-size*
                           *default-tbox-role-size*)
  
  (in-abox-internal abox
                           tbox
                           (or init
                               (not (find-abox abox nil)))))

(defun reasoner-set-current-tbox (tbox)
  (set-current-tbox tbox))

(defun reasoner-set-current-abox (abox)
  (set-current-abox abox))

(defun reasoner-dispose (reasoner)
  (setf *cached-prefixes* nil)

  (when (find-abox (owlapi-abox reasoner) nil)
    (forget-abox (owlapi-abox reasoner)))

  (when (find-tbox (owlapi-tbox reasoner) nil)
    (forget-tbox (owlapi-tbox reasoner)))

  t)

;;;
;;; Concept Axioms 
;;;

(defun reasoner-add-implication (concept-1 concept-2 tbox)
  (add-concept-axiom tbox concept-1 concept-2 :inclusion-p t))

(defun reasoner-add-equation (concept-1 concept-2 tbox)
  (add-concept-axiom tbox concept-1 concept-2 :inclusion-p nil))

(defun reasoner-declare-disjoint (descriptions tbox)
  (declare-disjoint descriptions tbox))

;;;
;;; Role Axioms 
;;; 

(defmacro with-checked-role ((role) &body body)
  `(when (and (not (eq ,role +top-object-role-symbol+))
              (not (eq ,role +krss-top-object-role-symbol+))
              (not (eq ,role +bottom-object-role-symbol+))
              (not (eq ,role +krss-bottom-object-role-symbol+))
              
              (not (eq ,role +top-datatype-role-symbol+))
              (not (eq ,role +bottom-datatype-role-symbol+)))
     ,@body))

(defun reasoner-ensure-role (role tbox) 
  (with-checked-role (role)
    (ensure-role role (find-tbox tbox))))

(defun reasoner-role-is-used-as-datatype-property (role tbox)
  (with-checked-role (role)
    (role-is-used-as-datatype-property role tbox)))

(defun reasoner-role-is-used-as-annotation-property (annotation-role tbox)
  (with-checked-role (annotation-role)
    (role-is-used-as-annotation-property annotation-role tbox)))

(defun reasoner-role-is-transitive (role tbox)
  (with-checked-role (role)
    (role-is-transitive role tbox)))

(defun reasoner-role-is-functional (role tbox)
  (with-checked-role (role)
    (role-is-functional role tbox)))

(defun reasoner-role-is-reflexive (role tbox)
  (with-checked-role (role)
    (role-is-reflexive role tbox)))

(defun reasoner-role-is-irreflexive (role tbox)
  (with-checked-role (role)
    (role-is-irreflexive role tbox)))
  
(defun reasoner-roles-are-disjoint (role1 role2 tbox)
  (with-checked-role (role1)
    (with-checked-role (role2)
      (roles-disjoint-1 role1 role2 tbox))))

(defun reasoner-roles-are-equivalent (role1 role2 tbox)
  (with-checked-role (role1)
    (with-checked-role (role2)
      (roles-equivalent-1 role1 role2 tbox))))

(defun reasoner-inverse-of-role (role1 role2 tbox)
  (with-checked-role (role1)
    (with-checked-role (role2)
      (inverse-of-role role1 role2 tbox))))

(defun reasoner-role-is-asymmetric (role tbox)
  (with-checked-role (role)
    (role-is-asymmetric role tbox)))

(defun reasoner-role-has-parent (role parent tbox)
  (with-checked-role (role)
    (with-checked-role (parent)
      (role-has-parent role parent tbox))))

  
(defun reasoner-register-subproperty (sub-property super-property)
  (let ((tbox (find-tbox (owlapi-tbox *cur-reasoner*))))

    (cond ((and (consp sub-property) ; composition 
                (not (eq (first sub-property) 'inv)))
           
           (dolist (role (append sub-property 
                                 (list super-property)))
             (if (consp role)
                 (reasoner-ensure-role (second role) tbox)
               (reasoner-ensure-role role tbox)))

           (let ((new-subproperties
                  (mapcar #'(lambda (role)
                              (if (consp role) ; inverse? 
                                  (let ((inv-role (gentemp (format nil "~A" role))))
                                    (push inv-role (tbox-internal-roles tbox))
                                    (reasoner-inverse-of-role inv-role (second role) tbox)
                                    inv-role)
                                role))
                          sub-property)))
             (declare-role-axiom new-subproperties super-property tbox)))

          (t 

           (dolist (role (list super-property sub-property))
             (if (consp role)
                 (reasoner-ensure-role (second role) tbox)
               (reasoner-ensure-role role tbox)))

           (when (consp super-property) ; inverse? 
             (let ((inv-role (gentemp (format nil "~A" super-property))))
               (push inv-role (tbox-internal-roles tbox))
               (reasoner-inverse-of-role inv-role (second super-property) tbox)
               (setf super-property inv-role)))

           (when (consp sub-property) ; inverse? 
             (let ((inv-role (gentemp (format nil "~A" sub-property))))
               (push inv-role (tbox-internal-roles tbox))
               (reasoner-inverse-of-role inv-role (second sub-property) tbox)
               (setf sub-property inv-role)))

           (reasoner-role-has-parent sub-property super-property tbox)))))


(defun reasoner-register-equivalent-properties (axiom properties)
  (declare (ignorable axiom))
  (let ((tbox (find-tbox (owlapi-tbox *cur-reasoner*))))
    (mapl #'(lambda (roles)
              (let ((role1 (first roles)))

                (reasoner-ensure-role role1 tbox)
                  
                (dolist (role2 (rest roles))
                  
                  (reasoner-ensure-role role2 tbox)

                  (cond ((and (symbolp role1)
                              (consp role2) 
                              (eq (first role2) 'inv)) 
                         (reasoner-inverse-of-role role1 (second role2) tbox))
                        ((and (symbolp role2)
                              (consp role1) 
                              (eq (first role1) 'inv))
                         (reasoner-inverse-of-role role2 (second role1) tbox))
                        ((and (consp role1) 
                              (eq (first role1) 'inv)
                              (consp role2)
                              (eq (first role2) 'inv))
                         (reasoner-roles-are-equivalent (second role1) (second role2) tbox))
                        (t (reasoner-roles-are-equivalent role1 role2 tbox))))))
          
          properties)))

(defun reasoner-role-has-domain (role domain tbox)
  (with-checked-role (role)
    (role-has-domain role domain tbox nil)))

(defun reasoner-datatype-role-has-range (role range tbox)
  (with-checked-role (role)
    (datatype-role-has-range role range tbox)))

(defun reasoner-declare-object-property-domain (object-property object-property-domain)
  (with-checked-role (object-property)
    (let ((tbox (find-tbox (owlapi-tbox *cur-reasoner*))))
      (declare-datatype-properties object-property-domain)
      (or (reasoner-role-has-domain object-property
                                    object-property-domain
                                    tbox)
          (reasoner-add-implication 
           `(some ,object-property top)
           object-property-domain 
           tbox)))))

(defun reasoner-declare-object-property-range (object-property object-property-range)
  (with-checked-role (object-property)
    (let ((tbox (find-tbox (owlapi-tbox *cur-reasoner*))))
      (declare-datatype-properties object-property-range)
      #| (or (role-has-range ax-property
                        property-domain
                        tbox
                        nil) |# 
      (reasoner-add-implication 'top `(all ,object-property ,object-property-range) tbox))))

;;;
;;; Datatypes 
;;;

(defun reasoner-define-datatype (datatype-name data-range tbox)
  (declare (ignorable datatype-name data-range tbox))
  #+:ignore
  (define-datatype-1 datatype-name data-range tbox))

;;;
;;; ABox 
;;; 

(defun translate-concept (concept)
  (cond ((symbolp concept)
         (cond ((reasoner-bottom-concept-p concept) 'bottom)
               ((reasoner-top-concept-p concept) 'top)
               (t concept)))
        ((consp concept)
         (mapcar #'translate-concept concept))
        (t concept)))


(defun translate-role (role)
  (cond ((symbolp role)
         (cond ((eq role +owlapi-owl-top-object-role+)
                (reasoner-get-object-top-role (owlapi-tbox *cur-reasoner*)))
               ((eq role +owlapi-owl-bottom-object-role+)
                (reasoner-get-object-bottom-role (owlapi-tbox *cur-reasoner*)))
               ((eq role +owlapi-owl-top-data-role+)
                (reasoner-get-datatype-top-role (owlapi-tbox *cur-reasoner*)))
               ((eq role +owlapi-owl-bottom-data-role+)
                (reasoner-get-datatype-bottom-role (owlapi-tbox *cur-reasoner*)))
               (t role)))
        ((consp role)
         (mapcar #'translate-role role))
        (t role)))

;;;
;;;
;;; 

(defun reasoner-add-concept-assertion (abox ind concept)
  (add-concept-assertion abox ind (translate-concept concept)))

(defun reasoner-add-all-different-assertion (abox individuals)
  (add-all-different-assertion abox individuals))

(defun reasoner-add-same-individual-as-assertion (abox i j)
  (add-same-individual-as-assertion abox i j))

(defun reasoner-add-role-assertion (abox subject object role)
  (add-role-assertion abox subject object (translate-role role)))

(defun reasoner-add-negated-role-assertion (abox subject object role)
  (add-negated-role-assertion abox subject object (translate-role role)))

(defun reasoner-add-datatype-role-filler (abox subject data-literal data-property)
  (add-datatype-role-filler abox subject data-literal (translate-role data-property)))

(defun reasoner-add-negative-datatype-role-filler (abox subject data-literal data-property)
  (add-negative-datatype-role-filler abox subject data-literal (translate-role data-property)))


(defun reasoner-process-annotation (entity annotation-role annotation-value abox)
  (if (and (consp annotation-value)
           (member (first annotation-value)
                   '(d-literal)))

      (add-annotation-concept-assertion
       abox
       entity
       `(d-filler ,(translate-role annotation-role) ,annotation-value))

    (add-annotation-role-assertion abox
                                   entity
                                   annotation-value
                                   (translate-role annotation-role))))

;;;
;;; ABox Forget (only have to be implemented if reasoner supports 
;;; incremental retraction of those axioms)
;;; 

(defun reasoner-forget-concept-assertion (abox ind concept)
  (forget-concept-assertion abox ind (translate-concept concept)))

(defun reasoner-forget-all-different-assertion (abox individuals)
  (forget-all-different-assertion abox individuals))
  
(defun reasoner-forget-same-individual-as-assertion (abox i j)
  (forget-same-individual-as-assertion abox i j))

(defun reasoner-forget-datatype-role-filler (abox subject literal role)
  (forget-datatype-role-filler abox subject literal (translate-role role)))

(defun reasoner-forget-negative-datatype-role-filler (abox subject literal role)
  (forget-negative-datatype-role-filler abox subject literal (translate-role role)))

(defun reasoner-forget-role-assertion (abox subject object role)
  (forget-role-assertion abox subject object (translate-role role)))

(defun reasoner-forget-negated-role-assertion (abox subject object role)
  (forget-negated-role-assertion abox
                                 subject
                                 object
                                 (translate-role role)))

;;;
;;; Basic Asks
;;; 

(defun reasoner-current-abox ()
  (current-abox))

(defun reasoner-current-tbox ()
  (current-tbox))

(defun reasoner-get-abox-language (abox)
  (reasoner-sync abox)
  (get-abox-language abox))

(defun reasoner-get-tbox-language (tbox)
  (reasoner-tbox-sync tbox)
  (get-tbox-language tbox))

;;;
;;; Roles
;;; 

(defun reasoner-all-roles (tbox)
  (reasoner-sync tbox)
  (all-roles tbox))

(defun reasoner-object-property-p (role tbox)
  (reasoner-sync tbox)      
  (dolist (abox (associated-aboxes tbox))
    (reasoner-sync abox))

  (or (and (consp role)
           (eq (first role) 'inv)
           (reasoner-object-property-p (second role) tbox))
      (reasoner-top-object-property-p role tbox)
      (reasoner-bottom-object-property-p role tbox)
      (and 
       (and (role-p role tbox)
            (not (role-used-as-datatype-property-p role tbox))
            (not (role-used-as-annotation-property-p role tbox))))))

(defun reasoner-data-property-p (role tbox)
  (reasoner-sync tbox)      
  (dolist (abox (associated-aboxes tbox))
    (reasoner-sync abox))

  (and (not (consp role))
       (or (reasoner-top-data-property-p role tbox)
           (reasoner-bottom-data-property-p role tbox)
           (and (not (reasoner-top-object-property-p role tbox))
                (not (reasoner-bottom-object-property-p role tbox))
                (role-used-as-datatype-property-p role tbox)
                (not (role-used-as-annotation-property-p role tbox))))))

(defun reasoner-annotation-property-p (role tbox)
  (reasoner-sync tbox)      
  (dolist (abox (associated-aboxes tbox))
    (reasoner-sync abox))

  (and (not (consp role))
       (not (reasoner-top-object-property-p role tbox))
       (not (reasoner-bottom-object-property-p role tbox))
       (not (reasoner-top-data-property-p role tbox))
       (not (reasoner-bottom-data-property-p role tbox))
       (role-used-as-annotation-property-p role tbox)))

(defun reasoner-get-object-properties (tbox)
  (reasoner-sync tbox)
  (reasoner-only-object-properties
   (list* +owlapi-owl-top-object-role+
          +owlapi-owl-bottom-object-role+
          (reasoner-all-roles tbox))
   tbox))

(defun reasoner-get-data-properties (tbox)
  (reasoner-sync tbox)
  (reasoner-only-data-properties
   (list* +owlapi-owl-top-data-role+
          +owlapi-owl-bottom-data-role+
          (reasoner-all-roles tbox))
   tbox))

(defun reasoner-get-annotation-properties (tbox)
  (reasoner-sync tbox)
  (reasoner-only-annotation-properties 
   (reasoner-all-roles tbox)
   tbox))

(defun reasoner-get-object-bottom-role (tbox)
  (reasoner-sync tbox)
  (get-object-bottom-role tbox))

(defun reasoner-get-datatype-bottom-role (tbox)
  (reasoner-sync tbox)
  (get-datatype-bottom-role tbox))

(defun reasoner-get-object-top-role (tbox)
  (reasoner-sync tbox)
  (get-object-top-role tbox))

(defun reasoner-get-datatype-top-role (tbox)
  (reasoner-sync tbox)
  (get-datatype-top-role tbox))

(defun reasoner-top-object-property-p (x tbox)
  (reasoner-sync tbox)
  (and (symbolp x)
       (or (eq x +owlapi-owl-top-object-role+)
           (and (not (eq x +owlapi-owl-bottom-object-role+)) 
                (not (eq x +owlapi-owl-top-data-role+)) 
                (not (eq x +owlapi-owl-bottom-data-role+))
                (not (role-used-as-datatype-property-p x tbox))
                (eq x (reasoner-get-object-top-role tbox))))))

(defun reasoner-bottom-object-property-p (x tbox)
  (reasoner-sync tbox)
  (and (symbolp x)
       (or (eq x +owlapi-owl-bottom-object-role+)
           (and (not (eq x +owlapi-owl-top-object-role+)) 
                (not (eq x +owlapi-owl-top-data-role+)) 
                (not (eq x +owlapi-owl-bottom-data-role+))
                (not (role-used-as-datatype-property-p x tbox))
                (or (eq x (reasoner-get-object-bottom-role tbox))
                    (not (role-satisfiable-p x tbox)))))))

(defun reasoner-top-data-property-p (x tbox)
  (reasoner-sync tbox)
  (and (symbolp x)
       (or (eq x +owlapi-owl-top-data-role+)
           (and (not (eq x +owlapi-owl-bottom-data-role+)) 
                (not (eq x +owlapi-owl-bottom-object-role+)) 
                (not (eq x +owlapi-owl-top-object-role+))
                (role-used-as-datatype-property-p x tbox)
                (eq x (reasoner-get-datatype-top-role tbox))))))

(defun reasoner-bottom-data-property-p (x tbox)
  (reasoner-sync tbox)
  (and (symbolp x)
       (or (eq x +owlapi-owl-bottom-data-role+)
           (and (not (eq x +owlapi-owl-top-data-role+)) 
                (not (eq x +owlapi-owl-top-object-role+)) 
                (not (eq x +owlapi-owl-bottom-object-role+))
                (role-used-as-datatype-property-p x tbox)
                (or (eq x (reasoner-get-datatype-bottom-role tbox))
                    (not (role-satisfiable-p x tbox)))))))

(defun reasoner-get-disjoint-object-properties (role tbox)
  (reasoner-sync tbox)
  (loop as r in (reasoner-get-object-properties tbox)
        when (reasoner-role-disjoint-p r role tbox)
        collect r))

(defun reasoner-get-disjoint-data-properties (role tbox)
  (reasoner-sync tbox)
  (loop as r in (reasoner-get-data-properties tbox)
        when (reasoner-role-disjoint-p r role tbox)
        collect r))

(defun reasoner-equivalent-roles (role tbox  &optional remove-self-p)
  (reasoner-sync tbox)
  (if remove-self-p
      (remove role 
              (atomic-role-synonyms role tbox))
    (atomic-role-synonyms role tbox)))

(defun reasoner-get-role-synonyms (role tbox)
  (reasoner-sync tbox)
  (when role
    (if (consp role)
        (mapcar #'(lambda (r) 
                    (reasoner-equivalent-roles r tbox))
                role)
      (reasoner-equivalent-roles role tbox))))

(defun reasoner-role-parents (role tbox)
  (reasoner-sync tbox)
  (reasoner-get-role-synonyms
   (atomic-role-parents role tbox)
   tbox))

(defun reasoner-atomic-role-parents (role tbox)
  ;;; for OWLlink
  (reasoner-sync tbox)
  (let ((parents  
         (atomic-role-parents2 role tbox)))
    (append (remove-if #'consp parents)
            (reduce #'append
                    (mapcar #'(lambda (x) 
                                (reasoner-atomic-role-parents x tbox))
                            (remove-if-not #'consp parents))))))
   
(defun reasoner-role-children (role tbox)
  (reasoner-sync tbox)
  (reasoner-get-role-synonyms
   (atomic-role-children role tbox)
   tbox))

(defun reasoner-atomic-role-children (role tbox)
  ;;; for OWLlink
  (reasoner-sync tbox)
  (let ((children
         (atomic-role-children2 role tbox)))
    (append (remove-if #'consp children)
            (reduce #'append
                    (mapcar #'(lambda (x) 
                                (reasoner-atomic-role-children x tbox))
                            (remove-if-not #'consp children))))))                

(defun reasoner-role-ancestors (role tbox &optional remove-self-p)
  (reasoner-sync tbox)
  (reasoner-get-role-synonyms
   (if remove-self-p 
       (remove role 
               (atomic-role-ancestors role tbox))
     (atomic-role-ancestors role tbox))
   tbox))

(defun reasoner-atomic-role-ancestors (role tbox)
  ;;; for OWLlink
  (reasoner-sync tbox)
  (atomic-role-ancestors2 role tbox))

(defun reasoner-role-descendants (role tbox &optional remove-self-p)
  (reasoner-sync tbox)
  (reasoner-get-role-synonyms
   (if remove-self-p 
       (remove role 
               (atomic-role-descendants role tbox))
     (atomic-role-descendants role tbox))
   tbox))

(defun reasoner-atomic-role-descendants (role tbox)
  ;;; for OWLlink
  (reasoner-sync tbox)
  (atomic-role-descendants2 role tbox))

(defun get-concept-synonyms (concept &optional include-complex-concepts-p)
  ;;; needs lists of concepts! only for role domain / role range
  (when concept
    (if (consp concept)
        (mapcar #'(lambda (c) 
                    (if (and (consp c) 
			     include-complex-concepts-p)
			(cons c 
			      (atomic-concept-synonyms c (owlapi-tbox *cur-reasoner*)))
		      (atomic-concept-synonyms c (owlapi-tbox *cur-reasoner*))))
                concept)
      (atomic-concept-synonyms concept (owlapi-tbox *cur-reasoner*)))))

(defun reasoner-get-role-domain (role owlapi-hacking-mode tbox)
  (reasoner-sync tbox)
  (case owlapi-hacking-mode
    (0 (atomic-role-domain role tbox))
    (1 (first (get-concept-synonyms (list (atomic-role-domain role tbox)) t)))
    (2 (get-concept-synonyms (list (atomic-role-domain role tbox)) t))
    (otherwise 
     (or 
      (atomic-concept-synonyms `(some ,role top) tbox)
      (atomic-concept-parents `(some ,role top) tbox)))))

(defun reasoner-get-role-range (role owlapi-hacking-mode tbox)
  (reasoner-sync tbox)
  (if (role-used-as-datatype-property-p role tbox)
      (ensure-list (datatype-role-range role tbox))
	      
    (case owlapi-hacking-mode 
      (0 (atomic-role-range role tbox))
      (1 (first (get-concept-synonyms (list (atomic-role-range role tbox)) t)))
      (2 (get-concept-synonyms (list (atomic-role-range role tbox)) t))
      (otherwise 
       (or 
        (atomic-concept-synonyms `(some (inv ,role) top) tbox)
        (atomic-concept-parents `(some (inv ,role) top) tbox))))))

(defun reasoner-role-functional-p (role tbox)
  (reasoner-sync tbox)
  (if (consp role) 
      (reasoner-role-inverse-functional-p (second role) tbox)
    (cond ((reasoner-bottom-object-property-p role tbox)
           t)
          ((reasoner-bottom-data-property-p role tbox)
           t)
          ((reasoner-top-object-property-p role tbox)
           nil)
          ((reasoner-top-data-property-p role tbox)
           nil)
          (t
           (feature-p role tbox)))))

(defun reasoner-role-satisfiable-p (role tbox)
  (reasoner-sync tbox)
  (cond ((reasoner-bottom-object-property-p role tbox)
         nil)
        ((reasoner-bottom-data-property-p role tbox)
         nil)
        (t (role-satisfiable-p role tbox))))

(defun reasoner-role-equivalent-p (role-1 role-2 tbox)
  (reasoner-sync tbox)
  (cond ((reasoner-top-object-property-p role-1 tbox)
         (reasoner-top-object-property-p role-2 tbox))
        ((reasoner-bottom-object-property-p role-1 tbox)
         (reasoner-bottom-object-property-p role-2 tbox))
        ((reasoner-top-data-property-p role-1 tbox)
         (reasoner-top-data-property-p role-2 tbox))
        ((reasoner-bottom-data-property-p role-1 tbox)
         (reasoner-bottom-data-property-p role-2 tbox))
        ((or (and (reasoner-object-property-p role-1 tbox)
                  (reasoner-object-property-p role-2 tbox))
             (and (reasoner-data-property-p role-1 tbox)
                  (reasoner-data-property-p role-2 tbox))
             (and (reasoner-annotation-property-p role-1 tbox)
                  (reasoner-annotation-property-p role-2 tbox)))
         (let ((role-1 
                (cond ((reasoner-top-object-property-p role-1 tbox)
                       (reasoner-get-object-top-role tbox))
                      ((reasoner-top-data-property-p role-1 tbox)
                       (reasoner-get-datatype-top-role tbox))
                      ((not (reasoner-role-satisfiable-p role-1 tbox))
                       (reasoner-get-object-bottom-role tbox))
                      (t
                       role-1)))
               (role-2
                (cond ((reasoner-top-object-property-p role-2 tbox)
                       (reasoner-get-object-top-role tbox))
                      ((reasoner-top-data-property-p role-2 tbox)
                       (reasoner-get-datatype-top-role tbox))
                      ((not (reasoner-role-satisfiable-p role-2 tbox))
                       (reasoner-get-object-bottom-role tbox))
                      (t
                       role-2))))
           (role-equivalent-p role-1 role-2 tbox)))
        (t nil)))

(defun reasoner-role-disjoint-p (role-1 role-2 tbox)
  (reasoner-sync tbox)
  (cond ((not
          (or (and (reasoner-object-property-p role-1 tbox)
                   (reasoner-object-property-p role-2 tbox))
              (and (reasoner-data-property-p role-1 tbox)
                   (reasoner-data-property-p role-2 tbox))
              (and (reasoner-annotation-property-p role-1 tbox)
                   (reasoner-annotation-property-p role-2 tbox))))
         t)
        ((or (reasoner-bottom-object-property-p role-1 tbox)
             (reasoner-bottom-object-property-p role-2 tbox))
         t)
        ((or (reasoner-bottom-data-property-p role-1 tbox)
             (reasoner-bottom-data-property-p role-2 tbox))
         t)
        (t 
         (let ((role-1 
                (cond ((reasoner-top-object-property-p role-1 tbox)
                       (reasoner-get-object-top-role tbox))
                      ((reasoner-top-data-property-p role-1 tbox)
                       (reasoner-get-datatype-top-role tbox))
                      ((not (reasoner-role-satisfiable-p role-1 tbox))
                       (reasoner-get-object-bottom-role tbox))
                      (t
                       role-1)))
               (role-2
                (cond ((reasoner-top-object-property-p role-2 tbox)
                       (reasoner-get-object-top-role tbox))
                      ((reasoner-top-data-property-p role-2 tbox)
                       (reasoner-get-datatype-top-role tbox))
                      ((not (reasoner-role-satisfiable-p role-2 tbox))
                       (reasoner-get-object-bottom-role tbox))
                      (t
                       role-2))))
           (role-disjoint-p role-1 role-2 tbox)))))

(defun reasoner-inverse-roles (role tbox)
  (reasoner-sync tbox)
  (let ((inverse
         (when (role-p role tbox)
           (atomic-role-inverse role tbox))))
      
    (if (consp inverse)
        nil
      (reasoner-get-role-synonyms (list inverse) tbox))))

(defun reasoner-role-inverse-functional-p (role tbox)
  (reasoner-sync tbox)
  (if (consp role) 
      (reasoner-role-functional-p (second role) tbox)
    (cond ((reasoner-bottom-object-property-p role tbox)
           t)
          ((reasoner-bottom-data-property-p role tbox)
           t)
          ((reasoner-top-object-property-p role tbox)
           nil)
          ((reasoner-top-data-property-p role tbox)
           nil)
          (t
           (reasoner-inverse-feature-p role tbox)))))

(defun reasoner-inverse-feature-p (role tbox)
  (reasoner-sync tbox)
  (inverse-feature-p role tbox))

(defun reasoner-role-symmetric-p (role tbox)
  (reasoner-sync tbox)
  (cond ((reasoner-bottom-object-property-p role tbox)
         t)
        ((reasoner-bottom-data-property-p role tbox)
         t)
        ((reasoner-top-object-property-p role tbox)
         t)
        ((reasoner-top-data-property-p role tbox)
         t)
        (t
         (symmetric-p role tbox))))

(defun reasoner-role-transitive-p (role tbox)
  (reasoner-sync tbox)
  (cond ((reasoner-bottom-object-property-p role tbox)
         t)
        ((reasoner-bottom-data-property-p role tbox)
         t)
        ((reasoner-top-object-property-p role tbox)
         t)
        ((reasoner-top-data-property-p role tbox)
         t)
        (t
         (transitive-p role tbox))))

(defun reasoner-role-reflexive-p (role tbox)
  (reasoner-sync tbox)
  (cond ((reasoner-bottom-object-property-p role tbox)
         t)
        ((reasoner-bottom-data-property-p role tbox)
         t)
        ((reasoner-top-object-property-p role tbox)
         t)
        ((reasoner-top-data-property-p role tbox)
         t)
        (t
         (reflexive-p role tbox))))

(defun reasoner-role-irreflexive-p (role tbox)
  (reasoner-sync tbox)
  (cond ((reasoner-bottom-object-property-p role tbox)
         t)
        ((reasoner-bottom-data-property-p role tbox)
         t)
        ((reasoner-top-object-property-p role tbox)
         nil)
        ((reasoner-top-data-property-p role tbox)
         nil)
        (t
         (irreflexive-p role tbox))))

(defun reasoner-role-asymmetric-p (role tbox)
  (reasoner-sync tbox)
  (cond ((reasoner-bottom-object-property-p role tbox)
         t)
        ((reasoner-bottom-data-property-p role tbox)
         t)
        ((reasoner-top-object-property-p role tbox)
         nil)
        ((reasoner-top-data-property-p role tbox)
         nil)
        (t
         (asymmetric-p role tbox))))

(defun reasoner-role-subsumes-p (role-1 role-2 tbox)
  (reasoner-sync tbox)
  (cond ((reasoner-bottom-object-property-p role-2 tbox)
         nil)
        
        ((reasoner-bottom-data-property-p role-2 tbox)
         nil)

        ((reasoner-top-object-property-p role-1 tbox)
         t)
        
        ((reasoner-top-data-property-p role-1 tbox)
         nil)

        (t (role-subsumes-p role-1 role-2 tbox))))

;;;
;;; Concepts 
;;;        

(defun reasoner-all-atomic-concepts (tbox)
  (reasoner-sync tbox)
  (all-atomic-concepts tbox))

(defun reasoner-bottom-concept-p (x) 
  (or 
   (eq x 'bottom)
   (eq x '*bottom*)
   (eq x +owlapi-owl-nothing+)))

(defun reasoner-top-concept-p (x)
  (or 
   (eq x 'top)
   (eq x '*top*)
   (eq x +owlapi-owl-thing+)))

;;;
;;; ABox 
;;;

(defun reasoner-all-individuals (abox)
  (reasoner-sync abox)
  (all-individuals abox))

;;;
;;; KB Consistency Queries 
;;;

(defun reasoner-abox-coherent-p (abox)
  (reasoner-sync abox)
  (slot-value 
   (find-abox abox)
   'coherent-p))

(defun reasoner-abox-consistent-p (abox)
  ;; (abox-coherence-checked-p (find-abox 'racer-user::|OWLAPI1|))
  (reasoner-sync abox)
  (abox-consistent-p abox))

(defun reasoner-kb-is-consistent-p (tbox abox &optional (force-p t))
  (reasoner-sync abox)
  (and (not
        (member 'top
                (atomic-concept-synonyms 'bottom tbox)))
       (cond (force-p
	      (reasoner-abox-consistent-p abox))
	     (t
	      ;; ignore the ABox consistency unless force-p = t 
	      ;; (default) 
	      ;; 
	      ;; Useful for P4 adapter, if "Only Taxonomy" 
	      ;; checkbox is set 
	      (reasoner-abox-coherent-p abox)
	      t))))

(defun reasoner-tbox-classified-p (tbox)
  (reasoner-sync tbox)
  (tbox-classified-p tbox))

(defun reasoner-abox-realized-p (abox)
  (reasoner-sync abox)
  (abox-realized-p abox))

(defun reasoner-classify (tbox)
  (reasoner-sync tbox)
  (taxonomy tbox)
  t)

(defun reasoner-taxonomy (tbox)
  (reasoner-sync tbox)
  (taxonomy tbox))

(defun reasoner-realize (abox)
  (reasoner-sync abox)
  (realize-abox abox)
  t)

;;;
;;; TBox / Concept Asks  
;;;

(defun reasoner-concept-p (concept tbox)
  (reasoner-sync tbox)      
  (concept-p concept tbox))

(defun reasoner-concept-satisfiable-p (concept tbox)
  (reasoner-sync tbox)
  (concept-satisfiable-p concept tbox))

(defun reasoner-concepts-disjoint-p (x y tbox)
  (reasoner-sync tbox)
  (concept-disjoint-p x y tbox))

(defun reasoner-concepts-equivalent-p (clsD clsC tbox)
  (reasoner-sync tbox)
  (concept-equivalent-p clsC clsD tbox))

(defun reasoner-concept-subsumes-p (clsD clsC tbox)
  (reasoner-sync tbox)
  (concept-subsumes-p clsD clsC tbox))

(defun reasoner-concept-parents (cls tbox)
  (reasoner-sync tbox)
  (atomic-concept-parents cls tbox))

(defun reasoner-concept-ancestors (cls tbox)
  (reasoner-sync tbox)
  (atomic-concept-ancestors cls tbox))

(defun reasoner-concept-children (cls tbox)
  (reasoner-sync tbox)
  (atomic-concept-children cls tbox))

(defun reasoner-concept-descendants (cls tbox)
  (reasoner-sync tbox)
  (atomic-concept-descendants cls tbox))

(defun reasoner-concept-synonyms (concept tbox)
  (reasoner-sync tbox)
  (cond ((member concept (list +owlapi-owl-thing+ 'top '*top*))
         (cons +owlapi-owl-thing+ 
               (without-top
                (atomic-concept-synonyms 'top tbox))))

        ((member concept (list +owlapi-owl-nothing+ 'bottom '*bottom*))
         (cons +owlapi-owl-nothing+
               (without-bottom
                (atomic-concept-synonyms 'bottom tbox))))

        (t 
         (let* ((res
                 (atomic-concept-synonyms concept tbox))
                (res2 
                 (without-top-and-bottom res)))
           
           (cond ((member 'top res)
                  (cons +owlapi-owl-thing+ res2))

                 ((member 'bottom res)
                  (cons +owlapi-owl-nothing+ res2))

                 (t res2))))))

(defun reasoner-atomic-concept-synonyms (concept tbox)
  ;;; without owl-thing etc. 
  (reasoner-sync tbox)
  (atomic-concept-synonyms concept tbox))

(defun reasoner-get-disjoint-concepts (concept tbox)
  (reasoner-sync tbox)
  (loop as c in (reasoner-all-atomic-concepts tbox)
        when (reasoner-concepts-disjoint-p c concept tbox)
        collect c))

(defun reasoner-equivalent-concepts (cls tbox)
  (reasoner-sync tbox)
  (atomic-concept-synonyms cls tbox))

(defun reasoner-inconsistent-concepts (tbox)
  (reasoner-sync tbox)
  (atomic-concept-synonyms 'bottom tbox))

;;;
;;; ABox Asks 
;;; 

(defmacro with-consistent-abox ((abox) &body body)
  `(progn 
     (reasoner-sync ,abox)
     (cond ((abox-consistent-p ,abox)
            ,@body)
           (t :inconsistent))))

(defun reasoner-individual-p (ind abox)
  (reasoner-sync abox)
  (individual-p ind abox))

(defun reasoner-individual-synonyms (ind abox)
  (with-consistent-abox (abox)
    (retrieve-individual-synonyms ind nil abox)))

(defun reasoner-individual-antonyms (ind abox)
  (with-consistent-abox (abox)
    (retrieve-individual-antonyms ind nil abox)))

(defun reasoner-get-types (ind abox direct)
  (reasoner-sync abox)
  (when (reasoner-abox-consistent-p abox)
    (if direct
        (most-specific-instantiators ind abox)
      (instantiators ind abox))))

(defun reasoner-get-instances (class abox direct)
  (with-consistent-abox (abox)
    (let ((instances 
           (retrieve-concept-instances class abox)))
      (if direct
          (let ((children (atomic-concept-children class (associated-tbox abox))))
            (remove-if #'(lambda (ind) 
                           (some #'(lambda (subclass)
                                     (reasoner-instance-of-p 
                                      ind (first subclass) 
                                      abox
                                      nil))
                                 children))
                       instances))
        instances))))

(defun reasoner-instance-of-p (ind type abox direct)
  (with-consistent-abox (abox)
    (if direct
        (let ((children (atomic-concept-children type (associated-tbox abox))))
          (not (some #'(lambda (subclass)
                         (individual-instance-p ind (first subclass) abox))
                     children)))
      (individual-instance-p ind type abox))))

(defun reasoner-get-individual-successors (source abox &rest args)
  (with-consistent-abox (abox)
    (apply #'get-individual-successors 
           source
           :abox abox
           args)))

(defun reasoner-get-individual-datatype-fillers (source abox)
  (with-consistent-abox (abox)
    (get-individual-datatype-fillers source abox)))

(defun reasoner-individuals-related-p (subject object property abox)
  (with-consistent-abox (abox)
    (individuals-related-p subject  
                           object 
                           property
                           abox)))

(defun reasoner-individual-has-data-filler-p (subject object property abox)
  (with-consistent-abox (abox)
    (when (member object   
                  (retrieve-individual-told-datatype-fillers subject 
                                                             property
                                                             nil
                                                             abox
                                                             t)
                  :test #'equalp)
      t)))

(defun reasoner-retrieve-individual-filled-roles (source target abox &rest args)
  (with-consistent-abox (abox)
    (apply #'retrieve-individual-filled-roles 
           source target abox
           args)))

(defun reasoner-retrieve-individual-fillers (ind role abox)
  (with-consistent-abox (abox)
    (retrieve-individual-fillers ind role abox)))

(defun reasoner-retrieve-individual-predecessors (ind role abox)
  (with-consistent-abox (abox)
    (retrieve-individual-predecessors ind role abox)))

(defun reasoner-retrieve-individual-told-datatype-fillers (ind property abox)
  (with-consistent-abox (abox)
    (retrieve-individual-told-datatype-fillers ind property nil abox t)))

(defun reasoner-get-synonym-individuals (ind abox)
  (with-consistent-abox (abox)
    (remove ind 
            (retrieve-individual-synonyms ind nil abox))))

(defun reasoner-synonym-individuals-p (i j abox)
  (with-consistent-abox (abox)
    (when (member i 
                  (retrieve-individual-synonyms j nil abox))
      t)))

(defun reasoner-get-antonym-individuals (ind abox)
  (with-consistent-abox (abox)
    (retrieve-individual-antonyms ind nil (owlapi-abox *cur-reasoner*))))

(defun reasoner-antonym-individuals-p (i j abox)
  (with-consistent-abox (abox)
    (when (member i (retrieve-individual-antonyms j nil (owlapi-abox *cur-reasoner*)))
      t)))

;;;
;;; Namespaces etc. 
;;;

(defun reasoner-get-default-namespace-prefix (tbox)
  (get-namespace-prefix tbox))

(defun reasoner-register-prefix (prefix namespace tbox)
  (setf *cached-prefixes* nil)
  (when (is-default-prefix-p prefix)
    (add-to-tbox-namespaces (find-tbox tbox)
                                   (list (cons nil namespace)))))

(defun reasoner-remove-prefix (prefix tbox)
  (declare (ignorable prefix tbox))
  (setf *cached-prefixes* nil))

(defun reasoner-get-prefixes (&optional (tbox (current-tbox)) (ask-owlapi-p t))
  (get-prefixes tbox ask-owlapi-p))

(defun reasoner-reset-prefix-cache ()
  (reset-prefix-cache))
 
(defun reasoner-delete-prefix-mappings ()
  (delete-prefix-mappings))

;;;
;;; Axiom Loading
;;; 

(defmethod load-axiom ((axiom |OWLDatatypeDefinitionAxiom|) &key ontology) 
  (declare (ignorable ontology))
  )

#+:ignore
(defmethod load-axiom ((axiom |OWLDatatypeDefinitionAxiom|) &key ontology) 
  (declare (ignorable ontology))
  (with-slots (datatype-name data-range) axiom
    (let ((tbox (find-tbox (owlapi-tbox *cur-reasoner*))))
      (reasoner-define-datatype datatype-name data-range tbox))))

(defmethod load-axiom ((axiom |OWLSubAnnotationPropertyOfAxiom|) &key ontology) 
  (declare (ignorable ontology))

  (with-slots (annotation-sub-property annotation-super-property) axiom
    (reasoner-register-subproperty annotation-sub-property annotation-super-property)))

(defmethod load-axiom ((axiom |OWLAnnotationPropertyDomainAxiom|) &key ontology) 
  (declare (ignorable ontology))

  (with-slots (annotation-property1 annotation-property-domain) axiom
    (reasoner-declare-object-property-domain annotation-property1 annotation-property-domain)))

(defmethod load-axiom ((axiom |OWLAnnotationPropertyRangeAxiom|) &key ontology) 
  (declare (ignorable ontology))

  (with-slots (annotation-property2 annotation-property-range) axiom
    (reasoner-declare-object-property-range annotation-property2 annotation-property-range)))

(defmethod load-axiom ((axiom |OWLDeclarationAxiom|) &key ontology)
  (declare (ignorable ontology))
  (let ((case-insensitive (eq 'Foo 'foo)))

    (flet ((find-key (key spec)
             (if case-insensitive
                 (let ((key-name (symbol-name key)))
                   (if (consp spec)
                       (loop for elem in spec
                             when (or (eq key elem)
                                      (when (symbolp elem)
                                        (string-equal key-name (symbol-name elem))))
                             do (return elem))
                     (or (eq key spec)
                         (when (symbolp spec)
                           (string-equal key-name (symbol-name spec))))))
               (if (consp spec)
                   (find key spec)
                 (eq key spec)))))

      (let ((tbox (find-tbox (owlapi-tbox *cur-reasoner*))))
        (with-slots (entity) axiom
          (cond ((find-key '|OWLClass| (first entity))
                 (reasoner-add-implication (second entity) 'top tbox))
                ((find-key '|Class| (first entity))
                 (reasoner-add-implication (second entity) 'top tbox))
                ((find-key '|ObjectProperty| (first entity))
                 (reasoner-ensure-role (second entity) tbox))
                ((find-key '|DataProperty| (first entity))
                 (reasoner-role-is-used-as-datatype-property (second entity) tbox))
                ((find-key '|AnnotationProperty| (first entity))
                 (reasoner-role-is-used-as-annotation-property (second entity) tbox))
                ((find-key '|Individual| (first entity))
                 (reasoner-add-concept-assertion (owlapi-abox *cur-reasoner*) 
                                                 (second entity) 'top))
                ((find-key '|NamedIndividual| (first entity))
                 (reasoner-add-concept-assertion (owlapi-abox *cur-reasoner*) (second entity)
                                                 'top))
                ((find-key '|Datatype| (first entity)))
                (t (error "Key ~A unknown." (first entity)))))))))

(defmethod load-axiom ((axiom |OWLDisjointClassesAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (descriptions) axiom
    (mapc #'declare-datatype-properties descriptions)
    (mapc #'(lambda (x) (register-referenced-concept x ontology)) descriptions)
    
    (reasoner-declare-disjoint descriptions (owlapi-tbox *cur-reasoner*))))

(defmethod load-axiom ((axiom |OWLDisjointUnionAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (descriptions) axiom
    (reasoner-add-equation  (first descriptions)
                   `(or ,@(rest descriptions))
                   (owlapi-tbox *cur-reasoner*))

    (reasoner-declare-disjoint (rest descriptions) (owlapi-tbox *cur-reasoner*))))

(defmethod load-axiom ((axiom |OWLEquivalentClassesAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (descriptions) axiom
    (mapc #'(lambda (first second)
              (reasoner-add-equation first second (owlapi-tbox *cur-reasoner*)))
          descriptions
          (rest descriptions))))

(defmethod load-axiom ((axiom |OWLSubClassAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (descriptions) axiom
    (reasoner-add-implication 
     (first descriptions)
     (second descriptions)
     (owlapi-tbox *cur-reasoner*))))

(defmethod load-axiom ((axiom |OWLClassAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (ax-individual description) axiom
    (reasoner-add-concept-assertion (owlapi-abox *cur-reasoner*)
                           ax-individual
                           description)))

(defmethod load-axiom ((axiom |OWLDifferentIndividualsAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (individuals) axiom
    (reasoner-add-all-different-assertion (owlapi-abox *cur-reasoner*) individuals)))


(defmethod load-axiom ((axiom |OWLSameIndividualsAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (individuals) axiom
    (mapl #'(lambda (inds)
              (let ((i (first inds)))
                (dolist (j (rest inds))
                  (reasoner-add-same-individual-as-assertion (owlapi-abox *cur-reasoner*) i j))))
          individuals)))

(defmethod load-axiom  ((axiom |OWLDataPropertyAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (subject rel-data-property data-literal) axiom
    (role-is-used-as-datatype-property rel-data-property (owlapi-tbox *cur-reasoner*))
    (reasoner-add-datatype-role-filler
     (owlapi-abox *cur-reasoner*) subject data-literal rel-data-property)))

(defmethod load-axiom  ((axiom |OWLNegativeDataPropertyAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (subject rel-data-property data-literal) axiom
    (reasoner-add-negative-datatype-role-filler
     (owlapi-abox *cur-reasoner*) subject data-literal rel-data-property)))

(defmethod load-axiom ((axiom |OWLObjectPropertyAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (subject rel-object-property object) axiom
    (reasoner-add-role-assertion (owlapi-abox *cur-reasoner*) subject object rel-object-property)))

(defmethod load-axiom ((axiom |OWLNegativeObjectPropertyAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))
  (with-slots (subject rel-object-property object) axiom
    
    (reasoner-add-concept-assertion (owlapi-abox *cur-reasoner*)
                                    subject 'top)

    (reasoner-add-concept-assertion (owlapi-abox *cur-reasoner*)
                                    object 'top)
    
    (reasoner-add-negated-role-assertion (owlapi-abox *cur-reasoner*)
                                         subject
                                         object
                                         rel-object-property)))

(defmethod load-axiom ((axiom |OWLFunctionalDataPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (data-property) axiom
    (reasoner-role-is-used-as-datatype-property data-property (owlapi-tbox *cur-reasoner*))
    (reasoner-role-is-functional data-property (owlapi-tbox *cur-reasoner*))))

(defmethod load-axiom ((axiom |OWLFunctionalObjectPropertyAxiom|) &key ontology) 
  (declare (ignorable ontology))

  (with-slots (object-property) axiom
    (reasoner-role-is-functional object-property (owlapi-tbox *cur-reasoner*))))

(defmethod load-axiom ((axiom |OWLDisjointDataPropertiesAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (data-properties) axiom

  (mapl #'(lambda (roles)
              (let ((role1 (first roles)))
              (dolist (role2 (rest roles))
                (reasoner-roles-are-disjoint role1 role2
                                             (owlapi-tbox *cur-reasoner*)))))
        data-properties)))

(defmethod load-axiom ((axiom |OWLDisjointObjectPropertiesAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-properties) axiom

  (mapl #'(lambda (roles)
              (let ((role1 (first roles)))
              (dolist (role2 (rest roles))
                (reasoner-roles-are-disjoint role1 role2
                                             (owlapi-tbox *cur-reasoner*)))))
        object-properties)))

(defmethod load-axiom ((axiom |OWLEquivalentDataPropertiesAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (data-properties) axiom
    (mapc #'(lambda (r) 
              (reasoner-role-is-used-as-datatype-property r (owlapi-tbox *cur-reasoner*)))
          data-properties)
    (reasoner-register-equivalent-properties axiom data-properties)))

(defmethod load-axiom ((axiom |OWLEquivalentObjectPropertiesAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-properties) axiom
    (reasoner-register-equivalent-properties axiom object-properties)))

(defmethod load-axiom ((axiom |OWLInverseObjectPropertiesAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-properties) axiom
    (let* ((tbox (find-tbox (owlapi-tbox *cur-reasoner*)))
           (r1 (first object-properties))
           (r2 (second object-properties)))

      (register-referenced-object-property r1 ontology)
      (register-referenced-object-property r2 ontology)
      
      (if (and (consp r1) (consp r2))
          (reasoner-inverse-of-role (second r1) (second r2) tbox)
        (reasoner-inverse-of-role r1 r2 tbox)))))

(defmethod load-axiom ((axiom |OWLObjectPropertyChainSubPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property-chain object-property) axiom
    (reasoner-register-subproperty object-property-chain object-property)))

(defmethod load-axiom ((axiom |OWLAsymmetricObjectPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property) axiom
    (let ((tbox (find-tbox (owlapi-tbox *cur-reasoner*))))
      (reasoner-role-is-asymmetric object-property tbox))))

(defmethod load-axiom ((axiom |OWLInverseFunctionalObjectPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property) axiom
    (let ((tbox (find-tbox (owlapi-tbox *cur-reasoner*))))
      (reasoner-add-implication 'top `(at-most 1 (inv ,object-property)) tbox))))

(defmethod load-axiom ((axiom |OWLIrreflexiveObjectPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property) axiom
    (let ((tbox (find-tbox (owlapi-tbox *cur-reasoner*))))
      (reasoner-role-is-irreflexive object-property tbox))))

(defmethod load-axiom ((axiom |OWLReflexiveObjectPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property) axiom
    (let ((tbox (find-tbox (owlapi-tbox *cur-reasoner*))))
      (reasoner-role-is-reflexive object-property tbox))))

(defmethod load-axiom ((axiom |OWLSymmetricObjectPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property) axiom
    (let ((tbox (find-tbox (owlapi-tbox *cur-reasoner*))))
      (reasoner-inverse-of-role object-property object-property tbox))))

(defmethod load-axiom ((axiom |OWLTransitiveObjectPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property) axiom
    (let ((tbox (find-tbox (owlapi-tbox *cur-reasoner*))))
      (reasoner-role-is-transitive object-property tbox))))

(defmethod load-axiom ((axiom |OWLDataSubPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (sub-data-property data-property) axiom

    (reasoner-role-is-used-as-datatype-property sub-data-property (owlapi-tbox *cur-reasoner*))
    (reasoner-role-is-used-as-datatype-property data-property (owlapi-tbox *cur-reasoner*))
    
    (reasoner-register-subproperty sub-data-property data-property)))

(defmethod load-axiom ((axiom |OWLObjectSubPropertyAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (sub-object-property object-property) axiom
    (reasoner-register-subproperty sub-object-property object-property)))

(defmethod load-axiom ((axiom |OWLDataPropertyDomainAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (data-property data-property-domain) axiom
    (let ((tbox (find-tbox (owlapi-tbox *cur-reasoner*))))

      (reasoner-role-is-used-as-datatype-property data-property (owlapi-tbox *cur-reasoner*))
      (declare-datatype-properties data-property-domain)

      (or (reasoner-role-has-domain data-property
                                    data-property-domain
                                    tbox)
	  (reasoner-add-implication 
	   `(some ,data-property top) 
	   data-property-domain 
	   tbox)))))

(defmethod load-axiom ((axiom |OWLObjectPropertyDomainAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property object-property-domain) axiom
    (reasoner-declare-object-property-domain object-property object-property-domain)))

(defmethod load-axiom ((axiom |OWLDataPropertyRangeAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (data-property data-property-range) axiom
    (let* ((tbox (find-tbox (owlapi-tbox *cur-reasoner*)))
           (role-name data-property))
      (reasoner-datatype-role-has-range role-name data-property-range tbox))))

(defmethod load-axiom ((axiom |OWLObjectPropertyRangeAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (object-property object-property-range) axiom
    (reasoner-declare-object-property-range object-property object-property-range)))

;;;
;;; Add a initialize-instance :after method for those OWL Axiom classes 
;;; which can be unloaded incrementally by your reasoner, and 
;;; override unload-axiom1:
;;;  

;;;
;;; Incremental Axiom Unloading  
;;; Needs only to be implemented for those axiom types which can
;;; be unloaded incrementally by the reasoner (unloading of remaining
;;; axioms will be handeled using clear and reload)
;;; 

(defmethod initialize-axiom :after ((axiom |OWLClassAssertionAxiom|) new-p)
  (declare (ignorable new-p))
  (setf (can-be-unloaded-p axiom) t))

(defmethod unload-axiom1 ((axiom |OWLClassAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (ax-individual description) axiom   
    (reasoner-forget-concept-assertion (owlapi-abox *cur-reasoner*)
                                       ax-individual
                                       description)))

(defmethod initialize-axiom :after ((axiom |OWLDifferentIndividualsAxiom|) new-p)
  (declare (ignorable new-p))
  (setf (can-be-unloaded-p axiom) t))

(defmethod unload-axiom1 ((axiom |OWLDifferentIndividualsAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (individuals) axiom
    (reasoner-forget-all-different-assertion (owlapi-abox *cur-reasoner*) individuals)))

(defmethod initialize-axiom :after ((axiom |OWLSameIndividualsAxiom|) new-p)
  (declare (ignorable new-p))
  (setf (can-be-unloaded-p axiom) t))

(defmethod unload-axiom1 ((axiom |OWLSameIndividualsAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (individuals) axiom
    (mapl #'(lambda (inds)
              (let ((i (first inds)))
                (dolist (j (rest inds))
                  (reasoner-forget-same-individual-as-assertion 
                   (owlapi-abox *cur-reasoner*) i j))))
          individuals)))

(defmethod initialize-axiom :after ((axiom |OWLDataPropertyAssertionAxiom|) new-p)
  (declare (ignorable new-p))
  (setf (can-be-unloaded-p axiom) t))

(defmethod unload-axiom1  ((axiom |OWLDataPropertyAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (subject rel-data-property data-literal) axiom
    (reasoner-forget-datatype-role-filler 
     (owlapi-abox *cur-reasoner*) subject data-literal rel-data-property)))

(defmethod initialize-axiom :after ((axiom |OWLNegativeDataPropertyAssertionAxiom|) new-p)
  (declare (ignorable new-p))
  (setf (can-be-unloaded-p axiom) t))

(defmethod unload-axiom1  ((axiom |OWLNegativeDataPropertyAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (subject rel-data-property data-literal) axiom
    (reasoner-forget-negative-datatype-role-filler
     (owlapi-abox *cur-reasoner*) subject data-literal rel-data-property)))

(defmethod initialize-axiom :after ((axiom |OWLObjectPropertyAssertionAxiom|) new-p)
  (declare (ignorable new-p))
  (setf (can-be-unloaded-p axiom) t))

(defmethod unload-axiom1  ((axiom |OWLObjectPropertyAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))

  (with-slots (subject rel-object-property object) axiom
    (reasoner-forget-role-assertion 
     (owlapi-abox *cur-reasoner*) subject object rel-object-property)))

(defmethod initialize-axiom :after ((axiom |OWLNegativeObjectPropertyAssertionAxiom|) new-p)
  (declare (ignorable new-p))
  (setf (can-be-unloaded-p axiom) t))

(defmethod unload-axiom1 ((axiom |OWLNegativeObjectPropertyAssertionAxiom|) &key ontology)
  (declare (ignorable ontology))
  
  (with-slots (subject rel-object-property object) axiom
    (reasoner-forget-negated-role-assertion 
     (owlapi-abox *cur-reasoner*)
     subject
     object
     rel-object-property)))

;;;
;;; Entailment Checks 
;;;

(defmethod entailed-p ((axiom |OWLDisjointClassesAxiom|))
  (with-slots (descriptions) axiom
    (for-all-pairs-holds-p
     descriptions 
     (lambda (x y)
       (reasoner-concepts-disjoint-p x y (owlapi-tbox *cur-reasoner*))))))

(defmethod entailed-p ((axiom |OWLDisjointUnionAxiom|))
  (with-slots (descriptions) axiom
    (and 
     (for-all-pairs-holds-p
      descriptions 
      (lambda (x y)
        (reasoner-concepts-disjoint-p x y (owlapi-tbox *cur-reasoner*))))
     (reasoner-concepts-equivalent-p (first descriptions)
                                     `(or ,@(rest descriptions))
                                     (owlapi-tbox *cur-reasoner*)))))

(defmethod entailed-p ((axiom |OWLEquivalentClassesAxiom|))
  (with-slots (descriptions) axiom
    (pairwise-equivalent-p
     descriptions 
     (lambda (x y)
       (reasoner-concepts-equivalent-p x y 
                                       (owlapi-tbox *cur-reasoner*))))))

(defmethod entailed-p ((axiom |OWLSubClassAxiom|))
  (with-slots (descriptions) axiom
    (let ((a (first descriptions))
          (b (second descriptions)))
      (reasoner-concept-subsumes-p b a (owlapi-tbox *cur-reasoner*)))))

(defmethod direct-entailed-p ((axiom |OWLSubClassAxiom|))
  (with-slots (descriptions) axiom
    (let ((a (first descriptions))
          (b (second descriptions)))
      (and (symbolp a)
           (symbolp b)
           (find-if #'(lambda (x) 
                        (member a (ensure-list x)))
                    (reasoner-concept-children b (owlapi-tbox *cur-reasoner*)))))))

(defmethod entailed-p ((axiom |OWLClassAssertionAxiom|))
  (with-slots (ax-individual description) axiom
    (reasoner-instance-of-p ax-individual
                            description
                            (owlapi-abox *cur-reasoner*)
                            nil)))

(defmethod direct-entailed-p ((axiom |OWLClassAssertionAxiom|))
  (with-slots (ax-individual description) axiom
    (find-if #'(lambda (x) 
                 (member description 
                         (ensure-list x)))
             (reasoner-get-types ax-individual 
                                 (owlapi-abox *cur-reasoner*)
                                 t))))

(defmethod entailed-p ((axiom |OWLDifferentIndividualsAxiom|))
  (with-slots (individuals) axiom
    (for-all-pairs-holds-p 
     individuals
     (lambda (x y) 
       (member x 
               (reasoner-individual-antonyms
                y (owlapi-abox *cur-reasoner*)))))))

(defmethod entailed-p ((axiom |OWLSameIndividualsAxiom|))
  (with-slots (individuals) axiom
    (pairwise-equivalent-p 
     individuals
     (lambda (x y) 
       (member x 
               (reasoner-individual-synonyms 
                y  
                (owlapi-abox *cur-reasoner*)))))))

(defmethod entailed-p ((axiom |OWLDataPropertyAssertionAxiom|))
  (with-slots (subject rel-data-property data-literal) axiom
    (or (member 
         data-literal
         (reasoner-retrieve-individual-told-datatype-fillers 
          subject rel-data-property (owlapi-abox *cur-reasoner*))
         :test #'equal)
        (reasoner-instance-of-p subject
                                `(d-filler ,rel-data-property ,data-literal)
                                (owlapi-abox *cur-reasoner*)
                                nil))))

(defmethod entailed-p ((axiom |OWLObjectPropertyAssertionAxiom|))
  (with-slots (subject rel-object-property object) axiom
    (reasoner-individuals-related-p subject object rel-object-property 
                                    (owlapi-abox *cur-reasoner*))))

(defmethod entailed-p ((axiom |OWLNegativeObjectPropertyAssertionAxiom|))
  (with-slots (subject rel-object-property object) axiom
    (reasoner-individuals-related-p subject object `(not ,rel-object-property)
                                    (owlapi-abox *cur-reasoner*))))



(defmethod entailed-p ((axiom |OWLFunctionalDataPropertyAxiom|))
  (with-slots (data-property) axiom
    (reasoner-role-functional-p data-property (owlapi-tbox *cur-reasoner*))))

(defmethod entailed-p ((axiom |OWLFunctionalObjectPropertyAxiom|))
  (with-slots (object-property) axiom
    (reasoner-role-functional-p object-property (owlapi-tbox *cur-reasoner*))))

(defmethod entailed-p ((axiom |OWLDisjointDataPropertiesAxiom|))
  (with-slots (data-properties) axiom
    (for-all-pairs-holds-p
     data-properties 
     #'(lambda (r1 r2) 
         (reasoner-role-disjoint-p r1 r2 (owlapi-tbox *cur-reasoner*))))))

(defmethod entailed-p ((axiom |OWLDisjointObjectPropertiesAxiom|))
  (with-slots (object-properties) axiom
    (for-all-pairs-holds-p
     object-properties 
     #'(lambda (r1 r2) 
         (reasoner-role-disjoint-p r1 r2 (owlapi-tbox *cur-reasoner*))))))

(defmethod entailed-p ((axiom |OWLEquivalentDataPropertiesAxiom|))
  (with-slots (data-properties) axiom
    (pairwise-equivalent-p
     data-properties 
     #'(lambda (r1 r2)
         (reasoner-role-equivalent-p r1 r2 (owlapi-tbox *cur-reasoner*))))))

(defmethod entailed-p ((axiom |OWLEquivalentObjectPropertiesAxiom|))
  (with-slots (object-properties) axiom
    (pairwise-equivalent-p
     object-properties 
     #'(lambda (r1 r2)
         (reasoner-role-equivalent-p r1 r2 (owlapi-tbox *cur-reasoner*))))))

(defmethod entailed-p ((axiom |OWLInverseObjectPropertiesAxiom|))
  (with-slots (object-properties) axiom
    (let ((marker (gensym)))
      (not (reasoner-concept-satisfiable-p 
            `(or 
              (and (some ,(first object-properties) ,marker)
                   (all (inv ,(second object-properties)) (not ,marker)))
              (and (some ,(second object-properties) ,marker)
                   (all (inv ,(first object-properties)) (not ,marker))))
            (owlapi-tbox *cur-reasoner*))))))

(defmethod entailed-p ((axiom |OWLAsymmetricObjectPropertyAxiom|))
  (with-slots (object-property) axiom
    (reasoner-role-asymmetric-p object-property (owlapi-tbox *cur-reasoner*))))

(defmethod entailed-p ((axiom |OWLInverseFunctionalObjectPropertyAxiom|))
  (with-slots (object-property) axiom
    (reasoner-role-inverse-functional-p object-property (owlapi-tbox *cur-reasoner*))))

(defmethod entailed-p ((axiom |OWLIrreflexiveObjectPropertyAxiom|))
  (with-slots (object-property) axiom
    (reasoner-role-irreflexive-p object-property (owlapi-tbox *cur-reasoner*))))

(defmethod entailed-p ((axiom |OWLReflexiveObjectPropertyAxiom|))
  (with-slots (object-property) axiom
    (reasoner-role-reflexive-p object-property (owlapi-tbox *cur-reasoner*))))

(defmethod entailed-p ((axiom |OWLSymmetricObjectPropertyAxiom|))
  (with-slots (object-property) axiom
    (reasoner-role-symmetric-p object-property (owlapi-tbox *cur-reasoner*))))

(defmethod entailed-p ((axiom |OWLTransitiveObjectPropertyAxiom|))
  (with-slots (object-property) axiom
    (reasoner-role-transitive-p object-property (owlapi-tbox *cur-reasoner*))))

(defmethod entailed-p ((axiom |OWLDataSubPropertyAxiom|))
  (with-slots (sub-data-property data-property) axiom
    (reasoner-role-subsumes-p data-property sub-data-property
                              (owlapi-tbox *cur-reasoner*))))

(defmethod direct-entailed-p ((axiom |OWLDataSubPropertyAxiom|))
  (with-slots (sub-data-property data-property) axiom
    (with-current-owllink-kb
      (member sub-data-property
              (owl-syntaxes::owllink-role-children data-property)))))

(defmethod entailed-p ((axiom |OWLObjectSubPropertyAxiom|))
  (with-slots (sub-object-property object-property) axiom
    (reasoner-role-subsumes-p object-property sub-object-property
                              (owlapi-tbox *cur-reasoner*))))

(defmethod direct-entailed-p ((axiom |OWLObjectSubPropertyAxiom|))
  (with-slots (sub-object-property object-property) axiom
    (with-current-owllink-kb
      (member sub-object-property
              (owl-syntaxes::owllink-role-children object-property)))))

(defmethod entailed-p ((axiom |OWLDataPropertyDomainAxiom|))
  (with-slots (data-property data-property-domain) axiom
    (not (reasoner-concept-satisfiable-p
          `(and 
            (not ,data-property-domain)
            (some ,data-property top))
          (owlapi-tbox *cur-reasoner*)))))

(defmethod entailed-p ((axiom |OWLObjectPropertyDomainAxiom|))
  (with-slots (object-property object-property-domain) axiom
    (not (reasoner-concept-satisfiable-p
          `(and 
            (not ,object-property-domain)
            (some ,object-property top))
          (owlapi-tbox *cur-reasoner*)))))

(defmethod entailed-p ((axiom |OWLDataPropertyRangeAxiom|))
  (with-slots (data-property data-property-range) axiom
    (not (reasoner-concept-satisfiable-p
          `(and 
            (some ,data-property (not ,data-property-range)))            
          (owlapi-tbox *cur-reasoner*)))))

(defmethod entailed-p ((axiom |OWLObjectPropertyRangeAxiom|))
  (with-slots (object-property object-property-range) axiom
    (not (reasoner-concept-satisfiable-p
          `(and 
            (some ,object-property (not ,object-property-range)))            
          (owlapi-tbox *cur-reasoner*)))))

;;;
;;; Warnings
;;;

(declaim (special *indent*))

(defun reasoner-redundant-import-warning (uri)
  (when (or ts::*really-warn-p*
            (and ts::*warnings-p*
                 (or *tbox-verbose*
                     *abox-verbose*)))
    
    (format t "~%~V@TI already have ontology ~A, skipping redundant import" 
            *indent* uri)))

(defun reasoner-syntax-message (message uri)
  (when (or ts::*really-warn-p*
            (and ts::*warnings-p*
                 (or *tbox-verbose*
                     *abox-verbose*)))
    
    (format t message (- *indent* 3) uri)))
