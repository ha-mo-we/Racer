;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWL-SYNTAXES; Base: 10 -*-

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

(in-package :owl-syntaxes)

;;;
;;;;  owllink-rasoner-bridge.lisp
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
;;;   Purpose: Example of a concrete OWLlink reasoner bridge (for Racer). 
;;;            Supplied for illustration purposes only. 
;;; 

(defun reduced-object-property-synset2 (role-nameset &optional up-p) 
  (if (not up-p)
      (cond ((reasoner-top-object-property-p (first role-nameset) *owllink-kb-and-reasoner*)
             (let ((res (without-top-object-property role-nameset *owllink-kb-and-reasoner*)))
               (cons owlapi:+owlapi-owl-top-object-role+ res)))
            ((reasoner-bottom-object-property-p (first role-nameset) *owllink-kb-and-reasoner*)
             nil)
            (t role-nameset))
    
    (cond ((reasoner-top-object-property-p (first role-nameset) *owllink-kb-and-reasoner*)
           nil)
          ((reasoner-bottom-object-property-p (first role-nameset) *owllink-kb-and-reasoner*)
           (let ((res (without-bottom-object-property role-nameset *owllink-kb-and-reasoner*)))
             (cons owlapi:+owlapi-owl-bottom-object-role+ res)))
          (t role-nameset))))


(defun reduced-data-property-synset2 (role-nameset &optional up-p) 
  (if (not up-p)
      (cond ((reasoner-top-data-property-p (first role-nameset) *owllink-kb-and-reasoner*)
             (let ((res (without-top-data-property role-nameset *owllink-kb-and-reasoner*)))
               (cons owlapi:+owlapi-owl-top-data-role+ res)))
            ((reasoner-bottom-data-property-p (first role-nameset) *owllink-kb-and-reasoner*)
             nil)
            (t role-nameset))
    
    (cond ((reasoner-top-data-property-p (first role-nameset) *owllink-kb-and-reasoner*)
           nil)
          ((reasoner-bottom-data-property-p (first role-nameset) *owllink-kb-and-reasoner*)
           (let ((res (without-bottom-data-property role-nameset *owllink-kb-and-reasoner*)))
             (cons owlapi:+owlapi-owl-bottom-data-role+ res)))
          (t role-nameset))))


(defun reduced-synset2 (concept-nameset &optional up-p) 
  (if (not up-p)
      (cond ((member (first concept-nameset)
                     '(top *top*))
             (let ((res (without-top concept-nameset)))
               (cons owlapi:+owlapi-owl-thing+ res)))
            ((member (first concept-nameset)
                     '(bottom *bottom*))
             nil)
            (t concept-nameset))
    
    (cond ((member (first concept-nameset)
                   '(top *top*))
             nil)
          ((member (first concept-nameset)
                   '(bottom *bottom*))
           (let ((res (without-bottom concept-nameset)))
             (cons owlapi:+owlapi-owl-nothing+ res)))
          (t concept-nameset))))

(defun reduced-synset (concept-nameset &optional up-p)
  (declare (ignorable up-p))
           
  (cond ((member (first concept-nameset)
                 '(top *top*))
         (cons owlapi:+owlapi-owl-thing+ 
               (without-top concept-nameset)))
        ((member (first concept-nameset)
                 '(bottom *bottom*))
         (cons owlapi:+owlapi-owl-nothing+ 
               (without-bottom concept-nameset)))
        (t concept-nameset)))

(defun concept-synset (concept-nameset &optional up-p)
  (make-synset '|Class| (reduced-synset concept-nameset up-p)))

;;;
;;;
;;;

(defun owllink-object-property-p (role)
  (reasoner-object-property-p role *owllink-kb-and-reasoner*))

(defun owllink-data-property-p (role)
  (reasoner-data-property-p role *owllink-kb-and-reasoner*))

(defun owllink-annotation-property-p (role)
  (reasoner-annotation-property-p role *owllink-kb-and-reasoner*))

;;;
;;;
;;;

(defun owllink-concept-synonyms (concept)
  (reasoner-concept-synonyms concept *owllink-kb-and-reasoner*))
         
(defun owllink-bottom-synonyms ()
  (let ((x 
         (without-bottom
          (reasoner-concept-synonyms 'bottom *owllink-kb-and-reasoner*))))
    (cons owlapi:+owlapi-owl-nothing+ x)))

;;;
;;;
;;;

(defun owllink-get-disjoint-classes (concept &optional flat-p)
  (let ((res
         (reasoner-get-disjoint-concepts concept *owllink-kb-and-reasoner*)))
    (if (not flat-p)
        (concept-list-result res)
      (flat-concept-list-result res))))

(defun owllink-get-sub-classes (concept &optional flat-p)
  (let ((res
         (if *direct*
             (reasoner-concept-children concept *owllink-kb-and-reasoner*)
           (remove-concept-by-synset 
            concept
            (reasoner-concept-descendants concept *owllink-kb-and-reasoner*)))))
    (if (not flat-p)
        (concept-list-result res nil t)
      (flat-concept-list-result res))))

(defun owllink-get-super-classes (concept &optional flat-p)
  (let ((res
         (if *direct*
             (reasoner-concept-parents
              concept *owllink-kb-and-reasoner*)
           (remove-concept-by-synset
            concept
            (reasoner-concept-ancestors concept *owllink-kb-and-reasoner*)))))
    (if (not flat-p)
        (concept-list-result res nil t)
      (flat-concept-list-result res))))

(defun owllink-class-hierarchy2 (concept &optional up-p)
  (let ((kb *owllink-kb-and-reasoner*))
    
    (make-owllink-message
     
     '|ClassHierarchy| 
     
     nil
     
     (cons 
      
      (make-synset '|Class| 
                   (owllink-bottom-synonyms))

      (loop for concept-nameset in
              
            (cons (reasoner-atomic-concept-synonyms concept kb)
                  (if (not up-p)
                      (reasoner-concept-descendants concept kb)
                    (reasoner-concept-ancestors concept kb)))

            as classes = 

            (when concept-nameset
              (if (not up-p)
                  (reasoner-concept-children
                   (first concept-nameset) kb)
                (reasoner-concept-parents
                 (first concept-nameset) kb)))

            as classes1 =  (mapcar #'(lambda (x) 
                                       (reduced-synset2 x up-p))
                                   classes)
            
            when (and classes1 
                      ;; not all NIL? 
                      (some #'identity classes1))
              
            collect (make-owllink-message
                     (if (not up-p)
                         '|ClassSubClassesPair|
                       '|ClassSuperClassesPair|)

                     nil

                     (list (concept-synset concept-nameset up-p)

                           (make-set-of-synsets '|SubClass|
                                                (remove nil 
                                                        (mapcar #'(lambda (x) 
                                                                    (make-synset '|Class| x))
                                                                classes1))))))))))

(defun owllink-roles-equivalent-p (role-1 role-2)
  (reasoner-role-equivalent-p role-1 role-2 *owllink-kb-and-reasoner*))

(defun owllink-roles-disjoint-p (role-1 role-2) 
  (reasoner-role-disjoint-p role-1 role-2 *owllink-kb-and-reasoner*))

(defun owllink-role-satisfiable-p (role) 
  (reasoner-role-satisfiable-p role *owllink-kb-and-reasoner*))

(defun owllink-role-functional-p (role) 
  (reasoner-role-functional-p role *owllink-kb-and-reasoner*))

(defun owllink-role-inverse-functional-p (role) 
  (reasoner-role-inverse-functional-p role *owllink-kb-and-reasoner*))

(defun owllink-role-reflexive-p (role)
  (reasoner-role-reflexive-p role *owllink-kb-and-reasoner*))

(defun owllink-role-irreflexive-p (role) 
  (reasoner-role-irreflexive-p role *owllink-kb-and-reasoner*))

(defun owllink-role-symmetric-p (role) 
  (reasoner-role-symmetric-p role *owllink-kb-and-reasoner*))

(defun owllink-role-asymmetric-p (role) 
  (reasoner-role-asymmetric-p role *owllink-kb-and-reasoner*))

(defun owllink-role-transitive-p (role) 
  (reasoner-role-transitive-p role *owllink-kb-and-reasoner*))

(defun owllink-role-subsumes-p (role-1 role-2) 
  (reasoner-role-subsumes-p role-1 role-2 *owllink-kb-and-reasoner*))

(defun owllink-object-property-synonyms (role &optional (no-internal-roles-p t))
  (when (owllink-object-property-p role)
    (cond ((reasoner-bottom-object-property-p role *owllink-kb-and-reasoner*)
           (remove-duplicates
            (remove-if #'(lambda (x) (or (not x) (consp x)))
                       (list owlapi:+owlapi-owl-bottom-object-role+
                             (unless no-internal-roles-p 
                               (reasoner-get-object-bottom-role *owllink-kb-and-reasoner*))))))

          ((reasoner-top-object-property-p role *owllink-kb-and-reasoner*)
           (remove-duplicates
            (remove-if #'(lambda (x) (or (not x) (consp x)))
                       (list owlapi:+owlapi-owl-top-object-role+
                             (unless no-internal-roles-p 
                               (reasoner-get-object-top-role *owllink-kb-and-reasoner*))))))
        
          (t 
           (reasoner-only-object-properties
            (remove-duplicates
             (cons role (reasoner-equivalent-roles role *owllink-kb-and-reasoner*)))
            *owllink-kb-and-reasoner*)))))

(defun owllink-role-children (role)
  (when role
    (cond ((owllink-object-property-p role)
           (cond ((reasoner-top-object-property-p role *owllink-kb-and-reasoner*)
                  (or
                   (remove-if #'(lambda (r)
                                  (or (consp r)
                                      (reasoner-top-object-property-p r *owllink-kb-and-reasoner*)
                                      (reasoner-bottom-object-property-p r *owllink-kb-and-reasoner*)
                                      (let ((res
                                             (reasoner-no-data-properties
                                              (reasoner-atomic-role-parents 
                                               r *owllink-kb-and-reasoner*)
                                              *owllink-kb-and-reasoner*)))
                                        (not ; haengt nicht unter TOP-ROLE -> raus
                                         (member 
                                          (reasoner-get-object-top-role *owllink-kb-and-reasoner*)
                                          res)))))
                              (reasoner-get-object-properties *owllink-kb-and-reasoner*))
                   (list owlapi:+owlapi-owl-bottom-object-role+)))
                 ((reasoner-bottom-object-property-p role *owllink-kb-and-reasoner*)
                  nil)
                 (t 
                  (or 
                   (reasoner-only-object-properties
                    (reasoner-atomic-role-children role *owllink-kb-and-reasoner*)
                    *owllink-kb-and-reasoner*)
                   (list owlapi:+owlapi-owl-bottom-object-role+)))))

          ((owllink-data-property-p role)
           (cond ((reasoner-top-data-property-p role *owllink-kb-and-reasoner*)
                  (or
                   (remove-if #'(lambda (r)
                                  (or (consp r)
                                      (reasoner-top-data-property-p r *owllink-kb-and-reasoner*)
                                      (reasoner-bottom-data-property-p r *owllink-kb-and-reasoner*)
                                      (let ((res
                                             (reasoner-no-object-properties
                                              (reasoner-atomic-role-parents 
                                               r *owllink-kb-and-reasoner*)
                                              *owllink-kb-and-reasoner*)))
                                        (not 
                                         (member 
                                          (reasoner-get-datatype-top-role *owllink-kb-and-reasoner*)
                                          res)))))
                              (reasoner-get-data-properties *owllink-kb-and-reasoner*))
                   (list owlapi:+owlapi-owl-bottom-data-role+)))
                 ((reasoner-bottom-data-property-p role *owllink-kb-and-reasoner*)
                  nil)
                 (t 
                  (or 
                   (reasoner-only-data-properties 
                    (reasoner-atomic-role-children role *owllink-kb-and-reasoner*)
                    *owllink-kb-and-reasoner*)
                   (list owlapi:+owlapi-owl-bottom-data-role+)))))

          ((owllink-annotation-property-p role)
           (reasoner-only-annotation-properties 
            (reasoner-atomic-role-children role *owllink-kb-and-reasoner*)
            *owllink-kb-and-reasoner*))

          (t ; (inv objectproperty) etc. 
           nil))))

(defun owllink-role-parents (role)
  (when role 
    (cond ((owllink-object-property-p role)
           (cond ((reasoner-bottom-object-property-p role *owllink-kb-and-reasoner*)
                  (or 
                   (remove-if #'(lambda (r)
                                  (or (consp r)
                                      (reasoner-bottom-object-property-p r *owllink-kb-and-reasoner*)
                                      (reasoner-top-object-property-p r *owllink-kb-and-reasoner*)
                                      (let ((res
                                             (reasoner-no-data-properties
                                              (reasoner-atomic-role-children
                                               r *owllink-kb-and-reasoner*)
                                              *owllink-kb-and-reasoner*)))
                                        (not 
                                         (member 
                                          (reasoner-get-object-bottom-role *owllink-kb-and-reasoner*)
                                          res)))))
                              (reasoner-get-object-properties *owllink-kb-and-reasoner*))
                   (list owlapi:+owlapi-owl-top-object-role+)))
                 ((reasoner-top-object-property-p role *owllink-kb-and-reasoner*)
                  nil)
                 (t 
                  (or 
                   (reasoner-only-object-properties
                    (reasoner-atomic-role-parents 
                     role *owllink-kb-and-reasoner*)
                    *owllink-kb-and-reasoner*)
                   (list owlapi:+owlapi-owl-top-object-role+)))))

          ((owllink-data-property-p role)
           (cond ((reasoner-bottom-data-property-p role *owllink-kb-and-reasoner*)
                  (or 
                   (remove-if #'(lambda (r)
                                  (or (consp r)
                                      (reasoner-bottom-data-property-p r *owllink-kb-and-reasoner*)
                                      (reasoner-top-data-property-p r *owllink-kb-and-reasoner*)
                                      (let ((res
                                             (reasoner-no-object-properties
                                              (reasoner-atomic-role-children
                                               r *owllink-kb-and-reasoner*)
                                              *owllink-kb-and-reasoner*)))
                                        (not 
                                         (member
                                          (reasoner-get-datatype-bottom-role *owllink-kb-and-reasoner*)
                                          res)))))
                              (reasoner-get-data-properties *owllink-kb-and-reasoner*))
                   (list owlapi:+owlapi-owl-top-data-role+)))
                 ((reasoner-top-data-property-p role *owllink-kb-and-reasoner*)
                  nil)
                 (t 
                  (or
                   (reasoner-only-data-properties 
                    (reasoner-atomic-role-parents role *owllink-kb-and-reasoner*)
                    *owllink-kb-and-reasoner*)
                   (list owlapi:+owlapi-owl-top-data-role+)))))

          ((owllink-annotation-property-p role)
           (reasoner-only-annotation-properties 
            (reasoner-atomic-role-parents role *owllink-kb-and-reasoner*)
            *owllink-kb-and-reasoner*))

          (t ; (inv objectproperty) etc. 
           nil))))

(defun owllink-role-descendants (role)
  (cond ((owllink-object-property-p role)
         (cond ((reasoner-bottom-object-property-p role *owllink-kb-and-reasoner*)
                nil)
               ((reasoner-top-object-property-p role *owllink-kb-and-reasoner*)
                (remove-role-by-synset 
                 role
                 (reasoner-get-object-properties *owllink-kb-and-reasoner*)))
               (t 
                (cons owlapi:+owlapi-owl-bottom-object-role+
                      (remove-role-by-synset role
                              (reasoner-only-object-properties
                               (reasoner-atomic-role-descendants 
                                role *owllink-kb-and-reasoner*)
                               *owllink-kb-and-reasoner*))))))

        ((owllink-data-property-p role)
         (cond ((reasoner-bottom-data-property-p role *owllink-kb-and-reasoner*)
                nil)
               ((reasoner-top-data-property-p role *owllink-kb-and-reasoner*)
                (remove-role-by-synset role
                        (reasoner-get-data-properties *owllink-kb-and-reasoner*)))
                (t 
                 (cons owlapi:+owlapi-owl-bottom-data-role+
                       (remove-role-by-synset role 
                               (reasoner-only-data-properties
                                (reasoner-atomic-role-descendants 
                                 role *owllink-kb-and-reasoner*)
                                *owllink-kb-and-reasoner*))))))

        ((owllink-annotation-property-p role)
         (remove-role-by-synset role 
                 (reasoner-only-annotation-properties 
                  (reasoner-atomic-role-descendants
                   role *owllink-kb-and-reasoner*)
                  *owllink-kb-and-reasoner*)))

        (t ; (inv objectproperty) etc. 
         nil)))

(defun owllink-role-ancestors (role)
  (cond ((owllink-object-property-p role)
         (cond ((reasoner-top-object-property-p role *owllink-kb-and-reasoner*)
                nil)
               ((reasoner-bottom-object-property-p role *owllink-kb-and-reasoner*)
                (remove-role-by-synset role
                        (reasoner-get-object-properties *owllink-kb-and-reasoner*)))
               (t 
                (cons owlapi:+owlapi-owl-top-object-role+
                      (remove-role-by-synset role 
                              (reasoner-only-object-properties
                               (reasoner-atomic-role-ancestors
                                role *owllink-kb-and-reasoner*)
                               *owllink-kb-and-reasoner*))))))

        ((owllink-data-property-p role)
         (cond ((reasoner-top-data-property-p role *owllink-kb-and-reasoner*)
                nil)
               ((reasoner-bottom-data-property-p role *owllink-kb-and-reasoner*)
                (remove-role-by-synset role
                        (reasoner-get-data-properties *owllink-kb-and-reasoner*)))
               (t 
                (cons owlapi:+owlapi-owl-top-data-role+
                      (remove-role-by-synset role 
                              (reasoner-only-data-properties
                               (reasoner-atomic-role-ancestors
                                role *owllink-kb-and-reasoner*)
                               *owllink-kb-and-reasoner*))))))

        ((owllink-annotation-property-p role)
         (remove-role-by-synset role 
                 (reasoner-only-annotation-properties 
                  (reasoner-atomic-role-ancestors
                   role *owllink-kb-and-reasoner*)
                  *owllink-kb-and-reasoner*)))

        (t ; (inv dataproperty) etc. 
         nil)))

(defun owllink-get-disjoint-object-properties (role &optional flat-p)
  (let ((res
         (reasoner-get-disjoint-object-properties role *owllink-kb-and-reasoner*)))
  (if (not flat-p)
      (object-property-list-result res nil nil t)
    (flat-object-property-list-result res))))

(defun owllink-object-property-hierarchy2 (role &optional up-p)
  (make-owllink-message 
   '|ObjectPropertyHierarchy|

   nil

   (cons 
      
    (make-synset '|ObjectProperty| 
                 (reasoner-only-object-properties
                  (owllink-object-property-synonyms owlapi:+owlapi-owl-bottom-object-role+)
                  *owllink-kb-and-reasoner*))

    (loop for role-set in
              
          (remove-duplicates
           (mapcar #'owllink-object-property-synonyms 
                   (cons role
                         (if (not up-p)
                             (owllink-role-descendants role)
                           (owllink-role-ancestors role))))
           :test #'owlapi:set-equal)

          as roles = 
          
          (remove nil 
                  (remove-duplicates
                   (mapcar #'owllink-object-property-synonyms
                           (when role
                             (if (not up-p)
                                 (owllink-role-children (first role-set))
                               (owllink-role-parents (first role-set)))))
                   :test #'equal))
          
          as roles1 =  (mapcar #'(lambda (x) 
                                   (reduced-object-property-synset2 x up-p))
                               roles)

          when (and roles1 
                    ;; not all NIL? 
                    (some #'identity roles1))
              
          collect (make-owllink-message
                   (if (not up-p)
                       '|ObjectPropertySubObjectPropertiesPair|
                     '|ObjectPropertySuperObjectPropertiesPair|)

                   nil

                   (list 

                    (make-synset '|ObjectProperty| role-set)

                    (make-set-of-synsets '|SubObjectProperty|
                                         (remove nil 
                                                 (mapcar #'(lambda (x) 
                                                             (make-synset '|ObjectProperty| x))
                                                         roles1)))))))))

(defun owllink-data-property-synonyms (role &optional (no-internal-roles-p t))
  (when (owllink-data-property-p role)
    (cond ((reasoner-bottom-data-property-p role *owllink-kb-and-reasoner*)
           (remove-duplicates
            (remove-if #'(lambda (x) (or (not x) (consp x)))
                       (list owlapi:+owlapi-owl-bottom-data-role+
                             (unless no-internal-roles-p
                               (reasoner-get-datatype-bottom-role *owllink-kb-and-reasoner*))))))

          ((reasoner-top-data-property-p role *owllink-kb-and-reasoner*)
           (remove-duplicates
            (remove-if #'(lambda (x) (or (not x) (consp x)))
                       (list owlapi:+owlapi-owl-top-data-role+
                             #+:ignore
                             (unless no-internal-roles-p 
                               (reasoner-get-datatype-top-role *owllink-kb-and-reasoner*))))))

          (t 
           (reasoner-only-data-properties
            (remove-duplicates
             (cons role (reasoner-equivalent-roles
                         role *owllink-kb-and-reasoner*)))
            *owllink-kb-and-reasoner*)))))

(defun owllink-get-disjoint-data-properties (role &optional flat-p)
  (let ((res
         (reasoner-get-disjoint-data-properties role *owllink-kb-and-reasoner*)))
    (if (not flat-p)
        (data-property-list-result res)
      (flat-data-property-list-result res))))

(defun owllink-data-property-hierarchy2 (role &optional up-p)
  (make-owllink-message 
   '|DataPropertyHierarchy|

   nil

   (cons 
      
    (make-synset '|DataProperty| 
                 (reasoner-only-data-properties
                  (owllink-data-property-synonyms owlapi:+owlapi-owl-bottom-data-role+)
                  *owllink-kb-and-reasoner*))

    (loop for role-set in
              
          (remove-duplicates
           (mapcar #'owllink-data-property-synonyms 
                   (cons role
                         (if (not up-p)
                             (owllink-role-descendants role)
                           (owllink-role-ancestors role))))
           :test #'owlapi:set-equal)

          as roles = 

          (remove nil 
                  (remove-duplicates
                   (mapcar #'owllink-data-property-synonyms
                           (when role
                             (if (not up-p)
                                 (owllink-role-children (first role-set))
                               (owllink-role-parents (first role-set)))))
                   :test #'equal))
          
          as roles1 =  (mapcar #'(lambda (x) 
                                   (reduced-data-property-synset2 x up-p))
                               roles)

          when (and roles1 
                    ;; not all NIL? 
                    (some #'identity roles1))

          collect (make-owllink-message
                   (if (not up-p)
                       '|DataPropertySubDataPropertiesPair|
                     '|DataPropertySuperDataPropertiesPair|)

                   nil

                   (list 

                    (make-synset '|DataProperty| role-set)

                    (make-set-of-synsets '|SubDataProperty|
                                         (remove nil 
                                                 (mapcar #'(lambda (x) 
                                                             (make-synset '|DataProperty| x))
                                                         roles1)))))))))

(defun owllink-is-instance-of (ind concept &optional direct-p)
  (reasoner-instance-of-p ind concept *owllink-kb-and-reasoner* direct-p))

(defun owllink-get-types (ind &optional flat-p)
  (let ((res 
         (reasoner-get-types ind *owllink-kb-and-reasoner* *direct*)))
    (if (not flat-p)
        (concept-list-result res)
      (flat-concept-list-result res))))

(defun owllink-get-disjoint-individuals (ind &optional flat-p)
  (let* (;; #+:racer-server
         ;; (racer::*optimize-datatype-role-fillers* nil) ; RACER BUG! behoben? 
         (res
          (reasoner-individual-antonyms ind *owllink-kb-and-reasoner*)))
    (if (not flat-p)
        (individual-list-result res nil nil t)
      (flat-individual-list-result res))))

(defun owllink-get-object-properties-between (source target &optional flat-p)
  (let* ((res 
          (reasoner-retrieve-individual-filled-roles
           source target *owllink-kb-and-reasoner*
           :roles (without-top-and-bottom-object-properties
                   (reasoner-get-object-properties *owllink-kb-and-reasoner*)
                   *owllink-kb-and-reasoner*)
           :negated-p *negative*
           :no-inverses-p t))
         (res 
          (if *negative* 
              (cons owlapi:+owlapi-owl-bottom-object-role+ res)
            (cons owlapi:+owlapi-owl-top-object-role+ res))))
    (if (not flat-p)
        (object-property-list-result res nil nil t)
      (flat-object-property-list-result res))))

(defun owllink-get-object-properties-of-source (source &optional flat-p)
  (let* ((res 
          (mapcar #'first 
                  (reasoner-get-individual-successors 
                   source *owllink-kb-and-reasoner*
                   :roles (without-top-and-bottom-object-properties 
                           (reasoner-get-object-properties *owllink-kb-and-reasoner*)
                           *owllink-kb-and-reasoner*)
                   :negated-p *negative*
                   :no-inverses-p t)))
         (res 
          (if *negative* 
              (cons owlapi:+owlapi-owl-bottom-object-role+ res)
            (cons owlapi:+owlapi-owl-top-object-role+ res))))
    (if (not flat-p)
        (object-property-list-result res nil nil t)
      (flat-object-property-list-result res))))

(defun owllink-get-object-properties-of-target (target &optional flat-p)
  (let* ((res 
          (delete nil
                  (mapcar #'(lambda (x)
                              (if (consp (first x))
                                  (second (first x))
                                (first x)))
                          (reasoner-get-individual-successors 
                           target *owllink-kb-and-reasoner*
                           :negated-p *negative*
                           :no-inverses-p nil
                           :roles (without-top-and-bottom-object-properties
                                   (reasoner-get-object-properties
                                    *owllink-kb-and-reasoner*)
                                   *owllink-kb-and-reasoner*)
                           :only-inverses-p t))))
         (res 
          (if *negative* 
              (cons owlapi:+owlapi-owl-bottom-object-role+ res)
            (cons owlapi:+owlapi-owl-top-object-role+ res))))
    (if (not flat-p)
        (object-property-list-result res nil nil t)
      (flat-object-property-list-result res))))
      
(defun owllink-get-data-properties-between (source literal &optional flat-p)
  (let* ((res 
          (loop for (role literals) in
                (reasoner-get-individual-datatype-fillers source *owllink-kb-and-reasoner*)
                when (find literal literals :test #'equal)
                collect role))
         (res 
          (if *negative* 
              (cons owlapi:+owlapi-owl-bottom-data-role+ res)
            (cons owlapi:+owlapi-owl-top-data-role+ res))))
    (if (not flat-p)
        (data-property-list-result res nil nil t)
      (flat-data-property-list-result res))))

(defun owllink-get-data-properties-of-source (source &optional flat-p)
  (let* ((res 
          (mapcar #'first 
                  (reasoner-get-individual-datatype-fillers source *owllink-kb-and-reasoner*)))
         (res 
          (if *negative* 
              (cons owlapi:+owlapi-owl-bottom-data-role+ res)
            (cons owlapi:+owlapi-owl-top-data-role+ res))))
    (if (not flat-p)
        (data-property-list-result res nil nil t)
      (flat-data-property-list-result res))))

(defun owllink-get-instances (concept direct-p &optional flat-p)
  (let ((res 
         (reasoner-get-instances 
          concept *owllink-kb-and-reasoner* direct-p)))
    (if (not flat-p)
        (individual-list-result res nil nil t)
      (flat-individual-list-result res))))

(defun owllink-get-object-property-targets (ind role &optional flat-p)
  (let ((res
         (reasoner-retrieve-individual-fillers 
          ind 
          (if *negative*
              `(not ,role)
            role)
          *owllink-kb-and-reasoner*)))
    (if (not flat-p)
        (individual-list-result res nil nil t)
      (flat-individual-list-result res))))

(defun owllink-get-object-property-sources (ind role &optional flat-p)
  (let ((res
         (reasoner-retrieve-individual-predecessors
          ind (if *negative*
                  `(not ,role)
                role)
          *owllink-kb-and-reasoner*)))
    (if (not flat-p)
        (individual-list-result res nil nil t)
      (flat-individual-list-result res))))

(defun owllink-get-data-property-targets (ind property)
  (flat-literal-list-result 
   (mapcar #'(lambda (x) 
               `(|Literal| nil ,x))
           (reasoner-retrieve-individual-told-datatype-fillers 
            ind property *owllink-kb-and-reasoner*))))

(defun owllink-individual-p (ind)
  (symbolp ind))

(defun owllink-get-individuals ()
  (all-individuals *owllink-kb-and-reasoner*))

(defun owllink-get-data-properties-of-literal (literal &optional flat-p)
  (racer-prepare-substrate :abox *owllink-kb-and-reasoner* :prepare-now-p t)
              
  (let* ((res
          (let ((hash (ts::datatype-property-values-and-types-cache *cur-substrate*))
                (res nil))
            (maphash #'(lambda (key vals)
                         (declare (ignorable key))
                         (dolist (x vals)
                           #+:ignore
                           (pprint (list literal 
                                         (third (first x)) 
                                         (equal (third (first x)) 
                                                literal)))
                           (when (and (equal (third (first x)) 
                                             literal)
                                      (not (second x))) ; no annotation 
                             (push (caar x)
                                   res))))
                     hash)
            res))
         (res 
          (remove-duplicates 
           (cons owlapi:+owlapi-owl-top-data-role+
                 (append res
                         (apply #'append
                                (mapcar #'owllink-role-ancestors res)))))))
    (if (not flat-p)
        (data-property-list-result res nil nil t)
      (flat-data-property-list-result res))))

(defun owllink-get-data-property-sources (literal property &optional flat-p)
  (racer-prepare-substrate :abox *owllink-kb-and-reasoner* :prepare-now-p t)
              
  (let ((res
         (let ((hash (ts::datatype-property-values-and-types-cache *cur-substrate*))
               (res nil))
                  (maphash #'(lambda (key vals)
                               (when (some #'(lambda (x)
                                               (and (equal property (first (first x)))
                                                    (equal literal  (third (first x)) )
                                                    (not (second x)))) ; no annotation 
                                           vals)
                                 (push key res)))
                           hash)
                  res)))
    (if (not flat-p)
        (individual-list-result res nil nil t)
      (flat-individual-list-result res))))

;;;
;;;
;;;

(defmethod owllink-entailed-p (axiom)
  (owlapi:entailed-p axiom))

(defmethod owllink-direct-entailed-p (axiom)
  (owlapi:direct-entailed-p axiom))

