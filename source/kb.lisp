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

(declaim (special *file-external-format*))

(defmacro with-environment (&body forms)
  `(with-alc-bindings
     (with-alc-environment
       .,forms)))

(defun save-kb (pathname-or-stream
                  &key 
                  (tbox *current-tbox*)
                  (abox *current-abox*)
                  (if-exists :supersede)
                  (if-does-not-exist :create)
                  (uri nil)
                  (syntax (if uri :owl :krss))
                  (ontology-name nil)
                  (header t))
  (check-type tbox (or tbox symbol))
  (check-type abox (or abox symbol null))
  (check-type pathname-or-stream (or pathname string file-stream))
  (check-type syntax (member :krss :racer :owl))
  (setf tbox (find-tbox tbox))
  (setf abox (and abox (find-abox abox nil)))
  (if (tbox-use-less-memory tbox)
    (racer-warn "~&Saving Tbox axioms ~A is only partially supported due to the ~
                 current memory management strategy. Some axioms might be missing. ~
                 Start Racer with option -x to enable these services.~%"
                (tbox-name tbox))      
    (flet ((save-kb-1 (stream)
             (case syntax
               ((:krss :racer)
                (print-racer-tbox :tbox tbox
                                  :stream stream
                                  :header header)
                (when abox
                  (print-abox :abox abox
                              :stream stream
                              :header header)))
               (:owl
                (let ((class-hashtable (make-hash-table))
                      (property-hashtable (make-hash-table))
                      (ind-hashtable (make-hash-table)))
                  (print-owl-tbox tbox stream :uri uri :as-part-of-kb abox
                                  :class-hashtable class-hashtable 
                                  :ontology-name ontology-name
                                  :property-hashtable property-hashtable
                                  :ind-hashtable ind-hashtable)
                  (when abox
                    (print-owl-abox abox stream :uri uri :as-part-of-kb t
                                    :class-hashtable class-hashtable 
                                    :property-hashtable property-hashtable
                                    :tbox-ind-hashtable ind-hashtable)))))))
      (ensure-knowledge-base-state ':tbox-prepared tbox)
      (with-concept-definition-mapping tbox
        (etypecase pathname-or-stream
          (file-stream (save-kb-1 pathname-or-stream))
          ((or pathname string)
           (with-open-file (stream pathname-or-stream
                                   ;; :element-type 'character
                                   :external-format *file-external-format*
                                   :direction :output
                                   :if-exists if-exists 
                                   :if-does-not-exist if-does-not-exist)
             (save-kb-1 stream))))))))




;;; ======================================================================


(defun forget-concept-axiom (tbox left right &key (inclusion-p nil))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (unless (tbox-use-less-memory tbox)
    (setf (tbox-original-concept-axioms tbox)
          (remove (list left right inclusion-p)
                  (tbox-original-concept-axioms tbox)
                  :test #'equal)))
  (let ((removed-from-index nil))
    (let ((top-name-set (concept-name-set (tbox-top-node tbox)))
          (bottom-name-set (concept-name-set (tbox-bottom-node tbox))))
      (if (and (symbolp left)
               (not (or (member left top-name-set) (member left bottom-name-set))))
        (let ((index (tbox-concept-axioms-index tbox)))
          (multiple-value-bind (found-entry found)
                               (gethash left index)
            (when (and found
                       (equal (first found-entry) right)       
                       (eq (second found-entry) inclusion-p))
              ;; Did we really found the right one?
              ;; If not, the one to be removed must be in the GCIs 
              ;; (and thus removed-from-index will still be nil).
              (remhash left index)
              (setf (tbox-concept-axioms tbox)
                    (remove (list left right inclusion-p) 
                            (tbox-concept-axioms tbox)
                            :test #'equal))
              (setf removed-from-index t))))))
    (unless removed-from-index
      ;; We did not find the axiom in the index, so it must be in the GCIs:
      (if inclusion-p
        (setf (tbox-generalized-concept-inclusions tbox)
              (remove (list left right)
                      (tbox-generalized-concept-inclusions tbox)
                      :test #'equal))
        (progn
          (setf (tbox-generalized-concept-inclusions tbox)
                (remove (list left right)
                        (tbox-generalized-concept-inclusions tbox)
                        :test #'equal))
          (setf (tbox-generalized-concept-inclusions tbox)
                (remove (list right left)
                        (tbox-generalized-concept-inclusions tbox)
                        :test #'equal)))))))

#|
;;; I tried to implement forget role. Things are tricky, however.
;;; Just removing a role is not enough because it might be mentioend
;;; as a superrole for some other role.
;;; So, the following function just does not work.
;;; rm June 07.

(defun forget-role (tbox role)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (remhash role (tbox-role-axioms-index tbox)))
|#

(defun forget-role-axioms (tbox role &key
                                   (cd-attribute nil)
                                   (parents nil) 
                                   (parent nil)
                                   (transitive nil)
                                   (transitive-p nil)
                                   (feature nil)
                                   (feature-p nil)
                                   (domain nil)
                                   (range nil)
                                   (inverse nil)
                                   (symmetric nil)
                                   (reflexive nil)
                                   (reflexive-p nil)
                                   (datatype nil)
                                   (annotation-p nil))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (setf transitive-p (or transitive-p transitive))
  (setf feature-p (or feature-p feature))
  (setf reflexive-p (or reflexive-p reflexive))
  (setf parents (or parents (and parent (or (eq parent t)
                                            (list parent)))))
  (when symmetric
    (setf inverse t))
  (let* ((index (tbox-role-axioms-index tbox))
         (entry (gethash role index)))
    (cond ((null entry)
           (racer-warn "Role ~A not found in Tbox ~A." 
                       role
                       (tbox-name tbox)))
          (t (when parents
               (if (eq parents t)
                 (setf (role-info-parents entry) nil)
                 (setf (role-info-parents entry) 
                       (set-difference (role-info-parents entry) parents))))
             (when transitive 
               (setf (role-info-transitive-p entry) nil))
             (when feature
               (setf (role-info-feature-p entry) nil))
             (when domain
               (setf (role-info-domain entry) nil))
             (when range 
               (setf (role-info-range entry) nil))
             (when inverse
               (setf (role-info-inverse entry) nil))             
             (when reflexive
               (setf (role-info-reflexive-p entry) nil))
             (when cd-attribute
               (setf (role-info-cd-attribute entry) nil))
             (when annotation-p
               (setf (role-info-annotation-p entry) nil))
             (when datatype
               (setf (role-info-datatype entry) nil))))))


(defun forget-disjointness-axiom (tbox concept-name group-name
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
    (setf (gethash group-name
                   (tbox-disjoint-set tbox))
          (remove concept-name
                  (gethash group-name
                           (tbox-disjoint-set tbox))))
    (error "Disjointness axioms can only be stated for concept names. ~
            Found ~S in ~S."
           concept-name (or form "call to forget-disjointness-axiom")))
  tbox)

(defmacro forget ((&key (tbox nil tbox-supplied-p)
                        (abox nil abox-supplied-p))
                  &body assertions)
  `(forget-statement ,(if tbox-supplied-p 
                        (list 'quote tbox)
                        '*current-tbox*)
                     ,(if abox-supplied-p 
                        (list 'quote abox)
                        '*current-abox*)
                     ',assertions))

(defun forget-statement (tbox abox assertions)
  (check-type tbox (or symbol tbox))
  (check-type abox (or symbol abox))
  (setf tbox (find-tbox tbox))
  (when abox (setf abox (find-abox abox)))
  (loop for assertion in assertions do
        (ecase (first assertion)
          (instance (forget-concept-assertion 
                     abox
                     (second assertion)
                     (third assertion)))
          (related (forget-role-assertion 
                    abox
                    (second assertion)
                    (third assertion)
                    (fourth assertion)))
          (constrained 
           (forget-constrained-assertion 
            abox
            (second assertion)
            (third assertion)
            (fourth assertion)))
          (constraints 
            (loop for constraint in (rest assertion) do
                  (forget-constraint abox constraint)))
          (implies 
           (forget-concept-axiom tbox (second assertion) (third assertion) :inclusion-p t))
          (equivalent
           (forget-concept-axiom tbox (second assertion) (third assertion) :inclusion-p nil))
          (disjoint 
           (forget-disjointness-axiom-statement tbox (rest assertion))))))

(defun forget-disjointness-axiom-statement (tbox concepts)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (when (or (tbox-classified-p-internal tbox) 
            (tbox-coherence-checked-p tbox)
            (tbox-index-structures-complete-p tbox))
    (reset-tbox tbox))
  (loop for entry being the hash-values in (tbox-disjoint-set tbox) using (hash-key group) 
        when (and (subsetp entry concepts) (subsetp concepts entry))
        do
        (remhash group (tbox-disjoint-set tbox))
        (forget-concept-axiom tbox (first entry) `(not (or . ,(rest entry))) 
                              :inclusion-p t)
        (return-from forget-disjointness-axiom-statement t)))
                 

;;;
;;;
;;;

(defun forget-concept-assertion (abox individual-name concept)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (setf (abox-individual-axioms abox)
        (delete (parse-ind-axiom (list individual-name concept))
                (abox-individual-axioms abox)
                :test #'equal))
  (when (or (not *use-less-abox-memory*) (abox-individual-axioms-index abox))
    (let ((entry (gethash individual-name (abox-individual-axioms-index abox))))
      (when entry
        (setf entry (delete concept entry :test #'equal))
        (when (or (not *use-less-abox-memory*) (abox-individual-axioms-index abox))
          (if entry
            (setf (gethash individual-name (abox-individual-axioms-index abox)) entry)
            (remhash individual-name (abox-individual-axioms-index abox)))))))
  
  (check-nrql-subscriptions (abox-name abox))

  (values))

(defun forget-annotation-concept-assertion (abox individual-name concept)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type individual-name symbol)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (setf (abox-annotation-individual-axioms abox) 
        (delete (parse-ind-axiom (list individual-name concept))
                (abox-annotation-individual-axioms abox)
                :test #'equal))

  (check-nrql-subscriptions (abox-name abox))

  (values))


(defun forget-role-assertion (abox predecessor-name filler-name role-term)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type predecessor-name symbol)
  (check-type filler-name symbol)
  (check-role-term role-term)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (let ((tuple (list predecessor-name filler-name)))
    (setf (abox-role-axioms abox)
          (delete (parse-role-axiom (list tuple role-term))
                  (abox-role-axioms abox)
                  :test #'equal))
    (when (or (not *use-less-abox-memory*) (abox-role-axioms-index abox))
      (let ((entry (gethash tuple (abox-role-axioms-index abox))))
        (when entry
          (setf entry (delete role-term entry))
          (if entry
            (setf (gethash tuple (abox-role-axioms-index abox)) entry)
            (remhash tuple (abox-role-axioms-index abox)))))))

  (check-nrql-subscriptions (abox-name abox))

  (values))

(defun forget-constrained-assertion (abox individual-name object-name attribute-term)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type individual-name symbol)
  (check-type object-name symbol)
  (check-role-term attribute-term)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (setf (abox-attribute-assertions abox)
        (delete (list (list individual-name object-name) attribute-term)
                (abox-attribute-assertions abox)
                :test #'equal))

  (check-nrql-subscriptions (abox-name abox))

  (values))

(defun forget-constraint (abox constraint)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (setf (abox-constraints abox)
        (delete (parse-constraint constraint)
                (abox-constraints abox)
                :test #'equal))

  (check-nrql-subscriptions (abox-name abox))

  (values))


(defun forget-individual (individual &optional (abox (current-abox)))
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type individual symbol)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))

  (let ((*check-subscriptions-inhibited* t))

    (let ((concept-assertions (copy-list (abox-individual-axioms abox)))
          (role-assertions (copy-list (abox-role-axioms abox)))
          (negated-role-assertions (copy-list (abox-negated-role-axioms abox)))
          (attribute-assertions (copy-list (abox-attribute-assertions abox)))
          (identity-disjointness-assertions 
           (copy-list (abox-individual-identity-disjointness-assertions abox)))
          (annotation-individual-axioms (abox-annotation-individual-axioms abox)))

      (loop for role-assertion in role-assertions do
            (when (eq individual (first (first role-assertion)))
              (unless (eq individual (second (first role-assertion)))
                (add-concept-assertion abox (second (first role-assertion)) 'top))
              (forget-role-assertion abox 
                                     (first (first role-assertion))
                                     (second (first role-assertion))
                                     (second role-assertion)))
            (when (eq individual (second (first role-assertion)))
              (unless (eq individual (first (first role-assertion)))
                (add-concept-assertion abox (first (first role-assertion)) 'top))
              (forget-role-assertion abox 
                                     (first (first role-assertion))
                                     (second (first role-assertion))
                                     (second role-assertion))))
    
      (loop for role-assertion in negated-role-assertions do
            (when (eq individual (first (first role-assertion)))
              (unless (eq individual (second (first role-assertion)))
                (add-concept-assertion abox (second (first role-assertion)) 'top))
              (forget-negated-role-assertion abox 
                                             (first (first role-assertion))
                                             (second (first role-assertion))
                                             (second role-assertion)))
            (when (eq individual (second (first role-assertion)))
              (unless (eq individual (first (first role-assertion)))
                (add-concept-assertion abox (first (first role-assertion)) 'top))
              (forget-negated-role-assertion abox 
                                             (first (first role-assertion))
                                             (second (first role-assertion))
                                             (second role-assertion))))
    
      (loop for attribute-assertion in attribute-assertions do
            (when (eq (first (first attribute-assertion)) individual)
              (forget-constrained-assertion 
               abox
               (first (first attribute-assertion))
               (second (first attribute-assertion))
               (second attribute-assertion))))

      (loop for identity-disjointness-assertion in identity-disjointness-assertions do
            (ecase (first identity-disjointness-assertion)
              (same-individual-as 
               (when (eq (second identity-disjointness-assertion) individual)
                 (unless (eq (third identity-disjointness-assertion) individual)
                   (add-concept-assertion abox (third identity-disjointness-assertion) 'top)))
               (when (eq (third identity-disjointness-assertion) individual)
                 (unless (eq (second identity-disjointness-assertion) individual)
                   (add-concept-assertion abox (second identity-disjointness-assertion) 'top)))
               (forget-same-individual-as-assertion abox (second identity-disjointness-assertion)
                                                    (third identity-disjointness-assertion)))
              (different-from 
               (when (eq (second identity-disjointness-assertion) individual)
                 (unless (eq (third identity-disjointness-assertion) individual)
                   (add-concept-assertion abox (third identity-disjointness-assertion) 'top)))
               (when (eq (third identity-disjointness-assertion) individual)
                 (unless (eq (second identity-disjointness-assertion) individual)
                   (add-concept-assertion abox (second identity-disjointness-assertion) 'top)))
               (forget-different-from-assertion abox (second identity-disjointness-assertion)
                                                (third identity-disjointness-assertion)))
              (all-different 
               (when (member individual (rest identity-disjointness-assertion))
                 ;; We delete the ind destructively here (and hope for the best :-)
                 (setf identity-disjointness-assertion ; Added setf identity-disjointness-assertion RM (Apr. 2014)
                       (delete individual identity-disjointness-assertion))))))

      (loop for annotation-individual-axiom in annotation-individual-axioms do
            (when (eq (first annotation-individual-axiom) individual)
              (forget-annotation-concept-assertion abox individual (second annotation-individual-axiom))))

      (loop for concept-assertion in concept-assertions do
            (when (eq (first concept-assertion) individual)
              (forget-concept-assertion abox 
                                        (first concept-assertion)
                                        (second concept-assertion))))))

  (check-nrql-subscriptions (abox-name abox))

  (values))


(defun forget-all-different-assertion (abox individual-name-set)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (loop for name in individual-name-set do
        (check-type name symbol))
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (setf (abox-individual-identity-disjointness-assertions abox)
    (delete `(all-different ,@individual-name-set)
	    (abox-individual-identity-disjointness-assertions abox)
	    :test #'equal))

  (check-nrql-subscriptions (abox-name abox))

  (values))

(defun forget-different-from-assertion (abox individual-1 individual-2)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type individual-1 symbol)
  (check-type individual-2 symbol)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (setf (abox-individual-identity-disjointness-assertions abox)
    (delete `(different-from ,individual-1 ,individual-2)
	    (abox-individual-identity-disjointness-assertions abox)
	    :test #'equal))

  (check-nrql-subscriptions (abox-name abox))

  (values))

(defun forget-same-individual-as-assertion (abox individual-1 individual-2)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type individual-1 symbol)
  (check-type individual-2 symbol)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (setf (abox-individual-identity-disjointness-assertions abox)
        (delete `(same-individual-as ,individual-1 ,individual-2)
                (abox-individual-identity-disjointness-assertions abox)
                :test #'equal))

  (check-nrql-subscriptions (abox-name abox))

  (values))

(defun forget-datatype-role-filler (abox individual value role)
  (check-type abox (or abox symbol))
  (check-type role symbol)
  (setf abox (find-abox abox))
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (let* ((tbox (tbox abox))
         (index (tbox-role-axioms-index tbox))
         (entry (gethash role index)))
    (unless (consp value)
      (setf value `(d-literal ,value nil)))
    (if (and entry (role-info-annotation-p entry))
        (forget-annotation-concept-assertion abox individual
                                             `(d-filler ,role ,value))
      (forget-concept-assertion abox individual
                                `(d-filler ,role ,value)))))

(defun forget-negative-datatype-role-filler (abox individual value role)
  (check-type abox (or abox symbol))
  (check-type role symbol)
  (setf abox (find-abox abox))
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (let* ((tbox (tbox abox))
         (index (tbox-role-axioms-index tbox))
         (entry (gethash role index)))
    (unless (consp value)
      (setf value `(d-literal ,value nil)))
    (if (and entry (role-info-annotation-p entry))
        (error "Negative annotation property assertions are not supported.")
      (forget-concept-assertion abox individual
                                `(not (d-filler ,role ,value))))))


(defun forget-negated-role-assertion (abox predecessor-name filler-name role-term)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type predecessor-name symbol)
  (check-type filler-name symbol)
  (check-role-term role-term)
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox))
  (let ((tuple (list predecessor-name filler-name)))
    (setf (abox-negated-role-axioms abox)
          (delete (parse-role-axiom (list tuple role-term))
                  (abox-negated-role-axioms abox)
                  :test #'equal))
    (when (or (not *use-less-abox-memory*) (abox-negated-role-axioms-index abox))
      (let ((entry (gethash tuple (abox-negated-role-axioms-index abox))))
        (when entry
          (setf entry (delete role-term entry))
          (if entry
            (setf (gethash tuple (abox-negated-role-axioms-index abox)) entry)
            (remhash tuple (abox-negated-role-axioms-index abox)))))))

  (check-nrql-subscriptions (abox-name abox))

  (values))  


;;;
;;;
;;;

(defun store-tbox-image (filename &optional (tbox *current-tbox*))
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (make-object-persistent tbox filename)
  nil)

(defun store-tboxes-image (tboxes filename)
  (make-object-persistent 
   (loop for tbox in tboxes
         do (check-type tbox (or symbol tbox))
         collect (find-tbox tbox))
   filename)
  nil)

;;;
;;;
;;;

(defun restore-tbox-image (filename)
  (with-environment ()
    (let ((tbox (load-persistent-object filename :initialization-protocol nil)))
      (check-type tbox tbox)
      (setf *current-tbox* tbox)
      (setf (find-tbox (tbox-name *current-tbox*))
            *current-tbox*)
      nil)))

(defun restore-tboxes-image (filename)
  (with-environment ()
    (let ((tboxes (load-persistent-object filename)))
      (loop for tbox in tboxes do
            (check-type tbox tbox))
      (loop for tbox in tboxes do
            (setf (find-tbox (tbox-name tbox)) tbox))
      (setf *current-tbox* (first (last tboxes))))
    nil))
          
;;;
;;;
;;;

(defun store-abox-image (filename &optional (abox *current-abox*))
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (make-object-persistent abox filename)
  nil)

(defun store-aboxes-image (filename aboxes)
  (make-object-persistent 
   (loop for abox in aboxes
        do (check-type abox (or symbol abox))
        collect (find-abox abox))
   filename)
  nil)

(defun restore-abox-image (filename)
  (with-environment ()
    (let ((abox (load-persistent-object filename)))
      (check-type abox abox)
      (setf *current-abox* abox)
      (setf *current-tbox* (tbox *current-abox*))
      (setf (find-abox (abox-name *current-abox*)) *current-abox*)
      (setf (find-tbox (tbox-name (tbox *current-abox*))) *current-tbox*)
      nil)))

(defun restore-aboxes-image (filename)
  (with-environment ()
    (let ((aboxes (load-persistent-object filename)))
      (loop for abox in aboxes do
            (check-type abox abox))
      (loop for abox in aboxes do
            (setf (find-abox (abox-name abox)) abox)
            (setf (find-tbox (tbox-name (tbox abox))) (tbox abox)))
      (when aboxes
	(setf *current-abox* (first (last aboxes)))
	(setf *current-tbox* (tbox *current-abox*))))
    nil))

;;;
;;;
;;;

(defun store-kb-image (filename &optional (kb *current-abox*))
  (store-abox-image filename (find-abox kb)))

(defun restore-kb-image (filename)
  (restore-abox-image filename))

(defun store-kbs-image (filename kbs)
  (store-aboxes-image filename kbs))

(defun restore-kbs-image (filename)
  (restore-aboxes-image filename))


;;; ======================================================================

(defun kb-ontologies (kb-name)
  (check-type kb-name symbol)
  (loop for ontology in (tbox-ontologies (find-tbox kb-name))
        collect
        (intern (ontology-name ontology))))



;;; ======================================================================

(defun get-kb-signature (kb-name)
  (check-type kb-name symbol)
  (let ((tbox (find-tbox kb-name nil))
        (abox (find-abox kb-name nil))
        (tbox-part nil)
        (abox-part nil))
    (unless (and tbox abox)
      (error "No such knowledge base ~A declared." kb-name))
    (when (tbox-signature tbox)
      (destructuring-bind (atomic-concepts
                           roles
                           transitive-roles
                           features
                           attributes)
                          (tbox-signature tbox)
        (setf tbox-part
              `(:atomic-concepts ,atomic-concepts
                                 :roles ,roles
                                 :transitive-roles ,transitive-roles
                                 :features ,features
                                 :attributes ,attributes))))
    (when (abox-signature abox)
      (destructuring-bind (individuals objects)
                          (abox-signature abox)
        (setf abox-part
              `(:individuals ,individuals :objects ,objects))))
    (if (or tbox-part abox-part)
      `(signature ,@tbox-part ,.abox-part)
      nil)))
                        


;;; ======================================================================


(defun check-concept-coherence-internal (concept &optional (tbox *current-tbox*))
  (setf tbox (find-tbox tbox))
  (with-race-trace-sublevel ("check-concept-coherence-internal"
                             :arguments (list concept tbox)
                             :trace-result t)
    (let ((abox (in-abox-internal (gensym "INTERNAL") (tbox-name tbox) t)))
      (add-concept-assertion abox :ref-ind concept)
      (prog1
          (second (check-abox-coherence abox))
        (forget-abox abox)))))

(defun check-ontology-internal (filename &key (verbose *tbox-verbose*) (explain-all nil) (n 1))
  (let* ((*tbox-verbose* verbose)
         (tbox (owl-read-file filename))
         (i 0))
    (setf tbox (find-tbox tbox))
    (ensure-knowledge-base-state ':tbox-prepared tbox)
    (let ((result nil)
          (counter 0))
      (racer:with-progress-range ((length (all-atomic-concepts tbox)) (0 100))
        (with-concept-definition-mapping tbox
          (loop for concept-name in (all-atomic-concepts tbox) 
                do 
                (racer:set-progress-value (incf counter))
                (unless (or (eq concept-name '*bottom*) (eq concept-name 'bottom))
                  (let ((concept (get-tbox-concept tbox concept-name)))
                    (unless (test-atomic-concept-satisfiable tbox concept)
                      (incf i)
                      (push (list :incoherent-concept concept-name 
                                  (check-concept-coherence-internal concept-name)) result)
                      (setf *tbox-verbose* nil)
                      (unless explain-all 
                        (unless (<= i n) 
                          (return-from check-ontology-internal (nreverse result))))))))))
      (nreverse result))))

