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

(defconstant +racer-internal-datatype-property-roles+  
  '(racer-internal%has-string-value
    racer-internal%has-integer-value 
    racer-internal%has-cardinal-value 
    racer-internal%has-real-value 
    racer-internal%has-boolean-value))

(defconstant +racer-substrate-default-tbox+ 'default)

;;;
;;;
;;;

(defconstant +cache-is-complete-marker+ :cache-complete-dvfkvf8974)

(defconstant +individual-exists-concept+ :individual-exists-7894jkjoif)

;;;
;;;
;;;

(defvar *racer-tbox* nil)

(defvar *racer-abox* nil)

;;;
;;;
;;;
;;;

(defconstant +default-hash-table-size+ 100)

#+:dlmaps
(defun mht (&key (size +default-hash-table-size+) (test #'eql))
  (make-hash-table              :size size :rehash-size 2.0 :test test))

#+:racer
(defun mht (&key (size +default-hash-table-size+) (test #'eql))
  (racer-make-hash-table :size size :rehash-size 2.0 :test test))

(defun mht2 (&key (size +default-hash-table-size+) (test #'eql))
  (make-hash-table              :size size :rehash-size 2.0 :test test))

;;;
;;;
;;;

(defconstant +table-size-concept-assertions-cache+ 3000)

(defconstant +table-size-individual-synonyms-cache+ 3000)

(defconstant +table-size-individual-same-as-cache+ 3000)

(defconstant +table-size-constraint-entailed-cache+ 3000)

(defconstant +table-size-concept-instances-cache+ 3000)

(defconstant +table-size-known-concept-instances-cache+ 3000)

(defconstant +table-size-individual-instance-cache+ 10000)

(defconstant +table-size-individual-attribute-fillers-cache+ 3000)

(defconstant +table-size-role-assertions-in-range-cache+ 10000)

(defconstant +table-size-role-assertions-in-domain-cache+ 10000)

(defconstant +table-size-role-descendants-cache+ 100)

(defconstant +table-size-role-ancestors-cache+ 100)

(defconstant +table-size-role-children-cache+ 10)

(defconstant +table-size-role-parents-cache+ 10)

(defconstant +table-size-atomic-role-inverse-cache+ 100)

(defconstant +table-size-role-transitive-or-transitive-subrole-cache+ 100)

(defconstant +table-size-role-transitive-or-transitive-superrole-cache+ 100)

(defconstant +table-size-role-transitive-or-transitive-superrole-of-subrole-cache+ 100)

(defconstant +table-size-role-successors-cache+ 100)

(defconstant +table-size-new-inds-hash+ 100)

(defconstant +table-size-datatype-property-values-and-types-cache+ 100)

(defconstant +table-size-told-values-cache+ 100)

;;;
;;;
;;;

(defpersistentclass racer-descriptions-substrate (substrate)

  ;;; Knoten und Kanten mit Racer-Descriptions

  ((tbox :initarg :tbox :reader tbox :initform nil)
   (racer-package :accessor racer-package :initarg :racer-package :initform *package* )))


(defpersistentclass dl-prover-substrate (substrate) 

  ;;; Knoten und Kanten mit Descriptions, optionale assoziierte ABox-Objekte!

  ((qbox :initarg :qbox :reader qbox :initform nil)
   (dbox :initarg :dbox :reader dbox :initform nil)

   (prepared-p :accessor prepared-p :initform nil)

   (saved-state-vector :accessor saved-state-vector :initform nil)
   
   (needs-filler-reasoning-p :reader needs-filler-reasoning-p :initform nil)

   (new-inds-hash :accessor new-inds-hash :initform (mht :size +table-size-new-inds-hash+ :test #'equal))

   (abox-individuals-cache :accessor abox-individuals-cache :initform :unknown)

   (concept-assertions-cache :accessor concept-assertions-cache 
                             :initform (mht :size +table-size-concept-assertions-cache+))

   ;;;
   ;;; 
   ;;; 

   (concept-instances-cache :accessor concept-instances-cache
                            :initform (mht :size +table-size-concept-instances-cache+ :test #'equal))

   (known-concept-instances-cache :accessor known-concept-instances-cache
                                  :initform (mht :size +table-size-known-concept-instances-cache+ :test #'equal))

   (individual-instance-cache :accessor individual-instance-cache
                              :initform (mht :test #'equal :size +table-size-individual-instance-cache+))

   (individual-synonyms-cache :accessor individual-synonyms-cache
                              :initform (mht :test #'equal :size +table-size-individual-synonyms-cache+))

   (individual-same-as-cache :accessor individual-same-as-cache
			     :initform (mht :test #'equal :size +table-size-individual-same-as-cache+))

   (same-as-assertions-present-p :accessor same-as-assertions-present-p :initform nil)
   
   (role-assertions-in-range-cache :accessor role-assertions-in-range-cache
                                   :initform (mht :size +table-size-role-assertions-in-range-cache+))

   (role-assertions-in-domain-cache :accessor role-assertions-in-domain-cache
                                    :initform (mht :size +table-size-role-assertions-in-domain-cache+))

   (role-descendants-cache :accessor role-descendants-cache
                           :initform (mht :size +table-size-role-descendants-cache+ :test #'equal))

   (role-ancestors-cache :accessor role-ancestors-cache
                         :initform (mht :size +table-size-role-ancestors-cache+ :test #'equal))

   (role-children-cache :accessor role-children-cache
                           :initform (mht :size +table-size-role-children-cache+ :test #'equal))

   (role-parents-cache :accessor role-parents-cache
                         :initform (mht :size +table-size-role-parents-cache+ :test #'equal))

   (role-successors-cache :accessor role-successors-cache
                          :initform (mht :size +table-size-role-successors-cache+ :test #'equal))

   (atomic-role-inverse-cache :accessor atomic-role-inverse-cache
                              :initform (mht :size +table-size-atomic-role-inverse-cache+ :test #'equal))

   (all-roles-cache :accessor all-roles-cache :initform :unknown)

   (transitive-roles-cache :accessor transitive-roles-cache :initform :unknown)

   (role-transitive-or-transitive-subrole-cache :accessor role-transitive-or-transitive-subrole-cache 
                                                :initform (mht :size +table-size-role-transitive-or-transitive-subrole-cache+))

   (role-transitive-or-transitive-superrole-cache :accessor role-transitive-or-transitive-superrole-cache
                                                  :initform (mht :size +table-size-role-transitive-or-transitive-superrole-cache+))

   (role-transitive-or-transitive-superrole-of-subrole-cache :accessor role-transitive-or-transitive-superrole-of-subrole-cache
                                                             :initform (mht :size +table-size-role-transitive-or-transitive-superrole-of-subrole-cache+))

   ;;;
   ;;; die folgenden Slots / Hashtabellen dienen dazu, die Kommunikation mit
   ;;; dem RACER-Server zu beschleunigen (Cache fuer haeuftig gestellte Anfragen!) 
   ;;; 

   (constraints-cache :accessor constraints-cache :initform :unknown)

   (told-values-cache :accessor told-values-cache :initform (mht :size +table-size-told-values-cache+ :test #'eql))

   (datatype-property-values-and-types-cache :accessor datatype-property-values-and-types-cache
                                             :initform (mht :test #'equal :size +table-size-datatype-property-values-and-types-cache+))

   (constraint-entailed-cache :accessor constraint-entailed-cache
                              :initform (mht :test #'equal :size +table-size-constraint-entailed-cache+))

   (individual-attribute-fillers-cache :accessor individual-attribute-fillers-cache
                                       :initform (mht :test #'equal :size +table-size-individual-attribute-fillers-cache+))

   ;;;
   ;;; 
   ;;; 

   (subscribers :accessor subscribers :initform nil)))


(defpersistentclass racer-substrate (dl-prover-substrate racer-descriptions-substrate)
  ;;; Knoten und Kanten mit Descriptions, optionale assoziierte ABox-Objekte!
  ((tbox :initarg :tbox :reader tbox :initform nil)
   (abox :initarg :abox :reader abox :initform nil)))


(defpersistentclass racer-dummy-substrate (racer-substrate) nil)
  ;;; nur fuer Ralf, keine Substrat-Objekte, nur ABox
  ;;; -> keine Substrate-Objekte-Unterklassen erforderlich, klar



#+:midelora
(defpersistentclass midelora-substrate (dl-prover-substrate prover::abox)
  ((abox :accessor abox)
   (tbox :accessor tbox)))

#+:midelora
(defpersistentclass midelora-substrate1 (midelora-substrate prover::abox1))

#+:midelora
(defmethod update-instance-for-different-class :after ((old prover::abox) (substrate midelora-substrate) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value substrate 'abox) substrate
        (slot-value substrate 'tbox) (prover::tbox substrate))
  (reset-substrate substrate))


(defpersistentclass tbox-mirror-substrate (dl-prover-substrate))

(defpersistentclass racer-tbox-mirror-substrate (tbox-mirror-substrate racer-dummy-substrate)
  ((mirror-of-tbox :reader mirror-of-tbox)
   (needs-filler-reasoning-p :initform nil)))

#+:midelora
(defpersistentclass midelora-tbox-mirror-substrate (tbox-mirror-substrate midelora-substrate)
  ((mirror-of-tbox :reader mirror-of-tbox)
   (needs-filler-reasoning-p :initform nil)))
  
;;;
;;;
;;;

(defmethod establish-context-for ((substrate racer-substrate) continuation)
  (let ((*racer-tbox* (tbox substrate))
        (*racer-abox* (abox substrate)))
    (if (next-method-p)
        (call-next-method)
      (funcall continuation))))

#-:dlmaps
(defmethod set-context-for ((substrate racer-substrate))
  (setf *racer-tbox* (tbox substrate)
        *racer-abox* (abox substrate)))

#+:dlmaps
(defmethod set-context-for progn ((substrate racer-substrate))
  (setf *racer-tbox* (tbox substrate)
        *racer-abox* (abox substrate)))

;;;
;;;
;;;

(defmethod reset-substrate :before ((substrate dl-prover-substrate) &key &allow-other-keys)
  (without-timeout
    (setf (saved-state-vector substrate) nil)
    (setf (prepared-p substrate) nil)))

(defmethod reset-substrate :after ((substrate dl-prover-substrate) &key &allow-other-keys)
  (without-timeout
    (setf (saved-state-vector substrate) 
          (get-state-vector substrate))
   
    (setf (prepared-p substrate) t)))
  
;;;
;;;
;;;

(defmethod reset-substrate ((substrate racer-substrate) &key &allow-other-keys)
  (clear-repository substrate)
  (reset-caches substrate)
  (if (find-tbox (tbox substrate) nil)
      (if (find-abox (abox substrate) nil)
          (compute-abox-mirror substrate)
        (nrql-error "Can't find ABox ~A" (abox substrate)))
    (nrql-error "Can't find TBox ~A" (tbox substrate))))

#+:midelora
(defmethod reset-substrate ((substrate midelora-substrate) &key &allow-other-keys)
  (clear-repository substrate)
  (reset-caches substrate)
  (compute-abox-mirror substrate))

(defmethod reset-substrate ((substrate racer-tbox-mirror-substrate) &key tbox)
  (reset-caches substrate)
  (setf (slot-value substrate 'tbox) 
        (or tbox
            (mirror-of-tbox substrate)))
  (if (find-tbox (tbox substrate) nil)
      (compute-tbox-mirror substrate)
    (nrql-error "Can't find TBox ~A" (tbox substrate))))

#+:midelora
(defmethod reset-substrate ((substrate midelora-tbox-mirror-substrate) &key tbox)
  (declare (ignorable tbox))
  (reset-caches substrate)
  
  (setf (slot-value substrate 'tbox) 
        (or tbox
            (mirror-of-tbox substrate)))

  (if (prover::find-tbox (tbox substrate) :error-p nil)
      (compute-tbox-mirror substrate)
    (nrql-error "Can't find TBox ~A" (tbox substrate))))


;;;
;;;
;;;


(defmethod reset-caches ((substrate dl-prover-substrate))
  (setf (constraints-cache substrate) :unknown)
  (setf (abox-individuals-cache substrate) :unknown)                        
  (setf (transitive-roles-cache substrate) :unknown)
  (setf (all-roles-cache substrate) :unknown)
  (setf (same-as-assertions-present-p substrate) nil)  

  (dolist (slot '(concept-assertions-cache
                  concept-instances-cache
                  known-concept-instances-cache
                  individual-instance-cache
                  individual-synonyms-cache
                  individual-same-as-cache
                  role-assertions-in-range-cache
                  role-assertions-in-domain-cache
                  role-descendants-cache
                  role-children-cache
                  role-ancestors-cache
                  role-successors-cache
                  role-parents-cache
                  atomic-role-inverse-cache
                  role-transitive-or-transitive-subrole-cache
                  role-transitive-or-transitive-superrole-cache
                  role-transitive-or-transitive-superrole-of-subrole-cache
                  
                  told-values-cache
                  datatype-property-values-and-types-cache
                  
                  constraint-entailed-cache
                  individual-attribute-fillers-cache

                  new-inds-hash))
    
    (clrhash (slot-value substrate slot))))
   
;;;
;;; DL-Prover-Ankopplung per Delegation 
;;;

(defmethod dl-all-individuals ((substrate racer-substrate) &rest args)
  (apply #'all-individuals args))

#+:midelora
(defmethod dl-all-individuals ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-individuals args))


(defmethod dl-all-roles ((substrate racer-substrate) &rest args)
  (apply #'all-roles args))

#+:midelora
(defmethod dl-all-roles ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-roles args))



(defmethod dl-all-features ((substrate racer-substrate) &rest args)
  (apply #'all-features args))

#+:midelora
(defmethod dl-all-features ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-features args))



(defmethod dl-all-transitive-roles ((substrate racer-substrate) &rest args)
  (apply #'all-transitive-roles args))

#+:midelora
(defmethod dl-all-transitive-roles ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-transitive-roles args))


(defmethod dl-all-reflexive-roles ((substrate racer-substrate) &rest args)
  (remove-if-not #'(lambda (r) 
                     (and (symbolp r)
                          (reflexive-p r (tbox substrate))))
                 (apply #'dl-all-roles substrate (tbox substrate) args)))


(defmethod dl-all-constraints ((substrate racer-substrate) &rest args)
  (apply #'all-constraints args))

#+:midelora
(defmethod dl-all-constraints ((substrate midelora-substrate) &rest args)
  (declare (ignorable args))
  nil)


(defmethod dl-all-concept-assertions-for-individual ((substrate racer-substrate) &rest args)
  (apply #'all-concept-assertions-for-individual args))

#+:midelora
(defmethod dl-all-concept-assertions-for-individual ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-concept-assertions-for-individual args))



(defmethod dl-retrieve-concept-instances ((substrate racer-substrate) concept abox 
                                          &key (candidates nil candidates-supplied-p) 
                                          &allow-other-keys)
  (if candidates-supplied-p 
      (retrieve-concept-instances concept abox candidates)
    (retrieve-concept-instances concept abox)))
    

#+:midelora
(defmethod dl-retrieve-concept-instances ((substrate midelora-substrate) concept abox 
                                          &rest args &key &allow-other-keys)
  (apply #'prover::get-concept-instances concept abox args))


(defmethod dl-individual-p ((substrate racer-substrate) &rest args)
  (apply #'individual-p args))

#+:midelora
(defmethod dl-individual-p ((substrate midelora-substrate) &rest args)
  (apply #'prover::individual-p args))


(defmethod dl-individual-instance-p ((substrate racer-substrate) &rest args)
  (apply #'individual-instance-p args))

#+:midelora
(defmethod dl-individual-instance-p ((substrate midelora-substrate) &rest args)
  (apply #'prover::individual-instance-p args))


(defmethod dl-constraint-entailed-p ((substrate racer-substrate) &rest args)
  (apply #'constraint-entailed-p args))

#+:midelora
(defmethod dl-constraint-entailed-p ((substrate midelora-substrate) &rest args)
  nil)


(defmethod dl-retrieve-individual-attribute-fillers ((substrate racer-substrate) &rest args)
  (apply #'retrieve-individual-attribute-fillers args))

#+:midelora
(defmethod dl-retrieve-individual-attribute-fillers ((substrate midelora-substrate) &rest args)
  nil)


(defmethod dl-all-role-assertions-for-individual-in-range ((substrate racer-substrate) &rest args)
  (append (apply #'all-role-assertions-for-individual-in-range args)
          (let ((ind (first args)))
            (mapcar #'(lambda (r) 
                        (list (list ind ind) r))
                    (dl-all-reflexive-roles substrate)))))

#+:midelora
(defmethod dl-all-role-assertions-for-individual-in-range ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-role-assertions-for-individual-in-range args))


(defmethod dl-all-role-assertions-for-individual-in-domain ((substrate racer-substrate) &rest args)
  (append (apply #'all-role-assertions-for-individual-in-domain args)
          (let ((ind (first args)))
            (mapcar #'(lambda (r) 
                        (list (list ind ind) r))
                    (dl-all-reflexive-roles substrate)))))

#+:midelora
(defmethod dl-all-role-assertions-for-individual-in-domain ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-role-assertions-for-individual-in-domain args))

(defmethod dl-atomic-role-descendants ((substrate racer-substrate) &rest args)
  (remove +bottom-object-role-symbol+ (apply #'atomic-role-descendants-internal args)))

#+:midelora
(defmethod dl-atomic-role-descendants ((substrate midelora-substrate) &rest args)
  (apply #'prover::atomic-role-descendants args))

(defmethod dl-atomic-role-children ((substrate racer-substrate) &rest args)
  (remove +bottom-object-role-symbol+ (apply #'atomic-role-children args)))


(defmethod dl-atomic-role-ancestors ((substrate racer-substrate) &rest args)
  (remove  +top-object-role-symbol+ (apply #'atomic-role-ancestors-internal args)))

#+:midelora
(defmethod dl-atomic-role-ancestors ((substrate midelora-substrate) &rest args)
  (apply #'prover::atomic-role-ancestors args))

(defmethod dl-atomic-role-parents ((substrate racer-substrate) &rest args)
  (remove +top-object-role-symbol+ (apply #'atomic-role-parents args)))


(defmethod dl-atomic-role-inverse ((substrate racer-substrate) &rest args)
  (apply #'atomic-role-inverse args))

#+:midelora
(defmethod dl-atomic-role-inverse ((substrate midelora-substrate) &rest args)
  (apply #'prover::atomic-role-inverse args))


(defmethod dl-all-role-assertions ((substrate racer-substrate) &rest args)
  (apply #'all-role-assertions args))

#+:midelora
(defmethod dl-all-role-assertions ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-role-assertions args))



(defmethod dl-all-annotation-role-assertions  ((substrate racer-substrate) &rest args)
  (apply #'all-annotation-role-assertions args))
  
#+:midelora
(defmethod dl-all-annotation-role-assertions  ((substrate midelora-substrate) &rest args)
  nil)


(defmethod dl-all-attribute-assertions ((substrate racer-substrate) &rest args)
  (apply #'all-attribute-assertions args))

#+:midelora
(defmethod dl-all-attribute-assertions  ((substrate midelora-substrate) &rest args)
  nil)
  

(defmethod dl-tbox-classified-p ((substrate racer-descriptions-substrate) &rest args)
  (apply #'tbox-classified-p args))

#+:midelora
(defmethod dl-tbox-classified-p ((substrate midelora-substrate) &rest args)
  (apply #'prover::tbox-classified-p args))


(defmethod dl-tbox-coherent-p ((substrate racer-descriptions-substrate) &rest args)
  (apply #'tbox-coherent-p args))

#+:midelora
(defmethod dl-tbox-coheren-p ((substrate midelora-substrate) &rest args)
  (apply #'prover::tbox-coherent-p args))



(defmethod dl-classify-tbox ((substrate racer-descriptions-substrate) &rest args)
  (apply #'classify-tbox args))

#+:midelora
(defmethod dl-classify-tbox ((substrate midelora-substrate) &rest args)
  (apply #'prover:classify-tbox args))


(defmethod dl-taxonomy ((substrate racer-descriptions-substrate) &rest args)
  (apply #'taxonomy (tbox substrate) args))

#+:midelora
(defmethod dl-taxonomy ((substrate midelora-substrate) &rest args)
  (to-be-implemented 'dl-taxonomy))



(defmethod dl-all-annotation-concept-assertions ((substrate racer-substrate) &rest args)
  (apply #'all-annotation-concept-assertions args))

#+:midelora
(defmethod dl-all-annotation-concept-assertions ((substrate midelora-substrate) &rest args)
  nil)


(defmethod dl-atomic-concept-ancestors ((substrate racer-substrate) &rest args)
  (apply #'atomic-concept-ancestors args))

#+:midelora
(defmethod dl-atomic-concept-ancestors ((substrate midelora-substrate) &rest args)
  nil)


(defmethod dl-atomic-concept-synonyms ((substrate racer-substrate) &rest args)
  (apply #'atomic-concept-synonyms args))

#+:midelora
(defmethod dl-atomic-concept-synonyms ((substrate midelora-substrate) &rest args)
  (apply #'prover::atomic-concept-synonyms args))


(defmethod dl-all-concept-assertions ((substrate racer-substrate) &rest args)
  (apply #'all-concept-assertions args))

#+:midelora
(defmethod dl-all-concept-assertions ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-concept-assertions args))

#+:racer-server
(defmethod dl-all-same-as-assertions ((substrate racer-substrate) &rest args)
  (remove-if-not #'(lambda (x) 
                     (eq (first x) 'same-individual-as))
                 (apply #'abox-individual-identity-disjointness-assertions (find-abox (abox substrate)) args)))

#-:racer-server
(defmethod dl-all-same-as-assertions ((substrate racer-substrate) &rest args)
  nil)

#+:midelora
(defmethod dl-all-same-as-assertions ((substrate midelora-substrate) &rest args)
  nil)


#+:racer-server
(defmethod dl-all-different-from-assertions ((substrate racer-substrate) &rest args)
  (remove-if-not #'(lambda (x) 
                     (eq (first x) 'different-from))
                 (apply #'abox-individual-identity-disjointness-assertions (find-abox (abox substrate)) args)))


#-:racer-server
(defmethod dl-all-different-from-assertions ((substrate racer-substrate) &rest args)
  nil)



#+:midelora
(defmethod dl-all-different-from-assertions ((substrate midelora-substrate) &rest args)
  nil)

;;;
;;;
;;;

(defmethod retrieve-datatype-values ((substrate racer-substrate) ind property &optional with-types-p)
  (loop as ((prop value orig-value) annotation-p) in
        (gethash ind (datatype-property-values-and-types-cache substrate))
        when (and (not annotation-p)
                  (eq prop property))
        collect (if (and with-types-p orig-value)
                    orig-value
                  value)))

#+:midelora
(defmethod retrieve-datatype-values ((substrate midelora-substrate) ind property &optional with-types-p)
  (declare (ignore ind property with-types-p))
  nil)


(defmethod retrieve-annotation-datatype-values ((substrate racer-substrate) ind property &optional with-types-p)
  (loop as ((prop value orig-value) annotation-p) in
        (gethash ind (datatype-property-values-and-types-cache substrate))
        when (and annotation-p
                  (eq prop property))
        collect (if (and with-types-p orig-value)
                    orig-value
                  value)))

#+:midelora
(defmethod retrieve-annotation-datatype-values ((substrate midelora-substrate) ind property &optional with-types-p)
  (declare (ignore ind property with-types-p))
  nil)


;;;
;;; Hoeherwertige Funktion 
;;; Abstraktionen
;;; nur diese Funktionen sollen im Query-Evaluation-Code verwendet werden!
;;; 

(defmethod dl-prover-internal-individuals-related-p ((substrate racer-substrate) &rest args)
  (apply #'internal-individuals-related-p args))


#+:midelora
(defmethod dl-prover-internal-individuals-related-p ((substrate midelora-substrate) &rest args)
  (apply #'prover::individuals-related-p args))


(defmethod dl-prover-individual-synonyms ((substrate racer-substrate) ind)
  (with-critical-section 
    (with-slots (individual-synonyms-cache
                 individual-same-as-cache
                 needs-filler-reasoning-p) substrate
	
      (if (not *use-individual-synonyms-p*)

          (list ind)
		
        (if (or *told-information-reasoning-p*
                (not needs-filler-reasoning-p))
	          
            (or (gethash ind individual-same-as-cache)
                (list ind))
		   
          (multiple-value-bind (syns found-p)
              (gethash ind individual-synonyms-cache)
        
            (if found-p 
	    
                syns

              (let ((res
                     (sort (copy-list (retrieve-individual-synonyms ind nil (abox substrate)))
                           #'string-lessp
                           :key #'symbol-name)))
		
                ;;; Sortierung erforderlich, damit Duplikate erkannt werden (equal-Hashtabellen) 
                ;;; das sollte nicht viel Zeit kosten, da das Ergebnis dann gecached wird 
                ;;; und die Listen der Synonyme i.d.R. klein sein sollten. 
		
                ;;; natuerlich sind hier auch die SAME-AS-Synonyme (OWL) enthalten, ebenso das Individum selbst
		
                (setf (gethash ind individual-synonyms-cache) res)
		
                res))))))))

#+:midelora
(defmethod dl-prover-individual-synonyms ((substrate midelora-substrate) ind)
  (declare (ignorable ind))
  (list ind))

;;;
;;;
;;;


(defmethod dl-prover-individual-synonyms1 ((substrate racer-substrate) ind)
  ;;; wird nur fuer evaluate-dl-prover-retrieve-individual-fillers benoetigt! 
  (let ((*use-individual-synonyms-p* t))
    (dl-prover-individual-synonyms substrate ind)))
				 
				 
#+:midelora
(defmethod dl-prover-individual-synonyms1 ((substrate midelora-substrate) ind)
  (declare (ignorable ind))
  (list ind))

;;;
;;;
;;;

(defmethod dl-prover-individual-antonyms ((substrate racer-substrate) ind)
  ;;; noch nicht effizient!
  (let ((res nil)
	(abox (current-abox)))
    
    (dolist (ind2 (dl-prover-all-individuals substrate))

      (let ((b1 (create-abox-clone (abox substrate) :copy-rules nil)))
        
        (add-same-individual-as-assertion b1 ind ind2)
        
        (unless (abox-consistent-p b1)
          (push ind2 res))

        (forget-abox b1)))
    
    (set-current-abox abox)

    res))

#|

(defmethod dl-prover-individual-antonyms ((substrate racer-substrate) ind)
  ;;; noch nicht implementiert 
  (retrieve-individual-antonyms ind nil (abox substrate)))  

|#


#+:midelora
(defmethod dl-prover-individual-antonyms ((substrate midelora-substrate) ind)
  (declare (ignorable ind))
  (to-be-implemented))

;;;
;;;
;;;


(defmethod dl-prover-all-same-as-assertions ((substrate dl-prover-substrate))
  (dl-all-same-as-assertions substrate))

(defmethod dl-prover-all-different-from-assertions ((substrate dl-prover-substrate))
  (dl-all-different-from-assertions substrate))
  
#+:midelora
(defmethod dl-prover-all-same-as-assertions ((substrate midelora-substrate))
  nil)

#+:midelora
(defmethod dl-prover-all-different-from-assertions ((substrate midelora-substrate))
  nil)

(defmethod dl-prover-abox-consistent-p ((substrate racer-substrate) &rest args)
  (apply #'abox-consistent-p args))

#+:midelora
(defmethod dl-prover-abox-consistent-p ((substrate midelora-substrate) &rest args)
  (apply #'prover::abox-consistent-p args))


(defmethod dl-prover-transitive-role-p ((substrate dl-prover-substrate) role)
  (with-critical-section 
    (when (eq (transitive-roles-cache substrate) :unknown)
      (setf (transitive-roles-cache substrate) 
            (dl-all-transitive-roles substrate (tbox substrate))))

    (member role (transitive-roles-cache substrate)
            :test #'equal)))

(defmethod dl-prover-role-transitive-or-transitive-subrole-p ((substrate dl-prover-substrate) role)
  (with-critical-section 
    (multiple-value-bind (res found)
        (gethash role (role-transitive-or-transitive-subrole-cache substrate))
      (if found
          res
        (setf (gethash role (role-transitive-or-transitive-subrole-cache substrate))
              (some #'(lambda (r) 
                        (dl-prover-transitive-role-p substrate r))
                    (dl-prover-atomic-role-descendants substrate role)))))))


(defmethod dl-prover-role-transitive-or-transitive-superrole-p ((substrate dl-prover-substrate) role)
  (with-critical-section 
    (multiple-value-bind (res found)
        (gethash role (role-transitive-or-transitive-superrole-cache substrate))
      (if found
          res
        (setf (gethash role (role-transitive-or-transitive-superrole-cache substrate))
              (some #'(lambda (r) 
                        (dl-prover-transitive-role-p substrate r))
                    (dl-prover-atomic-role-ancestors substrate role)))))))


(defmethod dl-prover-role-transitive-or-transitive-superrole-of-subrole-p ((substrate dl-prover-substrate) role superrole)
  (with-critical-section 
    (multiple-value-bind (res found)
        (gethash (list role superrole) (role-transitive-or-transitive-superrole-of-subrole-cache substrate))
      (if found
          res
        (let ((descendants (dl-prover-atomic-role-descendants substrate superrole)))
          (setf (gethash role (role-transitive-or-transitive-superrole-of-subrole-cache substrate))
                (some #'(lambda (r) 
                          (and (dl-prover-transitive-role-p substrate r)
                               (member r descendants :test #'equal))) 
                      (dl-prover-atomic-role-ancestors substrate role))))))))


(defmethod dl-prover-all-roles ((substrate dl-prover-substrate))
  (with-critical-section 
    (when (eq (all-roles-cache substrate) :unknown)
      (setf (all-roles-cache substrate) 
            (dl-all-roles substrate (tbox substrate))))

    (all-roles-cache substrate)))

(defmethod dl-prover-role-successors-cache-complete-for-role-p ((substrate dl-prover-substrate) from role)
  (with-critical-section
    (dl-prover-is-known-role-successor-of-p substrate +cache-is-complete-marker+ from role)))

(defmethod dl-prover-register-role-successors-cache-is-complete-for-role ((substrate dl-prover-substrate) from role)
  (with-critical-section
    (dl-prover-register-role-successor substrate +cache-is-complete-marker+ from role)))

(defmethod dl-prover-all-role-successors ((substrate dl-prover-substrate) from role)
  (with-critical-section
    (if (dl-prover-role-successors-cache-complete-for-role-p substrate from role)
        (let ((hash (gethash role (role-successors-cache substrate))))
          (if hash 
              (let ((succs (gethash from hash))) ; +complete-marker+ entfernen!
                ;; (princ succs)
                ;; (terpri)
                (rest succs))
            (nrql-runtime-error "Hashing problem")))
      :unknown)))

(defmethod dl-prover-is-known-role-successor-of-p ((substrate dl-prover-substrate) to from role)
  (with-critical-section

    ;;; auch wenn der Cache unvollstaendig ist! 
    ;;; er wird nur verwendet beim Suchen in der ABox "racer-retrieve-individual-fillers"
    ;;; fuer (transitive) Rollen, um Such-Endlosschleifen (Zyklen) aufzuloesen!
    ;;; 
    (let ((hash (gethash role (role-successors-cache substrate))))
      (if hash 
          (member to (gethash from hash))
        nil))))

(defmethod dl-prover-register-role-successor ((substrate dl-prover-substrate) to from role) 
  (with-critical-section

    (unless (gethash role (role-successors-cache substrate))      
      (setf (gethash role (role-successors-cache substrate))
            (mht :size 1000 :test #'equal)))

    (when (dl-prover-role-successors-cache-complete-for-role-p substrate from role)
      (nrql-runtime-error "Cache is already complete"))

    (multiple-value-bind (succs foundp)
        (gethash from (gethash role (role-successors-cache substrate)))
      (declare (ignorable succs))
      (if foundp 
          (push to (gethash from (gethash role (role-successors-cache substrate))))
        (setf (gethash from (gethash role (role-successors-cache substrate))) (list to))))))

(defmethod dl-prover-all-individuals ((substrate dl-prover-substrate))
  (with-critical-section 
    (if (eq (abox-individuals-cache substrate) :unknown)
        (unless *told-information-reasoning-p*              
          (values (setf (abox-individuals-cache substrate) 
                        (dl-all-individuals substrate (abox substrate)))
                  t))
      (values (abox-individuals-cache substrate) t))))

(defmethod dl-prover-individual-exists-p ((substrate dl-prover-substrate) ind)
  (with-critical-section 
    (dl-prover-individual-instance-p substrate ind +individual-exists-concept+)))

(defmethod dl-prover-all-constraints ((substrate dl-prover-substrate))
  (with-critical-section 
    (if (eq (constraints-cache substrate) :unknown)
        (unless *told-information-reasoning-p*
          (values (setf (constraints-cache substrate)
                        (dl-all-constraints substrate (abox substrate)))
                  t))
      (values (constraints-cache substrate) t))))

(defmethod dl-prover-all-concept-assertions-for-individual ((substrate dl-prover-substrate) ind)
  (with-critical-section
    (multiple-value-bind (val foundp)
        (gethash ind (concept-assertions-cache substrate))
      (if foundp 
          (values val t)
        (unless *told-information-reasoning-p*
          (let ((assertions 
                 (dl-all-concept-assertions-for-individual substrate ind (abox substrate))))
            
            (setf (gethash ind (concept-assertions-cache substrate)) assertions)
          
            (dolist (assertion assertions)
              (let ((concept (second assertion)))
                (setf (gethash (list ind concept) (individual-instance-cache substrate)) t)

                (if (gethash concept (known-concept-instances-cache substrate))
                    (push ind (gethash concept (known-concept-instances-cache substrate)))
                  (setf (gethash concept (known-concept-instances-cache substrate)) (list ind)))))                
        
            (values assertions t)))))))

(defmethod dl-prover-retrieve-concept-instances ((substrate dl-prover-substrate) concept 
                                                 &key (candidates nil candidates-supplied-p)
                                                 continuation)
  (with-critical-section
    (multiple-value-bind (val foundp)
        (gethash concept (concept-instances-cache substrate))

      (if foundp

          ;; zweiter Wert wichtig, um bei NIL-Ergebnis
          ;; zu sehen, dass vollstaendig bzw. bereits berechnet!

          (if continuation

              (progn 
                (dolist (x val)
                  (funcall continuation x))
                (values val t))

            (values val t))
        
        (unless *told-information-reasoning-p*       
          
          (let* ((continuation
                  #'(lambda (ind) 
                      (setf (gethash (list ind concept) (individual-instance-cache substrate)) t)
                      (when continuation
                        (funcall continuation ind))))
                 
                 (inds (if candidates-supplied-p 
                           (dl-retrieve-concept-instances substrate 
                                                          concept (abox substrate)
                                                          :candidates candidates
                                                          :continuation continuation)
                         (dl-retrieve-concept-instances substrate
                                                        concept (abox substrate)
                                                        :continuation continuation))))

            (setf (gethash concept (concept-instances-cache substrate)) inds)
              
            (values inds t)))))))


(defmethod dl-prover-retrieve-known-concept-instances ((substrate dl-prover-substrate) concept)
  (with-critical-section
    (gethash concept (known-concept-instances-cache substrate))))

(defmethod dl-prover-individual-instance-p ((substrate dl-prover-substrate) ind concept)
  (with-critical-section 
    (multiple-value-bind (val foundp)
        (gethash (list ind concept) (individual-instance-cache substrate))
      (if foundp
          (values val t)
        (unless *told-information-reasoning-p*
          (let ((res 
                 (if (eq concept +individual-exists-concept+)
                     (dl-individual-p substrate ind (abox substrate))
                   (dl-individual-instance-p substrate ind concept (abox substrate)))))
            (setf (gethash (list ind concept) (individual-instance-cache substrate)) res)
            (values res t)))))))

(defmethod dl-prover-constraint-entailed-p ((substrate dl-prover-substrate) constraint)
  (with-critical-section 
    (multiple-value-bind (val foundp)
        (gethash constraint (constraint-entailed-cache substrate))
      (if foundp
          (values val t)
        (unless *told-information-reasoning-p*
          (let ((res 
                 (dl-constraint-entailed-p substrate constraint (abox substrate))))
            (setf (gethash constraint (constraint-entailed-cache substrate)) res)
            (values res t)))))))

(defmethod dl-prover-retrieve-individual-attribute-fillers ((substrate dl-prover-substrate) ind attribute)
  (with-critical-section
    (multiple-value-bind (val foundp)
        (gethash (list ind attribute) (individual-attribute-fillers-cache substrate))
      (if foundp 
          (values val t)
        (unless *told-information-reasoning-p*
          (let ((fillers 
                 (dl-retrieve-individual-attribute-fillers substrate ind attribute (abox substrate))))
            (setf (gethash (list ind attribute) (individual-attribute-fillers-cache substrate)) fillers)
            (values fillers t)))))))

(defmethod dl-prover-all-role-assertions-for-individual-in-range ((substrate dl-prover-substrate) ind)
  (with-critical-section
    (multiple-value-bind (val foundp)
        (gethash ind (role-assertions-in-range-cache substrate))
      (if foundp 
          (values val t)
        (unless *told-information-reasoning-p*
          (let ((assertions
                 (dl-all-role-assertions-for-individual-in-range substrate ind (abox substrate))))
            (setf (gethash ind (role-assertions-in-range-cache substrate)) assertions)
            (values assertions t)))))))

(defmethod dl-prover-all-role-assertions-for-individual-in-domain ((substrate dl-prover-substrate) ind)
  (with-critical-section
    (multiple-value-bind (val foundp)
        (gethash ind (role-assertions-in-domain-cache substrate))
      (if foundp 
          (values val t)
        (unless *told-information-reasoning-p*
          (let ((assertions 
                 (dl-all-role-assertions-for-individual-in-domain substrate ind (abox substrate))))
            (setf (gethash ind (role-assertions-in-domain-cache substrate)) assertions)
            (values assertions t)))))))

(defmethod dl-prover-atomic-role-descendants ((substrate dl-prover-substrate) role)
  (with-critical-section 
    (multiple-value-bind (val foundp)
        (gethash role (role-descendants-cache substrate))
      (if foundp 
          (values val t)
        (unless *told-information-reasoning-p*
          (let ((roles
                 (dl-atomic-role-descendants substrate role (tbox substrate))))
            (setf (gethash role (role-descendants-cache substrate)) roles)
            (values roles t)))))))

(defmethod dl-prover-atomic-role-children ((substrate dl-prover-substrate) role)
  (with-critical-section 
    (multiple-value-bind (val foundp)
        (gethash role (role-children-cache substrate))
      (if foundp 
          (values val t)
        (unless *told-information-reasoning-p*
          (let ((roles
                 (dl-atomic-role-children substrate role (tbox substrate))))
            (setf (gethash role (role-children-cache substrate)) roles)
            (values roles t)))))))

(defmethod dl-prover-atomic-role-ancestors ((substrate dl-prover-substrate) role)
  (with-critical-section 
    (multiple-value-bind (val foundp)
        (gethash role (role-ancestors-cache substrate))
      (if foundp 
          (values val t)
        (unless *told-information-reasoning-p*
          (let ((roles
                 (dl-atomic-role-ancestors substrate role (tbox substrate))))
            (setf (gethash role (role-ancestors-cache substrate)) roles)
            (values roles t)))))))

(defmethod dl-prover-atomic-role-parents ((substrate dl-prover-substrate) role)
  (with-critical-section 
    (multiple-value-bind (val foundp)
        (gethash role (role-parents-cache substrate))
      (if foundp 
          (values val t)
        (unless *told-information-reasoning-p*
          (let ((roles
                 (dl-atomic-role-parents substrate role (tbox substrate))))
            (setf (gethash role (role-parents-cache substrate)) roles)
            (values roles t)))))))




(defmethod dl-prover-atomic-role-inverse ((substrate dl-prover-substrate) role)
  (with-critical-section 
    (multiple-value-bind (val foundp)
        (gethash role (atomic-role-inverse-cache substrate))
      (if foundp 
          (values val t)
        (unless *told-information-reasoning-p*
          (let ((inv-role
                 (dl-atomic-role-inverse substrate role (tbox substrate))))
            (setf (gethash role (atomic-role-inverse-cache substrate)) inv-role)
            (values inv-role t)))))))

(defmethod dl-prover-told-value ((substrate racer-substrate) cdo-or-abox-ind &key (cd-object-p t))
  (with-critical-section
    (with-slots (told-values-cache) substrate
      (labels ((reduce-expr (res visited) 
                 ;; (princ "res ") (princ res) (terprI)
                 (if (consp res)
                     (let ((res 
                            (mapcar #'(lambda (x) 
                                        (if (or (numberp x)
                                                (stringp x))
                                            x
                                          (or (do-it x (cons cdo-or-abox-ind visited))
                                              x)))
                                    res)))
                       ;;; (+ 35 30)? Ausdruck reduzieren (war z.B. (+ age-michael 30))  
                       (if (and (every #'numberp (cdr res))
                                (member (to-keyword (first res))
                                        '(:+ :- :*)))
                           (apply (symbol-function (first res))
                                  (rest res))
                         res))
                   res))
               
               (do-it (cdo-or-abox-ind visited)
                 (unless (member cdo-or-abox-ind visited)
                   (let ((res
                          (gethash cdo-or-abox-ind told-values-cache)))
                     ;; (princ "do-it ") (princ (list cdo-or-abox-ind res visited)) (terpri)
                 
                     (if cd-object-p

                         (reduce-expr res visited)

                       ;;; ( (age (+ 30 40)) ) aus (instance betty (= age (+ 30 40))), ... 
                       
                       (mapcar #'(lambda (res) 
                                   (list (first res)
                                         (reduce-expr (second res) visited)))
                               res))))))
                       
        (do-it cdo-or-abox-ind nil)))))

;;;
;;;
;;;

(defmethod dl-concept-p ((substrate racer-substrate) &rest args)
  (apply #'concept-p args))

#+:midelora
(defmethod dl-concept-p ((substrate midelora-substrate) &rest args)
  t)

(defmethod dl-all-atomic-concepts ((substrate racer-substrate) &rest args)
  (apply #'all-atomic-concepts args))

#+:midelora
(defmethod dl-all-atomic-concepts ((substrate midelora-substrate) &rest args)
  (apply #'prover::all-atomic-concepts args))


(defmethod dl-atomic-concept-children ((substrate racer-substrate) &rest args)
  (apply #'atomic-concept-children args))

#+:midelora
(defmethod dl-atomic-concept-children ((substrate midelora-substrate) &rest args)
  (apply #'prover::atomic-concept-children args))


;;;
;;; fuer ABox Augmentation (Rules etc.) 
;;;

(defmethod dl-add-concept-assertion ((substrate racer-substrate) &rest args)
  (apply #'add-concept-assertion args))

#+:midelora
(defmethod dl-add-concept-assertion ((substrate midelora-substrate) &rest args)
  (apply #'prover::add-concept-assertion args))


(defmethod dl-add-role-assertion ((substrate racer-substrate) &rest args)
  (apply #'add-role-assertion args))

#+:midelora
(defmethod dl-add-role-assertion ((substrate midelora-substrate) &rest args)
  (apply #'prover::add-role-assertion args))

(defmethod dl-add-same-as-assertion ((substrate racer-substrate) &rest args)
  (apply #'add-same-individual-as-assertion args))

(defmethod dl-add-different-from-assertion ((substrate racer-substrate) &rest args)
  (apply #'add-different-from-assertion args))

(defmethod dl-add-all-different-assertion ((substrate racer-substrate) &rest args)
  (apply #'add-all-different-assertion args))



#+:midelora
(defmethod dl-add-role-assertion ((substrate midelora-substrate) &rest args)
  (apply #'prover::add-role-assertion args))



(defmethod dl-add-attribute-assertion ((substrate racer-substrate) &rest args)
  (apply #'add-attribute-assertion args))

(defmethod dl-add-constraint-assertion ((substrate racer-substrate) &rest args)
  (apply #'add-constraint-assertion args))

;;;
;;;
;;;

(defmethod dl-forget-concept-assertion ((substrate racer-substrate) &rest args)
  (apply #'forget-concept-assertion args))

#+:midelora
(defmethod dl-forget-concept-assertion ((substrate midelora-substrate) &rest args)
  (apply #'prover::forget-concept-assertion args))


(defmethod dl-forget-role-assertion ((substrate racer-substrate) &rest args)
  (apply #'forget-role-assertion args))

#+:midelora
(defmethod dl-forget-role-assertion ((substrate midelora-substrate) &rest args)
  (apply #'prover::forget-role-assertion args))


(defmethod dl-forget-constrained-assertion ((substrate racer-substrate) &rest args)
  (apply #'forget-constrained-assertion args))

(defmethod dl-forget-constraint ((substrate racer-substrate) &rest args)
  (apply #'forget-constraint args))

;;;
;;; Hoeherwertige Funktionen
;;;

(defmethod dl-prover-forget-concept-assertion ((substrate dl-prover-substrate) &rest args)
  (apply #'dl-forget-concept-assertion substrate args)
  (substrate-needs-reset substrate))

(defmethod dl-prover-forget-role-assertion ((substrate dl-prover-substrate) &rest args)
  (apply #'dl-forget-role-assertion substrate args)
  (substrate-needs-reset substrate))

(defmethod dl-prover-forget-constrained-assertion ((substrate dl-prover-substrate) &rest args)
  (apply #'dl-forget-constrained-assertion substrate args)
  (substrate-needs-reset substrate))

(defmethod dl-prover-forget-constraint ((substrate dl-prover-substrate) &rest args)
  (apply #'dl-forget-constraint substrate args)
  (substrate-needs-reset substrate))


;;;
;;; 
;;;

(defmethod dl-prover-add-concept-assertion ((substrate dl-prover-substrate) ind concept &key (to-abox-p t))
  (with-critical-section
    (let ((assertion (list ind concept)))

      (when to-abox-p 
        (dl-add-concept-assertion substrate (abox substrate) ind concept)
        (substrate-needs-reset substrate)
        ;;; die naechste Query braucht ein reintialisiertes
        ;;; Substrate, klar... dennoch sinnvoll mit dem 
        ;;; veraenderten Substrate weiterzurechnen, s. 
        ;;; mode 6
        )

      (with-slots (concept-assertions-cache) substrate
        (setf (gethash (list ind concept) (individual-instance-cache substrate)) t)

        (setf (gethash ind concept-assertions-cache)
              (cons assertion (gethash ind concept-assertions-cache)))))))


(defmethod dl-prover-add-role-assertion ((substrate dl-prover-substrate) from to role &key (to-abox-p t))
  (with-critical-section 
    (let ((assertion (list (list from to) role)))

      (when to-abox-p 
        (dl-add-role-assertion substrate (abox substrate) from to role)
        (substrate-needs-reset substrate))

      (with-slots (role-assertions-in-domain-cache role-assertions-in-range-cache) substrate
        (setf (gethash from role-assertions-in-domain-cache)
              (cons assertion (gethash from role-assertions-in-domain-cache)))
        (setf (gethash to role-assertions-in-range-cache)
              (cons assertion (gethash to role-assertions-in-range-cache)))))))


#+:midelora
(defmethod dl-prover-add-attribute-assertion ((substrate midelora-substrate) ind object attribute  &key (to-abox-p t))
  (declare (ignorable ind object attribute to-abox-p))
  nil)

(defmethod dl-prover-add-attribute-assertion ((substrate racer-substrate) ind object attribute  &key (to-abox-p t))
  (with-critical-section 
   
    (when to-abox-p
      (dl-add-attribute-assertion substrate (abox substrate) ind object attribute)
      (substrate-needs-reset substrate))

    (with-slots (individual-attribute-fillers-cache) substrate
      (setf (gethash (list ind attribute) individual-attribute-fillers-cache)
            (cons object (gethash (list ind attribute) individual-attribute-fillers-cache))))))

#+:midelora
(defmethod dl-prover-add-constraint-assertion ((substrate midelora-substrate) constraint &key (to-abox-p t))
  (declare (ignorable constraint to-abox-p))
  nil)

(defmethod dl-prover-add-constraint-assertion ((substrate racer-substrate) constraint &key (to-abox-p t))
  (with-critical-section 
   
    (when to-abox-p 
      (dl-add-constraint-assertion substrate (abox substrate) constraint)
      (substrate-needs-reset substrate))
   
    (with-slots (constraints-cache)  substrate
      (push constraint constraints-cache))))


(defmethod dl-prover-add-same-as-assertion ((substrate racer-substrate) from to &key)
  (with-critical-section 
    (dl-add-same-as-assertion substrate (abox substrate) from to)
    (substrate-needs-reset substrate)))


#+:midelora
(defmethod dl-prover-add-same-as-assertion ((substrate midelora-substrate) from to &key)
  (to-be-implemented 'dl-prover-add-same-as-assertion))



(defmethod dl-prover-add-different-from-assertion ((substrate racer-substrate) from to &key)
  (with-critical-section 
    (dl-add-different-from-assertion substrate (abox substrate) from to)
    (substrate-needs-reset substrate)))


#+:midelora
(defmethod dl-prover-add-different-from-assertion ((substrate midelora-substrate) from to &key)
  (to-be-implemented 'dl-prover-add-different-from-assertion))



(defmethod dl-prover-add-all-different-assertion ((substrate racer-substrate) args &key)
  (with-critical-section 
    (dl-add-all-different-assertion substrate (abox substrate) args)
    (substrate-needs-reset substrate)))

#+:midelora
(defmethod dl-prover-add-all-different-assertion ((substrate midelora-substrate) args &key)
  (to-be-implemented 'dl-prover-add-all-different-assertion))

;;;
;;;
;;;

#-:dlmaps
(defmethod find-node ((substrate racer-substrate) ind &key &allow-other-keys)
  ind) 

#-:dlmaps
(defmethod get-associated-substrate-node ((substrate racer-substrate) abox-ind)
  (find-node substrate abox-ind :error-p nil))

;;;
;;;
;;;


(defun set-tbox (substrate
                 &key load-from-file error-p new-tbox-p tbox abox new-abox-p &allow-other-keys)
  (let ((tbox-name 
         (or (convert-to-racer-tbox-name tbox (racer-package substrate))
             (when (and abox new-abox-p) 
               +racer-substrate-default-tbox+))))
    (with-slots (tbox) substrate
      (setf tbox
            (if load-from-file 
                (current-tbox)
              (when tbox-name
                (if new-tbox-p
                    (progn 
                      (when (find-tbox tbox-name nil)
                        (forget-tbox tbox-name))
                      (init-tbox tbox-name)
                      tbox-name)
                  (or (if (find-tbox tbox-name nil)
                          tbox-name
                        nil)
                      (when error-p 
                        (nrql-error "Can't find TBox ~A" tbox-name))
                      tbox-name))))))))

(defun set-abox (substrate
                 &key load-from-file error-p new-abox-p abox &allow-other-keys)
  (let ((abox-name 
         (convert-to-racer-abox-name abox (racer-package substrate))))
    (with-slots (abox tbox) substrate
      (setf abox
            (if load-from-file 
                (current-abox)
              (when abox-name              
                (if new-abox-p
                    (progn 
                      (when (find-abox abox-name nil)
                        (forget-abox abox-name))
                      (init-abox abox-name tbox)
                      abox-name)
                  (or (if (find-abox abox-name nil)
                          abox-name
                        nil)
                      (when error-p 
                        (nrql-error "Can't find ABox ~A" abox-name))
                      abox-name))))))))

;;;
;;;
;;;

(defmethod initialize-instance :after ((substrate racer-substrate) &rest args
                                       &key load-from-file mirror-abox-p &allow-other-keys)
  (declare (ignorable mirror-abox-p))

  (with-critical-section
  
    (when load-from-file 
      (let ((*package* (racer-package substrate)))
        (load load-from-file)))

    (apply #'set-tbox substrate args)
    (apply #'set-abox substrate args)

    (setf (slot-value substrate 'dbox)
          (or (find-dbox (tbox substrate) :error-p nil)
              (create-dbox (tbox substrate))))))

#+:midelora
(defmethod initialize-instance :after ((substrate midelora-substrate) &rest args)

  (setf (slot-value substrate 'abox) substrate
        (slot-value substrate 'tbox) (prover::tbox substrate))

  (setf (slot-value substrate 'dbox)
        (or (find-dbox (tbox substrate) :error-p nil)
            (create-dbox (tbox substrate)))))

;;;
;;;
;;;

(defmethod delete-substrate :after ((substrate dl-prover-substrate) &key &allow-other-keys)
  (when (dbox substrate)
    (setf *all-dboxes* 
          (delete (dbox substrate) *all-dboxes*))))

;;;
;;;
;;;

(defmethod is-datatype-property-some-value-p ((substrate racer-substrate) concept)
  (and (consp concept)
       (member (first concept) '(some d-some))
       (let ((qual (third concept)))
         (and (consp qual)
              (third qual) ; Wert auch wirklich vorhanden? 
              (member (second qual)
                      +racer-internal-datatype-property-roles+)))))

#+:midelora
(defmethod is-datatype-property-some-value-p ((substrate midelora-substrate) concept)
  (declare (ignore concept))
  nil)

;;;
;;;
;;;

(defmethod is-datatype-property-at-least-value-p ((substrate racer-substrate) concept)
  (and (consp concept)
       (member (first concept) '(at-least d-at-least))
       (let ((qual (fourth concept)))
         (and (consp qual) 
              (third qual) ; Wert auch wirklich vorhanden? 
              (member (second qual)
                      +racer-internal-datatype-property-roles+)))))

#+:midelora
(defmethod is-datatype-property-at-least-value-p ((substrate midelora-substrate) concept)
  (declare (ignore concept))
  nil)

;;;
;;; sinc RacerPro 1.9.3 / OWL 2.0 
;;;

(defmethod is-datatype-d-filler-value-p ((substrate racer-substrate) concept)
  (and (consp concept)
       (eq (first concept) 'd-filler)))

#+:midelora
(defmethod is-datatype-d-filler-value-p ((substrate midelora-substrate) concept)
  (declare (ignore concept))
  nil)

;;;
;;;
;;;

(defmethod is-datatype-property-in-and-value-p ((substrate racer-substrate) concept)
  (and (consp concept)
       (some #'(lambda (x) 
                 (or (is-datatype-property-some-value-p substrate x)
                     (is-datatype-property-at-least-value-p substrate x)
                     (is-datatype-d-filler-value-p substrate x)
                     (is-datatype-property-in-and-value-p substrate x)))
             (cdr concept))))

;;;
;;;
;;;

(defmethod compute-abox-mirror ((substrate dl-prover-substrate) &key &allow-other-keys)
  (with-critical-section

    (let ((ancestors (mht :test #'equal :size 1000)))

      (with-slots (abox tbox
                        told-values-cache
                        datatype-property-values-and-types-cache
                       
                        individual-attribute-fillers-cache
                        role-assertions-in-domain-cache
                        role-assertions-in-range-cache
                        concept-assertions-cache
                        individual-same-as-cache
                        same-as-assertions-present-p
                        constraint-entailed-cache) substrate
      
        (labels ((register-is-instance-of (ind concept)
                  
                   (if (gethash concept (known-concept-instances-cache substrate))
                       (push ind (gethash concept (known-concept-instances-cache substrate)))
                     (setf (gethash concept (known-concept-instances-cache substrate))
                           (list ind)))

                   (setf (gethash (list ind concept) (individual-instance-cache substrate)) t))
                
                 (propagate (cd-object val &optional found) 
                   (let ((neighbours (gethash cd-object told-values-cache))
                         (found (cons cd-object found)))

                     (setf (gethash cd-object told-values-cache) val)

                     (when (consp neighbours)
                       (dolist (neighbour neighbours)
                         (unless (member neighbour found)
                           (propagate neighbour val found))))))
                
                 (register-told-value-for (a b)

                   (when (symbolp a)
                     (multiple-value-bind (found present-p)
                         (gethash a told-values-cache) 
                       (declare (ignorable found))
                       (unless present-p 
                         (setf (gethash a told-values-cache) nil))))

                   (when (symbolp b)
                     (multiple-value-bind (found present-p)
                         (gethash b told-values-cache)
                       (declare (ignorable found))
                       (unless present-p
                         (setf (gethash b told-values-cache) nil))))

                   (if (consp b) 
                       ;;; (= age-of-b (+ 30 age-of-a))

                       (propagate a b)

                     (progn


                       (if (symbolp a) 
                           (if (symbolp b)

                               ;;; a = b

                               (progn 
                                 (if (listp (gethash b told-values-cache)) ; b hat einen Wert! 
                                     (pushnew a (gethash b told-values-cache) :test #'equal)
                                   (propagate a (gethash b told-values-cache)))
                                 (if (listp (gethash a told-values-cache))
                                     (pushnew b (gethash a told-values-cache) :test #'equal)
                                   (propagate b (gethash a told-values-cache))))
                          
                             ;;; a = 3

                             (propagate a b))
                     
                         (if (symbolp b)
                           
                             ;;; 3 = b 
                          
                             (propagate b a)

                           ;;; 3 = 4 

                           ;;; gibt ne Inkonsistenz, nicht beruecksichtigen
                           )))))

                 (register-datatype-property-value-and-type (property ind property-value concept annotation-p)
                   (if (gethash ind datatype-property-values-and-types-cache)
                       (pushnew (list property-value annotation-p) (gethash ind datatype-property-values-and-types-cache)
                                :test #'equal)
                     (setf (gethash ind datatype-property-values-and-types-cache)
                           (list (list property-value annotation-p))))
                  
                   ;;; also add to ABox!
                  
                   (when *add-role-assertions-for-datatype-properties-p*

                     (let ((datatype-succs
                            (mapcar #'cadar
                                    (remove-if-not #'(lambda (x) 
                                                       (eq (second x) property))
                                                   (dl-prover-all-role-assertions-for-individual-in-domain 
                                                    substrate ind)))))

                       ;; is there a datatype successor which has the required concept assertion?
                       ;; -> dont need to add another one!

                       (unless (some #'(lambda (succ)
                                         (gethash (list succ concept)
                                                  (individual-instance-cache substrate)))
                                     datatype-succs)

                         (let ((new-ind
                                (intern (format nil "~A-~A"
                                                +secret-prefix+
                                                (incf *sym-count*)))))
                                 
                           (dl-prover-add-role-assertion substrate 
                                                         ind
                                                         new-ind
                                                         property)
                                 
                           (dl-prover-add-concept-assertion substrate
                                                            new-ind 
                                                            concept))))))



                 (get-conjuncts-of-concept (concept)
                   (if (symbolp concept)
                       (list concept)
                     (case (first concept)
                       (and (apply #'append 
                                   (mapcar #'get-conjuncts-of-concept (rest concept))))
                       (t (list concept)))))

                 (get-implied-concepts-for-cache (concept annotation)
                   (let ((concepts
                          (or (gethash concept ancestors) 
                              (setf (gethash concept ancestors)
                                    (remove-duplicates
                                     (cons concept
                                           (cons 'top
                                                 (cons '*top*
                                                       (if (consp concept)
                                                           (append
                                                    
                                                            (when (and *ensure-tbox-classification-p*
                                                                       (dl-tbox-classified-p substrate tbox)
                                                                       *classify-concepts-in-instance-assertions-p*)
                                                              (append 
                                                               (reduce #'append (dl-atomic-concept-ancestors substrate concept tbox))
                                                               (reduce #'append (mapcar #'ensure-list 
                                                                                        (dl-atomic-concept-synonyms substrate concept tbox)))))

                                                            (case (first concept)

                                                              ;;; nur fuer positive Konzepte:

                                                              (and (reduce #'append
                                                                           (mapcar #'(lambda (x) 
                                                                                       (get-implied-concepts-for-cache x annotation))
                                                                                   (rest concept))))
                                                      
                                                              ((some d-some)
                                                               (when (is-datatype-property-some-value-p substrate concept)
                                                                 (let ((role (second concept))
                                                                       (qual (third concept)))
                                                                   (mapcar #'(lambda (qc)
                                                                               `(d-some ,role ,qc))
                                                                           (get-implied-concepts-for-cache qual annotation)))))
                                                      
                                                              ((at-least d-at-least)
                                                               (when (is-datatype-property-at-least-value-p substrate concept)
                                                                 (let ((role (third concept))
                                                                       (qual (fourth concept))
                                                                       (n (second concept)))
                                                                   (mapcar #'(lambda (qc)
                                                                               `(d-at-least ,n ,role ,qc))
                                                                           (get-implied-concepts-for-cache qual annotation)))))

                                                              ((d-filler)
                                                               (apply #'append
                                                                      (mapcar #'(lambda (role) 
                                                                                  (let ((concept1 
                                                                                         `(d-some
                                                                                           ,role 
                                                                                           ,(transform-literal (third concept)))))
                                                                                    (cons concept1
                                                                                          (get-implied-concepts-for-cache concept1 annotation))))
                                                                              (if annotation
                                                                                  (dl-atomic-role-ancestors substrate (second concept) tbox)
                                                                                (list (second concept))))))
                                                      
                                                              ((min max divisible not-divisible string= string<>
                                                                    > >= < <= <> = equal unequal
                                                                    a an)
                                                               (let ((attrib (second concept)))
                                                                 `((an ,attrib)
                                                                   (a ,attrib))))))

                                                         (when *ensure-tbox-classification-p*
                                                           (unless (dl-tbox-classified-p substrate tbox)
                                                             (dl-classify-tbox substrate tbox))
                                                           (append 
                                                            (reduce #'append (dl-atomic-concept-ancestors substrate concept tbox))
                                                            (reduce #'append
                                                                    (mapcar #'ensure-list 
                                                                            (dl-atomic-concept-synonyms substrate concept tbox)))))))))
                                     :test #'equal)))))
                     
                     ;; (write "implied: ") 
                     ;; (write concepts) 

                     concepts))

                 ;; new: register datatype property values, also works for conjunctions now 

                 (process-datatype-property-assertion (ind concept annotation-p)

                   (cond ((and (consp concept) 
                               (eq (first concept) 'and))

                          (dolist (x (rest concept))
                            (process-datatype-property-assertion ind x annotation-p)))

                         ((is-datatype-property-some-value-p substrate concept)

                          (let* ((property (second concept))
                                 (internal-attribute (second (third concept)))
                                 (property-value (list property
                                                       (third (third concept)))))
                                 
                            (declare (ignorable internal-attribute))
                                 
                            ;;; e.g. 
                            ;;; (SOME |http://www.owl-ontologies.com/unnamed.owl#age| 
                            ;;;       (EQUAL RACER-INTERNAL%HAS-INTEGER-VALUE 35))
                                 
                            (register-datatype-property-value-and-type property ind property-value 
                                                                       (third concept)
                                                                       annotation-p)))
                            
                         ;;;
                         ;;; 
                         ;;; 

                         ((is-datatype-property-at-least-value-p substrate concept)
                               
                          (let* ((property (third concept))
                                 (internal-attribute (second (fourth concept)))
                                 (property-value (list property
                                                       (third (fourth concept)))))

                            (declare (ignorable internal-attribute))
                                 
                            ;;; e.g. 
                            ;;; (AT-LEAST 3 |http://www.owl-ontologies.com/unnamed.owl#id| 
                            ;;;              (EQUAL RACER-INTERNAL%HAS-INTEGER-VALUE 35))
                                 
                            (register-datatype-property-value-and-type property ind property-value
                                                                       (fourth concept)
                                                                       annotation-p)))
                          
                         ;;; 
                         ;;; Encoding in new OWL Interface: 
                         ;;; 

                         ((is-datatype-d-filler-value-p substrate concept)

                          ;; e.g. 
                          ;; (|http://www.owl-ontologies.com/unnamed.owl#michael|
                          ;; (D-FILLER
                          ;;     |http://www.owl-ontologies.com/unnamed.owl#age|
                          ;;  (D-LITERAL "35" (D-BASE-TYPE RACER::|http://www.w3.org/2001/XMLSchema#int|))))

                          (let* ((property (second concept))
                                 (value (third concept))
                                 (concept (transform-literal value))
                                 ;;; (= racer-internal%has-integer-value 123) 
                                 ;;; OR RACER::*DATATYPE-TOP* 
                                 )

                            (cond ((consp concept) 
                                   (let ((property-value
                                          (list property (third concept) value)))
                             
                                     (register-datatype-property-value-and-type property ind property-value 
                                                                                concept
                                                                                annotation-p)

                                     ;;; auch alte Repraesentation codieren: 
                                     ;;; nein, doch nicht, fuerht zur "Verdoppelung" beim naechsten prepare...

                                     '(register-datatype-property-value property ind property-value 
                                                                        `(some ,property ,literal)
                                                                        annotation-p)))

                                  (t
                                   (nrql-warning "Cannot register ~A filler ~S  (unsupported XML schema type?)" property value))))))))
          
          ;;;
          ;;; Start Mirroring
          ;;; 
                 
          (let ((*told-information-reasoning-p* nil)
                (datatype-assertions nil))

            ;; (princ 1)

            ;;; 1a) Told Values durch "implizite" (equal age 30) etc. CD-Konzepte in Assertionen

            (dolist (concept-assertion (dl-all-concept-assertions substrate abox))
              (let ((told-values nil))
                (dolist (cd-concept (get-conjuncts-of-concept (second concept-assertion)))
                  (when (and (consp cd-concept)
                             (member (to-keyword (first cd-concept))
                                     '(:= :equal :string= :boolean=)))
                    (pushnew (list (second cd-concept) ; Attribut 
                                   (third cd-concept)) ; Literal
                             told-values :test #'equal)))
		
                (when told-values
                  (let ((present (gethash (first concept-assertion) told-values-cache)))
                    (if present
                        (setf (gethash (first concept-assertion) told-values-cache)
                              (append present told-values))
                      (setf (gethash (first concept-assertion) told-values-cache)
                            told-values))))))	   
	    
            ;;; 1b) Ein Told Value wird fuer ein CD-Objekt registriert 

            (dolist (c (dl-prover-all-constraints substrate))
              (setf (gethash c constraint-entailed-cache) t)
              (when (and (member (to-keyword (first c))
                                 '(:= :equal :string= :boolean=))
                         ; (not (some #'consp (rest c))) 
                         )
                (apply #'register-told-value-for (rest c))))

            ;; (princ 2)
           
            ;;; Attribut-Fueller (CD-Objekte) registrieren
            ;;; zusaetzlich 1a) und 1b) kombinieren! 
           
            (dolist (aa (dl-all-attribute-assertions substrate abox))
              (let* ((aa (rest aa))
                     (from (first aa))                     
                     (to (second aa))
                     (attribute (third aa))
                     (entry (list from attribute)))
              
                (if (gethash entry individual-attribute-fillers-cache)
                    (pushnew to (gethash entry individual-attribute-fillers-cache))
                  (setf (gethash entry individual-attribute-fillers-cache)
                        (list to)))

                (let ((concept-told-values
                       (gethash from told-values-cache)))
                  (dolist (av concept-told-values)
                    (when (equal (first av) attribute)
                      (register-told-value-for to ; cd-object 
                                               (second av))))))) ; told-value

            ;; (princ 3)
           
            ;;; Rollenhierarchie etc. cachen
           
            (dolist (role (dl-prover-all-roles substrate))

              (when
                  #+:racer-server (role-p role tbox)
                #-:racer-server t
                ;; all-roles liefert mist!
                (dl-prover-atomic-role-descendants substrate role)
                (dl-prover-atomic-role-children substrate role)                
                (dl-prover-atomic-role-ancestors substrate role)               
                (dl-prover-atomic-role-parents substrate role)               
                (dl-prover-atomic-role-inverse substrate role)))

            ;; (princ 4)

            ;;; Individuen cachen 
	   
            (dolist (ind (dl-prover-all-individuals substrate))
             
              (setf (gethash (list ind +individual-exists-concept+) 
                             (individual-instance-cache substrate)) t)

              (register-is-instance-of ind 'top)
              (register-is-instance-of ind '*top*))

            ;;; (princ "4a")

            ;;; Syntaktische Individuen-Synonme cachen (notwendig fuer unvollstaendige Modi, 
            ;;; retrieve-individual-synonyms fuehrt einen ABox-Konsistenztest durch!) 

            (dolist (assertion (dl-prover-all-same-as-assertions substrate))

              (setf same-as-assertions-present-p t)

              ;;; (same-as i j)

              (let* ((first (second assertion))
                     (first-synonyms (gethash first individual-same-as-cache))
                     (second (third assertion))
                     (second-synonyms (gethash second individual-same-as-cache)))

                (unless (eq first second)

                  ;; (format t "~A ~A ~A ~%" assertion first-synonyms second-synonyms)

                  (cond ((not (or first-synonyms second-synonyms))

                         (let ((list (list first second)))
                                       
                           (setf (gethash first individual-same-as-cache) list)
                           (setf (gethash second individual-same-as-cache) list)))

                        ((eq first-synonyms second-synonyms)
                       
                         (let ((ref (last first-synonyms)))
                           (cond ((member first first-synonyms)

                                  (cond ((member second first-synonyms)

                                         t)

                                        (t
                                         (setf (cdr ref)
                                               (list second)))))

                                 (t 
                                  
                                  (cond ((member second first-synonyms)

                                         (setf (cdr ref) (list first)))

                                        (t
                                         (setf (cdr ref)
                                               (list first second))))))))

                        (t
                         (let ((cluster (remove-duplicates 
                                         (cons first 
                                               (cons second
                                                     (append first-synonyms second-synonyms))))))

                           (dolist (ind cluster)
                             (setf (gethash ind individual-same-as-cache) cluster))))))))
	    
            (dolist (assertion (dl-prover-all-same-as-assertions substrate))

              ;;; (same-as i j)	     	      

              (let* ((first (second assertion))
                     (second (third assertion))
                     (first-synonyms (gethash first individual-same-as-cache))
                     (second-synonyms (gethash second individual-same-as-cache))
		     
                     (cluster
                      (sort 
                       (remove-duplicates 
                        (cons first
                              (cons second
                                    (append first-synonyms second-synonyms))))
                       #'string-lessp
                       :key #'symbol-name)))
		
                (setf (gethash first individual-same-as-cache) cluster
                      (gethash second individual-same-as-cache) cluster)))
	    
            ;; (princ 5)
           
            ;;; Rollenassertionen cachen (Index) 
            ;;; Auch die OWL Annotation Object Properties (all-annotation-role-assertions) 

            (when (or *initial-abox-mirroring-p*
                      *initial-role-assertion-mirroring-p*)

              (dolist (role-assertions (list (dl-all-annotation-role-assertions substrate abox)
                                             (dl-all-role-assertions substrate abox)))
                
                (dolist (ra role-assertions)
                  (let ((from (first (first ra)))
                        (to (second (first ra))))
                    
                    (if (gethash to role-assertions-in-range-cache)
                        (push ra (gethash to role-assertions-in-range-cache))
                      (setf (gethash to role-assertions-in-range-cache)
                            (list ra)))
                    
                    (if (gethash from role-assertions-in-domain-cache)
                        (push ra (gethash from role-assertions-in-domain-cache))
                      (setf (gethash from role-assertions-in-domain-cache)
                            (list ra)))))))

            ;; (princ 6)

            ;;; Konzeptassertionen (unvollstaendige Modi 0,1,2) 
            ;;; Auch die OWL Annotation Concept Assertions werden hier durchlaufen: Diese kodieren 
            ;;; alle Datatype Properties, die Annotations sind (also sowohl
            ;;; Individuums, Klassen, und Ontologie-Annotationen, z.B. RDFS Comment, 
            ;;; aber auch selbstdefinierte Datatype Annotation Properties) 
            ;;; Diese Assertionen werden auf datatype-assertions gepushed und unter 7 weiterverarbeitet
            ;;; Hier werden also nur die einfachen Konzeptassertionen bearbeitet

            ;;; Die Concept Assertions kodieren jedoch auch Datatype Property Fueller 

            (let ((annotation t))

              (dolist (concept-assertions (list (dl-all-annotation-concept-assertions substrate abox)
                                                (dl-all-concept-assertions substrate abox)))
                                        
                (dolist (ca concept-assertions)

                  (let* ((ind (first ca))
                         (concept (second ca))
                         (dtp-p
                          (or (is-datatype-property-some-value-p substrate concept)
                              (is-datatype-property-at-least-value-p substrate concept)
                              (is-datatype-d-filler-value-p substrate concept)
                              (is-datatype-property-in-and-value-p substrate concept))))

                    (when dtp-p 
                      (push (list ca annotation) datatype-assertions))
                   
                    (when (gethash (list ind +individual-exists-concept+) 
                                   (individual-instance-cache substrate))

                      ;;; hier sollen nur die ABox-Individuen bearbeitet werden;
                      ;;; Racer gibt unter all-annotation-concept-assertions auch
                      ;;; Klassen-Annotation etc. zurueck! 

                      (if (gethash ind concept-assertions-cache)
                          (push ca (gethash ind concept-assertions-cache))
                        (setf (gethash ind concept-assertions-cache) (list ca)))
                    
                      (if (or t 
			      ;; *initial-abox-mirroring-p* ; unvollstaendige nRQL-Modi vorbereiten 
			      ;; dtp-p)	; muessen eingetragen werden, damit die Caches auf "(a <dtp>)" etc. reagieren!!! 
			      )
			      
                          (progn

                            (let ((concepts (get-implied-concepts-for-cache concept annotation)))

                              (dolist (concept concepts) ; orig. concept ist in (get-implied-...) enthalten! 
                                (register-is-instance-of ind concept))))

                        (register-is-instance-of ind concept)))))
             
                (setf annotation nil)))
           
            ;; (princ 7)

            ;;; OWL Datatype Properties und Annotation Datatype Properties 
            ;;; Werden hier als TOLD VALUES im Cache eingetragen 
            
            (dolist (da datatype-assertions)

              (let ((da (first da))
                    (annotation-p (second da)))

                (let ((ind (first da))
                      (concept (second da)))

                  (when (gethash (list ind +individual-exists-concept+) 
                                 (individual-instance-cache substrate))

                    ;;;
                    ;;; Datatype-Property Encoding of Racer
                    ;;; 

                    (process-datatype-property-assertion ind concept annotation-p)))))))))))


(defmethod initialize-instance :after ((substrate tbox-mirror-substrate) &rest args
                                       &key &allow-other-keys)
  (declare (ignorable args))

  (setf (slot-value substrate 'mirror-of-tbox)
        (tbox substrate))
             
  ;; (setf (saved-state-vector substrate) 
  ;;      (get-state-vector substrate))

  )


(defmethod compute-tbox-mirror ((substrate tbox-mirror-substrate))
  (with-critical-section      
    (with-slots (abox tbox 
                      role-descendants-cache 
                      role-ancestors-cache
                      role-children-cache 
                      role-parents-cache
                      atomic-role-inverse-cache 
                      concept-instances-cache
                      ;; constraints-cache
                      abox-individuals-cache 
                      concept-assertions-cache
                      role-assertions-in-range-cache 
                      role-assertions-in-domain-cache                       
                      individual-instance-cache) substrate
    
      (let* ((package
              #-:midelora (racer-package substrate)
              #+:midelora (find-package :prover))
	     
             (has-synonym-role (change-package-of-description 'has-synonym 
                                                              package))
             
             (has-child-role (change-package-of-description 'has-child 
                                                            package))

             (has-parent-role (change-package-of-description 'has-parent 
                                                             package))

             (has-descendant-role (change-package-of-description 'has-descendant 
                                                                 package))

             (has-ancestor-role (change-package-of-description 'has-ancestor 
                                                               package))
	     
             (taxonomy-node (change-package-of-description 'taxonomy-node
                                                           package)))
    
        (let ((taxonomy (dl-taxonomy substrate))
              (all-concepts nil))

          (setf (gethash taxonomy-node concept-instances-cache)
                (mapcar #'first (mapcar #'ensure-list (mapcar #'first taxonomy))))

          (dolist (triple taxonomy)
            (let* ((synonyms (ensure-list (first triple)))
                   (parents (second triple))
                   (children (third triple))
                   (representative (first synonyms))
                   (synonyms
                    (cond ((member 'top synonyms)
                           (cons '*top* synonyms))
                          ((member 'bottom synonyms)
                           (cons '*bottom* synonyms))
                          (t synonyms)))
                   (children
                    (cond ((member 'top children)
                           (cons '*top* children))
                          ((member 'bottom children)
                           (cons '*bottom* children))
                          (t children))))
	      	     
              (declare (ignorable parents))
	      
              (setf (gethash (list representative taxonomy-node)
                             individual-instance-cache) t)
	      
              (setf (gethash representative concept-assertions-cache) (list taxonomy-node))
              (setf (gethash (list representative taxonomy-node) individual-instance-cache) t)
              
              (dolist (name synonyms)
		
                (push name all-concepts)

                (setf (gethash (list name +individual-exists-concept+) individual-instance-cache) t)
                (setf (gethash name concept-assertions-cache) 
                      (remove-duplicates (append (gethash name concept-assertions-cache) synonyms)))
		
                (setf (gethash name concept-instances-cache)
                      synonyms)
		  
                (dolist (name2 synonyms)
                  (setf (gethash (list name name2) individual-instance-cache) t))
              
                ;;;
                ;;;
                ;;; 

                (let ((from-assertions nil))
                  (dolist (child children)
                    (let ((child-syns (ensure-list child)))
                      (dolist (child child-syns)
                        (push (list (list name child) has-child-role) from-assertions))))
                 
                  (dolist (syn (remove name synonyms))
                    (push (list (list name syn) has-synonym-role) from-assertions))
                                           
                  (setf (gethash name role-assertions-in-domain-cache) from-assertions)

                  (dolist (assertion from-assertions)
                    (let ((child (second (first assertion))))
                      (if (gethash child role-assertions-in-range-cache)
                          (push assertion (gethash child role-assertions-in-range-cache))                  
                        (setf (gethash child role-assertions-in-range-cache) (list assertion)))))))))

          ;; (set-tbox substrate :tbox 'default :new-tbox-p nil)
          ;; (set-abox substrate :abox 'default :new-abox-p nil)
	  
          (setf tbox nil
                abox nil)

          (setf abox-individuals-cache all-concepts)

          (setf (gethash has-parent-role role-descendants-cache) (list has-parent-role))
          (setf (gethash has-child-role role-descendants-cache) (list has-child-role))

          (setf (gethash has-parent-role role-ancestors-cache) (list has-parent-role has-ancestor-role))
          (setf (gethash has-child-role role-ancestors-cache) (list has-child-role has-descendant-role))

          (setf (gethash has-ancestor-role role-children-cache) (list has-parent-role))
          (setf (gethash has-parent-role role-parents-cache)  (list has-ancestor-role))
          
          (setf (gethash has-descendant-role role-children-cache) (list has-child-role))
          (setf (gethash has-child-role role-parents-cache) (list has-descendant-role))
	 	 
          (setf (gethash has-descendant-role role-descendants-cache)
                (list has-descendant-role has-child-role))
	  
          (setf (gethash has-ancestor-role role-descendants-cache)
                (list has-ancestor-role has-parent-role))
	 
          (setf (gethash has-descendant-role role-ancestors-cache)
                (list has-descendant-role))
	  
          (setf (gethash has-ancestor-role role-ancestors-cache)
                (list has-ancestor-role))
	 	 
          (setf (transitive-roles-cache substrate) 
                (list has-descendant-role has-ancestor-role has-synonym-role))

          (setf (gethash has-synonym-role role-descendants-cache) (list has-synonym-role))
          (setf (gethash has-synonym-role role-ancestors-cache)   (list has-synonym-role))

          
          (dolist (pair (list (list has-parent-role has-child-role)
                              (list has-descendant-role has-ancestor-role)
                              (list has-synonym-role has-synonym-role)))
	  
            (setf (gethash (first pair) atomic-role-inverse-cache)
                  (second pair))
            (setf (gethash (second pair) atomic-role-inverse-cache)
                  (first pair))))))))

;;;
;;;
;;;
;;;

#-:midelora
(defmethod convert-to-dl-prover-individual-name ((substrate racer-substrate) ind)
  (convert-to-racer-individual-name ind (racer-package substrate)))

#+:midelora
(defmethod convert-to-dl-prover-individual-name ((substrate midelora-substrate) ind)
  (change-package-of-description ind (find-package :prover)))

(defmethod convert-to-dl-prover-attribute-expression ((substrate racer-substrate) attrib)
  (convert-to-racer-attribute-expression attrib (racer-package substrate)))

(defmethod convert-to-dl-prover-role-expression ((substrate racer-substrate) role)
  (convert-to-racer-role-expression role (racer-package substrate)))
