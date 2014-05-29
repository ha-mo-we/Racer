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
;;; Marker fuer die ABox 
;;;

(defconstant +marker-for-abox-thing+ :abox-object)

(defconstant +marker-for-abox-relationship+ :abox-relationship)

;;;
;;; Unterklassen von :abox-object 
;;;

(defconstant +marker-for-abox-individual+ :abox-individual)

(defconstant +marker-for-abox-cd-object+ :abox-concrete-domain-object)

(defconstant +marker-for-abox-cd-value+ :abox-concrete-domain-value)

;;;
;;; Unterklassen von :abox-relationship 
;;; 

(defconstant +marker-for-abox-same-as-relationship+ :abox-same-as-relationship)

(defconstant +marker-for-abox-different-from-relationship+ :abox-different-from-relationship)

(defconstant +marker-for-abox-role-relationship+ :abox-role-relationship)

(defconstant +marker-for-abox-attribute-relationship+ :abox-attribute-relationship)

(defconstant +marker-for-abox-told-value-relationship+ :abox-told-value-relationship)

;;;
;;; Marker fuer OWL 
;;;

(defconstant +marker-for-owl-thing+ :owl-object)

(defconstant +marker-for-owl-relationship+ :owl-relationship)

;;;
;;; Unterklassen von :owl-object
;;;

(defconstant +marker-for-owl-individual+ :owl-individual)

(defconstant +marker-for-owl-class+ :owl-class)

(defconstant +marker-for-owl-literal+ :owl-literal)

;;;
;;; Unterklassen von :owl-relationship
;;;

(defconstant +marker-for-owl-object-property-relationship+ :owl-object-property-relationship)

(defconstant +marker-for-owl-datatype-property-relationship+ :owl-datatype-property-relationship) 

;;;
;;; Unterklassen von :owl-object-property-relationship
;;;

(defconstant +marker-for-owl-object-annotation-property-relationship+ :owl-object-annotation-property-relationship)

(defconstant +marker-for-owl-datatype-annotation-property-relationship+ :owl-datatype-annotation-property-relationship)

;;;
;;; fehlt: Properties fuer Klassen (child, parent, descendent, ancestor) und type! 
;;;

;;;
;;;
;;;

(defpersistentclass mirror-data-substrate (data-substrate)
  ())

#+:sql-substrate
(defpersistentclass mirror-sql-data-substrate (sql-data-substrate mirror-data-substrate)
  ())

;;;
;;;
;;;

#+:sql-substrate
(defmethod compute-abox-mirror :before ((substrate mirror-sql-data-substrate) &key &allow-other-keys)
  (initialize-db substrate))

(defmethod compute-abox-mirror :after ((substrate mirror-data-substrate) &key &allow-other-keys)
  (with-critical-section
 
    (with-slots (abox tbox
                      abox-individuals-cache
                      told-values-cache
                      datatype-property-values-and-types-cache
                      role-assertions-in-domain-cache) substrate

      (let ((*told-information-reasoning-p* t)
            (owl-p (racer:get-namespace-prefix (associated-tbox abox)))
              
            (annotation-object-properties (mht2 :test #'equal :size 100))
            
            (atoms (all-atomic-concepts tbox))
            (atoms-hash (mht2 :size 1000))
            (ancestors (mht2 :test #'equal :size 100)))

        ;;; ABox-Individuen -> Knoten  
        
        (dolist (atom atoms)
          (setf (gethash atom atoms-hash) t))
        
        (dolist (ind abox-individuals-cache)
          
          (node-instance substrate ind :description 
                         `(,+marker-for-abox-thing+
                           ,+marker-for-abox-individual+

                           ,@(when (and owl-p 
                                        (is-url-p ind))
                               (list +marker-for-owl-thing+
                                     +marker-for-owl-individual+))))
            
          (dolist (concepts atoms)
            (dolist (concept (ensure-list concepts))
              ;;; Known instance? *told-information-reasoning-p* = T ! 
              (when (dl-prover-individual-instance-p substrate ind concept)
                (node-instance substrate ind  :description concept)))))

        ;;; Attribute Assertions (kommen in OWL-Dateien nicht vor)
       
        (dolist (attribute-assertion (all-attribute-assertions abox))
          (let ((abox-ind (second attribute-assertion))
                (cd-object (third attribute-assertion))
                (cd-attribute (fourth attribute-assertion)))

            (nodes-related substrate abox-ind cd-object
                           `(,+marker-for-abox-relationship+
                             ,+marker-for-abox-attribute-relationship+
                             ,cd-attribute))

            (node-instance substrate abox-ind
                           :description  `(,+marker-for-abox-thing+ 
                                           ,+marker-for-abox-individual+))

            (node-instance substrate cd-object 
                           :description  `(,+marker-for-abox-thing+
                                           ,+marker-for-abox-cd-object+))))

        ;;; Told Values (kommen in OWL-Dateien nicht vor, die OWL DTP Fueller
        ;;; sind im datatype-property-value-cache, s.u.) 

        ;;; Dieser Cache enthaelt (s. Common*.lisp) Eintraege der Form:  
        ;;; a) CD-Objekt -> Told-Value 
        ;;; b) ABox-Individum -> List<List<CD-Attribut,Told-Value>>  
        
        (maphash #'(lambda (ind told-value) 

                     (cond ((gethash (list ind +individual-exists-concept+) 
                                     (individual-instance-cache substrate))
                            
                            ;;; ABox-Individuum  

                            (dolist (av told-value) ;; (attribute told-value)

                              (let ((told-value (second av))
                                    (cd-attribute (first av)))
                         
                                (node-instance substrate told-value 
                                               :description
                                               `(,+marker-for-abox-thing+
                                                 ,+marker-for-abox-cd-value+
                                                 ,told-value))

                                (nodes-related substrate ind told-value 
                                               `(,+marker-for-abox-relationship+
                                                 ,+marker-for-abox-told-value-relationship+ 
                                                 ,cd-attribute)))))
                                                 
                           (t

                            (let ((cd-object ind))
                              
                              (node-instance substrate told-value
                                             :description `(,+marker-for-abox-thing+ 
                                                            ,+marker-for-abox-cd-value+ 
                                                            ,told-value))

                              (nodes-related substrate cd-object told-value
                                             `(,+marker-for-abox-relationship+
                                               ,+marker-for-abox-told-value-relationship+))))))

                 told-values-cache)
      
        ;;; Role Assertions 
        ;;; und Object Properties (auch OWL Annotation Object Properties) 
        ;;; KEINE OWL Datatype (Annotation) Properties 

        ;;; Annotation Object Properties 
        ;;; zwischen OWL-Individuen registrieren 
        
        (dolist (role (dl-prover-all-roles substrate))
          (setf (gethash role ancestors)
                (atomic-role-ancestors role tbox)))

        (dolist (ra (all-annotation-role-assertions abox))
          (let ((from (first (first ra)))
                (to (second (first ra)))
                (role (second ra)))

            (setf (gethash role annotation-object-properties) t) 

            (node-instance substrate from
                           :description
                           `(,+marker-for-owl-thing+
                             ,+marker-for-owl-individual+))
            
            (node-instance substrate to
                           :description
                           `(,+marker-for-owl-thing+
                             ,+marker-for-owl-individual+))

            (dolist (role (gethash role ancestors))
              
              (nodes-related substrate from to 
                             `(,+marker-for-owl-relationship+
                               ,+marker-for-owl-object-property-relationship+
                               ,+marker-for-owl-object-annotation-property-relationship+ 
                               ,role)))))

        ;;; nun die normalen Role Assertions und Object Properties 
        ;;; List<ind,ind> oder List<owl-ind,owl-ind> 
        ;;; role-assertions-in-domain-cache enthaelt sowohl object- also auch 
        ;;; annotation object properties... daher Hashtable annotation-object-properties 


        (maphash #'(lambda (ind role-assertions)
                     (declare (ignorable ind))
                     (dolist (ra role-assertions)
                       (let ((from (first (first ra)))
                             (to (second (first ra)))
                             (role (second ra)))

                         ;;; keine Annotation Object Properties bearbeiten
                         ;;; (wurden oben schon eingtragen) 

                         (unless (gethash role annotation-object-properties)

                           (let ((object-property-p 
                                  ;;; HACK! Aber was soll ich machen?
                                  (and owl-p (is-url-p role))))

                             (dolist (node (list from to))
                               (node-instance substrate node
                                              :description
                                              `(,+marker-for-abox-thing+
                                                ,+marker-for-abox-individual+

                                                ,@(when object-property-p 
                                                    `(,+marker-for-owl-thing+
                                                      ,+marker-for-owl-individual+)))))
            
                             (let ((to   (if (consp role) from to))
                                   (from (if (consp role) to from))
                                   (role (if (consp role) (second role) role)))

                               (dolist (role (gethash role ancestors))

                                 (unless (eq role '*top-object-role*)
                                   
                                   (nodes-related substrate from to 
                                                  `(,+marker-for-abox-relationship+ 
                                                    ,+marker-for-abox-role-relationship+ 
                                                    ,@(when object-property-p 
                                                        `(,+marker-for-owl-relationship+ 
                                                          ,+marker-for-owl-object-property-relationship+))
                                                    ,role))

                                   (let ((inv (dl-prover-atomic-role-inverse substrate role)))
                                     (unless (consp inv)
                                       (nodes-related substrate to from
                                                      `(,+marker-for-abox-relationship+ 
                                                        ,+marker-for-abox-role-relationship+ 
                                                        ,@(when object-property-p 
                                                            `(,+marker-for-owl-relationship+ 
                                                              ,+marker-for-owl-object-property-relationship+))
                                                        ,inv))))))))))))

                 role-assertions-in-domain-cache)

        ;;;
        ;;; SAME-AS / DIFFERENT-FROM
        ;;;
        
        
        (dolist (sa (all-same-as-assertions abox))
          (let ((from (second sa))
                (to (third sa)))

	    (nodes-related substrate from to 
			   `(,+marker-for-abox-relationship+ 
			     ,+marker-for-abox-same-as-relationship+))))

        (dolist (sa (all-different-from-assertions abox))
          (let ((from (second sa))
                (to (third sa)))

	    (nodes-related substrate from to 
			   `(,+marker-for-abox-relationship+ 
			     ,+marker-for-abox-different-from-relationship+))))


        ;;;
        ;;; Ab hier nur noch OWL! 
        ;;; 
        ;;; Die OWL Datatypy (Annotation) Properties sind
        ;;; durch common*.lisp im datatype-values-and-types-cache gelandet
        ;;;
        ;;; Datatype Properties sind nur fuer Individuen definiert, 
        ;;; Datatype Annotation Properties hingegen fuer 
        ;;; - die Ontologie,
        ;;; - die Klassen, und 
        ;;; - die Individuen  
        ;;; 
        ;;; Es gibt sowohl RDFS Comment etc. als auch
        ;;; selbstdefinierte Annotation Datatype Properties 
        ;;; 
                  
        (maphash #'(lambda (node dtps)
                     (let ((type (if (gethash (list node +individual-exists-concept+) 
                                              (individual-instance-cache substrate))
                                     :individual 
                                   (if (gethash node atoms-hash)
                                       :class
                                     :ontology))))

                       (ecase type 

                         (:individual 
                          (node-instance substrate node 
                                         :description
                                         `(,+marker-for-owl-thing+ 
                                           ,+marker-for-owl-individual+)))

                         (:class 
                          (node-instance substrate node 
                                         :description
                                         `(,+marker-for-owl-class+)))
                  
                         (:ontology
                          ))

                       (unless (eq type :ontology)

                         (loop as ((role value orig-value) annotation) in dtps do

                               (node-instance substrate value
                                              :description
                                              `(,+marker-for-owl-thing+ 
                                                ,+marker-for-owl-literal+
                                                ;; "Selbstannotation" fuer Literale
                                                ,value))

                               (node-instance substrate value
                                              :description
                                              `(,+marker-for-owl-thing+ 
                                                ,+marker-for-owl-literal+
                                                ;; "Selbstannotation" fuer Literale
                                                ,orig-value))
                               
                               (nodes-related substrate node value
                                              `(,+marker-for-owl-relationship+ 
                                                ,+marker-for-owl-datatype-property-relationship+
                                                ,@(when annotation 
                                                    `(,+marker-for-owl-datatype-annotation-property-relationship+))
                                                ,role))))))
                  
                 datatype-property-values-and-types-cache)))))

;;;
;;;
;;;

(defmethod reset-caches :after ((substrate mirror-data-substrate))
  (with-slots (data-nodes data-edges told-info) substrate
    
    (clrhash data-nodes)
    (clrhash data-edges)

    (dolist (entry (reverse told-info))
      (apply (symbol-function (first entry))
             (cons substrate (cdr entry))))))

;;;
;;;
;;;

(nrql-defun (set-mirror-data-box
             :doc ((:doc-type :short)
                   (:category :substrate-layer)
                   (:description "Like \\funref{set-data-box}, but a mirror data box is created")))
  (name)
  (setf *type-of-substrate* 'mirror-data-substrate)
  (enable-abox-mirroring)
  (set-data-box name))

(nrql-defmacro (in-mirror-data-box :nrql-function set-mirror-data-box
                                   :doc 
                                   ((:doc-type :short)
                                    (:category :substrate-layer))))

;;;
;;;
;;;

#+:sql-substrate
(nrql-defun (set-mirror-sql-data-box) (name)
  (setf *type-of-substrate* 'mirror-sql-data-substrate)
  (enable-abox-mirroring)
  (set-sql-data-box name))

#+:sql-substrate
(nrql-defmacro (in-mirror-sql-data-box :nrql-function set-mirror-sql-data-box))


