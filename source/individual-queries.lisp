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

;;; ======================================================================

(defmacro individuals-related? (individual-1
                              individual-2 
                              role-term
                              &optional (abox nil abox-supplied-p))
  (check-type individual-1 symbol)
  (check-type individual-2 symbol)
  (check-type abox (or symbol null))
  (if abox-supplied-p
    `(individuals-related-p ',individual-1 ',individual-2 ',role-term ',abox)
    `(with-abox-defined-check *current-abox*
       (individuals-related-p ',individual-1 ',individual-2 ',role-term 
                              *current-abox*))))

(defun individuals-related-p (ind-predecessor-name-set
                                  ind-filler-name-set
                                  role-term
                                  abox)
  (setf abox (find-abox abox))
  (check-type abox abox)
  ;(check-role-term role-term)
  (with-nrql-standard-settings
    (racer-answer-query () `(,ind-predecessor-name-set
                             ,ind-filler-name-set
                             ,role-term)
                         :abox (abox-name abox))))

(defun retrieve-individual-filled-roles (ind-predecessor 
                                         ind-filler 
                                         abox
                                         &key synsets-p negated-p no-inverses-p roles)
  (check-type ind-predecessor symbol)
  (check-type ind-filler symbol)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (ensure-knowledge-base-state ':abox-prepared abox)
  (with-nrql-standard-settings
    (if synsets-p
        (transform-into-role-synsets 
         (mapcar #'(lambda (role) (get-tbox-role (tbox abox) role))
                 (racer-retrieve-individual-filled-roles ind-predecessor 
                                                         ind-filler
                                                         :roles roles
                                                         :abox (abox-name abox)
                                                         :no-inverses-p no-inverses-p
                                                         :negated-p negated-p))
         (associated-tbox abox))
      (racer-retrieve-individual-filled-roles ind-predecessor ind-filler 
                                              :roles roles
                                              :abox (abox-name abox)
                                              :no-inverses-p no-inverses-p
                                              :negated-p negated-p))))

(defmacro individual-filled-roles (ind-predecessor
                                 ind-filler 
                                 &optional (abox nil abox-supplied-p))
  (check-type abox (or symbol null))
  (if abox-supplied-p
    `(retrieve-individual-fillers ',ind-predecessor ',ind-filler ',abox)
    `(with-abox-defined-check *current-abox*
       (retrieve-individual-filled-roles ',ind-predecessor
                                         ',ind-filler
                                         *current-abox*))))

(defun retrieve-individual-fillers (ind-predecessor 
                                    role-term 
                                    abox &key told)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type ind-predecessor symbol)

  ;; (check-role-term role-term)
  ;; Role checks 
  ;; must be uncommented, otherwise error for negated roles (not R).
  ;; nRQL's racer-answer-query can handle these 
  ;; changed by MW 11.10.2009

  (ensure-knowledge-base-state ':abox-prepared abox)
  ;;(ensure-role-is-known role-term (tbox abox))
  ;;(ensure-no-cd-attribute role-term (tbox abox))
  
  #| 
  (mapcar #'(lambda (bindings)
              (second (first bindings)))
          (with-nrql-standard-settings
            (racer-answer-query '(?x) `(,ind-predecessor ?x ,role-term)
                                 :abox (abox-name abox))))
  |# 

  ;;; changed by MW, 7.11.2004
  ;;; changed by MW, 6.10.2006

  (with-nrql-standard-settings
    (let ((*told-information-reasoning-p* told))
      (racer-retrieve-individual-fillers ind-predecessor role-term 
                                         :abox (abox-name abox)
                                         :remove-synonyms-p nil
                                         :show-synonyms-p nil))))

(defmacro individual-fillers (ind-predecessor
                            role-term 
                            &optional (abox nil abox-supplied-p))
  (check-type abox (or symbol null))
  (if abox-supplied-p
    `(retrieve-individual-fillers ',ind-predecessor ',role-term ',abox)
    `(with-abox-defined-check *current-abox*
       (retrieve-individual-fillers ',ind-predecessor
                                    ',role-term
                                    *current-abox*))))




(defun retrieve-individual-predecessors (ind 
                                         role-term 
                                         abox &key told)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-type ind symbol)

  ;; (check-role-term role-term)
  ;; Role checks 
  ;; must be uncommented, otherwise error for negated roles (not R).
  ;; nRQL's racer-answer-query can handle these 
  ;; changed by MW 11.10.2009

  (ensure-knowledge-base-state ':abox-prepared abox)
  ;;(ensure-role-is-known role-term (tbox abox))
  ;;(ensure-no-cd-attribute role-term (tbox abox))
  
  #| 
  (mapcar #'(lambda (bindings)
              (second (first bindings)))
          (with-nrql-standard-settings
            (racer-answer-query '(?x) `(,ind ?x ,role-term)
                                 :abox (abox-name abox))))
  |# 

  ;;; changed by MW, 7.11.2004
  ;;; changed by MW, 6.10.2006

  (with-nrql-standard-settings
    (let ((*told-information-reasoning-p* told))
      (racer-retrieve-individual-fillers ind `(inv ,role-term)
                                             :abox (abox-name abox)
                                             :remove-synonyms-p nil
                                             :show-synonyms-p nil))))

(defmacro individual-predecessors (ind
                                   role-term 
                                   &optional (abox nil abox-supplied-p))
  (check-type abox (or symbol null))
  (if abox-supplied-p
    `(retrieve-individual-predecessors ',ind ',role-term ',abox)
    `(with-abox-defined-check *current-abox*
       (retrieve-individual-predecessors ',ind
                                         ',role-term
                                         *current-abox*))))



(defun retrieve-direct-predecessors (role-term 
                                          ind-filler 
                                          abox)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-role-term role-term)
  (check-type ind-filler symbol)
  (ensure-knowledge-base-state ':abox-prepared abox)
  (ensure-role-is-known role-term (tbox abox))
  (ensure-no-cd-attribute role-term (tbox abox))

  #| 
  (mapcar #'(lambda (bindings)
              (second (first bindings)))
          (with-nrql-standard-settings
            (racer-answer-query '(?x) `(?x ,ind-filler ,role-term)
                                 :abox (abox-name abox))))
  |# 

  ;;; changed by MW, 6.10.2006

  (with-nrql-standard-settings
   (racer-retrieve-individual-fillers ind-filler `(inv ,role-term) 
                                      :abox (abox-name abox)
                                      :remove-synonyms-p nil
                                      :show-synonyms-p nil)))

(defmacro direct-predecessors (role-term
                               ind-filler
                               &optional (abox nil abox-supplied-p))
  (check-type abox (or symbol null))
  (if abox-supplied-p
    `(retrieve-direct-predecessors ',role-term ',ind-filler ',abox)
    `(with-abox-defined-check *current-abox*
       (retrieve-direct-predecessors ',role-term
                                     ',ind-filler
                                     *current-abox*))))


(defun retrieve-related-individuals (role-term abox)
  (setf abox (find-abox abox))
  (check-type abox abox)
  (check-role-term role-term)
  (with-race-trace-sublevel ("retrieve-related-individuals"
                             :arguments (list role-term abox)
                             :trace-result t)
    (ensure-knowledge-base-state ':abox-prepared abox)
    (ensure-role-is-known role-term (tbox abox))
    (ensure-no-cd-attribute role-term (tbox abox))
    (with-nrql-standard-settings
      (racer-retrieve-related-individuals role-term :abox (abox-name abox)))))

(defmacro related-individuals (role-term &optional (abox-name nil abox-name-supplied-p))
  (if abox-name-supplied-p
    `(retrieve-related-individuals ',role-term ',abox-name)
    `(with-abox-defined-check *current-abox*
       (retrieve-related-individuals ',role-term *current-abox*))))



(defmacro individual-told-attribute-value (ind
                                         attribute 
                                         &optional (abox nil abox-supplied-p))
  (check-type abox (or symbol null))
  (if abox-supplied-p
    `(retrieve-individual-attribute-value ',ind ',attribute ',abox)
    `(with-abox-defined-check *current-abox*
       (retrieve-individual-told-attribute-value ',ind
                                                 ',attribute
                                                 *current-abox*))))

(defun retrieve-individual-told-attribute-value (ind attribute abox)
  (check-type ind symbol)
  (check-type attribute symbol)
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (ensure-knowledge-base-state ':abox-prepared abox)
  (first 
   (mapcar #'(lambda (bindings)
               (first (second (first bindings))))
           (with-nrql-standard-settings
             (racer-answer-query `((:told-value-if-exists (,attribute ,ind))) 
                                 `(,ind (an ,attribute))
                                  :abox (abox-name abox))))))
  

(defun retrieve-individual-annotation-property-fillers (individual-name role abox &optional with-types-p)
  (check-type individual-name symbol)
  (check-type role symbol)
  (check-type abox (or symbol abox))
  (setf abox (find-abox abox))
  (ensure-knowledge-base-state ':abox-prepared abox)
  (racer-prepare-substrate :abox (abox-name abox) :prepare-now-p t)
  (retrieve-annotation-datatype-values *cur-substrate* individual-name role with-types-p))
