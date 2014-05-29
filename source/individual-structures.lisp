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

(defvar *use-unique-name-assumption* nil)
(defparameter *print-individual-info* nil)



(defstruct (individual
            (:include racer-set-element)
            (:constructor make-individual (name-set abox))
            (:predicate individual-p-internal))
  (name-set nil)                        ; set of individual names; first element is used as canonical name
  (abox nil)                            ; associated abox
  (encoded-axioms nil)                  ; encoded versions of initial concept assertions
  (subgraph nil)                        ; associated subgraph
  (has-fillers 0)                       ; number of outgoing role assertions (used in constraint store)
  (used-as-filler 0)                    ; number of incoming role assertions (used in constraint store)
  (realized-p nil)                      ; already realized?
  (told-subsumers nil)                  ; concept names which are told subsumers
  (parent-concepts nil)                 ; parent concept names
  (ancestor-concepts nil)               ; ancestor names
  (concept-constraints nil)             ; concept constraints from completion; used to create ind model
  (model nil)                           ; pseudo individual model
  (concept-name nil)                    ; name of associated concept constructed from completion
  (told-non-subsumers nil)              ; concept names which are known non-subsumers
  (ind-concept-constraints nil)         ; concept constraints from completion that have no or-dependencies;
                                        ; used to create the ind concept
  (ind-concept-list nil)                ; concepts used to create the ind concept
  (completion-concept nil)              ; concept constructed from completion
  (model-built-p nil)                   ; has the data required for the individual pseudo model been stored?
  (incoming-role-assertions nil)        ; role assertions where ind is filler
  (outgoing-role-assertions nil)        ; role assertions starting from ind
  (distinct-p *use-unique-name-assumption*)     ; is this individual distinct? Only possible if UNA is set
  (told-disjoints nil)                  ; list of individuals knowm to be disjoint
  (added-relation-constraints nil)      ; used for ind pseudo model
                                        ; contains added relation constraints enforced by number restrictions
  (assertions nil)                      ; original concept assertions
  (attribute-assertions nil)            ; concrete domain attribute assertions
  (language *dl-empty*)                 ; dl language for known assertions
  )

(defmethod print-object ((object individual) stream)
  (when (and *print-individual-info* (individual-realized-p object))
    (format stream "("))
  (if (or (individual-incoming-role-assertions object)
          (individual-outgoing-role-assertions object))
    (format stream 
            #+:debug "~S/~D/~D"
            #-:debug "~S"
            (if (rest (individual-name-set object))
              (individual-name-set object)
              (first (individual-name-set object)))
            #+:debug (length (individual-incoming-role-assertions object))
            #+:debug (length (individual-outgoing-role-assertions object)))
    (format stream "~A" (first (individual-name-set object))))
  #+:debug 
  (when (and *print-individual-info* (individual-realized-p object))
    (format stream " : ~S)" (transform-concepts (individual-parent-concepts object)))))

(defmethod print-object :around ((object individual) stream)
  (if *print-internals*
    (print-unreadable-object (object stream :type nil :identity t)
      (call-next-method))
    (call-next-method)))

(defun individuals-disjoint-p-internal (ind-1 ind-2)
  (or (individual-distinct-p ind-1)
      (individual-distinct-p ind-2)
      (if (member ind-1 (individual-told-disjoints ind-2))
        (progn
          #+:debug (assert (member ind-2 (individual-told-disjoints ind-1)))
          t)
        (progn
          #+:debug (assert (not (member ind-2 (individual-told-disjoints ind-1))))
          nil))))

(defun individual-sets-disjoint-p-internal (abox ind-name-set-1 ind-name-set-2)
  (let ((ind-set-1
         (loop for ind-name in ind-name-set-1
               unless (true-old-individual-p ind-name)
               do (return-from individual-sets-disjoint-p-internal nil)
               collect (find-individual abox ind-name)))
        (ind-set-2
         (loop for ind-name in ind-name-set-2
               unless (true-old-individual-p ind-name)
               do (return-from individual-sets-disjoint-p-internal nil)
               collect (find-individual abox ind-name))))
    (or *use-unique-name-assumption*
        (every #'individual-distinct-p ind-set-1)
        (every #'individual-distinct-p ind-set-2)
        (loop for ind-1 in ind-set-1
              thereis
              (loop for ind-2 in ind-set-2
                    thereis 
                    (if (member ind-1 (individual-told-disjoints ind-2))
                      (progn
                        #+:debug (assert (member ind-2 (individual-told-disjoints ind-1)))
                        t)
                      (progn
                        #+:debug (assert (not (member ind-2 (individual-told-disjoints ind-1))))
                        nil)))))))
