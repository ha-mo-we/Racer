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

(defstruct (gci-absorption-dependency
	    (:conc-name gci-dependency-)
	    (:predicate gci-dependency-p)
	    (:constructor make-gci-dependency-internal (type removed added)))
  (type nil)				; absorption action represented as symbol
  (removed nil)				; list of removed axioms/concepts
  (added nil)				; list of added axioms/concepts
  )

(defvar absorption-types
  '(gci-pair-to-defined                 ; transform 2 matching gcis into 1 defined concept
    defined-to-gci-pair                 ; split 1 defined concept into 2 gci pairs
    defined-to-prim-and-gci             ; transform 1 defined concept into 1 primitive one and add 1 new gci
    primitive-to-atomic-and-gci         ; transform 1 primitive concept into 1 atomic one and add 1 new gci
    primitive-det			; primitive concept deterministically absorbed 1 gci
    primitive-non-det			; primitive concept non-deterministically absorbed 1 gci
    unfold-primitive			; unfold primitive concept definition
    unfold-defined			; unfold defined concept definition
    regroup				; combine 2 gcis into 1 gci
    range				; absorb all-gci as role range
    cd-range				; absorb cd-all-concept as role range
    simple-domain			; absorb gci as simple role domain
    qualified-domain			; absorb gci as qualified role domain
    functional                          ; at-most-1-gci makes role functional
    ))

(defmethod print-object ((object gci-absorption-dependency) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (when (gci-dependency-removed object)
      (format stream "~S ~S ~S"
              (gci-dependency-type object)
              ':removed 
              (gci-dependency-removed object)))
    (when (gci-dependency-added object)
      (if (gci-dependency-removed object)
        (format stream " ")
        (format stream "~S " (gci-dependency-type object)))
      (format stream "~S ~S"
              ':added
              (gci-dependency-added object)))))
