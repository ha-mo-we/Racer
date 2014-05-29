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

(defgeneric matches-p (query substrate-object ))

(defgeneric retrieve-matching-objects (substrate query &key only-thematic-objects))

;;;
;;; Reasoning: Simple Querys
;;;

(defmethod matches-p ((query substrate-simple-node-query) (description simple-node-description))
  (implies-p description query))

(defmethod matches-p ((query substrate-simple-edge-query) (description simple-edge-description))
  (implies-p description query))

;;;
;;;
;;;
;;;

(defmethod matches-p ((query substrate-racer-node-query) (description racer-node-description))
  (implies-p description query))

(defmethod matches-p ((query substrate-racer-edge-query) (description racer-edge-description))
  (implies-p description query))

;;;
;;;
;;;

(defmethod matches-p ((description description) (query query))
  (matches-p query description))

(defmethod matches-p ((query query) (description description))
  (nrql-error "Runtime error: no primary method (matches-p ~A ~A)" (type-of query) (type-of description)))

;;;
;;;
;;;

(defmethod matches-p ((query unary-query) (object substrate-node))
  (matches-p query (description object)))

(defmethod matches-p ((query substrate-predicate-node-query) (object substrate-node))
  (funcall (predicate query) object))

;;;
;;;
;;;

(defmethod retrieve-matching-objects ((substrate substrate) (query unary-query) &key only-thematic-objects)
  (declare (ignore only-thematic-objects))
  (remove-if-not #'(lambda (x) 
                     (matches-p query x))
                 (get-nodes substrate)))

(defmethod retrieve-matching-objects ((substrate racer-substrate) (query unary-query) &key only-thematic-objects)
  (remove-if-not #'(lambda (x) 
                     (matches-p query x))
                 (if only-thematic-objects
                     (thematic-nodes substrate)
                   (get-nodes substrate))))

;;;
;;;
;;;

(defmethod matches-p ((query binary-query) (object substrate-edge))
  (matches-p query (description object)))

(defmethod matches-p ((query substrate-predicate-edge-query) (object substrate-edge))
  (funcall (predicate query) object))

;;;
;;;
;;;

(defmethod retrieve-matching-objects ((substrate substrate) (query binary-query) &key only-thematic-objects)
  (declare (ignore only-thematic-objects))
  (remove-if-not #'(lambda (x) 
                     (matches-p query x))
                 (get-edges substrate)))

(defmethod retrieve-matching-objects ((substrate racer-substrate) (query binary-query) &key only-thematic-objects)
  (remove-if-not #'(lambda (x) 
                     (matches-p query x))
                 (if only-thematic-objects
                     (thematic-edges substrate)
                   (get-edges substrate))))
