;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

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

(in-package :cl-user)


(defpackage :dag
  (:use
   #+:clim :clim-lisp
   #+:clim :clim
   #-:clim :common-lisp)
  #+:clim 
  (:shadowing-import-from :clim-lisp #:boolean)
   
  (:export 
   
   #:dag
   #:dag-name
   #:make-dag
   #:make-dag-node

   #:copy-dag-node

   #:com-make-subdag-from-node
   #:com-exchange-child-node-positions

   #:dag-browser
   #:dag-options
   #:dag-info
   #:dag-display

   #:draw-dag-node
   #:draw-dag-display
   #:show-top-p
   #:show-bottom-p
   #:accept-options

   #:dag-node
   #:dag-nodes

   #:dag-roots
   #:dag-leaves

   #:dag-top
   #:dag-bottom

   #:orientation

   #:dag-root
   #:find-dag-node

   #:dag-node-name
   #:dag-node-marked-p
   #:dag-node-parents
   #:dag-node-children
   #:dag-node-ancestors
   #:dag-node-descendants
   #:in-dag

   #:mark-dag-node
   #:fn-mark-dag-node
   #:unmark-dag-node
   #:mark-all-dag-nodes
   #:fn-mark-all-dag-nodes
   #:unmark-all-dag-nodes

   #:dag-isomorphic-p
   #:node-equivalent-p
   #:insert-dag-node
   #:delete-dag-node

   #:visualize-dag

   #:make-postscript-file
   #:create-clos-classes
   #:compute-transitive-closure
   #:compute-hasse-diagram))

#+:racer-server
(defpackage thematic-substrate
  (:nicknames ts)
  (:use
   #+:clim clim
   #+:clim clim-lisp
   #-:clim common-lisp
   :dag
   :racer
   :nrql-symbols
   #+:sql-substrate :clsql
   )
  (:export 
   #:create-alias-dbox
   #:create-copy-dbox
   #:delete-dbox
   #:tree-substitute
   #:tree-find
   #:tree-flatten
   #:tree-map
   #:server-hook
   #:*server-hooks*
   #:to-keyword
   #:nrql-defun
   #:nrql-defmethod
   #:nrql-defmacro
   #:check-if-unsafe
   #:shrink-whitespaces
   #:string-substitute
   #:blank-line-p
   #:whitespace-char-p
   )

  (:shadow :query)
  #+:clim
  (:shadowing-import-from :clim-lisp #:boolean)   
  (:shadowing-import-from :racer
   #:attribute-type
   #:top #:bottom
   #:inv #:same-as))


#-:racer-server
(defpackage :thematic-substrate
  (:nicknames :ts)
  (:use
   #+:clim :clim
   #+:clim :clim-lisp
   #-:clim :common-lisp
   :dag
   :racer
   :nrql-symbols
   #+:sql-substrate :clsql
   )
  (:export
   #:nrql-defun
   #:nrql-defmethod
   #:nrql-defmacro

   #:shrink-whitespaces
   #:string-substitute
   #:blank-line-p
   #:to-keyword
   )
  (:shadow #:query)
  #+:clim
  (:shadowing-import-from :clim-lisp
   #:boolean)   
  (:shadowing-import-from :racer
   #:attribute-type
  ))

  
