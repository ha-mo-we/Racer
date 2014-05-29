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

(nrql-defun (|OWLAPI-storeImage|) (fn &optional reasoners)

  (ts:check-if-unsafe)

  (persistence-manager:make-object-persistent 
   
   (list (or (mapcar #'(lambda (reasoner) 
                         (let* ((reasoner (find-reasoner reasoner)))
                           (list (find-abox (owlapi-abox reasoner))
                                 (find-tbox (owlapi-tbox reasoner)))))
                     reasoners)
             (loop as reasoner being the hash-value of *reasoners* 
                   collect (list (find-abox (owlapi-abox reasoner))
                                 (find-tbox (owlapi-tbox reasoner)))))

         *current-abox*
         *current-tbox*
         
         *reasoners* 
         *cur-reasoner* 
         *default-reasoner* 
         *ano-concept* 
         *default-reasoner-name*
         
         *prefix-mappings* 
         *cached-prefixes*
         *uri-mirror-table*)

   fn)
  fn)


(nrql-defun (|OWLAPI-restoreImage|) (fn)
  (with-environment ()
    (let ((obj 
           (persistence-manager:load-persistent-object fn)))

      (loop as (abox tbox) in (pop obj) do 
            (setf (find-abox (abox-name abox)) abox)
            (setf (find-tbox (tbox-name (tbox-associated-with-abox abox))) 
                  (tbox-associated-with-abox abox))
            (setf (find-tbox (tbox-name tbox)) tbox))
    
      (setf *current-abox* (pop obj)
            *current-tbox* (pop obj))

      (setf (find-abox (abox-name *current-abox*)) *current-abox*)
      (setf (find-tbox (tbox-name (tbox-associated-with-abox *current-abox*))) 
            *current-tbox*)
      (setf (find-tbox (tbox-name *current-tbox*)) *current-tbox*)

      (setf *reasoners*  (pop obj)
            *cur-reasoner* (pop obj)
            *default-reasoner* (pop obj)
            *ano-concept* (pop obj)
            *default-reasoner-name* (pop obj)

            *prefix-mappings* (pop obj)
            *cached-prefixes* (pop obj)
            *uri-mirror-table* (pop obj))

      fn)))


