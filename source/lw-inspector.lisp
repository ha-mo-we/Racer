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

(defmethod lw:get-inspector-values ((object symbol) (mode (eql 'racer))) 
  (let ((tbox (find-tbox object nil))
        (abox (find-abox object nil))
        (concept (find-concept object))
        (role (find-role object))
        (individual (find-individual *current-abox* object nil))
        (cd-object (find-cd-object *current-abox* object nil)))
    (when (or tbox abox concept role individual cd-object)
      (values 
       (delete nil
               (list (when tbox
                       'tbox)
                     (when abox
                       'abox)
                     (when concept
                       'concept)
                     (when role
                       'role)
                     (when individual
                       'individual)
                     (when cd-object
                       'cd-object)))
       (delete nil (list tbox abox concept role individual cd-object))
       nil
       (lambda (object key index new-value)
         (declare (ignore key index))
         (ecase (type-of object)
           (tbox (setf (find-tbox object) new-value))
           (abox (setf (find-abox object) new-value))
           (concept (setf (get-concept object) new-value))
           (role-node (setf (get-role object) new-value))
           (individual
            (setf (gethash object (abox-individuals-table (individual-abox individual))) new-value))
           (cd-object (setf (gethash object (abox-objects-table (cd-object-abox object))) new-value))))
       'symbol))))

(defmethod lw:get-inspector-values ((object cache) (mode (eql 'racer)))
  (values '(name null-marker successors)
          (list (cache-name object)
                (cache-null-marker object)
                (compute-cache-entries object))
          nil
          (lambda (object key index new-value)
            (declare (ignore key))
            (ecase index
              (0 (setf (cache-name object) new-value))
              (1 (setf (cache-null-marker object) new-value))
              (2 nil)))
          'cache))

(defmethod lw:get-inspector-values ((object dl-descriptor) (mode (eql 'racer)))
  (values '(language-set decoded-bit-mask)
          (list (dl-language-set object)
                (decode-bit-mask (dl-language-set object)))
          nil
          (lambda (object key index new-value)
            (declare (ignore object key index new-value)))
          'dl-descriptor))

(editor:defcommand "Inspect Current Symbol" (p)
     (declare (ignore p))
     (let ((x (editor:buffer-symbol-at-point (editor:current-buffer))))
       (when x
         (editor:message "Inspecting ~S" x)
         (inspect x))))

(editor:bind-key "Inspect Current Symbol" #("Control-c" "Control-i") :mode "Execute")
(editor:bind-key "Inspect Current Symbol" #("Control-c" "Control-i") :mode "Lisp")
