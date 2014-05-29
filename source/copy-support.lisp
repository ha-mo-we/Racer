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

(defun copy-hash-table (hash-table &optional (added-size 0) (transform-value-fn nil))
  (if (null hash-table)
    nil
    (if (hash-table-p hash-table)
      (let* ((custom-hash-function-p
              #+:allegro
              (eq (hash-table-hash-function hash-table) 'concept-hash)
              #+:sbcl
              (eq (sb-impl::hash-table-hash-fun hash-table) 'concept-hash)
              #-(or :allegro :sbcl) nil)
             (clone (racer-make-hash-table :test (hash-table-test hash-table)
                                           :size (+ added-size (hash-table-count hash-table))
                                           :rehash-size (hash-table-rehash-size hash-table)
                                           :rehash-threshold (hash-table-rehash-threshold hash-table)
                                           :structure-p custom-hash-function-p)))
        (if transform-value-fn
          (loop for key being the hash-key of hash-table using (hash-value value) do
                (setf (gethash key clone) (funcall transform-value-fn value)))
          (loop for key being the hash-key of hash-table using (hash-value value) do
                (setf (gethash key clone) value)))
        (incf-statistics *number-of-copied-hash-tables*)
        (incf-statistics *number-of-copied-hash-table-entriess* (hash-table-count hash-table))
        (incf-statistics *size-of-copied-hash-tables* (hash-table-size hash-table))
        clone)
      (error "~S is not of type hash-table" hash-table))))

(defun merge-hash-tables (hash-table-1
                             hash-table-2
                             &optional
                             (added-size nil)
                             (transform-value-fn nil))
  #+:debug
  (assert (and (hash-table-p hash-table-1) (hash-table-p hash-table-2)))
  #+(and :debug (or :allegro))
  (assert (and (eq (hash-table-hash-function hash-table-1)
                   (hash-table-hash-function hash-table-2))))
  (let ((merged-table (copy-hash-table hash-table-1
                                       (or added-size (hash-table-count hash-table-2))
                                       transform-value-fn)))
    (loop for key being the hash-key of hash-table-2 using (hash-value value)
          for old-entry = (gethash key merged-table)
          do
          (if old-entry
            (if transform-value-fn
              (setf (gethash key merged-table) (funcall transform-value-fn value))
              (setf (gethash key merged-table) (append value old-entry)))
            (setf (gethash key merged-table) value)))
    (incf-statistics *number-of-merged-hash-tables* 2)
    (incf-statistics *number-of-merged-hash-table-entriess*
                     (+ (hash-table-count hash-table-1) (hash-table-count hash-table-2)))
    (incf-statistics *size-of-merged-hash-tables*
                     (+ (hash-table-size hash-table-1) (hash-table-size hash-table-2)))
    merged-table))
