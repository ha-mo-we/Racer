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

(defconstant +back-track-size+ 5)

(defun make-backtrack-stack ()
  (let ((result (make-array +back-track-size+
                            :element-type '(or function null)
                            :adjustable t
                            :fill-pointer t)))
    (setf (fill-pointer result) 0)
    result))

(race-inline (push-backtrack-stack
              pop-backtrack-stack
              empty-backtrack-stack-p))

(defun push-backtrack-stack (new-element stack)
  (unless stack
    (setf stack (make-backtrack-stack)))
  (vector-push-extend new-element stack)
  stack)

(defun pop-backtrack-stack (stack)
  (prog1
    (vector-pop stack)
    (setf (aref stack (fill-pointer stack)) 0)))

(defun empty-backtrack-stack-p (stack)
  (and stack (zerop (fill-pointer stack))))

(defun handle-clash-with-backtrack-state (state optional-1 optional-2 optional-3 unused)
  (declare (ignore unused)
	   #+:allegro (:explain :tailmerging))
  (let ((partially-expanded-or-stack (state-partially-expanded-or-stack state)))
    (if (and partially-expanded-or-stack
             (not (empty-backtrack-stack-p partially-expanded-or-stack)))
      (let ((backtrack-fn (pop-backtrack-stack partially-expanded-or-stack)))
        (race-trace  ("backtrack-fn ~S, stack ~S"
                      backtrack-fn
                      (copy-seq partially-expanded-or-stack))
                    ("~&Calling (~S nil ~S ~S ~S ~S)~%" 
                     backtrack-fn
                     state optional-1 optional-2 optional-3))
        (funcall backtrack-fn nil state optional-1 optional-2 optional-3))
      (progn
        (race-trace ("backtrack stack empty or NIL in state ~S" state))
        ;(break)
        nil))))

(defun unwrap-backtrack-state (state unused-1 unused-2 unused-3 unused-4)
  (declare #+:allegro (:explain :tailmerging)
	   (ignore unused-1 unused-2 unused-3 unused-4))
  (let ((partially-expanded-or-stack (state-partially-expanded-or-stack state)))
    (if partially-expanded-or-stack
      (if (not (empty-backtrack-stack-p partially-expanded-or-stack))
        (let ((backtrack-fn (pop-backtrack-stack partially-expanded-or-stack)))
          (race-trace #+:ccl ("backtrack-fn ~S" backtrack-fn)
                      ("~&Calling (~S t ~S nil nil nil)~%" 
                       backtrack-fn
                       state))
          (funcall backtrack-fn t state nil nil nil))
        t)
      t)))
