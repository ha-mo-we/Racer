;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: TOOLS; Base: 10 -*-

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

(in-package :tools)

(defparameter *tracer-tools-menu* nil)

(defvar *tracer-restore-lisp-functions* nil)

(defun tracer-make-point (h &optional v)
  (list h v))

(defun tracer-point-v (p)
  (declare (ignore p)))

(defclass tracer-simple-view
  ()
  ((nick-name :reader view-nick-name :initarg :view-nick-name))
  )

(defmethod view-nick-name ((view t))
  )

(defmethod tracer-view-container ((view tracer-simple-view))
  view)

(defclass tracer-menu-item
  (tracer-simple-view capi:menu-item)
  ())

(defmethod tracer-menu-item-enable ((item tracer-menu-item))
  )

(defmethod tracer-menu-item-disable ((item tracer-menu-item))
  )

(defmethod tracer-menu-item-enabled-p ((menu tracer-menu-item))
  )

(defclass tracer-dialog
  (tracer-simple-view capi:interface)
  ())

(defmethod tracer-view-named (nickname (dialog tracer-dialog))
  (third (capi:layout-description (capi:pane-layout dialog))))

(defmethod tracer-window-select ((w tracer-dialog))
  ;(capi:raise-interface w)
  )

(defmethod get-selected-items ((dialog tracer-dialog))
  (capi:choice-selected-items (third (capi:layout-description (capi:pane-layout dialog)))))

(defclass tracer-menu
  (tracer-simple-view capi:menu)
  ())

(defmethod tracer-find-menu-item ((menu tracer-menu) title)
  )

(defmethod tracer-menu-items ((menu tracer-menu)
                              &optional (menu-item-class 'tracer-menu-element))
  (capi:menu-items menu))

(defclass tracer-pull-down-menu
  (tracer-menu)
  ())

(defun tracer-insert-menu-item-after-menu-item (menu
                                                reference-menu-item
                                                inserted-menu-item)
  (declare (ignore menu reference-menu-item inserted-menu-item)))

(defclass tracer-table-dialog-item
  ()
  ())

(defclass tracer-sequence-dialog-item
  (tracer-simple-view tracer-table-dialog-item capi:list-panel)
  ()
  (:default-initargs
   :vertical-scroll t
   :items-count-function 'length
   :items-get-function (lambda (list n)
                         (nth n list))
   :items-map-function (lambda (items fn collect-results-p)
                         (if collect-results-p
                             (mapcar fn items)
                           (loop for item in items
                                 do (funcall fn item))))
   ))

(defmethod tracer-view-container ((view tracer-sequence-dialog-item))
  (capi:element-interface view))

(defmethod get-selected-items ((table tracer-sequence-dialog-item))
  (capi:choice-selected-items table))

(defmethod tracer-cell-select ((item tracer-sequence-dialog-item) h &optional v)
  (setf (capi:choice-selected-item item) h))

(defmethod tracer-scroll-to-cell ((item tracer-sequence-dialog-item) h &optional v)
  (tracer-cell-select item h v)
  ;(capi:scroll item ':vertical ':page 5)
  )

(defmethod tracer-cell-deselect ((item tracer-sequence-dialog-item) h &optional v)
  )

(defmethod tracer-selected-cells ((item tracer-sequence-dialog-item))
  (capi:choice-selected-items item))

(defmethod tracer-redraw-cell ((item tracer-sequence-dialog-item) h &optional v)
  (capi:redisplay-collection-item item h))

(defmethod tracer-table-sequence ((item tracer-sequence-dialog-item))
  (capi:collection-items item))

(defmethod tracer-index-to-cell ((item tracer-sequence-dialog-item) index)
  (elt (capi:collection-items item) index))

(defmethod tracer-set-table-sequence ((table tracer-sequence-dialog-item) seq)
  (setf (capi:collection-items table) seq))

(defmethod tracer-set-table-sequence ((dialog tracer-dialog) seq)
  (setf (capi:collection-items (third (capi:layout-description (capi:pane-layout dialog))))
        seq))

(defmethod tracer-update-table-sequence ((table tracer-sequence-dialog-item)
                                         changed-entry
                                         new-seq)
  (let* ((seq (capi:collection-items table))
         (pre-cdr (loop for last-cdr = nil then list
                        for list on seq
                        for elem = (first list)
                        when (eq elem changed-entry)
                        do (return last-cdr))))
    (if pre-cdr
        (progn
          (setf (cdr pre-cdr) new-seq)
          (setf (capi:collection-items table) seq)
          ;; needed to get old CAPI caches invalidated
          )
      (setf (capi:collection-items table) new-seq))))

(defun tracer-command-key-p ()
  )

(defun tracer-option-key-p ()
  )

(defun tracer-control-key-p ()
  )

(defun tracer-double-click-p ()
  )

(defun tracer-ed-beep ()
  #+:macosx (cocoa:ns-beep)
  #+:win32 (win32:message-beep "1"))

(defun tracer-get-string-from-user (message 
                                    &key
                                    initial-string
                                    (window-title ""))
  (declare (ignore window-title))
  (capi:prompt-for-string message :initial-value initial-string))

(defun tracer-edit-definition (name)
  (loop for symbol in (find-all-symbols name)
        until (editor:find-source-command nil symbol t)))

(defun tracer-find-window (title &optional class)  
  (declare (ignore title class)))

(defclass tracer-buffered-output-stream-mixin
  ()
  ())

(defmethod tracer-stream-force-output ((stream tracer-buffered-output-stream-mixin))
  )
