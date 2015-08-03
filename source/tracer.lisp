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

(defconstant +no-result-recorded+ '(no-result))

(defun no-result ()
  +no-result-recorded+)

(defconstant +no-arguments+ '(no-arguments))

(defun no-arguments ()
  +no-arguments+)

(defconstant +indent-offset+ 2)

(defvar *indent-level*)

(defparameter *current-trace-level* 0)

(defparameter *trace-history* nil)

(defparameter *trace-dialogs* nil)

(defparameter *trace-text-font* "Monaco")
(defparameter *trace-text-font-size* 11)

(defvar *ctraced-functions* nil)

(defvar *current-folder-entry*)

(defparameter *trace-manager-window-position* (tracer-make-point 400 50))
(defparameter *trace-manager-window-size* (tracer-make-point 520 400))
(defparameter *trace-manager-previous-search-string* "")
(defparameter *trace-manager-previous-filter-list* nil)
(defparameter *trace-manager-previous-filter-headers* t)
(defparameter *trace-manager-previous-subfilter* t)
(defparameter *trace-manager-previous-filter-logic* 'or)
(defparameter *trace-manager-previous-level-threshold* 0)

(defparameter *breakpoints* nil)

(defparameter *trace* t)
(defparameter *keep-trace-folders* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ignore-tracing* nil))

(defparameter *entry-number* 0)

(defstruct history-entry
  (name nil)
  (indent-level 0)
  (number (incf *entry-number*)))

(defun princ-indent (object stream)
  (multiple-value-bind (n-times rest)
                       (floor (history-entry-indent-level object) 2)
    (when (> n-times 0)
      (princ "  " stream))
    (loop repeat (- n-times 1)
          do (princ "| " stream))
    (unless (zerop rest)
      (princ " " stream))))

(defmethod print-object :before ((object history-entry) stream)
  (if (find (history-entry-number object) *breakpoints*)
    (princ "#" stream)
    (princ " " stream)))

(defmethod print-object ((object history-entry) stream)
  (princ (history-entry-name object) stream))

(defstruct (folder-history (:include history-entry))
  (expanded nil)
  (entries nil)
  (last-entry nil)
  (arglist-name-value-assoc nil)
  (arglist-name-value-assoc-shown-p nil)
  (arguments nil)
  (last-results +no-result-recorded+))
  
(defmethod print-object ((object folder-history) stream)
  (princ-indent object stream)
  (cond ((folder-history-expanded object)
         (princ "> " stream))
        (t (if (folder-history-entries object)
             (princ ">+" stream)
             (princ ">-" stream))))
  (let ((arguments (folder-history-arguments object)))
           (if (eq arguments +no-arguments+)
             (call-next-method)
             (format stream "(~A ~S~{ ~S~})"
                     (history-entry-name object)
                     (first arguments)
                     (rest arguments))))
  object)

(defstruct (end-folder-entry (:include history-entry))
  (referenced-entry nil))

(defmethod print-object ((object end-folder-entry) stream)
  (princ-indent object stream)
  (princ "< " stream)
  (princ (history-entry-name 
          (end-folder-entry-referenced-entry object)) stream)
  (let ((last-results (folder-history-last-results
                       (end-folder-entry-referenced-entry object))))
    (unless (eq last-results +no-result-recorded+)
      (if (null last-results)
        (princ " []" stream)
        (format stream " [~S~{, ~S~}]"
                (first last-results)
                (rest last-results)))))
  object)

(defstruct (format-history-entry (:include history-entry))
  message
  (arglist-name-value-assoc nil)
  (arglist-name-value-assoc-shown-p nil))

(defun princ-indent2 (object stream)
  (multiple-value-bind (n-times rest)
                       (floor (history-entry-indent-level object) 2)
    (when (> n-times 0)
      (princ "  " stream))
    (loop repeat (- n-times 2)
          do (princ "| " stream))
    (unless (zerop rest)
      (princ " " stream))
    (loop repeat +indent-offset+
          do (princ " " stream))))

(defmethod print-object ((object format-history-entry) stream)
  (princ-indent2 object stream)
  (if (null (history-entry-name object))
    (princ (format-history-entry-message object) stream)
    (call-next-method)))

(defstruct (arglist-history-entry (:include history-entry))
  ;; arg-name -- we reuse the name slot.
  arg-value)

(defconstant +bullet+ (code-char 8226))

(defmethod print-object ((object arglist-history-entry) stream)
  (princ-indent2 object stream)
  (format stream "~A ~A: ~S"
          +bullet+
          (arglist-history-entry-name object)
          (arglist-history-entry-arg-value object)))



;;; **********************************************************************

(defun insert-into-current-folder-entry (current-folder-entry new-entry)
  (cond ((null (folder-history-last-entry current-folder-entry))
         (setf (folder-history-last-entry current-folder-entry) 
               (list new-entry))
         (setf (folder-history-entries current-folder-entry)
               (folder-history-last-entry current-folder-entry)))
        (t (setf (cdr (folder-history-last-entry current-folder-entry))
                 (list new-entry))
           (setf (folder-history-last-entry current-folder-entry)
                 (cdr (folder-history-last-entry current-folder-entry)))))
  (when (find (history-entry-number new-entry) *breakpoints*)
    (unfold-last-entry *trace-history*)
    (if (null *trace-dialogs*)
      (make-trace-dialog)
      (progn
        (setup-tracer-table (tracer-view-named 'tracer-history-table 
                                               (first *trace-dialogs*)))
        (tracer-window-select (first *trace-dialogs*))))
    (if (history-entry-name new-entry)
      (break (format nil "~A" (history-entry-name new-entry)))
      (if (typep new-entry 'format-history-entry)
        (break (format nil "~A" (format-history-entry-message new-entry)))
        (break)))))


(defun new-folder-trace-entry (name arglist-name-value-assoc 
                                    arguments expanded trace-result code)
  (flet ((insert-into-current-folder ()
           (let* ((current-folder-entry *current-folder-entry*)
                  (trace-dialogs *trace-dialogs*)
                  (trace-filter-list (if trace-dialogs
                                         (previous-filter-list (first trace-dialogs))
                                       *trace-manager-previous-filter-list*))
                  (keep-trace-folders *keep-trace-folders*)
                  (old-trace *trace*)
                  (subfilterp (filter-subheaders (first trace-dialogs)))
                  (filterp (when (and trace-filter-list
                                      (or (not keep-trace-folders) subfilterp))
                             (and (filter-elements-foundp
                                   trace-filter-list
                                   (format nil "~S" arguments)
                                   (if (<= (history-entry-indent-level current-folder-entry)
                                           (if trace-dialogs
                                               (level-threshold (first trace-dialogs))
                                             *trace-manager-previous-level-threshold*))
                                       'or
                                     (if trace-dialogs
                                         (filter-logic (first trace-dialogs))
                                       *trace-manager-previous-filter-logic*)))
                                  t)))
                  (new-trace (if trace-filter-list
                                 (or keep-trace-folders filterp)
                               old-trace))
                  (*keep-trace-folders* (or keep-trace-folders filterp)))
             (if new-trace
                 (let ((*trace* (if (and trace-dialogs subfilterp)
                                    (or (null trace-filter-list) filterp)
                                  new-trace)))
                   (if *trace*
                       (let* ((new-entry
                               (make-folder-history :name name
                                                    :arglist-name-value-assoc arglist-name-value-assoc
                                                    :arguments arguments 
                                                    :expanded expanded
                                                    :indent-level (1+ (history-entry-indent-level
                                                                       current-folder-entry))))
                              (*current-folder-entry* new-entry))
                         (insert-into-current-folder-entry current-folder-entry new-entry)
                         (if trace-result
                             (let ((results (multiple-value-list (funcall code))))
                               (setf (folder-history-last-results new-entry) results)
                               (values-list results))
                           (funcall code)))
                     (let ((*keep-trace-folders* nil))
                       (funcall code))))
               (let ((*trace* nil))
                 (funcall code))))))
    (let ((trace-dialogs *trace-dialogs*))
      (if (and trace-dialogs (boundp '*current-folder-entry*))
          (insert-into-current-folder)
        (if (null trace-dialogs)
            (if *ignore-tracing*
                (insert-into-current-folder)
              (progn
                (make-trace-dialog)
                (multiple-value-prog1
                    (let ((trace-dialog (first *trace-dialogs*)))
                      (when *trace*
                        (setf *trace-history* (traced-folder trace-dialog)))
                      (let ((*current-folder-entry* *trace-history*))
                        (insert-into-current-folder)))
                  (when *trace*
                    (let ((trace-dialog (first *trace-dialogs*)))
                      (setup-tracer-table (tracer-view-named 'tracer-history-table
                                                             trace-dialog))
                      (tracer-window-select trace-dialog))))))
          (let ((*current-folder-entry* (traced-folder (first trace-dialogs))))
            (multiple-value-prog1
                (insert-into-current-folder)
              (setup-tracer-table (tracer-view-named 'tracer-history-table
                                                     (first trace-dialogs)))
              (tracer-window-select (first trace-dialogs)))))))))

(defmacro with-trace-context ((name 
                               &key 
                               (expanded '(or (zerop *current-trace-level*)
                                           (not (zerop (mod *current-trace-level* 10)))))
                               (arguments +no-arguments+)
                               (arglist-name-value-assoc nil)
                               trace-result) &body forms)
  `(if *ignore-tracing*
     (progn .,forms)
     (new-folder-trace-entry ,name 
                             ,arglist-name-value-assoc
                             ,arguments
                             ,expanded
                             ,trace-result
                             (lambda () .,forms))))

(defun traced-format (name format-string format-args arglist-name-value-assoc)
  (when (and *trace* (boundp '*current-folder-entry*))
    (let* ((*print-readably* nil)
           (current-folder-entry *current-folder-entry*)
           (message (nsubstitute #\space #\newline (apply #'format nil format-string format-args)))
           (trace-dialogs *trace-dialogs*)
           (trace-filter-list (if trace-dialogs
                                  (previous-filter-list (first trace-dialogs))
                                *trace-manager-previous-filter-list*)))
      (if (and trace-filter-list
               trace-dialogs
               (or (not (headers-only (first trace-dialogs)))
                   (> (history-entry-indent-level current-folder-entry)
                      (if trace-dialogs
                          (level-threshold (first trace-dialogs))
                        *trace-manager-previous-level-threshold*))))
          (when (filter-elements-foundp trace-filter-list
                                        message
                                        (if trace-dialogs
                                            (filter-logic (first trace-dialogs))
                                          *trace-manager-previous-filter-logic*))
            ;(break "~S" format-string)
            (insert-into-current-folder-entry
             current-folder-entry
             (make-format-history-entry :name name
                                        :message message
                                        :arglist-name-value-assoc arglist-name-value-assoc
                                        :indent-level (1+ (history-entry-indent-level
                                                           current-folder-entry)))))
        (progn
          ;(break "~S" format-string)
          (insert-into-current-folder-entry
           current-folder-entry
           (make-format-history-entry :name name
                                      :message message
                                      :arglist-name-value-assoc arglist-name-value-assoc
                                      :indent-level (1+ (history-entry-indent-level
                                                         current-folder-entry)))))))))

(defmacro formatting ((&optional (name nil) &key (arglist-name-value-assoc nil))
                      format-string &rest format-args)
  `(unless *ignore-tracing*
     (traced-format ,name ',format-string (list . ,format-args) ,arglist-name-value-assoc)))

(defun filter-elements-foundp (filter-list text filter-logic)
  (if filter-list
      (if (eq filter-logic 'or)
          (loop for filter in filter-list
                thereis (search filter text :test 'char-equal))
        (loop for filter in filter-list
              always (search filter text :test 'char-equal)))
    t))


;;;
;;;; Browser sequence table
;;;

(defclass tracer-history-table
  (tracer-sequence-dialog-item)
  ())

        
;;;
;;;; Tracer Fred view
;;;

(defclass tracer-fred-dialog-item
  (tracer-scrolling-fred-view tracer-dialog-item)
  ())

        
;;;
;;;; Trace dialog
;;; 

(defclass tracer-trace-menu-item
  (tracer-menu-item)
  ((enabling-entry-classes :initform nil 
                           :initarg :enabling-entry-classes
                           :accessor enabling-entry-classes)))

(defclass trace-dialog 
  (tracer-dialog)
  ((trace-folder :initarg :traced-folder
                 :accessor traced-folder)
   (previous-search-string :initform *trace-manager-previous-search-string*
                           :accessor previous-search-string)
   (search-with-automatic-unfold :initform nil
                                 :accessor search-with-automatic-unfold)
   (n-of-entry-to-be-found :initform 1
                           :accessor n-of-entries-to-be-found)
   (previous-filter-list :initform *trace-manager-previous-filter-list*
                         :accessor previous-filter-list)
   (headers-only :initform *trace-manager-previous-filter-headers* :accessor headers-only)
   (filter-subheaders :initform *trace-manager-previous-subfilter* :accessor filter-subheaders)
   (filter-logic :initform *trace-manager-previous-filter-logic* :accessor filter-logic)
   (level-threshold :initform *trace-manager-previous-level-threshold* :accessor level-threshold)))

(defconstant +meta-dot-char+ (code-char 179))

(defun compute-new-pos (selected-item selected-cell sequence char)
  (declare (ignore selected-item selected-cell sequence char))
  )

#+(and :lispworks (or :macosx :mswindows :linux))
(defmethod tracer-view-container ((view capi:push-button-panel))
  (capi:element-interface view))

(defun find-start-folder (end-folder sequence)
  (let ((result (find-start-folder-1 end-folder sequence)))
    (if result
      (- (length sequence) result 1)
      result)))

(defun find-start-folder-1 (end-folder sequence)
  (cond ((not (eq (type-of end-folder) 'end-folder-entry))
         (find-start-folder end-folder (rest sequence)))
        ((null sequence)
         nil)
        ((eq (first sequence) (end-folder-entry-referenced-entry end-folder))
         0)
        (t (+ (find-start-folder-1 end-folder (rest sequence)) 1))))

(defun find-end-folder (start-folder sequence)
  (cond ((not (eq (type-of start-folder) 'folder-history))
         0)
        ((null sequence)
         nil)
        ((and (eq (type-of (first sequence)) 'end-folder-entry)
              (eq (end-folder-entry-referenced-entry (first sequence)) start-folder))
         0)
        (t (+ (find-end-folder start-folder (rest sequence)) 1))))

(defun find-a-folder (selected-item sequence)
  (cond ((null sequence)
         nil)
        ((eq (first sequence) selected-item)
         (find-a-folder-1 (rest sequence)))
        (t (find-a-folder selected-item (rest sequence)))))

(defun find-a-folder-1 (sequence)
  (cond ((null sequence)
         0)
        ((eq (type-of (first sequence)) 'folder-history)
         1)
        (t (+ 1 (find-a-folder-1 (rest sequence))))))

#+(and :lispworks (or :macosx :mswindows :linux))
(defclass tracer-text-input-pane (capi:text-input-pane)
  ())

#+(and :lispworks (or :macosx :mswindows :linux))
(defun make-trace-dialog (&key (input-folder *trace-history*) (window-title "Trace"))
  (declare (special racer::*racer-trace*))
  (when (null input-folder)
    (setf *trace-history* (make-folder-history :name "Top"
                                               :arguments +no-arguments+
                                               :expanded t))
    (setf input-folder *trace-history*))
  (setf *entry-number* 0)
  (let* ((history (let ((*indent-level* 0))
                    (traverse-and-collect-expanded-entries input-folder)))
         (trace-table
          (make-instance 'tracer-history-table 
                         :items history
                         :view-nick-name 'trace-table
                         :font (gp:make-font-description :family *trace-text-font* 
                                                         :size *trace-text-font-size*
                                                         :weight :normal
                                                         ;:slant :roman
                                                         )
                         :interaction ':single-selection
                         :test-function (lambda (item1 item2)
                                          (eql (history-entry-number item1)
                                               (history-entry-number item2)))
                         :action-callback (lambda (element dialog)
                                            (case (type-of element)
                                              ((folder-history end-folder-entry)
                                               (expand-collapse-trace-entry dialog)
                                               (refresh-interface dialog))
                                              ((format-history-entry arglist-history-entry)
                                               (inspect-trace-entry dialog))))
                         ;:keep-selection-p t
                         :visible-min-width 400
                         :visible-min-height 200
                         ))
         (button-font (gp:make-font-description :family "Helvetica" 
                                                :size 10
                                                :weight :bold
                                                ;:slant :roman
                                                ))
         (trace-option-pane
          (make-instance 'capi:option-pane
                         :items '(on off)
                         :font button-font
                         :print-function (lambda (item)
                                           (case item
                                             (on "Racer Trace ON")
                                             (off "Racer Trace OFF")))
                         :selected-item (if (and (boundp 'racer::*racer-trace*) racer::*racer-trace*)
                                            'on
                                          'off)
                         :help-key 'trace-racer
                         :selection-callback 
                         (lambda (choice interface)
                           (declare (ignore interface))
                           (if (eq choice 'on)
                               (racer:trace-racer)
                             (racer:untrace-racer)))))
         (trace-dialog
          (make-instance 'trace-dialog
                         :title window-title
                         :font button-font
                         :best-x (first *trace-manager-window-position*)
                         :best-y (second *trace-manager-window-position*)
                         :best-width (first *trace-manager-window-size*)
                         :best-height (second *trace-manager-window-size*)
                         :traced-folder input-folder
                         :help-callback 'tracer-buttons-help-callback
                         :destroy-callback
                         (lambda (interface)
                           (setf *trace-history* nil)
                           (setf *entry-number* 0)
                           #-:linux
                           (multiple-value-bind (x y w h)
                               (capi:top-level-interface-geometry interface)
                             (setf *trace-manager-window-position* (tracer-make-point x y))
                             (setf *trace-manager-window-size* (tracer-make-point w h)))
                           (setf *trace-dialogs* (delete interface *trace-dialogs*)))
                         :activate-callback
                         (lambda (interface activatep)
                           (when activatep
                             (setf (capi:choice-selected-item trace-option-pane)
                                   (if (and (boundp 'racer::*racer-trace*) racer::*racer-trace*)
                                       'on
                                     'off))
                             (refresh-interface interface nil)))))
         (command-panel
          (make-instance
           'capi:push-button-panel
           :items
           (list (make-instance 'capi:push-button
                                :text "New"
                                :font button-font
                                :data 'new-tracer-window
                                :help-key 'new-tracer-window
                                :callback (lambda (data interface)
                                            (declare (ignore data interface))
                                            (capi:display
                                             (make-trace-dialog :input-folder nil))))
                 (make-instance 'capi:push-button
                                :text "Untrace All"
                                :font button-font
                                :data 'untrace-all
                                :help-key 'untrace-all
                                :callback (lambda (data interface)
                                            (declare (ignore data interface))
                                            (unctrace-function)))
                 (make-instance 'capi:push-button
                                :text "Inspect"
                                :font button-font
                                :data 'inspect
                                :help-key 'inspect
                                :callback (lambda (data trace-dialog)
                                            (declare (ignore data interface))
                                            (inspect-trace-entry trace-dialog)))
                 (make-instance 'capi:push-button
                                :text "(Un)Fold"
                                :font button-font
                                :help-key 'expand-collapse
                                :data 'expand-collapse
                                :callback (lambda (data interface)
                                            (declare (ignore data interface))
                                            (expand-collapse-trace-entry trace-dialog)
                                            (refresh-interface trace-dialog)))
                 (make-instance 'capi:push-button
                                :text "Args"
                                :font button-font
                                :data 'show-hide-args
                                :help-key 'show-hide-args
                                :callback (lambda (data interface)
                                            (declare (ignore data interface))
                                            (toggle-argument-display trace-dialog)))
                 (make-instance 'capi:push-button
                                :text "Find"
                                :font button-font
                                :data 'find
                                :help-key 'find
                                :callback (lambda (data interface)
                                            (declare (ignore data interface))
                                            (find-entry trace-dialog)))
                 (make-instance 'capi:check-button
                                :text "Unfold"
                                :help-key 'unfolding
                                :selected (search-with-automatic-unfold trace-dialog)
                                :selection-callback
                                (lambda (data interface)
                                  (declare (ignore data interface))
                                  (setf (search-with-automatic-unfold trace-dialog) t))
                                :retract-callback
                                (lambda (data interface)
                                  (declare (ignore data interface))
                                  (setf (search-with-automatic-unfold trace-dialog) nil)))))))
    (flet ((filter-callback (pane type)
             (case type
               (:start
                (setf (previous-filter-list trace-dialog)
                      (delete (capi:text-input-pane-text pane)
                              (previous-filter-list trace-dialog)
                              :test 'equal)))
               (:end
                (pushnew (capi:text-input-pane-text pane)
                         (previous-filter-list trace-dialog)
                         :test 'equal)))
             (setf (previous-filter-list trace-dialog)
                   (delete "" (previous-filter-list trace-dialog) :test 'equal))
             (setf *trace-manager-previous-filter-list*
                   (previous-filter-list trace-dialog))))
      (let ((filter-panel
             (make-instance
              'capi:row-layout
              :font button-font
              :ratios '(nil 1 1 1 nil nil nil)
              :description
              (list
               (make-instance 'capi:option-pane
                              :items '(or and)
                              :print-function (lambda (item)
                                                (case item
                                                  (or "OR Keywords")
                                                  (and "AND Keywords")))
                              :help-key 'filter-logic
                              :selected-item (filter-logic trace-dialog)
                              :selection-callback 
                              (lambda (choice interface)
                                (setf (filter-logic interface) choice)
                                (setf *trace-manager-previous-filter-logic* choice)))
               (make-instance 'tracer-text-input-pane
                             ;:font button-font
                              :help-key 'filter-panel
                              :text (or (first (previous-filter-list trace-dialog)) "")
                              :editing-callback #'filter-callback)
               (make-instance 'tracer-text-input-pane
                             ;:font button-font
                              :help-key 'filter-panel
                              :text (or (second (previous-filter-list trace-dialog)) "")
                              :editing-callback #'filter-callback)
               (make-instance 'tracer-text-input-pane
                             ;:font button-font
                              :help-key 'filter-panel
                              :text (or (third (previous-filter-list trace-dialog)) "")
                              :editing-callback #'filter-callback)
               (make-instance 'capi:check-button
                              :text "Headers"
                              :help-key 'headers-only
                              :selected *trace-manager-previous-filter-headers*
                              :selection-callback
                              (lambda (data interface)
                                (declare (ignore data))
                                (setf (headers-only interface) t)
                                (setf *trace-manager-previous-filter-headers* t))
                              :retract-callback
                              (lambda (data interface)
                                (declare (ignore data))
                                (setf (headers-only interface) nil)
                                (setf *trace-manager-previous-filter-headers* nil)))
               (make-instance 'capi:check-button
                              :text "Subfilter"
                              :help-key 'filter-subheaders
                              :selected *trace-manager-previous-subfilter*
                              :selection-callback
                              (lambda (data interface)
                                (declare (ignore data))
                                (setf (filter-subheaders interface) t)
                                (setf *trace-manager-previous-subfilter* t))
                              :retract-callback
                              (lambda (data interface)
                                (declare (ignore data))
                                (setf (filter-subheaders interface) nil)
                                (setf *trace-manager-previous-subfilter* nil)))
               (make-instance 'capi:option-pane
                              :items '(0 1 2 3 4 5 6 7 8 9)
                              :help-key 'level-threshold
                              :selected-item *trace-manager-previous-level-threshold*
                              :selection-callback 
                              (lambda (choice interface)
                                (setf (level-threshold interface) choice)
                                (setf *trace-manager-previous-level-threshold* choice))))
              :adjust :center))
            (menu-items
             (list
              (make-instance 'tracer-trace-menu-item
                             :mnemonic-title "Refresh"
                             :accelerator #\r
                             :callback  (lambda (&rest args)
                                          (declare (ignore args))
                                          (refresh-interface trace-dialog)))
              (make-instance 'tracer-trace-menu-item
                             :mnemonic-title "Increse font size"
                             :accelerator #\+
                             :callback  (lambda (&rest args)
                                          (declare (ignore args))
                                          (incf *trace-text-font-size*)))
              (make-instance 'tracer-trace-menu-item
                             :mnemonic-title "Decrese font size"
                             :accelerator #\-
                             :callback  (lambda (&rest args)
                                          (declare (ignore args))
                                          (decf *trace-text-font-size*)))
              (make-instance 'tracer-trace-menu-item
                             :mnemonic-title "Inspect"
                             :enabling-entry-classes '(folder-history end-folder-entry
                                                                      arglist-history-entry)
                             :accelerator #\i
                             :callback (lambda (&rest args)
                                         (declare (ignore args))
                                         (inspect-trace-entry trace-dialog))
                             )
              (make-instance 'tracer-trace-menu-item
                             :mnemonic-title "Definition"
                             :enabling-entry-classes '(folder-history end-folder-entry)
                             :accelerator #\d
                             :callback (lambda (&rest args)
                                         (declare (ignore args))
                                         (edit-definition-trace-entry trace-dialog))
                             )
              (make-instance 'capi:menu-component 
                             :items
                             (list
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Trace"
                                             :enabling-entry-classes '(folder-history end-folder-entry)
                                             :accelerator "accelerator-shift-t"
                                             :callback (lambda (&rest args)
                                                         (declare (ignore args))
                                                         (trace-trace-entry trace-dialog))
                                             )
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Untrace"
                                             :enabling-entry-classes '(folder-history end-folder-entry)
                                             :accelerator "accelerator-shift-u"
                                             :callback (lambda (&rest args)
                                                         (declare (ignore args))
                                                         (untrace-trace-entry trace-dialog))
                                             )
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Trace Setf"
                                             :enabling-entry-classes '(folder-history end-folder-entry)
                                             :accelerator "accelerator-meta-t"
                                             :callback (lambda (&rest args)
                                                         (declare (ignore args))
                                                         (trace-trace-entry trace-dialog))
                                             )
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Untrace Setf"
                                             :enabling-entry-classes '(folder-history end-folder-entry)
                                             :accelerator "accelerator-meta-u"
                                             :callback (lambda (&rest args)
                                                         (declare (ignore args))
                                                         (untrace-trace-entry trace-dialog))
                                             )
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Untrace All"
                                             :enabling-entry-classes 't
                                             :callback (lambda (&rest args)
                                                         (declare (ignore args))
                                                         (unctrace-function))
                                             )
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Print Traced"
                                             :enabling-entry-classes 't
                                             :accelerator #\p
                                             :callback
                                             (lambda (&rest args)
                                               (declare (ignore args))
                                               (let ((functions
                                                      (loop for fn in *ctraced-functions*
                                                            if (setf-fn-p fn)
                                                            collect (format nil "(setf ~S)" (fn-of-setf-fn fn))
                                                            else
                                                            collect (format nil "~S" fn))))
                                                 (capi:display
                                                  (make-instance 'capi:interface
                                                                 :title (format nil "Traced Functions: ~D"
                                                                                (length *ctraced-functions*))
                                                                 :layout
                                                                 (make-instance 'capi:simple-layout
                                                                                :description
                                                                                (list 
                                                                                 (make-instance 'capi:display-pane
                                                                                                :text functions)))))))
                                             )))
              (make-instance 'capi:menu-component 
                             :items
                             (list
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Break"
                                             :enabling-entry-classes '(folder-history format-history-entry 
                                                                                      end-folder-entry)
                                             :callback (lambda (&rest args)
                                                         (declare (ignore args))
                                                         (break-trace-entry trace-dialog)))
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Unbreak"
                                             :enabling-entry-classes '(folder-history format-history-entry
                                                                                      end-folder-entry)
                                             :callback (lambda (&rest args)
                                                         (declare (ignore args))
                                                         (unbreak-trace-entry trace-dialog)))
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Unbreak All"
                                             :enabling-entry-classes 't
                                             :callback (lambda (&rest args)
                                                         (declare (ignore args))
                                                         (unbreak-all-trace-entries trace-dialog)))))
              (make-instance 'capi:menu-component 
                             :items
                             (list
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Expand/Collapse"
                                             :enabling-entry-classes '(folder-history)
                                             :accelerator "Accelerator-Right"
                                             :callback (lambda (&rest args)
                                                         (declare (ignore args))
                                                         (expand-collapse-trace-entry trace-dialog)
                                                         (refresh-interface trace-dialog))
                                             )
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Fully Expand/Collapse"
                                             :enabling-entry-classes '(folder-history)
                                             :accelerator "Accelerator-Meta-Right"
                                             :callback (lambda (&rest args)
                                                         (declare (ignore args))
                                                         (expand-collapse-trace-entry trace-dialog t)
                                                         (refresh-interface trace-dialog))
                                             )
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Show/Hide Arguments"
                                             :enabling-entry-classes '(folder-history format-history-entry)
                                             :accelerator #\a
                                             :callback (lambda (&rest args)
                                                         (declare (ignore args))
                                                         (toggle-argument-display trace-dialog))
                                             )))
              (make-instance 'capi:menu-component 
                             :items
                             (list
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Find..."
                                             :enabling-entry-classes 't
                                             :accelerator #\f
                                             :callback (lambda (&rest args)
                                                         (declare (ignore args))
                                                         (find-entry trace-dialog))
                                             )
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Find Next"
                                             :enabling-entry-classes 't
                                             :accelerator #\g
                                             :callback (lambda (&rest args)
                                                         (declare (ignore args))
                                                         (find-next-entry trace-dialog))
                                             )
                              (make-instance 'tracer-trace-menu-item
                                             :mnemonic-title "Find Previous"
                                             :enabling-entry-classes 't
                                             :accelerator #\b
                                             :callback (lambda (&rest args)
                                                         (declare (ignore args))
                                                         (find-previous-entry trace-dialog))
                                             )))
              (make-instance 'tracer-trace-menu-item
                             :mnemonic-title "New Trace Window"
                             :enabling-entry-classes 't
                             :accelerator "accelerator-n"
                             :callback (lambda (&rest args)
                                         (declare (ignore args))
                                         (setf *trace-dialogs* (delete trace-dialog *trace-dialogs*))
                                         (capi:display (make-trace-dialog :input-folder nil))
                                         )
                             ))))
        (setf (capi:interface-menu-bar-items trace-dialog)
              (list
               (make-instance 'tracer-pull-down-menu
                              :mnemonic-title "Edit"
                              :view-nick-name 'edit-menu
                              :items
                              (list
                               (make-instance 'tracer-trace-menu-item
                                              :mnemonic-title "Cut"
                                              :accelerator #\x
                                              :callback-type '(:focus)
                                              :callback  (lambda (pane)
                                                           (filter-callback pane ':start)
                                                           (cut-copy-paste-action pane 'cut)
                                                           (filter-callback pane ':end)))
                               (make-instance 'tracer-trace-menu-item
                                              :mnemonic-title "Copy"
                                              :accelerator #\c
                                              :callback-type '(:focus)
                                              :callback  (lambda (pane)
                                                           (cut-copy-paste-action pane 'copy)))
                               (make-instance 'tracer-trace-menu-item
                                              :mnemonic-title "Paste"
                                              :accelerator #\v
                                              :callback-type '(:focus)
                                              :callback  (lambda (pane)
                                                           (filter-callback pane ':start)
                                                           (cut-copy-paste-action pane 'paste)
                                                           (filter-callback pane ':end)))))
               (make-instance 'tracer-pull-down-menu
                              :mnemonic-title "Commands"
                              :view-nick-name 'command-menu
                              :items menu-items)))
        (setf (capi:pane-layout trace-dialog)
              (make-instance 'capi:column-layout
                             :description (list (make-instance 'capi:row-layout
                                                               :description (list trace-option-pane
                                                                                  command-panel)
                                                               :uniform-size-p nil
                                                               :ratios '(nil nil)
                                                               :adjust :center)
                                                filter-panel
                                                trace-table)))
        (push trace-dialog *trace-dialogs*)
        trace-dialog
        ))))

#+(and :lispworks (or :macosx :mswindows :linux))
(defmethod cut-copy-paste-action ((pane t) action)
  nil)

#+(and :lispworks (not :lispworks4) (or :macosx :mswindows :linux))
(defmethod cut-copy-paste-action ((pane tracer-text-input-pane) action)
  (ecase action
    (cut (capi:text-input-pane-cut pane))
    (copy (capi:text-input-pane-copy pane))
    (paste (capi:text-input-pane-paste pane))))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun refresh-interface (interface &optional (recreate-table t))
  (let* ((table (tracer-view-named 'tracer-history-table interface))
         (selected-entry (first (get-selected-items table))))
    (when recreate-table
      (setup-tracer-table interface))
    (when selected-entry
      (tracer-cell-select table selected-entry))
    (capi:redisplay-interface interface)))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun tracer-buttons-help-callback (interface pane type key)
  (declare (ignore interface pane))
  (when (eq type :tooltip)
    (case key
      (trace-racer
         "Switch Racer Tracing on or off. The current selection is displayed.")
      (untrace-all "Untrace all Racer traced functions. Use in the editor control-c control-t to trace a function in Racer Tracer and control-c control-u to untrace it.")
      (inspect "Inspect the arguments/values of the selected trace entry")
      (expand-collapse "Unfold/Collapse trace entry")
      (show-hide-args "Show/Hide arguments of trace entry")
      (find "Find trace entry containing a string (use accelerator-G to search forward and accelerator-B to search backwards. The search ignores differences in case. The check box \"Unfold\" controls whether the search automatically unfolds collapsed trace entries.")
      (unfolding "If checked the (incremental) search automatically unfolds folded trace entries. This can be very slow if unfolded trace entries contain many elements. If unchecked the incremental search is automatically wrapped and starts from the selected element.")
      (new-tracer-window "Create a new empty trace window that will be used for follow-up traces")
      (filter-logic "Select AND/OR as combination function for filtering")
      (filter-panel "Only trace info elements containing any or all of the entered filter keywords will be recorded. The word search ignores differences in case.")
      (headers-only "If checked the filter words apply only to the headers of unfoldable trace entries. The bodies of the selcted entries are completely recorded.")
      (filter-subheaders "If checked folded trace info elements are only recorded if they match the filter condition.")
      )))

(defun inspect-trace-entry (trace-dialog)
  (let* ((table (tracer-view-named 'tracer-history-table trace-dialog))
         (selected-entry (first (get-selected-items table))))
    (when selected-entry
      (tracer-inspect-action table selected-entry))))

(defun edit-definition-trace-entry (trace-dialog)
  (let* ((table (tracer-view-named 'tracer-history-table trace-dialog))
         (selected-entry (first (get-selected-items table))))
    (when selected-entry
      (tracer-edit-definition-action table selected-entry))))

(defun trace-trace-entry (trace-dialog)
  (let* ((table (tracer-view-named 'tracer-history-table trace-dialog))
         (selected-entry (first (get-selected-items table))))
    (when selected-entry
      (tracer-trace-action table selected-entry))))

(defun untrace-trace-entry (trace-dialog)
  (let* ((table (tracer-view-named 'tracer-history-table trace-dialog))
         (selected-entry (first (get-selected-items table))))
    (when selected-entry
      (tracer-untrace-action table selected-entry))))

(defun trace-setf-trace-entry (trace-dialog)
  (let* ((table (tracer-view-named 'tracer-history-table trace-dialog))
         (selected-entry (first (get-selected-items table))))
    (when selected-entry
      (tracer-setf-trace-action table selected-entry))))

(defun untrace-setf-trace-entry (trace-dialog)
  (let* ((table (tracer-view-named 'tracer-history-table trace-dialog))
         (selected-entry (first (get-selected-items table))))
    (when selected-entry
      (tracer-setf-untrace-action table selected-entry))))

(defun break-trace-entry (trace-dialog)
  (let* ((table (tracer-view-named 'tracer-history-table trace-dialog))
         (selected-entry (first (get-selected-items table))))
    (when selected-entry
      (tracer-break-action table selected-entry))))

(defun unbreak-trace-entry (trace-dialog)
  (let* ((table (tracer-view-named 'tracer-history-table trace-dialog))
         (selected-entry (first (get-selected-items table))))
    (when selected-entry
      (tracer-unbreak-action table selected-entry))))

(defun unbreak-all-trace-entries (trace-dialog)
  (let* ((table (tracer-view-named 'tracer-history-table trace-dialog)))
    (tracer-unbreak-all-action table nil)))

(defun expand-collapse-trace-entry (trace-dialog &optional (full nil))
  (let* ((table (tracer-view-named 'tracer-history-table trace-dialog))
         (selected-entry (first (get-selected-items table))))
    (when selected-entry
      (if (or full (tracer-option-key-p))
        (tracer-fully-expand-collapse-action table selected-entry)
        (tracer-expand-collapse-action table selected-entry)))))

(defun unfold-all-entries (entries unfolded-p)
  (loop for entry in entries do
        (when (typep entry 'folder-history)
          (setf (folder-history-expanded entry) unfolded-p)
          (unfold-all-entries (folder-history-entries entry) unfolded-p))))

(defun tracer-table-entry-action (table)
  (let ((selected-items (get-selected-items table)))
    (when selected-items 
      (tracer-entry-action table (first selected-items)
                           :double-click-p (tracer-double-click-p)
                           :control-key-p (tracer-control-key-p)
                           :option-key-p (tracer-option-key-p)))
    (let ((command-menu (tracer-view-named 'command-menu (tracer-view-container table))))
      (loop for menu-item in (tracer-menu-items command-menu) do
            (if (or (eq (enabling-entry-classes menu-item) 't) 
                    (find (type-of (first selected-items)) (enabling-entry-classes menu-item)))
              (tracer-menu-item-enable menu-item)
              (tracer-menu-item-disable menu-item))))))

(defun toggle-argument-display (trace-dialog)
  (let* ((table (tracer-view-named 'tracer-history-table trace-dialog))
         (selected-entry (first (get-selected-items table))))
    (cond ((eq (type-of selected-entry) 'folder-history)
           (setf (folder-history-arglist-name-value-assoc-shown-p selected-entry)
                 (not (folder-history-arglist-name-value-assoc-shown-p selected-entry)))
           (setup-tracer-table table))
          ((eq (type-of selected-entry) 'format-history-entry)
           (setf (format-history-entry-arglist-name-value-assoc-shown-p selected-entry)
                 (not (format-history-entry-arglist-name-value-assoc-shown-p selected-entry)))
           (setup-tracer-table table)))
    #+(and :lispworks (or :macosx :mswindows :linux))
    (tracer-cell-select table selected-entry)))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun copy-selected-items-to-clip-board (trace-dialog)
  (let* ((table (tracer-view-named 'tracer-history-table trace-dialog))
         (entries (copy-entries (get-selected-items table))))
    (capi:set-clipboard trace-dialog
                        entries
                        (with-output-to-string (stream)
                          (loop for entry in entries
                                do (princ entry stream))))
    nil))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun copy-table-to-clip-board (trace-dialog)
  (let* ((table (tracer-view-named 'tracer-history-table trace-dialog))
         (entries (copy-entries (tracer-table-sequence table))))
    (capi:set-clipboard trace-dialog
                        entries
                        (with-output-to-string (stream)
                          (loop for entry in entries
                                do
                                (princ entry stream)
                                (terpri stream))))
    nil))

(defun copy-entries (entries)
  (if (listp entries)
      (loop for entry in entries
            collect (princ-to-string entry)
            when (folder-history-p entry)
            nconc (copy-entries (folder-history-entries entry)))
    (loop for entry across entries
          collect (princ-to-string entry)
          when (folder-history-p entry)
          nconc (copy-entries (folder-history-entries entry)))))

(defun find-entry (trace-dialog)
  (catch :cancel
    (let* ((automatic-unfold (search-with-automatic-unfold trace-dialog))
           (table (tracer-view-named 'tracer-history-table trace-dialog))
           (previous-search-string (previous-search-string trace-dialog)))
      (multiple-value-bind (search-string not-cancelled-p)
          (tracer-get-string-from-user (if automatic-unfold
                                           "Search with unfolding for..."
                                           "Search without unfolding for...")
                                       :initial-string previous-search-string
                                       :window-title "Search for...")
        (when not-cancelled-p
          (setf search-string (string-downcase search-string))
          (let* ((top-folder (traced-folder trace-dialog))
                 (found (if automatic-unfold
                            (find-entry-1 search-string (list top-folder))
                          #+:lispworks (capi:find-string-in-collection table search-string t))))
            (setf (previous-search-string trace-dialog) search-string)
            (setf *trace-manager-previous-search-string* search-string)
            (if found
                (when automatic-unfold
                  (setf (n-of-entries-to-be-found trace-dialog) 1)
                  (setf (folder-history-expanded top-folder) t)
                  (setup-tracer-table table)
                  (let ((cell (tracer-index-to-cell table
                                                    (position found
                                                              (tracer-table-sequence table))))
                        (selected-cell (first (tracer-selected-cells table))))
                    (when selected-cell
                      (tracer-cell-deselect table selected-cell))
                    (tracer-cell-select table cell)
                    (tracer-scroll-to-cell 
                     table 
                     cell)))
              (tracer-ed-beep))))))))

(defvar *n-of-found-entries* nil)

(defun find-entry-1 (search-string entries &optional n-entries)
  (loop for entry in entries do
        (when (search search-string (princ-to-string entry) :test 'char-equal)
          (if (or (null n-entries)
                  (= n-entries (+ 1 *n-of-found-entries*)))
            (return-from find-entry-1 entry)
            (incf *n-of-found-entries*)))
        (when (typep entry 'folder-history)
          (let ((result (find-entry-1 search-string 
                                      (folder-history-entries entry)
                                      n-entries)))
            (when result
              (setf (folder-history-expanded entry) t)
              (return-from find-entry-1 result))))))

(defun find-entry-2 (trace-dialog)
  (let* ((automatic-unfold (search-with-automatic-unfold trace-dialog))
         (table (tracer-view-named 'tracer-history-table trace-dialog))
         (search-string (previous-search-string trace-dialog)))
    (if (null search-string)
        (tracer-ed-beep)
      (let* ((top-folder (traced-folder trace-dialog))
             (*n-of-found-entries* 0)
             (found (if automatic-unfold
                        (find-entry-1 search-string 
                                      (list top-folder)
                                      (n-of-entries-to-be-found trace-dialog))
                      #+:lispworks (capi:find-string-in-collection table search-string t))))
        (setf search-string (string-downcase search-string))
        (if found 
            (when automatic-unfold
              (setf (folder-history-expanded top-folder) t)
              (setup-tracer-table table)
              (let ((cell (tracer-index-to-cell table
                                                (position found
                                                          (tracer-table-sequence table)))))
                (tracer-cell-deselect table (first (tracer-selected-cells table)))
                (tracer-cell-select table cell)
                (tracer-scroll-to-cell 
                 table 
                 cell))
              t)
          (progn
            (tracer-ed-beep)
            nil))))))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun find-next-entry (trace-dialog)
  (incf (n-of-entries-to-be-found trace-dialog))
  (let ((result (find-entry-2 trace-dialog)))
    (unless result (decf (n-of-entries-to-be-found trace-dialog)))
    result))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun find-previous-entry (trace-dialog)
  (decf (n-of-entries-to-be-found trace-dialog))
  (let ((result (find-entry-2 trace-dialog)))
    (unless result (incf (n-of-entries-to-be-found trace-dialog)))
    result))

(defmethod unfold-last-entry ((trace-dialog trace-dialog))
  (unfold-last-entry (traced-folder trace-dialog)))

(defmethod unfold-last-entry ((folder folder-history)) 
  (setf (folder-history-expanded folder) t)
  (unfold-last-entry (first (last (folder-history-entries folder)))))

(defmethod unfold-last-entry ((folder t))
  nil)
    


;;; **********************************************************************

(defmethod tracer-entry-action ((table tracer-history-table)
                                (folder folder-history)
                                &key control-key-p option-key-p double-click-p)
  (cond ((and control-key-p option-key-p)
         (tracer-edit-definition (history-entry-name folder)))
        (control-key-p
         (let ((arguments (folder-history-arguments folder)))
           (unless (eq arguments +no-arguments+)
             (inspect arguments))))
        ((and option-key-p (not double-click-p))
         (let ((selected-entry (first (get-selected-items table))))
           (setf (folder-history-arglist-name-value-assoc-shown-p selected-entry)
                 (not (folder-history-arglist-name-value-assoc-shown-p selected-entry))
                 )
           (setup-tracer-table table)
           (tracer-redraw-cell table (first (tracer-selected-cells table)))))
        (double-click-p
         (if option-key-p
           (tracer-fully-expand-collapse-action table folder)
           (progn
             (setf (folder-history-expanded folder)
                   (not (folder-history-expanded folder)))
             (setup-tracer-table table)
             (tracer-redraw-cell table (first (tracer-selected-cells table))))))))

(defmethod tracer-inspect-action ((table tracer-history-table)
                                  (folder folder-history))
  (let ((arguments (folder-history-arguments folder)))
    (unless (eq arguments +no-arguments+)
      (inspect (folder-history-arglist-name-value-assoc folder)))))

(defmethod tracer-edit-definition-action ((table tracer-history-table)
                                          (folder folder-history))
  (tracer-edit-definition (history-entry-name folder)))

(defmethod tracer-trace-action ((table tracer-history-table)
                                (folder folder-history))
  (ctrace-function (history-entry-name folder)))

(defmethod tracer-untrace-action ((table tracer-history-table)
                                  (folder folder-history))
  (unctrace-function (history-entry-name folder)))

(defmethod tracer-setf-trace-action ((table tracer-history-table)
                                     (folder folder-history))
  (ctrace-setf-function (history-entry-name folder)))

(defmethod tracer-setf-untrace-action ((table tracer-history-table)
                                       (folder folder-history))
  (unctrace-setf-function (history-entry-name folder)))

#+(and :lispworks (or :macosx :mswindows :linux))
(defmethod tracer-expand-collapse-action ((table tracer-history-table)
                                          (folder folder-history))
  (setf (folder-history-expanded folder)
        (not (folder-history-expanded folder)))
  (let ((cell (first (tracer-selected-cells table))))
    (setup-tracer-table table cell)
    (tracer-cell-select table cell)
    (tracer-scroll-to-cell table cell))
  (tracer-redraw-cell table (first (tracer-selected-cells table))))

#+(and :lispworks (or :macosx :mswindows :linux))
(defmethod tracer-fully-expand-collapse-action ((table tracer-history-table)
                                                (folder folder-history))
  (let ((cell (first (tracer-selected-cells table))))
    (unfold-all-entries (list folder) (not (folder-history-expanded folder)))
    (setup-tracer-table table cell)
    (tracer-cell-select table cell)
    (tracer-scroll-to-cell table cell))
  (tracer-redraw-cell table (first (tracer-selected-cells table))))

(defmethod tracer-break-action ((table tracer-history-table)
                                (entry history-entry))
  (pushnew (history-entry-number entry) *breakpoints*)
  (tracer-redraw-cell table (first (tracer-selected-cells table))))

(defmethod tracer-unbreak-action ((table tracer-history-table)
                                  (entry history-entry))
  (setf *breakpoints* (delete (history-entry-number entry) *breakpoints*))
  (tracer-redraw-cell table (first (tracer-selected-cells table))))

(defmethod tracer-unbreak-all-action ((table tracer-history-table) (entry t))
  (setf *breakpoints* nil)
  )


;;; ======================================================================

(defmethod tracer-entry-action ((table tracer-history-table)
                                (entry end-folder-entry)
                                &key control-key-p option-key-p double-click-p)
  (let ((folder (end-folder-entry-referenced-entry entry)))
    (cond (control-key-p
           (let ((last-results (folder-history-last-results folder)))
             (unless (eq last-results +no-result-recorded+)
               (inspect last-results))))
          ((or option-key-p double-click-p)
           (setf (folder-history-expanded folder)
                 (not (folder-history-expanded folder)))
           (let ((cell (tracer-index-to-cell table
                                      (position folder
                                                (tracer-table-sequence table)))))
             (tracer-cell-deselect table (first (tracer-selected-cells table)))
             (tracer-cell-select table cell)
             (tracer-scroll-to-cell 
              table 
              cell)
             (setup-tracer-table table))))))
  
(defmethod tracer-inspect-action ((table tracer-history-table)
                                  (entry end-folder-entry))
  (let ((folder (end-folder-entry-referenced-entry entry)))
    (let ((last-results (folder-history-last-results folder)))
      (unless (eq last-results +no-result-recorded+)
        (inspect last-results)))))

(defmethod tracer-edit-definition-action ((table tracer-history-table)
                                          (folder end-folder-entry))
  (tracer-edit-definition (history-entry-name (end-folder-entry-referenced-entry folder))))

(defmethod tracer-trace-action ((table tracer-history-table)
                                (folder end-folder-entry))
  (ctrace-function (history-entry-name (end-folder-entry-referenced-entry folder))))

(defmethod tracer-untrace-action ((table tracer-history-table)
                                  (folder end-folder-entry))
  (unctrace-function (history-entry-name (end-folder-entry-referenced-entry folder))))

(defmethod tracer-expand-collapse-action ((table tracer-history-table)
                                          (folder end-folder-entry))
  (setf (folder-history-expanded (end-folder-entry-referenced-entry folder))
        (not (folder-history-expanded (end-folder-entry-referenced-entry folder))))
  (let* ((folder (end-folder-entry-referenced-entry folder))
         (cell (tracer-index-to-cell table
                              (position folder
                                        (tracer-table-sequence table)))))
    (tracer-cell-deselect table (first (tracer-selected-cells table)))
    (tracer-cell-select table cell)
    (tracer-scroll-to-cell table cell)
    (setup-tracer-table table)
    #+(and :lispworks (or :macosx :mswindows :linux))
    (tracer-cell-select table cell)))

;;; ======================================================================

(defmethod tracer-entry-action ((table tracer-history-table)
                                (object arglist-history-entry)
                                &key control-key-p option-key-p double-click-p)
  (if (or option-key-p control-key-p double-click-p)
    (inspect (arglist-history-entry-arg-value object))))

(defmethod tracer-inspect-action ((table tracer-history-table)
                                  (object arglist-history-entry))
  (inspect (arglist-history-entry-arg-value object)))

(defmethod tracer-edit-definition-action ((table tracer-history-table)
                                        (folder arglist-history-entry))
  nil)

(defmethod tracer-trace-action ((table tracer-history-table)
                                (folder arglist-history-entry))
  nil)

(defmethod tracer-untrace-action ((table tracer-history-table)
                                  (folder arglist-history-entry))
  nil)

(defmethod tracer-setf-trace-action ((table tracer-history-table)
                                     (folder arglist-history-entry))
  nil)

(defmethod tracer-setf-untrace-action ((table tracer-history-table)
                                       (folder arglist-history-entry))
  nil)

(defmethod tracer-expand-collapse-action ((table tracer-history-table)
                                          (folder arglist-history-entry))
  nil)


(defmethod tracer-fully-expand-collapse-action ((table tracer-history-table)
                                               (folder arglist-history-entry))
  nil)

;;; **********************************************************************

(defun setup-tracer-table (table &optional (changed-entry nil))
  #-(and :lispworks (or :macosx :mswindows :linux)) (declare (ignore changed-entry))
  (when (tracer-view-container table)
    (if #+(and :lispworks (or :macosx :mswindows :linux)) changed-entry #-(and :lispworks (or :macosx :mswindows :linux)) nil
        (let ((*indent-level* (history-entry-indent-level changed-entry)))
          (tracer-update-table-sequence table
                                        changed-entry
                                        (traverse-and-collect-expanded-entries changed-entry)))
      (let ((*indent-level* 0))
        (tracer-set-table-sequence 
         table
         (traverse-and-collect-expanded-entries 
          (traced-folder (tracer-view-container table))))))))

(defmethod traverse-and-collect-expanded-entries ((folder folder-history))
  (setf (history-entry-indent-level folder) *indent-level*)
  (cond ((folder-history-expanded folder)
         (cons folder
               (nconc
                (if (folder-history-arglist-name-value-assoc-shown-p folder)
                  (loop for (name . value) in (folder-history-arglist-name-value-assoc folder)
                        collect (make-arglist-history-entry 
                                 :name name
                                 :arg-value value
                                 :indent-level (+ +indent-offset+ +indent-offset+ *indent-level*)))
                  nil)
                (loop for entry in (folder-history-entries folder)
                      nconc (let ((*indent-level* (+ +indent-offset+ *indent-level*)))
                              (traverse-and-collect-expanded-entries entry)))
                (list (make-end-folder-entry :referenced-entry folder
                                             :indent-level *indent-level*)))))
        ((folder-history-arglist-name-value-assoc-shown-p folder)
         (cons folder 
               (nconc (if (folder-history-arglist-name-value-assoc-shown-p folder)
                        (loop for (name . value) in (folder-history-arglist-name-value-assoc folder)
                              collect (make-arglist-history-entry 
                                       :name name
                                       :arg-value value
                                       :indent-level (+ +indent-offset+ +indent-offset+ *indent-level*)))
                        nil)
                      (list (make-end-folder-entry :referenced-entry folder
                                                   :indent-level *indent-level*)))))
        (t (if (equal (folder-history-last-results folder) +no-result-recorded+)
             (list folder)
             (list folder
                   (make-end-folder-entry :referenced-entry folder
                                          :indent-level *indent-level*))))))

(defmethod traverse-and-collect-expanded-entries ((entry format-history-entry))
  (setf (history-entry-indent-level entry) (+ *indent-level* +indent-offset+))
  (cons entry
        (if (format-history-entry-arglist-name-value-assoc-shown-p entry)
          (loop for (name . value) in (format-history-entry-arglist-name-value-assoc entry)
                collect (make-arglist-history-entry 
                         :name name
                         :arg-value value
                         :indent-level (+ *indent-level* +indent-offset+)))
          nil)))
                      

(defmethod tracer-entry-action ((table tracer-history-table)
                                (object format-history-entry)
                                &key control-key-p option-key-p double-click-p)
  (declare (ignore control-key-p))
  (cond ((and option-key-p (not double-click-p))
         (let ((selected-entry (first (get-selected-items table))))
           (setf (format-history-entry-arglist-name-value-assoc-shown-p selected-entry)
                 (not (format-history-entry-arglist-name-value-assoc-shown-p selected-entry)))
           (setup-tracer-table table)
           ))
        (double-click-p
         (let ((stream (or (tracer-view-named 'output-view (tracer-view-container table))
                           (tracer-find-window "Listener"))))
           (unless (null (history-entry-name object))
             (princ (history-entry-name object) stream)
             (princ ": " stream))
           (format stream (format-history-entry-message object))
           (tracer-stream-force-output stream)))))

(defmethod tracer-inspect-action ((table tracer-history-table)
                                 (object format-history-entry))
  (when (format-history-entry-arglist-name-value-assoc object)
    (inspect (format-history-entry-arglist-name-value-assoc object))))

(defmethod tracer-edit-definition-action ((table tracer-history-table)
                                          (folder format-history-entry))
  nil)

(defmethod tracer-trace-action ((table tracer-history-table)
                                (folder format-history-entry))
  nil)

(defmethod tracer-untrace-action ((table tracer-history-table)
                                  (folder format-history-entry))
  nil)

(defmethod tracer-expand-collapse-action ((table tracer-history-table)
                                          (folder format-history-entry))
  nil)

(defmethod traverse-and-collect-expanded-entries ((entry end-folder-entry))
  (setf (history-entry-indent-level entry) *indent-level*)
  (list entry))

;;; **********************************************************************
;;; Inspired by CTRACE from Mike Travers
;;; With modifications from Ralf Moeller
;;; **********************************************************************

(defmacro deletef (thing place &rest delete-args)
  `(setf ,place (delete ,thing ,place ,@delete-args)))


#+(and :lispworks (or :macosx :mswindows :linux))
(defun compute-argname-argvalue-association (sym actual-args)
  (traverse-arglist (lw:function-lambda-list sym nil) actual-args))

(defun traverse-arglist (arglist actual-args)
  (cond #-(and :lispworks (or :macosx :mswindows :linux))
        ((null actual-args)
         nil)
        ((eq (first arglist) '&key)
         (loop for (key value) on actual-args by #'cddr collect (cons key value)))
        ((eq (first arglist) '&rest)
         (cons (cons (second arglist) actual-args)
               (traverse-arglist (rest (rest arglist)) actual-args)))
        ((eq (first arglist) '&optional)
         (traverse-arglist (rest arglist) actual-args))
        #+(and :lispworks (or :macosx :mswindows :linux))
        ((consp (first arglist))
         (cons (cons (first (first arglist))
                     (or (first actual-args) (second (first arglist))))
               (traverse-arglist (rest arglist) (rest actual-args))))
        (t (if (null arglist)
             nil
             (cons (cons (first arglist) (first actual-args))
                   (traverse-arglist (rest arglist) (rest actual-args)))))))

(defun clean-lambda-list (lambda-list)
  (loop with key-mode = nil
        with result = nil
        for orig-elem in lambda-list
        for elem = (if (consp orig-elem)
                       (first orig-elem)
                     orig-elem)
        do
        (cond 
         ((eq elem '&allow-other-keys) (setf key-mode nil))
         (key-mode
          (push (find-symbol (symbol-name elem) :keyword) result)
          (push elem result))
         ((eq elem '&key) (setf key-mode t))
         ((eq elem '&rest) (setf key-mode nil))
         ((not (eq elem '&optional)) (push elem result)))
        finally (return (nreverse result))))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun ctrace-function-1 (&optional function-name setf)
  (let* ((function-names (remove-if-not #'fboundp (find-all-symbols (symbol-name function-name))))
         (function-name (if (rest function-names)
                            (first (last function-names))
                          (first function-names)))
         (fn-name (if setf
                      `(setf ,(fn-of-setf-fn function-name))
                    function-name)))
    (if function-name
        (let ((lambda-list (lw:function-lambda-list function-name nil)))
          (assert (fboundp function-name) ()
            "No definition for ~S" function-name)
          (pushnew function-name *ctraced-functions*)
          (eval
           `(defadvice (,function-name ctrace :around)
                ,lambda-list
              (let ((arglist (list .,(clean-lambda-list lambda-list))))
                (with-trace-context (',fn-name
                                     :arglist-name-value-assoc 
                                     (compute-argname-argvalue-association ',function-name arglist)
                                     :arguments arglist
                                     :trace-result t)
                  (let ((*current-trace-level* 
                         (+ *current-trace-level* 1)))
                    (call-next-advice .,(clean-lambda-list lambda-list))))))))
      (prog1 *ctraced-functions*
        (dolist (fn *ctraced-functions* ())
          (ctrace-function-1 fn))))))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun ctrace-setf-function (&optional function-name)
  (let* ((function-names (remove-if-not #'fboundp (find-all-symbols (symbol-name function-name))))
         (function-name-tmp (if (rest function-names)
                                (first (last function-names))
                              (first function-names))))
    (ctrace-function-1 (find-setf-fn-for-fn function-name-tmp) t)))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun ctrace-function (&optional function-name)
  (if (and (consp function-name)
           (eq (first function-name) 'setf)
           (symbolp (second function-name))
           (null (rest (rest function-name))))
      (ctrace-setf-function (second function-name))
    (ctrace-function-1 function-name)))

;;; put a ctrace around an existing function
(defmacro ctrace (&rest args)
  (if args
      `(mapc #'ctrace-function ',args)
    '*ctraced-functions*))

(defmacro when-functions-traced-p (&body body)
  `(when *ctraced-functions*
     (progn .,body)))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun unctrace-setf-function (&optional function-name)
  (unctrace-function-1 (when function-name
                         (find-setf-fn-for-fn function-name))))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun unctrace-function-1 (&optional function-name)
  (if function-name
    (progn
      (remove-advice function-name 'ctrace)
      (deletef function-name *ctraced-functions*))
    (prog1 *ctraced-functions*
      (dolist (fn *ctraced-functions* ())
        (unctrace-function-1 fn)))))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun unctrace-function (&optional function-name)
  (if (and (consp function-name)
           (eq (first function-name) 'setf)
           (symbolp (second function-name))
           (null (rest (rest function-name))))
      (unctrace-setf-function (second function-name))
    (unctrace-function-1 function-name)))

(defmacro unctrace (&rest args)
  (if args
    `(mapc #'unctrace-function ',args)
    `(mapc #'unctrace-function *ctraced-functions*)))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun find-setf-fn-for-fn (fn)
  (find-symbol (concatenate 'string "set " (symbol-name fn)) (symbol-package fn)))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun find-fn-for-setf-fn (fn)
  (let* ((name (symbol-name fn))
         (prefix (when (> (length name) 4)
                   (subseq name 0 4))))
    (when (and prefix (string= prefix "set "))
      (find-symbol (subseq name 4) (symbol-package fn)))))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun setf-fn-p (fn)
  (let ((name (symbol-name fn)))
    (when (> (length name) 4)
      (string= (subseq name 0 4) "set "))))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun fn-of-setf-fn (fn)
  (when (setf-fn-p fn)
    (find-symbol (subseq (symbol-name fn) 4) (symbol-package fn))))


;;; **********************************************************************

(defmacro trace-format (format-string &rest arguments)
  (let ((argvar (gensym))) 
    `(let ((,argvar (list .,arguments)))
       (traced-format nil
                      ,format-string
                      ,argvar
                      (mapcar #'cons
                              ',arguments
                              (list .,arguments))))))

;;; **********************************************************************

(defconstant +trace-dialog-menu-item-name+ "Trace Manager")

#+(and :lispworks (or :macosx :mswindows :linux))
(defun install-trace-manager-menu ()
  (let* ((lw-apple-menu
          #+:macosx capi-cocoa-library::*apple-menu*
          ;#+:win32 (first (capi:interface-menu-bar-items 
          ;                 (win32::representation-element 
          ;                  (first (last capi-win32-lib::*top-level-windows*)))))
          #+(or :linux :mswindows)
          (first (capi:interface-menu-bar-items capi::*main-interface*)))
         (menu-items (when lw-apple-menu
                       (capi:menu-items lw-apple-menu))))
    (when (and menu-items
               (not (member-if (lambda (item)
                                 (eq (view-nick-name item) 'racer-tracer-menu))
                               menu-items)))
      (setf (capi:menu-items lw-apple-menu)
            (nconc (capi:menu-items lw-apple-menu)
                    (list (make-instance 'tracer-trace-menu-item
                                         :mnemonic-title "Racer Tracer"
                                         #+:macosx :accelerator #+:macosx #\t
                                         :view-nick-name 'racer-tracer-menu
                                         :callback
                                         (lambda (&rest args)
                                           (declare (ignore args))
                                           (let ((trace-dialog (first *trace-dialogs*)))
                                             (if (null trace-dialog)
                                                 (capi:display (make-trace-dialog))
                                               (let* ((table (tracer-view-named 'tracer-history-table
                                                                                trace-dialog))
                                                      (selected-entry (first (get-selected-items table))))
                                                 (setup-tracer-table trace-dialog)
                                                 (capi:display trace-dialog)
                                                 (when selected-entry
                                                   (tracer-cell-select table selected-entry))
                                                 (capi:raise-interface trace-dialog)))))))))))
  (let ((trace-dialog (first *trace-dialogs*)))
    (if (null trace-dialog)
        (make-trace-dialog)
      (progn
        (setup-tracer-table trace-dialog)
        trace-dialog))))

#+(and :lispworks (or :macosx :mswindows :linux))
(defun remove-trace-manager-menu ()
  (let* ((lw-apple-menu
          #+:macosx capi-cocoa-library::*apple-menu*
          ;#+:win32 (first (capi:interface-menu-bar-items 
          ;                 (win32::representation-element 
          ;                  (first (last capi-win32-lib::*top-level-windows*)))))
          #+(or :linux :mswindows)
          (first (capi:interface-menu-bar-items capi::*main-interface*)))
         (menu-items (when lw-apple-menu 
                       (capi:menu-items lw-apple-menu)))
         (racer-menu-item (when menu-items 
                            (find-if (lambda (item)
                                       (eq (view-nick-name item) 'racer-tracer-menu))
                                     menu-items))))
    (when racer-menu-item
      (setf (capi:menu-items lw-apple-menu)
            (delete racer-menu-item (capi:menu-items lw-apple-menu))))))

#+:tracer-window
(remove-trace-manager-menu)
#+:tracer-window
(install-trace-manager-menu)

(defun restore-trace-manager-menu ()
  (setf *trace-dialogs* nil))
  
#+(and :lispworks (or :macosx :mswindows :linux))
(editor:defcommand "Trace Setf in Racer Tracer" (p)
     (declare (ignore p))
     (let* ((x (editor:buffer-symbol-at-point (editor:current-buffer)))
            (function (find-function-for-arglist x)))
       (if (and (fboundp function) (find-setf-fn-for-fn function))
           (progn
             (editor:message "Tracing (setf ~A) in Racer Tracer" function)
             (ctrace-function `(setf ,function)))
         (editor:message "Undefined function (setf ~A) cannot be traced" function))))

#+(and :lispworks (or :macosx :mswindows :linux))
(editor:defcommand "Untrace Setf in Racer Tracer" (p)
     (declare (ignore p))
     (let* ((x (editor:buffer-symbol-at-point (editor:current-buffer)))
            (function (find-function-for-arglist x)))
       (if (and (fboundp function) (find-setf-fn-for-fn function))
           (progn
             (editor:message "Untracing (setf ~A) in Racer Tracer" function)
             (unctrace-function `(setf ,function)))
         (editor:message "Undefined function (setf ~A) cannot be untraced" function))))

#+(and :lispworks (or :macosx :mswindows :linux))
(editor:defcommand "Trace in Racer Tracer" (p)
     (declare (ignore p))
     (let* ((x (editor:buffer-symbol-at-point (editor:current-buffer)))
            (function (find-function-for-arglist x)))
       (if (fboundp function)
           (progn
             (editor:message "Tracing ~A in Racer Tracer" function)
             (ctrace-function function))
         (editor:message "Undefined function ~A cannot be traced" function))))

#+(and :lispworks (or :macosx :mswindows :linux))
(editor:defcommand "Untrace in Racer Tracer" (p)
     (declare (ignore p))
     (let* ((x (editor:buffer-symbol-at-point (editor:current-buffer)))
            (function (find-function-for-arglist x)))
       (if (fboundp function)
           (progn
             (editor:message "Untracing ~A in Racer Tracer" function)
             (unctrace-function function))
         (editor:message "Undefined function ~A cannot be untraced" function))))

#+(and :lispworks (or :macosx :mswindows :linux)) 
(defun find-function-for-arglist (x)
  (typecase x
    (symbol x)
    (list (unless (dotted-list-p x)
            (if (eq (length x) 1)
                (find-function-for-arglist (car x))
              (case (car x)
                ((quote function) (find-function-for-arglist (cdr x)))
                (setf (and (= (length x) 2)
                           (symbolp (second x))
                           x))))))))

#+(and :lispworks (or :macosx :mswindows :linux))
(progn
  (editor:bind-key "Trace in Racer Tracer" #("Control-c" "Control-t") :mode "Lisp")
  (editor:bind-key "Untrace in Racer Tracer" #("Control-c" "Control-u") :mode "Lisp")
  (editor:bind-key "Trace Setf in Racer Tracer" #("Control-c" "Control-T") :mode "Lisp")
  (editor:bind-key "Untrace Setf in Racer Tracer" #("Control-c" "Control-U") :mode "Lisp")
  (editor:bind-key "Show Racer Tracer" #("Control-c" "Control-r") :mode "Lisp"))

#+(and :lispworks (or :macosx :mswindows :linux))
(editor:defcommand "Show Racer Tracer" (p)
     (declare (ignore p))
     (let ((trace-dialog (first *trace-dialogs*)))
       (if (null trace-dialog)
           (capi:display (make-trace-dialog))
         (let* ((table (tracer-view-named 'tracer-history-table
                                          trace-dialog))
                (selected-entry (first (get-selected-items table))))
           (setup-tracer-table trace-dialog)
           (capi:display trace-dialog)
           (when selected-entry
             (tracer-cell-select table selected-entry))
           (capi:raise-interface trace-dialog)))))

#+(and :lispworks :linux)
(editor:bind-key "Show Racer Tracer" "Hyper-t" :mode "Lisp")

;;; **********************************************************************

(export '(unctrace unctrace-function ctrace ctrace-function 
          when-functions-traced-p
          with-trace-context new-folder-trace-entry
          formatting traced-format trace-format
          *trace-dialogs* *trace* *ignore-tracing*))
