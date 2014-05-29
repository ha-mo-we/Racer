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

(defparameter *race-trace* nil
  "enables trace output to value of *race-trace* as stream, if not nil")

(defparameter *show-race-trace* t)

(defun trace-race ()
  (setf *race-trace* t))

(defun untrace-race ()
  (setf *race-trace* nil))

(defmacro race-inline (inline-fn-list)
  #+:debug `(declaim (notinline . ,inline-fn-list))
  #-:debug `(declaim (inline . ,inline-fn-list))
  )

(defmacro when-debug (condition &body body)
  #+(and :debug :lispworks)
  `(when ,condition
     (progn . ,body))
  #-(and :debug :lispworks)
  (declare (ignore condition body))
  )


(defun ensure-first-char-is-capitalized (format-string)
  (let ((position 0))
    (loop until (not (char= (aref format-string position) #\~))
          do (if (member (aref format-string (1+ position)) '(#\% #\&))
               (incf position 2)
               (incf position 1)))
    (nstring-capitalize format-string :start position :end (1+ position))))

(defmacro race-message-internal (trace-list title-list spec-p)
  (let ((title-format-string (first title-list))
        (title-arguments (rest title-list))
        (format-string (first trace-list))
        (arguments (rest trace-list)))
    #+(or (and :ccl (not :ccl)) (and :lispworks (or :macosx :mswindows :linux)))
    `(tools:formatting (,(if spec-p
                             `(format nil
                                      ,(ensure-first-char-is-capitalized 
                                        title-format-string)
                                      . ,title-arguments)
                           'nil)
                        :arglist-name-value-assoc 
                        (mapcar #'param-value-pair
                                ',(if spec-p
                                      (append title-arguments arguments)
                                    arguments)
                                (list ,@(if spec-p 
                                            title-arguments
                                          nil)
                                      .,arguments)))
                       ,(ensure-first-char-is-capitalized 
                         format-string)
                       . ,arguments)
    #-(or (and :ccl (not :ccl)) (and :lispworks (or :macosx :mswindows :linux)))
    (if spec-p
        `(let ((*print-readably* nil))
           (format *race-trace* ,title-format-string . ,title-arguments)
           (format *race-trace* ,format-string . ,arguments))
      `(let ((*print-readably* nil))
         (format *race-trace* ,format-string . ,arguments)))))

(defmacro race-trace (trace-list &optional (title-list trace-list spec-p))
  `(progn 
     (when-debug *race-trace*
       (race-message-internal ,trace-list ,title-list ,spec-p))))

(defmacro race-message (trace-list)
  `(if *race-trace*
     (race-message-internal ,trace-list nil nil)
     #+(or (and :ccl (not :ccl)) (and :lispworks (or :macosx :mswindows :linux)))
     (tools:trace-format ,(first trace-list) . ,(rest trace-list))
     #-(or (and :ccl (not :ccl)) (and :lispworks (or :macosx :mswindows :linux)))
     (format *race-trace* ,(first trace-list) . ,(rest trace-list))))

(defmacro when-race-trace (test-form &body then-forms)
  `(when-debug *race-trace*
     (when ,test-form
       . ,then-forms)))

(defmacro if-race-trace (then-form else-form)
  #+(or :debug :ccl (and :lispworks (or :macosx :mswindows :linux)))
  `(if *race-trace*
     ,then-form
     ,else-form)
  #-(or :debug :ccl (and :lispworks (or :macosx :mswindows :linux)))
  (declare (ignore then-form else-form))
  #-(or :debug :ccl (and :lispworks (or :macosx :mswindows :linux))) nil
  )

(defun param-value-pair (symbol value)
  (cons symbol value))

(defmacro with-race-trace-sublevel ((name &key (expanded nil expanded-spec-p)
                                          arguments trace-result)
                                    &body forms)
  #-(and :debug :lispworks)
  (declare (ignore name expanded expanded-spec-p arguments trace-result))
  #-(and :debug :lispworks)
  `(progn . ,forms)
  #+(and :debug :lispworks)
  `(if *race-trace*
     ,(let ((sym (gensym)))
        `(let ((,sym ,arguments))
           ,(if expanded-spec-p
              `(tools:with-trace-context ((intern (string-upcase ,name) *package*)
                                          :expanded ,expanded
                                          :arguments ,sym
                                          :arglist-name-value-assoc
                                          (mapcar #'param-value-pair
                                                  ',(rest arguments)
                                                  ,sym)
                                          :trace-result ,trace-result)
                 . ,forms)
              `(tools:with-trace-context ((intern (string-upcase ,name) *package*)
                                          :arguments ,arguments
                                          :arglist-name-value-assoc
                                          (mapcar #'param-value-pair
                                                  ',(rest arguments)
                                                  ,sym)
                                          :trace-result ,trace-result)
                 . ,forms))))
     (progn . ,forms)))

;;;===========================================================================

(defun show-trace ()
  #+(or (and :ccl (not :ccl)) (and :lispworks (or :macosx :mswindows :linux)))
  (when tools:*trace*
    (tools::make-trace-dialog)))
