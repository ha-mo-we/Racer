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

(defparameter *racer-trace* nil
  "enables trace output to value of *racer-trace* as stream, if not nil")

(defun trace-racer ()
  (setf *racer-trace* t))

(defun untrace-racer ()
  (setf *racer-trace* nil))

(defmacro race-inline (inline-fn-list)
  #+:debug `(declaim (notinline . ,inline-fn-list))
  #-:debug `(declaim (inline . ,inline-fn-list))
  )

(defmacro when-debug (condition &body body)
  #+:debug
  `(when ,condition
     (progn . ,body))
  #-:debug
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
    #+(and :lispworks (or :macosx :mswindows :linux))
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
    #-(and :lispworks (or :macosx :mswindows :linux))
    (if spec-p
        `(let ((*print-readably* nil))
           (format *racer-trace* ,title-format-string . ,title-arguments)
           (format *racer-trace* ,format-string . ,arguments))
      `(let ((*print-readably* nil))
         (format *racer-trace* ,format-string . ,arguments)))))

(defmacro race-trace (trace-list &optional (title-list trace-list spec-p))
  `(progn 
     (when-debug *racer-trace*
       (race-message-internal ,trace-list ,title-list ,spec-p))))

(defmacro race-message (trace-list)
  `(if *racer-trace*
     (race-message-internal ,trace-list nil nil)
     #+(and :lispworks (or :macosx :mswindows :linux))
     (tools:trace-format ,(first trace-list) . ,(rest trace-list))
     #-(and :lispworks (or :macosx :mswindows :linux))
     (format *racer-trace* ,(first trace-list) . ,(rest trace-list))))

(defmacro when-race-trace (test-form &body then-forms)
  `(when-debug *racer-trace*
     (when ,test-form
       . ,then-forms)))

(defmacro if-race-trace (then-form else-form)
  #+(or :debug :ccl (and :lispworks (or :macosx :mswindows :linux)))
  `(if *racer-trace*
     ,then-form
     ,else-form)
  #-(or :debug :ccl (and :lispworks (or :macosx :mswindows :linux)))
  (declare (ignore then-form else-form))
  #-(or :debug :ccl (and :lispworks (or :macosx :mswindows :linux))) nil
  )

(defun param-value-pair (symbol value)
  (cons symbol value))

(defmacro with-race-trace-sublevel ((name 
                                     &key 
                                     (expanded nil expanded-spec-p)
                                     arguments
                                     trace-result)
                                    &body forms)
  #-:debug
  (declare (ignore name expanded expanded-spec-p arguments trace-result))
  #-:debug
  `(progn . ,forms)
  #+(and :debug (not :lispworks))
  (declare (ignore expanded expanded-spec-p))
  #+:debug
  (let ((forms-sym (gensym)))
    `(let ((,forms-sym
	    (lambda ()
	      .,forms)))
       (if *racer-trace*
	   ,(let ((sym (gensym))
                  #-:lispworks
                  (sym2 (gensym)))
	      `(let ((,sym ,arguments))
		 #+:lispworks
		 ,(if expanded-spec-p
		      `(tools:with-trace-context ((intern (string-upcase ,name) *package*)
						  :expanded ,expanded
						  :arguments ,sym
						  :arglist-name-value-assoc
						  (mapcar #'param-value-pair
							  ',(rest arguments)
							  ,sym)
						  :trace-result ,trace-result)
			 (funcall ,forms-sym))
		    `(tools:with-trace-context ((intern (string-upcase ,name) *package*)
						:arguments ,arguments
						:arglist-name-value-assoc
						(mapcar #'param-value-pair
							',(rest arguments)
							,sym)
						:trace-result ,trace-result)
		       (funcall ,forms-sym)))
		 #-:lispworks
                 (progn
                   (race-trace ("~&Calling ~A ~A~%"
                                ,name
                                (mapcar #'param-value-pair
                                        ',(rest arguments)
                                        ,sym)))
                   (let ((,sym2
                          (multiple-value-list
                           (funcall ,forms-sym))))
                     ,(if trace-result
                          `(race-trace ("~&Exiting ~A with ~A~%" ,name ,sym2))
                        `(race-trace ("~&Exiting ~A~%" ,name)))
                     (apply #'values ,sym2)))))
	 (funcall ,forms-sym)))))

;;;===========================================================================

(defun show-trace ()
  #+(and :lispworks (or :macosx :mswindows :linux))
  (when tools:*trace*
    (tools::make-trace-dialog)))
