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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *structure-id-lock*
    #+(and :lispworks (or :lispworks6 :lispworks7))
    (mp:make-lock :name "Global Structure Id" :sharing t)
    #+(and :allegro :smp-macros)
    (mp:make-sharable-lock :name "Global Structure Id"
                           :max-shared 20
                           :recursive-p '(:exclusive :shared)
                           :auto-unlock-p t)
    #+:ccl
    (ccl:make-read-write-lock))
  )

#-(or :lispworks :allegro :ccl)
(defmacro racer-with-exclusive-lock ((lock) &body body)
  (declare (ignore body))
  (error "unexpected use of with-racer-exclusive-lock for lock ~S" lock))

#+(and :lispworks (or :lispworks6 :lispworks7))
(defmacro racer-with-exclusive-lock ((lock) &body body)
  `(mp:with-exclusive-lock (,lock)
     . ,body))

#+(and :lispworks (not (or :lispworks6 :lispworks7)))
(defmacro racer-with-exclusive-lock ((lock) &body body)
  (declare (ignore lock))
  `(lw:without-interrupts
     . ,body))

#+(and :allegro :allegro-v8.2 :smp-macros)
(defmacro racer-with-exclusive-lock ((lock result-sym) &body body)
  `(progn
     (mp:with-exclusive-lock (,lock)
       . ,body)
     ,result-sym))

#+:ccl
(defmacro racer-with-exclusive-lock ((lock) &body body)
  `(ccl:with-write-lock (,lock)
     . ,body))

#+(and :allegro :smp-macros)
(defmacro racer-with-shared-lock ((lock result-sym) &body body)
  `(progn
     (mp:with-shared-lock (,lock)
       . ,body)
     ,result-sym))

#+(and :lispworks (or :lispworks6 :lispworks7))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *without-interrupts-lock*
    (mp:make-lock :name "without-interrupts" :recursivep nil :important-p t))
  )

#+:ccl
(defmacro racer-with-shared-lock ((lock result-sym) &body body)
  `(progn
     (ccl:with-read-lock (,lock)
       . ,body)
     ,result-sym))

(defmacro racer-without-interrupts (&body forms)
  #+(and :allegro (not :smp-macros))
  `(excl:without-interrupts
    . ,forms)
  #+(and :allegro :smp-macros)
  `(excl:with-delayed-interrupts
    . ,forms)
  #+(and :lispworks (or :lispworks6 :lispworks7))
  `(mp:with-exclusive-lock (*without-interrupts-lock*)
     . ,forms)
  #+(and :lispworks (not (or :lispworks6 :lispworks7)))
  `(lw:without-interrupts
     . ,forms)
  #+:ccl
  `(ccl:without-interrupts
    . ,forms)
  #-(or (and :allegro (not :smp-macros)) :lispworks :ccl)
  `(progn
     #-(or :abcl :sbcl) (warn "WITHOUT-INTERRUPTS used but not implemented")
     . ,forms))

(defmacro racer-atomic-incf (place &optional (delta 1))
  #+(or :ccl (and :lispworks (or :lispworks6 :lispworks7)))
  `(racer-with-exclusive-lock (*structure-id-lock*)
     (incf ,place ,delta))
  #+(and :lispworks (not (or :lispworks6 :lispworks7)))
  `(racer-without-interrupts
     (incf ,place ,delta))
  #+(and :allegro :smp-macros :allegro-v8.2)
  (let ((sym (gensym)))
    `(let ((,sym nil))
       (racer-with-exclusive-lock (*structure-id-lock* ,sym)
         (setf ,sym (incf ,place ,delta)))))
  #+(and :allegro :smp-macros (not :allegro-v8.2))
  `(incf-atomic ,place ,delta)
  #-(or :lispworks (and :allegro :smp-macros) :ccl)
  `(racer-without-interrupts
     (incf ,place ,delta))
  )

(defmacro racer-atomic-setf (place value)
  #+(or :ccl (and :lispworks (or :lispworks6 :lispworks7)))
  `(racer-with-exclusive-lock (*structure-id-lock*)
     (setf ,place ,value))
  #+(and :lispworks (not (or :lispworks6 :lispworks7)))
  `(racer-without-interrupts
     (setf ,place ,value))
  #+(and :allegro :smp-macros :allegro-v8.2)
  (let ((sym (gensym)))
    `(let ((,sym nil))
       (racer-with-exclusive-lock (*structure-id-lock* ,sym)
         (setf ,sym (setf ,place ,value)))))
  #+(and :allegro :smp-macros (not :allegro-v8.2))
  (let ((sym-old (gensym))
	(sym-new (gensym)))
    `(let ((,sym-old ,place)
	   (,sym-new ,value))
       (loop
	 (when (atomic-conditional-setf ,place ,sym-new ,sym-old)
	   (return ,sym-new)))))
  #-(or :lispworks (and :allegro :smp-macros) :ccl)
  `(racer-without-interrupts
     (setf ,place ,value))
  )

(defmacro racer-atomic-push (object place)
  #+(or :ccl (and :lispworks (or :lispworks6 :lispworks7)))
  `(racer-with-exclusive-lock (*structure-id-lock*)
     (push ,object ,place))
  #+(and :lispworks (not (or :lispworks6 :lispworks7)))
  `(racer-without-interrupts
     (push ,object ,place))
  #+(and :allegro :smp-macros :allegro-v8.2)
  (let ((sym (gensym)))
    `(let ((,sym nil))
       (racer-with-exclusive-lock (*structure-id-lock* ,sym)
         (setf ,sym (push ,object ,place)))))
  #+(and :allegro :smp-macros (not :allegro-v8.2))
  `(push-atomic ,object ,place)
  #-(or :lispworks (and :allegro :smp-macros) :ccl)
  `(racer-without-interrupts
     (push ,object ,place))
  )

(defmacro racer-atomic-pop (place)
  #+(or :ccl (and :lispworks (or :lispworks6 :lispworks7)))
  `(racer-with-exclusive-lock (*structure-id-lock*)
     (pop ,place))
  #+(and :lispworks (not (or :lispworks6 :lispworks7)))
  `(racer-without-interrupts
     (pop ,place))
  #+(and :allegro :smp-macros :allegro-v8.2)
  (let ((sym (gensym)))
    `(let ((,sym nil))
       (racer-with-exclusive-lock (*structure-id-lock* ,sym)
         (setf ,sym (pop ,place)))))
  #+(and :allegro :smp-macros (not :allegro-v8.2))
  `(pop-atomic ,place)
  #-:lispworks
  `(racer-without-interrupts
     (pop ,place))
  )

(defmacro racer-shared-read (place)
  #+(and :lispworks (or :lispworks6 :lispworks7))
  `(mp:with-sharing-lock (*structure-id-lock*)
    ,place)
  #+(and :lispworks (not (or :lispworks6 :lispworks7)))
  `(racer-without-interrupts
     ,place)
  #+(or :ccl (and :allegro :smp-macros))
  (let ((sym (gensym)))
    `(let ((,sym nil))
       (racer-with-shared-lock (*structure-id-lock* ,sym)
         (setf ,sym ,place))))
  #-(or :lispworks (and :allegro :smp-macros))
  place
  )
