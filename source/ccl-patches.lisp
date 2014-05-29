;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CCL; Base: 10 -*-

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

(in-package :ccl)

(defparameter *do-not-slashify* t)

(let ((*warn-if-redefine-kernel* nil)
      (*warn-if-redefine* nil))
  
  (defun write-pname (name case stream)
    (declare (type simple-string name) (stream stream)
             (optimize (speed 3)(safety 0)))
    (let* ((readtable *readtable*)
           (syntax-readtable (if *print-readably*
                               %initial-readtable%
                               readtable))
           (readcase (readtable-case syntax-readtable))
           (attrtab (rdtab.ttab syntax-readtable))
           (escape? (or *print-readably* *print-escape*))
           (needs-escape nil))
      (flet ((slashify? (char)
               (declare (type character char))
               (and escape?                  
                    (or (and (eq readcase :upcase) (lower-case-p char))
                        (and (eq readcase :downcase) (upper-case-p char))
                        (eql char #\:)
                        (not (eql $cht_cnst (%character-attribute char attrtab))))))
             (single-case-p (name)
               (let ((sofar nil))
                 (dotimes (i (length name) sofar)
                   (declare (type fixnum i))
                   (declare (type simple-string name))
                   (let* ((c (schar name i))
                          (c-case (if (upper-case-p c)
                                    :upcase
                                    (if (lower-case-p c)
                                      :downcase))))
                     (when c-case
                       (if sofar 
                         (if (neq sofar c-case)
                           (return nil))
                         (setq sofar c-case))))))))
        (declare (dynamic-extent #'slashify? #'single-case-p))
        (block alice
          (let ((len (length name))
                (slash-count 0)
                (last-slash-pos 0))
            (declare (type fixnum len)
                     (type fixnum slash-count last-slash-pos))                
            (when escape?
              (when (or (%izerop len)
                        ;; if more than a few \, just use |...|
                        (and;;(not (memq readcase '(:invert :preserve))) ; these never slashify alpha-p
                         (let ((m (max (floor len 4) 2)))
                           (dotimes (i (the fixnum len) nil)
                             (declare (type fixnum i)) 
                             (when (slashify? (schar name i))
                               (setq slash-count (%i+ slash-count 1)
                                     needs-escape t)
                               (when (or *do-not-slashify*
                                         (eql slash-count m)
                                         (eq i (1+ last-slash-pos)))
                                 (return t))
                               (setq last-slash-pos i)))))
                        ;; or could be read as a number
                        (handler-case (%parse-number-token name 0 len *print-base*)
                          (arithmetic-error (c)
                                            (declare (ignore c))))
                        ;; or symbol consisting entirely of .'s
                        (dotimes (i len (setq needs-escape t))
                          (declare (fixnum i))
                          (unless (eql (schar name i) #\.)
                            (return nil))))
                (return-from alice
                  (write-escaped-string name stream #\|))))
            (case readcase
              (:preserve (return-from alice
                           (if needs-escape
                             (write-escaped-string name stream #\|)
                             (write-string name stream :start  0 :end len))))
              
              (:invert (return-from alice
                         (cond ((single-case-p name)
                                (write-perverted-string name stream len :invert (if needs-escape #\|)))
                               (t (if needs-escape
                                    (write-escaped-string name stream #\|)
                                    (write-string name stream :start  0 :end len))))))
              (t 
               (when (eql slash-count 0)
                 (return-from alice
                   (cond ((eq readcase case)
                          (write-string name stream :start  0 :end len))
                         (t (write-perverted-string name stream len case)))))))
            (let* ((outbuf-len (+ len len))
                   (outbuf-ptr -1)
                   (outbuf (make-string outbuf-len)))
              (declare (fixnum outbuf-ptr outbuf-len)
                       (dynamic-extent outbuf)
                       (simple-string outbuf))
              (dotimes (pos (the fixnum len))
                (declare (type fixnum pos))
                (let* ((char (schar name pos))
                       (slashify? (cond ((eql slash-count 0)
                                         nil)
                                        ((eql slash-count 1)
                                         (eql pos last-slash-pos))
                                        (t
                                         (slashify? char)))))
                  (declare (type character char))
                  (when slashify?
                    (setq slash-count (%i- slash-count 1))
                    (setf (schar outbuf (incf outbuf-ptr)) #\\))
                  (setf (schar outbuf (incf outbuf-ptr)) char)))
              (write-string outbuf stream :start  0 :end (1+ outbuf-ptr))))))))
  
  )

(let ((*warn-if-redefine-kernel* nil))

(defun %unlock-recursive-lock-ptr (ptr lock &optional (error t))
  (with-macptrs ((signal (%get-ptr ptr target::lockptr.signal))
                 (spin (%inc-ptr ptr target::lockptr.spinlock)))
    (unless (eql (%get-object ptr target::lockptr.owner) (%current-tcr))
      (if error
        (error 'not-lock-owner :lock lock)
        (return-from %unlock-recursive-lock-ptr nil)))
    (without-interrupts
     (when (eql 0 (decf (the fixnum
                          (%get-natural ptr target::lockptr.count))))
       (%get-spin-lock spin)
       (setf (%get-ptr ptr target::lockptr.owner) (%null-ptr))
       (let* ((pending (+ (the fixnum
                            (1- (the fixnum (%get-fixnum ptr target::lockptr.avail))))
                          (the fixnum (%get-fixnum ptr target::lockptr.waiting)))))
         (declare (fixnum pending))
         (setf (%get-natural ptr target::lockptr.avail) 0
               (%get-natural ptr target::lockptr.waiting) 0)
         (setf (%get-ptr spin) (%null-ptr))
         (dotimes (i pending)
           (%signal-semaphore-ptr signal)))))
    nil))

(defun %unlock-recursive-lock-object (lock &optional (error t))
  (%unlock-recursive-lock-ptr (%svref lock target::lock._value-cell) lock error))

(defun release-lock (lock &optional (error t))
  "Relinquish ownership of a given lock."
  (%unlock-recursive-lock-object lock error))

)
