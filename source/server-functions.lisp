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

(defparameter *temp-directory* nil)

(defun temp-directory ()
  (or *temp-directory*
      #+:ccl "~/temp/"
      #+(and (not :ccl) :lispworks :win32) 
      (or (and (lw:environment-variable "TEMP")
               (concatenate 'string
                            (lw:environment-variable "TEMP")
                            "\\"))
          (and (lw:environment-variable "TMP")
               (concatenate 'string
                            (lw:environment-variable "TMP")
                            "\\"))
          (and (lw:environment-variable "HOME")
               (concatenate 'string
                            (lw:environment-variable "HOME")
                            "\\Temp\\"))
          "~/temp/")
      #+(and (not :ccl) :lispworks (not :win32)) "~/temp/"
      #+(and (not :ccl) :allegro :mswindows) 
      (or (and (sys:getenv "TEMP")
               (concatenate 'string
                            (sys:getenv "TEMP")
                            "\\"))
          (and (sys:getenv "TMP")
               (concatenate 'string
                            (sys:getenv "TMP")
                            "\\"))
          (and (sys:getenv "HOME")
               (concatenate 'string
                            (sys:getenv "HOME")
                            "\\Temp\\"))
          "~/temp/")
      #+(and (not :ccl) :allegro (not :mswindows)) "/tmp/"))
  

(defun get-server-timeout ()
  *server-timeout*)

(defun set-server-timeout (timeout)
  (setf *server-timeout* timeout))

(defun logging-on (&optional filename debug)
  (declare (special *tcp-console-logging* *log-file* *debug-racer-server*))
  (setf *tcp-console-logging* t)
  (setf *log-file* filename)
  (setf *debug-racer-server* debug))

(defun logging-off ()
  (declare (special *tcp-console-logging* *log-file* *debug-racer-server*))
  (setf *tcp-console-logging* nil)
  (setf *log-file* nil)
  (setf *debug-racer-server* nil))

(defmacro with-server-connection ((&key tbox abox
                                        host  
                                        port)
                                  &body forms)
  (declare (ignore tbox abox host port))
  `(progn .,forms))

(defun set-find-tbox (tbox-name1 tbox-name2)
  (setf (find-tbox tbox-name1) (find-tbox tbox-name2)))

(defun set-find-abox (abox-name1 abox-name2)
  (setf (find-abox abox-name1) (find-abox abox-name2)))


(defun transmit-file (extension n-bytes)
  (declare (ignore extension n-bytes)))

