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

(defun generate-temp-filename-1 ()
  (loop for i from 0 to 10000
        for temp-filename = (format nil "~Aracer-~D.temp" (temp-directory) i)
        unless (probe-file temp-filename)
        do (return-from generate-temp-filename-1 temp-filename))
  (error "Cannot create temporary file."))

#|
#+:lispworks
(defun create-directory (directory-orig)
  (let ((directory directory-orig))
    (unless (consp directory)
      (setf directory (pathname-directory (pathname directory))))
    (let ((dir1 (list :absolute)))
      (loop for dir in (rest directory) do
            (setf dir1 (nconc  dir1 (list dir)))
            (let ((pathname (make-pathname :directory dir1)))
              (unless (probe-file pathname)
                (system:make-directory 
                 #+:win32 (concatenate 'string 
                                       (pathname-host directory-orig)
                                       ":"
                                       (namestring pathname))
                 #-:win32 pathname)))))))

(defun create-directory (directory)
  (unless (consp directory)
    (setf directory (pathname-directory (pathname directory))))
  (let ((dir1 (list :absolute)))
    (loop for dir in (rest directory) do
          (setf dir1 (nconc  dir1 (list dir)))
          (let ((pathname (make-pathname :directory dir1)))
          (unless (probe-file pathname)
            (system:make-directory 
	     #+:win32 (concatenate 'string 
                                   (pathname-host pathname) 
                                   (namestring pathname))
	     #-:win32 pathname))))))
|#

#+:allegro
(defun create-directory (directory-orig)
  (let ((directory directory-orig))
    (unless (consp directory)
      (setf directory (pathname-directory (pathname directory))))
    (let ((dir1 (list :absolute)))
      (loop for dir in (rest directory) do
            (setf dir1 (nconc  dir1 (list dir)))
            (let ((pathname (make-pathname :directory dir1)))
              (unless (probe-file pathname)
                (excl:make-directory 
                 #+:mswindows (concatenate 'string 
                                           (pathname-device directory-orig) 
                                           ":"
                                           (namestring pathname))
                 #-:mswindows pathname)))))))

#+:ccl
(defun create-directory (directory)
  (ccl:create-directory directory ;;:if-exists nil
                        ))

#+(and (not :ccl) (not :allegro) (:not :lispworks))
(defun create-directory (directory)
  (error "Not yet implemented."))

#+:lispworks
(defun create-directory (directory-orig)
  (let ((directory directory-orig))
    (unless (consp directory)
      (setf directory (pathname-directory (pathname directory))))
    (let ((dir1 (list :absolute)))
      (loop for dir in (rest directory) do
            (setf dir1 (nconc  dir1 (list dir)))
            (let ((pathname (make-pathname :directory dir1
                                           :host (pathname-host directory-orig))))
              (unless (probe-file pathname)
                (system:make-directory pathname)))))))



(defmacro with-temp-file (((stream-var) &rest generator-forms) &body parser-forms)
  (let ((temp-filename-var (gensym)))
    `(let ((,temp-filename-var (generate-temp-filename-1)))
       (create-directory (make-pathname :directory (pathname-directory 
                                                    (pathname ,temp-filename-var))
                                        :host (pathname-host 
                                               (pathname ,temp-filename-var))))
       (unwind-protect
         (progn
           (with-open-file (,stream-var 
                            ,temp-filename-var
                            ;; :element-type 'character
                            :external-format *file-external-format*
                            :direction :output
                            :if-exists :supersede)
             .,generator-forms)
           (with-open-file (,stream-var 
                            ,temp-filename-var
                            :element-type 'character
                            :external-format *file-external-format*
                            :direction :input)
             .,parser-forms)
           #+:ccl (sleep 1))
         (delete-file ,temp-filename-var)))))

#|
(defmacro with-temp-file (((stream-var) &rest generator-forms) &body parser-forms)
  (let ((temp-filename-var (gensym)))
    `(let ((,temp-filename-var (generate-temp-filename-1)))       
       (create-directory (make-pathname :directory (pathname-directory ,temp-filename-var)))
       (with-open-file (,stream-var 
                        ,temp-filename-var
                        :direction :output
                        :if-exists :supersede)
         .,generator-forms)
       (unwind-protect
         (with-open-file (,stream-var 
                          ,temp-filename-var
                          :direction :input)
           .,parser-forms)
         (delete-file ,temp-filename-var)))))
|#
                                             
