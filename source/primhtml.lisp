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

;;; must be in UTF8-UNIX encoding for ACL!
;;; doesnt compile otherwise 

(in-package :racer)

(defvar *stream* t)

(defvar *level* 0)

(defvar *cur-page* "")

(defconstant +url-base+ "http://localhost:8080/")

(defun process-umlaute (item)
  (typecase item
    (symbol item)
    (string
     (if (search "select" item)
         item
       (ts:string-substitute item)))
    (cons
     (cons (process-umlaute (car item))
           (process-umlaute (cdr item))))
    (otherwise item)))          

(defvar *indent-mode* :normal)

(defvar *beginning-of-line* t)

(defvar *last-was* nil)

(defun myformat (string &rest args)
  (apply #'format *stream* string args)
  (setf *beginning-of-line* nil))

(defun indent ()
  (when *beginning-of-line* 
    (dotimes (i *level*)
      (myformat " "))))

(defun freshline ()
  (unless *beginning-of-line*
    (terpri *stream*)
    (setf *beginning-of-line* t)))

(defun one-space ()
  (unless *beginning-of-line*
    (myformat " ")))

(defmacro with-mode ((mode) &body body)
  (unless (member mode '(:normal
                         :left
                         :right
                         :leftright
                         :sequence))
    (error "Syntax error: bad mode ~S" mode))
  `(let ((*indent-mode* ,mode))
     ,@body))

(defmacro html ((tag &rest args) &body body)
  `(let ((*level* (1+ *level*)))
     (case *indent-mode*
       (:normal
        (freshline)
        (indent))
       ((:left :leftright)
        (one-space)))
     ,(if args
          `(myformat 
                   "<~A~A>"
                   (string-downcase (symbol-name ',tag))
                   (reduce #'(lambda (x y) (concatenate 'string x y))
                           (loop as arg in ',(loop as arg in args by #'cddr collect arg)
                                 as val in (list ,@(loop as val in (cdr args) by #'cddr collect val))
                                 when val
                                 collect
                                 (format nil " ~A=\"~A\"" 
                                         (string-downcase (symbol-name arg))
                                         val))))
        `(myformat
                 "<~A>"
                 (string-downcase (symbol-name ',tag))))

     (setf *last-was* :open-tag)
     
     (let ((*level* (1+ *level*)))
       ,@body)
     
     (case *indent-mode*
       (:normal
        (freshline)
        (indent)))

     (myformat "</~A>" (string-downcase (symbol-name ',tag)))

     (setf *last-was* :close-tag)

     (case *indent-mode*
       ((:right :leftright)
        (one-space)))))

(defmacro html1 ((tag &rest args))
  `(let ((*level* (1+ *level*)))
     ;;(declare (ignorable args)) Removed (RM Apr. 2014). In the generated code there is no args to be ignored anyway.
     (case *indent-mode*
       (:normal
        (freshline)
        (indent))
       ((:left :leftright)
        (one-space)))
     ,(if args
          `(myformat "<~A~A/>" (string-downcase (symbol-name ',tag))
                   (reduce #'(lambda (x y) (concatenate 'string x y))
                           (loop as arg in ',(loop as arg in args by #'cddr collect arg)
                                 as val in (list ,@(loop as val in (cdr args) by #'cddr collect val))
                                 when val
                                 collect
                                 (format nil " ~A=\"~A\"" 
                                         (string-downcase (symbol-name arg))
                                         val))))
        `(myformat "<~A/>" (string-downcase (symbol-name ',tag))))
     
     (setf *last-was* :close-tag)

     (case *indent-mode*
       ((:right :leftright)
        (one-space)))))

(defmacro content (arg)
  `(progn 
     (case *indent-mode*
       (:normal
        (freshline))
       ((:left :leftright)
        (when (and (not *beginning-of-line*)
                   (not (eq *last-was* :close-tag)))
          (one-space))))

     (typecase ,arg
       (string
        (myformat "~A" 
                (ts:shrink-whitespaces
                 (process-umlaute ,arg))))
       (otherwise 
        (myformat "~S" ,arg)))

     (setf *last-was* :content)
     
     (case *indent-mode*
       (:normal
        (freshline))
       ((:right :leftright)
        (one-space)))))

(defmacro header (&body body)
  `(progn 

     (html (head)

       (html (title :lang "en")
         (content 
          "Racer Reference Manual"))

       (html1 (meta 
               :http-equiv "content-type" 
               :content "text/html\; charset=ISO-8859-1"))

       (html1 (base :href +url-base+))
       
       (html1 (meta 
               :http-equiv "cache-control" 
               :content "no-cache"))
       
       (html1 (meta :name "generator"
                    :content "Racer"))

       (html1 (link
               :rel "stylesheet" 
               :type "text/css" 
               :href "style.css"))

       (html1 (link 
               :rel "shortcut icon" 
               :type "image/x-icon" 
               :href "favicon.ico"))

       ,@body

      
       )))

(defun doctype ()
  (myformat "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"~%~
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">~%"))


(defun make-date (day month year)
  (format nil "~A-~A-~A" year month day))

;;;
;;;
;;;

(defmacro with-paragraph ((&rest args) &body body)
  `(progn 
     (html (p :style "width:100%; clear:both" ,@args))
     ,@body))

(defmacro with-div ((&rest args) &body body)
  `(html (div ,@args)
     ,@body))

(defmacro left ((&rest args) &body body)
  `(with-div (:style "clear:left; float:left" ,@args)
             ,@body))

(defmacro right ((&rest args) &body body)
  `(with-div (:style "clear:right; float:right" ,@args)
             ,@body))

(defmacro center ((&rest args) &body body)
  `(with-div (:style "text-align:center" ,@args)
             ,@body))

(defmacro frame ((&rest args) &body body)
  `(with-div (:style "background-color: #dddddd" ,@args)
             ,@body))

(defmacro justify ((&rest args) &body body)
  `(with-div (:style "text-align:justify" ,@args)
             ,@body))


(defmacro with-link ((href &rest args) &body body)
  `(html (a :href ,href ,@args) ,@body))

(defmacro pic (fn &rest args &key (alt fn) &allow-other-keys)
  `(html1 (img :src ,fn :alt ,alt ,@args)))

(defmacro bold ((&rest args) &body body)
  `(html (b ,@args)
     ,@body))


(defmacro caption ((&rest args) text)
  `(html (tr)
     (html (td :style "text-align:center; vertical-align:middle" ,@args)
       (bold ()
         (content ,text)))))

(defmacro pic-lc (fn &rest args &key alt href &allow-other-keys)
  (if href
      (if alt 
          `(html (table)
               (html (tr)
                 (html (td :style "text-align:center; vertical-align:middle")
                   (with-link (,href ,@args)
                     (pic ,fn ,@args))))
               (caption () ,alt))
        `(with-link (,href ,@args)
           (pic ,fn ,@args)))
    (if alt 
        `(html (table)
           (html (tr)
             (html (td :style "text-align:center; vertical-align:middle")
               (pic ,fn ,@args)))
           (caption () ,alt))
      `(pic ,fn ,@args))))

(defmacro table ((&rest args) &rest body)
  `(html (table ,@args)
     ,@body))

(defmacro row ((&key row-args td-args) &rest body)
  `(html (tr ,@row-args)
     ,@(mapcar #'(lambda (row)
                   `(html (td ,@td-args)
                      ,row))
               body)))
