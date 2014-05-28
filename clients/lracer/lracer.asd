;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

(in-package cl-user)

(let ((translation (concatenate 'string 
                                (directory-namestring *load-pathname*)
                                "**/*.*")))
  (setf (logical-pathname-translations "lracer")
        `(("**;*.*.*" ,translation))))

(defvar *lracer-dir*)

(defvar *lracer-demo-dir*)

(asdf:defsystem "lracer"
  :description "Lisp client for Racer"
  :license "BSD 3"
  :pathname #P"lracer:"
  :serial t
  :defsystem-depends-on (#+:lispworks 
                         (:require "comm") 
                         #+:sbcl
                         (:require "sb-bsd-sockets"))
  :components ((:file "nrql-symbols")
               (:file "lracer-package")
               (:file "lracer-core")
               (:file "lracer-stubs")
               (:file "lracer-reader-macros")))

