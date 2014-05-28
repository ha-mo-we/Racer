;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

(in-package cl-user)

(defun demo-file (fn)
  (substitute #\/   #\\
	      (namestring
	       (truename
		(merge-pathnames (pathname (format nil "demo/~A" fn))
				 (pathname *lracer-dir*))))))


(defun lracer-test ()
  (load (merge-pathnames (pathname "lracer-test")
			 (pathname *lracer-dir* ))))

(defun lracer-utf8-test ()
  (load (merge-pathnames (pathname "lracer-utf8-test")
			 (pathname *lracer-dir*))
	#+:allegro :external-format
	#+:allegro racer::*external-format*))

