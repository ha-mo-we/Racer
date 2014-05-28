;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

(in-package cl-user)

#-(or :sbcl :allegro :clisp)
(error "UTF8 demo requires ACL or SBCL.")

(close-server-connection)

(setf *external-format* :utf-8)

;;; make sure RacerPro is running in UTF8 mode: RacerPro -- -ef @UTF8

(enable-lracer-read-macros)

;;;
;;; Server Side Loading
;;; 

(full-reset)

(racer-read-file 
 (demo-file "family-j-utf8.racer"))

(pprint (taxonomy))

(pprint (describe-individual 'みよ))

(pprint (retrieve (?x ?y) (and (?x 母)
			       (?x ?y 子孫を持つ))))

;;;
;;; Client Side Loading
;;; 

(full-reset)

(load (demo-file "family-j-utf8.racer")
      #+:allegro :external-format
      #+:allegro *external-format*)

(pprint (taxonomy))

(pprint (describe-individual 'みよ))

(pprint (retrieve (?x ?y) (and (?x 母)
			       (?x ?y 子孫を持つ))))


