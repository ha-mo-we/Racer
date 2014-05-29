;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWLAPI; Base: 10 -*-

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

(in-package :owlapi)

;;;
;;;;  owlapi-tools.lisp
;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   The Original Software is 
;;;   OntoLisp (NOSA): A Semantic Web Framework for OWL 2 and OWLlink in Common Lisp
;;;
;;;   Copyright (c) 2007-2010 Michael Wessel and Racer Systems GmbH & Co. KG 
;;;   All Rights Reserved.
;;;
;;;   Contributor(s): Michael Wessel  (mailto:michael_wessel@gmx.de
;;;                                    mailto:wessel@racer-systems.com) 
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   Purpose: Some auxiliary functions for the OWLAPI 
;;; 

(defvar *sym-count* 0)

(defconstant +secret-prefix+ 'secret-cdsc7897qcjk)

(defconstant +default-hash-table-size+ 100)

(defun set-equal (a b &rest args)
  (and (apply #'subsetp a b args)
       (apply #'subsetp b a args)))

#-:racer-server
(defun to-keyword (x)
  (intern (format nil "~A" x) :keyword))

(defun ensure-list (arg)
  (if (listp arg)
      arg
    (list arg)))

(defun ensure-string (string) 
  (if (stringp string)
      string
    (if (symbolp string)
        (symbol-name string)
      (format nil "~A" string))))

;;;
;;;
;;;

(defun ends-with-#-p (prefix)
  (let ((string
         (ensure-string prefix)))
    (unless (zerop (length string)) 
      (char= #\# 
             (elt string 
                  (1- (length string)))))))
  
(defun starts-with-#-p (prefix)
  (let ((string
         (ensure-string prefix)))
    (unless (zerop (length string)) 
      (char= #\# 
             (elt string 
                  0)))))

(defun without-# (string)
  (if (stringp string)
      (let ((n (length string)))
        (if (char= #\# (elt string (1- n)))
            (subseq string 0 (1- n))
          string))
    string))

(defun without-starting-# (string)
  (if (stringp string)
      (if (char= #\# (elt string 0))
          (subseq string 1)
        string)
    string))

(defun with-# (string)
  (format nil "~A#" (without-# string)))

;;;
;;;
;;; 


(defun ends-with-colon-p (prefix)
  (let ((string
         (ensure-string prefix)))
    (unless (zerop (length string)) 
      (char= #\: 
             (elt string 
                  (1- (length string)))))))
  
(defun ensure-ends-with-colon (prefix)
  (if (ends-with-colon-p prefix)
      prefix
    (let ((res 
           (concatenate 'string (ensure-string prefix) ":")))
      (etypecase prefix
        (keyword (to-keyword res))
        (symbol (intern res))
        (string res)))))


(defun without-colon (prefix)
  (let ((prefix (ensure-string prefix)))
    (if (ends-with-colon-p prefix)
        (subseq prefix 0 (1- (length prefix)))
      prefix)))

(defun get-prefix-postfix (symbol) 
  (let* ((name (ensure-string symbol))
         (pos (position #\: name)))
    (if pos
        (values (intern (subseq name 0 (1+ pos)))
                (intern (subseq name (1+ pos))))
      (values nil symbol))))

;;;
;;;
;;;

#+:racer
(defun mht (&key (size +default-hash-table-size+) (test #'eql))
  (racer-make-hash-table :size size :rehash-size 2.0 :test test))

#-:racer
(defun mht (&key (size +default-hash-table-size+) (test #'eql))
  (make-hash-table :size size :rehash-size 2.0 :test test))

(defun create-marker (&optional sym (new-p t))
  (if sym       
      (if new-p 
          (intern (format nil "~A-~A-~A" +secret-prefix+ sym (incf *sym-count*)) :cl-user)
        (intern (format nil "~A-~A" +secret-prefix+ sym)  :cl-user))
    (intern (format nil "~A-~A" +secret-prefix+ (incf *sym-count*))  :cl-user)))


#+:racer-server
(defun owlapi-warning (string &rest args)
  (apply #'ts::nrql-warning
         (concatenate 'string "~%~%OWLAPI Warning: " 
		      (ensure-string string))
         args))

#-:racer-server
(defun owlapi-warning (string &rest args)
  (apply #'format
         t
         (concatenate 'string "~%~%OWLAPI Warning: " 
		      (ensure-string string))
         args))

(defun owlapi-runtime-error (string &rest args)
  (apply #+:racer-server
         #'ts::nrql-error 
         #-:racer-server
         #'error
         (concatenate 'string "OWLAPI Runtime Error: " 
		      (ensure-string string))
         args))

(defun owlapi-parser-error (string &rest args)
  (apply #+:racer-server
         #'ts::nrql-error
         #-:racer-server
         #'error
         (concatenate 'string "OWLAPI Parser Error: " 
		      (ensure-string string))
         args))

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun string-transform (string)
    (ecase (readtable-case *readtable*)
      (:upcase (if (char= (aref string 0) #\|)
                 string
                 (string-upcase string)))
      (:preserve string)
      (:downcase (string-downcase string)))))

;;;
;;;
;;;

#+(or :racer-server (and :dlmaps (not :midelora)))
(defmacro defpersistentclass (&rest rest)
  `(progn ,@(let ((name (first rest)))
	      (list 
	       `(persistence-manager:defclass ,@rest)
               `(defun ,(intern (format nil "~A-~A-~A"
                                        (string-transform "is") name (string-transform "p")))
                       (obj)
                  (typep obj ',name))))))

#-(or :racer-server (and :dlmaps (not :midelora)))
(defmacro defpersistentclass (&rest rest)
  `(defclass ,@rest))

;;;
;;;
;;;

#+(or :racer-server (and :dlmaps (not :midelora)))
(defmacro defpersistentstruct (name &rest args)
  `(persistence-manager::defstruct ,name ,@args))
  
#-(or :racer-server (and :dlmaps (not :midelora)))
(defmacro defpersistentstruct (name &rest args)
  (let ((name
         (if (consp name)
             (remove-if (lambda (x)
                          (and (consp x)
                               (eq (first x) :package)))
                        name)
           name)))
  `(defstruct ,name ,@args)))


;;;
;;;
;;;

(defmacro owlapi-defun ((name &key doc dont-export)
                        lambda-list 
                        &body body)
  #-:racer-server
  (declare (ignorable doc dont-export))
  #+:racer-server
  `(nrql-defun (,name :doc ,doc :dont-export ,dont-export) ,lambda-list ,@body)
  #-:racer-server
  `(defun ,name ,lambda-list ,@body))

(defmacro owlapi-defmethod ((name &key doc dont-export)
                        lambda-list 
                        &body body)
  #-:racer-server
  (declare (ignorable doc dont-export))
  #+:racer-server
  `(nrql-defmethod (,name :doc ,doc :dont-export ,dont-export) ,lambda-list ,@body)
  #-:racer-server
  `(defmethod ,name ,lambda-list ,@body))


(defmacro => (a b)
  `(or (not ,a) ,b))

(defun is-url-p (x)
  (let ((name (if (symbolp x)
                  (symbol-name x)
                x)))
    (or 
     (and (> (length name) 6)
          (or (string-equal "http://" 
                            (subseq name 0 7))
              (string-equal "file://" 
                            (subseq name 0 7))))
     (and (> (length name) 7)
          (or (string-equal "https://" 
                            (subseq name 0 8)))))))
     
(defun is-file-url-p (x)
  (let ((name (if (symbolp x)
                  (symbol-name x)
                x)))
    (and (is-url-p x)
         (> (length name) 6)
         (string-equal "file://" 
                       (subseq name 0 7)))))

(defun is-http-url-p (x)
  (let ((name (if (symbolp x)
                  (symbol-name x)
                x)))
    (and (is-url-p x)
         (or (and (> (length name) 6)
                  (string-equal "http://" 
                                (subseq name 0 7)))
             (and (> (length name) 7)
                  (string-equal "https://" 
                                (subseq name 0 8)))))))


(defparameter *replacements*
  '(("&" "&amp;" nil)
    ("!empty" "&nbsp;" nil)
#|
 These are non-ascii characters. The effect is not defined.
    ("ä" "&auml;" nil)
    ("ö" "&ouml;" nil)
    ("ü" "&uuml;" nil)
    ("Ä" "&Auml;" nil)
    ("Ö" "&Ouml;" nil)
    ("Ü" "&Uuml;" nil)
    ("ß" "&szlig;" nil)
|#
    ("<" "&lt;" nil)
    (">" "&gt;" nil)
    ("\\@" "&#64;" nil)))


#-:racer-server
(defun whitespace-char-p (char)
  #+:lispworks
  (lispworks:whitespace-char-p char)
  #+:allegro
  (stream::whitespace-char-p char)
  #+:sbcl
  (find char SB-FORMAT::*FORMAT-WHITESPACE-CHARS*)
  #+(and (not :allegro) (not :lispworks) (not :sbcl))
  (to-be-implemented 'whitespace-char-p))

#-:racer-server
(defun blank-line-p (line)
  (or (eq line 'newline)
      (and (typep line 'string)
	   (not (position-if-not #'(lambda (i) (char= i #\space)) line)))))


#-:racer-server
(defun string-substitute (string &optional (rules *replacements*)
			         &key add-spaces)
  (labels ((do-it (string akku)
             (cond ((blank-line-p string) akku)
                   (t 
                    (let ((min-pos nil)
                          (min-from-to))
                      (dolist (from-to rules)
                        (let* ((from (first from-to))
                               (pos (search from string)))
                          (when pos
                            (if (or (not min-pos) 
                                    (< pos min-pos))
                                (setf min-from-to from-to
                                      min-pos pos)))))
                      (let ((from (first min-from-to))
                            (to (second min-from-to))
                            (replaced-as-new-input-p (third min-from-to)))
                        (if min-pos
                            (if replaced-as-new-input-p
                                (do-it 
                                 (concatenate 'string 
					      to
					      (subseq string (+ min-pos (length from))))
                                 (append akku 
                                         (list (subseq string 0 min-pos))))
                              (do-it 
                               (subseq string (+ min-pos (length from)))
                               (append akku 
                                       (list (subseq string 0 min-pos))
                                       (list to))))
                          (append akku (list string)))))))))
      
    (let ((res (do-it (if add-spaces 
                          (concatenate 'string " " string " ")
                        string)
		      nil)))
      (if res 	  
          (reduce #'(lambda (x y)
                      (concatenate 'string x y))
                  res)
        ""))))




(defmacro defmethod1 (name lambda-list &body body)
  `(owlapi-defmethod (,name :dont-export t)
     ,lambda-list
     ,@body))

(defmacro defun1 (name lambda-list &body body)
  `(owlapi-defun (,name :dont-export t)
     ,lambda-list
     ,@body))


(defun string-to-boolean (x)
  (if (member x '(nil t))
      x
    (when (stringp x)
      (cond ((string= x "true") t)
            ((string= x "false") nil)
            ((string= x "t") t)
            ((string= x "T") t)
            ((string= x "nil") nil)
            ((string= x "NIL") nil)
            (t nil)))))


#+:racer-server
(defun to-be-implemented (method)
  (nrql-error "~A: To be implemented" method))

#-:racer-server
(defun to-be-implemented (method)
  (error "~A: To be implemented" method))

;;;
;;;
;;;

#-:racer-server
(defun shrink-whitespaces (string)
  (if (stringp string)
      (if (every #'whitespace-char-p string)
          ""
        (let ((pos (or (position-if-not #'whitespace-char-p string) 0))
              (pos2 (or (position-if-not #'whitespace-char-p string :from-end t)
                        (1- (length string)))))
          (subseq string pos (1+ pos2))))
    string))

(defun clean-url (url)
  (if url 
      (let ((pos (position #\space url)))
	(if pos
	    (concatenate 'string
	      (subseq url 0 pos)
	      "%20"
	      (clean-url (subseq url (1+ pos))))
	  url))
    ""))


(defun decode-month (m)
  (case m 
    (1 "January")
    (2 "February")
    (3 "March")
    (4 "April")
    (5 "May")
    (6 "June")
    (7 "July")
    (8 "August")
    (9 "September")
    (10 "October")
    (11 "November")
    (12 "December")))


(defun get-current-date ()
  (multiple-value-bind (s mi h d mo year)
      (decode-universal-time (get-universal-time))
    (declare (ignorable s))
    (format nil "~2,'0D ~2,'0D ~2,'0D, ~2,'0D:~2,'0D" (decode-month mo) d year 
            h mi)))

;;;
;;;
;;;

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))


