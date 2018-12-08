;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: THEMATIC-SUBSTRATE; Base: 10 -*-

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

(in-package :thematic-substrate)

;;;
;;; Diese Datei wird nun geladen, wenn #-dlmaps gilt
;;;

(defmacro with-new-marking-context (&body body)
  `(progn
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun string-transform (string)
    (ecase (readtable-case *readtable*)
      (:upcase (if (char= (aref string 0) #\|)
                 string
                 (string-upcase string)))
      (:preserve string)
      (:downcase (string-downcase string)))))

#+(and :lracer (not :dlmaps))
(defmacro defpersistentclass (&rest rest)
  `(progn ,@(let ((name (first rest)))
	      (list
	       `(defclass ,(first rest)
                          ,(second rest)
                          ,(mapcar #'(lambda (x)
                                       (remove :not-persistent x))
                                   (third rest)))
               `(defun ,(intern (format nil "~A-~A-~A"
                                        (string-transform "is") name (string-transform "p")))
                       (obj)
                  (typep obj ',name))))))

#+(or :racer-server (and :dlmaps (not :midelora))) ;If #+:racer-server does not hold, Racer cannot be compiled! RM April 2014
(defmacro defpersistentclass (&rest rest)
  `(progn ,@(let ((name (first rest)))
	      (list
	       `(persistence-manager:defclass ,@rest)
               `(defun ,(intern (format nil "~A-~A-~A"
                                        (string-transform "is") name (string-transform "p")))
                       (obj)
                  (typep obj ',name))))))

;;;
;;;
;;;

(defun subclass-responsibility (method)
  (nrql-error "~A: Subclasses must implement" method))

(defun to-be-implemented (method)
  (nrql-error "~A: To be implemented" method))

;;;
;;;
;;;

(defun no ())

(defmacro => (a b)
  `(or (not ,a) ,b))

(defmacro <=> (a b)
  `(and (=> ,a ,b)
	(=> ,b ,a)))

(defun set-equal (a b &rest args)
  (and (apply #'subsetp a b args)
       (apply #'subsetp b a args)))

(defun ensure-list (arg)
  (if (listp arg)
      arg
    (list arg)))

(defun unlist-if-one (a)
  (if (listp a)
      (if (not (cdr a))
          (first a)
        (let ((a (remove-duplicates a)))
          (if (not (cdr a))
              (first a)
            a)))
    a))

;;;
;;;
;;;

(defun tree-map (fn tree &rest args)
  (mapcar #'(lambda (item)
              (if (consp item)
                  (apply #'tree-map fn item args)
                (apply fn item args)))
          tree))


(defun tree-find (tree x &rest args)
  (or (apply #'member x tree args)
      (some #'(lambda (sub)
		(and (consp sub)
		     (apply #'tree-find sub x args)))
	    tree)))

(defun newprod (list-of-args)
  (if (not (cdr list-of-args))
      list-of-args
    (let ((list (first list-of-args))
	  (res (newprod (rest list-of-args))))
      (if (not (cddr list-of-args))
	  (reduce #'nconc
                  (mapcar #'(lambda (elem1)
                              (mapcar #'(lambda (elem2)
                                          (list elem2 elem1))
                                      (first list-of-args)))
                          (second list-of-args)))
	(reduce #'nconc
                (mapcar #'(lambda (list2)
                            (mapcar #'(lambda (elem1)
                                        (cons elem1 list2))
                                    list))
                        res))))))


(defun tree-flatten (tree)
  (if (consp tree)
      (append (tree-flatten (car tree))
              (tree-flatten (cdr tree)))
    (when tree (list tree))))

(defun tree-substitute (tree old new)
  (tree-map #'(lambda (x)
                (if (equal x old)
                    new
                  x))
            tree))

;;;
;;;
;;;

;;; Kopie aus dlmaps:tools!


(defun change-package-of-description (description &optional (package :cl-user) change-keyword-p (keep-racer-symbols-p t))
  (when description
    (typecase description
      (list
       (mapcar #'(lambda (x)
                   (change-package-of-description x package change-keyword-p keep-racer-symbols-p))
               description))
      (symbol
       (if (and (keywordp description)
                (not change-keyword-p))
           description
         (if (and (eq (symbol-package description)
                      (find-package :racer))
                  keep-racer-symbols-p
                  ;;; Racer "interne" Symbole werden beibehalten!!!
                  )
             description
           (let ((*package*
                  (if package (find-package package)
                    *package*)))
             (intern (symbol-name description))))))
      (otherwise description))))

;;;
;;;
;;;

(defun simplify-boolean-expression (expr &optional recursively-p)
  ;;; (OP (OP c d)) -> (OP c d) etc.
  (if (consp expr)
      (let ((op (intern (format nil "~A" (first expr))
                        :keyword)))
        (case op
          ((:project-to :pi)
             (if recursively-p
                 `(:project-to
                   ,(second expr)
                   ,@(mapcar #'(lambda (x)
                                 (simplify-boolean-expression x t))
                             (cddr expr)))
               expr))
          ((:not :neg)
           (if recursively-p
               `(:not ,(simplify-boolean-expression (second expr) t))
             expr))
          ((:and :or :intersection :union :cap :cup)
           (let ((args
                  (remove-duplicates
                   (if recursively-p
                       (mapcar #'(lambda (x)
                                   (simplify-boolean-expression x t))
                               (rest expr))
                     (rest expr))
                   :test #'equal)))
             (if (not args)
                 (ecase op
                   ((:and :intersection :cap)
                    (parser-error "Bad query term ~A" expr))
                   ((:or :union :cup)
                    (parser-error "Bad query term ~A" expr)))
               (if (not (cdr args))
                   (first args)
                 `(,(case op
                      ((:and :intersection :cap) :and)
                      ((:or :union :cup) :or))
                   ,@(reduce #'append
                             (mapcar #'(lambda (arg)
                                         (if (consp arg)
                                             (if (eq (first arg) op)
                                                 (rest arg)
                                               (list arg))
                                           (list arg)))
                                     args)))))))
          (otherwise expr)))
    expr))


(defun get-boolean-dnf (expr)
  ;;;
  ;;; (AND (OR C (NOT D)) (OR E F)) (muss in NNF sein!) ->
  ;;; (OR (AND C E) (AND C F) (AND (NOT D) E) (AND (NOT D) F))
  ;;;

  (labels ((dnf (expr)
             (if (consp expr)
                 (let ((op (intern (format nil "~A" (first expr))
                                   :keyword)))
                   (if (member op '(:not :neg))
                       `(:not ,(second expr))
                     (let ((args (remove-duplicates (rest expr) :test #'equal)))
                       (cond ((member op '(:or :union :cup))
                              (simplify-boolean-expression
                               (cons :or (mapcar #'dnf args))))
                             ((member op '(:and :intersection :cap))
                              (let* ((args (mapcar #'(lambda (x)
                                                       (let ((x (dnf x)))
                                                         (if (or (member (first x) '(:neg :not))
                                                                 (not (member (first x)
                                                                              '(:and :or :cup :cap :intersection :union))))
                                                             (list :or x)
                                                           x)))
                                                   args))
                                     (args (remove-duplicates args :test #'equal))
                                     (or-terms (remove-if-not #'(lambda (x)
                                                                  (member (first x) '(:or :union :cup)))
                                                              args))
                                     (non-or-terms (remove-if #'(lambda (x)
                                                                  (member (first x) '(:or :union :cup)))
                                                              args)))
                                (simplify-boolean-expression
                                 (if or-terms
                                     (let ((crosprod (newprod
                                                      (mapcar #'rest or-terms))))
                                       (cons :or
                                             (mapcar #'(lambda (or-term)
                                                         (simplify-boolean-expression
                                                          (cons :and (append non-or-terms
                                                                             or-term))))
                                                     crosprod)))
                                   (cons :and args)))))
                             (t expr)))))
               expr)))

    (dnf expr)))


;;;
;;;

(defun permutations (list &optional (n (length list)) (i 0))
  (perm list n i))

(defun perm (list &optional (n (length list)) (i 0))
  ;;; choose "k" out of "n"
  (if (= i n)
      '(nil)
    (remove-duplicates
     (mapcan #'(lambda (a)
                 (let ((rest (remove a list :count 1)))
                   (mapcar #'(lambda (rest)
                               (cons a rest))
                           (perm rest n (1+ i)))))
             list)
     :test #'equal)))


;;;
;;;
;;;


(defun to-keyword (x)
  (intern (format nil "~A" x) :keyword))

(defun to-symbol (x)
  (intern (format nil "~A" x)))


(defun to-keyword-big (x)
  (if (symbolp x)
      (intern (string-upcase (format nil "~A" x)) :keyword)
    (mapcar #'(lambda (x)
		(intern (string-upcase (format nil "~A" x)) :keyword))
	    x)))

(defun to-keyword-small (x)
  (if (symbolp x)
      (intern (string-downcase (format nil "~A" x)) :keyword)
    (mapcar #'(lambda (x)
		(intern (string-downcase (format nil "~A" x)) :keyword))
	    x)))


;;;
;;;
;;;

(defun smallest (list &optional (fn #'identity))
  (when list
    (let ((found nil)
          (val nil))
      (dolist (x list)
        (when x
          (unless found
            (setf found x))
          (unless val
            (setf val (funcall fn x)))
          (let ((fn-val (funcall fn x)))
            (if (< fn-val val)
                (setf found x
                      val fn-val)))))
      found)))

(defun greatest (list &optional (fn #'identity))
   (when list
    (let ((found nil)
          (val nil))
      (dolist (x list)
        (when x
          (unless found
            (setf found x))
          (unless val
            (setf val (funcall fn x)))
          (let ((fn-val (funcall fn x)))
            (if (> fn-val val)
                (setf found x
                      val fn-val)))))
      found)))

;;;
;;;
;;;


(defun blank-line-p (line)
  (or (eq line 'newline)
      (and (typep line 'string)
	   (not (position-if-not #'(lambda (i) (char= i #\space)) line)))))


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


(defun shrink-whitespaces (string)
  (if (stringp string)
      (if (every #'whitespace-char-p string)
          ""
        (let ((pos (or (position-if-not #'whitespace-char-p string) 0))
              (pos2 (or (position-if-not #'whitespace-char-p string :from-end t)
                        (1- (length string)))))
          (subseq string pos (1+ pos2))))
    string))


(defun whitespace-char-p (char)
  #+:lispworks
  (lispworks:whitespace-char-p char)
  #+(and :allegro (not (or :allegro-v9.0 :allegro-v10.0)))
  (stream::whitespace-char-p char)
  #+(and :allegro (or :allegro-v9.0 :allegro-v10.0))
  (excl::whitespace-char-p char)
  #+:sbcl
  ;;(find char SB-FORMAT::*FORMAT-WHITESPACE-CHARS*)
  (sb-unicode:whitespace-p char)
  #+:ccl (ccl:whitespacep char)
  #+(and (not :allegro) (not :lispworks) (not :sbcl) (not :ccl))
  (to-be-implemented 'whitespace-char-p))

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
