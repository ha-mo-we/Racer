;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: OWL-SYNTAXES; Base: 10 -*-

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

(in-package :owl-syntaxes)

;;;
;;;;  owl-functional.lisp
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
;;;   Purpose: Parser for OWL 2 functional syntax. 
;;; 

(define-constant +start-sym+ (gensym))

(define-constant +end-sym+ (gensym))

(define-constant +continue-sym+ (gensym))

(define-constant +ano-ind-prefix+ '|@#?ano|)

(define-constant +xsd-uri+
  #+:racer-server
  nox:-xsd-uri-
  #-:racer-server
  "http://www.w3.org/2001/XMLSchema#")

;;;
;;;
;;;

(defvar *use-owlapi-flipped-class-assertions-p* nil)

(defvar *strict-owl-functional* nil)

(defvar *ano-ind-counter* 0)

(defvar *merge-imported-ontologies-p* nil)

;; (defvar *indent* 0) inherited from racer

;;;
;;;
;;;

(defvar *use-xml-base* nil)

(defvar *owl-xml-mode* nil)

;;;
;;;
;;; 

(defvar *producer* nil)

(defvar *use-chopper-p* nil)

(defclass owl-producer ()
  ((char-count      :initform 0)
   (has-more-p      :initform t)
   (eof-p           :initform nil)
   (stream-closed-p :initform nil)
   (stream          :initarg :stream)

   (n               :initarg :n)
   (m               :initarg :m)
   
   (cdr-stack              :initform (list (list +start-sym+)))
   (first-token-p          :initform t)
         
   (token                  :initform nil)
   (literal                :initform nil)
   (last-was-whitespace-p  :initform nil)
   (reading-comment-p      :initform nil)
   (single-escape-p        :initform nil)
   (reading-string-p       :initform nil)
   (reading-number-p       :initform nil)
   (reading-uri-p          :initform nil)
   (reading-type-p         :initform nil)
   (reading-language-tag-p :initform nil)     
          
   (g-cell                 :initform nil)
   (first-cell             :initform nil)))

(defun get-owl-producer (stream)
  (let* ((n (or 
	     (if (typep stream 'file-stream)
		 (file-length stream)
	       0)))
         (m (floor (/ n 100)))
	 (m (if (zerop m) 1 m))
         (producer 
          (make-instance 'owl-producer 
                         :stream stream
                         :n n
                         :m m)))
    producer))

(defun get-token (self)
  (with-slots (has-more-p
               stream-closed-p
               stream
               char-count
               m
               n
               first-token-p
               token
               literal
               last-was-whitespace-p
               reading-comment-p
               single-escape-p
               reading-string-p
               reading-number-p
               reading-uri-p
               reading-type-p
               reading-language-tag-p) self

    (declare (ignorable m n))
      
    (macrolet ((get-char (&optional keep-in-buffer-p)
             
                 `(unless stream-closed-p 
                    (let ((char (read-char stream nil nil)))
                   
                      (when char
                        (when ,keep-in-buffer-p
                          (unread-char char stream)))
                     
                      (cond (char
                             (incf char-count)

                             #+:racer-server
                             (when (zerop (mod char-count m))
                               (set-progress-value
                                (file-position stream)))

                             (setf has-more-p t) 
                             
                             #+:ignore
                             (setf has-more-p
                                   (or ,keep-in-buffer-p
                                       (< (file-position stream) n)))

                             char)
                            
                           
                            (t (setf stream-closed-p t)
                               (setf has-more-p nil)
                               (close stream)
                              
                               nil)))))
               
               (next-char ()
                 `(get-char t))

               (clear ()
                               
                 `(prog1
                      (if reading-string-p
                          (shrink-whitespaces (coerce (nreverse token) 'string))
                        (when token
                          (cond 
                           (reading-number-p
                            (read-from-string (coerce (nreverse token) 'string)))
                           (reading-type-p 
                            `(,+start-sym+ 
                              |OWLLiteral| 
                              ,(shrink-whitespaces (coerce (nreverse token) 'string))
                              ,literal 
                              :no-language-tag
                              ,+end-sym+))
                           (reading-language-tag-p 
                            `(,+start-sym+
                              |OWLLiteral| 
                              :no-type
                              ,literal 
                              ,(shrink-whitespaces (coerce (nreverse token) 'string))
                              ,+end-sym+))
                           (t
                            (intern (shrink-whitespaces (coerce (nreverse token) 'string)))))))
                    (setf token nil
                          literal nil
                          reading-number-p nil
                          reading-comment-p nil
                          single-escape-p nil
                          reading-type-p nil
                          reading-language-tag-p nil
                          reading-string-p nil
                          reading-uri-p nil))))

      
      (cond (first-token-p 
             
             #+:racer-server
             (set-progress-100% n)
             
             (setf first-token-p nil)
             (values nil :open))
			    
            (t 
                           
             (loop 

              (let ((char (get-char)))

                (cond ((not char)
                       (return (values nil :eof)))

                      (t
                                     
                       (cond (single-escape-p 
                              (setf single-escape-p nil
                                    last-was-whitespace-p nil)
                              (push char token))
                
                             ((char= char #\\)
                              (setf single-escape-p t
                                    last-was-whitespace-p nil))
                              
                             ((and (char= char #\/) ; Kommentar!
                                   (char= (next-char) #\/)
                                   (not reading-uri-p)
                                   (not reading-string-p))

                              (setf last-was-whitespace-p nil
                                    reading-comment-p t)

                              (when token
                                (return-from nil 
                                  (values (clear) :comment))))
                     
                             (reading-comment-p 

                              (setf reading-comment-p nil)
                                              
                              (loop 
                               (let ((char (next-char)))
                                 (when (or (not char) 
                                           (char= char #\Newline))
                                   (return))
                                 (get-char))))
                    
                             ((and (owlapi:whitespace-char-p char)
                                   (not reading-string-p))
                  
                              (setf last-was-whitespace-p t)
                              (let ((tk (clear)))
                                (when tk
                                  (return-from nil tk))))

                             ((and (digit-char-p char)
                                   (not reading-number-p)
                                   (not reading-string-p)
                                   (not reading-uri-p)
                                   (not token))

                              (let ((tk (clear)))

                                (setf last-was-whitespace-p nil
                                      reading-number-p t)

                                (push char token)

                                (when tk
                                  (return-from nil tk))))

                             ((and (char= char #\") 
                                   reading-string-p)

                              (let ((next (next-char)))
                                (cond ((or (char= next #\^)
                                           (char= next #\@))

                                       (setf reading-string-p nil))

                                      (t

                                       (let ((tk (clear)))
                                         (setf last-was-whitespace-p nil)
                                 
                                         (when tk 
                                           (return-from nil tk)))))))
                                 
                             ((and (char= char #\") 
                                   (not reading-string-p))

                              (setf reading-string-p t
                                    last-was-whitespace-p nil))

                             ((and (char= char #\>) 
                                   reading-uri-p)

                              (unless (char= (next-char) #\()
                                (let ((tk (clear)))
                                  (setf last-was-whitespace-p nil)
                                  (when tk 
                                    (return-from nil tk)))))

                             ((and (char= char #\<) 
                                   (not reading-uri-p)
                                   (not reading-string-p))

                              (setf reading-uri-p t
                                    last-was-whitespace-p nil))

                             ((and (char= char #\^)
                                   (char= (next-char) #\^)
                                   (not reading-string-p))
                              (get-char)
                              (setf reading-string-p t)
                              (let ((tk (clear)))
                                (setf reading-string-p nil)
                                (setf literal tk
                                      reading-type-p t
                                      last-was-whitespace-p nil)))

                             ((and (char= char #\@)
                                   (not reading-string-p))
                              (setf reading-string-p t)
                              (let ((tk (clear)))
                                (setf reading-string-p nil)
                                (setf literal tk
                                      reading-language-tag-p t
                                      last-was-whitespace-p nil)))

                             ((and (char= char #\()
                                   (not reading-string-p))
                              (let ((tk (clear)))
                                (setf last-was-whitespace-p nil)
                                (return-from nil (values tk :open))))
                          
                             ((and (char= char #\))
                                   (not reading-string-p))
                              (let ((tk (clear)))
                                (setf last-was-whitespace-p nil)
                                (return-from nil (values tk :close))))

                             (t 
                              (setf last-was-whitespace-p nil)
                              (push char token))))))))))))


(defun get-expression (self)

  (with-slots (has-more-p
               g-cell
               first-cell
               cdr-stack
               eof-p) self
      
    (cond (eof-p 

           :eof)
		       
          (t
                      
           (multiple-value-bind (token bracket-or-comment)
               (get-token self)

             ;; (pprint token)
             ;; (pprint first-cell)
                            
             (case bracket-or-comment
                               
               (:eof 
                (setf eof-p t))
                               
               (:comment 

                (get-expression self))

               (:open

                (let ((cell 
                       (if has-more-p
                           (if token
                               (list +start-sym+ token +continue-sym+)
                             (list +start-sym+ +continue-sym+))
                         (if token
                             (list +start-sym+ token)
                           (list +start-sym+)))))

                  (when cdr-stack 
                    (let ((cell 
                           (list cell +continue-sym+)))
                      (setf (cdr (last (first cdr-stack)))
                            cell)
                      ;; advance last pointer
                      ;; keep "last" efficent (small list)
                      (setf (first cdr-stack)
                            cell)))

                  (setf g-cell cell)               
                  (push cell cdr-stack)))

               (:close 
                       
                (let ((cell (if token
                                (list token +end-sym+)
                              (list +end-sym+))))
                                 
                  (when cdr-stack 
                    (setf (cdr (last (first cdr-stack)))
                          cell))
                                 
                  (setf g-cell cell))
                               
                (pop cdr-stack))

               (otherwise

                (let ((cell 
                       (if has-more-p
                           (list token +continue-sym+)
                         (list token))))

                  (when cdr-stack 
                    (setf (cdr (last (first cdr-stack)))
                          cell)
                    ;; advance last pointer
                    ;; keep "last" efficent (small list)
                    (setf (first cdr-stack)
                          cell))

                  (setf g-cell cell)))))

           (cond (eof-p
                  (when (eq (first (last (first cdr-stack))) +continue-sym+)
                    (setf (cdr (last (first cdr-stack))) (list +end-sym+)))

                  :eof)

                 (t 
                  (unless first-cell 
                    (setf first-cell (list self g-cell)))

                  first-cell))))))

;;;
;;;
;;;

(defmacro f-list-p (arg)
  (let ((g (gensym)))
  `(let ((,g ,arg))
     (and (consp ,g)
          (typep (first ,g) 'owl-producer)
          (cdr ,g)))))

(defmacro f-first (arg)
  (let ((g (gensym)))
    `(let ((,g ,arg))

       (cond 

        ((f-list-p ,g)
         (let* ((self (first ,g))
                (arg (second ,g))
                (pos
                 (position-if-not
                  #'(lambda (x) 
                      (or (eq x +start-sym+)
                          (eq x +continue-sym+)))
                  arg))

                (return 
                 (cond (pos ; token oder +end-sym+
                        (let ((item (nth pos arg)))
                          (if (eq item +end-sym+)
                              nil
                            item)))

                       ((not arg) nil)
                       
                       (t 
                        (let ((last (first (last arg)))
                              (n (length arg)))
                          
                          (unless (eq last +continue-sym+)
                            (break "f-first problem: ~S ~S!" arg last))

                          (loop 
                           (get-expression self)
                           (let ((res 
                                  (first 
                                   (nthcdr n arg))))
                             (when res 
                               (return
                                (if (eq res +end-sym+)
                                    nil
                                  res))))))))))

           (if (consp return)
               (list self return)
             return)))

        (t (first ,g))))))

(defmacro f-rest (arg)
  (let ((g (gensym)))
    `(let ((,g ,arg))

       (cond 

        ((f-list-p ,g)
         (let* ((self (first ,g))
                (arg (second ,g))
                (pos
                 (position-if-not
                  #'(lambda (x) 
                      (or (eq x +start-sym+)
                          (eq x +continue-sym+)))
                  arg))
                (return
                 (cond (pos ; token oder +end-sym+
                        (let ((item (nth pos arg)))
                          (if (eq item +end-sym+)
                              nil
                            (let ((item (nthcdr (1+ pos) arg)))
                              (cond ((eq (first item) +end-sym+)
                                     nil)
                                    ((eq (first item) +continue-sym+)
                                     (loop 
                                      (get-expression self)
                                      (let ((item (nthcdr (+ 2 pos) arg)))
                                        (when item
                                          (return 
                                           (if (eq (first item) +end-sym+)
                                               nil
                                             item))))))
                                    (t 
                                     item))))))

                       ((not arg) nil)

                       (t 
                        (let ((last (first (last arg)))
                              (n (length arg)))

                          (unless (eq last +continue-sym+)
                            (break "f-rest problem: ~S ~S!" arg last))

                          (loop 
                           (get-expression self)
                           (let ((res 
                                  (nthcdr (1+ n) arg)))
                             (when res 
                               (return res)))))))))
          
           (if (consp return)
               (list self return)
             return)))

        (t (rest ,g))))))

(defmacro f-second (arg)
  (let ((g (gensym)))
    `(let ((,g ,arg))
       (f-first ,g)
       (f-first (f-rest ,g)))))

(defmacro f-third (arg)
  (let ((g (gensym)))
    `(let ((,g ,arg))
       (f-second ,g)
       (f-first (f-rest (f-rest ,g))))))

(defmacro f-fourth (arg)
  (let ((g (gensym)))
    `(let ((,g ,arg))
       (f-third ,g)
       (f-first (f-rest (f-rest (f-rest ,g)))))))

(defmacro f-consp (arg)
  (let ((g (gensym)))
    `(let ((,g ,arg))
       (cond ((f-list-p ,g)
              (let ((self (first ,g))
                    (arg (second ,g)))
                (declare (ignorable self))
                (cond ((consp arg)
                       (let* ((pos
                               (position-if-not
                                #'(lambda (x) 
                                    (or (eq x +start-sym+)
                                        (eq x +continue-sym+)))
                                arg)))
                         (cond (pos ; end-sym or token 
                                (let ((token
                                       (nth pos arg)))
                                  (if (eq token +end-sym+)
                                      nil
                                    t)))

                               (t ; start-sym or continue-sym
                                (let ((token
                                       (first (last arg))))

                                  (unless (eq token +continue-sym+)
                                    (break "f-consp problem: ~S ~S!" arg token))

                                  (loop 
                                   (get-expression self)
                                   (when (first (last arg))
                                     (return)))
                                  (not (eq (first (last arg)) +end-sym+)))))))
                      (t nil))))
             (t (consp ,g))))))

(defmacro f-null (arg)
  (let ((g (gensym)))
    `(let ((,g ,arg))
       (cond ((f-list-p ,g)
              (null (f-first ,g)))
             (t (null ,g))))))


(defmacro f-length (arg)
  (let ((g (gensym)))
    `(let ((,g ,arg)) 
       (cond ((f-list-p ,g)
              (let ((count 0))
                (loop 
                 (let ((first (f-first ,g)))
                   (cond (first
                          (incf count)
                          (setf ,g (f-rest ,g))
                          (unless ,g (return)))
                         (t (return)))))
                count))
             (t (length ,g))))))

;;;
;;;
;;;

(defun make-url-from-filename (fn)
  (let ((fn 
         (if (symbolp fn)
             (symbol-name fn)
           fn)))
    (owlapi:string-substitute 
     (if (owlapi:is-url-p fn)
	 fn
       (let ((pos (search "~/" fn)))
         ;;; ~ not sufficent here - Windows: "C:\\DOKUME~1\\.." 
	 (if pos
	     (format nil 
		     "file://~A~A~A"
		     (subseq fn 0 pos)
		     (let ((home
			    (namestring (user-homedir-pathname))))
		       (subseq home 0 (1- (length home))))
		     (subseq fn (1+ pos)))
	   (format nil "file://~A" fn))))
     '(("\\" "/")))))

;;;
;;;
;;;

(defun is-ano-ind-p (ind)
  (let* ((ind (symbol-name ind))
         (n (length ind)))
    (and (> n 6)
         (string= (symbol-name +ano-ind-prefix+)
                  (subseq ind 0 6)))))

(defun without-ano-ind-prefix (ind)
  (let ((ind (symbol-name ind)))
    (intern (let ((pos 
                   (position #\* ind :from-end t)))
              (subseq ind 6 pos)))))


;;;
;;;
;;;

(defmacro check-for (args type)
  `(or (eq (f-first (f-first ,args)) ',type)
       (eq (f-first (f-first ,args)) 
           (quote ,(intern (format nil "owl.~A" type))))))

(defun remove-sharp-brackets (string)
  (if (string-equal string "")
      string
    (progn
      (when (char= (elt string 0) #\<)
        (setf string (subseq string 1)))
      (when (char= (elt string (1- (length string))) #\>)
        (setf string (subseq string 0 (1- (length string)))))
      string)))

(defun parse-iri (args &optional ano-ind-p)
  (with-slots (id-to-object-table) *cur-reasoner*

    (cond ((and (f-first args)
                (symbolp (f-first args)))
         
           (let* ((name (symbol-name (f-first args)))
                  (name (remove-sharp-brackets name))

                  (name (if (char= #\: (elt name 0))
                            (subseq name 1)
                          name))

                  (rest (f-rest args))

                  (pos (position #\: name))
                  (name1 (if pos
                             (subseq name (1+ pos))
                           name)))

             (if (or ano-ind-p 
                     (string= "_" (subseq name 0 pos))) ; ano ind? 

                 (values (or (gethash name id-to-object-table)

                             (setf (gethash name id-to-object-table) 
                                   (intern
                                    (format nil "~A~A*~A" 
                                            +ano-ind-prefix+
                                            name1
                                            ;;; must be global! 
                                            (incf *ano-ind-counter*)))))
                                        
                         (f-rest args))

               (let* ((prefix 
                       (when pos
                         (owlapi:to-keyword (subseq name 0 (1+ pos))))))

                 (case prefix

                   ((:|http:| :|file:|)

                    (values (intern (owlapi:ensure-string name)) rest))

                   (otherwise
                    
                    (values 

                     (if (and *use-xml-base*
                              ;;; may be NIL 
                              ;;; use xml-base only as prefix if no default prefix 
                              ;;; was defined  
                              (not (owlapi:expand-prefix prefix)))
                         (intern (format nil "~A~A" *use-xml-base* name1))
                       (owlapi:make-uri (or prefix
                                     :defaultnamespace)
                                 name1))

                     rest)))))))
          
          ((and (stringp (f-first args))
                (char= #\< (elt (f-first args) 0)))
           
           (let ((string (f-first args)))
           
             (values 
              (remove-sharp-brackets string)
              (f-rest args))))

          ((stringp (f-first args))

           (parse-iri (cons (intern (f-first args)) 
                            (f-rest args))
                      ano-ind-p))

          ((and (f-consp (f-first args))
                (member (f-first (f-first args)) '(IRI |abbreviatedIRI|
                                                   |AbbreviatedIRI|
                                                   |abreviatedIRI|
                                                   |AbreviatedIRI|
                                                   |datatypeIRI|
                                                   |DatatypeIRI|)))

           (parse-iri (cons (f-second (f-first args))
                            (f-rest args))
                      ano-ind-p))

          (t 

           (values nil args)))))

(defun parse-literal (args)

  (if (and (f-consp args)
           (member (f-first args) '(|Literal|
                                    |Constant|)))

      ;;; wants a LIST of Literals... 
      (progn 
        (parse-literal (list args)))

    (let* ((args (if (stringp args)
                     (list args)
                   args))

           (val (f-first args))
           (rest (f-rest args))

           (val2 nil)
           (language nil)
           (type nil))

      (declare (ignorable language))

      (cond ((and (f-consp val)
                  (member (f-first val) '(|Literal|
                                          |Constant|))
                  (= (f-length val) 3))
             
             (if (and (f-consp (f-second val))
                      (equal (butlast (f-second val))
                             '(|Attribute| |datatypeIRI|)))
                 (progn 
                   (setf type (f-third (f-second val))
                         val2 (f-third val)))

               (progn 
                 (setf type (f-second val))
                 (setf val2 (f-third val))) ))

            ((and (f-consp val)
                  (member (f-first val) '(|Literal|
                                          |Constant|))
                  (= (f-length val) 2))

             (setf type nil)
             (setf val2 (f-second val)))

            ((and (f-consp val)
                  (member (f-first val) '(|OWLLiteral|))
                  (= (f-length val) 3))

             (setf type (f-third val))
             (setf val2 (f-second val)))

            ((and (f-consp val)
                  (member (f-first val) '(|OWLLiteral|))
                  (= (f-length val) 4))
             
             (setf language (f-fourth val))
             (setf type (if (not (eq language :no-language-tag))
                            "xsd:string"
                          (f-second val)))
             (setf val2 (f-third val)))
       
            ((stringp val)
             #|
             (if val-type
                 (let* ((pos1 (search "^^" val-type)))
                   (when pos1
                     (setf type 
                           (when pos1 
                             (subseq val-type (+ 2 pos1))))

                     (when pos1
                       (setf rest (f-rest rest)))
                     
                     (setf val2 val)))
             
               (let* ((pos1 (position #\^ val))
                      (pos2 (position #\@ val))
                      (args (remove nil (list pos1 pos2)))
                      (pos (when args (apply #'min args))))
         
                 (setf type 
                       (when pos1 
                         (subseq val (+ 2 pos1))))
                 
                 (setf val2 
                       (if pos
                           (subseq val 0 pos)
                         val)))) |# 

             (setf val2 val))

            (t (return-from parse-literal 
                 (values nil args))))
      
      (values `(d-literal ,val2 ,(convert-owl-base-type type))
              rest))))

(defun convert-owl-base-type (type)
  (cond ((not type)
         nil ; "Racer Auto Typing"
         )
        ((and (> (length type) 5)
              (string-equal "<http:" (subseq type 0 6)))             
         `(d-base-type 
          ,(intern 
            (subseq type 1 (1- (length type))))))
        ((and (> (length type) 4)
              (string-equal "http:" (subseq type 0 5)))
         `(d-base-type 
           ,(intern 
             (subseq type 0 (length type)))))
        (t
         (let* ((pos (position #\: type))
                (res 
                 `(d-base-type 
                   ,(intern 
                     (if pos
                         (concatenate 'string
                                      +xsd-uri+
                                      (subseq type (1+ pos)))
                       (concatenate 'string
                                    +xsd-uri+
                                    type))))))
           res))))

(defun convert-owl-data-range (owl-range)
  (cond ((symbolp owl-range)
         ;;; datatypeURI
         `(d-base-type ,owl-range))
        ((consp owl-range)
         (let ((op (first owl-range)))
           (ecase op 
             (|DataComplementOf| 
              `(d-complement 
                ,(convert-owl-data-range (second owl-range))))
             (|DataIntersectionOf| 
              `(d-and 
                ,@(mapcar #'convert-owl-data-range (rest owl-range))))
             (|DataUnionOf| 
              `(d-or 
                ,@(mapcar #'convert-owl-data-range (rest owl-range))))
             (|DataOneOf|
              `(d-possible-values 
                ,@(mapcar #'convert-owl-data-range (rest owl-range))))
             (|DatatypeRestriction| 
              (let* ((basetype (convert-owl-data-range (second owl-range)))
                     (facets-and-values (cddr owl-range))
                     (owl-facets-and-values
                      (loop as facet in facets-and-values by #'cddr 
                            as value in (cdr facets-and-values) by #'cddr 
                            collect (convert-owl-data-facet facet value))))
                `(d-restriction (d-base-type ,basetype)
                                ,@owl-facets-and-values)))
             (t (owlapi:owlapi-parser-error "Bad data range: ~A" owl-range)))))

        (t owl-range)))

(defun convert-owl-data-facet (facet value)
  (if (owlapi:is-url-p facet)
      `(d-facet 
        ,(if (stringp facet)
             (intern facet)
           facet)
        ,(if (consp value)
             value
           `(d-literal ,value nil)))

    (multiple-value-bind (prefix postfix) 
        (owlapi:get-prefix-postfix facet)

      (declare (ignorable prefix))
    
      `(d-facet 
        ,(ecase postfix
           (|length|         (intern (format nil "~Alength"         +xsd-uri+)))
           (|minLength|      (intern (format nil "~AminLength"      +xsd-uri+)))
           (|maxLength|      (intern (format nil "~AmaxLength"      +xsd-uri+)))
           (|pattern|        (intern (format nil "~Apattern"        +xsd-uri+)))
           (|minInclusive|   (intern (format nil "~AminInclusive"   +xsd-uri+)))
           (|maxInclusive|   (intern (format nil "~AmaxInclusive"   +xsd-uri+)))
           (|minExclusive|   (intern (format nil "~AminExclusive"   +xsd-uri+)))
           (|maxExclusive|   (intern (format nil "~AmaxExclusive"   +xsd-uri+)))
           (|totalDigits|    (intern (format nil "~AtotalDigits"    +xsd-uri+)))
           (|fractionDigits| (intern (format nil "~AfractionDigits" +xsd-uri+))))

        ,(if (consp value)
             value
           `(d-literal ,value nil))))))

;;;
;;;
;;; 

(defun create-annotations (constructor annotations)
  (let ((prev nil))
    (dolist (annotation-annotations annotations)
      (dolist (annotation annotation-annotations)
        (setf prev
              (if prev
                  (|OWLAPI-getOWLAxiomAnnotationAxiom| prev annotation)
                (funcall constructor annotation)))))))

;;;
;;;
;;; 

(defun parse-ontology-document (args &key uri (error-p t))
  (let ((ontology nil)
        (namespaces nil)
        (additional-namespaces nil))        

    (cond ((f-consp args) 

           (multiple-value-setq (namespaces args)
               (parse-ontology-prefixes args))

           (multiple-value-setq (ontology additional-namespaces 
                                          ;;; for OWL XML -> OWL Functional Converter
                                          )
               (parse-ontology args :uri uri :namespaces namespaces))

           (unless ontology
             (when error-p
               (owlapi:owlapi-parser-error "parse-ontology-document: no ontology in ~A" args))

             (return-from parse-ontology-document 
               nil))

           (values ontology
                   (append namespaces additional-namespaces)))

          (t (values nil args)))))

;;;
;;;
;;; 

(defun parse-ontology-prefixes (args)
  (let ((namespace nil)
        (namespaces nil)
        (done nil))

    (loop while (and args (not done) (f-consp args)) do
          (setf done t)
          (multiple-value-setq (namespace args)
              (parse-prefix-declaration args))
          (when namespace
            (push namespace namespaces)
            (setf done nil)))

    (values namespaces args)))


(defun parse-prefix-declaration (args &optional (error-p t))
  
  (labels ((register-prefix (prefix ns)
             (let* ((prefix (owlapi:ensure-string prefix))
                    (ns (owlapi:ensure-string ns))
                    (prefix 
                     (when (and prefix
                                (not (string= prefix "")))
                       (owlapi:ensure-ends-with-colon prefix)))
                    
                    (ns (remove-sharp-brackets ns))
                    (hte (if (and prefix
                                  (not (string= prefix "")))
                             (list prefix ns)
                           (list :defaultnamespace ns))))
               
               (|OWLAPI-getOWLPrefixDeclarationAxiom| prefix ns)

               (values (cons '|Namespace| hte) 
                       (f-rest args)))))

    (if (and (f-consp (f-first args))
             (or (check-for args |Namespace|)
                 (check-for args |Prefix|)))

        (cond ((= 2 (f-length (f-first args)))

               (cond ((consp (f-second (f-first args)))
                      
		      ;;; Syntax we get from OWL XML -> OWL Functional 

                      (let ((prefix 
                             (second 
                              (assoc '|name| 
                                     (f-second (f-first args)))))
                            (ns
                             (second 
                              (assoc '|IRI|
                                     (f-second (f-first args))))))
			
                        (unless ns 
                          (when error-p
                            (owlapi:owlapi-parser-error "parse-namespace: no namespace in ~A" args))
                          (return-from parse-prefix-declaration 
                            (values nil (f-rest args))))

                        (register-prefix prefix ns)))

                     (t
      
                      (let ((ns (symbol-name (f-second (f-first args)))))

                        (unless ns
                          (when error-p
                            (owlapi:owlapi-parser-error "parse-namespace: no namespace in ~A" args))
                          (return-from parse-prefix-declaration 
                            (values nil (f-rest args))))
        
                        (let* ((pos (position #\= ns))
                               (prefix (and pos (subseq ns 0 pos)))
                               (ns (or (and pos (subseq ns (1+ pos)))
                                       ns)))

                          (register-prefix prefix ns))))))

              ((member '= (f-first args))

               (let* ((pos (position '= (f-first args)))
                      (prefix (first (cdr (subseq (f-first args) 0 pos))))
                      (ns     (first (cdr (subseq (f-first args) pos)))))

                 (register-prefix (when prefix (symbol-name prefix))
                                  (symbol-name ns))))

              ((= 3 (length (f-first args)))
      
               (register-prefix (f-second (f-first args))
                                (f-third (f-first args))))

              
              ((and (= 4  (f-length (f-first args)))
                    (eq '= (f-third (f-first args))))

               ;;; OWLAPI 3.3: now correctly uses spaces
               ;;; Prefix( owl: = <http://www.w3.org/2002/07/owl#> )
               ;;; Prefix( : = <http://example.org/> )

               (register-prefix (f-second (f-first args))
                                (f-fourth (f-first args))))

              (t 
               (when error-p
                 (owlapi:owlapi-parser-error "parse-namespace: no namespace in ~A" args))

               (return-from parse-prefix-declaration 
                 (values nil (f-rest args)))))
               
      (values nil args))))


(defun parse-ontology (args &key uri (error-p t) namespaces)

  ;;; (setf *x* args)
  
  (if (and (f-consp (f-first args))
           (check-for args |Ontology|))

      (let ((args (f-rest (f-first args)))
            (imports nil)
            (annotations nil)
            (axioms nil)
            (uri1 nil)
            (uri2 nil)
            (version nil)

            (additional-namespaces nil))

        (declare (ignorable imports annotations  
                            uri2
                            axioms version))
        
        (multiple-value-setq (uri1 args)
            (parse-ontology-iri args))

        (setf uri (or uri1 uri))

        ;;; Reasoner name = file name
        ;;; Ontology name = from ontology file 
        ;;; Consistent with owl-read-file/document behaviour

        #| 

        (when (=> *merge-imported-ontologies-p*
                  (not (cdr *imported-ontologies*)))

          (if (owlapi:find-owl-ontology uri)
              
              (progn 

                (|OWLAPI-mergeOntologies| (|OWLAPI-getAutoOntology|) uri)
                (owlapi:set-primary-ontology-name (|OWLAPI-getAutoOntology|)
                                       uri))

            ;;; wenn merge, dann bestimmt die 1. Ontology 
            ;;; den primary ontology name
          
            (owlapi:set-primary-ontology-name (|OWLAPI-getAutoOntology|)
                                       uri)))

        |# 

        (if (owlapi:find-owl-ontology uri nil)
              
            (progn 

              (|OWLAPI-mergeOntologies| (|OWLAPI-getAutoOntology|) uri)
              (owlapi:set-primary-ontology-name (|OWLAPI-getAutoOntology|)
                                         uri))

          (let ((auto (|OWLAPI-getAutoOntology|)))
            (if (eq auto :void)
                (progn 
                  ;;; may be caused by parseNative (Ontology) 
                  (owlapi-warning "No ontology container specified. 
Creating ontology container
   ~A 
in reasoner container 
   ~A. 

Defined prefixes will be ignored - this can be avoided
by creating the reasoner and ontology containers manually 
beforehand, and enabling auto-addition of axioms to it." uri uri)
                  (|OWLAPI-newReasoner| uri)
                  (|OWLAPI-newOntology| uri uri)
                  (|OWLAPI-autoAddAxiomsTo| uri uri))
              (owlapi:set-primary-ontology-name auto uri))))

        ;;;
        ;;;
        ;;; 
         
        (unless uri
          (when error-p 
            (owlapi:owlapi-parser-error "parse-ontology: no ontology URI in ~A" uri))
          (return-from parse-ontology 
            (values nil (f-rest args))))

        ;;; Syntax we get from OWL XML -> OWL Functional transformation
        
        (multiple-value-setq (additional-namespaces args)
            (parse-ontology-prefixes args))

        ;;;
        ;;;
        ;;; 

        (when (and (not (find-if #'owlapi:is-default-prefix-p namespaces
                                 :key #'second))
                   (not (find-if #'owlapi:is-default-prefix-p additional-namespaces
                                 :key #'second)))

          (owlapi-warning "No default OWL \"Prefix\" found. Using ontology name ~A" uri)
               
          (|OWLAPI-getOWLPrefixDeclarationAxiom|
           :defaultnamespace
           (if (char= #\# (elt (symbol-name uri) (1- (length (symbol-name uri)))))
               uri
             (intern (format nil "~A#" uri)))))

        ;;;
        ;;;
        ;;; 

        (multiple-value-setq (uri2 args)
            (parse-ontology-version-iri args))


        (multiple-value-setq (imports args)
            (parse-directly-imports-documents args))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (create-annotations (lambda (x) 
                              (|OWLAPI-getOWLOntologyAnnotationAxiom| x))
                            annotations)

        (multiple-value-setq (axioms args)
            (parse-axioms args))

        (values uri additional-namespaces))

    (values nil args)))

(defun parse-ontology-iri (args)
  (parse-iri args))

(defun parse-ontology-version-iri (args)
  (multiple-value-bind (iri args)
      (parse-iri args)

    (when iri
      (|OWLAPI-getOWLOntologyVersionDeclarationAxiom| 
       iri))
    
    (values iri args)))


(defun parse-directly-imports-documents (args &optional (error-p t))
  (let ((done nil)
        (import nil)
        (imports nil))
    
    (labels ((parse-directly-imports-document (args)
               (if (and (f-consp (f-first args))
                        (check-for args |Import|))

                   (let* ((uri2
                           (parse-iri 
                            (list 
                             (f-second (f-first args)))))
                          
			  #+:ignore
			  (uri (check-for-url-mirror uri2))
			  
			  (uri uri2))
                     
                     (unless (equalp uri uri2)
                       (format t "~V@TURL ~A mirrored to ~A." 
                               #+:racer-server *indent* 
                               #-:racer-server *indent* 
                               uri uri2))

                     (unless uri
                       (when error-p
                         (owlapi:owlapi-parser-error "parse-import: no URI in ~A" args))
                       (return-from parse-directly-imports-documents
                         (values nil (f-rest args))))
        
                     (let ((axiom
                            (when (or (and (not *merge-imported-ontologies-p*)
                                           (not (member uri *imported-ontologies*)))
                                      *ignore-import-p*)
                              (|OWLAPI-getOWLImportsDeclarationAxiom| 
                               uri)))
                           (ontology (|OWLAPI-getAutoOntology| *cur-reasoner*)))                           

                       (owlapi-import-ontology uri
                                               :maintain-owlapi-axioms 
                                               (not (owlapi:dont-keep-axioms-p *cur-reasoner*))
                                               :ontology-name (when *merge-imported-ontologies-p*
                                                                ontology)
                                               :reasoner-name (owlapi:owlapi-reasoner-name *cur-reasoner*)
                                               :init nil)

                       (values axiom (f-rest args))))

                 (values nil args))))

      (loop while (and args (not done) (f-consp args)) do

            (setf done t)

            (multiple-value-setq (import args)
                (parse-directly-imports-document args))

            (when import
              (push import imports)
              (setf done nil)))

      (values imports args))))

(defun parse-axioms (args)
  (let ((done nil)
        (axiom nil)
        (axioms nil)
        (oargs args))
  
    (loop while (and args (not done) (f-consp args)) do

	  (multiple-value-setq (axiom args)
              (parse-axiom args))
          
          (when (and *producer* *use-chopper-p*) 
            (tree-chop oargs))
          
	  (when axiom
	    ;;; (push axiom axioms)
	    (setf done nil)))

    (values axioms args)))


(defun tree-chop (tree)
  (if (consp tree)
      (if (cddr tree)
          (progn 
            (setf (cdr tree) (last tree))
            (tree-chop (last tree)))
        (mapc #'tree-chop tree))
    tree))

;;;
;;; 
;;;

(defun parse-declaration (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |Declaration|))

      (let ((axiom-annotations nil)
            (entity nil)
            (oargs args)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (axiom-annotations args)
            (parse-annotations args))
        
        (multiple-value-setq (entity args)
            (parse-entity args nil))

        (unless entity
          (when (and error-p *strict-owl-functional*)
            (owlapi:owlapi-parser-error "parse-declaration: no entity in ~A" args))
          (return-from parse-declaration
            (values nil (f-rest oargs))))

        (let* ((axiom 
                (|OWLAPI-getOWLDeclarationAxiom| entity)))

          (unless (eq axiom :void)
            (create-annotations (lambda (x) 
                                  (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                                axiom-annotations))

          (values axiom rest)))

    (values nil args)))

(defun parse-entity (args &optional (error-p t))

  (cond ((f-consp args)

         (let ((entity nil))

           (multiple-value-setq (entity args)
               (parse-class-entity args))

           (when entity 
             (return-from parse-entity
               (values entity args)))

           (multiple-value-setq (entity args)
               (parse-datatype-entity args))

           (when entity 
             (return-from parse-entity
               (values entity args)))
      
           (multiple-value-setq (entity args)
               (parse-object-property-entity args))

           (when entity 
             (return-from parse-entity
               (values entity args)))
      
           (multiple-value-setq (entity args)
               (parse-data-property-entity args))

           (when entity 
             (return-from parse-entity
               (values entity args)))

           (multiple-value-setq (entity args)
               (parse-annotation-property-entity args))

           (when entity 
             (return-from parse-entity
               (values entity args)))
      
           (multiple-value-setq (entity args)
               (parse-named-individual-entity args))

           (when entity 
             (return-from parse-entity
               (values entity args)))    

           (when error-p
             (owlapi:owlapi-parser-error "parse-entity: unknown entity in ~A" args))
    
           ;;; NICHT (f-rest args) ! 
    
           (values nil args)))

        (t 
          
         (values nil args))))

(defun parse-entity-add-iri (args &optional ano-ind-p)
  (let ((entity (f-first (f-first args))))
    (values (list entity 
                  (parse-iri (f-rest (f-first args)) ano-ind-p))
            (f-rest args))))

(defun parse-class-entity (args)
  
  (if (f-consp (f-first args))

      (progn

        (if (or (check-for args |OWLClass|)
                (check-for args |Class|))

            (parse-entity-add-iri args)
          (values nil args)))

    (values nil args)))

(defun parse-datatype-entity (args)
  (if (and (f-consp (f-first args))
           (or (check-for args |Datatype|)
               (check-for args |DataType|)))

      (parse-entity-add-iri args)

    (values nil args)))
      
(defun parse-object-property-entity (args)
  (if (and (f-consp (f-first args))
           (check-for args |ObjectProperty|))

      (parse-entity-add-iri args)

    (values nil args)))
      
(defun parse-data-property-entity (args)
  (if (and (f-consp (f-first args))
           (check-for args |DataProperty|))

      (parse-entity-add-iri args)

    (values nil args)))

(defun parse-annotation-property-entity (args)
  (if (and (f-consp (f-first args))
           (check-for args |AnnotationProperty|))

      (parse-entity-add-iri args)

    (values nil args)))

(defun parse-named-individual-entity (args)
  (if (and (f-consp (f-first args))
           (or (check-for args |NamedIndividual|)
               (check-for args |Individual|)
               (check-for args |AnonymousIndividual|)))

      (parse-entity-add-iri args
                            (check-for args |AnonymousIndividual|))

    (values nil args)))

;;;
;;; 
;;;


(defun parse-annotation-subject (args)
  (parse-individual args))

#|

(defun parse-annotation-value (args)

  (let ((res nil))

    (multiple-value-setq (res args)
        (parse-individual args))

    (when res
      (return-from parse-annotation-value
        (values res args)))
    
    (parse-literal args)))

|#


(defun parse-annotation-value (args)
  (cond ((or (stringp (f-first args))
             (numberp (f-first args))
             (let ((first (f-first args)))                   
               (and (consp first)
                    (let ((first-first (f-first first)))
                      (member first-first 
                              '(|Literal| |Constant| |OWLLiteral|))))))
         (parse-literal args))

        ((consp (f-first args))

         (owlapi-warning "Found a bad annotation - annotation value was expected, but found ~A" (f-first args))
         ;; return base IRI if base is defined
         
         (or (parse-iri args)
             (owlapi:expand-prefix "")))
        
        (t 

         (parse-individual args))))

    
(defun parse-annotations (args)
  (let ((done nil)
        (annotation nil)
        (annotations nil))

    (loop while (and args (not done) (f-consp args)) do

          (setf done t)

          (multiple-value-setq (annotation args)
              (parse-annotation args))

          (when annotation
            (push annotation annotations)
            (setf done nil)))

    (values (if (owlapi:dont-keep-axioms-p *cur-reasoner*)
                nil
              annotations)
            args)))

(defun parse-annotation (args &optional (error-p t))

  (cond ((and (f-consp (f-first args))
              (check-for args |Annotation|))

         (let ((annotation-annotations nil)
               (annotation-property nil)
               (annotation-value nil)
               (args (f-rest (f-first args)))
               (rest (f-rest args)))

           (declare (ignorable annotation-annotations))

           (multiple-value-setq (annotation-annotations args)
               (parse-annotation args))

           #| (when annotation-annotations
                (owlapi-warning "Ignoring annotation annotations: ~A" annotation-annotations)) |# 

           (multiple-value-setq (annotation-property args)
               (parse-annotation-property args))

           (unless annotation-property
             (when error-p
               (owlapi:owlapi-parser-error "parse-annotation: no annotation property in ~A" args))
             (return-from parse-annotation 
               (values nil rest)))

           (multiple-value-setq (annotation-value args)
               (parse-annotation-value args))

           (unless annotation-value
             (when error-p
               (owlapi:owlapi-parser-error "parse-annotation: no annotation value in ~A" args))
             (return-from parse-annotation 
               (values nil rest)))
           
           (values (cons `(|Annotation| 
                           ,annotation-property
                           ,annotation-value)
                         annotation-annotations)
                   rest)))
        
        (t 

         (multiple-value-bind (res remaining)
             (parse-old-annotation args nil)
           (if res 
               (values (list res) remaining)
             (values nil remaining))))))


(defun parse-annotation-axiom (args &optional (error-p t))
  
  (cond ((f-consp args)

         (let ((axiom nil))

           (multiple-value-setq (axiom args)
               (parse-annotation-assertion args))

           (when axiom
             (return-from parse-annotation-axiom
               (values axiom args)))

           (multiple-value-setq (axiom args)
               (parse-sub-annotation-property-of args))

           (when axiom
             (return-from parse-annotation-axiom
               (values axiom args)))
    
           (multiple-value-setq (axiom args)
               (parse-annotation-property-domain args))

           (when axiom
             (return-from parse-annotation-axiom
               (values axiom args)))
    
           (multiple-value-setq (axiom args)
               (parse-annotation-property-range args))

           (when axiom
             (return-from parse-annotation-axiom
               (values axiom args)))

           ;;;
           ;;; old OWLAPI ( OWL 1.1) 
           ;;;
    
           (multiple-value-setq (axiom args)
               (parse-old-entity-annotation args))

           (when axiom
             (return-from parse-annotation-axiom 
               (values axiom args)))

           (multiple-value-setq (axiom args)
               (parse-old-annotation args))

           (when axiom
             (return-from parse-annotation-axiom
               (values axiom args)))

           ;;;
           ;;;
           ;;;

           (when error-p
             (owlapi:owlapi-parser-error "parse-annotation-axiom: unknown annotation axiom in ~A" args))

           (values nil args)))

        (t (values nil args))))


(defun parse-annotation-assertion (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |AnnotationAssertion|))

      (let ((axiom-annotations nil)
            (annotation-property nil)
            (annotation-subject nil)
            (annotation-value nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (axiom-annotations args)
            (parse-annotations args))

        (multiple-value-setq (annotation-property args)
            (parse-annotation-property args))

        (unless annotation-property
          (when error-p
            (owlapi:owlapi-parser-error "parse-annotation-assertion: no annotation property in ~A" args))
          (return-from parse-annotation-assertion
            (values nil rest)))
        
        (multiple-value-setq (annotation-subject args)
            (parse-annotation-subject args))

        (unless annotation-subject
          (when error-p
            (owlapi:owlapi-parser-error "parse-annotation-assertion: no annotation subject in ~A" args))
          (return-from parse-annotation-assertion
            (values nil rest)))

        (multiple-value-setq (annotation-value args)
            (parse-annotation-value args))

        (unless annotation-value
          (when error-p
            (owlapi:owlapi-parser-error "parse-annotation-assertion: no annotation value in ~A" args))
          (return-from parse-annotation-assertion
            (values nil rest)))
        
        (let ((axiom 
               (|OWLAPI-getOWLAnnotationAssertionAxiom| 
                annotation-subject annotation-property
                annotation-value)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              axiom-annotations)

          (values axiom rest)))

    (values nil args)))
        
(defun parse-sub-annotation-property-of (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |SubAnnotationPropertyOf|))

      (let ((axiom-annotations nil)
            (sub nil)
            (super nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (axiom-annotations args)
            (parse-annotations args))

        (multiple-value-setq (sub args)
            (parse-sub-annotation-property args))

        (unless sub
          (when error-p
            (owlapi:owlapi-parser-error "parse-sub-annotation-property-of: no sub annotation property in ~A" args))
          (return-from parse-sub-annotation-property-of
            (values nil rest)))

        (multiple-value-setq (super args)
            (parse-super-annotation-property args))

        (unless sub
          (when error-p
            (owlapi:owlapi-parser-error "parse-sub-annotation-property-of: no super annotation property in ~A" args))
          (return-from parse-sub-annotation-property-of
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLSubAnnotationPropertyOfAxiom| sub super)))
          
          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              axiom-annotations)
          
          (values axiom rest)))

    (values nil args)))
        
(defun parse-sub-annotation-property (args)
  (parse-annotation-property args))

(defun parse-super-annotation-property (args)
  (parse-annotation-property args))


(defun parse-annotation-property-domain (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |AnnotationPropertyDomain|))

      (let ((axiom-annotations nil)
            (prop nil)
            (domain nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (axiom-annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-annotation-property args))

        (unless prop
          (when error-p
            (owlapi:owlapi-parser-error "parse-annotation-property-domain: no annotation property in ~A" args))
          (return-from parse-annotation-property-domain
            (values nil rest)))

        (multiple-value-setq (domain args)
            (parse-iri args))

        (unless domain
          (when error-p
            (owlapi:owlapi-parser-error "parse-annotation-property-domain: no IRI in ~A" args))
          (return-from parse-annotation-property-domain
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLAnnotationPropertyDomainAxiom| prop domain)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              axiom-annotations)

          (values axiom rest)))

    (values nil args)))
                

(defun parse-annotation-property-range (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |AnnotationPropertyRange|))

      (let ((axiom-annotations nil)
            (prop nil)
            (range nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (declare (ignorable axiom-annotations))

        (multiple-value-setq (axiom-annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-annotation-property args))

        (unless prop
          (when error-p
            (owlapi:owlapi-parser-error "parse-annotation-property-range: no annotation property in ~A" args))
          (return-from parse-annotation-property-range
            (values nil rest)))

        (multiple-value-setq (range args)
            (parse-iri args))

        (unless range
          (when error-p
            (owlapi:owlapi-parser-error "parse-annotation-property-range: no IRI in ~A" args))
          (return-from parse-annotation-property-range
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLAnnotationPropertyRangeAxiom| prop range)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              axiom-annotations)

          (values axiom rest)))

    (values nil args)))


;;;
;;; OLD Annotations (OWL 1.1 / OWLAPI) 
;;; 

(defun parse-old-annotation (args &optional (error-p t) really-is-annotation-p)
  (let ((oargs args))

    (labels ((parse-annotation-local (args rest)

               (let ((uri nil)
                     (entity nil))
                 
                 (multiple-value-setq (uri args)
                     (parse-iri args))

                 (unless uri
                   (when error-p 
                     (owlapi:owlapi-parser-error "parse-annotation: no URI in ~A" args))
                   (return-from parse-old-annotation
                     (values nil (f-rest oargs))))

                 (multiple-value-setq (entity args)
                     (parse-entity args nil))

                 (cond (entity

                        ;;; annotation by entity

                        (values (list '|Annotation| uri entity)
                                rest))
                  
                       (t 

                        ;;; annotation by constant

                        (multiple-value-setq (entity args)
                            (parse-literal args))
             
                        (unless entity
                          (when error-p 
                            (owlapi:owlapi-parser-error "parse-annotation: no literal in ~A" args))
                          (return-from parse-old-annotation
                            (values nil (f-rest oargs))))

                        (values (list '|Annotation| uri entity)
                                rest))))))

    (if (f-consp (f-first args))

        (case (f-first (f-first args))
        
          (|Label|
           (let ((literal 
                  (parse-literal (f-rest (f-first args)))))
           
             (unless literal
               (when error-p 
                 (owlapi:owlapi-parser-error "parse-annotation: no constant in ~A" args))
               (return-from parse-old-annotation
                 (values nil (f-rest oargs))))

             (values `(;;d-filler 
		       ;; changed to: 
                       |Annotation| ,(owlapi:make-uri '|rdfs:| '|Label|)
                       ,literal) 
                     (f-rest args))))

          (|owl:versionInfo|
           (let ((literal 
                  (parse-literal (f-rest (f-first args)))))
           
             (unless literal
               (when error-p 
                 (owlapi:owlapi-parser-error "parse-annotation: no constant in ~A" args))
               (return-from parse-old-annotation
                 (values nil (f-rest oargs))))

             (values `(;;d-filler 
		       ;; changed to: 
                       |Annotation| ,(owlapi:make-uri '|rdfs:| '|versionInfo|)
                       ,literal) 
                     (f-rest args))))

          (|Comment| 
           (let ((literal 
                  (parse-literal (f-rest (f-first args)))))
           
             (unless literal
               (when error-p 
                 (owlapi:owlapi-parser-error "parse-annotation: no constant in ~A" args))
               (return-from parse-old-annotation
                 (values nil (f-rest oargs))))

             (values `(;; d-filler
                       |Annotation| 
                       ,(owlapi:make-uri '|rdfs:| '|Comment|)
                       ,literal)
                     (f-rest args))))
        
          (|Annotation|
           (let ((args (f-rest (f-first args)))
                 (rest (f-rest args)))

             (parse-annotation-local args rest)))

          (otherwise 

           (if really-is-annotation-p

               (parse-annotation-local (f-first args) (f-rest args))

             (values nil args))))

      (values nil args)))))
        
(defun parse-old-entity-annotation (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |EntityAnnotation|))

      (let ((args (f-rest (f-first args)))
            (rest (f-rest args))
            (annotations-for-axiom nil)
            (entity nil)
            (annotations-for-entity nil))

        (multiple-value-setq (annotations-for-axiom args)
            (parse-old-annotations-for-axiom args))

        (multiple-value-setq (entity args)
            (parse-entity args nil))

        (unless entity
          (when error-p
            (owlapi:owlapi-parser-error "parse-entity-annotation: no entity in ~A" args))
          (return-from parse-old-entity-annotation 
            (values nil rest)))

        (multiple-value-setq (annotations-for-entity args)
            (parse-old-annotations-for-entity args))

        (let* ((axioms 		
		(mapcar #'(lambda (annotation)
			    (|OWLAPI-getOWLEntityAnnotationAxiom| entity annotation))
			annotations-for-entity))
	       
	       (axiom (first axioms)) ; select the first axiom to be annotated... 

               (axioms2 
		(mapcar #'(lambda (annotation)
			    (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom annotation))
			annotations-for-axiom)))

          (values (if axioms
                      (if axioms2
                          (list axioms axioms2)
                        (list axioms nil))
                    (if axioms2 
                        (list nil axioms2)
                      nil))
                  rest)))

    (values nil args)))


(defun parse-old-annotations-for-axiom (args)
  (parse-old-annotations args))

(defun parse-old-annotations-for-entity (args)
  (parse-old-annotations args t))
             

(defun parse-old-annotations (args &optional really-is-annotation-p)
  (let ((done nil)
        (annotation nil)
        (annotations nil))

    (loop while (and args (not done) (f-consp args)) do

          (setf done t)

          (multiple-value-setq (annotation args)
              (parse-old-annotation args t really-is-annotation-p))

          (when annotation
            (push annotation annotations)
            (setf done nil)))

    (values annotations args)))
        
;;;
;;;
;;; 

(defun parse-class (args)
  (let ((class nil))
  
    (multiple-value-setq (class args)
        (parse-iri args))

    (when class
      (return-from parse-class 
        (values 
         (cond ((string= (symbol-name class) 
			 (symbol-name owlapi:+owlapi-owl-bottom+)) 
		+bottom-symbol+)
               ((string= (symbol-name class) 
			 (symbol-name owlapi:+owlapi-owl-top+)) 
		+top-symbol+)
               (t class))
         args)))

    ;;; extended OWL Functional Syntax to make it more like OWL XML:
    ;;; also accept entities here: (Class A)

    (multiple-value-setq (class args)
        (parse-class-entity args))

    (when class
      (multiple-value-setq (class args)
          (parse-class (cons (second class) args))))

    (values class args)))     


(defun parse-datatype (args)
  (let ((datatype nil))
    
    (multiple-value-setq (datatype args)
        (parse-iri args))

    (when datatype 
      (return-from parse-datatype
        (values datatype args)))

    ;;; extended (see above)

    (multiple-value-setq (datatype args)
        (parse-datatype-entity args))

    (when datatype
      (return-from parse-datatype
        (values (second datatype) args)))

    (values datatype args)))


(defun parse-object-property (args)
  (let ((prop nil))

    (multiple-value-setq (prop args)
        (parse-iri args))

    (when prop
      (return-from parse-object-property
        (values
         (cond ((string= (symbol-name prop) 
                         (symbol-name owlapi:+owlapi-owl-top-object-role+)) 
                +top-object-role-symbol+)
               ((string= (symbol-name prop) 
                         (symbol-name owlapi:+owlapi-owl-bottom-object-role+)) 
                +bottom-object-role-symbol+)
               (t prop))
         args)))

    (multiple-value-setq (prop args)
        (parse-object-property-entity args))

    (when prop
      (return-from parse-object-property
        (values (second prop) args)))

    (values prop args)))


(defun parse-data-property (args)
  (let ((prop nil))

    (multiple-value-setq (prop args)
        (parse-iri args))

    (when prop
      (return-from parse-data-property
        (values
         (cond ((string= (symbol-name prop) 
                         (symbol-name owlapi:+owlapi-owl-top-data-role+)) 
                +top-datatype-role-symbol+)
               ((string= (symbol-name prop) 
                         (symbol-name owlapi:+owlapi-owl-bottom-data-role+)) 
                +bottom-datatype-role-symbol+)
               (t prop))
         args)))

    (multiple-value-setq (prop args)
        (parse-data-property-entity args))

    (when prop
      (return-from parse-data-property
        (values (second prop) args)))

    (values prop args)))


(defun parse-annotation-property (args)
  (let ((prop nil))

    (multiple-value-setq (prop args)
        (parse-iri args))

    (when prop
      (return-from parse-annotation-property
        (values prop args)))

    (multiple-value-setq (prop args)
        (parse-annotation-property-entity args))

    (when prop
      (return-from parse-annotation-property
        (values (second prop) args)))

    (values prop args)))


(defun parse-individual (args)
  (let ((ind nil))

    (multiple-value-setq (ind args)
        (parse-iri args))

    (when ind
      (return-from parse-individual
        (values ind args)))

    (multiple-value-setq (ind args)
        (parse-named-individual-entity args))

    (when ind
      (return-from parse-individual
        (values (second ind) args)))

    (values ind args)))

;;;
;;;
;;;

(defun parse-literals (args)
  ;;;(mapcar #'parse-literal args) geht nicht mehr
  (let ((res nil)
        (done nil)
        (lit nil))

    (loop while (not done) do
          (setf done t)
          (multiple-value-setq (lit args)
              (parse-literal args))
          (when lit
            (push lit res )
            (setf done nil)))

    (reverse res)))

;;;
;;;
;;;
      
(defun parse-object-property-expression (args &optional (error-p t))
  (let ((object-property nil))
    
    (multiple-value-setq (object-property args)
        (parse-inverse-object-property args))

    (unless object-property
      (multiple-value-setq (object-property args)
          (parse-object-property args)))

    (unless object-property 
      (when error-p 
        (owlapi:owlapi-parser-error "parse-object-property-expression: no object property URI in ~A" args))
      (return-from parse-object-property-expression 
        (values nil args)))

    (values object-property args)))

(defun parse-inverse-object-property (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (or (check-for args |InverseObjectProperty|)
               (check-for args |ObjectInverseOf|)))
      
      (let ((property nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (property args)
            (parse-object-property-expression args))

        (unless property 
          (when error-p
            (owlapi:owlapi-parser-error "parse-object-property-expression: no object property URI in ~A" args))
          (return-from parse-inverse-object-property
            (values nil rest)))

        (values `(inv ,property) rest))

    (values nil args)))

(defun parse-data-property-expression (args)
  (parse-data-property args))


;;;
;;;
;;; 


(defun parse-data-type (args &optional (error-p t))

  (let ((type nil))

    (multiple-value-setq (type args)
        (parse-datatype args))

    (cond (type
           (values (convert-owl-data-range type) args))

          (t

           (when error-p
             (owlapi:owlapi-parser-error "parse-data-type: unknown type in ~A" args))
           
           (values nil args)))))

(defun parse-data-range (args &optional (error-p t))
  
  (let ((range nil))
 
    (multiple-value-setq (range args)
        (parse-data-type args nil))

    (when range
      (return-from parse-data-range
        (values range args)))

    (cond ((f-consp (f-first args))
      
           (multiple-value-setq (range args)
               (parse-data-intersection-of args))

           (when range
             (return-from parse-data-range
               (values range args)))

           (multiple-value-setq (range args)
               (parse-data-union-of args))

           (when range
             (return-from parse-data-range
               (values range args)))

           (multiple-value-setq (range args)
               (parse-data-complement-of args))

           (when range
             (return-from parse-data-range
               (values range args)))
    
           (multiple-value-setq (range args)
               (parse-data-one-of args))

           (when range
             (return-from parse-data-range
               (values range args)))

           (multiple-value-setq (range args)
               (parse-datatype-restriction args))

           (when range
             (return-from parse-data-range
               (values range args)))

           (when error-p
             (owlapi:owlapi-parser-error "parse-data-range: unknown range in ~A" args))

           (values nil (f-rest args)))

          (t (values nil args)))))


(defun parse-data-intersection-of (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |DataIntersectionOf|))
      
      (let ((ranges nil)
            (done nil)
            (range nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (loop while (and args (not done) (f-consp args)) do 

              (setf done t)

              (multiple-value-setq (range args)
                  (parse-data-range args))

              (when range
                (push range ranges)
                (setf done nil)))

        (unless (cdr ranges)
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-intersection-of: at least 2 data-range arguments required in ~A" args))
          (return-from parse-data-intersection-of
            (values nil rest)))

        (values (cons 'd-and ranges) rest))

    (values nil args)))


(defun parse-data-union-of (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |DataUnionOf|))
      
      (let ((ranges nil)
            (done nil)
            (range nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (loop while (and args (not done) (f-consp args)) do 

              (setf done t)

              (multiple-value-setq (range args)
                  (parse-data-range args))

              (when range
                (push range ranges)
                (setf done nil)))

        (unless (cdr ranges)
          (when error-p
            (owlapi:owlapi-parser-error "parse-data-union-of: at least 2 data-range arguments required in ~A" args))
          (return-from parse-data-union-of
            (values nil rest)))

        (values (cons 'd-or ranges) rest))

    (values nil args)))



(defun parse-data-complement-of (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |DataComplementOf|))
      (let ((range nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (range args)
            (parse-data-range args))

        (unless range
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-complement-of: no data range in ~A" args))
          (return-from parse-data-complement-of 
            (values nil rest)))

        (values `(d-complement ,range) rest))

    (values nil args)))



(defun parse-data-one-of (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |DataOneOf|))
      
      (let ((constants nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))
        
        (multiple-value-setq (constants args)
            (parse-literals args))

        (unless constants
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-one-of: no literals in ~A" args))
          (return-from parse-data-one-of 
            (values nil (f-rest args))))

        (values (cons 'd-possible-values constants) rest))

    (values nil args)))



(defun parse-datatype-restriction (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |DatatypeRestriction|))

      (let ((args (f-rest (f-first args)))
            (rest (f-rest args))
            (type nil)
            (range nil)
            (facets-and-values nil))

        (multiple-value-setq (type args)
            (parse-data-type args nil))

        ;;; altes OWL 1.1 (people+pets.funct etc.) 
        #+:ignore
        (multiple-value-setq (range args)
            (parse-data-range args nil))

        (unless (or type range)
          (when error-p 
            (owlapi:owlapi-parser-error "parse-datatype-restriction: no base datatype in ~A" args))
          (return-from parse-datatype-restriction
            (values nil rest)))

        (multiple-value-setq (facets-and-values args)
            (parse-datatype-facets-and-restriction-values args))

        (unless facets-and-values
          (when error-p 
            (owlapi:owlapi-parser-error "parse-datatype-restriction: bad or no facets and values in ~A" args))
          (return-from parse-datatype-restriction
            (values nil rest)))

        (values `(d-restriction (d-base-type
                                 ,@(or (cdr type)
                                       (cdr range) ;; fuehrendes d-datarange entfernen 
                                       ))
                                ,@facets-and-values)
                rest))

    (values nil args)))


(defun parse-datatype-facets-and-restriction-values (args &optional (error-p t))
  (let ((done nil)
        (facets-and-restriction-values nil)
        (facet nil)
        (restriction nil))

    (loop while (and args (not done) (f-consp args)) do

          (setf done t)          

          (when (and (f-consp (f-first args))
                     (check-for args |FacetRestriction|))

            ;;;
            ;;; Syntax we get from OWL XML -> OWL Functional transformation: 
            ;;; 
            ;;; ((|FacetRestriction|
            ;;;     (|Attribute| |facet| "xsd:minInclusive") (|Literal| "xsd:integer" "0")) 
            ;;; -> Rewrite as (xsd:minInclusive (Literal ...))
    
            (setf args
                  (list* (f-second (f-first args))
                         (f-third (f-first args))
                         (f-rest args))))

          (multiple-value-setq (facet args)
              (parse-constraining-facet args))

          (unless facet
            (when error-p 
              (owlapi:owlapi-parser-error "parse-datatype-facet-and-restriction-values: no facet in ~A" 
                                   args))
            (return-from parse-datatype-facets-and-restriction-values
              (values nil (f-rest args))))

          (multiple-value-setq (restriction args)
              (parse-restriction-value args))

          (unless restriction
            (when error-p 
              (owlapi:owlapi-parser-error "parse-datatype-facet-and-restriction-values: no value for facet ~A in ~A" 
                                   facet args))

            (return-from parse-datatype-facets-and-restriction-values
              (values nil (f-rest args))))

          (push (convert-owl-data-facet facet restriction)
                facets-and-restriction-values)

          (setf done nil))

    (values facets-and-restriction-values
            args)))

(defun parse-restriction-value (args)
  (parse-literal args))

(defun parse-constraining-facet (args &optional (error-p t))
  (if (owlapi:is-url-p (f-first args))
      (values (f-first args) (f-rest args))
    (multiple-value-bind (prefix postfix) 
        (owlapi:get-prefix-postfix (f-first args))
      (declare (ignorable prefix))
      (case postfix
        ((|length| 
          |minLength|
          |maxLength|
          |pattern|
          |minInclusive|
          |maxInclusive|
          |minExclusive|
          |maxExclusive|
          |totalDigits|
          |fractionDigits|)
         (values (f-first args) (f-rest args)))
        (otherwise
         (when error-p 
           (owlapi:owlapi-parser-error "parse-datatype-facet: unknown facet in ~A" args))
         (values nil (f-rest args)))))))


;;;
;;;
;;;

(defun parse-class-expression (args)
  (let ((expression nil))

    (dolist (parser '(parse-class
                      
                      parse-object-intersection-of
                      parse-object-union-of
                      parse-object-complement-of
                      parse-object-one-of

                      parse-object-some-values-from
                      parse-object-all-values-from
                      parse-object-has-value
                      parse-object-has-self
                      
                      parse-object-min-cardinality
                      parse-object-max-cardinality
                      parse-object-exact-cardinality

                      parse-data-some-values-from
                      parse-data-all-values-from
                      parse-data-has-value
                      
                      parse-data-min-cardinality
                      parse-data-max-cardinality
                      parse-data-exact-cardinality))

      (multiple-value-setq (expression args)
          (funcall parser args))

      (when expression 
        (return-from parse-class-expression
          (values expression args))))

    (values nil args)))

(defun parse-object-intersection-of (args &optional (error-p t))
  (declare (ignorable error-p))

  (if (and (f-consp (f-first args))
           (check-for args |ObjectIntersectionOf|))

      (let ((expressions nil)
            (expression nil)
            (done nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        ;;; min. 2 lt. Grammatik

        (multiple-value-setq (expression args)
            (parse-class-expression args))

        (unless expression 
          (setf expression 'top) 
          ;; make robust for OWLAPI: 
          (owlapi:owlapi-warning "Missing argument in ~A - using TOP" args)
          #+:ignore
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-intersection-of: no class expression in ~A" args))
          #+:ignore
          (return-from parse-object-intersection-of
            (values nil rest)))

        (push expression expressions)

        (multiple-value-setq (expression args)
            (parse-class-expression args))

        #+:ignore
        (unless expression 
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-intersection-of: no second class expression in ~A" args))
          (return-from parse-object-intersection-of
            (values nil rest)))

        (when expression
          (push expression expressions))

        (loop while (and args (not done) (f-consp args)) do

              (setf done t)

              (multiple-value-setq (expression args)
                  (parse-class-expression args))

              (when expression 
                (push expression expressions)
                (setf done nil)))

        (values `(and ,@expressions) rest))

    (values nil args)))


(defun parse-object-union-of (args &optional (error-p t))
  (declare (ignorable error-p))

  (if (and (f-consp (f-first args))
           (check-for args |ObjectUnionOf|))
      
      (let ((descriptions nil)
            (description nil)
            (done nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        ;;; min. 2 lt. Grammatik

        (multiple-value-setq (description args)
            (parse-class-expression args))

        (unless description 
          (setf description 'bottom) 
          (owlapi:owlapi-warning "Missing argument in ~A - using BOTTOM" args)
          #+:ignore
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-union-of: no class expression in ~A" args))
          #+:ignore
          (return-from parse-object-union-of 
            (values nil rest)))

        (push description descriptions)

        (multiple-value-setq (description args)
            (parse-class-expression args))

        #+:ignore
        (unless description 
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-union-of: no second class expression in ~A" args))
          (return-from parse-object-union-of 
            (values nil rest)))

        (when description
          (push description descriptions))

        (loop while (and args (not done) (f-consp args)) do

              (setf done t)

              (multiple-value-setq (description args)

                  (parse-class-expression args))

              (when description 
                (push description descriptions)
                (setf done nil)))

        (values `(or ,@descriptions) rest))

    (values nil args)))


(defun parse-object-complement-of (args &optional (error-p t))
  (declare (ignorable error-p))

  (if (and (f-consp (f-first args))
           (check-for args |ObjectComplementOf|))

      (let ((descr nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (descr args)
            (parse-class-expression args))

        (unless descr
          (setf descr 'bottom) 
          (owlapi:owlapi-warning "Missing argument in ~A - using BOTTOM" args)
          #+:ignore
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-complement-of: no class expression in ~A" descr))
          #+:ignore
          (return-from parse-object-complement-of 
            (values nil rest)))
          
        (values `(not ,descr) rest))

    (values nil args)))


(defun parse-object-one-of (args &optional (error-p t))
  (declare (ignorable error-p))

  (if (and (f-consp (f-first args))
           (check-for args |ObjectOneOf|))

      (let ((ind-iris nil)
            (ind-iri nil)
            (done nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (ind-iri args)
            (parse-individual args))

        (unless ind-iri
          #+:ignore
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-one-of: no individual URI in ~A" args))
          #+:ignore
          (return-from parse-object-one-of
            (values nil rest)))

        (push ind-iri ind-iris)

        (loop while (and args (not done) (f-consp args)) do

              (setf done t)

              (multiple-value-setq (ind-iri args)
                  (parse-individual args))

              (when ind-iri 
                (push ind-iri ind-iris)
                (setf done nil)))

        (unless ind-iris
          (owlapi:owlapi-warning "Missing arguments in ~A - returning TOP" args))

        (values (if ind-iris 
                    `(one-of ,@(remove nil ind-iris))
                  'top ;+owl-top+
                  )
                rest))

    (values nil args)))


(defun parse-object-some-values-from (args &optional (error-p t))
  (declare (ignorable error-p))

  (if (and (f-consp (f-first args))
           (check-for args |ObjectSomeValuesFrom|))

      (let ((property-expression nil)
            (expression nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (property-expression args)
            (parse-object-property-expression args))

        (unless property-expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-some-values-from: no object property expression in ~A" args))
          (return-from parse-object-some-values-from
            (values nil rest)))

        (multiple-value-setq (expression args)
            (parse-class-expression args))

        (unless expression
          (setf expression 'top)
          (owlapi:owlapi-warning "Missing arguments in ~A - using TOP" args)
          #+:ignore
          (when error-p
            (owlapi:owlapi-parser-error "parse-object-some-values-from: no class expression in ~A" args))
          #+:ignore
          (return-from parse-object-some-values-from
            (values nil rest)))

        (values `(some ,property-expression ,expression) rest))

    (values nil args)))


(defun parse-object-all-values-from (args &optional (error-p t))
  (declare (ignorable error-p))

  (if (and (f-consp (f-first args))
           (check-for args |ObjectAllValuesFrom|))

      (let ((property-expression nil)
            (expression nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (property-expression args)
            (parse-object-property-expression args))

        (unless property-expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-all-values-from: no object property expression in ~A" args))
          (return-from parse-object-all-values-from
            (values nil rest)))

        (multiple-value-setq (expression args)
            (parse-class-expression args))

        (unless expression
          (setf expression 'top) 
          (owlapi:owlapi-warning "Missing arguments in ~A - using TOP" args)       
          #+:ignore
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-all-values-from: no class expression in ~A" args))
          #+:ignore
          (return-from parse-object-all-values-from
            (values nil rest)))

        (values `(all ,property-expression ,expression) rest))

    (values nil args)))



(defun parse-object-has-value (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |ObjectHasValue|))

      (let ((object-property-expression nil)
            (uri nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (object-property-expression args)
            (parse-object-property-expression args))

        (unless object-property-expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-has-value: no object property expression in ~A" args))
          (return-from parse-object-has-value
            (values nil rest)))

        (multiple-value-setq (uri args)
            (parse-individual args))

        #+:ignore
        (unless uri
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-hast-value: no individual URI in ~A" args))
          (return-from parse-object-has-value
            (values nil rest)))

        (if uri
            (values `(has-value ,object-property-expression ,uri) rest)
          (progn 
            (owlapi:owlapi-warning "Missing arguments in ~A - returning TOP" args)       
            (values ; +owl-top+
             'top rest))))      

    (values nil args)))


(defun parse-object-has-self (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (or (check-for args |ObjectExistsSelf|)
               (check-for args |ObjectHasSelf|)))

      (let ((property-expression nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (property-expression args)
            (parse-object-property-expression args))

        (unless property-expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-has-self: no object property expression in ~A" args))
          (return-from parse-object-has-self
            (values nil rest)))

        (values `(self-reference ,property-expression) rest))

    (values nil args)))


(defun parse-non-negative-integer (args &optional (error-p t))
  (if (stringp (f-first args))

      (let ((num 
             (ignore-errors 
               (parse-integer (f-first args)))))

        (when (not (and (integerp num)
                        (not (minusp num))))

          (when error-p
            (owlapi:owlapi-parser-error "parse-non-negative-integer: got ~A" args))

          (return-from parse-non-negative-integer
            (values nil (f-rest args))))

        (values num (f-rest args)))

    (progn
      
      (when (not (and (integerp (f-first args))
                      (not (minusp (f-first args)))))
        (when error-p
          (owlapi:owlapi-parser-error "parse-non-negative-integer: got ~A" args))

        (return-from parse-non-negative-integer
          (values nil (f-rest args))))

      (values (f-first args) (f-rest args)))))

(defun parse-cardinality (args)
  (parse-non-negative-integer args))


(defun parse-object-min-cardinality (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |ObjectMinCardinality|))

      (let ((card nil)
            (object-property-expression nil)
            (expression nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (card args)
            (parse-cardinality args))

        (unless card
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-min-cardinality: no min cardinality in ~A" args))
          (return-from parse-object-min-cardinality
            (values nil rest)))

        (multiple-value-setq (object-property-expression args)
            (parse-object-property-expression args))

        (unless object-property-expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-min-cardinality: no object property expression in ~A" args))
          (return-from parse-object-min-cardinality
            (values nil rest)))

        ;;; optional 
        (multiple-value-setq (expression args)
            (parse-class-expression args))

        (values `(at-least ,card ,object-property-expression ,@(when expression 
                                                                 (list expression)))
                rest))

    (values nil args)))


(defun parse-object-max-cardinality (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |ObjectMaxCardinality|))

      (let ((card nil)
            (object-property-expression nil)
            (expression nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (card args)
            (parse-cardinality args))

        (unless card
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-max-cardinality: no max cardinality in ~A" args))
          (return-from parse-object-max-cardinality
            (values nil rest)))

        (multiple-value-setq (object-property-expression args)
            (parse-object-property-expression args))

        (unless object-property-expression
          (when error-p
            (owlapi:owlapi-parser-error "parse-object-max-cardinality: no object property expression in ~~A" args))
          (return-from parse-object-max-cardinality
            (values nil rest)))

        ;;; optional 
        (multiple-value-setq (expression args)
            (parse-class-expression args))

        (values `(at-most ,card ,object-property-expression ,@(when expression
                                                                (list expression)))
                rest))

    (values nil args)))



(defun parse-object-exact-cardinality (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |ObjectExactCardinality|))
      (let ((card nil)
            (object-property-expression nil)
            (expression nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (card args)
            (parse-cardinality args))

        (unless card
          (when error-p
            (owlapi:owlapi-parser-error "parse-object-exact-cardinality: no min/max cardinality in ~A" args))
          (return-from parse-object-exact-cardinality
            (values nil rest)))

        (multiple-value-setq (object-property-expression args)
            (parse-object-property-expression args))

        (unless object-property-expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-pbject-exact-cardinality: no object property expression in ~A" args))
          (return-from parse-object-exact-cardinality
            (values nil rest)))

        ;;; optional 
        (multiple-value-setq (expression args)
            (parse-class-expression args))

        (values `(exactly ,card ,object-property-expression ,@(when expression
                                                                (list expression)))
                rest))

    (values nil args)))
          



(defun parse-data-some-values-from (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |DataSomeValuesFrom|))
      (let ((property-expression nil)
            (property-expressions nil)
            (range nil)
            (done nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (property-expression args)
            (parse-data-property-expression args))

        (unless property-expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-some-values-from: no data property expression in ~A" args))
          (return-from parse-data-some-values-from
            (values nil rest)))

        (push property-expression property-expressions)

        (when (f-rest args) ; otherwise range 
          (loop while (and args (not done) (f-consp args)) do

                (setf done t)

                (multiple-value-setq (property-expression args)
                    (parse-data-property-expression args))

                (when property-expression
                  (push  property-expression property-expressions)
                  (setf done nil))))

        (multiple-value-setq (range args)
            (parse-data-range args))

        (unless range
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-some-values-from: no data range in ~A" args))
          (return-from parse-data-some-values-from
            (values nil rest)))

        (values `(d-some ,@(reverse property-expressions) ,range)
                rest))

    (values nil args)))


(defun parse-data-all-values-from (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |DataAllValuesFrom|))
      (let ((property-expression nil)
            (property-expressions nil)
            (range nil)
            (done nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (property-expression args)
            (parse-data-property-expression args))

        (unless property-expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-all-values-from: no data property expression in ~A" args))
          (return-from parse-data-all-values-from 
            (values nil rest)))

        (push property-expression property-expressions)

        (when (f-rest args)
          (loop while (and args (not done) (f-consp args)) do

                (setf done t)

                (multiple-value-setq (property-expression args)
                    (parse-data-property-expression args))

                (when property-expression
                  (push  property-expression property-expressions)
                  (setf done nil))))

        (multiple-value-setq (range args)
            (parse-data-range args))

        (unless range
          (when error-p
            (owlapi:owlapi-parser-error "parse-data-all-values-from: no data range in ~A" args))
          (return-from parse-data-all-values-from
            (values nil rest)))

        (values `(d-all ,@(reverse property-expressions) ,range)
                rest))

    (values nil args)))


(defun parse-data-has-value (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |DataHasValue|))
      (let ((data-property-expression nil)
            (constant nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (data-property-expression args)
            (parse-data-property-expression args))

        (unless data-property-expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-has-value: no data property expression in ~A" args))
          (return-from parse-data-has-value 
            (values nil rest)))

        (multiple-value-setq (constant args)
            (parse-literal args))

        (unless constant
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-has-value: no constant in ~A" args))
          (return-from parse-data-has-value
            (values nil rest)))

        (values `(d-filler ,data-property-expression ,constant)
                rest))

    (values nil args)))


(defun parse-data-min-cardinality (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |DataMinCardinality|))
      (let ((card nil)
            (data-property-expression nil)
            (data-range nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (card args)
            (parse-cardinality args))

        (unless card
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-min-cardinality: no min cardinality in ~A" args))
          (return-from parse-data-min-cardinality
            (values nil rest)))              

        (multiple-value-setq (data-property-expression args)
            (parse-data-property-expression args))

        (unless data-property-expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-min-cardinality: no data property expression in ~A" args))
          (return-from parse-data-min-cardinality
            (values nil rest)))

        ;;; optional 
        (multiple-value-setq (data-range args)
            (parse-data-range args nil))

        (values (remove nil `(d-at-least ,card ,data-property-expression ,data-range))
                rest))

    (values nil args)))



(defun parse-data-max-cardinality (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |DataMaxCardinality|))
      (let ((card nil)
            (data-property-expression nil)
            (data-range nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (card args)
            (parse-cardinality args))

        (unless card
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-max-cardinality: no max cardinality in ~A" args))
          (return-from parse-data-max-cardinality
            (values nil rest)))

        (multiple-value-setq (data-property-expression args)
            (parse-data-property-expression args))

        (unless data-property-expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-max-cardinality: no data property expresion in ~A" args))
          (return-from parse-data-max-cardinality
            (values nil rest)))

        ;;; optional 
        (multiple-value-setq (data-range args)
            (parse-data-range args nil))

        (values (remove nil `(d-at-most ,card ,data-property-expression ,data-range))
                rest))

    (values nil args)))


(defun parse-data-exact-cardinality (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |DataExactCardinality|))

      (let ((card nil)
            (data-property-expression nil)
            (data-range nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (card args)
            (parse-cardinality args))

        (unless card
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-exact-cardinality: no min/max cardinality in ~A" args))
          (return-from parse-data-exact-cardinality
            (values nil rest)))

        (multiple-value-setq (data-property-expression args)
            (parse-data-property-expression args))

        (unless data-property-expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-exact-cardinality: no data property expression in ~A" args))
          (return-from parse-data-exact-cardinality
            (values nil rest)))

        ;;; optional 
        (multiple-value-setq (data-range args)
            (parse-data-range args nil))

        (values (remove nil `(d-exactly ,card ,data-property-expression ,data-range))
                rest))

    (values nil args)))

;;;
;;;
;;; 

(defun parse-axiom (args)

  (cond ((f-consp args)

         (let ((axiom nil))

           (multiple-value-setq (axiom args)
               (parse-declaration args))

           (when axiom
             (return-from parse-axiom
               (values axiom args)))
    
           (multiple-value-setq (axiom args)
               (parse-class-axiom args))
    
           (when axiom 
             (return-from parse-axiom 
               (values axiom args)))

           (multiple-value-setq (axiom args)
               (parse-object-property-axiom args))

           (when axiom 
             (return-from parse-axiom
               (values axiom args)))

           (multiple-value-setq (axiom args)
               (parse-data-property-axiom args))

           (when axiom 
             (return-from parse-axiom
               (values axiom args)))

           (multiple-value-setq (axiom args)
               (parse-datatype-definition args))

           (when axiom 
             (return-from parse-axiom
               (values axiom args)))

           (multiple-value-setq (axiom args)
               (parse-has-key args))

           (when axiom 
             (return-from parse-axiom
               (values axiom args)))

           (multiple-value-setq (axiom args)
               (parse-assertion args))

           (when axiom 
             (return-from parse-axiom
               (values axiom args)))

           (multiple-value-setq (axiom args)
               (parse-annotation-axiom args))

           (values axiom args)))
        
        (t (values nil args))))
             
;;;
;;;
;;;

(defun parse-subclass (args)
  (parse-class-expression args))

(defun parse-superclass (args)
  (parse-class-expression args))

(defun parse-subclass-of (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |SubClassOf|))

      (let ((annotations nil)
            (subclass nil)
            (superclass nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        ;;; optional
        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (subclass args)
            (parse-subclass args))

        (unless subclass
          (when error-p 
            (owlapi:owlapi-parser-error "parse-subclass-of: no subclass class expression in ~A" args))
          (return-from parse-subclass-of
            (values nil rest)))

        (multiple-value-setq (superclass args)
            (parse-superclass args))

        (unless superclass
          (when error-p 
            (owlapi:owlapi-parser-error "parse-subclass-of: no superclass class expression in ~A" args))
          (return-from parse-subclass-of
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLSubClassAxiom| subclass superclass)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)

          (values axiom rest)))

    (values nil args)))

    

(defun parse-equivalent-classes (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |EquivalentClasses|))

      (let ((annotations nil)
            (expressions nil)
            (expression nil)
            (done nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        ;;; optional
        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (expression args)
            (parse-class-expression args))
        
        (unless expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-equivalent-classes: no first class expression in ~A" args))
          (return-from parse-equivalent-classes
            (values nil rest)))

        (push expression expressions)
        
        (multiple-value-setq (expression args)
            (parse-class-expression args))
        
        (unless expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-equivalent-classes: no second class expression in ~A" args))
          (return-from parse-equivalent-classes
            (values nil rest)))

        (push expression expressions)
        
        (loop while (and args (not done) (f-consp args)) do

              (setf done t)

              (multiple-value-setq (expression args)

                  (parse-class-expression args))

              (when expression 
                (push expression expressions)
                (setf done nil)))

        (let ((axiom 
               (|OWLAPI-getOWLEquivalentClassesAxiom| (reverse expressions))))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)

          (values axiom rest)))

    (values nil args)))


(defun parse-disjoint-classes (args &optional (error-p t))
  (declare (ignorable error-p))
  
  (if (and (f-consp (f-first args))
           (check-for args |DisjointClasses|))

      (let ((annotations nil)
            (expressions nil)
            (expression nil)
            (done nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        ;;; optional
        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (expression args)
            (parse-class-expression args))
        
        #+:ignore
	(unless expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-disjoint-classes: no first class expression in ~A" args))
          (return-from parse-disjoint-classes
            (values nil rest)))

        (when expression
	  (push expression expressions))
        
	(multiple-value-setq (expression args)
            (parse-class-expression args))
        
        #+:ignore
        (unless expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-disjoint-classes: no second class expression in ~A" args))
          (return-from parse-disjoint-classes
            (values nil rest)))

        (when expression 
	  (push expression expressions))
        
        (loop while (and args (not done) (f-consp args)) do

              (setf done t)

              (multiple-value-setq (expression args)

                  (parse-class-expression args))

              (when expression 
                (push expression expressions)
                (setf done nil)))

        (let ((axiom 
               (|OWLAPI-getOWLDisjointClassesAxiom| (reverse expressions))))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
          
          (values axiom rest)))

    (values nil args)))




(defun parse-disjoint-union (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |DisjointUnion|))

      (let ((annotations nil)
            (class-iri nil)
            (expressions nil)
            (expression nil)
            (done nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        ;;; optional
        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (class-iri args)
            (parse-class args))
        
        (unless class-iri
          (when error-p 
            (owlapi:owlapi-parser-error "parse-disjoint-union: no class uri in ~A" args))
          (return-from parse-disjoint-union
            (values nil rest)))

        (multiple-value-setq (expression args)
            (parse-class-expression args))

        #+:ignore
        (unless expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-disjoint-union: no first class expression in ~A" args))
          (return-from parse-disjoint-union
            (values nil rest)))

        (when expression
          (push expression expressions))
        
        (multiple-value-setq (expression args)
            (parse-class-expression args))
        
        #+:ignore
        (unless expression
          (when error-p 
            (owlapi:owlapi-parser-error "parse-disjoint-union: no second class expression in ~A" args))
          (return-from parse-disjoint-union
            (values nil rest)))

        (when expression
          (push expression expressions))
        
        (loop while (and args (not done) (f-consp args)) do

              (setf done t)

              (multiple-value-setq (expression args)
                  (parse-class-expression args))

              (when expression 
                (push expression expressions)
                (setf done nil)))

        (let ((axiom 
               (|OWLAPI-getOWLDisjointUnionAxiom| class-iri (reverse expressions))))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
        
          (values axiom rest)))

    (values nil args)))


(defun parse-class-axiom  (args)
  (let ((axiom nil))

    (dolist (parser '(parse-subclass-of
                      parse-equivalent-classes
                      parse-disjoint-classes
                      parse-disjoint-union))

      (multiple-value-setq (axiom args)
          (funcall parser args))

      (when axiom 
        (return-from parse-class-axiom
          (values axiom args)))))

  (values nil args))

;;;
;;;
;;; 

(defun parse-object-property-axiom (args)
  (let ((axiom nil))

    (dolist (parser '(parse-sub-object-property-of
                      parse-equivalent-object-properties

                      parse-disjoint-object-properties
                      parse-inverse-object-properties

                      parse-object-property-domain
                      parse-object-property-range

                      parse-functional-object-property
                      parse-inverse-functional-object-property

                      parse-reflexive-object-property
                      parse-irreflexive-object-property

                      parse-symmetric-object-property
                      parse-asymmetric-object-property

                      parse-transitive-object-property))

      (multiple-value-setq (axiom args)
          (funcall parser args))

      (when axiom 
        (return-from parse-object-property-axiom
          (values axiom args)))))

  (values nil args))   



(defun parse-property-expression-chain (args &optional (error-p t))
  
  (cond ((and (f-consp (f-first args))
              (or (check-for args |SubObjectPropertyChain|)
                  (check-for args |ObjectPropertyChain|)))

         (let ((first nil)
               (second nil)
               (args (f-rest (f-first args)))
               (rest (f-rest args))
               (others nil)
               (other nil)
               (done nil))

           (declare (ignorable first second))
        
           (multiple-value-setq (first args)
               (parse-object-property-expression args))

           (unless first 
             (when error-p 
               (owlapi:owlapi-parser-error "parse-property-expression-chain: no first property in ~A"))
             (return-from parse-property-expression-chain
               (values nil rest)))

           (multiple-value-setq (second args)
               (parse-object-property-expression args))

           (unless second 
             (when error-p 
               (owlapi:owlapi-parser-error "parse-property-expression-chain: no second property in ~A"))
             (return-from parse-property-expression-chain
               (values nil rest)))
                  
           (loop while (and args (not done) (f-consp args)) do
                 
                 (setf done t)

                 (multiple-value-setq (other args)
                     (parse-object-property-expression args))
                 
                 (when other
                   (push other others)
                   (setf done nil)))

           (values `(,first ,second ,@(reverse others)) rest)))

        (t 

         (let ((property nil))

           (multiple-value-setq (property args)
               (parse-object-property-expression args))

           (if property
               (values property args)
             (values nil args))))))


(defun parse-sub-object-property-expression (args)
  (let ((expr nil))

    (multiple-value-setq (expr args)
        (parse-object-property-expression args nil))

    (when expr 
      (return-from parse-sub-object-property-expression
        (values expr args)))

    (multiple-value-setq (expr args)
        (parse-property-expression-chain args nil))

    (when expr
      (return-from parse-sub-object-property-expression
        (values expr args)))

    (values nil args)))

(defun parse-super-object-property-expression (args)
  (parse-object-property-expression args))
        
(defun parse-sub-object-property-of (args &optional (error-p t))
  
  (if (and (f-consp (f-first args))
           (check-for args |SubObjectPropertyOf|))
        
      (let ((sub-prop nil)
            (prop nil)
            (annotations nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))
        
        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (sub-prop args)
            (parse-sub-object-property-expression args))

        (unless sub-prop 
          (when error-p
            (owlapi:owlapi-parser-error "parse-sub-object-property-of: no sub object property expression in ~A" args))
          (return-from parse-sub-object-property-of
            (values nil rest)))
        
        (multiple-value-setq (prop args)
            (parse-super-object-property-expression args))

        (unless prop 
          (when error-p 
            (owlapi:owlapi-parser-error "parse-sub-object-property-of: no super object property expression in ~A" args))
          (return-from parse-sub-object-property-of
            (values nil rest)))

        (let ((axiom 
               (if (or (symbolp sub-prop)
                       (and (consp sub-prop)
                            (eq (first sub-prop) 'inv)))
                   (|OWLAPI-getOWLObjectSubPropertyAxiom| sub-prop prop)
                 (|OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom| sub-prop prop))))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
          
          (values axiom rest)))

    (values nil args)))


(defun parse-equivalent-object-properties (args &optional (error-p t))
  
  (if (and (f-consp (f-first args))
           (check-for args |EquivalentObjectProperties|))
       
      (let ((annotations nil)
            (props nil)
            (prop nil)
            (done nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-equivalent-object-properties: no first object property expression in ~A" args))
          (return-from parse-equivalent-object-properties
            (values nil rest)))

        (push prop props)

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-equivalent-object-properties: no second object property expression in ~A" args))
          (return-from parse-equivalent-object-properties
            (values nil rest)))

        (push prop props)

        (loop while (and args (not done) (f-consp args)) do

              (setf done t)
              
              (multiple-value-setq (prop args)
                  (parse-object-property-expression args))

              (when prop
                (push prop props)
                (setf done nil)))

        (let ((axiom 
               (|OWLAPI-getOWLEquivalentObjectPropertiesAxiom| (reverse props))))

           (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                               annotations)

          (values axiom rest)))

    (values nil args)))

(defun parse-disjoint-object-properties (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |DisjointObjectProperties|))
       
      (let ((annotations nil)
            (props nil)
            (prop nil)
            (done nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-disjoint-object-properties: no first object property expression in ~A" args))
          (return-from parse-disjoint-object-properties
            (values nil rest)))

        (push prop props)

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-disjoint-object-properties: no second object property expression in ~A" args))

          (return-from parse-disjoint-object-properties
            (values nil rest)))

        (push prop props)

        (loop while (and args (not done) (f-consp args)) do

              (setf done t)
              
              (multiple-value-setq (prop args)
                  (parse-object-property-expression args))

              (when prop
                (push prop props)
                (setf done nil)))

        (let ((axiom 
               (|OWLAPI-getOWLDisjointObjectPropertiesAxiom| (reverse props))))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
          
          (values axiom rest)))

    (values nil args)))
        


(defun parse-inverse-object-properties (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |InverseObjectProperties|))
       
      (let ((annotations nil)
            (prop1 nil)
            (prop2 nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop1 args)
            (parse-object-property-expression args))

        (unless prop1
          (when error-p 
            (owlapi:owlapi-parser-error "parse-inverse-object-properties: no first object property expression in ~A" args))
          (return-from parse-inverse-object-properties 
            (values nil rest)))
        
        (multiple-value-setq (prop2 args)
            (parse-object-property-expression args))

        (unless prop2
          (when error-p 
            (owlapi:owlapi-parser-error "parse-inverse-object-properties: no second object property expression in ~A" args))
          (return-from parse-inverse-object-properties 
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLInverseObjectPropertiesAxiom| prop1 prop2)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
          
          (values axiom rest)))

    (values nil args)))

        
(defun parse-object-property-domain (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |ObjectPropertyDomain|))
       
      (let ((annotations nil)
            (prop nil)
            (descr nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-property-domain: no object property expression in ~A" args))
          (return-from parse-object-property-domain
            (values nil rest)))              

        (multiple-value-setq (descr args)
            (parse-class-expression args))

        (unless descr
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-property-domain: no class expression in ~A" args))
          (return-from parse-object-property-domain
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLObjectPropertyDomainAxiom| prop descr)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)

          (values axiom rest)))

    (values nil args)))
        
(defun parse-object-property-range (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |ObjectPropertyRange|))
       
      (let ((annotations nil)
            (prop nil)
            (descr nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-property-range: no object property expression in ~A" args))
          (return-from parse-object-property-range
            (values nil rest)))

        (multiple-value-setq (descr args)
            (parse-class-expression args))

        (unless descr
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-property-range: no class expression in ~A" args))
          (return-from parse-object-property-range
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLObjectPropertyRangeAxiom| prop descr)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)

          (values axiom rest)))

    (values nil args)))
        
               
   
(defun parse-functional-object-property (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |FunctionalObjectProperty|))
       
      (let ((annotations nil)
            (prop nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-functional-object-property: no object property expression in ~A" args))
          (return-from parse-functional-object-property
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLFunctionalObjectPropertyAxiom| prop)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)

          (values axiom rest)))

    (values nil args)))
                

(defun parse-inverse-functional-object-property (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |InverseFunctionalObjectProperty|))
       
      (let ((annotations nil)
            (prop nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-inverse-functional-object-property: no object property expression in ~A" args))
          (return-from parse-inverse-functional-object-property
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom| prop)))
          
          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                               annotations)
        
          (values axiom rest)))

    (values nil args)))


      
(defun parse-reflexive-object-property (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |ReflexiveObjectProperty|))
       
      (let ((annotations nil)
            (prop nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-reflexive-object-property: no object property expression in ~A" args))
          (return-from parse-reflexive-object-property
            (values nil rest)))
        
        (let ((axiom 
               (|OWLAPI-getOWLReflexiveObjectPropertyAxiom| prop)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                               annotations)

          (values axiom rest)))

    (values nil args)))


(defun parse-irreflexive-object-property (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |IrreflexiveObjectProperty|))
       
      (let ((annotations nil)
            (prop nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-irreflexive-object-property: no object property expression in ~A" args))
          (return-from parse-irreflexive-object-property 
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLIrreflexiveObjectPropertyAxiom| prop)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                               annotations)
        
          (values axiom rest)))

    (values nil args)))



(defun parse-symmetric-object-property (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |SymmetricObjectProperty|))
       
      (let ((annotations nil)
            (prop nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-symmetric-object-property: no object property expression in ~A" args))
          (return-from parse-symmetric-object-property
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLSymmetricObjectPropertyAxiom| prop)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
        
          (values axiom rest)))

    (values nil args)))


(defun parse-asymmetric-object-property (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |AsymmetricObjectProperty|))
       
      (let ((annotations nil)
            (prop nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-asymmetric-object-property: no object property expression in ~A" args))
          (return-from parse-asymmetric-object-property
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLAsymmetricObjectPropertyAxiom| prop)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
        
          (values axiom rest)))

    (values nil args)))


(defun parse-transitive-object-property (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |TransitiveObjectProperty|))
       
      (let ((annotations nil)
            (prop nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (let ((axiom 
               (|OWLAPI-getOWLTransitiveObjectPropertyAxiom| prop)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)

          (unless prop
            (when error-p 
              (owlapi:owlapi-parser-error "parse-transitive-object-property: no object property expression in ~A" args))
            (return-from parse-transitive-object-property
              (values nil rest)))
        
          (values axiom rest)))

    (values nil args)))

;;;
;;;
;;; 

(defun parse-data-property-axiom (args)
  (let ((axiom nil))

    (dolist (parser '(parse-sub-data-property-of
                      parse-equivalent-data-properties
                      parse-disjoint-data-properties
                      parse-data-property-domain
                      parse-data-property-range
                      parse-functional-data-property))

      (multiple-value-setq (axiom args)
          (funcall parser args))

      (when axiom 
        (return-from parse-data-property-axiom
          (values axiom args)))))

  (values nil args))


(defun parse-sub-data-property-expression (args)
  (parse-data-property-expression args))

(defun parse-super-data-property-expression (args)
  (parse-data-property-expression args))

(defun parse-sub-data-property-of (args &optional (error-p t))
  
  (if (and (f-consp (f-first args))
           (check-for args |SubDataPropertyOf|))
        
      (let ((sub-prop nil)
            (prop nil)
            (annotations nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))
        
        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (sub-prop args)
            (parse-sub-data-property-expression args))

        (unless sub-prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-sub-data-property-of: no sub data property expression in ~A" args))
          (return-from parse-sub-data-property-of
            (values nil rest)))
        
        (multiple-value-setq (prop args)
            (parse-super-data-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-sub-data-property-of: no super data property expression in ~A" args))
          (return-from parse-sub-data-property-of
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLDataSubPropertyAxiom| sub-prop prop)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
          
          (values axiom rest)))

    (values nil args)))


(defun parse-equivalent-data-properties (args &optional (error-p t))
  
  (if (and (f-consp (f-first args))
           (check-for args |EquivalentDataProperties|))
       
      (let ((annotations nil)
            (props nil)
            (prop nil)
            (done nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-data-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-equivalent-data-properties: no first data property expression in ~A" args))
          (return-from parse-equivalent-data-properties
            (values nil rest)))

        (push prop props)

        (multiple-value-setq (prop args)
            (parse-data-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-equivalent-data-properties: no second data property expression in ~A" args))
          (return-from parse-equivalent-data-properties
            (values nil rest)))

        (push prop props)

        (loop while (and args (not done) (f-consp args)) do

              (setf done t)
              
              (multiple-value-setq (prop args)
                  (parse-data-property-expression args))

              (when prop
                (push prop props)
                (setf done nil)))

        (let ((axiom 
               (|OWLAPI-getOWLEquivalentDataPropertiesAxiom| (reverse props))))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
          
          (values axiom rest)))

    (values nil args)))

(defun parse-disjoint-data-properties (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |DisjointDataProperties|))
       
      (let ((annotations nil)
            (props nil)
            (prop nil)
            (done nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-data-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-disjoint-data-properties: no first data property expression in ~A" args))
          (return-from parse-disjoint-data-properties
            (values nil rest)))

        (push prop props)

        (multiple-value-setq (prop args)
            (parse-data-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-disjoint-data-properties: no second data property expression in ~A" args))
          (return-from parse-disjoint-data-properties
            (values nil rest)))

        (push prop props)

        (loop while (and args (not done) (f-consp args)) do

              (setf done t)
              
              (multiple-value-setq (prop args)
                  (parse-data-property-expression args))

              (when prop
                (push prop props)
                (setf done nil)))

        (let ((axiom 
               (|OWLAPI-getOWLDisjointDataPropertiesAxiom| (reverse props))))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
          
          (values axiom rest)))

    (values nil args)))
        
        
(defun parse-data-property-domain (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |DataPropertyDomain|))
       
      (let ((annotations nil)
            (prop nil)
            (descr nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))
            
        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-data-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-property-domain: no data property expression in ~A" args))
          (return-from parse-data-property-domain
            (values nil rest)))

        (multiple-value-setq (descr args)
            (parse-class-expression args))

        (unless descr
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-property-domain: no class expression in ~A" args))
          (return-from parse-data-property-domain
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLDataPropertyDomainAxiom| prop descr)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)

          (values axiom rest)))

    (values nil args)))
        
(defun parse-data-property-range (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |DataPropertyRange|))
       
      (let ((annotations nil)
            (prop nil)
            (range nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-data-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-property-range: no data property expression in ~A" args))
          (return-from parse-data-property-range
            (values nil rest)))

        (multiple-value-setq (range args)
            (parse-data-range args))

        (unless range
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-property-range: no data range in ~A" args))
          (return-from parse-data-property-range
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLDataPropertyRangeAxiom| prop range)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
           
          (values axiom rest)))

    (values nil args)))
        
               
(defun parse-functional-data-property (args &optional (error-p t))

  (if (and (f-consp (f-first args))
           (check-for args |FunctionalDataProperty|))
       
      (let ((annotations nil)
            (prop nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-data-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-functional-data-property: no data property expression in ~A" args))
          (return-from parse-functional-data-property
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLFunctionalDataPropertyAxiom| prop)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
        
          (values axiom rest)))

    (values nil args)))
                

;;;
;;;
;;;

(defun parse-datatype-definition (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |DatatypeDefinition|))

      (let ((annotations nil)
            (datatype nil)
            (range nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (datatype args)
            (parse-datatype args))

        (unless datatype
          (when error-p 
            (owlapi:owlapi-parser-error "parse-datatype-definition: no datatype URI in ~A" args))
          (return-from parse-datatype-definition
            (values nil rest)))

        (multiple-value-setq (range args)
            (parse-data-range args))
        
        (unless range
          (when error-p 
            (owlapi:owlapi-parser-error "parse-datatype-definition: no data range in ~A" args))
          (return-from parse-datatype-definition
            (values nil rest)))
        
        (let ((axiom 
               (|OWLAPI-getOWLDatatypeDefinitionAxiom| datatype range)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
          
          (values axiom rest)))

    (values nil args)))                

;;;
;;;
;;; 


(defun parse-has-key (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |HasKey|))
      
      (let ((annotations nil)
            (class nil)
            (ops nil)
            (dps nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (class args)
            (parse-class-expression args))

        (unless class
          (when error-p 
            (owlapi:owlapi-parser-error "parse-has-key: no class expression in ~A" args))
          (return-from parse-has-key
            (values nil rest)))

        (cond (*owl-xml-mode* 

               (let ((dps nil)
                     (ops nil)
                     (prop nil))

                 (loop while (and args (f-consp args)) do

                       (setf prop nil)

                       (multiple-value-setq (prop args)
                           (parse-object-property-expression args nil))

                       (cond (prop 
                              (push prop ops))

                             (t
                              (multiple-value-setq (prop args)
                                  (parse-data-property-expression args))

                              (cond (prop
                                     (push prop dps))
                                    
                                    (t 

                                     (when error-p 
                                       (owlapi:owlapi-parser-error "parse-has-key: bad object or data property expression in ~A" args))
                                     (return-from parse-has-key
                                       (values nil rest)))))))

                 
                 (let ((axiom 
                        (|OWLAPI-getOWLHasKeyAxiom| class (reverse ops) (reverse dps))))

                   (create-annotations (lambda (x) 
                                         (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                                       annotations)
                   
                                   (values axiom rest))))
              
              (t

               (let ((args1 (f-first args))
                     (args2 (f-first (f-rest args))))

                 (cond #+:ignore
                       ((not (f-consp-p args1))
               
                        (when error-p 
                          (owlapi:owlapi-parser-error "parse-has-key: no list of object property expressions in ~A" args))
               
                        (return-from parse-has-key
                          (values nil rest)))

                       (t (let ((op nil)
                                (done nil))
                 
                            (loop while (and args1 (not done) (f-consp args1)) do
                       
                                  (setf done t)
              
                                  (multiple-value-setq (op args1)
                                      (parse-object-property-expression args1))

                                  (when op
                                    (push op ops)
                                    (setf done nil))))

                          (cond ((not (f-consp args2))
               
                                 (when error-p 
                                   (owlapi:owlapi-parser-error "parse-has-key: no list of data property expressions in ~A" args2))

                                 (return-from parse-has-key
                                   (values nil rest)))

                                (t

                                 (let ((dp nil)
                                       (done nil))
                 
                                   (loop while (and args2 (not done) (f-consp args2)) do
                       
                                         (setf done t)
              
                                         (multiple-value-setq (dp args2)
                                             (parse-data-property-expression args2))

                                         (when dp
                                           (push dp dps)
                                           (setf done nil))))

                                 (let ((axiom 
                                        (|OWLAPI-getOWLHasKeyAxiom| class (reverse ops) (reverse dps))))

                                   (create-annotations (lambda (x) 
                                                         (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                                                       annotations)
                        
                                   (values axiom rest))))))))))

    (values nil args)))

;;;
;;;
;;; 

(defun parse-assertion (args)
  (let ((axiom nil))

    (dolist (parser '(parse-class-assertion
                      parse-object-property-assertion
                      parse-data-property-assertion
                      
                      parse-same-individuals
                      parse-different-individuals
                      parse-negative-object-property-assertion

                      parse-negative-data-property-assertion))

      (multiple-value-setq (axiom args)
          (funcall parser args))

      (when axiom 
        (return-from parse-assertion
          (values axiom args))))

    (values nil args)))

#+:ignore
(defun parse-assertion (args)
  (let ((axiom nil))

    (dolist (parser '(parse-same-individuals
                      parse-different-individuals
                      parse-class-assertion

                      parse-object-property-assertion
                      parse-negative-object-property-assertion

                      parse-data-property-assertion
                      parse-negative-data-property-assertion))

      (multiple-value-setq (axiom args)
          (funcall parser args))

      (when axiom 
        (return-from parse-assertion
          (values axiom args))))

    (values nil args)))


(defun parse-source-individual-iri (args)
  (parse-individual args))

(defun parse-target-individual-iri (args)
  (parse-individual args))

(defun parse-target-value (args)
  (parse-literal args))

(defun parse-same-individuals (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (or (check-for args |SameIndividual|)
               (check-for args |SameIndividuals|)))
       
      (let ((annotations nil)
            (ind-iris nil)
            (ind-iri nil)
            (done nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (ind-iri args)
            (parse-individual args))

        (unless ind-iri
          (when error-p 
            (owlapi:owlapi-parser-error "parse-same-individuals: no first individual URI in ~A" args))
          (return-from parse-same-individuals
            (values nil rest)))

        (push ind-iri ind-iris)

        (multiple-value-setq (ind-iri args)
            (parse-individual args))

        (unless ind-iri
          (when error-p 
            (owlapi:owlapi-parser-error "parse-same-individuals: no second individual URI in ~A" args))
          (return-from parse-same-individuals
            (values nil rest)))

        (push ind-iri ind-iris)

        (loop while (and args (not done) (f-consp args)) do

              (setf done t)
              
              (multiple-value-setq (ind-iri args)
                  (parse-individual args))

              (when ind-iri
                (push ind-iri ind-iris)
                (setf done nil)))

        (let ((axiom 
               (|OWLAPI-getOWLSameIndividualsAxiom| (reverse ind-iris))))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)

          (values axiom rest)))

    (values nil args)))
  
(defun parse-different-individuals (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (or (check-for args |DifferentIndividual|)
               (check-for args |DifferentIndividuals|)))
       
      (let ((annotations nil)
            (ind-iris nil)
            (ind-iri nil)
            (done nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (ind-iri args)
            (parse-individual args))

        (unless ind-iri
          (when error-p 
            (owlapi:owlapi-parser-error "parse-different-individuals: no first individual URI in ~A" args))
          (return-from parse-different-individuals
            (values nil rest)))

        (push ind-iri ind-iris)

        (multiple-value-setq (ind-iri args)
            (parse-individual args))

        (unless ind-iri
          (when error-p 
            (owlapi:owlapi-parser-error "parse-different-individuals: no second individual URI in ~A" args))
          (return-from parse-different-individuals
            (values nil rest)))

        (push ind-iri ind-iris)

        (loop while (and args (not done) (f-consp args)) do

              (setf done t)
              
              (multiple-value-setq (ind-iri args)
                  (parse-individual args))

              (when ind-iri
                (push ind-iri ind-iris)
                (setf done nil)))

        (let ((axiom 
               (|OWLAPI-getOWLDifferentIndividualsAxiom| (reverse ind-iris))))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
          
          (values axiom rest)))
        
    (values nil args)))


(defun parse-class-assertion (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |ClassAssertion|))
       
      (let ((annotations nil)
            (ind-iri nil)
            (expression nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        ;;; error in OWLAPI 

        (cond (*use-owlapi-flipped-class-assertions-p* 
               
               (multiple-value-setq (ind-iri args)
                   (parse-individual args))

               (unless ind-iri
                 (when error-p 
                   (owlapi:owlapi-parser-error "parse-class-assertion: no individual URI in ~A" args))
                 (return-from parse-class-assertion 
                   (values nil rest)))
               
               (multiple-value-setq (expression args)
                   (parse-class-expression args))
               
               (unless expression
                 (when error-p 
                   (owlapi:owlapi-parser-error "parse-class-assertion: no class expression in ~A" args))
                 (return-from parse-class-assertion 
                   (values nil rest)))               
               
               (let ((axiom 
                      (|OWLAPI-getOWLClassAssertionAxiom| ind-iri expression)))

                 (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
                 
                 (values axiom rest)))

              (t 

               (multiple-value-setq (expression args)
                   (parse-class-expression args))
               
               (unless expression
                 (when error-p 
                   (owlapi:owlapi-parser-error "parse-class-assertion: no class expression in ~A" args))
                 (return-from parse-class-assertion 
                   (values nil rest)))
               
               (multiple-value-setq (ind-iri args)
                   (parse-individual args))

               (unless ind-iri
                 (when error-p 
                   (owlapi:owlapi-parser-error "parse-class-assertion: no individual URI in ~A" args))
                 (return-from parse-class-assertion 
                   (values nil rest)))
               
               (let ((axiom 
                      (|OWLAPI-getOWLClassAssertionAxiom| ind-iri expression)))

                 (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)
                 
                 (values axiom rest)))))

    (values nil args)))


(defun parse-object-property-assertion (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |ObjectPropertyAssertion|))
       
      (let ((annotations nil)
            (prop nil)
            (source-ind-iri nil)
            (target-ind-iri nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-property-assertion: no object property expression in ~A" args))
          (return-from parse-object-property-assertion
            (values nil rest)))

        (multiple-value-setq (source-ind-iri args)
            (parse-source-individual-iri args))

        (unless source-ind-iri
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-property-assertion: no source ind URI in ~A" args))
          (return-from parse-object-property-assertion
            (values nil rest)))
        
        (multiple-value-setq (target-ind-iri args)
            (parse-target-individual-iri args))

        (unless target-ind-iri
          (when error-p 
            (owlapi:owlapi-parser-error "parse-object-property-assertion: no target ind URI in ~A" args))
          (return-from parse-object-property-assertion
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLObjectPropertyAssertionAxiom| source-ind-iri prop target-ind-iri)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)

          (values axiom rest)))

    (values nil args)))



(defun parse-negative-object-property-assertion (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |NegativeObjectPropertyAssertion|))
       
      (let ((annotations nil)
            (prop nil)
            (source-ind-iri nil)
            (target-ind-iri nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-object-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-negative-object-property-assertion: no object property expression in ~A" args))
          (return-from parse-negative-object-property-assertion
            (values nil rest)))

        (multiple-value-setq (source-ind-iri args)
            (parse-source-individual-iri args))

        (unless source-ind-iri
          (when error-p 
            (owlapi:owlapi-parser-error "parse-negative-object-property-assertion: no source ind URI in ~A" args))
          (return-from parse-negative-object-property-assertion
            (values nil rest)))

        (multiple-value-setq (target-ind-iri args)
            (parse-target-individual-iri args))

        (unless target-ind-iri
          (when error-p 
            (owlapi:owlapi-parser-error "parse-negative-object-property-assertion: no target ind URI in ~A" args))
          (return-from parse-negative-object-property-assertion
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom| source-ind-iri prop target-ind-iri)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)

          (values axiom rest)))

    (values nil args)))



(defun parse-data-property-assertion (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |DataPropertyAssertion|))
       
      (let ((annotations nil)
            (prop nil)
            (source-ind-iri nil)
            (target-value nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-data-property-expression args))

        (unless prop
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-property-assertion: no data property expression in ~A" args))
          (return-from parse-data-property-assertion
            (values nil rest)))

        (multiple-value-setq (source-ind-iri args)
            (parse-source-individual-iri args))

        (unless source-ind-iri
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-property-assertion: no source ind URI in ~A" args))
          (return-from parse-data-property-assertion
            (values nil rest)))

        (multiple-value-setq (target-value args)
            (parse-target-value args))

        (unless target-value
          (when error-p 
            (owlapi:owlapi-parser-error "parse-data-property-assertion: no target value in ~A" args))
          (return-from parse-data-property-assertion
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLDataPropertyAssertionAxiom| source-ind-iri prop target-value)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)

          (values axiom rest)))

    (values nil args)))
  

(defun parse-negative-data-property-assertion (args &optional (error-p t))
  (if (and (f-consp (f-first args))
           (check-for args |NegativeDataPropertyAssertion|))
       
      (let ((annotations nil)
            (prop nil)
            (source-ind-iri nil)
            (target-value nil)
            (args (f-rest (f-first args)))
            (rest (f-rest args)))

        (multiple-value-setq (annotations args)
            (parse-annotations args))

        (multiple-value-setq (prop args)
            (parse-data-property-expression args))

        (unless prop
          (when error-p
            (owlapi:owlapi-parser-error "parse-negative-data-property-assertion: no data property expression in ~A" args))
          (return-from parse-negative-data-property-assertion
            (values nil rest)))

        (multiple-value-setq (source-ind-iri args)
            (parse-source-individual-iri args))

        (unless source-ind-iri
          (when error-p 
            (owlapi:owlapi-parser-error "parse-negative-data-property-assertion: no source ind URI in ~A" args))
          (return-from parse-negative-data-property-assertion
            (values nil rest)))

        (multiple-value-setq (target-value args)
            (parse-target-value args))

        (unless target-value
          (when error-p 
            (owlapi:owlapi-parser-error "parse-negative-data-property-assertion: no target value in ~A" args))
          (return-from parse-negative-data-property-assertion
            (values nil rest)))

        (let ((axiom 
               (|OWLAPI-getOWLNegativeDataPropertyAssertionAxiom| source-ind-iri prop target-value)))

          (create-annotations (lambda (x) 
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| axiom x))
                              annotations)

          (values axiom rest)))
    
    (values nil args)))

;;;
;;;
;;;

(defmacro with-owl-producer ((stream) &body body)
  `(let ((*producer* (get-owl-producer ,stream)))
     ,@body))

(defun sexpr-from-stream (stream)
  (with-owl-producer (stream)
    (let ((res 
           (get-expression *producer*)))
      (unless (eq res :eof)
        (f-tree-map res)))))

(defun parse-from-stream (stream)
  ;;; missnomer 
  (sexpr-from-stream stream))

(defun sexpr-time (url &optional print-p)
  (with-input-from-url (stream url)
    (let ((res (time (sexpr-from-stream stream))))
      (when print-p
        (pprint res))
      :ok)))

(defun get-f-list-from-url (url)
  (with-input-from-url (stream url :close-manually-p t)
    (with-owl-producer (stream)
      (get-expression *producer*))))

(defun f-tree-map (expr) 
  (if (f-consp expr) 

      (loop while (not done) 
            as done = t 
            as first = (f-first expr)
            when first
            collect 
            (progn
              (setf done nil)
              (f-tree-map first))
            do (setf expr (f-rest expr)))

    (progn 
      (unless (f-list-p expr)
        expr))))

;;;
;;;
;;; 

(owlapi-defun (|OWLAPI-parse|) (args &optional reasoner)
  (let* (;;(*package* (find-package :ts))
         (args (if (f-consp (f-first args))
                   args
                 (list args))))
    (with-reasoner (reasoner)

      (let ((description nil))

        (setf description
              (parse-ontology-document args :error-p nil))

        (unless description
          (multiple-value-setq (description args)
              (funcall #'parse-axiom args)))

        (if description 
            description
          (owlapi:owlapi-parser-error "|OWLAPI-parse|: unknown description ~A" args))))))

(owlapi-defun (|OWLAPI-parseNative|) (string &optional reasoner)
  (let* (;;(*package* (find-package :ts))
         (sexpr

          (with-input-from-string (stream string)
            (with-owl-producer (stream)
              (let ((res 
                     (get-expression *producer*)))
                (f-tree-map res))))))
            
    (let* ((id (|OWLAPI-parse| sexpr reasoner)))

      (cond ((numberp id) ; axiom was parsed? 
             
             (let ((ax (owlapi:find-owl-axiom id)))
               
               (setf (owlapi:told ax) string)
      
               (if (gethash string (owlapi:axioms-told *cur-reasoner*))
                   (push ax (gethash string (owlapi:axioms-told *cur-reasoner*)))
                 (setf (gethash string (owlapi:axioms-told *cur-reasoner*)) (list ax))))

             id)

            (t ; ontology was parsed

             id)))))

;;;
;;;
;;;

(defvar *global-expression* nil)

(defun owlapi-process-functional-ontology (ont-or-url
                                           &key 
                                           strict-syntax-p 
                                           use-flipped-class-assertions-p
                                           
                                           (ignore-import *ignore-import-p*)
                                           
                                           ignore-annotations 
                                           
                                           (merge-imported-ontologies-p *merge-imported-ontologies-p*)
                                           
                                           maintain-owlapi-axioms 
                                           
                                           kb-name
                                           reasoner-name
                                           
                                           ontology-name
                                           (init t) 
                                           (parser #'parse-ontology-document))

  (let* ((kb-name
          (intern 
           (owlapi:ensure-string
            (or kb-name 
                reasoner-name
                (if init
                    (make-url-from-filename kb-name)
                  (|OWLAPI-getCurrentReasoner|))))))

         (reasoner-name kb-name)

         (ontology-name 
          (or ontology-name (owlapi:get-temp-ontology-name)))

         (*use-owlapi-flipped-class-assertions-p* use-flipped-class-assertions-p)
         (*strict-owl-functional* strict-syntax-p)
         
         (*ignore-import-p* ignore-import)
         (*merge-imported-ontologies-p* merge-imported-ontologies-p))
    
    (|OWLAPI-newReasoner| reasoner-name t init)

    ;;; 
    ;;; if *merge-imported-ontologies-p*, then ontology already exists
    ;;; Otherwise, ontology-name = gensym and fresh
    ;;; 

    (unless (with-reasoner (reasoner-name) 
              (owlapi:find-owl-ontology ontology-name nil))
      (|OWLAPI-newOntology| ontology-name reasoner-name))
  
    ;;;
    ;;;
    ;;;

    (cond (maintain-owlapi-axioms
           (|OWLAPI-disableMemorySavingMode| reasoner-name))

          (t 
           (|OWLAPI-enableMemorySavingMode| ontology-name reasoner-name)))

    (cond (ignore-annotations 
           (|OWLAPI-ignoreAnnotations| reasoner-name))
          (t
           (|OWLAPI-keepAnnotations| reasoner-name)))

    (|OWLAPI-autoAddAxiomsTo| ontology-name reasoner-name)

    (multiple-value-bind (uri namespaces)

        (if (stringp ont-or-url)

            (with-input-from-url (stream ont-or-url)
              (with-owl-producer (stream)
                (let ((res 
                       (get-expression *producer*))
                      (*use-chopper-p* t))
                  (setf *global-expression* res)
                  (unless (eq res :eof)
                    (funcall parser res :uri kb-name)))))
          
          (funcall parser ont-or-url :uri kb-name))

      (declare (ignorable namespaces))

      (cond (uri 

             (when maintain-owlapi-axioms
               (|OWLAPI-loadOntology| ontology-name reasoner-name))

             (|OWLAPI-applyChanges| reasoner-name)
  
             uri)

            (t (owlapi:owlapi-parser-error "Unable to parse Ontology"))))))


;;;
;;;
;;;

(owlapi:defun1 owlapi-read-functional-ontology (fn &rest args 
                                           &key ontology-name reasoner-name kb-name &allow-other-keys)
  
  (let ((fn
         (if (owlapi:is-url-p fn)
             fn
           (make-url-from-filename fn))))

    (apply #'owlapi-process-functional-ontology 
           fn 
           
           :reasoner-name (or reasoner-name kb-name fn)
           :ontology-name ontology-name

           ;; strict-syntax-p 
           ;; use-flipped-class-assertions-p
           ;; (ignore-import *ignore-import-p*)
           ;; (merge-imported-ontologies-p *merge-imported-ontologies-p*)
           ;; maintain-owlapi-axioms 
           ;; ontology-name
	   ;; (init t) 
	   
	   :allow-other-keys t

           args)))


;;;
;;; old, but keep for compatibility
;;;
      
(owlapi:owlapi-defun (|OWLAPI-readFunctionalOntologyFile|) (fn &rest args
                                                      &key 
                                                      (ignore-import *ignore-import-p*)
                                                      &allow-other-keys)
  (let* ((*import-level* 0)

         (*ignore-import-p* ignore-import)

         (fn (make-url-from-filename fn))

         (*imported-ontologies* (list (intern (owlapi:ensure-string fn)))))

    (#+:racer-server without-duplicate-warnings 
     #-:racer-server progn

     (apply #'owlapi-read-functional-ontology fn 
            :parser #'parse-ontology-document 
            :allow-other-keys t
            args))))

(owlapi:owlapi-defun (|OWLAPI-readFunctionalOntologyDocument|) (url &rest args
                                                           &key 
                                                           (ignore-import *ignore-import-p*)
                                                           &allow-other-keys)
  (let* ((*import-level* 0)

         (*ignore-import-p* ignore-import)

         (*imported-ontologies* (list (intern (owlapi:ensure-string url)))))

    (#+:racer-server without-duplicate-warnings
     #-:racer-server progn

      (apply #'owlapi-read-functional-ontology url
             :parser #'parse-ontology-document 
             :allow-other-keys t
             args))))
