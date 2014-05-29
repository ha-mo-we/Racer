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

(defvar *documentation* nil)

(defvar *images* nil)

(defvar *doc-entry* nil)

(defvar *server-running* nil)

(defvar *published* (make-hash-table :test #'equalp))

(defvar *phrases* (make-hash-table :test #'equalp))

(defvar *labels* (make-hash-table))

(ts:nrql-defun (clear-all-documentation) ()
  (setf *documentation* nil
        *images* nil)
  (clrhash *phrases*)
  (clrhash *labels*)
  (clrhash *published*)
  t)

(defun find-doc-entry (name)
  (find name *documentation* :key #'cadar))

;;;
;;;
;;;

(defconstant +markup-mapping+
  (sort 
   (copy-list
    '(("b" :bold 1)
      ("i" :italic 1)
      ("u" :underline 1)
      ("em" :emph 1)
      ("tt" :tt 1)
      ("ic" :inlinecode 1)
      ("arg" :argument 1)

      ("s" :space 0)
      ("space" :space 0)
      ("at" :at 0)
      ("ocb" :open{ 0)
      ("ccb" :close{ 0)

      ("n" :newline 0)
      ("nl" :newline 0)
      ("rule" :rule 0)                             
      
      ("s00" :spacing00 1)
      ("s01" :spacing01 1)
      ("s10" :spacing10 1)
      ("s11" :spacing11 1)
      
      ("phrase" :phrase 1)
      ("x" :phrase 1)

      ("mail" :mail 1)
      ("url" :url 1)
      
      ("v" :verbatim 1)
      ("pc" :prettycode 1)
      
      ("link" :link 2)
      
      ("img" :img 4)
      ("cap" :caption 2)
      
      ("label" :label 1)
      ("ref" :ref 2)
      ("funref" :funref 1)

      ("index" :index 1)
                             
      ("p" :paragraph 1)
      ("block" :block 1)
      ("div" :block 2)
      
      ("l" :left 1)
      ("r" :right 1)
      ("c" :center 1)

      ("lf" :leftfloat 1)
      ("rf" :rightfloat 1)
      ("clear" :clear 0)
      
      ("table" :table 1)
      ("heading" :heading 1)
      ("row" :row 1)
      ("data" :data 1)

      ("ilist" :itemlist 1)
      ("nlist" :numberedlist 1)
      ("item" :item 1)

      ("dlist" :descrlist 1)
      ("dterm" :descrterm 1)
      ("ddef" :descrdef 1)
                             
      ("h1" :h1 1)
      ("h1*" :h1* 1)
      ("h2" :h2 1)
      ("h2*" :h2* 1)
      ("h3" :h3 1)
      ("h3*" :h3* 1)))

   #'>
   :key #'(lambda (x) (length (car x)))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defconstant +doc-slots+ 
    '((signature "Signature")
      (signatures "Signatures")
      (arguments "Arguments" )
      (returns "Returns")
      (description "Description")
      (remarks "Remarks")
      (examples "Examples")

      (see-also "See also")
      (delegates-to "Delegates to")
      (query-equivalent-of "Query equivalent of")
      (rule-equivalent-of "Rule equivalent of")
      (macro-for "Macro for")
      (corresponding-macro "Corresponding Macro")
      (synonym-for "Synonym for"))))

;;;
;;;
;;;

#+:cl-http
(defun regexp-replace (string pattern pattern-autom target &optional (placeholder "[^{}\\]*"))
  (let* ((string-buffer
          (make-array (length string)
                      :element-type 'character
                      :fill-pointer 0
                      :adjustable t))

         (pattern-content
          (search placeholder pattern))
         (target-content
          (search placeholder target))
         (pl-length
          (length placeholder))

         (chars-after-pattern-content
          (when pattern-content 
            (+ pl-length pattern-content)))
         (chars-after-target-content
          (when target-content 
            (+ pl-length target-content)))

         (rem-characters-from-end
          (when chars-after-pattern-content
            (- (length pattern) chars-after-pattern-content)))

         (target
          (if (and pattern-content target-content)
              (list (coerce (subseq target 0 target-content) 'list)
                    (coerce (subseq target chars-after-target-content) 'list))
            (coerce target 'list))))

    ;;; (break "~S" (list target-content pattern-content))

    (labels ((do-it (pos0)

               ;; (pprint string-buffer)

               (multiple-value-bind (pos len)
                   (find-regexp-in-string pattern-autom string  
                                          :start pos0)

                 (cond (pos 

                        ;; (format t "~&Match at pos ~D: ~S~%" pos string)
                        
                        (loop as char from pos0 to (1- pos) do
                              (vector-push-extend (aref string char)
                                                  string-buffer))

                        (if (and target-content pattern-content)

                            (let* ((content-start
                                    (when pattern-content 
                                      (+ -1 pos pattern-content)))
                                   (content-end
                                    (when content-start
                                      (- (+ pos len) rem-characters-from-end)))
                                   (content
                                    (when content-start
                                      (subseq string content-start content-end))))

                              (loop as char in (first target) do 
                                    (vector-push-extend char
                                                        string-buffer))
                              (loop as char in (coerce content 'list) do 
                                    (vector-push-extend char
                                                        string-buffer))
                              (loop as char in (second target) do 
                                    (vector-push-extend char
                                                        string-buffer))

                              ;; (pprint (list content-start content-end content))
                              
                              (do-it (+ pos len)))
                          
                          (progn 
                            (loop as char in target do 
                                  (vector-push-extend char
                                                      string-buffer))
                              
                            (do-it (+ pos len)))))

                       (t 

                        (loop as char from pos0 to (1- (length string)) do
                              (vector-push-extend (aref string char)
                                                  string-buffer)))))))

      (do-it 0)
      
      ;;; for some reason I have to return a copy of the string buffer here.... 
      (with-output-to-string (stream)
        (write-string (coerce string-buffer 'string) stream)))))

;;;
;;;
;;;

(defmacro with-doc-args ((entry) &body body)
  `(let* ((args ,entry)
          (label 
           (or 
            (assoc :label args)
            (list :label (gensym))))
          (title
           (second (assoc :title args)))
          (protected
           (or 
            (assoc :protected args)
            (list :protected nil)))
          (function
           (assoc :function args))
          (in-sections 
           (assoc :in-sections args))
          (argument-mapping
           (assoc :argument-mapping args))

          ;;;
          ;;;
          ;;;

          (concept-label
           (assoc :concept-label args))

          (relations
           (assoc :relations args))

          (fillers
           (assoc :fillers args))

          (abox 
           (assoc :abox args))

          (tbox 
           (assoc :tbox args))
           
          ;;;
          ;;;
          ;;;

          ,@(mapcar #'(lambda (slot)
                        (let ((slot (first slot)))
                          (list (intern (symbol-name slot))
                                `(assoc ,(intern (symbol-name slot) :keyword)
                                        args))))
                    +doc-slots+))

     (declare (ignorable
               
               ;;name  Removed RM Apr. 2014
               label protected in-sections function argument-mapping

               concept-label relations fillers abox tbox

               ,@(mapcar #'(lambda (x) (intern (symbol-name (first x))))
                         +doc-slots+)))

     ,@body))

;;;
;;; API
;;;

(ts:nrql-defun (add-doc-entry1) (&rest args)
  (add-doc-entry0 args))

(ts:nrql-defmacro (add-doc-entry :nrql-function add-doc-entry1))

(ts:nrql-defun (del-doc-entry1) (label) 
  (del-doc-entry0 label))

(ts:nrql-defmacro (del-doc-entry :nrql-function del-doc-entry1))

;;;
;;;
;;;

(ts:nrql-defun (add-doc-image-file1) (url type pathname)
  (pushnew (list url pathname type nil) *images*
           :test #'equal)
  
  (when *server-running* 
    (publish-image url pathname type)))

(ts:nrql-defmacro (add-doc-image-file :nrql-function add-doc-image-file1))

(ts:nrql-defun (add-doc-image-data-from-file1) (url type pathname)
  (let ((bytes

         (with-open-file (stream (get-image-pathname pathname) :direction :input
                                 :element-type '(unsigned-byte 8))
           (loop 
            as byte = (read-byte stream nil)
            when (not byte) return bytes
            collect byte into bytes))))
    
    (add-doc-image-data1 url type bytes)))

(ts:nrql-defmacro (add-doc-image-data-from-file :nrql-function add-doc-image-data-from-file1))

(ts:nrql-defun (add-doc-image-data1) (url type bytes)
  (pushnew (list url nil type bytes) *images*
           :test #'equal)
  
  (when *server-running* 
    (publish-image url nil type :bytes bytes)))

(ts:nrql-defmacro (add-doc-image-data :nrql-function add-doc-image-data1))

;;;
;;;
;;;

(ts:nrql-defun (add-doc-phrase1) (label string)
  (setf (gethash (symbol-name label) *phrases*) string))

(ts:nrql-defmacro (add-doc-phrase :nrql-function add-doc-phrase1))

;;;
;;;
;;;

(defun test ()
  (with-input-from-string (stream "1 2 3" ) 
    (loop as i = (read stream nil)
          when (not i) return numbers
          collect i into numbers)))

;;;
;;;
;;;

#+:cl-http
(defmethod doc-entry-url ((entry cons))
  (http::intern-url 
   (http:merge-url 
    (get-url entry)
    (http::local-context))))


#+:aserve
(defmethod doc-entry-url ((entry cons))
  (concatenate 'string
               "http://localhost:8080"
               (get-url entry)))


(defun retrieve-doc-entry-from-label (label)
  (find-if #'(lambda (x) 
               (equalp label 
                       (second (assoc :label x))))
           *documentation*))


(defun retrieve-doc-entry (args)
  (let ((label 
         (second (assoc :label args))))
    (retrieve-doc-entry-from-label label)))


(defun add-doc-entry0 (args)
  (let* ((found
          (retrieve-doc-entry args))
         
         (entry 
          (transform-doc-entry 
           args
           (second (assoc :type args))))

         (label
          (second (assoc :label entry))))

    (cond (found 
           
           (let* ((protected 
                   (second 
                    (assoc :protected found)))

                  (abox
                   (second
                    (assoc :abox found)))
                  (concept-label
                   (cdr
                    (assoc :concept-label found)))
                  (relations
                   (cdr
                    (assoc :relations found)))
                  (fillers
                   (cdr 
                    (assoc :fillers found))))
             
             (if protected
                 (error "Cannot override documentation for ~A" label)

               (setf *documentation*
                     (cons entry (delete found *documentation*))))
             
             (when (and abox (find-abox abox nil))

               (dolist (concept concept-label)
                 (forget-concept-assertion abox label concept))
    
               (dolist (relation relations)
                 (forget-role-assertion abox label (ts:to-keyword
                                                    (second relation))
                                        (first relation)))

               (dolist (filler fillers)
                 (forget-datatype-role-filler abox label (second filler) (first filler))))))
    
          (t
           
           (push entry *documentation*)))

    (let* ((abox
            (second
             (assoc :abox entry)))
           (tbox
            (second
             (assoc :tbox entry)))
           (concept-label
            (cdr
             (assoc :concept-label entry)))
           (relations
            (cdr
             (assoc :relations entry)))
           (fillers
            (cdr 
             (assoc :fillers entry))))

      ;;;
      ;;; Semantic Tagging
      ;;;
      
      (when (and tbox (not (find-tbox tbox nil)))
        (init-tbox tbox))
      
      (when (and abox (not (find-abox abox nil)))
        (init-abox abox (or tbox (current-tbox))))
      
      (dolist (concept concept-label)
        (add-concept-assertion abox label concept))
    
      (dolist (relation relations)
        (add-role-assertion abox label (ts:to-keyword
                                        (second relation))
                            (first relation)))

      (dolist (filler fillers)
        (role-is-used-as-datatype-property (first filler) (associated-tbox abox))
        (add-datatype-role-filler abox label (second filler) (first filler)))
    
      ;;;
      ;;;
      ;;;
    
      (when *server-running* 
        (publish-doc-entry entry :html))
    
      (dolist (media (remove :html (gethash label *published*)))
        (publish-doc-entry 
         entry
         media)))))

(defun del-doc-entry0 (label)
  (let* ((found
          (retrieve-doc-entry-from-label label))
         (protected 
          (second 
           (assoc :protected found))))
    
    (if found
        (if protected
            (error "Cannot delete protected entry ~A" label)
          (setf *documentation*
                (delete found *documentation*)))
      
      (error "Entry ~A not found" label))))
       
;;;
;;;
;;;

(defmethod render-doc-entries ((stream stream) (media (eql :html)) &optional (entries *documentation*))
  (let ((*stream* stream)
	(*package* (find-package :racer-user))
        (entries 
         ;;;(compute-document-structure entries)
         entries))

    (doctype)

    (html (html :xmlns "http://www.w3.org/1999/xhtml" 
            :lang "en")
       
      (header)

      (html (body) 

        (loop as entry in entries
              collect (render-doc-entry stream entry media))))))

(defmethod publish-doc-entries ((media (eql :html)) &optional (entries *documentation*))

  (setf *server-running* t)

  (let ((entries 
         ;;;(compute-document-structure entries)
         entries))

    (loop as entry in entries
          do (publish-doc-entry entry media)))

  (dolist (img *images*)
    (publish-image (first img) (second img) (third img) 
                   :bytes (fourth img))))

;;;
;;;
;;;

(defun get-image-pathname (url)
  (namestring 
   (make-pathname
    :directory '(:relative "images")
    :name url)))


#+:cl-http 
(defmethod publish-image ((url string) pathname type &key bytes)

  (if (not bytes)

      (http::export-url 
       (http::intern-url 
        (http:merge-url 
         url
         (http::local-context)))
       type
       :pathname (get-image-pathname pathname)
       :expiration `(:interval ,(* 24. 60. 60.))
       :public t)

    (http::export-url 
     (http::intern-url 
      (http:merge-url 
       url
       (http::local-context)))
     :computed
     :response-function
     #'(lambda (url stream) 
         (declare (ignorable url))
         (dolist (byte bytes)
           (write-byte byte stream))))))


(defmethod publish-doc-entry :before ((doc-entry cons) media)
  (pushnew media (gethash (second (assoc :label doc-entry))
                          *published*)))

#+:cl-http
(defmethod publish-doc-entry ((doc-entry cons) (media (eql :html)))

  (let ((url (doc-entry-url doc-entry)))

    ;; (format t "Publishing URL ~A...~%" url)

    (http::export-url url
                      :computed

                      :response-function 
                      #'(lambda (url stream) 
                          (declare (ignorable url))
                          (http::with-successful-response (stream :html :content-location url)
                            (let ((*stream* stream)
                                  (*doc-entry* doc-entry)
                                  (*package* (find-package :racer-user)))

                              (doctype)
                          
                              (html (html :xmlns "http://www.w3.org/1999/xhtml" 
                                      :lang "en")
                            
                                (header)
                            
                                (html (body) 
                              
                                  (render-doc-entry stream doc-entry :html))))))
                      ;; :expiration `(:interval ,(* 24. 60. 60.))
                      :cache-control `(:no-cache)
                      :public t
                      :language :en)))


#+:aserve 
(defmethod publish-image ((url string) pathname type &key bytes)
  (let ((url 
	 (if (char= (elt url 0) #\/)
	     url
	   (format nil "/~A" url))))
    
    (if (not bytes)

	(net.aserve:publish-file 
	 :path url
	 :file  (get-image-pathname pathname)
	 :content-type 
	 (ecase type
	   (:gif-image "image/gif")
	   (:jpeg-image "image/jpeg")))

      (net.aserve:publish :path url
			  :content-type 
			  (ecase type
			    (:gif-image "image/gif")
			    (:jpeg-image "image/jpeg"))
			  :function
			  #'(lambda (req ent)
			      (net.aserve:with-http-response (req ent)
                                                             (net.aserve:with-http-body (req ent
                                                                                             :external-format
                                                                                             ;;(crlf-base-ef :utf-8)
                                                                                             (crlf-base-ef :latin1))
                                                                                        (let* ((stream (net.aserve:request-reply-stream req)))
                                                                                          (dolist (byte bytes)
                                                                                            (write-byte byte stream))))))))))


#+:aserve
(defmethod publish-doc-entry ((doc-entry cons) (media (eql :html)))

  (let ((url (get-url doc-entry)))

    ;; (format t "Publishing URL ~A...~%" url)

    (net.aserve:publish :path url
			:content-type "text/html"
			:function
			#'(lambda (req ent)
			    (net.aserve:with-http-response (req ent)
                                                           (net.aserve:with-http-body (req ent
                                                                                           :external-format
                                                                                           ;;(crlf-base-ef :utf-8)
                                                                                           (crlf-base-ef :latin1))
                                                                                      (let* ((stream (net.aserve:request-reply-stream req))
                                                                                             (*stream* stream)
                                                                                             (*doc-entry* doc-entry)
                                                                                             (*package* (find-package :racer-user)))
				  
                                                                                        (doctype)
                          
                                                                                        (html (html :xmlns "http://www.w3.org/1999/xhtml" 
                                                                                                :lang "en")
                            
                                                                                          (header)
                            
                                                                                          (html (body) 
                              
                                                                                            (render-doc-entry stream doc-entry :html))))))))))

;;;
;;;
;;;

(defun m (tag content &rest attributes)
  `(:markup ,tag ,attributes ,content))

(defun c (content &rest attributes)
  `(:content ,attributes ,content))

;;;
;;;
;;;

(defmethod insert-space ((media (eql :html)))
  (render-content "!empty" :html))

(defmethod insert-at ((media (eql :html)))
  (render-content "@" :html))

(defmethod insert-{ ((media (eql :html)))
  (render-content "{" :html))

(defmethod insert-} ((media (eql :html)))
  (render-content "}" :html))


;;;
;;;
;;; 


(defmethod render-doc-entry ((stream stream) (doc-entry cons) (media (eql :html)))
  (render stream doc-entry (second (assoc :type doc-entry)) media))

;;;
;;;
;;;

(defun pretty-symbol-name (sym &optional with-bars-p)
  (if (keywordp sym)
      (let ((string
             (with-output-to-string (stream) 
               (write sym :stream stream))))
        (concatenate 'string ":"
                     (if (char= (elt string 1) #\|)
                         (progn 
                           (if with-bars-p
                               (concatenate 'string
                                            "|"
                                            (symbol-name sym)
                                            "|")
                             (symbol-name sym)))
                       (string-downcase (symbol-name sym)))))
    (let ((string 
           (with-output-to-string (stream) 
             (write sym :stream stream))))
      (if (char= (elt string 0) #\|)
          (progn 
            (if with-bars-p
                (concatenate 'string
                             "|"
                             (symbol-name sym)
                             "|")
              (symbol-name sym)))
	#+:mlisp
	(symbol-name sym)
	#-:mlisp 
        (string-downcase (symbol-name sym))))))

(defun render-reference (section media) 
  (html (a :href (get-url section))
    (if (symbolp section)
        (render-content (pretty-symbol-name section) media)
      (let ((title (second (assoc :title section))))
        (if (symbolp title)
            (render-content (pretty-symbol-name title) media)
          (render-content title media))))))

;;;
;;;
;;;    

(defvar *argument-mapping* nil)

(defun apply-argument-mapping (res)
  (ts:tree-map (lambda (x)
                 (typecase x
                   ((or string symbol)
                    (let ((found
                           (assoc x *argument-mapping*
                                  :test #'(lambda (x y)
                                            (let ((x (if (stringp x)
                                                         x
                                                       (format nil "~S" x)))
                                                  (y (if (stringp y)
                                                         y
                                                       (format nil "~S" y))))
                                              (string-equal x y))))))
                      (if found
                          (second found)
                        x)))
                   (otherwise x)))
               res))

;;;
;;;
;;;

(defmethod render ((stream stream) (doc-entry cons) (type (eql :computed)) 
                   (media (eql :html)))
  
  (let* ((function
          (second (assoc :function doc-entry)))

         (title
          (second (assoc :title doc-entry))))

    (html (h1) 
      (content "Computed Server Page")
      (insert-space :html)
      (render-content title :html))

    (render-in-sections stream doc-entry :html)  

    (html (h2) 
      (content "Executing MiniLisp Script:"))
    
    (with-mode (:sequence)
      (html (pre)
        (pprint function stream)))

    (html1 (hr))

    (html (h2) 
      (content "Result:"))

    (let ((*standard-output* stream))
      (evaluate1 function))))


(defmethod render ((stream stream) (doc-entry cons) (type (eql :macro)) 
                   (media (eql :html)))

  (render-function-or-macro stream "Macro" doc-entry :html))

(defmethod render ((stream stream) (doc-entry cons) (type (eql :env-macro)) 
                   (media (eql :html)))

  (render-function-or-macro stream "Environment Establishing Macro" doc-entry :html))


(defmethod render-in-sections ((stream stream) (doc-entry cons) (medi (eql :html)))

  (let ((sections (get-in-sections doc-entry)))

    (when sections

      (html (h3)
        (content "Referenced in:")
  
        (dolist (section sections)
          (with-mode (:left) 
            (render-reference section :html)))))))

    
(defmethod render ((stream stream) (doc-entry cons) (type (eql :function))
                   (media (eql :html)))

  (render-function-or-macro stream "Function" doc-entry :html))


(defmethod render ((stream stream) (doc-entry cons) (type (eql :synonym-function))
                   (media (eql :html)))

  (render-function-or-macro stream "Synonym Function" doc-entry :html))


(defmethod render ((stream stream) (doc-entry cons) (type (eql :synonym-macro))
                   (media (eql :html)))

  (render-function-or-macro stream "Synonym Macro" doc-entry :html))


(defun ends-with-p (substring string)
  (let ((pos (search substring string :from-end t)))
    (and pos
         (= pos
            (- (length string) 
               (length substring))))))


(defmethod render-function-or-macro  ((stream stream) type (doc-entry cons)  
                                      (media (eql :html)))

  (let ((pred-p nil))

    (declare (ignorable pred-p))
  
    (with-doc-args (doc-entry)

      (let ((*argument-mapping*
             (cdr argument-mapping)))

        (html (h1) 
          (content type)
          (let ((name (pretty-symbol-name title)))
            (render-content name :html)
            (when (or (ends-with-p "?" name)
                      (ends-with-p "-p" name))
              (setf pred-p t)
              (content " (Predicate)"))))

        (render-in-sections stream doc-entry :html)    

        (let ((count 0))
      
          (html (table :style "width:100%")

            (dolist (slot +doc-slots+)

              (let* ((descr (second slot))
                     (slot
                      (intern
                       (format nil "~A" (first slot))
                       :keyword))
                     (content 
                      (cdr 
                       (assoc slot doc-entry))))

                ;; (pprint (list type slot content))

                (cond ((and (string-equal type "Macro")
                            (eq slot :remarks)
                            (or (not content)
                                (equal content '(nil))))

                       (incf count)
                   
                       (render-slot stream slot descr
                                    (transform-markup "Please recall that quotation (i.e., using the quote character @tt{'} in front of macro arguments) is not necessary for macros.")
                                    media 
                                    :count count))
                  
                      ((and (string-equal type "Macro")
                            (eq slot :description)
                            (or (not content)
                                (equal content '(nil))))

                       (incf count)
                   
                       (render-slot stream slot descr
                                    (transform-markup
                                     (format nil "This the macro version of the function @s10{@funref{~A}.} Please consult the documentation of this function for further details." 
                                             (pretty-symbol-name (second (assoc :macro-for doc-entry)))))
                                    media 
                                    :count count))
                  
                      #+:ignore
                      ((and pred-p (eq slot :returns))

                       (incf count)

                       (render-slot stream slot descr '(:boolean) media
                                    :count count))

                      ((and content
                            (not (equal content '(nil))))
              
                       (incf count)

                       (render-slot stream slot descr content media
                                    :count count)))))))))))


(defmethod render ((stream stream) (doc-entry cons) (type (eql :section))
                   (media (eql :html)))

  (let ((title (second (assoc :title doc-entry))))

    (html (h1) 
      (content "Section")
      (content title))

    (render-in-sections stream doc-entry :html)    
  
    (let ((descr (second (assoc :description doc-entry))))
      (when descr
        (render-content descr
                        :html)))

    (let* ((label (get-label doc-entry))
           (subsections
            (remove-if-not #'(lambda (x) 
                               (member label (get-in-sections x)))
                           *documentation*)))

      ;;(pprint subsections stream)
      ;;(pprint (sort-entries subsections))

      (let ((more 
             (sort-entries (remove-if-not #'is-section-p subsections)
                           label)))

        (when more

          (html (hr) )

          (html (h3) 
            (content "Sub Sections"))

          (html (hr) )

          (dolist (subsection more)
            (html (h3)          
              (render-reference subsection :html)))))

      (let ((more (sort-entries (remove-if #'is-section-p subsections)
                                label)))

        (when more

          (html (hr) )

          (html (h3) 
            (content "Entries"))
      
          (dolist (entry more)
            (html (h3)          
              (render-reference entry :html))))))))
           

(defun is-section-p (entry)
  (eq (second (assoc :type entry)) :section))

;;;
;;;
;;;

(defmethod render-slot ((stream stream) (slot symbol) (descr string) content (media (eql :html))
                        &key (count 0))

  (html (tr :class (if (evenp count) "even" "odd"))
      
    (html (td :style "width:20%" :valign "baseline")
      (content (format nil "~A" descr)))
      
    (html (td :valign "baseline")
      (render-content content media))))


(defmethod render-slot ((stream stream) (slot (eql :examples)) (descr string) content (media (eql :html))
                        &key (count 0))

  (html (tr :class (if (evenp count) "even" "odd"))
      
    (html (td :style "width:20%" :valign "baseline")
      (content (format nil "~A" descr)))
      
    (html (td :valign "baseline")
      (render-content 
       (cons :examples-description-list
             content)
       media))))


(defmethod render-slot ((stream stream) (slot (eql :arguments)) (descr string) content (media (eql :html))
                        &key (count 0))

  (html (tr :class (if (evenp count) "even" "odd"))
      
    (html (td :style "width:20%" :valign "baseline")
      (content (format nil "~A" descr)))
      
    (let ((content
           (apply-argument-mapping content)))
      (html (td :valign "baseline")
        (render-content 
         (cons :argument-description-list
               content)
         media)))))


(defmethod render-slot ((stream stream) (slot (eql :returns)) (descr string) content (media (eql :html))
                        &key (count 0))

  (html (tr :class (if (evenp count) "even" "odd"))
      
    (html (td :style "width:20%" :valign "baseline")
      (content (format nil "~A" descr)))
      
    (html (td :valign "baseline")
      (render-content 
       (cons :return-description-list
             content)
       media))))



(defmethod render-slot ((stream stream) (slot (eql :signature)) (descr string) content (media (eql :html))
                        &key (count 0))

  (html (tr :class (if (evenp count) "even" "odd"))
      
    (html (td :style "width:20%" :valign "baseline")
      (content (format nil "~A" descr)))
      
    (let ((content
           (apply-argument-mapping content)))
      (html (td :valign "baseline")
        (render-content
         (m :prettycode (c (format nil "~S" content)) )
         media)))))

(defmethod render-slot ((stream stream) (slot (eql :signatures)) (descr string) content (media (eql :html))
                        &key (count 0))

  (html (tr :class (if (evenp count) "even" "odd"))
      
    (html (td :style "width:20%" :valign "baseline")
      (content (format nil "~A" descr)))
      
    (html (td :valign "baseline")
      (html (ol)
        (dolist (signature content)
          (let ((signature
                 (apply-argument-mapping signature)))
            (render-content
             (m :item (m :prettycode 
                         (c (format nil "~S" signature))))
             media)))))))

(defmethod render-reference-slot ((stream stream) slot references (media (eql :html))
                                  &key (count 0))

  (html (tr :class (if (evenp count) "even" "odd"))
      
    (html (td :style "width:20%" :valign "baseline")
      (content (format nil "~A" slot)))
      
    (html (td :valign "baseline")
      (dolist (ref references)
        (with-mode (:left)
          (render-reference ref :html))))))


(defmethod render-slot ((stream stream) (slot (eql :see-also)) (descr string) content (media (eql :html))
                        &key (count 0))

  (render-reference-slot stream descr content :html :count count))

(defmethod render-slot ((stream stream) (slot (eql :synonym-for)) (descr string) content (media (eql :html))
                        &key (count 0))

  (render-reference-slot stream descr content :html :count count))

(defmethod render-slot ((stream stream) (slot (eql :macro-for)) (descr string) content (media (eql :html))
                        &key (count 0))

  (render-reference-slot stream descr content :html :count count))

(defmethod render-slot ((stream stream) (slot (eql :corresponding-macro)) (descr string) content (media (eql :html))
                        &key (count 0))

  (render-reference-slot stream descr content :html :count count))

(defmethod render-slot ((stream stream) (slot (eql :rule-equivalent-of)) (descr string) content (media (eql :html))
                        &key (count 0))

  (render-reference-slot stream descr content :html :count count))

(defmethod render-slot ((stream stream) (slot (eql :query-equivalent-of)) (descr string) content (media (eql :html))
                        &key (count 0))

  (render-reference-slot stream descr content :html :count count))


;;;
;;;
;;;
   
(defun html-url (string)
  (ts:string-substitute
   string
   '((" " "-" nil)
     ("?" "-predicate" nil))))
                   
(defmethod get-url ((entry cons))
  (string-downcase
   (html-url
    (format nil "/~A.html"
            (second (assoc :label entry))))))
  
(defmethod get-url ((entry symbol))
  (string-downcase
   (html-url
    (format nil "/~A.html"
            entry))))
                                     
(defmethod get-label ((entry cons))
  (second (assoc :label entry)))

(defmethod get-in-sections ((entry cons))
  (mapcar #'(lambda (x) 
              (if (consp x)
                  (first x) 
                x))
          (cdr (assoc :in-sections entry))))

;;;
;;;
;;; 

(defun register-label (label)
  (setf (gethash (ts:to-keyword label) *labels*)
        (second (assoc :label *doc-entry*))))

(defun lookup-label (label)
  (gethash (ts:to-keyword label) *labels*))

;;;
;;;
;;; 

(defun transform-doc-entries (&optional (entries *documentation*))
  (loop as entry in entries
        as type = (second (assoc :type entry))
        collect (transform-doc-entry entry type)))

(defun transform-markup (expr)
  (etypecase expr
    (string 
     (transform-string-markup expr))
    (symbol expr)
    (list 
     (mapcar #'transform-markup expr))
    (number expr)))

(defun transform-string-markup (string)
  (let* ((pos (position #\@ string))
         (pos2 (and pos
                    (not (zerop pos))
                    (char= (elt string (1- pos)) #\\))))

    (cond ((and pos (not pos2))
           
           (let ((markup
                  (find-if #'(lambda (x) 
                               (string-equal 
                                (subseq string (1+ pos) 
                                        (min (+ 1 pos (length (first x)))
                                             (length string)))
                                (first x)))

                           +markup-mapping+)))
	     
             (cond (markup
                    (let ((pos2 (position-if-not #'ts:whitespace-char-p string)))

                      (multiple-value-bind (args rest)
                          (extract-args-and-continue (subseq string (+ 1 (length (first markup)) pos))
                                                     (third markup))
                        
                        (if (or (not pos2) (>= pos2 pos))
                            
                            (append 
                             `((:markup
                                ,(second markup)
                                nil
                                ,@(mapcar #'transform-string-markup args)))
                             (transform-string-markup rest))
                          
                          (append 
                           `((:content ()
                              ,(subseq string 0 pos))
                             (:markup
                              ,(second markup)
                              nil
                              ,@(mapcar #'transform-string-markup args)))
                           (transform-string-markup rest))))))

                   (t (error "Bad markup found: ~S" (subseq string pos 10))))))

          (t 
           (let ((string (ts:shrink-whitespaces  string)))
             (unless (string= string "")
               `((:content () ,string))))))))


(defun extract-args-and-continue (string &optional (n 0))
  (let ((m (length string)))

    (labels ((get-argument (string pos)

               (let ((count 0)
                     (start nil)
                     (end nil))
               
                 (loop as pos from pos to (1- m) do
                
                       (let* ((char (elt string pos))
                              (open-p (char= char #\{))
                              (close-p (char= char #\})))

                         (cond (close-p 

                                (decf count)

                                (when (zerop count)
                                  (setf end pos)
                                  (return-from get-argument
                                    (values (subseq string (1+ start) end)
                                            (1+ end)))))

                               (open-p 

                                (unless start
                                  (setf start pos))

                                (incf count))

                               (t 

                                (when (and (not (ts:whitespace-char-p char))
                                           (not start))
                                  (error "Found bad non-whitespace content before argument: ~A" 
                                         (subseq string pos (min (+ pos 20) m))))))))

                 nil)))
    
      (let* ((args nil)
             (pos 0))
        
        (dotimes (i n)
        
          (multiple-value-bind (arg pos2)
              (get-argument string pos)

            (unless arg
              (error "Error: ~Ath argument is missing in ~A" (1+ i)
                     (subseq string pos (min (+ pos 20) m))))

            (push arg args)

            (setf pos pos2)))

        (values (nreverse args) (subseq string pos))))))

;;;
;;;

(defparameter *old-pprint-dispatch* (copy-pprint-dispatch))

(defparameter *new-pprint-dispatch* (copy-pprint-dispatch))

(defvar *doc-readtable* (copy-readtable *readtable*))

(setf (readtable-case *doc-readtable*) :preserve)

;;;
;;;
;;;

(defmethod render-content1 (string (media (eql :html)))
  (render-content (transform-string-markup string) media))
                                           
(defmethod render-content (content (media (eql :html)))
  (labels ((do-it (content)
             (etypecase content
               (string 
                (content content))
               (symbol 
                ;;(let ((*print-case* :preserve))
                (html (tt)
                  (content (pretty-symbol-name content))))
               (number 
                (content content))
               (list 
                (let ((op (first content)))
                  (case op
                    (:markup
                     (setf content (cdr content))
                     (let ((op (first content))
                           (args (second content))
                           (rest (cddr content)))

                       (declare (ignorable args))

                       (ecase op

                         (:spacing00
                          (with-mode (:sequence)
                            (mapc #'do-it rest)))
                         (:spacing01
                          (with-mode (:right)
                            (mapc #'do-it rest))) 
                         (:spacing10
                          (with-mode (:left)
                            (mapc #'do-it rest)))
                         (:spacing11
                          (with-mode (:normal)
                            (mapc #'do-it rest)))

                         (:phrase
                          (let* ((label 
                                  (extract-simple-content 
                                   (first rest)))
                                 (phrase 
                                  (gethash label *phrases*)))
                            (if phrase
                                (do-it (transform-string-markup phrase))
                              (error "Cannot find phrase ~S" label))))

                         (:space
                          (insert-space media))
                         (:at
                          (insert-at media))
                         (:open{
                          (insert-{ media))
                         (:close{
                          (insert-} media))
                         
                         (:bold
                          (html (b) 
                            (mapc #'do-it rest)))
                         (:inlinecode
                          (html (tt) 
                            (let ((rest
                                   (apply-argument-mapping rest)))
                              (mapc #'do-it rest))))
                         (:argument
                          (html (tt) 
                            (let ((rest
                                   (apply-argument-mapping rest)))
                              (mapc #'do-it rest))))
                         (:tt 
                          (html (tt) 
                            (mapc #'do-it rest)))
                         (:italic 
                          (html (i) 
                            (mapc #'do-it rest)))
                         (:emph 
                          (html (em) 
                            (mapc #'do-it rest)))
                         (:underline 
                          (html (u) 
                            (mapc #'do-it rest)))
                         (:mail 
                          (html (tt) 
                            (mapc #'do-it rest)))
                         (:url 
                          (html (tt) 
                            (mapc #'do-it rest)))
                         
                         (:verbatim
                          (with-mode (:normal)
                            (html (pre) 
                              (mapc #'do-it rest))))

                         (:prettycode
                          (with-mode (:normal)
                            (html (pre)
                              (let ((code 
                                     (extract-simple-content 
                                      (first rest))))

                                (loop 

                                 (multiple-value-bind (res cont) 
                                     (let ((*readtable* *doc-readtable*))
                                       (read-from-string code nil))

                                   (setf res 
                                         (apply-argument-mapping res))

                                   (if res
                                       
                                       (let ((*print-pretty* t)
                                             (*print-pprint-dispatch* *new-pprint-dispatch*))

                                         (set-pprint-dispatch 'symbol
                                                              #'(lambda (stream &rest args)
                                                                  (declare (ignorable args))
                                                                  (when (keywordp (first args))
                                                                    ;;(write-char #\: stream)
                                                                    )
                                                                  (write-string
                                                                   (let ((*print-pretty* nil)
                                                                         (*print-pprint-dispatch* *old-pprint-dispatch*))
                                                                     (pretty-symbol-name (first args)))
                                                                   stream)))
                                         (pprint res *stream*))

                                     (return))

                                   (if cont
                                       (setf code (subseq code cont))
                                     (return))))))))
                                         
                         (:label 
                          (let ((label (extract-simple-content (first rest))))
                            (html (a :name label)
                              (register-label label))))
                         (:ref 
                          (let* ((label (extract-simple-content (first rest)))
                                 (ref 
                                  (format nil "~A#~A" 
                                          (get-url (lookup-label label))
                                          label)))
                            (html (a :href ref)
                              (mapc #'do-it (cdr rest)))))
                         (:funref 
                          (with-mode (:left)
                            (let* ((label (extract-simple-content (first rest)))
                                   (ref 
                                    (get-url (intern label))))
                              (html (a :href ref)
                                (do-it label)))))

                         (:index 
                          (let ((term (extract-simple-content (first rest))))
                            (declare (ignorable term))
                            (mapc #'do-it (cdr rest))))

                         (:link
                          (html (a :href (extract-simple-content (first rest)))
                            (mapc #'do-it (cdr rest))))

                         (:img
                          (html (img :src (extract-simple-content (first rest))
                                     :alt (extract-simple-content (second rest))
                                     :style (let ((width 
                                                   (extract-simple-content (third rest))))
                                              (when width 
                                                (format nil "width:~A" width)))
                                     :height (extract-simple-content (fourth rest)))
                            (mapc #'do-it (cddddr rest))))
                         
                         (:caption
                          (html (table)
                            (html (tr) 
                              (html (td) 
                                (do-it (first rest))))
                            (html (tr)
                              (html (td :style "text-align:center")
                                (do-it `(:markup :bold 
                                         nil
                                         (,(second rest))))))))

                         (:block 
                             (with-div (:style "text-align:justify")
                               (html (p) 
                                 (mapc #'do-it rest))))

                         (:paragraph 
                          (html (p) 
                            (mapc #'do-it rest)))

                         (:div 
                          (with-div ((extract-simple-content (first rest)))
                            (mapc #'do-it (cdr rest))))

                         (:left 
                          (with-div (:style "float:left")
                            (mapc #'do-it rest))
                          (with-div (:style "clear:both;")
                            ))                          
                         (:right 
                          (with-div (:style "float:right")
                            (mapc #'do-it rest))
                          (with-div (:style "clear:both;")
                            ))
                         (:center
                          (html (center)
                            (mapc #'do-it rest)))
                         (:leftfloat
                          (with-div (:style "float:left")
                            (mapc #'do-it rest)))
                         (:rightfloat
                          (with-div (:style "float:right")
                            (mapc #'do-it rest)))
                          
                         (:clear
                          (with-div (:style "clear:both;")
                            ))
                         
                         (:newline
                          (html1 (br)))

                         (:rule
                          (html1 (hr)))

                         ((:h1 :h1*)
                          (html (h1) 
                            (mapc #'do-it rest)))

                         ((:h2 :h2*)
                          (html (h2) 
                            (mapc #'do-it rest)))

                         ((:h3 :h3*)
                          (html (h3) 
                            (mapc #'do-it rest)))
                         
                         (:table
                          (html (table :border 1) 
                            (mapc #'do-it rest)))
                         (:heading
                          (html (th) 
                            (mapc #'do-it rest)))
                         (:row
                          (html (tr) 
                            (mapc #'do-it rest)))
                         (:data
                          (html (td) 
                            (mapc #'do-it rest)))
                         
                         (:itemlist
                          (html (ul) 
                            (mapc #'do-it rest)))
                         (:numberedlist
                          (html (ol) 
                            (mapc #'do-it rest)))
                         (:descrlist
                          (html (dl) 
                            (mapc #'do-it rest)))
                         (:descrterm
                          (html (dt) 
                            (mapc #'do-it rest)))
                         (:descrdef
                          (html (dd) 
                            (mapc #'do-it rest)))
                         (:item
                          (html (li) 
                            (mapc #'do-it rest))))))                         

                    (:content
                     (mapc #'do-it (cddr content)))

                    (:description-list
                     (mapc #'(lambda (x) 
                               (let ((topic (first x))
                                     (content (second x)))
                                 (when topic (do-it topic))
                                 (when content (do-it content))
                                 (html1 (br))))
                           (rest content)))

                    (:examples-description-list
                     (mapc #'(lambda (x) 
                               (let ((topic (first x))
                                     (content (cdr x)))
                                 (when topic (do-it topic))
                                 (if (ts:tree-find content :content)
                                     (do-it content)
                                   (dolist (content (if (consp (first content))
                                                        content
                                                      (list content)))
                                     (if (stringp content)
                                         (do-it content)
                                       (do-it
                                        (m :prettycode (c (format nil "~S" content)) )))))
                                 (html1 (br))))
                           (rest content)))

                    (:argument-description-list
                     (mapc #'(lambda (x) 
                               (let ((arg (first x))
                                     (type (second x))
                                     (type-description (third x)))

                                 (declare (ignorable type-description))

                                 (if (consp arg)
                                     (progn 
                                       (do-it (m :bold (first arg)))
                                       (with-mode (:leftright)
                                         (do-it "(default:"))
                                       (with-mode (:left)
                                         (do-it
                                          (second arg)))
                                       (with-mode (:right)
                                         (do-it ")")))
                                   (do-it (m :bold arg)))

                                 (when type 
                                   (with-mode (:left)
                                     (do-it "of type")
                                     ;; hand generated? see demo-function .... 
                                     (if (and (consp type)
                                              (consp (first type))
                                              (eq (caar type) :content))
                                         (do-it type)
                                       (do-it
                                        (m :bold
                                           (m :inlinecode (c (format nil "~S" type))))))))
                                 
                                 (when type-description
                                   (html1 (br))
                                   (do-it "Description: ")
                                   (do-it type-description))
                                 
                                 (html1 (br))
                                 (html1 (br))))
                           (rest content)))

                    (:return-description-list
                     (if (not (third content))

                         ;; hand authored (see demo-function.lisp)
                         (do-it (cdr content))

                       ;; generated - no multiple values yet... 
                       ;; could be a list 
                       (let ((type
                              (second content))
                             (type-description 
                              (third content)))
                         (do-it (m :bold
                                   (m :inlinecode
                                      (c (format nil "~S" type)))))
                         (html1 (br))
                         (do-it "Description: ")
                         (do-it type-description))))
                    
                    (otherwise
                     (if (listp content)
                         (mapc #'do-it content)
                       (error "Bad markup found: ~S" content)))))))))
    
    (do-it content)))

(defun extract-simple-content (content)
  (cond ((null content) nil)
        ((consp content)
         (cond ((eq (first content) :content)
                (third content))
               (t 
                (extract-simple-content (first content)))))
        (t
         (error "No simple content in ~S" content))))

;;;
;;;
;;;         

(defmethod transform-doc-entry ((entry cons) (type (eql :computed)))
  (get-doc-entry :computed entry))

(defmethod transform-doc-entry ((entry cons) (type (eql :macro)))
  (get-doc-entry :macro entry))

(defmethod transform-doc-entry ((entry cons) (type (eql :env-macro)))
  (get-doc-entry :env-macro entry))

(defmethod transform-doc-entry ((entry cons) (type (eql :function)))
  (get-doc-entry :function entry))

(defmethod transform-doc-entry ((entry cons) (type (eql :synonym-function)))
  (get-doc-entry :synonym-function entry))

(defmethod transform-doc-entry ((entry cons) (type (eql :synonym-macro)))
  (get-doc-entry :synonym-macro entry))

(defun get-doc-entry (type1 entry)
  (with-doc-args (entry)

    `(,label
      (:original-entry ,entry)
      (:title ,(or title (second label)))
      (:type ,type1)      
      ,protected      
 
      ,@(when in-sections (list in-sections))
      ,@(when function (list function))
      ,@(when (cdr argument-mapping) (list argument-mapping))

      ,@(when concept-label
          (list concept-label))
      ,@(when relations
          (list relations))
      ,@(when fillers 
          (list fillers))
      ,@(when abox
          (list abox))
      ,@(when tbox
          (list tbox))

      ,@(mapcan #'(lambda (slot) 
                    (let* ((slot1 (first slot))
                           (slot
                            (assoc (intern (symbol-name slot1)
                                           :keyword)
                                   entry)))
                      (when slot
                        (case slot1
                          (signature 
                           (list
                            (if (eq (second slot) :auto)
                                (cons :signature (ts::ensure-list (get-lambda title)))
                              (cons :signature (second slot)))))
                          (signatures
                           (list slot))
                          (otherwise
                           (let ((slot 
                                  (transform-markup slot)))
                             (when slot
                               (list slot))))))))
                +doc-slots+))))

(defmethod transform-doc-entry ((entry cons) (type (eql :section)))
  (with-doc-args (entry)
    `(,label 
      (:title ,(or title (second label)))
      (:type ,type)
      ,protected   
    
      ,@(when in-sections (list in-sections))
      ,@(when concept-label
          (list concept-label))
      ,@(when relations
          (list relations))
      ,@(when fillers 
          (list fillers))
      ,@(when abox
          (list abox))
      ,@(when tbox
          (list tbox))
      ,@(when description (list (transform-markup description))))))
       

;;;
;;;
;;;

(defun sort-entries (entries section) 
  (labels ((pos-of (item)
             (let ((sections 
                    (remove-if-not #'consp
                                   (cdr (assoc :in-sections item)))))
               (second (assoc section sections)))))

    (let* ((l1 (remove-if #'pos-of entries))
           (l2 (remove-if-not #'pos-of entries))
           (n (length entries))
           (entries
            (sort (copy-list l1)
                  #'string-lessp 
                  :key #'(lambda (x) 
                           (let ((x (second (assoc :title x))))
                             (if (stringp x) x (symbol-name x))))))
           (entries (append entries (make-list (length l2)))))                                  
           
      (dolist (item l2)
        (let* ((pos (pos-of item)))
          (if (<= n pos)
              (setf entries
                    (nconc entries (list item)))
            (setf entries
                  (append 
                   (subseq entries 0 pos)
                   (cons item
                         (subseq entries pos)))))))

      (remove nil entries))))
                     
;;;
;;;
;;;

(defun html-doc (&optional (stream *standard-output*))
  (render-doc-entries stream :html))

(defun compute-document-structure (entries)
  (let ((res nil)
        (sections nil))

    (labels ((do-it (entries)

               (when entries

                 (let* ((next-level 
                         (remove-if-not
                          #'(lambda (x)
                              (let ((in-sections
                                     (mapcar #'(lambda (x) 
                                                 (if (consp x)
                                                     (first x) 
                                                   x))
                                             (cdr (assoc :in-sections x)))))
                                (or (not in-sections)
                                    (subsetp in-sections sections))))
                          entries))

                        (next-level
                         (sort-entries next-level nil)))

                   (dolist (item next-level)
                     (push item res)
                     (let ((label (second (assoc :label (second item)))))
                       (when label 
                         (push label sections))))

                   (do-it (set-difference entries next-level))))))

      (do-it entries)

      (reverse res))))

