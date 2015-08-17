;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: RACER; Base: 10, Readtable: racer -*-

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

#+:sbcl
(defparameter *file-external-format* :latin-1)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (racer:enable-boolean-readers)
  (wilbur:enable-node-shorthand))

(defvar *current-ontology*)
(defvar *current-reasoner*)

(defvar *triple-position* nil)
(defvar *using-triple-store* nil)
(defvar *swrl-counter* 0)

(defvar *proxy* nil)

(defparameter *role-range* nil)  ; Used for parsing facets in the context of onProperty.

(defun register-namespace (namespace url)
  (wilbur:add-namespace namespace url))

(register-namespace "rdf" nox:-rdf-uri-)
(register-namespace "rdfs" nox:-rdfs-uri-)
(register-namespace "xsd" nox:-xsd-uri-)
(register-namespace "owl2" +owl-version+)
(register-namespace "owl" +owl-version+)   ; Eval this after owl2 s.t. owl is used for printing.
(register-namespace "swrl" +swrl-version+)

(defparameter *implicit-annotation-properties*
  (list !rdfs:comment !rdfs:label !rdfs:seeAlso !owl:isDefinedBy !owl:versionInfo !owl:priorVersion
        !rdfs:isDefinedBy))

(defvar *annotation-properties* nil)
(defvar *datatype-properties* nil)
(defvar *object-properties* nil)
(defvar *user-defined-datatypes* nil)

;;; ======================================================================

(defun resource-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((resource 
         (let ((old-case 
                (readtable-case *readtable*)))
           (unwind-protect
               (progn (setf (readtable-case *readtable*) :preserve)
                 (read stream))
             (setf (readtable-case *readtable*) old-case))))
        
        (namespace (get-namespace-prefix (current-tbox))))
    (if namespace
        (intern (format nil "~A~A" namespace resource))
      (intern (format nil "~A" resource)))))


;;; The following named readtable is useful for incremental compilation in Allegro.

;; (set-dispatch-macro-character #\# #\! 'resource-reader)
#+:allegro
(progn
  (setf (named-readtable :racer) *readtable*)
  ;; (set-dispatch-macro-character #\# #\! 'resource-reader (named-readtable :racer))
  ;; (set-macro-character #\! (get-macro-character #\!) (named-readtable :racer))
  )

;;; ======================================================================

(ts:nrql-defun (delete-prefix-mappings) ()

  (dolist (tbox (all-tboxes))
    (clear-tbox-namespaces tbox))
  
  (mapcar #'wilbur:del-namespace   
          (mapcar #'car (wilbur:dictionary-namespaces wilbur:*nodes*)))

  (register-namespace "rdf" nox:-rdf-uri-)
  (register-namespace "rdfs" nox:-rdfs-uri-)
  (register-namespace "xsd" nox:-xsd-uri-)
  (register-namespace "owl2" +owl-version+)
  (register-namespace "owl" +owl-version+)
  (register-namespace "swrl" +swrl-version+)

  (setf *prefix-mappings* nil
	*cached-prefixes* nil))

(defun reset-prefix-cache ()
  (setf *cached-prefixes* nil))

(defun add-prefix (prefix mapping)
  (reset-prefix-cache)
  (pushnew (cons prefix mapping) *prefix-mappings* :test #'equal))

(defmacro define-prefix (prefix mapping)
  `(add-prefix ',prefix ',mapping))

(ts:nrql-defun (get-prefixes) (&optional (tbox (tbox-name *current-tbox*)) (ask-owlapi-p t))
  (if (and (eq (first *cached-prefixes*) tbox)
           (eq (second *cached-prefixes*) ask-owlapi-p))
      (third *cached-prefixes*)
    (progn 
      (setf *cached-prefixes*
            (list tbox 
                  ask-owlapi-p

                  (let ((res nil))
	  
                    (mapcar #'(lambda (x)
                                (list (car x) (cdr x)))
		      
                            (remove-duplicates
		       
                             (append res
                                     (when ask-owlapi-p
                                       (|OWLAPI-getPrefixes| (|OWLAPI-getCurrentReasoner|)))
                                     (mapcar #'(lambda (x)
                                                 (cons (first x)
                                                       (cdr x)))
                                             *prefix-mappings*)
                                     (mapcar #'(lambda (x) 
                                                 (cons (first x)
                                                       (cdr x)))
                                             (tbox-namespaces (find-tbox tbox)))
                                     (wilbur:dictionary-namespaces wilbur:*nodes*))
                             :test #'(lambda (x y)
                                       (and (string-equal (car x) (car y))
                                            (string-equal (cdr x) (cdr y)))))))))
      (third *cached-prefixes*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun resource-reader1 (stream subchar arg)
    (declare (ignore subchar arg))
  
    (let* ((prefix 
	    (coerce 
	     (loop as 
		x = (read-char stream nil)
		while (and x (not (char-equal x #\:)))
		collect x)
	     'string))
	   (resource 
	    (let ((old-case 
		   (readtable-case *readtable*)))
	      (unwind-protect
		   (progn (setf (readtable-case *readtable*) :preserve)
			  (let ((char (peek-char nil stream nil)))
			    (when (and char 
				       (not (owlapi:whitespace-char-p char))
				       (not (char= char #\))))
			      (read stream))))
		(setf (readtable-case *readtable*) old-case))))
	 
	   (namespace (if (string= prefix "")
			  (get-namespace-prefix (current-tbox))
			  (second (assoc prefix 
					 (get-prefixes) 
					 :test #'string-equal))))
	   (namespace-sep  
	    (when namespace
	      (let ((n (1- (length  namespace))))
		(elt namespace n))))

	   (namespace 
	    (when namespace
	      (case namespace-sep
		((#\# #\/) namespace)
		(otherwise
		 (format nil "~A#" namespace))))))

      (if resource
	  (if namespace
	      (intern (format nil "~A~A" namespace resource))
	      (intern (format nil "~A" resource)))
	  (if namespace
	      (intern (format nil "~A" namespace))
	      nil))))


  (defun resource-reader2 (stream subchar arg)
    (declare (ignore subchar arg))
  
    (let* ((prefix 
	    (coerce 
	     (loop as 
		x = (read-char stream nil)
		while (and x (not (char-equal x #\:)))
		collect x)
	     'string))
	   (resource 
	    (let ((old-case 
		   (readtable-case *readtable*)))
	      (unwind-protect
		   (progn (setf (readtable-case *readtable*) :preserve)
			  (let ((char (peek-char nil stream nil)))
			    (when (and char 
				       (not (owlapi:whitespace-char-p char))
				       (not (char= char #\))))
			      (read stream))))
		(setf (readtable-case *readtable*) old-case))))
	 
	   (namespace (if (string= prefix "")
			  (get-namespace-prefix (current-tbox))
			  (second (assoc prefix 
					 (get-prefixes)
					 :test #'string-equal))))
         
	   (namespace-sep  
	    (when namespace
	      (let ((n (1- (length  namespace))))
		(elt namespace n))))

	   (namespace 
	    (when namespace
	      (case namespace-sep
		((#\# #\/) namespace)
		(otherwise
		 (format nil "~A#" namespace))))))
    
      (if resource
	  (if namespace
	      (intern (format nil "*~A~A" namespace resource))
	      (intern (format nil "*~A" resource)))
	  (if namespace
	      (intern (format nil "*~A" namespace))
	      nil)))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun enable-reader-macros ()
    (set-dispatch-macro-character #\# #\T 'true-value-reader)
    (set-dispatch-macro-character #\# #\F 'false-value-reader)
    (set-dispatch-macro-character #\# #\t 'true-value-reader)
    (set-dispatch-macro-character #\# #\f 'false-value-reader)
    
    (set-dispatch-macro-character #\# #\! 'resource-reader1)
    (set-dispatch-macro-character #\# #\& 'resource-reader2)))


;;; ======================================================================


(defun get-role-info (node node-ht)
  (gethash (owl-as-role node node-ht) (tbox-role-axioms-index *current-tbox*)))

(defun role-info-object-property-node-p (node node-ht)
  (let ((role-info (get-role-info node node-ht)))
    (if (null role-info)
        nil
      (not (role-info-datatype role-info)))))

(defun role-info-datatype-property-node-p (node node-ht)
  (let ((role-info (get-role-info node node-ht)))
    (if (null role-info)
        nil
      (role-info-datatype role-info))))

(defun node-eq (node-or-literal wilbur-node)
  (eq node-or-literal wilbur-node))

(defun ontology-node-p (node node-ht)
  (loop for (predicate object) in (get-attributes node node-ht)
        thereis (and (node-eq predicate !rdf:type)
                     (node-eq object !owl:Ontology))))

(defun datarange-node-p (node node-ht)
  (loop for (predicate object) in (get-attributes node node-ht)
        thereis (and (node-eq predicate !rdf:type)
                     (or (node-eq object !owl:DataRange)
			 (node-eq object !rdfs:Datatype)))))

(defun class-node-p (node node-ht)
  (or (node-eq node !owl:Thing)
      (node-eq node !owl:Nothing)
      (loop for (predicate object) in (get-attributes node node-ht)
            thereis (and (node-eq predicate !rdf:type)
                         (or (node-eq object !rdfs:Class)
                             (node-eq object !owl:Class)
                             (node-eq object !owl:Restriction))))))

(defun object-property-node-p (node node-ht)
  (or (role-info-object-property-node-p node node-ht)
      (let ((role (owl-as-role node node-ht)))
        (or (and (consp role) (eq (first role) 'inv))
            (gethash (owl-as-role node node-ht) *object-properties*)))))

(defun datatype-property-node-p (node node-ht)
  (or (role-info-datatype-property-node-p node node-ht)
      (gethash (owl-as-role node node-ht) *datatype-properties*)))

(defun annotation-property-node-p (node node-ht)
  (gethash (owl-as-role node node-ht) *annotation-properties*))

(defun only-object-property-p (node node-ht)
  (or 
   (and (object-property-node-p node node-ht)
        (not (datatype-property-node-p node node-ht)))
   (node-eq (get-attribute-value !rdf:type (get-attributes node node-ht))
            !owl:ObjectProperty)))

(defun only-datatype-property-p (node node-ht)
  (or 
   (and (datatype-property-node-p node node-ht)
        (not (object-property-node-p node node-ht)))
   (node-eq (get-attribute-value !rdf:type (get-attributes node node-ht))
            !owl:DatatypeProperty)))

(defun only-annotation-property-p (node node-ht)
  (or (and (annotation-property-node-p node node-ht)
       (not (object-property-node-p node node-ht))
       (not (datatype-property-node-p node node-ht)))
      (node-eq (get-attribute-value !rdf:type (get-attributes node node-ht))
               !owl:AnnotationProperty)))

(defun role-range-spec (node node-ht)
  (loop for (predicate object) in (get-attributes node node-ht)
        thereis (and (node-eq predicate !rdfs:range)
                     object)))

(defun list-item-p (node)
  (let ((pos (search "http://www.w3.org/1999/02/22-rdf-syntax-ns#_" (node-uri node))))
    (and pos 
         (zerop pos))))


;;; ======================================================================

(defun ensure-axiom (axiom-ht subject predicate object axiom-id)

  (declare (ignorable axiom-ht subject predicate object))

  #| 
  (unless (eq axiom-id :void) ; memory saving mode? no axiom annotation possible, dont keep them!
    (let* ((key (list subject predicate object))
           (found (gethash key axiom-ht)))
      (when (and found (not (eql axiom-id found)))
        (unless (node-eq predicate !rdf:type)
          (racer-warn "Overwriting axiom id for axiom ~A ~A ~A"
                      subject predicate object)))
      (setf (gethash key axiom-ht) axiom-id))) |# 

  axiom-id)

(defun find-axiom (key axiom-ht)
  (gethash key axiom-ht))


;;; ======================================================================

;;; An ontology represents a resource that is loaded into a Racer tbox.
;;; Ontology objects are associated with the tbox into which the corresponding
;;; resources (e.g., files) are loaded. This kind of ontology is not an OWL ontology 
;;; as provided by the OWLAPI.

(defvar *ontology*)

(defstruct ontology
  (name nil)
  (plist nil)
  )

;;; ======================================================================

;;; Retained for historical reasons

(defun enable-optimized-query-processing (&optional (rewrite-concept-definitions t))
  (set-rewrite-defined-concepts rewrite-concept-definitions)
  ;;(setf *encode-datatype-role-as-annotations* t) We do not need this any longer
  )


;;; ======================================================================

;;; This class is used by Wilbur.

(defclass racer-owl-parser
          (wilbur::owl-parser)
  ())


;;; ======================================================================

;;; Namespace prefixes declared in the OWL resource are explicitly represented
;;; This allow for a convenient way to denote resource in a REPL.

(defparameter *tbox-namespaces* (make-hash-table :test #'equal))

(defun clear-tbox-namespaces (tbox)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (setf (tbox-namespaces tbox) nil))

(defun (setf tbox-namespaces) (new-value tbox)
  (if (null new-value)
      (remhash (tbox-name tbox) *tbox-namespaces*)
    (setf (gethash (tbox-name tbox) *tbox-namespaces*) new-value)))
  
(defun tbox-namespaces (tbox)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (gethash (tbox-name tbox) *tbox-namespaces*))

#|

(let (#+:allegro (excl::*redefinition-warnings* nil)
                 #+:ccl (ccl:*warn-if-redefine* nil)
                 #+:lispworks (lispworks:*handle-warn-on-redefinition* :quit))

  (defun prepend-prefix (name tbox)
    (if (null tbox)
	name
      (let ((name-string (symbol-name name)))
	(if (char= (char name-string 0) #\#)
	    (let ((prefix (get-namespace-prefix tbox)))
	      (if prefix 
		  (let ((prefix-length (length prefix)))
		    (if (and (> prefix-length 0)
                             (char= (char prefix (- prefix-length 1)) #\#))
			(intern (concatenate 'string prefix (subseq name-string 1)))
		      (intern (concatenate 'string prefix name-string))))
		name))
	  name)))))

|#

(declaim (special *prefix-mappings*))

(defun get-namespace-prefix (tbox)
  (check-type tbox (or tbox symbol))
  (setf tbox (find-tbox tbox))
  (cdr (assoc 'nil (tbox-namespaces tbox))))

(defun remove-prefix (name tbox)
  (check-type name symbol)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (let* ((prefix (get-namespace-prefix tbox))
         (name-string (symbol-name name))
         (sharp-position (position #\# name-string)))
    (if sharp-position
        (if prefix
            (let ((prefix-length (length prefix)))
              (if (char= (char prefix (- prefix-length 1)) #\#)
                  (if (string= prefix (subseq name-string 0 (min prefix-length (length name-string))))
                      (intern (subseq name-string sharp-position))
                    name)
                name))
          name)
      name)))

(defconstant +rdf-tag+ (concatenate 'string nox:-rdf-uri- "RDF"))

(defmethod nox:start-element :before ((self wilbur:rdf-syntax-normalizer) (tag nox:open-tag) mode)
  (declare (ignore mode))

  (when (string= +rdf-tag+ (nox:token-string tag))

    (let* ((parser (nox:sax-producer-consumer self))
           (namespaces (nox:tag-namespaces tag))
           (base (nox:tag-base tag))
           (default-namespace (cdr (assoc 'nil namespaces)))
           (default-uri-prefix (if (string= base (first (wilbur:parser-base parser)))
                                   default-namespace
                                 (or base default-namespace)))
           (length-prefix (and default-uri-prefix
                               (length default-uri-prefix))))
      
      (when (and default-uri-prefix
                 (> length-prefix 0)
                 (char= (char default-uri-prefix (- length-prefix 1)) #\#))
        (setf default-uri-prefix (subseq default-uri-prefix 0 (- length-prefix 1))))

      (unless (or (first (wilbur:parser-base parser))
                  default-uri-prefix)
        (error "Cannot determine default namespace. Use xmlns=\"....\" as an attribute to the RDF tag."))

      (when default-uri-prefix
        (setf (first (wilbur:parser-base parser)) default-uri-prefix))

      (setf (nox:tag-base tag) default-uri-prefix)
      
      ;; (print (wilbur:parser-base parser))
      ;; (print default-uri-prefix)
      ;; (break)

      (let ((namespace (or default-uri-prefix 
                           (first (wilbur:parser-base parser)))))
        (add-to-tbox-namespaces *current-tbox*
                                (acons nil 
                                       (ensure-namespace-ends-with-sharp namespace)
                                       namespaces))))))

(defun ensure-namespace-ends-with-sharp (namespace)
  (let ((len (length namespace)))
    (if (zerop len)
        "#"
      (if (or (char= (aref namespace (1- len)) #\#)
              (char= (aref namespace (1- len)) #\/)
              (char= (aref namespace (1- len)) #\\)
              (char= (aref namespace (1- len)) #\.))
          namespace
        (concatenate 'string namespace "#")))))

(defun add-to-tbox-namespaces (tbox namespaces)
  (unless (null namespaces)
    (let ((spec (first namespaces)))
      (unless (assoc (first spec) (tbox-namespaces tbox) :test #'string=) 
        ;;(break)
        (setf (tbox-namespaces tbox)
              (nconc (tbox-namespaces tbox)
                     (list spec))))
      (add-to-tbox-namespaces tbox (rest namespaces)))))


;;; ======================================================================

#+:allegro 
(defun parse-db-from-file (filename locator &rest options)
  (declare (dynamic-extent options))
  (handler-case 
      (with-open-file (stream filename :element-type 'character :external-format *file-external-format*)
        (let ((stream2))
          ;;; gzipped?
          (util.zip::skip-gzip-header stream)
          (setq stream2 (make-instance 'util.zip:inflate-stream :input-handle stream))
          (apply #'wilbur:parse-db-from-stream stream2 locator options)))
    (error ()
      (with-open-file (stream filename :element-type 'character :external-format *file-external-format*)	    
        ;;; nicht gezipped
        (apply #'wilbur:parse-db-from-stream stream locator options)))))


#-:allegro 
(defun parse-db-from-file (filename locator &rest options)
  (declare (dynamic-extent options))
  (with-open-file (stream filename :element-type 'character :external-format *file-external-format*)
    (apply #'wilbur:parse-db-from-stream stream locator options)))


(defun parse-db-from-stream (stream locator &rest options)
  (declare (dynamic-extent options))
  (apply #'wilbur:parse-db-from-stream stream locator options))


;;; ======================================================================

;;; Mirroring URIs is supported for single URIs as well as for prefixes.
;;; For prefixes, a mapping to a directory must be provided 
;;; (e.g.: (mirror "http://www.foo.de/blah/" "file:///Users/rm/subdir/"))
;;; The directory structure at the destination must match with the source 
;;; from this point on.

(defun clear-mirror-table ()
  (clrhash *uri-mirror-table*))

#+:cl-http
(defun check-for-url-mirror (url-spec)
  (or (gethash url-spec *uri-mirror-table*)
      url-spec))

#+:aserve
(defun check-for-url-mirror (url-spec)
  (let* ((parsed-uri (net.uri:parse-uri (clean-url url-spec)))
	 (scheme (net.uri:uri-scheme parsed-uri))
	 (host (and (eq scheme :http) (net.uri:uri-host parsed-uri)))
	 (mapped-host (and host
			   (gethash (concatenate 'string
                                                 "http://"
                                                 host)
				    *uri-mirror-table*))))
    (if mapped-host
	(concatenate 'string 
                     "file://"
                     (namestring (make-pathname
                                  :device (pathname-device 
                                           (pathname mapped-host))
                                  :directory (append 
                                              (let ((dir (pathname-directory
                                                          (pathname mapped-host))))
                                                (if (string= (second dir) "file:")
                                                    `(:absolute .,(rest (rest dir)))
                                                  dir))
                                              (butlast 
                                               (rest (net.uri:uri-parsed-path parsed-uri))))
                                  :name (first (last (net.uri:uri-parsed-path parsed-uri))))))
      (or (gethash url-spec *uri-mirror-table*)
	  url-spec))))

(defun mirror (url-spec1 url-or-filename)
  (setf (gethash url-spec1 *uri-mirror-table*) url-or-filename))


;;; ======================================================================

(defun owl-read-file (filename &key (verbose *tbox-verbose*) (init t) (kb-name) (locator) 
                               (recursive nil)
                               (ignore-import nil) (import-meta-ontologies nil) (excluded-meta-ontologies nil)
                               (fire-rules nil) (maintain-owlapi-axioms nil)
                               (ignore-annotations nil) 
                               ontology-name
                               merge-imported-ontologies-p &allow-other-keys)
  (flet ((parse ()
           (let ((*indent* (+ *indent* 3))
                 (pathname (pathname (translate-logical-pathname filename))))
             (push (format nil "file://~{/~A~}/~A~A~A"
                           (rest (pathname-directory pathname))
                           (pathname-name pathname)
                           (if (pathname-type pathname)
                               "."
                             "")
                           (or (pathname-type pathname) ""))
                   *visited-urls*)
             (owl-read-document-1 filename ':file (or kb-name 
                                                      (intern filename))
                                  :verbose verbose :init init :locator (or locator filename) 
                                  :kb-name kb-name
                                  :ontology-name ontology-name
                                  :merge-imported-ontologies-p merge-imported-ontologies-p 
                                  :recursive recursive
                                  :ignore-import ignore-import
                                  :import-meta-ontologies import-meta-ontologies
                                  :excluded-meta-ontologies excluded-meta-ontologies
                                  :fire-rules fire-rules
                                  :maintain-owlapi-axioms maintain-owlapi-axioms
                                  :ignore-annotations ignore-annotations))))
    (declare (dynamic-extent #'parse))
    (if recursive
        (if (find filename *visited-urls*
                  :test #'(lambda (s1 s2) 
                            (string= s1 s2)))
            (parse)
          'already-open)
      (let ((*visited-urls* nil)
            (*indent* -3))
        (parse)
        ))))
  

#+:cl-http
(defun pathname-from-url (url)
  (let* ((host (url::host-for-pathname url))
         (host (if (and (stringp host)
                        (not (string-equal host ""))
                        (char= #\: (elt host (1- (length host)))))
                   (subseq host 0 (1- (length host)))
                 host)))

    (make-pathname :directory
                   `(:absolute 
                     .,(mapcar #'(lambda (x) 
                                   (ts:string-substitute 
                                    x
                                    '(("%20" " " nil))))
                               (url:path url)))
                   :host host 
                   :name 
                   (ts:string-substitute
                    (url:object url)
                    '(("%20" " " nil)))
                   :type (url:extension url))))

#+:cl-http
(defmethod nox::read-external-dtd :around ((consumer racer-owl-parser) stream dtd-stuff namespaces)
  (declare (ignore stream))
  (let* ((url-spec (second dtd-stuff))
         (base (first *visited-urls*)))
    (if url-spec
	(let* ((merged-url (merge-urls url-spec base))
	       (merged-url (clean-url merged-url))
	     (real-url-spec (check-for-url-mirror merged-url))
	     (real-url-spec (clean-url real-url-spec))
	     (url (url:intern-url real-url-spec)))
        (etypecase url 
          (url::file-url 
           (with-open-file (dtd-stream (pathname-from-url url)
                                       :direction :input)
             (call-next-method consumer dtd-stream dtd-stuff namespaces)))
          ((or url:http-object url:http-path)
           (let ((found nil))
             (with-temp-file ((dtd-stream)
                              (setf found (http:show-url url :stream dtd-stream)))
               (if (eq found :not-found)
                   (error "External DTD not found.")
                 (call-next-method consumer dtd-stream dtd-stuff namespaces)))))))
      (error "External DTD not found."))))

#+:aserve
(defun uri-to-pathname (uri)
  (parse-namestring 
   #+:allegro (net.uri:uri-to-pathname uri)
   #-:allegro (net.uri:uri-path uri)))

#+:aserve
(defmethod nox::read-external-dtd :around ((consumer racer-owl-parser) stream dtd-stuff namespaces)
  (declare (ignore stream))
  (let* ((url-spec (second dtd-stuff))
         (base (first *visited-urls*)))
    (if url-spec
        (let* ((merged-url (merge-urls url-spec base))
               (real-url-spec (check-for-url-mirror merged-url))
	       (real-url-spec (clean-url real-url-spec))
               (url (net.uri:parse-uri real-url-spec)))
          (ecase (net.uri:uri-scheme url)
            (:file
             (with-open-file (dtd-stream (uri-to-pathname url)
                                         :direction :input)
               (call-next-method consumer dtd-stream dtd-stuff namespaces)))
            (:http
             (with-input-from-string (dtd-stream
                                      (net.aserve.client:do-http-request url
					:proxy *proxy*))
               (call-next-method consumer dtd-stream dtd-stuff namespaces)))))
      (error "External DTD not found."))))

(defun merge-urls (url base)
  (when (null base) 
    (setf base (format nil "http://~A/"
                       #+:cl-http (www-utils:local-host-domain-name) 
                       #+:aserve "localhost"
                       #-(or :cl-http :aserve) "localhost")))
  (merge-urls-1 url base 0 (position #\/ base :from-end t)))

(defun merge-urls-1 (url base start-url end-base)
  (let ((pos (search "../" url :start2 start-url)))
    (if (and pos (zerop pos))
        (merge-urls-1 url base (+ 3 start-url) (position #\/ base :end end-base :from-end t))
      (concatenate 'string 
                   (subseq base 0 end-base)
                   "/"
                   (subseq url start-url)))))

(defun url-convenience-transformation (url)
  (let ((pos1 (search "file:" url))
        (pos2 (position #\/ url)))
    (if (and pos1 (zerop pos1) *visited-urls*)
        (if (null pos2)
            (merge-urls (subseq url 5) (first *visited-urls*))
          url)
      url)))

#-(:or :cl-http :aserve)
(defun owl-read-document (url-spec &rest args 
                                   &key (verbose *tbox-verbose*) 
                                   (init t) (kb-name) (locator)
                                   (recursive nil) 
                                   (ignore-import nil) 
                                   (import-meta-ontologies nil)
                                   (fire-rules nil) 
                                   (maintain-owlapi-axioms nil)
                                   (ignore-annotations nil))
  (declare (ignore url-spec args verbose init 
		   kb-name locator recursive ignore-import
                   import-meta-ontologies fire-rules 
                   maintain-owlapi-axioms ignore-annotations))
  (error "Not implemented in this version of Racer."))

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


#+:cl-http
(defun url-name-string (url)
  (url:name-string url))

#+:cl-http
(defun owl-read-document (url-spec &rest args 
                                   &key (verbose *tbox-verbose*) 
                                   (init t) (kb-name) (locator)
                                   (recursive nil) 
                                   (ignore-import nil) 
                                   (ontology-name nil)
                                   (merge-imported-ontologies-p nil)
                                   (import-meta-ontologies nil)
                                   (excluded-meta-ontologies nil)
                                   (fire-rules nil)
                                   (maintain-owlapi-axioms nil)
                                   (ignore-annotations nil))

  (let* ((url-spec (clean-url url-spec))
	 (url-spec (url-convenience-transformation url-spec))
         (real-url-spec (check-for-url-mirror url-spec))
	 (real-url-spec (clean-url real-url-spec))
         (url (url:intern-url real-url-spec)))

    (flet ((parse ()
             (let ((*indent* (+ *indent* 3)))
               (push real-url-spec *visited-urls*)
               (etypecase url 
                 (url::file-url 
                  (let ((pathname (pathname-from-url url)))
                    (when verbose
                      (unless (string= url-spec real-url-spec)
                        (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
                    (let ((result
                           (apply #'owl-read-document-1 
                                  pathname
                                  ':file 
                                  (or kb-name 
                                      (intern (url-name-string url)))
                                  :kb-name kb-name
                                  :verbose verbose
                                  :init init
                                  :locator (or locator (url-name-string url))
                                  :ignore-import ignore-import
                                  :import-meta-ontologies import-meta-ontologies
                                  :excluded-meta-ontologies excluded-meta-ontologies
                                  :fire-rules fire-rules
				  :recursive recursive
                                  :merge-imported-ontologies-p merge-imported-ontologies-p 
                                  :ontology-name ontology-name
                                  args)))
                      result)))
                 ((or url:http-object url:http-path)
                  (when verbose
                    (unless (string= url-spec real-url-spec)
                      (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
                  (let* ((found nil)
                         (result
                          (with-temp-file ((stream)
                                           (setf found (http:show-url url :stream stream)))
                            (if (eq found :not-found)
                                (error "HTTP request for ~A returned code 404" (url-name-string url))
                              (apply #'owl-read-document-1 stream ':stream 
                                     (or kb-name 
                                         (intern (url-name-string url)))
                                     :kb-name kb-name
                                     :verbose verbose 
                                     :init init
                                     :locator (or locator (url-name-string url))
                                     :ignore-import ignore-import
                                     :import-meta-ontologies import-meta-ontologies
                                     :excluded-meta-ontologies excluded-meta-ontologies
                                     :fire-rules fire-rules
                                     :maintain-owlapi-axioms maintain-owlapi-axioms
                                     :ignore-annotations ignore-annotations
                                     :recursive recursive
                                     :merge-imported-ontologies-p merge-imported-ontologies-p 
                                     :ontology-name ontology-name
                                     args)))))
                    result))))))

      (declare (dynamic-extent #'parse))
      
      (if recursive
          (if (find real-url-spec *visited-urls*
                    :test #'(lambda (s1 s2) 
                              (string= s1 s2)))
              'already-open
            (parse))
        (let ((*visited-urls* nil)
              (*indent* -3))
          (parse)
          ))))
  #-(and :racer-server :cl-http)
  (error "Reading documents from web servers is not supported in this Racer version."))

#+:aserve
(defun owl-read-document (url-spec &rest args 
                                   &key (verbose *tbox-verbose*) 
                                   (init t) (kb-name) (locator)
                                   (recursive nil) (ignore-import nil)
                                   (import-meta-ontologies nil)
                                   (excluded-meta-ontologies nil)
                                   (fire-rules nil)
                                   (maintain-owlapi-axioms nil)
                                   (ignore-annotations nil))
  (let* ((url-spec (clean-url url-spec))
	 (url-spec (url-convenience-transformation url-spec))
         (real-url-spec (check-for-url-mirror url-spec))
	 (real-url-spec (clean-url real-url-spec))
         (url (net.uri:parse-uri real-url-spec)))
    (flet ((parse ()
             (let ((*indent* (+ *indent* 3)))
               (push real-url-spec *visited-urls*)
               (ecase (net.uri:uri-scheme url)
                 (:file
                  (let ((pathname (uri-to-pathname url)))
                    (when verbose
                      (unless (string= url-spec real-url-spec)
                        (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
                    (let ((result
                           (apply #'owl-read-document-1 
                                  pathname
                                  ':file 
                                  (or kb-name 
                                      (intern (concatenate 'string
                                                           "file://"
                                                           (if (net.uri:uri-host url)
                                                               (concatenate 'string
                                                                            (net.uri:uri-host url)
                                                                            ":"
                                                                            (net.uri:uri-path url))
                                                             (net.uri:uri-path url)))))
                                  :kb-name kb-name
                                  :verbose verbose
                                  :init init
                                  :locator (or locator (net.uri:uri-path url))
                                  :ignore-import ignore-import
                                  :import-meta-ontologies import-meta-ontologies
                                  :excluded-meta-ontologies excluded-meta-ontologies
                                  :fire-rules fire-rules
				  :recursive recursive
                                  args)))
                      result)))
                 (:http
                  (when verbose
                    (unless (string= url-spec real-url-spec)
                      (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
		  
		  (multiple-value-bind (body res headers)
		      (net.aserve.client:do-http-request url
                                                         :headers '(("Accept-Encoding" . "gzip"))
                                                         :format :binary
							 :proxy *proxy*
							 ;; :external-format :latin1-base
                                                         )
		    
		    #+:allegro 
                    (cond ((equal (cdr (assoc :content-encoding headers)) "gzip")
			   
			   (when verbose 
			     (format t "HTTP request returned ~s gzip'ed bytes, " (length body)))
			   
			   (let ((so (make-string-output-stream))
				 (bis (make-buffer-input-stream body))
				 (bi))
			      
			     (util.zip::skip-gzip-header bis)

			     (setq bi (make-instance 'util.zip:inflate-stream :input-handle bis))
			     
			     (do ((byte (read-byte bi nil nil) (read-byte bi nil nil)))
				 ((null byte)
				  (setq body (get-output-stream-string so))
				  
				  (when verbose
				    (format t "~s characters after inflation~%" (length body))))
			       
			       (write-char (code-char byte) so))))
			   
			  (t (setq body (octets-to-string body))))
		    
                    #+(or :ccl :lispworks)
                    (setq body (deflate body headers verbose))
		    
		    (cond ((not (eql 200 res))
			   (error "HTTP request returned code ~s" res))
			  
			  (t 
			   
			   (with-input-from-string (stream body)
		  
			     (let ((result
				    (owl-read-document-1 stream ':stream 
                                                         (or kb-name 
                                                             (intern (net.uri:uri-path url)))
                                                         :kb-name kb-name
                                                         :verbose verbose 
                                                         :init init
                                                         :locator (or locator 
								      (net.uri:uri-path url))
                                                         :ignore-import ignore-import
                                                         :import-meta-ontologies import-meta-ontologies
                                                         :excluded-meta-ontologies excluded-meta-ontologies
                                                         :fire-rules fire-rules
                                                         :maintain-owlapi-axioms maintain-owlapi-axioms
                                                         :ignore-annotations ignore-annotations
							 :recursive recursive)))
			       result))))))))))
      (declare (dynamic-extent #'parse))
      
      (if recursive
          (if (find real-url-spec *visited-urls*
                    :test #'(lambda (s1 s2) 
                              (string= s1 s2)))
              'already-open
            (parse))
        (let ((*visited-urls* nil)
              (*indent* -3))
          (parse)
          )))))



(defun retrieve-triples (spec type locator)
  (let ((current-parser nil))
    (sort-triples
     (wilbur:db-triples 
      (ecase type
        (:file 
         (handler-case
             (handler-bind ((nox:wilbur-error #'(lambda (c)
                                                  (declare (ignore c))
                                                  (setf current-parser
                                                        nox:*current-parser*))))
               
               (parse-db-from-file 
                spec 
                (or locator (nox:make-file-url 
                             (translate-logical-pathname spec)))
                :parser-class 'racer-owl-parser))
           (error (error)
                  (if current-parser
                    (error "~A @ stream position ~A" 
                           error
                           (nox:parser-filepos current-parser))
                    (error "~A" error)))))
        (:stream
         (parse-db-from-stream 
          spec 
          locator
          :parser-class 'racer-owl-parser)))))))


(defun owl-read-document-1 (spec type kb-name 
			    &key 
			    (verbose *tbox-verbose*) 
			    (init t) (locator) (ignore-import nil) 
			    (import-meta-ontologies nil)
			    (excluded-meta-ontologies nil)
			    (fire-rules nil) 
			    (maintain-owlapi-axioms nil)
			    (ignore-annotations nil)
			    (recursive nil)
			    ontology-name 
			    merge-imported-ontologies-p 
			    &allow-other-keys)

  (without-duplicate-warnings

   (let ((ontology-locator (or locator spec)))
     (when (and verbose
		ontology-locator)
       (format t "~%~V@TReading ontology ~A into" *indent* 
	       ontology-locator)
       ;;(force-output t)
       )
  
     (let* (

	    ;; (*current-tbox* *current-tbox*)
	    ;; (*current-abox* *current-abox*)
	    (*current-reasoner* (|OWLAPI-getCurrentReasoner|))
	    (*current-ontology* (if recursive
				    (|OWLAPI-getAutoOntology| *current-reasoner*)
				  :void)))

       (|OWLAPI-newReasoner| kb-name t init)
	
       (owlapi::with-progress-state-of-reasoner (kb-name)
	 (owlapi::with-non-toplevel-owlapi-request

	     (let* ((tbox *current-tbox*)
		    (abox *current-abox*) 
		    (*swrl-counter* (if recursive *swrl-counter* 0))
		    (node-ht (make-hash-table :test #'eq)) 
		    ;; node-ht is a table which associates unique names with anonymous resources 
		    ;; which are mentioned in an OWL resource.
		    (axiom-ht
		     ;; (make-hash-table :test #'equal)
		     nil)
		    (rule-ht (make-hash-table :test #'equal))
		    (var-ht (make-hash-table :test #'eq))
		    (*tbox-verbose* verbose)
		    (*abox-verbose* verbose)
		    (*ontology* (make-ontology))
		    (*user-defined-datatypes* nil)
		    (*annotation-properties* (or *annotation-properties* (make-hash-table)))
		    (*datatype-properties* (or *datatype-properties* (make-hash-table)))
		    (*object-properties* (or *object-properties* (make-hash-table)))
		    (wilbur:*user-defined-nodes* (or wilbur:*user-defined-nodes*
						     (make-instance 'wilbur::dictionary))))

	       (setf (tbox-ontologies tbox) (append (tbox-ontologies tbox) (list *ontology*)))
	       (setf (abox-ontologies abox) (append (abox-ontologies abox) (list *ontology*)))

	       (let* ((triples
		       (racer:with-progress-range (nil (0 50))
			 (retrieve-triples spec type locator)))
		      (ontologies nil)
		      (second-level-ontologies nil)
		      (n-triples (* 
				  (if maintain-owlapi-axioms 5 4)
				  (length triples)))
		      (triple-counter 0))

		 (unless triples
		   (racer-warn "There are no RDF triples in this ontology! Probably not an OWL RDF ontology? Try owlapi-read-ontology instead which automatically guesses the syntax from the file content."))

		 ;; (setf *t* triples)

		 ;; First phase: Set up index structures.
		  
		 (racer:with-progress-range (n-triples (50 100))

		   (loop for triple in triples do
		      
			 (let ((subject (wilbur:triple-subject triple))
			       (predicate (wilbur:triple-predicate triple))
			       (object (wilbur:triple-object triple)))
                    
			   (racer:set-progress-value (incf triple-counter))

			   (cond ((or (null (node-uri subject))
				      (eq (wilbur:node-name-resolved-p subject) :node-id)
				      (node-eq predicate !rdfs:range)
                             
				      (and (node-eq predicate !rdf:type)
					   (node-eq object !rdf:List))
                             
				      (node-eq predicate !rdf:first)
				      (node-eq predicate !rdf:rest))
                        
				  (push (list predicate object) (wilbur:triple-attributes subject)))
                        
				 ((and (node-uri subject)
				       (node-eq predicate !rdf:type))

				  (cond ((node-eq object !owl:Ontology)
					 (push (owl-as-ontology subject node-ht) ontologies))
					((or (node-eq object !rdfs:Property)
					     (node-eq object !owl:ObjectProperty)
					     (node-eq object !owl:TransitiveProperty)
					     (node-eq object !owl2:ReflexiveProperty)
					     (node-eq object !owl:SymmetricProperty)
					     (node-eq object !owl2:AsymmetricProperty))
					 (setf (gethash (owl-as-role subject node-ht) *object-properties*)
					   t))
					((and (node-eq predicate !rdf:type)
					      (node-eq object !owl:DatatypeProperty))
					 (setf (gethash (owl-as-role subject node-ht) *datatype-properties*)
					   t))
					((and (node-eq predicate !rdf:type)
					      (node-eq object !owl:AnnotationProperty))
					 (setf (gethash (owl-as-role subject node-ht) *annotation-properties*)
					   t)))
				  (push (list predicate object) (wilbur:triple-attributes subject))))))
		
		   ;; (format *trace-output* "~%Phase 2~%")

		   ;; Second pass: for named datarange nodes and ontologies (an explicit pass is used
		   ;;              in order to avoid iteration dependencies

		   (loop for triple in triples do
			 ;;(print triple)

			 (let ((subject (wilbur:triple-subject triple))
			       (predicate (wilbur:triple-predicate triple))
			       (object (wilbur:triple-object triple)))

			   (racer:set-progress-value (incf triple-counter))
                  
			   (cond ((datarange-node-p subject node-ht)
				  (push (list predicate object) (wilbur:triple-attributes subject)))
				 ;;((and (not (literal-p object)) (ontology-node-p object node-ht)
				 ;;      (node-eq predicate !owl:imports))
				 ;; (break))
				 #|((and (not (literal-p object))
				       (ontology-node-p object node-ht)
				       (not (eq subject object)))
				  ;; Ontology appears in object position?
				  ;; Then it is a second-level-ontology
				  (push (owl-as-ontology object node-ht) second-level-ontologies))|#
                                 )))
		
		   ;; (format *trace-output* "~%Phase 2~%")

		   (loop with created-name = nil
		       for ontology in (if ontology-name
					   (list ontology-name) 
					 (or ontologies (list kb-name)))
		       do
			 (cond ((find ontology second-level-ontologies)
				(unless (member ontology (|OWLAPI-getOntologies| kb-name))
				  (|OWLAPI-newOntology| ontology kb-name t)))
			       (t
				(when (and (null created-name)
					   (not (member ontology (|OWLAPI-getOntologies| kb-name))))
				  (|OWLAPI-newOntology| ontology kb-name)
				  (cond (maintain-owlapi-axioms
					 (|OWLAPI-disableMemorySavingMode| kb-name))
					(t 
					 (|OWLAPI-enableMemorySavingMode| ontology kb-name 
									  (cond ((boundp '*use-less-tbox-memory*)
										 *use-less-tbox-memory*)
										((find-tbox tbox nil)
										 (tbox-use-less-memory tbox))
										(t t)))))
				  (cond (ignore-annotations 
					 (|OWLAPI-ignoreAnnotations| kb-name))
					(t
					 (|OWLAPI-keepAnnotations| kb-name)))
				  (|OWLAPI-autoAddAxiomsTo| ontology kb-name)
				  (setf created-name ontology))
				(when (and (not (null created-name))
					   (or (not (eq created-name ontology))
					       ontology-name))
				  (|OWLAPI-SetOntologyURI| created-name 
							   (or ontology-name ontology)
							   kb-name)))))

	      ;;;
	      ;;; Output some helpful information
	      ;;;

		   (when verbose 
		     (format t "~%~V@Tontology container ~A of"
			     (1+ *indent*)
			     (|OWLAPI-getAutoOntology| kb-name))
		     (format t "~%~V@Treasoner (= KB) container ~A"
			     (1+ *indent*)
			     (|OWLAPI-getCurrentReasoner|))
		     (let ((names 
			    (owlapi:all-names 
			     (owlapi:find-owl-ontology
			      (|OWLAPI-getAutoOntology| kb-name)))))
		       (when (cdr names)
			 (format t "~%~V@This container has additional names: ~A..." (1+ *indent*) names)))
		     ;;(force-output t)
		     )
              
		   ;; Must be done after the ontologies are created.
		   (loop for property in *implicit-annotation-properties* 
		       do
			 (setf (gethash (owl-as-role property node-ht) *annotation-properties*) t)
			 (setf (gethash (owl-as-role property node-ht) *datatype-properties*) t)
			 (setf (gethash (owl-as-role property node-ht) *object-properties*) t)
			 (|OWLAPI-getOWLReallyImplicitDeclarationAxiom|
			  `(|AnnotationProperty| ,(owl-as-role property node-ht))
			  kb-name)
			 (|OWLAPI-getOWLReallyImplicitDeclarationAxiom| 
			  `(|ObjectProperty| ,(owl-as-role property node-ht))
			  kb-name)
			 (|OWLAPI-getOWLReallyImplicitDeclarationAxiom| 
			  `(|DataProperty| ,(owl-as-role property node-ht))
			  kb-name))

		   (when maintain-owlapi-axioms
		     (|OWLAPI-applyChanges| kb-name))

		   ;; (format *trace-output* "~%Phase 3 starts~%")
		
		   ;; Third pass: deal with owl:imports
		   (loop for triple in triples do
			 ;;(print triple)
			 (let ((subject (wilbur:triple-subject triple))
			       (predicate (wilbur:triple-predicate triple))
			       (object (wilbur:triple-object triple)))

			   (racer:set-progress-value (incf triple-counter))
			
			   (without-progress

			    (when (node-eq predicate !owl:imports)

			      (let ((uri (node-uri object)))

				(unless (or ignore-import (null uri)
					    (find uri excluded-meta-ontologies :test #'string=))

				  (if (or import-meta-ontologies
					  (not (or (string=1 uri +protege-url+)
						   (string=1 uri +owl-version+)
						   (string=1 uri nox:-rdfs-uri-)
						   (string=1 uri nox:-rdf-uri-)
						   (member uri +ignored-meta-ontologies+
							   :test #'string=1))))

				      (let* ((ont  
					      (|OWLAPI-getAutoOntology|
					       (|OWLAPI-getCurrentReasoner|)))

					     (ontology-1
					      (or (owlapi:find-owl-ontology 
						   (owl-as-ontology subject node-ht)
						   nil)
					    ;;; in case an anonymous ontology imports something... 
					    ;;; see all.rdf OWL2 Test Cases 
					    ;;; <ontology> <imports ....> </ontology> : 
						  (owlapi:find-owl-ontology ont)))
                                      
					     (secondary-p (owlapi:secondary-p ontology-1))
					     (previous-ontologies (|OWLAPI-getOntologies| kb-name)))

					(cond ((not merge-imported-ontologies-p)
                              
					       (|OWLAPI-getOWLImportsDeclarationAxiom|
						(node-uri object))

					       (when (or maintain-owlapi-axioms (not secondary-p))

						 (owl-syntaxes:owlapi-import-ontology (node-uri object)
										      :verbose *tbox-verbose*
										      :init nil
										      :kb-name kb-name
										      :recursive t
										      :fire-rules fire-rules
										      :ignore-import ignore-import
										      :import-meta-ontologies import-meta-ontologies
										      :excluded-meta-ontologies excluded-meta-ontologies
										      :maintain-owlapi-axioms maintain-owlapi-axioms
										      :ignore-annotations ignore-annotations)))

					      (t 

					       (when (or maintain-owlapi-axioms (not secondary-p))

						 (owl-syntaxes:owlapi-import-ontology (node-uri object)
										      :verbose *tbox-verbose*
										      :merge-imported-ontologies-p t
										      :ontology-name ont
										      :init nil
										      :kb-name kb-name
										      :recursive t
										      :fire-rules fire-rules
										      :ignore-import ignore-import
										      :import-meta-ontologies import-meta-ontologies
										      :excluded-meta-ontologies excluded-meta-ontologies
										      :maintain-owlapi-axioms maintain-owlapi-axioms
										      :ignore-annotations ignore-annotations))))

					(push (node-uri object) *visited-urls*)
                                
					(when secondary-p
					  (loop for ont-1 in (|OWLAPI-getOntologies| kb-name)
					      unless (find ont-1 previous-ontologies) do
						(setf (owlapi:secondary-p (owlapi:find-owl-ontology ont-1)) t)))
                                
					(|OWLAPI-autoAddAxiomsTo| ont kb-name))

				    (racer-warn "Ignoring import of meta ontology ~a. ~
                                           ~%A meta ontology is not required for reasoning. ~
                                           ~%Use :import-meta-ontologies t to enforce the import." (node-uri object)))))))))
		
		   ;; (format *trace-output* "~%Phase 4 starts~%")

		   ;; Fourth pass: process OWL triples.

		   (loop for triple in triples
		       do
                  
			 (racer:set-progress-value (incf triple-counter))

			 (without-progress
		      
			  (unless (and (node-eq (wilbur:triple-predicate triple) !rdf:type)
				       (node-eq (wilbur:triple-object triple) !owl2:Axiom))
                  
			    (flet ((doit ()
				     (let ((*triple-position*
					    (wilbur:triple-filepos triple)))

				       (process-owl-triple (wilbur:triple-subject triple)
							   (wilbur:triple-predicate triple)
							   (wilbur:triple-object triple)
							   tbox
							   abox
							   node-ht
							   axiom-ht
							   rule-ht
							   var-ht))))
			      (if *debug*
				  (doit)
				(handler-case
				    (doit)
				  (error (error)
				    (error (format nil "~A @ stream position: ~A" 
						   error
						   (wilbur:triple-filepos triple))))))))))
		
		   ;; (format *trace-output* "~%Phase 5 starts~%")
            
		   ;; Fifth pass: process axiom annotations

		   (when maintain-owlapi-axioms
		     (loop for triple in triples
			 do
			   ;;(print triple)
			   #| (process-axiom (wilbur:triple-subject triple)
                                 (wilbur:triple-predicate triple)
                                 (wilbur:triple-object triple)
                                 node-ht
                                 axiom-ht) |# 

			   (racer:set-progress-value (incf triple-counter))

			   (without-progress
			    (when (and (node-eq (wilbur:triple-predicate triple) !rdf:type)
				       (node-eq (wilbur:triple-object triple) !owl2:Axiom))
                    
			      (let ((*triple-position* (wilbur:triple-filepos triple)))
				(process-axiom1 (wilbur:triple-subject triple)
						(wilbur:triple-predicate triple)
						(wilbur:triple-object triple)
						tbox
						abox
						node-ht
						axiom-ht
						rule-ht
						var-ht))))))
		
		   ;; (format *trace-output* "~%Phase 5 ends~%")
		    
		   )

		 ;; otherwise, load-axiom will increment progress even further, hence
		 ;; messing up the progress bar :-(
	      
	      
		 (without-progress
		  (when (and (not recursive) maintain-owlapi-axioms)
		    (|OWLAPI-loadOntologies| 
		     (remove-if #'owlapi:secondary-p (|OWLAPI-getOntologies| kb-name) 
				:key #'owlapi:find-owl-ontology)
		     kb-name)
		     
		    ;; required for SetOntologyURI (not an axiom, was not loaded)
		    (|OWLAPI-applyChanges| kb-name))
          
		  (when (not recursive)	; Macht das Sinn? Noch mal prüfen! Vormals: init
		    ;; I have used init as a hack here. The function owl-read-document-1 is called recursively
		    ;; if there is an import statement in the ontology.
		    ;; The rules are first collected and processes at the end.
		    ;; For the recursive calls init is definitely false, only for the toplevel call, init is true.
		    ;; The problem is that rule processing does not work with init being nil at toplevel!!!!
		    ;; RM 31.7.08
		    (loop for rule being the hash-values of rule-ht 
			do (push rule (abox-rules abox)))
		    ;; MW 5.10.10 - we do not know until here which resource in a SWRL atom 
		    ;; is a variable or individual!
		    (build-swrl-rules tbox abox var-ht))

		  (when verbose 
		    (format t "~%~V@TReading ontology ~A done.~%" *indent* ontology-locator)
		    ;;(force-output t)
		    )

		  (when (and init fire-rules) ;; ditto, see above!!!
		    (swrl-forward-chaining :abox (abox-name abox)))

		  (unless (eq *current-ontology* :void)
              
		    ;; Prefixe im Ontologie-Objekt speichern
		     
		    (reset-prefix-cache)
		    (setf (owlapi:ontology-prefixes (owlapi:find-owl-ontology (|OWLAPI-getAutoOntology|)))
		      (get-prefixes))
              
		    (|OWLAPI-autoAddAxiomsTo| *current-ontology* *current-reasoner*))

		  ;; wichtig, sonst werden die WILBUR Prefixe 
		  ;; nicht uebernommen! 
		   
		  (reset-prefix-cache)

		  kb-name)))))))))

(defun string=1 (s1 s2)
  (let ((pos-sharp (position #\# s2 :from-end t))) 
    (if pos-sharp
        (string= s1 (subseq s2 0 pos-sharp))
      (string= s1 s2))))

(defun transform-name (symbol &optional var)
  (let* ((symbol-name (symbol-name symbol))
         (pos-sharp (position #\# symbol-name)))
    (if pos-sharp
        (concatenate 'string 
                     (if var "?" "")
                     (subseq symbol-name (1+ pos-sharp)))
      symbol-name)))


(defun sort-triples (triples) (reverse triples))



;;; **********************************************************************

;;; In order to be independent of AGraph, even resources found in AGraph
;;; triples are represented as Wilbur nodes during the parsing process of a triple store.

(defun node-p (object)
  (typep object 'wilbur:node))

(defun without-# (string)
  (if (stringp string)
      (let ((n (length string)))
        (if (char= #\# (elt string (1- n)))
            (subseq string 0 (1- n))
          string))
    string))

(defun node-uri (node)
  (let ((uri 
         (wilbur:node-uri node)))
    (when uri
      (if (char= #\# (elt uri 0))
	  (let ((ns 
		 (without-\# 
		  (get-namespace-prefix *current-tbox*))))
	    (racer-warn "Node ~A not in any namespace, using default namespace: ~A~A.  " uri ns uri) 
	    (concatenate 'string ns uri))
	uri))))

(defun get-attributes (node node-ht) 
  (declare (ignore node-ht))
  (progn 
    (if (typep node 'wilbur::literal)
        nil
      (wilbur:triple-attributes node))))

(defun get-attribute-value (attribute attributes)
  (second (assoc attribute attributes :test #'node-eq)))

(defun literal-p (node)
  (typep node 'wilbur::literal))

(defun literal-datatype (spec)
  (wilbur:literal-datatype spec))

(defun literal-string (spec)
  (wilbur:literal-string spec))


;;; **********************************************************************

(defvar *concept-stack* nil)

(defun owl-as-concept (node node-ht) 
  (cond ((node-eq node !owl:Thing) 'top)
        ((node-eq node !owl:Nothing) 'bottom)
        (t 
	 (let ((node-uri (node-uri node)))

           (if (and (or (eq (wilbur:node-name-resolved-p node) :node-id)
                        (null node-uri))

                    ;;; (MW, 26.2.2010) 
                    ;;; sonst Endlosschleife beim Parsen von: 
                    ;;;
                    ;;; <owl:Class rdf:nodeID="B">
                    ;;;    <owl:intersectionOf rdf:parseType="Collection">
                    ;;;        <owl:Class rdf:ID="B"/>
                    ;;;    </owl:intersectionOf>
                    ;;; </owl:Class>

                    (not (member node *concept-stack*)))

               (let* ((*concept-stack* (cons node *concept-stack*)) 
                      (attributes (get-attributes node node-ht))
                      (type (get-attribute-value !rdf:type attributes)))

                 (cond ((node-eq type !owl2:SelfRestriction)
                        (let* ((on-property (get-attribute-value !owl:onProperty attributes))
                               (role (owl-as-role on-property node-ht)))
                          `(self-reference ,role)))

                       ((or (node-eq type !owl:Restriction)
                            (node-eq type !owl2:DataRestriction)
                            (node-eq type !owl2:ObjectRestriction))

                        (let* ((on-property (get-attribute-value !owl:onProperty attributes))
                               (*role-range* (role-range-spec on-property node-ht))
                               (has-self (get-attribute-value !owl:hasSelf attributes))
                               (has-value (get-attribute-value !owl:hasValue attributes))
                               (some-values-from (get-attribute-value !owl:someValuesFrom attributes))
                               (all-values-from (get-attribute-value !owl:allValuesFrom attributes))
                               (cardinality 
                                (or (get-attribute-value !owl:cardinality attributes)
                                    (get-attribute-value !owl:qualifiedCardinality attributes)))
                               (min-cardinality 
                                (or (get-attribute-value !owl:minCardinality attributes)
                                    (get-attribute-value !owl:minQualifiedCardinality attributes)))
                               (max-cardinality
                                (or (get-attribute-value !owl:maxCardinality attributes)
                                    (get-attribute-value !owl:maxQualifiedCardinality attributes)))
                               (object-values-from (let ((node
                                                          (or (get-attribute-value !owl2:onClass attributes)
                                                              (get-attribute-value !owl:valuesFrom attributes))))
                                                     (if node
                                                         (owl-as-concept node node-ht)
                                                       nil)))
                               (data-values-from (let ((dt-node
                                                        (get-attribute-value 
                                                         !owl2:onDataRange attributes)))
                                                   (cond (dt-node
                                                          (owl-as-datarange dt-node node-ht))
                                                         ((datatype-property-node-p on-property node-ht)
                                                          +datatype-top-symbol+)
                                                         (t nil)))))

                          (cond (has-self
                                 `(self-reference ,(owl-as-role on-property node-ht)))
                                        
                                (has-value
                                 (if (literal-p has-value)
                                     `(d-filler ,(owl-as-role on-property node-ht)
                                                ,(owl-as-literal has-value 
                                                                 node-ht 
                                                                 (role-range-spec on-property node-ht)))
                                   (let ((ind (owl-as-individual has-value node-ht)))
                                     `(has-value ,(owl-as-role on-property node-ht) ,ind))))

                                (some-values-from 
                                 (cond ((or (node-eq type !owl2:ObjectRestriction)
                                            (and (node-eq type !owl:Restriction)
                                                 (only-object-property-p on-property node-ht)))
                                        `(some ,(owl-as-role on-property node-ht) 
                                               ,(owl-as-concept some-values-from node-ht)))
                                       ((or (node-eq type !owl2:DataRestriction)
                                            (and (node-eq type !owl:Restriction)
                                                 (only-datatype-property-p on-property node-ht)))
                                        `(d-some ,(owl-as-role on-property node-ht)
                                                 ,(owl-as-datarange some-values-from node-ht)))
                                       ((known-datatype-p some-values-from)
                                        (racer-warn "owl:SomeValuesFrom term for role ~A is ambiguous -- datatype found as qualified. The property is treated as a data property."
                                                    (owl-as-role on-property node-ht))
                                        `(d-some ,(owl-as-role on-property node-ht)
                                                 ,(owl-as-datarange some-values-from node-ht)))
                                       (t
                                        (racer-warn "owl:SomeValuesFrom term for role ~A is ambiguous -- replaced with top."
                                                    (owl-as-role on-property node-ht))
                                        'top)))

                                (all-values-from 
                                 (cond ((or (node-eq type !owl2:ObjectRestriction)
                                            (and (node-eq type !owl:Restriction)
                                                 (only-object-property-p on-property node-ht)))
                                        `(all ,(owl-as-role on-property node-ht) 
                                              ,(owl-as-concept all-values-from node-ht)))
                                       ((or (node-eq type !owl2:DataRestriction)
                                            (and (node-eq type !owl:Restriction)
                                                 (only-datatype-property-p on-property node-ht)))
                                        `(d-all ,(owl-as-role on-property node-ht)
                                                ,(owl-as-datarange all-values-from node-ht)))
                                       ((known-datatype-p all-values-from)
                                        (racer-warn "owl:AllValuesFrom term for role ~A is ambiguous -- datatype found as qualified. The property is treated as a data property."
                                                    (owl-as-role on-property node-ht))
                                        `(d-all ,(owl-as-role on-property node-ht)
                                                ,(owl-as-datarange all-values-from node-ht)))
                                       (t (racer-warn "owl:AllValuesFrom term for role ~A is ambiguous -- replaced with top."
                                                      (owl-as-role on-property node-ht))
                                          'top)))

                                (cardinality
                                 (cond ((null data-values-from)
                                        (let ((role (owl-as-role on-property node-ht)))
                                          `(exactly ,(transform-cardinality cardinality node-ht)
                                                    ,role
                                                    ,@(if object-values-from
                                                          (list object-values-from)
                                                        nil))))
                                       (t
                                        (let ((role (owl-as-role on-property node-ht)))
                                          `(d-exactly ,(transform-cardinality cardinality node-ht)
                                                      ,role
                                                      ,data-values-from)))))

                                (min-cardinality
                                 (cond  ((null data-values-from)
                                         (let ((role (owl-as-role on-property node-ht)))
                                           `(at-least ,(transform-cardinality min-cardinality node-ht)
                                                      ,role
                                                      ,@(if object-values-from
                                                            (list object-values-from)
                                                          nil))))
                                        (t (let ((role (owl-as-role on-property node-ht)))
                                             `(d-at-least ,(transform-cardinality min-cardinality node-ht)
                                                          ,role
                                                          ,data-values-from)))))

                                (max-cardinality
                                 (cond ((null data-values-from)
                                        (let ((role (owl-as-role on-property node-ht)))
                                          `(at-most ,(transform-cardinality max-cardinality node-ht)
                                                    ,role
                                                    ,@(if object-values-from
                                                          (list object-values-from)
                                                        nil))))
                                       (t (let ((role (owl-as-role on-property node-ht)))
                                            `(d-at-most ,(transform-cardinality max-cardinality node-ht)
                                                        ,role
                                                        ,data-values-from)))))

                                (t (racer-warn "Node ~A replaced with concept TOP." attributes)
                                   'top))))

                       ((or (node-eq type !rdfs:Class) (node-eq type !owl:Class))
                        (cond ((let ((class (get-attribute-value !owl:complementOf attributes)))
                                 (if class
                                     `(not ,(owl-as-concept class node-ht)))))
                              ((let ((class (get-attribute-value !owl:intersectionOf attributes)))
                                 (if class
                                     `(and .,(mapcar #'(lambda (node) 
                                                         (owl-as-concept node node-ht))
                                                     (owl-as-list class node-ht))))))
                              ((let ((class (get-attribute-value !owl:unionOf attributes)))
                                 (if class
                                     (let ((disjuncts (mapcar #'(lambda (node) 
                                                                  (owl-as-concept node node-ht))
                                                              (owl-as-list class node-ht))))
                                       `(or .,disjuncts)))))
                              ((let ((class (get-attribute-value !owl:oneOf attributes)))
                                 (if class
                                     (let ((inds (mapcar #'(lambda (node)
                                                             (owl-as-individual node node-ht))
                                                         (owl-as-list class node-ht))))
                                       (unless (loop for ind in inds always (symbolp ind))
                                         (error "One-of concept must contain individuals only - found: ~A." inds))
                                       `(one-of .,inds)))))
                              (t
                               (when *tbox-verbose*
                                 (racer-warn "Node ~A replaced with concept TOP." attributes))
                               'top)))

                       (t (when *tbox-verbose*
                            (racer-warn "Ignoring node ~A." attributes))
                          (values 'top t))))

             (values 
              (intern node-uri)
              nil  ;; not ignored, sonst 2. Wert von intern -> :internal! 
              ))))))

(defun known-datatype-p (node)
  (or (node-eq node !xsd:byte)
      (node-eq node !xsd:negativeInteger)
      (node-eq node !xsd:nonPositiveInteger)
      (node-eq node !xsd:positiveInteger)
      (node-eq node !xsd:nonNegativeInteger)
      (node-eq node !xsd:integer)
      (node-eq node !xsd:unsignedShort)
      (node-eq node !xsd:unsignedInt)
      (node-eq node !xsd:unsignedLong)
      (node-eq node !xsd:unsignedByte)
      (node-eq node !xsd:long)
      (node-eq node !xsd:int)
      (node-eq node !xsd:short)
      (node-eq node !xsd:float)
      (node-eq node !xsd:double)
      (node-eq node !xsd:decimal)
      (node-eq node !xsd:boolean)
      (node-eq node !rdf:XMLLiteral)
      (node-eq node !rdfs:Literal)
      (node-eq node !xsd:anyURI) 
      (node-eq node !xsd:base64binary) 
      (node-eq node !xsd:hexBinary) 
      (node-eq node !xsd:string) 
      (node-eq node !xsd:normalizedString)
      (node-eq node !xsd:token)
      (node-eq node !xsd:language)
      (node-eq node !xsd:Name)
      (node-eq node !xsd:NMTOKEN)
      (node-eq node !xsd:NMTOKENS)
      (node-eq node !xsd:NCName)
      (node-eq node !xsd:ID)
      (node-eq node !xsd:IDREF)
      (node-eq node !xsd:IDREFS)
      (node-eq node !xsd:ENTITY)
      (node-eq node !xsd:ENTITIES)
      (node-eq node !xsd:QName)
      (node-eq node !xsd:date)
      (node-eq node !xsd:time)
      (node-eq node !xsd:dateTime)
      (node-eq node !xsd:gYear)
      (node-eq node !xsd:gYearMonth)
      (node-eq node !xsd:gMonth)
      (node-eq node !xsd:gMonthDay)
      (node-eq node !xsd:gDay)
      (node-eq node !xsd:duration)
      (let ((name (wilbur:node-uri node)))
        (when name
          (member (intern name) *user-defined-datatypes*)))))

(defun owl-as-datatype (node node-ht)
  (owl-as-datarange node node-ht))

(defun owl-as-individual (node node-ht)
  (if (literal-p node)
      (intern (literal-string node))
    (let ((node-uri (node-uri node)))
      (if node-uri
          (intern node-uri)
        (let ((found (gethash node node-ht)))
          (or found 
              (setf (gethash node node-ht) (gensym "ANONYMOUS"))))))))

(defun owl-as-literal (node node-ht default-type) 
  (cond ((literal-datatype node)
	 `(d-literal ,(string-trim '(#\Newline #\Return #\Tab #\Space) 
				   (literal-string node))
		     ,(owl-as-datatype (literal-datatype node) node-ht)))
	(default-type
	    `(d-literal ,(string-trim '(#\Newline #\Return #\Tab #\Space) 
				      (literal-string node))
                        ,(owl-as-datatype default-type node-ht)))
	(t `(d-literal ,(literal-string node) ,(owl-as-datatype !xsd:string node-ht)))))

(defun owl-as-ontology (node node-ht)
  (let ((node-uri (node-uri node)))
    (if node-uri
        (intern node-uri)
      (let ((found (gethash node node-ht)))
        (or found 
            (setf (gethash node node-ht) (gensym "ANONYMOUS")))))))

(defun owl-as-entity (node node-ht)
  (let ((node-uri (node-uri node)))
    (if node-uri
        (intern node-uri)
      (let ((found (gethash node node-ht)))
        (or found 
            (setf (gethash node node-ht) (gensym "ANONYMOUS")))))))

(defun owl-as-list (node node-ht &optional (acc nil))
  (if (node-eq node !rdf:nil)
      (reverse acc)
      
    (let* ((attributes (get-attributes node node-ht))
           (type (get-attribute-value !rdf:type attributes)))

      (if (and (null type)
               (get-attribute-value !rdf:first attributes))
          (setf type !rdf:List))

      (cond 
       ((node-eq type !rdf:List) ;(node-eq type !rdf:List)
        (owl-as-list 
         (get-attribute-value !rdf:rest attributes)
         node-ht
         (cons (get-attribute-value !rdf:first attributes) acc)))

       ((node-eq type !rdf:Seq)
        (construct-list-from-sequence attributes))

       ((node-eq type !rdf:nil) 
        (reverse acc))

       (t (when *tbox-verbose*
            (racer-warn "Ignoring node ~A." attributes))
          (reverse acc))))))


(defun construct-list-from-sequence (attributes)
  (loop for (predicate value) in attributes
        when (list-item-p predicate)
        collect value))

(defun owl-as-datarange (node node-ht) 
  (let* ((attributes (get-attributes node node-ht))
         (type (get-attribute-value !rdf:type attributes)))
    
    (cond ((known-datatype-p node)
           `(d-base-type ,(intern (node-uri node))))

          ((node-eq type !rdfs:Datatype)
	   
	   (let* ((on-datatype (or (get-attribute-value !owl2:onDatatype attributes)
                                   (get-attribute-value !owl2:onDataType attributes)))
		  
                  (restrictions (get-attribute-value !owl2:withRestrictions attributes))
		  (facets (and restrictions
			       (owl-as-list restrictions node-ht)))
		  (min-inclusive-facets (owl-as-facet-list !xsd:minInclusive facets node-ht))
		  (min-exclusive-facets (owl-as-facet-list !xsd:minExclusive facets node-ht))
		  (max-inclusive-facets (owl-as-facet-list !xsd:maxInclusive facets node-ht))
		  (max-exclusive-facets (owl-as-facet-list !xsd:maxExclusive facets node-ht))
		  (min-length-facets (owl-as-facet-list !xsd:minLength facets node-ht))
		  (max-length-facets (owl-as-facet-list !xsd:maxLength facets node-ht))
		  (length-facets (owl-as-facet-list !xsd:length facets node-ht))
		  (pattern-facets (owl-as-facet-list !oxsd:pattern facets node-ht))
		  (total-digits-facets (owl-as-facet-list !xsd:totalDigits facets node-ht))
		  (fraction-digits-facet (owl-as-facet-list !xsd:fractionDigits facets node-ht))

                  (possible-values (get-attribute-value !owl:oneOf attributes))
                  (intersection-of (or (get-attribute-value !owl:intersectionOf attributes)))
                  (union-of (or (get-attribute-value !owl:unionOf attributes)))
                  (complement-of (or (get-attribute-value !owl:complementOf attributes)
                                     (get-attribute-value !owl:datatypeComplementOf attributes))))           

             (cond (intersection-of
                    (let* ((datarange 
                            (owl-as-list intersection-of node-ht))
                           (ranges (mapcar #'(lambda (x) 
                                               (owl-as-datarange x node-ht))
                                           datarange)))
                      `(d-and 
                        ,@ranges)))
                   
                   (union-of
                    (let* ((datarange 
                            (owl-as-list union-of node-ht))
                           (ranges (mapcar #'(lambda (x) 
                                               (owl-as-datarange x node-ht))
                                           datarange)))
                      `(d-or 
                        ,@ranges)))

                   (complement-of
                    `(d-complement ,(owl-as-datarange complement-of node-ht)))

                   (possible-values
                    `(d-possible-values 
                      .,(mapcar #'(lambda (node) (owl-as-literal node node-ht nil))
                                (owl-as-list possible-values node-ht))))

                   ((or restrictions facets)
                    
                    `(d-restriction 
                      ,@(if on-datatype
                            (list (owl-as-datatype on-datatype node-ht))
                          nil)
                      ,@min-inclusive-facets
                      ,@min-exclusive-facets
                      ,@max-inclusive-facets
                      ,@max-exclusive-facets
                      ,@min-length-facets
                      ,@max-length-facets
                      ,@length-facets
                      ,@pattern-facets
                      ,@total-digits-facets
                      ,@fraction-digits-facet))

                   (t 
                    (if (node-uri node)
                        `(d-base-type ,(intern (node-uri node)))
                      (progn 
                        (racer-warn "Malformed DataRange expression -- treated as string.")
                        `(d-base-type ,(intern (node-uri !xsd:string)))))))))
          
          ((node-eq type !owl:DataRange) ; old  OWL 1.1 (?)
           (let ((complement-of
                  (or (get-attribute-value !owl:complementOf attributes)
                      (get-attribute-value !owl:datatypeComplementOf attributes))))
                 
             (cond (complement-of
                    (let ((datarange (owl-as-datarange complement-of node-ht)))
                      `(d-complement ,datarange)))
                    
                   (t

                    (let* ((on-datarange (get-attribute-value !owl2:onDataRange attributes))
                           (possible-values (get-attribute-value !owl:oneOf attributes))
                           (restrictions (get-attribute-value !owl2:withRestrictions attributes))
                           (facets (and restrictions
                                        (owl-as-list restrictions node-ht)))
                           (min-inclusive-facets (owl-as-facet-list !xsd:minInclusive facets node-ht))
                           (min-exclusive-facets (owl-as-facet-list !xsd:minExclusive facets node-ht))
                           (max-inclusive-facets (owl-as-facet-list !xsd:maxInclusive facets node-ht))
                           (max-exclusive-facets (owl-as-facet-list !xsd:maxExclusive facets node-ht))
                           (min-length-facets (owl-as-facet-list !xsd:minLength facets node-ht))
                           (max-length-facets (owl-as-facet-list !xsd:maxLength facets node-ht))
                           (length-facets (owl-as-facet-list !xsd:length facets node-ht))
                           (pattern-facets (owl-as-facet-list !oxsd:pattern facets node-ht))
                           (total-digits-facets (owl-as-facet-list !xsd:totalDigits facets node-ht))
                           (fraction-digits-facet (owl-as-facet-list !xsd:fractionDigits facets node-ht)))

                      `(d-datarange 
                        ,@(if on-datarange
                              (list (owl-as-datatype on-datarange node-ht))
                            nil)

                        ,@(if possible-values 
                              `((d-possible-values 
                                 .,(mapcar #'(lambda (node) (owl-as-literal node node-ht nil))
                                           (owl-as-list (get-attribute-value !owl:oneOf attributes) node-ht))))
                            nil)
                        ,@min-inclusive-facets
                        ,@min-exclusive-facets
                        ,@max-inclusive-facets
                        ,@max-exclusive-facets
                        ,@min-length-facets
                        ,@max-length-facets
                        ,@length-facets
                        ,@pattern-facets
                        ,@total-digits-facets
                        ,@fraction-digits-facet))))))

          (t 
           (if (node-uri node)
               `(d-base-type ,(intern (node-uri node)))
             (progn 
               (racer-warn "Malformed DataRange expression -- treated as string.")
               `(d-base-type ,(intern (node-uri !xsd:string)))))))))

(defun owl-as-facet-list (facet facets node-ht)
  (loop for node in facets
        for attributes = (get-attributes node node-ht)
        for facet-value = (get-attribute-value facet attributes)
        when facet-value 
        collect `(d-facet ,(owl-as-facet facet) 
                          ,(owl-as-literal facet-value node-ht *role-range*))))

(defun owl-as-facet (node)
  (let ((node-uri (node-uri node)))
    (if node-uri
        (intern node-uri)
      (error "Errorneous facet specification found."))))



(defun transform-cardinality (spec node-ht)
  (declare (ignore node-ht))
  (let ((cardinality
         (cond ((stringp spec)
                (ignore-errors (read-from-string spec)))
               ((literal-p spec)
                (ignore-errors 
                  (read-from-string (literal-string spec))))
               (t (error "Erroneous cardinality specification ~A." spec)))))
    (unless (and (integerp cardinality) (>= cardinality 0))
      (error "Malformed cardinality specification ~A." spec))
    cardinality))


#+:ignore
(defun owl-as-role (node node-ht)
  (if (node-uri node)
      (intern (node-uri node))
    (owl-as-role-composition-or-inverse node node-ht)))


(defun owl-as-role (node node-ht)
  (let (node-uri)
    (cond ((null node)
           nil)
          ((setf node-uri (node-uri node))
           (cond ((node-eq node !owl:topObjectProperty)
                  +top-object-role-symbol+)
                 ((node-eq node !owl:topDataProperty)
                  +top-datatype-role-symbol+)
                 ((node-eq node !owl:bottomObjectProperty)
                  +bottom-object-role-symbol+)
                 ((node-eq node !owl:bottomDataProperty)
                  +bottom-datatype-role-symbol+)
                 (t (intern node-uri ;; (find-package :racer-user)
                            ))))
          (t (owl-as-role-composition-or-inverse node node-ht)))))


(defun owl-as-role-composition-or-inverse (node node-ht)
  (if (node-eq node !rdf:nil)
      nil
    (let* ((attributes (get-attributes node nil))
           (type (get-attribute-value !rdf:type attributes))
           (inverse (get-attribute-value !owl:inverseOf attributes))
           (car (get-attribute-value !rdf:first attributes))
           (cdr (get-attribute-value !rdf:rest attributes)))
      (cond (inverse
             `(inv ,(owl-as-role inverse node-ht)))
             ((or (node-eq type !rdf:List) (node-eq type !rdf:Seq))
             (cons (owl-as-role (get-attribute-value !rdf:first attributes) node-ht)
                   (owl-as-role-composition-or-inverse (get-attribute-value !rdf:rest attributes) node-ht)))
            ((and car cdr)
             (cons (owl-as-role car node-ht)
                   (owl-as-role-composition-or-inverse cdr node-ht)))
            (t (error "Malformed property expression."))))))

(defun owl-as-variable (var-name)
  (intern (node-uri var-name)))


;;; ======================================================================

(defun process-owl-triple (subject predicate object tbox abox 
                                   node-ht axiom-ht rule-ht var-ht) 
  (declare (ignore abox tbox))

  ;;; (pprint (list subject predicate object))

  (cond ((and (node-eq predicate !rdf:type)
              (node-uri subject)
              (or (node-eq object !owl:Class)
                  (node-eq object !rdfs:Class)))

         (let ((subject1 (owl-as-concept subject node-ht))) 
           (ensure-axiom axiom-ht subject1 predicate object
                         (|OWLAPI-getOWLDeclarationAxiom| 
                          `(|Class| ,subject1)))))
        
        ((and (node-eq predicate !rdf:type)
              (node-uri subject)
              (node-eq object !owl:NamedIndividual))
              
         (let ((subject1 (owl-as-individual subject node-ht))) 
           (ensure-axiom axiom-ht subject1 predicate object
                         (|OWLAPI-getOWLDeclarationAxiom| 
                          `(|NamedIndividual| ,subject1)))))
        
        ((and (node-eq predicate !rdf:type)
              (node-uri subject)
              (node-eq object !owl:AnnotationProperty))
              
         (let ((subject1 (owl-as-role subject node-ht))) 
           (ensure-axiom axiom-ht subject1 predicate object
                         (|OWLAPI-getOWLDeclarationAxiom| 
                          `(|AnnotationProperty| ,subject1)))))
        
        ((and (node-eq predicate !rdf:type)
              (node-uri subject)
              (node-eq object !owl:ObjectProperty))
              
         (let ((subject1 (owl-as-role subject node-ht))) 
           (unless (consp subject1)
             (ensure-axiom axiom-ht subject1 predicate object
                           (|OWLAPI-getOWLDeclarationAxiom| 
                            `(|ObjectProperty| ,subject1))))))

        ((and (node-eq predicate !rdf:type)
              (node-uri subject)
              (node-eq object !owl:DataProperty))
              
         (let ((subject1 (owl-as-role subject node-ht))) 
           (ensure-axiom axiom-ht subject1 predicate object
                         (|OWLAPI-getOWLDeclarationAxiom| 
                          `(|DataProperty| ,subject1)))))

	((and (node-eq predicate !rdf:type)
              (node-uri subject)
              (node-eq object !rdfs:Datatype))
         (let ((subject1 (owl-as-concept subject node-ht))) ; so lassen, korrekt
           (push subject1 *user-defined-datatypes*)
           (ensure-axiom axiom-ht subject1 predicate object
                         (|OWLAPI-getOWLDeclarationAxiom| 
                          `(|Datatype| ,subject1)))))

        ;;;
        ;;;
        ;;; 

        ((and (class-node-p subject node-ht)
              (node-eq predicate !owl:hasKey))

         (let* ((concept (owl-as-concept subject node-ht))
                (properties (owl-as-list object node-ht))
                (roles nil)
                (datatype-roles nil))

           (loop for property in properties do
                 (cond ((only-object-property-p property node-ht)
                        (push (owl-as-role property node-ht) roles))
                       ((only-datatype-property-p property node-ht)
                        (push (owl-as-role property node-ht) datatype-roles))
                       (t
                        (let ((role (owl-as-role property node-ht)))
                          (racer-warn "Ambiguous use of property name in !owl:hasKey. Cannot determine whether ~A is either a datatype or object property -- treated as object property."
                                      role)
                          (push role roles)))))

           (ensure-axiom axiom-ht concept predicate object
                         (|OWLAPI-getOWLHasKeyAxiom| 
                          concept roles datatype-roles))))

	((and (node-eq predicate !rdf:type)
              (node-eq object !owl:AllDisjointClasses))
         (let* ((attributes (get-attributes subject node-ht))
                (members (get-attribute-value !owl:members attributes))
                (concepts (mapcar (lambda (class) (owl-as-concept class node-ht))
                                  (owl-as-list members node-ht)))
                (axiom 
                 (ensure-axiom axiom-ht concepts predicate object
                               (|OWLAPI-getOWLDisjointClassesAxiom| concepts))))

           (unless (owlapi:dont-keep-axioms-p *cur-reasoner*)
             (process-axiom-annotations axiom attributes node-ht))
           axiom))
                          
	((and (node-eq predicate !rdf:type)
              (node-eq object !owl:AllDisjointProperties))
         
         (let* ((attributes (get-attributes subject node-ht))
                (members (get-attribute-value !owl:members attributes))
                (roles nil)
                (datatype-roles nil))

           (loop for property in (owl-as-list members node-ht) do
                 (cond ((only-object-property-p property node-ht)
                        (push (owl-as-role property node-ht) roles))
                       ((only-datatype-property-p property node-ht)
                        (push (owl-as-role property node-ht) datatype-roles))
                       (t
                        (let ((role (owl-as-role property node-ht)))
                          (racer-warn "Ambiguous use of property name in !owl:AllDisjointProperties. Cannot determine whether ~A is either a datatype or object property -- treated as object property."
                                      role)
                          (push role roles)))))


           (when roles
             (let ((axiom
                    (ensure-axiom axiom-ht roles predicate object 
                                  (|OWLAPI-getOWLDisjointObjectPropertiesAxiom| roles))))

               (unless (owlapi:dont-keep-axioms-p *cur-reasoner*)
                 (process-axiom-annotations axiom attributes node-ht))

               axiom))

           (when datatype-roles
             (let ((axiom
                    (ensure-axiom axiom-ht datatype-roles predicate object 
                                  (|OWLAPI-getOWLDisjointDataPropertiesAxiom| datatype-roles))))

               (unless (owlapi:dont-keep-axioms-p *cur-reasoner*)
                 (process-axiom-annotations axiom attributes node-ht))

               axiom))))


        ((node-eq predicate !owl:propertyDisjointWith)
         (let ((role1 (owl-as-role subject node-ht))
               (role2 (owl-as-role object node-ht)))

           (cond ((and (only-object-property-p subject node-ht)
                       (only-object-property-p object node-ht))
                       
                  (ensure-axiom axiom-ht role1 predicate role2
                                (|OWLAPI-getOWLDisjointObjectPropertiesAxiom| (list role1 role2))))

                 ((and (only-datatype-property-p subject node-ht)
                       (only-datatype-property-p object node-ht))
                       
                  (ensure-axiom axiom-ht role1 predicate role2
                                (|OWLAPI-getOWLDisjointDataPropertiesAxiom| (list role1 role2))))

                 (t (racer-warn "Ambiguous use of property names in !owl:propertyDisjointWith. Assuming both roles ~A are object properties."
                                (list role1 role2))
                    (ensure-axiom axiom-ht role1 predicate role2
                                  (|OWLAPI-getOWLDisjointObjectPropertiesAxiom| (list role1 role2)))))))

	((and (node-eq predicate !rdf:type)
              (node-eq object !owl2:ReflexiveProperty))
         (when (node-uri subject)
           (let ((role (owl-as-role subject node-ht)))
             (ensure-axiom axiom-ht role predicate object
                           (|OWLAPI-getOWLReflexiveObjectPropertyAxiom| role)))))

        ((and (node-eq predicate !rdf:type)
              (node-eq object !owl2:IrreflexiveProperty))
	 (when (node-uri subject)
           (let ((role (owl-as-role subject node-ht)))
             (when (node-uri subject) ;; ?? 
               (ensure-axiom axiom-ht role predicate object
                             (|OWLAPI-getOWLIrreflexiveObjectPropertyAxiom| role))))))
	
        ((and (node-eq predicate !rdf:type)
              (node-eq object !owl:SymmetricProperty))
	 (when (node-uri subject)
           (let ((role (owl-as-role subject node-ht)))
             (ensure-axiom axiom-ht role predicate object
                           (|OWLAPI-getOWLSymmetricObjectPropertyAxiom| role)))))
	
        ((and (node-eq predicate !rdf:type)
              (node-eq object !owl2:AsymmetricProperty))
         (when (node-uri subject)
           (let ((role (owl-as-role subject node-ht)))
             (ensure-axiom axiom-ht role predicate object
                           (|OWLAPI-getOWLAsymmetricObjectPropertyAxiom| role)))))
	
	((and (class-node-p subject node-ht)
	      (or (node-eq predicate !rdfs:subClassOf) 
		  (node-eq predicate !owl:subClassOf)))
         (let ((subject1 (owl-as-concept subject node-ht))
               (object1 (owl-as-concept object node-ht)))
           (ensure-axiom axiom-ht subject1 predicate object1
                         (|OWLAPI-getOWLSubClassAxiom| subject1 object1))))
	
        ((and (class-node-p subject node-ht)
	      (node-eq predicate !owl:equivalentClass))

         (let* ((subject1 (owl-as-concept subject node-ht))
                (object1 (owl-as-concept object node-ht)))

           (ensure-axiom axiom-ht subject1 predicate object1
                         (|OWLAPI-getOWLEquivalentClassesAxiom| 
                          (list subject1 object1))))) 

        ((and (not (class-node-p subject node-ht))
	      (node-eq predicate !owl:equivalentClass))

         (let* ((subject1 (owl-as-concept subject node-ht))
                (object1 (owl-as-datarange object node-ht)))
           (push subject1 *user-defined-datatypes*)
           (ensure-axiom axiom-ht subject predicate object
                         (|OWLAPI-getOWLDatatypeDefinitionAxiom| 
                          subject1 object1))))
	 
        ((and (class-node-p subject node-ht)
	      (node-eq predicate !owl:oneOf))
	 (when (node-uri subject)
           (let ((inds (mapcar #'(lambda (node) (owl-as-individual node node-ht)) 
                               (owl-as-list object node-ht)))
                 (subject1 (owl-as-concept subject node-ht)))
             (unless (loop for ind in inds always (symbolp ind))
               (error "One-of concept must contain individuals only - found: ~A." inds))
             (loop for ind in inds do
                   (|OWLAPI-getOWLClassAssertionAxiom| ind ind))
             (ensure-axiom axiom-ht subject1 predicate inds
                           (|OWLAPI-getOWLEquivalentClassesAxiom| 
                            (list subject1 
                                  `(one-of . ,inds)))))))
	
        ((and (class-node-p subject node-ht)
	      (node-eq predicate !owl:intersectionOf))
	 (when (node-uri subject)
           (let ((concepts (mapcar #'(lambda (node) 
                                       (owl-as-concept node node-ht))
                                   (owl-as-list object node-ht)))
                 (subject1 (owl-as-concept subject node-ht)))
             (ensure-axiom axiom-ht subject1 predicate concepts
                           (|OWLAPI-getOWLEquivalentClassesAxiom| 
                            (list subject1 
                                  `(and . ,concepts)))))))
	
        ((and (class-node-p subject node-ht)
	      (node-eq predicate !owl:unionOf))
	 (when (node-uri subject)
           (let ((concepts (mapcar #'(lambda (node) 
                                       (owl-as-concept node node-ht))
                                   (owl-as-list object node-ht)))
                 (subject1 (owl-as-concept subject node-ht)))
             (ensure-axiom axiom-ht subject1 predicate concepts
                           (|OWLAPI-getOWLEquivalentClassesAxiom| 
                            (list subject1
                                  `(or . ,concepts)))))))
	
        ((and (class-node-p subject node-ht)
	      (node-eq predicate !owl:complementOf))
	 (when (node-uri subject)
           (let ((concept (owl-as-concept object node-ht))
                 (subject1 (owl-as-concept subject node-ht)))
	   
             (ensure-axiom axiom-ht subject1 predicate concept
                           (|OWLAPI-getOWLEquivalentClassesAxiom| 
                            (list subject1
                                  `(not ,concept)))))))
	
        ((and (class-node-p subject node-ht)
	      (or (node-eq predicate !owl:disjointUnionOf)
                  (node-eq predicate !owl2:disjointUnionOf)))
	 (when (node-uri subject)
           (let ((disjuncts (mapcar #'(lambda (node) (owl-as-concept node node-ht))
                                    (owl-as-list object node-ht)))
                 (subject1 (owl-as-concept subject node-ht)))
	   
             (ensure-axiom axiom-ht subject1 predicate disjuncts
                           (|OWLAPI-getOWLDisjointUnionAxiom| subject1  
                                                              disjuncts)))))

        ;;;
        ;;; werden noch benoetigt! kein (node-uri subject) gefordert! -> implicit
        ;;; 
	
        ((and (node-eq predicate !rdf:type)
              (or (node-eq object !owl:ObjectProperty)
                  (node-eq object !rdfs:Property)))
         (let ((role (owl-as-role subject node-ht)))
           (unless (consp role)
             (ensure-axiom axiom-ht role predicate object
                           (;|OWLAPI-getOWLImplicitDeclarationAxiom| 
                            |OWLAPI-getOWLDeclarationAxiom| 
                            `(|ObjectProperty| ,role))))))
	
        ((and (node-eq predicate !rdf:type)
              (node-eq object !owl:AnnotationProperty))
         (let ((role (owl-as-role subject node-ht)))
           (ensure-axiom axiom-ht role predicate object
                         (;|OWLAPI-getOWLImplicitDeclarationAxiom|
                          |OWLAPI-getOWLDeclarationAxiom| 
                          `(|AnnotationProperty| ,role)))))
	
        ((and (node-eq predicate !rdf:type)
              (node-eq object !owl:DatatypeProperty))
         (let ((role (owl-as-role subject node-ht)))
           (ensure-axiom axiom-ht role predicate object
                         (;|OWLAPI-getOWLImplicitDeclarationAxiom|
                          |OWLAPI-getOWLDeclarationAxiom|
                          `(|DataProperty| ,role)))))

        ((and (node-eq predicate !rdf:type)
              (node-eq object !owl2:Individual))
         (let ((ind (owl-as-individual subject node-ht)))
           (ensure-axiom axiom-ht ind predicate object
                         (;|OWLAPI-getOWLImplicitDeclarationAxiom|
                          |OWLAPI-getOWLDeclarationAxiom|
                          `(|Individual| ,ind)))))

        ;;;
        ;;;
        ;;;
	
        ((node-eq predicate !owl2:equivalentObjectProperties)
         (let ((role1 (owl-as-role subject node-ht))
               (role2 (owl-as-role object node-ht)))
           (ensure-axiom axiom-ht role1 predicate role2
                         (|OWLAPI-getOWLEquivalentObjectPropertiesAxiom| 
                          (list role1 role2)))))

        ((node-eq predicate !owl2:equivalentDataProperties)
         (let ((role1 (owl-as-role subject node-ht))
               (role2 (owl-as-role object node-ht)))
           (ensure-axiom axiom-ht role1 predicate role2
                         (|OWLAPI-getOWLEquivalentDataPropertiesAxiom| 
                          (list role1 role2)))))

        ((node-eq predicate !owl:equivalentProperty)
	 (let ((role2 (owl-as-role subject node-ht))
               (role1 (owl-as-role object node-ht)))
	   (cond ((or (only-datatype-property-p subject node-ht)
                      (only-datatype-property-p object node-ht))
                  (ensure-axiom axiom-ht role2 predicate role1
                                (|OWLAPI-getOWLEquivalentDataPropertiesAxiom| 
                                 (list role1 role2))))
                 ((or ;;(inverse-property-p subject)  We assume that OWL does not allow (inv r) to be used.
                      ;;(inverse-property-p object) 
                      (only-object-property-p subject node-ht)
                      (only-object-property-p object node-ht))
                  (ensure-axiom axiom-ht role2 predicate role1
                                (|OWLAPI-getOWLEquivalentObjectPropertiesAxiom| 
                                 (list role1 role2))))
                 (t (racer-warn "Ambiguous use of property name in owl:equivalentProperty declaration. Cannot determine whether ~A or ~A is either a datatype or object property -- treated as object property."
                                role1 role2)
                    (ensure-axiom axiom-ht role2 predicate role1
                                  (|OWLAPI-getOWLEquivalentObjectPropertiesAxiom| 
                                   (list role1 role2)))))))

        ((node-eq predicate !owl2:disjointObjectProperties)
         (let ((role2 (owl-as-role subject node-ht))
               (role1 (owl-as-role object node-ht)))
           (ensure-axiom axiom-ht role2 predicate role1
                         (|OWLAPI-getOWLDisjointObjectPropertiesAxiom| 
                          (list role1 role2)))))

        ((node-eq predicate !owl2:disjointDataProperties)
         (let ((role2 (owl-as-role subject node-ht))
               (role1 (owl-as-role object node-ht)))
	   (ensure-axiom axiom-ht role2 predicate role1
                         (|OWLAPI-getOWLDisjointDataPropertiesAxiom| (list role1 role2)))))
	
        ((node-eq predicate !owl2:declaredAs)         
         (cond ((node-eq object !owl:Class)
                (let ((ind (owl-as-individual subject node-ht)))
                  (ensure-axiom axiom-ht ind predicate object
                                (|OWLAPI-getOWLDeclarationAxiom| 
                                 `(|Class| ,ind)))))
               ((node-eq object !rdfs:Datatype)
                (let ((ind (owl-as-individual subject node-ht)))
                  (ensure-axiom axiom-ht ind predicate object
                                (|OWLAPI-getOWLDeclarationAxiom| 
                                 `(|Datatype| ,ind)))))
               ((node-eq object !owl:ObjectProperty)
                (let ((ind (owl-as-individual subject node-ht)))
                  (unless (consp ind)
                    (ensure-axiom axiom-ht ind predicate object
                                  (|OWLAPI-getOWLDeclarationAxiom|
                                   `(|ObjectProperty| ,ind))))))
               ;;((node-eq object !owl:AnnotationProperty)
               ;; (ensure-axiom axiom-ht subject predicate object
               ;;               (|OWLAPI-getOWLDeclarationAxiom| `(|AnnotationProperty| ,(owl-as-individual subject node-ht)))))
               ((node-eq object !owl:DatatypeProperty)
                (let ((ind (owl-as-individual subject node-ht)))
                  (ensure-axiom axiom-ht ind predicate object
                                (|OWLAPI-getOWLDeclarationAxiom| 
                                 `(|DataProperty| ,ind)))))
               ((node-eq object !owl2:Individual)
                (let ((ind (owl-as-individual subject node-ht)))
                  (ensure-axiom axiom-ht subject predicate object
                                (|OWLAPI-getOWLDeclarationAxiom| 
                                 `(|NamedIndividual| ,ind)))))
               (t (racer-warn "owl2:declaredAs ~A not allowed -- ignored." object))))
	
        ((and (node-eq predicate !owl:inverseOf)
              (node-uri subject))
         (let ((subject1 (owl-as-role subject node-ht) )
               (object1  (owl-as-role object node-ht) ))
           (ensure-axiom axiom-ht subject1 predicate object1
                         (|OWLAPI-getOWLInverseObjectPropertiesAxiom| 
                          subject1 
                          object1))))
	
        ((node-eq predicate !rdfs:domain)
         (let ((role-name (owl-as-role subject node-ht)))
           (cond ((only-datatype-property-p subject node-ht)
                  (let ((object1 (owl-as-concept object node-ht)))
                    (ensure-axiom axiom-ht role-name predicate object1
                                  (|OWLAPI-getOWLDataPropertyDomainAxiom| 
                                   role-name 
                                   object1))))
                 ((only-object-property-p subject node-ht)
                  (let ((object1 (owl-as-concept object node-ht)))
                    (ensure-axiom axiom-ht role-name predicate object1
                                  (|OWLAPI-getOWLObjectPropertyDomainAxiom|
                                   role-name
                                   object1))))
                 ((annotation-property-node-p subject node-ht)
                  (racer-warn "Domain declarations for annotation properties are ignored."))
                 (t (racer-warn "Ambiguous use of property name in rdfs:domain declaration. Cannot determine whether ~A is either a datatype or object property -- treated as object property."
                                role-name)
                    (let ((object1 
                           (owl-as-concept object node-ht)))
                      (ensure-axiom axiom-ht role-name predicate object1 
                                    (|OWLAPI-getOWLObjectPropertyDomainAxiom| role-name object1)))))))

        ((node-eq predicate !owl2:objectPropertyDomain)
         (let ((role-name (owl-as-role subject node-ht))
               (object1 
                (owl-as-concept object node-ht)))
           (ensure-axiom axiom-ht role-name predicate object1 
                         (|OWLAPI-getOWLObjectPropertyDomainAxiom| role-name object1))))

        ((node-eq predicate !owl2:dataPropertyDomain)
	 (let ((role-name (owl-as-role subject node-ht))
               (object1
                (owl-as-concept object node-ht)))
           (ensure-axiom axiom-ht role-name predicate object1 
                         (|OWLAPI-getOWLDataPropertyDomainAxiom| role-name object1))))

        ((node-eq predicate !rdfs:range)
         (let ((role-name (owl-as-role subject node-ht)))
           (cond ((only-datatype-property-p subject node-ht)
                  (let ((object1 
                         (owl-as-datatype object node-ht)))
                    (ensure-axiom axiom-ht role-name predicate object1 
                                  (|OWLAPI-getOWLDataPropertyRangeAxiom| role-name object1))))
                 ((let ((role-range (role-range-spec subject node-ht)))
                    (and role-range
                         (known-datatype-p role-range)))
                  (let ((object1 
                         (owl-as-datatype object node-ht)))
                    (ensure-axiom axiom-ht role-name predicate object1 
                                  (|OWLAPI-getOWLDataPropertyRangeAxiom| role-name object1))))
                 ((only-object-property-p subject node-ht)
                  (let ((object1 (owl-as-concept object node-ht)))
                    (ensure-axiom axiom-ht role-name predicate object1 
                                  (|OWLAPI-getOWLObjectPropertyRangeAxiom| role-name object1))))
                 ((annotation-property-node-p subject node-ht)
                  (racer-warn "Range declarations for annotation properties are ignored."))
                 (t (racer-warn "Ambiguous use of property name in rdfs:range declaration. Cannot determine whether ~A is either a datatype or object property -- treated as object property."
                                role-name)
                    (let ((object1 
                           (owl-as-concept object node-ht)))
                      (ensure-axiom axiom-ht role-name predicate object1 
                                    (|OWLAPI-getOWLObjectPropertyRangeAxiom| role-name object1)))))))

        ((node-eq predicate !owl2:objectPropertyRange)
         (let ((role-name (owl-as-role subject node-ht))
               (object1 
                (owl-as-concept object node-ht)))
           (ensure-axiom axiom-ht role-name predicate object1 
                         (|OWLAPI-getOWLObjectPropertyRangeAxiom| role-name object1))))

        ((node-eq predicate !owl2:dataPropertyRange)
         (let ((role-name (owl-as-role subject node-ht))
               (object1 
                (owl-as-datatype object node-ht)))
           (ensure-axiom axiom-ht role-name predicate object1 
                         (|OWLAPI-getOWLDataPropertyRangeAxiom| role-name object1))))

        ((node-eq predicate !owl:disjointWith)
         (let ((subject1 (owl-as-concept subject node-ht))
               (object1  (owl-as-concept object node-ht)))
           (ensure-axiom axiom-ht subject1 predicate object1
                         (|OWLAPI-getOWLDisjointClassesAxiom| (list subject1 object1)))))
	
        ((and (node-eq predicate !rdf:type)
              (node-eq object !owl:FunctionalProperty))

         (let ((role-name (owl-as-role subject node-ht)))
           (cond ((only-datatype-property-p subject node-ht)
                  (ensure-axiom axiom-ht role-name predicate object
                                (|OWLAPI-getOWLFunctionalDataPropertyAxiom| role-name)))
                 ((only-object-property-p subject node-ht)
                  (ensure-axiom axiom-ht role-name predicate object
                                (|OWLAPI-getOWLFunctionalObjectPropertyAxiom| role-name)))
                 ((let ((role-range (role-range-spec subject node-ht)))
                    (and role-range
                         (known-datatype-p role-range)))
                  (ensure-axiom axiom-ht role-name predicate object
                                (|OWLAPI-getOWLFunctionalDataPropertyAxiom| role-name)))
                 (t (racer-warn "Ambiguous use of property name in owl:FunctionalProperty declaration. Cannot determine whether ~A is either a datatype or object property -- treated as object property."
                                role-name)
                    (ensure-axiom axiom-ht role-name predicate object
                                  (|OWLAPI-getOWLFunctionalObjectPropertyAxiom| role-name))))))

        ((node-eq object !owl2:FunctionalObjectProperty)
         (let ((subject1 (owl-as-role subject node-ht)))
           (ensure-axiom axiom-ht subject1 predicate object
                         (|OWLAPI-getOWLFunctionalObjectPropertyAxiom| subject1))))

        ((node-eq object !owl2:FunctionalDataProperty)
         (let ((subject1 (owl-as-role subject node-ht)))
           (ensure-axiom axiom-ht subject1 predicate object
                         (|OWLAPI-getOWLFunctionalDataPropertyAxiom| subject1))))
	
        ((and (node-eq predicate !rdf:type)
              (node-eq object !owl:InverseFunctionalProperty))
         (let ((subject1 (owl-as-role subject node-ht)))
           (ensure-axiom axiom-ht subject1 predicate object
                         (|OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom| subject1))))
	
        ((and (node-eq predicate !rdf:type)
              (node-eq object !owl:TransitiveProperty))
         (let ((subject1 (owl-as-role subject node-ht)))
           (ensure-axiom axiom-ht subject predicate object
                         (|OWLAPI-getOWLTransitiveObjectPropertyAxiom| subject1))))

        ((and (node-eq predicate !rdf:type) 
              (node-eq object !owl:AllDifferent))
         (let* ((attributes (get-attributes subject node-ht))
                (distinct-members (or (get-attribute-value !owl:distinctMembers attributes)
                                      (get-attribute-value !owl:members attributes)))
                (disjoint-inds (mapcar #'(lambda (node) (owl-as-individual node node-ht))
                                       (owl-as-list distinct-members node-ht)))
                (axiom
                 (ensure-axiom axiom-ht disjoint-inds predicate object
                               (|OWLAPI-getOWLDifferentIndividualsAxiom| disjoint-inds))))

           (unless (owlapi:dont-keep-axioms-p *cur-reasoner*)
             (process-axiom-annotations axiom attributes node-ht))

           axiom))

        ((and (node-eq predicate !rdf:type)
              (node-eq object !swrl:Imp))
         (process-rule subject rule-ht))
	
        ((and (node-eq predicate !rdf:type)
              (node-eq object !swrl:Variable))
         (setf (gethash (owl-as-variable subject) var-ht) t))
	
        ((and (node-eq predicate !swrl:body))
         (process-body subject object rule-ht node-ht))        
	
        ((and (node-eq predicate !swrl:head))
         (process-head subject object rule-ht node-ht))        
	
        ((and (node-eq predicate !rdf:type)
              (node-eq object !owl:Ontology)))
	
        ((node-eq predicate !owl:imports)
         )

	((or (node-eq predicate !rdfs:subPropertyOf)
             (node-eq predicate !owl:subPropertyOf))

         (let* ((attributes (get-attributes subject node-ht))
                (comp-role (get-attribute-value !owl:propertyChain attributes)))
                
           (if comp-role
               (let ((composition (owl-as-role comp-role node-ht))
                     (role1 (owl-as-role object node-ht)))
                 (ensure-axiom axiom-ht composition predicate role1
                               (|OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom| composition role1)))

             (let ((role1 (owl-as-role subject node-ht))
                   (role2 (owl-as-role object node-ht)))
           
               (cond ((or (only-datatype-property-p subject node-ht)
                          (only-datatype-property-p object node-ht))
                      (ensure-axiom axiom-ht role1 predicate role2
                                    (|OWLAPI-getOWLDataSubPropertyAxiom| role1 role2)))

                     ((or ;; (inverse-property-p subject) We assume that OWL does not allow (inv r) to be used.
                          ;; (inverse-property-p object) 
                          (only-object-property-p subject node-ht)
                          (only-object-property-p object node-ht))
                  
                      (ensure-axiom axiom-ht role1 predicate role2
                                    (|OWLAPI-getOWLObjectSubPropertyAxiom| role1 role2)))

                     (t 

                      (racer-warn "Ambiguous use of property name in declaration ~A rdfs:subPropertyOf ~A  -- treated as object property." role1 role2)
                  
                      (ensure-axiom axiom-ht role1 predicate role2
                                    (|OWLAPI-getOWLObjectSubPropertyAxiom| role1 role2))))))))

        ((node-eq predicate !owl:subObjectPropertyOf)
         (let ((role2 (owl-as-role subject node-ht))
               (role1 (owl-as-role object node-ht)))
             (ensure-axiom axiom-ht role2 predicate role1
                           (|OWLAPI-getOWLObjectSubPropertyAxiom| role1 role2))))

        ((node-eq predicate !owl:subDataPropertyOf)
         (let ((role2 (owl-as-role subject node-ht))
               (role1 (owl-as-role object node-ht)))
	   (ensure-axiom axiom-ht role2 predicate role1
                         (|OWLAPI-getOWLDataSubPropertyAxiom| role1 role2))))
   
        ((node-eq predicate !owl:propertyChainAxiom) 
         (let* ((role2 (owl-as-role subject node-ht))
                (role1 (mapcar #'(lambda (x) (owl-as-role x node-ht))
                               (owl-as-list object node-ht))))
           (ensure-axiom axiom-ht role1 predicate role2
                         (|OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom| role1 role2))))

        ((and (node-p object)
              (node-p subject)
              (node-uri subject)
              (node-uri object)
              (node-eq predicate !owl:differentFrom))	 
         (let ((subject1 (owl-as-individual subject node-ht))
               (object1  (owl-as-individual object node-ht)))
           (ensure-axiom axiom-ht subject1 predicate object1
                         (|OWLAPI-getOWLDifferentIndividualsAxiom| (list subject1 object1)))))
	
        ((and (node-p object)
              (node-p subject)
              (node-uri subject)
              (node-uri object)
              (or (node-eq predicate !owl:sameAs)
                  (node-eq predicate !owl:sameIndividualAs)))
         (let ((subject1 (owl-as-individual subject node-ht))
               (object1  (owl-as-individual object node-ht)))
           (ensure-axiom axiom-ht subject1 predicate object1
                         (|OWLAPI-getOWLSameIndividualsAxiom| 
                          (list subject1 object1)))))
	
        ((and (node-eq predicate !rdf:type)
              (or (node-eq object !owl2:NegativeObjectPropertyAssertion)
                  (node-eq object !owl2:NegativeDataPropertyAssertion)
                  (node-eq object !owl2:NegativePropertyAssertion)))
	 
	 (let* ((attributes (get-attributes subject node-ht))
                
                (ref-subject (get-attribute-value !owl:sourceIndividual attributes))
                (ref-predicate (get-attribute-value !owl:assertionProperty attributes))
                (ref-object 
                 (or (get-attribute-value !owl:targetIndividual attributes)
                     (get-attribute-value !owl:targetValue attributes)))
                (ind1 (owl-as-individual ref-subject node-ht))
                (role (owl-as-role ref-predicate node-ht))

                (axiom

                 (cond ((or (node-eq object !owl2:NegativeDataPropertyAssertion)
                            (only-datatype-property-p ref-predicate node-ht)
                            (get-attribute-value !owl:targetValue attributes))
                            
                        (ensure-axiom axiom-ht subject predicate object
                                      (|OWLAPI-getOWLNegativeDataPropertyAssertionAxiom|
                                       ind1
                                       role
                                       (owl-as-literal ref-object node-ht 
                                                       (role-range-spec ref-predicate node-ht)))))

                       ((or (node-eq object !owl2:NegativeObjectPropertyAssertion)
                            (only-object-property-p ref-predicate node-ht))

                        (ensure-axiom axiom-ht subject predicate object
                                      (|OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom|
                                       ind1
                                       role
                                       (owl-as-individual ref-object node-ht))))

                       (t 
                        
                        (racer-warn "Ambiguous use of property name in owl:NegativePropertyAssertion. Cannot determine whether ~A is either a datatype or object property -- treated as object property."
                                    role)
                        
                        (ensure-axiom axiom-ht subject predicate object
                                      (|OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom|
                                       ind1
                                       role
                                       (owl-as-individual ref-object node-ht)))))))

           (unless (owlapi:dont-keep-axioms-p *cur-reasoner*)
             (process-axiom-annotations axiom attributes node-ht))

           axiom))

        ((and (node-p subject)
              (literal-p object))

         (unless (or (node-eq predicate !rdf:first)
                     (node-eq predicate !owl:minCardinality)
                     (node-eq predicate !owl:maxCardinality)
                     (node-eq predicate !owl:cardinality)
                     (node-eq predicate !owl:minQualifiedCardinality)
                     (node-eq predicate !owl:maxQualifiedCardinality)
                     (node-eq predicate !owl:qualifiedCardinality)
                     (node-eq predicate !owl:hasValue)
                     (node-eq predicate !owl2:onDataRange)
                     (node-eq predicate !owl:oneOf)
                     (node-eq predicate !owl2:withRestrictions)
                     (node-eq predicate !xsd:minInclusive)
                     (node-eq predicate !xsd:minExclusive)
                     (node-eq predicate !xsd:maxInclusive)
                     (node-eq predicate !xsd:maxExclusive)
                     (node-eq predicate !xsd:minLength)
                     (node-eq predicate !xsd:maxLength)
                     (node-eq predicate !xsd:length)
                     (node-eq predicate !xsd:pattern)
                     (node-eq predicate !xsd:totalDigits)
                     (node-eq predicate !xsd:fractionDigits)
                     (node-eq predicate !swrl:argument2)
                     (node-eq predicate !swrl:argument1)
                     (list-item-p predicate))

           (let ((value (owl-as-literal object node-ht (role-range-spec predicate node-ht))))
             (cond ((annotation-property-node-p predicate node-ht)
                    (if (ontology-node-p subject node-ht)
                        (ensure-axiom axiom-ht subject predicate object
                                      (|OWLAPI-getOWLOntologyAnnotationAxiom| 
                                       ;; (owl-as-ontology subject node-ht)
                                       (list '|Annotation| 
                                             (owl-as-role predicate node-ht) 
                                             value)))
                      (progn
                        #|
                        (unless (only-annotation-property-p predicate node-ht)
                          (racer-warn "Property ~A is used in an ambiguous way. It is an annotation property as well as an object or datatype property. It is considered it as an annotation property." 
                                      (owl-as-role predicate node-ht)))
                        |#
                        (when (node-uri subject) ; if not, it is an axiom annotation axiom -> later! 
                          (let ((kinds (determine-declarations subject node-ht)))

                            (if (null (rest kinds))

                                (ensure-axiom axiom-ht subject predicate object
                                              (|OWLAPI-getOWLAnnotationAssertionAxiom|
                                               (owl-as-entity subject node-ht)
                                               (owl-as-role predicate node-ht) 
                                               value))

                              (ensure-axiom axiom-ht subject predicate object
                                            (|OWLAPI-getOWLEntityAnnotationAxiom| 
                                             `(,(if (null (rest kinds))
                                                    (first kinds)
                                                  kinds) 
                                               ,(owl-as-entity subject node-ht)) 
                                             `(|Annotation| ,(owl-as-role predicate node-ht) 
                                                            ,value)))))))))

                   ((datatype-property-node-p predicate node-ht)
                    (let ((subject1 (owl-as-individual subject node-ht))
                          (role (owl-as-role predicate node-ht)))
                      
                      (ensure-axiom axiom-ht subject1 role value
                                    (|OWLAPI-getOWLDataPropertyAssertionAxiom| subject1 
                                                                               role 
                                                                               value))))
                   ((let ((role-range (role-range-spec predicate node-ht)))
                      (and role-range (known-datatype-p role-range)))
                    (let ((subject1 (owl-as-individual subject node-ht))
                          (role (owl-as-role predicate node-ht)))

                      (ensure-axiom axiom-ht subject1 role value
                                    (|OWLAPI-getOWLDataPropertyAssertionAxiom| subject1 
                                                                               role
                                                                               value))))
                   (t #|(racer-warn "Propperty ~A is not declared -- to be filled with literal ~A. Triple (~A, ~A, ~A) ignored." 
                                  (owl-as-role predicate node-ht)
                                  value
                                  (owl-as-individual subject node-ht)
                                  (owl-as-role predicate node-ht)
                                  value)|#

                      (let ((subject1 (owl-as-individual subject node-ht))
                            (role (owl-as-role predicate node-ht)))
                        
                        (unless (or (node-eq predicate !owl:hasSelf)
                                    (node-eq predicate !owl:targetIndividual)
                                    (node-eq predicate !owl:targetValue)
                                    (not (node-uri subject)))

                        (ensure-axiom axiom-ht subject1 role value
                                      (|OWLAPI-getOWLDataPropertyAssertionAxiom| subject1
                                                                                 role 
                                                                                 value)))))))))
        
        ((and (node-p subject)
              (node-p object))

         (if (node-eq predicate !rdf:type)
             (if (null (node-uri subject))
                 (unless (or
                          (node-eq object !rdf:Description)
                          (node-eq object !rdf:nil)
                          (node-eq object !rdf:List)
                          (node-eq object !rdf:Seq)
                          
                          (node-eq object !owl:Class)
                          (node-eq object !owl:ObjectProperty)
                          (node-eq object !owl:DataProperty)
                          (node-eq object !owl:AnnotationProperty)
                          
                          (node-eq object !rdfs:Class)
                          (node-eq object !rdfs:Property)
                          (node-eq object !owl:ObjectProperty)
                          (node-eq object !owl:AnnotationProperty)
                          (node-eq object !owl:DatatypeProperty)
                          (node-eq object !owl:Restriction)
                          (node-eq object !owl2:DataRestriction)
                          (node-eq object !owl2:ObjectRestriction)
                          (node-eq object !owl:AllDifferent)
                          (node-eq object !owl:AllDisjointClasses)
                          (node-eq object !owl:AllDisjointProperties)
                          (node-eq object !owl:DataRange)
			  (node-eq object !rdfs:Datatype)
                          (node-eq object !owl2:Facet)
                          (node-eq object !owl:Ontology)
                          (node-eq object !owl2:SelfRestriction)
                          (node-eq object !swrl:AtomList)
                          (node-eq object !swrl:ClassAtom)
                          (node-eq object !swrl:IndividualPropertyAtom)
                          (node-eq object !swrl:DatavaluedPropertyAtom)
                          (node-eq object !swrl:DifferentIndividualAtom)
                          (node-eq object !swrl:DifferentIndividualsAtom)
                          (node-eq object !swrl:SameIndividualAtom)
                          (node-eq object !swrl:BuiltinAtom)
                          (node-eq object !swrl:DataRangeAtom)

                          (node-eq object !rdf:first)
                          (node-eq object !rdf:rest)
                          (member (owl-as-concept object node-ht) *user-defined-datatypes*))
                          
                   (let ((ind (owl-as-individual subject node-ht))
                         (concept (owl-as-concept object node-ht)))

                     (ensure-axiom axiom-ht ind predicate concept
                                   (|OWLAPI-getOWLClassAssertionAxiom| ind concept))))
               (progn 
                 (let ((ind (owl-as-individual subject node-ht))
                       (concept (owl-as-concept object node-ht)))     
                   (unless (or (node-eq object !rdf:List))

                     (ensure-axiom axiom-ht ind predicate concept
                                   (|OWLAPI-getOWLClassAssertionAxiom| ind concept))))))

           (if (or (node-eq predicate !owl:equivalentClass)
                   (node-eq predicate !owl:subClassOf)
                   (node-eq predicate !rdfs:subClassOf)
                   (node-eq predicate !owl:unionOf)
                   (node-eq predicate !owl:intersectionOf)
                   (node-eq predicate !owl:oneOf)
                   (node-eq predicate !owl:complementOf))
               (when (node-uri subject)
                 (when *tbox-verbose*
                   (racer-warn "Ignoring triple (~A, ~A, ~A), ~A is not a class. Assuming it is a user-defined datatype." 
                               subject predicate object (node-uri subject)))
                 (push (node-uri subject) *user-defined-datatypes*))
             (unless (or (node-eq predicate !rdf:first)
                         (node-eq predicate !rdf:rest)
                         (node-eq predicate !rdf:List)
                     
                         (node-eq predicate !owl:subject)
                         (node-eq predicate !owl:predicate)
                         (node-eq predicate !owl:object)
                         
                         (node-eq predicate !rdf:subject)
                         (node-eq predicate !rdf:predicate)
                         (node-eq predicate !rdf:object)
                     
                         (node-eq predicate !owl:propertyChain)
                              
                         (node-eq predicate !owl:annotatedProperty)
                         (node-eq predicate !owl:annotatedSource)
                         (node-eq predicate !owl:annotatedTarget)

                         (node-eq predicate !owl:sourceIndividual)
                         (node-eq predicate !owl:assertionProperty)
                         (node-eq predicate !owl:targetIndividual)
                         (node-eq predicate !owl:targetValue)

                         (node-eq predicate !owl:datatypeComplementOf)
                         (node-eq predicate !owl:onDataType)
                         (node-eq predicate !owl:onDatatype)

                         (node-eq predicate !owl:inverseOf)
                         (node-eq predicate !owl:oneOf) 
                         (node-eq predicate !owl:onProperty)
                         (node-eq predicate !owl:hasValue)
                         (node-eq predicate !owl2:onClass)
                         (node-eq predicate !owl:valuesFrom)
                         (node-eq predicate !owl2:onDataRange)
			 (node-eq predicate !owl2:onDatatype)
                         (node-eq predicate !owl:allValuesFrom)
                         (node-eq predicate !owl:someValuesFrom)
                         (node-eq predicate !owl2:withRestrictions)
                         (node-eq predicate !owl:distinctMembers)
                         (node-eq predicate !owl:members)
                         (node-eq predicate !owl:complementOf)
                         (node-eq predicate !swrl:argument2)
                         (node-eq predicate !swrl:argument1)
                         (node-eq predicate !swrl:arguments)
                         (node-eq predicate !swrl:propertyPredicate)
                         (node-eq predicate !swrl:classPredicate)
                         (node-eq predicate !swrl:builtin))
               (cond ((annotation-property-node-p predicate node-ht)
                      #|
                    (unless (only-annotation-property-p predicate node-ht)
                      (racer-warn "Property ~A is used in an ambiguous way. It is an annotation property as well as an object or datatype property. It is considered as an annotation property." (owl-as-role predicate node-ht)))
                    |#
                      (if (ontology-node-p subject node-ht)
                          (ensure-axiom axiom-ht subject predicate object
                                        (|OWLAPI-getOWLOntologyAnnotationAxiom| 
                                         (list '|Annotation| 
                                               (owl-as-role predicate node-ht) 
                                               (owl-as-ontology object node-ht))))
                        (let ((kinds (determine-declarations subject node-ht)))
                          
                          (if (null (rest kinds))
                              
                              (ensure-axiom axiom-ht subject predicate object
                                            (|OWLAPI-getOWLAnnotationAssertionAxiom|
                                             (owl-as-entity subject node-ht)
                                             (owl-as-role predicate node-ht) 
                                             (owl-as-individual object node-ht)))
                            
                            (ensure-axiom axiom-ht subject predicate object
                                          (|OWLAPI-getOWLEntityAnnotationAxiom|
                                           `(,(if (null (rest kinds))
                                                  (first kinds)
                                                kinds)
                                             ,(owl-as-entity subject node-ht))
                                           (list '|Annotation|
                                                 (owl-as-role predicate node-ht) 
                                                 (owl-as-individual object node-ht))))))))
                     
                     ((object-property-node-p predicate node-ht)
                      (let ((ind1 (owl-as-individual subject node-ht))
                            (role (owl-as-role predicate node-ht))
                            (ind2 (owl-as-individual object node-ht)))
                        (ensure-axiom axiom-ht ind1 role ind2
                                      (|OWLAPI-getOWLObjectPropertyAssertionAxiom| 
                                       ind1
                                       role 
                                       ind2))))
                     ((datatype-property-node-p predicate node-ht)
                      (let ((ind1 (owl-as-individual subject node-ht))
                            (role (owl-as-role predicate node-ht))
                            (ind2 (owl-as-individual object node-ht)))
                        (racer-warn "Property ~A declared as datatype property -- used as object property."
                                    role)
                        (ensure-axiom axiom-ht ind1 role ind2
                                      (|OWLAPI-getOWLObjectPropertyAssertionAxiom| 
                                       ind1 
                                       role
                                       ind2))))
                     (t (racer-warn "Property ~A is not declared -- treated as object property."
                                    (owl-as-role predicate node-ht))
                        (let ((ind1 (owl-as-individual subject node-ht))
                              (role (owl-as-role predicate node-ht))
                              (ind2 (owl-as-individual object node-ht)))
                          (ensure-axiom axiom-ht ind1 role ind2
                                        (|OWLAPI-getOWLObjectPropertyAssertionAxiom| 
                                         ind1 
                                         role
                                         ind2)))))))))
        
        (t
         (when *tbox-verbose*
           (racer-warn "Ignoring triple (~A, ~A, ~A)" subject predicate object)))))


(defun determine-declarations (subject node-ht)
  (let ((types
         (remove-duplicates
          (loop for (predicate value) in (get-attributes subject node-ht)
                when (and (node-eq predicate !rdf:type) (not (node-eq value !owl:Ontology)))
                collect (cond ((node-eq value !owl:Class) '|Class|)
                              ((node-eq value !owl:ObjectProperty) '|ObjectProperty|)
                              ((node-eq value !owl:DataProperty) '|DataProperty|)
                              ((node-eq value !owl:DatatypeProperty) '|DataProperty|)
                              ((node-eq value !rdfs:Datatype) '|Datatype|)
                              (t '|NamedIndividual|))))))
    (if (and (member '|NamedIndividual| types)
             (cdr types))
        (remove '|NamedIndividual| types)
      types)))             
             

#|

(defun process-axiom (subject predicate object
                              node-ht axiom-ht) 
  (let* ((attributes (get-attributes subject node-ht))
         (ref-predicate
          (or 
           (get-attribute-value !rdf:predicate attributes)
           (get-attribute-value !owl:predicate attributes)
           (get-attribute-value !owl:annotatedPredicate attributes))))

    (multiple-value-bind (ref-subject ignored-p)
        (let ((*tbox-verbose* nil))
          (owl-as-concept
           (or 
            (get-attribute-value !rdf:subject attributes)
            (get-attribute-value !owl:subject attributes)
            (get-attribute-value !owl:annotatedSource attributes))
           node-ht))

      (when ignored-p 
        (setf ref-subject 
              (owl-as-role
               (or 
                (get-attribute-value !rdf:subject attributes)
                (get-attribute-value !owl:subject attributes)
                (get-attribute-value !owl:annotatedSource attributes))
               node-ht)))

      (multiple-value-bind (ref-object ignored-p)
          (let ((*tbox-verbose* nil))
            (owl-as-concept
             (or 
              (get-attribute-value !rdf:object attributes)
              (get-attribute-value !owl:object attributes)
              (get-attribute-value !owl:annotatedTarget attributes))
             node-ht))

        (when ignored-p 
          (setf ref-object 
                (owl-as-role
                 (or 
                  (get-attribute-value !rdf:object attributes)
                  (get-attribute-value !owl:object attributes)
                  (get-attribute-value !owl:annotatedTarget attributes))
                 node-ht)))

        (loop for (property value) in attributes
              when (annotation-property-node-p property node-ht)
              do 
              (let ((axiom (find-axiom (list ref-subject ref-predicate ref-object) axiom-ht)))
                (if (not axiom)
                    (nrql-warning "Cannot find axiom, ignoring axiom annotation")
                  (ensure-axiom axiom-ht subject predicate object
                                (|OWLAPI-getOWLAxiomAnnotationAxiom| 
                                 axiom
                                 (list '|Annotation| 
                                       (owl-as-role property node-ht) 
                                       (if (literal-p value)
                                           `(d-literal ,(literal-string value) pe
                                                       ,(and (literal-datatype value)
                                                             (owl-as-datatype (literal-datatype value) node-ht)))
                                         (owl-as-individual value node-ht))))))))))))

|#

(defun process-axiom-annotations (axiom attributes node-ht)
  (loop for (property value) in attributes
        when (annotation-property-node-p property node-ht)
        do 
        (|OWLAPI-getOWLAxiomAnnotationAxiom| 
         axiom
         (list '|Annotation| 
               (owl-as-role property node-ht) 
               (if (literal-p value)
                   `(d-literal ,(literal-string value) 
                               ,(and (literal-datatype value)
                                     (owl-as-datatype (literal-datatype value) node-ht)))
                 (owl-as-individual value node-ht))))))
          

(defun process-axiom1 (subject predicate object 
                               tbox
                               abox
                               node-ht
                               axiom-ht
                               rule-ht
                               var-ht)

  (declare (ignore object predicate))

  (let* ((attributes (get-attributes subject node-ht))
         (ref-predicate
          (or 
           (get-attribute-value !rdf:predicate attributes)
           (get-attribute-value !owl:predicate attributes)
           (get-attribute-value !owl:annotatedProperty attributes)))
         
         (ref-subject
          (or (get-attribute-value !rdf:subject attributes)
              (get-attribute-value !owl:subject attributes)
              (get-attribute-value !owl:annotatedSource attributes)))

         (ref-object
          (or (get-attribute-value !rdf:object attributes)
              (get-attribute-value !owl:object attributes)
              (get-attribute-value !owl:annotatedTarget attributes))))

    (let ((id 
           (owlapi:with-lookup-mode ()
             (process-owl-triple ref-subject
                                 ref-predicate
                                 ref-object
                                 tbox
                                 abox
                                 node-ht
                                 axiom-ht
                                 rule-ht
                                 var-ht))))

      (cond ((not (numberp id))
             (nrql-warning "Cannot find axiom, ignoring axiom annotation"))

            (t
             
             (loop for (property value) in attributes
                   ;; when (annotation-property-node-p property node-ht)
                   ;; too strong ? 
                
                   unless (member property 
                                  (list !owl:annotatedProperty 
                                        !owl:annotatedSource 
                                        !owl:annotatedTarget

                                        !rdf:predicate 
                                        !owl:predicate 
                                        
                                        !rdf:subject 
                                        !owl:subject
                                        
                                        !rdf:object 
                                        !owl:object

                                        !rdf:type)

                                  :test #'node-eq)
                   do 
                   
                   (|OWLAPI-getOWLAxiomAnnotationAxiom| 
                    id
                    (list '|Annotation| 
                          (owl-as-role property node-ht) 
                          (if (literal-p value)
                              `(d-literal ,(literal-string value) 
                                          ,(and (literal-datatype value)
                                                (owl-as-datatype (literal-datatype value) node-ht)))
                            (owl-as-individual value node-ht))))))))))
          

;;; ======================================================================

(defun rdfs-read-tbox-file (filename)
  (let ((db (parse-db-from-file filename (nox:make-file-url 
                                          (translate-logical-pathname filename))))
        (tbox (find-tbox 
               (in-tbox-internal (intern (string-upcase (pathname-name filename)))
                                 t
                                 nil
                                 *default-tbox-concept-size* 
                                 *default-tbox-role-size*))))
    (loop for triple in (reverse (wilbur:db-triples db)) do
          (let ((subject (wilbur:node-uri (wilbur:triple-subject triple)))
                (property (wilbur:triple-predicate triple))
                (object (if (stringp (wilbur:triple-object triple))
                            (wilbur:triple-object triple)
                          (wilbur:node-uri (wilbur:triple-object triple)))))
            (cond ((eq property !rdf:type)
                   (cond ((eq (wilbur:triple-object triple) !rdfs:Class)
                          (xml-define-concept (intern subject) tbox))
                         ((eq (wilbur:triple-object triple) !rdf:Property)
                          (ensure-role (intern subject) tbox))
                         (*tbox-verbose* (racer-warn "Ignoring triple (~A, ~A, ~A)" 
                                                     subject (wilbur:node-uri property) object))))
                  ((eq property !rdfs:subClassOf)
                   (xml-add-implication (intern subject)
                                        (intern object) tbox))
                  ((eq property !rdfs:range)
                   (xml-add-implication 'top
                                        `(all ,(intern subject)
                                              ,(intern object))
                                        tbox))
                  ((eq property !rdfs:domain)
                   (xml-add-implication `(some ,(intern subject)
                                               top)
                                        (intern object)
                                        tbox))
                  (*tbox-verbose* (racer-warn "Ignoring triple (~A, ~A, ~A)" subject (wilbur:node-uri property) object)))))
    (tbox-name tbox)))

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-reader-macros))


