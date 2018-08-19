;;; -*- package: WILBUR-RACER; Syntax: Common-lisp; Base: 10 -*-

;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   The contents of this file are subject to the NOKOS License Version 1.0a (the
;;;   "License"); you may not use this file except in compliance with the License. 
;;;
;;;   Software distributed under the License is distributed on an "AS IS" basis, WITHOUT
;;;   WARRANTY OF ANY KIND, either express or implied. See the License for the specific
;;;   language governing rights and limitations under the License. 
;;;
;;;   The Original Software is 
;;;     WILBUR2: Nokia Semantic Web Toolkit for CLOS
;;;
;;;   Copyright (c) 2001-2005 Nokia and others. All Rights Reserved.
;;;   Portions Copyright (c) 1989-1992 Ora Lassila. All Rights Reserved.
;;;
;;;   Contributor(s): Ora Lassila (mailto:ora.lassila@nokia.com)
;;;                   Louis Theran <theran@pobox.com>
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   Purpose: This file contains an implementation of an RDF parser, using a
;;;   "near streaming" algorithm based on a simple state machine. The parser
;;;   implements all of RDF M+S excluding "aboutEachPrefix" (what, are you
;;;   surprised?) as well as RDFCore.
;;;


(in-package :wilbur-racer)


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS RDF-SYNTAX-NORMALIZER
;;;

(defclass rdf-syntax-normalizer (nox-racer:sax-filter)
  ())

(defmethod nox-racer:sax-consumer-mode ((self rdf-syntax-normalizer))
  (nox-racer:sax-consumer-mode (nox-racer:sax-producer-consumer self)))

(defmethod nox-racer:start-element ((self rdf-syntax-normalizer)
                              (tag nox-racer:open-tag)
                              mode)
  (let ((attributes (nox-racer:tag-attributes tag))
        (properties nil)
        (consumer (nox-racer:sax-producer-consumer self))
        (namespaces (nox-racer:tag-namespaces tag)))
    (nox-racer:do-string-dict (key value attributes)
      (cond ((null (find key -rdf-attrs- :test #'string=))
             (setf properties (nox-racer:string-dict-add properties key value)
                   attributes (nox-racer:string-dict-del attributes key)))
            ((string= key -rdf-abouteachprefix-uri-)
             (cerror "Ignore" 'feature-not-supported :thing "aboutEachPrefix")
             (setf attributes (nox-racer:string-dict-del attributes key)))))
    (setf (nox-racer:tag-attributes tag) attributes)
    (nox-racer:start-element consumer tag mode)
    (nox-racer:do-string-dict (key value properties)
      (unless (or (string-equal key -xml-lang-attr-)
                  (string-equal key "xml:space")
                  (string-equal key "xml:base"))
        (let ((new-tag (make-instance 'nox-racer:open-tag
                         :string key
                         :base (nox-racer::tag-base tag)
                         :namespaces namespaces))
              (close-tag (make-instance 'nox-racer:close-tag
                           :string key)))
          (setf (nox-racer:tag-counterpart new-tag) close-tag)
          (setf (nox-racer:tag-counterpart close-tag) new-tag)
          (nox-racer:start-element consumer new-tag (nox-racer:sax-consumer-mode self))
          (nox-racer:char-content consumer value (nox-racer:sax-consumer-mode self))
          (nox-racer:end-element consumer new-tag (nox-racer:sax-consumer-mode self)))))))

(defmethod nox-racer:maybe-use-namespace ((self rdf-syntax-normalizer) prefix uri)
  (nox-racer:maybe-use-namespace (nox-racer:sax-producer-consumer self) prefix uri))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CLASS RDF-PARSER
;;;

(defclass rdf-parser (nox-racer:sax-consumer)
  ((base
    :initform nil
    :accessor parser-base)
   (db
    :initarg :db
    :initform (make-instance 'indexed-db :emptyp t)
    :reader parser-db)
   (states
    :initform nil
    :accessor parser-states)
   (literal
    :accessor parser-literal)
   (rdfcorep 
    :initarg rdfcorep
    :initform t
    :accessor parser-rdfcore-p)
   (harvest-namespaces-p
    :initarg :harvest-namespaces-p
    :initform t
    :reader parser-harvest-namespaces-p)
   (initial-state
    :initarg :initial-state
    :initform :scan
    :reader parser-initial-state))
  (:default-initargs
    :producer (make-instance 'nox-racer:xml-parser
			     :consumer (make-instance 'rdf-syntax-normalizer))))

(define-condition close-rdf-element (condition)
  ())

(defstruct (state (:constructor make-state (mode
                                            &optional node
                                                      property
                                                      statement-id
                                                      language
                                                      datatype)))
  mode
  node
  property
  (statement-id nil)
  (language nil)
  (datatype nil)
  (index 0)
  task-queue)

#+:junk
(defmethod print-object ((state state) stream)
  (print-unreadable-object (state stream :type t)
    (format stream "~A ~S/~S/~A"
            (let ((mode (state-mode state)))
              (case mode
                ((:description :property :scan :literal)
                 (char (string mode) 0))
                (t mode)))
            (state-node state)
            (state-property state)
            (state-statement-id state))))

(defmethod add-state ((parser rdf-parser) mode &rest args)
  (declare (dynamic-extent args))
  (push (apply #'make-state mode args) (parser-states parser)))

(defmethod parser-task-state ((parser rdf-parser))
  (find :description (parser-states parser) :key #'state-mode))

(defmethod nox-racer:sax-consumer-mode ((parser rdf-parser))
  (state-mode (first (parser-states parser))))

(defstruct (task (:constructor make-task (type node &rest parameters)))
  type
  node
  parameters)

(defmacro task-parameter (task parameter)
  `(getf (task-parameters ,task) ,parameter))

(defmethod defer-task ((parser rdf-parser) type node &rest args)
  (declare (dynamic-extent args))
  (pushnew (apply #'make-task type node args)
           (state-task-queue (parser-task-state parser))
           :test #'(lambda (p q)
                     (and (eq (task-type p) (task-type q))
                          (eq (task-node p) (task-node q))))))

(defmethod make-container ((parser rdf-parser)
                           elements
                           &optional container-uri
                                     (container-type-uri -rdf-bag-uri-))
  (let ((node (ensure-node parser container-uri nil))
	(i 0))
    (add-as-triple parser node
		   (ensure-node parser -rdf-type-uri- t)
		   (ensure-node parser container-type-uri t))
    (dolist (element elements)
      (add-as-triple parser node (index-uri (incf i)) element))
    node))

(defmethod initialize-instance :after ((parser rdf-parser) &key &allow-other-keys)
  (let ((normalizer (nox-racer:sax-producer-consumer (nox-racer:sax-consumer-producer parser))))
    (setf (nox-racer:sax-producer-consumer normalizer) parser)))

(defmethod uri ((parser rdf-parser) uri should-exist-p)
  (let ((base (first (parser-base parser))))
    (cond ((or (null uri) (position #\: uri))
           uri)
          ((string= uri "")
           (or base ""))
          ((string= base "")
           (if (char= (char uri 0) #\#)
             (subseq uri 1)
             uri))
          ((char= (char uri 0) #\#) (char= (char uri 0) #\#)
           (let* ((length-base (length base))
                  (base-ends-in-hash-p (char= (char base (1- length-base)) #\#)))
             (concatenate 'string base (if base-ends-in-hash-p (subseq uri 1) uri))))
          ((and (not (position #\: uri))   ; relative uri with a slash not necessarily at the beginning
                (position #\/ uri))        ; --> merging required.
           (let ((base-protocol-end (position #\: base :test #'char=)))
             (if base-protocol-end
               (if (and (> (length uri) 1)
                        (char= (char uri 1) #\/))
                 (concatenate 'string (subseq base (1+ base-protocol-end)) uri)
                 (let ((domain-end (position #\/ base :from-end t :start (+ 3 base-protocol-end))))
                   (if domain-end
                     (if (char= (char uri 0) #\/)
                         (concatenate 'string (subseq base 0 domain-end) uri)
                       (concatenate 'string (subseq base 0 domain-end) "/" uri))
                     (concatenate 'string base "/" uri))))
               uri)))
          ((null should-exist-p) 
           (let* ((length-base (length base))
                  (base-ends-in-hash-p (char= (char base (1- length-base)) #\#)))
             (if base-ends-in-hash-p 
               (concatenate 'string base uri)
               (concatenate 'string base "#" uri))))
          (t
           (let* ((length-base (length base))
                  (last-ch (char base (1- length-base)))
                  (base-ends-in-slash-p (and last-ch (char= last-ch  #\/)))
                  (base-ends-in-hash-p (and last-ch (char= last-ch #\#))))
             (cond (base-ends-in-hash-p
                    (concatenate 'string base uri))
                   (base-ends-in-slash-p 
                    (concatenate 'string base uri))
                   (t (concatenate 'string base "#" uri))))))))

(defmethod ensure-node ((parser rdf-parser) uri should-exist-p)
  (if (and (stringp uri) (zerop (length uri)))
    (node (first (parser-base parser))) ; should it be something that does not change?
    (node (uri parser uri should-exist-p))))

(defmethod nox-racer:parse ((parser rdf-parser) stream locator)
  (catch :terminate-rdf-parser
    (nox-racer:parse (nox-racer:find-first-producer parser) stream locator))
  (node (first (parser-base parser))))

(defmethod add-as-triple ((parser rdf-parser)
                          (subject node)
                          (predicate string)
                          object
                          &optional statement-id)
  (add-as-triple parser subject (ensure-node parser predicate t) object statement-id))

(defmethod add-as-triple ((parser rdf-parser)
                          (subject node)
                          (predicate node)
                          object
                          &optional statement-id)
  (let* ((db (parser-db parser))
         (source (node (first (parser-base parser))))
	 (triple (db-make-triple db subject predicate object source)))
    (db-add-triple db triple)
    (dolist (state (parser-states parser))
      ;; for higher order statements
      (dolist (task (state-task-queue state))
        (when (and (eq (task-type task) :bagid)
                   (eq subject (task-node task)))
          (push (cons triple statement-id) (task-parameter task :statements))
          (return-from add-as-triple triple))))
    (when statement-id
      ;; no bagid but statement-id exists
      (db-reify triple db (uri parser statement-id nil) source))
    triple))

(defun new-index-uri (parser)
  (index-uri (incf (state-index (first (parser-states parser))))))

(defun parse-db-from-stream (stream locator
                                    &rest options
                                    &key (parser-class 'rdf-parser)
                                    &allow-other-keys)
  (declare (dynamic-extent options))
  (remf options :parser-class)
  (multiple-value-bind (source-node parser)
        (apply #'nox-racer:parse-from-stream
               stream locator parser-class options)
      (values (parser-db parser) source-node)))

(defmethod nox-racer:maybe-use-namespace ((self rdf-parser) prefix uri)
  (when (and (parser-harvest-namespaces-p self)
             (not (nox-racer:string-dict-get (dictionary-namespaces *nodes*) prefix)))
    (add-namespace prefix uri)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   RDF PARSER STATE MACHINE TRANSITIONS
;;;

(defmethod nox-racer:start-document ((parser rdf-parser)
                               locator)
  (setf (parser-base parser) (list locator))
  (add-state parser (parser-initial-state parser)))

(defmethod nox-racer:end-document ((parser rdf-parser)
                             mode)
  (declare (ignore mode))
  nil)

(defmethod nox-racer:start-element :before ((parser rdf-parser)
                                      (tag nox-racer:open-tag)
                                      mode)
  (declare (ignore mode))
  (when (nox-racer::tag-base tag)
    (push (nox-racer::tag-base tag) (parser-base parser))))

(defmethod nox-racer:end-element :after ((parser rdf-parser)
                                   (tag nox-racer:open-tag)
                                   mode)
  (declare (ignore mode))
  (when (nox-racer::tag-base tag)
    (pop (parser-base parser))))

(defmethod nox-racer:start-element ((parser rdf-parser)
                              (tag nox-racer:open-tag)
                              (mode (eql :scan)))
  (cond ((string= (nox-racer:token-string tag) -rdf-rdf-uri-)
         (add-state parser :description))
        ((string= (nox-racer:token-string tag) -rdf-description-uri-)
         (nox-racer:start-element parser tag :description))))

(defmethod nox-racer:start-element ((parser rdf-parser)
                              (tag nox-racer:open-tag)
                              (mode (eql :description)))
  (let* ((each  (nox-racer:tag-attribute tag -rdf-abouteach-uri-))
         (about (nox-racer:tag-attribute tag -rdf-about-uri-))
         (id    (nox-racer:tag-attribute tag -rdf-id-uri-))
         (node-id (nox-racer:tag-attribute tag -rdf-nodeid-uri-))
         (type-uri (nox-racer:tag-attribute tag -rdf-type-uri-))  ; added, rm: see below
         (type  (nox-racer:token-string tag))
         (node  (ensure-node parser
                             (and (null each) (or about id node-id))
                             (and (null each) (null id)))))
    ;;(when node-id
    ;;  (error "Racer cannot handle rdf:nodeID."))
    (when node-id
      (setf (node-name-resolved-p node) :node-id))
    (when (and about id)
      (cerror "Use \"about\"" 'about-and-id-both-present))
    ;; Types might be implicitly declared with an attribute.
    (when type-uri
      (let ((type-node (ensure-node parser type-uri t)))
        (add-as-triple parser node -rdf-type-uri- type-node)))
    (if each
      (if (parser-rdfcore-p parser)
        (cerror "Ignore \"aboutEach\"" 'feature-disabled :feature "aboutEach")
        (defer-task parser :abouteach node :target (ensure-node parser each t)))
      (let* ((bagid (nox-racer:tag-attribute tag -rdf-bagid-uri-))
             (state (first (parser-states parser)))
             (parent (state-node state)))
        (when bagid
          (defer-task parser :bagid node :bagid bagid :statements nil))
        (when parent
          (attach-to-parent parser parent node (state-statement-id state)))))
    (unless (string= type -rdf-description-uri-)
      (add-as-triple parser node -rdf-type-uri- (ensure-node parser type t)))
    (add-state parser :property node)))

(defmethod attach-to-parent ((parser rdf-parser)
                             (parent node)
                             (child node)
                             &optional statement-id)
  (let ((state (first (parser-states parser))))
    (cond ((not (eq (state-mode state) :collection))
           (add-as-triple parser parent
                          (state-property (first (parser-states parser)))
                          child statement-id))
          (t
           (setf parent (state-node state))
           (add-as-triple parser parent -rdf-first-uri- child)
           (add-as-triple parser parent -rdf-rest-uri-
                          (setf (state-node state) (ensure-node parser nil t)))))))
    
(defmethod nox-racer:start-element ((parser rdf-parser)
                              (tag nox-racer:open-tag)
                              (mode (eql :property)))
  (let* ((state (first (parser-states parser)))
         (node (state-node state))
         (property-uri (nox-racer:token-string tag))
         (property (ensure-node parser
                                (cond ((string= property-uri -rdf-li-uri-)
                                       ;; (defer-task parser :container node)
                                       (new-index-uri parser))
                                      (t
                                       property-uri))
                                t))
         (resource-uri (nox-racer:tag-attribute tag -rdf-resource-uri-))
         (node-id (nox-racer:tag-attribute tag -rdf-nodeid-uri-))
         (statement-id (nox-racer:tag-attribute tag -rdf-id-uri-)))
    ;;(when node-id
    ;;  (error "Racer cannot handle rdf:nodeID."))
    (if (or resource-uri node-id)
      (let ((value (ensure-node parser (or resource-uri node-id) t)))
        (setf (state-property state) property)
        (attach-to-parent parser node value statement-id)
        (add-state parser :property value))
      (parse-using-parsetype parser node property
                             (nox-racer:tag-attribute tag -rdf-parsetype-uri-)
                             statement-id
                             (nox-racer:tag-attribute tag -xml-lang-attr-)
                             (nox-racer:tag-attribute tag -rdf-datatype-uri-)))))

(defmethod parse-using-parsetype ((parser rdf-parser) node property parsetype
                                  &optional statement-id language datatype)
  (cond ((null parsetype)
         (add-state parser :description node property statement-id language datatype))
        ((string= parsetype "Literal")
         (setf (parser-literal parser) nil)
         (add-state parser :literal node property))
        ((string= parsetype "Resource")
         (add-as-triple parser
                        node property (setf node (ensure-node parser nil t))
                        statement-id)
         (add-state parser :property node))
        ((string= parsetype "Collection")       ; adapted from daml-parser
         (let ((list-node (ensure-node parser nil t)))
           (add-as-triple parser node property list-node)
           (add-state parser :collection list-node)))
        ((search "ollection" parsetype)       ; for some stupid examples that use daml:collection
         (warn "Illegal parsetype ~S -- treated as Collection" parsetype)
         (let ((list-node (ensure-node parser nil t)))
           (add-as-triple parser node property list-node)
           (add-state parser :collection list-node)))
        (t
         ;;(cerror "Ignore parseType"  "Unknown parseType ~S" parsetype)
         (warn "Unknown parseType: ~S -- treated as Literal" parsetype)
         (setf (parser-literal parser) nil)
         (add-state parser :literal node property)
        )))
  
(defmethod nox-racer:start-element ((parser rdf-parser)
                              (tag nox-racer:open-tag)
                              (mode (eql :literal)))
  (add-state parser :literal)
  (push tag (parser-literal parser)))

(declaim (special *db*))

(defmethod nox-racer:end-element ((parser rdf-parser)
                            (tag nox-racer:open-tag)
                            (mode (eql :literal)))
  (let ((state (first (parser-states parser))))
    (call-next-method)
    (cond ((not (null (state-node state)))
	   (let ((string (with-output-to-string (s)
			   (nox-racer:replay (make-instance 'nox-racer:xml-formatter :stream s)
				       (nreverse (parser-literal parser))))))
	     (add-as-triple parser
			    (state-node state)
			    (state-property state)
			    (db-make-literal (or ;*db*
                                              (parser-db parser)) string))))
	  ((not (nox-racer:tag-empty-p tag))
           (if (nox-racer:tag-counterpart tag)
             (push (nox-racer:tag-counterpart tag) (parser-literal parser))
	     (break "No tag counterpart found."))))))

(defmethod nox-racer:end-element ((parser rdf-parser)
                            (tag nox-racer:open-tag)
                            (mode (eql :scan)))
  nil)

(defmethod nox-racer:end-element :after ((parser rdf-parser)
                                   (tag nox-racer:open-tag)
                                   (mode (eql :property)))
  (let ((state (parser-task-state parser)))
    (when state
      (dolist (task (shiftf (state-task-queue state) nil))
	(execute-deferred-task parser task (task-type task))))))

(defmethod execute-deferred-task ((parser rdf-parser) task type)
  (let ((db (parser-db parser))
	(source (node (first (parser-base parser)))))
    (ecase type
      ;; (:container
      ;;  (is-container-p db (task-node task) t)
      ;;  t)
      (:abouteach
       (let ((target (task-parameter task :target))
	     (index-predicates nil))
	 (is-container-p db target t)
	 (dolist (triple (db-query db target nil nil))
	   (let ((uri (node-uri (triple-predicate triple))))
	     (when (find uri *index-uris* :test #'string=)
	       (push (ensure-node parser uri t) index-predicates))))
	 (dolist (triple (db-query db (task-node task) nil nil))
	   (db-del-triple db triple)
	   (dolist (p index-predicates)
	     (add-as-triple parser
			    (triple-object (first (db-query db target p nil)))
			    (triple-predicate triple)
			    (triple-object triple)
			    source)))))
      (:bagid
       (let ((statements (task-parameter task :statements)))
	 (when statements
	   (make-container parser
			   (mapcar #'(lambda (s)
				       (destructuring-bind (triple . id) s
					 (db-reify triple db
						   (and id (uri parser id nil))
						   source)))
				   statements)
			   (uri parser (task-parameter task :bagid) nil))))))))


;;; Wilbur should not emit arbitrary signals because this causes trouble with handlers
#|
(defmethod nox-racer:end-element :after ((parser rdf-parser)
                                   (tag nox-racer:open-tag)
                                   (mode (eql :description)))
  (when (string= (nox-racer:token-string tag) -rdf-rdf-uri-)
    (signal 'close-rdf-element)))
|#

(defmethod nox-racer:end-element ((parser rdf-parser)
                            (tag nox-racer:open-tag)
                            mode)
  (declare (ignore mode))
  (pop (parser-states parser)))

(defmethod nox-racer:char-content ((parser rdf-parser)
                             (content string)
                             (mode (eql :description)))
  (let* ((state (first (parser-states parser)))
         (datatype (state-datatype state)))
    (add-as-triple parser
                   (state-node state)
                   (state-property state)
                   (db-make-literal (or 
                                     ;*db*
                                     (parser-db parser)) content
				    :language (state-language state)
				    :datatype (and datatype (node datatype)))
                   (state-statement-id state))))

(defmethod nox-racer:char-content ((parser rdf-parser)
                             (content string)
                             (mode (eql :literal)))
  (push content (parser-literal parser)))

(defmethod nox-racer:char-content ((parser rdf-parser)
                             (content string)
                             (mode (eql :scan)))
  ;; ignore character content in :scan mode
  nil)

(defmethod nox-racer:char-content ((parser rdf-parser)
                             (content string)
                             mode)
  (declare (ignore mode))
  (cerror "Ignore" 'illegal-character-content :thing content))

(defmethod nox-racer:start-element ((parser rdf-parser)
                              (tag nox-racer:open-tag)
                              (mode (eql :collection)))
  (nox-racer:start-element parser tag :description))

(defmethod nox-racer:end-element :before ((parser rdf-parser)
                                    (tag nox-racer:open-tag)
                                    (mode (eql :collection)))
  (let* ((db (parser-db parser))
         (node (state-node (first (parser-states parser))))
         (triple (db-del-triple db (first (db-query db nil nil node)))))
    (add-as-triple parser
                   (triple-subject triple)
                   (triple-predicate triple)
                   (ensure-node parser -rdf-nil-uri- t))))
