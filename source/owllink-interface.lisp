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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (racer:enable-boolean-readers)
  (wilbur:enable-node-shorthand))

(defparameter *public-kbs* nil)
(defparameter *kbs* (make-hash-table))

(defparameter *owllink-unique-name-assumption* nil)
(defparameter *owllink-ignore-annotations* nil)
(defparameter *owllink-lean-mode* nil)
(defparameter *owllink-verbose* nil)
(defparameter *release-kb-deletes-kb* t)

(defparameter *owllink-major* "1")
(defparameter *owllink-minor* "9")
(defparameter *owllink-build* "3")

(defparameter *silent* nil)

;;; ======================================================================

(defmacro pop-stack (expr)
  `(progn #+:debug (when (null ,expr) (error "Stack underflow."))
     (pop ,expr)))

(defmacro push-stack (expr stack)
  `(progn ;(break (format nil "~A" ,expr))
     (push ,expr ,stack)))


;;; ======================================================================

(define-condition kb-error (error) 
                   ((argument :reader kb-error-argument :initarg :argument))
   (:report (lambda (condition stream)
              (format stream "~A"
                      (kb-error-argument condition)))))

(define-condition syntax-error (error) 
                   ((argument :reader syntax-error-argument :initarg :argument))
   (:report (lambda (condition stream)
              (format stream "~A"
                      (syntax-error-argument condition)))))

(define-condition semantic-error (error) 
                   ((argument :reader semantic-error-argument :initarg :argument))
   (:report (lambda (condition stream)
              (format stream "~A"
                      (semantic-error-argument condition)))))

;;; ======================================================================

(defstruct (kb-descriptor (:constructor make-kb-descriptor (name)))
  name
  (unique-name-assumption *owllink-unique-name-assumption*)
  (ignore-annotations *owllink-ignore-annotations*)
  (verbose *owllink-verbose*))

(defun find-kb-descriptor (name &optional (error-p t))
  (or (gethash name *kbs*)
      (and error-p (error 'kb-error :argument (format nil "KB '~A' unknown." name)))))

(defun (setf find-kb-descriptor) (new-value name)
  (if (null new-value)
      (remhash name *kbs*)
    (setf (gethash name *kbs*) new-value)))

;;; ======================================================================

(defun update-tbox (tbox)
  (setf tbox (find-tbox tbox))
  (let ((use-less-tbox-memory (if (boundp '*use-less-tbox-memory*)
                                  *use-less-tbox-memory*
                                (tbox-use-less-memory tbox))))
    (when (or (tbox-classified-p-internal tbox) 
              (tbox-coherence-checked-p tbox)
              (tbox-index-structures-complete-p tbox))
      (when use-less-tbox-memory
	(racer-warn "~&Updating of Tbox ~A only partially supported due to the ~
                   current memory management strategy. Start Racer with option -x to ~
                   enable these services.~%"
		    (tbox-name tbox))
	(return-from update-tbox))
      (reset-tbox tbox))))

(defun update-abox (abox)
  (setf abox (find-abox abox))
  (when (abox-index-structures-complete-p abox)
    (initialize-abox abox)))

(defun change-settings (literal key kb)
  (cond ((string= key "uniqueNameAssumption")
         (if kb
             (unless (eq (kb-descriptor-unique-name-assumption (find-kb-descriptor kb))
                         (ol-literal-value literal))
               (update-abox kb)
               (setf (kb-descriptor-unique-name-assumption (find-kb-descriptor kb))
                     (ol-literal-value literal)))
           (setf *owllink-unique-name-assumption* (ol-literal-value literal))))
        ((string= key "ignoresAnnotations")
         (if kb
             (unless (eq (kb-descriptor-ignore-annotations (find-kb-descriptor kb))
                         (ol-literal-value literal))
               (setf (kb-descriptor-ignore-annotations (find-kb-descriptor kb))
                     (ol-literal-value literal)))
           (setf *owllink-ignore-annotations* (ol-literal-value literal))))
        ((string= key "releaseKBDeletesKB")
         (if kb
             (error 'kb-error :argument "ReleaseKBDeletesKB can only be set globally -- no kb attribute possible.")
           (setf *release-kb-deletes-kb* (ol-literal-value literal))))
        ((string= key "leanMode")
         (if kb
             (error 'kb-error :argument "Lean mode can only be set globally -- no kb attribute possible.")
           (setf *owllink-lean-mode* (ol-literal-value literal))))
        ((string= key "verboseMode")
         (if kb
             (unless (eq (kb-descriptor-verbose (find-kb-descriptor kb))
                         (ol-literal-value literal))
               (setf (kb-descriptor-verbose (find-kb-descriptor kb))
                     (ol-literal-value literal)))
           (setf *owllink-verbose* (ol-literal-value literal))))
        (t (error 'syntax-error :argument (format nil "Cannot change settings for key ~A." key)))))

(defun ol-literal-value (literal)
  (cond ((string= (second literal) "true")
         t)
        ((string= (second literal) "false")
         nil)
        (t (error 'syntax-error :argument (format nil "Unknown literal value ~A." (second literal))))))

;;; ======================================================================

(defmacro with-kb-settings ((kb) &body forms)
  (let ((kb-descriptor-sym (gensym "KB-Desc")))
    `(let* ((,kb-descriptor-sym (find-kb-descriptor ,kb))
            (*use-unique-name-assumption* (kb-descriptor-unique-name-assumption ,kb-descriptor-sym))
            (*use-less-tbox-memory* *owllink-lean-mode*)
            (*use-less-abox-memory* *use-less-tbox-memory*)
            (*tbox-verbose* (kb-descriptor-verbose ,kb-descriptor-sym))
            (*abox-verbose* (kb-descriptor-verbose ,kb-descriptor-sym)))
       ., forms)))



;;; ======================================================================


(defun get-warnings (&optional kb)
  (if (if kb
          (kb-descriptor-verbose (find-kb-descriptor kb))
        *owllink-verbose*)
    (get-output-stream-string *standard-output*)
    ""))



;;; ======================================================================

(defclass dig2-racer 
          (dig-racer)
  ((ignore-import :initform nil :accessor ignore-import :initarg :ignore-import)
   (import-meta-ontologies :initform nil 
                           :accessor import-meta-ontologies
                           :initarg :import-meta-ontologies)
   (excluded-meta-ontologies :initform nil
                             :accessor excluded-meta-ontologies
                             :initarg :excluded-meta-ontologies)
   (maintain-owlapi-axioms :initform (not *owllink-lean-mode*)
                           :accessor maintain-owlapi-axioms
                           :initarg :maintain-owlapi-axioms)
   (recursive :initform nil 
              :accessor recursive 
              :initarg :recursive)))

(defclass owllink-parser
          (dig2-racer)
  ())

(defmethod nox:start-document :before ((self owllink-parser) locator)
  (declare (ignorable locator))
  ;;(setf *stack* nil)
  (owllink-start-response-generation (parser-stream self)))

(defmethod nox:start-element ((self owllink-parser) (tag nox:open-tag) mode)
  (declare (ignore mode))
    ;(print *stack*)
  (with-error-handling 
   (let ((token-string (nox:token-string tag))
         (attributes (nox:tag-attributes tag)))
      
     (cond ((or (eq (parser-state self) ':normal)
                (null (parser-state self)))

            (owllink-start-normal-mode self token-string attributes))
               
           ((and (consp (parser-state self))
                 (eq (first (parser-state self)) ':telling))
            (owllink-start-kb-request self token-string attributes
                                               (second (parser-state self))
                                               ':telling))
               
           ((and (consp (parser-state self))
                 (eq (first (parser-state self)) ':retracting))
            (owllink-start-kb-request self token-string attributes
                                               (second (parser-state self))
                                               ':retracting))
               
           (t (error 'syntax-error :argument (format nil "Cannot handle tag ~A" token-string)))))
   (error (c) 
          (owllink-generate-error self c))))

(defmethod nox:end-document :after ((self owllink-parser) locator)
  (declare (ignorable locator))
  #+:debug
  (when *debug-dig*
    (unless (null *stack*)
      (loop for entry in *stack* do (print entry))
      (error "Stack is not empty. Some elements have not been considered.")))
  (owllink-end-response-generation (parser-stream self)))

(defmethod nox:end-element ((self owllink-parser) tag mode)
  (declare (ignore mode))
  ;;(print *stack*)
  (with-error-handling
   (let ((token-string (nox:token-string tag)))
     ;;(format t "~&END ~A ~S" token-string mode)
        
     (cond ((or (eq (parser-state self) ':normal)
                (null (parser-state self)))
            (owllink-end-normal-mode self token-string))

           ((and (consp (parser-state self))
                 (eq (first (parser-state self)) ':telling))
            (owllink-end-kb-request self token-string (second (parser-state self)) ':telling))
             
           ((and (consp (parser-state self))
                 (eq (first (parser-state self)) ':retracting))
            (owllink-end-kb-request self token-string (second (parser-state self)) ':retracting))
             
           (t (error 'syntax-error :argument (format nil "Cannot handle tag ~A" token-string)))))

   (error (c)
          (owllink-generate-error self c))))


;;; ======================================================================

(defun owllink-read-file (filename &rest args &key &allow-other-keys)
  (apply #'owl-syntaxes:owllink-read-file1 filename args))

(defun owllink-read-document (url &rest args &key &allow-other-keys)
  (apply #'owl-syntaxes:owllink-read-document1 url args))
    

#|
(defun owllink-read-file (filename &rest initargs &key kb-name (init t)  
                                   &allow-other-keys)
  (let ((*stack* nil)
        (*visited-urls* (cons filename *visited-urls*))
        (syntax (get-syntax-from-file filename)))
    (with-output-to-string (*standard-output*)
      (ecase syntax
        ((:owl-xml :owllink)
         (unless (probe-file filename)
           (error "File ~S does not exist." filename))
         (dig-read-file-internal 'owllink-parser filename kb-name init initargs))
        (:rdf-xml
         (apply #'owl-read-file filename :kb-name kb-name :init init initargs))
        (:dont-know (error "Cannot determine syntax used in resource ~A." filename))))))

(defun owllink-read-document (url-spec &rest initargs 
                                       &key kb-name 
                                       (recursive nil) (init t)
                                       &allow-other-keys)
  (with-output-to-string (*standard-output*)
    (unless (find url-spec *visited-urls* :test #'string=)
      (let ((*stack* nil)
            (*visited-urls* (cons url-spec *visited-urls*))
            (syntax (get-syntax-from-document url-spec 
                                              #+:aserve (getf initargs ':verbose))))
        (ecase syntax
          ((:owl-xml :owllink)
           (dig-read-document-internal 'owllink-parser url-spec kb-name init 
                                       (list* :recursive recursive initargs)))
          (:rdf-xml
           (apply #'owl-read-document url-spec :kb-name kb-name :init init :recursive recursive
                  initargs))
          (:dont-know (error "Cannot determine syntax used in resource ~A." url-spec)))))))

|#


(defun owllink-eval-request (req ent)
  (with-racer-critical-section
    (racer:new-request "Evaluating OWLlink request...")
    (racer:set-progress-value 1)
    (let ((*error-indicator* "SyntaxError errorMessage"))
      (with-output-to-string (*standard-output*)
        (dig-eval-request-internal 'owllink-parser req ent)))
    (racer:clear-request)))

;;; ======================================================================

(defun owllink-tag-equal (token-string tag)
  (let ((tag-length (length tag)))
    (and (loop for i from 0 below #.(length +owllink-url-prefix+)
               always (char= (aref +owllink-url-prefix+ i) (aref token-string i)))
         (= (+ #.(length +owllink-url-prefix+) tag-length) (length token-string))
         (loop for i from 0 below tag-length
               for j from #.(length +owllink-url-prefix+)
               always (char= (aref tag i) (aref token-string j))))))

(defun ox-tag-equal (token-string tag)
  (let ((tag-length (length tag)))
    (and (loop for i from 0 below #.(length +owl-version+)
               always (char= (aref +owl-version+ i) (aref token-string i)))
         (= (+ #.(length +owl-version+) tag-length) (length token-string))
         (loop for i from 0 below tag-length
               for j from #.(length +owl-version+)
               always (char= (aref tag i) (aref token-string j))))))

(defun find-owllink-attribute (name attributes &optional (intern t))
  (let ((value (cdr (assoc (concatenate 'string +owllink-url-prefix+ name)
                           attributes :test #'string-equal))))
    (if (and intern (stringp value))
      (intern value)
      value)))

(defun find-ox-attribute (name attributes &optional (intern t))
  (let ((value (cdr (assoc (concatenate 'string +owl-version+ name)
                           attributes :test #'string-equal))))
    (if (and intern (stringp value))
      (intern value)
      value)))

(defun owllink-as-symbol (node)
  (intern (node-uri node)))


;;; ===============================================================================

(defun owllink-start-parse-concept-or-role (self token-string attributes)
  (declare (ignorable self))
  (cond ((ox-tag-equal token-string "Annotation")
         (let ((annotation-uri (or (find-ox-attribute "annotationURI" attributes)
                                   (find-ox-attribute "URI" attributes))))
           (push-stack annotation-uri *stack*)))
        ((ox-tag-equal token-string "Label")
         )
        ((ox-tag-equal token-string "Comment")
         )
        ((ox-tag-equal token-string "Datatype")
         (let ((name (find-ox-attribute "URI" attributes))
               (arity (find-ox-attribute "arity" attributes)))
           (push-stack (list '|Datatype| name arity) *stack*)))
        ((ox-tag-equal token-string "Class")
         (push-stack (list '|OWLClass| (find-ox-attribute "URI" attributes)) *stack*))
        ((ox-tag-equal token-string "ObjectProperty")
         (push-stack (list '|ObjectProperty| (find-ox-attribute "URI" attributes)) *stack*))
        ((ox-tag-equal token-string "SubObjectPropertyChain")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "InverseObjectProperty")
         (push-stack (list '|InverseObjectProperty| 
                     (find-ox-attribute "URI" attributes))
               *stack*))
        ((ox-tag-equal token-string "DataProperty")
         (push-stack (list '|DataProperty| (find-ox-attribute "URI" attributes)) *stack*))
        ((ox-tag-equal token-string "AnnotationProperty")
         (push-stack (list '|AnnotationProperty| (find-ox-attribute "URI" attributes)) *stack*))
        ((ox-tag-equal token-string "Individual")
         (push-stack (list '|Individual| (find-ox-attribute "URI" attributes)) *stack*))
        ((ox-tag-equal token-string "Constant")
         (push-stack 'end *stack*)
         (let ((datatype (find-ox-attribute "datatypeURI" attributes)))
           (push-stack (or datatype (owllink-as-symbol !xsd:string))
                 *stack*)))
        ((ox-tag-equal token-string "ObjectIntersectionOf")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "ObjectUnionOf")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "ObjectComplementOf") ; no end marker required
         )
        ((ox-tag-equal token-string "ObjectOneOf")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "ObjectSomeValuesFrom") ; no end marker required
         )
        ((ox-tag-equal token-string "ObjectAllValuesFrom") ; no end marker required
         )
        ((ox-tag-equal token-string "ObjectExistsSelf") ; no end marker required
         )
        ((ox-tag-equal token-string "ObjectHasValue") ; no end marker required
         )
        ((ox-tag-equal token-string "ObjectMinCardinality")
         (let ((cardinality (find-ox-attribute "cardinality" attributes nil))) 
           (push-stack 'end *stack*) ; end marker because quantification can be missing
           (push-stack cardinality *stack*)))
        ((ox-tag-equal token-string "ObjectMaxCardinality")
         (let ((cardinality (find-ox-attribute "cardinality" attributes nil)))
           (push-stack 'end *stack*) ; end marker because quantification can be missing
           (push-stack cardinality *stack*)))
        ((ox-tag-equal token-string "ObjectExactCardinality")
         (let ((cardinality (find-ox-attribute "cardinality" attributes nil)))
           (push-stack 'end *stack*) ; end marker because quantification can be missing
           (push-stack cardinality *stack*)))
        ((ox-tag-equal token-string "DataComplementOf") ; no marker required
         )
        ((ox-tag-equal token-string "DataOneOf")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "DataSomeValuesFrom") ; no marker required
         )
        ((ox-tag-equal token-string "DataAllValuesFrom") ; no marker required
         )
        ((ox-tag-equal token-string "DataHasValue") ; no marker required
         )
        ((ox-tag-equal token-string "DataMinCardinality")
         (let ((cardinality (find-ox-attribute "cardinality" attributes nil)))
           (push-stack 'end *stack*) ; end marker because quantification can be missing
           (push-stack cardinality *stack*)))
        ((ox-tag-equal token-string "DataMaxCardinality")
         (let ((cardinality (find-ox-attribute "cardinality" attributes nil)))
           (push-stack 'end *stack*) ; end marker because quantification can be missing
           (push-stack cardinality *stack*)))
        ((ox-tag-equal token-string "DataExactCardinality")
         (let ((cardinality (find-ox-attribute "cardinality" attributes nil)))
           (push-stack 'end *stack*) ; end marker because quantification can be missing
           (push-stack cardinality *stack*)))
        ((ox-tag-equal token-string "DatatypeRestriction")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "Restriction")
         (let ((facet (find-ox-attribute "facet" attributes)))
           (push-stack facet *stack*)))
        (t (error 'syntax-error :argument (format nil "Cannot handle tag ~A" token-string))))
  nil)

(defun owllink-end-parse-concept-or-role (self token-string)
  (declare (ignore self))
  (cond ((ox-tag-equal token-string "Annotation")
         (let ((constant-or-entity (pop-stack *stack*))
               (role (ol-as-role (pop-stack *stack*))))
           (push-stack `(|Annotation| ,role ,constant-or-entity) *stack*)))
        ((ox-tag-equal token-string "Label")
         (let ((string (pop-stack *stack*)))
           (push-stack `('|Annotation| ,(ol-as-role (concatenate 'string +owl-version+ "Label"))
                                       ,string)
                       *stack*)))
        ((ox-tag-equal token-string "Comment")
         (let ((string (pop-stack *stack*)))
           (push-stack `('|Annotation| ,(ol-as-role (concatenate 'string +owl-version+ "Comment")) 
                                       ,string)
                       *stack*)))
        ((ox-tag-equal token-string "Datatype") ; keep stack
         )
        ((ox-tag-equal token-string "Class") ; keep stack
         )
        ((ox-tag-equal token-string "ObjectProperty") ; keep stack
         )
        ((ox-tag-equal token-string "SubObjectPropertyChain")
         (let ((roles (loop for elem = (pop-stack *stack*)
                            until (eq elem 'end)
                            collect (ol-as-role elem))))
           (push-stack `(chain .,roles) *stack*)))
        ((ox-tag-equal token-string "InverseObjectProperty") ; keep stack
         )
        ((ox-tag-equal token-string "DataProperty") ; keep stack
         )
        ((ox-tag-equal token-string "AnnotationProperty") ; keep stack
         )
        ((ox-tag-equal token-string "Individual") ; keep stack
         )
        ((ox-tag-equal token-string "Constant")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (declare (ignore annotations))
           (let ((string (if (= (length elements) 1) 
                             ""
                           (second elements)))
                 (datatype (first elements)))
             (push-stack (ol-as-literal string datatype) *stack*))))
        ((ox-tag-equal token-string "ObjectIntersectionOf")
         (let ((concepts (loop for elem = (pop-stack *stack*)
                               until (eq elem 'end)
                               collect (ol-as-concept elem))))
           (push-stack `(and .,concepts) *stack*)))
        ((ox-tag-equal token-string "ObjectUnionOf")
         (let ((concepts (loop for elem = (pop-stack *stack*)
                               until (eq elem 'end)
                               collect (ol-as-concept elem))))
           (push-stack `(or .,concepts) *stack*)))
        ((ox-tag-equal token-string "ObjectComplementOf")
         (push-stack `(not ,(ol-as-concept (pop-stack *stack*))) *stack*))
        ((ox-tag-equal token-string "ObjectOneOf")
         (let ((inds (loop for elem = (pop-stack *stack*)
                           until (eq elem 'end)
                           collect (ol-as-individual elem))))
           (push-stack `(one-of .,inds) *stack*)))
        ((ox-tag-equal token-string "ObjectSomeValuesFrom")
         (let ((qualification (ol-as-concept (pop-stack *stack*)))
               (role (ol-as-role (pop-stack *stack*))))
           (push-stack `(some ,role ,qualification) *stack*)))
        ((ox-tag-equal token-string "ObjectAllValuesFrom")
         (let ((qualification (ol-as-concept (pop-stack *stack*)))
               (role (ol-as-role (pop-stack *stack*))))
           (push-stack `(all ,role ,qualification) *stack*)))
        ((ox-tag-equal token-string "ObjectExistsSelf")
         (push-stack `(self-reference ,(ol-as-role (pop-stack *stack*))) *stack*))
        ((ox-tag-equal token-string "ObjectHasValue")
         (let ((individual (ol-as-individual (pop-stack *stack*)))
               (role (ol-as-role (pop-stack *stack*))))
           (push-stack `(some ,role (one-of ,individual)) *stack*)))
        ((ox-tag-equal token-string "ObjectMinCardinality")
         (let* ((qualification (if (eq (third *stack*) 'end)
                                   'top
                                 (ol-as-concept (pop *stack*))))
                (role (ol-as-role (pop-stack *stack*)))
                (cardinality (pop-stack *stack*)))
           (pop-stack *stack*)
           (push-stack `(at-least ,(read-from-string cardinality) 
                                  ,role ,qualification) *stack*)))
        ((ox-tag-equal token-string "ObjectMaxCardinality")
         (let* ((qualification (if (eq (third *stack*) 'end)
                                   'top
                                 (ol-as-concept (pop *stack*))))
                (role (ol-as-role (pop-stack *stack*)))
                (cardinality (pop-stack *stack*)))
           (pop-stack *stack*)
           (push-stack `(at-most ,(read-from-string cardinality)
                                 ,role ,qualification) *stack*)))
        ((ox-tag-equal token-string "ObjectExactCardinality")
         (let* ((qualification (if (eq (third *stack*) 'end)
                                   'top
                                 (ol-as-concept (pop *stack*))))
                (role (ol-as-role (pop-stack *stack*)))
                (cardinality (pop-stack *stack*)))
           (pop-stack *stack*)
           (push-stack `(exactly ,(read-from-string cardinality)
                                 ,role ,qualification) *stack*)))
        ((ox-tag-equal token-string "DataComplementOf")
         (push-stack `(d-complement ,(pop-stack *stack*)) *stack*))
        ((ox-tag-equal token-string "DataOneOf")
         (let ((possible-values (loop for elem = (pop-stack *stack*)
                                      until (eq elem 'end)
                                      collect (ol-as-literal elem))))
           (push-stack `(d-datarange (d-possible-values .,possible-values)) *stack*)))
       
	((ox-tag-equal token-string "DataSomeValuesFrom")
         (let ((qualification
		`(d-datarange ,(ol-as-datarange (pop-stack *stack*))))
               (role (ol-as-role (pop-stack *stack*))))
           (push-stack `(d-some ,role ,qualification) *stack*)))
	#|
	((ox-tag-equal token-string "DataSomeValuesFrom")
         (let ((qualification (ol-as-concept (pop-stack *stack*)))
               (role (ol-as-role (pop-stack *stack*))))
	       (push-stack `(d-some ,role ,qualification) *stack*)))
        |# 
        ((ox-tag-equal token-string "DataAllValuesFrom")
         (let ((qualification (ol-as-concept (pop-stack *stack*)))
               (role (ol-as-role (pop-stack *stack*))))
           (push-stack `(d-all ,role ,qualification) *stack*)))
        ((ox-tag-equal token-string "DataHasValue")
         (let ((constant (ol-as-literal (pop-stack *stack*)))
               (role (ol-as-role (pop-stack *stack*))))
           (push-stack `(d-filler ,role ,constant) *stack*)))
        ((ox-tag-equal token-string "DataMinCardinality")
         (let* ((qualification (if (eq (third *stack*) 'end)
                                   'top
                                 (ol-as-concept (pop *stack*))))
                (role (ol-as-role (pop-stack *stack*)))
                (cardinality (pop-stack *stack*)))
           (pop-stack *stack*)
           (push-stack `(d-at-least ,(read-from-string cardinality)
                                    ,role ,qualification) *stack*)))
        ((ox-tag-equal token-string "DataMaxCardinality")
         (let* ((qualification (if (eq (third *stack*) 'end)
                                   'top
                                 (ol-as-concept (pop *stack*))))
                (role (ol-as-role (pop-stack *stack*)))
                (cardinality (pop-stack *stack*)))
               (pop-stack *stack*)
               (push-stack `(d-at-most ,(read-from-string cardinality)
                                       ,role ,qualification) *stack*)))
        ((ox-tag-equal token-string "DataExactCardinality")
         (let* ((qualification (if (eq (third *stack*) 'end)
                                   'top
                                 (ol-as-concept (pop *stack*))))
                (role (ol-as-role (pop-stack *stack*)))
                (cardinality (pop-stack *stack*)))
           (pop-stack *stack*)
           (push-stack `(and (d-least ,cardinality ,role ,qualification)
                             (d-atmost ,cardinality ,role ,qualification)) *stack*)))
        ((ox-tag-equal token-string "DatatypeRestriction")
         (let ((restrictions (loop for elem = (pop-stack *stack*)
                                   until (eq elem 'end)
                                   collect elem)))
           (push-stack `(d-datarange ,restrictions) *stack*)))
        ((ox-tag-equal token-string "Restriction")
         (let ((constant (pop-stack *stack*))
               (facet (ol-as-facet (pop-stack *stack*))))
           (push-stack `(d-facet ,facet ,constant) *stack*)))
        (t (error 'syntax-error :argument (format nil "Cannot handle tag ~A" token-string))))
  nil)

;;; ----------------------------------------------------------------------

(defun owllink-start-normal-mode (self token-string attributes)  
  (cond ((owllink-tag-equal token-string "RequestMessage")
         )
        ((owllink-tag-equal token-string "GetDescription")
         )
        ((owllink-tag-equal token-string "GetSettings")
         (push-stack (find-owllink-attribute "kb" attributes) *stack*)
         )
        ((owllink-tag-equal token-string "Set")
         (push-stack (find-owllink-attribute "kb" attributes) *stack*)
         (push-stack (find-owllink-attribute "key" attributes) *stack*))
        ((owllink-tag-equal token-string "CreateKB")
         (push-stack (find-owllink-attribute "kb" attributes) *stack*)
         (push-stack (find-owllink-attribute "name" attributes) *stack*)
         )
        ((owllink-tag-equal token-string "ClearKB")
         (push-stack (find-owllink-attribute "kb" attributes) *stack*))
        ((owllink-tag-equal token-string "ReleaseKB")
         (push-stack (find-owllink-attribute "kb" attributes) *stack*))
        ((owllink-tag-equal token-string "DeleteKBs")
         )
        ((owllink-tag-equal token-string "DeleteKB")
         (push-stack (find-owllink-attribute "kb" attributes) *stack*))
        ((owllink-tag-equal token-string "Racer")
         (push-stack (find-owllink-attribute "expr" attributes nil) *stack*))
        ((owllink-tag-equal token-string "Tell") 
         (let* ((kb-name (find-owllink-attribute "kb" attributes))
                (internal-name (member kb-name *public-kbs* :key #'second)))
           (when internal-name
             (setf kb-name (first (first internal-name))))
           (cond ((null kb-name)
                  (setf kb-name 'owllink-default-kb)
                  (create-new-kb-internal kb-name
                                          nil (maintain-owlapi-axioms self))
                  (setf (parser-state self) (list ':telling kb-name))
                  ;;(setf (find-kb-descriptor kb-name) (make-kb-descriptor kb-name))
                  (error 'kb-error :argument "No kb attribute specified."))
                 ((null (find-tbox kb-name nil))
                  (let ((kb-name-1 kb-name))
                    (setf kb-name 'owllink-default-kb)
                    (create-new-kb-internal kb-name
                                            nil (maintain-owlapi-axioms self))
                    (setf (parser-state self) (list ':telling kb-name))
                    ;;(setf (find-kb-descriptor kb-name) (make-kb-descriptor kb-name))
                    (error 'kb-error :argument (format nil "KB '~A' not found -- using '~A'." 
                                                       kb-name-1 kb-name))
                    
                    ))
                  (t (setf (parser-state self) (list ':telling kb-name))))))
        ((ox-tag-equal token-string "Ontology")
         (push-stack 'end *stack*)
         (let ((kb-name (or (find-ox-attribute "ontologyURI" attributes)
                            (find-ox-attribute "URI" attributes))))
           (when (null kb-name)
             (setf kb-name 'owllink-default-kb))
           (create-new-kb-internal  kb-name
                                    nil (maintain-owlapi-axioms self))
           ;;(setf (find-kb-descriptor kb-name) (make-kb-descriptor kb-name))
           (setf (parser-state self) (list ':telling kb-name))))
        ((owllink-tag-equal token-string "Retract") 
         (let* ((kb-name (find-owllink-attribute "kb" attributes))
                (internal-name (member kb-name *public-kbs* :key #'second)))
           (when internal-name
             (setf kb-name (first (first internal-name))))
           (cond ((null kb-name)
                  (setf kb-name 'owllink-default-kb)
                  (setf (parser-state self) (list ':retracting kb-name))
                  (error 'kb-error :argument "No kb attribute specified.")
                  ;;(setf (find-kb-descriptor kb-name) (make-kb-descriptor kb-name))
                  (|OWLAPI-autoBatchRemoveAxiomsFrom| kb-name kb-name))
                 ((null (find-tbox kb-name nil))
                  (let ((kb-name-1 kb-name))
                    (setf kb-name 'owllink-default-kb)
                    ;;(setf (find-kb-descriptor kb-name) (make-kb-descriptor kb-name))
                    (create-new-kb-internal kb-name
                                            nil (maintain-owlapi-axioms self))
                    (|OWLAPI-autoBatchRemoveAxiomsFrom| kb-name kb-name)
                    (setf (parser-state self) (list ':retracting kb-name))
                    ;;(setf (find-kb-descriptor kb-name) (make-kb-descriptor kb-name))
                    (error 'kb-error :argument (format nil "KB '~A' not found -- using '~A'." 
                                                       kb-name-1 kb-name))
                    
                    ))
                  (t (|OWLAPI-autoBatchRemoveAxiomsFrom| kb-name kb-name)
                     (setf (parser-state self) (list ':retracting kb-name))))))
        ((owllink-tag-equal token-string "GetAllClasses")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetAllObjectProperties")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetAllDataProperties")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetAllAnnotationProperties")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetAllIndividuals")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetAllDatatypes")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsKBSatisfiable")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsKBStructurallyConsistent") ; 
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetKBLanguage") 
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsClassSatisfiable")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsClassSubsumedBy")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "AreClassesDisjoint")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "AreClassesEquivalent")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetSubClasses")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*)
         (push-stack (find-owllink-attribute "direct" attributes nil) 
               *stack*))
        ((owllink-tag-equal token-string "GetSuperClasses")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*) 
         (push-stack (find-owllink-attribute "direct" attributes nil) 
               *stack*))
        ((owllink-tag-equal token-string "GetEquivalentClasses")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetSubClassHierarchy")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*)
         (push-stack 'end *stack*))
        ((owllink-tag-equal token-string "IsObjectPropertySatisfiable")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsObjectPropertySubsumedBy")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsObjectPropertyFunctional")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsObjectPropertyInverseFunctional")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsObjectPropertyTransitive")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsObjectPropertySymmetric")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsObjectPropertyAsymmetric")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsObjectPropertyReflexive")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsObjectPropertyIrreflexive")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "AreObjectPropertiesEquivalent")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "AreObjectPropertiesDisjoint")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetSubObjectProperties")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*)
         (push-stack (find-owllink-attribute "direct" attributes nil)  
               *stack*))
        ((owllink-tag-equal token-string "GetSuperObjectProperties")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*)
         (push-stack (find-owllink-attribute "direct" attributes nil) 
               *stack*))
        ((owllink-tag-equal token-string "GetEquivalentObjectProperties")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetSubObjectPropertyHierarchy")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsDataPropertySatisfiable")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsDataPropertySubsumedBy")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsDataPropertyFunctional")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "AreDataPropertiesEquivalent")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "AreDataPropertiesDisjoint")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetSubDataProperties")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetSuperDataProperties")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetEquivalentDataProperties")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetSubDataPropertyHierarchy")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "AreIndividualsEqual")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "AreIndividualsDisjoint")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsInstanceOf")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*) 
         (push-stack (find-owllink-attribute "direct" attributes nil) 
               *stack*))
        ((owllink-tag-equal token-string "GetTypes")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*)
         (push-stack (find-owllink-attribute "direct" attributes nil) 
               *stack*)) 
        ((owllink-tag-equal token-string "GetFlattenTypes")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*)
         (push-stack (find-owllink-attribute "direct" attributes nil) 
               *stack*))
        ((owllink-tag-equal token-string "GetEquivalentIndividuals")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetDisjointIndividuals")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetFlattenDisjointIndividuals")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetObjectPropertiesOfSource")
         (push-stack (find-owllink-attribute "negated" attributes) 
               *stack*) 
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetDataPropertiesOfSource")
         (push-stack (find-owllink-attribute "negated" attributes) 
               *stack*) 
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetObjectPropertiesOfFiller")
         (push-stack (find-owllink-attribute "negated" attributes) 
               *stack*) 
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetDataPropertiesOfConstant")
         (push-stack (find-owllink-attribute "negated" attributes) 
               *stack*) 
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetObjectPropertiesBetween")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetDataPropertiesBetween")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetInstances")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*)
         (push-stack (find-owllink-attribute "direct" attributes nil) 
               *stack*))
        ((owllink-tag-equal token-string "GetObjectPropertyFillers")
         (push-stack (find-owllink-attribute "negated" attributes) 
               *stack*) 
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetDataPropertyFillers")
         (push-stack (find-owllink-attribute "negated" attributes) 
               *stack*) 
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetObjectPropertySources")
         (push-stack (find-owllink-attribute "negated" attributes) 
               *stack*) 
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetDataPropertySources")
         (push-stack (find-owllink-attribute "negated" attributes) 
               *stack*) 
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetFlattenInstances")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*) 
         (push-stack (find-owllink-attribute "direct" attributes nil) 
               *stack*))
        ((owllink-tag-equal token-string "GetFlattenObjectPropertyFillers")
         (push-stack (find-owllink-attribute "negated" attributes) 
               *stack*) 
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "GetFlattenObjectPropertySources")
         (push-stack (find-owllink-attribute "negated" attributes) 
               *stack*) 
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "AreIndividualsRelated")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "IsIndividualRelatedWithConstant")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*))
        ((owllink-tag-equal token-string "Literal") ; do not do anything
         )
        ((owllink-tag-equal token-string "Retrieve")
         (push-stack (find-owllink-attribute "kb" attributes) 
               *stack*)
         (push-stack (find-owllink-attribute "ntuples" attributes) 
               *stack*)
         (push-stack 'end *stack*))
        ((owllink-tag-equal token-string "QueryHead")
         (push-stack 'end *stack*))
        ((owllink-tag-equal token-string "QueryBody")
         )
        ((owllink-tag-equal token-string "QueryVariable")
         (push-stack `(variable ,(find-owllink-attribute "URI" attributes))
               *stack*))
        #|((owllink-tag-equal token-string "AttrValue")
         (push-stack `(attribute-value ,(find-owllink-attribute "URI" attributes))
               *stack*))|#
        ((owllink-tag-equal token-string "QueryObjectIntersectionOf")
         (push-stack 'end *stack*))
        ((owllink-tag-equal token-string "QueryObjectUnionOf")
         (push-stack 'end *stack*))
        ((owllink-tag-equal token-string "QueryObjectComplementOf"))
        ((owllink-tag-equal token-string "ConceptQueryAtom")
         )
        ((owllink-tag-equal token-string "RoleQueryAtom")
         )
        (t (owllink-start-parse-concept-or-role self token-string attributes))))

;;; ----------------------------------------------------------------------


(defun owllink-end-normal-mode (self token-string)
  (with-error-handling
   (cond ((owllink-tag-equal token-string "RequestMessage") 
          ;; request message tags are used to set up namespace
          )
         ((owllink-tag-equal token-string "GetDescription")
          (owllink-describe-racer (parser-stream self)))
         ((owllink-tag-equal token-string "GetSettings")
          (let ((kb (pop-stack *stack*)))
            (owllink-describe-settings (parser-stream self) kb)))
         ((owllink-tag-equal token-string "Set")
          (let ((literal (pop-stack *stack*))
                (key (pop-stack *stack*))
                (kb (pop-stack *stack*)))
            (change-settings literal key kb)
            (setf (maintain-owlapi-axioms self) (not *owllink-lean-mode*))
            (owllink-generate-ok (parser-stream self) (get-warnings kb))))
         ((owllink-tag-equal token-string "CreateKB")
          (let ((name (pop-stack *stack*))
                (kb-name (pop-stack *stack*)))
            (owllink-create-new-kb (parser-stream self) kb-name name 
                                   (maintain-owlapi-axioms self))))
         ((owllink-tag-equal token-string "ClearKB")
          ;; Should we dispose the OWLAPI reasoner as well ???
          (let ((kb-name (pop-stack *stack*)))
            (init-tbox kb-name)
            (init-abox kb-name kb-name)
            (owllink-generate-ok (parser-stream self) (get-warnings kb-name))))
         ((owllink-tag-equal token-string "ReleaseKB")
          (let ((kb-name (pop-stack *stack*)))
            (owllink-generate-ok (parser-stream self) (get-warnings kb-name))
            (when *release-kb-deletes-kb*
              (setf *public-kbs* (delete kb-name *public-kbs* :key #'first))
              (setf (find-kb-descriptor kb-name) nil)
              (|OWLAPI-disposeReasoner| kb-name))))
         ((owllink-tag-equal token-string "DeleteKBs")
          (loop for kb being the hash-values of *kbs* do 
                (setf *public-kbs* (delete kb *public-kbs* :key #'first))
                (|OWLAPI-disposeReasoner| kb))
          (clrhash *kbs*)
          (owllink-generate-ok (parser-stream self) (get-warnings)))
         ((owllink-tag-equal token-string "DeleteKB")          
          (let ((kb-name (pop-stack *stack*)))
            (owllink-generate-ok (parser-stream self) (get-warnings kb-name))
            (setf *public-kbs* (delete kb-name *public-kbs* :key #'first))
            (setf (find-kb-descriptor kb-name) nil)
            (|OWLAPI-disposeReasoner| kb-name)))
         ((owllink-tag-equal token-string "Racer") ; just return ok, handled above
          (let ((expression (pop-stack *stack*)))
            (owllink-generate-result 
             (parser-stream self)
             (if (null expression)
                 (error 'syntax-error :argument (format nil "Expression not found in Racer command."))
               (with-output-to-string (stream)
                 (racer-execute-expression expression stream)))
             'racer
             (get-warnings))))
         ((owllink-tag-equal token-string "GetAllClasses")
          (let ((kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (remove-if (lambda (concept) 
                                                    (or (eq concept 'top)
                                                        (eq concept 'bottom)))
                                                  (all-atomic-concepts kb))
                                       'concept-set
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetAllObjectProperties")
          (let ((kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (all-roles kb
                                                  :inverse-test #'no-inverse-roles
                                                  :test (lambda (role)
                                                          (not (role-used-as-datatype-property-p role kb))))
                                       'role-set
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetAllDataProperties")
          (let ((kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (all-roles kb
                                                  :inverse-test (lambda (role)
                                                                  (declare (ignore role))
                                                                  nil)
                                                  :test (lambda (role)
                                                          (role-used-as-datatype-property-p role kb)))
                                       'role-set
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetAllAnnotationProperties")
          (let ((kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (all-roles kb
                                                  :inverse-test #'no-inverse-roles
                                                  :test (lambda (role)
                                                          (role-used-as-annotation-property-p role kb)))
                                       'role-set
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetAllIndividuals")
          (let ((kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (all-individuals kb)
                                       'individual-set
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetAllDatatypes")
          (let ((kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (error "Not yet implemented."))))
         ((owllink-tag-equal token-string "IsKBSatisfiable")
          (let ((kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (abox-consistent-p kb)
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsKBStructurallyConsistent") ; Signature
          ;; I do not know how to implement this.
          (let ((kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (error "Not yet implemented."))))
         ((owllink-tag-equal token-string "GetKBLanguage") 
          (let ((kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (get-tbox-language kb)
                                       'values
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsClassSatisfiable")
          (let ((concept (ol-as-concept (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (concept-satisfiable-p concept kb)
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsClassSubsumedBy")
          (let ((concept-2 (ol-as-concept (pop-stack *stack*)))
                (concept-1 (ol-as-concept (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (concept-subsumes-p concept-2 concept-1 kb)
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "AreClassesDisjoint")
          (let ((concept-2 (ol-as-concept (pop-stack *stack*)))
                (concept-1 (ol-as-concept (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (concept-disjoint-p concept-1 concept-2 kb)
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "AreClassesEquivalent")
          (let ((concept-2 (ol-as-concept (pop-stack *stack*)))
                (concept-1 (ol-as-concept (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (concept-equivalent-p concept-1 concept-2 kb)
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetSubClasses")
          (let ((concept (ol-as-concept (pop-stack *stack*)))
                (direct (ol-as-boolean (pop-stack *stack*) t))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (if direct
                                           (atomic-concept-children concept kb)
                                         (atomic-concept-descendants concept kb))
                                       'concept-synsets
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetSuperClasses")
          (let ((concept (ol-as-concept (pop-stack *stack*)))
                (direct (ol-as-boolean (pop-stack *stack*) t))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (if direct
                                           (atomic-concept-parents concept kb)
                                         (atomic-concept-ancestors concept kb))
                                       'concept-synsets
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetEquivalentClasses")
          (let ((concept (ol-as-concept (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (remove concept (atomic-concept-synonyms concept kb))
                                       'concept-set
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetSubClassHierarchy") 
          (let ((concept (ol-as-concept (pop-stack *stack*))))
            (if (eq concept 'end) ; no class specified, use top
                (setf concept 'top)
              (pop-stack *stack*)) ; skip end on stack
            (let ((kb (pop-stack *stack*)))
              (with-kb-settings (kb)
                (owllink-generate-result (parser-stream self)
                                         (generate-subconcept-hierarchy 
                                          (if (null concept)
                                              'top
                                            concept)
                                          kb)
                                         'concept-hierarchy
                                         (get-warnings kb))))))
         ((owllink-tag-equal token-string "IsObjectPropertySatisfiable")
          (let ((role (ol-as-role (pop-stack *stack*)))
                (kb (pop-stack *stack*))) 
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-bottom-object-role+)
                                              nil)
                                             ((eq role +owl-top-object-role+)
                                              (tbox-coherent-p kb))
                                             (t (role-satisfiable-p role kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsObjectPropertySubsumedBy")
          (let ((role-2 (ol-as-role (pop-stack *stack*)))
                (role-1 (ol-as-role (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role-2 +owl-top-object-role+)
                                              t)
                                             ((eq role-1 +owl-bottom-object-role+)
                                              t)
                                             ((eq role-1 +owl-top-object-role+)
                                              nil)
                                             ((eq role-2 +owl-bottom-object-role+)
                                              (not (role-satisfiable-p role-1 kb)))
                                             (t (role-subsumes-p role-1 role-2 kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsObjectPropertyFunctional")
          (let ((role (ol-as-role (pop-stack *stack*)) )
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-bottom-object-role+)
                                              t)
                                             ((eq role +owl-top-object-role+)
                                              nil)
                                             (t
                                              (feature-p role kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsObjectPropertyInverseFunctional")
          (let ((role (ol-as-role (pop-stack *stack*)) )
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-bottom-object-role+)
                                              t)
                                             ((eq role +owl-top-object-role+)
                                              nil)
                                             (t
                                              (inverse-feature-p role kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsObjectPropertyTransitive")
          (let ((role (ol-as-role (pop-stack *stack*)) )
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-bottom-object-role+)
                                              t)
                                             ((eq role +owl-top-object-role+)
                                              t)
                                             (t
                                              (transitive-p role kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsObjectPropertySymmetric")
          (let ((role (ol-as-role (pop-stack *stack*)) )
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-bottom-object-role+)
                                              t)
                                             ((eq role +owl-top-object-role+)
                                              t)
                                             (t
                                              (symmetric-p role kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsObjectPropertyAsymmetric")
          (let ((role (ol-as-role (pop-stack *stack*)) )
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-bottom-object-role+)
                                              t)
                                             ((eq role +owl-top-object-role+)
                                              nil)
                                             (t
                                              (asymmetric-p role kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsObjectPropertyReflexive")
          (let ((role (ol-as-role (pop-stack *stack*)) )
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-bottom-object-role+)
                                              t)
                                             ((eq role +owl-top-object-role+)
                                              t)
                                             (t
                                              (reflexive-p role kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsObjectPropertyIrreflexive")
          (let ((role (ol-as-role (pop-stack *stack*)) )
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-bottom-object-role+)
                                              t)
                                             ((eq role +owl-top-object-role+)
                                              nil)
                                             (t
                                              (irreflexive-p role kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "AreObjectPropertiesEquivalent")
          (let ((role-2 (ol-as-role (pop-stack *stack*)) )
                (role-1 (ol-as-role (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role-1 +owl-top-object-role+)
                                              (eq role-2 +owl-top-object-role+))
                                             ((eq role-2 +owl-top-object-role+)
                                              nil)
                                             ((eq role-2 +owl-bottom-object-role+)
                                              (or (eq role-1 +owl-bottom-object-role+)
                                                  (not (role-satisfiable-p role-1 kb))))
                                             (t (role-equivalent-p role-1 role-2 kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "AreObjectPropertiesDisjoint")
          (let ((role-2 (ol-as-role (pop-stack *stack*)))
                (role-1 (ol-as-role (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role-1 +owl-top-object-role+)
                                              nil)
                                             ((eq role-2 +owl-top-object-role+)
                                              nil)
                                             ((eq role-1 +owl-bottom-object-role+)
                                              t)
                                             ((eq role-2 +owl-bottom-object-role+)
                                              t)
                                             (t (role-disjoint-p role-1 role-2 kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetSubObjectProperties")
          (let ((role (ol-as-role (pop-stack *stack*)))
                (direct (ol-as-boolean (pop-stack *stack*) t))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-bottom-object-role+)
                                              nil)
                                             ((eq role +owl-top-object-role+)
                                              (transform-into-role-synsets
                                               (all-roles 
                                                kb
                                                :inverse-test #'no-inverse-roles
                                                :test (lambda (role)
                                                        (and (not (role-used-as-datatype-property-p role kb))
                                                             (or (not direct)
                                                                 (null (atomic-role-parents role kb)))))
                                                :default (list +owl-bottom-object-role+))
                                               kb))
                                             (t
                                              (if direct
                                                  (transform-into-role-synsets
                                                   (or (atomic-role-children role kb)
                                                       (list +owl-bottom-object-role+))
                                                   kb)
                                                (transform-into-role-synsets 
                                                 (cons +owl-bottom-object-role+
                                                       (remove role (atomic-role-descendants role kb)))
                                                 kb))))
                                       'role-synsets
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetSuperObjectProperties")
          (let ((role (ol-as-role (pop-stack *stack*)) )
                (direct (ol-as-boolean (pop-stack *stack*) t))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-top-object-role+)
                                              nil)
                                             ((eq role +owl-bottom-object-role+)
                                              (transform-into-role-synsets
                                               (if (get-object-bottom-role kb)
                                                   (let ((parents 
                                                          (atomic-role-parents 
                                                           (get-object-bottom-role kb) kb)))
                                                     (if (null parents)
                                                         (list +owl-top-object-role+)))
                                                 (all-roles 
                                                  kb
                                                  :inverse-test #'no-inverse-roles
                                                  :test (lambda (role)
                                                          (and (not (role-used-as-datatype-property-p role kb))
                                                               (or (not direct)
                                                                   (null (atomic-role-children role kb)))))
                                                  :default (list +owl-top-object-role+)))
                                               kb))
                                             (t
                                              (if direct
                                                  (transform-into-role-synsets
                                                   (or (atomic-role-parents role kb)
                                                       (list +owl-top-object-role+ )kb)
                                                   kb)
                                                (transform-into-role-synsets
                                                 (cons +owl-top-object-role+
                                                       (remove role (atomic-role-ancestors role kb)))
                                                 kb))))
                                       'role-synsets
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetEquivalentObjectProperties")
          (let ((role (ol-as-role (pop-stack *stack*)) )
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-top-object-role+)
                                              nil)
                                             ((eq role +owl-bottom-object-role+)
                                              (if (get-object-bottom-role kb)
                                                  (atomic-role-synonyms 
                                                   (get-object-bottom-role kb) kb)
                                                nil))
                                             (t (remove role (atomic-role-synonyms role kb))))
                                       'role-set
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetSubObjectPropertyHierarchy")
          (let ((role (ol-as-role (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (generate-sub-object-role-hierarchy role kb)
                                       'role-hierarchy
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsDataPropertySatisfiable")
          (let ((role (ol-as-role (pop-stack *stack*)))
                (kb (pop-stack *stack*))) 
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-bottom-data-role+)
                                              nil)
                                             ((eq role +owl-top-data-role+)
                                              (tbox-coherent-p kb))
                                             (t (role-satisfiable-p role kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsDataPropertySubsumedBy")
          (let ((role-2 (ol-as-role (pop-stack *stack*)) )
                (role-1 (ol-as-role (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role-2 +owl-top-data-role+)
                                              t)
                                             ((eq role-1 +owl-bottom-data-role+)
                                              t)
                                             ((eq role-1 +owl-top-data-role+)
                                              nil)
                                             ((eq role-2 +owl-bottom-data-role+)
                                              (not (role-satisfiable-p role-2 kb)))
                                             (t (role-subsumes-p role-1 role-2 kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsDataPropertyFunctional")
          (let ((role (ol-as-role (pop-stack *stack*)) )
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-bottom-data-role+)
                                              t)
                                             ((eq role +owl-top-data-role+)
                                              nil)
                                             (t
                                              (feature-p role kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "AreDataPropertiesEquivalent")
          (let ((role-2 (ol-as-role (pop-stack *stack*))) 
                (role-1 (ol-as-role (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role-1 +owl-top-data-role+)
                                              (eq role-2 +owl-top-data-role+))
                                             ((eq role-2 +owl-top-data-role+)
                                              nil)
                                             ((eq role-2 +owl-bottom-data-role+)
                                              (or (eq role-1 +owl-bottom-data-role+)
                                                  (not (role-satisfiable-p role-1 kb))))
                                             (t (role-equivalent-p role-1 role-2 kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "AreDataPropertiesDisjoint")
          (let ((role-2 (ol-as-role (pop-stack *stack*)))
                (role-1 (ol-as-role (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role-1 +owl-top-data-role+)
                                              nil)
                                             ((eq role-2 +owl-top-data-role+)
                                              nil)
                                             ((eq role-1 +owl-bottom-data-role+)
                                              t)
                                             ((eq role-2 +owl-bottom-data-role+)
                                              t)
                                             (t (role-disjoint-p role-1 role-2 kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetSubDataProperties")
          (let ((role (ol-as-role (pop-stack *stack*)) )
                (direct (ol-as-boolean (pop-stack *stack*) t))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-bottom-data-role+)
                                              nil)
                                             ((eq role +owl-top-data-role+)
                                              (transform-into-role-synsets
                                               (all-roles 
                                                kb
                                                :inverse-test #'no-inverse-roles
                                                :test (lambda (role)
                                                        (and (role-used-as-datatype-property-p role kb)
                                                             (or (not direct)
                                                                 (null (atomic-role-parents role kb)))))
                                                :default (list +owl-bottom-data-role+))
                                               kb))
                                             (t 
                                              (if direct
                                                  (transform-into-role-synsets
                                                   (or (atomic-role-children role kb)
                                                       (list +owl-bottom-data-role+))
                                                   kb)
                                                (transform-into-role-synsets 
                                                 (or (remove role (atomic-role-descendants role kb))
                                                     (list +owl-bottom-data-role+))
                                                 kb))))
                                       'role-synsets
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetSuperDataProperties")
          (let ((role (ol-as-role (pop-stack *stack*)))
                (direct (ol-as-boolean (pop-stack *stack*) t))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-top-data-role+)
                                              nil)
                                             ((eq role +owl-bottom-data-role+)
                                              (transform-into-role-synsets
                                               (if (get-data-bottom-role kb)
                                                   (let ((parents 
                                                          (atomic-role-parents (get-data-bottom-role kb)
                                                                               kb)))
                                                     (if (null parents)
                                                         (list +owl-top-object-role+)))
                                                 (all-roles 
                                                  kb
                                                  :inverse-test #'no-inverse-roles
                                                  :test (lambda (role)
                                                          (and (role-used-as-datatype-property-p role kb)
                                                               (or (not direct)
                                                                   (null (atomic-role-children role kb)))))
                                                  :default (list +owl-top-data-role+)))
                                               kb))
                                             (t
                                              (if direct
                                                  (transform-into-role-synsets
                                                   (or (atomic-role-parents role kb)
                                                       (list +owl-top-data-role+))
                                                   kb)
                                                (transform-into-role-synsets
                                                 (cons +owl-top-data-role+
                                                       (remove role (atomic-role-ancestors role kb)))
                                                 kb))))
                                       'role-synsets
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetEquivalentDataProperties")
          (let ((role (ol-as-role (pop-stack *stack*)) )
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (cond ((eq role +owl-top-data-role+)
                                              nil)
                                             ((eq role +owl-bottom-data-role+)
                                              (if (get-data-bottom-role kb)
                                                  (atomic-role-synonyms (get-data-bottom-role kb)
                                                                        kb)
                                                nil))
                                             (t (remove role (atomic-role-synonyms role kb))))
                                       'role-set
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetSubDataPropertyHierarchy")
          (let ((role (ol-as-role (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (generate-sub-data-role-hierarchy role kb)
                                       'role-hierarchy
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "AreIndividualsEqual")
          (let ((individual-2 (ol-as-individual (pop-stack *stack*)) )
                (individual-1 (ol-as-individual (pop-stack *stack*)) )
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (member individual-1
                                               (retrieve-individual-synonyms individual-2
                                                                             nil kb))
                                       'boolean
                                       (get-warnings kb)))))  
         ((owllink-tag-equal token-string "AreIndividualsDisjoint")
          (let ((individual-2 (ol-as-individual (pop-stack *stack*)))
                (individual-1 (ol-as-individual (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (member individual-1
                                               (retrieve-individual-antonyms individual-2
                                                                             nil kb))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsInstanceOf")
          (let ((individual (ol-as-individual (pop-stack *stack*)) )
                (concept (ol-as-concept (pop-stack *stack*)))
                (direct-p (ol-as-boolean (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result 
               (parser-stream self)
               (if direct-p
                   (and (individual-instance-p individual concept kb)
                        (not (find-if (lambda (child)
                                        (individual-instance-p individual (first child) kb))
                                      (atomic-concept-children concept kb))))
                 (individual-instance-p individual concept kb))
               'boolean
               (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetTypes")
          (let ((individual (ol-as-individual (pop-stack *stack*)))
                (direct-p (ol-as-boolean (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (if direct-p
                                           (most-specific-instantiators individual kb)
                                         (instantiators individual kb))
                                       'concept-synsets
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetFlattenTypes")
          (let ((individual (ol-as-individual (pop-stack *stack*)))
                (direct-p (ol-as-boolean (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (if direct-p
                                           (most-specific-instantiators individual kb)
                                         (instantiators individual kb))
                                       'concept-synsets
                                       (get-warnings kb))))) 
         ((owllink-tag-equal token-string "GetEquivalentIndividuals")
          (let ((individual (ol-as-individual (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (retrieve-individual-synonyms individual nil kb)
                                       'individual-set
                                       (get-warnings kb)))))
         (t 
          (owllink-end-normal-mode1 self token-string)))

   (error (c)
          (owllink-generate-error self c))))


(defun owllink-end-normal-mode1 (self token-string)
  (with-error-handling

   (cond ((owllink-tag-equal token-string "GetDisjointIndividuals")
         (let ((individual (ol-as-individual (pop-stack *stack*))  )
               (kb (pop-stack *stack*)))
           (with-kb-settings (kb)
                             (owllink-generate-result (parser-stream self)
                                                      (retrieve-individual-antonyms individual nil kb)
                                                      'individual-set
                                                      (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetFlattenDisjointIndividuals")
          (let ((individual (ol-as-individual (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (retrieve-individual-antonyms individual nil kb)
                                       'individual-set
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetObjectPropertiesOfSource")
          (let ((individual (ol-as-individual (pop-stack *stack*))  )
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (transform-into-role-synsets
                                        (mapcar #'first 
                                                (get-individual-successors individual :abox kb
                                                                           :no-inverses-p t))
                                        kb)
                                       'role-synsets
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetDataPropertiesOfSource")
          (let ((individual (ol-as-individual (pop-stack *stack*))  )
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (transform-into-role-synsets
                                        (mapcar #'first 
                                                (get-individual-datatype-fillers individual kb))
                                        kb)
                                       'role-synsets
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetObjectPropertiesOfFiller")
          (let ((individual (ol-as-individual (pop-stack *stack*))  )
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (transform-into-role-synsets
                                        (mapcar #'first 
                                                (get-individual-successors individual :abox kb
                                                                           :only-inverses-p t
                                                                           :no-inverses-p t))
                                        kb)
                                       'role-synsets
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetDataPropertiesOfConstant")
          (let ((constant (ol-as-literal (pop-stack *stack*)))
                (individual (ol-as-individual (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (declare (ignore constant individual))
            (with-kb-settings (kb)
              (error "Not yet implemented"))))
         ((owllink-tag-equal token-string "GetObjectPropertiesBetween") 
          (let ((individual-2 (ol-as-individual (pop-stack *stack*)))
                (individual-1 (ol-as-individual (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (retrieve-individual-filled-roles
                                        individual-1 individual-2 kb :synsets-p t)
                                       'role-synsets
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetDataPropertiesBetween")
          (let ((constant (ol-as-literal (pop-stack *stack*)))
                (individual (ol-as-individual (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result
               (parser-stream self)
               (transform-into-role-synsets
                (loop for (role literals) in (get-individual-datatype-fillers individual kb)
                      when (find constant literals :test #'equal)
                      collect role)
                kb)
               'role-synsets
               (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetInstances")
          (let ((concept (ol-as-concept (pop-stack *stack*)))
                (direct (ol-as-boolean (pop-stack *stack*) nil))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result 
               (parser-stream self)
               (transform-into-individual-synsets
                (if direct
                    (let* ((instances (retrieve-concept-instances concept kb))
                           (children (atomic-concept-children concept kb)))
                      (remove-if #'(lambda (ind) 
                                     (some #'(lambda (subconcept)
                                               (individual-instance-p ind (first subconcept) kb))
                                           children))
                                 instances))
                  (retrieve-concept-instances concept kb))
                kb)
               'individual-synsets
               (get-warnings kb)))))
         ((owllink-tag-equal token-string "GetObjectPropertyFillers")
          (let ((role (ol-as-role (pop-stack *stack*)) )
                (individual (ol-as-individual (pop-stack *stack*)) )
                (negated-p (ol-as-boolean (pop-stack *stack*))) 
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (if negated-p
                  (error "Not yet implemented")
                (owllink-generate-result (parser-stream self)
                                         (transform-into-individual-synsets
                                          (retrieve-individual-fillers individual role kb) 
                                          kb)
                                         'individual-synsets
                                         (get-warnings kb))))))
         ((owllink-tag-equal token-string "GetDataPropertyFillers")
          (let ((role (pop-stack *stack*)) 
                (individual (pop-stack *stack*)) 
                (negated-p (ol-as-boolean (pop-stack *stack*)))  
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (if negated-p
                  (error "Not yet implemented")
                (owllink-generate-result (parser-stream self)
                                         (retrieve-individual-told-datatype-fillers 
                                          (ol-as-individual individual) (ol-as-role role) nil kb t)
                                         'literal-set
                                         (get-warnings kb))))))
         ((owllink-tag-equal token-string "GetObjectPropertySources")
          (let ((role (ol-as-role (pop-stack *stack*)) )
                (individual (ol-as-individual (pop-stack *stack*)) )
                (negated-p (ol-as-boolean (pop-stack *stack*)))  
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (if negated-p
                  (error "Not yet implemented")
                (owllink-generate-result (parser-stream self)
                                         (transform-into-individual-synsets
                                          (retrieve-direct-predecessors individual role kb)
                                          kb)
                                         'individual-synsets
                                         (get-warnings kb))))))
         ((owllink-tag-equal token-string "GetDataPropertySources")
          (let ((role (ol-as-role (pop-stack *stack*)))
                (individual (ol-as-individual (pop-stack *stack*)) )
                (negated-p (ol-as-boolean (pop-stack *stack*)))  
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (if negated-p
                  (error "Not yet implemented")
                (owllink-generate-result (parser-stream self)
                                         (transform-into-individual-synsets
                                          (retrieve-direct-predecessors individual role kb)
                                          kb)
                                         'individual-synsets
                                         (get-warnings kb))))))
         ((owllink-tag-equal token-string "GetFlattenInstances")
          (let ((concept (ol-as-concept (pop-stack *stack*)))
                (direct (ol-as-boolean (pop-stack *stack*) nil))
                (kb (pop-stack *stack*)))
            (owllink-generate-result 
             (parser-stream self)
             (with-kb-settings (kb)
               (if direct
                   (let* ((instances (retrieve-concept-instances concept kb))
                          (children (atomic-concept-children concept kb)))
                     (remove-if #'(lambda (ind) 
                                    (some #'(lambda (subconcept)
                                              (individual-instance-p ind (first subconcept) kb))
                                          children))
                                instances))
                 (retrieve-concept-instances concept kb)))
             'individual-set
             (get-warnings kb))))
         ((owllink-tag-equal token-string "GetFlattenObjectPropertyFillers")
          (let ((role (ol-as-role (pop-stack *stack*)))
                (individual (ol-as-individual (pop-stack *stack*)))
                (negated-p (ol-as-boolean (pop-stack *stack*))) 
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (if negated-p
                  (error "Not yet implemented")
                (owllink-generate-result (parser-stream self)
                                         (retrieve-individual-fillers individual role kb)
                                         'individual-set
                                         (get-warnings kb))))))
         ((owllink-tag-equal token-string "GetFlattenObjectPropertySources")
          (let ((role (ol-as-role (pop-stack *stack*)))
                (individual (ol-as-individual (pop-stack *stack*)))
                (negated-p (ol-as-boolean (pop-stack *stack*)))  
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (if negated-p
                  (error "Not yet implemented")
                (owllink-generate-result (parser-stream self)
                                         (retrieve-direct-predecessors individual role kb)
                                         'individual-set
                                         (get-warnings kb))))))
         ((owllink-tag-equal token-string "AreIndividualsRelated")
          (let ((individual-2 (ol-as-individual (pop-stack *stack*)))
                (individual-1 (ol-as-individual (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (not (null (retrieve-individual-filled-roles
                                                   individual-1 individual-2 kb)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "IsIndividualRelatedWithConstant")
          (let ((constant (ol-as-literal (pop-stack *stack*)))
                (individual (ol-as-individual (pop-stack *stack*)))
                (kb (pop-stack *stack*)))
            (with-kb-settings (kb)
              (owllink-generate-result (parser-stream self)
                                       (find constant
                                             (get-individual-datatype-fillers individual kb)
                                             :key #'second
                                             :test (lambda (literal literal-list)
                                                     (find literal literal-list :test #'equal)))
                                       'boolean
                                       (get-warnings kb)))))
         ((owllink-tag-equal token-string "Literal") ; do not do anything
          (let ((literal-value (pop-stack *stack*)))
            (push-stack`(literal ,literal-value) *stack*)))
         ((owllink-tag-equal token-string "Retrieve")
          (let (head 
                body) 
            (unless (eq (first *stack*) 'end) ; Looks strange but Retrieve can be empty.
              (setf body (pop-stack *stack*))
              (setf head (pop-stack *stack*)))
            (pop-stack *stack*)  ; remove the end in any case.
            (let ((ntuples (pop-stack *stack*))
                  (kb (pop-stack *stack*)))
              (with-kb-settings (kb)
                (if (null head)
                    (owllink-generate-result (parser-stream self)
                                             (dig-retrieve (gensym "ID") ntuples head body kb)
                                             'boolean
                                             (get-warnings kb))
                  (owllink-generate-result (parser-stream self)
                                           (list head 
                                                 (dig-retrieve (gensym "ID") ntuples head body kb))
                                           'query-result
                                           (get-warnings kb)))))))
         ((owllink-tag-equal token-string "QueryHead")
          (multiple-value-bind (head annotations)
              (collect-stack-elements)
            (declare (ignore annotations))
            (push-stack head *stack*)))
         ((owllink-tag-equal token-string "QueryBody")
          )
         ((owllink-tag-equal token-string "QueryVariable")
          )
         #|((owllink-tag-equal token-string "AttrValue")
          (let ((var (pop-stack *stack*))
                (attribute-value-spec (pop-stack *stack*)))
            (push-stack (append attribute-value-spec (list var))
                        *stack*)))|#
         ((owllink-tag-equal token-string "QueryObjectIntersectionOf")
          (multiple-value-bind (query-conjunction annotations)
              (collect-stack-elements)
            (declare (ignore annotations))
            (push-stack `(qand .,query-conjunction) *stack*)))
         ((owllink-tag-equal token-string "QueryObjectUnionOf")
          (multiple-value-bind (query-conjunction annotations)
              (collect-stack-elements)
            (declare (ignore annotations))
            (push-stack `(qunion .,query-conjunction) *stack*)))
         ((owllink-tag-equal token-string "QueryObjectComplementOf")
          (push `(qneg ,(pop-stack *stack*)) *stack*))
         ((owllink-tag-equal token-string "ConceptQueryAtom")
          (let ((concept (ol-as-concept (pop-stack *stack*)))
                (var-or-ind (ol-as-var-or-ind (pop-stack *stack*))))
            (push `(cqatom ,var-or-ind ,concept) *stack*)))
         ((owllink-tag-equal token-string "RoleQueryAtom")
          (let ((role (ol-as-role (pop-stack *stack*)))
                (var-or-ind2 (ol-as-var-or-ind (pop-stack *stack*)))
                (var-or-ind1 (ol-as-var-or-ind (pop-stack *stack*))))
            (push `(rqatom ,var-or-ind1 ,var-or-ind2 ,role) *stack*)))
         (t (owllink-end-parse-concept-or-role self token-string)))
   (error (c)
          (owllink-generate-error self c))))



(defun no-inverse-roles (role)
  (declare (ignore role))
  nil)




;;; ----------------------------------------------------------------------

(defun owllink-start-kb-request (self token-string attributes kb-name mode)
  (declare (ignore kb-name mode))
  (cond ((ox-tag-equal token-string "Imports")
         (push-stack 'end *stack*)
         (push-stack (find-owllink-attribute "URI" attributes) *stack*))
        ((ox-tag-equal token-string "Import") ; for OWL/XML syntax
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "Declaration")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "SubClassOf")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "EquivalentClasses")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "DisjointClasses")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "DisjointUnion")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "SubObjectPropertyOf")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "EquivalentObjectProperties")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "DisjointObjectProperties")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "ObjectPropertyDomain")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "ObjectPropertyRange")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "InverseObjectProperties")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "FunctionalObjectProperty")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "InverseFunctionalObjectProperty")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "SymmetricObjectProperty")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "SymmetricObjectProperty")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "ReflexiveObjectProperty")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "TransitiveObjectProperty")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "SubDataPropertyOf")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "EquivalentDataProperties")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "DisjointDataProperties")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "DataPropertyDomain")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "DataPropertyRange")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "FunctionalDataProperty")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "SameIndividuals")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "DifferentIndividuals")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "ObjectPropertyAssertion")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "NegativeObjectPropertyAssertion")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "DataPropertyAssertion")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "NegativeDataPropertyAssertion")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "ClassAssertion")
         (push-stack 'end *stack*))
        ((ox-tag-equal token-string "EntityAnnotation")
         (push-stack 'end *stack*))
        (t (owllink-start-parse-concept-or-role self token-string attributes))))

(defun owllink-end-kb-request (self token-string kb-name mode)
  (cond ((or (ox-tag-equal token-string "Imports") (ox-tag-equal token-string "Import")) 
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (declare (ignore annotations))
           (let ((uri (first (reverse elements)))
                 ;; Imports can have a URI attribute or the URI can be in the tag body
                 ;; In both cases (first (reverse elements)) leads us to the kb to be imported.
                 (ignore-import (ignore-import self))
                 (import-meta-ontologies (import-meta-ontologies self))
                 (excluded-meta-ontologies (excluded-meta-ontologies self))
                 (maintain-owlapi-axioms (maintain-owlapi-axioms self)))
             (unless (or ignore-import (null uri))
               (if (or import-meta-ontologies
                       (not (find uri 
                                  excluded-meta-ontologies
                                  :test #'string=))
                       (not (or (string= uri "http://protege.stanford.edu/plugins/owl/protege")
                                ;;(string= uri "http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl")
                                (string= uri +owl-version+)
                                ;;(string= uri "http://www.w3.org/2002/07/owl#")
                                (string= uri nox:-rdfs-uri-)
                                (string= uri nox:-rdf-uri-)
                                (string= uri +swrl-version+)
                                (string= uri +swrlx-version+)
                                (string= uri +swrlb-version+)
                                ;;(string= uri "http://www.daml.org/rules/proposal/swrl.owl")
                                ;;(string= uri "http://www.daml.org/rules/proposal/swrlb.owl")
                                )))
                   (let ((ont (|OWLAPI-getAutoOntology| kb-name))
                         (*stack* *stack*)
                         (*response-end-tag* nil)
                         (*silent* t))
                     (owllink-read-document uri :kb-name kb-name 
                                            :recursive t
                                            :ignore-import ignore-import
                                            :import-meta-ontologies import-meta-ontologies
                                            :excluded-meta-ontologies excluded-meta-ontologies
                                            :maintain-owlapi-axioms maintain-owlapi-axioms)
                     (ecase mode
                       (:telling (|OWLAPI-autoAddAxiomsTo| ont kb-name))
                       (:retracting (|OWLAPI-autoBatchRemoveAxiomsFrom| ont kb-name))))

                 (racer-warn "Ignoring import of meta ontology ~a. ~
                              ~%A meta ontology is not required for reasoning. ~
                              ~%Use :import-meta-ontologies t to enforce the import." 
                             uri))))))
        ((ox-tag-equal token-string "Declaration") 
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((declaration (first elements)))
             (handle-annotations mode kb-name
                                    annotations
                                    (|OWLAPI-getOWLDeclarationAxiom| declaration kb-name)))))
        ((ox-tag-equal token-string "SubClassOf")         
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((subsumer (second elements))
                 (subsumee (first elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLSubClassAxiom| (ol-as-concept subsumee) (ol-as-concept subsumer) kb-name)))))
        ((ox-tag-equal token-string "EquivalentClasses")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (handle-annotations mode kb-name
            annotations
            (|OWLAPI-getOWLEquivalentClassesAxiom| (mapcar #'ol-as-concept elements) kb-name))))
        ((ox-tag-equal token-string "DisjointClasses") 
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (handle-annotations mode kb-name
            annotations
            (|OWLAPI-getOWLDisjointClassesAxiom| (mapcar #'ol-as-concept elements) kb-name))))
        ((ox-tag-equal token-string "DisjointUnion")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (setf elements (reverse elements))
           (handle-annotations mode kb-name
            annotations
            (|OWLAPI-getOWLDisjointUnionAxiom| (ol-as-concept (first elements))
                                               (mapcar #'ol-as-concept (rest elements))
                                               kb-name))))
        ((ox-tag-equal token-string "SubObjectPropertyOf")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((subsumer (second elements))
                 (subsumee (first elements)))
             (if (and (consp subsumee) (eq (first subsumee) 'chain))
                 (handle-annotations mode kb-name
                  annotations
                  (|OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom|
                   (mapcar #'ol-as-role (rest subsumee))
                   (ol-as-role subsumer) kb-name))
               (handle-annotations mode kb-name
                annotations
                (|OWLAPI-getOWLObjectSubPropertyAxiom| (ol-as-role subsumee) 
                                                       (ol-as-role subsumer) kb-name))))))
        ((ox-tag-equal token-string "EquivalentObjectProperties")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLEquivalentObjectPropertiesAxiom| (mapcar #'ol-as-role elements) kb-name))))
        ((ox-tag-equal token-string "DisjointObjectProperties")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLDisjointObjectPropertiesAxiom| (mapcar #'ol-as-role elements) kb-name))))
        ((ox-tag-equal token-string "ObjectPropertyDomain")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((role (first elements))
                 (class (second elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLObjectPropertyDomainAxiom| (ol-as-role role)
                                                        (ol-as-concept class) kb-name)))))
        ((ox-tag-equal token-string "ObjectPropertyRange")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((role (first elements))
                 (class (second elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLObjectPropertyRangeAxiom| (ol-as-role role)
                                                       (ol-as-concept class) kb-name)))))
        ((ox-tag-equal token-string "InverseObjectProperties")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((role-1 (second elements))
                 (role-2 (first elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLInverseObjectPropertiesAxiom| (ol-as-role role-1) 
                                                           (ol-as-role role-2) kb-name)))))
        ((ox-tag-equal token-string "FunctionalObjectProperty")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((role-1 (first elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLFunctionalObjectPropertyAxiom| (ol-as-role role-1) kb-name)))))
        ((ox-tag-equal token-string "InverseFunctionalObjectProperty")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((role-1 (first elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom| (ol-as-role role-1) kb-name)))))
        ((ox-tag-equal token-string "SymmetricObjectProperty")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((role-1 (first elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLSymmetricObjectPropertyAxiom| (ol-as-role role-1) kb-name)))))
        ((ox-tag-equal token-string "AsymmetricObjectProperty")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((role-1 (first elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLAsymmetricObjectPropertyAxiom| (ol-as-role role-1) kb-name)))))
        ((ox-tag-equal token-string "ReflexiveObjectProperty")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((role-1 (first elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLReflexiveObjectPropertyAxiom| (ol-as-role role-1) kb-name)))))
        ((ox-tag-equal token-string "TransitiveObjectProperty")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((role-1 (first elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLTransitiveObjectPropertyAxiom| (ol-as-role role-1) kb-name)))))
        ((ox-tag-equal token-string "SubDataPropertyOf")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((subsumer (second elements))
                 (subsumee (first elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLDataSubPropertyAxiom| (ol-as-role subsumee) 
                                                   (ol-as-role subsumer) kb-name)))))
        ((ox-tag-equal token-string "EquivalentDataProperties")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLEquivalentDataPropertiesAxiom| (mapcar #'ol-as-role elements) kb-name))))
        ((ox-tag-equal token-string "DisjointDataProperties")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLDisjointDataPropertiesAxiom| (mapcar #'ol-as-role elements) kb-name))))
        ((ox-tag-equal token-string "FunctionalDataProperty")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((role-1 (first elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLFunctionalDataPropertyAxiom| (ol-as-role role-1) kb-name)))))
        ((ox-tag-equal token-string "DataPropertyDomain")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((role (first elements))
                 (class (second elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLDataPropertyDomainAxiom| (ol-as-role role)
                                                      (ol-as-concept class) kb-name)))))
        ((ox-tag-equal token-string "DataPropertyRange")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((role (first elements))
                 (datatype (second elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLDataPropertyRangeAxiom| (ol-as-role role)
                                                     (ol-as-datarange datatype) kb-name)))))
        ((ox-tag-equal token-string "SameIndividuals") 
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (handle-annotations mode kb-name
            annotations
            (|OWLAPI-getOWLSameIndividualsAxiom| (mapcar #'ol-as-individual elements) kb-name))))
        ((ox-tag-equal token-string "DifferentIndividuals")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (handle-annotations mode kb-name
            annotations
            (|OWLAPI-getOWLDifferentIndividualsAxiom| (mapcar #'ol-as-individual elements) kb-name))))
        ((ox-tag-equal token-string "ObjectPropertyAssertion")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((role (first elements))
                 (individual-1 (second elements))
                 (individual-2 (third elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLObjectPropertyAssertionAxiom| (ol-as-individual individual-1) 
                                                           (ol-as-role role) 
                                                           (ol-as-individual individual-2)
                                                           kb-name)))))
        ((ox-tag-equal token-string "NegativeObjectPropertyAssertion")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((individual-2 (first elements))
                 (role (second elements))
                 (individual-1 (third elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom|
                          (ol-as-individual individual-1)
                          (ol-as-role role)
                          (ol-as-individual individual-2)
                          kb-name)))))
        ((ox-tag-equal token-string "DataPropertyAssertion")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((role (first elements))
                 (individual (second elements))
                 (literal (third elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLDataPropertyAssertionAxiom| (ol-as-individual individual) 
                                                         (ol-as-role role) 
                                                         literal
                                                         kb-name)))))
        ((ox-tag-equal token-string "NegativeDataPropertyAssertion")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((literal (first elements))
                 (role (second elements))
                 (individual (third elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom|
                          (ol-as-individual individual)
                          (ol-as-role role)
                          (ol-as-literal literal)
                          kb-name)))))
        ((ox-tag-equal token-string "EntityAnnotation")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements t)
           (let ((entity (second elements))
                 (annotation (first elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLEntityAnnotationAxiom| entity annotation kb-name)))))
        ((ox-tag-equal token-string "ClassAssertion")
         (multiple-value-bind (elements annotations)
             (collect-stack-elements)
           (let ((class (first elements))
                 (individual (second elements)))
             (handle-annotations mode kb-name
              annotations
              (|OWLAPI-getOWLClassAssertionAxiom| (ol-as-individual individual)
                                                  (ol-as-concept class)
                                                  kb-name)))))
        ((owllink-tag-equal token-string "Tell")
         (let ((kb (second (parser-state self))))
           (setf (parser-state self) ':normal)
           (owllink-generate-ok (parser-stream self) (get-warnings kb))))
        ((owllink-tag-equal token-string "Retract")
         (let ((kb (second (parser-state self))))
           (|OWLAPI-batchSynchronize| kb-name)
           (|OWLAPI-autoAddAxiomsTo| kb-name kb-name)
           (setf (parser-state self) ':normal)
           (owllink-generate-ok (parser-stream self) (get-warnings kb))))
        ((ox-tag-equal token-string "Ontology") 
         (let ((kb (second (parser-state self))))
           (multiple-value-bind (elements annotations)
               (collect-stack-elements)
             (declare (ignore elements))
             (loop for annotation in annotations do
                   (|OWLAPI-getOWLOntologyAnnotationAxiom| annotation
                                                           kb-name)))
           (setf (parser-state self) ':normal)
           (owllink-generate-ok (parser-stream self) (get-warnings kb))))
        (t (owllink-end-parse-concept-or-role self token-string))))

(defun generate-subconcept-hierarchy (concept kb)
  (loop for concept-nameset in (cons (atomic-concept-synonyms concept kb)
                                     (atomic-concept-descendants concept kb)) 
        when (member 'top concept-nameset)
        collect (list (cons (intern +owl-thing+ :racer) (remove '*top* (remove 'top concept-nameset)))
                      (remove-bottom (atomic-concept-children (first concept-nameset) kb)))
        else
        when (and (member 'bottom concept-nameset) (not (member 'top concept-nameset)))
        collect (list (cons (intern +owl-nothing+ :racer) (remove '*bottom* (remove 'bottom concept-nameset)))
                      (atomic-concept-children (first concept-nameset) kb))
        else
        collect (list concept-nameset (remove-bottom (atomic-concept-children (first concept-nameset) kb)))))

(defun remove-bottom (concept-namesets)
  (loop for concept-nameset in concept-namesets 
        unless (member 'bottom concept-nameset)
        collect concept-nameset))

(defun generate-sub-object-role-hierarchy (role kb)
  (let ((object-bottom-role (get-object-bottom-role kb)))
    (loop for role in (cond ((eq role +owl-bottom-object-role+)
                             (list +owl-bottom-object-role+))
                            ((eq role +owl-top-object-role+)
                             (list* +owl-top-object-role+
                                    +owl-bottom-object-role+
                                    (remove object-bottom-role 
                                            (all-roles 
                                             kb
                                             :inverse-test #'no-inverse-roles
                                             :test (lambda (role)
                                                     (and (not (role-used-as-datatype-property-p role kb))
                                                          (null (atomic-role-parents role kb))))))))
                            ((eq role object-bottom-role)
                             (list +owl-bottom-object-role+))
                            (t (cons +owl-bottom-object-role+ 
                                     (if object-bottom-role
                                         (remove object-bottom-role (atomic-role-descendants role kb))
                                       (atomic-role-descendants role kb)))))
          collect (list (cond ((eq role +owl-bottom-object-role+)
                               (if object-bottom-role
                                   (cons +owl-bottom-object-role+ 
                                         (atomic-role-synonyms object-bottom-role kb))
                                 (list +owl-bottom-object-role+)))
                              ((eq role +owl-top-object-role+)
                               (list +owl-top-object-role+))
                              (t (atomic-role-synonyms role kb)))
                        (cond ((eq role +owl-bottom-object-role+)
                               nil)
                              ((eq role +owl-top-object-role+)
                               (transform-into-role-synsets 
                                (all-roles 
                                 kb
                                 :inverse-test #'no-inverse-roles
                                 :test (lambda (role)
                                         (and (not (role-used-as-datatype-property-p role kb))
                                              (null (atomic-role-parents role kb))))
                                 :default (list +owl-bottom-object-role+))
                                kb))
                              (t (if (null (atomic-role-children role kb))
                                     (if object-bottom-role
                                         (cons +owl-bottom-object-role+ 
                                               (atomic-role-synonyms object-bottom-role kb))
                                       (list +owl-bottom-object-role+))
                                   (loop for role in (atomic-role-children role kb) 
                                         when (eq role object-bottom-role)
                                         collect (cons +owl-bottom-object-role+ 
                                                       (atomic-role-synonyms object-bottom-role kb))
                                         else
                                         collect (atomic-role-synonyms role kb)))))))))

(defun generate-sub-data-role-hierarchy (role kb)
  (let ((data-bottom-role (get-data-bottom-role kb)))
    (loop for role in (cond ((eq role +owl-bottom-data-role+)
                             (list +owl-bottom-data-role+))
                            ((eq role +owl-top-data-role+)
                             (list* +owl-top-data-role+
                                    +owl-bottom-data-role+
                                    (remove data-bottom-role 
                                            (all-roles 
                                             kb
                                             :inverse-test #'no-inverse-roles
                                             :test (lambda (role)
                                                     (and (role-used-as-datatype-property-p role kb)
                                                          (null (atomic-role-parents role kb))))))))
                            ((eq role data-bottom-role)
                             (list +owl-bottom-data-role+))
                            (t (cons +owl-bottom-data-role+ 
                                     (if data-bottom-role
                                         (remove data-bottom-role (atomic-role-descendants role kb))
                                       (atomic-role-descendants role kb)))))
          collect (list (cond ((eq role +owl-bottom-data-role+)
                               (if data-bottom-role
                                   (cons +owl-bottom-data-role+ 
                                         (atomic-role-synonyms data-bottom-role kb))
                                 (list +owl-bottom-data-role+)))
                              ((eq role +owl-top-data-role+)
                               (list +owl-top-data-role+))
                              (t (atomic-role-synonyms role kb)))
                        (cond ((eq role +owl-bottom-data-role+)
                               nil)
                              ((eq role +owl-top-data-role+)
                               (transform-into-role-synsets 
                                (all-roles 
                                 kb
                                 :inverse-test #'no-inverse-roles
                                 :test (lambda (role)
                                         (and (role-used-as-datatype-property-p role kb)
                                              (null (atomic-role-parents role kb))))
                                 :default (list +owl-bottom-data-role+))
                                kb))
                              (t (if (null (atomic-role-children role kb))
                                     (if data-bottom-role
                                         (cons +owl-bottom-data-role+ 
                                               (atomic-role-synonyms data-bottom-role kb))
                                       (list +owl-bottom-data-role+))
                                   (loop for role in (atomic-role-children role kb) 
                                         when (eq role data-bottom-role)
                                         collect (cons +owl-bottom-data-role+ 
                                                       (atomic-role-synonyms data-bottom-role kb))
                                         else
                                         collect (atomic-role-synonyms role kb)))))))))



;;; ======================================================================
           

(defun owllink-start-response-generation (stream)
  (unless *silent*
    (terpri stream)
    (format stream
            "<ResponseMessage xmlns=\"~A\"
                 xmlns:owl=\"~A\">"
            +owllink-url-prefix+ +owl-version+)
    (setf *response-end-tag* "</ResponseMessage>
")
    (terpri stream)))

(defun owllink-end-response-generation (stream)
  (unless *silent*
    (write-string "</ResponseMessage>" stream)
    (terpri stream)))

(defun owllink-format-class (concept stream)
  (format stream " <owl:Class URI=\"~A\"/>"
          (cond ((eq concept 'top)
                 +owl-thing+)
                ((eq concept 'bottom)
                 +owl-nothing+)
                (t (symbol-name concept)))))

(defun owllink-format-role (role stream)
  (format stream " <owl:ObjectProperty URI=\"~A\"/>"
          (symbol-name role)))

(defun owllink-format-individual (individual stream)
  (format stream " <owl:Individual URI=\"~A\"/>"
          (symbol-name individual)))

(defun owllink-format-literal (literal stream)
  (let ((literal-string (second literal))
        (datatype (second (third literal))))
    (format stream " <owl:Constant datatypeURI=\"~A\">~A</ox:Constant>"
            datatype
            literal-string)))

(defun owllink-generate-ok (stream warnings)
  (if (string= warnings "")
      (format stream "   <OK/>~%")
    (format stream "<OK warning=\"~A\"/>~%" (convert-to-xml warnings))))

(defun owllink-generate-result (stream result result-type warnings)
  (flet ((get-warnings () 
           (if (string= warnings "")
               ""
             (format nil " warning=\"~A\"" (convert-to-xml warnings)))))
    (terpri stream)
    (ecase result-type
      (racer (format stream "<Racer result=\"~A\"/>" (convert-to-xml result))
             (terpri stream))
      (boolean
       (cond ((eq result t)
              (format stream "<BooleanResponse~A result=\"true\"/>" (get-warnings))
              (terpri stream))
             ((eq result nil)
              (format stream "<BooleanResponse~A result=\"false\"/>" (get-warnings))
              (terpri stream))
             (t (error "should not happen."))))
      (concept-synsets
       (format stream "<SetOfClassSynsets~A>" (get-warnings))
       (terpri stream)
       (loop for concept-set in result do
             (write-string " <ClassSynset>" stream)
             (terpri stream)
             (loop for concept in concept-set 
                   unless (or (eq concept '*bottom*)
                              (eq concept '*top*)) do
                   (owllink-format-class concept stream)
                   (terpri stream))
             (write-string " </ClassSynset>" stream)
             (terpri stream))
       (write-string "</SetOfClassSynsets>" stream)
       (terpri stream))
      (concept-set
       (format stream "<SetOfClasses~A>" (get-warnings))
       (terpri stream)
       (loop for concept in result 
             unless (or (eq concept '*bottom*)
                        (eq concept '*top*)) do
             (owllink-format-class concept stream)
             (terpri stream))
       (write-string "</SetOfClasses>" stream)
       (terpri stream))
      (role-set
       (format stream "<SetOfRoles~A>" (get-warnings))
       (terpri stream)
       (loop for role in result do
             (owllink-format-role role stream)
             (terpri stream))
       (write-string "</SetOfRoles>" stream)
       (terpri stream))
      (role-synsets
       (format stream "<SetOfRoleSynsets~A>" (get-warnings))
       (terpri stream)
       (loop for roles in result do
             (write-string " <RoleSynset>" stream)
             (terpri stream)
             (loop for role in roles do
                   (owllink-format-role role stream)
                   (terpri stream))
             (write-string " </RoleSynset>" stream)
             (terpri stream))
       (write-string "</SetOfRoleSynsets>" stream)
       (terpri stream))
      (individual-synsets
       (format stream "<SetOfIndividualSynsets~A>" (get-warnings))
       (loop for individuals in result do
             (write-string "<IndividualSynset>" stream) 
             (loop for individual in individuals do
                   (owllink-format-individual individual stream)
                   (terpri stream))
             (write-string " </IndividualSynset>" stream)
             (terpri stream))
       (write-string " </SeOfIndividualSynsets>" stream)
       (terpri stream))
      (individual-set
       (format stream "<SetOfIndividuals~A>" (get-warnings))
       (terpri stream)
       (loop for individual in result do
             (owllink-format-individual individual stream)
             (terpri stream))
       (write-string "</SetOfIndividuals>" stream)
       (terpri stream))
      (literal-set
       (format stream "<SetofConstants~A>" (get-warnings))
       (terpri stream)
       (loop for literal in result do
             (owllink-format-literal literal stream)
             (terpri stream))
       (format stream "</SetofConstants>")
       (terpri stream))
      (concept-hierarchy
       (format stream "<ClassHierarchy~A>" (get-warnings))
       (terpri stream)
       (loop for hierarchy-pair in result do
             (format stream "<ClassSubClassPair>")
             (terpri stream)
             (let ((concept-set (first hierarchy-pair))
                   (subclass-concept-sets (second hierarchy-pair)))
               (format stream "<ClassSynset>")
               (terpri stream)
               (loop for concept in concept-set 
                     unless (or (eq concept '*bottom*)
                                (eq concept '*top*)) do
                     (owllink-format-class concept stream)
                     (terpri stream))
               (format stream "</ClassSynset>")
               (terpri stream)
               (format stream "<SetOfSubClassSynsets>")
               (terpri stream)
               (loop for concept-set in subclass-concept-sets do
                     (format stream "<ClassSynset>")
                     (terpri stream)
                     (loop for concept in concept-set 
                           unless (or (eq concept '*bottom*)
                                      (eq concept '*top*)) do
                           (owllink-format-class concept stream)
                           (terpri stream))
                     (format stream "</ClassSynset>")
                     (terpri stream))
               (format stream "</SetOfSubClassSynsets>")
               (terpri stream))
             (format stream "</ClassSubClassPair>") 
             (terpri stream))
       (format stream "</ClassHierarchy>")
       (terpri stream))
      (role-hierarchy
       (format stream "<PropertyHierarchy~A>" (get-warnings))
       (terpri stream)
       (loop for hierarchy-pair in result do
             (format stream "<PropertyHierarchyPair>")
             (terpri stream)
             (let ((role-set (first hierarchy-pair))
                   (subrole-role-sets (second hierarchy-pair)))
               (format stream "<PropertySynset>")
               (terpri stream)
               (loop for role in role-set 
                     unless (or (eq role '*bottom*)
                                (eq role '*top*)) do
                     (owllink-format-role role stream)
                     (terpri stream))
               (format stream "</PropertySynset>")
               (terpri stream)
               (format stream "<SetOfSubPropertySynsets>")
               (terpri stream)
               (loop for role-set in subrole-role-sets do
                     (format stream "<PropertySynset>")
                     (terpri stream)
                     (loop for role in role-set 
                           unless (or (eq role '*bottom*)
                                      (eq role '*top*)) do
                           (owllink-format-role role stream)
                           (terpri stream))
                     (format stream "</PropertySynset>")
                     (terpri stream))
               (format stream "</SetOfSubPropertySynsets>")
               (terpri stream))
             (format stream "</PropertyHierarchyPair>") 
             (terpri stream))
       (format stream "</PropertyHierarchy>")
       (terpri stream))
      (ind-pair-set
       (format stream "<IndividualPairSet~A>" (get-warnings))
       (loop for (individual1 individual2) in result do
             (format stream " <IndividualPair> <owl:Individual URI=\"~A\"/> <owl:Individual URI=\"~A\"/> </IndividualPair>"
                     (symbol-name individual1)
                     (symbol-name individual2))
             (terpri stream))
       (write-string " </IndividualPairSet>" stream)
       (terpri stream))
      (values
       (format stream "   <ConstantSet~A>" (get-warnings))
       (terpri stream)
       (loop for value in result do
             (etypecase value
               (integer 
                (owllink-format-literal `(d-literal ,value (d-base-type ,(transform-type 'integer)))
                                        stream))
               (real
                (owllink-format-literal `(d-literal ,value (d-base-type ,(transform-type 'real)))
                                        stream))
               (racer-boolean 
                (owllink-format-literal `(d-literal ,value (d-base-type ,(transform-type 'boolean)))
                                        stream))
               (string
                (owllink-format-literal `(d-literal ,value (d-base-type ,(transform-type 'boolean)))
                                        stream))))
       (format stream "   </ConstantSet>")
       (terpri stream))
      (query-result
       (let ((head (reverse (first result)))
             (tuples (second result)))
         (format stream "<QueryAnswers~A>" (get-warnings))
         (terpri stream)
         (format stream "<QueryHead>")
         (terpri stream)
         (owllink-print-head (reverse head) stream)
         (format stream "</QueryHead>")
         (terpri stream)
         (loop for tuple in tuples do
               (write-string "<Binding>" stream)
               (loop for (nil value) in (reverse tuple) do
                     (etypecase value
                       (integer 
                        (owllink-format-literal `(d-literal ,value (d-base-type ,(transform-type 'integer)))
                                                stream))
                       (real
                        (owllink-format-literal `(d-literal ,value (d-base-type ,(transform-type 'real)))
                                                stream))
                       (racer-boolean 
                        (owllink-format-literal `(d-literal ,value (d-base-type ,(transform-type 'boolean)))
                                                stream))
                       (string
                        (owllink-format-literal `(d-literal ,value (d-base-type ,(transform-type 'boolean)))
                                                stream))
                       (null (format stream "<ConstantSet/>"))
                       (symbol 
                        (format stream "<owl:Individual URI=\"~A\"/>" value))
                       (cons (format stream "<ConstantSet>")
                             (loop for value in value do
                                   (etypecase value
                                     (integer 
                                      (owllink-format-literal `(d-literal ,value (d-base-type ,(transform-type 'integer)))
                                                              stream))
                                     (real
                                      (owllink-format-literal `(d-literal ,value (d-base-type ,(transform-type 'real)))
                                                              stream))
                                     (racer-boolean 
                                      (owllink-format-literal `(d-literal ,value (d-base-type ,(transform-type 'boolean)))
                                                              stream))
                                     (string
                                      (owllink-format-literal `(d-literal ,value (d-base-type ,(transform-type 'boolean)))
                                                              stream))))
                             (format stream "</ConstantSet>"))))
               (write-string "</Binding>" stream)
               (terpri stream))
         ;;(terpri stream)
         (write-string "</QueryAnswers>" stream)
         (terpri stream)))
      )))


(defun owllink-print-head (head stream)
  (if (null head)
    nil
    (let ((entry (first head)))
      (ecase (first entry)
        (variable (format stream "<QueryVariable URI=\"~A\"/>" (second entry)))
        (individual (format stream "<owl:Individual URI=\"~A\"/>" (second entry)))
        (attribute-value 
         (format stream "<attrValue URI=\"~A\">" (second entry))
         (format stream "<QueryVariable URI=\"~A\"/>" (second (third entry)))
         (format stream "</attrValue>")))
      (terpri stream)
      (owllink-print-head (rest head) stream))))


(defun owllink-generate-error (parser c) 
  (setf (error-during-parse parser) t)                        
  (let ((stream (parser-stream parser)))
    (typecase c
      (kb-error (format stream "   <KBError errorMessage=\"~A\"/>" (transform-to-html c)))
      (syntax-error (format stream "   <SyntaxError errorMessage=\"~A\"/>" (transform-to-html c)))
      (semantic-error (format stream "   <SemanticError errorMessage=\"~A\"/>" (transform-to-html c)))
      (otherwise (format stream "   <Error errorMessage=\"~A\"/>" (transform-to-html c))))
    (terpri stream)))

(defun owllink-describe-racer (stream)
  (format stream "
   <Description name=\"~A\">
      <OWLlinkVersion major=\"1\" minor=\"0\"/>
      <ReasonerVersion major=\"~A\" minor=\"~A\" build=\"~A\"/>"
          (get-product-name)
          *owllink-major*
          *owllink-minor*
          *owllink-build*)
  (format-properties stream)
  (format-settings stream nil)
  (format stream "
      <SupportedExtension URI=\"http://www.owllink.org/ext/EpistemicGroundedConjunctiveQueries\"/>")
  (format stream "
      <SupportedExtension URI=\"http://www.owllink.org/ext/retraction\"/>")

  (loop for (public-kb name) in *public-kbs* do
        (format stream "
      <publicKB kb=\"~A\" name=\"~A\"/>" public-kb name))
  (format stream"
   </Description>")
  (terpri stream))

(defun owllink-describe-settings (stream kb)
  (format stream "
   <Settings>")
  (format-settings stream kb)
  (format stream"
   </Settings>")
  (terpri stream))

(defun format-properties (stream)
  (format stream "
      <Property key=\"selectedProfile\">
         <List>
            <OneOf>
               <Literal URI=\"~A#string\">OWL-DL</Literal>
            </OneOf>
         </List>
         <Literal>OWL-DL</Literal>
      </Property>"
          nox:-xsd-uri-)
  (format stream "
      <Property key=\"ignoresDeclarations\">
         <Datatype URI=\"~Aboolean\"/>
         <Literal>true</Literal>
      </Property>"
          nox:-xsd-uri-))

(defun format-settings (stream kb)
  (format stream "
      <Setting key=\"uniqueNameAssumption\">
         <Datatype URI=\"~Aboolean\"/>
         <Literal>~A</Literal>
      </Setting>"
          nox:-xsd-uri-
          (cond (kb 
                 (if (kb-descriptor-unique-name-assumption (find-kb-descriptor kb))
                     "true"
                   "false"))
                (*owllink-unique-name-assumption* 
                 "true")
                (t "false")))
  (format stream "
      <Setting key=\"ignoresAnnotations\">
         <Datatype URI=\"~Aboolean\"/>
         <Literal>~A</Literal>
      </Setting>"
          nox:-xsd-uri-
          (cond (kb 
                 (if (kb-descriptor-ignore-annotations (find-kb-descriptor kb))
                     "true"
                   "false"))
                (*owllink-ignore-annotations*
                 "true")
                (t "false")))
  (format stream "
      <Setting key=\"leanMode\">
         <Datatype URI=\"~Aboolean\"/>
         <Literal>~A</Literal>
      </Setting>"
          nox:-xsd-uri-
          (cond (*owllink-lean-mode*
                 "true")
                (t "false")))
  (format stream "
      <Setting key=\"releaseKBDeletesKB\">
         <Datatype URI=\"~Aboolean\"/>
         <Literal>~A</Literal>
      </Setting>"
          nox:-xsd-uri-
          (cond (*release-kb-deletes-kb* 
                 "true")
                (t "false")))
  (format stream "
      <Setting key=\"verboseMode\">
         <Datatype URI=\"~Aboolean\"/>
         <Literal>~A</Literal>
      </Setting>"
          nox:-xsd-uri-
          (cond (kb 
                 (if (kb-descriptor-verbose (find-kb-descriptor kb))
                     "true"
                   "false"))
                (*owllink-verbose*
                 "true")
                (t "false"))))

(defun owllink-create-new-kb (stream kb-name name maintain-owlapi-axioms)
  (cond ((null kb-name)
         (let ((uuid (create-uuid)))           
           (setf kb-name (intern uuid))
           (create-new-kb-internal kb-name name maintain-owlapi-axioms)
           (format stream
                   "   <KB kb=\"~A\"/>~%" kb-name)))
        ((and *release-kb-deletes-kb* (find-tbox kb-name nil))
         (error 'kb-error :argument (format nil "Knowledge base '~A' already exists." kb-name))
         )
        (t
         (create-new-kb-internal kb-name name maintain-owlapi-axioms)
         (format stream
                   "   <KB kb=\"~A\"/>~%" kb-name))))
  

(defun create-new-kb-internal (kb-name name maintain-owlapi-axioms)
  (let* ((*current-reasoner* (|OWLAPI-getCurrentReasoner|))
         (*current-ontology* (|OWLAPI-getAutoOntology| *current-reasoner*)))

    (|OWLAPI-newReasoner| kb-name t t)
      
    (unless (member kb-name (|OWLAPI-getOntologies| kb-name))
      (|OWLAPI-newOntology| kb-name kb-name))

    (if maintain-owlapi-axioms
        (progn 
	  (|OWLAPI-disableMemorySavingMode| kb-name)
	  (|OWLAPI-enableIncrementalUpdates| kb-name))
      (|OWLAPI-enableMemorySavingMode| kb-name kb-name))
    
    (|OWLAPI-autoAddAxiomsTo| kb-name kb-name)

    (when maintain-owlapi-axioms
      (|OWLAPI-loadOntology| kb-name kb-name))

    (setf (find-kb-descriptor kb-name) (make-kb-descriptor kb-name))
    (when name (push (list kb-name name) *public-kbs*))))           

(defun ol-as-ontology (expr)
  expr)

(defun ol-as-concept (expr)
  (if (and (consp expr) (eq (first expr) '|OWLClass|))
      (let ((class-name (second expr)))
        (cond ((string= (symbol-name class-name)
                        +owl-thing+)
               'top)
              ((string= (symbol-name class-name)
                        +owl-nothing+)
               'bottom)
              (t class-name)))
    expr))

(defun ol-as-role (expr)
  (cond ((and (consp expr) (eq (first expr) '|ObjectProperty|))
         (second expr))
        ((and (consp expr) (eq (first expr) '|DataProperty|))
         (second expr))
        (t 
         expr)))

(defun ol-as-individual (expr)
  (if (and (consp expr) (eq (first expr) '|Individual|))
      (second expr)
    expr))

(defun ol-as-var-or-ind (expr)
  (if (and (consp expr) (eq (first expr) '|Individual|))
      `(individual ,(second expr))
    `(variable ,(second expr))))

(defun ol-as-literal (string &optional (datatype (owllink-as-symbol !xsd:string)))
  `(d-literal ,(string-trim '(#\Newline #\Return #\Tab #\Space) string)
              ,(ol-as-datatype datatype)))

(defun ol-as-datatype (datatype)
  (if (and (consp datatype) (eq (first datatype) '|Datatype|))
      `(d-base-type ,(intern (second datatype) :racer))
      `(d-base-type ,(intern datatype :racer))))

(defun ol-as-facet (expr) expr)

(defun ol-as-datarange (expr) 
  (if (and (consp expr) (eq (first expr) '|Datatype|))
      (second expr)
    expr))

(defun ol-as-boolean (expr &optional (default nil))
  (cond ((string-equal expr "false")
         nil)
        ((string-equal expr "true")
         t)
        ((null expr)
         default)
        (t 
         (error 'syntax-error :argument (format nil "Invalid boolean expresssion: ~A" expr)))))

;;; ======================================================================

(defun ol-object-property-p (role)
  (let ((role-info (gethash role (tbox-role-axioms-index *current-tbox*))))
    (if (null role-info)
        nil
      (not (role-info-datatype role-info)))))

(defun ol-datatype-property-p (role)
  (let ((role-info (gethash role (tbox-role-axioms-index *current-tbox*))))
    (if (null role-info)
        nil
      (role-info-datatype role-info))))


;;; ======================================================================

(defun collect-stack-elements (&optional (entity-annotation nil))
  (if entity-annotation
      (let ((annotation (pop-stack *stack*))
            (entity (pop-stack *stack*)))
        (multiple-value-bind (elements annotations)
            (collect-stack-elements)
          (values (list* annotation entity elements) annotations)))
    (loop for element = (pop-stack *stack*)
          until (eq element 'end)
          if (and (consp element) (eq (first element) '|Annotation|))
          collect element into annotations
          else
          collect element into elements
          finally (return (values (nreverse elements) annotations)))))


(defun handle-annotations (mode kb annotations owlapi-axiom)
  (declare (ignore mode))
  (unless (kb-descriptor-ignore-annotations (find-kb-descriptor kb))
    (loop for annotation in annotations do
          (|OWLAPI-getOWLAxiomAnnotationAxiom| 
           owlapi-axiom
           annotation))))

;; ======================================================================

(defclass syntax-analyzer
          (dig2-racer)
  ())

(defmethod nox:start-element ((self syntax-analyzer) (tag nox:open-tag) mode)
  (declare (ignore mode))
  ;;(print tag)
  (with-error-handling 
   (let ((token-string (nox:token-string tag))
         (oname (nox:tag-original-name tag)))
     (cond ((string= +rdf-tag+ token-string)
            (throw 'language :rdf-xml))
           #+:ignore
           ((string= (concatenate 'string +owl-version+ "Ontology") token-string)
            (throw 'language :owl-xml))
           ((string= oname "Ontology") ; relaxed
            (throw 'language :owl-xml))
           ((string= (concatenate 'string +owl-version+ "Ontology") token-string)
            (throw 'language :owl-xml))
           ((string= (concatenate 'string +owllink-url-prefix+ "RequestMessage") token-string)
            (throw 'language :owllink))
           (t (throw 'language :dont-know))))
   (error (c) 
          (declare (ignore c))
          (throw 'language :dont-know))))

#+:cl-http
(defun get-syntax-from-document (url-spec &optional (verbose nil))
  ;; We need to extend this to deal with Allegro Aserve!!
  (let* ((url-spec (clean-url url-spec))
	 (url-spec (url-convenience-transformation url-spec))
         (real-url-spec (check-for-url-mirror url-spec))
         (real-url-spec (clean-url real-url-spec))
	 (url (url:intern-url real-url-spec))
         (*silent* t)
         (*response-end-tag* nil))
    (etypecase url 
      (url::file-url 
       (when verbose
         (unless (string= url-spec real-url-spec)
           (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
       (get-syntax-from-file (pathname-from-url url)))
      (url:http-object 
       (when verbose
         (unless (string= url-spec real-url-spec)
           (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
       (catch 'language
         (let ((found nil))
           (with-temp-file ((stream)
                            (setf found (http:show-url url :stream stream)))
             (if (eq found :not-found)
                 (error "HTTP request for ~A returned code 404" url-spec)
               (nox:parse-from-stream stream url 
                                      'nox:xml-parser 
                                      :consumer (make-instance 'syntax-analyzer 
                                                               :url url
                                                               :stream stream))))))))))


#+:aserve
(defun get-syntax-from-document (url-spec &optional verbose)
  ;; We need to extend this to deal with Allegro Aserve!!
  (let* ((url-spec (clean-url url-spec))
         (url-spec (url-convenience-transformation url-spec))
	 (real-url-spec (check-for-url-mirror url-spec))
         (real-url-spec (clean-url real-url-spec))
         (url (net.uri:parse-uri real-url-spec))
         (*silent* t)
         (*response-end-tag* nil))
    (let ((*indent* 0))
      (ecase (net.uri:uri-scheme url) 
	(:file 
	 (when verbose
	   (unless (string= url-spec real-url-spec)
	     (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
	 (let ((pathname (uri-to-pathname url)))
           (when verbose
             (unless (string= url-spec real-url-spec)
               (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
           (get-syntax-from-file pathname)))
	(:http
	 (when verbose
	   (unless (string= url-spec real-url-spec)
	     (format t "~V@TURL ~A mirrored to ~A." *indent* url-spec real-url-spec)))
	 (catch 'language
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
		    
             #+(or :ccl :lispworks) (setq body (deflate body headers verbose))
		    
	     (cond ((not (eql 200 res))
		    (error "HTTP request for ~A returned code ~s" url-spec res))
			  
		   (t 
			   
		    (with-input-from-string (stream body)
		  
		      (nox:parse-from-stream stream url 
					     'nox:xml-parser 
					     :consumer (make-instance 'syntax-analyzer 
							 :url url
							 :stream stream))))))))))))

(defun get-syntax-from-file (filename)
  (let ((*silent* t)
        (*response-end-tag* nil))
    (catch 'language
      (nox:parse-from-file filename 
                           'nox:xml-parser 
                           :consumer (make-instance 'syntax-analyzer
                                                    :stream *standard-output*)))))
