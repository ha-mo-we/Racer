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

(in-package racer)

(defun xml-define-tbox (name)
  (find-tbox 
   (in-tbox-internal name
                     t
                     nil
                     *default-tbox-concept-size* 
                     *default-tbox-role-size*)))

(defun xml-define-abox (abox-name tbox-name)
  (find-abox (in-abox-internal abox-name tbox-name t)))

(defun xml-define-concept (name tbox)
  (add-concept-axiom tbox name nil :inclusion-p t))

(defun xml-add-implication (concept-1 concept-2 tbox)
  (add-concept-axiom tbox concept-1 concept-2 :inclusion-p t))

(defun xml-add-equation (concept-1 concept-2 tbox)
  (add-concept-axiom tbox concept-1 concept-2 :inclusion-p nil))


;;; ======================================================================

(defparameter *stack* nil)

;;; ======================================================================


(defclass xml-racer 
  (nox-racer:sax-consumer)
  ((tbox :initform nil :accessor xml-racer-tbox)
   (tbox-default-name :initarg :tbox-default-name :initform nil :accessor xml-racer-tbox-default-name)))

(defmethod start-document ((self xml-racer) locator)
  (declare (ignorable self locator))
  ;(format t "~&START DOCUMENT ~S" locator)
  )

(defun find-attribute (name attributes &optional (intern t))
  (let ((value (cdr (assoc (string-upcase (symbol-name name))
                           attributes :test #'string-equal))))
    (if (and intern (stringp value))
      (intern value)
      value)))

(defmethod nox-racer:start-element ((self xml-racer) (tag nox-racer:open-tag) mode)
  (let ((token-string (nox-racer:token-string tag))
        (attributes (nox-racer:tag-attributes tag)))
    ;;(format t "~&START ~A ~S ~S" token-string attributes mode)
    (cond ((string-equal token-string "knowledgebase") 
           (setf (xml-racer-tbox self)
                 (xml-define-tbox (or (find-attribute 'name attributes)
                                      (xml-racer-tbox-default-name self)))))
          ((string-equal token-string "defrole")
           (ensure-role (find-attribute 'name attributes) (xml-racer-tbox self)))
          ((string-equal token-string "defconcept")
           (xml-define-concept (find-attribute 'name attributes) (xml-racer-tbox self)))
          ((string-equal token-string "primrole")
           (push (find-attribute 'name attributes) *stack*))
          ((string-equal token-string "primitive")
           (push (find-attribute 'name attributes) *stack*))
          ((string-equal token-string "impliesr")
           (setf *stack* nil))
          ((string-equal token-string "equalr")
           (setf *stack* nil))
          ((string-equal token-string "impliesc")
           (setf *stack* nil))
          ((string-equal token-string "equalc")
           (setf *stack* nil))
          ((string-equal token-string "transitive")
           (setf *stack* nil))
          ((string-equal token-string "functional")
           (setf *stack* nil))
          ((string-equal token-string "role"))
          ((string-equal token-string "concept"))
          ((string-equal token-string "and")
           (push 'end *stack*))
          ((string-equal token-string "or")
           (push 'end *stack*))
          ((string-equal token-string "some"))
          ((string-equal token-string "all"))
          ((string-equal token-string "top"))
          ((string-equal token-string "bottom"))
          ((string-equal token-string "not"))
          ((string-equal token-string "invrole"))
          ((string-equal token-string "atmost")
           (push (find-attribute 'num attributes nil) *stack*))
          ((string-equal token-string "atleast")
           (push (find-attribute 'num attributes nil) *stack*))
          (t (format t "~&Ignoring START ~A ~S ~S" token-string attributes mode)))))

(defmethod nox-racer:end-element ((self xml-racer) tag mode)
  (let ((token-string (nox-racer:token-string tag)))
    ;;(format t "~&END ~A ~S" token-string mode)
    (cond ((string-equal token-string "knowledgebase"))
          ((string-equal token-string "defrole"))
          ((string-equal token-string "defconcept"))
          ((string-equal token-string "primrole"))
          ((string-equal token-string "primitive"))
          ((string-equal token-string "impliesr")
           (role-has-parent (second *stack*) (first *stack*) (xml-racer-tbox self)))
          ((string-equal token-string "equalr")
           (let ((role2 (first *stack*))
                 (role1 (second *stack*))
                 (tbox (xml-racer-tbox self)))
             (ensure-role role1 tbox)
             (ensure-role role2 tbox)
             (cond ((and (symbolp role1)
                         (consp role2) 
                         (eq (first role2) 'inv)) 
                    (inverse-of-role role1 (second role2) tbox))
                   ((and (symbolp role2)
                         (consp role1) 
                         (eq (first role1) 'inv))
                    (inverse-of-role role2 (second role1) tbox))
                   ((and (consp role1) 
                         (eq (first role1) 'inv)
                         (consp role2)
                         (eq (first role2) 'inv))
                    (roles-equivalent-1 (second role1) (second role2) tbox))
                   (t (roles-equivalent-1 role1 role2 tbox)))))
          ((string-equal token-string "impliesc")
           (xml-add-implication (second *stack*) (first *stack*) (xml-racer-tbox self)))
          ((string-equal token-string "equalc")
           (xml-add-equation (second *stack*) (first *stack*) (xml-racer-tbox self)))
          ((string-equal token-string "transitive")
           (role-is-transitive (first *stack*) (xml-racer-tbox self)))
          ((string-equal token-string "functional")
           (role-is-functional (first *stack*) (xml-racer-tbox self)))
          ((string-equal token-string "role"))
          ((string-equal token-string "concept"))
          ((string-equal token-string "and")
           (let ((conjuncts (loop for top in *stack*
                                  until (eq top 'end)
                                  collect top
                                  do (pop *stack*))))
             (pop *stack*)
             (push `(and .,conjuncts) *stack*)))
          ((string-equal token-string "or")
           (let ((conjuncts (loop for top in *stack*
                                  until (eq top 'end)
                                  collect top
                                  do (pop *stack*))))
             (pop *stack*)
             (push `(or .,conjuncts) *stack*)))
          ((string-equal token-string "some")
           (let ((concept (pop *stack*))
                 (role (pop *stack*)))
             (push `(some ,role ,concept) *stack*)))
          ((string-equal token-string "all")
           (let ((concept (pop *stack*))
                 (role (pop *stack*)))
             (push `(all ,role ,concept) *stack*)))
          ((string-equal token-string "not")
           (let ((concept (pop *stack*)))
             (push `(not ,concept) *stack*)))
          ((string-equal token-string "invrole")
           (let ((concept (pop *stack*)))
             (push `(inv ,concept) *stack*)))
          ((string-equal token-string "top")
           (push 'top *stack*))
          ((string-equal token-string "bottom")
           (push 'bottom *stack*))
          ((string-equal token-string "atmost")
           (let ((concept (pop *stack*))
                 (role (pop *stack*))
                 (num (read-from-string (pop *stack*))))
             (push `(at-most ,num ,role ,concept) *stack*)))
          ((string-equal token-string "atleast")
           (let ((concept (pop *stack*))
                 (role (pop *stack*))
                 (num (read-from-string (pop *stack*))))
             (push `(at-least ,num ,role ,concept) *stack*)))
          (t (format t "~&Ignoring END ~A ~S" token-string mode)))))

(defmethod nox-racer:char-content ((self xml-racer) char-content mode)
  (declare (ignore mode))
  (string-trim '(#\Space #\Tab #\Newline) char-content))

(defmethod end-document ((self xml-racer) mode)
  (declare (ignore mode))
  ;(format t "~&END DOCUMENT ~S" mode)
  )

(defmethod proc-instruction ((self xml-racer) (tag nox-racer:proc-instruction) mode)
  (format t "~&Ignoring: PI ~S ~S" (nox-racer:token-string tag) mode)
  ;(format t "~&PI ~S ~S" (nox-racer:token-string tag) mode)
  )

;;; ======================================================================

(defun xml-read-tbox-file (filename)
  (let ((result
         (nox-racer:parse-from-file filename 'nox-racer:xml-parser 
                              :consumer (make-instance 'xml-racer 
                                          :tbox-default-name 
                                          (intern (string-upcase (pathname-name filename)))))))
    (tbox-name (xml-racer-tbox (nox-racer:sax-producer-consumer result)))))

;;; ======================================================================

(defun print-xml-tbox (tbox stream)
  (check-type tbox (or symbol tbox))
  (setf tbox (find-tbox tbox))
  (format stream "<?xml version=\"1.0\"?>~
                  ~%<!DOCTYPE KNOWLEDGEBASE SYSTEM \"fact.dtd\"> ~
                  ~%<KNOWLEDGEBASE>")
  (loop for (role-name nil feature-p transitive-p parents inverse nil nil nil)
        in (reverse (tbox-role-axioms tbox))
        do (xml-print-role-declarations stream role-name feature-p transitive-p parents inverse))
  (loop for disjoint-set being the hash-value of (tbox-disjoint-set tbox) do
        (xml-print-disjoint-axiom stream disjoint-set))
  (loop for (name definition primitive) in (reverse (tbox-original-concept-axioms tbox)) do
        (unless (and primitive (disjoint-and-p definition))
          (xml-print-concept-definition stream
                                        name
                                        definition
                                        primitive)))
  (loop for (role-name nil nil nil nil nil nil domain range)
        in (reverse (tbox-role-axioms tbox))
        do (xml-print-domain-and-range-declarations stream role-name domain range))
  (loop for (role-name-1 . role-name-2) in (tbox-role-synonyms tbox) do
        (xml-print-roles-equivalent-declaration stream role-name-1 role-name-2))
  (format stream "~%</KNOWLEDGEBASE>"))

(defun xml-print-roles-equivalent-declaration (stream role-name-1 role-name-2)
  (format stream "~%<EQUALR>~
	                ~%  <ROLE>~
		        ~%    <PRIMROLE NAME=\"~A\"/>~
	                ~%  </ROLE>~
	                ~%  <ROLE>~
		        ~%    <PRIMROLE NAME=\"~A\"/>~
	                ~%</ROLE>~
                        ~%</EQUALR>"
                role-name-1 
                role-name-2))

(defun xml-print-role-declarations (stream role-name feature-p transitive-p parents inverse)
  (format stream "~%<DEFROLE NAME=\"~A\"/>" role-name)
  (loop for parent in parents do
        (format stream "~%<IMPLIESR>~
	                ~%  <ROLE>~
		        ~%    <PRIMROLE NAME=\"~A\"/>~
	                ~%  </ROLE>~
	                ~%  <ROLE>~
		        ~%    <PRIMROLE NAME=\"~A\"/>~
	                ~%</ROLE>~
                        ~%</IMPLIESR>"
                role-name 
                parent))
  (if feature-p
    (format stream "~%<FUNCTIONAL>~
                    ~%  <ROLE>~
	            ~%    <PRIMROLE NAME=\"~A\"/>~
                    ~%  </ROLE>~
                    ~%</FUNCTIONAL>"
            role-name))
  (if transitive-p
    (format stream "~%<TRANSITIVE>~
                    ~%  <ROLE>~
	            ~%    <PRIMROLE NAME=\"~A\"/>~
                    ~%  </ROLE>~
                    ~%</TRANSITIVE>"
            role-name))
  (if inverse
    (error "TBoxes with inverse declarations cannot be saved in XML format.")))


(defun xml-print-concept-definition (stream name definition primitive)
  (format stream "~%<DEFCONCEPT NAME=\"~A\"/>" name)
  (if definition
    (cond (primitive
           (format stream "~%<IMPLIESC>~
	                   ~%  <CONCEPT>~
		           ~%     <PRIMITIVE NAME=\"~A\"/>~
                           ~%  </CONCEPT>~
	                   ~%  <CONCEPT>~
                           ~%     "
                   name)
           (xml-format-concept stream definition)
           (format stream "~%  </CONCEPT>~
                           ~%</IMPLIESC>"))
          (t (format stream "~%<EQUALC>~
	                     ~%  <CONCEPT>~
		             ~%     <PRIMITIVE NAME=\"~A\"/>~
                             ~%  </CONCEPT>~
	                     ~%  <CONCEPT>~
                             ~%     "
                     name)
             (xml-format-concept stream definition)
             (format stream "~%  </CONCEPT>~
                             ~%</EQUALC>")))))

(defun xml-format-concept (stream concept)
  (if (symbolp concept)
    (format stream "<PRIMITIVE NAME=\"~A\"/>" concept)
    (case (first concept)
      (not (format stream "<NOT> ")
           (xml-format-concept stream (second concept))
           (format stream " </NOT>"))
      (and (format stream "<AND> ")
           (loop for concept1 in (rest concept) do
                 (xml-format-concept stream concept1))
           (format stream " </AND>"))
      (or (format stream "<OR> ")
          (loop for concept1 in (rest concept) do
                (xml-format-concept stream concept1))
          (format stream " </OR>"))
      (all (format stream "<ALL> ")
           (xml-format-role stream (second concept))
           (xml-format-concept stream (third concept))
           (format stream "</ALL>"))
      (some (format stream "<SOME> ")
            (xml-format-role stream (second concept))
            (xml-format-concept stream (third concept))
            (format stream "</SOME>"))
      (at-least (format stream "<ATLEAST NUM=\"~A\"> " (second concept))
                (xml-format-role stream (third concept))
                (if (fourth concept)
                  (xml-format-concept stream (fourth concept)))
                (format stream "</ATLEAST>"))
      (at-most (format stream "<ATMOST NUM=\"~A\"> " (second concept))
               (xml-format-role stream (third concept))
               (if (fourth concept)
                 (xml-format-concept stream (fourth concept)))
               (format stream "</ATMOST>"))
      (exactly 
       (let ((restrictions (if (fourth concept)
                             (list (second concept) (third concept) (fourth concept))
                             (list (second concept) (third concept))))) 
         (xml-format-concept stream `(and (at-least .,restrictions)
                                         (at-most .,restrictions)))))
      (otherwise (error "Cannot save TBox with ~A in XML format."
                        concept)))))
      
(defun xml-format-role (stream role)
  (if (symbolp role)
    (format stream "<PRIMROLE NAME=\"~A\"/>" role)
    (format stream "<INVROLE> <PRIMROLE NAME=\"~A\"/> </INVROLE>" role)))

(defun xml-print-disjoint-axiom (stream concepts)
  (loop for concepts1 on concepts 
        when (rest concepts1) do
        (let ((concept (first concepts1)))
          (format stream "~%<IMPLIESC>~
                          ~%  <CONCEPT>~
                          ~%     <PRIMITIVE NAME=\"~A\"/>~
                          ~%  </CONCEPT>~
                          ~%  <CONCEPT>~
                          ~%     "
                  concept)
          (xml-format-concept stream `(not (or .,(rest concepts1))))
          (format stream "~%  </CONCEPT>~
                          ~%</IMPLIESC>"))))

(defun xml-print-domain-and-range-declarations (stream role-name domain range)
  (when domain
    (format stream "~%<IMPLIESC>~
                    ~%  <CONCEPT>~
                    ~%     <SOME> <PRIMROLE NAME=\"~A\"/> <TOP/> </SOME>~
                    ~%  </CONCEPT>~
                    ~%  <CONCEPT>~
                    ~%     "
            role-name)
    (xml-format-concept stream domain)
    (format stream "~%  </CONCEPT>~
                    ~%</IMPLIESC>"))
  (when range 
    (format stream "~%<IMPLIESC>~
                    ~%  <CONCEPT>~
                    ~%     <TOP/>~
                    ~%  </CONCEPT>~
                    ~%  <CONCEPT>~
                    ~%     <ALL> <PRIMROLE NAME=\"~A\"/> "
            role-name)
    (xml-format-concept stream range)
    (format stream "~%     </ALL>~
                    ~%  </CONCEPT>~
                    ~%</IMPLIESC>")))


(defun print-wilbur-info (&optional (stream t))
  (format stream ";;; The XML/RDF/RDFS/OWL parser is implemented with Wilbur developed~%~
                  ;;; by Ora Lassila. For more information on Wilbur see ~%~
                  ;;; http://wilbur-rdf.sourceforge.net/.~2%"))

