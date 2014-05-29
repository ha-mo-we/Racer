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
;;;;  owl-import.lisp
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
;;;   Purpose: Master import function of OWL 2 parser and OWL 2 syntax guesser 
;;; 

(defvar *owllink-mirror-mappings* nil)

(defvar *imported-ontologies* nil)

(defvar *ignore-import-p* nil)

(defvar *import-level* 0)

#+:racer-server
(defun get-syntax-from-document (uri &rest args)
  (declare (ignorable args))
  (get-syntax-from-document 
   uri
   #+:aserve (getf args ':verbose)))

#-:racer-server
(defun get-syntax-from-document (uri &rest args)
  (declare (ignorable args))
  (with-input-from-url (stream uri)
    (loop 
      (let ((line (read-line stream nil nil)))
       (if line
           (let ((res 
                  (cond ((search "<rdf:RDF" line)
                         :owl-rdf)
                        ((search "<Ontology" line)
                         :owl-xml)
                        ((search "Ontology(" line)
                         :owl-functional))))
             (when res
               (return res)))
         (return :unknown))))))

(owlapi::defun1 owlapi-import-ontology (uri &rest args 
                                            &key syntax reasoner-name kb-name &allow-other-keys)
  
  (declare (ignorable args))

  (let* ((uri (if (symbolp uri)
                  (symbol-name uri)
                uri))
         (uri (or 
               (substitute-name uri *owllink-mirror-mappings*)
               uri))
         (reasoner *cur-reasoner*))
     
    (cond ((member (intern (owlapi::ensure-string uri)) *imported-ontologies*)

           #+:racer-server
           (reasoner-redundant-import-warning uri)
           #-:racer-server
           (owlapi-warning "Skipping redundant import: ~A" uri))

          (t

           (labels ((do-it ()

                      (when (or (not *ignore-import-p*)
                                (minusp *import-level*))

                        (unless (minusp *import-level*)
                          (owlapi::push-namespace-table reasoner))
             
                        (let* ((*import-level* (1+ *import-level*))
                               (*imported-ontologies* 
                                (cons (intern (owlapi::ensure-string uri))
                                      *imported-ontologies*))
                               (syntax 
                                (or syntax
                                    (get-syntax-from-document 
                                     uri))))

                          (let ((ontology (|OWLAPI-getAutoOntology| reasoner)))

                            (labels ((syntax-message (message)
                                       #+:racer-server
                                       (reasoner-syntax-message message uri)
                                       #-:racer-server
                                       (owlapi-warning message 0 uri)))

                              (unwind-protect
          
                                  (case syntax
                                    #+:racer-server
                                    ((:owl-rdf :rdf-xml :owl)

                                     (syntax-message "~%~V@TLooks like ontology ~A is in OWL RDF syntax~%")

                                     (apply #'owl-read-document uri 
                                            :kb-name (or kb-name reasoner-name) ; wg. imports
                                            :allow-other-keys t
                                            args))

                                    #-:racer-server
                                    ((:owl-rdf :rdf-xml :owl)

                                     (owlapi-runtime-error "Cannot parse ontology ~A in OWL RDF syntax. Only OWL XML and OWL Functional are supported.~%" uri))

                                    ((:owl-xml :xml :owx)

                                     (syntax-message "~%~V@TLooks like ontology ~A is in OWL XML syntax~%")

                                     (apply #'owlapi-read-xml-ontology uri 
                                            :allow-other-keys t
                                            args))

                                    (t

                                     (syntax-message "~%~V@TAssuming ontology ~A is in OWL Functional syntax~%")

                                     (handler-case
                                         (apply #'owlapi-read-functional-ontology 
                                                uri
                                                :allow-other-keys t
                                                args)
                                       (error (error)
                                         (owlapi-runtime-error "Import of ontology ~A  failed - perhaps this ontology is neither in OWL RDF, OWL XML, nor OWL Functional syntax. Functional parser error was: ~A" uri error)))))
            
                                (let* ((ontology
                                        (if (owlapi::find-owl-ontology ontology nil)  
                                            ;; perhaps ontology was merged with another one 
                                            ;; (due to Import)
                                            ontology
                                          (|OWLAPI-getAutoOntology| reasoner))))

                                  (unless (eq ontology :void)
                                    (|OWLAPI-autoAddAxiomsTo| ontology reasoner)))))))
                                
                        (unless (minusp *import-level*)
                          (owlapi::pop-namespace-table reasoner))

                        (or kb-name reasoner-name))))

             (if (minusp *import-level*)
                 #+:racer-server
               (without-duplicate-warnings (do-it))
                 #-:racer-server
                 (do-it)
               (do-it)))))))
