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
;;;;  import-export.lisp
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
;;;   Purpose: Top-level interface for OWL 2 import and export functionality. 
;;;            

(owlapi:defmethod1 write-owl-file ((ontology owlapi:ontology) (fn pathname) (syntax symbol)
                                    &rest args)
  
  (apply #'write-owl-file ontology (namestring fn) syntax args))
  
(owlapi:defmethod1 write-owl-file ((ontology owlapi:ontology) (fn string) (syntax symbol) 
                                   &key
                                   prefixes
                                   (p4-mode *p4-mode*)
                                   (comments *comments*) 
                                   &allow-other-keys)

  (with-open-file (stream fn
                          :direction :output 
                          :if-exists :supersede
                          :if-does-not-exist :create)
    
    (let* ((*add-prefixes* prefixes)
           (*p4-mode* p4-mode)
           (*comments* comments)
           (axioms
            (render stream ontology syntax)))

      (when
          #+:racer-server
        *tbox-verbose*
        #-:racer-server t
        (format t "~%Rendered ~A axioms." axioms)
        (when (zerop axioms)
          (format t "~%Note that OWLAPI rendering only works if axiom objects were maintained! 
Please make sure that the ontology was loading using 
\"(owlapi-read-ontology <filename> :maintain-owlapi-axioms t)\".")))

      (if (zerop axioms)
          (list fn :warning-no-renderable-axioms-found)
        fn))))

;;;
;;;
;;;

(owlapi:owlapi-defun (|OWLAPI-writeOntologyFile|) (ontology fn &rest args)
		     
		     ;; (new-request *server-request*)

  (#+:racer-server
   without-duplicate-warnings
   #-:racer-server
   progn
   #+:racer-server
   (ts:check-if-unsafe)
   (let ((ont (owlapi:find-owl-ontology ontology)))
     (apply #'write-owl-file ont fn :owl-rdf args))))

(owlapi:owlapi-defun (|OWLAPI-writeFunctionalOntologyFile|) (ontology fn &rest args)

		     ;; (new-request *server-request*)

  (#+:racer-server
   without-duplicate-warnings
   #-:racer-server
   progn
   #+:racer-server
   (ts:check-if-unsafe)
    (let ((ont (owlapi:find-owl-ontology ontology)))
      (apply #'write-owl-file ont fn :owl-functional args))))

(owlapi:owlapi-defun (|OWLAPI-writeXMLOntologyFile|) (ontology fn &rest args)
		     
		     ;; (new-request *server-request*)
		     
  (#+:racer-server
   without-duplicate-warnings
   #-:racer-server
   progn
   #+:racer-server
   (ts::check-if-unsafe)
   (let ((ont (owlapi:find-owl-ontology ontology)))
      (apply #'write-owl-file ont fn :owl-xml args))))

;;;
;;; Export Master
;;;

(owlapi:owlapi-defun (|OWLAPI-saveOntology|) (ontology fn &rest args
                                              &key 
                                              reasoner
                                              (syntax :owl-rdf) 
                                              prefixes
                                              (p4-mode *p4-mode*)
                                              (comments *comments*) 
                                              &allow-other-keys)

  (declare (ignorable prefixes p4-mode comments))
  
  ;; (new-request *server-request*)
  
  (#+:racer-server
   without-duplicate-warnings
   #-:racer-server
   progn
    (with-reasoner (reasoner)
      (ecase syntax
        ((:owl-rdf :rdf-xml :owl)
         (apply #'|OWLAPI-writeOntologyFile| ontology fn args))
        ((:owl-functional :ofn :owf :funct :functional) 
         (apply #'|OWLAPI-writeFunctionalOntologyFile| ontology fn args))
        ((:owl-xml :xml :owx)
         (apply #'|OWLAPI-writeXMLOntologyFile| ontology fn args))))))

;;;
;;; Import Master 
;;;

(owlapi:owlapi-defun (|OWLAPI-readOntology|) (url &rest args
                                         &key 
                                         (ignore-import *ignore-import-p*)
                                         &allow-other-keys)

  (let ((*imported-ontologies* nil)
        (*import-level* -1) ; richtig! 
        (*ignore-import-p* ignore-import))
    
    ;; (new-request *server-request*)

    (if (not (owlapi:is-url-p url))
        (apply #'owlapi-import-ontology (make-url-from-filename url) 
	       :allow-other-keys t
	       args)
      (apply #'owlapi-import-ontology url 
	     :allow-other-keys t
	     args))))

