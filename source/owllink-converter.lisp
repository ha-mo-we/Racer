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
;;;;  owllink-converter.lisp
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
;;;   Purpose: A converter between the three different OWLlink syntaxes. 
;;;            And an OWLlink batch processor. 
;;; 

(defun owllink-converter (directory &key
                                    (from :owllink-xml) (to :owllink-functional)
                                    (only-requests-p t))
  (let ((owl-syntaxes:*owllink2-input-syntax* from)
        (owl-syntaxes:*owllink2-output-syntax* to)
        (*converter-mode* t))    

    (dolist (file (directory (format nil 
                                     (if only-requests-p 
                                         "~A/*request*.~A"
                                       "~A/*.~A")
                                     directory
                                     (case from 
                                       ((:xml :owllink-xml) "xml")
                                       ((:owllink-functional) "funct")
                                       ((:owllink-sexpr) "sexpr")))))
      (when (=> only-requests-p 
                (not (search "response" (namestring file))))
        (let ((file2 (format nil "~A/~A.~A.~A" 
                             directory
                             (pathname-name file)
                             (pathname-type file)
                             (case to
                               ((:xml :owllink-xml) "xml")
                               ((:owllink-functional) "funct")
                               ((:owllink-sexpr) "sexpr")))))

          (format t "~%~%***********************************************************~%")
          (format t "~A~%   ==>~%~A~%~%(~A ==> ~A)~%" 
                  (namestring file) (namestring file2) from to) 

          (full-reset)
          (owllink-read-file1 file
                              :input-syntax from
                              :output-syntax to)

          ;; (pprint (result *my-owllink2-parser*))

          (with-open-file (stream file2 :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
            (let ((*standard-output* stream))
              (full-reset)
              (owllink-read-file1 file
                                  :input-syntax from
                                  :output-syntax to))))))))

(defun convert-all (directory &optional (only-requests-p t))
  (dolist (output-syntax '(;:owllink-xml
                           :owllink-functional
                           :owllink-sexpr))
    (owllink-converter directory 
                       :to output-syntax
                       :only-requests-p only-requests-p)))

(defun process-all (directory &optional output-syntax)
  (let (#+:racer-server 
        (*tbox-verbose* nil)
        #+:racer-server 
        (*abox-verbose* nil))
    (dolist (from '(:owllink-xml
                    :owllink-functional
                    :owllink-sexpr))
      (let ((to
             (or output-syntax from)))
        (dolist (file (directory (format nil 
                                         "~A/*request*.~A"
                                         directory
                                         (case from 
                                           ((:xml :owllink-xml) "xml")
                                           ((:owllink-functional) "funct")
                                           ((:owllink-sexpr) "sexpr")))))
          (unless (search "response" (namestring file))
            (let ((file2 (format nil "~A.response.~A" 
                                 (namestring file)
                                 (case to
                                   ((:xml :owllink-xml) "xml")
                                   ((:owllink-functional) "funct")
                                   ((:owllink-sexpr) "sexpr")))))
              (with-open-file (stream file2
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)

                (format t "~%~%***********************************************************~%")
                (format t "~A~%   ==>~%~A~%~%(~A ==> ~A)~%" 
                        (namestring file) (namestring file2) from to)

                (full-reset)
                (owllink-read-file1 file
                                    :input-syntax from
                                    :output-syntax to)

                (let ((*standard-output* stream))
                  (full-reset)
                  (owllink-read-file1 file
                                      :input-syntax from
                                      :output-syntax to))))))))))

