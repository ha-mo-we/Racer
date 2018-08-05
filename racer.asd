;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

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

(in-package :cl-user)

(let ((translation (concatenate 'string 
                                (directory-namestring *load-pathname*)
                                "**/*.*")))
  (setf (logical-pathname-translations "racer")
    `(("**;*.*.*" ,translation))))

;;; ========================================================================================

;;;
;;; For debugging nRQL-Processes activate either #+:nrql-dev or #+:ensure-nrql-debugging 
;;;

;;(pushnew :ensure-nrql-debugging *features*)

;;; For debugging active #+:debug

;;(pushnew :debug *features*)
 
(pushnew :racer-server *features*) ; Can't go without
(pushnew :racer *features*)

;;; (push :sequential-query-scheduling *features*)

(pushnew :onlinedoc *features*)

(pushnew :owl-datatype-support *features*)

#+(and (not :nrql-dev) (not :ensure-nrql-debugging)) 
(pushnew :nrql-error-handler *features*)
;; In a Racer executable, #+:nrql-error-handler must hold such that errors are handled correctly
;; with resources being freed and process pools being managed correctly.

#+(and (not :ensure-nrql-debugging)
       (or :lispworks4 :lispworks5 :lispworks6 :allegro :ccl) 
       (not :allegro-v9.0)) 
(pushnew :multiprocess-queries *features*)

#+(and (not :ensure-nrql-debugging)
       (or :lispworks4 :lispworks5 :lispworks6 :allegro :ccl)
       (not :dlmaps)
       (not :lispworks6.1))
(pushnew :process-pooling *features*)

#-:dlmaps
(pushnew :only-runtime-evaluation *features*)

(when (eq (readtable-case *readtable*) :preserve)
  (pushnew :mlisp *features*))

#+:allegro
(pushnew ':aserve *features*)

#+(and :lispworks (not :application-generation))
(pushnew :tracer-window *features*)


;;; ========================================================================================


#+:lispworks
(require "comm") ; For some reason, Lispworks does not find "comm" via the ASDF system definition.

#+(and :allegro :smp-macros)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :smputil))

#+:lispworks
(require "asdf")

#-:asdf3
(when (< (read-from-string (asdf:asdf-version)) 2.32)
  (error "Racer needs at least ASDF 2.32"))

#+:lispworks 
(setf asdf::*default-encoding* :default)

;;; ========================================================================================

(asdf:defsystem "racer"
  :description "Description Logic Reasoner"
  :author "Volker Haarslev, Ralf Moeller, Michael Wessel"
  :mailto "haarslev@cs.concordia.ca, moeller@uni-luebeck.de, miacwess@gmail.com"
  :license "BSD 3"
  :homepage "http://www.racer-systems.com"
  :pathname #P"racer:source;"
  :serial t
  :depends-on (#-:abcl (:require "aserve") 
               #-:allegro (:require "flexi-streams")
               #-:allegro (:require "deflate")
               #+:allegro (:require "uri")
               #+:allegro (:require "inflate")
               #+:allegro (:require "streama"))
  :components (#+:abcl (:file "abcl-patches")
               #+:sbcl (:file "sbcl-patches")
               #+:allegro (:file "acl-patches")
               #+:ccl (:file "ccl-patches")
               (:module "persistence"
                        :pathname #P"racer:source;"
                        :components ((:file "persistence-package")
                                     (:file "racer-persistence")))
               (:file "nrql-symbols")
               (:file "racer-package")
               (:file "version")
               (:file "readtable")
               (:module "nrql-packages"
                        :pathname #P"racer:source;"
                        :components (#-:dlmaps 
                                     (:file "nrql-packages")
                                     #+:dlmaps  
                                     (:file "packages")
                                     (:file "owlapi-package")
                                     (:file "owl-syntaxes-package")))
               (:file "racer-user-package")
               (:file "progress")
	       (:file "processes")
               (:module "wilbur"
                        :pathname #P"racer:source;"
                        :components ((:file "nox-package")
                                     (:file "core-constants")
                                     (:file "xml-util")
                                     (:file "xml-parser")
                                     (:file "wilbur-package")
                                     (:file "data")
                                     (:file "literal")
                                     (:file "rdf-parser")
                                     (:file "http")
                                     (:file "data-sources")
                                     (:file "wilbur-ql")))
               #+(and :lispworks (or :macosx :mswindows :linux))
               (:module "trace"
                        :pathname #P"racer:source;"
                        :components ((:file "tracer-interface")
                                     (:file "tracer")))
               (:file "trace-support")
               (:file "warnings")
               (:module "racer-kernel"
                        :pathname #P"racer:source;"
                        :components ((:file "variables-support")
                                     (:file "dl-language")
                                     (:file "dl-descriptor-persistence")
                                     (:file "set-structure")
                                     (:file "structures-id")
                                     (:file "concept-structures")
                                     (:file "role-structures")
                                     (:file "individual-structures")
                                     (:file "subgraph-structure")
                                     (:file "concrete-domains")
                                     (:file "termination")
                                     (:file "racer-parameters")
                                     (:file "kernel-state-structures")
                                     (:file "disjunct-statistics")
                                     (:file "tbox-structure")
                                     (:file "racer-statistics")
                                     (:file "copy-support")
                                     (:file "kernel-structures")
                                     (:file "set-operations")
                                     (:file "racer-utilities")
                                     (:file "sparse-array")
                                     (:file "solver-structures")
                                     (:file "constraint-store")
                                     (:file "relation-store")
                                     (:file "label-info-structures")
                                     (:file "dependency-utilities")
                                     (:file "models")
                                     (:file "subset-cache")
                                     (:file "encoding")
                                     (:file "clash-handling")
                                     (:file "lookahead")
                                     (:file "backtrack-stack")
                                     (:file "signatures")
                                     (:file "cd-satisfiable")
                                     (:file "racer")))
               (:file "gomory")
               (:module "racer-tools"
                        :pathname #P"racer:source;"
                        :components ((:file "gci-absorption-structures")
                                     (:file "gci-absorption")
                                     (:file "linear-inequations")
                                     (:file "lin-int-inequations")
                                     (:file "nonlin-inequations")
                                     (:file "string-inequations")
                                     (:file "divisible")
                                     (:file "tbox")
                                     (:file "abox")
                                     (:file "kb")
                                     (:file "query-optimizer")
                                     (:file "gen-model")
                                     (:file "timeout")))
               #+:lispworks
               (:file "lw-inspector")
               (:file "num-restr")
               (:file "simplex-support")
               #+(or :lispworks :ccl)
               (:module "aserve"
                        :pathname #P"racer:source;"
                        :components ((:file "aserve-config")))
               #+:allegro
               (:module "aserve"
                        :pathname #P"racer:source;"
                        :components ((:file "aserve-config")
                                     (:file "aserve-extensions")))
               (:file "server-functions")
               (:file "internal-ind-queries")
               (:file "utils")
               (:file "critical-section")
               (:module "nrql"
                        :pathname #P"racer:source;"
                        :components (#+:dlmaps 
                                     (:file "nrql-symbols")
                                     #-:dlmaps (:file "tools")
                                     (:file "specials")
                                     (:file "messages")
                                     (:file "process")
                                     (:file "process-pool")
                                     (:file "macros")
                                     (:file "dag")
                                     #-:dlmaps (:file "common")
                                     #-:dlmaps (:file "basic-substrate")
                                     #-:dlmaps (:file "basic-descriptions")
                                     #-:dlmaps (:file "racer-conversions")
                                     #-:dlmaps (:file "rolebox")
                                     #-:dlmaps (:file "tables")
                                     #+:dlmaps (:file "logic-tools")
                                     #+:dlmaps (:file "racer-substrate")
                                     (:file "query")
                                     (:file "nrql-queries")
                                     (:file "syntax")
                                     (:file "parser")
                                     (:file "dispatcher")
                                     (:file "syntactic-sugar")
                                     (:file "syntactic-rewriting")   
                                     #+:dlmaps (:file "query-clustering")
                                     (:file "query-realizer")
                                     (:file "tuple-construction")
                                     (:file "compiler")
                                     (:file "caching-code")
                                     (:file "abox-queries-code")
                                     #+:dlmaps (:file "runtime")
                                     (:file "optimizer")
                                     (:file "reasoning")
                                     (:file "hooks")
                                     (:file "preparation")
                                     (:file "execution")
                                     (:file "get-next-tuple")
                                     (:file "interface")
                                     #-:midelora (:file "racer-critical-functions")
                                     (:file "api")
                                     #+:midelora (:file "midelora-api")
                                     (:file "expressions")
                                     #+:racer-server (:file "xml-trafo")
                                     (:file "repository")
                                     (:file "cache-references")
                                     (:file "dl-prover-interface")
                                     (:file "defined-queries")
                                     #+:clim (:file "browser")
                                     #+(and (not :dlmaps) (not :midelora)) (:file "data-substrate")
                                     #+(and (not :dlmaps) (not :midelora)) (:file "mirror-data-substrate")
                                     #+(and (not :dlmaps) (not :midelora)) (:file "rcc-substrate")
                                     #+:sql-substrate (:file "sql-substrate")
                                     (:file "subscribe")
                                     (:file "persistence")
                                     (:file "nrql-version")))
               (:module "owlapi"
                        :pathname #P"racer:source;"
                        :components ((:file "owlapi-tools")
                                     (:file "reasoners")
                                     (:file "owlapi")
                                     (:file "owlapi-synonyms")
                                     (:file "owlapi-reasoner-bridge")
                                     (:file "owlapi-persistence")))
               (:file "rules")
               (:file "individual-queries")
               (:file "benchmark-support")
               (:file "storage-management")
               ;; #+:racer-with-sirius
               ;; sirius
               #+(or :ccl :lispworks :allegro)
               (:file "racer-server")
	       #+(or :abcl :sbcl)
	       (:file "ersatz-racer-server")
               #+(or :ccl :lispworks :allegro)
               (:file "nrql-server-case")
               (:file "racer-interface")
               (:file "xml-interface")
               (:file "owl-parser")
               (:file "owl-interface")
               (:file "swrl-interface")
               (:file "krss-owl-export")
               (:file "dig-interface")
               (:file "owllink-interface")
               ;; (:file "print-version")
               (:file "initialize-default-kb")
               (:file "nonstandard-inferences")
               (:module "owl-syntaxes"
                        :pathname #P"racer:source;"
                        :components ((:file "http-stream")
                                     (:file "owl-import")
                                     #-:abcl (:file "owl-functional") ; It seems the ABCL compiler has a bug, it cannot compile this file.
                                     (:file "functional-renderer")
                                     (:file "owllink-parsers")
                                     (:file "owllink-functional")
                                     (:file "owllink-reasoner-bridge")
                                     (:file "xml-renderer")
                                     (:file "owl-renderer")
                                     (:file "owllink-xml")
                                     (:file "import-export")
                                     (:file "owllink-converter")))
               (:file "abduction")
               (:file "abox-diff")
               (:module "online-doc"
                        :pathname #P"racer:source;"
                        :components (#+(and :nrql-dev :lispworks :linux) 
                                       (:file "lambda")
                                       #+(not (and :nrql-dev :lispworks :linux))
                                       (:file "lambda-registry")
                                       (:file "primhtml")
                                       (:file "docgen")))
               #+:nrql-dev
               (:module "stub-generator"
                        :pathname #P"racer:source;"
                        :components ((:file "stub-generation")
                                     (:file "documentation")))
               #+:nrql-dev
               (:file "nrql-delivery")
               #-:nrql-dev
               (:file "docgen-stubs")
               (:file "setup-readtable")))
