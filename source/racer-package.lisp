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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *enforce-racer* t))

#+(and :lispworks (or :macosx :mswindows :linux)) 
(unless (find-package :tools)
 (make-package :tools))

#+:allegro
(unless (fboundp 'old-package-nicknames)
  (setf (symbol-function 'old-package-nicknames)
        #'package-nicknames)
  (let ((excl::*enable-package-locked-errors* nil))
    (setf (symbol-function 'package-nicknames)
          #'(lambda (package)
              (reverse (funcall 'old-package-nicknames package))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (not (find ':racer *features*))
      (unless (or (member "RACER" (package-nicknames 'common-lisp-user) :test #'equal)
                  (member "racer" (package-nicknames 'common-lisp-user) :test #'equal))
        #-(or :abcl)
        (progn
          (defpackage common-lisp-user
            (:nicknames :cl-user #|:race|# :racer))
          (shadowing-import '(persistence-manager:defstruct
                                 persistence-manager:defclass)
                            'cl-user)
          (import '(persistence-manager:make-object-persistent
                    persistence-manager:load-persistent-object
                    persistence-manager:print-persistence-manager-info)
                  'common-lisp-user)))
    (defpackage :racer
      (:use common-lisp #+:allegro :excl :nrql-symbols)
      ;;(:nicknames #|:race|# #+:ccl :racer)
      ;;#+:ccl (:import-from :ccl #:ignore-if-unused #:choose-file-dialog)
      (:shadowing-import-from :persistence-manager #:defstruct #:defclass)
      (:import-from :persistence-manager 
       #:make-object-persistent
       #:load-persistent-object
       #:print-persistence-manager-info))))


(export (loop for sym in 
              (append +nrql-symbols+
                      '(#:+top-symbol+
                        #:+bottom-symbol+
                        #:init-tbox
                        #:save-tbox
                        #:delete-tbox
                        #:forget-tbox
                        #:*current-tbox*
                        #:current-tbox
                        #:set-current-tbox
                        #:*tbox-verbose*
                        #:find-tbox
                        #:set-find-tbox
                        #:create-tbox-clone
                        #:clone-tbox
                        #:in-tbox
                        #:in-tbox-internal
                        #:init-abox
                        #:in-abox-internal
                        #:save-abox
                        #:delete-abox
                        #:forget-abox
                        #:*auto-realize*
                        #:*auto-classify*
                        #:*default-tbox-concept-size*
                        #:*default-tbox-role-size*
                        #:*current-abox*
			
                        #:current-abox
                        #:set-current-abox
                        #:*abox-verbose*
                        #:*abox-clash-verbose*
                        #:find-abox
                        #:set-find-abox
                        #:create-abox-clone
                        #:clone-abox
                        #:in-abox
                        #:*top*
                        #:top
                        #:*bottom*
                        #:bottom
                        ;;#:*datatype-top*
                        ;;#:*datatype-bottom*
                        #:*datatype-top-concept*
                        #:*datatype-bottom-concept*
                        #:*top-object-role*
                        #:top-object-role
			#:+top-object-role-symbol+
			#:+bottom-object-role-symbol+
			#:+top-datatype-role-symbol+
			#:+bottom-datatype-role-symbol+
			#:+krss-top-object-role-symbol+
                        #:+krss-bottom-object-role-symbol+

                        #:*bottom-object-role*
                        #:bottom-object-role
                        #:*top-datatype-role*
                        #:*bottom-datatype-role*
                        #:at-least
                        #:at-most
                        #:exactly
                        #:some
                        #:all
			#:self-reference
                        #:has-value
                        #:not
                        #:and
                        #:or
                        #:one-of
                        #:inv
                        #:define-tbox
                        #:define-tbox-1
                        #:signature
                        #:ensure-tbox-signature
                        #:ensure-abox-signature
                        #:get-kb-signature
                        #:get-tbox-signature
                        #:get-abox-signature
                        #:in-knowledge-base
                        #:define-primitive-concept
                        #:defprimconcept
                        #:define-concept
                        #:defconcept
                        #:implies
                        #:equivalent
                        #:disjoint
                        #:define-disjoint-primitive-concept
                        #:add-disjointness-axiom
                        #:add-concept-axiom
                        #:forget-concept-axiom
                        #:forget-disjointness-axiom
                        #:forget-disjointness-axiom-statement
                        #:define-primitive-role
                        #:defprimrole
                        #:define-primitive-attribute
                        #:defprimattribute
                        #:defcdattribute
                        #:define-concrete-domain-attribute
                        #:ensure-cd-attribute
                        #:add-role-axioms
	                ;#:add-role-axiom ;now obsolete?
                        #:define-distinct-individual
                        #:define-individual
                        #:add-individual
                        #:state
                        #:instance
                        #:related
                        #:unrelated
                        #:different-from
                        #:all-different
                        #:same-individual-as
                        #:same-as
                        #:with-unique-name-assumption
                        #:without-unique-name-assumption
                        #:set-unique-name-assumption
                        #:add-concept-assertion
                        #:add-role-assertion
                        #:add-negated-role-assertion
                        #:forget-concept-assertion
                        #:forget-role-assertion
                        #:forget-constrained-assertion
                        #:forget-constraint
                        #:forget
                        #:forget-statement
                        #:concept-satisfiable?
                        #:concept-satisfiable-p
                        #:alc-concept-coherent
                        #:concept-subsumes?
                        #:concept-subsumes-p
                        #:concept-equivalent?
                        #:concept-equivalent-p
                        #:concept-disjoint?
                        #:concept-disjoint-p
                        #:classify-tbox
                        #:check-tbox-coherence
                        #:tbox-coherent-p
                        #:tbox-coherent?
                        #:tbox-classified-p
                        #:tbox-classified?
                        #:tbox-prepared-p
                        #:tbox-prepared?
                        #:tbox-cyclic-p
                        #:tbox-cyclic?
                        #:atomic-concept-descendants
                        #:atomic-concept-ancestors
                        #:atomic-concept-children
                        #:atomic-concept-parents
                        #:atomic-concept-synonyms
                        #:concept-synonyms
                        #:concept-descendants
                        #:concept-offspring
                        #:concept-children
                        #:concept-ancestors
                        #:concept-parents
                        #:concept-is-primitive-p
                        #:concept-is-primitive?
                        #:concept-instances
                        #:taxonomy
                        #:role-parents
                        #:role-children
                        #:role-offspring
                        #:role-ancestors
                        #:role-descendants
                        #:role-inverse
                        #:atomic-role-parents
                        #:atomic-role-children
                        #:atomic-role-ancestors
                        #:atomic-role-descendants
                        #:atomic-role-inverse
                        #:define-abox
                        #:define-abox-1
                        #:realize-abox
                        #:check-abox-coherence
                        #:abox-realized-p
                        #:abox-realized?
                        #:abox-prepared-p
                        #:abox-prepared?
                        #:abox-consistent-p
                        #:abox-consistent?
                        #:abox-una-consistent-p
                        #:abox-una-consistent?
                        #:role-satisfiable?
                        #:role-satisfiable-p
                        #:role-subsumes?
                        #:role-subsumes-p
                        #:individual-p
                        #:individual?
                        #:cd-object-p
                        #:cd-object?
                        #:individual-types
                        #:individual-direct-types
                        #:individual-instance?
                        #:individual-instance-p
                        #:individual-fillers
                        #:individual-attribute-fillers
                        #:individuals-related?
                        #:individuals-related-p
                        #:individuals-equal?
                        #:individuals-not-equal?
                        #:individuals-equal-p
                        #:individuals-not-equal-p
                        #:instantiators
                        #:most-specific-instantiators
                        #:retrieve-concept-instances
                        #:retrieve-individual-filled-roles
                        #:individual-filled-roles
                        #:retrieve-direct-predecessors
                        #:direct-predecessors
                        #:retrieve-related-individuals
                        #:retrieve-individual-fillers
                        #:retrieve-individual-predecessors
                        #:retrieve-individual-told-datatype-fillers
                        #:retrieve-individual-attribute-fillers
                        #:told-value
                        #:related-individuals
                        #:all-tboxes
                        #:delete-all-tboxes
                        #:all-aboxes
                        #:delete-all-aboxes
                        #:loop-over-aboxes
                        #:loop-over-tboxes
                        #:all-roles
                        #:all-features
                        #:all-transitive-roles
                        #:all-attributes
                        #:attribute-type
                        #:attribute-has-range
                        #:attribute-domain
                        #:attribute-domain-1
                        #:attribute-has-domain
                        #:all-atomic-concepts
                        #:all-equivalent-concepts
                        #:all-individuals
                        #:all-concept-assertions-for-individual
                        #:all-role-assertions-for-individual-in-domain
                        #:all-role-assertions-for-individual-in-range
                        #:all-role-assertions
                        #:all-attribute-assertions
                        #:all-concept-assertions
                        #:all-constraints
                        #:role-p
                        #:role?
                        #:transitive-p
                        #:transitive?
                        #:feature-p
                        #:feature?
                        #:inverse-feature-p
                        #:inverse-feature?
                        #:cd-attribute-p
                        #:cd-attribute?
                        #:symmetric-p
                        #:symmetric?
                        #:asymmetric-p
                        #:asymmetric?
                        #:reflexive-p
                        #:reflexive?
                        #:irreflexive-p
                        #:irreflexive?
                        #:concept-p
                        #:concept?
                        #:describe-tbox
                        #:describe-concept
                        #:describe-role
                        #:describe-abox
                        #:describe-individual
                        #:describe-individual1
                        #:*encode-roles-as-reflexive*
                        #:*encode-roles-as-transitive*
                        #:validate-true
                        #:validate-false
                        #:validate-set
                        #:verify-with-concept-tree-list
                        #:verify-with-abox-individuals-list
                        #:verify-with-concept-tree
                        #:verify-with-role-tree-list
                        #:verify-with-role-tree
                        #:test-assert
                        #:timed-out-test-assert
                        #:test-verify-with-concept-tree-list
                        #:test-verify-with-abox-individuals-list
                        #:test-verify-with-role-tree-list
                        #:timed-out-test-verify-with-abox-individuals-list
                        #:test-with-timeout
                        #:display-graph
                        #:print-tbox-tree
                        #:print-tbox-role-tree
                        #:with-racer-statistics
                        #:with-sat-statistics
                        #:with-kb-statistics
                        #:with-verbose-statistics
                        #:*dl-prover-version*
                        #:*dl-prover-name*
                        #:constrained
                        #:add-attribute-assertion
                        #:constraints
                        #:add-constraint-assertion
                        #:>=
                        #:<=
                        #:>
                        #:<
                        #:<>
                        #:=
                        #:*
                        #:+
                        #:-
                        #:a
                        #:an
                        #:no
                        #:ensure
                        #:min
                        #:max
                        #:range
                        #:equal
                        #:unequal
                        #:string=
                        #:string<>
                        #:boolean=
                        #:boolean<>
                        #:divisible
                        #:not-divisible
                        ;;=constant
                        #:complex
                        #:real
                        #:integer
                        #:cardinal
                        #:string
                        #:boolean
                        #:xml-read-tbox-file
                        #:rdfs-read-tbox-file
                        #:racer-read-file
                        #:racer-read-document
                        #:owl-read-file
                        #:owl-read-document
                        #:dig-read-file
                        #:dig-read-document
                        #:owllink-read-file
                        #:owllink-read-document
                        #:associated-tbox
                        #:set-associated-tbox
                        #:associated-aboxes
                        #:save-kb
                        #:include-kb
                        #:import-kb
                        #:clear-default-tbox
                        #:compute-index-for-instance-retrieval
                        #:ensure-subsumption-based-query-answering
                        #:ensure-small-tboxes
                        #:*prevent-lean-tbox*
                        #:*always-use-lean-tbox*
                        #:*enforce-lean-tbox-threshold*
                        #:publish
                        #:publish-1
                        #:unpublish
                        #:unpublish-1
                        #:subscribe
                        #:subscribe-1
                        #:unsubscribe
                        #:unsubscribe-1
                        #:init-subscriptions
                        #:init-subscriptions-1
                        #:init-publications
                        #:init-publications-1
                        #:check-subscriptions
                        #:functional
                        #:role-is-functional
                        #:reflexive
                        #:role-is-reflexive
                        #:irreflexive
                        #:role-is-irreflexive
                        #:symmetric
                        #:role-is-symmetric
                        #:asymmetric
                        #:role-is-asymmetric
                        #:transitive
                        #:role-is-transitive
                        #:inverse
                        #:inverse-of-role
                        #:declare-disjoint
                        #:domain
                        #:role-has-domain
                        ;;range
                        #:role-has-range
                        #:implies-role
			#:implies-role1			
                        #:role-has-parent
                        #:get-tbox-language
                        #:get-abox-language
                        #:get-meta-constraint
                        #:get-concept-definition
                        #:get-concept-negated-definition
                        #:get-concept-definition-1
                        #:get-concept-negated-definition-1
                        #:atomic-role-range
                        #:role-range
                        #:atomic-role-domain
                        #:role-domain
                        #:default
                        #:*racer-user-package*
                        #:logging-on
                        #:logging-off
                        #:tbox-namespaces
                        #:remove-prefix
                        #:get-namespace-prefix
			#:get-namespace-prefixes
                        #:expt
                        #:store-tbox-image
                        #:store-tboxes-image
                        #:store-abox-image
                        #:store-aboxes-image
                        #:store-kb-image
                        #:store-kbs-image
                        #:restore-abox-image
                        #:restore-aboxes-image
                        #:restore-tbox-image
                        #:restore-tboxes-image
                        #:restore-kb-image
                        #:restore-kbs-image
                        #:mirror
                        #:clear-mirror-table
                        #:kb-ontologies
                        #:compute-implicit-role-fillers
                        #:compute-all-implicit-role-fillers
                        #:constraint-entailed-p
                        #:constraint-entailed?
                        #:roles-equivalent
                        #:roles-equivalent-1
                        #:roles-disjoint
                        #:roles-disjoint-1
                        #:atomic-role-synonyms
                        #:role-synonyms
                        #:atomic-role-antonyms
                        #:role-antonyms
                        #:print-abox-individuals
                        
                        #:individual-told-datatype-fillers
                        #:retrieve-individual-told-datatype-fillers
                        #:retrieve-individual-annotation-property-fillers 
                        #:role-equivalent?
                        #:role-equivalent-p                  
                        #:role-disjoint?
                        #:role-disjoint-p                  
                        #:racer-internal%has-integer-value
                        #:racer-internal%has-real-value
                        #:racer-internal%has-string-value
                        #:racer-internal%has-boolean-value
                        #:add-different-from-assertion
                        #:add-same-individual-as-assertion
                        #:add-all-different-assertion
                        #:role-used-as-datatype-property-p
                        #:role-is-used-as-datatype-property
                        #:datatype-role-range
                        #:datatype-role-has-range
                        
                        #:set-server-timeout
                        #:get-server-timeout
                        #:define-datatype-property
                        #:add-datatype-property
                        #:retrieve-individual-synonyms
                        #:individual-synonyms
                        #:retrieve-individual-antonyms
                        #:individual-antonyms
                        
                        #:with-server-timeout
                        #:with-server-connection
                        #:*racer-process-id*
                        #:with-racer-critical-section
                        
                        #:get-tbox-version
                        #:get-abox-version
                        
                        #:individual-told-attribute-value
                        #:retrieve-individual-told-attribute-value
                        #:create-tbox-internal-marker-concept
                        
                        #:parse-expression
                        
                        #:role-used-as-annotation-property-p
                        #:role-is-used-as-annotation-property
                        #:add-annotation-concept-assertion
                        #:add-annotation-role-assertion
                        #:all-annotation-concept-assertions
                        #:all-annotation-role-assertions
                        
                        #:internal-individuals-related-p                  
                        #:internal-retrieve-individual-filled-roles
                        #:internal-retrieve-individual-fillers
                        #:internal-retrieve-direct-predecessors
                        #:internal-retrieve-related-individuals
                        
                         
			
                        #:attribute-filler
                        #:set-attribute-filler
                        #:datatype-role-filler
                        #:add-datatype-role-filler
                        #:add-negative-datatype-role-filler
			
                        #:*use-unique-name-assumption*
                        
                        #:get-individual-pmodel
                        #:get-concept-pmodel
                        
                        #:time
                        
                        #:*server-timeout*
                        #:*unsafe-mode*
                        #:with-timeout
                        #:with-racer-critical-section
                        #:with-non-interruptable-timeout
                        #:without-timeout
                        #:with-timeout-cleanup
                        #:*check-subscriptions-inhibited*
                        #:print-racer-info
                        #:print-wilbur-info
                        #:print-cl-http-info
                        #:ensure-tbox-name
                        #:ensure-abox-name
                        
                        #:transmit-file
                        
                        #:sirius
                        
                        #:without-signature-checks
                        
                        #:*temp-directory*
                        #:temp-directory
                        
                        #:*use-owllink-interface*
                        #:*use-owllink2-interface*
                        #:*dig-version*
                        #:print-debug-info
                        
                        #:+dig-1-1+
                        #:+dig-1-1-racer+
                        
                        #:swrl-forward-chaining
                        #:prepare-racer-engine
                        #:rewrite-concept
                        #:ensure-precompletion-materialization
                        #:set-rewrite-defined-concepts
                        #:enable-optimized-query-processing
                        #:prepare-abox
			
			#:triple-store-read-file
			#:use-triple-store
			#:materialize-inferences
			#:save-ontology-to-triple-store
			#:pretrieve
			#:pracer-answer-query
			#:triple-store-graphs

			#:triple-store-open-p
			#:close-triple-store
			#:open-triple-store
			#:create-triple-store
			#:index-all-triples
		      
                        #:declare-current-knowledge-bases-as-persistent
                        
                        #:true-value-reader   ; internal
                        #:false-value-reader  ; internal

                        #:server-case

                        #:define-event-assertion 
                        #:add-event-assertion
                        #:define-event-rule
                        #:add-event-rule
                        #:timenet-retrieve
                        #:timenet-answer-query
                        #:recognize-events
			#:convert-event-specs

                        #:get-role-datatype
                        #:define-rule
                        #:add-rule-axiom

                        #:sparql-retrieve
                        #:sparql-answer-query
                        
                        #:enable-abduction
                        #:disable-abduction
                        #:retrieve-with-explanation
                        #:racer-answer-query-with-explanation

                        #:forget-individual
                        
                        #:lcs
                        #:lcs-unfold
                        #:msc-k
			
			#:define-prefix
			#:add-prefix
			
                        #:enable-alisp-compatibility-mode
			#:disable-alisp-compatibility-mode

			#:forget-role-axioms
			
			#+:sonic #:ale-lcs
			#+:sonic #:alen-lcs
			#+:sonic #:alc-ale-approximation
                        #+:sonic #:alen-approx
                        #+:sonic #:alen-approx-to-new-concept

                        #:declare-role-axiom
                        #:publish-file
			
			#:retrieve-2
			#:racer-answer-query-2
			
			#:*cur-reasoner*
			#:with-reasoner
			#:find-reasoner
			#:last-answer
			#:simple-output
			#:return-policy
			#:last-output-stream-string

			#:abox-consistent-if-assertions-added-p
                               
                        #:get-object-top-role
                        #:get-object-bottom-role
                        #:get-datatype-top-role
                        #:get-datatype-bottom-role
                        #:get-data-bottom-role                        

                        #:forget-all-different-assertion
                        #:forget-different-from-assertion
                        #:forget-same-individual-as-assertion
                        #:forget-datatype-role-filler
                        #:forget-negative-datatype-role-filler
                        #:forget-negated-role-assertion
                        #:forget-annotation-concept-assertion

                        #:*file-external-format*
                        #:*proxy*
                        #:enable-racer-server

                        #:enable-reader-macros
                        #:enable-boolean-readers
                        
                        #:new-request
                        #:get-current-request
                        
                        #:clear-request
                        #:request-abort
                        #:confirm-abort
                        #:abort-requested-p
                        #:progress-certain-p
                        
                        #:*progress*
                        #:set-progress-value
                        #:get-progress-value
                        #:set-progress-range
                        #:set-progress-100%
                        #:with-progress-range
                        #:without-progress
                        #:with-progress-state
                        #:make-progress-state

                        #:owllink-eval-request
                        #:owllink2-eval-request
                        #:dig-eval-request
                        #:*use-owllink-interface*
                        #:*use-owllink2-interface*
                        #:racer-warn
                        
                        #:dl-descriptor
                        #:dl-language-set
                        
                        #:send-to-subscriber
			#:with-logging
			#:with-temp-file
			#:transform-to-html
			#:without-duplicate-warnings
			#:racer-make-hash-table
			#:transform-literal
                        #:transform-type
                        #:check-for-url-mirror
                        #:+owl-thing+
                        #:+owl-nothing+
                        #:+owl-top-object-role+
                        #:+owl-bottom-object-role+
                        #:+owl-top-data-role+
                        #:+owl-bottom-data-role+

                        #:racer-boolean
                        
                        ;; Relevant for :thematic-substrate only
                        #:*server-request* 
                        #:*multiprocessing-server*
                        #:*socket*
                        #:*server-control-socket*
                        #:*server-control-process*
                        #:*exit-server-requested*
                        #:*one-simple-output*
                        #:*tcp-console-logging*
                        #:*log-file*
                        #:*indent*
                        #:*prefix-mappings*
                        #:get-prefixes
                        #:*cached-prefixes*
                        #:*uri-mirror-table*
                        #:*use-less-tbox-memory*
                        #:ensure-knowledge-base-state
                        #:tbox-associated-with-abox
                        #:tbox-name
                        #:abox-name
                        
                        #:get-tbox-role
                        #:ensure-role
                        #:tbox-internal-roles
                        #:role-transitive-p
                        #:role-descendants-internal
                        #:abox-individual-identity-disjointness-assertions
                        #:check-concept-coherence-internal
                        #:check-ontology-internal

                        #:atomic-role-descendants-internal
                        #:atomic-role-ancestors-internal
                        #:atomic-role-parents2
                        #:atomic-role-children2
                        #:atomic-role-ancestors2
                        #:atomic-role-descendants2

                        #:process-racer-file
                        #:process-racer-string

                        #:get-product-name
                        #:get-product-version
                        #:get-build-version
                        #:get-build-id

                        #:add-to-tbox-namespaces
                        #:reset-prefix-cache
                        
                        #:enable-boolean-readers

                        #:with-temp-file
                        #:without-duplicate-warnings
                        #:with-environment

                        #:deflate
                        #:define-datatype-1
                        #:coherent-p
                        #:abox-coherence-checked-p

                        #:+racer-specials+
                        #:+owl-version+
                        #:make-owlapi-progress-state
                        #:progress-state-owlapi-reasoner

                        #:transform-into-individual-synsets
                        #:transform-data-specification

                        #:*test-assert-timeout*
                        #:*racer-trace*
                        #:trace-racer
                        #:untrace-racer

                        #:racer-toplevel

                        ))
              
              collect (if (stringp sym)
                          (intern sym :racer)
                        (intern (symbol-name sym) :racer)))
        :racer)


(defun use-racer-in-cl-user ()
  (dolist (package '(racer nrql-symbols))
    (loop for sym being the external-symbols of (find-package package) do
	 (shadowing-import sym 'cl-user))))

#|

The old version was:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun use-racer-in-cl-user ()
    (dolist (package '(racer nrql-symbols))
      (loop for sym being the external-symbols of (find-package package) do
	    (let ((cl-user-sym (find-symbol (symbol-name sym) 'cl-user)))
	      (when cl-user-sym
		(unintern cl-user-sym 'cl-user)))))))
It did not work in sbcl due to name conflicts that were, apparently, not solved by unintern.

(loop for name in foo
      for symbol = (find-symbol name)
      unless (and symbol
                  (or (fboundp symbol)
                      (functionp (macro-function symbol))
                      (boundp symbol)))
      collect name)
|#

#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(toplevel 
	    racer-execute-expression
	    process-racer-expr
	    process-racer-file
	    process-racer-string
	    answer-1
	    *file-external-format*
	    *last-answer*
	    *last-error*
	    *one-simple-output* 
	    *tcp-console-logging*
	    *log-file*
	    *debug-racer-server*

	    *build-version*
	    *product-version*
	    *product-name*
	    *build-id*
	    *is-preview-version*
	    *is-beta-version*
	    *porter-name*
	    *porter-version*

	    get-product-version
	    get-product-name
	    set-product-name
	    get-build-version                  
	    get-build-id
	    beta-version-p
	    preview-version-p)
	  :cl-user))
;;; has to be evaluated AFTER the export!

#+:racer
(eval-when (:compile-toplevel :load-toplevel :execute)
;;(eval-when (:load-toplevel :execute)
  (when *enforce-racer*
    (use-racer-in-cl-user)))

|#                  
