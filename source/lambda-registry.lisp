(in-package :racer)

;;;
;;;----------------------------------------------
;;;    Automatically Generated Lambda Registry   
;;;          Version: 2.0, Build: 2013-03-07 
;;;          Date: March 07 2013, 13:05  
;;;----------------------------------------------
;;;

(defconstant +lambda-registry+ (quote
                                ((xml-read-tbox-file (filename))
                                 (without-unique-name-assumption
                                   (&body body))
                                 (with-unique-name-assumption
                                   (&body body))
                                 (with-nrql-settings-evaluated ((&key
                                                                 thematic-substrate::mode
                                                                 thematic-substrate::dont-show-variables
                                                                 thematic-substrate::dont-show-lambdas
                                                                 thematic-substrate::dont-show-head-projection-operators
                                                                 thematic-substrate::abox-mirroring
                                                                 thematic-substrate::query-optimization
                                                                 optimizer-use-cardinality-heuristics
                                                                 thematic-substrate::how-many-tuples
                                                                 thematic-substrate::timeout
                                                                 thematic-substrate::warnings
                                                                 add-rule-consequences-automatically
                                                                 thematic-substrate::dont-add-abox-duplicates
                                                                 thematic-substrate::two-phase-query-processing-mode
                                                                 thematic-substrate::phase-two-starts-warning-tokens
                                                                 thematic-substrate::kb-has-changed-warning-tokens
                                                                 thematic-substrate::told-information-querying
                                                                 thematic-substrate::tuple-computation-mode
                                                                 exclude-permutations
                                                                 thematic-substrate::query-repository
                                                                 thematic-substrate::report-inconsistent-queries
                                                                 thematic-substrate::report-tautological-queries
                                                                 thematic-substrate::query-realization
                                                                 thematic-substrate::bindings
                                                                 thematic-substrate::check-abox-consistency
                                                                 thematic-substrate::use-individual-equivalence-classes
                                                                 thematic-substrate::rewrite-to-dnf
                                                                 thematic-substrate::type-of-substrate
                                                                 thematic-substrate::abox
                                                                 thematic-substrate::tbox)
                                                                &body
                                                                body))
                                 (with-nrql-settings ((&key
                                                       thematic-substrate::mode
                                                       thematic-substrate::dont-show-variables
                                                       thematic-substrate::dont-show-lambdas
                                                       thematic-substrate::dont-show-head-projection-operators
                                                       thematic-substrate::abox-mirroring
                                                       thematic-substrate::query-optimization
                                                       optimizer-use-cardinality-heuristics
                                                       thematic-substrate::how-many-tuples
                                                       thematic-substrate::timeout
                                                       thematic-substrate::warnings
                                                       add-rule-consequences-automatically
                                                       thematic-substrate::dont-add-abox-duplicates
                                                       thematic-substrate::two-phase-query-processing-mode
                                                       thematic-substrate::phase-two-starts-warning-tokens
                                                       thematic-substrate::kb-has-changed-warning-tokens
                                                       thematic-substrate::told-information-querying
                                                       thematic-substrate::tuple-computation-mode
                                                       exclude-permutations
                                                       thematic-substrate::query-repository
                                                       thematic-substrate::report-inconsistent-queries
                                                       thematic-substrate::report-tautological-queries
                                                       thematic-substrate::query-realization
                                                       thematic-substrate::bindings
                                                       thematic-substrate::check-abox-consistency
                                                       thematic-substrate::use-individual-equivalence-classes
                                                       thematic-substrate::rewrite-to-dnf
                                                       thematic-substrate::type-of-substrate
                                                       thematic-substrate::abox
                                                       thematic-substrate::tbox)
                                                      &body
                                                      body))
                                 (with-future-bindings-evaluated ((&key)
                                                                  &body
                                                                  body))
                                 (with-future-bindings ((&key)
                                                        &body
                                                        body))
                                 (with-bindings-evaluated ((&key)
                                                           &body
                                                           body))
                                 (with-bindings ((&key) &body body))
                                 (waiting-rules (&key
                                                 thematic-substrate::abox
                                                 thematic-substrate::type-of-substrate))
                                 (waiting-queries (&key
                                                   thematic-substrate::abox
                                                   thematic-substrate::type-of-substrate))
                                 (waiting-expensive-rules (&key
                                                           thematic-substrate::abox
                                                           thematic-substrate::type-of-substrate))
                                 (waiting-expensive-queries (&key
                                                             thematic-substrate::abox
                                                             thematic-substrate::type-of-substrate))
                                 (waiting-cheap-rules (&key
                                                       thematic-substrate::abox
                                                       thematic-substrate::type-of-substrate))
                                 (waiting-cheap-queries (&key
                                                         thematic-substrate::abox
                                                         thematic-substrate::type-of-substrate))
                                 (wait-for-rules-to-terminate nil)
                                 (wait-for-queries-to-terminate nil)
                                 (verify-with-concept-tree-list (tree-list
                                                                 &optional
                                                                 tbox
                                                                 ignore-error))
                                 (verify-with-abox-individuals-list (individuals-list
                                                                     &optional
                                                                     abox))
                                 (use-triple-store (db
                                                    &key
                                                    kb-name
                                                    graph
                                                    subgraph
                                                    partition
                                                    told-only
                                                    init
                                                    verbose
                                                    directory
                                                    ignore-import))
                                 (use-injective-variables-by-default nil)
                                 (use-individual-synonym-equivalence-classes nil)
                                 (update-racer (&key
                                                thematic-substrate::patchdir
                                                thematic-substrate::plugindir
                                                thematic-substrate::url))
                                 (unsubscribe-from (thematic-substrate::query thematic-substrate::subscriber-name
                                                                              &key
                                                                              thematic-substrate::ip
                                                                              thematic-substrate::port
                                                                              thematic-substrate::use-simplified-protocol-p))
                                 (unsubscribe-1 (subscriber-name
                                                 &optional
                                                 query-concept
                                                 abox))
                                 (unsubscribe (subscriber
                                               &optional
                                               query-concept
                                               abox))
                                 (unrelated (left-name
                                             right-name
                                             role-name))
                                 (unpublish-1 (individual
                                               &optional
                                               abox))
                                 (unpublish (individual
                                             &optional
                                             abox))
                                 (undefquery (thematic-substrate::name &key
                                                                       thematic-substrate::tbox
                                                                       thematic-substrate::arity))
                                 (undefine1 (thematic-substrate::name))
                                 (undefine-query (thematic-substrate::name &key
                                                                           thematic-substrate::tbox
                                                                           thematic-substrate::arity))
                                 (undefine-all nil)
                                 (undefine (thematic-substrate::name))
                                 (unbind1 (thematic-substrate::name))
                                 (unbind-all nil)
                                 (unbind (thematic-substrate::name))
                                 (unapplicable-rules (&key
                                                      thematic-substrate::abox
                                                      thematic-substrate::type-of-substrate))
                                 (triple-store-read-file (filename
                                                          &key
                                                          db
                                                          init
                                                          verbose
                                                          if-exists
                                                          index-p
                                                          graph
                                                          data-version-level
                                                          &allow-other-keys))
                                 (triple-store-open-p (&optional
                                                       db-name))
                                 (triple-store-graphs (&key
                                                       db
                                                       directory))
                                 (transmit-file (extension
                                                 n-bytes))
                                 (transitive? (role-term
                                               &optional
                                               tbox-name))
                                 (transitive-p (role-term
                                                &optional
                                                tbox))
                                 (transitive (rolename
                                              &optional
                                              tbox))
                                 (told-value (object
                                              &optional
                                              abox))
                                 (timenet-retrieve (query
                                                    &key
                                                    abox))
                                 (timenet-answer-query (query
                                                        &key
                                                        abox))
                                 (time (form))
                                 (terminated-rules (&key
                                                    thematic-substrate::abox
                                                    thematic-substrate::type-of-substrate))
                                 (terminated-queries (&key
                                                      thematic-substrate::abox
                                                      thematic-substrate::type-of-substrate))
                                 (tbox-retrieve1 (thematic-substrate::query thematic-substrate::res-args
                                                                            &key
                                                                            thematic-substrate::execute-p
                                                                            thematic-substrate::dont-add-abox-duplicates-p
                                                                            thematic-substrate::remove-duplicates-p
                                                                            thematic-substrate::two-phase-processing-p
                                                                            thematic-substrate::deliver-phase-two-warning-tokens-p
                                                                            thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                                            thematic-substrate::add-rule-consequences-p
                                                                            thematic-substrate::continuation-based-instance-retrieval-p
                                                                            thematic-substrate::told-information-reasoning-p
                                                                            thematic-substrate::final-consistency-checking-p
                                                                            thematic-substrate::runtime-consistency-checking-p
                                                                            thematic-substrate::verbose-p
                                                                            thematic-substrate::dont-show-variables
                                                                            thematic-substrate::dont-show-head-projection-operators-p
                                                                            thematic-substrate::dont-show-lambdas-p
                                                                            thematic-substrate::how-many
                                                                            thematic-substrate::only-new-tuples-p
                                                                            thematic-substrate::timeout
                                                                            thematic-substrate::proactive-tuple-computation-p
                                                                            thematic-substrate::tuple-at-a-time-p
                                                                            thematic-substrate::use-individual-synonyms-p
                                                                            thematic-substrate::check-abox-consistency-p
                                                                            thematic-substrate::ensure-tbox-classification-p
                                                                            thematic-substrate::initial-abox-mirroring-p
                                                                            thematic-substrate::initial-role-assertion-mirroring-p
                                                                            thematic-substrate::classify-concepts-in-instance-assertions-p
                                                                            thematic-substrate::exclude-permutations-p
                                                                            thematic-substrate::record-explanations-p
                                                                            thematic-substrate::parser-class
                                                                            thematic-substrate::rewrite-defined-concepts-p
                                                                            thematic-substrate::group-by-ops
                                                                            thematic-substrate::bind-specials-p
                                                                            thematic-substrate::original-query
                                                                            thematic-substrate::rule-con-pattern
                                                                            thematic-substrate::new-ind-ops
                                                                            thematic-substrate::premise
                                                                            thematic-substrate::generate-code-p
                                                                            thematic-substrate::optimize-p
                                                                            thematic-substrate::rewrite-semantically-p
                                                                            thematic-substrate::rewrite-to-dnf-p
                                                                            thematic-substrate::report-inconsistent-queries-p
                                                                            thematic-substrate::report-tautological-queries-p
                                                                            thematic-substrate::use-repository-p
                                                                            thematic-substrate::put-into-repository-p
                                                                            thematic-substrate::id
                                                                            thematic-substrate::dont-check-id-p
                                                                            thematic-substrate::parser
                                                                            thematic-substrate::result-vois
                                                                            thematic-substrate::tbox
                                                                            package
                                                                            thematic-substrate::create-tbox-if-not-found-p
                                                                            thematic-substrate::substrate))
                                 (tbox-retrieve (thematic-substrate::res-args
                                                 thematic-substrate::query
                                                 &key
                                                 thematic-substrate::execute-p
                                                 thematic-substrate::dont-add-abox-duplicates-p
                                                 thematic-substrate::remove-duplicates-p
                                                 thematic-substrate::two-phase-processing-p
                                                 thematic-substrate::deliver-phase-two-warning-tokens-p
                                                 thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                 thematic-substrate::add-rule-consequences-p
                                                 thematic-substrate::continuation-based-instance-retrieval-p
                                                 thematic-substrate::told-information-reasoning-p
                                                 thematic-substrate::final-consistency-checking-p
                                                 thematic-substrate::runtime-consistency-checking-p
                                                 thematic-substrate::verbose-p
                                                 thematic-substrate::dont-show-variables
                                                 thematic-substrate::dont-show-head-projection-operators-p
                                                 thematic-substrate::dont-show-lambdas-p
                                                 thematic-substrate::how-many
                                                 thematic-substrate::only-new-tuples-p
                                                 thematic-substrate::timeout
                                                 thematic-substrate::proactive-tuple-computation-p
                                                 thematic-substrate::tuple-at-a-time-p
                                                 thematic-substrate::use-individual-synonyms-p
                                                 thematic-substrate::check-abox-consistency-p
                                                 thematic-substrate::ensure-tbox-classification-p
                                                 thematic-substrate::initial-abox-mirroring-p
                                                 thematic-substrate::initial-role-assertion-mirroring-p
                                                 thematic-substrate::classify-concepts-in-instance-assertions-p
                                                 thematic-substrate::exclude-permutations-p
                                                 thematic-substrate::record-explanations-p
                                                 thematic-substrate::parser-class
                                                 thematic-substrate::rewrite-defined-concepts-p
                                                 thematic-substrate::group-by-ops
                                                 thematic-substrate::bind-specials-p
                                                 thematic-substrate::original-query
                                                 thematic-substrate::rule-con-pattern
                                                 thematic-substrate::new-ind-ops
                                                 thematic-substrate::premise
                                                 thematic-substrate::generate-code-p
                                                 thematic-substrate::optimize-p
                                                 thematic-substrate::rewrite-semantically-p
                                                 thematic-substrate::rewrite-to-dnf-p
                                                 thematic-substrate::report-inconsistent-queries-p
                                                 thematic-substrate::report-tautological-queries-p
                                                 thematic-substrate::use-repository-p
                                                 thematic-substrate::put-into-repository-p
                                                 thematic-substrate::id
                                                 thematic-substrate::dont-check-id-p
                                                 thematic-substrate::parser
                                                 thematic-substrate::result-vois
                                                 thematic-substrate::tbox
                                                 package
                                                 thematic-substrate::create-tbox-if-not-found-p
                                                 thematic-substrate::substrate))
                                 (tbox-prepared? (&optional
                                                  tbox-name))
                                 (tbox-prepared-p (&optional
                                                   tbox))
                                 (tbox-cyclic? (&optional
                                                tbox-name))
                                 (tbox-cyclic-p (&optional
                                                 tbox))
                                 (tbox-coherent? (&optional
                                                  tbox-name))
                                 (tbox-coherent-p (&optional
                                                   tbox))
                                 (tbox-classified? (&optional
                                                    tbox-name))
                                 (tbox-classified-p (&optional
                                                     tbox))
                                 (taxonomy (&optional tbox))
                                 (symmetric? (role-term
                                              &optional
                                              tbox-name))
                                 (symmetric-p (role-term
                                               &optional
                                               tbox))
                                 (symmetric (rolename
                                             &optional
                                             tbox))
                                 (swrl-forward-chaining (&key
                                                         abox
                                                         verbose
                                                         delete-rules))
                                 (swrl-create-forward-chainging-rules nil)
                                 (swrl-create-abduction-rules-if-possible nil)
                                 (subscribe-to (thematic-substrate::query thematic-substrate::subscriber-name
                                                                          &key
                                                                          thematic-substrate::ip
                                                                          thematic-substrate::port
                                                                          thematic-substrate::use-simplified-protocol-p))
                                 (subscribe-1 (subscriber-name
                                               query-concept
                                               &optional
                                               abox
                                               ip
                                               port
                                               simple-protocol-p))
                                 (subscribe (subscriber
                                             query-concept
                                             &optional
                                             abox
                                             ip
                                             port
                                             use-simplified-protocol-p))
                                 (store-tboxes-image (tboxes
                                                      filename))
                                 (store-tbox-image (filename
                                                    &optional
                                                    tbox))
                                 (store-substrate-for-abox (thematic-substrate::filename
                                                            &optional
                                                            thematic-substrate::for-abox
                                                            thematic-substrate::type-of-substrate))
                                 (store-server-image (thematic-substrate::filename))
                                 (store-kbs-image (filename
                                                   kbs))
                                 (store-kb-image (filename
                                                  &optional
                                                  kb))
                                 (store-all-substrates (thematic-substrate::filename))
                                 (store-aboxes-image (filename
                                                      aboxes))
                                 (store-abox-image (filename
                                                    &optional
                                                    abox))
                                 (state (&body forms))
                                 (sparql-retrieve (sparql-query
                                                   &rest
                                                   args
                                                   &key
                                                   racer
                                                   stream
                                                   native
                                                   add-standard-prefixes
                                                   use-optimizer
                                                   &allow-other-keys))
                                 (sparql-answer-query (sparql-query
                                                       &rest
                                                       args
                                                       &key
                                                       racer
                                                       stream
                                                       native
                                                       add-standard-prefixes
                                                       use-optimizer
                                                       &allow-other-keys))
                                 (sleeping-rules (&key
                                                  thematic-substrate::abox
                                                  thematic-substrate::type-of-substrate))
                                 (sleeping-queries (&key
                                                    thematic-substrate::abox
                                                    thematic-substrate::type-of-substrate))
                                 (sleeping-expensive-rules (&key
                                                            thematic-substrate::abox
                                                            thematic-substrate::type-of-substrate))
                                 (sleeping-expensive-queries (&key
                                                              thematic-substrate::abox
                                                              thematic-substrate::type-of-substrate))
                                 (sleeping-cheap-rules (&key
                                                        thematic-substrate::abox
                                                        thematic-substrate::type-of-substrate))
                                 (sleeping-cheap-queries (&key
                                                          thematic-substrate::abox
                                                          thematic-substrate::type-of-substrate))
                                 (signature (&whole
                                             signature-form
                                             &key
                                             atomic-concepts
                                             roles
                                             transitive-roles
                                             features
                                             attributes
                                             individuals
                                             objects))
                                 (show-qbox-for-abox (&optional
                                                      thematic-substrate::abox
                                                      thematic-substrate::definitions-p))
                                 (set-unique-name-assumption (value))
                                 (set-substrate-type (type))
                                 (set-server-timeout (timeout))
                                 (set-rewrite-defined-concepts (thematic-substrate::val))
                                 (set-rcc-box (thematic-substrate::name &optional
                                                                        thematic-substrate::rcc-type
                                                                        type))
                                 (set-racer-parameter (thematic-substrate::name thematic-substrate::value))
                                 (set-proxy-server (thematic-substrate::proxy))
                                 (set-nrql-mode (thematic-substrate::mode))
                                 (set-new-ind-prefix (thematic-substrate::prefix))
                                 (set-new-ind-counter (thematic-substrate::n))
                                 (set-mirror-data-box (thematic-substrate::name))
                                 (set-maximum-size-of-process-pool (thematic-substrate::n))
                                 (set-max-no-of-tuples-bound (&optional
                                                              thematic-substrate::n))
                                 (set-initial-size-of-process-pool (thematic-substrate::n))
                                 (set-find-tbox (tbox-name1
                                                 tbox-name2))
                                 (set-find-abox (abox-name1
                                                 abox-name2))
                                 (set-edge-label-for-non-existent-edges (edge-label &key
                                                                                    thematic-substrate::abox
                                                                                    thematic-substrate::type-of-substrate))
                                 (set-data-box (thematic-substrate::name))
                                 (set-current-tbox (tbox))
                                 (set-current-abox (abox))
                                 (set-attribute-filler (abox
                                                        individual
                                                        value
                                                        attribute
                                                        &optional
                                                        type))
                                 (server-value (thematic-substrate::name))
                                 (server-function (thematic-substrate::name))
                                 (server-case nil)
                                 (save-tbox (pathname-or-stream
                                             &optional
                                             tbox
                                             &key
                                             syntax
                                             transformed
                                             avoid-duplicate-definitions
                                             if-exists
                                             if-does-not-exist
                                             uri
                                             anonymized
                                             header))
                                 (save-ontology-to-triple-store (&rest
                                                                 args))
                                 (save-kb (pathname-or-stream
                                           &key
                                           tbox
                                           abox
                                           if-exists
                                           if-does-not-exist
                                           uri
                                           syntax
                                           ontology-name
                                           header))
                                 (save-abox (pathname-or-stream
                                             &optional
                                             abox
                                             &key
                                             syntax
                                             transformed
                                             if-exists
                                             if-does-not-exist
                                             header
                                             uri
                                             import-list
                                             ontology-name))
                                 (same-individual-as (individual-name-1
                                                      individual-name-2))
                                 (same-as (individual-name-1
                                           individual-name-2))
                                 (running-rules (&key
                                                 thematic-substrate::abox
                                                 thematic-substrate::type-of-substrate))
                                 (running-queries (&key
                                                   thematic-substrate::abox
                                                   thematic-substrate::type-of-substrate))
                                 (running-expensive-rules (&key
                                                           thematic-substrate::abox
                                                           thematic-substrate::type-of-substrate))
                                 (running-expensive-queries (&key
                                                             thematic-substrate::abox
                                                             thematic-substrate::type-of-substrate))
                                 (running-cheap-rules (&key
                                                       thematic-substrate::abox
                                                       thematic-substrate::type-of-substrate))
                                 (running-cheap-queries (&key
                                                         thematic-substrate::abox
                                                         thematic-substrate::type-of-substrate))
                                 (run-all-rules (&key
                                                 thematic-substrate::abox
                                                 thematic-substrate::type-of-substrate))
                                 (run-all-queries (&key
                                                   thematic-substrate::abox
                                                   thematic-substrate::type-of-substrate
                                                   thematic-substrate::dont-add-abox-duplicates-p
                                                   thematic-substrate::remove-duplicates-p
                                                   thematic-substrate::two-phase-processing-p
                                                   thematic-substrate::deliver-phase-two-warning-tokens-p
                                                   thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                   thematic-substrate::add-rule-consequences-p
                                                   thematic-substrate::continuation-based-instance-retrieval-p
                                                   thematic-substrate::told-information-reasoning-p
                                                   thematic-substrate::final-consistency-checking-p
                                                   thematic-substrate::runtime-consistency-checking-p
                                                   thematic-substrate::verbose-p
                                                   thematic-substrate::dont-show-variables
                                                   thematic-substrate::dont-show-head-projection-operators-p
                                                   thematic-substrate::dont-show-lambdas-p
                                                   thematic-substrate::how-many
                                                   thematic-substrate::only-new-tuples-p
                                                   thematic-substrate::timeout
                                                   thematic-substrate::proactive-tuple-computation-p
                                                   thematic-substrate::tuple-at-a-time-p
                                                   thematic-substrate::use-individual-synonyms-p
                                                   thematic-substrate::check-abox-consistency-p
                                                   thematic-substrate::ensure-tbox-classification-p
                                                   thematic-substrate::initial-abox-mirroring-p
                                                   thematic-substrate::initial-role-assertion-mirroring-p
                                                   thematic-substrate::classify-concepts-in-instance-assertions-p
                                                   thematic-substrate::exclude-permutations-p
                                                   thematic-substrate::record-explanations-p))
                                 (rule-waiting-p (thematic-substrate::query))
                                 (rule-unapplicable-p (thematic-substrate::query))
                                 (rule-terminated-p (thematic-substrate::query))
                                 (rule-sleeping-p (thematic-substrate::query))
                                 (rule-running-p (thematic-substrate::query))
                                 (rule-ready-p (thematic-substrate::query))
                                 (rule-processed-p (thematic-substrate::query))
                                 (rule-prepared-p (thematic-substrate::query))
                                 (rule-consistent-p (thematic-substrate::query &key))
                                 (rule-consequence (thematic-substrate::query))
                                 (rule-applicable-p (thematic-substrate::query))
                                 (rule-antecedence (thematic-substrate::query))
                                 (rule-active-p (thematic-substrate::query))
                                 (rule-accurate-p (thematic-substrate::query))
                                 (roles-equivalent-1 (role1
                                                      role2
                                                      tbox))
                                 (roles-equivalent (role1
                                                    role2
                                                    &optional
                                                    tbox))
                                 (roles-disjoint-1 (role1
                                                    role2
                                                    tbox))
                                 (roles-disjoint (role1
                                                  role2
                                                  &optional
                                                  tbox))
                                 (role? (role-term
                                         &optional
                                         tbox-name))
                                 (role-used-as-datatype-property-p (role-name tbox))
                                 (role-used-as-annotation-property-p (role-name tbox))
                                 (role-synonyms (role-term
                                                 &optional
                                                 tbox))
                                 (role-subsumes? (role-term-1
                                                  role-term-2
                                                  &optional
                                                  tbox))
                                 (role-subsumes-p (role-term-1
                                                   role-term-2
                                                   tbox))
                                 (role-satisfiable? (role
                                                     &optional
                                                     tbox))
                                 (role-satisfiable-p (role
                                                      tbox))
                                 (role-range (role-term
                                              &optional
                                              tbox))
                                 (role-parents (role-term
                                                &optional
                                                tbox))
                                 (role-p (role-term
                                          &optional
                                          tbox))
                                 (role-is-used-as-datatype-property (rolename
                                                                     tbox))
                                 (role-is-used-as-annotation-property (rolename
                                                                       tbox))
                                 (role-is-transitive (rolename
                                                      tbox))
                                 (role-is-symmetric (rolename
                                                     tbox))
                                 (role-is-reflexive (rolename
                                                     tbox))
                                 (role-is-irreflexive (rolename
                                                       tbox))
                                 (role-is-functional (rolename
                                                      tbox))
                                 (role-is-asymmetric (rolename
                                                      tbox))
                                 (role-inverse (role-term
                                                &optional
                                                tbox))
                                 (role-has-range (rolename
                                                  concept
                                                  tbox
                                                  &optional
                                                  errorp))
                                 (role-has-parent (rolename-1
                                                   rolename-2
                                                   tbox))
                                 (role-has-domain (rolename
                                                   concept
                                                   tbox
                                                   &optional
                                                   errorp))
                                 (role-equivalent? (role-term-1
                                                    role-term-2
                                                    &optional
                                                    tbox))
                                 (role-equivalent-p (role-1
                                                     role-2
                                                     tbox))
                                 (role-domain (role-term
                                               &optional
                                               tbox))
                                 (role-disjoint? (role-term-1
                                                  role-term-2
                                                  &optional
                                                  tbox))
                                 (role-disjoint-p (role-term-1
                                                   role-term-2
                                                   tbox))
                                 (role-descendants (role-term
                                                    &optional
                                                    tbox))
                                 (role-children (role-term
                                                 &optional
                                                 tbox))
                                 (role-ancestors (role-term
                                                  &optional
                                                  tbox))
                                 (rmi (thematic-substrate::args))
                                 (retrieve1 (thematic-substrate::query thematic-substrate::res-args
                                                                       &key
                                                                       thematic-substrate::execute-p
                                                                       thematic-substrate::dont-add-abox-duplicates-p
                                                                       thematic-substrate::remove-duplicates-p
                                                                       thematic-substrate::two-phase-processing-p
                                                                       thematic-substrate::deliver-phase-two-warning-tokens-p
                                                                       thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                                       thematic-substrate::add-rule-consequences-p
                                                                       thematic-substrate::continuation-based-instance-retrieval-p
                                                                       thematic-substrate::told-information-reasoning-p
                                                                       thematic-substrate::final-consistency-checking-p
                                                                       thematic-substrate::runtime-consistency-checking-p
                                                                       thematic-substrate::verbose-p
                                                                       thematic-substrate::dont-show-variables
                                                                       thematic-substrate::dont-show-head-projection-operators-p
                                                                       thematic-substrate::dont-show-lambdas-p
                                                                       thematic-substrate::how-many
                                                                       thematic-substrate::only-new-tuples-p
                                                                       thematic-substrate::timeout
                                                                       thematic-substrate::proactive-tuple-computation-p
                                                                       thematic-substrate::tuple-at-a-time-p
                                                                       thematic-substrate::use-individual-synonyms-p
                                                                       thematic-substrate::check-abox-consistency-p
                                                                       thematic-substrate::ensure-tbox-classification-p
                                                                       thematic-substrate::initial-abox-mirroring-p
                                                                       thematic-substrate::initial-role-assertion-mirroring-p
                                                                       thematic-substrate::classify-concepts-in-instance-assertions-p
                                                                       thematic-substrate::exclude-permutations-p
                                                                       thematic-substrate::record-explanations-p
                                                                       thematic-substrate::parser-class
                                                                       thematic-substrate::rewrite-defined-concepts-p
                                                                       thematic-substrate::group-by-ops
                                                                       thematic-substrate::bind-specials-p
                                                                       thematic-substrate::original-query
                                                                       thematic-substrate::rule-con-pattern
                                                                       thematic-substrate::new-ind-ops
                                                                       thematic-substrate::premise
                                                                       thematic-substrate::generate-code-p
                                                                       thematic-substrate::optimize-p
                                                                       thematic-substrate::rewrite-semantically-p
                                                                       thematic-substrate::rewrite-to-dnf-p
                                                                       thematic-substrate::report-inconsistent-queries-p
                                                                       thematic-substrate::report-tautological-queries-p
                                                                       thematic-substrate::use-repository-p
                                                                       thematic-substrate::put-into-repository-p
                                                                       thematic-substrate::id
                                                                       thematic-substrate::dont-check-id-p
                                                                       thematic-substrate::parser
                                                                       thematic-substrate::result-vois
                                                                       thematic-substrate::substrate
                                                                       thematic-substrate::abox
                                                                       thematic-substrate::create-abox-if-not-found-p
                                                                       package
                                                                       thematic-substrate::type-of-substrate
                                                                       thematic-substrate::prepare-now-p))
                                 (retrieve-with-explanation (thematic-substrate::res-args
                                                             thematic-substrate::query
                                                             &rest
                                                             thematic-substrate::args
                                                             &key
                                                             thematic-substrate::cutoff-fn
                                                             thematic-substrate::hypo-mode-stack
                                                             thematic-substrate::c-mode
                                                             thematic-substrate::r-mode
                                                             thematic-substrate::only-best-p
                                                             thematic-substrate::order-by
                                                             thematic-substrate::reverse-order-p
                                                             thematic-substrate::ensure-permutations-p
                                                             thematic-substrate::how-many
                                                             thematic-substrate::strategy
                                                             thematic-substrate::simple-result-p
                                                             thematic-substrate::runtime-consistency-checking-p
                                                             thematic-substrate::final-consistency-checking-p
                                                             thematic-substrate::same-as-only-p
                                                             thematic-substrate::candidate-individuals
                                                             thematic-substrate::binding-validator
                                                             &allow-other-keys))
                                 (retrieve-under-premise1 (thematic-substrate::query thematic-substrate::res-args
                                                                                     &key
                                                                                     thematic-substrate::execute-p
                                                                                     thematic-substrate::dont-add-abox-duplicates-p
                                                                                     thematic-substrate::remove-duplicates-p
                                                                                     thematic-substrate::two-phase-processing-p
                                                                                     thematic-substrate::deliver-phase-two-warning-tokens-p
                                                                                     thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                                                     thematic-substrate::add-rule-consequences-p
                                                                                     thematic-substrate::continuation-based-instance-retrieval-p
                                                                                     thematic-substrate::told-information-reasoning-p
                                                                                     thematic-substrate::final-consistency-checking-p
                                                                                     thematic-substrate::runtime-consistency-checking-p
                                                                                     thematic-substrate::verbose-p
                                                                                     thematic-substrate::dont-show-variables
                                                                                     thematic-substrate::dont-show-head-projection-operators-p
                                                                                     thematic-substrate::dont-show-lambdas-p
                                                                                     thematic-substrate::how-many
                                                                                     thematic-substrate::only-new-tuples-p
                                                                                     thematic-substrate::timeout
                                                                                     thematic-substrate::proactive-tuple-computation-p
                                                                                     thematic-substrate::tuple-at-a-time-p
                                                                                     thematic-substrate::use-individual-synonyms-p
                                                                                     thematic-substrate::check-abox-consistency-p
                                                                                     thematic-substrate::ensure-tbox-classification-p
                                                                                     thematic-substrate::initial-abox-mirroring-p
                                                                                     thematic-substrate::initial-role-assertion-mirroring-p
                                                                                     thematic-substrate::classify-concepts-in-instance-assertions-p
                                                                                     thematic-substrate::exclude-permutations-p
                                                                                     thematic-substrate::record-explanations-p
                                                                                     thematic-substrate::parser-class
                                                                                     thematic-substrate::rewrite-defined-concepts-p
                                                                                     thematic-substrate::group-by-ops
                                                                                     thematic-substrate::bind-specials-p
                                                                                     thematic-substrate::original-query
                                                                                     thematic-substrate::rule-con-pattern
                                                                                     thematic-substrate::new-ind-ops
                                                                                     thematic-substrate::premise
                                                                                     thematic-substrate::generate-code-p
                                                                                     thematic-substrate::optimize-p
                                                                                     thematic-substrate::rewrite-semantically-p
                                                                                     thematic-substrate::rewrite-to-dnf-p
                                                                                     thematic-substrate::report-inconsistent-queries-p
                                                                                     thematic-substrate::report-tautological-queries-p
                                                                                     thematic-substrate::use-repository-p
                                                                                     thematic-substrate::put-into-repository-p
                                                                                     thematic-substrate::id
                                                                                     thematic-substrate::dont-check-id-p
                                                                                     thematic-substrate::parser
                                                                                     thematic-substrate::result-vois
                                                                                     thematic-substrate::substrate
                                                                                     thematic-substrate::abox
                                                                                     thematic-substrate::create-abox-if-not-found-p
                                                                                     package
                                                                                     thematic-substrate::type-of-substrate
                                                                                     thematic-substrate::prepare-now-p))
                                 (retrieve-under-premise (thematic-substrate::res-args
                                                          thematic-substrate::query
                                                          &key
                                                          thematic-substrate::execute-p
                                                          thematic-substrate::dont-add-abox-duplicates-p
                                                          thematic-substrate::remove-duplicates-p
                                                          thematic-substrate::two-phase-processing-p
                                                          thematic-substrate::deliver-phase-two-warning-tokens-p
                                                          thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                          thematic-substrate::add-rule-consequences-p
                                                          thematic-substrate::continuation-based-instance-retrieval-p
                                                          thematic-substrate::told-information-reasoning-p
                                                          thematic-substrate::final-consistency-checking-p
                                                          thematic-substrate::runtime-consistency-checking-p
                                                          thematic-substrate::verbose-p
                                                          thematic-substrate::dont-show-variables
                                                          thematic-substrate::dont-show-head-projection-operators-p
                                                          thematic-substrate::dont-show-lambdas-p
                                                          thematic-substrate::how-many
                                                          thematic-substrate::only-new-tuples-p
                                                          thematic-substrate::timeout
                                                          thematic-substrate::proactive-tuple-computation-p
                                                          thematic-substrate::tuple-at-a-time-p
                                                          thematic-substrate::use-individual-synonyms-p
                                                          thematic-substrate::check-abox-consistency-p
                                                          thematic-substrate::ensure-tbox-classification-p
                                                          thematic-substrate::initial-abox-mirroring-p
                                                          thematic-substrate::initial-role-assertion-mirroring-p
                                                          thematic-substrate::classify-concepts-in-instance-assertions-p
                                                          thematic-substrate::exclude-permutations-p
                                                          thematic-substrate::record-explanations-p
                                                          thematic-substrate::parser-class
                                                          thematic-substrate::rewrite-defined-concepts-p
                                                          thematic-substrate::group-by-ops
                                                          thematic-substrate::bind-specials-p
                                                          thematic-substrate::original-query
                                                          thematic-substrate::rule-con-pattern
                                                          thematic-substrate::new-ind-ops
                                                          thematic-substrate::premise
                                                          thematic-substrate::generate-code-p
                                                          thematic-substrate::optimize-p
                                                          thematic-substrate::rewrite-semantically-p
                                                          thematic-substrate::rewrite-to-dnf-p
                                                          thematic-substrate::report-inconsistent-queries-p
                                                          thematic-substrate::report-tautological-queries-p
                                                          thematic-substrate::use-repository-p
                                                          thematic-substrate::put-into-repository-p
                                                          thematic-substrate::id
                                                          thematic-substrate::dont-check-id-p
                                                          thematic-substrate::parser
                                                          thematic-substrate::result-vois
                                                          thematic-substrate::substrate
                                                          thematic-substrate::abox
                                                          thematic-substrate::create-abox-if-not-found-p
                                                          package
                                                          thematic-substrate::type-of-substrate
                                                          thematic-substrate::prepare-now-p))
                                 (retrieve-related-individuals (role-term
                                                                abox))
                                 (retrieve-individual-told-datatype-fillers (ind
                                                                             datatype-role
                                                                             &optional
                                                                             direct-p
                                                                             abox
                                                                             with-types-p))
                                 (retrieve-individual-told-attribute-value (ind
                                                                            attribute
                                                                            abox))
                                 (retrieve-individual-synonyms (individual
                                                                &optional
                                                                told-only
                                                                abox))
                                 (retrieve-individual-fillers (ind-predecessor
                                                               role-term
                                                               abox
                                                               &key
                                                               told))
                                 (retrieve-individual-filled-roles (ind-predecessor
                                                                    ind-filler
                                                                    abox
                                                                    &key
                                                                    synsets-p
                                                                    negated-p
                                                                    no-inverses-p
                                                                    roles))
                                 (retrieve-individual-attribute-fillers (ind
                                                                         attribute
                                                                         abox))
                                 (retrieve-individual-antonyms (individual
                                                                &optional
                                                                told-only
                                                                abox))
                                 (retrieve-individual-annotation-property-fillers (individual-name
                                                                                   role
                                                                                   abox
                                                                                   &optional
                                                                                   with-types-p))
                                 (retrieve-direct-predecessors (role-term
                                                                ind-filler
                                                                abox))
                                 (retrieve-concept-instances (concept-term abox
                                                                                  &optional
                                                                                  candidates))
                                 (retrieve (thematic-substrate::res-args
                                            thematic-substrate::query
                                            &key
                                            thematic-substrate::execute-p
                                            thematic-substrate::dont-add-abox-duplicates-p
                                            thematic-substrate::remove-duplicates-p
                                            thematic-substrate::two-phase-processing-p
                                            thematic-substrate::deliver-phase-two-warning-tokens-p
                                            thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                            thematic-substrate::add-rule-consequences-p
                                            thematic-substrate::continuation-based-instance-retrieval-p
                                            thematic-substrate::told-information-reasoning-p
                                            thematic-substrate::final-consistency-checking-p
                                            thematic-substrate::runtime-consistency-checking-p
                                            thematic-substrate::verbose-p
                                            thematic-substrate::dont-show-variables
                                            thematic-substrate::dont-show-head-projection-operators-p
                                            thematic-substrate::dont-show-lambdas-p
                                            thematic-substrate::how-many
                                            thematic-substrate::only-new-tuples-p
                                            thematic-substrate::timeout
                                            thematic-substrate::proactive-tuple-computation-p
                                            thematic-substrate::tuple-at-a-time-p
                                            thematic-substrate::use-individual-synonyms-p
                                            thematic-substrate::check-abox-consistency-p
                                            thematic-substrate::ensure-tbox-classification-p
                                            thematic-substrate::initial-abox-mirroring-p
                                            thematic-substrate::initial-role-assertion-mirroring-p
                                            thematic-substrate::classify-concepts-in-instance-assertions-p
                                            thematic-substrate::exclude-permutations-p
                                            thematic-substrate::record-explanations-p
                                            thematic-substrate::parser-class
                                            thematic-substrate::rewrite-defined-concepts-p
                                            thematic-substrate::group-by-ops
                                            thematic-substrate::bind-specials-p
                                            thematic-substrate::original-query
                                            thematic-substrate::rule-con-pattern
                                            thematic-substrate::new-ind-ops
                                            thematic-substrate::premise
                                            thematic-substrate::generate-code-p
                                            thematic-substrate::optimize-p
                                            thematic-substrate::rewrite-semantically-p
                                            thematic-substrate::rewrite-to-dnf-p
                                            thematic-substrate::report-inconsistent-queries-p
                                            thematic-substrate::report-tautological-queries-p
                                            thematic-substrate::use-repository-p
                                            thematic-substrate::put-into-repository-p
                                            thematic-substrate::id
                                            thematic-substrate::dont-check-id-p
                                            thematic-substrate::parser
                                            thematic-substrate::result-vois
                                            thematic-substrate::substrate
                                            thematic-substrate::abox
                                            thematic-substrate::create-abox-if-not-found-p
                                            package
                                            thematic-substrate::type-of-substrate
                                            thematic-substrate::prepare-now-p))
                                 (restore-tboxes-image (filename))
                                 (restore-tbox-image (filename))
                                 (restore-substrate (thematic-substrate::filename))
                                 (restore-standard-settings nil)
                                 (restore-server-image (thematic-substrate::filename))
                                 (restore-kbs-image (filename))
                                 (restore-kb-image (filename))
                                 (restore-all-substrates (thematic-substrate::filename))
                                 (restore-aboxes-image (filename))
                                 (restore-abox-image (filename))
                                 (reset-nrql-engine (&key
                                                     thematic-substrate::full-reset-p))
                                 (reset-all-substrates (&key
                                                        thematic-substrate::abox
                                                        thematic-substrate::type-of-substrate))
                                 (reprepare-rule (thematic-substrate::query))
                                 (reprepare-query (thematic-substrate::query &key
                                                                             thematic-substrate::to-substrate
                                                                             thematic-substrate::copy-p
                                                                             thematic-substrate::new-id))
                                 (report-inconsistent-queries-and-rules nil)
                                 (remove-implied-concept-assertions (thematic-substrate::abox))
                                 (related-individuals (role-term
                                                       &optional
                                                       abox-name))
                                 (related (left-name
                                           right-name
                                           role-name))
                                 (register-rcc-synonym (thematic-substrate::role
                                                        thematic-substrate::rcc-relation))
                                 (reflexive? (role-term
                                              &optional
                                              tbox-name))
                                 (reflexive-p (role-term
                                               &optional
                                               tbox))
                                 (reflexive (rolename
                                             &optional
                                             tbox))
                                 (reexecute-rule (thematic-substrate::query))
                                 (reexecute-query (thematic-substrate::query))
                                 (reexecute-all-rules (&key
                                                       thematic-substrate::abox
                                                       thematic-substrate::type-of-substrate))
                                 (reexecute-all-queries (&key
                                                         thematic-substrate::abox
                                                         thematic-substrate::type-of-substrate))
                                 (recognize-events (&optional
                                                    abox))
                                 (realize-abox (&optional
                                                abox
                                                individual-name))
                                 (ready-rules (&key
                                               thematic-substrate::abox
                                               thematic-substrate::type-of-substrate))
                                 (ready-queries (&key
                                                 thematic-substrate::abox
                                                 thematic-substrate::type-of-substrate))
                                 (rdfs-read-tbox-file (filename))
                                 (rcc-synonym (thematic-substrate::role
                                               thematic-substrate::rcc-relation))
                                 (rcc-related1 nil)
                                 (rcc-related nil)
                                 (rcc-node1 nil)
                                 (rcc-node-label1 nil)
                                 (rcc-node-label nil)
                                 (rcc-node-description1 nil)
                                 (rcc-node-description nil)
                                 (rcc-node nil)
                                 (rcc-instance1 nil)
                                 (rcc-instance nil)
                                 (rcc-edge1 nil)
                                 (rcc-edge-label1 nil)
                                 (rcc-edge-label nil)
                                 (rcc-edge-description1 nil)
                                 (rcc-edge-description nil)
                                 (rcc-edge nil)
                                 (rcc-consistent? (&optional
                                                   thematic-substrate::abox
                                                   thematic-substrate::type-of-substrate))
                                 (rcc-consistent-p (&optional
                                                    thematic-substrate::abox
                                                    thematic-substrate::type-of-substrate))
                                 (range (rolename
                                         concept
                                         &optional
                                         tbox
                                         errorp))
                                 (racer-read-file (filename))
                                 (racer-read-document (url-spec
                                                       &key
                                                       verbose))
                                 (racer-prepare-tbox-query1 (thematic-substrate::query thematic-substrate::res-args
                                                                                       &key
                                                                                       thematic-substrate::execute-p
                                                                                       thematic-substrate::dont-add-abox-duplicates-p
                                                                                       thematic-substrate::remove-duplicates-p
                                                                                       thematic-substrate::two-phase-processing-p
                                                                                       thematic-substrate::deliver-phase-two-warning-tokens-p
                                                                                       thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                                                       thematic-substrate::add-rule-consequences-p
                                                                                       thematic-substrate::continuation-based-instance-retrieval-p
                                                                                       thematic-substrate::told-information-reasoning-p
                                                                                       thematic-substrate::final-consistency-checking-p
                                                                                       thematic-substrate::runtime-consistency-checking-p
                                                                                       thematic-substrate::verbose-p
                                                                                       thematic-substrate::dont-show-variables
                                                                                       thematic-substrate::dont-show-head-projection-operators-p
                                                                                       thematic-substrate::dont-show-lambdas-p
                                                                                       thematic-substrate::how-many
                                                                                       thematic-substrate::only-new-tuples-p
                                                                                       thematic-substrate::timeout
                                                                                       thematic-substrate::proactive-tuple-computation-p
                                                                                       thematic-substrate::tuple-at-a-time-p
                                                                                       thematic-substrate::use-individual-synonyms-p
                                                                                       thematic-substrate::check-abox-consistency-p
                                                                                       thematic-substrate::ensure-tbox-classification-p
                                                                                       thematic-substrate::initial-abox-mirroring-p
                                                                                       thematic-substrate::initial-role-assertion-mirroring-p
                                                                                       thematic-substrate::classify-concepts-in-instance-assertions-p
                                                                                       thematic-substrate::exclude-permutations-p
                                                                                       thematic-substrate::record-explanations-p
                                                                                       thematic-substrate::parser-class
                                                                                       thematic-substrate::rewrite-defined-concepts-p
                                                                                       thematic-substrate::group-by-ops
                                                                                       thematic-substrate::bind-specials-p
                                                                                       thematic-substrate::original-query
                                                                                       thematic-substrate::rule-con-pattern
                                                                                       thematic-substrate::new-ind-ops
                                                                                       thematic-substrate::premise
                                                                                       thematic-substrate::generate-code-p
                                                                                       thematic-substrate::optimize-p
                                                                                       thematic-substrate::rewrite-semantically-p
                                                                                       thematic-substrate::rewrite-to-dnf-p
                                                                                       thematic-substrate::report-inconsistent-queries-p
                                                                                       thematic-substrate::report-tautological-queries-p
                                                                                       thematic-substrate::use-repository-p
                                                                                       thematic-substrate::put-into-repository-p
                                                                                       thematic-substrate::id
                                                                                       thematic-substrate::dont-check-id-p
                                                                                       thematic-substrate::parser
                                                                                       thematic-substrate::result-vois
                                                                                       thematic-substrate::tbox
                                                                                       package
                                                                                       thematic-substrate::create-tbox-if-not-found-p
                                                                                       thematic-substrate::substrate))
                                 (racer-prepare-tbox-query (thematic-substrate::res-args
                                                            thematic-substrate::query
                                                            &key
                                                            thematic-substrate::execute-p
                                                            thematic-substrate::dont-add-abox-duplicates-p
                                                            thematic-substrate::remove-duplicates-p
                                                            thematic-substrate::two-phase-processing-p
                                                            thematic-substrate::deliver-phase-two-warning-tokens-p
                                                            thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                            thematic-substrate::add-rule-consequences-p
                                                            thematic-substrate::continuation-based-instance-retrieval-p
                                                            thematic-substrate::told-information-reasoning-p
                                                            thematic-substrate::final-consistency-checking-p
                                                            thematic-substrate::runtime-consistency-checking-p
                                                            thematic-substrate::verbose-p
                                                            thematic-substrate::dont-show-variables
                                                            thematic-substrate::dont-show-head-projection-operators-p
                                                            thematic-substrate::dont-show-lambdas-p
                                                            thematic-substrate::how-many
                                                            thematic-substrate::only-new-tuples-p
                                                            thematic-substrate::timeout
                                                            thematic-substrate::proactive-tuple-computation-p
                                                            thematic-substrate::tuple-at-a-time-p
                                                            thematic-substrate::use-individual-synonyms-p
                                                            thematic-substrate::check-abox-consistency-p
                                                            thematic-substrate::ensure-tbox-classification-p
                                                            thematic-substrate::initial-abox-mirroring-p
                                                            thematic-substrate::initial-role-assertion-mirroring-p
                                                            thematic-substrate::classify-concepts-in-instance-assertions-p
                                                            thematic-substrate::exclude-permutations-p
                                                            thematic-substrate::record-explanations-p
                                                            thematic-substrate::parser-class
                                                            thematic-substrate::rewrite-defined-concepts-p
                                                            thematic-substrate::group-by-ops
                                                            thematic-substrate::bind-specials-p
                                                            thematic-substrate::original-query
                                                            thematic-substrate::rule-con-pattern
                                                            thematic-substrate::new-ind-ops
                                                            thematic-substrate::premise
                                                            thematic-substrate::generate-code-p
                                                            thematic-substrate::optimize-p
                                                            thematic-substrate::rewrite-semantically-p
                                                            thematic-substrate::rewrite-to-dnf-p
                                                            thematic-substrate::report-inconsistent-queries-p
                                                            thematic-substrate::report-tautological-queries-p
                                                            thematic-substrate::use-repository-p
                                                            thematic-substrate::put-into-repository-p
                                                            thematic-substrate::id
                                                            thematic-substrate::dont-check-id-p
                                                            thematic-substrate::parser
                                                            thematic-substrate::result-vois
                                                            thematic-substrate::tbox
                                                            package
                                                            thematic-substrate::create-tbox-if-not-found-p
                                                            thematic-substrate::substrate))
                                 (racer-prepare-rule1 (thematic-substrate::res-args
                                                       thematic-substrate::query
                                                       &key
                                                       thematic-substrate::execute-p
                                                       thematic-substrate::parser-class
                                                       thematic-substrate::rewrite-defined-concepts-p
                                                       thematic-substrate::group-by-ops
                                                       thematic-substrate::bind-specials-p
                                                       thematic-substrate::original-query
                                                       thematic-substrate::rule-con-pattern
                                                       thematic-substrate::new-ind-ops
                                                       thematic-substrate::premise
                                                       thematic-substrate::generate-code-p
                                                       thematic-substrate::optimize-p
                                                       thematic-substrate::rewrite-semantically-p
                                                       thematic-substrate::rewrite-to-dnf-p
                                                       thematic-substrate::report-inconsistent-queries-p
                                                       thematic-substrate::report-tautological-queries-p
                                                       thematic-substrate::use-repository-p
                                                       thematic-substrate::put-into-repository-p
                                                       thematic-substrate::id
                                                       thematic-substrate::dont-check-id-p
                                                       thematic-substrate::parser
                                                       thematic-substrate::result-vois
                                                       thematic-substrate::substrate
                                                       thematic-substrate::abox
                                                       thematic-substrate::create-abox-if-not-found-p
                                                       package
                                                       thematic-substrate::type-of-substrate
                                                       thematic-substrate::prepare-now-p))
                                 (racer-prepare-rule (thematic-substrate::query thematic-substrate::res-args
                                                                                &key
                                                                                thematic-substrate::execute-p
                                                                                thematic-substrate::parser-class
                                                                                thematic-substrate::rewrite-defined-concepts-p
                                                                                thematic-substrate::group-by-ops
                                                                                thematic-substrate::bind-specials-p
                                                                                thematic-substrate::original-query
                                                                                thematic-substrate::rule-con-pattern
                                                                                thematic-substrate::new-ind-ops
                                                                                thematic-substrate::premise
                                                                                thematic-substrate::generate-code-p
                                                                                thematic-substrate::optimize-p
                                                                                thematic-substrate::rewrite-semantically-p
                                                                                thematic-substrate::rewrite-to-dnf-p
                                                                                thematic-substrate::report-inconsistent-queries-p
                                                                                thematic-substrate::report-tautological-queries-p
                                                                                thematic-substrate::use-repository-p
                                                                                thematic-substrate::put-into-repository-p
                                                                                thematic-substrate::id
                                                                                thematic-substrate::dont-check-id-p
                                                                                thematic-substrate::parser
                                                                                thematic-substrate::result-vois
                                                                                thematic-substrate::substrate
                                                                                thematic-substrate::abox
                                                                                thematic-substrate::create-abox-if-not-found-p
                                                                                package
                                                                                thematic-substrate::type-of-substrate
                                                                                thematic-substrate::prepare-now-p))
                                 (racer-prepare-query1 (thematic-substrate::query thematic-substrate::res-args
                                                                                  &key
                                                                                  thematic-substrate::execute-p
                                                                                  thematic-substrate::dont-add-abox-duplicates-p
                                                                                  thematic-substrate::remove-duplicates-p
                                                                                  thematic-substrate::two-phase-processing-p
                                                                                  thematic-substrate::deliver-phase-two-warning-tokens-p
                                                                                  thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                                                  thematic-substrate::add-rule-consequences-p
                                                                                  thematic-substrate::continuation-based-instance-retrieval-p
                                                                                  thematic-substrate::told-information-reasoning-p
                                                                                  thematic-substrate::final-consistency-checking-p
                                                                                  thematic-substrate::runtime-consistency-checking-p
                                                                                  thematic-substrate::verbose-p
                                                                                  thematic-substrate::dont-show-variables
                                                                                  thematic-substrate::dont-show-head-projection-operators-p
                                                                                  thematic-substrate::dont-show-lambdas-p
                                                                                  thematic-substrate::how-many
                                                                                  thematic-substrate::only-new-tuples-p
                                                                                  thematic-substrate::timeout
                                                                                  thematic-substrate::proactive-tuple-computation-p
                                                                                  thematic-substrate::tuple-at-a-time-p
                                                                                  thematic-substrate::use-individual-synonyms-p
                                                                                  thematic-substrate::check-abox-consistency-p
                                                                                  thematic-substrate::ensure-tbox-classification-p
                                                                                  thematic-substrate::initial-abox-mirroring-p
                                                                                  thematic-substrate::initial-role-assertion-mirroring-p
                                                                                  thematic-substrate::classify-concepts-in-instance-assertions-p
                                                                                  thematic-substrate::exclude-permutations-p
                                                                                  thematic-substrate::record-explanations-p
                                                                                  thematic-substrate::parser-class
                                                                                  thematic-substrate::rewrite-defined-concepts-p
                                                                                  thematic-substrate::group-by-ops
                                                                                  thematic-substrate::bind-specials-p
                                                                                  thematic-substrate::original-query
                                                                                  thematic-substrate::rule-con-pattern
                                                                                  thematic-substrate::new-ind-ops
                                                                                  thematic-substrate::premise
                                                                                  thematic-substrate::generate-code-p
                                                                                  thematic-substrate::optimize-p
                                                                                  thematic-substrate::rewrite-semantically-p
                                                                                  thematic-substrate::rewrite-to-dnf-p
                                                                                  thematic-substrate::report-inconsistent-queries-p
                                                                                  thematic-substrate::report-tautological-queries-p
                                                                                  thematic-substrate::use-repository-p
                                                                                  thematic-substrate::put-into-repository-p
                                                                                  thematic-substrate::id
                                                                                  thematic-substrate::dont-check-id-p
                                                                                  thematic-substrate::parser
                                                                                  thematic-substrate::result-vois
                                                                                  thematic-substrate::substrate
                                                                                  thematic-substrate::abox
                                                                                  thematic-substrate::create-abox-if-not-found-p
                                                                                  package
                                                                                  thematic-substrate::type-of-substrate
                                                                                  thematic-substrate::prepare-now-p))
                                 (racer-prepare-query (thematic-substrate::res-args
                                                       thematic-substrate::query
                                                       &key
                                                       thematic-substrate::execute-p
                                                       thematic-substrate::dont-add-abox-duplicates-p
                                                       thematic-substrate::remove-duplicates-p
                                                       thematic-substrate::two-phase-processing-p
                                                       thematic-substrate::deliver-phase-two-warning-tokens-p
                                                       thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                       thematic-substrate::add-rule-consequences-p
                                                       thematic-substrate::continuation-based-instance-retrieval-p
                                                       thematic-substrate::told-information-reasoning-p
                                                       thematic-substrate::final-consistency-checking-p
                                                       thematic-substrate::runtime-consistency-checking-p
                                                       thematic-substrate::verbose-p
                                                       thematic-substrate::dont-show-variables
                                                       thematic-substrate::dont-show-head-projection-operators-p
                                                       thematic-substrate::dont-show-lambdas-p
                                                       thematic-substrate::how-many
                                                       thematic-substrate::only-new-tuples-p
                                                       thematic-substrate::timeout
                                                       thematic-substrate::proactive-tuple-computation-p
                                                       thematic-substrate::tuple-at-a-time-p
                                                       thematic-substrate::use-individual-synonyms-p
                                                       thematic-substrate::check-abox-consistency-p
                                                       thematic-substrate::ensure-tbox-classification-p
                                                       thematic-substrate::initial-abox-mirroring-p
                                                       thematic-substrate::initial-role-assertion-mirroring-p
                                                       thematic-substrate::classify-concepts-in-instance-assertions-p
                                                       thematic-substrate::exclude-permutations-p
                                                       thematic-substrate::record-explanations-p
                                                       thematic-substrate::parser-class
                                                       thematic-substrate::rewrite-defined-concepts-p
                                                       thematic-substrate::group-by-ops
                                                       thematic-substrate::bind-specials-p
                                                       thematic-substrate::original-query
                                                       thematic-substrate::rule-con-pattern
                                                       thematic-substrate::new-ind-ops
                                                       thematic-substrate::premise
                                                       thematic-substrate::generate-code-p
                                                       thematic-substrate::optimize-p
                                                       thematic-substrate::rewrite-semantically-p
                                                       thematic-substrate::rewrite-to-dnf-p
                                                       thematic-substrate::report-inconsistent-queries-p
                                                       thematic-substrate::report-tautological-queries-p
                                                       thematic-substrate::use-repository-p
                                                       thematic-substrate::put-into-repository-p
                                                       thematic-substrate::id
                                                       thematic-substrate::dont-check-id-p
                                                       thematic-substrate::parser
                                                       thematic-substrate::result-vois
                                                       thematic-substrate::substrate
                                                       thematic-substrate::abox
                                                       thematic-substrate::create-abox-if-not-found-p
                                                       package
                                                       thematic-substrate::type-of-substrate
                                                       thematic-substrate::prepare-now-p))
                                 (racer-apply-rule1 (thematic-substrate::res-args
                                                     thematic-substrate::query
                                                     &key
                                                     thematic-substrate::execute-p
                                                     thematic-substrate::parser-class
                                                     thematic-substrate::rewrite-defined-concepts-p
                                                     thematic-substrate::group-by-ops
                                                     thematic-substrate::bind-specials-p
                                                     thematic-substrate::original-query
                                                     thematic-substrate::rule-con-pattern
                                                     thematic-substrate::new-ind-ops
                                                     thematic-substrate::premise
                                                     thematic-substrate::generate-code-p
                                                     thematic-substrate::optimize-p
                                                     thematic-substrate::rewrite-semantically-p
                                                     thematic-substrate::rewrite-to-dnf-p
                                                     thematic-substrate::report-inconsistent-queries-p
                                                     thematic-substrate::report-tautological-queries-p
                                                     thematic-substrate::use-repository-p
                                                     thematic-substrate::put-into-repository-p
                                                     thematic-substrate::id
                                                     thematic-substrate::dont-check-id-p
                                                     thematic-substrate::parser
                                                     thematic-substrate::result-vois
                                                     thematic-substrate::substrate
                                                     thematic-substrate::abox
                                                     thematic-substrate::create-abox-if-not-found-p
                                                     package
                                                     thematic-substrate::type-of-substrate
                                                     thematic-substrate::prepare-now-p))
                                 (racer-apply-rule-under-premise1 (thematic-substrate::res-args
                                                                   thematic-substrate::query
                                                                   &key
                                                                   thematic-substrate::execute-p
                                                                   thematic-substrate::parser-class
                                                                   thematic-substrate::rewrite-defined-concepts-p
                                                                   thematic-substrate::group-by-ops
                                                                   thematic-substrate::bind-specials-p
                                                                   thematic-substrate::original-query
                                                                   thematic-substrate::rule-con-pattern
                                                                   thematic-substrate::new-ind-ops
                                                                   thematic-substrate::premise
                                                                   thematic-substrate::generate-code-p
                                                                   thematic-substrate::optimize-p
                                                                   thematic-substrate::rewrite-semantically-p
                                                                   thematic-substrate::rewrite-to-dnf-p
                                                                   thematic-substrate::report-inconsistent-queries-p
                                                                   thematic-substrate::report-tautological-queries-p
                                                                   thematic-substrate::use-repository-p
                                                                   thematic-substrate::put-into-repository-p
                                                                   thematic-substrate::id
                                                                   thematic-substrate::dont-check-id-p
                                                                   thematic-substrate::parser
                                                                   thematic-substrate::result-vois
                                                                   thematic-substrate::substrate
                                                                   thematic-substrate::abox
                                                                   thematic-substrate::create-abox-if-not-found-p
                                                                   package
                                                                   thematic-substrate::type-of-substrate
                                                                   thematic-substrate::prepare-now-p))
                                 (racer-apply-rule-under-premise (thematic-substrate::query thematic-substrate::res-args
                                                                                            &key
                                                                                            thematic-substrate::execute-p
                                                                                            thematic-substrate::parser-class
                                                                                            thematic-substrate::rewrite-defined-concepts-p
                                                                                            thematic-substrate::group-by-ops
                                                                                            thematic-substrate::bind-specials-p
                                                                                            thematic-substrate::original-query
                                                                                            thematic-substrate::rule-con-pattern
                                                                                            thematic-substrate::new-ind-ops
                                                                                            thematic-substrate::premise
                                                                                            thematic-substrate::generate-code-p
                                                                                            thematic-substrate::optimize-p
                                                                                            thematic-substrate::rewrite-semantically-p
                                                                                            thematic-substrate::rewrite-to-dnf-p
                                                                                            thematic-substrate::report-inconsistent-queries-p
                                                                                            thematic-substrate::report-tautological-queries-p
                                                                                            thematic-substrate::use-repository-p
                                                                                            thematic-substrate::put-into-repository-p
                                                                                            thematic-substrate::id
                                                                                            thematic-substrate::dont-check-id-p
                                                                                            thematic-substrate::parser
                                                                                            thematic-substrate::result-vois
                                                                                            thematic-substrate::substrate
                                                                                            thematic-substrate::abox
                                                                                            thematic-substrate::create-abox-if-not-found-p
                                                                                            package
                                                                                            thematic-substrate::type-of-substrate
                                                                                            thematic-substrate::prepare-now-p))
                                 (racer-apply-rule (thematic-substrate::query thematic-substrate::res-args
                                                                              &key
                                                                              thematic-substrate::execute-p
                                                                              thematic-substrate::parser-class
                                                                              thematic-substrate::rewrite-defined-concepts-p
                                                                              thematic-substrate::group-by-ops
                                                                              thematic-substrate::bind-specials-p
                                                                              thematic-substrate::original-query
                                                                              thematic-substrate::rule-con-pattern
                                                                              thematic-substrate::new-ind-ops
                                                                              thematic-substrate::premise
                                                                              thematic-substrate::generate-code-p
                                                                              thematic-substrate::optimize-p
                                                                              thematic-substrate::rewrite-semantically-p
                                                                              thematic-substrate::rewrite-to-dnf-p
                                                                              thematic-substrate::report-inconsistent-queries-p
                                                                              thematic-substrate::report-tautological-queries-p
                                                                              thematic-substrate::use-repository-p
                                                                              thematic-substrate::put-into-repository-p
                                                                              thematic-substrate::id
                                                                              thematic-substrate::dont-check-id-p
                                                                              thematic-substrate::parser
                                                                              thematic-substrate::result-vois
                                                                              thematic-substrate::substrate
                                                                              thematic-substrate::abox
                                                                              thematic-substrate::create-abox-if-not-found-p
                                                                              package
                                                                              thematic-substrate::type-of-substrate
                                                                              thematic-substrate::prepare-now-p))
                                 (racer-answer-tbox-query1 (thematic-substrate::query thematic-substrate::res-args
                                                                                      &key
                                                                                      thematic-substrate::execute-p
                                                                                      thematic-substrate::dont-add-abox-duplicates-p
                                                                                      thematic-substrate::remove-duplicates-p
                                                                                      thematic-substrate::two-phase-processing-p
                                                                                      thematic-substrate::deliver-phase-two-warning-tokens-p
                                                                                      thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                                                      thematic-substrate::add-rule-consequences-p
                                                                                      thematic-substrate::continuation-based-instance-retrieval-p
                                                                                      thematic-substrate::told-information-reasoning-p
                                                                                      thematic-substrate::final-consistency-checking-p
                                                                                      thematic-substrate::runtime-consistency-checking-p
                                                                                      thematic-substrate::verbose-p
                                                                                      thematic-substrate::dont-show-variables
                                                                                      thematic-substrate::dont-show-head-projection-operators-p
                                                                                      thematic-substrate::dont-show-lambdas-p
                                                                                      thematic-substrate::how-many
                                                                                      thematic-substrate::only-new-tuples-p
                                                                                      thematic-substrate::timeout
                                                                                      thematic-substrate::proactive-tuple-computation-p
                                                                                      thematic-substrate::tuple-at-a-time-p
                                                                                      thematic-substrate::use-individual-synonyms-p
                                                                                      thematic-substrate::check-abox-consistency-p
                                                                                      thematic-substrate::ensure-tbox-classification-p
                                                                                      thematic-substrate::initial-abox-mirroring-p
                                                                                      thematic-substrate::initial-role-assertion-mirroring-p
                                                                                      thematic-substrate::classify-concepts-in-instance-assertions-p
                                                                                      thematic-substrate::exclude-permutations-p
                                                                                      thematic-substrate::record-explanations-p
                                                                                      thematic-substrate::parser-class
                                                                                      thematic-substrate::rewrite-defined-concepts-p
                                                                                      thematic-substrate::group-by-ops
                                                                                      thematic-substrate::bind-specials-p
                                                                                      thematic-substrate::original-query
                                                                                      thematic-substrate::rule-con-pattern
                                                                                      thematic-substrate::new-ind-ops
                                                                                      thematic-substrate::premise
                                                                                      thematic-substrate::generate-code-p
                                                                                      thematic-substrate::optimize-p
                                                                                      thematic-substrate::rewrite-semantically-p
                                                                                      thematic-substrate::rewrite-to-dnf-p
                                                                                      thematic-substrate::report-inconsistent-queries-p
                                                                                      thematic-substrate::report-tautological-queries-p
                                                                                      thematic-substrate::use-repository-p
                                                                                      thematic-substrate::put-into-repository-p
                                                                                      thematic-substrate::id
                                                                                      thematic-substrate::dont-check-id-p
                                                                                      thematic-substrate::parser
                                                                                      thematic-substrate::result-vois
                                                                                      thematic-substrate::tbox
                                                                                      package
                                                                                      thematic-substrate::create-tbox-if-not-found-p
                                                                                      thematic-substrate::substrate))
                                 (racer-answer-tbox-query (thematic-substrate::res-args
                                                           thematic-substrate::query
                                                           &key
                                                           thematic-substrate::execute-p
                                                           thematic-substrate::dont-add-abox-duplicates-p
                                                           thematic-substrate::remove-duplicates-p
                                                           thematic-substrate::two-phase-processing-p
                                                           thematic-substrate::deliver-phase-two-warning-tokens-p
                                                           thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                           thematic-substrate::add-rule-consequences-p
                                                           thematic-substrate::continuation-based-instance-retrieval-p
                                                           thematic-substrate::told-information-reasoning-p
                                                           thematic-substrate::final-consistency-checking-p
                                                           thematic-substrate::runtime-consistency-checking-p
                                                           thematic-substrate::verbose-p
                                                           thematic-substrate::dont-show-variables
                                                           thematic-substrate::dont-show-head-projection-operators-p
                                                           thematic-substrate::dont-show-lambdas-p
                                                           thematic-substrate::how-many
                                                           thematic-substrate::only-new-tuples-p
                                                           thematic-substrate::timeout
                                                           thematic-substrate::proactive-tuple-computation-p
                                                           thematic-substrate::tuple-at-a-time-p
                                                           thematic-substrate::use-individual-synonyms-p
                                                           thematic-substrate::check-abox-consistency-p
                                                           thematic-substrate::ensure-tbox-classification-p
                                                           thematic-substrate::initial-abox-mirroring-p
                                                           thematic-substrate::initial-role-assertion-mirroring-p
                                                           thematic-substrate::classify-concepts-in-instance-assertions-p
                                                           thematic-substrate::exclude-permutations-p
                                                           thematic-substrate::record-explanations-p
                                                           thematic-substrate::parser-class
                                                           thematic-substrate::rewrite-defined-concepts-p
                                                           thematic-substrate::group-by-ops
                                                           thematic-substrate::bind-specials-p
                                                           thematic-substrate::original-query
                                                           thematic-substrate::rule-con-pattern
                                                           thematic-substrate::new-ind-ops
                                                           thematic-substrate::premise
                                                           thematic-substrate::generate-code-p
                                                           thematic-substrate::optimize-p
                                                           thematic-substrate::rewrite-semantically-p
                                                           thematic-substrate::rewrite-to-dnf-p
                                                           thematic-substrate::report-inconsistent-queries-p
                                                           thematic-substrate::report-tautological-queries-p
                                                           thematic-substrate::use-repository-p
                                                           thematic-substrate::put-into-repository-p
                                                           thematic-substrate::id
                                                           thematic-substrate::dont-check-id-p
                                                           thematic-substrate::parser
                                                           thematic-substrate::result-vois
                                                           thematic-substrate::tbox
                                                           package
                                                           thematic-substrate::create-tbox-if-not-found-p
                                                           thematic-substrate::substrate))
                                 (racer-answer-query1 (thematic-substrate::query thematic-substrate::res-args
                                                                                 &key
                                                                                 thematic-substrate::execute-p
                                                                                 thematic-substrate::dont-add-abox-duplicates-p
                                                                                 thematic-substrate::remove-duplicates-p
                                                                                 thematic-substrate::two-phase-processing-p
                                                                                 thematic-substrate::deliver-phase-two-warning-tokens-p
                                                                                 thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                                                 thematic-substrate::add-rule-consequences-p
                                                                                 thematic-substrate::continuation-based-instance-retrieval-p
                                                                                 thematic-substrate::told-information-reasoning-p
                                                                                 thematic-substrate::final-consistency-checking-p
                                                                                 thematic-substrate::runtime-consistency-checking-p
                                                                                 thematic-substrate::verbose-p
                                                                                 thematic-substrate::dont-show-variables
                                                                                 thematic-substrate::dont-show-head-projection-operators-p
                                                                                 thematic-substrate::dont-show-lambdas-p
                                                                                 thematic-substrate::how-many
                                                                                 thematic-substrate::only-new-tuples-p
                                                                                 thematic-substrate::timeout
                                                                                 thematic-substrate::proactive-tuple-computation-p
                                                                                 thematic-substrate::tuple-at-a-time-p
                                                                                 thematic-substrate::use-individual-synonyms-p
                                                                                 thematic-substrate::check-abox-consistency-p
                                                                                 thematic-substrate::ensure-tbox-classification-p
                                                                                 thematic-substrate::initial-abox-mirroring-p
                                                                                 thematic-substrate::initial-role-assertion-mirroring-p
                                                                                 thematic-substrate::classify-concepts-in-instance-assertions-p
                                                                                 thematic-substrate::exclude-permutations-p
                                                                                 thematic-substrate::record-explanations-p
                                                                                 thematic-substrate::parser-class
                                                                                 thematic-substrate::rewrite-defined-concepts-p
                                                                                 thematic-substrate::group-by-ops
                                                                                 thematic-substrate::bind-specials-p
                                                                                 thematic-substrate::original-query
                                                                                 thematic-substrate::rule-con-pattern
                                                                                 thematic-substrate::new-ind-ops
                                                                                 thematic-substrate::premise
                                                                                 thematic-substrate::generate-code-p
                                                                                 thematic-substrate::optimize-p
                                                                                 thematic-substrate::rewrite-semantically-p
                                                                                 thematic-substrate::rewrite-to-dnf-p
                                                                                 thematic-substrate::report-inconsistent-queries-p
                                                                                 thematic-substrate::report-tautological-queries-p
                                                                                 thematic-substrate::use-repository-p
                                                                                 thematic-substrate::put-into-repository-p
                                                                                 thematic-substrate::id
                                                                                 thematic-substrate::dont-check-id-p
                                                                                 thematic-substrate::parser
                                                                                 thematic-substrate::result-vois
                                                                                 thematic-substrate::substrate
                                                                                 thematic-substrate::abox
                                                                                 thematic-substrate::create-abox-if-not-found-p
                                                                                 package
                                                                                 thematic-substrate::type-of-substrate
                                                                                 thematic-substrate::prepare-now-p))
                                 (racer-answer-query-with-explanation (thematic-substrate::res-args
                                                                       thematic-substrate::query
                                                                       &rest
                                                                       thematic-substrate::args
                                                                       &key
                                                                       thematic-substrate::cutoff-fn
                                                                       thematic-substrate::hypo-mode-stack
                                                                       thematic-substrate::c-mode
                                                                       thematic-substrate::r-mode
                                                                       thematic-substrate::only-best-p
                                                                       thematic-substrate::order-by
                                                                       thematic-substrate::reverse-order-p
                                                                       thematic-substrate::ensure-permutations-p
                                                                       thematic-substrate::how-many
                                                                       thematic-substrate::strategy
                                                                       thematic-substrate::simple-result-p
                                                                       thematic-substrate::runtime-consistency-checking-p
                                                                       thematic-substrate::final-consistency-checking-p
                                                                       thematic-substrate::same-as-only-p
                                                                       thematic-substrate::candidate-individuals
                                                                       thematic-substrate::binding-validator
                                                                       &allow-other-keys))
                                 (racer-answer-query-under-premise1 (thematic-substrate::query thematic-substrate::res-args
                                                                                               &key
                                                                                               thematic-substrate::execute-p
                                                                                               thematic-substrate::dont-add-abox-duplicates-p
                                                                                               thematic-substrate::remove-duplicates-p
                                                                                               thematic-substrate::two-phase-processing-p
                                                                                               thematic-substrate::deliver-phase-two-warning-tokens-p
                                                                                               thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                                                               thematic-substrate::add-rule-consequences-p
                                                                                               thematic-substrate::continuation-based-instance-retrieval-p
                                                                                               thematic-substrate::told-information-reasoning-p
                                                                                               thematic-substrate::final-consistency-checking-p
                                                                                               thematic-substrate::runtime-consistency-checking-p
                                                                                               thematic-substrate::verbose-p
                                                                                               thematic-substrate::dont-show-variables
                                                                                               thematic-substrate::dont-show-head-projection-operators-p
                                                                                               thematic-substrate::dont-show-lambdas-p
                                                                                               thematic-substrate::how-many
                                                                                               thematic-substrate::only-new-tuples-p
                                                                                               thematic-substrate::timeout
                                                                                               thematic-substrate::proactive-tuple-computation-p
                                                                                               thematic-substrate::tuple-at-a-time-p
                                                                                               thematic-substrate::use-individual-synonyms-p
                                                                                               thematic-substrate::check-abox-consistency-p
                                                                                               thematic-substrate::ensure-tbox-classification-p
                                                                                               thematic-substrate::initial-abox-mirroring-p
                                                                                               thematic-substrate::initial-role-assertion-mirroring-p
                                                                                               thematic-substrate::classify-concepts-in-instance-assertions-p
                                                                                               thematic-substrate::exclude-permutations-p
                                                                                               thematic-substrate::record-explanations-p
                                                                                               thematic-substrate::parser-class
                                                                                               thematic-substrate::rewrite-defined-concepts-p
                                                                                               thematic-substrate::group-by-ops
                                                                                               thematic-substrate::bind-specials-p
                                                                                               thematic-substrate::original-query
                                                                                               thematic-substrate::rule-con-pattern
                                                                                               thematic-substrate::new-ind-ops
                                                                                               thematic-substrate::premise
                                                                                               thematic-substrate::generate-code-p
                                                                                               thematic-substrate::optimize-p
                                                                                               thematic-substrate::rewrite-semantically-p
                                                                                               thematic-substrate::rewrite-to-dnf-p
                                                                                               thematic-substrate::report-inconsistent-queries-p
                                                                                               thematic-substrate::report-tautological-queries-p
                                                                                               thematic-substrate::use-repository-p
                                                                                               thematic-substrate::put-into-repository-p
                                                                                               thematic-substrate::id
                                                                                               thematic-substrate::dont-check-id-p
                                                                                               thematic-substrate::parser
                                                                                               thematic-substrate::result-vois
                                                                                               thematic-substrate::substrate
                                                                                               thematic-substrate::abox
                                                                                               thematic-substrate::create-abox-if-not-found-p
                                                                                               package
                                                                                               thematic-substrate::type-of-substrate
                                                                                               thematic-substrate::prepare-now-p))
                                 (racer-answer-query-under-premise (thematic-substrate::res-args
                                                                    thematic-substrate::query
                                                                    &key
                                                                    thematic-substrate::execute-p
                                                                    thematic-substrate::dont-add-abox-duplicates-p
                                                                    thematic-substrate::remove-duplicates-p
                                                                    thematic-substrate::two-phase-processing-p
                                                                    thematic-substrate::deliver-phase-two-warning-tokens-p
                                                                    thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                                    thematic-substrate::add-rule-consequences-p
                                                                    thematic-substrate::continuation-based-instance-retrieval-p
                                                                    thematic-substrate::told-information-reasoning-p
                                                                    thematic-substrate::final-consistency-checking-p
                                                                    thematic-substrate::runtime-consistency-checking-p
                                                                    thematic-substrate::verbose-p
                                                                    thematic-substrate::dont-show-variables
                                                                    thematic-substrate::dont-show-head-projection-operators-p
                                                                    thematic-substrate::dont-show-lambdas-p
                                                                    thematic-substrate::how-many
                                                                    thematic-substrate::only-new-tuples-p
                                                                    thematic-substrate::timeout
                                                                    thematic-substrate::proactive-tuple-computation-p
                                                                    thematic-substrate::tuple-at-a-time-p
                                                                    thematic-substrate::use-individual-synonyms-p
                                                                    thematic-substrate::check-abox-consistency-p
                                                                    thematic-substrate::ensure-tbox-classification-p
                                                                    thematic-substrate::initial-abox-mirroring-p
                                                                    thematic-substrate::initial-role-assertion-mirroring-p
                                                                    thematic-substrate::classify-concepts-in-instance-assertions-p
                                                                    thematic-substrate::exclude-permutations-p
                                                                    thematic-substrate::record-explanations-p
                                                                    thematic-substrate::parser-class
                                                                    thematic-substrate::rewrite-defined-concepts-p
                                                                    thematic-substrate::group-by-ops
                                                                    thematic-substrate::bind-specials-p
                                                                    thematic-substrate::original-query
                                                                    thematic-substrate::rule-con-pattern
                                                                    thematic-substrate::new-ind-ops
                                                                    thematic-substrate::premise
                                                                    thematic-substrate::generate-code-p
                                                                    thematic-substrate::optimize-p
                                                                    thematic-substrate::rewrite-semantically-p
                                                                    thematic-substrate::rewrite-to-dnf-p
                                                                    thematic-substrate::report-inconsistent-queries-p
                                                                    thematic-substrate::report-tautological-queries-p
                                                                    thematic-substrate::use-repository-p
                                                                    thematic-substrate::put-into-repository-p
                                                                    thematic-substrate::id
                                                                    thematic-substrate::dont-check-id-p
                                                                    thematic-substrate::parser
                                                                    thematic-substrate::result-vois
                                                                    thematic-substrate::substrate
                                                                    thematic-substrate::abox
                                                                    thematic-substrate::create-abox-if-not-found-p
                                                                    package
                                                                    thematic-substrate::type-of-substrate
                                                                    thematic-substrate::prepare-now-p))
                                 (racer-answer-query (thematic-substrate::res-args
                                                      thematic-substrate::query
                                                      &key
                                                      thematic-substrate::execute-p
                                                      thematic-substrate::dont-add-abox-duplicates-p
                                                      thematic-substrate::remove-duplicates-p
                                                      thematic-substrate::two-phase-processing-p
                                                      thematic-substrate::deliver-phase-two-warning-tokens-p
                                                      thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                      thematic-substrate::add-rule-consequences-p
                                                      thematic-substrate::continuation-based-instance-retrieval-p
                                                      thematic-substrate::told-information-reasoning-p
                                                      thematic-substrate::final-consistency-checking-p
                                                      thematic-substrate::runtime-consistency-checking-p
                                                      thematic-substrate::verbose-p
                                                      thematic-substrate::dont-show-variables
                                                      thematic-substrate::dont-show-head-projection-operators-p
                                                      thematic-substrate::dont-show-lambdas-p
                                                      thematic-substrate::how-many
                                                      thematic-substrate::only-new-tuples-p
                                                      thematic-substrate::timeout
                                                      thematic-substrate::proactive-tuple-computation-p
                                                      thematic-substrate::tuple-at-a-time-p
                                                      thematic-substrate::use-individual-synonyms-p
                                                      thematic-substrate::check-abox-consistency-p
                                                      thematic-substrate::ensure-tbox-classification-p
                                                      thematic-substrate::initial-abox-mirroring-p
                                                      thematic-substrate::initial-role-assertion-mirroring-p
                                                      thematic-substrate::classify-concepts-in-instance-assertions-p
                                                      thematic-substrate::exclude-permutations-p
                                                      thematic-substrate::record-explanations-p
                                                      thematic-substrate::parser-class
                                                      thematic-substrate::rewrite-defined-concepts-p
                                                      thematic-substrate::group-by-ops
                                                      thematic-substrate::bind-specials-p
                                                      thematic-substrate::original-query
                                                      thematic-substrate::rule-con-pattern
                                                      thematic-substrate::new-ind-ops
                                                      thematic-substrate::premise
                                                      thematic-substrate::generate-code-p
                                                      thematic-substrate::optimize-p
                                                      thematic-substrate::rewrite-semantically-p
                                                      thematic-substrate::rewrite-to-dnf-p
                                                      thematic-substrate::report-inconsistent-queries-p
                                                      thematic-substrate::report-tautological-queries-p
                                                      thematic-substrate::use-repository-p
                                                      thematic-substrate::put-into-repository-p
                                                      thematic-substrate::id
                                                      thematic-substrate::dont-check-id-p
                                                      thematic-substrate::parser
                                                      thematic-substrate::result-vois
                                                      thematic-substrate::substrate
                                                      thematic-substrate::abox
                                                      thematic-substrate::create-abox-if-not-found-p
                                                      package
                                                      thematic-substrate::type-of-substrate
                                                      thematic-substrate::prepare-now-p))
                                 (query-waiting-p (thematic-substrate::query))
                                 (query-terminated-p (thematic-substrate::query))
                                 (query-subscribers (thematic-substrate::query))
                                 (query-sleeping-p (thematic-substrate::query))
                                 (query-running-p (thematic-substrate::query))
                                 (query-ready-p (thematic-substrate::query))
                                 (query-processed-p (thematic-substrate::query))
                                 (query-prepared-p (thematic-substrate::query))
                                 (query-parents (thematic-substrate::query))
                                 (query-head (thematic-substrate::query))
                                 (query-equivalents (thematic-substrate::query))
                                 (query-equivalent-p (a
                                                      thematic-substrate::b
                                                      &key))
                                 (query-entails-p (a
                                                   thematic-substrate::b
                                                   &key))
                                 (query-descendants (thematic-substrate::query))
                                 (query-consistent-p (thematic-substrate::query &key))
                                 (query-children (thematic-substrate::query))
                                 (query-body (thematic-substrate::query))
                                 (query-ancestors (thematic-substrate::query))
                                 (query-active-p (thematic-substrate::query))
                                 (query-accurate-p (thematic-substrate::query))
                                 (publish-file (filename
                                                url
                                                content-type))
                                 (publish-1 (individual
                                             &optional
                                             abox))
                                 (publish (individual
                                           &optional
                                           abox))
                                 (processed-rules (&key
                                                   thematic-substrate::abox
                                                   thematic-substrate::type-of-substrate))
                                 (processed-queries (&key
                                                     thematic-substrate::abox
                                                     thematic-substrate::type-of-substrate))
                                 (process-tuple-at-a-time nil)
                                 (process-set-at-a-time nil)
                                 (print-tbox-tree (&optional
                                                   tbox
                                                   stream
                                                   hide-role-inverses))
                                 (print-abox-individuals (&key
                                                          stream
                                                          abox
                                                          concept-mapping
                                                          ind-mapping
                                                          pretty))
                                 (pretrieve (head
                                             body
                                             &rest
                                             args))
                                 (preprule1 (thematic-substrate::res-args
                                             thematic-substrate::query
                                             &key
                                             thematic-substrate::execute-p
                                             thematic-substrate::parser-class
                                             thematic-substrate::rewrite-defined-concepts-p
                                             thematic-substrate::group-by-ops
                                             thematic-substrate::bind-specials-p
                                             thematic-substrate::original-query
                                             thematic-substrate::rule-con-pattern
                                             thematic-substrate::new-ind-ops
                                             thematic-substrate::premise
                                             thematic-substrate::generate-code-p
                                             thematic-substrate::optimize-p
                                             thematic-substrate::rewrite-semantically-p
                                             thematic-substrate::rewrite-to-dnf-p
                                             thematic-substrate::report-inconsistent-queries-p
                                             thematic-substrate::report-tautological-queries-p
                                             thematic-substrate::use-repository-p
                                             thematic-substrate::put-into-repository-p
                                             thematic-substrate::id
                                             thematic-substrate::dont-check-id-p
                                             thematic-substrate::parser
                                             thematic-substrate::result-vois
                                             thematic-substrate::substrate
                                             thematic-substrate::abox
                                             thematic-substrate::create-abox-if-not-found-p
                                             package
                                             thematic-substrate::type-of-substrate
                                             thematic-substrate::prepare-now-p))
                                 (preprule (thematic-substrate::query thematic-substrate::res-args
                                                                      &key
                                                                      thematic-substrate::execute-p
                                                                      thematic-substrate::parser-class
                                                                      thematic-substrate::rewrite-defined-concepts-p
                                                                      thematic-substrate::group-by-ops
                                                                      thematic-substrate::bind-specials-p
                                                                      thematic-substrate::original-query
                                                                      thematic-substrate::rule-con-pattern
                                                                      thematic-substrate::new-ind-ops
                                                                      thematic-substrate::premise
                                                                      thematic-substrate::generate-code-p
                                                                      thematic-substrate::optimize-p
                                                                      thematic-substrate::rewrite-semantically-p
                                                                      thematic-substrate::rewrite-to-dnf-p
                                                                      thematic-substrate::report-inconsistent-queries-p
                                                                      thematic-substrate::report-tautological-queries-p
                                                                      thematic-substrate::use-repository-p
                                                                      thematic-substrate::put-into-repository-p
                                                                      thematic-substrate::id
                                                                      thematic-substrate::dont-check-id-p
                                                                      thematic-substrate::parser
                                                                      thematic-substrate::result-vois
                                                                      thematic-substrate::substrate
                                                                      thematic-substrate::abox
                                                                      thematic-substrate::create-abox-if-not-found-p
                                                                      package
                                                                      thematic-substrate::type-of-substrate
                                                                      thematic-substrate::prepare-now-p))
                                 (prepared-rules (&key
                                                  thematic-substrate::abox
                                                  thematic-substrate::type-of-substrate))
                                 (prepared-queries (&key
                                                    thematic-substrate::abox
                                                    thematic-substrate::type-of-substrate))
                                 (prepare-tbox-query1 (thematic-substrate::query thematic-substrate::res-args
                                                                                 &key
                                                                                 thematic-substrate::execute-p
                                                                                 thematic-substrate::dont-add-abox-duplicates-p
                                                                                 thematic-substrate::remove-duplicates-p
                                                                                 thematic-substrate::two-phase-processing-p
                                                                                 thematic-substrate::deliver-phase-two-warning-tokens-p
                                                                                 thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                                                 thematic-substrate::add-rule-consequences-p
                                                                                 thematic-substrate::continuation-based-instance-retrieval-p
                                                                                 thematic-substrate::told-information-reasoning-p
                                                                                 thematic-substrate::final-consistency-checking-p
                                                                                 thematic-substrate::runtime-consistency-checking-p
                                                                                 thematic-substrate::verbose-p
                                                                                 thematic-substrate::dont-show-variables
                                                                                 thematic-substrate::dont-show-head-projection-operators-p
                                                                                 thematic-substrate::dont-show-lambdas-p
                                                                                 thematic-substrate::how-many
                                                                                 thematic-substrate::only-new-tuples-p
                                                                                 thematic-substrate::timeout
                                                                                 thematic-substrate::proactive-tuple-computation-p
                                                                                 thematic-substrate::tuple-at-a-time-p
                                                                                 thematic-substrate::use-individual-synonyms-p
                                                                                 thematic-substrate::check-abox-consistency-p
                                                                                 thematic-substrate::ensure-tbox-classification-p
                                                                                 thematic-substrate::initial-abox-mirroring-p
                                                                                 thematic-substrate::initial-role-assertion-mirroring-p
                                                                                 thematic-substrate::classify-concepts-in-instance-assertions-p
                                                                                 thematic-substrate::exclude-permutations-p
                                                                                 thematic-substrate::record-explanations-p
                                                                                 thematic-substrate::parser-class
                                                                                 thematic-substrate::rewrite-defined-concepts-p
                                                                                 thematic-substrate::group-by-ops
                                                                                 thematic-substrate::bind-specials-p
                                                                                 thematic-substrate::original-query
                                                                                 thematic-substrate::rule-con-pattern
                                                                                 thematic-substrate::new-ind-ops
                                                                                 thematic-substrate::premise
                                                                                 thematic-substrate::generate-code-p
                                                                                 thematic-substrate::optimize-p
                                                                                 thematic-substrate::rewrite-semantically-p
                                                                                 thematic-substrate::rewrite-to-dnf-p
                                                                                 thematic-substrate::report-inconsistent-queries-p
                                                                                 thematic-substrate::report-tautological-queries-p
                                                                                 thematic-substrate::use-repository-p
                                                                                 thematic-substrate::put-into-repository-p
                                                                                 thematic-substrate::id
                                                                                 thematic-substrate::dont-check-id-p
                                                                                 thematic-substrate::parser
                                                                                 thematic-substrate::result-vois
                                                                                 thematic-substrate::tbox
                                                                                 package
                                                                                 thematic-substrate::create-tbox-if-not-found-p
                                                                                 thematic-substrate::substrate))
                                 (prepare-tbox-query (thematic-substrate::res-args
                                                      thematic-substrate::query
                                                      &key
                                                      thematic-substrate::execute-p
                                                      thematic-substrate::dont-add-abox-duplicates-p
                                                      thematic-substrate::remove-duplicates-p
                                                      thematic-substrate::two-phase-processing-p
                                                      thematic-substrate::deliver-phase-two-warning-tokens-p
                                                      thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                      thematic-substrate::add-rule-consequences-p
                                                      thematic-substrate::continuation-based-instance-retrieval-p
                                                      thematic-substrate::told-information-reasoning-p
                                                      thematic-substrate::final-consistency-checking-p
                                                      thematic-substrate::runtime-consistency-checking-p
                                                      thematic-substrate::verbose-p
                                                      thematic-substrate::dont-show-variables
                                                      thematic-substrate::dont-show-head-projection-operators-p
                                                      thematic-substrate::dont-show-lambdas-p
                                                      thematic-substrate::how-many
                                                      thematic-substrate::only-new-tuples-p
                                                      thematic-substrate::timeout
                                                      thematic-substrate::proactive-tuple-computation-p
                                                      thematic-substrate::tuple-at-a-time-p
                                                      thematic-substrate::use-individual-synonyms-p
                                                      thematic-substrate::check-abox-consistency-p
                                                      thematic-substrate::ensure-tbox-classification-p
                                                      thematic-substrate::initial-abox-mirroring-p
                                                      thematic-substrate::initial-role-assertion-mirroring-p
                                                      thematic-substrate::classify-concepts-in-instance-assertions-p
                                                      thematic-substrate::exclude-permutations-p
                                                      thematic-substrate::record-explanations-p
                                                      thematic-substrate::parser-class
                                                      thematic-substrate::rewrite-defined-concepts-p
                                                      thematic-substrate::group-by-ops
                                                      thematic-substrate::bind-specials-p
                                                      thematic-substrate::original-query
                                                      thematic-substrate::rule-con-pattern
                                                      thematic-substrate::new-ind-ops
                                                      thematic-substrate::premise
                                                      thematic-substrate::generate-code-p
                                                      thematic-substrate::optimize-p
                                                      thematic-substrate::rewrite-semantically-p
                                                      thematic-substrate::rewrite-to-dnf-p
                                                      thematic-substrate::report-inconsistent-queries-p
                                                      thematic-substrate::report-tautological-queries-p
                                                      thematic-substrate::use-repository-p
                                                      thematic-substrate::put-into-repository-p
                                                      thematic-substrate::id
                                                      thematic-substrate::dont-check-id-p
                                                      thematic-substrate::parser
                                                      thematic-substrate::result-vois
                                                      thematic-substrate::tbox
                                                      package
                                                      thematic-substrate::create-tbox-if-not-found-p
                                                      thematic-substrate::substrate))
                                 (prepare-rule1 (&key
                                                 thematic-substrate::execute-p
                                                 thematic-substrate::parser-class
                                                 thematic-substrate::rewrite-defined-concepts-p
                                                 thematic-substrate::group-by-ops
                                                 thematic-substrate::bind-specials-p
                                                 thematic-substrate::original-query
                                                 thematic-substrate::rule-con-pattern
                                                 thematic-substrate::new-ind-ops
                                                 thematic-substrate::premise
                                                 thematic-substrate::generate-code-p
                                                 thematic-substrate::optimize-p
                                                 thematic-substrate::rewrite-semantically-p
                                                 thematic-substrate::rewrite-to-dnf-p
                                                 thematic-substrate::report-inconsistent-queries-p
                                                 thematic-substrate::report-tautological-queries-p
                                                 thematic-substrate::use-repository-p
                                                 thematic-substrate::put-into-repository-p
                                                 thematic-substrate::id
                                                 thematic-substrate::dont-check-id-p
                                                 thematic-substrate::parser
                                                 thematic-substrate::result-vois
                                                 thematic-substrate::substrate
                                                 thematic-substrate::abox
                                                 thematic-substrate::create-abox-if-not-found-p
                                                 package
                                                 thematic-substrate::type-of-substrate
                                                 thematic-substrate::prepare-now-p))
                                 (prepare-rule (&key
                                                thematic-substrate::execute-p
                                                thematic-substrate::parser-class
                                                thematic-substrate::rewrite-defined-concepts-p
                                                thematic-substrate::group-by-ops
                                                thematic-substrate::bind-specials-p
                                                thematic-substrate::original-query
                                                thematic-substrate::rule-con-pattern
                                                thematic-substrate::new-ind-ops
                                                thematic-substrate::premise
                                                thematic-substrate::generate-code-p
                                                thematic-substrate::optimize-p
                                                thematic-substrate::rewrite-semantically-p
                                                thematic-substrate::rewrite-to-dnf-p
                                                thematic-substrate::report-inconsistent-queries-p
                                                thematic-substrate::report-tautological-queries-p
                                                thematic-substrate::use-repository-p
                                                thematic-substrate::put-into-repository-p
                                                thematic-substrate::id
                                                thematic-substrate::dont-check-id-p
                                                thematic-substrate::parser
                                                thematic-substrate::result-vois
                                                thematic-substrate::substrate
                                                thematic-substrate::abox
                                                thematic-substrate::create-abox-if-not-found-p
                                                package
                                                thematic-substrate::type-of-substrate
                                                thematic-substrate::prepare-now-p))
                                 (prepare-racer-engine (&key
                                                        abox
                                                        classify-tbox-p))
                                 (prepare-query1 (&key
                                                  thematic-substrate::execute-p
                                                  thematic-substrate::dont-add-abox-duplicates-p
                                                  thematic-substrate::remove-duplicates-p
                                                  thematic-substrate::two-phase-processing-p
                                                  thematic-substrate::deliver-phase-two-warning-tokens-p
                                                  thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                  thematic-substrate::add-rule-consequences-p
                                                  thematic-substrate::continuation-based-instance-retrieval-p
                                                  thematic-substrate::told-information-reasoning-p
                                                  thematic-substrate::final-consistency-checking-p
                                                  thematic-substrate::runtime-consistency-checking-p
                                                  thematic-substrate::verbose-p
                                                  thematic-substrate::dont-show-variables
                                                  thematic-substrate::dont-show-head-projection-operators-p
                                                  thematic-substrate::dont-show-lambdas-p
                                                  thematic-substrate::how-many
                                                  thematic-substrate::only-new-tuples-p
                                                  thematic-substrate::timeout
                                                  thematic-substrate::proactive-tuple-computation-p
                                                  thematic-substrate::tuple-at-a-time-p
                                                  thematic-substrate::use-individual-synonyms-p
                                                  thematic-substrate::check-abox-consistency-p
                                                  thematic-substrate::ensure-tbox-classification-p
                                                  thematic-substrate::initial-abox-mirroring-p
                                                  thematic-substrate::initial-role-assertion-mirroring-p
                                                  thematic-substrate::classify-concepts-in-instance-assertions-p
                                                  thematic-substrate::exclude-permutations-p
                                                  thematic-substrate::record-explanations-p
                                                  thematic-substrate::parser-class
                                                  thematic-substrate::rewrite-defined-concepts-p
                                                  thematic-substrate::group-by-ops
                                                  thematic-substrate::bind-specials-p
                                                  thematic-substrate::original-query
                                                  thematic-substrate::rule-con-pattern
                                                  thematic-substrate::new-ind-ops
                                                  thematic-substrate::premise
                                                  thematic-substrate::generate-code-p
                                                  thematic-substrate::optimize-p
                                                  thematic-substrate::rewrite-semantically-p
                                                  thematic-substrate::rewrite-to-dnf-p
                                                  thematic-substrate::report-inconsistent-queries-p
                                                  thematic-substrate::report-tautological-queries-p
                                                  thematic-substrate::use-repository-p
                                                  thematic-substrate::put-into-repository-p
                                                  thematic-substrate::id
                                                  thematic-substrate::dont-check-id-p
                                                  thematic-substrate::parser
                                                  thematic-substrate::result-vois
                                                  thematic-substrate::substrate
                                                  thematic-substrate::abox
                                                  thematic-substrate::create-abox-if-not-found-p
                                                  package
                                                  thematic-substrate::type-of-substrate
                                                  thematic-substrate::prepare-now-p))
                                 (prepare-query (&key
                                                 thematic-substrate::execute-p
                                                 thematic-substrate::dont-add-abox-duplicates-p
                                                 thematic-substrate::remove-duplicates-p
                                                 thematic-substrate::two-phase-processing-p
                                                 thematic-substrate::deliver-phase-two-warning-tokens-p
                                                 thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                 thematic-substrate::add-rule-consequences-p
                                                 thematic-substrate::continuation-based-instance-retrieval-p
                                                 thematic-substrate::told-information-reasoning-p
                                                 thematic-substrate::final-consistency-checking-p
                                                 thematic-substrate::runtime-consistency-checking-p
                                                 thematic-substrate::verbose-p
                                                 thematic-substrate::dont-show-variables
                                                 thematic-substrate::dont-show-head-projection-operators-p
                                                 thematic-substrate::dont-show-lambdas-p
                                                 thematic-substrate::how-many
                                                 thematic-substrate::only-new-tuples-p
                                                 thematic-substrate::timeout
                                                 thematic-substrate::proactive-tuple-computation-p
                                                 thematic-substrate::tuple-at-a-time-p
                                                 thematic-substrate::use-individual-synonyms-p
                                                 thematic-substrate::check-abox-consistency-p
                                                 thematic-substrate::ensure-tbox-classification-p
                                                 thematic-substrate::initial-abox-mirroring-p
                                                 thematic-substrate::initial-role-assertion-mirroring-p
                                                 thematic-substrate::classify-concepts-in-instance-assertions-p
                                                 thematic-substrate::exclude-permutations-p
                                                 thematic-substrate::record-explanations-p
                                                 thematic-substrate::parser-class
                                                 thematic-substrate::rewrite-defined-concepts-p
                                                 thematic-substrate::group-by-ops
                                                 thematic-substrate::bind-specials-p
                                                 thematic-substrate::original-query
                                                 thematic-substrate::rule-con-pattern
                                                 thematic-substrate::new-ind-ops
                                                 thematic-substrate::premise
                                                 thematic-substrate::generate-code-p
                                                 thematic-substrate::optimize-p
                                                 thematic-substrate::rewrite-semantically-p
                                                 thematic-substrate::rewrite-to-dnf-p
                                                 thematic-substrate::report-inconsistent-queries-p
                                                 thematic-substrate::report-tautological-queries-p
                                                 thematic-substrate::use-repository-p
                                                 thematic-substrate::put-into-repository-p
                                                 thematic-substrate::id
                                                 thematic-substrate::dont-check-id-p
                                                 thematic-substrate::parser
                                                 thematic-substrate::result-vois
                                                 thematic-substrate::substrate
                                                 thematic-substrate::abox
                                                 thematic-substrate::create-abox-if-not-found-p
                                                 package
                                                 thematic-substrate::type-of-substrate
                                                 thematic-substrate::prepare-now-p))
                                 (prepare-nrql-engine (&optional
                                                       &key
                                                       thematic-substrate::mode
                                                       thematic-substrate::dont-show-variables
                                                       thematic-substrate::dont-show-lambdas
                                                       thematic-substrate::dont-show-head-projection-operators
                                                       thematic-substrate::abox-mirroring
                                                       thematic-substrate::query-optimization
                                                       optimizer-use-cardinality-heuristics
                                                       thematic-substrate::how-many-tuples
                                                       thematic-substrate::timeout
                                                       thematic-substrate::warnings
                                                       add-rule-consequences-automatically
                                                       thematic-substrate::dont-add-abox-duplicates
                                                       thematic-substrate::two-phase-query-processing-mode
                                                       thematic-substrate::phase-two-starts-warning-tokens
                                                       thematic-substrate::kb-has-changed-warning-tokens
                                                       thematic-substrate::told-information-querying
                                                       thematic-substrate::tuple-computation-mode
                                                       exclude-permutations
                                                       thematic-substrate::query-repository
                                                       thematic-substrate::report-inconsistent-queries
                                                       thematic-substrate::report-tautological-queries
                                                       thematic-substrate::query-realization
                                                       thematic-substrate::bindings
                                                       thematic-substrate::check-abox-consistency
                                                       thematic-substrate::use-individual-equivalence-classes
                                                       thematic-substrate::rewrite-to-dnf
                                                       thematic-substrate::tbox
                                                       thematic-substrate::substrate
                                                       thematic-substrate::abox
                                                       thematic-substrate::create-abox-if-not-found-p
                                                       package
                                                       thematic-substrate::type-of-substrate
                                                       thematic-substrate::prepare-now-p))
                                 (prepare-abox-rule1 (thematic-substrate::res-args
                                                      thematic-substrate::query
                                                      &key
                                                      thematic-substrate::execute-p
                                                      thematic-substrate::parser-class
                                                      thematic-substrate::rewrite-defined-concepts-p
                                                      thematic-substrate::group-by-ops
                                                      thematic-substrate::bind-specials-p
                                                      thematic-substrate::original-query
                                                      thematic-substrate::rule-con-pattern
                                                      thematic-substrate::new-ind-ops
                                                      thematic-substrate::premise
                                                      thematic-substrate::generate-code-p
                                                      thematic-substrate::optimize-p
                                                      thematic-substrate::rewrite-semantically-p
                                                      thematic-substrate::rewrite-to-dnf-p
                                                      thematic-substrate::report-inconsistent-queries-p
                                                      thematic-substrate::report-tautological-queries-p
                                                      thematic-substrate::use-repository-p
                                                      thematic-substrate::put-into-repository-p
                                                      thematic-substrate::id
                                                      thematic-substrate::dont-check-id-p
                                                      thematic-substrate::parser
                                                      thematic-substrate::result-vois
                                                      thematic-substrate::substrate
                                                      thematic-substrate::abox
                                                      thematic-substrate::create-abox-if-not-found-p
                                                      package
                                                      thematic-substrate::type-of-substrate
                                                      thematic-substrate::prepare-now-p))
                                 (prepare-abox-rule (thematic-substrate::query thematic-substrate::res-args
                                                                               &key
                                                                               thematic-substrate::execute-p
                                                                               thematic-substrate::parser-class
                                                                               thematic-substrate::rewrite-defined-concepts-p
                                                                               thematic-substrate::group-by-ops
                                                                               thematic-substrate::bind-specials-p
                                                                               thematic-substrate::original-query
                                                                               thematic-substrate::rule-con-pattern
                                                                               thematic-substrate::new-ind-ops
                                                                               thematic-substrate::premise
                                                                               thematic-substrate::generate-code-p
                                                                               thematic-substrate::optimize-p
                                                                               thematic-substrate::rewrite-semantically-p
                                                                               thematic-substrate::rewrite-to-dnf-p
                                                                               thematic-substrate::report-inconsistent-queries-p
                                                                               thematic-substrate::report-tautological-queries-p
                                                                               thematic-substrate::use-repository-p
                                                                               thematic-substrate::put-into-repository-p
                                                                               thematic-substrate::id
                                                                               thematic-substrate::dont-check-id-p
                                                                               thematic-substrate::parser
                                                                               thematic-substrate::result-vois
                                                                               thematic-substrate::substrate
                                                                               thematic-substrate::abox
                                                                               thematic-substrate::create-abox-if-not-found-p
                                                                               package
                                                                               thematic-substrate::type-of-substrate
                                                                               thematic-substrate::prepare-now-p))
                                 (prepare-abox-query1 (thematic-substrate::query thematic-substrate::res-args
                                                                                 &key
                                                                                 thematic-substrate::execute-p
                                                                                 thematic-substrate::dont-add-abox-duplicates-p
                                                                                 thematic-substrate::remove-duplicates-p
                                                                                 thematic-substrate::two-phase-processing-p
                                                                                 thematic-substrate::deliver-phase-two-warning-tokens-p
                                                                                 thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                                                 thematic-substrate::add-rule-consequences-p
                                                                                 thematic-substrate::continuation-based-instance-retrieval-p
                                                                                 thematic-substrate::told-information-reasoning-p
                                                                                 thematic-substrate::final-consistency-checking-p
                                                                                 thematic-substrate::runtime-consistency-checking-p
                                                                                 thematic-substrate::verbose-p
                                                                                 thematic-substrate::dont-show-variables
                                                                                 thematic-substrate::dont-show-head-projection-operators-p
                                                                                 thematic-substrate::dont-show-lambdas-p
                                                                                 thematic-substrate::how-many
                                                                                 thematic-substrate::only-new-tuples-p
                                                                                 thematic-substrate::timeout
                                                                                 thematic-substrate::proactive-tuple-computation-p
                                                                                 thematic-substrate::tuple-at-a-time-p
                                                                                 thematic-substrate::use-individual-synonyms-p
                                                                                 thematic-substrate::check-abox-consistency-p
                                                                                 thematic-substrate::ensure-tbox-classification-p
                                                                                 thematic-substrate::initial-abox-mirroring-p
                                                                                 thematic-substrate::initial-role-assertion-mirroring-p
                                                                                 thematic-substrate::classify-concepts-in-instance-assertions-p
                                                                                 thematic-substrate::exclude-permutations-p
                                                                                 thematic-substrate::record-explanations-p
                                                                                 thematic-substrate::parser-class
                                                                                 thematic-substrate::rewrite-defined-concepts-p
                                                                                 thematic-substrate::group-by-ops
                                                                                 thematic-substrate::bind-specials-p
                                                                                 thematic-substrate::original-query
                                                                                 thematic-substrate::rule-con-pattern
                                                                                 thematic-substrate::new-ind-ops
                                                                                 thematic-substrate::premise
                                                                                 thematic-substrate::generate-code-p
                                                                                 thematic-substrate::optimize-p
                                                                                 thematic-substrate::rewrite-semantically-p
                                                                                 thematic-substrate::rewrite-to-dnf-p
                                                                                 thematic-substrate::report-inconsistent-queries-p
                                                                                 thematic-substrate::report-tautological-queries-p
                                                                                 thematic-substrate::use-repository-p
                                                                                 thematic-substrate::put-into-repository-p
                                                                                 thematic-substrate::id
                                                                                 thematic-substrate::dont-check-id-p
                                                                                 thematic-substrate::parser
                                                                                 thematic-substrate::result-vois
                                                                                 thematic-substrate::substrate
                                                                                 thematic-substrate::abox
                                                                                 thematic-substrate::create-abox-if-not-found-p
                                                                                 package
                                                                                 thematic-substrate::type-of-substrate
                                                                                 thematic-substrate::prepare-now-p))
                                 (prepare-abox-query (thematic-substrate::res-args
                                                      thematic-substrate::query
                                                      &key
                                                      thematic-substrate::execute-p
                                                      thematic-substrate::dont-add-abox-duplicates-p
                                                      thematic-substrate::remove-duplicates-p
                                                      thematic-substrate::two-phase-processing-p
                                                      thematic-substrate::deliver-phase-two-warning-tokens-p
                                                      thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                      thematic-substrate::add-rule-consequences-p
                                                      thematic-substrate::continuation-based-instance-retrieval-p
                                                      thematic-substrate::told-information-reasoning-p
                                                      thematic-substrate::final-consistency-checking-p
                                                      thematic-substrate::runtime-consistency-checking-p
                                                      thematic-substrate::verbose-p
                                                      thematic-substrate::dont-show-variables
                                                      thematic-substrate::dont-show-head-projection-operators-p
                                                      thematic-substrate::dont-show-lambdas-p
                                                      thematic-substrate::how-many
                                                      thematic-substrate::only-new-tuples-p
                                                      thematic-substrate::timeout
                                                      thematic-substrate::proactive-tuple-computation-p
                                                      thematic-substrate::tuple-at-a-time-p
                                                      thematic-substrate::use-individual-synonyms-p
                                                      thematic-substrate::check-abox-consistency-p
                                                      thematic-substrate::ensure-tbox-classification-p
                                                      thematic-substrate::initial-abox-mirroring-p
                                                      thematic-substrate::initial-role-assertion-mirroring-p
                                                      thematic-substrate::classify-concepts-in-instance-assertions-p
                                                      thematic-substrate::exclude-permutations-p
                                                      thematic-substrate::record-explanations-p
                                                      thematic-substrate::parser-class
                                                      thematic-substrate::rewrite-defined-concepts-p
                                                      thematic-substrate::group-by-ops
                                                      thematic-substrate::bind-specials-p
                                                      thematic-substrate::original-query
                                                      thematic-substrate::rule-con-pattern
                                                      thematic-substrate::new-ind-ops
                                                      thematic-substrate::premise
                                                      thematic-substrate::generate-code-p
                                                      thematic-substrate::optimize-p
                                                      thematic-substrate::rewrite-semantically-p
                                                      thematic-substrate::rewrite-to-dnf-p
                                                      thematic-substrate::report-inconsistent-queries-p
                                                      thematic-substrate::report-tautological-queries-p
                                                      thematic-substrate::use-repository-p
                                                      thematic-substrate::put-into-repository-p
                                                      thematic-substrate::id
                                                      thematic-substrate::dont-check-id-p
                                                      thematic-substrate::parser
                                                      thematic-substrate::result-vois
                                                      thematic-substrate::substrate
                                                      thematic-substrate::abox
                                                      thematic-substrate::create-abox-if-not-found-p
                                                      package
                                                      thematic-substrate::type-of-substrate
                                                      thematic-substrate::prepare-now-p))
                                 (prepare-abox (&optional abox))
                                 (prefer-defined-queries nil)
                                 (pracer-answer-query (res-args
                                                       query
                                                       &key
                                                       id
                                                       print-query
                                                       use-optimizer))
                                 (owllink-read-file (filename
                                                     &rest
                                                     args
                                                     &key
                                                     &allow-other-keys))
                                 (owllink-read-document (url
                                                         &rest
                                                         args
                                                         &key
                                                         &allow-other-keys))
                                 (|OWLAPI-writeXMLOntologyFile| (owlapi:ontology
                                                                 owl-syntaxes::fn
                                                                 &key
                                                                 owl-syntaxes::prefixes
                                                                 owl-syntaxes::p4-mode
                                                                 owl-syntaxes::comments))
                                 (|OWLAPI-writeOntologyFile| (owlapi:ontology
                                                              owl-syntaxes::fn
                                                              &key
                                                              owl-syntaxes::prefixes
                                                              owl-syntaxes::p4-mode
                                                              owl-syntaxes::comments))
                                 (|OWLAPI-writeFunctionalOntologyFile| (owlapi:ontology
                                                                        owl-syntaxes::fn
                                                                        &key
                                                                        owl-syntaxes::prefixes
                                                                        owl-syntaxes::p4-mode
                                                                        owl-syntaxes::comments))
                                 (|OWLAPI-usesSimplifiedProtocol| (&optional
                                                                   owlapi:reasoner))
                                 (|OWLAPI-usesIncrementalUpdates| (&optional
                                                                   owlapi:reasoner))
                                 (|OWLAPI-unloadOntology| (owlapi:ontology
                                                           &optional
                                                           owlapi:reasoner))
                                 (|OWLAPI-unloadOntologies| (owlapi:ontologies &optional
                                                                               owlapi:reasoner))
                                 (|OWLAPI-unloadAxioms| (owlapi::ont
                                                         owlapi:axioms
                                                         &optional
                                                         owlapi:reasoner))
                                 (|OWLAPI-unloadAxiom| (owlapi::ont
                                                        owlapi:axiom
                                                        &optional
                                                        owlapi:reasoner))
                                 (|OWLAPI-storeImage| (owlapi::fn
                                                       &optional
                                                       owlapi::reasoners))
                                 (|OWLAPI-sleep| (owlapi::seconds
                                                  &optional
                                                  owlapi:reasoner))
                                 (|OWLAPI-setReturnPolicy| (type
                                                            &optional
                                                            owlapi:reasoner))
                                 (|OWLAPI-setCurrentReasoner| (owlapi:name &optional
                                                                           owlapi::make-racer-kb-current-p))
                                 (|OWLAPI-setAxiomCounter| (owlapi::n
                                                            &optional
                                                            owlapi:reasoner))
                                 (|OWLAPI-setAutoDeclareDataProperties| (owlapi::val
                                                                         &optional
                                                                         owlapi:reasoner))
                                 (|OWLAPI-saveOntology| (owlapi:ontology
                                                         owl-syntaxes::fn
                                                         &key
                                                         owlapi:reasoner
                                                         owl-syntaxes::syntax
                                                         owl-syntaxes::prefixes
                                                         owl-syntaxes::p4-mode
                                                         owl-syntaxes::comments))
                                 (|OWLAPI-restoreImage| (owlapi::fn))
                                 (|OWLAPI-resetAxiomCounter| (&optional
                                                              owlapi:reasoner))
                                 (|OWLAPI-removePrefix| (owlapi:prefix &optional
                                                                       owlapi:reasoner))
                                 (|OWLAPI-removeAxioms| nil)
                                 (|OWLAPI-removeAxiom| nil)
                                 (|OWLAPI-reloadLoadedOntologies| (&optional
                                                                   owlapi:reasoner))
                                 (|OWLAPI-registerReferencedEntities| (&optional
                                                                       owlapi:reasoner))
                                 (|OWLAPI-registerObject| (owlapi:obj))
                                 (|OWLAPI-registerLastAnswer| (&optional
                                                               owlapi:reasoner))
                                 (|OWLAPI-registerDeclaredEntities| (&optional
                                                                     owlapi:reasoner))
                                 (|OWLAPI-realize| (&optional
                                                    owlapi:reasoner
                                                    owlapi::check-abox-consistency-p))
                                 (|OWLAPI-readXMLOntologyFile| (owl-syntaxes::fn
                                                                &key
                                                                owl-syntaxes::strict-syntax-p
                                                                owl-syntaxes::use-flipped-class-assertions-p
                                                                owl-syntaxes::ignore-import
                                                                owl-syntaxes::ignore-annotations
                                                                owl-syntaxes::merge-imported-ontologies-p
                                                                owl-syntaxes::maintain-owlapi-axioms
                                                                owl-syntaxes::kb-name
                                                                owl-syntaxes::reasoner-name
                                                                owl-syntaxes::ontology-name
                                                                owl-syntaxes::init
                                                                owl-syntaxes::parser))
                                 (|OWLAPI-readXMLOntologyDocument| (owl-syntaxes::url
                                                                    &key
                                                                    owl-syntaxes::strict-syntax-p
                                                                    owl-syntaxes::use-flipped-class-assertions-p
                                                                    owl-syntaxes::ignore-import
                                                                    owl-syntaxes::ignore-annotations
                                                                    owl-syntaxes::merge-imported-ontologies-p
                                                                    owl-syntaxes::maintain-owlapi-axioms
                                                                    owl-syntaxes::kb-name
                                                                    owl-syntaxes::reasoner-name
                                                                    owl-syntaxes::ontology-name
                                                                    owl-syntaxes::init
                                                                    owl-syntaxes::parser))
                                 (|OWLAPI-readOntology| (owl-syntaxes::url
                                                         &key
                                                         owl-syntaxes::syntax
                                                         owl-syntaxes::strict-syntax-p
                                                         owl-syntaxes::use-flipped-class-assertions-p
                                                         owl-syntaxes::ignore-import
                                                         owl-syntaxes::ignore-annotations
                                                         owl-syntaxes::merge-imported-ontologies-p
                                                         owl-syntaxes::maintain-owlapi-axioms
                                                         owl-syntaxes::kb-name
                                                         owl-syntaxes::reasoner-name
                                                         owl-syntaxes::ontology-name
                                                         owl-syntaxes::init
                                                         owl-syntaxes::parser
                                                         verbose
                                                         init
                                                         kb-name
                                                         locator
                                                         recursive
                                                         ignore-import
                                                         ontology-name
                                                         merge-imported-ontologies-p
                                                         import-meta-ontologies
                                                         excluded-meta-ontologies
                                                         fire-rules
                                                         maintain-owlapi-axioms
                                                         ignore-annotations))
                                 (|OWLAPI-readFunctionalOntologyFile| (owl-syntaxes::fn
                                                                       &key
                                                                       owl-syntaxes::strict-syntax-p
                                                                       owl-syntaxes::use-flipped-class-assertions-p
                                                                       owl-syntaxes::ignore-import
                                                                       owl-syntaxes::ignore-annotations
                                                                       owl-syntaxes::merge-imported-ontologies-p
                                                                       owl-syntaxes::maintain-owlapi-axioms
                                                                       owl-syntaxes::kb-name
                                                                       owl-syntaxes::reasoner-name
                                                                       owl-syntaxes::ontology-name
                                                                       owl-syntaxes::init
                                                                       owl-syntaxes::parser))
                                 (|OWLAPI-readFunctionalOntologyDocument| (owl-syntaxes::url
                                                                           &key
                                                                           owl-syntaxes::strict-syntax-p
                                                                           owl-syntaxes::use-flipped-class-assertions-p
                                                                           owl-syntaxes::ignore-import
                                                                           owl-syntaxes::ignore-annotations
                                                                           owl-syntaxes::merge-imported-ontologies-p
                                                                           owl-syntaxes::maintain-owlapi-axioms
                                                                           owl-syntaxes::kb-name
                                                                           owl-syntaxes::reasoner-name
                                                                           owl-syntaxes::ontology-name
                                                                           owl-syntaxes::init
                                                                           owl-syntaxes::parser))
                                 (|OWLAPI-parseNative| (string &optional
                                                               owlapi:reasoner))
                                 (|OWLAPI-parse| (owl-syntaxes::args
                                                  &optional
                                                  owlapi:reasoner))
                                 (|OWLAPI-nextAxiomUseID| (owlapi::id
                                                           &optional
                                                           owlapi:reasoner))
                                 (|OWLAPI-newReasoner1| (&optional
                                                         owlapi:owlapi-reasoner-name
                                                         owlapi::make-racer-kb-current-p
                                                         owlapi::init
                                                         owlapi:owlapi-tbox
                                                         owlapi:owlapi-abox))
                                 (|OWLAPI-newReasoner| (&optional
                                                        owlapi:owlapi-reasoner-name
                                                        owlapi::make-racer-kb-current-p
                                                        owlapi::init
                                                        owlapi:owlapi-tbox
                                                        owlapi:owlapi-abox
                                                        owlapi::own-racer-p))
                                 (|OWLAPI-newOntology| (owlapi:name &optional
                                                                    owlapi:reasoner
                                                                    owlapi:secondary-p))
                                 (|OWLAPI-mergeOntologies| (owlapi::ont1
                                                            owlapi::ont2
                                                            &optional
                                                            owlapi:reasoner))
                                 (|OWLAPI-manuallyApplyChanges| (&optional
                                                                 owlapi:reasoner))
                                 (|OWLAPI-loadOntology| (owlapi:ontology
                                                         &optional
                                                         owlapi:reasoner))
                                 (|OWLAPI-loadOntologies| (owlapi:ontologies &optional
                                                                             owlapi:reasoner))
                                 (|OWLAPI-loadAxioms| (owlapi::ont
                                                       owlapi:axioms
                                                       &optional
                                                       owlapi:reasoner))
                                 (|OWLAPI-loadAxiom| (owlapi::ont
                                                      owlapi:axiom
                                                      &optional
                                                      owlapi:reasoner))
                                 (|OWLAPI-keepAnnotations| (&optional
                                                            owlapi:reasoner))
                                 (|OWLAPI-isTransitive| (owlapi:property
                                                         &optional
                                                         owlapi:reasoner))
                                 (|OWLAPI-isSymmetric| (owlapi:property
                                                        &optional
                                                        owlapi:reasoner))
                                 (|OWLAPI-isSubClassOf| (owlapi:clsc
                                                         owlapi:clsd
                                                         &optional
                                                         owlapi:reasoner))
                                 (|OWLAPI-isSatisfiable| (owlapi:description &optional
                                                                             owlapi:reasoner))
                                 (|OWLAPI-isSameIndividual| (owlapi::i
                                                             owlapi::j
                                                             &optional
                                                             owlapi:reasoner))
                                 (|OWLAPI-isReflexive| (owlapi:property
                                                        &optional
                                                        owlapi:reasoner))
                                 (|OWLAPI-isRealised| (&optional
                                                       owlapi:reasoner))
                                 (|OWLAPI-isIrreflexive| (owlapi:property
                                                          &optional
                                                          owlapi:reasoner))
                                 (|OWLAPI-isInverseFunctional| (owlapi:property
                                                                &optional
                                                                owlapi:reasoner))
                                 (|OWLAPI-isFunctional| (owlapi:property
                                                         &optional
                                                         owlapi:reasoner))
                                 (|OWLAPI-isEquivalentClass| (owlapi:clsc
                                                              owlapi:clsd
                                                              &optional
                                                              owlapi:reasoner))
                                 (|OWLAPI-isEntailed| (owlapi::axiom-id-or-constructor
                                                       &optional
                                                       owlapi:reasoner))
                                 (|OWLAPI-isDifferentIndividual| (owlapi::i
                                                                  owlapi::j
                                                                  &optional
                                                                  owlapi:reasoner))
                                 (|OWLAPI-isDefinedObjectProperty| (owlapi:property
                                                                    &optional
                                                                    owlapi:reasoner))
                                 (|OWLAPI-isDefinedIndividual| (owlapi::ind
                                                                &optional
                                                                owlapi:reasoner))
                                 (|OWLAPI-isDefinedDataProperty| (owlapi:property
                                                                  &optional
                                                                  owlapi:reasoner))
                                 (|OWLAPI-isDefinedClass| (owlapi:cls
                                                           &optional
                                                           owlapi:reasoner))
                                 (|OWLAPI-isConsistent| (owlapi:ontology
                                                         &optional
                                                         owlapi:reasoner
                                                         owlapi::force-p))
                                 (|OWLAPI-isClassified| (&optional
                                                         owlapi:reasoner))
                                 (|OWLAPI-isClass| (owlapi:clsc
                                                    &optional
                                                    owlapi:reasoner))
                                 (|OWLAPI-isAsymmetric| (owlapi:property
                                                         &optional
                                                         owlapi:reasoner))
                                 (|OWLAPI-init| nil)
                                 (|OWLAPI-ignoreDeclarations| (&optional
                                                               owlapi:reasoner))
                                 (|OWLAPI-ignoreAnnotations| (&optional
                                                              owlapi:reasoner))
                                 (|OWLAPI-hasType| (owlapi::ind
                                                    type
                                                    owlapi::direct
                                                    &optional
                                                    owlapi:reasoner))
                                 (|OWLAPI-hasObjectPropertyRelationship| (owlapi:subject owlapi:property
                                                                                         owlapi:object
                                                                                         &optional
                                                                                         owlapi:reasoner))
                                 (|OWLAPI-hasDataPropertyRelationship| (owlapi:subject owlapi:property
                                                                                       owlapi:object
                                                                                       &optional
                                                                                       owlapi:reasoner))
                                 (|OWLAPI-getTypes| (owlapi:individual
                                                     owlapi::direct
                                                     &optional
                                                     owlapi:reasoner))
                                 (|OWLAPI-getSuperProperties| (owlapi:property
                                                               &optional
                                                               owlapi:reasoner))
                                 (|OWLAPI-getSuperClasses| (owlapi:cls
                                                            &optional
                                                            owlapi:reasoner))
                                 (|OWLAPI-getSubProperties| (owlapi:property
                                                             &optional
                                                             owlapi:reasoner))
                                 (|OWLAPI-getSubClasses| (owlapi:cls
                                                          &optional
                                                          owlapi:reasoner))
                                 (|OWLAPI-getSameIndividuals| (owlapi::ind
                                                               &optional
                                                               owlapi:reasoner))
                                 (|OWLAPI-getRelatedValues| (owlapi:subject owlapi:data-property
                                                                            &optional
                                                                            owlapi:reasoner))
                                 (|OWLAPI-getRelatedIndividuals| (owlapi:subject owlapi:object-property
                                                                                 &optional
                                                                                 owlapi:reasoner))
                                 (|OWLAPI-getReasoners| nil)
                                 (|OWLAPI-getRanges| (owlapi:property
                                                      &optional
                                                      owlapi:reasoner
                                                      owlapi::owlapi-hacking-mode))
                                 (|OWLAPI-getPrefixes| (&optional
                                                        owlapi:reasoner))
                                 (|OWLAPI-getOntologies| (&optional
                                                          owlapi:reasoner))
                                 (|OWLAPI-getObjectPropertyValues| (owlapi::ind
                                                                    owlapi:property
                                                                    &optional
                                                                    owlapi:reasoner
                                                                    owlapi::synonyms))
                                 (|OWLAPI-getObjectPropertyRelationships| (owlapi::ind
                                                                           &optional
                                                                           owlapi:reasoner))
                                 (|OWLAPI-getOWLTransitiveObjectPropertyAxiom| (owlapi:object-property &optional
                                                                                                       owlapi:reasoner))
                                 (|OWLAPI-getOWLSymmetricObjectPropertyAxiom| (owlapi:object-property &optional
                                                                                                      owlapi:reasoner))
                                 (|OWLAPI-getOWLSubClassAxiom| (owlapi:sub-class
                                                                owlapi:super-class
                                                                &optional
                                                                owlapi:reasoner))
                                 (|OWLAPI-getOWLSubAnnotationPropertyOfAxiom| (owlapi:annotation-sub-property owlapi:annotation-super-property
                                                                                                              &optional
                                                                                                              owlapi:reasoner))
                                 (|OWLAPI-getOWLSubAnnotationPropertyAxiom| nil)
                                 (|OWLAPI-getOWLSameIndividualsAxiom| (owlapi:individuals &optional
                                                                                          owlapi:reasoner))
                                 (|OWLAPI-getOWLReflexiveObjectPropertyAxiom| (owlapi:object-property &optional
                                                                                                      owlapi:reasoner))
                                 (|OWLAPI-getOWLReallyImplicitDeclarationAxiom| (owlapi:entity &optional
                                                                                               owlapi:reasoner))
                                 (|OWLAPI-getOWLPrefixDeclarationAxiom| (owlapi::namespace-prefix
                                                                         owlapi:namespace
                                                                         &optional
                                                                         owlapi:reasoner))
                                 (|OWLAPI-getOWLOntologyVersionDeclarationAxiom| (owlapi::ontology-version-uri
                                                                                  &optional
                                                                                  owlapi:reasoner))
                                 (|OWLAPI-getOWLOntologyAnnotationAxiom| (owlapi:annotation &optional
                                                                                            owlapi:reasoner))
                                 (|OWLAPI-getOWLObjectSubPropertyAxiom| (owlapi:object-sub-property
                                                                         owlapi:object-super-property
                                                                         &optional
                                                                         owlapi:reasoner))
                                 (|OWLAPI-getOWLObjectPropertyRangeAxiom| (owlapi:object-property owlapi:object-property-range
                                                                                                  &optional
                                                                                                  owlapi:reasoner))
                                 (|OWLAPI-getOWLObjectPropertyDomainAxiom| (owlapi:object-property owlapi:object-property-domain
                                                                                                   &optional
                                                                                                   owlapi:reasoner))
                                 (|OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom| (owlapi:object-property-chain owlapi:object-super-property
                                                                                                                   &optional
                                                                                                                   owlapi:reasoner))
                                 (|OWLAPI-getOWLObjectPropertyAssertionAxiom| (owlapi:subject owlapi:rel-object-property
                                                                                              owlapi:object
                                                                                              &optional
                                                                                              owlapi:reasoner))
                                 (|OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom| (owlapi:subject owlapi:rel-object-property
                                                                                                      owlapi:object
                                                                                                      &optional
                                                                                                      owlapi:reasoner))
                                 (|OWLAPI-getOWLNegativeDataPropertyAssertionAxiom| (owlapi:subject owlapi:rel-data-property
                                                                                                    owlapi::value
                                                                                                    &optional
                                                                                                    owlapi:reasoner))
                                 (|OWLAPI-getOWLIrreflexiveObjectPropertyAxiom| (owlapi:object-property &optional
                                                                                                        owlapi:reasoner))
                                 (|OWLAPI-getOWLInverseObjectPropertiesAxiom| (owlapi:first-object-property
                                                                               owlapi:second-object-property
                                                                               &optional
                                                                               owlapi:reasoner))
                                 (|OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom| (owlapi:object-property &optional
                                                                                                              owlapi:reasoner))
                                 (|OWLAPI-getOWLImportsDeclarationAxiom| (owlapi::ontology-import-uri
                                                                          &optional
                                                                          owlapi:reasoner))
                                 (|OWLAPI-getOWLImplicitDeclarationAxiom| (owlapi:entity &optional
                                                                                         owlapi:reasoner))
                                 (|OWLAPI-getOWLHasKeyAxiom| (owlapi:key-class owlapi:key-object-properties
                                                                               owlapi:key-data-properties
                                                                               &optional
                                                                               owlapi:reasoner))
                                 (|OWLAPI-getOWLFunctionalObjectPropertyAxiom| (owlapi:object-property &optional
                                                                                                       owlapi:reasoner))
                                 (|OWLAPI-getOWLFunctionalDataPropertyAxiom| (owlapi:data-property &optional
                                                                                                   owlapi:reasoner))
                                 (|OWLAPI-getOWLEquivalentObjectPropertiesAxiom| (owlapi:object-properties &optional
                                                                                                           owlapi:reasoner))
                                 (|OWLAPI-getOWLEquivalentDataPropertiesAxiom| (owlapi:data-properties &optional
                                                                                                       owlapi:reasoner))
                                 (|OWLAPI-getOWLEquivalentClassesAxiom| (owlapi:descriptions &optional
                                                                                             owlapi:reasoner))
                                 (|OWLAPI-getOWLEntityAnnotationAxiom| (owlapi:entity owlapi:annotation
                                                                                      &optional
                                                                                      owlapi:reasoner))
                                 (|OWLAPI-getOWLDisjointUnionAxiom| (owlapi:description owlapi:descriptions
                                                                                        &optional
                                                                                        owlapi:reasoner))
                                 (|OWLAPI-getOWLDisjointObjectPropertiesAxiom| (owlapi:object-properties &optional
                                                                                                         owlapi:reasoner))
                                 (|OWLAPI-getOWLDisjointDataPropertiesAxiom| (owlapi:data-properties &optional
                                                                                                     owlapi:reasoner))
                                 (|OWLAPI-getOWLDisjointClassesAxiom| (owlapi:descriptions &optional
                                                                                           owlapi:reasoner))
                                 (|OWLAPI-getOWLDifferentIndividualsAxiom| (owlapi:individuals &optional
                                                                                               owlapi:reasoner))
                                 (|OWLAPI-getOWLDeclarationAxiom| (owlapi:entity &optional
                                                                                 owlapi:reasoner))
                                 (|OWLAPI-getOWLDatatypeDefinitionAxiom| (owlapi:datatype-name owlapi:data-range
                                                                                               &optional
                                                                                               owlapi:reasoner))
                                 (|OWLAPI-getOWLDataSubPropertyAxiom| (owlapi:data-sub-property
                                                                       owlapi:data-super-property
                                                                       &optional
                                                                       owlapi:reasoner))
                                 (|OWLAPI-getOWLDataPropertyRangeAxiom| (owlapi:data-property owlapi:data-range
                                                                                              &optional
                                                                                              owlapi:reasoner))
                                 (|OWLAPI-getOWLDataPropertyDomainAxiom| (owlapi:data-property owlapi:data-property-domain
                                                                                               &optional
                                                                                               owlapi:reasoner))
                                 (|OWLAPI-getOWLDataPropertyAssertionAxiom| (owlapi:subject owlapi:rel-data-property
                                                                                            owlapi::value
                                                                                            &optional
                                                                                            owlapi:reasoner))
                                 (|OWLAPI-getOWLClassAssertionAxiom| (owlapi:individual
                                                                      owlapi:description
                                                                      &optional
                                                                      owlapi:reasoner))
                                 (|OWLAPI-getOWLAxiomAnnotationAxiom| (owlapi:axiom-id owlapi:annotation
                                                                                       &optional
                                                                                       owlapi:reasoner))
                                 (|OWLAPI-getOWLAsymmetricObjectPropertyAxiom| (owlapi:object-property &optional
                                                                                                       owlapi:reasoner))
                                 (|OWLAPI-getOWLAnnotationPropertyRangeAxiom| (owlapi:annotation-property owlapi:annotation-property-range
                                                                                                          &optional
                                                                                                          owlapi:reasoner))
                                 (|OWLAPI-getOWLAnnotationPropertyDomainAxiom| (owlapi:annotation-property owlapi:annotation-property-domain
                                                                                                           &optional
                                                                                                           owlapi:reasoner))
                                 (|OWLAPI-getOWLAnnotationAssertionAxiom| (owlapi:annotation-subject owlapi:annotation-property
                                                                                                     owlapi:annotation-value
                                                                                                     &optional
                                                                                                     owlapi:reasoner))
                                 (|OWLAPI-getLoadedOntologies| (&optional
                                                                owlapi:reasoner))
                                 (|OWLAPI-getInverseProperties| (owlapi:property
                                                                 &optional
                                                                 owlapi:reasoner))
                                 (|OWLAPI-getInstances| (class
                                                         owlapi::direct
                                                         &optional
                                                         owlapi:reasoner
                                                         owlapi::synonyms))
                                 (|OWLAPI-getIndividuals| (class
                                                           owlapi::direct
                                                           &optional
                                                           owlapi:reasoner))
                                 (|OWLAPI-getInconsistentClasses| (&optional
                                                                   owlapi:reasoner))
                                 (|OWLAPI-getEquivalentProperties| (owlapi:property
                                                                    &optional
                                                                    owlapi:reasoner
                                                                    owlapi::remove-self-p))
                                 (|OWLAPI-getEquivalentClasses| (owlapi:cls
                                                                 &optional
                                                                 owlapi:reasoner))
                                 (|OWLAPI-getDomains| (owlapi:property
                                                       &optional
                                                       owlapi:reasoner
                                                       owlapi::owlapi-hacking-mode))
                                 (|OWLAPI-getDisjointObjectProperties| (owlapi:property
                                                                        &optional
                                                                        owlapi:reasoner))
                                 (|OWLAPI-getDisjointDataProperties| (owlapi:property
                                                                      &optional
                                                                      owlapi:reasoner))
                                 (|OWLAPI-getDisjointClasses| (owlapi::concept
                                                               &optional
                                                               owlapi:reasoner))
                                 (|OWLAPI-getDifferentIndividuals| (owlapi::ind
                                                                    &optional
                                                                    owlapi:reasoner
                                                                    owlapi::synonyms))
                                 (|OWLAPI-getDescendantProperties| (owlapi:property
                                                                    &optional
                                                                    owlapi:reasoner
                                                                    owlapi::remove-self-p))
                                 (|OWLAPI-getDescendantClasses| (owlapi:cls
                                                                 &optional
                                                                 owlapi:reasoner))
                                 (|OWLAPI-getDataPropertyValues| (owlapi::ind
                                                                  owlapi:property
                                                                  &optional
                                                                  owlapi:reasoner))
                                 (|OWLAPI-getDataPropertyRelationships| (owlapi::ind
                                                                         &optional
                                                                         owlapi:reasoner))
                                 (|OWLAPI-getCurrentReasoner| nil)
                                 (|OWLAPI-getChanges| (&optional
                                                       owlapi:reasoner))
                                 (|OWLAPI-getAxiomsPerOntology| (&optional
                                                                 owlapi:reasoner))
                                 (|OWLAPI-getAxiomsOfTypeIn| (owlapi::type-or-types
                                                              owlapi::ont
                                                              &optional
                                                              owlapi:reasoner
                                                              owlapi:with-ids-p
                                                              owlapi::status))
                                 (|OWLAPI-getAxiomsOfType| (owlapi::type-or-types
                                                            &optional
                                                            owlapi:reasoner
                                                            owlapi:with-ids-p
                                                            owlapi::with-ont-names-p
                                                            owlapi::status))
                                 (|OWLAPI-getAxiomsIn| (owlapi::ont
                                                        &optional
                                                        owlapi:reasoner
                                                        owlapi:with-ids-p
                                                        owlapi::status))
                                 (|OWLAPI-getAxioms| (&optional
                                                      owlapi:reasoner
                                                      owlapi:with-ids-p
                                                      owlapi::with-ont-names-p
                                                      owlapi::status))
                                 (|OWLAPI-getAxiomCounter| (&optional
                                                            owlapi:reasoner))
                                 (|OWLAPI-getAutoOntology| (&optional
                                                            owlapi:reasoner))
                                 (|OWLAPI-getAutoDeclareDataProperties| (&optional
                                                                         owlapi:reasoner))
                                 (|OWLAPI-getAnnotationAxiomsForAxiom| (owlapi:axiom-id &optional
                                                                                        owlapi:reasoner))
                                 (|OWLAPI-getAncestorProperties| (owlapi:property
                                                                  &optional
                                                                  owlapi:reasoner
                                                                  owlapi::remove-self-p))
                                 (|OWLAPI-getAncestorClasses| (owlapi:cls
                                                               &optional
                                                               owlapi:reasoner))
                                 (|OWLAPI-getAllOntologies| nil)
                                 (|OWLAPI-findObjectFromID| (owlapi::id))
                                 (|OWLAPI-findIDFromObject| (owlapi:obj))
                                 (|OWLAPI-exportReasoner| (owlapi:reasoner
                                                           owlapi::fn
                                                           &key
                                                           owlapi::syntax
                                                           owlapi::quoted
                                                           owlapi::init))
                                 (|OWLAPI-exportOntology| (owlapi:ontology
                                                           owlapi::fn
                                                           &key
                                                           owlapi:reasoner
                                                           owlapi::syntax
                                                           owlapi::quoted
                                                           owlapi::init
                                                           owlapi::header))
                                 (|OWLAPI-enableTransientAxiomMode| (&optional
                                                                     owlapi:reasoner))
                                 (|OWLAPI-enableSimplifiedProtocol| (&optional
                                                                     owlapi:reasoner))
                                 (|OWLAPI-enableMemorySavingMode| (owlapi:ontology
                                                                   &optional
                                                                   owlapi:reasoner
                                                                   owlapi::use-less-tbox-memory-p))
                                 (|OWLAPI-enableLookupMode| (&optional
                                                             owlapi:reasoner))
                                 (|OWLAPI-enableIncrementalUpdates| (&optional
                                                                     owlapi:reasoner))
                                 (|OWLAPI-dontRegisterReferencedEntities| (&optional
                                                                           owlapi:reasoner))
                                 (|OWLAPI-dontRegisterDeclaredEntities| (&optional
                                                                         owlapi:reasoner))
                                 (|OWLAPI-disposeReasoner| (owlapi:name))
                                 (|OWLAPI-disposeOntology| (owlapi::ont-name
                                                            &optional
                                                            owlapi:reasoner
                                                            owlapi:dispose-axioms-p))
                                 (|OWLAPI-disposeOntologies| (owlapi:ontologies &optional
                                                                                owlapi:reasoner))
                                 (|OWLAPI-disposeAxioms| (owlapi::ids-or-constructors
                                                          &optional
                                                          owlapi:reasoner))
                                 (|OWLAPI-disposeAxiom| (owlapi::id-or-constructor
                                                         &optional
                                                         owlapi:reasoner))
                                 (|OWLAPI-dispose| nil)
                                 (|OWLAPI-disableTransientAxiomMode| (&optional
                                                                      owlapi:reasoner))
                                 (|OWLAPI-disableSimplifiedProtocol| (&optional
                                                                      owlapi:reasoner))
                                 (|OWLAPI-disableMemorySavingMode| (owlapi:reasoner))
                                 (|OWLAPI-disableLookupMode| (&optional
                                                              owlapi:reasoner))
                                 (|OWLAPI-disableIncrementalUpdates| (&optional
                                                                      owlapi:reasoner))
                                 (|OWLAPI-disableAutoMode| (&optional
                                                            owlapi:reasoner))
                                 (|OWLAPI-describeReasoners| nil)
                                 (|OWLAPI-describeReasoner| (&optional
                                                             owlapi:reasoner))
                                 (|OWLAPI-describeOntology| (owlapi:ontology
                                                             &optional
                                                             owlapi:reasoner))
                                 (|OWLAPI-describeOntologies| (&optional
                                                               owlapi:reasoner))
                                 (|OWLAPI-contains| (owlapi::ont-name
                                                     &optional
                                                     owlapi:reasoner))
                                 (|OWLAPI-considerDeclarations| (&optional
                                                                 owlapi:reasoner))
                                 (|OWLAPI-clearRegistry| (&optional
                                                          owlapi:reasoner))
                                 (|OWLAPI-clearOntologies| (&optional
                                                            owlapi:reasoner))
                                 (|OWLAPI-clearChanges| (&optional
                                                         owlapi:reasoner))
                                 (|OWLAPI-classify| (&optional
                                                     owlapi:reasoner
                                                     owlapi::check-abox-consistency-p))
                                 (|OWLAPI-batchSynchronize| (owlapi:ontology
                                                             &optional
                                                             owlapi:reasoner))
                                 (|OWLAPI-autoRemoveAxiomsFrom| (owlapi:ontology
                                                                 &optional
                                                                 owlapi:reasoner))
                                 (|OWLAPI-autoBatchRemoveAxiomsFrom| (owlapi:ontology
                                                                      &optional
                                                                      owlapi:reasoner))
                                 (|OWLAPI-autoBatchAddAxiomsTo| (owlapi:ontology
                                                                 &optional
                                                                 owlapi:reasoner))
                                 (|OWLAPI-autoApplyChanges| (&optional
                                                             owlapi:reasoner))
                                 (|OWLAPI-autoAddAxiomsTo| (owlapi:ontology
                                                            &optional
                                                            owlapi:reasoner))
                                 (|OWLAPI-applyChanges| (&optional
                                                         owlapi:reasoner))
                                 (|OWLAPI-addPrefix| (owlapi:prefix owlapi:namespace
                                                                    &optional
                                                                    owlapi:reasoner))
                                 (|OWLAPI-addAxioms| nil)
                                 (|OWLAPI-addAxiom| nil)
                                 (|OWLAPI-abort| (&optional
                                                  owlapi:reasoner))
                                 (owlapi-write-xml-ontology-file (owlapi:ontology
                                                                  owl-syntaxes::fn
                                                                  &key
                                                                  owl-syntaxes::prefixes
                                                                  owl-syntaxes::p4-mode
                                                                  owl-syntaxes::comments))
                                 (owlapi-write-ontology-file (owlapi:ontology
                                                              owl-syntaxes::fn
                                                              &key
                                                              owl-syntaxes::prefixes
                                                              owl-syntaxes::p4-mode
                                                              owl-syntaxes::comments))
                                 (owlapi-write-functional-ontology-file (owlapi:ontology
                                                                         owl-syntaxes::fn
                                                                         &key
                                                                         owl-syntaxes::prefixes
                                                                         owl-syntaxes::p4-mode
                                                                         owl-syntaxes::comments))
                                 (owlapi-uses-simplified-protocol (&optional
                                                                   owlapi:reasoner))
                                 (owlapi-uses-incremental-updates (&optional
                                                                   owlapi:reasoner))
                                 (owlapi-unload-ontology (owlapi:ontology
                                                          &optional
                                                          owlapi:reasoner))
                                 (owlapi-unload-ontologies (owlapi:ontologies &optional
                                                                              owlapi:reasoner))
                                 (owlapi-unload-axioms (owlapi::ont
                                                        owlapi:axioms
                                                        &optional
                                                        owlapi:reasoner))
                                 (owlapi-unload-axiom (owlapi::ont
                                                       owlapi:axiom
                                                       &optional
                                                       owlapi:reasoner))
                                 (|OWLAPI-SetOntologyURI| (owlapi::ont
                                                           owlapi:uri
                                                           &optional
                                                           owlapi:reasoner))
                                 (owlapi-store-image (owlapi::fn
                                                      &optional
                                                      owlapi::reasoners))
                                 (owlapi-sleep (owlapi::seconds
                                                &optional
                                                owlapi:reasoner))
                                 (owlapi-set-return-policy (type
                                                            &optional
                                                            owlapi:reasoner))
                                 (owlapi-set-ontology-uri (owlapi::ont
                                                           owlapi:uri
                                                           &optional
                                                           owlapi:reasoner))
                                 (owlapi-set-current-reasoner (owlapi:name &optional
                                                                           owlapi::make-racer-kb-current-p))
                                 (owlapi-set-axiom-counter (owlapi::n
                                                            &optional
                                                            owlapi:reasoner))
                                 (owlapi-set-auto-declare-data-properties (owlapi::val
                                                                           &optional
                                                                           owlapi:reasoner))
                                 (owlapi-save-ontology (owlapi:ontology
                                                        owl-syntaxes::fn
                                                        &key
                                                        owlapi:reasoner
                                                        owl-syntaxes::syntax
                                                        owl-syntaxes::prefixes
                                                        owl-syntaxes::p4-mode
                                                        owl-syntaxes::comments))
                                 (|OWLAPI-RemoveAxioms| (owlapi::ont
                                                         owlapi:axioms
                                                         &optional
                                                         owlapi:reasoner))
                                 (|OWLAPI-RemoveAxiom| (owlapi::ont
                                                        owlapi:axiom
                                                        &optional
                                                        owlapi:reasoner))
                                 (owlapi-restore-image (owlapi::fn))
                                 (owlapi-reset-axiom-counter (&optional
                                                              owlapi:reasoner))
                                 (owlapi-remove-prefix (owlapi:prefix &optional
                                                                      owlapi:reasoner))
                                 (owlapi-remove-axioms (owlapi::ont
                                                        owlapi:axioms
                                                        &optional
                                                        owlapi:reasoner))
                                 (owlapi-remove-axiom (owlapi::ont
                                                       owlapi:axiom
                                                       &optional
                                                       owlapi:reasoner))
                                 (owlapi-reload-loaded-ontologies (&optional
                                                                   owlapi:reasoner))
                                 (owlapi-register-referenced-entities (&optional
                                                                       owlapi:reasoner))
                                 (owlapi-register-object (owlapi:obj))
                                 (owlapi-register-last-answer (&optional
                                                               owlapi:reasoner))
                                 (owlapi-register-declared-entities (&optional
                                                                     owlapi:reasoner))
                                 (owlapi-realize (&optional
                                                  owlapi:reasoner
                                                  owlapi::check-abox-consistency-p))
                                 (owlapi-read-xml-ontology-file (owl-syntaxes::fn
                                                                 &key
                                                                 owl-syntaxes::strict-syntax-p
                                                                 owl-syntaxes::use-flipped-class-assertions-p
                                                                 owl-syntaxes::ignore-import
                                                                 owl-syntaxes::ignore-annotations
                                                                 owl-syntaxes::merge-imported-ontologies-p
                                                                 owl-syntaxes::maintain-owlapi-axioms
                                                                 owl-syntaxes::kb-name
                                                                 owl-syntaxes::reasoner-name
                                                                 owl-syntaxes::ontology-name
                                                                 owl-syntaxes::init
                                                                 owl-syntaxes::parser))
                                 (owlapi-read-xml-ontology-document (owl-syntaxes::url
                                                                     &key
                                                                     owl-syntaxes::strict-syntax-p
                                                                     owl-syntaxes::use-flipped-class-assertions-p
                                                                     owl-syntaxes::ignore-import
                                                                     owl-syntaxes::ignore-annotations
                                                                     owl-syntaxes::merge-imported-ontologies-p
                                                                     owl-syntaxes::maintain-owlapi-axioms
                                                                     owl-syntaxes::kb-name
                                                                     owl-syntaxes::reasoner-name
                                                                     owl-syntaxes::ontology-name
                                                                     owl-syntaxes::init
                                                                     owl-syntaxes::parser))
                                 (owlapi-read-ontology (owl-syntaxes::url
                                                        &key
                                                        owl-syntaxes::syntax
                                                        owl-syntaxes::strict-syntax-p
                                                        owl-syntaxes::use-flipped-class-assertions-p
                                                        owl-syntaxes::ignore-import
                                                        owl-syntaxes::ignore-annotations
                                                        owl-syntaxes::merge-imported-ontologies-p
                                                        owl-syntaxes::maintain-owlapi-axioms
                                                        owl-syntaxes::kb-name
                                                        owl-syntaxes::reasoner-name
                                                        owl-syntaxes::ontology-name
                                                        owl-syntaxes::init
                                                        owl-syntaxes::parser
                                                        verbose
                                                        init
                                                        kb-name
                                                        locator
                                                        recursive
                                                        ignore-import
                                                        ontology-name
                                                        merge-imported-ontologies-p
                                                        import-meta-ontologies
                                                        excluded-meta-ontologies
                                                        fire-rules
                                                        maintain-owlapi-axioms
                                                        ignore-annotations))
                                 (owlapi-read-functional-ontology-file (owl-syntaxes::fn
                                                                        &key
                                                                        owl-syntaxes::strict-syntax-p
                                                                        owl-syntaxes::use-flipped-class-assertions-p
                                                                        owl-syntaxes::ignore-import
                                                                        owl-syntaxes::ignore-annotations
                                                                        owl-syntaxes::merge-imported-ontologies-p
                                                                        owl-syntaxes::maintain-owlapi-axioms
                                                                        owl-syntaxes::kb-name
                                                                        owl-syntaxes::reasoner-name
                                                                        owl-syntaxes::ontology-name
                                                                        owl-syntaxes::init
                                                                        owl-syntaxes::parser))
                                 (owlapi-read-functional-ontology-document (owl-syntaxes::url
                                                                            &key
                                                                            owl-syntaxes::strict-syntax-p
                                                                            owl-syntaxes::use-flipped-class-assertions-p
                                                                            owl-syntaxes::ignore-import
                                                                            owl-syntaxes::ignore-annotations
                                                                            owl-syntaxes::merge-imported-ontologies-p
                                                                            owl-syntaxes::maintain-owlapi-axioms
                                                                            owl-syntaxes::kb-name
                                                                            owl-syntaxes::reasoner-name
                                                                            owl-syntaxes::ontology-name
                                                                            owl-syntaxes::init
                                                                            owl-syntaxes::parser))
                                 (owlapi-parse-native (string &optional
                                                              owlapi:reasoner))
                                 (owlapi-parse (owl-syntaxes::args
                                                &optional
                                                owlapi:reasoner))
                                 (owlapi-next-axiom-use-id (owlapi::id
                                                            &optional
                                                            owlapi:reasoner))
                                 (owlapi-new-reasoner1 (&optional
                                                        owlapi:owlapi-reasoner-name
                                                        owlapi::make-racer-kb-current-p
                                                        owlapi::init
                                                        owlapi:owlapi-tbox
                                                        owlapi:owlapi-abox))
                                 (owlapi-new-reasoner (&optional
                                                       owlapi:owlapi-reasoner-name
                                                       owlapi::make-racer-kb-current-p
                                                       owlapi::init
                                                       owlapi:owlapi-tbox
                                                       owlapi:owlapi-abox
                                                       owlapi::own-racer-p))
                                 (owlapi-new-ontology (owlapi:name &optional
                                                                   owlapi:reasoner
                                                                   owlapi:secondary-p))
                                 (owlapi-merge-ontologies (owlapi::ont1
                                                           owlapi::ont2
                                                           &optional
                                                           owlapi:reasoner))
                                 (owlapi-manually-apply-changes (&optional
                                                                 owlapi:reasoner))
                                 (owlapi-load-ontology (owlapi:ontology
                                                        &optional
                                                        owlapi:reasoner))
                                 (owlapi-load-ontologies (owlapi:ontologies &optional
                                                                            owlapi:reasoner))
                                 (owlapi-load-axioms (owlapi::ont
                                                      owlapi:axioms
                                                      &optional
                                                      owlapi:reasoner))
                                 (owlapi-load-axiom (owlapi::ont
                                                     owlapi:axiom
                                                     &optional
                                                     owlapi:reasoner))
                                 (owlapi-keep-annotations (&optional
                                                           owlapi:reasoner))
                                 (owlapi-is-transitive (owlapi:property
                                                        &optional
                                                        owlapi:reasoner))
                                 (owlapi-is-symmetric (owlapi:property
                                                       &optional
                                                       owlapi:reasoner))
                                 (owlapi-is-sub-class-of (owlapi:clsc
                                                          owlapi:clsd
                                                          &optional
                                                          owlapi:reasoner))
                                 (owlapi-is-satisfiable (owlapi:description &optional
                                                                            owlapi:reasoner))
                                 (owlapi-is-same-individual (owlapi::i
                                                             owlapi::j
                                                             &optional
                                                             owlapi:reasoner))
                                 (owlapi-is-reflexive (owlapi:property
                                                       &optional
                                                       owlapi:reasoner))
                                 (owlapi-is-realised (&optional
                                                      owlapi:reasoner))
                                 (owlapi-is-irreflexive (owlapi:property
                                                         &optional
                                                         owlapi:reasoner))
                                 (owlapi-is-inverse-functional (owlapi:property
                                                                &optional
                                                                owlapi:reasoner))
                                 (owlapi-is-functional (owlapi:property
                                                        &optional
                                                        owlapi:reasoner))
                                 (owlapi-is-equivalent-class (owlapi:clsc
                                                              owlapi:clsd
                                                              &optional
                                                              owlapi:reasoner))
                                 (owlapi-is-entailed (owlapi::axiom-id-or-constructor
                                                      &optional
                                                      owlapi:reasoner))
                                 (owlapi-is-different-individual (owlapi::i
                                                                  owlapi::j
                                                                  &optional
                                                                  owlapi:reasoner))
                                 (owlapi-is-defined-object-property (owlapi:property
                                                                     &optional
                                                                     owlapi:reasoner))
                                 (owlapi-is-defined-individual (owlapi::ind
                                                                &optional
                                                                owlapi:reasoner))
                                 (owlapi-is-defined-data-property (owlapi:property
                                                                   &optional
                                                                   owlapi:reasoner))
                                 (owlapi-is-defined-class (owlapi:cls
                                                           &optional
                                                           owlapi:reasoner))
                                 (owlapi-is-consistent (owlapi:ontology
                                                        &optional
                                                        owlapi:reasoner
                                                        owlapi::force-p))
                                 (owlapi-is-classified (&optional
                                                        owlapi:reasoner))
                                 (owlapi-is-class (owlapi:clsc
                                                   &optional
                                                   owlapi:reasoner))
                                 (owlapi-is-asymmetric (owlapi:property
                                                        &optional
                                                        owlapi:reasoner))
                                 (owlapi-init nil)
                                 (owlapi-ignore-declarations (&optional
                                                              owlapi:reasoner))
                                 (owlapi-ignore-annotations (&optional
                                                             owlapi:reasoner))
                                 (|OWLAPI-IDToAxiom| (owlapi::id
                                                      &optional
                                                      owlapi:reasoner))
                                 (owlapi-id-to-axiom (owlapi::id
                                                      &optional
                                                      owlapi:reasoner))
                                 (owlapi-has-type (owlapi::ind
                                                   type
                                                   owlapi::direct
                                                   &optional
                                                   owlapi:reasoner))
                                 (owlapi-has-object-property-relationship (owlapi:subject owlapi:property
                                                                                          owlapi:object
                                                                                          &optional
                                                                                          owlapi:reasoner))
                                 (owlapi-has-data-property-relationship (owlapi:subject owlapi:property
                                                                                        owlapi:object
                                                                                        &optional
                                                                                        owlapi:reasoner))
                                 (owlapi-get-types (owlapi:individual
                                                    owlapi::direct
                                                    &optional
                                                    owlapi:reasoner))
                                 (owlapi-get-super-properties (owlapi:property
                                                               &optional
                                                               owlapi:reasoner))
                                 (owlapi-get-super-classes (owlapi:cls
                                                            &optional
                                                            owlapi:reasoner))
                                 (owlapi-get-sub-properties (owlapi:property
                                                             &optional
                                                             owlapi:reasoner))
                                 (owlapi-get-sub-classes (owlapi:cls
                                                          &optional
                                                          owlapi:reasoner))
                                 (owlapi-get-same-individuals (owlapi::ind
                                                               &optional
                                                               owlapi:reasoner))
                                 (owlapi-get-related-values (owlapi:subject owlapi:data-property
                                                                            &optional
                                                                            owlapi:reasoner))
                                 (owlapi-get-related-individuals (owlapi:subject owlapi:object-property
                                                                                 &optional
                                                                                 owlapi:reasoner))
                                 (owlapi-get-reasoners nil)
                                 (owlapi-get-ranges (owlapi:property
                                                     &optional
                                                     owlapi:reasoner
                                                     owlapi::owlapi-hacking-mode))
                                 (owlapi-get-prefixes (&optional
                                                       owlapi:reasoner))
                                 (owlapi-get-owl-transitive-object-property-axiom (owlapi:object-property &optional
                                                                                                          owlapi:reasoner))
                                 (owlapi-get-owl-symmetric-object-property-axiom (owlapi:object-property &optional
                                                                                                         owlapi:reasoner))
                                 (owlapi-get-owl-sub-class-axiom (owlapi:sub-class
                                                                  owlapi:super-class
                                                                  &optional
                                                                  owlapi:reasoner))
                                 (owlapi-get-owl-sub-annotation-property-of-axiom (owlapi:annotation-sub-property owlapi:annotation-super-property
                                                                                                                  &optional
                                                                                                                  owlapi:reasoner))
                                 (owlapi-get-owl-sub-annotation-property-axiom nil)
                                 (owlapi-get-owl-same-individuals-axiom (owlapi:individuals &optional
                                                                                            owlapi:reasoner))
                                 (owlapi-get-owl-reflexive-object-property-axiom (owlapi:object-property &optional
                                                                                                         owlapi:reasoner))
                                 (owlapi-get-owl-really-implicit-declaration-axiom (owlapi:entity &optional
                                                                                                  owlapi:reasoner))
                                 (owlapi-get-owl-prefix-declaration-axiom (owlapi::namespace-prefix
                                                                           owlapi:namespace
                                                                           &optional
                                                                           owlapi:reasoner))
                                 (owlapi-get-owl-ontology-version-declaration-axiom (owlapi::ontology-version-uri
                                                                                     &optional
                                                                                     owlapi:reasoner))
                                 (owlapi-get-owl-ontology-annotation-axiom (owlapi:annotation &optional
                                                                                              owlapi:reasoner))
                                 (owlapi-get-owl-object-sub-property-axiom (owlapi:object-sub-property
                                                                            owlapi:object-super-property
                                                                            &optional
                                                                            owlapi:reasoner))
                                 (owlapi-get-owl-object-property-range-axiom (owlapi:object-property owlapi:object-property-range
                                                                                                     &optional
                                                                                                     owlapi:reasoner))
                                 (owlapi-get-owl-object-property-domain-axiom (owlapi:object-property owlapi:object-property-domain
                                                                                                      &optional
                                                                                                      owlapi:reasoner))
                                 (owlapi-get-owl-object-property-chain-sub-property-axiom (owlapi:object-property-chain owlapi:object-super-property
                                                                                                                        &optional
                                                                                                                        owlapi:reasoner))
                                 (owlapi-get-owl-object-property-assertion-axiom (owlapi:subject owlapi:rel-object-property
                                                                                                 owlapi:object
                                                                                                 &optional
                                                                                                 owlapi:reasoner))
                                 (owlapi-get-owl-negative-object-property-assertion-axiom (owlapi:subject owlapi:rel-object-property
                                                                                                          owlapi:object
                                                                                                          &optional
                                                                                                          owlapi:reasoner))
                                 (owlapi-get-owl-negative-data-property-assertion-axiom (owlapi:subject owlapi:rel-data-property
                                                                                                        owlapi::value
                                                                                                        &optional
                                                                                                        owlapi:reasoner))
                                 (owlapi-get-owl-irreflexive-object-property-axiom (owlapi:object-property &optional
                                                                                                           owlapi:reasoner))
                                 (owlapi-get-owl-inverse-object-properties-axiom (owlapi:first-object-property
                                                                                  owlapi:second-object-property
                                                                                  &optional
                                                                                  owlapi:reasoner))
                                 (owlapi-get-owl-inverse-functional-object-property-axiom (owlapi:object-property &optional
                                                                                                                  owlapi:reasoner))
                                 (owlapi-get-owl-imports-declaration-axiom (owlapi::ontology-import-uri
                                                                            &optional
                                                                            owlapi:reasoner))
                                 (owlapi-get-owl-implicit-declaration-axiom (owlapi:entity &optional
                                                                                           owlapi:reasoner))
                                 (owlapi-get-owl-has-key-axiom (owlapi:key-class owlapi:key-object-properties
                                                                                 owlapi:key-data-properties
                                                                                 &optional
                                                                                 owlapi:reasoner))
                                 (owlapi-get-owl-functional-object-property-axiom (owlapi:object-property &optional
                                                                                                          owlapi:reasoner))
                                 (owlapi-get-owl-functional-data-property-axiom (owlapi:data-property &optional
                                                                                                      owlapi:reasoner))
                                 (owlapi-get-owl-equivalent-object-properties-axiom (owlapi:object-properties &optional
                                                                                                              owlapi:reasoner))
                                 (owlapi-get-owl-equivalent-data-properties-axiom (owlapi:data-properties &optional
                                                                                                          owlapi:reasoner))
                                 (owlapi-get-owl-equivalent-classes-axiom (owlapi:descriptions &optional
                                                                                               owlapi:reasoner))
                                 (owlapi-get-owl-entity-annotation-axiom (owlapi:entity owlapi:annotation
                                                                                        &optional
                                                                                        owlapi:reasoner))
                                 (owlapi-get-owl-disjoint-union-axiom (owlapi:description owlapi:descriptions
                                                                                          &optional
                                                                                          owlapi:reasoner))
                                 (owlapi-get-owl-disjoint-object-properties-axiom (owlapi:object-properties &optional
                                                                                                            owlapi:reasoner))
                                 (owlapi-get-owl-disjoint-data-properties-axiom (owlapi:data-properties &optional
                                                                                                        owlapi:reasoner))
                                 (owlapi-get-owl-disjoint-classes-axiom (owlapi:descriptions &optional
                                                                                             owlapi:reasoner))
                                 (owlapi-get-owl-different-individuals-axiom (owlapi:individuals &optional
                                                                                                 owlapi:reasoner))
                                 (owlapi-get-owl-declaration-axiom (owlapi:entity &optional
                                                                                  owlapi:reasoner))
                                 (owlapi-get-owl-datatype-definition-axiom (owlapi:datatype-name owlapi:data-range
                                                                                                 &optional
                                                                                                 owlapi:reasoner))
                                 (owlapi-get-owl-data-sub-property-axiom (owlapi:data-sub-property
                                                                          owlapi:data-super-property
                                                                          &optional
                                                                          owlapi:reasoner))
                                 (owlapi-get-owl-data-property-range-axiom (owlapi:data-property owlapi:data-range
                                                                                                 &optional
                                                                                                 owlapi:reasoner))
                                 (owlapi-get-owl-data-property-domain-axiom (owlapi:data-property owlapi:data-property-domain
                                                                                                  &optional
                                                                                                  owlapi:reasoner))
                                 (owlapi-get-owl-data-property-assertion-axiom (owlapi:subject owlapi:rel-data-property
                                                                                               owlapi::value
                                                                                               &optional
                                                                                               owlapi:reasoner))
                                 (owlapi-get-owl-class-assertion-axiom (owlapi:individual
                                                                        owlapi:description
                                                                        &optional
                                                                        owlapi:reasoner))
                                 (owlapi-get-owl-axiom-annotation-axiom (owlapi:axiom-id owlapi:annotation
                                                                                         &optional
                                                                                         owlapi:reasoner))
                                 (owlapi-get-owl-asymmetric-object-property-axiom (owlapi:object-property &optional
                                                                                                          owlapi:reasoner))
                                 (owlapi-get-owl-annotation-property-range-axiom (owlapi:annotation-property owlapi:annotation-property-range
                                                                                                             &optional
                                                                                                             owlapi:reasoner))
                                 (owlapi-get-owl-annotation-property-domain-axiom (owlapi:annotation-property owlapi:annotation-property-domain
                                                                                                              &optional
                                                                                                              owlapi:reasoner))
                                 (owlapi-get-owl-annotation-assertion-axiom (owlapi:annotation-subject owlapi:annotation-property
                                                                                                       owlapi:annotation-value
                                                                                                       &optional
                                                                                                       owlapi:reasoner))
                                 (owlapi-get-ontologies (&optional
                                                         owlapi:reasoner))
                                 (owlapi-get-object-property-values (owlapi::ind
                                                                     owlapi:property
                                                                     &optional
                                                                     owlapi:reasoner
                                                                     owlapi::synonyms))
                                 (owlapi-get-object-property-relationships (owlapi::ind
                                                                            &optional
                                                                            owlapi:reasoner))
                                 (owlapi-get-loaded-ontologies (&optional
                                                                owlapi:reasoner))
                                 (owlapi-get-inverse-properties (owlapi:property
                                                                 &optional
                                                                 owlapi:reasoner))
                                 (owlapi-get-instances (class
                                                        owlapi::direct
                                                        &optional
                                                        owlapi:reasoner
                                                        owlapi::synonyms))
                                 (owlapi-get-individuals (class
                                                          owlapi::direct
                                                          &optional
                                                          owlapi:reasoner))
                                 (owlapi-get-inconsistent-classes (&optional
                                                                   owlapi:reasoner))
                                 (owlapi-get-equivalent-properties (owlapi:property
                                                                    &optional
                                                                    owlapi:reasoner
                                                                    owlapi::remove-self-p))
                                 (owlapi-get-equivalent-classes (owlapi:cls
                                                                 &optional
                                                                 owlapi:reasoner))
                                 (owlapi-get-domains (owlapi:property
                                                      &optional
                                                      owlapi:reasoner
                                                      owlapi::owlapi-hacking-mode))
                                 (owlapi-get-disjoint-object-properties (owlapi:property
                                                                         &optional
                                                                         owlapi:reasoner))
                                 (owlapi-get-disjoint-data-properties (owlapi:property
                                                                       &optional
                                                                       owlapi:reasoner))
                                 (owlapi-get-disjoint-classes (owlapi::concept
                                                               &optional
                                                               owlapi:reasoner))
                                 (owlapi-get-different-individuals (owlapi::ind
                                                                    &optional
                                                                    owlapi:reasoner
                                                                    owlapi::synonyms))
                                 (owlapi-get-descendant-properties (owlapi:property
                                                                    &optional
                                                                    owlapi:reasoner
                                                                    owlapi::remove-self-p))
                                 (owlapi-get-descendant-classes (owlapi:cls
                                                                 &optional
                                                                 owlapi:reasoner))
                                 (owlapi-get-data-property-values (owlapi::ind
                                                                   owlapi:property
                                                                   &optional
                                                                   owlapi:reasoner))
                                 (owlapi-get-data-property-relationships (owlapi::ind
                                                                          &optional
                                                                          owlapi:reasoner))
                                 (owlapi-get-current-reasoner nil)
                                 (owlapi-get-changes (&optional
                                                      owlapi:reasoner))
                                 (owlapi-get-axioms-per-ontology (&optional
                                                                  owlapi:reasoner))
                                 (owlapi-get-axioms-of-type-in (owlapi::type-or-types
                                                                owlapi::ont
                                                                &optional
                                                                owlapi:reasoner
                                                                owlapi:with-ids-p
                                                                owlapi::status))
                                 (owlapi-get-axioms-of-type (owlapi::type-or-types
                                                             &optional
                                                             owlapi:reasoner
                                                             owlapi:with-ids-p
                                                             owlapi::with-ont-names-p
                                                             owlapi::status))
                                 (owlapi-get-axioms-in (owlapi::ont
                                                        &optional
                                                        owlapi:reasoner
                                                        owlapi:with-ids-p
                                                        owlapi::status))
                                 (owlapi-get-axioms (&optional
                                                     owlapi:reasoner
                                                     owlapi:with-ids-p
                                                     owlapi::with-ont-names-p
                                                     owlapi::status))
                                 (owlapi-get-axiom-counter (&optional
                                                            owlapi:reasoner))
                                 (owlapi-get-auto-ontology (&optional
                                                            owlapi:reasoner))
                                 (owlapi-get-auto-declare-data-properties (&optional
                                                                           owlapi:reasoner))
                                 (owlapi-get-annotation-axioms-for-axiom (owlapi:axiom-id &optional
                                                                                          owlapi:reasoner))
                                 (owlapi-get-ancestor-properties (owlapi:property
                                                                  &optional
                                                                  owlapi:reasoner
                                                                  owlapi::remove-self-p))
                                 (owlapi-get-ancestor-classes (owlapi:cls
                                                               &optional
                                                               owlapi:reasoner))
                                 (owlapi-get-all-ontologies nil)
                                 (owlapi-find-object-from-id (owlapi::id))
                                 (owlapi-find-id-from-object (owlapi:obj))
                                 (owlapi-export-reasoner (owlapi:reasoner
                                                          owlapi::fn
                                                          &key
                                                          owlapi::syntax
                                                          owlapi::quoted
                                                          owlapi::init))
                                 (owlapi-export-ontology (owlapi:ontology
                                                          owlapi::fn
                                                          &key
                                                          owlapi:reasoner
                                                          owlapi::syntax
                                                          owlapi::quoted
                                                          owlapi::init
                                                          owlapi::header))
                                 (owlapi-enable-transient-axiom-mode (&optional
                                                                      owlapi:reasoner))
                                 (owlapi-enable-simplified-protocol (&optional
                                                                     owlapi:reasoner))
                                 (owlapi-enable-memory-saving-mode (owlapi:ontology
                                                                    &optional
                                                                    owlapi:reasoner
                                                                    owlapi::use-less-tbox-memory-p))
                                 (owlapi-enable-lookup-mode (&optional
                                                             owlapi:reasoner))
                                 (owlapi-enable-incremental-updates (&optional
                                                                     owlapi:reasoner))
                                 (owlapi-dont-register-referenced-entities (&optional
                                                                            owlapi:reasoner))
                                 (owlapi-dont-register-declared-entities (&optional
                                                                          owlapi:reasoner))
                                 (owlapi-dispose-reasoner (owlapi:name))
                                 (owlapi-dispose-ontology (owlapi::ont-name
                                                           &optional
                                                           owlapi:reasoner
                                                           owlapi:dispose-axioms-p))
                                 (owlapi-dispose-ontologies (owlapi:ontologies &optional
                                                                               owlapi:reasoner))
                                 (owlapi-dispose-axioms (owlapi::ids-or-constructors
                                                         &optional
                                                         owlapi:reasoner))
                                 (owlapi-dispose-axiom (owlapi::id-or-constructor
                                                        &optional
                                                        owlapi:reasoner))
                                 (owlapi-dispose nil)
                                 (owlapi-disable-transient-axiom-mode (&optional
                                                                       owlapi:reasoner))
                                 (owlapi-disable-simplified-protocol (&optional
                                                                      owlapi:reasoner))
                                 (owlapi-disable-memory-saving-mode (owlapi:reasoner))
                                 (owlapi-disable-lookup-mode (&optional
                                                              owlapi:reasoner))
                                 (owlapi-disable-incremental-updates (&optional
                                                                      owlapi:reasoner))
                                 (owlapi-disable-auto-mode (&optional
                                                            owlapi:reasoner))
                                 (owlapi-describe-reasoners nil)
                                 (owlapi-describe-reasoner (&optional
                                                            owlapi:reasoner))
                                 (owlapi-describe-ontology (owlapi:ontology
                                                            &optional
                                                            owlapi:reasoner))
                                 (owlapi-describe-ontologies (&optional
                                                              owlapi:reasoner))
                                 (owlapi-contains (owlapi::ont-name
                                                   &optional
                                                   owlapi:reasoner))
                                 (owlapi-consider-declarations (&optional
                                                                owlapi:reasoner))
                                 (owlapi-clear-registry (&optional
                                                         owlapi:reasoner))
                                 (owlapi-clear-ontologies (&optional
                                                           owlapi:reasoner))
                                 (owlapi-clear-changes (&optional
                                                        owlapi:reasoner))
                                 (owlapi-classify (&optional
                                                   owlapi:reasoner
                                                   owlapi::check-abox-consistency-p))
                                 (owlapi-batch-synchronize (owlapi:ontology
                                                            &optional
                                                            owlapi:reasoner))
                                 (|OWLAPI-AxiomToID| (owlapi::axiom-constructor-call
                                                      &optional
                                                      owlapi:reasoner
                                                      owlapi::ont))
                                 (|OWLAPI-AxiomLoaded?| (owlapi::id
                                                         &optional
                                                         owlapi:reasoner))
                                 (|OWLAPI-AddAxioms| (owlapi::ont
                                                      owlapi:axioms
                                                      &optional
                                                      owlapi:reasoner))
                                 (|OWLAPI-AddAxiom| (owlapi::ont
                                                     owlapi:axiom
                                                     &optional
                                                     owlapi:reasoner))
                                 (owlapi-axiom-to-id (owlapi::axiom-constructor-call
                                                      &optional
                                                      owlapi:reasoner
                                                      owlapi::ont))
                                 (owlapi-axiom-loaded? (owlapi::id
                                                        &optional
                                                        owlapi:reasoner))
                                 (owlapi-auto-remove-axioms-from (owlapi:ontology
                                                                  &optional
                                                                  owlapi:reasoner))
                                 (owlapi-auto-batch-remove-axioms-from (owlapi:ontology
                                                                        &optional
                                                                        owlapi:reasoner))
                                 (owlapi-auto-batch-add-axioms-to (owlapi:ontology
                                                                   &optional
                                                                   owlapi:reasoner))
                                 (owlapi-auto-apply-changes (&optional
                                                             owlapi:reasoner))
                                 (owlapi-auto-add-axioms-to (owlapi:ontology
                                                             &optional
                                                             owlapi:reasoner))
                                 (owlapi-apply-changes (&optional
                                                        owlapi:reasoner))
                                 (owlapi-add-prefix (owlapi:prefix owlapi:namespace
                                                                   &optional
                                                                   owlapi:reasoner))
                                 (owlapi-add-axioms (owlapi::ont
                                                     owlapi:axioms
                                                     &optional
                                                     owlapi:reasoner))
                                 (owlapi-add-axiom (owlapi::ont
                                                    owlapi:axiom
                                                    &optional
                                                    owlapi:reasoner))
                                 (owlapi-abort (&optional
                                                owlapi:reasoner))
                                 (owl-read-file (filename
                                                 &key
                                                 verbose
                                                 init
                                                 kb-name
                                                 locator
                                                 recursive
                                                 ignore-import
                                                 import-meta-ontologies
                                                 excluded-meta-ontologies
                                                 fire-rules
                                                 maintain-owlapi-axioms
                                                 ignore-annotations
                                                 ontology-name
                                                 merge-imported-ontologies-p
                                                 &allow-other-keys))
                                 (owl-read-document (url-spec
                                                     &rest
                                                     args
                                                     &key
                                                     verbose
                                                     init
                                                     kb-name
                                                     locator
                                                     recursive
                                                     ignore-import
                                                     ontology-name
                                                     merge-imported-ontologies-p
                                                     import-meta-ontologies
                                                     excluded-meta-ontologies
                                                     fire-rules
                                                     maintain-owlapi-axioms
                                                     ignore-annotations))
                                 (original-rule-consequence (thematic-substrate::query))
                                 (original-rule-antecedence (thematic-substrate::query))
                                 (original-query-head (thematic-substrate::query))
                                 (original-query-body (thematic-substrate::query))
                                 (optimizer-use-cardinality-heuristics nil)
                                 (optimizer-set-time-bound (thematic-substrate::n))
                                 (optimizer-set-no-of-plans-upper-bound (thematic-substrate::n))
                                 (optimizer-get-time-bound nil)
                                 (optimizer-get-no-of-plans-upper-bound nil)
                                 (optimizer-ensure-late-lambda-evaluation nil)
                                 (optimizer-dont-use-cardinality-heuristics nil)
                                 (optimizer-dont-ensure-late-lambda-evaluation nil)
                                 (open-triple-store (name
                                                     &key
                                                     directory
                                                     rdfs-reasoning))
                                 (node-label1 (thematic-substrate::name &optional
                                                                        thematic-substrate::abox
                                                                        thematic-substrate::type-of-substrate))
                                 (node-label (thematic-substrate::name &optional
                                                                       thematic-substrate::abox
                                                                       thematic-substrate::type-of-substrate))
                                 (node-description1 (thematic-substrate::name &optional
                                                                              thematic-substrate::abox
                                                                              thematic-substrate::type-of-substrate))
                                 (node-description (thematic-substrate::name &optional
                                                                             thematic-substrate::abox
                                                                             thematic-substrate::type-of-substrate))
                                 (next-tuple-available-p (thematic-substrate::query))
                                 (next-set-of-rule-consequences-available-p (thematic-substrate::query))
                                 (msc-k (individual
                                         k
                                         &rest
                                         args
                                         &key
                                         include-direct-types
                                         abox
                                         name
                                         &allow-other-keys))
                                 (move-rules (thematic-substrate::from-abox
                                              thematic-substrate::to-abox
                                              &key
                                              thematic-substrate::type-of-substrate))
                                 (most-specific-instantiators (individual-name
                                                               abox))
                                 (mirror (url-spec1
                                          url-or-filename))
                                 (materialize-inferences (kb-name
                                                          &key
                                                          db
                                                          directory
                                                          index-p
                                                          abox-told-only-p
                                                          abox
                                                          tbox
                                                          if-exists
                                                          if-does-not-exist
                                                          all-different-p
                                                          same-individual-as
                                                          role-fillers
                                                          told-datatype-fillers
                                                          subgraph-markers
                                                          rename-individuals
                                                          in-case-individuals-are-renamed-keep-originals
                                                          data-version-level))
                                 (make-query-from-abox (thematic-substrate::abox-or-name
                                                        &key
                                                        thematic-substrate::known-correspondences
                                                        thematic-substrate::common-concept-assertions
                                                        thematic-substrate::common-role-assertions
                                                        thematic-substrate::common-same-as-assertions
                                                        thematic-substrate::common-different-from-assertions
                                                        thematic-substrate::common-as-strict-atoms-p
                                                        thematic-substrate::injective-variables-p
                                                        thematic-substrate::forward-rule-consequence-p))
                                 (make-plugin-from-fasl-file (thematic-substrate::fn2
                                                              &key
                                                              thematic-substrate::plugin-name
                                                              thematic-substrate::rand
                                                              type
                                                              thematic-substrate::extension
                                                              thematic-substrate::text-description
                                                              thematic-substrate::short-description
                                                              thematic-substrate::id
                                                              thematic-substrate::patch-name
                                                              thematic-substrate::for-version
                                                              thematic-substrate::for-build
                                                              thematic-substrate::platform))
                                 (make-forward-rule-from-aboxes (thematic-substrate::precond-abox
                                                                 thematic-substrate::postcond-abox
                                                                 thematic-substrate::for-abox
                                                                 &key
                                                                 thematic-substrate::execute-p
                                                                 thematic-substrate::parser-class
                                                                 thematic-substrate::rewrite-defined-concepts-p
                                                                 thematic-substrate::group-by-ops
                                                                 thematic-substrate::bind-specials-p
                                                                 thematic-substrate::original-query
                                                                 thematic-substrate::rule-con-pattern
                                                                 thematic-substrate::new-ind-ops
                                                                 thematic-substrate::premise
                                                                 thematic-substrate::generate-code-p
                                                                 thematic-substrate::optimize-p
                                                                 thematic-substrate::rewrite-semantically-p
                                                                 thematic-substrate::rewrite-to-dnf-p
                                                                 thematic-substrate::report-inconsistent-queries-p
                                                                 thematic-substrate::report-tautological-queries-p
                                                                 thematic-substrate::use-repository-p
                                                                 thematic-substrate::put-into-repository-p
                                                                 thematic-substrate::id
                                                                 thematic-substrate::dont-check-id-p
                                                                 thematic-substrate::parser
                                                                 thematic-substrate::result-vois
                                                                 thematic-substrate::substrate
                                                                 thematic-substrate::abox
                                                                 thematic-substrate::create-abox-if-not-found-p
                                                                 package
                                                                 thematic-substrate::type-of-substrate
                                                                 thematic-substrate::prepare-now-p
                                                                 thematic-substrate::known-correspondences
                                                                 thematic-substrate::common-concept-assertions
                                                                 thematic-substrate::common-role-assertions
                                                                 thematic-substrate::common-same-as-assertions
                                                                 thematic-substrate::common-different-from-assertions
                                                                 thematic-substrate::common-as-strict-atoms-p
                                                                 thematic-substrate::injective-variables-p
                                                                 thematic-substrate::forward-rule-consequence-p))
                                 (make-backward-rule-from-aboxes (thematic-substrate::precond-abox
                                                                  thematic-substrate::postcond-abox
                                                                  thematic-substrate::for-abox
                                                                  &key
                                                                  thematic-substrate::known-correspondences
                                                                  thematic-substrate::common-concept-assertions
                                                                  thematic-substrate::common-role-assertions
                                                                  thematic-substrate::common-same-as-assertions
                                                                  thematic-substrate::common-different-from-assertions
                                                                  thematic-substrate::common-as-strict-atoms-p
                                                                  thematic-substrate::injective-variables-p
                                                                  thematic-substrate::forward-rule-consequence-p))
                                 (make-abduction-rule-from-aboxes (thematic-substrate::precond-abox
                                                                   thematic-substrate::postcond-abox
                                                                   thematic-substrate::for-abox
                                                                   &key
                                                                   thematic-substrate::forward-rule-p
                                                                   thematic-substrate::backward-rule-p
                                                                   thematic-substrate::known-correspondences
                                                                   thematic-substrate::common-concept-assertions
                                                                   thematic-substrate::common-role-assertions
                                                                   thematic-substrate::common-same-as-assertions
                                                                   thematic-substrate::common-different-from-assertions
                                                                   thematic-substrate::common-as-strict-atoms-p
                                                                   thematic-substrate::injective-variables-p
                                                                   thematic-substrate::forward-rule-consequence-p))
                                 (logging-on (&optional
                                              filename
                                              debug))
                                 (logging-off nil)
                                 (load-racer-plugins (directory))
                                 (load-racer-plugin (thematic-substrate::fn))
                                 (load-racer-patches (directory))
                                 (load-racer-patch (thematic-substrate::fn))
                                 (lcs-unfold (concept-1
                                              concept-2
                                              &optional
                                              tbox))
                                 (lcs (concept1
                                       concept2))
                                 (keep-defined-query-atoms nil)
                                 (kb-ontologies (kb-name))
                                 (irreflexive? (role-term
                                                &optional
                                                tbox-name))
                                 (irreflexive-p (role-term
                                                 &optional
                                                 tbox))
                                 (irreflexive (rolename
                                               &optional
                                               tbox))
                                 (inverse-of-role (rolename
                                                   inverse-role
                                                   tbox))
                                 (inverse-feature-p (role-term
                                                     &optional
                                                     tbox))
                                 (inverse (rolename
                                           inverse-role
                                           &optional
                                           tbox))
                                 (internal-individuals-related-p (ind-predecessor-name-set
                                                                  ind-filler-name-set
                                                                  role-term
                                                                  abox
                                                                  &optional
                                                                  check-p))
                                 (instantiators (individual-name
                                                 abox))
                                 (instance (name
                                            concept))
                                 (installed-plugins nil)
                                 (installed-patches nil)
                                 (init-tbox (tbox &key
                                                         original
                                                         reset))
                                 (init-subscriptions-1 (&optional
                                                        abox))
                                 (init-subscriptions (&optional
                                                      abox))
                                 (init-publications-1 (&optional
                                                       abox))
                                 (init-publications (&optional
                                                     abox))
                                 (init-abox (abox
                                             &optional
                                             tbox))
                                 (individuals-related? (individual-1
                                                        individual-2
                                                        role-term
                                                        &optional
                                                        abox))
                                 (individuals-related-p (ind-predecessor-name-set
                                                         ind-filler-name-set
                                                         role-term
                                                         abox))
                                 (individuals-not-equal? (individual-1
                                                          individual-2
                                                          &optional
                                                          abox))
                                 (individuals-not-equal-p (individual-1
                                                           individual-2
                                                           &optional
                                                           abox))
                                 (individuals-equal? (individual-1
                                                      individual-2
                                                      &optional
                                                      abox))
                                 (individuals-equal-p (individual-1
                                                       individual-2
                                                       &optional
                                                       abox))
                                 (individual? (individual-name
                                               &optional
                                               abox-name))
                                 (individual-types (individual-name
                                                    &optional
                                                    abox))
                                 (individual-told-datatype-fillers (ind
                                                                    datatype-role
                                                                    &optional
                                                                    abox))
                                 (individual-told-attribute-value (ind
                                                                   attribute
                                                                   &optional
                                                                   abox))
                                 (individual-synonyms (individual
                                                       &optional
                                                       told-only
                                                       abox-name))
                                 (individual-p (individual-name
                                                &optional
                                                abox))
                                 (individual-instance? (individual
                                                        concept
                                                        &optional
                                                        abox))
                                 (individual-instance-p (individual-name
                                                         concept
                                                         abox))
                                 (individual-fillers (ind-predecessor
                                                      role-term
                                                      &optional
                                                      abox))
                                 (individual-filled-roles (ind-predecessor
                                                           ind-filler
                                                           &optional
                                                           abox))
                                 (individual-direct-types (individual-name
                                                           &optional
                                                           abox))
                                 (individual-attribute-fillers (ind
                                                                attribute
                                                                &optional
                                                                abox))
                                 (individual-antonyms (individual
                                                       &optional
                                                       told-only
                                                       abox-name))
                                 (index-all-triples (&key db))
                                 (include-permutations nil)
                                 (include-kb (pathname))
                                 (inaccurate-rules (&key
                                                    thematic-substrate::abox
                                                    thematic-substrate::type-of-substrate))
                                 (inaccurate-queries (&key
                                                      thematic-substrate::abox
                                                      thematic-substrate::type-of-substrate))
                                 (in-unsafe-mode? nil)
                                 (in-tbox (name
                                           &key
                                           init
                                           size
                                           role-size
                                           signature))
                                 (in-rcc-box (thematic-substrate::name &optional
                                                                       thematic-substrate::rcc-type
                                                                       type))
                                 (in-mirror-data-box (thematic-substrate::name))
                                 (in-knowledge-base (tbox-name &rest
                                                                      args))
                                 (in-data-box (thematic-substrate::name))
                                 (in-abox (abox-name &optional
                                                            tbox-name))
                                 (implies-role (rolename-1
                                                rolename-2
                                                &optional
                                                tbox))
                                 (implies (left right))
                                 (get-tbox-version (tbox))
                                 (get-tbox-signature (&optional
                                                      tbox))
                                 (get-tbox-language (&optional
                                                     tbox))
                                 (get-substrate-type nil)
                                 (get-substrate-nodes (&key
                                                       thematic-substrate::abox
                                                       thematic-substrate::type-of-substrate))
                                 (get-substrate-edges (&key
                                                       thematic-substrate::abox
                                                       thematic-substrate::type-of-substrate))
                                 (get-server-timeout nil)
                                 (get-role-hierarchy (&optional
                                                      thematic-substrate::tbox
                                                      &key
                                                      thematic-substrate::for-roles))
                                 (get-role-datatype (role-name &optional
                                                                      tbox))
                                 (get-proxy-server nil)
                                 (get-process-pool-size nil)
                                 (get-prefixes (&optional
                                                tbox
                                                ask-owlapi-p))
                                 (get-object-bottom-role (tbox))
                                 (get-number-of-explanations (thematic-substrate::query &key
                                                                                        thematic-substrate::dont-show-variables
                                                                                        thematic-substrate::execute-p))
                                 (get-nrql-version nil)
                                 (get-nodes-in-qbox-for-abox (&optional
                                                              thematic-substrate::abox))
                                 (get-next-tuple (thematic-substrate::query &key
                                                                            thematic-substrate::execute-p))
                                 (get-next-set-of-rule-consequences (thematic-substrate::query &key))
                                 (get-next-n-remaining-tuples (thematic-substrate::query &optional
                                                                                         thematic-substrate::n
                                                                                         &key
                                                                                         thematic-substrate::execute-p))
                                 (get-next-n-remaining-sets-of-rule-consequences (thematic-substrate::query &optional
                                                                                                            thematic-substrate::n
                                                                                                            &key))
                                 (get-new-ind-prefix nil)
                                 (get-new-ind-counter nil)
                                 (get-namespace-prefix (tbox))
                                 (get-minimum (thematic-substrate::aboxes
                                               &key
                                               thematic-substrate::key))
                                 (get-meta-constraint (&optional
                                                       tbox))
                                 (get-maximum-size-of-process-pool nil)
                                 (get-maximum (thematic-substrate::aboxes
                                               &key
                                               thematic-substrate::key))
                                 (get-max-no-of-tuples-bound nil)
                                 (get-kb-signature (kb-name))
                                 (get-initial-size-of-process-pool nil)
                                 (get-individual-successors (thematic-substrate::ind
                                                             &key
                                                             thematic-substrate::no-inverses-p
                                                             thematic-substrate::only-inverses-p
                                                             thematic-substrate::no-transitives-p
                                                             thematic-substrate::no-top-role-p
                                                             thematic-substrate::negated-p
                                                             thematic-substrate::roles
                                                             thematic-substrate::only-one-p
                                                             thematic-substrate::only-if-p
                                                             thematic-substrate::abox
                                                             thematic-substrate::remove-synonyms-p
                                                             thematic-substrate::show-synonyms-p))
                                 (get-individual-pmodel (individual-name
                                                         abox))
                                 (get-individual-datatype-fillers (thematic-substrate::individual-name
                                                                   &optional
                                                                   thematic-substrate::abox
                                                                   thematic-substrate::with-types-p))
                                 (get-individual-annotation-fillers (thematic-substrate::individual-name
                                                                     &optional
                                                                     thematic-substrate::abox))
                                 (get-individual-annotation-datatype-fillers (thematic-substrate::individual-name
                                                                              &optional
                                                                              thematic-substrate::abox
                                                                              thematic-substrate::with-types-p))
                                 (get-explanations (thematic-substrate::query &key
                                                                              thematic-substrate::from
                                                                              thematic-substrate::to
                                                                              thematic-substrate::only-best-p
                                                                              thematic-substrate::order-by
                                                                              thematic-substrate::reverse-order-p
                                                                              thematic-substrate::equi-order-by
                                                                              thematic-substrate::remove-marker-symbols-p
                                                                              thematic-substrate::remove-entailed-explanations-p
                                                                              thematic-substrate::new-inds-p
                                                                              thematic-substrate::tuples-p
                                                                              thematic-substrate::full-tuples-p
                                                                              thematic-substrate::all-assertions-p
                                                                              thematic-substrate::hypothesized-assertions-p
                                                                              thematic-substrate::show-score-p
                                                                              thematic-substrate::abox-entailment
                                                                              thematic-substrate::ensure-permutations-p))
                                 (get-edge-label-for-non-existent-edges (&key
                                                                         thematic-substrate::abox
                                                                         thematic-substrate::type-of-substrate))
                                 (get-data-node-label (thematic-substrate::name &key
                                                                                thematic-substrate::abox
                                                                                thematic-substrate::type-of-substrate))
                                 (get-data-node-description (thematic-substrate::name &key
                                                                                      thematic-substrate::abox
                                                                                      thematic-substrate::type-of-substrate))
                                 (get-data-edge-label (thematic-substrate::from
                                                       thematic-substrate::to
                                                       &key
                                                       thematic-substrate::abox
                                                       thematic-substrate::type-of-substrate))
                                 (get-data-edge-description (thematic-substrate::from
                                                             thematic-substrate::to
                                                             &key
                                                             thematic-substrate::abox
                                                             thematic-substrate::type-of-substrate))
                                 (get-data-bottom-role (tbox))
                                 (get-dag-of-qbox-for-abox (&optional
                                                            thematic-substrate::abox))
                                 (get-current-tuple (thematic-substrate::query))
                                 (get-current-set-of-rule-consequences (thematic-substrate::query))
                                 (get-concept-properties (thematic-substrate::concept
                                                          &optional
                                                          thematic-substrate::tbox
                                                          &key
                                                          thematic-substrate::for-roles
                                                          thematic-substrate::qualifications))
                                 (get-concept-pmodel (concept-expr
                                                      tbox))
                                 (get-concept-negated-definition-1 (concept-name
                                                                    tbox))
                                 (get-concept-negated-definition (concept-name
                                                                  &optional
                                                                  tbox))
                                 (get-concept-definition-1 (concept-name
                                                            tbox))
                                 (get-concept-definition (concept-name
                                                          &optional
                                                          tbox))
                                 (get-chosen-sets-of-rule-consequences (thematic-substrate::query))
                                 (get-build-version nil)
                                 (get-answer-size (thematic-substrate::query &optional
                                                                             &key
                                                                             thematic-substrate::dont-show-variables
                                                                             thematic-substrate::execute-p))
                                 (get-answer (thematic-substrate::query &key
                                                                        thematic-substrate::dont-show-variables
                                                                        thematic-substrate::execute-p))
                                 (get-all-values nil)
                                 (get-all-server-values nil)
                                 (get-all-server-functions nil)
                                 (get-all-remaining-tuples (thematic-substrate::query &key
                                                                                      thematic-substrate::execute-p))
                                 (get-all-remaining-sets-of-rule-consequences (thematic-substrate::query &key))
                                 (get-all-functions nil)
                                 (get-all-answers (&key
                                                   thematic-substrate::ready-p
                                                   thematic-substrate::active-p
                                                   thematic-substrate::processed-p
                                                   thematic-substrate::queries-p
                                                   thematic-substrate::rules-p
                                                   thematic-substrate::abox
                                                   thematic-substrate::type-of-substrate
                                                   thematic-substrate::dont-show-variables
                                                   thematic-substrate::execute-p))
                                 (get-agraph-version nil)
                                 (get-abox-version (abox))
                                 (get-abox-signature (&optional
                                                      abox))
                                 (get-abox-language (&optional
                                                     abox))
                                 (get-abox-graph (&optional
                                                  thematic-substrate::abox
                                                  &key
                                                  thematic-substrate::depth
                                                  thematic-substrate::no-transitives-p
                                                  thematic-substrate::no-top-role-p
                                                  thematic-substrate::browsing-mode-p
                                                  thematic-substrate::told-only-p
                                                  thematic-substrate::root-individuals
                                                  thematic-substrate::selected-individuals
                                                  thematic-substrate::only-successors-in-selected-individuals-p
                                                  thematic-substrate::for-roles
                                                  thematic-substrate::for-datatype-properties
                                                  thematic-substrate::for-annotation-properties))
                                 (functional (rolename
                                              &optional
                                              tbox))
                                 (full-reset nil)
                                 (forget-tbox (tbox))
                                 (forget-statement (tbox abox
                                                                assertions))
                                 (forget-same-individual-as-assertion (abox
                                                                       individual-1
                                                                       individual-2))
                                 (forget-role-axioms (tbox role
                                                                  &key
                                                                  cd-attribute
                                                                  parents
                                                                  parent
                                                                  transitive
                                                                  transitive-p
                                                                  feature
                                                                  feature-p
                                                                  domain
                                                                  range
                                                                  inverse
                                                                  symmetric
                                                                  reflexive
                                                                  reflexive-p
                                                                  datatype
                                                                  annotation-p))
                                 (forget-role-assertion (abox
                                                         predecessor-name
                                                         filler-name
                                                         role-term))
                                 (forget-negative-datatype-role-filler (abox
                                                                        individual
                                                                        value
                                                                        role))
                                 (forget-negated-role-assertion (abox
                                                                 predecessor-name
                                                                 filler-name
                                                                 role-term))
                                 (forget-individual (individual
                                                     &optional
                                                     abox))
                                 (forget-disjointness-axiom-statement (tbox concepts))
                                 (forget-disjointness-axiom (tbox concept-name
                                                                         group-name
                                                                         &optional
                                                                         form))
                                 (forget-different-from-assertion (abox
                                                                   individual-1
                                                                   individual-2))
                                 (forget-datatype-role-filler (abox
                                                               individual
                                                               value
                                                               role))
                                 (forget-constraint (abox
                                                     constraint))
                                 (forget-constrained-assertion (abox
                                                                individual-name
                                                                object-name
                                                                attribute-term))
                                 (forget-concept-axiom (tbox left
                                                                    right
                                                                    &key
                                                                    inclusion-p))
                                 (forget-concept-assertion (abox
                                                            individual-name
                                                            concept))
                                 (forget-annotation-concept-assertion (abox
                                                                       individual-name
                                                                       concept))
                                 (forget-all-different-assertion (abox
                                                                  individual-name-set))
                                 (forget-abox (abox))
                                 (forget (&key
                                          &body
                                          assertions))
                                 (firerule1 (thematic-substrate::res-args
                                             thematic-substrate::query
                                             &key
                                             thematic-substrate::execute-p
                                             thematic-substrate::parser-class
                                             thematic-substrate::rewrite-defined-concepts-p
                                             thematic-substrate::group-by-ops
                                             thematic-substrate::bind-specials-p
                                             thematic-substrate::original-query
                                             thematic-substrate::rule-con-pattern
                                             thematic-substrate::new-ind-ops
                                             thematic-substrate::premise
                                             thematic-substrate::generate-code-p
                                             thematic-substrate::optimize-p
                                             thematic-substrate::rewrite-semantically-p
                                             thematic-substrate::rewrite-to-dnf-p
                                             thematic-substrate::report-inconsistent-queries-p
                                             thematic-substrate::report-tautological-queries-p
                                             thematic-substrate::use-repository-p
                                             thematic-substrate::put-into-repository-p
                                             thematic-substrate::id
                                             thematic-substrate::dont-check-id-p
                                             thematic-substrate::parser
                                             thematic-substrate::result-vois
                                             thematic-substrate::substrate
                                             thematic-substrate::abox
                                             thematic-substrate::create-abox-if-not-found-p
                                             package
                                             thematic-substrate::type-of-substrate
                                             thematic-substrate::prepare-now-p))
                                 (firerule-under-premise1 (thematic-substrate::res-args
                                                           thematic-substrate::query
                                                           &key
                                                           thematic-substrate::execute-p
                                                           thematic-substrate::parser-class
                                                           thematic-substrate::rewrite-defined-concepts-p
                                                           thematic-substrate::group-by-ops
                                                           thematic-substrate::bind-specials-p
                                                           thematic-substrate::original-query
                                                           thematic-substrate::rule-con-pattern
                                                           thematic-substrate::new-ind-ops
                                                           thematic-substrate::premise
                                                           thematic-substrate::generate-code-p
                                                           thematic-substrate::optimize-p
                                                           thematic-substrate::rewrite-semantically-p
                                                           thematic-substrate::rewrite-to-dnf-p
                                                           thematic-substrate::report-inconsistent-queries-p
                                                           thematic-substrate::report-tautological-queries-p
                                                           thematic-substrate::use-repository-p
                                                           thematic-substrate::put-into-repository-p
                                                           thematic-substrate::id
                                                           thematic-substrate::dont-check-id-p
                                                           thematic-substrate::parser
                                                           thematic-substrate::result-vois
                                                           thematic-substrate::substrate
                                                           thematic-substrate::abox
                                                           thematic-substrate::create-abox-if-not-found-p
                                                           package
                                                           thematic-substrate::type-of-substrate
                                                           thematic-substrate::prepare-now-p))
                                 (firerule-under-premise (thematic-substrate::query thematic-substrate::res-args
                                                                                    &key
                                                                                    thematic-substrate::execute-p
                                                                                    thematic-substrate::parser-class
                                                                                    thematic-substrate::rewrite-defined-concepts-p
                                                                                    thematic-substrate::group-by-ops
                                                                                    thematic-substrate::bind-specials-p
                                                                                    thematic-substrate::original-query
                                                                                    thematic-substrate::rule-con-pattern
                                                                                    thematic-substrate::new-ind-ops
                                                                                    thematic-substrate::premise
                                                                                    thematic-substrate::generate-code-p
                                                                                    thematic-substrate::optimize-p
                                                                                    thematic-substrate::rewrite-semantically-p
                                                                                    thematic-substrate::rewrite-to-dnf-p
                                                                                    thematic-substrate::report-inconsistent-queries-p
                                                                                    thematic-substrate::report-tautological-queries-p
                                                                                    thematic-substrate::use-repository-p
                                                                                    thematic-substrate::put-into-repository-p
                                                                                    thematic-substrate::id
                                                                                    thematic-substrate::dont-check-id-p
                                                                                    thematic-substrate::parser
                                                                                    thematic-substrate::result-vois
                                                                                    thematic-substrate::substrate
                                                                                    thematic-substrate::abox
                                                                                    thematic-substrate::create-abox-if-not-found-p
                                                                                    package
                                                                                    thematic-substrate::type-of-substrate
                                                                                    thematic-substrate::prepare-now-p))
                                 (firerule (thematic-substrate::query thematic-substrate::res-args
                                                                      &key
                                                                      thematic-substrate::execute-p
                                                                      thematic-substrate::parser-class
                                                                      thematic-substrate::rewrite-defined-concepts-p
                                                                      thematic-substrate::group-by-ops
                                                                      thematic-substrate::bind-specials-p
                                                                      thematic-substrate::original-query
                                                                      thematic-substrate::rule-con-pattern
                                                                      thematic-substrate::new-ind-ops
                                                                      thematic-substrate::premise
                                                                      thematic-substrate::generate-code-p
                                                                      thematic-substrate::optimize-p
                                                                      thematic-substrate::rewrite-semantically-p
                                                                      thematic-substrate::rewrite-to-dnf-p
                                                                      thematic-substrate::report-inconsistent-queries-p
                                                                      thematic-substrate::report-tautological-queries-p
                                                                      thematic-substrate::use-repository-p
                                                                      thematic-substrate::put-into-repository-p
                                                                      thematic-substrate::id
                                                                      thematic-substrate::dont-check-id-p
                                                                      thematic-substrate::parser
                                                                      thematic-substrate::result-vois
                                                                      thematic-substrate::substrate
                                                                      thematic-substrate::abox
                                                                      thematic-substrate::create-abox-if-not-found-p
                                                                      package
                                                                      thematic-substrate::type-of-substrate
                                                                      thematic-substrate::prepare-now-p))
                                 (find-tbox (tbox &optional
                                                         errorp))
                                 (find-abox (abox-name-or-abox
                                             &optional
                                             errorp))
                                 (feature? (role-term
                                            &optional
                                            tbox-name))
                                 (feature-p (role-term
                                             &optional
                                             tbox))
                                 (fcall (thematic-substrate::name))
                                 (expensive-rules (&key
                                                   thematic-substrate::abox
                                                   thematic-substrate::type-of-substrate))
                                 (expensive-rule-p (thematic-substrate::query))
                                 (expensive-query-p (thematic-substrate::query))
                                 (expensive-queries (&key
                                                     thematic-substrate::abox
                                                     thematic-substrate::type-of-substrate))
                                 (exit-server nil)
                                 (execute-rule (thematic-substrate::query &key))
                                 (execute-query (thematic-substrate::query &key
                                                                           thematic-substrate::dont-add-abox-duplicates-p
                                                                           thematic-substrate::remove-duplicates-p
                                                                           thematic-substrate::two-phase-processing-p
                                                                           thematic-substrate::deliver-phase-two-warning-tokens-p
                                                                           thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                                           thematic-substrate::add-rule-consequences-p
                                                                           thematic-substrate::continuation-based-instance-retrieval-p
                                                                           thematic-substrate::told-information-reasoning-p
                                                                           thematic-substrate::final-consistency-checking-p
                                                                           thematic-substrate::runtime-consistency-checking-p
                                                                           thematic-substrate::verbose-p
                                                                           thematic-substrate::dont-show-variables
                                                                           thematic-substrate::dont-show-head-projection-operators-p
                                                                           thematic-substrate::dont-show-lambdas-p
                                                                           thematic-substrate::how-many
                                                                           thematic-substrate::only-new-tuples-p
                                                                           thematic-substrate::timeout
                                                                           thematic-substrate::proactive-tuple-computation-p
                                                                           thematic-substrate::tuple-at-a-time-p
                                                                           thematic-substrate::use-individual-synonyms-p
                                                                           thematic-substrate::check-abox-consistency-p
                                                                           thematic-substrate::ensure-tbox-classification-p
                                                                           thematic-substrate::initial-abox-mirroring-p
                                                                           thematic-substrate::initial-role-assertion-mirroring-p
                                                                           thematic-substrate::classify-concepts-in-instance-assertions-p
                                                                           thematic-substrate::exclude-permutations-p
                                                                           thematic-substrate::record-explanations-p))
                                 (execute-or-reexecute-rule (thematic-substrate::query))
                                 (execute-or-reexecute-query (thematic-substrate::query &key
                                                                                        thematic-substrate::dont-add-abox-duplicates-p
                                                                                        thematic-substrate::remove-duplicates-p
                                                                                        thematic-substrate::two-phase-processing-p
                                                                                        thematic-substrate::deliver-phase-two-warning-tokens-p
                                                                                        thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                                                        thematic-substrate::add-rule-consequences-p
                                                                                        thematic-substrate::continuation-based-instance-retrieval-p
                                                                                        thematic-substrate::told-information-reasoning-p
                                                                                        thematic-substrate::final-consistency-checking-p
                                                                                        thematic-substrate::runtime-consistency-checking-p
                                                                                        thematic-substrate::verbose-p
                                                                                        thematic-substrate::dont-show-variables
                                                                                        thematic-substrate::dont-show-head-projection-operators-p
                                                                                        thematic-substrate::dont-show-lambdas-p
                                                                                        thematic-substrate::how-many
                                                                                        thematic-substrate::only-new-tuples-p
                                                                                        thematic-substrate::timeout
                                                                                        thematic-substrate::proactive-tuple-computation-p
                                                                                        thematic-substrate::tuple-at-a-time-p
                                                                                        thematic-substrate::use-individual-synonyms-p
                                                                                        thematic-substrate::check-abox-consistency-p
                                                                                        thematic-substrate::ensure-tbox-classification-p
                                                                                        thematic-substrate::initial-abox-mirroring-p
                                                                                        thematic-substrate::initial-role-assertion-mirroring-p
                                                                                        thematic-substrate::classify-concepts-in-instance-assertions-p
                                                                                        thematic-substrate::exclude-permutations-p
                                                                                        thematic-substrate::record-explanations-p))
                                 (execute-or-reexecute-all-rules (&key
                                                                  thematic-substrate::debug-p
                                                                  thematic-substrate::abox
                                                                  thematic-substrate::type-of-substrate))
                                 (execute-or-reexecute-all-queries (&key
                                                                    thematic-substrate::debug-p
                                                                    thematic-substrate::abox
                                                                    thematic-substrate::type-of-substrate
                                                                    thematic-substrate::dont-add-abox-duplicates-p
                                                                    thematic-substrate::remove-duplicates-p
                                                                    thematic-substrate::two-phase-processing-p
                                                                    thematic-substrate::deliver-phase-two-warning-tokens-p
                                                                    thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                                    thematic-substrate::add-rule-consequences-p
                                                                    thematic-substrate::continuation-based-instance-retrieval-p
                                                                    thematic-substrate::told-information-reasoning-p
                                                                    thematic-substrate::final-consistency-checking-p
                                                                    thematic-substrate::runtime-consistency-checking-p
                                                                    thematic-substrate::verbose-p
                                                                    thematic-substrate::dont-show-variables
                                                                    thematic-substrate::dont-show-head-projection-operators-p
                                                                    thematic-substrate::dont-show-lambdas-p
                                                                    thematic-substrate::how-many
                                                                    thematic-substrate::only-new-tuples-p
                                                                    thematic-substrate::timeout
                                                                    thematic-substrate::proactive-tuple-computation-p
                                                                    thematic-substrate::tuple-at-a-time-p
                                                                    thematic-substrate::use-individual-synonyms-p
                                                                    thematic-substrate::check-abox-consistency-p
                                                                    thematic-substrate::ensure-tbox-classification-p
                                                                    thematic-substrate::initial-abox-mirroring-p
                                                                    thematic-substrate::initial-role-assertion-mirroring-p
                                                                    thematic-substrate::classify-concepts-in-instance-assertions-p
                                                                    thematic-substrate::exclude-permutations-p
                                                                    thematic-substrate::record-explanations-p))
                                 (execute-applicable-rules (&key
                                                            thematic-substrate::abox
                                                            thematic-substrate::type-of-substrate))
                                 (execute-all-rules (&key
                                                     thematic-substrate::abox
                                                     thematic-substrate::type-of-substrate))
                                 (execute-all-queries (&key
                                                       thematic-substrate::abox
                                                       thematic-substrate::type-of-substrate
                                                       thematic-substrate::dont-add-abox-duplicates-p
                                                       thematic-substrate::remove-duplicates-p
                                                       thematic-substrate::two-phase-processing-p
                                                       thematic-substrate::deliver-phase-two-warning-tokens-p
                                                       thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                       thematic-substrate::add-rule-consequences-p
                                                       thematic-substrate::continuation-based-instance-retrieval-p
                                                       thematic-substrate::told-information-reasoning-p
                                                       thematic-substrate::final-consistency-checking-p
                                                       thematic-substrate::runtime-consistency-checking-p
                                                       thematic-substrate::verbose-p
                                                       thematic-substrate::dont-show-variables
                                                       thematic-substrate::dont-show-head-projection-operators-p
                                                       thematic-substrate::dont-show-lambdas-p
                                                       thematic-substrate::how-many
                                                       thematic-substrate::only-new-tuples-p
                                                       thematic-substrate::timeout
                                                       thematic-substrate::proactive-tuple-computation-p
                                                       thematic-substrate::tuple-at-a-time-p
                                                       thematic-substrate::use-individual-synonyms-p
                                                       thematic-substrate::check-abox-consistency-p
                                                       thematic-substrate::ensure-tbox-classification-p
                                                       thematic-substrate::initial-abox-mirroring-p
                                                       thematic-substrate::initial-role-assertion-mirroring-p
                                                       thematic-substrate::classify-concepts-in-instance-assertions-p
                                                       thematic-substrate::exclude-permutations-p
                                                       thematic-substrate::record-explanations-p))
                                 (exclude-permutations nil)
                                 (evaluate1 nil)
                                 (evaluate nil)
                                 (equivalent (left
                                              right))
                                 (ensure-tbox-signature (tbox &key
                                                                     atomic-concepts
                                                                     roles
                                                                     transitive-roles
                                                                     features
                                                                     attributes))
                                 (ensure-subsumption-based-query-answering (&optional
                                                                            abox))
                                 (ensure-abox-signature (abox-name-or-abox
                                                         &key
                                                         individuals
                                                         objects))
                                 (enable-very-smart-abox-mirroring nil)
                                 (enable-two-phase-query-processing-mode nil)
                                 (enable-told-information-querying nil)
                                 (enable-smart-abox-mirroring nil)
                                 (enable-rcc-substrate-mirroring nil)
                                 (enable-query-repository nil)
                                 (enable-query-realization nil)
                                 (enable-query-optimization nil)
                                 (enable-phase-two-starts-warning-tokens nil)
                                 (enable-optimized-query-processing (&optional
                                                                     rewrite-concept-definitions))
                                 (enable-nrql-warnings nil)
                                 (enable-lazy-unfolding-of-defined-queries nil)
                                 (enable-lazy-tuple-computation nil)
                                 (enable-kb-has-changed-warning-tokens nil)
                                 (enable-eager-tuple-computation nil)
                                 (enable-defined-queries nil)
                                 (enable-data-substrate-mirroring nil)
                                 (enable-abox-mirroring nil)
                                 (enable-abduction (thematic-substrate::c-mode
                                                    thematic-substrate::r-mode
                                                    &key
                                                    thematic-substrate::reset-p
                                                    thematic-substrate::hypo-mode-stack
                                                    thematic-substrate::runtime-consistency-checking-p
                                                    thematic-substrate::final-consistency-checking-p
                                                    thematic-substrate::ensure-permutations-p
                                                    thematic-substrate::same-as-only-p
                                                    thematic-substrate::how-many
                                                    thematic-substrate::candidate-individuals
                                                    thematic-substrate::binding-validator
                                                    thematic-substrate::cutoff-fn
                                                    thematic-substrate::order-by
                                                    thematic-substrate::only-best-p
                                                    thematic-substrate::reverse-order-p))
                                 (edge-label1 (thematic-substrate::from
                                               thematic-substrate::to
                                               &optional
                                               thematic-substrate::abox
                                               thematic-substrate::type-of-substrate))
                                 (edge-label (thematic-substrate::from
                                              thematic-substrate::to
                                              &optional
                                              thematic-substrate::abox
                                              thematic-substrate::type-of-substrate))
                                 (edge-description1 (thematic-substrate::from
                                                     thematic-substrate::to
                                                     &optional
                                                     thematic-substrate::abox
                                                     thematic-substrate::type-of-substrate))
                                 (edge-description (thematic-substrate::from
                                                    thematic-substrate::to
                                                    &optional
                                                    thematic-substrate::abox
                                                    thematic-substrate::type-of-substrate))
                                 (dont-use-injective-variables-by-default nil)
                                 (dont-use-individual-synonym-equivalence-classes nil)
                                 (dont-report-inconsistent-queries-and-rules nil)
                                 (dont-prefer-defined-queries nil)
                                 (dont-keep-defined-query-atoms nil)
                                 (dont-check-abox-consistency-before-querying nil)
                                 (dont-allow-overloaded-definitions nil)
                                 (dont-add-rule-consequences-automatically nil)
                                 (dont-add-role-assertions-for-datatype-properties nil)
                                 (dont-add-missing-top-conjuncts nil)
                                 (domain (rolename
                                          concept
                                          &optional
                                          tbox
                                          errorp))
                                 (disjoint (&whole
                                            form
                                            &rest
                                            concept-names))
                                 (disable-two-phase-query-processing-mode nil)
                                 (disable-told-information-querying nil)
                                 (disable-rcc-substrate-mirroring nil)
                                 (disable-query-repository nil)
                                 (disable-query-realization nil)
                                 (disable-query-optimization nil)
                                 (disable-phase-two-starts-warning-tokens nil)
                                 (disable-nrql-warnings nil)
                                 (disable-lazy-unfolding-of-defined-queries nil)
                                 (disable-kb-has-changed-warning-tokens nil)
                                 (disable-defined-queries nil)
                                 (disable-data-substrate-mirroring nil)
                                 (disable-abox-mirroring nil)
                                 (disable-abduction (&key
                                                     thematic-substrate::reset-p))
                                 (direct-predecessors (role-term
                                                       ind-filler
                                                       &optional
                                                       abox))
                                 (dig-read-file (filename
                                                 &key
                                                 kb-name
                                                 init))
                                 (dig-read-document (url-spec
                                                     &optional
                                                     kb-name
                                                     init))
                                 (different-from (individual-name-1
                                                  individual-name-2))
                                 (description-implies? (a
                                                        thematic-substrate::b))
                                 (description-implies-p (a
                                                         thematic-substrate::b))
                                 (describe-tbox (&optional
                                                 tbox
                                                 stream))
                                 (describe-substrate (&key
                                                      thematic-substrate::abox
                                                      thematic-substrate::type-of-substrate))
                                 (describe-rule-status (thematic-substrate::query))
                                 (describe-rule (thematic-substrate::query &optional
                                                                           thematic-substrate::rewritten-p))
                                 (describe-role (role-term
                                                 &optional
                                                 tbox
                                                 stream))
                                 (describe-query-status (thematic-substrate::query))
                                 (describe-query-processing-mode nil)
                                 (describe-query (thematic-substrate::query &optional
                                                                            thematic-substrate::rewritten-p))
                                 (describe-individual1 (individual-name
                                                        &optional
                                                        abox
                                                        stream))
                                 (describe-individual (individual-name
                                                       &optional
                                                       abox
                                                       stream))
                                 (describe-definition (thematic-substrate::name &key
                                                                                thematic-substrate::tbox
                                                                                thematic-substrate::arity
                                                                                thematic-substrate::error-p))
                                 (describe-current-substrate nil)
                                 (describe-concept (concept-name
                                                    &optional
                                                    tbox
                                                    stream))
                                 (describe-all-substrates nil)
                                 (describe-all-rules (&optional
                                                      thematic-substrate::rewritten-p
                                                      &key
                                                      thematic-substrate::abox
                                                      thematic-substrate::type-of-substrate))
                                 (describe-all-queries (&optional
                                                        thematic-substrate::rewritten-p
                                                        &key
                                                        thematic-substrate::abox
                                                        thematic-substrate::type-of-substrate))
                                 (describe-all-nodes (&key
                                                      thematic-substrate::abox
                                                      thematic-substrate::type-of-substrate))
                                 (describe-all-edges (&key
                                                      thematic-substrate::abox
                                                      thematic-substrate::type-of-substrate))
                                 (describe-all-definitions (&key
                                                            thematic-substrate::tbox
                                                            thematic-substrate::error-p))
                                 (describe-abox (&optional
                                                 abox
                                                 stream))
                                 (delete-tbox (tbox))
                                 (delete-rule (thematic-substrate::query))
                                 (delete-rcc-synonyms nil)
                                 (delete-query (thematic-substrate::query))
                                 (delete-prefix-mappings nil)
                                 (delete-data-node (thematic-substrate::name &key
                                                                             thematic-substrate::abox
                                                                             thematic-substrate::type-of-substrate
                                                                             thematic-substrate::told-info-p))
                                 (delete-data-edge (thematic-substrate::from
                                                    thematic-substrate::to
                                                    &key
                                                    thematic-substrate::abox
                                                    thematic-substrate::type-of-substrate
                                                    thematic-substrate::told-info-p))
                                 (delete-all-tboxes nil)
                                 (delete-all-substrates (&key
                                                         thematic-substrate::abox
                                                         thematic-substrate::type-of-substrate))
                                 (delete-all-rules (&key
                                                    thematic-substrate::abox
                                                    thematic-substrate::type-of-substrate))
                                 (delete-all-queries (&key
                                                      thematic-substrate::abox
                                                      thematic-substrate::type-of-substrate))
                                 (delete-all-definitions (&key
                                                          thematic-substrate::tbox))
                                 (delete-all-aboxes nil)
                                 (delete-abox (abox))
                                 (del-rcc-node1 nil)
                                 (del-rcc-node nil)
                                 (del-rcc-edge1 nil)
                                 (del-rcc-edge nil)
                                 (del-doc-entry1 (label))
                                 (del-doc-entry (label))
                                 (del-data-node1 (thematic-substrate::name &optional
                                                                           thematic-substrate::abox
                                                                           thematic-substrate::type-of-substrate))
                                 (del-data-node (thematic-substrate::name &optional
                                                                          thematic-substrate::abox
                                                                          thematic-substrate::type-of-substrate))
                                 (del-data-edge1 (thematic-substrate::from
                                                  thematic-substrate::to
                                                  &optional
                                                  thematic-substrate::abox
                                                  thematic-substrate::type-of-substrate))
                                 (del-data-edge (thematic-substrate::from
                                                 thematic-substrate::to
                                                 &optional
                                                 thematic-substrate::abox
                                                 thematic-substrate::type-of-substrate))
                                 (defquery (thematic-substrate::name thematic-substrate::head
                                                                     thematic-substrate::body
                                                                     &key
                                                                     thematic-substrate::keep-p
                                                                     thematic-substrate::tbox
                                                                     thematic-substrate::consider-head-atom-for-consistency-check-p
                                                                     thematic-substrate::allow-multiple-definitions-p))
                                 (defpar1 (thematic-substrate::name thematic-substrate::value))
                                 (defpar (thematic-substrate::name thematic-substrate::value))
                                 (define1 (thematic-substrate::name thematic-substrate::arglist))
                                 (define-tbox (name
                                               &rest
                                               axioms))
                                 (define-rule (lefthand-side
                                               righthand-side
                                               &key
                                               forward-rule-p
                                               backward-rule-p))
                                 (define-query (thematic-substrate::name thematic-substrate::head
                                                                         thematic-substrate::body
                                                                         &key
                                                                         thematic-substrate::keep-p
                                                                         thematic-substrate::tbox
                                                                         thematic-substrate::consider-head-atom-for-consistency-check-p
                                                                         thematic-substrate::allow-multiple-definitions-p))
                                 (define-primitive-role (name
                                                         &key
                                                         parents
                                                         parent
                                                         transitive
                                                         feature
                                                         domain
                                                         range
                                                         inverse
                                                         symmetric
                                                         reflexive
                                                         irreflexive
                                                         asymmetric
                                                         compositions))
                                 (define-primitive-concept (name
                                                            &optional
                                                            definition))
                                 (define-primitive-attribute (name
                                                              &key
                                                              parent
                                                              parents
                                                              domain
                                                              range
                                                              inverse
                                                              symmetric
                                                              asymmetric
                                                              reflexive
                                                              irreflexive))
                                 (define-prefix (prefix
                                                 mapping))
                                 (define-individual (individual-name
                                                     &optional
                                                     concept))
                                 (define-event-rule (head
                                                     &rest
                                                     body))
                                 (define-event-assertion (assertion))
                                 (define-distinct-individual (individual-name
                                                              &optional
                                                              concept))
                                 (define-disjoint-primitive-concept (&whole
                                                                     form
                                                                     name
                                                                     disjoint-list
                                                                     definition))
                                 (define-datatype-property (&rest
                                                            args))
                                 (define-concrete-domain-attribute (name
                                                                    &key
                                                                    domain
                                                                    type))
                                 (define-concept (name
                                                  definition))
                                 (define-and-prepare-query (thematic-substrate::name thematic-substrate::head
                                                                                     thematic-substrate::body
                                                                                     &key
                                                                                     thematic-substrate::keep-p
                                                                                     thematic-substrate::tbox
                                                                                     thematic-substrate::consider-head-atom-for-consistency-check-p
                                                                                     thematic-substrate::allow-multiple-definitions-p))
                                 (define-and-execute-query (thematic-substrate::name thematic-substrate::head
                                                                                     thematic-substrate::body
                                                                                     &key
                                                                                     thematic-substrate::keep-p
                                                                                     thematic-substrate::tbox
                                                                                     thematic-substrate::consider-head-atom-for-consistency-check-p
                                                                                     thematic-substrate::allow-multiple-definitions-p))
                                 (define-abox (abox-name &body
                                                                axioms))
                                 (define (thematic-substrate::name thematic-substrate::arglist))
                                 (defcon1 (thematic-substrate::name thematic-substrate::value))
                                 (defcon (thematic-substrate::name thematic-substrate::value))
                                 (def-and-prep-query (thematic-substrate::name thematic-substrate::head
                                                                               thematic-substrate::body
                                                                               &key
                                                                               thematic-substrate::keep-p
                                                                               thematic-substrate::tbox
                                                                               thematic-substrate::consider-head-atom-for-consistency-check-p
                                                                               thematic-substrate::allow-multiple-definitions-p))
                                 (def-and-exec-query (thematic-substrate::name thematic-substrate::head
                                                                               thematic-substrate::body
                                                                               &key
                                                                               thematic-substrate::keep-p
                                                                               thematic-substrate::tbox
                                                                               thematic-substrate::consider-head-atom-for-consistency-check-p
                                                                               thematic-substrate::allow-multiple-definitions-p))
                                 (declare-disjoint (concepts
                                                    tbox))
                                 (declare-current-knowledge-bases-as-persistent nil)
                                 (deactivate-defined-query (thematic-substrate::name thematic-substrate::arity
                                                                                     &key
                                                                                     thematic-substrate::pos
                                                                                     thematic-substrate::tbox))
                                 (datatype-role-range (role-name tbox))
                                 (datatype-role-has-range (rolename
                                                           range
                                                           tbox))
                                 (datatype-role-filler (individual
                                                        value
                                                        role
                                                        &optional
                                                        type))
                                 (data-node1 (thematic-substrate::name &optional
                                                                       thematic-substrate::descr
                                                                       thematic-substrate::racer-descr
                                                                       thematic-substrate::abox
                                                                       thematic-substrate::type-of-substrate))
                                 (data-node (thematic-substrate::name &optional
                                                                      thematic-substrate::descr
                                                                      thematic-substrate::racer-descr
                                                                      thematic-substrate::abox
                                                                      thematic-substrate::type-of-substrate))
                                 (data-edge1 (thematic-substrate::from
                                              thematic-substrate::to
                                              thematic-substrate::data-relation
                                              &optional
                                              thematic-substrate::racer-descr
                                              thematic-substrate::abox
                                              thematic-substrate::type-of-substrate))
                                 (data-edge (thematic-substrate::from
                                             thematic-substrate::to
                                             thematic-substrate::data-relation
                                             &optional
                                             thematic-substrate::racer-descr
                                             thematic-substrate::abox
                                             thematic-substrate::type-of-substrate))
                                 (current-tbox nil)
                                 (current-abox nil)
                                 (create-triple-store (name
                                                       &key
                                                       if-exists
                                                       directory
                                                       data-version-level))
                                 (create-tbox-internal-marker-concept (tbox &optional
                                                                                   marker-name))
                                 (create-tbox-clone (tbox &key
                                                                 new-name
                                                                 overwrite))
                                 (create-subgraph-aboxes (thematic-substrate::abox-or-name
                                                          &optional
                                                          thematic-substrate::new-name
                                                          thematic-substrate::tbox))
                                 (create-rcc-node (&key
                                                   thematic-substrate::abox
                                                   thematic-substrate::type-of-substrate
                                                   thematic-substrate::racer-descr
                                                   thematic-substrate::descr
                                                   thematic-substrate::told-info-p))
                                 (create-rcc-edge (&key
                                                   thematic-substrate::abox
                                                   thematic-substrate::type-of-substrate
                                                   thematic-substrate::racer-descr
                                                   thematic-substrate::told-info-p))
                                 (create-data-node (thematic-substrate::name &key
                                                                             thematic-substrate::abox
                                                                             thematic-substrate::type-of-substrate
                                                                             thematic-substrate::racer-descr
                                                                             thematic-substrate::descr
                                                                             thematic-substrate::told-info-p))
                                 (create-data-edge (thematic-substrate::from
                                                    thematic-substrate::to
                                                    thematic-substrate::descr
                                                    &key
                                                    thematic-substrate::abox
                                                    thematic-substrate::type-of-substrate
                                                    thematic-substrate::racer-descr
                                                    thematic-substrate::told-info-p))
                                 (create-abox-clone (abox
                                                     &key
                                                     new-name
                                                     overwrite
                                                     copy-rules))
                                 (copy-rules (thematic-substrate::from-abox
                                              thematic-substrate::to-abox
                                              &key
                                              thematic-substrate::type-of-substrate
                                              thematic-substrate::keep-old-names-p))
                                 (convert-event-specs (in-file
                                                       out-file))
                                 (constraints (&body forms))
                                 (constraint-entailed? (constraint &optional
                                                                   abox-name))
                                 (constraint-entailed-p (constraint &optional
                                                                    abox))
                                 (constrained (individual
                                               object
                                               attribute))
                                 (concept? (concept-name
                                            &optional
                                            tbox-name))
                                 (concept-synonyms (concept-term &optional
                                                                        tbox))
                                 (concept-subsumes? (concept-1
                                                     concept-2
                                                     &optional
                                                     tbox-name))
                                 (concept-subsumes-p (subsumer
                                                      subsumee
                                                      tbox))
                                 (concept-satisfiable? (concept-1
                                                        &optional
                                                        tbox-name))
                                 (concept-satisfiable-p (concept-term tbox))
                                 (concept-parents (concept-term &optional
                                                                       tbox))
                                 (concept-p (concept-name
                                             &optional
                                             tbox))
                                 (concept-is-primitive? (concept-name
                                                         &optional
                                                         tbox))
                                 (concept-is-primitive-p (concept-name
                                                          &optional
                                                          tbox))
                                 (concept-instances (concept-term &optional
                                                                         abox
                                                                         candidates))
                                 (concept-equivalent? (concept-1
                                                       concept-2
                                                       &optional
                                                       tbox-name))
                                 (concept-equivalent-p (concept-1
                                                        concept-2
                                                        tbox))
                                 (concept-disjoint? (concept-1
                                                     concept-2
                                                     &optional
                                                     tbox-name))
                                 (concept-disjoint-p (concept-1
                                                      concept-2
                                                      tbox))
                                 (concept-descendants (concept-term &optional
                                                                           tbox))
                                 (concept-children (concept-term &optional
                                                                        tbox))
                                 (concept-ancestors (concept-term &optional
                                                                         tbox))
                                 (compute-subgraph-aboxes (thematic-substrate::abox-or-name))
                                 (compute-index-for-instance-retrieval (&optional
                                                                        abox))
                                 (compute-implicit-role-fillers (individual-name
                                                                 &optional
                                                                 abox))
                                 (compute-all-implicit-role-fillers (&optional
                                                                     abox))
                                 (compute-abox-difference2 (a
                                                            thematic-substrate::b
                                                            &key
                                                            thematic-substrate::also-unmapped-differences-p
                                                            thematic-substrate::remove-redundant-diffs-p
                                                            thematic-substrate::optimizer-max-plans
                                                            thematic-substrate::known-correspondances
                                                            thematic-substrate::auto-correspondances-p
                                                            thematic-substrate::only-difference-p
                                                            thematic-substrate::full-tuples-p
                                                            thematic-substrate::show-score-p
                                                            thematic-substrate::equi-order-by
                                                            thematic-substrate::remove-implied-concept-assertions-p
                                                            thematic-substrate::remove-common-assertions-p
                                                            thematic-substrate::common-assertions-as-strict-atoms-p
                                                            thematic-substrate::map-new-inds-to-new-inds-p
                                                            thematic-substrate::cutoff-fn
                                                            thematic-substrate::hypo-mode-stack
                                                            thematic-substrate::c-mode
                                                            thematic-substrate::r-mode
                                                            thematic-substrate::only-best-p
                                                            thematic-substrate::order-by
                                                            thematic-substrate::reverse-order-p
                                                            thematic-substrate::ensure-permutations-p
                                                            thematic-substrate::how-many
                                                            thematic-substrate::strategy
                                                            thematic-substrate::simple-result-p
                                                            thematic-substrate::runtime-consistency-checking-p
                                                            thematic-substrate::final-consistency-checking-p
                                                            thematic-substrate::same-as-only-p
                                                            thematic-substrate::candidate-individuals
                                                            thematic-substrate::binding-validator))
                                 (compute-abox-difference1 (a
                                                            thematic-substrate::b
                                                            &key
                                                            thematic-substrate::also-unmapped-differences-p
                                                            thematic-substrate::remove-redundant-diffs-p
                                                            thematic-substrate::optimizer-max-plans
                                                            thematic-substrate::known-correspondances
                                                            thematic-substrate::auto-correspondances-p
                                                            thematic-substrate::only-difference-p
                                                            thematic-substrate::full-tuples-p
                                                            thematic-substrate::show-score-p
                                                            thematic-substrate::equi-order-by
                                                            thematic-substrate::remove-implied-concept-assertions-p
                                                            thematic-substrate::remove-common-assertions-p
                                                            thematic-substrate::common-assertions-as-strict-atoms-p
                                                            thematic-substrate::map-new-inds-to-new-inds-p
                                                            thematic-substrate::cutoff-fn
                                                            thematic-substrate::hypo-mode-stack
                                                            thematic-substrate::c-mode
                                                            thematic-substrate::r-mode
                                                            thematic-substrate::only-best-p
                                                            thematic-substrate::order-by
                                                            thematic-substrate::reverse-order-p
                                                            thematic-substrate::ensure-permutations-p
                                                            thematic-substrate::how-many
                                                            thematic-substrate::strategy
                                                            thematic-substrate::simple-result-p
                                                            thematic-substrate::runtime-consistency-checking-p
                                                            thematic-substrate::final-consistency-checking-p
                                                            thematic-substrate::same-as-only-p
                                                            thematic-substrate::candidate-individuals
                                                            thematic-substrate::binding-validator))
                                 (compute-abox-difference-alternative (a
                                                                       thematic-substrate::b
                                                                       &key
                                                                       thematic-substrate::also-unmapped-differences-p
                                                                       thematic-substrate::remove-redundant-diffs-p
                                                                       thematic-substrate::optimizer-max-plans
                                                                       thematic-substrate::known-correspondances
                                                                       thematic-substrate::auto-correspondances-p
                                                                       thematic-substrate::only-difference-p
                                                                       thematic-substrate::full-tuples-p
                                                                       thematic-substrate::show-score-p
                                                                       thematic-substrate::equi-order-by
                                                                       thematic-substrate::remove-implied-concept-assertions-p
                                                                       thematic-substrate::remove-common-assertions-p
                                                                       thematic-substrate::common-assertions-as-strict-atoms-p
                                                                       thematic-substrate::map-new-inds-to-new-inds-p
                                                                       thematic-substrate::cutoff-fn
                                                                       thematic-substrate::hypo-mode-stack
                                                                       thematic-substrate::c-mode
                                                                       thematic-substrate::r-mode
                                                                       thematic-substrate::only-best-p
                                                                       thematic-substrate::order-by
                                                                       thematic-substrate::reverse-order-p
                                                                       thematic-substrate::ensure-permutations-p
                                                                       thematic-substrate::how-many
                                                                       thematic-substrate::strategy
                                                                       thematic-substrate::simple-result-p
                                                                       thematic-substrate::runtime-consistency-checking-p
                                                                       thematic-substrate::final-consistency-checking-p
                                                                       thematic-substrate::same-as-only-p
                                                                       thematic-substrate::candidate-individuals
                                                                       thematic-substrate::binding-validator))
                                 (compute-abox-difference (a
                                                           thematic-substrate::b
                                                           &key
                                                           thematic-substrate::also-unmapped-differences-p
                                                           thematic-substrate::remove-redundant-diffs-p
                                                           thematic-substrate::optimizer-max-plans
                                                           thematic-substrate::known-correspondances
                                                           thematic-substrate::auto-correspondances-p
                                                           thematic-substrate::only-difference-p
                                                           thematic-substrate::full-tuples-p
                                                           thematic-substrate::show-score-p
                                                           thematic-substrate::equi-order-by
                                                           thematic-substrate::remove-implied-concept-assertions-p
                                                           thematic-substrate::remove-common-assertions-p
                                                           thematic-substrate::common-assertions-as-strict-atoms-p
                                                           thematic-substrate::map-new-inds-to-new-inds-p
                                                           thematic-substrate::cutoff-fn
                                                           thematic-substrate::hypo-mode-stack
                                                           thematic-substrate::c-mode
                                                           thematic-substrate::r-mode
                                                           thematic-substrate::only-best-p
                                                           thematic-substrate::order-by
                                                           thematic-substrate::reverse-order-p
                                                           thematic-substrate::ensure-permutations-p
                                                           thematic-substrate::how-many
                                                           thematic-substrate::strategy
                                                           thematic-substrate::simple-result-p
                                                           thematic-substrate::runtime-consistency-checking-p
                                                           thematic-substrate::final-consistency-checking-p
                                                           thematic-substrate::same-as-only-p
                                                           thematic-substrate::candidate-individuals
                                                           thematic-substrate::binding-validator))
                                 (close-triple-store (&key
                                                      db
                                                      if-closed))
                                 (clone-tbox (tbox &key
                                                          new-name
                                                          overwrite))
                                 (clone-abox (abox
                                              &key
                                              new-name
                                              overwrite))
                                 (clear-default-tbox nil)
                                 (clear-all-documentation nil)
                                 (classify-tbox (&optional
                                                 tbox))
                                 (classify-query (thematic-substrate::query))
                                 (choose-current-set-of-rule-consequences (thematic-substrate::query))
                                 (check-tbox-coherence (&optional
                                                        tbox
                                                        &key
                                                        stream))
                                 (check-subscriptions (abox))
                                 (check-ontology (thematic-substrate::filename
                                                  &key
                                                  thematic-substrate::verbose
                                                  thematic-substrate::explain-all
                                                  thematic-substrate::n))
                                 (check-nrql-subscriptions (&optional
                                                            thematic-substrate::abox))
                                 (check-for-updates (&key
                                                     thematic-substrate::url))
                                 (check-concept-coherence (thematic-substrate::concept
                                                           &optional
                                                           thematic-substrate::tbox))
                                 (check-abox-consistency-before-querying nil)
                                 (check-abox-coherence (&optional
                                                        abox
                                                        filename-or-stream))
                                 (cheap-rules (&key
                                               thematic-substrate::abox
                                               thematic-substrate::type-of-substrate))
                                 (cheap-rule-p (thematic-substrate::query))
                                 (cheap-query-p (thematic-substrate::query))
                                 (cheap-queries (&key
                                                 thematic-substrate::abox
                                                 thematic-substrate::type-of-substrate))
                                 (cd-object? (object-name
                                              &optional
                                              abox-name))
                                 (cd-object-p (object-name
                                               &optional
                                               abox))
                                 (cd-attribute? (attribute
                                                 &optional
                                                 tbox-name))
                                 (cd-attribute-p (attribute
                                                  &optional
                                                  tbox))
                                 (attribute-type (attribute-name
                                                  &optional
                                                  tbox))
                                 (attribute-filler (individual
                                                    value
                                                    attribute
                                                    &optional
                                                    type))
                                 (attribute-domain-1 (attribute-name
                                                      &optional
                                                      tbox))
                                 (attribute-domain (attribute-name
                                                    &optional
                                                    tbox))
                                 (atomic-role-synonyms (role-term
                                                        tbox))
                                 (atomic-role-range (role-term
                                                     tbox))
                                 (atomic-role-parents (role-term
                                                       tbox
                                                       &key
                                                       synsets-p))
                                 (atomic-role-inverse (role-term
                                                       &optional
                                                       tbox))
                                 (atomic-role-domain (role-term
                                                      tbox))
                                 (atomic-role-descendants (role-term
                                                           tbox))
                                 (atomic-role-children (role-term
                                                        tbox
                                                        &key
                                                        synsets-p))
                                 (atomic-role-ancestors (role-term
                                                         tbox))
                                 (atomic-concept-synonyms (concept-term tbox))
                                 (atomic-concept-parents (concept-term tbox))
                                 (atomic-concept-descendants (concept-term tbox))
                                 (atomic-concept-children (concept-term tbox))
                                 (atomic-concept-ancestors (concept-term tbox))
                                 (asymmetric? (role-term
                                               &optional
                                               tbox-name))
                                 (asymmetric-p (role-term
                                                &optional
                                                tbox))
                                 (asymmetric (rolename
                                              &optional
                                              tbox))
                                 (associated-tbox (abox))
                                 (associated-aboxes (tbox))
                                 (apply-rule-under-premise1 (&key
                                                             thematic-substrate::execute-p
                                                             thematic-substrate::parser-class
                                                             thematic-substrate::rewrite-defined-concepts-p
                                                             thematic-substrate::group-by-ops
                                                             thematic-substrate::bind-specials-p
                                                             thematic-substrate::original-query
                                                             thematic-substrate::rule-con-pattern
                                                             thematic-substrate::new-ind-ops
                                                             thematic-substrate::premise
                                                             thematic-substrate::generate-code-p
                                                             thematic-substrate::optimize-p
                                                             thematic-substrate::rewrite-semantically-p
                                                             thematic-substrate::rewrite-to-dnf-p
                                                             thematic-substrate::report-inconsistent-queries-p
                                                             thematic-substrate::report-tautological-queries-p
                                                             thematic-substrate::use-repository-p
                                                             thematic-substrate::put-into-repository-p
                                                             thematic-substrate::id
                                                             thematic-substrate::dont-check-id-p
                                                             thematic-substrate::parser
                                                             thematic-substrate::result-vois
                                                             thematic-substrate::substrate
                                                             thematic-substrate::abox
                                                             thematic-substrate::create-abox-if-not-found-p
                                                             package
                                                             thematic-substrate::type-of-substrate
                                                             thematic-substrate::prepare-now-p))
                                 (apply-rule-under-premise (&key
                                                            thematic-substrate::execute-p
                                                            thematic-substrate::parser-class
                                                            thematic-substrate::rewrite-defined-concepts-p
                                                            thematic-substrate::group-by-ops
                                                            thematic-substrate::bind-specials-p
                                                            thematic-substrate::original-query
                                                            thematic-substrate::rule-con-pattern
                                                            thematic-substrate::new-ind-ops
                                                            thematic-substrate::premise
                                                            thematic-substrate::generate-code-p
                                                            thematic-substrate::optimize-p
                                                            thematic-substrate::rewrite-semantically-p
                                                            thematic-substrate::rewrite-to-dnf-p
                                                            thematic-substrate::report-inconsistent-queries-p
                                                            thematic-substrate::report-tautological-queries-p
                                                            thematic-substrate::use-repository-p
                                                            thematic-substrate::put-into-repository-p
                                                            thematic-substrate::id
                                                            thematic-substrate::dont-check-id-p
                                                            thematic-substrate::parser
                                                            thematic-substrate::result-vois
                                                            thematic-substrate::substrate
                                                            thematic-substrate::abox
                                                            thematic-substrate::create-abox-if-not-found-p
                                                            package
                                                            thematic-substrate::type-of-substrate
                                                            thematic-substrate::prepare-now-p))
                                 (apply-rule (&key
                                              thematic-substrate::execute-p
                                              thematic-substrate::parser-class
                                              thematic-substrate::rewrite-defined-concepts-p
                                              thematic-substrate::group-by-ops
                                              thematic-substrate::bind-specials-p
                                              thematic-substrate::original-query
                                              thematic-substrate::rule-con-pattern
                                              thematic-substrate::new-ind-ops
                                              thematic-substrate::premise
                                              thematic-substrate::generate-code-p
                                              thematic-substrate::optimize-p
                                              thematic-substrate::rewrite-semantically-p
                                              thematic-substrate::rewrite-to-dnf-p
                                              thematic-substrate::report-inconsistent-queries-p
                                              thematic-substrate::report-tautological-queries-p
                                              thematic-substrate::use-repository-p
                                              thematic-substrate::put-into-repository-p
                                              thematic-substrate::id
                                              thematic-substrate::dont-check-id-p
                                              thematic-substrate::parser
                                              thematic-substrate::result-vois
                                              thematic-substrate::substrate
                                              thematic-substrate::abox
                                              thematic-substrate::create-abox-if-not-found-p
                                              package
                                              thematic-substrate::type-of-substrate
                                              thematic-substrate::prepare-now-p))
                                 (apply-abox-rule1 (thematic-substrate::res-args
                                                    thematic-substrate::query
                                                    &key
                                                    thematic-substrate::execute-p
                                                    thematic-substrate::parser-class
                                                    thematic-substrate::rewrite-defined-concepts-p
                                                    thematic-substrate::group-by-ops
                                                    thematic-substrate::bind-specials-p
                                                    thematic-substrate::original-query
                                                    thematic-substrate::rule-con-pattern
                                                    thematic-substrate::new-ind-ops
                                                    thematic-substrate::premise
                                                    thematic-substrate::generate-code-p
                                                    thematic-substrate::optimize-p
                                                    thematic-substrate::rewrite-semantically-p
                                                    thematic-substrate::rewrite-to-dnf-p
                                                    thematic-substrate::report-inconsistent-queries-p
                                                    thematic-substrate::report-tautological-queries-p
                                                    thematic-substrate::use-repository-p
                                                    thematic-substrate::put-into-repository-p
                                                    thematic-substrate::id
                                                    thematic-substrate::dont-check-id-p
                                                    thematic-substrate::parser
                                                    thematic-substrate::result-vois
                                                    thematic-substrate::substrate
                                                    thematic-substrate::abox
                                                    thematic-substrate::create-abox-if-not-found-p
                                                    package
                                                    thematic-substrate::type-of-substrate
                                                    thematic-substrate::prepare-now-p))
                                 (apply-abox-rule-under-premise1 (thematic-substrate::res-args
                                                                  thematic-substrate::query
                                                                  &key
                                                                  thematic-substrate::execute-p
                                                                  thematic-substrate::parser-class
                                                                  thematic-substrate::rewrite-defined-concepts-p
                                                                  thematic-substrate::group-by-ops
                                                                  thematic-substrate::bind-specials-p
                                                                  thematic-substrate::original-query
                                                                  thematic-substrate::rule-con-pattern
                                                                  thematic-substrate::new-ind-ops
                                                                  thematic-substrate::premise
                                                                  thematic-substrate::generate-code-p
                                                                  thematic-substrate::optimize-p
                                                                  thematic-substrate::rewrite-semantically-p
                                                                  thematic-substrate::rewrite-to-dnf-p
                                                                  thematic-substrate::report-inconsistent-queries-p
                                                                  thematic-substrate::report-tautological-queries-p
                                                                  thematic-substrate::use-repository-p
                                                                  thematic-substrate::put-into-repository-p
                                                                  thematic-substrate::id
                                                                  thematic-substrate::dont-check-id-p
                                                                  thematic-substrate::parser
                                                                  thematic-substrate::result-vois
                                                                  thematic-substrate::substrate
                                                                  thematic-substrate::abox
                                                                  thematic-substrate::create-abox-if-not-found-p
                                                                  package
                                                                  thematic-substrate::type-of-substrate
                                                                  thematic-substrate::prepare-now-p))
                                 (apply-abox-rule-under-premise (thematic-substrate::query thematic-substrate::res-args
                                                                                           &key
                                                                                           thematic-substrate::execute-p
                                                                                           thematic-substrate::parser-class
                                                                                           thematic-substrate::rewrite-defined-concepts-p
                                                                                           thematic-substrate::group-by-ops
                                                                                           thematic-substrate::bind-specials-p
                                                                                           thematic-substrate::original-query
                                                                                           thematic-substrate::rule-con-pattern
                                                                                           thematic-substrate::new-ind-ops
                                                                                           thematic-substrate::premise
                                                                                           thematic-substrate::generate-code-p
                                                                                           thematic-substrate::optimize-p
                                                                                           thematic-substrate::rewrite-semantically-p
                                                                                           thematic-substrate::rewrite-to-dnf-p
                                                                                           thematic-substrate::report-inconsistent-queries-p
                                                                                           thematic-substrate::report-tautological-queries-p
                                                                                           thematic-substrate::use-repository-p
                                                                                           thematic-substrate::put-into-repository-p
                                                                                           thematic-substrate::id
                                                                                           thematic-substrate::dont-check-id-p
                                                                                           thematic-substrate::parser
                                                                                           thematic-substrate::result-vois
                                                                                           thematic-substrate::substrate
                                                                                           thematic-substrate::abox
                                                                                           thematic-substrate::create-abox-if-not-found-p
                                                                                           package
                                                                                           thematic-substrate::type-of-substrate
                                                                                           thematic-substrate::prepare-now-p))
                                 (apply-abox-rule (thematic-substrate::query thematic-substrate::res-args
                                                                             &key
                                                                             thematic-substrate::execute-p
                                                                             thematic-substrate::parser-class
                                                                             thematic-substrate::rewrite-defined-concepts-p
                                                                             thematic-substrate::group-by-ops
                                                                             thematic-substrate::bind-specials-p
                                                                             thematic-substrate::original-query
                                                                             thematic-substrate::rule-con-pattern
                                                                             thematic-substrate::new-ind-ops
                                                                             thematic-substrate::premise
                                                                             thematic-substrate::generate-code-p
                                                                             thematic-substrate::optimize-p
                                                                             thematic-substrate::rewrite-semantically-p
                                                                             thematic-substrate::rewrite-to-dnf-p
                                                                             thematic-substrate::report-inconsistent-queries-p
                                                                             thematic-substrate::report-tautological-queries-p
                                                                             thematic-substrate::use-repository-p
                                                                             thematic-substrate::put-into-repository-p
                                                                             thematic-substrate::id
                                                                             thematic-substrate::dont-check-id-p
                                                                             thematic-substrate::parser
                                                                             thematic-substrate::result-vois
                                                                             thematic-substrate::substrate
                                                                             thematic-substrate::abox
                                                                             thematic-substrate::create-abox-if-not-found-p
                                                                             package
                                                                             thematic-substrate::type-of-substrate
                                                                             thematic-substrate::prepare-now-p))
                                 (applicable-rules (&key
                                                    thematic-substrate::abox
                                                    thematic-substrate::type-of-substrate))
                                 (answer-tbox-query1 (&key
                                                      thematic-substrate::execute-p
                                                      thematic-substrate::dont-add-abox-duplicates-p
                                                      thematic-substrate::remove-duplicates-p
                                                      thematic-substrate::two-phase-processing-p
                                                      thematic-substrate::deliver-phase-two-warning-tokens-p
                                                      thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                      thematic-substrate::add-rule-consequences-p
                                                      thematic-substrate::continuation-based-instance-retrieval-p
                                                      thematic-substrate::told-information-reasoning-p
                                                      thematic-substrate::final-consistency-checking-p
                                                      thematic-substrate::runtime-consistency-checking-p
                                                      thematic-substrate::verbose-p
                                                      thematic-substrate::dont-show-variables
                                                      thematic-substrate::dont-show-head-projection-operators-p
                                                      thematic-substrate::dont-show-lambdas-p
                                                      thematic-substrate::how-many
                                                      thematic-substrate::only-new-tuples-p
                                                      thematic-substrate::timeout
                                                      thematic-substrate::proactive-tuple-computation-p
                                                      thematic-substrate::tuple-at-a-time-p
                                                      thematic-substrate::use-individual-synonyms-p
                                                      thematic-substrate::check-abox-consistency-p
                                                      thematic-substrate::ensure-tbox-classification-p
                                                      thematic-substrate::initial-abox-mirroring-p
                                                      thematic-substrate::initial-role-assertion-mirroring-p
                                                      thematic-substrate::classify-concepts-in-instance-assertions-p
                                                      thematic-substrate::exclude-permutations-p
                                                      thematic-substrate::record-explanations-p
                                                      thematic-substrate::parser-class
                                                      thematic-substrate::rewrite-defined-concepts-p
                                                      thematic-substrate::group-by-ops
                                                      thematic-substrate::bind-specials-p
                                                      thematic-substrate::original-query
                                                      thematic-substrate::rule-con-pattern
                                                      thematic-substrate::new-ind-ops
                                                      thematic-substrate::premise
                                                      thematic-substrate::generate-code-p
                                                      thematic-substrate::optimize-p
                                                      thematic-substrate::rewrite-semantically-p
                                                      thematic-substrate::rewrite-to-dnf-p
                                                      thematic-substrate::report-inconsistent-queries-p
                                                      thematic-substrate::report-tautological-queries-p
                                                      thematic-substrate::use-repository-p
                                                      thematic-substrate::put-into-repository-p
                                                      thematic-substrate::id
                                                      thematic-substrate::dont-check-id-p
                                                      thematic-substrate::parser
                                                      thematic-substrate::result-vois
                                                      thematic-substrate::tbox
                                                      package
                                                      thematic-substrate::create-tbox-if-not-found-p
                                                      thematic-substrate::substrate))
                                 (answer-tbox-query (&key
                                                     thematic-substrate::execute-p
                                                     thematic-substrate::dont-add-abox-duplicates-p
                                                     thematic-substrate::remove-duplicates-p
                                                     thematic-substrate::two-phase-processing-p
                                                     thematic-substrate::deliver-phase-two-warning-tokens-p
                                                     thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                     thematic-substrate::add-rule-consequences-p
                                                     thematic-substrate::continuation-based-instance-retrieval-p
                                                     thematic-substrate::told-information-reasoning-p
                                                     thematic-substrate::final-consistency-checking-p
                                                     thematic-substrate::runtime-consistency-checking-p
                                                     thematic-substrate::verbose-p
                                                     thematic-substrate::dont-show-variables
                                                     thematic-substrate::dont-show-head-projection-operators-p
                                                     thematic-substrate::dont-show-lambdas-p
                                                     thematic-substrate::how-many
                                                     thematic-substrate::only-new-tuples-p
                                                     thematic-substrate::timeout
                                                     thematic-substrate::proactive-tuple-computation-p
                                                     thematic-substrate::tuple-at-a-time-p
                                                     thematic-substrate::use-individual-synonyms-p
                                                     thematic-substrate::check-abox-consistency-p
                                                     thematic-substrate::ensure-tbox-classification-p
                                                     thematic-substrate::initial-abox-mirroring-p
                                                     thematic-substrate::initial-role-assertion-mirroring-p
                                                     thematic-substrate::classify-concepts-in-instance-assertions-p
                                                     thematic-substrate::exclude-permutations-p
                                                     thematic-substrate::record-explanations-p
                                                     thematic-substrate::parser-class
                                                     thematic-substrate::rewrite-defined-concepts-p
                                                     thematic-substrate::group-by-ops
                                                     thematic-substrate::bind-specials-p
                                                     thematic-substrate::original-query
                                                     thematic-substrate::rule-con-pattern
                                                     thematic-substrate::new-ind-ops
                                                     thematic-substrate::premise
                                                     thematic-substrate::generate-code-p
                                                     thematic-substrate::optimize-p
                                                     thematic-substrate::rewrite-semantically-p
                                                     thematic-substrate::rewrite-to-dnf-p
                                                     thematic-substrate::report-inconsistent-queries-p
                                                     thematic-substrate::report-tautological-queries-p
                                                     thematic-substrate::use-repository-p
                                                     thematic-substrate::put-into-repository-p
                                                     thematic-substrate::id
                                                     thematic-substrate::dont-check-id-p
                                                     thematic-substrate::parser
                                                     thematic-substrate::result-vois
                                                     thematic-substrate::tbox
                                                     package
                                                     thematic-substrate::create-tbox-if-not-found-p
                                                     thematic-substrate::substrate))
                                 (answer-query1 (&key
                                                 thematic-substrate::execute-p
                                                 thematic-substrate::dont-add-abox-duplicates-p
                                                 thematic-substrate::remove-duplicates-p
                                                 thematic-substrate::two-phase-processing-p
                                                 thematic-substrate::deliver-phase-two-warning-tokens-p
                                                 thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                 thematic-substrate::add-rule-consequences-p
                                                 thematic-substrate::continuation-based-instance-retrieval-p
                                                 thematic-substrate::told-information-reasoning-p
                                                 thematic-substrate::final-consistency-checking-p
                                                 thematic-substrate::runtime-consistency-checking-p
                                                 thematic-substrate::verbose-p
                                                 thematic-substrate::dont-show-variables
                                                 thematic-substrate::dont-show-head-projection-operators-p
                                                 thematic-substrate::dont-show-lambdas-p
                                                 thematic-substrate::how-many
                                                 thematic-substrate::only-new-tuples-p
                                                 thematic-substrate::timeout
                                                 thematic-substrate::proactive-tuple-computation-p
                                                 thematic-substrate::tuple-at-a-time-p
                                                 thematic-substrate::use-individual-synonyms-p
                                                 thematic-substrate::check-abox-consistency-p
                                                 thematic-substrate::ensure-tbox-classification-p
                                                 thematic-substrate::initial-abox-mirroring-p
                                                 thematic-substrate::initial-role-assertion-mirroring-p
                                                 thematic-substrate::classify-concepts-in-instance-assertions-p
                                                 thematic-substrate::exclude-permutations-p
                                                 thematic-substrate::record-explanations-p
                                                 thematic-substrate::parser-class
                                                 thematic-substrate::rewrite-defined-concepts-p
                                                 thematic-substrate::group-by-ops
                                                 thematic-substrate::bind-specials-p
                                                 thematic-substrate::original-query
                                                 thematic-substrate::rule-con-pattern
                                                 thematic-substrate::new-ind-ops
                                                 thematic-substrate::premise
                                                 thematic-substrate::generate-code-p
                                                 thematic-substrate::optimize-p
                                                 thematic-substrate::rewrite-semantically-p
                                                 thematic-substrate::rewrite-to-dnf-p
                                                 thematic-substrate::report-inconsistent-queries-p
                                                 thematic-substrate::report-tautological-queries-p
                                                 thematic-substrate::use-repository-p
                                                 thematic-substrate::put-into-repository-p
                                                 thematic-substrate::id
                                                 thematic-substrate::dont-check-id-p
                                                 thematic-substrate::parser
                                                 thematic-substrate::result-vois
                                                 thematic-substrate::substrate
                                                 thematic-substrate::abox
                                                 thematic-substrate::create-abox-if-not-found-p
                                                 package
                                                 thematic-substrate::type-of-substrate
                                                 thematic-substrate::prepare-now-p))
                                 (answer-query-under-premise1 (&key
                                                               thematic-substrate::execute-p
                                                               thematic-substrate::dont-add-abox-duplicates-p
                                                               thematic-substrate::remove-duplicates-p
                                                               thematic-substrate::two-phase-processing-p
                                                               thematic-substrate::deliver-phase-two-warning-tokens-p
                                                               thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                               thematic-substrate::add-rule-consequences-p
                                                               thematic-substrate::continuation-based-instance-retrieval-p
                                                               thematic-substrate::told-information-reasoning-p
                                                               thematic-substrate::final-consistency-checking-p
                                                               thematic-substrate::runtime-consistency-checking-p
                                                               thematic-substrate::verbose-p
                                                               thematic-substrate::dont-show-variables
                                                               thematic-substrate::dont-show-head-projection-operators-p
                                                               thematic-substrate::dont-show-lambdas-p
                                                               thematic-substrate::how-many
                                                               thematic-substrate::only-new-tuples-p
                                                               thematic-substrate::timeout
                                                               thematic-substrate::proactive-tuple-computation-p
                                                               thematic-substrate::tuple-at-a-time-p
                                                               thematic-substrate::use-individual-synonyms-p
                                                               thematic-substrate::check-abox-consistency-p
                                                               thematic-substrate::ensure-tbox-classification-p
                                                               thematic-substrate::initial-abox-mirroring-p
                                                               thematic-substrate::initial-role-assertion-mirroring-p
                                                               thematic-substrate::classify-concepts-in-instance-assertions-p
                                                               thematic-substrate::exclude-permutations-p
                                                               thematic-substrate::record-explanations-p
                                                               thematic-substrate::parser-class
                                                               thematic-substrate::rewrite-defined-concepts-p
                                                               thematic-substrate::group-by-ops
                                                               thematic-substrate::bind-specials-p
                                                               thematic-substrate::original-query
                                                               thematic-substrate::rule-con-pattern
                                                               thematic-substrate::new-ind-ops
                                                               thematic-substrate::premise
                                                               thematic-substrate::generate-code-p
                                                               thematic-substrate::optimize-p
                                                               thematic-substrate::rewrite-semantically-p
                                                               thematic-substrate::rewrite-to-dnf-p
                                                               thematic-substrate::report-inconsistent-queries-p
                                                               thematic-substrate::report-tautological-queries-p
                                                               thematic-substrate::use-repository-p
                                                               thematic-substrate::put-into-repository-p
                                                               thematic-substrate::id
                                                               thematic-substrate::dont-check-id-p
                                                               thematic-substrate::parser
                                                               thematic-substrate::result-vois
                                                               thematic-substrate::substrate
                                                               thematic-substrate::abox
                                                               thematic-substrate::create-abox-if-not-found-p
                                                               package
                                                               thematic-substrate::type-of-substrate
                                                               thematic-substrate::prepare-now-p))
                                 (answer-query-under-premise (&key
                                                              thematic-substrate::execute-p
                                                              thematic-substrate::dont-add-abox-duplicates-p
                                                              thematic-substrate::remove-duplicates-p
                                                              thematic-substrate::two-phase-processing-p
                                                              thematic-substrate::deliver-phase-two-warning-tokens-p
                                                              thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                              thematic-substrate::add-rule-consequences-p
                                                              thematic-substrate::continuation-based-instance-retrieval-p
                                                              thematic-substrate::told-information-reasoning-p
                                                              thematic-substrate::final-consistency-checking-p
                                                              thematic-substrate::runtime-consistency-checking-p
                                                              thematic-substrate::verbose-p
                                                              thematic-substrate::dont-show-variables
                                                              thematic-substrate::dont-show-head-projection-operators-p
                                                              thematic-substrate::dont-show-lambdas-p
                                                              thematic-substrate::how-many
                                                              thematic-substrate::only-new-tuples-p
                                                              thematic-substrate::timeout
                                                              thematic-substrate::proactive-tuple-computation-p
                                                              thematic-substrate::tuple-at-a-time-p
                                                              thematic-substrate::use-individual-synonyms-p
                                                              thematic-substrate::check-abox-consistency-p
                                                              thematic-substrate::ensure-tbox-classification-p
                                                              thematic-substrate::initial-abox-mirroring-p
                                                              thematic-substrate::initial-role-assertion-mirroring-p
                                                              thematic-substrate::classify-concepts-in-instance-assertions-p
                                                              thematic-substrate::exclude-permutations-p
                                                              thematic-substrate::record-explanations-p
                                                              thematic-substrate::parser-class
                                                              thematic-substrate::rewrite-defined-concepts-p
                                                              thematic-substrate::group-by-ops
                                                              thematic-substrate::bind-specials-p
                                                              thematic-substrate::original-query
                                                              thematic-substrate::rule-con-pattern
                                                              thematic-substrate::new-ind-ops
                                                              thematic-substrate::premise
                                                              thematic-substrate::generate-code-p
                                                              thematic-substrate::optimize-p
                                                              thematic-substrate::rewrite-semantically-p
                                                              thematic-substrate::rewrite-to-dnf-p
                                                              thematic-substrate::report-inconsistent-queries-p
                                                              thematic-substrate::report-tautological-queries-p
                                                              thematic-substrate::use-repository-p
                                                              thematic-substrate::put-into-repository-p
                                                              thematic-substrate::id
                                                              thematic-substrate::dont-check-id-p
                                                              thematic-substrate::parser
                                                              thematic-substrate::result-vois
                                                              thematic-substrate::substrate
                                                              thematic-substrate::abox
                                                              thematic-substrate::create-abox-if-not-found-p
                                                              package
                                                              thematic-substrate::type-of-substrate
                                                              thematic-substrate::prepare-now-p))
                                 (answer-query (&key
                                                thematic-substrate::execute-p
                                                thematic-substrate::dont-add-abox-duplicates-p
                                                thematic-substrate::remove-duplicates-p
                                                thematic-substrate::two-phase-processing-p
                                                thematic-substrate::deliver-phase-two-warning-tokens-p
                                                thematic-substrate::deliver-kb-has-changed-warning-tokens-p
                                                thematic-substrate::add-rule-consequences-p
                                                thematic-substrate::continuation-based-instance-retrieval-p
                                                thematic-substrate::told-information-reasoning-p
                                                thematic-substrate::final-consistency-checking-p
                                                thematic-substrate::runtime-consistency-checking-p
                                                thematic-substrate::verbose-p
                                                thematic-substrate::dont-show-variables
                                                thematic-substrate::dont-show-head-projection-operators-p
                                                thematic-substrate::dont-show-lambdas-p
                                                thematic-substrate::how-many
                                                thematic-substrate::only-new-tuples-p
                                                thematic-substrate::timeout
                                                thematic-substrate::proactive-tuple-computation-p
                                                thematic-substrate::tuple-at-a-time-p
                                                thematic-substrate::use-individual-synonyms-p
                                                thematic-substrate::check-abox-consistency-p
                                                thematic-substrate::ensure-tbox-classification-p
                                                thematic-substrate::initial-abox-mirroring-p
                                                thematic-substrate::initial-role-assertion-mirroring-p
                                                thematic-substrate::classify-concepts-in-instance-assertions-p
                                                thematic-substrate::exclude-permutations-p
                                                thematic-substrate::record-explanations-p
                                                thematic-substrate::parser-class
                                                thematic-substrate::rewrite-defined-concepts-p
                                                thematic-substrate::group-by-ops
                                                thematic-substrate::bind-specials-p
                                                thematic-substrate::original-query
                                                thematic-substrate::rule-con-pattern
                                                thematic-substrate::new-ind-ops
                                                thematic-substrate::premise
                                                thematic-substrate::generate-code-p
                                                thematic-substrate::optimize-p
                                                thematic-substrate::rewrite-semantically-p
                                                thematic-substrate::rewrite-to-dnf-p
                                                thematic-substrate::report-inconsistent-queries-p
                                                thematic-substrate::report-tautological-queries-p
                                                thematic-substrate::use-repository-p
                                                thematic-substrate::put-into-repository-p
                                                thematic-substrate::id
                                                thematic-substrate::dont-check-id-p
                                                thematic-substrate::parser
                                                thematic-substrate::result-vois
                                                thematic-substrate::substrate
                                                thematic-substrate::abox
                                                thematic-substrate::create-abox-if-not-found-p
                                                package
                                                thematic-substrate::type-of-substrate
                                                thematic-substrate::prepare-now-p))
                                 (allow-overloaded-definitions nil)
                                 (all-transitive-roles (&optional
                                                        tbox
                                                        &key
                                                        count))
                                 (all-tboxes nil)
                                 (all-substrates (&key
                                                  thematic-substrate::abox
                                                  thematic-substrate::type-of-substrate))
                                 (all-same-as-assertions (&optional
                                                          thematic-substrate::abox
                                                          &key
                                                          count))
                                 (all-rules (&key
                                             thematic-substrate::abox
                                             thematic-substrate::type-of-substrate))
                                 (all-roles (&optional
                                             tbox
                                             &key
                                             count
                                             test
                                             inverse-test
                                             default))
                                 (all-role-assertions-for-individual-in-range (individual
                                                                               &optional
                                                                               abox
                                                                               &key
                                                                               count))
                                 (all-role-assertions-for-individual-in-domain (individual
                                                                                &optional
                                                                                abox
                                                                                &key
                                                                                count))
                                 (all-role-assertions (&optional
                                                       abox
                                                       &key
                                                       count))
                                 (all-queries (&key
                                               thematic-substrate::abox
                                               thematic-substrate::type-of-substrate))
                                 (all-individuals (&optional
                                                   abox
                                                   &key
                                                   count))
                                 (all-features (&optional
                                                tbox
                                                &key
                                                count))
                                 (all-equivalent-concepts (&optional
                                                           tbox
                                                           &key
                                                           count))
                                 (all-different-from-assertions (&optional
                                                                 thematic-substrate::abox
                                                                 &key
                                                                 count))
                                 (all-different (&rest
                                                 individual-name-set))
                                 (all-constraints (&optional
                                                   abox
                                                   object-names
                                                   &key
                                                   count))
                                 (all-concept-assertions-for-individual (individual
                                                                         &optional
                                                                         abox
                                                                         &key
                                                                         count))
                                 (all-concept-assertions (&optional
                                                          abox
                                                          &key
                                                          count))
                                 (all-attributes (&optional
                                                  tbox
                                                  &key
                                                  count))
                                 (all-attribute-assertions (&optional
                                                            abox
                                                            individual
                                                            &key
                                                            count))
                                 (all-atomic-concepts (&optional
                                                       tbox
                                                       &key
                                                       count))
                                 (all-annotation-role-assertions (&optional
                                                                  abox
                                                                  &key
                                                                  count))
                                 (all-annotation-concept-assertions (&optional
                                                                     abox
                                                                     &key
                                                                     count))
                                 (all-aboxes nil)
                                 (alc-concept-coherent (concept-term &key
                                                                            logic))
                                 (add-same-individual-as-assertion (abox
                                                                    individual-name-1
                                                                    individual-name-2))
                                 (add-rule-consequences-automatically nil)
                                 (add-rule-axiom (abox
                                                  lefthand-side
                                                  righthand-side
                                                  &key
                                                  id
                                                  forward-rule-p
                                                  backward-rule-p
                                                  fire-once-p))
                                 (add-role-axioms (tbox role-name
                                                               &key
                                                               cd-attribute
                                                               feature-p
                                                               feature
                                                               transitive-p
                                                               transitive
                                                               parents
                                                               parent
                                                               inverse
                                                               inverse-feature-p
                                                               domain
                                                               range
                                                               symmetric
                                                               symmetric-p
                                                               reflexive
                                                               reflexive-p
                                                               datatype
                                                               annotation-p
                                                               irreflexive
                                                               irreflexive-p
                                                               asymmetric
                                                               asymmetric-p
                                                               compositions))
                                 (add-role-assertions-for-datatype-properties nil)
                                 (add-role-assertion (abox
                                                      predecessor-name
                                                      filler-name
                                                      role-term))
                                 (add-prefix (prefix
                                              mapping))
                                 (add-negative-datatype-role-filler (abox
                                                                     individual
                                                                     value
                                                                     role
                                                                     &optional
                                                                     type))
                                 (add-negated-role-assertion (abox
                                                              predecessor-name
                                                              filler-name
                                                              role-term))
                                 (add-missing-top-conjuncts nil)
                                 (add-explanation-assertions (thematic-substrate::query thematic-substrate::expl-no
                                                                                        &key
                                                                                        thematic-substrate::from
                                                                                        thematic-substrate::to
                                                                                        thematic-substrate::only-best-p
                                                                                        thematic-substrate::order-by
                                                                                        thematic-substrate::reverse-order-p
                                                                                        thematic-substrate::equi-order-by
                                                                                        thematic-substrate::remove-marker-symbols-p
                                                                                        thematic-substrate::remove-entailed-explanations-p
                                                                                        thematic-substrate::new-inds-p
                                                                                        thematic-substrate::tuples-p
                                                                                        thematic-substrate::full-tuples-p
                                                                                        thematic-substrate::all-assertions-p
                                                                                        thematic-substrate::hypothesized-assertions-p
                                                                                        thematic-substrate::show-score-p
                                                                                        thematic-substrate::abox-entailment
                                                                                        thematic-substrate::ensure-permutations-p))
                                 (add-event-rule (head
                                                  body
                                                  &optional
                                                  abox))
                                 (add-event-assertion (assertion
                                                       &optional
                                                       abox))
                                 (add-doc-phrase1 (label string))
                                 (add-doc-phrase (label string))
                                 (add-doc-image-file1 (url
                                                       type
                                                       pathname))
                                 (add-doc-image-file (url
                                                      type
                                                      pathname))
                                 (add-doc-image-data1 (url type bytes))
                                 (add-doc-image-data-from-file1 (url
                                                                 type
                                                                 pathname))
                                 (add-doc-image-data-from-file (url
                                                                type
                                                                pathname))
                                 (add-doc-image-data (url type bytes))
                                 (add-doc-entry1 nil)
                                 (add-doc-entry nil)
                                 (add-disjointness-axiom (tbox concept-name
                                                                      group-name
                                                                      &optional
                                                                      form))
                                 (add-different-from-assertion (abox
                                                                individual-name-1
                                                                individual-name-2))
                                 (add-datatype-role-filler (abox
                                                            individual
                                                            value
                                                            role
                                                            &optional
                                                            type))
                                 (add-datatype-property (tbox name
                                                                     &rest
                                                                     args))
                                 (add-constraint-assertion (abox
                                                            constraint))
                                 (add-concept-axiom (tbox left
                                                                 right
                                                                 &key
                                                                 inclusion-p))
                                 (add-concept-assertion (abox
                                                         individual-name
                                                         concept))
                                 (add-chosen-sets-of-rule-consequences (thematic-substrate::query &key
                                                                                                  thematic-substrate::dont-add-abox-duplicates-p))
                                 (add-attribute-assertion (abox
                                                           individual
                                                           object
                                                           attribute))
                                 (add-annotation-role-assertion (abox
                                                                 predecessor-name
                                                                 filler-name
                                                                 role-term))
                                 (add-annotation-concept-assertion (abox
                                                                    individual-name
                                                                    concept))
                                 (add-all-different-assertion (abox
                                                               individual-name-set))
                                 (active-rules (&key
                                                thematic-substrate::abox
                                                thematic-substrate::type-of-substrate))
                                 (active-queries (&key
                                                  thematic-substrate::abox
                                                  thematic-substrate::type-of-substrate))
                                 (activate-defined-query (thematic-substrate::name thematic-substrate::arity
                                                                                   &key
                                                                                   thematic-substrate::pos
                                                                                   thematic-substrate::tbox))
                                 (accurate-rules (&key
                                                  thematic-substrate::abox
                                                  thematic-substrate::type-of-substrate))
                                 (accurate-queries (&key
                                                    thematic-substrate::abox
                                                    thematic-substrate::type-of-substrate))
                                 (abox-una-consistent? (&optional
                                                        abox-name))
                                 (abox-una-consistent-p (&optional
                                                         abox))
                                 (abox-realized? (&optional
                                                  abox-name))
                                 (abox-realized-p (&optional
                                                   abox))
                                 (abox-prepared? (&optional
                                                  abox-name))
                                 (abox-prepared-p (&optional
                                                   abox))
                                 (abox-entails-abox-p (a
                                                       thematic-substrate::b
                                                       &optional
                                                       thematic-substrate::tbox))
                                 (abox-consistent? (&optional
                                                    abox-name))
                                 (abox-consistent-p (&optional
                                                     abox))
                                 (abox-consistent-if-assertions-added-p (abox
                                                                         assertions))
                                 (abort-rule (thematic-substrate::query))
                                 (abort-query (thematic-substrate::query))
                                 (abort-all-rules (&key
                                                   thematic-substrate::abox
                                                   thematic-substrate::type-of-substrate))
                                 (abort-all-queries (&key
                                                     thematic-substrate::abox
                                                     thematic-substrate::type-of-substrate)))))
(defun get-lambda (fn) (second (assoc fn +lambda-registry+)))
