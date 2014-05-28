package com.racersystems.jracer;

/** 
 *       Automatically Generated Racer Stubs    
 *          Version: 2.0, Build: 2013-03-07 
 *          Date: March 07 2013, 13:05  
 */

abstract public class RacerStubs {

    abstract public boolean returnBoolean(RacerResult answer); 

    abstract public boolean returnBoolean(String answer); 

    abstract public RacerResult racerCall(Object... args) throws RacerClientException; 

    abstract public void pushWith(String withMacro, Object... args); 

    abstract public void popWith(String withMacro);

/** Racer Function abort-all-queries
(abort-all-queries &key
                   abox
                   type-of-substrate)
 */

     public String abortAllQueries( ) throws RacerClientException {
          return racerCall("abort-all-queries"  ).toString();
     }

     public RacerResult abortAllQueries$( ) throws RacerClientException {
          return racerCall("abort-all-queries"  );
     }

     public String abortAllQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("abort-all-queries"  , keyArgs).toString();
     }

     public RacerResult abortAllQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("abort-all-queries"  , keyArgs);
     }

/** Racer Function abort-all-rules
(abort-all-rules &key
                 abox
                 type-of-substrate)
 */

     public String abortAllRules( ) throws RacerClientException {
          return racerCall("abort-all-rules"  ).toString();
     }

     public RacerResult abortAllRules$( ) throws RacerClientException {
          return racerCall("abort-all-rules"  );
     }

     public String abortAllRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("abort-all-rules"  , keyArgs).toString();
     }

     public RacerResult abortAllRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("abort-all-rules"  , keyArgs);
     }

/** Racer Function abort-query
(abort-query query)
 */

     public String abortQuery(Object query ) throws RacerClientException {
          return racerCall("abort-query" , query ).toString();
     }

     public RacerResult abortQuery$(Object query ) throws RacerClientException {
          return racerCall("abort-query" , query );
     }

/** Racer Function abort-rule
(abort-rule query)
 */

     public String abortRule(Object query ) throws RacerClientException {
          return racerCall("abort-rule" , query ).toString();
     }

     public RacerResult abortRule$(Object query ) throws RacerClientException {
          return racerCall("abort-rule" , query );
     }

/** Racer Function abox-consistent-if-assertions-added-p
(abox-consistent-if-assertions-added-p abox assertions)
 */

     public boolean aboxConsistentIfAssertionsAddedP(Object abox, Object assertions ) throws RacerClientException {
          return returnBoolean(racerCall("abox-consistent-if-assertions-added-p" , abox, assertions ));
     }

/** Racer Function abox-consistent-p
(abox-consistent-p &optional abox)
 */

     public boolean aboxConsistentP( ) throws RacerClientException {
          return returnBoolean(racerCall("abox-consistent-p"  ));
     }

     public boolean aboxConsistentP(Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("abox-consistent-p" , abox ));
     }

/** Racer Function abox-entails-abox-p
(abox-entails-abox-p a
                     b
                     &optional
                     tbox)
 */

     public boolean aboxEntailsAboxP(Object a, Object b ) throws RacerClientException {
          return returnBoolean(racerCall("abox-entails-abox-p" , a, b ));
     }

     public boolean aboxEntailsAboxP(Object a, Object b, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("abox-entails-abox-p" , a, b, tbox ));
     }

/** Racer Function abox-prepared-p
(abox-prepared-p &optional abox)
 */

     public boolean aboxPreparedP( ) throws RacerClientException {
          return returnBoolean(racerCall("abox-prepared-p"  ));
     }

     public boolean aboxPreparedP(Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("abox-prepared-p" , abox ));
     }

/** Racer Function abox-realized-p
(abox-realized-p &optional abox)
 */

     public boolean aboxRealizedP( ) throws RacerClientException {
          return returnBoolean(racerCall("abox-realized-p"  ));
     }

     public boolean aboxRealizedP(Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("abox-realized-p" , abox ));
     }

/** Racer Function abox-una-consistent-p
(abox-una-consistent-p &optional abox)
 */

     public boolean aboxUnaConsistentP( ) throws RacerClientException {
          return returnBoolean(racerCall("abox-una-consistent-p"  ));
     }

     public boolean aboxUnaConsistentP(Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("abox-una-consistent-p" , abox ));
     }

/** Racer Function accurate-queries
(accurate-queries &key
                  abox
                  type-of-substrate)
 */

     public String accurateQueries( ) throws RacerClientException {
          return racerCall("accurate-queries"  ).toString();
     }

     public RacerResult accurateQueries$( ) throws RacerClientException {
          return racerCall("accurate-queries"  );
     }

     public String accurateQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("accurate-queries"  , keyArgs).toString();
     }

     public RacerResult accurateQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("accurate-queries"  , keyArgs);
     }

/** Racer Function accurate-rules
(accurate-rules &key
                abox
                type-of-substrate)
 */

     public String accurateRules( ) throws RacerClientException {
          return racerCall("accurate-rules"  ).toString();
     }

     public RacerResult accurateRules$( ) throws RacerClientException {
          return racerCall("accurate-rules"  );
     }

     public String accurateRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("accurate-rules"  , keyArgs).toString();
     }

     public RacerResult accurateRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("accurate-rules"  , keyArgs);
     }

/** Racer Function activate-defined-query
(activate-defined-query name
                        arity
                        &key
                        pos
                        tbox)
 */

     public String activateDefinedQuery(Object name, Object arity ) throws RacerClientException {
          return racerCall("activate-defined-query" , name, arity ).toString();
     }

     public RacerResult activateDefinedQuery$(Object name, Object arity ) throws RacerClientException {
          return racerCall("activate-defined-query" , name, arity );
     }

     public String activateDefinedQuery(Object name, Object arity , Object... keyArgs) throws RacerClientException {
          return racerCall("activate-defined-query" , name, arity , keyArgs).toString();
     }

     public RacerResult activateDefinedQuery$(Object name, Object arity , Object... keyArgs) throws RacerClientException {
          return racerCall("activate-defined-query" , name, arity , keyArgs);
     }

/** Racer Function active-queries
(active-queries &key
                abox
                type-of-substrate)
 */

     public String activeQueries( ) throws RacerClientException {
          return racerCall("active-queries"  ).toString();
     }

     public RacerResult activeQueries$( ) throws RacerClientException {
          return racerCall("active-queries"  );
     }

     public String activeQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("active-queries"  , keyArgs).toString();
     }

     public RacerResult activeQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("active-queries"  , keyArgs);
     }

/** Racer Function active-rules
(active-rules &key
              abox
              type-of-substrate)
 */

     public String activeRules( ) throws RacerClientException {
          return racerCall("active-rules"  ).toString();
     }

     public RacerResult activeRules$( ) throws RacerClientException {
          return racerCall("active-rules"  );
     }

     public String activeRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("active-rules"  , keyArgs).toString();
     }

     public RacerResult activeRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("active-rules"  , keyArgs);
     }

/** Racer Function add-all-different-assertion
(add-all-different-assertion abox individual-name-set)
 */

     public String addAllDifferentAssertion(Object abox, Object individualNameSet ) throws RacerClientException {
          return racerCall("add-all-different-assertion" , abox, individualNameSet ).toString();
     }

     public RacerResult addAllDifferentAssertion$(Object abox, Object individualNameSet ) throws RacerClientException {
          return racerCall("add-all-different-assertion" , abox, individualNameSet );
     }

/** Racer Function add-annotation-concept-assertion
(add-annotation-concept-assertion abox
                                  individual-name
                                  concept)
 */

     public String addAnnotationConceptAssertion(Object abox, Object individualName, Object concept ) throws RacerClientException {
          return racerCall("add-annotation-concept-assertion" , abox, individualName, concept ).toString();
     }

     public RacerResult addAnnotationConceptAssertion$(Object abox, Object individualName, Object concept ) throws RacerClientException {
          return racerCall("add-annotation-concept-assertion" , abox, individualName, concept );
     }

/** Racer Function add-annotation-role-assertion
(add-annotation-role-assertion abox
                               predecessor-name
                               filler-name
                               role-term)
 */

     public String addAnnotationRoleAssertion(Object abox, Object predecessorName, Object fillerName, Object roleTerm ) throws RacerClientException {
          return racerCall("add-annotation-role-assertion" , abox, predecessorName, fillerName, roleTerm ).toString();
     }

     public RacerResult addAnnotationRoleAssertion$(Object abox, Object predecessorName, Object fillerName, Object roleTerm ) throws RacerClientException {
          return racerCall("add-annotation-role-assertion" , abox, predecessorName, fillerName, roleTerm );
     }

/** Racer Function add-attribute-assertion
(add-attribute-assertion abox
                         individual
                         object
                         attribute)
 */

     public String addAttributeAssertion(Object abox, Object individual, Object object, Object attribute ) throws RacerClientException {
          return racerCall("add-attribute-assertion" , abox, individual, object, attribute ).toString();
     }

     public RacerResult addAttributeAssertion$(Object abox, Object individual, Object object, Object attribute ) throws RacerClientException {
          return racerCall("add-attribute-assertion" , abox, individual, object, attribute );
     }

/** Racer Function add-chosen-sets-of-rule-consequences
(add-chosen-sets-of-rule-consequences query
                                      &key
                                      dont-add-abox-duplicates-p)
 */

     public String addChosenSetsOfRuleConsequences(Object query ) throws RacerClientException {
          return racerCall("add-chosen-sets-of-rule-consequences" , query ).toString();
     }

     public RacerResult addChosenSetsOfRuleConsequences$(Object query ) throws RacerClientException {
          return racerCall("add-chosen-sets-of-rule-consequences" , query );
     }

     public String addChosenSetsOfRuleConsequences(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("add-chosen-sets-of-rule-consequences" , query , keyArgs).toString();
     }

     public RacerResult addChosenSetsOfRuleConsequences$(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("add-chosen-sets-of-rule-consequences" , query , keyArgs);
     }

/** Racer Function add-concept-assertion
(add-concept-assertion abox
                       individual-name
                       concept)
 */

     public String addConceptAssertion(Object abox, Object individualName, Object concept ) throws RacerClientException {
          return racerCall("add-concept-assertion" , abox, individualName, concept ).toString();
     }

     public RacerResult addConceptAssertion$(Object abox, Object individualName, Object concept ) throws RacerClientException {
          return racerCall("add-concept-assertion" , abox, individualName, concept );
     }

/** Racer Function add-concept-axiom
(add-concept-axiom tbox
                   left
                   right
                   &key
                   inclusion-p)
 */

     public String addConceptAxiom(Object tbox, Object left, Object right ) throws RacerClientException {
          return racerCall("add-concept-axiom" , tbox, left, right ).toString();
     }

     public RacerResult addConceptAxiom$(Object tbox, Object left, Object right ) throws RacerClientException {
          return racerCall("add-concept-axiom" , tbox, left, right );
     }

     public String addConceptAxiom(Object tbox, Object left, Object right , Object... keyArgs) throws RacerClientException {
          return racerCall("add-concept-axiom" , tbox, left, right , keyArgs).toString();
     }

     public RacerResult addConceptAxiom$(Object tbox, Object left, Object right , Object... keyArgs) throws RacerClientException {
          return racerCall("add-concept-axiom" , tbox, left, right , keyArgs);
     }

/** Racer Function add-constraint-assertion
(add-constraint-assertion abox constraint)
 */

     public String addConstraintAssertion(Object abox, Object constraint ) throws RacerClientException {
          return racerCall("add-constraint-assertion" , abox, constraint ).toString();
     }

     public RacerResult addConstraintAssertion$(Object abox, Object constraint ) throws RacerClientException {
          return racerCall("add-constraint-assertion" , abox, constraint );
     }

/** Racer Function add-datatype-property
(add-datatype-property tbox name &rest (args))
 */

     public String addDatatypeProperty(Object tbox, Object name ) throws RacerClientException {
          return racerCall("add-datatype-property" , tbox, name ).toString();
     }

     public RacerResult addDatatypeProperty$(Object tbox, Object name ) throws RacerClientException {
          return racerCall("add-datatype-property" , tbox, name );
     }

     public String addDatatypeProperty(Object tbox, Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("add-datatype-property" , tbox, name , keyArgs).toString();
     }

     public RacerResult addDatatypeProperty$(Object tbox, Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("add-datatype-property" , tbox, name , keyArgs);
     }

/** Racer Function add-datatype-role-filler
(add-datatype-role-filler abox
                          individual
                          value
                          role
                          &optional
                          type)
 */

     public String addDatatypeRoleFiller(Object abox, Object individual, Object value, Object role ) throws RacerClientException {
          return racerCall("add-datatype-role-filler" , abox, individual, value, role ).toString();
     }

     public RacerResult addDatatypeRoleFiller$(Object abox, Object individual, Object value, Object role ) throws RacerClientException {
          return racerCall("add-datatype-role-filler" , abox, individual, value, role );
     }

     public String addDatatypeRoleFiller(Object abox, Object individual, Object value, Object role, Object type ) throws RacerClientException {
          return racerCall("add-datatype-role-filler" , abox, individual, value, role, type ).toString();
     }

     public RacerResult addDatatypeRoleFiller$(Object abox, Object individual, Object value, Object role, Object type ) throws RacerClientException {
          return racerCall("add-datatype-role-filler" , abox, individual, value, role, type );
     }

/** Racer Function add-different-from-assertion
(add-different-from-assertion abox
                              individual-name-1
                              individual-name-2)
 */

     public String addDifferentFromAssertion(Object abox, Object individualName1, Object individualName2 ) throws RacerClientException {
          return racerCall("add-different-from-assertion" , abox, individualName1, individualName2 ).toString();
     }

     public RacerResult addDifferentFromAssertion$(Object abox, Object individualName1, Object individualName2 ) throws RacerClientException {
          return racerCall("add-different-from-assertion" , abox, individualName1, individualName2 );
     }

/** Racer Function add-disjointness-axiom
(add-disjointness-axiom tbox
                        concept-name
                        group-name
                        &optional
                        form)
 */

     public String addDisjointnessAxiom(Object tbox, Object conceptName, Object groupName ) throws RacerClientException {
          return racerCall("add-disjointness-axiom" , tbox, conceptName, groupName ).toString();
     }

     public RacerResult addDisjointnessAxiom$(Object tbox, Object conceptName, Object groupName ) throws RacerClientException {
          return racerCall("add-disjointness-axiom" , tbox, conceptName, groupName );
     }

     public String addDisjointnessAxiom(Object tbox, Object conceptName, Object groupName, Object form ) throws RacerClientException {
          return racerCall("add-disjointness-axiom" , tbox, conceptName, groupName, form ).toString();
     }

     public RacerResult addDisjointnessAxiom$(Object tbox, Object conceptName, Object groupName, Object form ) throws RacerClientException {
          return racerCall("add-disjointness-axiom" , tbox, conceptName, groupName, form );
     }

/** Racer Function add-doc-entry1
(add-doc-entry1)
 */

     public String addDocEntry1( ) throws RacerClientException {
          return racerCall("add-doc-entry1"  ).toString();
     }

     public RacerResult addDocEntry1$( ) throws RacerClientException {
          return racerCall("add-doc-entry1"  );
     }

/** Racer Function add-doc-image-data-from-file1
(add-doc-image-data-from-file1 url type pathname)
 */

     public String addDocImageDataFromFile1(Object url, Object type, Object pathname ) throws RacerClientException {
          return racerCall("add-doc-image-data-from-file1" , url, type, pathname ).toString();
     }

     public RacerResult addDocImageDataFromFile1$(Object url, Object type, Object pathname ) throws RacerClientException {
          return racerCall("add-doc-image-data-from-file1" , url, type, pathname );
     }

/** Racer Function add-doc-image-data1
(add-doc-image-data1 url type bytes)
 */

     public String addDocImageData1(Object url, Object type, Object bytes ) throws RacerClientException {
          return racerCall("add-doc-image-data1" , url, type, bytes ).toString();
     }

     public RacerResult addDocImageData1$(Object url, Object type, Object bytes ) throws RacerClientException {
          return racerCall("add-doc-image-data1" , url, type, bytes );
     }

/** Racer Function add-doc-image-file1
(add-doc-image-file1 url type pathname)
 */

     public String addDocImageFile1(Object url, Object type, Object pathname ) throws RacerClientException {
          return racerCall("add-doc-image-file1" , url, type, pathname ).toString();
     }

     public RacerResult addDocImageFile1$(Object url, Object type, Object pathname ) throws RacerClientException {
          return racerCall("add-doc-image-file1" , url, type, pathname );
     }

/** Racer Function add-doc-phrase1
(add-doc-phrase1 label string)
 */

     public String addDocPhrase1(Object label, Object string ) throws RacerClientException {
          return racerCall("add-doc-phrase1" , label, string ).toString();
     }

     public RacerResult addDocPhrase1$(Object label, Object string ) throws RacerClientException {
          return racerCall("add-doc-phrase1" , label, string );
     }

/** Racer Function add-event-assertion
(add-event-assertion assertion &optional abox)
 */

     public String addEventAssertion(Object assertion ) throws RacerClientException {
          return racerCall("add-event-assertion" , assertion ).toString();
     }

     public RacerResult addEventAssertion$(Object assertion ) throws RacerClientException {
          return racerCall("add-event-assertion" , assertion );
     }

     public String addEventAssertion(Object assertion, Object abox ) throws RacerClientException {
          return racerCall("add-event-assertion" , assertion, abox ).toString();
     }

     public RacerResult addEventAssertion$(Object assertion, Object abox ) throws RacerClientException {
          return racerCall("add-event-assertion" , assertion, abox );
     }

/** Racer Function add-event-rule
(add-event-rule head body &optional abox)
 */

     public String addEventRule(Object head, Object body ) throws RacerClientException {
          return racerCall("add-event-rule" , head, body ).toString();
     }

     public RacerResult addEventRule$(Object head, Object body ) throws RacerClientException {
          return racerCall("add-event-rule" , head, body );
     }

     public String addEventRule(Object head, Object body, Object abox ) throws RacerClientException {
          return racerCall("add-event-rule" , head, body, abox ).toString();
     }

     public RacerResult addEventRule$(Object head, Object body, Object abox ) throws RacerClientException {
          return racerCall("add-event-rule" , head, body, abox );
     }

/** Racer Function add-explanation-assertions
(add-explanation-assertions query
                            expl-no
                            &key
                            from
                            to
                            only-best-p
                            order-by
                            reverse-order-p
                            equi-order-by
                            remove-marker-symbols-p
                            remove-entailed-explanations-p
                            new-inds-p
                            tuples-p
                            full-tuples-p
                            all-assertions-p
                            hypothesized-assertions-p
                            show-score-p
                            abox-entailment
                            ensure-permutations-p)
 */

     public String addExplanationAssertions(Object query, Object explNo ) throws RacerClientException {
          return racerCall("add-explanation-assertions" , query, explNo ).toString();
     }

     public RacerResult addExplanationAssertions$(Object query, Object explNo ) throws RacerClientException {
          return racerCall("add-explanation-assertions" , query, explNo );
     }

     public String addExplanationAssertions(Object query, Object explNo , Object... keyArgs) throws RacerClientException {
          return racerCall("add-explanation-assertions" , query, explNo , keyArgs).toString();
     }

     public RacerResult addExplanationAssertions$(Object query, Object explNo , Object... keyArgs) throws RacerClientException {
          return racerCall("add-explanation-assertions" , query, explNo , keyArgs);
     }

/** Racer Function add-missing-top-conjuncts
(add-missing-top-conjuncts)
 */

     public String addMissingTopConjuncts( ) throws RacerClientException {
          return racerCall("add-missing-top-conjuncts"  ).toString();
     }

     public RacerResult addMissingTopConjuncts$( ) throws RacerClientException {
          return racerCall("add-missing-top-conjuncts"  );
     }

/** Racer Function add-negated-role-assertion
(add-negated-role-assertion abox
                            predecessor-name
                            filler-name
                            role-term)
 */

     public String addNegatedRoleAssertion(Object abox, Object predecessorName, Object fillerName, Object roleTerm ) throws RacerClientException {
          return racerCall("add-negated-role-assertion" , abox, predecessorName, fillerName, roleTerm ).toString();
     }

     public RacerResult addNegatedRoleAssertion$(Object abox, Object predecessorName, Object fillerName, Object roleTerm ) throws RacerClientException {
          return racerCall("add-negated-role-assertion" , abox, predecessorName, fillerName, roleTerm );
     }

/** Racer Function add-negative-datatype-role-filler
(add-negative-datatype-role-filler abox
                                   individual
                                   value
                                   role
                                   &optional
                                   type)
 */

     public String addNegativeDatatypeRoleFiller(Object abox, Object individual, Object value, Object role ) throws RacerClientException {
          return racerCall("add-negative-datatype-role-filler" , abox, individual, value, role ).toString();
     }

     public RacerResult addNegativeDatatypeRoleFiller$(Object abox, Object individual, Object value, Object role ) throws RacerClientException {
          return racerCall("add-negative-datatype-role-filler" , abox, individual, value, role );
     }

     public String addNegativeDatatypeRoleFiller(Object abox, Object individual, Object value, Object role, Object type ) throws RacerClientException {
          return racerCall("add-negative-datatype-role-filler" , abox, individual, value, role, type ).toString();
     }

     public RacerResult addNegativeDatatypeRoleFiller$(Object abox, Object individual, Object value, Object role, Object type ) throws RacerClientException {
          return racerCall("add-negative-datatype-role-filler" , abox, individual, value, role, type );
     }

/** Racer Function add-prefix
(add-prefix prefix mapping)
 */

     public String addPrefix(Object prefix, Object mapping ) throws RacerClientException {
          return racerCall("add-prefix" , prefix, mapping ).toString();
     }

     public RacerResult addPrefix$(Object prefix, Object mapping ) throws RacerClientException {
          return racerCall("add-prefix" , prefix, mapping );
     }

/** Racer Function add-role-assertion
(add-role-assertion abox
                    predecessor-name
                    filler-name
                    role-term)
 */

     public String addRoleAssertion(Object abox, Object predecessorName, Object fillerName, Object roleTerm ) throws RacerClientException {
          return racerCall("add-role-assertion" , abox, predecessorName, fillerName, roleTerm ).toString();
     }

     public RacerResult addRoleAssertion$(Object abox, Object predecessorName, Object fillerName, Object roleTerm ) throws RacerClientException {
          return racerCall("add-role-assertion" , abox, predecessorName, fillerName, roleTerm );
     }

/** Racer Function add-role-assertions-for-datatype-properties
(add-role-assertions-for-datatype-properties)
 */

     public String addRoleAssertionsForDatatypeProperties( ) throws RacerClientException {
          return racerCall("add-role-assertions-for-datatype-properties"  ).toString();
     }

     public RacerResult addRoleAssertionsForDatatypeProperties$( ) throws RacerClientException {
          return racerCall("add-role-assertions-for-datatype-properties"  );
     }

/** Racer Function add-role-axiom
(add-role-axiom
 tbox
 role-name
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
 compositions)
 */

     public String addRoleAxiom(Object tbox, Object roleName ) throws RacerClientException {
          return racerCall("add-role-axiom" , tbox, roleName ).toString();
     }

     public RacerResult addRoleAxiom$(Object tbox, Object roleName ) throws RacerClientException {
          return racerCall("add-role-axiom" , tbox, roleName );
     }

     public String addRoleAxiom(Object tbox, Object roleName , Object... keyArgs) throws RacerClientException {
          return racerCall("add-role-axiom" , tbox, roleName , keyArgs).toString();
     }

     public RacerResult addRoleAxiom$(Object tbox, Object roleName , Object... keyArgs) throws RacerClientException {
          return racerCall("add-role-axiom" , tbox, roleName , keyArgs);
     }

/** Racer Function add-role-axioms
(add-role-axioms tbox
                 role-name
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
                 compositions)
 */

     public String addRoleAxioms(Object tbox, Object roleName ) throws RacerClientException {
          return racerCall("add-role-axioms" , tbox, roleName ).toString();
     }

     public RacerResult addRoleAxioms$(Object tbox, Object roleName ) throws RacerClientException {
          return racerCall("add-role-axioms" , tbox, roleName );
     }

     public String addRoleAxioms(Object tbox, Object roleName , Object... keyArgs) throws RacerClientException {
          return racerCall("add-role-axioms" , tbox, roleName , keyArgs).toString();
     }

     public RacerResult addRoleAxioms$(Object tbox, Object roleName , Object... keyArgs) throws RacerClientException {
          return racerCall("add-role-axioms" , tbox, roleName , keyArgs);
     }

/** Racer Function add-rule-axiom
(add-rule-axiom abox
                lefthand-side
                righthand-side
                &key
                id
                forward-rule-p
                backward-rule-p
                fire-once-p)
 */

     public String addRuleAxiom(Object abox, Object lefthandSide, Object righthandSide ) throws RacerClientException {
          return racerCall("add-rule-axiom" , abox, lefthandSide, righthandSide ).toString();
     }

     public RacerResult addRuleAxiom$(Object abox, Object lefthandSide, Object righthandSide ) throws RacerClientException {
          return racerCall("add-rule-axiom" , abox, lefthandSide, righthandSide );
     }

     public String addRuleAxiom(Object abox, Object lefthandSide, Object righthandSide , Object... keyArgs) throws RacerClientException {
          return racerCall("add-rule-axiom" , abox, lefthandSide, righthandSide , keyArgs).toString();
     }

     public RacerResult addRuleAxiom$(Object abox, Object lefthandSide, Object righthandSide , Object... keyArgs) throws RacerClientException {
          return racerCall("add-rule-axiom" , abox, lefthandSide, righthandSide , keyArgs);
     }

/** Racer Function add-rule-consequences-automatically
(add-rule-consequences-automatically)
 */

     public String addRuleConsequencesAutomatically( ) throws RacerClientException {
          return racerCall("add-rule-consequences-automatically"  ).toString();
     }

     public RacerResult addRuleConsequencesAutomatically$( ) throws RacerClientException {
          return racerCall("add-rule-consequences-automatically"  );
     }

/** Racer Function add-same-individual-as-assertion
(add-same-individual-as-assertion abox
                                  individual-name-1
                                  individual-name-2)
 */

     public String addSameIndividualAsAssertion(Object abox, Object individualName1, Object individualName2 ) throws RacerClientException {
          return racerCall("add-same-individual-as-assertion" , abox, individualName1, individualName2 ).toString();
     }

     public RacerResult addSameIndividualAsAssertion$(Object abox, Object individualName1, Object individualName2 ) throws RacerClientException {
          return racerCall("add-same-individual-as-assertion" , abox, individualName1, individualName2 );
     }

/** Racer Function alc-concept-coherent
(alc-concept-coherent concept-term &key logic)
 */

     public String alcConceptCoherent(Object conceptTerm ) throws RacerClientException {
          return racerCall("alc-concept-coherent" , conceptTerm ).toString();
     }

     public RacerResult alcConceptCoherent$(Object conceptTerm ) throws RacerClientException {
          return racerCall("alc-concept-coherent" , conceptTerm );
     }

     public String alcConceptCoherent(Object conceptTerm , Object... keyArgs) throws RacerClientException {
          return racerCall("alc-concept-coherent" , conceptTerm , keyArgs).toString();
     }

     public RacerResult alcConceptCoherent$(Object conceptTerm , Object... keyArgs) throws RacerClientException {
          return racerCall("alc-concept-coherent" , conceptTerm , keyArgs);
     }

/** Racer Function all-aboxes
(all-aboxes)
 */

     public String allAboxes( ) throws RacerClientException {
          return racerCall("all-aboxes"  ).toString();
     }

     public RacerResult allAboxes$( ) throws RacerClientException {
          return racerCall("all-aboxes"  );
     }

/** Racer Function all-annotation-concept-assertions
(all-annotation-concept-assertions &optional abox &key count)
 */

     public String allAnnotationConceptAssertions( ) throws RacerClientException {
          return racerCall("all-annotation-concept-assertions"  ).toString();
     }

     public RacerResult allAnnotationConceptAssertions$( ) throws RacerClientException {
          return racerCall("all-annotation-concept-assertions"  );
     }

     public String allAnnotationConceptAssertions(Object abox ) throws RacerClientException {
          return racerCall("all-annotation-concept-assertions" , abox ).toString();
     }

     public RacerResult allAnnotationConceptAssertions$(Object abox ) throws RacerClientException {
          return racerCall("all-annotation-concept-assertions" , abox );
     }

     public String allAnnotationConceptAssertions(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-annotation-concept-assertions" , abox , keyArgs).toString();
     }

     public RacerResult allAnnotationConceptAssertions$(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-annotation-concept-assertions" , abox , keyArgs);
     }

/** Racer Function all-annotation-role-assertions
(all-annotation-role-assertions &optional abox &key count)
 */

     public String allAnnotationRoleAssertions( ) throws RacerClientException {
          return racerCall("all-annotation-role-assertions"  ).toString();
     }

     public RacerResult allAnnotationRoleAssertions$( ) throws RacerClientException {
          return racerCall("all-annotation-role-assertions"  );
     }

     public String allAnnotationRoleAssertions(Object abox ) throws RacerClientException {
          return racerCall("all-annotation-role-assertions" , abox ).toString();
     }

     public RacerResult allAnnotationRoleAssertions$(Object abox ) throws RacerClientException {
          return racerCall("all-annotation-role-assertions" , abox );
     }

     public String allAnnotationRoleAssertions(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-annotation-role-assertions" , abox , keyArgs).toString();
     }

     public RacerResult allAnnotationRoleAssertions$(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-annotation-role-assertions" , abox , keyArgs);
     }

/** Racer Function all-atomic-concepts
(all-atomic-concepts &optional tbox &key count)
 */

     public String allAtomicConcepts( ) throws RacerClientException {
          return racerCall("all-atomic-concepts"  ).toString();
     }

     public RacerResult allAtomicConcepts$( ) throws RacerClientException {
          return racerCall("all-atomic-concepts"  );
     }

     public String allAtomicConcepts(Object tbox ) throws RacerClientException {
          return racerCall("all-atomic-concepts" , tbox ).toString();
     }

     public RacerResult allAtomicConcepts$(Object tbox ) throws RacerClientException {
          return racerCall("all-atomic-concepts" , tbox );
     }

     public String allAtomicConcepts(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-atomic-concepts" , tbox , keyArgs).toString();
     }

     public RacerResult allAtomicConcepts$(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-atomic-concepts" , tbox , keyArgs);
     }

/** Racer Function all-attribute-assertions
(all-attribute-assertions &optional
                          abox
                          individual
                          &key
                          count)
 */

     public String allAttributeAssertions( ) throws RacerClientException {
          return racerCall("all-attribute-assertions"  ).toString();
     }

     public RacerResult allAttributeAssertions$( ) throws RacerClientException {
          return racerCall("all-attribute-assertions"  );
     }

     public String allAttributeAssertions(Object abox, Object individual ) throws RacerClientException {
          return racerCall("all-attribute-assertions" , abox, individual ).toString();
     }

     public RacerResult allAttributeAssertions$(Object abox, Object individual ) throws RacerClientException {
          return racerCall("all-attribute-assertions" , abox, individual );
     }

     public String allAttributeAssertions(Object abox ) throws RacerClientException {
          return racerCall("all-attribute-assertions" , abox ).toString();
     }

     public RacerResult allAttributeAssertions$(Object abox ) throws RacerClientException {
          return racerCall("all-attribute-assertions" , abox );
     }

     public String allAttributeAssertions(Object abox, Object individual , Object... keyArgs) throws RacerClientException {
          return racerCall("all-attribute-assertions" , abox, individual , keyArgs).toString();
     }

     public RacerResult allAttributeAssertions$(Object abox, Object individual , Object... keyArgs) throws RacerClientException {
          return racerCall("all-attribute-assertions" , abox, individual , keyArgs);
     }

/** Racer Function all-attributes
(all-attributes &optional tbox &key count)
 */

     public String allAttributes( ) throws RacerClientException {
          return racerCall("all-attributes"  ).toString();
     }

     public RacerResult allAttributes$( ) throws RacerClientException {
          return racerCall("all-attributes"  );
     }

     public String allAttributes(Object tbox ) throws RacerClientException {
          return racerCall("all-attributes" , tbox ).toString();
     }

     public RacerResult allAttributes$(Object tbox ) throws RacerClientException {
          return racerCall("all-attributes" , tbox );
     }

     public String allAttributes(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-attributes" , tbox , keyArgs).toString();
     }

     public RacerResult allAttributes$(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-attributes" , tbox , keyArgs);
     }

/** Racer Function all-concept-assertions
(all-concept-assertions &optional abox &key count)
 */

     public String allConceptAssertions( ) throws RacerClientException {
          return racerCall("all-concept-assertions"  ).toString();
     }

     public RacerResult allConceptAssertions$( ) throws RacerClientException {
          return racerCall("all-concept-assertions"  );
     }

     public String allConceptAssertions(Object abox ) throws RacerClientException {
          return racerCall("all-concept-assertions" , abox ).toString();
     }

     public RacerResult allConceptAssertions$(Object abox ) throws RacerClientException {
          return racerCall("all-concept-assertions" , abox );
     }

     public String allConceptAssertions(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-concept-assertions" , abox , keyArgs).toString();
     }

     public RacerResult allConceptAssertions$(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-concept-assertions" , abox , keyArgs);
     }

/** Racer Function all-concept-assertions-for-individual
(all-concept-assertions-for-individual individual
                                       &optional
                                       abox
                                       &key
                                       count)
 */

     public String allConceptAssertionsForIndividual(Object individual ) throws RacerClientException {
          return racerCall("all-concept-assertions-for-individual" , individual ).toString();
     }

     public RacerResult allConceptAssertionsForIndividual$(Object individual ) throws RacerClientException {
          return racerCall("all-concept-assertions-for-individual" , individual );
     }

     public String allConceptAssertionsForIndividual(Object individual, Object abox ) throws RacerClientException {
          return racerCall("all-concept-assertions-for-individual" , individual, abox ).toString();
     }

     public RacerResult allConceptAssertionsForIndividual$(Object individual, Object abox ) throws RacerClientException {
          return racerCall("all-concept-assertions-for-individual" , individual, abox );
     }

     public String allConceptAssertionsForIndividual(Object individual, Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-concept-assertions-for-individual" , individual, abox , keyArgs).toString();
     }

     public RacerResult allConceptAssertionsForIndividual$(Object individual, Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-concept-assertions-for-individual" , individual, abox , keyArgs);
     }

/** Racer Function all-constraints
(all-constraints &optional abox object-names &key count)
 */

     public String allConstraints( ) throws RacerClientException {
          return racerCall("all-constraints"  ).toString();
     }

     public RacerResult allConstraints$( ) throws RacerClientException {
          return racerCall("all-constraints"  );
     }

     public String allConstraints(Object abox, Object objectNames ) throws RacerClientException {
          return racerCall("all-constraints" , abox, objectNames ).toString();
     }

     public RacerResult allConstraints$(Object abox, Object objectNames ) throws RacerClientException {
          return racerCall("all-constraints" , abox, objectNames );
     }

     public String allConstraints(Object abox ) throws RacerClientException {
          return racerCall("all-constraints" , abox ).toString();
     }

     public RacerResult allConstraints$(Object abox ) throws RacerClientException {
          return racerCall("all-constraints" , abox );
     }

     public String allConstraints(Object abox, Object objectNames , Object... keyArgs) throws RacerClientException {
          return racerCall("all-constraints" , abox, objectNames , keyArgs).toString();
     }

     public RacerResult allConstraints$(Object abox, Object objectNames , Object... keyArgs) throws RacerClientException {
          return racerCall("all-constraints" , abox, objectNames , keyArgs);
     }

/** Racer Function all-different-from-assertions
(all-different-from-assertions &optional
                               abox
                               &key
                               count)
 */

     public String allDifferentFromAssertions( ) throws RacerClientException {
          return racerCall("all-different-from-assertions"  ).toString();
     }

     public RacerResult allDifferentFromAssertions$( ) throws RacerClientException {
          return racerCall("all-different-from-assertions"  );
     }

     public String allDifferentFromAssertions(Object abox ) throws RacerClientException {
          return racerCall("all-different-from-assertions" , abox ).toString();
     }

     public RacerResult allDifferentFromAssertions$(Object abox ) throws RacerClientException {
          return racerCall("all-different-from-assertions" , abox );
     }

     public String allDifferentFromAssertions(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-different-from-assertions" , abox , keyArgs).toString();
     }

     public RacerResult allDifferentFromAssertions$(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-different-from-assertions" , abox , keyArgs);
     }

/** Racer Function all-equivalent-concepts
(all-equivalent-concepts &optional tbox &key count)
 */

     public String allEquivalentConcepts( ) throws RacerClientException {
          return racerCall("all-equivalent-concepts"  ).toString();
     }

     public RacerResult allEquivalentConcepts$( ) throws RacerClientException {
          return racerCall("all-equivalent-concepts"  );
     }

     public String allEquivalentConcepts(Object tbox ) throws RacerClientException {
          return racerCall("all-equivalent-concepts" , tbox ).toString();
     }

     public RacerResult allEquivalentConcepts$(Object tbox ) throws RacerClientException {
          return racerCall("all-equivalent-concepts" , tbox );
     }

     public String allEquivalentConcepts(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-equivalent-concepts" , tbox , keyArgs).toString();
     }

     public RacerResult allEquivalentConcepts$(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-equivalent-concepts" , tbox , keyArgs);
     }

/** Racer Function all-features
(all-features &optional tbox &key count)
 */

     public String allFeatures( ) throws RacerClientException {
          return racerCall("all-features"  ).toString();
     }

     public RacerResult allFeatures$( ) throws RacerClientException {
          return racerCall("all-features"  );
     }

     public String allFeatures(Object tbox ) throws RacerClientException {
          return racerCall("all-features" , tbox ).toString();
     }

     public RacerResult allFeatures$(Object tbox ) throws RacerClientException {
          return racerCall("all-features" , tbox );
     }

     public String allFeatures(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-features" , tbox , keyArgs).toString();
     }

     public RacerResult allFeatures$(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-features" , tbox , keyArgs);
     }

/** Racer Function all-individuals
(all-individuals &optional abox &key count)
 */

     public String allIndividuals( ) throws RacerClientException {
          return racerCall("all-individuals"  ).toString();
     }

     public RacerResult allIndividuals$( ) throws RacerClientException {
          return racerCall("all-individuals"  );
     }

     public String allIndividuals(Object abox ) throws RacerClientException {
          return racerCall("all-individuals" , abox ).toString();
     }

     public RacerResult allIndividuals$(Object abox ) throws RacerClientException {
          return racerCall("all-individuals" , abox );
     }

     public String allIndividuals(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-individuals" , abox , keyArgs).toString();
     }

     public RacerResult allIndividuals$(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-individuals" , abox , keyArgs);
     }

/** Racer Function all-queries
(all-queries &key
             abox
             type-of-substrate)
 */

     public String allQueries( ) throws RacerClientException {
          return racerCall("all-queries"  ).toString();
     }

     public RacerResult allQueries$( ) throws RacerClientException {
          return racerCall("all-queries"  );
     }

     public String allQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("all-queries"  , keyArgs).toString();
     }

     public RacerResult allQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("all-queries"  , keyArgs);
     }

/** Racer Function all-role-assertions
(all-role-assertions &optional abox &key count)
 */

     public String allRoleAssertions( ) throws RacerClientException {
          return racerCall("all-role-assertions"  ).toString();
     }

     public RacerResult allRoleAssertions$( ) throws RacerClientException {
          return racerCall("all-role-assertions"  );
     }

     public String allRoleAssertions(Object abox ) throws RacerClientException {
          return racerCall("all-role-assertions" , abox ).toString();
     }

     public RacerResult allRoleAssertions$(Object abox ) throws RacerClientException {
          return racerCall("all-role-assertions" , abox );
     }

     public String allRoleAssertions(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-role-assertions" , abox , keyArgs).toString();
     }

     public RacerResult allRoleAssertions$(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-role-assertions" , abox , keyArgs);
     }

/** Racer Function all-role-assertions-for-individual-in-domain
(all-role-assertions-for-individual-in-domain individual
                                              &optional
                                              abox
                                              &key
                                              count)
 */

     public String allRoleAssertionsForIndividualInDomain(Object individual ) throws RacerClientException {
          return racerCall("all-role-assertions-for-individual-in-domain" , individual ).toString();
     }

     public RacerResult allRoleAssertionsForIndividualInDomain$(Object individual ) throws RacerClientException {
          return racerCall("all-role-assertions-for-individual-in-domain" , individual );
     }

     public String allRoleAssertionsForIndividualInDomain(Object individual, Object abox ) throws RacerClientException {
          return racerCall("all-role-assertions-for-individual-in-domain" , individual, abox ).toString();
     }

     public RacerResult allRoleAssertionsForIndividualInDomain$(Object individual, Object abox ) throws RacerClientException {
          return racerCall("all-role-assertions-for-individual-in-domain" , individual, abox );
     }

     public String allRoleAssertionsForIndividualInDomain(Object individual, Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-role-assertions-for-individual-in-domain" , individual, abox , keyArgs).toString();
     }

     public RacerResult allRoleAssertionsForIndividualInDomain$(Object individual, Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-role-assertions-for-individual-in-domain" , individual, abox , keyArgs);
     }

/** Racer Function all-role-assertions-for-individual-in-range
(all-role-assertions-for-individual-in-range individual
                                             &optional
                                             abox
                                             &key
                                             count)
 */

     public String allRoleAssertionsForIndividualInRange(Object individual ) throws RacerClientException {
          return racerCall("all-role-assertions-for-individual-in-range" , individual ).toString();
     }

     public RacerResult allRoleAssertionsForIndividualInRange$(Object individual ) throws RacerClientException {
          return racerCall("all-role-assertions-for-individual-in-range" , individual );
     }

     public String allRoleAssertionsForIndividualInRange(Object individual, Object abox ) throws RacerClientException {
          return racerCall("all-role-assertions-for-individual-in-range" , individual, abox ).toString();
     }

     public RacerResult allRoleAssertionsForIndividualInRange$(Object individual, Object abox ) throws RacerClientException {
          return racerCall("all-role-assertions-for-individual-in-range" , individual, abox );
     }

     public String allRoleAssertionsForIndividualInRange(Object individual, Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-role-assertions-for-individual-in-range" , individual, abox , keyArgs).toString();
     }

     public RacerResult allRoleAssertionsForIndividualInRange$(Object individual, Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-role-assertions-for-individual-in-range" , individual, abox , keyArgs);
     }

/** Racer Function all-roles
(all-roles &optional
           tbox
           &key
           count
           test
           inverse-test
           default)
 */

     public String allRoles( ) throws RacerClientException {
          return racerCall("all-roles"  ).toString();
     }

     public RacerResult allRoles$( ) throws RacerClientException {
          return racerCall("all-roles"  );
     }

     public String allRoles(Object tbox ) throws RacerClientException {
          return racerCall("all-roles" , tbox ).toString();
     }

     public RacerResult allRoles$(Object tbox ) throws RacerClientException {
          return racerCall("all-roles" , tbox );
     }

     public String allRoles(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-roles" , tbox , keyArgs).toString();
     }

     public RacerResult allRoles$(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-roles" , tbox , keyArgs);
     }

/** Racer Function all-rules
(all-rules &key
           abox
           type-of-substrate)
 */

     public String allRules( ) throws RacerClientException {
          return racerCall("all-rules"  ).toString();
     }

     public RacerResult allRules$( ) throws RacerClientException {
          return racerCall("all-rules"  );
     }

     public String allRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("all-rules"  , keyArgs).toString();
     }

     public RacerResult allRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("all-rules"  , keyArgs);
     }

/** Racer Function all-same-as-assertions
(all-same-as-assertions &optional abox &key count)
 */

     public String allSameAsAssertions( ) throws RacerClientException {
          return racerCall("all-same-as-assertions"  ).toString();
     }

     public RacerResult allSameAsAssertions$( ) throws RacerClientException {
          return racerCall("all-same-as-assertions"  );
     }

     public String allSameAsAssertions(Object abox ) throws RacerClientException {
          return racerCall("all-same-as-assertions" , abox ).toString();
     }

     public RacerResult allSameAsAssertions$(Object abox ) throws RacerClientException {
          return racerCall("all-same-as-assertions" , abox );
     }

     public String allSameAsAssertions(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-same-as-assertions" , abox , keyArgs).toString();
     }

     public RacerResult allSameAsAssertions$(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-same-as-assertions" , abox , keyArgs);
     }

/** Racer Function all-substrates
(all-substrates &key
                abox
                type-of-substrate)
 */

     public String allSubstrates( ) throws RacerClientException {
          return racerCall("all-substrates"  ).toString();
     }

     public RacerResult allSubstrates$( ) throws RacerClientException {
          return racerCall("all-substrates"  );
     }

     public String allSubstrates(  Object... keyArgs) throws RacerClientException {
          return racerCall("all-substrates"  , keyArgs).toString();
     }

     public RacerResult allSubstrates$(  Object... keyArgs) throws RacerClientException {
          return racerCall("all-substrates"  , keyArgs);
     }

/** Racer Function all-tboxes
(all-tboxes)
 */

     public String allTboxes( ) throws RacerClientException {
          return racerCall("all-tboxes"  ).toString();
     }

     public RacerResult allTboxes$( ) throws RacerClientException {
          return racerCall("all-tboxes"  );
     }

/** Racer Function all-transitive-roles
(all-transitive-roles &optional tbox &key count)
 */

     public String allTransitiveRoles( ) throws RacerClientException {
          return racerCall("all-transitive-roles"  ).toString();
     }

     public RacerResult allTransitiveRoles$( ) throws RacerClientException {
          return racerCall("all-transitive-roles"  );
     }

     public String allTransitiveRoles(Object tbox ) throws RacerClientException {
          return racerCall("all-transitive-roles" , tbox ).toString();
     }

     public RacerResult allTransitiveRoles$(Object tbox ) throws RacerClientException {
          return racerCall("all-transitive-roles" , tbox );
     }

     public String allTransitiveRoles(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-transitive-roles" , tbox , keyArgs).toString();
     }

     public RacerResult allTransitiveRoles$(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("all-transitive-roles" , tbox , keyArgs);
     }

/** Racer Function allow-overloaded-definitions
(allow-overloaded-definitions)
 */

     public String allowOverloadedDefinitions( ) throws RacerClientException {
          return racerCall("allow-overloaded-definitions"  ).toString();
     }

     public RacerResult allowOverloadedDefinitions$( ) throws RacerClientException {
          return racerCall("allow-overloaded-definitions"  );
     }

/** Racer Function answer-query
(answer-query &key
              execute-p
              dont-add-abox-duplicates-p
              remove-duplicates-p
              two-phase-processing-p
              deliver-phase-two-warning-tokens-p
              deliver-kb-has-changed-warning-tokens-p
              add-rule-consequences-p
              continuation-based-instance-retrieval-p
              told-information-reasoning-p
              final-consistency-checking-p
              runtime-consistency-checking-p
              verbose-p
              dont-show-variables
              dont-show-head-projection-operators-p
              dont-show-lambdas-p
              how-many
              only-new-tuples-p
              timeout
              proactive-tuple-computation-p
              tuple-at-a-time-p
              use-individual-synonyms-p
              check-abox-consistency-p
              ensure-tbox-classification-p
              initial-abox-mirroring-p
              initial-role-assertion-mirroring-p
              classify-concepts-in-instance-assertions-p
              exclude-permutations-p
              record-explanations-p
              parser-class
              rewrite-defined-concepts-p
              group-by-ops
              bind-specials-p
              original-query
              rule-con-pattern
              new-ind-ops
              premise
              generate-code-p
              optimize-p
              rewrite-semantically-p
              rewrite-to-dnf-p
              report-inconsistent-queries-p
              report-tautological-queries-p
              use-repository-p
              put-into-repository-p
              id
              dont-check-id-p
              parser
              result-vois
              substrate
              abox
              create-abox-if-not-found-p
              package
              type-of-substrate
              prepare-now-p)
 */

     public String answerQuery( ) throws RacerClientException {
          return racerCall("answer-query"  ).toString();
     }

     public RacerResult answerQuery$( ) throws RacerClientException {
          return racerCall("answer-query"  );
     }

     public String answerQuery(  Object... keyArgs) throws RacerClientException {
          return racerCall("answer-query"  , keyArgs).toString();
     }

     public RacerResult answerQuery$(  Object... keyArgs) throws RacerClientException {
          return racerCall("answer-query"  , keyArgs);
     }

/** Racer Function answer-query-under-premise
(answer-query-under-premise &key
                            execute-p
                            dont-add-abox-duplicates-p
                            remove-duplicates-p
                            two-phase-processing-p
                            deliver-phase-two-warning-tokens-p
                            deliver-kb-has-changed-warning-tokens-p
                            add-rule-consequences-p
                            continuation-based-instance-retrieval-p
                            told-information-reasoning-p
                            final-consistency-checking-p
                            runtime-consistency-checking-p
                            verbose-p
                            dont-show-variables
                            dont-show-head-projection-operators-p
                            dont-show-lambdas-p
                            how-many
                            only-new-tuples-p
                            timeout
                            proactive-tuple-computation-p
                            tuple-at-a-time-p
                            use-individual-synonyms-p
                            check-abox-consistency-p
                            ensure-tbox-classification-p
                            initial-abox-mirroring-p
                            initial-role-assertion-mirroring-p
                            classify-concepts-in-instance-assertions-p
                            exclude-permutations-p
                            record-explanations-p
                            parser-class
                            rewrite-defined-concepts-p
                            group-by-ops
                            bind-specials-p
                            original-query
                            rule-con-pattern
                            new-ind-ops
                            premise
                            generate-code-p
                            optimize-p
                            rewrite-semantically-p
                            rewrite-to-dnf-p
                            report-inconsistent-queries-p
                            report-tautological-queries-p
                            use-repository-p
                            put-into-repository-p
                            id
                            dont-check-id-p
                            parser
                            result-vois
                            substrate
                            abox
                            create-abox-if-not-found-p
                            package
                            type-of-substrate
                            prepare-now-p)
 */

     public String answerQueryUnderPremise( ) throws RacerClientException {
          return racerCall("answer-query-under-premise"  ).toString();
     }

     public RacerResult answerQueryUnderPremise$( ) throws RacerClientException {
          return racerCall("answer-query-under-premise"  );
     }

     public String answerQueryUnderPremise(  Object... keyArgs) throws RacerClientException {
          return racerCall("answer-query-under-premise"  , keyArgs).toString();
     }

     public RacerResult answerQueryUnderPremise$(  Object... keyArgs) throws RacerClientException {
          return racerCall("answer-query-under-premise"  , keyArgs);
     }

/** Racer Function answer-query-under-premise1
(answer-query-under-premise1 &key
                             execute-p
                             dont-add-abox-duplicates-p
                             remove-duplicates-p
                             two-phase-processing-p
                             deliver-phase-two-warning-tokens-p
                             deliver-kb-has-changed-warning-tokens-p
                             add-rule-consequences-p
                             continuation-based-instance-retrieval-p
                             told-information-reasoning-p
                             final-consistency-checking-p
                             runtime-consistency-checking-p
                             verbose-p
                             dont-show-variables
                             dont-show-head-projection-operators-p
                             dont-show-lambdas-p
                             how-many
                             only-new-tuples-p
                             timeout
                             proactive-tuple-computation-p
                             tuple-at-a-time-p
                             use-individual-synonyms-p
                             check-abox-consistency-p
                             ensure-tbox-classification-p
                             initial-abox-mirroring-p
                             initial-role-assertion-mirroring-p
                             classify-concepts-in-instance-assertions-p
                             exclude-permutations-p
                             record-explanations-p
                             parser-class
                             rewrite-defined-concepts-p
                             group-by-ops
                             bind-specials-p
                             original-query
                             rule-con-pattern
                             new-ind-ops
                             premise
                             generate-code-p
                             optimize-p
                             rewrite-semantically-p
                             rewrite-to-dnf-p
                             report-inconsistent-queries-p
                             report-tautological-queries-p
                             use-repository-p
                             put-into-repository-p
                             id
                             dont-check-id-p
                             parser
                             result-vois
                             substrate
                             abox
                             create-abox-if-not-found-p
                             package
                             type-of-substrate
                             prepare-now-p)
 */

     public String answerQueryUnderPremise1( ) throws RacerClientException {
          return racerCall("answer-query-under-premise1"  ).toString();
     }

     public RacerResult answerQueryUnderPremise1$( ) throws RacerClientException {
          return racerCall("answer-query-under-premise1"  );
     }

     public String answerQueryUnderPremise1(  Object... keyArgs) throws RacerClientException {
          return racerCall("answer-query-under-premise1"  , keyArgs).toString();
     }

     public RacerResult answerQueryUnderPremise1$(  Object... keyArgs) throws RacerClientException {
          return racerCall("answer-query-under-premise1"  , keyArgs);
     }

/** Racer Function answer-query1
(answer-query1 &key
               execute-p
               dont-add-abox-duplicates-p
               remove-duplicates-p
               two-phase-processing-p
               deliver-phase-two-warning-tokens-p
               deliver-kb-has-changed-warning-tokens-p
               add-rule-consequences-p
               continuation-based-instance-retrieval-p
               told-information-reasoning-p
               final-consistency-checking-p
               runtime-consistency-checking-p
               verbose-p
               dont-show-variables
               dont-show-head-projection-operators-p
               dont-show-lambdas-p
               how-many
               only-new-tuples-p
               timeout
               proactive-tuple-computation-p
               tuple-at-a-time-p
               use-individual-synonyms-p
               check-abox-consistency-p
               ensure-tbox-classification-p
               initial-abox-mirroring-p
               initial-role-assertion-mirroring-p
               classify-concepts-in-instance-assertions-p
               exclude-permutations-p
               record-explanations-p
               parser-class
               rewrite-defined-concepts-p
               group-by-ops
               bind-specials-p
               original-query
               rule-con-pattern
               new-ind-ops
               premise
               generate-code-p
               optimize-p
               rewrite-semantically-p
               rewrite-to-dnf-p
               report-inconsistent-queries-p
               report-tautological-queries-p
               use-repository-p
               put-into-repository-p
               id
               dont-check-id-p
               parser
               result-vois
               substrate
               abox
               create-abox-if-not-found-p
               package
               type-of-substrate
               prepare-now-p)
 */

     public String answerQuery1( ) throws RacerClientException {
          return racerCall("answer-query1"  ).toString();
     }

     public RacerResult answerQuery1$( ) throws RacerClientException {
          return racerCall("answer-query1"  );
     }

     public String answerQuery1(  Object... keyArgs) throws RacerClientException {
          return racerCall("answer-query1"  , keyArgs).toString();
     }

     public RacerResult answerQuery1$(  Object... keyArgs) throws RacerClientException {
          return racerCall("answer-query1"  , keyArgs);
     }

/** Racer Function answer-tbox-query
(answer-tbox-query &key
                   execute-p
                   dont-add-abox-duplicates-p
                   remove-duplicates-p
                   two-phase-processing-p
                   deliver-phase-two-warning-tokens-p
                   deliver-kb-has-changed-warning-tokens-p
                   add-rule-consequences-p
                   continuation-based-instance-retrieval-p
                   told-information-reasoning-p
                   final-consistency-checking-p
                   runtime-consistency-checking-p
                   verbose-p
                   dont-show-variables
                   dont-show-head-projection-operators-p
                   dont-show-lambdas-p
                   how-many
                   only-new-tuples-p
                   timeout
                   proactive-tuple-computation-p
                   tuple-at-a-time-p
                   use-individual-synonyms-p
                   check-abox-consistency-p
                   ensure-tbox-classification-p
                   initial-abox-mirroring-p
                   initial-role-assertion-mirroring-p
                   classify-concepts-in-instance-assertions-p
                   exclude-permutations-p
                   record-explanations-p
                   parser-class
                   rewrite-defined-concepts-p
                   group-by-ops
                   bind-specials-p
                   original-query
                   rule-con-pattern
                   new-ind-ops
                   premise
                   generate-code-p
                   optimize-p
                   rewrite-semantically-p
                   rewrite-to-dnf-p
                   report-inconsistent-queries-p
                   report-tautological-queries-p
                   use-repository-p
                   put-into-repository-p
                   id
                   dont-check-id-p
                   parser
                   result-vois
                   tbox
                   package
                   create-tbox-if-not-found-p
                   substrate)
 */

     public String answerTboxQuery( ) throws RacerClientException {
          return racerCall("answer-tbox-query"  ).toString();
     }

     public RacerResult answerTboxQuery$( ) throws RacerClientException {
          return racerCall("answer-tbox-query"  );
     }

     public String answerTboxQuery(  Object... keyArgs) throws RacerClientException {
          return racerCall("answer-tbox-query"  , keyArgs).toString();
     }

     public RacerResult answerTboxQuery$(  Object... keyArgs) throws RacerClientException {
          return racerCall("answer-tbox-query"  , keyArgs);
     }

/** Racer Function answer-tbox-query1
(answer-tbox-query1 &key
                    execute-p
                    dont-add-abox-duplicates-p
                    remove-duplicates-p
                    two-phase-processing-p
                    deliver-phase-two-warning-tokens-p
                    deliver-kb-has-changed-warning-tokens-p
                    add-rule-consequences-p
                    continuation-based-instance-retrieval-p
                    told-information-reasoning-p
                    final-consistency-checking-p
                    runtime-consistency-checking-p
                    verbose-p
                    dont-show-variables
                    dont-show-head-projection-operators-p
                    dont-show-lambdas-p
                    how-many
                    only-new-tuples-p
                    timeout
                    proactive-tuple-computation-p
                    tuple-at-a-time-p
                    use-individual-synonyms-p
                    check-abox-consistency-p
                    ensure-tbox-classification-p
                    initial-abox-mirroring-p
                    initial-role-assertion-mirroring-p
                    classify-concepts-in-instance-assertions-p
                    exclude-permutations-p
                    record-explanations-p
                    parser-class
                    rewrite-defined-concepts-p
                    group-by-ops
                    bind-specials-p
                    original-query
                    rule-con-pattern
                    new-ind-ops
                    premise
                    generate-code-p
                    optimize-p
                    rewrite-semantically-p
                    rewrite-to-dnf-p
                    report-inconsistent-queries-p
                    report-tautological-queries-p
                    use-repository-p
                    put-into-repository-p
                    id
                    dont-check-id-p
                    parser
                    result-vois
                    tbox
                    package
                    create-tbox-if-not-found-p
                    substrate)
 */

     public String answerTboxQuery1( ) throws RacerClientException {
          return racerCall("answer-tbox-query1"  ).toString();
     }

     public RacerResult answerTboxQuery1$( ) throws RacerClientException {
          return racerCall("answer-tbox-query1"  );
     }

     public String answerTboxQuery1(  Object... keyArgs) throws RacerClientException {
          return racerCall("answer-tbox-query1"  , keyArgs).toString();
     }

     public RacerResult answerTboxQuery1$(  Object... keyArgs) throws RacerClientException {
          return racerCall("answer-tbox-query1"  , keyArgs);
     }

/** Racer Function applicable-rules
(applicable-rules &key
                  abox
                  type-of-substrate)
 */

     public String applicableRules( ) throws RacerClientException {
          return racerCall("applicable-rules"  ).toString();
     }

     public RacerResult applicableRules$( ) throws RacerClientException {
          return racerCall("applicable-rules"  );
     }

     public String applicableRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("applicable-rules"  , keyArgs).toString();
     }

     public RacerResult applicableRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("applicable-rules"  , keyArgs);
     }

/** Racer Function apply-rule
(apply-rule &key
            execute-p
            parser-class
            rewrite-defined-concepts-p
            group-by-ops
            bind-specials-p
            original-query
            rule-con-pattern
            new-ind-ops
            premise
            generate-code-p
            optimize-p
            rewrite-semantically-p
            rewrite-to-dnf-p
            report-inconsistent-queries-p
            report-tautological-queries-p
            use-repository-p
            put-into-repository-p
            id
            dont-check-id-p
            parser
            result-vois
            substrate
            abox
            create-abox-if-not-found-p
            package
            type-of-substrate
            prepare-now-p)
 */

     public String applyRule( ) throws RacerClientException {
          return racerCall("apply-rule"  ).toString();
     }

     public RacerResult applyRule$( ) throws RacerClientException {
          return racerCall("apply-rule"  );
     }

     public String applyRule(  Object... keyArgs) throws RacerClientException {
          return racerCall("apply-rule"  , keyArgs).toString();
     }

     public RacerResult applyRule$(  Object... keyArgs) throws RacerClientException {
          return racerCall("apply-rule"  , keyArgs);
     }

/** Racer Function apply-rule-under-premise
(apply-rule-under-premise &key
                          execute-p
                          parser-class
                          rewrite-defined-concepts-p
                          group-by-ops
                          bind-specials-p
                          original-query
                          rule-con-pattern
                          new-ind-ops
                          premise
                          generate-code-p
                          optimize-p
                          rewrite-semantically-p
                          rewrite-to-dnf-p
                          report-inconsistent-queries-p
                          report-tautological-queries-p
                          use-repository-p
                          put-into-repository-p
                          id
                          dont-check-id-p
                          parser
                          result-vois
                          substrate
                          abox
                          create-abox-if-not-found-p
                          package
                          type-of-substrate
                          prepare-now-p)
 */

     public String applyRuleUnderPremise( ) throws RacerClientException {
          return racerCall("apply-rule-under-premise"  ).toString();
     }

     public RacerResult applyRuleUnderPremise$( ) throws RacerClientException {
          return racerCall("apply-rule-under-premise"  );
     }

     public String applyRuleUnderPremise(  Object... keyArgs) throws RacerClientException {
          return racerCall("apply-rule-under-premise"  , keyArgs).toString();
     }

     public RacerResult applyRuleUnderPremise$(  Object... keyArgs) throws RacerClientException {
          return racerCall("apply-rule-under-premise"  , keyArgs);
     }

/** Racer Function apply-rule-under-premise1
(apply-rule-under-premise1 &key
                           execute-p
                           parser-class
                           rewrite-defined-concepts-p
                           group-by-ops
                           bind-specials-p
                           original-query
                           rule-con-pattern
                           new-ind-ops
                           premise
                           generate-code-p
                           optimize-p
                           rewrite-semantically-p
                           rewrite-to-dnf-p
                           report-inconsistent-queries-p
                           report-tautological-queries-p
                           use-repository-p
                           put-into-repository-p
                           id
                           dont-check-id-p
                           parser
                           result-vois
                           substrate
                           abox
                           create-abox-if-not-found-p
                           package
                           type-of-substrate
                           prepare-now-p)
 */

     public String applyRuleUnderPremise1( ) throws RacerClientException {
          return racerCall("apply-rule-under-premise1"  ).toString();
     }

     public RacerResult applyRuleUnderPremise1$( ) throws RacerClientException {
          return racerCall("apply-rule-under-premise1"  );
     }

     public String applyRuleUnderPremise1(  Object... keyArgs) throws RacerClientException {
          return racerCall("apply-rule-under-premise1"  , keyArgs).toString();
     }

     public RacerResult applyRuleUnderPremise1$(  Object... keyArgs) throws RacerClientException {
          return racerCall("apply-rule-under-premise1"  , keyArgs);
     }

/** Racer Function associated-aboxes
(associated-aboxes tbox)
 */

     public String associatedAboxes(Object tbox ) throws RacerClientException {
          return racerCall("associated-aboxes" , tbox ).toString();
     }

     public RacerResult associatedAboxes$(Object tbox ) throws RacerClientException {
          return racerCall("associated-aboxes" , tbox );
     }

/** Racer Function associated-tbox
(associated-tbox abox)
 */

     public String associatedTbox(Object abox ) throws RacerClientException {
          return racerCall("associated-tbox" , abox ).toString();
     }

     public RacerResult associatedTbox$(Object abox ) throws RacerClientException {
          return racerCall("associated-tbox" , abox );
     }

/** Racer Function asymmetric-p
(asymmetric-p role-term &optional tbox)
 */

     public boolean asymmetricP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("asymmetric-p" , roleTerm ));
     }

     public boolean asymmetricP(Object roleTerm, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("asymmetric-p" , roleTerm, tbox ));
     }

/** Racer Function atomic-concept-ancestors
(atomic-concept-ancestors concept-term tbox)
 */

     public String atomicConceptAncestors(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-concept-ancestors" , conceptTerm, tbox ).toString();
     }

     public RacerResult atomicConceptAncestors$(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-concept-ancestors" , conceptTerm, tbox );
     }

/** Racer Function atomic-concept-children
(atomic-concept-children concept-term tbox)
 */

     public String atomicConceptChildren(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-concept-children" , conceptTerm, tbox ).toString();
     }

     public RacerResult atomicConceptChildren$(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-concept-children" , conceptTerm, tbox );
     }

/** Racer Function atomic-concept-descendants
(atomic-concept-descendants concept-term tbox)
 */

     public String atomicConceptDescendants(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-concept-descendants" , conceptTerm, tbox ).toString();
     }

     public RacerResult atomicConceptDescendants$(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-concept-descendants" , conceptTerm, tbox );
     }

/** Racer Function atomic-concept-parents
(atomic-concept-parents concept-term tbox)
 */

     public String atomicConceptParents(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-concept-parents" , conceptTerm, tbox ).toString();
     }

     public RacerResult atomicConceptParents$(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-concept-parents" , conceptTerm, tbox );
     }

/** Racer Function atomic-concept-synonyms
(atomic-concept-synonyms concept-term tbox)
 */

     public String atomicConceptSynonyms(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-concept-synonyms" , conceptTerm, tbox ).toString();
     }

     public RacerResult atomicConceptSynonyms$(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-concept-synonyms" , conceptTerm, tbox );
     }

/** Racer Function atomic-role-ancestors
(atomic-role-ancestors role-term tbox)
 */

     public String atomicRoleAncestors(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-ancestors" , roleTerm, tbox ).toString();
     }

     public RacerResult atomicRoleAncestors$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-ancestors" , roleTerm, tbox );
     }

/** Racer Function atomic-role-children
(atomic-role-children role-term
                      tbox
                      &key
                      synsets-p)
 */

     public String atomicRoleChildren(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-children" , roleTerm, tbox ).toString();
     }

     public RacerResult atomicRoleChildren$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-children" , roleTerm, tbox );
     }

     public String atomicRoleChildren(Object roleTerm, Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("atomic-role-children" , roleTerm, tbox , keyArgs).toString();
     }

     public RacerResult atomicRoleChildren$(Object roleTerm, Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("atomic-role-children" , roleTerm, tbox , keyArgs);
     }

/** Racer Function atomic-role-descendants
(atomic-role-descendants role-term tbox)
 */

     public String atomicRoleDescendants(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-descendants" , roleTerm, tbox ).toString();
     }

     public RacerResult atomicRoleDescendants$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-descendants" , roleTerm, tbox );
     }

/** Racer Function atomic-role-domain
(atomic-role-domain role-term tbox)
 */

     public String atomicRoleDomain(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-domain" , roleTerm, tbox ).toString();
     }

     public RacerResult atomicRoleDomain$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-domain" , roleTerm, tbox );
     }

/** Racer Function atomic-role-inverse
(atomic-role-inverse role-term &optional tbox)
 */

     public String atomicRoleInverse(Object roleTerm ) throws RacerClientException {
          return racerCall("atomic-role-inverse" , roleTerm ).toString();
     }

     public RacerResult atomicRoleInverse$(Object roleTerm ) throws RacerClientException {
          return racerCall("atomic-role-inverse" , roleTerm );
     }

     public String atomicRoleInverse(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-inverse" , roleTerm, tbox ).toString();
     }

     public RacerResult atomicRoleInverse$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-inverse" , roleTerm, tbox );
     }

/** Racer Function atomic-role-parents
(atomic-role-parents role-term tbox &key synsets-p)
 */

     public String atomicRoleParents(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-parents" , roleTerm, tbox ).toString();
     }

     public RacerResult atomicRoleParents$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-parents" , roleTerm, tbox );
     }

     public String atomicRoleParents(Object roleTerm, Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("atomic-role-parents" , roleTerm, tbox , keyArgs).toString();
     }

     public RacerResult atomicRoleParents$(Object roleTerm, Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("atomic-role-parents" , roleTerm, tbox , keyArgs);
     }

/** Racer Function atomic-role-range
(atomic-role-range role-term tbox)
 */

     public String atomicRoleRange(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-range" , roleTerm, tbox ).toString();
     }

     public RacerResult atomicRoleRange$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-range" , roleTerm, tbox );
     }

/** Racer Function atomic-role-synonyms
(atomic-role-synonyms role-term tbox)
 */

     public String atomicRoleSynonyms(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-synonyms" , roleTerm, tbox ).toString();
     }

     public RacerResult atomicRoleSynonyms$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("atomic-role-synonyms" , roleTerm, tbox );
     }

/** Racer Function attribute-domain-1
(attribute-domain-1 attribute-name &optional tbox)
 */

     public String attributeDomain1(Object attributeName ) throws RacerClientException {
          return racerCall("attribute-domain-1" , attributeName ).toString();
     }

     public RacerResult attributeDomain1$(Object attributeName ) throws RacerClientException {
          return racerCall("attribute-domain-1" , attributeName );
     }

     public String attributeDomain1(Object attributeName, Object tbox ) throws RacerClientException {
          return racerCall("attribute-domain-1" , attributeName, tbox ).toString();
     }

     public RacerResult attributeDomain1$(Object attributeName, Object tbox ) throws RacerClientException {
          return racerCall("attribute-domain-1" , attributeName, tbox );
     }

/** Racer Function attribute-type
(attribute-type attribute-name &optional tbox)
 */

     public String attributeType(Object attributeName ) throws RacerClientException {
          return racerCall("attribute-type" , attributeName ).toString();
     }

     public RacerResult attributeType$(Object attributeName ) throws RacerClientException {
          return racerCall("attribute-type" , attributeName );
     }

     public String attributeType(Object attributeName, Object tbox ) throws RacerClientException {
          return racerCall("attribute-type" , attributeName, tbox ).toString();
     }

     public RacerResult attributeType$(Object attributeName, Object tbox ) throws RacerClientException {
          return racerCall("attribute-type" , attributeName, tbox );
     }

/** Racer Function cd-attribute-p
(cd-attribute-p attribute &optional tbox)
 */

     public boolean cdAttributeP(Object attribute ) throws RacerClientException {
          return returnBoolean(racerCall("cd-attribute-p" , attribute ));
     }

     public boolean cdAttributeP(Object attribute, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("cd-attribute-p" , attribute, tbox ));
     }

/** Racer Function cd-object-p
(cd-object-p object-name &optional abox)
 */

     public boolean cdObjectP(Object objectName ) throws RacerClientException {
          return returnBoolean(racerCall("cd-object-p" , objectName ));
     }

     public boolean cdObjectP(Object objectName, Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("cd-object-p" , objectName, abox ));
     }

/** Racer Function cheap-queries
(cheap-queries &key
               abox
               type-of-substrate)
 */

     public String cheapQueries( ) throws RacerClientException {
          return racerCall("cheap-queries"  ).toString();
     }

     public RacerResult cheapQueries$( ) throws RacerClientException {
          return racerCall("cheap-queries"  );
     }

     public String cheapQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("cheap-queries"  , keyArgs).toString();
     }

     public RacerResult cheapQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("cheap-queries"  , keyArgs);
     }

/** Racer Function cheap-query-p
(cheap-query-p query)
 */

     public boolean cheapQueryP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("cheap-query-p" , query ));
     }

/** Racer Function cheap-rule-p
(cheap-rule-p query)
 */

     public boolean cheapRuleP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("cheap-rule-p" , query ));
     }

/** Racer Function cheap-rules
(cheap-rules &key
             abox
             type-of-substrate)
 */

     public String cheapRules( ) throws RacerClientException {
          return racerCall("cheap-rules"  ).toString();
     }

     public RacerResult cheapRules$( ) throws RacerClientException {
          return racerCall("cheap-rules"  );
     }

     public String cheapRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("cheap-rules"  , keyArgs).toString();
     }

     public RacerResult cheapRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("cheap-rules"  , keyArgs);
     }

/** Racer Function check-abox-coherence
(check-abox-coherence &optional abox filename-or-stream)
 */

     public String checkAboxCoherence( ) throws RacerClientException {
          return racerCall("check-abox-coherence"  ).toString();
     }

     public RacerResult checkAboxCoherence$( ) throws RacerClientException {
          return racerCall("check-abox-coherence"  );
     }

     public String checkAboxCoherence(Object abox, Object filenameOrStream ) throws RacerClientException {
          return racerCall("check-abox-coherence" , abox, filenameOrStream ).toString();
     }

     public RacerResult checkAboxCoherence$(Object abox, Object filenameOrStream ) throws RacerClientException {
          return racerCall("check-abox-coherence" , abox, filenameOrStream );
     }

     public String checkAboxCoherence(Object abox ) throws RacerClientException {
          return racerCall("check-abox-coherence" , abox ).toString();
     }

     public RacerResult checkAboxCoherence$(Object abox ) throws RacerClientException {
          return racerCall("check-abox-coherence" , abox );
     }

/** Racer Function check-abox-consistency-before-querying
(check-abox-consistency-before-querying)
 */

     public String checkAboxConsistencyBeforeQuerying( ) throws RacerClientException {
          return racerCall("check-abox-consistency-before-querying"  ).toString();
     }

     public RacerResult checkAboxConsistencyBeforeQuerying$( ) throws RacerClientException {
          return racerCall("check-abox-consistency-before-querying"  );
     }

/** Racer Function check-concept-coherence
(check-concept-coherence concept
                         &optional
                         tbox)
 */

     public String checkConceptCoherence(Object concept ) throws RacerClientException {
          return racerCall("check-concept-coherence" , concept ).toString();
     }

     public RacerResult checkConceptCoherence$(Object concept ) throws RacerClientException {
          return racerCall("check-concept-coherence" , concept );
     }

     public String checkConceptCoherence(Object concept, Object tbox ) throws RacerClientException {
          return racerCall("check-concept-coherence" , concept, tbox ).toString();
     }

     public RacerResult checkConceptCoherence$(Object concept, Object tbox ) throws RacerClientException {
          return racerCall("check-concept-coherence" , concept, tbox );
     }

/** Racer Function check-for-updates
(check-for-updates &key url)
 */

     public String checkForUpdates( ) throws RacerClientException {
          return racerCall("check-for-updates"  ).toString();
     }

     public RacerResult checkForUpdates$( ) throws RacerClientException {
          return racerCall("check-for-updates"  );
     }

     public String checkForUpdates(  Object... keyArgs) throws RacerClientException {
          return racerCall("check-for-updates"  , keyArgs).toString();
     }

     public RacerResult checkForUpdates$(  Object... keyArgs) throws RacerClientException {
          return racerCall("check-for-updates"  , keyArgs);
     }

/** Racer Function check-nrql-subscriptions
(check-nrql-subscriptions &optional abox)
 */

     public String checkNrqlSubscriptions( ) throws RacerClientException {
          return racerCall("check-nrql-subscriptions"  ).toString();
     }

     public RacerResult checkNrqlSubscriptions$( ) throws RacerClientException {
          return racerCall("check-nrql-subscriptions"  );
     }

     public String checkNrqlSubscriptions(Object abox ) throws RacerClientException {
          return racerCall("check-nrql-subscriptions" , abox ).toString();
     }

     public RacerResult checkNrqlSubscriptions$(Object abox ) throws RacerClientException {
          return racerCall("check-nrql-subscriptions" , abox );
     }

/** Racer Function check-ontology
(check-ontology filename
                &key
                verbose
                explain-all
                n)
 */

     public String checkOntology(Object filename ) throws RacerClientException {
          return racerCall("check-ontology" , filename ).toString();
     }

     public RacerResult checkOntology$(Object filename ) throws RacerClientException {
          return racerCall("check-ontology" , filename );
     }

     public String checkOntology(Object filename , Object... keyArgs) throws RacerClientException {
          return racerCall("check-ontology" , filename , keyArgs).toString();
     }

     public RacerResult checkOntology$(Object filename , Object... keyArgs) throws RacerClientException {
          return racerCall("check-ontology" , filename , keyArgs);
     }

/** Racer Function check-subscriptions
(check-subscriptions abox)
 */

     public String checkSubscriptions(Object abox ) throws RacerClientException {
          return racerCall("check-subscriptions" , abox ).toString();
     }

     public RacerResult checkSubscriptions$(Object abox ) throws RacerClientException {
          return racerCall("check-subscriptions" , abox );
     }

/** Racer Function check-tbox-coherence
(check-tbox-coherence &optional tbox &key stream)
 */

     public String checkTboxCoherence( ) throws RacerClientException {
          return racerCall("check-tbox-coherence"  ).toString();
     }

     public RacerResult checkTboxCoherence$( ) throws RacerClientException {
          return racerCall("check-tbox-coherence"  );
     }

     public String checkTboxCoherence(Object tbox ) throws RacerClientException {
          return racerCall("check-tbox-coherence" , tbox ).toString();
     }

     public RacerResult checkTboxCoherence$(Object tbox ) throws RacerClientException {
          return racerCall("check-tbox-coherence" , tbox );
     }

     public String checkTboxCoherence(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("check-tbox-coherence" , tbox , keyArgs).toString();
     }

     public RacerResult checkTboxCoherence$(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("check-tbox-coherence" , tbox , keyArgs);
     }

/** Racer Function choose-current-set-of-rule-consequences
(choose-current-set-of-rule-consequences query)
 */

     public String chooseCurrentSetOfRuleConsequences(Object query ) throws RacerClientException {
          return racerCall("choose-current-set-of-rule-consequences" , query ).toString();
     }

     public RacerResult chooseCurrentSetOfRuleConsequences$(Object query ) throws RacerClientException {
          return racerCall("choose-current-set-of-rule-consequences" , query );
     }

/** Racer Function classify-query
(classify-query query)
 */

     public String classifyQuery(Object query ) throws RacerClientException {
          return racerCall("classify-query" , query ).toString();
     }

     public RacerResult classifyQuery$(Object query ) throws RacerClientException {
          return racerCall("classify-query" , query );
     }

/** Racer Function classify-tbox
(classify-tbox &optional tbox)
 */

     public String classifyTbox( ) throws RacerClientException {
          return racerCall("classify-tbox"  ).toString();
     }

     public RacerResult classifyTbox$( ) throws RacerClientException {
          return racerCall("classify-tbox"  );
     }

     public String classifyTbox(Object tbox ) throws RacerClientException {
          return racerCall("classify-tbox" , tbox ).toString();
     }

     public RacerResult classifyTbox$(Object tbox ) throws RacerClientException {
          return racerCall("classify-tbox" , tbox );
     }

/** Racer Function clear-all-documentation
(clear-all-documentation)
 */

     public String clearAllDocumentation( ) throws RacerClientException {
          return racerCall("clear-all-documentation"  ).toString();
     }

     public RacerResult clearAllDocumentation$( ) throws RacerClientException {
          return racerCall("clear-all-documentation"  );
     }

/** Racer Function clear-default-tbox
(clear-default-tbox)
 */

     public String clearDefaultTbox( ) throws RacerClientException {
          return racerCall("clear-default-tbox"  ).toString();
     }

     public RacerResult clearDefaultTbox$( ) throws RacerClientException {
          return racerCall("clear-default-tbox"  );
     }

/** Racer Function close-triple-store
(close-triple-store &key db if-closed)
 */

     public String closeTripleStore( ) throws RacerClientException {
          return racerCall("close-triple-store"  ).toString();
     }

     public RacerResult closeTripleStore$( ) throws RacerClientException {
          return racerCall("close-triple-store"  );
     }

     public String closeTripleStore(  Object... keyArgs) throws RacerClientException {
          return racerCall("close-triple-store"  , keyArgs).toString();
     }

     public RacerResult closeTripleStore$(  Object... keyArgs) throws RacerClientException {
          return racerCall("close-triple-store"  , keyArgs);
     }

/** Racer Function compute-abox-difference1
(compute-abox-difference1 a
                          b
                          &key
                          also-unmapped-differences-p
                          remove-redundant-diffs-p
                          optimizer-max-plans
                          known-correspondances
                          auto-correspondances-p
                          only-difference-p
                          full-tuples-p
                          show-score-p
                          equi-order-by
                          remove-implied-concept-assertions-p
                          remove-common-assertions-p
                          common-assertions-as-strict-atoms-p
                          map-new-inds-to-new-inds-p
                          cutoff-fn
                          hypo-mode-stack
                          c-mode
                          r-mode
                          only-best-p
                          order-by
                          reverse-order-p
                          ensure-permutations-p
                          how-many
                          strategy
                          simple-result-p
                          runtime-consistency-checking-p
                          final-consistency-checking-p
                          same-as-only-p
                          candidate-individuals
                          binding-validator)
 */

     public String computeAboxDifference1(Object a, Object b ) throws RacerClientException {
          return racerCall("compute-abox-difference1" , a, b ).toString();
     }

     public RacerResult computeAboxDifference1$(Object a, Object b ) throws RacerClientException {
          return racerCall("compute-abox-difference1" , a, b );
     }

     public String computeAboxDifference1(Object a, Object b , Object... keyArgs) throws RacerClientException {
          return racerCall("compute-abox-difference1" , a, b , keyArgs).toString();
     }

     public RacerResult computeAboxDifference1$(Object a, Object b , Object... keyArgs) throws RacerClientException {
          return racerCall("compute-abox-difference1" , a, b , keyArgs);
     }

/** Racer Function compute-abox-difference2
(compute-abox-difference2 a
                          b
                          &key
                          also-unmapped-differences-p
                          remove-redundant-diffs-p
                          optimizer-max-plans
                          known-correspondances
                          auto-correspondances-p
                          only-difference-p
                          full-tuples-p
                          show-score-p
                          equi-order-by
                          remove-implied-concept-assertions-p
                          remove-common-assertions-p
                          common-assertions-as-strict-atoms-p
                          map-new-inds-to-new-inds-p
                          cutoff-fn
                          hypo-mode-stack
                          c-mode
                          r-mode
                          only-best-p
                          order-by
                          reverse-order-p
                          ensure-permutations-p
                          how-many
                          strategy
                          simple-result-p
                          runtime-consistency-checking-p
                          final-consistency-checking-p
                          same-as-only-p
                          candidate-individuals
                          binding-validator)
 */

     public String computeAboxDifference2(Object a, Object b ) throws RacerClientException {
          return racerCall("compute-abox-difference2" , a, b ).toString();
     }

     public RacerResult computeAboxDifference2$(Object a, Object b ) throws RacerClientException {
          return racerCall("compute-abox-difference2" , a, b );
     }

     public String computeAboxDifference2(Object a, Object b , Object... keyArgs) throws RacerClientException {
          return racerCall("compute-abox-difference2" , a, b , keyArgs).toString();
     }

     public RacerResult computeAboxDifference2$(Object a, Object b , Object... keyArgs) throws RacerClientException {
          return racerCall("compute-abox-difference2" , a, b , keyArgs);
     }

/** Racer Function compute-all-implicit-role-fillers
(compute-all-implicit-role-fillers &optional abox)
 */

     public String computeAllImplicitRoleFillers( ) throws RacerClientException {
          return racerCall("compute-all-implicit-role-fillers"  ).toString();
     }

     public RacerResult computeAllImplicitRoleFillers$( ) throws RacerClientException {
          return racerCall("compute-all-implicit-role-fillers"  );
     }

     public String computeAllImplicitRoleFillers(Object abox ) throws RacerClientException {
          return racerCall("compute-all-implicit-role-fillers" , abox ).toString();
     }

     public RacerResult computeAllImplicitRoleFillers$(Object abox ) throws RacerClientException {
          return racerCall("compute-all-implicit-role-fillers" , abox );
     }

/** Racer Function compute-implicit-role-fillers
(compute-implicit-role-fillers individual-name
                               &optional
                               abox)
 */

     public String computeImplicitRoleFillers(Object individualName ) throws RacerClientException {
          return racerCall("compute-implicit-role-fillers" , individualName ).toString();
     }

     public RacerResult computeImplicitRoleFillers$(Object individualName ) throws RacerClientException {
          return racerCall("compute-implicit-role-fillers" , individualName );
     }

     public String computeImplicitRoleFillers(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("compute-implicit-role-fillers" , individualName, abox ).toString();
     }

     public RacerResult computeImplicitRoleFillers$(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("compute-implicit-role-fillers" , individualName, abox );
     }

/** Racer Function compute-index-for-instance-retrieval
(compute-index-for-instance-retrieval &optional abox)
 */

     public String computeIndexForInstanceRetrieval( ) throws RacerClientException {
          return racerCall("compute-index-for-instance-retrieval"  ).toString();
     }

     public RacerResult computeIndexForInstanceRetrieval$( ) throws RacerClientException {
          return racerCall("compute-index-for-instance-retrieval"  );
     }

     public String computeIndexForInstanceRetrieval(Object abox ) throws RacerClientException {
          return racerCall("compute-index-for-instance-retrieval" , abox ).toString();
     }

     public RacerResult computeIndexForInstanceRetrieval$(Object abox ) throws RacerClientException {
          return racerCall("compute-index-for-instance-retrieval" , abox );
     }

/** Racer Function compute-subgraph-aboxes
(compute-subgraph-aboxes abox-or-name)
 */

     public String computeSubgraphAboxes(Object aboxOrName ) throws RacerClientException {
          return racerCall("compute-subgraph-aboxes" , aboxOrName ).toString();
     }

     public RacerResult computeSubgraphAboxes$(Object aboxOrName ) throws RacerClientException {
          return racerCall("compute-subgraph-aboxes" , aboxOrName );
     }

/** Racer Function concept-disjoint-p
(concept-disjoint-p concept-1 concept-2 tbox)
 */

     public boolean conceptDisjointP(Object concept1, Object concept2, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("concept-disjoint-p" , concept1, concept2, tbox ));
     }

/** Racer Function concept-equivalent-p
(concept-equivalent-p concept-1 concept-2 tbox)
 */

     public boolean conceptEquivalentP(Object concept1, Object concept2, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("concept-equivalent-p" , concept1, concept2, tbox ));
     }

/** Racer Function concept-is-primitive-p
(concept-is-primitive-p concept-name &optional tbox)
 */

     public boolean conceptIsPrimitiveP(Object conceptName ) throws RacerClientException {
          return returnBoolean(racerCall("concept-is-primitive-p" , conceptName ));
     }

     public boolean conceptIsPrimitiveP(Object conceptName, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("concept-is-primitive-p" , conceptName, tbox ));
     }

/** Racer Function concept-p
(concept-p concept-name &optional tbox)
 */

     public boolean conceptP(Object conceptName ) throws RacerClientException {
          return returnBoolean(racerCall("concept-p" , conceptName ));
     }

     public boolean conceptP(Object conceptName, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("concept-p" , conceptName, tbox ));
     }

/** Racer Function concept-satisfiable-p
(concept-satisfiable-p concept-term tbox)
 */

     public boolean conceptSatisfiableP(Object conceptTerm, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("concept-satisfiable-p" , conceptTerm, tbox ));
     }

/** Racer Function concept-subsumes-p
(concept-subsumes-p subsumer subsumee tbox)
 */

     public boolean conceptSubsumesP(Object subsumer, Object subsumee, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("concept-subsumes-p" , subsumer, subsumee, tbox ));
     }

/** Racer Function constraint-entailed-p
(constraint-entailed-p constraint &optional abox)
 */

     public boolean constraintEntailedP(Object constraint ) throws RacerClientException {
          return returnBoolean(racerCall("constraint-entailed-p" , constraint ));
     }

     public boolean constraintEntailedP(Object constraint, Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("constraint-entailed-p" , constraint, abox ));
     }

/** Racer Function convert-event-specs
(convert-event-specs in-file out-file)
 */

     public String convertEventSpecs(Object inFile, Object outFile ) throws RacerClientException {
          return racerCall("convert-event-specs" , inFile, outFile ).toString();
     }

     public RacerResult convertEventSpecs$(Object inFile, Object outFile ) throws RacerClientException {
          return racerCall("convert-event-specs" , inFile, outFile );
     }

/** Racer Function copy-rules
(copy-rules from-abox
            to-abox
            &key
            type-of-substrate
            keep-old-names-p)
 */

     public String copyRules(Object fromAbox, Object toAbox ) throws RacerClientException {
          return racerCall("copy-rules" , fromAbox, toAbox ).toString();
     }

     public RacerResult copyRules$(Object fromAbox, Object toAbox ) throws RacerClientException {
          return racerCall("copy-rules" , fromAbox, toAbox );
     }

     public String copyRules(Object fromAbox, Object toAbox , Object... keyArgs) throws RacerClientException {
          return racerCall("copy-rules" , fromAbox, toAbox , keyArgs).toString();
     }

     public RacerResult copyRules$(Object fromAbox, Object toAbox , Object... keyArgs) throws RacerClientException {
          return racerCall("copy-rules" , fromAbox, toAbox , keyArgs);
     }

/** Racer Function create-abox-clone
(create-abox-clone abox
                   &key
                   new-name
                   overwrite
                   copy-rules)
 */

     public String createAboxClone(Object abox ) throws RacerClientException {
          return racerCall("create-abox-clone" , abox ).toString();
     }

     public RacerResult createAboxClone$(Object abox ) throws RacerClientException {
          return racerCall("create-abox-clone" , abox );
     }

     public String createAboxClone(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("create-abox-clone" , abox , keyArgs).toString();
     }

     public RacerResult createAboxClone$(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("create-abox-clone" , abox , keyArgs);
     }

/** Racer Function create-data-edge
(create-data-edge from
                  to
                  descr
                  &key
                  abox
                  type-of-substrate
                  racer-descr
                  told-info-p)
 */

     public String createDataEdge(Object from, Object to, Object descr ) throws RacerClientException {
          return racerCall("create-data-edge" , from, to, descr ).toString();
     }

     public RacerResult createDataEdge$(Object from, Object to, Object descr ) throws RacerClientException {
          return racerCall("create-data-edge" , from, to, descr );
     }

     public String createDataEdge(Object from, Object to, Object descr , Object... keyArgs) throws RacerClientException {
          return racerCall("create-data-edge" , from, to, descr , keyArgs).toString();
     }

     public RacerResult createDataEdge$(Object from, Object to, Object descr , Object... keyArgs) throws RacerClientException {
          return racerCall("create-data-edge" , from, to, descr , keyArgs);
     }

/** Racer Function create-data-node
(create-data-node name
                  &key
                  abox
                  type-of-substrate
                  racer-descr
                  descr
                  told-info-p)
 */

     public String createDataNode(Object name ) throws RacerClientException {
          return racerCall("create-data-node" , name ).toString();
     }

     public RacerResult createDataNode$(Object name ) throws RacerClientException {
          return racerCall("create-data-node" , name );
     }

     public String createDataNode(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("create-data-node" , name , keyArgs).toString();
     }

     public RacerResult createDataNode$(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("create-data-node" , name , keyArgs);
     }

/** Racer Function create-rcc-edge
(create-rcc-edge &key
                 abox
                 type-of-substrate
                 racer-descr
                 told-info-p)
 */

     public String createRccEdge( ) throws RacerClientException {
          return racerCall("create-rcc-edge"  ).toString();
     }

     public RacerResult createRccEdge$( ) throws RacerClientException {
          return racerCall("create-rcc-edge"  );
     }

     public String createRccEdge(  Object... keyArgs) throws RacerClientException {
          return racerCall("create-rcc-edge"  , keyArgs).toString();
     }

     public RacerResult createRccEdge$(  Object... keyArgs) throws RacerClientException {
          return racerCall("create-rcc-edge"  , keyArgs);
     }

/** Racer Function create-rcc-node
(create-rcc-node &key
                 abox
                 type-of-substrate
                 racer-descr
                 descr
                 told-info-p)
 */

     public String createRccNode( ) throws RacerClientException {
          return racerCall("create-rcc-node"  ).toString();
     }

     public RacerResult createRccNode$( ) throws RacerClientException {
          return racerCall("create-rcc-node"  );
     }

     public String createRccNode(  Object... keyArgs) throws RacerClientException {
          return racerCall("create-rcc-node"  , keyArgs).toString();
     }

     public RacerResult createRccNode$(  Object... keyArgs) throws RacerClientException {
          return racerCall("create-rcc-node"  , keyArgs);
     }

/** Racer Function create-subgraph-aboxes
(create-subgraph-aboxes abox-or-name
                        &optional
                        new-name
                        tbox)
 */

     public String createSubgraphAboxes(Object aboxOrName ) throws RacerClientException {
          return racerCall("create-subgraph-aboxes" , aboxOrName ).toString();
     }

     public RacerResult createSubgraphAboxes$(Object aboxOrName ) throws RacerClientException {
          return racerCall("create-subgraph-aboxes" , aboxOrName );
     }

     public String createSubgraphAboxes(Object aboxOrName, Object newName, Object tbox ) throws RacerClientException {
          return racerCall("create-subgraph-aboxes" , aboxOrName, newName, tbox ).toString();
     }

     public RacerResult createSubgraphAboxes$(Object aboxOrName, Object newName, Object tbox ) throws RacerClientException {
          return racerCall("create-subgraph-aboxes" , aboxOrName, newName, tbox );
     }

     public String createSubgraphAboxes(Object aboxOrName, Object newName ) throws RacerClientException {
          return racerCall("create-subgraph-aboxes" , aboxOrName, newName ).toString();
     }

     public RacerResult createSubgraphAboxes$(Object aboxOrName, Object newName ) throws RacerClientException {
          return racerCall("create-subgraph-aboxes" , aboxOrName, newName );
     }

/** Racer Function create-tbox-clone
(create-tbox-clone tbox &key new-name overwrite)
 */

     public String createTboxClone(Object tbox ) throws RacerClientException {
          return racerCall("create-tbox-clone" , tbox ).toString();
     }

     public RacerResult createTboxClone$(Object tbox ) throws RacerClientException {
          return racerCall("create-tbox-clone" , tbox );
     }

     public String createTboxClone(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("create-tbox-clone" , tbox , keyArgs).toString();
     }

     public RacerResult createTboxClone$(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("create-tbox-clone" , tbox , keyArgs);
     }

/** Racer Function create-tbox-internal-marker-concept
(create-tbox-internal-marker-concept tbox
                                     &optional
                                     marker-name)
 */

     public String createTboxInternalMarkerConcept(Object tbox ) throws RacerClientException {
          return racerCall("create-tbox-internal-marker-concept" , tbox ).toString();
     }

     public RacerResult createTboxInternalMarkerConcept$(Object tbox ) throws RacerClientException {
          return racerCall("create-tbox-internal-marker-concept" , tbox );
     }

     public String createTboxInternalMarkerConcept(Object tbox, Object markerName ) throws RacerClientException {
          return racerCall("create-tbox-internal-marker-concept" , tbox, markerName ).toString();
     }

     public RacerResult createTboxInternalMarkerConcept$(Object tbox, Object markerName ) throws RacerClientException {
          return racerCall("create-tbox-internal-marker-concept" , tbox, markerName );
     }

/** Racer Function create-triple-store
(create-triple-store name
                     &key
                     if-exists
                     directory
                     data-version-level)
 */

     public String createTripleStore(Object name ) throws RacerClientException {
          return racerCall("create-triple-store" , name ).toString();
     }

     public RacerResult createTripleStore$(Object name ) throws RacerClientException {
          return racerCall("create-triple-store" , name );
     }

     public String createTripleStore(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("create-triple-store" , name , keyArgs).toString();
     }

     public RacerResult createTripleStore$(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("create-triple-store" , name , keyArgs);
     }

/** Racer Function current-abox
(current-abox)
 */

     public String currentAbox( ) throws RacerClientException {
          return racerCall("current-abox"  ).toString();
     }

     public RacerResult currentAbox$( ) throws RacerClientException {
          return racerCall("current-abox"  );
     }

/** Racer Function current-tbox
(current-tbox)
 */

     public String currentTbox( ) throws RacerClientException {
          return racerCall("current-tbox"  ).toString();
     }

     public RacerResult currentTbox$( ) throws RacerClientException {
          return racerCall("current-tbox"  );
     }

/** Racer Function data-edge1
(data-edge1 from
            to
            data-relation
            &optional
            racer-descr
            abox
            type-of-substrate)
 */

     public String dataEdge1(Object from, Object to, Object dataRelation ) throws RacerClientException {
          return racerCall("data-edge1" , from, to, dataRelation ).toString();
     }

     public RacerResult dataEdge1$(Object from, Object to, Object dataRelation ) throws RacerClientException {
          return racerCall("data-edge1" , from, to, dataRelation );
     }

     public String dataEdge1(Object from, Object to, Object dataRelation, Object racerDescr, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("data-edge1" , from, to, dataRelation, racerDescr, abox, typeOfSubstrate ).toString();
     }

     public RacerResult dataEdge1$(Object from, Object to, Object dataRelation, Object racerDescr, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("data-edge1" , from, to, dataRelation, racerDescr, abox, typeOfSubstrate );
     }

     public String dataEdge1(Object from, Object to, Object dataRelation, Object racerDescr, Object abox ) throws RacerClientException {
          return racerCall("data-edge1" , from, to, dataRelation, racerDescr, abox ).toString();
     }

     public RacerResult dataEdge1$(Object from, Object to, Object dataRelation, Object racerDescr, Object abox ) throws RacerClientException {
          return racerCall("data-edge1" , from, to, dataRelation, racerDescr, abox );
     }

     public String dataEdge1(Object from, Object to, Object dataRelation, Object racerDescr ) throws RacerClientException {
          return racerCall("data-edge1" , from, to, dataRelation, racerDescr ).toString();
     }

     public RacerResult dataEdge1$(Object from, Object to, Object dataRelation, Object racerDescr ) throws RacerClientException {
          return racerCall("data-edge1" , from, to, dataRelation, racerDescr );
     }

/** Racer Function data-node1
(data-node1 name
            &optional
            descr
            racer-descr
            abox
            type-of-substrate)
 */

     public String dataNode1(Object name ) throws RacerClientException {
          return racerCall("data-node1" , name ).toString();
     }

     public RacerResult dataNode1$(Object name ) throws RacerClientException {
          return racerCall("data-node1" , name );
     }

     public String dataNode1(Object name, Object descr, Object racerDescr, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("data-node1" , name, descr, racerDescr, abox, typeOfSubstrate ).toString();
     }

     public RacerResult dataNode1$(Object name, Object descr, Object racerDescr, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("data-node1" , name, descr, racerDescr, abox, typeOfSubstrate );
     }

     public String dataNode1(Object name, Object descr, Object racerDescr, Object abox ) throws RacerClientException {
          return racerCall("data-node1" , name, descr, racerDescr, abox ).toString();
     }

     public RacerResult dataNode1$(Object name, Object descr, Object racerDescr, Object abox ) throws RacerClientException {
          return racerCall("data-node1" , name, descr, racerDescr, abox );
     }

     public String dataNode1(Object name, Object descr, Object racerDescr ) throws RacerClientException {
          return racerCall("data-node1" , name, descr, racerDescr ).toString();
     }

     public RacerResult dataNode1$(Object name, Object descr, Object racerDescr ) throws RacerClientException {
          return racerCall("data-node1" , name, descr, racerDescr );
     }

     public String dataNode1(Object name, Object descr ) throws RacerClientException {
          return racerCall("data-node1" , name, descr ).toString();
     }

     public RacerResult dataNode1$(Object name, Object descr ) throws RacerClientException {
          return racerCall("data-node1" , name, descr );
     }

/** Racer Function datatype-role-has-range
(datatype-role-has-range rolename range tbox)
 */

     public String datatypeRoleHasRange(Object rolename, Object range, Object tbox ) throws RacerClientException {
          return racerCall("datatype-role-has-range" , rolename, range, tbox ).toString();
     }

     public RacerResult datatypeRoleHasRange$(Object rolename, Object range, Object tbox ) throws RacerClientException {
          return racerCall("datatype-role-has-range" , rolename, range, tbox );
     }

/** Racer Function datatype-role-range
(datatype-role-range role-name tbox)
 */

     public String datatypeRoleRange(Object roleName, Object tbox ) throws RacerClientException {
          return racerCall("datatype-role-range" , roleName, tbox ).toString();
     }

     public RacerResult datatypeRoleRange$(Object roleName, Object tbox ) throws RacerClientException {
          return racerCall("datatype-role-range" , roleName, tbox );
     }

/** Racer Function deactivate-defined-query
(deactivate-defined-query name
                          arity
                          &key
                          pos
                          tbox)
 */

     public String deactivateDefinedQuery(Object name, Object arity ) throws RacerClientException {
          return racerCall("deactivate-defined-query" , name, arity ).toString();
     }

     public RacerResult deactivateDefinedQuery$(Object name, Object arity ) throws RacerClientException {
          return racerCall("deactivate-defined-query" , name, arity );
     }

     public String deactivateDefinedQuery(Object name, Object arity , Object... keyArgs) throws RacerClientException {
          return racerCall("deactivate-defined-query" , name, arity , keyArgs).toString();
     }

     public RacerResult deactivateDefinedQuery$(Object name, Object arity , Object... keyArgs) throws RacerClientException {
          return racerCall("deactivate-defined-query" , name, arity , keyArgs);
     }

/** Racer Function declare-current-knowledge-bases-as-persistent
(declare-current-knowledge-bases-as-persistent)
 */

     public String declareCurrentKnowledgeBasesAsPersistent( ) throws RacerClientException {
          return racerCall("declare-current-knowledge-bases-as-persistent"  ).toString();
     }

     public RacerResult declareCurrentKnowledgeBasesAsPersistent$( ) throws RacerClientException {
          return racerCall("declare-current-knowledge-bases-as-persistent"  );
     }

/** Racer Function declare-disjoint
(declare-disjoint concepts tbox)
 */

     public String declareDisjoint(Object concepts, Object tbox ) throws RacerClientException {
          return racerCall("declare-disjoint" , concepts, tbox ).toString();
     }

     public RacerResult declareDisjoint$(Object concepts, Object tbox ) throws RacerClientException {
          return racerCall("declare-disjoint" , concepts, tbox );
     }

/** Racer Function defcon1
(defcon1 name value)
 */

     public String defcon1(Object name, Object value ) throws RacerClientException {
          return racerCall("defcon1" , name, value ).toString();
     }

     public RacerResult defcon1$(Object name, Object value ) throws RacerClientException {
          return racerCall("defcon1" , name, value );
     }

/** Racer Function define-and-execute-query
(define-and-execute-query name
                          head
                          body
                          &key
                          keep-p
                          tbox
                          consider-head-atom-for-consistency-check-p
                          allow-multiple-definitions-p)
 */

     public String defineAndExecuteQuery(Object name, Object head, Object body ) throws RacerClientException {
          return racerCall("define-and-execute-query" , name, head, body ).toString();
     }

     public RacerResult defineAndExecuteQuery$(Object name, Object head, Object body ) throws RacerClientException {
          return racerCall("define-and-execute-query" , name, head, body );
     }

     public String defineAndExecuteQuery(Object name, Object head, Object body , Object... keyArgs) throws RacerClientException {
          return racerCall("define-and-execute-query" , name, head, body , keyArgs).toString();
     }

     public RacerResult defineAndExecuteQuery$(Object name, Object head, Object body , Object... keyArgs) throws RacerClientException {
          return racerCall("define-and-execute-query" , name, head, body , keyArgs);
     }

/** Racer Function define-and-prepare-query
(define-and-prepare-query name
                          head
                          body
                          &key
                          keep-p
                          tbox
                          consider-head-atom-for-consistency-check-p
                          allow-multiple-definitions-p)
 */

     public String defineAndPrepareQuery(Object name, Object head, Object body ) throws RacerClientException {
          return racerCall("define-and-prepare-query" , name, head, body ).toString();
     }

     public RacerResult defineAndPrepareQuery$(Object name, Object head, Object body ) throws RacerClientException {
          return racerCall("define-and-prepare-query" , name, head, body );
     }

     public String defineAndPrepareQuery(Object name, Object head, Object body , Object... keyArgs) throws RacerClientException {
          return racerCall("define-and-prepare-query" , name, head, body , keyArgs).toString();
     }

     public RacerResult defineAndPrepareQuery$(Object name, Object head, Object body , Object... keyArgs) throws RacerClientException {
          return racerCall("define-and-prepare-query" , name, head, body , keyArgs);
     }

/** Racer Function define-query
(define-query name
              head
              body
              &key
              keep-p
              tbox
              consider-head-atom-for-consistency-check-p
              allow-multiple-definitions-p)
 */

     public String defineQuery(Object name, Object head, Object body ) throws RacerClientException {
          return racerCall("define-query" , name, head, body ).toString();
     }

     public RacerResult defineQuery$(Object name, Object head, Object body ) throws RacerClientException {
          return racerCall("define-query" , name, head, body );
     }

     public String defineQuery(Object name, Object head, Object body , Object... keyArgs) throws RacerClientException {
          return racerCall("define-query" , name, head, body , keyArgs).toString();
     }

     public RacerResult defineQuery$(Object name, Object head, Object body , Object... keyArgs) throws RacerClientException {
          return racerCall("define-query" , name, head, body , keyArgs);
     }

/** Racer Function define1
(define1 name arglist)
 */

     public String define1(Object name, Object arglist ) throws RacerClientException {
          return racerCall("define1" , name, arglist ).toString();
     }

     public RacerResult define1$(Object name, Object arglist ) throws RacerClientException {
          return racerCall("define1" , name, arglist );
     }

/** Racer Function defpar1
(defpar1 name value)
 */

     public String defpar1(Object name, Object value ) throws RacerClientException {
          return racerCall("defpar1" , name, value ).toString();
     }

     public RacerResult defpar1$(Object name, Object value ) throws RacerClientException {
          return racerCall("defpar1" , name, value );
     }

/** Racer Function del-data-edge1
(del-data-edge1 from
                to
                &optional
                abox
                type-of-substrate)
 */

     public String delDataEdge1(Object from, Object to ) throws RacerClientException {
          return racerCall("del-data-edge1" , from, to ).toString();
     }

     public RacerResult delDataEdge1$(Object from, Object to ) throws RacerClientException {
          return racerCall("del-data-edge1" , from, to );
     }

     public String delDataEdge1(Object from, Object to, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("del-data-edge1" , from, to, abox, typeOfSubstrate ).toString();
     }

     public RacerResult delDataEdge1$(Object from, Object to, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("del-data-edge1" , from, to, abox, typeOfSubstrate );
     }

     public String delDataEdge1(Object from, Object to, Object abox ) throws RacerClientException {
          return racerCall("del-data-edge1" , from, to, abox ).toString();
     }

     public RacerResult delDataEdge1$(Object from, Object to, Object abox ) throws RacerClientException {
          return racerCall("del-data-edge1" , from, to, abox );
     }

/** Racer Function del-data-node1
(del-data-node1 name
                &optional
                abox
                type-of-substrate)
 */

     public String delDataNode1(Object name ) throws RacerClientException {
          return racerCall("del-data-node1" , name ).toString();
     }

     public RacerResult delDataNode1$(Object name ) throws RacerClientException {
          return racerCall("del-data-node1" , name );
     }

     public String delDataNode1(Object name, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("del-data-node1" , name, abox, typeOfSubstrate ).toString();
     }

     public RacerResult delDataNode1$(Object name, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("del-data-node1" , name, abox, typeOfSubstrate );
     }

     public String delDataNode1(Object name, Object abox ) throws RacerClientException {
          return racerCall("del-data-node1" , name, abox ).toString();
     }

     public RacerResult delDataNode1$(Object name, Object abox ) throws RacerClientException {
          return racerCall("del-data-node1" , name, abox );
     }

/** Racer Function del-doc-entry1
(del-doc-entry1 label)
 */

     public String delDocEntry1(Object label ) throws RacerClientException {
          return racerCall("del-doc-entry1" , label ).toString();
     }

     public RacerResult delDocEntry1$(Object label ) throws RacerClientException {
          return racerCall("del-doc-entry1" , label );
     }

/** Racer Function del-rcc-edge1
(del-rcc-edge1)
 */

     public String delRccEdge1( ) throws RacerClientException {
          return racerCall("del-rcc-edge1"  ).toString();
     }

     public RacerResult delRccEdge1$( ) throws RacerClientException {
          return racerCall("del-rcc-edge1"  );
     }

/** Racer Function del-rcc-node1
(del-rcc-node1)
 */

     public String delRccNode1( ) throws RacerClientException {
          return racerCall("del-rcc-node1"  ).toString();
     }

     public RacerResult delRccNode1$( ) throws RacerClientException {
          return racerCall("del-rcc-node1"  );
     }

/** Racer Function delete-all-aboxes
(delete-all-aboxes)
 */

     public String deleteAllAboxes( ) throws RacerClientException {
          return racerCall("delete-all-aboxes"  ).toString();
     }

     public RacerResult deleteAllAboxes$( ) throws RacerClientException {
          return racerCall("delete-all-aboxes"  );
     }

/** Racer Function delete-all-definitions
(delete-all-definitions &key tbox)
 */

     public String deleteAllDefinitions( ) throws RacerClientException {
          return racerCall("delete-all-definitions"  ).toString();
     }

     public RacerResult deleteAllDefinitions$( ) throws RacerClientException {
          return racerCall("delete-all-definitions"  );
     }

     public String deleteAllDefinitions(  Object... keyArgs) throws RacerClientException {
          return racerCall("delete-all-definitions"  , keyArgs).toString();
     }

     public RacerResult deleteAllDefinitions$(  Object... keyArgs) throws RacerClientException {
          return racerCall("delete-all-definitions"  , keyArgs);
     }

/** Racer Function delete-all-queries
(delete-all-queries &key
                    abox
                    type-of-substrate)
 */

     public String deleteAllQueries( ) throws RacerClientException {
          return racerCall("delete-all-queries"  ).toString();
     }

     public RacerResult deleteAllQueries$( ) throws RacerClientException {
          return racerCall("delete-all-queries"  );
     }

     public String deleteAllQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("delete-all-queries"  , keyArgs).toString();
     }

     public RacerResult deleteAllQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("delete-all-queries"  , keyArgs);
     }

/** Racer Function delete-all-rules
(delete-all-rules &key
                  abox
                  type-of-substrate)
 */

     public String deleteAllRules( ) throws RacerClientException {
          return racerCall("delete-all-rules"  ).toString();
     }

     public RacerResult deleteAllRules$( ) throws RacerClientException {
          return racerCall("delete-all-rules"  );
     }

     public String deleteAllRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("delete-all-rules"  , keyArgs).toString();
     }

     public RacerResult deleteAllRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("delete-all-rules"  , keyArgs);
     }

/** Racer Function delete-all-substrates
(delete-all-substrates &key
                       abox
                       type-of-substrate)
 */

     public String deleteAllSubstrates( ) throws RacerClientException {
          return racerCall("delete-all-substrates"  ).toString();
     }

     public RacerResult deleteAllSubstrates$( ) throws RacerClientException {
          return racerCall("delete-all-substrates"  );
     }

     public String deleteAllSubstrates(  Object... keyArgs) throws RacerClientException {
          return racerCall("delete-all-substrates"  , keyArgs).toString();
     }

     public RacerResult deleteAllSubstrates$(  Object... keyArgs) throws RacerClientException {
          return racerCall("delete-all-substrates"  , keyArgs);
     }

/** Racer Function delete-all-tboxes
(delete-all-tboxes)
 */

     public String deleteAllTboxes( ) throws RacerClientException {
          return racerCall("delete-all-tboxes"  ).toString();
     }

     public RacerResult deleteAllTboxes$( ) throws RacerClientException {
          return racerCall("delete-all-tboxes"  );
     }

/** Racer Function delete-data-edge
(delete-data-edge from
                  to
                  &key
                  abox
                  type-of-substrate
                  told-info-p)
 */

     public String deleteDataEdge(Object from, Object to ) throws RacerClientException {
          return racerCall("delete-data-edge" , from, to ).toString();
     }

     public RacerResult deleteDataEdge$(Object from, Object to ) throws RacerClientException {
          return racerCall("delete-data-edge" , from, to );
     }

     public String deleteDataEdge(Object from, Object to , Object... keyArgs) throws RacerClientException {
          return racerCall("delete-data-edge" , from, to , keyArgs).toString();
     }

     public RacerResult deleteDataEdge$(Object from, Object to , Object... keyArgs) throws RacerClientException {
          return racerCall("delete-data-edge" , from, to , keyArgs);
     }

/** Racer Function delete-data-node
(delete-data-node name
                  &key
                  abox
                  type-of-substrate
                  told-info-p)
 */

     public String deleteDataNode(Object name ) throws RacerClientException {
          return racerCall("delete-data-node" , name ).toString();
     }

     public RacerResult deleteDataNode$(Object name ) throws RacerClientException {
          return racerCall("delete-data-node" , name );
     }

     public String deleteDataNode(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("delete-data-node" , name , keyArgs).toString();
     }

     public RacerResult deleteDataNode$(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("delete-data-node" , name , keyArgs);
     }

/** Racer Function delete-prefix-mappings
(delete-prefix-mappings)
 */

     public String deletePrefixMappings( ) throws RacerClientException {
          return racerCall("delete-prefix-mappings"  ).toString();
     }

     public RacerResult deletePrefixMappings$( ) throws RacerClientException {
          return racerCall("delete-prefix-mappings"  );
     }

/** Racer Function delete-query
(delete-query query)
 */

     public String deleteQuery(Object query ) throws RacerClientException {
          return racerCall("delete-query" , query ).toString();
     }

     public RacerResult deleteQuery$(Object query ) throws RacerClientException {
          return racerCall("delete-query" , query );
     }

/** Racer Function delete-rcc-synonyms
(delete-rcc-synonyms)
 */

     public String deleteRccSynonyms( ) throws RacerClientException {
          return racerCall("delete-rcc-synonyms"  ).toString();
     }

     public RacerResult deleteRccSynonyms$( ) throws RacerClientException {
          return racerCall("delete-rcc-synonyms"  );
     }

/** Racer Function delete-rule
(delete-rule query)
 */

     public String deleteRule(Object query ) throws RacerClientException {
          return racerCall("delete-rule" , query ).toString();
     }

     public RacerResult deleteRule$(Object query ) throws RacerClientException {
          return racerCall("delete-rule" , query );
     }

/** Racer Function describe-abox
(describe-abox &optional abox stream)
 */

     public String describeAbox( ) throws RacerClientException {
          return racerCall("describe-abox"  ).toString();
     }

     public RacerResult describeAbox$( ) throws RacerClientException {
          return racerCall("describe-abox"  );
     }

     public String describeAbox(Object abox, Object stream ) throws RacerClientException {
          return racerCall("describe-abox" , abox, stream ).toString();
     }

     public RacerResult describeAbox$(Object abox, Object stream ) throws RacerClientException {
          return racerCall("describe-abox" , abox, stream );
     }

     public String describeAbox(Object abox ) throws RacerClientException {
          return racerCall("describe-abox" , abox ).toString();
     }

     public RacerResult describeAbox$(Object abox ) throws RacerClientException {
          return racerCall("describe-abox" , abox );
     }

/** Racer Function describe-all-definitions
(describe-all-definitions &key
                          tbox
                          error-p)
 */

     public String describeAllDefinitions( ) throws RacerClientException {
          return racerCall("describe-all-definitions"  ).toString();
     }

     public RacerResult describeAllDefinitions$( ) throws RacerClientException {
          return racerCall("describe-all-definitions"  );
     }

     public String describeAllDefinitions(  Object... keyArgs) throws RacerClientException {
          return racerCall("describe-all-definitions"  , keyArgs).toString();
     }

     public RacerResult describeAllDefinitions$(  Object... keyArgs) throws RacerClientException {
          return racerCall("describe-all-definitions"  , keyArgs);
     }

/** Racer Function describe-all-edges
(describe-all-edges &key
                    abox
                    type-of-substrate)
 */

     public String describeAllEdges( ) throws RacerClientException {
          return racerCall("describe-all-edges"  ).toString();
     }

     public RacerResult describeAllEdges$( ) throws RacerClientException {
          return racerCall("describe-all-edges"  );
     }

     public String describeAllEdges(  Object... keyArgs) throws RacerClientException {
          return racerCall("describe-all-edges"  , keyArgs).toString();
     }

     public RacerResult describeAllEdges$(  Object... keyArgs) throws RacerClientException {
          return racerCall("describe-all-edges"  , keyArgs);
     }

/** Racer Function describe-all-nodes
(describe-all-nodes &key
                    abox
                    type-of-substrate)
 */

     public String describeAllNodes( ) throws RacerClientException {
          return racerCall("describe-all-nodes"  ).toString();
     }

     public RacerResult describeAllNodes$( ) throws RacerClientException {
          return racerCall("describe-all-nodes"  );
     }

     public String describeAllNodes(  Object... keyArgs) throws RacerClientException {
          return racerCall("describe-all-nodes"  , keyArgs).toString();
     }

     public RacerResult describeAllNodes$(  Object... keyArgs) throws RacerClientException {
          return racerCall("describe-all-nodes"  , keyArgs);
     }

/** Racer Function describe-all-queries
(describe-all-queries &optional
                      rewritten-p
                      &key
                      abox
                      type-of-substrate)
 */

     public String describeAllQueries( ) throws RacerClientException {
          return racerCall("describe-all-queries"  ).toString();
     }

     public RacerResult describeAllQueries$( ) throws RacerClientException {
          return racerCall("describe-all-queries"  );
     }

     public String describeAllQueries(Object rewrittenP ) throws RacerClientException {
          return racerCall("describe-all-queries" , rewrittenP ).toString();
     }

     public RacerResult describeAllQueries$(Object rewrittenP ) throws RacerClientException {
          return racerCall("describe-all-queries" , rewrittenP );
     }

     public String describeAllQueries(Object rewrittenP , Object... keyArgs) throws RacerClientException {
          return racerCall("describe-all-queries" , rewrittenP , keyArgs).toString();
     }

     public RacerResult describeAllQueries$(Object rewrittenP , Object... keyArgs) throws RacerClientException {
          return racerCall("describe-all-queries" , rewrittenP , keyArgs);
     }

/** Racer Function describe-all-rules
(describe-all-rules &optional
                    rewritten-p
                    &key
                    abox
                    type-of-substrate)
 */

     public String describeAllRules( ) throws RacerClientException {
          return racerCall("describe-all-rules"  ).toString();
     }

     public RacerResult describeAllRules$( ) throws RacerClientException {
          return racerCall("describe-all-rules"  );
     }

     public String describeAllRules(Object rewrittenP ) throws RacerClientException {
          return racerCall("describe-all-rules" , rewrittenP ).toString();
     }

     public RacerResult describeAllRules$(Object rewrittenP ) throws RacerClientException {
          return racerCall("describe-all-rules" , rewrittenP );
     }

     public String describeAllRules(Object rewrittenP , Object... keyArgs) throws RacerClientException {
          return racerCall("describe-all-rules" , rewrittenP , keyArgs).toString();
     }

     public RacerResult describeAllRules$(Object rewrittenP , Object... keyArgs) throws RacerClientException {
          return racerCall("describe-all-rules" , rewrittenP , keyArgs);
     }

/** Racer Function describe-all-substrates
(describe-all-substrates)
 */

     public String describeAllSubstrates( ) throws RacerClientException {
          return racerCall("describe-all-substrates"  ).toString();
     }

     public RacerResult describeAllSubstrates$( ) throws RacerClientException {
          return racerCall("describe-all-substrates"  );
     }

/** Racer Function describe-concept
(describe-concept concept-name &optional tbox stream)
 */

     public String describeConcept(Object conceptName ) throws RacerClientException {
          return racerCall("describe-concept" , conceptName ).toString();
     }

     public RacerResult describeConcept$(Object conceptName ) throws RacerClientException {
          return racerCall("describe-concept" , conceptName );
     }

     public String describeConcept(Object conceptName, Object tbox, Object stream ) throws RacerClientException {
          return racerCall("describe-concept" , conceptName, tbox, stream ).toString();
     }

     public RacerResult describeConcept$(Object conceptName, Object tbox, Object stream ) throws RacerClientException {
          return racerCall("describe-concept" , conceptName, tbox, stream );
     }

     public String describeConcept(Object conceptName, Object tbox ) throws RacerClientException {
          return racerCall("describe-concept" , conceptName, tbox ).toString();
     }

     public RacerResult describeConcept$(Object conceptName, Object tbox ) throws RacerClientException {
          return racerCall("describe-concept" , conceptName, tbox );
     }

/** Racer Function describe-current-substrate
(describe-current-substrate)
 */

     public String describeCurrentSubstrate( ) throws RacerClientException {
          return racerCall("describe-current-substrate"  ).toString();
     }

     public RacerResult describeCurrentSubstrate$( ) throws RacerClientException {
          return racerCall("describe-current-substrate"  );
     }

/** Racer Function describe-definition
(describe-definition name
                     &key
                     tbox
                     arity
                     error-p)
 */

     public String describeDefinition(Object name ) throws RacerClientException {
          return racerCall("describe-definition" , name ).toString();
     }

     public RacerResult describeDefinition$(Object name ) throws RacerClientException {
          return racerCall("describe-definition" , name );
     }

     public String describeDefinition(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("describe-definition" , name , keyArgs).toString();
     }

     public RacerResult describeDefinition$(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("describe-definition" , name , keyArgs);
     }

/** Racer Function describe-individual
(describe-individual individual-name
                     &optional
                     abox
                     stream)
 */

     public String describeIndividual(Object individualName ) throws RacerClientException {
          return racerCall("describe-individual" , individualName ).toString();
     }

     public RacerResult describeIndividual$(Object individualName ) throws RacerClientException {
          return racerCall("describe-individual" , individualName );
     }

     public String describeIndividual(Object individualName, Object abox, Object stream ) throws RacerClientException {
          return racerCall("describe-individual" , individualName, abox, stream ).toString();
     }

     public RacerResult describeIndividual$(Object individualName, Object abox, Object stream ) throws RacerClientException {
          return racerCall("describe-individual" , individualName, abox, stream );
     }

     public String describeIndividual(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("describe-individual" , individualName, abox ).toString();
     }

     public RacerResult describeIndividual$(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("describe-individual" , individualName, abox );
     }

/** Racer Function describe-individual1
(describe-individual1 individual-name
                      &optional
                      abox
                      stream)
 */

     public String describeIndividual1(Object individualName ) throws RacerClientException {
          return racerCall("describe-individual1" , individualName ).toString();
     }

     public RacerResult describeIndividual1$(Object individualName ) throws RacerClientException {
          return racerCall("describe-individual1" , individualName );
     }

     public String describeIndividual1(Object individualName, Object abox, Object stream ) throws RacerClientException {
          return racerCall("describe-individual1" , individualName, abox, stream ).toString();
     }

     public RacerResult describeIndividual1$(Object individualName, Object abox, Object stream ) throws RacerClientException {
          return racerCall("describe-individual1" , individualName, abox, stream );
     }

     public String describeIndividual1(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("describe-individual1" , individualName, abox ).toString();
     }

     public RacerResult describeIndividual1$(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("describe-individual1" , individualName, abox );
     }

/** Racer Function describe-query
(describe-query query
                &optional
                rewritten-p)
 */

     public String describeQuery(Object query ) throws RacerClientException {
          return racerCall("describe-query" , query ).toString();
     }

     public RacerResult describeQuery$(Object query ) throws RacerClientException {
          return racerCall("describe-query" , query );
     }

     public String describeQuery(Object query, Object rewrittenP ) throws RacerClientException {
          return racerCall("describe-query" , query, rewrittenP ).toString();
     }

     public RacerResult describeQuery$(Object query, Object rewrittenP ) throws RacerClientException {
          return racerCall("describe-query" , query, rewrittenP );
     }

/** Racer Function describe-query-processing-mode
(describe-query-processing-mode)
 */

     public String describeQueryProcessingMode( ) throws RacerClientException {
          return racerCall("describe-query-processing-mode"  ).toString();
     }

     public RacerResult describeQueryProcessingMode$( ) throws RacerClientException {
          return racerCall("describe-query-processing-mode"  );
     }

/** Racer Function describe-query-status
(describe-query-status query)
 */

     public String describeQueryStatus(Object query ) throws RacerClientException {
          return racerCall("describe-query-status" , query ).toString();
     }

     public RacerResult describeQueryStatus$(Object query ) throws RacerClientException {
          return racerCall("describe-query-status" , query );
     }

/** Racer Function describe-role
(describe-role role-term &optional tbox stream)
 */

     public String describeRole(Object roleTerm ) throws RacerClientException {
          return racerCall("describe-role" , roleTerm ).toString();
     }

     public RacerResult describeRole$(Object roleTerm ) throws RacerClientException {
          return racerCall("describe-role" , roleTerm );
     }

     public String describeRole(Object roleTerm, Object tbox, Object stream ) throws RacerClientException {
          return racerCall("describe-role" , roleTerm, tbox, stream ).toString();
     }

     public RacerResult describeRole$(Object roleTerm, Object tbox, Object stream ) throws RacerClientException {
          return racerCall("describe-role" , roleTerm, tbox, stream );
     }

     public String describeRole(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("describe-role" , roleTerm, tbox ).toString();
     }

     public RacerResult describeRole$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("describe-role" , roleTerm, tbox );
     }

/** Racer Function describe-rule
(describe-rule query
               &optional
               rewritten-p)
 */

     public String describeRule(Object query ) throws RacerClientException {
          return racerCall("describe-rule" , query ).toString();
     }

     public RacerResult describeRule$(Object query ) throws RacerClientException {
          return racerCall("describe-rule" , query );
     }

     public String describeRule(Object query, Object rewrittenP ) throws RacerClientException {
          return racerCall("describe-rule" , query, rewrittenP ).toString();
     }

     public RacerResult describeRule$(Object query, Object rewrittenP ) throws RacerClientException {
          return racerCall("describe-rule" , query, rewrittenP );
     }

/** Racer Function describe-rule-status
(describe-rule-status query)
 */

     public String describeRuleStatus(Object query ) throws RacerClientException {
          return racerCall("describe-rule-status" , query ).toString();
     }

     public RacerResult describeRuleStatus$(Object query ) throws RacerClientException {
          return racerCall("describe-rule-status" , query );
     }

/** Racer Function describe-substrate
(describe-substrate &key
                    abox
                    type-of-substrate)
 */

     public String describeSubstrate( ) throws RacerClientException {
          return racerCall("describe-substrate"  ).toString();
     }

     public RacerResult describeSubstrate$( ) throws RacerClientException {
          return racerCall("describe-substrate"  );
     }

     public String describeSubstrate(  Object... keyArgs) throws RacerClientException {
          return racerCall("describe-substrate"  , keyArgs).toString();
     }

     public RacerResult describeSubstrate$(  Object... keyArgs) throws RacerClientException {
          return racerCall("describe-substrate"  , keyArgs);
     }

/** Racer Function describe-tbox
(describe-tbox &optional tbox stream)
 */

     public String describeTbox( ) throws RacerClientException {
          return racerCall("describe-tbox"  ).toString();
     }

     public RacerResult describeTbox$( ) throws RacerClientException {
          return racerCall("describe-tbox"  );
     }

     public String describeTbox(Object tbox, Object stream ) throws RacerClientException {
          return racerCall("describe-tbox" , tbox, stream ).toString();
     }

     public RacerResult describeTbox$(Object tbox, Object stream ) throws RacerClientException {
          return racerCall("describe-tbox" , tbox, stream );
     }

     public String describeTbox(Object tbox ) throws RacerClientException {
          return racerCall("describe-tbox" , tbox ).toString();
     }

     public RacerResult describeTbox$(Object tbox ) throws RacerClientException {
          return racerCall("describe-tbox" , tbox );
     }

/** Racer Function description-implies-p
(description-implies-p a b)
 */

     public boolean descriptionImpliesP(Object a, Object b ) throws RacerClientException {
          return returnBoolean(racerCall("description-implies-p" , a, b ));
     }

/** Racer Function dig-read-document
(dig-read-document url-spec &optional kb-name init)
 */

     public String digReadDocument(Object urlSpec ) throws RacerClientException {
          return racerCall("dig-read-document" , urlSpec ).toString();
     }

     public RacerResult digReadDocument$(Object urlSpec ) throws RacerClientException {
          return racerCall("dig-read-document" , urlSpec );
     }

     public String digReadDocument(Object urlSpec, Object kbName, Object init ) throws RacerClientException {
          return racerCall("dig-read-document" , urlSpec, kbName, init ).toString();
     }

     public RacerResult digReadDocument$(Object urlSpec, Object kbName, Object init ) throws RacerClientException {
          return racerCall("dig-read-document" , urlSpec, kbName, init );
     }

     public String digReadDocument(Object urlSpec, Object kbName ) throws RacerClientException {
          return racerCall("dig-read-document" , urlSpec, kbName ).toString();
     }

     public RacerResult digReadDocument$(Object urlSpec, Object kbName ) throws RacerClientException {
          return racerCall("dig-read-document" , urlSpec, kbName );
     }

/** Racer Function dig-read-file
(dig-read-file filename &key kb-name init)
 */

     public String digReadFile(Object filename ) throws RacerClientException {
          return racerCall("dig-read-file" , filename ).toString();
     }

     public RacerResult digReadFile$(Object filename ) throws RacerClientException {
          return racerCall("dig-read-file" , filename );
     }

     public String digReadFile(Object filename , Object... keyArgs) throws RacerClientException {
          return racerCall("dig-read-file" , filename , keyArgs).toString();
     }

     public RacerResult digReadFile$(Object filename , Object... keyArgs) throws RacerClientException {
          return racerCall("dig-read-file" , filename , keyArgs);
     }

/** Racer Function disable-abduction
(disable-abduction &key reset-p)
 */

     public String disableAbduction( ) throws RacerClientException {
          return racerCall("disable-abduction"  ).toString();
     }

     public RacerResult disableAbduction$( ) throws RacerClientException {
          return racerCall("disable-abduction"  );
     }

     public String disableAbduction(  Object... keyArgs) throws RacerClientException {
          return racerCall("disable-abduction"  , keyArgs).toString();
     }

     public RacerResult disableAbduction$(  Object... keyArgs) throws RacerClientException {
          return racerCall("disable-abduction"  , keyArgs);
     }

/** Racer Function disable-abox-mirroring
(disable-abox-mirroring)
 */

     public String disableAboxMirroring( ) throws RacerClientException {
          return racerCall("disable-abox-mirroring"  ).toString();
     }

     public RacerResult disableAboxMirroring$( ) throws RacerClientException {
          return racerCall("disable-abox-mirroring"  );
     }

/** Racer Function disable-data-substrate-mirroring
(disable-data-substrate-mirroring)
 */

     public String disableDataSubstrateMirroring( ) throws RacerClientException {
          return racerCall("disable-data-substrate-mirroring"  ).toString();
     }

     public RacerResult disableDataSubstrateMirroring$( ) throws RacerClientException {
          return racerCall("disable-data-substrate-mirroring"  );
     }

/** Racer Function disable-defined-queries
(disable-defined-queries)
 */

     public String disableDefinedQueries( ) throws RacerClientException {
          return racerCall("disable-defined-queries"  ).toString();
     }

     public RacerResult disableDefinedQueries$( ) throws RacerClientException {
          return racerCall("disable-defined-queries"  );
     }

/** Racer Function disable-kb-has-changed-warning-tokens
(disable-kb-has-changed-warning-tokens)
 */

     public String disableKbHasChangedWarningTokens( ) throws RacerClientException {
          return racerCall("disable-kb-has-changed-warning-tokens"  ).toString();
     }

     public RacerResult disableKbHasChangedWarningTokens$( ) throws RacerClientException {
          return racerCall("disable-kb-has-changed-warning-tokens"  );
     }

/** Racer Function disable-lazy-unfolding-of-defined-queries
(disable-lazy-unfolding-of-defined-queries)
 */

     public String disableLazyUnfoldingOfDefinedQueries( ) throws RacerClientException {
          return racerCall("disable-lazy-unfolding-of-defined-queries"  ).toString();
     }

     public RacerResult disableLazyUnfoldingOfDefinedQueries$( ) throws RacerClientException {
          return racerCall("disable-lazy-unfolding-of-defined-queries"  );
     }

/** Racer Function disable-nrql-warnings
(disable-nrql-warnings)
 */

     public String disableNrqlWarnings( ) throws RacerClientException {
          return racerCall("disable-nrql-warnings"  ).toString();
     }

     public RacerResult disableNrqlWarnings$( ) throws RacerClientException {
          return racerCall("disable-nrql-warnings"  );
     }

/** Racer Function disable-phase-two-starts-warning-tokens
(disable-phase-two-starts-warning-tokens)
 */

     public String disablePhaseTwoStartsWarningTokens( ) throws RacerClientException {
          return racerCall("disable-phase-two-starts-warning-tokens"  ).toString();
     }

     public RacerResult disablePhaseTwoStartsWarningTokens$( ) throws RacerClientException {
          return racerCall("disable-phase-two-starts-warning-tokens"  );
     }

/** Racer Function disable-query-optimization
(disable-query-optimization)
 */

     public String disableQueryOptimization( ) throws RacerClientException {
          return racerCall("disable-query-optimization"  ).toString();
     }

     public RacerResult disableQueryOptimization$( ) throws RacerClientException {
          return racerCall("disable-query-optimization"  );
     }

/** Racer Function disable-query-realization
(disable-query-realization)
 */

     public String disableQueryRealization( ) throws RacerClientException {
          return racerCall("disable-query-realization"  ).toString();
     }

     public RacerResult disableQueryRealization$( ) throws RacerClientException {
          return racerCall("disable-query-realization"  );
     }

/** Racer Function disable-query-repository
(disable-query-repository)
 */

     public String disableQueryRepository( ) throws RacerClientException {
          return racerCall("disable-query-repository"  ).toString();
     }

     public RacerResult disableQueryRepository$( ) throws RacerClientException {
          return racerCall("disable-query-repository"  );
     }

/** Racer Function disable-rcc-substrate-mirroring
(disable-rcc-substrate-mirroring)
 */

     public String disableRccSubstrateMirroring( ) throws RacerClientException {
          return racerCall("disable-rcc-substrate-mirroring"  ).toString();
     }

     public RacerResult disableRccSubstrateMirroring$( ) throws RacerClientException {
          return racerCall("disable-rcc-substrate-mirroring"  );
     }

/** Racer Function disable-told-information-querying
(disable-told-information-querying)
 */

     public String disableToldInformationQuerying( ) throws RacerClientException {
          return racerCall("disable-told-information-querying"  ).toString();
     }

     public RacerResult disableToldInformationQuerying$( ) throws RacerClientException {
          return racerCall("disable-told-information-querying"  );
     }

/** Racer Function disable-two-phase-query-processing-mode
(disable-two-phase-query-processing-mode)
 */

     public String disableTwoPhaseQueryProcessingMode( ) throws RacerClientException {
          return racerCall("disable-two-phase-query-processing-mode"  ).toString();
     }

     public RacerResult disableTwoPhaseQueryProcessingMode$( ) throws RacerClientException {
          return racerCall("disable-two-phase-query-processing-mode"  );
     }

/** Racer Function dont-add-missing-top-conjuncts
(dont-add-missing-top-conjuncts)
 */

     public String dontAddMissingTopConjuncts( ) throws RacerClientException {
          return racerCall("dont-add-missing-top-conjuncts"  ).toString();
     }

     public RacerResult dontAddMissingTopConjuncts$( ) throws RacerClientException {
          return racerCall("dont-add-missing-top-conjuncts"  );
     }

/** Racer Function dont-add-role-assertions-for-datatype-properties
(dont-add-role-assertions-for-datatype-properties)
 */

     public String dontAddRoleAssertionsForDatatypeProperties( ) throws RacerClientException {
          return racerCall("dont-add-role-assertions-for-datatype-properties"  ).toString();
     }

     public RacerResult dontAddRoleAssertionsForDatatypeProperties$( ) throws RacerClientException {
          return racerCall("dont-add-role-assertions-for-datatype-properties"  );
     }

/** Racer Function dont-add-rule-consequences-automatically
(dont-add-rule-consequences-automatically)
 */

     public String dontAddRuleConsequencesAutomatically( ) throws RacerClientException {
          return racerCall("dont-add-rule-consequences-automatically"  ).toString();
     }

     public RacerResult dontAddRuleConsequencesAutomatically$( ) throws RacerClientException {
          return racerCall("dont-add-rule-consequences-automatically"  );
     }

/** Racer Function dont-allow-overloaded-definitions
(dont-allow-overloaded-definitions)
 */

     public String dontAllowOverloadedDefinitions( ) throws RacerClientException {
          return racerCall("dont-allow-overloaded-definitions"  ).toString();
     }

     public RacerResult dontAllowOverloadedDefinitions$( ) throws RacerClientException {
          return racerCall("dont-allow-overloaded-definitions"  );
     }

/** Racer Function dont-check-abox-consistency-before-querying
(dont-check-abox-consistency-before-querying)
 */

     public String dontCheckAboxConsistencyBeforeQuerying( ) throws RacerClientException {
          return racerCall("dont-check-abox-consistency-before-querying"  ).toString();
     }

     public RacerResult dontCheckAboxConsistencyBeforeQuerying$( ) throws RacerClientException {
          return racerCall("dont-check-abox-consistency-before-querying"  );
     }

/** Racer Function dont-keep-defined-query-atoms
(dont-keep-defined-query-atoms)
 */

     public String dontKeepDefinedQueryAtoms( ) throws RacerClientException {
          return racerCall("dont-keep-defined-query-atoms"  ).toString();
     }

     public RacerResult dontKeepDefinedQueryAtoms$( ) throws RacerClientException {
          return racerCall("dont-keep-defined-query-atoms"  );
     }

/** Racer Function dont-prefer-defined-queries
(dont-prefer-defined-queries)
 */

     public String dontPreferDefinedQueries( ) throws RacerClientException {
          return racerCall("dont-prefer-defined-queries"  ).toString();
     }

     public RacerResult dontPreferDefinedQueries$( ) throws RacerClientException {
          return racerCall("dont-prefer-defined-queries"  );
     }

/** Racer Function dont-report-inconsistent-queries-and-rules
(dont-report-inconsistent-queries-and-rules)
 */

     public String dontReportInconsistentQueriesAndRules( ) throws RacerClientException {
          return racerCall("dont-report-inconsistent-queries-and-rules"  ).toString();
     }

     public RacerResult dontReportInconsistentQueriesAndRules$( ) throws RacerClientException {
          return racerCall("dont-report-inconsistent-queries-and-rules"  );
     }

/** Racer Function dont-use-individual-synonym-equivalence-classes
(dont-use-individual-synonym-equivalence-classes)
 */

     public String dontUseIndividualSynonymEquivalenceClasses( ) throws RacerClientException {
          return racerCall("dont-use-individual-synonym-equivalence-classes"  ).toString();
     }

     public RacerResult dontUseIndividualSynonymEquivalenceClasses$( ) throws RacerClientException {
          return racerCall("dont-use-individual-synonym-equivalence-classes"  );
     }

/** Racer Function dont-use-injective-variables-by-default
(dont-use-injective-variables-by-default)
 */

     public String dontUseInjectiveVariablesByDefault( ) throws RacerClientException {
          return racerCall("dont-use-injective-variables-by-default"  ).toString();
     }

     public RacerResult dontUseInjectiveVariablesByDefault$( ) throws RacerClientException {
          return racerCall("dont-use-injective-variables-by-default"  );
     }

/** Racer Function edge-description1
(edge-description1 from
                   to
                   &optional
                   abox
                   type-of-substrate)
 */

     public String edgeDescription1(Object from, Object to ) throws RacerClientException {
          return racerCall("edge-description1" , from, to ).toString();
     }

     public RacerResult edgeDescription1$(Object from, Object to ) throws RacerClientException {
          return racerCall("edge-description1" , from, to );
     }

     public String edgeDescription1(Object from, Object to, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("edge-description1" , from, to, abox, typeOfSubstrate ).toString();
     }

     public RacerResult edgeDescription1$(Object from, Object to, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("edge-description1" , from, to, abox, typeOfSubstrate );
     }

     public String edgeDescription1(Object from, Object to, Object abox ) throws RacerClientException {
          return racerCall("edge-description1" , from, to, abox ).toString();
     }

     public RacerResult edgeDescription1$(Object from, Object to, Object abox ) throws RacerClientException {
          return racerCall("edge-description1" , from, to, abox );
     }

/** Racer Function edge-label1
(edge-label1 from
             to
             &optional
             abox
             type-of-substrate)
 */

     public String edgeLabel1(Object from, Object to ) throws RacerClientException {
          return racerCall("edge-label1" , from, to ).toString();
     }

     public RacerResult edgeLabel1$(Object from, Object to ) throws RacerClientException {
          return racerCall("edge-label1" , from, to );
     }

     public String edgeLabel1(Object from, Object to, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("edge-label1" , from, to, abox, typeOfSubstrate ).toString();
     }

     public RacerResult edgeLabel1$(Object from, Object to, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("edge-label1" , from, to, abox, typeOfSubstrate );
     }

     public String edgeLabel1(Object from, Object to, Object abox ) throws RacerClientException {
          return racerCall("edge-label1" , from, to, abox ).toString();
     }

     public RacerResult edgeLabel1$(Object from, Object to, Object abox ) throws RacerClientException {
          return racerCall("edge-label1" , from, to, abox );
     }

/** Racer Function enable-abduction
(enable-abduction c-mode
                  r-mode
                  &key
                  reset-p
                  hypo-mode-stack
                  runtime-consistency-checking-p
                  final-consistency-checking-p
                  ensure-permutations-p
                  same-as-only-p
                  how-many
                  candidate-individuals
                  binding-validator
                  cutoff-fn
                  order-by
                  only-best-p
                  reverse-order-p)
 */

     public String enableAbduction(Object cMode, Object rMode ) throws RacerClientException {
          return racerCall("enable-abduction" , cMode, rMode ).toString();
     }

     public RacerResult enableAbduction$(Object cMode, Object rMode ) throws RacerClientException {
          return racerCall("enable-abduction" , cMode, rMode );
     }

     public String enableAbduction(Object cMode, Object rMode , Object... keyArgs) throws RacerClientException {
          return racerCall("enable-abduction" , cMode, rMode , keyArgs).toString();
     }

     public RacerResult enableAbduction$(Object cMode, Object rMode , Object... keyArgs) throws RacerClientException {
          return racerCall("enable-abduction" , cMode, rMode , keyArgs);
     }

/** Racer Function enable-abox-mirroring
(enable-abox-mirroring)
 */

     public String enableAboxMirroring( ) throws RacerClientException {
          return racerCall("enable-abox-mirroring"  ).toString();
     }

     public RacerResult enableAboxMirroring$( ) throws RacerClientException {
          return racerCall("enable-abox-mirroring"  );
     }

/** Racer Function enable-data-substrate-mirroring
(enable-data-substrate-mirroring)
 */

     public String enableDataSubstrateMirroring( ) throws RacerClientException {
          return racerCall("enable-data-substrate-mirroring"  ).toString();
     }

     public RacerResult enableDataSubstrateMirroring$( ) throws RacerClientException {
          return racerCall("enable-data-substrate-mirroring"  );
     }

/** Racer Function enable-defined-queries
(enable-defined-queries)
 */

     public String enableDefinedQueries( ) throws RacerClientException {
          return racerCall("enable-defined-queries"  ).toString();
     }

     public RacerResult enableDefinedQueries$( ) throws RacerClientException {
          return racerCall("enable-defined-queries"  );
     }

/** Racer Function enable-eager-tuple-computation
(enable-eager-tuple-computation)
 */

     public String enableEagerTupleComputation( ) throws RacerClientException {
          return racerCall("enable-eager-tuple-computation"  ).toString();
     }

     public RacerResult enableEagerTupleComputation$( ) throws RacerClientException {
          return racerCall("enable-eager-tuple-computation"  );
     }

/** Racer Function enable-kb-has-changed-warning-tokens
(enable-kb-has-changed-warning-tokens)
 */

     public String enableKbHasChangedWarningTokens( ) throws RacerClientException {
          return racerCall("enable-kb-has-changed-warning-tokens"  ).toString();
     }

     public RacerResult enableKbHasChangedWarningTokens$( ) throws RacerClientException {
          return racerCall("enable-kb-has-changed-warning-tokens"  );
     }

/** Racer Function enable-lazy-tuple-computation
(enable-lazy-tuple-computation)
 */

     public String enableLazyTupleComputation( ) throws RacerClientException {
          return racerCall("enable-lazy-tuple-computation"  ).toString();
     }

     public RacerResult enableLazyTupleComputation$( ) throws RacerClientException {
          return racerCall("enable-lazy-tuple-computation"  );
     }

/** Racer Function enable-lazy-unfolding-of-defined-queries
(enable-lazy-unfolding-of-defined-queries)
 */

     public String enableLazyUnfoldingOfDefinedQueries( ) throws RacerClientException {
          return racerCall("enable-lazy-unfolding-of-defined-queries"  ).toString();
     }

     public RacerResult enableLazyUnfoldingOfDefinedQueries$( ) throws RacerClientException {
          return racerCall("enable-lazy-unfolding-of-defined-queries"  );
     }

/** Racer Function enable-nrql-warnings
(enable-nrql-warnings)
 */

     public String enableNrqlWarnings( ) throws RacerClientException {
          return racerCall("enable-nrql-warnings"  ).toString();
     }

     public RacerResult enableNrqlWarnings$( ) throws RacerClientException {
          return racerCall("enable-nrql-warnings"  );
     }

/** Racer Function enable-optimized-query-processing
(enable-optimized-query-processing &optional
                                   rewrite-concept-definitions)
 */

     public String enableOptimizedQueryProcessing( ) throws RacerClientException {
          return racerCall("enable-optimized-query-processing"  ).toString();
     }

     public RacerResult enableOptimizedQueryProcessing$( ) throws RacerClientException {
          return racerCall("enable-optimized-query-processing"  );
     }

     public String enableOptimizedQueryProcessing(Object rewriteConceptDefinitions ) throws RacerClientException {
          return racerCall("enable-optimized-query-processing" , rewriteConceptDefinitions ).toString();
     }

     public RacerResult enableOptimizedQueryProcessing$(Object rewriteConceptDefinitions ) throws RacerClientException {
          return racerCall("enable-optimized-query-processing" , rewriteConceptDefinitions );
     }

/** Racer Function enable-phase-two-starts-warning-tokens
(enable-phase-two-starts-warning-tokens)
 */

     public String enablePhaseTwoStartsWarningTokens( ) throws RacerClientException {
          return racerCall("enable-phase-two-starts-warning-tokens"  ).toString();
     }

     public RacerResult enablePhaseTwoStartsWarningTokens$( ) throws RacerClientException {
          return racerCall("enable-phase-two-starts-warning-tokens"  );
     }

/** Racer Function enable-query-optimization
(enable-query-optimization)
 */

     public String enableQueryOptimization( ) throws RacerClientException {
          return racerCall("enable-query-optimization"  ).toString();
     }

     public RacerResult enableQueryOptimization$( ) throws RacerClientException {
          return racerCall("enable-query-optimization"  );
     }

/** Racer Function enable-query-realization
(enable-query-realization)
 */

     public String enableQueryRealization( ) throws RacerClientException {
          return racerCall("enable-query-realization"  ).toString();
     }

     public RacerResult enableQueryRealization$( ) throws RacerClientException {
          return racerCall("enable-query-realization"  );
     }

/** Racer Function enable-query-repository
(enable-query-repository)
 */

     public String enableQueryRepository( ) throws RacerClientException {
          return racerCall("enable-query-repository"  ).toString();
     }

     public RacerResult enableQueryRepository$( ) throws RacerClientException {
          return racerCall("enable-query-repository"  );
     }

/** Racer Function enable-rcc-substrate-mirroring
(enable-rcc-substrate-mirroring)
 */

     public String enableRccSubstrateMirroring( ) throws RacerClientException {
          return racerCall("enable-rcc-substrate-mirroring"  ).toString();
     }

     public RacerResult enableRccSubstrateMirroring$( ) throws RacerClientException {
          return racerCall("enable-rcc-substrate-mirroring"  );
     }

/** Racer Function enable-smart-abox-mirroring
(enable-smart-abox-mirroring)
 */

     public String enableSmartAboxMirroring( ) throws RacerClientException {
          return racerCall("enable-smart-abox-mirroring"  ).toString();
     }

     public RacerResult enableSmartAboxMirroring$( ) throws RacerClientException {
          return racerCall("enable-smart-abox-mirroring"  );
     }

/** Racer Function enable-told-information-querying
(enable-told-information-querying)
 */

     public String enableToldInformationQuerying( ) throws RacerClientException {
          return racerCall("enable-told-information-querying"  ).toString();
     }

     public RacerResult enableToldInformationQuerying$( ) throws RacerClientException {
          return racerCall("enable-told-information-querying"  );
     }

/** Racer Function enable-two-phase-query-processing-mode
(enable-two-phase-query-processing-mode)
 */

     public String enableTwoPhaseQueryProcessingMode( ) throws RacerClientException {
          return racerCall("enable-two-phase-query-processing-mode"  ).toString();
     }

     public RacerResult enableTwoPhaseQueryProcessingMode$( ) throws RacerClientException {
          return racerCall("enable-two-phase-query-processing-mode"  );
     }

/** Racer Function enable-very-smart-abox-mirroring
(enable-very-smart-abox-mirroring)
 */

     public String enableVerySmartAboxMirroring( ) throws RacerClientException {
          return racerCall("enable-very-smart-abox-mirroring"  ).toString();
     }

     public RacerResult enableVerySmartAboxMirroring$( ) throws RacerClientException {
          return racerCall("enable-very-smart-abox-mirroring"  );
     }

/** Racer Function ensure-abox-signature
(ensure-abox-signature abox-name-or-abox
                       &key
                       individuals
                       objects)
 */

     public String ensureAboxSignature(Object aboxNameOrAbox ) throws RacerClientException {
          return racerCall("ensure-abox-signature" , aboxNameOrAbox ).toString();
     }

     public RacerResult ensureAboxSignature$(Object aboxNameOrAbox ) throws RacerClientException {
          return racerCall("ensure-abox-signature" , aboxNameOrAbox );
     }

     public String ensureAboxSignature(Object aboxNameOrAbox , Object... keyArgs) throws RacerClientException {
          return racerCall("ensure-abox-signature" , aboxNameOrAbox , keyArgs).toString();
     }

     public RacerResult ensureAboxSignature$(Object aboxNameOrAbox , Object... keyArgs) throws RacerClientException {
          return racerCall("ensure-abox-signature" , aboxNameOrAbox , keyArgs);
     }

/** Racer Function ensure-subsumption-based-query-answering
(ensure-subsumption-based-query-answering &optional abox)
 */

     public String ensureSubsumptionBasedQueryAnswering( ) throws RacerClientException {
          return racerCall("ensure-subsumption-based-query-answering"  ).toString();
     }

     public RacerResult ensureSubsumptionBasedQueryAnswering$( ) throws RacerClientException {
          return racerCall("ensure-subsumption-based-query-answering"  );
     }

     public String ensureSubsumptionBasedQueryAnswering(Object abox ) throws RacerClientException {
          return racerCall("ensure-subsumption-based-query-answering" , abox ).toString();
     }

     public RacerResult ensureSubsumptionBasedQueryAnswering$(Object abox ) throws RacerClientException {
          return racerCall("ensure-subsumption-based-query-answering" , abox );
     }

/** Racer Function ensure-tbox-signature
(ensure-tbox-signature tbox
                       &key
                       atomic-concepts
                       roles
                       transitive-roles
                       features
                       attributes)
 */

     public String ensureTboxSignature(Object tbox ) throws RacerClientException {
          return racerCall("ensure-tbox-signature" , tbox ).toString();
     }

     public RacerResult ensureTboxSignature$(Object tbox ) throws RacerClientException {
          return racerCall("ensure-tbox-signature" , tbox );
     }

     public String ensureTboxSignature(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("ensure-tbox-signature" , tbox , keyArgs).toString();
     }

     public RacerResult ensureTboxSignature$(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("ensure-tbox-signature" , tbox , keyArgs);
     }

/** Racer Function exclude-permutations
(exclude-permutations)
 */

     public String excludePermutations( ) throws RacerClientException {
          return racerCall("exclude-permutations"  ).toString();
     }

     public RacerResult excludePermutations$( ) throws RacerClientException {
          return racerCall("exclude-permutations"  );
     }

/** Racer Function execute-all-queries
(execute-all-queries &key
                     abox
                     type-of-substrate
                     dont-add-abox-duplicates-p
                     remove-duplicates-p
                     two-phase-processing-p
                     deliver-phase-two-warning-tokens-p
                     deliver-kb-has-changed-warning-tokens-p
                     add-rule-consequences-p
                     continuation-based-instance-retrieval-p
                     told-information-reasoning-p
                     final-consistency-checking-p
                     runtime-consistency-checking-p
                     verbose-p
                     dont-show-variables
                     dont-show-head-projection-operators-p
                     dont-show-lambdas-p
                     how-many
                     only-new-tuples-p
                     timeout
                     proactive-tuple-computation-p
                     tuple-at-a-time-p
                     use-individual-synonyms-p
                     check-abox-consistency-p
                     ensure-tbox-classification-p
                     initial-abox-mirroring-p
                     initial-role-assertion-mirroring-p
                     classify-concepts-in-instance-assertions-p
                     exclude-permutations-p
                     record-explanations-p)
 */

     public String executeAllQueries( ) throws RacerClientException {
          return racerCall("execute-all-queries"  ).toString();
     }

     public RacerResult executeAllQueries$( ) throws RacerClientException {
          return racerCall("execute-all-queries"  );
     }

     public String executeAllQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("execute-all-queries"  , keyArgs).toString();
     }

     public RacerResult executeAllQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("execute-all-queries"  , keyArgs);
     }

/** Racer Function execute-all-rules
(execute-all-rules &key
                   abox
                   type-of-substrate)
 */

     public String executeAllRules( ) throws RacerClientException {
          return racerCall("execute-all-rules"  ).toString();
     }

     public RacerResult executeAllRules$( ) throws RacerClientException {
          return racerCall("execute-all-rules"  );
     }

     public String executeAllRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("execute-all-rules"  , keyArgs).toString();
     }

     public RacerResult executeAllRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("execute-all-rules"  , keyArgs);
     }

/** Racer Function execute-applicable-rules
(execute-applicable-rules &key
                          abox
                          type-of-substrate)
 */

     public String executeApplicableRules( ) throws RacerClientException {
          return racerCall("execute-applicable-rules"  ).toString();
     }

     public RacerResult executeApplicableRules$( ) throws RacerClientException {
          return racerCall("execute-applicable-rules"  );
     }

     public String executeApplicableRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("execute-applicable-rules"  , keyArgs).toString();
     }

     public RacerResult executeApplicableRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("execute-applicable-rules"  , keyArgs);
     }

/** Racer Function execute-or-reexecute-all-queries
(execute-or-reexecute-all-queries &key
                                  debug-p
                                  abox
                                  type-of-substrate
                                  dont-add-abox-duplicates-p
                                  remove-duplicates-p
                                  two-phase-processing-p
                                  deliver-phase-two-warning-tokens-p
                                  deliver-kb-has-changed-warning-tokens-p
                                  add-rule-consequences-p
                                  continuation-based-instance-retrieval-p
                                  told-information-reasoning-p
                                  final-consistency-checking-p
                                  runtime-consistency-checking-p
                                  verbose-p
                                  dont-show-variables
                                  dont-show-head-projection-operators-p
                                  dont-show-lambdas-p
                                  how-many
                                  only-new-tuples-p
                                  timeout
                                  proactive-tuple-computation-p
                                  tuple-at-a-time-p
                                  use-individual-synonyms-p
                                  check-abox-consistency-p
                                  ensure-tbox-classification-p
                                  initial-abox-mirroring-p
                                  initial-role-assertion-mirroring-p
                                  classify-concepts-in-instance-assertions-p
                                  exclude-permutations-p
                                  record-explanations-p)
 */

     public String executeOrReexecuteAllQueries( ) throws RacerClientException {
          return racerCall("execute-or-reexecute-all-queries"  ).toString();
     }

     public RacerResult executeOrReexecuteAllQueries$( ) throws RacerClientException {
          return racerCall("execute-or-reexecute-all-queries"  );
     }

     public String executeOrReexecuteAllQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("execute-or-reexecute-all-queries"  , keyArgs).toString();
     }

     public RacerResult executeOrReexecuteAllQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("execute-or-reexecute-all-queries"  , keyArgs);
     }

/** Racer Function execute-or-reexecute-all-rules
(execute-or-reexecute-all-rules &key
                                debug-p
                                abox
                                type-of-substrate)
 */

     public String executeOrReexecuteAllRules( ) throws RacerClientException {
          return racerCall("execute-or-reexecute-all-rules"  ).toString();
     }

     public RacerResult executeOrReexecuteAllRules$( ) throws RacerClientException {
          return racerCall("execute-or-reexecute-all-rules"  );
     }

     public String executeOrReexecuteAllRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("execute-or-reexecute-all-rules"  , keyArgs).toString();
     }

     public RacerResult executeOrReexecuteAllRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("execute-or-reexecute-all-rules"  , keyArgs);
     }

/** Racer Function execute-or-reexecute-query
(execute-or-reexecute-query query
                            &key
                            dont-add-abox-duplicates-p
                            remove-duplicates-p
                            two-phase-processing-p
                            deliver-phase-two-warning-tokens-p
                            deliver-kb-has-changed-warning-tokens-p
                            add-rule-consequences-p
                            continuation-based-instance-retrieval-p
                            told-information-reasoning-p
                            final-consistency-checking-p
                            runtime-consistency-checking-p
                            verbose-p
                            dont-show-variables
                            dont-show-head-projection-operators-p
                            dont-show-lambdas-p
                            how-many
                            only-new-tuples-p
                            timeout
                            proactive-tuple-computation-p
                            tuple-at-a-time-p
                            use-individual-synonyms-p
                            check-abox-consistency-p
                            ensure-tbox-classification-p
                            initial-abox-mirroring-p
                            initial-role-assertion-mirroring-p
                            classify-concepts-in-instance-assertions-p
                            exclude-permutations-p
                            record-explanations-p)
 */

     public String executeOrReexecuteQuery(Object query ) throws RacerClientException {
          return racerCall("execute-or-reexecute-query" , query ).toString();
     }

     public RacerResult executeOrReexecuteQuery$(Object query ) throws RacerClientException {
          return racerCall("execute-or-reexecute-query" , query );
     }

     public String executeOrReexecuteQuery(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("execute-or-reexecute-query" , query , keyArgs).toString();
     }

     public RacerResult executeOrReexecuteQuery$(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("execute-or-reexecute-query" , query , keyArgs);
     }

/** Racer Function execute-or-reexecute-rule
(execute-or-reexecute-rule query)
 */

     public String executeOrReexecuteRule(Object query ) throws RacerClientException {
          return racerCall("execute-or-reexecute-rule" , query ).toString();
     }

     public RacerResult executeOrReexecuteRule$(Object query ) throws RacerClientException {
          return racerCall("execute-or-reexecute-rule" , query );
     }

/** Racer Function execute-query
(execute-query query
               &key
               dont-add-abox-duplicates-p
               remove-duplicates-p
               two-phase-processing-p
               deliver-phase-two-warning-tokens-p
               deliver-kb-has-changed-warning-tokens-p
               add-rule-consequences-p
               continuation-based-instance-retrieval-p
               told-information-reasoning-p
               final-consistency-checking-p
               runtime-consistency-checking-p
               verbose-p
               dont-show-variables
               dont-show-head-projection-operators-p
               dont-show-lambdas-p
               how-many
               only-new-tuples-p
               timeout
               proactive-tuple-computation-p
               tuple-at-a-time-p
               use-individual-synonyms-p
               check-abox-consistency-p
               ensure-tbox-classification-p
               initial-abox-mirroring-p
               initial-role-assertion-mirroring-p
               classify-concepts-in-instance-assertions-p
               exclude-permutations-p
               record-explanations-p)
 */

     public String executeQuery(Object query ) throws RacerClientException {
          return racerCall("execute-query" , query ).toString();
     }

     public RacerResult executeQuery$(Object query ) throws RacerClientException {
          return racerCall("execute-query" , query );
     }

     public String executeQuery(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("execute-query" , query , keyArgs).toString();
     }

     public RacerResult executeQuery$(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("execute-query" , query , keyArgs);
     }

/** Racer Function execute-rule
(execute-rule query)
 */

     public String executeRule(Object query ) throws RacerClientException {
          return racerCall("execute-rule" , query ).toString();
     }

     public RacerResult executeRule$(Object query ) throws RacerClientException {
          return racerCall("execute-rule" , query );
     }

/** Racer Function expensive-queries
(expensive-queries &key
                   abox
                   type-of-substrate)
 */

     public String expensiveQueries( ) throws RacerClientException {
          return racerCall("expensive-queries"  ).toString();
     }

     public RacerResult expensiveQueries$( ) throws RacerClientException {
          return racerCall("expensive-queries"  );
     }

     public String expensiveQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("expensive-queries"  , keyArgs).toString();
     }

     public RacerResult expensiveQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("expensive-queries"  , keyArgs);
     }

/** Racer Function expensive-query-p
(expensive-query-p query)
 */

     public boolean expensiveQueryP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("expensive-query-p" , query ));
     }

/** Racer Function expensive-rule-p
(expensive-rule-p query)
 */

     public boolean expensiveRuleP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("expensive-rule-p" , query ));
     }

/** Racer Function expensive-rules
(expensive-rules &key
                 abox
                 type-of-substrate)
 */

     public String expensiveRules( ) throws RacerClientException {
          return racerCall("expensive-rules"  ).toString();
     }

     public RacerResult expensiveRules$( ) throws RacerClientException {
          return racerCall("expensive-rules"  );
     }

     public String expensiveRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("expensive-rules"  , keyArgs).toString();
     }

     public RacerResult expensiveRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("expensive-rules"  , keyArgs);
     }

/** Racer Function fcall
(fcall name)
 */

     public String fcall(Object name ) throws RacerClientException {
          return racerCall("fcall" , name ).toString();
     }

     public RacerResult fcall$(Object name ) throws RacerClientException {
          return racerCall("fcall" , name );
     }

/** Racer Function feature-p
(feature-p role-term &optional tbox)
 */

     public boolean featureP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("feature-p" , roleTerm ));
     }

     public boolean featureP(Object roleTerm, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("feature-p" , roleTerm, tbox ));
     }

/** Racer Function find-abox
(find-abox abox-name-or-abox &optional errorp)
 */

     public String findAbox(Object aboxNameOrAbox ) throws RacerClientException {
          return racerCall("find-abox" , aboxNameOrAbox ).toString();
     }

     public RacerResult findAbox$(Object aboxNameOrAbox ) throws RacerClientException {
          return racerCall("find-abox" , aboxNameOrAbox );
     }

     public String findAbox(Object aboxNameOrAbox, Object errorp ) throws RacerClientException {
          return racerCall("find-abox" , aboxNameOrAbox, errorp ).toString();
     }

     public RacerResult findAbox$(Object aboxNameOrAbox, Object errorp ) throws RacerClientException {
          return racerCall("find-abox" , aboxNameOrAbox, errorp );
     }

/** Racer Function find-tbox
(find-tbox tbox &optional errorp)
 */

     public String findTbox(Object tbox ) throws RacerClientException {
          return racerCall("find-tbox" , tbox ).toString();
     }

     public RacerResult findTbox$(Object tbox ) throws RacerClientException {
          return racerCall("find-tbox" , tbox );
     }

     public String findTbox(Object tbox, Object errorp ) throws RacerClientException {
          return racerCall("find-tbox" , tbox, errorp ).toString();
     }

     public RacerResult findTbox$(Object tbox, Object errorp ) throws RacerClientException {
          return racerCall("find-tbox" , tbox, errorp );
     }

/** Racer Function forget-abox
(forget-abox abox)
 */

     public String forgetAbox(Object abox ) throws RacerClientException {
          return racerCall("forget-abox" , abox ).toString();
     }

     public RacerResult forgetAbox$(Object abox ) throws RacerClientException {
          return racerCall("forget-abox" , abox );
     }

/** Racer Function forget-all-different-assertion
(forget-all-different-assertion abox individual-name-set)
 */

     public String forgetAllDifferentAssertion(Object abox, Object individualNameSet ) throws RacerClientException {
          return racerCall("forget-all-different-assertion" , abox, individualNameSet ).toString();
     }

     public RacerResult forgetAllDifferentAssertion$(Object abox, Object individualNameSet ) throws RacerClientException {
          return racerCall("forget-all-different-assertion" , abox, individualNameSet );
     }

/** Racer Function forget-annotation-concept-assertion
(forget-annotation-concept-assertion abox
                                     individual-name
                                     concept)
 */

     public String forgetAnnotationConceptAssertion(Object abox, Object individualName, Object concept ) throws RacerClientException {
          return racerCall("forget-annotation-concept-assertion" , abox, individualName, concept ).toString();
     }

     public RacerResult forgetAnnotationConceptAssertion$(Object abox, Object individualName, Object concept ) throws RacerClientException {
          return racerCall("forget-annotation-concept-assertion" , abox, individualName, concept );
     }

/** Racer Function forget-concept-assertion
(forget-concept-assertion abox
                          individual-name
                          concept)
 */

     public String forgetConceptAssertion(Object abox, Object individualName, Object concept ) throws RacerClientException {
          return racerCall("forget-concept-assertion" , abox, individualName, concept ).toString();
     }

     public RacerResult forgetConceptAssertion$(Object abox, Object individualName, Object concept ) throws RacerClientException {
          return racerCall("forget-concept-assertion" , abox, individualName, concept );
     }

/** Racer Function forget-concept-axiom
(forget-concept-axiom tbox
                      left
                      right
                      &key
                      inclusion-p)
 */

     public String forgetConceptAxiom(Object tbox, Object left, Object right ) throws RacerClientException {
          return racerCall("forget-concept-axiom" , tbox, left, right ).toString();
     }

     public RacerResult forgetConceptAxiom$(Object tbox, Object left, Object right ) throws RacerClientException {
          return racerCall("forget-concept-axiom" , tbox, left, right );
     }

     public String forgetConceptAxiom(Object tbox, Object left, Object right , Object... keyArgs) throws RacerClientException {
          return racerCall("forget-concept-axiom" , tbox, left, right , keyArgs).toString();
     }

     public RacerResult forgetConceptAxiom$(Object tbox, Object left, Object right , Object... keyArgs) throws RacerClientException {
          return racerCall("forget-concept-axiom" , tbox, left, right , keyArgs);
     }

/** Racer Function forget-constrained-assertion
(forget-constrained-assertion abox
                              individual-name
                              object-name
                              attribute-term)
 */

     public String forgetConstrainedAssertion(Object abox, Object individualName, Object objectName, Object attributeTerm ) throws RacerClientException {
          return racerCall("forget-constrained-assertion" , abox, individualName, objectName, attributeTerm ).toString();
     }

     public RacerResult forgetConstrainedAssertion$(Object abox, Object individualName, Object objectName, Object attributeTerm ) throws RacerClientException {
          return racerCall("forget-constrained-assertion" , abox, individualName, objectName, attributeTerm );
     }

/** Racer Function forget-constraint
(forget-constraint abox constraint)
 */

     public String forgetConstraint(Object abox, Object constraint ) throws RacerClientException {
          return racerCall("forget-constraint" , abox, constraint ).toString();
     }

     public RacerResult forgetConstraint$(Object abox, Object constraint ) throws RacerClientException {
          return racerCall("forget-constraint" , abox, constraint );
     }

/** Racer Function forget-datatype-role-filler
(forget-datatype-role-filler abox
                             individual
                             value
                             role)
 */

     public String forgetDatatypeRoleFiller(Object abox, Object individual, Object value, Object role ) throws RacerClientException {
          return racerCall("forget-datatype-role-filler" , abox, individual, value, role ).toString();
     }

     public RacerResult forgetDatatypeRoleFiller$(Object abox, Object individual, Object value, Object role ) throws RacerClientException {
          return racerCall("forget-datatype-role-filler" , abox, individual, value, role );
     }

/** Racer Function forget-different-from-assertion
(forget-different-from-assertion abox
                                 individual-1
                                 individual-2)
 */

     public String forgetDifferentFromAssertion(Object abox, Object individual1, Object individual2 ) throws RacerClientException {
          return racerCall("forget-different-from-assertion" , abox, individual1, individual2 ).toString();
     }

     public RacerResult forgetDifferentFromAssertion$(Object abox, Object individual1, Object individual2 ) throws RacerClientException {
          return racerCall("forget-different-from-assertion" , abox, individual1, individual2 );
     }

/** Racer Function forget-disjointness-axiom
(forget-disjointness-axiom tbox
                           concept-name
                           group-name
                           &optional
                           form)
 */

     public String forgetDisjointnessAxiom(Object tbox, Object conceptName, Object groupName ) throws RacerClientException {
          return racerCall("forget-disjointness-axiom" , tbox, conceptName, groupName ).toString();
     }

     public RacerResult forgetDisjointnessAxiom$(Object tbox, Object conceptName, Object groupName ) throws RacerClientException {
          return racerCall("forget-disjointness-axiom" , tbox, conceptName, groupName );
     }

     public String forgetDisjointnessAxiom(Object tbox, Object conceptName, Object groupName, Object form ) throws RacerClientException {
          return racerCall("forget-disjointness-axiom" , tbox, conceptName, groupName, form ).toString();
     }

     public RacerResult forgetDisjointnessAxiom$(Object tbox, Object conceptName, Object groupName, Object form ) throws RacerClientException {
          return racerCall("forget-disjointness-axiom" , tbox, conceptName, groupName, form );
     }

/** Racer Function forget-disjointness-axiom-statement
(forget-disjointness-axiom-statement tbox concepts)
 */

     public String forgetDisjointnessAxiomStatement(Object tbox, Object concepts ) throws RacerClientException {
          return racerCall("forget-disjointness-axiom-statement" , tbox, concepts ).toString();
     }

     public RacerResult forgetDisjointnessAxiomStatement$(Object tbox, Object concepts ) throws RacerClientException {
          return racerCall("forget-disjointness-axiom-statement" , tbox, concepts );
     }

/** Racer Function forget-individual
(forget-individual individual &optional abox)
 */

     public String forgetIndividual(Object individual ) throws RacerClientException {
          return racerCall("forget-individual" , individual ).toString();
     }

     public RacerResult forgetIndividual$(Object individual ) throws RacerClientException {
          return racerCall("forget-individual" , individual );
     }

     public String forgetIndividual(Object individual, Object abox ) throws RacerClientException {
          return racerCall("forget-individual" , individual, abox ).toString();
     }

     public RacerResult forgetIndividual$(Object individual, Object abox ) throws RacerClientException {
          return racerCall("forget-individual" , individual, abox );
     }

/** Racer Function forget-negated-role-assertion
(forget-negated-role-assertion abox
                               predecessor-name
                               filler-name
                               role-term)
 */

     public String forgetNegatedRoleAssertion(Object abox, Object predecessorName, Object fillerName, Object roleTerm ) throws RacerClientException {
          return racerCall("forget-negated-role-assertion" , abox, predecessorName, fillerName, roleTerm ).toString();
     }

     public RacerResult forgetNegatedRoleAssertion$(Object abox, Object predecessorName, Object fillerName, Object roleTerm ) throws RacerClientException {
          return racerCall("forget-negated-role-assertion" , abox, predecessorName, fillerName, roleTerm );
     }

/** Racer Function forget-negative-datatype-role-filler
(forget-negative-datatype-role-filler abox
                                      individual
                                      value
                                      role)
 */

     public String forgetNegativeDatatypeRoleFiller(Object abox, Object individual, Object value, Object role ) throws RacerClientException {
          return racerCall("forget-negative-datatype-role-filler" , abox, individual, value, role ).toString();
     }

     public RacerResult forgetNegativeDatatypeRoleFiller$(Object abox, Object individual, Object value, Object role ) throws RacerClientException {
          return racerCall("forget-negative-datatype-role-filler" , abox, individual, value, role );
     }

/** Racer Function forget-role-assertion
(forget-role-assertion abox
                       predecessor-name
                       filler-name
                       role-term)
 */

     public String forgetRoleAssertion(Object abox, Object predecessorName, Object fillerName, Object roleTerm ) throws RacerClientException {
          return racerCall("forget-role-assertion" , abox, predecessorName, fillerName, roleTerm ).toString();
     }

     public RacerResult forgetRoleAssertion$(Object abox, Object predecessorName, Object fillerName, Object roleTerm ) throws RacerClientException {
          return racerCall("forget-role-assertion" , abox, predecessorName, fillerName, roleTerm );
     }

/** Racer Function forget-role-axioms
(forget-role-axioms tbox
                    role
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
                    annotation-p)
 */

     public String forgetRoleAxioms(Object tbox, Object role ) throws RacerClientException {
          return racerCall("forget-role-axioms" , tbox, role ).toString();
     }

     public RacerResult forgetRoleAxioms$(Object tbox, Object role ) throws RacerClientException {
          return racerCall("forget-role-axioms" , tbox, role );
     }

     public String forgetRoleAxioms(Object tbox, Object role , Object... keyArgs) throws RacerClientException {
          return racerCall("forget-role-axioms" , tbox, role , keyArgs).toString();
     }

     public RacerResult forgetRoleAxioms$(Object tbox, Object role , Object... keyArgs) throws RacerClientException {
          return racerCall("forget-role-axioms" , tbox, role , keyArgs);
     }

/** Racer Function forget-same-individual-as-assertion
(forget-same-individual-as-assertion abox
                                     individual-1
                                     individual-2)
 */

     public String forgetSameIndividualAsAssertion(Object abox, Object individual1, Object individual2 ) throws RacerClientException {
          return racerCall("forget-same-individual-as-assertion" , abox, individual1, individual2 ).toString();
     }

     public RacerResult forgetSameIndividualAsAssertion$(Object abox, Object individual1, Object individual2 ) throws RacerClientException {
          return racerCall("forget-same-individual-as-assertion" , abox, individual1, individual2 );
     }

/** Racer Function forget-statement
(forget-statement tbox abox assertions)
 */

     public String forgetStatement(Object tbox, Object abox, Object assertions ) throws RacerClientException {
          return racerCall("forget-statement" , tbox, abox, assertions ).toString();
     }

     public RacerResult forgetStatement$(Object tbox, Object abox, Object assertions ) throws RacerClientException {
          return racerCall("forget-statement" , tbox, abox, assertions );
     }

/** Racer Function forget-tbox
(forget-tbox tbox)
 */

     public String forgetTbox(Object tbox ) throws RacerClientException {
          return racerCall("forget-tbox" , tbox ).toString();
     }

     public RacerResult forgetTbox$(Object tbox ) throws RacerClientException {
          return racerCall("forget-tbox" , tbox );
     }

/** Racer Function full-reset
(full-reset)
 */

     public String fullReset( ) throws RacerClientException {
          return racerCall("full-reset"  ).toString();
     }

     public RacerResult fullReset$( ) throws RacerClientException {
          return racerCall("full-reset"  );
     }

/** Racer Function get-abox-graph
(get-abox-graph &optional
                abox
                &key
                depth
                no-transitives-p
                no-top-role-p
                browsing-mode-p
                told-only-p
                root-individuals
                selected-individuals
                only-successors-in-selected-individuals-p
                for-roles
                for-datatype-properties
                for-annotation-properties)
 */

     public String getAboxGraph( ) throws RacerClientException {
          return racerCall("get-abox-graph"  ).toString();
     }

     public RacerResult getAboxGraph$( ) throws RacerClientException {
          return racerCall("get-abox-graph"  );
     }

     public String getAboxGraph(Object abox ) throws RacerClientException {
          return racerCall("get-abox-graph" , abox ).toString();
     }

     public RacerResult getAboxGraph$(Object abox ) throws RacerClientException {
          return racerCall("get-abox-graph" , abox );
     }

     public String getAboxGraph(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("get-abox-graph" , abox , keyArgs).toString();
     }

     public RacerResult getAboxGraph$(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("get-abox-graph" , abox , keyArgs);
     }

/** Racer Function get-abox-language
(get-abox-language &optional abox)
 */

     public String getAboxLanguage( ) throws RacerClientException {
          return racerCall("get-abox-language"  ).toString();
     }

     public RacerResult getAboxLanguage$( ) throws RacerClientException {
          return racerCall("get-abox-language"  );
     }

     public String getAboxLanguage(Object abox ) throws RacerClientException {
          return racerCall("get-abox-language" , abox ).toString();
     }

     public RacerResult getAboxLanguage$(Object abox ) throws RacerClientException {
          return racerCall("get-abox-language" , abox );
     }

/** Racer Function get-abox-signature
(get-abox-signature &optional abox)
 */

     public String getAboxSignature( ) throws RacerClientException {
          return racerCall("get-abox-signature"  ).toString();
     }

     public RacerResult getAboxSignature$( ) throws RacerClientException {
          return racerCall("get-abox-signature"  );
     }

     public String getAboxSignature(Object abox ) throws RacerClientException {
          return racerCall("get-abox-signature" , abox ).toString();
     }

     public RacerResult getAboxSignature$(Object abox ) throws RacerClientException {
          return racerCall("get-abox-signature" , abox );
     }

/** Racer Function get-abox-version
(get-abox-version abox)
 */

     public String getAboxVersion(Object abox ) throws RacerClientException {
          return racerCall("get-abox-version" , abox ).toString();
     }

     public RacerResult getAboxVersion$(Object abox ) throws RacerClientException {
          return racerCall("get-abox-version" , abox );
     }

/** Racer Function get-agraph-version
(get-agraph-version)
 */

     public String getAgraphVersion( ) throws RacerClientException {
          return racerCall("get-agraph-version"  ).toString();
     }

     public RacerResult getAgraphVersion$( ) throws RacerClientException {
          return racerCall("get-agraph-version"  );
     }

/** Racer Function get-all-answers
(get-all-answers &key
                 ready-p
                 active-p
                 processed-p
                 queries-p
                 rules-p
                 abox
                 type-of-substrate
                 dont-show-variables
                 execute-p)
 */

     public String getAllAnswers( ) throws RacerClientException {
          return racerCall("get-all-answers"  ).toString();
     }

     public RacerResult getAllAnswers$( ) throws RacerClientException {
          return racerCall("get-all-answers"  );
     }

     public String getAllAnswers(  Object... keyArgs) throws RacerClientException {
          return racerCall("get-all-answers"  , keyArgs).toString();
     }

     public RacerResult getAllAnswers$(  Object... keyArgs) throws RacerClientException {
          return racerCall("get-all-answers"  , keyArgs);
     }

/** Racer Function get-all-functions
(get-all-functions)
 */

     public String getAllFunctions( ) throws RacerClientException {
          return racerCall("get-all-functions"  ).toString();
     }

     public RacerResult getAllFunctions$( ) throws RacerClientException {
          return racerCall("get-all-functions"  );
     }

/** Racer Function get-all-remaining-sets-of-rule-consequences
(get-all-remaining-sets-of-rule-consequences query)
 */

     public String getAllRemainingSetsOfRuleConsequences(Object query ) throws RacerClientException {
          return racerCall("get-all-remaining-sets-of-rule-consequences" , query ).toString();
     }

     public RacerResult getAllRemainingSetsOfRuleConsequences$(Object query ) throws RacerClientException {
          return racerCall("get-all-remaining-sets-of-rule-consequences" , query );
     }

/** Racer Function get-all-remaining-tuples
(get-all-remaining-tuples query
                          &key
                          execute-p)
 */

     public String getAllRemainingTuples(Object query ) throws RacerClientException {
          return racerCall("get-all-remaining-tuples" , query ).toString();
     }

     public RacerResult getAllRemainingTuples$(Object query ) throws RacerClientException {
          return racerCall("get-all-remaining-tuples" , query );
     }

     public String getAllRemainingTuples(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("get-all-remaining-tuples" , query , keyArgs).toString();
     }

     public RacerResult getAllRemainingTuples$(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("get-all-remaining-tuples" , query , keyArgs);
     }

/** Racer Function get-all-server-functions
(get-all-server-functions)
 */

     public String getAllServerFunctions( ) throws RacerClientException {
          return racerCall("get-all-server-functions"  ).toString();
     }

     public RacerResult getAllServerFunctions$( ) throws RacerClientException {
          return racerCall("get-all-server-functions"  );
     }

/** Racer Function get-all-server-values
(get-all-server-values)
 */

     public String getAllServerValues( ) throws RacerClientException {
          return racerCall("get-all-server-values"  ).toString();
     }

     public RacerResult getAllServerValues$( ) throws RacerClientException {
          return racerCall("get-all-server-values"  );
     }

/** Racer Function get-all-values
(get-all-values)
 */

     public String getAllValues( ) throws RacerClientException {
          return racerCall("get-all-values"  ).toString();
     }

     public RacerResult getAllValues$( ) throws RacerClientException {
          return racerCall("get-all-values"  );
     }

/** Racer Function get-answer
(get-answer query
            &key
            dont-show-variables
            execute-p)
 */

     public String getAnswer(Object query ) throws RacerClientException {
          return racerCall("get-answer" , query ).toString();
     }

     public RacerResult getAnswer$(Object query ) throws RacerClientException {
          return racerCall("get-answer" , query );
     }

     public String getAnswer(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("get-answer" , query , keyArgs).toString();
     }

     public RacerResult getAnswer$(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("get-answer" , query , keyArgs);
     }

/** Racer Function get-answer-size
(get-answer-size query
                 &key
                 dont-show-variables
                 execute-p)
 */

     public String getAnswerSize(Object query ) throws RacerClientException {
          return racerCall("get-answer-size" , query ).toString();
     }

     public RacerResult getAnswerSize$(Object query ) throws RacerClientException {
          return racerCall("get-answer-size" , query );
     }

     public String getAnswerSize(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("get-answer-size" , query , keyArgs).toString();
     }

     public RacerResult getAnswerSize$(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("get-answer-size" , query , keyArgs);
     }

/** Racer Function get-build-version
(get-build-version)
 */

     public String getBuildVersion( ) throws RacerClientException {
          return racerCall("get-build-version"  ).toString();
     }

     public RacerResult getBuildVersion$( ) throws RacerClientException {
          return racerCall("get-build-version"  );
     }

/** Racer Function get-chosen-sets-of-rule-consequences
(get-chosen-sets-of-rule-consequences query)
 */

     public String getChosenSetsOfRuleConsequences(Object query ) throws RacerClientException {
          return racerCall("get-chosen-sets-of-rule-consequences" , query ).toString();
     }

     public RacerResult getChosenSetsOfRuleConsequences$(Object query ) throws RacerClientException {
          return racerCall("get-chosen-sets-of-rule-consequences" , query );
     }

/** Racer Function get-concept-definition-1
(get-concept-definition-1 concept-name tbox)
 */

     public String getConceptDefinition1(Object conceptName, Object tbox ) throws RacerClientException {
          return racerCall("get-concept-definition-1" , conceptName, tbox ).toString();
     }

     public RacerResult getConceptDefinition1$(Object conceptName, Object tbox ) throws RacerClientException {
          return racerCall("get-concept-definition-1" , conceptName, tbox );
     }

/** Racer Function get-concept-negated-definition-1
(get-concept-negated-definition-1 concept-name tbox)
 */

     public String getConceptNegatedDefinition1(Object conceptName, Object tbox ) throws RacerClientException {
          return racerCall("get-concept-negated-definition-1" , conceptName, tbox ).toString();
     }

     public RacerResult getConceptNegatedDefinition1$(Object conceptName, Object tbox ) throws RacerClientException {
          return racerCall("get-concept-negated-definition-1" , conceptName, tbox );
     }

/** Racer Function get-concept-pmodel
(get-concept-pmodel concept-expr tbox)
 */

     public String getConceptPmodel(Object conceptExpr, Object tbox ) throws RacerClientException {
          return racerCall("get-concept-pmodel" , conceptExpr, tbox ).toString();
     }

     public RacerResult getConceptPmodel$(Object conceptExpr, Object tbox ) throws RacerClientException {
          return racerCall("get-concept-pmodel" , conceptExpr, tbox );
     }

/** Racer Function get-concept-properties
(get-concept-properties concept
                        &optional
                        tbox
                        &key
                        for-roles
                        qualifications)
 */

     public String getConceptProperties(Object concept ) throws RacerClientException {
          return racerCall("get-concept-properties" , concept ).toString();
     }

     public RacerResult getConceptProperties$(Object concept ) throws RacerClientException {
          return racerCall("get-concept-properties" , concept );
     }

     public String getConceptProperties(Object concept, Object tbox ) throws RacerClientException {
          return racerCall("get-concept-properties" , concept, tbox ).toString();
     }

     public RacerResult getConceptProperties$(Object concept, Object tbox ) throws RacerClientException {
          return racerCall("get-concept-properties" , concept, tbox );
     }

     public String getConceptProperties(Object concept, Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("get-concept-properties" , concept, tbox , keyArgs).toString();
     }

     public RacerResult getConceptProperties$(Object concept, Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("get-concept-properties" , concept, tbox , keyArgs);
     }

/** Racer Function get-current-set-of-rule-consequences
(get-current-set-of-rule-consequences query)
 */

     public String getCurrentSetOfRuleConsequences(Object query ) throws RacerClientException {
          return racerCall("get-current-set-of-rule-consequences" , query ).toString();
     }

     public RacerResult getCurrentSetOfRuleConsequences$(Object query ) throws RacerClientException {
          return racerCall("get-current-set-of-rule-consequences" , query );
     }

/** Racer Function get-current-tuple
(get-current-tuple query)
 */

     public String getCurrentTuple(Object query ) throws RacerClientException {
          return racerCall("get-current-tuple" , query ).toString();
     }

     public RacerResult getCurrentTuple$(Object query ) throws RacerClientException {
          return racerCall("get-current-tuple" , query );
     }

/** Racer Function get-dag-of-qbox-for-abox
(get-dag-of-qbox-for-abox &optional abox)
 */

     public String getDagOfQboxForAbox( ) throws RacerClientException {
          return racerCall("get-dag-of-qbox-for-abox"  ).toString();
     }

     public RacerResult getDagOfQboxForAbox$( ) throws RacerClientException {
          return racerCall("get-dag-of-qbox-for-abox"  );
     }

     public String getDagOfQboxForAbox(Object abox ) throws RacerClientException {
          return racerCall("get-dag-of-qbox-for-abox" , abox ).toString();
     }

     public RacerResult getDagOfQboxForAbox$(Object abox ) throws RacerClientException {
          return racerCall("get-dag-of-qbox-for-abox" , abox );
     }

/** Racer Function get-data-bottom-role
(get-data-bottom-role tbox)
 */

     public String getDataBottomRole(Object tbox ) throws RacerClientException {
          return racerCall("get-data-bottom-role" , tbox ).toString();
     }

     public RacerResult getDataBottomRole$(Object tbox ) throws RacerClientException {
          return racerCall("get-data-bottom-role" , tbox );
     }

/** Racer Function get-data-edge-description
(get-data-edge-description from
                           to
                           &key
                           abox
                           type-of-substrate)
 */

     public String getDataEdgeDescription(Object from, Object to ) throws RacerClientException {
          return racerCall("get-data-edge-description" , from, to ).toString();
     }

     public RacerResult getDataEdgeDescription$(Object from, Object to ) throws RacerClientException {
          return racerCall("get-data-edge-description" , from, to );
     }

     public String getDataEdgeDescription(Object from, Object to , Object... keyArgs) throws RacerClientException {
          return racerCall("get-data-edge-description" , from, to , keyArgs).toString();
     }

     public RacerResult getDataEdgeDescription$(Object from, Object to , Object... keyArgs) throws RacerClientException {
          return racerCall("get-data-edge-description" , from, to , keyArgs);
     }

/** Racer Function get-data-edge-label
(get-data-edge-label from
                     to
                     &key
                     abox
                     type-of-substrate)
 */

     public String getDataEdgeLabel(Object from, Object to ) throws RacerClientException {
          return racerCall("get-data-edge-label" , from, to ).toString();
     }

     public RacerResult getDataEdgeLabel$(Object from, Object to ) throws RacerClientException {
          return racerCall("get-data-edge-label" , from, to );
     }

     public String getDataEdgeLabel(Object from, Object to , Object... keyArgs) throws RacerClientException {
          return racerCall("get-data-edge-label" , from, to , keyArgs).toString();
     }

     public RacerResult getDataEdgeLabel$(Object from, Object to , Object... keyArgs) throws RacerClientException {
          return racerCall("get-data-edge-label" , from, to , keyArgs);
     }

/** Racer Function get-data-node-description
(get-data-node-description name
                           &key
                           abox
                           type-of-substrate)
 */

     public String getDataNodeDescription(Object name ) throws RacerClientException {
          return racerCall("get-data-node-description" , name ).toString();
     }

     public RacerResult getDataNodeDescription$(Object name ) throws RacerClientException {
          return racerCall("get-data-node-description" , name );
     }

     public String getDataNodeDescription(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("get-data-node-description" , name , keyArgs).toString();
     }

     public RacerResult getDataNodeDescription$(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("get-data-node-description" , name , keyArgs);
     }

/** Racer Function get-data-node-label
(get-data-node-label name
                     &key
                     abox
                     type-of-substrate)
 */

     public String getDataNodeLabel(Object name ) throws RacerClientException {
          return racerCall("get-data-node-label" , name ).toString();
     }

     public RacerResult getDataNodeLabel$(Object name ) throws RacerClientException {
          return racerCall("get-data-node-label" , name );
     }

     public String getDataNodeLabel(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("get-data-node-label" , name , keyArgs).toString();
     }

     public RacerResult getDataNodeLabel$(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("get-data-node-label" , name , keyArgs);
     }

/** Racer Function get-edge-label-for-non-existent-edges
(get-edge-label-for-non-existent-edges &key
                                       abox
                                       type-of-substrate)
 */

     public String getEdgeLabelForNonExistentEdges( ) throws RacerClientException {
          return racerCall("get-edge-label-for-non-existent-edges"  ).toString();
     }

     public RacerResult getEdgeLabelForNonExistentEdges$( ) throws RacerClientException {
          return racerCall("get-edge-label-for-non-existent-edges"  );
     }

     public String getEdgeLabelForNonExistentEdges(  Object... keyArgs) throws RacerClientException {
          return racerCall("get-edge-label-for-non-existent-edges"  , keyArgs).toString();
     }

     public RacerResult getEdgeLabelForNonExistentEdges$(  Object... keyArgs) throws RacerClientException {
          return racerCall("get-edge-label-for-non-existent-edges"  , keyArgs);
     }

/** Racer Function get-explanations
(get-explanations query
                  &key
                  from
                  to
                  only-best-p
                  order-by
                  reverse-order-p
                  equi-order-by
                  remove-marker-symbols-p
                  remove-entailed-explanations-p
                  new-inds-p
                  tuples-p
                  full-tuples-p
                  all-assertions-p
                  hypothesized-assertions-p
                  show-score-p
                  abox-entailment
                  ensure-permutations-p)
 */

     public String getExplanations(Object query ) throws RacerClientException {
          return racerCall("get-explanations" , query ).toString();
     }

     public RacerResult getExplanations$(Object query ) throws RacerClientException {
          return racerCall("get-explanations" , query );
     }

     public String getExplanations(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("get-explanations" , query , keyArgs).toString();
     }

     public RacerResult getExplanations$(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("get-explanations" , query , keyArgs);
     }

/** Racer Function get-individual-annotation-datatype-fillers
(get-individual-annotation-datatype-fillers individual-name
                                            &optional
                                            abox
                                            with-types-p)
 */

     public String getIndividualAnnotationDatatypeFillers(Object individualName ) throws RacerClientException {
          return racerCall("get-individual-annotation-datatype-fillers" , individualName ).toString();
     }

     public RacerResult getIndividualAnnotationDatatypeFillers$(Object individualName ) throws RacerClientException {
          return racerCall("get-individual-annotation-datatype-fillers" , individualName );
     }

     public String getIndividualAnnotationDatatypeFillers(Object individualName, Object abox, Object withTypesP ) throws RacerClientException {
          return racerCall("get-individual-annotation-datatype-fillers" , individualName, abox, withTypesP ).toString();
     }

     public RacerResult getIndividualAnnotationDatatypeFillers$(Object individualName, Object abox, Object withTypesP ) throws RacerClientException {
          return racerCall("get-individual-annotation-datatype-fillers" , individualName, abox, withTypesP );
     }

     public String getIndividualAnnotationDatatypeFillers(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("get-individual-annotation-datatype-fillers" , individualName, abox ).toString();
     }

     public RacerResult getIndividualAnnotationDatatypeFillers$(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("get-individual-annotation-datatype-fillers" , individualName, abox );
     }

/** Racer Function get-individual-annotation-fillers
(get-individual-annotation-fillers individual-name
                                   &optional
                                   abox)
 */

     public String getIndividualAnnotationFillers(Object individualName ) throws RacerClientException {
          return racerCall("get-individual-annotation-fillers" , individualName ).toString();
     }

     public RacerResult getIndividualAnnotationFillers$(Object individualName ) throws RacerClientException {
          return racerCall("get-individual-annotation-fillers" , individualName );
     }

     public String getIndividualAnnotationFillers(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("get-individual-annotation-fillers" , individualName, abox ).toString();
     }

     public RacerResult getIndividualAnnotationFillers$(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("get-individual-annotation-fillers" , individualName, abox );
     }

/** Racer Function get-individual-datatype-fillers
(get-individual-datatype-fillers individual-name
                                 &optional
                                 abox
                                 with-types-p)
 */

     public String getIndividualDatatypeFillers(Object individualName ) throws RacerClientException {
          return racerCall("get-individual-datatype-fillers" , individualName ).toString();
     }

     public RacerResult getIndividualDatatypeFillers$(Object individualName ) throws RacerClientException {
          return racerCall("get-individual-datatype-fillers" , individualName );
     }

     public String getIndividualDatatypeFillers(Object individualName, Object abox, Object withTypesP ) throws RacerClientException {
          return racerCall("get-individual-datatype-fillers" , individualName, abox, withTypesP ).toString();
     }

     public RacerResult getIndividualDatatypeFillers$(Object individualName, Object abox, Object withTypesP ) throws RacerClientException {
          return racerCall("get-individual-datatype-fillers" , individualName, abox, withTypesP );
     }

     public String getIndividualDatatypeFillers(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("get-individual-datatype-fillers" , individualName, abox ).toString();
     }

     public RacerResult getIndividualDatatypeFillers$(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("get-individual-datatype-fillers" , individualName, abox );
     }

/** Racer Function get-individual-pmodel
(get-individual-pmodel individual-name abox)
 */

     public String getIndividualPmodel(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("get-individual-pmodel" , individualName, abox ).toString();
     }

     public RacerResult getIndividualPmodel$(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("get-individual-pmodel" , individualName, abox );
     }

/** Racer Function get-individual-successors
(get-individual-successors ind
                           &key
                           no-inverses-p
                           only-inverses-p
                           no-transitives-p
                           no-top-role-p
                           negated-p
                           roles
                           only-one-p
                           only-if-p
                           abox
                           remove-synonyms-p
                           show-synonyms-p)
 */

     public String getIndividualSuccessors(Object ind ) throws RacerClientException {
          return racerCall("get-individual-successors" , ind ).toString();
     }

     public RacerResult getIndividualSuccessors$(Object ind ) throws RacerClientException {
          return racerCall("get-individual-successors" , ind );
     }

     public String getIndividualSuccessors(Object ind , Object... keyArgs) throws RacerClientException {
          return racerCall("get-individual-successors" , ind , keyArgs).toString();
     }

     public RacerResult getIndividualSuccessors$(Object ind , Object... keyArgs) throws RacerClientException {
          return racerCall("get-individual-successors" , ind , keyArgs);
     }

/** Racer Function get-initial-size-of-process-pool
(get-initial-size-of-process-pool)
 */

     public String getInitialSizeOfProcessPool( ) throws RacerClientException {
          return racerCall("get-initial-size-of-process-pool"  ).toString();
     }

     public RacerResult getInitialSizeOfProcessPool$( ) throws RacerClientException {
          return racerCall("get-initial-size-of-process-pool"  );
     }

/** Racer Function get-kb-signature
(get-kb-signature kb-name)
 */

     public String getKbSignature(Object kbName ) throws RacerClientException {
          return racerCall("get-kb-signature" , kbName ).toString();
     }

     public RacerResult getKbSignature$(Object kbName ) throws RacerClientException {
          return racerCall("get-kb-signature" , kbName );
     }

/** Racer Function get-max-no-of-tuples-bound
(get-max-no-of-tuples-bound)
 */

     public String getMaxNoOfTuplesBound( ) throws RacerClientException {
          return racerCall("get-max-no-of-tuples-bound"  ).toString();
     }

     public RacerResult getMaxNoOfTuplesBound$( ) throws RacerClientException {
          return racerCall("get-max-no-of-tuples-bound"  );
     }

/** Racer Function get-maximum
(get-maximum aboxes &key key)
 */

     public String getMaximum(Object aboxes ) throws RacerClientException {
          return racerCall("get-maximum" , aboxes ).toString();
     }

     public RacerResult getMaximum$(Object aboxes ) throws RacerClientException {
          return racerCall("get-maximum" , aboxes );
     }

     public String getMaximum(Object aboxes , Object... keyArgs) throws RacerClientException {
          return racerCall("get-maximum" , aboxes , keyArgs).toString();
     }

     public RacerResult getMaximum$(Object aboxes , Object... keyArgs) throws RacerClientException {
          return racerCall("get-maximum" , aboxes , keyArgs);
     }

/** Racer Function get-maximum-size-of-process-pool
(get-maximum-size-of-process-pool)
 */

     public String getMaximumSizeOfProcessPool( ) throws RacerClientException {
          return racerCall("get-maximum-size-of-process-pool"  ).toString();
     }

     public RacerResult getMaximumSizeOfProcessPool$( ) throws RacerClientException {
          return racerCall("get-maximum-size-of-process-pool"  );
     }

/** Racer Function get-meta-constraint
(get-meta-constraint &optional tbox)
 */

     public String getMetaConstraint( ) throws RacerClientException {
          return racerCall("get-meta-constraint"  ).toString();
     }

     public RacerResult getMetaConstraint$( ) throws RacerClientException {
          return racerCall("get-meta-constraint"  );
     }

     public String getMetaConstraint(Object tbox ) throws RacerClientException {
          return racerCall("get-meta-constraint" , tbox ).toString();
     }

     public RacerResult getMetaConstraint$(Object tbox ) throws RacerClientException {
          return racerCall("get-meta-constraint" , tbox );
     }

/** Racer Function get-minimum
(get-minimum aboxes &key key)
 */

     public String getMinimum(Object aboxes ) throws RacerClientException {
          return racerCall("get-minimum" , aboxes ).toString();
     }

     public RacerResult getMinimum$(Object aboxes ) throws RacerClientException {
          return racerCall("get-minimum" , aboxes );
     }

     public String getMinimum(Object aboxes , Object... keyArgs) throws RacerClientException {
          return racerCall("get-minimum" , aboxes , keyArgs).toString();
     }

     public RacerResult getMinimum$(Object aboxes , Object... keyArgs) throws RacerClientException {
          return racerCall("get-minimum" , aboxes , keyArgs);
     }

/** Racer Function get-namespace-prefix
(get-namespace-prefix tbox)
 */

     public String getNamespacePrefix(Object tbox ) throws RacerClientException {
          return racerCall("get-namespace-prefix" , tbox ).toString();
     }

     public RacerResult getNamespacePrefix$(Object tbox ) throws RacerClientException {
          return racerCall("get-namespace-prefix" , tbox );
     }

/** Racer Function get-new-ind-counter
(get-new-ind-counter)
 */

     public String getNewIndCounter( ) throws RacerClientException {
          return racerCall("get-new-ind-counter"  ).toString();
     }

     public RacerResult getNewIndCounter$( ) throws RacerClientException {
          return racerCall("get-new-ind-counter"  );
     }

/** Racer Function get-new-ind-prefix
(get-new-ind-prefix)
 */

     public String getNewIndPrefix( ) throws RacerClientException {
          return racerCall("get-new-ind-prefix"  ).toString();
     }

     public RacerResult getNewIndPrefix$( ) throws RacerClientException {
          return racerCall("get-new-ind-prefix"  );
     }

/** Racer Function get-next-n-remaining-sets-of-rule-consequences
(get-next-n-remaining-sets-of-rule-consequences query
                                                &optional
                                                n)
 */

     public String getNextNRemainingSetsOfRuleConsequences(Object query ) throws RacerClientException {
          return racerCall("get-next-n-remaining-sets-of-rule-consequences" , query ).toString();
     }

     public RacerResult getNextNRemainingSetsOfRuleConsequences$(Object query ) throws RacerClientException {
          return racerCall("get-next-n-remaining-sets-of-rule-consequences" , query );
     }

     public String getNextNRemainingSetsOfRuleConsequences(Object query, Object n ) throws RacerClientException {
          return racerCall("get-next-n-remaining-sets-of-rule-consequences" , query, n ).toString();
     }

     public RacerResult getNextNRemainingSetsOfRuleConsequences$(Object query, Object n ) throws RacerClientException {
          return racerCall("get-next-n-remaining-sets-of-rule-consequences" , query, n );
     }

/** Racer Function get-next-n-remaining-tuples
(get-next-n-remaining-tuples query
                             &optional
                             n
                             &key
                             execute-p)
 */

     public String getNextNRemainingTuples(Object query ) throws RacerClientException {
          return racerCall("get-next-n-remaining-tuples" , query ).toString();
     }

     public RacerResult getNextNRemainingTuples$(Object query ) throws RacerClientException {
          return racerCall("get-next-n-remaining-tuples" , query );
     }

     public String getNextNRemainingTuples(Object query, Object n ) throws RacerClientException {
          return racerCall("get-next-n-remaining-tuples" , query, n ).toString();
     }

     public RacerResult getNextNRemainingTuples$(Object query, Object n ) throws RacerClientException {
          return racerCall("get-next-n-remaining-tuples" , query, n );
     }

     public String getNextNRemainingTuples(Object query, Object n , Object... keyArgs) throws RacerClientException {
          return racerCall("get-next-n-remaining-tuples" , query, n , keyArgs).toString();
     }

     public RacerResult getNextNRemainingTuples$(Object query, Object n , Object... keyArgs) throws RacerClientException {
          return racerCall("get-next-n-remaining-tuples" , query, n , keyArgs);
     }

/** Racer Function get-next-set-of-rule-consequences
(get-next-set-of-rule-consequences query)
 */

     public String getNextSetOfRuleConsequences(Object query ) throws RacerClientException {
          return racerCall("get-next-set-of-rule-consequences" , query ).toString();
     }

     public RacerResult getNextSetOfRuleConsequences$(Object query ) throws RacerClientException {
          return racerCall("get-next-set-of-rule-consequences" , query );
     }

/** Racer Function get-next-tuple
(get-next-tuple query
                &key
                execute-p)
 */

     public String getNextTuple(Object query ) throws RacerClientException {
          return racerCall("get-next-tuple" , query ).toString();
     }

     public RacerResult getNextTuple$(Object query ) throws RacerClientException {
          return racerCall("get-next-tuple" , query );
     }

     public String getNextTuple(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("get-next-tuple" , query , keyArgs).toString();
     }

     public RacerResult getNextTuple$(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("get-next-tuple" , query , keyArgs);
     }

/** Racer Function get-nodes-in-qbox-for-abox
(get-nodes-in-qbox-for-abox &optional abox)
 */

     public String getNodesInQboxForAbox( ) throws RacerClientException {
          return racerCall("get-nodes-in-qbox-for-abox"  ).toString();
     }

     public RacerResult getNodesInQboxForAbox$( ) throws RacerClientException {
          return racerCall("get-nodes-in-qbox-for-abox"  );
     }

     public String getNodesInQboxForAbox(Object abox ) throws RacerClientException {
          return racerCall("get-nodes-in-qbox-for-abox" , abox ).toString();
     }

     public RacerResult getNodesInQboxForAbox$(Object abox ) throws RacerClientException {
          return racerCall("get-nodes-in-qbox-for-abox" , abox );
     }

/** Racer Function get-nrql-version
(get-nrql-version)
 */

     public String getNrqlVersion( ) throws RacerClientException {
          return racerCall("get-nrql-version"  ).toString();
     }

     public RacerResult getNrqlVersion$( ) throws RacerClientException {
          return racerCall("get-nrql-version"  );
     }

/** Racer Function get-number-of-explanations
(get-number-of-explanations query
                            &key
                            dont-show-variables
                            execute-p)
 */

     public String getNumberOfExplanations(Object query ) throws RacerClientException {
          return racerCall("get-number-of-explanations" , query ).toString();
     }

     public RacerResult getNumberOfExplanations$(Object query ) throws RacerClientException {
          return racerCall("get-number-of-explanations" , query );
     }

     public String getNumberOfExplanations(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("get-number-of-explanations" , query , keyArgs).toString();
     }

     public RacerResult getNumberOfExplanations$(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("get-number-of-explanations" , query , keyArgs);
     }

/** Racer Function get-object-bottom-role
(get-object-bottom-role tbox)
 */

     public String getObjectBottomRole(Object tbox ) throws RacerClientException {
          return racerCall("get-object-bottom-role" , tbox ).toString();
     }

     public RacerResult getObjectBottomRole$(Object tbox ) throws RacerClientException {
          return racerCall("get-object-bottom-role" , tbox );
     }

/** Racer Function get-prefixes
(get-prefixes &optional tbox ask-owlapi-p)
 */

     public String getPrefixes( ) throws RacerClientException {
          return racerCall("get-prefixes"  ).toString();
     }

     public RacerResult getPrefixes$( ) throws RacerClientException {
          return racerCall("get-prefixes"  );
     }

     public String getPrefixes(Object tbox, Object askOwlapiP ) throws RacerClientException {
          return racerCall("get-prefixes" , tbox, askOwlapiP ).toString();
     }

     public RacerResult getPrefixes$(Object tbox, Object askOwlapiP ) throws RacerClientException {
          return racerCall("get-prefixes" , tbox, askOwlapiP );
     }

     public String getPrefixes(Object tbox ) throws RacerClientException {
          return racerCall("get-prefixes" , tbox ).toString();
     }

     public RacerResult getPrefixes$(Object tbox ) throws RacerClientException {
          return racerCall("get-prefixes" , tbox );
     }

/** Racer Function get-process-pool-size
(get-process-pool-size)
 */

     public String getProcessPoolSize( ) throws RacerClientException {
          return racerCall("get-process-pool-size"  ).toString();
     }

     public RacerResult getProcessPoolSize$( ) throws RacerClientException {
          return racerCall("get-process-pool-size"  );
     }

/** Racer Function get-proxy-server
(get-proxy-server)
 */

     public String getProxyServer( ) throws RacerClientException {
          return racerCall("get-proxy-server"  ).toString();
     }

     public RacerResult getProxyServer$( ) throws RacerClientException {
          return racerCall("get-proxy-server"  );
     }

/** Racer Function get-role-datatype
(get-role-datatype role-name &optional tbox)
 */

     public String getRoleDatatype(Object roleName ) throws RacerClientException {
          return racerCall("get-role-datatype" , roleName ).toString();
     }

     public RacerResult getRoleDatatype$(Object roleName ) throws RacerClientException {
          return racerCall("get-role-datatype" , roleName );
     }

     public String getRoleDatatype(Object roleName, Object tbox ) throws RacerClientException {
          return racerCall("get-role-datatype" , roleName, tbox ).toString();
     }

     public RacerResult getRoleDatatype$(Object roleName, Object tbox ) throws RacerClientException {
          return racerCall("get-role-datatype" , roleName, tbox );
     }

/** Racer Function get-role-hierarchy
(get-role-hierarchy &optional
                    tbox
                    &key
                    for-roles)
 */

     public String getRoleHierarchy( ) throws RacerClientException {
          return racerCall("get-role-hierarchy"  ).toString();
     }

     public RacerResult getRoleHierarchy$( ) throws RacerClientException {
          return racerCall("get-role-hierarchy"  );
     }

     public String getRoleHierarchy(Object tbox ) throws RacerClientException {
          return racerCall("get-role-hierarchy" , tbox ).toString();
     }

     public RacerResult getRoleHierarchy$(Object tbox ) throws RacerClientException {
          return racerCall("get-role-hierarchy" , tbox );
     }

     public String getRoleHierarchy(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("get-role-hierarchy" , tbox , keyArgs).toString();
     }

     public RacerResult getRoleHierarchy$(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("get-role-hierarchy" , tbox , keyArgs);
     }

/** Racer Function get-server-timeout
(get-server-timeout)
 */

     public String getServerTimeout( ) throws RacerClientException {
          return racerCall("get-server-timeout"  ).toString();
     }

     public RacerResult getServerTimeout$( ) throws RacerClientException {
          return racerCall("get-server-timeout"  );
     }

/** Racer Function get-substrate-edges
(get-substrate-edges &key
                     abox
                     type-of-substrate)
 */

     public String getSubstrateEdges( ) throws RacerClientException {
          return racerCall("get-substrate-edges"  ).toString();
     }

     public RacerResult getSubstrateEdges$( ) throws RacerClientException {
          return racerCall("get-substrate-edges"  );
     }

     public String getSubstrateEdges(  Object... keyArgs) throws RacerClientException {
          return racerCall("get-substrate-edges"  , keyArgs).toString();
     }

     public RacerResult getSubstrateEdges$(  Object... keyArgs) throws RacerClientException {
          return racerCall("get-substrate-edges"  , keyArgs);
     }

/** Racer Function get-substrate-nodes
(get-substrate-nodes &key
                     abox
                     type-of-substrate)
 */

     public String getSubstrateNodes( ) throws RacerClientException {
          return racerCall("get-substrate-nodes"  ).toString();
     }

     public RacerResult getSubstrateNodes$( ) throws RacerClientException {
          return racerCall("get-substrate-nodes"  );
     }

     public String getSubstrateNodes(  Object... keyArgs) throws RacerClientException {
          return racerCall("get-substrate-nodes"  , keyArgs).toString();
     }

     public RacerResult getSubstrateNodes$(  Object... keyArgs) throws RacerClientException {
          return racerCall("get-substrate-nodes"  , keyArgs);
     }

/** Racer Function get-substrate-type
(get-substrate-type)
 */

     public String getSubstrateType( ) throws RacerClientException {
          return racerCall("get-substrate-type"  ).toString();
     }

     public RacerResult getSubstrateType$( ) throws RacerClientException {
          return racerCall("get-substrate-type"  );
     }

/** Racer Function get-tbox-language
(get-tbox-language &optional tbox)
 */

     public String getTboxLanguage( ) throws RacerClientException {
          return racerCall("get-tbox-language"  ).toString();
     }

     public RacerResult getTboxLanguage$( ) throws RacerClientException {
          return racerCall("get-tbox-language"  );
     }

     public String getTboxLanguage(Object tbox ) throws RacerClientException {
          return racerCall("get-tbox-language" , tbox ).toString();
     }

     public RacerResult getTboxLanguage$(Object tbox ) throws RacerClientException {
          return racerCall("get-tbox-language" , tbox );
     }

/** Racer Function get-tbox-signature
(get-tbox-signature &optional tbox)
 */

     public String getTboxSignature( ) throws RacerClientException {
          return racerCall("get-tbox-signature"  ).toString();
     }

     public RacerResult getTboxSignature$( ) throws RacerClientException {
          return racerCall("get-tbox-signature"  );
     }

     public String getTboxSignature(Object tbox ) throws RacerClientException {
          return racerCall("get-tbox-signature" , tbox ).toString();
     }

     public RacerResult getTboxSignature$(Object tbox ) throws RacerClientException {
          return racerCall("get-tbox-signature" , tbox );
     }

/** Racer Function get-tbox-version
(get-tbox-version tbox)
 */

     public String getTboxVersion(Object tbox ) throws RacerClientException {
          return racerCall("get-tbox-version" , tbox ).toString();
     }

     public RacerResult getTboxVersion$(Object tbox ) throws RacerClientException {
          return racerCall("get-tbox-version" , tbox );
     }

/** Racer Function in-unsafe-mode?
(in-unsafe-mode?)
 */

     public boolean inUnsafeModeP( ) throws RacerClientException {
          return returnBoolean(racerCall("in-unsafe-mode?"  ));
     }

/** Racer Function inaccurate-queries
(inaccurate-queries &key
                    abox
                    type-of-substrate)
 */

     public String inaccurateQueries( ) throws RacerClientException {
          return racerCall("inaccurate-queries"  ).toString();
     }

     public RacerResult inaccurateQueries$( ) throws RacerClientException {
          return racerCall("inaccurate-queries"  );
     }

     public String inaccurateQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("inaccurate-queries"  , keyArgs).toString();
     }

     public RacerResult inaccurateQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("inaccurate-queries"  , keyArgs);
     }

/** Racer Function inaccurate-rules
(inaccurate-rules &key
                  abox
                  type-of-substrate)
 */

     public String inaccurateRules( ) throws RacerClientException {
          return racerCall("inaccurate-rules"  ).toString();
     }

     public RacerResult inaccurateRules$( ) throws RacerClientException {
          return racerCall("inaccurate-rules"  );
     }

     public String inaccurateRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("inaccurate-rules"  , keyArgs).toString();
     }

     public RacerResult inaccurateRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("inaccurate-rules"  , keyArgs);
     }

/** Racer Function include-kb
(include-kb pathname)
 */

     public String includeKb(Object pathname ) throws RacerClientException {
          return racerCall("include-kb" , pathname ).toString();
     }

     public RacerResult includeKb$(Object pathname ) throws RacerClientException {
          return racerCall("include-kb" , pathname );
     }

/** Racer Function include-permutations
(include-permutations)
 */

     public String includePermutations( ) throws RacerClientException {
          return racerCall("include-permutations"  ).toString();
     }

     public RacerResult includePermutations$( ) throws RacerClientException {
          return racerCall("include-permutations"  );
     }

/** Racer Function index-all-triples
(index-all-triples &key db)
 */

     public String indexAllTriples( ) throws RacerClientException {
          return racerCall("index-all-triples"  ).toString();
     }

     public RacerResult indexAllTriples$( ) throws RacerClientException {
          return racerCall("index-all-triples"  );
     }

     public String indexAllTriples(  Object... keyArgs) throws RacerClientException {
          return racerCall("index-all-triples"  , keyArgs).toString();
     }

     public RacerResult indexAllTriples$(  Object... keyArgs) throws RacerClientException {
          return racerCall("index-all-triples"  , keyArgs);
     }

/** Racer Function individual-instance-p
(individual-instance-p individual-name
                       concept
                       abox)
 */

     public boolean individualInstanceP(Object individualName, Object concept, Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("individual-instance-p" , individualName, concept, abox ));
     }

/** Racer Function individual-p
(individual-p individual-name &optional abox)
 */

     public boolean individualP(Object individualName ) throws RacerClientException {
          return returnBoolean(racerCall("individual-p" , individualName ));
     }

     public boolean individualP(Object individualName, Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("individual-p" , individualName, abox ));
     }

/** Racer Function individuals-equal-p
(individuals-equal-p individual-1
                     individual-2
                     &optional
                     abox)
 */

     public boolean individualsEqualP(Object individual1, Object individual2 ) throws RacerClientException {
          return returnBoolean(racerCall("individuals-equal-p" , individual1, individual2 ));
     }

     public boolean individualsEqualP(Object individual1, Object individual2, Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("individuals-equal-p" , individual1, individual2, abox ));
     }

/** Racer Function individuals-not-equal-p
(individuals-not-equal-p individual-1
                         individual-2
                         &optional
                         abox)
 */

     public boolean individualsNotEqualP(Object individual1, Object individual2 ) throws RacerClientException {
          return returnBoolean(racerCall("individuals-not-equal-p" , individual1, individual2 ));
     }

     public boolean individualsNotEqualP(Object individual1, Object individual2, Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("individuals-not-equal-p" , individual1, individual2, abox ));
     }

/** Racer Function individuals-related-p
(individuals-related-p ind-predecessor-name-set
                       ind-filler-name-set
                       role-term
                       abox)
 */

     public boolean individualsRelatedP(Object indPredecessorNameSet, Object indFillerNameSet, Object roleTerm, Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("individuals-related-p" , indPredecessorNameSet, indFillerNameSet, roleTerm, abox ));
     }

/** Racer Function init-abox
(init-abox abox &optional tbox)
 */

     public String initAbox(Object abox ) throws RacerClientException {
          return racerCall("init-abox" , abox ).toString();
     }

     public RacerResult initAbox$(Object abox ) throws RacerClientException {
          return racerCall("init-abox" , abox );
     }

     public String initAbox(Object abox, Object tbox ) throws RacerClientException {
          return racerCall("init-abox" , abox, tbox ).toString();
     }

     public RacerResult initAbox$(Object abox, Object tbox ) throws RacerClientException {
          return racerCall("init-abox" , abox, tbox );
     }

/** Racer Function init-publications-1
(init-publications-1 &optional abox)
 */

     public String initPublications1( ) throws RacerClientException {
          return racerCall("init-publications-1"  ).toString();
     }

     public RacerResult initPublications1$( ) throws RacerClientException {
          return racerCall("init-publications-1"  );
     }

     public String initPublications1(Object abox ) throws RacerClientException {
          return racerCall("init-publications-1" , abox ).toString();
     }

     public RacerResult initPublications1$(Object abox ) throws RacerClientException {
          return racerCall("init-publications-1" , abox );
     }

/** Racer Function init-subscriptions-1
(init-subscriptions-1 &optional abox)
 */

     public String initSubscriptions1( ) throws RacerClientException {
          return racerCall("init-subscriptions-1"  ).toString();
     }

     public RacerResult initSubscriptions1$( ) throws RacerClientException {
          return racerCall("init-subscriptions-1"  );
     }

     public String initSubscriptions1(Object abox ) throws RacerClientException {
          return racerCall("init-subscriptions-1" , abox ).toString();
     }

     public RacerResult initSubscriptions1$(Object abox ) throws RacerClientException {
          return racerCall("init-subscriptions-1" , abox );
     }

/** Racer Function init-tbox
(init-tbox tbox &key original reset)
 */

     public String initTbox(Object tbox ) throws RacerClientException {
          return racerCall("init-tbox" , tbox ).toString();
     }

     public RacerResult initTbox$(Object tbox ) throws RacerClientException {
          return racerCall("init-tbox" , tbox );
     }

     public String initTbox(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("init-tbox" , tbox , keyArgs).toString();
     }

     public RacerResult initTbox$(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("init-tbox" , tbox , keyArgs);
     }

/** Racer Function installed-patches
(installed-patches)
 */

     public String installedPatches( ) throws RacerClientException {
          return racerCall("installed-patches"  ).toString();
     }

     public RacerResult installedPatches$( ) throws RacerClientException {
          return racerCall("installed-patches"  );
     }

/** Racer Function installed-plugins
(installed-plugins)
 */

     public String installedPlugins( ) throws RacerClientException {
          return racerCall("installed-plugins"  ).toString();
     }

     public RacerResult installedPlugins$( ) throws RacerClientException {
          return racerCall("installed-plugins"  );
     }

/** Racer Function instantiators
(instantiators individual-name abox)
 */

     public String instantiators(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("instantiators" , individualName, abox ).toString();
     }

     public RacerResult instantiators$(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("instantiators" , individualName, abox );
     }

/** Racer Function internal-individuals-related-p
(internal-individuals-related-p ind-predecessor-name-set
                                ind-filler-name-set
                                role-term
                                abox
                                &optional
                                check-p)
 */

     public boolean internalIndividualsRelatedP(Object indPredecessorNameSet, Object indFillerNameSet, Object roleTerm, Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("internal-individuals-related-p" , indPredecessorNameSet, indFillerNameSet, roleTerm, abox ));
     }

     public boolean internalIndividualsRelatedP(Object indPredecessorNameSet, Object indFillerNameSet, Object roleTerm, Object abox, Object checkP ) throws RacerClientException {
          return returnBoolean(racerCall("internal-individuals-related-p" , indPredecessorNameSet, indFillerNameSet, roleTerm, abox, checkP ));
     }

/** Racer Function inverse-feature-p
(inverse-feature-p role-term &optional tbox)
 */

     public boolean inverseFeatureP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("inverse-feature-p" , roleTerm ));
     }

     public boolean inverseFeatureP(Object roleTerm, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("inverse-feature-p" , roleTerm, tbox ));
     }

/** Racer Function inverse-of-role
(inverse-of-role rolename inverse-role tbox)
 */

     public String inverseOfRole(Object rolename, Object inverseRole, Object tbox ) throws RacerClientException {
          return racerCall("inverse-of-role" , rolename, inverseRole, tbox ).toString();
     }

     public RacerResult inverseOfRole$(Object rolename, Object inverseRole, Object tbox ) throws RacerClientException {
          return racerCall("inverse-of-role" , rolename, inverseRole, tbox );
     }

/** Racer Function irreflexive-p
(irreflexive-p role-term &optional tbox)
 */

     public boolean irreflexiveP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("irreflexive-p" , roleTerm ));
     }

     public boolean irreflexiveP(Object roleTerm, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("irreflexive-p" , roleTerm, tbox ));
     }

/** Racer Function kb-ontologies
(kb-ontologies kb-name)
 */

     public String kbOntologies(Object kbName ) throws RacerClientException {
          return racerCall("kb-ontologies" , kbName ).toString();
     }

     public RacerResult kbOntologies$(Object kbName ) throws RacerClientException {
          return racerCall("kb-ontologies" , kbName );
     }

/** Racer Function keep-defined-query-atoms
(keep-defined-query-atoms)
 */

     public String keepDefinedQueryAtoms( ) throws RacerClientException {
          return racerCall("keep-defined-query-atoms"  ).toString();
     }

     public RacerResult keepDefinedQueryAtoms$( ) throws RacerClientException {
          return racerCall("keep-defined-query-atoms"  );
     }

/** Racer Function lcs
(lcs concept1 concept2)
 */

     public String lcs(Object concept1, Object concept2 ) throws RacerClientException {
          return racerCall("lcs" , concept1, concept2 ).toString();
     }

     public RacerResult lcs$(Object concept1, Object concept2 ) throws RacerClientException {
          return racerCall("lcs" , concept1, concept2 );
     }

/** Racer Function lcs-unfold
(lcs-unfold concept-1 concept-2 &optional tbox)
 */

     public String lcsUnfold(Object concept1, Object concept2 ) throws RacerClientException {
          return racerCall("lcs-unfold" , concept1, concept2 ).toString();
     }

     public RacerResult lcsUnfold$(Object concept1, Object concept2 ) throws RacerClientException {
          return racerCall("lcs-unfold" , concept1, concept2 );
     }

     public String lcsUnfold(Object concept1, Object concept2, Object tbox ) throws RacerClientException {
          return racerCall("lcs-unfold" , concept1, concept2, tbox ).toString();
     }

     public RacerResult lcsUnfold$(Object concept1, Object concept2, Object tbox ) throws RacerClientException {
          return racerCall("lcs-unfold" , concept1, concept2, tbox );
     }

/** Racer Function load-racer-patch
(load-racer-patch fn)
 */

     public String loadRacerPatch(Object fn ) throws RacerClientException {
          return racerCall("load-racer-patch" , fn ).toString();
     }

     public RacerResult loadRacerPatch$(Object fn ) throws RacerClientException {
          return racerCall("load-racer-patch" , fn );
     }

/** Racer Function load-racer-patches
(load-racer-patches directory)
 */

     public String loadRacerPatches(Object directory ) throws RacerClientException {
          return racerCall("load-racer-patches" , directory ).toString();
     }

     public RacerResult loadRacerPatches$(Object directory ) throws RacerClientException {
          return racerCall("load-racer-patches" , directory );
     }

/** Racer Function load-racer-plugin
(load-racer-plugin fn)
 */

     public String loadRacerPlugin(Object fn ) throws RacerClientException {
          return racerCall("load-racer-plugin" , fn ).toString();
     }

     public RacerResult loadRacerPlugin$(Object fn ) throws RacerClientException {
          return racerCall("load-racer-plugin" , fn );
     }

/** Racer Function load-racer-plugins
(load-racer-plugins directory)
 */

     public String loadRacerPlugins(Object directory ) throws RacerClientException {
          return racerCall("load-racer-plugins" , directory ).toString();
     }

     public RacerResult loadRacerPlugins$(Object directory ) throws RacerClientException {
          return racerCall("load-racer-plugins" , directory );
     }

/** Racer Function logging-off
(logging-off)
 */

     public String loggingOff( ) throws RacerClientException {
          return racerCall("logging-off"  ).toString();
     }

     public RacerResult loggingOff$( ) throws RacerClientException {
          return racerCall("logging-off"  );
     }

/** Racer Function logging-on
(logging-on &optional filename debug)
 */

     public String loggingOn( ) throws RacerClientException {
          return racerCall("logging-on"  ).toString();
     }

     public RacerResult loggingOn$( ) throws RacerClientException {
          return racerCall("logging-on"  );
     }

     public String loggingOn(Object filename, Object debug ) throws RacerClientException {
          return racerCall("logging-on" , filename, debug ).toString();
     }

     public RacerResult loggingOn$(Object filename, Object debug ) throws RacerClientException {
          return racerCall("logging-on" , filename, debug );
     }

     public String loggingOn(Object filename ) throws RacerClientException {
          return racerCall("logging-on" , filename ).toString();
     }

     public RacerResult loggingOn$(Object filename ) throws RacerClientException {
          return racerCall("logging-on" , filename );
     }

/** Racer Function make-abduction-rule-from-aboxes
(make-abduction-rule-from-aboxes precond-abox
                                 postcond-abox
                                 for-abox
                                 &key
                                 forward-rule-p
                                 backward-rule-p
                                 known-correspondences
                                 common-concept-assertions
                                 common-role-assertions
                                 common-same-as-assertions
                                 common-different-from-assertions
                                 common-as-strict-atoms-p
                                 injective-variables-p
                                 forward-rule-consequence-p)
 */

     public String makeAbductionRuleFromAboxes(Object precondAbox, Object postcondAbox, Object forAbox ) throws RacerClientException {
          return racerCall("make-abduction-rule-from-aboxes" , precondAbox, postcondAbox, forAbox ).toString();
     }

     public RacerResult makeAbductionRuleFromAboxes$(Object precondAbox, Object postcondAbox, Object forAbox ) throws RacerClientException {
          return racerCall("make-abduction-rule-from-aboxes" , precondAbox, postcondAbox, forAbox );
     }

     public String makeAbductionRuleFromAboxes(Object precondAbox, Object postcondAbox, Object forAbox , Object... keyArgs) throws RacerClientException {
          return racerCall("make-abduction-rule-from-aboxes" , precondAbox, postcondAbox, forAbox , keyArgs).toString();
     }

     public RacerResult makeAbductionRuleFromAboxes$(Object precondAbox, Object postcondAbox, Object forAbox , Object... keyArgs) throws RacerClientException {
          return racerCall("make-abduction-rule-from-aboxes" , precondAbox, postcondAbox, forAbox , keyArgs);
     }

/** Racer Function make-backward-rule-from-aboxes
(make-backward-rule-from-aboxes precond-abox
                                postcond-abox
                                for-abox
                                &key
                                known-correspondences
                                common-concept-assertions
                                common-role-assertions
                                common-same-as-assertions
                                common-different-from-assertions
                                common-as-strict-atoms-p
                                injective-variables-p
                                forward-rule-consequence-p)
 */

     public String makeBackwardRuleFromAboxes(Object precondAbox, Object postcondAbox, Object forAbox ) throws RacerClientException {
          return racerCall("make-backward-rule-from-aboxes" , precondAbox, postcondAbox, forAbox ).toString();
     }

     public RacerResult makeBackwardRuleFromAboxes$(Object precondAbox, Object postcondAbox, Object forAbox ) throws RacerClientException {
          return racerCall("make-backward-rule-from-aboxes" , precondAbox, postcondAbox, forAbox );
     }

     public String makeBackwardRuleFromAboxes(Object precondAbox, Object postcondAbox, Object forAbox , Object... keyArgs) throws RacerClientException {
          return racerCall("make-backward-rule-from-aboxes" , precondAbox, postcondAbox, forAbox , keyArgs).toString();
     }

     public RacerResult makeBackwardRuleFromAboxes$(Object precondAbox, Object postcondAbox, Object forAbox , Object... keyArgs) throws RacerClientException {
          return racerCall("make-backward-rule-from-aboxes" , precondAbox, postcondAbox, forAbox , keyArgs);
     }

/** Racer Function make-forward-rule-from-aboxes
(make-forward-rule-from-aboxes precond-abox
                               postcond-abox
                               for-abox
                               &key
                               execute-p
                               parser-class
                               rewrite-defined-concepts-p
                               group-by-ops
                               bind-specials-p
                               original-query
                               rule-con-pattern
                               new-ind-ops
                               premise
                               generate-code-p
                               optimize-p
                               rewrite-semantically-p
                               rewrite-to-dnf-p
                               report-inconsistent-queries-p
                               report-tautological-queries-p
                               use-repository-p
                               put-into-repository-p
                               id
                               dont-check-id-p
                               parser
                               result-vois
                               substrate
                               abox
                               create-abox-if-not-found-p
                               package
                               type-of-substrate
                               prepare-now-p
                               known-correspondences
                               common-concept-assertions
                               common-role-assertions
                               common-same-as-assertions
                               common-different-from-assertions
                               common-as-strict-atoms-p
                               injective-variables-p
                               forward-rule-consequence-p)
 */

     public String makeForwardRuleFromAboxes(Object precondAbox, Object postcondAbox, Object forAbox ) throws RacerClientException {
          return racerCall("make-forward-rule-from-aboxes" , precondAbox, postcondAbox, forAbox ).toString();
     }

     public RacerResult makeForwardRuleFromAboxes$(Object precondAbox, Object postcondAbox, Object forAbox ) throws RacerClientException {
          return racerCall("make-forward-rule-from-aboxes" , precondAbox, postcondAbox, forAbox );
     }

     public String makeForwardRuleFromAboxes(Object precondAbox, Object postcondAbox, Object forAbox , Object... keyArgs) throws RacerClientException {
          return racerCall("make-forward-rule-from-aboxes" , precondAbox, postcondAbox, forAbox , keyArgs).toString();
     }

     public RacerResult makeForwardRuleFromAboxes$(Object precondAbox, Object postcondAbox, Object forAbox , Object... keyArgs) throws RacerClientException {
          return racerCall("make-forward-rule-from-aboxes" , precondAbox, postcondAbox, forAbox , keyArgs);
     }

/** Racer Function make-plugin-from-fasl-file
(make-plugin-from-fasl-file fn2
                            &key
                            plugin-name
                            rand
                            type
                            extension
                            text-description
                            short-description
                            id
                            patch-name
                            for-version
                            for-build
                            platform)
 */

     public String makePluginFromFaslFile(Object fn2 ) throws RacerClientException {
          return racerCall("make-plugin-from-fasl-file" , fn2 ).toString();
     }

     public RacerResult makePluginFromFaslFile$(Object fn2 ) throws RacerClientException {
          return racerCall("make-plugin-from-fasl-file" , fn2 );
     }

     public String makePluginFromFaslFile(Object fn2 , Object... keyArgs) throws RacerClientException {
          return racerCall("make-plugin-from-fasl-file" , fn2 , keyArgs).toString();
     }

     public RacerResult makePluginFromFaslFile$(Object fn2 , Object... keyArgs) throws RacerClientException {
          return racerCall("make-plugin-from-fasl-file" , fn2 , keyArgs);
     }

/** Racer Function make-query-from-abox
(make-query-from-abox abox-or-name
                      &key
                      known-correspondences
                      common-concept-assertions
                      common-role-assertions
                      common-same-as-assertions
                      common-different-from-assertions
                      common-as-strict-atoms-p
                      injective-variables-p
                      forward-rule-consequence-p)
 */

     public String makeQueryFromAbox(Object aboxOrName ) throws RacerClientException {
          return racerCall("make-query-from-abox" , aboxOrName ).toString();
     }

     public RacerResult makeQueryFromAbox$(Object aboxOrName ) throws RacerClientException {
          return racerCall("make-query-from-abox" , aboxOrName );
     }

     public String makeQueryFromAbox(Object aboxOrName , Object... keyArgs) throws RacerClientException {
          return racerCall("make-query-from-abox" , aboxOrName , keyArgs).toString();
     }

     public RacerResult makeQueryFromAbox$(Object aboxOrName , Object... keyArgs) throws RacerClientException {
          return racerCall("make-query-from-abox" , aboxOrName , keyArgs);
     }

/** Racer Function materialize-inferences
(materialize-inferences kb-name
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
                        data-version-level)
 */

     public String materializeInferences(Object kbName ) throws RacerClientException {
          return racerCall("materialize-inferences" , kbName ).toString();
     }

     public RacerResult materializeInferences$(Object kbName ) throws RacerClientException {
          return racerCall("materialize-inferences" , kbName );
     }

     public String materializeInferences(Object kbName , Object... keyArgs) throws RacerClientException {
          return racerCall("materialize-inferences" , kbName , keyArgs).toString();
     }

     public RacerResult materializeInferences$(Object kbName , Object... keyArgs) throws RacerClientException {
          return racerCall("materialize-inferences" , kbName , keyArgs);
     }

/** Racer Function mirror
(mirror url-spec1 url-or-filename)
 */

     public String mirror(Object urlSpec1, Object urlOrFilename ) throws RacerClientException {
          return racerCall("mirror" , urlSpec1, urlOrFilename ).toString();
     }

     public RacerResult mirror$(Object urlSpec1, Object urlOrFilename ) throws RacerClientException {
          return racerCall("mirror" , urlSpec1, urlOrFilename );
     }

/** Racer Function most-specific-instantiators
(most-specific-instantiators individual-name abox)
 */

     public String mostSpecificInstantiators(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("most-specific-instantiators" , individualName, abox ).toString();
     }

     public RacerResult mostSpecificInstantiators$(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("most-specific-instantiators" , individualName, abox );
     }

/** Racer Function move-rules
(move-rules from-abox
            to-abox
            &key
            type-of-substrate)
 */

     public String moveRules(Object fromAbox, Object toAbox ) throws RacerClientException {
          return racerCall("move-rules" , fromAbox, toAbox ).toString();
     }

     public RacerResult moveRules$(Object fromAbox, Object toAbox ) throws RacerClientException {
          return racerCall("move-rules" , fromAbox, toAbox );
     }

     public String moveRules(Object fromAbox, Object toAbox , Object... keyArgs) throws RacerClientException {
          return racerCall("move-rules" , fromAbox, toAbox , keyArgs).toString();
     }

     public RacerResult moveRules$(Object fromAbox, Object toAbox , Object... keyArgs) throws RacerClientException {
          return racerCall("move-rules" , fromAbox, toAbox , keyArgs);
     }

/** Racer Function msc-k
(msc-k individual
       k
       &key
       include-direct-types
       abox
       name
       &rest
       (args))
 */

     public String mscK(Object individual, Object k ) throws RacerClientException {
          return racerCall("msc-k" , individual, k ).toString();
     }

     public RacerResult mscK$(Object individual, Object k ) throws RacerClientException {
          return racerCall("msc-k" , individual, k );
     }

     public String mscK(Object individual, Object k , Object... keyArgs) throws RacerClientException {
          return racerCall("msc-k" , individual, k , keyArgs).toString();
     }

     public RacerResult mscK$(Object individual, Object k , Object... keyArgs) throws RacerClientException {
          return racerCall("msc-k" , individual, k , keyArgs);
     }

/** Racer Function next-set-of-rule-consequences-available-p
(next-set-of-rule-consequences-available-p query)
 */

     public boolean nextSetOfRuleConsequencesAvailableP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("next-set-of-rule-consequences-available-p" , query ));
     }

/** Racer Function next-tuple-available-p
(next-tuple-available-p query)
 */

     public boolean nextTupleAvailableP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("next-tuple-available-p" , query ));
     }

/** Racer Function node-description1
(node-description1 name
                   &optional
                   abox
                   type-of-substrate)
 */

     public String nodeDescription1(Object name ) throws RacerClientException {
          return racerCall("node-description1" , name ).toString();
     }

     public RacerResult nodeDescription1$(Object name ) throws RacerClientException {
          return racerCall("node-description1" , name );
     }

     public String nodeDescription1(Object name, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("node-description1" , name, abox, typeOfSubstrate ).toString();
     }

     public RacerResult nodeDescription1$(Object name, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("node-description1" , name, abox, typeOfSubstrate );
     }

     public String nodeDescription1(Object name, Object abox ) throws RacerClientException {
          return racerCall("node-description1" , name, abox ).toString();
     }

     public RacerResult nodeDescription1$(Object name, Object abox ) throws RacerClientException {
          return racerCall("node-description1" , name, abox );
     }

/** Racer Function node-label1
(node-label1 name
             &optional
             abox
             type-of-substrate)
 */

     public String nodeLabel1(Object name ) throws RacerClientException {
          return racerCall("node-label1" , name ).toString();
     }

     public RacerResult nodeLabel1$(Object name ) throws RacerClientException {
          return racerCall("node-label1" , name );
     }

     public String nodeLabel1(Object name, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("node-label1" , name, abox, typeOfSubstrate ).toString();
     }

     public RacerResult nodeLabel1$(Object name, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("node-label1" , name, abox, typeOfSubstrate );
     }

     public String nodeLabel1(Object name, Object abox ) throws RacerClientException {
          return racerCall("node-label1" , name, abox ).toString();
     }

     public RacerResult nodeLabel1$(Object name, Object abox ) throws RacerClientException {
          return racerCall("node-label1" , name, abox );
     }

/** Racer Function open-triple-store
(open-triple-store name &key directory rdfs-reasoning)
 */

     public String openTripleStore(Object name ) throws RacerClientException {
          return racerCall("open-triple-store" , name ).toString();
     }

     public RacerResult openTripleStore$(Object name ) throws RacerClientException {
          return racerCall("open-triple-store" , name );
     }

     public String openTripleStore(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("open-triple-store" , name , keyArgs).toString();
     }

     public RacerResult openTripleStore$(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("open-triple-store" , name , keyArgs);
     }

/** Racer Function optimizer-dont-ensure-late-lambda-evaluation
(optimizer-dont-ensure-late-lambda-evaluation)
 */

     public String optimizerDontEnsureLateLambdaEvaluation( ) throws RacerClientException {
          return racerCall("optimizer-dont-ensure-late-lambda-evaluation"  ).toString();
     }

     public RacerResult optimizerDontEnsureLateLambdaEvaluation$( ) throws RacerClientException {
          return racerCall("optimizer-dont-ensure-late-lambda-evaluation"  );
     }

/** Racer Function optimizer-dont-use-cardinality-heuristics
(optimizer-dont-use-cardinality-heuristics)
 */

     public String optimizerDontUseCardinalityHeuristics( ) throws RacerClientException {
          return racerCall("optimizer-dont-use-cardinality-heuristics"  ).toString();
     }

     public RacerResult optimizerDontUseCardinalityHeuristics$( ) throws RacerClientException {
          return racerCall("optimizer-dont-use-cardinality-heuristics"  );
     }

/** Racer Function optimizer-ensure-late-lambda-evaluation
(optimizer-ensure-late-lambda-evaluation)
 */

     public String optimizerEnsureLateLambdaEvaluation( ) throws RacerClientException {
          return racerCall("optimizer-ensure-late-lambda-evaluation"  ).toString();
     }

     public RacerResult optimizerEnsureLateLambdaEvaluation$( ) throws RacerClientException {
          return racerCall("optimizer-ensure-late-lambda-evaluation"  );
     }

/** Racer Function optimizer-get-no-of-plans-upper-bound
(optimizer-get-no-of-plans-upper-bound)
 */

     public String optimizerGetNoOfPlansUpperBound( ) throws RacerClientException {
          return racerCall("optimizer-get-no-of-plans-upper-bound"  ).toString();
     }

     public RacerResult optimizerGetNoOfPlansUpperBound$( ) throws RacerClientException {
          return racerCall("optimizer-get-no-of-plans-upper-bound"  );
     }

/** Racer Function optimizer-get-time-bound
(optimizer-get-time-bound)
 */

     public String optimizerGetTimeBound( ) throws RacerClientException {
          return racerCall("optimizer-get-time-bound"  ).toString();
     }

     public RacerResult optimizerGetTimeBound$( ) throws RacerClientException {
          return racerCall("optimizer-get-time-bound"  );
     }

/** Racer Function optimizer-set-no-of-plans-upper-bound
(optimizer-set-no-of-plans-upper-bound n)
 */

     public String optimizerSetNoOfPlansUpperBound(Object n ) throws RacerClientException {
          return racerCall("optimizer-set-no-of-plans-upper-bound" , n ).toString();
     }

     public RacerResult optimizerSetNoOfPlansUpperBound$(Object n ) throws RacerClientException {
          return racerCall("optimizer-set-no-of-plans-upper-bound" , n );
     }

/** Racer Function optimizer-set-time-bound
(optimizer-set-time-bound n)
 */

     public String optimizerSetTimeBound(Object n ) throws RacerClientException {
          return racerCall("optimizer-set-time-bound" , n ).toString();
     }

     public RacerResult optimizerSetTimeBound$(Object n ) throws RacerClientException {
          return racerCall("optimizer-set-time-bound" , n );
     }

/** Racer Function optimizer-use-cardinality-heuristics
(optimizer-use-cardinality-heuristics)
 */

     public String optimizerUseCardinalityHeuristics( ) throws RacerClientException {
          return racerCall("optimizer-use-cardinality-heuristics"  ).toString();
     }

     public RacerResult optimizerUseCardinalityHeuristics$( ) throws RacerClientException {
          return racerCall("optimizer-use-cardinality-heuristics"  );
     }

/** Racer Function original-query-body
(original-query-body query)
 */

     public String originalQueryBody(Object query ) throws RacerClientException {
          return racerCall("original-query-body" , query ).toString();
     }

     public RacerResult originalQueryBody$(Object query ) throws RacerClientException {
          return racerCall("original-query-body" , query );
     }

/** Racer Function original-query-head
(original-query-head query)
 */

     public String originalQueryHead(Object query ) throws RacerClientException {
          return racerCall("original-query-head" , query ).toString();
     }

     public RacerResult originalQueryHead$(Object query ) throws RacerClientException {
          return racerCall("original-query-head" , query );
     }

/** Racer Function original-rule-antecedence
(original-rule-antecedence query)
 */

     public String originalRuleAntecedence(Object query ) throws RacerClientException {
          return racerCall("original-rule-antecedence" , query ).toString();
     }

     public RacerResult originalRuleAntecedence$(Object query ) throws RacerClientException {
          return racerCall("original-rule-antecedence" , query );
     }

/** Racer Function original-rule-consequence
(original-rule-consequence query)
 */

     public String originalRuleConsequence(Object query ) throws RacerClientException {
          return racerCall("original-rule-consequence" , query ).toString();
     }

     public RacerResult originalRuleConsequence$(Object query ) throws RacerClientException {
          return racerCall("original-rule-consequence" , query );
     }

/** Racer Function owl-read-document
(owl-read-document url-spec
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
                   ignore-annotations
                   &rest
                   (args))
 */

     public String owlReadDocument(Object urlSpec ) throws RacerClientException {
          return racerCall("owl-read-document" , urlSpec ).toString();
     }

     public RacerResult owlReadDocument$(Object urlSpec ) throws RacerClientException {
          return racerCall("owl-read-document" , urlSpec );
     }

     public String owlReadDocument(Object urlSpec , Object... keyArgs) throws RacerClientException {
          return racerCall("owl-read-document" , urlSpec , keyArgs).toString();
     }

     public RacerResult owlReadDocument$(Object urlSpec , Object... keyArgs) throws RacerClientException {
          return racerCall("owl-read-document" , urlSpec , keyArgs);
     }

/** Racer Function owl-read-file
(owl-read-file filename
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
               merge-imported-ontologies-p)
 */

     public String owlReadFile(Object filename ) throws RacerClientException {
          return racerCall("owl-read-file" , filename ).toString();
     }

     public RacerResult owlReadFile$(Object filename ) throws RacerClientException {
          return racerCall("owl-read-file" , filename );
     }

     public String owlReadFile(Object filename , Object... keyArgs) throws RacerClientException {
          return racerCall("owl-read-file" , filename , keyArgs).toString();
     }

     public RacerResult owlReadFile$(Object filename , Object... keyArgs) throws RacerClientException {
          return racerCall("owl-read-file" , filename , keyArgs);
     }

/** Racer Function owlapi-addaxiom
(|OWLAPI-addAxiom|)
 */

     public String owlapiAddAxiom( ) throws RacerClientException {
          return racerCall("|OWLAPI-addAxiom|"  ).toString();
     }

     public RacerResult owlapiAddAxiom$( ) throws RacerClientException {
          return racerCall("|OWLAPI-addAxiom|"  );
     }

/** Racer Function owlapi-addaxioms
(|OWLAPI-addAxioms|)
 */

     public String owlapiAddAxioms( ) throws RacerClientException {
          return racerCall("|OWLAPI-addAxioms|"  ).toString();
     }

     public RacerResult owlapiAddAxioms$( ) throws RacerClientException {
          return racerCall("|OWLAPI-addAxioms|"  );
     }

/** Racer Function owlapi-addprefix
(|OWLAPI-addPrefix| owlapi:prefix
                    owlapi:namespace
                    &optional
                    owlapi:reasoner)
 */

     public String owlapiAddPrefix(Object prefix, Object namespace ) throws RacerClientException {
          return racerCall("|OWLAPI-addPrefix|" , prefix, namespace ).toString();
     }

     public RacerResult owlapiAddPrefix$(Object prefix, Object namespace ) throws RacerClientException {
          return racerCall("|OWLAPI-addPrefix|" , prefix, namespace );
     }

     public String owlapiAddPrefix(Object prefix, Object namespace, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-addPrefix|" , prefix, namespace, reasoner ).toString();
     }

     public RacerResult owlapiAddPrefix$(Object prefix, Object namespace, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-addPrefix|" , prefix, namespace, reasoner );
     }

/** Racer Function owlapi-advanceprogress
(|OWLAPI-advanceProgress| &optional owlapi:reasoner)
 */

     public String owlapiAdvanceProgress( ) throws RacerClientException {
          return racerCall("|OWLAPI-advanceProgress|"  ).toString();
     }

     public RacerResult owlapiAdvanceProgress$( ) throws RacerClientException {
          return racerCall("|OWLAPI-advanceProgress|"  );
     }

     public String owlapiAdvanceProgress(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-advanceProgress|" , reasoner ).toString();
     }

     public RacerResult owlapiAdvanceProgress$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-advanceProgress|" , reasoner );
     }

/** Racer Function owlapi-applychanges
(|OWLAPI-applyChanges| &optional owlapi:reasoner)
 */

     public String owlapiApplyChanges( ) throws RacerClientException {
          return racerCall("|OWLAPI-applyChanges|"  ).toString();
     }

     public RacerResult owlapiApplyChanges$( ) throws RacerClientException {
          return racerCall("|OWLAPI-applyChanges|"  );
     }

     public String owlapiApplyChanges(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-applyChanges|" , reasoner ).toString();
     }

     public RacerResult owlapiApplyChanges$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-applyChanges|" , reasoner );
     }

/** Racer Function owlapi-autoaddaxiomsto
(|OWLAPI-autoAddAxiomsTo| owlapi:ontology &optional owlapi:reasoner)
 */

     public String owlapiAutoAddAxiomsTo(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-autoAddAxiomsTo|" , ontology ).toString();
     }

     public RacerResult owlapiAutoAddAxiomsTo$(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-autoAddAxiomsTo|" , ontology );
     }

     public String owlapiAutoAddAxiomsTo(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-autoAddAxiomsTo|" , ontology, reasoner ).toString();
     }

     public RacerResult owlapiAutoAddAxiomsTo$(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-autoAddAxiomsTo|" , ontology, reasoner );
     }

/** Racer Function owlapi-autoapplychanges
(|OWLAPI-autoApplyChanges| &optional owlapi:reasoner)
 */

     public String owlapiAutoApplyChanges( ) throws RacerClientException {
          return racerCall("|OWLAPI-autoApplyChanges|"  ).toString();
     }

     public RacerResult owlapiAutoApplyChanges$( ) throws RacerClientException {
          return racerCall("|OWLAPI-autoApplyChanges|"  );
     }

     public String owlapiAutoApplyChanges(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-autoApplyChanges|" , reasoner ).toString();
     }

     public RacerResult owlapiAutoApplyChanges$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-autoApplyChanges|" , reasoner );
     }

/** Racer Function owlapi-autobatchaddaxiomsto
(|OWLAPI-autoBatchAddAxiomsTo| owlapi:ontology
                               &optional
                               owlapi:reasoner)
 */

     public String owlapiAutoBatchAddAxiomsTo(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-autoBatchAddAxiomsTo|" , ontology ).toString();
     }

     public RacerResult owlapiAutoBatchAddAxiomsTo$(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-autoBatchAddAxiomsTo|" , ontology );
     }

     public String owlapiAutoBatchAddAxiomsTo(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-autoBatchAddAxiomsTo|" , ontology, reasoner ).toString();
     }

     public RacerResult owlapiAutoBatchAddAxiomsTo$(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-autoBatchAddAxiomsTo|" , ontology, reasoner );
     }

/** Racer Function owlapi-autobatchremoveaxiomsfrom
(|OWLAPI-autoBatchRemoveAxiomsFrom| owlapi:ontology
                                    &optional
                                    owlapi:reasoner)
 */

     public String owlapiAutoBatchRemoveAxiomsFrom(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-autoBatchRemoveAxiomsFrom|" , ontology ).toString();
     }

     public RacerResult owlapiAutoBatchRemoveAxiomsFrom$(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-autoBatchRemoveAxiomsFrom|" , ontology );
     }

     public String owlapiAutoBatchRemoveAxiomsFrom(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-autoBatchRemoveAxiomsFrom|" , ontology, reasoner ).toString();
     }

     public RacerResult owlapiAutoBatchRemoveAxiomsFrom$(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-autoBatchRemoveAxiomsFrom|" , ontology, reasoner );
     }

/** Racer Function owlapi-autoremoveaxiomsfrom
(|OWLAPI-autoRemoveAxiomsFrom| owlapi:ontology
                               &optional
                               owlapi:reasoner)
 */

     public String owlapiAutoRemoveAxiomsFrom(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-autoRemoveAxiomsFrom|" , ontology ).toString();
     }

     public RacerResult owlapiAutoRemoveAxiomsFrom$(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-autoRemoveAxiomsFrom|" , ontology );
     }

     public String owlapiAutoRemoveAxiomsFrom(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-autoRemoveAxiomsFrom|" , ontology, reasoner ).toString();
     }

     public RacerResult owlapiAutoRemoveAxiomsFrom$(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-autoRemoveAxiomsFrom|" , ontology, reasoner );
     }

/** Racer Function owlapi-axiomloaded?
(|OWLAPI-AxiomLoaded?| owlapi::id &optional owlapi:reasoner)
 */

     public boolean owlapiAxiomLoadedP(Object id ) throws RacerClientException {
          return returnBoolean(racerCall("|OWLAPI-AxiomLoaded?|" , id ));
     }

     public boolean owlapiAxiomLoadedP(Object id, Object reasoner ) throws RacerClientException {
          return returnBoolean(racerCall("|OWLAPI-AxiomLoaded?|" , id, reasoner ));
     }

/** Racer Function owlapi-axiomtoid
(|OWLAPI-AxiomToID| owlapi::axiom-constructor-call
                    &optional
                    owlapi:reasoner
                    owlapi::ont)
 */

     public String owlapiAxiomToID(Object axiomConstructorCall ) throws RacerClientException {
          return racerCall("|OWLAPI-AxiomToID|" , axiomConstructorCall ).toString();
     }

     public RacerResult owlapiAxiomToID$(Object axiomConstructorCall ) throws RacerClientException {
          return racerCall("|OWLAPI-AxiomToID|" , axiomConstructorCall );
     }

     public String owlapiAxiomToID(Object axiomConstructorCall, Object reasoner, Object ont ) throws RacerClientException {
          return racerCall("|OWLAPI-AxiomToID|" , axiomConstructorCall, reasoner, ont ).toString();
     }

     public RacerResult owlapiAxiomToID$(Object axiomConstructorCall, Object reasoner, Object ont ) throws RacerClientException {
          return racerCall("|OWLAPI-AxiomToID|" , axiomConstructorCall, reasoner, ont );
     }

     public String owlapiAxiomToID(Object axiomConstructorCall, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-AxiomToID|" , axiomConstructorCall, reasoner ).toString();
     }

     public RacerResult owlapiAxiomToID$(Object axiomConstructorCall, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-AxiomToID|" , axiomConstructorCall, reasoner );
     }

/** Racer Function owlapi-batchsynchronize
(|OWLAPI-batchSynchronize| owlapi:ontology &optional owlapi:reasoner)
 */

     public String owlapiBatchSynchronize(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-batchSynchronize|" , ontology ).toString();
     }

     public RacerResult owlapiBatchSynchronize$(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-batchSynchronize|" , ontology );
     }

     public String owlapiBatchSynchronize(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-batchSynchronize|" , ontology, reasoner ).toString();
     }

     public RacerResult owlapiBatchSynchronize$(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-batchSynchronize|" , ontology, reasoner );
     }

/** Racer Function owlapi-clearchanges
(|OWLAPI-clearChanges| &optional owlapi:reasoner)
 */

     public String owlapiClearChanges( ) throws RacerClientException {
          return racerCall("|OWLAPI-clearChanges|"  ).toString();
     }

     public RacerResult owlapiClearChanges$( ) throws RacerClientException {
          return racerCall("|OWLAPI-clearChanges|"  );
     }

     public String owlapiClearChanges(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-clearChanges|" , reasoner ).toString();
     }

     public RacerResult owlapiClearChanges$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-clearChanges|" , reasoner );
     }

/** Racer Function owlapi-clearontologies
(|OWLAPI-clearOntologies| &optional owlapi:reasoner)
 */

     public String owlapiClearOntologies( ) throws RacerClientException {
          return racerCall("|OWLAPI-clearOntologies|"  ).toString();
     }

     public RacerResult owlapiClearOntologies$( ) throws RacerClientException {
          return racerCall("|OWLAPI-clearOntologies|"  );
     }

     public String owlapiClearOntologies(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-clearOntologies|" , reasoner ).toString();
     }

     public RacerResult owlapiClearOntologies$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-clearOntologies|" , reasoner );
     }

/** Racer Function owlapi-clearregistry
(|OWLAPI-clearRegistry| &optional owlapi:reasoner)
 */

     public String owlapiClearRegistry( ) throws RacerClientException {
          return racerCall("|OWLAPI-clearRegistry|"  ).toString();
     }

     public RacerResult owlapiClearRegistry$( ) throws RacerClientException {
          return racerCall("|OWLAPI-clearRegistry|"  );
     }

     public String owlapiClearRegistry(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-clearRegistry|" , reasoner ).toString();
     }

     public RacerResult owlapiClearRegistry$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-clearRegistry|" , reasoner );
     }

/** Racer Function owlapi-considerdeclarations
(|OWLAPI-considerDeclarations| &optional owlapi:reasoner)
 */

     public String owlapiConsiderDeclarations( ) throws RacerClientException {
          return racerCall("|OWLAPI-considerDeclarations|"  ).toString();
     }

     public RacerResult owlapiConsiderDeclarations$( ) throws RacerClientException {
          return racerCall("|OWLAPI-considerDeclarations|"  );
     }

     public String owlapiConsiderDeclarations(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-considerDeclarations|" , reasoner ).toString();
     }

     public RacerResult owlapiConsiderDeclarations$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-considerDeclarations|" , reasoner );
     }

/** Racer Function owlapi-describeontologies
(|OWLAPI-describeOntologies| &optional owlapi:reasoner)
 */

     public String owlapiDescribeOntologies( ) throws RacerClientException {
          return racerCall("|OWLAPI-describeOntologies|"  ).toString();
     }

     public RacerResult owlapiDescribeOntologies$( ) throws RacerClientException {
          return racerCall("|OWLAPI-describeOntologies|"  );
     }

     public String owlapiDescribeOntologies(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-describeOntologies|" , reasoner ).toString();
     }

     public RacerResult owlapiDescribeOntologies$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-describeOntologies|" , reasoner );
     }

/** Racer Function owlapi-describeontology
(|OWLAPI-describeOntology| owlapi:ontology &optional owlapi:reasoner)
 */

     public String owlapiDescribeOntology(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-describeOntology|" , ontology ).toString();
     }

     public RacerResult owlapiDescribeOntology$(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-describeOntology|" , ontology );
     }

     public String owlapiDescribeOntology(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-describeOntology|" , ontology, reasoner ).toString();
     }

     public RacerResult owlapiDescribeOntology$(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-describeOntology|" , ontology, reasoner );
     }

/** Racer Function owlapi-describereasoner
(|OWLAPI-describeReasoner| &optional owlapi:reasoner)
 */

     public String owlapiDescribeReasoner( ) throws RacerClientException {
          return racerCall("|OWLAPI-describeReasoner|"  ).toString();
     }

     public RacerResult owlapiDescribeReasoner$( ) throws RacerClientException {
          return racerCall("|OWLAPI-describeReasoner|"  );
     }

     public String owlapiDescribeReasoner(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-describeReasoner|" , reasoner ).toString();
     }

     public RacerResult owlapiDescribeReasoner$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-describeReasoner|" , reasoner );
     }

/** Racer Function owlapi-describereasoners
(|OWLAPI-describeReasoners|)
 */

     public String owlapiDescribeReasoners( ) throws RacerClientException {
          return racerCall("|OWLAPI-describeReasoners|"  ).toString();
     }

     public RacerResult owlapiDescribeReasoners$( ) throws RacerClientException {
          return racerCall("|OWLAPI-describeReasoners|"  );
     }

/** Racer Function owlapi-disableautomode
(|OWLAPI-disableAutoMode| &optional owlapi:reasoner)
 */

     public String owlapiDisableAutoMode( ) throws RacerClientException {
          return racerCall("|OWLAPI-disableAutoMode|"  ).toString();
     }

     public RacerResult owlapiDisableAutoMode$( ) throws RacerClientException {
          return racerCall("|OWLAPI-disableAutoMode|"  );
     }

     public String owlapiDisableAutoMode(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disableAutoMode|" , reasoner ).toString();
     }

     public RacerResult owlapiDisableAutoMode$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disableAutoMode|" , reasoner );
     }

/** Racer Function owlapi-disableincrementalupdates
(|OWLAPI-disableIncrementalUpdates| &optional owlapi:reasoner)
 */

     public String owlapiDisableIncrementalUpdates( ) throws RacerClientException {
          return racerCall("|OWLAPI-disableIncrementalUpdates|"  ).toString();
     }

     public RacerResult owlapiDisableIncrementalUpdates$( ) throws RacerClientException {
          return racerCall("|OWLAPI-disableIncrementalUpdates|"  );
     }

     public String owlapiDisableIncrementalUpdates(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disableIncrementalUpdates|" , reasoner ).toString();
     }

     public RacerResult owlapiDisableIncrementalUpdates$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disableIncrementalUpdates|" , reasoner );
     }

/** Racer Function owlapi-disablelookupmode
(|OWLAPI-disableLookupMode| &optional owlapi:reasoner)
 */

     public String owlapiDisableLookupMode( ) throws RacerClientException {
          return racerCall("|OWLAPI-disableLookupMode|"  ).toString();
     }

     public RacerResult owlapiDisableLookupMode$( ) throws RacerClientException {
          return racerCall("|OWLAPI-disableLookupMode|"  );
     }

     public String owlapiDisableLookupMode(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disableLookupMode|" , reasoner ).toString();
     }

     public RacerResult owlapiDisableLookupMode$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disableLookupMode|" , reasoner );
     }

/** Racer Function owlapi-disablememorysavingmode
(|OWLAPI-disableMemorySavingMode| owlapi:reasoner)
 */

     public String owlapiDisableMemorySavingMode(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disableMemorySavingMode|" , reasoner ).toString();
     }

     public RacerResult owlapiDisableMemorySavingMode$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disableMemorySavingMode|" , reasoner );
     }

/** Racer Function owlapi-disablesimplifiedprotocol
(|OWLAPI-disableSimplifiedProtocol| &optional owlapi:reasoner)
 */

     public String owlapiDisableSimplifiedProtocol( ) throws RacerClientException {
          return racerCall("|OWLAPI-disableSimplifiedProtocol|"  ).toString();
     }

     public RacerResult owlapiDisableSimplifiedProtocol$( ) throws RacerClientException {
          return racerCall("|OWLAPI-disableSimplifiedProtocol|"  );
     }

     public String owlapiDisableSimplifiedProtocol(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disableSimplifiedProtocol|" , reasoner ).toString();
     }

     public RacerResult owlapiDisableSimplifiedProtocol$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disableSimplifiedProtocol|" , reasoner );
     }

/** Racer Function owlapi-disabletransientaxiommode
(|OWLAPI-disableTransientAxiomMode| &optional owlapi:reasoner)
 */

     public String owlapiDisableTransientAxiomMode( ) throws RacerClientException {
          return racerCall("|OWLAPI-disableTransientAxiomMode|"  ).toString();
     }

     public RacerResult owlapiDisableTransientAxiomMode$( ) throws RacerClientException {
          return racerCall("|OWLAPI-disableTransientAxiomMode|"  );
     }

     public String owlapiDisableTransientAxiomMode(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disableTransientAxiomMode|" , reasoner ).toString();
     }

     public RacerResult owlapiDisableTransientAxiomMode$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disableTransientAxiomMode|" , reasoner );
     }

/** Racer Function owlapi-disposeaxiom
(|OWLAPI-disposeAxiom| owlapi::id-or-constructor
                       &optional
                       owlapi:reasoner)
 */

     public String owlapiDisposeAxiom(Object idOrConstructor ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeAxiom|" , idOrConstructor ).toString();
     }

     public RacerResult owlapiDisposeAxiom$(Object idOrConstructor ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeAxiom|" , idOrConstructor );
     }

     public String owlapiDisposeAxiom(Object idOrConstructor, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeAxiom|" , idOrConstructor, reasoner ).toString();
     }

     public RacerResult owlapiDisposeAxiom$(Object idOrConstructor, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeAxiom|" , idOrConstructor, reasoner );
     }

/** Racer Function owlapi-disposeaxioms
(|OWLAPI-disposeAxioms| owlapi::ids-or-constructors
                        &optional
                        owlapi:reasoner)
 */

     public String owlapiDisposeAxioms(Object idsOrConstructors ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeAxioms|" , idsOrConstructors ).toString();
     }

     public RacerResult owlapiDisposeAxioms$(Object idsOrConstructors ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeAxioms|" , idsOrConstructors );
     }

     public String owlapiDisposeAxioms(Object idsOrConstructors, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeAxioms|" , idsOrConstructors, reasoner ).toString();
     }

     public RacerResult owlapiDisposeAxioms$(Object idsOrConstructors, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeAxioms|" , idsOrConstructors, reasoner );
     }

/** Racer Function owlapi-disposeontologies
(|OWLAPI-disposeOntologies| owlapi:ontologies &optional owlapi:reasoner)
 */

     public String owlapiDisposeOntologies(Object ontologies ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeOntologies|" , ontologies ).toString();
     }

     public RacerResult owlapiDisposeOntologies$(Object ontologies ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeOntologies|" , ontologies );
     }

     public String owlapiDisposeOntologies(Object ontologies, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeOntologies|" , ontologies, reasoner ).toString();
     }

     public RacerResult owlapiDisposeOntologies$(Object ontologies, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeOntologies|" , ontologies, reasoner );
     }

/** Racer Function owlapi-disposeontology
(|OWLAPI-disposeOntology| owlapi::ont-name
                          &optional
                          owlapi:reasoner
                          owlapi:dispose-axioms-p)
 */

     public String owlapiDisposeOntology(Object ontName ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeOntology|" , ontName ).toString();
     }

     public RacerResult owlapiDisposeOntology$(Object ontName ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeOntology|" , ontName );
     }

     public String owlapiDisposeOntology(Object ontName, Object reasoner, Object disposeAxiomsP ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeOntology|" , ontName, reasoner, disposeAxiomsP ).toString();
     }

     public RacerResult owlapiDisposeOntology$(Object ontName, Object reasoner, Object disposeAxiomsP ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeOntology|" , ontName, reasoner, disposeAxiomsP );
     }

     public String owlapiDisposeOntology(Object ontName, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeOntology|" , ontName, reasoner ).toString();
     }

     public RacerResult owlapiDisposeOntology$(Object ontName, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeOntology|" , ontName, reasoner );
     }

/** Racer Function owlapi-disposereasoner
(|OWLAPI-disposeReasoner| owlapi:name)
 */

     public String owlapiDisposeReasoner(Object name ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeReasoner|" , name ).toString();
     }

     public RacerResult owlapiDisposeReasoner$(Object name ) throws RacerClientException {
          return racerCall("|OWLAPI-disposeReasoner|" , name );
     }

/** Racer Function owlapi-dontregisterdeclaredentities
(|OWLAPI-dontRegisterDeclaredEntities| &optional owlapi:reasoner)
 */

     public String owlapiDontRegisterDeclaredEntities( ) throws RacerClientException {
          return racerCall("|OWLAPI-dontRegisterDeclaredEntities|"  ).toString();
     }

     public RacerResult owlapiDontRegisterDeclaredEntities$( ) throws RacerClientException {
          return racerCall("|OWLAPI-dontRegisterDeclaredEntities|"  );
     }

     public String owlapiDontRegisterDeclaredEntities(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-dontRegisterDeclaredEntities|" , reasoner ).toString();
     }

     public RacerResult owlapiDontRegisterDeclaredEntities$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-dontRegisterDeclaredEntities|" , reasoner );
     }

/** Racer Function owlapi-dontregisterreferencedentities
(|OWLAPI-dontRegisterReferencedEntities| &optional owlapi:reasoner)
 */

     public String owlapiDontRegisterReferencedEntities( ) throws RacerClientException {
          return racerCall("|OWLAPI-dontRegisterReferencedEntities|"  ).toString();
     }

     public RacerResult owlapiDontRegisterReferencedEntities$( ) throws RacerClientException {
          return racerCall("|OWLAPI-dontRegisterReferencedEntities|"  );
     }

     public String owlapiDontRegisterReferencedEntities(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-dontRegisterReferencedEntities|" , reasoner ).toString();
     }

     public RacerResult owlapiDontRegisterReferencedEntities$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-dontRegisterReferencedEntities|" , reasoner );
     }

/** Racer Function owlapi-enableincrementalupdates
(|OWLAPI-enableIncrementalUpdates| &optional owlapi:reasoner)
 */

     public String owlapiEnableIncrementalUpdates( ) throws RacerClientException {
          return racerCall("|OWLAPI-enableIncrementalUpdates|"  ).toString();
     }

     public RacerResult owlapiEnableIncrementalUpdates$( ) throws RacerClientException {
          return racerCall("|OWLAPI-enableIncrementalUpdates|"  );
     }

     public String owlapiEnableIncrementalUpdates(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-enableIncrementalUpdates|" , reasoner ).toString();
     }

     public RacerResult owlapiEnableIncrementalUpdates$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-enableIncrementalUpdates|" , reasoner );
     }

/** Racer Function owlapi-enablelookupmode
(|OWLAPI-enableLookupMode| &optional owlapi:reasoner)
 */

     public String owlapiEnableLookupMode( ) throws RacerClientException {
          return racerCall("|OWLAPI-enableLookupMode|"  ).toString();
     }

     public RacerResult owlapiEnableLookupMode$( ) throws RacerClientException {
          return racerCall("|OWLAPI-enableLookupMode|"  );
     }

     public String owlapiEnableLookupMode(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-enableLookupMode|" , reasoner ).toString();
     }

     public RacerResult owlapiEnableLookupMode$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-enableLookupMode|" , reasoner );
     }

/** Racer Function owlapi-enablememorysavingmode
(|OWLAPI-enableMemorySavingMode| owlapi:ontology
                                 &optional
                                 owlapi:reasoner
                                 owlapi::use-less-tbox-memory-p)
 */

     public String owlapiEnableMemorySavingMode(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-enableMemorySavingMode|" , ontology ).toString();
     }

     public RacerResult owlapiEnableMemorySavingMode$(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-enableMemorySavingMode|" , ontology );
     }

     public String owlapiEnableMemorySavingMode(Object ontology, Object reasoner, Object useLessTboxMemoryP ) throws RacerClientException {
          return racerCall("|OWLAPI-enableMemorySavingMode|" , ontology, reasoner, useLessTboxMemoryP ).toString();
     }

     public RacerResult owlapiEnableMemorySavingMode$(Object ontology, Object reasoner, Object useLessTboxMemoryP ) throws RacerClientException {
          return racerCall("|OWLAPI-enableMemorySavingMode|" , ontology, reasoner, useLessTboxMemoryP );
     }

     public String owlapiEnableMemorySavingMode(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-enableMemorySavingMode|" , ontology, reasoner ).toString();
     }

     public RacerResult owlapiEnableMemorySavingMode$(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-enableMemorySavingMode|" , ontology, reasoner );
     }

/** Racer Function owlapi-enablesimplifiedprotocol
(|OWLAPI-enableSimplifiedProtocol| &optional owlapi:reasoner)
 */

     public String owlapiEnableSimplifiedProtocol( ) throws RacerClientException {
          return racerCall("|OWLAPI-enableSimplifiedProtocol|"  ).toString();
     }

     public RacerResult owlapiEnableSimplifiedProtocol$( ) throws RacerClientException {
          return racerCall("|OWLAPI-enableSimplifiedProtocol|"  );
     }

     public String owlapiEnableSimplifiedProtocol(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-enableSimplifiedProtocol|" , reasoner ).toString();
     }

     public RacerResult owlapiEnableSimplifiedProtocol$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-enableSimplifiedProtocol|" , reasoner );
     }

/** Racer Function owlapi-enabletransientaxiommode
(|OWLAPI-enableTransientAxiomMode| &optional owlapi:reasoner)
 */

     public String owlapiEnableTransientAxiomMode( ) throws RacerClientException {
          return racerCall("|OWLAPI-enableTransientAxiomMode|"  ).toString();
     }

     public RacerResult owlapiEnableTransientAxiomMode$( ) throws RacerClientException {
          return racerCall("|OWLAPI-enableTransientAxiomMode|"  );
     }

     public String owlapiEnableTransientAxiomMode(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-enableTransientAxiomMode|" , reasoner ).toString();
     }

     public RacerResult owlapiEnableTransientAxiomMode$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-enableTransientAxiomMode|" , reasoner );
     }

/** Racer Function owlapi-exportontology
(|OWLAPI-exportOntology| owlapi:ontology
                         owlapi::fn
                         &key
                         owlapi:reasoner
                         owlapi::syntax
                         owlapi::quoted
                         owlapi::init
                         owlapi::header)
 */

     public String owlapiExportOntology(Object ontology, Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-exportOntology|" , ontology, fn ).toString();
     }

     public RacerResult owlapiExportOntology$(Object ontology, Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-exportOntology|" , ontology, fn );
     }

     public String owlapiExportOntology(Object ontology, Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-exportOntology|" , ontology, fn , keyArgs).toString();
     }

     public RacerResult owlapiExportOntology$(Object ontology, Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-exportOntology|" , ontology, fn , keyArgs);
     }

/** Racer Function owlapi-exportreasoner
(|OWLAPI-exportReasoner| owlapi:reasoner
                         owlapi::fn
                         &key
                         owlapi::syntax
                         owlapi::quoted
                         owlapi::init)
 */

     public String owlapiExportReasoner(Object reasoner, Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-exportReasoner|" , reasoner, fn ).toString();
     }

     public RacerResult owlapiExportReasoner$(Object reasoner, Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-exportReasoner|" , reasoner, fn );
     }

     public String owlapiExportReasoner(Object reasoner, Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-exportReasoner|" , reasoner, fn , keyArgs).toString();
     }

     public RacerResult owlapiExportReasoner$(Object reasoner, Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-exportReasoner|" , reasoner, fn , keyArgs);
     }

/** Racer Function owlapi-findidfromobject
(|OWLAPI-findIDFromObject| owlapi:obj)
 */

     public String owlapiFindIDFromObject(Object obj ) throws RacerClientException {
          return racerCall("|OWLAPI-findIDFromObject|" , obj ).toString();
     }

     public RacerResult owlapiFindIDFromObject$(Object obj ) throws RacerClientException {
          return racerCall("|OWLAPI-findIDFromObject|" , obj );
     }

/** Racer Function owlapi-findobjectfromid
(|OWLAPI-findObjectFromID| owlapi::id)
 */

     public String owlapiFindObjectFromID(Object id ) throws RacerClientException {
          return racerCall("|OWLAPI-findObjectFromID|" , id ).toString();
     }

     public RacerResult owlapiFindObjectFromID$(Object id ) throws RacerClientException {
          return racerCall("|OWLAPI-findObjectFromID|" , id );
     }

/** Racer Function owlapi-getallontologies
(|OWLAPI-getAllOntologies|)
 */

     public String owlapiGetAllOntologies( ) throws RacerClientException {
          return racerCall("|OWLAPI-getAllOntologies|"  ).toString();
     }

     public RacerResult owlapiGetAllOntologies$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getAllOntologies|"  );
     }

/** Racer Function owlapi-getancestorclasses
(|OWLAPI-getAncestorClasses| owlapi:cls &optional owlapi:reasoner)
 */

     public String owlapiGetAncestorClasses(Object cls ) throws RacerClientException {
          return racerCall("|OWLAPI-getAncestorClasses|" , cls ).toString();
     }

     public RacerResult owlapiGetAncestorClasses$(Object cls ) throws RacerClientException {
          return racerCall("|OWLAPI-getAncestorClasses|" , cls );
     }

     public String owlapiGetAncestorClasses(Object cls, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAncestorClasses|" , cls, reasoner ).toString();
     }

     public RacerResult owlapiGetAncestorClasses$(Object cls, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAncestorClasses|" , cls, reasoner );
     }

/** Racer Function owlapi-getancestorproperties
(|OWLAPI-getAncestorProperties| owlapi:property
                                &optional
                                owlapi:reasoner
                                owlapi::remove-self-p)
 */

     public String owlapiGetAncestorProperties(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getAncestorProperties|" , property ).toString();
     }

     public RacerResult owlapiGetAncestorProperties$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getAncestorProperties|" , property );
     }

     public String owlapiGetAncestorProperties(Object property, Object reasoner, Object removeSelfP ) throws RacerClientException {
          return racerCall("|OWLAPI-getAncestorProperties|" , property, reasoner, removeSelfP ).toString();
     }

     public RacerResult owlapiGetAncestorProperties$(Object property, Object reasoner, Object removeSelfP ) throws RacerClientException {
          return racerCall("|OWLAPI-getAncestorProperties|" , property, reasoner, removeSelfP );
     }

     public String owlapiGetAncestorProperties(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAncestorProperties|" , property, reasoner ).toString();
     }

     public RacerResult owlapiGetAncestorProperties$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAncestorProperties|" , property, reasoner );
     }

/** Racer Function owlapi-getannotationaxiomsforaxiom
(|OWLAPI-getAnnotationAxiomsForAxiom| owlapi:axiom-id
                                      &optional
                                      owlapi:reasoner)
 */

     public String owlapiGetAnnotationAxiomsForAxiom(Object axiomId ) throws RacerClientException {
          return racerCall("|OWLAPI-getAnnotationAxiomsForAxiom|" , axiomId ).toString();
     }

     public RacerResult owlapiGetAnnotationAxiomsForAxiom$(Object axiomId ) throws RacerClientException {
          return racerCall("|OWLAPI-getAnnotationAxiomsForAxiom|" , axiomId );
     }

     public String owlapiGetAnnotationAxiomsForAxiom(Object axiomId, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAnnotationAxiomsForAxiom|" , axiomId, reasoner ).toString();
     }

     public RacerResult owlapiGetAnnotationAxiomsForAxiom$(Object axiomId, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAnnotationAxiomsForAxiom|" , axiomId, reasoner );
     }

/** Racer Function owlapi-getautodeclaredataproperties
(|OWLAPI-getAutoDeclareDataProperties| &optional owlapi:reasoner)
 */

     public String owlapiGetAutoDeclareDataProperties( ) throws RacerClientException {
          return racerCall("|OWLAPI-getAutoDeclareDataProperties|"  ).toString();
     }

     public RacerResult owlapiGetAutoDeclareDataProperties$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getAutoDeclareDataProperties|"  );
     }

     public String owlapiGetAutoDeclareDataProperties(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAutoDeclareDataProperties|" , reasoner ).toString();
     }

     public RacerResult owlapiGetAutoDeclareDataProperties$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAutoDeclareDataProperties|" , reasoner );
     }

/** Racer Function owlapi-getautoontology
(|OWLAPI-getAutoOntology| &optional owlapi:reasoner)
 */

     public String owlapiGetAutoOntology( ) throws RacerClientException {
          return racerCall("|OWLAPI-getAutoOntology|"  ).toString();
     }

     public RacerResult owlapiGetAutoOntology$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getAutoOntology|"  );
     }

     public String owlapiGetAutoOntology(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAutoOntology|" , reasoner ).toString();
     }

     public RacerResult owlapiGetAutoOntology$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAutoOntology|" , reasoner );
     }

/** Racer Function owlapi-getaxiomcounter
(|OWLAPI-getAxiomCounter| &optional owlapi:reasoner)
 */

     public String owlapiGetAxiomCounter( ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomCounter|"  ).toString();
     }

     public RacerResult owlapiGetAxiomCounter$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomCounter|"  );
     }

     public String owlapiGetAxiomCounter(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomCounter|" , reasoner ).toString();
     }

     public RacerResult owlapiGetAxiomCounter$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomCounter|" , reasoner );
     }

/** Racer Function owlapi-getaxioms
(|OWLAPI-getAxioms| &optional
                    owlapi:reasoner
                    owlapi:with-ids-p
                    owlapi::with-ont-names-p
                    owlapi::status)
 */

     public String owlapiGetAxioms( ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxioms|"  ).toString();
     }

     public RacerResult owlapiGetAxioms$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxioms|"  );
     }

     public String owlapiGetAxioms(Object reasoner, Object withIdsP, Object withOntNamesP, Object status ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxioms|" , reasoner, withIdsP, withOntNamesP, status ).toString();
     }

     public RacerResult owlapiGetAxioms$(Object reasoner, Object withIdsP, Object withOntNamesP, Object status ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxioms|" , reasoner, withIdsP, withOntNamesP, status );
     }

     public String owlapiGetAxioms(Object reasoner, Object withIdsP, Object withOntNamesP ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxioms|" , reasoner, withIdsP, withOntNamesP ).toString();
     }

     public RacerResult owlapiGetAxioms$(Object reasoner, Object withIdsP, Object withOntNamesP ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxioms|" , reasoner, withIdsP, withOntNamesP );
     }

     public String owlapiGetAxioms(Object reasoner, Object withIdsP ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxioms|" , reasoner, withIdsP ).toString();
     }

     public RacerResult owlapiGetAxioms$(Object reasoner, Object withIdsP ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxioms|" , reasoner, withIdsP );
     }

     public String owlapiGetAxioms(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxioms|" , reasoner ).toString();
     }

     public RacerResult owlapiGetAxioms$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxioms|" , reasoner );
     }

/** Racer Function owlapi-getaxiomsin
(|OWLAPI-getAxiomsIn| owlapi::ont
                      &optional
                      owlapi:reasoner
                      owlapi:with-ids-p
                      owlapi::status)
 */

     public String owlapiGetAxiomsIn(Object ont ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsIn|" , ont ).toString();
     }

     public RacerResult owlapiGetAxiomsIn$(Object ont ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsIn|" , ont );
     }

     public String owlapiGetAxiomsIn(Object ont, Object reasoner, Object withIdsP, Object status ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsIn|" , ont, reasoner, withIdsP, status ).toString();
     }

     public RacerResult owlapiGetAxiomsIn$(Object ont, Object reasoner, Object withIdsP, Object status ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsIn|" , ont, reasoner, withIdsP, status );
     }

     public String owlapiGetAxiomsIn(Object ont, Object reasoner, Object withIdsP ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsIn|" , ont, reasoner, withIdsP ).toString();
     }

     public RacerResult owlapiGetAxiomsIn$(Object ont, Object reasoner, Object withIdsP ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsIn|" , ont, reasoner, withIdsP );
     }

     public String owlapiGetAxiomsIn(Object ont, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsIn|" , ont, reasoner ).toString();
     }

     public RacerResult owlapiGetAxiomsIn$(Object ont, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsIn|" , ont, reasoner );
     }

/** Racer Function owlapi-getaxiomsoftype
(|OWLAPI-getAxiomsOfType| owlapi::type-or-types
                          &optional
                          owlapi:reasoner
                          owlapi:with-ids-p
                          owlapi::with-ont-names-p
                          owlapi::status)
 */

     public String owlapiGetAxiomsOfType(Object typeOrTypes ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfType|" , typeOrTypes ).toString();
     }

     public RacerResult owlapiGetAxiomsOfType$(Object typeOrTypes ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfType|" , typeOrTypes );
     }

     public String owlapiGetAxiomsOfType(Object typeOrTypes, Object reasoner, Object withIdsP, Object withOntNamesP, Object status ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfType|" , typeOrTypes, reasoner, withIdsP, withOntNamesP, status ).toString();
     }

     public RacerResult owlapiGetAxiomsOfType$(Object typeOrTypes, Object reasoner, Object withIdsP, Object withOntNamesP, Object status ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfType|" , typeOrTypes, reasoner, withIdsP, withOntNamesP, status );
     }

     public String owlapiGetAxiomsOfType(Object typeOrTypes, Object reasoner, Object withIdsP, Object withOntNamesP ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfType|" , typeOrTypes, reasoner, withIdsP, withOntNamesP ).toString();
     }

     public RacerResult owlapiGetAxiomsOfType$(Object typeOrTypes, Object reasoner, Object withIdsP, Object withOntNamesP ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfType|" , typeOrTypes, reasoner, withIdsP, withOntNamesP );
     }

     public String owlapiGetAxiomsOfType(Object typeOrTypes, Object reasoner, Object withIdsP ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfType|" , typeOrTypes, reasoner, withIdsP ).toString();
     }

     public RacerResult owlapiGetAxiomsOfType$(Object typeOrTypes, Object reasoner, Object withIdsP ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfType|" , typeOrTypes, reasoner, withIdsP );
     }

     public String owlapiGetAxiomsOfType(Object typeOrTypes, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfType|" , typeOrTypes, reasoner ).toString();
     }

     public RacerResult owlapiGetAxiomsOfType$(Object typeOrTypes, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfType|" , typeOrTypes, reasoner );
     }

/** Racer Function owlapi-getaxiomsoftypein
(|OWLAPI-getAxiomsOfTypeIn| owlapi::type-or-types
                            owlapi::ont
                            &optional
                            owlapi:reasoner
                            owlapi:with-ids-p
                            owlapi::status)
 */

     public String owlapiGetAxiomsOfTypeIn(Object typeOrTypes, Object ont ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfTypeIn|" , typeOrTypes, ont ).toString();
     }

     public RacerResult owlapiGetAxiomsOfTypeIn$(Object typeOrTypes, Object ont ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfTypeIn|" , typeOrTypes, ont );
     }

     public String owlapiGetAxiomsOfTypeIn(Object typeOrTypes, Object ont, Object reasoner, Object withIdsP, Object status ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfTypeIn|" , typeOrTypes, ont, reasoner, withIdsP, status ).toString();
     }

     public RacerResult owlapiGetAxiomsOfTypeIn$(Object typeOrTypes, Object ont, Object reasoner, Object withIdsP, Object status ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfTypeIn|" , typeOrTypes, ont, reasoner, withIdsP, status );
     }

     public String owlapiGetAxiomsOfTypeIn(Object typeOrTypes, Object ont, Object reasoner, Object withIdsP ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfTypeIn|" , typeOrTypes, ont, reasoner, withIdsP ).toString();
     }

     public RacerResult owlapiGetAxiomsOfTypeIn$(Object typeOrTypes, Object ont, Object reasoner, Object withIdsP ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfTypeIn|" , typeOrTypes, ont, reasoner, withIdsP );
     }

     public String owlapiGetAxiomsOfTypeIn(Object typeOrTypes, Object ont, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfTypeIn|" , typeOrTypes, ont, reasoner ).toString();
     }

     public RacerResult owlapiGetAxiomsOfTypeIn$(Object typeOrTypes, Object ont, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsOfTypeIn|" , typeOrTypes, ont, reasoner );
     }

/** Racer Function owlapi-getaxiomsperontology
(|OWLAPI-getAxiomsPerOntology| &optional owlapi:reasoner)
 */

     public String owlapiGetAxiomsPerOntology( ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsPerOntology|"  ).toString();
     }

     public RacerResult owlapiGetAxiomsPerOntology$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsPerOntology|"  );
     }

     public String owlapiGetAxiomsPerOntology(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsPerOntology|" , reasoner ).toString();
     }

     public RacerResult owlapiGetAxiomsPerOntology$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getAxiomsPerOntology|" , reasoner );
     }

/** Racer Function owlapi-getchanges
(|OWLAPI-getChanges| &optional owlapi:reasoner)
 */

     public String owlapiGetChanges( ) throws RacerClientException {
          return racerCall("|OWLAPI-getChanges|"  ).toString();
     }

     public RacerResult owlapiGetChanges$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getChanges|"  );
     }

     public String owlapiGetChanges(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getChanges|" , reasoner ).toString();
     }

     public RacerResult owlapiGetChanges$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getChanges|" , reasoner );
     }

/** Racer Function owlapi-getcurrentreasoner
(|OWLAPI-getCurrentReasoner|)
 */

     public String owlapiGetCurrentReasoner( ) throws RacerClientException {
          return racerCall("|OWLAPI-getCurrentReasoner|"  ).toString();
     }

     public RacerResult owlapiGetCurrentReasoner$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getCurrentReasoner|"  );
     }

/** Racer Function owlapi-getdatapropertyrelationships
(|OWLAPI-getDataPropertyRelationships| owlapi::ind
                                       &optional
                                       owlapi:reasoner)
 */

     public String owlapiGetDataPropertyRelationships(Object ind ) throws RacerClientException {
          return racerCall("|OWLAPI-getDataPropertyRelationships|" , ind ).toString();
     }

     public RacerResult owlapiGetDataPropertyRelationships$(Object ind ) throws RacerClientException {
          return racerCall("|OWLAPI-getDataPropertyRelationships|" , ind );
     }

     public String owlapiGetDataPropertyRelationships(Object ind, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDataPropertyRelationships|" , ind, reasoner ).toString();
     }

     public RacerResult owlapiGetDataPropertyRelationships$(Object ind, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDataPropertyRelationships|" , ind, reasoner );
     }

/** Racer Function owlapi-getdatapropertyvalues
(|OWLAPI-getDataPropertyValues| owlapi::ind
                                owlapi:property
                                &optional
                                owlapi:reasoner)
 */

     public String owlapiGetDataPropertyValues(Object ind, Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getDataPropertyValues|" , ind, property ).toString();
     }

     public RacerResult owlapiGetDataPropertyValues$(Object ind, Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getDataPropertyValues|" , ind, property );
     }

     public String owlapiGetDataPropertyValues(Object ind, Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDataPropertyValues|" , ind, property, reasoner ).toString();
     }

     public RacerResult owlapiGetDataPropertyValues$(Object ind, Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDataPropertyValues|" , ind, property, reasoner );
     }

/** Racer Function owlapi-getdescendantclasses
(|OWLAPI-getDescendantClasses| owlapi:cls &optional owlapi:reasoner)
 */

     public String owlapiGetDescendantClasses(Object cls ) throws RacerClientException {
          return racerCall("|OWLAPI-getDescendantClasses|" , cls ).toString();
     }

     public RacerResult owlapiGetDescendantClasses$(Object cls ) throws RacerClientException {
          return racerCall("|OWLAPI-getDescendantClasses|" , cls );
     }

     public String owlapiGetDescendantClasses(Object cls, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDescendantClasses|" , cls, reasoner ).toString();
     }

     public RacerResult owlapiGetDescendantClasses$(Object cls, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDescendantClasses|" , cls, reasoner );
     }

/** Racer Function owlapi-getdescendantproperties
(|OWLAPI-getDescendantProperties| owlapi:property
                                  &optional
                                  owlapi:reasoner
                                  owlapi::remove-self-p)
 */

     public String owlapiGetDescendantProperties(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getDescendantProperties|" , property ).toString();
     }

     public RacerResult owlapiGetDescendantProperties$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getDescendantProperties|" , property );
     }

     public String owlapiGetDescendantProperties(Object property, Object reasoner, Object removeSelfP ) throws RacerClientException {
          return racerCall("|OWLAPI-getDescendantProperties|" , property, reasoner, removeSelfP ).toString();
     }

     public RacerResult owlapiGetDescendantProperties$(Object property, Object reasoner, Object removeSelfP ) throws RacerClientException {
          return racerCall("|OWLAPI-getDescendantProperties|" , property, reasoner, removeSelfP );
     }

     public String owlapiGetDescendantProperties(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDescendantProperties|" , property, reasoner ).toString();
     }

     public RacerResult owlapiGetDescendantProperties$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDescendantProperties|" , property, reasoner );
     }

/** Racer Function owlapi-getdifferentindividuals
(|OWLAPI-getDifferentIndividuals| owlapi::ind
                                  &optional
                                  owlapi:reasoner
                                  owlapi::synonyms)
 */

     public String owlapiGetDifferentIndividuals(Object ind ) throws RacerClientException {
          return racerCall("|OWLAPI-getDifferentIndividuals|" , ind ).toString();
     }

     public RacerResult owlapiGetDifferentIndividuals$(Object ind ) throws RacerClientException {
          return racerCall("|OWLAPI-getDifferentIndividuals|" , ind );
     }

     public String owlapiGetDifferentIndividuals(Object ind, Object reasoner, Object synonyms ) throws RacerClientException {
          return racerCall("|OWLAPI-getDifferentIndividuals|" , ind, reasoner, synonyms ).toString();
     }

     public RacerResult owlapiGetDifferentIndividuals$(Object ind, Object reasoner, Object synonyms ) throws RacerClientException {
          return racerCall("|OWLAPI-getDifferentIndividuals|" , ind, reasoner, synonyms );
     }

     public String owlapiGetDifferentIndividuals(Object ind, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDifferentIndividuals|" , ind, reasoner ).toString();
     }

     public RacerResult owlapiGetDifferentIndividuals$(Object ind, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDifferentIndividuals|" , ind, reasoner );
     }

/** Racer Function owlapi-getdisjointclasses
(|OWLAPI-getDisjointClasses| owlapi::concept &optional owlapi:reasoner)
 */

     public String owlapiGetDisjointClasses(Object concept ) throws RacerClientException {
          return racerCall("|OWLAPI-getDisjointClasses|" , concept ).toString();
     }

     public RacerResult owlapiGetDisjointClasses$(Object concept ) throws RacerClientException {
          return racerCall("|OWLAPI-getDisjointClasses|" , concept );
     }

     public String owlapiGetDisjointClasses(Object concept, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDisjointClasses|" , concept, reasoner ).toString();
     }

     public RacerResult owlapiGetDisjointClasses$(Object concept, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDisjointClasses|" , concept, reasoner );
     }

/** Racer Function owlapi-getdisjointdataproperties
(|OWLAPI-getDisjointDataProperties| owlapi:property
                                    &optional
                                    owlapi:reasoner)
 */

     public String owlapiGetDisjointDataProperties(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getDisjointDataProperties|" , property ).toString();
     }

     public RacerResult owlapiGetDisjointDataProperties$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getDisjointDataProperties|" , property );
     }

     public String owlapiGetDisjointDataProperties(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDisjointDataProperties|" , property, reasoner ).toString();
     }

     public RacerResult owlapiGetDisjointDataProperties$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDisjointDataProperties|" , property, reasoner );
     }

/** Racer Function owlapi-getdisjointobjectproperties
(|OWLAPI-getDisjointObjectProperties| owlapi:property
                                      &optional
                                      owlapi:reasoner)
 */

     public String owlapiGetDisjointObjectProperties(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getDisjointObjectProperties|" , property ).toString();
     }

     public RacerResult owlapiGetDisjointObjectProperties$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getDisjointObjectProperties|" , property );
     }

     public String owlapiGetDisjointObjectProperties(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDisjointObjectProperties|" , property, reasoner ).toString();
     }

     public RacerResult owlapiGetDisjointObjectProperties$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDisjointObjectProperties|" , property, reasoner );
     }

/** Racer Function owlapi-getdomains
(|OWLAPI-getDomains| owlapi:property
                     &optional
                     owlapi:reasoner
                     owlapi::owlapi-hacking-mode)
 */

     public String owlapiGetDomains(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getDomains|" , property ).toString();
     }

     public RacerResult owlapiGetDomains$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getDomains|" , property );
     }

     public String owlapiGetDomains(Object property, Object reasoner, Object owlapiHackingMode ) throws RacerClientException {
          return racerCall("|OWLAPI-getDomains|" , property, reasoner, owlapiHackingMode ).toString();
     }

     public RacerResult owlapiGetDomains$(Object property, Object reasoner, Object owlapiHackingMode ) throws RacerClientException {
          return racerCall("|OWLAPI-getDomains|" , property, reasoner, owlapiHackingMode );
     }

     public String owlapiGetDomains(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDomains|" , property, reasoner ).toString();
     }

     public RacerResult owlapiGetDomains$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getDomains|" , property, reasoner );
     }

/** Racer Function owlapi-getequivalentclasses
(|OWLAPI-getEquivalentClasses| owlapi:cls &optional owlapi:reasoner)
 */

     public String owlapiGetEquivalentClasses(Object cls ) throws RacerClientException {
          return racerCall("|OWLAPI-getEquivalentClasses|" , cls ).toString();
     }

     public RacerResult owlapiGetEquivalentClasses$(Object cls ) throws RacerClientException {
          return racerCall("|OWLAPI-getEquivalentClasses|" , cls );
     }

     public String owlapiGetEquivalentClasses(Object cls, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getEquivalentClasses|" , cls, reasoner ).toString();
     }

     public RacerResult owlapiGetEquivalentClasses$(Object cls, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getEquivalentClasses|" , cls, reasoner );
     }

/** Racer Function owlapi-getequivalentproperties
(|OWLAPI-getEquivalentProperties| owlapi:property
                                  &optional
                                  owlapi:reasoner
                                  owlapi::remove-self-p)
 */

     public String owlapiGetEquivalentProperties(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getEquivalentProperties|" , property ).toString();
     }

     public RacerResult owlapiGetEquivalentProperties$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getEquivalentProperties|" , property );
     }

     public String owlapiGetEquivalentProperties(Object property, Object reasoner, Object removeSelfP ) throws RacerClientException {
          return racerCall("|OWLAPI-getEquivalentProperties|" , property, reasoner, removeSelfP ).toString();
     }

     public RacerResult owlapiGetEquivalentProperties$(Object property, Object reasoner, Object removeSelfP ) throws RacerClientException {
          return racerCall("|OWLAPI-getEquivalentProperties|" , property, reasoner, removeSelfP );
     }

     public String owlapiGetEquivalentProperties(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getEquivalentProperties|" , property, reasoner ).toString();
     }

     public RacerResult owlapiGetEquivalentProperties$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getEquivalentProperties|" , property, reasoner );
     }

/** Racer Function owlapi-getinconsistentclasses
(|OWLAPI-getInconsistentClasses| &optional owlapi:reasoner)
 */

     public String owlapiGetInconsistentClasses( ) throws RacerClientException {
          return racerCall("|OWLAPI-getInconsistentClasses|"  ).toString();
     }

     public RacerResult owlapiGetInconsistentClasses$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getInconsistentClasses|"  );
     }

     public String owlapiGetInconsistentClasses(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getInconsistentClasses|" , reasoner ).toString();
     }

     public RacerResult owlapiGetInconsistentClasses$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getInconsistentClasses|" , reasoner );
     }

/** Racer Function owlapi-getindividuals
(|OWLAPI-getIndividuals| class owlapi::direct &optional owlapi:reasoner)
 */

     public String owlapiGetIndividuals(Object cls1, Object direct ) throws RacerClientException {
          return racerCall("|OWLAPI-getIndividuals|" , cls1, direct ).toString();
     }

     public RacerResult owlapiGetIndividuals$(Object cls1, Object direct ) throws RacerClientException {
          return racerCall("|OWLAPI-getIndividuals|" , cls1, direct );
     }

     public String owlapiGetIndividuals(Object cls2, Object direct, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getIndividuals|" , cls2, direct, reasoner ).toString();
     }

     public RacerResult owlapiGetIndividuals$(Object cls2, Object direct, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getIndividuals|" , cls2, direct, reasoner );
     }

/** Racer Function owlapi-getinstances
(|OWLAPI-getInstances| class
                       owlapi::direct
                       &optional
                       owlapi:reasoner
                       owlapi::synonyms)
 */

     public String owlapiGetInstances(Object cls3, Object direct ) throws RacerClientException {
          return racerCall("|OWLAPI-getInstances|" , cls3, direct ).toString();
     }

     public RacerResult owlapiGetInstances$(Object cls3, Object direct ) throws RacerClientException {
          return racerCall("|OWLAPI-getInstances|" , cls3, direct );
     }

     public String owlapiGetInstances(Object cls4, Object direct, Object reasoner, Object synonyms ) throws RacerClientException {
          return racerCall("|OWLAPI-getInstances|" , cls4, direct, reasoner, synonyms ).toString();
     }

     public RacerResult owlapiGetInstances$(Object cls4, Object direct, Object reasoner, Object synonyms ) throws RacerClientException {
          return racerCall("|OWLAPI-getInstances|" , cls4, direct, reasoner, synonyms );
     }

     public String owlapiGetInstances(Object cls5, Object direct, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getInstances|" , cls5, direct, reasoner ).toString();
     }

     public RacerResult owlapiGetInstances$(Object cls5, Object direct, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getInstances|" , cls5, direct, reasoner );
     }

/** Racer Function owlapi-getinverseproperties
(|OWLAPI-getInverseProperties| owlapi:property
                               &optional
                               owlapi:reasoner)
 */

     public String owlapiGetInverseProperties(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getInverseProperties|" , property ).toString();
     }

     public RacerResult owlapiGetInverseProperties$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getInverseProperties|" , property );
     }

     public String owlapiGetInverseProperties(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getInverseProperties|" , property, reasoner ).toString();
     }

     public RacerResult owlapiGetInverseProperties$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getInverseProperties|" , property, reasoner );
     }

/** Racer Function owlapi-getloadedontologies
(|OWLAPI-getLoadedOntologies| &optional owlapi:reasoner)
 */

     public String owlapiGetLoadedOntologies( ) throws RacerClientException {
          return racerCall("|OWLAPI-getLoadedOntologies|"  ).toString();
     }

     public RacerResult owlapiGetLoadedOntologies$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getLoadedOntologies|"  );
     }

     public String owlapiGetLoadedOntologies(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getLoadedOntologies|" , reasoner ).toString();
     }

     public RacerResult owlapiGetLoadedOntologies$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getLoadedOntologies|" , reasoner );
     }

/** Racer Function owlapi-getobjectpropertyrelationships
(|OWLAPI-getObjectPropertyRelationships| owlapi::ind
                                         &optional
                                         owlapi:reasoner)
 */

     public String owlapiGetObjectPropertyRelationships(Object ind ) throws RacerClientException {
          return racerCall("|OWLAPI-getObjectPropertyRelationships|" , ind ).toString();
     }

     public RacerResult owlapiGetObjectPropertyRelationships$(Object ind ) throws RacerClientException {
          return racerCall("|OWLAPI-getObjectPropertyRelationships|" , ind );
     }

     public String owlapiGetObjectPropertyRelationships(Object ind, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getObjectPropertyRelationships|" , ind, reasoner ).toString();
     }

     public RacerResult owlapiGetObjectPropertyRelationships$(Object ind, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getObjectPropertyRelationships|" , ind, reasoner );
     }

/** Racer Function owlapi-getobjectpropertyvalues
(|OWLAPI-getObjectPropertyValues| owlapi::ind
                                  owlapi:property
                                  &optional
                                  owlapi:reasoner
                                  owlapi::synonyms)
 */

     public String owlapiGetObjectPropertyValues(Object ind, Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getObjectPropertyValues|" , ind, property ).toString();
     }

     public RacerResult owlapiGetObjectPropertyValues$(Object ind, Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getObjectPropertyValues|" , ind, property );
     }

     public String owlapiGetObjectPropertyValues(Object ind, Object property, Object reasoner, Object synonyms ) throws RacerClientException {
          return racerCall("|OWLAPI-getObjectPropertyValues|" , ind, property, reasoner, synonyms ).toString();
     }

     public RacerResult owlapiGetObjectPropertyValues$(Object ind, Object property, Object reasoner, Object synonyms ) throws RacerClientException {
          return racerCall("|OWLAPI-getObjectPropertyValues|" , ind, property, reasoner, synonyms );
     }

     public String owlapiGetObjectPropertyValues(Object ind, Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getObjectPropertyValues|" , ind, property, reasoner ).toString();
     }

     public RacerResult owlapiGetObjectPropertyValues$(Object ind, Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getObjectPropertyValues|" , ind, property, reasoner );
     }

/** Racer Function owlapi-getontologies
(|OWLAPI-getOntologies| &optional owlapi:reasoner)
 */

     public String owlapiGetOntologies( ) throws RacerClientException {
          return racerCall("|OWLAPI-getOntologies|"  ).toString();
     }

     public RacerResult owlapiGetOntologies$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getOntologies|"  );
     }

     public String owlapiGetOntologies(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOntologies|" , reasoner ).toString();
     }

     public RacerResult owlapiGetOntologies$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOntologies|" , reasoner );
     }

/** Racer Function owlapi-getowlannotationassertionaxiom
(|OWLAPI-getOWLAnnotationAssertionAxiom| owlapi:annotation-subject
                                         owlapi:annotation-property
                                         owlapi:annotation-value
                                         &optional
                                         owlapi:reasoner)
 */

     public String owlapiGetOWLAnnotationAssertionAxiom(Object annotationSubject, Object annotationProperty, Object annotationValue ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAnnotationAssertionAxiom|" , annotationSubject, annotationProperty, annotationValue ).toString();
     }

     public RacerResult owlapiGetOWLAnnotationAssertionAxiom$(Object annotationSubject, Object annotationProperty, Object annotationValue ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAnnotationAssertionAxiom|" , annotationSubject, annotationProperty, annotationValue );
     }

     public String owlapiGetOWLAnnotationAssertionAxiom(Object annotationSubject, Object annotationProperty, Object annotationValue, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAnnotationAssertionAxiom|" , annotationSubject, annotationProperty, annotationValue, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLAnnotationAssertionAxiom$(Object annotationSubject, Object annotationProperty, Object annotationValue, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAnnotationAssertionAxiom|" , annotationSubject, annotationProperty, annotationValue, reasoner );
     }

/** Racer Function owlapi-getowlannotationpropertydomainaxiom
(|OWLAPI-getOWLAnnotationPropertyDomainAxiom| owlapi:annotation-property
                                              owlapi:annotation-property-domain
                                              &optional
                                              owlapi:reasoner)
 */

     public String owlapiGetOWLAnnotationPropertyDomainAxiom(Object annotationProperty, Object annotationPropertyDomain ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAnnotationPropertyDomainAxiom|" , annotationProperty, annotationPropertyDomain ).toString();
     }

     public RacerResult owlapiGetOWLAnnotationPropertyDomainAxiom$(Object annotationProperty, Object annotationPropertyDomain ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAnnotationPropertyDomainAxiom|" , annotationProperty, annotationPropertyDomain );
     }

     public String owlapiGetOWLAnnotationPropertyDomainAxiom(Object annotationProperty, Object annotationPropertyDomain, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAnnotationPropertyDomainAxiom|" , annotationProperty, annotationPropertyDomain, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLAnnotationPropertyDomainAxiom$(Object annotationProperty, Object annotationPropertyDomain, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAnnotationPropertyDomainAxiom|" , annotationProperty, annotationPropertyDomain, reasoner );
     }

/** Racer Function owlapi-getowlannotationpropertyrangeaxiom
(|OWLAPI-getOWLAnnotationPropertyRangeAxiom| owlapi:annotation-property
                                             owlapi:annotation-property-range
                                             &optional
                                             owlapi:reasoner)
 */

     public String owlapiGetOWLAnnotationPropertyRangeAxiom(Object annotationProperty, Object annotationPropertyRange ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAnnotationPropertyRangeAxiom|" , annotationProperty, annotationPropertyRange ).toString();
     }

     public RacerResult owlapiGetOWLAnnotationPropertyRangeAxiom$(Object annotationProperty, Object annotationPropertyRange ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAnnotationPropertyRangeAxiom|" , annotationProperty, annotationPropertyRange );
     }

     public String owlapiGetOWLAnnotationPropertyRangeAxiom(Object annotationProperty, Object annotationPropertyRange, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAnnotationPropertyRangeAxiom|" , annotationProperty, annotationPropertyRange, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLAnnotationPropertyRangeAxiom$(Object annotationProperty, Object annotationPropertyRange, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAnnotationPropertyRangeAxiom|" , annotationProperty, annotationPropertyRange, reasoner );
     }

/** Racer Function owlapi-getowlasymmetricobjectpropertyaxiom
(|OWLAPI-getOWLAsymmetricObjectPropertyAxiom| owlapi:object-property
                                              &optional
                                              owlapi:reasoner)
 */

     public String owlapiGetOWLAsymmetricObjectPropertyAxiom(Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAsymmetricObjectPropertyAxiom|" , objectProperty ).toString();
     }

     public RacerResult owlapiGetOWLAsymmetricObjectPropertyAxiom$(Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAsymmetricObjectPropertyAxiom|" , objectProperty );
     }

     public String owlapiGetOWLAsymmetricObjectPropertyAxiom(Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAsymmetricObjectPropertyAxiom|" , objectProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLAsymmetricObjectPropertyAxiom$(Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAsymmetricObjectPropertyAxiom|" , objectProperty, reasoner );
     }

/** Racer Function owlapi-getowlaxiomannotationaxiom
(|OWLAPI-getOWLAxiomAnnotationAxiom| owlapi:axiom-id
                                     owlapi:annotation
                                     &optional
                                     owlapi:reasoner)
 */

     public String owlapiGetOWLAxiomAnnotationAxiom(Object axiomId, Object annotation ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAxiomAnnotationAxiom|" , axiomId, annotation ).toString();
     }

     public RacerResult owlapiGetOWLAxiomAnnotationAxiom$(Object axiomId, Object annotation ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAxiomAnnotationAxiom|" , axiomId, annotation );
     }

     public String owlapiGetOWLAxiomAnnotationAxiom(Object axiomId, Object annotation, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAxiomAnnotationAxiom|" , axiomId, annotation, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLAxiomAnnotationAxiom$(Object axiomId, Object annotation, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLAxiomAnnotationAxiom|" , axiomId, annotation, reasoner );
     }

/** Racer Function owlapi-getowlclassassertionaxiom
(|OWLAPI-getOWLClassAssertionAxiom| owlapi:individual
                                    owlapi:description
                                    &optional
                                    owlapi:reasoner)
 */

     public String owlapiGetOWLClassAssertionAxiom(Object individual, Object description ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLClassAssertionAxiom|" , individual, description ).toString();
     }

     public RacerResult owlapiGetOWLClassAssertionAxiom$(Object individual, Object description ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLClassAssertionAxiom|" , individual, description );
     }

     public String owlapiGetOWLClassAssertionAxiom(Object individual, Object description, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLClassAssertionAxiom|" , individual, description, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLClassAssertionAxiom$(Object individual, Object description, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLClassAssertionAxiom|" , individual, description, reasoner );
     }

/** Racer Function owlapi-getowldatapropertyassertionaxiom
(|OWLAPI-getOWLDataPropertyAssertionAxiom| owlapi:subject
                                           owlapi:rel-data-property
                                           owlapi::value
                                           &optional
                                           owlapi:reasoner)
 */

     public String owlapiGetOWLDataPropertyAssertionAxiom(Object subject, Object relDataProperty, Object value ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataPropertyAssertionAxiom|" , subject, relDataProperty, value ).toString();
     }

     public RacerResult owlapiGetOWLDataPropertyAssertionAxiom$(Object subject, Object relDataProperty, Object value ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataPropertyAssertionAxiom|" , subject, relDataProperty, value );
     }

     public String owlapiGetOWLDataPropertyAssertionAxiom(Object subject, Object relDataProperty, Object value, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataPropertyAssertionAxiom|" , subject, relDataProperty, value, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLDataPropertyAssertionAxiom$(Object subject, Object relDataProperty, Object value, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataPropertyAssertionAxiom|" , subject, relDataProperty, value, reasoner );
     }

/** Racer Function owlapi-getowldatapropertydomainaxiom
(|OWLAPI-getOWLDataPropertyDomainAxiom| owlapi:data-property
                                        owlapi:data-property-domain
                                        &optional
                                        owlapi:reasoner)
 */

     public String owlapiGetOWLDataPropertyDomainAxiom(Object dataProperty, Object dataPropertyDomain ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataPropertyDomainAxiom|" , dataProperty, dataPropertyDomain ).toString();
     }

     public RacerResult owlapiGetOWLDataPropertyDomainAxiom$(Object dataProperty, Object dataPropertyDomain ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataPropertyDomainAxiom|" , dataProperty, dataPropertyDomain );
     }

     public String owlapiGetOWLDataPropertyDomainAxiom(Object dataProperty, Object dataPropertyDomain, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataPropertyDomainAxiom|" , dataProperty, dataPropertyDomain, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLDataPropertyDomainAxiom$(Object dataProperty, Object dataPropertyDomain, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataPropertyDomainAxiom|" , dataProperty, dataPropertyDomain, reasoner );
     }

/** Racer Function owlapi-getowldatapropertyrangeaxiom
(|OWLAPI-getOWLDataPropertyRangeAxiom| owlapi:data-property
                                       owlapi:data-range
                                       &optional
                                       owlapi:reasoner)
 */

     public String owlapiGetOWLDataPropertyRangeAxiom(Object dataProperty, Object dataRange ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataPropertyRangeAxiom|" , dataProperty, dataRange ).toString();
     }

     public RacerResult owlapiGetOWLDataPropertyRangeAxiom$(Object dataProperty, Object dataRange ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataPropertyRangeAxiom|" , dataProperty, dataRange );
     }

     public String owlapiGetOWLDataPropertyRangeAxiom(Object dataProperty, Object dataRange, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataPropertyRangeAxiom|" , dataProperty, dataRange, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLDataPropertyRangeAxiom$(Object dataProperty, Object dataRange, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataPropertyRangeAxiom|" , dataProperty, dataRange, reasoner );
     }

/** Racer Function owlapi-getowldatasubpropertyaxiom
(|OWLAPI-getOWLDataSubPropertyAxiom| owlapi:data-sub-property
                                     owlapi:data-super-property
                                     &optional
                                     owlapi:reasoner)
 */

     public String owlapiGetOWLDataSubPropertyAxiom(Object dataSubProperty, Object dataSuperProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataSubPropertyAxiom|" , dataSubProperty, dataSuperProperty ).toString();
     }

     public RacerResult owlapiGetOWLDataSubPropertyAxiom$(Object dataSubProperty, Object dataSuperProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataSubPropertyAxiom|" , dataSubProperty, dataSuperProperty );
     }

     public String owlapiGetOWLDataSubPropertyAxiom(Object dataSubProperty, Object dataSuperProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataSubPropertyAxiom|" , dataSubProperty, dataSuperProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLDataSubPropertyAxiom$(Object dataSubProperty, Object dataSuperProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDataSubPropertyAxiom|" , dataSubProperty, dataSuperProperty, reasoner );
     }

/** Racer Function owlapi-getowldatatypedefinitionaxiom
(|OWLAPI-getOWLDatatypeDefinitionAxiom| owlapi:datatype-name
                                        owlapi:data-range
                                        &optional
                                        owlapi:reasoner)
 */

     public String owlapiGetOWLDatatypeDefinitionAxiom(Object datatypeName, Object dataRange ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDatatypeDefinitionAxiom|" , datatypeName, dataRange ).toString();
     }

     public RacerResult owlapiGetOWLDatatypeDefinitionAxiom$(Object datatypeName, Object dataRange ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDatatypeDefinitionAxiom|" , datatypeName, dataRange );
     }

     public String owlapiGetOWLDatatypeDefinitionAxiom(Object datatypeName, Object dataRange, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDatatypeDefinitionAxiom|" , datatypeName, dataRange, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLDatatypeDefinitionAxiom$(Object datatypeName, Object dataRange, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDatatypeDefinitionAxiom|" , datatypeName, dataRange, reasoner );
     }

/** Racer Function owlapi-getowldeclarationaxiom
(|OWLAPI-getOWLDeclarationAxiom| owlapi:entity
                                 &optional
                                 owlapi:reasoner)
 */

     public String owlapiGetOWLDeclarationAxiom(Object entity ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDeclarationAxiom|" , entity ).toString();
     }

     public RacerResult owlapiGetOWLDeclarationAxiom$(Object entity ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDeclarationAxiom|" , entity );
     }

     public String owlapiGetOWLDeclarationAxiom(Object entity, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDeclarationAxiom|" , entity, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLDeclarationAxiom$(Object entity, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDeclarationAxiom|" , entity, reasoner );
     }

/** Racer Function owlapi-getowldifferentindividualsaxiom
(|OWLAPI-getOWLDifferentIndividualsAxiom| owlapi:individuals
                                          &optional
                                          owlapi:reasoner)
 */

     public String owlapiGetOWLDifferentIndividualsAxiom(Object individuals ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDifferentIndividualsAxiom|" , individuals ).toString();
     }

     public RacerResult owlapiGetOWLDifferentIndividualsAxiom$(Object individuals ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDifferentIndividualsAxiom|" , individuals );
     }

     public String owlapiGetOWLDifferentIndividualsAxiom(Object individuals, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDifferentIndividualsAxiom|" , individuals, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLDifferentIndividualsAxiom$(Object individuals, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDifferentIndividualsAxiom|" , individuals, reasoner );
     }

/** Racer Function owlapi-getowldisjointclassesaxiom
(|OWLAPI-getOWLDisjointClassesAxiom| owlapi:descriptions
                                     &optional
                                     owlapi:reasoner)
 */

     public String owlapiGetOWLDisjointClassesAxiom(Object descriptions ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointClassesAxiom|" , descriptions ).toString();
     }

     public RacerResult owlapiGetOWLDisjointClassesAxiom$(Object descriptions ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointClassesAxiom|" , descriptions );
     }

     public String owlapiGetOWLDisjointClassesAxiom(Object descriptions, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointClassesAxiom|" , descriptions, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLDisjointClassesAxiom$(Object descriptions, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointClassesAxiom|" , descriptions, reasoner );
     }

/** Racer Function owlapi-getowldisjointdatapropertiesaxiom
(|OWLAPI-getOWLDisjointDataPropertiesAxiom| owlapi:data-properties
                                            &optional
                                            owlapi:reasoner)
 */

     public String owlapiGetOWLDisjointDataPropertiesAxiom(Object dataProperties ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointDataPropertiesAxiom|" , dataProperties ).toString();
     }

     public RacerResult owlapiGetOWLDisjointDataPropertiesAxiom$(Object dataProperties ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointDataPropertiesAxiom|" , dataProperties );
     }

     public String owlapiGetOWLDisjointDataPropertiesAxiom(Object dataProperties, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointDataPropertiesAxiom|" , dataProperties, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLDisjointDataPropertiesAxiom$(Object dataProperties, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointDataPropertiesAxiom|" , dataProperties, reasoner );
     }

/** Racer Function owlapi-getowldisjointobjectpropertiesaxiom
(|OWLAPI-getOWLDisjointObjectPropertiesAxiom| owlapi:object-properties
                                              &optional
                                              owlapi:reasoner)
 */

     public String owlapiGetOWLDisjointObjectPropertiesAxiom(Object objectProperties ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointObjectPropertiesAxiom|" , objectProperties ).toString();
     }

     public RacerResult owlapiGetOWLDisjointObjectPropertiesAxiom$(Object objectProperties ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointObjectPropertiesAxiom|" , objectProperties );
     }

     public String owlapiGetOWLDisjointObjectPropertiesAxiom(Object objectProperties, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointObjectPropertiesAxiom|" , objectProperties, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLDisjointObjectPropertiesAxiom$(Object objectProperties, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointObjectPropertiesAxiom|" , objectProperties, reasoner );
     }

/** Racer Function owlapi-getowldisjointunionaxiom
(|OWLAPI-getOWLDisjointUnionAxiom| owlapi:description
                                   owlapi:descriptions
                                   &optional
                                   owlapi:reasoner)
 */

     public String owlapiGetOWLDisjointUnionAxiom(Object description, Object descriptions ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointUnionAxiom|" , description, descriptions ).toString();
     }

     public RacerResult owlapiGetOWLDisjointUnionAxiom$(Object description, Object descriptions ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointUnionAxiom|" , description, descriptions );
     }

     public String owlapiGetOWLDisjointUnionAxiom(Object description, Object descriptions, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointUnionAxiom|" , description, descriptions, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLDisjointUnionAxiom$(Object description, Object descriptions, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLDisjointUnionAxiom|" , description, descriptions, reasoner );
     }

/** Racer Function owlapi-getowlentityannotationaxiom
(|OWLAPI-getOWLEntityAnnotationAxiom| owlapi:entity
                                      owlapi:annotation
                                      &optional
                                      owlapi:reasoner)
 */

     public String owlapiGetOWLEntityAnnotationAxiom(Object entity, Object annotation ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEntityAnnotationAxiom|" , entity, annotation ).toString();
     }

     public RacerResult owlapiGetOWLEntityAnnotationAxiom$(Object entity, Object annotation ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEntityAnnotationAxiom|" , entity, annotation );
     }

     public String owlapiGetOWLEntityAnnotationAxiom(Object entity, Object annotation, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEntityAnnotationAxiom|" , entity, annotation, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLEntityAnnotationAxiom$(Object entity, Object annotation, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEntityAnnotationAxiom|" , entity, annotation, reasoner );
     }

/** Racer Function owlapi-getowlequivalentclassesaxiom
(|OWLAPI-getOWLEquivalentClassesAxiom| owlapi:descriptions
                                       &optional
                                       owlapi:reasoner)
 */

     public String owlapiGetOWLEquivalentClassesAxiom(Object descriptions ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEquivalentClassesAxiom|" , descriptions ).toString();
     }

     public RacerResult owlapiGetOWLEquivalentClassesAxiom$(Object descriptions ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEquivalentClassesAxiom|" , descriptions );
     }

     public String owlapiGetOWLEquivalentClassesAxiom(Object descriptions, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEquivalentClassesAxiom|" , descriptions, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLEquivalentClassesAxiom$(Object descriptions, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEquivalentClassesAxiom|" , descriptions, reasoner );
     }

/** Racer Function owlapi-getowlequivalentdatapropertiesaxiom
(|OWLAPI-getOWLEquivalentDataPropertiesAxiom| owlapi:data-properties
                                              &optional
                                              owlapi:reasoner)
 */

     public String owlapiGetOWLEquivalentDataPropertiesAxiom(Object dataProperties ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEquivalentDataPropertiesAxiom|" , dataProperties ).toString();
     }

     public RacerResult owlapiGetOWLEquivalentDataPropertiesAxiom$(Object dataProperties ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEquivalentDataPropertiesAxiom|" , dataProperties );
     }

     public String owlapiGetOWLEquivalentDataPropertiesAxiom(Object dataProperties, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEquivalentDataPropertiesAxiom|" , dataProperties, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLEquivalentDataPropertiesAxiom$(Object dataProperties, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEquivalentDataPropertiesAxiom|" , dataProperties, reasoner );
     }

/** Racer Function owlapi-getowlequivalentobjectpropertiesaxiom
(|OWLAPI-getOWLEquivalentObjectPropertiesAxiom| owlapi:object-properties
                                                &optional
                                                owlapi:reasoner)
 */

     public String owlapiGetOWLEquivalentObjectPropertiesAxiom(Object objectProperties ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEquivalentObjectPropertiesAxiom|" , objectProperties ).toString();
     }

     public RacerResult owlapiGetOWLEquivalentObjectPropertiesAxiom$(Object objectProperties ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEquivalentObjectPropertiesAxiom|" , objectProperties );
     }

     public String owlapiGetOWLEquivalentObjectPropertiesAxiom(Object objectProperties, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEquivalentObjectPropertiesAxiom|" , objectProperties, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLEquivalentObjectPropertiesAxiom$(Object objectProperties, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLEquivalentObjectPropertiesAxiom|" , objectProperties, reasoner );
     }

/** Racer Function owlapi-getowlfunctionaldatapropertyaxiom
(|OWLAPI-getOWLFunctionalDataPropertyAxiom| owlapi:data-property
                                            &optional
                                            owlapi:reasoner)
 */

     public String owlapiGetOWLFunctionalDataPropertyAxiom(Object dataProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLFunctionalDataPropertyAxiom|" , dataProperty ).toString();
     }

     public RacerResult owlapiGetOWLFunctionalDataPropertyAxiom$(Object dataProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLFunctionalDataPropertyAxiom|" , dataProperty );
     }

     public String owlapiGetOWLFunctionalDataPropertyAxiom(Object dataProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLFunctionalDataPropertyAxiom|" , dataProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLFunctionalDataPropertyAxiom$(Object dataProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLFunctionalDataPropertyAxiom|" , dataProperty, reasoner );
     }

/** Racer Function owlapi-getowlfunctionalobjectpropertyaxiom
(|OWLAPI-getOWLFunctionalObjectPropertyAxiom| owlapi:object-property
                                              &optional
                                              owlapi:reasoner)
 */

     public String owlapiGetOWLFunctionalObjectPropertyAxiom(Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLFunctionalObjectPropertyAxiom|" , objectProperty ).toString();
     }

     public RacerResult owlapiGetOWLFunctionalObjectPropertyAxiom$(Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLFunctionalObjectPropertyAxiom|" , objectProperty );
     }

     public String owlapiGetOWLFunctionalObjectPropertyAxiom(Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLFunctionalObjectPropertyAxiom|" , objectProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLFunctionalObjectPropertyAxiom$(Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLFunctionalObjectPropertyAxiom|" , objectProperty, reasoner );
     }

/** Racer Function owlapi-getowlhaskeyaxiom
(|OWLAPI-getOWLHasKeyAxiom| owlapi:key-class
                            owlapi:key-object-properties
                            owlapi:key-data-properties
                            &optional
                            owlapi:reasoner)
 */

     public String owlapiGetOWLHasKeyAxiom(Object keyClass, Object keyObjectProperties, Object keyDataProperties ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLHasKeyAxiom|" , keyClass, keyObjectProperties, keyDataProperties ).toString();
     }

     public RacerResult owlapiGetOWLHasKeyAxiom$(Object keyClass, Object keyObjectProperties, Object keyDataProperties ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLHasKeyAxiom|" , keyClass, keyObjectProperties, keyDataProperties );
     }

     public String owlapiGetOWLHasKeyAxiom(Object keyClass, Object keyObjectProperties, Object keyDataProperties, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLHasKeyAxiom|" , keyClass, keyObjectProperties, keyDataProperties, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLHasKeyAxiom$(Object keyClass, Object keyObjectProperties, Object keyDataProperties, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLHasKeyAxiom|" , keyClass, keyObjectProperties, keyDataProperties, reasoner );
     }

/** Racer Function owlapi-getowlimplicitdeclarationaxiom
(|OWLAPI-getOWLImplicitDeclarationAxiom| owlapi:entity
                                         &optional
                                         owlapi:reasoner)
 */

     public String owlapiGetOWLImplicitDeclarationAxiom(Object entity ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLImplicitDeclarationAxiom|" , entity ).toString();
     }

     public RacerResult owlapiGetOWLImplicitDeclarationAxiom$(Object entity ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLImplicitDeclarationAxiom|" , entity );
     }

     public String owlapiGetOWLImplicitDeclarationAxiom(Object entity, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLImplicitDeclarationAxiom|" , entity, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLImplicitDeclarationAxiom$(Object entity, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLImplicitDeclarationAxiom|" , entity, reasoner );
     }

/** Racer Function owlapi-getowlimportsdeclarationaxiom
(|OWLAPI-getOWLImportsDeclarationAxiom| owlapi::ontology-import-uri
                                        &optional
                                        owlapi:reasoner)
 */

     public String owlapiGetOWLImportsDeclarationAxiom(Object ontologyImportUri ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLImportsDeclarationAxiom|" , ontologyImportUri ).toString();
     }

     public RacerResult owlapiGetOWLImportsDeclarationAxiom$(Object ontologyImportUri ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLImportsDeclarationAxiom|" , ontologyImportUri );
     }

     public String owlapiGetOWLImportsDeclarationAxiom(Object ontologyImportUri, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLImportsDeclarationAxiom|" , ontologyImportUri, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLImportsDeclarationAxiom$(Object ontologyImportUri, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLImportsDeclarationAxiom|" , ontologyImportUri, reasoner );
     }

/** Racer Function owlapi-getowlinversefunctionalobjectpropertyaxiom
(|OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom| owlapi:object-property
                                                     &optional
                                                     owlapi:reasoner)
 */

     public String owlapiGetOWLInverseFunctionalObjectPropertyAxiom(Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom|" , objectProperty ).toString();
     }

     public RacerResult owlapiGetOWLInverseFunctionalObjectPropertyAxiom$(Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom|" , objectProperty );
     }

     public String owlapiGetOWLInverseFunctionalObjectPropertyAxiom(Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom|" , objectProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLInverseFunctionalObjectPropertyAxiom$(Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom|" , objectProperty, reasoner );
     }

/** Racer Function owlapi-getowlinverseobjectpropertiesaxiom
(|OWLAPI-getOWLInverseObjectPropertiesAxiom| owlapi:first-object-property
                                             owlapi:second-object-property
                                             &optional
                                             owlapi:reasoner)
 */

     public String owlapiGetOWLInverseObjectPropertiesAxiom(Object firstObjectProperty, Object secondObjectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLInverseObjectPropertiesAxiom|" , firstObjectProperty, secondObjectProperty ).toString();
     }

     public RacerResult owlapiGetOWLInverseObjectPropertiesAxiom$(Object firstObjectProperty, Object secondObjectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLInverseObjectPropertiesAxiom|" , firstObjectProperty, secondObjectProperty );
     }

     public String owlapiGetOWLInverseObjectPropertiesAxiom(Object firstObjectProperty, Object secondObjectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLInverseObjectPropertiesAxiom|" , firstObjectProperty, secondObjectProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLInverseObjectPropertiesAxiom$(Object firstObjectProperty, Object secondObjectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLInverseObjectPropertiesAxiom|" , firstObjectProperty, secondObjectProperty, reasoner );
     }

/** Racer Function owlapi-getowlirreflexiveobjectpropertyaxiom
(|OWLAPI-getOWLIrreflexiveObjectPropertyAxiom| owlapi:object-property
                                               &optional
                                               owlapi:reasoner)
 */

     public String owlapiGetOWLIrreflexiveObjectPropertyAxiom(Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLIrreflexiveObjectPropertyAxiom|" , objectProperty ).toString();
     }

     public RacerResult owlapiGetOWLIrreflexiveObjectPropertyAxiom$(Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLIrreflexiveObjectPropertyAxiom|" , objectProperty );
     }

     public String owlapiGetOWLIrreflexiveObjectPropertyAxiom(Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLIrreflexiveObjectPropertyAxiom|" , objectProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLIrreflexiveObjectPropertyAxiom$(Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLIrreflexiveObjectPropertyAxiom|" , objectProperty, reasoner );
     }

/** Racer Function owlapi-getowlnegativedatapropertyassertionaxiom
(|OWLAPI-getOWLNegativeDataPropertyAssertionAxiom| owlapi:subject
                                                   owlapi:rel-data-property
                                                   owlapi::value
                                                   &optional
                                                   owlapi:reasoner)
 */

     public String owlapiGetOWLNegativeDataPropertyAssertionAxiom(Object subject, Object relDataProperty, Object value ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLNegativeDataPropertyAssertionAxiom|" , subject, relDataProperty, value ).toString();
     }

     public RacerResult owlapiGetOWLNegativeDataPropertyAssertionAxiom$(Object subject, Object relDataProperty, Object value ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLNegativeDataPropertyAssertionAxiom|" , subject, relDataProperty, value );
     }

     public String owlapiGetOWLNegativeDataPropertyAssertionAxiom(Object subject, Object relDataProperty, Object value, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLNegativeDataPropertyAssertionAxiom|" , subject, relDataProperty, value, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLNegativeDataPropertyAssertionAxiom$(Object subject, Object relDataProperty, Object value, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLNegativeDataPropertyAssertionAxiom|" , subject, relDataProperty, value, reasoner );
     }

/** Racer Function owlapi-getowlnegativeobjectpropertyassertionaxiom
(|OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom| owlapi:subject
                                                     owlapi:rel-object-property
                                                     owlapi:object
                                                     &optional
                                                     owlapi:reasoner)
 */

     public String owlapiGetOWLNegativeObjectPropertyAssertionAxiom(Object subject, Object relObjectProperty, Object object ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom|" , subject, relObjectProperty, object ).toString();
     }

     public RacerResult owlapiGetOWLNegativeObjectPropertyAssertionAxiom$(Object subject, Object relObjectProperty, Object object ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom|" , subject, relObjectProperty, object );
     }

     public String owlapiGetOWLNegativeObjectPropertyAssertionAxiom(Object subject, Object relObjectProperty, Object object, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom|" , subject, relObjectProperty, object, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLNegativeObjectPropertyAssertionAxiom$(Object subject, Object relObjectProperty, Object object, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom|" , subject, relObjectProperty, object, reasoner );
     }

/** Racer Function owlapi-getowlobjectpropertyassertionaxiom
(|OWLAPI-getOWLObjectPropertyAssertionAxiom| owlapi:subject
                                             owlapi:rel-object-property
                                             owlapi:object
                                             &optional
                                             owlapi:reasoner)
 */

     public String owlapiGetOWLObjectPropertyAssertionAxiom(Object subject, Object relObjectProperty, Object object ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyAssertionAxiom|" , subject, relObjectProperty, object ).toString();
     }

     public RacerResult owlapiGetOWLObjectPropertyAssertionAxiom$(Object subject, Object relObjectProperty, Object object ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyAssertionAxiom|" , subject, relObjectProperty, object );
     }

     public String owlapiGetOWLObjectPropertyAssertionAxiom(Object subject, Object relObjectProperty, Object object, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyAssertionAxiom|" , subject, relObjectProperty, object, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLObjectPropertyAssertionAxiom$(Object subject, Object relObjectProperty, Object object, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyAssertionAxiom|" , subject, relObjectProperty, object, reasoner );
     }

/** Racer Function owlapi-getowlobjectpropertychainsubpropertyaxiom
(|OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom| owlapi:object-property-chain
                                                    owlapi:object-super-property
                                                    &optional
                                                    owlapi:reasoner)
 */

     public String owlapiGetOWLObjectPropertyChainSubPropertyAxiom(Object objectPropertyChain, Object objectSuperProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom|" , objectPropertyChain, objectSuperProperty ).toString();
     }

     public RacerResult owlapiGetOWLObjectPropertyChainSubPropertyAxiom$(Object objectPropertyChain, Object objectSuperProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom|" , objectPropertyChain, objectSuperProperty );
     }

     public String owlapiGetOWLObjectPropertyChainSubPropertyAxiom(Object objectPropertyChain, Object objectSuperProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom|" , objectPropertyChain, objectSuperProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLObjectPropertyChainSubPropertyAxiom$(Object objectPropertyChain, Object objectSuperProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom|" , objectPropertyChain, objectSuperProperty, reasoner );
     }

/** Racer Function owlapi-getowlobjectpropertydomainaxiom
(|OWLAPI-getOWLObjectPropertyDomainAxiom| owlapi:object-property
                                          owlapi:object-property-domain
                                          &optional
                                          owlapi:reasoner)
 */

     public String owlapiGetOWLObjectPropertyDomainAxiom(Object objectProperty, Object objectPropertyDomain ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyDomainAxiom|" , objectProperty, objectPropertyDomain ).toString();
     }

     public RacerResult owlapiGetOWLObjectPropertyDomainAxiom$(Object objectProperty, Object objectPropertyDomain ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyDomainAxiom|" , objectProperty, objectPropertyDomain );
     }

     public String owlapiGetOWLObjectPropertyDomainAxiom(Object objectProperty, Object objectPropertyDomain, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyDomainAxiom|" , objectProperty, objectPropertyDomain, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLObjectPropertyDomainAxiom$(Object objectProperty, Object objectPropertyDomain, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyDomainAxiom|" , objectProperty, objectPropertyDomain, reasoner );
     }

/** Racer Function owlapi-getowlobjectpropertyrangeaxiom
(|OWLAPI-getOWLObjectPropertyRangeAxiom| owlapi:object-property
                                         owlapi:object-property-range
                                         &optional
                                         owlapi:reasoner)
 */

     public String owlapiGetOWLObjectPropertyRangeAxiom(Object objectProperty, Object objectPropertyRange ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyRangeAxiom|" , objectProperty, objectPropertyRange ).toString();
     }

     public RacerResult owlapiGetOWLObjectPropertyRangeAxiom$(Object objectProperty, Object objectPropertyRange ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyRangeAxiom|" , objectProperty, objectPropertyRange );
     }

     public String owlapiGetOWLObjectPropertyRangeAxiom(Object objectProperty, Object objectPropertyRange, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyRangeAxiom|" , objectProperty, objectPropertyRange, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLObjectPropertyRangeAxiom$(Object objectProperty, Object objectPropertyRange, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectPropertyRangeAxiom|" , objectProperty, objectPropertyRange, reasoner );
     }

/** Racer Function owlapi-getowlobjectsubpropertyaxiom
(|OWLAPI-getOWLObjectSubPropertyAxiom| owlapi:object-sub-property
                                       owlapi:object-super-property
                                       &optional
                                       owlapi:reasoner)
 */

     public String owlapiGetOWLObjectSubPropertyAxiom(Object objectSubProperty, Object objectSuperProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectSubPropertyAxiom|" , objectSubProperty, objectSuperProperty ).toString();
     }

     public RacerResult owlapiGetOWLObjectSubPropertyAxiom$(Object objectSubProperty, Object objectSuperProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectSubPropertyAxiom|" , objectSubProperty, objectSuperProperty );
     }

     public String owlapiGetOWLObjectSubPropertyAxiom(Object objectSubProperty, Object objectSuperProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectSubPropertyAxiom|" , objectSubProperty, objectSuperProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLObjectSubPropertyAxiom$(Object objectSubProperty, Object objectSuperProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLObjectSubPropertyAxiom|" , objectSubProperty, objectSuperProperty, reasoner );
     }

/** Racer Function owlapi-getowlontologyannotationaxiom
(|OWLAPI-getOWLOntologyAnnotationAxiom| owlapi:annotation
                                        &optional
                                        owlapi:reasoner)
 */

     public String owlapiGetOWLOntologyAnnotationAxiom(Object annotation ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLOntologyAnnotationAxiom|" , annotation ).toString();
     }

     public RacerResult owlapiGetOWLOntologyAnnotationAxiom$(Object annotation ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLOntologyAnnotationAxiom|" , annotation );
     }

     public String owlapiGetOWLOntologyAnnotationAxiom(Object annotation, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLOntologyAnnotationAxiom|" , annotation, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLOntologyAnnotationAxiom$(Object annotation, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLOntologyAnnotationAxiom|" , annotation, reasoner );
     }

/** Racer Function owlapi-getowlontologyversiondeclarationaxiom
(|OWLAPI-getOWLOntologyVersionDeclarationAxiom| owlapi::ontology-version-uri
                                                &optional
                                                owlapi:reasoner)
 */

     public String owlapiGetOWLOntologyVersionDeclarationAxiom(Object ontologyVersionUri ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLOntologyVersionDeclarationAxiom|" , ontologyVersionUri ).toString();
     }

     public RacerResult owlapiGetOWLOntologyVersionDeclarationAxiom$(Object ontologyVersionUri ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLOntologyVersionDeclarationAxiom|" , ontologyVersionUri );
     }

     public String owlapiGetOWLOntologyVersionDeclarationAxiom(Object ontologyVersionUri, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLOntologyVersionDeclarationAxiom|" , ontologyVersionUri, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLOntologyVersionDeclarationAxiom$(Object ontologyVersionUri, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLOntologyVersionDeclarationAxiom|" , ontologyVersionUri, reasoner );
     }

/** Racer Function owlapi-getowlprefixdeclarationaxiom
(|OWLAPI-getOWLPrefixDeclarationAxiom| owlapi::namespace-prefix
                                       owlapi:namespace
                                       &optional
                                       owlapi:reasoner)
 */

     public String owlapiGetOWLPrefixDeclarationAxiom(Object namespacePrefix, Object namespace ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLPrefixDeclarationAxiom|" , namespacePrefix, namespace ).toString();
     }

     public RacerResult owlapiGetOWLPrefixDeclarationAxiom$(Object namespacePrefix, Object namespace ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLPrefixDeclarationAxiom|" , namespacePrefix, namespace );
     }

     public String owlapiGetOWLPrefixDeclarationAxiom(Object namespacePrefix, Object namespace, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLPrefixDeclarationAxiom|" , namespacePrefix, namespace, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLPrefixDeclarationAxiom$(Object namespacePrefix, Object namespace, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLPrefixDeclarationAxiom|" , namespacePrefix, namespace, reasoner );
     }

/** Racer Function owlapi-getowlreallyimplicitdeclarationaxiom
(|OWLAPI-getOWLReallyImplicitDeclarationAxiom| owlapi:entity
                                               &optional
                                               owlapi:reasoner)
 */

     public String owlapiGetOWLReallyImplicitDeclarationAxiom(Object entity ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLReallyImplicitDeclarationAxiom|" , entity ).toString();
     }

     public RacerResult owlapiGetOWLReallyImplicitDeclarationAxiom$(Object entity ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLReallyImplicitDeclarationAxiom|" , entity );
     }

     public String owlapiGetOWLReallyImplicitDeclarationAxiom(Object entity, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLReallyImplicitDeclarationAxiom|" , entity, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLReallyImplicitDeclarationAxiom$(Object entity, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLReallyImplicitDeclarationAxiom|" , entity, reasoner );
     }

/** Racer Function owlapi-getowlreflexiveobjectpropertyaxiom
(|OWLAPI-getOWLReflexiveObjectPropertyAxiom| owlapi:object-property
                                             &optional
                                             owlapi:reasoner)
 */

     public String owlapiGetOWLReflexiveObjectPropertyAxiom(Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLReflexiveObjectPropertyAxiom|" , objectProperty ).toString();
     }

     public RacerResult owlapiGetOWLReflexiveObjectPropertyAxiom$(Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLReflexiveObjectPropertyAxiom|" , objectProperty );
     }

     public String owlapiGetOWLReflexiveObjectPropertyAxiom(Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLReflexiveObjectPropertyAxiom|" , objectProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLReflexiveObjectPropertyAxiom$(Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLReflexiveObjectPropertyAxiom|" , objectProperty, reasoner );
     }

/** Racer Function owlapi-getowlsameindividualsaxiom
(|OWLAPI-getOWLSameIndividualsAxiom| owlapi:individuals
                                     &optional
                                     owlapi:reasoner)
 */

     public String owlapiGetOWLSameIndividualsAxiom(Object individuals ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSameIndividualsAxiom|" , individuals ).toString();
     }

     public RacerResult owlapiGetOWLSameIndividualsAxiom$(Object individuals ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSameIndividualsAxiom|" , individuals );
     }

     public String owlapiGetOWLSameIndividualsAxiom(Object individuals, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSameIndividualsAxiom|" , individuals, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLSameIndividualsAxiom$(Object individuals, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSameIndividualsAxiom|" , individuals, reasoner );
     }

/** Racer Function owlapi-getowlsubannotationpropertyaxiom
(|OWLAPI-getOWLSubAnnotationPropertyAxiom|)
 */

     public String owlapiGetOWLSubAnnotationPropertyAxiom( ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSubAnnotationPropertyAxiom|"  ).toString();
     }

     public RacerResult owlapiGetOWLSubAnnotationPropertyAxiom$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSubAnnotationPropertyAxiom|"  );
     }

/** Racer Function owlapi-getowlsubannotationpropertyofaxiom
(|OWLAPI-getOWLSubAnnotationPropertyOfAxiom| owlapi:annotation-sub-property
                                             owlapi:annotation-super-property
                                             &optional
                                             owlapi:reasoner)
 */

     public String owlapiGetOWLSubAnnotationPropertyOfAxiom(Object annotationSubProperty, Object annotationSuperProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSubAnnotationPropertyOfAxiom|" , annotationSubProperty, annotationSuperProperty ).toString();
     }

     public RacerResult owlapiGetOWLSubAnnotationPropertyOfAxiom$(Object annotationSubProperty, Object annotationSuperProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSubAnnotationPropertyOfAxiom|" , annotationSubProperty, annotationSuperProperty );
     }

     public String owlapiGetOWLSubAnnotationPropertyOfAxiom(Object annotationSubProperty, Object annotationSuperProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSubAnnotationPropertyOfAxiom|" , annotationSubProperty, annotationSuperProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLSubAnnotationPropertyOfAxiom$(Object annotationSubProperty, Object annotationSuperProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSubAnnotationPropertyOfAxiom|" , annotationSubProperty, annotationSuperProperty, reasoner );
     }

/** Racer Function owlapi-getowlsubclassaxiom
(|OWLAPI-getOWLSubClassAxiom| owlapi:sub-class
                              owlapi:super-class
                              &optional
                              owlapi:reasoner)
 */

     public String owlapiGetOWLSubClassAxiom(Object subClass, Object superClass ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSubClassAxiom|" , subClass, superClass ).toString();
     }

     public RacerResult owlapiGetOWLSubClassAxiom$(Object subClass, Object superClass ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSubClassAxiom|" , subClass, superClass );
     }

     public String owlapiGetOWLSubClassAxiom(Object subClass, Object superClass, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSubClassAxiom|" , subClass, superClass, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLSubClassAxiom$(Object subClass, Object superClass, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSubClassAxiom|" , subClass, superClass, reasoner );
     }

/** Racer Function owlapi-getowlsymmetricobjectpropertyaxiom
(|OWLAPI-getOWLSymmetricObjectPropertyAxiom| owlapi:object-property
                                             &optional
                                             owlapi:reasoner)
 */

     public String owlapiGetOWLSymmetricObjectPropertyAxiom(Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSymmetricObjectPropertyAxiom|" , objectProperty ).toString();
     }

     public RacerResult owlapiGetOWLSymmetricObjectPropertyAxiom$(Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSymmetricObjectPropertyAxiom|" , objectProperty );
     }

     public String owlapiGetOWLSymmetricObjectPropertyAxiom(Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSymmetricObjectPropertyAxiom|" , objectProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLSymmetricObjectPropertyAxiom$(Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLSymmetricObjectPropertyAxiom|" , objectProperty, reasoner );
     }

/** Racer Function owlapi-getowltransitiveobjectpropertyaxiom
(|OWLAPI-getOWLTransitiveObjectPropertyAxiom| owlapi:object-property
                                              &optional
                                              owlapi:reasoner)
 */

     public String owlapiGetOWLTransitiveObjectPropertyAxiom(Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLTransitiveObjectPropertyAxiom|" , objectProperty ).toString();
     }

     public RacerResult owlapiGetOWLTransitiveObjectPropertyAxiom$(Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLTransitiveObjectPropertyAxiom|" , objectProperty );
     }

     public String owlapiGetOWLTransitiveObjectPropertyAxiom(Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLTransitiveObjectPropertyAxiom|" , objectProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetOWLTransitiveObjectPropertyAxiom$(Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getOWLTransitiveObjectPropertyAxiom|" , objectProperty, reasoner );
     }

/** Racer Function owlapi-getprefixes
(|OWLAPI-getPrefixes| &optional owlapi:reasoner)
 */

     public String owlapiGetPrefixes( ) throws RacerClientException {
          return racerCall("|OWLAPI-getPrefixes|"  ).toString();
     }

     public RacerResult owlapiGetPrefixes$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getPrefixes|"  );
     }

     public String owlapiGetPrefixes(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getPrefixes|" , reasoner ).toString();
     }

     public RacerResult owlapiGetPrefixes$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getPrefixes|" , reasoner );
     }

/** Racer Function owlapi-getranges
(|OWLAPI-getRanges| owlapi:property
                    &optional
                    owlapi:reasoner
                    owlapi::owlapi-hacking-mode)
 */

     public String owlapiGetRanges(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getRanges|" , property ).toString();
     }

     public RacerResult owlapiGetRanges$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getRanges|" , property );
     }

     public String owlapiGetRanges(Object property, Object reasoner, Object owlapiHackingMode ) throws RacerClientException {
          return racerCall("|OWLAPI-getRanges|" , property, reasoner, owlapiHackingMode ).toString();
     }

     public RacerResult owlapiGetRanges$(Object property, Object reasoner, Object owlapiHackingMode ) throws RacerClientException {
          return racerCall("|OWLAPI-getRanges|" , property, reasoner, owlapiHackingMode );
     }

     public String owlapiGetRanges(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getRanges|" , property, reasoner ).toString();
     }

     public RacerResult owlapiGetRanges$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getRanges|" , property, reasoner );
     }

/** Racer Function owlapi-getreasoners
(|OWLAPI-getReasoners|)
 */

     public String owlapiGetReasoners( ) throws RacerClientException {
          return racerCall("|OWLAPI-getReasoners|"  ).toString();
     }

     public RacerResult owlapiGetReasoners$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getReasoners|"  );
     }

/** Racer Function owlapi-getrelatedindividuals
(|OWLAPI-getRelatedIndividuals| owlapi:subject
                                owlapi:object-property
                                &optional
                                owlapi:reasoner)
 */

     public String owlapiGetRelatedIndividuals(Object subject, Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getRelatedIndividuals|" , subject, objectProperty ).toString();
     }

     public RacerResult owlapiGetRelatedIndividuals$(Object subject, Object objectProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getRelatedIndividuals|" , subject, objectProperty );
     }

     public String owlapiGetRelatedIndividuals(Object subject, Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getRelatedIndividuals|" , subject, objectProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetRelatedIndividuals$(Object subject, Object objectProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getRelatedIndividuals|" , subject, objectProperty, reasoner );
     }

/** Racer Function owlapi-getrelatedvalues
(|OWLAPI-getRelatedValues| owlapi:subject
                           owlapi:data-property
                           &optional
                           owlapi:reasoner)
 */

     public String owlapiGetRelatedValues(Object subject, Object dataProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getRelatedValues|" , subject, dataProperty ).toString();
     }

     public RacerResult owlapiGetRelatedValues$(Object subject, Object dataProperty ) throws RacerClientException {
          return racerCall("|OWLAPI-getRelatedValues|" , subject, dataProperty );
     }

     public String owlapiGetRelatedValues(Object subject, Object dataProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getRelatedValues|" , subject, dataProperty, reasoner ).toString();
     }

     public RacerResult owlapiGetRelatedValues$(Object subject, Object dataProperty, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getRelatedValues|" , subject, dataProperty, reasoner );
     }

/** Racer Function owlapi-getsameindividuals
(|OWLAPI-getSameIndividuals| owlapi::ind &optional owlapi:reasoner)
 */

     public String owlapiGetSameIndividuals(Object ind ) throws RacerClientException {
          return racerCall("|OWLAPI-getSameIndividuals|" , ind ).toString();
     }

     public RacerResult owlapiGetSameIndividuals$(Object ind ) throws RacerClientException {
          return racerCall("|OWLAPI-getSameIndividuals|" , ind );
     }

     public String owlapiGetSameIndividuals(Object ind, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getSameIndividuals|" , ind, reasoner ).toString();
     }

     public RacerResult owlapiGetSameIndividuals$(Object ind, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getSameIndividuals|" , ind, reasoner );
     }

/** Racer Function owlapi-getsubclasses
(|OWLAPI-getSubClasses| owlapi:cls &optional owlapi:reasoner)
 */

     public String owlapiGetSubClasses(Object cls ) throws RacerClientException {
          return racerCall("|OWLAPI-getSubClasses|" , cls ).toString();
     }

     public RacerResult owlapiGetSubClasses$(Object cls ) throws RacerClientException {
          return racerCall("|OWLAPI-getSubClasses|" , cls );
     }

     public String owlapiGetSubClasses(Object cls, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getSubClasses|" , cls, reasoner ).toString();
     }

     public RacerResult owlapiGetSubClasses$(Object cls, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getSubClasses|" , cls, reasoner );
     }

/** Racer Function owlapi-getsubproperties
(|OWLAPI-getSubProperties| owlapi:property &optional owlapi:reasoner)
 */

     public String owlapiGetSubProperties(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getSubProperties|" , property ).toString();
     }

     public RacerResult owlapiGetSubProperties$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getSubProperties|" , property );
     }

     public String owlapiGetSubProperties(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getSubProperties|" , property, reasoner ).toString();
     }

     public RacerResult owlapiGetSubProperties$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getSubProperties|" , property, reasoner );
     }

/** Racer Function owlapi-getsuperclasses
(|OWLAPI-getSuperClasses| owlapi:cls &optional owlapi:reasoner)
 */

     public String owlapiGetSuperClasses(Object cls ) throws RacerClientException {
          return racerCall("|OWLAPI-getSuperClasses|" , cls ).toString();
     }

     public RacerResult owlapiGetSuperClasses$(Object cls ) throws RacerClientException {
          return racerCall("|OWLAPI-getSuperClasses|" , cls );
     }

     public String owlapiGetSuperClasses(Object cls, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getSuperClasses|" , cls, reasoner ).toString();
     }

     public RacerResult owlapiGetSuperClasses$(Object cls, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getSuperClasses|" , cls, reasoner );
     }

/** Racer Function owlapi-getsuperproperties
(|OWLAPI-getSuperProperties| owlapi:property &optional owlapi:reasoner)
 */

     public String owlapiGetSuperProperties(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getSuperProperties|" , property ).toString();
     }

     public RacerResult owlapiGetSuperProperties$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-getSuperProperties|" , property );
     }

     public String owlapiGetSuperProperties(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getSuperProperties|" , property, reasoner ).toString();
     }

     public RacerResult owlapiGetSuperProperties$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getSuperProperties|" , property, reasoner );
     }

/** Racer Function owlapi-gettypes
(|OWLAPI-getTypes| owlapi:individual
                   owlapi::direct
                   &optional
                   owlapi:reasoner)
 */

     public String owlapiGetTypes(Object individual, Object direct ) throws RacerClientException {
          return racerCall("|OWLAPI-getTypes|" , individual, direct ).toString();
     }

     public RacerResult owlapiGetTypes$(Object individual, Object direct ) throws RacerClientException {
          return racerCall("|OWLAPI-getTypes|" , individual, direct );
     }

     public String owlapiGetTypes(Object individual, Object direct, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getTypes|" , individual, direct, reasoner ).toString();
     }

     public RacerResult owlapiGetTypes$(Object individual, Object direct, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getTypes|" , individual, direct, reasoner );
     }

/** Racer Function owlapi-hasdatapropertyrelationship
(|OWLAPI-hasDataPropertyRelationship| owlapi:subject
                                      owlapi:property
                                      owlapi:object
                                      &optional
                                      owlapi:reasoner)
 */

     public String owlapiHasDataPropertyRelationship(Object subject, Object property, Object object ) throws RacerClientException {
          return racerCall("|OWLAPI-hasDataPropertyRelationship|" , subject, property, object ).toString();
     }

     public RacerResult owlapiHasDataPropertyRelationship$(Object subject, Object property, Object object ) throws RacerClientException {
          return racerCall("|OWLAPI-hasDataPropertyRelationship|" , subject, property, object );
     }

     public String owlapiHasDataPropertyRelationship(Object subject, Object property, Object object, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-hasDataPropertyRelationship|" , subject, property, object, reasoner ).toString();
     }

     public RacerResult owlapiHasDataPropertyRelationship$(Object subject, Object property, Object object, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-hasDataPropertyRelationship|" , subject, property, object, reasoner );
     }

/** Racer Function owlapi-hasobjectpropertyrelationship
(|OWLAPI-hasObjectPropertyRelationship| owlapi:subject
                                        owlapi:property
                                        owlapi:object
                                        &optional
                                        owlapi:reasoner)
 */

     public String owlapiHasObjectPropertyRelationship(Object subject, Object property, Object object ) throws RacerClientException {
          return racerCall("|OWLAPI-hasObjectPropertyRelationship|" , subject, property, object ).toString();
     }

     public RacerResult owlapiHasObjectPropertyRelationship$(Object subject, Object property, Object object ) throws RacerClientException {
          return racerCall("|OWLAPI-hasObjectPropertyRelationship|" , subject, property, object );
     }

     public String owlapiHasObjectPropertyRelationship(Object subject, Object property, Object object, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-hasObjectPropertyRelationship|" , subject, property, object, reasoner ).toString();
     }

     public RacerResult owlapiHasObjectPropertyRelationship$(Object subject, Object property, Object object, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-hasObjectPropertyRelationship|" , subject, property, object, reasoner );
     }

/** Racer Function owlapi-hastype
(|OWLAPI-hasType| owlapi::ind
                  type
                  owlapi::direct
                  &optional
                  owlapi:reasoner)
 */

     public String owlapiHasType(Object ind, Object type, Object direct ) throws RacerClientException {
          return racerCall("|OWLAPI-hasType|" , ind, type, direct ).toString();
     }

     public RacerResult owlapiHasType$(Object ind, Object type, Object direct ) throws RacerClientException {
          return racerCall("|OWLAPI-hasType|" , ind, type, direct );
     }

     public String owlapiHasType(Object ind, Object type, Object direct, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-hasType|" , ind, type, direct, reasoner ).toString();
     }

     public RacerResult owlapiHasType$(Object ind, Object type, Object direct, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-hasType|" , ind, type, direct, reasoner );
     }

/** Racer Function owlapi-idtoaxiom
(|OWLAPI-IDToAxiom| owlapi::id &optional owlapi:reasoner)
 */

     public String owlapiIDToAxiom(Object id ) throws RacerClientException {
          return racerCall("|OWLAPI-IDToAxiom|" , id ).toString();
     }

     public RacerResult owlapiIDToAxiom$(Object id ) throws RacerClientException {
          return racerCall("|OWLAPI-IDToAxiom|" , id );
     }

     public String owlapiIDToAxiom(Object id, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-IDToAxiom|" , id, reasoner ).toString();
     }

     public RacerResult owlapiIDToAxiom$(Object id, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-IDToAxiom|" , id, reasoner );
     }

/** Racer Function owlapi-ignoreannotations
(|OWLAPI-ignoreAnnotations| &optional owlapi:reasoner)
 */

     public String owlapiIgnoreAnnotations( ) throws RacerClientException {
          return racerCall("|OWLAPI-ignoreAnnotations|"  ).toString();
     }

     public RacerResult owlapiIgnoreAnnotations$( ) throws RacerClientException {
          return racerCall("|OWLAPI-ignoreAnnotations|"  );
     }

     public String owlapiIgnoreAnnotations(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-ignoreAnnotations|" , reasoner ).toString();
     }

     public RacerResult owlapiIgnoreAnnotations$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-ignoreAnnotations|" , reasoner );
     }

/** Racer Function owlapi-ignoredeclarations
(|OWLAPI-ignoreDeclarations| &optional owlapi:reasoner)
 */

     public String owlapiIgnoreDeclarations( ) throws RacerClientException {
          return racerCall("|OWLAPI-ignoreDeclarations|"  ).toString();
     }

     public RacerResult owlapiIgnoreDeclarations$( ) throws RacerClientException {
          return racerCall("|OWLAPI-ignoreDeclarations|"  );
     }

     public String owlapiIgnoreDeclarations(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-ignoreDeclarations|" , reasoner ).toString();
     }

     public RacerResult owlapiIgnoreDeclarations$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-ignoreDeclarations|" , reasoner );
     }

/** Racer Function owlapi-isasymmetric
(|OWLAPI-isAsymmetric| owlapi:property &optional owlapi:reasoner)
 */

     public String owlapiIsAsymmetric(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isAsymmetric|" , property ).toString();
     }

     public RacerResult owlapiIsAsymmetric$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isAsymmetric|" , property );
     }

     public String owlapiIsAsymmetric(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isAsymmetric|" , property, reasoner ).toString();
     }

     public RacerResult owlapiIsAsymmetric$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isAsymmetric|" , property, reasoner );
     }

/** Racer Function owlapi-isclass
(|OWLAPI-isClass| owlapi:clsc &optional owlapi:reasoner)
 */

     public String owlapiIsClass(Object clsc ) throws RacerClientException {
          return racerCall("|OWLAPI-isClass|" , clsc ).toString();
     }

     public RacerResult owlapiIsClass$(Object clsc ) throws RacerClientException {
          return racerCall("|OWLAPI-isClass|" , clsc );
     }

     public String owlapiIsClass(Object clsc, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isClass|" , clsc, reasoner ).toString();
     }

     public RacerResult owlapiIsClass$(Object clsc, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isClass|" , clsc, reasoner );
     }

/** Racer Function owlapi-isclassified
(|OWLAPI-isClassified| &optional owlapi:reasoner)
 */

     public String owlapiIsClassified( ) throws RacerClientException {
          return racerCall("|OWLAPI-isClassified|"  ).toString();
     }

     public RacerResult owlapiIsClassified$( ) throws RacerClientException {
          return racerCall("|OWLAPI-isClassified|"  );
     }

     public String owlapiIsClassified(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isClassified|" , reasoner ).toString();
     }

     public RacerResult owlapiIsClassified$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isClassified|" , reasoner );
     }

/** Racer Function owlapi-isconsistent
(|OWLAPI-isConsistent| owlapi:ontology
                       &optional
                       owlapi:reasoner
                       owlapi::force-p)
 */

     public String owlapiIsConsistent(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-isConsistent|" , ontology ).toString();
     }

     public RacerResult owlapiIsConsistent$(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-isConsistent|" , ontology );
     }

     public String owlapiIsConsistent(Object ontology, Object reasoner, Object forceP ) throws RacerClientException {
          return racerCall("|OWLAPI-isConsistent|" , ontology, reasoner, forceP ).toString();
     }

     public RacerResult owlapiIsConsistent$(Object ontology, Object reasoner, Object forceP ) throws RacerClientException {
          return racerCall("|OWLAPI-isConsistent|" , ontology, reasoner, forceP );
     }

     public String owlapiIsConsistent(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isConsistent|" , ontology, reasoner ).toString();
     }

     public RacerResult owlapiIsConsistent$(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isConsistent|" , ontology, reasoner );
     }

/** Racer Function owlapi-isdefinedclass
(|OWLAPI-isDefinedClass| owlapi:cls &optional owlapi:reasoner)
 */

     public String owlapiIsDefinedClass(Object cls ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedClass|" , cls ).toString();
     }

     public RacerResult owlapiIsDefinedClass$(Object cls ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedClass|" , cls );
     }

     public String owlapiIsDefinedClass(Object cls, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedClass|" , cls, reasoner ).toString();
     }

     public RacerResult owlapiIsDefinedClass$(Object cls, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedClass|" , cls, reasoner );
     }

/** Racer Function owlapi-isdefineddataproperty
(|OWLAPI-isDefinedDataProperty| owlapi:property
                                &optional
                                owlapi:reasoner)
 */

     public String owlapiIsDefinedDataProperty(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedDataProperty|" , property ).toString();
     }

     public RacerResult owlapiIsDefinedDataProperty$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedDataProperty|" , property );
     }

     public String owlapiIsDefinedDataProperty(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedDataProperty|" , property, reasoner ).toString();
     }

     public RacerResult owlapiIsDefinedDataProperty$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedDataProperty|" , property, reasoner );
     }

/** Racer Function owlapi-isdefinedindividual
(|OWLAPI-isDefinedIndividual| owlapi::ind &optional owlapi:reasoner)
 */

     public String owlapiIsDefinedIndividual(Object ind ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedIndividual|" , ind ).toString();
     }

     public RacerResult owlapiIsDefinedIndividual$(Object ind ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedIndividual|" , ind );
     }

     public String owlapiIsDefinedIndividual(Object ind, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedIndividual|" , ind, reasoner ).toString();
     }

     public RacerResult owlapiIsDefinedIndividual$(Object ind, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedIndividual|" , ind, reasoner );
     }

/** Racer Function owlapi-isdefinedobjectproperty
(|OWLAPI-isDefinedObjectProperty| owlapi:property
                                  &optional
                                  owlapi:reasoner)
 */

     public String owlapiIsDefinedObjectProperty(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedObjectProperty|" , property ).toString();
     }

     public RacerResult owlapiIsDefinedObjectProperty$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedObjectProperty|" , property );
     }

     public String owlapiIsDefinedObjectProperty(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedObjectProperty|" , property, reasoner ).toString();
     }

     public RacerResult owlapiIsDefinedObjectProperty$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isDefinedObjectProperty|" , property, reasoner );
     }

/** Racer Function owlapi-isdifferentindividual
(|OWLAPI-isDifferentIndividual| owlapi::i
                                owlapi::j
                                &optional
                                owlapi:reasoner)
 */

     public String owlapiIsDifferentIndividual(Object i, Object j ) throws RacerClientException {
          return racerCall("|OWLAPI-isDifferentIndividual|" , i, j ).toString();
     }

     public RacerResult owlapiIsDifferentIndividual$(Object i, Object j ) throws RacerClientException {
          return racerCall("|OWLAPI-isDifferentIndividual|" , i, j );
     }

     public String owlapiIsDifferentIndividual(Object i, Object j, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isDifferentIndividual|" , i, j, reasoner ).toString();
     }

     public RacerResult owlapiIsDifferentIndividual$(Object i, Object j, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isDifferentIndividual|" , i, j, reasoner );
     }

/** Racer Function owlapi-isentailed
(|OWLAPI-isEntailed| owlapi::axiom-id-or-constructor
                     &optional
                     owlapi:reasoner)
 */

     public String owlapiIsEntailed(Object axiomIdOrConstructor ) throws RacerClientException {
          return racerCall("|OWLAPI-isEntailed|" , axiomIdOrConstructor ).toString();
     }

     public RacerResult owlapiIsEntailed$(Object axiomIdOrConstructor ) throws RacerClientException {
          return racerCall("|OWLAPI-isEntailed|" , axiomIdOrConstructor );
     }

     public String owlapiIsEntailed(Object axiomIdOrConstructor, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isEntailed|" , axiomIdOrConstructor, reasoner ).toString();
     }

     public RacerResult owlapiIsEntailed$(Object axiomIdOrConstructor, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isEntailed|" , axiomIdOrConstructor, reasoner );
     }

/** Racer Function owlapi-isequivalentclass
(|OWLAPI-isEquivalentClass| owlapi:clsc
                            owlapi:clsd
                            &optional
                            owlapi:reasoner)
 */

     public String owlapiIsEquivalentClass(Object clsc, Object clsd ) throws RacerClientException {
          return racerCall("|OWLAPI-isEquivalentClass|" , clsc, clsd ).toString();
     }

     public RacerResult owlapiIsEquivalentClass$(Object clsc, Object clsd ) throws RacerClientException {
          return racerCall("|OWLAPI-isEquivalentClass|" , clsc, clsd );
     }

     public String owlapiIsEquivalentClass(Object clsc, Object clsd, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isEquivalentClass|" , clsc, clsd, reasoner ).toString();
     }

     public RacerResult owlapiIsEquivalentClass$(Object clsc, Object clsd, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isEquivalentClass|" , clsc, clsd, reasoner );
     }

/** Racer Function owlapi-isfunctional
(|OWLAPI-isFunctional| owlapi:property &optional owlapi:reasoner)
 */

     public String owlapiIsFunctional(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isFunctional|" , property ).toString();
     }

     public RacerResult owlapiIsFunctional$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isFunctional|" , property );
     }

     public String owlapiIsFunctional(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isFunctional|" , property, reasoner ).toString();
     }

     public RacerResult owlapiIsFunctional$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isFunctional|" , property, reasoner );
     }

/** Racer Function owlapi-isinversefunctional
(|OWLAPI-isInverseFunctional| owlapi:property &optional owlapi:reasoner)
 */

     public String owlapiIsInverseFunctional(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isInverseFunctional|" , property ).toString();
     }

     public RacerResult owlapiIsInverseFunctional$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isInverseFunctional|" , property );
     }

     public String owlapiIsInverseFunctional(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isInverseFunctional|" , property, reasoner ).toString();
     }

     public RacerResult owlapiIsInverseFunctional$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isInverseFunctional|" , property, reasoner );
     }

/** Racer Function owlapi-isirreflexive
(|OWLAPI-isIrreflexive| owlapi:property &optional owlapi:reasoner)
 */

     public String owlapiIsIrreflexive(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isIrreflexive|" , property ).toString();
     }

     public RacerResult owlapiIsIrreflexive$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isIrreflexive|" , property );
     }

     public String owlapiIsIrreflexive(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isIrreflexive|" , property, reasoner ).toString();
     }

     public RacerResult owlapiIsIrreflexive$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isIrreflexive|" , property, reasoner );
     }

/** Racer Function owlapi-isrealised
(|OWLAPI-isRealised| &optional owlapi:reasoner)
 */

     public String owlapiIsRealised( ) throws RacerClientException {
          return racerCall("|OWLAPI-isRealised|"  ).toString();
     }

     public RacerResult owlapiIsRealised$( ) throws RacerClientException {
          return racerCall("|OWLAPI-isRealised|"  );
     }

     public String owlapiIsRealised(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isRealised|" , reasoner ).toString();
     }

     public RacerResult owlapiIsRealised$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isRealised|" , reasoner );
     }

/** Racer Function owlapi-isreflexive
(|OWLAPI-isReflexive| owlapi:property &optional owlapi:reasoner)
 */

     public String owlapiIsReflexive(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isReflexive|" , property ).toString();
     }

     public RacerResult owlapiIsReflexive$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isReflexive|" , property );
     }

     public String owlapiIsReflexive(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isReflexive|" , property, reasoner ).toString();
     }

     public RacerResult owlapiIsReflexive$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isReflexive|" , property, reasoner );
     }

/** Racer Function owlapi-issameindividual
(|OWLAPI-isSameIndividual| owlapi::i
                           owlapi::j
                           &optional
                           owlapi:reasoner)
 */

     public String owlapiIsSameIndividual(Object i, Object j ) throws RacerClientException {
          return racerCall("|OWLAPI-isSameIndividual|" , i, j ).toString();
     }

     public RacerResult owlapiIsSameIndividual$(Object i, Object j ) throws RacerClientException {
          return racerCall("|OWLAPI-isSameIndividual|" , i, j );
     }

     public String owlapiIsSameIndividual(Object i, Object j, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isSameIndividual|" , i, j, reasoner ).toString();
     }

     public RacerResult owlapiIsSameIndividual$(Object i, Object j, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isSameIndividual|" , i, j, reasoner );
     }

/** Racer Function owlapi-issatisfiable
(|OWLAPI-isSatisfiable| owlapi:description &optional owlapi:reasoner)
 */

     public String owlapiIsSatisfiable(Object description ) throws RacerClientException {
          return racerCall("|OWLAPI-isSatisfiable|" , description ).toString();
     }

     public RacerResult owlapiIsSatisfiable$(Object description ) throws RacerClientException {
          return racerCall("|OWLAPI-isSatisfiable|" , description );
     }

     public String owlapiIsSatisfiable(Object description, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isSatisfiable|" , description, reasoner ).toString();
     }

     public RacerResult owlapiIsSatisfiable$(Object description, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isSatisfiable|" , description, reasoner );
     }

/** Racer Function owlapi-issubclassof
(|OWLAPI-isSubClassOf| owlapi:clsc
                       owlapi:clsd
                       &optional
                       owlapi:reasoner)
 */

     public String owlapiIsSubClassOf(Object clsc, Object clsd ) throws RacerClientException {
          return racerCall("|OWLAPI-isSubClassOf|" , clsc, clsd ).toString();
     }

     public RacerResult owlapiIsSubClassOf$(Object clsc, Object clsd ) throws RacerClientException {
          return racerCall("|OWLAPI-isSubClassOf|" , clsc, clsd );
     }

     public String owlapiIsSubClassOf(Object clsc, Object clsd, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isSubClassOf|" , clsc, clsd, reasoner ).toString();
     }

     public RacerResult owlapiIsSubClassOf$(Object clsc, Object clsd, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isSubClassOf|" , clsc, clsd, reasoner );
     }

/** Racer Function owlapi-issymmetric
(|OWLAPI-isSymmetric| owlapi:property &optional owlapi:reasoner)
 */

     public String owlapiIsSymmetric(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isSymmetric|" , property ).toString();
     }

     public RacerResult owlapiIsSymmetric$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isSymmetric|" , property );
     }

     public String owlapiIsSymmetric(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isSymmetric|" , property, reasoner ).toString();
     }

     public RacerResult owlapiIsSymmetric$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isSymmetric|" , property, reasoner );
     }

/** Racer Function owlapi-istransitive
(|OWLAPI-isTransitive| owlapi:property &optional owlapi:reasoner)
 */

     public String owlapiIsTransitive(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isTransitive|" , property ).toString();
     }

     public RacerResult owlapiIsTransitive$(Object property ) throws RacerClientException {
          return racerCall("|OWLAPI-isTransitive|" , property );
     }

     public String owlapiIsTransitive(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isTransitive|" , property, reasoner ).toString();
     }

     public RacerResult owlapiIsTransitive$(Object property, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-isTransitive|" , property, reasoner );
     }

/** Racer Function owlapi-keepannotations
(|OWLAPI-keepAnnotations| &optional owlapi:reasoner)
 */

     public String owlapiKeepAnnotations( ) throws RacerClientException {
          return racerCall("|OWLAPI-keepAnnotations|"  ).toString();
     }

     public RacerResult owlapiKeepAnnotations$( ) throws RacerClientException {
          return racerCall("|OWLAPI-keepAnnotations|"  );
     }

     public String owlapiKeepAnnotations(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-keepAnnotations|" , reasoner ).toString();
     }

     public RacerResult owlapiKeepAnnotations$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-keepAnnotations|" , reasoner );
     }

/** Racer Function owlapi-loadaxiom
(|OWLAPI-loadAxiom| owlapi::ont owlapi:axiom &optional owlapi:reasoner)
 */

     public String owlapiLoadAxiom(Object ont, Object axiom ) throws RacerClientException {
          return racerCall("|OWLAPI-loadAxiom|" , ont, axiom ).toString();
     }

     public RacerResult owlapiLoadAxiom$(Object ont, Object axiom ) throws RacerClientException {
          return racerCall("|OWLAPI-loadAxiom|" , ont, axiom );
     }

     public String owlapiLoadAxiom(Object ont, Object axiom, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-loadAxiom|" , ont, axiom, reasoner ).toString();
     }

     public RacerResult owlapiLoadAxiom$(Object ont, Object axiom, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-loadAxiom|" , ont, axiom, reasoner );
     }

/** Racer Function owlapi-loadaxioms
(|OWLAPI-loadAxioms| owlapi::ont
                     owlapi:axioms
                     &optional
                     owlapi:reasoner)
 */

     public String owlapiLoadAxioms(Object ont, Object axioms ) throws RacerClientException {
          return racerCall("|OWLAPI-loadAxioms|" , ont, axioms ).toString();
     }

     public RacerResult owlapiLoadAxioms$(Object ont, Object axioms ) throws RacerClientException {
          return racerCall("|OWLAPI-loadAxioms|" , ont, axioms );
     }

     public String owlapiLoadAxioms(Object ont, Object axioms, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-loadAxioms|" , ont, axioms, reasoner ).toString();
     }

     public RacerResult owlapiLoadAxioms$(Object ont, Object axioms, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-loadAxioms|" , ont, axioms, reasoner );
     }

/** Racer Function owlapi-loadontologies
(|OWLAPI-loadOntologies| owlapi:ontologies &optional owlapi:reasoner)
 */

     public String owlapiLoadOntologies(Object ontologies ) throws RacerClientException {
          return racerCall("|OWLAPI-loadOntologies|" , ontologies ).toString();
     }

     public RacerResult owlapiLoadOntologies$(Object ontologies ) throws RacerClientException {
          return racerCall("|OWLAPI-loadOntologies|" , ontologies );
     }

     public String owlapiLoadOntologies(Object ontologies, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-loadOntologies|" , ontologies, reasoner ).toString();
     }

     public RacerResult owlapiLoadOntologies$(Object ontologies, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-loadOntologies|" , ontologies, reasoner );
     }

/** Racer Function owlapi-loadontology
(|OWLAPI-loadOntology| owlapi:ontology &optional owlapi:reasoner)
 */

     public String owlapiLoadOntology(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-loadOntology|" , ontology ).toString();
     }

     public RacerResult owlapiLoadOntology$(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-loadOntology|" , ontology );
     }

     public String owlapiLoadOntology(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-loadOntology|" , ontology, reasoner ).toString();
     }

     public RacerResult owlapiLoadOntology$(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-loadOntology|" , ontology, reasoner );
     }

/** Racer Function owlapi-manuallyapplychanges
(|OWLAPI-manuallyApplyChanges| &optional owlapi:reasoner)
 */

     public String owlapiManuallyApplyChanges( ) throws RacerClientException {
          return racerCall("|OWLAPI-manuallyApplyChanges|"  ).toString();
     }

     public RacerResult owlapiManuallyApplyChanges$( ) throws RacerClientException {
          return racerCall("|OWLAPI-manuallyApplyChanges|"  );
     }

     public String owlapiManuallyApplyChanges(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-manuallyApplyChanges|" , reasoner ).toString();
     }

     public RacerResult owlapiManuallyApplyChanges$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-manuallyApplyChanges|" , reasoner );
     }

/** Racer Function owlapi-mergeontologies
(|OWLAPI-mergeOntologies| owlapi::ont1
                          owlapi::ont2
                          &optional
                          owlapi:reasoner)
 */

     public String owlapiMergeOntologies(Object ont1, Object ont2 ) throws RacerClientException {
          return racerCall("|OWLAPI-mergeOntologies|" , ont1, ont2 ).toString();
     }

     public RacerResult owlapiMergeOntologies$(Object ont1, Object ont2 ) throws RacerClientException {
          return racerCall("|OWLAPI-mergeOntologies|" , ont1, ont2 );
     }

     public String owlapiMergeOntologies(Object ont1, Object ont2, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-mergeOntologies|" , ont1, ont2, reasoner ).toString();
     }

     public RacerResult owlapiMergeOntologies$(Object ont1, Object ont2, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-mergeOntologies|" , ont1, ont2, reasoner );
     }

/** Racer Function owlapi-newontology
(|OWLAPI-newOntology| owlapi:name
                      &optional
                      owlapi:reasoner
                      owlapi:secondary-p)
 */

     public String owlapiNewOntology(Object name ) throws RacerClientException {
          return racerCall("|OWLAPI-newOntology|" , name ).toString();
     }

     public RacerResult owlapiNewOntology$(Object name ) throws RacerClientException {
          return racerCall("|OWLAPI-newOntology|" , name );
     }

     public String owlapiNewOntology(Object name, Object reasoner, Object secondaryP ) throws RacerClientException {
          return racerCall("|OWLAPI-newOntology|" , name, reasoner, secondaryP ).toString();
     }

     public RacerResult owlapiNewOntology$(Object name, Object reasoner, Object secondaryP ) throws RacerClientException {
          return racerCall("|OWLAPI-newOntology|" , name, reasoner, secondaryP );
     }

     public String owlapiNewOntology(Object name, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-newOntology|" , name, reasoner ).toString();
     }

     public RacerResult owlapiNewOntology$(Object name, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-newOntology|" , name, reasoner );
     }

/** Racer Function owlapi-newreasoner
(|OWLAPI-newReasoner| &optional
                      owlapi:owlapi-reasoner-name
                      owlapi::make-racer-kb-current-p
                      owlapi::init
                      owlapi:owlapi-tbox
                      owlapi:owlapi-abox
                      owlapi::own-racer-p)
 */

     public String owlapiNewReasoner( ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner|"  ).toString();
     }

     public RacerResult owlapiNewReasoner$( ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner|"  );
     }

     public String owlapiNewReasoner(Object owlapiReasonerName, Object makeRacerKbCurrentP, Object init, Object owlapiTbox, Object owlapiAbox, Object ownRacerP ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner|" , owlapiReasonerName, makeRacerKbCurrentP, init, owlapiTbox, owlapiAbox, ownRacerP ).toString();
     }

     public RacerResult owlapiNewReasoner$(Object owlapiReasonerName, Object makeRacerKbCurrentP, Object init, Object owlapiTbox, Object owlapiAbox, Object ownRacerP ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner|" , owlapiReasonerName, makeRacerKbCurrentP, init, owlapiTbox, owlapiAbox, ownRacerP );
     }

     public String owlapiNewReasoner(Object owlapiReasonerName, Object makeRacerKbCurrentP, Object init, Object owlapiTbox, Object owlapiAbox ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner|" , owlapiReasonerName, makeRacerKbCurrentP, init, owlapiTbox, owlapiAbox ).toString();
     }

     public RacerResult owlapiNewReasoner$(Object owlapiReasonerName, Object makeRacerKbCurrentP, Object init, Object owlapiTbox, Object owlapiAbox ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner|" , owlapiReasonerName, makeRacerKbCurrentP, init, owlapiTbox, owlapiAbox );
     }

     public String owlapiNewReasoner(Object owlapiReasonerName, Object makeRacerKbCurrentP, Object init, Object owlapiTbox ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner|" , owlapiReasonerName, makeRacerKbCurrentP, init, owlapiTbox ).toString();
     }

     public RacerResult owlapiNewReasoner$(Object owlapiReasonerName, Object makeRacerKbCurrentP, Object init, Object owlapiTbox ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner|" , owlapiReasonerName, makeRacerKbCurrentP, init, owlapiTbox );
     }

     public String owlapiNewReasoner(Object owlapiReasonerName, Object makeRacerKbCurrentP, Object init ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner|" , owlapiReasonerName, makeRacerKbCurrentP, init ).toString();
     }

     public RacerResult owlapiNewReasoner$(Object owlapiReasonerName, Object makeRacerKbCurrentP, Object init ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner|" , owlapiReasonerName, makeRacerKbCurrentP, init );
     }

     public String owlapiNewReasoner(Object owlapiReasonerName, Object makeRacerKbCurrentP ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner|" , owlapiReasonerName, makeRacerKbCurrentP ).toString();
     }

     public RacerResult owlapiNewReasoner$(Object owlapiReasonerName, Object makeRacerKbCurrentP ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner|" , owlapiReasonerName, makeRacerKbCurrentP );
     }

     public String owlapiNewReasoner(Object owlapiReasonerName ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner|" , owlapiReasonerName ).toString();
     }

     public RacerResult owlapiNewReasoner$(Object owlapiReasonerName ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner|" , owlapiReasonerName );
     }

/** Racer Function owlapi-newreasoner1
(|OWLAPI-newReasoner1| &optional
                       owlapi:owlapi-reasoner-name
                       owlapi::make-racer-kb-current-p
                       owlapi::init
                       owlapi:owlapi-tbox
                       owlapi:owlapi-abox)
 */

     public String owlapiNewReasoner1( ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner1|"  ).toString();
     }

     public RacerResult owlapiNewReasoner1$( ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner1|"  );
     }

     public String owlapiNewReasoner1(Object owlapiReasonerName, Object makeRacerKbCurrentP, Object init, Object owlapiTbox, Object owlapiAbox ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner1|" , owlapiReasonerName, makeRacerKbCurrentP, init, owlapiTbox, owlapiAbox ).toString();
     }

     public RacerResult owlapiNewReasoner1$(Object owlapiReasonerName, Object makeRacerKbCurrentP, Object init, Object owlapiTbox, Object owlapiAbox ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner1|" , owlapiReasonerName, makeRacerKbCurrentP, init, owlapiTbox, owlapiAbox );
     }

     public String owlapiNewReasoner1(Object owlapiReasonerName, Object makeRacerKbCurrentP, Object init, Object owlapiTbox ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner1|" , owlapiReasonerName, makeRacerKbCurrentP, init, owlapiTbox ).toString();
     }

     public RacerResult owlapiNewReasoner1$(Object owlapiReasonerName, Object makeRacerKbCurrentP, Object init, Object owlapiTbox ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner1|" , owlapiReasonerName, makeRacerKbCurrentP, init, owlapiTbox );
     }

     public String owlapiNewReasoner1(Object owlapiReasonerName, Object makeRacerKbCurrentP, Object init ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner1|" , owlapiReasonerName, makeRacerKbCurrentP, init ).toString();
     }

     public RacerResult owlapiNewReasoner1$(Object owlapiReasonerName, Object makeRacerKbCurrentP, Object init ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner1|" , owlapiReasonerName, makeRacerKbCurrentP, init );
     }

     public String owlapiNewReasoner1(Object owlapiReasonerName, Object makeRacerKbCurrentP ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner1|" , owlapiReasonerName, makeRacerKbCurrentP ).toString();
     }

     public RacerResult owlapiNewReasoner1$(Object owlapiReasonerName, Object makeRacerKbCurrentP ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner1|" , owlapiReasonerName, makeRacerKbCurrentP );
     }

     public String owlapiNewReasoner1(Object owlapiReasonerName ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner1|" , owlapiReasonerName ).toString();
     }

     public RacerResult owlapiNewReasoner1$(Object owlapiReasonerName ) throws RacerClientException {
          return racerCall("|OWLAPI-newReasoner1|" , owlapiReasonerName );
     }

/** Racer Function owlapi-nextaxiomuseid
(|OWLAPI-nextAxiomUseID| owlapi::id &optional owlapi:reasoner)
 */

     public String owlapiNextAxiomUseID(Object id ) throws RacerClientException {
          return racerCall("|OWLAPI-nextAxiomUseID|" , id ).toString();
     }

     public RacerResult owlapiNextAxiomUseID$(Object id ) throws RacerClientException {
          return racerCall("|OWLAPI-nextAxiomUseID|" , id );
     }

     public String owlapiNextAxiomUseID(Object id, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-nextAxiomUseID|" , id, reasoner ).toString();
     }

     public RacerResult owlapiNextAxiomUseID$(Object id, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-nextAxiomUseID|" , id, reasoner );
     }

/** Racer Function owlapi-parsenative
(|OWLAPI-parseNative| string &optional owlapi:reasoner)
 */

     public String owlapiParseNative(Object string ) throws RacerClientException {
          return racerCall("|OWLAPI-parseNative|" , string ).toString();
     }

     public RacerResult owlapiParseNative$(Object string ) throws RacerClientException {
          return racerCall("|OWLAPI-parseNative|" , string );
     }

     public String owlapiParseNative(Object string, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-parseNative|" , string, reasoner ).toString();
     }

     public RacerResult owlapiParseNative$(Object string, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-parseNative|" , string, reasoner );
     }

/** Racer Function owlapi-readfunctionalontologydocument
(|OWLAPI-readFunctionalOntologyDocument| owl-syntaxes::url
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
                                         owl-syntaxes::parser)
 */

     public String owlapiReadFunctionalOntologyDocument(Object url ) throws RacerClientException {
          return racerCall("|OWLAPI-readFunctionalOntologyDocument|" , url ).toString();
     }

     public RacerResult owlapiReadFunctionalOntologyDocument$(Object url ) throws RacerClientException {
          return racerCall("|OWLAPI-readFunctionalOntologyDocument|" , url );
     }

     public String owlapiReadFunctionalOntologyDocument(Object url , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-readFunctionalOntologyDocument|" , url , keyArgs).toString();
     }

     public RacerResult owlapiReadFunctionalOntologyDocument$(Object url , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-readFunctionalOntologyDocument|" , url , keyArgs);
     }

/** Racer Function owlapi-readfunctionalontologyfile
(|OWLAPI-readFunctionalOntologyFile| owl-syntaxes::fn
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
                                     owl-syntaxes::parser)
 */

     public String owlapiReadFunctionalOntologyFile(Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-readFunctionalOntologyFile|" , fn ).toString();
     }

     public RacerResult owlapiReadFunctionalOntologyFile$(Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-readFunctionalOntologyFile|" , fn );
     }

     public String owlapiReadFunctionalOntologyFile(Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-readFunctionalOntologyFile|" , fn , keyArgs).toString();
     }

     public RacerResult owlapiReadFunctionalOntologyFile$(Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-readFunctionalOntologyFile|" , fn , keyArgs);
     }

/** Racer Function owlapi-readontology
(|OWLAPI-readOntology| owl-syntaxes::url
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
                       ignore-annotations)
 */

     public String owlapiReadOntology(Object url ) throws RacerClientException {
          return racerCall("|OWLAPI-readOntology|" , url ).toString();
     }

     public RacerResult owlapiReadOntology$(Object url ) throws RacerClientException {
          return racerCall("|OWLAPI-readOntology|" , url );
     }

     public String owlapiReadOntology(Object url , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-readOntology|" , url , keyArgs).toString();
     }

     public RacerResult owlapiReadOntology$(Object url , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-readOntology|" , url , keyArgs);
     }

/** Racer Function owlapi-readxmlontologydocument
(|OWLAPI-readXMLOntologyDocument| owl-syntaxes::url
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
                                  owl-syntaxes::parser)
 */

     public String owlapiReadXMLOntologyDocument(Object url ) throws RacerClientException {
          return racerCall("|OWLAPI-readXMLOntologyDocument|" , url ).toString();
     }

     public RacerResult owlapiReadXMLOntologyDocument$(Object url ) throws RacerClientException {
          return racerCall("|OWLAPI-readXMLOntologyDocument|" , url );
     }

     public String owlapiReadXMLOntologyDocument(Object url , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-readXMLOntologyDocument|" , url , keyArgs).toString();
     }

     public RacerResult owlapiReadXMLOntologyDocument$(Object url , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-readXMLOntologyDocument|" , url , keyArgs);
     }

/** Racer Function owlapi-readxmlontologyfile
(|OWLAPI-readXMLOntologyFile| owl-syntaxes::fn
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
                              owl-syntaxes::parser)
 */

     public String owlapiReadXMLOntologyFile(Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-readXMLOntologyFile|" , fn ).toString();
     }

     public RacerResult owlapiReadXMLOntologyFile$(Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-readXMLOntologyFile|" , fn );
     }

     public String owlapiReadXMLOntologyFile(Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-readXMLOntologyFile|" , fn , keyArgs).toString();
     }

     public RacerResult owlapiReadXMLOntologyFile$(Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-readXMLOntologyFile|" , fn , keyArgs);
     }

/** Racer Function owlapi-registerdeclaredentities
(|OWLAPI-registerDeclaredEntities| &optional owlapi:reasoner)
 */

     public String owlapiRegisterDeclaredEntities( ) throws RacerClientException {
          return racerCall("|OWLAPI-registerDeclaredEntities|"  ).toString();
     }

     public RacerResult owlapiRegisterDeclaredEntities$( ) throws RacerClientException {
          return racerCall("|OWLAPI-registerDeclaredEntities|"  );
     }

     public String owlapiRegisterDeclaredEntities(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-registerDeclaredEntities|" , reasoner ).toString();
     }

     public RacerResult owlapiRegisterDeclaredEntities$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-registerDeclaredEntities|" , reasoner );
     }

/** Racer Function owlapi-registerlastanswer
(|OWLAPI-registerLastAnswer| &optional owlapi:reasoner)
 */

     public String owlapiRegisterLastAnswer( ) throws RacerClientException {
          return racerCall("|OWLAPI-registerLastAnswer|"  ).toString();
     }

     public RacerResult owlapiRegisterLastAnswer$( ) throws RacerClientException {
          return racerCall("|OWLAPI-registerLastAnswer|"  );
     }

     public String owlapiRegisterLastAnswer(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-registerLastAnswer|" , reasoner ).toString();
     }

     public RacerResult owlapiRegisterLastAnswer$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-registerLastAnswer|" , reasoner );
     }

/** Racer Function owlapi-registerobject
(|OWLAPI-registerObject| owlapi:obj)
 */

     public String owlapiRegisterObject(Object obj ) throws RacerClientException {
          return racerCall("|OWLAPI-registerObject|" , obj ).toString();
     }

     public RacerResult owlapiRegisterObject$(Object obj ) throws RacerClientException {
          return racerCall("|OWLAPI-registerObject|" , obj );
     }

/** Racer Function owlapi-registerreferencedentities
(|OWLAPI-registerReferencedEntities| &optional owlapi:reasoner)
 */

     public String owlapiRegisterReferencedEntities( ) throws RacerClientException {
          return racerCall("|OWLAPI-registerReferencedEntities|"  ).toString();
     }

     public RacerResult owlapiRegisterReferencedEntities$( ) throws RacerClientException {
          return racerCall("|OWLAPI-registerReferencedEntities|"  );
     }

     public String owlapiRegisterReferencedEntities(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-registerReferencedEntities|" , reasoner ).toString();
     }

     public RacerResult owlapiRegisterReferencedEntities$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-registerReferencedEntities|" , reasoner );
     }

/** Racer Function owlapi-reloadloadedontologies
(|OWLAPI-reloadLoadedOntologies| &optional owlapi:reasoner)
 */

     public String owlapiReloadLoadedOntologies( ) throws RacerClientException {
          return racerCall("|OWLAPI-reloadLoadedOntologies|"  ).toString();
     }

     public RacerResult owlapiReloadLoadedOntologies$( ) throws RacerClientException {
          return racerCall("|OWLAPI-reloadLoadedOntologies|"  );
     }

     public String owlapiReloadLoadedOntologies(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-reloadLoadedOntologies|" , reasoner ).toString();
     }

     public RacerResult owlapiReloadLoadedOntologies$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-reloadLoadedOntologies|" , reasoner );
     }

/** Racer Function owlapi-removeaxiom
(|OWLAPI-removeAxiom|)
 */

     public String owlapiRemoveAxiom( ) throws RacerClientException {
          return racerCall("|OWLAPI-removeAxiom|"  ).toString();
     }

     public RacerResult owlapiRemoveAxiom$( ) throws RacerClientException {
          return racerCall("|OWLAPI-removeAxiom|"  );
     }

/** Racer Function owlapi-removeaxioms
(|OWLAPI-removeAxioms|)
 */

     public String owlapiRemoveAxioms( ) throws RacerClientException {
          return racerCall("|OWLAPI-removeAxioms|"  ).toString();
     }

     public RacerResult owlapiRemoveAxioms$( ) throws RacerClientException {
          return racerCall("|OWLAPI-removeAxioms|"  );
     }

/** Racer Function owlapi-removeprefix
(|OWLAPI-removePrefix| owlapi:prefix &optional owlapi:reasoner)
 */

     public String owlapiRemovePrefix(Object prefix ) throws RacerClientException {
          return racerCall("|OWLAPI-removePrefix|" , prefix ).toString();
     }

     public RacerResult owlapiRemovePrefix$(Object prefix ) throws RacerClientException {
          return racerCall("|OWLAPI-removePrefix|" , prefix );
     }

     public String owlapiRemovePrefix(Object prefix, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-removePrefix|" , prefix, reasoner ).toString();
     }

     public RacerResult owlapiRemovePrefix$(Object prefix, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-removePrefix|" , prefix, reasoner );
     }

/** Racer Function owlapi-resetaxiomcounter
(|OWLAPI-resetAxiomCounter| &optional owlapi:reasoner)
 */

     public String owlapiResetAxiomCounter( ) throws RacerClientException {
          return racerCall("|OWLAPI-resetAxiomCounter|"  ).toString();
     }

     public RacerResult owlapiResetAxiomCounter$( ) throws RacerClientException {
          return racerCall("|OWLAPI-resetAxiomCounter|"  );
     }

     public String owlapiResetAxiomCounter(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-resetAxiomCounter|" , reasoner ).toString();
     }

     public RacerResult owlapiResetAxiomCounter$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-resetAxiomCounter|" , reasoner );
     }

/** Racer Function owlapi-restoreimage
(|OWLAPI-restoreImage| owlapi::fn)
 */

     public String owlapiRestoreImage(Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-restoreImage|" , fn ).toString();
     }

     public RacerResult owlapiRestoreImage$(Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-restoreImage|" , fn );
     }

/** Racer Function owlapi-saveontology
(|OWLAPI-saveOntology| owlapi:ontology
                       owl-syntaxes::fn
                       &key
                       owlapi:reasoner
                       owl-syntaxes::syntax
                       owl-syntaxes::prefixes
                       owl-syntaxes::p4-mode
                       owl-syntaxes::comments)
 */

     public String owlapiSaveOntology(Object ontology, Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-saveOntology|" , ontology, fn ).toString();
     }

     public RacerResult owlapiSaveOntology$(Object ontology, Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-saveOntology|" , ontology, fn );
     }

     public String owlapiSaveOntology(Object ontology, Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-saveOntology|" , ontology, fn , keyArgs).toString();
     }

     public RacerResult owlapiSaveOntology$(Object ontology, Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-saveOntology|" , ontology, fn , keyArgs);
     }

/** Racer Function owlapi-setautodeclaredataproperties
(|OWLAPI-setAutoDeclareDataProperties| owlapi::val
                                       &optional
                                       owlapi:reasoner)
 */

     public String owlapiSetAutoDeclareDataProperties(Object val ) throws RacerClientException {
          return racerCall("|OWLAPI-setAutoDeclareDataProperties|" , val ).toString();
     }

     public RacerResult owlapiSetAutoDeclareDataProperties$(Object val ) throws RacerClientException {
          return racerCall("|OWLAPI-setAutoDeclareDataProperties|" , val );
     }

     public String owlapiSetAutoDeclareDataProperties(Object val, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-setAutoDeclareDataProperties|" , val, reasoner ).toString();
     }

     public RacerResult owlapiSetAutoDeclareDataProperties$(Object val, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-setAutoDeclareDataProperties|" , val, reasoner );
     }

/** Racer Function owlapi-setaxiomcounter
(|OWLAPI-setAxiomCounter| owlapi::n &optional owlapi:reasoner)
 */

     public String owlapiSetAxiomCounter(Object n ) throws RacerClientException {
          return racerCall("|OWLAPI-setAxiomCounter|" , n ).toString();
     }

     public RacerResult owlapiSetAxiomCounter$(Object n ) throws RacerClientException {
          return racerCall("|OWLAPI-setAxiomCounter|" , n );
     }

     public String owlapiSetAxiomCounter(Object n, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-setAxiomCounter|" , n, reasoner ).toString();
     }

     public RacerResult owlapiSetAxiomCounter$(Object n, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-setAxiomCounter|" , n, reasoner );
     }

/** Racer Function owlapi-setcurrentreasoner
(|OWLAPI-setCurrentReasoner| owlapi:name
                             &optional
                             owlapi::make-racer-kb-current-p)
 */

     public String owlapiSetCurrentReasoner(Object name ) throws RacerClientException {
          return racerCall("|OWLAPI-setCurrentReasoner|" , name ).toString();
     }

     public RacerResult owlapiSetCurrentReasoner$(Object name ) throws RacerClientException {
          return racerCall("|OWLAPI-setCurrentReasoner|" , name );
     }

     public String owlapiSetCurrentReasoner(Object name, Object makeRacerKbCurrentP ) throws RacerClientException {
          return racerCall("|OWLAPI-setCurrentReasoner|" , name, makeRacerKbCurrentP ).toString();
     }

     public RacerResult owlapiSetCurrentReasoner$(Object name, Object makeRacerKbCurrentP ) throws RacerClientException {
          return racerCall("|OWLAPI-setCurrentReasoner|" , name, makeRacerKbCurrentP );
     }

/** Racer Function owlapi-setontologyuri
(|OWLAPI-SetOntologyURI| owlapi::ont
                         owlapi:uri
                         &optional
                         owlapi:reasoner)
 */

     public String owlapiSetOntologyURI(Object ont, Object uri ) throws RacerClientException {
          return racerCall("|OWLAPI-SetOntologyURI|" , ont, uri ).toString();
     }

     public RacerResult owlapiSetOntologyURI$(Object ont, Object uri ) throws RacerClientException {
          return racerCall("|OWLAPI-SetOntologyURI|" , ont, uri );
     }

     public String owlapiSetOntologyURI(Object ont, Object uri, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-SetOntologyURI|" , ont, uri, reasoner ).toString();
     }

     public RacerResult owlapiSetOntologyURI$(Object ont, Object uri, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-SetOntologyURI|" , ont, uri, reasoner );
     }

/** Racer Function owlapi-setprogress
(|OWLAPI-setProgress| owlapi::n &optional owlapi:reasoner)
 */

     public String owlapiSetProgress(Object n ) throws RacerClientException {
          return racerCall("|OWLAPI-setProgress|" , n ).toString();
     }

     public RacerResult owlapiSetProgress$(Object n ) throws RacerClientException {
          return racerCall("|OWLAPI-setProgress|" , n );
     }

     public String owlapiSetProgress(Object n, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-setProgress|" , n, reasoner ).toString();
     }

     public RacerResult owlapiSetProgress$(Object n, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-setProgress|" , n, reasoner );
     }

/** Racer Function owlapi-setprogressrange
(|OWLAPI-setProgressRange| owlapi::steps
                           owlapi:from
                           owlapi::to
                           &optional
                           owlapi:reasoner)
 */

     public String owlapiSetProgressRange(Object steps, Object from, Object to ) throws RacerClientException {
          return racerCall("|OWLAPI-setProgressRange|" , steps, from, to ).toString();
     }

     public RacerResult owlapiSetProgressRange$(Object steps, Object from, Object to ) throws RacerClientException {
          return racerCall("|OWLAPI-setProgressRange|" , steps, from, to );
     }

     public String owlapiSetProgressRange(Object steps, Object from, Object to, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-setProgressRange|" , steps, from, to, reasoner ).toString();
     }

     public RacerResult owlapiSetProgressRange$(Object steps, Object from, Object to, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-setProgressRange|" , steps, from, to, reasoner );
     }

/** Racer Function owlapi-setreturnpolicy
(|OWLAPI-setReturnPolicy| type &optional owlapi:reasoner)
 */

     public String owlapiSetReturnPolicy(Object type ) throws RacerClientException {
          return racerCall("|OWLAPI-setReturnPolicy|" , type ).toString();
     }

     public RacerResult owlapiSetReturnPolicy$(Object type ) throws RacerClientException {
          return racerCall("|OWLAPI-setReturnPolicy|" , type );
     }

     public String owlapiSetReturnPolicy(Object type, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-setReturnPolicy|" , type, reasoner ).toString();
     }

     public RacerResult owlapiSetReturnPolicy$(Object type, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-setReturnPolicy|" , type, reasoner );
     }

/** Racer Function owlapi-storeimage
(|OWLAPI-storeImage| owlapi::fn &optional owlapi::reasoners)
 */

     public String owlapiStoreImage(Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-storeImage|" , fn ).toString();
     }

     public RacerResult owlapiStoreImage$(Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-storeImage|" , fn );
     }

     public String owlapiStoreImage(Object fn, Object reasoners ) throws RacerClientException {
          return racerCall("|OWLAPI-storeImage|" , fn, reasoners ).toString();
     }

     public RacerResult owlapiStoreImage$(Object fn, Object reasoners ) throws RacerClientException {
          return racerCall("|OWLAPI-storeImage|" , fn, reasoners );
     }

/** Racer Function owlapi-unloadaxiom
(|OWLAPI-unloadAxiom| owlapi::ont
                      owlapi:axiom
                      &optional
                      owlapi:reasoner)
 */

     public String owlapiUnloadAxiom(Object ont, Object axiom ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadAxiom|" , ont, axiom ).toString();
     }

     public RacerResult owlapiUnloadAxiom$(Object ont, Object axiom ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadAxiom|" , ont, axiom );
     }

     public String owlapiUnloadAxiom(Object ont, Object axiom, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadAxiom|" , ont, axiom, reasoner ).toString();
     }

     public RacerResult owlapiUnloadAxiom$(Object ont, Object axiom, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadAxiom|" , ont, axiom, reasoner );
     }

/** Racer Function owlapi-unloadaxioms
(|OWLAPI-unloadAxioms| owlapi::ont
                       owlapi:axioms
                       &optional
                       owlapi:reasoner)
 */

     public String owlapiUnloadAxioms(Object ont, Object axioms ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadAxioms|" , ont, axioms ).toString();
     }

     public RacerResult owlapiUnloadAxioms$(Object ont, Object axioms ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadAxioms|" , ont, axioms );
     }

     public String owlapiUnloadAxioms(Object ont, Object axioms, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadAxioms|" , ont, axioms, reasoner ).toString();
     }

     public RacerResult owlapiUnloadAxioms$(Object ont, Object axioms, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadAxioms|" , ont, axioms, reasoner );
     }

/** Racer Function owlapi-unloadontologies
(|OWLAPI-unloadOntologies| owlapi:ontologies &optional owlapi:reasoner)
 */

     public String owlapiUnloadOntologies(Object ontologies ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadOntologies|" , ontologies ).toString();
     }

     public RacerResult owlapiUnloadOntologies$(Object ontologies ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadOntologies|" , ontologies );
     }

     public String owlapiUnloadOntologies(Object ontologies, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadOntologies|" , ontologies, reasoner ).toString();
     }

     public RacerResult owlapiUnloadOntologies$(Object ontologies, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadOntologies|" , ontologies, reasoner );
     }

/** Racer Function owlapi-unloadontology
(|OWLAPI-unloadOntology| owlapi:ontology &optional owlapi:reasoner)
 */

     public String owlapiUnloadOntology(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadOntology|" , ontology ).toString();
     }

     public RacerResult owlapiUnloadOntology$(Object ontology ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadOntology|" , ontology );
     }

     public String owlapiUnloadOntology(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadOntology|" , ontology, reasoner ).toString();
     }

     public RacerResult owlapiUnloadOntology$(Object ontology, Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-unloadOntology|" , ontology, reasoner );
     }

/** Racer Function owlapi-usesincrementalupdates
(|OWLAPI-usesIncrementalUpdates| &optional owlapi:reasoner)
 */

     public String owlapiUsesIncrementalUpdates( ) throws RacerClientException {
          return racerCall("|OWLAPI-usesIncrementalUpdates|"  ).toString();
     }

     public RacerResult owlapiUsesIncrementalUpdates$( ) throws RacerClientException {
          return racerCall("|OWLAPI-usesIncrementalUpdates|"  );
     }

     public String owlapiUsesIncrementalUpdates(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-usesIncrementalUpdates|" , reasoner ).toString();
     }

     public RacerResult owlapiUsesIncrementalUpdates$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-usesIncrementalUpdates|" , reasoner );
     }

/** Racer Function owlapi-usessimplifiedprotocol
(|OWLAPI-usesSimplifiedProtocol| &optional owlapi:reasoner)
 */

     public String owlapiUsesSimplifiedProtocol( ) throws RacerClientException {
          return racerCall("|OWLAPI-usesSimplifiedProtocol|"  ).toString();
     }

     public RacerResult owlapiUsesSimplifiedProtocol$( ) throws RacerClientException {
          return racerCall("|OWLAPI-usesSimplifiedProtocol|"  );
     }

     public String owlapiUsesSimplifiedProtocol(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-usesSimplifiedProtocol|" , reasoner ).toString();
     }

     public RacerResult owlapiUsesSimplifiedProtocol$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-usesSimplifiedProtocol|" , reasoner );
     }

/** Racer Function owlapi-writefunctionalontologyfile
(|OWLAPI-writeFunctionalOntologyFile| owlapi:ontology
                                      owl-syntaxes::fn
                                      &key
                                      owl-syntaxes::prefixes
                                      owl-syntaxes::p4-mode
                                      owl-syntaxes::comments)
 */

     public String owlapiWriteFunctionalOntologyFile(Object ontology, Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-writeFunctionalOntologyFile|" , ontology, fn ).toString();
     }

     public RacerResult owlapiWriteFunctionalOntologyFile$(Object ontology, Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-writeFunctionalOntologyFile|" , ontology, fn );
     }

     public String owlapiWriteFunctionalOntologyFile(Object ontology, Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-writeFunctionalOntologyFile|" , ontology, fn , keyArgs).toString();
     }

     public RacerResult owlapiWriteFunctionalOntologyFile$(Object ontology, Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-writeFunctionalOntologyFile|" , ontology, fn , keyArgs);
     }

/** Racer Function owlapi-writeontologyfile
(|OWLAPI-writeOntologyFile| owlapi:ontology
                            owl-syntaxes::fn
                            &key
                            owl-syntaxes::prefixes
                            owl-syntaxes::p4-mode
                            owl-syntaxes::comments)
 */

     public String owlapiWriteOntologyFile(Object ontology, Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-writeOntologyFile|" , ontology, fn ).toString();
     }

     public RacerResult owlapiWriteOntologyFile$(Object ontology, Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-writeOntologyFile|" , ontology, fn );
     }

     public String owlapiWriteOntologyFile(Object ontology, Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-writeOntologyFile|" , ontology, fn , keyArgs).toString();
     }

     public RacerResult owlapiWriteOntologyFile$(Object ontology, Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-writeOntologyFile|" , ontology, fn , keyArgs);
     }

/** Racer Function owlapi-writexmlontologyfile
(|OWLAPI-writeXMLOntologyFile| owlapi:ontology
                               owl-syntaxes::fn
                               &key
                               owl-syntaxes::prefixes
                               owl-syntaxes::p4-mode
                               owl-syntaxes::comments)
 */

     public String owlapiWriteXMLOntologyFile(Object ontology, Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-writeXMLOntologyFile|" , ontology, fn ).toString();
     }

     public RacerResult owlapiWriteXMLOntologyFile$(Object ontology, Object fn ) throws RacerClientException {
          return racerCall("|OWLAPI-writeXMLOntologyFile|" , ontology, fn );
     }

     public String owlapiWriteXMLOntologyFile(Object ontology, Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-writeXMLOntologyFile|" , ontology, fn , keyArgs).toString();
     }

     public RacerResult owlapiWriteXMLOntologyFile$(Object ontology, Object fn , Object... keyArgs) throws RacerClientException {
          return racerCall("|OWLAPI-writeXMLOntologyFile|" , ontology, fn , keyArgs);
     }

/** Racer Function owllink-read-document
(owllink-read-document url &rest (args))
 */

     public String owllinkReadDocument(Object url ) throws RacerClientException {
          return racerCall("owllink-read-document" , url ).toString();
     }

     public RacerResult owllinkReadDocument$(Object url ) throws RacerClientException {
          return racerCall("owllink-read-document" , url );
     }

     public String owllinkReadDocument(Object url , Object... keyArgs) throws RacerClientException {
          return racerCall("owllink-read-document" , url , keyArgs).toString();
     }

     public RacerResult owllinkReadDocument$(Object url , Object... keyArgs) throws RacerClientException {
          return racerCall("owllink-read-document" , url , keyArgs);
     }

/** Racer Function owllink-read-file
(owllink-read-file filename &rest (args))
 */

     public String owllinkReadFile(Object filename ) throws RacerClientException {
          return racerCall("owllink-read-file" , filename ).toString();
     }

     public RacerResult owllinkReadFile$(Object filename ) throws RacerClientException {
          return racerCall("owllink-read-file" , filename );
     }

     public String owllinkReadFile(Object filename , Object... keyArgs) throws RacerClientException {
          return racerCall("owllink-read-file" , filename , keyArgs).toString();
     }

     public RacerResult owllinkReadFile$(Object filename , Object... keyArgs) throws RacerClientException {
          return racerCall("owllink-read-file" , filename , keyArgs);
     }

/** Racer Function pracer-answer-query
(pracer-answer-query res-args
                     query
                     &key
                     id
                     print-query
                     use-optimizer)
 */

     public String pracerAnswerQuery(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("pracer-answer-query" , resArgs, query ).toString();
     }

     public RacerResult pracerAnswerQuery$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("pracer-answer-query" , resArgs, query );
     }

     public String pracerAnswerQuery(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("pracer-answer-query" , resArgs, query , keyArgs).toString();
     }

     public RacerResult pracerAnswerQuery$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("pracer-answer-query" , resArgs, query , keyArgs);
     }

/** Racer Function prefer-defined-queries
(prefer-defined-queries)
 */

     public String preferDefinedQueries( ) throws RacerClientException {
          return racerCall("prefer-defined-queries"  ).toString();
     }

     public RacerResult preferDefinedQueries$( ) throws RacerClientException {
          return racerCall("prefer-defined-queries"  );
     }

/** Racer Function prepare-abox
(prepare-abox &optional abox)
 */

     public String prepareAbox( ) throws RacerClientException {
          return racerCall("prepare-abox"  ).toString();
     }

     public RacerResult prepareAbox$( ) throws RacerClientException {
          return racerCall("prepare-abox"  );
     }

     public String prepareAbox(Object abox ) throws RacerClientException {
          return racerCall("prepare-abox" , abox ).toString();
     }

     public RacerResult prepareAbox$(Object abox ) throws RacerClientException {
          return racerCall("prepare-abox" , abox );
     }

/** Racer Function prepare-nrql-engine
(prepare-nrql-engine &key
                     mode
                     dont-show-variables
                     dont-show-lambdas
                     dont-show-head-projection-operators
                     abox-mirroring
                     query-optimization
                     optimizer-use-cardinality-heuristics
                     how-many-tuples
                     timeout
                     warnings
                     add-rule-consequences-automatically
                     dont-add-abox-duplicates
                     two-phase-query-processing-mode
                     phase-two-starts-warning-tokens
                     kb-has-changed-warning-tokens
                     told-information-querying
                     tuple-computation-mode
                     exclude-permutations
                     query-repository
                     report-inconsistent-queries
                     report-tautological-queries
                     query-realization
                     bindings
                     check-abox-consistency
                     use-individual-equivalence-classes
                     rewrite-to-dnf
                     tbox
                     substrate
                     abox
                     create-abox-if-not-found-p
                     package
                     type-of-substrate
                     prepare-now-p)
 */

     public String prepareNrqlEngine( ) throws RacerClientException {
          return racerCall("prepare-nrql-engine"  ).toString();
     }

     public RacerResult prepareNrqlEngine$( ) throws RacerClientException {
          return racerCall("prepare-nrql-engine"  );
     }

     public String prepareNrqlEngine(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-nrql-engine"  , keyArgs).toString();
     }

     public RacerResult prepareNrqlEngine$(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-nrql-engine"  , keyArgs);
     }

/** Racer Function prepare-query
(prepare-query &key
               execute-p
               dont-add-abox-duplicates-p
               remove-duplicates-p
               two-phase-processing-p
               deliver-phase-two-warning-tokens-p
               deliver-kb-has-changed-warning-tokens-p
               add-rule-consequences-p
               continuation-based-instance-retrieval-p
               told-information-reasoning-p
               final-consistency-checking-p
               runtime-consistency-checking-p
               verbose-p
               dont-show-variables
               dont-show-head-projection-operators-p
               dont-show-lambdas-p
               how-many
               only-new-tuples-p
               timeout
               proactive-tuple-computation-p
               tuple-at-a-time-p
               use-individual-synonyms-p
               check-abox-consistency-p
               ensure-tbox-classification-p
               initial-abox-mirroring-p
               initial-role-assertion-mirroring-p
               classify-concepts-in-instance-assertions-p
               exclude-permutations-p
               record-explanations-p
               parser-class
               rewrite-defined-concepts-p
               group-by-ops
               bind-specials-p
               original-query
               rule-con-pattern
               new-ind-ops
               premise
               generate-code-p
               optimize-p
               rewrite-semantically-p
               rewrite-to-dnf-p
               report-inconsistent-queries-p
               report-tautological-queries-p
               use-repository-p
               put-into-repository-p
               id
               dont-check-id-p
               parser
               result-vois
               substrate
               abox
               create-abox-if-not-found-p
               package
               type-of-substrate
               prepare-now-p)
 */

     public String prepareQuery( ) throws RacerClientException {
          return racerCall("prepare-query"  ).toString();
     }

     public RacerResult prepareQuery$( ) throws RacerClientException {
          return racerCall("prepare-query"  );
     }

     public String prepareQuery(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-query"  , keyArgs).toString();
     }

     public RacerResult prepareQuery$(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-query"  , keyArgs);
     }

/** Racer Function prepare-query1
(prepare-query1 &key
                execute-p
                dont-add-abox-duplicates-p
                remove-duplicates-p
                two-phase-processing-p
                deliver-phase-two-warning-tokens-p
                deliver-kb-has-changed-warning-tokens-p
                add-rule-consequences-p
                continuation-based-instance-retrieval-p
                told-information-reasoning-p
                final-consistency-checking-p
                runtime-consistency-checking-p
                verbose-p
                dont-show-variables
                dont-show-head-projection-operators-p
                dont-show-lambdas-p
                how-many
                only-new-tuples-p
                timeout
                proactive-tuple-computation-p
                tuple-at-a-time-p
                use-individual-synonyms-p
                check-abox-consistency-p
                ensure-tbox-classification-p
                initial-abox-mirroring-p
                initial-role-assertion-mirroring-p
                classify-concepts-in-instance-assertions-p
                exclude-permutations-p
                record-explanations-p
                parser-class
                rewrite-defined-concepts-p
                group-by-ops
                bind-specials-p
                original-query
                rule-con-pattern
                new-ind-ops
                premise
                generate-code-p
                optimize-p
                rewrite-semantically-p
                rewrite-to-dnf-p
                report-inconsistent-queries-p
                report-tautological-queries-p
                use-repository-p
                put-into-repository-p
                id
                dont-check-id-p
                parser
                result-vois
                substrate
                abox
                create-abox-if-not-found-p
                package
                type-of-substrate
                prepare-now-p)
 */

     public String prepareQuery1( ) throws RacerClientException {
          return racerCall("prepare-query1"  ).toString();
     }

     public RacerResult prepareQuery1$( ) throws RacerClientException {
          return racerCall("prepare-query1"  );
     }

     public String prepareQuery1(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-query1"  , keyArgs).toString();
     }

     public RacerResult prepareQuery1$(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-query1"  , keyArgs);
     }

/** Racer Function prepare-racer-engine
(prepare-racer-engine &key abox classify-tbox-p)
 */

     public String prepareRacerEngine( ) throws RacerClientException {
          return racerCall("prepare-racer-engine"  ).toString();
     }

     public RacerResult prepareRacerEngine$( ) throws RacerClientException {
          return racerCall("prepare-racer-engine"  );
     }

     public String prepareRacerEngine(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-racer-engine"  , keyArgs).toString();
     }

     public RacerResult prepareRacerEngine$(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-racer-engine"  , keyArgs);
     }

/** Racer Function prepare-rule
(prepare-rule &key
              execute-p
              parser-class
              rewrite-defined-concepts-p
              group-by-ops
              bind-specials-p
              original-query
              rule-con-pattern
              new-ind-ops
              premise
              generate-code-p
              optimize-p
              rewrite-semantically-p
              rewrite-to-dnf-p
              report-inconsistent-queries-p
              report-tautological-queries-p
              use-repository-p
              put-into-repository-p
              id
              dont-check-id-p
              parser
              result-vois
              substrate
              abox
              create-abox-if-not-found-p
              package
              type-of-substrate
              prepare-now-p)
 */

     public String prepareRule( ) throws RacerClientException {
          return racerCall("prepare-rule"  ).toString();
     }

     public RacerResult prepareRule$( ) throws RacerClientException {
          return racerCall("prepare-rule"  );
     }

     public String prepareRule(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-rule"  , keyArgs).toString();
     }

     public RacerResult prepareRule$(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-rule"  , keyArgs);
     }

/** Racer Function prepare-rule1
(prepare-rule1 &key
               execute-p
               parser-class
               rewrite-defined-concepts-p
               group-by-ops
               bind-specials-p
               original-query
               rule-con-pattern
               new-ind-ops
               premise
               generate-code-p
               optimize-p
               rewrite-semantically-p
               rewrite-to-dnf-p
               report-inconsistent-queries-p
               report-tautological-queries-p
               use-repository-p
               put-into-repository-p
               id
               dont-check-id-p
               parser
               result-vois
               substrate
               abox
               create-abox-if-not-found-p
               package
               type-of-substrate
               prepare-now-p)
 */

     public String prepareRule1( ) throws RacerClientException {
          return racerCall("prepare-rule1"  ).toString();
     }

     public RacerResult prepareRule1$( ) throws RacerClientException {
          return racerCall("prepare-rule1"  );
     }

     public String prepareRule1(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-rule1"  , keyArgs).toString();
     }

     public RacerResult prepareRule1$(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-rule1"  , keyArgs);
     }

/** Racer Function prepared-queries
(prepared-queries &key
                  abox
                  type-of-substrate)
 */

     public String preparedQueries( ) throws RacerClientException {
          return racerCall("prepared-queries"  ).toString();
     }

     public RacerResult preparedQueries$( ) throws RacerClientException {
          return racerCall("prepared-queries"  );
     }

     public String preparedQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepared-queries"  , keyArgs).toString();
     }

     public RacerResult preparedQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepared-queries"  , keyArgs);
     }

/** Racer Function prepared-rules
(prepared-rules &key
                abox
                type-of-substrate)
 */

     public String preparedRules( ) throws RacerClientException {
          return racerCall("prepared-rules"  ).toString();
     }

     public RacerResult preparedRules$( ) throws RacerClientException {
          return racerCall("prepared-rules"  );
     }

     public String preparedRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepared-rules"  , keyArgs).toString();
     }

     public RacerResult preparedRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("prepared-rules"  , keyArgs);
     }

/** Racer Function print-abox-individuals
(print-abox-individuals &key
                        stream
                        abox
                        concept-mapping
                        ind-mapping
                        pretty)
 */

     public String printAboxIndividuals( ) throws RacerClientException {
          return racerCall("print-abox-individuals"  ).toString();
     }

     public RacerResult printAboxIndividuals$( ) throws RacerClientException {
          return racerCall("print-abox-individuals"  );
     }

     public String printAboxIndividuals(  Object... keyArgs) throws RacerClientException {
          return racerCall("print-abox-individuals"  , keyArgs).toString();
     }

     public RacerResult printAboxIndividuals$(  Object... keyArgs) throws RacerClientException {
          return racerCall("print-abox-individuals"  , keyArgs);
     }

/** Racer Function print-tbox-tree
(print-tbox-tree &optional tbox stream hide-role-inverses)
 */

     public String printTboxTree( ) throws RacerClientException {
          return racerCall("print-tbox-tree"  ).toString();
     }

     public RacerResult printTboxTree$( ) throws RacerClientException {
          return racerCall("print-tbox-tree"  );
     }

     public String printTboxTree(Object tbox, Object stream, Object hideRoleInverses ) throws RacerClientException {
          return racerCall("print-tbox-tree" , tbox, stream, hideRoleInverses ).toString();
     }

     public RacerResult printTboxTree$(Object tbox, Object stream, Object hideRoleInverses ) throws RacerClientException {
          return racerCall("print-tbox-tree" , tbox, stream, hideRoleInverses );
     }

     public String printTboxTree(Object tbox, Object stream ) throws RacerClientException {
          return racerCall("print-tbox-tree" , tbox, stream ).toString();
     }

     public RacerResult printTboxTree$(Object tbox, Object stream ) throws RacerClientException {
          return racerCall("print-tbox-tree" , tbox, stream );
     }

     public String printTboxTree(Object tbox ) throws RacerClientException {
          return racerCall("print-tbox-tree" , tbox ).toString();
     }

     public RacerResult printTboxTree$(Object tbox ) throws RacerClientException {
          return racerCall("print-tbox-tree" , tbox );
     }

/** Racer Function process-set-at-a-time
(process-set-at-a-time)
 */

     public String processSetAtATime( ) throws RacerClientException {
          return racerCall("process-set-at-a-time"  ).toString();
     }

     public RacerResult processSetAtATime$( ) throws RacerClientException {
          return racerCall("process-set-at-a-time"  );
     }

/** Racer Function process-tuple-at-a-time
(process-tuple-at-a-time)
 */

     public String processTupleAtATime( ) throws RacerClientException {
          return racerCall("process-tuple-at-a-time"  ).toString();
     }

     public RacerResult processTupleAtATime$( ) throws RacerClientException {
          return racerCall("process-tuple-at-a-time"  );
     }

/** Racer Function processed-queries
(processed-queries &key
                   abox
                   type-of-substrate)
 */

     public String processedQueries( ) throws RacerClientException {
          return racerCall("processed-queries"  ).toString();
     }

     public RacerResult processedQueries$( ) throws RacerClientException {
          return racerCall("processed-queries"  );
     }

     public String processedQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("processed-queries"  , keyArgs).toString();
     }

     public RacerResult processedQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("processed-queries"  , keyArgs);
     }

/** Racer Function processed-rules
(processed-rules &key
                 abox
                 type-of-substrate)
 */

     public String processedRules( ) throws RacerClientException {
          return racerCall("processed-rules"  ).toString();
     }

     public RacerResult processedRules$( ) throws RacerClientException {
          return racerCall("processed-rules"  );
     }

     public String processedRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("processed-rules"  , keyArgs).toString();
     }

     public RacerResult processedRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("processed-rules"  , keyArgs);
     }

/** Racer Function publish-1
(publish-1 individual &optional abox)
 */

     public String publish1(Object individual ) throws RacerClientException {
          return racerCall("publish-1" , individual ).toString();
     }

     public RacerResult publish1$(Object individual ) throws RacerClientException {
          return racerCall("publish-1" , individual );
     }

     public String publish1(Object individual, Object abox ) throws RacerClientException {
          return racerCall("publish-1" , individual, abox ).toString();
     }

     public RacerResult publish1$(Object individual, Object abox ) throws RacerClientException {
          return racerCall("publish-1" , individual, abox );
     }

/** Racer Function publish-file
(publish-file http::filename url:url http::content-type)
 */

     public String publishFile(Object filename, Object url, Object contentType ) throws RacerClientException {
          return racerCall("publish-file" , filename, url, contentType ).toString();
     }

     public RacerResult publishFile$(Object filename, Object url, Object contentType ) throws RacerClientException {
          return racerCall("publish-file" , filename, url, contentType );
     }

/** Racer Function query-accurate-p
(query-accurate-p query)
 */

     public boolean queryAccurateP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("query-accurate-p" , query ));
     }

/** Racer Function query-active-p
(query-active-p query)
 */

     public boolean queryActiveP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("query-active-p" , query ));
     }

/** Racer Function query-ancestors
(query-ancestors query)
 */

     public String queryAncestors(Object query ) throws RacerClientException {
          return racerCall("query-ancestors" , query ).toString();
     }

     public RacerResult queryAncestors$(Object query ) throws RacerClientException {
          return racerCall("query-ancestors" , query );
     }

/** Racer Function query-body
(query-body query)
 */

     public String queryBody(Object query ) throws RacerClientException {
          return racerCall("query-body" , query ).toString();
     }

     public RacerResult queryBody$(Object query ) throws RacerClientException {
          return racerCall("query-body" , query );
     }

/** Racer Function query-children
(query-children query)
 */

     public String queryChildren(Object query ) throws RacerClientException {
          return racerCall("query-children" , query ).toString();
     }

     public RacerResult queryChildren$(Object query ) throws RacerClientException {
          return racerCall("query-children" , query );
     }

/** Racer Function query-consistent-p
(query-consistent-p query)
 */

     public boolean queryConsistentP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("query-consistent-p" , query ));
     }

/** Racer Function query-descendants
(query-descendants query)
 */

     public String queryDescendants(Object query ) throws RacerClientException {
          return racerCall("query-descendants" , query ).toString();
     }

     public RacerResult queryDescendants$(Object query ) throws RacerClientException {
          return racerCall("query-descendants" , query );
     }

/** Racer Function query-entails-p
(query-entails-p a b)
 */

     public boolean queryEntailsP(Object a, Object b ) throws RacerClientException {
          return returnBoolean(racerCall("query-entails-p" , a, b ));
     }

/** Racer Function query-equivalent-p
(query-equivalent-p a b)
 */

     public boolean queryEquivalentP(Object a, Object b ) throws RacerClientException {
          return returnBoolean(racerCall("query-equivalent-p" , a, b ));
     }

/** Racer Function query-equivalents
(query-equivalents query)
 */

     public String queryEquivalents(Object query ) throws RacerClientException {
          return racerCall("query-equivalents" , query ).toString();
     }

     public RacerResult queryEquivalents$(Object query ) throws RacerClientException {
          return racerCall("query-equivalents" , query );
     }

/** Racer Function query-head
(query-head query)
 */

     public String queryHead(Object query ) throws RacerClientException {
          return racerCall("query-head" , query ).toString();
     }

     public RacerResult queryHead$(Object query ) throws RacerClientException {
          return racerCall("query-head" , query );
     }

/** Racer Function query-parents
(query-parents query)
 */

     public String queryParents(Object query ) throws RacerClientException {
          return racerCall("query-parents" , query ).toString();
     }

     public RacerResult queryParents$(Object query ) throws RacerClientException {
          return racerCall("query-parents" , query );
     }

/** Racer Function query-prepared-p
(query-prepared-p query)
 */

     public boolean queryPreparedP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("query-prepared-p" , query ));
     }

/** Racer Function query-processed-p
(query-processed-p query)
 */

     public boolean queryProcessedP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("query-processed-p" , query ));
     }

/** Racer Function query-ready-p
(query-ready-p query)
 */

     public boolean queryReadyP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("query-ready-p" , query ));
     }

/** Racer Function query-running-p
(query-running-p query)
 */

     public boolean queryRunningP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("query-running-p" , query ));
     }

/** Racer Function query-sleeping-p
(query-sleeping-p query)
 */

     public boolean querySleepingP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("query-sleeping-p" , query ));
     }

/** Racer Function query-subscribers
(query-subscribers query)
 */

     public String querySubscribers(Object query ) throws RacerClientException {
          return racerCall("query-subscribers" , query ).toString();
     }

     public RacerResult querySubscribers$(Object query ) throws RacerClientException {
          return racerCall("query-subscribers" , query );
     }

/** Racer Function query-terminated-p
(query-terminated-p query)
 */

     public boolean queryTerminatedP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("query-terminated-p" , query ));
     }

/** Racer Function query-waiting-p
(query-waiting-p query)
 */

     public boolean queryWaitingP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("query-waiting-p" , query ));
     }

/** Racer Function racer-answer-query
(racer-answer-query res-args
                    query
                    &key
                    execute-p
                    dont-add-abox-duplicates-p
                    remove-duplicates-p
                    two-phase-processing-p
                    deliver-phase-two-warning-tokens-p
                    deliver-kb-has-changed-warning-tokens-p
                    add-rule-consequences-p
                    continuation-based-instance-retrieval-p
                    told-information-reasoning-p
                    final-consistency-checking-p
                    runtime-consistency-checking-p
                    verbose-p
                    dont-show-variables
                    dont-show-head-projection-operators-p
                    dont-show-lambdas-p
                    how-many
                    only-new-tuples-p
                    timeout
                    proactive-tuple-computation-p
                    tuple-at-a-time-p
                    use-individual-synonyms-p
                    check-abox-consistency-p
                    ensure-tbox-classification-p
                    initial-abox-mirroring-p
                    initial-role-assertion-mirroring-p
                    classify-concepts-in-instance-assertions-p
                    exclude-permutations-p
                    record-explanations-p
                    parser-class
                    rewrite-defined-concepts-p
                    group-by-ops
                    bind-specials-p
                    original-query
                    rule-con-pattern
                    new-ind-ops
                    premise
                    generate-code-p
                    optimize-p
                    rewrite-semantically-p
                    rewrite-to-dnf-p
                    report-inconsistent-queries-p
                    report-tautological-queries-p
                    use-repository-p
                    put-into-repository-p
                    id
                    dont-check-id-p
                    parser
                    result-vois
                    substrate
                    abox
                    create-abox-if-not-found-p
                    package
                    type-of-substrate
                    prepare-now-p)
 */

     public String racerAnswerQuery(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-answer-query" , resArgs, query ).toString();
     }

     public RacerResult racerAnswerQuery$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-answer-query" , resArgs, query );
     }

     public String racerAnswerQuery(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-answer-query" , resArgs, query , keyArgs).toString();
     }

     public RacerResult racerAnswerQuery$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-answer-query" , resArgs, query , keyArgs);
     }

/** Racer Function racer-answer-query-under-premise
(racer-answer-query-under-premise res-args
                                  query
                                  &key
                                  execute-p
                                  dont-add-abox-duplicates-p
                                  remove-duplicates-p
                                  two-phase-processing-p
                                  deliver-phase-two-warning-tokens-p
                                  deliver-kb-has-changed-warning-tokens-p
                                  add-rule-consequences-p
                                  continuation-based-instance-retrieval-p
                                  told-information-reasoning-p
                                  final-consistency-checking-p
                                  runtime-consistency-checking-p
                                  verbose-p
                                  dont-show-variables
                                  dont-show-head-projection-operators-p
                                  dont-show-lambdas-p
                                  how-many
                                  only-new-tuples-p
                                  timeout
                                  proactive-tuple-computation-p
                                  tuple-at-a-time-p
                                  use-individual-synonyms-p
                                  check-abox-consistency-p
                                  ensure-tbox-classification-p
                                  initial-abox-mirroring-p
                                  initial-role-assertion-mirroring-p
                                  classify-concepts-in-instance-assertions-p
                                  exclude-permutations-p
                                  record-explanations-p
                                  parser-class
                                  rewrite-defined-concepts-p
                                  group-by-ops
                                  bind-specials-p
                                  original-query
                                  rule-con-pattern
                                  new-ind-ops
                                  premise
                                  generate-code-p
                                  optimize-p
                                  rewrite-semantically-p
                                  rewrite-to-dnf-p
                                  report-inconsistent-queries-p
                                  report-tautological-queries-p
                                  use-repository-p
                                  put-into-repository-p
                                  id
                                  dont-check-id-p
                                  parser
                                  result-vois
                                  substrate
                                  abox
                                  create-abox-if-not-found-p
                                  package
                                  type-of-substrate
                                  prepare-now-p)
 */

     public String racerAnswerQueryUnderPremise(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-answer-query-under-premise" , resArgs, query ).toString();
     }

     public RacerResult racerAnswerQueryUnderPremise$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-answer-query-under-premise" , resArgs, query );
     }

     public String racerAnswerQueryUnderPremise(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-answer-query-under-premise" , resArgs, query , keyArgs).toString();
     }

     public RacerResult racerAnswerQueryUnderPremise$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-answer-query-under-premise" , resArgs, query , keyArgs);
     }

/** Racer Function racer-answer-query-under-premise1
(racer-answer-query-under-premise1 query
                                   res-args
                                   &key
                                   execute-p
                                   dont-add-abox-duplicates-p
                                   remove-duplicates-p
                                   two-phase-processing-p
                                   deliver-phase-two-warning-tokens-p
                                   deliver-kb-has-changed-warning-tokens-p
                                   add-rule-consequences-p
                                   continuation-based-instance-retrieval-p
                                   told-information-reasoning-p
                                   final-consistency-checking-p
                                   runtime-consistency-checking-p
                                   verbose-p
                                   dont-show-variables
                                   dont-show-head-projection-operators-p
                                   dont-show-lambdas-p
                                   how-many
                                   only-new-tuples-p
                                   timeout
                                   proactive-tuple-computation-p
                                   tuple-at-a-time-p
                                   use-individual-synonyms-p
                                   check-abox-consistency-p
                                   ensure-tbox-classification-p
                                   initial-abox-mirroring-p
                                   initial-role-assertion-mirroring-p
                                   classify-concepts-in-instance-assertions-p
                                   exclude-permutations-p
                                   record-explanations-p
                                   parser-class
                                   rewrite-defined-concepts-p
                                   group-by-ops
                                   bind-specials-p
                                   original-query
                                   rule-con-pattern
                                   new-ind-ops
                                   premise
                                   generate-code-p
                                   optimize-p
                                   rewrite-semantically-p
                                   rewrite-to-dnf-p
                                   report-inconsistent-queries-p
                                   report-tautological-queries-p
                                   use-repository-p
                                   put-into-repository-p
                                   id
                                   dont-check-id-p
                                   parser
                                   result-vois
                                   substrate
                                   abox
                                   create-abox-if-not-found-p
                                   package
                                   type-of-substrate
                                   prepare-now-p)
 */

     public String racerAnswerQueryUnderPremise1(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-answer-query-under-premise1" , query, resArgs ).toString();
     }

     public RacerResult racerAnswerQueryUnderPremise1$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-answer-query-under-premise1" , query, resArgs );
     }

     public String racerAnswerQueryUnderPremise1(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-answer-query-under-premise1" , query, resArgs , keyArgs).toString();
     }

     public RacerResult racerAnswerQueryUnderPremise1$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-answer-query-under-premise1" , query, resArgs , keyArgs);
     }

/** Racer Function racer-answer-query-with-explanation
(racer-answer-query-with-explanation res-args
                                     query
                                     &key
                                     cutoff-fn
                                     hypo-mode-stack
                                     c-mode
                                     r-mode
                                     only-best-p
                                     order-by
                                     reverse-order-p
                                     ensure-permutations-p
                                     how-many
                                     strategy
                                     simple-result-p
                                     runtime-consistency-checking-p
                                     final-consistency-checking-p
                                     same-as-only-p
                                     candidate-individuals
                                     binding-validator
                                     &rest
                                     (args))
 */

     public String racerAnswerQueryWithExplanation(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-answer-query-with-explanation" , resArgs, query ).toString();
     }

     public RacerResult racerAnswerQueryWithExplanation$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-answer-query-with-explanation" , resArgs, query );
     }

     public String racerAnswerQueryWithExplanation(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-answer-query-with-explanation" , resArgs, query , keyArgs).toString();
     }

     public RacerResult racerAnswerQueryWithExplanation$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-answer-query-with-explanation" , resArgs, query , keyArgs);
     }

/** Racer Function racer-answer-query1
(racer-answer-query1 query
                     res-args
                     &key
                     execute-p
                     dont-add-abox-duplicates-p
                     remove-duplicates-p
                     two-phase-processing-p
                     deliver-phase-two-warning-tokens-p
                     deliver-kb-has-changed-warning-tokens-p
                     add-rule-consequences-p
                     continuation-based-instance-retrieval-p
                     told-information-reasoning-p
                     final-consistency-checking-p
                     runtime-consistency-checking-p
                     verbose-p
                     dont-show-variables
                     dont-show-head-projection-operators-p
                     dont-show-lambdas-p
                     how-many
                     only-new-tuples-p
                     timeout
                     proactive-tuple-computation-p
                     tuple-at-a-time-p
                     use-individual-synonyms-p
                     check-abox-consistency-p
                     ensure-tbox-classification-p
                     initial-abox-mirroring-p
                     initial-role-assertion-mirroring-p
                     classify-concepts-in-instance-assertions-p
                     exclude-permutations-p
                     record-explanations-p
                     parser-class
                     rewrite-defined-concepts-p
                     group-by-ops
                     bind-specials-p
                     original-query
                     rule-con-pattern
                     new-ind-ops
                     premise
                     generate-code-p
                     optimize-p
                     rewrite-semantically-p
                     rewrite-to-dnf-p
                     report-inconsistent-queries-p
                     report-tautological-queries-p
                     use-repository-p
                     put-into-repository-p
                     id
                     dont-check-id-p
                     parser
                     result-vois
                     substrate
                     abox
                     create-abox-if-not-found-p
                     package
                     type-of-substrate
                     prepare-now-p)
 */

     public String racerAnswerQuery1(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-answer-query1" , query, resArgs ).toString();
     }

     public RacerResult racerAnswerQuery1$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-answer-query1" , query, resArgs );
     }

     public String racerAnswerQuery1(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-answer-query1" , query, resArgs , keyArgs).toString();
     }

     public RacerResult racerAnswerQuery1$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-answer-query1" , query, resArgs , keyArgs);
     }

/** Racer Function racer-answer-tbox-query
(racer-answer-tbox-query res-args
                         query
                         &key
                         execute-p
                         dont-add-abox-duplicates-p
                         remove-duplicates-p
                         two-phase-processing-p
                         deliver-phase-two-warning-tokens-p
                         deliver-kb-has-changed-warning-tokens-p
                         add-rule-consequences-p
                         continuation-based-instance-retrieval-p
                         told-information-reasoning-p
                         final-consistency-checking-p
                         runtime-consistency-checking-p
                         verbose-p
                         dont-show-variables
                         dont-show-head-projection-operators-p
                         dont-show-lambdas-p
                         how-many
                         only-new-tuples-p
                         timeout
                         proactive-tuple-computation-p
                         tuple-at-a-time-p
                         use-individual-synonyms-p
                         check-abox-consistency-p
                         ensure-tbox-classification-p
                         initial-abox-mirroring-p
                         initial-role-assertion-mirroring-p
                         classify-concepts-in-instance-assertions-p
                         exclude-permutations-p
                         record-explanations-p
                         parser-class
                         rewrite-defined-concepts-p
                         group-by-ops
                         bind-specials-p
                         original-query
                         rule-con-pattern
                         new-ind-ops
                         premise
                         generate-code-p
                         optimize-p
                         rewrite-semantically-p
                         rewrite-to-dnf-p
                         report-inconsistent-queries-p
                         report-tautological-queries-p
                         use-repository-p
                         put-into-repository-p
                         id
                         dont-check-id-p
                         parser
                         result-vois
                         tbox
                         package
                         create-tbox-if-not-found-p
                         substrate)
 */

     public String racerAnswerTboxQuery(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-answer-tbox-query" , resArgs, query ).toString();
     }

     public RacerResult racerAnswerTboxQuery$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-answer-tbox-query" , resArgs, query );
     }

     public String racerAnswerTboxQuery(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-answer-tbox-query" , resArgs, query , keyArgs).toString();
     }

     public RacerResult racerAnswerTboxQuery$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-answer-tbox-query" , resArgs, query , keyArgs);
     }

/** Racer Function racer-answer-tbox-query1
(racer-answer-tbox-query1 query
                          res-args
                          &key
                          execute-p
                          dont-add-abox-duplicates-p
                          remove-duplicates-p
                          two-phase-processing-p
                          deliver-phase-two-warning-tokens-p
                          deliver-kb-has-changed-warning-tokens-p
                          add-rule-consequences-p
                          continuation-based-instance-retrieval-p
                          told-information-reasoning-p
                          final-consistency-checking-p
                          runtime-consistency-checking-p
                          verbose-p
                          dont-show-variables
                          dont-show-head-projection-operators-p
                          dont-show-lambdas-p
                          how-many
                          only-new-tuples-p
                          timeout
                          proactive-tuple-computation-p
                          tuple-at-a-time-p
                          use-individual-synonyms-p
                          check-abox-consistency-p
                          ensure-tbox-classification-p
                          initial-abox-mirroring-p
                          initial-role-assertion-mirroring-p
                          classify-concepts-in-instance-assertions-p
                          exclude-permutations-p
                          record-explanations-p
                          parser-class
                          rewrite-defined-concepts-p
                          group-by-ops
                          bind-specials-p
                          original-query
                          rule-con-pattern
                          new-ind-ops
                          premise
                          generate-code-p
                          optimize-p
                          rewrite-semantically-p
                          rewrite-to-dnf-p
                          report-inconsistent-queries-p
                          report-tautological-queries-p
                          use-repository-p
                          put-into-repository-p
                          id
                          dont-check-id-p
                          parser
                          result-vois
                          tbox
                          package
                          create-tbox-if-not-found-p
                          substrate)
 */

     public String racerAnswerTboxQuery1(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-answer-tbox-query1" , query, resArgs ).toString();
     }

     public RacerResult racerAnswerTboxQuery1$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-answer-tbox-query1" , query, resArgs );
     }

     public String racerAnswerTboxQuery1(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-answer-tbox-query1" , query, resArgs , keyArgs).toString();
     }

     public RacerResult racerAnswerTboxQuery1$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-answer-tbox-query1" , query, resArgs , keyArgs);
     }

/** Racer Function racer-apply-rule
(racer-apply-rule query
                  res-args
                  &key
                  execute-p
                  parser-class
                  rewrite-defined-concepts-p
                  group-by-ops
                  bind-specials-p
                  original-query
                  rule-con-pattern
                  new-ind-ops
                  premise
                  generate-code-p
                  optimize-p
                  rewrite-semantically-p
                  rewrite-to-dnf-p
                  report-inconsistent-queries-p
                  report-tautological-queries-p
                  use-repository-p
                  put-into-repository-p
                  id
                  dont-check-id-p
                  parser
                  result-vois
                  substrate
                  abox
                  create-abox-if-not-found-p
                  package
                  type-of-substrate
                  prepare-now-p)
 */

     public String racerApplyRule(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-apply-rule" , query, resArgs ).toString();
     }

     public RacerResult racerApplyRule$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-apply-rule" , query, resArgs );
     }

     public String racerApplyRule(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-apply-rule" , query, resArgs , keyArgs).toString();
     }

     public RacerResult racerApplyRule$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-apply-rule" , query, resArgs , keyArgs);
     }

/** Racer Function racer-apply-rule-under-premise
(racer-apply-rule-under-premise query
                                res-args
                                &key
                                execute-p
                                parser-class
                                rewrite-defined-concepts-p
                                group-by-ops
                                bind-specials-p
                                original-query
                                rule-con-pattern
                                new-ind-ops
                                premise
                                generate-code-p
                                optimize-p
                                rewrite-semantically-p
                                rewrite-to-dnf-p
                                report-inconsistent-queries-p
                                report-tautological-queries-p
                                use-repository-p
                                put-into-repository-p
                                id
                                dont-check-id-p
                                parser
                                result-vois
                                substrate
                                abox
                                create-abox-if-not-found-p
                                package
                                type-of-substrate
                                prepare-now-p)
 */

     public String racerApplyRuleUnderPremise(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-apply-rule-under-premise" , query, resArgs ).toString();
     }

     public RacerResult racerApplyRuleUnderPremise$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-apply-rule-under-premise" , query, resArgs );
     }

     public String racerApplyRuleUnderPremise(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-apply-rule-under-premise" , query, resArgs , keyArgs).toString();
     }

     public RacerResult racerApplyRuleUnderPremise$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-apply-rule-under-premise" , query, resArgs , keyArgs);
     }

/** Racer Function racer-apply-rule-under-premise1
(racer-apply-rule-under-premise1 res-args
                                 query
                                 &key
                                 execute-p
                                 parser-class
                                 rewrite-defined-concepts-p
                                 group-by-ops
                                 bind-specials-p
                                 original-query
                                 rule-con-pattern
                                 new-ind-ops
                                 premise
                                 generate-code-p
                                 optimize-p
                                 rewrite-semantically-p
                                 rewrite-to-dnf-p
                                 report-inconsistent-queries-p
                                 report-tautological-queries-p
                                 use-repository-p
                                 put-into-repository-p
                                 id
                                 dont-check-id-p
                                 parser
                                 result-vois
                                 substrate
                                 abox
                                 create-abox-if-not-found-p
                                 package
                                 type-of-substrate
                                 prepare-now-p)
 */

     public String racerApplyRuleUnderPremise1(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-apply-rule-under-premise1" , resArgs, query ).toString();
     }

     public RacerResult racerApplyRuleUnderPremise1$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-apply-rule-under-premise1" , resArgs, query );
     }

     public String racerApplyRuleUnderPremise1(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-apply-rule-under-premise1" , resArgs, query , keyArgs).toString();
     }

     public RacerResult racerApplyRuleUnderPremise1$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-apply-rule-under-premise1" , resArgs, query , keyArgs);
     }

/** Racer Function racer-apply-rule1
(racer-apply-rule1 res-args
                   query
                   &key
                   execute-p
                   parser-class
                   rewrite-defined-concepts-p
                   group-by-ops
                   bind-specials-p
                   original-query
                   rule-con-pattern
                   new-ind-ops
                   premise
                   generate-code-p
                   optimize-p
                   rewrite-semantically-p
                   rewrite-to-dnf-p
                   report-inconsistent-queries-p
                   report-tautological-queries-p
                   use-repository-p
                   put-into-repository-p
                   id
                   dont-check-id-p
                   parser
                   result-vois
                   substrate
                   abox
                   create-abox-if-not-found-p
                   package
                   type-of-substrate
                   prepare-now-p)
 */

     public String racerApplyRule1(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-apply-rule1" , resArgs, query ).toString();
     }

     public RacerResult racerApplyRule1$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-apply-rule1" , resArgs, query );
     }

     public String racerApplyRule1(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-apply-rule1" , resArgs, query , keyArgs).toString();
     }

     public RacerResult racerApplyRule1$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-apply-rule1" , resArgs, query , keyArgs);
     }

/** Racer Function racer-prepare-query
(racer-prepare-query res-args
                     query
                     &key
                     execute-p
                     dont-add-abox-duplicates-p
                     remove-duplicates-p
                     two-phase-processing-p
                     deliver-phase-two-warning-tokens-p
                     deliver-kb-has-changed-warning-tokens-p
                     add-rule-consequences-p
                     continuation-based-instance-retrieval-p
                     told-information-reasoning-p
                     final-consistency-checking-p
                     runtime-consistency-checking-p
                     verbose-p
                     dont-show-variables
                     dont-show-head-projection-operators-p
                     dont-show-lambdas-p
                     how-many
                     only-new-tuples-p
                     timeout
                     proactive-tuple-computation-p
                     tuple-at-a-time-p
                     use-individual-synonyms-p
                     check-abox-consistency-p
                     ensure-tbox-classification-p
                     initial-abox-mirroring-p
                     initial-role-assertion-mirroring-p
                     classify-concepts-in-instance-assertions-p
                     exclude-permutations-p
                     record-explanations-p
                     parser-class
                     rewrite-defined-concepts-p
                     group-by-ops
                     bind-specials-p
                     original-query
                     rule-con-pattern
                     new-ind-ops
                     premise
                     generate-code-p
                     optimize-p
                     rewrite-semantically-p
                     rewrite-to-dnf-p
                     report-inconsistent-queries-p
                     report-tautological-queries-p
                     use-repository-p
                     put-into-repository-p
                     id
                     dont-check-id-p
                     parser
                     result-vois
                     substrate
                     abox
                     create-abox-if-not-found-p
                     package
                     type-of-substrate
                     prepare-now-p)
 */

     public String racerPrepareQuery(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-prepare-query" , resArgs, query ).toString();
     }

     public RacerResult racerPrepareQuery$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-prepare-query" , resArgs, query );
     }

     public String racerPrepareQuery(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-prepare-query" , resArgs, query , keyArgs).toString();
     }

     public RacerResult racerPrepareQuery$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-prepare-query" , resArgs, query , keyArgs);
     }

/** Racer Function racer-prepare-query1
(racer-prepare-query1 query
                      res-args
                      &key
                      execute-p
                      dont-add-abox-duplicates-p
                      remove-duplicates-p
                      two-phase-processing-p
                      deliver-phase-two-warning-tokens-p
                      deliver-kb-has-changed-warning-tokens-p
                      add-rule-consequences-p
                      continuation-based-instance-retrieval-p
                      told-information-reasoning-p
                      final-consistency-checking-p
                      runtime-consistency-checking-p
                      verbose-p
                      dont-show-variables
                      dont-show-head-projection-operators-p
                      dont-show-lambdas-p
                      how-many
                      only-new-tuples-p
                      timeout
                      proactive-tuple-computation-p
                      tuple-at-a-time-p
                      use-individual-synonyms-p
                      check-abox-consistency-p
                      ensure-tbox-classification-p
                      initial-abox-mirroring-p
                      initial-role-assertion-mirroring-p
                      classify-concepts-in-instance-assertions-p
                      exclude-permutations-p
                      record-explanations-p
                      parser-class
                      rewrite-defined-concepts-p
                      group-by-ops
                      bind-specials-p
                      original-query
                      rule-con-pattern
                      new-ind-ops
                      premise
                      generate-code-p
                      optimize-p
                      rewrite-semantically-p
                      rewrite-to-dnf-p
                      report-inconsistent-queries-p
                      report-tautological-queries-p
                      use-repository-p
                      put-into-repository-p
                      id
                      dont-check-id-p
                      parser
                      result-vois
                      substrate
                      abox
                      create-abox-if-not-found-p
                      package
                      type-of-substrate
                      prepare-now-p)
 */

     public String racerPrepareQuery1(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-prepare-query1" , query, resArgs ).toString();
     }

     public RacerResult racerPrepareQuery1$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-prepare-query1" , query, resArgs );
     }

     public String racerPrepareQuery1(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-prepare-query1" , query, resArgs , keyArgs).toString();
     }

     public RacerResult racerPrepareQuery1$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-prepare-query1" , query, resArgs , keyArgs);
     }

/** Racer Function racer-prepare-rule
(racer-prepare-rule query
                    res-args
                    &key
                    execute-p
                    parser-class
                    rewrite-defined-concepts-p
                    group-by-ops
                    bind-specials-p
                    original-query
                    rule-con-pattern
                    new-ind-ops
                    premise
                    generate-code-p
                    optimize-p
                    rewrite-semantically-p
                    rewrite-to-dnf-p
                    report-inconsistent-queries-p
                    report-tautological-queries-p
                    use-repository-p
                    put-into-repository-p
                    id
                    dont-check-id-p
                    parser
                    result-vois
                    substrate
                    abox
                    create-abox-if-not-found-p
                    package
                    type-of-substrate
                    prepare-now-p)
 */

     public String racerPrepareRule(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-prepare-rule" , query, resArgs ).toString();
     }

     public RacerResult racerPrepareRule$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-prepare-rule" , query, resArgs );
     }

     public String racerPrepareRule(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-prepare-rule" , query, resArgs , keyArgs).toString();
     }

     public RacerResult racerPrepareRule$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-prepare-rule" , query, resArgs , keyArgs);
     }

/** Racer Function racer-prepare-rule1
(racer-prepare-rule1 res-args
                     query
                     &key
                     execute-p
                     parser-class
                     rewrite-defined-concepts-p
                     group-by-ops
                     bind-specials-p
                     original-query
                     rule-con-pattern
                     new-ind-ops
                     premise
                     generate-code-p
                     optimize-p
                     rewrite-semantically-p
                     rewrite-to-dnf-p
                     report-inconsistent-queries-p
                     report-tautological-queries-p
                     use-repository-p
                     put-into-repository-p
                     id
                     dont-check-id-p
                     parser
                     result-vois
                     substrate
                     abox
                     create-abox-if-not-found-p
                     package
                     type-of-substrate
                     prepare-now-p)
 */

     public String racerPrepareRule1(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-prepare-rule1" , resArgs, query ).toString();
     }

     public RacerResult racerPrepareRule1$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-prepare-rule1" , resArgs, query );
     }

     public String racerPrepareRule1(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-prepare-rule1" , resArgs, query , keyArgs).toString();
     }

     public RacerResult racerPrepareRule1$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-prepare-rule1" , resArgs, query , keyArgs);
     }

/** Racer Function racer-prepare-tbox-query
(racer-prepare-tbox-query res-args
                          query
                          &key
                          execute-p
                          dont-add-abox-duplicates-p
                          remove-duplicates-p
                          two-phase-processing-p
                          deliver-phase-two-warning-tokens-p
                          deliver-kb-has-changed-warning-tokens-p
                          add-rule-consequences-p
                          continuation-based-instance-retrieval-p
                          told-information-reasoning-p
                          final-consistency-checking-p
                          runtime-consistency-checking-p
                          verbose-p
                          dont-show-variables
                          dont-show-head-projection-operators-p
                          dont-show-lambdas-p
                          how-many
                          only-new-tuples-p
                          timeout
                          proactive-tuple-computation-p
                          tuple-at-a-time-p
                          use-individual-synonyms-p
                          check-abox-consistency-p
                          ensure-tbox-classification-p
                          initial-abox-mirroring-p
                          initial-role-assertion-mirroring-p
                          classify-concepts-in-instance-assertions-p
                          exclude-permutations-p
                          record-explanations-p
                          parser-class
                          rewrite-defined-concepts-p
                          group-by-ops
                          bind-specials-p
                          original-query
                          rule-con-pattern
                          new-ind-ops
                          premise
                          generate-code-p
                          optimize-p
                          rewrite-semantically-p
                          rewrite-to-dnf-p
                          report-inconsistent-queries-p
                          report-tautological-queries-p
                          use-repository-p
                          put-into-repository-p
                          id
                          dont-check-id-p
                          parser
                          result-vois
                          tbox
                          package
                          create-tbox-if-not-found-p
                          substrate)
 */

     public String racerPrepareTboxQuery(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-prepare-tbox-query" , resArgs, query ).toString();
     }

     public RacerResult racerPrepareTboxQuery$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("racer-prepare-tbox-query" , resArgs, query );
     }

     public String racerPrepareTboxQuery(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-prepare-tbox-query" , resArgs, query , keyArgs).toString();
     }

     public RacerResult racerPrepareTboxQuery$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-prepare-tbox-query" , resArgs, query , keyArgs);
     }

/** Racer Function racer-prepare-tbox-query1
(racer-prepare-tbox-query1 query
                           res-args
                           &key
                           execute-p
                           dont-add-abox-duplicates-p
                           remove-duplicates-p
                           two-phase-processing-p
                           deliver-phase-two-warning-tokens-p
                           deliver-kb-has-changed-warning-tokens-p
                           add-rule-consequences-p
                           continuation-based-instance-retrieval-p
                           told-information-reasoning-p
                           final-consistency-checking-p
                           runtime-consistency-checking-p
                           verbose-p
                           dont-show-variables
                           dont-show-head-projection-operators-p
                           dont-show-lambdas-p
                           how-many
                           only-new-tuples-p
                           timeout
                           proactive-tuple-computation-p
                           tuple-at-a-time-p
                           use-individual-synonyms-p
                           check-abox-consistency-p
                           ensure-tbox-classification-p
                           initial-abox-mirroring-p
                           initial-role-assertion-mirroring-p
                           classify-concepts-in-instance-assertions-p
                           exclude-permutations-p
                           record-explanations-p
                           parser-class
                           rewrite-defined-concepts-p
                           group-by-ops
                           bind-specials-p
                           original-query
                           rule-con-pattern
                           new-ind-ops
                           premise
                           generate-code-p
                           optimize-p
                           rewrite-semantically-p
                           rewrite-to-dnf-p
                           report-inconsistent-queries-p
                           report-tautological-queries-p
                           use-repository-p
                           put-into-repository-p
                           id
                           dont-check-id-p
                           parser
                           result-vois
                           tbox
                           package
                           create-tbox-if-not-found-p
                           substrate)
 */

     public String racerPrepareTboxQuery1(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-prepare-tbox-query1" , query, resArgs ).toString();
     }

     public RacerResult racerPrepareTboxQuery1$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("racer-prepare-tbox-query1" , query, resArgs );
     }

     public String racerPrepareTboxQuery1(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-prepare-tbox-query1" , query, resArgs , keyArgs).toString();
     }

     public RacerResult racerPrepareTboxQuery1$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-prepare-tbox-query1" , query, resArgs , keyArgs);
     }

/** Racer Function racer-read-document
(racer-read-document url-spec &key verbose)
 */

     public String racerReadDocument(Object urlSpec ) throws RacerClientException {
          return racerCall("racer-read-document" , urlSpec ).toString();
     }

     public RacerResult racerReadDocument$(Object urlSpec ) throws RacerClientException {
          return racerCall("racer-read-document" , urlSpec );
     }

     public String racerReadDocument(Object urlSpec , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-read-document" , urlSpec , keyArgs).toString();
     }

     public RacerResult racerReadDocument$(Object urlSpec , Object... keyArgs) throws RacerClientException {
          return racerCall("racer-read-document" , urlSpec , keyArgs);
     }

/** Racer Function racer-read-file
(racer-read-file filename)
 */

     public String racerReadFile(Object filename ) throws RacerClientException {
          return racerCall("racer-read-file" , filename ).toString();
     }

     public RacerResult racerReadFile$(Object filename ) throws RacerClientException {
          return racerCall("racer-read-file" , filename );
     }

/** Racer Function rcc-consistent-p
(rcc-consistent-p &optional
                  abox
                  type-of-substrate)
 */

     public boolean rccConsistentP( ) throws RacerClientException {
          return returnBoolean(racerCall("rcc-consistent-p"  ));
     }

     public boolean rccConsistentP(Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return returnBoolean(racerCall("rcc-consistent-p" , abox, typeOfSubstrate ));
     }

     public boolean rccConsistentP(Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("rcc-consistent-p" , abox ));
     }

/** Racer Function rcc-edge-description1
(rcc-edge-description1)
 */

     public String rccEdgeDescription1( ) throws RacerClientException {
          return racerCall("rcc-edge-description1"  ).toString();
     }

     public RacerResult rccEdgeDescription1$( ) throws RacerClientException {
          return racerCall("rcc-edge-description1"  );
     }

/** Racer Function rcc-edge-label1
(rcc-edge-label1)
 */

     public String rccEdgeLabel1( ) throws RacerClientException {
          return racerCall("rcc-edge-label1"  ).toString();
     }

     public RacerResult rccEdgeLabel1$( ) throws RacerClientException {
          return racerCall("rcc-edge-label1"  );
     }

/** Racer Function rcc-edge1
(rcc-edge1)
 */

     public String rccEdge1( ) throws RacerClientException {
          return racerCall("rcc-edge1"  ).toString();
     }

     public RacerResult rccEdge1$( ) throws RacerClientException {
          return racerCall("rcc-edge1"  );
     }

/** Racer Function rcc-instance1
(rcc-instance1)
 */

     public String rccInstance1( ) throws RacerClientException {
          return racerCall("rcc-instance1"  ).toString();
     }

     public RacerResult rccInstance1$( ) throws RacerClientException {
          return racerCall("rcc-instance1"  );
     }

/** Racer Function rcc-node-description1
(rcc-node-description1)
 */

     public String rccNodeDescription1( ) throws RacerClientException {
          return racerCall("rcc-node-description1"  ).toString();
     }

     public RacerResult rccNodeDescription1$( ) throws RacerClientException {
          return racerCall("rcc-node-description1"  );
     }

/** Racer Function rcc-node-label1
(rcc-node-label1)
 */

     public String rccNodeLabel1( ) throws RacerClientException {
          return racerCall("rcc-node-label1"  ).toString();
     }

     public RacerResult rccNodeLabel1$( ) throws RacerClientException {
          return racerCall("rcc-node-label1"  );
     }

/** Racer Function rcc-node1
(rcc-node1)
 */

     public String rccNode1( ) throws RacerClientException {
          return racerCall("rcc-node1"  ).toString();
     }

     public RacerResult rccNode1$( ) throws RacerClientException {
          return racerCall("rcc-node1"  );
     }

/** Racer Function rcc-related1
(rcc-related1)
 */

     public String rccRelated1( ) throws RacerClientException {
          return racerCall("rcc-related1"  ).toString();
     }

     public RacerResult rccRelated1$( ) throws RacerClientException {
          return racerCall("rcc-related1"  );
     }

/** Racer Function rdfs-read-tbox-file
(rdfs-read-tbox-file filename)
 */

     public String rdfsReadTboxFile(Object filename ) throws RacerClientException {
          return racerCall("rdfs-read-tbox-file" , filename ).toString();
     }

     public RacerResult rdfsReadTboxFile$(Object filename ) throws RacerClientException {
          return racerCall("rdfs-read-tbox-file" , filename );
     }

/** Racer Function ready-queries
(ready-queries &key
               abox
               type-of-substrate)
 */

     public String readyQueries( ) throws RacerClientException {
          return racerCall("ready-queries"  ).toString();
     }

     public RacerResult readyQueries$( ) throws RacerClientException {
          return racerCall("ready-queries"  );
     }

     public String readyQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("ready-queries"  , keyArgs).toString();
     }

     public RacerResult readyQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("ready-queries"  , keyArgs);
     }

/** Racer Function ready-rules
(ready-rules &key
             abox
             type-of-substrate)
 */

     public String readyRules( ) throws RacerClientException {
          return racerCall("ready-rules"  ).toString();
     }

     public RacerResult readyRules$( ) throws RacerClientException {
          return racerCall("ready-rules"  );
     }

     public String readyRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("ready-rules"  , keyArgs).toString();
     }

     public RacerResult readyRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("ready-rules"  , keyArgs);
     }

/** Racer Function realize-abox
(realize-abox &optional abox individual-name)
 */

     public String realizeAbox( ) throws RacerClientException {
          return racerCall("realize-abox"  ).toString();
     }

     public RacerResult realizeAbox$( ) throws RacerClientException {
          return racerCall("realize-abox"  );
     }

     public String realizeAbox(Object abox, Object individualName ) throws RacerClientException {
          return racerCall("realize-abox" , abox, individualName ).toString();
     }

     public RacerResult realizeAbox$(Object abox, Object individualName ) throws RacerClientException {
          return racerCall("realize-abox" , abox, individualName );
     }

     public String realizeAbox(Object abox ) throws RacerClientException {
          return racerCall("realize-abox" , abox ).toString();
     }

     public RacerResult realizeAbox$(Object abox ) throws RacerClientException {
          return racerCall("realize-abox" , abox );
     }

/** Racer Function recognize-events
(recognize-events &optional abox)
 */

     public String recognizeEvents( ) throws RacerClientException {
          return racerCall("recognize-events"  ).toString();
     }

     public RacerResult recognizeEvents$( ) throws RacerClientException {
          return racerCall("recognize-events"  );
     }

     public String recognizeEvents(Object abox ) throws RacerClientException {
          return racerCall("recognize-events" , abox ).toString();
     }

     public RacerResult recognizeEvents$(Object abox ) throws RacerClientException {
          return racerCall("recognize-events" , abox );
     }

/** Racer Function reexecute-all-queries
(reexecute-all-queries &key
                       abox
                       type-of-substrate)
 */

     public String reexecuteAllQueries( ) throws RacerClientException {
          return racerCall("reexecute-all-queries"  ).toString();
     }

     public RacerResult reexecuteAllQueries$( ) throws RacerClientException {
          return racerCall("reexecute-all-queries"  );
     }

     public String reexecuteAllQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("reexecute-all-queries"  , keyArgs).toString();
     }

     public RacerResult reexecuteAllQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("reexecute-all-queries"  , keyArgs);
     }

/** Racer Function reexecute-all-rules
(reexecute-all-rules &key
                     abox
                     type-of-substrate)
 */

     public String reexecuteAllRules( ) throws RacerClientException {
          return racerCall("reexecute-all-rules"  ).toString();
     }

     public RacerResult reexecuteAllRules$( ) throws RacerClientException {
          return racerCall("reexecute-all-rules"  );
     }

     public String reexecuteAllRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("reexecute-all-rules"  , keyArgs).toString();
     }

     public RacerResult reexecuteAllRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("reexecute-all-rules"  , keyArgs);
     }

/** Racer Function reexecute-query
(reexecute-query query)
 */

     public String reexecuteQuery(Object query ) throws RacerClientException {
          return racerCall("reexecute-query" , query ).toString();
     }

     public RacerResult reexecuteQuery$(Object query ) throws RacerClientException {
          return racerCall("reexecute-query" , query );
     }

/** Racer Function reexecute-rule
(reexecute-rule query)
 */

     public String reexecuteRule(Object query ) throws RacerClientException {
          return racerCall("reexecute-rule" , query ).toString();
     }

     public RacerResult reexecuteRule$(Object query ) throws RacerClientException {
          return racerCall("reexecute-rule" , query );
     }

/** Racer Function reflexive-p
(reflexive-p role-term &optional tbox)
 */

     public boolean reflexiveP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("reflexive-p" , roleTerm ));
     }

     public boolean reflexiveP(Object roleTerm, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("reflexive-p" , roleTerm, tbox ));
     }

/** Racer Function register-rcc-synonym
(register-rcc-synonym role
                      rcc-relation)
 */

     public String registerRccSynonym(Object role, Object rccRelation ) throws RacerClientException {
          return racerCall("register-rcc-synonym" , role, rccRelation ).toString();
     }

     public RacerResult registerRccSynonym$(Object role, Object rccRelation ) throws RacerClientException {
          return racerCall("register-rcc-synonym" , role, rccRelation );
     }

/** Racer Function remove-implied-concept-assertions
(remove-implied-concept-assertions abox)
 */

     public String removeImpliedConceptAssertions(Object abox ) throws RacerClientException {
          return racerCall("remove-implied-concept-assertions" , abox ).toString();
     }

     public RacerResult removeImpliedConceptAssertions$(Object abox ) throws RacerClientException {
          return racerCall("remove-implied-concept-assertions" , abox );
     }

/** Racer Function report-inconsistent-queries-and-rules
(report-inconsistent-queries-and-rules)
 */

     public String reportInconsistentQueriesAndRules( ) throws RacerClientException {
          return racerCall("report-inconsistent-queries-and-rules"  ).toString();
     }

     public RacerResult reportInconsistentQueriesAndRules$( ) throws RacerClientException {
          return racerCall("report-inconsistent-queries-and-rules"  );
     }

/** Racer Function reprepare-query
(reprepare-query query
                 &key
                 to-substrate
                 copy-p
                 new-id)
 */

     public String reprepareQuery(Object query ) throws RacerClientException {
          return racerCall("reprepare-query" , query ).toString();
     }

     public RacerResult reprepareQuery$(Object query ) throws RacerClientException {
          return racerCall("reprepare-query" , query );
     }

     public String reprepareQuery(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("reprepare-query" , query , keyArgs).toString();
     }

     public RacerResult reprepareQuery$(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("reprepare-query" , query , keyArgs);
     }

/** Racer Function reprepare-rule
(reprepare-rule query)
 */

     public String reprepareRule(Object query ) throws RacerClientException {
          return racerCall("reprepare-rule" , query ).toString();
     }

     public RacerResult reprepareRule$(Object query ) throws RacerClientException {
          return racerCall("reprepare-rule" , query );
     }

/** Racer Function reset-all-substrates
(reset-all-substrates &key
                      abox
                      type-of-substrate)
 */

     public String resetAllSubstrates( ) throws RacerClientException {
          return racerCall("reset-all-substrates"  ).toString();
     }

     public RacerResult resetAllSubstrates$( ) throws RacerClientException {
          return racerCall("reset-all-substrates"  );
     }

     public String resetAllSubstrates(  Object... keyArgs) throws RacerClientException {
          return racerCall("reset-all-substrates"  , keyArgs).toString();
     }

     public RacerResult resetAllSubstrates$(  Object... keyArgs) throws RacerClientException {
          return racerCall("reset-all-substrates"  , keyArgs);
     }

/** Racer Function reset-nrql-engine
(reset-nrql-engine &key full-reset-p)
 */

     public String resetNrqlEngine( ) throws RacerClientException {
          return racerCall("reset-nrql-engine"  ).toString();
     }

     public RacerResult resetNrqlEngine$( ) throws RacerClientException {
          return racerCall("reset-nrql-engine"  );
     }

     public String resetNrqlEngine(  Object... keyArgs) throws RacerClientException {
          return racerCall("reset-nrql-engine"  , keyArgs).toString();
     }

     public RacerResult resetNrqlEngine$(  Object... keyArgs) throws RacerClientException {
          return racerCall("reset-nrql-engine"  , keyArgs);
     }

/** Racer Function restore-abox-image
(restore-abox-image filename)
 */

     public String restoreAboxImage(Object filename ) throws RacerClientException {
          return racerCall("restore-abox-image" , filename ).toString();
     }

     public RacerResult restoreAboxImage$(Object filename ) throws RacerClientException {
          return racerCall("restore-abox-image" , filename );
     }

/** Racer Function restore-aboxes-image
(restore-aboxes-image filename)
 */

     public String restoreAboxesImage(Object filename ) throws RacerClientException {
          return racerCall("restore-aboxes-image" , filename ).toString();
     }

     public RacerResult restoreAboxesImage$(Object filename ) throws RacerClientException {
          return racerCall("restore-aboxes-image" , filename );
     }

/** Racer Function restore-all-substrates
(restore-all-substrates filename)
 */

     public String restoreAllSubstrates(Object filename ) throws RacerClientException {
          return racerCall("restore-all-substrates" , filename ).toString();
     }

     public RacerResult restoreAllSubstrates$(Object filename ) throws RacerClientException {
          return racerCall("restore-all-substrates" , filename );
     }

/** Racer Function restore-kb-image
(restore-kb-image filename)
 */

     public String restoreKbImage(Object filename ) throws RacerClientException {
          return racerCall("restore-kb-image" , filename ).toString();
     }

     public RacerResult restoreKbImage$(Object filename ) throws RacerClientException {
          return racerCall("restore-kb-image" , filename );
     }

/** Racer Function restore-kbs-image
(restore-kbs-image filename)
 */

     public String restoreKbsImage(Object filename ) throws RacerClientException {
          return racerCall("restore-kbs-image" , filename ).toString();
     }

     public RacerResult restoreKbsImage$(Object filename ) throws RacerClientException {
          return racerCall("restore-kbs-image" , filename );
     }

/** Racer Function restore-server-image
(restore-server-image filename)
 */

     public String restoreServerImage(Object filename ) throws RacerClientException {
          return racerCall("restore-server-image" , filename ).toString();
     }

     public RacerResult restoreServerImage$(Object filename ) throws RacerClientException {
          return racerCall("restore-server-image" , filename );
     }

/** Racer Function restore-standard-settings
(restore-standard-settings)
 */

     public String restoreStandardSettings( ) throws RacerClientException {
          return racerCall("restore-standard-settings"  ).toString();
     }

     public RacerResult restoreStandardSettings$( ) throws RacerClientException {
          return racerCall("restore-standard-settings"  );
     }

/** Racer Function restore-substrate
(restore-substrate filename)
 */

     public String restoreSubstrate(Object filename ) throws RacerClientException {
          return racerCall("restore-substrate" , filename ).toString();
     }

     public RacerResult restoreSubstrate$(Object filename ) throws RacerClientException {
          return racerCall("restore-substrate" , filename );
     }

/** Racer Function restore-tbox-image
(restore-tbox-image filename)
 */

     public String restoreTboxImage(Object filename ) throws RacerClientException {
          return racerCall("restore-tbox-image" , filename ).toString();
     }

     public RacerResult restoreTboxImage$(Object filename ) throws RacerClientException {
          return racerCall("restore-tbox-image" , filename );
     }

/** Racer Function restore-tboxes-image
(restore-tboxes-image filename)
 */

     public String restoreTboxesImage(Object filename ) throws RacerClientException {
          return racerCall("restore-tboxes-image" , filename ).toString();
     }

     public RacerResult restoreTboxesImage$(Object filename ) throws RacerClientException {
          return racerCall("restore-tboxes-image" , filename );
     }

/** Racer Function retrieve-concept-instances
(retrieve-concept-instances concept-term
                            abox
                            &optional
                            candidates)
 */

     public String retrieveConceptInstances(Object conceptTerm, Object abox ) throws RacerClientException {
          return racerCall("retrieve-concept-instances" , conceptTerm, abox ).toString();
     }

     public RacerResult retrieveConceptInstances$(Object conceptTerm, Object abox ) throws RacerClientException {
          return racerCall("retrieve-concept-instances" , conceptTerm, abox );
     }

     public String retrieveConceptInstances(Object conceptTerm, Object abox, Object candidates ) throws RacerClientException {
          return racerCall("retrieve-concept-instances" , conceptTerm, abox, candidates ).toString();
     }

     public RacerResult retrieveConceptInstances$(Object conceptTerm, Object abox, Object candidates ) throws RacerClientException {
          return racerCall("retrieve-concept-instances" , conceptTerm, abox, candidates );
     }

/** Racer Function retrieve-direct-predecessors
(retrieve-direct-predecessors role-term
                              ind-filler
                              abox)
 */

     public String retrieveDirectPredecessors(Object roleTerm, Object indFiller, Object abox ) throws RacerClientException {
          return racerCall("retrieve-direct-predecessors" , roleTerm, indFiller, abox ).toString();
     }

     public RacerResult retrieveDirectPredecessors$(Object roleTerm, Object indFiller, Object abox ) throws RacerClientException {
          return racerCall("retrieve-direct-predecessors" , roleTerm, indFiller, abox );
     }

/** Racer Function retrieve-individual-annotation-property-fillers
(retrieve-individual-annotation-property-fillers individual-name
                                                 role
                                                 abox
                                                 &optional
                                                 with-types-p)
 */

     public String retrieveIndividualAnnotationPropertyFillers(Object individualName, Object role, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-annotation-property-fillers" , individualName, role, abox ).toString();
     }

     public RacerResult retrieveIndividualAnnotationPropertyFillers$(Object individualName, Object role, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-annotation-property-fillers" , individualName, role, abox );
     }

     public String retrieveIndividualAnnotationPropertyFillers(Object individualName, Object role, Object abox, Object withTypesP ) throws RacerClientException {
          return racerCall("retrieve-individual-annotation-property-fillers" , individualName, role, abox, withTypesP ).toString();
     }

     public RacerResult retrieveIndividualAnnotationPropertyFillers$(Object individualName, Object role, Object abox, Object withTypesP ) throws RacerClientException {
          return racerCall("retrieve-individual-annotation-property-fillers" , individualName, role, abox, withTypesP );
     }

/** Racer Function retrieve-individual-antonyms
(retrieve-individual-antonyms individual
                              &optional
                              told-only
                              abox)
 */

     public String retrieveIndividualAntonyms(Object individual ) throws RacerClientException {
          return racerCall("retrieve-individual-antonyms" , individual ).toString();
     }

     public RacerResult retrieveIndividualAntonyms$(Object individual ) throws RacerClientException {
          return racerCall("retrieve-individual-antonyms" , individual );
     }

     public String retrieveIndividualAntonyms(Object individual, Object toldOnly, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-antonyms" , individual, toldOnly, abox ).toString();
     }

     public RacerResult retrieveIndividualAntonyms$(Object individual, Object toldOnly, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-antonyms" , individual, toldOnly, abox );
     }

     public String retrieveIndividualAntonyms(Object individual, Object toldOnly ) throws RacerClientException {
          return racerCall("retrieve-individual-antonyms" , individual, toldOnly ).toString();
     }

     public RacerResult retrieveIndividualAntonyms$(Object individual, Object toldOnly ) throws RacerClientException {
          return racerCall("retrieve-individual-antonyms" , individual, toldOnly );
     }

/** Racer Function retrieve-individual-attribute-fillers
(retrieve-individual-attribute-fillers ind
                                       attribute
                                       abox)
 */

     public String retrieveIndividualAttributeFillers(Object ind, Object attribute, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-attribute-fillers" , ind, attribute, abox ).toString();
     }

     public RacerResult retrieveIndividualAttributeFillers$(Object ind, Object attribute, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-attribute-fillers" , ind, attribute, abox );
     }

/** Racer Function retrieve-individual-filled-roles
(retrieve-individual-filled-roles ind-predecessor
                                  ind-filler
                                  abox
                                  &key
                                  synsets-p
                                  negated-p
                                  no-inverses-p
                                  roles)
 */

     public String retrieveIndividualFilledRoles(Object indPredecessor, Object indFiller, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-filled-roles" , indPredecessor, indFiller, abox ).toString();
     }

     public RacerResult retrieveIndividualFilledRoles$(Object indPredecessor, Object indFiller, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-filled-roles" , indPredecessor, indFiller, abox );
     }

     public String retrieveIndividualFilledRoles(Object indPredecessor, Object indFiller, Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("retrieve-individual-filled-roles" , indPredecessor, indFiller, abox , keyArgs).toString();
     }

     public RacerResult retrieveIndividualFilledRoles$(Object indPredecessor, Object indFiller, Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("retrieve-individual-filled-roles" , indPredecessor, indFiller, abox , keyArgs);
     }

/** Racer Function retrieve-individual-fillers
(retrieve-individual-fillers ind-predecessor
                             role-term
                             abox
                             &key
                             told)
 */

     public String retrieveIndividualFillers(Object indPredecessor, Object roleTerm, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-fillers" , indPredecessor, roleTerm, abox ).toString();
     }

     public RacerResult retrieveIndividualFillers$(Object indPredecessor, Object roleTerm, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-fillers" , indPredecessor, roleTerm, abox );
     }

     public String retrieveIndividualFillers(Object indPredecessor, Object roleTerm, Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("retrieve-individual-fillers" , indPredecessor, roleTerm, abox , keyArgs).toString();
     }

     public RacerResult retrieveIndividualFillers$(Object indPredecessor, Object roleTerm, Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("retrieve-individual-fillers" , indPredecessor, roleTerm, abox , keyArgs);
     }

/** Racer Function retrieve-individual-synonyms
(retrieve-individual-synonyms individual
                              &optional
                              told-only
                              abox)
 */

     public String retrieveIndividualSynonyms(Object individual ) throws RacerClientException {
          return racerCall("retrieve-individual-synonyms" , individual ).toString();
     }

     public RacerResult retrieveIndividualSynonyms$(Object individual ) throws RacerClientException {
          return racerCall("retrieve-individual-synonyms" , individual );
     }

     public String retrieveIndividualSynonyms(Object individual, Object toldOnly, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-synonyms" , individual, toldOnly, abox ).toString();
     }

     public RacerResult retrieveIndividualSynonyms$(Object individual, Object toldOnly, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-synonyms" , individual, toldOnly, abox );
     }

     public String retrieveIndividualSynonyms(Object individual, Object toldOnly ) throws RacerClientException {
          return racerCall("retrieve-individual-synonyms" , individual, toldOnly ).toString();
     }

     public RacerResult retrieveIndividualSynonyms$(Object individual, Object toldOnly ) throws RacerClientException {
          return racerCall("retrieve-individual-synonyms" , individual, toldOnly );
     }

/** Racer Function retrieve-individual-told-attribute-value
(retrieve-individual-told-attribute-value ind
                                          attribute
                                          abox)
 */

     public String retrieveIndividualToldAttributeValue(Object ind, Object attribute, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-told-attribute-value" , ind, attribute, abox ).toString();
     }

     public RacerResult retrieveIndividualToldAttributeValue$(Object ind, Object attribute, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-told-attribute-value" , ind, attribute, abox );
     }

/** Racer Function retrieve-individual-told-datatype-fillers
(retrieve-individual-told-datatype-fillers ind
                                           datatype-role
                                           &optional
                                           direct-p
                                           abox
                                           with-types-p)
 */

     public String retrieveIndividualToldDatatypeFillers(Object ind, Object datatypeRole ) throws RacerClientException {
          return racerCall("retrieve-individual-told-datatype-fillers" , ind, datatypeRole ).toString();
     }

     public RacerResult retrieveIndividualToldDatatypeFillers$(Object ind, Object datatypeRole ) throws RacerClientException {
          return racerCall("retrieve-individual-told-datatype-fillers" , ind, datatypeRole );
     }

     public String retrieveIndividualToldDatatypeFillers(Object ind, Object datatypeRole, Object directP, Object abox, Object withTypesP ) throws RacerClientException {
          return racerCall("retrieve-individual-told-datatype-fillers" , ind, datatypeRole, directP, abox, withTypesP ).toString();
     }

     public RacerResult retrieveIndividualToldDatatypeFillers$(Object ind, Object datatypeRole, Object directP, Object abox, Object withTypesP ) throws RacerClientException {
          return racerCall("retrieve-individual-told-datatype-fillers" , ind, datatypeRole, directP, abox, withTypesP );
     }

     public String retrieveIndividualToldDatatypeFillers(Object ind, Object datatypeRole, Object directP, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-told-datatype-fillers" , ind, datatypeRole, directP, abox ).toString();
     }

     public RacerResult retrieveIndividualToldDatatypeFillers$(Object ind, Object datatypeRole, Object directP, Object abox ) throws RacerClientException {
          return racerCall("retrieve-individual-told-datatype-fillers" , ind, datatypeRole, directP, abox );
     }

     public String retrieveIndividualToldDatatypeFillers(Object ind, Object datatypeRole, Object directP ) throws RacerClientException {
          return racerCall("retrieve-individual-told-datatype-fillers" , ind, datatypeRole, directP ).toString();
     }

     public RacerResult retrieveIndividualToldDatatypeFillers$(Object ind, Object datatypeRole, Object directP ) throws RacerClientException {
          return racerCall("retrieve-individual-told-datatype-fillers" , ind, datatypeRole, directP );
     }

/** Racer Function retrieve-related-individuals
(retrieve-related-individuals role-term abox)
 */

     public String retrieveRelatedIndividuals(Object roleTerm, Object abox ) throws RacerClientException {
          return racerCall("retrieve-related-individuals" , roleTerm, abox ).toString();
     }

     public RacerResult retrieveRelatedIndividuals$(Object roleTerm, Object abox ) throws RacerClientException {
          return racerCall("retrieve-related-individuals" , roleTerm, abox );
     }

/** Racer Function rmi
(rmi args)
 */

     public String rmi(Object args ) throws RacerClientException {
          return racerCall("rmi" , args ).toString();
     }

     public RacerResult rmi$(Object args ) throws RacerClientException {
          return racerCall("rmi" , args );
     }

/** Racer Function role-disjoint-p
(role-disjoint-p role-term-1 role-term-2 tbox)
 */

     public boolean roleDisjointP(Object roleTerm1, Object roleTerm2, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("role-disjoint-p" , roleTerm1, roleTerm2, tbox ));
     }

/** Racer Function role-equivalent-p
(role-equivalent-p role-1 role-2 tbox)
 */

     public boolean roleEquivalentP(Object role1, Object role2, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("role-equivalent-p" , role1, role2, tbox ));
     }

/** Racer Function role-has-domain
(role-has-domain rolename
                 concept
                 tbox
                 &optional
                 errorp)
 */

     public String roleHasDomain(Object rolename, Object concept, Object tbox ) throws RacerClientException {
          return racerCall("role-has-domain" , rolename, concept, tbox ).toString();
     }

     public RacerResult roleHasDomain$(Object rolename, Object concept, Object tbox ) throws RacerClientException {
          return racerCall("role-has-domain" , rolename, concept, tbox );
     }

     public String roleHasDomain(Object rolename, Object concept, Object tbox, Object errorp ) throws RacerClientException {
          return racerCall("role-has-domain" , rolename, concept, tbox, errorp ).toString();
     }

     public RacerResult roleHasDomain$(Object rolename, Object concept, Object tbox, Object errorp ) throws RacerClientException {
          return racerCall("role-has-domain" , rolename, concept, tbox, errorp );
     }

/** Racer Function role-has-parent
(role-has-parent rolename-1 rolename-2 tbox)
 */

     public String roleHasParent(Object rolename1, Object rolename2, Object tbox ) throws RacerClientException {
          return racerCall("role-has-parent" , rolename1, rolename2, tbox ).toString();
     }

     public RacerResult roleHasParent$(Object rolename1, Object rolename2, Object tbox ) throws RacerClientException {
          return racerCall("role-has-parent" , rolename1, rolename2, tbox );
     }

/** Racer Function role-has-range
(role-has-range rolename
                concept
                tbox
                &optional
                errorp)
 */

     public String roleHasRange(Object rolename, Object concept, Object tbox ) throws RacerClientException {
          return racerCall("role-has-range" , rolename, concept, tbox ).toString();
     }

     public RacerResult roleHasRange$(Object rolename, Object concept, Object tbox ) throws RacerClientException {
          return racerCall("role-has-range" , rolename, concept, tbox );
     }

     public String roleHasRange(Object rolename, Object concept, Object tbox, Object errorp ) throws RacerClientException {
          return racerCall("role-has-range" , rolename, concept, tbox, errorp ).toString();
     }

     public RacerResult roleHasRange$(Object rolename, Object concept, Object tbox, Object errorp ) throws RacerClientException {
          return racerCall("role-has-range" , rolename, concept, tbox, errorp );
     }

/** Racer Function role-is-asymmetric
(role-is-asymmetric rolename tbox)
 */

     public String roleIsAsymmetric(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-asymmetric" , rolename, tbox ).toString();
     }

     public RacerResult roleIsAsymmetric$(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-asymmetric" , rolename, tbox );
     }

/** Racer Function role-is-functional
(role-is-functional rolename tbox)
 */

     public String roleIsFunctional(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-functional" , rolename, tbox ).toString();
     }

     public RacerResult roleIsFunctional$(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-functional" , rolename, tbox );
     }

/** Racer Function role-is-irreflexive
(role-is-irreflexive rolename tbox)
 */

     public String roleIsIrreflexive(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-irreflexive" , rolename, tbox ).toString();
     }

     public RacerResult roleIsIrreflexive$(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-irreflexive" , rolename, tbox );
     }

/** Racer Function role-is-reflexive
(role-is-reflexive rolename tbox)
 */

     public String roleIsReflexive(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-reflexive" , rolename, tbox ).toString();
     }

     public RacerResult roleIsReflexive$(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-reflexive" , rolename, tbox );
     }

/** Racer Function role-is-symmetric
(role-is-symmetric rolename tbox)
 */

     public String roleIsSymmetric(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-symmetric" , rolename, tbox ).toString();
     }

     public RacerResult roleIsSymmetric$(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-symmetric" , rolename, tbox );
     }

/** Racer Function role-is-transitive
(role-is-transitive rolename tbox)
 */

     public String roleIsTransitive(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-transitive" , rolename, tbox ).toString();
     }

     public RacerResult roleIsTransitive$(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-transitive" , rolename, tbox );
     }

/** Racer Function role-is-used-as-annotation-property
(role-is-used-as-annotation-property rolename tbox)
 */

     public String roleIsUsedAsAnnotationProperty(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-used-as-annotation-property" , rolename, tbox ).toString();
     }

     public RacerResult roleIsUsedAsAnnotationProperty$(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-used-as-annotation-property" , rolename, tbox );
     }

/** Racer Function role-is-used-as-datatype-property
(role-is-used-as-datatype-property rolename tbox)
 */

     public String roleIsUsedAsDatatypeProperty(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-used-as-datatype-property" , rolename, tbox ).toString();
     }

     public RacerResult roleIsUsedAsDatatypeProperty$(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("role-is-used-as-datatype-property" , rolename, tbox );
     }

/** Racer Function role-p
(role-p role-term &optional tbox)
 */

     public boolean roleP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("role-p" , roleTerm ));
     }

     public boolean roleP(Object roleTerm, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("role-p" , roleTerm, tbox ));
     }

/** Racer Function role-satisfiable-p
(role-satisfiable-p role tbox)
 */

     public boolean roleSatisfiableP(Object role, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("role-satisfiable-p" , role, tbox ));
     }

/** Racer Function role-subsumes-p
(role-subsumes-p role-term-1 role-term-2 tbox)
 */

     public boolean roleSubsumesP(Object roleTerm1, Object roleTerm2, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("role-subsumes-p" , roleTerm1, roleTerm2, tbox ));
     }

/** Racer Function role-used-as-annotation-property-p
(role-used-as-annotation-property-p role-name tbox)
 */

     public boolean roleUsedAsAnnotationPropertyP(Object roleName, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("role-used-as-annotation-property-p" , roleName, tbox ));
     }

/** Racer Function role-used-as-datatype-property-p
(role-used-as-datatype-property-p role-name tbox)
 */

     public boolean roleUsedAsDatatypePropertyP(Object roleName, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("role-used-as-datatype-property-p" , roleName, tbox ));
     }

/** Racer Function roles-disjoint-1
(roles-disjoint-1 role1 role2 tbox)
 */

     public String rolesDisjoint1(Object role1, Object role2, Object tbox ) throws RacerClientException {
          return racerCall("roles-disjoint-1" , role1, role2, tbox ).toString();
     }

     public RacerResult rolesDisjoint1$(Object role1, Object role2, Object tbox ) throws RacerClientException {
          return racerCall("roles-disjoint-1" , role1, role2, tbox );
     }

/** Racer Function roles-equivalent-1
(roles-equivalent-1 role1 role2 tbox)
 */

     public String rolesEquivalent1(Object role1, Object role2, Object tbox ) throws RacerClientException {
          return racerCall("roles-equivalent-1" , role1, role2, tbox ).toString();
     }

     public RacerResult rolesEquivalent1$(Object role1, Object role2, Object tbox ) throws RacerClientException {
          return racerCall("roles-equivalent-1" , role1, role2, tbox );
     }

/** Racer Function rule-accurate-p
(rule-accurate-p query)
 */

     public boolean ruleAccurateP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("rule-accurate-p" , query ));
     }

/** Racer Function rule-active-p
(rule-active-p query)
 */

     public boolean ruleActiveP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("rule-active-p" , query ));
     }

/** Racer Function rule-antecedence
(rule-antecedence query)
 */

     public String ruleAntecedence(Object query ) throws RacerClientException {
          return racerCall("rule-antecedence" , query ).toString();
     }

     public RacerResult ruleAntecedence$(Object query ) throws RacerClientException {
          return racerCall("rule-antecedence" , query );
     }

/** Racer Function rule-applicable-p
(rule-applicable-p query)
 */

     public boolean ruleApplicableP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("rule-applicable-p" , query ));
     }

/** Racer Function rule-consequence
(rule-consequence query)
 */

     public String ruleConsequence(Object query ) throws RacerClientException {
          return racerCall("rule-consequence" , query ).toString();
     }

     public RacerResult ruleConsequence$(Object query ) throws RacerClientException {
          return racerCall("rule-consequence" , query );
     }

/** Racer Function rule-consistent-p
(rule-consistent-p query)
 */

     public boolean ruleConsistentP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("rule-consistent-p" , query ));
     }

/** Racer Function rule-prepared-p
(rule-prepared-p query)
 */

     public boolean rulePreparedP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("rule-prepared-p" , query ));
     }

/** Racer Function rule-processed-p
(rule-processed-p query)
 */

     public boolean ruleProcessedP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("rule-processed-p" , query ));
     }

/** Racer Function rule-ready-p
(rule-ready-p query)
 */

     public boolean ruleReadyP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("rule-ready-p" , query ));
     }

/** Racer Function rule-running-p
(rule-running-p query)
 */

     public boolean ruleRunningP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("rule-running-p" , query ));
     }

/** Racer Function rule-sleeping-p
(rule-sleeping-p query)
 */

     public boolean ruleSleepingP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("rule-sleeping-p" , query ));
     }

/** Racer Function rule-terminated-p
(rule-terminated-p query)
 */

     public boolean ruleTerminatedP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("rule-terminated-p" , query ));
     }

/** Racer Function rule-unapplicable-p
(rule-unapplicable-p query)
 */

     public boolean ruleUnapplicableP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("rule-unapplicable-p" , query ));
     }

/** Racer Function rule-waiting-p
(rule-waiting-p query)
 */

     public boolean ruleWaitingP(Object query ) throws RacerClientException {
          return returnBoolean(racerCall("rule-waiting-p" , query ));
     }

/** Racer Function run-all-queries
(run-all-queries &key
                 abox
                 type-of-substrate
                 dont-add-abox-duplicates-p
                 remove-duplicates-p
                 two-phase-processing-p
                 deliver-phase-two-warning-tokens-p
                 deliver-kb-has-changed-warning-tokens-p
                 add-rule-consequences-p
                 continuation-based-instance-retrieval-p
                 told-information-reasoning-p
                 final-consistency-checking-p
                 runtime-consistency-checking-p
                 verbose-p
                 dont-show-variables
                 dont-show-head-projection-operators-p
                 dont-show-lambdas-p
                 how-many
                 only-new-tuples-p
                 timeout
                 proactive-tuple-computation-p
                 tuple-at-a-time-p
                 use-individual-synonyms-p
                 check-abox-consistency-p
                 ensure-tbox-classification-p
                 initial-abox-mirroring-p
                 initial-role-assertion-mirroring-p
                 classify-concepts-in-instance-assertions-p
                 exclude-permutations-p
                 record-explanations-p)
 */

     public String runAllQueries( ) throws RacerClientException {
          return racerCall("run-all-queries"  ).toString();
     }

     public RacerResult runAllQueries$( ) throws RacerClientException {
          return racerCall("run-all-queries"  );
     }

     public String runAllQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("run-all-queries"  , keyArgs).toString();
     }

     public RacerResult runAllQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("run-all-queries"  , keyArgs);
     }

/** Racer Function run-all-rules
(run-all-rules &key
               abox
               type-of-substrate)
 */

     public String runAllRules( ) throws RacerClientException {
          return racerCall("run-all-rules"  ).toString();
     }

     public RacerResult runAllRules$( ) throws RacerClientException {
          return racerCall("run-all-rules"  );
     }

     public String runAllRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("run-all-rules"  , keyArgs).toString();
     }

     public RacerResult runAllRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("run-all-rules"  , keyArgs);
     }

/** Racer Function running-cheap-queries
(running-cheap-queries &key
                       abox
                       type-of-substrate)
 */

     public String runningCheapQueries( ) throws RacerClientException {
          return racerCall("running-cheap-queries"  ).toString();
     }

     public RacerResult runningCheapQueries$( ) throws RacerClientException {
          return racerCall("running-cheap-queries"  );
     }

     public String runningCheapQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("running-cheap-queries"  , keyArgs).toString();
     }

     public RacerResult runningCheapQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("running-cheap-queries"  , keyArgs);
     }

/** Racer Function running-cheap-rules
(running-cheap-rules &key
                     abox
                     type-of-substrate)
 */

     public String runningCheapRules( ) throws RacerClientException {
          return racerCall("running-cheap-rules"  ).toString();
     }

     public RacerResult runningCheapRules$( ) throws RacerClientException {
          return racerCall("running-cheap-rules"  );
     }

     public String runningCheapRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("running-cheap-rules"  , keyArgs).toString();
     }

     public RacerResult runningCheapRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("running-cheap-rules"  , keyArgs);
     }

/** Racer Function running-expensive-queries
(running-expensive-queries &key
                           abox
                           type-of-substrate)
 */

     public String runningExpensiveQueries( ) throws RacerClientException {
          return racerCall("running-expensive-queries"  ).toString();
     }

     public RacerResult runningExpensiveQueries$( ) throws RacerClientException {
          return racerCall("running-expensive-queries"  );
     }

     public String runningExpensiveQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("running-expensive-queries"  , keyArgs).toString();
     }

     public RacerResult runningExpensiveQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("running-expensive-queries"  , keyArgs);
     }

/** Racer Function running-expensive-rules
(running-expensive-rules &key
                         abox
                         type-of-substrate)
 */

     public String runningExpensiveRules( ) throws RacerClientException {
          return racerCall("running-expensive-rules"  ).toString();
     }

     public RacerResult runningExpensiveRules$( ) throws RacerClientException {
          return racerCall("running-expensive-rules"  );
     }

     public String runningExpensiveRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("running-expensive-rules"  , keyArgs).toString();
     }

     public RacerResult runningExpensiveRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("running-expensive-rules"  , keyArgs);
     }

/** Racer Function running-queries
(running-queries &key
                 abox
                 type-of-substrate)
 */

     public String runningQueries( ) throws RacerClientException {
          return racerCall("running-queries"  ).toString();
     }

     public RacerResult runningQueries$( ) throws RacerClientException {
          return racerCall("running-queries"  );
     }

     public String runningQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("running-queries"  , keyArgs).toString();
     }

     public RacerResult runningQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("running-queries"  , keyArgs);
     }

/** Racer Function running-rules
(running-rules &key
               abox
               type-of-substrate)
 */

     public String runningRules( ) throws RacerClientException {
          return racerCall("running-rules"  ).toString();
     }

     public RacerResult runningRules$( ) throws RacerClientException {
          return racerCall("running-rules"  );
     }

     public String runningRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("running-rules"  , keyArgs).toString();
     }

     public RacerResult runningRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("running-rules"  , keyArgs);
     }

/** Racer Function save-abox
(save-abox pathname-or-stream
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
           ontology-name)
 */

     public String saveAbox(Object pathnameOrStream ) throws RacerClientException {
          return racerCall("save-abox" , pathnameOrStream ).toString();
     }

     public RacerResult saveAbox$(Object pathnameOrStream ) throws RacerClientException {
          return racerCall("save-abox" , pathnameOrStream );
     }

     public String saveAbox(Object pathnameOrStream, Object abox ) throws RacerClientException {
          return racerCall("save-abox" , pathnameOrStream, abox ).toString();
     }

     public RacerResult saveAbox$(Object pathnameOrStream, Object abox ) throws RacerClientException {
          return racerCall("save-abox" , pathnameOrStream, abox );
     }

     public String saveAbox(Object pathnameOrStream, Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("save-abox" , pathnameOrStream, abox , keyArgs).toString();
     }

     public RacerResult saveAbox$(Object pathnameOrStream, Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("save-abox" , pathnameOrStream, abox , keyArgs);
     }

/** Racer Function save-kb
(save-kb pathname-or-stream
         &key
         tbox
         abox
         if-exists
         if-does-not-exist
         uri
         syntax
         ontology-name
         header)
 */

     public String saveKb(Object pathnameOrStream ) throws RacerClientException {
          return racerCall("save-kb" , pathnameOrStream ).toString();
     }

     public RacerResult saveKb$(Object pathnameOrStream ) throws RacerClientException {
          return racerCall("save-kb" , pathnameOrStream );
     }

     public String saveKb(Object pathnameOrStream , Object... keyArgs) throws RacerClientException {
          return racerCall("save-kb" , pathnameOrStream , keyArgs).toString();
     }

     public RacerResult saveKb$(Object pathnameOrStream , Object... keyArgs) throws RacerClientException {
          return racerCall("save-kb" , pathnameOrStream , keyArgs);
     }

/** Racer Function save-ontology-to-triple-store
(save-ontology-to-triple-store &rest (args))
 */

     public String saveOntologyToTripleStore( ) throws RacerClientException {
          return racerCall("save-ontology-to-triple-store"  ).toString();
     }

     public RacerResult saveOntologyToTripleStore$( ) throws RacerClientException {
          return racerCall("save-ontology-to-triple-store"  );
     }

     public String saveOntologyToTripleStore(  Object... keyArgs) throws RacerClientException {
          return racerCall("save-ontology-to-triple-store"  , keyArgs).toString();
     }

     public RacerResult saveOntologyToTripleStore$(  Object... keyArgs) throws RacerClientException {
          return racerCall("save-ontology-to-triple-store"  , keyArgs);
     }

/** Racer Function save-tbox
(save-tbox pathname-or-stream
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
           header)
 */

     public String saveTbox(Object pathnameOrStream ) throws RacerClientException {
          return racerCall("save-tbox" , pathnameOrStream ).toString();
     }

     public RacerResult saveTbox$(Object pathnameOrStream ) throws RacerClientException {
          return racerCall("save-tbox" , pathnameOrStream );
     }

     public String saveTbox(Object pathnameOrStream, Object tbox ) throws RacerClientException {
          return racerCall("save-tbox" , pathnameOrStream, tbox ).toString();
     }

     public RacerResult saveTbox$(Object pathnameOrStream, Object tbox ) throws RacerClientException {
          return racerCall("save-tbox" , pathnameOrStream, tbox );
     }

     public String saveTbox(Object pathnameOrStream, Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("save-tbox" , pathnameOrStream, tbox , keyArgs).toString();
     }

     public RacerResult saveTbox$(Object pathnameOrStream, Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("save-tbox" , pathnameOrStream, tbox , keyArgs);
     }

/** Racer Function server-case
(server-case)
 */

     public String serverCase( ) throws RacerClientException {
          return racerCall("server-case"  ).toString();
     }

     public RacerResult serverCase$( ) throws RacerClientException {
          return racerCall("server-case"  );
     }

/** Racer Function server-function
(server-function name)
 */

     public String serverFunction(Object name ) throws RacerClientException {
          return racerCall("server-function" , name ).toString();
     }

     public RacerResult serverFunction$(Object name ) throws RacerClientException {
          return racerCall("server-function" , name );
     }

/** Racer Function server-value
(server-value name)
 */

     public String serverValue(Object name ) throws RacerClientException {
          return racerCall("server-value" , name ).toString();
     }

     public RacerResult serverValue$(Object name ) throws RacerClientException {
          return racerCall("server-value" , name );
     }

/** Racer Function set-attribute-filler
(set-attribute-filler abox
                      individual
                      value
                      attribute
                      &optional
                      type)
 */

     public String setAttributeFiller(Object abox, Object individual, Object value, Object attribute ) throws RacerClientException {
          return racerCall("set-attribute-filler" , abox, individual, value, attribute ).toString();
     }

     public RacerResult setAttributeFiller$(Object abox, Object individual, Object value, Object attribute ) throws RacerClientException {
          return racerCall("set-attribute-filler" , abox, individual, value, attribute );
     }

     public String setAttributeFiller(Object abox, Object individual, Object value, Object attribute, Object type ) throws RacerClientException {
          return racerCall("set-attribute-filler" , abox, individual, value, attribute, type ).toString();
     }

     public RacerResult setAttributeFiller$(Object abox, Object individual, Object value, Object attribute, Object type ) throws RacerClientException {
          return racerCall("set-attribute-filler" , abox, individual, value, attribute, type );
     }

/** Racer Function set-current-abox
(set-current-abox abox)
 */

     public String setCurrentAbox(Object abox ) throws RacerClientException {
          return racerCall("set-current-abox" , abox ).toString();
     }

     public RacerResult setCurrentAbox$(Object abox ) throws RacerClientException {
          return racerCall("set-current-abox" , abox );
     }

/** Racer Function set-current-tbox
(set-current-tbox tbox)
 */

     public String setCurrentTbox(Object tbox ) throws RacerClientException {
          return racerCall("set-current-tbox" , tbox ).toString();
     }

     public RacerResult setCurrentTbox$(Object tbox ) throws RacerClientException {
          return racerCall("set-current-tbox" , tbox );
     }

/** Racer Function set-data-box
(set-data-box name)
 */

     public String setDataBox(Object name ) throws RacerClientException {
          return racerCall("set-data-box" , name ).toString();
     }

     public RacerResult setDataBox$(Object name ) throws RacerClientException {
          return racerCall("set-data-box" , name );
     }

/** Racer Function set-edge-label-for-non-existent-edges
(set-edge-label-for-non-existent-edges edge-label
                                       &key
                                       abox
                                       type-of-substrate)
 */

     public String setEdgeLabelForNonExistentEdges(Object edgeLabel ) throws RacerClientException {
          return racerCall("set-edge-label-for-non-existent-edges" , edgeLabel ).toString();
     }

     public RacerResult setEdgeLabelForNonExistentEdges$(Object edgeLabel ) throws RacerClientException {
          return racerCall("set-edge-label-for-non-existent-edges" , edgeLabel );
     }

     public String setEdgeLabelForNonExistentEdges(Object edgeLabel , Object... keyArgs) throws RacerClientException {
          return racerCall("set-edge-label-for-non-existent-edges" , edgeLabel , keyArgs).toString();
     }

     public RacerResult setEdgeLabelForNonExistentEdges$(Object edgeLabel , Object... keyArgs) throws RacerClientException {
          return racerCall("set-edge-label-for-non-existent-edges" , edgeLabel , keyArgs);
     }

/** Racer Function set-find-abox
(set-find-abox abox-name1 abox-name2)
 */

     public String setFindAbox(Object aboxName1, Object aboxName2 ) throws RacerClientException {
          return racerCall("set-find-abox" , aboxName1, aboxName2 ).toString();
     }

     public RacerResult setFindAbox$(Object aboxName1, Object aboxName2 ) throws RacerClientException {
          return racerCall("set-find-abox" , aboxName1, aboxName2 );
     }

/** Racer Function set-find-tbox
(set-find-tbox tbox-name1 tbox-name2)
 */

     public String setFindTbox(Object tboxName1, Object tboxName2 ) throws RacerClientException {
          return racerCall("set-find-tbox" , tboxName1, tboxName2 ).toString();
     }

     public RacerResult setFindTbox$(Object tboxName1, Object tboxName2 ) throws RacerClientException {
          return racerCall("set-find-tbox" , tboxName1, tboxName2 );
     }

/** Racer Function set-initial-size-of-process-pool
(set-initial-size-of-process-pool n)
 */

     public String setInitialSizeOfProcessPool(Object n ) throws RacerClientException {
          return racerCall("set-initial-size-of-process-pool" , n ).toString();
     }

     public RacerResult setInitialSizeOfProcessPool$(Object n ) throws RacerClientException {
          return racerCall("set-initial-size-of-process-pool" , n );
     }

/** Racer Function set-max-no-of-tuples-bound
(set-max-no-of-tuples-bound &optional n)
 */

     public String setMaxNoOfTuplesBound( ) throws RacerClientException {
          return racerCall("set-max-no-of-tuples-bound"  ).toString();
     }

     public RacerResult setMaxNoOfTuplesBound$( ) throws RacerClientException {
          return racerCall("set-max-no-of-tuples-bound"  );
     }

     public String setMaxNoOfTuplesBound(Object n ) throws RacerClientException {
          return racerCall("set-max-no-of-tuples-bound" , n ).toString();
     }

     public RacerResult setMaxNoOfTuplesBound$(Object n ) throws RacerClientException {
          return racerCall("set-max-no-of-tuples-bound" , n );
     }

/** Racer Function set-maximum-size-of-process-pool
(set-maximum-size-of-process-pool n)
 */

     public String setMaximumSizeOfProcessPool(Object n ) throws RacerClientException {
          return racerCall("set-maximum-size-of-process-pool" , n ).toString();
     }

     public RacerResult setMaximumSizeOfProcessPool$(Object n ) throws RacerClientException {
          return racerCall("set-maximum-size-of-process-pool" , n );
     }

/** Racer Function set-mirror-data-box
(set-mirror-data-box name)
 */

     public String setMirrorDataBox(Object name ) throws RacerClientException {
          return racerCall("set-mirror-data-box" , name ).toString();
     }

     public RacerResult setMirrorDataBox$(Object name ) throws RacerClientException {
          return racerCall("set-mirror-data-box" , name );
     }

/** Racer Function set-new-ind-counter
(set-new-ind-counter n)
 */

     public String setNewIndCounter(Object n ) throws RacerClientException {
          return racerCall("set-new-ind-counter" , n ).toString();
     }

     public RacerResult setNewIndCounter$(Object n ) throws RacerClientException {
          return racerCall("set-new-ind-counter" , n );
     }

/** Racer Function set-new-ind-prefix
(set-new-ind-prefix prefix)
 */

     public String setNewIndPrefix(Object prefix ) throws RacerClientException {
          return racerCall("set-new-ind-prefix" , prefix ).toString();
     }

     public RacerResult setNewIndPrefix$(Object prefix ) throws RacerClientException {
          return racerCall("set-new-ind-prefix" , prefix );
     }

/** Racer Function set-nrql-mode
(set-nrql-mode mode)
 */

     public String setNrqlMode(Object mode ) throws RacerClientException {
          return racerCall("set-nrql-mode" , mode ).toString();
     }

     public RacerResult setNrqlMode$(Object mode ) throws RacerClientException {
          return racerCall("set-nrql-mode" , mode );
     }

/** Racer Function set-proxy-server
(set-proxy-server proxy)
 */

     public String setProxyServer(Object proxy ) throws RacerClientException {
          return racerCall("set-proxy-server" , proxy ).toString();
     }

     public RacerResult setProxyServer$(Object proxy ) throws RacerClientException {
          return racerCall("set-proxy-server" , proxy );
     }

/** Racer Function set-racer-parameter
(set-racer-parameter name value)
 */

     public String setRacerParameter(Object name, Object value ) throws RacerClientException {
          return racerCall("set-racer-parameter" , name, value ).toString();
     }

     public RacerResult setRacerParameter$(Object name, Object value ) throws RacerClientException {
          return racerCall("set-racer-parameter" , name, value );
     }

/** Racer Function set-rcc-box
(set-rcc-box name
             &optional
             rcc-type
             type)
 */

     public String setRccBox(Object name ) throws RacerClientException {
          return racerCall("set-rcc-box" , name ).toString();
     }

     public RacerResult setRccBox$(Object name ) throws RacerClientException {
          return racerCall("set-rcc-box" , name );
     }

     public String setRccBox(Object name, Object rccType, Object type ) throws RacerClientException {
          return racerCall("set-rcc-box" , name, rccType, type ).toString();
     }

     public RacerResult setRccBox$(Object name, Object rccType, Object type ) throws RacerClientException {
          return racerCall("set-rcc-box" , name, rccType, type );
     }

     public String setRccBox(Object name, Object rccType ) throws RacerClientException {
          return racerCall("set-rcc-box" , name, rccType ).toString();
     }

     public RacerResult setRccBox$(Object name, Object rccType ) throws RacerClientException {
          return racerCall("set-rcc-box" , name, rccType );
     }

/** Racer Function set-rewrite-defined-concepts
(set-rewrite-defined-concepts val)
 */

     public String setRewriteDefinedConcepts(Object val ) throws RacerClientException {
          return racerCall("set-rewrite-defined-concepts" , val ).toString();
     }

     public RacerResult setRewriteDefinedConcepts$(Object val ) throws RacerClientException {
          return racerCall("set-rewrite-defined-concepts" , val );
     }

/** Racer Function set-server-timeout
(set-server-timeout timeout)
 */

     public String setServerTimeout(Object timeout ) throws RacerClientException {
          return racerCall("set-server-timeout" , timeout ).toString();
     }

     public RacerResult setServerTimeout$(Object timeout ) throws RacerClientException {
          return racerCall("set-server-timeout" , timeout );
     }

/** Racer Function set-substrate-type
(set-substrate-type type)
 */

     public String setSubstrateType(Object type ) throws RacerClientException {
          return racerCall("set-substrate-type" , type ).toString();
     }

     public RacerResult setSubstrateType$(Object type ) throws RacerClientException {
          return racerCall("set-substrate-type" , type );
     }

/** Racer Function set-unique-name-assumption
(set-unique-name-assumption value)
 */

     public String setUniqueNameAssumption(Object value ) throws RacerClientException {
          return racerCall("set-unique-name-assumption" , value ).toString();
     }

     public RacerResult setUniqueNameAssumption$(Object value ) throws RacerClientException {
          return racerCall("set-unique-name-assumption" , value );
     }

/** Racer Function show-qbox-for-abox
(show-qbox-for-abox &optional
                    abox
                    definitions-p)
 */

     public String showQboxForAbox( ) throws RacerClientException {
          return racerCall("show-qbox-for-abox"  ).toString();
     }

     public RacerResult showQboxForAbox$( ) throws RacerClientException {
          return racerCall("show-qbox-for-abox"  );
     }

     public String showQboxForAbox(Object abox, Object definitionsP ) throws RacerClientException {
          return racerCall("show-qbox-for-abox" , abox, definitionsP ).toString();
     }

     public RacerResult showQboxForAbox$(Object abox, Object definitionsP ) throws RacerClientException {
          return racerCall("show-qbox-for-abox" , abox, definitionsP );
     }

     public String showQboxForAbox(Object abox ) throws RacerClientException {
          return racerCall("show-qbox-for-abox" , abox ).toString();
     }

     public RacerResult showQboxForAbox$(Object abox ) throws RacerClientException {
          return racerCall("show-qbox-for-abox" , abox );
     }

/** Racer Function sleeping-cheap-queries
(sleeping-cheap-queries &key
                        abox
                        type-of-substrate)
 */

     public String sleepingCheapQueries( ) throws RacerClientException {
          return racerCall("sleeping-cheap-queries"  ).toString();
     }

     public RacerResult sleepingCheapQueries$( ) throws RacerClientException {
          return racerCall("sleeping-cheap-queries"  );
     }

     public String sleepingCheapQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("sleeping-cheap-queries"  , keyArgs).toString();
     }

     public RacerResult sleepingCheapQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("sleeping-cheap-queries"  , keyArgs);
     }

/** Racer Function sleeping-cheap-rules
(sleeping-cheap-rules &key
                      abox
                      type-of-substrate)
 */

     public String sleepingCheapRules( ) throws RacerClientException {
          return racerCall("sleeping-cheap-rules"  ).toString();
     }

     public RacerResult sleepingCheapRules$( ) throws RacerClientException {
          return racerCall("sleeping-cheap-rules"  );
     }

     public String sleepingCheapRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("sleeping-cheap-rules"  , keyArgs).toString();
     }

     public RacerResult sleepingCheapRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("sleeping-cheap-rules"  , keyArgs);
     }

/** Racer Function sleeping-expensive-queries
(sleeping-expensive-queries &key
                            abox
                            type-of-substrate)
 */

     public String sleepingExpensiveQueries( ) throws RacerClientException {
          return racerCall("sleeping-expensive-queries"  ).toString();
     }

     public RacerResult sleepingExpensiveQueries$( ) throws RacerClientException {
          return racerCall("sleeping-expensive-queries"  );
     }

     public String sleepingExpensiveQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("sleeping-expensive-queries"  , keyArgs).toString();
     }

     public RacerResult sleepingExpensiveQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("sleeping-expensive-queries"  , keyArgs);
     }

/** Racer Function sleeping-expensive-rules
(sleeping-expensive-rules &key
                          abox
                          type-of-substrate)
 */

     public String sleepingExpensiveRules( ) throws RacerClientException {
          return racerCall("sleeping-expensive-rules"  ).toString();
     }

     public RacerResult sleepingExpensiveRules$( ) throws RacerClientException {
          return racerCall("sleeping-expensive-rules"  );
     }

     public String sleepingExpensiveRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("sleeping-expensive-rules"  , keyArgs).toString();
     }

     public RacerResult sleepingExpensiveRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("sleeping-expensive-rules"  , keyArgs);
     }

/** Racer Function sleeping-queries
(sleeping-queries &key
                  abox
                  type-of-substrate)
 */

     public String sleepingQueries( ) throws RacerClientException {
          return racerCall("sleeping-queries"  ).toString();
     }

     public RacerResult sleepingQueries$( ) throws RacerClientException {
          return racerCall("sleeping-queries"  );
     }

     public String sleepingQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("sleeping-queries"  , keyArgs).toString();
     }

     public RacerResult sleepingQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("sleeping-queries"  , keyArgs);
     }

/** Racer Function sleeping-rules
(sleeping-rules &key
                abox
                type-of-substrate)
 */

     public String sleepingRules( ) throws RacerClientException {
          return racerCall("sleeping-rules"  ).toString();
     }

     public RacerResult sleepingRules$( ) throws RacerClientException {
          return racerCall("sleeping-rules"  );
     }

     public String sleepingRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("sleeping-rules"  , keyArgs).toString();
     }

     public RacerResult sleepingRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("sleeping-rules"  , keyArgs);
     }

/** Racer Function store-abox-image
(store-abox-image filename &optional abox)
 */

     public String storeAboxImage(Object filename ) throws RacerClientException {
          return racerCall("store-abox-image" , filename ).toString();
     }

     public RacerResult storeAboxImage$(Object filename ) throws RacerClientException {
          return racerCall("store-abox-image" , filename );
     }

     public String storeAboxImage(Object filename, Object abox ) throws RacerClientException {
          return racerCall("store-abox-image" , filename, abox ).toString();
     }

     public RacerResult storeAboxImage$(Object filename, Object abox ) throws RacerClientException {
          return racerCall("store-abox-image" , filename, abox );
     }

/** Racer Function store-aboxes-image
(store-aboxes-image filename aboxes)
 */

     public String storeAboxesImage(Object filename, Object aboxes ) throws RacerClientException {
          return racerCall("store-aboxes-image" , filename, aboxes ).toString();
     }

     public RacerResult storeAboxesImage$(Object filename, Object aboxes ) throws RacerClientException {
          return racerCall("store-aboxes-image" , filename, aboxes );
     }

/** Racer Function store-all-substrates
(store-all-substrates filename)
 */

     public String storeAllSubstrates(Object filename ) throws RacerClientException {
          return racerCall("store-all-substrates" , filename ).toString();
     }

     public RacerResult storeAllSubstrates$(Object filename ) throws RacerClientException {
          return racerCall("store-all-substrates" , filename );
     }

/** Racer Function store-kb-image
(store-kb-image filename &optional kb)
 */

     public String storeKbImage(Object filename ) throws RacerClientException {
          return racerCall("store-kb-image" , filename ).toString();
     }

     public RacerResult storeKbImage$(Object filename ) throws RacerClientException {
          return racerCall("store-kb-image" , filename );
     }

     public String storeKbImage(Object filename, Object kb ) throws RacerClientException {
          return racerCall("store-kb-image" , filename, kb ).toString();
     }

     public RacerResult storeKbImage$(Object filename, Object kb ) throws RacerClientException {
          return racerCall("store-kb-image" , filename, kb );
     }

/** Racer Function store-kbs-image
(store-kbs-image filename kbs)
 */

     public String storeKbsImage(Object filename, Object kbs ) throws RacerClientException {
          return racerCall("store-kbs-image" , filename, kbs ).toString();
     }

     public RacerResult storeKbsImage$(Object filename, Object kbs ) throws RacerClientException {
          return racerCall("store-kbs-image" , filename, kbs );
     }

/** Racer Function store-server-image
(store-server-image filename)
 */

     public String storeServerImage(Object filename ) throws RacerClientException {
          return racerCall("store-server-image" , filename ).toString();
     }

     public RacerResult storeServerImage$(Object filename ) throws RacerClientException {
          return racerCall("store-server-image" , filename );
     }

/** Racer Function store-substrate-for-abox
(store-substrate-for-abox filename
                          &optional
                          for-abox
                          type-of-substrate)
 */

     public String storeSubstrateForAbox(Object filename ) throws RacerClientException {
          return racerCall("store-substrate-for-abox" , filename ).toString();
     }

     public RacerResult storeSubstrateForAbox$(Object filename ) throws RacerClientException {
          return racerCall("store-substrate-for-abox" , filename );
     }

     public String storeSubstrateForAbox(Object filename, Object forAbox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("store-substrate-for-abox" , filename, forAbox, typeOfSubstrate ).toString();
     }

     public RacerResult storeSubstrateForAbox$(Object filename, Object forAbox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("store-substrate-for-abox" , filename, forAbox, typeOfSubstrate );
     }

     public String storeSubstrateForAbox(Object filename, Object forAbox ) throws RacerClientException {
          return racerCall("store-substrate-for-abox" , filename, forAbox ).toString();
     }

     public RacerResult storeSubstrateForAbox$(Object filename, Object forAbox ) throws RacerClientException {
          return racerCall("store-substrate-for-abox" , filename, forAbox );
     }

/** Racer Function store-tbox-image
(store-tbox-image filename &optional tbox)
 */

     public String storeTboxImage(Object filename ) throws RacerClientException {
          return racerCall("store-tbox-image" , filename ).toString();
     }

     public RacerResult storeTboxImage$(Object filename ) throws RacerClientException {
          return racerCall("store-tbox-image" , filename );
     }

     public String storeTboxImage(Object filename, Object tbox ) throws RacerClientException {
          return racerCall("store-tbox-image" , filename, tbox ).toString();
     }

     public RacerResult storeTboxImage$(Object filename, Object tbox ) throws RacerClientException {
          return racerCall("store-tbox-image" , filename, tbox );
     }

/** Racer Function store-tboxes-image
(store-tboxes-image tboxes filename)
 */

     public String storeTboxesImage(Object tboxes, Object filename ) throws RacerClientException {
          return racerCall("store-tboxes-image" , tboxes, filename ).toString();
     }

     public RacerResult storeTboxesImage$(Object tboxes, Object filename ) throws RacerClientException {
          return racerCall("store-tboxes-image" , tboxes, filename );
     }

/** Racer Function subscribe-1
(subscribe-1 subscriber-name
             query-concept
             &optional
             abox
             ip
             port
             simple-protocol-p)
 */

     public String subscribe1(Object subscriberName, Object queryConcept ) throws RacerClientException {
          return racerCall("subscribe-1" , subscriberName, queryConcept ).toString();
     }

     public RacerResult subscribe1$(Object subscriberName, Object queryConcept ) throws RacerClientException {
          return racerCall("subscribe-1" , subscriberName, queryConcept );
     }

     public String subscribe1(Object subscriberName, Object queryConcept, Object abox, Object ip, Object port, Object simpleProtocolP ) throws RacerClientException {
          return racerCall("subscribe-1" , subscriberName, queryConcept, abox, ip, port, simpleProtocolP ).toString();
     }

     public RacerResult subscribe1$(Object subscriberName, Object queryConcept, Object abox, Object ip, Object port, Object simpleProtocolP ) throws RacerClientException {
          return racerCall("subscribe-1" , subscriberName, queryConcept, abox, ip, port, simpleProtocolP );
     }

     public String subscribe1(Object subscriberName, Object queryConcept, Object abox, Object ip, Object port ) throws RacerClientException {
          return racerCall("subscribe-1" , subscriberName, queryConcept, abox, ip, port ).toString();
     }

     public RacerResult subscribe1$(Object subscriberName, Object queryConcept, Object abox, Object ip, Object port ) throws RacerClientException {
          return racerCall("subscribe-1" , subscriberName, queryConcept, abox, ip, port );
     }

     public String subscribe1(Object subscriberName, Object queryConcept, Object abox, Object ip ) throws RacerClientException {
          return racerCall("subscribe-1" , subscriberName, queryConcept, abox, ip ).toString();
     }

     public RacerResult subscribe1$(Object subscriberName, Object queryConcept, Object abox, Object ip ) throws RacerClientException {
          return racerCall("subscribe-1" , subscriberName, queryConcept, abox, ip );
     }

     public String subscribe1(Object subscriberName, Object queryConcept, Object abox ) throws RacerClientException {
          return racerCall("subscribe-1" , subscriberName, queryConcept, abox ).toString();
     }

     public RacerResult subscribe1$(Object subscriberName, Object queryConcept, Object abox ) throws RacerClientException {
          return racerCall("subscribe-1" , subscriberName, queryConcept, abox );
     }

/** Racer Function subscribe-to
(subscribe-to query
              subscriber-name
              &key
              ip
              port
              use-simplified-protocol-p)
 */

     public String subscribeTo(Object query, Object subscriberName ) throws RacerClientException {
          return racerCall("subscribe-to" , query, subscriberName ).toString();
     }

     public RacerResult subscribeTo$(Object query, Object subscriberName ) throws RacerClientException {
          return racerCall("subscribe-to" , query, subscriberName );
     }

     public String subscribeTo(Object query, Object subscriberName , Object... keyArgs) throws RacerClientException {
          return racerCall("subscribe-to" , query, subscriberName , keyArgs).toString();
     }

     public RacerResult subscribeTo$(Object query, Object subscriberName , Object... keyArgs) throws RacerClientException {
          return racerCall("subscribe-to" , query, subscriberName , keyArgs);
     }

/** Racer Function swrl-create-abduction-rules-if-possible
(swrl-create-abduction-rules-if-possible)
 */

     public String swrlCreateAbductionRulesIfPossible( ) throws RacerClientException {
          return racerCall("swrl-create-abduction-rules-if-possible"  ).toString();
     }

     public RacerResult swrlCreateAbductionRulesIfPossible$( ) throws RacerClientException {
          return racerCall("swrl-create-abduction-rules-if-possible"  );
     }

/** Racer Function swrl-create-forward-chainging-rules
(swrl-create-forward-chainging-rules)
 */

     public String swrlCreateForwardChaingingRules( ) throws RacerClientException {
          return racerCall("swrl-create-forward-chainging-rules"  ).toString();
     }

     public RacerResult swrlCreateForwardChaingingRules$( ) throws RacerClientException {
          return racerCall("swrl-create-forward-chainging-rules"  );
     }

/** Racer Function swrl-forward-chaining
(swrl-forward-chaining &key
                       abox
                       verbose
                       delete-rules)
 */

     public String swrlForwardChaining( ) throws RacerClientException {
          return racerCall("swrl-forward-chaining"  ).toString();
     }

     public RacerResult swrlForwardChaining$( ) throws RacerClientException {
          return racerCall("swrl-forward-chaining"  );
     }

     public String swrlForwardChaining(  Object... keyArgs) throws RacerClientException {
          return racerCall("swrl-forward-chaining"  , keyArgs).toString();
     }

     public RacerResult swrlForwardChaining$(  Object... keyArgs) throws RacerClientException {
          return racerCall("swrl-forward-chaining"  , keyArgs);
     }

/** Racer Function symmetric-p
(symmetric-p role-term &optional tbox)
 */

     public boolean symmetricP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("symmetric-p" , roleTerm ));
     }

     public boolean symmetricP(Object roleTerm, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("symmetric-p" , roleTerm, tbox ));
     }

/** Racer Function taxonomy
(taxonomy &optional tbox)
 */

     public String taxonomy( ) throws RacerClientException {
          return racerCall("taxonomy"  ).toString();
     }

     public RacerResult taxonomy$( ) throws RacerClientException {
          return racerCall("taxonomy"  );
     }

     public String taxonomy(Object tbox ) throws RacerClientException {
          return racerCall("taxonomy" , tbox ).toString();
     }

     public RacerResult taxonomy$(Object tbox ) throws RacerClientException {
          return racerCall("taxonomy" , tbox );
     }

/** Racer Function tbox-classified-p
(tbox-classified-p &optional tbox)
 */

     public boolean tboxClassifiedP( ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-classified-p"  ));
     }

     public boolean tboxClassifiedP(Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-classified-p" , tbox ));
     }

/** Racer Function tbox-coherent-p
(tbox-coherent-p &optional tbox)
 */

     public boolean tboxCoherentP( ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-coherent-p"  ));
     }

     public boolean tboxCoherentP(Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-coherent-p" , tbox ));
     }

/** Racer Function tbox-cyclic-p
(tbox-cyclic-p &optional tbox)
 */

     public boolean tboxCyclicP( ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-cyclic-p"  ));
     }

     public boolean tboxCyclicP(Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-cyclic-p" , tbox ));
     }

/** Racer Function tbox-prepared-p
(tbox-prepared-p &optional tbox)
 */

     public boolean tboxPreparedP( ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-prepared-p"  ));
     }

     public boolean tboxPreparedP(Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-prepared-p" , tbox ));
     }

/** Racer Function terminated-queries
(terminated-queries &key
                    abox
                    type-of-substrate)
 */

     public String terminatedQueries( ) throws RacerClientException {
          return racerCall("terminated-queries"  ).toString();
     }

     public RacerResult terminatedQueries$( ) throws RacerClientException {
          return racerCall("terminated-queries"  );
     }

     public String terminatedQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("terminated-queries"  , keyArgs).toString();
     }

     public RacerResult terminatedQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("terminated-queries"  , keyArgs);
     }

/** Racer Function terminated-rules
(terminated-rules &key
                  abox
                  type-of-substrate)
 */

     public String terminatedRules( ) throws RacerClientException {
          return racerCall("terminated-rules"  ).toString();
     }

     public RacerResult terminatedRules$( ) throws RacerClientException {
          return racerCall("terminated-rules"  );
     }

     public String terminatedRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("terminated-rules"  , keyArgs).toString();
     }

     public RacerResult terminatedRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("terminated-rules"  , keyArgs);
     }

/** Racer Function timenet-answer-query
(timenet-answer-query query &key abox)
 */

     public String timenetAnswerQuery(Object query ) throws RacerClientException {
          return racerCall("timenet-answer-query" , query ).toString();
     }

     public RacerResult timenetAnswerQuery$(Object query ) throws RacerClientException {
          return racerCall("timenet-answer-query" , query );
     }

     public String timenetAnswerQuery(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("timenet-answer-query" , query , keyArgs).toString();
     }

     public RacerResult timenetAnswerQuery$(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("timenet-answer-query" , query , keyArgs);
     }

/** Racer Function told-value
(told-value object &optional abox)
 */

     public String toldValue(Object object ) throws RacerClientException {
          return racerCall("told-value" , object ).toString();
     }

     public RacerResult toldValue$(Object object ) throws RacerClientException {
          return racerCall("told-value" , object );
     }

     public String toldValue(Object object, Object abox ) throws RacerClientException {
          return racerCall("told-value" , object, abox ).toString();
     }

     public RacerResult toldValue$(Object object, Object abox ) throws RacerClientException {
          return racerCall("told-value" , object, abox );
     }

/** Racer Function transitive-p
(transitive-p role-term &optional tbox)
 */

     public boolean transitiveP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("transitive-p" , roleTerm ));
     }

     public boolean transitiveP(Object roleTerm, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("transitive-p" , roleTerm, tbox ));
     }

/** Racer Function transmit-file
(transmit-file extension n-bytes)
 */

     public String transmitFile(Object extension, Object nBytes ) throws RacerClientException {
          return racerCall("transmit-file" , extension, nBytes ).toString();
     }

     public RacerResult transmitFile$(Object extension, Object nBytes ) throws RacerClientException {
          return racerCall("transmit-file" , extension, nBytes );
     }

/** Racer Function triple-store-graphs
(triple-store-graphs &key db directory)
 */

     public String tripleStoreGraphs( ) throws RacerClientException {
          return racerCall("triple-store-graphs"  ).toString();
     }

     public RacerResult tripleStoreGraphs$( ) throws RacerClientException {
          return racerCall("triple-store-graphs"  );
     }

     public String tripleStoreGraphs(  Object... keyArgs) throws RacerClientException {
          return racerCall("triple-store-graphs"  , keyArgs).toString();
     }

     public RacerResult tripleStoreGraphs$(  Object... keyArgs) throws RacerClientException {
          return racerCall("triple-store-graphs"  , keyArgs);
     }

/** Racer Function triple-store-open-p
(triple-store-open-p &optional db-name)
 */

     public boolean tripleStoreOpenP( ) throws RacerClientException {
          return returnBoolean(racerCall("triple-store-open-p"  ));
     }

     public boolean tripleStoreOpenP(Object dbName ) throws RacerClientException {
          return returnBoolean(racerCall("triple-store-open-p" , dbName ));
     }

/** Racer Function triple-store-read-file
(triple-store-read-file filename
                        &key
                        db
                        init
                        verbose
                        if-exists
                        index-p
                        graph
                        data-version-level)
 */

     public String tripleStoreReadFile(Object filename ) throws RacerClientException {
          return racerCall("triple-store-read-file" , filename ).toString();
     }

     public RacerResult tripleStoreReadFile$(Object filename ) throws RacerClientException {
          return racerCall("triple-store-read-file" , filename );
     }

     public String tripleStoreReadFile(Object filename , Object... keyArgs) throws RacerClientException {
          return racerCall("triple-store-read-file" , filename , keyArgs).toString();
     }

     public RacerResult tripleStoreReadFile$(Object filename , Object... keyArgs) throws RacerClientException {
          return racerCall("triple-store-read-file" , filename , keyArgs);
     }

/** Racer Function unapplicable-rules
(unapplicable-rules &key
                    abox
                    type-of-substrate)
 */

     public String unapplicableRules( ) throws RacerClientException {
          return racerCall("unapplicable-rules"  ).toString();
     }

     public RacerResult unapplicableRules$( ) throws RacerClientException {
          return racerCall("unapplicable-rules"  );
     }

     public String unapplicableRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("unapplicable-rules"  , keyArgs).toString();
     }

     public RacerResult unapplicableRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("unapplicable-rules"  , keyArgs);
     }

/** Racer Function unbind-all
(unbind-all)
 */

     public String unbindAll( ) throws RacerClientException {
          return racerCall("unbind-all"  ).toString();
     }

     public RacerResult unbindAll$( ) throws RacerClientException {
          return racerCall("unbind-all"  );
     }

/** Racer Function unbind1
(unbind1 name)
 */

     public String unbind1(Object name ) throws RacerClientException {
          return racerCall("unbind1" , name ).toString();
     }

     public RacerResult unbind1$(Object name ) throws RacerClientException {
          return racerCall("unbind1" , name );
     }

/** Racer Function undefine-all
(undefine-all)
 */

     public String undefineAll( ) throws RacerClientException {
          return racerCall("undefine-all"  ).toString();
     }

     public RacerResult undefineAll$( ) throws RacerClientException {
          return racerCall("undefine-all"  );
     }

/** Racer Function undefine-query
(undefine-query name
                &key
                tbox
                arity)
 */

     public String undefineQuery(Object name ) throws RacerClientException {
          return racerCall("undefine-query" , name ).toString();
     }

     public RacerResult undefineQuery$(Object name ) throws RacerClientException {
          return racerCall("undefine-query" , name );
     }

     public String undefineQuery(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("undefine-query" , name , keyArgs).toString();
     }

     public RacerResult undefineQuery$(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("undefine-query" , name , keyArgs);
     }

/** Racer Function undefine1
(undefine1 name)
 */

     public String undefine1(Object name ) throws RacerClientException {
          return racerCall("undefine1" , name ).toString();
     }

     public RacerResult undefine1$(Object name ) throws RacerClientException {
          return racerCall("undefine1" , name );
     }

/** Racer Function unpublish-1
(unpublish-1 individual &optional abox)
 */

     public String unpublish1(Object individual ) throws RacerClientException {
          return racerCall("unpublish-1" , individual ).toString();
     }

     public RacerResult unpublish1$(Object individual ) throws RacerClientException {
          return racerCall("unpublish-1" , individual );
     }

     public String unpublish1(Object individual, Object abox ) throws RacerClientException {
          return racerCall("unpublish-1" , individual, abox ).toString();
     }

     public RacerResult unpublish1$(Object individual, Object abox ) throws RacerClientException {
          return racerCall("unpublish-1" , individual, abox );
     }

/** Racer Function unsubscribe-1
(unsubscribe-1 subscriber-name
               &optional
               query-concept
               abox)
 */

     public String unsubscribe1(Object subscriberName ) throws RacerClientException {
          return racerCall("unsubscribe-1" , subscriberName ).toString();
     }

     public RacerResult unsubscribe1$(Object subscriberName ) throws RacerClientException {
          return racerCall("unsubscribe-1" , subscriberName );
     }

     public String unsubscribe1(Object subscriberName, Object queryConcept, Object abox ) throws RacerClientException {
          return racerCall("unsubscribe-1" , subscriberName, queryConcept, abox ).toString();
     }

     public RacerResult unsubscribe1$(Object subscriberName, Object queryConcept, Object abox ) throws RacerClientException {
          return racerCall("unsubscribe-1" , subscriberName, queryConcept, abox );
     }

     public String unsubscribe1(Object subscriberName, Object queryConcept ) throws RacerClientException {
          return racerCall("unsubscribe-1" , subscriberName, queryConcept ).toString();
     }

     public RacerResult unsubscribe1$(Object subscriberName, Object queryConcept ) throws RacerClientException {
          return racerCall("unsubscribe-1" , subscriberName, queryConcept );
     }

/** Racer Function unsubscribe-from
(unsubscribe-from query
                  subscriber-name
                  &key
                  ip
                  port
                  use-simplified-protocol-p)
 */

     public String unsubscribeFrom(Object query, Object subscriberName ) throws RacerClientException {
          return racerCall("unsubscribe-from" , query, subscriberName ).toString();
     }

     public RacerResult unsubscribeFrom$(Object query, Object subscriberName ) throws RacerClientException {
          return racerCall("unsubscribe-from" , query, subscriberName );
     }

     public String unsubscribeFrom(Object query, Object subscriberName , Object... keyArgs) throws RacerClientException {
          return racerCall("unsubscribe-from" , query, subscriberName , keyArgs).toString();
     }

     public RacerResult unsubscribeFrom$(Object query, Object subscriberName , Object... keyArgs) throws RacerClientException {
          return racerCall("unsubscribe-from" , query, subscriberName , keyArgs);
     }

/** Racer Function update-racer
(update-racer &key
              patchdir
              plugindir
              url)
 */

     public String updateRacer( ) throws RacerClientException {
          return racerCall("update-racer"  ).toString();
     }

     public RacerResult updateRacer$( ) throws RacerClientException {
          return racerCall("update-racer"  );
     }

     public String updateRacer(  Object... keyArgs) throws RacerClientException {
          return racerCall("update-racer"  , keyArgs).toString();
     }

     public RacerResult updateRacer$(  Object... keyArgs) throws RacerClientException {
          return racerCall("update-racer"  , keyArgs);
     }

/** Racer Function use-individual-synonym-equivalence-classes
(use-individual-synonym-equivalence-classes)
 */

     public String useIndividualSynonymEquivalenceClasses( ) throws RacerClientException {
          return racerCall("use-individual-synonym-equivalence-classes"  ).toString();
     }

     public RacerResult useIndividualSynonymEquivalenceClasses$( ) throws RacerClientException {
          return racerCall("use-individual-synonym-equivalence-classes"  );
     }

/** Racer Function use-injective-variables-by-default
(use-injective-variables-by-default)
 */

     public String useInjectiveVariablesByDefault( ) throws RacerClientException {
          return racerCall("use-injective-variables-by-default"  ).toString();
     }

     public RacerResult useInjectiveVariablesByDefault$( ) throws RacerClientException {
          return racerCall("use-injective-variables-by-default"  );
     }

/** Racer Function use-triple-store
(use-triple-store db
                  &key
                  kb-name
                  graph
                  subgraph
                  partition
                  told-only
                  init
                  verbose
                  directory
                  ignore-import)
 */

     public String useTripleStore(Object db ) throws RacerClientException {
          return racerCall("use-triple-store" , db ).toString();
     }

     public RacerResult useTripleStore$(Object db ) throws RacerClientException {
          return racerCall("use-triple-store" , db );
     }

     public String useTripleStore(Object db , Object... keyArgs) throws RacerClientException {
          return racerCall("use-triple-store" , db , keyArgs).toString();
     }

     public RacerResult useTripleStore$(Object db , Object... keyArgs) throws RacerClientException {
          return racerCall("use-triple-store" , db , keyArgs);
     }

/** Racer Function verify-with-abox-individuals-list
(verify-with-abox-individuals-list individuals-list
                                   &optional
                                   abox)
 */

     public String verifyWithAboxIndividualsList(Object individualsList ) throws RacerClientException {
          return racerCall("verify-with-abox-individuals-list" , individualsList ).toString();
     }

     public RacerResult verifyWithAboxIndividualsList$(Object individualsList ) throws RacerClientException {
          return racerCall("verify-with-abox-individuals-list" , individualsList );
     }

     public String verifyWithAboxIndividualsList(Object individualsList, Object abox ) throws RacerClientException {
          return racerCall("verify-with-abox-individuals-list" , individualsList, abox ).toString();
     }

     public RacerResult verifyWithAboxIndividualsList$(Object individualsList, Object abox ) throws RacerClientException {
          return racerCall("verify-with-abox-individuals-list" , individualsList, abox );
     }

/** Racer Function verify-with-concept-tree-list
(verify-with-concept-tree-list tree-list
                               &optional
                               tbox
                               ignore-error)
 */

     public String verifyWithConceptTreeList(Object treeList ) throws RacerClientException {
          return racerCall("verify-with-concept-tree-list" , treeList ).toString();
     }

     public RacerResult verifyWithConceptTreeList$(Object treeList ) throws RacerClientException {
          return racerCall("verify-with-concept-tree-list" , treeList );
     }

     public String verifyWithConceptTreeList(Object treeList, Object tbox, Object ignoreError ) throws RacerClientException {
          return racerCall("verify-with-concept-tree-list" , treeList, tbox, ignoreError ).toString();
     }

     public RacerResult verifyWithConceptTreeList$(Object treeList, Object tbox, Object ignoreError ) throws RacerClientException {
          return racerCall("verify-with-concept-tree-list" , treeList, tbox, ignoreError );
     }

     public String verifyWithConceptTreeList(Object treeList, Object tbox ) throws RacerClientException {
          return racerCall("verify-with-concept-tree-list" , treeList, tbox ).toString();
     }

     public RacerResult verifyWithConceptTreeList$(Object treeList, Object tbox ) throws RacerClientException {
          return racerCall("verify-with-concept-tree-list" , treeList, tbox );
     }

/** Racer Function wait-for-queries-to-terminate
(wait-for-queries-to-terminate)
 */

     public String waitForQueriesToTerminate( ) throws RacerClientException {
          return racerCall("wait-for-queries-to-terminate"  ).toString();
     }

     public RacerResult waitForQueriesToTerminate$( ) throws RacerClientException {
          return racerCall("wait-for-queries-to-terminate"  );
     }

/** Racer Function wait-for-rules-to-terminate
(wait-for-rules-to-terminate)
 */

     public String waitForRulesToTerminate( ) throws RacerClientException {
          return racerCall("wait-for-rules-to-terminate"  ).toString();
     }

     public RacerResult waitForRulesToTerminate$( ) throws RacerClientException {
          return racerCall("wait-for-rules-to-terminate"  );
     }

/** Racer Function waiting-cheap-queries
(waiting-cheap-queries &key
                       abox
                       type-of-substrate)
 */

     public String waitingCheapQueries( ) throws RacerClientException {
          return racerCall("waiting-cheap-queries"  ).toString();
     }

     public RacerResult waitingCheapQueries$( ) throws RacerClientException {
          return racerCall("waiting-cheap-queries"  );
     }

     public String waitingCheapQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("waiting-cheap-queries"  , keyArgs).toString();
     }

     public RacerResult waitingCheapQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("waiting-cheap-queries"  , keyArgs);
     }

/** Racer Function waiting-cheap-rules
(waiting-cheap-rules &key
                     abox
                     type-of-substrate)
 */

     public String waitingCheapRules( ) throws RacerClientException {
          return racerCall("waiting-cheap-rules"  ).toString();
     }

     public RacerResult waitingCheapRules$( ) throws RacerClientException {
          return racerCall("waiting-cheap-rules"  );
     }

     public String waitingCheapRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("waiting-cheap-rules"  , keyArgs).toString();
     }

     public RacerResult waitingCheapRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("waiting-cheap-rules"  , keyArgs);
     }

/** Racer Function waiting-expensive-queries
(waiting-expensive-queries &key
                           abox
                           type-of-substrate)
 */

     public String waitingExpensiveQueries( ) throws RacerClientException {
          return racerCall("waiting-expensive-queries"  ).toString();
     }

     public RacerResult waitingExpensiveQueries$( ) throws RacerClientException {
          return racerCall("waiting-expensive-queries"  );
     }

     public String waitingExpensiveQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("waiting-expensive-queries"  , keyArgs).toString();
     }

     public RacerResult waitingExpensiveQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("waiting-expensive-queries"  , keyArgs);
     }

/** Racer Function waiting-expensive-rules
(waiting-expensive-rules &key
                         abox
                         type-of-substrate)
 */

     public String waitingExpensiveRules( ) throws RacerClientException {
          return racerCall("waiting-expensive-rules"  ).toString();
     }

     public RacerResult waitingExpensiveRules$( ) throws RacerClientException {
          return racerCall("waiting-expensive-rules"  );
     }

     public String waitingExpensiveRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("waiting-expensive-rules"  , keyArgs).toString();
     }

     public RacerResult waitingExpensiveRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("waiting-expensive-rules"  , keyArgs);
     }

/** Racer Function waiting-queries
(waiting-queries &key
                 abox
                 type-of-substrate)
 */

     public String waitingQueries( ) throws RacerClientException {
          return racerCall("waiting-queries"  ).toString();
     }

     public RacerResult waitingQueries$( ) throws RacerClientException {
          return racerCall("waiting-queries"  );
     }

     public String waitingQueries(  Object... keyArgs) throws RacerClientException {
          return racerCall("waiting-queries"  , keyArgs).toString();
     }

     public RacerResult waitingQueries$(  Object... keyArgs) throws RacerClientException {
          return racerCall("waiting-queries"  , keyArgs);
     }

/** Racer Function waiting-rules
(waiting-rules &key
               abox
               type-of-substrate)
 */

     public String waitingRules( ) throws RacerClientException {
          return racerCall("waiting-rules"  ).toString();
     }

     public RacerResult waitingRules$( ) throws RacerClientException {
          return racerCall("waiting-rules"  );
     }

     public String waitingRules(  Object... keyArgs) throws RacerClientException {
          return racerCall("waiting-rules"  , keyArgs).toString();
     }

     public RacerResult waitingRules$(  Object... keyArgs) throws RacerClientException {
          return racerCall("waiting-rules"  , keyArgs);
     }

/** Racer Function xml-read-tbox-file
(xml-read-tbox-file filename)
 */

     public String xmlReadTboxFile(Object filename ) throws RacerClientException {
          return racerCall("xml-read-tbox-file" , filename ).toString();
     }

     public RacerResult xmlReadTboxFile$(Object filename ) throws RacerClientException {
          return racerCall("xml-read-tbox-file" , filename );
     }

/** Racer Macro abox-consistent?
(abox-consistent? &optional abox-name)
 */

     public boolean aboxConsistentMP( ) throws RacerClientException {
          return returnBoolean(racerCall("abox-consistent?"  ));
     }

     public boolean aboxConsistentMP(Object aboxName ) throws RacerClientException {
          return returnBoolean(racerCall("abox-consistent?" , aboxName ));
     }

/** Racer Macro abox-prepared?
(abox-prepared? &optional abox-name)
 */

     public boolean aboxPreparedMP( ) throws RacerClientException {
          return returnBoolean(racerCall("abox-prepared?"  ));
     }

     public boolean aboxPreparedMP(Object aboxName ) throws RacerClientException {
          return returnBoolean(racerCall("abox-prepared?" , aboxName ));
     }

/** Racer Macro abox-realized?
(abox-realized? &optional abox-name)
 */

     public boolean aboxRealizedMP( ) throws RacerClientException {
          return returnBoolean(racerCall("abox-realized?"  ));
     }

     public boolean aboxRealizedMP(Object aboxName ) throws RacerClientException {
          return returnBoolean(racerCall("abox-realized?" , aboxName ));
     }

/** Racer Macro abox-una-consistent?
(abox-una-consistent? &optional abox-name)
 */

     public boolean aboxUnaConsistentMP( ) throws RacerClientException {
          return returnBoolean(racerCall("abox-una-consistent?"  ));
     }

     public boolean aboxUnaConsistentMP(Object aboxName ) throws RacerClientException {
          return returnBoolean(racerCall("abox-una-consistent?" , aboxName ));
     }

/** Racer Macro add-doc-entry
(add-doc-entry)
 */

     public String addDocEntryM( ) throws RacerClientException {
          return racerCall("add-doc-entry"  ).toString();
     }

     public RacerResult addDocEntryM$( ) throws RacerClientException {
          return racerCall("add-doc-entry"  );
     }

/** Racer Macro add-doc-image-data
(add-doc-image-data url type bytes)
 */

     public String addDocImageDataM(Object url, Object type, Object bytes ) throws RacerClientException {
          return racerCall("add-doc-image-data" , url, type, bytes ).toString();
     }

     public RacerResult addDocImageDataM$(Object url, Object type, Object bytes ) throws RacerClientException {
          return racerCall("add-doc-image-data" , url, type, bytes );
     }

/** Racer Macro add-doc-image-data-from-file
(add-doc-image-data-from-file url type pathname)
 */

     public String addDocImageDataFromFileM(Object url, Object type, Object pathname ) throws RacerClientException {
          return racerCall("add-doc-image-data-from-file" , url, type, pathname ).toString();
     }

     public RacerResult addDocImageDataFromFileM$(Object url, Object type, Object pathname ) throws RacerClientException {
          return racerCall("add-doc-image-data-from-file" , url, type, pathname );
     }

/** Racer Macro add-doc-image-file
(add-doc-image-file url type pathname)
 */

     public String addDocImageFileM(Object url, Object type, Object pathname ) throws RacerClientException {
          return racerCall("add-doc-image-file" , url, type, pathname ).toString();
     }

     public RacerResult addDocImageFileM$(Object url, Object type, Object pathname ) throws RacerClientException {
          return racerCall("add-doc-image-file" , url, type, pathname );
     }

/** Racer Macro add-doc-phrase
(add-doc-phrase label string)
 */

     public String addDocPhraseM(Object label, Object string ) throws RacerClientException {
          return racerCall("add-doc-phrase" , label, string ).toString();
     }

     public RacerResult addDocPhraseM$(Object label, Object string ) throws RacerClientException {
          return racerCall("add-doc-phrase" , label, string );
     }

/** Racer Macro all-different
(all-different &rest (individual-name-set))
 */

     public String allDifferentM( ) throws RacerClientException {
          return racerCall("all-different"  ).toString();
     }

     public RacerResult allDifferentM$( ) throws RacerClientException {
          return racerCall("all-different"  );
     }

     public String allDifferentM(  Object... keyArgs) throws RacerClientException {
          return racerCall("all-different"  , keyArgs).toString();
     }

     public RacerResult allDifferentM$(  Object... keyArgs) throws RacerClientException {
          return racerCall("all-different"  , keyArgs);
     }

/** Racer Macro apply-abox-rule
(apply-abox-rule query
                 res-args
                 &key
                 execute-p
                 parser-class
                 rewrite-defined-concepts-p
                 group-by-ops
                 bind-specials-p
                 original-query
                 rule-con-pattern
                 new-ind-ops
                 premise
                 generate-code-p
                 optimize-p
                 rewrite-semantically-p
                 rewrite-to-dnf-p
                 report-inconsistent-queries-p
                 report-tautological-queries-p
                 use-repository-p
                 put-into-repository-p
                 id
                 dont-check-id-p
                 parser
                 result-vois
                 substrate
                 abox
                 create-abox-if-not-found-p
                 package
                 type-of-substrate
                 prepare-now-p)
 */

     public String applyAboxRuleM(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("apply-abox-rule" , query, resArgs ).toString();
     }

     public RacerResult applyAboxRuleM$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("apply-abox-rule" , query, resArgs );
     }

     public String applyAboxRuleM(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("apply-abox-rule" , query, resArgs , keyArgs).toString();
     }

     public RacerResult applyAboxRuleM$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("apply-abox-rule" , query, resArgs , keyArgs);
     }

/** Racer Macro apply-abox-rule-under-premise
(apply-abox-rule-under-premise query
                               res-args
                               &key
                               execute-p
                               parser-class
                               rewrite-defined-concepts-p
                               group-by-ops
                               bind-specials-p
                               original-query
                               rule-con-pattern
                               new-ind-ops
                               premise
                               generate-code-p
                               optimize-p
                               rewrite-semantically-p
                               rewrite-to-dnf-p
                               report-inconsistent-queries-p
                               report-tautological-queries-p
                               use-repository-p
                               put-into-repository-p
                               id
                               dont-check-id-p
                               parser
                               result-vois
                               substrate
                               abox
                               create-abox-if-not-found-p
                               package
                               type-of-substrate
                               prepare-now-p)
 */

     public String applyAboxRuleUnderPremiseM(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("apply-abox-rule-under-premise" , query, resArgs ).toString();
     }

     public RacerResult applyAboxRuleUnderPremiseM$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("apply-abox-rule-under-premise" , query, resArgs );
     }

     public String applyAboxRuleUnderPremiseM(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("apply-abox-rule-under-premise" , query, resArgs , keyArgs).toString();
     }

     public RacerResult applyAboxRuleUnderPremiseM$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("apply-abox-rule-under-premise" , query, resArgs , keyArgs);
     }

/** Racer Macro apply-abox-rule-under-premise1
(apply-abox-rule-under-premise1 res-args
                                query
                                &key
                                execute-p
                                parser-class
                                rewrite-defined-concepts-p
                                group-by-ops
                                bind-specials-p
                                original-query
                                rule-con-pattern
                                new-ind-ops
                                premise
                                generate-code-p
                                optimize-p
                                rewrite-semantically-p
                                rewrite-to-dnf-p
                                report-inconsistent-queries-p
                                report-tautological-queries-p
                                use-repository-p
                                put-into-repository-p
                                id
                                dont-check-id-p
                                parser
                                result-vois
                                substrate
                                abox
                                create-abox-if-not-found-p
                                package
                                type-of-substrate
                                prepare-now-p)
 */

     public String applyAboxRuleUnderPremise1M(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("apply-abox-rule-under-premise1" , resArgs, query ).toString();
     }

     public RacerResult applyAboxRuleUnderPremise1M$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("apply-abox-rule-under-premise1" , resArgs, query );
     }

     public String applyAboxRuleUnderPremise1M(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("apply-abox-rule-under-premise1" , resArgs, query , keyArgs).toString();
     }

     public RacerResult applyAboxRuleUnderPremise1M$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("apply-abox-rule-under-premise1" , resArgs, query , keyArgs);
     }

/** Racer Macro apply-abox-rule1
(apply-abox-rule1 res-args
                  query
                  &key
                  execute-p
                  parser-class
                  rewrite-defined-concepts-p
                  group-by-ops
                  bind-specials-p
                  original-query
                  rule-con-pattern
                  new-ind-ops
                  premise
                  generate-code-p
                  optimize-p
                  rewrite-semantically-p
                  rewrite-to-dnf-p
                  report-inconsistent-queries-p
                  report-tautological-queries-p
                  use-repository-p
                  put-into-repository-p
                  id
                  dont-check-id-p
                  parser
                  result-vois
                  substrate
                  abox
                  create-abox-if-not-found-p
                  package
                  type-of-substrate
                  prepare-now-p)
 */

     public String applyAboxRule1M(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("apply-abox-rule1" , resArgs, query ).toString();
     }

     public RacerResult applyAboxRule1M$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("apply-abox-rule1" , resArgs, query );
     }

     public String applyAboxRule1M(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("apply-abox-rule1" , resArgs, query , keyArgs).toString();
     }

     public RacerResult applyAboxRule1M$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("apply-abox-rule1" , resArgs, query , keyArgs);
     }

/** Racer Macro asymmetric
(asymmetric rolename &optional tbox)
 */

     public String asymmetricM(Object rolename ) throws RacerClientException {
          return racerCall("asymmetric" , rolename ).toString();
     }

     public RacerResult asymmetricM$(Object rolename ) throws RacerClientException {
          return racerCall("asymmetric" , rolename );
     }

     public String asymmetricM(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("asymmetric" , rolename, tbox ).toString();
     }

     public RacerResult asymmetricM$(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("asymmetric" , rolename, tbox );
     }

/** Racer Macro asymmetric?
(asymmetric? role-term &optional tbox-name)
 */

     public boolean asymmetricMP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("asymmetric?" , roleTerm ));
     }

     public boolean asymmetricMP(Object roleTerm, Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("asymmetric?" , roleTerm, tboxName ));
     }

/** Racer Macro attribute-domain
(attribute-domain attribute-name &optional tbox)
 */

     public String attributeDomainM(Object attributeName ) throws RacerClientException {
          return racerCall("attribute-domain" , attributeName ).toString();
     }

     public RacerResult attributeDomainM$(Object attributeName ) throws RacerClientException {
          return racerCall("attribute-domain" , attributeName );
     }

     public String attributeDomainM(Object attributeName, Object tbox ) throws RacerClientException {
          return racerCall("attribute-domain" , attributeName, tbox ).toString();
     }

     public RacerResult attributeDomainM$(Object attributeName, Object tbox ) throws RacerClientException {
          return racerCall("attribute-domain" , attributeName, tbox );
     }

/** Racer Macro attribute-filler
(attribute-filler individual
                  value
                  attribute
                  &optional
                  type)
 */

     public String attributeFillerM(Object individual, Object value, Object attribute ) throws RacerClientException {
          return racerCall("attribute-filler" , individual, value, attribute ).toString();
     }

     public RacerResult attributeFillerM$(Object individual, Object value, Object attribute ) throws RacerClientException {
          return racerCall("attribute-filler" , individual, value, attribute );
     }

     public String attributeFillerM(Object individual, Object value, Object attribute, Object type ) throws RacerClientException {
          return racerCall("attribute-filler" , individual, value, attribute, type ).toString();
     }

     public RacerResult attributeFillerM$(Object individual, Object value, Object attribute, Object type ) throws RacerClientException {
          return racerCall("attribute-filler" , individual, value, attribute, type );
     }

/** Racer Macro cd-attribute?
(cd-attribute? attribute &optional tbox-name)
 */

     public boolean cdAttributeMP(Object attribute ) throws RacerClientException {
          return returnBoolean(racerCall("cd-attribute?" , attribute ));
     }

     public boolean cdAttributeMP(Object attribute, Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("cd-attribute?" , attribute, tboxName ));
     }

/** Racer Macro cd-object?
(cd-object? object-name &optional abox-name)
 */

     public boolean cdObjectMP(Object objectName ) throws RacerClientException {
          return returnBoolean(racerCall("cd-object?" , objectName ));
     }

     public boolean cdObjectMP(Object objectName, Object aboxName ) throws RacerClientException {
          return returnBoolean(racerCall("cd-object?" , objectName, aboxName ));
     }

/** Racer Macro clone-abox
(clone-abox abox &key new-name overwrite)
 */

     public String cloneAboxM(Object abox ) throws RacerClientException {
          return racerCall("clone-abox" , abox ).toString();
     }

     public RacerResult cloneAboxM$(Object abox ) throws RacerClientException {
          return racerCall("clone-abox" , abox );
     }

     public String cloneAboxM(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("clone-abox" , abox , keyArgs).toString();
     }

     public RacerResult cloneAboxM$(Object abox , Object... keyArgs) throws RacerClientException {
          return racerCall("clone-abox" , abox , keyArgs);
     }

/** Racer Macro clone-tbox
(clone-tbox tbox &key new-name overwrite)
 */

     public String cloneTboxM(Object tbox ) throws RacerClientException {
          return racerCall("clone-tbox" , tbox ).toString();
     }

     public RacerResult cloneTboxM$(Object tbox ) throws RacerClientException {
          return racerCall("clone-tbox" , tbox );
     }

     public String cloneTboxM(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("clone-tbox" , tbox , keyArgs).toString();
     }

     public RacerResult cloneTboxM$(Object tbox , Object... keyArgs) throws RacerClientException {
          return racerCall("clone-tbox" , tbox , keyArgs);
     }

/** Racer Macro compute-abox-difference
(compute-abox-difference a
                         b
                         &key
                         also-unmapped-differences-p
                         remove-redundant-diffs-p
                         optimizer-max-plans
                         known-correspondances
                         auto-correspondances-p
                         only-difference-p
                         full-tuples-p
                         show-score-p
                         equi-order-by
                         remove-implied-concept-assertions-p
                         remove-common-assertions-p
                         common-assertions-as-strict-atoms-p
                         map-new-inds-to-new-inds-p
                         cutoff-fn
                         hypo-mode-stack
                         c-mode
                         r-mode
                         only-best-p
                         order-by
                         reverse-order-p
                         ensure-permutations-p
                         how-many
                         strategy
                         simple-result-p
                         runtime-consistency-checking-p
                         final-consistency-checking-p
                         same-as-only-p
                         candidate-individuals
                         binding-validator)
 */

     public String computeAboxDifferenceM(Object a, Object b ) throws RacerClientException {
          return racerCall("compute-abox-difference" , a, b ).toString();
     }

     public RacerResult computeAboxDifferenceM$(Object a, Object b ) throws RacerClientException {
          return racerCall("compute-abox-difference" , a, b );
     }

     public String computeAboxDifferenceM(Object a, Object b , Object... keyArgs) throws RacerClientException {
          return racerCall("compute-abox-difference" , a, b , keyArgs).toString();
     }

     public RacerResult computeAboxDifferenceM$(Object a, Object b , Object... keyArgs) throws RacerClientException {
          return racerCall("compute-abox-difference" , a, b , keyArgs);
     }

/** Racer Macro compute-abox-difference-alternative
(compute-abox-difference-alternative a
                                     b
                                     &key
                                     also-unmapped-differences-p
                                     remove-redundant-diffs-p
                                     optimizer-max-plans
                                     known-correspondances
                                     auto-correspondances-p
                                     only-difference-p
                                     full-tuples-p
                                     show-score-p
                                     equi-order-by
                                     remove-implied-concept-assertions-p
                                     remove-common-assertions-p
                                     common-assertions-as-strict-atoms-p
                                     map-new-inds-to-new-inds-p
                                     cutoff-fn
                                     hypo-mode-stack
                                     c-mode
                                     r-mode
                                     only-best-p
                                     order-by
                                     reverse-order-p
                                     ensure-permutations-p
                                     how-many
                                     strategy
                                     simple-result-p
                                     runtime-consistency-checking-p
                                     final-consistency-checking-p
                                     same-as-only-p
                                     candidate-individuals
                                     binding-validator)
 */

     public String computeAboxDifferenceAlternativeM(Object a, Object b ) throws RacerClientException {
          return racerCall("compute-abox-difference-alternative" , a, b ).toString();
     }

     public RacerResult computeAboxDifferenceAlternativeM$(Object a, Object b ) throws RacerClientException {
          return racerCall("compute-abox-difference-alternative" , a, b );
     }

     public String computeAboxDifferenceAlternativeM(Object a, Object b , Object... keyArgs) throws RacerClientException {
          return racerCall("compute-abox-difference-alternative" , a, b , keyArgs).toString();
     }

     public RacerResult computeAboxDifferenceAlternativeM$(Object a, Object b , Object... keyArgs) throws RacerClientException {
          return racerCall("compute-abox-difference-alternative" , a, b , keyArgs);
     }

/** Racer Macro concept-ancestors
(concept-ancestors concept-term &optional tbox)
 */

     public String conceptAncestorsM(Object conceptTerm ) throws RacerClientException {
          return racerCall("concept-ancestors" , conceptTerm ).toString();
     }

     public RacerResult conceptAncestorsM$(Object conceptTerm ) throws RacerClientException {
          return racerCall("concept-ancestors" , conceptTerm );
     }

     public String conceptAncestorsM(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("concept-ancestors" , conceptTerm, tbox ).toString();
     }

     public RacerResult conceptAncestorsM$(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("concept-ancestors" , conceptTerm, tbox );
     }

/** Racer Macro concept-children
(concept-children concept-term &optional tbox)
 */

     public String conceptChildrenM(Object conceptTerm ) throws RacerClientException {
          return racerCall("concept-children" , conceptTerm ).toString();
     }

     public RacerResult conceptChildrenM$(Object conceptTerm ) throws RacerClientException {
          return racerCall("concept-children" , conceptTerm );
     }

     public String conceptChildrenM(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("concept-children" , conceptTerm, tbox ).toString();
     }

     public RacerResult conceptChildrenM$(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("concept-children" , conceptTerm, tbox );
     }

/** Racer Macro concept-descendants
(concept-descendants concept-term &optional tbox)
 */

     public String conceptDescendantsM(Object conceptTerm ) throws RacerClientException {
          return racerCall("concept-descendants" , conceptTerm ).toString();
     }

     public RacerResult conceptDescendantsM$(Object conceptTerm ) throws RacerClientException {
          return racerCall("concept-descendants" , conceptTerm );
     }

     public String conceptDescendantsM(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("concept-descendants" , conceptTerm, tbox ).toString();
     }

     public RacerResult conceptDescendantsM$(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("concept-descendants" , conceptTerm, tbox );
     }

/** Racer Macro concept-disjoint?
(concept-disjoint? concept-1
                   concept-2
                   &optional
                   tbox-name)
 */

     public boolean conceptDisjointMP(Object concept1, Object concept2 ) throws RacerClientException {
          return returnBoolean(racerCall("concept-disjoint?" , concept1, concept2 ));
     }

     public boolean conceptDisjointMP(Object concept1, Object concept2, Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("concept-disjoint?" , concept1, concept2, tboxName ));
     }

/** Racer Macro concept-equivalent?
(concept-equivalent? concept-1
                     concept-2
                     &optional
                     tbox-name)
 */

     public boolean conceptEquivalentMP(Object concept1, Object concept2 ) throws RacerClientException {
          return returnBoolean(racerCall("concept-equivalent?" , concept1, concept2 ));
     }

     public boolean conceptEquivalentMP(Object concept1, Object concept2, Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("concept-equivalent?" , concept1, concept2, tboxName ));
     }

/** Racer Macro concept-instances
(concept-instances concept-term
                   &optional
                   abox
                   candidates)
 */

     public String conceptInstancesM(Object conceptTerm ) throws RacerClientException {
          return racerCall("concept-instances" , conceptTerm ).toString();
     }

     public RacerResult conceptInstancesM$(Object conceptTerm ) throws RacerClientException {
          return racerCall("concept-instances" , conceptTerm );
     }

     public String conceptInstancesM(Object conceptTerm, Object abox, Object candidates ) throws RacerClientException {
          return racerCall("concept-instances" , conceptTerm, abox, candidates ).toString();
     }

     public RacerResult conceptInstancesM$(Object conceptTerm, Object abox, Object candidates ) throws RacerClientException {
          return racerCall("concept-instances" , conceptTerm, abox, candidates );
     }

     public String conceptInstancesM(Object conceptTerm, Object abox ) throws RacerClientException {
          return racerCall("concept-instances" , conceptTerm, abox ).toString();
     }

     public RacerResult conceptInstancesM$(Object conceptTerm, Object abox ) throws RacerClientException {
          return racerCall("concept-instances" , conceptTerm, abox );
     }

/** Racer Macro concept-is-primitive?
(concept-is-primitive? concept-name &optional tbox)
 */

     public boolean conceptIsPrimitiveMP(Object conceptName ) throws RacerClientException {
          return returnBoolean(racerCall("concept-is-primitive?" , conceptName ));
     }

     public boolean conceptIsPrimitiveMP(Object conceptName, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("concept-is-primitive?" , conceptName, tbox ));
     }

/** Racer Macro concept-parents
(concept-parents concept-term &optional tbox)
 */

     public String conceptParentsM(Object conceptTerm ) throws RacerClientException {
          return racerCall("concept-parents" , conceptTerm ).toString();
     }

     public RacerResult conceptParentsM$(Object conceptTerm ) throws RacerClientException {
          return racerCall("concept-parents" , conceptTerm );
     }

     public String conceptParentsM(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("concept-parents" , conceptTerm, tbox ).toString();
     }

     public RacerResult conceptParentsM$(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("concept-parents" , conceptTerm, tbox );
     }

/** Racer Macro concept-satisfiable?
(concept-satisfiable? concept-1 &optional tbox-name)
 */

     public boolean conceptSatisfiableMP(Object concept1 ) throws RacerClientException {
          return returnBoolean(racerCall("concept-satisfiable?" , concept1 ));
     }

     public boolean conceptSatisfiableMP(Object concept1, Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("concept-satisfiable?" , concept1, tboxName ));
     }

/** Racer Macro concept-subsumes?
(concept-subsumes? concept-1
                   concept-2
                   &optional
                   tbox-name)
 */

     public boolean conceptSubsumesMP(Object concept1, Object concept2 ) throws RacerClientException {
          return returnBoolean(racerCall("concept-subsumes?" , concept1, concept2 ));
     }

     public boolean conceptSubsumesMP(Object concept1, Object concept2, Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("concept-subsumes?" , concept1, concept2, tboxName ));
     }

/** Racer Macro concept-synonyms
(concept-synonyms concept-term &optional tbox)
 */

     public String conceptSynonymsM(Object conceptTerm ) throws RacerClientException {
          return racerCall("concept-synonyms" , conceptTerm ).toString();
     }

     public RacerResult conceptSynonymsM$(Object conceptTerm ) throws RacerClientException {
          return racerCall("concept-synonyms" , conceptTerm );
     }

     public String conceptSynonymsM(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("concept-synonyms" , conceptTerm, tbox ).toString();
     }

     public RacerResult conceptSynonymsM$(Object conceptTerm, Object tbox ) throws RacerClientException {
          return racerCall("concept-synonyms" , conceptTerm, tbox );
     }

/** Racer Macro concept?
(concept? concept-name &optional tbox-name)
 */

     public boolean conceptMP(Object conceptName ) throws RacerClientException {
          return returnBoolean(racerCall("concept?" , conceptName ));
     }

     public boolean conceptMP(Object conceptName, Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("concept?" , conceptName, tboxName ));
     }

/** Racer Macro constrained
(constrained individual object attribute)
 */

     public String constrainedM(Object individual, Object object, Object attribute ) throws RacerClientException {
          return racerCall("constrained" , individual, object, attribute ).toString();
     }

     public RacerResult constrainedM$(Object individual, Object object, Object attribute ) throws RacerClientException {
          return racerCall("constrained" , individual, object, attribute );
     }

/** Racer Macro constraint-entailed?
(constraint-entailed? constraint &optional abox-name)
 */

     public boolean constraintEntailedMP(Object constraint ) throws RacerClientException {
          return returnBoolean(racerCall("constraint-entailed?" , constraint ));
     }

     public boolean constraintEntailedMP(Object constraint, Object aboxName ) throws RacerClientException {
          return returnBoolean(racerCall("constraint-entailed?" , constraint, aboxName ));
     }

/** Racer Macro constraints
(constraints &rest (forms))
 */

     public String constraintsM( ) throws RacerClientException {
          return racerCall("constraints"  ).toString();
     }

     public RacerResult constraintsM$( ) throws RacerClientException {
          return racerCall("constraints"  );
     }

     public String constraintsM(  Object... keyArgs) throws RacerClientException {
          return racerCall("constraints"  , keyArgs).toString();
     }

     public RacerResult constraintsM$(  Object... keyArgs) throws RacerClientException {
          return racerCall("constraints"  , keyArgs);
     }

/** Racer Macro data-edge
(data-edge from
           to
           data-relation
           &optional
           racer-descr
           abox
           type-of-substrate)
 */

     public String dataEdgeM(Object from, Object to, Object dataRelation ) throws RacerClientException {
          return racerCall("data-edge" , from, to, dataRelation ).toString();
     }

     public RacerResult dataEdgeM$(Object from, Object to, Object dataRelation ) throws RacerClientException {
          return racerCall("data-edge" , from, to, dataRelation );
     }

     public String dataEdgeM(Object from, Object to, Object dataRelation, Object racerDescr, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("data-edge" , from, to, dataRelation, racerDescr, abox, typeOfSubstrate ).toString();
     }

     public RacerResult dataEdgeM$(Object from, Object to, Object dataRelation, Object racerDescr, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("data-edge" , from, to, dataRelation, racerDescr, abox, typeOfSubstrate );
     }

     public String dataEdgeM(Object from, Object to, Object dataRelation, Object racerDescr, Object abox ) throws RacerClientException {
          return racerCall("data-edge" , from, to, dataRelation, racerDescr, abox ).toString();
     }

     public RacerResult dataEdgeM$(Object from, Object to, Object dataRelation, Object racerDescr, Object abox ) throws RacerClientException {
          return racerCall("data-edge" , from, to, dataRelation, racerDescr, abox );
     }

     public String dataEdgeM(Object from, Object to, Object dataRelation, Object racerDescr ) throws RacerClientException {
          return racerCall("data-edge" , from, to, dataRelation, racerDescr ).toString();
     }

     public RacerResult dataEdgeM$(Object from, Object to, Object dataRelation, Object racerDescr ) throws RacerClientException {
          return racerCall("data-edge" , from, to, dataRelation, racerDescr );
     }

/** Racer Macro data-node
(data-node name
           &optional
           descr
           racer-descr
           abox
           type-of-substrate)
 */

     public String dataNodeM(Object name ) throws RacerClientException {
          return racerCall("data-node" , name ).toString();
     }

     public RacerResult dataNodeM$(Object name ) throws RacerClientException {
          return racerCall("data-node" , name );
     }

     public String dataNodeM(Object name, Object descr, Object racerDescr, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("data-node" , name, descr, racerDescr, abox, typeOfSubstrate ).toString();
     }

     public RacerResult dataNodeM$(Object name, Object descr, Object racerDescr, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("data-node" , name, descr, racerDescr, abox, typeOfSubstrate );
     }

     public String dataNodeM(Object name, Object descr, Object racerDescr, Object abox ) throws RacerClientException {
          return racerCall("data-node" , name, descr, racerDescr, abox ).toString();
     }

     public RacerResult dataNodeM$(Object name, Object descr, Object racerDescr, Object abox ) throws RacerClientException {
          return racerCall("data-node" , name, descr, racerDescr, abox );
     }

     public String dataNodeM(Object name, Object descr, Object racerDescr ) throws RacerClientException {
          return racerCall("data-node" , name, descr, racerDescr ).toString();
     }

     public RacerResult dataNodeM$(Object name, Object descr, Object racerDescr ) throws RacerClientException {
          return racerCall("data-node" , name, descr, racerDescr );
     }

     public String dataNodeM(Object name, Object descr ) throws RacerClientException {
          return racerCall("data-node" , name, descr ).toString();
     }

     public RacerResult dataNodeM$(Object name, Object descr ) throws RacerClientException {
          return racerCall("data-node" , name, descr );
     }

/** Racer Macro datatype-role-filler
(datatype-role-filler individual
                      value
                      role
                      &optional
                      type)
 */

     public String datatypeRoleFillerM(Object individual, Object value, Object role ) throws RacerClientException {
          return racerCall("datatype-role-filler" , individual, value, role ).toString();
     }

     public RacerResult datatypeRoleFillerM$(Object individual, Object value, Object role ) throws RacerClientException {
          return racerCall("datatype-role-filler" , individual, value, role );
     }

     public String datatypeRoleFillerM(Object individual, Object value, Object role, Object type ) throws RacerClientException {
          return racerCall("datatype-role-filler" , individual, value, role, type ).toString();
     }

     public RacerResult datatypeRoleFillerM$(Object individual, Object value, Object role, Object type ) throws RacerClientException {
          return racerCall("datatype-role-filler" , individual, value, role, type );
     }

/** Racer Macro def-and-exec-query
(def-and-exec-query name
                    head
                    body
                    &key
                    keep-p
                    tbox
                    consider-head-atom-for-consistency-check-p
                    allow-multiple-definitions-p)
 */

     public String defAndExecQueryM(Object name, Object head, Object body ) throws RacerClientException {
          return racerCall("def-and-exec-query" , name, head, body ).toString();
     }

     public RacerResult defAndExecQueryM$(Object name, Object head, Object body ) throws RacerClientException {
          return racerCall("def-and-exec-query" , name, head, body );
     }

     public String defAndExecQueryM(Object name, Object head, Object body , Object... keyArgs) throws RacerClientException {
          return racerCall("def-and-exec-query" , name, head, body , keyArgs).toString();
     }

     public RacerResult defAndExecQueryM$(Object name, Object head, Object body , Object... keyArgs) throws RacerClientException {
          return racerCall("def-and-exec-query" , name, head, body , keyArgs);
     }

/** Racer Macro def-and-prep-query
(def-and-prep-query name
                    head
                    body
                    &key
                    keep-p
                    tbox
                    consider-head-atom-for-consistency-check-p
                    allow-multiple-definitions-p)
 */

     public String defAndPrepQueryM(Object name, Object head, Object body ) throws RacerClientException {
          return racerCall("def-and-prep-query" , name, head, body ).toString();
     }

     public RacerResult defAndPrepQueryM$(Object name, Object head, Object body ) throws RacerClientException {
          return racerCall("def-and-prep-query" , name, head, body );
     }

     public String defAndPrepQueryM(Object name, Object head, Object body , Object... keyArgs) throws RacerClientException {
          return racerCall("def-and-prep-query" , name, head, body , keyArgs).toString();
     }

     public RacerResult defAndPrepQueryM$(Object name, Object head, Object body , Object... keyArgs) throws RacerClientException {
          return racerCall("def-and-prep-query" , name, head, body , keyArgs);
     }

/** Racer Macro defcon
(defcon name value)
 */

     public String defconM(Object name, Object value ) throws RacerClientException {
          return racerCall("defcon" , name, value ).toString();
     }

     public RacerResult defconM$(Object name, Object value ) throws RacerClientException {
          return racerCall("defcon" , name, value );
     }

/** Racer Macro define
(define name arglist)
 */

     public String defineM(Object name, Object arglist ) throws RacerClientException {
          return racerCall("define" , name, arglist ).toString();
     }

     public RacerResult defineM$(Object name, Object arglist ) throws RacerClientException {
          return racerCall("define" , name, arglist );
     }

/** Racer Macro define-abox
(define-abox abox-name &rest (axioms))
 */

     public String defineAboxM(Object aboxName ) throws RacerClientException {
          return racerCall("define-abox" , aboxName ).toString();
     }

     public RacerResult defineAboxM$(Object aboxName ) throws RacerClientException {
          return racerCall("define-abox" , aboxName );
     }

     public String defineAboxM(Object aboxName , Object... keyArgs) throws RacerClientException {
          return racerCall("define-abox" , aboxName , keyArgs).toString();
     }

     public RacerResult defineAboxM$(Object aboxName , Object... keyArgs) throws RacerClientException {
          return racerCall("define-abox" , aboxName , keyArgs);
     }

/** Racer Macro define-concept
(define-concept name definition)
 */

     public String defineConceptM(Object name, Object definition ) throws RacerClientException {
          return racerCall("define-concept" , name, definition ).toString();
     }

     public RacerResult defineConceptM$(Object name, Object definition ) throws RacerClientException {
          return racerCall("define-concept" , name, definition );
     }

/** Racer Macro define-concrete-domain-attribute
(define-concrete-domain-attribute name &key domain type)
 */

     public String defineConcreteDomainAttributeM(Object name ) throws RacerClientException {
          return racerCall("define-concrete-domain-attribute" , name ).toString();
     }

     public RacerResult defineConcreteDomainAttributeM$(Object name ) throws RacerClientException {
          return racerCall("define-concrete-domain-attribute" , name );
     }

     public String defineConcreteDomainAttributeM(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("define-concrete-domain-attribute" , name , keyArgs).toString();
     }

     public RacerResult defineConcreteDomainAttributeM$(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("define-concrete-domain-attribute" , name , keyArgs);
     }

/** Racer Macro define-datatype-property
(define-datatype-property &rest (args))
 */

     public String defineDatatypePropertyM( ) throws RacerClientException {
          return racerCall("define-datatype-property"  ).toString();
     }

     public RacerResult defineDatatypePropertyM$( ) throws RacerClientException {
          return racerCall("define-datatype-property"  );
     }

     public String defineDatatypePropertyM(  Object... keyArgs) throws RacerClientException {
          return racerCall("define-datatype-property"  , keyArgs).toString();
     }

     public RacerResult defineDatatypePropertyM$(  Object... keyArgs) throws RacerClientException {
          return racerCall("define-datatype-property"  , keyArgs);
     }

/** Racer Macro define-disjoint-primitive-concept
(define-disjoint-primitive-concept name
                                   disjoint-list
                                   definition)
 */

     public String defineDisjointPrimitiveConceptM(Object name, Object disjointList, Object definition ) throws RacerClientException {
          return racerCall("define-disjoint-primitive-concept" , name, disjointList, definition ).toString();
     }

     public RacerResult defineDisjointPrimitiveConceptM$(Object name, Object disjointList, Object definition ) throws RacerClientException {
          return racerCall("define-disjoint-primitive-concept" , name, disjointList, definition );
     }

/** Racer Macro define-distinct-individual
(define-distinct-individual individual-name
                            &optional
                            concept)
 */

     public String defineDistinctIndividualM(Object individualName ) throws RacerClientException {
          return racerCall("define-distinct-individual" , individualName ).toString();
     }

     public RacerResult defineDistinctIndividualM$(Object individualName ) throws RacerClientException {
          return racerCall("define-distinct-individual" , individualName );
     }

     public String defineDistinctIndividualM(Object individualName, Object concept ) throws RacerClientException {
          return racerCall("define-distinct-individual" , individualName, concept ).toString();
     }

     public RacerResult defineDistinctIndividualM$(Object individualName, Object concept ) throws RacerClientException {
          return racerCall("define-distinct-individual" , individualName, concept );
     }

/** Racer Macro define-event-assertion
(define-event-assertion assertion)
 */

     public String defineEventAssertionM(Object assertion ) throws RacerClientException {
          return racerCall("define-event-assertion" , assertion ).toString();
     }

     public RacerResult defineEventAssertionM$(Object assertion ) throws RacerClientException {
          return racerCall("define-event-assertion" , assertion );
     }

/** Racer Macro define-event-rule
(define-event-rule head &rest (body))
 */

     public String defineEventRuleM(Object head ) throws RacerClientException {
          return racerCall("define-event-rule" , head ).toString();
     }

     public RacerResult defineEventRuleM$(Object head ) throws RacerClientException {
          return racerCall("define-event-rule" , head );
     }

     public String defineEventRuleM(Object head , Object... keyArgs) throws RacerClientException {
          return racerCall("define-event-rule" , head , keyArgs).toString();
     }

     public RacerResult defineEventRuleM$(Object head , Object... keyArgs) throws RacerClientException {
          return racerCall("define-event-rule" , head , keyArgs);
     }

/** Racer Macro define-individual
(define-individual individual-name &optional concept)
 */

     public String defineIndividualM(Object individualName ) throws RacerClientException {
          return racerCall("define-individual" , individualName ).toString();
     }

     public RacerResult defineIndividualM$(Object individualName ) throws RacerClientException {
          return racerCall("define-individual" , individualName );
     }

     public String defineIndividualM(Object individualName, Object concept ) throws RacerClientException {
          return racerCall("define-individual" , individualName, concept ).toString();
     }

     public RacerResult defineIndividualM$(Object individualName, Object concept ) throws RacerClientException {
          return racerCall("define-individual" , individualName, concept );
     }

/** Racer Macro define-prefix
(define-prefix prefix mapping)
 */

     public String definePrefixM(Object prefix, Object mapping ) throws RacerClientException {
          return racerCall("define-prefix" , prefix, mapping ).toString();
     }

     public RacerResult definePrefixM$(Object prefix, Object mapping ) throws RacerClientException {
          return racerCall("define-prefix" , prefix, mapping );
     }

/** Racer Macro define-primitive-attribute
(define-primitive-attribute name
                            &key
                            parent
                            parents
                            domain
                            range
                            inverse
                            symmetric
                            asymmetric
                            reflexive
                            irreflexive)
 */

     public String definePrimitiveAttributeM(Object name ) throws RacerClientException {
          return racerCall("define-primitive-attribute" , name ).toString();
     }

     public RacerResult definePrimitiveAttributeM$(Object name ) throws RacerClientException {
          return racerCall("define-primitive-attribute" , name );
     }

     public String definePrimitiveAttributeM(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("define-primitive-attribute" , name , keyArgs).toString();
     }

     public RacerResult definePrimitiveAttributeM$(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("define-primitive-attribute" , name , keyArgs);
     }

/** Racer Macro define-primitive-concept
(define-primitive-concept name &optional definition)
 */

     public String definePrimitiveConceptM(Object name ) throws RacerClientException {
          return racerCall("define-primitive-concept" , name ).toString();
     }

     public RacerResult definePrimitiveConceptM$(Object name ) throws RacerClientException {
          return racerCall("define-primitive-concept" , name );
     }

     public String definePrimitiveConceptM(Object name, Object definition ) throws RacerClientException {
          return racerCall("define-primitive-concept" , name, definition ).toString();
     }

     public RacerResult definePrimitiveConceptM$(Object name, Object definition ) throws RacerClientException {
          return racerCall("define-primitive-concept" , name, definition );
     }

/** Racer Macro define-primitive-role
(define-primitive-role name
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
                       compositions)
 */

     public String definePrimitiveRoleM(Object name ) throws RacerClientException {
          return racerCall("define-primitive-role" , name ).toString();
     }

     public RacerResult definePrimitiveRoleM$(Object name ) throws RacerClientException {
          return racerCall("define-primitive-role" , name );
     }

     public String definePrimitiveRoleM(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("define-primitive-role" , name , keyArgs).toString();
     }

     public RacerResult definePrimitiveRoleM$(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("define-primitive-role" , name , keyArgs);
     }

/** Racer Macro define-rule
(define-rule lefthand-side
             righthand-side
             &key
             forward-rule-p
             backward-rule-p)
 */

     public String defineRuleM(Object lefthandSide, Object righthandSide ) throws RacerClientException {
          return racerCall("define-rule" , lefthandSide, righthandSide ).toString();
     }

     public RacerResult defineRuleM$(Object lefthandSide, Object righthandSide ) throws RacerClientException {
          return racerCall("define-rule" , lefthandSide, righthandSide );
     }

     public String defineRuleM(Object lefthandSide, Object righthandSide , Object... keyArgs) throws RacerClientException {
          return racerCall("define-rule" , lefthandSide, righthandSide , keyArgs).toString();
     }

     public RacerResult defineRuleM$(Object lefthandSide, Object righthandSide , Object... keyArgs) throws RacerClientException {
          return racerCall("define-rule" , lefthandSide, righthandSide , keyArgs);
     }

/** Racer Macro define-tbox
(define-tbox name &rest (axioms))
 */

     public String defineTboxM(Object name ) throws RacerClientException {
          return racerCall("define-tbox" , name ).toString();
     }

     public RacerResult defineTboxM$(Object name ) throws RacerClientException {
          return racerCall("define-tbox" , name );
     }

     public String defineTboxM(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("define-tbox" , name , keyArgs).toString();
     }

     public RacerResult defineTboxM$(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("define-tbox" , name , keyArgs);
     }

/** Racer Macro defpar
(defpar name value)
 */

     public String defparM(Object name, Object value ) throws RacerClientException {
          return racerCall("defpar" , name, value ).toString();
     }

     public RacerResult defparM$(Object name, Object value ) throws RacerClientException {
          return racerCall("defpar" , name, value );
     }

/** Racer Macro defquery
(defquery name
          head
          body
          &key
          keep-p
          tbox
          consider-head-atom-for-consistency-check-p
          allow-multiple-definitions-p)
 */

     public String defqueryM(Object name, Object head, Object body ) throws RacerClientException {
          return racerCall("defquery" , name, head, body ).toString();
     }

     public RacerResult defqueryM$(Object name, Object head, Object body ) throws RacerClientException {
          return racerCall("defquery" , name, head, body );
     }

     public String defqueryM(Object name, Object head, Object body , Object... keyArgs) throws RacerClientException {
          return racerCall("defquery" , name, head, body , keyArgs).toString();
     }

     public RacerResult defqueryM$(Object name, Object head, Object body , Object... keyArgs) throws RacerClientException {
          return racerCall("defquery" , name, head, body , keyArgs);
     }

/** Racer Macro del-data-edge
(del-data-edge from
               to
               &optional
               abox
               type-of-substrate)
 */

     public String delDataEdgeM(Object from, Object to ) throws RacerClientException {
          return racerCall("del-data-edge" , from, to ).toString();
     }

     public RacerResult delDataEdgeM$(Object from, Object to ) throws RacerClientException {
          return racerCall("del-data-edge" , from, to );
     }

     public String delDataEdgeM(Object from, Object to, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("del-data-edge" , from, to, abox, typeOfSubstrate ).toString();
     }

     public RacerResult delDataEdgeM$(Object from, Object to, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("del-data-edge" , from, to, abox, typeOfSubstrate );
     }

     public String delDataEdgeM(Object from, Object to, Object abox ) throws RacerClientException {
          return racerCall("del-data-edge" , from, to, abox ).toString();
     }

     public RacerResult delDataEdgeM$(Object from, Object to, Object abox ) throws RacerClientException {
          return racerCall("del-data-edge" , from, to, abox );
     }

/** Racer Macro del-data-node
(del-data-node name
               &optional
               abox
               type-of-substrate)
 */

     public String delDataNodeM(Object name ) throws RacerClientException {
          return racerCall("del-data-node" , name ).toString();
     }

     public RacerResult delDataNodeM$(Object name ) throws RacerClientException {
          return racerCall("del-data-node" , name );
     }

     public String delDataNodeM(Object name, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("del-data-node" , name, abox, typeOfSubstrate ).toString();
     }

     public RacerResult delDataNodeM$(Object name, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("del-data-node" , name, abox, typeOfSubstrate );
     }

     public String delDataNodeM(Object name, Object abox ) throws RacerClientException {
          return racerCall("del-data-node" , name, abox ).toString();
     }

     public RacerResult delDataNodeM$(Object name, Object abox ) throws RacerClientException {
          return racerCall("del-data-node" , name, abox );
     }

/** Racer Macro del-doc-entry
(del-doc-entry label)
 */

     public String delDocEntryM(Object label ) throws RacerClientException {
          return racerCall("del-doc-entry" , label ).toString();
     }

     public RacerResult delDocEntryM$(Object label ) throws RacerClientException {
          return racerCall("del-doc-entry" , label );
     }

/** Racer Macro del-rcc-edge
(del-rcc-edge)
 */

     public String delRccEdgeM( ) throws RacerClientException {
          return racerCall("del-rcc-edge"  ).toString();
     }

     public RacerResult delRccEdgeM$( ) throws RacerClientException {
          return racerCall("del-rcc-edge"  );
     }

/** Racer Macro del-rcc-node
(del-rcc-node)
 */

     public String delRccNodeM( ) throws RacerClientException {
          return racerCall("del-rcc-node"  ).toString();
     }

     public RacerResult delRccNodeM$( ) throws RacerClientException {
          return racerCall("del-rcc-node"  );
     }

/** Racer Macro delete-abox
(delete-abox abox)
 */

     public String deleteAboxM(Object abox ) throws RacerClientException {
          return racerCall("delete-abox" , abox ).toString();
     }

     public RacerResult deleteAboxM$(Object abox ) throws RacerClientException {
          return racerCall("delete-abox" , abox );
     }

/** Racer Macro delete-tbox
(delete-tbox tbox)
 */

     public String deleteTboxM(Object tbox ) throws RacerClientException {
          return racerCall("delete-tbox" , tbox ).toString();
     }

     public RacerResult deleteTboxM$(Object tbox ) throws RacerClientException {
          return racerCall("delete-tbox" , tbox );
     }

/** Racer Macro description-implies?
(description-implies? a b)
 */

     public boolean descriptionImpliesMP(Object a, Object b ) throws RacerClientException {
          return returnBoolean(racerCall("description-implies?" , a, b ));
     }

/** Racer Macro different-from
(different-from individual-name-1 individual-name-2)
 */

     public String differentFromM(Object individualName1, Object individualName2 ) throws RacerClientException {
          return racerCall("different-from" , individualName1, individualName2 ).toString();
     }

     public RacerResult differentFromM$(Object individualName1, Object individualName2 ) throws RacerClientException {
          return racerCall("different-from" , individualName1, individualName2 );
     }

/** Racer Macro direct-predecessors
(direct-predecessors role-term
                     ind-filler
                     &optional
                     abox)
 */

     public String directPredecessorsM(Object roleTerm, Object indFiller ) throws RacerClientException {
          return racerCall("direct-predecessors" , roleTerm, indFiller ).toString();
     }

     public RacerResult directPredecessorsM$(Object roleTerm, Object indFiller ) throws RacerClientException {
          return racerCall("direct-predecessors" , roleTerm, indFiller );
     }

     public String directPredecessorsM(Object roleTerm, Object indFiller, Object abox ) throws RacerClientException {
          return racerCall("direct-predecessors" , roleTerm, indFiller, abox ).toString();
     }

     public RacerResult directPredecessorsM$(Object roleTerm, Object indFiller, Object abox ) throws RacerClientException {
          return racerCall("direct-predecessors" , roleTerm, indFiller, abox );
     }

/** Racer Macro disjoint
(disjoint &rest (concept-names))
 */

     public String disjointM( ) throws RacerClientException {
          return racerCall("disjoint"  ).toString();
     }

     public RacerResult disjointM$( ) throws RacerClientException {
          return racerCall("disjoint"  );
     }

     public String disjointM(  Object... keyArgs) throws RacerClientException {
          return racerCall("disjoint"  , keyArgs).toString();
     }

     public RacerResult disjointM$(  Object... keyArgs) throws RacerClientException {
          return racerCall("disjoint"  , keyArgs);
     }

/** Racer Macro domain
(domain rolename
        concept
        &optional
        tbox
        errorp)
 */

     public String domainM(Object rolename, Object concept ) throws RacerClientException {
          return racerCall("domain" , rolename, concept ).toString();
     }

     public RacerResult domainM$(Object rolename, Object concept ) throws RacerClientException {
          return racerCall("domain" , rolename, concept );
     }

     public String domainM(Object rolename, Object concept, Object tbox, Object errorp ) throws RacerClientException {
          return racerCall("domain" , rolename, concept, tbox, errorp ).toString();
     }

     public RacerResult domainM$(Object rolename, Object concept, Object tbox, Object errorp ) throws RacerClientException {
          return racerCall("domain" , rolename, concept, tbox, errorp );
     }

     public String domainM(Object rolename, Object concept, Object tbox ) throws RacerClientException {
          return racerCall("domain" , rolename, concept, tbox ).toString();
     }

     public RacerResult domainM$(Object rolename, Object concept, Object tbox ) throws RacerClientException {
          return racerCall("domain" , rolename, concept, tbox );
     }

/** Racer Macro edge-description
(edge-description from
                  to
                  &optional
                  abox
                  type-of-substrate)
 */

     public String edgeDescriptionM(Object from, Object to ) throws RacerClientException {
          return racerCall("edge-description" , from, to ).toString();
     }

     public RacerResult edgeDescriptionM$(Object from, Object to ) throws RacerClientException {
          return racerCall("edge-description" , from, to );
     }

     public String edgeDescriptionM(Object from, Object to, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("edge-description" , from, to, abox, typeOfSubstrate ).toString();
     }

     public RacerResult edgeDescriptionM$(Object from, Object to, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("edge-description" , from, to, abox, typeOfSubstrate );
     }

     public String edgeDescriptionM(Object from, Object to, Object abox ) throws RacerClientException {
          return racerCall("edge-description" , from, to, abox ).toString();
     }

     public RacerResult edgeDescriptionM$(Object from, Object to, Object abox ) throws RacerClientException {
          return racerCall("edge-description" , from, to, abox );
     }

/** Racer Macro edge-label
(edge-label from
            to
            &optional
            abox
            type-of-substrate)
 */

     public String edgeLabelM(Object from, Object to ) throws RacerClientException {
          return racerCall("edge-label" , from, to ).toString();
     }

     public RacerResult edgeLabelM$(Object from, Object to ) throws RacerClientException {
          return racerCall("edge-label" , from, to );
     }

     public String edgeLabelM(Object from, Object to, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("edge-label" , from, to, abox, typeOfSubstrate ).toString();
     }

     public RacerResult edgeLabelM$(Object from, Object to, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("edge-label" , from, to, abox, typeOfSubstrate );
     }

     public String edgeLabelM(Object from, Object to, Object abox ) throws RacerClientException {
          return racerCall("edge-label" , from, to, abox ).toString();
     }

     public RacerResult edgeLabelM$(Object from, Object to, Object abox ) throws RacerClientException {
          return racerCall("edge-label" , from, to, abox );
     }

/** Racer Macro equivalent
(equivalent left right)
 */

     public String equivalentM(Object left, Object right ) throws RacerClientException {
          return racerCall("equivalent" , left, right ).toString();
     }

     public RacerResult equivalentM$(Object left, Object right ) throws RacerClientException {
          return racerCall("equivalent" , left, right );
     }

/** Racer Macro feature?
(feature? role-term &optional tbox-name)
 */

     public boolean featureMP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("feature?" , roleTerm ));
     }

     public boolean featureMP(Object roleTerm, Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("feature?" , roleTerm, tboxName ));
     }

/** Racer Macro firerule
(firerule query
          res-args
          &key
          execute-p
          parser-class
          rewrite-defined-concepts-p
          group-by-ops
          bind-specials-p
          original-query
          rule-con-pattern
          new-ind-ops
          premise
          generate-code-p
          optimize-p
          rewrite-semantically-p
          rewrite-to-dnf-p
          report-inconsistent-queries-p
          report-tautological-queries-p
          use-repository-p
          put-into-repository-p
          id
          dont-check-id-p
          parser
          result-vois
          substrate
          abox
          create-abox-if-not-found-p
          package
          type-of-substrate
          prepare-now-p)
 */

     public String fireruleM(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("firerule" , query, resArgs ).toString();
     }

     public RacerResult fireruleM$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("firerule" , query, resArgs );
     }

     public String fireruleM(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("firerule" , query, resArgs , keyArgs).toString();
     }

     public RacerResult fireruleM$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("firerule" , query, resArgs , keyArgs);
     }

/** Racer Macro firerule-under-premise
(firerule-under-premise query
                        res-args
                        &key
                        execute-p
                        parser-class
                        rewrite-defined-concepts-p
                        group-by-ops
                        bind-specials-p
                        original-query
                        rule-con-pattern
                        new-ind-ops
                        premise
                        generate-code-p
                        optimize-p
                        rewrite-semantically-p
                        rewrite-to-dnf-p
                        report-inconsistent-queries-p
                        report-tautological-queries-p
                        use-repository-p
                        put-into-repository-p
                        id
                        dont-check-id-p
                        parser
                        result-vois
                        substrate
                        abox
                        create-abox-if-not-found-p
                        package
                        type-of-substrate
                        prepare-now-p)
 */

     public String fireruleUnderPremiseM(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("firerule-under-premise" , query, resArgs ).toString();
     }

     public RacerResult fireruleUnderPremiseM$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("firerule-under-premise" , query, resArgs );
     }

     public String fireruleUnderPremiseM(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("firerule-under-premise" , query, resArgs , keyArgs).toString();
     }

     public RacerResult fireruleUnderPremiseM$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("firerule-under-premise" , query, resArgs , keyArgs);
     }

/** Racer Macro firerule-under-premise1
(firerule-under-premise1 res-args
                         query
                         &key
                         execute-p
                         parser-class
                         rewrite-defined-concepts-p
                         group-by-ops
                         bind-specials-p
                         original-query
                         rule-con-pattern
                         new-ind-ops
                         premise
                         generate-code-p
                         optimize-p
                         rewrite-semantically-p
                         rewrite-to-dnf-p
                         report-inconsistent-queries-p
                         report-tautological-queries-p
                         use-repository-p
                         put-into-repository-p
                         id
                         dont-check-id-p
                         parser
                         result-vois
                         substrate
                         abox
                         create-abox-if-not-found-p
                         package
                         type-of-substrate
                         prepare-now-p)
 */

     public String fireruleUnderPremise1M(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("firerule-under-premise1" , resArgs, query ).toString();
     }

     public RacerResult fireruleUnderPremise1M$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("firerule-under-premise1" , resArgs, query );
     }

     public String fireruleUnderPremise1M(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("firerule-under-premise1" , resArgs, query , keyArgs).toString();
     }

     public RacerResult fireruleUnderPremise1M$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("firerule-under-premise1" , resArgs, query , keyArgs);
     }

/** Racer Macro firerule1
(firerule1 res-args
           query
           &key
           execute-p
           parser-class
           rewrite-defined-concepts-p
           group-by-ops
           bind-specials-p
           original-query
           rule-con-pattern
           new-ind-ops
           premise
           generate-code-p
           optimize-p
           rewrite-semantically-p
           rewrite-to-dnf-p
           report-inconsistent-queries-p
           report-tautological-queries-p
           use-repository-p
           put-into-repository-p
           id
           dont-check-id-p
           parser
           result-vois
           substrate
           abox
           create-abox-if-not-found-p
           package
           type-of-substrate
           prepare-now-p)
 */

     public String firerule1M(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("firerule1" , resArgs, query ).toString();
     }

     public RacerResult firerule1M$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("firerule1" , resArgs, query );
     }

     public String firerule1M(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("firerule1" , resArgs, query , keyArgs).toString();
     }

     public RacerResult firerule1M$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("firerule1" , resArgs, query , keyArgs);
     }

/** Racer Macro forget
(forget &rest (assertions))
 */

     public String forgetM( ) throws RacerClientException {
          return racerCall("forget"  ).toString();
     }

     public RacerResult forgetM$( ) throws RacerClientException {
          return racerCall("forget"  );
     }

     public String forgetM(  Object... keyArgs) throws RacerClientException {
          return racerCall("forget"  , keyArgs).toString();
     }

     public RacerResult forgetM$(  Object... keyArgs) throws RacerClientException {
          return racerCall("forget"  , keyArgs);
     }

/** Racer Macro functional
(functional rolename &optional tbox)
 */

     public String functionalM(Object rolename ) throws RacerClientException {
          return racerCall("functional" , rolename ).toString();
     }

     public RacerResult functionalM$(Object rolename ) throws RacerClientException {
          return racerCall("functional" , rolename );
     }

     public String functionalM(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("functional" , rolename, tbox ).toString();
     }

     public RacerResult functionalM$(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("functional" , rolename, tbox );
     }

/** Racer Macro get-concept-definition
(get-concept-definition concept-name &optional tbox)
 */

     public String getConceptDefinitionM(Object conceptName ) throws RacerClientException {
          return racerCall("get-concept-definition" , conceptName ).toString();
     }

     public RacerResult getConceptDefinitionM$(Object conceptName ) throws RacerClientException {
          return racerCall("get-concept-definition" , conceptName );
     }

     public String getConceptDefinitionM(Object conceptName, Object tbox ) throws RacerClientException {
          return racerCall("get-concept-definition" , conceptName, tbox ).toString();
     }

     public RacerResult getConceptDefinitionM$(Object conceptName, Object tbox ) throws RacerClientException {
          return racerCall("get-concept-definition" , conceptName, tbox );
     }

/** Racer Macro get-concept-negated-definition
(get-concept-negated-definition concept-name
                                &optional
                                tbox)
 */

     public String getConceptNegatedDefinitionM(Object conceptName ) throws RacerClientException {
          return racerCall("get-concept-negated-definition" , conceptName ).toString();
     }

     public RacerResult getConceptNegatedDefinitionM$(Object conceptName ) throws RacerClientException {
          return racerCall("get-concept-negated-definition" , conceptName );
     }

     public String getConceptNegatedDefinitionM(Object conceptName, Object tbox ) throws RacerClientException {
          return racerCall("get-concept-negated-definition" , conceptName, tbox ).toString();
     }

     public RacerResult getConceptNegatedDefinitionM$(Object conceptName, Object tbox ) throws RacerClientException {
          return racerCall("get-concept-negated-definition" , conceptName, tbox );
     }

/** Racer Macro implies
(implies left right)
 */

     public String impliesM(Object left, Object right ) throws RacerClientException {
          return racerCall("implies" , left, right ).toString();
     }

     public RacerResult impliesM$(Object left, Object right ) throws RacerClientException {
          return racerCall("implies" , left, right );
     }

/** Racer Macro implies-role
(implies-role rolename-1 rolename-2 &optional tbox)
 */

     public String impliesRoleM(Object rolename1, Object rolename2 ) throws RacerClientException {
          return racerCall("implies-role" , rolename1, rolename2 ).toString();
     }

     public RacerResult impliesRoleM$(Object rolename1, Object rolename2 ) throws RacerClientException {
          return racerCall("implies-role" , rolename1, rolename2 );
     }

     public String impliesRoleM(Object rolename1, Object rolename2, Object tbox ) throws RacerClientException {
          return racerCall("implies-role" , rolename1, rolename2, tbox ).toString();
     }

     public RacerResult impliesRoleM$(Object rolename1, Object rolename2, Object tbox ) throws RacerClientException {
          return racerCall("implies-role" , rolename1, rolename2, tbox );
     }

/** Racer Macro in-abox
(in-abox abox-name &optional tbox-name)
 */

     public String inAboxM(Object aboxName ) throws RacerClientException {
          return racerCall("in-abox" , aboxName ).toString();
     }

     public RacerResult inAboxM$(Object aboxName ) throws RacerClientException {
          return racerCall("in-abox" , aboxName );
     }

     public String inAboxM(Object aboxName, Object tboxName ) throws RacerClientException {
          return racerCall("in-abox" , aboxName, tboxName ).toString();
     }

     public RacerResult inAboxM$(Object aboxName, Object tboxName ) throws RacerClientException {
          return racerCall("in-abox" , aboxName, tboxName );
     }

/** Racer Macro in-data-box
(in-data-box name)
 */

     public String inDataBoxM(Object name ) throws RacerClientException {
          return racerCall("in-data-box" , name ).toString();
     }

     public RacerResult inDataBoxM$(Object name ) throws RacerClientException {
          return racerCall("in-data-box" , name );
     }

/** Racer Macro in-knowledge-base
(in-knowledge-base tbox-name &rest (args))
 */

     public String inKnowledgeBaseM(Object tboxName ) throws RacerClientException {
          return racerCall("in-knowledge-base" , tboxName ).toString();
     }

     public RacerResult inKnowledgeBaseM$(Object tboxName ) throws RacerClientException {
          return racerCall("in-knowledge-base" , tboxName );
     }

     public String inKnowledgeBaseM(Object tboxName , Object... keyArgs) throws RacerClientException {
          return racerCall("in-knowledge-base" , tboxName , keyArgs).toString();
     }

     public RacerResult inKnowledgeBaseM$(Object tboxName , Object... keyArgs) throws RacerClientException {
          return racerCall("in-knowledge-base" , tboxName , keyArgs);
     }

/** Racer Macro in-mirror-data-box
(in-mirror-data-box name)
 */

     public String inMirrorDataBoxM(Object name ) throws RacerClientException {
          return racerCall("in-mirror-data-box" , name ).toString();
     }

     public RacerResult inMirrorDataBoxM$(Object name ) throws RacerClientException {
          return racerCall("in-mirror-data-box" , name );
     }

/** Racer Macro in-rcc-box
(in-rcc-box name
            &optional
            rcc-type
            type)
 */

     public String inRccBoxM(Object name ) throws RacerClientException {
          return racerCall("in-rcc-box" , name ).toString();
     }

     public RacerResult inRccBoxM$(Object name ) throws RacerClientException {
          return racerCall("in-rcc-box" , name );
     }

     public String inRccBoxM(Object name, Object rccType, Object type ) throws RacerClientException {
          return racerCall("in-rcc-box" , name, rccType, type ).toString();
     }

     public RacerResult inRccBoxM$(Object name, Object rccType, Object type ) throws RacerClientException {
          return racerCall("in-rcc-box" , name, rccType, type );
     }

     public String inRccBoxM(Object name, Object rccType ) throws RacerClientException {
          return racerCall("in-rcc-box" , name, rccType ).toString();
     }

     public RacerResult inRccBoxM$(Object name, Object rccType ) throws RacerClientException {
          return racerCall("in-rcc-box" , name, rccType );
     }

/** Racer Macro in-tbox
(in-tbox name
         &key
         init
         size
         role-size
         signature)
 */

     public String inTboxM(Object name ) throws RacerClientException {
          return racerCall("in-tbox" , name ).toString();
     }

     public RacerResult inTboxM$(Object name ) throws RacerClientException {
          return racerCall("in-tbox" , name );
     }

     public String inTboxM(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("in-tbox" , name , keyArgs).toString();
     }

     public RacerResult inTboxM$(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("in-tbox" , name , keyArgs);
     }

/** Racer Macro individual-antonyms
(individual-antonyms individual
                     &optional
                     told-only
                     abox-name)
 */

     public String individualAntonymsM(Object individual ) throws RacerClientException {
          return racerCall("individual-antonyms" , individual ).toString();
     }

     public RacerResult individualAntonymsM$(Object individual ) throws RacerClientException {
          return racerCall("individual-antonyms" , individual );
     }

     public String individualAntonymsM(Object individual, Object toldOnly, Object aboxName ) throws RacerClientException {
          return racerCall("individual-antonyms" , individual, toldOnly, aboxName ).toString();
     }

     public RacerResult individualAntonymsM$(Object individual, Object toldOnly, Object aboxName ) throws RacerClientException {
          return racerCall("individual-antonyms" , individual, toldOnly, aboxName );
     }

     public String individualAntonymsM(Object individual, Object toldOnly ) throws RacerClientException {
          return racerCall("individual-antonyms" , individual, toldOnly ).toString();
     }

     public RacerResult individualAntonymsM$(Object individual, Object toldOnly ) throws RacerClientException {
          return racerCall("individual-antonyms" , individual, toldOnly );
     }

/** Racer Macro individual-attribute-fillers
(individual-attribute-fillers ind
                              attribute
                              &optional
                              abox)
 */

     public String individualAttributeFillersM(Object ind, Object attribute ) throws RacerClientException {
          return racerCall("individual-attribute-fillers" , ind, attribute ).toString();
     }

     public RacerResult individualAttributeFillersM$(Object ind, Object attribute ) throws RacerClientException {
          return racerCall("individual-attribute-fillers" , ind, attribute );
     }

     public String individualAttributeFillersM(Object ind, Object attribute, Object abox ) throws RacerClientException {
          return racerCall("individual-attribute-fillers" , ind, attribute, abox ).toString();
     }

     public RacerResult individualAttributeFillersM$(Object ind, Object attribute, Object abox ) throws RacerClientException {
          return racerCall("individual-attribute-fillers" , ind, attribute, abox );
     }

/** Racer Macro individual-direct-types
(individual-direct-types individual-name &optional abox)
 */

     public String individualDirectTypesM(Object individualName ) throws RacerClientException {
          return racerCall("individual-direct-types" , individualName ).toString();
     }

     public RacerResult individualDirectTypesM$(Object individualName ) throws RacerClientException {
          return racerCall("individual-direct-types" , individualName );
     }

     public String individualDirectTypesM(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("individual-direct-types" , individualName, abox ).toString();
     }

     public RacerResult individualDirectTypesM$(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("individual-direct-types" , individualName, abox );
     }

/** Racer Macro individual-filled-roles
(individual-filled-roles ind-predecessor
                         ind-filler
                         &optional
                         abox)
 */

     public String individualFilledRolesM(Object indPredecessor, Object indFiller ) throws RacerClientException {
          return racerCall("individual-filled-roles" , indPredecessor, indFiller ).toString();
     }

     public RacerResult individualFilledRolesM$(Object indPredecessor, Object indFiller ) throws RacerClientException {
          return racerCall("individual-filled-roles" , indPredecessor, indFiller );
     }

     public String individualFilledRolesM(Object indPredecessor, Object indFiller, Object abox ) throws RacerClientException {
          return racerCall("individual-filled-roles" , indPredecessor, indFiller, abox ).toString();
     }

     public RacerResult individualFilledRolesM$(Object indPredecessor, Object indFiller, Object abox ) throws RacerClientException {
          return racerCall("individual-filled-roles" , indPredecessor, indFiller, abox );
     }

/** Racer Macro individual-fillers
(individual-fillers ind-predecessor
                    role-term
                    &optional
                    abox)
 */

     public String individualFillersM(Object indPredecessor, Object roleTerm ) throws RacerClientException {
          return racerCall("individual-fillers" , indPredecessor, roleTerm ).toString();
     }

     public RacerResult individualFillersM$(Object indPredecessor, Object roleTerm ) throws RacerClientException {
          return racerCall("individual-fillers" , indPredecessor, roleTerm );
     }

     public String individualFillersM(Object indPredecessor, Object roleTerm, Object abox ) throws RacerClientException {
          return racerCall("individual-fillers" , indPredecessor, roleTerm, abox ).toString();
     }

     public RacerResult individualFillersM$(Object indPredecessor, Object roleTerm, Object abox ) throws RacerClientException {
          return racerCall("individual-fillers" , indPredecessor, roleTerm, abox );
     }

/** Racer Macro individual-instance?
(individual-instance? individual
                      concept
                      &optional
                      abox)
 */

     public boolean individualInstanceMP(Object individual, Object concept ) throws RacerClientException {
          return returnBoolean(racerCall("individual-instance?" , individual, concept ));
     }

     public boolean individualInstanceMP(Object individual, Object concept, Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("individual-instance?" , individual, concept, abox ));
     }

/** Racer Macro individual-synonyms
(individual-synonyms individual
                     &optional
                     told-only
                     abox-name)
 */

     public String individualSynonymsM(Object individual ) throws RacerClientException {
          return racerCall("individual-synonyms" , individual ).toString();
     }

     public RacerResult individualSynonymsM$(Object individual ) throws RacerClientException {
          return racerCall("individual-synonyms" , individual );
     }

     public String individualSynonymsM(Object individual, Object toldOnly, Object aboxName ) throws RacerClientException {
          return racerCall("individual-synonyms" , individual, toldOnly, aboxName ).toString();
     }

     public RacerResult individualSynonymsM$(Object individual, Object toldOnly, Object aboxName ) throws RacerClientException {
          return racerCall("individual-synonyms" , individual, toldOnly, aboxName );
     }

     public String individualSynonymsM(Object individual, Object toldOnly ) throws RacerClientException {
          return racerCall("individual-synonyms" , individual, toldOnly ).toString();
     }

     public RacerResult individualSynonymsM$(Object individual, Object toldOnly ) throws RacerClientException {
          return racerCall("individual-synonyms" , individual, toldOnly );
     }

/** Racer Macro individual-told-attribute-value
(individual-told-attribute-value ind
                                 attribute
                                 &optional
                                 abox)
 */

     public String individualToldAttributeValueM(Object ind, Object attribute ) throws RacerClientException {
          return racerCall("individual-told-attribute-value" , ind, attribute ).toString();
     }

     public RacerResult individualToldAttributeValueM$(Object ind, Object attribute ) throws RacerClientException {
          return racerCall("individual-told-attribute-value" , ind, attribute );
     }

     public String individualToldAttributeValueM(Object ind, Object attribute, Object abox ) throws RacerClientException {
          return racerCall("individual-told-attribute-value" , ind, attribute, abox ).toString();
     }

     public RacerResult individualToldAttributeValueM$(Object ind, Object attribute, Object abox ) throws RacerClientException {
          return racerCall("individual-told-attribute-value" , ind, attribute, abox );
     }

/** Racer Macro individual-told-datatype-fillers
(individual-told-datatype-fillers ind
                                  datatype-role
                                  &optional
                                  abox)
 */

     public String individualToldDatatypeFillersM(Object ind, Object datatypeRole ) throws RacerClientException {
          return racerCall("individual-told-datatype-fillers" , ind, datatypeRole ).toString();
     }

     public RacerResult individualToldDatatypeFillersM$(Object ind, Object datatypeRole ) throws RacerClientException {
          return racerCall("individual-told-datatype-fillers" , ind, datatypeRole );
     }

     public String individualToldDatatypeFillersM(Object ind, Object datatypeRole, Object abox ) throws RacerClientException {
          return racerCall("individual-told-datatype-fillers" , ind, datatypeRole, abox ).toString();
     }

     public RacerResult individualToldDatatypeFillersM$(Object ind, Object datatypeRole, Object abox ) throws RacerClientException {
          return racerCall("individual-told-datatype-fillers" , ind, datatypeRole, abox );
     }

/** Racer Macro individual-types
(individual-types individual-name &optional abox)
 */

     public String individualTypesM(Object individualName ) throws RacerClientException {
          return racerCall("individual-types" , individualName ).toString();
     }

     public RacerResult individualTypesM$(Object individualName ) throws RacerClientException {
          return racerCall("individual-types" , individualName );
     }

     public String individualTypesM(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("individual-types" , individualName, abox ).toString();
     }

     public RacerResult individualTypesM$(Object individualName, Object abox ) throws RacerClientException {
          return racerCall("individual-types" , individualName, abox );
     }

/** Racer Macro individual?
(individual? individual-name &optional abox-name)
 */

     public boolean individualMP(Object individualName ) throws RacerClientException {
          return returnBoolean(racerCall("individual?" , individualName ));
     }

     public boolean individualMP(Object individualName, Object aboxName ) throws RacerClientException {
          return returnBoolean(racerCall("individual?" , individualName, aboxName ));
     }

/** Racer Macro individuals-equal?
(individuals-equal? individual-1
                    individual-2
                    &optional
                    abox)
 */

     public boolean individualsEqualMP(Object individual1, Object individual2 ) throws RacerClientException {
          return returnBoolean(racerCall("individuals-equal?" , individual1, individual2 ));
     }

     public boolean individualsEqualMP(Object individual1, Object individual2, Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("individuals-equal?" , individual1, individual2, abox ));
     }

/** Racer Macro individuals-not-equal?
(individuals-not-equal? individual-1
                        individual-2
                        &optional
                        abox)
 */

     public boolean individualsNotEqualMP(Object individual1, Object individual2 ) throws RacerClientException {
          return returnBoolean(racerCall("individuals-not-equal?" , individual1, individual2 ));
     }

     public boolean individualsNotEqualMP(Object individual1, Object individual2, Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("individuals-not-equal?" , individual1, individual2, abox ));
     }

/** Racer Macro individuals-related?
(individuals-related? individual-1
                      individual-2
                      role-term
                      &optional
                      abox)
 */

     public boolean individualsRelatedMP(Object individual1, Object individual2, Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("individuals-related?" , individual1, individual2, roleTerm ));
     }

     public boolean individualsRelatedMP(Object individual1, Object individual2, Object roleTerm, Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("individuals-related?" , individual1, individual2, roleTerm, abox ));
     }

/** Racer Macro init-publications
(init-publications &optional abox)
 */

     public String initPublicationsM( ) throws RacerClientException {
          return racerCall("init-publications"  ).toString();
     }

     public RacerResult initPublicationsM$( ) throws RacerClientException {
          return racerCall("init-publications"  );
     }

     public String initPublicationsM(Object abox ) throws RacerClientException {
          return racerCall("init-publications" , abox ).toString();
     }

     public RacerResult initPublicationsM$(Object abox ) throws RacerClientException {
          return racerCall("init-publications" , abox );
     }

/** Racer Macro init-subscriptions
(init-subscriptions &optional abox)
 */

     public String initSubscriptionsM( ) throws RacerClientException {
          return racerCall("init-subscriptions"  ).toString();
     }

     public RacerResult initSubscriptionsM$( ) throws RacerClientException {
          return racerCall("init-subscriptions"  );
     }

     public String initSubscriptionsM(Object abox ) throws RacerClientException {
          return racerCall("init-subscriptions" , abox ).toString();
     }

     public RacerResult initSubscriptionsM$(Object abox ) throws RacerClientException {
          return racerCall("init-subscriptions" , abox );
     }

/** Racer Macro instance
(instance name concept)
 */

     public String instanceM(Object name, Object concept ) throws RacerClientException {
          return racerCall("instance" , name, concept ).toString();
     }

     public RacerResult instanceM$(Object name, Object concept ) throws RacerClientException {
          return racerCall("instance" , name, concept );
     }

/** Racer Macro inverse
(inverse rolename inverse-role &optional tbox)
 */

     public String inverseM(Object rolename, Object inverseRole ) throws RacerClientException {
          return racerCall("inverse" , rolename, inverseRole ).toString();
     }

     public RacerResult inverseM$(Object rolename, Object inverseRole ) throws RacerClientException {
          return racerCall("inverse" , rolename, inverseRole );
     }

     public String inverseM(Object rolename, Object inverseRole, Object tbox ) throws RacerClientException {
          return racerCall("inverse" , rolename, inverseRole, tbox ).toString();
     }

     public RacerResult inverseM$(Object rolename, Object inverseRole, Object tbox ) throws RacerClientException {
          return racerCall("inverse" , rolename, inverseRole, tbox );
     }

/** Racer Macro irreflexive
(irreflexive rolename &optional tbox)
 */

     public String irreflexiveM(Object rolename ) throws RacerClientException {
          return racerCall("irreflexive" , rolename ).toString();
     }

     public RacerResult irreflexiveM$(Object rolename ) throws RacerClientException {
          return racerCall("irreflexive" , rolename );
     }

     public String irreflexiveM(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("irreflexive" , rolename, tbox ).toString();
     }

     public RacerResult irreflexiveM$(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("irreflexive" , rolename, tbox );
     }

/** Racer Macro irreflexive?
(irreflexive? role-term &optional tbox-name)
 */

     public boolean irreflexiveMP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("irreflexive?" , roleTerm ));
     }

     public boolean irreflexiveMP(Object roleTerm, Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("irreflexive?" , roleTerm, tboxName ));
     }

/** Racer Macro node-description
(node-description name
                  &optional
                  abox
                  type-of-substrate)
 */

     public String nodeDescriptionM(Object name ) throws RacerClientException {
          return racerCall("node-description" , name ).toString();
     }

     public RacerResult nodeDescriptionM$(Object name ) throws RacerClientException {
          return racerCall("node-description" , name );
     }

     public String nodeDescriptionM(Object name, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("node-description" , name, abox, typeOfSubstrate ).toString();
     }

     public RacerResult nodeDescriptionM$(Object name, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("node-description" , name, abox, typeOfSubstrate );
     }

     public String nodeDescriptionM(Object name, Object abox ) throws RacerClientException {
          return racerCall("node-description" , name, abox ).toString();
     }

     public RacerResult nodeDescriptionM$(Object name, Object abox ) throws RacerClientException {
          return racerCall("node-description" , name, abox );
     }

/** Racer Macro node-label
(node-label name
            &optional
            abox
            type-of-substrate)
 */

     public String nodeLabelM(Object name ) throws RacerClientException {
          return racerCall("node-label" , name ).toString();
     }

     public RacerResult nodeLabelM$(Object name ) throws RacerClientException {
          return racerCall("node-label" , name );
     }

     public String nodeLabelM(Object name, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("node-label" , name, abox, typeOfSubstrate ).toString();
     }

     public RacerResult nodeLabelM$(Object name, Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return racerCall("node-label" , name, abox, typeOfSubstrate );
     }

     public String nodeLabelM(Object name, Object abox ) throws RacerClientException {
          return racerCall("node-label" , name, abox ).toString();
     }

     public RacerResult nodeLabelM$(Object name, Object abox ) throws RacerClientException {
          return racerCall("node-label" , name, abox );
     }

/** Racer Macro prepare-abox-query
(prepare-abox-query res-args
                    query
                    &key
                    execute-p
                    dont-add-abox-duplicates-p
                    remove-duplicates-p
                    two-phase-processing-p
                    deliver-phase-two-warning-tokens-p
                    deliver-kb-has-changed-warning-tokens-p
                    add-rule-consequences-p
                    continuation-based-instance-retrieval-p
                    told-information-reasoning-p
                    final-consistency-checking-p
                    runtime-consistency-checking-p
                    verbose-p
                    dont-show-variables
                    dont-show-head-projection-operators-p
                    dont-show-lambdas-p
                    how-many
                    only-new-tuples-p
                    timeout
                    proactive-tuple-computation-p
                    tuple-at-a-time-p
                    use-individual-synonyms-p
                    check-abox-consistency-p
                    ensure-tbox-classification-p
                    initial-abox-mirroring-p
                    initial-role-assertion-mirroring-p
                    classify-concepts-in-instance-assertions-p
                    exclude-permutations-p
                    record-explanations-p
                    parser-class
                    rewrite-defined-concepts-p
                    group-by-ops
                    bind-specials-p
                    original-query
                    rule-con-pattern
                    new-ind-ops
                    premise
                    generate-code-p
                    optimize-p
                    rewrite-semantically-p
                    rewrite-to-dnf-p
                    report-inconsistent-queries-p
                    report-tautological-queries-p
                    use-repository-p
                    put-into-repository-p
                    id
                    dont-check-id-p
                    parser
                    result-vois
                    substrate
                    abox
                    create-abox-if-not-found-p
                    package
                    type-of-substrate
                    prepare-now-p)
 */

     public String prepareAboxQueryM(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("prepare-abox-query" , resArgs, query ).toString();
     }

     public RacerResult prepareAboxQueryM$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("prepare-abox-query" , resArgs, query );
     }

     public String prepareAboxQueryM(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-abox-query" , resArgs, query , keyArgs).toString();
     }

     public RacerResult prepareAboxQueryM$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-abox-query" , resArgs, query , keyArgs);
     }

/** Racer Macro prepare-abox-query1
(prepare-abox-query1 query
                     res-args
                     &key
                     execute-p
                     dont-add-abox-duplicates-p
                     remove-duplicates-p
                     two-phase-processing-p
                     deliver-phase-two-warning-tokens-p
                     deliver-kb-has-changed-warning-tokens-p
                     add-rule-consequences-p
                     continuation-based-instance-retrieval-p
                     told-information-reasoning-p
                     final-consistency-checking-p
                     runtime-consistency-checking-p
                     verbose-p
                     dont-show-variables
                     dont-show-head-projection-operators-p
                     dont-show-lambdas-p
                     how-many
                     only-new-tuples-p
                     timeout
                     proactive-tuple-computation-p
                     tuple-at-a-time-p
                     use-individual-synonyms-p
                     check-abox-consistency-p
                     ensure-tbox-classification-p
                     initial-abox-mirroring-p
                     initial-role-assertion-mirroring-p
                     classify-concepts-in-instance-assertions-p
                     exclude-permutations-p
                     record-explanations-p
                     parser-class
                     rewrite-defined-concepts-p
                     group-by-ops
                     bind-specials-p
                     original-query
                     rule-con-pattern
                     new-ind-ops
                     premise
                     generate-code-p
                     optimize-p
                     rewrite-semantically-p
                     rewrite-to-dnf-p
                     report-inconsistent-queries-p
                     report-tautological-queries-p
                     use-repository-p
                     put-into-repository-p
                     id
                     dont-check-id-p
                     parser
                     result-vois
                     substrate
                     abox
                     create-abox-if-not-found-p
                     package
                     type-of-substrate
                     prepare-now-p)
 */

     public String prepareAboxQuery1M(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("prepare-abox-query1" , query, resArgs ).toString();
     }

     public RacerResult prepareAboxQuery1M$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("prepare-abox-query1" , query, resArgs );
     }

     public String prepareAboxQuery1M(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-abox-query1" , query, resArgs , keyArgs).toString();
     }

     public RacerResult prepareAboxQuery1M$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-abox-query1" , query, resArgs , keyArgs);
     }

/** Racer Macro prepare-abox-rule
(prepare-abox-rule query
                   res-args
                   &key
                   execute-p
                   parser-class
                   rewrite-defined-concepts-p
                   group-by-ops
                   bind-specials-p
                   original-query
                   rule-con-pattern
                   new-ind-ops
                   premise
                   generate-code-p
                   optimize-p
                   rewrite-semantically-p
                   rewrite-to-dnf-p
                   report-inconsistent-queries-p
                   report-tautological-queries-p
                   use-repository-p
                   put-into-repository-p
                   id
                   dont-check-id-p
                   parser
                   result-vois
                   substrate
                   abox
                   create-abox-if-not-found-p
                   package
                   type-of-substrate
                   prepare-now-p)
 */

     public String prepareAboxRuleM(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("prepare-abox-rule" , query, resArgs ).toString();
     }

     public RacerResult prepareAboxRuleM$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("prepare-abox-rule" , query, resArgs );
     }

     public String prepareAboxRuleM(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-abox-rule" , query, resArgs , keyArgs).toString();
     }

     public RacerResult prepareAboxRuleM$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-abox-rule" , query, resArgs , keyArgs);
     }

/** Racer Macro prepare-abox-rule1
(prepare-abox-rule1 res-args
                    query
                    &key
                    execute-p
                    parser-class
                    rewrite-defined-concepts-p
                    group-by-ops
                    bind-specials-p
                    original-query
                    rule-con-pattern
                    new-ind-ops
                    premise
                    generate-code-p
                    optimize-p
                    rewrite-semantically-p
                    rewrite-to-dnf-p
                    report-inconsistent-queries-p
                    report-tautological-queries-p
                    use-repository-p
                    put-into-repository-p
                    id
                    dont-check-id-p
                    parser
                    result-vois
                    substrate
                    abox
                    create-abox-if-not-found-p
                    package
                    type-of-substrate
                    prepare-now-p)
 */

     public String prepareAboxRule1M(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("prepare-abox-rule1" , resArgs, query ).toString();
     }

     public RacerResult prepareAboxRule1M$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("prepare-abox-rule1" , resArgs, query );
     }

     public String prepareAboxRule1M(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-abox-rule1" , resArgs, query , keyArgs).toString();
     }

     public RacerResult prepareAboxRule1M$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-abox-rule1" , resArgs, query , keyArgs);
     }

/** Racer Macro prepare-tbox-query
(prepare-tbox-query res-args
                    query
                    &key
                    execute-p
                    dont-add-abox-duplicates-p
                    remove-duplicates-p
                    two-phase-processing-p
                    deliver-phase-two-warning-tokens-p
                    deliver-kb-has-changed-warning-tokens-p
                    add-rule-consequences-p
                    continuation-based-instance-retrieval-p
                    told-information-reasoning-p
                    final-consistency-checking-p
                    runtime-consistency-checking-p
                    verbose-p
                    dont-show-variables
                    dont-show-head-projection-operators-p
                    dont-show-lambdas-p
                    how-many
                    only-new-tuples-p
                    timeout
                    proactive-tuple-computation-p
                    tuple-at-a-time-p
                    use-individual-synonyms-p
                    check-abox-consistency-p
                    ensure-tbox-classification-p
                    initial-abox-mirroring-p
                    initial-role-assertion-mirroring-p
                    classify-concepts-in-instance-assertions-p
                    exclude-permutations-p
                    record-explanations-p
                    parser-class
                    rewrite-defined-concepts-p
                    group-by-ops
                    bind-specials-p
                    original-query
                    rule-con-pattern
                    new-ind-ops
                    premise
                    generate-code-p
                    optimize-p
                    rewrite-semantically-p
                    rewrite-to-dnf-p
                    report-inconsistent-queries-p
                    report-tautological-queries-p
                    use-repository-p
                    put-into-repository-p
                    id
                    dont-check-id-p
                    parser
                    result-vois
                    tbox
                    package
                    create-tbox-if-not-found-p
                    substrate)
 */

     public String prepareTboxQueryM(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("prepare-tbox-query" , resArgs, query ).toString();
     }

     public RacerResult prepareTboxQueryM$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("prepare-tbox-query" , resArgs, query );
     }

     public String prepareTboxQueryM(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-tbox-query" , resArgs, query , keyArgs).toString();
     }

     public RacerResult prepareTboxQueryM$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-tbox-query" , resArgs, query , keyArgs);
     }

/** Racer Macro prepare-tbox-query1
(prepare-tbox-query1 query
                     res-args
                     &key
                     execute-p
                     dont-add-abox-duplicates-p
                     remove-duplicates-p
                     two-phase-processing-p
                     deliver-phase-two-warning-tokens-p
                     deliver-kb-has-changed-warning-tokens-p
                     add-rule-consequences-p
                     continuation-based-instance-retrieval-p
                     told-information-reasoning-p
                     final-consistency-checking-p
                     runtime-consistency-checking-p
                     verbose-p
                     dont-show-variables
                     dont-show-head-projection-operators-p
                     dont-show-lambdas-p
                     how-many
                     only-new-tuples-p
                     timeout
                     proactive-tuple-computation-p
                     tuple-at-a-time-p
                     use-individual-synonyms-p
                     check-abox-consistency-p
                     ensure-tbox-classification-p
                     initial-abox-mirroring-p
                     initial-role-assertion-mirroring-p
                     classify-concepts-in-instance-assertions-p
                     exclude-permutations-p
                     record-explanations-p
                     parser-class
                     rewrite-defined-concepts-p
                     group-by-ops
                     bind-specials-p
                     original-query
                     rule-con-pattern
                     new-ind-ops
                     premise
                     generate-code-p
                     optimize-p
                     rewrite-semantically-p
                     rewrite-to-dnf-p
                     report-inconsistent-queries-p
                     report-tautological-queries-p
                     use-repository-p
                     put-into-repository-p
                     id
                     dont-check-id-p
                     parser
                     result-vois
                     tbox
                     package
                     create-tbox-if-not-found-p
                     substrate)
 */

     public String prepareTboxQuery1M(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("prepare-tbox-query1" , query, resArgs ).toString();
     }

     public RacerResult prepareTboxQuery1M$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("prepare-tbox-query1" , query, resArgs );
     }

     public String prepareTboxQuery1M(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-tbox-query1" , query, resArgs , keyArgs).toString();
     }

     public RacerResult prepareTboxQuery1M$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("prepare-tbox-query1" , query, resArgs , keyArgs);
     }

/** Racer Macro preprule
(preprule query
          res-args
          &key
          execute-p
          parser-class
          rewrite-defined-concepts-p
          group-by-ops
          bind-specials-p
          original-query
          rule-con-pattern
          new-ind-ops
          premise
          generate-code-p
          optimize-p
          rewrite-semantically-p
          rewrite-to-dnf-p
          report-inconsistent-queries-p
          report-tautological-queries-p
          use-repository-p
          put-into-repository-p
          id
          dont-check-id-p
          parser
          result-vois
          substrate
          abox
          create-abox-if-not-found-p
          package
          type-of-substrate
          prepare-now-p)
 */

     public String prepruleM(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("preprule" , query, resArgs ).toString();
     }

     public RacerResult prepruleM$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("preprule" , query, resArgs );
     }

     public String prepruleM(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("preprule" , query, resArgs , keyArgs).toString();
     }

     public RacerResult prepruleM$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("preprule" , query, resArgs , keyArgs);
     }

/** Racer Macro preprule1
(preprule1 res-args
           query
           &key
           execute-p
           parser-class
           rewrite-defined-concepts-p
           group-by-ops
           bind-specials-p
           original-query
           rule-con-pattern
           new-ind-ops
           premise
           generate-code-p
           optimize-p
           rewrite-semantically-p
           rewrite-to-dnf-p
           report-inconsistent-queries-p
           report-tautological-queries-p
           use-repository-p
           put-into-repository-p
           id
           dont-check-id-p
           parser
           result-vois
           substrate
           abox
           create-abox-if-not-found-p
           package
           type-of-substrate
           prepare-now-p)
 */

     public String preprule1M(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("preprule1" , resArgs, query ).toString();
     }

     public RacerResult preprule1M$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("preprule1" , resArgs, query );
     }

     public String preprule1M(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("preprule1" , resArgs, query , keyArgs).toString();
     }

     public RacerResult preprule1M$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("preprule1" , resArgs, query , keyArgs);
     }

/** Racer Macro pretrieve
(pretrieve head body &rest (args))
 */

     public String pretrieveM(Object head, Object body ) throws RacerClientException {
          return racerCall("pretrieve" , head, body ).toString();
     }

     public RacerResult pretrieveM$(Object head, Object body ) throws RacerClientException {
          return racerCall("pretrieve" , head, body );
     }

     public String pretrieveM(Object head, Object body , Object... keyArgs) throws RacerClientException {
          return racerCall("pretrieve" , head, body , keyArgs).toString();
     }

     public RacerResult pretrieveM$(Object head, Object body , Object... keyArgs) throws RacerClientException {
          return racerCall("pretrieve" , head, body , keyArgs);
     }

/** Racer Macro publish
(publish individual &optional abox)
 */

     public String publishM(Object individual ) throws RacerClientException {
          return racerCall("publish" , individual ).toString();
     }

     public RacerResult publishM$(Object individual ) throws RacerClientException {
          return racerCall("publish" , individual );
     }

     public String publishM(Object individual, Object abox ) throws RacerClientException {
          return racerCall("publish" , individual, abox ).toString();
     }

     public RacerResult publishM$(Object individual, Object abox ) throws RacerClientException {
          return racerCall("publish" , individual, abox );
     }

/** Racer Macro range
(range rolename
       concept
       &optional
       tbox
       errorp)
 */

     public String rangeM(Object rolename, Object concept ) throws RacerClientException {
          return racerCall("range" , rolename, concept ).toString();
     }

     public RacerResult rangeM$(Object rolename, Object concept ) throws RacerClientException {
          return racerCall("range" , rolename, concept );
     }

     public String rangeM(Object rolename, Object concept, Object tbox, Object errorp ) throws RacerClientException {
          return racerCall("range" , rolename, concept, tbox, errorp ).toString();
     }

     public RacerResult rangeM$(Object rolename, Object concept, Object tbox, Object errorp ) throws RacerClientException {
          return racerCall("range" , rolename, concept, tbox, errorp );
     }

     public String rangeM(Object rolename, Object concept, Object tbox ) throws RacerClientException {
          return racerCall("range" , rolename, concept, tbox ).toString();
     }

     public RacerResult rangeM$(Object rolename, Object concept, Object tbox ) throws RacerClientException {
          return racerCall("range" , rolename, concept, tbox );
     }

/** Racer Macro rcc-consistent?
(rcc-consistent? &optional
                 abox
                 type-of-substrate)
 */

     public boolean rccConsistentMP( ) throws RacerClientException {
          return returnBoolean(racerCall("rcc-consistent?"  ));
     }

     public boolean rccConsistentMP(Object abox, Object typeOfSubstrate ) throws RacerClientException {
          return returnBoolean(racerCall("rcc-consistent?" , abox, typeOfSubstrate ));
     }

     public boolean rccConsistentMP(Object abox ) throws RacerClientException {
          return returnBoolean(racerCall("rcc-consistent?" , abox ));
     }

/** Racer Macro rcc-edge
(rcc-edge)
 */

     public String rccEdgeM( ) throws RacerClientException {
          return racerCall("rcc-edge"  ).toString();
     }

     public RacerResult rccEdgeM$( ) throws RacerClientException {
          return racerCall("rcc-edge"  );
     }

/** Racer Macro rcc-edge-description
(rcc-edge-description)
 */

     public String rccEdgeDescriptionM( ) throws RacerClientException {
          return racerCall("rcc-edge-description"  ).toString();
     }

     public RacerResult rccEdgeDescriptionM$( ) throws RacerClientException {
          return racerCall("rcc-edge-description"  );
     }

/** Racer Macro rcc-edge-label
(rcc-edge-label)
 */

     public String rccEdgeLabelM( ) throws RacerClientException {
          return racerCall("rcc-edge-label"  ).toString();
     }

     public RacerResult rccEdgeLabelM$( ) throws RacerClientException {
          return racerCall("rcc-edge-label"  );
     }

/** Racer Macro rcc-instance
(rcc-instance)
 */

     public String rccInstanceM( ) throws RacerClientException {
          return racerCall("rcc-instance"  ).toString();
     }

     public RacerResult rccInstanceM$( ) throws RacerClientException {
          return racerCall("rcc-instance"  );
     }

/** Racer Macro rcc-node
(rcc-node)
 */

     public String rccNodeM( ) throws RacerClientException {
          return racerCall("rcc-node"  ).toString();
     }

     public RacerResult rccNodeM$( ) throws RacerClientException {
          return racerCall("rcc-node"  );
     }

/** Racer Macro rcc-node-description
(rcc-node-description)
 */

     public String rccNodeDescriptionM( ) throws RacerClientException {
          return racerCall("rcc-node-description"  ).toString();
     }

     public RacerResult rccNodeDescriptionM$( ) throws RacerClientException {
          return racerCall("rcc-node-description"  );
     }

/** Racer Macro rcc-node-label
(rcc-node-label)
 */

     public String rccNodeLabelM( ) throws RacerClientException {
          return racerCall("rcc-node-label"  ).toString();
     }

     public RacerResult rccNodeLabelM$( ) throws RacerClientException {
          return racerCall("rcc-node-label"  );
     }

/** Racer Macro rcc-related
(rcc-related)
 */

     public String rccRelatedM( ) throws RacerClientException {
          return racerCall("rcc-related"  ).toString();
     }

     public RacerResult rccRelatedM$( ) throws RacerClientException {
          return racerCall("rcc-related"  );
     }

/** Racer Macro rcc-synonym
(rcc-synonym role rcc-relation)
 */

     public String rccSynonymM(Object role, Object rccRelation ) throws RacerClientException {
          return racerCall("rcc-synonym" , role, rccRelation ).toString();
     }

     public RacerResult rccSynonymM$(Object role, Object rccRelation ) throws RacerClientException {
          return racerCall("rcc-synonym" , role, rccRelation );
     }

/** Racer Macro reflexive
(reflexive rolename &optional tbox)
 */

     public String reflexiveM(Object rolename ) throws RacerClientException {
          return racerCall("reflexive" , rolename ).toString();
     }

     public RacerResult reflexiveM$(Object rolename ) throws RacerClientException {
          return racerCall("reflexive" , rolename );
     }

     public String reflexiveM(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("reflexive" , rolename, tbox ).toString();
     }

     public RacerResult reflexiveM$(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("reflexive" , rolename, tbox );
     }

/** Racer Macro reflexive?
(reflexive? role-term &optional tbox-name)
 */

     public boolean reflexiveMP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("reflexive?" , roleTerm ));
     }

     public boolean reflexiveMP(Object roleTerm, Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("reflexive?" , roleTerm, tboxName ));
     }

/** Racer Macro related
(related left-name right-name role-name)
 */

     public String relatedM(Object leftName, Object rightName, Object roleName ) throws RacerClientException {
          return racerCall("related" , leftName, rightName, roleName ).toString();
     }

     public RacerResult relatedM$(Object leftName, Object rightName, Object roleName ) throws RacerClientException {
          return racerCall("related" , leftName, rightName, roleName );
     }

/** Racer Macro related-individuals
(related-individuals role-term &optional abox-name)
 */

     public String relatedIndividualsM(Object roleTerm ) throws RacerClientException {
          return racerCall("related-individuals" , roleTerm ).toString();
     }

     public RacerResult relatedIndividualsM$(Object roleTerm ) throws RacerClientException {
          return racerCall("related-individuals" , roleTerm );
     }

     public String relatedIndividualsM(Object roleTerm, Object aboxName ) throws RacerClientException {
          return racerCall("related-individuals" , roleTerm, aboxName ).toString();
     }

     public RacerResult relatedIndividualsM$(Object roleTerm, Object aboxName ) throws RacerClientException {
          return racerCall("related-individuals" , roleTerm, aboxName );
     }

/** Racer Macro retrieve
(retrieve res-args
          query
          &key
          execute-p
          dont-add-abox-duplicates-p
          remove-duplicates-p
          two-phase-processing-p
          deliver-phase-two-warning-tokens-p
          deliver-kb-has-changed-warning-tokens-p
          add-rule-consequences-p
          continuation-based-instance-retrieval-p
          told-information-reasoning-p
          final-consistency-checking-p
          runtime-consistency-checking-p
          verbose-p
          dont-show-variables
          dont-show-head-projection-operators-p
          dont-show-lambdas-p
          how-many
          only-new-tuples-p
          timeout
          proactive-tuple-computation-p
          tuple-at-a-time-p
          use-individual-synonyms-p
          check-abox-consistency-p
          ensure-tbox-classification-p
          initial-abox-mirroring-p
          initial-role-assertion-mirroring-p
          classify-concepts-in-instance-assertions-p
          exclude-permutations-p
          record-explanations-p
          parser-class
          rewrite-defined-concepts-p
          group-by-ops
          bind-specials-p
          original-query
          rule-con-pattern
          new-ind-ops
          premise
          generate-code-p
          optimize-p
          rewrite-semantically-p
          rewrite-to-dnf-p
          report-inconsistent-queries-p
          report-tautological-queries-p
          use-repository-p
          put-into-repository-p
          id
          dont-check-id-p
          parser
          result-vois
          substrate
          abox
          create-abox-if-not-found-p
          package
          type-of-substrate
          prepare-now-p)
 */

     public String retrieveM(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("retrieve" , resArgs, query ).toString();
     }

     public RacerResult retrieveM$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("retrieve" , resArgs, query );
     }

     public String retrieveM(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("retrieve" , resArgs, query , keyArgs).toString();
     }

     public RacerResult retrieveM$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("retrieve" , resArgs, query , keyArgs);
     }

/** Racer Macro retrieve-under-premise
(retrieve-under-premise res-args
                        query
                        &key
                        execute-p
                        dont-add-abox-duplicates-p
                        remove-duplicates-p
                        two-phase-processing-p
                        deliver-phase-two-warning-tokens-p
                        deliver-kb-has-changed-warning-tokens-p
                        add-rule-consequences-p
                        continuation-based-instance-retrieval-p
                        told-information-reasoning-p
                        final-consistency-checking-p
                        runtime-consistency-checking-p
                        verbose-p
                        dont-show-variables
                        dont-show-head-projection-operators-p
                        dont-show-lambdas-p
                        how-many
                        only-new-tuples-p
                        timeout
                        proactive-tuple-computation-p
                        tuple-at-a-time-p
                        use-individual-synonyms-p
                        check-abox-consistency-p
                        ensure-tbox-classification-p
                        initial-abox-mirroring-p
                        initial-role-assertion-mirroring-p
                        classify-concepts-in-instance-assertions-p
                        exclude-permutations-p
                        record-explanations-p
                        parser-class
                        rewrite-defined-concepts-p
                        group-by-ops
                        bind-specials-p
                        original-query
                        rule-con-pattern
                        new-ind-ops
                        premise
                        generate-code-p
                        optimize-p
                        rewrite-semantically-p
                        rewrite-to-dnf-p
                        report-inconsistent-queries-p
                        report-tautological-queries-p
                        use-repository-p
                        put-into-repository-p
                        id
                        dont-check-id-p
                        parser
                        result-vois
                        substrate
                        abox
                        create-abox-if-not-found-p
                        package
                        type-of-substrate
                        prepare-now-p)
 */

     public String retrieveUnderPremiseM(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("retrieve-under-premise" , resArgs, query ).toString();
     }

     public RacerResult retrieveUnderPremiseM$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("retrieve-under-premise" , resArgs, query );
     }

     public String retrieveUnderPremiseM(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("retrieve-under-premise" , resArgs, query , keyArgs).toString();
     }

     public RacerResult retrieveUnderPremiseM$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("retrieve-under-premise" , resArgs, query , keyArgs);
     }

/** Racer Macro retrieve-under-premise1
(retrieve-under-premise1 query
                         res-args
                         &key
                         execute-p
                         dont-add-abox-duplicates-p
                         remove-duplicates-p
                         two-phase-processing-p
                         deliver-phase-two-warning-tokens-p
                         deliver-kb-has-changed-warning-tokens-p
                         add-rule-consequences-p
                         continuation-based-instance-retrieval-p
                         told-information-reasoning-p
                         final-consistency-checking-p
                         runtime-consistency-checking-p
                         verbose-p
                         dont-show-variables
                         dont-show-head-projection-operators-p
                         dont-show-lambdas-p
                         how-many
                         only-new-tuples-p
                         timeout
                         proactive-tuple-computation-p
                         tuple-at-a-time-p
                         use-individual-synonyms-p
                         check-abox-consistency-p
                         ensure-tbox-classification-p
                         initial-abox-mirroring-p
                         initial-role-assertion-mirroring-p
                         classify-concepts-in-instance-assertions-p
                         exclude-permutations-p
                         record-explanations-p
                         parser-class
                         rewrite-defined-concepts-p
                         group-by-ops
                         bind-specials-p
                         original-query
                         rule-con-pattern
                         new-ind-ops
                         premise
                         generate-code-p
                         optimize-p
                         rewrite-semantically-p
                         rewrite-to-dnf-p
                         report-inconsistent-queries-p
                         report-tautological-queries-p
                         use-repository-p
                         put-into-repository-p
                         id
                         dont-check-id-p
                         parser
                         result-vois
                         substrate
                         abox
                         create-abox-if-not-found-p
                         package
                         type-of-substrate
                         prepare-now-p)
 */

     public String retrieveUnderPremise1M(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("retrieve-under-premise1" , query, resArgs ).toString();
     }

     public RacerResult retrieveUnderPremise1M$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("retrieve-under-premise1" , query, resArgs );
     }

     public String retrieveUnderPremise1M(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("retrieve-under-premise1" , query, resArgs , keyArgs).toString();
     }

     public RacerResult retrieveUnderPremise1M$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("retrieve-under-premise1" , query, resArgs , keyArgs);
     }

/** Racer Macro retrieve-with-explanation
(retrieve-with-explanation res-args
                           query
                           &key
                           cutoff-fn
                           hypo-mode-stack
                           c-mode
                           r-mode
                           only-best-p
                           order-by
                           reverse-order-p
                           ensure-permutations-p
                           how-many
                           strategy
                           simple-result-p
                           runtime-consistency-checking-p
                           final-consistency-checking-p
                           same-as-only-p
                           candidate-individuals
                           binding-validator
                           &rest
                           (args))
 */

     public String retrieveWithExplanationM(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("retrieve-with-explanation" , resArgs, query ).toString();
     }

     public RacerResult retrieveWithExplanationM$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("retrieve-with-explanation" , resArgs, query );
     }

     public String retrieveWithExplanationM(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("retrieve-with-explanation" , resArgs, query , keyArgs).toString();
     }

     public RacerResult retrieveWithExplanationM$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("retrieve-with-explanation" , resArgs, query , keyArgs);
     }

/** Racer Macro retrieve1
(retrieve1 query
           res-args
           &key
           execute-p
           dont-add-abox-duplicates-p
           remove-duplicates-p
           two-phase-processing-p
           deliver-phase-two-warning-tokens-p
           deliver-kb-has-changed-warning-tokens-p
           add-rule-consequences-p
           continuation-based-instance-retrieval-p
           told-information-reasoning-p
           final-consistency-checking-p
           runtime-consistency-checking-p
           verbose-p
           dont-show-variables
           dont-show-head-projection-operators-p
           dont-show-lambdas-p
           how-many
           only-new-tuples-p
           timeout
           proactive-tuple-computation-p
           tuple-at-a-time-p
           use-individual-synonyms-p
           check-abox-consistency-p
           ensure-tbox-classification-p
           initial-abox-mirroring-p
           initial-role-assertion-mirroring-p
           classify-concepts-in-instance-assertions-p
           exclude-permutations-p
           record-explanations-p
           parser-class
           rewrite-defined-concepts-p
           group-by-ops
           bind-specials-p
           original-query
           rule-con-pattern
           new-ind-ops
           premise
           generate-code-p
           optimize-p
           rewrite-semantically-p
           rewrite-to-dnf-p
           report-inconsistent-queries-p
           report-tautological-queries-p
           use-repository-p
           put-into-repository-p
           id
           dont-check-id-p
           parser
           result-vois
           substrate
           abox
           create-abox-if-not-found-p
           package
           type-of-substrate
           prepare-now-p)
 */

     public String retrieve1M(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("retrieve1" , query, resArgs ).toString();
     }

     public RacerResult retrieve1M$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("retrieve1" , query, resArgs );
     }

     public String retrieve1M(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("retrieve1" , query, resArgs , keyArgs).toString();
     }

     public RacerResult retrieve1M$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("retrieve1" , query, resArgs , keyArgs);
     }

/** Racer Macro role-ancestors
(role-ancestors role-term &optional tbox)
 */

     public String roleAncestorsM(Object roleTerm ) throws RacerClientException {
          return racerCall("role-ancestors" , roleTerm ).toString();
     }

     public RacerResult roleAncestorsM$(Object roleTerm ) throws RacerClientException {
          return racerCall("role-ancestors" , roleTerm );
     }

     public String roleAncestorsM(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-ancestors" , roleTerm, tbox ).toString();
     }

     public RacerResult roleAncestorsM$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-ancestors" , roleTerm, tbox );
     }

/** Racer Macro role-children
(role-children role-term &optional tbox)
 */

     public String roleChildrenM(Object roleTerm ) throws RacerClientException {
          return racerCall("role-children" , roleTerm ).toString();
     }

     public RacerResult roleChildrenM$(Object roleTerm ) throws RacerClientException {
          return racerCall("role-children" , roleTerm );
     }

     public String roleChildrenM(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-children" , roleTerm, tbox ).toString();
     }

     public RacerResult roleChildrenM$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-children" , roleTerm, tbox );
     }

/** Racer Macro role-descendants
(role-descendants role-term &optional tbox)
 */

     public String roleDescendantsM(Object roleTerm ) throws RacerClientException {
          return racerCall("role-descendants" , roleTerm ).toString();
     }

     public RacerResult roleDescendantsM$(Object roleTerm ) throws RacerClientException {
          return racerCall("role-descendants" , roleTerm );
     }

     public String roleDescendantsM(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-descendants" , roleTerm, tbox ).toString();
     }

     public RacerResult roleDescendantsM$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-descendants" , roleTerm, tbox );
     }

/** Racer Macro role-disjoint?
(role-disjoint? role-term-1
                role-term-2
                &optional
                tbox)
 */

     public boolean roleDisjointMP(Object roleTerm1, Object roleTerm2 ) throws RacerClientException {
          return returnBoolean(racerCall("role-disjoint?" , roleTerm1, roleTerm2 ));
     }

     public boolean roleDisjointMP(Object roleTerm1, Object roleTerm2, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("role-disjoint?" , roleTerm1, roleTerm2, tbox ));
     }

/** Racer Macro role-domain
(role-domain role-term &optional tbox)
 */

     public String roleDomainM(Object roleTerm ) throws RacerClientException {
          return racerCall("role-domain" , roleTerm ).toString();
     }

     public RacerResult roleDomainM$(Object roleTerm ) throws RacerClientException {
          return racerCall("role-domain" , roleTerm );
     }

     public String roleDomainM(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-domain" , roleTerm, tbox ).toString();
     }

     public RacerResult roleDomainM$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-domain" , roleTerm, tbox );
     }

/** Racer Macro role-equivalent?
(role-equivalent? role-term-1
                  role-term-2
                  &optional
                  tbox)
 */

     public boolean roleEquivalentMP(Object roleTerm1, Object roleTerm2 ) throws RacerClientException {
          return returnBoolean(racerCall("role-equivalent?" , roleTerm1, roleTerm2 ));
     }

     public boolean roleEquivalentMP(Object roleTerm1, Object roleTerm2, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("role-equivalent?" , roleTerm1, roleTerm2, tbox ));
     }

/** Racer Macro role-inverse
(role-inverse role-term &optional tbox)
 */

     public String roleInverseM(Object roleTerm ) throws RacerClientException {
          return racerCall("role-inverse" , roleTerm ).toString();
     }

     public RacerResult roleInverseM$(Object roleTerm ) throws RacerClientException {
          return racerCall("role-inverse" , roleTerm );
     }

     public String roleInverseM(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-inverse" , roleTerm, tbox ).toString();
     }

     public RacerResult roleInverseM$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-inverse" , roleTerm, tbox );
     }

/** Racer Macro role-parents
(role-parents role-term &optional tbox)
 */

     public String roleParentsM(Object roleTerm ) throws RacerClientException {
          return racerCall("role-parents" , roleTerm ).toString();
     }

     public RacerResult roleParentsM$(Object roleTerm ) throws RacerClientException {
          return racerCall("role-parents" , roleTerm );
     }

     public String roleParentsM(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-parents" , roleTerm, tbox ).toString();
     }

     public RacerResult roleParentsM$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-parents" , roleTerm, tbox );
     }

/** Racer Macro role-range
(role-range role-term &optional tbox)
 */

     public String roleRangeM(Object roleTerm ) throws RacerClientException {
          return racerCall("role-range" , roleTerm ).toString();
     }

     public RacerResult roleRangeM$(Object roleTerm ) throws RacerClientException {
          return racerCall("role-range" , roleTerm );
     }

     public String roleRangeM(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-range" , roleTerm, tbox ).toString();
     }

     public RacerResult roleRangeM$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-range" , roleTerm, tbox );
     }

/** Racer Macro role-satisfiable?
(role-satisfiable? role &optional tbox)
 */

     public boolean roleSatisfiableMP(Object role ) throws RacerClientException {
          return returnBoolean(racerCall("role-satisfiable?" , role ));
     }

     public boolean roleSatisfiableMP(Object role, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("role-satisfiable?" , role, tbox ));
     }

/** Racer Macro role-subsumes?
(role-subsumes? role-term-1
                role-term-2
                &optional
                tbox)
 */

     public boolean roleSubsumesMP(Object roleTerm1, Object roleTerm2 ) throws RacerClientException {
          return returnBoolean(racerCall("role-subsumes?" , roleTerm1, roleTerm2 ));
     }

     public boolean roleSubsumesMP(Object roleTerm1, Object roleTerm2, Object tbox ) throws RacerClientException {
          return returnBoolean(racerCall("role-subsumes?" , roleTerm1, roleTerm2, tbox ));
     }

/** Racer Macro role-synonyms
(role-synonyms role-term &optional tbox)
 */

     public String roleSynonymsM(Object roleTerm ) throws RacerClientException {
          return racerCall("role-synonyms" , roleTerm ).toString();
     }

     public RacerResult roleSynonymsM$(Object roleTerm ) throws RacerClientException {
          return racerCall("role-synonyms" , roleTerm );
     }

     public String roleSynonymsM(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-synonyms" , roleTerm, tbox ).toString();
     }

     public RacerResult roleSynonymsM$(Object roleTerm, Object tbox ) throws RacerClientException {
          return racerCall("role-synonyms" , roleTerm, tbox );
     }

/** Racer Macro role?
(role? role-term &optional tbox-name)
 */

     public boolean roleMP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("role?" , roleTerm ));
     }

     public boolean roleMP(Object roleTerm, Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("role?" , roleTerm, tboxName ));
     }

/** Racer Macro roles-disjoint
(roles-disjoint role1 role2 &optional tbox)
 */

     public String rolesDisjointM(Object role1, Object role2 ) throws RacerClientException {
          return racerCall("roles-disjoint" , role1, role2 ).toString();
     }

     public RacerResult rolesDisjointM$(Object role1, Object role2 ) throws RacerClientException {
          return racerCall("roles-disjoint" , role1, role2 );
     }

     public String rolesDisjointM(Object role1, Object role2, Object tbox ) throws RacerClientException {
          return racerCall("roles-disjoint" , role1, role2, tbox ).toString();
     }

     public RacerResult rolesDisjointM$(Object role1, Object role2, Object tbox ) throws RacerClientException {
          return racerCall("roles-disjoint" , role1, role2, tbox );
     }

/** Racer Macro roles-equivalent
(roles-equivalent role1 role2 &optional tbox)
 */

     public String rolesEquivalentM(Object role1, Object role2 ) throws RacerClientException {
          return racerCall("roles-equivalent" , role1, role2 ).toString();
     }

     public RacerResult rolesEquivalentM$(Object role1, Object role2 ) throws RacerClientException {
          return racerCall("roles-equivalent" , role1, role2 );
     }

     public String rolesEquivalentM(Object role1, Object role2, Object tbox ) throws RacerClientException {
          return racerCall("roles-equivalent" , role1, role2, tbox ).toString();
     }

     public RacerResult rolesEquivalentM$(Object role1, Object role2, Object tbox ) throws RacerClientException {
          return racerCall("roles-equivalent" , role1, role2, tbox );
     }

/** Racer Macro same-as
(same-as individual-name-1 individual-name-2)
 */

     public String sameAsM(Object individualName1, Object individualName2 ) throws RacerClientException {
          return racerCall("same-as" , individualName1, individualName2 ).toString();
     }

     public RacerResult sameAsM$(Object individualName1, Object individualName2 ) throws RacerClientException {
          return racerCall("same-as" , individualName1, individualName2 );
     }

/** Racer Macro same-individual-as
(same-individual-as individual-name-1 individual-name-2)
 */

     public String sameIndividualAsM(Object individualName1, Object individualName2 ) throws RacerClientException {
          return racerCall("same-individual-as" , individualName1, individualName2 ).toString();
     }

     public RacerResult sameIndividualAsM$(Object individualName1, Object individualName2 ) throws RacerClientException {
          return racerCall("same-individual-as" , individualName1, individualName2 );
     }

/** Racer Macro signature
(signature &key
           atomic-concepts
           roles
           transitive-roles
           features
           attributes
           individuals
           objects)
 */

     public String signatureM( ) throws RacerClientException {
          return racerCall("signature"  ).toString();
     }

     public RacerResult signatureM$( ) throws RacerClientException {
          return racerCall("signature"  );
     }

     public String signatureM(  Object... keyArgs) throws RacerClientException {
          return racerCall("signature"  , keyArgs).toString();
     }

     public RacerResult signatureM$(  Object... keyArgs) throws RacerClientException {
          return racerCall("signature"  , keyArgs);
     }

/** Racer Macro sparql-answer-query
(sparql-answer-query sparql-query
                     &key
                     racer
                     stream
                     native
                     add-standard-prefixes
                     use-optimizer
                     &rest
                     (args))
 */

     public String sparqlAnswerQueryM(Object sparqlQuery ) throws RacerClientException {
          return racerCall("sparql-answer-query" , sparqlQuery ).toString();
     }

     public RacerResult sparqlAnswerQueryM$(Object sparqlQuery ) throws RacerClientException {
          return racerCall("sparql-answer-query" , sparqlQuery );
     }

     public String sparqlAnswerQueryM(Object sparqlQuery , Object... keyArgs) throws RacerClientException {
          return racerCall("sparql-answer-query" , sparqlQuery , keyArgs).toString();
     }

     public RacerResult sparqlAnswerQueryM$(Object sparqlQuery , Object... keyArgs) throws RacerClientException {
          return racerCall("sparql-answer-query" , sparqlQuery , keyArgs);
     }

/** Racer Macro sparql-retrieve
(sparql-retrieve sparql-query
                 &key
                 racer
                 stream
                 native
                 add-standard-prefixes
                 use-optimizer
                 &rest
                 (args))
 */

     public String sparqlRetrieveM(Object sparqlQuery ) throws RacerClientException {
          return racerCall("sparql-retrieve" , sparqlQuery ).toString();
     }

     public RacerResult sparqlRetrieveM$(Object sparqlQuery ) throws RacerClientException {
          return racerCall("sparql-retrieve" , sparqlQuery );
     }

     public String sparqlRetrieveM(Object sparqlQuery , Object... keyArgs) throws RacerClientException {
          return racerCall("sparql-retrieve" , sparqlQuery , keyArgs).toString();
     }

     public RacerResult sparqlRetrieveM$(Object sparqlQuery , Object... keyArgs) throws RacerClientException {
          return racerCall("sparql-retrieve" , sparqlQuery , keyArgs);
     }

/** Racer Macro state
(state &rest (forms))
 */

     public String stateM( ) throws RacerClientException {
          return racerCall("state"  ).toString();
     }

     public RacerResult stateM$( ) throws RacerClientException {
          return racerCall("state"  );
     }

     public String stateM(  Object... keyArgs) throws RacerClientException {
          return racerCall("state"  , keyArgs).toString();
     }

     public RacerResult stateM$(  Object... keyArgs) throws RacerClientException {
          return racerCall("state"  , keyArgs);
     }

/** Racer Macro subscribe
(subscribe subscriber
           query-concept
           &optional
           abox
           ip
           port
           use-simplified-protocol-p)
 */

     public String subscribeM(Object subscriber, Object queryConcept ) throws RacerClientException {
          return racerCall("subscribe" , subscriber, queryConcept ).toString();
     }

     public RacerResult subscribeM$(Object subscriber, Object queryConcept ) throws RacerClientException {
          return racerCall("subscribe" , subscriber, queryConcept );
     }

     public String subscribeM(Object subscriber, Object queryConcept, Object abox, Object ip, Object port, Object useSimplifiedProtocolP ) throws RacerClientException {
          return racerCall("subscribe" , subscriber, queryConcept, abox, ip, port, useSimplifiedProtocolP ).toString();
     }

     public RacerResult subscribeM$(Object subscriber, Object queryConcept, Object abox, Object ip, Object port, Object useSimplifiedProtocolP ) throws RacerClientException {
          return racerCall("subscribe" , subscriber, queryConcept, abox, ip, port, useSimplifiedProtocolP );
     }

     public String subscribeM(Object subscriber, Object queryConcept, Object abox, Object ip, Object port ) throws RacerClientException {
          return racerCall("subscribe" , subscriber, queryConcept, abox, ip, port ).toString();
     }

     public RacerResult subscribeM$(Object subscriber, Object queryConcept, Object abox, Object ip, Object port ) throws RacerClientException {
          return racerCall("subscribe" , subscriber, queryConcept, abox, ip, port );
     }

     public String subscribeM(Object subscriber, Object queryConcept, Object abox, Object ip ) throws RacerClientException {
          return racerCall("subscribe" , subscriber, queryConcept, abox, ip ).toString();
     }

     public RacerResult subscribeM$(Object subscriber, Object queryConcept, Object abox, Object ip ) throws RacerClientException {
          return racerCall("subscribe" , subscriber, queryConcept, abox, ip );
     }

     public String subscribeM(Object subscriber, Object queryConcept, Object abox ) throws RacerClientException {
          return racerCall("subscribe" , subscriber, queryConcept, abox ).toString();
     }

     public RacerResult subscribeM$(Object subscriber, Object queryConcept, Object abox ) throws RacerClientException {
          return racerCall("subscribe" , subscriber, queryConcept, abox );
     }

/** Racer Macro symmetric
(symmetric rolename &optional tbox)
 */

     public String symmetricM(Object rolename ) throws RacerClientException {
          return racerCall("symmetric" , rolename ).toString();
     }

     public RacerResult symmetricM$(Object rolename ) throws RacerClientException {
          return racerCall("symmetric" , rolename );
     }

     public String symmetricM(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("symmetric" , rolename, tbox ).toString();
     }

     public RacerResult symmetricM$(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("symmetric" , rolename, tbox );
     }

/** Racer Macro symmetric?
(symmetric? role-term &optional tbox-name)
 */

     public boolean symmetricMP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("symmetric?" , roleTerm ));
     }

     public boolean symmetricMP(Object roleTerm, Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("symmetric?" , roleTerm, tboxName ));
     }

/** Racer Macro tbox-classified?
(tbox-classified? &optional tbox-name)
 */

     public boolean tboxClassifiedMP( ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-classified?"  ));
     }

     public boolean tboxClassifiedMP(Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-classified?" , tboxName ));
     }

/** Racer Macro tbox-coherent?
(tbox-coherent? &optional tbox-name)
 */

     public boolean tboxCoherentMP( ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-coherent?"  ));
     }

     public boolean tboxCoherentMP(Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-coherent?" , tboxName ));
     }

/** Racer Macro tbox-cyclic?
(tbox-cyclic? &optional tbox-name)
 */

     public boolean tboxCyclicMP( ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-cyclic?"  ));
     }

     public boolean tboxCyclicMP(Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-cyclic?" , tboxName ));
     }

/** Racer Macro tbox-prepared?
(tbox-prepared? &optional tbox-name)
 */

     public boolean tboxPreparedMP( ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-prepared?"  ));
     }

     public boolean tboxPreparedMP(Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("tbox-prepared?" , tboxName ));
     }

/** Racer Macro tbox-retrieve
(tbox-retrieve res-args
               query
               &key
               execute-p
               dont-add-abox-duplicates-p
               remove-duplicates-p
               two-phase-processing-p
               deliver-phase-two-warning-tokens-p
               deliver-kb-has-changed-warning-tokens-p
               add-rule-consequences-p
               continuation-based-instance-retrieval-p
               told-information-reasoning-p
               final-consistency-checking-p
               runtime-consistency-checking-p
               verbose-p
               dont-show-variables
               dont-show-head-projection-operators-p
               dont-show-lambdas-p
               how-many
               only-new-tuples-p
               timeout
               proactive-tuple-computation-p
               tuple-at-a-time-p
               use-individual-synonyms-p
               check-abox-consistency-p
               ensure-tbox-classification-p
               initial-abox-mirroring-p
               initial-role-assertion-mirroring-p
               classify-concepts-in-instance-assertions-p
               exclude-permutations-p
               record-explanations-p
               parser-class
               rewrite-defined-concepts-p
               group-by-ops
               bind-specials-p
               original-query
               rule-con-pattern
               new-ind-ops
               premise
               generate-code-p
               optimize-p
               rewrite-semantically-p
               rewrite-to-dnf-p
               report-inconsistent-queries-p
               report-tautological-queries-p
               use-repository-p
               put-into-repository-p
               id
               dont-check-id-p
               parser
               result-vois
               tbox
               package
               create-tbox-if-not-found-p
               substrate)
 */

     public String tboxRetrieveM(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("tbox-retrieve" , resArgs, query ).toString();
     }

     public RacerResult tboxRetrieveM$(Object resArgs, Object query ) throws RacerClientException {
          return racerCall("tbox-retrieve" , resArgs, query );
     }

     public String tboxRetrieveM(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("tbox-retrieve" , resArgs, query , keyArgs).toString();
     }

     public RacerResult tboxRetrieveM$(Object resArgs, Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("tbox-retrieve" , resArgs, query , keyArgs);
     }

/** Racer Macro tbox-retrieve1
(tbox-retrieve1 query
                res-args
                &key
                execute-p
                dont-add-abox-duplicates-p
                remove-duplicates-p
                two-phase-processing-p
                deliver-phase-two-warning-tokens-p
                deliver-kb-has-changed-warning-tokens-p
                add-rule-consequences-p
                continuation-based-instance-retrieval-p
                told-information-reasoning-p
                final-consistency-checking-p
                runtime-consistency-checking-p
                verbose-p
                dont-show-variables
                dont-show-head-projection-operators-p
                dont-show-lambdas-p
                how-many
                only-new-tuples-p
                timeout
                proactive-tuple-computation-p
                tuple-at-a-time-p
                use-individual-synonyms-p
                check-abox-consistency-p
                ensure-tbox-classification-p
                initial-abox-mirroring-p
                initial-role-assertion-mirroring-p
                classify-concepts-in-instance-assertions-p
                exclude-permutations-p
                record-explanations-p
                parser-class
                rewrite-defined-concepts-p
                group-by-ops
                bind-specials-p
                original-query
                rule-con-pattern
                new-ind-ops
                premise
                generate-code-p
                optimize-p
                rewrite-semantically-p
                rewrite-to-dnf-p
                report-inconsistent-queries-p
                report-tautological-queries-p
                use-repository-p
                put-into-repository-p
                id
                dont-check-id-p
                parser
                result-vois
                tbox
                package
                create-tbox-if-not-found-p
                substrate)
 */

     public String tboxRetrieve1M(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("tbox-retrieve1" , query, resArgs ).toString();
     }

     public RacerResult tboxRetrieve1M$(Object query, Object resArgs ) throws RacerClientException {
          return racerCall("tbox-retrieve1" , query, resArgs );
     }

     public String tboxRetrieve1M(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("tbox-retrieve1" , query, resArgs , keyArgs).toString();
     }

     public RacerResult tboxRetrieve1M$(Object query, Object resArgs , Object... keyArgs) throws RacerClientException {
          return racerCall("tbox-retrieve1" , query, resArgs , keyArgs);
     }

/** Racer Macro timenet-retrieve
(timenet-retrieve query &key abox)
 */

     public String timenetRetrieveM(Object query ) throws RacerClientException {
          return racerCall("timenet-retrieve" , query ).toString();
     }

     public RacerResult timenetRetrieveM$(Object query ) throws RacerClientException {
          return racerCall("timenet-retrieve" , query );
     }

     public String timenetRetrieveM(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("timenet-retrieve" , query , keyArgs).toString();
     }

     public RacerResult timenetRetrieveM$(Object query , Object... keyArgs) throws RacerClientException {
          return racerCall("timenet-retrieve" , query , keyArgs);
     }

/** Racer Macro transitive
(transitive rolename &optional tbox)
 */

     public String transitiveM(Object rolename ) throws RacerClientException {
          return racerCall("transitive" , rolename ).toString();
     }

     public RacerResult transitiveM$(Object rolename ) throws RacerClientException {
          return racerCall("transitive" , rolename );
     }

     public String transitiveM(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("transitive" , rolename, tbox ).toString();
     }

     public RacerResult transitiveM$(Object rolename, Object tbox ) throws RacerClientException {
          return racerCall("transitive" , rolename, tbox );
     }

/** Racer Macro transitive?
(transitive? role-term &optional tbox-name)
 */

     public boolean transitiveMP(Object roleTerm ) throws RacerClientException {
          return returnBoolean(racerCall("transitive?" , roleTerm ));
     }

     public boolean transitiveMP(Object roleTerm, Object tboxName ) throws RacerClientException {
          return returnBoolean(racerCall("transitive?" , roleTerm, tboxName ));
     }

/** Racer Macro unbind
(unbind name)
 */

     public String unbindM(Object name ) throws RacerClientException {
          return racerCall("unbind" , name ).toString();
     }

     public RacerResult unbindM$(Object name ) throws RacerClientException {
          return racerCall("unbind" , name );
     }

/** Racer Macro undefine
(undefine name)
 */

     public String undefineM(Object name ) throws RacerClientException {
          return racerCall("undefine" , name ).toString();
     }

     public RacerResult undefineM$(Object name ) throws RacerClientException {
          return racerCall("undefine" , name );
     }

/** Racer Macro undefquery
(undefquery name
            &key
            tbox
            arity)
 */

     public String undefqueryM(Object name ) throws RacerClientException {
          return racerCall("undefquery" , name ).toString();
     }

     public RacerResult undefqueryM$(Object name ) throws RacerClientException {
          return racerCall("undefquery" , name );
     }

     public String undefqueryM(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("undefquery" , name , keyArgs).toString();
     }

     public RacerResult undefqueryM$(Object name , Object... keyArgs) throws RacerClientException {
          return racerCall("undefquery" , name , keyArgs);
     }

/** Racer Macro unpublish
(unpublish individual &optional abox)
 */

     public String unpublishM(Object individual ) throws RacerClientException {
          return racerCall("unpublish" , individual ).toString();
     }

     public RacerResult unpublishM$(Object individual ) throws RacerClientException {
          return racerCall("unpublish" , individual );
     }

     public String unpublishM(Object individual, Object abox ) throws RacerClientException {
          return racerCall("unpublish" , individual, abox ).toString();
     }

     public RacerResult unpublishM$(Object individual, Object abox ) throws RacerClientException {
          return racerCall("unpublish" , individual, abox );
     }

/** Racer Macro unrelated
(unrelated left-name right-name role-name)
 */

     public String unrelatedM(Object leftName, Object rightName, Object roleName ) throws RacerClientException {
          return racerCall("unrelated" , leftName, rightName, roleName ).toString();
     }

     public RacerResult unrelatedM$(Object leftName, Object rightName, Object roleName ) throws RacerClientException {
          return racerCall("unrelated" , leftName, rightName, roleName );
     }

/** Racer Macro unsubscribe
(unsubscribe subscriber
             &optional
             query-concept
             abox)
 */

     public String unsubscribeM(Object subscriber ) throws RacerClientException {
          return racerCall("unsubscribe" , subscriber ).toString();
     }

     public RacerResult unsubscribeM$(Object subscriber ) throws RacerClientException {
          return racerCall("unsubscribe" , subscriber );
     }

     public String unsubscribeM(Object subscriber, Object queryConcept, Object abox ) throws RacerClientException {
          return racerCall("unsubscribe" , subscriber, queryConcept, abox ).toString();
     }

     public RacerResult unsubscribeM$(Object subscriber, Object queryConcept, Object abox ) throws RacerClientException {
          return racerCall("unsubscribe" , subscriber, queryConcept, abox );
     }

     public String unsubscribeM(Object subscriber, Object queryConcept ) throws RacerClientException {
          return racerCall("unsubscribe" , subscriber, queryConcept ).toString();
     }

     public RacerResult unsubscribeM$(Object subscriber, Object queryConcept ) throws RacerClientException {
          return racerCall("unsubscribe" , subscriber, queryConcept );
     }

/** Racer With-Macro with-bindings
(with-bindings &body body)
 */

     public void withBindings() {
          pushWith("with-bindings");
     }

     public void endWithBindings() {
          popWith("with-bindings");
     }

/** Racer With-Macro with-bindings-evaluated
(with-bindings-evaluated &body body)
 */

     public void withBindingsEvaluated() {
          pushWith("with-bindings-evaluated");
     }

     public void endWithBindingsEvaluated() {
          popWith("with-bindings-evaluated");
     }

/** Racer With-Macro with-critical-section
(with-critical-section (&key &body body) &body body)
 */

     public void withCriticalSection(Object... keyArgs) {
          pushWith("with-critical-section", keyArgs);
     }

     public void endWithCriticalSection() {
          popWith("with-critical-section");
     }

/** Racer With-Macro with-future-bindings
(with-future-bindings &body body)
 */

     public void withFutureBindings() {
          pushWith("with-future-bindings");
     }

     public void endWithFutureBindings() {
          popWith("with-future-bindings");
     }

/** Racer With-Macro with-future-bindings-evaluated
(with-future-bindings-evaluated &body body)
 */

     public void withFutureBindingsEvaluated() {
          pushWith("with-future-bindings-evaluated");
     }

     public void endWithFutureBindingsEvaluated() {
          popWith("with-future-bindings-evaluated");
     }

/** Racer With-Macro with-nrql-settings
(with-nrql-settings (&key
                     mode
                     dont-show-variables
                     dont-show-lambdas
                     dont-show-head-projection-operators
                     abox-mirroring
                     query-optimization
                     optimizer-use-cardinality-heuristics
                     how-many-tuples
                     timeout
                     warnings
                     add-rule-consequences-automatically
                     dont-add-abox-duplicates
                     two-phase-query-processing-mode
                     phase-two-starts-warning-tokens
                     kb-has-changed-warning-tokens
                     told-information-querying
                     tuple-computation-mode
                     exclude-permutations
                     query-repository
                     report-inconsistent-queries
                     report-tautological-queries
                     query-realization
                     bindings
                     check-abox-consistency
                     use-individual-equivalence-classes
                     rewrite-to-dnf
                     type-of-substrate
                     abox
                     tbox)
  &body
  body)
 */

     public void withNrqlSettings(Object... keyArgs) {
          pushWith("with-nrql-settings", keyArgs);
     }

     public void endWithNrqlSettings() {
          popWith("with-nrql-settings");
     }

/** Racer With-Macro with-nrql-settings-evaluated
(with-nrql-settings-evaluated (&key
                               mode
                               dont-show-variables
                               dont-show-lambdas
                               dont-show-head-projection-operators
                               abox-mirroring
                               query-optimization
                               optimizer-use-cardinality-heuristics
                               how-many-tuples
                               timeout
                               warnings
                               add-rule-consequences-automatically
                               dont-add-abox-duplicates
                               two-phase-query-processing-mode
                               phase-two-starts-warning-tokens
                               kb-has-changed-warning-tokens
                               told-information-querying
                               tuple-computation-mode
                               exclude-permutations
                               query-repository
                               report-inconsistent-queries
                               report-tautological-queries
                               query-realization
                               bindings
                               check-abox-consistency
                               use-individual-equivalence-classes
                               rewrite-to-dnf
                               type-of-substrate
                               abox
                               tbox)
  &body
  body)
 */

     public void withNrqlSettingsEvaluated(Object... keyArgs) {
          pushWith("with-nrql-settings-evaluated", keyArgs);
     }

     public void endWithNrqlSettingsEvaluated() {
          popWith("with-nrql-settings-evaluated");
     }

/** Racer With-Macro with-unique-name-assumption
(with-unique-name-assumption (&key &body body) &body body)
 */

     public void withUniqueNameAssumption(Object... keyArgs) {
          pushWith("with-unique-name-assumption", keyArgs);
     }

     public void endWithUniqueNameAssumption() {
          popWith("with-unique-name-assumption");
     }

/** Racer With-Macro without-unique-name-assumption
(without-unique-name-assumption (&key &body body) &body body)
 */

     public void withoutUniqueNameAssumption(Object... keyArgs) {
          pushWith("without-unique-name-assumption", keyArgs);
     }

     public void endWithoutUniqueNameAssumption() {
          popWith("without-unique-name-assumption");
     }

/** Racer Macro xml-output
(xml-output expr)
 */

     public String xmlOutputM(Object expr ) throws RacerClientException {
          return racerCall("xml-output" , expr ).toString();
     }

     public RacerResult xmlOutputM$(Object expr ) throws RacerClientException {
          return racerCall("xml-output" , expr );
     }

/** Racer Macro xml-native-output
(xml-native-output expr)
 */

     public String xmlNativeOutputM(Object expr ) throws RacerClientException {
          return racerCall("xml-native-output" , expr ).toString();
     }

     public RacerResult xmlNativeOutputM$(Object expr ) throws RacerClientException {
          return racerCall("xml-native-output" , expr );
     }

/** Racer Macro xml-input
(xml-input expr)
 */

     public String xmlInputM(Object expr ) throws RacerClientException {
          return racerCall("xml-input" , expr ).toString();
     }

     public RacerResult xmlInputM$(Object expr ) throws RacerClientException {
          return racerCall("xml-input" , expr );
     }

/** Racer Macro owlapi-getlastoutputstreamstring
(|OWLAPI-getLastOutputStreamString| &optional reasoner)
 */

     public String owlapiGetLastOutputStreamStringM( ) throws RacerClientException {
          return racerCall("|OWLAPI-getLastOutputStreamString|"  ).toString();
     }

     public RacerResult owlapiGetLastOutputStreamStringM$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getLastOutputStreamString|"  );
     }

     public String owlapiGetLastOutputStreamStringM(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getLastOutputStreamString|" , reasoner ).toString();
     }

     public RacerResult owlapiGetLastOutputStreamStringM$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getLastOutputStreamString|" , reasoner );
     }

/** Racer Macro owlapi-getlastanswer
(|OWLAPI-getLastAnswer| &optional reasoner)
 */

     public String owlapiGetLastAnswerM( ) throws RacerClientException {
          return racerCall("|OWLAPI-getLastAnswer|"  ).toString();
     }

     public RacerResult owlapiGetLastAnswerM$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getLastAnswer|"  );
     }

     public String owlapiGetLastAnswerM(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getLastAnswer|" , reasoner ).toString();
     }

     public RacerResult owlapiGetLastAnswerM$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getLastAnswer|" , reasoner );
     }

/** Racer Macro owlapi-getidsoflastanswer
(|OWLAPI-getIDsOfLastAnswer| &optional reasoner)
 */

     public String owlapiGetIDsOfLastAnswerM( ) throws RacerClientException {
          return racerCall("|OWLAPI-getIDsOfLastAnswer|"  ).toString();
     }

     public RacerResult owlapiGetIDsOfLastAnswerM$( ) throws RacerClientException {
          return racerCall("|OWLAPI-getIDsOfLastAnswer|"  );
     }

     public String owlapiGetIDsOfLastAnswerM(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getIDsOfLastAnswer|" , reasoner ).toString();
     }

     public RacerResult owlapiGetIDsOfLastAnswerM$(Object reasoner ) throws RacerClientException {
          return racerCall("|OWLAPI-getIDsOfLastAnswer|" , reasoner );
     }

/** Racer Macro get-namespace-prefixes
(get-namespace-prefixes)
 */

     public String getNamespacePrefixesM( ) throws RacerClientException {
          return racerCall("get-namespace-prefixes"  ).toString();
     }

     public RacerResult getNamespacePrefixesM$( ) throws RacerClientException {
          return racerCall("get-namespace-prefixes"  );
     }

/** Racer Macro ensure-small-tboxes
(ensure-small-tboxes)
 */

     public String ensureSmallTboxesM( ) throws RacerClientException {
          return racerCall("ensure-small-tboxes"  ).toString();
     }

     public RacerResult ensureSmallTboxesM$( ) throws RacerClientException {
          return racerCall("ensure-small-tboxes"  );
     }

/** Racer Macro evaluate
(evaluate expr)
 */

     public String evaluateM(Object expr ) throws RacerClientException {
          return racerCall("evaluate" , expr ).toString();
     }

     public RacerResult evaluateM$(Object expr ) throws RacerClientException {
          return racerCall("evaluate" , expr );
     }

/** Racer Macro evaluate1
(evaluate1 expr)
 */

     public String evaluate1M(Object expr ) throws RacerClientException {
          return racerCall("evaluate1" , expr ).toString();
     }

     public RacerResult evaluate1M$(Object expr ) throws RacerClientException {
          return racerCall("evaluate1" , expr );
     }

/** Racer Function exit-server
(exit-server)
 */

  public boolean exitServer( ) throws RacerClientException {

    try {
	    
	racerCall("exit-server"  ).toString();
	
    } catch (RacerClientException ex) { return true; }

    return false;
  
  }

}
