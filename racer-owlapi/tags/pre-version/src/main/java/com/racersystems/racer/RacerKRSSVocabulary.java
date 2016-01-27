package com.racersystems.racer;
/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Vocabulary for the KRSS-like syntax of RACER.
 *
 * @author Olaf Noppens
 * @version 1.0
 */
public class RacerKRSSVocabulary {
    public static final int PLAIN_ROLE = 0;
    public static final int TRANSITIVE_ROLE = 1;
    public static final int FEATURE = 2;
    public static final int SYMMETRIC_ROLE = 4;
    public static final int REFLEXIVE_ROLE = 8;

    public static final int K_LOGIC = 1;
    public static final String K_LOGIC_ATTRIBUTE = ":K";
    public static final int K4_LOGIC = 2;
    public static final String K4_LOGIC_ATTRIBUTE = ":K4";
    public static final int S4_LOGIC = 3;
    public static final String S4_LOGIC_ATTRIBUTE = ":S4";

    public static final String OPEN_BRACKET = "(";
    public static final String CLOSE_BRACKET = ")";
    public static final String SPACE = " ";
    public static final String DELIMITER_STRING = "|";
    public static final String QUOTE = "\"";
    public static final String BQUOTE = "'";

    public static final char OPEN_BRACKET_CHAR = '(';
    public static final char CLOSE_BRACKET_CHAR = ')';
    public static final char QUOTE_CHAR = '"';
    public static final char DELIMITER_LINE = '|';
    public static final char SPACE_CHAR = ' ';

    public static final String ABOX_ATTRIBUTE = ":abox";
    public static final String ABOX_CONSISTENT = "abox-consistent?";
    public static final String ABOX_REALIZED_P = "abox-realized-p";
    public static final String ADD_CONCEPT_ASSERTION = "add-concept-assertion";
    public static final String ADD_CONCEPT_AXIOM = "add-concept-axiom";
    public static final String ADD_DATATYPE_PROPERTY = "add-datatype-property";
    public static final String ADD_DIFFERENT_FROM_ASSERTION = "add-different-from-assertion";
    public static final String ADD_SAME_INDIVIDUAL_AS_ASSERTION = "add-same-individual-as-assertion";
    public static final String ADD_ROLE_ASSERTION = "add-role-assertion";
    public static final String ADD_DATATYPE_ROLE_FILLER = "add-datatype-role-filler";
    public static final String ALC_CONCEPT_COHERENT = "alc-concept-coherent";
    public static final String ALL_ABOXES = "all-aboxes";
    public static final String ALL_ATOMIC_CONCEPTS = "all-atomic-concepts";
    public static final String ALL_FEATURES = "all-features";
    public static final String ALL_INDIVIDUALS = "all-individuals";
    public static final String ALL_ROLES = "all-roles";
    public static final String ALL_ROLE_ASSERTION_FOR_INDIVIDUAL_IN_DOMAIN = "ALL-ROLE-ASSERTIONS-FOR-INDIVIDUAL-IN-DOMAIN";
    public static final String ALL_TBOXES = "all-tboxes";
    public static final String ALL_TRANSITIVE_ROLES = "all-transitive-roles";
    public static final String AND = "and";
    public static final String ASSERTIONS_ATTRIBUTE = ":assertions";
    public static final String CHECK_TBOX_COHERENCE = "check-tbox-coherence";
    public static final String CD_ATTRIBUTE_P = "cd-attribute-p";
    public static final String CD_OBJECT_P = "cd-object-p";
    public static final String CLASSIFY_TBOX = "classify-tbox";
    public static final String CLONE_ABOX = "clone-abox";
    public static final String CLONE_TBOX = "clone-tbox";
    public static final String CONCEPT_QM = "concept?";
    public static final String CONCEPT_ANCESTORS = "concept-ancestors";
    public static final String CONCEPT_CHILDREN = "concept-children";
    public static final String CONCEPT_DESCENDANTS = "concept-descendants";
    public static final String CONCEPT_DISJOINT = "concept-disjoint?";
    public static final String CONCEPT_DISJOINT_p = "concept-disjoint-p";
    public static final String CONCEPT_EQUIVALENT = "concept-equivalent?";
    public static final String CONCEPT_EQUIVALENT_P = "concept-equivalent-P";
    public static final String CONCEPT_INSTANCES = "concept-instances";
    public static final String CONCEPT_IS_PRIMITIVE_P = "concept-is-primitive-p";
    public static final String CONCEPT_P = "concept-p";
    public static final String CONCEPT_PARENTS = "concept-parents";
    public static final String CONCEPT_SATISFIABLE = "concept-satisfiable?";
    public static final String CONCEPT_SATISFIABLE_P = "concept-satisfiable-p";
    public static final String CONCEPT_SUBSUMES = "concept-subsumes?";
    public static final String CONCEPT_SYNONYMS = "concept-synonyms";
    public static final String CURRENT_TBOX = "current-tbox";
    public static final String DATATYPE_ROLE_HAS_RANGE = "datatype-role-has-range";
    public static final String DATATYPE_ROLE_RANGE = "datatype-role-range";
    public static final String DECLARE_DISJOINT = "declare-disjoint";
    public static final String DEFINE_CONCEPT = "define-concept";
    public static final String DEFINE_DISTINCT_INDIVIDUAL = "define-distinct-individual";
    public static final String DEFINE_PRIMITIVE_CONCEPT = "define-primitive-concept";
    public static final String DEFINE_PRIMITIVE_ROLE = "define-primitive-role";
    public static final String DELETE_ABOX = "delete-abox";
    public static final String DELETE_ALL_ABOXES = "delete-all-tboxes";
    public static final String DELETE_TBOX = "delete-tbox";
    public static final String DELETE_ALL_TBOXES = "delete-all-tboxes";
    public static final String DISJOINT = "disjoint";
    public static final String DOMAIN_ATTRIBUTE = ":domain";
    public static final String FEATURE_ATTRIBUTE = ":feature";
    public static final String FEATURE_P = "feature-p";
    public static final String FORGET = "forget";
    public static final String FORGET_CONCEPT_ASSERTION = "forget-concept-assertion";
    public static final String FORGET_DISJOINTNESS_AXIOM_STATEMENT = "forget-disjointness-axiom-statement";
    public static final String FORGET_ROLE_ASSERTION = "forget-role-assertion";
    public static final String GET_CONCEPT_ASSERTION = "get-concept-definition";
    public static final String GET_RACER_VERSION = "get-racer-version";
    public static final String GET_RACER_BUILD_VERSION = "get-build-version";
    public static final String IMPLIES = "implies";
    public static final String IMPLIES_ROLE = "implies-role";
    public static final String IN_ABOX = "in-abox";
    public static final String INCLUSION_P_ATTRIBUTE = ":inclusion:p";
    public static final String IN_KNOWLEDGEBASE = "in-knowledge-base";
    public static final String IN_TBOX = "in-tbox";
    public static final String INVERSE_ATTRIBUTE = ":inverse";
    public static final String INDIVIDUAL_DIRECT_TYPES = "individual-direct-types";
    public static final String INDIVIDUAL_EQUAL = "individual-equal?";
    public static final String INDIVIDUAL_FILLERS = "individual-fillers";
    public static final String INDIVIDUAL_INSTANCE = "individual-instance?";
    public static final String INDIVIDUAL_NOT_EQUALS = "individual-not-equal?";
    public static final String INDIVIDUAL_P = "individual-p";
    public static final String INDIVIDUAL_SYNONYMS = "individual-synonyms";
    public static final String INDIVIDUALS_RELATED = "individuals-related?";
    public static final String INDIVIDUALS_RELATED_P = "individuals-related-p";
    public static final String INDIVIDUAL_TYPES = "individual-types";
    public static final String INIT_ATTRIBUTE = ":init";
    public static final String INSTANCE = "instance ";
    public static final String INSTANTIATORS = "instantiators";
    public static final String LAMBDA_ATTRIBUTE = ":lambda";
    public static final String LOGIC_ATTRIBUTE = ":logic";
    public static final String MOST_SPECIFIC_INSTANTIATORS = "most-specific-instantiators";
    public static final String NEG = "neg";
    public static final String NEQ = "neq";
    public static final String NEW_NAME_ATTRIBUTE = ":new-name";
    public static final String OPTIMIZED = "optimized";
    public static final String OVERWRITE_ATTRIBUTE = ":overwrite";
    public static final String PARENTS_ATTRIBUTE = ":parents";
    public static final String PREPARE_ABOX = "prepare-abox";
    public static final String PREPARE_RACER_ENGINE_ABOX = "prepare-racer-engine :abox ";
    public static final String RANGE = "range";
    public static final String RANGE_ATTRIBUTE = ":range";
    public static final String REALIZE_ABOX = "realize-abox";
    public static final String RETRIEVE = "retrieve";
    public static final String RETRIEVE1 = "retrieve1";
    public static final String RRETRIEVE_DIRECT_PRECESSORS = "retrieve-direct-predecessors";
    public static final String REFLEXIVE_ATTRIBUTE = ":reflexive";
    public static final String REFLEXIVE_P = "reflexive-p";
    public static final String DESCRIBE_INDIVIDUAL = "describe-individual";
    public static final String RELATED = "related";
    public static final String RETRIEVE_DIRECT_PREDECESSORS = "retrieve-direct-predecessors";
    public static final String RETRIEVE_INDIVIDUAL_FILLERS = "retrieve-individual-fillers";
    public static final String RETRIEVE_INDIVIDUAL_FILLED_ROLES = "retrieve-individual-filled-roles";
    public static final String RETRIEVE_INDIVIDUAL_TOLD_DATATYPE_FILLERS = "retrieve-individual-told-datatype-fillers";
    public static final String ROLE_QM = "role?";
    public static final String ROLE_ANCESTORS = "role-ancestors";
    public static final String ROLE_CHILDREN = "role-children";
    public static final String ROLE_DESCENDANTS = "role-descendants";
    public static final String ROLE_DOMAIN = "role-domain";
    public static final String ROLE_INVERSE = "role-inverse";
    public static final String ROLE_IS_USED_AS_DATATYPE_PROPERTY = "role-is-used-as-datatype-property";
    public static final String ROLE_FILLERS_ATTRIBUTE = ":ROLE-FILLERS";
    public static final String ROLE_P = "role-p";
    public static final String ROLE_PARENTS = "role-parents";
    public static final String ROLE_SUBSUMES = "role-subsumes?";
    public static final String ROLE_EQUIVALENT = "role-equivalent?";
    public static final String ROLES_EQUIVALENT = "roles-equivalent";
    public static final String ROLE_RANGE = "role-range";
    public static final String ROLE_SYNONYMS = "role-synonyms";
    public static final String ROLE_USED_AS_DATATYPE_PROPERTY = "role-used-as-datatype-property-p";
    public static final String PROJECT_TO = "project-to";
    public static final String SAME_AS = "same-as";
    public static final String SEQUENCE = "sequence";
    public static final String SET_NRQL_MODE = "set-nrql-mode";
    public static final String SET_UNIQUE_NAME_ASSUMPTION = "set-unique-name-assumption";
    public static final String SYMMETRIC_ATTRIBUTE = ":symmetric";
    public static final String SYMMETRIC_P = "symmetric-p";
    public static final String TAXONOMY = "taxonomy";
    public static final String TBOX_ATTRIBUTE = ":tbox";
    public static final String TBOX_CLASSIFIED_P = "tbox-classified-p";
    public static final String TBOX_COHERENT = "tbox-coherent?";
    public static final String TBOX_COHERENT_P = "tbox-coherent-p";
    public static final String TOLD_ATTRIBUTE = ":TOLD";
    public static final String TRANSITIVE = "transitive";
    public static final String TRANSITIVE_P = "transitive-p";
    public static final String TRANSITIVE_ATTRIBUTE = ":transitive";
    public static final String NIL = "nil";
    public static final String BOTTOM = "bottom";
    public static final String BOTTOM_STAR = "*bottom*";
    public static final String TOP = "top";
    public static final String TOP_STAR = "*top*";
    public static final String TRUE = "t";
    public static final String BOTTOM_OBJECT_ROLE = "bottom-object-role";
    public static final String BOTTOM_OBJECT_ROLE_STAR = "*bottom-object-role*";
    public static final String BOTTOM_DATA_ROLE = "bottom-datatype-role";
    public static final String BOTTOM_DATA_ROLE_STAR = "*bottom-datatype-role*";
    public static final String TOP_OBJECT_ROLE = "top-object-role";
    public static final String TOP_OBJECT_ROLE_STAR = "*top-object-role*";
    public static final String TOP_DATA_ROLE = "top-datatype-role";
    public static final String TOP_DATA_ROLE_STAR = "*top-datatype-role*";

    public static final String AT_LEAST = "at-least";
    public static final String DAT_LEAST = "d-at-least";
    public static final String AT_MOST = "at-most";
    public static final String DAT_MOST = "d-at-most";
    public static final String EXACTLY = "exactly";
    public static final String DEXACTLY = "d-exactly";
    public static final String ALL = "all";
    public static final String DALL = "d-all";
    public static final String SOME = "some";
    public static final String DSOME = "d-some";
    public static final String OR = "or";
    public static final String NOT = "not";
    public static final String INV = "inv";
    public static final String ONE_OF = "one-of";
    public static final String HAS_VALUE = "has-value";

    public static final String STRING = "string";
    public static final String INTEGER = "integer";
    public static final String REAL = "real";
    public static final String CARDINAL = "cardinal";
    public static final String COMPLEX = "complex";


    public static final String INVERSE_ROLE = "inverse";

    public static final String DIRECT_INSTANCES_RETRIEVAL_PROLOG_START = "(evaluate (let ((inds (retrieve-concept-instances ";
    public static final String DIRECT_INSTANCES_RETRIEVAL_PROLOG_END = "(subclasses (maplist first (atomic-concept-children ";
    public static final String DIRECT_INSTANCES_RETRIEVAL_EPILOG = "(remove-if (lambda (ind) (some (lambda (subclass) (individual-instance-p ind subclass (current-abox))) subclasses)) inds))";

    public static final String ROLES_EQUIVALENT_MINILISP_PROLOG = "(evaluate ((lambda (role1 role2) (let ((tbox (current-tbox)))(cond ((and (symbolp role1)(consp role2) (eq (first role2) 'inv))  (progn (unless (role-p role1)(add-role-axioms tbox role1))(unless (role-p (second role2))(add-role-axioms tbox (second role2)))(inverse-of-role role1 (second role2) tbox)))((and (symbolp role2)(consp role1) (eq (first role1) 'inv))(progn  (unless (role-p (second role1))(add-role-axioms tbox (second role1)))(unless (role-p role2)(add-role-axioms tbox role2))(inverse-of-role role2 (second role1) tbox)))((and (consp role1)(eq (first role1) 'inv)(consp role2)(eq (first role2) 'inv))(progn (unless (role-p (second role1))(add-role-axioms tbox (second role1)))(unless (role-p (second role2))(add-role-axioms tbox (second role2)))(roles-equivalent-1 (second role1) (second role2) tbox)))(t (roles-equivalent-1 role1 role2 tbox)))))";
}
