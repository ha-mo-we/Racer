package com.racersystems.racer;

import com.racersystems.racer.RacerKRSSVocabulary;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Extension of the RacerVocabulary.
 * @author Olaf Noppens (ON)
 */
public class RacerOWLAPIVocabulary extends RacerKRSSVocabulary {
    public static final String OWLAPI_ENABLE_SIMPLIFY_PROTOCOL = "OWLAPI-enableSimplifiedProtocol";
    public static final String OWLAPI_GETLASTANSWER = "OWLAPI-getLastAnswer";

    public static final String OWLAPI_APPLY_CHANGES = "OWLAPI-applyChanges";
    public static final String OWLAPI_CLASSIFY = "OWLAPI-classify";
    public static final String OWLAPI_DISPOSE_REASONER ="OWLAPI-disposeReasoner";
    public static final String OWLAPI_INIT = "OWLAPI-init";

    public static final String OWLAPI_ADDAXIOM = "OWLAPI-AddAxiom";
    public static final String OWLAPI_REMOVEAXIOM = "OWLAPI-RemoveAxiom";

    public static final String OWLAPI_GET_ANCESTOR_CLASSES ="OWLAPI-getAncestorClasses";
    public static final String OWLAPI_GET_ANCESTOR_PROPERTIES = "OWLAPI-getAncestorProperties";
    public static final String OWLAPI_GET_DESCENDANT_CLASSES ="OWLAPI-getDescendantClasses";
    public static final String OWLAPI_GET_DESCENDANT_PROPERTIES = "OWLAPI-getDescendantProperties";
    public static final String OWLAPI_GET_DOMAINS = "OWLAPI-getDomains";
    public static final String OWLAPI_GET_EQUIVALENT_CLASSES ="OWLAPI-getEquivalentClasses";
    public static final String OWLAPI_GET_EQUIVALENT_PROPERTIES = "OWLAPI-getEquivalentProperties";
    public static final String OWLAPI_GET_INDIVIDIDUALS = "OWLAPI-getIndividuals";
    public static final String OWLAPI_GET_INSTANCES = "OWLAPI-getInstances";
    public static final String OWLAPI_GET_INVERSE_PROPERTIES = "OWLAPI-getInverseProperties";
    public static final String OWLAPI_GET_RANGES = "OWLAPI-getRanges";
    public static final String OWLAPI_GET_RELATEDINDIVIDUALS = "OWLAPI-getRelatedIndividuals";
    public static final String OWLAPI_GET_RELATEDVALUES = "OWLAPI-getRelatedValues";
    public static final String OWLAPI_GET_SAMEINDIVIDUALS = "OWLAPI-getSameIndividuals";
    public static final String OWLAPI_GETSUBCLASSES ="OWLAPI-getSubClasses";
    public static final String OWLAPI_GETSUBPROPERTIES = "OWLAPI-getSubProperties";
    public static final String OWLAPI_GETSUPERCLASSES ="OWLAPI-getSuperClasses";
    public static final String OWLAPI_GETSUPERPROPERTIES = "OWLAPI-getSuperProperties";
    public static final String OWLAPI_GETTYPES = "OWLAPI-getTypes";
    public static final String OWLAPI_HASTYPE = "OWLAPI-hasType";

    public static final String OWLAPI_GETOBJECTPROPERTYRELATIONSHIPS = "OWLAPI-getObjectPropertyRelationships";
    public static final String OWLAPI_HASOBJECTPROPERTYRELATIONSHIP = "OWLAPI-hasObjectPropertyRelationship";
    public static final String OWLAPI_GET_OBJECTPROPERTY_VALUES = "OWLAPI-getObjectPropertyValues";
    public static final String OWLAPI_GET_DATAPROPERTYRELATIONSHIPS = "OWLAPI-getDataPropertyRelationships";
    public static final String OWLAPI_HAS_DATAPROPERTYRELATIONSHIP = "OWLAPI-hasDataPropertyRelationship";
    public static final String OWLAPI_GET_DATAPROPERTY_VALUES = "OWLAPI-getDataPropertyValues";

    public static final String OWLAPI_GET_DECLARATION_AXIOM ="OWLAPI-getOWLImplicitDeclarationAxiom";
                                                              
    public static final String OWLAPI_GET_DISJOINTCLASSES_AXIOM = "OWLAPI-getOWLDisjointClassesAxiom";
    public static final String OWLAPI_GET_DISJOINTUNTION_AXIOM = "OWLAPI-getOWLDisjointUnionAxiom";
    public static final String OWLAPI_GET_EQUIVALENTCLASSES_AXIOM = "OWLAPI-getOWLEquivalentClassesAxiom";
    public static final String OWLAPI_GET_OWLSUBCLASS_AXIOM = "OWLAPI-getOWLSubClassAxiom";
    public static final String OWLAPI_GET_OWLOBJECTSUBPROPERTY_AXIOM = "OWLAPI-getOWLObjectSubPropertyAxiom";
    public static final String OWLAPI_GET_OWLDATAPROPERTYASSERTION_AXIOM = "OWLAPI-getOWLDataPropertyAssertionAxiom";
    public static final String OWLAPI_GET_OWLDATASUBPROPERTY_AXIOM = "OWLAPI-getOWLDataSubPropertyAxiom";
    public static final String OWLAPI_GET_OWLNEGATIVEPDATAROPERTYASSERTION_AXIOM ="OWLAPI-getOWLNegativeDataPropertyAssertionAxiom";
    public static final String OWLAPI_GET_OWLDATAPROPERTYRANGE_AXIOM = "OWLAPI-getOWLDataPropertyRangeAxiom";
    public static final String OWLAPI_GET_OWLOBJECTPROPERTYDOMAIN_AXIOM = "OWLAPI-getOWLObjectPropertyDomainAxiom";
    public static final String OWLAPI_GET_OWLTRANSITIVEOBJECTPROPERTY_AXIOM = "OWLAPI-getOWLTransitiveObjectPropertyAxiom";
    public static final String OWLAPI_GET_OWLSYMMETRICOBJECTPROPERTY_AXIOM = "OWLAPI-getOWLSymmetricObjectPropertyAxiom";
    public static final String OWLAPI_GET_OWLINVERSEOBJECTPROPERTIES_AXIOM = "OWLAPI-getOWLInverseObjectPropertiesAxiom";
    public static final String OWLAPI_GET_OWLOBJECTPROPERTYRANGE_AXIOM = "OWLAPI-getOWLObjectPropertyRangeAxiom";
    public static final String OWLAPI_GET_OWLNEGATIVEDATAPROPERTYASSERTION_AXIOM = "OWLAPI-getOWLNegativeDataPropertyAssertionAxiom";
    public static final String OWLAPI_GET_DISJOINTOBJECTPROPERTY_AXIOM = "OWLAPI-getOWLDisjointObjectPropertiesAxiom";
    public static final String OWLAPI_GET_ASYMMETRICPROPERTY_AXIOM = "OWLAPI-getOWLAsymmetricObjectPropertyAxiom";
    public static final String OWLAPI_GET_REFLEXIVEPROPERTY_AXIOM = "OWLAPI-getOWLReflexiveObjectPropertyAxiom";
    public static final String OWLAPI_GET_FUNCTIONALOBJECTPROPERTY_AXIOM = "OWLAPI-getOWLFunctionalObjectPropertyAxiom";
    public static final String OWLAPPI_GET_IRREFLEXIVEOBJECTPROPERTY_AXIOM ="OWLAPI-getOWLIrreflexiveObjectPropertyAxiom";
    public static final String OWLAPI_GET_INVERSEFUNCTIONALOBJECTPROPERTY_AXIOM = "OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom";
    public static final String OWLAPI_GET_OBJECTPROPERTYCHAINSUBPROPERTY_AXIOM = "OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom";
    public static final String OWLAPI_GET_EQUIVALENTOBJECTPROPERTIES_AXIOM = "OWLAPI-getOWLEquivalentObjectPropertiesAxiom";



    public static final String OWLAPI_GET_DATAPROPERTYDOMAIN_AXIOM = "OWLAPI-getOWLDataPropertyDomainAxiom";
    public static final String OWLAPI_GET_DISJOINTDATAPROPERTY_AXIOM = "OWLAPI-getOWLDisjointDataPropertiesAxiom";
    public static final String OWLAPI_GET_FUNCTIONALDATAPROPERTY_AXIOM = "OWLAPI-getOWLFunctionalDataPropertyAxiom";
    public static final String OWLAPI_GET_EQUIVALENTDATAPROPERTY_AXIOM = "OWLAPI-getOWLEquivalentDataPropertiesAxiom";

    public static final String OWLAPI_GET_CLASSASSERTION_AXIOM = "OWLAPI-getOWLClassAssertionAxiom";
    public static final String OWLAPI_GET_OBJECTPROPERTYASSERTION_AXIOM = "OWLAPI-getOWLObjectPropertyAssertionAxiom";
    public static final String OWLAPI_GET_NEGATIVEOBJECTPROPERTYASSERTION_AXIOM = "OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom";
    public static final String OWLAPI_GET_SAMEINDIVIDUALAS_AXIOM = "OWLAPI-getOWLSameIndividualsAxiom";
    public static final String OWLAPI_GET_DIFFERENTINDIVIDUALS_AXIOM = "OWLAPI-getOWLDifferentIndividualsAxiom";

    public static final String OWLAPI_GET_ENTITYANNOTATION_AXIOM = "OWLAPI-getOWLEntityAnnotationAxiom";
    public static final String OWLAPI_GET_AXIOMANNTION_AXIOM = "OWLAPI-getOWLAxiomAnnotationAxiom";
    public static final String OWLCLASS = "OWLClass";
    public static final String DATATYPE = "Datatype";
    public static final String OBJECTPROPERTY = "ObjectProperty";
    public static final String DATAPROPERTY = "DataProperty";
    public static final String INDIVIDUAL = "Individual";
    public static final String ANNOTATION = "Annotation";

    public static final String OWLAPI_IS_ANTISYMMETRIC = "OWLAPI-isAntiSymmetric";
    public static final String OWLAPI_ISCLASSIFIED = "OWLAPI-isClassified";
    public static final String OWLAPI_ISDEFINED_CLASS = "OWLAPI-isDefinedClass";
    public static final String OWLAPI_ISDEFINED_DATAPROPERTY = "OWLAPI-isDefinedDataProperty";
    public static final String OWLAPI_ISDEFINED_INDIVIDUAL = "OWLAPI-isDefinedIndividual";
    public static final String OWLAPI_ISDEFINED_OBJECTPROPERTY= "OWLAPI-isDefinedObjectProperty";
    public static final String OWLAPI_ISEQUIVLANT_CLASS= "OWLAPI-isEquivalentClass";
    public static final String OWLAPI_IS_FUNCTIONAL = "OWLAPI-isFunctional";
    public static final String OWLAPI_IS_INVERSEFUNCTIONAL = "OWLAPI-isInverseFunctional";
    public static final String OWLAPI_ISSUBCLASSOF = "OWLAPI-isSubClassOf";
    public static final String OWLAPI_ISREALIZED = "OWLAPI-isRealised";
    public static final String OWLAPI_IS_IRREFLEXIVE = "OWLAPI-isIrreflexive";
    public static final String OWLAPI_IS_REFLEXIVE = "OWLAPI-isReflexive";
    public static final String OWLAPI_IS_SYMMETRIC = "OWLAPI-isSymmetric";
    public static final String OWLAPI_IS_TRANSITIVE = "OWLAPI-isTransitive";
    public static final String OWLAPI_IS_SAMEINDIVIDUAL = "OWLAPI-isSameIndividual";

    public static final String OWLAPI_LOADONTOLOGY = "OWLAPI-loadOntology";
    public static final String OWLAPI_NEW_ONTOLOGY = "OWLAPI-newOntology";
    public static final String OWLAPI_NEW_REASONER = "OWLAPI-newReasoner";
    public static final String OWLAPI_REALIZE = "OWLAPI-realize";
    public static final String OWLAPI_SETCURRENT_REASONER = "OWLAPI-setCurrentReasoner";
    public static final String OWLAPI_UNLOAD_ONTOLOGY ="OWLAPI-unloadOntology";
    public static final String OWLAPI_AUTOAPPLYCHANGES = "OWLAPI-autoApplyChanges";
    public static final String OWLAPI_AUTOADDAXIOMS ="OWLAPI-autoAddAxiomsTo";
    public static final String OWLAPI_AUTOREMOVEAXIOMS ="OWLAPI-autoRemoveAxiomsFrom";
    public static final String OWLAPI_AUTOBATCHREMOVEAXIOMSFROM = "OWLAPI-autoBatchRemoveAxiomsFrom";
    public static final String OWLAPI_BATCHSYNCHONIZE = "OWLAPI-batchSynchronize";
    public static final String OWLAPI_SETAXIOMXCOUNTER = "OWLAPI-setAxiomCounter";
    public static final String OWLAPI_GETAXIOMXCOUNTER = "OWLAPI-getAxiomCounter";

    public static final String OWLAPI_DBASETYPE = "d-base-type";
    public static final String OWLAPI_DLITERAL = "d-literal";
    public static final String DFACET = "d-facet";
    public static final String DCOMPLEMENT = "d-complement";
    public static final String DDATARANGE ="d-datarange";
    public static final String DPOSSIBLEVALUES = "d-possible-values ";
    public static final String DALL ="d-all";
    public static final String DSOME ="d-some";

    public static final String OWLAPI_ISENTAILED = "OWLAPI-isEntailed?";

    public static final String ABORT = "abort";
    public static final String GETPROGRESSINDICATOR ="get-progress-indicator";
    public static final String ISPROGRESSCERTAIN = "progress-certain?";

    public static final String OWLAPI_ABORT = "OWLAPI-abort";
    public static final String OWLAPI_GET_PROGRESS = "OWLAPI-getProgress";
    public static final String OWLAPI_PROGRESS_CERTAIN = "OWLAPI-progressCertain?";
    public static final String OWLAPI_GET_CURRENT_REQUEST = "OWLAPI-getCurrentRequest";

    public static final String OWLAPI_SEQUENCE = "owlapi-sequence";

    
}
