package com.racersystems.racer;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * The <code>AxiomLoadingStrategy</code> specifies when the axioms of the ontology are
 * transfered to RacerPro:
 * <ul>
 * <li>{@link #LOAD_ON_CREATION}: all axioms are loaded into the reasoner when the reasoner is created, i.e.,
 * during the constructor call.
 * <li>{@link #LOAD_ON_QUERY}: all axioms are loaded into the reasoner when the first query is called or when
 * precompute is called. <blockquote> It is recommended to call <i>precompute</i> before querying the reasoner.</blockquote>
 * <li>
 * <li>{@link #LOAD_ON_PRECOMPUTE}: all axioms are loaded into the reasoner when precompute is
 * called. If it is not called, no axioms are transfered to the reasoner! Use this method, if it is sure
 * that precompute is always called. Otherwise, use the {@link #LOAD_ON_QUERY} strategy.
 * </ul>
 * <p/>
 * Author: Olaf Noppens
 * Date: 03.12.2011
 */
public enum AxiomLoadingStrategy {
    LOAD_ON_CREATION, LOAD_ON_QUERY, LOAD_ON_PRECOMPUTE;
}
