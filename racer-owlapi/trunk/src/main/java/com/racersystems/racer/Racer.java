package com.racersystems.racer;

import org.semanticweb.owlapi.reasoner.OWLReasoner;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Reasoner interface for the RACER reasoning engine. It provides
 * additional, RACER-specific methods.
 *
 * @author Olaf Noppens
 */
public interface Racer extends OWLReasoner {

    /**
     * Sends a native racer command.
     * Note: end the string with a newline character.
     * <blockquote>
     * If you are using racer commands that require a tbox or abox name
     * use {@link #getKnowledgeBaseName()} to get the name of the tbox (
     * which is the same name for the abox).
     * Note that some commands are related to the default knowledge base.
     * If you like to answer queries wrt. to this reasoner's knowledge base
     * either set the parameter  <code>inKBase</code> to <code>true</code>
     * or manually send an <code>(in-knowledge-base r :init nil)</code>
     * command (r is the name returned by {@link #getKnowledgeBaseName()}).
     * </blockquote>
     * <b>
     * Note that changes to the abox resp. tbox are not synchronized
     * with the reasoner object and might result in incorrect or
     * erroneous results or might cause exceptions!
     * </b>
     *
     * @param command
     * @param inKBase
     * @return
     */
    public String sendNativeCommand(String command, boolean inKBase);

    /**
     * Returns the name of the knowledge base associated with this reasoner object.
     * <blockquote>
     * The knowledge base name corresponds to the tbox and abox used for
     * this reasoner object in racer.
     * </blockquote>
     *
     * @return knowledge base name
     */
    public String getKnowledgeBaseName();

    /**
     * Checks to see if the the union of loaded ontologies is consistent.
     *
     * @return <code>true</code> if the union of loaded ontologies is consistent,
     *         or <code>false</code> if the union of loaded ontologies is not consistent.
     */
    public boolean isConsistent();

}
