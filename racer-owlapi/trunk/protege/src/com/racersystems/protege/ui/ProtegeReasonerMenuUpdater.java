package com.racersystems.protege.ui;

import org.protege.editor.core.ProtegeManager;
import org.protege.editor.core.editorkit.EditorKit;
import org.protege.editor.core.ui.workspace.WorkspaceFrame;
import org.protege.editor.owl.model.OWLWorkspace;
import org.protege.editor.owl.model.inference.NoOpReasonerInfo;
import org.protege.editor.owl.model.inference.OWLReasonerManager;

import javax.swing.*;
import java.util.HashSet;
import java.util.Set;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * The <code>ProtegeReasonerMenuUpdater</code> tries to update the Menubar of all instances if
 * the Racer binaries are not available.
 * <p/>
 * Author: Olaf Noppens
 * Date: 01.12.2011
 */
public class ProtegeReasonerMenuUpdater {

    private static ProtegeReasonerMenuUpdater INSTANCE = new ProtegeReasonerMenuUpdater();

    public static ProtegeReasonerMenuUpdater getInstance() {
        return INSTANCE;
    }

    public void setRacerReasonerMenuItemenEnabled(boolean enable) {
        Set<OWLWorkspace> workspaces = new HashSet<OWLWorkspace>();
        for (EditorKit kit : ProtegeManager.getInstance().getEditorKitManager().getEditorKits()) {
            if (kit.getWorkspace() instanceof OWLWorkspace) {
                workspaces.add((OWLWorkspace) kit.getWorkspace());
            }
        }
        for (OWLWorkspace workspace : workspaces) {

            WorkspaceFrame frame = ProtegeManager.getInstance().getFrame(workspace);
            JMenu menu = frame.getMenu("Reasoner");
            if (menu != null) {
                JRadioButtonMenuItem noneButton = null;
                for (int i = 0; i < menu.getItemCount(); i++) {
                    JMenuItem item = menu.getItem(i);
                    if (item instanceof JRadioButtonMenuItem) {
                        if (item.getText().equals("RacerPro")) {
                            item.setEnabled(enable);
                        } else if (item.getText().equals("None")) {
                            noneButton = (JRadioButtonMenuItem) item;
                        }
                    }
                }
                if (noneButton != null && !enable)
                    noneButton.setSelected(false);
            }
        }
    }

    public void setNoneReasoner(OWLReasonerManager manager) {
        //the straightforward way does not work in P4 because of a missing model-view-controller implementation
        //manager.setCurrentReasonerFactoryId(NoOpReasonerInfo.NULL_REASONER_ID);
        //instead, we have to manipulate the menu by-hand
        //Caution: might be changed in future versions of P4
        switchToNoneReasoner(manager);

    }

    protected void switchToNoneReasoner(OWLReasonerManager manager) {
        OWLWorkspace owlWorkspac = null;
        for (EditorKit kit : ProtegeManager.getInstance().getEditorKitManager().getEditorKits()) {
            if (kit.getWorkspace() instanceof OWLWorkspace) {
                OWLWorkspace wkspace = (OWLWorkspace) kit.getWorkspace();
                if (wkspace.getOWLModelManager().getOWLReasonerManager() == manager) {
                    owlWorkspac = wkspace;
                    break;
                }
            }
        }
        if (owlWorkspac != null) {
            WorkspaceFrame frame = ProtegeManager.getInstance().getFrame(owlWorkspac);
            JMenu menu = frame.getMenu("Reasoner");
            if (menu != null) {
                JRadioButtonMenuItem noneButton = null;
                for (int i = 0; i < menu.getItemCount(); i++) {
                    JMenuItem item = menu.getItem(i);
                    if (item instanceof JRadioButtonMenuItem) {
                        if (item.getText().equals("None")) {
                            //we have to compare the menu text because of bad programming in P4
                            //it only works if the plugins.xml contains the "None" string and
                            //not a localized version of it!
                            noneButton = (JRadioButtonMenuItem) item;
                            break;
                        }
                    }
                }
                if (noneButton != null)   {
                    noneButton.doClick();
                }
            }
        }
    }
}
