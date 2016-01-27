package com.racersystems.protege;

import com.racersystems.protege.update.UpdateChecker;
import org.protege.editor.core.plugin.DefaultPluginActivator;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 07.10.2010
 */
public class PluginActivator extends DefaultPluginActivator {

    @Override
    public void start(org.osgi.framework.BundleContext context) throws Exception {
        super.start(context);
        new RacerProInstalledBinaryInfo(context);
        try {
            UpdateChecker checker = new UpdateChecker(context.getBundle());
            checker.checkForUpdate();
        } catch (Exception e) {
            //an exception occurs, could not be checked.
            // e.printStackTrace();
        }
        new RacerProInstalledBinaryInfo(context);

    }
}
