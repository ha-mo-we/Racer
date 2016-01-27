package com.racersystems.protege;

import com.racersystems.protege.preferences.RacerPreferences;
import com.racersystems.protege.update.UpdateChecker;
import org.protege.editor.core.plugin.DefaultPluginActivator;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 07.10.2010
 */
public class RacerPluginActivator extends DefaultPluginActivator {

    @Override
    public void start(org.osgi.framework.BundleContext context) throws Exception {
        super.start(context);
        new RacerInstalledBinaryInfo(context);
        if (RacerPreferences.getInstance().isAutoUpdateEnabled()) {
            try {
                UpdateChecker checker = new UpdateChecker(context.getBundle());
                checker.checkForUpdate();
            } catch (Exception e) {
                //an exception occurs, could not be checked.
                e.printStackTrace();
            }
            new RacerInstalledBinaryInfo(context);
        }
    }
}
