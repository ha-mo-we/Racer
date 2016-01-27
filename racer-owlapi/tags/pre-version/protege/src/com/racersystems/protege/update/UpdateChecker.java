package com.racersystems.protege.update;

import com.racersystems.protege.RacerInstalledBinaryInfo;
import org.osgi.framework.Bundle;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 11.10.2010
 */
public class UpdateChecker {
    private static final String UPDATE_URL_KEY = "racer-bin-update-URL";
    protected URL updateInfoLocationURL;

    public UpdateChecker(Bundle bundle) {
        String updateLocation = (String) bundle.getHeaders().get(UPDATE_URL_KEY);
        try {
            this.updateInfoLocationURL = new URL(updateLocation);
        } catch (MalformedURLException e) {
            e.printStackTrace();
        }
    }

    /**
     * Performs an update check. If a newer version is available it will be downloaded
     * and installed.
     *
     * @return <code>true</code> if a new version is available and successfully downloaded
     *         and installed.
     * @throws IOException
     */
    public boolean checkForUpdate() throws Exception {
        //System.out.println("check for update...");
        RacerUpdateInfo remoteInfo = RacerUpdateInfo.read(updateInfoLocationURL);
        RacerInstalledBinaryInfo installedInfo = RacerInstalledBinaryInfo.getInstance();
        if (remoteInfo == null || installedInfo == null) return false;
        if (installedInfo.isBinaryFileAvailable()) {
          //  System.out.println("installed " + installedInfo.getInstalledBinaryVersion());
          //  System.out.println("available " + remoteInfo.getAvailableVersion());
            if (installedInfo.getInstalledBinaryVersion() != null)
                if (remoteInfo.getAvailableVersion().compareTo(installedInfo.getInstalledBinaryVersion()) <= 0) {
                    return false;
                }
        } else {
           // System.out.println("no binary file available");
        }
        UpdateInstaller installer = new UpdateInstaller();
        installer.downloadFile(remoteInfo);
        return true;
    }
}
