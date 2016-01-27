package com.racersystems.protege.update;

import com.racersystems.protege.RacerProInstalledBinaryInfo;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

import javax.swing.*;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.MalformedURLException;
import java.net.URL;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 11.10.2010
 */
public class UpdateChecker {
    private static final String UPDATE_URL_KEY = "racer-bin-update-URL";
    private static final String REQUIRED_VERSION_KEY = "required-racer-version";
    protected URL updateInfoLocationURL;
    protected Version requiredVersion;

    public UpdateChecker(Bundle bundle) {
        String updateLocation = (String) bundle.getHeaders().get(UPDATE_URL_KEY);
        try {
            this.updateInfoLocationURL = new URL(updateLocation);
        } catch (MalformedURLException e) {
            e.printStackTrace();
        }
        String requiredVersion = (String) bundle.getHeaders().get(REQUIRED_VERSION_KEY);
        if (requiredVersion != null)
            this.requiredVersion = Version.parseVersion(requiredVersion);
        else
            this.requiredVersion = Version.parseVersion("2.0.0"); //this is our minimal version
    }

    /**
     * Performs an update check. If a newer version is available it will be downloaded
     * and installed.
     *
     * @return <code>true</code> if a new version is available and successfully downloaded
     *         and installed.
     * @throws IOException
     */
    public boolean checkForRemoteUpdate() throws Exception {
        RacerProUpdateInfo remoteInfo = RacerProUpdateInfo.read(updateInfoLocationURL);
        RacerProInstalledBinaryInfo installedInfo = RacerProInstalledBinaryInfo.getInstance();
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

    public boolean checkForUpdate() throws Exception {
        RacerProInstalledBinaryInfo installedInfo = RacerProInstalledBinaryInfo.getInstance();
        if (installedInfo == null) return false; //something went totally wrong
        if (installedInfo.isBinaryFileAvailable()) {
            if (requiredVersion.compareTo(installedInfo.getInstalledBinaryVersion()) <= 0) {
                //we have the required version. No need to update anything.
                return false;
            } else {
                if (JOptionPane.NO_OPTION == JOptionPane.showConfirmDialog(null,
                        //"An update for the RacerPro binary files is available.\nDo you want to download it?",
                        "The installed RacerPro binary files are outdated.\n" +
                                "Do you want to download a new version now?\n" +
                                "(Otherwise you can only use an external RacerPro binary file.)",
                        "RacerPro Plugin", JOptionPane.YES_NO_OPTION)) {
                    return false;
                }
                //download
                //return true;
            }
        } else {
            if (JOptionPane.NO_OPTION == JOptionPane.showConfirmDialog(null,
                    "No RacerPro reasoner file is installed.\n" +
                            "Do you want to download the reasoner file now?",
                    "RacerPro Plugin", JOptionPane.YES_NO_OPTION)) {
                return false;
            }
            //download
            //return true

        }
        try {
            RacerProUpdateInfo remoteInfo = RacerProUpdateInfo.read(updateInfoLocationURL);
            if (remoteInfo.getAvailableVersion().compareTo(requiredVersion) < 0) {
                //there is a problem because RacerPro only offers an older version than required!
                //System.out.println("remoteInfo " + remoteInfo.getAvailableVersion() + " < required info " + requiredVersion);
                JOptionPane.showMessageDialog(null, "No valid RacerPro binary file could be found.\n" +
                        "Please contact racersystems.", "No valid RacerPro binary file", JOptionPane.ERROR_MESSAGE);
                return false;
            }
            UpdateInstaller installer = new UpdateInstaller();
            installer.downloadFile(remoteInfo);
        } catch (InterruptedIOException e) {
            JOptionPane.showMessageDialog(null, "Installation of Racer binary has been cancelled by the user",
                    "RacerPro installation cancelled", JOptionPane.ERROR_MESSAGE);
            throw e;
        } catch (Exception e) {
            e.printStackTrace();
            JOptionPane.showMessageDialog(null, "Racer could not be downloaded or installed.\n" +
                    "Please check your network connection.\nPlease also check whether you have write access to the P4 plugin folder (try running P4 as administrator).", "RacerPro download error", JOptionPane.ERROR_MESSAGE);
            throw e;
        }
        return true;
    }


    
}
