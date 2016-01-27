package com.racersystems.protege;

import com.racersystems.protege.update.RacerUpdateInfo;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.Version;
import org.protege.editor.core.BundleManager;

import java.io.File;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * The <code>RacerInstalledBinaryInfo</code> class encapsulates information about the
 * directories and executables of the racer binary.
 * <p/>
 * Author: Olaf Noppens
 * Date: 11.10.2010
 */
public class RacerInstalledBinaryInfo {
    private static final String racerWindowsBinaryName = "RacerPro.exe";
    private static final String racerLinuxBinaryName = "RacerPro";
    private static final String racerMacOsXBinaryName = "RacerPro";

    private static final String windowsDirectoryName = "win";
    private static final String linuxDirectoryName = "linux";
    private static final String macOsXDirectoryName = "mac";

    private static final String racerDirectoryName = "racerPro";

    private File binaryAbsoluteFile;
    private File binaryDirectory;

    private static RacerInstalledBinaryInfo theInstance;
    private Version version;
    private String osID;

    public static RacerInstalledBinaryInfo getInstance() {
        if (theInstance == null) {
            throw new NullPointerException("RacerInstalledBinaryInfo has not been initialized");
        }
        return theInstance;
    }

    protected RacerInstalledBinaryInfo(BundleContext context) {
        final String osName = context.getProperty(Constants.FRAMEWORK_OS_NAME);
        final String processor = context.getProperty(Constants.FRAMEWORK_PROCESSOR);
        File pluginsFolder = new File(System.getProperty(BundleManager.BUNDLE_DIR_PROP));
        String directoryName = pluginsFolder.getAbsoluteFile().getParentFile().getAbsolutePath() + File.separator + "racerPro" + File.separator;
        createDirectory(directoryName);

        osID = "";
        if ("macos".equalsIgnoreCase(osName) || ("macosx".equalsIgnoreCase(osName))) {
            osID += macOsXDirectoryName;
        } else if ("linux".equalsIgnoreCase(osName)) {
            osID += linuxDirectoryName;
        } else if (osName.startsWith("win")) {
            osID += windowsDirectoryName;
        }
        if ("x86".equals(processor)) {
            osID += "32";
        } else if ("powerpc".equalsIgnoreCase(processor))
            osID += "32";
        else if ("Sparc".equals(processor))
            osID += 32;
        else
            osID += "64";
        directoryName += osID + File.separator;
        createDirectory(directoryName);
        this.binaryDirectory = new File(directoryName);

        File updateFile = new File(directoryName + RacerUpdateInfo.UPDATE_INFO_NAME);
        //System.out.println("updateFile " + updateFile.toString());

        try {
            RacerUpdateInfo info = RacerUpdateInfo.read(updateFile.toURI().toURL());
            this.version = info.getAvailableVersion();
           // System.out.println("installed File " + version);
        } catch (Exception e) {

            //no version is available!
        }

        if ("macos".equalsIgnoreCase(osName) || ("macosx".equalsIgnoreCase(osName))) {
            directoryName += racerMacOsXBinaryName;
        } else if ("linux".equalsIgnoreCase(osName)) {
            directoryName += racerLinuxBinaryName;
        } else if (osName.startsWith("win")) {
            directoryName += racerWindowsBinaryName;
        }
        this.binaryAbsoluteFile = new File(directoryName);
        this.binaryAbsoluteFile.setExecutable(true);
        
        RacerInstalledBinaryInfo.theInstance = this;
    }

    /**
     * Returns an identifier for the current operating system and processor architecture.
     * For example, windows64
     *
     * @return identifier for the current operation system and processor architecture.
     */
    public String getOSIdentifier() {
        return osID;
    }

    private final void createDirectory(String directory) {
        File file = new File(directory);
        if (!file.exists())
            file.mkdir();
    }

    /**
     * Returns the file name of the binary racer file. If the binary racer file is not available, the
     * expected name will be returned.
     *
     * @return absolute file name of the binary
     */
    public File getBinaryAbsoluteFile() {
        return this.binaryAbsoluteFile;
    }

    /**
     * Returns <code>true</code> if the binary racer file is available.
     * Actually it is a shortcut for {@link #getBinaryAbsoluteFile().exists()}.
     *
     * @return <code>true</code> if the binary racer file is available, otherwise <code>false</code>
     */
    public boolean isBinaryFileAvailable() {
        return this.binaryAbsoluteFile.exists();
    }

    /**
     * Returns the installed version of the binary or <code>null</code> if there is no version installed.
     *
     * @return installed version or <code>null</code> if no version information has been found
     */
    public Version getInstalledBinaryVersion() {
        return this.version;
    }

    /**
     * Returns the directory where the binary should be installed or is expected to be installed.
     *
     * @return directory where the binary is exprected to be installed.
     */
    public File getBinaryInstallationDirectory() {
        return this.binaryDirectory;
    }

}
