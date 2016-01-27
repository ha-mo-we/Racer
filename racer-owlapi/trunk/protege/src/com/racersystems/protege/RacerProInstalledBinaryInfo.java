package com.racersystems.protege;

import com.racersystems.protege.update.RacerProUpdateInfo;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.framework.Version;
import org.protege.common.CommonProtegeProperties;
import org.protege.editor.core.BundleManager;
import org.protege.editor.core.plugin.PluginUtilities;

import java.io.File;
import java.io.IOException;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * The <code>RacerInstalledBinaryInfo</code> class encapsulates information about the
 * directories and executables of the racer binary.
 * <p/>
 * Author: Olaf Noppens
 * Date: 11.10.2010
 */
public class RacerProInstalledBinaryInfo {
    public enum OSTYPE {
        WIN, LINUX, MAC, OTHER
    }

    public static final String racerWindowsBinaryName = "RacerPro.exe";
    public static final String racerLinuxBinaryName = "RacerPro";
    public static final String racerMacOsXBinaryName = "RacerPro";

    private static final String windowsDirectoryName = "win";
    private static final String linuxDirectoryName = "linux";
    private static final String macOsXDirectoryName = "mac";

    private static final String racerDirectoryName = "RacerPro";

    private File binaryAbsoluteFile;
    private File binaryDirectory;

    private static RacerProInstalledBinaryInfo theInstance;
    private Version version;
    private String osID;
    private OSTYPE osType;

    public static RacerProInstalledBinaryInfo getInstance() {
        if (theInstance == null) {
            throw new NullPointerException("RacerInstalledBinaryInfo has not been initialized");
        }
        return theInstance;
    }

   /* private boolean isP42Something() {
        BundleContext applicationContext = PluginUtilities.getInstance().getApplicationContext();
        System.out.println(applicationContext == null);
        Bundle application = applicationContext.getBundle();
        Version v = PluginUtilities.getBundleVersion(application);
        return v.getMajor() == 4 && v.getMinor() >= 2;
    } */

    protected RacerProInstalledBinaryInfo(BundleContext context) {
        final String osName = context.getProperty(Constants.FRAMEWORK_OS_NAME);
        final String processor = context.getProperty(Constants.FRAMEWORK_PROCESSOR);
        File pluginsFolder = new File(System.getProperty(BundleManager.BUNDLE_DIR_PROP));
        String directoryName = pluginsFolder.getAbsoluteFile().getParentFile().getAbsolutePath() + File.separator + racerDirectoryName + File.separator;


        String programDirectory = pluginsFolder.getAbsoluteFile().getParentFile().getAbsolutePath() + File.separator + racerDirectoryName;
        File file = new File(programDirectory);
        //System.out.println("trying " + programDirectory);
        boolean ok = true;
        try {
            if (!file.exists()) {
                ok = file.mkdir();
                //System.out.println("!file.exists " + ok);
            } else {
                File tmpFile = File.createTempFile("lock", "tmp", file);
                tmpFile.deleteOnExit();
                //System.out.println("creatred tmp File " + tmpFile.getPath());
            }
        } catch (SecurityException se) {
            ok = false;
        } catch (IOException e) {
            ok = false;
        }
        if (ok) {
            directoryName = programDirectory + File.separator;
        }
        else  {
            try {
            //we have to try the user plugin directory
            File userPluginsFolder = CommonProtegeProperties.getUserPluginDirectory();
            String userDirectory = userPluginsFolder.getAbsoluteFile().getParentFile().getAbsolutePath() + File.separator + racerDirectoryName;
            ok = true;
            try {
                file = new File(userDirectory);
                //System.out.println("trying " + userDirectory);
                if (!file.exists()) {
                    ok = file.mkdir();
                    //System.out.println("!file.exists " + ok);
                } else {
                    File tmpFile = File.createTempFile("lock", "tmp", file);
                    tmpFile.deleteOnExit();
                     //System.out.println("creatred tmp File " + tmpFile.getPath());
                }
            } catch (SecurityException e) {
                ok = false;

            } catch (IOException e) {
                ok = false;
            }
            if (ok) {
                directoryName = userDirectory + File.separator;
            }
            } catch (NoSuchMethodError error) {
                //it is not P4.2.x
            }
        }
        createDirectory(directoryName);
        osID = "";
        if ("macos".equalsIgnoreCase(osName) || ("macosx".equalsIgnoreCase(osName))) {
            osID += macOsXDirectoryName;
            osType = OSTYPE.MAC;
        } else if ("linux".equalsIgnoreCase(osName)) {
            osID += linuxDirectoryName;
            osType = OSTYPE.LINUX;
        } else if (osName.startsWith("win")) {
            osID += windowsDirectoryName;
            osType = OSTYPE.WIN;
        } else
            osType = OSTYPE.OTHER;
        if ("x86".equals(processor)) {
            osID += "32";
        } else if ("powerpc".equalsIgnoreCase(processor))
            osID += "32";
        else if ("Sparc".equals(processor))
            osID += "32";
        else
            osID += "64";
        directoryName += osID + File.separator;
        createDirectory(directoryName);
        this.binaryDirectory = new File(directoryName);

        File updateFile = new File(directoryName + RacerProUpdateInfo.UPDATE_INFO_NAME);


        try {
            RacerProUpdateInfo info = RacerProUpdateInfo.read(updateFile.toURI().toURL());
            this.version = info.getAvailableVersion();

        } catch (Exception e) {
        }

        if ("macos".equalsIgnoreCase(osName) || ("macosx".equalsIgnoreCase(osName))) {
            directoryName += racerMacOsXBinaryName;
        } else if ("linux".equalsIgnoreCase(osName)) {
            directoryName += racerLinuxBinaryName;
        } else if (osName.startsWith("win")) {
            directoryName += racerWindowsBinaryName;
        }
        this.binaryAbsoluteFile = new File(directoryName);
        //System.out.println(binaryAbsoluteFile);
        //this.binaryAbsoluteFile.setExecutable(true);
        if (osType != OSTYPE.WIN) {
            try {
                Runtime.getRuntime().exec("chmod 755 " + binaryAbsoluteFile.toString());
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        RacerProInstalledBinaryInfo.theInstance = this;
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

    /**
     * Returns the type of the current operating system (e.g., WIN, LINUX, MAC).
     *
     * @return OSTYPE identifier
     * @see RacerProInstalledBinaryInfo.OSTYPE
     */
    public OSTYPE getOSType() {
        return this.osType;
    }


    private void CheckDirectoryAccess() {

    }


    private final boolean createDirectory(String directory) {
        File file = new File(directory);
        if (!file.exists())
            return file.mkdir();
        return true;
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
        //System.out.println(binaryAbsoluteFile + " " + binaryAbsoluteFile.exists());
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
