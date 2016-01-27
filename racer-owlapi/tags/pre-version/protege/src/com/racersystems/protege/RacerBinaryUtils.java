package com.racersystems.protege;

import com.racersystems.protege.preferences.RacerPreferences;
import org.protege.editor.core.BundleManager;

import java.io.File;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Created by IntelliJ IDEA.
 * User: noppens
 * Date: 26.05.2009
 * Time: 11:14:36
 * To change this template use File | Settings | File Templates.
 */
public class RacerBinaryUtils {

    public static final boolean osIsMacOsX;
    public static final boolean osIsWindows;
    public static final boolean osIsLinux;

    public static final String racerWindowsBinaryName = "RacerPro.exe";
    public static final String racerLinuxBinaryName = "RacerPro";
    public static final String racerMacOsXBinaryName = "RacerPro";
    public static final String racerBinaryDirectoryname = "racer";

    static {
        String os = System.getProperty("os.name").toLowerCase();
        osIsMacOsX = "mac os x".equals(os);
        osIsWindows = os != null && os.indexOf("windows") != -1;
        osIsLinux = os != null && os.indexOf("linux") != -1;
    }

    public static class BinaryInfo {
        public final File binaryFile;
        public final int port;

        public BinaryInfo(File binaryFile, int port) {
            this.binaryFile = binaryFile;
            this.port = port;
        }
    }

    public static BinaryInfo getRacerBinary() {
        final RacerPreferences prefs = RacerPreferences.getInstance();
        if (prefs.isStartRacerEnabled()) {
            if (prefs.isIntegratedRacerEnabled()) {
                RacerInstalledBinaryInfo info = RacerInstalledBinaryInfo.getInstance();
                return new BinaryInfo(info.getBinaryAbsoluteFile(), prefs.getLocalRacerPort());
            } else {

            }
        } else return null;


        /* //first we check whether  there is a racer specified
final RacerPreferences prefs = RacerPreferences.getInstance();
if (prefs.getDirectory() != null && prefs.getDirectory().length() > 0) {
File file = new File(prefs.getDirectory());
if (file.exists()) {
int port = prefs.getPort();
return new BinaryInfo(file, port);
} else {
System.out.println("file " + file + " does not exists. Trying to locate Racer in P4 directory");
}
}
//second we check whether the binaries can be found in the plugins folder
File pluginsFolder = new File(System.getProperty(BundleManager.BUNDLE_DIR_PROP));
String directory = pluginsFolder.getAbsolutePath();
if (osIsWindows)
directory += File.separator + ".." + File.separator + racerBinaryDirectoryname + File.separator + racerWindowsBinaryName;
else if (osIsLinux)
directory += File.separator + ".." + File.separator + racerBinaryDirectoryname + File.separator + racerLinuxBinaryName;
else if (osIsMacOsX)
directory += File.separator + ".." + File.separator + racerBinaryDirectoryname + File.separator + racerMacOsXBinaryName;
int port = prefs.getPort();
File binaryFile = new File(directory);
if (binaryFile.exists()) {
BinaryInfo info = new BinaryInfo(binaryFile, port);
return info;
}               */
        return null;
    }


    public File getBinaryDirectory() {
        File pluginsFolder = new File(System.getProperty(BundleManager.BUNDLE_DIR_PROP));
        String directory = pluginsFolder.getAbsolutePath();


        if (osIsWindows)
            directory += File.separator + ".." + File.separator + racerBinaryDirectoryname + File.separator + racerWindowsBinaryName;
        else if (osIsLinux)
            directory += File.separator + ".." + File.separator + racerBinaryDirectoryname + File.separator + racerLinuxBinaryName;
        else if (osIsMacOsX)
            directory += File.separator + ".." + File.separator + racerBinaryDirectoryname + File.separator + racerMacOsXBinaryName;
        return null;
    }


}
