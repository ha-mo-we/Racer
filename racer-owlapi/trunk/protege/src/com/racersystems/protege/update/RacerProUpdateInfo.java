package com.racersystems.protege.update;

import com.racersystems.protege.RacerProInstalledBinaryInfo;
import org.osgi.framework.Version;

import java.io.BufferedInputStream;
import java.net.URL;
import java.util.Properties;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Encapsulates information about version and download-url for a racer binary.
 * Author: Olaf Noppens
 * Date: 11.10.2010
 */
public class RacerProUpdateInfo {
    public static final String UPDATE_INFO_NAME = "update.properties";
    public static final String VERSION_PROP_KEY = "version";
    public static final String DOWNLOAD_PROP_KEY = "download-url";
    private static final String OSID_PREFIX = "-";
    private Version availableVersion;
    private URL downloadURL;

    public RacerProUpdateInfo(Version version, URL downloadURL) {
        this.availableVersion = version;
        this.downloadURL = downloadURL;
    }

    public Version getAvailableVersion() {
        return this.availableVersion;
    }

    public URL getDownloadURL() {
        return this.downloadURL;
    }

    /**
     * Creates a RacerUpdateInfo from a property file accessible at the given parameter.
     * The property file needs to specify at least the following keys:
     * <ul>
     * <li>{@link #VERSION_PROP_KEY}</li>
     * <li>{@link #DOWNLOAD_PROP_KEY}</li>
     * </ul>
     * The <i>version</i> identifies the available version and the <i>download-url</i> return a generic URL where
     * to find a binary zip file for the mentioned version. <br/>
     * From this generic URL we will determine the actual URL for downloading the zip file as follows:
     * If the zip file ends with ".zip" we will add the OSIdentifier (see
     * {@link com.racersystems.protege.RacerProInstalledBinaryInfo#getOSIdentifier()})
     * before the ending, otherwise we add the OSIdentifier and ".zip".
     * For example, let <i>http://www.example.org/racer-1.2.3.zip</i> be the download-url. If the OSIdentifier is
     * <i>win64</i> (i.e., the system is actually running on a windows 64bit machine) then the zip file will
     * be exprected at <i>http://www.example.org/racer-1.2.3{@link #OSID_PREFIX}win64.zip</i>. By default
     * the {@link #OSID_PREFIX} is "-".
     * <p/>
     * It is assumed that the zip file contains the binary in root directory (and not in any subdirectories!)
     *
     * @param updateFileLocation Location of the update file
     * @return RacerUpdateInfo
     * @throws Exception if an exception occurs during acessing the update file. This can be, for instance, an
     *                   IOException when reading the file or an UnknownHostException when there is the remote file
     *                   not available.
     */
    public static RacerProUpdateInfo read(URL updateFileLocation) throws Exception {
        Properties properties = new Properties();
        properties.load(new BufferedInputStream(updateFileLocation.openStream()));
        //read the version
        String versionString = properties.getProperty(VERSION_PROP_KEY);
        Version version = null;
        if (versionString != null)
            version = new Version(versionString);
        String downloadString = properties.getProperty(DOWNLOAD_PROP_KEY);
        URL downloadURL = null;
        if (downloadString != null) {
            //the downloadURL represents a zip file.
            //but we have to add the osIdentifier, e.g. -win64
            int index = downloadString.lastIndexOf(".zip");
            if (index > -1) {
                downloadString = downloadString.substring(0, index) + OSID_PREFIX +
                        RacerProInstalledBinaryInfo.getInstance().getOSIdentifier() + ".zip";
            } else {
                downloadString += OSID_PREFIX + RacerProInstalledBinaryInfo.getInstance().getOSIdentifier() + ".zip";
            }
            downloadURL = new URL(downloadString);
            //System.out.println(downloadURL.toString());
        }
        RacerProUpdateInfo info = new RacerProUpdateInfo(version, downloadURL);
        return info;

    }



}
