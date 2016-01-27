package com.racersystems.protege.preferences;

import com.racersystems.protege.RacerProInstalledBinaryInfo;
import org.protege.editor.core.prefs.Preferences;
import org.protege.editor.core.prefs.PreferencesManager;

import java.io.File;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * The <code>RacerPreferences</code> are a wrapper around Protege preferences for the Racer
 * reasoner. Some of the prefs are used for creating a {@link org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration},
 * some are used for configuring the Racer binary.
 *
 * @author Olaf Noppens
 */
public class RacerProPreferences {

    private static RacerProPreferences instance;

    private static final String KEY = "com.racersystems.protege";

    private static final String BINARY = "binary";

    private static final String PORT = "port";

    private static final String LOGGING = "loggingEnabled";
    private static final String LOGGING_FILE = "logging";
    private static final String UNSAFE_MODE = "unsafeMode";
    private static final String UTF8 = "utf8";
    private static final String AUTO_UPDATE = "autoupdate";
    private static final String START_RACER_ENABLED_KEY = "startRacerEnabled";
    private static final String INTEGRATED_RACER_ENABLED_KEY = "integratedRacerEnabled";
    private static final String EXTERNAL_RACER_BINARY_PATH_KEY = "externalRacerBinaryPath";
    private static final String REMOTE_RACER_ADDRESS_KEY = "remoteRacerAddress";
    private static final String REMOTE_RACER_PORT_KEY = "removeRacerPort";
    private static final String LOCAL_RACER_PORT_KEY = "localRacerPort";
    private static final String DISPOSE_REASONER_ON_CLOSE = "disposeReasonerOnClose";
    private static final String DISABLE_ABOX_REALIZATION = "disableABoxRealization";
    private static final String DISABLE_ABOX_CONSISTENCY_TEST = "disableABoxConsistencyTest";
    private static final String ENABLE_MEMORY_SAVING_MODE = "enableMemorySavingMode";
    private static final String ENABLE_TERMINALWINDOW = "enableTerminalWindow";
    private static final String ENABLE_TERMINALWINDO_NAVIGATION = "enableNavigationInTerminalWindow";


    public static synchronized RacerProPreferences getInstance() {
        if (instance == null) {
            instance = new RacerProPreferences();
        }
        return instance;
    }

    private Preferences getPrefs() {
        return PreferencesManager.getInstance().getApplicationPreferences(KEY);
    }


    public String getDirectory() {
        return getPrefs().getString(BINARY, "");
    }

    public void setDirectory(String directory) {
        getPrefs().putString(BINARY, directory);
    }

    public int getPort() {
        return getPrefs().getInt(PORT, 8088);
    }

    public void setPort(int port) {
        getPrefs().putInt(PORT, port);
    }

    public void setLoggingEnabled(boolean logging) {
        getPrefs().putBoolean(LOGGING, logging);
    }

    public boolean isLoggingEnabled() {
        return getPrefs().getBoolean(LOGGING, false);
    }

    public void setLoggingDirectory(String file) {
        if (file == null)
            file = "";
        getPrefs().putString(LOGGING_FILE, file);
    }

    public String getLoggingDirectory() {
        String file = new String(getPrefs().getString(LOGGING_FILE, ""));
        if (file == null || file == "" || file.replace(" ", "").length() == 0) {
            return getDefaultLoggingDirectory();
        }
        return file;
    }

    public String getDefaultLoggingDirectory() {
        switch (RacerProInstalledBinaryInfo.getInstance().getOSType()) {
            case MAC:
                String s = System.getProperty("user.home");
                if (s != null && s.length() > 0) {
                    s += File.separator + "Library" + File.separator + "Logs" + File.separator + "RacerPro";
                    File f = new File(s);
                    if (!f.exists())
                        try {
                            f.mkdir();
                            return s;
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    return s;
                }
                break;
            case WIN:
                s = System.getProperty("user.home");
                if (s != null && s.length() > 0) {
                    s += File.separator + "RacerPro" + File.separator + "Logs";
                    File f = new File(s);
                    if (!f.exists())
                        try {
                            f.mkdir();
                            return s;
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    return s;
                }
                break;
            case LINUX:
                s = System.getProperty("user.home");
                if (s != null && s.length() > 0) {
                    s += File.separator + "RacerPro" + File.separator + "log";
                    File f = new File(s);
                    if (!f.exists())
                        try {
                            f.mkdir();
                            return s;
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    return s;
                }
        }
        return RacerProInstalledBinaryInfo.getInstance().getBinaryInstallationDirectory().getParent();
    }

    public void setUnsafeModeEnabled(boolean enabled) {
        getPrefs().putBoolean(UNSAFE_MODE, enabled);
    }

    public boolean isUnsafeModeEnabled() {
        return getPrefs().getBoolean(UNSAFE_MODE, false);
    }

    public void setUTF8(boolean utf8) {
        getPrefs().putBoolean(UTF8, utf8);
    }

    public boolean isUTF8() {
        return getPrefs().getBoolean(UTF8, true);
    }

    public boolean isAutoUpdateEnabled() {
        return getPrefs().getBoolean(AUTO_UPDATE, true);
    }

    public void setAutoUpdateEnabled(boolean enabled) {
        getPrefs().putBoolean(AUTO_UPDATE, enabled);
    }

    /**
     * Determines whether the RacerPro system is started automatically by Protege or not.
     *
     * @return
     */
    public boolean isStartRacerEnabled() {
        return getPrefs().getBoolean(START_RACER_ENABLED_KEY, true);
    }

    public void setStartRacerEnabled(boolean enabled) {
        getPrefs().putBoolean(START_RACER_ENABLED_KEY, enabled);
    }

    /**
     * Determines whether the internal RacerPro system should be used or a user-defined one.
     *
     * @return
     */
    public boolean isIntegratedRacerEnabled() {
        return getPrefs().getBoolean(INTEGRATED_RACER_ENABLED_KEY, true);
    }

    public void setIntegratedRacerEnabled(boolean enabled) {
        getPrefs().putBoolean(INTEGRATED_RACER_ENABLED_KEY, enabled);
    }

    /**
     * Determines the user-defined path to the external (but local) Racer binary
     *
     * @return
     */
    public String getExternalRacerBinaryPath() {
        return getPrefs().getString(EXTERNAL_RACER_BINARY_PATH_KEY, RacerProInstalledBinaryInfo.getInstance().getBinaryAbsoluteFile().getAbsolutePath());
    }

    /**
     * Sets the absolute path to the external racer pro binary file.
     * If only the directory is given, the binary file name will be added according to the
     * values defined in {@link RacerProInstalledBinaryInfo}.
     * 
     * @param binaryPath
     */
    public void setExternalRacerBinaryPath(String binaryPath) {
        RacerProInstalledBinaryInfo.OSTYPE ostype = RacerProInstalledBinaryInfo.getInstance().getOSType();
        switch (ostype) {
            case WIN:
                if (!binaryPath.endsWith(File.separator + RacerProInstalledBinaryInfo.racerWindowsBinaryName)) {
                    binaryPath = binaryPath + File.separator + RacerProInstalledBinaryInfo.racerWindowsBinaryName;
                }
                break;
            case MAC:
                if (!binaryPath.endsWith(File.separator + RacerProInstalledBinaryInfo.racerMacOsXBinaryName)) {
                    binaryPath = binaryPath + File.separator + RacerProInstalledBinaryInfo.racerMacOsXBinaryName;
                }
                break;
            case LINUX:
            case OTHER:
                if (!binaryPath.endsWith(File.separator + RacerProInstalledBinaryInfo.racerLinuxBinaryName)) {
                    binaryPath = binaryPath + File.separator + RacerProInstalledBinaryInfo.racerLinuxBinaryName;
                }
        }
        getPrefs().putString(EXTERNAL_RACER_BINARY_PATH_KEY, binaryPath);
    }

    /**
     * Returns the IP Address of a remove Racer instance
     *
     * @return
     */
    public String getRemoteRacerAddress() {
        return getPrefs().getString(REMOTE_RACER_ADDRESS_KEY, "127.0.0.1");
    }

    public void setRemoteRacerAddress(String address) {
        getPrefs().putString(REMOTE_RACER_ADDRESS_KEY, address);
    }

    /**
     * Returns the port number of a remove Racer instance
     *
     * @return
     */
    public int getRemoteRacerPort() {
        return getPrefs().getInt(REMOTE_RACER_PORT_KEY, 8088);
    }

    public void setRemoteRacerPort(int port) {
        getPrefs().putInt(REMOTE_RACER_PORT_KEY, port);
    }

    /**
     * Return the port number of the local Racer instance
     *
     * @return
     */
    public int getLocalRacerPort() {
        return getPrefs().getInt(LOCAL_RACER_PORT_KEY, 8088);
    }

    public void setLocalRacerPort(int port) {
        getPrefs().putInt(LOCAL_RACER_PORT_KEY, port);
    }

    public void setDisposeReasonerOnClose(boolean disposeOnClose) {
        getPrefs().putBoolean(DISPOSE_REASONER_ON_CLOSE, disposeOnClose);
    }

    public boolean isDisposeReasonerOnClose() {
        return getPrefs().getBoolean(DISPOSE_REASONER_ON_CLOSE, false);
    }

    public void setDisableABoxRealization(boolean disableABoxRealization) {
        getPrefs().putBoolean(DISABLE_ABOX_REALIZATION, disableABoxRealization);
    }

    public boolean isDisableABoxRealization() {
        return getPrefs().getBoolean(DISABLE_ABOX_REALIZATION, true);
    }

    public void setDisableABoxConsistencyTest(boolean disableAboxConsistencyCheck) {
        getPrefs().putBoolean(DISABLE_ABOX_CONSISTENCY_TEST, disableAboxConsistencyCheck);
    }

    public boolean isDisableABoxConsistencyTest() {
        return getPrefs().getBoolean(DISABLE_ABOX_CONSISTENCY_TEST, true);
    }

    public void setEnableMemorySavingMode(boolean enable) {
        getPrefs().putBoolean(ENABLE_MEMORY_SAVING_MODE, enable);
    }

    public boolean isMemorySavingModeEnabled() {
        return getPrefs().getBoolean(ENABLE_MEMORY_SAVING_MODE, false);
    }

    public void setEnableTerminalWindow(boolean enable) {
        getPrefs().putBoolean(ENABLE_TERMINALWINDOW, enable);
    }

    public boolean isTerminalWindowEnabled() {
        return getPrefs().getBoolean(ENABLE_TERMINALWINDOW, false);
    }

    public void setEnableNavigationInTerminalWindow(boolean enable) {
        getPrefs().putBoolean(ENABLE_TERMINALWINDO_NAVIGATION, enable);
    }

    public boolean isNavigationInTerminalWindowEnabeld() {
        return getPrefs().getBoolean(ENABLE_TERMINALWINDO_NAVIGATION, false);
    }
}

