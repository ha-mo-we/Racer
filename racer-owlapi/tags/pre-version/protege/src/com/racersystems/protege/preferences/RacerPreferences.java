package com.racersystems.protege.preferences;

import com.racersystems.protege.RacerInstalledBinaryInfo;
import org.protege.editor.core.prefs.Preferences;
import org.protege.editor.core.prefs.PreferencesManager;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * The <code>RacerPreferences</code> are a wrapper around Protege preferences for the Racer
 * reasoner. Some of the prefs are used for creating a {@link org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration},
 * some are used for configuring the Racer binary.
 *
 * @author Olaf Noppens
 */
public class RacerPreferences {

    private static RacerPreferences instance;

    private static final String KEY = "de.uulm.ecs.ai.racer";

    private static final String BINARY = "binary";

    private static final String PORT = "port";

    private static final String LOGGING = "loggingEnabled";
    private static final String LOGGING_FILE = "logging";
    private static final String UNSAFE_MODE = "unsafeMode";
    private static final String UTF8 = "utf8";
    private static final String AUTO_UPDATE = "autoupdate";
    private static final String START_RACER_ENABLED_KEY = "startRacerEnabled";
    public static final String INTEGRATED_RACER_ENABLED_KEY = "integratedRacerEnabled";
    public static final String EXTERNAL_RACER_BINARY_PATH_KEY = "externalRacerBinaryPath";
    public static final String REMOTE_RACER_ADDRESS_KEY = "remoteRacerAddress";
    public static final String REMOTE_RACER_PORT_KEY = "removeRacerPort";
    public static final String LOCAL_RACER_PORT_KEY = "localRacerPort";


    public static synchronized RacerPreferences getInstance() {
        if (instance == null) {
            instance = new RacerPreferences();
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
        boolean b = getPrefs().getBoolean(LOGGING, false);
        return b;
    }

    public void setLoggingFile(String file) {
        getPrefs().putString(LOGGING_FILE, file);
    }

    public String getLoggingFile() {
        return getPrefs().getString(LOGGING_FILE, "");
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
     * @return
     */
    public String getExternalRacerBinaryPath() {
        return getPrefs().getString(EXTERNAL_RACER_BINARY_PATH_KEY, RacerInstalledBinaryInfo.getInstance().getBinaryAbsoluteFile().getAbsolutePath());
    }

    public void setExternalRacerBinaryPath(String binaryPath) {
        getPrefs().putString(EXTERNAL_RACER_BINARY_PATH_KEY, binaryPath);
    }

    /**
     * Returns the IP Address of a remove Racer instance
     * @return
     */
    public String getRemoteRacerAddress() {
        return getPrefs().getString(REMOTE_RACER_ADDRESS_KEY, "localhost");
    }

    public void setRemoteRacerAddress(String address) {
        getPrefs().putString(REMOTE_RACER_ADDRESS_KEY, address);
    }

    /**
     * Returns the port number of a remove Racer instance
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
     * @return
     */
    public int getLocalRacerPort() {
        return getPrefs().getInt(LOCAL_RACER_PORT_KEY, 8088);
    }

    public void setLocalRacerPort(int port) {
        getPrefs().putInt(LOCAL_RACER_PORT_KEY, port);
    }

}

