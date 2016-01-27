package com.racersystems.protege.preferences;

import org.protege.editor.owl.ui.preferences.OWLPreferencesPanel;
import org.protege.editor.core.ui.preferences.PreferencesPanelLayoutManager;

import javax.swing.*;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.*;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * The <code>RacerPreferencePanel</code> is the UI component that is responsible to set
 * preferences (see @link{RacerPreferences}). Typically it is visible as a tab.
 *
 * @author Olaf Noppens
 */
public class RacerPreferencePanel extends OWLPreferencesPanel implements ActionListener {

    protected JTextField directoryField;
    protected JTextField portField;
    protected JCheckBox unsafeModeCheckBox;
    protected JTextField loggingFile;
    protected JCheckBox usePluginFolderBinary;
    protected JCheckBox utf8CheckBox;
    protected JCheckBox loggingEnableCheckBox;


    public void applyChanges() {
        RacerPreferences prefs = RacerPreferences.getInstance();
        prefs.setDirectory(directoryField.getText());
        prefs.setPort(Integer.parseInt(portField.getText()));
        prefs.setUnsafeModeEnabled(this.unsafeModeCheckBox.isSelected());
        prefs.setLoggingEnabled(this.loggingEnableCheckBox.isSelected());
        prefs.setLoggingFile(this.loggingFile.getText());
    }

    public void initialise() throws Exception {
        setLayout(new PreferencesPanelLayoutManager(this));

        this.usePluginFolderBinary = new JCheckBox("Use binary in P4 folder (racer)");
        // add(this.usePluginFolderBinary, "");
        this.usePluginFolderBinary.addActionListener(this);

        directoryField = new JTextField(RacerPreferences.getInstance().getDirectory());
        String dir = RacerPreferences.getInstance().getDirectory();
        if (dir == null || dir.length() == 0) {
            directoryField.setText("");
            directoryField.setPreferredSize(new Dimension(450, (int) directoryField.getPreferredSize().getHeight()));
        }
        add(directoryField, "Path to RACER binary file");
        portField = new JTextField();
        portField.setText(String.valueOf(RacerPreferences.getInstance().getPort()));
        add(portField, "Port number for RACER service");
        if (portField.getText() == null || portField.getText().length() == 0) {
            portField.setText("");
            portField.setPreferredSize(new Dimension(450, (int) portField.getPreferredSize().getHeight()));
        }
        this.unsafeModeCheckBox = new JCheckBox("");
        this.unsafeModeCheckBox.setSelected(RacerPreferences.getInstance().isUnsafeModeEnabled());
        add(unsafeModeCheckBox, "Unsafe mode");

        this.loggingEnableCheckBox = new JCheckBox("");
        this.loggingEnableCheckBox.setSelected(RacerPreferences.getInstance().isLoggingEnabled());
        this.loggingEnableCheckBox.addActionListener(this);
        add(this.loggingEnableCheckBox, "Enable logging");
        this.loggingFile = new JTextField();
        this.loggingFile.setText(RacerPreferences.getInstance().getLoggingFile());
        if (this.loggingFile.getText() == null || this.loggingFile.getText().length() == 0) {
            this.loggingFile.setPreferredSize(new Dimension(450, (int) this.loggingFile.getPreferredSize().getHeight()));
        }
        add(this.loggingFile, "Logging to file");
        this.loggingFile.setEnabled(this.loggingEnableCheckBox.isSelected());
        this.utf8CheckBox = new JCheckBox("");
        this.utf8CheckBox.setSelected(RacerPreferences.getInstance().isUTF8());
        add(this.utf8CheckBox, "Use UTF-8 encoding");
    }

    public void dispose() throws Exception {
    }

    public void actionPerformed(ActionEvent e) {
        if (e.getSource() == this.usePluginFolderBinary) {
            this.directoryField.setEnabled(usePluginFolderBinary.isSelected());
        }

        if (e.getSource() == this.loggingEnableCheckBox) {
            this.loggingFile.setEnabled(this.loggingEnableCheckBox.isSelected());
        }
    }
}
