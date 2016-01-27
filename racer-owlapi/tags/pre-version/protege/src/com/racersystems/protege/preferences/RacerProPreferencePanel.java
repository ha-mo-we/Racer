package com.racersystems.protege.preferences;

import com.racersystems.protege.RacerInstalledBinaryInfo;
import org.protege.editor.core.ui.preferences.PreferencesPanelLayoutManager;
import org.protege.editor.core.ui.util.ComponentFactory;
import org.protege.editor.owl.ui.preferences.OWLPreferencesPanel;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 14.10.2010
 */
public class RacerProPreferencePanel extends OWLPreferencesPanel implements ActionListener {
    private JRadioButton startRacerPro;
    private JRadioButton connectRacerPro;

    private JRadioButton useIntegratedRacerPro;
    private JRadioButton useExternalRacerPro;
    protected JTextField binaryField;
    private JTextField localRacerPort;

    protected JTextField remoteRacerProAdress;
    protected JTextField remoteRacerProPort;

    protected JCheckBox checkForUpdates;

    private final RacerPreferences prefs = RacerPreferences.getInstance();

    @Override
    public void applyChanges() {
        prefs.setStartRacerEnabled(this.startRacerPro.isSelected());
        prefs.setIntegratedRacerEnabled(this.useIntegratedRacerPro.isSelected());
        prefs.setExternalRacerBinaryPath(this.binaryField.getText());
        if (this.localRacerPort.getText() != null && this.localRacerPort.getText().length() > 0)
            prefs.setLocalRacerPort(Integer.valueOf(this.localRacerPort.getText()));
        prefs.setRemoteRacerAddress(this.remoteRacerProAdress.getText());
        if (this.remoteRacerProPort.getText() != null && this.remoteRacerProPort.getText().length() > 0)
            prefs.setRemoteRacerPort(Integer.valueOf(this.remoteRacerProPort.getText()));

        prefs.setAutoUpdateEnabled(this.checkForUpdates.isSelected());
    }

    public void initialise() throws Exception {
        /*  JPanel autoExpansionPanel = new JPanel();
 PreferencesPanelLayoutManager layout = new PreferencesPanelLayoutManager(autoExpansionPanel);
 autoExpansionPanel.setLayout(layout);
 autoExpansionPanel.add("", createBinaryPanel());
 setLayout(new BorderLayout());
 add(autoExpansionPanel, BorderLayout.CENTER);      */

        JComponent panel = createBinaryPanel();

        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
        add(panel);
    }

    public void dispose() throws Exception {
    }


    protected JComponent createBinaryPanel() {
        final int HORIZONTAL_SPACE = 35;
        JComponent c = createPane(null, BoxLayout.PAGE_AXIS);

        this.startRacerPro = new JRadioButton("Start RacerPro automatically");
        JPanel startRacerProDetails = new JPanel();
        startRacerProDetails.setLayout(new BoxLayout(startRacerProDetails, BoxLayout.PAGE_AXIS));
        this.localRacerPort = new JTextField();
        JComponent localPortPanel = new JPanel();
        localPortPanel.setLayout(new BoxLayout(localPortPanel, BoxLayout.LINE_AXIS));
        localPortPanel.setAlignmentX(0.0f);
        localPortPanel.add(new JLabel("Local Port"));
        localPortPanel.add(this.localRacerPort);
        startRacerProDetails.add(localPortPanel);
        RacerPreferences prefs = new RacerPreferences();
        this.useIntegratedRacerPro = new JRadioButton("Use integrated RacerPro ("+RacerInstalledBinaryInfo.getInstance().getInstalledBinaryVersion().toString() +")");
        startRacerProDetails.add(this.useIntegratedRacerPro);
        this.useExternalRacerPro = new JRadioButton("Use external RacerPro");
        startRacerProDetails.add(this.useExternalRacerPro);
        startRacerProDetails.setBorder(BorderFactory.createEmptyBorder(0, HORIZONTAL_SPACE, 0, 0));

        JPanel binaryPanel = new JPanel();
        binaryPanel.setLayout(new BoxLayout(binaryPanel, BoxLayout.LINE_AXIS));
        binaryPanel.setAlignmentX(0.0f);
        binaryPanel.add(Box.createHorizontalStrut(HORIZONTAL_SPACE));
        binaryPanel.add(new JLabel("Binary"));
        this.binaryField = new JTextField();
        binaryPanel.add(this.binaryField);
        startRacerProDetails.add(binaryPanel);


        this.connectRacerPro = new JRadioButton("Connect to running RacerPro");

        JComponent remoteAdressPanel = new JPanel();
        remoteAdressPanel.setLayout(new BoxLayout(remoteAdressPanel, BoxLayout.LINE_AXIS));
        remoteAdressPanel.setAlignmentX(0.0f);
        remoteAdressPanel.add(Box.createHorizontalStrut(HORIZONTAL_SPACE));
        remoteAdressPanel.add(new JLabel("Remote Adress"));
        this.remoteRacerProAdress = new JTextField();
        remoteAdressPanel.add(this.remoteRacerProAdress);

        JComponent remotePortPanel = new JPanel();
        remotePortPanel.setLayout(new BoxLayout(remotePortPanel, BoxLayout.LINE_AXIS));
        remotePortPanel.setAlignmentX(0.0f);
        remotePortPanel.add(Box.createHorizontalStrut(HORIZONTAL_SPACE));
        remotePortPanel.add(new JLabel("Remote Port"));
        this.remoteRacerProPort = new JTextField();
        remotePortPanel.add(this.remoteRacerProPort);


        this.checkForUpdates = new JCheckBox("Check for Racer binary updates");
        this.checkForUpdates.setSelected(true);
        this.checkForUpdates.setToolTipText("Check (and download if available) for newer version of Racer when starting Protege.");


        JPanel customPane = new JPanel();
        customPane.setLayout(new BoxLayout(customPane, BoxLayout.PAGE_AXIS));
        customPane.setAlignmentX(0.0f);
        customPane.add(this.startRacerPro);
        customPane.add(startRacerProDetails);
        customPane.add(this.connectRacerPro);
        customPane.add(remoteAdressPanel);
        customPane.add(remotePortPanel);

        c.add(customPane);


        //some button logics
        ButtonGroup localRemote = new ButtonGroup();
        localRemote.add(this.startRacerPro);
        localRemote.add(this.connectRacerPro);

        ButtonGroup internalGroup = new ButtonGroup();
        internalGroup.add(this.useIntegratedRacerPro);
        internalGroup.add(this.useExternalRacerPro);


        if (prefs.isStartRacerEnabled()) {
            this.startRacerPro.setSelected(true);
            this.localRacerPort.setEnabled(true);
            this.useIntegratedRacerPro.setEnabled(true);
            this.useExternalRacerPro.setEnabled(true);
            this.remoteRacerProAdress.setEnabled(false);
            this.remoteRacerProPort.setEnabled(false);
        } else {
            this.localRacerPort.setEnabled(false);
            this.binaryField.setEnabled(false);
            this.useIntegratedRacerPro.setEnabled(false);
            this.useExternalRacerPro.setEnabled(false);
            this.connectRacerPro.setSelected(true);
            this.remoteRacerProAdress.setEnabled(true);
            this.remoteRacerProPort.setEnabled(true);
        }

        if (prefs.isIntegratedRacerEnabled()) {
            this.useIntegratedRacerPro.setSelected(true);
            this.useExternalRacerPro.setSelected(false);
            if (this.useIntegratedRacerPro.isEnabled())
                this.binaryField.setEnabled(false);
        } else {
            this.useExternalRacerPro.setSelected(true);
            this.useIntegratedRacerPro.setSelected(false);
            if (this.useExternalRacerPro.isEnabled())
                this.binaryField.setEnabled(true);
        }

        this.remoteRacerProAdress.setText(prefs.getRemoteRacerAddress());
        this.remoteRacerProPort.setText("" + prefs.getRemoteRacerPort());
        this.localRacerPort.setText("" + prefs.getLocalRacerPort());
        this.binaryField.setText(prefs.getExternalRacerBinaryPath());

        this.startRacerPro.addActionListener(this);
        this.connectRacerPro.addActionListener(this);
        this.useIntegratedRacerPro.addActionListener(this);
        this.useExternalRacerPro.addActionListener(this);
        this.checkForUpdates.addActionListener(this);

        return c;
    }

    private JComponent createPane(String title, int orientation) {
        JComponent c = new Box(orientation) {
            public Dimension getMaximumSize() {
                return new Dimension(super.getMaximumSize().width, getPreferredSize().height);
            }
        };
        c.setAlignmentX(0.0f);
        if (title != null) {
            c.setBorder(ComponentFactory.createTitledBorder(title));
        }
        return c;
    }


    protected JPanel createBinaryPanel2() {
        final Insets insets = new Insets(0, 50, 0, 0);
        JPanel binaryPanel = new JPanel();
        PreferencesPanelLayoutManager layout = new PreferencesPanelLayoutManager(binaryPanel);
        binaryPanel.setLayout(layout);
        this.startRacerPro = new JRadioButton("Start RacerPro automatically");
        binaryPanel.add("", this.startRacerPro);

        JPanel localPortPanel = new JPanel();
        localPortPanel.setBorder(new EmptyBorder(insets));
        PreferencesPanelLayoutManager localPortLayout = new PreferencesPanelLayoutManager(localPortPanel);
        localPortPanel.setLayout(localPortLayout);
        this.localRacerPort = new JTextField();
        localRacerPort.setPreferredSize(new Dimension(450, (int) localRacerPort.getPreferredSize().getHeight()));
        localPortPanel.add("Local port", localRacerPort);
        binaryPanel.add("", localPortPanel);

        this.useIntegratedRacerPro = new JRadioButton("Use integrated RacerPro");
        this.useIntegratedRacerPro.setBorder(new EmptyBorder(insets));
        binaryPanel.add("", this.useIntegratedRacerPro);
        this.useExternalRacerPro = new JRadioButton("Use external RacerPro");
        this.useExternalRacerPro.setBorder(new EmptyBorder(insets));
        binaryPanel.add("", this.useExternalRacerPro);
        JPanel externalPanel = new JPanel();
        externalPanel.setBorder(new EmptyBorder(new Insets(0, 100, 0, 0)));
        PreferencesPanelLayoutManager layout2 = new PreferencesPanelLayoutManager(externalPanel);
        externalPanel.setLayout(layout2);
        binaryPanel.add(externalPanel);
        this.binaryField = new JTextField();
        binaryField.setPreferredSize(new Dimension(450, (int) binaryField.getPreferredSize().getHeight()));
        externalPanel.add("Binary", binaryField);

        this.connectRacerPro = new JRadioButton("Connect to running RacerPro");
        binaryPanel.add("", this.connectRacerPro);

        JPanel externalPanel2 = new JPanel();
        externalPanel2.setBorder(new EmptyBorder(new Insets(0, 100, 0, 0)));
        PreferencesPanelLayoutManager layout3 = new PreferencesPanelLayoutManager(externalPanel2);
        externalPanel2.setLayout(layout3);
        binaryPanel.add(externalPanel2);

        this.remoteRacerProAdress = new JTextField();
        remoteRacerProAdress.setPreferredSize(new Dimension(450, (int) remoteRacerProAdress.getPreferredSize().getHeight()));

        externalPanel2.add("Remote address", this.remoteRacerProAdress);
        this.remoteRacerProPort = new JTextField();
        remoteRacerProPort.setPreferredSize(new Dimension(450, (int) remoteRacerProPort.getPreferredSize().getHeight()));
        externalPanel2.add("Remote port", this.remoteRacerProPort);


        this.checkForUpdates = new JCheckBox("Check for Racer binary updates");
        this.checkForUpdates.setSelected(true);
        this.checkForUpdates.setToolTipText("Check (and download if available) for newer version of Racer when starting Protege.");

        //some button logics
        ButtonGroup localRemote = new ButtonGroup();
        localRemote.add(this.startRacerPro);
        localRemote.add(this.connectRacerPro);

        ButtonGroup internalGroup = new ButtonGroup();
        internalGroup.add(this.useIntegratedRacerPro);
        internalGroup.add(this.useExternalRacerPro);


        if (prefs.isStartRacerEnabled()) {
            this.startRacerPro.setSelected(true);
            this.localRacerPort.setEnabled(true);
            this.useIntegratedRacerPro.setEnabled(true);
            this.useExternalRacerPro.setEnabled(true);
            this.remoteRacerProAdress.setEnabled(false);
            this.remoteRacerProPort.setEnabled(false);
        } else {
            this.localRacerPort.setEnabled(false);
            this.binaryField.setEnabled(false);
            this.useIntegratedRacerPro.setEnabled(false);
            this.useExternalRacerPro.setEnabled(false);
            this.connectRacerPro.setSelected(true);
            this.remoteRacerProAdress.setEnabled(true);
            this.remoteRacerProPort.setEnabled(true);
        }

        if (prefs.isIntegratedRacerEnabled()) {
            this.useIntegratedRacerPro.setSelected(true);
            this.useExternalRacerPro.setSelected(false);
            if (this.useIntegratedRacerPro.isEnabled())
                this.binaryField.setEnabled(false);
        } else {
            this.useExternalRacerPro.setSelected(true);
            this.useIntegratedRacerPro.setSelected(false);
            if (this.useExternalRacerPro.isEnabled())
                this.binaryField.setEnabled(true);
        }

        this.remoteRacerProAdress.setText(prefs.getRemoteRacerAddress());
        this.remoteRacerProPort.setText("" + prefs.getRemoteRacerPort());
        this.localRacerPort.setText("" + prefs.getLocalRacerPort());
        this.binaryField.setText(prefs.getExternalRacerBinaryPath());

        this.startRacerPro.addActionListener(this);
        this.connectRacerPro.addActionListener(this);
        this.useIntegratedRacerPro.addActionListener(this);
        this.useExternalRacerPro.addActionListener(this);
        this.checkForUpdates.addActionListener(this);

        return binaryPanel;

    }

    public void actionPerformed(ActionEvent event) {
        if (startRacerPro.isSelected()) {
            this.localRacerPort.setEnabled(true);
            this.useIntegratedRacerPro.setEnabled(true);
            this.useExternalRacerPro.setEnabled(true);
            this.remoteRacerProAdress.setEnabled(false);
            this.remoteRacerProPort.setEnabled(false);
            this.binaryField.setEnabled(this.useExternalRacerPro.isSelected());
        } else {
            this.remoteRacerProAdress.setEnabled(true);
            this.remoteRacerProPort.setEnabled(true);
            this.useIntegratedRacerPro.setEnabled(false);
            this.useExternalRacerPro.setEnabled(false);
            this.binaryField.setEnabled(false);
            this.localRacerPort.setEnabled(false);
        }
        if (useIntegratedRacerPro.isSelected()) {
            this.binaryField.setEnabled(false);
        } else {
            this.binaryField.setEnabled(useExternalRacerPro.isEnabled());
        }
    }
}
