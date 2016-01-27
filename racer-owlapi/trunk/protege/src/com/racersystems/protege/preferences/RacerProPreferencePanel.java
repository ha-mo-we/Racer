package com.racersystems.protege.preferences;

import com.racersystems.protege.RacerProInstalledBinaryInfo;
import org.protege.editor.core.ui.util.ComponentFactory;
import org.protege.editor.owl.ui.preferences.OWLPreferencesPanel;

import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
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

    protected JCheckBox enableLogging;
    protected JTextField loggingFile;

    JCheckBox enableTerminalWindow;
    JCheckBox enableNavigationInTerminalWindow;



    private final RacerProPreferences prefs = RacerProPreferences.getInstance();

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
        prefs.setLoggingDirectory(this.loggingFile.getText());
        prefs.setLoggingEnabled(this.enableLogging.isSelected());
        prefs.setEnableTerminalWindow(this.enableTerminalWindow.isSelected());
        prefs.setEnableNavigationInTerminalWindow(this.enableNavigationInTerminalWindow.isSelected());
    }

    public void initialise() throws Exception {
        JComponent panel = createBinaryPanel();
        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));
        add(panel);
        //  add(createAutoUpdatePanel());
        add(createDisposeReasonerOnClosePanel());
        add(createOverwriteReasonerConfiguration());
        add(createDebugPanel());
    }

    public void dispose() throws Exception {
    }

    protected JComponent createAutoUpdatePanel() {
        JComponent c = createPane(null, BoxLayout.PAGE_AXIS);

        JCheckBox autoUpdateBox = new JCheckBox("Check for RacerPro updates");
        autoUpdateBox.setSelected(prefs.isAutoUpdateEnabled());
        autoUpdateBox.setActionCommand("autoupdate");
        autoUpdateBox.addActionListener(this);

        JPanel customPane = new JPanel();
        customPane.setLayout(new BoxLayout(customPane, BoxLayout.PAGE_AXIS));
        customPane.setAlignmentX(0.0f);
        customPane.add(autoUpdateBox);
        c.add(customPane);
        return c;
    }

    protected JComponent createDisposeReasonerOnClosePanel() {
        JComponent c = createPane("General configuration", BoxLayout.PAGE_AXIS);
        JCheckBox disposeBox = new JCheckBox("Don't dispose Racer KB on reasoner dispose");
        disposeBox.setAlignmentX(LEFT_ALIGNMENT);
        disposeBox.setAlignmentX(0);
        disposeBox.setToolTipText("Enable this option if the Racer KB should not be disposed.\nThis is useful if you" +
                " want to inspect the KB with an external program such as RacerPorter.\n\nPlease note that you are" +
                " responsible for the disposal of the KB!\n(This only applies to external Racer instances)");
        disposeBox.setSelected(!prefs.isDisposeReasonerOnClose());
        disposeBox.setActionCommand("disposeReasonerOnClose");
        disposeBox.addActionListener(this);

        JComponent disposeInfoBox = createInfoPanel("If this option is enabled, knowledge bases are not disposed in RacerPro" +
                " after switching the reasoner in Protege or after closing the ontology. Use this option only if you want to " +
                "inspect the knowledge base with RacerPorter or a similar application and if you are running an external " +
                "RacerPro instance that is neither started nor shutdowned automatically by Protege");
        disposeInfoBox.setAlignmentX(LEFT_ALIGNMENT);

        JCheckBox enableMemorySavingMode = new JCheckBox("Enable memory saving mode");
        enableMemorySavingMode.setAlignmentX(LEFT_ALIGNMENT);
        enableMemorySavingMode.setSelected(prefs.isMemorySavingModeEnabled());
        enableMemorySavingMode.setActionCommand("enableMemorySavingMode");
        enableMemorySavingMode.addActionListener(this);
        JComponent memorySavingModeInfo = createInfoPanel("If this option is enabled," +
                " less memory is needed for ontologies in RacerPro but removing axioms" +
                " might require to transfer the whole ontology to RacerPro" +
                "");
        memorySavingModeInfo.setAlignmentX(LEFT_ALIGNMENT);

        /* JPanel customPane = new JPanel();
        customPane.setLayout(new BoxLayout(customPane, BoxLayout.PAGE_AXIS));
        customPane.setAlignmentX(0.0f);
        customPane.add(disposeBox);
        customPane.add(disposeInfoBox);
        customPane.add(enableMemorySavingMode);
        customPane.add(memorySavingModeInfo);
        c.add(customPane);*/


        Box holder = new Box(BoxLayout.Y_AXIS);

        c.add(holder);
        holder.add(disposeBox);
        holder.add(disposeInfoBox);
        holder.add(enableMemorySavingMode);
        holder.add(memorySavingModeInfo);


        return c;
    }

    protected JComponent createOverwriteReasonerConfiguration() {
        final int HORIZONTAL_SPACE = 35;
        JComponent c = createPane("ABox Configuration", BoxLayout.PAGE_AXIS);
        RacerProPreferences prefs = new RacerProPreferences();

        JCheckBox disableABoxRealization = new JCheckBox("Disable ABox realization");
        disableABoxRealization.setSelected(prefs.isDisableABoxRealization());
        disableABoxRealization.setActionCommand("DisableABoxRealization");
        disableABoxRealization.addActionListener(this);
        JPanel disableABoxRealizationDetails = new JPanel();
        disableABoxRealizationDetails.setLayout(new BoxLayout(disableABoxRealizationDetails, BoxLayout.PAGE_AXIS));
        JComponent disableABoxDetailPanel = new JPanel();
        disableABoxDetailPanel.setLayout(new BoxLayout(disableABoxDetailPanel, BoxLayout.LINE_AXIS));
        disableABoxDetailPanel.setAlignmentX(0.0f);

        JLabel warningLabel = new JLabel();
        warningLabel.setFont(warningLabel.getFont().deriveFont(Font.ITALIC));
        warningLabel.setText("If this option is enabled, Protege Preferences for Reasoner initalization have no effect on ABox realization.");

        JTextArea warningTA = new JTextArea();
        warningTA.setFont(warningTA.getFont().deriveFont(Font.ITALIC));
        warningTA.setText("If this option is enabled, Protege Preferences for Reasoner initalization have no effect on ABox realization.");
        warningTA.setEditable(false);

        disableABoxDetailPanel.add(warningTA);
        disableABoxRealizationDetails.add(disableABoxDetailPanel);
        disableABoxRealizationDetails.setBorder(BorderFactory.createEmptyBorder(0, HORIZONTAL_SPACE, 0, 0));


        JCheckBox disableAboxConsistencyTest = new JCheckBox("Disable ABox consistency checks");
        disableAboxConsistencyTest.setSelected(prefs.isDisableABoxConsistencyTest());
        disableAboxConsistencyTest.setActionCommand("DisableABoxConsistencyTest");
        disableAboxConsistencyTest.addActionListener(this);


        JPanel customPane = new JPanel();
        customPane.setLayout(new BoxLayout(customPane, BoxLayout.PAGE_AXIS));
        customPane.setAlignmentX(0.0f);
        customPane.add(disableABoxRealization);
        customPane.add(disableABoxRealizationDetails);
        customPane.add(disableAboxConsistencyTest);
        c.add(customPane);
        return c;
    }

    protected JComponent createDebugPanel() {
        final int HORIZONTAL_SPACE = 35;
        JComponent c = createPane("Debug Configuration", BoxLayout.PAGE_AXIS);
        RacerProPreferences prefs = new RacerProPreferences();

        enableTerminalWindow = new JCheckBox("Show Racer terminal window");
        enableTerminalWindow.setSelected(prefs.isTerminalWindowEnabled());
        enableTerminalWindow.setActionCommand("terminalWindowEnabled");
        enableTerminalWindow.addActionListener(this);

        this.enableTerminalWindow.setEnabled(prefs.isStartRacerEnabled());

        JPanel enableTerminalWindowDetails = new JPanel();
        enableTerminalWindowDetails.setLayout(new BoxLayout(enableTerminalWindowDetails, BoxLayout.PAGE_AXIS));
        JComponent disableABoxDetailPanel = new JPanel();
        disableABoxDetailPanel.setLayout(new BoxLayout(disableABoxDetailPanel, BoxLayout.LINE_AXIS));
        disableABoxDetailPanel.setAlignmentX(0.0f);


        JTextArea detailsTA = new JTextArea();
        detailsTA.setFont(detailsTA.getFont().deriveFont(Font.ITALIC));
        detailsTA.setText("If this option is enabled, a terminal window is openend showing RacerPro's debug and warning messages.");
        detailsTA.setEditable(false);

        disableABoxDetailPanel.add(detailsTA);
        enableTerminalWindowDetails.add(disableABoxDetailPanel);
        enableTerminalWindowDetails.setBorder(BorderFactory.createEmptyBorder(0, HORIZONTAL_SPACE, 0, 0));


        this.enableNavigationInTerminalWindow = new JCheckBox("Enable navigation in terminal window");
        this.enableNavigationInTerminalWindow.setSelected(prefs.isNavigationInTerminalWindowEnabeld());
        this.enableNavigationInTerminalWindow.setActionCommand("navigationTerminalWindowEnabeled");
        this.enableNavigationInTerminalWindow.addActionListener(this);
        this.enableNavigationInTerminalWindow.setEnabled(this.enableTerminalWindow.isEnabled());

        JPanel enableNavigationTerminalWindowDetails = new JPanel();
        enableNavigationTerminalWindowDetails.setLayout(new BoxLayout(enableNavigationTerminalWindowDetails, BoxLayout.PAGE_AXIS));
        JComponent disableTABoxDetailPanel = new JPanel();
        disableTABoxDetailPanel.setLayout(new BoxLayout(disableTABoxDetailPanel, BoxLayout.LINE_AXIS));
        disableTABoxDetailPanel.setAlignmentX(0.0f);


        JTextArea detailsNTA = new JTextArea();
        detailsNTA.setFont(detailsNTA.getFont().deriveFont(Font.ITALIC));
        detailsNTA.setText("If this option is enabled, clicking on entites in the terminal window displays them in P4");
        detailsNTA.setEditable(false);

        disableTABoxDetailPanel.add(detailsNTA);
        enableNavigationTerminalWindowDetails.add(disableTABoxDetailPanel);
        enableNavigationTerminalWindowDetails.setBorder(BorderFactory.createEmptyBorder(0, HORIZONTAL_SPACE, 0, 0));


         this.enableNavigationInTerminalWindow.setEnabled(this.enableTerminalWindow.isSelected());


        JPanel customPane = new JPanel();
        customPane.setLayout(new BoxLayout(customPane, BoxLayout.PAGE_AXIS));
        customPane.setAlignmentX(0.0f);
        customPane.add(enableTerminalWindow);
        customPane.add(enableTerminalWindowDetails);
        customPane.add(enableNavigationInTerminalWindow);
        customPane.add(enableNavigationTerminalWindowDetails);
        c.add(customPane);
        return c;

    }


    protected JComponent createBinaryPanel() {
        final int HORIZONTAL_SPACE = 35;
        JComponent c = createPane("RacerPro", BoxLayout.PAGE_AXIS);

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
        RacerProPreferences prefs = new RacerProPreferences();
        if (RacerProInstalledBinaryInfo.getInstance().isBinaryFileAvailable())
            this.useIntegratedRacerPro = new JRadioButton("Use integrated RacerPro (" + RacerProInstalledBinaryInfo.getInstance().getInstalledBinaryVersion().toString() + ")");
        else
            this.useIntegratedRacerPro = new JRadioButton("Use integrated RacerPro ( Not Available!)");
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
        this.binaryField.setToolTipText("Specify the path to the RacerPro directory.");
        binaryPanel.add(this.binaryField);
        JButton binaryFileChooser = new JButton("...");
        binaryFileChooser.addActionListener(this);
        binaryFileChooser.setActionCommand("binaryFileChooser");
        binaryPanel.add(binaryFileChooser);
        startRacerProDetails.add(binaryPanel);

        JComponent loggingPanel = new JPanel();
        loggingPanel.setLayout(new BoxLayout(loggingPanel, BoxLayout.LINE_AXIS));
        loggingPanel.setAlignmentX(0.0f);
        this.enableLogging = new JCheckBox("Enable logging");
        loggingPanel.add(this.enableLogging);
        this.loggingFile = new JTextField(prefs.getLoggingDirectory());
        this.loggingFile.setToolTipText("Specify logging directory. Filename will automatically created using the current date (racer-YYYY-MM-DD.log");
        loggingPanel.add(this.loggingFile);

        JButton logginFileChooser = new JButton("...");
        logginFileChooser.addActionListener(this);
        logginFileChooser.setActionCommand("loggingFileChooser");
        loggingPanel.add(logginFileChooser);

        JButton resetButton = new JButton("Reset to default");
        resetButton.setActionCommand("loggingFileReset");
        resetButton.addActionListener(this);
        loggingPanel.add(resetButton);

        startRacerProDetails.add(loggingPanel);

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


        this.checkForUpdates = new JCheckBox("Check for RacerPro binary updates");
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
            this.loggingFile.setEnabled(true);
            this.enableLogging.setEnabled(true);
        } else {
            this.localRacerPort.setEnabled(false);
            this.binaryField.setEnabled(false);
            this.useIntegratedRacerPro.setEnabled(false);
            this.useExternalRacerPro.setEnabled(false);
            this.connectRacerPro.setSelected(true);
            this.remoteRacerProAdress.setEnabled(true);
            this.remoteRacerProPort.setEnabled(true);
            this.enableLogging.setEnabled(false);
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
        this.enableLogging.setSelected(prefs.isLoggingEnabled());
        this.loggingFile.setEnabled(prefs.isLoggingEnabled() && prefs.isStartRacerEnabled());

        this.remoteRacerProAdress.setText(prefs.getRemoteRacerAddress());
        this.remoteRacerProPort.setText("" + prefs.getRemoteRacerPort());
        this.localRacerPort.setText("" + prefs.getLocalRacerPort());
        this.binaryField.setText(prefs.getExternalRacerBinaryPath());
        this.loggingFile.setText(prefs.getLoggingDirectory());

        this.startRacerPro.addActionListener(this);
        this.connectRacerPro.addActionListener(this);
        this.useIntegratedRacerPro.addActionListener(this);
        this.useExternalRacerPro.addActionListener(this);
        this.checkForUpdates.addActionListener(this);
        this.enableLogging.addActionListener(this);



        return c;
    }

    private JComponent createInfoPanel(String text) {
        final int HORIZONTAL_SPACE = 35;
        JTextArea infoPanel = new JTextArea();
        infoPanel.setLineWrap(true);
        infoPanel.setWrapStyleWord(true);
        infoPanel.setFont(infoPanel.getFont().deriveFont(Font.ITALIC));
        infoPanel.setText(text);
        infoPanel.setEditable(false);
        infoPanel.setBorder(BorderFactory.createEmptyBorder(0, HORIZONTAL_SPACE, 0, 0));
        return infoPanel;
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

    public void actionPerformed(ActionEvent event) {
        if (startRacerPro.isSelected()) {
            this.localRacerPort.setEnabled(true);
            this.useIntegratedRacerPro.setEnabled(true);
            this.useExternalRacerPro.setEnabled(true);
            this.remoteRacerProAdress.setEnabled(false);
            this.remoteRacerProPort.setEnabled(false);
            this.binaryField.setEnabled(this.useExternalRacerPro.isSelected());
            this.enableLogging.setEnabled(true);
            this.loggingFile.setEnabled(true);
            this.enableTerminalWindow.setEnabled(true);

        } else {
            this.remoteRacerProAdress.setEnabled(true);
            this.remoteRacerProPort.setEnabled(true);
            this.useIntegratedRacerPro.setEnabled(false);
            this.useExternalRacerPro.setEnabled(false);
            this.binaryField.setEnabled(false);
            this.localRacerPort.setEnabled(false);
            this.loggingFile.setEnabled(false);
            this.enableLogging.setEnabled(false);
            this.enableTerminalWindow.setEnabled(false);
        }
        if (useIntegratedRacerPro.isSelected()) {
            this.binaryField.setEnabled(false);
        } else {
            this.binaryField.setEnabled(useExternalRacerPro.isEnabled());
        }
        this.loggingFile.setEnabled(prefs.isLoggingEnabled() && prefs.isStartRacerEnabled());
        this.loggingFile.setEnabled(this.enableLogging.isSelected());

        if ("autoupdate".equals(event.getActionCommand())) {
            AbstractButton button = (AbstractButton) event.getSource();
            prefs.setAutoUpdateEnabled(button.getModel().isSelected());
        } else if ("disposeReasonerOnClose".equals(event.getActionCommand())) {
            AbstractButton button = (AbstractButton) event.getSource();
            prefs.setDisposeReasonerOnClose(!button.getModel().isSelected());
        }
        if ("terminalWindowEnabled".equals(event.getActionCommand())) {
            AbstractButton button = (AbstractButton) event.getSource();
            prefs.setEnableTerminalWindow(button.getModel().isSelected());
        }
        if ("navigationTerminalWindowEnabeled".equals(event.getActionCommand())) {
            AbstractButton button = (AbstractButton) event.getSource();
            prefs.setEnableNavigationInTerminalWindow(button.getModel().isSelected());
        }

        if ("DisableABoxRealization".equals(event.getActionCommand())) {
            AbstractButton button = (AbstractButton) event.getSource();
            prefs.setDisableABoxRealization(button.getModel().isSelected());
        }
        if ("DisableABoxConsistencyTest".equals(event.getActionCommand())) {
            AbstractButton button = (AbstractButton) event.getSource();
            prefs.setDisableABoxConsistencyTest(button.getModel().isSelected());
        }
        if ("binaryFileChooser".equals(event.getActionCommand())) {
            JFileChooser fileChooser = new JFileChooser((adjustBinaryFilePath(prefs.getExternalRacerBinaryPath())));
            fileChooser = new JFileChooser();
            fileChooser.setMultiSelectionEnabled(false);
            fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            fileChooser.setAcceptAllFileFilterUsed(false);
            final RacerProInstalledBinaryInfo.OSTYPE ostype = RacerProInstalledBinaryInfo.getInstance().getOSType();
            fileChooser.resetChoosableFileFilters();
            fileChooser.removeChoosableFileFilter(fileChooser.getAcceptAllFileFilter());
            fileChooser.addChoosableFileFilter(new FileFilter() {
                @Override
                public boolean accept(File f) {
                    return f.isDirectory();
                    //return true;
                    /*  System.out.println("accept ? " + f.toString());
                 if (f.isDirectory()) {
                     System.out.println("f is directory");
                     String binaryFile = f.toString();
                     switch (ostype) {
                         case WIN:
                             binaryFile = binaryFile + File.separator +RacerProInstalledBinaryInfo.racerWindowsBinaryName;
                             break;
                         case MAC:
                             binaryFile = binaryFile + File.separator +RacerProInstalledBinaryInfo.racerMacOsXBinaryName;
                             break;
                         case OTHER:
                         case LINUX:
                             binaryFile = binaryFile + File.separator +RacerProInstalledBinaryInfo.racerLinuxBinaryName;
                     }
                     File ff = new File(binaryFile);
                     System.out.println(ff.toString() + " " + ff.exists());
                     return ff.exists();
                 }   else {
                     System.out.println("f is kein directory");
                 }
                 System.out.println("not accept " + f);
                 return false;  */
                }

                @Override
                public String getDescription() {
                    if (RacerProInstalledBinaryInfo.getInstance().getOSType() == RacerProInstalledBinaryInfo.OSTYPE.WIN)
                        return "RacerPro folder";
                    return "RacerPro directory";
                }
            });
            if (RacerProInstalledBinaryInfo.getInstance().getOSType() == RacerProInstalledBinaryInfo.OSTYPE.WIN)
                fileChooser.setDialogTitle("Choose RacerPro folder");
            else
                fileChooser.setDialogTitle("Choose RacerPro directory");

            /*  fileChooser.setFileFilter(new FileFilter() {
              @Override
              public boolean accept(File f) {
                  return (f.getName().startsWith("m"));
              }

              @Override
              public String getDescription() {
                  return "something";
              }
          });  */

            //alternative solution: choose the RacerPro binary file name
            /*   JFileChooser binaryFileChooser = new JFileChooser();
         binaryFileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
         binaryFileChooser.setMultiSelectionEnabled(false);
         binaryFileChooser.setAcceptAllFileFilterUsed(false);
         binaryFileChooser.setSelectedFile(new File(prefs.getExternalRacerBinaryPath()));
         binaryFileChooser.setCurrentDirectory(new File(prefs.getExternalRacerBinaryPath()));
         binaryFileChooser.setDialogTitle("Choose RacerPro binary");
         final RacerProInstalledBinaryInfo.OSTYPE ostype = RacerProInstalledBinaryInfo.getInstance().getOSType();
         binaryFileChooser.setFileFilter(new FileFilter() {
             @Override
             public boolean accept(File f) {
                 if (f.isFile()) {
                     switch (ostype) {
                         case WIN:
                             return f.getName().equals(RacerProInstalledBinaryInfo.racerWindowsBinaryName);
                         case MAC:
                             return f.getName().equals(RacerProInstalledBinaryInfo.racerMacOsXBinaryName);
                         case OTHER:
                         case LINUX:
                             return f.getName().equals(RacerProInstalledBinaryInfo.racerLinuxBinaryName);
                     }
                 }         
                 return false;
             }

             @Override
             public String getDescription() {
                 return "RacerPro binary file";
             }
         });

         if (JFileChooser.APPROVE_OPTION == binaryFileChooser.showDialog(this, null)) {
             String directory = fileChooser.getSelectedFile().toString();
             binaryField.setText(directory);
             prefs.setExternalRacerBinaryPath(directory);
         }   */

            if (JFileChooser.APPROVE_OPTION == fileChooser.showDialog(this, null)) {
                String directory = fileChooser.getSelectedFile().toString();

                String binaryFile = directory + File.separator;
                switch (ostype) {
                    case WIN:
                        binaryFile += RacerProInstalledBinaryInfo.racerWindowsBinaryName;
                        break;
                    case MAC:
                        binaryFile += RacerProInstalledBinaryInfo.racerMacOsXBinaryName;
                        break;
                    case OTHER:
                    case LINUX:
                        binaryFile += RacerProInstalledBinaryInfo.racerLinuxBinaryName;
                }
                File fff = new File(binaryFile);
                if (!fff.exists()) {
                    JOptionPane.showMessageDialog(null, "The given directory does not contain a racer file", "Racer binary not found", JOptionPane.ERROR_MESSAGE);
                } else {

                    binaryField.setText(directory);
                    prefs.setExternalRacerBinaryPath(directory);
                }
            }
        } else if ("loggingFileChooser".equals(event.getActionCommand())) {
            JFileChooser fileChooser = new JFileChooser(prefs.getLoggingDirectory());
            fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            fileChooser.setAcceptAllFileFilterUsed(false);
            fileChooser.setMultiSelectionEnabled(false);
            fileChooser.setDialogTitle("Choose logging directory");
            fileChooser.setFileFilter(new FileFilter() {
                @Override
                public boolean accept(File f) {
                    return f.isDirectory();
                }

                @Override
                public String getDescription() {
                    if (RacerProInstalledBinaryInfo.getInstance().getOSType() == RacerProInstalledBinaryInfo.OSTYPE.WIN)
                        return "folder";
                    return "directory";
                }
            });
            if (JFileChooser.APPROVE_OPTION == fileChooser.showDialog(this, null)) {
                loggingFile.setText(fileChooser.getSelectedFile().toString());
                prefs.setLoggingDirectory(fileChooser.getSelectedFile().toString());
            }
        } else if ("loggingFileReset".equals(event.getActionCommand())) {
            loggingFile.setText(prefs.getDefaultLoggingDirectory());
            prefs.setLoggingDirectory(prefs.getDefaultLoggingDirectory());
        } else if ("enableMemorySavingMode".equals(event.getActionCommand())) {
            AbstractButton button = (AbstractButton) event.getSource();
            prefs.setEnableMemorySavingMode(button.getModel().isSelected());
        }

        this.enableNavigationInTerminalWindow.setEnabled(this.enableTerminalWindow.isSelected());
    }

    protected String adjustBinaryFilePath(String path) {
        String separator = File.separator;
        int index = path.lastIndexOf(separator);
        if (index > -1) {
            path = path.substring(0, index);
        }
        return path;
    }
}
