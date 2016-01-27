package com.racersystems.protege.ui;

import com.racersystems.protege.preferences.RacerProPreferences;
import org.protege.editor.core.ProtegeManager;
import org.protege.editor.owl.model.OWLWorkspace;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;

import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/*******************************************************************************
 * Copyright (c) 2013 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 22.01.2013
 */
public class TerminalWindow extends JFrame {
    JTextPane output;
    InputStream errorStream;
    InputStream inputStream;
    Thread errorThread, outputThread;

    boolean suppressExternalFormatHint = true;

    Set<Pattern> patternsForHighlighting;
    Set<Pattern> patternsForIgnoring;

    Style linkStyle;

    boolean cancelListening = false;


    public TerminalWindow(InputStream input, InputStream error) {
        this.errorStream = error;
        this.inputStream = input;
        initializeDefaultPatterns();
        initializeComponents();
        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                StopListening();
                setVisible(false);
            }
        });
        setTitle("RacerPro Terminal - Close to Dismiss");
    }

    protected void initializeDefaultPatterns() {
        this.patternsForHighlighting = new HashSet<Pattern>();
        this.patternsForHighlighting.add(Pattern.compile("\\QConcept\\E (.)+\\Qcauses a cycle in TBox\\E(.)*"));

        this.patternsForIgnoring = new HashSet<Pattern>();
        this.patternsForIgnoring.add(Pattern.compile("\\QHTTP service enabled for:\\E(.)*"));
        this.patternsForIgnoring.add(Pattern.compile("\\QHTML documentation at\\E(.)*"));
        this.patternsForIgnoring.add(Pattern.compile("\\QTCP service enabled for\\E(.)*"));
        this.patternsForIgnoring.add(Pattern.compile("\\QTCP control enabled for\\E(.)*"));
        this.patternsForIgnoring.add(Pattern.compile("\\QExternal format of 8088 is utf8-base\\E(.)*"));
    }

    protected void initializeStyles() {
        linkStyle = output.getStyledDocument().addStyle("LINK_STYLE", null);
        StyleConstants.setForeground(linkStyle, Color.BLUE);
        StyleConstants.setUnderline(linkStyle, true);
    }

    protected boolean isHighlight(String line) {
        for (Pattern pattern : this.patternsForHighlighting) {
            Matcher matcher = pattern.matcher(line);
            if (matcher.find()) return true;
        }
        return false;
    }

    protected boolean isIgnore(String line) {
        for (Pattern pattern : this.patternsForIgnoring) {
            Matcher matcher = pattern.matcher(line);
            if (matcher.find()) return true;
        }
        return false;
    }


    public int[] getPositionOfEntity(Point point) {
        StyledDocument doc = this.output.getStyledDocument();
        int position = output.viewToModel(point);
        boolean foundStart = false;
        while (position >= 0) {
            try {
                if (doc.getText(position, 1).equals("|")) {
                    foundStart = true;
                    break;
                }
            } catch (BadLocationException e1) {
                position = -1;
                break;
            }
            position--;

        }
        if (foundStart) {
            int startPosition = position + 1;
            position = output.viewToModel(point) + 1;
            boolean foundEnd = false;
            while (position <= doc.getLength()) {
                try {
                    if (doc.getText(position, 1).equals("|")) {
                        foundEnd = true;
                        break;
                    }
                } catch (BadLocationException e1) {
                    position = -1;
                    break;
                }
                position++;
            }
            if (foundEnd) {
                try {
                    IRI iri = IRI.create(doc.getText(startPosition, (position - startPosition)));

                    Set<OWLWorkspace> workspaces = new HashSet<OWLWorkspace>();
                    for (org.protege.editor.core.editorkit.EditorKit kit : ProtegeManager.getInstance().getEditorKitManager().getEditorKits()) {
                        if (kit.getWorkspace() instanceof OWLWorkspace) {
                            workspaces.add((OWLWorkspace) kit.getWorkspace());
                        }
                    }

                    for (OWLWorkspace workspace : workspaces) {
                        if (workspace.getOWLModelManager().getOWLEntityFinder().getEntities(iri).size() > 0) {
                            return new int[]{startPosition, position};
                        }
                    }
                } catch (BadLocationException e1) {
                }
            }
        }
        return null;
    }


    public Set<OWLEntity> getEntities
            (Point
                    point) {
        Set<OWLEntity> entities = new HashSet<OWLEntity>();
        StyledDocument doc = this.output.getStyledDocument();
        int position = output.viewToModel(point);
        while (position >= 0) {
            try {
                if (doc.getText(position, 1).equals("|")) {
                    break;
                }
            } catch (BadLocationException e1) {
                position = -1;
                break;
            }
            position--;

        }
        if (position > -1) {
            int startPosition = position + 1;
            position = output.viewToModel(point) + 1;
            boolean foundEnd = false;
            while (position <= doc.getLength()) {
                try {
                    if (doc.getText(position, 1).equals("|")) {
                        foundEnd = true;
                        break;
                    }
                } catch (BadLocationException e1) {
                    position = -1;
                    break;
                }
                position++;
            }
            if (foundEnd) {
                try {
                    String iri = doc.getText(startPosition, (position - startPosition));

                    Set<OWLWorkspace> workspaces = new HashSet<OWLWorkspace>();
                    for (org.protege.editor.core.editorkit.EditorKit kit : ProtegeManager.getInstance().getEditorKitManager().getEditorKits()) {
                        if (kit.getWorkspace() instanceof OWLWorkspace) {
                            workspaces.add((OWLWorkspace) kit.getWorkspace());
                        }
                    }

                    for (OWLWorkspace workspace : workspaces) {
                        for (OWLEntity entity : workspace.getOWLModelManager().getOWLEntityFinder().getEntities(IRI.create(iri))) {
                            entities.add(entity);
                            /*System.out.println(entity.isOWLClass() + " " + entity);
                            workspace.getOWLSelectionModel().setSelectedEntity(entity);
                            workspace.displayOWLEntity(entity);*/
                        }
                        //workspace.dis
                    }


                } catch (BadLocationException e1) {
                }
            }
        }
        return entities;
    }

    protected void initializeComponents() {
        this.output = new JTextPane();
        final StyledDocument doc = output.getStyledDocument();
        Style def = StyleContext.getDefaultStyleContext().getStyle(StyleContext.DEFAULT_STYLE);

        Style regular = doc.addStyle("regular", def);
        StyleConstants.setFontFamily(def, "SansSerif");

        Style s = doc.addStyle("blue", regular);
        StyleConstants.setForeground(s, Color.BLUE);

        s = doc.addStyle("red", regular);
        StyleConstants.setForeground(s, Color.RED);


        this.output.setEditable(false);
        JScrollPane scrolly = new JScrollPane(this.output);
        setContentPane(scrolly);

        initializeStyles();


        this.output.addMouseListener(new MouseListener() {
            public void mouseClicked(MouseEvent e) {


                if (!RacerProPreferences.getInstance().isNavigationInTerminalWindowEnabeld()) return;

                int position = output.viewToModel(e.getPoint());


                while (position >= 0) {
                    try {
                        if (doc.getText(position, 1).equals("|")) {
                            position = position;
                            break;
                        }
                    } catch (BadLocationException e1) {
                        position = -1;

                        break;
                    }
                    position--;

                }
                if (position > -1) {
                    int startPosition = position + 1;
                    position = output.viewToModel(e.getPoint()) + 1;
                    boolean foundEnd = false;
                    while (position <= doc.getLength()) {
                        try {
                            if (doc.getText(position, 1).equals("|")) {
                                foundEnd = true;
                                break;
                            }
                        } catch (BadLocationException e1) {
                            position = -1;
                            break;
                        }
                        position++;
                    }
                    if (foundEnd) {
                        try {
                            String iri = doc.getText(startPosition, (position - startPosition));



                            Set<OWLWorkspace> workspaces = new HashSet<OWLWorkspace>();
                            for (org.protege.editor.core.editorkit.EditorKit kit : ProtegeManager.getInstance().getEditorKitManager().getEditorKits()) {
                                if (kit.getWorkspace() instanceof OWLWorkspace) {
                                    workspaces.add((OWLWorkspace) kit.getWorkspace());
                                }
                            }

                            for (OWLWorkspace workspace : workspaces) {

                                Set<OWLEntity> entities = workspace.getOWLModelManager().getOWLEntityFinder().getEntities(IRI.create(iri));
                                if (entities.size() > 1) {
                                    //there are more than one entity with the same iri.
                                    Vector<String> types = new Vector<String>();
                                    for(OWLEntity entity : entities) {
                                        if (entity.isOWLClass())
                                            types.add("Class");
                                        else if (entity.isOWLDataProperty())
                                            types.add("DataProeprty");
                                        else if (entity.isOWLObjectProperty())
                                            types.add("ObjectProperty");
                                        else if (entity.isOWLNamedIndividual())
                                            types.add("NamedIndividual");
                                        else if (entity.isOWLDatatype())
                                            types.add("Datatype");
                                    }

                                    if (types.size() > 1)  {

                                    String type = (String) JOptionPane.showInputDialog(null, "Which type should be selected for " + iri + "?",
                                            "Multiple entities with same iri",
                                            JOptionPane.QUESTION_MESSAGE,
                                            null,
                                            types.toArray(),
                                            types.contains("Class") ? "Class" : types.iterator().next()
                                            );
                                        if (type != null) {

                                            for (OWLEntity entity : entities) {
                                                if ("Class".equals(type) && entity.isOWLClass()) {
                                                    workspace.getOWLSelectionModel().setSelectedEntity(entity);
                                                    workspace.displayOWLEntity(entity);
                                                    break;
                                                }
                                                else if ("DataProperty".equals(type) && entity.isOWLDataProperty()) {
                                                    workspace.getOWLSelectionModel().setSelectedEntity(entity);
                                                    workspace.displayOWLEntity(entity);
                                                    break;
                                                }
                                                else if ("ObjectProperty".equals(type) && entity.isOWLObjectProperty()) {
                                                    workspace.getOWLSelectionModel().setSelectedEntity(entity);
                                                    workspace.displayOWLEntity(entity);
                                                    break;
                                                }
                                                else if ("NamedIndividual".equals(type) && entity.isOWLNamedIndividual()) {
                                                    workspace.getOWLSelectionModel().setSelectedEntity(entity);
                                                    workspace.displayOWLEntity(entity);
                                                    break;
                                                }
                                                else if ("Datatype".equals(type) && entity.isOWLDatatype()) {
                                                    workspace.getOWLSelectionModel().setSelectedEntity(entity);
                                                    workspace.displayOWLEntity(entity);
                                                    break;
                                                }

                                            }
                                        }


                                    }

                                } else {

                                for (OWLEntity entity : workspace.getOWLModelManager().getOWLEntityFinder().getEntities(IRI.create(iri))) {

                                    workspace.getOWLSelectionModel().setSelectedEntity(entity);
                                    workspace.displayOWLEntity(entity);
                                }
                                }
                                
                            }


                        } catch (BadLocationException e1) {
                            e1.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                        }
                    }

                }


            }

            public void mousePressed(MouseEvent e) {
                mouseClicked(e);
            }

            public void mouseReleased(MouseEvent e) {
                //To change body of implemented methods use File | Settings | File Templates.
            }

            public void mouseEntered(MouseEvent e) {
                //To change body of implemented methods use File | Settings | File Templates.
            }

            public void mouseExited(MouseEvent e) {
                //To change body of implemented methods use File | Settings | File Templates.
            }
        });

        this.output.addMouseMotionListener(new MouseMotionListener() {
            int[] previousPosition = null;

            public void mouseDragged(MouseEvent e) {
            }

            public void mouseMoved(MouseEvent e) {
                int[] position = getPositionOfEntity(e.getPoint());


                if (previousPosition != null) {
                    if (Arrays.equals(previousPosition, position)) return;
                }

                if (previousPosition != null) {
                    SimpleAttributeSet sas = new SimpleAttributeSet();
                    StyleConstants.setUnderline(sas, false);
                    setCharaceterAttributes(previousPosition, sas, false);
                    previousPosition = null;
                }

                if (position != null) {
                    SimpleAttributeSet sas = new SimpleAttributeSet();
                    StyleConstants.setUnderline(sas, true);
                    setCharaceterAttributes(position, sas, false);
                    previousPosition = position;
                }
            }

            void setCharaceterAttributes(int[] position, AttributeSet attr, boolean replace) {
                StyledDocument doc = output.getStyledDocument();
                doc.setCharacterAttributes(position[0], position[1] - position[0], attr, replace);
            }

        });


        this.outputThread = new Thread(new Runnable() {
            public void run() {
                BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
                String line = "";
                boolean introFound = false;
                int linesAfterCannotFind = 0;
                boolean firstLine = true;
                try {

                    output.getDocument().insertString(0, "racer", null);

                    int x = 0;
                    while (x > -1) {
                        x = reader.read();
                        output.getDocument().insertString(0, "\nracer " + x, null);
                    }

                    while ((line = reader.readLine()) != null) {
                       // if (cancelListening) break;
                          output.getDocument().insertString(output.getDocument().getEndPosition().getOffset() - 1, "\nsomething read", null);
                        Position position = output.getDocument().getEndPosition();
                        try {

                            line = line.trim();
                            output.getDocument().insertString(0, line + "\n", doc.getStyle("blue"));
                            if (line.replaceAll("\\s", "").length() == 0) continue;
                            if (line.startsWith(";;;")) {
                                introFound = true;
                                continue;
                            }

                            if (line.startsWith("Cannot find \"racerpatches\"")) {
                                linesAfterCannotFind = 1;
                                continue;
                            }
                            if ((linesAfterCannotFind == 1 || linesAfterCannotFind == 2) && !introFound) {
                                continue;
                            }

                            if (isIgnore(line))
                                continue;

                            if (isHighlight(line)) {
                                if (line.contains("\t")) line = "T! " + line;
                                if (line.contains("\n")) line = "N! " + line;
                                if (line.contains("\f")) line = "F! " + line;
                                if (line.contains("\r")) line = "R! " + line;
                                if (firstLine) {
                                    output.getDocument().insertString(0, line + "\n", doc.getStyle("red"));
                                    firstLine = false;
                                } else {
                                    output.getDocument().insertString(position.getOffset() - 1, line + "\n", doc.getStyle("red"));
                                }

                            } else {

                                if (line.contains("\t")) line = "T! " + line;
                                if (line.contains("\n")) line = "N! " + line;
                                if (line.contains("\f")) line = "F! " + line;
                                if (line.contains("\r")) line = "R! " + line;

                                if (firstLine) {
                                    output.getDocument().insertString(0, line + "\n", doc.getStyle("blue"));
                                    firstLine = false;
                                } else {
                                    output.getDocument().insertString(position.getOffset() - 1, line + "\n", doc.getStyle("blue"));
                                }


                            }
                        } catch (Exception ec) {
                            System.out.println(ec);
                        }
                    }
                     output.getDocument().insertString(0, "finsihed", null);
                } catch (IOException e) { System.out.println(e);

                } catch (BadLocationException e) {
                    e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                } finally {
                    try {
                        reader.close();
                    } catch (IOException e) {
                         //System.out.println("closed " + e);
                    }
                }
            }
        });

        this.errorThread = new Thread(new Runnable() {
            public void run() {
                BufferedReader reader = new BufferedReader(new InputStreamReader(errorStream));
                String line = "";
                try {
                    while ((line = reader.readLine()) != null) {
                        if (cancelListening) break;
                        line = "error " + line;
                        Position position = output.getDocument().getEndPosition();
                        try {
                            output.getDocument().insertString(position.getOffset(), "\n" + line, doc.getStyle("red"));
                        } catch (Exception ec) {
                        }
                    }
                } catch (IOException e) {

                } finally {
                    try {
                        reader.close();
                    } catch (IOException e) {

                    }
                }
            }
        });
        outputThread.start();
        errorThread.start();





    }
       Thread t ;

    public void StopListening() {
        cancelListening = true;

        setVisible(false);
    }

    public void RestartListening() {

        if (outputThread != null && errorThread != null) {
            cancelListening = false;
            if (!outputThread.isAlive()) {
                outputThread.start();

            }
        }
        setVisible(true);
    }

}
