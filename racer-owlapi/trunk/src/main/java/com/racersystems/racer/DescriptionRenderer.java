package com.racersystems.racer;

import com.racersystems.racer.util.NonShortFormProvider;
import com.racersystems.racer.util.RacerPrefixManager;
import com.racersystems.racer.util.ShortFormProvider;
import org.semanticweb.owlapi.model.*;

import java.io.IOException;
import java.io.Writer;


/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/
public class DescriptionRenderer implements OWLClassExpressionVisitor, OWLPropertyExpressionVisitor, OWLDataVisitor, OWLNamedObjectVisitor, OWLIndividualVisitor {
    protected final ShortFormProvider shortFormProvider;
    private final Writer writer;
    RacerPrefixManager manager;


    public DescriptionRenderer(Writer writer) {
        this(new RacerPrefixManager(), writer, new NonShortFormProvider());
    }

    public DescriptionRenderer(RacerPrefixManager manager, Writer writer, ShortFormProvider shortFormProvider) {
        this.writer = writer;
        this.shortFormProvider = shortFormProvider;
        this.manager = manager;
    }

    public ShortFormProvider getShortFormProvider() {
        return this.shortFormProvider;
    }


    protected final void writeS(final IRI uri) throws IOException {
        writer.write(RacerKRSSVocabulary.SPACE);
        writer.write(shortFormProvider.shortForm(uri));
    }


    /**
     * Writes a string with an starting space signs.
     *
     * @param s
     */
    protected final void writeS(final String s) throws IOException {
        writer.write(RacerKRSSVocabulary.SPACE);
        writer.write(s);
    }

    private void writeS(int i) throws IOException {
        writer.write(RacerKRSSVocabulary.SPACE);
        writer.write(String.valueOf(i));
    }

    protected final void write(final String s) throws IOException {
        writer.write(s);
    }

    public final void writeOpenPar() throws IOException {
        writer.write(RacerKRSSVocabulary.OPEN_BRACKET);
    }

    public final void writeClosePar() throws IOException {
        writer.write(RacerKRSSVocabulary.CLOSE_BRACKET);
    }

    public final void write(final OWLClassExpression obj) throws IOException {
        write(RacerKRSSVocabulary.SPACE);
        obj.accept(this);
    }

    public final void write(final OWLNamedIndividual ind) throws IOException {
        write(RacerKRSSVocabulary.SPACE);
        ind.accept((OWLIndividualVisitor) this);
    }

    public final void write(final OWLAnonymousIndividual ind) throws IOException {
        write(RacerKRSSVocabulary.SPACE);
        ind.accept(this);
    }

    public final void write(final OWLIndividual ind) throws IOException {
        if (ind.isAnonymous())
            write(ind.asOWLAnonymousIndividual());
        else
            write(ind.asOWLNamedIndividual());
    }

    public final void write(final OWLPropertyExpression obj) throws IOException {
        write(RacerKRSSVocabulary.SPACE);
        obj.accept(this);
    }

    public final void write(final OWLAnnotationProperty obj) throws IOException {
        write(RacerKRSSVocabulary.SPACE);
        obj.accept(this);
    }

    public final void write(final OWLDataRange obj) throws IOException {
        write(RacerKRSSVocabulary.SPACE);
        obj.accept(this);
    }



    public final void visit(final OWLClass desc) {
        try {
     //       writeS(manager.getShortForm(desc.getIRI()));
            writeS(desc.getIRI());

        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    public final void visit(final OWLDataProperty property) {
        try {
            writeS(property.getIRI());
              //writeS(manager.getShortForm(property.getIRI()));
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    public final void visit(final OWLObjectProperty property) {
        try {
            writeS(property.getIRI());
            //writeS(manager.getShortForm(property.getIRI()));

        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    public void visit(OWLNamedIndividual individual) {
        try {
            writeS(individual.getIRI());
           // writeS(manager.getShortForm(individual.getIRI()));

        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    public void visit(OWLAnonymousIndividual individual) {
        try {
            writer.write(RacerKRSSVocabulary.SPACE);
         //   writer.write(RacerKRSSVocabulary.DELIMITER_LINE);
            writer.write( RacerKRSSVocabulary.DELIMITER_LINE +individual.getID().getID()+RacerKRSSVocabulary.DELIMITER_LINE);
           // writer.write(RacerKRSSVocabulary.DELIMITER_LINE);
            //writeS(individual.getID().getID());
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    public final void visit(final OWLObjectMinCardinality desc) {
        try {
            writeOpenPar();
            write(RacerKRSSVocabulary.AT_LEAST);
            writeS(desc.getCardinality());
            write(desc.getProperty());
            if (desc.isQualified())
                write(desc.getFiller());
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    public final void visit(final OWLObjectMaxCardinality desc) {
        try {
            writeOpenPar();
            write(RacerKRSSVocabulary.AT_MOST);
            writeS(desc.getCardinality());
            write(desc.getProperty());
            if (desc.isQualified())
                write(desc.getFiller());
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    public final void visit(final OWLObjectExactCardinality desc) {
        try {
            writeOpenPar();
            write(RacerKRSSVocabulary.EXACTLY);
            writeS(desc.getCardinality());
            write(desc.getProperty());
            if (desc.isQualified())
                write(desc.getFiller());
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    public final void visit(final OWLObjectAllValuesFrom desc) {
        try {
            writeOpenPar();
            write(RacerKRSSVocabulary.ALL);
            write(desc.getProperty());
            write(desc.getFiller());
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    public final void visit(final OWLObjectSomeValuesFrom desc) {
        try {
            writeOpenPar();
            write(RacerKRSSVocabulary.SOME);
            write(desc.getProperty());
            write(desc.getFiller());
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }

    }


    public final void visit(final OWLDataExactCardinality desc) {
        try {
            writeOpenPar();
            write(RacerKRSSVocabulary.DEXACTLY);
            writeS(desc.getCardinality());
            write(desc.getProperty());
            if (desc.isQualified())
                write(desc.getFiller());
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }

    }

    public final void visit(final OWLDataMaxCardinality desc) {
        try {
            writeOpenPar();
            write(RacerKRSSVocabulary.DAT_MOST);
            writeS(desc.getCardinality());
            write(desc.getProperty());
            if (desc.isQualified())
                write(desc.getFiller());
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }

    }

    public final void visit(final OWLDataMinCardinality desc) {
        try {
            writeOpenPar();
            write(RacerKRSSVocabulary.DAT_LEAST);
            writeS(desc.getCardinality());
            write(desc.getProperty());
            if (desc.isQualified())
                write(desc.getFiller());
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }

    }


    public final void visit(final OWLObjectIntersectionOf desc) {
        try {
            writeOpenPar();
            write(RacerKRSSVocabulary.AND);
            for (OWLClassExpression operand : desc.getOperands()) {
                write(operand);
            }
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }

    }

    public final void visit(final OWLObjectUnionOf desc) {
        try {
            writeOpenPar();
            write(RacerKRSSVocabulary.OR);
            for (OWLClassExpression operand : desc.getOperands()) {
                write(operand);
            }
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }

    }

    public final void visit(final OWLObjectComplementOf desc) {
        try {
            writeOpenPar();
            write(RacerKRSSVocabulary.NOT);
            write(desc.getOperand());
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }

    }

    public final void visit(final OWLObjectInverseOf property) {
        try {
            writeOpenPar();
            write(RacerKRSSVocabulary.INV);
            write(property.getInverse());
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }

    }

    public final void visit(final OWLDataHasValue desc) {
        try {
            writeOpenPar();
            write(RacerOWLAPIVocabulary.DFILLER);
            desc.getProperty().accept(this);
            desc.getValue().accept(this);
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    public final void visit(final OWLObjectOneOf desc) {
        try {
            writeOpenPar();
            write(RacerKRSSVocabulary.ONE_OF);
            for (OWLIndividual individual : desc.getIndividuals())
                if (individual.isAnonymous())
                    write(individual.asOWLAnonymousIndividual());
                else
                    write(individual.asOWLNamedIndividual());
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }

    }

    public void visit(OWLOntology ontology) {
    }


    public void visit(final OWLDatatype node) {
        try {
            final IRI uri = node.getIRI();
            /*if (XSDVocabulary.ALL_DATATYPES.contains(uri.toURI())) {
                writeOpenPar();
                write(OWLAPI_DBASETYPE);
                writeS(shortFormProvider.shortForm(uri));
                writeClosePar();
            } else */
            {
                writeOpenPar();
                write(RacerOWLAPIVocabulary.OWLAPI_DBASETYPE);
             //   writeS(shortFormProvider.shortForm(XSDVocabulary.ANY_TYPE.getIRI()));
                   writeS(shortFormProvider.shortForm(uri));
                writeClosePar();
            }
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    public void visit(OWLDataComplementOf node) {
        try {
            writeOpenPar();
            write(RacerOWLAPIVocabulary.DCOMPLEMENT);
            node.getDataRange().accept(this);
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }

    }

    public void visit(OWLDataOneOf node) {
        try {
            writeOpenPar();
            write(RacerOWLAPIVocabulary.DPOSSIBLEVALUES);
            for (OWLLiteral constant : node.getValues())
                constant.accept(this);
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }

    }

    public void visit(OWLDatatypeRestriction node) {
        try {
            writeOpenPar();
            writeS(RacerOWLAPIVocabulary.DDATARANGE);
            node.getDatatype().accept((OWLDataVisitor) this);
            for (OWLFacetRestriction facet : node.getFacetRestrictions()) {
                this.visit(facet);
            }
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    public void visit(OWLFacetRestriction node) {
        try {
            writeOpenPar();
            writeS(RacerOWLAPIVocabulary.DFACET);
            writeS(shortFormProvider.shortForm(node.getFacet().getIRI()));
            node.getFacetValue().accept(this);
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    public void visit(final OWLDataAllValuesFrom desc) {
        try {
            writeOpenPar();
            write(RacerOWLAPIVocabulary.DALL);
            write(desc.getProperty());
            write(desc.getFiller());
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    public void visit(final OWLDataSomeValuesFrom desc) {
        try {
            writeOpenPar();
            write(RacerOWLAPIVocabulary.DSOME);
            write(desc.getProperty());
            write(desc.getFiller());
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }

    }

    public void visit(OWLLiteral owlLiteral) {
        try {
            writeOpenPar();
            write(RacerOWLAPIVocabulary.OWLAPI_DLITERAL);
            write(RacerKRSSVocabulary.SPACE);
            write(RacerKRSSVocabulary.QUOTE);
            write(makePretty(owlLiteral.getLiteral()));
            write(RacerKRSSVocabulary.QUOTE);
            write(RacerKRSSVocabulary.SPACE);
            visit(owlLiteral.getDatatype());
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

    protected String makePretty(String s) {
        String res = s.replaceAll("\n", "\\\\N");
    //    res = res.replace(",","P");
        return res.replace("\"", "\\S");
    }

    public void visit(OWLDataIntersectionOf node) {
    }

    public void visit(OWLDataUnionOf node) {
    }

    public void visit(OWLAnnotationProperty property) {
    }

    public void visit(OWLObjectHasSelf ce) {
    }

    //////////////////////////////////////////////
    //
    // Annotations
    //
    //////////////////////////////////////////////


    /* public void visit(OWLObjectAnnotation annotation) {
      try {
          writeOpenPar();
          write(ANNOTATION);
          writeS(annotation.getAnnotationURI());
          if (annotation.isAnnotationByConstant()) {
              annotation.getAnnotationValueAsConstant().accept(this);
          } else {
              annotation.getAnnotationValue().accept(this);
          }
          writeClosePar();
      } catch (IOException e) {
          throw new OWLRuntimeException(e);
      }
  }  */

    /* public void visit(OWLConstantAnnotation annotation) {
    try {
        writeOpenPar();
        write(ANNOTATION);
        writeS(annotation.getAnnotationURI());
        annotation.getAnnotationValue().accept(this);
        writeClosePar();
    } catch (IOException e) {
        throw new OWLRuntimeException(e);
    }

}    */


    public void visit(OWLObjectHasValue desc) {
        try {
            writeOpenPar();
            write(RacerKRSSVocabulary.HAS_VALUE);
            desc.getProperty().accept(this);
            desc.getValue().accept(this);
            writeClosePar();
        } catch (IOException e) {
            throw new OWLRuntimeException(e);
        }
    }

}
