RacerPro 2.0 Preview now supports the latest OWL2 functional-style
syntax (.ofn), the OWL2 XML syntax (.owx), and OWL2 RDF (.owl). All
axioms as of November 22, 2009 are now supported by the parsers. But
please note that some OWL2 axioms are currently ignored for reasoning,
though. We are planning to update the RacerPro reasoning engine to
OWL2 ASAP. The included example files are modified version of the
official OWL2 functional syntax included taken from

http://www.w3.org/TR/2009/REC-owl2-primer-20091027/ 

as of November 22, 2009. We have fixed some obvious errors from the
official OWL primer and modified it in such a way that the ontologies
are consistent (the official OWL primer is currently inconsistent) and
added some axioms here and there in order to ensure that all three
variants contain the same set of logical axioms (this wasn't the
case).  Also, the import statements were removed since the imported
ontologies are not accessible on the Web.

In order to test the new OWL2 parsers of RacerPro, simply use the
RacerPorter "Load..." button to load one of the primer ontologies in
this folder (the *mod versions). Answer the query "Shall I create
OWLAPI Axioms?" witch yes if you are planning to export / save the
ontology in a different syntax using RacerPro's new OWL2 rendering
engine, or if want to access the axioms via RacerPro's native
OWLAPI-like interface, the so-called NOSA (New OWLAPI Support
API). Also, if you are planning to inspect and edit the axioms with
RacerPorters axiom editors in the "Axioms" tab, answer the query with
"Yes".

If OWLAPI axioms are created in the NOSA, then the original axioms are
kept as told and can be used to render the original ontology in a
different form, or you can work with the axioms programmatically via
the NOSA. If the axioms are not preserved, then RacerPro directly
encodes the axioms into its own (logically equivalent) internal
representation and thus, the original form is lost. For example, axiom
annotations axioms are discarded then, and logical axioms will be
rewritten into a logically equivalent form in most cases. However,
maintaining the OWLAPI axioms in the NOSA "as told" required quite
some memory for bigger ontologies. So, both options have their pros
and cons.

RacerPro has a new generic import / load function now which is behind 
the "Load..." button of RacerPorter. The function

(OWLAPI-readOntology
       <fn-or-URI> 
       :maintain-owlapi-axioms
       t)

analyzes the input syntax and selected the appropriate parser. In
order to bypass the syntax analysis, use the functions

- owl-read-file, owl-read-document
- OWLAPI-readFunctionalOntologyFile, OWLAPI-readFunctionalOntologyDocument
- OWLAPI-readXMLOntologyFile, OWLAPI-readXMLOntologyDocument

(whereas the *Document versions accept URLs, the *File versions accept
filenames only). readOntology simply delegates to these functions.

Also, import statements in ontologies can now import any of these
syntaxes, thus, it is possible to import OWL XML from OWL RDF, etc.
Cross-syntax import is thus possible now.

If ":maintain-owlapi-axioms t" is specified, RacerPro will keep and
maintain the original axioms as told. The Racer-internal
implementation of the OWLAPI (the so-called NOSA interface) allows
you to access the individual axioms as told to the reasoner. Storing
the axioms in their original form requires as lot of memory. 

RacerPro now also has three new OWL rendering functions which can
render a NOSA ontology container containing OWLAPI / NOSA axioms as
either OWL RDF, OWL XML, or OWL Functional. To test this
functionality, simply use the axioms tab of RacerPorter (given that
NOSA axioms are present), select an non-empty "Ontology Container"
from the list of ontologies (axioms will be shown in the axioms tab in
case the container is non-empty), and press the "Export Ont Axioms..."
button. In order to export as OWL Functional, use ".ofn" (or ".funct")
as the file type. In order to export as OWL RDF, use ".owl". Or use
".owx" (or ".xml") to export as OWL XML.

There is now one new generic export / ontology rendering function: 

 (OWLAPI-saveOntology 
       <ontology-container>
       <fn> 
       :syntax <syntax>)

where syntax is one of :ofn (:funct, :functional), :owl (:rdf), :owx
(:xml). This function is behind the RacerPorter "Export Ont Axioms..."
button.  Also, :racer and :lisp are accepted as file types. The
ontology container is then rendered in the NOSA representation.

To use the classic OWL export functionality (without the NOSA, e.g.,
in case the ontology is too big such that the NOSA axioms cannot be
kept and maintained for reasons of memory consumption), use the
RacerPorter shell and lookup the documentation of the classical API
function save-abox, save-tbox, save-kb, using appropriate :syntax
keyword arguments.

In order to access the NOSA and its axioms, use the functions which
begin with "OWLAPI-". Or simply use RacerPorter and its axioms tab to
learn about the available functionality of the NOSA. Or use the
RacerPorter Shell and enter "(OWLAPI- " into the RacerPorter Shell and
press "Tab" (Completion) to learn about the available functionality.

For example, to retrieve the axioms from the owl-primer use 

? (OWLAPI-getAxioms)

(OWLAPI-getAxiomsPerOntology), 

and related functions. 

Please note that RacerPorter now also allows graphical editing of
these axioms (go to the "Axioms" tab, given that the axioms have been
kept).

