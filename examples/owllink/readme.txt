OWLlink (see http://www.owllink.org/) is the successor of DIG, a
standard protocol for accessing (telling and asking) OWL
reasoners. This directory contains the standard OWLlink example
messages as of March 18, 2010 in OWLLink XML binding (folder xml),
the OWLlink functional-style binding (folder funct), and the OWLlink
SExpression binding (folder sexpr).

At the time of this writing, RacerPro is the de-facto "reference
implementation" of OWLlink, and Racer Systems has actively contributed
to and supported the development of the protocol. Moreover, the
functional-style and SExpression bindings have been developed by Racer
Systems.

In order to test the OWLlink interface in XML binding, start RacerPro
with the "-protocol OWLlinK" command line argument, e.g.,

RacerPro -- -protocol OWLlink

Then, use a tool such a cURL to send an OWLlink message to RacerPro as
follows:

curl -0 -d@owllink-example-Prefix-request-20091023.xml http://localhost:8080

RacerPro will return the following answer message: 

<?xml version="1.0" encoding="UTF-8"?>
<ResponseMessage xmlns="http://www.owllink.org/owllink-xml#"
                 xmlns:owl="http://www.w3.org/2002/07/owl#">
  <KB kb="http://www.owllink.org/examples/KB_2"/>
  <OK/>
  <OK/>
  <SetOfClassSynsets>
    <ClassSynset>
      <owl:Class IRI="http://www.owllink.org/test/ont#A"/>
    </ClassSynset>
  </SetOfClassSynsets>
  <SetOfClassSynsets>
    <ClassSynset>
      <owl:Class IRI="http://www.owllink.org/test/ont#A"/>
    </ClassSynset>
  </SetOfClassSynsets>
  <OK/>
  <SetOfClassSynsets>
    <ClassSynset>
      <owl:Class abbreviatedIRI="test:A"/>
    </ClassSynset>
  </SetOfClassSynsets>
  <SetOfClassSynsets>
    <ClassSynset>
      <owl:Class abbreviatedIRI="test:A"/>
    </ClassSynset>
  </SetOfClassSynsets>
  <OK/>
</ResponseMessage>

The full OWLlink specification can be found here:
http://www.owllink.org/

To test one of the alternative bindings, use the options

-owllink-input-syntax 

and 

-owllink-output-syntax 

Both arguments accept "function", "sexpr", and "xml" as arguments. For
example, use

./RacerPro -- -protocol OWLlink 
  -owllink-input-syntax  functional
  -owllink-output-syntax functional

to test the OWLlink functional binding, and

./RacerPro -- -protocol OWLlink
  -owllink-input-syntax  sexpr 
  -owllink-output-syntax sexpr

to test the SExpression binding.

Moreover, RacerPro can be used to convert messages (without processing
them) from one OWLlink binding into another. E.g, to convert OWLlink
XML binding messages to OWLlink functional binding messages, use the
"owllink-converter" option for the "protocol" option as follows:

./RacerPro -- -protocol owllink-converter 
  -owllink-input-syntax xml
  -owllink-output-syntax functional

Note: HTTP 1.0 is currently much faster than HTTP 1.1 in the OWLlink
implementation of RacerPro. We therefore recommend using the "-0"
command line option of cURL, as shown above.



