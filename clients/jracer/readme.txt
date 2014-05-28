JRacer is a Java API for accessing a RacerPro 2.0 server.  At least RacerPro
2.0 is required for this version of LRacer. JRacer comes with complete source
code in the folder "com/racersystems/jracer". The "RacerClient" class provides
a simple socket-based interface to RacerPro.

The files "test*.jar" illustrate the basic use of JRacer as described in the
RacerPro 2.0 Users Guide. They are helpful for following the step-by-step
instructions given there. For your convenience we set the filename of JRacer
to "jracer.jar". Please don't mix it with "racer.jar" which can be found in 
the "OWLAPI" subdirectory, also included in this package. Please refer to the
Users Guide on more information about how to use "racer.jar" and the OWLAPI
to access the RacerPro server from your custom Java application. (The included
"racer.jar" adapter for the OWLAPI is identical to "com.racersystems.racer.jar",
the plugin to the Protégé editor.)

Installation of JRacer:

Unpack the "jracer-2-0.zip" archive in some directory (here, "~/temp").  This
will create the "jracer-2-0" directory. Please adjust the line

String peopleAndPets = "\"jracer-2-0/demo/people+pets.owl\"";

in the file "jracer-2-0/com/racersystems/jracer/RacerTest.java" to match
your environment, and also the line 

String jFamily = "\"jracer-2-0/demo/family-j-utf8.racer\""; 

in the file "jracer-2-0/com/racersystems/jracer/RacerUTF8Test.java". 

Then, change to the "jracer-2-0" directory, and compile JRacer as follows:

racer-user@host:~/temp/jracer-2-0> javac com/racersystems/jracer/RacerClient.java

racer-user@host:~/temp/jracer-2-0> javac com/racersystems/jracer/RacerTest.java

racer-user@host:~/temp/jracer-2-0> javac com/racersystems/jracer/RacerUTF8Test.java

Start a RacerPro server in UTF8 mode, using the "-ef @UTF8" command line 
option as follows: RacerPro -- -ef @UTF8 

You can then execute the test programs:

racer-user@host:~/temp/jracer-2-0> java com/racersystems/jracer/RacerTest

racer-user@host:~/temp/jracer-2-0> java com/racersystems/jracer/RacerUTF8Test

For each native Racer function, a set of (overloaded) Java methods is provided
by the class "RacerStubs" (which is extended by "RacerClient"). The signatures
of these Java methods are computed from the signatures of the corresponding
native Racer functions. The number of obligatory arguments is
identical. However, in case a Racer function accept a number of optional
arguments, multiple overloaded Java methods for the same API function are
provided, representing the different invocation possibilities (one method for
calls without optional arguments, one method for calls with one provided
actual argument, one method for calls with two provided optional arguments,
etc.)  There is no direct equivalent for the so-called keyword arguments of
the native Racer functions.  In case a Racer function accepts keyword
arguments, then the Java method is also equipped with a Java ellipsis that
additionally accepts an arbitrary (possibly zero) number of additional
arguments. Here is an example call, demonstrating how to supply arguments for
the keyword parameters ":how-many" and ":exclude-permutations":

   racer.racerAnswerQuery$("(?x ?y)",
                           "(and (?x #!:person) (?x ?y #!:has_pet))",   
                           ":how-many",3,
                           ":exclude-permutations",true);

It is also easy to iterate over such a result: 

   RacerList<RacerList<RacerList<RacerSymbol>>> res = 
       (RacerList<RacerList<RacerList<RacerSymbol>>>)
            racer.racerAnswerQuery$("(?x ?y)","(and (?x #!:person) (?x ?y #!:has_pet))"); 

   for (RacerList<RacerList<RacerSymbol>> bindings : res) {
       for (RacerList<RacerSymbol> binding : bindings) {
            for (RacerSymbol varval : binding) {
                System.out.println(varval);
            }
       }
  } 

Each set of (overloaded) Java methods for a single RacerPro API function comes
in two disguises - one set of methods returning Strings, and another set of
methods returning structured "RacerResult" instances, which allow for easy
iteration etc. "RacerLists" are special "RacerResults" which can be iterated
over (by employing ArrayLists). The methods returning structured collections
("RacerResult"s) end with "$". Moreover, Java methods which represent a
RacerPro predicate (in this case, the RacerPro function ends with a "?" or
"-p") end with a "P". In addition, RacerPro API functions which are macros have
an "M" in their names. For example, "transitiveMP" means that the RacerPro
predicate "transitive?" is a macro. In fact, there is no difference regarding
functions and macro from the Java perspective. However, this suffices are
needed in order the collision of function and macro names (they might have
different signatures).

Please take a look into the files "RacerTest.java" and "RacerUTF8Test.java"
for more examples and demonstration of usage of JRacer. Note that the latter
file is UTF8 encoded; your editor / IDE should show some Japanese characters
in this file. On Windows, make sure support for Asian Languages and character
sets / code pages is installed. In Emacs, there is the "Ctrl-x RET f" command
for setting the file coding system of a buffer.

By default, the client prints no Racer warning messages. This behavior can be
modified using the "setPrintWarningMessages(boolean printWarningMessages)"
method. Also, there is a set of variables in "RacerClient.java" which can be
adjusted for debugging puposes:

   private boolean debugging = false;
   private boolean useStringBuildersEvenForTokens = false; 

Acknowledgments:

Many thanks to Mr. Atila Kaya for adding UTF8 support to the socket connection
and for drastically speeding up the communication.

Many thanks to Mr. KURODA Hisao from Mathematical Systems Inc., Tokyo, Japan
for providing the Japanese version of the "family.racer" knowledge base.

The OWLAPI adapter was developed by Olaf Noppens of Derivo GmbH, Ulm, Germany
and is courtesy of Racer Systems GmbH & Co., Hamburg, Germany.
