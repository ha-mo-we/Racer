LRacer is a Common Lisp API for accessing a RacerPro 2.0 server.  At least
RacerPro 2.0 is required for this version of LRacer. LRacer comes with
complete source code. LRacer is based on TCP. LRacer was tested with ACL 8.1
(mlisp & alisp), LispWorks 5.1, and SBCL 1.0.29 + SLIME, and CLisp 2.44.1.

Load the file lracer-sysdcl.lisp and evaluate

(compile-load-lracer "<<<directory to where you copied lracer>>>" t), 

e.g.

(compile-load-lracer "~/lracer/" t)

to compile and load LRacer. In future sessions, the "t" argument can be
ommited in order to skip the compilation, e.g. evaluate

(compile-load-lracer "~/lracer/")

to load LRacer. 

Note: if run from CLisp, absolute pathnames have to be used here.  Moreover,
logical pathnames will not work.

Then, make sure that a RacerPro 2.0 server is running and that it accepts
connections on port 8088 (the RacerPro default port, read further if you need
to change that port).  You can test LRacer by simply evaluating
"(lracer-test)" and "(lracer-urf8-test)". For the UTF8 test, RacerPro must be
running in UTF8 mode (read further below).

All Racer functions and macros documented in the Users' Guide and Reference
Manuals can be used directly from Lisp, e.g.

cl-user 3 > (instance i c)
t

cl-user 4 > (instance i (not c))
t

cl-user 5 > (abox-consistent?)
nil

For the first function call, a TCP socket connection to the RacerPro server is
opened.  The connection is kept open and reused for further requests, until
explicitly closed via "(close-server-connection)".  Also note the variables
*keep-alive* (which is "t" by default) and the "with-server-connection" macro.

You can set the variables

*default-racer-host* (default "localhost")

*default-racer-tcp-port* (default 8088)

and 

*external-format* (default is :utf-8 for SBCL and ACL) 

to fit your environment.

In order for UTF8 to work correctly, RacerPro must be running in UTF8 mode:

./RacerPro -- -ef @utf8 
./RacerPro -- -ef @utf8-base
./RacerPro -- -ef @(:e-cr :utf8-base)

Please evaluate "(lracer-utf8-test)" and also take a look at the files
"lracer-utf8-test.lisp" and "family-j-utf8.racer" from the "demo"
subdirectory, which is used as the test KB. Note that these files are in UTF8
encoding, so you will need an editor supporting UTF8, e.g.  Emacs, in order to
see the Japanese characters. On Windows, you will have to install support for
Asian Languages etc. In Emacs, there is the "Ctrl-x RET f" command for setting
the file coding system of a buffer. UTF8 will only work on SBCL and ACL, not
on LispWorks.

Please note that the "-ef" external format argument of RacerPro specifies also
the end of line (EOL) convention; e.g., in the last example, the Macintosh EOL
convention (single CR) is specified. For more information about external
formats, please refer to

http://www.franz.com/support/documentation/8.1/doc/iacl.htm#external-formats-1

LRacer can be used with case sensitive modern Lisps (such as mlisp from Franz
Inc.), but also with standard case insensitive (ANSI) Lisps (e.g., Lispworks,
SBCL). Internally, RacerPro 2.0 is case sensitive.  LRacer tries its best to
ensure that no difficulties will appear if used in a case insensitive Lisp
(such as Lispworks). This, however, requires that certain symbols are
automatically converted.  For example, RacerPro (and mlisp) uses lower case
symbols "t" and "nil" to represent boolean values, e.g. answers to boolean
queries (predicates).  In case such a boolean answer is returned by RacerPro
to LRacer running in a case insensitive Lisp, the symbols "t" and "nil" have
to be upcased / converted to "T" and "NIL" in order to retain their boolean
semantics (without upcasing, these symbols could only be represented as "|t|",
"|nil|", but both would be considered as "true" in a case-insensitive Lisp,
since only "NIL" has the semantics of a boolean value false). In contrast, if
LRacer runs in a case sensitive modern Lisp (e.g., mlisp from Franz Inc.),
then these symbols are *not* converted.  A number of other symbols is affected
as well by this automatic case transformations, e.g., RacerPro optional
(keyword) arguments to functions, etc.

Acknowledgments:

Many thanks to Mr. KURODA Hisao from Mathematical Systems Inc., Tokyo, Japan,
for testing LRacer and providing valuable suggestions and the "family-j"
knowledge base.

Many thanks also to Nikodemus Siivola for providing a patch for SBCL 1.0.29
for Windows, allowing to set / change the external format of the standard CL
streams (e.g., *standard-output*).
