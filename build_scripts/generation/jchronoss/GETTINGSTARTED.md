GETTING STARTED (Version 1.2) Last updated September 27th, 2015    {#starter}
===============================================================================



INSTALLATION
-------------------------------------------------------------------------------

First, untar JCHRONOSS archive :

	$user> tar -xf JCHRONOSS-X.X.tar.gz

Where X.X is the version number.

JCHRONOSS is built with CMake system. Best way to run it is to create a build
directory in root folder and run CMake from it :

	$user /> mkdir build && cd build/
	$user /build/> cmake .. -DENABLE_TEST_SUITE=yes

As shown in main README, multiple CMake macros can affect JCHRONOSS behavior. We
recommand to enable at least JCHRONOSS validation suite, in order to check that
JCHRONOSS fully works on your system. Please note that Pthreads, libxml2 and C++
compiler are needed to compile JCHRONOSS.

Then, compile and check JCHRONOSS works:

	$user /build/> make
	$user /build/> make test

JCRHONOSS binaries can be found into /build/bin directory.


USAGE
-------------------------------------------------------------------------------

#### JCHRONOSS ################################################################
First, you can directly run JCHRONOSS and see what kind of output it produces:
	
	$user /build/> ./bin/jchronoss

You can see the logo, current configuration (the default one, here) and
execution summary with no job, obviously. Now you can give as an argument all
your XML job files. The more files there are, the faster dependency resolving
step will be:

	$user /build/>./bin/jchronoss ../tests/testSimple.xml

Replace ../tests/testSimple.xml with your own XML files.

You can tell to JCHRONOSS how many resources (nodes, sockets, cores...) you want
it uses as a maximum value. Thus, JCHRONOSS can use up to your value with *-r*
option :

	$user /build/> ./jchronoss -r 32 # allocate 32 nodes, for example

Similarly, you can set number max of slaves which are allowed to run
concurrently with *-j* option. JCHRONOSS will split equitably resources among
slaves:

	$user /build/> ./bin/jchronoss -r 32 -j 4 # allocate 32 nodes among 4 slaves

JCHRONOSS implements three different scheduling, enabled with *-p* option:

	$user /build/> ./bin/jchronoss -p0 # default one
	$user /build/> ./bin/jchronoss -p1 # criteria scheduling
	$user /build/> ./bin/jchronoss -p2 # best fit scheduling

To find the best one for your validation suite is to try them all to find the
more efficient on your given cluster.

With a *-b* option, you can define where temporary files will be stored. You
have to be sure JCHRONOSS has write access into it. A common place is /tmp.
Similarly a *-o* option set where output files will be stored.

We recommend you to specify an autokill value. This triggers an SIGALRM when
number of second of execution have been reached by JCHRONOSS. This avoid some
cluster resource squashing, particularly when your have to share a frontend
node:

	$user /build/> ./bin/jchronoss -k 3600 # JCHRONOSS will abort in one hour

Finally, below is what a basic JCHRONOSS command line looks like :

	$user /build/> ./bin/jchronoss
		--output=./res
		--build=/tmp
		-r 32
		-j 4
		-L ../my_launcher
		-k 3600


Please read our complete documentation to understand how JCHRONOSS works and
discover the other beautiful options (especially about colors...).

#### JSLOC ####################################################################

JsLoc is the scheduling visualizer. It can be found in $ROOT/tools/jsloc. It is
really easy to use. You just have to specify the root folder where your output
xml file have been stored. JsLoc will run through all subfolders and will match
with testFile-\*.trace file (created with you have compiled JCHRONOSS with 
-DENABLE_TRACE macro). You also have to provide total number of resources
allowed to JCHRONOSS (value or *-r* option). Be sure to have Python in your
path:

	$user /tools/jsLoc/> ./jsLoc_gen_all.sh --in=../../build/traces/ --res=32

JsLoc will generate one graph per slave, a inter-slave graph and a complete
graph with the complete validation. For a huge number of tests, this last graph
can take a while to build.

#### WEBVIEW ##################################################################

The webview is a standalone result reviewer, complete and lightweight. Like
JsLoc, it is really easy to use. Be sure to have xsltproc in your path. You need
to specify here output xml file are stored (output-\*.xml files):

	$user /tools/webview/> ./webview_gen_all.sh --in=../../build/

