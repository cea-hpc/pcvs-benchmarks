DOCUMENTATION (Version 1.2) Last updated February 23th, 2015     {#mainpage}
===============================================================================



PREAMBLE 
-------------------------------------------------------------------------------

JCHRONOSS is a tool used as a validation driving force, particularly suited
for parallel environments. Its aim is, from a given lists of tasks, to distribute
this workload over the machine used to start the validation.  The integrity and
coherency of tasks is maintained over the whole validation. A task can be many
things. A single task is started with `sh -c` calls and support a lot of commands.
Thus, a test can represent any command that can be launched from a shell. As the
majority of validation are started from a shell, this technique allows us to be
as generic as possible.

The JCHRONOSS philosophy is to make a distinction between what is validation 
specific and what is not. Typically, tasks (or jobs, as we like to call them), 
should no be job-manager dependent. JCHRONOSS allows to split jobs handling and
cluster specifications. Thus, this tool can be used with multiple batch managers
(SLURM, Torque...), provided that user gives methods and routines to apply for
the current cluster. So, JCHRONOSS is batch manager independent. We will see later
how this behavior is possible.  So many functionalities are available with this
tool as a filter system to specify which jobs to execute or not or multiple
scheduling policy, in order to be as close as possible to the optimal global
execution time for the given test-suite.




HOW-TO : CONFIGURATION, COMPILATION AND INSTALLATION
-------------------------------------------------------------------------------

This section describes how to compile and install JCHRONOSS, explaining options
impacting tool behavior. JCHRONOSS is compiled with the following flags : 
__-Wall -pedantic -ansi -Wextra__ . There should be no warnings, even with these
 flags (except for Clang + CMake, -ansi is an invalid flag for linking step and
throws a warning).  JCHRONOSS has been developed under GCC 4.8.1 and the
whole Test-Suite has been launched under the following compilers:

| Compiler | Tested Versions |  Status  |       Comments      | 
| :------: |-----------------|----------|---------------------| 
|   GCC    |      4.4.7      |    OK    |                     |
|   GCC    |      4.5.4      |    OK    |                     |
|   GCC    |      4.6.3      |    OK    |                     |
|   GCC    |      4.7.1      |    OK    |                     |
|   GCC    |      4.8.3      |    OK    |                     |
|   GCC    |      4.9.1      |    OK    |                     |
|  Clang   |      3.4        |  * OK *  | Without OpenMP lib  |
|   ICC    |     12.1.3      |    OK    |                     |
|   ICC    |     13.1.3      |    OK    |                     |
|   ICC    |     14.0.3      |    OK    |                     |
|   ICC    |     15.0.0      |    OK    |                     |

First of all, to simply compile the project:

	$user> cd JCHRONOSS && mkdir -p build && cd build 
	$user> cmake .. 
	$user> make
	$user> make install

By default, the `make install` command simply copies the JCHRONOSS binary file into
/usr/local/bin. So, you need high privileges to do this. However, you can stop
at `make` step, the application will work as well.



#### CONFIGURATION ############################################################

On the one hand, there are some options to customize CMake producing. It is
possible to enable test-suite module from JCHRONOSS:

	$user> cmake .. -DENABLE_TEST_SUITE=yes 

or enable trace module (used by external tool, JsLoc, to visualize scheduling
policy):

	$user> cmake ..	-DENABLE_TRACE=yes 

or activate timer print (used for performance measures) :
	
	$user> cmake .. -DENABLE_MEASURE=yes

and even a color mode =) :

	$user> cmake .. -DENABLE_COLOR=yes

All these options can be enabled at once:

	$user> cmake ..	-DENABLE_ALL=yes 

On the other hand, there are some options to improve tool functionalities. The
main one is the possibility to enable or disable OpenMP using at compile time.
In fact, this option is really used when we do not want to use OpenMP whereas CMake
has found the OpenMP module. If not, OpenMP is automatically enabled/disabled,
according to module request:

	$user> cmake .. -DDISABLE_OPENMP=yes 


Finally, you can change the prefix for installation with the command:

	$user> cmake .. -DCMAKE_INSTALL_PREFIX=/path/as/prefix

Of course, all other CMake variables can be set at configuration time, the most
useful are:
 * CMAKE_BUILD_TYPE : Defines type of build (Debug or Release)
 * CMAKE_VERBOSE_MAKEFILE : Enable verbose mode during compilation
 * EXECUTABLE_OUTPUT_PATH : Where output files will be stored
 * CMAKE_CXX_FLAGS : Some extra flags to pass to the compilation
 * etc...



#### COMPILATION ##############################################################

Some target are available to customize JCHRONOSS compilation through CMake.
First of all, the most known option is -j, allowing to start multiple
independent targets simultaneously. According to what specified when CMake
creates makefiles, this command can be compile either the tool, the test-suite
or both:

	$user> make -j 

If you want to just execute the test-suite :
	$user> make test

To generate the documentation (through doxygen, generates HTML and Latex
documentation)

	$user> make doc



#### INSTALLATION #############################################################

Once compilation ended, you can directly use the software as is. However you can
also run `make install` to install JCHRONOSS on your system, by default in
/usr/local/bin (WARNING: Root privileges needed). Moreover, if you want to export
or install JCHRONOSS from packages, simply run:

	$user> make package

and CMake will create zip, tgz, bz2, deb and rpm packages. Keep in mind that,
for this step, you will need, respectively, gzip, tar -z, tar -j, debhelper and
rpmbuild. Only binary packages are kept and not source ones.




HOW-TO : USING 
-------------------------------------------------------------------------------

JCHRONOSS is designed to be callable from anywhere. There is no relative path
and each file is registered in absolute way. As we see later, some options
allow to define accurately where each type of data has to be stored.



### GLOBAL OPTIONS ############################################################

This section describes how to use JCHRONOSS, making choices allowing it to do 
exactly what you want. In this documentation, we always use long options for more
lisibility. If you want to check short ones, you can run :
	
	$user> jchronoss --help

The most simple using of JCHRONOSS is the following command:
	
	$user> jchronoss

By default, the application is started as a master (a slave should not be
launched by end user). This command does nothing, excepts displaying default
configuration used by JCHRONOSS if nothing is specified. To force explicitly the
master mode as current, you can use `--master` option:

	$user> jchronoss --master

In order to provide something to do to JCHRONOSS, you have to pass by command
line the complete list of XML files containing jobs to run. Be careful about
XML files syntax, they will be validated by a XML schema and any invalid file
will be refused (if so, a warning message is displayed):

	$user> jchronoss --master ./xmlFile1 ./xmlFile2 ./xmlFile3 

Finally, if you want to specify a configuration file (see below), use the
command:

	$user> jchronoss --config-file=./xmlConfFile



### JOB OPTIONS ###############################################################

Here will be described options that modifies jobs handling by JCHRONOSS, i.e.
data handling, time limits, etc...

First, verbosity. There are three verbosity levels, according to the information
precision you require:
 1. __Silent__ : Used by default, no useless printing
 2. __verbose__ : print output for failed jobs (some jobs can have important
 output)
 3. __very verbose__ : print everything, every time (can be very verbose and
 quickly unreadable).

A special mode is available only if compiled in Debug mode, with specific prints
used to debug in case of issues. This mode should not be used for a normal usage.

To use these modes, you can try one of the following commands:
	
	$user> jchronoss ./xmlFile1 --silent
	$user> jchronoss ./xmlFile1 --verbose
	$user> jchronoss ./xmlFile1 --very-verbose

An options with argument is also available:

	$user> jchronoss ./xmlFile1 --verbosity=X

where X belongs to S = {0, 1, 2}

Second, logging. The "logging" defines which level of verbosity to keep when
data will be pushed into XML output files. There are four levels:
 1. __none__ : Nothins is kept (neither failed or passed jobs);
 2. __fail__ : Keep output for only failed jobs
 3. __success__ : keep output for only suceess jobs
 4. __all__ : keep everything at any time

To use these modes, you can try one of the following commands:

	$user> jchronoss ./xmlFile1 --keep-none
	$user> jchronoss ./xmlFile1 --keep-fail
	$user> jchronoss ./xmlFile1 --keep-sucess
	$user> jchronoss ./xmlFile1 --keep-all

An option with argument is also available:

	$user> jchronoss ./xmlFile1 --keep=X

where X belongs to S = {0, 1, 2, 3} 

Next, Filters. JCHRONOSS is able to filter jobs in order to remove (or keep)
specific jobs defined in separated files. When a job name matches a name
from a filters list, the job is kept or removed according to the corresponding 
filter type. A whitelist contains jobs which have to be executed exclusively.
Conversely, a blacklist contains jobs which must not be executed. These two modes
are compatible. However if a job name appears in several files, it is the
name from the last file parsed which will be compared to.

To add a white/black lists files to JCHRONOSS, use the command:
	
	$user> jchronoss ./xmlFile1 --white=file1,fil2
	$user> jchronoss ./xmlFile1 --black=file1,fil2

You can provide as many of files as you want, concatenate them, as
comma-separated list.

Next time limits. For job configuration, there are, for now, only one time limit
to define: the max time allowed to a job to execute. This time is used to
schedule jobs and fill a worker for time policy. By defualt this value is set to
300 seconds.
To redefine this value, use the command:
	
	$user> jchronoss ./xmlFile1 --maxt-job=X

Where X have to be an non-negative integer

Finally, the last option is not really useful for end users. It is possible to
simulate execution time for a given jobs suite. This aspect is useful for
schecduling policies testing. After policy implementation, we check its
behavior with several clusters and generate a trace with filling rate. In order
to save time, jobs time are mocked and a random number is drawn between 0
(exclusive) and 10 (inclusive) with an applied coefficient to affect range
distribution.
To enable/disable this feature, use the command:
	
	$user> jchronoss ./xmlFile1 --fake
	$user> jchronoss ./xmlFile1 --no-fake



### SYSTEM OPTIONS ############################################################

Here will be described options that modifies JCHRONOSS behaviour on system
handling and interaction with him. Options that we will show you impact the
whole configuration and are available for each components in JCHRONOSS
application.

As we explain later, JCHRONOSS is path-independent. There are some options
allowing to specify where each type of data should be stored. If you want to
store produced files (XML output files, traces...), use:

	$user> jchronoss ./xmlFile1 --output=/path/to/folder

In this case, all output files will be stored in the same folder. Thus, if you
provide input files with same names (but differents folders) and you specify the
`--output` option, JCHRONOSS will prefix all files with an arbitrary number, to
distinct each file. (see below to visualize output files format). All files
follows the format output-{unique_id}-{original_filename}.xml

If you want to specify where temporary files should be stored (like binary
files), use:

	$user> jchronoss ./xmlFile1 --build=/path/to/folder

By default current path is used (path from where JCHRONOSS has been called : "./")

Next, you must provide a number of available resources on which JCHRONOSS can
apply its scheduling policy. A job requires a number of resources for working
(which can be everything, even an abstract representation). A number of available
resources is provided to JCHRONOSS, which tries to optimize job launching for
the best resources rendering. Of course, the number of resources must be a
non-negative integer. Without this value, the tool is unable to schedule jobs:

	$user> jchronoss ./xmlFile1 --nb-resources=X

Equally, you can specify the number of slaves allowed to start simultaneously.
This option is very useful when, for example, you use a batch manager which
only permits a defined number of parallel jobs at the same time. To define this
value, use the command:
	
	$user> jchronoss ./xmlFile1 --nb-slaves=X

Now time limits. You can define accurately the time on which a slave can work.
Each of these arguments is optional. Default value for min is obviously zero and
the max value is set to infinite. If neither min nor max is defined, only one
worker will take the whole set of jobs (only on scheduling by time (policy 2)).

	$user> jchronoss ./xmlFile1 --mint-slave=X --maxt-slave=Y

Of course, the assertion X < Y will be checked and will throw an error if false.

Next, the launchers are an important part in JCHRONOSS concept and especially
for using in parallel environment. A launcher is a script (or a set of scripts)
which will be launched as a wrapper at each slave starting. The aim of a
launcher is to prepare the environment for the slave execution. In HPC, a good way
to save time is to allocate a set of nodes and launch multiples jobs in it
rather than allocate a specific number of nodes every time for each job. In order
to do this, we provide the capability to give two launching scripts. The first 
one is used for the first JCHRONOSS launch (compilation), and the second
one is used for the others launches.
For standard starting:

	$user> jchronoss ./xmlFile1 --launcher=/path/to/file

or for compilation starting:
	
	$user> jchronoss ./xmlFile1 --compil-launcher=/path/to/file


Next, the autokill. It is possible to provide to JCHRONOSS a timeout after which
the process is killed and current jobs results are written down in their files,
respectively. This option is used by the master to kill the slave when the
allowed time is exceeded (see `--maxt-slave` option). But, it can also be used
by the user to specify to the application how much time it has to execute jobs.
However, this option does not ensure all jobs will be executed. It just allows to
kill the master process and back up current results.
To use autokill, use the command:

	$user> jchronoss ./xmlFile1 --autokill=X

where X is a non-negative integer.

The next one in checkpoint-restart relative. To understand this mechanism, 
please see _Advanced Concepts_ below. When JCHRONOSS checkpoints, a file is 
created in build directory ("." by default). When JCHRONOSS is interrupted, 
the contents can be dumped in JCHRONOSS again with :

	$user> jchronoss --restart=/path/to/json/file

The last option in the system configuration is the scheduling policy. In
JCHRONOSS, four policies have been configured:
 1. __Default Policy__ (DefaultPolicy)
 	This scheduling policy is the default one. Its aim is to consider each
	job as a unique work. Thus each job is launched individually in a
	separate worker. This is the behavior used by default if nothing is
	specified. It is not the best policy but it is the least aggressive for
	the cluster.  A job is not currently considered as indepedent from the
	others, it is a policy used to launch one worker per job. This method
	is closer to a classical sequential validation. It can be used on small
	cluster or where huge allocations are discouraged.
 2. __Scheduling by resources policy__ (SchedResPolicy)
 	This scheduling policy tries to create a group of similar jobs, according
	to their number of required resources. Rather than allocate multiple 
	times the same number or resources, these will be allocated once, and 
	one worker will be responsible of a group of tests which requires exactly
	the same number of resources. Of course, the worker will launch each job
	sequentially in its own environment. Without being very aggressive, this
	policy has the advantage to easily save time thanks to the number of
	allocations reduction.
 3. __Scheduling by time policy__   (SchedTimePolicy)
	This scheduling policy is more rendering oriented. The aim here is to
	fill with a maximum of jobs, a defined amount of work (whatever to be
	executed or not), and let the time to schedule by himself. We can see
	this policy as if we would want to fill bag with stones. No matter what
	stone we take, as long as the bag is filled. Then, the slave will
	launch jobs thanks to a "greedy" algorithm, in order to optimize idle
	time. This policy can be slower to generate worker contents but will be
	more effective than others. For now, jobs times are estimated but this
	policy should handle effective time jobs to be more performant.
 4. __Custom policy__ (CustomPolicy)
 	Actually, this policy is not operable as is. It is a defined template,
	available for users who wants to implements their own policy into the
	scheduler. All informations about classes, objects and struct to
	manipulate will be detailed in the template file itself.

To use an another policy as default(0), you can use the command:
	
	$user> jchronoss ./xmlFile1 --policy=X

where X must be in S = {0, 1, 2, 3}


### CONFIGURATION FILE ########################################################

In order to simplify command line when JCHRONOSS is often used with same options
set, it is possible to define a XML file with common options we see above, and
avoid to repeat them in command line. The loading arguments chain is as follow:
 
 1. Loading default configuration registered in implementation
 2. Override defined options from configuration file
 3. Override with command line

Then, each level can set same of different options sets, no conflict will be
raised, the higher layer will set the option. First, see how to specify a
configuration file to the tool:


As we see it for job XML files, this configuration xml file is validated over a
XML schema and in case of invalidity, the whole configuration file is rejected.
So, the syntax is important. You can see below a sample of what the configuration
looks like:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.xml}
<?xml version="1.0" ?>
<configuration>
	<job>
		<jobslist></jobslist>
		<whitelist></whitelist>
		<blacklist></blacklist>
		<verbosity></verbosity>
		<logging></logging>
		<fakeExecution></fakeExecution>
		<maxJobTime></maxJobTime>
		<maxNbJobs></maxNbJobs>
	</job>
	<system>
		<jobsCommand></jobsCommand>
		<compilationCommand></compilationCommand>
		<output></output>
		<build></build>
		<maxResources></maxResources>
		<maxSlaves></maxSlaves>
		<maxSlaveTime></maxSlaveTime>
		<minSlaveTime></minSlaveTime>
		<policy></policy>
		<autokill></autokill>
	</system>
</configuration>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Keep in mind each sub-entity in __job__ or __system__ entities is optional. If
omitted, the value of lowest layer is kept.

__WARNING__ : For now, entities which concerns INT values cannot be empty ! So
please, either remove the entity or comment it if you do not want to use it in
the configuration file. Actually, the attribute "nillable" in XML schema seems
to not be understood by libxml2 when parsing. This warning does not concern 
string entities.




ADVANCED CONCEPTS
-------------------------------------------------------------------------------

Here, we provide extra information about JCHRONOSS, about internals and I/O 
handling.

### HPC HANDLING ##############################################################

##### OpenMP ##################################################################

If desired, OpenMP can be used to increase some parts of JCHRONOSS. It can be
enabled or disabled by CMake (whether OpenMP module is found or not), and, in 
case of success, can be manually disabled with `DISABLE_OPENMP` flag to yes to 
CMake generation. 

First is input XML file parsing. JCHRONOSS creates a parallel section for XML 
file reading, each tasks being independent. A "by hand" reduce on max is done 
(because reduction on max operator is not available on all compilers we work with) 
to determine what is the biggest job, in terms or resources requirements. This 
tip is used to sensibly reduce job manager size.

Second is dependencies resolving. As each job have to iterate over the entire
jobs lists to find its dependency, this behaviour have been improved with 
OpenMP. As OpenMP does not handle (yet?) the `parallel for` directive with C++ 
container, we create a threads pool, sharing workload. It is not the best 
solution for now. If threads are distributed on cores located on different 
sockets, speedup is degraded.

__WARNING__ : JCHRONOSS does not set any number of threads. So, please check 
your OMP_NUM_THREADS value before running JCHRONOSS, to use multiple threads.
By default, this value is set with the number of availables cores on node. But,
in supercomputers case, OMP_NUM_THREADS can be set to 1 by default (probably an 
administrator decision) on login node, in order to avoid node squashing. As 
JCHRONOSS run (for now) on node where it is started, default value of 
OMP_NUM_THREADS will be 1. If you increase this value, please be careful to not 
make node overloaded on interactive nodes.

### CHECKPOINT - RESTART ######################################################

JCHRONOSS have a checkpoint/restart mechanism (CR) making him more flexible with
HPC requirements. In case of interrupt, JCHRONOSS can restart from a previous
validation point and finish validation without any job result losing.

#### CHECKPOINT ###############################################################

JCHRONOSS create a backup with main component, necessary to restart. This backup 
contains :

* The complete configuration (Global, job-specific and system-specific)
* The complete job manager (job lists, executed job lists)
* Running worload (clone from hash table)

All data are stored in one file, where content is structured in JSon format 
(I/O with JsonCpp library, which sources are integrated with JCHRONOSS sources). 
Content is "compressed", not really human-readable. But this file is text-filled 
and can take a lot of space on disk (for example, a complete backup from 35,000 
not-run jobs is around 15 MB). A backup file is created after each worker launch. 
In case of default policy using, this can have an important overhead. On big 
workers (i.e. schedByTimes policy), this solution allows to never losing what 
have already been done. 

A checkpoint does not interrupt JCHRONOSS. it is done just before JCHRONOSS 
waits for a remote process finishing. Thus, cluster remain fullfilled during 
checkpointing.

#### RESTART ##################################################################

After an interruption, JCHRONOSS can be restarted from the backup Json file 
stored in build directory ("." by default). With this, all current configuration 
or files (or whatever) previously provided to JCHRONOSS is deleted and replaced 
by the one stored in backup file. Thus, it is not possible to add a verbose 
option in restart mode if this option had not been provided first time. Jobs 
lists are reset to their previous states. JCHRONOSS then restarts as no 
interruption had happened. 

### INPUT INJECTION ###########################################################

JCHRONOSS reads two types of entries:
 1. jobs depicted as XML files
 2. filters depicted as one-line lists.

Here is an example of what a XML input file looks like:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.xml}
<?xml version="1.0" ?>
<jobSuite package="package1">
	<job>
		<name>compile_01</name>
		<command>make</command>
		<rc>0</rc>
		<resources>1</resources>
		<constraints>
			<constraint>compilation</constraint>
		</constraints>
		
	</job>
	<job>
		<name>test_01</name>
		<command>make test</command>
		<rc>0</rc>
		<resources>4</resources>
		<deps>
			<dep>compile_01</dep>
		</deps>
	</job>
</jobSuite>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A job is depicted by its name and its command. Other entities are optional. By
default, the return code is zero, the number of resources is zero, there are
neither dependences nor constraints. If a job have to be launched in first step,
set a constraint with tag "compilation". If a test have an other one as
dependances, simply fill an entity __dep__ with the dep name. The whole entity
__jobSuite__ gathers all jobs from same (sub-)package. Here this package is
named "package1". This denomination allows to distinguish some jobs which would
have the same name but are not extracted from the same layer. This attribute 
allows also to apply a accurate filtering on what to be executed or not. The job
command can be a lot of things, as long as this command is understandable by a
shell interpreter. For further details on what is needed or not, you can read
jobValidator.xsd in /tools/resources/ from root project directory. It is this
XML schema which is used to validate job files (actually, the XML schema is
hardly implemented in source code, so, updating this file will have no effect).

The next type of input supported by JCHRONOSS are filter files. To describe a
test which should not be executed, you have to specify the job name and complete
path depicted as packages and subpackages. For example, if the job tree is
depicted like this :
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
- /
  |-- Package1
  |   |-- subPackage1
  |   |   |-- test1
  |   |   |-- test2
  |   |
  |   |--subPackage2
  |       |-- test1
  |       |-- test2
  |  
  |-- Package2
      |-- subPackage1
      |   |-- test1
      |   |-- test2
      |
      |-- subPackage2
          |-- test1
          |-- test2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
and if you want to execute only the test 2 located in subpackage 2 from package
2, you can provide the following list as a blacklist (--black option):
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
package1.subpackage1.test1
package1.subpackage1.test2
package1.subpackage2.test1
package1.subpackage2.test2
package2.subpackage1.test1
package2.subpackage1.test2
package2.subpackage2.test1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

or this one as a whitelist (--white option):

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.sh}
package2.subpackage2.test2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

the behavior will be the same. Structure of packages is not file-system dependant.
You choose your own package splitting. By convention, it is better to have a 
folder per package, it is easier to browse through the tree.

### OUTPUT PRODUCING ##########################################################

JCHRONOSS produces two types of data. First are JUnit files containing jobs
results. The format is XML as you can see below:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.xml}
<?xml version="1.0"?>
<testsuite name="package1" failures="1" success="14" errors="0" skipped="2" time="0.06">
	<testcase classname="package1" name="compile_01" time="0.01" > </testcase>
	<testcase classname="package1" name="compile_02" time="0.00" >
		<failure message="echo compile_02" type="ExecutionError"/>
		<system-out>compile_02</system-out>
	</testcase>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are a lot of information in this XML file. The root entity gathers
information about the number of passed, failed, skipped jobs and global time
elapsed. Each job result owns his time and the output is kept according to
provided options. One way to visualize results as a web page, a webview is
provided by JCHRONOSS, to extract data from JUnit and generate HTML for a
standalone reading.

### TRACES GENERATION #########################################################

Some traces about scheduling can be generated, with the objective of analyze.
Files generated consists of entries, each one corresponding to a job. You can
find the job name, its starting and ending timestamp and the number of required
resources. This file is not usable as is and has to be formated by an external
tool called JsLoc (Job Scheduling LOCality). This software will build SVG files
drawing job distribution over available resources. Thus, it is easy to visualize
if scheduling policy is good for this run, and have to be adjusted if not. This
option is not very useful for a standard usage of JCHRONOSS. However, when it is
the first time you launch a run on a cluster, it can be interesting to see how
the computer reacts with our policies and adapt it in order to have the yield as
good as possible. Just for the example, you can see below an example of
traces-generated files (.trace extension) :

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#nbJobs:3:1408532729.42
test_13:1408532729.44:1408532729.44:3
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to interpret these files, you can start JsLoc (located in
/tools/jsLoc/) and run the following command :

	$user> ./jsLoc_gen_all.sh --in=/path/to/file --res=<number_of_resources>

You can see :
	
	$user> ./jsLoc_gen_all.sh --help

for further explanations.

### VIEWING RESULTS ###########################################################

In order to consult data, you can use every platform with a JUnit compliance.
The format used by JCHRONOSS follows a XML Scheme available on the Web. But, as it
is not alway possible to have network access, JCHRONOSS embeds a stand alone 
visualizer, especially developed to display JCHRONOSS raw data. It is developed
thanks to XSLT so you will need to __xsltproc__ if you want to use it. With this
module, you may have a summary about the run, divided by packages and a detailed
view about each test (according to options you provide, the verbosity level may
vary). It will be possible (I hope) in next versions, to generate a diff between
the new build and a previous one. Thanks to that, it will inform you about
improving/regression in your jobs.

This tool is possible thanks to Sebastien Valat <sebastien.valat.dev@orange.fr> 
who have provided webview structure, under CeCILL-C license. 

### COMMAND-LINE GENERATOR ####################################################

With the objective of simplification, JCHRONOSS embeds a little module allowing
to generate the command line according to fields filled in a web form. All
available options with JCHRONOSS can be set from this form. And, in order to
make your life easier, it is also possible to generate configuration and jobs
file from these modules. To use it, simply start in your favorite web browser
the /tools/commandGen/index.html.

MISCELLANEOUS
-------------------------------------------------------------------------------

The whole project has been documented with Doxygen comments. It is possible to
generate the documentation with `doxygen` command or by using `make doc` if
using CMake. The current global documentation is available on the main page of
doxygen generation. However, as markdown support is a recent feature from
doxygen, it is possible that this documentation does not appear in HTML/Latex
formats (doxygen \< 1.8.X)

### DEPENDENCIES ##############################################################

* LibXML2-devel : <http://xmlsoft.org/>
* CMake : <http://cmake.org/>
* (optional) Python : <https://www.python.org/>
* (optional) svUnitTest : <https://github.com/svalat/svUnitTest.git/>
* (integrated) Jsoncpp : <https://github.com/open-source-parsers/jsoncpp/>

### CONTACT ###################################################################

For any request about the project, its concepts or something else, please
contact at <julien.adam@cea.fr> or <marc.perache@cea.fr>

### LICENCE ###################################################################

JCHRONOSS is distributed under CeCILL-C licence and abiding by the rules of
distribution of free software. The fact that you are presently reading this 
means that you have had knowledge of the CeCILL-C license and that you accept 
its terms. You can find the CeCILL-C licence in *.COPYRIGHT provided with
JCHRONOSS source code.
