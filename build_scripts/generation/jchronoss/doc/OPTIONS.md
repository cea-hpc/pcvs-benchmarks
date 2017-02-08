OPTIONS: JCHRONOSS Version 2.0 {#optionspage}
===============================================================================

__Last Updated:__ *February 6, 2017*

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

__WARNING:__ For now, no explicit binding are done by JCHRONOSS. This option
means that no more that four 1-core jobs will be scheduled at the same time, but
it does not means that the same 4 cores will be used. This distribution is
handled by the operating system. It is part or our milestone to effectively
bound each test to the real resources.


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

### CHECKPOINT - RESTART ######################################################

JCHRONOSS have a checkpoint/restart mechanism (CR) enabled by defaut, making it
more flexible with HPC requirements. In case of interrupt, JCHRONOSS can restart
from a previous validation point and finish validation without any job result
losing.

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



