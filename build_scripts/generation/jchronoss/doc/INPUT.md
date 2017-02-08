INPUT: JCHRONOSS Version 2.0 {#inputpage}
===============================================================================

__Last Updated:__ *February 6, 2017*

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

the behavior will be the same. Structure of packages is not file-system
dependant.  You choose your own package splitting. By convention, it is better
to have a folder per package, it is easier to browse through the tree.

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
So, the syntax is important. You can see below a sample of what the
configuration looks like:
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

### LAUNCHER WRAPPER ##########################################################



