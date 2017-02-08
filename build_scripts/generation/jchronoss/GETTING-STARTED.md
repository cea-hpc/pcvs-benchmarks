GETTING-STARTED: JCHRONOSS Version 2.0 {#gettingstartedpage}
===============================================================================

__Last Updated:__ *January 19, 2017*

Installation
-------------------------------------------------------------------------------

To see a complete documentation on the installation method, please follow the
[installation page](@ref installpage).

1. If needed and not already done, un-compress the JCHRONOSS archive (replace
X.X with your version number):

	$user> tar xf JCHRONOSS-X.X.tar.gz

2. Configure JCHRONOSS with CMake. By default, JCHRONOSS is installed into
`/usr/local/bin`. If you want to choose a different prefix, please use
`-DMAKE_INSTALL_PREFIX` macro at `cmake` step.

	$user /> mkdir build && cd build/
	$user /build/> cmake .. -DCMAKE_INSTALL_PREFIX=/tmp/install_jchronoss

3. Install JCHRONOSS:

	$user /build/> make install

Command-line Usage
-------------------------------------------------------------------------------

You will find a complete documentation of options by following this link:
[OPTIONS](@ref optionspage). For examples below, we will suppose your current
working directory is located in \${INSTALL\_PREFIX}.

1. Create input data for a JCHRONOSS run. Input data are XML files, formatted
and validated in a specific format, shown and detailed in the [dedicated page](@ref inputpage).
   An XML file describes a set of jobs. A job is at least a name associated with
a command to execute. For the purpose of this guide, we will suppose a file
named `my_test.xml` containing around 100 jobs. A minimalist file would be
the following job description, repeated 100 times:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.xml}
<?xml version="1.0" ?>
<jobSuite>
	<job>
		<name>my_first_test</name>
		<command>echo "First Test"</command>
	</job>
</jobSuite>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

2. Provide this XML file as an argument of the JCHRONOSS binary. You can add as
much file as you want with the command line. Be aware that
the more input files you provide, the faster parsing will be. All default
options will allow you to execute this first test on any architecture: 

	$user> ./bin/jchronoss ./my_test.xml

3. Custom your run by adding command-line arguments to JCHRONOSS. You can
consult a complete list with `-h|--help` option, or by reading the detailed
documentation [here](@ref optionspage). The most used will be the number of
resources you will dedicate to your run. Please, be sure to fully understand
that this does not really *bind* tests to hardware resources (follow the
documentation). For example, to schedule JCHRONOSS over 4 cores on the
current node:

	$user> ./jchronoss --nb-resources 4 ./my_test.xml


4. __ADVANCED__: Select the parallelism level for your execution. Basically,
   this means the max number of allocations you permit to make. This can be
   related to job-manager command. In case of SLURM for example, it sets the
   maximum number of `srun` command which can be run concurrently. By reading
   [this page](@ref optionspage), you will learn further about "slaves/workers":

	$user> ./bin/jchronoss --nb-resources 4 --nb-slaves 2  ./my_test.xml

6. __ADVANCED__: Provide the script to be called each time JCHRONOSS needs to
make an invocation to the job manager. For example, just before job launch,
JCHRONOSS allocates resources over the cluster. The way these resources are
allocated can be caught by this user script. Some samples can be found in
`tools/` directory:

	$user> ./bin/jchronoss --nb-resources 4 --nb-slaves 2 --launcher
	./script.sh ./my_test.xml

5. __ADVANCED__: Select a scheduling approach to pack jobs inside one cluster
allocation. To see detailed information about scheduling policies, please
refer to [this page.](@ref optionspage). In summary, we have 3 policies,
numbered from {0, 1, 2}.

	$user /build/> ./bin/jchronoss -r 4 -j 2 -p{0,1,2} ./my_test.xml
