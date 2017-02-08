README: JCHRONOSS Version 2.0 {#mainpage}
===============================================================================

__Last Updated:__ *February 6, 2017*

JCHRONOSS is a parallel and resilient front end, particularly designed for HPC
validation suites. Many projects are built with a validation system not
scaled to be efficient and self-contained whatever the environment the
validation runs. Even if the work done in the project deals with cutting-edge
technology, the fact that validation process does not take advantage of the
maximum peak provided by a cluster is insane. This led us to take the time to
think about an all-in-one validation tool, able to interact easily with a large
scope of clusters and/or environments, in order to make a testing system
instantly ready-to-use. This what JCHRONOSS is all about. 

The JCHRONOSS's goal is, from a given set of jobs, to distribute the workload
over the validation machines and in a mandated testing environment. From a
simple and clear data formatting, the idea is to provide integration
platform-compatible output, aimed to be reviewed by third-party tools, for
example with the Jenkins software. To make it easy, JCHRONOSS can run any
commands which could be run in a standard shell and captures and complete output for
later analysis.

To accomplish that, JCHRONOSS provides an abstraction mechanism, making a
distinction between what matters of testing and what is not. As an example,
the semantics of launching jobs over a given cluster and its job manager, is
absolutely not specific to the validation. This information should not be
considered when trying to validate a standard project. JCHRONOSS is
not bounded to a kind of system to run jobs and provides a high level of
flexibility, through a number of customizable scripts, allowing the end user to
make JCHRONOSS as close as possible to cluster needs. Finally, around this
core model, JCHRONOSS provides some extensible mini-applications, helping you to
build your validation systems with and review test results with JCHRONOSS.
Probably not as powerful as dedicated software, these extensions will be
helpful to have immediate reviewing access, far away from any continuous
integration solution.

__Quick links:__

* For building JCHRONOSS, please refer to [INSTALL](@ref installpage) page;
* For a Quick-Start Documentation, please refer to [GETTING-STARTED](@ref gettingstartedpage) page;
* To see how to generate well-formatted input data for JCHRONOSS, everything is
  explained [here](@ref inputpage);
* A complete explanation of `jchronoss` options are available at [OPTIONS](@ref optionspage) page;
* To list tools provided by JCHRONOSS implementation, read [TOOLS](@ref toolspage) page.

The whole project has been documented with Doxygen. You can generate the global
documentation by running `make doc` in your build. The documentation is then
available in `${BUILD}/doc/build/` and under two formats: HTML &
ready-to-compile Latex report. All MarkDown documentation files are included in
it, to gather at the same place all information you need. Be aware that Markdown
inclusion inside a Doxygen is only supported after the version 1.8.x.

License
--------------------------------------------------------------------------------

JCHRONOSS is distributed under CeCILL-C license and abiding by the rules of
distribution of free software. The fact that you are presently reading this 
means that you have had knowledge of the CeCILL-C license and that you accept 
its terms. You can find the CeCILL-C license in COPYRIGHT file, provided with
JCHRONOSS source code.

In order to always make our tool better, we are open for suggestions, remarks or
questions about this work. Please address your requests to the authors:

* Julien Adam <adamj@paratools.com>
* Marc Pérache <marc.perache@cea.fr>

Third-party tools (dependencies)
--------------------------------------------------------------------------------

* LibXML2-devel: <http://xmlsoft.org/>
* CMake: <http://cmake.org/>
* (optional) XSLT engine like <http://xmlsoft.org/XSLT/xsltproc2.html>
* (optional) Python: <https://www.python.org/>
* (optional) svUnitTest: <https://github.com/svalat/svUnitTest.git/>
* (optional) LibEV : <http://software.schmorp.de/pkg/libev.html> 
* (integrated) Jsoncpp: <https://github.com/open-source-parsers/jsoncpp/>
* (integrated) LibWebSockets (LWS): <https://libwebsockets.org/>

