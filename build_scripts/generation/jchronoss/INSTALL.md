INSTALL: JCHRONOSS Version 2.0 {#installpage}
===============================================================================

__Last Updated:__ *February 6, 2017*

We will describe how to compile and install JCHRONOSS and explain options
affecting the build. JCHRONOSS is compiled with strict flags: `-Wall -pedantic
-ansi -Wextra` to reduce the number of hidden pitfalls. We aim to have no
warnings at compiling time, even with these flags. If you find one, please send
us an email, mentioning the JCHRONOSS version and the compiler. The only known
exception is under Clang. CMake forwarding any compilation flags to the link
step, Clang does not support `-ansi` at link time. Furthermore, in case of some
dependency compilation, some warnings implied by included headers have been
hidden through GCC `system_headers` pragma, and could be visible under other
compilers.  JCHRONOSS has been initially developed under GCC 4.8.1 and is now
maintained under GCC 6.2.0 version. We tested our validation suite and some
validations run at regular basis under the following compilers:

| Compiler | Tested Versions | 
| :------: | :-------------: | 
|   GCC    |      4.4.7      |
|   GCC    |      4.5.4      |
|   GCC    |      4.6.3      |
|   GCC    |      4.7.1      |
|   GCC    |      4.8.x      |
|   GCC    |      4.9.1      |
|   GCC    |      5.4.0      |
|   GCC    |      6.2.0      |
|  Clang |      3.4        |
|   ICC    |     12.1.3      |
|   ICC    |     13.1.3      |
|   ICC    |     14.0.3      |
|   ICC    |     15.0.0      |

In a short way, JCHRONOSS can be built as a standard CMake build:

	$user> cd JCHRONOSS && mkdir -p build && cd build 
	$user> cmake .. 
	$user> make install

By default, JCHRONOSS will be deployed into `/usr/local/bin` and you will
probably need higher privileges for that. To choose a different prefix, you can
use `-DCMAKE_INSTALL_PREFIX` macro at `cmake` step. The next section will
describe configuration options which can be given to CMake run, in order to
customize the JCHRONOSS installation.

CMake configuration
--------------------------------------------------------------------------------

### Core Options ###############################################################

Core options depicts actions taken in the main JCHRONOSS binary to add/remove
features in it. We distinguish here two parts: CMake-specific options and
JCHRONOSS-specific options.

#### CMake-specific options ####################################################

All CMake variables are available for this project. Among them, we will
describe here ones which can be useful for your installation:

* `CC=` and `CXX=`: once set in the environment, before calling `cmake`, you can
  alter the default compiler used by JCHRONOSS to be built;
* `-DCMAKE_BUILD_TYPE`: Switch Release/Debug mode. If a SEGV happens, please try
  compiling with `-DCMAKE_BUILD_TYPE=Debug` and check if an assert triggers the
  same error. If so, please don't hesitate to send us an email with your minimal
  trace.
* `CFLAGS`, `CXXFLAGS` and `LDFLAGS`: if you want to add your customized flags
  to JCHRONOSS build.

#### JCHRONOSS-specific options ################################################

In this section, we will describe all CMake macros affecting JCHRONOSS code
model. We will add between parenthesis the default value. To change behavior of
any of these variables, use the syntax `-DENABLE_*=ON|OFF`:

* `ENABLE_OPENMP` (ON): Enable/Disable OpenMP support for JCHRONOSS. OpenMP is
  used to accelerate input parsing and should be remain active in most cases;

* `ENABLE_TESTS` (OFF): Enable/Disable the test-suite building; Each test will
  be compiled and JCHRONOSS is built as a library to facilitate unit-test
  validation.

* `ENABLE_TRACE` (OFF): Enable/Disable scheduling trace generation. This option
  is used to produce \*.trace file, designed to build scheduling graph through
  the JsLoc tool. These graphs helps users to detect if their runs are
  consistent with a given cluster;

* `ENABLE_PROFILING` (OFF): Enable/Disable the lightweight profiling model. By
  activating this option, JCHRONOSS will print time elapsed in preset code
  sections. This should only be used for debug.

* `ENABLE_COLOR` (OFF): Enable/Disable colorized output.

* `ENABLE_ALL` (OFF): Enable/Disable all previous options.

* `ENABLE_SSL` (OFF): Experimental, enabling SSL support for JCHRONOSS. In a
  near future, JCHRONOSS should add a security layer under its communication
  protocol to avoid troubles induced by multi-users cluster management.

### Plugin options #############################################################

JCHRONOSS provides some extensions to its core model, designed as plugins. These
plugins can be built by requesting their activation from CMake configuration
macros. Default values are shown between parenthesis:

* `ENABLE_PLUGIN_SERVER` (OFF): Enable/disable real-time support inside
JCHRONOSS. Real-time support is a functionality allowing to review results
before the end of JCHRONOSS;
* ENABLE_PLUGIN_CURSES (OFF): Enable/Disable Curses-based front end, runnable
in a deported way. This extension connects to a running JCHRONOSS validation,
and display contents directly inside the terminal. __This plugin is WIP.__
* ENABLE_PLUGIN_JENKINS (OFF): Enable/Disable JCHRONOSS plugin designed for
Jenkins integration. __This plugin is WIP.__
* ENABLE_PLUGIN_ALL (OFF): Enable/Disable all options above.

### Dependency Options #########################################################

For now, whatever your environment, some dependencies are automatically built,
we do not provide any way to select an internal implementation. Dependency
detections are based on Find\*.cmake calls (`find_package()` through
CMakeLists.txt) and the selection capability depends on the implementation. This
way, it is not possible to select __Doxygen and LibXML__ in custom installation
but by setting their locations in your own paths.

LibWebSockets (LWS) dependency is automatically built if you enabled the
real-time plugin. This project being based on CMake, both tools are fully
integrated. However, be sure to have libwebsockets dependencies deployed in your
environment.

Finally, we provide a way to specify a custom installation for LibEV. This
avoids creating a dirty environment by setting multiple variables. To
define a custom-defined path to LibEV, use `-DLIBEV_PATH=` macro. This is
possible because we decided to embed FindLibEV.cmake inside JCHRONOSS and we can
then modify it. This is not always possible with other dependencies. If you
installed dependencies we mentioned here in custom paths and you absolutely want
a dedicated macro to set them, please send us an email request.

Makefile Targets
-------------------------------------------------------------------------------

Once you configured your makefile, you are ready to build JCHRONOSS. The main
target you can run is `jchronoss`. This will create the binary inside
`${BUILD}/src/`. Some other targets exist:
* `jchronoss_online_server`: build the server binary (in case of
`ENABLE_PLUGIN_SERVER`).
* `jchronoss_curses`: build the curses-based app (in case of
ENABLE_PLUGIN_CURSES).

In order to validate the tool, some other targets are available if you provided
`ENABLE_TESTS`:
* `testSuite`: Build the "library" version of JCHRONOSS
* Test\*Bin: Build test binary.
* test: Run the test suite.

You can run "CPack" tool from CMake system by running `make package`. This will
generate lightweight debian and redhat packages, alongside with compressed
archives in multiple formats. Furthermore, a complete, user-friendly,
documentation can be generated with `make doc`. One you validate with `make
test` and ensure everything is good, you can install it by running `make
install`.  Be sure to provide `CMAKE_INSTALL_PREFIX` with a path where you have
write-access permissions.

