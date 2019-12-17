# Parallel Computing -- Validation Suite #

**Parallel Computing -- Validation Suite** (*PCVS* for short) is a validation engine designed to
help users to run large test bases in a scalable manner, by taking advantage of highly parallel
environments to reduce their time to result, improving subsequently the project efficiency thanks to
a more regular validation process. While designed to answer validation needs in High-Performance
Computing, PCVS is not limited to such projects and can run any kind of test suites over multiple
types of architectures. Through a large range of configuration, PCVS makes no assumptions about any
HPC-related concepts and can easily translate from a validation run on an Exascale machine to a
simple workstation. This README intends to give a simple, global overview of what PCVS can do. For
more details, please refer to the documentation listed [here](#more-in-depth-documentation).

## Requirement & basic setup ##

PCVS is written is Perl 5. It should only be present on any node where `run_validation` can be run.
This means Perl is not a requirement for nodes actually running tests, only where the validation
process can be launched. All subsequent Perl modules are embedded and not external dependencies are
needed. Nothing more is necessary to set up. Unless using Spack as a package manager to handle it,
the framework going to be tested should be loaded into the shell before going further (For instance,
if the purpose is to run the validation over an MPI implementation, the subsequent `mpicc`, `mpic++`
and `mpirun` should be in the `PATH` environment variable).

## Directory layout ##

The actual repository is composed, at the top-level, from files and directories. While all top-level
files are mainly documentation or license, the main entry point users will interact with, is the
`run_validation` script. Then the only mandatory directory is `build_scripts` and contains PCVS
engine code base, as long as the whole configuration steering a validation run. This means that the
only requirement to import/export/copy the PCVS engine consists in carrying the `run_validation`
script and `build_scripts` directory.

Other directories are not mandatory and reflect the current non-regression bases provided with PCVS
bases for direct use. These bases are categorized into multiple directories, mainly depending on the
feature they assess. This categorization is not mandatory but help users to run only what they need,
instead of running the whole validation process. The main categories are MPI and OpenMP but one
could add as many as desired (top-level or subdirectory). As PCVS is a validation engine, it is not
dependent on any presented test bases. To get the full list of directories included by default, one
can use `--list-directories`. Each line of this command can be used as an argument to the `--select`
opt. A list of directories can be provided either by using multiple this option multiple times or by
giving a comma-separated list of paths, as described later.

An important part of PCVS is its ability to be configured to fit the actual needs. This
configuration takes place in `build_scripts/configuration/` (detailed later).

## Usage ##

When `run_validation` is invoked, a complete validation is started. Note that PCVS currently
contains more than 350,000 tests in its default behavior, and running the startup script without any
option can be harmful. One could restrain the test set to be run at a time with the option
`--select`. This option takes a comma-separated list of directories (from top-level) to be searched
for tests. This list can also take subdirectories (for instance `--select=MPI/IMB`).

Once the test set is chosen, PCVS requires some set up to test frameworks, which means answering the
following questions :

* What does building a test mean ? PCVS refers to it as the compiling configuration.
* What does running a test mean ? PCVS refers to it as the runtime configuration.
* Which resource (kind and number) are available to run these tests ? PCVS refers to it as the
  environment configuration.

While a complete documentation about validating a new framework is covered in
[CUSTOMIZATION][CUSTOM] file, PCVS comes with default framework with all configurations already set
up. It includes two MPI frameworks: [OpenMPI][ompi_website] and [MPC][mpc_website]. For instance,
considering the validation of the OpenMPI framework:

* The **compiler configuration** describes how to build a test, depending on the language, various
  supported flavors (OpenMP, CUDA).
* The **runtime configuration** is related to the way of running tests once built. In case of MPI
  runtime, this is mainly represented by the `mpirun` command. It also expresses the mapping between
  test requirements (in terms of MPI processes, number of nodes, etc.) and the actual option/method
  to request those requirements to the runtime. For instance, behind the PCVS naming "number of MPI
  processes,” a good OpenMPI runtime configuration should associate the option name "-np" to it. How
  this filing this file is shown with a higher level of detail in the [CUSTOMIZATION][CUSTOM] file.
* Finally, the **environment configuration** deals with the machine used to run these tests. It
  provides the number of resources used, alongside with characteristics to vary for these tests (for
  instance, any program requiring a number of MPI processes as input will have any value defined
  here under the label "number of MPI processes").

To basically run the validation base over OpenMPI as both the compiler and the runtime, to assess
the support of various MPI scenarios over the Curie cluster, one could invoke:

```sh
./run_validation --target=ompi --select=MPI --env=curie
```

`--target` is an alias for both `--compiler` and `--runtime` option in an attempt to simplify the
command line. These "sub-options" can be useful when either compilation or runtime setups can change
while not the other (considering, for instance, an Intel-flavored vs GCC-flavored MPI
implementation). Please note that options are parsed through the Getopt module, where option name
and value can be given with either space or equal sign. Plenty of options can be given to customize
PCVS to fit with the needs, among the most useful (please read the documentation for a complete
list):

* `--output`: defining where artifacts will be stored during and after the run;
* `--[no]log`: captures stdout and stderr of the actual PCVS output for further usage;
* `--verbose <i>`: select verbosity level when reporting test results, where `ì` can be one of:
  * `0`: none, silent;
  * `1`: print only failed jobs
  * `2`: print everything
* `--list-<token>`: the listing service, where `token` is part of:
  * `directories`: directories eligible to be used with `--select` option;
  * `runtimes`: defined runtime configurations able to be chosen when running a validation;
  * `compilers`: same as above but for compiler configurations;
  * `environments`: same as above but for environment configurations;
* and some more... please consult the `--help|--man` options for more information.

## Key-components used by PCVS ##

### Iterators ###

The core model of PCVS is to keep its semantic agnostic from any framework or benchmark. In order to
create a connection between them without explicitly naming them, we consider that a benchmark always
exposes a "component" and associates to it a given value to it as input. The combination of the
program and this unique input consists of being a "test.” This test is then run by the framework to
assess it. Explaining this through an example will make it clearer. Consider a component named
"number of MPI processes". A benchmark (=set of programs intended to produce specific feature
validation) can declare this component as relevant, meaning that any program issued from this
benchmark can take a value of this component as an input. Outside of the benchmark context, the
environment used to run this validation process can set up the limitations of the current machine,
by giving the list of values the component "number of MPI processes" can take. This way, the way
tests are built for a given benchmark depends on the machine where the validation is run and not on
the benchmark itself, illustrating a high level of flexibility when validation occurs on multiple,
heterogeneous machines. This component is called an "iterator" under PCVS semantics, a
characteristic validation engine can "iterate" to produce a set of tests for a given program. The
list of available iterators is defined in the **environment configuration** and can be extended
without any restriction. While being not bound to HPC, PCVS has been designed to validate massively
parallel framework and a set of iterators are provided by default:

* `n_mpi`: number of MPI processes a program can take as an input;
* `n_omp`: number OpenMP threads a program ca take as an input;
* `n_node`: number of nodes the program is intended to span on;
* `n_core`: number of cores allocated to the program to run;
* `net`: the network layer to be used (if applicable);
* ...

For instance, if one has an MPI program `./a.out`, a cluster offering to run from 1 to 10 MPI
processes for a single run will lead PCVS to generate 10 tests, one for each value the iterator can
take.

### Test expressions ###

Where environment configuration exposes iterator definitions, compiler & runtime configurations
express what these iterator means from framework perspective. Finally, benchmarks expose programs
(=binaries) to run over these iterators. Trying to keep it simple, we came up with the idea of "Test
expressions" (TE for short). TE describes how a program should be built and run to be compliant with
benchmark standards. It can list potential restrictions over existing iterators (for example, an MPI
program can be written only for 2 processes) but nothing more specific that could limit it
scalability (in terms of combinations generated) to larger machines. The simplest TE would look
like:

```yml
my_TE_name:
        bin: "run.sh"
```

Considering an iterator like `n_core` taking a range of value from 1 to 10, the unrolling of this TE
by PCVS will lead to 10 tests, each one taking a different value as input (the way this value is
handled is up to the runtime, through CPUSETs, env variables, etc.). There are a lot of keywords
allowed in a TE, where an exhaustive list can be found in the [associated documentation][TE-doc].
For instance, here is a more detailed and commented example:

```yml
# TE describing a simple file to compile and launch
simple_compilation_and_run:
        # type COMPLETE : Compilation & Execution (DEFAULT VALUE if nothing specified)
        type: "complete"
        # File to compile : SRCPATH refers to current src directory
        files: "@SRCPATH@/main.c"
        # binary name to build, prefixed by @BUILDPATH@
        bin: "compilation_and_run.bin"
        # test args
        args: "-iter 100"
        # change dir before running the command (prefixed w/ build path)
        chdir: 'examples/'
        # reference time to result, in seconds (can be floating-point values)
        time: 100.00
        # max allowed gap above which a TE is considered failed (if T > time + tolerance)
        tolerance: 2.5
        # timeout before being potentially killed by the runtime (in seconds)
        timeout: 110
        # expected returned code by tests (0 by default)
        returns: 0
```

These TE are placed in `pcvs.yml` files, alongside with the source code. When PCVS is invoked, each
found file is processed and tests are generated from their defined expressions. Some macros exist to
let users refer some information only known at runtime (like the actual build path for instance).
PCVS's dynamic way to address that is to allow the use of macros (prefixed and appended with extras
`@`), among the most used are `@SRCPATH@` referring to the current source path the `pcvs.yml` file
is, and `@BUILDPATH@` referring to the associated artefact path this benchmark is isolated to. This
means that, during the unrolling phase, PCVS needs to process some files. To differentiate them from
regular ones, macro-containing files are renamed `pcvs.yml.in`. But still, in some corner cases,
defining tests following that static is not well suited to benchmarks where programs are depending
on iterator values. To tackle this, PCVS offers a way for a benchmark to emit its workflow **only
when unrolled by the engine**. This consists in writing an executable script (whatever the language,
as long as it is an executable), necessarily named `pcvs.setup`. This script will have access to a
lot of information from the engine (list of iterators, their values, the source path, the build
path...) and will be able to infer according to this information. At the end, the script will have
to write **on its standard output** the content of the `pcvs.yml` file that would have been written
statically. This way, from a generic script, PCVS can handle an infinite set of tricky situations.
This pre-script can also be used to execute some pre-actions (like copying an input data file from a
source path to artefact path). As an example, this is the way PCVS provides the NAS benchmarks,
where binary names highly depends on the number of MPI processes they will be run with.

To summarize, here is the workflow used to process TEs, for each requested directory:

1. Identify any `pcvs.setup` file in the current directory and recursively and process them.
    * **If the script writes content on STDOUT**, this output is considered to be the actual TE
      declaration to process for this location. The process will then continue to the next file.
    * **If the script DOES NOT write anything**, then the workflow continues as it had never
      encountered this file.
2. Identify any `pcvs.yml.in` file in both source and build paths. If found, resolve the macros and
   write the resulting output will process for this location. The execution will then advance to the
   next file.
3. Identify any `pcvs.yml` file in both source and build paths. If found, process the file.

## More in-depth documentation ##

The documentation has been categorized and the present README will only give the big picture,
probably omitting some advanced details, available in each subsequent documentation files. Here is
an attempt to list (non-exhaustively) where more documentation can be found in PCVS:

* The script man page itself, reachable with `./run_validation -man`
* [a basic Getting-started guide for beginners][GETTING-STARTED]
* [How to customize PCVS to fit it with the needs][CUSTOM]
* [An exhaustive list of TE's keywords][TE-doc]
* [An exhaustive list of compiler-specific configuration keywords][compiler-doc]
* [An exhaustive list of runtime-specific configuration keywords][runtime-doc]

## Disclaimer ##

None of the test bases here are mandatory to be used with PCVS. Even if our goal was to keep them
vanilla, some could have been slightly modified to fit with PCVS needs. Under the CeCILL-C license
(LGPL-compatible), PCVS can be used by any user or company having needs for validation purposes
without any restriction. However, feedback and improvements are highly encouraged, to help this tool
growing both in terms of features and test cases. Any of the benchmarks currently present are
open-source and freely usable within PCVS, provided without warranty of any kind. Please refer to
the [LISTINGS][LISTINGS] file for an exhaustive list of integrated benchmarks, their license and the
URI they have been extracted from. PCVS being the validation engine to process these test bases any
can be removed upon request from copyright holders. If so, please contact the support team as shown
in the [AUTHORS][AUTHORS] file.

<!-- websites -->
[ompi_website]: https://www.open-mpi.org/
[mpc_website]:  http://mpc.hpcframework.com/
<!-- relative links -->
[GETTING-STARTED]: ./GETTING-STARTED.md
[CUSTOM]:          ./CUSTOMIZATION.md
[LISTINGS]:        ./LISTINGS
[AUTHORS]:         ./AUTHORS
[TE-doc]:          ./build_scripts/resources/template-pcvs.yml
[compiler-doc]:    ./build_scripts/resources/template-compiler.yml
[runtime-doc]:     ./build_scripts/resources/template-runtime.yml
