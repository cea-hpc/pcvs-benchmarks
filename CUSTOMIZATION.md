# Adding new objects to PCVS #

This documentation will detail how to expand PCVS. It will assume the reader has a basic knowledge
of what an iterator, a TE are and know how to start a simple validation process.

## Iterators ##

### Update the list of values of an iterator ###

An iterator will take a **list of elements** as a value. An element can be anything scalar as
defined by the YAML standard: an integer, a float, a string... Nested lists are not supported.
Please note that duplicate values will be sorted out to make each list composed of a set of unique
values. For example, to let TEs generate tests for different values of MPI processes, one should
define `n_mpi: [1, 2, 3, 4]` in the environment configuration file. Any TE relying on this iterator
(a TE relies on all defined iterators unless expressed otherwise).

A special character `~` let the user express this iterator should be disabled. This can be
interpreted as "this iterator is not relevant in my validation step and should be ignored by the
whole process". Do not confuse with an empty set of values `[]`, leading to deactivate any PCVS
object using this iterator. For instance, a TE using such a construct will never unroll a single
test, as an empty set cannot intersect with any set of values. This means than not a single valid
combination can be found and the whole program will be dropped off from the validation process.

### Sequences ###

To ease the way to list possible value for iterators, PCVS offers multiple syntax to keep it
concise. Here the following syntax (all string-based) supported by the tool for **numeric only**
iterators (a numeric iterator is, by convention, identified with a `n_` prefix):

| value       | Meaning                       | Comment                                                                     |
|:-----------:|:-----------------------------:|---------------------------------------------------|
|`"1-9"`      | `[1, 2, 3, 4, 5, 6, 7, 8, 9]` | Define a range of values, linear from min to max.                                    |
|`"2:12:+2"`  |`[2, 4, 6, 8, 10, 12]`         |Iterate with a specific additive step from min to max.                        |
|`"1:16:*2"`  |`[1, 2, 4, 8, 16]`             |Iterate from to max with a specific multiplicative step.                      |
|`"15:220:^3"`|`[27,64,125,216]`              |From min to max, list all values being a power of X.                          |
|`">=2"`      | Remove any value lower than 2 | Conditional value, to filter out invalid ones (can be derived to `>, <, <=`|

These patterns are not mandatory and only valid for numeric iterators as defined by the
[Conventions](#conventions) section.

### Adding a new iterator ###

To add new characteristic to run tests against, it is as easy as adding a single line. Iterators are
declared in the *environment configuration* file, located under
`build_scripts/configuration/environment/` path. In the YAML description, a node `iterators` exist.
Adding a new iterator consists of adding a new entry, by choosing a name (see the
[Conventions](#conventions) below) and a list of values.

### Conventions ###

while there are not mandatory, it is best practice to follow the guidelines below when adding a new
iterator:

* Adding the documentation to this iterator if it is supposed to be integrated to PCVS in a
  long-term basis. This will help later users to understand the scope of such characteristics and
  how it should/would be handled by subsequent frameworks.
* A numeric iterator (under YAML definition) *can* be prefixed with `n_`. While this is not
  mandatory, this [section](#sequences) will show which optimizations can be made only for these
  special iterators.
* When a single TE is unrolled, a test name (i.e. label) is built from the benchmark it is issued,
  the TE name and the combination of iterators used as inputs. To keep explicit the given value of
  an iterator in test name, a prefix can be defined under the node `naming` in the environment
  configuration file for this iterator. For instance, a benchmark `mybench` providing a program
  named "my_test" (as its TE name) and using an iterator `n_mpi` with values 4 and 8, will produce
  the following test named after setting under the `naming` node the following `n_mpi: "np"`:

```bash
#full long-names
mybench.my_test_np4
mybench.my_test_np8
#shorten names
my_test_np4
my_test_np8
```

## New program ##

As described in the main [README](./README.md#test-expressions), parsing of programs to test is
driven by well-formatted YAML files, put directly into benchmarks source code.

### Through `pcvs.yml{.in}` files ###

These raw files will contain a full static description about how to compile and run a program. a
top-level YAML node is named after the program to be added. It will be used to generate the final
test name and an explicit name, helping users to know which benchmark it refers to. We will know
list all subkeys such a node can contain the following:

#### Main attributes ####

* `type`: depicts what this TE describes. Is it a `build` rule, a `run` rule or `complete` (both)
  rules. Depending on this type, some key behavior slightly changes and is detailed for each of
  them. the `complete` keyword will build, from a single TE, one test as a compilation step, and any
  further tests to run (depending on iterators) being dependent tests on this compilation,
  implicitly.
* `files`: list files to forward to the compiler in order to compile the program. This key is
  ignored when `run` type is used. To give multiple files, please use an array of strings.
* `target`: if this is not present, notify the engine that this program should be built from
  Makefile rule. Providing `''` as a value is equivalent to run `make`.
* `bin`: inform PCVS about the final program name. In case of a `build` step, this information is
  forwarded to the compiler to define the output program (kind of `-o` option). For other steps,
  this will set which program should be launched by the runtime, as long as an executable is given.
  A relative path to the current source directory is expected (later expanded with an absolute path
  to the current directory).
* `cargs`: give some command line arguments to the compiler in order to build the program. This way,
  a TE can increase verbosity levels, debug symbols, etc.
* `args`: give some command line arguments to the program. If this is only static in these files, we
  will see that `pcvs.setup` files can provide an easy way to create custom iterators over benchmark
  parameters (local scope).
* `chdir`: some tests need to be run from a specific directory to complete. Instead of modifying the
  existing benchmark, PCVS allow TEs to move the current directory before running the test. Note
  that this should be relative to the current source directory. Note also that jobs are run from
  mirrored directories (i.e. the whole benchmark directory is re-created elsewhere and tests are run
  in this tree, avoiding artefacts to pollute the source directory, harder to clean up afterwards).
* `time`: elapsed time running this program should take. This is **NOT** hard timeout. A jot won't
  be killed if time exceeds but will be considered as a `failed test` as slower than expected. To
  avoid putting a limit to a job, just omit this key.
* `tolerance`: because time measurement is not perfect, especially from one machine to another, the
  elapsed time for a test has to be lower than `time` + `tolerance`. Note that (for now), it is only
  an upper bound, meaning that a test will always succeed if its elapsed time is lower than `time` -
  `tolerance`.
* `timeout`: Had timeout, after which a running test can be killed by any means used by the runtime.
* `returns`: expected return code to consider the test as a success. By default, a zero return code
  is expected but any other value can be provided.
* `openmp`: The program needs an OpenMP implementation to be compiled and thus the appropriate
  OpenMP compilation flag will be set (depending on the compiler used). Note that this is not
  symmetrical how MPI is handled, considering that the compiler needs to be MPI flavored (Adding an
  `mpi` node is in progress).
* `tbb`: same as above, to notify to the compiler that this program needs to be built with tbb
* `accl`: same as above, no notify to the compiler that this program is accelerator-related. As this
  is only related to CUDA-written tests, this is directly translated to the use of an Nvidia-related
  compiler in specs.

In addition to these keys, any TE can set iterators with a subset of values to be filtered for this
TE to be processed. For instance, if a program is only valid when the `n_mpi` iterator is providing
a value of 100, then a TE should contain `n_mpi: [100]`. **If this value is also present in the
definition of this iterator, in the environment configuration, then a test will be issued**.
Otherwise, the combination will be simply discarded, considering that not a single test can be
created from this TE defirnition as it does not meet the minimum criteria to be runnable under the
given configuratioN.

The only mandatory nodes to be able to unroll a TE are a name (the top-level one), the `files` field
in case of a compilation and the `bin` in case of a run. Any others are optional, disabling the
feature if not specified.

#### Dependencies ####

Dependence, within PCVS semantics means a strong correlation between two tests, one being the
upstream, the other one being the dependent. A dependent test can only be run if all its upstream
tests **PASSED**. Otherwise it is considered as **NOT RUNNABLE** (which is a different category than
**FAILED**). This helps to save time when compilation does not succeed, there is no reason to
attempt running the (non-existent) binary. A dependent TE can take as many upstream TE as needed.
the TE node used to accomplish that is `deps`, taking an array of strings, each one being a TE name
**declared in the same YAML file**. This is mandatory as YAML parser does not handle
cross-references between files.

For example, the two TEs below are roughly the same: Using dependencies:

```yml
my_TE_build:
    type: "build"
    target: "install"

my_TE_run:
    type: "run"
    deps: [ "my_TE_build" ]
    bin: "a.out"
    args: "iter -100"
```

Using the `type: "complete"` definition:

```yml
my_TE_complete:
    type: "complete"
    target: "install"
    bin: "a.out"
    args: "iter -100"
```

Such support allows to create a different run purposes (=varying `time` to result, runtime args,
etc.) from a single build.

#### Base/Derived TEs ####

Setting a large benchmark can be a fastidious task as a lot of TEs can be redundant. For instance,
to build each program, the same subset of nodes has to be used. Instead, thanks to basic YAML
syntax, the gathering of such keys can be done, and reused as many times as necessary.

First note that a given top-level can be attached to a unique tag (from YAML semantic) by using an
ampersand just after the first line of its name. This will be named as a "base TE":

```yml
my_TE: &my_tag
    type: "run"
    deps: [ "my_TE_build" ]
    bin: "a.out"
    time: 100.0
```

To then use this tag (and all its subsequent nodes), a special key `herit` should contain the name
of the tag to be unrolled. The associated TE will be referred as the "derivated TE". If TEs contain
the following:

```yml
my_TE_run1:
    herit: *my_tag
    args: [ "iter 10", "-cache on" ]

my_TE_run2:
    herit: *my_tag
    args: [ "iter 100", "-cache off" ]
```

it would be roughly equivalent to:

```yml
my_TE_run1:
    type: "run"
    deps: [ "my_TE_build" ]
    bin: "a.out"
    time: 100.0
    args: [ "iter 10", "-cache on" ]

my_TE_run2:
    type: "run"
    deps: [ "my_TE_build" ]
    bin: "a.out"
    time: 100.0
    args: [ "iter 100", "-cache off" ]
```

Making information less redundant, keeping it easy to maintain. A base TE is still processed as a
regular one and will produce associated tests. If this is not desired (i.e. an abstract TE, used to
factorize information but not being usable "as is"), PCVS decided to use a convention, the prefix
`pcvst_` to each TE name that should not be unrolled by default (an extra `t` for "template").

#### Macro-based files ####

Macros are available to explicit some information that would only be known when running PCVS. This
is mainly true for paths but can be extended to any other situation. A macro is a keyword surrounded
by `@`. Each of these macros is substituted during the test generation phase, by the appropriate
information. The following macros are currently handled by PCVS:

* `@SRCPATH@`: When used in a YAML file will be replaced by the current directory string the file
  is.
* `@BUILDPATH@`: When used in a YAML file, will be replaced by the same subtree from the source
  directory. But as we already mentioned, tests are run in a separate worktree (as specified by
  `./run_validation --output=` option)
* `@ROOTPATH@`: Source root path, should be used with caution as it will break any benchmark
  isolation.
* `@BROOTPATH@`: build root path, where some artefact from pre-run setups can be found (complete
  build configuration, env variable declaration, etc.).
* `@SPACKAGE_PATH@`: Path where the associated Spack package has been found (if any). Please see the
  [Spack section](#spack) for more information.

#### Grouping ####

The grouping is a mechanism to filter (in or out) iterators by classifying them into groups. These
groups are defined under `build_scripts/configuration/groups` and can be specified from the command
line thanks to the `--group` option. Thanks to the [Base/Derived approach](#basederived-tes)
described above, groups can be easily shown as templates, where iterators are enabled/disabled to
dismiss combinations where the characteristic is not relevant for the current group. In other words,
consider any pure MPI tests, not needing to be run over multiple OpenMP configuration, by
definition. An "MPI" group would disable globally any OpenMP-related iterators. Any pure MPI TE
definition would have to "herit" from this base group to shrink a large portion of combinations.
Basic groups, available by default are:

```yml
pcvst_grp_mpi:  &GRPMPI
        n_omp: ~

pcvst_grp_omp:  &GRPOMP
        n_node: ~
        n_proc: ~
        n_mpi: ~
        net: ~
```

Note that, as YAML does not support cross-referencing, the content of the group file is copying at
the beginning of each built `pcvs.yml` located in the build path. This is also the reason why a
specific concept, independent from templating, exists.

#### Spack ####

PCVS is having a Spack support for TEs. A Spack definition can be inserted into an existing TE.
There is two scenarios:

* The TE is valid only if the specified Spack recipe is available during run time. This way, Spack
  can be seen as a dependence, where a third-party program is required for this test to run
  (consider a Python module or an external library). This approach is really interesting as it
  defers to Spack the responsibility of building a program, being more powerful and flexible than
  what PCVS can do.
* The TE is only based on Spack, meaning that the purpose of this TE is to build and load a Spack
  recipe, No "compilation" or "run" tests should be emitted by the engine. For this special case, a
  new value `spack` can be provided to the `type` node.

To add Spack features to a TE, the top-level node is `spackage` (for Spack package) and can contain
the following:

* `name`: the package name, as referenced by Spack. This is the only mandatory field.
* `version`: A package version
* `variants`: the potential variants to enable/disable
* `build_if_missing`: if the concretization is not able to identify an already installed package, is
  PCVS allowed to build it ? In other terms, does the absence should be considered as a failure ?
* `deps`: specify potential restriction to dependent packages (for instance, to force some specific
  version/variant). A dep could contain:
  * a dep name, used as a key
  * the same fields as above
  * the special case, where the dep is a virtual package (for instance, a dep to MPI), the field
    `upstream` let the use choose between the `compiler`-defined, `runtime`-defined value or
    anything else.

It is not possible to directly set a dependency to a compiler, as this is enforced by the actual
PCVS compiler setup; to ensure consistency. Here is a example of a Spack definition, as found in
`build_scripts/resources/template-pcvs.yml`:

```yml
simple_spack_compilation:
        spackage:
                #REQUIRED package name, as specified by Spack
                name: "mypackage"
                version: 1.2.0
                variants: ["+debug", "cflags='-O3 -Werror'"]
                build_if_missing: 0
                deps:
                        mydep:
                                version: 0.5.0
                                build_if_missing: 0
                        mpi:
                                upstream: "compiler"
```

### Through `pcvs.setup` files ###

To create even more flexibility to dynamically adjust the list of jobs to process, PCVS offer
`pcvs.setup` script to handle it. Such a script is required to have executable rights but no
restriction about language or format used. The purpose is to write on the standard output, the actual
content of `pcvs.yml`. The difference is in the fact that this script can access during run time, to
some extra information to build the proper TEs. When booting up, PCVS builds a file containing
concretized configuration for the current run, a file called `config.env` exported before
`pcvs.setup` is run. It will export as environment variable, a large set of information to steer a
benchmark configuration. The name of each variable is a concatenation of YAML nodes from top-level.
As every YAML configuration files is converted, almost every setup should be reachable through it.
Among them, here are some of them:

* `pcvs_src`: root src path
* `pcvs_testbuild`: root build path
* `pcvs_compiler_target`: loaded compiler configuration file
* `pcvs_runtime_target`: loaded runtime configuration file
* `pcvs_iterators_<xxx>`: the range of values defined in the configuration for the iterator named
  `<xxx>`.
* ...

In addition to these variables, each script is fed with a single argument containing the relative
source path from where `run_validation` script is located. It can help to rebuild the actual
absolute path, combined with `pcvs_src`, alongside with the artefact path with `pcvs_testbuild`.

To keep these test bases easy to maintain, one should keep in mind the lesser modifications of
sources, the better. Most of complex scenarios can be taken care with a simple script. A recurrent
pattern is a file open from the program itself by using a relative path. This can be solved by
fixing the code to accept a relative path (or expandable with an env variable) *or* by copying the
input file at the right place before the test begins, the second option being preferable.

## New test base ##

Major benchmarks can be divided into categories (ex: IMB belongs to MPI, CLOMP belongs to OpenMP).
If a benchmark can fit with an existing category, the whole source base should be put in a dedicated
subdirectory, under the directory named after the parent category. By doing so, any users looking
for assessing support of a framework to this standard will have access to a better coverage thanks
to this new benchmark without having to modify anything in his own process of running validation.
This may be pretty convenient for automatic validation processed by third-party tools (like
Jenkins).

Once a new directory has been added, it should be directly taken into account for later use. Indeed,
by default, any directory present in the root directory will be scanned for tests (the only
exclusions are `build_scripts`, `build` and any paths provided by `--output`). To specifically use
this new benchmark, the `--select` option will process it. To ensure the proper set up for this new
directory, two `run_validation` option may be considered (for help):

* `--engine-debug`: will print out an output file alongside with each found `pcvs.yml` file and will
  list, for each TE in it, how the engine unrolled it. More specifically, it will detail what were
  the system configuration, how each TE may have restrained it and what is the final outcome.
* `engine-sim`: For debug only. The whole validation process will be executed normally **until the
  actual execution of the command**, replaced by a short random `nanosleep()`. This allows PCVS developers and test integrators to assess the
  proper integration of new tests in the workflow. For obvious reasons, we highly discourage users
  to enable this option for any other purposes.

## New compiler ##

A new compiler configuration file should be placed under `build_scripts/configuration/compilers` and is a YAML-formatted. Here the example of the default file:

```yml
c: "gcc"
cxx: "g++"
cu: "nvcc"
f77: "gfortran"
f90: "gfortran"
f95: "gfortran"
f03: "gfortran"
f08: "gfortran"

cflags: ""
openmp: ""
tbb: ""
accl: ""
strict: "-Wall -Werror -pedantic"
```

Its structure is flat, pretty straightforward. For each supported language, a compiler configuration file has to declare the program to use. A missing one will inform PCVS a lack of support (not a error, any test will be discarded.

## New runtime ##

Adding a new runtime configuration is a bit more complex. Symmetrically, the associated file should be placed under `build_scripts/configuration/runtimes`. Here is the first part, straightforward:

```yml
#command to prefix any test with
cmd: "mpirun"
#fixed arguments to append to the command above
args: ""
#Runtime filter module (see below for more detail)
module: "Default"
#If a timeout value is given for the test, how should it be prefixed when forwarded to runtime ?
timeout_prefix: "-t"
```

The second part of a runtime configuration file is a bit different. Each iterator being independant of how PCVS works, it is up to the runtime to express the relation between an iterator (and its set of values) and actual runtime options. This is made through the following architecture:

* Each iterator supported by the runtime is expressed with a top-level node with the same name in ths configuration file (if missing, the iterator is discarded for the whole validation run).
* a `key` subnode indicates the prefix used with a particular value. The string obtained should be then readable by the runtime.
* a `usage`subnode indicates how the runtime is supposed to read this value. The `argument` indicates that the string built above will be forwarded as a command line argument, while `environment` indicates the string will be given as an environment variable. Here two examples to fully understand how it works:

```yml
n_mpi:
        key: "-np "
        usage: "argument"

n_omp:
        key: "OMP_NUM_THREADS="
        usage: "environment"
```

For now, a « prefix » mode is not supported but could be added if necessary. Each value, combined with a key above, should be valid for the targeted runtime. This is easy to ensure when iterators are numeric but can become complex when dealing with string-based values. The most illustrative example is an iterator based on the network layer used. While the number of different technologies is limited, each MPI runtime could have its own name to call it. To address this situation, a correlation can be made between a standard name (used by iterators) and its specific translation. A value to convert is suffixed by `_val` and the associated value will be used in place when building the test. Here is an example based on network definition. Considering the iterator `net` and its range of values of `["ib", "tcp", "shmem", "portals" ]` for instance. A mapping from the runtime could look like:

```yml
net:
        key: "-mca btl "
        usage: "argument"

        ib_val: "openib,self"
        tcp_val: "tcp,self"
        shmem_val: "sm,self"
        portals_val: "portals,self"
```

## New environment ##

TBW

## New group ##

For a detailed use of grouping, please read [this section](#grouping) first. A group is a dedicated file, located under `build_scripts/configuration/groups` and contains TEs copied in each `pcvs.yml` files. Purpose of groups is to create categories under manage globally some iterators for any TEs referring to this category. For instance, the MPI category should not have to consider OpenMP iterators when unrolling tests. A basic group file looks like:

```yml
pcvst_grp_mpi:     &GRPMPI
        n_omp: ~

pcvst_grp_omp:     &GRPOMP
        n_node: ~
        n_proc: ~
        n_mpi: ~
        net: ~
```

This should fit with most of current needs, but can be expanded as needed. A `~` indicates an iterator not relevant for any subsequent TEs derived from it. To add an existing TE to a group, add the node `herit: *<group_tag>` in the TE, where `group_tag` is the name given in YAML tag (i.e. the name written after the ampersand).
