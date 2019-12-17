# Parallel Computing -- Validation Suite

PCVS is a test engine designed to help you run

## Requirement & Setup

## Directory layout

The actual repository is composed, at top-level, from files and directories. While all top-level files are mainly documentation or license, the main entry point users will have to interact with is the `run_validation` script. Then the only mandatory directory is `build_scripts` and contains PCVS engine code base, as long as the whole configuration sterring a validation run. This means that the only requirement to import/export/copy the PCVS engine consists in carrying over the `run_validation` script and `build_scripts` directory, reaching a total size around 6MB. 




While users need to edit some files in this directory, none of its content need to be handled to use PCVS. The main path one will need to interact with are under `build_scripts/configuration/` directory (detailed later).

The actual repository is split into multiple directories. The actual layout is purely arbitrary and one could create a flat tree, Each directory consists in an aggregation of non-regression bases (NRB), all related to coming from a large range of test sets, divided among the characteristic tests are validating. For example, one can found a `MPI` directory (aimed to contain NRB testing the MPI standard), `OpenMP` directory, `Hybdir` and so on. The directory chosen for each NRB is not mandatory but this categorization is recommended for coherency purposes. When the validation is run, users can easily specify which type of components to validate. 



The single entry point to use PCVS is the `run_validation` script. Written in Perl, this command can be run from anywhere.

## Usage

## Advanced design

## More documentation

The documentation has been categorized and the present README will only give the big picture, probably omitting some advanced details, available in each subsequent documentation files. Here is an attempt to list (non-exhaustively) where more documentation can be found for PCVS :

* The actual main script man page reachable with `./run_validation -man`
* [a basic Getting-started guide for beginners](GETTING-STARTED.md)
* [How to customize PCVS to fit him your needs](CUSTOMIZATION.md)
* [An exhaustive list of TE's keywords](build_scripts/resources/template-pcvs.yml)
* [An exhaustive list of compiler-specific configuration keywords](build_scripts/resources/template-compiler.yml)
* [An exhaustive list of runtime-specific configuration keywords](build_scripts/resources/template-runtime.yml)
