# CloverLeaf Mantevo Project ![image](Clover_alpha_small.png "CloverLeaf")


## Description

CloverLeaf is a mini-app that solves the compressible Euler equations on a Cartesian grid, using an explicit, second-order accurate method. 
Each cell stores three values: energy, density, and pressure. 
A velocity vector is stored at each cell corner. 
This arrangement of data, with some quantities at cell centers, and others at cell corners is known as a staggered grid. 

CloverLeaf was jointly developed by AWE and the University of Warwick.
The main CloverLeaf repositories can be found on the [UK-MAC GitHub page](https://github.com/UK-MAC).

This project is for the 2D version of the code, however a 3D version is made available on the UK-MAC site.

### Code Structure

CloverLeaf make heavy use of compute kernels.
Where possible everything is split up into dedicated routines with clear interfaces.

In most cases these kernels have been implemented in both C and Fortran.
This allows the user to compare the performance between the two languages.
Users can switch between these languages using the input parameters discussed below.

The control code is still written in Fortran.

 
## Directory Structure

Included in this project are 5 versions of CloverLeaf:


* Ref - contains a hybrid OpenMP/MPI implemention. The Serial, OpenMP and MPI 
* Serial - contains a serial version with no MPI or OpenMP
* OpenMP - contains an OpenMP version only with no MPI
* MPI - contains an MPI only implementation
* OpenACC - contains an OpenACC/MPI implementation, based on the Kernels directive

Each of these versions are consistent with each other, in terms of source code modifications and optimisation.
Further versions of CloverLeaf can be found on [UK-MAC GitHub page](https://github.com/UK-MAC) however these versions may be out of date.



## CloverLeaf Build Procedure


CloverLeaf uses a very simple Makefile to build the code, with a number of compiler flags pre-populated.
To make use of these configurations simply specify the compiler when building.

`make COMPILER=INTEL`

Some other configurations can be controlled on the make line as follows:

* MPI_COMPILER - Manually specify the Fortran compiler
* C_MPI_COMPILER - Manually specify the C compiler
* OPTIONS - Specify Fortran compiler options
* C_OPTIONS - Specify C compiler options





### Other Flags

The default compilation with the COMPILER flag set chooses the optimal 
performing set of flags for the specified compiler, but with no hardware 
specific options or IEEE compatability.

To produce a version that has IEEE compatiblity a further flag has to be set on 
the compiler line.

`make COMPILER=INTEL IEEE=1`

This flag has no effect if the compiler flag is not set because IEEE options 
are always compiler specific.

For each compiler the flags associated with IEEE are shown below:-

* INTEL: -fp-model strict –fp-model source –prec-div –prec-sqrt
* CRAY: -hpflex_mp=intolerant
* SUN: -fsimple=0 –fns=no
* GNU: -ffloat-store
* PGI: -Kieee
* PATHSCALE: -mieee-fp
* XL: -qstrict –qfloat=nomaf

Note that the MPI communications have been written to ensure bitwise identical 
answers independent of core count. However under some compilers this is not 
true unless the IEEE flags is set to be true. This is certainly true of the 
Intel and Cray compiler. Even with the IEEE options set, this is not guarantee 
that different compilers or platforms will produce the same answers. Indeed a 
Fortran run can give different answers from a C run with the same compiler, 
same options and same hardware.

Extra options can be added without modifying the makefile by adding two further 
flags, `OPTIONS` and `C_OPTIONS`, one for the Fortran and one for the C options.

`make COMPILER=INTEL OPTIONS=-xavx C_OPTIONS=-xavx`

A build for a Xeon Phi would just need the -xavx option above replaced by -mmic.

Finally, a `DEBUG` flag can be set to use debug options for a specific compiler.

`make COMPILER=PGI DEBUG=1`

These flags are also compiler specific, and so will depend on the `COMPILER` 
environment variable.

So on a system without the standard MPI wrappers, for a build that requires 
OpenMP, IEEE and AVX this would look like so:-

```
make COMPILER=INTEL MPI_COMPILER=mpiifort C_MPI_COMPILER=mpiicc IEEE=1 \
OPTIONS="-xavx" C_OPTIONS="-xavx"
```

## Input Files

CloverLeaf makes use of a local clover.in input file. 
Without this file present it will generate a default configuration - to solve a very small problem.

Each directory contains a number of sample input files - simply rename the corresponding file to clover.in within the same directory as the binary to run.

### Standard Problem Configurations

When reporting results for CloverLeaf it is common to refer to one of four standard problems.
These are defined by the problem size and the number of timesteps.

* Small Short - 960x960 cells for 87 steps
* Small Long - 960x960 cells for 2955 steps
* Large Short - 3840x3840 cells for 87 steps
* Large Long - 3840x3840 cells for 2955 steps


|       | Short                | Long           |
| ----- | -------------------- | -------------- |
| Small | clover_bm_short.in   | clover_bm.in   |
| Large | clover_bm16_short.in | clover_bm16.in |

Larger mesh size problems have been included for scaling studies.


### Input Parameters

The provides input files have a number of default parameters set, but it is easy to add further configuration.

| Parameter            | Description                                          | Default   |
|----------------------|------------------------------------------------------|-----------|
| profiler_on          | Enable the kernel level timers - for basic profiling | Off       |
| visit_frequency 10   | Enable VTK visualisation output every 10 steps       | Off       |
| summary_frequency 10 | Enable a regional summary output every 10 steps      | 10 steps  |
| use_fortran_kernels  | Use the Fortran kernel implementations               | On        |
| use_c_kernels        | Use the C kernel implementations                     | Off       |
| tiles_per_chunk      | How many tiles to use per MPI rank                   | 1         |
| tiles_per_problem    | How many tiles to use across the whole problem       | MPI ranks |



## Build Notes

### Vectorisation

Fortran tends to vectorise without the use the specific pragmas due to the 
higher level definition of data compared to C. C almost always needs pragmas to 
ensure that the compiler knows loops are safe to vectorise. Unfortunately there 
is no common standard for vector pragmas across compiler vendors, though 
\#pragma ivdep works on many. This means that on some systems (e.g. IBM) the 
vector pragmas may need to be modified to attain peak performance of the C code. 
Care needs to be taken with forcing vectorisation because even loops without 
obviously data dependencies can calculate the wrong answers.

### OpenACC Build

The makefile for this build does not differ from the Base version. In this case 
it is important to have the correct environment loaded on the system of use. 
On the Cray systems this will usually involve loading some NVIDIA and 
accelerator modules. Without these the code will still compile and run but the 
OpenACC pragmas wil be ignored and the calculation will take place on the CPU.


## Running the Code

CloverLeaf takes no command line arguments. It expects to find a file called 
`clover.in` in the directory it is running in.

There are a number of input files that come with the code. To use any of these 
they simply need to be copied to `clover.in` in the run directory and 
CloverLeaf invoked. The invocation is system dependent. 

For example for a hybrid run:

```
export OMP_NUM_THREADS=4

mpirun -np 8 clover_leaf
```

### Weak and Strong Scaling

Note that with strong scaling, as the task count increases for the same size 
global problem, the memory use of each task decreases. Eventually, the mesh 
data starts to fit into the various levels of cache. So even though the 
communications overhead is increasing, super-scalar leaps in performance can be 
seen as task count increases. Eventually all cache benefits are gained and the 
communications dominate. 

For weak scaling, memory use stays close to constant and these super-scalar 
increases aren't seen but the communications overhead stays constant relative 
to the computational overhead, and scaling remains good.

### Other Issues to Consider

System libraries and settings can also have a significant effect on performance. 

The use of the `HugePage` library can make memory access more efficient. The 
implementation of this is very system specific and the details will not be 
expanded here. 

Variation in clock speed, such as `SpeedStep` or `Turbo Boost`, is also 
available on some hardware and care needs to be taken that the settings are 
known. 

Many systems also allow some level of hyperthreading at a core level. These 
usually share floating point units and for a code like CloverLeaf, which is 
floating point intensive and light on integer operations, are unlikely to 
produce a benefit and more likely to reduce performance.

CloverLeaf is considered a memory bound code. Most data does not stay in cache 
very long before it is replaced. For this reason, memory speed can have a 
significant effect on performance. For the same reason, the same is true of 
hardware caches.

## Testing the Results

Even though bitwise answers cannot be expected across systems, answers should be 
very close. A summary print of state variables is printed out by default every 
ten hydrodynamic steps and then at the end of the run. This print gives average 
value of the volume, mass, density, pressure, kinetic energy and internal 
energy. 

As all boundaries are reflective the volume, mass, density and total energy 
should remain constant, though the scheme is not exactly conservative in energy 
due to the nature of the staggered grid. Kinetic energy is usually the most 
sensitive state variable and this is most likely to show compiler/system 
differences. If mass and volume do not stay constant through a run, then 
something is seriously wrong.

There is a very simple, small self test include in the CloverLeaf. If the code is
invoked with no clover.in input file present, this test will be run and the answer tested
against a "known" solution. Run time is much less than a second. This test is only for validation.
It is far too small for any genuine benchmarking comparisons.

There are four standard input files that are recommended for testing. 
Initially it is suggested than `clover_bm_short.in` is run. This is not a very 
sensitive test and the kinetic energy at the end of this run should be 0.1193E+01.
It is quick to run, even on a single core, and should stop after 87 steps. If the 
keyword "test_problem 2" is included in the input, then it will self test against a known solution.
Typical run time on a single core is about 20 seconds though this clearly depends on the hardware.
Typical run time on a dual socket node is about 2.5 seconds.

The second test to try is `clover_bm.in`. This runs for 2955 timesteps and is 
more sensitive than the first test. Through this simulation the whole 
computational mesh in traversed by a shock and so it is a good test of the 
parallel implementation because all internal boundaries will be crossed during 
the course of the simulation. The final kinetic energy should be 0.2590E+01. If the 
keyword "test_problem 3" is included in the input, then it will self test against a known solution.
Typical run time on a single core is about 950 seconds.
Typical run time on a dual socket node is about 100 seconds.

The third test to try is `clover_bm16_short.in`. This is the "socket" test and 
has a much larger mesh size and therefore, memory footprint. The final kinetic 
energy should be 0.3075E+00. If the keyword "test_problem 4" is included in the input,
then it will self test against a known solution.
Typical run time on a single core is about 450 seconds.
Typical run time on a single dual socket node is about 70 seconds.

The last test to run for validation purposes is `clover_bm16.in`. This is a 
fairly long, large mesh run and the kinetic energy at the final time should be 
0.4854E+01. If the keyword "test_problem 5" is included in the input, then it will self test against
a known solution.
Typical run time on a single core is about 14500 seconds.
Typical run time on a single dual socket node is about 1700 seconds.

A wide ranging set of performance figures can be found in the README for each version.
