#!/bin/bash

# This script can be used to build an instrumented 
# version of MCB on a Blue Gnen/Q system.

# The script should be run from this directory.

# Use with the following parameter:
# HPM        // build with mpitrace
# PROF       // build with hpmprof
# LOMP       // build using the new omp compiler
# For example,
#   ./build-bgq-instr.sh HPM

if [ -n "$1" ]; then 
    export $1=1
fi 

cd ..
if [ ! -d boost_headers ]; then
  tar xzf boost_headers.tgz
fi

mfile="makefile-bgq-instr.mk"

cp instr_build/$mfile src

mkdir -p bin

cd src

# Use veryclean when switching architectures. Only need clean
# when building on the same architecture as the last build.
make -f $mfile veryclean

make -f $mfile

