#! /bin/bash
#
#PBS -l mppwidth=49152
#PBS -l walltime=15:00
#PBS -q regular
#
cd $PBS_O_WORKDIR
#
aprun -ss -n 49152 ../src/test/amg2013 -laplace -P 32 32 48 -n 150 150 150 -solver 2
