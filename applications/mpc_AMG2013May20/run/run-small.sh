#! /bin/bash
#
#PBS -l mppwidth=96
#PBS -l walltime=10:00
#
cd $PBS_O_WORKDIR
#
aprun -ss -n 96 ../src/test/amg2013 -laplace -P 4 4 6 -n 150 150 150 -solver 2
