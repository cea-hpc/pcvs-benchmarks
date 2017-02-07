#! /bin/bash
#
#PBS -l mppwidth=960000
#PBS -l walltime=10:00
#
cd $PBS_O_WORKDIR
#
aprun -ss -n 960000 ../src/test/amg2013 -laplace -P 96 100 100 -n 150 150 150 -solver 2
