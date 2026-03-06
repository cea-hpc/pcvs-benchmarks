#!/bin/tcsh
#
# M_mcb_coral_x86_1ht.csh -- MCB parallel run script
#MSUB -S /bin/tcsh
#MSUB -r n
#MSUB -o
#MSUB -l resfailpolicy=ignore
#MSUB -j oe

umask 027
setenv echo 1

##############################################################################
# set CODE

setenv CODE ~/MiniApps/mcb/src/MCBenchmark.exe

##############################################################################
# perform the run

setenv CORES_PER_NODE 16
setenv THREADS_PER_CORE 1
# Specify the number of Monte Carlo particles per core
setenv N_PARTICLES 20000

echo "\n $SLURM_NNODES nodes, $CORES_PER_NODE cores per node, $THREADS_PER_CORE threads per core"

setenv PROCS_PER_NODE 4
@ CORES_PER_PROC = ($CORES_PER_NODE / $PROCS_PER_NODE)
echo "\n $CORES_PER_PROC cores per process"
@ TOTAL_NPROCS = ($PROCS_PER_NODE * $NUM_NODES)
echo $TOTAL_NPROCS total processes
srun -N $NUM_NODES -n $TOTAL_NPROCS --ntasks-per-node=$PROCS_PER_NODE --cpus-per-task=$CORES_PER_PROC $CODE --weakScaling --distributedSource --nCores=${CORES_PER_PROC} --numParticles=20000 --multiSigma --nThreadCore=$THREADS_PER_CORE
