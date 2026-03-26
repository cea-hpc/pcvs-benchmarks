#!/bin/tcsh
#
# M_mcb_coral_BGQ_1ht.csh -- MCB parallel run script
#MSUB -S /bin/tcsh
#MSUB -r n
#MSUB -o
#MSUB -l resfailpolicy=ignore
#MSUB -j oe

umask 027
setenv echo 1

##############################################################################
# set CODE

setenv CODE ../src/MCBenchmark.exe

##############################################################################
# Print batch queue status and env variables

squeue -j ${SLURM_JOBID}

# dump all the variables
env | sort

##############################################################################
date

##############################################################################
# perform the run

setenv CORES_PER_NODE 16
setenv THREADS_PER_CORE 1
setenv NUM_NODES $SLURM_NNODES

echo "\n $NUM_NODES nodes, $CORES_PER_NODE cores per node, $THREADS_PER_CORE threads per core"

setenv PROCS_PER_NODE 1
@ CORES_PER_PROC = ($CORES_PER_NODE / $PROCS_PER_NODE)
echo "\n $CORES_PER_PROC cores per process"
@ TOTAL_NPROCS = ($PROCS_PER_NODE * $NUM_NODES)
echo $TOTAL_NPROCS total processes
srun --runjob-opts="--mapping TEDCBA" -N $NUM_NODES -n $TOTAL_NPROCS --ntasks-per-node=$PROCS_PER_NODE --cpus-per-task=$CORES_PER_PROC $CODE --weakScaling --distributedSource --nCores=${CORES_PER_PROC} --numParticles=20000 --multiSigma --nThreadCore=$THREADS_PER_CORE

setenv PROCS_PER_NODE 2
@ CORES_PER_PROC = ($CORES_PER_NODE / $PROCS_PER_NODE)
echo "\n $CORES_PER_PROC cores per process"
@ TOTAL_NPROCS = ($PROCS_PER_NODE * $NUM_NODES)
echo $TOTAL_NPROCS total processes
srun --runjob-opts="--mapping TEDCBA" -N $NUM_NODES -n $TOTAL_NPROCS --ntasks-per-node=$PROCS_PER_NODE --cpus-per-task=$CORES_PER_PROC $CODE --weakScaling --distributedSource --nCores=${CORES_PER_PROC} --numParticles=20000 --multiSigma --nThreadCore=$THREADS_PER_CORE

setenv PROCS_PER_NODE 4
@ CORES_PER_PROC = ($CORES_PER_NODE / $PROCS_PER_NODE)
echo "\n $CORES_PER_PROC cores per process"
@ TOTAL_NPROCS = ($PROCS_PER_NODE * $NUM_NODES)
echo $TOTAL_NPROCS total processes
srun --runjob-opts="--mapping TEDCBA" -N $NUM_NODES -n $TOTAL_NPROCS --ntasks-per-node=$PROCS_PER_NODE --cpus-per-task=$CORES_PER_PROC $CODE --weakScaling --distributedSource --nCores=${CORES_PER_PROC} --numParticles=20000 --multiSigma --nThreadCore=$THREADS_PER_CORE

setenv PROCS_PER_NODE 8
@ CORES_PER_PROC = ($CORES_PER_NODE / $PROCS_PER_NODE)
echo "\n $CORES_PER_PROC cores per process"
@ TOTAL_NPROCS = ($PROCS_PER_NODE * $NUM_NODES)
echo $TOTAL_NPROCS total processes
srun --runjob-opts="--mapping TEDCBA" -N $NUM_NODES -n $TOTAL_NPROCS --ntasks-per-node=$PROCS_PER_NODE --cpus-per-task=$CORES_PER_PROC $CODE --weakScaling --distributedSource --nCores=${CORES_PER_PROC} --numParticles=20000 --multiSigma --nThreadCore=$THREADS_PER_CORE

setenv PROCS_PER_NODE 16
@ CORES_PER_PROC = ($CORES_PER_NODE / $PROCS_PER_NODE)
echo "\n $CORES_PER_PROC cores per process"
@ TOTAL_NPROCS = ($PROCS_PER_NODE * $NUM_NODES)
echo $TOTAL_NPROCS total processes
srun --runjob-opts="--mapping TEDCBA" -N $NUM_NODES -n $TOTAL_NPROCS --ntasks-per-node=$PROCS_PER_NODE --cpus-per-task=$CORES_PER_PROC $CODE --weakScaling --distributedSource --nCores=${CORES_PER_PROC} --numParticles=20000 --multiSigma --nThreadCore=$THREADS_PER_CORE


##############################################################################
date

##############################################################################
# All done

exit

#MSUB -m                                                ##PSUB # 
