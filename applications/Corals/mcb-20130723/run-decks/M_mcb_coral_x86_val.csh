#!/bin/tcsh
#
# M_mcb_coral_BGQ_4ht.csh -- MCB parallel run script
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

# NUM_PART is the total number of Monte Carlo particles summed
# over all processes and threads.

echo "\n $NUM_NODES nodes, $CORES_PER_NODE cores per node, $THREADS_PER_CORE threads per core"

setenv PROCS_PER_NODE 4
@ CORES_PER_PROC = ($CORES_PER_NODE / $PROCS_PER_NODE)
echo "\n $CORES_PER_PROC cores per process"
@ OMP_NUM_THREADS = ($CORES_PER_PROC * $THREADS_PER_CORE)
@ TOTAL_NPROCS = ($PROCS_PER_NODE * $NUM_NODES)
echo $TOTAL_NPROCS total processes
@ NUM_PART = (10000 * $CORES_PER_NODE * $NUM_NODES)
srun -N $NUM_NODES -n $TOTAL_NPROCS --ntasks-per-node=$PROCS_PER_NODE --cpus-per-task=$CORES_PER_PROC $CODE --nCores=${CORES_PER_PROC} --nThreadCore=$THREADS_PER_CORE --numParticles=$NUM_PART --nZonesX=400 --nZonesY=400 

setenv PROCS_PER_NODE 4
@ CORES_PER_PROC = ($CORES_PER_NODE / $PROCS_PER_NODE)
echo "\n $CORES_PER_PROC cores per process"
@ OMP_NUM_THREADS = ($CORES_PER_PROC * $THREADS_PER_CORE)
@ TOTAL_NPROCS = ($PROCS_PER_NODE * $NUM_NODES)
echo $TOTAL_NPROCS total processes
@ NUM_PART = (40000 * $CORES_PER_NODE * $NUM_NODES)
srun -N $NUM_NODES -n $TOTAL_NPROCS --ntasks-per-node=$PROCS_PER_NODE --cpus-per-task=$CORES_PER_PROC $CODE --nCores=${CORES_PER_PROC} --nThreadCore=$THREADS_PER_CORE --numParticles=$NUM_PART --nZonesX=400 --nZonesY=400

setenv PROCS_PER_NODE 4
@ CORES_PER_PROC = ($CORES_PER_NODE / $PROCS_PER_NODE)
echo "\n $CORES_PER_PROC cores per process"
@ OMP_NUM_THREADS = ($CORES_PER_PROC * $THREADS_PER_CORE)
@ TOTAL_NPROCS = ($PROCS_PER_NODE * $NUM_NODES)
echo $TOTAL_NPROCS total processes
@ NUM_PART = (160000 * $CORES_PER_NODE * $NUM_NODES)
srun -N $NUM_NODES -n $TOTAL_NPROCS --ntasks-per-node=$PROCS_PER_NODE --cpus-per-task=$CORES_PER_PROC $CODE --nCores=${CORES_PER_PROC} --nThreadCore=$THREADS_PER_CORE --numParticles=$NUM_PART --nZonesX=400 --nZonesY=400

##############################################################################
date

##############################################################################
# All done

exit

#MSUB -m                                                ##PSUB # 
