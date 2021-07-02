#! /bin/bash
#
# Copyright (c) 2016-2018 Los Alamos National Security, LLC.  All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Put header for run system here

OMPI_HOME=/lustre/ttscratch1/hjelmn/build/ompi/dec17
PATH=${OMPI_HOME}/bin:${PATH}
LD_LIBRARY_PATH=${OMPI_HOME}/lib:${LD_LIBRARY_PATH}

BENCHMARK_HOME=/lustre/ttscratch1/hjelmn/rmamt_benchmarks
BENCHMARK_VERSION=1.0.1
OPERATIONS=(put get)
SYNC=(flush lock lock_all fence pscw)
TEST=(rmamt_bw rmamt_lat rmamt_bibw)
BENCHMARK_ITER=10
BENCHMARK_THREADS=32

RESULT_BASE=${BENCHMARK_HOME}/rma-mt/results/
RUN=ompi-haswell-final-v3-512kbuffer # CHANGE ME FOR EACH RUN
VERSION=ugni_rdma
# Change this to reflect system naming. At lanl all systems have a two-letter prefix.
# Examples: tt - Trinitite, ct - Cielito, mu - Mustang, etc.

#HOST=$(hostname | cut -d'-' -f 1)
HOST=tt

PREFIX=${RESULT_BASE}/${RUN}-${VERSION}-${HOST}

mkdir -p ${PREFIX} 2>/dev/null

# Set MCA variables here
export OMPI_MCA_osc_rdma_buffer_size=512k
export OMPI_MCA_pml=ob1
export OMPI_MCA_btl=self,vader,ugni
export OMPI_MCA_osc=rdma
export OMPI_MCA_pml=ob1
# Bind the master thread to the whole socket to minimize contention
# with worker threads
export OMPI_MCA_hwloc_base_binding_policy=socket
export OMPI_MCA_btl_ugni_fma_put_limit=4k
export OMPI_MCA_btl_ugni_fma_get_limit=2k
#export OMPI_MCA_btl_ugni_rcache=grdma
#export OMPI_MCA_orte_bound_at_launch=true

#export PMI_NO_FORK=1

waitnow() {
    for job in `jobs -p` ; do
        wait $job || echo "Job failed"
    done
    running=0
}

_run_benchmarks () {
    cp $0 ${PREFIX}/runscript.sh
    ompi_info -a > ${PREFIX}/ompi_info.out
    hosts=($(mpirun -N 1 hostname | sed 's/nid0*//g' | sort -d))
    node_count=${#hosts[@]}

    echo "*******************************************************************"
    echo "Starting ${RUN} with ${VERSION} on ${HOST} with ${node_count} nodes"
    echo
    echo "Open MPI Version: " $(mpirun --version | head -1)
    echo "Open MPI Install base: ${OMPI_HOME}"
    echo "Benchmark Version: " ${BENCHMARK_VERSION}
    echo "Host List: " $(IFS=", " ; echo "${hosts[*]}")
    echo "Start Time: " $(date)
    echo "*******************************************************************"

    j=0
    k=1
    for operation in ${OPERATIONS[@]} ; do
	for sync in ${SYNC[@]} ; do
	    for test in ${TEST[@]} ; do
		threads=1
		while test ${threads} -le ${BENCHMARK_THREADS} ; do
		    output_dir="${PREFIX}/${operation}/${sync}/${test}"
		    mkdir -p ${output_dir} 2>/dev/null

		    for i in `seq 1 ${BENCHMARK_ITER}` ; do
			cmd="mpirun -n 2 -N 1 --bind-to socket ${BENCHMARK_HOME}/${test} -x -t ${threads} -o ${operation} -s ${sync} -r ${output_dir}/threads_${threads}-${i}"
			echo "Running $cmd Output: ${output_dir}/threads_${threads}-${i}"
			if ! test -e ${output_dir}/threads_${threads}-${i} ; then
			    time $cmd
			    if test $? -gt 0 ; then
				rm -f ${output_dir}/threads_${threads}-${i}
				exit
			    fi
			fi
			let j=j+2
			let k=k+2
			if test $j -ge 20 ; then
			    waitnow
			    let j=0
			    let k=1
			fi
		    done
		    let threads=threads*2
		done
	    done
	done
    done
    waitnow
}

# Capture the entire output
_run_benchmarks | tee ${PREFIX}/run_log
gzip -9 ${PREFIX}/run_log
