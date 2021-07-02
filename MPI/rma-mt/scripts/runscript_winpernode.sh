#! /bin/bash
# Put header for run system here

OMPI_HOME=/users/hjelmn/build/ompi.latest
PATH=${OMPI_HOME}/bin:${PATH}
LD_LIBRARY_PATH=${OMPI_HOME}/lib:${LD_LIBRARY_PATH}

BENCHMARK_HOME=${HOME}/rmamt_benchmarks
BENCHMARK_VERSION=1.0.0
OPERATIONS=(put get)
SYNC=(flush lock lock_all fence pscw)
TEST=(rmamt_bw rmamt_lat)
BENCHMARK_ITER=10
BENCHMARK_THREADS=64

RESULT_BASE=${HOME}/rma-mt/results/
RUN=ompi-winperthread
VERSION=ugni_rdma # CHANGE ME FOR EACH RUN
# Change this to reflect system naming. At lanl all systems have a two-letter prefix.
# Examples: tt - Trinitite, ct - Cielito, mu - Mustang, etc.
HOST=$(hostname | cut -d'-' -f 1)

PREFIX=${RESULT_BASE}/${RUN}-${VERSION}-${HOST}

mkdir -p ${PREFIX} 2>/dev/null

# Set MCA variables here
export OMPI_MCA_osc=rdma
export OMPI_MCA_pml=ob1
export OMPI_MCA_hwloc_base_binding_policy=none
export OMPI_MCA_btl_ugni_rcache=grdma
export OMPI_MCA_orte_bound_at_launch=true

export PMI_NO_FORK=1

# workaround for pmix bug
export PMIX_SERVER_TMPDIR=/var/tmp
export TMPDIR=/var/tmp

waitnow() {
    for job in `jobs -p` ; do
        wait $job || echo "Job failed"
    done
    running=0
}

_run_benchmarks () {
    cp $0 ${PREFIX}/runscript.sh
    ompi_info -a > ${PREFIX}/ompi_info.out
    hosts=($(mpirun -N 1 hostname | sort -d))
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

    for operation in ${OPERATIONS[@]} ; do
	for sync in ${SYNC[@]} ; do
	    for test in ${TEST[@]} ; do
		threads=1
		while test ${threads} -le ${BENCHMARK_THREADS} ; do
		    output_dir="${PREFIX}/${operation}/${sync}/${test}"
		    mkdir -p ${output_dir} 2>/dev/null

		    for i in `seq 1 ${BENCHMARK_ITER}` ; do
			output_file=${output_dir}/threads_${threads}-${i}
			if test ! -s ${output_file} ; then
			    cmd="aprun -n 2 -N 1 -cc none ${BENCHMARK_HOME}/${test} -t ${threads} -o ${operation} -s ${sync} -w"
			    echo "Running $cmd"
			    time $cmd | tee ${output_file} &
			fi
		    done
		    let threads=threads*2
		done
		waitnow
	    done
	done
    done
}

# Capture the entire output
_run_benchmarks | tee ${PREFIX}/run_log
gzip -9 ${PREFIX}/run_log
