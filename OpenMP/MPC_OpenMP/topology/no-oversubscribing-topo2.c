#include <stdlib.h>
#include <stdio.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <mpi.h>
#include <omp.h>
#include <hwloc.h>
#include <assert.h>

struct worker_info_s {
	int mpi_rank;
	int tid;
	int local_id;
	int core;
};


void print_header(int rank) {
	if (rank == 0) {
		printf("\n");
		printf("TESTING OPENMP WORKERS POS\n");
		printf("--------------------------\n\n");
	}
}



int main(int argc, char **argv, char **envp) {
	int rank, size, i, *rbuf;
	struct worker_info_s *info;
	int sinfo = sizeof(struct worker_info_s);


	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	print_header(rank);

	/* Get Hardware threads avail and MPI tasks */
	int num_hwthreads;
	MPC_Processor_number(&num_hwthreads);
	if (rank ==0) { 
		printf("HW threads available: %d\n", num_hwthreads);
		printf("MPI tasks: %d\n", size);
	}

	/* Get and check max number of workers per MPI task */
	int max_num_threads, local_num_threads, total_num_threads;
	#pragma omp parallel
	{
		#pragma omp single
		{
			local_num_threads = omp_get_num_threads();
		}
	}

	MPI_Allreduce(&local_num_threads, &max_num_threads, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);

	info = (struct worker_info_s*)malloc(max_num_threads*sizeof(struct worker_info_s));
	for (i=0; i<max_num_threads; i++) {
		info[i].local_id = -1;
		info[i].tid = -1;
		info[i].mpi_rank = -1;
		info[i].core = -1;
	} 
 	/* Gather info from all workers */
	#pragma omp parallel 
  	{
		int thread_num =  omp_get_thread_num();
		info[thread_num].local_id = thread_num;
		info[thread_num].tid = syscall(SYS_gettid);
		info[thread_num].mpi_rank = rank;

		int ret;
		hwloc_topology_t topology = NULL;
		hwloc_cpuset_t newset;
		hwloc_obj_t obj;

		ret = hwloc_topology_init(&topology);
		assert(ret == 0);
		ret = hwloc_topology_load(topology);
		assert(ret == 0);
		newset = hwloc_bitmap_alloc();
		ret = hwloc_get_last_cpu_location(topology, newset, HWLOC_CPUBIND_THREAD);
		assert(ret == 0);

		obj = hwloc_get_first_largest_obj_inside_cpuset(topology, newset);
        	if (obj->type == HWLOC_OBJ_PU) obj = obj->parent; 
        		info[thread_num].core = obj->logical_index;
  	}

	if (rank == 0) {
		rbuf = (int *)malloc(size*max_num_threads*sizeof(struct worker_info_s));
	}

	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Gather(info, 4*max_num_threads, MPI_INT, rbuf, 4*max_num_threads, MPI_INT, 0, MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);

	/* Check tid unicity */
	if (rank == 0) {
		printf("* Checking tid unicity... \n");
		for (i=0; i<size*max_num_threads; i++) {
			if (rbuf[i*4] != -1) {
				int j;
				printf("\n  WORKER %d [MPI rank:%d, local id:%d]\n",
					 i, rbuf[i*4], rbuf[i*4+2]);
				printf("  tid: %d\n", rbuf[i*4+1]);
				for (j=0; j<i; j++) {
					if (rbuf[i*4+1]==rbuf[j*4+1]) { 
						printf("  unicity: FAILED\n");
						abort();
					}
				}
				printf("  unicity: OK\n");
			}
		}
	}


	/* Check local id unicity */
	if (rank == 0) {
		printf("\n* Checking local id unicity... \n");
		for (i=0; i<size*max_num_threads; i++) {
			if (rbuf[i*4] != -1) {
				int j;
				printf("\n  WORKER %d [MPI rank:%d, local id:%d]\n",
					 i, rbuf[i*4], rbuf[i*4+2]);
				printf("  local id: %d\n", rbuf[i*4+2]);
				for (j=0; j<i; j++) {
					if (rbuf[i*4+2]==rbuf[j*4+2] && rbuf[i*4]==rbuf[j*4]) { 
						printf("  local unicity: FAILED\n");
						abort();
					}
				}
				printf("  local unicity: OK\n");
			}
		}
	}

	/* Check consecutive binding per mpi task */
	if (rank == 0) {
		printf("\n* Checking binding... \n");
		for (i=0; i<size; i++) {
			int mpirank = i;
			printf("\n  In MPI task:%d \n", i);
			int first_id = size*max_num_threads; 
			int last_id = 0;
			int count = 0;
			int j;

			for (j=0; j<size*max_num_threads; j++) {
				if (rbuf[j*4] == mpirank) {
					printf("  WORKER %d [MPI rank:%d, core id:%d]\n",
						count, rbuf[j*4], rbuf[j*4+3]);
					if (first_id > rbuf[j*4+3]) first_id = rbuf[j*4+3];
					if (last_id < rbuf[j*4+3]) last_id = rbuf[j*4+3];
					count++;
				}
			}

			/* Consecutive ? */
			if (last_id - first_id == count-1) { 
				printf("  consecutive core ids: OK\n");
			} else {
				printf("  consecutive core ids: FAILED\n");
				abort();
			}

			/* Unicity ? (test only valid for nonover) */
			for (j=0; j<size*max_num_threads; j++) {
				int k;
				for (k=0; k<j; k++) {
					if (rbuf[j*4] == mpirank && rbuf[k*4] == mpirank) {
						if (rbuf[j*4+3]==rbuf[k*4+3]) { 
							printf("  core id unicity: FAILED\n");
							abort();
						}
					}
				}
			}
			printf("  core id unicity: OK\n");
			
		}
	}

	MPI_Finalize();

	/* Free memory */
	free(info);

	return 0;
}

