#include <stdlib.h>
#include <stdio.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <mpi.h>
#include <omp.h>
#include <assert.h>


void print_header(int rank) {
	if (rank == 0) {
		printf("\n");
		printf("TESTING NUMBER OF MPI TASKS AND OPENMP WORKERS\n");
		printf("----------------------------------------------\n\n");
	}
}

int main(int argc, char **argv, char **envp) {
	int rank, size, i;

	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
		MPI_Comm_size(MPI_COMM_WORLD, &size);

	print_header(rank);

	/* Get Hardware threads avail */
	int num_hwthreads;
	MPC_Processor_number(&num_hwthreads);
	if (rank ==0) printf(" HW threads available: %d\n", num_hwthreads);

	/* Get env OMP_NUM_THREADS */
	char *env_omp_num_threads = getenv ("OMP_NUM_THREADS"); 
	int omp_num_threads = -1;
	if (env_omp_num_threads != NULL) {
		omp_num_threads = atoi(env_omp_num_threads);	
		if (rank ==0) printf(" OMP_NUM_TRHEADS=%d\n\n", omp_num_threads);
	} else {
		omp_num_threads = num_hwthreads;
		if (rank ==0) printf(" OMP_NUM_TRHEADS: not set, should use number of hw threads\n\n");
	}

	/* Check number of MPI tasks */
	if (rank ==0) {
		int task_number;
		MPC_Local_task_number( &task_number);
		printf("* Checking (Local) MPI task number... [expected: %d, found: %d]\n", size, task_number);
		assert(size == task_number);
	}
	MPI_Barrier(MPI_COMM_WORLD);

	/* Check local number of OMP workers per MPI task */
	/* And get max number of OMP workers */
	int max_num_threads, local_num_threads, total_num_threads;
	#pragma omp parallel
	{
		#pragma omp single
		{
			local_num_threads = omp_get_num_threads();
			printf("* Checking local number of workers on MPI task %d... [expected <= %d, found: %d]\n", rank, omp_num_threads, local_num_threads);
			assert( local_num_threads <= omp_num_threads);
		}
	}

	/* Check total nmber of OMP workers */
	MPI_Reduce(&local_num_threads, &total_num_threads, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);

	if (rank ==0) {
		printf("* Checking total number of workers... [expected <= %d, found: %d]\n", omp_num_threads*size, total_num_threads);
		assert(total_num_threads <= omp_num_threads*size);
	}


	MPI_Finalize();

	return 0;
}

