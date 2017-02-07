#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <omp.h>

int main (int argc, char **argv)
{
	int number_threads = -1;
	int thread_num;
	int env_num = -1;
	char * num_threads = NULL ;

	num_threads = getenv("OMP_NUM_THREADS");

	fprintf( stderr, "env = %s\n", num_threads ) ;

	env_num = atoi(num_threads);

	/* OpenMP */
#pragma omp parallel firstprivate(number_threads, thread_num)
	{
		number_threads = omp_get_num_threads();
		thread_num = omp_get_thread_num();
		fprintf(stderr, "thread num %d: num_threads %d\n", thread_num, number_threads);

		assert(env_num > -1);
		assert(number_threads > -1);

		if(number_threads != env_num)
		{	
			abort();
		}

#pragma omp barrier

#pragma omp master
		fprintf(stderr, "test PASSED\n");
	}
	return EXIT_SUCCESS;
}

