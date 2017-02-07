
#include <stdio.h>
#include "omp_testsuite.h"

int test_omp_parallel_num_threads(FILE * logFile){
    
	int failed;
	int threads;
	int nthreads;
    

    int max_threads = 0;

    failed = 0;

    /* first we check how many threads are available */
#pragma omp parallel
    {
#pragma omp master
	max_threads = omp_get_num_threads ();
    }

    /* we increase the number of threads from one to maximum:*/
    for (threads = 1; threads <= max_threads; threads++)
    {
	nthreads = 0;

	
#pragma omp parallel reduction(+:failed) num_threads(threads)
	    {
		failed = failed + !(threads == omp_get_num_threads ());
#pragma omp atomic
	    nthreads += 1;
	    }
	
	failed = failed + !(nthreads == threads);
    }
    return (!failed);
}
int main()
{
	int i;			/* Loop index */
	int result;		/* return value of the program */
	int failed=0; 		/* Number of failed tests */
	int success=0;		/* number of succeeded tests */


	printf("######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	printf("## Repetitions: %3d                       ####\n",REPETITIONS);
	printf("## Loop Count : %6d                    ####\n",LOOPCOUNT);
	printf("##############################################\n");
	printf("Testing omp parellel num_threads\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp parellel num_threads\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_parallel_num_threads out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_parallel_num_threads(stdout)){
			fprintf(stdout,"Test successful.\n");
			success++;
		}
		else {
			fprintf(stdout,"Error: Test failed.\n");
			printf("Error: Test failed.\n");
			failed++;
		}
	}

    if(failed==0){
		fprintf(stdout,"\nDirective worked without errors.\n");
		printf("Directive worked without errors.\n");
		result=0;
	}
	else{
		fprintf(stdout,"\nDirective failed the test %i times out of %i. %i were successful\n",failed,REPETITIONS,success);
		printf("Directive failed the test %i times out of %i.\n%i test(s) were successful\n",failed,REPETITIONS,success);
		result = (int) (((double) failed / (double) REPETITIONS ) * 100 );
	}
	printf ("Result: %i\n", result);
	return result;
}
