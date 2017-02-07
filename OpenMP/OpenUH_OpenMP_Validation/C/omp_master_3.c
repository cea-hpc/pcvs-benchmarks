
#include <stdio.h>
#include "omp_testsuite.h"

int test_omp_master_3(FILE * logFile)
{
    
	int nthreads;
	int executing_thread;
        int tid_result = 0; /* counts up the number of wrong thread no. for
                               the master thread. (Must be 0) */
    

    nthreads = 0;
    executing_thread = -1;

#pragma omp parallel
    {
	
	    #pragma omp master 
	    {
                int tid = omp_get_thread_num();
                if (tid != 0) {
#pragma omp critical
                    { tid_result++; }
                }
#pragma omp critical
		{
		    nthreads++;
		}
		executing_thread = omp_get_thread_num ();

	    } /* end of master*/
	
    } /* end of parallel*/
    return ((nthreads == 1) && (executing_thread == 0) && (tid_result == 0));
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
	printf("Testing omp master\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp master\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_master_3 out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_master_3(stdout)){
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
