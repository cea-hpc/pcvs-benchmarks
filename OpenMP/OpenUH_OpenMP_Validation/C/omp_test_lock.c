
#include <stdio.h>
#include "omp_testsuite.h"

omp_lock_t lck;

int test_omp_test_lock(FILE * logFile)
{
    int nr_threads_in_single = 0;
    int result = 0;
    int nr_iterations = 0;
    int i;

    omp_init_lock (&lck);

#pragma omp parallel shared(lck)  
    {

#pragma omp for
	for (i = 0; i < LOOPCOUNT; i++)
	{
	    /*omp_set_lock(&lck);*/
	    
		while (!omp_test_lock (&lck))
		{};
	    
#pragma omp flush
	    nr_threads_in_single++;
#pragma omp flush           
	    nr_iterations++;
	    nr_threads_in_single--;
	    result = result + nr_threads_in_single;
	    omp_unset_lock (&lck);
	}
    }
    omp_destroy_lock(&lck);

    return ((result == 0) && (nr_iterations == LOOPCOUNT));

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
	printf("Testing omp_test_lock\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp_test_lock\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_test_lock out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_test_lock(stdout)){
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
