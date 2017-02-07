
#include <stdio.h>
#include <math.h>
#include "omp_testsuite.h"

/* Utility function do spend some time in a loop */
static void do_some_work (){
    int i;
    double sum = 0;
    for(i = 0; i < 1000; i++){
	sum += sqrt ((double) i);
    }
}

int sum1;
#pragma omp threadprivate(sum1)

int test_omp_for_private (FILE * logFile)
{
    int sum = 0;
    
	int sum0;
    

    int known_sum;

    sum0 = 0;	/* setting (global) sum0 = 0 */

#pragma omp parallel
    {
	sum1 = 0;	/* setting sum1 in each thread to 0 */

	{	/* begin of orphaned block */
	
	    int i;
#pragma omp for private(sum0) schedule(static,1)
	    for (i = 1; i <= LOOPCOUNT; i++)
	    {
		sum0 = sum1;
#pragma omp flush
		sum0 = sum0 + i;
		do_some_work ();
#pragma omp flush
		sum1 = sum0;
	    }	/* end of for */
	
	}	/* end of orphaned block */

#pragma omp critical
	{
	    sum = sum + sum1;
	}	/*end of critical*/
    }	/* end of parallel*/    

    known_sum = (LOOPCOUNT * (LOOPCOUNT + 1)) / 2;
    return (known_sum == sum);
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
	printf("Testing omp for private\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp for private\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_for_private out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_for_private(stdout)){
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
