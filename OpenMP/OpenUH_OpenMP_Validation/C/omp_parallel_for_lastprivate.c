
#include <stdio.h>
#include "omp_testsuite.h"

int test_omp_parallel_for_lastprivate(FILE * logFile){
    
    int sum;
    int i;
    int i0;
    

    sum =0;
    i0 = -1;
    int known_sum;

#pragma omp parallel for reduction(+:sum) schedule(static,7) private(i) lastprivate(i0)
    
    for (i = 1; i <= LOOPCOUNT; i++)
    {
	sum = sum + i;
	i0 = i;
    } /*end of for*/
    /* end of parallel*/    
    
    known_sum = (LOOPCOUNT * (LOOPCOUNT + 1)) / 2;
    return ((known_sum == sum) && (i0 == LOOPCOUNT));
} /* end of check_parallel_for_lastprivate */
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
	printf("Testing omp parallel for lastprivate\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp parallel for lastprivate\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_parallel_for_lastprivate out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_parallel_for_lastprivate(stdout)){
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
