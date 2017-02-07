
#include <stdio.h>
#include <math.h>
#include "omp_testsuite.h"

int test_omp_parallel_for_if(FILE * logFile){
    int known_sum;
    
    int num_threads;
    int sum, sum2;
    int i;
    int control;
    
    control = 0;
    num_threads=0;
    sum = 0;
    sum2 = 0;

#pragma omp parallel for private(i) if (control==1)
    
    for (i=0; i <= LOOPCOUNT; i++)
    {
        num_threads = omp_get_num_threads();
	sum = sum + i;
    } /*end of for*/

    
    known_sum = (LOOPCOUNT * (LOOPCOUNT + 1)) / 2;
    fprintf (logFile, "Number of threads determined by omp_get_num_threads: %d\n", num_threads);
    return (known_sum == sum && num_threads == 1);
} /* end of check_paralel_for_private */
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
	printf("Testing omp parallel for if\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp parallel for if\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_parallel_for_if out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_parallel_for_if(stdout)){
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
