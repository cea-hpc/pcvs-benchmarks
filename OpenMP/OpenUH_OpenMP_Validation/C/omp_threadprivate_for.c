
#include "omp_testsuite.h"
#include <stdlib.h>
#include <stdio.h>

static int j;
#pragma omp threadprivate(j)

int test_omp_threadprivate_for(FILE * logFile)
{
		int known_sum;
		int sum;
        int i;
		known_sum = (LOOPCOUNT * (LOOPCOUNT + 1)) / 2;
		sum = 0;

#pragma omp parallel
	{
		int sum0 = 0;
#pragma omp for private(i)
		for (i = 1; i <= LOOPCOUNT; i++)
		{
            j = i;
			sum0 = sum0 + j;
		} /*end of for*/
#pragma omp critical
		{
			sum = sum + sum0;
		} /*end of critical */
	} /* end of parallel */    
	
	if (known_sum != sum ) {
		fprintf (logFile, " known_sum = %d, sum = %d\n", known_sum, sum);
	}

	return (known_sum == sum);

} /* end of check_threadprivate*/
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
	printf("Testing omp threadprivate\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp threadprivate\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_threadprivate_for out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_threadprivate_for(stdout)){
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
