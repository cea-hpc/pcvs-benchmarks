
#include <stdio.h>
#include <math.h>

#include "omp_testsuite.h"

int sum1;
#pragma omp threadprivate(sum1)

int test_omp_for_auto (FILE * logFile)
{
    int sum;
    
	int sum0;
    

    int known_sum;
    int threadsnum;

    sum = 0;
    sum0 = 12345;
    sum1 = 0;

#pragma omp parallel
    {
#pragma omp single
        {
            threadsnum=omp_get_num_threads();
        }
	/* sum0 = 0; */
	
	int i;
#pragma omp for firstprivate(sum0) schedule(auto)
	for (i = 1; i <= LOOPCOUNT; i++)
	{
	    sum0 = sum0 + i;
	    sum1 = sum0;
	}	/* end of for */
	
#pragma omp critical
	{
	    sum = sum + sum1;
	}	/* end of critical */
    }	/* end of parallel */    

    known_sum = 12345* threadsnum+ (LOOPCOUNT * (LOOPCOUNT + 1)) / 2;
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
	printf("Testing omp for auto\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp for auto\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_for_auto out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_for_auto(stdout)){
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
