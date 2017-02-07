
#include <stdio.h>
#include <math.h>
#include "omp_testsuite.h"

/* Utility function do spend some time in a loop */
int test_omp_task_private (FILE * logFile)
{
    int i;
    
    int known_sum;
    int sum = 0;
    int result = 0; /* counts the wrong sums from tasks */
    

    known_sum = (LOOPCOUNT * (LOOPCOUNT + 1)) / 2;

#pragma omp parallel
    {
#pragma omp single
        {
            for (i = 0; i < NUM_TASKS; i++)
            {
                
#pragma omp task private(sum) shared(result, known_sum)
                {
                    int j;
		    //if sum is private, initialize to 0
		    sum = 0;
                    for (j = 0; j <= LOOPCOUNT; j++) {
#pragma omp flush
                        sum += j;
                    }
                    /* check if calculated sum was right */
                    if (sum != known_sum) {
#pragma omp critical 
                        result++;
                    }
                } /* end of omp task */
                
            }	/* end of for */
        } /* end of single */
    }	/* end of parallel*/

    return (result == 0);
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
	printf("Testing omp task private\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp task private\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_task_private out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_task_private(stdout)){
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
