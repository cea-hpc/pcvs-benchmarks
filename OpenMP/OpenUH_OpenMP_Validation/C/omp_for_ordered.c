
#include <stdio.h>
#include <math.h>

#include "omp_testsuite.h"

static int last_i = 0;

/* Utility function to check that i is increasing monotonically 
   with each call */
static int check_i_islarger (int i)
{
    int islarger;
    islarger = (i > last_i);
    last_i = i;
    return (islarger);
}

int test_omp_for_ordered (FILE * logFile)
{
    
	int sum;
	int is_larger = 1;
    
    int known_sum;

    last_i = 0;
    sum = 0;

#pragma omp parallel
    {
	
	    int i;
	    int my_islarger = 1;
#pragma omp for schedule(static,1) ordered
	    for (i = 1; i < 100; i++)
	    {
		#pragma omp ordered
		{
		    my_islarger = check_i_islarger(i) && my_islarger;
		    sum = sum + i;
		}	/* end of ordered */
	    }	/* end of for */
#pragma omp critical
	    {
		is_larger = is_larger && my_islarger;
	    }	/* end of critical */
	
    }

    known_sum=(99 * 100) / 2;
    return ((known_sum == sum) && is_larger);
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
	printf("Testing omp for ordered\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp for ordered\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_for_ordered out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_for_ordered(stdout)){
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
