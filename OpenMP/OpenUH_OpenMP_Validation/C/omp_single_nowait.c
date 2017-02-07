
#include <stdio.h>
#include "omp_testsuite.h"

int my_iterations;
#pragma omp threadprivate(my_iterations)

int test_omp_single_nowait(FILE * logFile)
{
    
	int nr_iterations;
    

    int total_iterations = 0;
    int i;

    nr_iterations = 0;
    my_iterations = 0;

#pragma omp parallel private(i)
    {
	for (i = 0; i < LOOPCOUNT; i++)
	{
	    
		#pragma omp single nowait
		{
#pragma omp atomic  
		    nr_iterations++;
		} /* end of single*/    
	    
	} /* end of for  */
    } /* end of parallel */

#pragma omp parallel private(i) 
    {
	my_iterations = 0;
	for (i = 0; i < LOOPCOUNT; i++)
	{
	    
		#pragma omp single nowait
		{
		    my_iterations++;
		} /* end of single*/    
	    
	} /* end of for  */
#pragma omp critical
	{
	    total_iterations += my_iterations;
	}

    } /* end of parallel */
    return ((nr_iterations == LOOPCOUNT) && (total_iterations == LOOPCOUNT));
} /* end of check_single_nowait*/
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
	printf("Testing omp single nowait\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp single nowait\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_single_nowait out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_single_nowait(stdout)){
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
