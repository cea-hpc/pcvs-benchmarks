
#include <stdio.h>
#include "omp_testsuite.h"

static int last_i = 0;
int i;

#pragma omp threadprivate(i)

/* Variable ii is used to avoid problems with a threadprivate variable used as a loop
 * index. See test omp_threadprivate_for.
 */
static int ii;
#pragma omp threadprivate(ii)

/*! 
  Utility function: returns true if the passed argument is larger than 
  the argument of the last call of this function.
  */
static int check_i_islarger2 (int i){
	int islarger;
	islarger = (i > last_i);
	last_i = i;
	return (islarger);
}

int test_omp_parallel_for_ordered(FILE * logFile){
	
	int sum;
	int is_larger;
	

	int known_sum;
	int i;
	
	sum = 0;
	is_larger = 1;
	last_i = 0;
#pragma omp parallel for schedule(static,1) private(i) ordered
	for (i = 1; i < 100; i++)
	{
		ii = i;
	
#pragma omp ordered
		{
			is_larger = check_i_islarger2 (ii) && is_larger;
			sum  = sum + ii;
		}
	
	}
	known_sum = (99 * 100) / 2;
	fprintf (logFile," known_sum = %d , sum = %d \n", known_sum, sum);
	fprintf (logFile," is_larger = %d\n", is_larger);
	return (known_sum == sum) && is_larger;
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
	printf("Testing omp parallel for ordered\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp parallel for ordered\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_parallel_for_ordered out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_parallel_for_ordered(stdout)){
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
