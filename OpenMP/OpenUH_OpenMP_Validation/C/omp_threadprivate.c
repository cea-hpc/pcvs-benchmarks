
/*
 * Threadprivate is tested in 2 ways:
 * 1. The global variable declared as threadprivate should have
 *    local copy for each thread. Otherwise race condition and 
 *    wrong result.
 * 2. If the value of local copy is retained for the two adjacent
 *    parallel regions
 */
#include "omp_testsuite.h"
#include <stdlib.h>
#include <stdio.h>

static int sum0=0;
static int myvalue = 0;

#pragma omp threadprivate(sum0)
#pragma omp threadprivate(myvalue)


int test_omp_threadprivate(FILE * logFile)
{
	int sum = 0;
	int known_sum;
	int i; 
	int iter;
	int *data;
	int size;
	int failed = 0;
	int my_random;
	omp_set_dynamic(0);

    #pragma omp parallel private(i) 
    {
	  sum0 = 0;
      #pragma omp for 
	    for (i = 1; i <= LOOPCOUNT; i++)
		{
			sum0 = sum0 + i;
		} /*end of for*/
      #pragma omp critical
	  {
	      sum = sum + sum0;
	  } /*end of critical */
	} /* end of parallel */    
	known_sum = (LOOPCOUNT * (LOOPCOUNT + 1)) / 2;
	if (known_sum != sum ) {
		fprintf (logFile, " known_sum = %d, sum = %d\n", known_sum, sum);
	}

	/* the next parallel region is just used to get the number of threads*/
	omp_set_dynamic(0);
    #pragma omp parallel
	{
      #pragma omp master
	  {
			size=omp_get_num_threads();
			data=(int*) malloc(size*sizeof(int));
	  }
	}/* end parallel*/


	srand(45);
	for (iter = 0; iter < 100; iter++){
		my_random = rand();	/* random number generator is called inside serial region*/

	/* the first parallel region is used to initialiye myvalue and the array with my_random+rank*/
    #pragma omp parallel
	{
	    int rank;
		rank = omp_get_thread_num ();
		myvalue = data[rank] = my_random + rank;
	}

	/* the second parallel region verifies that the value of "myvalue" is retained */
    #pragma omp parallel reduction(+:failed)
	{
	    int rank;
		rank = omp_get_thread_num ();
		failed = failed + (myvalue != data[rank]);
		if(myvalue != data[rank]){
		  fprintf (logFile, " myvalue = %d, data[rank]= %d\n", myvalue, data[rank]);
		}
	}
  }
  free (data);

	return (known_sum == sum) && !failed;

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
		fprintf (stdout, "\n\n%d. run of test_omp_threadprivate out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_threadprivate(stdout)){
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
