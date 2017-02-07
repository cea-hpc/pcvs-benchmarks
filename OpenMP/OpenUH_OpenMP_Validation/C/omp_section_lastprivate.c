
#include <stdio.h>
#include "omp_testsuite.h"


int test_omp_section_lastprivate(FILE * logFile){
    
	int i0 = -1;
	int sum = 0;
        int i;
        int sum0 = 0;
    
    int known_sum;

    i0 = -1;
    sum = 0;

#pragma omp parallel
    {
	
#pragma omp sections lastprivate(i0) private(i,sum0)
	{
#pragma omp section  
	    {
		sum0 = 0;
		for (i = 1; i < 400; i++)
		{
		    sum0 = sum0 + i;
		    i0 = i;
		}
#pragma omp critical
		{
		    sum = sum + sum0;
		} /*end of critical*/
	    } /* end of section */
#pragma omp section 
	    {
		sum0 = 0;
		for(i = 400; i < 700; i++)
		{
		    sum0 = sum0 + i;
		    i0 = i;
		}
#pragma omp critical
		{
		    sum = sum + sum0;
		} /*end of critical*/
	    }
#pragma omp section 
	    {
		sum0 = 0;
		for(i = 700; i < 1000; i++)
		{
		    sum0 = sum0 + i;
		    i0 = i;
		}
#pragma omp critical
		{
		    sum = sum + sum0;
		} /*end of critical*/
	    }
	} /* end of sections*/
	
    } /* end of parallel*/    
    known_sum = (999 * 1000) / 2;
    return ((known_sum == sum) && (i0 == 999) );
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
	printf("Testing omp section lastprivate\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp section lastprivate\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_section_lastprivate out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_section_lastprivate(stdout)){
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
