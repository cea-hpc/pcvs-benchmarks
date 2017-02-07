
#include <stdio.h>
#include <math.h>
#include "omp_testsuite.h"




/* Utility function do spend some time in a loop */
int test_omp_task_imp_firstprivate (FILE * logFile)
{
    int i=5;
    int k = 0;
    int result = 0;
    int task_result = 1;
   #pragma omp parallel firstprivate(i)
    {
      #pragma omp single
      {
     
        
          for (k = 0; k < NUM_TASKS; k++)
	        {
                    #pragma omp task shared(result , task_result)
                        {
                          int j;
			  //check if i is private
                          if(i != 5)
                            task_result = 0;
                       
                          for(j = 0; j < NUM_TASKS; j++)
                              i++;
                          //this should be firstprivate implicitly
                        }
		}

	  #pragma omp taskwait
	  result = (task_result && i==5);
       }
                
    }
    
    return result;
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
	printf("Testing omp task\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp task\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_task_imp_firstprivate out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_task_imp_firstprivate(stdout)){
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
