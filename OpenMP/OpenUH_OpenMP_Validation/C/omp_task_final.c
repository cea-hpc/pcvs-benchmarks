
#include <stdio.h>
#include <math.h>
#include "omp_testsuite.h"
#include "omp_my_sleep.h"


int test_omp_task_final(FILE * logFile){
    
    int tids[NUM_TASKS][NUM_TASKS];
    int i,j;
    
    int error = 0;
#pragma omp parallel 
{
#pragma omp single
    {
        for (i = 0; i < NUM_TASKS; i++) {
            
            /* First we have to store the value of the loop index in a new variable
             * which will be private for each task because otherwise it will be overwritten
             * if the execution of the task takes longer than the time which is needed to 
             * enter the next step of the loop!
             */
            int myi, myj;
            myi = i;

            #pragma omp task final(i>=10)
            {
                myj = 0;
                my_sleep (SLEEPTIME);
                tids[myi][myj] = omp_get_thread_num();

                for( myj = 1; myj < NUM_TASKS; myj++ ){
                  #pragma omp task
                  {
                    my_sleep (SLEEPTIME);
                    tids[myi][myj] = omp_get_thread_num();
                  }
                }
            } /* end of omp task */
            
        } /* end of for */
    } /* end of single */
} /*end of parallel */

/* Now we ckeck if more than one thread executed the tasks. */
    for (i = 10; i < NUM_TASKS; i++) {
      for( j = 1; j < NUM_TASKS; j++ ){
        if (tids[i][0] != tids[i][j])
            error++;
      }
    }
    return (error==0);
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
	printf("Testing omp task final\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp task final\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_task_final out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_task_final(stdout)){
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
