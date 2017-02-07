
#include <stdio.h>
#include <math.h>
#include "omp_testsuite.h"
#include "omp_my_sleep.h"


int test_omp_taskwait(FILE * logFile){
    int result1 = 0;     /* Stores number of not finished tasks after the taskwait */
    int result2 = 0;     /* Stores number of wrong array elements at the end */

    int array[NUM_TASKS];
    int i;

    /* fill array */
    for (i = 0; i < NUM_TASKS; i++) 
        array[i] = 0;

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
                int myi;
                myi = i;
#pragma omp task
                {
                    my_sleep (SLEEPTIME);
                    array[myi] = 1;
                } /* end of omp task */
            } /* end of for */


#pragma omp taskwait


            /* check if all tasks were finished */
            for (i = 0; i < NUM_TASKS; i++) 
                if (array[i] != 1)
                    result1++;

            /* generate some more tasks which now shall overwrite 
             * the values in the tids array */
            for (i = 0; i < NUM_TASKS; i++) {
                int myi;
                myi = i;
#pragma omp task
                {
                    array[myi] = 2;
                } /* end of omp task */
            } /* end of for */

        } /* end of single */
    } /*end of parallel */

    /* final check, if all array elements contain the right values: */
    for (i = 0; i < NUM_TASKS; i++) {
        if (array[i] != 2)
            result2++;
    }

    return ((result1 == 0) && (result2 == 0));
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
	printf("Testing omp taskwait\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp taskwait\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_taskwait out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_taskwait(stdout)){
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
