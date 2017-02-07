
#include <stdio.h>
#include <math.h>
#include "omp_testsuite.h"
#include "omp_my_sleep.h"

int test_omp_task_untied(FILE * logFile){

  
  int i;
  int count;
  int start_tid[NUM_TASKS];
  int current_tid[NUM_TASKS];
  
  count = 0;
  
  /*initialization*/
  for (i=0; i< NUM_TASKS; i++){
    start_tid[i]=0;
    current_tid[i]=0;
  }
  
  #pragma omp parallel firstprivate(i)
  {
    #pragma omp single
    {
      for (i = 0; i < NUM_TASKS; i++) {
        
        int myi = i;
        #pragma omp task untied
        {
          my_sleep(SLEEPTIME);
          start_tid[myi] = omp_get_thread_num();
          current_tid[myi] = omp_get_thread_num();
          
          #pragma omp taskwait
          
          if((start_tid[myi] %2) !=0){
            my_sleep(SLEEPTIME);
            current_tid[myi] = omp_get_thread_num();
          
          } /* end of if */ 
          else {
            current_tid[myi] = omp_get_thread_num();
          }
          

        } /*end of omp task */
        
      } /* end of for */
    } /* end of single */
  } /* end of parallel */

  for (i=0;i<NUM_TASKS; i++)
  {
    printf("start_tid[%d]=%d, current_tid[%d]=%d\n",i, start_tid[i], i , current_tid[i]);
    if (current_tid[i] == start_tid[i])
      count++;
  }
  return (count<NUM_TASKS);
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
	printf("Testing omp task untied\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp task untied\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_task_untied out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_task_untied(stdout)){
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
