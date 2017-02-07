

/*
* Test for dynamic scheduling with chunk size
* Method: caculate how many times the iteration space is dispatched
*         and judge if each dispatch has the requested chunk size
*         unless it is the last one.
* It is possible for two adjacent chunks are assigned to the same thread
* Modifyied by Chunhua Liao
*/
#include <stdio.h>
#include <omp.h>
#include <unistd.h>
#include <stdlib.h>

#include "omp_testsuite.h"
#include "omp_my_sleep.h"

#define CFDMAX_SIZE 100
const int chunk_size = 7;

int test_omp_for_schedule_dynamic (FILE * logFile)
{
  int tid;
  
  int *tids;
  int i;


  int tidsArray[CFDMAX_SIZE];
  int count = 0;
  int tmp_count = 0; /*dispatch times*/
  int *tmp;  /*store chunk size for each dispatch*/
  int result = 0;
  
  tids = tidsArray;

#pragma omp parallel private(tid) shared(tids)
  {				/* begin of parallel */
     
      int tid;

    tid = omp_get_thread_num ();
#pragma omp for schedule(dynamic,chunk_size)
    for (i = 0; i < CFDMAX_SIZE; i++)
      {
	tids[i] = tid;
      }
     
  }				/* end of parallel */

  for (i = 0; i < CFDMAX_SIZE - 1; ++i)
    {
      if (tids[i] != tids[i + 1])
	{
	  count++;
	}
    }

  tmp = (int *) malloc (sizeof (int) * (count + 1));
  tmp[0] = 1;

  for (i = 0; i < CFDMAX_SIZE - 1; ++i)
    {
      if (tmp_count > count)
	{
	  printf ("--------------------\nTestinternal Error: List too small!!!\n--------------------\n");	/* Error handling */
	  break;
	}
      if (tids[i] != tids[i + 1])
	{
	  tmp_count++;
	  tmp[tmp_count] = 1;
	}
      else
	{
	  tmp[tmp_count]++;
	}
    }
/*
printf("debug----\n");
    for (i = 0; i < CFDMAX_SIZE; ++i)
	printf("%d ",tids[i]);
printf("debug----\n");
*/
/* is dynamic statement working? */
  for (i = 0; i < count; i++)
    {
      if ((tmp[i]%chunk_size)!=0) 
/*it is possible for 2 adjacent chunks assigned to a same thread*/
	{
         result++;
  fprintf(logFile,"The intermediate dispatch has wrong chunksize.\n");
	  /*result += ((tmp[i] / chunk_size) - 1);*/
	}
    }
  if ((tmp[count]%chunk_size)!=(CFDMAX_SIZE%chunk_size))
   { 
   result++;
  fprintf(logFile,"the last dispatch has wrong chunksize.\n");
   }
  /* for (int i=0;i<count+1;++i) printf("%d\t:=\t%d\n",i+1,tmp[i]); */
  return (result==0);
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
	printf("Testing omp for schedule(dynamic)\n\n");

	fprintf(stdout,"######## OpenMP Validation Suite V %s ######\n", OMPTS_VERSION );
	fprintf(stdout,"## Repetitions: %3d                       ####\n",REPETITIONS);
	fprintf(stdout,"## Loop Count : %6d                    ####\n",LOOPCOUNT);
	fprintf(stdout,"##############################################\n");
	fprintf(stdout,"Testing omp for schedule(dynamic)\n\n");

	for ( i = 0; i < REPETITIONS; i++ ) {
		fprintf (stdout, "\n\n%d. run of test_omp_for_schedule_dynamic out of %d\n\n",i+1,REPETITIONS);
		if(test_omp_for_schedule_dynamic(stdout)){
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
