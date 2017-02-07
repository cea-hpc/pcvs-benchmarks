
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <omp.h>
#include "src/omp_testsuite.h"

typedef struct
{
  char *name;
  a_ptr_to_test_function pass;
  a_ptr_to_test_function fail;
}
testcall;

static FILE *logFile;
static const char *logFileName = "test.log";

static testcall alltests[] = {
#include "src/feasible_test.h"
  {"end", 0, 0}
};

int
main (int argc, char **argv)
{
  int i = 0;
  int failed = 0;
  int success = 0;
  int crosschecked = 0;
  int N = 20;
  int j = 0;
  int result = 1;
  int crossfailed = 0;

  logFile = fopen (logFileName, "a");
  printf ("##############################################\n");
  printf ("########## OpenMP Validation Test ############\n");
  printf ("##############################################\n\n");

  while (strcmp(alltests[i].name,argv[1]) != 0)
    {
	i++;
    }


      		crossfailed = 0;
      		result = 1;
      		fprintf (logFile,"--------------------------------------------------\n					%%s\n--------------------------------------------------\n", alltests[i].name);
	      printf ("Test: %s ... ", alltests[i].name);
	      fflush (stdout);
      	for (j = 0; j < N; j++)
	{
		  fprintf (logFile, "# Check: ");

		  if (alltests[i].pass (logFile))
		    {
	 	     fprintf (logFile, "No errors occured during the %d. test.\n",
			       j + 1);
		      if (!alltests[i].fail (logFile))
			{
			  fprintf (logFile, "# Crosscheck: Verified result\n");
		  	  crossfailed++;
			}
		      else
			{
	     		  fprintf (logFile, "# Crosscheck: Coudn't verify result.\n");
			}
	    	    }
	  	   else
	    	    {
	         	fprintf (logFile, "--> Erroros occured during the %d. test.\n", j + 1);
	      		result = 0;
	    	    }
	}
      	if (result == 0)
	{
	  failed++;
	}
      	else
	{
	  success++;
	}
      	if (crossfailed > 0)
	{
	  crosschecked++;
	}
      	fprintf (logFile, "Result for %s:\n", alltests[i].name);
      	if (result)
	{
	  fprintf (logFile,
		   "Directiv worked without errors.\nCrosschecks verified this result with %5.2f%% certainty.\n",
		   100.0 * crossfailed / N);
	  printf (" verified with %5.2f%% certainty\n\n",
		  100.0 * crossfailed / N);
	}
     	else
	{
	  fprintf (logFile, "Directive failed the tests!\n");
	  printf (" FAILED\n\n");
	}
      
  

	fprintf (logFile,"\n\n Performed a total of %d tests, %d failed and %d successful with %d cross checked\n",i, 		failed, success, crosschecked);
  fclose (logFile);
  return failed;
}
