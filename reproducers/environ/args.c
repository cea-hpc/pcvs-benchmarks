#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <mpi.h>

#ifdef CC
	const char * c = "gcc";
#else
	const char * c = "toto";
#endif

int main(int argc, char *argv)
{
	#ifdef MINIFE_GLOBAL_ORDINAL
		char var_ref[] = MINIFE_GLOBAL_ORDINAL;
		char * myvar = (char *)malloc(32*sizeof(char));
		strcpy(myvar, "long long int");
		if(strcmp(var_ref, myvar) == 0)
		{
			if (strcmp(c, "gcc") == 0)
			{
				fprintf(stderr, "SUCCESS\n");
			}
			else
			{
				fprintf(stderr, "FAILURE\n");
			}
		}
		else
		{
			fprintf(stderr, "FAILURE\n");
			abort();
		}
	#else
		fprintf(stderr, "FAILURE\n");
		abort();
	#endif
	return EXIT_SUCCESS;
}
