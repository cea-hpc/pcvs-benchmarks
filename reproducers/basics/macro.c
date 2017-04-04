
#ifndef S_ARG
#define S_ARG "DEFAULT_S_ARG"
#endif

#ifndef C_ARG
#define C_ARG 0
#endif

#include <stdio.h>

int main(int argc, char *argv[])
{
	fprintf(stderr, "S_ARG = '%s'\n", S_ARG  );
	
	if(C_ARG)
		fprintf(stderr, "C_ARG seems valid\n");
	else
		fprintf(stderr, "C_ARG does not exist\n" );

	return 0;
}
