#include <mpi.h>
#include <stdio.h>

extern int a;
extern int * zz;
extern int * uu;


int main( int argc, char ** argv )
{


	printf("%p=%p=%p\n", &a, zz, uu );

	if( (zz != &a) || (zz != uu ) )
	{
		printf("ERROR variables are different\n");
		return 1;
	}
	else
	{
		printf("OK variables are equal\n");
	}

	return  0;
}
