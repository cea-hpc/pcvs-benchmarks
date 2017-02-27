#include <mpc.h>
#include <stdio.h>
#include "struct.h"


int a = 123;
struct pint root = { NULL, &a };
struct pint child = { &root, NULL };


int main( int argc, char ** argv )
{
	printf("A is %d\n", *child.spi->a );

	if( *child.spi->a != 123 )
	{
		printf("Error value not expected\n");
		return 1;
	}

	return 0;
}
