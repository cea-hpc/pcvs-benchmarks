#include <mpi.h>
#include "a.h"

extern struct a * s;


struct b H5P_s = { &s };


int main( int argc, char ** argv )
{
	if( H5P_s.ps != &s )
	{
		printf("Error\n");
		return 1;
	}
	else
	{
		printf("OK\n");
	}

	return 0;
}
