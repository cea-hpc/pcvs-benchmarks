#include <stdio.h>

int a = 8;
int * b = &a;

extern int * zz;

int main( int argc, char ** argv )
{
	printf("b : %d\n", *b );

	if( *b != 8 )
	{
		printf("ERROR BAD VALUE\n");
		return 1;
	}

	printf("zz : %d\n", *zz );

	if( *zz != 90 )
	{
		printf("ERROR BAD VALUE\n");
		return 1;
	}

	return  0;
}
