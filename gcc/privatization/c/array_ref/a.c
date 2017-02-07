#include <stdio.h>
#include <string.h>

char aa[8192] = "Hello";
char  *b = aa;


int main( int argc, char ** argv )
{
	printf("%s == %s\n", aa, b );

	if( aa != b )
	{
		return 1;
	}

	return 0;
}
