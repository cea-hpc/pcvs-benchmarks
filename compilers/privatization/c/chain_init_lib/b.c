extern int * uu;
int * zz = uu;

#include <stdio.h>

void foo()
{	
	printf("b.c:  zz %p, uu %p\n", &zz, &uu ); 
}
