int a = 8;
int * uu = &a;

#include <stdio.h>

void bar()
{	
	printf("c.c:  a:  zz %p, uu %p\n", &a,  &uu ); 
}
