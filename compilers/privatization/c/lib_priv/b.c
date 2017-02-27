#include <stdio.h>

int a = 456;


int foo(int rem_a)
{

  printf("LIBT: &a = %p\n", &a);
  printf("%d == %d ? \n", rem_a, a);
  if (rem_a != a) {
    return 1;
  }

  return 0;
}
