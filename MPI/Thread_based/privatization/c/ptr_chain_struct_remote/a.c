#include <mpc.h>
#include <stdio.h>
#include "struct.h"


extern struct pint root;
struct pint child = { &root, NULL };


int main( int argc, char ** argv )
{
  printf("&root  = %p, root.spi  = %p, root->a  = %p, *root->a     = %d\n",
         &root, root.spi, root.a, *root.a);
  printf("&child = %p, child.spi = %p, child->a = %p, child.spi->a = %p, "
         "*child.spi->a = %d\n",
         &child, child.spi, child.a, child.spi->a, *child.spi->a);
  printf("A is %d\n", *child.spi->a);

  if (*child.spi->a != 123) {
    printf("Error value not expected\n");
    return 1;
  }

  return 0;
}
