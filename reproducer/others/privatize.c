#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>
#include <assert.h>

int toto = 4;
int * toto_p = &toto;


int main(int argc, char **argv)
{
  fprintf(stderr,"ADDR %p value %p\n",toto_p,&toto);
  assert(&toto == toto_p);
  return 0;
}
