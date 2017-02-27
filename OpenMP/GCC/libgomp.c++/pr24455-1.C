#include <mpc.h>
#include <omp.h>
// { dg-do compile }
// { dg-require-effective-target tls }
extern int i;
#pragma omp threadprivate (i)

int i;

int main() {
return 0 ;
}
