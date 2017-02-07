#include <mpc.h>
#include <omp.h>
/* { dg-do compile } */
/* { dg-require-effective-target tls } */
extern int i;
#pragma omp threadprivate (i)

int i;
