#ifndef __TETON_VAR_DECLS_HH__
#define __TETON_VAR_DECLS_HH__
#include "scstd.h"

#if (defined __INTEL_COMPILER) && (__INTEL_COMPILER >= 1100)
//
// icpc >= version 11.0
//
void
F77_ID(getrunstats_, getrunstats, GETRUNSTATS)
    (int *, int *, int *, int *, int *,
     double *, double *, double *, double *, double *,
     double *, double *, double *, double *, double *,
     double *, double *, double *, double *, char *, char *,
     long int *, long int*);

void
F77_ID(addboundary_, addboundary, ADDBOUNDARY)
    (int *, char *, int *, int *, int *, int *, int *,long int *);

void
F77_ID(addprofile_, addprofile, ADDPROFILE)
    (int *, int *, int *, int *, double *,
     char *, char *, char *, double *, double *, 
     long int*, long int *, long int *);

void
F77_ID(constructsize_, constructsize, CONSTRUCTSIZE)
    (int *, int *, int *, int *, int *,
     int *, int *, int *, int *, int *,
     int *, int *, int *, int *, int *, 
     double *, double *, double *, 
     char *, char *, char *, 
     char *, char *, char *,
     long int*, long int *, long int *,
     long int*, long int *, long int *);

void
F77_ID(resetsize_, resetsize, RESETSIZE)
    (double *, double *, double*, char *, char *, 
     char *, char *, char *, char *,
     long int *, long int *, long int *,
     long int *, long int *, long int *); 
#else

//
// icpc pre-11.0 and other compilers
//
void
F77_ID(getrunstats_, getrunstats, GETRUNSTATS)
    (int *, int *, int *, int *, int *,
     double *, double *, double *, double *, double *,
     double *, double *, double *, double *, double *,
     double *, double *, double *, double *, char *, char *);

void
F77_ID(addboundary_, addboundary, ADDBOUNDARY)
    (int *, char *, int *, int *, int *, int *, int *);

void
F77_ID(addprofile_, addprofile, ADDPROFILE)
    (int *, int *, int *, int *, double *,
     char *, char *, char *, double *, double *);

void
F77_ID(constructsize_, constructsize, CONSTRUCTSIZE)
    (int *, int *, int *, int *, int *,
     int *, int *, int *, int *, int *,
     int *, int *, int *, int *, int *, 
     double *, double *, double *, 
     char *, char *, char *, 
     char *, char *, char *);

void
F77_ID(resetsize_, resetsize, RESETSIZE)
    (double *, double *, double*, char *, char *, 
     char *, char *, char *, char *); 
#endif //end of compiler vendor condition


#endif
