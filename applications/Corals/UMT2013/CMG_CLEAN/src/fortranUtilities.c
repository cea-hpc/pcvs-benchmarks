/*
  // Copyright 2005 The Regents of the University of California.
  // All rights reserved.
  //--------------------------------------------------------------------------
  //--------------------------------------------------------------------------
  //
  // This work was produced at the University of California, Lawrence
  // Livermore National Laboratory (UC LLNL) under contract no.
  // W-7405-ENG-48 (Contract 48) between the U.S. Department of Energy
  // (DOE) and the Regents of the University of California (University)
  // for the operation of UC LLNL.  The rights of the Federal Government are
  // reserved under Contract 48 subject to the restrictions agreed upon by
  // the DOE and University as allowed under DOE Acquisition Letter 97-1.
  //

  This file contains the implementation of the functions that mangle data
  to return to Fortran.

*/

#include <string.h>

#include "fortranUtilities.h"

void fixStringF77(char *str, int storageSize)
{
  int ii;
 
  /* Strlen does not include the terminating null, so that is the first
     character that will be overwritten */
  for (ii = strlen(str); ii < storageSize; ++ii) {
    str[ii] = ' ';
  }
}

void copyStringF77(char *dest, const char* src, int storageSize) {

  strcpy(dest, src);
  fixStringF77(dest, storageSize);

}
