/*
// Copyright 2006 The Regents of the University of California.
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
// Benjamin T. Grover and Walter Nissen

Debugging tools for the library
*/

#ifndef __CMG_DEBUG_H__
#define __CMG_DEBUG_H__


#ifdef CMGDEBUG
#include <stdio.h>
#define CMGDPRINT(...) printf(__VA_ARGS__)
#define CMGFPRINT(...) fprintf(stderr, __VA_ARGS__)
#else
#define CMGDPRINT(...)
#define CMGFPRINT(...)
#endif

#endif
