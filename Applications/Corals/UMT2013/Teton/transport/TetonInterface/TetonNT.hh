//---------------------------------*-C++-*----------------------------------//
// Copyright 1998 The Regents of the University of California.
// All rights reserved.
//
// Created on: May 1998
// Created by:  Pat Miller
// Also maintained by: Michael Nemanic
//--------------------------------------------------------------------------//

#ifndef __TETON_TETONNT_HH__
#define __TETON_TETONNT_HH__

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <vector>
#include "scstd.h"
using std::vector;

#undef max

extern "C"
{

// Used to get pointer back as void * from the fortran wrapper.

   int
   compr(void const*, void const*);

   void
   rmdupsort(int*, int*);

}

#endif                   // __TETON_TETONNT_HH__
