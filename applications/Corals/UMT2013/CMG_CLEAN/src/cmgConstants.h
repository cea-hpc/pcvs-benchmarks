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
  // Walt Nissen
  //
  
  This defines the user-settable and other constant definitions.

*/

#ifndef CMGCONSTANTS_H
#define CMGCONSTANTS_H

/* Defines the maximum number of blk calls that can be made
to define blocks on or off (because this is not declared in advance).
*/
#define CMG_MAX_BLOCK_DEFINITIONS 200000

#define CMG_MAX_FACE_DEFINITIONS 400000

#define CMG_MAX_EDGE_DEFINITIONS 400000

#define CMG_MAX_VERTEX_DEFINITIONS 400000

/* Defines the maximum number of tag calls that can be made
to define mesh tags (because this is not declared in advance).
*/
#define CMG_MAX_MESH_TAG_DEFINITIONS 1000

/* Defines the maximum number of sub calls that can be made
to define subdivisions (because this is not declared in advance).
*/
#define CMG_MAX_SUBDIVISION_DEFINITIONS 1000

/* Defines the currently unused maximum length of a line in input */
#define CMG_MAX_LINE 1000

/* Defines the maximum number of characters (including the null terminator)
that may be used in CMG. This is also the size of the character array given
to Fortran */
#define CMG_MAX_NAME_SIZE 80

/* DEFINEs so Walt can retain his sanity */
#define true 1;
#define false 0;

#ifndef bool
typedef unsigned bool;
#endif

#endif /* __CMG_CONSTANTS_H__ */
