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
// Benjamin T. Grover and Walter Nissen

This file contains function declarations specifically for the CMGGenerator
file.
*/
#ifndef __CMG_GENERATOR_H__
#define __CMG_GENERATOR_H__

/*!\file
  Methods to be used only by cmg internally, not for use by application
  code
*/
int calculateBlockFromIJK(int i, int j, int k);
void initMesh( );
void sendInput( );
void setupSubdivisons(int cellContainerIndex,int iZones,int jZones,int kZones);


#endif
