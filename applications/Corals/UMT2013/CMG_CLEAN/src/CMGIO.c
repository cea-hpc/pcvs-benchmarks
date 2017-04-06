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

  This file contains the implementation of the commands from cmg.h that will
  be used to query the tags and materials on the mesh

*/

/*!\file CMGIO.c
  \brif IO Routines for the CMG

*/
#include "cmg.h"
#include "parseStructs.h"

extern SuperMeshSize sms;
extern SubBlockContainer blocks;
extern NumZones numZones;
extern MeshTagContainer meshTags;
extern SubdivisionContainer subdivisions;
extern NodeDataContainer nodeData;



/* The FILE read by the parser */
extern FILE *yyin;

void csetinp(FILE* inputFile) {
  yyin = inputFile;
}


/* Read the mesh input from stdin */
void creadinp( ) {
  /* Call the parsing routine */
  yyparse();
}

/* Fortran creadinp */
void creadinp_( ) {
  creadinp( );
}

/* Print the result of parsing the input */
void cprntinp( ) {
  /* Now print the structures we just defined */
  SuperMeshSizePrint( &sms );
  SubBlockContainerPrint( &blocks );
  NumZonesPrint( &numZones);
  MeshTagContainerPrint( &meshTags );
  SubdivisionContainerPrint( &subdivisions );

}

/* fortran cprntinp */
void cprntinp_( ) {
  /* Just call the C API */
  cprntinp( );
}



  
