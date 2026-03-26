/* -------------------------------------------------------------------------  */
/*  Copyright 2006.  The Regents of the University of California.  */
/*  All rights reserved.  */
/* -------------------------------------------------------------------------  */
/*  This work was produced at the University of California, Lawrence  */
/*  Livermore National Laboratory (UC LLNL) under contract no.  */
/*  W-7405-ENG-48 (Contract 48) between the U.S. Department of Energy  */
/*  (DOE) and the Regents of the University of California (University)  */
/*  for the operation of UC LLNL.  The rights of the Federal Government are  */
/*  reserved under Contract 48 subject to the restrictions agreed upon by  */
/*  the DOE and University as allowed under DOE Acquisition Letter 97-1.  */
/* -------------------------------------------------------------------------  */
#ifndef C2KKCCHECK
#define C2KKCCHECK

#include "C2K-Storage.h"

//-------------------- Prototypes for functions

int  CheckFaceInfo ( FacePtr pF, char *s );
int  CheckZoneInfo ( ZonePtr pZ, char *s );
int  CheckEdgeInfo ( EdgePtr pE, char *s );

int  CheckAllEdgeInfo		   ( char *s );
void CheckAllConnectivityInfo  ( char *s );
void CACI					   ( char *s );	// shorthand alias for CheckAllConnectivityInfo

#endif
