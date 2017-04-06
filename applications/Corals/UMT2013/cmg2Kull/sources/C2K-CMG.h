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
#ifndef C2KCMG
#define C2KCMG

#ifdef CMG_FAKE

typedef enum {CMG_INVALID=0, CMG_TET=4, CMG_PYR=5, CMG_PRI=6, CMG_HEX=8} cZoneType;

void Init_CMG_Problem (int ProblemNumber);

							// ALL IDs HERE ARE GLOBAL IDs
								
//-------------------------------------------------- NODE query functions

void cnmnoda (int *nnodes);	// Get # of ALL nodes inside or on the boundary of this domain

void cnodsa (int *nodes, int *offset);	// Get a list of GIDs for ALL nodes, and
										// the 0 origin offset to the start of UNOWNED nodes

void cnodpos (int nodeid, double *x, double *y, double *z);	// Get position for the given node GID

//-------------------------------------------------- ZONE query functions

void cnumzns (int *nzones);	// Get the number of zones on this domain (ALL ZONES are OWNED ZONES)
void czns    (int *zones );	// Get the list of GIDs for zones on this domain

void cgetznn (int gloid, int *nodes);		// Get the nodes in order for the given zone GID

void cgetztp (int gloid, cZoneType *typ);	// Get the zone type for a given zone GID

//-------------------------------------------------- FACE query functions

void cnumbdf(int *nfaces);	// Get # of COMM boundary faces on this domain; owned + unowned

void cbdrfc (int *faces);	// Get the list of COMM boundary face GIDs; owned + unowned

void cfcnda(int facid, int *nodid);	// get four node GIDs for a COMM boundary face

//-------------------------------------------------- COMM neighbor query functions

void crecfsz ( int *size );		// the size of the domain ID and # of Faces per domain arrays

void crecfar (int *domar, int *fcszar );	// domain ID array and # of Faces per domain array

void crecf ( int domid, int *faces );		// get the GIDs of the COMM Faces for domain domid

#else

#include "cmg.h"
#include "CMGDebug.h"

void Init_CMG_Problem (int ProblemNumber);
void Init_CMG_Problem_From_File (const char* meshFile);
#endif /* end of CMG_FAKE block */

#endif
