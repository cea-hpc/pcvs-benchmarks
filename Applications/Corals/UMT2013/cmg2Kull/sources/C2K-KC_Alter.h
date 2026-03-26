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
#ifndef C2KKCALTER
#define C2KKCALTER

#include <stdio.h>
#include "C2K-Storage.h"
#include "C2K-Lists.h"

void InsertSideInFacesOrderedSideList (	SidePtr pNewSide,	// the Side to be inserted
										FacePtr pFace,		// the Face that gains the Side
										SidePtr pSideAfter ); // after this Side to maintain CCW order
void DeleteSidesCCWInFace (FacePtr pFace, /* between */ SidePtr pSide1, /* and */ SidePtr pSide2);

void InsertSideInEdgesSideList (SidePtr pSide, EdgePtr pEdge ); // this list is unordered
void RemoveSideInEdgesSideList (SidePtr pSide, EdgePtr pEdge );

void      AddSideToNodesSideList (SidePtr pSide, NodePtr pNode);
void RemoveSideFromNodesSideList (SidePtr pSide, NodePtr pNode);
void RemoveSideFromItsNodes		 (SidePtr pSide				  );

SidePtr   FindSideOnFace ( FacePtr pFace, NodePtr pNode1);

void      AddFaceToZonesFaceList ( FacePtr pFace, ZonePtr pZone );
void RemoveFaceFromZonesFaceList ( FacePtr pFace, ZonePtr pZone );

void      AddSideToZonesSideList ( SidePtr pS, ZonePtr pZone );
void RemoveSideFromZonesSideList ( SidePtr pS, ZonePtr pZone );

void ClearZonesFaceList ( ZonePtr pZone );
void ClearZonesSideList ( ZonePtr pZone );

LItemPtr CreateListOfAZonesNodes ( ZonePtr pZone );

LItemPtr CreateListOfZonalNodes ( ZonePtr pZ1, ZonePtr pZ2 );

void MakeAFacesSideList				(FacePtr pF, SidePtr pFirstSide);
void MakeAFacesNodeListFromSideList (FacePtr pF);

FacePtr FindOppositeFace (FacePtr pFace);

#endif
