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
#ifndef CUTZONE
#define CUTZONE

#include <stdio.h>
#include "C2K-Storage.h"

struct Intersection {
	Loc3DTD		intersect_loc;	// At this location ...
	EdgePtr		pEdge2BCut;		// this edge is intersected by the cut plane.
	NodePtr		pStartNode,		// node created to divide edge2Bcut
				pNode1L,pNode1R,//   and the nodes to its left and right
				pEndNode,		// node that divides the next edge [convenient duplicate]
				pNode2L,pNode2R;//   and the nodes to its left and right
	EdgePtr		pEdge2CutFace;	// created between start_node and end_node to divide a face
	SidePtr		pSideOnFace2BCut;// need to remember this for routine divideAllFaces
	SidePtr		pLeft_base_side,  pLeft_top_side,		// sides created around edge2cutFace
				pRight_base_side, pRight_top_side;
//	ptr_to_No		leftSide, rightSide;	// needed?? avoid findSide later in function _____?
};											// same question about left and right nodes
typedef struct Intersection	IntersectionTD;

//----- ATOMIC OPERATIONS -----

void MakeNewEdgeAndDivide2Faces ( IntersectionTD *I );

ZonePtr Insert2Faces ( ZonePtr pZone2BCut, FacePtr *ppANewFace );

//----- Private array access needed to use the atomic operations

void ClearCutEdgeArray ( );
int AddCutEdge2Array ( IntersectionTD *I );

#endif
