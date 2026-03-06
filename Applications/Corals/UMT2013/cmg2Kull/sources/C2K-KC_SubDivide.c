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
#include <stdio.h>
#include <stdlib.h>

#ifdef __APPLE__
#include <Math.h>
#else
#include <math.h>
#endif


#include "C2K-Storage.h"
//#include "C2K-Lists.h"
#include "C2K-KC_CutZone.h"
#include "C2K-KC_SubDivide.h"
#include "C2K-KC_Info.h"
#include "C2K-KC_Check.h"

NodePtr		pHexNode[8];	// to store STANDARD ORDER hex Nodes
FacePtr		pF_Bottom, pF_Top, pF_Front, pF_Back, pF_Left, pF_Right;	// SO Faces
EdgePtr		pE_A, pE_B, pE_C, pE_D, pE_E, pE_F;							// SO Edges

IntersectionTD	I_A, I_C, I_E, I_F;	// to store info for & from four Face cuts
IntersectionTD	I_B, I_D;			// to store info for the 1st Face insertion
IntersectionTD	NewFaceEdges[4];	// to store info for inserting Faces


//================================================================================
void printFsNodes (FacePtr pF)
{
	SidePtr		pS = pF->p1stSide;
	
	while (pS != NULL)
	{
		printf("%3d | ", pS->pNode1->nNodeLID);
		pS = pS->pFsNextS;
	}
	printf("\n");
}

//================================================================================
EdgePtr Split4SidedFace ( IntersectionTD *pI, FacePtr pFace, NodePtr pSN, NodePtr pEN)
//--------------------------------------------------------------------------------
// An FOUR-SIDED Face will be cut into two Faces.
//--------------------------------------------------------------------------------
{	EdgePtr			pNewE;	//
	
	if ( NumSidesInFace (pFace) != 4 )
	{	printf ("ERROR; didn't find 4-Sided Face.\n");
		exit (1);
	}
		
	pI->pStartNode	     = pSN;	// set parameters for call to divide the Face
	pI->pEndNode		 = pEN;
	pI->pSideOnFace2BCut = pFace->p1stSide;
	pI->pLeft_base_side  = NULL;
	pI->pRight_base_side = NULL;
	
	MakeNewEdgeAndDivide2Faces ( pI );
	
	pNewE = pI->pEdge2CutFace;
	
	if (pI->pLeft_base_side == NULL) {	printf("Split4SidedFace: LEFT BASE SIDE WAS NOT SET.\n");
		exit(1);}
	
	if (pI->pRight_base_side == NULL) {	printf("Split4SidedFace: RIGHT BASE SIDE WAS NOT SET.\n");
		exit(1);}
	
	printf("Split4SidedFace:\n Edge LID = %3d, N1 LID = %3d, N2 LID = %3d\n", 
         pNewE->nEdgeLID, pNewE->pNode1->nNodeLID, pNewE->pNode2->nNodeLID);
	
	return (pNewE);
}

//================================================================================
void GatherSO_NodesFacesEdges (ZonePtr pZ)	// Gather into STANDARD ORDER a Zone's Nodes
{									// ONLY works for PRISTINE hexahedra
	FacePtr		pF;		// to navigate a Zone's Faces
	SidePtr		pS;		// to loop over the Sides of a Face, +++
	int			nN = 0;	// index of the Node to be stored, and to loop over Nodes
	NodePtr		pN;		// 
	Loc3DTD		L;		// a Node's physical location in 3D space
	
	pF = pZ->p1stFace;	// use the CCW ordered Nodes on the 1st Face
	pS = pF->p1stSide;	// Start at the 1st Side
	while (pS != NULL)
	{
		pHexNode[nN] = pS->pNode1;
		nN++;
		pS = pS->pFsNextS;	// go to the next CCW Side on the Face
	}
	printf("GatherSO_Nodes:\n #Nodes so far  = %3d\n", nN);
	
	pF_Bottom = pF;			// remember the "Bottom" Face
	
	pS = pF->p1stSide;	// go back to the Face's 1st Side (bottom of SO Hex)
	pS = pS->pTopSide;	// move to ajoining Face (side of SO Hex)
	pF_Front = pS->pFace;	// remember the "Front" Face
	
	pS = pS->pLeftSide;	// move "up" the hex
	pS = pS->pLeftSide;	// move to "top" Edge of SO Hex's side
	pS = pS->pTopSide;	// moves onto "top" face of SO Hex, "above" the 1st Face's 1st Side
	pF_Top = pS->pFace;	// remember the "Top" Face
	
	pHexNode[nN] = pS->pNode2;	pS = pS->pRightSide; nN++;	// store Node, move CW, increment index
	pF_Right = pS->pTopSide->pFace;							// store the "Right" Face
	
	pHexNode[nN] = pS->pNode2;	pS = pS->pRightSide; nN++;	// store Node, move CW, increment index
	pF_Back = pS->pTopSide->pFace;							// store the "Back" Face
	
	pHexNode[nN] = pS->pNode2;	pS = pS->pRightSide; nN++;	// store Node, move CW, increment index
	pF_Left = pS->pTopSide->pFace;							// store the "Left" Face
	
	pHexNode[nN] = pS->pNode2;	pS = pS->pRightSide; nN++;	// store Node, move CW, increment index

	printf(" #Nodes at end  = %3d\n", nN);
	for (nN = 0; nN < 8; nN++)
	{	pN = pHexNode[nN];
		L  = pN->Loc3D;
		printf(" #N = %3d, ID = %3d, %g, %g, %g\n", nN, pN->nNodeLID, L.x, L.y, L.z);
	}
	
	printf("Faces collected:\n");
	printf(" Bottom Face LID  = %3d, Nodes: ", pF_Bottom->nFaceLID); printFsNodes (pF_Bottom);
	printf(" Top Face LID     = %3d, Nodes: ", pF_Top->nFaceLID   ); printFsNodes (pF_Top   );
	printf(" Front Face LID   = %3d, Nodes: ", pF_Front->nFaceLID ); printFsNodes (pF_Front );
	printf(" Back Face LID    = %3d, Nodes: ", pF_Back->nFaceLID  ); printFsNodes (pF_Back  );
	printf(" Right Face LID   = %3d, Nodes: ", pF_Right->nFaceLID ); printFsNodes (pF_Right );
	printf(" Left Face LID    = %3d, Nodes: ", pF_Left->nFaceLID  ); printFsNodes (pF_Left  );
	
	//-------------------- Find Edges B & D
	pS = pZ->p1stSide;
	while (pS != NULL)	// loop over all Sides of a Zone
	{
		if ((pS->pNode1 == pHexNode[2]) && (pS->pNode2 == pHexNode[6]))
			pE_B = pS->pEdge;
		
		if ((pS->pNode1 == pHexNode[4]) && (pS->pNode2 == pHexNode[0]))
			pE_D = pS->pEdge;
		
		pS = pS->pZsNextS;	// go to the next CCW Side on the Face
	}
	
	CheckAllConnectivityInfo ("Before 4 Face cuts.\n");
	
	//-------------------- Create 4 new Edges & store in I_A, I_C, I_E, I_F
	pE_A =  Split4SidedFace ( &I_A, pF_Bottom, pHexNode[0], pHexNode[2]);
	CheckAllConnectivityInfo ("***** After cut to make Edge A. *****\n");
	
	pE_C =  Split4SidedFace ( &I_C, pF_Top   , pHexNode[6], pHexNode[4]);
	CheckAllConnectivityInfo ("***** After cut to make Edge C. *****\n");
	
	pE_E =  Split4SidedFace ( &I_E, pF_Back  , pHexNode[2], pHexNode[7]);
	CheckAllConnectivityInfo ("***** After cut to make Edge E. *****\n");
	
	pE_F =  Split4SidedFace ( &I_F, pF_Left  , pHexNode[7], pHexNode[0]);
	CheckAllConnectivityInfo ("***** After cut to make Edge F. *****\n");
	
	//----- Collect information about Edge B
	I_B.pStartNode	     = pHexNode[2];
	I_B.pEndNode		 = pHexNode[6];
	I_B.pEdge2CutFace    = pE_B;
	
	pS = I_E.pRight_base_side->pLeftSide;
	I_B.pLeft_base_side  = pS;
	I_B.pRight_base_side = pS->pTopSide;
	
	//----- Collect information about Edge D
	I_D.pStartNode	     = pHexNode[4];
	I_D.pEndNode		 = pHexNode[0];
	I_D.pEdge2CutFace    = pE_D;
	
	pS = I_F.pRight_base_side->pRightSide;
	I_D.pLeft_base_side  = pS;
	I_D.pRight_base_side = pS->pTopSide;
	
}

//================================================================================
void LinkOppositeFaces (ZonePtr pZ)	// Ensures that all Faces of a Zone that have
{	FacePtr		pF, pOF;			// opposite Faces are properly linked.
	SidePtr		pS, pOS;
	
	pF = pZ->p1stFace;
	while (pF != NULL)
	{	pS  = pF->p1stSide;
		pOS = pS->pOppositeSide;
		pOF = pOS->pFace;
		pF->pOppositeFace = pOF;
		pOF->pOppositeFace = pF;
		
		pF = pF->pZsNextF;
	}
}

//================================================================================
void DoTwoCuts (ZonePtr pZ2Cut)
{	
	ZonePtr		pNewZ1, pNewZ2;
	FacePtr		pNewF1, pNewF2;
	
	ClearCutEdgeArray ( );

	AddCutEdge2Array ( &I_A );
	AddCutEdge2Array ( &I_B );
	AddCutEdge2Array ( &I_C );
	AddCutEdge2Array ( &I_D );
	
	pNewZ1 = Insert2Faces ( pZ2Cut, &pNewF1 ); 
//	printf("DoTwoCuts:\n 1st Zone cut ran without crashing.\n");
	
	CheckAllConnectivityInfo ("After 1st Cut.\n");
	
	//----- Reset one information field in Intersection struct A	
	I_A.pRight_base_side = I_A.pLeft_base_side->pTopSide;
	
	ClearCutEdgeArray ( );

	AddCutEdge2Array ( &I_A );
	AddCutEdge2Array ( &I_E );
	AddCutEdge2Array ( &I_F );

	pNewZ2 = Insert2Faces ( pNewZ1, &pNewF2 ); 
//	printf("DoTwoCuts:\n 2nd Zone cut ran without crashing.\n");
	
	LinkOppositeFaces (pZ2Cut);	
	LinkOppositeFaces (pNewZ1);	
	LinkOppositeFaces (pNewZ2);	
	
}

//================================================================================
void DoOneCut (ZonePtr pZ2Cut)
{	
	ZonePtr		pNewZ1;
	FacePtr		pNewF1;
	
	ClearCutEdgeArray ( );

	AddCutEdge2Array ( &I_A );
	AddCutEdge2Array ( &I_B );
	AddCutEdge2Array ( &I_C );
	AddCutEdge2Array ( &I_D );
	
	pNewZ1 = Insert2Faces ( pZ2Cut, &pNewF1 ); 
//	printf("DoTwoCuts:\n 1st Zone cut ran without crashing.\n");
	
	CheckAllConnectivityInfo ("After 1st Cut.\n");
	
	//----- Reset one information field in Intersection struct A	
	I_A.pRight_base_side = I_A.pLeft_base_side->pTopSide;
	
	
	LinkOppositeFaces (pZ2Cut);	
	LinkOppositeFaces (pNewZ1);	
	
}

//================================================================================
int doZoneSubDivision ( ZonePtr pZ )
{
	int		numC, numF, numN, numS, numD1, numD2;	// Zone characteristics
	int didSubdivision = 0;
  
	numC  = NumCornersOfZone (pZ);
	numF  = NumFacesOfZone   (pZ);
	numS  = NumSidesOfZone   (pZ);
  numN  = NumNodesOfZone   (pZ);
	numD1 = numF - 6;
	numD2 = (numS / 2) - 12;
	
	
	if ( (numD1 != 0) || (numD2 != 0) || (numN != 8) )	// had to give up checking : (numC != 8) || 
	{
		printf("doZoneSubDivision: Not able to handle this kind of Zone.\n");
//		printf("doZoneSubDivision:\n #C  = %3d\n #F  = %3d\n #S  = %3d\n #D1 = %3d\n #D2 = %3d\n",
		printf(" #C  = %3d\n #F  = %3d\n #S  = %3d\n #D1 = %3d\n #D2 = %3d\n",
           numC,       numF,       numS,       numD1,      numD2);
	}
	else //---------- Here starts the subdivision of the Zone
	{
		GatherSO_NodesFacesEdges (pZ);
/* 		DoTwoCuts (pZ); */
		DoOneCut (pZ);
    didSubdivision = 1;
	}
  return didSubdivision;
}

//================================================================================
void SubDivideZones ( double ratio )
{
	ZonePtr		pZ;	// to loop over all Zones
  int subdividedZoneCount = 0;
  
  static long int theSeedValue = 1;
  
  if ( theSeedValue != -1 ) // only seed the random number generator once
  {
    srand48( theSeedValue );
    theSeedValue = -1;
  }
  
	pZ = pZone ( 0 );
	while (pZ != NULL)
	{
		if (pZ->SubDivisionControl == SUBDIVISION_OK)
		{
      if( pZ->nZoneLID == 62)
      {
        printf("SubDivideZones: Zone LID = %3d can be subdivided.\n", pZ->nZoneLID);
        if( 1 == doZoneSubDivision ( pZ ) )
        {
          subdividedZoneCount++;
        }
      }
      
/*       if( drand48() < ratio )  // only subdivide a portion of the mesh */
/*       { */
/*         printf("SubDivideZones: Zone LID = %3d can be subdivided.\n", pZ->nZoneLID); */
/*         if( 1 == doZoneSubDivision ( pZ ) ) */
/*         { */
/*           subdividedZoneCount++; */
/*         } */
/*       } */
		}
		pZ = pZ->pNextZone;
	}
  printf(" SubDivideZones:  total number of zones divided = %d\n",subdividedZoneCount);
}


