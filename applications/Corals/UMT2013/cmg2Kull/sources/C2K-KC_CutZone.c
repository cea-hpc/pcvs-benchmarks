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
#include <stdlib.h>

//#ifdef __APPLE__
//#include <Math.h>
//#else
#include <math.h>
//#endif

#include "C2K-Storage.h"
#include "C2K-KC_Alter.h"
//#include "C2K_KC_Check.h"
#include "C2K-KC_CutZone.h"
#include "C2K-KC_Info.h"
//======================================== Globals private to CutZone
#define MAX_CZ_CUTS  100
#define MAX_CZ_FACES 100

IntersectionTD	cut_edge[MAX_CZ_CUTS];	// array to store intersected edges + many more items
int				num_cuts;				// count of the number of edges intersected

//---------- These face arrays contain ALL Faces AFTER Faces have been divided by new edges
FacePtr		pRightFace[MAX_CZ_FACES], pLeftFace[MAX_CZ_FACES];
int			num_right_faces, num_left_faces;

double		minCutRatio = 1.0; // diagnostic for Edge intersection points close to Nodes

void CheckCutRatio (double FVP1, double FVP2)
{	double	A1, A2, Ratio;
	
	A1 = fabs(FVP1); A2 = fabs(FVP2);
	if (A1 > A2) Ratio = A2/(A1+A2); else Ratio = A1/(A1+A2);
	if (Ratio < minCutRatio) minCutRatio = Ratio;
//	if (minCutRatio < 0.1) printf("minCutRatio = %e\n", minCutRatio);
}

void ClearCutEdgeArray ( )
{	num_cuts = 0;
}

int AddCutEdge2Array ( IntersectionTD *I )
{
	cut_edge[num_cuts] = *I;
	num_cuts += 1;
	return (num_cuts);
}

//===============================================================================
void LinkUpNewSide ( FacePtr pBFace, 
                     NodePtr pRNode, NodePtr pLNode, 
                     SidePtr pRSide, SidePtr pLSide,
                     SidePtr pAddedSide, EdgePtr pNewEdge)
{	SidePtr		pS;

	pAddedSide->pNode1		= pRNode;
	pAddedSide->pNode2		= pLNode;
	pAddedSide->pEdge		= pNewEdge;
	pAddedSide->pFace		= pBFace;
	pAddedSide->pZone		= pBFace->pZone;
	//-------------------- now set LEFT and RIGHT pointers for the new side
	pAddedSide->pRightSide  = pRSide;
	pAddedSide->pLeftSide	= pLSide;
	pRSide->pLeftSide		= pAddedSide;
	pLSide->pRightSide		= pAddedSide;
	//-------------------- (Re)create the Face's ORDERED Side and Node Lists
	MakeAFacesSideList				(pBFace, pAddedSide);	// only AFTER L&R pointers!!!
	MakeAFacesNodeListFromSideList	(pBFace);
	//-------------------- point ALL Sides at the "new" Face
	pS = pBFace->p1stSide;
	while (pS != NULL)
	{	pS->pFace = pBFace;
		pS = pS->pFsNextS;
	}
	//-------------------- insert the new side into various LISTs
	InsertSideInEdgesSideList (pAddedSide, pNewEdge );
	AddSideToZonesSideList	  (pAddedSide, pBFace->pZone );
	AddSideToNodesSideList    (pAddedSide, pRNode);
	AddSideToNodesSideList    (pAddedSide, pLNode);
//	CalcFaceCentroidFromNodes (pBFace);
}
//===============================================================================
void MakeNewEdgeAndDivide2Faces ( IntersectionTD *I )
{
	SidePtr		pTFSide2L, pTFSide2R, pTFSide1L, pTFSide1R;
	SidePtr		pBFSide2L, pBFSide2R, pBFSide1L, pBFSide1R;
	SidePtr		pTLSide,   pTRSide,   pBLSide,   pBRSide  ;
	FacePtr		pTopFace,  pNewTopFace, pBottomFace, pNewBottomFace;
	EdgePtr		pNewEdge;
	int			numTS, numTN, numBS, numBN,	// for debugging
    nE = 0;							// to count errors
	ZonePtr		pZ, pOZ;	// FOR DEBUG
			
	//-------------------- assemble the "absolute frame" variables
	//----- BOTH FACES
	pTopFace    = I->pSideOnFace2BCut->pFace;
	pBottomFace = FindOppositeFace ( pTopFace );			// returns NULL on boundary
	
	pZ = pTopFace->pZone;
	if (pBottomFace != NULL) pOZ = pBottomFace->pZone;

	//----- TOP SIDES
	pTFSide2L   = FindSideOnFace ( pTopFace, I->pEndNode);	// end Node is Node 1 of the found Side
	pTFSide2R   = pTFSide2L->pRightSide;
	pTFSide1R   = FindSideOnFace ( pTopFace, I->pStartNode);// start Node is Node 1 of the found Side
	pTFSide1L   = pTFSide1R->pRightSide;

	//----- BOTTOM SIDES
	if (pBottomFace != NULL)	//---------- Face is NOT on boundary. Set bottom side pointers
	{	pBFSide2L = pTFSide2L->pOppositeSide;
		pBFSide2R = pTFSide2R->pOppositeSide;
		pBFSide1L = pTFSide1L->pOppositeSide;
		pBFSide1R = pTFSide1R->pOppositeSide;
	}
	else	//---------- Face is on boundary. Set bottom side pointers to NULL
	{	pBFSide2L = NULL;
		pBFSide2R = NULL;
		pBFSide1L = NULL;
		pBFSide1R = NULL;
	}
	
	//-------------------- create new EDGE
	pNewEdge = allocateEdge ( getNumE() );     I->pEdge2CutFace = pNewEdge;	// for later use
	pNewEdge->pNode1 = I->pStartNode;
	pNewEdge->pNode2 = I->pEndNode;
	pNewEdge->p1stSide   = NULL;
	pNewEdge->p1stCorner = NULL;
	//-------------------- create new SIDES. (See figure 5e)
	pTLSide = allocateSide ( getNumS() );		I->pLeft_base_side  = pTLSide;	// for later use
	pTRSide = allocateSide ( getNumS() );		I->pRight_base_side = pTRSide;	// for later use
	if (pBottomFace != NULL)
	{	pBLSide = allocateSide ( getNumS() );
		pBRSide = allocateSide ( getNumS() );
	}
	else
	{	pBLSide = NULL;
		pBRSide = NULL;
	}
	//----- Establish "Opposite" and "Top" pointers for the four new Sides
	pTLSide->pOppositeSide	= pBLSide;
	pTRSide->pOppositeSide	= pBRSide;
	pTLSide->pTopSide		= pTRSide;
	pTRSide->pTopSide		= pTLSide;
							 
	if (pBottomFace != NULL)
	{	pBLSide->pOppositeSide	= pTLSide;
		pBRSide->pOppositeSide	= pTRSide;
		pBLSide->pTopSide		= pBRSide;
		pBRSide->pTopSide		= pBLSide;
	}
  //nE = CheckFaceInfo ( pTopFace, "In MNEAD2F, before linking new Sides.\n" );
  numTS = NumSidesInFace (pTopFace);
  numTN = NumNodesOnFace (pTopFace);
  if (pBottomFace != NULL)
  {	numBS = NumSidesInFace (pBottomFace);
    numBN = NumNodesOnFace (pBottomFace);
    //nE = nE + CheckFaceInfo ( pBottomFace, "In MNEAD2F, before linking new Sides.\n" );
  }
	//----------------------------------- create the NEW TOP FACE, and link new Sides
		
  pNewTopFace = allocateFace ( getNumF() );
	pNewTopFace->pZone		 = pTopFace->pZone;
  pNewTopFace->FType = INTERIOR_FACE;
	AddFaceToZonesFaceList ( pNewTopFace , pTopFace->pZone );
	DeleteList (pTopFace->p1stNodeLI);
//				  (BFace,          RNode,		  LNode,		 RSide,     LSide,    addedSide, newEdge);
	LinkUpNewSide (pTopFace,       I->pStartNode, I->pEndNode  , pTFSide1L, pTFSide2L, pTLSide, pNewEdge);
	LinkUpNewSide (pNewTopFace,    I->pEndNode  , I->pStartNode, pTFSide2R, pTFSide1R, pTRSide, pNewEdge);
  //nE = nE + CheckFaceInfo ( pTopFace,    "In MNEAD2F, after adding Side to TopFace.\n" );
  //nE = nE + CheckFaceInfo ( pNewTopFace, "In MNEAD2F, after adding Side to NewTopFace.\n" );
			
	if (pBottomFace != NULL)	//------- create the NEW BOTTOM FACE, and link new Sides
	{		
		pNewBottomFace = allocateFace ( getNumF() );
		pNewBottomFace->pZone		= pBottomFace->pZone;
    pNewBottomFace->FType = INTERIOR_FACE;
		AddFaceToZonesFaceList ( pNewBottomFace, pBottomFace->pZone );
		DeleteList (pBottomFace->p1stNodeLI);
//					  (BFace,          RNode,		  LNode,		 RSide,     LSide,    addedSide, newEdge);
		LinkUpNewSide (pBottomFace,    I->pEndNode  , I->pStartNode, pBFSide2L, pBFSide1L, pBLSide, pNewEdge);
		LinkUpNewSide (pNewBottomFace, I->pStartNode, I->pEndNode  , pBFSide1R, pBFSide2R, pBRSide, pNewEdge);
    //nE = nE + CheckFaceInfo ( pBottomFace,    "In MNEAD2F, after adding Side to BottomFace.\n" );
    //nE = nE + CheckFaceInfo ( pNewBottomFace, "In MNEAD2F, after adding Side to NewBottomFace.\n" );
	}
  if (nE > 0) // number of errors > 0
  {	printf("TopFace    ORIGINAL Nodes =%2d\n", numTN);
    printf("TopFace    ORIGINAL Sides =%2d\n", numTS);
    printf("BottomFace ORIGINAL Nodes =%2d\n", numBN);
    printf("BottomFace ORIGINAL Sides =%2d\n", numBS);
    exit (1);
  }
} // ----- END of makeNewEdgeAndDivide2Faces

//===============================================================================
void NEWSortFaces ( ZonePtr pZone2BCut )
//--------------------------------------------------------------------------------
// A new implementation that is completely contained in this one function.
//--------------------------------------------------------------------------------
{
	FacePtr		pF;				// pointer for loop over all Faces of the Zone
	int			numUnassigned;	// the number of unassigned Faces
	int			numC;			// for loop index
	FS_States	NFaceSS;		// a neighbor Face's sort state
	SidePtr		pS;				// to loop over a Face's Sides
	
	num_right_faces = 0;
	num_left_faces  = 0;
	
	pF = pZone2BCut->p1stFace;					// Loop through all of the Zone's Faces
	while (pF != NULL)							// setting their state to "unassigned"
	{	pF->SortState = S_UNASSIGNED; pF = pF->pZsNextF;
	}
	
	if (num_cuts > MAX_CZ_CUTS)
	{	printf ("ERROR; num_cuts is too large\n");
		exit(1);
	}
	
	for ( numC = 0; numC < num_cuts; numC++ )	// Sort Faces on each side of the cut Edges
	{	
		pLeftFace[num_left_faces] = cut_edge[numC].pLeft_base_side->pFace;
		pLeftFace[num_left_faces]->SortState = S_LEFT;
		num_left_faces  += 1;
		pRightFace[num_right_faces] = cut_edge[numC].pRight_base_side->pFace;
		pRightFace[num_right_faces]->SortState = S_RIGHT;
		num_right_faces += 1;
	}
	
	do
	{	numUnassigned = 0;
		
		pF = pZone2BCut->p1stFace;	//----- Loop over the Zone's Faces, trying to
		while (pF != NULL)			//		sort any Unassigned Faces
		{
			if (pF->SortState == S_UNASSIGNED)
			{	numUnassigned += 1;					// increment the count of unassigned Faces
			
				pS = pF->p1stSide;
				while (pS != NULL)
				{	
					NFaceSS = pS->pTopSide->pFace->SortState;	// get neighbor Face's sort state
					
					if (NFaceSS != S_UNASSIGNED)	// neighbor has been assigned
					{	numUnassigned -= 1;			// decrement the count of unassigned Faces
						pF->SortState  = NFaceSS;	// store the assignment
						if (NFaceSS == S_LEFT)
						{	
							pLeftFace[num_left_faces] = pF;		// store with left Faces
							num_left_faces  += 1;
						}
						else
						{	pRightFace[num_right_faces] = pF;	// store with right Faces
							num_right_faces += 1;
						}
						break;	// to exit the while loop over Sides, to move to the next Face
					
					}	// end of if block
					pS = pS->pFsNextS;
				}	// end of while loop over a Face's Sides
			}	// end of if == S_UNASSIGNED
			pF = pF->pZsNextF;
		}	// end of while loop over all Faces of the Zone
	} while (numUnassigned > 0);
	
} //----- End of NewSortFaces
//===============================================================================
ZonePtr Insert2Faces ( ZonePtr pZone2BCut, FacePtr *ppANewFace )
{
	int		numC;			// index to walk the cut_edge data structure
	int		nF;
//	int		nErr;
	ZonePtr	pLEFT_Zone;		// newly created zone; should it be an argument??
	FacePtr	pNewRightFace, pNewLeftFace;	// to be created
	SidePtr	pS,		// to loop over sides of a Face
    pNewS;	// created for each new Face
	
	pLEFT_Zone = allocateZone ( getNumZ() );
	
	NEWSortFaces ( pZone2BCut );				// sort all faces into right and left faces

	pNewRightFace = allocateFace ( getNumF() );			// create the new right face
	pRightFace[num_right_faces] = pNewRightFace;// and add it to the right face list
  pNewRightFace->FType = INTERIOR_FACE;
	num_right_faces = num_right_faces + 1;
				
	pNewLeftFace  = allocateFace ( getNumF() );			// create the new left face
	pLeftFace[num_left_faces] = pNewLeftFace;	// and add it to the left face list
  pNewLeftFace->FType = INTERIOR_FACE;
	num_left_faces = num_left_faces + 1;
		
	for ( numC = 0; numC < num_cuts; numC++ )	// create the two new Sides for each "cut"
	{	//----- first create the top left side and fill its pointers
		pNewS = allocateSide ( getNumS() );
		pNewS->pFace  = pNewLeftFace;
		pNewS->pNode1 = cut_edge[numC].pEndNode;
		pNewS->pNode2 = cut_edge[numC].pStartNode;
		pNewS->pEdge  = cut_edge[numC].pEdge2CutFace;
		AddSideToNodesSideList    (pNewS, pNewS->pNode1);
		AddSideToNodesSideList    (pNewS, pNewS->pNode2);
		InsertSideInEdgesSideList (pNewS, pNewS->pEdge );
		cut_edge[numC].pLeft_top_side  = pNewS;
		
		//----- next create the top right side and fill its pointers
		pNewS = allocateSide ( getNumS() );
		pNewS->pFace  = pNewRightFace;
		pNewS->pNode1 = cut_edge[numC].pStartNode;
		pNewS->pNode2 = cut_edge[numC].pEndNode;
		pNewS->pEdge  = cut_edge[numC].pEdge2CutFace;
		AddSideToNodesSideList    (pNewS, pNewS->pNode1);
		AddSideToNodesSideList    (pNewS, pNewS->pNode2);
		InsertSideInEdgesSideList (pNewS, pNewS->pEdge);
		cut_edge[numC].pRight_top_side  = pNewS;
		
		//---------- now do pTopSide and pOppositeSide linkages starting from left to right
		cut_edge[numC].pLeft_base_side->pTopSide		= cut_edge[numC].pLeft_top_side;
		cut_edge[numC].pLeft_top_side->pTopSide			= cut_edge[numC].pLeft_base_side;
		cut_edge[numC].pLeft_top_side->pOppositeSide	= cut_edge[numC].pRight_top_side;
		cut_edge[numC].pRight_top_side->pOppositeSide	= cut_edge[numC].pLeft_top_side;
		cut_edge[numC].pRight_top_side->pTopSide		= cut_edge[numC].pRight_base_side;
		cut_edge[numC].pRight_base_side->pTopSide		= cut_edge[numC].pRight_top_side;
	}
	
	//-------------------- set right and left linkages of all newly created sides
	for ( numC = 0; numC < num_cuts; numC++ )
	{
		if ( numC == 0 )
		{	cut_edge[0].pLeft_top_side->pLeftSide   = cut_edge[num_cuts-1].pLeft_top_side;
			cut_edge[0].pRight_top_side->pRightSide = cut_edge[num_cuts-1].pRight_top_side;
		}
		else
		{	cut_edge[numC].pLeft_top_side->pLeftSide   = cut_edge[numC-1].pLeft_top_side;
			cut_edge[numC].pRight_top_side->pRightSide = cut_edge[numC-1].pRight_top_side;
		}
		if (numC == num_cuts-1 )
		{	cut_edge[numC].pLeft_top_side->pRightSide  = cut_edge[0].pLeft_top_side;
			cut_edge[numC].pRight_top_side->pLeftSide  = cut_edge[0].pRight_top_side;
		}
		else
		{	cut_edge[numC].pLeft_top_side->pRightSide  = cut_edge[numC+1].pLeft_top_side;
			cut_edge[numC].pRight_top_side->pLeftSide  = cut_edge[numC+1].pRight_top_side;
		}
	}
	
	//----- create CCW ordered Side lists for both new Faces; only works AFTER L&R pointers are set
	MakeAFacesSideList ( pNewRightFace, cut_edge[0].pRight_top_side);
	MakeAFacesSideList ( pNewLeftFace , cut_edge[0].pLeft_top_side );
	
	//----------------------------------- create the CCW ordered Node lists both new Faces
	MakeAFacesNodeListFromSideList ( pNewRightFace);
	MakeAFacesNodeListFromSideList ( pNewLeftFace);

	//nErr = CheckFaceInfo ( pNewRightFace, "I2F, after creating Node list for pNewRightFace.\n" );
	//nErr = CheckFaceInfo ( pNewLeftFace , "I2F, after creating Node list for pNewLeftFace.\n"  );

	ClearZonesFaceList ( pZone2BCut );	ClearZonesSideList ( pZone2BCut );
	ClearZonesFaceList ( pLEFT_Zone );	ClearZonesSideList ( pLEFT_Zone );
	
	for ( nF = 0; nF < num_right_faces; nF++ ) //----- Assign Faces and their Sides to Zone2BCut
	{										   //	   AND Sides to their Faces.
		pRightFace[nF]->pZone = pZone2BCut;
		AddFaceToZonesFaceList (pRightFace[nF], pZone2BCut );
		pS = pRightFace[nF]->p1stSide;
		while ( pS != NULL ) {
			pS->pZone = pZone2BCut;
			pS->pFace = pRightFace[nF];
			AddSideToZonesSideList ( pS, pZone2BCut );
			pS = pS->pFsNextS;							// we follow the FACE'S side list
		}
	}
//	CalcFaceCentroidFromNodes (pNewRightFace);		// Update Face and Zone centroids
//	CalcZoneCentroidFromFaces (pZone2BCut   );
	
	for ( nF = 0; nF < num_left_faces; nF++ ) //----- assign faces and their sides to LEFT_Zone
	{
		pLeftFace[nF]->pZone = pLEFT_Zone;
		AddFaceToZonesFaceList (pLeftFace[nF], pLEFT_Zone );
		pS = pLeftFace[nF]->p1stSide;
		while ( pS != NULL ) {
			pS->pZone = pLEFT_Zone;
			pS->pFace = pLeftFace[nF];
			AddSideToZonesSideList ( pS, pLEFT_Zone );
			pS = pS->pFsNextS;							// we follow the FACE'S side list
		}
	}
//	CalcFaceCentroidFromNodes (pNewLeftFace);		// Update Face and Zone centroids
//	CalcZoneCentroidFromFaces (pLEFT_Zone  );
	
	*ppANewFace = pNewLeftFace;
	return ( pLEFT_Zone );
} // ----- END of Insert2Faces


